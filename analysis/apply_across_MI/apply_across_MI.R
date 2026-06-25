# ------------------------------------------------------------------------------
#
# apply_across_MI.R
#
# This file applies multiple imputation to the BMI and Smoking covariates
# MI is conducted in "across" fashion, meaning that the result is a dataframe
# 10x larger than the origina containing all datasets
# 
# Arguments:
#  - cohort - string, defines which of three opensafely cohorts to describe
#             (prevax, vax, unvax)
#  - preex - boolean/string, defines preexisting conditions
#            for the replication preex = FALSE always
#            ("All", TRUE, or FALSE)
#
# Returns:
#  - output/apply_across_MI/input_<cohort>_clean_across_MI_ami.rds
#  - output/apply_across_MI/input_<cohort>_clean_across_MI_sahhs.rds
#
# Authors: Emma Tarmey
#
# ------------------------------------------------------------------------------


# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(mice)
library(here)
library(dplyr)
library(fs)
library(survival)


# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")

# only needed here, generates unique patient IDs for stacked data
# unique IDs needed for cox_ipw action
new_patient_ids <- function(ids = NULL, num_needed = NULL, shift = NULL) {
  if (num_needed == 1) {
    return (ids + shift)
  } else {
    new_ids <- c(
      ids,
      new_patient_ids(ids = (ids + shift), num_needed = (num_needed - 1), shift = shift)
    )
  }
  return (new_ids)
}


# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)
print(length(args))

if (length(args) == 0) {
  # default argument values
  cohort  <- "prevax"
  preex   <- "All"

} else {
  # YAML arguments
  cohort  <- args[[1]]

  # optional argument
  if (length(args) < 2) {
    preex <- "All"
  } else {
    preex <- args[[2]]
  } # allow an empty input for the preex variable
}


# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_rds(paste0(
  "output/dataset_clean/input_",
  cohort,
  "_clean_prehoc.rds"
))

df <- as.data.frame(df)


# Define variables -------------------------------------------------------------
print("Define variables")

df$cov_bin_covid <- !is.na(df$exp_date_covid)
df$cov_bin_sahhs <- !is.na(df$out_date_stroke_sahhs)


# Check all covariates types ---------------------------------------------------
print("Check all covariate types")

df$cov_bin_ami   <- as.factor(df$cov_bin_ami)   # outcome
df$cov_bin_sahhs <- as.factor(df$cov_bin_sahhs) # outcome
df$cov_bin_covid <- as.factor(df$cov_bin_covid) # exposure

df$cov_num_age       <- as.numeric(df$cov_num_age)
df$cov_cat_sex       <- as.factor(df$cov_cat_sex)
df$cov_num_bmi       <- as.numeric(df$cov_num_bmi)
df$cov_cat_ethnicity <- as.factor(df$cov_cat_ethnicity)
df$cov_cat_imd       <- as.factor(df$cov_cat_imd)
df$cov_cat_smoking   <- as.factor(df$cov_cat_smoking)

df$cov_bin_carehome      <- as.factor(df$cov_bin_carehome)
df$cov_bin_hcworker      <- as.factor(df$cov_bin_hcworker)
df$cov_bin_dementia      <- as.factor(df$cov_bin_dementia)
df$cov_bin_liver_disease <- as.factor(df$cov_bin_liver_disease)
df$cov_bin_ckd           <- as.factor(df$cov_bin_ckd)

df$cov_bin_cancer       <- as.factor(df$cov_bin_cancer)
df$cov_bin_hypertension <- as.factor(df$cov_bin_hypertension)
df$cov_bin_diabetes     <- as.factor(df$cov_bin_diabetes)
df$cov_bin_obesity      <- as.factor(df$cov_bin_obesity)
df$cov_bin_copd         <- as.factor(df$cov_bin_copd)

df$cov_bin_depression <- as.factor(df$cov_bin_depression)
df$cov_bin_stroke_all <- as.factor(df$cov_bin_stroke_all)
df$cov_bin_other_ae   <- as.factor(df$cov_bin_other_ae)
df$cov_bin_vte        <- as.factor(df$cov_bin_vte)
df$cov_bin_hf         <- as.factor(df$cov_bin_hf)

df$cov_bin_angina        <- as.factor(df$cov_bin_angina)
df$cov_bin_lipidmed      <- as.factor(df$cov_bin_lipidmed)
df$cov_bin_antiplatelet  <- as.factor(df$cov_bin_antiplatelet)
df$cov_bin_anticoagulant <- as.factor(df$cov_bin_anticoagulant)
df$cov_bin_cocp          <- as.factor(df$cov_bin_cocp)

df$cov_bin_hrt      <- as.factor(df$cov_bin_hrt)
df$strat_cat_region <- as.factor(df$strat_cat_region)


# Applying multiple imputation to BMI and smoking covariates -------------------
print("Applying multiple imputation to BMI and smoking covariates")

# set random seed
set.seed(2026)

# re-cast missing smoking to NA
smoking_missing <- df$cov_cat_smoking == "Missing"
df$cov_cat_smoking[smoking_missing] <- NA

# check missingness of smoking and bmi variables
percent_smoking_missing <- signif(100 * (sum(is.na(df$cov_cat_smoking)) / length(df$cov_cat_smoking)), digits = 4)
percent_bmi_missing     <- signif(100 * (sum(is.na(df$cov_num_bmi))     / length(df$cov_num_bmi)),     digits = 4)

print(paste0("The variable smoking is ", percent_smoking_missing, "% missing"))
print(paste0("The variable bmi is ",     percent_bmi_missing,     "% missing"))


# Specify imputation methods for each outcome (ami and sahhs) ------------------
print("Specify imputation methods for each outcome (ami and sahhs)")

# The below ensures that bmi and smoking are handled with specific
# imputation methods and that all other covariates are left alone
# See: https://www.rdocumentation.org/packages/mice/versions/3.17.0/topics/mice
imp_method                    <- rep("", length.out = length(colnames(df)))
names(imp_method)             <- colnames(df)
imp_method["cov_cat_smoking"] <- "polyreg" # smoking is categorical with 3 levels, Polytomous logistic regression
imp_method["cov_num_bmi"]     <- "norm"    # bmi is numerical, Bayesian linear regression


# Specify imputation formulas for each outcome (ami and sahhs) -----------------
print("Specify imputation formulas for outcome")

# Specify imputation formulas, exclude variable such as index date
all_var_names <- c(
  "cov_bin_ami",
  "cov_bin_sahhs",
  "cov_bin_covid",

  "cov_num_age",
  "cov_cat_sex",
  # "cov_num_bmi", # excluded
  "cov_cat_ethnicity",
  "cov_cat_imd",
  # "cov_cat_smoking", # excluded

  "cov_bin_carehome",
  "cov_bin_hcworker",
  "cov_bin_dementia",
  "cov_bin_liver_disease",
  "cov_bin_ckd",

  "cov_bin_cancer",
  "cov_bin_hypertension",
  "cov_bin_diabetes",
  "cov_bin_obesity",
  "cov_bin_copd",

  "cov_bin_depression",
  "cov_bin_stroke_all",
  "cov_bin_other_ae",
  "cov_bin_vte",
  "cov_bin_hf",

  "cov_bin_angina",
  "cov_bin_lipidmed",
  "cov_bin_antiplatelet",
  "cov_bin_anticoagulant",
  "cov_bin_cocp",

  "cov_bin_hrt",
  "strat_cat_region",
  "vax_cat_jcvi_group"
  # "cens_status" # excluded
)

my_formulas <- list(
  cov_cat_smoking = as.formula(paste0("cov_cat_smoking ~ ", paste(all_var_names, collapse = " + "), " + H0")),
  cov_num_bmi     = as.formula(paste0("cov_num_bmi ~ ",     paste(all_var_names, collapse = " + "), " + H0"))
)

# Calculate Nelson-Aalen Estimator for outcome -----------
print("Calculate Nelson-Aalen Estimator for outcome")

df_ami   <- df
df_sahhs <- df

outcome_cox_dates_ami <- rep(as.Date(NA), times = nrow(df_ami))
cens_status_ami       <- rep(NA, times = nrow(df_ami))

# 0 = censoring time = date of end of study
# 1 = failure time = time of outcome event
# See: https://www.rdocumentation.org/packages/survival/versions/3.8-3/topics/Surv
# and: https://glmnet.stanford.edu/articles/Coxnet.html#basic-usage-for-right-censored-data
for (i in c(1:nrow(df_ami))) {
  if (is.na(df_ami$out_date_ami[i])) {
    # right-hand censorship takes place
    cens_status_ami[i]       <- 0
    outcome_cox_dates_ami[i] <- df_ami$end_date_outcome[i]
  } else {
    # event takes place (failure)
    cens_status_ami[i]       <- 1
    outcome_cox_dates_ami[i] <- df_ami$out_date_ami[i]
  }
}

outcome_cox_dates_sahhs <- rep(as.Date(NA), times = nrow(df_sahhs))
cens_status_sahhs       <- rep(NA, times = nrow(df_sahhs))

# 0 = censoring time = date of end of study
# 1 = failure time = time of outcome event
# See: https://www.rdocumentation.org/packages/survival/versions/3.8-3/topics/Surv
# and: https://glmnet.stanford.edu/articles/Coxnet.html#basic-usage-for-right-censored-data
for (i in c(1:nrow(df_sahhs))) {
  if (is.na(df_sahhs$out_date_stroke_sahhs[i])) {
    # right-hand censorship takes place
    cens_status_sahhs[i]       <- 0
    outcome_cox_dates_sahhs[i] <- df_sahhs$end_date_outcome[i]
  } else {
    # event takes place (failure)
    cens_status_sahhs[i]       <- 1
    outcome_cox_dates_sahhs[i] <- df_sahhs$out_date_stroke_sahhs[i]
  }
}

# add data to dataframes
df_ami$outcome_cox_dates_ami <- as.numeric(outcome_cox_dates_ami)
df_ami$cens_status_ami       <- cens_status_ami

df_sahhs$outcome_cox_dates_sahhs <- as.numeric(outcome_cox_dates_sahhs)
df_sahhs$cens_status_sahhs       <- cens_status_sahhs

# ami
H0          <- (survfit(Surv(outcome_cox_dates_ami, cens_status_ami) ~ 1, data = df_ami) %>% summary(times = unique(df_ami$outcome_cox_dates_ami)))
H0          <- H0[c("time", "surv")]
names(H0)   <- c("outcome_cox_dates_ami", "surv")
H0          <- as.data.frame(H0)
df_ami <- merge(df_ami, H0, all.x = TRUE, by = "outcome_cox_dates_ami")
df_ami <- rename(df_ami, H0 = surv)

# sahhs
H0          <- (survfit(Surv(outcome_cox_dates_sahhs, cens_status_sahhs) ~ 1, data = df_sahhs) %>% summary(times = unique(df_sahhs$outcome_cox_dates_sahhs)))
H0          <- H0[c("time", "surv")]
names(H0)   <- c("outcome_cox_dates_sahhs", "surv")
H0          <- as.data.frame(H0)
df_sahhs <- merge(df_sahhs, H0, all.x = TRUE, by = "outcome_cox_dates_sahhs")
df_sahhs <- rename(df_sahhs, H0 = surv)


# Applying multiple imputation to BMI and smoking covariates for outcome ---
print("Applying multiple imputation to BMI and smoking covariates for outcome")

# Apply multiple imputation for ami outcome
imp_ami <- mice::mice(
  data       = df_ami,
  m          = get_number_of_imputed_datasets(),
  maxit      = 20,
  formulas   = my_formulas,
  imp_method = unname(imp_method)
)

df_post_imputation_ami <- mice::complete(
  imp_ami,
  action  = "long",
  include = FALSE
)

df_post_imputation_ami <- subset(
  df_post_imputation_ami,
  select = -c(.imp, .id)
)

# Apply multiple imputation for sahhs outcome
imp_sahhs <- mice::mice(
  data       = df_sahhs,
  m          = get_number_of_imputed_datasets(),
  maxit      = 20,
  formulas   = my_formulas,
  imp_method = unname(imp_method)
)

df_post_imputation_sahhs <- mice::complete(
  imp_sahhs,
  action  = "long",
  include = FALSE
)

df_post_imputation_sahhs <- subset(
  df_post_imputation_sahhs,
  select = -c(.imp, .id)
)


# Remove now unused level 'missing' from smoking covariate -----
print("Remove now unused level 'missing' from smoking covariate")

df_post_imputation_ami$cov_cat_smoking <- factor(
  df_post_imputation_ami$cov_cat_smoking,
  levels = levels(droplevels(df_post_imputation_ami$cov_cat_smoking))
)

df_post_imputation_sahhs$cov_cat_smoking <- factor(
  df_post_imputation_sahhs$cov_cat_smoking,
  levels = levels(droplevels(df_post_imputation_sahhs$cov_cat_smoking))
)


# Re-assign unique identifiers to imputed dataset for outcome  -----
print("Re-assign unique identifiers to imputed dataset for outcome ")

patient_id_1 <- as.numeric(unique(df_post_imputation_ami$patient_id))
shift        <- max(patient_id_1) + 1

new_patient_id <- new_patient_ids(
  ids        = patient_id_1,
  num_needed = get_number_of_imputed_datasets(),
  shift      = shift
)

df_post_imputation_ami$patient_id   <- new_patient_id
df_post_imputation_sahhs$patient_id <- new_patient_id

# Re-convert date variables to date type
all_date_vars   <- grep("date", colnames(df_post_imputation_ami))
for (var in all_date_vars) {
  df_post_imputation_ami[, var]   <- as.Date(format(as.Date(df_post_imputation_ami[, var], origin = lubridate::origin), "%Y-%m-%d"))
  df_post_imputation_sahhs[, var] <- as.Date(format(as.Date(df_post_imputation_sahhs[, var], origin = lubridate::origin), "%Y-%m-%d"))
}

# check missingness of smoking and bmi variables
percent_smoking_missing_ami <- signif(100 * (sum(is.na(df_post_imputation_ami$cov_cat_smoking)) / length(df_post_imputation_ami$cov_cat_smoking)), digits = 4)
percent_bmi_missing_ami     <- signif(100 * (sum(is.na(df_post_imputation_ami$cov_num_bmi))     / length(df_post_imputation_ami$cov_num_bmi)),     digits = 4)

print(paste0("The variable smoking is ", percent_smoking_missing_ami, "% missing (for ami)"))
print(paste0("The variable bmi is ",     percent_bmi_missing_ami,     "% missing (for ami)"))

percent_smoking_missing_sahhs <- signif(100 * (sum(is.na(df_post_imputation_sahhs$cov_cat_smoking)) / length(df_post_imputation_sahhs$cov_cat_smoking)), digits = 4)
percent_bmi_missing_sahhs     <- signif(100 * (sum(is.na(df_post_imputation_sahhs$cov_num_bmi))     / length(df_post_imputation_sahhs$cov_num_bmi)),     digits = 4)

print(paste0("The variable smoking is ", percent_smoking_missing_sahhs, "% missing (for sahhs)"))
print(paste0("The variable bmi is ",     percent_bmi_missing_sahhs,     "% missing (for sahhs)"))


# Remove unnecessary variables ---
print("Remove unnecessary_variables ")

print(colnames(df_post_imputation_ami))

df_post_imputation_ami_clean <- (
  df_post_imputation_ami %>%
    select(
      # outcome_cox_dates_ami, # excluded
      patient_id,
      index_date,
      end_date_exposure,
      end_date_outcome,
      sub_bin_covidhistory,
      sub_cat_covidhospital,
      exp_date_covid,
      out_date_ami,
      out_date_stroke_sahhs,
      cov_num_age,
      cov_cat_sex,
      cov_num_bmi,
      cov_cat_ethnicity,
      cov_cat_imd,
      cov_cat_smoking,
      cov_bin_carehome,
      cov_bin_hcworker,
      cov_bin_dementia,
      cov_bin_liver_disease,
      cov_bin_ckd,
      cov_bin_cancer,
      cov_bin_hypertension,
      cov_bin_diabetes,
      cov_bin_obesity,
      cov_bin_copd,
      cov_bin_ami,
      cov_bin_depression,
      cov_bin_stroke_all,
      cov_bin_other_ae,
      cov_bin_vte,
      cov_bin_hf,
      cov_bin_angina,
      cov_bin_lipidmed,
      cov_bin_antiplatelet,
      cov_bin_anticoagulant,
      cov_bin_cocp,
      cov_bin_hrt,
      cens_date_dereg,
      cens_date_death,
      strat_cat_region,
      vax_cat_jcvi_group,
      cov_bin_covid,
      cov_bin_sahhs,
      # cens_status_ami, # excluded
      # H0,              # excluded
      vax_date_eligible,
      vax_date_covid_1,
      vax_date_covid_2,
      vax_date_covid_3,
      vax_date_Pfizer_1,
      vax_date_Pfizer_2,
      vax_date_Pfizer_3,
      vax_date_AstraZeneca_1,
      vax_date_AstraZeneca_2,
      vax_date_AstraZeneca_3,
      vax_date_Moderna_1,
      vax_date_Moderna_2,
      vax_date_Moderna_3
    )
)

df_post_imputation_sahhs_clean <- (
  df_post_imputation_sahhs %>%
    select(
      # outcome_cox_dates_sahhs, # excluded
      patient_id,
      index_date,
      end_date_exposure,
      end_date_outcome,
      sub_bin_covidhistory,
      sub_cat_covidhospital,
      exp_date_covid,
      out_date_ami,
      out_date_stroke_sahhs,
      cov_num_age,
      cov_cat_sex,
      cov_num_bmi,
      cov_cat_ethnicity,
      cov_cat_imd,
      cov_cat_smoking,
      cov_bin_carehome,
      cov_bin_hcworker,
      cov_bin_dementia,
      cov_bin_liver_disease,
      cov_bin_ckd,
      cov_bin_cancer,
      cov_bin_hypertension,
      cov_bin_diabetes,
      cov_bin_obesity,
      cov_bin_copd,
      cov_bin_ami,
      cov_bin_depression,
      cov_bin_stroke_all,
      cov_bin_other_ae,
      cov_bin_vte,
      cov_bin_hf,
      cov_bin_angina,
      cov_bin_lipidmed,
      cov_bin_antiplatelet,
      cov_bin_anticoagulant,
      cov_bin_cocp,
      cov_bin_hrt,
      cens_date_dereg,
      cens_date_death,
      strat_cat_region,
      vax_cat_jcvi_group,
      cov_bin_covid,
      cov_bin_sahhs,
      # cens_status_ami, # excluded
      # H0,              # excluded
      vax_date_eligible,
      vax_date_covid_1,
      vax_date_covid_2,
      vax_date_covid_3,
      vax_date_Pfizer_1,
      vax_date_Pfizer_2,
      vax_date_Pfizer_3,
      vax_date_AstraZeneca_1,
      vax_date_AstraZeneca_2,
      vax_date_AstraZeneca_3,
      vax_date_Moderna_1,
      vax_date_Moderna_2,
      vax_date_Moderna_3
    )
)



# Save data after 'across' multiple imputation  ---
print("Save data after 'across' multiple imputation ")

saveRDS(
  df_post_imputation_ami_clean,
  file = paste0("output/dataset_clean/input_", cohort, "_clean_ami.rds"),
  compress = TRUE
)

saveRDS(
  df_post_imputation_ami,
  file = paste0("output/dataset_clean/input_", cohort, "_clean_ami_diagnostic.rds"),
  compress = TRUE
)

saveRDS(
  df_post_imputation_sahhs_clean,
  file = paste0("output/dataset_clean/input_", cohort, "_clean_sahhs.rds"),
  compress = TRUE
)

saveRDS(
  df_post_imputation_sahhs,
  file = paste0("output/dataset_clean/input_", cohort, "_clean_sahhs_diagnostic.rds"),
  compress = TRUE
)
