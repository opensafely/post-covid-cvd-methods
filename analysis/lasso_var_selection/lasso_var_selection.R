# ------------------------------------------------------------------------------
#
# lasso_var_selection.R
#
# This file runs LASSO (Least absolute shrinkage and selection operator)
# regression on the data to determine a subset of the available confounders
# in turn determining the cox_ipw argument. "--covariate_other=this;that;other"
#
# Arguments:
#  - cohort - string, defines which of three opensafely cohorts to describe
#             (prevax, vax, unvax)
#  - age_str - vector of form "XX;XX;XX;XX;XX"
#              defines the age ranges over which the study population is stratified
#  - preex - boolean/string, defines preexisting conditions
#            for the replication preex = FALSE always
#            ("All", TRUE, or FALSE)
#
# Returns:
#  - Table of variables selected by the LASSO regression
#    (output/lasso_var_selection/lasso_var_selection-cohort_prevax.csv)
#
# Authors: Emma Tarmey
#
# ------------------------------------------------------------------------------


# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)
library(glmnet)
library(survival)


# Define lasso_var_selection output folder -------------------------------------
print("Creating output/lasso_var_selection output folder")

lasso_var_selection_dir <- "output/lasso_var_selection/"
fs::dir_create(here::here(lasso_var_selection_dir))


# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")


# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

print(length(args))

if (length(args) == 0) {
  # default argument values
  name    <- "cohort_prevax-main-ami"
  cohort  <- "prevax"
  age_str <- "18;30;40;50;60;70;80;90"
  preex   <- FALSE
} else {
  # YAML arguments
  name    <- args[[1]]
  cohort  <- args[[2]]
  age_str <- args[[3]]

  # optional argument
  if (length(args) < 4) {
    preex <- "All"
  } else {
    preex <- args[[4]]
  } # allow an empty input for the preex variable
}

age_bounds   <- as.numeric(stringr::str_split(as.vector(age_str), ";")[[1]])
preex_string <- ""


# Load subsample data ----------------------------------------------------------
print("Load subsample data")

# subsample
model_input_df <- readr::read_rds(paste0(
  "output/model/model_input_subsample-",
  name,
  ".rds"
))


# Check all covariates for cox model -------------------------------------
message("Check all covariates for cox model")

model_input_df$index_date        <- as.Date(model_input_df$index_date)
model_input_df$end_date_exposure <- as.Date(model_input_df$end_date_exposure)
model_input_df$end_date_outcome  <- as.Date(model_input_df$end_date_outcome)

model_input_df$exp_date <- as.Date(model_input_df$exp_date)
model_input_df$out_date <- as.Date(model_input_df$out_date)

model_input_df$cov_num_age       <- as.numeric(model_input_df$cov_num_age)
model_input_df$cov_cat_sex       <- as.factor(model_input_df$cov_cat_sex)
model_input_df$cov_cat_ethnicity <- as.factor(model_input_df$cov_cat_ethnicity)
model_input_df$cov_cat_imd       <- as.factor(model_input_df$cov_cat_imd)
model_input_df$cov_cat_smoking   <- as.factor(model_input_df$cov_cat_smoking)

model_input_df$cov_bin_carehome      <- as.logical(model_input_df$cov_bin_carehome)
model_input_df$cov_bin_hcworker      <- as.factor(model_input_df$cov_bin_hcworker)
model_input_df$cov_bin_dementia      <- as.logical(model_input_df$cov_bin_dementia)
model_input_df$cov_bin_liver_disease <- as.logical(model_input_df$cov_bin_liver_disease)
model_input_df$cov_bin_ckd           <- as.logical(model_input_df$cov_bin_ckd)

model_input_df$cov_bin_cancer       <- as.logical(model_input_df$cov_bin_cancer)
model_input_df$cov_bin_hypertension <- as.logical(model_input_df$cov_bin_hypertension)
model_input_df$cov_bin_diabetes     <- as.logical(model_input_df$cov_bin_diabetes)
model_input_df$cov_bin_obesity      <- as.logical(model_input_df$cov_bin_obesity)
model_input_df$cov_bin_copd         <- as.logical(model_input_df$cov_bin_copd)

model_input_df$cov_bin_depression <- as.logical(model_input_df$cov_bin_depression)
model_input_df$cov_bin_stroke_all <- as.logical(model_input_df$cov_bin_stroke_all)
model_input_df$cov_bin_other_ae   <- as.logical(model_input_df$cov_bin_other_ae)
model_input_df$cov_bin_vte        <- as.logical(model_input_df$cov_bin_vte)
model_input_df$cov_bin_hf         <- as.logical(model_input_df$cov_bin_hf)

model_input_df$cov_bin_angina        <- as.logical(model_input_df$cov_bin_angina)
model_input_df$cov_bin_lipidmed      <- as.logical(model_input_df$cov_bin_lipidmed)
model_input_df$cov_bin_antiplatelet  <- as.logical(model_input_df$cov_bin_antiplatelet)
model_input_df$cov_bin_anticoagulant <- as.logical(model_input_df$cov_bin_anticoagulant)
model_input_df$cov_bin_cocp          <- as.logical(model_input_df$cov_bin_cocp)

model_input_df$cov_bin_hrt      <- as.logical(model_input_df$cov_bin_hrt)
model_input_df$strat_cat_region <- as.factor(model_input_df$strat_cat_region)


# Data preparation for lasso cox model ----------------------------------
message("Data preparation for lasso cox model")

lasso_cox_conf_matrix <- (model_input_df %>% select(c(
  cov_bin_covid,

  cov_num_age,
  cov_cat_sex,
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
  strat_cat_region
)))

lasso_cox_conf_matrix_preserving_factors <- model.matrix(
   ~ ., # formula meaning take all terms
  data = lasso_cox_conf_matrix
)

outcome_cox_dates <- rep(as.Date(NA), times = nrow(model_input_df))
cens_status       <- rep(NA, times = nrow(model_input_df))

# 0 = censoring time = date of end of study
# 1 = failure time = time of outcome event
# See: https://www.rdocumentation.org/packages/survival/versions/3.8-3/topics/Surv
# and: https://glmnet.stanford.edu/articles/Coxnet.html#basic-usage-for-right-censored-data
for (i in c(1:nrow(model_input_df))) {
  if (is.na(model_input_df$out_date[i])) {
    # right-hand censorship takes place
    cens_status[i]       <- 0
    outcome_cox_dates[i] <- model_input_df$end_date_outcome[i]
  } else {
    # event takes place (failure)
    cens_status[i]       <- 1
    outcome_cox_dates[i] <- model_input_df$out_date[i]
  }
}

lasso_cox_outcome_survival <- Surv(time  = as.numeric(outcome_cox_dates),
                                   event = cens_status,
                                   type  = "right")


# Fitting the lasso cox model ----------------------------------------------------
message("Fitting the lasso cox model")

lasso_cox_outcome_survival <- Surv(time  = as.numeric(outcome_cox_dates),
                                   event = cens_status,
                                   type  = "right")

cv_lasso_cox_model <- cv.glmnet(x      = lasso_cox_conf_matrix_preserving_factors,
                                y      = lasso_cox_outcome_survival,
                                family = "cox",      # logistic regression
                                alpha  = 1)          # LASSO penalty

# tune regularisation parameter lambda to minimise cross-validated error (cvm)
lambda         <- cv_lasso_cox_model$lambda.min

lasso_cox_model    <- glmnet(x = lasso_cox_conf_matrix_preserving_factors,
                             y = lasso_cox_outcome_survival,
                             family = "cox",      # logistic regression
                             alpha=1,             # LASSO penalty
                             lambda=lambda)       # optimal lambda

print(lasso_cox_model$beta)
stop("check")


# Extract covariate selection results ------------------------------------------
print("Extract covariate selection results")

lasso_coefs        <- as.vector(lasso_model$beta)
names(lasso_coefs) <- rownames(lasso_model$beta)

vars_selected <- names(lasso_coefs[lasso_coefs != 0.0])
vars_selected <- vars_selected[vars_selected != "(Intercept)"] # remove intercept

# always include exposure
if (!("binary_covid19_exposure" %in% vars_selected)) {
  vars_selected <- c(vars_selected, "binary_covid19_exposure")
}

# remove all dates
vars_selected <- vars_selected[!vars_selected %in% c("index_date", "end_date_exposure", "end_date_outcome", "exp_date", "out_date")]


# Save covariate selection and coefficients ------------------------------------
print("Save Covariate Selection and Coefficients")

write.csv(
  lasso_coefs,
  paste0(lasso_var_selection_dir, "lasso_var_selection-coefs-", name, preex_string, ".csv"),
  row.names = TRUE
)


write.csv(
  vars_selected,
  paste0(lasso_var_selection_dir, "lasso_var_selection-", name, preex_string, ".csv"),
  row.names = FALSE
)
