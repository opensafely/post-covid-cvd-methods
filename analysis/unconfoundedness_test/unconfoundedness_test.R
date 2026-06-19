# ------------------------------------------------------------------------------
#
# unconfoundedness_test.R
#
# This file implements the empirical unconfoundedness plausibility test
# which was the main result of Hartwig et al., 2024
# Original text: https://doi.org/10.48550/arXiv.2402.10156
#
# Arguments:
#  - name - string, specifies cohort and outcome
#           (cohort_prevax-main-ami)
#  - cohort - string, defines which of three opensafely cohorts to describe
#             (prevax, vax, unvax)
#  - age_str - vector of form "XX;XX;XX;XX;XX"
#              defines the age ranges over which the study population is stratified
#  - preex - boolean/string, defines preexisting conditions
#            for the replication preex = FALSE always
#            ("All", TRUE, or FALSE)
#
# Returns:
#  - Table of variables selected, regression results and test conclusions
#    for each of the previously selected confounder sets (LASSO, LASSO_X and LASSO_union)
#    (output/unconfoundedness_test/unconfoundedness_test-cohort_prevax.csv)
#
# Authors: Emma Tarmey
#
# ------------------------------------------------------------------------------

# Refresh local R session ------------------------------------------------------
print("Refresh local R session")

rm(list=ls())


# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)
library(glmnet)
library(survival)


# Define unconfoundedness_test output folder -------------------------------------
print("Creating output/unconfoundedness_test output folder")

unconfoundedness_test_dir <- "output/unconfoundedness_test/"
fs::dir_create(here::here(unconfoundedness_test_dir))


# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")


# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  # default argument values
  name    <- "cohort_prevax-main-ami"
  cohort  <- "prevax"
  age_str <- "18;30;40;50;60;70;80;90"
} else {
  # YAML arguments
  name    <- args[[1]]
  cohort  <- args[[2]]
  age_str <- args[[3]]
}

preex_string <- ""

if (grepl("ami", name)) {
  outcome <- "cov_bin_ami"
} else {
  outcome <- "cov_bin_sahhs"
}


# Load data --------------------------------------------------------------------
print("Load data")

# subsample
if (grepl("ami", name)) {
  df <- readr::read_rds(paste0(
    "output/generate_subsample/input_",
    cohort,
    "_clean_subsample_ami.rds"
  ))
))
} else {
  df <- readr::read_rds(paste0(
    "output/generate_subsample/input_",
    cohort,
    "_clean_subsample_sahhs.rds"
  ))
}

# subsample
model_input_df <- readr::read_rds(paste0(
  "output/model/model_input_subsample-",
  name,
  ".rds"
))

lasso_vars_selected       <- union( read.csv(paste0("output/lasso_var_selection/lasso_var_selection-", name, ".csv"))[, 'x'], c("cov_cat_sex", "cov_num_age"))
lasso_X_vars_selected     <- union( read.csv(paste0("output/lasso_X_var_selection/lasso_X_var_selection-", name, ".csv"))[, 'x'], c("cov_cat_sex", "cov_num_age"))
lasso_union_vars_selected <- union( read.csv(paste0("output/lasso_union_var_selection/lasso_union_var_selection-", name, ".csv"))[, 'x'], c("cov_cat_sex", "cov_num_age"))


# Data preparation for logistic models ---------------------
message("Data preparation for logistic models")

logistic_df <- (df %>% select(c(
  cov_bin_ami,
  cov_bin_sahhs,
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


# Data preparation for cox model ----------------------------------
message("Data preparation for cox model")

cox_df <- (model_input_df %>% select(c(
  out_date,
  end_date_outcome,

  cov_bin_ami,
  cov_bin_sahhs,
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

cox_df$outcome_cox_dates <- rep(as.Date(NA), times = nrow(cox_df))
cox_df$cens_status       <- rep(NA, times = nrow(cox_df))

# 0 = censoring time = date of end of study
# 1 = failure time = time of outcome event
# See: https://www.rdocumentation.org/packages/survival/versions/3.8-3/topics/Surv
# and: https://glmnet.stanford.edu/articles/Coxnet.html#basic-usage-for-right-censored-data
for (i in c(1:nrow(cox_df))) {
  if (is.na(cox_df$out_date[i])) {
    # right-hand censorship takes place
    cox_df$cens_status[i]       <- 0
    cox_df$outcome_cox_dates[i] <- cox_df$end_date_outcome[i]
  } else {
    # event takes place (failure)
    cox_df$cens_status[i]       <- 1
    cox_df$outcome_cox_dates[i] <- cox_df$out_date[i]
  }
}


# Test fully_adjusted variable selection results ----------------
print("Performing empirical unconfoundedness plausibility test on fully_adjusted variable selection results")

# extract covariate selection results
fully_adjusted_vars_selected <- colnames(logistic_df)[
  !colnames(logistic_df) %in% c(outcome)
]

# make formula strings for both regressions
# outcome regression is cox model, exposure regression is logistic
fully_adjusted_exposure_regression_formula <- make_exposure_formula(
  vars_selected = fully_adjusted_vars_selected
)
fully_adjusted_outcome_regression_formula  <- make_outcome_formula(
  vars_selected = fully_adjusted_vars_selected,
  outcome = outcome
)

# fit regression models for outcome and for exposure
# outcome regression is cox model, exposure regression is logistic
fully_adjusted_exposure_regression <- glm(
  fully_adjusted_exposure_regression_formula,
  family = "binomial",
  data   = logistic_df
)
fully_adjusted_outcome_regression  <- coxph(
  formula = as.formula(fully_adjusted_outcome_regression_formula),
  data    = cox_df
)

# extract regression results
# coefficients, SEs, p_values
fully_adjusted_exposure_regression_results <- (
  fully_adjusted_exposure_regression %>%
    broom::tidy() %>%
    as.data.frame()
)
fully_adjusted_outcome_regression_results  <- (
  fully_adjusted_outcome_regression %>%
    broom::tidy() %>%
    as.data.frame()
)

# extract significant variables from both models
fully_adjusted_outcome_regression_significant_vars <- (
  fully_adjusted_outcome_regression_results  %>% filter(p.value < 0.05)
)$term %>% convert_terms_to_vars()

fully_adjusted_exposure_regression_significant_vars <- (
  fully_adjusted_exposure_regression_results  %>% filter(p.value < 0.05)
)$term %>% convert_terms_to_vars()

# fully_adjusted_condition (i)
# Z is associated with (i.e., not independent of) X given all other covariates
fully_adjusted_condition_i        <- rep(FALSE, length.out = length(fully_adjusted_vars_selected))
names(fully_adjusted_condition_i) <- fully_adjusted_vars_selected
for (var in fully_adjusted_vars_selected) {
  if (var %in% fully_adjusted_exposure_regression_significant_vars) {
    fully_adjusted_condition_i[var] <- TRUE
  }
}

# fully_adjusted_condition (ii)
# Z and Y are fully_adjusted_conditionally independent given X and all other covariates
fully_adjusted_condition_ii        <- rep(FALSE, length.out = length(fully_adjusted_vars_selected))
names(fully_adjusted_condition_ii) <- fully_adjusted_vars_selected
for (var in fully_adjusted_vars_selected) {
  if (!var %in% fully_adjusted_outcome_regression_significant_vars) {
    fully_adjusted_condition_ii[var] <- TRUE
  }
}

# fully_adjusted_conditions (i) and (ii)
fully_adjusted_conditions_i_and_ii        <- fully_adjusted_condition_i & fully_adjusted_condition_ii
names(fully_adjusted_conditions_i_and_ii) <- fully_adjusted_vars_selected

# test is TRUE if any covariate Z satisfies (i) and (ii)
# test is FALSE otherwise
fully_adjusted_conclusion <- any(fully_adjusted_conditions_i_and_ii)
fully_adjusted_conclusion_string <- ""
if (fully_adjusted_conclusion) {
  fully_adjusted_conclusion_string <- "Covariate set is sufficient for confounding adjustment"
} else {
  fully_adjusted_conclusion_string <- "Test is inconclusive, covariate set may or may not be sufficient"
}

fully_adjusted_test_table <- cbind(
  fully_adjusted_condition_i,
  fully_adjusted_condition_ii,
  fully_adjusted_conditions_i_and_ii
)
colnames(fully_adjusted_test_table) <- c("condition_i", "condition_ii", "condition_i_and_ii")
rownames(fully_adjusted_test_table) <- fully_adjusted_vars_selected



# Test lasso variable selection results ----------------
print("Performing empirical unconfoundedness plausibility test on lasso variable selection results")

# make formula strings for both regressions
# outcome regression is cox model, exposure regression is logistic
lasso_exposure_regression_formula <- make_exposure_formula(
  vars_selected = lasso_vars_selected
)
lasso_outcome_regression_formula  <- make_outcome_formula(
  vars_selected = lasso_vars_selected,
  outcome = outcome
)

# fit regression models for outcome and for exposure
# outcome regression is cox model, exposure regression is logistic
lasso_exposure_regression <- glm(
  lasso_exposure_regression_formula,
  family = "binomial",
  data   = logistic_df
)
lasso_outcome_regression  <- coxph(
  formula = as.formula(lasso_outcome_regression_formula),
  data    = cox_df
)

# extract regression results
# coefficients, SEs, p_values
lasso_exposure_regression_results <- (
  lasso_exposure_regression %>%
    broom::tidy() %>%
    as.data.frame()
)
lasso_outcome_regression_results  <- (
  lasso_outcome_regression %>%
    broom::tidy() %>%
    as.data.frame()
)

# extract significant variables from both models
lasso_outcome_regression_significant_vars <- (
  lasso_outcome_regression_results  %>% filter(p.value < 0.05)
)$term %>% convert_terms_to_vars()

lasso_exposure_regression_significant_vars <- (
  lasso_exposure_regression_results  %>% filter(p.value < 0.05)
)$term %>% convert_terms_to_vars()

# lasso_condition (i)
# Z is associated with (i.e., not independent of) X given all other covariates
lasso_condition_i        <- rep(FALSE, length.out = length(lasso_vars_selected))
names(lasso_condition_i) <- lasso_vars_selected
for (var in lasso_vars_selected) {
  if (var %in% lasso_exposure_regression_significant_vars) {
    lasso_condition_i[var] <- TRUE
  }
}

# lasso_condition (ii)
# Z and Y are lasso_conditionally independent given X and all other covariates
lasso_condition_ii        <- rep(FALSE, length.out = length(lasso_vars_selected))
names(lasso_condition_ii) <- lasso_vars_selected
for (var in lasso_vars_selected) {
  if (!var %in% lasso_outcome_regression_significant_vars) {
    lasso_condition_ii[var] <- TRUE
  }
}

# lasso_conditions (i) and (ii)
lasso_conditions_i_and_ii        <- lasso_condition_i & lasso_condition_ii
names(lasso_conditions_i_and_ii) <- lasso_vars_selected

# test is TRUE if any covariate Z satisfies (i) and (ii)
# test is FALSE otherwise
lasso_conclusion <- any(lasso_conditions_i_and_ii)
lasso_conclusion_string <- ""
if (lasso_conclusion) {
  lasso_conclusion_string <- "Covariate set is sufficient for confounding adjustment"
} else {
  lasso_conclusion_string <- "Test is inconclusive, covariate set may or may not be sufficient"
}

lasso_test_table <- cbind(
  lasso_condition_i,
  lasso_condition_ii,
  lasso_conditions_i_and_ii
)
colnames(lasso_test_table) <- c("condition_i", "condition_ii", "condition_i_and_ii")
rownames(lasso_test_table) <- lasso_vars_selected



# Test lasso_X variable selection results ----------------
print("Performing empirical unconfoundedness plausibility test on lasso_X variable selection results")

# make formula strings for both regressions
# outcome regression is cox model, exposure regression is logistic
lasso_X_exposure_regression_formula <- make_exposure_formula(
  vars_selected = lasso_X_vars_selected
)
lasso_X_outcome_regression_formula  <- make_outcome_formula(
  vars_selected = lasso_X_vars_selected,
  outcome = outcome
)

# fit regression models for outcome and for exposure
# outcome regression is cox model, exposure regression is logistic
lasso_X_exposure_regression <- glm(
  lasso_X_exposure_regression_formula,
  family = "binomial",
  data   = logistic_df
)
lasso_X_outcome_regression  <- coxph(
  formula = as.formula(lasso_X_outcome_regression_formula),
  data    = cox_df
)

# extract regression results
# coefficients, SEs, p_values
lasso_X_exposure_regression_results <- (
  lasso_X_exposure_regression %>%
    broom::tidy() %>%
    as.data.frame()
)
lasso_X_outcome_regression_results  <- (
  lasso_X_outcome_regression %>%
    broom::tidy() %>%
    as.data.frame()
)

# extract significant variables from both models
lasso_X_outcome_regression_significant_vars <- (
  lasso_X_outcome_regression_results  %>% filter(p.value < 0.05)
)$term %>% convert_terms_to_vars()

lasso_X_exposure_regression_significant_vars <- (
  lasso_X_exposure_regression_results  %>% filter(p.value < 0.05)
)$term %>% convert_terms_to_vars()

# lasso_X_condition (i)
# Z is associated with (i.e., not independent of) X given all other covariates
lasso_X_condition_i        <- rep(FALSE, length.out = length(lasso_X_vars_selected))
names(lasso_X_condition_i) <- lasso_X_vars_selected
for (var in lasso_X_vars_selected) {
  if (var %in% lasso_X_exposure_regression_significant_vars) {
    lasso_X_condition_i[var] <- TRUE
  }
}

# lasso_X_condition (ii)
# Z and Y are lasso_X_conditionally independent given X and all other covariates
lasso_X_condition_ii        <- rep(FALSE, length.out = length(lasso_X_vars_selected))
names(lasso_X_condition_ii) <- lasso_X_vars_selected
for (var in lasso_X_vars_selected) {
  if (!var %in% lasso_X_outcome_regression_significant_vars) {
    lasso_X_condition_ii[var] <- TRUE
  }
}

# lasso_X_conditions (i) and (ii)
lasso_X_conditions_i_and_ii        <- lasso_X_condition_i & lasso_X_condition_ii
names(lasso_X_conditions_i_and_ii) <- lasso_X_vars_selected

# test is TRUE if any covariate Z satisfies (i) and (ii)
# test is FALSE otherwise
lasso_X_conclusion <- any(lasso_X_conditions_i_and_ii)
lasso_X_conclusion_string <- ""
if (lasso_X_conclusion) {
  lasso_X_conclusion_string <- "Covariate set is sufficient for confounding adjustment"
} else {
  lasso_X_conclusion_string <- "Test is inconclusive, covariate set may or may not be sufficient"
}

lasso_X_test_table <- cbind(
  lasso_X_condition_i,
  lasso_X_condition_ii,
  lasso_X_conditions_i_and_ii
)
colnames(lasso_X_test_table) <- c("condition_i", "condition_ii", "condition_i_and_ii")
rownames(lasso_X_test_table) <- lasso_X_vars_selected



# Test lasso_union variable selection results ----------------
print("Performing empirical unconfoundedness plausibility test on lasso_union variable selection results")

# make formula strings for both regressions
# outcome regression is cox model, exposure regression is logistic
lasso_union_exposure_regression_formula <- make_exposure_formula(
  vars_selected = lasso_union_vars_selected
)
lasso_union_outcome_regression_formula  <- make_outcome_formula(
  vars_selected = lasso_union_vars_selected,
  outcome = outcome
)

# fit regression models for outcome and for exposure
# outcome regression is cox model, exposure regression is logistic
lasso_union_exposure_regression <- glm(
  lasso_union_exposure_regression_formula,
  family = "binomial",
  data   = logistic_df
)
lasso_union_outcome_regression  <- coxph(
  formula = as.formula(lasso_union_outcome_regression_formula),
  data    = cox_df
)

# extract regression results
# coefficients, SEs, p_values
lasso_union_exposure_regression_results <- (
  lasso_union_exposure_regression %>%
    broom::tidy() %>%
    as.data.frame()
)
lasso_union_outcome_regression_results  <- (
  lasso_union_outcome_regression %>%
    broom::tidy() %>%
    as.data.frame()
)

# extract significant variables from both models
lasso_union_outcome_regression_significant_vars <- (
  lasso_union_outcome_regression_results  %>% filter(p.value < 0.05)
)$term %>% convert_terms_to_vars()

lasso_union_exposure_regression_significant_vars <- (
  lasso_union_exposure_regression_results  %>% filter(p.value < 0.05)
)$term %>% convert_terms_to_vars()

# lasso_union_condition (i)
# Z is associated with (i.e., not independent of) X given all other covariates
lasso_union_condition_i        <- rep(FALSE, length.out = length(lasso_union_vars_selected))
names(lasso_union_condition_i) <- lasso_union_vars_selected
for (var in lasso_union_vars_selected) {
  if (var %in% lasso_union_exposure_regression_significant_vars) {
    lasso_union_condition_i[var] <- TRUE
  }
}

# lasso_union_condition (ii)
# Z and Y are lasso_union_conditionally independent given X and all other covariates
lasso_union_condition_ii        <- rep(FALSE, length.out = length(lasso_union_vars_selected))
names(lasso_union_condition_ii) <- lasso_union_vars_selected
for (var in lasso_union_vars_selected) {
  if (!var %in% lasso_union_outcome_regression_significant_vars) {
    lasso_union_condition_ii[var] <- TRUE
  }
}

# lasso_union_conditions (i) and (ii)
lasso_union_conditions_i_and_ii        <- lasso_union_condition_i & lasso_union_condition_ii
names(lasso_union_conditions_i_and_ii) <- lasso_union_vars_selected

# test is TRUE if any covariate Z satisfies (i) and (ii)
# test is FALSE otherwise
lasso_union_conclusion <- any(lasso_union_conditions_i_and_ii)
lasso_union_conclusion_string <- ""
if (lasso_union_conclusion) {
  lasso_union_conclusion_string <- "Covariate set is sufficient for confounding adjustment"
} else {
  lasso_union_conclusion_string <- "Test is inconclusive, covariate set may or may not be sufficient"
}

lasso_union_test_table <- cbind(
  lasso_union_condition_i,
  lasso_union_condition_ii,
  lasso_union_conditions_i_and_ii
)
colnames(lasso_union_test_table) <- c("condition_i", "condition_ii", "condition_i_and_ii")
rownames(lasso_union_test_table) <- lasso_union_vars_selected



# Conclusion table for all var sets -------------------------------------------
print("Conclusion table for all var sets")

all_var_sets_conclusion_table <- cbind(
  c("fully_adjusted", "lasso", "lasso_X", "lasso_union"),
  c(fully_adjusted_conclusion, lasso_conclusion, lasso_X_conclusion, lasso_union_conclusion),
  c(fully_adjusted_conclusion_string, lasso_conclusion_string, lasso_X_conclusion_string, lasso_union_conclusion_string)
)



# Save results ----------------------------------------------------------------

write.csv(
  all_var_sets_conclusion_table,
  paste0(unconfoundedness_test_dir, "all_var_sets_conclusion_table-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  fully_adjusted_exposure_regression_results,
  paste0(unconfoundedness_test_dir, "fully_adjusted_exposure_regression_results-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  fully_adjusted_outcome_regression_results,
  paste0(unconfoundedness_test_dir, "fully_adjusted_outcome_regression_results-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  fully_adjusted_test_table,
  paste0(unconfoundedness_test_dir, "fully_adjusted_test_table-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_exposure_regression_results,
  paste0(unconfoundedness_test_dir, "lasso_exposure_regression_results-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_outcome_regression_results,
  paste0(unconfoundedness_test_dir, "lasso_outcome_regression_results-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_test_table,
  paste0(unconfoundedness_test_dir, "lasso_test_table-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_exposure_regression_results,
  paste0(unconfoundedness_test_dir, "lasso_X_exposure_regression_results-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_outcome_regression_results,
  paste0(unconfoundedness_test_dir, "lasso_X_outcome_regression_results-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_test_table,
  paste0(unconfoundedness_test_dir, "lasso_X_test_table-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_exposure_regression_results,
  paste0(unconfoundedness_test_dir, "lasso_union_exposure_regression_results-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_outcome_regression_results,
  paste0(unconfoundedness_test_dir, "lasso_union_outcome_regression_results-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_test_table,
  paste0(unconfoundedness_test_dir, "lasso_union_test_table-", name, ".csv"),
  row.names = TRUE
)
