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

print(length(args))

if (length(args) == 0) {
  # default argument values
  name    <- "cohort_prevax-main-ami"
  cohort  <- "prevax"
  age_str <- "18;30;40;50;50;70;80;90"
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

age_bounds <- as.numeric(stringr::str_split(as.vector(age_str), ";")[[1]])
preex_string <- ""


# Load data --------------------------------------------------------------------
print("Load data")

model_input_df <- readr::read_rds(paste0(
  "output/model/model_input-",
  name,
  ".rds"
))


# Cox data setup ---------------------------------------------------------------
print("Cox data matrix setup")

model_input_df$binary_outcome          <- !is.na(model_input_df$out_date)
model_input_df$binary_covid19_exposure <- !is.na(model_input_df$exp_date)

model_input_df <- (model_input_df %>% select(!c(patient_id, index_date, exp_date)))

model_input_df$outcome_cox_dates <- rep(as.Date(NA), times = nrow(model_input_df))
for (i in c(1:nrow(model_input_df))) {
  if (model_input_df$binary_outcome[i]) {
    model_input_df$outcome_cox_dates[i] <- model_input_df$out_date[i]
  } else {
    model_input_df$outcome_cox_dates[i] <- model_input_df$end_date_outcome[i]
  }
}


# Test LASSO selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on LASSO results")

method <- "lasso"
vars_selected <- read.csv(paste0("output/lasso_var_selection/lasso_var_selection-", name, ".csv"))[, 'x']
vars_selected_without_exposure <- vars_selected[vars_selected != "binary_covid19_exposure"]
vars_selected_without_exposure <- vars_selected_without_exposure[vars_selected_without_exposure != "exp_date"]

## exposure logistic regression

if (length(vars_selected_without_exposure) > 1) {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ ", vars_selected_without_exposure[1])
  for (i in c(2:length(vars_selected_without_exposure))) {
    var <- vars_selected_without_exposure[i]
    exposure_regression_formula <- paste0(exposure_regression_formula, " + ", var)
  }
} else if (length(vars_selected_without_exposure) == 1) {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ ", vars_selected_without_exposure[1])
} else {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ cov_cat_ethnicity")
}

exposure_regression       <- glm(exposure_regression_formula,
                                 family = binomial(link = 'logit'),
                                 data   = model_input_df)
exposure_significance     <- ((summary(exposure_regression)$coefficients)[, "Pr(>|z|)"] <= 0.05)
exposure_significant_vars <- names(exposure_significance[exposure_significance == TRUE])
exposure_significant_vars <- exposure_significant_vars[exposure_significant_vars != "(Intercept)"]


## outcome cox regression

if (length(vars_selected_without_exposure) > 1) {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure + ", vars_selected_without_exposure[1])
  for (i in c(2:length(vars_selected_without_exposure))) {
    var <- vars_selected_without_exposure[i]
    outcome_regression_formula <- paste0(outcome_regression_formula, " + ", var)
  }
} else if (length(vars_selected_without_exposure) == 1) {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure + ", vars_selected_without_exposure[1])
} else {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure")
}

print(outcome_regression_formula)
print(("cov_bin_hf" %in% colnames(model_input_df)))

outcome_regression <- coxph(formula = eval(parse(text = outcome_regression_formula)),
                            data    = model_input_df)

outcome_significance     <- ((summary(outcome_regression)$coefficients)[, "Pr(>|z|)"] <= 0.05)
outcome_significant_vars <- names(outcome_significance[outcome_significance == TRUE])
outcome_significant_vars <- outcome_significant_vars[outcome_significant_vars != "(Intercept)"]

## check empirical unconfoundedness plausibility test condition

both_significant_vars <- intersect(exposure_significant_vars, outcome_significant_vars)

if (length(both_significant_vars) == 0) {
  condition <- FALSE
} else {
  condition <- TRUE
}

conclusion <- ""
if (condition) {
  conclusion <- "covariate set is sufficient for confounding adjustment"
} else {
  conclusion <- "Test is inconclusive, covariate set may or may not be sufficient"
}

print("***** LASSO *****")

print("Variables selected:")
print(vars_selected)

print("Exposure regression:")
print(exposure_regression_formula)
print(summary(exposure_regression))
print(exposure_significance)
print(exposure_significant_vars)

print("Outcome regression:")
print(outcome_regression_formula)
print(summary(outcome_regression))
print(outcome_significance)
print(outcome_significant_vars)

print("Conclusion:")
print(both_significant_vars)
print(condition)
print(conclusion)

lasso_results <- c(method,
                   paste(vars_selected,             collapse = "\n"),
                   paste(exposure_significant_vars, collapse = "\n"),
                   paste(outcome_significant_vars,  collapse = "\n"),
                   paste(both_significant_vars,     collapse = "\n"),
                   condition,
                   conclusion)


# Test lasso_X selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on lasso_X results")

method <- "lasso_X"
vars_selected <- read.csv(paste0("output/lasso_X_var_selection/lasso_X_var_selection-", name, ".csv"))[, 'x']
vars_selected_without_exposure <- vars_selected[vars_selected != "binary_covid19_exposure"]
vars_selected_without_exposure <- vars_selected_without_exposure[vars_selected_without_exposure != "exp_date"]

## exposure logistic regression

if (length(vars_selected_without_exposure) > 1) {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ ", vars_selected_without_exposure[1])
  for (i in c(2:length(vars_selected_without_exposure))) {
    var <- vars_selected_without_exposure[i]
    exposure_regression_formula <- paste0(exposure_regression_formula, " + ", var)
  }
} else if (length(vars_selected_without_exposure) == 1) {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ ", vars_selected_without_exposure[1])
} else {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ cov_cat_ethnicity")
}

exposure_regression       <- glm(exposure_regression_formula,
                                 family = binomial(link = 'logit'),
                                 data   = model_input_df)
exposure_significance     <- ((summary(exposure_regression)$coefficients)[, "Pr(>|z|)"] <= 0.05)
exposure_significant_vars <- names(exposure_significance[exposure_significance == TRUE])
exposure_significant_vars <- exposure_significant_vars[exposure_significant_vars != "(Intercept)"]


## outcome cox regression

if (length(vars_selected_without_exposure) > 1) {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure + ", vars_selected_without_exposure[1])
  for (i in c(2:length(vars_selected_without_exposure))) {
    var <- vars_selected_without_exposure[i]
    outcome_regression_formula <- paste0(outcome_regression_formula, " + ", var)
  }
} else if (length(vars_selected_without_exposure) == 1) {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure + ", vars_selected_without_exposure[1])
} else {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure")
}

print(outcome_regression_formula)
print(("cov_bin_hf" %in% colnames(model_input_df)))

outcome_regression <- coxph(formula = eval(parse(text = outcome_regression_formula)),
                            data    = model_input_df)

outcome_significance     <- ((summary(outcome_regression)$coefficients)[, "Pr(>|z|)"] <= 0.05)
outcome_significant_vars <- names(outcome_significance[outcome_significance == TRUE])
outcome_significant_vars <- outcome_significant_vars[outcome_significant_vars != "(Intercept)"]

## check empirical unconfoundedness plausibility test condition

both_significant_vars <- intersect(exposure_significant_vars, outcome_significant_vars)

if (length(both_significant_vars) == 0) {
  condition <- FALSE
} else {
  condition <- TRUE
}

conclusion <- ""
if (condition) {
  conclusion <- "covariate set is sufficient for confounding adjustment"
} else {
  conclusion <- "Test is inconclusive, covariate set may or may not be sufficient"
}

print("***** lasso_X *****")

print("Variables selected:")
print(vars_selected)

print("Exposure regression:")
print(exposure_regression_formula)
print(summary(exposure_regression))
print(exposure_significance)
print(exposure_significant_vars)

print("Outcome regression:")
print(outcome_regression_formula)
print(summary(outcome_regression))
print(outcome_significance)
print(outcome_significant_vars)

print("Conclusion:")
print(both_significant_vars)
print(condition)
print(conclusion)

lasso_X_results <- c(method,
                   paste(vars_selected,             collapse = "\n"),
                   paste(exposure_significant_vars, collapse = "\n"),
                   paste(outcome_significant_vars,  collapse = "\n"),
                   paste(both_significant_vars,     collapse = "\n"),
                   condition,
                   conclusion)


# Test lasso_union selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on lasso_union results")

method <- "lasso_union"
vars_selected <- read.csv(paste0("output/lasso_union_var_selection/lasso_union_var_selection-", name, ".csv"))[, 'x']
vars_selected_without_exposure <- vars_selected[vars_selected != "binary_covid19_exposure"]
vars_selected_without_exposure <- vars_selected_without_exposure[vars_selected_without_exposure != "exp_date"]

## exposure logistic regression

if (length(vars_selected_without_exposure) > 1) {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ ", vars_selected_without_exposure[1])
  for (i in c(2:length(vars_selected_without_exposure))) {
    var <- vars_selected_without_exposure[i]
    exposure_regression_formula <- paste0(exposure_regression_formula, " + ", var)
  }
} else if (length(vars_selected_without_exposure) == 1) {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ ", vars_selected_without_exposure[1])
} else {
  exposure_regression_formula    <- paste0("binary_covid19_exposure ~ cov_cat_ethnicity")
}

exposure_regression       <- glm(exposure_regression_formula,
                                 family = binomial(link = 'logit'),
                                 data   = model_input_df)
exposure_significance     <- ((summary(exposure_regression)$coefficients)[, "Pr(>|z|)"] <= 0.05)
exposure_significant_vars <- names(exposure_significance[exposure_significance == TRUE])
exposure_significant_vars <- exposure_significant_vars[exposure_significant_vars != "(Intercept)"]


## outcome cox regression

if (length(vars_selected_without_exposure) > 1) {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure + ", vars_selected_without_exposure[1])
  for (i in c(2:length(vars_selected_without_exposure))) {
    var <- vars_selected_without_exposure[i]
    outcome_regression_formula <- paste0(outcome_regression_formula, " + ", var)
  }
} else if (length(vars_selected_without_exposure) == 1) {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure + ", vars_selected_without_exposure[1])
} else {
  outcome_regression_formula    <- paste0("Surv(as.numeric(outcome_cox_dates), binary_outcome) ~ binary_covid19_exposure")
}

print(outcome_regression_formula)
print(("cov_bin_hf" %in% colnames(model_input_df)))

outcome_regression <- coxph(formula = eval(parse(text = outcome_regression_formula)),
                            data    = model_input_df)

outcome_significance     <- ((summary(outcome_regression)$coefficients)[, "Pr(>|z|)"] <= 0.05)
outcome_significant_vars <- names(outcome_significance[outcome_significance == TRUE])
outcome_significant_vars <- outcome_significant_vars[outcome_significant_vars != "(Intercept)"]

## check empirical unconfoundedness plausibility test condition

both_significant_vars <- intersect(exposure_significant_vars, outcome_significant_vars)

if (length(both_significant_vars) == 0) {
  condition <- FALSE
} else {
  condition <- TRUE
}

conclusion <- ""
if (condition) {
  conclusion <- "covariate set is sufficient for confounding adjustment"
} else {
  conclusion <- "Test is inconclusive, covariate set may or may not be sufficient"
}

print("***** lasso_union *****")

print("Variables selected:")
print(vars_selected)

print("Exposure regression:")
print(exposure_regression_formula)
print(summary(exposure_regression))
print(exposure_significance)
print(exposure_significant_vars)

print("Outcome regression:")
print(outcome_regression_formula)
print(summary(outcome_regression))
print(outcome_significance)
print(outcome_significant_vars)

print("Conclusion:")
print(both_significant_vars)
print(condition)
print(conclusion)

lasso_union_results <- c(method,
                   paste(vars_selected,             collapse = "\n"),
                   paste(exposure_significant_vars, collapse = "\n"),
                   paste(outcome_significant_vars,  collapse = "\n"),
                   paste(both_significant_vars,     collapse = "\n"),
                   condition,
                   conclusion)



# Put together results table --------------------------------------------------

results <- array(
    data = NaN,
    dim = c(3, 7),
    dimnames = list(c(1:3), c("method", "vars_selected", "exposure_significant_vars", "outcome_significant_vars", "both_significant_vars", "Condition", "Conclusion"))
)

results[1,] <- lasso_results
results[2,] <- lasso_X_results
results[3,] <- lasso_union_results

results_table <- data.frame(results)
print(results_table)


# Save results ----------------------------------------------------------------

write.csv(
  results_table,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test-", name, ".csv"),
  row.names = FALSE
)

