# ------------------------------------------------------------------------------
#
# unconfoundedness_test.R
#
# This file implements the unconfoundedness plausibility test
# which was the main result of Hartwig et al., 2024
# Original text: https://doi.org/10.48550/arXiv.2402.10156
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
  cohort  <- "prevax"
  age_str <- "18;30;40;50;50;70;80;90"
  preex   <- FALSE
} else {
  # YAML arguments
  cohort  <- args[[1]]
  age_str <- args[[2]]

  # optional argument
  if (length(args) < 3) {
    preex <- "All"
  } else {
    preex <- args[[3]]
  } # allow an empty input for the preex variable
}

age_bounds <- as.numeric(stringr::str_split(as.vector(age_str), ";")[[1]])
preex_string <- ""

# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_rds(paste0(
  "output/dataset_clean/input_",
  cohort,
  "_clean.rds"
))


# Processing Start -------------------------------------------------------------
print("Processing Start")

# Remove people with history of COVID-19 ---------------------------------------
print("Remove people with history of COVID-19")

df <- df[df$sub_bin_covidhistory == FALSE, ]

# Create exposure indicator ----------------------------------------------------
print("Create exposure indicator")

df$exposed <- !is.na(df$exp_date_covid)

# Select for pre-existing conditions
print("Select for pre-existing conditions")

preex_string <- ""
if (preex != "All") {
  df <- df[df$sup_bin_preex == preex, ]
  preex_string <- paste0("-preex_", preex)
}


# Define age groups ------------------------------------------------------------
print("Define age groups")

df$cov_cat_age_group <- numerical_to_categorical(df$cov_num_age, age_bounds) # See utility.R

df$cov_cat_consrate2019 <- numerical_to_categorical(
  df$cov_num_consrate2019,
  c(1, 6),
  zero_flag = TRUE
)

median_iqr_age <- create_median_iqr_string(df$cov_num_age) # See utility.R

# Filter data ------------------------------------------------------------------
print("Filter data")

df <- df[, c(
  "patient_id",
  "exposed",
  colnames(df)[grepl("cov_cat_", colnames(df))],
  colnames(df)[grepl("strat_cat_", colnames(df))],
  colnames(df)[grepl("cov_bin_", colnames(df))]
)]

df$All <- "All"

# Filter binary data -----------------------------------------------------------
print("Filter binary data")

for (colname in colnames(df)[grepl("cov_bin_", colnames(df))]) {
  df[[colname]] <- sapply(df[[colname]], as.character)
}

df <- df %>%
  mutate(across(where(is.factor), as.character))


# Check exposure and outcome (acute MI) ----------------------------------------
print("Check exposure and outcome (acute MI)")

print("Exposure")
print(head(df$exposed))

print("Outcome")
print(head(df$cov_bin_ami))


# Convert explicit data type to binary where applicable ------------------------
print("Convert explicit data type to binary where applicable")

# All binary columns:
# exposed
# cov_bin_carehome cov_bin_hcworker cov_bin_dementia cov_bin_liver_disease
# cov_bin_ckd     cov_bin_cancer  cov_bin_hypertension cov_bin_diabetes
# cov_bin_obesity cov_bin_copd    cov_bin_ami     cov_bin_depression
# cov_bin_stroke_all cov_bin_other_ae cov_bin_vte     cov_bin_hf    
# cov_bin_angina  cov_bin_lipidmed cov_bin_antiplatelet cov_bin_anticoagulant
# cov_bin_cocp    cov_bin_hrt

df$exposed <- as.logical(df$exposed)
df$cov_bin_carehome <- as.logical(df$cov_bin_carehome)
df$cov_bin_hcworker <- as.logical(df$cov_bin_hcworker)
df$cov_bin_dementia <- as.logical(df$cov_bin_dementia)
df$cov_bin_liver_disease <- as.logical(df$cov_bin_liver_disease)
df$cov_bin_ckd <- as.logical(df$cov_bin_ckd)
df$cov_bin_cancer <- as.logical(df$cov_bin_cancer)
df$cov_bin_hypertension <- as.logical(df$cov_bin_hypertension)
df$cov_bin_diabetes <- as.logical(df$cov_bin_diabetes)
df$cov_bin_obesity <- as.logical(df$cov_bin_obesity)
df$cov_bin_copd <- as.logical(df$cov_bin_copd)
df$cov_bin_ami <- as.logical(df$cov_bin_ami)
df$cov_bin_depression <- as.logical(df$cov_bin_depression)
df$cov_bin_stroke_all <- as.logical(df$cov_bin_stroke_all)
df$cov_bin_other_ae <- as.logical(df$cov_bin_other_ae)
df$cov_bin_vte <- as.logical(df$cov_bin_vte)
df$cov_bin_hf <- as.logical(df$cov_bin_hf)
df$cov_bin_angina <- as.logical(df$cov_bin_angina)
df$cov_bin_lipidmed <- as.logical(df$cov_bin_lipidmed)
df$cov_bin_antiplatelet <- as.logical(df$cov_bin_antiplatelet)
df$cov_bin_anticoagulant <- as.logical(df$cov_bin_anticoagulant)
df$cov_bin_cocp <- as.logical(df$cov_bin_cocp)
df$cov_bin_hrt <- as.logical(df$cov_bin_hrt)


# Test LASSO selection --------------------------------------------------------

method <- "LASSO"
vars_selected <- read.csv(paste0("output/lasso_var_selection/lasso_var_selection-cohort_", cohort, ".csv"))[, 'x']

if (length(vars_selected) > 1) {
  exposure_regression_formula    <- paste0("exposed ~ ", vars_selected[1])
  for (i in c(2:length(vars_selected))) {
    var <- vars_selected[i]
    exposure_regression_formula <- paste0(exposure_regression_formula, " + ", var)
  }
} else if (length(vars_selected) == 1) {
  exposure_regression_formula    <- paste0("exposed ~ ", vars_selected[1])
} else {
  exposure_regression_formula    <- paste0("exposed ~ 0")
}

exposure_regression       <- lm(exposure_regression_formula, data = df)
exposure_significance     <- (summary(exposure_regression)$coefficients[, "Pr(>|t|)"] <= 0.05)[-c(1)] # remove intercept term
exposure_significant_vars <- names(exposure_significance[exposure_significance == TRUE])

if (length(vars_selected) > 1) {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed + ", vars_selected[1])
  for (i in c(2:length(vars_selected))) {
    var <- vars_selected[i]
    outcome_regression_formula <- paste0(outcome_regression_formula, " + ", var)
  }
} else if (length(vars_selected) == 1) {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed + ", vars_selected[1])
} else {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed")
}

outcome_regression       <- lm(outcome_regression_formula, data = df)
outcome_significance     <- (summary(outcome_regression)$coefficients[, "Pr(>|t|)"] <= 0.05)[-c(1)] # remove intercept term
outcome_significant_vars <- names(outcome_significance[outcome_significance == TRUE])

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


# Test LASSO_X selection --------------------------------------------------------

method <- "LASSO_X"
vars_selected <- read.csv(paste0("output/lasso_X_var_selection/lasso_X_var_selection-cohort_", cohort, ".csv"))[, 'x']

if (length(vars_selected) > 1) {
  exposure_regression_formula    <- paste0("exposed ~ ", vars_selected[1])
  for (i in c(2:length(vars_selected))) {
    var <- vars_selected[i]
    exposure_regression_formula <- paste0(exposure_regression_formula, " + ", var)
  }
} else if (length(vars_selected) == 1) {
  exposure_regression_formula    <- paste0("exposed ~ ", vars_selected[1])
} else {
  exposure_regression_formula    <- paste0("exposed ~ 0")
}

exposure_regression       <- lm(exposure_regression_formula, data = df)
exposure_significance     <- (summary(exposure_regression)$coefficients[, "Pr(>|t|)"] <= 0.05)[-c(1)] # remove intercept term
exposure_significant_vars <- names(exposure_significance[exposure_significance == TRUE])

if (length(vars_selected) > 1) {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed + ", vars_selected[1])
  for (i in c(2:length(vars_selected))) {
    var <- vars_selected[i]
    outcome_regression_formula <- paste0(outcome_regression_formula, " + ", var)
  }
} else if (length(vars_selected) == 1) {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed + ", vars_selected[1])
} else {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed")
}

outcome_regression       <- lm(outcome_regression_formula, data = df)
outcome_significance     <- (summary(outcome_regression)$coefficients[, "Pr(>|t|)"] <= 0.05)[-c(1)] # remove intercept term
outcome_significant_vars <- names(outcome_significance[outcome_significance == TRUE])

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

print("***** LASSO_X *****")

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



# Test LASSO_UNION selection --------------------------------------------------------

method <- "LASSO_UNION"
vars_selected <- read.csv(paste0("output/lasso_union_var_selection/lasso_union_var_selection-cohort_", cohort, ".csv"))[, 'x']

if (length(vars_selected) > 1) {
  exposure_regression_formula    <- paste0("exposed ~ ", vars_selected[1])
  for (i in c(2:length(vars_selected))) {
    var <- vars_selected[i]
    exposure_regression_formula <- paste0(exposure_regression_formula, " + ", var)
  }
} else if (length(vars_selected) == 1) {
  exposure_regression_formula    <- paste0("exposed ~ ", vars_selected[1])
} else {
  exposure_regression_formula    <- paste0("exposed ~ 0")
}

exposure_regression       <- lm(exposure_regression_formula, data = df)
exposure_significance     <- (summary(exposure_regression)$coefficients[, "Pr(>|t|)"] <= 0.05)[-c(1)] # remove intercept term
exposure_significant_vars <- names(exposure_significance[exposure_significance == TRUE])

if (length(vars_selected) > 1) {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed + ", vars_selected[1])
  for (i in c(2:length(vars_selected))) {
    var <- vars_selected[i]
    outcome_regression_formula <- paste0(outcome_regression_formula, " + ", var)
  }
} else if (length(vars_selected) == 1) {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed + ", vars_selected[1])
} else {
  outcome_regression_formula    <- paste0("cov_bin_ami ~ exposed")
}

outcome_regression       <- lm(outcome_regression_formula, data = df)
outcome_significance     <- (summary(outcome_regression)$coefficients[, "Pr(>|t|)"] <= 0.05)[-c(1)] # remove intercept term
outcome_significant_vars <- names(outcome_significance[outcome_significance == TRUE])

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

print("***** LASSO_UNION *****")

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
  paste0(unconfoundedness_test_dir, "unconfoundedness_test-cohort_", cohort, preex_string, ".csv"),
  row.names = FALSE
)
