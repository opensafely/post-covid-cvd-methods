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

fill_in_blanks <- function(p_values = NULL, labels = NULL) {
  for (label in labels) {
    # if a given variable doesn't exist, create as NaN
    if (!(label %in% names(p_values))) {
      p_values[label] <- NaN
    }
  }
  
  # assert variable ordering
  p_values <- p_values[order(factor(names(p_values), levels = labels))]
  
  return (p_values)
}

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


# Re-define missing variables --------------------------------------------------
print("Re-define missing variables")

model_input_df$cov_cat_age_group <- numerical_to_categorical(model_input_df$cov_num_age, age_bounds) # See utility.R


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


# Test lasso selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on lasso results")

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

exposure_regression <- glm(exposure_regression_formula,
                                 family = binomial(link = 'logit'),
                                 data   = model_input_df)

exposure_p_values        <- (summary(exposure_regression)$coefficients)[, "Pr(>|z|)"]
exposure_p_values        <- exposure_p_values[names(exposure_p_values) != "(Intercept)"]
exposure_p_values        <- exposure_p_values[names(exposure_p_values) != "binary_covid19_exposure"] # remove exposure

exposure_coefs           <- (summary(exposure_regression)$coefficients)[, "Estimate"]
exposure_coefs           <- exposure_coefs[names(exposure_coefs) != "(Intercept)"]
exposure_coefs           <- exposure_coefs[names(exposure_coefs) != "binary_covid19_exposure"] # remove exposure

exposure_standard_errors <- (summary(exposure_regression)$coefficients)[, "Std. Error"]
exposure_standard_errors <- exposure_standard_errors[names(exposure_standard_errors) != "(Intercept)"]
exposure_standard_errors <- exposure_standard_errors[names(exposure_standard_errors) != "binary_covid19_exposure"] # remove exposure


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

outcome_regression <- coxph(formula = eval(parse(text = outcome_regression_formula)),
                            data    = model_input_df)

outcome_p_values        <- (summary(outcome_regression)$coefficients)[, "Pr(>|z|)"]
outcome_p_values        <- outcome_p_values[names(outcome_p_values) != "(Intercept)"]
outcome_p_values        <- outcome_p_values[names(outcome_p_values) != "binary_covid19_exposure"] # remove exposure

outcome_coefs           <- (summary(outcome_regression)$coefficients)[, "coef"]
outcome_coefs           <- outcome_coefs[names(outcome_coefs) != "(Intercept)"]
outcome_coefs           <- outcome_coefs[names(outcome_coefs) != "binary_covid19_exposure"] # remove exposure

outcome_standard_errors <- (summary(outcome_regression)$coefficients)[, "se(coef)"]
outcome_standard_errors <- outcome_standard_errors[names(outcome_standard_errors) != "(Intercept)"]
outcome_standard_errors <- outcome_standard_errors[names(outcome_standard_errors) != "binary_covid19_exposure"] # remove exposure


## check empirical unconfoundedness plausibility test conditions

all_var_names <- union(names(exposure_p_values), names(outcome_p_values))

exposure_p_values <- fill_in_blanks(exposure_p_values, all_var_names)
outcome_p_values  <- fill_in_blanks(outcome_p_values,  all_var_names)

exposure_coefs <- fill_in_blanks(exposure_coefs, all_var_names)
outcome_coefs  <- fill_in_blanks(outcome_coefs,  all_var_names)

exposure_standard_errors <- fill_in_blanks(exposure_standard_errors, all_var_names)
outcome_standard_errors  <- fill_in_blanks(outcome_standard_errors,  all_var_names)

lasso_all_p_values <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_all_p_values["exposure", ] <- exposure_p_values
lasso_all_p_values["outcome", ]  <- outcome_p_values

lasso_all_coefs <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_all_coefs["exposure", ] <- exposure_coefs
lasso_all_coefs["outcome", ]  <- outcome_coefs

lasso_all_standard_errors <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_all_standard_errors["exposure", ] <- exposure_standard_errors
lasso_all_standard_errors["outcome", ]  <- outcome_standard_errors

# condition (i)
# Z is associated with (i.e., not independent of) X given all other covariates
condition_i <- rep(FALSE, length.out = length(all_var_names))
for (i in c(1:length(exposure_p_values))) {
  if (!is.nan(exposure_p_values[i])) {
    # check association is significant using corresponding p-value
    if (exposure_p_values[i] < 0.05) {
      condition_i[i] <- TRUE
    }
  }
}
names(condition_i) <- all_var_names

# condition (ii)
# Z and Y are conditionally independent given X and all other covariates
condition_ii <- rep(FALSE, length.out = length(all_var_names))
for (i in c(1:length(outcome_p_values))) {
  if (!is.nan(outcome_p_values[i])) {
    # check conditional independence using corresponding p-value
    if (outcome_p_values[i] >= 0.05) {
      condition_ii[i] <- TRUE
    }
  }
}
names(condition_ii) <- all_var_names

# conditions (i) and (ii)
conditions_i_and_ii <- condition_i & condition_ii
names(conditions_i_and_ii) <- all_var_names

# test is TRUE if any covariate Z satisfies (i) and (ii)
# test is FALSE otherwise
conclusion <- any(conditions_i_and_ii)
conclusion_string <- ""
if (conclusion) {
  conclusion_string <- "Covariate set is sufficient for confounding adjustment"
} else {
  conclusion_string <- "Test is inconclusive, covariate set may or may not be sufficient"
}

lasso_all_tests <- data.frame(array(data     = NaN,
                              dim      = c(3, length(all_var_names)),
                              dimnames = list(c("Condition (i)", "Condition (ii)", "Conditions (i) and (ii)"), all_var_names) ))
lasso_all_tests["Condition (i)", ]           <- condition_i
lasso_all_tests["Condition (ii)", ]          <- condition_ii
lasso_all_tests["Conditions (i) and (ii)", ] <- conditions_i_and_ii

message("\n\nlasso p values:")
print(lasso_all_p_values)

message("\n\nlasso all test conditions:")
print(lasso_all_tests)

message("\n\nfinal results:")
print(conclusion)
print(conclusion_string)

lasso_results <- c("lasso",
                   conclusion,
                   conclusion_string)



# Test lasso_X selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on lasso_X results")

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

exposure_regression <- glm(exposure_regression_formula,
                                 family = binomial(link = 'logit'),
                                 data   = model_input_df)

exposure_p_values        <- (summary(exposure_regression)$coefficients)[, "Pr(>|z|)"]
exposure_p_values        <- exposure_p_values[names(exposure_p_values) != "(Intercept)"]
exposure_p_values        <- exposure_p_values[names(exposure_p_values) != "binary_covid19_exposure"] # remove exposure

exposure_coefs           <- (summary(exposure_regression)$coefficients)[, "Estimate"]
exposure_coefs           <- exposure_coefs[names(exposure_coefs) != "(Intercept)"]
exposure_coefs           <- exposure_coefs[names(exposure_coefs) != "binary_covid19_exposure"] # remove exposure

exposure_standard_errors <- (summary(exposure_regression)$coefficients)[, "Std. Error"]
exposure_standard_errors <- exposure_standard_errors[names(exposure_standard_errors) != "(Intercept)"]
exposure_standard_errors <- exposure_standard_errors[names(exposure_standard_errors) != "binary_covid19_exposure"] # remove exposure


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

outcome_regression <- coxph(formula = eval(parse(text = outcome_regression_formula)),
                            data    = model_input_df)

outcome_p_values        <- (summary(outcome_regression)$coefficients)[, "Pr(>|z|)"]
outcome_p_values        <- outcome_p_values[names(outcome_p_values) != "(Intercept)"]
outcome_p_values        <- outcome_p_values[names(outcome_p_values) != "binary_covid19_exposure"] # remove exposure

outcome_coefs           <- (summary(outcome_regression)$coefficients)[, "coef"]
outcome_coefs           <- outcome_coefs[names(outcome_coefs) != "(Intercept)"]
outcome_coefs           <- outcome_coefs[names(outcome_coefs) != "binary_covid19_exposure"] # remove exposure

outcome_standard_errors <- (summary(outcome_regression)$coefficients)[, "se(coef)"]
outcome_standard_errors <- outcome_standard_errors[names(outcome_standard_errors) != "(Intercept)"]
outcome_standard_errors <- outcome_standard_errors[names(outcome_standard_errors) != "binary_covid19_exposure"] # remove exposure


## check empirical unconfoundedness plausibility test conditions

all_var_names <- union(names(exposure_p_values), names(outcome_p_values))

exposure_p_values <- fill_in_blanks(exposure_p_values, all_var_names)
outcome_p_values  <- fill_in_blanks(outcome_p_values,  all_var_names)

exposure_coefs <- fill_in_blanks(exposure_coefs, all_var_names)
outcome_coefs  <- fill_in_blanks(outcome_coefs,  all_var_names)

exposure_standard_errors <- fill_in_blanks(exposure_standard_errors, all_var_names)
outcome_standard_errors  <- fill_in_blanks(outcome_standard_errors,  all_var_names)

lasso_X_all_p_values <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_X_all_p_values["exposure", ] <- exposure_p_values
lasso_X_all_p_values["outcome", ]  <- outcome_p_values

lasso_X_all_coefs <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_X_all_coefs["exposure", ] <- exposure_coefs
lasso_X_all_coefs["outcome", ]  <- outcome_coefs

lasso_X_all_standard_errors <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_X_all_standard_errors["exposure", ] <- exposure_standard_errors
lasso_X_all_standard_errors["outcome", ]  <- outcome_standard_errors

# condition (i)
# Z is associated with (i.e., not independent of) X given all other covariates
condition_i <- rep(FALSE, length.out = length(all_var_names))
for (i in c(1:length(exposure_p_values))) {
  if (!is.nan(exposure_p_values[i])) {
    # check association is significant using corresponding p-value
    if (exposure_p_values[i] < 0.05) {
      condition_i[i] <- TRUE
    }
  }
}
names(condition_i) <- all_var_names

# condition (ii)
# Z and Y are conditionally independent given X and all other covariates
condition_ii <- rep(FALSE, length.out = length(all_var_names))
for (i in c(1:length(outcome_p_values))) {
  if (!is.nan(outcome_p_values[i])) {
    # check conditional independence using corresponding p-value
    if (outcome_p_values[i] >= 0.05) {
      condition_ii[i] <- TRUE
    }
  }
}
names(condition_ii) <- all_var_names

# conditions (i) and (ii)
conditions_i_and_ii <- condition_i & condition_ii
names(conditions_i_and_ii) <- all_var_names

# test is TRUE if any covariate Z satisfies (i) and (ii)
# test is FALSE otherwise
conclusion <- any(conditions_i_and_ii)
conclusion_string <- ""
if (conclusion) {
  conclusion_string <- "Covariate set is sufficient for confounding adjustment"
} else {
  conclusion_string <- "Test is inconclusive, covariate set may or may not be sufficient"
}

lasso_X_all_tests <- data.frame(array(data     = NaN,
                              dim      = c(3, length(all_var_names)),
                              dimnames = list(c("Condition (i)", "Condition (ii)", "Conditions (i) and (ii)"), all_var_names) ))
lasso_X_all_tests["Condition (i)", ]           <- condition_i
lasso_X_all_tests["Condition (ii)", ]          <- condition_ii
lasso_X_all_tests["Conditions (i) and (ii)", ] <- conditions_i_and_ii

message("\n\nlasso_X p values:")
print(lasso_X_all_p_values)

message("\n\nlasso_X all test conditions:")
print(lasso_X_all_tests)

message("\n\nfinal results:")
print(conclusion)
print(conclusion_string)

lasso_X_results <- c("lasso_X",
                   conclusion,
                   conclusion_string)


# Test lasso_union selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on lasso_union results")

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

exposure_regression <- glm(exposure_regression_formula,
                                 family = binomial(link = 'logit'),
                                 data   = model_input_df)

exposure_p_values        <- (summary(exposure_regression)$coefficients)[, "Pr(>|z|)"]
exposure_p_values        <- exposure_p_values[names(exposure_p_values) != "(Intercept)"]
exposure_p_values        <- exposure_p_values[names(exposure_p_values) != "binary_covid19_exposure"] # remove exposure

exposure_coefs           <- (summary(exposure_regression)$coefficients)[, "Estimate"]
exposure_coefs           <- exposure_coefs[names(exposure_coefs) != "(Intercept)"]
exposure_coefs           <- exposure_coefs[names(exposure_coefs) != "binary_covid19_exposure"] # remove exposure

exposure_standard_errors <- (summary(exposure_regression)$coefficients)[, "Std. Error"]
exposure_standard_errors <- exposure_standard_errors[names(exposure_standard_errors) != "(Intercept)"]
exposure_standard_errors <- exposure_standard_errors[names(exposure_standard_errors) != "binary_covid19_exposure"] # remove exposure


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

outcome_regression <- coxph(formula = eval(parse(text = outcome_regression_formula)),
                            data    = model_input_df)

outcome_p_values        <- (summary(outcome_regression)$coefficients)[, "Pr(>|z|)"]
outcome_p_values        <- outcome_p_values[names(outcome_p_values) != "(Intercept)"]
outcome_p_values        <- outcome_p_values[names(outcome_p_values) != "binary_covid19_exposure"] # remove exposure

outcome_coefs           <- (summary(outcome_regression)$coefficients)[, "coef"]
outcome_coefs           <- outcome_coefs[names(outcome_coefs) != "(Intercept)"]
outcome_coefs           <- outcome_coefs[names(outcome_coefs) != "binary_covid19_exposure"] # remove exposure

outcome_standard_errors <- (summary(outcome_regression)$coefficients)[, "se(coef)"]
outcome_standard_errors <- outcome_standard_errors[names(outcome_standard_errors) != "(Intercept)"]
outcome_standard_errors <- outcome_standard_errors[names(outcome_standard_errors) != "binary_covid19_exposure"] # remove exposure


## check empirical unconfoundedness plausibility test conditions

all_var_names <- union(names(exposure_p_values), names(outcome_p_values))

exposure_p_values <- fill_in_blanks(exposure_p_values, all_var_names)
outcome_p_values  <- fill_in_blanks(outcome_p_values,  all_var_names)

exposure_coefs <- fill_in_blanks(exposure_coefs, all_var_names)
outcome_coefs  <- fill_in_blanks(outcome_coefs,  all_var_names)

exposure_standard_errors <- fill_in_blanks(exposure_standard_errors, all_var_names)
outcome_standard_errors  <- fill_in_blanks(outcome_standard_errors,  all_var_names)

lasso_union_all_p_values <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_union_all_p_values["exposure", ] <- exposure_p_values
lasso_union_all_p_values["outcome", ]  <- outcome_p_values

lasso_union_all_coefs <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_union_all_coefs["exposure", ] <- exposure_coefs
lasso_union_all_coefs["outcome", ]  <- outcome_coefs

lasso_union_all_standard_errors <- data.frame(array(data     = NaN,
                                 dim      = c(2, length(all_var_names)),
                                 dimnames = list(c("exposure", "outcome"), all_var_names) ))
lasso_union_all_standard_errors["exposure", ] <- exposure_standard_errors
lasso_union_all_standard_errors["outcome", ]  <- outcome_standard_errors

# condition (i)
# Z is associated with (i.e., not independent of) X given all other covariates
condition_i <- rep(FALSE, length.out = length(all_var_names))
for (i in c(1:length(exposure_p_values))) {
  if (!is.nan(exposure_p_values[i])) {
    # check association is significant using corresponding p-value
    if (exposure_p_values[i] < 0.05) {
      condition_i[i] <- TRUE
    }
  }
}
names(condition_i) <- all_var_names

# condition (ii)
# Z and Y are conditionally independent given X and all other covariates
condition_ii <- rep(FALSE, length.out = length(all_var_names))
for (i in c(1:length(outcome_p_values))) {
  if (!is.nan(outcome_p_values[i])) {
    # check conditional independence using corresponding p-value
    if (outcome_p_values[i] >= 0.05) {
      condition_ii[i] <- TRUE
    }
  }
}
names(condition_ii) <- all_var_names

# conditions (i) and (ii)
conditions_i_and_ii <- condition_i & condition_ii
names(conditions_i_and_ii) <- all_var_names

# test is TRUE if any covariate Z satisfies (i) and (ii)
# test is FALSE otherwise
conclusion <- any(conditions_i_and_ii)
conclusion_string <- ""
if (conclusion) {
  conclusion_string <- "Covariate set is sufficient for confounding adjustment"
} else {
  conclusion_string <- "Test is inconclusive, covariate set may or may not be sufficient"
}

lasso_union_all_tests <- data.frame(array(data     = NaN,
                              dim      = c(3, length(all_var_names)),
                              dimnames = list(c("Condition (i)", "Condition (ii)", "Conditions (i) and (ii)"), all_var_names) ))
lasso_union_all_tests["Condition (i)", ]           <- condition_i
lasso_union_all_tests["Condition (ii)", ]          <- condition_ii
lasso_union_all_tests["Conditions (i) and (ii)", ] <- conditions_i_and_ii

message("\n\nlasso_union p values:")
print(lasso_union_all_p_values)

message("\n\nlasso_union all test conditions:")
print(lasso_union_all_tests)

message("\n\nfinal results:")
print(conclusion)
print(conclusion_string)

lasso_union_results <- c("lasso_union",
                   conclusion,
                   conclusion_string)



# Put together results table --------------------------------------------------

results <- array(
    data = NaN,
    dim = c(3, 3),
    dimnames = list(c(1:3), c("method", "Condition", "Conclusion"))
)

results[1,] <- lasso_results
results[2,] <- lasso_X_results
results[3,] <- lasso_union_results

results_table <- data.frame(results)
print(results_table)



# Save results ----------------------------------------------------------------

write.csv(
  lasso_all_p_values,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_p_values-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_all_coefs,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_coefs-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_all_standard_errors,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_standard_errors-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_all_tests,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_tests-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_all_p_values,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_X_p_values-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_all_coefs,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_X_coefs-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_all_standard_errors,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_X_standard_errors-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_all_tests,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_X_tests-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_all_tests,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_X_tests-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_all_p_values,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_union_p_values-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_all_coefs,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_union_coefs-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_all_standard_errors,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_union_standard_errors-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_all_tests,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_union_tests-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_all_tests,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_union_tests-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  results_table,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_results-", name, ".csv"),
  row.names = TRUE
)

