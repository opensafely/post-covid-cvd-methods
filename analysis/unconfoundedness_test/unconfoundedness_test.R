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


# Load data --------------------------------------------------------------------
print("Load data")

# subsample
model_input_df <- readr::read_rds(paste0(
  "output/model/model_input_subsample-",
  name,
  ".rds"
))


# Test lasso selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on lasso results")




# Test lasso_X selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on lasso_X results")




# Test lasso_union selection --------------------------------------------------------
print("Performing empirical unfoncoundedness plausibility test on lasso_union results")





# Put together results tables -------------------------------------------------
print("Put together results tables")

labels <- c(
  "p_values", "p_values", "coefs", "coefs", "standard_errors", "standard_errors", "tests", "tests", "tests"
)

lasso_explanatory <- rbind(
  lasso_all_p_values,
  lasso_all_coefs,
  lasso_all_standard_errors,
  lasso_all_tests
)
lasso_explanatory$labels <- labels
lasso_explanatory <- lasso_explanatory %>% select(labels, everything())

lasso_X_explanatory <- rbind(
  lasso_X_all_p_values,
  lasso_X_all_coefs,
  lasso_X_all_standard_errors,
  lasso_X_all_tests
)
lasso_X_explanatory$labels <- labels
lasso_X_explanatory <- lasso_X_explanatory %>% select(labels, everything())

lasso_union_explanatory <- rbind(
  lasso_union_all_p_values,
  lasso_union_all_coefs,
  lasso_union_all_standard_errors,
  lasso_union_all_tests
)
lasso_union_explanatory$labels <- labels
lasso_union_explanatory <- lasso_union_explanatory %>% select(labels, everything())

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
  lasso_explanatory,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_explanatory-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_explanatory,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_X_explanatory-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_union_explanatory,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_lasso_union_explanatory-", name, ".csv"),
  row.names = TRUE
)

write.csv(
  results_table,
  paste0(unconfoundedness_test_dir, "unconfoundedness_test_results-", name, ".csv"),
  row.names = TRUE
)
