# ------------------------------------------------------------------------------
#
# lasso_X_var_selection.R
#
# This file runs an exposure-LASSO (Least absolute shrinkage and selection operator)
# regression on the data with covid-19 exposure taken as the outcome / response
# and the outcome acute MI (cov_bin_ami) being excluded
# to determine a subset of the available confounders
# in turn determining the cox_ipw argument. "--covariate_other=this;that;other"
#
# Arguments:
#  - cohort - string, defines which of three opensafely cohorts to describe
#             (prevax, vax, unvax)
#  - age_str - vector of form "XX;XX;XX;XX;XX"
#              defines the age ranges over which the study population is stratified
#  - preex - boolean/string, defines preexisting conditions
#            for the replication preex = FALSE always (i.e ignored)
#            ("All", TRUE, or FALSE)
#
# Returns:
#  - Table of variables selected by the exposure-LASSO regression
#    (output/lasso_X_var_selection/lasso_X_var_selection-cohort_prevax.csv)
#
# Authors: Emma Tarmey
#
# ------------------------------------------------------------------------------

# UPDATED CODE TO TEST CONVERGENCE ISSUE ------------------------------------
print("UPDATED CODE TO TEST CONVERGENCE ISSUE")

# Refresh local R session ------------------------------------------------------
print("Refresh local R session")

rm(list=ls())


# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)
library(glmnet)


# Define lasso_X_var_selection output folder -----------------------------------
print("Creating output/lasso_X_var_selection output folder")

lasso_X_var_selection_dir <- "output/lasso_X_var_selection/"
fs::dir_create(here::here(lasso_X_var_selection_dir))


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
} else {
  # YAML arguments
  name    <- args[[1]]
  cohort  <- args[[2]]
  age_str <- args[[3]]
}

preex_string <- ""


# Load subsample data ----------------------------------------------------------
print("Load subsample data")

df <- readr::read_rds(paste0(
  "output/generate_subsample/input_",
  cohort,
  "_clean_subsample.rds"
))


# Data preparation for fully adjusted logistic model ---------------------
message("Data preparation for fully adjusted logistic model")

df2 <- (df %>% select(c(
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


# Data preparation for the lasso_X logistic model ------------------------------
message("Data preparation for the lasso_X logistic model")

# NB: Outcomes AMI and SAHHS are excluded altogether
# Covid is the response for the X model
lasso_X_conf_matrix <- (df %>% select(c(
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

lasso_X_conf_matrix_preserving_factors <- model.matrix(
   ~ ., # formula meaning take all terms
  data = lasso_X_conf_matrix
)

lasso_X_exposure_matrix <- (df %>% select(c(
  cov_bin_covid,
)))

lasso_X_exposure_matrix_preserving_factors <- model.matrix(
   ~ .,
  data = lasso_X_exposure_matrix
)


# Cross validating the lasso_X logistic model --------------------------------
message("Cross validating the lasso_X logistic model")

cv_lasso_logistic_model <- NULL
lasso_logistic_model    <- NULL

set.seed(2025) # k fold cross validation uses randomised shuffling
cv_lasso_X_logistic_model <- cv.glmnet(
  x       = lasso_X_conf_matrix_preserving_factors,
  y       = lasso_X_exposure_matrix_preserving_factors,
  nlambda = 200,       # length of lambda sequence
  nfolds  = 3,         # number of cv datasets
  family  = "binomial", # logistic regression
  alpha   = 1           # LASSO penalty
)


# Selecting optimal regularization parameter (lambda) ---------
message("Selecting optimal regularization parameter (lambda)")

# tune regularisation parameter lambda to minimise cross-validated error (cvm)
lambda     <- cv_lasso_X_logistic_model$lambda.min
lambda_1se <- cv_lasso_X_logistic_model$lambda.1se
print(paste0("Optimal lambda:", lambda))

lambda_sequence <- data.frame(
  lambda     = cv_lasso_X_logistic_model$lambda,
  cvm        = cv_lasso_X_logistic_model$cvm,
  cvm_se     = cv_lasso_X_logistic_model$cvsd,
  cvm_upper  = cv_lasso_X_logistic_model$cvup,
  cvm_lower  = cv_lasso_X_logistic_model$cvlo,
  lambda_min = cv_lasso_X_logistic_model$lambda.min, # constant column
  lambda_1se = cv_lasso_X_logistic_model$lambda.1se  # constant column
)


# Extracting the lasso_X cox model coefficients ---------------------------
message("Extracting the lasso_X cox model coefficients")

lasso_X_logistic_coefs           <- coef(cv_lasso_X_logistic_model, s = lambda)
lasso_X_logistic_coefs           <- as.data.frame(as.matrix(lasso_X_logistic_coefs))
colnames(lasso_X_logistic_coefs) <- c("coefficient")


# Fitting the Fully adjusted logistic model ------------------------------------
print("Fitting the fully-adjusted logistic model")

fully_adjusted_formula <- "cov_bin_covid ~ ."

fully_adjusted_logistic <- glm(
  fully_adjusted_formula,
  family = "binomial",
  data = df2
)



# Extract covariate selection results ------------------------------------------
print("Extract covariate selection results")

non_zero_coefs <- lasso_X_logistic_coefs %>% dplyr::filter(coefficient != 0.0)
non_zero_vars  <- rownames(non_zero_coefs)
vars_selected  <- convert_terms_to_vars(non_zero_vars)

# always include exposure
if (!("cov_bin_covid" %in% vars_selected)) {
  vars_selected <- c(vars_selected, "cov_bin_covid")
}


# Save covariate selection and coefficients ------------------------------------
print("Save Covariate Selection and Coefficients")

write.csv(
  summary(fully_adjusted_logistic)$coefficients,
  paste0(lasso_X_var_selection_dir, "fully_adjusted_logistic_coefs-", name, preex_string, ".csv"),
  row.names = TRUE
)

write.csv(
  lasso_X_logistic_coefs,
  paste0(lasso_X_var_selection_dir, "lasso_X_var_selection-coefs-", name, preex_string, ".csv"),
  row.names = TRUE
)

write.csv(
  vars_selected,
  paste0(lasso_X_var_selection_dir, "lasso_X_var_selection-", name, preex_string, ".csv"),
  row.names = FALSE
)

write.csv(
  lambda_sequence,
  paste0(lasso_X_var_selection_dir, "lambda_sequence-", name, preex_string, ".csv"),
  row.names = FALSE
)
