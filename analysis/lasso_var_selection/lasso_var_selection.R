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

age_bounds   <- as.numeric(stringr::str_split(as.vector(age_str), ";")[[1]])
preex_string <- ""


# Load data --------------------------------------------------------------------
print("Load data")

model_input_df <- df <- readr::read_rds(paste0(
  "output/model/model_input-",
  name,
  ".rds"
))


# LASSO data matrix setup ------------------------------------------------------
print("LASSO data matrix setup")

model_input_df$binary_outcome  <- !is.na(model_input_df$out_date)
model_input_df$binary_exposure <- !is.na(model_input_df$exp_date)

# remove unnecessary columns, remove all Date columns where unnecessary
# exposure (covid-19) is recast to binary
df2 <- (model_input_df %>% select(!c(patient_id, index_date, out_date, end_date_outcome, exp_date, end_date_exposure)))
df3 <- (model_input_df %>% select(c(binary_outcome, out_date, end_date_outcome)))

df3$outcome_cox_dates <- rep(as.Date(NA), times = nrow(df3))
for (i in c(1:nrow(df3))) {
  if (df3$binary_outcome[i]) {
    df3$outcome_cox_dates[i] <- df3$out_date[i]
  } else {
    df3$outcome_cox_dates[i] <- df3$end_date_outcome[i]
  }
}


lasso_exposure_and_conf_matrix <- data.matrix(df2)
lasso_outcome_survival         <- Surv(time  = as.numeric(df3$outcome_cox_dates),
                                       event = df3$binary_outcome)

message("\n\nOutcome Data:")
print(head(df3))
message("\n\nSurvival Curve:")
print(head(lasso_outcome_survival))


# Fitting the LASSO model ------------------------------------------------------
print("Fitting the LASSO model")

cv_lasso_model <- cv.glmnet(x = lasso_exposure_and_conf_matrix,
                            y = lasso_outcome_survival,
                            family="cox", # cox (survival curve) regression
                            alpha=1)      # LASSO penalty

# tune regularisation parameter lambda to minimise cross-validated error (cvm)
lambda         <- cv_lasso_model$lambda.min

lasso_model    <- glmnet(x = lasso_exposure_and_conf_matrix,
                         y = lasso_outcome_survival,
                         family="cox",  # cox (survival curve) regression
                         alpha=1,       # LASSO penalty
                         lambda=lambda) # optimal lambda


# Extract covariate selection results ------------------------------------------
print("Extract covariate selection results")

lasso_coefs        <- as.vector(lasso_model$beta)
names(lasso_coefs) <- rownames(lasso_model$beta)

vars_selected <- names(lasso_coefs[lasso_coefs != 0.0])
vars_selected <- vars_selected[vars_selected != "(Intercept)"]

print(vars_selected)


# Save covariate selection ----------------------------------------------------------
print("Save Covariate Selection")

write.csv(
  vars_selected,
  paste0(lasso_var_selection_dir, "lasso_var_selection-", name, preex_string, ".csv"),
  row.names = FALSE
)
