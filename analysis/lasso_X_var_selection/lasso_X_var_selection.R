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
#            for the replication preex = FALSE always
#            ("All", TRUE, or FALSE)
#
# Returns:
#  - Table of variables selected by the exposure-LASSO regression
#    (output/lasso_X_var_selection/lasso_X_var_selection-cohort_prevax.csv)
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

# preex optional argument deliberately ignored, sup_bin_preex does not exist
# because neither asthma nor copdoutcomes are present
preex_string <- ""
# if (preex != "All") {
#   df <- df[df$sup_bin_preex == preex, ]
#   preex_string <- paste0("-preex_", preex)
# }


# Define age groups ------------------------------------------------------------
print("Define age groups")

df$cov_cat_age_group <- numerical_to_categorical(df$cov_num_age, age_bounds) # See utility.R

# df$cov_cat_consrate2019 <- numerical_to_categorical(
#   df$cov_num_consrate2019,
#   c(1, 6),
#   zero_flag = TRUE
# )

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

print(summary(df))


# LASSO_X data matrix setup ----------------------------------------------------
print("LASSO_X data matrix setup")

df2 <- (df %>% select(!c(patient_id, exposed, cov_bin_ami)))
df3 <- (df %>% select(exposed))

lasso_X_conf_matrix <- data.matrix(df2)
lasso_X_exposure_matrix <- data.matrix(df3)


# Fitting the LASSO_X model ----------------------------------------------------
print("Fitting the LASSO_X model")

cv_lasso_X_model <- cv.glmnet(x = lasso_X_conf_matrix,
                              y = lasso_X_exposure_matrix,
                              alpha=1)

lambda         <- cv_lasso_X_model$lambda.min
lasso_X_model    <- glmnet(x = lasso_X_conf_matrix,
                           y = lasso_X_exposure_matrix,
                           alpha=1,
                           lambda=lambda)


# Extract covariate selection results ------------------------------------------
print("Extract covariate selection results")

lasso_X_coefs        <- as.vector(lasso_X_model$beta)
names(lasso_X_coefs) <- rownames(lasso_X_model$beta)

vars_selected <- names(lasso_X_coefs[lasso_X_coefs != 0.0])
vars_selected <- vars_selected[vars_selected != "(Intercept)"]

print(vars_selected)


# Save covariate selection ----------------------------------------------------------
print("Save Covariate Selection")

write.csv(
  vars_selected,
  paste0(lasso_X_var_selection_dir, "lasso_X_var_selection-cohort_", cohort, preex_string, ".csv"),
  row.names = FALSE
)
