# ------------------------------------------------------------------------------
#
# apply_across_MI.R
#
# This file handles unpacking missingness and applying new methodology
# 
# Arguments:
#  - cohort - string, defines which of three opensafely cohorts to describe
#             (prevax, vax, unvax)
#  - preex - boolean/string, defines preexisting conditions
#            for the replication preex = FALSE always
#            ("All", TRUE, or FALSE)
#
# Returns:
#  - output/apply_across_MI/input_<cohort>_clean_across_MI.rds
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


# Define apply_across_MI output folder ---------------------------------------
print("Creating output/apply_across_MI output folder")

apply_across_MI_dir <- "output/apply_across_MI/"
dir_create(here::here(apply_across_MI_dir))


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
  "_clean.rds"
))


# Handle missingness ----------------------------------------------------------
print("Applying multiple imputation to BMI and smoking covariates")

# set random seed
set.seed(2026)

# remove dates
date_vars   <- grep("date", colnames(df))
df_dates    <- df[, date_vars]
df_no_dates <- df[, -date_vars]

# re-cast missing smoking to NA
smoking_missing <- df_no_dates$cov_cat_smoking == "Missing"
df_no_dates$cov_cat_smoking[smoking_missing] <- NA

# check missingness of smoking and bmi variables
percent_smoking_missing <- signif(100 * (sum(is.na(df_no_dates$cov_cat_smoking)) / length(df_no_dates$cov_cat_smoking)), digits = 4)
percent_bmi_missing     <- signif(100 * (sum(is.na(df_no_dates$cov_num_bmi))     / length(df_no_dates$cov_num_bmi)),     digits = 4)

print(paste0("The variable smoking is ", percent_smoking_missing, "% missing"))
print(paste0("The variable bmi is ",     percent_bmi_missing,     "% missing"))

# The below ensures that bmi and smoking are handled with specific
# imputation methods and that all other covariates are left alone
# See: https://www.rdocumentation.org/packages/mice/versions/3.17.0/topics/mice
imp_method                    <- rep("", length.out = length(colnames(df_no_dates)))
names(imp_method)             <- colnames(df_no_dates)
imp_method["cov_cat_smoking"] <- "polyreg" # smoking is categorical with 3 levels, Polytomous logistic regression
imp_method["cov_num_bmi"]     <- "norm"    # bmi is numerical, Bayesian linear regression

# Apply multiple imputation
num_datasets <- 10
imp <- mice::mice(
  data       = df_no_dates,
  m          = num_datasets,
  maxit      = 20,
  imp_method = unname(imp_method)
)

df_post_imputation <- mice::complete(
  imp,
  action  = "long",
  include = FALSE
)

df_post_imputation <- subset(
  df_post_imputation,
  select = -c(.imp, .id)
)

df_dates_stacked <- rbind(
  df_dates, df_dates, df_dates, df_dates, df_dates,
  df_dates, df_dates, df_dates, df_dates, df_dates
)

df_post_imputation <- cbind(
  df_post_imputation, df_dates_stacked
)


# Remove now unused level 'missing' from smoking covariate ---------------------
print("Remove now unused level 'missing' from smoking covariate")

df_post_imputation$cov_cat_smoking <- factor(
  df_post_imputation$cov_cat_smoking,
  levels = levels(droplevels(df_post_imputation$cov_cat_smoking))
)


# Re-assign unique identifiers to imputed dataset ------------------------------
print("Re-assign unique identifiers to imputed dataset")

patient_id_1 <- as.numeric(unique(df_post_imputation$patient_id))
shift        <- max(patient_id_1) + 1

patient_id_2  <- patient_id_1 + shift
patient_id_3  <- patient_id_2 + shift
patient_id_4  <- patient_id_3 + shift
patient_id_5  <- patient_id_4 + shift
patient_id_6  <- patient_id_5 + shift
patient_id_7  <- patient_id_6 + shift
patient_id_8  <- patient_id_7 + shift
patient_id_9  <- patient_id_8 + shift
patient_id_10 <- patient_id_9 + shift

new_patient_id <- c(
  patient_id_1, patient_id_2, patient_id_3, patient_id_4, patient_id_5,
  patient_id_6, patient_id_7, patient_id_8, patient_id_9, patient_id_10
)

df_post_imputation$patient_id <- new_patient_id


# Save data after 'across' multiple imputation --------------------------------
print("Save data after 'across' multiple imputation")

saveRDS(
  df_post_imputation,
  file = paste0(apply_across_MI_dir, "input_", cohort, "_clean_across_MI.rds"),
  compress = TRUE
)
