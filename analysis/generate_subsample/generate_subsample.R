# ------------------------------------------------------------------------------
#
# generate_subsample.R
#
# This file generates a smaller cohort (10% of original sample size)
# for use when fitting computationally expensive models e.g. LASSO
# 
# Arguments:
#  - cohort - string, defines which of three opensafely cohorts to describe
#             (prevax, vax, unvax)
#  - preex - boolean/string, defines preexisting conditions
#            for the replication preex = FALSE always
#            ("All", TRUE, or FALSE)
#
# Returns:
#  - dataframe of patient data, random 10% subsample of input data
#    (output/generate_subsample/input_{cohort}_subsample.rds)
#
# Authors: Emma Tarmey
#
# ------------------------------------------------------------------------------


# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)
library(fs)


# Define generate_subsample output folder ---------------------------------------
print("Creating output/generate_subsample output folder")

generate_subsample_dir <- "output/generate_subsample/"
dir_create(here::here(generate_subsample_dir))


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


# Sanity check all covariate data types ----------------------------------------
print("Sanity check all covariate data types")

df$cov_bin_ami   <- as.logical(df$cov_bin_ami)   # outcome
df$cov_bin_sahhs <- as.logical(df$cov_bin_sahhs) # outcome
df$cov_bin_covid <- as.logical(df$cov_bin_covid) # exposure

df$cov_num_age       <- as.numeric(df$cov_num_age)
df$cov_cat_sex       <- as.factor(df$cov_cat_sex)
df$cov_cat_ethnicity <- as.factor(df$cov_cat_ethnicity)
df$cov_cat_imd       <- as.factor(df$cov_cat_imd)
df$cov_cat_smoking   <- as.factor(df$cov_cat_smoking)

df$cov_bin_carehome      <- as.factor(df$cov_bin_carehome)
df$cov_bin_hcworker      <- as.factor(df$cov_bin_hcworker)
df$cov_bin_dementia      <- as.logical(df$cov_bin_dementia)
df$cov_bin_liver_disease <- as.logical(df$cov_bin_liver_disease)
df$cov_bin_ckd           <- as.logical(df$cov_bin_ckd)

df$cov_bin_cancer       <- as.logical(df$cov_bin_cancer)
df$cov_bin_hypertension <- as.logical(df$cov_bin_hypertension)
df$cov_bin_diabetes     <- as.logical(df$cov_bin_diabetes)
df$cov_bin_obesity      <- as.logical(df$cov_bin_obesity)
df$cov_bin_copd         <- as.logical(df$cov_bin_copd)

df$cov_bin_depression <- as.logical(df$cov_bin_depression)
df$cov_bin_stroke_all <- as.logical(df$cov_bin_stroke_all)
df$cov_bin_other_ae   <- as.logical(df$cov_bin_other_ae)
df$cov_bin_vte        <- as.logical(df$cov_bin_vte)
df$cov_bin_hf         <- as.logical(df$cov_bin_hf)

df$cov_bin_angina        <- as.logical(df$cov_bin_angina)
df$cov_bin_lipidmed      <- as.logical(df$cov_bin_lipidmed)
df$cov_bin_antiplatelet  <- as.logical(df$cov_bin_antiplatelet)
df$cov_bin_anticoagulant <- as.logical(df$cov_bin_anticoagulant)
df$cov_bin_cocp          <- as.logical(df$cov_bin_cocp)

df$cov_bin_hrt      <- as.logical(df$cov_bin_hrt)
df$strat_cat_region <- as.factor(df$strat_cat_region)


# Generate 10% subsample ------------------------------------------------------
print("Generate 10% subsample")

set.seed(2026) # fixed for reproducibility, no overlapping RNG sequences so fine to handle in this way
sample_size  <- nrow(df)
selection    <- sample(x = c(1:sample_size), size = ceiling(sample_size/10), replace = FALSE)
subsample_df <- df[selection, ]


# Save subsample --------------------------------------------------------------
print("Save subsample")

saveRDS(
  subsample_df,
  file = paste0(generate_subsample_dir, "input_", cohort, "_clean_subsample.rds"),
  compress = TRUE
)
