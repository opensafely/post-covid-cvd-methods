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

# Refresh local R session ------------------------------------------------------
print("Refresh local R session")

rm(list=ls())


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

df_ami <- readr::read_rds(paste0(
  "output/dataset_clean/input_",
  cohort,
  "_clean_ami.rds"
))

df_sahhs <- readr::read_rds(paste0(
  "output/dataset_clean/input_",
  cohort,
  "_clean_sahhs.rds"
))


# Sanity check all covariate data types ----------------------------------------
print("Sanity check all covariate data types")

df_ami$cov_bin_ami   <- as.factor(df_ami$cov_bin_ami)   # outcome
df_ami$cov_bin_sahhs <- as.factor(df_ami$cov_bin_sahhs) # outcome
df_ami$cov_bin_covid <- as.factor(df_ami$cov_bin_covid) # exposure

df_ami$cov_num_age       <- as.numeric(df_ami$cov_num_age)
df_ami$cov_cat_sex       <- as.factor(df_ami$cov_cat_sex)
df_ami$cov_cat_ethnicity <- as.factor(df_ami$cov_cat_ethnicity)
df_ami$cov_cat_imd       <- as.factor(df_ami$cov_cat_imd)
df_ami$cov_cat_smoking   <- as.factor(df_ami$cov_cat_smoking)

df_ami$cov_bin_carehome      <- as.factor(df_ami$cov_bin_carehome)
df_ami$cov_bin_hcworker      <- as.factor(df_ami$cov_bin_hcworker)
df_ami$cov_bin_dementia      <- as.factor(df_ami$cov_bin_dementia)
df_ami$cov_bin_liver_disease <- as.factor(df_ami$cov_bin_liver_disease)
df_ami$cov_bin_ckd           <- as.factor(df_ami$cov_bin_ckd)

df_ami$cov_bin_cancer       <- as.factor(df_ami$cov_bin_cancer)
df_ami$cov_bin_hypertension <- as.factor(df_ami$cov_bin_hypertension)
df_ami$cov_bin_diabetes     <- as.factor(df_ami$cov_bin_diabetes)
df_ami$cov_bin_obesity      <- as.factor(df_ami$cov_bin_obesity)
df_ami$cov_bin_copd         <- as.factor(df_ami$cov_bin_copd)

df_ami$cov_bin_depression <- as.factor(df_ami$cov_bin_depression)
df_ami$cov_bin_stroke_all <- as.factor(df_ami$cov_bin_stroke_all)
df_ami$cov_bin_other_ae   <- as.factor(df_ami$cov_bin_other_ae)
df_ami$cov_bin_vte        <- as.factor(df_ami$cov_bin_vte)
df_ami$cov_bin_hf         <- as.factor(df_ami$cov_bin_hf)

df_ami$cov_bin_angina        <- as.factor(df_ami$cov_bin_angina)
df_ami$cov_bin_lipidmed      <- as.factor(df_ami$cov_bin_lipidmed)
df_ami$cov_bin_antiplatelet  <- as.factor(df_ami$cov_bin_antiplatelet)
df_ami$cov_bin_anticoagulant <- as.factor(df_ami$cov_bin_anticoagulant)
df_ami$cov_bin_cocp          <- as.factor(df_ami$cov_bin_cocp)

df_ami$cov_bin_hrt      <- as.factor(df_ami$cov_bin_hrt)
df_ami$strat_cat_region <- as.factor(df_ami$strat_cat_region)


df_sahhs$cov_bin_sahhs   <- as.factor(df_sahhs$cov_bin_sahhs)   # outcome
df_sahhs$cov_bin_sahhs <- as.factor(df_sahhs$cov_bin_sahhs) # outcome
df_sahhs$cov_bin_covid <- as.factor(df_sahhs$cov_bin_covid) # exposure

df_sahhs$cov_num_age       <- as.numeric(df_sahhs$cov_num_age)
df_sahhs$cov_cat_sex       <- as.factor(df_sahhs$cov_cat_sex)
df_sahhs$cov_cat_ethnicity <- as.factor(df_sahhs$cov_cat_ethnicity)
df_sahhs$cov_cat_imd       <- as.factor(df_sahhs$cov_cat_imd)
df_sahhs$cov_cat_smoking   <- as.factor(df_sahhs$cov_cat_smoking)

df_sahhs$cov_bin_carehome      <- as.factor(df_sahhs$cov_bin_carehome)
df_sahhs$cov_bin_hcworker      <- as.factor(df_sahhs$cov_bin_hcworker)
df_sahhs$cov_bin_dementia      <- as.factor(df_sahhs$cov_bin_dementia)
df_sahhs$cov_bin_liver_disease <- as.factor(df_sahhs$cov_bin_liver_disease)
df_sahhs$cov_bin_ckd           <- as.factor(df_sahhs$cov_bin_ckd)

df_sahhs$cov_bin_cancer       <- as.factor(df_sahhs$cov_bin_cancer)
df_sahhs$cov_bin_hypertension <- as.factor(df_sahhs$cov_bin_hypertension)
df_sahhs$cov_bin_diabetes     <- as.factor(df_sahhs$cov_bin_diabetes)
df_sahhs$cov_bin_obesity      <- as.factor(df_sahhs$cov_bin_obesity)
df_sahhs$cov_bin_copd         <- as.factor(df_sahhs$cov_bin_copd)

df_sahhs$cov_bin_depression <- as.factor(df_sahhs$cov_bin_depression)
df_sahhs$cov_bin_stroke_all <- as.factor(df_sahhs$cov_bin_stroke_all)
df_sahhs$cov_bin_other_ae   <- as.factor(df_sahhs$cov_bin_other_ae)
df_sahhs$cov_bin_vte        <- as.factor(df_sahhs$cov_bin_vte)
df_sahhs$cov_bin_hf         <- as.factor(df_sahhs$cov_bin_hf)

df_sahhs$cov_bin_angina        <- as.factor(df_sahhs$cov_bin_angina)
df_sahhs$cov_bin_lipidmed      <- as.factor(df_sahhs$cov_bin_lipidmed)
df_sahhs$cov_bin_antiplatelet  <- as.factor(df_sahhs$cov_bin_antiplatelet)
df_sahhs$cov_bin_anticoagulant <- as.factor(df_sahhs$cov_bin_anticoagulant)
df_sahhs$cov_bin_cocp          <- as.factor(df_sahhs$cov_bin_cocp)

df_sahhs$cov_bin_hrt      <- as.factor(df_sahhs$cov_bin_hrt)
df_sahhs$strat_cat_region <- as.factor(df_sahhs$strat_cat_region)



# Generate 10% subsample ------------------------------------------------------
print("Generate 10% subsample")

set.seed(2026) # fixed for reproducibility, no overlapping RNG sequences so fine to handle in this way

sample_size  <- nrow(df_ami) # same for both
selection    <- sample(x = c(1:sample_size), size = ceiling(sample_size/10), replace = FALSE) # sxame for both

subsample_df_ami   <- df_ami[selection, ]
subsample_df_sahhs <- df_sahhs[selection, ]


# Save subsample --------------------------------------------------------------
print("Save subsample")

saveRDS(
  subsample_df_ami,
  file = paste0(generate_subsample_dir, "input_", cohort, "_clean_subsample_ami.rds"),
  compress = TRUE
)

saveRDS(
  subsample_df_sahhs,
  file = paste0(generate_subsample_dir, "input_", cohort, "_clean_subsample_sahhs.rds"),
  compress = TRUE
)
