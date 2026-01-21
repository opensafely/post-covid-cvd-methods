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
#  - ...
#    (output/generate_subsample/venn-cohort_{cohort}_subsample.rds)
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


# Generate 10% subsample ------------------------------------------------------
print("Generate 10% subsample")

set.seed(2026) # fixed for reproducibility, no overlapping RNG sequences so fine to handle in this way

sample_size  <- nrow(df)
selection    <- sample(x = c(1:sample_size), size = ceiling(sample_size/10), replace = FALSE)
subsample_df <- df[selection, ]

message("\n\nTotal study population:")
print(nrow(df))
print(sum(!is.na(df$out_date_stroke_sahhs)))
print(sum(!is.na(df$out_date_ami)))

message("\n\nSubsample:")
print(nrow(subsample_df))
print(sum(!is.na(subsample_df$out_date_stroke_sahhs)))
print(sum(!is.na(subsample_df$out_date_ami)))


# Save subsample --------------------------------------------------------------
print("Save subsample")

saveRDS(
  subsample_df,
  file = paste0(generate_subsample_dir, "input_", cohort, "_clean_subsample.rds"),
  compress = TRUE
)

