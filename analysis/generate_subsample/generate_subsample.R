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

set.seed(2025) # this file only runs once so should be fine (i.e. no overlapping RNG sequences)

sample_size  <- nrow(df)

ami_positive_cases                <- which(!is.na(df$out_date_ami))
sahhs_positive_cases              <- which(!is.na(df$out_date_stroke_sahhs))

both_positive_cases               <- intersect( ami_positive_cases,     sahhs_positive_cases )
both_negative_cases               <- intersect( which(is.na(df$out_date_ami)), which(is.na(df$out_date_stroke_sahhs)) )
ami_positive_sahhs_negative_cases <- setdiff(ami_positive_cases,   both_positive_cases)
sahhs_positive_ami_negative_cases <- setdiff(sahhs_positive_cases, both_positive_cases)

both_positive_selection               <- sample(both_positive_cases,               size = ceiling(length(both_positive_cases)/10),              )
both_negative_selection               <- sample(both_negative_cases,               size = ceiling(length(both_negative_cases)/10),              )
ami_positive_sahhs_negative_selection <- sample(ami_positive_sahhs_negative_cases, size = ceiling(length(ami_positive_sahhs_negative_cases)/10) )
sahhs_positive_ami_negative_selection <- sample(sahhs_positive_ami_negative_cases, size = ceiling(length(sahhs_positive_ami_negative_cases)/10) )

selection    <- c(both_positive_selection,
                  both_negative_selection,
                  ami_positive_sahhs_negative_selection,
                  sahhs_positive_ami_negative_selection)

subsample_df <- df[selection, ]


# Save subsample --------------------------------------------------------------
print("Save subsample")

saveRDS(
  subsample_df,
  file = paste0(generate_subsample_dir, "input_", cohort, "_clean_subsample.rds"),
  compress = TRUE
)

