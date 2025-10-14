# ------------------------------------------------------------------------------
#
# lasso_union_var_selection.R
#
# This file implements the union-LASSO variable selection method
# by taking the union of the previous LASSO and LASSO_X confounder sets
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
#  - Table of variables selected by the union LASSO method
#    (output/lasso_union_var_selection/lasso_union_var_selection-cohort_prevax.csv)
#
# Authors: Emma Tarmey
#
# ------------------------------------------------------------------------------


# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)

# Define lasso_union_var_selection output folder -------------------------------
print("Creating output/lasso_union_var_selection output folder")

lasso_union_var_selection_dir <- "output/lasso_union_var_selection/"
fs::dir_create(here::here(lasso_union_var_selection_dir))

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

age_bounds <- as.numeric(stringr::str_split(as.vector(age_str), ";")[[1]])


# Load data --------------------------------------------------------------------
print("Load data")

lasso_vars_selected <- read.csv(paste0(
  "output/lasso_var_selection/lasso_var_selection-",
  name,
  ".csv"
))$x

lasso_X_vars_selected <- read.csv(paste0(
  "output/lasso_X_var_selection/lasso_X_var_selection-",
  name,
  ".csv"
))$x


# Select for no pre-existing conditions ----------------------------------------
print("Select for pre-existing conditions")
preex_string <- ""


# Take union of LASSO and LASSO_X covariate sets -------------------------------
print("Take union of LASSO and LASSO_X covariate sets")

lasso_union_vars_selected <- union(lasso_vars_selected, lasso_X_vars_selected)
print(lasso_union_vars_selected)


# Save covariate selection -----------------------------------------------------
print("Save Covariate Selection")

write.csv(
  lasso_union_vars_selected,
  paste0(lasso_union_var_selection_dir, "lasso_union_var_selection-", name, preex_string, ".csv"),
  row.names = FALSE
)
