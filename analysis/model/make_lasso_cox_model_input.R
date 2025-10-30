# ------------------------------------------------------------------------------
#
# make_lasso_cox_model_input.R
#
# This file converts the output of the LASSO, LASSO_X and LASSO_union
# models from CSV to TXT
#
# Arguments:
#  - name - string, specifies cohort and subgroup pairing
#
# Returns:
#  - Plain text output from LASSO models
#    (output/model/...)
#
# Authors: Emma Tarmey
#
# ------------------------------------------------------------------------------


# Load packages ----------------------------------------------------------------
print("Load packages")

library(magrittr)
library(data.table)
library(stringr)


# Source functions -------------------------------------------------------------
print("Source functions")

lapply(
  list.files("analysis/model", full.names = TRUE, pattern = "fn-"),
  source
)


# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  name <- "cohort_prevax-main-ami"
} else {
  name <- args[[1]]
}

analysis <- gsub(
  "cohort_.*vax-",
  "",
  name
)


# Define model output folder ---------------------------------------
print("Creating output/model output folder")

# setting up the sub directory
model_dir <- "output/model/"

# check if sub directory exists, create if not
fs::dir_create(here::here(model_dir))


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

lasso_union_vars_selected <- read.csv(paste0(
  "output/lasso_union_var_selection/lasso_union_var_selection-",
  name,
  ".csv"
))$x


# Remove non-confounders and remove double-counted variables -------------------
print("Removing non-confounders and remove double-counted variables")

lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "binary_covid19_exposure"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "exp_date"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "out_date"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "strat_cat_region"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "cov_cat_sex"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "cov_num_age"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "cov_cat_age_group"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "median_iqr_age"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "index_date"]
lasso_vars_selected       <- lasso_vars_selected[lasso_vars_selected != "end_date_outcome"]

lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "binary_covid19_exposure"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "exp_date"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "out_date"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "strat_cat_region"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "cov_cat_sex"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "cov_num_age"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "cov_cat_age_group"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "median_iqr_age"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "index_date"]
lasso_X_vars_selected       <- lasso_X_vars_selected[lasso_X_vars_selected != "end_date_outcome"]

lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "binary_covid19_exposure"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "exp_date"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "out_date"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "strat_cat_region"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "cov_cat_sex"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "cov_num_age"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "cov_cat_age_group"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "median_iqr_age"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "index_date"]
lasso_union_vars_selected       <- lasso_union_vars_selected[lasso_union_vars_selected != "end_date_outcome"]


# Generate text ----------------------------------------------------------------
print("Generating text files")

# NB: If an empty vector is passed in, an empty string is correctly generated
lasso_vars_text       <- paste(lasso_vars_selected, collapse = ";")
lasso_X_vars_text     <- paste(lasso_X_vars_selected, collapse = ";")
lasso_union_vars_text <- paste(lasso_union_vars_selected, collapse = ";")

# empty string currently cannot be handled by the cox_ipw reusable action
if (lasso_vars_text == "") {
  lasso_vars_text <- "cov_cat_ethnicity"
  print("LOG NOTE: Lasso variable selection has produced an empty covariate set.  Ethnicity variable chosen as default.")
}
if (lasso_X_vars_text == "") {
  lasso_X_vars_text <- "cov_cat_ethnicity"
  print("LOG NOTE: Exposure lasso variable selection has produced an empty covariate set.  Ethnicity variable chosen as default.")
}
if (lasso_union_vars_text == "") {
  lasso_union_vars_text <- "cov_cat_ethnicity"
  print("LOG NOTE: Union lasso variable selection has produced an empty covariate set.  Ethnicity variable chosen as default.")
}


# Save covariate selections ----------------------------------------------------
print("Save Covariate Selections")

writeLines(lasso_vars_text,       paste0(model_dir, "lasso_cox_model_input-", name, ".txt"))
writeLines(lasso_X_vars_text,     paste0(model_dir, "lasso_X_cox_model_input-", name, ".txt"))
writeLines(lasso_union_vars_text, paste0(model_dir, "lasso_union_cox_model_input-", name, ".txt"))
