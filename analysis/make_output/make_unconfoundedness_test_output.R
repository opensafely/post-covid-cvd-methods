# ------------------------------------------------------------------------------
#
# make_unconfoundnessness_test_output.R
#
# Tidy all outputs from the unconfoundedness test
#
# Arguments:
#  - None!
#
# Returns:
#  - All regression results table: table of coefficients, SEs and p values for every regression
#  - All test tables: all test conditions
#  - All conclusion tables: all conclusions for all variable sets
#
# Authors: Emma Tarmey, Venexia Walker, UoB ehrQL Team
#
# ------------------------------------------------------------------------------


# Load packages ----------------------------------------------------------------
print('Load packages')

library(magrittr)
library(data.table)
library(stringr)
library(tidyr)


# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")


# Define make_output folder ----------------------------------------------------
print("Creating output/make_output output folder")

makeout_dir <- "output/make_output/"
fs::dir_create(here::here(makeout_dir))


# Load unconfoundness test files -----------------------------------------------
print("Load unconfoundness test files")

# load all conclusion tables
main_ami_conclusion_table               <- read.csv("output/unconfoundedness_test/all_var_sets_conclusion_table-cohort_prevax-main-ami.csv")
sub_covidhospital_FALSE_ami_conclusion_table          <- read.csv("output/unconfoundedness_test/all_var_sets_conclusion_table-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
sub_covidhospital_TRUE_ami_conclusion_table           <- read.csv("output/unconfoundedness_test/all_var_sets_conclusion_table-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
main_stroke_sahhs_conclusion_table      <- read.csv("output/unconfoundedness_test/all_var_sets_conclusion_table-cohort_prevax-main-stroke_sahhs.csv")
sub_covidhospital_FALSE_stroke_sahhs_conclusion_table <- read.csv("output/unconfoundedness_test/all_var_sets_conclusion_table-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
sub_covidhospital_TRUE_stroke_sahhs_conclusion_table  <- read.csv("output/unconfoundedness_test/all_var_sets_conclusion_table-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all fully_adjusted exposure regression results
fully_adjusted_exposure_regression_main_ami               <- read.csv("output/unconfoundedness_test/fully_adjusted_exposure_regression_results-cohort_prevax-main-ami.csv")
fully_adjusted_exposure_regression_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/fully_adjusted_exposure_regression_results-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
fully_adjusted_exposure_regression_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/fully_adjusted_exposure_regression_results-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
fully_adjusted_exposure_regression_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/fully_adjusted_exposure_regression_results-cohort_prevax-main-stroke_sahhs.csv")
fully_adjusted_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/fully_adjusted_exposure_regression_results-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
fully_adjusted_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/fully_adjusted_exposure_regression_results-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all fully_adjusted outcome regression results
fully_adjusted_outcome_regression_main_ami               <- read.csv("output/unconfoundedness_test/fully_adjusted_outcome_regression_results-cohort_prevax-main-ami.csv")
fully_adjusted_outcome_regression_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/fully_adjusted_outcome_regression_results-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
fully_adjusted_outcome_regression_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/fully_adjusted_outcome_regression_results-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
fully_adjusted_outcome_regression_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/fully_adjusted_outcome_regression_results-cohort_prevax-main-stroke_sahhs.csv")
fully_adjusted_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/fully_adjusted_outcome_regression_results-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
fully_adjusted_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/fully_adjusted_outcome_regression_results-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all fully_adjusted test tables
fully_adjusted_test_table_main_ami               <- read.csv("output/unconfoundedness_test/fully_adjusted_test_table-cohort_prevax-main-ami.csv")
fully_adjusted_test_table_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/fully_adjusted_test_table-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
fully_adjusted_test_table_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/fully_adjusted_test_table-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
fully_adjusted_test_table_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/fully_adjusted_test_table-cohort_prevax-main-stroke_sahhs.csv")
fully_adjusted_test_table_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/fully_adjusted_test_table-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
fully_adjusted_test_table_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/fully_adjusted_test_table-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso exposure regression results
lasso_exposure_regression_main_ami               <- read.csv("output/unconfoundedness_test/lasso_exposure_regression_results-cohort_prevax-main-ami.csv")
lasso_exposure_regression_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_exposure_regression_results-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_exposure_regression_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_exposure_regression_results-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_exposure_regression_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_exposure_regression_results-cohort_prevax-main-stroke_sahhs.csv")
lasso_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_exposure_regression_results-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_exposure_regression_results-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso outcome regression results
lasso_outcome_regression_main_ami               <- read.csv("output/unconfoundedness_test/lasso_outcome_regression_results-cohort_prevax-main-ami.csv")
lasso_outcome_regression_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_outcome_regression_results-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_outcome_regression_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_outcome_regression_results-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_outcome_regression_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_outcome_regression_results-cohort_prevax-main-stroke_sahhs.csv")
lasso_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_outcome_regression_results-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_outcome_regression_results-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso test tables
lasso_test_table_main_ami               <- read.csv("output/unconfoundedness_test/lasso_test_table-cohort_prevax-main-ami.csv")
lasso_test_table_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_test_table-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_test_table_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_test_table-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_test_table_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_test_table-cohort_prevax-main-stroke_sahhs.csv")
lasso_test_table_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_test_table-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_test_table_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_test_table-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso_X exposure regression results
lasso_X_exposure_regression_main_ami               <- read.csv("output/unconfoundedness_test/lasso_X_exposure_regression_results-cohort_prevax-main-ami.csv")
lasso_X_exposure_regression_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_X_exposure_regression_results-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_X_exposure_regression_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_X_exposure_regression_results-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_X_exposure_regression_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_X_exposure_regression_results-cohort_prevax-main-stroke_sahhs.csv")
lasso_X_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_X_exposure_regression_results-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_X_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_X_exposure_regression_results-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso_X outcome regression results
lasso_X_outcome_regression_main_ami               <- read.csv("output/unconfoundedness_test/lasso_X_outcome_regression_results-cohort_prevax-main-ami.csv")
lasso_X_outcome_regression_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_X_outcome_regression_results-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_X_outcome_regression_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_X_outcome_regression_results-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_X_outcome_regression_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_X_outcome_regression_results-cohort_prevax-main-stroke_sahhs.csv")
lasso_X_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_X_outcome_regression_results-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_X_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_X_outcome_regression_results-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso_X test tables
lasso_X_test_table_main_ami               <- read.csv("output/unconfoundedness_test/lasso_X_test_table-cohort_prevax-main-ami.csv")
lasso_X_test_table_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_X_test_table-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_X_test_table_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_X_test_table-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_X_test_table_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_X_test_table-cohort_prevax-main-stroke_sahhs.csv")
lasso_X_test_table_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_X_test_table-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_X_test_table_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_X_test_table-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso_union exposure regression results
lasso_union_exposure_regression_main_ami               <- read.csv("output/unconfoundedness_test/lasso_union_exposure_regression_results-cohort_prevax-main-ami.csv")
lasso_union_exposure_regression_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_union_exposure_regression_results-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_union_exposure_regression_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_union_exposure_regression_results-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_union_exposure_regression_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_union_exposure_regression_results-cohort_prevax-main-stroke_sahhs.csv")
lasso_union_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_union_exposure_regression_results-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_union_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_union_exposure_regression_results-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso_union outcome regression results
lasso_union_outcome_regression_main_ami               <- read.csv("output/unconfoundedness_test/lasso_union_outcome_regression_results-cohort_prevax-main-ami.csv")
lasso_union_outcome_regression_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_union_outcome_regression_results-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_union_outcome_regression_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_union_outcome_regression_results-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_union_outcome_regression_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_union_outcome_regression_results-cohort_prevax-main-stroke_sahhs.csv")
lasso_union_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_union_outcome_regression_results-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_union_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_union_outcome_regression_results-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")

# load all lasso_union test tables
lasso_union_test_table_main_ami               <- read.csv("output/unconfoundedness_test/lasso_union_test_table-cohort_prevax-main-ami.csv")
lasso_union_test_table_sub_covidhospital_FALSE_ami          <- read.csv("output/unconfoundedness_test/lasso_union_test_table-cohort_prevax-sub_covidhospital_FALSE-ami.csv")
lasso_union_test_table_sub_covidhospital_TRUE_ami           <- read.csv("output/unconfoundedness_test/lasso_union_test_table-cohort_prevax-sub_covidhospital_TRUE-ami.csv")
lasso_union_test_table_main_stroke_sahhs      <- read.csv("output/unconfoundedness_test/lasso_union_test_table-cohort_prevax-main-stroke_sahhs.csv")
lasso_union_test_table_sub_covidhospital_FALSE_stroke_sahhs <- read.csv("output/unconfoundedness_test/lasso_union_test_table-cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs.csv")
lasso_union_test_table_sub_covidhospital_TRUE_stroke_sahhs  <- read.csv("output/unconfoundedness_test/lasso_union_test_table-cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs.csv")


# Add method, name and response columns to each fully_adjusted regression table ----------------------
print("Add method, name and response columns to each fully_adjusted regression table")

fully_adjusted_exposure_regression_main_ami <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-main-ami",
  response = "exposure",
  fully_adjusted_exposure_regression_main_ami
)
fully_adjusted_exposure_regression_main_ami <- subset(
  fully_adjusted_exposure_regression_main_ami,
  select = -c(X)
)

fully_adjusted_exposure_regression_sub_covidhospital_FALSE_ami <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-sub_covidhospital_FALSE-ami",
  response = "exposure",
  fully_adjusted_exposure_regression_sub_covidhospital_FALSE_ami
)
fully_adjusted_exposure_regression_sub_covidhospital_FALSE_ami <- subset(
  fully_adjusted_exposure_regression_sub_covidhospital_FALSE_ami,
  select = -c(X)
)

fully_adjusted_exposure_regression_sub_covidhospital_TRUE_ami <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-sub_covidhospital_TRUE-ami",
  response = "exposure",
  fully_adjusted_exposure_regression_sub_covidhospital_TRUE_ami
)
fully_adjusted_exposure_regression_sub_covidhospital_TRUE_ami <- subset(
  fully_adjusted_exposure_regression_sub_covidhospital_TRUE_ami,
  select = -c(X)
)

fully_adjusted_exposure_regression_main_stroke_sahhs <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-main-stroke_sahhs",
  response = "exposure",
  fully_adjusted_exposure_regression_main_stroke_sahhs
)
fully_adjusted_exposure_regression_main_stroke_sahhs <- subset(
  fully_adjusted_exposure_regression_main_stroke_sahhs,
  select = -c(X)
)

fully_adjusted_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  response = "exposure",
  fully_adjusted_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs
)
fully_adjusted_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- subset(
  fully_adjusted_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs,
  select = -c(X)
)

fully_adjusted_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  response = "exposure",
  fully_adjusted_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs
)
fully_adjusted_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs <- subset(
  fully_adjusted_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs,
  select = -c(X)
)

fully_adjusted_outcome_regression_main_ami <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-main-ami",
  response = "outcome",
  fully_adjusted_outcome_regression_main_ami
)
fully_adjusted_outcome_regression_main_ami <- subset(
  fully_adjusted_outcome_regression_main_ami,
  select = -c(X)
)

fully_adjusted_outcome_regression_sub_covidhospital_FALSE_ami <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-sub_covidhospital_FALSE-ami",
  response = "outcome",
  fully_adjusted_outcome_regression_sub_covidhospital_FALSE_ami
)
fully_adjusted_outcome_regression_sub_covidhospital_FALSE_ami <- subset(
  fully_adjusted_outcome_regression_sub_covidhospital_FALSE_ami,
  select = -c(X)
)

fully_adjusted_outcome_regression_sub_covidhospital_TRUE_ami <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-sub_covidhospital_TRUE-ami",
  response = "outcome",
  fully_adjusted_outcome_regression_sub_covidhospital_TRUE_ami
)
fully_adjusted_outcome_regression_sub_covidhospital_TRUE_ami <- subset(
  fully_adjusted_outcome_regression_sub_covidhospital_TRUE_ami,
  select = -c(X)
)

fully_adjusted_outcome_regression_main_stroke_sahhs <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-main-stroke_sahhs",
  response = "outcome",
  fully_adjusted_outcome_regression_main_stroke_sahhs
)
fully_adjusted_outcome_regression_main_stroke_sahhs <- subset(
  fully_adjusted_outcome_regression_main_stroke_sahhs,
  select = -c(X)
)

fully_adjusted_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  response = "outcome",
  fully_adjusted_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs
)
fully_adjusted_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- subset(
  fully_adjusted_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs,
  select = -c(X)
)

fully_adjusted_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method   = "fully_adjusted",
  name     = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  response = "outcome",
  fully_adjusted_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs
)
fully_adjusted_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs <- subset(
  fully_adjusted_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs,
  select = -c(X)
)


# Stack fully_adjusted tables -----
print("Stack fully_adjusted tables")

fully_adjusted_all_regression <- rbind(
  fully_adjusted_exposure_regression_main_ami,
  fully_adjusted_exposure_regression_sub_covidhospital_FALSE_ami,
  fully_adjusted_exposure_regression_sub_covidhospital_TRUE_ami,
  fully_adjusted_exposure_regression_main_stroke_sahhs,
  fully_adjusted_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs,
  fully_adjusted_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs,
  fully_adjusted_outcome_regression_main_ami,
  fully_adjusted_outcome_regression_sub_covidhospital_FALSE_ami,
  fully_adjusted_outcome_regression_sub_covidhospital_TRUE_ami,
  fully_adjusted_outcome_regression_main_stroke_sahhs,
  fully_adjusted_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs,
  fully_adjusted_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs
)


# Add method, name and response columns to each lasso regression table ----------------------
print("Add method, name and response columns to each lasso regression table")

lasso_exposure_regression_main_ami <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-main-ami",
  response = "exposure",
  lasso_exposure_regression_main_ami
)
lasso_exposure_regression_main_ami <- subset(
  lasso_exposure_regression_main_ami,
  select = -c(X)
)

lasso_exposure_regression_sub_covidhospital_FALSE_ami <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-sub_covidhospital_FALSE-ami",
  response = "exposure",
  lasso_exposure_regression_sub_covidhospital_FALSE_ami
)
lasso_exposure_regression_sub_covidhospital_FALSE_ami <- subset(
  lasso_exposure_regression_sub_covidhospital_FALSE_ami,
  select = -c(X)
)

lasso_exposure_regression_sub_covidhospital_TRUE_ami <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-sub_covidhospital_TRUE-ami",
  response = "exposure",
  lasso_exposure_regression_sub_covidhospital_TRUE_ami
)
lasso_exposure_regression_sub_covidhospital_TRUE_ami <- subset(
  lasso_exposure_regression_sub_covidhospital_TRUE_ami,
  select = -c(X)
)

lasso_exposure_regression_main_stroke_sahhs <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-main-stroke_sahhs",
  response = "exposure",
  lasso_exposure_regression_main_stroke_sahhs
)
lasso_exposure_regression_main_stroke_sahhs <- subset(
  lasso_exposure_regression_main_stroke_sahhs,
  select = -c(X)
)

lasso_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  response = "exposure",
  lasso_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs
)
lasso_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- subset(
  lasso_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs,
  select = -c(X)
)

lasso_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  response = "exposure",
  lasso_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs
)
lasso_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs <- subset(
  lasso_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs,
  select = -c(X)
)

lasso_outcome_regression_main_ami <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-main-ami",
  response = "outcome",
  lasso_outcome_regression_main_ami
)
lasso_outcome_regression_main_ami <- subset(
  lasso_outcome_regression_main_ami,
  select = -c(X)
)

lasso_outcome_regression_sub_covidhospital_FALSE_ami <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-sub_covidhospital_FALSE-ami",
  response = "outcome",
  lasso_outcome_regression_sub_covidhospital_FALSE_ami
)
lasso_outcome_regression_sub_covidhospital_FALSE_ami <- subset(
  lasso_outcome_regression_sub_covidhospital_FALSE_ami,
  select = -c(X)
)

lasso_outcome_regression_sub_covidhospital_TRUE_ami <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-sub_covidhospital_TRUE-ami",
  response = "outcome",
  lasso_outcome_regression_sub_covidhospital_TRUE_ami
)
lasso_outcome_regression_sub_covidhospital_TRUE_ami <- subset(
  lasso_outcome_regression_sub_covidhospital_TRUE_ami,
  select = -c(X)
)

lasso_outcome_regression_main_stroke_sahhs <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-main-stroke_sahhs",
  response = "outcome",
  lasso_outcome_regression_main_stroke_sahhs
)
lasso_outcome_regression_main_stroke_sahhs <- subset(
  lasso_outcome_regression_main_stroke_sahhs,
  select = -c(X)
)

lasso_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  response = "outcome",
  lasso_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs
)
lasso_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- subset(
  lasso_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs,
  select = -c(X)
)

lasso_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method   = "lasso",
  name     = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  response = "outcome",
  lasso_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs
)
lasso_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs <- subset(
  lasso_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs,
  select = -c(X)
)


# Stack lasso tables -----
print("Stack lasso tables")

lasso_all_regression <- rbind(
  lasso_exposure_regression_main_ami,
  lasso_exposure_regression_sub_covidhospital_FALSE_ami,
  lasso_exposure_regression_sub_covidhospital_TRUE_ami,
  lasso_exposure_regression_main_stroke_sahhs,
  lasso_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs,
  lasso_outcome_regression_main_ami,
  lasso_outcome_regression_sub_covidhospital_FALSE_ami,
  lasso_outcome_regression_sub_covidhospital_TRUE_ami,
  lasso_outcome_regression_main_stroke_sahhs,
  lasso_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs
)


# Add method, name and response columns to each lasso_X regression table ----------------------
print("Add method, name and response columns to each lasso_X regression table")

lasso_X_exposure_regression_main_ami <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-main-ami",
  response = "exposure",
  lasso_X_exposure_regression_main_ami
)
lasso_X_exposure_regression_main_ami <- subset(
  lasso_X_exposure_regression_main_ami,
  select = -c(X)
)

lasso_X_exposure_regression_sub_covidhospital_FALSE_ami <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-sub_covidhospital_FALSE-ami",
  response = "exposure",
  lasso_X_exposure_regression_sub_covidhospital_FALSE_ami
)
lasso_X_exposure_regression_sub_covidhospital_FALSE_ami <- subset(
  lasso_X_exposure_regression_sub_covidhospital_FALSE_ami,
  select = -c(X)
)

lasso_X_exposure_regression_sub_covidhospital_TRUE_ami <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-sub_covidhospital_TRUE-ami",
  response = "exposure",
  lasso_X_exposure_regression_sub_covidhospital_TRUE_ami
)
lasso_X_exposure_regression_sub_covidhospital_TRUE_ami <- subset(
  lasso_X_exposure_regression_sub_covidhospital_TRUE_ami,
  select = -c(X)
)

lasso_X_exposure_regression_main_stroke_sahhs <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-main-stroke_sahhs",
  response = "exposure",
  lasso_X_exposure_regression_main_stroke_sahhs
)
lasso_X_exposure_regression_main_stroke_sahhs <- subset(
  lasso_X_exposure_regression_main_stroke_sahhs,
  select = -c(X)
)

lasso_X_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  response = "exposure",
  lasso_X_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs
)
lasso_X_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- subset(
  lasso_X_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs,
  select = -c(X)
)

lasso_X_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  response = "exposure",
  lasso_X_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs
)
lasso_X_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs <- subset(
  lasso_X_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs,
  select = -c(X)
)

lasso_X_outcome_regression_main_ami <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-main-ami",
  response = "outcome",
  lasso_X_outcome_regression_main_ami
)
lasso_X_outcome_regression_main_ami <- subset(
  lasso_X_outcome_regression_main_ami,
  select = -c(X)
)

lasso_X_outcome_regression_sub_covidhospital_FALSE_ami <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-sub_covidhospital_FALSE-ami",
  response = "outcome",
  lasso_X_outcome_regression_sub_covidhospital_FALSE_ami
)
lasso_X_outcome_regression_sub_covidhospital_FALSE_ami <- subset(
  lasso_X_outcome_regression_sub_covidhospital_FALSE_ami,
  select = -c(X)
)

lasso_X_outcome_regression_sub_covidhospital_TRUE_ami <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-sub_covidhospital_TRUE-ami",
  response = "outcome",
  lasso_X_outcome_regression_sub_covidhospital_TRUE_ami
)
lasso_X_outcome_regression_sub_covidhospital_TRUE_ami <- subset(
  lasso_X_outcome_regression_sub_covidhospital_TRUE_ami,
  select = -c(X)
)

lasso_X_outcome_regression_main_stroke_sahhs <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-main-stroke_sahhs",
  response = "outcome",
  lasso_X_outcome_regression_main_stroke_sahhs
)
lasso_X_outcome_regression_main_stroke_sahhs <- subset(
  lasso_X_outcome_regression_main_stroke_sahhs,
  select = -c(X)
)

lasso_X_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  response = "outcome",
  lasso_X_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs
)
lasso_X_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- subset(
  lasso_X_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs,
  select = -c(X)
)

lasso_X_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method   = "lasso_X",
  name     = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  response = "outcome",
  lasso_X_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs
)
lasso_X_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs <- subset(
  lasso_X_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs,
  select = -c(X)
)


# Stack lasso_X tables -----
print("Stack lasso_X tables")

lasso_X_all_regression <- rbind(
  lasso_X_exposure_regression_main_ami,
  lasso_X_exposure_regression_sub_covidhospital_FALSE_ami,
  lasso_X_exposure_regression_sub_covidhospital_TRUE_ami,
  lasso_X_exposure_regression_main_stroke_sahhs,
  lasso_X_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_X_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs,
  lasso_X_outcome_regression_main_ami,
  lasso_X_outcome_regression_sub_covidhospital_FALSE_ami,
  lasso_X_outcome_regression_sub_covidhospital_TRUE_ami,
  lasso_X_outcome_regression_main_stroke_sahhs,
  lasso_X_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_X_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs
)


# Add method, name and response columns to each lasso_union regression table ----------------------
print("Add method, name and response columns to each lasso_union regression table")

lasso_union_exposure_regression_main_ami <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-main-ami",
  response = "exposure",
  lasso_union_exposure_regression_main_ami
)
lasso_union_exposure_regression_main_ami <- subset(
  lasso_union_exposure_regression_main_ami,
  select = -c(X)
)

lasso_union_exposure_regression_sub_covidhospital_FALSE_ami <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-sub_covidhospital_FALSE-ami",
  response = "exposure",
  lasso_union_exposure_regression_sub_covidhospital_FALSE_ami
)
lasso_union_exposure_regression_sub_covidhospital_FALSE_ami <- subset(
  lasso_union_exposure_regression_sub_covidhospital_FALSE_ami,
  select = -c(X)
)

lasso_union_exposure_regression_sub_covidhospital_TRUE_ami <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-sub_covidhospital_TRUE-ami",
  response = "exposure",
  lasso_union_exposure_regression_sub_covidhospital_TRUE_ami
)
lasso_union_exposure_regression_sub_covidhospital_TRUE_ami <- subset(
  lasso_union_exposure_regression_sub_covidhospital_TRUE_ami,
  select = -c(X)
)

lasso_union_exposure_regression_main_stroke_sahhs <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-main-stroke_sahhs",
  response = "exposure",
  lasso_union_exposure_regression_main_stroke_sahhs
)
lasso_union_exposure_regression_main_stroke_sahhs <- subset(
  lasso_union_exposure_regression_main_stroke_sahhs,
  select = -c(X)
)

lasso_union_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  response = "exposure",
  lasso_union_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs
)
lasso_union_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs <- subset(
  lasso_union_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs,
  select = -c(X)
)

lasso_union_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  response = "exposure",
  lasso_union_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs
)
lasso_union_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs <- subset(
  lasso_union_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs,
  select = -c(X)
)

lasso_union_outcome_regression_main_ami <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-main-ami",
  response = "outcome",
  lasso_union_outcome_regression_main_ami
)
lasso_union_outcome_regression_main_ami <- subset(
  lasso_union_outcome_regression_main_ami,
  select = -c(X)
)

lasso_union_outcome_regression_sub_covidhospital_FALSE_ami <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-sub_covidhospital_FALSE-ami",
  response = "outcome",
  lasso_union_outcome_regression_sub_covidhospital_FALSE_ami
)
lasso_union_outcome_regression_sub_covidhospital_FALSE_ami <- subset(
  lasso_union_outcome_regression_sub_covidhospital_FALSE_ami,
  select = -c(X)
)

lasso_union_outcome_regression_sub_covidhospital_TRUE_ami <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-sub_covidhospital_TRUE-ami",
  response = "outcome",
  lasso_union_outcome_regression_sub_covidhospital_TRUE_ami
)
lasso_union_outcome_regression_sub_covidhospital_TRUE_ami <- subset(
  lasso_union_outcome_regression_sub_covidhospital_TRUE_ami,
  select = -c(X)
)

lasso_union_outcome_regression_main_stroke_sahhs <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-main-stroke_sahhs",
  response = "outcome",
  lasso_union_outcome_regression_main_stroke_sahhs
)
lasso_union_outcome_regression_main_stroke_sahhs <- subset(
  lasso_union_outcome_regression_main_stroke_sahhs,
  select = -c(X)
)

lasso_union_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  response = "outcome",
  lasso_union_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs
)
lasso_union_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs <- subset(
  lasso_union_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs,
  select = -c(X)
)

lasso_union_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method   = "lasso_union",
  name     = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  response = "outcome",
  lasso_union_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs
)
lasso_union_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs <- subset(
  lasso_union_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs,
  select = -c(X)
)


# Stack lasso_union tables -----
print("Stack lasso_union tables")

lasso_union_all_regression <- rbind(
  lasso_union_exposure_regression_main_ami,
  lasso_union_exposure_regression_sub_covidhospital_FALSE_ami,
  lasso_union_exposure_regression_sub_covidhospital_TRUE_ami,
  lasso_union_exposure_regression_main_stroke_sahhs,
  lasso_union_exposure_regression_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_union_exposure_regression_sub_covidhospital_TRUE_stroke_sahhs,
  lasso_union_outcome_regression_main_ami,
  lasso_union_outcome_regression_sub_covidhospital_FALSE_ami,
  lasso_union_outcome_regression_sub_covidhospital_TRUE_ami,
  lasso_union_outcome_regression_main_stroke_sahhs,
  lasso_union_outcome_regression_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_union_outcome_regression_sub_covidhospital_TRUE_stroke_sahhs
)


# Stack all regression tables -----
print("Stack all regression tables")

all_regression <- rbind(
  fully_adjusted_all_regression,
  lasso_all_regression,
  lasso_X_all_regression,
  lasso_union_all_regression
)

print(all_regression)
print(dim(all_regression))


# Add method and name columns to each test table table ----------------------
print("Add method and name columns to each test table table")

fully_adjusted_test_table_main_ami <- cbind(
  method = "fully_adjusted",
  name   = "cohort_prevax-main-ami",
  fully_adjusted_test_table_main_ami
)
colnames(fully_adjusted_test_table_main_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

fully_adjusted_test_table_sub_covidhospital_FALSE_ami <- cbind(
  method = "fully_adjusted",
  name   = "cohort_prevax-sub_covidhospital_FALSE-ami",
  fully_adjusted_test_table_sub_covidhospital_FALSE_ami
)
colnames(fully_adjusted_test_table_sub_covidhospital_FALSE_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

fully_adjusted_test_table_sub_covidhospital_TRUE_ami <- cbind(
  method = "fully_adjusted",
  name   = "cohort_prevax-sub_covidhospital_TRUE-ami",
  fully_adjusted_test_table_sub_covidhospital_TRUE_ami
)
colnames(fully_adjusted_test_table_sub_covidhospital_TRUE_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

fully_adjusted_test_table_main_stroke_sahhs <- cbind(
  method = "fully_adjusted",
  name   = "cohort_prevax-main-stroke_sahhs",
  fully_adjusted_test_table_main_stroke_sahhs
)
colnames(fully_adjusted_test_table_main_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

fully_adjusted_test_table_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method = "fully_adjusted",
  name   = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  fully_adjusted_test_table_sub_covidhospital_FALSE_stroke_sahhs
)
colnames(fully_adjusted_test_table_sub_covidhospital_FALSE_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

fully_adjusted_test_table_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method = "fully_adjusted",
  name   = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  fully_adjusted_test_table_sub_covidhospital_TRUE_stroke_sahhs
)
colnames(fully_adjusted_test_table_sub_covidhospital_TRUE_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_test_table_main_ami <- cbind(
  method = "lasso",
  name   = "cohort_prevax-main-ami",
  lasso_test_table_main_ami
)
colnames(lasso_test_table_main_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_test_table_sub_covidhospital_FALSE_ami <- cbind(
  method = "lasso",
  name   = "cohort_prevax-sub_covidhospital_FALSE-ami",
  lasso_test_table_sub_covidhospital_FALSE_ami
)
colnames(lasso_test_table_sub_covidhospital_FALSE_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_test_table_sub_covidhospital_TRUE_ami <- cbind(
  method = "lasso",
  name   = "cohort_prevax-sub_covidhospital_TRUE-ami",
  lasso_test_table_sub_covidhospital_TRUE_ami
)
colnames(lasso_test_table_sub_covidhospital_TRUE_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_test_table_main_stroke_sahhs <- cbind(
  method = "lasso",
  name   = "cohort_prevax-main-stroke_sahhs",
  lasso_test_table_main_stroke_sahhs
)
colnames(lasso_test_table_main_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_test_table_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method = "lasso",
  name   = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  lasso_test_table_sub_covidhospital_FALSE_stroke_sahhs
)
colnames(lasso_test_table_sub_covidhospital_FALSE_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_test_table_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method = "lasso",
  name   = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  lasso_test_table_sub_covidhospital_TRUE_stroke_sahhs
)
colnames(lasso_test_table_sub_covidhospital_TRUE_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_X_test_table_main_ami <- cbind(
  method = "lasso_X",
  name   = "cohort_prevax-main-ami",
  lasso_X_test_table_main_ami
)
colnames(lasso_X_test_table_main_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_X_test_table_sub_covidhospital_FALSE_ami <- cbind(
  method = "lasso_X",
  name   = "cohort_prevax-sub_covidhospital_FALSE-ami",
  lasso_X_test_table_sub_covidhospital_FALSE_ami
)
colnames(lasso_X_test_table_sub_covidhospital_FALSE_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_X_test_table_sub_covidhospital_TRUE_ami <- cbind(
  method = "lasso_X",
  name   = "cohort_prevax-sub_covidhospital_TRUE-ami",
  lasso_X_test_table_sub_covidhospital_TRUE_ami
)
colnames(lasso_X_test_table_sub_covidhospital_TRUE_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_X_test_table_main_stroke_sahhs <- cbind(
  method = "lasso_X",
  name   = "cohort_prevax-main-stroke_sahhs",
  lasso_X_test_table_main_stroke_sahhs
)
colnames(lasso_X_test_table_main_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_X_test_table_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method = "lasso_X",
  name   = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  lasso_X_test_table_sub_covidhospital_FALSE_stroke_sahhs
)
colnames(lasso_X_test_table_sub_covidhospital_FALSE_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_X_test_table_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method = "lasso_X",
  name   = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  lasso_X_test_table_sub_covidhospital_TRUE_stroke_sahhs
)
colnames(lasso_X_test_table_sub_covidhospital_TRUE_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_union_test_table_main_ami <- cbind(
  method = "lasso_union",
  name   = "cohort_prevax-main-ami",
  lasso_union_test_table_main_ami
)
colnames(lasso_union_test_table_main_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_union_test_table_sub_covidhospital_FALSE_ami <- cbind(
  method = "lasso_union",
  name   = "cohort_prevax-sub_covidhospital_FALSE-ami",
  lasso_union_test_table_sub_covidhospital_FALSE_ami
)
colnames(lasso_union_test_table_sub_covidhospital_FALSE_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_union_test_table_sub_covidhospital_TRUE_ami <- cbind(
  method = "lasso_union",
  name   = "cohort_prevax-sub_covidhospital_TRUE-ami",
  lasso_union_test_table_sub_covidhospital_TRUE_ami
)
colnames(lasso_union_test_table_sub_covidhospital_TRUE_ami) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_union_test_table_main_stroke_sahhs <- cbind(
  method = "lasso_union",
  name   = "cohort_prevax-main-stroke_sahhs",
  lasso_union_test_table_main_stroke_sahhs
)
colnames(lasso_union_test_table_main_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_union_test_table_sub_covidhospital_FALSE_stroke_sahhs <- cbind(
  method = "lasso_union",
  name   = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  lasso_union_test_table_sub_covidhospital_FALSE_stroke_sahhs
)
colnames(lasso_union_test_table_sub_covidhospital_FALSE_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)

lasso_union_test_table_sub_covidhospital_TRUE_stroke_sahhs <- cbind(
  method = "lasso_union",
  name   = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  lasso_union_test_table_sub_covidhospital_TRUE_stroke_sahhs
)
colnames(lasso_union_test_table_sub_covidhospital_TRUE_stroke_sahhs) <- c(
  "method", "name", "covariate", "condition_i", "condition_ii", "condition_i_and_ii"
)


# Stack all test tables -----
print("Stack all test tables")

all_test_tables <- rbind(
  fully_adjusted_test_table_main_ami,
  fully_adjusted_test_table_sub_covidhospital_FALSE_ami,
  fully_adjusted_test_table_sub_covidhospital_TRUE_ami,
  fully_adjusted_test_table_main_stroke_sahhs,
  fully_adjusted_test_table_sub_covidhospital_FALSE_stroke_sahhs,
  fully_adjusted_test_table_sub_covidhospital_TRUE_stroke_sahhs,
  lasso_test_table_main_ami,
  lasso_test_table_sub_covidhospital_FALSE_ami,
  lasso_test_table_sub_covidhospital_TRUE_ami,
  lasso_test_table_main_stroke_sahhs,
  lasso_test_table_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_test_table_sub_covidhospital_TRUE_stroke_sahhs,
  lasso_X_test_table_main_ami,
  lasso_X_test_table_sub_covidhospital_FALSE_ami,
  lasso_X_test_table_sub_covidhospital_TRUE_ami,
  lasso_X_test_table_main_stroke_sahhs,
  lasso_X_test_table_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_X_test_table_sub_covidhospital_TRUE_stroke_sahhs,
  lasso_union_test_table_main_ami,
  lasso_union_test_table_sub_covidhospital_FALSE_ami,
  lasso_union_test_table_sub_covidhospital_TRUE_ami,
  lasso_union_test_table_main_stroke_sahhs,
  lasso_union_test_table_sub_covidhospital_FALSE_stroke_sahhs,
  lasso_union_test_table_sub_covidhospital_TRUE_stroke_sahhs
)


# Add name column to each conclusion table ----------------------
print("Add name column to each conclusion table")

main_ami_conclusion_table <- cbind(
  name = "cohort_prevax-main-ami",
  main_ami_conclusion_table
)
main_ami_conclusion_table <- subset(
  main_ami_conclusion_table,
  select = -c(X)
)

sub_covidhospital_FALSE_ami_conclusion_table <- cbind(
  name = "cohort_prevax-sub_covidhospital_FALSE-ami",
  sub_covidhospital_FALSE_ami_conclusion_table
)
sub_covidhospital_FALSE_ami_conclusion_table <- subset(
  sub_covidhospital_FALSE_ami_conclusion_table,
  select = -c(X)
)

sub_covidhospital_TRUE_ami_conclusion_table <- cbind(
  name = "cohort_prevax-sub_covidhospital_TRUE-ami",
  sub_covidhospital_TRUE_ami_conclusion_table
)
sub_covidhospital_TRUE_ami_conclusion_table <- subset(
  sub_covidhospital_TRUE_ami_conclusion_table,
  select = -c(X)
)

main_stroke_sahhs_conclusion_table <- cbind(
  name = "cohort_prevax-main-stroke_sahhs",
  main_stroke_sahhs_conclusion_table
)
main_stroke_sahhs_conclusion_table <- subset(
  main_stroke_sahhs_conclusion_table,
  select = -c(X)
)

sub_covidhospital_FALSE_stroke_sahhs_conclusion_table <- cbind(
  name = "cohort_prevax-sub_covidhospital_FALSE-stroke_sahhs",
  sub_covidhospital_FALSE_stroke_sahhs_conclusion_table
)
sub_covidhospital_FALSE_stroke_sahhs_conclusion_table <- subset(
  sub_covidhospital_FALSE_stroke_sahhs_conclusion_table,
  select = -c(X)
)

sub_covidhospital_TRUE_stroke_sahhs_conclusion_table <- cbind(
  name = "cohort_prevax-sub_covidhospital_TRUE-stroke_sahhs",
  sub_covidhospital_TRUE_stroke_sahhs_conclusion_table
)
sub_covidhospital_TRUE_stroke_sahhs_conclusion_table <- subset(
  sub_covidhospital_TRUE_stroke_sahhs_conclusion_table,
  select = -c(X)
)


# Stack all conclusion tables -----
print("Stack all conclusion tables")

all_conclusion_tables <- rbind(
  main_ami_conclusion_table,
  sub_covidhospital_FALSE_ami_conclusion_table,
  sub_covidhospital_TRUE_ami_conclusion_table,
  main_stroke_sahhs_conclusion_table,
  sub_covidhospital_FALSE_stroke_sahhs_conclusion_table,
  sub_covidhospital_TRUE_stroke_sahhs_conclusion_table
)


# Save all final results tables -----
print("Save all final results tables")

colnames(all_conclusion_tables) <- c(
  "name", "method", "test_result", "interpretation"
)

write.csv(
  all_regression,
  paste0(makeout_dir, "unconfoundedness_test_all_regression_results.csv"),
  row.names = FALSE
)

write.csv(
  all_test_tables,
  paste0(makeout_dir, "unconfoundedness_test_all_test_tables.csv"),
  row.names = FALSE
)

write.csv(
  all_conclusion_tables,
  paste0(makeout_dir, "unconfoundedness_test_all_conclusion_tables.csv"),
  row.names = FALSE
)
