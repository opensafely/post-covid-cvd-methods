# Load packages ----------------------------------------------------------------
print("Load packages")

library(magrittr)
library(data.table)
library(stringr)
library(lubridate)

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

# Load and prepare data by selecting project-required columns
print("Load and prepare data for analysis")

pmi <- prepare_model_input(name)

# Restrict to required population -------------------------------------------
print('Restrict to required population')

# Creating a pre-existing condition variable where appropriate
if (grepl("preex", name)) {
  # True false indicator of preex
  preex <- as.logical(
    gsub(
      ".*preex_([^\\-]+)-.*",
      "\\1",
      name
    )
  )

  # Remove preex string from analysis string
  analysis <- gsub(
    "_preex_.*",
    "",
    analysis
  )
  df <- pmi$input[pmi$input$sup_bin_preex == preex, ]
} else {
  df <- pmi$input
}

## Perform subgroup-specific manipulation
print("Perform subgroup-specific manipulation")

print(paste0("Make model input: ", analysis))

check_for_subgroup <- (grepl("main", analysis)) # TRUE if subgroup is main, FALSE otherwise

# Make model input: main/sub_covidhistory ------------------------------------
if (grepl("sub_covidhistory", analysis)) {
  check_for_subgroup <- TRUE
  df <- df[df$sub_bin_covidhistory == TRUE, ] # Only selecting for this subgroup
} else {
  df <- df[df$sub_bin_covidhistory == FALSE, ] # all other subgroups (inc. Main)
}

# Stop code if no subgroup/main analysis was correctly selected ------------
if (isFALSE(check_for_subgroup)) {
  stop(paste0("Input: ", name, " did not undergo any subgroup filtering!"))
}

# Save model output
df <- df %>%
  dplyr::select(tidyselect::all_of(pmi$keep))

check_vitals(df)
readr::write_rds(
  df,
  file.path(
    model_dir,
    paste0("model_input-", name, ".rds")
  ),
  compress = "gz"
)
print(paste0(
  "Saved: ",
  model_dir,
  "model_input-",
  name,
  ".rds"
))
