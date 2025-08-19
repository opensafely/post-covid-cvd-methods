# ------------------------------------------------------------------------------
#
# table1.R
#
# This file generates the "patient characteristics" table
# for the replicated Covid-19 x Cardiovascular paper
# Original text: https://doi.org/10.1038/s41467-024-46497-0
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
#  - The patient characteristics data table, rounded
#    (output/table1/table1-cohort_prevax-midpoint6.csv)
#  - The patient characteristics data table
#    (output/table1/table1-cohort_prevax.csv)
#
# Authors: Emma Tarmey, Venexia Walker, UoB ehrQL Team
#
# ------------------------------------------------------------------------------


# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)

# Define table1 output folder ---------------------------------------------------------
print("Creating output/table1 output folder")

table1_dir <- "output/table1/"
fs::dir_create(here::here(table1_dir))

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
  preex   <- "All"
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

# Table 1 Processing Start -----------------------------------------------------
print("Table 1 processing")

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

df$cov_cat_consrate2019 <- numerical_to_categorical(
  df$cov_num_consrate2019,
  c(1, 6),
  zero_flag = TRUE
)

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

# Filter binary data

for (colname in colnames(df)[grepl("cov_bin_", colnames(df))]) {
  df[[colname]] <- sapply(df[[colname]], as.character)
}

df <- df %>%
  mutate(across(where(is.factor), as.character))


# Convert to characteristics and subcharacteristics ----------------------------
print("Convert to characteristics and subcharacteristics")

df <- tidyr::pivot_longer(
  df,
  cols = setdiff(colnames(df), c("patient_id", "exposed")),
  names_to = "characteristic",
  values_to = "subcharacteristic"
)

df$total <- 1

# Tidy missing data labels -----------------------------------------------------
print("Tidy missing data labels")

df$subcharacteristic <- ifelse(
  df$subcharacteristic == "" |
    df$subcharacteristic == "unknown" |
    is.na(df$subcharacteristic),
  "Missing",
  df$subcharacteristic
)

# Aggregate data ---------------------------------------------------------------

print("Aggregate data")

df <- aggregate(
  cbind(total, exposed) ~ characteristic + subcharacteristic,
  data = df,
  sum
)


# Sort characteristics ---------------------------------------------------------
print("Sort characteristics")

df <- df[order(df$characteristic, df$subcharacteristic), ]

# Add in Median IQR
print('Add median (IQR) age')

# Pastes: "Mean Age (LQ Age - UQ Age)" as a string for each cohort
df[nrow(df) + 1, ] <- c("Age, years", "Median (IQR)", median_iqr_age, 0)

# Save Table 1 -----------------------------------------------------------------
print("Save Table 1")

write.csv(
  df,
  paste0(table1_dir, "table1-cohort_", cohort, preex_string, ".csv"),
  row.names = FALSE
)

# Perform redaction ------------------------------------------------------------
print("Perform redaction")

df <- df[df$subcharacteristic != "Median (IQR)", ] # Remove Median IQR row
df <- df[df$subcharacteristic != FALSE, ] # Remove False binary data

df$total_midpoint6 <- roundmid_any(df$total)
df$exposed_midpoint6 <- roundmid_any(df$exposed)

# Calculate column percentages -------------------------------------------------

df$N_midpoint6_derived <- df$total_midpoint6

df$percent_midpoint6_derived <- paste0(
  ifelse(
    df$characteristic == "All",
    "",
    paste0(
      round(
        100 *
          (df$total_midpoint6 /
            df[df$characteristic == "All", "total_midpoint6"]),
        1
      ),
      "%"
    )
  )
)

df$percent_exposed_midpoint6 <- paste0(
  ifelse(
    df$characteristic == "All",
    "",
    paste0(
      round(
        100 *
          (df$exposed_midpoint6 /
            df[df$characteristic == "All", "exposed_midpoint6"]),
        1
      ),
      "%"
    )
  )
)

df <- df[, c(
  "characteristic",
  "subcharacteristic",
  "N_midpoint6_derived",
  "percent_midpoint6_derived",
  "exposed_midpoint6",
  "percent_exposed_midpoint6"
)]

df[nrow(df) + 1, ] <- c("Age, years", "Median (IQR)", median_iqr_age, "", 0, "")

df <- dplyr::rename(
  df,
  "Characteristic" = "characteristic",
  "Subcharacteristic" = "subcharacteristic",
  "N [midpoint6_derived]" = "N_midpoint6_derived",
  "(%) [midpoint6_derived]" = "percent_midpoint6_derived",
  "COVID-19 diagnoses [midpoint6]" = "exposed_midpoint6"
)

# Save Table 1 -----------------------------------------------------------------
print("Save rounded Table 1")

write.csv(
  df,
  paste0(table1_dir, "table1-cohort_", cohort, preex_string, "-midpoint6.csv"),
  row.names = FALSE
)
