# ------------------------------------------------------------------------------
#
# table1_subsample.R
#
# This file generates the "patient characteristics" table
# for the replicated Covid-19 x Cardiovascular paper
# Original text: https://doi.org/10.1038/s41467-024-46497-0
# using the subsample data instead of the full population
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

# Refresh local R session ------------------------------------------------------
print("Refresh local R session")

rm(list=ls())


# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)
library(fs)

# Define table1 output folder ---------------------------------------------------------
print("Creating output/table1 output folder")

table1_dir <- "output/table1/"
dir_create(here::here(table1_dir))

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
  age_str <- "18;30;40;50;60;70;80;90"
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

# subsample, ami
df_ami <- readr::read_rds(paste0(
  "output/generate_subsample/input_",
  cohort,
  "_clean_subsample_ami.rds"
))

# subsample, sahhs
df_sahhs <- readr::read_rds(paste0(
  "output/generate_subsample/input_",
  cohort,
  "_clean_subsample_sahhs.rds"
))


# Check all covariates  --------------------------------------------------------
message("Check all covariates")

df_ami$cov_bin_ami   <- as.logical(df_ami$cov_bin_ami)   # outcome
df_ami$cov_bin_sahhs <- as.logical(df_ami$cov_bin_sahhs) # outcome
df_ami$cov_bin_covid <- as.logical(df_ami$cov_bin_covid) # exposure

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


df_sahhs$cov_bin_sahhs   <- as.logical(df_sahhs$cov_bin_sahhs)   # outcome
df_sahhs$cov_bin_sahhs <- as.logical(df_sahhs$cov_bin_sahhs) # outcome
df_sahhs$cov_bin_covid <- as.logical(df_sahhs$cov_bin_covid) # exposure

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


# Table 1 Processing Start -----------------------------------------------------
print("Table 1 processing")

# Remove people with history of COVID-19 ---------------------------------------
print("Remove people with history of COVID-19")

df_ami <- df_ami[df_ami$sub_bin_covidhistory == FALSE, ]

df_sahhs <- df_sahhs[df_sahhs$sub_bin_covidhistory == FALSE, ]

# Create exposure indicator ----------------------------------------------------
print("Create exposure indicator")

df_ami$exposed <- df_ami$cov_bin_covid

df_sahhs$exposed <- df_sahhs$cov_bin_covid

# Select for pre-existing conditions
print("Select for pre-existing conditions")

# preex optional argument deliberately ignored, sup_bin_preex does not exist
# because neither asthma nor copdoutcomes are present
preex_string <- ""
# if (preex != "All") {
#   df_ami <- df_ami[df_ami$sup_bin_preex == preex, ]
#   preex_string <- paste0("-preex_", preex)
# }

# preex optional argument deliberately ignored, sup_bin_preex does not exist
# because neither asthma nor copdoutcomes are present
preex_string <- ""
# if (preex != "All") {
#   df_sahhs <- df_sahhs[df_sahhs$sup_bin_preex == preex, ]
#   preex_string <- paste0("-preex_", preex)
# }


# Define age groups ------------------------------------------------------------
print("Define age groups")

df_ami$cov_cat_age_group <- numerical_to_categorical(df_ami$cov_num_age, age_bounds) # See utility.R

# df_ami$cov_cat_consrate2019 <- numerical_to_categorical(
#   df_ami$cov_num_consrate2019,
#   c(1, 6),
#   zero_flag = TRUE
# )

median_iqr_age <- create_median_iqr_string(df_ami$cov_num_age) # See utility.R


df_sahhs$cov_cat_age_group <- numerical_to_categorical(df_sahhs$cov_num_age, age_bounds) # See utility.R

# df_sahhs$cov_cat_consrate2019 <- numerical_to_categorical(
#   df_sahhs$cov_num_consrate2019,
#   c(1, 6),
#   zero_flag = TRUE
# )

median_iqr_age <- create_median_iqr_string(df_sahhs$cov_num_age) # See utility.R


# Filter data ------------------------------------------------------------------
print("Filter data")

df_ami <- df_ami[, c(
  "patient_id",
  "exposed",
  colnames(df_ami)[grepl("cov_cat_", colnames(df_ami))],
  colnames(df_ami)[grepl("strat_cat_", colnames(df_ami))],
  colnames(df_ami)[grepl("cov_bin_", colnames(df_ami))]
)]

df_ami$All <- "All"

# Filter binary data

for (colname in colnames(df_ami)[grepl("cov_bin_", colnames(df_ami))]) {
  df_ami[[colname]] <- sapply(df_ami[[colname]], as.character)
}

df_ami <- df_ami %>%
  mutate(across(where(is.factor), as.character))

df_sahhs <- df_sahhs[, c(
  "patient_id",
  "exposed",
  colnames(df_sahhs)[grepl("cov_cat_", colnames(df_sahhs))],
  colnames(df_sahhs)[grepl("strat_cat_", colnames(df_sahhs))],
  colnames(df_sahhs)[grepl("cov_bin_", colnames(df_sahhs))]
)]

df_sahhs$All <- "All"

# Filter binary data

for (colname in colnames(df_sahhs)[grepl("cov_bin_", colnames(df_sahhs))]) {
  df_sahhs[[colname]] <- sapply(df_sahhs[[colname]], as.character)
}

df_sahhs <- df_sahhs %>%
  mutate(across(where(is.factor), as.character))


# Convert to characteristics and subcharacteristics ----------------------------
print("Convert to characteristics and subcharacteristics")

df_ami <- tidyr::pivot_longer(
  df_ami,
  cols = setdiff(colnames(df_ami), c("patient_id", "exposed")),
  names_to = "characteristic",
  values_to = "subcharacteristic"
)

df_ami$total <- 1

df_sahhs <- tidyr::pivot_longer(
  df_sahhs,
  cols = setdiff(colnames(df_sahhs), c("patient_id", "exposed")),
  names_to = "characteristic",
  values_to = "subcharacteristic"
)

df_sahhs$total <- 1

# Tidy missing data labels -----------------------------------------------------
print("Tidy missing data labels")

df_ami$subcharacteristic <- ifelse(
  df_ami$subcharacteristic == "" |
    df_ami$subcharacteristic == "unknown" |
    is.na(df_ami$subcharacteristic),
  "Missing",
  df_ami$subcharacteristic
)

df_sahhs$subcharacteristic <- ifelse(
  df_sahhs$subcharacteristic == "" |
    df_sahhs$subcharacteristic == "unknown" |
    is.na(df_sahhs$subcharacteristic),
  "Missing",
  df_sahhs$subcharacteristic
)

# Aggregate data ---------------------------------------------------------------

print("Aggregate data")

df_ami <- aggregate(
  cbind(total, exposed) ~ characteristic + subcharacteristic,
  data = df_ami,
  sum
)

df_sahhs <- aggregate(
  cbind(total, exposed) ~ characteristic + subcharacteristic,
  data = df_sahhs,
  sum
)


# Sort characteristics ---------------------------------------------------------
print("Sort characteristics")

df_ami <- df_ami[order(df_ami$characteristic, df_ami$subcharacteristic), ]

df_sahhs <- df_sahhs[order(df_sahhs$characteristic, df_sahhs$subcharacteristic), ]


# Add in Median IQR
print('Add median (IQR) age')

# Pastes: "Mean Age (LQ Age - UQ Age)" as a string for each cohort
df_sahhs[nrow(df_sahhs) + 1, ] <- c("Age, years", "Median (IQR)", median_iqr_age, 0)


# Save Table 1 -----------------------------------------------------------------
print("Save Table 1")

write.csv(
  df_ami,
  paste0(table1_dir, "table1-cohort_", cohort, preex_string, "_ami_subsample.csv"),
  row.names = FALSE
)

write.csv(
  df_sahhs,
  paste0(table1_dir, "table1-cohort_", cohort, preex_string, "_sahhs_subsample.csv"),
  row.names = FALSE
)


# Perform redaction ------------------------------------------------------------
print("Perform redaction")

df_ami <- df_ami[df_ami$subcharacteristic != "Median (IQR)", ] # Remove Median IQR row
df_ami <- df_ami[df_ami$subcharacteristic != FALSE, ] # Remove False binary data

df_ami$total_midpoint6 <- roundmid_any(df_ami$total)
df_ami$exposed_midpoint6 <- roundmid_any(df_ami$exposed)

df_sahhs <- df_sahhs[df_sahhs$subcharacteristic != "Median (IQR)", ] # Remove Median IQR row
df_sahhs <- df_sahhs[df_sahhs$subcharacteristic != FALSE, ] # Remove False binary data

df_sahhs$total_midpoint6 <- roundmid_any(df_sahhs$total)
df_sahhs$exposed_midpoint6 <- roundmid_any(df_sahhs$exposed)


# Calculate column percentages -------------------------------------------------
print("Calculate column percentages")

df_ami$N_midpoint6_derived <- df_ami$total_midpoint6

df_ami$percent_midpoint6_derived <- paste0(
  ifelse(
    df_ami$characteristic == "All",
    "",
    paste0(
      round(
        100 *
          (df_ami$total_midpoint6 /
            df_ami[df_ami$characteristic == "All", "total_midpoint6"]),
        1
      ),
      "%"
    )
  )
)

df_ami$percent_exposed_midpoint6 <- paste0(
  ifelse(
    df_ami$characteristic == "All",
    "",
    paste0(
      round(
        100 *
          (df_ami$exposed_midpoint6 /
            df_ami[df_ami$characteristic == "All", "exposed_midpoint6"]),
        1
      ),
      "%"
    )
  )
)

df_ami <- df_ami[, c(
  "characteristic",
  "subcharacteristic",
  "N_midpoint6_derived",
  "percent_midpoint6_derived",
  "exposed_midpoint6",
  "percent_exposed_midpoint6"
)]

df_ami[nrow(df_ami) + 1, ] <- c("Age, years", "Median (IQR)", median_iqr_age, "", 0, "")

df_ami <- dplyr::rename(
  df_ami,
  "Characteristic" = "characteristic",
  "Subcharacteristic" = "subcharacteristic",
  "N [midpoint6_derived]" = "N_midpoint6_derived",
  "(%) [midpoint6_derived]" = "percent_midpoint6_derived",
  "COVID-19 diagnoses [midpoint6]" = "exposed_midpoint6"
)

df_sahhs$N_midpoint6_derived <- df_sahhs$total_midpoint6

df_sahhs$percent_midpoint6_derived <- paste0(
  ifelse(
    df_sahhs$characteristic == "All",
    "",
    paste0(
      round(
        100 *
          (df_sahhs$total_midpoint6 /
            df_sahhs[df_sahhs$characteristic == "All", "total_midpoint6"]),
        1
      ),
      "%"
    )
  )
)

df_sahhs$percent_exposed_midpoint6 <- paste0(
  ifelse(
    df_sahhs$characteristic == "All",
    "",
    paste0(
      round(
        100 *
          (df_sahhs$exposed_midpoint6 /
            df_sahhs[df_sahhs$characteristic == "All", "exposed_midpoint6"]),
        1
      ),
      "%"
    )
  )
)

df_sahhs <- df_sahhs[, c(
  "characteristic",
  "subcharacteristic",
  "N_midpoint6_derived",
  "percent_midpoint6_derived",
  "exposed_midpoint6",
  "percent_exposed_midpoint6"
)]

df_sahhs[nrow(df_sahhs) + 1, ] <- c("Age, years", "Median (IQR)", median_iqr_age, "", 0, "")

df_sahhs <- dplyr::rename(
  df_sahhs,
  "Characteristic" = "characteristic",
  "Subcharacteristic" = "subcharacteristic",
  "N [midpoint6_derived]" = "N_midpoint6_derived",
  "(%) [midpoint6_derived]" = "percent_midpoint6_derived",
  "COVID-19 diagnoses [midpoint6]" = "exposed_midpoint6"
)


# Save Table 1 -----------------------------------------------------------------
print("Save rounded Table 1")

write.csv(
  df_ami,
  paste0(table1_dir, "table1-cohort_", cohort, preex_string, "_ami-midpoint6_subsample.csv"),
  row.names = FALSE
)

write.csv(
  df_sahhs,
  paste0(table1_dir, "table1-cohort_", cohort, preex_string, "_sahhs-midpoint6_subsample.csv"),
  row.names = FALSE
)