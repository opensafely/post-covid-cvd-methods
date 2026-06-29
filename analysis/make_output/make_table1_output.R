# ------------------------------------------------------------------------------
#
# make_table1_output.R
#
# This file handles any other required post-processing of tables 1 and 2
# (patient characteristics and outcomes)
# for the replicated Covid-19 x Cardiovascular paper
# Original text: https://doi.org/10.1038/s41467-024-46497-0
#
# Arguments:
#  - output - string, spoecifies which table is being processed
#             (table1, table2, etc)
#  - cohort - string, specifies which study cohort is being processed
#             (prevax, vax, unvax)
#  - subgroup - string, specifies other subgroups by which the study population
#               can be divided (e.g. main, covid hospitalisation)
#
# Returns:
#  - The post-processed patient characteristics data table
#    (output/make_output/table1_output_midpoint6.csv)
#  - The post-processed outcomes table
#    (output/make_output/table2-sub_covidhospital_output_midpoint.csv)
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


# Define make_output folder ------------------------------------------
print("Creating output/make_output output folder")

makeout_dir <- "output/make_output/"
fs::dir_create(here::here(makeout_dir))


# Specify arguments ------------------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  output <- "table1" # the action to apply
  cohorts <- "prevax-preex_FALSE;vax-preex_FALSE;unvax-preex_FALSE;prevax-preex_TRUE;vax-preex_TRUE;unvax-preex_TRUE" # The iterative label
  subgroup <- ""
} else {
  output <- args[[1]]
  cohorts <- args[[2]]
  if (length(args) < 3) {
    subgroup <- "" # an optional subgroup label (e.g. preex_FALSE)
  } else {
    subgroup <- args[[3]]
  }
}


# Separate cohorts -------------------------------------------------------------
print('Separate cohorts')

cohorts <- stringr::str_split(as.vector(cohorts), ";")[[1]]


# Generate output/saving string ------------------------------------------------
print('Generate strings')

if (subgroup == "All" | subgroup == "") {
  sub_str <- ""
} else {
  if (grepl("preex", subgroup)) {
    sub_str <- paste0("-", subgroup)
  } else {
    sub_str <- paste0("-sub_", subgroup)
  }
}


# Create blank table -----------------------------------------------------------
print('Create blank table')

df_ami   <- NULL
df_sahhs <- NULL


# Add output from each cohort --------------------------------------------------
print('Add output from each cohort')

for (i in cohorts) {
  # load input
  tmp_ami <- readr::read_csv(paste0(
    "output/",
    output,
    "/",
    output,
    "-cohort_",
    i,
    sub_str,
    "_ami-midpoint6.csv"
  ))

  # create column for cohort
  tmp_ami$cohort <- i

  # combining dataframes
  df_ami <- rbind(df_ami, tmp_ami, fill = TRUE)
}

df_ami <- df_ami[df_ami["cohort"] != TRUE, ]

for (i in cohorts) {
  # load input
  tmp_sahhs <- readr::read_csv(paste0(
    "output/",
    output,
    "/",
    output,
    "-cohort_",
    i,
    sub_str,
    "_sahhs-midpoint6.csv"
  ))

  # create column for cohort
  tmp_sahhs$cohort <- i

  # combining dataframes
  df_sahhs <- rbind(df_sahhs, tmp_sahhs, fill = TRUE)
}

df_sahhs <- df_sahhs[df_sahhs["cohort"] != TRUE, ]


# table1-specific processing ---------------------------------------------------
if (output == "table1") {
  print("table1 processing (ami)")

  df_ami <- pivot_wider(
    df_ami,
    names_from = "cohort",
    values_from = c(
      "N [midpoint6_derived]",
      "(%) [midpoint6_derived]",
      "COVID-19 diagnoses [midpoint6]",
      "percent_exposed_midpoint6"
    ),
    names_vary = "slowest"
  )

  # ensure same decimal places for n_percent_midpoint6 column
  n_percent_midpoint6 <- unname(unlist(df_ami[4]))
  for (i in seq_along(n_percent_midpoint6)) {
    if ((!is.na(n_percent_midpoint6[i])) && (!grepl(".", n_percent_midpoint6[i], fixed = TRUE))) {
      n_percent_midpoint6[i] <- paste0(str_sub(n_percent_midpoint6[i], end=-2), ".0%")
    }
  }
  df_ami[4] <- n_percent_midpoint6

  # ensure same decimal places for percent_exposed_midpoint6_prevax column
  percent_exposed_midpoint6_prevax <- unname(unlist(df_ami[6]))
  for (i in seq_along(percent_exposed_midpoint6_prevax)) {
    if ((!is.na(percent_exposed_midpoint6_prevax[i])) && (!grepl(".", percent_exposed_midpoint6_prevax[i], fixed = TRUE))) {
      percent_exposed_midpoint6_prevax[i] <- paste0(str_sub(percent_exposed_midpoint6_prevax[i], end=-2), ".0%")
    }
  }
  df_ami[6] <- percent_exposed_midpoint6_prevax
}

if (output == "table1") {
  print("table1 processing (sahhs)")
  
  df_sahhs <- pivot_wider(
    df_sahhs,
    names_from = "cohort",
    values_from = c(
      "N [midpoint6_derived]",
      "(%) [midpoint6_derived]",
      "COVID-19 diagnoses [midpoint6]",
      "percent_exposed_midpoint6"
    ),
    names_vary = "slowest"
  )

  # ensure same decimal places for n_percent_midpoint6 column
  n_percent_midpoint6 <- unname(unlist(df_sahhs[4]))
  for (i in seq_along(n_percent_midpoint6)) {
    if ((!is.na(n_percent_midpoint6[i])) && (!grepl(".", n_percent_midpoint6[i], fixed = TRUE))) {
      n_percent_midpoint6[i] <- paste0(str_sub(n_percent_midpoint6[i], end=-2), ".0%")
    }
  }
  df_sahhs[4] <- n_percent_midpoint6

  # ensure same decimal places for percent_exposed_midpoint6_prevax column
  percent_exposed_midpoint6_prevax <- unname(unlist(df_sahhs[6]))
  for (i in seq_along(percent_exposed_midpoint6_prevax)) {
    if ((!is.na(percent_exposed_midpoint6_prevax[i])) && (!grepl(".", percent_exposed_midpoint6_prevax[i], fixed = TRUE))) {
      percent_exposed_midpoint6_prevax[i] <- paste0(str_sub(percent_exposed_midpoint6_prevax[i], end=-2), ".0%")
    }
  }
  df_sahhs[6] <- percent_exposed_midpoint6_prevax
}


# Save output ------------------------------------------------------------------
print('Save output')

readr::write_csv(
  df_ami,
  paste0(makeout_dir, output, sub_str, "_output_ami_midpoint6.csv"),
  na = "-"
)

readr::write_csv(
  df_sahhs,
  paste0(makeout_dir, output, sub_str, "_output_sahhs_midpoint6.csv"),
  na = "-"
)
