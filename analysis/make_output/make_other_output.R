# make_other_output.R - handles any other required post-processing of any table
# NB: copied tastefully from post-covid-respiratory, credit to ehrQL team


# Load packages ----------------------------------------------------------------
print('Load packages')

library(magrittr)
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)

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

df <- NULL

# Add output from each cohort --------------------------------------------------
print('Add output from each cohort')

for (i in cohorts) {
  # load input
  tmp <- readr::read_csv(paste0(
    "output/",
    output,
    "/",
    output,
    "-cohort_",
    i,
    sub_str,
    "-midpoint6.csv"
  ))

  # create column for cohort
  tmp$cohort <- i

  # combining dataframes
  df <- rbind(df, tmp, fill = TRUE)
}

df <- df[df["cohort"] != TRUE, ]

# table1-specific processing ---------------------------------------------------
if (output == "table1") {
  print("table1 processing")
  df <- pivot_wider(
    df,
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
  n_percent_midpoint6_fixed <- unname(unlist(df[4]))
  for (i in seq_along(n_percent_midpoint6_fixed)) {
    if ((!is.na(n_percent_midpoint6_fixed[i])) && (!grepl(".", n_percent_midpoint6_fixed[i], fixed = TRUE))) {
      n_percent_midpoint6_fixed[i] <- paste0(str_sub(n_percent_midpoint6_fixed[i], end=-2), ".0%")
    }
  }
  n_percent_midpoint6_fixed[is.na(n_percent_midpoint6_fixed)] <- ""

  # ensure same decimal places for percent_exposed_midpoint6_prevax column
  percent_exposed_midpoint6_prevax_fixed <- unname(unlist(df[6]))
  for (i in seq_along(percent_exposed_midpoint6_prevax_fixed)) {
    if ((!is.na(percent_exposed_midpoint6_prevax_fixed[i])) && (!grepl(".", percent_exposed_midpoint6_prevax_fixed[i], fixed = TRUE))) {
      percent_exposed_midpoint6_prevax_fixed[i] <- paste0(str_sub(percent_exposed_midpoint6_prevax_fixed[i], end=-2), ".0%")
    }
  }
  percent_exposed_midpoint6_prevax_fixed[is.na(percent_exposed_midpoint6_prevax_fixed)] <- ""

  df <- df %>% mutate(`N [midpoint6_derived]_prevax`   = n_percent_midpoint6_fixed)
  df <- df %>% mutate(percent_exposed_midpoint6_prevax = percent_exposed_midpoint6_prevax_fixed)
}

# Save output ------------------------------------------------------------------
print('Save output')

readr::write_csv(
  df,
  paste0(makeout_dir, "/", output, sub_str, "_output_midpoint6.csv"),
  na = "-"
)
