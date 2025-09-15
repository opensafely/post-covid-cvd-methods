library(jsonlite)
library(dplyr)

# Create output directory ----
fs::dir_create(here::here("lib"))

# Source common functions ----
lapply(
  list.files("analysis/active_analyses/", full.names = TRUE, pattern = "fn-"),
  source
)

# Define cohorts ----
# Options are: "vax", "unvax", "prevax"
cohorts <- c("prevax")

# Define subgroups ----
subgroups <- c(
  "sub_covidhospital_TRUE",
  "sub_covidhospital_FALSE",
  "sub_covidhistory"
)

# Define preex groups ----
# Options are: "" (which means none), "_preex_TRUE", "_preex_FALSE"
preex_groups <- c("")

# Define general covariates ----
core_covariates <- c(
  "cov_cat_ethnicity",
  "cov_cat_imd",
#  "cov_num_consrate2019",
  "cov_bin_hcworker",
  "cov_cat_smoking",
  "cov_bin_carehome",
  "cov_bin_obesity",
  "cov_bin_ami",
  "cov_bin_dementia",
  "cov_bin_liver_disease",
  "cov_bin_ckd",
  "cov_bin_cancer",
  "cov_bin_hypertension",
  "cov_bin_diabetes",
  "cov_bin_depression",
  "cov_bin_copd",
  "cov_bin_stroke_isch"
)

project_covariates <- c(
  "cov_bin_stroke_all",
  "cov_bin_other_ae",
  "cov_bin_vte",
  "cov_bin_hf",
  "cov_bin_angina",
  "cov_bin_lipidmed",
  "cov_bin_antiplatelet",
  "cov_bin_anticoagulant",
  "cov_bin_cocp",
  "cov_bin_hrt"
)

# Define covariate and outcome combos ----

# For 'all' analyses
outcomes <- c("out_date_ami", "out_date_stroke_sahhs")
covariates <- setdiff(
  c(core_covariates, project_covariates),
  "cov_bin_stroke_isch"
)

# For preex=TRUE analyses
outcomes_preex_TRUE <- ""
covariates_preex_TRUE <- ""

# For preex=FALSE analyses
outcomes_preex_FALSE <- ""
covariates_preex_FALSE <- ""

# Create empty data frame ----
df <- data.frame(
  cohort = character(),
  exposure = character(),
  outcome = character(),
  ipw = logical(),
  strata = character(),
  covariate_sex = character(),
  covariate_age = character(),
  covariate_other = character(),
  cox_start = character(),
  cox_stop = character(),
  study_start = character(),
  study_stop = character(),
  cut_points = character(),
  controls_per_case = numeric(),
  total_event_threshold = numeric(),
  episode_event_threshold = numeric(),
  covariate_threshold = numeric(),
  age_spline = logical(),
  analysis = character(),
  stringsAsFactors = FALSE
)

# Generate analyses ----
for (i in preex_groups) {
  for (j in cohorts) {
    # Retrieve outcomes and covariates for preex group ----
    out <- get(paste0("outcomes", i))
    covars <- get(paste0("covariates", i))

    for (k in out) {
      # Collapse covariates ----

      covariate_other <- paste0(covars, collapse = ";")

      # Add main analysis ----
      df[nrow(df) + 1, ] <- add_analysis(
        cohort = j,
        outcome = k,
        analysis_name = paste0("main", i),
        covariate_other = covariate_other,
        age_spline = TRUE
      )

      # Add subgroup analyses ----
      for (sub in subgroups) {
        # Skip sub_covidhistory if cohort is "prevax"
        if (sub == "sub_covidhistory" && j == "prevax") {
          next
        }

        # Adjust covariate_other for ethnicity and smoking subgroups
        adjusted_covariate_other <- covariate_other
        if (grepl("sub_ethnicity", sub)) {
          adjusted_covariate_other <- paste0(
            setdiff(strsplit(covariate_other, ";")[[1]], "cov_cat_ethnicity"),
            collapse = ";"
          )
        } else if (grepl("sub_smoking", sub)) {
          adjusted_covariate_other <- paste0(
            setdiff(strsplit(covariate_other, ";")[[1]], "cov_cat_smoking"),
            collapse = ";"
          )
        }

        # Add analysis for the subgroup
        df[nrow(df) + 1, ] <- add_analysis(
          cohort = j,
          outcome = k,
          analysis_name = paste0(sub, i),
          covariate_other = adjusted_covariate_other,
          age_spline = ifelse(grepl("sub_age", sub), FALSE, TRUE)
        )
      }
    }
  }
}

# Add name for each analysis ----
df$name <- paste0(
  "cohort_",
  df$cohort,
  "-",
  df$analysis,
  "-",
  gsub("out_date_", "", df$outcome)
)

# Check names are unique and save active analyses list ----
if (length(unique(df$name)) == nrow(df)) {
  saveRDS(df, file = "lib/active_analyses.rds", compress = "gzip")
} else {
  stop("ERROR: names must be unique in active analyses table")
}
