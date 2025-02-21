library(jsonlite)

# Create output directory ----

fs::dir_create(here::here("lib"))

# Create empty data frame ----

df <- data.frame(cohort = character(),
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
                 stringsAsFactors = FALSE)

# Set constant values ----

ipw <- TRUE
age_spline <- TRUE
exposure <- "exp_date_covid"
strata <- "strat_cat_region"
covariate_sex <- "cov_cat_sex"
covariate_age <- "cov_num_age"
cox_start <- "index_date"
cox_stop <- "end_date_outcome"
controls_per_case <- 20L
total_event_threshold <- 50L
episode_event_threshold <- 5L
covariate_threshold <- 5L

# Define dates ----

study_dates <- fromJSON("output/study_dates.json")
prevax_start <- study_dates$pandemic_start
vax_unvax_start <- study_dates$delta_date
study_stop <- study_dates$lcd_date

# Define cut points ----

prevax_cuts <- "1;7;14;28;56;84;183;365;730;1065;1582"
vax_unvax_cuts <- "1;7;14;28;56;84;183;365;730;1065"

# Define covariates ----

## Core covariates (common across projects) ----

core_covars <- c("cov_cat_ethnicity", 
                 "cov_cat_imd", 
                 "cov_cat_smoking", 
                 "cov_bin_carehome", 
                 "cov_num_consrate2019", 
                 "cov_bin_hcworker", 
                 "cov_bin_dementia", 
                 "cov_bin_liver_disease",
                 "cov_bin_ckd", 
                 "cov_bin_cancer", 
                 "cov_bin_hypertension", 
                 "cov_bin_diabetes", 
                 "cov_bin_obesity", 
                 "cov_bin_copd",
                 "cov_bin_ami", 
                 #"cov_bin_stroke_isch",
                 "cov_bin_depression")

## Define project-specific covariates ----

project_covars <- c("cov_bin_stroke_all",
                    "cov_bin_other_ae",
                    "cov_bin_vte",
                    "cov_bin_hf",
                    "cov_bin_angina",
                    "cov_bin_lipidmed",
                    "cov_bin_antiplatelet",
                    "cov_bin_anticoagulant",
                    "cov_bin_cocp",
                    "cov_bin_hrt")

## Combine covariates into a single vector ----

all_covars <- paste0(c(core_covars, project_covars), collapse = ";")

# Specify cohorts ----

cohorts <- c("prevax")

# Specify outcomes ----

outcomes <- c("out_date_ami",
              "out_date_stroke_sahhs")

## For each cohort ----

for (c in cohorts) {
  
  ## For each outcome ----
  
  for (i in outcomes) {
      
      ## Define analyses ----
      
      ### analysis: main ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = all_covars,
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = "main")
      
  }
  
}

# Add name for each analysis ----

df$name <- paste0("cohort_",df$cohort, "-", 
                  df$analysis, "-", 
                  gsub("out_date_","",df$outcome))

# Check names are unique and save active analyses list ----

if (!dir.exists("lib")) {
  dir.create("lib")
}
if (length(unique(df$name))==nrow(df)) {
  saveRDS(df, file = "lib/active_analyses.rds", compress = "gzip")
} else {
  stop(paste0("ERROR: names must be unique in active analyses table"))
}