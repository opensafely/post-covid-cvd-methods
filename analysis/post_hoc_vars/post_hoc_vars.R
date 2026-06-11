# ------------------------------------------------------------------------------
#
# post_hoc_vars.R
#
# This file generates any commonly used variables defined after the dataset
# defintion runs, such as indicator variables.
#
# Authors: Emma Tarmey, Venexia Walker, UoB ehrQL Team
#
# ------------------------------------------------------------------------------


# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cohort  <- "prevax"
} else {
  cohort  <- args[[1]]
}


# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_rds(paste0(
  "output/dataset_clean/input_",
  cohort,
  "_clean_prehoc.rds"
))


# Define variables -------------------------------------------------------------
print("Define variables")

df$cov_bin_covid <- !is.na(df$exp_date_covid)
df$cov_bin_sahhs <- !is.na(df$out_date_stroke_sahhs)


# Check all covariates types ---------------------------------------------------
print("Check all covariate types")

stop("TODO: CONTINUE FROM HERE DECIDING FACTOR VARS FOR MAKE MODEL INPUT")

df$cov_bin_ami   <- as.logical(df$cov_bin_ami)   # outcome
df$cov_bin_sahhs <- as.logical(df$cov_bin_sahhs) # outcome
df$cov_bin_covid <- as.logical(df$cov_bin_covid) # exposure

df$cov_num_age       <- as.numeric(df$cov_num_age)
df$cov_cat_sex       <- as.factor(df$cov_cat_sex)
df$cov_cat_ethnicity <- as.factor(df$cov_cat_ethnicity)
df$cov_cat_imd       <- as.factor(df$cov_cat_imd)
df$cov_cat_smoking   <- as.factor(df$cov_cat_smoking)

df$cov_bin_carehome      <- as.factor(df$cov_bin_carehome)
df$cov_bin_hcworker      <- as.factor(df$cov_bin_hcworker)
df$cov_bin_dementia      <- as.logical(df$cov_bin_dementia)
df$cov_bin_liver_disease <- as.logical(df$cov_bin_liver_disease)
df$cov_bin_ckd           <- as.logical(df$cov_bin_ckd)

df$cov_bin_cancer       <- as.logical(df$cov_bin_cancer)
df$cov_bin_hypertension <- as.logical(df$cov_bin_hypertension)
df$cov_bin_diabetes     <- as.logical(df$cov_bin_diabetes)
df$cov_bin_obesity      <- as.logical(df$cov_bin_obesity)
df$cov_bin_copd         <- as.logical(df$cov_bin_copd)

df$cov_bin_depression <- as.logical(df$cov_bin_depression)
df$cov_bin_stroke_all <- as.logical(df$cov_bin_stroke_all)
df$cov_bin_other_ae   <- as.logical(df$cov_bin_other_ae)
df$cov_bin_vte        <- as.logical(df$cov_bin_vte)
df$cov_bin_hf         <- as.logical(df$cov_bin_hf)

df$cov_bin_angina        <- as.logical(df$cov_bin_angina)
df$cov_bin_lipidmed      <- as.logical(df$cov_bin_lipidmed)
df$cov_bin_antiplatelet  <- as.logical(df$cov_bin_antiplatelet)
df$cov_bin_anticoagulant <- as.logical(df$cov_bin_anticoagulant)
df$cov_bin_cocp          <- as.logical(df$cov_bin_cocp)

df$cov_bin_hrt      <- as.logical(df$cov_bin_hrt)
df$strat_cat_region <- as.factor(df$strat_cat_region)


# Save results -----------------------------------------------------------------
print("Save results")

saveRDS(
  df,
  file = paste0("output/dataset_clean/input_", cohort, "_clean.rds"),
  compress = TRUE
)
