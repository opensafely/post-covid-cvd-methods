# Load packages ----------------------------------------------------------------
print("Load packages")

library(magrittr)
library(data.table)
library(glue)
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

# Define model output folder ---------------------------------------------------
print("Creating output/model output folder")

# setting up the sub directory
model_dir <- "output/"

# check if sub directory exists, create if not
fs::dir_create(here::here(model_dir))


# Create JSON ------------------------------------------------------------------

# extracting prior covariate selection
lasso_vars_selected <- read.csv(paste0(
  "output/lasso_var_selection/lasso_var_selection-",
  name,
  ".csv"
))$x

# remove exposure to covid and convert to string with ; delimiter
print(lasso_vars_selected)
lasso_vars_selected        <- setdiff(lasso_vars_selected, "exposed")
lasso_vars_selected_string <- paste(lasso_vars_selected, collapse = ";")
print(lasso_vars_selected_string)

# creating the list
list1 <- vector(mode = "list", length = 2)
list1[[1]] <- c("covariate_other")
list1[[2]] <- c(lasso_vars_selected_string) # TODO: PULL FROM FILE!

# creating the data for JSON file
jsonData <- jsonlite::toJSON(list1)

# writing into JSON file
# glue("lasso_model_config-{name}.json")
write(jsonData, paste0(model_dir, glue("lasso_model_config-{name}.json")))

config <- jsonlite::fromJSON("output/lasso_model_config-cohort_prevax-main-ami.json")
print(config)
