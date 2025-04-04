from ehrql import (
    create_dataset,
    days,
    minimum_of,
    maximum_of,
)

# Bring table definitions from the TPP backend 

from ehrql.tables.tpp import ( 
    patients, 
)

# create dataset to create dates for different cohorts

dataset = create_dataset()

dataset.define_population(
    patients.date_of_birth.is_not_null()
)

dataset.configure_dummy_data(population_size=5000)

# Import study_dates dictionary

from variables_dates import study_dates

# Extract relevant dates from the study_dates dictionary

pandemic_start = study_dates["pandemic_start"]
delta_date = study_dates["delta_date"]
omicron_date = study_dates["omicron_date"]
all_eligible = study_dates["all_eligible"] 
lcd_date = study_dates["lcd_date"]

# Import preliminary date variables (death date, vax dates)

from variables_dates import prelim_date_variables

  ## Add the imported variables to the dataset

for var_name, var_value in prelim_date_variables.items():
    setattr(dataset, var_name, var_value)

# Import jcvi variables ( JCVI group and derived variables; eligible date for vaccination based on JCVI group)
from variables_dates import jcvi_variables

  ## Add the imported variables to the dataset
for var_name, var_value in jcvi_variables.items():
    setattr(dataset, var_name, var_value)

# Generate cohort dates

## Prevax

dataset.index_prevax = minimum_of(pandemic_start, pandemic_start)

dataset.end_prevax_exposure = minimum_of(
    dataset.cens_date_death, 
    dataset.vax_date_covid_1, 
    dataset.vax_date_eligible, 
    all_eligible
)

dataset.end_prevax_outcome = minimum_of(
    dataset.cens_date_death, 
    lcd_date
)

## Vax

dataset.index_vax = maximum_of(
    dataset.vax_date_covid_2 + days(14),
    delta_date
)

dataset.end_vax_exposure = minimum_of(
    dataset.cens_date_death, 
    omicron_date
)

dataset.end_vax_outcome = minimum_of(
    dataset.cens_date_death, 
    lcd_date
)

## Unvax

dataset.index_unvax = maximum_of(
    dataset.vax_date_eligible + days(84),
    delta_date
)

dataset.end_unvax_exposure = minimum_of(
    dataset.cens_date_death, 
    omicron_date, 
    dataset.vax_date_covid_1
)

dataset.end_unvax_outcome = minimum_of(
    dataset.cens_date_death, 
    lcd_date
)