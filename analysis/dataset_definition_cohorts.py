# ZoeMZou code
# Imported from opensafely/post-covid-respiratory
# https://github.com/opensafely/post-covid-respiratory


from ehrql import (
    create_dataset,
)
# Bring table definitions from the TPP backend 
from ehrql.tables.tpp import ( 
    patients, 
)

from ehrql.query_language import table_from_file, PatientFrame, Series

from datetime import date

# Create dataset

def generate_dataset(index_date, end_date_exp, end_date_out):
    dataset = create_dataset()
    
    dataset.define_population(
        patients.date_of_birth.is_not_null()
    )

# Configure dummy data

    dataset.configure_dummy_data(population_size=1000)

# Import variables function

    from variables_cohorts import generate_variables

    variables = generate_variables(index_date, end_date_exp, end_date_out)

    # Assign each variable to the dataset

    for var_name, var_value in variables.items():
        setattr(dataset, var_name, var_value)
    
    # Extract index dates for cohorts from index_dates.csv

    @table_from_file("output/index_dates.csv.gz")
    
    class index_dates(PatientFrame):
        index_prevax = Series(date)
        end_prevax_exposure = Series(date)
        end_prevax_outcome = Series(date)
        index_vax = Series(date)
        end_vax_exposure = Series(date)
        end_vax_outcome = Series(date)
        index_unvax = Series(date)
        end_unvax_exposure = Series(date)
        end_unvax_outcome = Series(date)

    dataset.index_prevax = index_dates.index_prevax
    dataset.end_prevax_exposure = index_dates.end_prevax_exposure
    dataset.end_prevax_outcome = index_dates.end_prevax_outcome

    dataset.index_unvax = index_dates.index_unvax
    dataset.end_unvax_exposure = index_dates.end_unvax_exposure
    dataset.end_unvax_outcome = index_dates.end_unvax_outcome

    dataset.index_vax = index_dates.index_vax
    dataset.end_vax_exposure = index_dates.end_vax_exposure
    dataset.end_vax_outcome = index_dates.end_vax_outcome

    return dataset

