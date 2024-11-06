# ZoeMZou code
# Imported from opensafely/post-covid-respiratory
# https://github.com/opensafely/post-covid-respiratory


from dataset_definition_cohorts import generate_dataset
from ehrql.query_language import table_from_file, PatientFrame, Series
from datetime import date

# extract index dates for prevax cohort from index_dates.csv
@table_from_file("output/index_dates.csv.gz")

class index_dates(PatientFrame):
    index_prevax = Series(date)
    end_prevax_exposure = Series(date)
    end_prevax_outcome = Series(date)

index_date = index_dates.index_prevax
end_date_exp = index_dates.end_prevax_exposure
end_date_out = index_dates.end_prevax_outcome

# Create dataset
dataset = generate_dataset(index_date, end_date_exp, end_date_out)
