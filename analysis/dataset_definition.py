from ehrql import create_dataset
from ehrql.tables.tpp import patients, practice_registrations

dataset = create_dataset()

index_date = "2020-03-31"

# true / false
has_registration = practice_registrations.for_patient_on(
    index_date
).exists_for_patient()

dataset.define_population(has_registration)

# extract these columns
# attempting to find acute MI, SAH
dataset.sex = patients.sex
dataset.age = patients.age_on(index_date)



