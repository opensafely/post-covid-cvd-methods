# Acute MI and Subarachnoid haemorrhage dataset_definition
# Emma Tarmey, Zoe M Zou, Venexia Walker
# Most recent update: 06/11/2024


from ehrql import create_dataset
from ehrql.tables.core import patients, medications

dataset = create_dataset()

# TBD: define by pre-vax cohort
dataset.define_population(patients.date_of_birth.is_on_or_before("1999-12-31"))

# TBD: REPLACE WITH CORRECT CODES
cvd_outcomes_codes = [
    "39113311000001107", # acute MI
    "39113611000001102"  # subarachnoid haemorrhage
] 

latest_cvd_outcomes_med = (
    medications.where(medications.dmd_code.is_in(cvd_outcomes_codes))
    .sort_by(medications.date)
    .last_for_patient()
)

dataset.cvd_outcomes_med_date = latest_cvd_outcomes_med.date
dataset.cvd_outcomes_med_code = latest_cvd_outcomes_med.dmd_code
