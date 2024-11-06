# Asthma dataset_definition example
# Imported from OpenSAFELY Documentation
# https://docs.opensafely.org/ehrql/tutorial/writing-a-dataset-definition/

from ehrql import create_dataset
from ehrql.tables.core import patients, medications

dataset = create_dataset()

dataset.define_population(patients.date_of_birth.is_on_or_before("1999-12-31"))

asthma_codes = ["39113311000001107", "39113611000001102"]
latest_asthma_med = (
    medications.where(medications.dmd_code.is_in(asthma_codes))
    .sort_by(medications.date)
    .last_for_patient()
)

dataset.asthma_med_date = latest_asthma_med.date
dataset.asthma_med_code = latest_asthma_med.dmd_code