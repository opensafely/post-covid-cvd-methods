# Structure ---------------------------------------------------------------------

""" 

ALL PROJECTS (USUALLY NO EDITS REQUIRED):
- Setup
- Exposures
- Quality assurance
- JCVI groups
- Strata
- Core covariates

PROJECT SPECIFIC (PLEASE EDIT FOR YOUR PROJECT):
- Outcomes
- Project specific covariates

"""

# Setup ------------------------------------------------------------------------

from ehrql import codelist_from_csv

# Exposures --------------------------------------------------------------------

## COVID-19
covid_codes = codelist_from_csv(
    "codelists/user-RochelleKnight-confirmed-hospitalised-covid-19.csv",
    column="code"
)
covid_primary_care_positive_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
    column="CTV3ID"
)
covid_primary_care_code = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
    column="CTV3ID"
)
covid_primary_care_sequalae = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
    column="CTV3ID"
)

# Quality assurance ------------------------------------------------------------

## Prostate cancer
prostate_cancer_snomed = codelist_from_csv(
    "codelists/user-RochelleKnight-prostate_cancer_snomed.csv",
    column="code"
)
prostate_cancer_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-prostate_cancer_icd10.csv",
    column="code"
)

## Pregnancy
pregnancy_snomed = codelist_from_csv(
    "codelists/user-RochelleKnight-pregnancy_and_birth_snomed.csv",
    column="code"
)

## Combined oral contraceptive pill
cocp_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-cocp_dmd.csv",
    column="dmd_id"
)

## Hormone replacement therapy
hrt_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-hrt_dmd.csv",
    column="dmd_id"
)

# JCVI groups ------------------------------------------------------------------

## Wider learning disability
learndis_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-learndis.csv",
    column="code"
)

## Patients in long-stay nursing and residential care
longres_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-longres.csv",
    column="code"
)

## High risk from COVID-19 code
shield_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-shield.csv",
    column="code"
)

## Lower risk from COVID-19
nonshield_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-nonshield.csv",
    column="code"
)

## Pregnancy
preg_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-preg.csv",
    column="code"
)

## Pregnancy or delivery
pregdel_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-pregdel.csv",
    column="code"
)

## All BMI coded terms
bmi_stage_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-bmi_stage.csv",
    column="code"
)

## Severe obesity code recorded
sev_obesity_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-sev_obesity.csv",
    column="code"
)

## Asthma diagnosis code
ast_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-ast.csv",
    column="code"
)

## Asthma admission
astadm_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-astadm.csv",
    column="code"
)

## Asthma systemic steroid prescription
astrx_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-astrx.csv",
    column="code"
)

## Chronic Respiratory Disease
resp_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-resp_cov.csv",
    column="code"
)

## Chronic neurological disease including significantlearning disorder
cns_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-cns_cov.csv",
    column="code"
)

## Asplenia or dysfunction of the spleen
spln_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-spln_cov.csv",
    column="code"
)

## Diabetes diagnosis
diab_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-diab.csv",
    column="code"
)

## Diabetes resolved
dmres_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-dmres.csv",
    column="code"
)

## Severe mental illness
sev_mental_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-sev_mental.csv",
    column="code"
)

## Remission relating to severe mental illness
smhres_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-smhres.csv",
    column="code"
)

## Chronic heart disease
chd_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-chd_cov.csv",
    column="code"
)

## Chronic kidney disease diagnostic
ckd_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-ckd_cov.csv",
    column="code"
)

## Chronic kidney disease - all stages
ckd15_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-ckd15.csv",
    column="code"
)

## Chronic kidney disease-stages 3 - 5
ckd35_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-ckd35.csv",
    column="code"
)

## Chronic liver disease
cld_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-cld.csv",
    column="code"
)

## Immunosuppression diagnosis
immdx_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-immdx_cov.csv",
    column="code"
)

## Immunosuppression medication
immrx_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-immrx.csv",
    column="code"
)

# Household contact of shielding individual
hhld_imdef_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-hhld_imdef.csv",
    column="code",
)

# Strata -----------------------------------------------------------------------

### Region 
#### No codelist required

# Core covariates --------------------------------------------------------------

### Age 
#### No codelist required

### Sex
#### No codelist required

### Ethnicity  
ethnicity_snomed = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    column="code",
    category_column="Grouping_6"
)

### Deprivation 
#### No codelist required

### Smoking status 
smoking_clear = codelist_from_csv(
    "codelists/opensafely-smoking-clear.csv",
    column="CTV3Code",
    category_column="Category"
)

### Care home status 
#### No codelist required

### Consultation rate 
#### No codelist required

### Health care worker 
#### No codelist required

### Dementia 
dementia_nonvas_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_snomed.csv",
    column="code"
)
dementia_vas_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_vascular_snomed.csv",
    column="code"
)
dementia_nonvas_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_icd10.csv",
    column="code"
)
dementia_vas_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dementia_vascular_icd10.csv",
    column="code"
)
dementia_snomed = dementia_nonvas_snomed + dementia_vas_snomed
dementia_icd10 = dementia_nonvas_icd10 + dementia_vas_icd10

### Liver disease 
liver_disease_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-liver_disease_snomed.csv",
    column="code"
)
liver_disease_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-liver_disease_icd10.csv",
    column="code"
)

### Chronic kidney disease 
ckd_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-ckd_snomed.csv",
    column="code"
)
ckd_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ckd_icd10.csv",
    column="code"
)

### Cancer 
cancer_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-cancer_snomed.csv",
    column="code"
)
cancer_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-cancer_icd10.csv",
    column="code"
)

### Hypertension 
hypertension_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv",
    column="code"
)
hypertension_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-hypertension_icd10.csv",
    column="code"
)
hypertension_drugs_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-hypertension_drugs_dmd.csv",
    column="dmd_id"
)

### Diabetes 
diabetes_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_icd10.csv",
    column="code"
)
diabetes_drugs_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_drugs_dmd.csv",
    column="dmd_id"
)
diabetes_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-diabetes_snomed.csv",
    column="code"
)   

### Obesity 
obesity_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-bmi_obesity_snomed.csv",
    column="code"
)
obesity_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-bmi_obesity_icd10.csv",
    column="code"
)
bmi_primis = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-bmi.csv",
    column="code"
)

### Chronic obstructive pulmonary disease (COPD) 
copd_ctv3 = codelist_from_csv(
    "codelists/opensafely-current-copd.csv",
    column="CTV3ID"
)
copd_icd10 = codelist_from_csv(
    "codelists/opensafely-copd-secondary-care.csv",
    column="code"
)

### Acute myocardial infarction 
ami_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-ami_snomed.csv",
    column="code",
)
ami_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-ami_icd10.csv",
    column="code",
)
ami_prior_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-ami_prior_icd10.csv",
    column="code"
)

### Ischaemic stroke 
stroke_isch_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-stroke_isch_snomed.csv",
    column="code",
)
stroke_isch_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-stroke_isch_icd10.csv",  
    column="code",
)

### Depression
depression_snomed = codelist_from_csv(
    "codelists/user-hjforbes-depression-symptoms-and-diagnoses.csv",
    column="code"
)
depression_icd10 = codelist_from_csv(
    "codelists/user-kurttaylor-depression_icd10.csv",
    column="code",
)

# Outcomes ---------------------------------------------------------------------

## Acute myocardial infarction
#### ami_snomed defined earlier for this project - see 'Core covariates'
#### ami_icd10 defined earlier for this project - see 'Core covariates'

## Subarachnoid haemorrhage and haemorrhagic stroke
stroke_sahhs_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-stroke_sah_hs_snomed.csv",
    column="code",
)
stroke_sahhs_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-stroke_sah_hs_icd10.csv",
    column="code",
)

# Project specific covariates --------------------------------------------------

### All stroke ('all stroke' will replace the core covariate 'ischaemic stroke' for this project)
stroke_snomed = stroke_isch_snomed + stroke_sahhs_snomed
stroke_icd10 = stroke_isch_icd10 + stroke_sahhs_icd10

### Other arterial embolism 
other_ae_snomed = codelist_from_csv(
    "codelists/user-tomsrenin-other_art_embol.csv",
    column="code",
)
other_ae_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-other_arterial_embolism_icd10.csv",
    column="code",
)

### Venous thromboembolism events (VTE)
#### Deep vein thrombosis (DVT) [includes during pregnancy]
dvt_nonpreg_snomed = codelist_from_csv(
    "codelists/user-tomsrenin-dvt_main.csv",    
    column="code",
)
dvt_preg_snomed = codelist_from_csv(
    "codelists/user-tomsrenin-dvt-preg.csv",   
    column="code",
)
dvt_snomed = dvt_nonpreg_snomed + dvt_preg_snomed
dvt_nonpreg_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-dvt_dvt_icd10.csv",   
    column="code",
)
dvt_preg_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dvt_pregnancy_icd10.csv",   
    column="code",
)
dvt_icd10 = dvt_nonpreg_icd10 + dvt_preg_icd10
#### Intracranial venous thrombosis (ICVT) [includes during pregnancy]
icvt_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-dvt_icvt_snomed.csv",    
    column="code",
)
icvt_nonpreg_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-dvt_icvt_icd10.csv",   
    column="code",
)
icvt_preg_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-icvt_pregnancy_icd10.csv",  
    column="code",
)
icvt_icd10 = icvt_nonpreg_icd10 + icvt_preg_icd10
#### Other deep vein thrombosis
other_dvt_snomed = codelist_from_csv(
    "codelists/user-tomsrenin-dvt-other.csv",   
    column="code",
)
other_dvt_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-other_dvt_icd10.csv",    
    column="code",
)
#### Pulmonary embolism (PE)
pe_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-pe_snomed.csv",    
    column="code",
)
pe_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-pe_icd10.csv",    
    column="code",
)
#### Portal vein thrombosis (PVT)
pvt_snomed = codelist_from_csv(
    "codelists/user-tomsrenin-pvt.csv",   
    column="code",
)
pvt_icd10 = codelist_from_csv(
    "codelists/user-elsie_horne-portal_vein_thrombosis_icd10.csv",  
    column="code",
)
#### Venous thrombotic event (VTE)
vte_snomed = dvt_snomed + icvt_snomed + other_dvt_snomed + pe_snomed + pvt_snomed
vte_icd10 = dvt_icd10 + icvt_icd10 + other_dvt_icd10 + pe_icd10 + pvt_icd10

### Heart failure 
hf_snomed = codelist_from_csv(
    "codelists/user-elsie_horne-hf_snomed.csv",   
    column="code",
)
hf_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-hf_icd10.csv",  
    column="code",
)

### Angina 
angina_snomed = codelist_from_csv(
    "codelists/user-hjforbes-angina_snomed.csv",  
    column="code",
)
angina_icd10 = codelist_from_csv(
    "codelists/user-RochelleKnight-angina_icd10.csv",   
    column="code",
)

### Lipid lowering medications 
lipid_lowering_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-lipid_lowering_dmd.csv",
    column="dmd_id",
)

### Antiplatelet medications 
antiplatelet_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-antiplatelet_dmd.csv",
    column="dmd_id",
)

### Anticoagulation medications 
anticoagulant_dmd = codelist_from_csv(
    "codelists/user-elsie_horne-anticoagulant_dmd.csv",
    column="dmd_id",
)

### Combined oral contraceptive pill 
#### cocp_dmd defined earlier for this project - see 'Quality assurance'

### Hormone replacement therapy 
#### hrt_dmd defined earlier for this project - see 'Quality assurance'