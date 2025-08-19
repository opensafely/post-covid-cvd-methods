from ehrql import (
    days,
    case,
    when,
    minimum_of
)

# Bring table definitions from the TPP backend 
from ehrql.tables.tpp import ( 
    patients, 
    practice_registrations, 
    addresses, 
    appointments, 
    occupation_on_covid_vaccine_record,
    sgss_covid_all_tests,
    apcs, 
    clinical_events, 
    ons_deaths,
)

# Codelists from codelists.py (which pulls all variables from the codelist folder)
from codelists import *

# Call functions from variable_helper_functions
from variable_helper_functions import (
    ever_matching_event_clinical_ctv3_before,
    first_matching_event_clinical_ctv3_between,
    first_matching_event_clinical_snomed_between,
    first_matching_event_apc_between,
    matching_death_between,
    last_matching_event_clinical_ctv3_before,
    last_matching_event_clinical_snomed_before,
    last_matching_med_dmd_before,
    last_matching_event_apc_before,
    matching_death_before,
    filter_codes_by_category,
)

# Define generate variables function
def generate_variables(index_date, end_date_exp, end_date_out):  

    ## Inclusion/exclusion criteria------------------------------------------------------------------------

    ### Registered for a minimum of 6 months prior to index date
    inex_bin_6m_reg = (practice_registrations.spanning(
        index_date - days(180), index_date
        )).exists_for_patient()

    ### Alive on the index date
    inex_bin_alive = (((patients.date_of_death.is_null()) | (patients.date_of_death.is_after(index_date))) & 
    ((ons_deaths.date.is_null()) | (ons_deaths.date.is_after(index_date))))

    ## Censoring criteria----------------------------------------------------------------------------------

    ### Deregistered
    cens_date_dereg = (
        practice_registrations.where(practice_registrations.end_date.is_not_null())
        .where(practice_registrations.end_date.is_on_or_after(index_date))
        .sort_by(practice_registrations.end_date)
        .first_for_patient()
        .end_date
    )

    ## Exposures-------------------------------------------------------------------------------------------

    ### COVID-19
    tmp_exp_date_covid_sgss = (
        sgss_covid_all_tests.where(
            sgss_covid_all_tests.specimen_taken_date.is_on_or_between(index_date, end_date_exp)
        )
        .where(sgss_covid_all_tests.is_positive)
        .sort_by(sgss_covid_all_tests.specimen_taken_date)
        .first_for_patient()
        .specimen_taken_date
    )
    tmp_exp_date_covid_gp = (
        clinical_events.where(
            (clinical_events.ctv3_code.is_in(
                covid_primary_care_code + 
                covid_primary_care_positive_test +
                covid_primary_care_sequalae)) &
            clinical_events.date.is_on_or_between(index_date, end_date_exp)
        )
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )
    tmp_exp_date_covid_apc = (
        apcs.where(
            ((apcs.primary_diagnosis.is_in(covid_codes)) | 
             (apcs.secondary_diagnosis.is_in(covid_codes))) & 
            (apcs.admission_date.is_on_or_between(index_date, end_date_exp))
        )
        .sort_by(apcs.admission_date)
        .first_for_patient()
        .admission_date
    )
    tmp_exp_covid_death = matching_death_between(covid_codes, index_date, end_date_exp)
    tmp_exp_date_death = ons_deaths.date
    tmp_exp_date_covid_death = case(
        when(tmp_exp_covid_death).then(tmp_exp_date_death)
    )
    
    exp_date_covid = minimum_of(
        tmp_exp_date_covid_sgss, 
        tmp_exp_date_covid_gp,
        tmp_exp_date_covid_apc,
        tmp_exp_date_covid_death
    )

    ## Quality assurance-----------------------------------------------------------------------------------

    ### Prostate cancer
    qa_bin_prostate_cancer = (
        (last_matching_event_clinical_snomed_before(
            prostate_cancer_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            prostate_cancer_icd10, index_date
        ).exists_for_patient())
    )

    ### Pregnancy
    qa_bin_pregnancy = last_matching_event_clinical_snomed_before(
        pregnancy_snomed, index_date
    ).exists_for_patient()

    ### Year of birth
    qa_num_birth_year = patients.date_of_birth.year

    ## COCP or heart medication
    qa_bin_hrtcocp = last_matching_med_dmd_before(
        cocp_dmd + hrt_dmd, index_date
    ).exists_for_patient()

    ## Outcomes--------------------------------------------------------------------------------------------

     ### Acute myocardial infarction (AMI)
    tmp_out_date_ami_gp = (
        first_matching_event_clinical_snomed_between(
            ami_snomed, index_date, end_date_out
            ).date
    )
    tmp_out_date_ami_apc = (
        first_matching_event_apc_between(
            ami_icd10, index_date, end_date_out
            ).admission_date
    )
    tmp_out_date_ami_death = case(
        when(
            matching_death_between(ami_icd10, index_date, end_date_out)
            ).then(ons_deaths.date)
    )
    out_date_ami = minimum_of(
        tmp_out_date_ami_gp,
        tmp_out_date_ami_apc,
        tmp_out_date_ami_death
    )

    ### Subarachnoid haemorrhage and haemorrhagic stroke
    tmp_out_date_stroke_sahhs_gp = (
        first_matching_event_clinical_snomed_between(
            stroke_sahhs_snomed, index_date, end_date_out
            ).date
    )
    tmp_out_date_stroke_sahhs_apc = (
        first_matching_event_apc_between(
            stroke_sahhs_icd10, index_date, end_date_out
            ).admission_date
    )
    tmp_out_date_stroke_sahhs_death = case(
        when(
            matching_death_between(stroke_sahhs_icd10, index_date, end_date_out)
            ).then(ons_deaths.date)
    )
    out_date_stroke_sahhs = minimum_of(
        tmp_out_date_stroke_sahhs_gp,
        tmp_out_date_stroke_sahhs_apc,
        tmp_out_date_stroke_sahhs_death
    )

    ## Strata----------------------------------------------------------------------------------------------

    ### Region
    strat_cat_region = practice_registrations.for_patient_on(index_date).practice_nuts1_region_name

    ## Core covariates-------------------------------------------------------------------------------------

    ### Age
    cov_num_age = patients.age_on(index_date)

    ### Sex
    cov_cat_sex = patients.sex

    ### Ethnicity
    tmp_cov_cat_ethnicity = (
        clinical_events.where(clinical_events.snomedct_code.is_in(ethnicity_snomed))
        .where(clinical_events.date.is_on_or_before(index_date))
        .sort_by(clinical_events.date)
        .last_for_patient()
        .snomedct_code
    )

    cov_cat_ethnicity = tmp_cov_cat_ethnicity.to_category(
        ethnicity_snomed
    )

    ### Deprivation
    cov_cat_imd = case(
        when((addresses.for_patient_on(index_date).imd_rounded >= 0) & 
                (addresses.for_patient_on(index_date).imd_rounded < int(32844 * 1 / 5))).then("1 (most deprived)"),
        when(addresses.for_patient_on(index_date).imd_rounded < int(32844 * 2 / 5)).then("2"),
        when(addresses.for_patient_on(index_date).imd_rounded < int(32844 * 3 / 5)).then("3"),
        when(addresses.for_patient_on(index_date).imd_rounded < int(32844 * 4 / 5)).then("4"),
        when(addresses.for_patient_on(index_date).imd_rounded < int(32844 * 5 / 5)).then("5 (least deprived)"),
        otherwise="unknown",
    )

    ### Smoking status
    tmp_most_recent_smoking_cat = (
        last_matching_event_clinical_ctv3_before(smoking_clear, index_date)
        .ctv3_code.to_category(smoking_clear)
    )
    tmp_ever_smoked = ever_matching_event_clinical_ctv3_before(
        (filter_codes_by_category(smoking_clear, include=["S", "E"])), index_date
        ).exists_for_patient()

    cov_cat_smoking = case(
        when(tmp_most_recent_smoking_cat == "S").then("S"),
        when((tmp_most_recent_smoking_cat == "E") | ((tmp_most_recent_smoking_cat == "N") & (tmp_ever_smoked == True))).then("E"),
        when((tmp_most_recent_smoking_cat == "N") & (tmp_ever_smoked == False)).then("N"),
        otherwise="M"
    )

    ### Care home status
    cov_bin_carehome = (
        addresses.for_patient_on(index_date).care_home_is_potential_match |
        addresses.for_patient_on(index_date).care_home_requires_nursing |
        addresses.for_patient_on(index_date).care_home_does_not_require_nursing
    )

    ### Consultation rate in 2019
    tmp_cov_num_consrate2019 = appointments.where(
        appointments.status.is_in([
            "Arrived",
            "In Progress",
            "Finished",
            "Visit",
            "Waiting",
            "Patient Walked Out",
            ]) & appointments.start_date.is_on_or_between("2019-01-01", "2019-12-31")
            ).count_for_patient()    

    cov_num_consrate2019 = case(
        when(tmp_cov_num_consrate2019 <= 365).then(tmp_cov_num_consrate2019),
        otherwise=365,
    )

    ### Healthcare worker
    cov_bin_hcworker = occupation_on_covid_vaccine_record.where(
        (occupation_on_covid_vaccine_record.is_healthcare_worker == True)
    ).exists_for_patient()

    ### Dementia
    cov_bin_dementia = (
        (last_matching_event_clinical_snomed_before(
            dementia_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            dementia_icd10, index_date
        ).exists_for_patient())
    )

    ### Liver disease
    cov_bin_liver_disease = (
        (last_matching_event_clinical_snomed_before(
            liver_disease_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            liver_disease_icd10, index_date
        ).exists_for_patient())
    )

    ### Chronic kidney disease (CKD)
    cov_bin_ckd = (
        (last_matching_event_clinical_snomed_before(
            ckd_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            ckd_icd10, index_date
        ).exists_for_patient())
    )

    ### Cancer
    cov_bin_cancer = (
        (last_matching_event_clinical_snomed_before(
            cancer_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            cancer_icd10, index_date
        ).exists_for_patient())
    )

    ### Hypertension
    cov_bin_hypertension = (
        (last_matching_event_clinical_snomed_before(
            hypertension_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            hypertension_drugs_dmd, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            hypertension_icd10, index_date
        ).exists_for_patient())
    )

    ### Diabetes 
    cov_bin_diabetes = (
        (last_matching_event_clinical_snomed_before(
            diabetes_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_med_dmd_before(
            diabetes_drugs_dmd, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            diabetes_icd10, index_date
        ).exists_for_patient())
    )

    ### Obesity 
    cov_bin_obesity = (
        (last_matching_event_clinical_snomed_before(
            obesity_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            obesity_icd10, index_date
        ).exists_for_patient())
    )

    ### Chronic obstructive pulmonary disease (COPD)
    cov_bin_copd = (
        (last_matching_event_clinical_ctv3_before(
            copd_ctv3, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            copd_icd10, index_date
        ).exists_for_patient())
    )

    ### Acute myocardial infarction (AMI)
    cov_bin_ami = (
        (last_matching_event_clinical_snomed_before(
            ami_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            ami_icd10 + ami_prior_icd10, index_date
        ).exists_for_patient())
    )

    ### Depression
    cov_bin_depression = (
        (last_matching_event_clinical_snomed_before(
            depression_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            depression_icd10, index_date
        ).exists_for_patient())
    )

    # ### Ischaemic stroke
    # cov_bin_stroke_isch = (
    #     (last_matching_event_clinical_snomed_before(
    #         stroke_isch_snomed, index_date
    #     ).exists_for_patient()) |
    #     (last_matching_event_apc_before(
    #         stroke_isch_icd10, index_date
    #     ).exists_for_patient())
    # )

    ## Project specific covariates-------------------------------------------------------------------------

    ### All stroke ('all stroke' will replace the core covariate 'ischaemic stroke' for this project)
    cov_bin_stroke_all = (
        (last_matching_event_clinical_snomed_before(
            stroke_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            stroke_icd10, index_date
        ).exists_for_patient())
    )

    ### Other arterial embolism 
    cov_bin_other_ae = (
        (last_matching_event_clinical_snomed_before(
            other_ae_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            other_ae_icd10, index_date
        ).exists_for_patient())
    )

    ### Venous thromboembolism events 
    cov_bin_vte = (
        (last_matching_event_clinical_snomed_before(
            vte_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            vte_icd10, index_date
        ).exists_for_patient())
    )

    ### Heart failure 
    cov_bin_hf = (
        (last_matching_event_clinical_snomed_before(
            hf_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            hf_icd10, index_date
        ).exists_for_patient())
    )

    ### Angina 
    cov_bin_angina = (
        (last_matching_event_clinical_snomed_before(
            angina_snomed, index_date
        ).exists_for_patient()) |
        (last_matching_event_apc_before(
            angina_icd10, index_date
        ).exists_for_patient())
    )

    ### Lipid lowering medications
    cov_bin_lipidmed = last_matching_med_dmd_before(
        lipid_lowering_dmd, index_date
    ).exists_for_patient()

    ### Antiplatelet medications 
    cov_bin_antiplatelet = last_matching_med_dmd_before(
        antiplatelet_dmd, index_date
    ).exists_for_patient()

    ### Anticoagulation medications 
    cov_bin_anticoagulant = last_matching_med_dmd_before(
        anticoagulant_dmd, index_date
    ).exists_for_patient()

    ### Combined oral contraceptive pill
    cov_bin_cocp = last_matching_med_dmd_before(
        cocp_dmd, index_date
    ).exists_for_patient()

    ### Hormone replacement therapy
    cov_bin_hrt = last_matching_med_dmd_before(
        hrt_dmd, index_date
    ).exists_for_patient()

    ## Subgroups-------------------------------------------------------------------------------------------

    ### History of COVID-19
    tmp_sub_bin_covidhistory_sgss = (
        sgss_covid_all_tests.where(
            sgss_covid_all_tests.specimen_taken_date.is_before(index_date)
        )
        .where(sgss_covid_all_tests.is_positive)
        .exists_for_patient()
    )
    tmp_sub_bin_covidhistory_gp = (
        clinical_events.where(
            (clinical_events.ctv3_code.is_in(
                covid_primary_care_code + 
                covid_primary_care_positive_test + 
                covid_primary_care_sequalae)) &
            clinical_events.date.is_before(index_date)
        )
        .exists_for_patient()
    )
    tmp_sub_bin_covidhistory_apc = (
        apcs.where(
            ((apcs.primary_diagnosis.is_in(covid_codes)) | (apcs.secondary_diagnosis.is_in(covid_codes))) & 
            (apcs.admission_date.is_before(index_date))
        )
        .exists_for_patient()
    )

    sub_bin_covidhistory = (
        tmp_sub_bin_covidhistory_sgss |
        tmp_sub_bin_covidhistory_gp |
        tmp_sub_bin_covidhistory_apc
    )

    ### COVID-19 severity
    tmp_sub_date_covidhospital = (
        apcs.where(
            (apcs.primary_diagnosis.is_in(covid_codes)) & 
            (apcs.admission_date.is_on_or_after(exp_date_covid))
        )
        .sort_by(apcs.admission_date)
        .first_for_patient()
        .admission_date
    )

    sub_cat_covidhospital = case(
        when(
            (exp_date_covid.is_not_null()) &
            (tmp_sub_date_covidhospital.is_not_null()) &
            ((tmp_sub_date_covidhospital - exp_date_covid).days >= 0) &
            ((tmp_sub_date_covidhospital - exp_date_covid).days < 29)
            ).then("hospitalised"),
        when(exp_date_covid.is_not_null()).then("non_hospitalised"),
        when(exp_date_covid.is_null()).then("no_infection")
    )


    ## Define dictionary of variables to be written into dataset-------------------------------------------

    dynamic_variables = dict(
        ### Inclusion/exclusion criteria
        inex_bin_6m_reg = inex_bin_6m_reg,
        inex_bin_alive = inex_bin_alive,
        ### Censoring criteria
        cens_date_dereg = cens_date_dereg,
        ### Exposures
        exp_date_covid = exp_date_covid,
        ### Quality assurance
        qa_bin_prostate_cancer = qa_bin_prostate_cancer,
        qa_bin_pregnancy = qa_bin_pregnancy,
        qa_num_birth_year = qa_num_birth_year,
        qa_bin_hrtcocp = qa_bin_hrtcocp,
        ### Outcomes (including tmp_* for Venn diagrams)
        tmp_out_date_ami_gp = tmp_out_date_ami_gp,
        tmp_out_date_ami_apc = tmp_out_date_ami_apc,
        tmp_out_date_ami_death = tmp_out_date_ami_death,
        out_date_ami = out_date_ami,
        tmp_out_date_stroke_sahhs_gp = tmp_out_date_stroke_sahhs_gp,
        tmp_out_date_stroke_sahhs_apc = tmp_out_date_stroke_sahhs_apc,
        tmp_out_date_stroke_sahhs_death = tmp_out_date_stroke_sahhs_death,
        out_date_stroke_sahhs = out_date_stroke_sahhs,
        ### Strata
        strat_cat_region = strat_cat_region,
        ### Core covariates
        cov_num_age = cov_num_age,
        cov_cat_sex = cov_cat_sex,
        cov_cat_ethnicity = cov_cat_ethnicity,
        cov_cat_imd = cov_cat_imd,
        cov_cat_smoking = cov_cat_smoking,
        cov_bin_carehome = cov_bin_carehome,
        cov_num_consrate2019 = cov_num_consrate2019,
        cov_bin_hcworker = cov_bin_hcworker,
        cov_bin_dementia = cov_bin_dementia,
        cov_bin_liver_disease = cov_bin_liver_disease,
        cov_bin_ckd = cov_bin_ckd,
        cov_bin_cancer = cov_bin_cancer,
        cov_bin_hypertension = cov_bin_hypertension,
        cov_bin_diabetes = cov_bin_diabetes,
        cov_bin_obesity = cov_bin_obesity,
        cov_bin_copd = cov_bin_copd,
        cov_bin_ami = cov_bin_ami,
        #cov_bin_stroke_isch = cov_bin_stroke_isch,
        cov_bin_depression = cov_bin_depression,
        ### Project specific covariates
        cov_bin_stroke_all = cov_bin_stroke_all,
        cov_bin_other_ae = cov_bin_other_ae,
        cov_bin_vte = cov_bin_vte,
        cov_bin_hf = cov_bin_hf,
        cov_bin_angina = cov_bin_angina,
        cov_bin_lipidmed = cov_bin_lipidmed,
        cov_bin_antiplatelet = cov_bin_antiplatelet,
        cov_bin_anticoagulant = cov_bin_anticoagulant,
        cov_bin_cocp = cov_bin_cocp,
        cov_bin_hrt = cov_bin_hrt,
        ### Subgroups
        sub_bin_covidhistory = sub_bin_covidhistory,
        sub_cat_covidhospital = sub_cat_covidhospital
    )

    return dynamic_variables
