
-   Detailed protocols are in the [`protocol`](./protocol/) folder.

-   If you are interested in how we defined our code lists, look in the [`codelists`](./codelists) folder.

-   Analyses scripts are in the [`analysis`](./analysis) directory:

    -   Dataset definition scripts are in the [`dataset_definition`](./analysis/dataset_definition/) directory:

        -   If you are interested in how we defined our variables, we use the variable script [`variable_helper_fuctions`](analysis/dataset_definition/variable_helper_functions.py) to define functions that generate variables. We then apply these functions in [`variables_cohorts`](analysis/variables_cohorts.py) to create a dictionary of variables for cohort definitions, and in [`variables_dates`](analysis/dataset_definition/variables_dates.py) to create a dictionary of variables for calculating study start dates and end dates.
        -   If you are interested in how we defined study dates (e.g., index and end dates), these vary by cohort and are described in the protocol. We use the script [`dataset_definition_dates`](analysis/dataset_definition/dataset_definition_dates.py) to generate a dataset with all required dates for each cohort. This script imported all variables generated from [`variables_dates`](analysis/dataset_definition/variables_dates.py).
        -   If you are interested in how we defined our cohorts, we use the dataset definition script [`dataset_definition_cohorts`](analysis/dataset_definition/dataset_definition_cohorts.py) to define a function that generates cohorts. This script imports all variables generated from [`variables_cohorts`](analysis/dataset_definition/variables_cohorts.py) using the patient's index date, the cohort start date and the cohort end date. This approach is used to generate three cohorts: pre-vaccination, vaccinated, and unvaccinated—found in [`dataset_definition_prevax`](analysis/dataset_definition/dataset_definition_prevax.py), [`dataset_definition_vax`](analysis/dataset_definition/dataset_definition_vax.py), and [`dataset_definition_unvax`](analysis/dataset_definition/dataset_definition_unvax.py), respectively. For each cohort, the extracted data is initially processed in the preprocess data script [`preprocess data script`](analysis/preprocess/preprocess_data.R), which generates a flag variable for pre-existing respiratory conditions and restricts the data to relevant variables.

    -   Dataset cleaning scripts are in the [`dataset_clean`](./analysis/dataset_clean/) directory:
        -   This directory also contains all the R scripts that process, describe, and analyse the extracted data.
        -   [`dataset_clean`](analysis/dataset_clean/dataset_clean.R) is the core script which executes all the other scripts in this folder
        -   [`fn-preprocess`](analysis/dataset_clean/fn-preprocess.R) is the function carrying out initial preprocessing, formatting columns correctly
        -   [`fn-modify_dummy`](analysis/dataset_clean/fn-modify_dummy.R) is called from within fn-preprocess.R, and alters the proportions of dummy variables to better suit analyses
        -   [`fn-inex`](analysis/dataset_clean/fn-inex.R) is the inclusion/exclusion function
        -   [`fn-qa`](analysis/dataset_clean/fn-qa.R) is the quality assurance function
        -   [`fn-ref`](analysis/dataset_clean/fn-ref.R) is the function that sets the reference levels for factors 
    

    -   Modelling scripts are in the [`model`](./analysis/model/) directory:
        -   [`make_model_input.R`](analysis/model/make_model_input.R) works with the output of [`dataset_clean`](./analysis/dataset_clean/) to prepare suitable data subsets for Cox analysis. Combines each outcome and subgroup in one formatted .rds file.
        -   [`fn-prepare_model_input.R`](analysis/model/fn-prepare_model_input.R) is a companion function to `make_model_input.R` which handles the interaction with `active_analyses.rds`.
        -   [`cox-ipw`](https://github.com/opensafely-actions/cox-ipw/) is a reusable action which uses the output of `make_model_input.R` to fit a Cox model to the data.
        -   [`make_model_output.R`](analysis/model/make_model_output.R) combines all the Cox results in one formatted .csv file.

-   The [`active_analyses`](lib/active_analyses.rds) contains a list of active analyses.

-   The [`project.yaml`](./project.yaml) defines run-order and dependencies for all the analysis scripts. This file should not be edited directly. To make changes to the yaml, edit and run the [`create_project_actions.R`](analysis/create_project_actions.R) script which generates all the actions.

-   Descriptive and Model outputs, including figures and tables are in the [`released_outputs`](./release_outputs) directory.
  
## Output

Outputs follow OpenSAFELY naming conventions related to suppression rules by adding the suffix "_midpoint6". The suffix "_midpoint6_derived" means that the value(s) are derived from the midpoint6 values. Detailed information regarding naming conventions can be found [here](https://docs.opensafely.org/releasing-files/#naming-convention-for-midpoint-6-rounding).

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic health records research in the NHS, with a focus on public accountability and research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 

