This repo is intended to replicate the study [`Impact of vaccination on the association of COVID-19 with cardiovascular diseases: An OpenSAFELY cohort study`](https://doi.org/10.1038/s41467-024-46497-0) (Cezard et al., 2024).  Specifically doing so in order to investigate the variable selection methodology and handling of confounders within the original study.  This is done by means of replicating the section of the study on the ``pre-vaccination'' cohort and then implementing the following:

-   Repeating the methodology of the original paper up to and including the fitting of Cox regression models, the hazard ratios from which form the results of the study
-   Running a number of different variable selection methods (lasso, lasso_X and lasso_union, see below) to produce alternative variable sets
-   Repeating the fitting of the cox regression models under each of these new sets
-   Implementing and running the empirical unconfoundedness test provided by [Hartwig et al., 2024](https://arxiv.org/abs/2402.10156) to verify the unconfoundedness of these variable sets

Technical details:

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
     
    -   The script for generating a random 10% sample of the study population is in the [`generate_subsample`](./analysis/generate_subsample) directory:
        -   [`generate_subsample.R`](analysis/generate_subsample/generate_subsample.R) generates the subsample itself.  The subsample is randomly sampled, but for reproducibility-sake, the seed is set in the program.
        
    -   The script for conducting variable selection using a LASSO (Least absolute shrinkage and selection) model is in the [`lasso_var_selection`](./analysis/lasso_var_selection) directory:
        -   [`lasso_var_selection.R`](analysis/lasso_var_selection/lasso_var_selection.R) fits a cox-regression model (family = "cox") using the subsample data (10% subsample as generated by [`generate_subsample.R`](analysis/generate_subsample/generate_subsample.R)) and applying a LASSO penalty function (alpha = 1).  The regularisation parameter lambda is tuned using cross-validation (cv.glmnet) to minimise cvm (mean cross-validated error).  The result is a subset of selected variables whose corresponding coefficient does not shrink to zero.  For further information please see the documentation for the glmnet and cv.glmnet functions:
            -   [`glmnet`](https://www.rdocumentation.org/packages/glmnet/versions/4.1-10/topics/glmnet)
            -   [`cv.glmnet`](https://www.rdocumentation.org/packages/glmnet/versions/4.1-10/topics/cv.glmnet)
        
    -   The script for conducting variable selection using a LASSO X (Least absolute shrinkage and selection for exposure) model which takes the exposure (COVID-19) as the response variable is in the [`lasso_X_var_selection`](./analysis/lasso_X_var_selection) directory:
        -   [`lasso_X_var_selection.R`](analysis/lasso_X_var_selection/lasso_X_var_selection.R) fits a logistic regression (family = "binomial") using binary exposure (X) to COVID-19 as the response variable and excluding the oucomes (Y, acute MI and subarachnoid haemorrhage / haemorrhage stroke) from the dataset.  The model is fit using the subsample data (10% subsample as generated by [`generate_subsample.R`](analysis/generate_subsample/generate_subsample.R)).  LASSO penalty is applied (alpha = 1).  The regularisation parameter lambda is tuned using cross-validation (cv.glmnet) to minimise cvm (mean cross-validated error).  The result is a subset of selected variables whose corresponding coefficient does not shrink to zero.  For further information please see the documentation for the glmnet and cv.glmnet functions:
            -   [`glmnet`](https://www.rdocumentation.org/packages/glmnet/versions/4.1-10/topics/glmnet)
            -   [`cv.glmnet`](https://www.rdocumentation.org/packages/glmnet/versions/4.1-10/topics/cv.glmnet)
        
    -   The script for conducting variable selection using a Union LASSO (Least absolute shrinkage and selection) model is in the [`lasso_union_var_selection`](./analysis/lasso_union_var_selection) directory:
        -   [`lasso_union_var_selection.R`](analysis/lasso_union_var_selection/lasso_union_var_selection.R) takes the union of the two variable sets selected by [`lasso_var_selection.R`](analysis/lasso_var_selection/lasso_var_selection.R) and [`lasso_X_var_selection.R`](analysis/lasso_X_var_selection/lasso_X_var_selection.R).
        
    -   The script which implements the [Hartwig et al., 2024](https://arxiv.org/abs/2402.10156) empirical unconfoundedness test is in the [`unconfoundedness_test`](./analysis/unconfoundedness_test) directory:
        -   [`unconfoundedness_test.R`](analysis/unconfoundedness_test/unconfoundedness_test.R) performs the empirical unconfoundedness test in the following manner:
            -   A cox-regression model taking the oucomes (Y) as the response is fit in the same manner as in [`lasso_var_selection.R`](analysis/lasso_var_selection/lasso_var_selection.R).
            -   A logistic regression model taking the exposure (X) as the response is fit in the same manner as in [`lasso_X_var_selection.R`](analysis/lasso_X_var_selection/lasso_X_var_selection.R).
            -   These two regression models are used to evaluate associations of each confounder (Z) with the exposure (X) and outcome (Y) in the following manner:
                -   For every candidate confounder Z, condition (i) is checked (is Z associated with (i.e., not independent of) X given all other covariates?)
                -   For every candidate confounder Z, condition (ii) is checked (are Z and Y are conditionally independent given X and all other covariates?)
                -   If any covariate Z satisfies both (i) and (ii), then the covariate set is sufficient for confounding adjustment.  If not, then the test is inconclusive.
            -   Test conditions, coefifcient values, p-values and standard errors are saved for each confounder Z.

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

