# factors_impacting_influenza_vaccination
The objective of this project is to analyse the patient factors/features 'Age', 'Sex', 'SIMD' and 'UR8' in relation to their impact on the uptake of the influenza vaccination. 

For package compatibility, it is recommended for this project to be run on Python 3.13.0.

This repository mostly contains Machine Learning Code as the data extraction and cleaning stages required are specific from dataset to dataset.

At the point of using the dataset with the Machine Learning scripts, it is expected for the master dataset to have the following characteristic:

The variables as follows:

- cohort_group_ML_analysis - The respective cohort groups (18_TO_64_FLU_AT_RISK, HEALTH_CARE_WORKERS, etc.)

- patient_sex	- Male or Female in integer form 0 (Male) or 1 (Female)

- patient_age	- Continuous variable in integer form

- SIMD_quintile - integer 1 to 5

- UR8_2022	- integer 1 to 8 

- attended_vaccination_event - attended vaccination boolean (TRUE or FALSE) in integer form  0 (FALSE) or 1 (TRUE)



The datasets used for this project will not be uploaded to github.
