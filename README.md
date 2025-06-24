# Donor Migration to Contractual Relationships and the Impact on Charity Income

1) DATA
   
The raw data used in this study are subject to a non-disclosure agreement and cannot be shared.

3) CODE

All code was written in R and the major .R files are stored in the src folder.

4) MODEL

a) Main model

The file "model_estimation.R" contains the code used to estimate the proposed model described in the paper.

b) Model identification

To assess model identification, we simulated a dataset using the proposed model and the observed values of the independent variables, then tested whether we could retrieve the true parameters.

The file "model_identification_simulation_data_creation.R" contains the code used to simulate the dataset. 

The file "model_identification_simulation_estimation.R" contains the code used to estimate the parameters using the simulated dataset.  


5) OUTPUT

Detailed outputs from the model identification simulation and robustness checks are stored in the output folder.

The Excel file "Model_identification_full_results.xlsx" contains the true parameter values used in the simulation alongside the estimated parameters, their standard errors, t-values, and p-values.

The Excel file "Robustness_checks_results.xlsx" contains all parameter estimates for the main model and the four robustness checks presented in the paper, along with their corresponding standard errors, t-values, and p-values. 



   




