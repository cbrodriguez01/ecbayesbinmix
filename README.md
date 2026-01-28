
This repository contains R code used for the paper titled "A Bayesian Mixture Model Approach to Examining Neighborhood Social Determinants of Health Disparities in Endometrial Cancer Care in Massachusetts". 

*Notes:* 

- We do not include our code for data cleaning and defining the outcome "optimal care" from the Massachusetts Cancer Registry data. Access to this data requires IRB approval. For more information visit [MCR](https://www.mass.gov/info-details/massachusetts-cancer-registry-mcr-data).

- We have census data for three surveys of the American Community Survey (ACS) in this repository as we looked at all three, however the paper referenced above only includes data from the 2015-2019 ACS.


For any further inquiries, please email `crodriguezcabrera@g.harvard.edu`.

Main files for manuscript:

* `CENSUSBinPrep_.R` : Data binarization for the multivariate Bernoulli mixture model.

* `MBMMmodels_manuscript_.R`:  Multivariate Bernoulli mixture model output processing, and figures.

* `ECbayesbinmix_manuscript_.R`: Tables and regression analysis.

* `nsdoh_profiles_app_edited`:  Folder containing code for the interactive map created with Shiny. This one contains the urban boundaries.

