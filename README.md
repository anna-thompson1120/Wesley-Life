
# Title: Modeling Food Insecurity in Iowa
### Author: Anna Thompson
### Group Members: Amelia Eibert, Zofia Landowska, and Nicholas Nix
### Date: 2024-12-10



## Introduction 
This repository contains the code and data required to reproduce the results found in "Modeling Food Insecurity in Iowa". Specifically, to process and clean ACS and CPS data in order to run a lasso, ridge, and MLE regression to predict specific PUMA regions with various levels of senior food insecurity in Iowa. 

## Methods
This code uses training and test datasets randomly selected from the cleaned_cps dataset to create a random forest with a variable importance plot. 

It then creates a general MLE, as well as ridge and lasso regressions that have both been optimized using various lambda values to create finalized versions of each model. These were then all plotted on an ROC curve where we determined that the lasso model had performed the best of the three. Finally, this finalized lasso model was applied to our cleaned_acs dataset in order to predict the percentages of food insecure people in each PUMA, using FSFOODS or FSSTATUS as the measure of food insecurity. Finally, these predictions and this model was used to create our cloropleth maps as well as our variable coefficient interpretations.

## Requirements
To install the required R packages, run the following code in R:

```{r, eval=FALSE}
install.packages(c("tidyverse", "ggthemes", "logistf", "glmnet", "haven", 
                   "randomForest", "pROC", "sf", "tigris"))

```

## File and Folder Structure
The following files can be found in the corresponding folders.

### Code Files
"code/cleaned_cps.R"  
"code/cleaned_acs.R"   
"code/model_FSFOODS.R"   
"code/model_FSSTATUS.R"  

### Data Files
"data/cps_00006.csv"  
"data/spm_pu_2002.sas7bdat"  
"data/total_iowa_seniors_by_puma.csv"  

## Data

We use three sources of data containing demographics, locations, and food insecurity measures of families around Iowa. This data can be found in the following subfolder:

"data/"

The data files that will be called are "cps_00006.csv", "spm_pu_2002.sas7bdat", and "total_iowa_seniors_by_puma.csv".

In the process of running our main R files below, it will also run the following code:

"code/clean_cps.R" 
"code/clean_acs.R"

These files will do generally the same preperation work on the raw ACS and CPS data. Specifically, they will create the following variables:

- female &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;          Total number of females in the household
- hispanic  &nbsp;&nbsp;&nbsp;&nbsp;      Total number of Hispanic people in the household
- black  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;         Total number of Black people in the household
- kids     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;       Total number of people under 18 years old in the household
- elderly    &nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;    Total number of people older than 60 in the household
- education   &nbsp;&nbsp;    Total number of people with a post high school degree in the household
- married   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;      Total number of married people in the household
- p.female   &nbsp;&nbsp;&nbsp;&nbsp;     The percentage of females in the household
- p.hispanic  &nbsp;    The percentage of Hispanic people in the household
- p.black    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;     The percentage of Black people in the household
- p.kids     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;     The percentage of people under 18 years old in the household
- p.elderly   &nbsp;&nbsp;&nbsp;    The percentage of people over 60 years old in the household

## Reproduce
1. Run "code/model_FSFOODS.R" to create a random forest, ridge regression, lasso regression, and MLE regression to predict the variable FSFOODS in both the CPS and ACS datasets. This code will also create multiple graphs including a variable importance plot, ROC curves for all three regressions, and two choropleth maps based on the final lasso model. One of these cloropleth maps shows areas and the predicted percentage of food insecure families in that area, and the other shows the predicted amount of food insecure seniors in that area. This code also includes interpretations of some of the beta coefficients of the final lasso model.

2. Run "code/model_FSSTATUS.R" to create a random forest, ridge regression, lasso regression, and MLE regression to predict the variable FSSTATUS in both the CPS and ACS datasets. This code will also create multiple graphs including a variable importance plot, ROC curves for all three regressions, and two choropleth maps based on the final lasso model. One of these cloropleth maps shows areas and the predicted percentage of food insecure families in that area, and the other shows the predicted amount of food insecure seniors in that area. This code also includes interpretations of some of the beta coefficients of the final lasso model.
