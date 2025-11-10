

# APS VE TTE Model Fitting #3
# 2021-22 Ages 5-11 
# 2025-11-10
# Modified to run on the cluster
# Amy Moore

#Run Locally?
cluster = TRUE
# cluster = FALSE

#Package Library Location
if (cluster) {
  .libPaths("~/Rlibs")
} else {
  library(here) # File Locations
}


library(readr) #Read in Files
library(tidyverse) # Data Manipulation
library(lubridate) #Dates
# library(ggplot2)
library(survival) #Cox Model
library(coxme) #Cox Model w/ RE

if (cluster) {
  source("/home/amoor53/APSVE_cluster/code/functions/fit_TTE_model.R")
} else {
  source(here("code", "functions", "fit_TTE_model.R"))
}



##########################
##### Fitting Models #####
##########################



### Model #3 - Regression Adjusting for Demographic Covariates on Super tester Subset

if (cluster) {
  data <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_ST_matched_data_set.csv")
} else {
  data <- read_csv(here("cleandata", "2021_age_5_11_ST_matched_data_set.csv"))
}

data <- read_csv(here("cleandata", "2021_age_5_11_ST_matched_data_set.csv"))
formula <- Surv(time = time_to_event, event = event_occured) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname)
method <- "TTE03"

results <- fit_TTE_model(data = data, 
                         model_formula = formula, 
                         method = method)


if (cluster) {
  saveRDS(results, file = paste0("/home/amoor53/APSVE_cluster/models/2021_age_5_11_", method, "_model.rds"))
  write.csv(results[[2]], paste0("/home/amoor53/APSVE_cluster/results/2021_age_5_11_", method, "_model_fit.csv"))
} else {
  saveRDS(results, file = here("models", paste0("2021_age_5_11_", method, "_model.rds")))
  write.csv(results[[2]], here("results", paste0("2021_age_5_11_", method, "_model_fit.csv")))
}
