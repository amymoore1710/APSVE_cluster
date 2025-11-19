
# APS VE TTE Model parameter sets
# 2021-22 Ages 5-11 
# 2025-11-11
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
# library(lubridate) #Dates
# # library(ggplot2)
# library(survival) #Cox Model
# library(coxme) #Cox Model w/ RE


####################
#### Param Sets ####
####################

print("")
print("")
print("------------------------------")
print("Begin File: Create Param Sets")
print("------------------------------")
print("")
print("")

num_models <- 6

params_list <- data.frame(data_file = rep(NA, num_models),
                          model_formula = rep(NA, num_models),
                          method = rep(NA, num_models))

### Model #1 - Simple Regression no covariates

method <- "TTE_01"
model_formula <- "Surv(time = time_to_event, event = event_occured) ~ vax_status"
data_file <- "2021_age_5_11_matched_data_set.csv"

params_list[1,] <- c(data_file, model_formula, method)



### Model #2 - Regression Adjusting for Demographic Covariates

method <- "TTE_02"
model_formula <- "Surv(time = time_to_event, event = event_occured) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname)"
data_file <- "2021_age_5_11_matched_data_set.csv"

params_list[2,] <- c(data_file, model_formula, method)


### Model #3 - Regression Adjusting for Demographic Covariates on Super tester Subset

method <- "TTE_03"
model_formula <- "Surv(time = time_to_event, event = event_occured) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname)"
data_file <- "2021_age_5_11_ST_matched_data_set.csv"

params_list[3,] <- c(data_file, model_formula, method)


### Model #4 - Regression Adjusting for Average Testing Behavior

method <- "TTE_04"
model_formula <- "Surv(time = time_to_event, event = event_occured) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + n_tests + avg_tests_in_28 + avg_tests_in_14 + avg_test_density + avg_adj_test_density + (1 | schoolname)"
data_file <- "2021_age_5_11_matched_data_set.csv"

params_list[4,] <- c(data_file, model_formula, method)


### Model #5 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior

method <- "TTE_05"
model_formula <- "Surv(time = previous_week, time2 = week, event = result) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + no_history + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + (1 | schoolname)"
data_file <- "2021_age_5_11_matched_TVC.csv"

params_list[5,] <- c(data_file, model_formula, method)


### Model #6 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior with Propensity Weights

method <- "TTE_06"
model_formula <- "Surv(time = previous_week, time2 = week, event = result) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + no_history + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + (1 | schoolname)"
data_file <- "2021_age_5_11_matched_TVC.csv"

params_list[6,] <- c(data_file, model_formula, method)


if (cluster) {
  write.csv(params_list, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_TTE_params.csv")
} else {
  write.csv(params_list, file = here("cleandata", "2021_age_5_11_TTE_params.csv"))
}

print("")
print("")
print("------------------------------")
print("End File: Save Param Sets")
print("------------------------------")
print("")
print("")


