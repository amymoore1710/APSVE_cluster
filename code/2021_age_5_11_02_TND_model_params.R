
# APS VE TND Model parameter sets
# 2021-22 Ages 5-11 
# 2025-11-11
# Modified to run on the cluster
# Amy Moore

#Run Locally?
# cluster = TRUE
cluster = FALSE

#Package Library Location
if (cluster) {
  .libPaths("~/Rlibs")
} else {
  library(here) # File Locations
}


####################
#### Param Sets ####
####################

num_models <- 6

params_list <- data.frame(data_file = rep(NA, num_models),
                          model_formula = rep(NA, num_models),
                          method = rep(NA, num_models))

### Model #1 - Simple Regression no covariates

method <- "TND_01"
model_formula <- "result ~ vax_status"
data_file <- "2021_age_5_11_all_tested.csv"

params_list[1,] <- c(data_file, model_formula, method)



### Model #2 - Regression Adjusting for Demographic Covariates

method <- "TND_02"
model_formula <- "result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5)"
data_file <- "2021_age_5_11_all_tested.csv"

params_list[2,] <- c(data_file, model_formula, method)


### Model #3 - Regression Adjusting for Demographic Covariates on Super tester Subset

method <- "TND_03"
model_formula <- "result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5)"
data_file <- "2021_age_5_11_ST_all_tested.csv"

params_list[3,] <- c(data_file, model_formula, method)


### Model #4 - Regression Adjusting for Average Testing Behavior

method <- "TND_04"
model_formula <- "result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + total_tests + avg_time_since_last + avg_tests_in_28 + avg_tests_in_14 + avg_test_density + avg_adj_test_density"
data_file <- "2021_age_5_11_all_tested.csv"

params_list[4,] <- c(data_file, model_formula, method)


### Model #5 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior

method <- "TND_05"
model_formula <- "result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history"
data_file <- "2021_age_5_11_all_tested.csv"

params_list[5,] <- c(data_file, model_formula, method)


### Model #6 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior with Propensity Weights

method <- "TND_06"
model_formula <- "result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history"
data_file <- "2021_age_5_11_all_tested.csv"

params_list[6,] <- c(data_file, model_formula, method)


if (cluster) {
  write.csv(params_list, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_TND_params.csv")
} else {
  write.csv(params_list, file = here("cleandata", "2021_age_5_11_TND_params.csv"))
}





