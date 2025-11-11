
# APS VE TND Model Fitting Parallel
# 2021-22 Ages 5-11 
# 2025-11-11
# Modified to run on the cluster
# Amy Moore

#Package Library Location
.libPaths("~/Rlibs")

library(readr) #Read in Files
library(lubridate) #Date Objects
library(tidyverse) # Data Manipulation
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs

  #Model fitting function
source("/home/amoor53/APSVE_cluster/code/functions/fit_TND_model.R")

model_num <- commandArgs(trailingOnly = TRUE)[[1]]



if (model_num == "1") {
  
  ### Model #1 - Simple Regression no covariates
  
  VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
  VScohort.alltested <- VScohort.tested[which(VScohort.tested$tested == 1),]
  print("Fitting Model 1")
  print(" ")
  print(" ")
  
  method <- "TND_01"
  model_formula <- result ~ vax_status
  data <- VScohort.alltested
  
} else if (model_num == "2") {
  
  ### Model #2 - Regression Adjusting for Demographic Covariates
  
  VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
  VScohort.alltested <- VScohort.tested[which(VScohort.tested$tested == 1),]
  print("Fitting Model 2")
  print(" ")
  print(" ")
  
  method <- "TND_02"
  model_formula <- result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5)
  data <- VScohort.alltested
  
} else if (model_num == "3") {
  
  ### Model #3 - Regression Adjusting for Demographic Covariates on Super tester Subset
  
  VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
  VScohort.alltested <- VScohort.tested[which(VScohort.tested$tested == 1),]
  print("Defining Super Tester Cohort")
  print(" ")
  print(" ")
  
  #Defining the Super Tester Cohort
  VScohort.byID <- VScohort.tested %>% group_by(ID) %>% summarize(num_tests = sum(tested))
  max_tests <- max(VScohort.byID$num_tests)
  Q3 <- max_tests
  n <- nrow(VScohort.byID)
  
  for (i in seq(max_tests, 3, -1)){
    n_above <- nrow(VScohort.byID %>% filter(num_tests >= i))
    perc_above <- round(n_above/n * 100, digits = 2)
    
    if (perc_above < 25.00) {
      Q3 <- i
    }
  }
  print(Q3)
  
  VScohort.subset.byID <- VScohort.byID %>% filter(num_tests >= Q3)
  Supertester.IDs <- VScohort.subset.byID$ID
  
  STcohort.alltested <- VScohort.alltested %>% filter(ID %in% Supertester.IDs)
  
  print("Fitting Model 3")
  print(" ")
  print(" ")
  
  method <- "TND_03"
  model_formula <- result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5)
  data <- STcohort.alltested
  
} else if (model_num == "4") {
  
  ### Model #4 - Regression Adjusting for Average Testing Behavior
  
  VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
  VScohort.alltested <- VScohort.tested[which(VScohort.tested$tested == 1),]
  average_testing_behavior_byID <- VScohort.alltested %>% group_by(ID) %>% 
    summarize(total_tests = sum(tested), avg_time_since_last = mean(time_since_last), 
              avg_tests_in_28 = mean(tests_in_28), avg_tests_in_14 = mean(tests_in_14), 
              avg_test_density = mean(test_density), avg_adj_test_density = mean(adj_test_density))
  
  VScohort.alltested <- merge(x = VScohort.alltested, 
                              y = average_testing_behavior_byID,
                              by = "ID",
                              all.x = TRUE)
  
  print("Fitting Model 4")
  print(" ")
  print(" ")
  
  method <- "TND_04"
  model_formula <- result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + total_tests + avg_time_since_last + avg_tests_in_28 + avg_tests_in_14 + avg_test_density + avg_adj_test_density
  data <- VScohort.alltested
  
  
} else if (model_num == "5") {
  
  ### Model #5 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior
  
  VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
  VScohort.alltested <- VScohort.tested[which(VScohort.tested$tested == 1),]
  average_testing_behavior_byID <- VScohort.alltested %>% group_by(ID) %>% 
    summarize(total_tests = sum(tested), avg_time_since_last = mean(time_since_last), 
              avg_tests_in_28 = mean(tests_in_28), avg_tests_in_14 = mean(tests_in_14), 
              avg_test_density = mean(test_density), avg_adj_test_density = mean(adj_test_density))
  
  VScohort.alltested <- merge(x = VScohort.alltested, 
                              y = average_testing_behavior_byID,
                              by = "ID",
                              all.x = TRUE)
  
  print("Fitting Model 5")
  print(" ")
  print(" ")
  
  method <- "TND_05"
  model_formula <- result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history
  data <- VScohort.alltested
  
} else { #if (model_num == "6") {
  
  ### Model #6 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior with Propensity Weights
  
  VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
  VScohort.alltested <- VScohort.tested[which(VScohort.tested$tested == 1),]
  average_testing_behavior_byID <- VScohort.alltested %>% group_by(ID) %>% 
    summarize(total_tests = sum(tested), avg_time_since_last = mean(time_since_last), 
              avg_tests_in_28 = mean(tests_in_28), avg_tests_in_14 = mean(tests_in_14), 
              avg_test_density = mean(test_density), avg_adj_test_density = mean(adj_test_density))
  
  VScohort.alltested <- merge(x = VScohort.alltested, 
                              y = average_testing_behavior_byID,
                              by = "ID",
                              all.x = TRUE)
  
  # Create Inverse Propensity Weights
  IPS_weights <- round(1 / VScohort.alltested$propensity, digits = 0)
  VScohort.alltested$IPS_weights <- IPS_weights
  
  print("Fitting Model 6")
  print(" ")
  print(" ")
  
  method <- "TND_06"
  model_formula <- result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history
  data <- VScohort.alltested
  
}

results <- fit_TND_model(data = VScohort.alltested, 
                         model_formula = model_formula, 
                         method = method)

saveRDS(results, file = paste0("/home/amoor53/APSVE_cluster/models/2021_age_5_11_", method, "_model.rds"))
write.csv(results[[2]], paste0("/home/amoor53/APSVE_cluster/results/2021_age_5_11_", method, "_model_fit.csv"))



