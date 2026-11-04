
# APS VE TTE Model Fitting Parallel
# 2026-11-03
# Modified to run on the cluster
# Amy Moore

#update to play with git

print("")
print("")
print("------------------------------")
print("Begin File 5: Fit VE Models")
print("------------------------------")
print("")
print("")


#Run Locally?
cluster = TRUE
# cluster = FALSE

#Extra Print outs
verbose = TRUE
# verbose = FALSE

#Package Library Location
if (cluster) {
  .libPaths("~/Rlibs")
} else {
  library(here) # File Locations
}

library(readr) #Read in Files
library(tidyverse) # Data Manipulation
library(lubridate) #Dates
library(survival) #Cox Model
library(coxme) #Cox Model w/ RE

if(cluster){
  source("/home/amoor53/APSVE_cluster/code/00_config.R")
  source("/home/amoor53/APSVE_cluster/code/helpers.R")
  task <- get_task(model_grid)
  set.seed(cfg$seed + match(task$subcohort, subcohorts))   # reproducible per task
  VScohort_weekly_match_observed <- readRDS(paste0("/home/amoor53/APSVE_cluster/cleandata/", task$subcohort, "_", task$cohort_type, "_matched.rds"))
} else {
  source(here("code", "00_config.R"))
  source(here("code", "helpers.R"))
  task <- get_task(model_grid)
  set.seed(cfg$seed + match(task$subcohort, subcohorts))   # reproducible per task
  VScohort_weekly_match_observed <- readRDS(here("cleandata", paste0(task$subcohort, "_", task$cohort_type, "_matched.rds")))
}

print(paste("Task:", task$subcohort, task$cohort_type, task$model_method))

print("")
print("")
print("------------------------------")
print("Step 1: Fit Models")
print("------------------------------")
print("")
print("")

if(task$cohort_type == "full" & task$model_method == "Standard") {
  model_formula <- "Surv(time = prior_week, time2 = week, event = positive) ~ vax_status + continuous_age + gender + race + dir_cert + prior_infections + (1 | schoolname)"
} else if(task$cohort_type == "super_tester" & task$model_method == "Standard") {
  model_formula <- "Surv(time = prior_week, time2 = week, event = positive) ~ vax_status + continuous_age + gender + race + dir_cert + prior_infections + (1 | schoolname)"
} else if(task$cohort_type == "full" & task$model_method == "Weighted") {
  model_formula <- "Surv(time = prior_week, time2 = week, event = positive) ~ vax_status + continuous_age + gender + race + dir_cert + prior_infections + first_test_indicator + weeks_since_last_test + tests_last_4weeks + test_density + (1 | schoolname)"
} else {
  model_formula <- "Error"
}

print(paste("Model Formula:", model_formula))


model_fit_list <- fit_TTE_model(data = VScohort_weekly_match_observed,
                                model_formula = as.formula(model_formula),
                                method = task$model_method)

print(summary(model_fit_list[[1]]))

print(model_fit_list[[2]])

if (cluster) {
  saveRDS(model_fit_list[[1]], 
          file = paste0("/home/amoor53/APSVE_cluster/models/", task$subcohort, "_", task$cohort_type, "_", task$model_method,"_VE_model.rds"))
  saveRDS(model_fit_list[[2]], 
          file = paste0("/home/amoor53/APSVE_cluster/results", task$subcohort, "_", task$cohort_type, "_", task$model_method,"_VE_model_summary.rds"))
} else {
  saveRDS(model_fit_list[[1]], 
          file = here("models", paste0(task$subcohort, "_", task$cohort_type, "_", task$model_method,"_VE_model.rds")))
  saveRDS(model_fit_list[[2]], 
          file = here("results", paste0(task$subcohort, "_", task$cohort_type, "_", task$model_method,"_VE_model_summary.rds")))
}

print("")
print("")
print("------------------------------")
print("End File: Model Fit and Saved")
print("------------------------------")
print("")
print("")