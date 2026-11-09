##### Start of File #3 #####


print("")
print("")
print("------------------------------")
print("Begin File 3: Compute Propensity Scores")
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
library(lubridate) #Date Objects
library(dplyr) #Tidy Data
library(tidyverse) # Data Manipulation
library(tidyr) # Data Manipulation
library(slider) # Calendar Time Handling
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs


if(cluster){
  source("/home/amoor53/APSVE_cluster/code/00_config.R")
  task <- get_task(ps_grid)
  set.seed(cfg$seed + match(task$subcohort, subcohorts))   # reproducible per task
  VScohort_weekly <- readRDS(paste0("/home/amoor53/APSVE_cluster/cleandata/", task$subcohort, "_weekly_testing_by_ID.rds"))
} else {
  source(here("code", "00_config.R"))
  task <- get_task(ps_grid)
  set.seed(cfg$seed + match(task$subcohort, subcohorts))   # reproducible per task
  VScohort_weekly <- readRDS(here("cleandata", paste0(task$subcohort, "_weekly_testing_by_ID.rds")))
  
}

print(paste("Task:", task$subcohort))

# VScohort_by_ID_by_school <- VScohort_weekly %>%
#   group_by(schoolname, ID) %>%
#   summarize(n_tests = sum(num_tests))
# 
# table(VScohort_by_ID_by_school$schoolname)

#   #Testing with Small Subset --> Just two Schools
# if(task$subcohort %in% c("2021_5_11", "2022_5_11")) {
#   VScohort_weekly <- VScohort_weekly %>% filter(schoolname %in% c("Smith", "Burgess"))
# } else if (task$subcohort %in% c("2021_12_18", "2022_12_18")) {
#   VScohort_weekly <- VScohort_weekly %>% filter(schoolname %in% c("Howard", "Jackson High"))
# }


print("")
print("------------------------------")
print("Step 1: Fit Propensity Score Model with No Testing Behavior Covariates")
print("------------------------------")
print("")


start_time <- Sys.time()

reg.fit1 <-  glmer(formula = tested ~ vax_status + continuous_age + gender + race + dir_cert + 
                    prior_infections + week + (1 | schoolname) + (1 | ID),
                  family = binomial,
                  data = VScohort_weekly,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

end_time <- Sys.time()
elapsed_time <- end_time - start_time

print(elapsed_time)



summary(reg.fit1)

if (cluster) {
  saveRDS(reg.fit1,
          file = paste0("/home/amoor53/APSVE_cluster/models/", task$subcohort, "_propensity_model_basic.rds"))
  saveRDS(list(coefs = summary(reg.fit1)$coefficients, varcor = summary(reg.fit1)$varcor),
          file = paste0("/home/amoor53/APSVE_cluster/models/", task$subcohort, "_propensity_model_basic_summary.rds"))
} else {
  saveRDS(reg.fit1,
          file = here("models", paste0(task$subcohort, "_propensity_model_basic.rds")))
  saveRDS(list(coefs = summary(reg.fit1)$coefficients, varcor = summary(reg.fit1)$varcor),
          file = here("models", paste0(task$subcohort, "_propensity_model_basic_summary.rds")))
}



print("")
print("------------------------------")
print("Step 2: Fit Propensity Score Model with Testing Behavior Covariates")
print("------------------------------")
print("")

start_time <- Sys.time()

reg.fit2 <-  glmer(formula = tested ~ vax_status + continuous_age + gender + race + dir_cert + 
                    prior_infections + first_test_indicator + weeks_since_last_test + 
                    tests_last_4weeks + test_density + week + (1 | schoolname) + (1 | ID),
                  family = binomial,
                  data = VScohort_weekly,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

end_time <- Sys.time()
elapsed_time <- end_time - start_time

print(elapsed_time)



summary(reg.fit2)

propensities <- predict(reg.fit2, type = "response")
VScohort_weekly$propensity <- propensities

if (cluster) {
  saveRDS(reg.fit2,
          file = paste0("/home/amoor53/APSVE_cluster/models/", task$subcohort, "_propensity_model.rds"))
  saveRDS(list(coefs = summary(reg.fit2)$coefficients, varcor = summary(reg.fit2)$varcor),
          file = paste0("/home/amoor53/APSVE_cluster/models/", task$subcohort, "_propensity_model_summary.rds"))
  saveRDS(VScohort_weekly,
          file = paste0("/home/amoor53/APSVE_cluster/cleandata/", task$subcohort, "_weekly_propensity.rds"))
} else {
  saveRDS(reg.fit2,
          file = here("models", paste0(task$subcohort, "_propensity_model.rds")))
  saveRDS(list(coefs = summary(reg.fit2)$coefficients, varcor = summary(reg.fit2)$varcor),
          file = here("models", paste0(task$subcohort, "_propensity_model_summary.rds")))
  saveRDS(VScohort_weekly,
          file = here("cleandata", paste0(task$subcohort, "_weekly_propensity.rds")))
}

print("")
print("")
print("")
print("------------------------------")
print("------------------------------")
print("End File 3: Saving Weekly testing by ID with Propensity Scores")
print("------------------------------")
print("------------------------------")
print("")
print("")
print("")