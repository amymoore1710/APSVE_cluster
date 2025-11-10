

# APS VE TTE Model Fitting
# 2021-22 Ages 5-11 
# 2025-11-05
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


library(readr) #Read in Files
library(tidyverse) # Data Manipulation
library(lubridate) #Dates
# library(ggplot2)
library(survival) #Cox Model
library(coxme) #Cox Model w/ RE


# Read in Long Form with Missing Data lines Data set
if (cluster) {
  VScohort.matched <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_matched_data_set.csv")
  STcohort.matched <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_ST_matched_data_set.csv")
} else {
  VScohort.matched <- read_csv(here("cleandata", "2021_age_5_11_matched_data_set.csv"))
  STcohort.matched <- read_csv(here("cleandata", "2021_age_5_11_ST_matched_data_set.csv"))
}

start_date <- ymd("2021-09-07") #First day of VS testing
end_date <- ymd("2022-05-26") #Last day of School
first_sunday <- as.numeric(ymd("2021-09-05")) - 7

alpha <- 0.05

VScohort.IDs <- sort(unique(VScohort.matched$ID))


results_table <- data.frame(method = c("No Adjustment", "Demographic Covariates", 
                                       "Super Tester Subset", "Average Testing Behavior", 
                                       "Time-varying Testing Behavior", 
                                       "Time-varying Testing Behavior with Propensity Covariate", 
                                       "Time-varying Testing Behavior with Propensity Weights"), 
                            nstudents = NA, ntests = NA, VE = NA, pval = NA)



    ##########################
    ##### Fitting Models #####
    ##########################



  ### Model #1 - Simple Regression no covariates


#Fit Basic Cox Model
model1 <- coxph(Surv(time = time_to_event, event = event_occured) ~ vax_status,
                data = VScohort.matched
)
summary(model1)

saveRDS(model1, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TTE_01_model.rds")
# saveRDS(model1, file = here("models", "2021-22 Ages 5-11", "TTE Model 1 - No Adjustment.rds"))
# model1 <- readRDS(file = here("models", "2021-22 Ages 5-11", "TTE Model 1 - No Adjustment.rds"))

nstudents <- length(unique(VScohort.matched$ID))
ntests <- sum(VScohort.matched$n_tests)
log_HR <- summary(model1)$coefficients[1,1]
VE_est <- 100 * (1 - exp(log_HR))
log_HR_CI <- log_HR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model1)$coefficients[1,3]
VE_CI <- 100 * (1 - exp(log_HR_CI))
pval <- summary(model1)$coefficients[1,5]


results_model1 <- c("No Adjustment", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[1,] <- results_model1



  ### Model #2 - Regression Adjusting for Demographic Covariates

model2 <- coxme(Surv(time = time_to_event, event = event_occured) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname),
                       data = VScohort.matched
)
summary(model2)

saveRDS(model2, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TTE_02_model.rds")
# saveRDS(model2, file = here("models", "2021-22 Ages 5-11", "TTE Model 2 - Demographic Covariates.rds"))
# model2 <- readRDS(here("models", "2021-22 Ages 5-11", "TTE Model 2 - Demographic Covariates.rds"))


nstudents <- length(unique(VScohort.matched$ID))
ntests <- sum(VScohort.matched$n_tests)
log_HR <- summary(model2)$coefficients[1,1]
VE_est <- 100 * (1 - exp(log_HR))
log_HR_CI <- log_HR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model2)$coefficients[1,3]
VE_CI <- 100 * (1 - exp(log_HR_CI))
pval <- summary(model2)$coefficients[1,5]


results_model2 <- c("Demographic Covariates", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[2,] <- results_model2



  ### Model #3 - Regression Adjusting for Demographic Covariates on Super tester Subset
model3 <- coxme(Surv(time = time_to_event, event = event_occured) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname),
                data = STcohort.matched
)
summary(model3)

saveRDS(model3, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TTE_03_model.rds")
# saveRDS(model3, file = here("models", "2021-22 Ages 5-11", "TTE Model 3 - Super Tester Subset.rds"))
# model3 <- readRDS(here("models", "2021-22 Ages 5-11", "TTE Model 3 - Super Tester Subset.rds"))


nstudents <- length(unique(STcohort.matched$ID))
ntests <- sum(STcohort.matched$n_tests)
log_HR <- summary(model3)$coefficients[1,1]
VE_est <- 100 * (1 - exp(log_HR))
log_HR_CI <- log_HR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model3)$coefficients[1,3]
VE_CI <- 100 * (1 - exp(log_HR_CI))
pval <- summary(model3)$coefficients[1,5]


results_model3 <- c("Super Tester Subset", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[3,] <- results_model3



### Model #4 - Regression Adjusting for Demographic Covariates and Average Testing Behavior

#Fit the model
model4 <- coxme(Surv(time = time_to_event, event = event_occured) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + n_tests + avg_tests_in_28 + avg_tests_in_14 + avg_test_density + avg_adj_test_density + (1 | schoolname),
                       data = VScohort.matched
)
summary(model4)

saveRDS(model4, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TTE_04_model.rds")
# saveRDS(model4, file = here("models", "2021-22 Ages 5-11", "TTE Model 4 - Average Testing Behavior Adjustment.rds"))
# model4 <- readRDS(here("models", "2021-22 Ages 5-11", "TTE Model 4 - Average Testing Behavior Adjustment.rds"))


nstudents <- length(unique(VScohort.matched$ID))
ntests <- sum(VScohort.matched$n_tests)
log_HR <- summary(model4)$coefficients[1,1]
VE_est <- 100 * (1 - exp(log_HR))
log_HR_CI <- log_HR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model4)$coefficients[1,3]
VE_CI <- 100 * (1 - exp(log_HR_CI))
pval <- summary(model4)$coefficients[1,5]


results_model4 <- c("Average Testing Behavior", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[4,] <- results_model4




### Model #5 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior

VScohort.matched.TVC <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_matched_TVC.csv")

model5 <- coxme(Surv(time = previous_week, time2 = week, event = result) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname),
                data = VScohort.matched.TVC
)
summary(model5)


saveRDS(model5, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TTE_05_model.rds")
# saveRDS(model5, file = here("models", "2021-22 Ages 5-11", "TTE Model 5 - Time-varying Testing Behavior.rds"))
# model5 <- readRDS(here("models", "2021-22 Ages 5-11", "TTE Model 5 - Time-varying Testing Behavior.rds"))

nstudents <- length(unique(VScohort.matched.TVC$ID))
ntests <- nrow(VScohort.matched.TVC)
log_HR <- summary(model5)$coefficients[1,1]
VE_est <- 100 * (1 - exp(log_HR))
log_HR_CI <- log_HR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model5)$coefficients[1,3]
VE_CI <- 100 * (1 - exp(log_HR_CI))
pval <- summary(model5)$coefficients[1,5]


results_model5 <- c("Time-varying Testing Behavior", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[5,] <- results_model5




### Model #6A - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior with Covariate for Testing Propensity

model6A <- coxme(Surv(time = previous_week, time2 = week, event = result) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + propensity + (1 | schoolname),
                data = VScohort.matched.TVC
)
summary(model6A)


saveRDS(model6A, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TTE_06A_model.rds")
# saveRDS(model6A, file = here("models", "2021-22 Ages 5-11", "TTE Model 6A - Time-varying Testing Behavior with Propensity Covariate.rds"))
# model6A <- readRDS(here("models", "2021-22 Ages 5-11", "TTE Model 6A - Time-varying Testing Behavior with Propensity Covariate.rds"))

nstudents <- length(unique(VScohort.matched.TVC$ID))
ntests <- nrow(VScohort.matched.TVC)
log_HR <- summary(model6A)$coefficients[1,1]
VE_est <- 100 * (1 - exp(log_HR))
log_HR_CI <- log_HR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model6A)$coefficients[1,3]
VE_CI <- 100 * (1 - exp(log_OR_CI))
pval <- summary(model6A)$coefficients[1,5]


results_model6A <- c("Time-varying Testing Behavior with Propensity Covariate", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[6,] <- results_model6A





### Model #6B - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior with Testing Propensity Weights

IPS_weights <- round(1 / VScohort.matched.TVC$propensity, digits = 0)
VScohort.matched.TVC$IPS_weights <- IPS_weights

model6B <- coxme(Surv(time = previous_week, time2 = week, event = result) ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname),
                 data = VScohort.matched.TVC,
                 weights = IPS_weights
)
summary(model6B)


saveRDS(model6B, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TTE_06B_model.rds")
# saveRDS(model6B, file = here("models", "2021-22 Ages 5-11", "TTE Model 6B - Time-varying Testing Behavior with Propensity Weights.rds"))
# model6A <- readRDS(here("models", "2021-22 Ages 5-11", "TTE Model 6B - Time-varying Testing Behavior with Propensity Weights.rds"))

nstudents <- length(unique(VScohort.matched.TVC$ID))
ntests <- nrow(VScohort.matched.TVC)
log_HR <- summary(model6B)$coefficients[1,1]
VE_est <- 100 * (1 - exp(log_HR))
log_HR_CI <- log_HR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model6B)$coefficients[1,3]
VE_CI <- 100 * (1 - exp(log_HR_CI))
pval <- summary(model6B)$coefficients[1,5]


results_model6B <- c("Time-varying Testing Behavior with Propensity Weights", nstudents, ntests, 
                     paste0(round(VE_est, digits = 2), "% (", 
                            round(min(VE_CI), digits = 2), "% to ", 
                            round(max(VE_CI), digits = 2), "%)"), 
                     round(pval, digits = 4))

results_table[7,] <- results_model6B



write.csv(results_table, "/home/amoor53/APSVE_cluster/results/2021_age_5_11_TTE_model_fit.csv")
# write.csv(results_table, here("tables", "Result Table Model Fit - 2021-22 Ages 5-11 TTE.csv"))


