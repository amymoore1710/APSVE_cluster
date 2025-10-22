
# APS VE TND Model Fitting
# 2021-22 Ages 5-11 
# 2025-10-22
# Modified to run on the cluster
# Amy Moore

library(readr) #Read in Files
library(here) # File Locations
library(lubridate) #Date Objects
library(tidyverse) # Data Manipulation
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs


# Read in Long Form with Missing Data lines Data set
VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")

start_date <- ymd("2021-09-07") #First day of VS testing
end_date <- ymd("2022-05-26") #Last day of School
first_sunday <- as.numeric(ymd("2021-09-05")) - 7

alpha <- 0.05

VScohort.IDs <- sort(unique(VScohort.tested$ID))

weeks <- sort(unique(VScohort.tested$week))


#Select only the weeks/IDs where testing actually occurred
VScohort.alltested <- VScohort.tested[which(VScohort.tested$tested == 1),]

hist(VScohort.tested$propensity)
summary(VScohort.tested$propensity)

hist(VScohort.alltested$propensity)
summary(VScohort.alltested$propensity)


results_table <- data.frame(method = c("No Adjustment", "Demographic Covariates", 
                                       "Super Tester Subset", "Average Testing Behavior", 
                                       "Time-varying Testing Behavior", 
                                       "Time-varying Testing Behavior with Propensity Weighting"), 
                            nstudents = NA, ntests = NA, VE = NA, pval = NA)


    ##########################
    ##### Fitting Models #####
    ##########################



  ### Model #1 - Simple Regression no covariates 

model1 <- glm(formula = result ~ vax_status,
                family = binomial,
                data = VScohort.alltested)
summary(model1)

saveRDS(model1, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TND_01_model.rds")

nstudents <- length(unique(VScohort.alltested$ID))
ntests <- nrow(VScohort.alltested)
log_OR <- summary(model1)$coefficients[2,1]
VE_est <- 100 * (1 - exp(log_OR))
log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model1)$coefficients[2,2]
VE_CI <- 100 * (1 - exp(log_OR_CI))
pval <- summary(model1)$coefficients[2,4]


results_model1 <- c("No Adjustment", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[1,] <- results_model1



  ### Model #2 - Regression Adjusting for Demographic Covariates

model2 <- glmer(formula = result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5),
                family = binomial,
                data = VScohort.alltested,
                control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1000000)))
summary(model2)

saveRDS(model2, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TND_02_model.rds")

nstudents <- length(unique(VScohort.alltested$ID))
ntests <- nrow(VScohort.alltested)
log_OR <- summary(model2)$coefficients[2,1]
VE_est <- 100 * (1 - exp(log_OR))
log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model2)$coefficients[2,2]
VE_CI <- 100 * (1 - exp(log_OR_CI))
pval <- summary(model2)$coefficients[2,4]


results_model2 <- c("Demographic Covariates", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[2,] <- results_model2


  ### Model #3 - Regression Adjusting for Demographic Covariates on Super tester Subset

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


model3 <- glmer(formula = result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5),
                family = binomial,
                data = STcohort.alltested,
                control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1000000)))
summary(model3)

saveRDS(model3, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TND_03_model.rds")

nstudents <- length(unique(STcohort.alltested$ID))
ntests <- nrow(STcohort.alltested)
log_OR <- summary(model3)$coefficients[2,1]
VE_est <- 100 * (1 - exp(log_OR))
log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model3)$coefficients[2,2]
VE_CI <- 100 * (1 - exp(log_OR_CI))
pval <- summary(model3)$coefficients[2,4]


results_model3 <- c("Super Tester Subset", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[3,] <- results_model3



  ### Model #4 - Regression Adjusting for Demographic Covariates and Average Testing Behavior

#Compute Average Testing Behavior
average_testing_behavior_byID <- VScohort.alltested %>% group_by(ID) %>% 
  summarize(total_tests = sum(tested), avg_time_since_last = mean(time_since_last), 
            avg_tests_in_28 = mean(tests_in_28), avg_tests_in_14 = mean(tests_in_14), 
            avg_test_density = mean(test_density), avg_adj_test_density = mean(adj_test_density))

VScohort.alltested <- merge(x = VScohort.alltested, 
               y = average_testing_behavior_byID,
               by = "ID",
               all.x = TRUE)

#Fit the model
model4 <- glmer(formula = result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + total_tests + avg_time_since_last + avg_tests_in_28 + avg_tests_in_14 + avg_test_density + avg_adj_test_density,
                family = binomial,
                data = VScohort.alltested,
                control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=10000000)))
summary(model4)

saveRDS(model4, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TND_04_model.rds")

nstudents <- length(unique(VScohort.alltested$ID))
ntests <- nrow(VScohort.alltested)
log_OR <- summary(model4)$coefficients[2,1]
VE_est <- 100 * (1 - exp(log_OR))
log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model4)$coefficients[2,2]
VE_CI <- 100 * (1 - exp(log_OR_CI))
pval <- summary(model4)$coefficients[2,4]


results_model4 <- c("Average Testing Behavior", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[4,] <- results_model4



  ### Model #5 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior
model5 <- glmer(formula = result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history,
                family = binomial,
                data = VScohort.alltested,
                control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=10000000)))
summary(model5)

saveRDS(model5, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TND_05_model.rds")

nstudents <- length(unique(VScohort.alltested$ID))
ntests <- nrow(VScohort.alltested)
log_OR <- summary(model5)$coefficients[2,1]
VE_est <- 100 * (1 - exp(log_OR))
log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model5)$coefficients[2,2]
VE_CI <- 100 * (1 - exp(log_OR_CI))
pval <- summary(model5)$coefficients[2,4]


results_model5 <- c("Time-varying Testing Behavior", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[5,] <- results_model5



  ### Model #6 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior with Propensity Weights

# Create Inverse Propensity Weights
IPS_weights <- round(1 / VScohort.alltested$propensity, digits = 0)
VScohort.alltested$IPS_weights <- IPS_weights

model6 <- glmer(formula = result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history,
                family = binomial,
                data = VScohort.alltested,
                weights = IPS_weights,
                control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=10000000)))
summary(model6)

saveRDS(model6, file = "/home/amoor53/APSVE_cluster/models/2021_age_5_11_TND_06_model.rds")

nstudents <- length(unique(VScohort.alltested$ID))
ntests <- nrow(VScohort.alltested)
log_OR <- summary(model6)$coefficients[2,1]
VE_est <- 100 * (1 - exp(log_OR))
log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model6)$coefficients[2,2]
VE_CI <- 100 * (1 - exp(log_OR_CI))
pval <- summary(model6)$coefficients[2,4]


results_model6 <- c("Time-varying Testing Behavior with Propensity Weighting", nstudents, ntests, 
                    paste0(round(VE_est, digits = 2), "% (", 
                           round(min(VE_CI), digits = 2), "% to ", 
                           round(max(VE_CI), digits = 2), "%)"), 
                    round(pval, digits = 4))

results_table[6,] <- results_model6


write.csv(results_table, "/home/amoor53/APSVE_cluster/results/2021_age_5_11_TND_model_fit.csv")



