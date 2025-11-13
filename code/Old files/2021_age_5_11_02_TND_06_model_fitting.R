
# APS VE TND Model Fitting #6
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
library(lubridate) #Date Objects
library(tidyverse) # Data Manipulation
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs


# Read in Long Form with Missing Data lines Data set
if (cluster) {
  VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
} else {
  VScohort.tested <- read_csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))
}

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


results_table <- data.frame(method = c("No Adjustment"), 
                            nstudents = NA, ntests = NA, VE = NA, pval = NA)


##########################
##### Fitting Models #####
##########################


### Model #6 - Regression Adjusting for Demographic Covariates and Time-varying Testing Behavior with Propensity Weights

print("Fitting Model 6")
print(" ")
print(" ")
method <- "TND_06"

# Create Inverse Propensity Weights
IPS_weights <- round(1 / VScohort.alltested$propensity, digits = 0)
VScohort.alltested$IPS_weights <- IPS_weights

model6 <- glmer(formula = result ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history,
                family = binomial,
                data = VScohort.alltested,
                weights = IPS_weights,
                control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=10000000)))
summary(model6)

if (cluster) {
  saveRDS(model1, file = paste0("/home/amoor53/APSVE_cluster/models/2021_age_5_11_", method, "_model.rds"))
} else {
  saveRDS(model1, file = here("models", paste0("2021_age_5_11_", method, "_model.rds")))
}


nstudents <- length(unique(VScohort.alltested$ID))
ntests <- nrow(VScohort.alltested)
log_OR <- summary(model6)$coefficients[2,1]
VE_est <- 100 * (1 - exp(log_OR))
log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model6)$coefficients[2,2]
VE_CI <- 100 * (1 - exp(log_OR_CI))
pval <- summary(model6)$coefficients[2,4]


results_model6 <- data.frame(method = "Time-varying Testing Behavior with Propensity Weighting", 
                             nstudents = nstudents, 
                             ntests = ntests, 
                             VE = paste0(round(VE_est, digits = 2), "% (", 
                                         round(min(VE_CI), digits = 2), "% to ", 
                                         round(max(VE_CI), digits = 2), "%)"), 
                             pval = round(pval, digits = 4))

if (cluster) {
  write.csv(results_model1, paste0("/home/amoor53/APSVE_cluster/results/2021_age_5_11_", method, "_model_fit.csv"))
} else {
  write.csv(results_model1, here("results", paste0("2021_age_5_11_", method, "_model_fit.csv")))
}