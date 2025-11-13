
# APS VE TND Model Fitting #1
# 2021-22 Ages 5-11 
# 2025-11-10
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
library(lubridate) #Date Objects
library(tidyverse) # Data Manipulation
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs

if (cluster) {
  source("/home/amoor53/APSVE_cluster/code/functions/fit_TND_model.R")
} else {
  source(here("code", "functions", "fit_TND_model.R"))
}


# Read in Long Form with Missing Data lines Data set
if (cluster) {
  VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
} else {
  VScohort.tested <- read_csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))
}

 

# start_date <- ymd("2021-09-07") #First day of VS testing
# end_date <- ymd("2022-05-26") #Last day of School
# first_sunday <- as.numeric(ymd("2021-09-05")) - 7
# 
# alpha <- 0.05
# 
# VScohort.IDs <- sort(unique(VScohort.tested$ID))
# 
# weeks <- sort(unique(VScohort.tested$week))


  #Select only the weeks/IDs where testing actually occurred
VScohort.alltested <- VScohort.tested[which(VScohort.tested$tested == 1),]

# hist(VScohort.tested$propensity)
# summary(VScohort.tested$propensity)
# 
# hist(VScohort.alltested$propensity)
# summary(VScohort.alltested$propensity)


# results_table <- data.frame(method = c("No Adjustment"), 
#                             nstudents = NA, ntests = NA, VE = NA, pval = NA)


##########################
##### Fitting Models #####
##########################



### Model #1 - Simple Regression no covariates 

print("Fitting Model 1")
print(" ")
print(" ")

method <- "TND_01"
model_formula <- result ~ vax_status

results <- fit_TND_model(data = VScohort.alltested, 
                         model_formula = model_formula, 
                         method = method)

if (cluster) {
  saveRDS(results, file = paste0("/home/amoor53/APSVE_cluster/models/2021_age_5_11_", method, "_model.rds"))
  write.csv(results[[2]], paste0("/home/amoor53/APSVE_cluster/results/2021_age_5_11_", method, "_model_fit.csv"))
} else {
  saveRDS(results, file = here("models", paste0("2021_age_5_11_", method, "_model.rds")))
  write.csv(results[[2]], here("results", paste0("2021_age_5_11_", method, "_model_fit.csv")))
}



# model1 <- glm(formula = result ~ vax_status,
#               family = binomial,
#               data = VScohort.alltested)
# summary(model1)
# 
# if (cluster) {
#   saveRDS(model1, file = paste0("/home/amoor53/APSVE_cluster/models/2021_age_5_11_", method, "_model.rds"))
# } else {
#   saveRDS(model1, file = here("models", paste0("2021_age_5_11_", method, "_model.rds")))
# }
# 
# 
# nstudents <- length(unique(VScohort.alltested$ID))
# ntests <- nrow(VScohort.alltested)
# log_OR <- summary(model1)$coefficients[2,1]
# VE_est <- 100 * (1 - exp(log_OR))
# log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model1)$coefficients[2,2]
# VE_CI <- 100 * (1 - exp(log_OR_CI))
# pval <- summary(model1)$coefficients[2,4]
# 
# 
# results_model1 <- data.frame(method = "No Adjustment", 
#                              nstudents = nstudents, 
#                              ntests = ntests, 
#                              VE = paste0(round(VE_est, digits = 2), "% (", 
#                                          round(min(VE_CI), digits = 2), "% to ", 
#                                          round(max(VE_CI), digits = 2), "%)"), 
#                              pval = round(pval, digits = 4))
# 
# if (cluster) {
#   write.csv(results_model1, paste0("/home/amoor53/APSVE_cluster/results/2021_age_5_11_", method, "_model_fit.csv"))
# } else {
#   write.csv(results_model1, here("results", paste0("2021_age_5_11_", method, "_model_fit.csv")))
# }

