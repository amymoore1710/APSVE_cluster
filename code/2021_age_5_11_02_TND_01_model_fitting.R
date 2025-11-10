
# APS VE TND Model Fitting #1
# 2021-22 Ages 5-11 
# 2025-11-10
# Modified to run on the cluster
# Amy Moore

#Package Library Location
.libPaths("~/Rlibs")

library(readr) #Read in Files
# library(here) # File Locations
library(lubridate) #Date Objects
library(tidyverse) # Data Manipulation
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs


# Read in Long Form with Missing Data lines Data set
VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")
# VScohort.tested <- read_csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))


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



### Model #1 - Simple Regression no covariates 

print("Fitting Model 1")
print(" ")
print(" ")

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


results_model1 <- data.frame(method = "No Adjustment", 
                             nstudents = nstudents, 
                             ntests = ntests, 
                             VE = paste0(round(VE_est, digits = 2), "% (", 
                                         round(min(VE_CI), digits = 2), "% to ", 
                                         round(max(VE_CI), digits = 2), "%)"), 
                             pval = round(pval, digits = 4))

write.csv(results_model1, "/home/amoor53/APSVE_cluster/results/2021_age_5_11_TND_01_model_fit.csv")
