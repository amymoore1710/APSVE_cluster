
# APS VE TND Model Fitting #3
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


### Model #3 - Regression Adjusting for Demographic Covariates on Super tester Subset

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



results_model3 <- data.frame(method = "Super Tester Subset", 
                             nstudents = nstudents, 
                             ntests = ntests, 
                             VE = paste0(round(VE_est, digits = 2), "% (", 
                                         round(min(VE_CI), digits = 2), "% to ", 
                                         round(max(VE_CI), digits = 2), "%)"), 
                             pval = round(pval, digits = 4))

write.csv(results_model3, "/home/amoor53/APSVE_cluster/results/2021_age_5_11_TND_03_model_fit.csv")
