
# APS VE TTE Model Fitting Parallel
# 2022-23 Ages 5-11 
# 2025-11-24
# Modified to run on the cluster
# Amy Moore


#Package Library Location
.libPaths("~/Rlibs")

library(readr) #Read in Files
library(tidyverse) # Data Manipulation
library(lubridate) #Dates
library(survival) #Cox Model
library(coxme) #Cox Model w/ RE

#Model fitting function
source("/home/amoor53/APSVE_cluster/code/functions/fit_TTE_model.R")

model_num <- commandArgs(trailingOnly = TRUE)[[1]]

year <- "2022"
age <- "5_11"

#Read in the list of parameters
params_list <- read_csv(paste0("/home/amoor53/APSVE_cluster/cleandata/", year, "_age_", age, "_TTE_params.csv"))

#select only the parameters for this model_num index
params <- params_list[model_num,]

print("")
print("")
print("------------------------------")
print("Begin File: Fit Models")
print("------------------------------")
print("")
print("")
print(params)

#convert the parameter strings into objects (load the specified data set, convert to formula, leave as is)
data <- read_csv(paste0("/home/amoor53/APSVE_cluster/cleandata/", params$data_file))
model_formula <- as.formula(params$model_formula)
method <- params$method


results <- fit_TTE_model(data = data, 
                         model_formula = model_formula, 
                         method = method)

saveRDS(results, file = paste0("/home/amoor53/APSVE_cluster/models/", year, "_age_", age, "_", method, "_model.rds"))
write.csv(results[[2]], paste0("/home/amoor53/APSVE_cluster/results/", year, "_age_", age, "_", method, "_model_fit.csv"))

print("")
print("")
print("------------------------------")
print("End File: Model Fit and Saved")
print("------------------------------")
print("")
print("")

