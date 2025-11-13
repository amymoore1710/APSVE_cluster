
# APS VE TTE Model Fitting Parallel
# 2021-22 Ages 5-11 
# 2025-11-13
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
source("/home/amoor53/APSVE_cluster/code/functions/fit_TTE_model.R")

model_num <- commandArgs(trailingOnly = TRUE)[[1]]

#Read in the list of parameters
params_list <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_TTE_params.csv")

#select only the parameters for this model_num index
params <- params_list[model_num,]

#convert the parameter strings into objects (load the specified data set, convert to formula, leave as is)
data <- read_csv(paste0("/home/amoor53/APSVE_cluster/cleandata/", params$data_file))
model_formula <- as.formula(params$model_formula)
method <- params$method


results <- fit_TND_model(data = data, 
                         model_formula = model_formula, 
                         method = method)

saveRDS(results, file = paste0("/home/amoor53/APSVE_cluster/models/2021_age_5_11_", method, "_model.rds"))
write.csv(results[[2]], paste0("/home/amoor53/APSVE_cluster/results/2021_age_5_11_", method, "_model_fit.csv"))



