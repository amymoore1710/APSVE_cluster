# APS VE summaries
# 2021-22 Ages 5-11 
# 2025-11-20
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

sub_cohorts <- data.frame(year = c("2021", "2021", "2022", "2022"),
                          age = c("5_11", "12_18", "5_11", "12_18"))

results_table <- data.frame(X = rep(1, 48),
                            method = NA,
                            nstudents = NA,
                            ntests = NA,
                            VE = NA,
                            pval = NA)

for (j in 1:nrow(sub_cohorts)) {
  year <- sub_cohorts[j,]$year
  age <- sub_cohorts[j,]$age
  
  #Load parameter lists
  if (cluster) {
    params_list_TND <- read_csv(paste0("/home/amoor53/APSVE_cluster/cleandata/", year, "_age_", age, "_TND_params.csv"))
    params_list_TTE <- read_csv(paste0("/home/amoor53/APSVE_cluster/cleandata/", year, "_age_", age, "_TTE_params.csv"))
  } else {
    params_list_TND <- read.csv(here("cleandata", paste0(year, "_age_", age, "_TND_params.csv")))
    params_list_TTE <- read.csv(here("cleandata",paste0(year, "_age_", age, "_TTE_params.csv")))
  }
  
  params_list <- rbind(params_list_TTE, params_list_TND)
  
  for (i in 1:nrow(params_list)) {
    method <- params_list$method[i]
    if (cluster) {
      results_table[(12*(j-1) +i),] <- read.csv(paste0("/home/amoor53/APSVE_cluster/results/", year, "_age_", age, "_", method, "_model_fit.csv"))
    } else {
      results_table[(12*(j-1) +i),] <- read.csv(here("results", paste0(year, "_age_", age, "_", method, "_model_fit.csv")))
    }
  }
}







if (cluster) {
  write.csv(results_table, "/home/amoor53/APSVE_cluster/results/together_model_results.csv")
} else {
  write.csv(results_table, file = here("results", "together_model_results.csv"))
}