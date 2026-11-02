
# APS VE summaries
# 2022-23 Ages 5-11 
# 2025-12-03
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

year <- "2022"
age <- "5_11"

  #Load parameter lists
if (cluster) {
  params_list_TND <- read_csv(paste0("/home/amoor53/APSVE_cluster/cleandata/", year, "_age_", age, "_TND_params.csv"))
  params_list_TTE <- read_csv(paste0("/home/amoor53/APSVE_cluster/cleandata/", year, "_age_", age, "_TTE_params.csv"))
} else {
  params_list_TND <- read.csv(here("cleandata", paste0(year, "_age_", age, "_TND_params.csv")))
  params_list_TTE <- read.csv(here("cleandata",paste0(year, "_age_", age, "_TTE_params.csv")))
}

params_list <- rbind(params_list_TTE, params_list_TND)


results_table <- data.frame(X = 1,
                            method = NA,
                            nstudents = NA,
                            ntests = NA,
                            VE = NA,
                            pval = NA)

for (i in 1:nrow(params_list)) {
  method <- params_list$method[i]
  if (cluster) {
    results_table[i,] <- read.csv(paste0("/home/amoor53/APSVE_cluster/results/", year, "_age_", age, "_", method, "_model_fit.csv"))
  } else {
    results_table[i,] <- read.csv(here("results", paste0(year, "_age_", age, "_", method, "_model_fit.csv")))
  }
}

if (cluster) {
  write.csv(results_table, paste0("/home/amoor53/APSVE_cluster/results/", year, "_age_", age, "_model_results.csv"))
} else {
  write.csv(results_table, file = here("results", paste0(year, "_age_", age, "_model_results.csv")))
}


