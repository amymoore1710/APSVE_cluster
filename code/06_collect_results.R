
  #APSVE
  #Collect Results from cluster runs of all modeling

library(here)

#Toggle to save results
# saving <- TRUE
saving <- FALSE


  #Combining Sample Size table with matched datasets 

sample_sizes <- readRDS(here("results", "cleaning_sample_sizes.RDS"))

# by_ID_2021_5_11  <- readRDS(here("cleandata", "2021_5_11_full_by_ID.rds"))
# by_ID_2022_5_11  <- readRDS(here("cleandata", "2022_5_11_full_by_ID.rds"))
# by_ID_2021_12_18 <- readRDS(here("cleandata", "2021_12_18_full_by_ID.rds"))
# by_ID_2022_12_18 <- readRDS(here("cleandata", "2022_12_18_full_by_ID.rds"))
# 
# by_ID_ST_2021_5_11  <- readRDS(here("cleandata", "2021_5_11_super_tester_by_ID.rds"))
# by_ID_ST_2022_5_11  <- readRDS(here("cleandata", "2022_5_11_super_tester_by_ID.rds"))
# by_ID_ST_2021_12_18 <- readRDS(here("cleandata", "2021_12_18_super_tester_by_ID.rds"))
# by_ID_ST_2022_12_18 <- readRDS(here("cleandata", "2022_12_18_super_tester_by_ID.rds"))
# 
# 
# matched_2021_5_11  <- readRDS(here("cleandata", "2021_5_11_full_matched.rds"))
# matched_2022_5_11  <- readRDS(here("cleandata", "2022_5_11_full_matched.rds"))
# matched_2021_12_18 <- readRDS(here("cleandata", "2021_12_18_full_matched.rds"))
# matched_2022_12_18 <- readRDS(here("cleandata", "2022_12_18_full_matched.rds"))
# 
# matched_ST_2021_5_11  <- readRDS(here("cleandata", "2021_5_11_super_tester_matched.rds"))
# matched_ST_2022_5_11  <- readRDS(here("cleandata", "2022_5_11_super_tester_matched.rds"))
# matched_ST_2021_12_18 <- readRDS(here("cleandata", "2021_12_18_super_tester_matched.rds"))
# matched_ST_2022_12_18 <- readRDS(here("cleandata", "2022_12_18_super_tester_matched.rds"))

cohort_groups <- c("2021_5_11_full", "2021_5_11_super_tester",
                   "2021_12_18_full", "2021_12_18_super_tester",
                   "2022_5_11_full", "2022_5_11_super_tester",
                   "2022_12_18_full", "2022_12_18_super_tester")

for(i in 1:8) {
  cohort <- cohort_groups[i]
  sample_sizes$super_testers[i] <- nrow(readRDS(here("cleandata", paste0(cohort, "_by_ID.rds"))))
  matched <- readRDS(here("cleandata", paste0(cohort, "_matched.rds")))
  sample_sizes$matched[i] <- length(unique(matched$ID))
}

if (saving) {
  saveRDS(sample_sizes, here("results", "sample_sizes_complete.RDS"))
}



