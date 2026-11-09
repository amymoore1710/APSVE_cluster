# Setting Up Parameter Options

#Run Locally?
cluster = TRUE
# cluster = FALSE

#Extra Print outs
verbose = TRUE
# verbose = FALSE

#Package Library Location
if (cluster) {
  .libPaths("~/Rlibs")
} else {
  library(here) # File Locations
}

library(dplyr)
library(tidyr)

#Sensitivity Parameters
  #Seed --> Randomization set for reproducability
  #Supertester_cutoff --> between 0 and 1 for what percent of missingness is allowed
  #Min_tests --> between 0 and 34 for the minimum number of tests reported to be included
  #School_min_IDs --> minimum participation from a school
  #School_min_tests --> minimum participation from a school
  #Doses --> between 1 and 5 for how many doses recorded means vaccinated (and the date of vaccination with final dose)
cfg <- list(seed = 2026, supertester_cutoff = 0.15, min_tests = 3, 
            school_min_IDs = 1, school_min_tests = 10, doses = 1)

subcohorts <- c("2021_5_11", "2021_12_18", "2022_5_11", "2022_12_18")


ps_grid       <- tibble(subcohort = subcohorts)
analysis_grid <- expand_grid(subcohort = subcohorts,
                             cohort_type = c("full", "super_tester"))

analysis_grid <- analysis_grid %>%
  mutate(start_dates = c(rep(ymd("2021-09-07"), 4), rep(ymd("2022-08-01"),4)),
         end_dates = c(rep(ymd("2022-05-26"), 4), rep(ymd("2023-05-24"),4)),
         first_sunday = c(rep(ymd("2021-09-05"), 4), rep(ymd("2022-07-31"),4)))

model_grid <- expand_grid(subcohort = subcohorts,
                          cohort_type = c("full", "super_tester", "full")) %>%
  mutate(model_method = rep(c("Standard", "Standard", "Weighted"), 4))


# Maps the SLURM array index to a row of the grid; defaults to task 1 so you can test interactively
get_task <- function(grid) {
  id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = "1"))
  stopifnot(id >= 1, id <= nrow(grid))
  as.list(grid[id, ])
}
