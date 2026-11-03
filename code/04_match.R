##### File #4 - Creating Matched data sets #####

print("")
print("")
print("------------------------------")
print("Begin File 4: Super Tester Subset and Matching")
print("------------------------------")
print("")
print("")


#Run Locally?
# cluster = TRUE
cluster = FALSE

#Extra Print outs
verbose = TRUE
# verbose = FALSE

#Package Library Location
if (cluster) {
  .libPaths("~/Rlibs")
} else {
  library(here) # File Locations
}

library(readr) #Read in Files
library(lubridate) #Date Objects
library(dplyr) #Tidy Data
library(tidyverse) # Data Manipulation
library(tidyr) # Data Manipulation
library(slider) # Calendar Time Handling
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs
library(MatchIt) #Matching Pairs


if(cluster){
  source("/home/amoor53/APSVE_cluster/code/00_config.R")
  task <- get_task(analysis_grid)
  set.seed(cfg$seed + match(task$subcohort, subcohorts))   # reproducible per task
  VScohort_weekly <- readRDS(paste0("/home/amoor53/APSVE_cluster/cleandata/", task$subcohort, "_weekly_propensity.rds"))
} else {
  source(here("code", "00_config.R"))
  task <- get_task(analysis_grid)
  set.seed(cfg$seed + match(task$subcohort, subcohorts))   # reproducible per task
  VScohort_weekly <- readRDS(here("cleandata", paste0(task$subcohort, "_weekly_propensity.rds")))
}

print("")
print("------------------------------")
print("Step 1: Create a by ID dataset")
print("------------------------------")
print("")



#Compute Testing Completeness for each ID
VScohort_by_ID <- VScohort_weekly %>% 
  group_by(ID) %>%
  summarize(n_weeks_available = n(),
            n_weeks_tested = sum(tested),
            testing_completeness = n_weeks_tested/n_weeks_available,
            first_test_week = sum(first_test_indicator))

demographics <- VScohort_weekly %>%
  distinct(ID, school_year, birthdate, gender, race, schoolname, dir_cert, vax_date, final_group, age, age_group, subcohort)

VScohort_by_ID <- demographics %>%
  left_join(VScohort_by_ID, by = "ID") %>%
  mutate(vax_status = case_when(
    final_group == "Unvaccinated" ~ 0,
    final_group == "Unvaccinated (censored at vax date)" ~ 0,
    final_group == "Vaccinated" ~ 1,
    .default = NA
  ))



if (task$cohort_type == "super_tester") {
  
  print("")
  print("------------------------------")
  print("Step 1b: Subset to Super Tester Cohort")
  print("------------------------------")
  print("")
  
  #Using Global parameter to include in sensitivity analyses
  ST_cutoff <- cfg$supertester_cutoff
  
  #Select IDs that meet super tester completeness rating
  STcohort_by_ID <- VScohort_by_ID %>%
    filter(testing_completeness >= 1 - ST_cutoff)
  
  #Select Weekly testing data from Super Tester IDs
  VScohort_weekly <- VScohort_weekly %>%
    filter(ID %in% STcohort_by_ID$ID)
  
  if (cluster) {
    saveRDS(VScohort_weekly,
            file = paste0("/home/amoor53/APSVE_cluster/cleandata/", task$subcohort, "_ST_weekly_propensity.rds"))
  } else {
    saveRDS(VScohort_weekly,
            file = here("cleandata", paste0(task$subcohort, "_ST_weekly_propensity.rds")))
  }
}




print("")
print("------------------------------")
print("Step 2: Match Cohort")
print("------------------------------")
print("")


if (verbose) {
  #diagnostics of the prematch dataset
  print("")
  print("Checking Age")
  print("")
  table(VScohort_by_ID$age)
  
  print("")
  print("Checking Vax Status")
  print("")
  table(VScohort_by_ID$vax_status)
  
  print("")
  print("Checking Vax Status by Age")
  print("")
  table(VScohort_by_ID$vax_status, VScohort_by_ID$age)
  
  print("")
  print("Checking by School")
  print("")
  table(VScohort_by_ID$schoolname)
  
  print("")
  print("Number of Students")
  print("")
  nrow(VScohort_by_ID)
  
  print("")
  print("Checking Max Number of Students that could be matched")
  print("")
  estimate_of_match <- table(VScohort_by_ID$schoolname, VScohort_by_ID$vax_status)
  max_n_of_match <- apply(estimate_of_match, 1, min)
  2*sum(max_n_of_match)
}

VScohort_match_raw <- matchit(formula = vax_status ~ schoolname + first_test_week,
                          data = VScohort_by_ID,
                          method = "nearest",
                          distance = "glm",
                          replace = FALSE,
                          exact = ~schoolname
)

VScohort_match <- match.data(VScohort_match_raw) %>% arrange(subclass, vax_status)

VScohort_match <- VScohort_match %>% select(!c("distance", "weights"))

if (verbose) {
  print("")
  print("Checking Matched Cohort Sample Sizes")
  print("")
  ntests <- sum(VScohort_match$n_weeks_tested)
  nIDs <-   nrow(VScohort_match)
  print(paste0("Number of Tests: ", ntests))
  print(paste0("Number of Students: ", nIDs))
  
}

print("")
print("------------------------------")
print("Step 3: Assigning Enrollment based on Matches")
print("------------------------------")
print("")


VScohort_enrollment_date <- VScohort_match %>%
  filter(vax_status == 1) %>%
  mutate(enrollment_case = case_when(
    vax_date > task$start_date ~ "Vax during study",
    vax_date <= task$start_date ~ "Vax before study",
    .default = "Error"
  ),
  enrollment_date = case_when(
    enrollment_case == "Vax during study" ~ vax_date,
    enrollment_case == "Vax before study" ~ task$start_date,
    enrollment_case == "Error" ~ NA,
    .default = NA
  )) %>%
  select(subclass, enrollment_case, enrollment_date)

VScohort_match_simple <- VScohort_match %>%
  left_join(VScohort_enrollment_date, by = "subclass") %>%
  select(ID, subclass, enrollment_date)

  #Reduce Weekly test data to only include matched data 
    #Merge in match and enroll date
    #Exclude unmatched IDs
VScohort_weekly_match <- VScohort_weekly %>%
  left_join(VScohort_match_simple, by = "ID") %>%
  filter(!is.na(subclass))

#Checking that both matches have tests
checking <- VScohort_weekly_match %>% group_by(subclass) %>% summarize(n_IDs = length(unique(ID))) %>% filter(n_IDs < 2)
nrow(checking)

  #Reduce Weekly test data to only include eligible tests
    #Exclude weeks from before enrollment
    #Exclude weeks for late vaxers who are used as unvaxed and censored at vaccination
    #Exclude weeks when no testing occured
VScohort_weekly_match_observed <- VScohort_weekly_match %>%
  filter(enrollment_date <= week_start_date) %>%
  filter(final_group != "Unvaccinated (censored at vax date)" |
           week_start_date < vax_date) %>%
  filter(tested == 1)

  #Checking that both matches have tests
checking <- VScohort_weekly_match_observed %>% group_by(subclass) %>% summarize(n_IDs = length(unique(ID))) %>% filter(n_IDs < 2)
nrow(checking)

  #Removing matched pairs where one member has no tests after enrollment
VScohort_weekly_match_observed <- VScohort_weekly_match_observed %>%
  filter(!(subclass %in% checking$subclass))


  #Reduce Weekly test data to censor after first positive result
    #Collect Week number of first positive for each with at least one positive
first_positive_events <- VScohort_weekly_match_observed %>%
  filter(positive == 1) %>%
  group_by(ID) %>%
  summarise(first_positive_week = min(week, na.rm = TRUE), .groups = "drop")

VScohort_weekly_match_observed <- VScohort_weekly_match_observed %>%
  left_join(first_positive_events, by = "ID") %>%
  filter(is.na(first_positive_week) | week <= first_positive_week)

VScohort_weekly_match_observed <- VScohort_weekly_match_observed %>%
  mutate(prior_week = week - 1) %>%
  select(ID, subcohort, school_year, age_group,
         schoolname, gender, race, age, continuous_age,
         dir_cert, prior_infections,
         subclass, vax_status, vax_date, enrollment_date,
         prior_week, week, week_start_date, avail_index,
         num_tests, tested, positive, 
         weeks_since_last_test, tests_last_4weeks,
         test_density, first_test_indicator,
         propensity)


if (cluster) {
  saveRDS(VScohort_weekly_match_observed,
          file = paste0("/home/amoor53/APSVE_cluster/cleandata/", task$subcohort, "_", task$cohort_type, "_matched.rds"))
} else {
  saveRDS(VScohort_weekly_match_observed,
          file = here("cleandata", paste0(task$subcohort, "_", task$cohort_type, "_matched.rds")))
}

print("")
print("")
print("")
print("------------------------------")
print("------------------------------")
print("End File 4: Saving Matched Data")
print("------------------------------")
print("------------------------------")
print("")
print("")
print("")




