

# APSVE Testing Data Cleaning
# Amy Moore
# Last Update: 2026-11-02

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

# library(readr) #Read in Files
library(lubridate) #Date Objects
library(dplyr) #Tidy Data
library(tidyverse) # Data Manipulation
library(tidyr) # Data Manipulation
library(slider) # Calendar Time Handling
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs



print("")
print("")
print("------------------------------")
print("Begin File 1: Read in Raw Data")
print("------------------------------")
print("")
print("")


##### Read in Raw Data Files #####


if (cluster) {
  source("/home/amoor53/APSVE_cluster/code/00_config.R")
  set.seed(cfg$seed)   # reproducible per task
    #VS testing results by student ID
  vstests_deid_raw <- read.csv("/home/amoor53/APSVE_cluster/data/vstests_newid_5.6.25.csv")
    #Vaccination records by Student ID
  aps_vax <- read.csv("/home/amoor53/APSVE_cluster/data/aps_vax.csv")
    #Demographic info by Student ID
  studentinfo_deid <- read.csv("/home/amoor53/APSVE_cluster/data/studentinfo_newid_5.6.25.csv")
    #School Enrollment by Student ID
  enrollment_newid <- read.csv("/home/amoor53/APSVE_cluster/data/enrollment_newid_5.6.25.csv")
    #Direct Certification
  Direct_Certification_2021 <- read.csv("/home/amoor53/APSVE_cluster/data/2022_directly_certified_school.csv")
  Direct_Certification_2022 <- read.csv("/home/amoor53/APSVE_cluster/data/2023_directly_certified_school.csv")
    #Schools by Cluster
  APS_School_List <- read.csv("/home/amoor53/APSVE_cluster/data/APS School List.csv")
} else {
  source(here("code", "00_config.R"))
  set.seed(cfg$seed)   # reproducible per task
    #VS testing results by student ID
  vstests_deid_raw <- read.csv(here("data", "vstests_newid_5.6.25.csv"))
    #Vaccination records by Student ID
  aps_vax <- read.csv(here("data", "aps_vax.csv"))
    #Demographic info by Student ID
  studentinfo_deid <- read.csv(here("data", "studentinfo_newid_5.6.25.csv"))
    #School Enrollment by Student ID
  enrollment_newid <- read.csv(here("data", "enrollment_newid_5.6.25.csv"))
    #Direct Certification
  Direct_Certification_2021 <- read.csv(here("data", "2022_directly_certified_school.csv"))
  Direct_Certification_2022 <- read.csv(here("data", "2023_directly_certified_school.csv"))
    #Schools by Cluster
  APS_School_List <- read.csv(here("data", "APS School List.csv"))
}




  #Define Sample Size Table 
sample_sizes <- data.frame(cohort = c("2021-22 Age 5-11 Full Cohort",
                                      "2021-22 Age 5-11 Super Testers",
                                      "2021-22 Age 12-18 Full Cohort",
                                      "2021-22 Age 12-18 Super Testers",
                                      "2022-23 Age 5-11 Full Cohort",
                                      "2022-23 Age 5-11 Super Testers",
                                      "2022-23 Age 12-18 Full Cohort",
                                      "2022-23 Age 12-18 Super Testers"),
                           precleaning = rep(NA, 8),
                           eligible = rep(NA, 8),
                           school_year = rep(NA, 8),
                           age_group = rep(NA, 8),
                           super_testers = rep(NA, 8),
                           matched = rep(NA, 8))

testing_numbers <- data.frame(cohort = c("2021-22 Age 5-11 Full Cohort",
                                      "2021-22 Age 5-11 Super Testers",
                                      "2021-22 Age 12-18 Full Cohort",
                                      "2021-22 Age 12-18 Super Testers",
                                      "2022-23 Age 5-11 Full Cohort",
                                      "2022-23 Age 5-11 Super Testers",
                                      "2022-23 Age 12-18 Full Cohort",
                                      "2022-23 Age 12-18 Super Testers"),
                              precleaning = rep(NA,8),
                              eligible = rep(NA, 8),
                              school_year = rep(NA, 8),
                              age_group = rep(NA, 8),
                              super_testers = rep(NA, 8),
                              matched = rep(NA, 8))
ntests <- nrow(vstests_deid_raw)
nIDs <- length(unique(vstests_deid_raw$ID))

print("")
print("------------------------------")
print("Intial Raw Data")
print("------------------------------")
print("")

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))

sample_sizes$precleaning <- nIDs
testing_numbers$precleaning <- ntests


##### Cleaning Step 1: Specific School year and remove missing IDs #####

print("")
print("------------------------------")
print("Step 1: Remove NA IDs & Summer Break testing")
print("------------------------------")
print("")


start_date_2021 <- ymd("2021-09-07") #First day of VS testing
end_date_2022 <- ymd("2022-05-26") #Last day of School
first_sunday_2021 <- as.numeric(ymd("2021-09-05"))

start_date_2022 <- ymd("2022-08-01")
end_date_2023 <- ymd("2023-05-24")
first_sunday_2022 <- as.numeric(ymd("2022-07-31"))

#Filter out tests not during the specified school year and with missing student IDs
vstests_deid <- vstests_deid_raw %>% 
  filter(!is.na(ID)) %>% #No NA IDs
  filter(resultdate >= start_date_2021 & #No tests before start
         resultdate <= end_date_2023 & #No tests after end
         !(resultdate > end_date_2022 & resultdate < start_date_2022) #No tests between years
         ) %>%
  mutate(school_year = case_when( #add school year 
    resultdate >= start_date_2021 & resultdate <= end_date_2022 ~ "2021-22",
    resultdate >= start_date_2022 & resultdate <= end_date_2023 ~ "2022-23",
    .default = NA),
    result = case_when( #recode test results as binary 0/1
      result == "POSITIVE" ~ 1,
      result == "NEGATIVE" ~ 0,
      .default = NA),
    resultdate = as.Date(resultdate, orgin = "1970-01-01"),
    dob = as.Date(dob, orgin = "1970-01-01")
    ) %>%
  select(ID, dob, resultdate, result, schoolname, school_year)

ntests <- nrow(vstests_deid) 
VScohort_IDs <- unique(vstests_deid$ID) 
nIDs <- length(VScohort_IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))






##### Cleaning Step 2: Merge with Vaccination and Demographic Records #####

print("")
print("------------------------------")
print("Step 2: Merge with Vaccination and Demographic Records")
print("------------------------------")
print("")

doses <- cfg$doses

dose_col <- paste0("ADMIN_DATE_2_", cfg$doses)

#Find Vaccination Records for the students reporting tests
VScohort_vax <- aps_vax %>%
  rename(ID = ID.x) %>%
  filter(ID %in% VScohort_IDs) %>%
  select(ID, vax_date = all_of(dose_col)) %>%
  mutate(vax_date = as.Date(vax_date, origin = "1970-01-01")) 
length(unique(VScohort_vax$ID)) 



#Find Demographic info for the students reporting tests
VScohort_demographics <- studentinfo_deid %>%
  filter(ID %in% VScohort_IDs) %>%
  mutate(race = case_when(
    fed_race_ethnicity_name == "White" ~ "White",
    fed_race_ethnicity_name == "Black or African American" ~ "Black",
    fed_race_ethnicity_name == "Asian" ~ "Asian",
    fed_race_ethnicity_name == "Hispanic/Latino" ~ "Hispanic",
    fed_race_ethnicity_name == "American Indian or Alaska Native" ~ "Other",
    fed_race_ethnicity_name == "Native Hawaiian or Other Pacific Islander" ~ "Other",
    fed_race_ethnicity_name == "Two or more races" ~ "Other",
    .default = NA), 
    birthdate = as.Date(birthdate, origin = "1970-01-01")
    ) %>%
  select(ID, race, gender, birthdate, zip_most_recent)
length(unique(VScohort_demographics$ID)) 

#merge the 3 datasets --> keep students with missing vax records (assigns them as unvaxed --> vax_date = NA)
VScohort_merged <- merge(vstests_deid,
                         merge(VScohort_vax,
                               VScohort_demographics, 
                               by = "ID", 
                               all.y = TRUE), 
                         by = "ID")

#Removing tests when dob did not match from test to demographic records
VScohort_merged <- VScohort_merged %>%
  mutate(check = ifelse(birthdate == dob, 1,0)) %>%
  filter(check == 1) %>%
  select(ID, birthdate, gender, race, schoolname, school_year,
         resultdate, result, vax_date, zip_most_recent)


ntests <- nrow(VScohort_merged) 
VScohort_IDs <- unique(VScohort_merged$ID) 
nIDs <- length(VScohort_IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))











##### Cleaning Step 3 - Only Tests from Primary Enrollment Schools #####

print("")
print("------------------------------")
print("Step 3: Only Tests from Primary Enrollment Schools")
print("------------------------------")
print("")


  # Create a list of each school and the number of primary enrolled students who reported tests
    # Exclude North Metro --> Primary School for Disabilities that does not report direct certification
    # Fix the symbol mismatch for Best MS-HS
primary_schools <- enrollment_newid %>%
  filter(servicetype == "P" & !is.na(school) & ID %in% VScohort_IDs & school != "North Metro") %>% 
  group_by(school) %>%
  summarize(enrollment = n()) %>%
  mutate(school = ifelse(school == "BEST MS/HS", "BEST MS-HS", school))

  #Create a vector of all of the schools that are both primary enrolled and reporting tests
schools_to_include <- intersect(primary_schools$school,
                                unique(VScohort_merged$schoolname))

VScohort_merged <- VScohort_merged %>%
  filter(schoolname %in% schools_to_include)

ntests <- nrow(VScohort_merged) 
VScohort_IDs <- unique(VScohort_merged$ID) 
nIDs <- length(VScohort_IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))


##### Cleaning Step 4 - No Transfer Students #####

print("")
print("------------------------------")
print("Step 4: No Transfer Students")
print("------------------------------")
print("")

  #Collecting list of students with multiple enrollments during the same year
multi_school_ids <- VScohort_merged %>%
  distinct(ID, school_year, schoolname) %>%
  count(ID, school_year, name = "n_schools") %>%
  filter(n_schools > 1) %>%
  select(ID, school_year)

  #Removing transfer students
VScohort_merged <- VScohort_merged %>%
  anti_join(multi_school_ids, by = c("ID", "school_year"))

ntests <- nrow(VScohort_merged) 
VScohort_IDs <- unique(VScohort_merged$ID) 
nIDs <- length(VScohort_IDs) 


print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))







##### Cleaning Step 5 - Adding Age Group #####

print("")
print("------------------------------")
print("Step 5: Add in Age Group to subdivide subcohorts")
print("------------------------------")
print("")

  #Age is calculated as of the start date for the year (you cannot jump to the other cohort mid-year)
    #For each test, age is calculated using the birth date and the start of the study that year
    #Age group is sorted as 5-11 or 12-18
VScohort_merged <- VScohort_merged %>%
  mutate(age = ifelse(school_year == "2021-22", 
                      trunc((birthdate %--% start_date_2021) / years(1)),
                      trunc((birthdate %--% start_date_2022) / years(1))),
         age_group = case_when(
           age < 5 ~ "Under 5",
           age >= 5 & age <=11 ~ "5-11",
           age >= 12 & age <=18 ~"12-18",
           age > 18 ~ "Over 18",
           .default = NA),
         vax_status = case_when(
           is.na(vax_date) ~ 0,
           resultdate < vax_date ~ 0,
           resultdate >= vax_date ~ 1,
           .default = NA),
         ever_vax_status = case_when(
           is.na(vax_date) ~ 0,
           school_year == "2021-22" & end_date_2022 <= vax_date ~ 0,
           school_year == "2021-22" & end_date_2022 > vax_date ~ 1,
           school_year == "2022-23" & end_date_2023 <= vax_date ~ 0,
           school_year == "2022-23" & end_date_2023 > vax_date ~ 1,
           .default = NA),
         subcohort = paste0(school_year, " Age ", age_group)) %>%
  filter(age_group %in% c("5-11", "12-18"))


ntests <- nrow(VScohort_merged) 
VScohort_IDs <- unique(VScohort_merged$ID) 
nIDs <- length(VScohort_IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))



##### Cleaning Step 6 - Only Individuals with at least X tests at vax status #####

min_tests <- cfg$min_tests

print("")
print("------------------------------")
print(paste0("Step 6: At least ", min_tests," tests per student"))
print("------------------------------")
print("")

  #Count Up tests by ID by vax status
id_counts <- VScohort_merged %>%
  count(ID, subcohort, vax_status, name = "n") %>%
  pivot_wider(names_from = vax_status, values_from = n, values_fill = 0) %>%
  rename(Unvaccinated = "0",
         Vaccinated = "1") %>%
  left_join(
    VScohort_merged %>% distinct(ID, subcohort, ever_vax_status),
    by = c("ID", "subcohort")
  )

  #Sort into four groups
    #Vaccinated = At least min tests post vaccination (include in Vax group)
    #Unvaccinated = At least min tests reported and no vaccination (include in Unvax group)
    #Unvaccinated (Censor at vax) = At least min tests before vax, but few after (include in Unvax group, censor at vax date)
    #Excluded = Less than min tests both pre vax and post vax
id_eligibility <- id_counts %>%
  mutate(
    final_group = case_when(
      ever_vax_status == 0 & Unvaccinated >= min_tests                             ~ "Unvaccinated",
      ever_vax_status == 1 & Vaccinated >= min_tests                               ~ "Vaccinated",
      ever_vax_status == 1 & Vaccinated < min_tests & Unvaccinated >= min_tests    ~ "Unvaccinated (censored at vax date)",
      TRUE                                                                         ~ "Excluded"
    )
  )


  #Apply classfication onto the testing data
VScohort_merged <- VScohort_merged %>%
  inner_join(id_eligibility %>% select(ID, subcohort, final_group),
             by = c("ID", "subcohort")) %>%
  filter(final_group != "Excluded") %>%
  mutate(censor_post_vax = ifelse(final_group == "Unvaccinated (censored at vax date)" & vax_status == 1, 
                                  1,0), #Censor at vax if <3 tests after vax, but keep for testing behavior
         censor_pre_vax = ifelse(final_group == "Vaccinated" & vax_status == 0,
                                 1,0)) #Only include post-vax tests if >3 after vax, but keep for testing behavior


ntests <- nrow(VScohort_merged) 
VScohort_IDs <- unique(VScohort_merged$ID) 
nIDs <- length(VScohort_IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))



##### Cleaning Step 7 - Only School With Active Participation #####

print("")
print("------------------------------")
print("Step 7: Only School With Active Participation")
print("------------------------------")
print("")

school_min_IDs <- cfg$school_min_IDs
school_min_tests <- cfg$school_min_tests


  #Part 1: Flagging Schools in subcohorts with no matching ability 
    #either not enough vaccinated or unvaccinated students to be considered active

  #Summarizing each person as vaccinated or unvaccinated for each school year
id_match_group <- VScohort_merged %>%
  group_by(ID, subcohort, schoolname) %>%
  summarise(ever_vaccinated = any(vax_status == 1), .groups = "drop") %>%
  mutate(match_group = if_else(ever_vaccinated, "Vaccinated", "Never vaccinated"))

  #Summarizing by school --> counting number of never vaccinated versus vaccinated during that year
school_id_counts <- id_match_group %>%
  count(subcohort, schoolname, match_group, name = "n_individuals") %>%
  pivot_wider(names_from = match_group, values_from = n_individuals, values_fill = 0)

  #Flagging Schools with either not enough vaccinated or unvaccinated students to be considered active
schools_no_pair <- school_id_counts %>%
  filter(Vaccinated < school_min_IDs | `Never vaccinated` < school_min_IDs) %>%
  select(subcohort, schoolname)

  
  #Part 2: Flagging schools with less than 10 tests overall

  #Only counting tests after vaccination for vaccinated students
VScohort_postvax <- VScohort_merged %>%
  filter(vax_status == ever_vax_status)


school_counts <- VScohort_postvax %>%
  count(subcohort, schoolname, vax_status, name = "n_tests")

school_counts_wide <- school_counts %>%
  pivot_wider(names_from = vax_status, values_from = n_tests, values_fill = 0)

schools_to_drop <- school_counts_wide %>%
  filter(if_any(-c(subcohort, schoolname), ~ . < school_min_tests)) %>%
  select(subcohort, schoolname)

schools_to_drop_all <- bind_rows(schools_to_drop, schools_no_pair) %>%
  distinct(subcohort, schoolname)

VScohort_merged <- VScohort_merged %>%
  anti_join(schools_to_drop_all, by = c("subcohort", "schoolname"))


ntests <- nrow(VScohort_merged) 
VScohort_IDs <- unique(VScohort_merged$ID) 
nIDs <- length(VScohort_IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))



##### Cleaning Step 8 - Only where covariates have positivity #####

print("")
print("------------------------------")
print("Step 8: Only where covariates have positivity")
print("------------------------------")
print("")

  #Update the eligible IDs with school cuts
id_eligibility <- id_eligibility %>%
  semi_join(VScohort_merged, by = c("ID", "subcohort"))

  #Create list of IDs with their covariates
covariate_pool <- id_eligibility %>%
  filter(final_group != "Excluded") %>%
  mutate(match_pool = if_else(final_group == "Vaccinated", "Vaccinated", "Unvaccinated")) %>%
  select(ID, subcohort, match_pool) %>%
  left_join(
    VScohort_merged %>% distinct(ID, subcohort, race, gender, age),
    by = c("ID", "subcohort")
  )

  #Function to check covariate balance
check_covariate_balance <- function(data, covariate) {
  data %>%
    count(subcohort, match_pool, .data[[covariate]]) %>%
    pivot_wider(names_from = match_pool, values_from = n, values_fill = 0) %>%
    filter(Vaccinated < 1 | Unvaccinated < 1) %>%
    mutate(covariate = covariate) %>%
    rename(level = {{ covariate }}) %>%
    select(covariate, level, subcohort, Vaccinated, Unvaccinated)
}

  #Check Covariate Balance across race and gender
covariates_to_check <- c("race", "gender")

balance_flags <- map_dfr(covariates_to_check, ~ check_covariate_balance(covariate_pool, .x))

balance_flags



VScohort_merged <- VScohort_merged %>%
  filter(gender != "N")

ntests <- nrow(VScohort_merged) 
VScohort_IDs <- unique(VScohort_merged$ID) 
nIDs <- length(VScohort_IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))













##### Cleaning Step 9 - Add direct certification #####

print("")
print("------------------------------")
print("Step 9: Merge in Direct Certification Covariate")
print("------------------------------")
print("")

  #Combine files across years
dir_cert_combined <- bind_rows(
  Direct_Certification_2021 %>% mutate(school_year = "2021-22"),
  Direct_Certification_2022 %>% mutate(school_year = "2022-23")
)

dir_cert_combined <- dir_cert_combined %>%
  filter(SYSTEM_NAME == "Atlanta Public Schools") %>%
  select(SCHOOL_ID, direct_cert_perc, school_year) %>%
  rename(school_num = SCHOOL_ID,
         dir_cert = direct_cert_perc)

APS_School_List <- APS_School_List %>%
  select(X., School) %>%
  rename(school_num = X.,
         schoolname = School) %>%
  mutate(schoolname = ifelse(schoolname == "BEST MS/HS", "BEST MS-HS", schoolname))

VScohort_merged <- VScohort_merged %>%
  left_join(APS_School_List %>% select(schoolname, school_num),
            by = "schoolname") %>%
  left_join(dir_cert_combined %>% select(school_num, school_year, dir_cert),
            by = c("school_num", "school_year"))

ntests <- nrow(VScohort_merged) 
VScohort_IDs <- unique(VScohort_merged$ID) 
nIDs <- length(VScohort_IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))

sample_sizes$eligible <- nIDs
testing_numbers$eligible <- ntests

summary_sample_size <- VScohort_merged %>%
  group_by(school_year) %>%
  summarize(
    n_tests = n(),
    n_IDs = length(unique(ID))
  )

sample_sizes$school_year <- c(rep(summary_sample_size[[1,3]], 4),
                              rep(summary_sample_size[[2,3]], 4))
testing_numbers$school_year <- c(rep(summary_sample_size[[1,2]], 4),
                              rep(summary_sample_size[[2,2]], 4))

summary_sample_size <- VScohort_merged %>%
  group_by(subcohort) %>%
  summarize(
    n_tests = n(),
    n_IDs = length(unique(ID))
  )

sample_sizes$age_group <- c(rep(summary_sample_size[[2,3]], 2),
                            rep(summary_sample_size[[1,3]], 2),
                            rep(summary_sample_size[[4,3]], 2),
                            rep(summary_sample_size[[3,3]], 2))
testing_numbers$age_group <- c(rep(summary_sample_size[[2,2]], 2),
                               rep(summary_sample_size[[1,2]], 2),
                               rep(summary_sample_size[[4,2]], 2),
                               rep(summary_sample_size[[3,2]], 2))

print("")
print("------------------------------")
print("Saving Final Dataset & Sample Size Table")
print("------------------------------")
print("")


if(cluster) {
  saveRDS(VScohort_merged, "/home/amoor53/APSVE_cluster/cleandata/All_cohort_daily_testing_by_ID.rds")
  saveRDS(sample_sizes, "/home/amoor53/APSVE_cluster/results/cleaning_sample_sizes.rds")
  saveRDS(testing_numbers, "/home/amoor53/APSVE_cluster/results/cleaning_testing_numbers.rds")
} else {
  saveRDS(VScohort_merged, here("cleandata", "All_cohort_daily_testing_by_ID.rds"))
  saveRDS(sample_sizes, here("results", "cleaning_sample_sizes.rds"))
  saveRDS(testing_numbers, here("results", "cleaning_testing_numbers.rds"))
}

print("")
print("------------------------------")
print("File Complete")
print("------------------------------")
print("")