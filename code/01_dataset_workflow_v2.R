

# APSVE Full Data Set Workflow
# Modified to run on the cluster
# Amy Moore

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
    #SendSS cases
  SendSS_tests <- read.csv("/home/amoor53/APSVE_cluster/data/pui_deid.csv")
} else {
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
    #SendSS cases
  SendSS_tests <- read.csv(here("data","pui_deid.csv"))
}

  #Fixing a mis-matched symbol
APS_School_List[which(APS_School_List$School == "BEST MS/HS"),]$School <- "BEST MS-HS"




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
VScohort.IDs <- unique(vstests_deid$ID) 
nIDs <- length(VScohort.IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))






##### Cleaning Step 2: Merge with Vaccination and Demographic Records #####

print("")
print("------------------------------")
print("Step 2: Merge with Vaccination and Demographic Records")
print("------------------------------")
print("")


#Find Vaccination Records for the students reporting tests
VScohort.vax <- aps_vax %>%
  rename(ID = ID.x) %>%
  filter(ID %in% VScohort.IDs) %>%
  mutate(vax_date = as.Date(ADMIN_DATE_2_1, origin = "1970-01-01")) %>%
  select(ID, vax_date)
length(unique(VScohort.vax$ID)) 



#Find Demographic info for the students reporting tests
VScohort.demographics <- studentinfo_deid %>%
  filter(ID %in% VScohort.IDs) %>%
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
length(unique(VScohort.demographics$ID)) 

#merge the 3 datasets --> keep students with missing vax records (assigns them as unvaxed --> vax_date = NA)
VScohort.merged <- merge(vstests_deid,
                         merge(VScohort.vax,
                               VScohort.demographics, 
                               by = "ID", 
                               all.y = TRUE), 
                         by = "ID")

#Removing tests when dob did not match from test to demographic records
VScohort.merged <- VScohort.merged %>%
  mutate(check = ifelse(birthdate == dob, 1,0)) %>%
  filter(check == 1) %>%
  select(ID, birthdate, gender, race, schoolname, school_year,
         resultdate, result, vax_date, zip_most_recent)


ntests <- nrow(VScohort.merged) 
VScohort.IDs <- unique(VScohort.merged$ID) 
nIDs <- length(VScohort.IDs) 

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
  filter(servicetype == "P" & !is.na(school) & ID %in% VScohort.IDs & school != "North Metro") %>% 
  group_by(school) %>%
  summarize(enrollment = n()) %>%
  mutate(school = ifelse(school == "BEST MS/HS", "BEST MS-HS", school))

  #Create a vector of all of the schools that are both primary enrolled and reporting tests
schools_to_include <- intersect(primary_schools$school,
                                unique(VScohort.merged$schoolname))

VScohort.merged <- VScohort.merged %>%
  filter(schoolname %in% schools_to_include)

ntests <- nrow(VScohort.merged) 
VScohort.IDs <- unique(VScohort.merged$ID) 
nIDs <- length(VScohort.IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))

  #Collecting list of students with multiple enrollments during the same year
multi_school_ids <- VScohort.merged %>%
  distinct(ID, school_year, schoolname) %>%
  count(ID, school_year, name = "n_schools") %>%
  filter(n_schools > 1) %>%
  select(ID, school_year)

  #Removing transfer students
VScohort.merged <- VScohort.merged %>%
  anti_join(multi_school_ids, by = c("ID", "school_year"))

ntests <- nrow(VScohort.merged) 
VScohort.IDs <- unique(VScohort.merged$ID) 
nIDs <- length(VScohort.IDs) 


print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))







##### Cleaning Step 4 - Adding Age Group #####

print("")
print("------------------------------")
print("Step 4: Add in Age Group to subdivide subcohorts")
print("------------------------------")
print("")

  #Age is calculated as of the start date for the year (you cannot jump to the other cohort mid-year)
    #For each test, age is calculated using the birth date and the start of the study that year
    #Age group is sorted as 5-11 or 12-18
VScohort.merged <- VScohort.merged %>%
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
           resultdate <= vax_date ~ 0,
           resultdate > vax_date ~ 1,
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


ntests <- nrow(VScohort.merged) 
VScohort.IDs <- unique(VScohort.merged$ID) 
nIDs <- length(VScohort.IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))



##### Cleaning Step 5 - Only Individuals with at least 3 tests at vax status #####

  #Count Up tests by ID by vax status
id_counts <- VScohort.merged %>%
  count(ID, subcohort, vax_status, name = "n") %>%
  pivot_wider(names_from = vax_status, values_from = n, values_fill = 0) %>%
  rename(Unvaccinated = "0",
         Vaccinated = "1") %>%
  left_join(
    VScohort.merged %>% distinct(ID, subcohort, ever_vax_status),
    by = c("ID", "subcohort")
  )

  #Sort into four groups
    #Vaccinated = At least 3 tests post vaccination (include in Vax group)
    #Unvaccinated = At least 3 tests reported and no vaccination (include in Unvax group)
    #Unvaccinated (Censor at vax) = At least 3 tests before vax, but few after (include in Unvax group, censor at vax date)
    #Excluded = Less than 3 tests both pre vax and post vax
id_eligibility <- id_counts %>%
  mutate(
    final_group = case_when(
      ever_vax_status == 0 & Unvaccinated >= 3                     ~ "Unvaccinated",
      ever_vax_status == 1 & Vaccinated >= 3                       ~ "Vaccinated",
      ever_vax_status == 1 & Vaccinated < 3 & Unvaccinated >= 3    ~ "Unvaccinated (censored at vax date)",
      TRUE                                                          ~ "Excluded"
    )
  )


  #Apply classfication onto the testing data
VScohort.merged <- VScohort.merged %>%
  inner_join(id_eligibility %>% select(ID, subcohort, final_group),
             by = c("ID", "subcohort")) %>%
  filter(final_group != "Excluded") %>%
  mutate(censor_post_vax = ifelse(final_group == "Unvaccinated (censored at vax date)" & vax_status == 1, 
                                  1,0), #Censor at vax if <3 tests after vax, but keep for testing behavior
         censor_pre_vax = ifelse(final_group == "Vaccinated" & vax_status == 0,
                                 1,0)) #Only include post-vax tests if >3 after vax, but keep for testing behavior


ntests <- nrow(VScohort.merged) 
VScohort.IDs <- unique(VScohort.merged$ID) 
nIDs <- length(VScohort.IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))



##### Cleaning Step 5 - Only schools reporting >10 tests #####

print("")
print("------------------------------")
print("Step 5: Only schools reporting >10 tests")
print("------------------------------")
print("")

if (verbose) {
  print("")
  print("Checking Vaccination status")
  print("")
  table(VScohort.merged$vax_status)
}

if (verbose) { 
  print("")
  print("Checking Vaccination Status by Result")
  print("")
  table(VScohort.merged$vax_status, VScohort.merged$result)
}


  #Part 1: Flagging Schools in subcohorts with no matching ability 
    #Either no vaccinated students or no never vaccinated students

  #Summarizing each person as vaccinated or unvaccinated for each school year
id_match_group <- VScohort.merged %>%
  group_by(ID, subcohort, schoolname) %>%
  summarise(ever_vaccinated = any(vax_status == 1), .groups = "drop") %>%
  mutate(match_group = if_else(ever_vaccinated, "Vaccinated", "Never vaccinated"))

  #Summarizing by school --> counting number of never vaccinated versus vaccinated during that year
school_id_counts <- id_match_group %>%
  count(subcohort, schoolname, match_group, name = "n_individuals") %>%
  pivot_wider(names_from = match_group, values_from = n_individuals, values_fill = 0)

  #Flagging Schools with either no vaccinated or no never vaccinated individuals
schools_no_pair <- school_id_counts %>%
  filter(Vaccinated < 1 | `Never vaccinated` < 1) %>%
  select(subcohort, schoolname)

  
  #Part 2: Flagging schools with less than 10 tests overall

  #Only counting tests after vaccination for vaccinated students
VScohort.postvax <- VScohort.merged %>%
  filter(vax_status == ever_vax_status)


school_counts <- VScohort.postvax %>%
  count(subcohort, schoolname, vax_status, name = "n_tests")

school_counts_wide <- school_counts %>%
  pivot_wider(names_from = vax_status, values_from = n_tests, values_fill = 0)

schools_to_drop <- school_counts_wide %>%
  filter(if_any(-c(subcohort, schoolname), ~ . < 10)) %>%
  select(subcohort, schoolname)

schools_to_drop_all <- bind_rows(schools_to_drop, schools_no_pair) %>%
  distinct(subcohort, schoolname)

VScohort.merged <- VScohort.merged %>%
  anti_join(schools_to_drop_all, by = c("subcohort", "schoolname"))


ntests <- nrow(VScohort.merged) 
VScohort.IDs <- unique(VScohort.merged$ID) 
nIDs <- length(VScohort.IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))



##### Cleaning Step 7 - Only where covariates have positivity #####

print("")
print("------------------------------")
print("Step 7: Only where covariates have positivity")
print("------------------------------")
print("")

  #Update the eligible IDs with school cuts
id_eligibility <- id_eligibility %>%
  semi_join(VScohort.merged, by = c("ID", "subcohort"))

  #Create list of IDs with their covariates
covariate_pool <- id_eligibility %>%
  filter(final_group != "Excluded") %>%
  mutate(match_pool = if_else(final_group == "Vaccinated", "Vaccinated", "Unvaccinated")) %>%
  select(ID, subcohort, match_pool) %>%
  left_join(
    VScohort.merged %>% distinct(ID, subcohort, race, gender, age),
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



VScohort.merged <- VScohort.merged %>%
  filter(gender != "N")

ntests <- nrow(VScohort.merged) 
VScohort.IDs <- unique(VScohort.merged$ID) 
nIDs <- length(VScohort.IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))













##### Cleaning Step 8 - Add direct certification #####

print("")
print("------------------------------")
print("Step 8: Create additional covariates")
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

VScohort.merged <- VScohort.merged %>%
  left_join(APS_School_List %>% select(schoolname, school_num),
            by = "schoolname") %>%
  left_join(dir_cert_combined %>% select(school_num, school_year, dir_cert),
            by = c("school_num", "school_year"))

ntests <- nrow(VScohort.merged) 
VScohort.IDs <- unique(VScohort.merged$ID) 
nIDs <- length(VScohort.IDs) 

print(paste0("Number of Tests: ", ntests))
print(paste0("Number of Students: ", nIDs))

sample_sizes$eligible <- nIDs
testing_numbers$eligible <- ntests

summary_sample_size <- VScohort.merged %>%
  group_by(school_year) %>%
  summarize(
    n_tests = n(),
    n_IDs = length(unique(ID))
  )

sample_sizes$school_year <- c(rep(summary_sample_size[[1,3]], 4),
                              rep(summary_sample_size[[2,3]], 4))
testing_numbers$school_year <- c(rep(summary_sample_size[[1,2]], 4),
                              rep(summary_sample_size[[2,2]], 4))

summary_sample_size <- VScohort.merged %>%
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


if(cluster) {
  write.csv(VScohort.merged, "/home/amoor53/APSVE_cluster/cleandata/All_cohort_daily_testing_by_ID.csv")
} else {
  write.csv(VScohort.merged, here("cleandata", "All_cohort_daily_testing_by_ID.csv"))
}

# VScohort.merged <- read.csv(here("cleandata", "All_cohort_daily_testing_by_ID.csv"))







print("")
print("")
print("")
print("------------------------------")
print("------------------------------")
print("End File 1: Saving Daily testing by ID")
print("------------------------------")
print("------------------------------")
print("")
print("")
print("")



##### Start of File #2 #####

print("")
print("")
print("------------------------------")
print("Begin File 2: Transition to Weekly Testing Summaries")
print("------------------------------")
print("")
print("")


print("")
print("------------------------------")
print("Step 1: Assign tests to a week of the school year")
print("------------------------------")
print("")


  #Defining the weeks of testing for each school year
school_year_dates <- tibble::tibble(
  school_year = c("2021-22", "2022-23"),
  start_date  = c(first_sunday_2021, first_sunday_2022),
  n_weeks     = c(ceiling((as.numeric(end_date_2022) - first_sunday_2021)/7), 
                  ceiling((as.numeric(end_date_2023) - first_sunday_2022)/7))   
)

  #Assign each test a school year start date
VScohort.weekly_tests <- VScohort.merged %>%
  left_join(school_year_dates, by = "school_year") %>%
  mutate(week = as.integer(floor(as.numeric(as.Date(resultdate) - start_date) / 7) + 1)) 

  #Calculate num_tests reported per week, if tested, and if positive by ID
weekly_agg <- VScohort.weekly_tests %>%
  group_by(ID, school_year, week) %>%
  summarise(
    num_tests = n(),
    tested    = as.integer(num_tests > 0),
    positive  = as.integer(any(result == 1)),
    .groups = "drop"
  )

  #Pull in the demographic data
demographics <- VScohort.merged %>%
  distinct(ID, school_year, birthdate, gender, race, schoolname, dir_cert, vax_date, final_group, age, age_group, subcohort)

  #Add demographics to ID by week grid
full_grid <- demographics %>%
  left_join(school_year_dates %>% select(school_year, n_weeks), by = "school_year") %>%
  group_by(ID, school_year) 

  #Create a grid of ID by week test results
full_grid <- full_grid %>%
  uncount(n_weeks, .remove = FALSE) %>% 
  group_by(ID, school_year) %>% 
  mutate(week = row_number()) %>%
  ungroup() %>%
  rename(max_weeks = n_weeks)

  #Add in the weekly testing records to week by ID grid
VScohort.weekly <- full_grid %>%
  left_join(weekly_agg, by = c("ID", "school_year", "week")) %>%
  mutate(
    num_tests = replace_na(num_tests, 0),
    tested    = replace_na(tested, 0L),
    positive  = replace_na(positive, 0L)
  ) %>%
  select(ID, school_year, week, birthdate, gender, race, 
         schoolname, dir_cert, vax_date,
         final_group, age, age_group, subcohort,
         num_tests, tested, positive)



  #Adding in prior infections variable
  
VS_positives <- VScohort.merged %>%
  filter(result == 1) %>%
  select(ID, event_date = resultdate, birthdate) %>%
  mutate(event_date = as.Date(event_date, orgin = "1970-01-01"),
         birthdate = as.Date(birthdate, origin = "1970-01-01"))

sendss_positives <- SendSS_tests %>%
  select(ID, event_date = result_date, birthdate = DOB) %>%
  mutate(event_date = as.Date(event_date, orgin = "1970-01-01"),
         birthdate = as.Date(birthdate, origin = "1970-01-01"))

all_positive_events <- bind_rows(VS_positives, sendss_positives) %>%
  distinct() %>%
  filter(!is.na(ID))

  #Check to ensure date of birth matches ID from sendss positives
# check_dob <- all_positive_events %>%
#   rename(sendss_dob = birthdate) %>%
#   left_join(VScohort.demographics %>% select(ID, birthdate), by = "ID") %>%
#   mutate(match_dob = (sendss_dob == birthdate))

  #Filter to just the first positive test for each person
first_positive <- all_positive_events %>%
  group_by(ID) %>%
  summarise(first_positive_date = min(event_date, na.rm = TRUE), .groups = "drop")

  #Add a week start date that corresponds to the sunday before the week of testing
VScohort.weekly <- VScohort.weekly %>%
  left_join(school_year_dates %>% select(school_year, start_date), by = "school_year") %>%
  mutate(week_start_date = as.Date(start_date + (week - 1) * 7,origin = "1970-01-01")) %>%
  select(-start_date)

  #Add in prior infection indicator
VScohort.weekly <- VScohort.weekly %>%
  left_join(first_positive, by = "ID") %>%
  mutate(
    prior_infections = as.integer(!is.na(first_positive_date) &
                                    week_start_date > first_positive_date)
  ) 




  #Adding in time varying testing behavior


  #Building the calendar to exclude weeks when testing was not available
blackout_weeks <- tibble::tibble(
  school_year = c(rep("2021-22", 4), rep("2022-23", 6)),
  week        = c(12, 16, 17, 31,      11, 17, 21, 22, 30, 36)
)

calendar <- school_year_dates %>%                     # from earlier: school_year, n_weeks
  group_by(school_year) %>%
  reframe(week = 1:n_weeks[1]) %>%
  left_join(blackout_weeks %>% mutate(is_available = FALSE), by = c("school_year", "week")) %>%
  mutate(is_available = coalesce(is_available, TRUE)) %>%
  group_by(school_year) %>%
  mutate(avail_index = cumsum(is_available)) %>%        # freezes during blackout weeks — see note below
  ungroup()

  #Join the calendar onto weekly testing summary
VScohort.weekly <- VScohort.weekly %>%
  left_join(calendar, by = c("school_year", "week"))

  #Compute Rolling testing behavior measures

# work only on available weeks — avail_index is a dense 1..n sequence here,
# one row per ID x school_year x available week (blackout weeks dropped for now)
avail_only <- VScohort.weekly %>%
  filter(is_available) %>%
  arrange(ID, school_year, week) %>%
  group_by(ID, school_year) %>%
  mutate(
    # most recent avail_index with a positive test, STRICTLY before this row
    tested_idx_if_tested = if_else(tested == 1, avail_index, NA_integer_),
    last_tested_idx      = lag(cummax(coalesce(tested_idx_if_tested, -Inf))),
    first_test_indicator = as.integer(is.na(last_tested_idx) | last_tested_idx == -Inf),
    
    weeks_since_last_test = if_else(first_test_indicator == 1, NA_real_,
                                    avail_index - last_tested_idx),
    
    tests_last_4weeks = slide_dbl(num_tests, sum, .before = 3, .after = -1, .complete = FALSE),
    
    cum_tests_prior = lag(cumsum(num_tests), default = 0),
    n_avail_prior   = avail_index - 1,
    test_density    = if_else(n_avail_prior == 0, NA_real_, cum_tests_prior / n_avail_prior)
  ) %>%
  ungroup() %>%
  select(ID, school_year, week, weeks_since_last_test, tests_last_4weeks,
         test_density, first_test_indicator)

  #Join back to the full weekly grid 
VScohort.weekly <- VScohort.weekly %>%
  left_join(avail_only, by = c("ID", "school_year", "week")) %>%
  arrange(ID, school_year, week) %>%
  group_by(ID, school_year) %>%
  fill(weeks_since_last_test, tests_last_4weeks, test_density, first_test_indicator,
       .direction = "down") %>%
  ungroup()



#Save Dataset
if(cluster) {
  write.csv(VScohort.weekly, "/home/amoor53/APSVE_cluster/cleandata/All_cohort_weekly_testing_by_ID.csv")
} else {
  write.csv(VScohort.weekly, here("cleandata", "All_cohort_weekly_testing_by_ID.csv"))
}


print("")
print("")
print("")
print("------------------------------")
print("------------------------------")
print("End File 2: Saving Weekly testing by ID")
print("------------------------------")
print("------------------------------")
print("")
print("")
print("")




















