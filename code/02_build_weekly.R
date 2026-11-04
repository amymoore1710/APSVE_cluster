

# APSVE Weekly Data Cleaning
# Amy Moore
# Last Update: 2026-11-02

print("")
print("")
print("------------------------------")
print("Begin File 2: Transition to Weekly Testing Summaries")
print("------------------------------")
print("")
print("")


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

library(readr) #Read in Files
library(lubridate) #Date Objects
library(dplyr) #Tidy Data
library(tidyverse) # Data Manipulation
library(tidyr) # Data Manipulation
library(slider) # Calendar Time Handling
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs



if (cluster) {
  #Cleaned VS Testing Data
  VScohort_merged <- readRDS("/home/amoor53/APSVE_cluster/cleandata/All_cohort_daily_testing_by_ID.rds")
  #SendSS cases
  SendSS_tests <- read_csv("/home/amoor53/APSVE_cluster/data/pui_deid.csv")
} else {
  #Cleaned VS Testing Data
  VScohort_merged <- readRDS(here("cleandata", "All_cohort_daily_testing_by_ID.rds"))
  #SendSS cases
  SendSS_tests <- read_csv(here("data","pui_deid.csv"))
}

print("")
print("------------------------------")
print("Step 1: Create Weekly Grid")
print("------------------------------")
print("")


start_date_2021 <- ymd("2021-09-07") #First day of VS testing
end_date_2022 <- ymd("2022-05-26") #Last day of School
first_sunday_2021 <- as.numeric(ymd("2021-09-05"))

start_date_2022 <- ymd("2022-08-01")
end_date_2023 <- ymd("2023-05-24")
first_sunday_2022 <- as.numeric(ymd("2022-07-31"))


#Defining the weeks of testing for each school year
school_year_dates <- tibble::tibble(
  school_year = c("2021-22", "2022-23"),
  start_date  = c(first_sunday_2021, first_sunday_2022),
  n_weeks     = c(ceiling((as.numeric(end_date_2022) - first_sunday_2021)/7), 
                  ceiling((as.numeric(end_date_2023) - first_sunday_2022)/7))   
)

#Assign each test a school year start date
VScohort_weekly_tests <- VScohort_merged %>%
  left_join(school_year_dates, by = "school_year") %>%
  mutate(week = as.integer(floor(as.numeric(as.Date(resultdate) - start_date) / 7) + 1)) 

#Calculate num_tests reported per week, if tested, and if positive by ID
weekly_agg <- VScohort_weekly_tests %>%
  group_by(ID, school_year, week) %>%
  summarise(
    num_tests = n(),
    tested    = as.integer(num_tests > 0),
    positive  = as.integer(any(result == 1)),
    .groups = "drop"
  )

#Pull in the demographic data
demographics <- VScohort_merged %>%
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
VScohort_weekly <- full_grid %>%
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



print("")
print("------------------------------")
print("Step 2: Add Prior Infections Indicator")
print("------------------------------")
print("")



#Adding in prior infections variable

VS_positives <- VScohort_merged %>%
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
#   left_join(VScohort_demographics %>% select(ID, birthdate), by = "ID") %>%
#   mutate(match_dob = (sendss_dob == birthdate))

#Filter to just the first positive test for each person
first_positive <- all_positive_events %>%
  group_by(ID) %>%
  summarise(first_positive_date = min(event_date, na.rm = TRUE), .groups = "drop")

#Add a week start date that corresponds to the sunday before the week of testing
VScohort_weekly <- VScohort_weekly %>%
  left_join(school_year_dates %>% select(school_year, start_date), by = "school_year") %>%
  mutate(week_start_date = as.Date(start_date + (week - 1) * 7,origin = "1970-01-01")) %>%
  select(-start_date)

#Add in prior infection indicator
VScohort_weekly <- VScohort_weekly %>%
  left_join(first_positive, by = "ID") %>%
  mutate(
    prior_infections = as.integer(!is.na(first_positive_date) &
                                    week_start_date > first_positive_date)
  ) 




#Adding in time varying testing behavior

print("")
print("------------------------------")
print("Step 3: Add Time Varying Testing Behavior")
print("------------------------------")
print("")


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
VScohort_weekly <- VScohort_weekly %>%
  left_join(calendar, by = c("school_year", "week"))

#Compute Rolling testing behavior measures

# work only on available weeks — avail_index is a dense 1..n sequence here,
# one row per ID x school_year x available week (blackout weeks dropped for now)
avail_only <- VScohort_weekly %>%
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
VScohort_weekly <- VScohort_weekly %>%
  left_join(avail_only, by = c("ID", "school_year", "week")) %>%
  arrange(ID, school_year, week) %>%
  group_by(ID, school_year) %>%
  fill(weeks_since_last_test, tests_last_4weeks, test_density, first_test_indicator,
       .direction = "down") %>%
  ungroup()

  #Remove weeks with no testing available & fix time varying NAs
VScohort_weekly <- VScohort_weekly  %>%
  filter(is_available == TRUE) %>%
  mutate(weeks_since_last_test = coalesce(weeks_since_last_test, 0),
         test_density = coalesce(test_density, 0),
         continuous_age = as.numeric(week_start_date - birthdate) / 365.25,
         vax_status = ifelse(is.na(vax_date), 0,
                             ifelse(as.numeric(week_start_date - vax_date) > 0, 1, 0)))

print("")
print("------------------------------")
print("Step 3: Split into Subcohorts")
print("------------------------------")
print("")


VScohort_weekly_2021_5to11 <- VScohort_weekly %>% filter(subcohort == "2021-22 Age 5-11")
VScohort_weekly_2021_12to18 <- VScohort_weekly %>% filter(subcohort == "2021-22 Age 12-18")
VScohort_weekly_2022_5to11 <- VScohort_weekly %>% filter(subcohort == "2022-23 Age 5-11")
VScohort_weekly_2022_12to18 <- VScohort_weekly %>% filter(subcohort == "2022-23 Age 12-18")


#Save Dataset
if(cluster) {
  saveRDS(VScohort_weekly, "/home/amoor53/APSVE_cluster/cleandata/All_cohort_weekly_testing_by_ID.rds")
  saveRDS(VScohort_weekly_2021_5to11, "/home/amoor53/APSVE_cluster/cleandata/2021_5_11_weekly_testing_by_ID.rds")
  saveRDS(VScohort_weekly_2021_12to18, "/home/amoor53/APSVE_cluster/cleandata/2021_12_18_weekly_testing_by_ID.rds")
  saveRDS(VScohort_weekly_2022_5to11, "/home/amoor53/APSVE_cluster/cleandata/2022_5_11_weekly_testing_by_ID.rds")
  saveRDS(VScohort_weekly_2022_12to18, "/home/amoor53/APSVE_cluster/cleandata/2022_12_18_weekly_testing_by_ID.rds")
} else {
  saveRDS(VScohort_weekly, here("cleandata", "All_cohort_weekly_testing_by_ID.rds"))
  saveRDS(VScohort_weekly_2021_5to11, here("cleandata", "2021_5_11_weekly_testing_by_ID.rds"))
  saveRDS(VScohort_weekly_2021_12to18, here("cleandata", "2021_12_18_weekly_testing_by_ID.rds"))
  saveRDS(VScohort_weekly_2022_5to11, here("cleandata", "2022_5_11_weekly_testing_by_ID.rds"))
  saveRDS(VScohort_weekly_2022_12to18, here("cleandata", "2022_12_18_weekly_testing_by_ID.rds"))
}


print("File Summary")

print(head(VScohort_weekly))

print("")
print("")

ntests <- sum(VScohort_weekly$num_tests)
print(paste("Number of tests:", ntests))
nIDs <- length(unique(VScohort_weekly$ID))
print(paste("Number of tests:", nIDs))

print("Testing by week by subcohort 0 = missing, 1 = tested")

print(table(VScohort_weekly$week, VScohort_weekly$subcohort, VScohort_weekly$tested))

print("")
print("------------------------------")
print("File Complete")
print("------------------------------")
print("")

