
  #Run this file Locally

library(readr) #Read in Files
library(here) # File Locations
library(tidyverse) # Data Manipulation
library(ggplot2) # plotting
library(table1) #pretty tables


VScohort.merged <- read_csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))

start_date <- ymd("2021-09-07") #First day of VS testing
end_date <- ymd("2022-05-26") #Last day of School
first_sunday <- as.numeric(ymd("2021-09-05")) - 7


head(VScohort.merged)

VScohort.IDs <- sort(unique(VScohort.merged$ID))

length(VScohort.IDs)

#Swapping into individual as experimental unit instead of test

VScohort.byID <- data.frame(ID = VScohort.IDs, Age = NA, Race = NA, Gender = NA, Dir_cert = NA, Vax_status = NA, Vax_date = NA, Prior_Infections = NA, Num_tests = NA, Case = NA, School = NA, flag = 0)  

for (i in 1:length(VScohort.IDs)) {
  # for (i in 1:10) {
  
  ID <- VScohort.IDs[i]
  
  # ID <- 90026842 #vaxed example
  # ID <- 90000007 #unvaxed example
  
  tests <- VScohort.merged[which(VScohort.merged$ID == ID),]
  flag <- 0
  
  #Age is as of the start date of the school year
  age <- unique(tests$start_age)
  if (length(age) != 1) {
    flag <- 1
    age <- NA
  }
  
  #Race reported for each test
  race <- unique(tests$race)
  if (length(race) != 1) {
    flag <- 1
    race <- NA
  }
  
  #Gender reported for each test
  gender <- unique(tests$gender)
  if (length(gender) != 1) {
    flag <- 1
    gender <- NA
  }
  
  #Direct Certification (socio-economic indicator)
  dir_cert <- unique(tests$dir_cert)
  if (length(dir_cert) != 1) {
    flag <- 1
    dir_cert <- NA
  }
  
  #vax_status may change throughout trial period, this determines if they reported a test during the year after being vaccinated
  vax_status <- max(tests$vax_status)
  
  #vax_date is reported for every test if it exists
  vax_date <- unique(tests$vax_date)
  if (length(vax_date) != 1) {
    flag <- 1
    vax_date <- start_date
  }
  
  #Prior infections for each test
  prior_infect <- max(tests$prior_infections)
  
  #Number of tests reported
  num_tests <- sum(tests$tested)
  
  #1 if any tests are positive
  case <- max(tests$result)
  
  #School reported for each test
  school <- unique(tests$schoolname)
  if (length(school) != 1) {
    flag <- 1
    school <- NA
  }
  
  VScohort.byID[i,] <- c(ID, age, race, gender, dir_cert, vax_status, vax_date, prior_infect, num_tests, case, school, flag)
  
}

VScohort.byID$ID <- as.numeric(VScohort.byID$ID)
VScohort.byID$Age <- as.numeric(VScohort.byID$Age)
VScohort.byID$Dir_cert <- as.numeric(VScohort.byID$Dir_cert)
VScohort.byID$Vax_status <- as.numeric(VScohort.byID$Vax_status)
VScohort.byID$Prior_Infections <- as.numeric(VScohort.byID$Prior_Infections)
VScohort.byID$Num_tests <- as.numeric(VScohort.byID$Num_tests)
VScohort.byID$Case <- as.numeric(VScohort.byID$Case)

#N
n <- nrow(VScohort.byID)
n_tests <- sum(VScohort.byID$Num_tests)

#Age
age_mean <- mean(VScohort.byID$Age)
age_sd <- sd(VScohort.byID$Age)
age_summary <- paste0(round(age_mean, digits = 2), " (", round(age_sd, digits = 2), ")")

#Race
race_black_n <- nrow(VScohort.byID[which(VScohort.byID$Race == "Black or African American"),])
race_black_summary <- paste0(race_black_n, " (", round(race_black_n/n * 100, digits = 2), "%)")

race_white_n <- nrow(VScohort.byID[which(VScohort.byID$Race == "White"),])
race_white_summary <- paste0(race_white_n, " (", round(race_white_n/n * 100, digits = 2), "%)")

race_asian_n <- nrow(VScohort.byID[which(VScohort.byID$Race == "Asian"),])
race_asian_summary <- paste0(race_asian_n, " (", round(race_asian_n/n * 100, digits = 2), "%)")

race_hisp_n <- nrow(VScohort.byID[which(VScohort.byID$Race == "Hispanic/Latino"),])
race_hisp_summary <- paste0(race_hisp_n, " (", round(race_hisp_n/n * 100, digits = 2), "%)")

race_other_n <- nrow(VScohort.byID[which(VScohort.byID$Race == "Other"),])
race_other_summary <- paste0(race_other_n, " (", round(race_other_n/n * 100, digits = 2), "%)")

#Gender
gender_F_n <- nrow(VScohort.byID[which(VScohort.byID$Gender == "F"),])
gender_F_summary <- paste0(gender_F_n, " (", round(gender_F_n/n * 100, digits = 2), "%)")

gender_M_n <- nrow(VScohort.byID[which(VScohort.byID$Gender == "M"),])
gender_M_summary <- paste0(gender_M_n, " (", round(gender_M_n/n * 100, digits = 2), "%)")

#Direct Certification
dc_mean <- mean(VScohort.byID$Dir_cert)
dc_sd <- sd(VScohort.byID$Dir_cert)
dc_summary <- paste0(round(dc_mean, digits = 2), " (", round(dc_sd, digits = 2), ")")

#Vaccination Status
vaxed_n <- nrow(VScohort.byID[which(VScohort.byID$Vax_status == 1),])
vaxed_summary <- paste0(vaxed_n, " (", round(vaxed_n/n * 100, digits = 2), "%)")

unvaxed_n <- nrow(VScohort.byID[which(VScohort.byID$Vax_status == 0),])
unvaxed_summary <- paste0(unvaxed_n, " (", round(unvaxed_n/n * 100, digits = 2), "%)")

#Prior Infections
infect_n <- nrow(VScohort.byID[which(VScohort.byID$Prior_Infections > 0),])
infect_summary <- paste0(infect_n, " (", round(infect_n/n * 100, digits = 2), "%)")

noinfect_n <- nrow(VScohort.byID[which(VScohort.byID$Prior_Infections == 0),])
noinfect_summary <- paste0(noinfect_n, " (", round(noinfect_n/n * 100, digits = 2), "%)")

#Num_tests
num_tests_mean <- mean(VScohort.byID$Num_tests)
num_tests_sd <- sd(VScohort.byID$Num_tests)
num_tests_summary <- paste0(round(num_tests_mean, digits = 2), " (", round(num_tests_sd, digits = 2), ")")


#Cases
cases_n <- nrow(VScohort.byID[which(VScohort.byID$Case == 1),])
cases_summary <- paste0(cases_n, " (", round(cases_n/n * 100, digits = 2), "%)")

controls_n <- nrow(VScohort.byID[which(VScohort.byID$Case == 0),])
controls_summary <- paste0(controls_n, " (", round(controls_n/n * 100, digits = 2), "%)")

#Positivity
# positives_n <- nrow(VScohort.merged[which(VScohort.merged$result == 1),])
# positivity_summary <- paste0(positives_n, " (", round(positives_n/n * 100, digits = 2), "%)")




table1 <- data.frame(Ages5to11_2021 = c(n, n_tests, num_tests_summary,
                                        "", vaxed_summary, unvaxed_summary,
                                        "", cases_summary, controls_summary, 
                                        age_summary, 
                                        "", race_black_summary, race_white_summary, race_asian_summary, race_hisp_summary, race_other_summary, 
                                        "", gender_F_summary, gender_M_summary, 
                                        dc_summary, 
                                        "", infect_summary, noinfect_summary))

covariates <- c("N", "N tests", "Number of Tests per student - mean (SD)", 
                "Vaccination Status - n (%)", "Vaccinated", "Unvaccinated",
                "Case Recorded - n (%)", "Case", "Control", 
                "Age - mean (SD)", 
                "Race - n (%)", "Black or African American", "White", "Asian", "Hispanic/Latino", "Other",
                "Gender - n (%)", "Female", "Male",
                "Direct Certification - mean (SD)",
                "Prior Infections - n (%)", "At Least 1", "None")

rownames(table1) <- covariates

table1

write.csv(table1, here("results", "2021_age_5_11_table1.csv"))


