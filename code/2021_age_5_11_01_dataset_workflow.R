

# APSVE Full Data Set Workflow
# 2021-22 Ages 5-11 
# 2025-10-21
# Modified to run on the cluster
# Amy Moore

  #Package Library Location
.libPaths("~/Rlibs")

library(readr) #Read in Files
library(lubridate) #Date Objects
library(dplyr) #Tidy Data
library(tidyverse) # Data Manipulation
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs




##### Read in Raw Data Files #####


  #VS testing results by student ID
vstests_deid_raw <- read_csv("/home/amoor53/APSVE_cluster/data/vstests_newid_5.6.25.csv")

  #Vaccination records by Student ID
aps_vax <- read_csv("/home/amoor53/APSVE_cluster/data/aps_vax.csv")

  #Demographic info by Student ID
studentinfo_deid <- read_csv("/home/amoor53/APSVE_cluster/data/studentinfo_newid_5.6.25.csv")

  #School Enrollment by Student ID
enrollment_newid <- read_csv("/home/amoor53/APSVE_cluster/data/enrollment_newid_5.6.25.csv")

  #Direct Certification
Direct_Certification <- read_csv("/home/amoor53/APSVE_cluster/data/2022_directly_certified_school.csv")

  #Schools by Cluster
APS_School_List <- read_csv("/home/amoor53/APSVE_cluster/data/APS School List.csv")
APS_School_List[which(APS_School_List$School == "BEST MS/HS"),]$School <- "BEST MS-HS"

  #SendSS cases
SendSS_tests <- read_csv("/home/amoor53/APSVE_cluster/data/pui_deid.csv")



  #Define Sample Size Table 
ntests <- nrow(vstests_deid_raw)
nIDs <- length(unique(vstests_deid_raw$ID))
sample.sizes <- data.frame(raw_data = c(ntests, nIDs),
                           school_year_21_22 = NA,
                           primary_enroll = NA,
                           ages_5_11 = NA,
                           school_sample_size_large = NA,
                           gender = NA,
                           no_transfers = NA,
                           more_than_3_tests = NA,
                           only_1_per_week = NA, 
                           more_than_3_weeks = NA)
rownames(sample.sizes) <- c("ntests", "nIDs")

sample.sizes


##### Cleaning Step 1: Specific School year and remove missing IDs #####


start_date <- ymd("2021-09-07") #First day of VS testing
end_date <- ymd("2022-05-26") #Last day of School
first_sunday <- as.numeric(ymd("2021-09-05")) - 7

#Filter out tests not during the specified school year and with missing student IDs
vstests_deid <- vstests_deid_raw[which(!(is.na(vstests_deid_raw$ID)) & 
                                         start_date <= vstests_deid_raw$resultdate &
                                         end_date >= vstests_deid_raw$resultdate),]

ntests <- nrow(vstests_deid) 
ntests #640094 tests

#Recode result as a binary 0/1 variable instead of characters
vstests_deid$result <- ifelse(vstests_deid$result == "POSITIVE",
                              1,
                              0)

VScohort.IDs <- unique(vstests_deid$ID) 
nIDs <- length(VScohort.IDs) 
nIDs #25059 students

sample.sizes$school_year_21_22 <- c(ntests, nIDs)






##### Cleaning Step 2: Merge with Vaccination and Demographic Records #####


#Find Vaccination Records for the students reporting tests
VScohort.vax <- aps_vax[which(aps_vax$ID.x %in% VScohort.IDs),]
VScohort.vax <- rename(VScohort.vax, ID = ID.x)
length(unique(VScohort.vax$ID)) #25039 students --> 20 missing vaccination status

na_vax.IDs <- VScohort.IDs[which(!(VScohort.IDs %in% unique(VScohort.vax$ID)))]
# length(na_vax.IDs)

#Vaccination date
VScohort.vax$vax_date <- VScohort.vax$ADMIN_DATE_2_1

#Find Demographic info for the students reporting tests
VScohort.demographics <- studentinfo_deid[which(studentinfo_deid$ID %in% VScohort.IDs),]
length(unique(VScohort.demographics$ID)) #25059

#Creating Age group variable
#Age as of start date
VScohort.demographics$start_age <- floor(as.numeric(start_date - VScohort.demographics$birthdate)/365.25)

#Age group as of start date
# 0 => under 5
# 1 => 5-11
# 2 => 12+
VScohort.demographics$start_age_group <- ifelse(VScohort.demographics$start_age >= 12, 2, 
                                                ifelse(VScohort.demographics$start_age >=5, 1, 0))

race <- VScohort.demographics$fed_race_ethnicity_name
VScohort.demographics$race <- ifelse(VScohort.demographics$fed_race_ethnicity_name %in% c("White", "Black or African American", "Asian", "Hispanic/Latino"), race, "Other")


d1 <- VScohort.vax[,c("ID","vax_date")] #vaccination dates --> 25039
d2 <- VScohort.demographics[,c("ID", "gender", "race", "start_age", "start_age_group")] #Demographics --> 25059
d3 <- vstests_deid[,c("ID", "schoolname", "result", "resultdate")] #tests --> 637497

#merge the 3 datasets --> keep students with missing vax records (assigns them as unvaxed)
VScohort.merged <- merge(d3,merge(d1,d2, by = "ID", all.y = TRUE), by = "ID") #merged
print(paste("There are", nrow(VScohort.merged), "tests with matching vaccine and demographic info")) #637497

VScohort.IDs <- unique(VScohort.merged$ID)








##### Cleaning Step 3 - Only Tests from Primary Enrollment Schools #####


primary_schools <- unique(enrollment_newid[which(enrollment_newid$servicetype == "P" & !is.na(enrollment_newid$school) & enrollment_newid$ID %in% VScohort.IDs),]$school)
primary_schools[which(primary_schools == "BEST MS/HS")] <- "BEST MS-HS"

#North Metro is primary enroll for students with disabilities --> does not report direct certification
primary_schools <- primary_schools[which(primary_schools != "North Metro")]

schools <- unique(VScohort.merged$schoolname)

school.include <- intersect(primary_schools, schools)

VScohort.merged <- VScohort.merged[which(VScohort.merged$schoolname %in% school.include),]

ntests <- nrow(VScohort.merged)
nIDs <- length(unique(VScohort.merged$ID))
sample.sizes$primary_enroll <- c(ntests, nIDs)

print(paste("There are", ntests, "tests with matching vaccine and demographic info at primary enrolled schools")) #635573
print(paste("There are", nIDs, "unique students reporting tests with matching vaccine and demographic info at primary enrolled schools"))








##### Cleaning Step 4 - Only students aged 5-11 #####


#vax status is only 1 if the patient is vaccinated before the test date
VScohort.merged$vax_status <- ifelse(is.na(VScohort.merged$vax_date), 
                                     0, 
                                     ifelse(VScohort.merged$vax_date - VScohort.merged$resultdate < 0,
                                            1,
                                            0)
)

VScohort.merged <- VScohort.merged[which(VScohort.merged$start_age_group == 1),]

ntests <- nrow(VScohort.merged)
nIDs <- length(unique(VScohort.merged$ID))
sample.sizes$ages_5_11 <- c(ntests, nIDs)


print(paste("There are", ntests, "tests with matching vaccine and demographic info at primary enrolled schools between the ages of 5 and 11"))

ntests #483847
nIDs #16021






##### Cleaning Step 5 - only schools reporting >10 tests #####


table(VScohort.merged$vax_status)

table(VScohort.merged$vax_status, VScohort.merged$result)

#Checking positivity of covariate categories

#positivity by school
schools <- unique(VScohort.merged$schoolname)
summary.by.school <- NA

for (i in 1:length(schools)) {
  VS.by.school <- VScohort.merged[which(VScohort.merged$schoolname == schools[[i]]),]
  summary <- c(nrow(VS.by.school), #num students
               sum(VS.by.school$result), #num positive tests
               sum(VS.by.school$vax_status), #num vax tests
               sum(VS.by.school$vax_status)/nrow(VS.by.school), #percent vaxed
               sum(VS.by.school[which(VS.by.school$vax_status == 1),]$result), #num positive tests among vaxed
               sum(VS.by.school[which(VS.by.school$vax_status == 1),]$result)/sum(VS.by.school$vax_status), #percent positive among vaxed
               sum(VS.by.school[which(VS.by.school$vax_status == 0),]$result), #num positive tests among unvaxed
               sum(VS.by.school[which(VS.by.school$vax_status == 0),]$result)/(nrow(VS.by.school) - sum(VS.by.school$vax_status)) #percent positive among unvaxed
  )
  summary.by.school <- rbind(summary.by.school, summary)
  
}

#remove NA row
summary.by.school <- summary.by.school[-1,]

school.summary <- data.frame(summary.by.school)
colnames(school.summary) <- c("n tests", "n + tests", "n vaxed", "% vaxed", 
                              "n + vaxed", "% + vaxed", "n + unvaxed", "% + unvaxed")
rownames(school.summary) <- schools

school.summary

problems <- rownames(school.summary[which(school.summary$`n tests` < 10 | school.summary$`n tests` == school.summary$`n vaxed` | school.summary$`n + tests` == 0), ])

VScohort.merged <- VScohort.merged[which(!(VScohort.merged$schoolname %in% problems)),]

ntests <- nrow(VScohort.merged)
nIDs <- length(unique(VScohort.merged$ID))
sample.sizes$school_sample_size_large <- c(ntests, nIDs)

print(paste("There are", ntests, "tests with matching vaccine and demographic info at primary enrolled schools  at schools with more than 10 tests and with some students of both vaccination statuses among chidren between the ages of 5 and 11"))
print(paste("There are", nIDs, "unique students reporting tests with matching vaccine and demographic info at primary enrolled schools  at schools with more than 10 tests and with some students of both vaccination statuses among children between the ages of 5 and 11"))






##### Cleaning Step 6 - Only where covariates have positivity #####


school.summary2 <- school.summary[which(!(rownames(school.summary) %in% problems)),]

school.summary2

#gender

table(VScohort.merged$gender, VScohort.merged$result)

VScohort.merged <- VScohort.merged[which(VScohort.merged$gender != "N"),]

ntests <- nrow(VScohort.merged)
nIDs <- length(unique(VScohort.merged$ID))
sample.sizes$gender <- c(ntests, nIDs)

print(paste("There are", ntests, "tests with matching vaccine and demographic info at primary enrolled schools  at schools with more than 10 tests and with some students of both vaccination statuses among chidren between the ages of 5 and 11 and who identify with a binary gender"))
print(paste("There are", nIDs, "tests with matching vaccine and demographic info at primary enrolled schools  at schools with more than 10 tests and with some students of both vaccination statuses among chidren between the ages of 5 and 11 and who identify with a binary gender"))

table(VScohort.merged$gender, VScohort.merged$result)


#Checking positivity of vaccination by school by ID


schools <- unique(VScohort.merged$schoolname)

school.vaccination <- NA

for (i in 1:length(schools)){
  VS.by.school <- VScohort.merged[which(VScohort.merged$schoolname == schools[[i]]),]
  #print(schools[[i]])
  IDs.by.school <- unique(VS.by.school$ID)
  #print(paste("Number of students:", length(IDs.by.school)))
  num_vaxed <- 0
  for (j in 1:length(IDs.by.school)) {
    ID <- IDs.by.school[[j]]
    tests.by.ID <- VS.by.school[which(VS.by.school$ID == ID),]
    
    #if a student is vaccinated before at least one test
    if (length(intersect(tests.by.ID$vax_status == 1, TRUE)) == 1) {
      num_vaxed <- num_vaxed + 1
    }
    
  }
  
  #print(paste("Number Vaccinated: ", num_vaxed))
  
  school.vaccination <- rbind(school.vaccination, c(length(IDs.by.school), num_vaxed,  num_vaxed/length(IDs.by.school), length(IDs.by.school) - num_vaxed))
}

school.vaccination <- school.vaccination[-1,]

rownames(school.vaccination) <- schools
colnames(school.vaccination) <- c("num_students", "num_vaxed", "Perc_vaxed", "num_unvaxed")

school.vaccination

# Race
table(VScohort.merged$race, VScohort.merged$result)
table(VScohort.merged$race, VScohort.merged$vax_status)

table(VScohort.merged$race, VScohort.merged$vax_status, VScohort.merged$result)

# Age
table(VScohort.merged$start_age, VScohort.merged$result)
table(VScohort.merged$start_age, VScohort.merged$vax_status)










##### Cleaning Step 7 - Create additional covariates #####

#direct certification
schools <- unique(VScohort.merged$schoolname)

school.info <- data.frame(schoolname = schools)
school.info$schoolnum <- NA
school.info$dir_cert <- NA

for (i in 1:nrow(school.info)){
  school.info[i,]$schoolnum <- as.numeric(APS_School_List[which(APS_School_List$School == school.info[i,]$schoolname),]$`#`)
  school.info[i,]$dir_cert <- Direct_Certification[which(Direct_Certification$SYSTEM_NAME == "Atlanta Public Schools" & Direct_Certification$SCHOOL_ID == school.info[i,]$schoolnum),]$direct_cert_perc
}

VScohort.merged <- merge(VScohort.merged, school.info, by = "schoolname")








##### Cleaning Step 8 - Remove Students who reported tests at multiple schools within the same year #####

VScohort.IDs <- unique(VScohort.merged$ID)

non_transfers <- VScohort.merged %>% group_by(ID) %>% summarize(schools = length(unique(schoolname)))
table(non_transfers$schools)

non_transfers <- non_transfers %>% filter(schools == 1)
non_transferIDS <- non_transfers$ID

VScohort.merged <- VScohort.merged %>% filter(ID %in% non_transferIDS)



ntests <- nrow(VScohort.merged)
nIDs <- length(unique(VScohort.merged$ID))
sample.sizes$no_transfers <- c(ntests, nIDs)

print(paste("There are", ntests, "tests with matching vaccine and demographic info at primary enrolled schools  at schools with more than 10 tests and with some students of both vaccination statuses among chidren between the ages of 5 and 11 and who identify with a binary gender and reported tests at only 1 school"))
print(paste("There are", nIDs, "students with tests with matching vaccine and demographic info at primary enrolled schools  at schools with more than 10 tests and with some students of both vaccination statuses among chidren between the ages of 5 and 11 and who identify with a binary gender and reported tests at only 1 school"))








##### Cleaning Step 9 - Exclude Students with less than 3 tests total reported


VScohort.IDs <- unique(VScohort.merged$ID)

number_of_tests <- VScohort.merged %>% group_by(ID) %>% summarize(num_tests = n())
table(number_of_tests$num_tests)

number_of_tests <- number_of_tests %>% filter(num_tests >= 3)
tested_enough_IDs <- number_of_tests$ID

VScohort.merged <- VScohort.merged %>% filter(ID %in% tested_enough_IDs)


ntests <- nrow(VScohort.merged)
nIDs <- length(unique(VScohort.merged$ID))
sample.sizes$more_than_3_tests <- c(ntests, nIDs)



print(paste("There are", ntests, "tests with matching vaccine and demographic info at primary enrolled schools  at schools with more than 10 tests and with some students of both vaccination statuses among chidren between the ages of 5 and 11 and who identify with a binary gender and reported tests at only 1 school and reported at least 3 tests"))
print(paste("There are", nIDs, "students with tests with matching vaccine and demographic info at primary enrolled schools  at schools with more than 10 tests and with some students of both vaccination statuses among chidren between the ages of 5 and 11 and who identify with a binary gender and reported tests at only 1 school and reported at least 3 tests"))

write.csv(VScohort.merged, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_daily_testing_by_ID.csv")




##### Start of File #2 #####




##### Read in Cleaned Data Set #####

VScohort.IDs <- sort(unique(VScohort.merged$ID))



##### Manipulating into data frame we want #####

VScohort.merged$sunday_before <- ifelse(as.numeric(as.Date(VScohort.merged$resultdate)) %% 7 >= 3,
                                        as.numeric(as.Date(VScohort.merged$resultdate)) - (as.numeric(as.Date(VScohort.merged$resultdate)) %% 7) + 3,
                                        as.numeric(as.Date(VScohort.merged$resultdate)) - (as.numeric(as.Date(VScohort.merged$resultdate)) %% 7) - 4)

#Turning Sunday before from random large numbers to week numbers
VScohort.merged$week <- (VScohort.merged$sunday_before - first_sunday)/7

weeks <- sort(unique(VScohort.merged$week))
weeks_as_dates <- weeks*7 + first_sunday


VScohort.merged$sunday_after_vax <- ifelse(is.na(VScohort.merged$vax_date), NA, 
                                           ifelse(as.numeric(as.Date(VScohort.merged$vax_date)) %% 7 >= 3,
                                                  as.numeric(as.Date(VScohort.merged$vax_date)) - (as.numeric(as.Date(VScohort.merged$vax_date)) %% 7) + 10,
                                                  as.numeric(as.Date(VScohort.merged$vax_date)) - (as.numeric(as.Date(VScohort.merged$vax_date)) %% 7) + 3))

#Turning Sunday after vax from random large numbers to week numbers
VScohort.merged$vax_week <- (VScohort.merged$sunday_after_vax - first_sunday)/7

#Create Empty Data Frame to fill
VScohort.tests <- data.frame(ID = rep(VScohort.IDs, each = length(weeks)),
                             week = rep(weeks, times = length(VScohort.IDs)),
                             tested = NA,
                             vax_status = NA,
                             prior_infections = NA, 
                             no_history = NA,
                             num_prev_test = NA,
                             time_since_last = NA,
                             tests_in_28 = NA,
                             tests_in_14 = NA,
                             test_density = NA,
                             adj_test_density = NA,
                             first_week = NA,
                             last_week = NA)

all_tests <- VScohort.merged %>% arrange(ID, week)

all_tests_vars <- unique(all_tests %>% select(ID, vax_date, vax_week, start_age, gender, race, schoolname, dir_cert))


together <- merge(x = VScohort.tests,
                  y = all_tests_vars,
                  all.x = TRUE)


i <- 0
for (ID in VScohort.IDs) {
  
  i <- i + 1
  
  tests <- all_tests[which(all_tests$ID == ID),]
  weeks_tested <- unique(tests$week)
  first_week <- min(weeks_tested)
  last_week <- max(weeks_tested)
  
  for (k in weeks) {
    test <- tests[which(tests$week == k),]
    
    
    #No Test this week
    if (nrow(test) == 0) {
      
      tested <- 0
      
      #Previous Tests on Record
      if (k > first_week) {
        previous_tests <- tests[which(tests$week < k),]
        #Creating a fake result date for the lack of test this week
        pseudo_resultdate <- k*7 + first_sunday
        
        no_history <- 0
        num_prev_test <- nrow(previous_tests)
        time_since_last <- pseudo_resultdate - as.numeric(tail(previous_tests, n=1)$resultdate)
        tests_in_28 <- nrow(previous_tests[which(previous_tests$week >= (k - 4)),])
        tests_in_14 <- nrow(previous_tests[which(previous_tests$week >= (k - 2)),])
        test_density <- num_prev_test/(pseudo_resultdate - as.numeric(start_date))
        adj_test_density <- num_prev_test/(pseudo_resultdate - as.numeric(tests[1,]$resultdate))
        
        
        
        #No Previous Tests on Record
      } else {
        no_history <- 1
        num_prev_test <- 0
        time_since_last <- NA
        tests_in_28 <- 0
        tests_in_14 <- 0
        test_density <- 0
        adj_test_density <- 0
      }
      
      
      
      #At Least one Test this week
    } else {
      
      tested <- 1
      
      # 2+ tests this week
      if (nrow(test) > 1) {
        test <- test[1,] #Just use the first test that week
      }
      
      date <- test$resultdate
      
      
      
      # Previous Tests on Record
      if(k > first_week) { #if at least one test has been taken previously
        previous_tests <- tests[which(tests$week < k),]
        
        no_history <- 0
        num_prev_test <- nrow(previous_tests)
        time_since_last <- as.numeric(date - tail(previous_tests, n=1)$resultdate)
        tests_in_28 <- nrow(previous_tests[which(previous_tests$week >= (k - 4)),])
        tests_in_14 <- nrow(previous_tests[which(previous_tests$week >= (k - 2)),])
        test_density <- num_prev_test/(as.numeric(date - start_date))
        adj_test_density <- num_prev_test/(as.numeric(date - tests[1,]$resultdate))
        
        
        
        #No Previous Tests on Record
      } else { #if this is the first test
        no_history <- 1
        num_prev_test <- 0
        time_since_last <- NA
        tests_in_28 <- 0
        tests_in_14 <- 0
        test_density <- 0
        adj_test_density <- 0
      }
      
      
    }
    
    
    #Save this set of testing history into the combined data frame
    together[which(together$ID == ID & together$week == k),
             c("tested", "no_history", "num_prev_test", 
               "time_since_last", "tests_in_28", "tests_in_14", 
               "test_density", "adj_test_density", "first_week", "last_week")] <- 
      c(tested, no_history, num_prev_test, time_since_last, tests_in_28, 
        tests_in_14, test_density, adj_test_density, first_week, last_week)
    
    
  }
  
}


  #Redefining Vaccination Status and time since vaccination

together$vax_status <- ifelse(is.na(together$vax_week), 0,
                              ifelse(together$vax_week <= together$week, 1, 0))

together$weeks_since_vax <- ifelse(is.na(together$vax_week), 0,
                                  ifelse(together$vax_week > together$week, 0, 
                                         together$week - together$vax_week))

  #Defining Prior Infections
VScohort.alltests <- SendSS_tests[which(SendSS_tests$ID %in% VScohort.IDs), c("ID", "result_date")]
VScohort.alltests <- rename(VScohort.alltests, resultdate = result_date)

VSpositives <- VScohort.merged[which(VScohort.merged$result == 1), c("ID", "resultdate")]

VScohort.alltests <- rbind(VScohort.alltests, VSpositives)
VScohort.alltests.IDs <- unique(VScohort.alltests$ID)

#removing duplicates
VScohort.alltests$duplicate <- 0
duplicates <- unique(VScohort.alltests[duplicated(VScohort.alltests$ID),]$ID)

for (i in 1:length(duplicates)) {
  
  ID <- duplicates[[i]]
  tests <- VScohort.alltests[which(VScohort.alltests$ID == ID),] %>%
    arrange(resultdate)
  flags <- data.frame(index = which(VScohort.alltests$ID == ID), flag = rep(0, nrow(tests)) )
  
  for (j in 1: (nrow(tests)-1) ) {
    current_test <- tests[j,]$resultdate
    next_test <- tests[j+1,]$resultdate
    if (next_test - current_test <= 14) {
      flags[j+1,]$flag <- 1
    } 
  }
  
  VScohort.alltests[flags$index,]$duplicate <- flags$flag
  
}

VScohort.alltests <- VScohort.alltests[which(VScohort.alltests$duplicate == 0),]

#Save the list of all positive tests as a csv
write.csv(VScohort.alltests, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_positive_tests.csv")

VScohort.alltests <- VScohort.alltests %>% select(c(ID, resultdate)) %>% arrange(resultdate)

for (ID in VScohort.IDs) {
  
  positives <- VScohort.alltests[which(VScohort.alltests$ID == ID),]
  
  prior_infections <- rep(0, length(weeks))
  
  #if a positive test is recorded
  if (nrow(positives) != 0) {
    
    for (k in 1:nrow(positives)) {
      
      positive_date <- as.numeric(positives[k,]$resultdate)
      
      for (j in 1:length(weeks_as_dates)) {
        
        if (weeks_as_dates[j] > positive_date) {
          prior_infections[j] = prior_infections[j] + 1
        }
        
      }
      
    }
    
  }
  
  # print(prior_infections)
  
  #Saving the prior infections into the data set
  together[which(together$ID == ID),]$prior_infections <- prior_infections
  
}


# Adding in positivity of tests

VScohort.positives <- VScohort.merged[which(VScohort.merged$result == 1),]

together$result <- 0

for (i in 1:nrow(VScohort.positives)) {
  
  positive_result <- VScohort.positives[i,]
  ID <- positive_result$ID
  week <- positive_result$week
  
  together[which(together$ID == ID & together$week == week), ]$result <- 1
  
}


#Final Confirmations

ntests <- sum(together$tested)
nIDs <- length(unique(together$ID))
sample.sizes$only_1_per_week <- c(ntests, nIDs)


#Double check that all weeks included have multiple tests
VScohort.byWeek <- together %>% group_by(week) %>% summarize(num_tests = sum(tested))

VScohort.byWeek

#Remove week 16 --> only 2 tests
together <- together %>% filter(week != 16)
VScohort.byWeek <- together %>% group_by(week) %>% summarize(num_tests = sum(tested))
VScohort.byWeek

#Checking that all individuals have at least 3 weeks of testing
VScohort.byID <- together %>% group_by(ID) %>% summarize(num_tests = sum(tested))
table(VScohort.byID$num_tests)

#Removing low testers
lowTesting <- VScohort.byID %>% filter(num_tests < 3)
lowTestingIDs <- lowTesting$ID
together <- together %>% filter(!ID %in% lowTestingIDs)


ntests <- sum(together$tested)
nIDs <- length(unique(together$ID))
sample.sizes$more_than_3_weeks <- c(ntests, nIDs)

#Save Dataset
write.csv(together, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_weekly_testing_by_ID.csv")












##### Start of File #3 #####

VScohort.tested <- together

VScohort.IDs <- sort(unique(VScohort.tested$ID))

weeks <- sort(unique(VScohort.tested$week))


#Creating Interaction terms implicitly

#If there is no testing history, set all testing behavior covariates to 0
VScohort.tested[which(VScohort.tested$no_history == 1), c("num_prev_test", "time_since_last", "tests_in_28", "tests_in_14", "test_density", "adj_test_density")] <- 0


# Full Sample Fit --> Random effect for ID

VScohort.IDs <- sort(unique(VScohort.tested$ID))

#Logistic Regression Model (school and ID as random effect)
reg.fit <-  glmer(formula = tested ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history,
                    family = binomial,
                    data = VScohort.tested,
                    control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1000000)))
summary(reg.fit)

saveRDS(reg.fit, "/home/amoor53/APSVE_cluster/models/2021_age_5_11_predicted_propensity_scores_model.rds")


prediction_data <- VScohort.tested %>% select(vax_status, start_age, gender, race, dir_cert, prior_infections, schoolname, ID, week, num_prev_test, time_since_last, tests_in_28, tests_in_14, test_density, adj_test_density, no_history)

propensities <- predict(reg.fit, newdata = VScohort.tested, type = "response")
VScohort.tested$propensity <- propensities


#Save Dataset
write.csv(VScohort.tested, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")

write.csv(sample.sizes, "/home/amoor53/APSVE_cluster/results/2021_age_5_11_dataset_workflow_sample_sizes.csv")


