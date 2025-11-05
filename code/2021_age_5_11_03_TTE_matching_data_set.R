
# APS VE TTE Matching Data Set
# 2021-22 Ages 5-11 
# 2025-11-05
# Modified to run on the cluster
# Amy Moore

  #Load Packages
library(readr) #Read in Files
# library(here) # File Locations
library(lubridate) #Dates
library(MatchIt) #Matching Pairs
library(tidyverse)

# Read in Long Form with Missing Data lines Data set
VScohort.tested <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")

start_date <- ymd("2021-09-07") #First day of VS testing
end_date <- ymd("2022-05-26") #Last day of School
first_sunday <- as.numeric(ymd("2021-09-05")) - 7

alpha <- 0.05

weeks <- sort(unique(VScohort.tested$week))


#Select only the weeks/IDs where testing actually occurred
VScohort.alltests <- VScohort.tested[which(VScohort.tested$tested == 1),]


#Need to switch back to a student level data set

VScohort.IDs <- unique(VScohort.alltests$ID)

  #Adding Variables that do not change over time
    #ID
    #Infections Prior to study start
    #Week of first test recorded
    #Vaccination Date (date does not change but status might)
    #Age
    #Gender
    #Race
    #School
    #Socioeconmic status (based on school)
VScohort.byStudent <- VScohort.alltests %>% group_by(ID) %>% slice(1) %>% 
  select("ID", "prior_infections", "first_week", "vax_date", "vax_week", "start_age", "gender", "race", "schoolname", "dir_cert")

  #If vaccination occurs before week 1, just set it to 0 --> implies vaccination before study start, enroll at week 1
  #If vaccination occurs after week 35 (3 weeks before the end of school year) --> vax status = 0, vax_week = NA
VScohort.byStudent$vax_week <- ifelse(is.na(VScohort.byStudent$vax_week), NA,
                                      ifelse(VScohort.byStudent$vax_week > (max(weeks) - 3), NA, 
                                      ifelse(VScohort.byStudent$vax_week < 1, 0, VScohort.byStudent$vax_week)))

  #define vax status based on if vax week occurs before the end of the school year
VScohort.byStudent$vax_status <- ifelse(is.na(VScohort.byStudent$vax_week), 0, 1)

  #Define result date - primarily using the first positive result
first_positives <- VScohort.alltests %>% group_by(ID) %>% filter(result == 1) %>% reframe(n_positive = length(result), positive_week = head(week,1))

  #Listing all positives in case first positive occurs before enrollment due to vax date or matching
all_positives <- VScohort.alltests %>% group_by(ID) %>% filter(result == 1) %>% reframe(n_positive = length(result), positive_week = week)

VScohort.byStudent <- merge(x = VScohort.byStudent, 
      y = first_positives,
      by = "ID",
      all.x = TRUE)

VScohort.byStudent$result <- ifelse(is.na(VScohort.byStudent$n_positive), 0, 1)
VScohort.byStudent$n_positive <- ifelse(is.na(VScohort.byStudent$n_positive), 0, VScohort.byStudent$n_positive)


  #Adding in total number of tests reported
num_tests <- VScohort.alltests %>% group_by(ID) %>% summarize(n_tests = n())



VScohort.byStudent <- merge(x = VScohort.byStudent, 
                            y = num_tests,
                            by = "ID",
                            all.x = TRUE)


  #Simplifying start date to be the maximum of Vax Week or First Test week 
    #If vax date is NA --> start week is first week of testing
    #If vax date exists --> start after both vaccination occurs and testing begins
VScohort.byStudent$start_week <- ifelse(is.na(VScohort.byStudent$vax_week), 
                                        VScohort.byStudent$first_week,
                                        pmax(VScohort.byStudent$first_week, VScohort.byStudent$vax_week))




#diagnostics of the prematch dataset
table(VScohort.byStudent$start_age)
table(VScohort.byStudent$vax_status)
table(VScohort.byStudent$vax_status, VScohort.byStudent$start_age)
table(VScohort.byStudent$schoolname)

nrow(VScohort.byStudent) #15202

estimate_of_match <- table(VScohort.byStudent$schoolname, VScohort.byStudent$vax_status)
max_n_of_match <- apply(estimate_of_match, 1, max)
sum(max_n_of_match) #11690




set.seed(9222025)
VScohort.match <- matchit(formula = vax_status ~ schoolname + start_age + start_week + n_tests,
                          data = VScohort.byStudent,
                          method = "nearest",
                          distance = "glm",
                          replace = FALSE,
                          exact = ~schoolname
)

VScohort.matched <- match.data(VScohort.match) %>% arrange(subclass, vax_status)

VScohort.matched <- VScohort.matched %>% select(!c("distance", "weights"))

nrow(VScohort.matched)
sum(VScohort.matched$n_tests)

VScohort.matched$flags <- 0
VScohort.matched$enrollment_week <- NA

for (i in seq(1, nrow(VScohort.matched), 2)) {
  
  #Grab the pair of students
  matched_pair <- VScohort.matched[c(i,i+1),]
  
  #Update Enrollment week to match between paired students --> match to start week for vaxed student
  enroll_week <- matched_pair[2,]$start_week
  VScohort.matched[c(i, i+1),]$enrollment_week <- enroll_week
  
  #Check if positives occurred before enrollment
  if (!is.na(matched_pair[1,]$positive_week) & matched_pair[1,]$positive_week < enroll_week) {
      #0 out result 
    VScohort.matched[i,]$result <- 0
    VScohort.matched[i,]$positive_week <- NA
      #Flag for checking for subsequent positives
    VScohort.matched[i,]$flags <- 1
  } 
  if (!is.na(matched_pair[2,]$positive_week) & matched_pair[2,]$positive_week < enroll_week) {
      #0 out result
    VScohort.matched[i+1,]$result <- 0
    VScohort.matched[i+1,]$positive_week <- NA
      #Flag for checking subsequent positives
    VScohort.matched[i+1,]$flags <- 1
    
  }
  
}


sum(VScohort.matched$flags) #198 (yikes!!!)

table(VScohort.matched$result)

flagged.IDs <- VScohort.matched[which(VScohort.matched$flags == 1),]$ID

flagged.positives <- VScohort.alltests[which(VScohort.alltests$ID %in% flagged.IDs & VScohort.alltests$result == 1), c("ID", "week", "prior_infections")]

for (i in 1:nrow(flagged.positives)) {
  
    #Grab info about the positive test
  ID <- flagged.positives[i,]$ID
  positive_week <- flagged.positives[i,]$week
  prior_infections <- flagged.positives[i,]$prior_infections
  
  
    #Grab info about the enrollment time
  enroll_week <- VScohort.matched[which(VScohort.matched$ID == ID),]$enrollment_week
  
  if (enroll_week <= positive_week) {
    VScohort.matched[which(VScohort.matched$ID == ID),]$result <- 1
    VScohort.matched[which(VScohort.matched$ID == ID),]$positive_week <- positive_week
      #Fix Prior Infections
    VScohort.matched[which(VScohort.matched$ID == ID),]$prior_infections <- prior_infections
  }
  
}

  #Confirming that all positive tests occur after enrollment
Confirmation <- VScohort.matched[which(VScohort.matched$result == 1),c("ID", "enrollment_week", "positive_week")]
Confirmation$difference <- Confirmation$positive_week - Confirmation$enrollment_week
summary(Confirmation$difference) #min is non-negative



table(VScohort.matched$result)


  #Average Testing Behavior Variables

VScohort.matched$avg_test_density <- NA
VScohort.matched$avg_adj_test_density <- NA
VScohort.matched$avg_time_since_last <- NA
VScohort.matched$avg_tests_in_28 <- NA
VScohort.matched$avg_tests_in_14 <- NA
VScohort.matched$flags <- 0
VScohort.matched$last_test <- NA

for (i in 1:nrow(VScohort.matched)) {
  
  ID <- VScohort.matched[i,]$ID
  
  result <- VScohort.matched[i,]$result
  
  enroll_week <- VScohort.matched[i,]$enrollment_week
  
  if (result == 0) {
    
      #Testing is censored at study end
    tests <- VScohort.alltests[which(VScohort.alltests$ID == ID & VScohort.alltests$week >= enroll_week),]
    
      #Flag individuals who have less than 3 tests after enrollment assignment
    if (nrow(tests) < 3) {
      tests <- VScohort.alltests[which(VScohort.alltests$ID == ID),]
      VScohort.matched[i,]$flags <- 2
    }
    
    
  } else {
    
    positive_week <- VScohort.matched[i,]$positive_week
      
      #Testing ends at positive test
    tests <- VScohort.alltests[which(VScohort.alltests$ID == ID & VScohort.alltests$week >= enroll_week & VScohort.alltests$week <= positive_week),]
    
    
    
  }
  
  last_test <- tail(tests, n =1)
  VScohort.matched[i,]$n_tests <- nrow(tests)
  VScohort.matched[i,]$avg_test_density <- last_test$test_density
  VScohort.matched[i,]$avg_adj_test_density <- last_test$adj_test_density
  VScohort.matched[i,]$avg_time_since_last <- mean(tests$time_since_last)
  VScohort.matched[i,]$avg_tests_in_28 <- mean(tests$tests_in_28)
  VScohort.matched[i,]$avg_tests_in_14 <- mean(tests$tests_in_14)
  VScohort.matched[i,]$last_test <- last_test$week
  
}

  #flag = 2 implies that a partner does not have >3 tests from enrollment to study end
    #throw out these matches
table(VScohort.matched$flags)

subclass_errors <- unique(as.numeric(VScohort.matched[which(VScohort.matched$flags == 2),]$subclass))

VScohort.matched <- VScohort.matched[which(!(as.numeric(VScohort.matched$subclass) %in% subclass_errors)),]

  #Checking Sample Size
nrow(VScohort.matched)
sum(VScohort.matched$n_tests)


  #Create Time to event variables
VScohort.matched$time_to_event <- ifelse(is.na(VScohort.matched$positive_week), VScohort.matched$last_test - VScohort.matched$enrollment_week, VScohort.matched$positive_week - VScohort.matched$enrollment_week)
VScohort.matched$event_occured <- ifelse(is.na(VScohort.matched$positive_week), 0, 1)

VScohort.simplified <- VScohort.matched %>% select("ID", "start_age", "n_tests", "vax_week", "first_week", "start_week", "enrollment_week", "positive_week", "last_test", "time_to_event", "event_occured")


# write.csv(VScohort.matched, here("data", "2021-22 Ages 5-11 Matched Data Set.csv"))
write.csv(VScohort.tested, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_matched_data_set.csv")








#Time-varying Covariate Version of Matched Dataset

VScohort.windows <- VScohort.matched %>% select("ID", "subclass", "enrollment_week", "last_test", "n_tests")

VScohort.IDs <- unique(VScohort.matched$ID)

VScohort.matched.bytest <- VScohort.alltests %>% filter(ID %in% VScohort.IDs)

VScohort.matched.bytest <- merge(x = VScohort.matched.bytest,
                                 y = VScohort.windows,
                                 by = "ID",
                                 all.x = TRUE)

VScohort.matched.bytest <- VScohort.matched.bytest %>% filter(week >= enrollment_week & week <= last_test)

  #The number of tests should precisely match between the ID level number of tests and the tests level number of tests
nrow(VScohort.matched.bytest)
sum(VScohort.matched$n_tests)
  #N = 109392


  #The number of students should match precisely
nrow(VScohort.matched)
length(unique(VScohort.matched.bytest$ID))




  #Adding time windows to each test
VScohort.matched.bytest$previous_week <- NA

for (i in 1:length(VScohort.IDs)) {
  ID.i <- VScohort.IDs[i]
  
  tests <- VScohort.matched.bytest %>% filter(ID == ID.i)
  
  enrollment_week <- tests[1,]$enrollment_week
  
  for (j in 1:nrow(tests)) {
    
    current_week <- tests[j,]$week
    
    if (j == 1) {
        #For first test, interval starts at enrollment
      VScohort.matched.bytest[which(VScohort.matched.bytest$ID == ID.i & VScohort.matched.bytest$week == current_week),]$previous_week <- enrollment_week
      
    } else {
        #For not first test, interval starts at previous test
      previous_week <- tests[(j-1),]$week
      # print(tests[(j-1),]$week)
      
      VScohort.matched.bytest[which(VScohort.matched.bytest$ID == ID.i & VScohort.matched.bytest$week == current_week),]$previous_week <- previous_week
      
    }
    
    
  }
  
}



# write.csv(VScohort.matched.bytest, here("data", "2021-22 Ages 5-11 Matched Data Set TVC.csv"))
write.csv(VScohort.tested, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_matched_TVC.csv")

































  #############################
  #####   Super Testers   #####
  #############################


head(VScohort.byStudent)
total_num_tests <- sum(VScohort.byStudent$n_tests)

  #Define Super Testers the same way we did in TND

max_tests <- max(VScohort.byStudent$n_tests)
Q3 <- max_tests
n <- nrow(VScohort.byStudent)

for (i in seq(max_tests, 3, -1)){
  n_above <- nrow(VScohort.byStudent %>% filter(n_tests >= i))
  perc_above <- round(n_above/n * 100, digits = 2)
  
  if (perc_above < 25.00) {
    Q3 <- i
  }
}
print(Q3)

STcohort.byStudent <- VScohort.byStudent %>% filter(n_tests >= Q3)
total_num_tests <- sum(STcohort.byStudent$n_tests)


#diagnostics of the prematch dataset
table(STcohort.byStudent$start_age)
table(STcohort.byStudent$vax_status)
table(STcohort.byStudent$vax_status, STcohort.byStudent$start_age)
table(STcohort.byStudent$schoolname)

nrow(STcohort.byStudent) #2823

estimate_of_match <- table(STcohort.byStudent$schoolname, STcohort.byStudent$vax_status)
max_n_of_match <- apply(estimate_of_match, 1, max)
sum(max_n_of_match) #2077

table(STcohort.byStudent$vax_status)
table(STcohort.byStudent$vax_status, STcohort.byStudent$result)


STcohort.match <- matchit(formula = vax_status ~ schoolname,
                          data = STcohort.byStudent,
                          method = "nearest",
                          distance = "glm",
                          replace = FALSE,
                          exact = ~schoolname
)

STcohort.matched <- match.data(STcohort.match) %>% arrange(subclass, vax_status)

STcohort.matched <- STcohort.matched %>% select(!c("distance", "weights"))

nrow(STcohort.matched)
sum(STcohort.matched$n_tests)

STcohort.matched$flags <- 0
STcohort.matched$enrollment_week <- NA

for (i in seq(1, nrow(STcohort.matched), 2)) {
  
  #Grab the pair of students
  matched_pair <- STcohort.matched[c(i,i+1),]
  
  #Update Enrollment week to match between paired students --> match to start week for vaxed student
  enroll_week <- matched_pair[2,]$start_week
  STcohort.matched[c(i, i+1),]$enrollment_week <- enroll_week
  
  #Check if positives occurred before enrollment
  if (!is.na(matched_pair[1,]$positive_week) & matched_pair[1,]$positive_week < enroll_week) {
    #0 out result 
    STcohort.matched[i,]$result <- 0
    STcohort.matched[i,]$positive_week <- NA
    #Flag for checking for subsequent positives
    STcohort.matched[i,]$flags <- 1
  } 
  if (!is.na(matched_pair[2,]$positive_week) & matched_pair[2,]$positive_week < enroll_week) {
    #0 out result
    STcohort.matched[i+1,]$result <- 0
    STcohort.matched[i+1,]$positive_week <- NA
    #Flag for checking subsequent positives
    STcohort.matched[i+1,]$flags <- 1
    
  }
  
}


sum(STcohort.matched$flags) #45 (yikes!!!)

table(STcohort.matched$result)

flagged.IDs <- STcohort.matched[which(STcohort.matched$flags == 1),]$ID

flagged.positives <- VScohort.alltests[which(VScohort.alltests$ID %in% flagged.IDs & VScohort.alltests$result == 1), c("ID", "week", "prior_infections")]

for (i in 1:nrow(flagged.positives)) {
  
  #Grab info about the positive test
  ID <- flagged.positives[i,]$ID
  positive_week <- flagged.positives[i,]$week
  prior_infections <- flagged.positives[i,]$prior_infections
  
  
  #Grab info about the enrollment time
  enroll_week <- STcohort.matched[which(STcohort.matched$ID == ID),]$enrollment_week
  
  if (enroll_week <= positive_week) {
    STcohort.matched[which(STcohort.matched$ID == ID),]$result <- 1
    STcohort.matched[which(STcohort.matched$ID == ID),]$positive_week <- positive_week
    #Fix Prior Infections
    STcohort.matched[which(STcohort.matched$ID == ID),]$prior_infections <- prior_infections
  }
  
}

#Confirming that all positive tests occur after enrollment
Confirmation <- STcohort.matched[which(STcohort.matched$result == 1),c("ID", "enrollment_week", "positive_week")]
Confirmation$difference <- Confirmation$positive_week - Confirmation$enrollment_week
summary(Confirmation$difference) #min is non-negative



table(STcohort.matched$result)


#Average Testing Behavior Variables

STcohort.matched$avg_test_density <- NA
STcohort.matched$avg_adj_test_density <- NA
STcohort.matched$avg_time_since_last <- NA
STcohort.matched$avg_tests_in_28 <- NA
STcohort.matched$avg_tests_in_14 <- NA
STcohort.matched$flags <- 0
STcohort.matched$last_test <- NA

for (i in 1:nrow(STcohort.matched)) {
  
  ID <- STcohort.matched[i,]$ID
  
  result <- STcohort.matched[i,]$result
  
  enroll_week <- STcohort.matched[i,]$enrollment_week
  
  if (result == 0) {
    
    #Testing is censored at study end
    tests <- VScohort.alltests[which(VScohort.alltests$ID == ID & VScohort.alltests$week >= enroll_week),]
    
    #Flag individuals who have less than 3 tests after enrollment assignment
    if (nrow(tests) < 3) {
      tests <- VScohort.alltests[which(VScohort.alltests$ID == ID),]
      STcohort.matched[i,]$flags <- 2
    }
    
    
  } else {
    
    positive_week <- STcohort.matched[i,]$positive_week
    
    #Testing ends at positive test
    tests <- VScohort.alltests[which(VScohort.alltests$ID == ID & VScohort.alltests$week >= enroll_week & VScohort.alltests$week <= positive_week),]
    
    
    
  }
  
  last_test <- tail(tests, n =1)
  STcohort.matched[i,]$n_tests <- nrow(tests)
  STcohort.matched[i,]$avg_test_density <- last_test$test_density
  STcohort.matched[i,]$avg_adj_test_density <- last_test$adj_test_density
  STcohort.matched[i,]$avg_time_since_last <- mean(tests$time_since_last)
  STcohort.matched[i,]$avg_tests_in_28 <- mean(tests$tests_in_28)
  STcohort.matched[i,]$avg_tests_in_14 <- mean(tests$tests_in_14)
  STcohort.matched[i,]$last_test <- last_test$week
  
}

#flag = 2 implies that a partner does not have >3 tests from enrollment to study end
#throw out these matches
table(STcohort.matched$flags)

subclass_errors <- unique(as.numeric(STcohort.matched[which(STcohort.matched$flags == 2),]$subclass))

STcohort.matched <- STcohort.matched[which(!(as.numeric(STcohort.matched$subclass) %in% subclass_errors)),]

#Checking Sample Size
nrow(STcohort.matched)
sum(STcohort.matched$n_tests)


#Create Time to event variables
STcohort.matched$time_to_event <- ifelse(is.na(STcohort.matched$positive_week), STcohort.matched$last_test - STcohort.matched$enrollment_week, STcohort.matched$positive_week - STcohort.matched$enrollment_week)
STcohort.matched$event_occured <- ifelse(is.na(STcohort.matched$positive_week), 0, 1)

STcohort.simplified <- STcohort.matched %>% select("ID", "start_age", "n_tests", "vax_week", "first_week", "start_week", "enrollment_week", "positive_week", "last_test", "time_to_event", "event_occured")


# write.csv(STcohort.matched, here("data", "2021-22 Ages 5-11 Super Tester Matched Data Set.csv"))
write.csv(VScohort.tested, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_ST_matched_data_set.csv")





