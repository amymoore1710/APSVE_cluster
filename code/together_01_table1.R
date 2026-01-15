
# APS VE Table 1
# 2025-12-18
# Run Locally
# Amy Moore

library(here)
library(dplyr)

# Making a dataframe to fill out:

table1 <- data.frame(summary_2021_age_5_11 = rep(NA, 27),
                     summary_2022_age_5_11 = rep(NA, 27),
                     blank = c(rep("", 7), "Age", "     12", "     13", "     14", 
                               "     15", "     16", "     17", "     18", rep("",12)),
                     summary_2021_age_12_18 = rep(NA, 27),
                     summary_2022_age_12_18 = rep(NA, 27))
rownames(table1) <- c("Overall", 
                      "Vaccination Status", "Vaccinated", "Unvaccinated", 
                      "Test Result", "Positive", "Negative",
                      "Age", "5", "6", "7", "8", "9", "10", "11",
                      "Race", "Black or African American", "White", 
                      "Asian", "Hispanic/Latino", "Other",
                      "Gender", "Female", "Male",
                      "Prior Infections", "None", "At least one")


table1_summary <- function(data) {
  testing_by_ID <- data %>% 
    group_by(ID) %>% 
    summarize(num_tests = sum(tested), 
              age = first(start_age), 
              vax_status = max(vax_status),
              result = max(result),
              race = first(race),
              gender = first(gender),
              prior_infections = max(prior_infections))
  
  overall <- testing_by_ID %>%
    summarize(n = n()) 
  
  
  vax_status <- testing_by_ID %>%
    group_by(vax_status) %>%
    summarize(n = n()) %>%
    mutate(percent = (n/sum(n)) *100)

  test_results <- testing_by_ID %>%
    group_by(result) %>%
    summarize(n = n()) %>%
    mutate(percent = (n/sum(n)) *100)

  age <- testing_by_ID %>%
    group_by(age) %>%
    summarize(n = n()) %>%
    mutate(percent = (n/sum(n)) *100)

  race <- testing_by_ID %>%
    group_by(race) %>%
    summarize(n = n()) %>%
    mutate(percent = (n/sum(n)) *100)

  gender <- testing_by_ID %>%
    group_by(gender) %>%
    summarize(n = n()) %>%
    mutate(percent = (n/sum(n)) *100)

  prior_infections <- testing_by_ID %>%
    group_by(prior_infections > 1) %>%
    summarize(n = n()) %>%
    mutate(percent = (n/sum(n)) *100)



  summary <- c(paste0(overall$n),
               "",
               paste0(vax_status$n[2], " (", format(round(vax_status$percent[2], 2), nsmall = 2), "%)"),
               paste0(vax_status$n[1], " (", format(round(vax_status$percent[1], 2), nsmall = 2), "%)"),
               "",
               paste0(test_results$n[2], " (", format(round(test_results$percent[2], 2), nsmall = 2), "%)"), 
               paste0(test_results$n[1], " (", format(round(test_results$percent[1], 2), nsmall = 2), "%)"), 
               "",
               paste0(age$n[1], " (", format(round(age$percent[1], 2), nsmall = 2), "%)"), 
               paste0(age$n[2], " (", format(round(age$percent[2], 2), nsmall = 2), "%)"), 
               paste0(age$n[3], " (", format(round(age$percent[3], 2), nsmall = 2), "%)"), 
               paste0(age$n[4], " (", format(round(age$percent[4], 2), nsmall = 2), "%)"), 
               paste0(age$n[5], " (", format(round(age$percent[5], 2), nsmall = 2), "%)"), 
               paste0(age$n[6], " (", format(round(age$percent[6], 2), nsmall = 2), "%)"), 
               paste0(age$n[7], " (", format(round(age$percent[7], 2), nsmall = 2), "%)"), 
               "",
               paste0(race$n[2], " (", format(round(race$percent[2], 2), nsmall = 2), "%)"), 
               paste0(race$n[5], " (", format(round(race$percent[5], 2), nsmall = 2), "%)"),
               paste0(race$n[1], " (", format(round(race$percent[1], 2), nsmall = 2), "%)"),
               paste0(race$n[3], " (", format(round(race$percent[3], 2), nsmall = 2), "%)"),
               paste0(race$n[4], " (", format(round(race$percent[4], 2), nsmall = 2), "%)"),
               "",
               paste0(gender$n[1], " (", format(round(gender$percent[1], 2), nsmall = 2), "%)"),
               paste0(gender$n[2], " (", format(round(gender$percent[2], 2), nsmall = 2), "%)"),
               "",
               paste0(prior_infections$n[1], " (", format(round(prior_infections$percent[1], 2), nsmall = 2),"%)"),
               paste0(prior_infections$n[2], " (", format(round(prior_infections$percent[2], 2), nsmall = 2), "%)"))
  
  return(summary)
}


### 2021 Ages 5-11


weekly_tests_2021_ages_5_11 <- read.csv(here("cleandata", "2021_age_5_11_all_tested.csv"))

table1$summary_2021_age_5_11 <- table1_summary(data = weekly_tests_2021_ages_5_11)


### 2022 Ages 5-11

weekly_tests_2022_ages_5_11 <- read.csv(here("cleandata", "2022_age_5_11_all_tested.csv"))

table1$summary_2022_age_5_11 <- table1_summary(data = weekly_tests_2022_ages_5_11)

### 2021 Ages 12-18

weekly_tests_2021_ages_12_18 <- read.csv(here("cleandata", "2021_age_12_18_all_tested.csv"))

table1$summary_2021_age_12_18 <- table1_summary(data = weekly_tests_2021_ages_12_18)

### 2022 Ages 12-18

weekly_tests_2022_ages_12_18 <- read.csv(here("cleandata", "2022_age_12_18_all_tested.csv"))

table1$summary_2022_age_12_18 <- table1_summary(data = weekly_tests_2022_ages_12_18)

table1

write.csv(table1, file = here("results", "together_table1.csv"))


























  ###### Other Summaries #####


num_tests_summary <- function(data) {
  testing_by_ID <- data %>% 
    group_by(ID) %>% 
    summarize(num_tests = sum(tested), 
              age = first(start_age), 
              vax_status = max(vax_status),
              result = max(result),
              race = first(race),
              gender = first(gender),
              prior_infections = max(prior_infections))
  
  overall <- testing_by_ID %>%
    summarize(median_tests = median(num_tests),
              min_tests = min(num_tests),
              max_tests = max(num_tests),
              n = n())
  
  
  vax_status <- testing_by_ID %>% 
    group_by(vax_status) %>% 
    summarize(median_tests = median(num_tests), 
              min_tests = min(num_tests),
              max_tests = max(num_tests),
              n = n())
  
  test_results <- testing_by_ID %>% 
    group_by(result) %>% 
    summarize(median_tests = median(num_tests), 
              min_tests = min(num_tests),
              max_tests = max(num_tests),
              n = n())
  
  age <- testing_by_ID %>% 
    group_by(age) %>% 
    summarize(median_tests = median(num_tests), 
              min_tests = min(num_tests),
              max_tests = max(num_tests),
              n = n())
  
  race <- testing_by_ID %>% 
    group_by(race) %>% 
    summarize(median_tests = median(num_tests), 
              min_tests = min(num_tests),
              max_tests = max(num_tests),
              n = n())
  
  gender <- testing_by_ID %>% 
    group_by(gender) %>% 
    summarize(median_tests = median(num_tests), 
              min_tests = min(num_tests),
              max_tests = max(num_tests),
              n = n())
  
  prior_infections <- testing_by_ID %>% 
    group_by(prior_infections > 1) %>% 
    summarize(median_tests = median(num_tests), 
              min_tests = min(num_tests),
              max_tests = max(num_tests),
              n = n())
  
  
  
  summary <- c(paste0(overall$median_tests, " (n = ", overall$n, ")"),
               "",
               paste0(vax_status$median_tests[2], " (n = ", vax_status$n[2], ")"),
               paste0(vax_status$median_tests[1], " (n = ", vax_status$n[1], ")"),
               "",
               paste0(test_results$median_tests[2], " (n = ", test_results$n[2], ")"),
               paste0(test_results$median_tests[1], " (n = ", test_results$n[1], ")"),
               "",
               paste0(age$median_tests[1], " (n = ", age$n[1], ")"),
               paste0(age$median_tests[2], " (n = ", age$n[2], ")"),
               paste0(age$median_tests[3], " (n = ", age$n[3], ")"),
               paste0(age$median_tests[4], " (n = ", age$n[4], ")"),
               paste0(age$median_tests[5], " (n = ", age$n[5], ")"),
               paste0(age$median_tests[6], " (n = ", age$n[6], ")"),
               paste0(age$median_tests[7], " (n = ", age$n[7], ")"),
               "",
               paste0(race$median_tests[2], " (n = ", race$n[2], ")"),
               paste0(race$median_tests[5], " (n = ", race$n[5], ")"),
               paste0(race$median_tests[1], " (n = ", race$n[1], ")"),
               paste0(race$median_tests[3], " (n = ", race$n[3], ")"),
               paste0(race$median_tests[4], " (n = ", race$n[4], ")"),
               "",
               paste0(gender$median_tests[1], " (n = ", gender$n[1], ")"),
               paste0(gender$median_tests[2], " (n = ", gender$n[2], ")"),
               "",
               paste0(prior_infections$median_tests[1], " (n = ", prior_infections$n[1], ")"),
               paste0(prior_infections$median_tests[2], " (n = ", prior_infections$n[2], ")"))
  
  return(summary)
}
