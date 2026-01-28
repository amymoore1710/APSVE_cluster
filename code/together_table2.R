
# APS VE Table 2 - Missing Tests
# 2026-01-15
# Run Locally
# Amy Moore

library(here)
library(dplyr)

# Making a dataframe to fill out:

table2 <- data.frame(summary_2021_age_5_11 = rep(NA, 7),
                     summary_ST_2021_age_5_11 = rep(NA,7),
                     summary_2022_age_5_11 = rep(NA, 7),
                     summary_ST_2022_age_5_11 = rep(NA,7),
                     summary_2021_age_12_18 = rep(NA, 7),
                     summary_ST_2021_age_12_18 = rep(NA,7),
                     summary_2022_age_12_18 = rep(NA, 7),
                     summary_ST_2022_age_12_18 = rep(NA,7))


table2_summary <- function(data) {
  summary_by_ID <- data %>% group_by(ID) %>% summarize(num_weeks_tested = n())
  num_IDs <- nrow(summary_by_ID)
  
  six_num_summary <- unclass(summary(summary_by_ID$num_weeks_tested))
  
  max <- six_num_summary[6]
  
  six_num_summary <- max - six_num_summary
  
  full_summary <- c(paste0(round(six_num_summary[6], digits = 2)), 
                    paste0(round(six_num_summary[5], digits = 2)),
                    paste0(round(six_num_summary[3], digits = 2)),
                    paste0(round(six_num_summary[4], digits = 2)),
                    paste0(round(six_num_summary[2], digits = 2)),
                    paste0(round(six_num_summary[1], digits = 2)),
                    paste0(round(num_IDs, digits = 0)))
  
  return(full_summary)
  
}


### 2021 Ages 5-11


weekly_tests_2021_ages_5_11 <- read.csv(here("cleandata", "2021_age_5_11_all_tested.csv"))

table2$summary_2021_age_5_11 <- table2_summary(data = weekly_tests_2021_ages_5_11)

weekly_tests_ST_2021_ages_5_11 <- read.csv(here("cleandata", "2021_age_5_11_ST_all_tested.csv"))

table2$summary_ST_2021_age_5_11 <- table2_summary(data = weekly_tests_ST_2021_ages_5_11)

### 2022 Ages 5-11

weekly_tests_2022_ages_5_11 <- read.csv(here("cleandata", "2022_age_5_11_all_tested.csv"))

table2$summary_2022_age_5_11 <- table2_summary(data = weekly_tests_2022_ages_5_11)

weekly_tests_ST_2022_ages_5_11 <- read.csv(here("cleandata", "2022_age_5_11_ST_all_tested.csv"))

table2$summary_ST_2022_age_5_11 <- table2_summary(data = weekly_tests_ST_2022_ages_5_11)

### 2021 Ages 12-18

weekly_tests_2021_ages_12_18 <- read.csv(here("cleandata", "2021_age_12_18_all_tested.csv"))

table2$summary_2021_age_12_18 <- table2_summary(data = weekly_tests_2021_ages_12_18)

weekly_tests_ST_2021_ages_12_18 <- read.csv(here("cleandata", "2021_age_12_18_ST_all_tested.csv"))

table2$summary_ST_2021_age_12_18 <- table2_summary(data = weekly_tests_ST_2021_ages_12_18)

### 2022 Ages 12-18

weekly_tests_2022_ages_12_18 <- read.csv(here("cleandata", "2022_age_12_18_all_tested.csv"))

table2$summary_2022_age_12_18 <- table2_summary(data = weekly_tests_2022_ages_12_18)

weekly_tests_ST_2022_ages_12_18 <- read.csv(here("cleandata", "2022_age_12_18_ST_all_tested.csv"))

table2$summary_ST_2022_age_12_18 <- table2_summary(data = weekly_tests_ST_2022_ages_12_18)





rownames(table2) <- c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max", "N students")
colnames(table2) <- c("2021_ages_5_11", "ST_2021_ages_5_11",
                      "2022_ages_5_11", "ST_2022_ages_5_11",
                      "2021_ages_12_18", "ST_2021_ages_12_18",
                      "2022_ages_12_18", "ST_2022_ages_12_18")
table2

write.csv(table2, file = here("results", "together_table2.csv"))











