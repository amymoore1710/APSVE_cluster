
# APS VE Figure 1
# 2025-12-10
# Run Locally
# Amy Moore

library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(latex2exp)

#Plotting Color Defaults
color_main <- "skyblue3"
color_accent <- "#e9ecef"
color_alt1 <- "tomato"
color_alt2 <- "seagreen3"
color_alt3 <- "mediumpurple1"



### 2021 Ages 5-11
weekly_tests_2021_ages_5_11 <- read.csv(here("cleandata", "2021_age_5_11_all_tested.csv"))
weekly_tests_2021_ages_5_11$school_year <- "2021"
weekly_tests_2021_ages_5_11$age_group <- "5-11"
weekly_tests_2021_ages_5_11$cohort <- "2021_ages_5_11"
weekly_tests_2021_ages_5_11$ID <- paste0(weekly_tests_2021_ages_5_11$ID, "A")


### 2022 Ages 5-11
weekly_tests_2022_ages_5_11 <- read.csv(here("cleandata", "2022_age_5_11_all_tested.csv"))
weekly_tests_2022_ages_5_11$school_year <- "2022"
weekly_tests_2022_ages_5_11$age_group <- "5-11"
weekly_tests_2022_ages_5_11$cohort <- "2022_ages_5_11"
weekly_tests_2022_ages_5_11$ID <- paste0(weekly_tests_2022_ages_5_11$ID, "B")

### 2021 Ages 12-18
weekly_tests_2021_ages_12_18 <- read.csv(here("cleandata", "2021_age_12_18_all_tested.csv"))
weekly_tests_2021_ages_12_18$school_year <- "2021"
weekly_tests_2021_ages_12_18$age_group <- "12-18"
weekly_tests_2021_ages_12_18$cohort <- "2021_ages_12_18"
weekly_tests_2021_ages_12_18$ID <- paste0(weekly_tests_2021_ages_12_18$ID, "C")

### 2022 Ages 12-18
weekly_tests_2022_ages_12_18 <- read.csv(here("cleandata", "2022_age_12_18_all_tested.csv"))
weekly_tests_2022_ages_12_18$school_year <- "2022"
weekly_tests_2022_ages_12_18$age_group <- "12-18"
weekly_tests_2022_ages_12_18$cohort <- "2022_ages_12_18"
weekly_tests_2022_ages_12_18$ID <- paste0(weekly_tests_2022_ages_12_18$ID, "D")


data <- rbind(weekly_tests_2021_ages_5_11,
              weekly_tests_2022_ages_5_11,
              weekly_tests_2021_ages_12_18,
              weekly_tests_2022_ages_12_18)

testing_by_ID <- data %>% 
  group_by(ID) %>% 
  summarize(num_tests = sum(tested), 
            age = first(start_age), 
            vax_status = max(vax_status),
            result = max(result),
            race = first(race),
            gender = first(gender),
            prior_infections = max(prior_infections),
            cohort = first(cohort))

vax_status <- testing_by_ID %>% ggplot(aes(x = as.factor(vax_status), y = num_tests, fill = as.factor(vax_status))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = color_alt1, "1" = color_main)) + 
  xlab("Vaccination Status") +
  ylab("Number of Tests") + 
  theme(legend.position = "none") + 
  scale_x_discrete(breaks = c(0,1), labels = c("Unvaccinated", "Vaccinated"))
  
vax_status

test_results <- testing_by_ID %>% ggplot(aes(x = as.factor(result), y = num_tests, fill = as.factor(result))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = color_alt2, "1" = color_alt3)) + 
  xlab("Test Result") +
  ylab("Number of Tests") + 
  theme(legend.position = "none") + 
  scale_x_discrete(breaks = c(0,1), labels = c("Never positive", "At least one positive"))

test_results

age <- testing_by_ID %>% ggplot(aes(x = as.factor(age), y = num_tests)) + 
  geom_boxplot(fill = color_main) +
  xlab("Age") +
  ylab("Number of Tests") + 
  theme(legend.position = "none")

age

testing_by_ID$race <- fct_reorder(testing_by_ID$race, testing_by_ID$num_tests, .fun = median)

race <- testing_by_ID %>% ggplot(aes(x = as.factor(race), y = num_tests)) + 
  geom_boxplot(fill = color_main) +
  xlab("Race") +
  ylab("Number of Tests") + 
  theme(legend.position = "none")

race

gender <- testing_by_ID %>% ggplot(aes(x = as.factor(gender), y = num_tests, fill = as.factor(gender))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("F" = color_alt3, "M" = color_main)) + 
  xlab("Gender") +
  ylab("Number of Tests") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = c("F" = "Female", "M" = "Male"))

gender

prior_infections <- testing_by_ID %>% ggplot(aes(x = as.factor(prior_infections > 1), y = num_tests, fill = as.factor(prior_infections > 1))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("TRUE" = color_alt3, "FALSE" = color_alt2)) + 
  xlab("Test Result") +
  ylab("Number of Tests") + 
  theme(legend.position = "none") + 
  scale_x_discrete(labels = c("TRUE" = "At least one", "FALSE" = "None"))

prior_infections





  #Combined
vax_status_data <- testing_by_ID %>% 
  select(ID, num_tests, vax_status) %>% 
  mutate(factor = "Vaccination Status",
         vax_status = 1 - vax_status) %>%
  rename(category = vax_status)

test_result_data <- testing_by_ID %>% 
  select(ID, num_tests, result) %>% 
  mutate(factor = "Test Results",
         result = (1 - result) + 2) %>%
  rename(category = result)

age_data <- testing_by_ID %>% 
  select(ID, num_tests, age) %>% 
  mutate(factor = "Age",
         age = (24 - age)) %>%
  rename(category = age)

race_data <- testing_by_ID %>% 
  select(ID, num_tests, race) %>% 
  mutate(factor = "Race",
         race = (1 - as.numeric(as.factor(race))) + 18) %>%
  rename(category = race)

gender_data <- testing_by_ID %>% 
  select(ID, num_tests, gender) %>% 
  mutate(factor = "Gender",
         gender = (1 - as.numeric(as.factor(gender))) + 25) %>%
  rename(category = gender)

p_infect_data <- testing_by_ID %>% 
  select(ID, num_tests, prior_infections) %>% 
  mutate(factor = "Prior Infections",
         prior_infections = (1 - as.numeric((prior_infections > 1))) + 26) %>%
  rename(category = prior_infections)

cohort_data <- testing_by_ID %>% 
  select(ID, num_tests, cohort) %>% 
  mutate(factor = "Cohort",
         cohort = as.numeric(factor(cohort, levels = c("2022_ages_12_18", 
                                                          "2021_ages_12_18",
                                                          "2022_ages_5_11",
                                                          "2021_ages_5_11"))) + 27) %>%
  rename(category = cohort)



combined_data <- rbind(vax_status_data, test_result_data, age_data, race_data, gender_data, p_infect_data, cohort_data)
combined_data$factor <- factor(combined_data$factor, levels = c("Race", "Age", "Gender", 
                                                                "Prior Infections", "Test Results",
                                                                "Vaccination Status", "Cohort"))

new_labels <- c("Black \n Hispanic/Latino \n Other \n Asian \n White", 
                "Age (5 to 18)",
                "Female \n Male", 
                "No Prior Infections \n At Least One Prior Infection",
                "Never Positive \n At Least One Positive", 
                "Unvaccinated \n Vaccinated",
                "2021 age 5-11 \n 2022 age 5-11 \n 2021 age 12-18 \n 2022 age 12-18")

combined_plot <- combined_data %>% ggplot(aes(x = factor, y = num_tests, fill = as.factor(category))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = color_main, "1" = color_alt1,
                               "2" = color_alt3, "3" = color_alt2,
                               "4" = "grey", "5" = "grey", 
                               "6" = "grey", "7" = "grey",
                               "8" = "grey", "9" = "grey",
                               "10" = "grey", "11" = "grey", 
                               "12" = "grey", "13" = "grey",
                               "14" = "grey", "15" = "grey", 
                               "16" = "grey", "17" = "grey",
                               "18" = "grey", "19" = "grey",
                               "20" = "grey", "21" = "grey",
                               "22" = "grey", "23" = "grey",
                               "24" = "skyblue", "25" = "plum2",
                               "26" = "lightsalmon", "27" = "paleturquoise1",
                               "28" = "hotpink", "29" = "dodgerblue",
                               "30" = "palegreen1", "31" = "orange")) + 
  xlab("Covariates") +
  ylab("Number of Tests") + 
  coord_flip() +
  theme(legend.position = "none") + 
  scale_x_discrete(labels = new_labels) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

combined_plot

ggsave(here("results","2021_age_5_11_figure1.png"), plot = combined_plot, width = 6, height = 4, dpi = 300)


# ### 2022 Ages 5-11
# 
# weekly_tests_2022_ages_5_11 <- read.csv(here("cleandata", "2022_age_5_11_all_tested.csv"))
# 
# table1$summary_2022_age_5_11 <- table1_summary(data = weekly_tests_2022_ages_5_11)
# 
# ### 2021 Ages 12-18
# 
# weekly_tests_2021_ages_12_18 <- read.csv(here("cleandata", "2021_age_12_18_all_tested.csv"))
# 
# table1$summary_2021_age_12_18 <- table1_summary(data = weekly_tests_2021_ages_12_18)
# 
# ### 2022 Ages 12-18
# 
# weekly_tests_2022_ages_12_18 <- read.csv(here("cleandata", "2022_age_12_18_all_tested.csv"))
# 
# table1$summary_2022_age_12_18 <- table1_summary(data = weekly_tests_2022_ages_12_18)
# 
# 
# write.csv(table1, file = here("results", "together_table1.csv"))


