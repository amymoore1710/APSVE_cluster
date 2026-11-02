
# APS VE Figure 1
# 2025-12-10
# Run Locally
# Amy Moore

library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(latex2exp)
library(gridExtra)

#Plotting Color Defaults
color_main <- "skyblue3"
color_accent <- "#e9ecef"
color_alt1 <- "tomato"
color_alt2 <- "seagreen3"
color_alt3 <- "mediumpurple1"



### 2021 Ages 5-11
weekly_tests_2021_ages_5_11 <- read.csv(here("cleandata", "2021_age_5_11_weekly_testing_by_ID.csv"))
weekly_tests_2021_ages_5_11$school_year <- "2021-22"
weekly_tests_2021_ages_5_11$age_group <- "5-11"
weekly_tests_2021_ages_5_11$cohort <- "2021_ages_5_11"
weekly_tests_2021_ages_5_11$ID <- paste0(weekly_tests_2021_ages_5_11$ID, "A")


### 2022 Ages 5-11
weekly_tests_2022_ages_5_11 <- read.csv(here("cleandata", "2022_age_5_11_weekly_testing_by_ID.csv"))
weekly_tests_2022_ages_5_11$school_year <- "2022-23"
weekly_tests_2022_ages_5_11$age_group <- "5-11"
weekly_tests_2022_ages_5_11$cohort <- "2022_ages_5_11"
weekly_tests_2022_ages_5_11$ID <- paste0(weekly_tests_2022_ages_5_11$ID, "B")

### 2021 Ages 12-18
weekly_tests_2021_ages_12_18 <- read.csv(here("cleandata", "2021_age_12_18_weekly_testing_by_ID.csv"))
weekly_tests_2021_ages_12_18$school_year <- "2021-22"
weekly_tests_2021_ages_12_18$age_group <- "12-18"
weekly_tests_2021_ages_12_18$cohort <- "2021_ages_12_18"
weekly_tests_2021_ages_12_18$ID <- paste0(weekly_tests_2021_ages_12_18$ID, "C")

### 2022 Ages 12-18
weekly_tests_2022_ages_12_18 <- read.csv(here("cleandata", "2022_age_12_18_weekly_testing_by_ID.csv"))
weekly_tests_2022_ages_12_18$school_year <- "2022-23"
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
            num_missing = sum(1 - tested),
            total_weeks = n(),
            testing_rate = num_tests/total_weeks,
            age = first(start_age), 
            age_group = first(age_group),
            vax_status = max(vax_status),
            result = max(result),
            race = first(race),
            gender = first(gender),
            prior_infections = min(prior_infections),
            cohort = first(cohort),
            school_year = first(school_year))

df <- testing_by_ID %>%
  mutate(race = recode(race,
                       "Black or African American" = "Black",
                       "Hispanic/Latino"           = "Hispanic"),
         prior_infections = ifelse(prior_infections == 1, "Yes", "No"),
         vax_status = ifelse(vax_status == 1, "Vaccinated", "Unvaccinated")
         ) %>%
  rename(id = ID,
         year = school_year,
         prior_inf = prior_infections,
         vax = vax_status,
         possible = total_weeks,
         tests = num_tests,
         rate = testing_rate) %>%
  select(id, year, age_group, race, gender, prior_inf,
         vax, possible, tests, rate)


mean_testing_rate <- mean(df$tests/df$possible)



# ---- 2. Summarize: mean tests per person with 95% CI ------------------------
level_order <- c("Asian", "Black", "Hispanic", "White", "Other",
                 "F", "M", "No", "Yes", "Unvaccinated", "Vaccinated")

cohort_levels <- c("2021-22 Age 5-11", 
                   "2021-22 Age 12-18",
                   "2022-23 Age 5-11",
                   "2022-23 Age 12-18")

long <- df %>%
  mutate(cohort = factor(paste0(year, " Age ", age_group), levels = cohort_levels)) %>%
  mutate(across(c(race, gender, prior_inf, vax), as.character)) %>%
  pivot_longer(c(race, gender, prior_inf, vax),
               names_to = "covariate", values_to = "level") %>%
  mutate(covariate = recode(covariate,
                            race = "Race",
                            gender = "Gender", prior_inf = "Prior infection",
                            vax = "Vaccine status"),
         covariate = factor(covariate,
                            levels = c("Race", "Gender",
                                       "Prior infection", "Vaccine status")))

# in the summary step, use rate instead of tests
summ <- long %>%
  group_by(covariate, level, cohort) %>%
  summarise(mean = mean(rate),
            se   = sd(rate) / sqrt(n()),
            lo   = mean - 1.96 * se,
            hi   = mean + 1.96 * se,
            .groups = "drop") %>%
  mutate(year = substr(cohort, 1, 7),
         age_group = sub("^.* ", "", cohort))

# keep level order within each panel (Gender/Race both contain "Other")
summ <- summ %>%
  mutate(level = factor(level, levels = unique(level_order)))

mean_testing_rate <- df %>%
  group_by(age_group) %>%
  summarize(mean_rate = mean(rate))

# ---- 3. Plot ----------------------------------------------------------------
ggplot(summ, aes(x = level, y = mean, color = age_group, shape = year)) +
  geom_hline(yintercept = mean_testing_rate$mean_rate[1], linetype = "dotdash", linewidth = 1, color = color_alt2) +
  geom_hline(yintercept = mean_testing_rate$mean_rate[2], linetype = "longdash", linewidth = 1, color = color_main) +
  geom_pointrange(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.4),
                  linewidth = 1,
                  size = 0.8) +
  facet_grid(. ~ covariate, scales = "free_x", space = "free_x") +
  scale_shape_manual(name = "School year",
                     values = c("2021-22" = 16, 
                                "2022-23" = 17)) +
  scale_color_manual(name = "Age Group",
                     values = c("5-11" = color_main, #circle
                                "12-18" = color_alt2)) + #triangle
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "% of available tests completed",
       color = "School year") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")


ggsave(here("results","together_figure1_v3.png"), width = 11, height = 4.5, dpi = 300)









# vax_status <- testing_by_ID %>% ggplot(aes(x = as.factor(vax_status), y = num_tests, fill = as.factor(vax_status))) + 
#   geom_boxplot() +
#   scale_fill_manual(values = c("0" = color_alt1, "1" = color_main)) + 
#   xlab("Vaccination Status") +
#   ylab("Number of Tests") + 
#   theme(legend.position = "none") + 
#   scale_x_discrete(breaks = c(0,1), labels = c("Unvaccinated", "Vaccinated"))
#   
# vax_status
# 
# ggsave(here("results","Extra Figures","together_figure1_vax_status.png"), plot = vax_status, width = 6, height = 4, dpi = 300)
# 
# 
# 
# 
# test_results <- testing_by_ID %>% ggplot(aes(x = as.factor(result), y = num_tests, fill = as.factor(result))) + 
#   geom_boxplot() +
#   scale_fill_manual(values = c("0" = color_alt2, "1" = color_alt3)) + 
#   xlab("Test Result") +
#   ylab("Number of Tests") + 
#   theme(legend.position = "none") + 
#   scale_x_discrete(breaks = c(0,1), labels = c("Never positive", "At least one positive"))
# 
# test_results
# 
# ggsave(here("results","Extra Figures","together_figure1_test_results.png"), plot = test_results, width = 6, height = 4, dpi = 300)
# 
# 
# age <- testing_by_ID %>% ggplot(aes(x = as.factor(age), y = num_tests)) + 
#   geom_boxplot(fill = color_main) +
#   xlab("Age") +
#   ylab("Number of Tests") + 
#   theme(legend.position = "none")
# 
# age
# 
# ggsave(here("results","Extra Figures","together_figure1_age.png"), plot = age, width = 6, height = 4, dpi = 300)
# 
# 
# testing_by_ID$race <- fct_reorder(testing_by_ID$race, testing_by_ID$num_tests, .fun = median)
# 
# race <- testing_by_ID %>% ggplot(aes(x = as.factor(race), y = num_tests)) + 
#   geom_boxplot(fill = color_main) +
#   xlab("Race") +
#   ylab("Number of Tests") + 
#   theme(legend.position = "none")
# 
# race
# 
# ggsave(here("results","Extra Figures","together_figure1_race.png"), plot = race, width = 6, height = 4, dpi = 300)
# 
# 
# gender <- testing_by_ID %>% ggplot(aes(x = as.factor(gender), y = num_tests, fill = as.factor(gender))) + 
#   geom_boxplot() +
#   scale_fill_manual(values = c("F" = "plum2", "M" = "skyblue")) + 
#   xlab("Gender") +
#   ylab("Number of Tests") + 
#   theme(legend.position = "none") + 
#   scale_x_discrete(labels = c("F" = "Female", "M" = "Male"))
# 
# gender
# 
# ggsave(here("results","Extra Figures","together_figure1_gender.png"), plot = gender, width = 6, height = 4, dpi = 300)
# 
# 
# prior_infections <- testing_by_ID %>% ggplot(aes(x = as.factor(prior_infections >= 1), y = num_tests, fill = as.factor(prior_infections >= 1))) + 
#   geom_boxplot() +
#   scale_fill_manual(values = c("TRUE" = "lightsalmon", "FALSE" = "paleturquoise1")) + 
#   xlab("Prior Infections") +
#   ylab("Number of Tests") + 
#   theme(legend.position = "none") + 
#   scale_x_discrete(labels = c("TRUE" = "At least one", "FALSE" = "None"))
# 
# prior_infections
# 
# ggsave(here("results","Extra Figures","together_figure1_prior_infections.png"), plot = prior_infections, width = 6, height = 4, dpi = 300)
# 
# 
# school_year <- testing_by_ID %>% ggplot(aes(x = as.factor(school_year), y = num_tests, fill = as.factor(school_year))) + 
#   geom_boxplot() +
#   scale_fill_manual(values = c("2021" = "dodgerblue", "2022" = "hotpink")) + 
#   xlab("School Year") +
#   ylab("Number of Tests") + 
#   theme(legend.position = "none") + 
#   scale_x_discrete(labels = c("2021" = "2021-22", "2022" = "2022-23"))
# 
# school_year
# 
# ggsave(here("results","Extra Figures","together_figure1_school_year.png"), plot = school_year, width = 6, height = 4, dpi = 300)
# 
# 
# 
# 
# 
#   #Combined
# vax_status_data <- testing_by_ID %>% 
#   select(ID, num_tests, vax_status) %>% 
#   mutate(factor = "Vaccination Status",
#          vax_status = 1 - vax_status) %>%
#   rename(category = vax_status)
# 
# test_result_data <- testing_by_ID %>% 
#   select(ID, num_tests, result) %>% 
#   mutate(factor = "Test Results",
#          result = (1 - result) + 2) %>%
#   rename(category = result)
# 
# age_data <- testing_by_ID %>% 
#   select(ID, num_tests, age) %>% 
#   mutate(factor = "Age",
#          age = (24 - age)) %>%
#   rename(category = age)
# 
# race_data <- testing_by_ID %>% 
#   select(ID, num_tests, race) %>% 
#   mutate(factor = "Race",
#          race = (1 - as.numeric(as.factor(race))) + 18) %>%
#   rename(category = race)
# 
# gender_data <- testing_by_ID %>% 
#   select(ID, num_tests, gender) %>% 
#   mutate(factor = "Gender",
#          gender = (1 - as.numeric(as.factor(gender))) + 25) %>%
#   rename(category = gender)
# 
# p_infect_data <- testing_by_ID %>% 
#   select(ID, num_tests, prior_infections) %>% 
#   mutate(factor = "Prior Infections",
#          prior_infections = (1 - as.numeric((prior_infections >= 1))) + 26) %>%
#   rename(category = prior_infections)
# 
# # cohort_data <- testing_by_ID %>% 
# #   select(ID, num_tests, cohort) %>% 
# #   mutate(factor = "Cohort",
# #          cohort = as.numeric(factor(cohort, levels = c("2022_ages_12_18", 
# #                                                           "2021_ages_12_18",
# #                                                           "2022_ages_5_11",
# #                                                           "2021_ages_5_11"))) + 27) %>%
# #   rename(category = cohort)
# 
# school_year_data <- testing_by_ID %>%
#   select(ID, num_tests, school_year) %>%
#   mutate(factor = "School Year",
#          school_year = as.numeric(factor(school_year, levels = c("2022", "2021"))) + 27) %>%
#   rename(category = school_year)
# 
# 
# 
# combined_data <- rbind(vax_status_data, test_result_data, age_data, race_data, gender_data, p_infect_data, school_year_data)
# combined_data$factor <- factor(combined_data$factor, levels = c("Race", "Age", "Gender", 
#                                                                 "Prior Infections", "Test Results",
#                                                                 "Vaccination Status", "School Year"))
# 
# combined_data$box_width <- ifelse(combined_data$factor == "Age", 1, 0.5)
# 
# new_labels <- c("Black \n Hispanic/Latino \n Other \n Asian \n White", 
#                 "Age (5 to 18)",
#                 "Female \n Male", 
#                 "No Prior Infections \n At Least One Prior Infection",
#                 "Never Positive \n At Least One Positive", 
#                 "Unvaccinated \n Vaccinated",
#                 "2021\n 2022")
# 
# # combined_plot <- ggplot() + 
# #   geom_boxplot(
# #     data = school_year_data,
# #     aes(x = factor, y = num_tests, fill = as.factor(category)),
# #     width = 0.5,
# #     position = position_dodge(width = 0.6)) +
# #   geom_boxplot(
# #     data = vax_status_data, 
# #     aes(x = factor, y = num_tests, fill = as.factor(category)),
# #     width = 0.5,
# #     position = position_dodge(width = 0.6)) +
# #   geom_boxplot(
# #     data = test_result_data,
# #     aes(x = factor, y = num_tests, fill = as.factor(category)),
# #     width = 0.5,
# #     position = position_dodge(width = 0.6)) +
# #   geom_boxplot(
# #     data = p_infect_data,
# #     aes(x = factor, y = num_tests, fill = as.factor(category)),
# #     width = 0.5,
# #     position = position_dodge(width = 0.6)) +
# #   geom_boxplot(
# #     data = gender_data,
# #     aes(x = factor, y = num_tests, fill = as.factor(category)),
# #     width = 0.5,
# #     position = position_dodge(width = 0.6)) +
# #   geom_boxplot(
# #     data = age_data,
# #     aes(x = factor, y = num_tests, fill = as.factor(category)),
# #     width = 2,
# #     position = position_dodge(width = 2.1)) +
# #   geom_boxplot(
# #     data = race_data,
# #     aes(x = factor, y = num_tests, fill = as.factor(category)),
# #     width = 1,
# #     position = position_dodge(width = 1.1)) +
# #   scale_fill_manual(values = c("0" = color_main, "1" = color_alt1,
# #                                "2" = color_alt3, "3" = color_alt2,
# #                                "4" = "grey", "5" = "grey", 
# #                                "6" = "grey", "7" = "grey",
# #                                "8" = "grey", "9" = "grey",
# #                                "10" = "grey", "11" = "grey", 
# #                                "12" = "grey", "13" = "grey",
# #                                "14" = "grey", "15" = "grey", 
# #                                "16" = "grey", "17" = "grey",
# #                                "18" = "grey", "19" = "grey",
# #                                "20" = "grey", "21" = "grey",
# #                                "22" = "grey", "23" = "grey",
# #                                "24" = "skyblue", "25" = "plum2",
# #                                "26" = "lightsalmon", "27" = "paleturquoise1",
# #                                "28" = "hotpink", "29" = "dodgerblue",
# #                                "30" = "palegreen1", "31" = "orange")) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = new_labels, expand = c(0.1, 0.2)) +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
# 
# # combined_plot <- combined_data %>% ggplot() + 
# #   geom_boxplot(
# #     aes(x = factor, y = num_tests, fill = as.factor(category)),
# #     width = 1,
# #     position = position_dodge(width = 1.1)) +
# #   scale_fill_manual(values = c("0" = color_main, "1" = color_alt1,
# #                                "2" = color_alt3, "3" = color_alt2,
# #                                "4" = "grey", "5" = "grey", 
# #                                "6" = "grey", "7" = "grey",
# #                                "8" = "grey", "9" = "grey",
# #                                "10" = "grey", "11" = "grey", 
# #                                "12" = "grey", "13" = "grey",
# #                                "14" = "grey", "15" = "grey", 
# #                                "16" = "grey", "17" = "grey",
# #                                "18" = "grey", "19" = "grey",
# #                                "20" = "grey", "21" = "grey",
# #                                "22" = "grey", "23" = "grey",
# #                                "24" = "skyblue", "25" = "plum2",
# #                                "26" = "lightsalmon", "27" = "paleturquoise1",
# #                                "28" = "hotpink", "29" = "dodgerblue",
# #                                "30" = "palegreen1", "31" = "orange")) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = new_labels) +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) + 
# #   facet_grid(rows = vars(factor), scales = "free", space = "free")
# # 
# # combined_plot
# 
# 
# combined_plot <- combined_data %>% ggplot() + 
#   geom_boxplot(
#     aes(x = factor, y = num_tests, fill = as.factor(category))) +
#   scale_fill_manual(values = c("0" = color_main, "1" = color_alt1,
#                                "2" = color_alt3, "3" = color_alt2,
#                                "4" = "grey", "5" = "grey", 
#                                "6" = "grey", "7" = "grey",
#                                "8" = "grey", "9" = "grey",
#                                "10" = "grey", "11" = "grey", 
#                                "12" = "grey", "13" = "grey",
#                                "14" = "grey", "15" = "grey", 
#                                "16" = "grey", "17" = "grey",
#                                "18" = "grey", "19" = "grey",
#                                "20" = "grey", "21" = "grey",
#                                "22" = "grey", "23" = "grey",
#                                "24" = "skyblue", "25" = "plum2",
#                                "26" = "lightsalmon", "27" = "paleturquoise1",
#                                "28" = "hotpink", "29" = "dodgerblue",
#                                "30" = "palegreen1", "31" = "orange")) + 
#   xlab("Covariates") +
#   ylab("Number of Tests") + 
#   coord_flip() +
#   theme(legend.position = "none") + 
#   scale_x_discrete(labels = new_labels) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
# 
# combined_plot
# 
# ggsave(here("results","together_figure1.png"), plot = combined_plot, width = 6, height = 4, dpi = 300)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # #Combined
# # vax_status_data <- testing_by_ID %>% 
# #   select(ID, num_tests, vax_status) %>% 
# #   mutate(factor = "Vaccination Status",
# #          vax_status = 1 - vax_status) %>%
# #   rename(category = vax_status)
# # 
# # test_result_data <- testing_by_ID %>% 
# #   select(ID, num_tests, result) %>% 
# #   mutate(factor = "Test Results",
# #          result = result) %>%
# #   rename(category = result)
# # 
# # age_data <- testing_by_ID %>% 
# #   select(ID, num_tests, age) %>% 
# #   mutate(factor = "Age",
# #          age = (age)) %>%
# #   rename(category = age)
# # 
# # race_data <- testing_by_ID %>% 
# #   select(ID, num_tests, race) %>% 
# #   mutate(factor = "Race",
# #          race = (as.numeric(as.factor(race)))) %>%
# #   rename(category = race)
# # 
# # gender_data <- testing_by_ID %>% 
# #   select(ID, num_tests, gender) %>% 
# #   mutate(factor = "Gender",
# #          gender = (as.numeric(as.factor(gender)))) %>%
# #   rename(category = gender)
# # 
# # p_infect_data <- testing_by_ID %>% 
# #   select(ID, num_tests, prior_infections) %>% 
# #   mutate(factor = "Prior Infections",
# #          prior_infections = (as.numeric((prior_infections >= 1)))) %>%
# #   rename(category = prior_infections)
# # 
# # school_year_data <- testing_by_ID %>%
# #   select(ID, num_tests, school_year) %>%
# #   mutate(factor = "School Year",
# #          school_year = as.numeric(factor(school_year, levels = c("2021", "2022")))) %>%
# #   rename(category = school_year)
# # 
# # 
# # p_race <- race_data %>% ggplot(aes(x = factor, y = num_tests, fill = as.factor(category))) +
# #   geom_boxplot(width = 1) +
# #   scale_fill_manual(values = c("1" = "grey", "2" = "grey", 
# #                                "3" = "grey", "4" = "grey",
# #                                "5" = "grey")) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = "White \n Asian \n Other \n Hspanic/Latino \n Black ") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
# #         plot.margin = margin(150, 5, 150,5)) +
# #   facet_wrap(~factor)
# # p_race
# # 
# # p_age <- age_data %>% ggplot(aes(x = factor, y = num_tests, fill = as.factor(category))) +
# #   geom_boxplot(width = 1) +
# #   scale_fill_manual(values = c("5" = "grey", "6" = "grey", 
# #                                "7" = "grey", "8" = "grey",
# #                                "9" = "grey", "10" = "grey",
# #                                "11" = "grey", "12" = "grey",
# #                                "13" = "grey", "14" = "grey",
# #                                "15" = "grey", "16" = "grey",
# #                                "17" = "grey", "18" = "grey")) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = "Age (5 to 18)") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
# #         plot.margin = margin(125, 5, 125,5)) +
# #   facet_wrap(~factor)
# # p_age
# # 
# # p_gender <- gender_data %>% ggplot(aes(x = factor, y = num_tests, fill = as.factor(category))) +
# #   geom_boxplot(width = 1) +
# #   scale_fill_manual(values = c("2" = "skyblue", "1" = "plum2")) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = "Male \n Female ") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
# #         plot.margin = margin(150, 5, 150,5)) +
# #   facet_wrap(~factor)
# # p_gender
# # 
# # p_infect <- p_infect_data %>% ggplot(aes(x = factor, y = num_tests, fill = as.factor(category))) +
# #   geom_boxplot(width = 1) +
# #   scale_fill_manual(values = c("1" = "lightsalmon", "0" = "paleturquoise1")) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = "At Least One Prior Infection \n No Prior Infections ") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
# #         plot.margin = margin(150, 5, 150,5)) +
# #   facet_wrap(~factor)
# # p_infect
# # 
# # p_result <- test_result_data %>% ggplot(aes(x = factor, y = num_tests, fill = as.factor(category))) +
# #   geom_boxplot(width = 1) +
# #   scale_fill_manual(values = c("1" = color_alt3, "0" = color_alt2)) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = "At Least One Positive \n Never Positive ") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
# #         plot.margin = margin(150, 5, 150,5)) +
# #   facet_wrap(~factor)
# # p_result
# # 
# # p_vax <- vax_status_data %>% ggplot(aes(x = factor, y = num_tests, fill = as.factor(category))) +
# #   geom_boxplot(width = 1) +
# #   scale_fill_manual(values = c("1" = color_alt1, "0" = color_main)) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = "Unvaccinated \n Vaccinated ") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
# #         plot.margin = margin(150, 50, 150,50)) +
# #   facet_wrap(~factor)
# # p_vax
# # 
# # p_year <- school_year_data %>% ggplot(aes(x = factor, y = num_tests, fill = as.factor(category))) +
# #   geom_boxplot(width = 1) +
# #   scale_fill_manual(values = c("1" = "palegreen1", "2" = "orange")) + 
# #   xlab("Covariates") +
# #   ylab("Number of Tests") + 
# #   coord_flip() +
# #   theme(legend.position = "none") + 
# #   scale_x_discrete(labels = "2022-23 \n 2021-22 ") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
# #         plot.margin = margin(150, 50, 150,50)) +
# #   facet_wrap(~factor)
# # p_year
# 
# 
# 
