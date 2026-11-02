

# APS VE Figure 3 - Testing Behavior Covariates
# 2026-02-04
# Run Locally
# Amy Moore

library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)

#Plotting Color Defaults
color_main <- "skyblue3"
color_accent <- "#e9ecef"
color_alt1 <- "tomato"
color_alt2 <- "seagreen3"
color_alt3 <- "mediumpurple1"
color_alt4 <- "deepskyblue4"
color_alt5 <- "tomato4"


VScohort.tested.2021.5to11 <- read.csv(here("cleandata", "2021_age_5_11_all_tested.csv"))
VScohort.raw.2021.5to11 <- read.csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))

behavior.byWeek.2021.5to11 <- VScohort.raw.2021.5to11 %>% 
  group_by(week, vax_status) %>% 
  summarize(num_tested = sum(tested),
            no_history = sum(ifelse(no_history ==1 & tested == 1, 1, 0)),
            num_tests = mean(num_prev_test),
            time_since_last = mean(time_since_last),
            tests_in_28 = mean(tests_in_28),
            tests_in_14 = mean(tests_in_14),
            test_density = mean(test_density),
            adj_test_density = mean(adj_test_density)) %>%
  mutate(time = "Time-varying")

# behavior.byWeek.2021.5to11 <- VScohort.tested.2021.5to11 %>% 
#   group_by(week, vax_status) %>% 
#   summarize(num_tested = n(),
#     no_history = sum(no_history),
#     num_tests = mean(num_prev_test),
#     time_since_last = mean(time_since_last),
#     tests_in_28 = mean(tests_in_28),
#     tests_in_14 = mean(tests_in_14),
#     test_density = mean(test_density),
#     adj_test_density = mean(adj_test_density)) %>%
#   mutate(time = "Time-varying")

weeks.2021 <- unique(behavior.byWeek.2021.5to11$week)

  #Average Testing behavior is equal for all observations for the same students so just grabbing the last test reported for each student
behavior.avg.2021.5to11 <- VScohort.tested.2021.5to11 %>% 
  group_by(ID) %>%
  slice_tail(n = 1) %>%
  group_by(vax_status) %>%
  summarize(num_tested = n(),
            no_history = sum(no_history),
            num_tests = mean(total_tests),
            time_since_last = mean(avg_time_since_last),
            tests_in_28 = mean(avg_tests_in_28),
            tests_in_14 = mean(avg_tests_in_14),
            test_density = mean(avg_test_density),
            adj_test_density = mean(avg_adj_test_density)) %>%
  mutate(count_col = 34) %>% uncount(count_col) %>%
  mutate(week = rep(weeks.2021, 2),
         time = "Average over Time")


behavior.combined.2021.5to11 <- rbind(behavior.byWeek.2021.5to11, behavior.avg.2021.5to11)
behavior.combined.2021.5to11$year <- "2021"
behavior.combined.2021.5to11$age_group <- "5to11"

VScohort.tested.2021.12to18 <- read.csv(here("cleandata", "2021_age_12_18_all_tested.csv"))
VScohort.raw.2021.12to18 <- read.csv(here("cleandata", "2021_age_12_18_predicted_propensity_scores.csv"))

behavior.byWeek.2021.12to18 <- VScohort.raw.2021.12to18 %>% 
  group_by(week, vax_status) %>% 
  summarize(num_tested = sum(tested),
            no_history = sum(ifelse(no_history ==1 & tested == 1, 1, 0)),
            num_tests = mean(num_prev_test),
            time_since_last = mean(time_since_last),
            tests_in_28 = mean(tests_in_28),
            tests_in_14 = mean(tests_in_14),
            test_density = mean(test_density),
            adj_test_density = mean(adj_test_density)) %>%
  mutate(time = "Time-varying")


# behavior.byWeek.2021.12to18 <- VScohort.tested.2021.12to18 %>% 
#   group_by(week, vax_status) %>% 
#   summarize(num_tested = n(),
#             no_history = sum(no_history),
#             num_tests = mean(num_prev_test),
#             time_since_last = mean(time_since_last),
#             tests_in_28 = mean(tests_in_28),
#             tests_in_14 = mean(tests_in_14),
#             test_density = mean(test_density),
#             adj_test_density = mean(adj_test_density)) %>%
#   mutate(time = "Time-varying")

#Average Testing behavior is equal for all observations for the same students so just grabbing the last test reported for each student
behavior.avg.2021.12to18 <- VScohort.tested.2021.12to18 %>% 
  group_by(ID) %>%
  slice_tail(n = 1) %>%
  group_by(vax_status) %>%
  summarize(num_tested = n(),
            no_history = sum(no_history),
            num_tests = mean(total_tests),
            time_since_last = mean(avg_time_since_last),
            tests_in_28 = mean(avg_tests_in_28),
            tests_in_14 = mean(avg_tests_in_14),
            test_density = mean(avg_test_density),
            adj_test_density = mean(avg_adj_test_density)) %>%
  mutate(count_col = 34) %>% uncount(count_col) %>%
  mutate(week = rep(weeks.2021, 2),
         time = "Average over Time")


behavior.combined.2021.12to18 <- rbind(behavior.byWeek.2021.12to18, behavior.avg.2021.12to18)
behavior.combined.2021.12to18$year <- "2021"
behavior.combined.2021.12to18$age_group <- "12to18"





VScohort.tested.2022.5to11 <- read.csv(here("cleandata", "2022_age_5_11_all_tested.csv"))
VScohort.tested.2022.5to11$week <- VScohort.tested.2022.5to11$week + 48
VScohort.raw.2022.5to11 <- read.csv(here("cleandata", "2022_age_5_11_predicted_propensity_scores.csv"))
VScohort.raw.2022.5to11$week <- VScohort.raw.2022.5to11$week + 48

behavior.byWeek.2022.5to11 <- VScohort.raw.2022.5to11 %>% 
  group_by(week, vax_status) %>% 
  summarize(num_tested = sum(tested),
            no_history = sum(ifelse(no_history ==1 & tested == 1, 1, 0)),
            num_tests = mean(num_prev_test),
            time_since_last = mean(time_since_last),
            tests_in_28 = mean(tests_in_28),
            tests_in_14 = mean(tests_in_14),
            test_density = mean(test_density),
            adj_test_density = mean(adj_test_density)) %>%
  mutate(time = "Time-varying")



# behavior.byWeek.2022.5to11 <- VScohort.tested.2022.5to11 %>% 
#   group_by(week, vax_status) %>% 
#   summarize(num_tested = n(),
#             no_history = sum(no_history),
#             num_tests = mean(num_prev_test),
#             time_since_last = mean(time_since_last),
#             tests_in_28 = mean(tests_in_28),
#             tests_in_14 = mean(tests_in_14),
#             test_density = mean(test_density),
#             adj_test_density = mean(adj_test_density)) %>%
#   mutate(time = "Time-varying")

weeks.2022 <- unique(behavior.byWeek.2022.5to11$week)

#Average Testing behavior is equal for all observations for the same students so just grabbing the last test reported for each student
behavior.avg.2022.5to11 <- VScohort.tested.2022.5to11 %>% 
  group_by(ID) %>%
  slice_tail(n = 1) %>%
  group_by(vax_status) %>%
  summarize(num_tested = n(),
            no_history = sum(no_history),
            num_tests = mean(total_tests),
            time_since_last = mean(avg_time_since_last),
            tests_in_28 = mean(avg_tests_in_28),
            tests_in_14 = mean(avg_tests_in_14),
            test_density = mean(avg_test_density),
            adj_test_density = mean(avg_adj_test_density)) %>%
  mutate(count_col = 37) %>% uncount(count_col) %>%
  mutate(week = rep(weeks.2022, 2),
         time = "Average over Time")


behavior.combined.2022.5to11 <- rbind(behavior.byWeek.2022.5to11, behavior.avg.2022.5to11)
behavior.combined.2022.5to11$year <- "2022"
behavior.combined.2022.5to11$age_group <- "5to11"

VScohort.tested.2022.12to18 <- read.csv(here("cleandata", "2022_age_12_18_all_tested.csv"))
VScohort.tested.2022.12to18$week <- VScohort.tested.2022.12to18$week + 48
#2022-23 Ages 12-18 has to extra weeks included that aren't included in 5 to 11 (only 1 student tested these weeks)
VScohort.tested.2022.12to18 <- VScohort.tested.2022.12to18 %>% filter(!week %in% c(58, 77))
VScohort.raw.2022.12to18 <- read.csv(here("cleandata", "2022_age_12_18_predicted_propensity_scores.csv"))
VScohort.raw.2022.12to18$week <- VScohort.raw.2022.12to18$week + 48

behavior.byWeek.2022.12to18 <- VScohort.raw.2022.12to18 %>% 
  group_by(week, vax_status) %>% 
  summarize(num_tested = sum(tested),
            no_history = sum(ifelse(no_history ==1 & tested == 1, 1, 0)),
            num_tests = mean(num_prev_test),
            time_since_last = mean(time_since_last),
            tests_in_28 = mean(tests_in_28),
            tests_in_14 = mean(tests_in_14),
            test_density = mean(test_density),
            adj_test_density = mean(adj_test_density)) %>%
  mutate(time = "Time-varying")


# behavior.byWeek.2022.12to18 <- VScohort.tested.2022.12to18 %>% 
#   group_by(week, vax_status) %>% 
#   summarize(num_tested = n(),
#             no_history = sum(no_history),
#             num_tests = mean(num_prev_test),
#             time_since_last = mean(time_since_last),
#             tests_in_28 = mean(tests_in_28),
#             tests_in_14 = mean(tests_in_14),
#             test_density = mean(test_density),
#             adj_test_density = mean(adj_test_density)) %>%
#   mutate(time = "Time-varying")

#Average Testing behavior is equal for all observations for the same students so just grabbing the last test reported for each student
behavior.avg.2022.12to18 <- VScohort.tested.2022.12to18 %>% 
  group_by(ID) %>%
  slice_tail(n = 1) %>%
  group_by(vax_status) %>%
  summarize(num_tested = n(),
            no_history = sum(no_history),
            num_tests = mean(total_tests),
            time_since_last = mean(avg_time_since_last),
            tests_in_28 = mean(avg_tests_in_28),
            tests_in_14 = mean(avg_tests_in_14),
            test_density = mean(avg_test_density),
            adj_test_density = mean(avg_adj_test_density)) %>%
  mutate(count_col = 37) %>% uncount(count_col) %>%
  mutate(week = rep(weeks.2022, 2),
         time = "Average over Time")


behavior.combined.2022.12to18 <- rbind(behavior.byWeek.2022.12to18, behavior.avg.2022.12to18)
behavior.combined.2022.12to18$year <- "2022"
behavior.combined.2022.12to18$age_group <- "12to18"


behavior.combined <- rbind(behavior.combined.2021.5to11, behavior.combined.2021.12to18,
                           behavior.combined.2022.5to11, behavior.combined.2022.12to18)



behavior.combined$grouping <- paste0(behavior.combined$year, ".",
                                     behavior.combined$age_group, ".", 
                                     behavior.combined$time, ".", 
                                     behavior.combined$vax_status)
behavior.combined$vax_status_word <- ifelse(behavior.combined$vax_status == 1, 
                                            "Vaccinated",
                                            "Unvaccinated")

behavior.combined$coloring <- paste0(behavior.combined$age_group, ".",
                                     behavior.combined$vax_status_word)

# line_positions <- c(12,16.5,31)
line_positions <- c(12,16.5,31, 58, 64, 68.5, 77, 83)


plot1 <- behavior.combined %>% 
  ggplot( aes(x=week, y=num_tests, color = coloring, group = grouping, linetype = time) ) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 1) +
  scale_color_manual(values = c("5to11.0" = color_alt1, "5to11.1" = color_main,
                                "12to18.0" = color_alt5, "12to18.1" = color_alt4)) +
  scale_linetype_manual(values = c("Time-varying" = "solid", "Average over Time" = "longdash")) +
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,0.02)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Total Number of Reported Tests", x = "Week") +
  geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1)

plot1



plot2 <- behavior.combined %>% 
  ggplot( aes(x=week, y=time_since_last, color = coloring, group = grouping, linetype = time) ) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 1) +
  scale_color_manual(values = c("5to11.0" = color_alt1, "5to11.1" = color_main,
                                "12to18.0" = color_alt5, "12to18.1" = color_alt4)) +
  scale_linetype_manual(values = c("Time-varying" = "solid", "Average over Time" = "longdash")) +
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,0.02)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Average Number of Days Since Most Recent Test", x = "Week") +
  geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1)

plot2


plot3 <- behavior.combined %>% 
  ggplot( aes(x=week, y=tests_in_28, color = coloring, group = grouping, linetype = time) ) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 1) +
  scale_color_manual(values = c("5to11.Unvaccinated" = color_alt1, "5to11.Vaccinated" = color_main,
                                "12to18.Unvaccinated" = color_alt5, "12to18.Vaccinated" = color_alt4)) +
  scale_linetype_manual(values = c("Time-varying" = "solid", "Average over Time" = "longdash")) +
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,0.02)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "top")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Number of Tests in Last 28 Days", x = "Week") +
  geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) +
  guides(linetype = "none", color = guide_legend(nrow = 2))

plot3




plot4 <- behavior.combined %>% 
  ggplot( aes(x=week, y=tests_in_14, color = coloring, group = grouping, linetype = time) ) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 1) +
  scale_color_manual(values = c("5to11.0" = color_alt1, "5to11.1" = color_main,
                                "12to18.0" = color_alt5, "12to18.1" = color_alt4)) +
  scale_linetype_manual(values = c("Time-varying" = "solid", "Average over Time" = "longdash")) +
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,0.02)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Average Number of Tests in Last 14 Days", x = "Week") +
  geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) 

plot4


plot5 <- behavior.combined %>% 
  ggplot( aes(x=week, y=test_density, color = coloring, group = grouping, linetype = time) ) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 1) +
  scale_color_manual(values = c("5to11.0" = color_alt1, "5to11.1" = color_main,
                                "12to18.0" = color_alt5, "12to18.1" = color_alt4)) +
  scale_linetype_manual(values = c("Time-varying" = "solid", "Average over Time" = "longdash")) +
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,0.02)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Average Number of Tests per Days since Study Start", x = "Week") +
  geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1)

plot5

plot6 <- behavior.combined %>% 
  ggplot( aes(x=week, y=adj_test_density, color = coloring, group = grouping, linetype = time) ) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 1) +
  scale_color_manual(values = c("5to11.0" = color_alt1, "5to11.1" = color_main,
                                "12to18.0" = color_alt5, "12to18.1" = color_alt4)) +
  scale_linetype_manual(values = c("Time-varying" = "solid", "Average over Time" = "longdash")) +
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,0.02)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Average Number of Tests per Days since Enrollment", x = "Week") +
  geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1)

plot6





ggsave(here("results","together_figure3A.png"), plot = plot1, width = 6, height = 4, dpi = 300)
ggsave(here("results","together_figure3B.png"), plot = plot2, width = 6, height = 4, dpi = 300)
ggsave(here("results","together_figure3C.png"), plot = plot3, width = 6, height = 4, dpi = 300)
ggsave(here("results","together_figure3D.png"), plot = plot4, width = 6, height = 4, dpi = 300)
ggsave(here("results","together_figure3E.png"), plot = plot5, width = 6, height = 4, dpi = 300)
ggsave(here("results","together_figure3F.png"), plot = plot6, width = 6, height = 4, dpi = 300)


































# VScohort.raw.2021.5to11 <- read.csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))
# 
# behavior.byWeek.raw.2021.5to11 <- VScohort.raw.2021.5to11 %>% 
#   group_by(week, vax_status) %>% 
#   summarize(num_tested = sum(tested),
#             no_history = sum(ifelse(no_history ==1 & tested == 1, 1, 0)),
#             num_tests = mean(num_prev_test),
#             time_since_last = mean(time_since_last),
#             tests_in_28 = mean(tests_in_28),
#             tests_in_14 = mean(tests_in_14),
#             test_density = mean(test_density),
#             adj_test_density = mean(adj_test_density)) %>%
#   mutate(time = "Time-varying")
# 
# weeks.2021 <- unique(behavior.byWeek.2021.5to11$week)
# 
# #Average Testing behavior is equal for all observations for the same students so just grabbing the last test reported for each student
# behavior.avg.2021.5to11 <- VScohort.tested.2021.5to11 %>% 
#   group_by(ID) %>%
#   slice_tail(n = 1) %>%
#   group_by(vax_status) %>%
#   summarize(num_tested = n(),
#             no_history = sum(no_history),
#             num_tests = mean(total_tests),
#             time_since_last = mean(avg_time_since_last),
#             tests_in_28 = mean(avg_tests_in_28),
#             tests_in_14 = mean(avg_tests_in_14),
#             test_density = mean(avg_test_density),
#             adj_test_density = mean(avg_adj_test_density)) %>%
#   mutate(count_col = 34) %>% uncount(count_col) %>%
#   mutate(week = rep(weeks.2021, 2),
#          time = "Average over Time")
# 
# 
# behavior.combined.raw.2021.5to11 <- rbind(behavior.byWeek.raw.2021.5to11, behavior.avg.2021.5to11)
# behavior.combined.raw.2021.5to11$year <- "2021"
# behavior.combined.raw.2021.5to11$age_group <- "5to11"
# 
# behavior.combined.2021.5to11$grouping <- paste0(behavior.combined.2021.5to11$year, ".",
#                                      behavior.combined.2021.5to11$age_group, ".", 
#                                      behavior.combined.2021.5to11$time, ".", 
#                                      behavior.combined.2021.5to11$vax_status)
# behavior.combined.2021.5to11$coloring <- paste0(behavior.combined.2021.5to11$age_group, ".",
#                                      behavior.combined.2021.5to11$vax_status)
# 
# 
# behavior.combined.raw.2021.5to11$grouping <- paste0(behavior.combined.raw.2021.5to11$year, ".",
#                                                 behavior.combined.raw.2021.5to11$age_group, ".", 
#                                                 behavior.combined.raw.2021.5to11$time, ".", 
#                                                 behavior.combined.raw.2021.5to11$vax_status)
# behavior.combined.raw.2021.5to11$coloring <- paste0(behavior.combined.raw.2021.5to11$age_group, ".",
#                                                 behavior.combined.raw.2021.5to11$vax_status)
# 
# line_positions <- c(12,16.5,31)
# 
# plot1 <- behavior.combined.2021.5to11 %>% 
#   ggplot( aes(x=week, y=adj_test_density, color = coloring, group = grouping, linetype = time) ) +
#   geom_line(linewidth = 1, alpha = 0.3) + 
#   geom_point(size = 1) +
#   scale_color_manual(values = c("5to11.0" = color_alt1, "5to11.1" = color_main,
#                                 "12to18.0" = color_alt5, "12to18.1" = color_alt4)) +
#   scale_linetype_manual(values = c("Time-varying" = "solid", "Average over Time" = "longdash")) +
#   geom_smooth(method = "loess", se = FALSE) +
#   # ylim(c(0,0.02)) +
#   # xlim(c(0,34)) +
#   theme_classic() + theme(legend.position = "none")  + 
#   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   # scale_y_continuous(breaks = c(0, 1)) + 
#   labs(y = "Total Number of Reported Tests", x = "Week") +
#   geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) # +
#   # geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1)
# 
# plot1
# 
# plot1a <- behavior.combined.raw.2021.5to11 %>% 
#   ggplot( aes(x=week, y=adj_test_density, color = coloring, group = grouping, linetype = time) ) +
#   geom_line(linewidth = 1, alpha = 0.3) + 
#   geom_point(size = 1) +
#   scale_color_manual(values = c("5to11.0" = color_alt1, "5to11.1" = color_main,
#                                 "12to18.0" = color_alt5, "12to18.1" = color_alt4)) +
#   scale_linetype_manual(values = c("Time-varying" = "solid", "Average over Time" = "longdash")) +
#   geom_smooth(method = "loess", se = FALSE) +
#   # ylim(c(0,0.02)) +
#   # xlim(c(0,34)) +
#   theme_classic() + theme(legend.position = "none")  + 
#   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   # scale_y_continuous(breaks = c(0, 1)) + 
#   labs(y = "Total Number of Reported Tests", x = "Week") +
#   geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) # +
# # geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1)
# 
# plot1a





