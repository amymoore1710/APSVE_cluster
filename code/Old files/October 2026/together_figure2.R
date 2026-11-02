
# APS VE Figure 2 - Testing over Calendar Time
# 2026-02-04
# Run Locally
# Amy Moore

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(slider)
library(scales)

#Plotting Color Defaults
color_main <- "skyblue3"
color_accent <- "#e9ecef"
color_alt1 <- "tomato"
color_alt2 <- "seagreen3"
color_alt3 <- "mediumpurple1"
color_alt4 <- "deepskyblue4"
color_alt5 <- "tomato4"


  #Scenario Set-up
subcohorts <- crossing(year = c("2021-22", "2022-23"), age = c("5-11", "12-18"))

# each year's school-year start date (edit to your real dates)
start_dates <- tibble(
  year       = c("2021-22", "2022-23"),
  start_date = as.Date(c("2021-09-07", "2022-08-01"))
)



# ---- break weeks differ by year: define as (start, end) study-week ranges ----
breaks_df <- tibble::tibble(
  year   = c("2021-22", "2021-22", "2021-22",
             "2022-23", "2022-23", "2022-23", "2022-23", "2022-23"),
  break_name = c("Thanksgiving", "Christmas", "Spring",
                 "Fall", "Thanksgiving", "Christmas", "Winter", "Spring"),
  start  = c(11, 15, 30, 10, 16, 20, 29, 35),
  end    = c(13, 18, 32, 12, 18, 23, 31, 37)
) %>%
  left_join(start_dates, by = "year") %>%
  mutate(start_date_break = start_date + (start - 1) * 7,
         end_date_break   = start_date + (end - 1) * 7) %>%
  select(year, start_date_break, end_date_break)


in_break <- function(year, w) {
  b <- tibble(year = c("Year 1","Year 1","Year 2","Year 2"),
              start = c(11,21,15,24), end = c(13,23,17,26)) %>%
    filter(year == !!year)
  any(w >= b$start & w <= b$end)
}


  #Step 1: Compute plotting variables by subcohort by week
    #Cohort size --> The number of students who had tested that week or a prior week without a gap of >4 weeks
    #Tests --> The number of tests reported that week
    #Testing rate --> The number of tests taken divided by the cohort size that week
    #Positivity rate --> The number of positive tests divided by the number of tests
    #Cumulative Vaccination rate --> The number of individuals that were vaccinated at or before the week

weekly_tests_2021_ages_5_11 <- read.csv(here("cleandata", "Old Versions - Pre October 2026", "2021_age_5_11_weekly_testing_by_ID.csv"))
  
  #Add a variable to denote individuals actively in the cohort
df_2021_5_11 <- weekly_tests_2021_ages_5_11 %>%
  mutate(
    year = "2021-22",
    age = "5-11"
  ) %>%
  arrange(ID, week) %>%
  group_by(ID) %>%
  mutate(
    active_cohort = as.integer(
      slide_dbl(tested, sum, .before = 4, .complete = FALSE) > 0
    )
  ) %>%
  ungroup()



weekly_tests_2021_ages_12_18 <- read.csv(here("cleandata", "Old Versions - Pre October 2026", "2021_age_12_18_weekly_testing_by_ID.csv"))

#Add a variable to denote individuals actively in the cohort
df_2021_12_18 <- weekly_tests_2021_ages_12_18 %>%
  mutate(
    year = "2021-22",
    age = "12-18"
  ) %>%
  arrange(ID, week) %>%
  group_by(ID) %>%
  mutate(
    active_cohort = as.integer(
      slide_dbl(tested, sum, .before = 4, .complete = FALSE) > 0
    )
  ) %>%
  ungroup()


weekly_tests_2022_ages_5_11 <- read.csv(here("cleandata", "Old Versions - Pre October 2026", "2022_age_5_11_weekly_testing_by_ID.csv"))

#Add a variable to denote individuals actively in the cohort
df_2022_5_11 <- weekly_tests_2022_ages_5_11 %>%
  mutate(
    year = "2022-23",
    age = "5-11",
    week = week+1 
  ) %>%
  arrange(ID, week) %>%
  group_by(ID) %>%
  mutate(
    active_cohort = as.integer(
      slide_dbl(tested, sum, .before = 4, .complete = FALSE) > 0
    )
  ) %>%
  ungroup()



weekly_tests_2022_ages_12_18 <- read.csv(here("cleandata", "Old Versions - Pre October 2026", "2022_age_12_18_weekly_testing_by_ID.csv"))

#Add a variable to denote individuals actively in the cohort
df_2022_12_18 <- weekly_tests_2022_ages_12_18 %>%
  mutate(
    year = "2022-23",
    age = "12-18",
    week = week+1
  ) %>%
  arrange(ID, week) %>%
  group_by(ID) %>%
  mutate(
    active_cohort = as.integer(
      slide_dbl(tested, sum, .before = 4, .complete = FALSE) > 0
    )
  ) %>%
  ungroup() %>%
  filter(!week %in% c(11,30))

df_combined <- rbind(df_2021_5_11,
                     df_2021_12_18,
                     df_2022_5_11,
                     df_2022_12_18)

df <- df_combined %>%
  left_join(start_dates, by = "year") %>%
  mutate(date = start_date + (week - 1) * 7) %>%
  select(-start_date)

n0 <- tibble::tibble(
  year = c("2021-22", "2021-22", "2022-23", "2022-23"),
  age  = c("5-11", "12-18", "5-11", "12-18"),
  n0   = c(length(unique(weekly_tests_2021_ages_5_11$ID)), 
           length(unique(weekly_tests_2021_ages_12_18$ID)), 
           length(unique(weekly_tests_2022_ages_5_11$ID)), 
           length(unique(weekly_tests_2022_ages_12_18$ID)))
)


# df <- df_combined %>%
#   group_by(year, age, week) %>%
#   summarize(
#     active_n = sum(active_cohort),
#     tests = sum(tested),
#     rate = tests/active_n,
#     posrate = sum(result)/tests,
#     cumvax = sum(vax_status*active_cohort)/active_n
#   ) %>%
#   select(year, age, week, rate, active_n, tests, posrate, cumvax)

# =====================================================================
# Shared plotting helper
# =====================================================================

age_colors <- c("5-11" = color_main, "12-18" = color_alt2)

make_panel <- function(data, y_var, y_lab, split_vax = FALSE) {
  p <- ggplot(data, aes(x = date, y = .data[[y_var]], color = age)) +
    geom_rect(data = breaks_df, inherit.aes = FALSE,
              aes(xmin = start_date_break, xmax = end_date_break, ymin = -Inf, ymax = Inf),
              fill = "grey70", alpha = 0.25) +
    facet_grid(. ~ year, scales = "free_x") +
    scale_color_manual(values = age_colors, name = "Age group") +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    labs(x = NULL, y = y_lab) +
    theme_bw(base_size = 11) +
    theme(strip.background = element_blank(), strip.text = element_text(face = "bold"))
  
  if (split_vax) {
    p + geom_line(aes(linetype = vax_status), linewidth = 0.7) +
      scale_linetype_manual(values = c(Unvaccinated = "dashed", Vaccinated = "solid"),
                            name = "Vaccination status")
  } else {
    p + geom_line(linewidth = 0.9)
  }
}

# =====================================================================
# FIGURE 1: primary descriptive figure (unsplit by vax status)
# =====================================================================

weekly_main <- df %>%
  group_by(year, age, week, date) %>%
  summarise(rate     = mean(tested),
            cohort = n(),
            posrate  = sum(result == 1, na.rm = TRUE) / sum(tested),
            cumvax   = sum(vax_status)/cohort,
            .groups = "drop")

p1_rate   <- make_panel(weekly_main, "rate",     "Weekly testing rate")
p1_pos    <- make_panel(weekly_main, "posrate",  "Weekly positivity")
p1_vax    <- make_panel(weekly_main, "cumvax",   "Cumulative % vaccinated") +
  labs(x = NULL) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1))

fig1_grid <- plot_grid(
  p1_rate   + theme(legend.position = "none"),
  # p1_active + theme(legend.position = "none"),
  p1_pos    + theme(legend.position = "none"),
  p1_vax    + theme(legend.position = "none"),
  ncol = 1, align = "v", axis = "lr"
)

fig1_legend <- get_legend(p1_rate)
# fig1_title <- ggdraw() +
#   draw_label("Figure 1. Weekly testing, positivity, and vaccination uptake by subcohort",
#              fontface = "bold", size = 13, x = 0, hjust = 0)

fig1 <- plot_grid(plot_grid(fig1_grid, fig1_legend, ncol = 2, rel_widths = c(1, 0.18)),
                  ncol = 1, rel_heights = c(0.05, 1))

fig1

ggsave(here("results","together_figure2_v2.png"), fig1, width = 10, height = 10.5, dpi = 300)






  ##### Figure 3 - by vax status

weekly_vax <- df %>%
  group_by(year, age, vax_status, week, date) %>%
  summarise(rate    = mean(tested),
            posrate = sum(result == 1, na.rm = TRUE) / sum(tested),
            .groups = "drop")

weekly_vax <- weekly_vax %>% 
  mutate(vax_status = recode(vax_status,
                             `0` = "Unvaccinated",
                             `1` = "Vaccinated"
  ))

p2_rate <- make_panel(weekly_vax, "rate",    "Weekly testing rate", split_vax = TRUE)
p2_pos  <- make_panel(weekly_vax, "posrate", "Weekly positivity",   split_vax = TRUE) +
  labs(x = NULL)

fig2_grid <- plot_grid(
  p2_rate + theme(legend.position = "none"),
  p2_pos  + theme(legend.position = "none"),
  ncol = 1, align = "v", axis = "lr"
)

fig2_legend <- plot_grid(get_legend(p2_rate + guides(color = "none")),
                         get_legend(p2_rate + guides(linetype = "none")),
                         ncol = 1)
# fig2_title <- ggdraw() +
#   draw_label("Figure 2. Testing rate and positivity by current vaccination\nstatus, within each subcohort",
#              fontface = "bold", size = 13, x = 0, hjust = 0)

fig2 <- plot_grid(plot_grid(fig2_grid, fig2_legend, ncol = 2, rel_widths = c(1, 0.2)),
                  ncol = 1, rel_heights = c(0.09, 1))
fig2

ggsave(here("results","together_figure3_v2.png"), fig2, width = 10, height = 6, dpi = 300)






# make_figure <- function(VScohort.raw, VScohort.tested) {
#   # VScohort.vax.positives <- unique(VScohort.tested %>% filter(result == 1, vax_status == 1) %>% select(ID))
#   # VScohort.vaxed <- unique(VScohort.tested %>% filter(vax_status == 1) %>% select(ID))
#   # 
#   # 
#   # VScohort.vaxed.byWeek <- VScohort.tested %>% filter(vax_status == 1) %>% group_by(week, age_group, year) %>% summarize(n = n())
#   # VScohort.unvaxed.byWeek <- VScohort.tested %>% filter(vax_status == 0) %>% group_by(week, age_group, year) %>% summarize(n = n())
#   # 
#   # VScohort.vaxed.allbyWeek <- VScohort.raw %>% filter(vax_status == 1) %>% group_by(week,age_group, year) %>% summarize(n = n())
#   # VScohort.unvaxed.allbyWeek <- VScohort.raw %>% filter(vax_status == 0) %>% group_by(week, age_group, year) %>% summarize(n = n())
#   # 
#   # vax.proportions <- data.frame(week = VScohort.vaxed.byWeek$week, 
#   #                               year = VScohort.vaxed.byWeek$year,
#   #                               age_group = VScohort.vaxed.byWeek$age_group,
#   #                               tested = VScohort.vaxed.byWeek$n,
#   #                               all = VScohort.vaxed.allbyWeek$n,
#   #                               prop = VScohort.vaxed.byWeek$n / VScohort.vaxed.allbyWeek$n, 
#   #                               vax = "vaccinated")
#   # unvax.proportions <- data.frame(week = VScohort.unvaxed.byWeek$week,
#   #                                 year = VScohort.unvaxed.byWeek$year,
#   #                                 age_group = VScohort.unvaxed.byWeek$age_group,
#   #                                 tested = VScohort.unvaxed.byWeek$n,
#   #                                 all = VScohort.unvaxed.allbyWeek$n,
#   #                                 prop = VScohort.unvaxed.byWeek$n / VScohort.unvaxed.allbyWeek$n, 
#   #                                 vax = "unvaccinated")
#   # 
#   # 
#   # testing.averages <- rbind(vax.proportions, unvax.proportions)
#   # 
#   # testing.averages$groups <- paste0(testing.averages$vax, ".", testing.averages$year, ".", testing.averages$age_group)
#   # testing.averages$colorings <- paste0(testing.averages$vax, ".", testing.averages$age_group)
#   # 
#   # 
#   # 
#   # if("2021" %in% unique(testing.averages$year)) {
#   #   if ( "2022"  %in% unique(testing.averages$year)) {
#   #     line_positions <- c(12,16.5,31, 58, 64, 68.5, 77, 83)
#   #     x_limits <- c(0, 95)
#   #   } else {
#   #     line_positions <- c(12,16.5,31)
#   #     x_limits <- c(0, 40)
#   #   }
#   # } else {
#   #   line_positions <- c(58, 64, 68.5, 77, 83)
#   #   x_limits <- c(45, 95)
#   # }
#   # 
#   # 
#   # 
#   # plot1 <- testing.averages %>%
#   #   ggplot( aes(x=week, y=prop, color = colorings, group = groups)) +
#   #   geom_line(linewidth = 1, alpha = 0.3) + 
#   #   geom_point(size = 1) +
#   #   scale_color_manual(values = c("vaccinated.5to11" = color_main,
#   #                                 "unvaccinated.5to11" = color_alt1,
#   #                                 "unvaccinated.12to18" = color_alt5, 
#   #                                 "vaccinated.12to18" = color_alt4)) + 
#   #   # scale_linetype_manual(values = c("5to11" = "solid", "12to18" = "longdash")) +
#   #   geom_smooth(method = "loess", se = FALSE) +
#   #   ylim(c(0,1)) + xlim(x_limits) +
#   #   theme_classic() + theme(legend.position = "top")  + 
#   #   labs(color = "Color") + 
#   #   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   #   # scale_y_continuous(breaks = c(0, 1)) + 
#   #   labs(y = "Proportion of Enrolled Tested", x = "Week") +
#   #   geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   #   geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) +
#   #   guides(color = guide_legend(nrow = 2))
#   # 
#   # 
#   # 
#   # plot1
#   # 
#   # VScohort.vaxed.byWeek <- VScohort.tested %>% filter(vax_status == 1) %>% group_by(week,year) %>% summarize(n = n())
#   # VScohort.unvaxed.byWeek <- VScohort.tested %>% filter(vax_status == 0) %>% group_by(week,year) %>% summarize(n = n())
#   # VScohort.vaxed.byWeek$vax <- "vaccinated"
#   # VScohort.unvaxed.byWeek$vax <- "unvaccinated"
#   # VScohort.total.byWeek <- data.frame(week = VScohort.vaxed.byWeek$week,
#   #                                     year = VScohort.vaxed.byWeek$year,
#   #                                     n = VScohort.vaxed.byWeek$n + VScohort.unvaxed.byWeek$n,
#   #                                     vax = "combined")
#   # testing.numbers <- rbind(VScohort.vaxed.byWeek, VScohort.unvaxed.byWeek, VScohort.total.byWeek)
#   # 
#   # testing.numbers$groups <- paste0(testing.numbers$vax, ".", testing.numbers$year)
#   # 
#   # plot2 <- testing.numbers %>%
#   #   ggplot( aes(x=week, y=n, color = vax, group = groups)) +
#   #   geom_line(linewidth = 1, alpha = 0.3) + 
#   #   geom_point(size = 1) +
#   #   scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1, "combined" = "mediumpurple4")) + 
#   #   geom_smooth(method = "loess", se = FALSE) +
#   #   # ylim(c(0,1)) +
#   #   theme_classic() + theme(legend.position = "top")  + 
#   #   labs(color = "Color") + 
#   #   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   #   # scale_y_continuous(breaks = c(0, 1)) + 
#   #   labs(y = "Number of Tests Reported", x = "Week") +
#   #   geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   #   geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) 
#   # 
#   # 
#   # 
#   # plot2
#   # 
#   # VS.positives <- VScohort.raw %>% filter(result == 1)
#   # 
#   # VS.positives.byWeek <- VS.positives %>% group_by(week, year, vax_status) %>% summarize(n = n())
#   # VS.positives.byWeek$result <- "positive"
#   # 
#   # VS.tests.byWeek <- VScohort.tested %>% group_by(week,year, vax_status) %>% summarize(n = n())
#   # 
#   # VS.positivity.byWeek <- merge(x = VS.positives.byWeek,
#   #                               y = VS.tests.byWeek,
#   #                               by = c("week", "year","vax_status"),
#   #                               all.y = TRUE)
#   # VS.positivity.byWeek <- VS.positivity.byWeek %>% 
#   #   rename(num_positive = n.x,
#   #          num_tests = n.y) %>%
#   #   select(-result) %>%
#   #   mutate(num_positive = ifelse(is.na(num_positive), 0, num_positive),
#   #          proportion = num_positive/num_tests,
#   #          groups = paste0(vax_status, ".", year))
#   # 
#   # plot6 <- VS.positivity.byWeek %>%
#   #   ggplot( aes(x=week, y=proportion, color = factor(vax_status, labels = c("Unvaccinated", "Vaccinated")), group = groups)) +
#   #   geom_line(linewidth = 1, alpha = 0.3) + 
#   #   geom_point(size = 1) +
#   #   scale_color_manual(values = c("Unvaccinated" = color_alt1, "Vaccinated" = color_main)) + 
#   #   geom_smooth(method = "loess", se = FALSE) +
#   #   # ylim(c(0,0.02)) +
#   #   # xlim(c(0,34)) +
#   #   theme_classic() + theme(legend.position = "top")  + 
#   #   labs(color = "Color") + 
#   #   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   #   # scale_y_continuous(breaks = c(0, 1)) + 
#   #   labs(y = "Proportion of Positive Tests", x = "Week") +
#   #   geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   #   geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) 
#   # 
#   # plot6
#   # 
#   # plot_list <- list(plot1, plot2, plot6)
#   # 
#   # return(plot_list)
# }
# 
# VScohort.raw.2021.5to11 <- read.csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))
# VScohort.tested.2021.5to11 <- read.csv(here("cleandata", "2021_age_5_11_all_tested.csv"))
# 
# VScohort.raw.2021.5to11$age_group <- "5to11"
# VScohort.raw.2021.5to11$year <- "2021"
# VScohort.tested.2021.5to11$age_group <- "5to11"
# VScohort.tested.2021.5to11$year <- "2021"
# 
# 
# 
# plot.2021.5to11 <- make_figure(VScohort.raw.2021.5to11, VScohort.tested.2021.5to11)
# plot.2021.5to11[[1]]
# plot.2021.5to11[[2]]
# plot.2021.5to11[[3]]
# 
# 
# VScohort.raw.2021.12to18 <- read.csv(here("cleandata", "2021_age_12_18_predicted_propensity_scores.csv"))
# VScohort.tested.2021.12to18 <- read.csv(here("cleandata", "2021_age_12_18_all_tested.csv"))
# 
# VScohort.raw.2021.12to18$age_group <- "12to18"
# VScohort.raw.2021.12to18$year <- "2021"
# VScohort.tested.2021.12to18$age_group <- "12to18"
# VScohort.tested.2021.12to18$year <- "2021"
# 
# plot.2021.12to18 <- make_figure(VScohort.raw.2021.12to18, VScohort.tested.2021.12to18)
# plot.2021.12to18[[1]]
# plot.2021.12to18[[2]]
# plot.2021.12to18[[3]]
# 
# 
# 
# VScohort.raw.2021 <- rbind(VScohort.raw.2021.5to11,
#                            VScohort.raw.2021.12to18)
# VScohort.tested.2021 <- rbind(VScohort.tested.2021.5to11,
#                               VScohort.tested.2021.12to18)
# plot.2021 <- make_figure(VScohort.raw.2021, VScohort.tested.2021)
# plot.2021[[1]]
# plot.2021[[2]]
# plot.2021[[3]]
# 
# 
# VScohort.raw.2022.5to11 <- read.csv(here("cleandata", "2022_age_5_11_predicted_propensity_scores.csv"))
# #Since testing starts earlier in the year in 2022, the second school year starts 48 weeks after the first
# VScohort.raw.2022.5to11$week <- VScohort.raw.2022.5to11$week + 48
# 
# VScohort.tested.2022.5to11 <- read.csv(here("cleandata", "2022_age_5_11_all_tested.csv"))
# #Since testing starts earlier in the year in 2022, the second school year starts 48 weeks after the first
# VScohort.tested.2022.5to11$week <- VScohort.tested.2022.5to11$week + 48
# 
# VScohort.raw.2022.5to11$age_group <- "5to11"
# VScohort.raw.2022.5to11$year <- "2022"
# VScohort.tested.2022.5to11$age_group <- "5to11"
# VScohort.tested.2022.5to11$year <- "2022"
# 
# plot.2022.5to11 <- make_figure(VScohort.raw.2022.5to11, VScohort.tested.2022.5to11)
# plot.2022.5to11[[1]]
# plot.2022.5to11[[2]]
# plot.2022.5to11[[3]]
# 
# 
# 
# VScohort.raw.2022.12to18 <- read.csv(here("cleandata", "2022_age_12_18_predicted_propensity_scores.csv"))
# #Since testing starts earlier in the year in 2022, the second school year starts 48 weeks after the first
# VScohort.raw.2022.12to18$week <- VScohort.raw.2022.12to18$week + 48
# #2022-23 Ages 12-18 has to extra weeks included that aren't included in 5 to 11
# VScohort.raw.2022.12to18 <- VScohort.raw.2022.12to18 %>% filter(!week %in% c(58, 77))
# 
# VScohort.tested.2022.12to18 <- read.csv(here("cleandata", "2022_age_12_18_all_tested.csv"))
# #Since testing starts earlier in the year in 2022, the second school year starts 48 weeks after the first
# VScohort.tested.2022.12to18$week <- VScohort.tested.2022.12to18$week + 48
# #2022-23 Ages 12-18 has to extra weeks included that aren't included in 5 to 11 (only 1 student tested these weeks)
# VScohort.tested.2022.12to18 <- VScohort.tested.2022.12to18 %>% filter(!week %in% c(58, 77))
# 
# VScohort.raw.2022.12to18$age_group <- "12to18"
# VScohort.raw.2022.12to18$year <- "2022"
# VScohort.tested.2022.12to18$age_group <- "12to18"
# VScohort.tested.2022.12to18$year <- "2022"
# 
# plot.2022.12to18 <- make_figure(VScohort.raw.2022.12to18, VScohort.tested.2022.12to18)
# plot.2022.12to18[[1]]
# plot.2022.12to18[[2]]
# plot.2022.12to18[[3]]
# 
# 
# VScohort.raw.2022 <- rbind(VScohort.raw.2022.5to11,
#                            VScohort.raw.2022.12to18)
# VScohort.tested.2022 <- rbind(VScohort.tested.2022.5to11,
#                               VScohort.tested.2022.12to18)
# plot.2022 <- make_figure(VScohort.raw.2022, VScohort.tested.2022)
# plot.2022[[1]]
# plot.2022[[2]]
# plot.2022[[3]]
# 
# 
# VScohort.raw <- rbind(VScohort.raw.2021.5to11,
#                                VScohort.raw.2021.12to18,
#                                VScohort.raw.2022.5to11,
#                                VScohort.raw.2022.12to18)
# 
# VScohort.tested <- rbind(VScohort.tested.2021.5to11,
#                                VScohort.tested.2021.12to18,
#                                VScohort.tested.2022.5to11,
#                                VScohort.tested.2022.12to18)
# 
# plot.combined <- make_figure(VScohort.raw, VScohort.tested)
# plot.combined[[1]]
# plot.combined[[2]]
# plot.combined[[3]]
# 
# 
# ggsave(here("results","together_figure2A.png"), plot = plot.combined[[1]], width = 6, height = 4, dpi = 300)
# ggsave(here("results","together_figure2B.png"), plot = plot.combined[[2]], width = 6, height = 4, dpi = 300)
# ggsave(here("results","together_figure2C.png"), plot = plot.combined[[3]], width = 6, height = 4, dpi = 300)
# 




























# plot2 <- testing.numbers %>%
#   ggplot( aes(x=week, y=n, color = vax, group = vax)) +
#   geom_line(linewidth = 1, alpha = 0.3) + 
#   geom_point(size = 3) +
#   scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1)) + 
#   geom_smooth(method = "loess", se = FALSE) +
#   # ylim(c(0,1)) +
#   theme_classic() + theme(legend.position = "none")  + 
#   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   # scale_y_continuous(breaks = c(0, 1)) + 
#   labs(y = "Number of Tests Reported", x = "Week") +
#   geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)
# 
# 
# 
# plot2
# 
# VScohort.raw$vax_week_adj <- ifelse(VScohort.raw$vax_week < 0, 0,
#                         VScohort.raw$vax_week)
# 
# vax.by.ID <- VScohort.raw %>% group_by(ID) %>% summarize(vax_week_adj = first(vax_week_adj))
# vax.by.ID$vax_week_adj <- ifelse(is.na(vax.by.ID$vax_week_adj), 200, vax.by.ID$vax_week_adj)
# 
# vax.over.time <- vax.by.ID %>% group_by(vax_week_adj) %>% summarize(n = n())
# vax.over.time$vax <- "vaccinated"
# 
# plot3 <- vax.over.time %>%
#   ggplot( aes(x=vax_week_adj, y=n, color = vax, group = vax)) +
#   geom_line(linewidth = 1, alpha = 0.3) + 
#   geom_point(size = 3) +
#   scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1)) + 
#   geom_smooth(method = "loess", se = FALSE) +
#   ylim(c(0,1600)) +
#   xlim(c(0,95)) +
#   theme_classic() + theme(legend.position = "none")  + 
#   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   # scale_y_continuous(breaks = c(0, 1)) + 
#   labs(y = "Number of Students Newly Vaccinated", x = "Week") +
#   geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)
# 
# plot3
# 
# vax.cumulative <- vax.over.time %>% mutate(cumulative_n = cumsum(n))
# 
#   #Note: Unvaccinated are marked as vax week = 200 so 1 item will not be plotted and give a warning
# plot4 <- vax.cumulative %>%
#   ggplot( aes(x=vax_week_adj, y=cumulative_n, color = vax, group = vax)) +
#   geom_line(linewidth = 1, alpha = 0.3) + 
#   geom_point(size = 3) +
#   scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1)) + 
#   geom_smooth(method = "loess", se = FALSE) +
#   ylim(c(0,10000)) +
#   xlim(c(-1,95)) +
#   theme_classic() + theme(legend.position = "none")  + 
#   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   # scale_y_continuous(breaks = c(0, 1)) + 
#   labs(y = "Total Number of Students Vaccinated", x = "Week") +
#   geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)
# 
# plot4
# 
# 
# VS.positives <- VScohort.raw %>% filter(result == 1)
# 
# VS.positives.byWeek <- VS.positives %>% group_by(week, year, vax_status) %>% summarize(n = n())
# VS.positives.byWeek$result <- "positive"
# 
# VS.tests.byWeek <- VScohort.tested %>% group_by(week,year, vax_status) %>% summarize(n = n())
# 
# VS.positivity.byWeek <- merge(x = VS.positives.byWeek,
#                               y = VS.tests.byWeek,
#                               by = c("week", "year","vax_status"),
#                               all.y = TRUE)
# VS.positivity.byWeek <- VS.positivity.byWeek %>% 
#   rename(num_positive = n.x,
#          num_tests = n.y) %>%
#   select(-result) %>%
#   mutate(num_positive = ifelse(is.na(num_positive), 0, num_positive),
#          proportion = num_positive/num_tests,
#          groups = paste0(vax_status, ".", year))

# plot5 <- VS.positivity.byWeek %>%
#   ggplot( aes(x=week, y=n, color = result)) +
#   geom_line(linewidth = 1, alpha = 0.3) + 
#   geom_point(size = 3) +
#   scale_color_manual(values = c("positive" = color_alt1, "negative" = color_main)) + 
#   geom_smooth(method = "loess", se = FALSE) +
#   # ylim(c(0,5100)) +
#   # xlim(c(0,34)) +
#   theme_classic() + theme(legend.position = "none")  + 
#   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   # scale_y_continuous(breaks = c(0, 1)) + 
#   labs(y = "Number of Positive Tests", x = "Week") +
#   geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)
# 
# plot5

# plot6 <- VS.positivity.byWeek %>%
#   ggplot( aes(x=week, y=proportion, color = as.factor(vax_status), group = groups)) +
#   geom_line(linewidth = 1, alpha = 0.3) + 
#   geom_point(size = 3) +
#   scale_color_manual(values = c("0" = color_alt1, "1" = color_main)) + 
#   geom_smooth(method = "loess", se = FALSE) +
#   # ylim(c(0,0.02)) +
#   # xlim(c(0,34)) +
#   theme_classic() + theme(legend.position = "none")  + 
#   theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
#   # scale_y_continuous(breaks = c(0, 1)) + 
#   labs(y = "Proportion of Positive Tests", x = "Week") +
#   geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) 
# 
# plot6
# 
# 
# 
# ggsave(here("results","2021_age_5_11_figure2A.png"), plot = plot1, width = 6, height = 4, dpi = 300)
# ggsave(here("results","2021_age_5_11_figure2B.png"), plot = plot2, width = 6, height = 4, dpi = 300)
# ggsave(here("results","2021_age_5_11_figure2C.png"), plot = plot3, width = 6, height = 4, dpi = 300)
# ggsave(here("results","2021_age_5_11_figure2D.png"), plot = plot4, width = 6, height = 4, dpi = 300)
# ggsave(here("results","2021_age_5_11_figure2E.png"), plot = plot5, width = 6, height = 4, dpi = 300)
# ggsave(here("results","2021_age_5_11_figure2F.png"), plot = plot6, width = 6, height = 4, dpi = 300)
