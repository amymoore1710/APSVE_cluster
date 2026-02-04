
# APS VE Figure 2 - Testing over Calendar Time
# 2025-12-11
# Run Locally
# Amy Moore

library(here)
library(dplyr)
library(ggplot2)
library(forcats)

#Plotting Color Defaults
color_main <- "skyblue3"
color_accent <- "#e9ecef"
color_alt1 <- "tomato"
color_alt2 <- "seagreen3"
color_alt3 <- "mediumpurple1"


make_figure <- function(VScohort.raw, VScohort.tested) {
  VScohort.vax.positives <- unique(VScohort.tested %>% filter(result == 1, vax_status == 1) %>% select(ID))
  VScohort.vaxed <- unique(VScohort.tested %>% filter(vax_status == 1) %>% select(ID))
  
  
  VScohort.vaxed.byWeek <- VScohort.tested %>% filter(vax_status == 1) %>% group_by(week, age_group, year) %>% summarize(n = n())
  VScohort.unvaxed.byWeek <- VScohort.tested %>% filter(vax_status == 0) %>% group_by(week, age_group, year) %>% summarize(n = n())
  
  VScohort.vaxed.allbyWeek <- VScohort.raw %>% filter(vax_status == 1) %>% group_by(week,age_group, year) %>% summarize(n = n())
  VScohort.unvaxed.allbyWeek <- VScohort.raw %>% filter(vax_status == 0) %>% group_by(week, age_group, year) %>% summarize(n = n())
  
  vax.proportions <- data.frame(week = VScohort.vaxed.byWeek$week, 
                                year = VScohort.vaxed.byWeek$year,
                                age_group = VScohort.vaxed.byWeek$age_group,
                                tested = VScohort.vaxed.byWeek$n,
                                all = VScohort.vaxed.allbyWeek$n,
                                prop = VScohort.vaxed.byWeek$n / VScohort.vaxed.allbyWeek$n, 
                                vax = "vaccinated")
  unvax.proportions <- data.frame(week = VScohort.unvaxed.byWeek$week,
                                  year = VScohort.unvaxed.byWeek$year,
                                  age_group = VScohort.unvaxed.byWeek$age_group,
                                  tested = VScohort.unvaxed.byWeek$n,
                                  all = VScohort.unvaxed.allbyWeek$n,
                                  prop = VScohort.unvaxed.byWeek$n / VScohort.unvaxed.allbyWeek$n, 
                                  vax = "unvaccinated")
  
  
  testing.averages <- rbind(vax.proportions, unvax.proportions)
  
  testing.averages$groups <- paste0(testing.averages$vax, ".", testing.averages$year, ".", testing.averages$age_group)
  
  
  
  if("2021" %in% unique(testing.averages$year)) {
    if ( "2022"  %in% unique(testing.averages$year)) {
      line_positions <- c(12,16.5,31, 58, 64, 68.5, 77, 83)
      x_limits <- c(0, 95)
    } else {
      line_positions <- c(12,16.5,31)
      x_limits <- c(0, 40)
    }
  } else {
    line_positions <- c(58, 64, 68.5, 77, 83)
    x_limits <- c(45, 95)
  }
  
  
  
  plot1 <- testing.averages %>%
    ggplot( aes(x=week, y=prop, color = vax, group = groups, linetype = age_group)) +
    geom_line(linewidth = 1, alpha = 0.3) + 
    geom_point(size = 1) +
    scale_color_manual(values = c("vaccinated" = color_main,
                                  "unvaccinated" = color_alt1)) + 
    scale_linetype_manual(values = c("5to11" = "solid", "12to18" = "longdash")) +
    geom_smooth(method = "loess", se = FALSE) +
    ylim(c(0,1)) + xlim(x_limits) +
    theme_classic() + theme(legend.position = "none")  + 
    theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
    # scale_y_continuous(breaks = c(0, 1)) + 
    labs(y = "Proportion of Enrolled Tested", x = "Week") +
    geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) 
  
  
  
  plot1
  
  VScohort.vaxed.byWeek <- VScohort.tested %>% filter(vax_status == 1) %>% group_by(week,year) %>% summarize(n = n())
  VScohort.unvaxed.byWeek <- VScohort.tested %>% filter(vax_status == 0) %>% group_by(week,year) %>% summarize(n = n())
  VScohort.vaxed.byWeek$vax <- "vaccinated"
  VScohort.unvaxed.byWeek$vax <- "unvaccinated"
  VScohort.total.byWeek <- data.frame(week = VScohort.vaxed.byWeek$week,
                                      year = VScohort.vaxed.byWeek$year,
                                      n = VScohort.vaxed.byWeek$n + VScohort.unvaxed.byWeek$n,
                                      vax = "combined")
  testing.numbers <- rbind(VScohort.vaxed.byWeek, VScohort.unvaxed.byWeek, VScohort.total.byWeek)
  
  testing.numbers$groups <- paste0(testing.numbers$vax, ".", testing.numbers$year)
  
  plot2 <- testing.numbers %>%
    ggplot( aes(x=week, y=n, color = vax, group = groups)) +
    geom_line(linewidth = 1, alpha = 0.3) + 
    geom_point(size = 1) +
    scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1, "combined" = "snow4")) + 
    geom_smooth(method = "loess", se = FALSE) +
    # ylim(c(0,1)) +
    theme_classic() + theme(legend.position = "none")  + 
    theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
    # scale_y_continuous(breaks = c(0, 1)) + 
    labs(y = "Number of Tests Reported", x = "Week") +
    geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) 
  
  
  
  plot2
  
  VS.positives <- VScohort.raw %>% filter(result == 1)
  
  VS.positives.byWeek <- VS.positives %>% group_by(week, year, vax_status) %>% summarize(n = n())
  VS.positives.byWeek$result <- "positive"
  
  VS.tests.byWeek <- VScohort.tested %>% group_by(week,year, vax_status) %>% summarize(n = n())
  
  VS.positivity.byWeek <- merge(x = VS.positives.byWeek,
                                y = VS.tests.byWeek,
                                by = c("week", "year","vax_status"),
                                all.y = TRUE)
  VS.positivity.byWeek <- VS.positivity.byWeek %>% 
    rename(num_positive = n.x,
           num_tests = n.y) %>%
    select(-result) %>%
    mutate(num_positive = ifelse(is.na(num_positive), 0, num_positive),
           proportion = num_positive/num_tests,
           groups = paste0(vax_status, ".", year))
  
  plot6 <- VS.positivity.byWeek %>%
    ggplot( aes(x=week, y=proportion, color = as.factor(vax_status), group = groups)) +
    geom_line(linewidth = 1, alpha = 0.3) + 
    geom_point(size = 1) +
    scale_color_manual(values = c("0" = color_alt1, "1" = color_main)) + 
    geom_smooth(method = "loess", se = FALSE) +
    # ylim(c(0,0.02)) +
    # xlim(c(0,34)) +
    theme_classic() + theme(legend.position = "none")  + 
    theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
    # scale_y_continuous(breaks = c(0, 1)) + 
    labs(y = "Proportion of Positive Tests", x = "Week") +
    geom_vline(xintercept = line_positions, color = color_alt2, linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = 43, color = color_alt3, linetype = "dashed", linewidth = 1) 
  
  plot6
  
  plot_list <- list(plot1, plot2, plot6)
  
  return(plot_list)
}

VScohort.raw.2021.5to11 <- read.csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))
VScohort.tested.2021.5to11 <- read.csv(here("cleandata", "2021_age_5_11_all_tested.csv"))

VScohort.raw.2021.5to11$age_group <- "5to11"
VScohort.raw.2021.5to11$year <- "2021"
VScohort.tested.2021.5to11$age_group <- "5to11"
VScohort.tested.2021.5to11$year <- "2021"



plot.2021.5to11 <- make_figure(VScohort.raw.2021.5to11, VScohort.tested.2021.5to11)
plot.2021.5to11[[1]]
plot.2021.5to11[[2]]
plot.2021.5to11[[3]]


VScohort.raw.2021.12to18 <- read.csv(here("cleandata", "2021_age_12_18_predicted_propensity_scores.csv"))
VScohort.tested.2021.12to18 <- read.csv(here("cleandata", "2021_age_12_18_all_tested.csv"))

VScohort.raw.2021.12to18$age_group <- "12to18"
VScohort.raw.2021.12to18$year <- "2021"
VScohort.tested.2021.12to18$age_group <- "12to18"
VScohort.tested.2021.12to18$year <- "2021"

plot.2021.12to18 <- make_figure(VScohort.raw.2021.12to18, VScohort.tested.2021.12to18)
plot.2021.12to18[[1]]
plot.2021.12to18[[2]]
plot.2021.12to18[[3]]



VScohort.raw.2021 <- rbind(VScohort.raw.2021.5to11,
                           VScohort.raw.2021.12to18)
VScohort.tested.2021 <- rbind(VScohort.tested.2021.5to11,
                              VScohort.tested.2021.12to18)
plot.2021 <- make_figure(VScohort.raw.2021, VScohort.tested.2021)
plot.2021[[1]]
plot.2021[[2]]
plot.2021[[3]]


VScohort.raw.2022.5to11 <- read.csv(here("cleandata", "2022_age_5_11_predicted_propensity_scores.csv"))
#Since testing starts earlier in the year in 2022, the second school year starts 48 weeks after the first
VScohort.raw.2022.5to11$week <- VScohort.raw.2022.5to11$week + 48

VScohort.tested.2022.5to11 <- read.csv(here("cleandata", "2022_age_5_11_all_tested.csv"))
#Since testing starts earlier in the year in 2022, the second school year starts 48 weeks after the first
VScohort.tested.2022.5to11$week <- VScohort.tested.2022.5to11$week + 48

VScohort.raw.2022.5to11$age_group <- "5to11"
VScohort.raw.2022.5to11$year <- "2022"
VScohort.tested.2022.5to11$age_group <- "5to11"
VScohort.tested.2022.5to11$year <- "2022"

plot.2022.5to11 <- make_figure(VScohort.raw.2022.5to11, VScohort.tested.2022.5to11)
plot.2022.5to11[[1]]
plot.2022.5to11[[2]]
plot.2022.5to11[[3]]



VScohort.raw.2022.12to18 <- read.csv(here("cleandata", "2022_age_12_18_predicted_propensity_scores.csv"))
#Since testing starts earlier in the year in 2022, the second school year starts 48 weeks after the first
VScohort.raw.2022.12to18$week <- VScohort.raw.2022.12to18$week + 48
#2022-23 Ages 12-18 has to extra weeks included that aren't included in 5 to 11
VScohort.raw.2022.12to18 <- VScohort.raw.2022.12to18 %>% filter(!week %in% c(58, 77))

VScohort.tested.2022.12to18 <- read.csv(here("cleandata", "2022_age_12_18_all_tested.csv"))
#Since testing starts earlier in the year in 2022, the second school year starts 48 weeks after the first
VScohort.tested.2022.12to18$week <- VScohort.tested.2022.12to18$week + 48
#2022-23 Ages 12-18 has to extra weeks included that aren't included in 5 to 11 (only 1 student tested these weeks)
VScohort.tested.2022.12to18 <- VScohort.tested.2022.12to18 %>% filter(!week %in% c(58, 77))

VScohort.raw.2022.12to18$age_group <- "12to18"
VScohort.raw.2022.12to18$year <- "2022"
VScohort.tested.2022.12to18$age_group <- "12to18"
VScohort.tested.2022.12to18$year <- "2022"

plot.2022.12to18 <- make_figure(VScohort.raw.2022.12to18, VScohort.tested.2022.12to18)
plot.2022.12to18[[1]]
plot.2022.12to18[[2]]
plot.2022.12to18[[3]]


VScohort.raw.2022 <- rbind(VScohort.raw.2022.5to11,
                           VScohort.raw.2022.12to18)
VScohort.tested.2022 <- rbind(VScohort.tested.2022.5to11,
                              VScohort.tested.2022.12to18)
plot.2022 <- make_figure(VScohort.raw.2022, VScohort.tested.2022)
plot.2022[[1]]
plot.2022[[2]]
plot.2022[[3]]


VScohort.raw <- rbind(VScohort.raw.2021.5to11,
                               VScohort.raw.2021.12to18,
                               VScohort.raw.2022.5to11,
                               VScohort.raw.2022.12to18)

VScohort.tested <- rbind(VScohort.tested.2021.5to11,
                               VScohort.tested.2021.12to18,
                               VScohort.tested.2022.5to11,
                               VScohort.tested.2022.12to18)

plot.combined <- make_figure(VScohort.raw, VScohort.tested)
plot.combined[[1]]
plot.combined[[2]]
plot.combined[[3]]


ggsave(here("results","together_figure2A.png"), plot = plot.combined[[1]], width = 6, height = 4, dpi = 300)
ggsave(here("results","together_figure2B.png"), plot = plot.combined[[2]], width = 6, height = 4, dpi = 300)
ggsave(here("results","together_figure2C.png"), plot = plot.combined[[3]], width = 6, height = 4, dpi = 300)





























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
