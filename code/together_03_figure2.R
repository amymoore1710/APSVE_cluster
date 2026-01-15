
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

VScohort.raw <- read.csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))

VScohort.tested <- read.csv(here("cleandata", "2021_age_5_11_all_tested.csv"))

VScohort.vax.positives <- unique(VScohort.tested %>% filter(result == 1, vax_status == 1) %>% select(ID))
VScohort.vaxed <- unique(VScohort.tested %>% filter(vax_status == 1) %>% select(ID))


VScohort.vaxed.byWeek <- VScohort.tested %>% filter(vax_status == 1) %>% group_by(week) %>% summarize(n = n())
VScohort.unvaxed.byWeek <- VScohort.tested %>% filter(vax_status == 0) %>% group_by(week) %>% summarize(n = n())

VScohort.vaxed.allbyWeek <- VScohort.raw %>% filter(vax_status == 1) %>% group_by(week) %>% summarize(n = n())
VScohort.unvaxed.allbyWeek <- VScohort.raw %>% filter(vax_status == 0) %>% group_by(week) %>% summarize(n = n())

vax.proportions <- data.frame(week = VScohort.vaxed.byWeek$week, prop = VScohort.vaxed.byWeek$n / VScohort.vaxed.allbyWeek$n, vax = "vaccinated")
unvax.proportions <- data.frame(week = VScohort.unvaxed.byWeek$week, prop = VScohort.unvaxed.byWeek$n / VScohort.unvaxed.allbyWeek$n, vax = "unvaccinated")


testing.averages <- rbind(vax.proportions, unvax.proportions)

VScohort.vaxed.byWeek$vax <- "vaccinated"
VScohort.unvaxed.byWeek$vax <- "unvaccinated"
testing.numbers <- rbind(VScohort.vaxed.byWeek, VScohort.unvaxed.byWeek)



plot1 <- testing.averages %>%
  ggplot( aes(x=week, y=prop, color = vax, group = vax)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1)) + 
  geom_smooth(method = "loess", se = FALSE) +
  ylim(c(0,1)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Proportion of Enrolled Tested", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)



plot1

plot2 <- testing.numbers %>%
  ggplot( aes(x=week, y=n, color = vax, group = vax)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1)) + 
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,1)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Number of Tests Reported", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)



plot2

VScohort.raw$vax_week_adj <- ifelse(VScohort.raw$vax_week < 0, 0,
                        VScohort.raw$vax_week)

vax.by.ID <- VScohort.raw %>% group_by(ID) %>% summarize(vax_week_adj = first(vax_week_adj))
vax.by.ID$vax_week_adj <- ifelse(is.na(vax.by.ID$vax_week_adj), 100, vax.by.ID$vax_week_adj)

vax.over.time <- vax.by.ID %>% group_by(vax_week_adj) %>% summarize(n = n())
vax.over.time$vax <- "vaccinated"

plot3 <- vax.over.time %>%
  ggplot( aes(x=vax_week_adj, y=n, color = vax, group = vax)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1)) + 
  geom_smooth(method = "loess", se = FALSE) +
  ylim(c(0,1600)) +
  xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Number of Students Newly Vaccinated", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)

plot3

vax.cumulative <- vax.over.time %>% mutate(cumulative_n = cumsum(n))

plot4 <- vax.cumulative %>%
  ggplot( aes(x=vax_week_adj, y=cumulative_n, color = vax, group = vax)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("vaccinated" = color_main, "unvaccinated" = color_alt1)) + 
  geom_smooth(method = "loess", se = FALSE) +
  ylim(c(0,5100)) +
  xlim(c(0,38)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Total Number of Students Vaccinated", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)

plot4


VS.positives <- VScohort.raw %>% filter(result == 1)

VS.positives.byWeek <- VS.positives %>% group_by(week) %>% summarize(n = n())
VS.positives.byWeek$result <- "positive"

VS.tests.byWeek <- VScohort.tested %>% group_by(week) %>% summarize(n = n())

VS.positives.byWeek$proportion <- VS.positives.byWeek$n/VS.tests.byWeek$n

plot5 <- VS.positives.byWeek %>%
  ggplot( aes(x=week, y=n, color = result)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("positive" = color_alt1, "negative" = color_main)) + 
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,5100)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Number of Positive Tests", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)

plot5

plot6 <- VS.positives.byWeek %>%
  ggplot( aes(x=week, y=proportion, color = result)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("positive" = color_alt1, "negative" = color_main)) + 
  geom_smooth(method = "loess", se = FALSE) +
  # ylim(c(0,5100)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Proportion of Positive Tests", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)

plot6



ggsave(here("results","2021_age_5_11_figure2A.png"), plot = plot1, width = 6, height = 4, dpi = 300)
ggsave(here("results","2021_age_5_11_figure2B.png"), plot = plot2, width = 6, height = 4, dpi = 300)
ggsave(here("results","2021_age_5_11_figure2C.png"), plot = plot3, width = 6, height = 4, dpi = 300)
ggsave(here("results","2021_age_5_11_figure2D.png"), plot = plot4, width = 6, height = 4, dpi = 300)
ggsave(here("results","2021_age_5_11_figure2E.png"), plot = plot5, width = 6, height = 4, dpi = 300)
ggsave(here("results","2021_age_5_11_figure2F.png"), plot = plot6, width = 6, height = 4, dpi = 300)
