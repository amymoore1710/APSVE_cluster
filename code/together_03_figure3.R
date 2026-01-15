
# APS VE Figure 3 - Propensity Score analysis
# 2025-12-11
# Run Locally
# Amy Moore


library(here)
library(dplyr)
library(tidyr)
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


VScohort.tested$propensity_breaks <- cut(VScohort.tested$propensity,
                      breaks = c(0, 0.25, 0.5, 0.75, 1),
                      labels = c("Q1", "Q2", "Q3", "Q4"),
                      include.lowest = TRUE)

VS.positives <- VScohort.tested %>% filter(result == 1)

VS.positives.byWeek <- VS.positives %>% group_by(week, propensity_breaks) %>% summarize(n = n(), .groups = 'drop')
VS.positives.byWeek <- VS.positives.byWeek %>% complete(week, propensity_breaks, fill = list(n = 0))
VS.positives.byWeek$result <- "positive"

VS.tests.byWeek <- VScohort.tested %>% group_by(week, propensity_breaks) %>% summarize(n = n(), .groups = 'drop')
VS.tests.byWeek <- VS.tests.byWeek %>% complete(week, propensity_breaks, fill = list(n = 0))

VS.positives.byWeek$proportion <- VS.positives.byWeek$n/VS.tests.byWeek$n


plot1 <- VS.positives.byWeek %>% filter(propensity_breaks %in% c("Q1", "Q4")) %>%
  ggplot( aes(x=week, y=proportion, color = propensity_breaks)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("Q1" = color_alt1, "Q2" = color_alt2,
                                "Q3" = color_alt3, "Q4" = color_main)) + 
  geom_smooth(method = "loess", se = FALSE) +
  ylim(c(0,0.075)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Proportion of Positive Tests", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)

plot1

plot2 <- VS.positives.byWeek %>% filter(propensity_breaks %in% c("Q2", "Q4")) %>%
  ggplot( aes(x=week, y=proportion, color = propensity_breaks)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("Q1" = color_alt1, "Q2" = color_alt2,
                                "Q3" = color_alt3, "Q4" = color_main)) + 
  geom_smooth(method = "loess", se = FALSE) +
  ylim(c(0,0.075)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Proportion of Positive Tests", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)

plot2


plot3 <- VS.positives.byWeek %>% filter(propensity_breaks %in% c("Q3", "Q4")) %>%
  ggplot( aes(x=week, y=proportion, color = propensity_breaks)) +
  geom_line(linewidth = 1, alpha = 0.3) + 
  geom_point(size = 3) +
  scale_color_manual(values = c("Q1" = color_alt1, "Q2" = color_alt2,
                                "Q3" = color_alt3, "Q4" = color_main)) + 
  geom_smooth(method = "loess", se = FALSE) +
  ylim(c(0,0.075)) +
  # xlim(c(0,34)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=15), axis.text = element_text(size = 15)) + 
  # scale_y_continuous(breaks = c(0, 1)) + 
  labs(y = "Proportion of Positive Tests", x = "Week") +
  geom_vline(xintercept = 12, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 17, color = color_alt2, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 31, color = color_alt2, linetype = "dashed", linewidth = 1)

plot3

ggsave(here("results","2021_age_5_11_figure3A.png"), plot = plot1, width = 6, height = 4, dpi = 300)
ggsave(here("results","2021_age_5_11_figure3B.png"), plot = plot2, width = 6, height = 4, dpi = 300)
ggsave(here("results","2021_age_5_11_figure3C.png"), plot = plot3, width = 6, height = 4, dpi = 300)
