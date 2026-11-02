

testing_2021_5_11 <- read.csv(here("cleandata", "Old Versions - Pre October 2026", "2021_age_5_11_predicted_propensity_scores.csv")) %>%
  mutate(cohort = "2021-22, Ages 5-11")
testing_2022_5_11 <- read.csv(here("cleandata", "Old Versions - Pre October 2026", "2022_age_5_11_predicted_propensity_scores.csv")) %>%
  mutate(cohort = "2022-23, Ages 5-11")
testing_2021_12_18 <- read.csv(here("cleandata", "Old Versions - Pre October 2026", "2021_age_12_18_predicted_propensity_scores.csv")) %>%
  mutate(cohort = "2021-22, Ages 12-18")
testing_2022_12_18 <- read.csv(here("cleandata", "Old Versions - Pre October 2026", "2022_age_12_18_predicted_propensity_scores.csv")) %>%
  mutate(cohort = "2022-23, Ages 12-18")

testing_propensity <- rbind(testing_2021_5_11,
                            testing_2022_5_11,
                            testing_2021_12_18,
                            testing_2022_12_18)

testing_propensity <- testing_propensity %>%
  mutate(cohort = factor(cohort, levels = c("2021-22, Ages 5-11",
                                            "2022-23, Ages 5-11",
                                            "2021-22, Ages 12-18",
                                            "2022-23, Ages 12-18")))


plot_propensity <- ggplot(testing_propensity, aes(x = propensity, fill = as.factor(vax_status))) +
  geom_histogram(position = "stack", binwidth = 0.10, color = "white", ) +
  theme_bw(base_size = 20) + 
  scale_fill_manual(
    name = "Vaccination Status",
    values = c("0" = "skyblue3", "1" = "deepskyblue4"),
    labels = c("0" = "Unvaccinated", "1" = "Vaccinated")
  ) + 
  facet_wrap(~ cohort, scales = "free_y")

plot_propensity

ggsave(here("results","together_plot_propensity.png"), plot_propensity, width = 20, height = 10.5, dpi = 300)


