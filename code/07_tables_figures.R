
  #APSVE
  #Display Results in Tables and Figures

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

color_5_11 <- "skyblue3"
color_12_18 <- "seagreen3"

  #Toggle to save results
# saving <- TRUE
saving <- FALSE

  #Load in Data/Results

  #Clean Data
All_cohort_daily        <- readRDS(file = here("cleandata", "All_cohort_daily_testing_by_ID.rds"))
All_cohort_weekly       <- readRDS(file = here("cleandata", "All_cohort_weekly_testing_by_ID.rds"))
weekly_2021_5_11        <- readRDS(file = here("cleandata", "2021_5_11_weekly_testing_by_ID.rds"))
propensity_2021_5_11    <- readRDS(file = here("cleandata", "2021_5_11_weekly_propensity.rds"))
propensity_ST_2021_5_11 <- readRDS(file = here("cleandata", "2021_5_11_ST_weekly_propensity.rds"))
matched_2021_5_11       <- readRDS(file = here("cleandata", "2021_5_11_full_matched.rds"))
matched_ST_2021_5_11    <- readRDS(file = here("cleandata", "2021_5_11_super_tester_matched.rds"))
by_ID_2021_5_11         <- readRDS(file = here("cleandata", "2021_5_11_full_by_ID.rds"))
# by_ID_ST_2021_5_11      <- readRDS(file = here("cleandata", "2021_5_11_super_tester_by_ID.rds"))

#   #Models
# propensity_model_2021_5_11 <- readRDS(here("models", "2021_5_11_propensity_model.rds"))
# summary(propensity_model_2021_5_11)
# 
# standard_ve_2021_5_11 <- readRDS(here("models", "2021_5_11_full_Standard_VE_model.rds"))
# summary(standard_ve_2021_5_11)
# 
# ST_standard_ve_2021_5_11 <- readRDS(here("models", "2021_5_11_super_tester_Standard_VE_model.rds"))
# summary(full_standard_ve_2021_5_11)
# 
# weighted_ve_2021_5_11 <- readRDS(here("models", "2021_5_11_full_Weighted_VE_model.rds"))
# summary(weighted_ve_2021_5_11)
# 
# 
#   #Model Summaries
# propensity_model_summary_2021_5_11 <- readRDS(here("models", "2021_5_11_propensity_model_summary.rds"))
# propensity_model_summary_2021_5_11
# 
# 
# standard_ve_summary_2021_5_11 <- readRDS(here("results", "2021_5_11_full_Standard_VE_model_summary.rds"))
# standard_ve_summary_2021_5_11
# 
# standard_ST_ve_summary_2021_5_11 <- readRDS(here("results", "2021_5_11_super_tester_Standard_VE_model_summary.rds"))
# standard_ST_ve_summary_2021_5_11
# 
# weighted_ve_summary_2021_5_11 <- readRDS(here("results", "2021_5_11_full_Weighted_VE_model_summary.rds"))
# weighted_ve_summary_2021_5_11



  ##### Table 1: Demographics #####

by_ID_2021_5_11  <- readRDS(file = here("cleandata", "2021_5_11_full_by_ID.rds"))
by_ID_2021_12_18 <- readRDS(file = here("cleandata", "2021_12_18_full_by_ID.rds"))

age_group_order <- c("5-11", "12-18")
age_group_labels <- c("Age 5-11", "Age 12-18")

race_order <- c("Black", "White", "Asian", "Hispanic", "Other")
race_labels <- c("Black", "White", "Asian", "Hispanic", "Other")

gender_order <- c("F", "M")
gender_labels <- c("Female", "Male")

prior_infection_order <- c(1, 0)
prior_infection_labels <- c("At least one", "None")

vax_status_order <- c(1, 0)
vax_status_labels <- c("Vaccinated", "Unvaccinated")

ever_positive_order <- c(1, 0)
ever_positive_labels <- c("At least one", "None")

by_ID <- rbind(by_ID_2021_5_11, by_ID_2021_12_18) %>%
  mutate(age_group = factor(age_group, levels = age_group_order, labels = age_group_labels),
         race = factor(race, levels = race_order, labels = race_labels),
         gender = factor(gender, levels = gender_order, labels = gender_labels),
         prior_infection = factor(prior_infection, levels = prior_infection_order, labels = prior_infection_labels),
         vax_status = factor(vax_status, levels = vax_status_order, labels = vax_status_labels),
         ever_positive = factor(ever_positive, levels = ever_positive_order, labels = ever_positive_labels))


# ---- N students --------------------------------------------------------------
n_row <- by_ID %>%
  group_by(age_group, .drop = FALSE) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  mutate(characteristic = "Cohort size", level = "N students", value = scales::comma(n)) %>%
  select(characteristic, level, age_group, value)


# ---- Age: median (IQR) -------------------------------------------------------
age_row <- by_ID %>%
  group_by(age_group, .drop = FALSE) %>%
  summarise(value = sprintf("%.1f (%.1f)",
                            mean(age, na.rm = TRUE),
                            sd(age, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(characteristic = "Age", level = "Mean (sd), years") %>%
  select(characteristic, level, age_group, value)


# ---- Categorical variables: n (column %) -------------------------------------
make_cat_rows <- function(data, var, label) {
  x  <- data[[var]]
  lv <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  lv <- unique(c(lv, "Missing"))
  
  data %>%
    mutate(level = factor(coalesce(as.character(.data[[var]]), "Missing"), levels = lv)) %>%
    group_by(age_group, level, .drop = FALSE) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(age_group) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    group_by(level) %>%
    filter(!(level == "Missing" & sum(n) == 0)) %>%    # drop the Missing row if nobody is missing
    ungroup() %>%
    mutate(characteristic = label,
           level = as.character(level),
           value = sprintf("%s (%.1f%%)", scales::comma(n), pct)) %>%
    select(characteristic, level, age_group, value)
}

cat_vars <- c(race            = "Race",
              gender          = "Gender",
              prior_infection = "Prior infection",
              vax_status      = "Vaccination status",    
              ever_positive   = "Positive test during follow-up")

cat_rows <- imap_dfr(cat_vars, ~ make_cat_rows(by_ID, var = .y, label = .x))


# ---- Combine and pivot to one column per subcohort ---------------------------
table1_data <- bind_rows(n_row, age_row, cat_rows) %>%
  pivot_wider(names_from = age_group, values_from = value, names_sort = TRUE)

# ---- Save to .csv file -------------------------------------------------------
if (saving) {
  write.csv(table1_data,
            file = here("results", "tab and figs", "table1.csv"))
}








  ##### Figure 1: Testing Completeness by Characteristic #####

# ---- 1. Set the level order for each covariate (edit labels to match your data) ----
level_order <- list(
  race            = race_labels,
  gender          = gender_labels,
  prior_infection = prior_infection_labels,
  vax_status      = vax_status_labels
)

# ---- 2. Reshape to long: one row per person x covariate -----------------------------
fig_data <- by_ID %>%
  mutate(across(c(race, gender, prior_infection, vax_status), as.character)) %>%  # avoids pivot type conflicts
  pivot_longer(c(race, gender, prior_infection, vax_status),
               names_to = "covariate", values_to = "level") %>%
  mutate(
    covariate = factor(covariate,
                       levels = c("race", "gender", "prior_infection", "vax_status"),
                       labels = c("Race", "Gender", "Prior infection", "Vaccination status")),
    level = factor(level, levels = unique(unlist(level_order)))
  ) %>%
  filter(!is.na(level))

# ---- 3. Mean completeness with 95% CI by covariate level and age group --------------
summ <- fig_data %>%
  group_by(covariate, level, age_group) %>%
  summarise(n    = n(),
            mean = mean(testing_completeness, na.rm = TRUE),
            se   = sd(testing_completeness, na.rm = TRUE) / sqrt(n),
            .groups = "drop") %>%
  mutate(lo = pmax(mean - 1.96 * se, 0),
         hi = pmin(mean + 1.96 * se, 1))

mean_testing_rate <- by_ID %>%
  group_by(age_group) %>%
  summarize(mean_rate = mean(testing_completeness))

# ---- 4. Plot -------------------------------------------------------------------------
ggplot(summ, aes(x = level, y = mean, color = age_group, shape = age_group)) +
  geom_hline(yintercept = mean_testing_rate$mean_rate[1], linetype = "dotdash", linewidth = 1, color = color_5_11) +
  geom_hline(yintercept = mean_testing_rate$mean_rate[2], linetype = "longdash", linewidth = 1, color = color_12_18) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.5)) +
  facet_grid(. ~ covariate, scales = "free_x", space = "free_x") +
  scale_color_manual(name = "Age group", values = c(color_5_11, color_12_18)) +   # in factor-level order
  scale_shape_manual(name = "Age group", values = c(16, 17)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Testing completeness\n(% of available weeks tested)") +
  theme_bw(base_size = 16) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")

if (saving) {
  ggsave(here("results", "tab and figs", "figure1.png"), width = 10, height = 4.5, dpi = 300)
}


