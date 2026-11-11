
  #APSVE
  #Display Results in Tables and Figures


library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(scales)
library(lubridate)
library(gt)
library(stringr)
library(purrr)
library(glue)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

#Color Palette
color_2021_5_11_full <- '#6DC7FF'
color_2021_5_11_full_matched <- '#BCE2FF'
color_2021_5_11_ST <- '#028DC3'
color_2021_5_11_ST_matched <- '#04ABEC'

color_2021_12_18_full <- '#7FCF9B'
color_2021_12_18_full_matched <- '#AFEAC2'
color_2021_12_18_ST <- '#209659'
color_2021_12_18_ST_matched <- '#50B377'

color_2022_5_11_full <- '#CBB2FB'
color_2022_5_11_full_matched <- '#E5D6FF'
color_2022_5_11_ST <- '#9074CE'
color_2022_5_11_ST_matched <- '#AE92E7'

color_2022_12_18_full <- '#FFA3C7'
color_2022_12_18_full_matched <- '#FFD0E1'
color_2022_12_18_ST <- '#D05B8F'
color_2022_12_18_ST_matched <- '#EB7DAB'

color_2021 <- '#05C4CB'
color_2022 <- '#E497DC'

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
                       labels = c("Race", "Gender", "Prior Infection", "Vaccination Status")),
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
  geom_hline(yintercept = mean_testing_rate$mean_rate[1], linetype = "dotdash", linewidth = 1, color = color_2021_5_11_full) +
  geom_hline(yintercept = mean_testing_rate$mean_rate[2], linetype = "longdash", linewidth = 1, color = color_2021_12_18_full) +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.5)) +
  facet_grid(. ~ covariate, scales = "free_x", space = "free_x", labeller = label_wrap_gen(width = 14)) +
  scale_color_manual(name = "Age group", values = c(color_2021_5_11_full, color_2021_12_18_full)) +   # in factor-level order
  scale_shape_manual(name = "Age group", values = c(16, 17)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Testing completeness\n(% of weeks tested)") +
  theme_bw(base_size = 18) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")

if (saving) {
  ggsave(here("results", "tab and figs", "figure1.png"), width = 10, height = 4.5, dpi = 300)
}


  ##### Figure 2: Testing Completeness over Time #####

all_weekly_2021_5_11  <- readRDS(file = here("cleandata", "2021_5_11_all_weekly_testing_by_ID.rds"))
all_weekly_2021_12_18  <- readRDS(file = here("cleandata", "2021_12_18_all_weekly_testing_by_ID.rds"))

all_weekly <- rbind(all_weekly_2021_5_11, all_weekly_2021_12_18)

start_date_2021 <- ymd("2021-09-07") #First day of VS testing
end_date_2022 <- ymd("2022-05-26") #Last day of School
first_sunday_2021 <- as.numeric(ymd("2021-09-05"))

# ---- 0. Settings (edit) ---------------------------------------------------------------
study_start    <- start_date_2021         # week-1 start date; use the same one you used to build `weekly`
blackout_weeks <- c(12, 16, 17, 31)       # weeks with <100 tests statewide (2021-22)
min_people     <- 30                      # hide a point if fewer students than this are in the group that week
min_tests_pos  <- 20                      # hide a positivity point if fewer tests than this

age_colors <- c("5-11" = color_2021_5_11_full, "12-18" = color_2021_12_18_full)   # match your age_group labels

all_weekly <- all_weekly %>%
  mutate(
    date      = study_start + (week - 1) * 7,    # skip if you already have a calendar date column
    age_group = factor(age_group, levels = names(age_colors)),
    vax_label = factor(vax_status, levels = c(0, 1), labels = c("Unvaccinated", "Vaccinated"))
  )

# ---- 1. Weekly summaries ----------------------------------------------------------------
by_group <- all_weekly %>%                                   # panels 1 and 2: age group x vax status
  group_by(age_group, vax_label, week, date) %>%
  summarise(n_people  = n(),
            n_tested  = sum(tested),
            n_pos     = sum(positive),
            .groups   = "drop") %>%
  mutate(
    completeness = n_tested / n_people,
    positivity   = if_else(n_tested > 0, n_pos / n_tested, NA_real_),
    unavailable  = week %in% blackout_weeks,
    completeness = if_else(unavailable | n_people < min_people,    NA_real_, completeness),
    positivity   = if_else(unavailable | n_tested < min_tests_pos, NA_real_, positivity)
  )

by_age <- all_weekly %>%                                     # panel 3: age group only
  group_by(age_group, week, date) %>%
  summarise(pct_vax = mean(vax_status == 1), .groups = "drop")

# ---- 2. Shared plot components -----------------------------------------------------------
date_range <- range(all_weekly$date) + c(0, 7)

shade_df <- all_weekly %>%
  distinct(week, date) %>%
  filter(week %in% blackout_weeks) %>%
  arrange(week) %>%
  mutate(block = cumsum(week - lag(week, default = first(week)) > 1)) %>%   # new block whenever weeks aren't consecutive
  group_by(block) %>%
  summarise(xmin = max(min(date) - 7, date_range[1]),
            xmax = min(max(date) + 7, date_range[2]),
            .groups = "drop")

shade   <- geom_rect(data = shade_df, inherit.aes = FALSE,
                     aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                     fill = "grey70", alpha = 0.3)
x_scale <- scale_x_date(limits = date_range, date_breaks = "3 months", date_labels = "%b\n%Y")
base    <- theme_bw(base_size = 16) + theme(panel.grid.minor = element_blank())
no_x    <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
color_scale    <- scale_color_manual(name = "Age group", values = age_colors)
linetype_scale <- scale_linetype_manual(name = "Vaccination status",
                                        values = c(Unvaccinated = "dashed", Vaccinated = "solid"))

# ---- 3. Panels ------------------------------------------------------------------------------
p_comp <- ggplot(by_group, aes(date, completeness, color = age_group, linetype = vax_label)) +
  shade + geom_line(linewidth = 0.7) + x_scale + color_scale + linetype_scale +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  labs(x = NULL, y = "Testing completeness\n(% of students tested)") + base

p_pos <- ggplot(by_group, aes(date, positivity, color = age_group, linetype = vax_label)) +
  shade + geom_line(linewidth = 0.7) + x_scale + color_scale + linetype_scale +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = "Test positivity\n(% of tests positive)") + base

p_vax <- ggplot(by_age, aes(date, pct_vax, color = age_group)) +
  geom_line(linewidth = 0.8) + x_scale + color_scale +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  labs(x = "Calendar date", y = "% vaccinated") + base

# ---- 4. Stack and add one shared legend ---------------------------------------------------
grid <- plot_grid(
  p_comp + no_x + theme(legend.position = "none"),
  p_pos  + no_x + theme(legend.position = "none"),
  p_vax  + theme(legend.position = "none"),
  ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 1, 1.15),
  labels = c("A", "B", "C"), label_size = 16
)

legend <- get_legend(p_comp + theme(legend.box.margin = margin(0, 0, 0, 15)))

plot_grid(grid, NULL, legend, ncol = 3, rel_widths = c(1, 0.02, 0.32))

if(saving) {
  ggsave(here("results", "tab and figs", "figure2.png"), width = 9, height = 9, dpi = 300)
}

  ##### Table 2: Super Tester Subset #####

by_ID_ST_2021_5_11      <- readRDS(file = here("cleandata", "2021_5_11_super_tester_by_ID.rds"))
by_ID_ST_2021_12_18      <- readRDS(file = here("cleandata", "2021_12_18_super_tester_by_ID.rds"))

by_ID_ST <- rbind(by_ID_ST_2021_5_11, by_ID_ST_2021_12_18) %>%
  mutate(age_group = factor(age_group, levels = age_group_order, labels = age_group_labels),
         race = factor(race, levels = race_order, labels = race_labels),
         gender = factor(gender, levels = gender_order, labels = gender_labels),
         prior_infection = factor(prior_infection, levels = prior_infection_order, labels = prior_infection_labels),
         vax_status = factor(vax_status, levels = vax_status_order, labels = vax_status_labels),
         ever_positive = factor(ever_positive, levels = ever_positive_order, labels = ever_positive_labels))

summarize_cohort <- function(data, label) {
  data %>%
    group_by(age_group, .drop = FALSE) %>%
    summarise(n   = n_distinct(ID),
              med = median(testing_completeness, na.rm = TRUE) * 100,
              q1  = quantile(testing_completeness, .25, na.rm = TRUE) * 100,
              q3  = quantile(testing_completeness, .75, na.rm = TRUE) * 100,
              .groups = "drop") %>%
    mutate(n_fmt    = scales::comma(n),
           comp_fmt = sprintf("%.1f%% (%.1f-%.1f%%)", med, q1, q3)) %>%
    select(age_group, n_fmt, comp_fmt) %>%
    pivot_longer(c(n_fmt, comp_fmt), names_to = "stat", values_to = "value") %>%
    mutate(row = if_else(stat == "n_fmt",
                         paste0(label, " (n)"),
                         paste0(label, ", % completeness, median (IQR)"))) %>%
    select(row, age_group, value)
}

table2_data <- bind_rows(
  summarize_cohort(by_ID,    "Full cohort"),
  summarize_cohort(by_ID_ST, "Super tester cohort")
) %>%
  pivot_wider(names_from = age_group, values_from = value)

# ---- Save to .csv file -------------------------------------------------------
if (saving) {
  write.csv(table2_data,
            file = here("results", "tab and figs", "table2.csv"))
}














  ##### Supplementary Table 1: Demographics for Super Testers #####
by_ID <- by_ID %>% mutate(cohort = "Full")
by_ID_ST <- by_ID_ST %>% mutate(cohort = "Super Tester")

age_group_cohort_order <- c("Age 5-11 Full", "Age 5-11 Super Tester",
                            "Age 12-18 Full", "Age 12-18 Super Tester")

by_ID_combined <- rbind(by_ID, by_ID_ST) %>%
  mutate(age_group_cohort = factor(paste(age_group, cohort), levels = age_group_cohort_order))

# ---- N students --------------------------------------------------------------
n_row_combined <- by_ID_combined %>%
  group_by(age_group_cohort, .drop = FALSE) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  mutate(characteristic = "Cohort size", level = "N students", value = scales::comma(n)) %>%
  select(characteristic, level, age_group_cohort, value)


# ---- Age: median (IQR) -------------------------------------------------------
age_row_combined <- by_ID_combined %>%
  group_by(age_group_cohort, .drop = FALSE) %>%
  summarise(value = sprintf("%.1f (%.1f)",
                            mean(age, na.rm = TRUE),
                            sd(age, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(characteristic = "Age", level = "Mean (sd), years") %>%
  select(characteristic, level, age_group_cohort, value)


# ---- Categorical variables: n (column %) -------------------------------------
make_cat_rows_combined <- function(data, var, label) {
  x  <- data[[var]]
  lv <- if (is.factor(x)) levels(x) else sort(unique(as.character(x)))
  lv <- unique(c(lv, "Missing"))
  
  data %>%
    mutate(level = factor(coalesce(as.character(.data[[var]]), "Missing"), levels = lv)) %>%
    group_by(age_group_cohort, level, .drop = FALSE) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(age_group_cohort) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    group_by(level) %>%
    filter(!(level == "Missing" & sum(n) == 0)) %>%    # drop the Missing row if nobody is missing
    ungroup() %>%
    mutate(characteristic = label,
           level = as.character(level),
           value = sprintf("%s (%.1f%%)", scales::comma(n), pct)) %>%
    select(characteristic, level, age_group_cohort, value)
}

cat_vars <- c(race            = "Race",
              gender          = "Gender",
              prior_infection = "Prior infection",
              vax_status      = "Vaccination status",    
              ever_positive   = "Positive test during follow-up")

cat_rows_combined <- imap_dfr(cat_vars, ~ make_cat_rows_combined(by_ID_combined, var = .y, label = .x))


# ---- Combine and pivot to one column per subcohort ---------------------------
supplement_table1_data <- bind_rows(n_row_combined, age_row_combined, cat_rows_combined) %>%
  pivot_wider(names_from = age_group_cohort, values_from = value, names_sort = TRUE)

# ---- Save to .csv file -------------------------------------------------------
if (saving) {
  write.csv(supplement_table1_data,
            file = here("results", "tab and figs", "supplement_table1.csv"))
}





  #####Supplementary Figure 1 #####


# ---- 1. Define your numbers here (placeholders shown) -----------------

combined <- readRDS(here("results", "sample_sizes_complete.RDS"))

precleaning_n <- combined$precleaning[1]

eligible_n <- combined$eligible[1]

year_pools <- tibble::tibble(
  year   = c("2021-22", "2022-23"),
  id     = c("y1", "y2"),
  n_pool = c(combined$school_year[1], combined$school_year[5]),
  color = c(color_2021, color_2022)
)

age_pools <- tibble::tibble(
  year_id = c("y1", "y1", "y2", "y2"),
  age_id  = c("young", "old", "young", "old"),
  id      = c("y1y", "y1o", "y2y", "y2o"),
  label   = c("2021-22, Age 5-11", "2021-22, Age 12-18", "2022-23, Age 5-11", "2022-23, Age 12-18"),
  n_age   = c(combined$age_group[1], combined$age_group[3],
              combined$age_group[5], combined$age_group[7]),
  color   = c(color_2021_5_11_full, color_2021_12_18_full,
              color_2022_5_11_full, color_2022_12_18_full)
)

subcohorts <- tibble::tibble(
  id     = c("y1y", "y1o", "y2y", "y2o"),
  n_super         = c(combined$super_testers[2], combined$super_testers[4],
                      combined$super_testers[6], combined$super_testers[8]),
  n_full_matched  = c(combined$matched[1], combined$matched[3],
                      combined$matched[5], combined$matched[7]),
  n_super_matched = c(combined$matched[2], combined$matched[4],
                      combined$matched[6], combined$matched[8]),
  full_color = c(color_2021_5_11_full, color_2021_12_18_full,
                 color_2022_5_11_full, color_2022_12_18_full),
  st_color = c(color_2021_5_11_ST, color_2021_12_18_ST,
               color_2022_5_11_ST, color_2022_12_18_ST),
  full_matched_color = c(color_2021_5_11_full_matched, color_2021_12_18_full_matched,
                         color_2022_5_11_full_matched, color_2022_12_18_full_matched),
  st_matched_color = c(color_2021_5_11_ST_matched, color_2021_12_18_ST_matched,
                       color_2022_5_11_ST_matched, color_2022_12_18_ST_matched)
) %>%
  left_join(age_pools, by = "id")

fmt <- function(n) format(n, big.mark = ",")

# ---- 2. Top-level, year-pool, and age-pool nodes -----------------------

precleaning_node <- glue("
  precleaning [label = 'Unique Individuals in Orignal Dataset \\nN = {fmt(precleaning_n)}', fillcolor = '#eaf2f8']
")

eligible_node <- glue("
  eligible [label = 'Eligible (\u2265 3 tests)\\nN = {fmt(eligible_n)}', fillcolor = '#eaf2f8']
  precleaning -> eligible
")

year_blocks <- year_pools %>%
  rowwise() %>%
  mutate(block = glue("
    {id}_pool [label = '{year} enrolled\\nN = {fmt(n_pool)}', fillcolor = '{color}']
    eligible -> {id}_pool
  ")) %>%
  pull(block) %>%
  paste(collapse = "\n")

age_blocks <- age_pools %>%
  rowwise() %>%
  mutate(block = glue("
    {id}_age [label = '{label}\\nN = {fmt(n_age)}', fillcolor = '{color}']
    {year_id}_pool -> {id}_age
  ")) %>%
  pull(block) %>%
  paste(collapse = "\n")

# ---- 3. Eligibility -> full/supertester -> matched (no exclusion boxes) ----

subcohort_blocks <- subcohorts %>%
  rowwise() %>%
  mutate(block = glue("
    {id}_full  [label = 'Full cohort\\nN = {fmt(n_age)}', fillcolor = '{full_color}']
    {id}_super [label = 'Supertesters\\nN = {fmt(n_super)}', fillcolor = '{st_color}']
    {id}_age -> {id}_full
    {id}_age -> {id}_super

    {id}_full_m  [label = 'Matched pairs\\nN = {fmt(n_full_matched)}', fillcolor = '{full_matched_color}']
    {id}_super_m [label = 'Matched pairs\\nN = {fmt(n_super_matched)}', fillcolor = '{st_matched_color}']
    {id}_full -> {id}_full_m
    {id}_super -> {id}_super_m
  ")) %>%
  pull(block) %>%
  paste(collapse = "\n")

# ---- 4. Assemble and render (narrower layout) --------------------------

dot <- glue("
digraph sample_flow {{
  graph [layout = dot, rankdir = TB, nodesep = 0.15, ranksep = 0.4,
         fontsize = 9, size = '6,10!', ratio = 'compress']
  node [shape = box, style = filled, fontname = Helvetica, fontsize = 8, width = 1.1, height = 0.55]

  {precleaning_node}
  {eligible_node}
  {year_blocks}
  {age_blocks}
  {subcohort_blocks}

  labelloc = 'b'
  label = 'Note: individuals enrolled in both years branch into both year pools above,\\nso totals sum to more than the overall N.'
  fontsize = 8
  fontcolor = 'grey30'
}}
")

grViz(dot)

if (saving) {
  grViz(dot) %>%
    export_svg() %>%
    charToRaw() %>%
    rsvg_png(here("results","tab and figs","supplement_figure1.png"), width = 1600)
}


  ##### Figure 3: Testing Behavior by Covariates #####

propensity_2021_5_11    <- readRDS(file = here("cleandata", "2021_5_11_weekly_propensity.rds"))
propensity_2021_12_18    <- readRDS(file = here("cleandata", "2021_12_18_weekly_propensity.rds"))

propensity_2021 <- rbind(propensity_2021_5_11, propensity_2021_12_18)


# ---- 0. Settings (edit) --------------------------------------------------------------
blackout_weeks <- c(12, 16, 17, 31)                       # same unavailable weeks as Figure 2
age_colors <- c("5-11" = color_2021_5_11_full, "12-18" = color_2021_12_18_full)

# ---- 1. Analytic data: available weeks only -------------------------------------------
dat <- propensity_2021 %>%
  filter(!week %in% blackout_weeks) %>%
  mutate(
    age_group = factor(age_group, levels = names(age_colors)),
    vax_label = factor(vax_status, levels = c(0, 1), labels = c("Unvaccinated", "Vaccinated"))
  )

with_hist <- dat %>% filter(first_test_indicator == 0)    # used for panels 1-3

# Tertile cutpoints from the pooled with-history data (same cutpoints for every stratum)

  #Switched to meaningful cut points --> 0.5 = 1 test per 2 weeks available, 1 = 1 test per week available
    #Low = <1 test every two weeks
    #Medium = 1 test every 1 to 2 weeks
    #High = >1 test every week

# q <- quantile(with_hist$test_density, probs = c(1/3, 2/3), na.rm = TRUE)
q_defined <- c(0.5, 1) 
# tertile_labels <- c(sprintf("Low\n(\u2264 %.2f)", q[1]),
                    # sprintf("Medium\n(%.2f\u2013%.2f)", q[1], q[2]),
                    # sprintf("High\n(> %.2f)", q[2]))

tertile_labels_simple <- c("Low", "Medium", "High")

# ---- 2. Define the category for each panel ---------------------------------------------
weeks_since <- function(x) if_else(x >= 5, "5+ weeks", 
                                   ifelse(x == 1, 
                                          paste(as.character(x), "week"),
                                          paste(as.character(x), "weeks")))
tests_since <- function(x) if_else(x >= 5, "5+ tests", 
                                  ifelse(x == 1, 
                                         paste(as.character(x), "test"),
                                         paste(as.character(x), "tests")))

panel_data <- list(
  "Weeks Since Last Test" = with_hist %>% mutate(level = weeks_since(weeks_since_last_test)),
  "Tests in Last 4 Weeks" = with_hist %>% mutate(level = tests_since(tests_last_4weeks)),
  "Test Density"          = with_hist %>% mutate(level = as.character(
    cut(test_density, breaks = c(-Inf, q_defined, Inf), labels = tertile_labels_simple))),
  "Testing History"       = dat %>% mutate(level = if_else(first_test_indicator == 1,
                                                           "No history", "Some history"))
)

# ---- 3. Completeness with cluster-robust (by ID) 95% CIs -------------------------------
summarize_panel <- function(data, panel_name) {
  data %>%
    group_by(level, age_group, vax_label, ID) %>%
    summarise(tests = sum(tested), weeks = n(), .groups = "drop") %>%   # one row per student per category
    group_by(level, age_group, vax_label) %>%
    summarise(n_ids = n(),
              p  = sum(tests) / sum(weeks),
              se = if_else(n_ids > 1,
                           sqrt(sum((tests - p * weeks)^2) * n_ids / (n_ids - 1)) / sum(weeks),
                           NA_real_),
              .groups = "drop") %>%
    mutate(panel = panel_name)
}

level_order <- c(weeks_since(c(5,4,3,2,1)),tests_since(c(0,1,2,3,4,5)), tertile_labels_simple, "No history", "Some history")

summ <- imap_dfr(panel_data, ~ summarize_panel(.x, .y)) %>%
  mutate(panel = factor(panel, levels = names(panel_data)),
         level = factor(level, levels = level_order),
         lo = pmax(p - 1.96 * se, 0),
         hi = pmin(p + 1.96 * se, 1))

summ <- summ %>%
  mutate(shape_group = factor(paste(age_group, vax_label), levels = c("5-11 Vaccinated",
                                                                      "5-11 Unvaccinated",
                                                                      "12-18 Vaccinated",
                                                                      "12-18 Unvaccinated")))

# ---- 4. Plot -----------------------------------------------------------------------------
ggplot(summ, aes(x = level, y = p, color = shape_group, shape = shape_group)) +
  geom_pointrange(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.6), size = 0.4, linewidth = 0.5) +
  facet_grid(. ~ panel, scales = "free_x", space = "free_x", labeller = label_wrap_gen(width = 14)) +
  scale_color_manual(name = "Cohort", values = c("5-11 Unvaccinated" = color_2021_5_11_full, 
                                                 "5-11 Vaccinated" = color_2021_5_11_full,
                                                 "12-18 Unvaccinated" = color_2021_12_18_full,
                                                 "12-18 Vaccinated" = color_2021_12_18_full)) +
  scale_shape_manual(name = "Cohort", values = c("5-11 Unvaccinated" = 1, 
                                                 "5-11 Vaccinated" = 16,
                                                 "12-18 Unvaccinated" = 2,
                                                 "12-18 Vaccinated" = 17)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = NULL, y = "Testing completeness\n(% of weeks tested)") +
  theme_bw(base_size = 18) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")  +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  )


if (saving) {
  ggsave(here("results", "tab and figs", "figure3.png"), width = 10, height = 4.5, dpi = 300)
}


  ##### Table 3: Propensity Score Model OR Estimates #####

propensity_model_2021_5_11 <- readRDS(here("models", "2021_5_11_propensity_model.RDS"))
# summary(propensity_model_2021_5_11)

propensity_model_basic_2021_5_11 <- readRDS(here("models", "2021_5_11_propensity_model_basic.RDS"))
# summary(propensity_model_basic_2021_5_11)

propensity_model_2021_12_18 <- readRDS(here("models", "2021_12_18_propensity_model.RDS"))
# summary(propensity_model_2021_5_11)

propensity_model_basic_2021_12_18 <- readRDS(here("models", "2021_12_18_propensity_model_basic.RDS"))
# summary(propensity_model_basic_2021_12_18)


# ---- Pull Summaries from Models  ---------------------------------------------
make_CIs <- function(model, alpha = 0.05) {
  
  coefs <- summary(model)$coefficients[,1]
  SEs <- summary(model)$coefficients[,2]
  
  CI_upper <- coefs + qnorm(1 - alpha/2) * SEs
  CI_lower <- coefs + qnorm(alpha/2) * SEs
  
  CI_statement <- data.frame(Covariate = names(coefs),
                             CIs = paste0(round(coefs, digits = 2), " (",
                                          round(CI_lower, digits = 2), ", ",
                                          round(CI_upper, digits = 2), ")"))
  
  return(CI_statement)
  
}

column_2021_5_11_basic_model <- make_CIs(propensity_model_basic_2021_5_11) %>% rename(basic_5_11 = CIs)
column_2021_5_11_full_model <- make_CIs(propensity_model_2021_5_11) %>% rename(full_5_11 = CIs)
column_2021_12_18_basic_model <- make_CIs(propensity_model_basic_2021_12_18) %>% rename(basic_12_18 = CIs)
column_2021_12_18_full_model <- make_CIs(propensity_model_2021_12_18) %>% rename(full_12_18 = CIs)

combine_colums_list <- list(column_2021_5_11_basic_model,
                            column_2021_5_11_full_model,
                            column_2021_12_18_basic_model,
                            column_2021_12_18_full_model)

table3_data <- combine_colums_list %>% reduce(full_join, by = "Covariate")

# ---- Save to .csv file -------------------------------------------------------
if (saving) {
  write.csv(table3_data,
            file = here("results", "tab and figs", "table3.csv"))
}
