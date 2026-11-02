
# APS VE Table 2 - Missing Tests
# 2026-01-15
# Run Locally
# Amy Moore

library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(gt)

# =====================================================================
# 1. Read and combine the 4 subcohort files
#    Assumes each file has one row per person-week with columns:
#      id, week, tested (1/0)
# =====================================================================

file_info <- tibble(
  file  = c(here("cleandata", "2021_age_5_11_all_tested.csv"), 
            here("cleandata", "2021_age_12_18_all_tested.csv"),
            here("cleandata", "2022_age_5_11_all_tested.csv"),
            here("cleandata", "2022_age_12_18_all_tested.csv")),
  year  = c("2021-22", "2021-22", "2022-23", "2022-23"),
  age   = c("5-11", "12-18", "5-11", "12-18"),
  n_possible = c(34, 32, 37, 37)   # total tests offered that year
)

df <- file_info %>%
  mutate(data = map(file, read.csv)) %>%
  unnest(data) %>%
  rename(id = ID) %>%
  filter(!(id == 90011552 & week == 29)) %>%
  select(id, year, age, n_possible, week, tested) 

# =====================================================================
# 2. Collapse to one row per person: tests completed, % missing,
#    supertester flag
#    (Assumes the >=3 tests inclusion criterion has already been
#    applied upstream, so every id here already qualifies.)
# =====================================================================

person <- df %>%
  group_by(id, year, age, n_possible) %>%
  summarise(tests_completed = sum(tested), .groups = "drop") %>%
  mutate(
    tests_missing = n_possible - tests_completed,
    pct_missing   = tests_missing / n_possible,
    supertester   = pct_missing < 0.15
  )

# =====================================================================
# 3. Build the summary table: N, N supertesters (%), % missing
#    median (IQR) -- by subcohort + overall (no range row)
# =====================================================================

subcohort_order <- c("2021-22, 5-11", "2022-23, 5-11", "2021-22, 12-18", "2022-23, 12-18")

summarize_group <- function(data) {
  tibble(
    n_individuals    = nrow(data),
    n_supertesters   = sum(data$supertester),
    pct_supertesters = n_supertesters / n_individuals * 100,
    median_pct       = median(data$pct_missing) * 100,
    q1_pct           = quantile(data$pct_missing, .25) * 100,
    q3_pct           = quantile(data$pct_missing, .75) * 100
  )
}

by_subcohort <- person %>%
  group_by(year, age) %>%
  group_modify(~ summarize_group(.x)) %>%
  ungroup() %>%
  mutate(subcohort = paste(year, age, sep = ", ")) %>%
  select(subcohort, everything(), -year, -age)

# overall <- summarize_group(person) %>%
#   mutate(subcohort = "Overall") %>%
#   select(subcohort, everything())

table2_data <- by_subcohort %>%
  mutate(
    subcohort          = factor(subcohort, levels = subcohort_order),
    n_supertesters_fmt = sprintf("%s (%.1f%%)", n_supertesters, pct_supertesters),
    median_iqr         = sprintf("%.1f%% (%.1f\u2013%.1f%%)", median_pct, q1_pct, q3_pct)
  ) %>%
  arrange(subcohort) %>%
  select(subcohort, n_individuals, n_supertesters_fmt, median_iqr)

# =====================================================================
# 4. Transpose: subcohorts as columns, measures as rows
# =====================================================================

table2_transposed <- table2_data %>%
  mutate(across(-subcohort, as.character)) %>%
  pivot_longer(-subcohort, names_to = "measure", values_to = "value") %>%
  pivot_wider(names_from = subcohort, values_from = value) %>%
  mutate(measure = recode(measure,
                          n_individuals      = "N individuals",
                          n_supertesters_fmt = "N supertesters (%)",
                          median_iqr         = "% tests missing, median (IQR)"
  )) %>%
  select(measure, all_of(subcohort_order))   # enforces column order explicitly



