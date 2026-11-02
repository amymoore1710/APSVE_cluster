library(DiagrammeR)
library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(here)
library(DiagrammeRsvg)
library(rsvg)

# ---- 1. Define your numbers here (placeholders shown) -----------------

cohort_names <- paste0(rep(c("2021-22", "2022-23"), each = 4), ", Age ", 
                       rep(c("5-11", "12-18"), each = 2, times = 2), ", ", 
                       rep(c("Full", "Super Tester"), times = 4))

file_name_prelims <- paste0(rep(c("2021", "2022"), each = 4), "_age_", 
                            rep(c("5_11", "12_18"), each = 2, times = 2), "_", 
                            rep(c("", "ST_"), times = 4))

sample_size_table <- list()

for (i in 1:length(cohort_names)) {
  cohort_name <- cohort_names[i]
  file_name_prelim <- file_name_prelims[i]
  
  #Summary file is shared between full and super tester cohort
  summary_prelim <- file_name_prelim
  if(str_sub(file_name_prelim, -3, -1) == "ST_") {
    summary_prelim <- str_sub(file_name_prelim, 1, -4)
  } 
  
  summary <- read.csv(file = here("results", paste0(summary_prelim, "dataset_workflow_sample_sizes.csv")))
  cohort <- read.csv(file = here("cleandata", paste0(file_name_prelim, "all_tested.csv")))
  matched <- read.csv(file = here("cleandata", paste0(file_name_prelim, "matched_data_set.csv")))
  
  sample_sizes <- summary %>%
    filter(X == "nIDs") %>%
    mutate(cohort_name = cohort_name,
           cohort = length(unique(cohort$ID)),
           matched = length(unique(matched$ID))) 
  
  sample_size_table[[i]] <- sample_sizes
  
}

df_sample_size_table <- map(sample_size_table, ~ rename(.x, overall = 2, school_year = 3, age_group = 5, eligible = 11))   # renames whichever column is in position 1

combined <- bind_rows(df_sample_size_table)

combined <- combined %>%
  select(cohort_name, overall, school_year, age_group, eligible, cohort, matched)

overall_n <- combined$overall[1]

year_pools <- tibble::tibble(
  year   = c("2021-22", "2022-23"),
  id     = c("y1", "y2"),
  n_pool = c(combined$school_year[1], combined$school_year[5])
)

age_pools <- tibble::tibble(
  year_id = c("y1", "y1", "y2", "y2"),
  age_id  = c("young", "old", "young", "old"),
  id      = c("y1y", "y1o", "y2y", "y2o"),
  label   = c("2021-22, Age 5-11", "2021-22, Age 12-18", "2022-23, Age 5-11", "2022-23, Age 12-18"),
  n_age   = c(combined$age_group[1], combined$age_group[3],
              combined$age_group[5], combined$age_group[7])
)

subcohorts <- tibble::tibble(
  id     = c("y1y", "y1o", "y2y", "y2o"),
  n_elig = c(combined$eligible[1], combined$eligible[3],
             combined$eligible[5], combined$eligible[7]),
  n_super         = c(combined$cohort[2], combined$cohort[4],
                      combined$cohort[6], combined$cohort[8]),
  n_full_matched  = c(combined$matched[1], combined$matched[3],
                      combined$matched[5], combined$matched[7]),
  n_super_matched = c(combined$matched[2], combined$matched[4],
                      combined$matched[6], combined$matched[8])
) %>%
  left_join(age_pools, by = "id")

fmt <- function(n) format(n, big.mark = ",")

# ---- 2. Top-level, year-pool, and age-pool nodes -----------------------

top_nodes <- glue("
  overall [label = 'Overall unique individuals\\nN = {fmt(overall_n)}', fillcolor = '#eaf2f8']
")

year_blocks <- year_pools %>%
  rowwise() %>%
  mutate(block = glue("
    {id}_pool [label = '{year} enrolled\\nN = {fmt(n_pool)}', fillcolor = '#eaf2f8']
    overall -> {id}_pool
  ")) %>%
  pull(block) %>%
  paste(collapse = "\n")

age_blocks <- age_pools %>%
  rowwise() %>%
  mutate(block = glue("
    {id}_age [label = '{label}\\nN = {fmt(n_age)}', fillcolor = '#c9e5b3']
    {year_id}_pool -> {id}_age
  ")) %>%
  pull(block) %>%
  paste(collapse = "\n")

# ---- 3. Eligibility -> full/supertester -> matched (no exclusion boxes) ----

subcohort_blocks <- subcohorts %>%
  rowwise() %>%
  mutate(block = glue("
    {id}_elig  [label = 'Eligible (\u2265 3 tests)\\nN = {fmt(n_elig)}', fillcolor = '#d9ead3']
    {id}_age -> {id}_elig

    {id}_full  [label = 'Full cohort\\nN = {fmt(n_elig)}', fillcolor = '#cfe2f3']
    {id}_super [label = 'Supertesters\\nN = {fmt(n_super)}', fillcolor = '#fce5cd']
    {id}_elig -> {id}_full
    {id}_elig -> {id}_super

    {id}_full_m  [label = 'Matched pairs\\nN = {fmt(n_full_matched)}', fillcolor = '#a4c2f4']
    {id}_super_m [label = 'Matched pairs\\nN = {fmt(n_super_matched)}', fillcolor = '#f9cb9c']
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

  {top_nodes}
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

grViz(dot) %>%
  export_svg() %>%
  charToRaw() %>%
  rsvg_png(here("results","sample_size_flow_diagram.png"), width = 1600)
