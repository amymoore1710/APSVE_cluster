
  #06_bootstrapping.R

print("")
print("")
print("------------------------------")
print("Begin File 6: Bootstrap Variance")
print("------------------------------")
print("")
print("")


#Run Locally?
cluster = TRUE
# cluster = FALSE

#Extra Print outs
verbose = TRUE
# verbose = FALSE

#Package Library Location
if (cluster) {
  .libPaths("~/Rlibs")
} else {
  library(here) # File Locations
}

library(lme4)
library(MatchIt)
library(coxme)
library(survival)
library(dplyr)
library(future.apply)   # MASS is called with :: below to avoid masking dplyr::select
# library(readr) #Read in Files
# library(tidyverse) # Data Manipulation
library(lubridate) #Dates

if(cluster){
  source("/home/amoor53/APSVE_cluster/code/00_config.R")
  source("/home/amoor53/APSVE_cluster/code/helpers.R")
  task <- get_task(bootstrap_grid)
  set.seed(cfg$seed + match(task$subcohort, subcohorts))   # reproducible per task
  dat <- readRDS(paste0("/home/amoor53/APSVE_cluster/cleandata/", task$subcohort,"_weekly_propensity.rds"))
  ps_fit <- readRDS(paste0("/home/amoor53/APSVE_cluster/models/", task$subcohort,"_propensity_model.rds"))
} else {
  source(here("code", "00_config.R"))
  source(here("code", "helpers.R"))
  task <- get_task(bootstrap_grid)
  set.seed(cfg$seed + match(task$subcohort, subcohorts))   # reproducible per task
  dat <- readRDS(here("cleandata", paste0(task$subcohort, "_weekly_propensity.rds")))
  ps_fit <- readRDS(here("models", paste0(task$subcohort, "_propensity_model.rds")))
}

print(paste("Task:", task$subcohort, task$cohort_type, task$model_method))

# ------------------------------------------------------------------
# Inputs
#   dat    : person-week data (every week, tested or not), one row per
#            ID x week, with columns: ID, school, week, prior_week, tested,
#            positive, vax_status, demographics, prior_testing_behavior
#   ps_fit : the already-fitted propensity GLMM (the 5-hour model), fit once
#            on `dat` with these formula terms:
#            tested ~ vax_status + demographics + prior_testing_behavior +
#                     (1 | school) + (1 | ID)
# ------------------------------------------------------------------



# ---- Step 1: one-time setup from the fitted propensity model ----
beta_hat <- fixef(ps_fit)            # fixed-effect estimates
V_hat    <- vcov(ps_fit)             # their covariance matrix
X        <- getME(ps_fit, "X")            # fixed-effects design matrix
stopifnot(nrow(X) == nrow(dat))           # rows must align with dat (no NAs dropped)

# Everything in the linear predictor that is NOT the fixed effects
# (i.e., the school and person random-intercept predictions). We hold
# these at their fitted values and only perturb the fixed effects.
eta_hat   <- predict(ps_fit, type = "link")
re_offset <- as.numeric(eta_hat - X %*% beta_hat)

# ---- Step 2: draw all propensity coefficient vectors up front ----
# B <- 10
B <- 500
# set.seed(2026)
beta_draws <- MASS::mvrnorm(B, mu = beta_hat, Sigma = V_hat)   # B x p matrix

# ---- Step 3: By ID dataset for resampling ----

# Student-level table used for resampling and matching
#Compute Testing Completeness for each ID
students <- dat %>% 
  group_by(ID) %>%
  summarize(n_weeks_available = n(),
            n_weeks_tested = sum(tested),
            ever_positive = ifelse(sum(positive) >= 1, 1, 0),
            prior_infection = ifelse(min(prior_infections) == 0, 0, 1),
            testing_completeness = n_weeks_tested/n_weeks_available,
            first_test_week = sum(first_test_indicator))

demographics <- dat %>%
  distinct(ID, school_year, birthdate, gender, race, 
           schoolname, dir_cert, vax_date, 
           final_group, age, age_group, subcohort
           )

students <- demographics %>%
  left_join(students, by = "ID") %>%
  mutate(vax_status = case_when(
    final_group == "Unvaccinated" ~ 0,
    final_group == "Unvaccinated (censored at vax date)" ~ 0,
    final_group == "Vaccinated" ~ 1,
    .default = NA
  )) 

# %>% select(ID, schoolname, vax_status, first_test_week, testing_completeness)

# Name of the vaccination coefficient in the coxme fit (check your coding)
vax_coef <- "vax_status"

# ---- Step 4: one bootstrap replicate ----
one_rep <- function(b) {
  tryCatch({
    # (a) Recompute propensity-based weights from the drawn coefficients
    p_star <- plogis(as.numeric(X %*% beta_draws[b, ]) + re_offset)
    d <- dat
    # NOTE: you described weights = predicted propensity scores. If your
    # actual analysis uses inverse-probability weights, use 1 / p_star here
    # (and apply the same truncation/stabilization you used originally).
    d$propensity_resampled <- p_star
    
    
    # (b) Person-level resampling, stratified by school x vax_status so the
    #     exact matching still has candidates in each school
    boot_students <- students %>%
      group_by(schoolname, vax_status) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      ungroup() %>%
      mutate(original_ID = ID,
             boot_ID = row_number(),
             ID = boot_ID) # duplicated people become distinct
    
    # (c) Create Super Tester cohort
    boot_ST_students <- boot_students %>%
      filter(testing_completeness >= 1 - cfg$supertester_cutoff)
    
    # (d) Re-match: exact on school, at the student level
    match_full <- matching_procedure(boot_students, d)
    
    d_boot_full <- match_full[[1]]
    d_boot_resampled_full <- d_boot_full %>% 
      rename(original_propensity = propensity) %>%
      mutate(propensity = propensity_resampled)
    
    match_ST <- matching_procedure(boot_ST_students, d)
    
    d_boot_ST <- match_ST[[1]]
    
    # (e) Weighted Cox models with school random effect
    
      #Model 1: Full data, Standard Model
    Standard_formula <- "Surv(time = prior_week, time2 = week, event = positive) ~ vax_status + continuous_age + gender + race + dir_cert + prior_infections + (1 | schoolname)"
    fit_model1 <- fit_TTE_model(data = d_boot_full,
                                model_formula = as.formula(Standard_formula),
                                method = "Standard")
    
      #Model 2: ST data, Standard Model
    fit_model2 <- fit_TTE_model(data = d_boot_ST,
                                model_formula = as.formula(Standard_formula),
                                method = "Standard")
    
      #Model 3: Full data, Weighted Model OG weights
    Weighted_formula <- "Surv(time = prior_week, time2 = week, event = positive) ~ vax_status + continuous_age + gender + race + dir_cert + prior_infections + first_test_indicator + weeks_since_last_test + tests_last_4weeks + test_density + (1 | schoolname)"
    fit_model3 <-  fit_TTE_model(data = d_boot_full,
                                 model_formula = as.formula(Weighted_formula),
                                 method = "Weighted")
    
      #Model 4: Full data, Weighted Model, Resampled weights
    fit_model4 <-  fit_TTE_model(data = d_boot_resampled_full,
                                 model_formula = as.formula(Weighted_formula),
                                 method = "Weighted")
    
    c(full_standard  = unname(fixef(fit_model1[[1]])[vax_coef]),
      super_tester   = unname(fixef(fit_model2[[1]])[vax_coef]),
      weighted       = unname(fixef(fit_model3[[1]])[vax_coef]),
      weighted_rs    = unname(fixef(fit_model4[[1]])[vax_coef]))
    

  }, error = function(e) c(full_standard = NA_real_,
                           super_tester = NA_real_,
                           weighted = NA_real_, 
                           weighted_rs = NA_real_))
}

# ---- Step 4: run in parallel ----
start_time <- Sys.time()

if (cluster) {
  cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 1))
} else {
  cores <- 8
}

plan(multisession, workers = cores)
log_hr_full <- future_sapply(seq_len(B), one_rep, future.seed = TRUE)
plan(sequential)

end_time <- Sys.time()
elapsed_time <- end_time - start_time

print(elapsed_time)

# ---- Step 5: summarize ----

bootstrap_summary <- data.frame(se_log_hr = rep(NA, 4),
                                mean_ve = NA,
                                median_ve = NA,
                                ve_lower = NA,
                                ve_upper = NA)

for (i in 1:4) {
  log_hr <- log_hr_full[i,]
  
  mean(is.na(log_hr))                           # proportion of failed replicates
  log_hr <- log_hr[!is.na(log_hr)]
  
  se_log_hr <- sd(log_hr)
  ci_hr     <- quantile(exp(log_hr), c(0.025, 0.975))
  ci_ve     <- sort(1 - ci_hr)                  # VE = 1 - HR, so the bounds flip
  mean_ve <- 1 - exp(mean(log_hr))
  median_ve <- 1 - exp(median(log_hr))
  
  bootstrap_summary[i,] <- c(se_log_hr = se_log_hr, 
                         mean_ve = mean_ve, 
                         median_ve = median_ve,
                         ve_lower = ci_ve[1], 
                         ve_upper = ci_ve[2])
  
  bootstrap_summary
  
  hist(log_hr)
  hist(1 - exp(log_hr))
}


if (cluster) {
  saveRDS(bootstrap_summary, 
          file = paste0("/home/amoor53/APSVE_cluster/results/", task$subcohort, "_bootstrap_summary.rds"))
  saveRDS(log_hr_full, 
          file = paste0("/home/amoor53/APSVE_cluster/results/", task$subcohort, "_bootstrap_resamples.rds"))
} else {
  saveRDS(bootstrap_summary, 
          file = here("results", paste0(task$subcohort, "_bootstrap_summary.rds")))
  saveRDS(log_hr_full, 
          file = here("results", paste0(task$subcohort, "_bootstrap_resamples.rds")))
}
