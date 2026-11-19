
  #Helper Functions 

  # Matching Procedure
    # data is a data.frame with one row per ID
    # columns for ID, schoolname, vax_status, and first_test_week
matching_procedure <- function(ID_data, weekly_data, verbose = TRUE) {
  
  match_raw <- matchit(formula = vax_status ~ schoolname + first_test_week,
                       data = ID_data,
                       method = "nearest",
                       distance = "glm",
                       replace = FALSE,
                       exact = ~schoolname
  )  
  
  match_output <- match.data(match_raw) %>% 
    arrange(subclass, vax_status) %>% 
    select(!c("distance", "weights"))
  
  if (verbose) {
    print("")
    print("Checking Matched Cohort Sample Sizes")
    print("")
    ntests <- sum(match_output$n_weeks_tested)
    nIDs <-   nrow(match_output)
    print(paste0("Number of Tests: ", ntests))
    print(paste0("Number of Students: ", nIDs))
    
  }
  
  print("")
  print("------------------------------")
  print("Assigning Enrollment based on Matches")
  print("------------------------------")
  print("")
  
  enrollment_date <- match_output %>%
    filter(vax_status == 1) %>%
    mutate(enrollment_case = case_when(
      vax_date > task$start_date ~ "Vax during study",
      vax_date <= task$start_date ~ "Vax before study",
      .default = "Error"
    ),
    enrollment_date = case_when(
      enrollment_case == "Vax during study" ~ vax_date,
      enrollment_case == "Vax before study" ~ task$start_date,
      enrollment_case == "Error" ~ NA,
      .default = NA
    )) %>%
    select(subclass, enrollment_case, enrollment_date)
  
  match_simple <- match_output %>%
    left_join(enrollment_date, by = "subclass") %>%
    select(ID, subclass, enrollment_date)
  
  print("")
  print("")
  
  print("Reduce Weekly test data to only include matched data")
  
  print("")
  print("")
  
  #Reduce Weekly test data to only include matched data 
  #Merge in match and enroll date
  #Exclude unmatched IDs
  weekly_data <- weekly_data %>% rename(original_ID = ID)
  
  weekly_data_v2 <- ID_data %>% select("original_ID", "boot_ID", "ID") %>%
    inner_join(weekly_data, by = "original_ID", relationship = "many-to-many")
  
  weekly_match <- weekly_data_v2 %>%
    left_join(match_simple, by = "ID") %>%
    filter(!is.na(subclass))
  
  #Checking that both matches have tests
  checking <- weekly_match %>% group_by(subclass) %>% summarize(n_IDs = length(unique(ID))) %>% filter(n_IDs < 2)
  print(paste("Check #1 - # of matches missing partners:", nrow(checking)))
  
  print("")
  print("")
  
  print("Reduce Weekly test data to censor before enrollment and remove weeks when testing did not occur")
  
  print("")
  print("")
  
  #Reduce Weekly test data to only include eligible tests
    #Exclude weeks from before enrollment
    #Exclude weeks for late vaxers who are used as unvaxed and censored at vaccination
  weekly_match_only <- weekly_match %>%
    filter(enrollment_date <= week_start_date) %>%
    filter(final_group != "Unvaccinated (censored at vax date)" |
             week_start_date < vax_date)
  
  #Reduce Weekly test data to censor after first positive result
  #Collect Week number of first positive for each with at least one positive
  first_positive_events <- weekly_match_only %>%
    filter(positive == 1) %>%
    group_by(ID) %>%
    summarise(first_positive_week = min(week, na.rm = TRUE), .groups = "drop")
  
  weekly_match_only <- weekly_match_only %>%
    left_join(first_positive_events, by = "ID") %>%
    filter(is.na(first_positive_week) | week <= first_positive_week)
  
  weekly_match_only <- weekly_match_only %>%
    mutate(prior_week = week - 1) %>%
    select(ID, subcohort, school_year, age_group,
           schoolname, gender, race, age, continuous_age,
           dir_cert, prior_infections,
           subclass, vax_status, vax_date, enrollment_date,
           prior_week, week, week_start_date, avail_index,
           num_tests, tested, positive, 
           weeks_since_last_test, tests_last_4weeks,
           test_density, first_test_indicator,
           propensity, propensity_resampled,
           original_ID, boot_ID)
  
  #Exclude weeks when no testing occured
  weekly_match_observed <- weekly_match_only %>%
    filter(tested == 1)
  
  #Checking that both matches have tests
  checking <- weekly_match_observed %>% group_by(subclass) %>% summarize(n_IDs = length(unique(ID))) %>% filter(n_IDs < 2)
  print(paste("Check #2 - # matches with a partner with 0 tests post enrollment:", nrow(checking)))
  
  
  #Removing matched pairs where one member has no tests after enrollment
  weekly_match_observed <- weekly_match_observed %>%
    filter(!(subclass %in% checking$subclass))
  weekly_match_only <- weekly_match_only %>%
    filter(!(subclass %in% checking$subclass))
  
  return(list(weekly_match_observed, match_simple))
  
  
}


fit_TTE_model <- function (data, model_formula, method, alpha = 0.05) {
  
    #Standard Model
  if(method == "Standard") {
    model <- coxme(formula = model_formula,
                   data = data)
    # summary(model)
  }
  
    #Weighted
  if(method == "Weighted") {
    
    IPS_weights <- round(1 / data$propensity, digits = 0)
    data$IPS_weights <- IPS_weights
    
    model <- coxme(formula = model_formula,
                   data = data,
                   weights = IPS_weights
    )
    # summary(model)
  }
  
  
  #Summarize the model output
  nstudents <- length(unique(data$ID))
  npositives <- nrow(data %>% filter(positive == 1))
  log_HR <- summary(model)$coefficients[1,1]
  VE_est <- 100 * (1 - exp(log_HR))
  log_HR_CI_lower <- log_HR - qnorm(1 - alpha/2) * summary(model)$coefficients[1,3]
  log_HR_CI_upper <- log_HR + qnorm(1 - alpha/2) * summary(model)$coefficients[1,3]
  VE_CI_lower <- 100 * (1 - exp(log_HR_CI_upper))
  VE_CI_upper <- 100 * (1 - exp(log_HR_CI_lower))
  pval <- summary(model)$coefficients[1,5]
  
  
  #Save the output as a data frame
  model_results <- data.frame(method = method,
                              nstudents = nstudents,
                              npositives = npositives,
                              VE_est = VE_est,
                              VE_lower = VE_CI_lower,
                              VE_upper = VE_CI_upper,
                              pval = pval)
  
  #return the model object and the results data frame
  return_list <- list(model, model_results)
  return(return_list)
  
}