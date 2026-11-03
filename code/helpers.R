
  #Helper Functions 

fit_TTE_model <- function (data, model_formula, method, alpha = 0.05) {
  
    #Standard Model
  if(method == "Standard") {
    model <- coxme(formula = model_formula,
                   data = data)
    summary(model)
  }
  
    #Weighted
  if(method == "Weighted") {
    
    IPS_weights <- round(1 / data$propensity, digits = 0)
    data$IPS_weights <- IPS_weights
    
    model <- coxme(formula = model_formula,
                   data = data,
                   weights = IPS_weights
    )
    summary(model)
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