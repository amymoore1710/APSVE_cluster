
fit_TTE_model <- function (data, model_formula, method, alpha = 0.05) {
  
  # print(length(unique(data$ID)))
  
    #Standard Cox Model (No random effects)
  if (method == "TTE_01") {
    model <- coxph(formula = model_formula,
                    data = data
    )
    summary(model)

  }
  
    #Mixed Effect Cox Model
  if (method %in% c("TTE_02", "TTE_03", "TTE_04", "TTE_05")) {
    model <- coxme(formula = model_formula,
                   data = data
    )
    summary(model)
    
  }
  
    #Weighted Cox Model
  if (method == "TTE_06") {
    
    print(method)
    
    IPS_weights <- round(1 / data$propensity, digits = 0)
    data$IPS_weights <- IPS_weights
    
    model <- coxme(formula = model_formula,
                   data = data,
                   weights = IPS_weights
    )
    summary(model)
    
  }
    
    if (method %in% c("TTE_01", "TTE_02", "TTE_03", "TTE_04")) {
      ntests <- sum(data$n_tests)
    } else { # "TTE_05" & "TTE_06"
      ntests <- nrow(data)
    }
    
    nstudents <- length(unique(data$ID))
    log_HR <- summary(model)$coefficients[1,1]
    VE_est <- 100 * (1 - exp(log_HR))
    log_HR_CI <- log_HR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model)$coefficients[1,3]
    VE_CI <- 100 * (1 - exp(log_HR_CI))
    pval <- summary(model)$coefficients[1,5]
    

  results_model <- data.frame(method = method,
                              nstudents = nstudents,
                              ntests = ntests,
                              VE = paste0(round(VE_est, digits = 2), "% (",
                                          round(min(VE_CI), digits = 2), "% to ",
                                          round(max(VE_CI), digits = 2), "%)"),
                              pval = round(pval, digits = 4))


  return_list <- list(model, results_model)

  return(return_list)
  
}

