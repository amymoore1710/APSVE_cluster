
fit_TND_model <- function (data, model_formula, method, alpha = 0.05, seed = 582000) {
  
  set.seed(58200)
  
  #Standard GLM (No random effects)
  if (method == "TND_01") {
    model <- glm(formula = model_formula,
                  family = binomial,
                  data = data)
    summary(model)
  }
  
  #Mixed Effect GLM (Methods 2-5 only differ by data set and model formula) 
  if (method %in% c("TND_02", "TND_03", "TND_04", "TND_05")) {
    model <- glmer(formula = model_formula,
          family = binomial,
          data = data,
          control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1000000)))
    summary(model)
    
  }
  
  #Weighted GLM (Method 6)
  if (method == "TND_06") {
    
    IPS_weights <- round(1 / data$propensity, digits = 0)
    data$IPS_weights <- IPS_weights
    
    model <- glmer(formula = model_formula,
                    family = binomial,
                    data = data,
                    weights = IPS_weights,
                    control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=10000000)))
    summary(model)
    
  }
  
    #Summarize the model output
  nstudents <- length(unique(data$ID))
  ntests <- nrow(data)
  log_OR <- summary(model)$coefficients[2,1]
  VE_est <- 100 * (1 - exp(log_OR))
  log_OR_CI <- log_OR + c(-1, 1) * qnorm(1 - alpha/2) * summary(model)$coefficients[2,2]
  VE_CI <- 100 * (1 - exp(log_OR_CI))
  pval <- summary(model)$coefficients[2,4]
  
    #Save the output as a data frame
  results_model <- data.frame(method = method,
                              nstudents = nstudents,
                              ntests = ntests,
                              VE = paste0(round(VE_est, digits = 2), "% (",
                                          round(min(VE_CI), digits = 2), "% to ",
                                          round(max(VE_CI), digits = 2), "%)"),
                              pval = round(pval, digits = 4))
  
    #return the model object and the results data frame
  return_list <- list(model, results_model)
  return(return_list)
  
}