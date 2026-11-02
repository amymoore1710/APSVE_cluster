##### Start of File #3 #####


print("")
print("")
print("------------------------------")
print("Begin File 3: Compute Propensity Scores")
print("------------------------------")
print("")
print("")


print("")
print("------------------------------")
print("Step 1: Fit Propensity Score Model")
print("------------------------------")
print("")

library(readr) #Read in Files
library(lubridate) #Date Objects
library(dplyr) #Tidy Data
library(tidyverse) # Data Manipulation
library(tidyr) # Data Manipulation
library(slider) # Calendar Time Handling
library(lme4) #Fitting GLMMs (for adding random effects)
library(splines) #Adding Splines to GLMMs
library(here)


  #Download and prep dataset
VScohort.weekly <- read_csv(here("cleandata", "All_cohort_weekly_testing_by_ID.csv"))




  #Split Data by Subcohort
# subcohort_to_fit <- "2021-22 Age 5-11"
# subcohort_to_fit <- "2021-22 Age 12-18"
# subcohort_to_fit <- "2022-23 Age 5-11"
subcohort_to_fit <- "2022-23 Age 12-18"

Propensity_model_data <- VScohort.weekly  %>%
  filter(is_available == TRUE) %>%
  filter(subcohort == subcohort_to_fit) %>%
  mutate(weeks_since_last_test = coalesce(weeks_since_last_test, 0),
         test_density = coalesce(test_density, 0),
         continuous_age = as.numeric(week_start_date - birthdate) / 365.25,
         vax_status = ifelse(is.na(vax_date), 0,
                             ifelse(as.numeric(week_start_date - vax_date) > 0, 1, 0)))

length(unique(Propensity_model_data$ID))

# Propensity_model_data %>% summarise(pct_na = mean(is.na(weeks_since_last_test)) * 100)

Propensity_model_data <- Propensity_model_data %>%
  mutate(across(c(continuous_age, weeks_since_last_test, tests_last_4weeks,
                  test_density, week), ~ as.numeric(scale(.))))


#   #Test model with first 100 students
# Subset_ids <- unique(Propensity_model_data$ID)[1:100]
# 
# Subset_model_data <- Propensity_model_data %>%
#   filter(ID %in% Subset_ids)
# 
# library(performance)
# # fit a quick non-mixed glm first as a fast proxy check, before touching the full glmer
# quick_glm <- glm(tested ~ vax_status + continuous_age + gender + race + dir_cert + prior_infections +
#                    first_test_indicator + weeks_since_last_test + tests_last_4weeks +
#                    test_density + week, data = Propensity_model_data, family = binomial)
# check_collinearity(quick_glm)
# 
# 
# Propensity_model_data %>% count(race)
# Propensity_model_data %>% count(gender)
# Propensity_model_data %>% count(dir_cert)
# 
# # check per-school variation specifically, since dir_cert may barely vary within school
# Propensity_model_data %>% group_by(schoolname) %>% summarise(dir_cert_sd = sd(dir_cert, na.rm = TRUE)) %>%
#   filter(dir_cert_sd == 0 | is.na(dir_cert_sd))
# 
# # How many schools do you actually have per subcohort?
# Propensity_model_data %>% summarise(n_schools = n_distinct(schoolname))
# 
# # Is dir_cert essentially unique per school (one value per school)?
# Propensity_model_data %>% distinct(schoolname, dir_cert) %>% count(schoolname) %>% filter(n > 1)  # should be empty if truly school-level
# Propensity_model_data %>% distinct(schoolname, dir_cert) %>% nrow()   # vs. n_distinct(schoolname) above
# 
# 
# Propensity_model_data %>% count(schoolname, tested) %>% tidyr::pivot_wider(names_from = tested, values_from = n, values_fill = 0) %>%
#   filter(`0` == 0 | `1` == 0)

# Propensity_model_data %>% summarise(n_schools = n_distinct(schoolname), n_ids = n_distinct(ID))
# Propensity_model_data %>% count(schoolname) %>% summarise(min_n = min(n), median_n = median(n))


start_time <- Sys.time()

reg.fit <-  glmer(formula = tested ~ vax_status + continuous_age + gender + race + dir_cert + 
                    prior_infections + first_test_indicator + weeks_since_last_test + 
                    tests_last_4weeks + test_density + week + (1 | schoolname) + (1 | ID),
                  family = binomial,
                  data = Propensity_model_data,
                  control = glmerControl(optimizer = "bobyqa"))

end_time <- Sys.time()
elapsed_time <- end_time - start_time

print(elapsed_time)

summary(reg.fit)

diff_optims <- allFit(reg.fit)
summary(diff_optims)

saveRDS(reg.fit,
        file = here("models", paste0(subcohort_to_fit, " Propensity Model.rds")))


prediction_data <- Propensity_model_data %>% select(vax_status, continuous_age, gender, race, dir_cert, prior_infections, 
                                                    first_test_indicator, weeks_since_last_test, tests_last_4weeks, test_density,
                                                    schoolname, ID, week)

propensities <- predict(reg.fit, type = "response")
VScohort.tested$propensity <- propensities






VScohort.tested <- together

VScohort.IDs <- sort(unique(VScohort.tested$ID))

weeks <- sort(unique(VScohort.tested$week))


#Creating Interaction terms implicitly

# VScohort.tested <- read_csv(here("cleandata", "2021_age_5_11_weekly_testing_by_ID.csv"))


#If there is no testing history, set all testing behavior covariates to 0
VScohort.tested[which(VScohort.tested$no_history == 1), c("num_prev_test", "time_since_last", "tests_in_28", "tests_in_14", "test_density", "adj_test_density")] <- 0



# Full Sample Fit --> Random effect for ID

VScohort.IDs <- sort(unique(VScohort.tested$ID))

print("Fitting Model Now")

#Logistic Regression Model (school and ID as random effect)
# reg.fit <-  glmer(formula = tested ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history,
#                     family = binomial,
#                     data = VScohort.tested,
#                     control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=1000000), tolPwrss = 1e-3))

start_time <- Sys.time()

reg.fit <-  glmer(formula = tested ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + num_prev_test + time_since_last + tests_in_28 + tests_in_14 + test_density + adj_test_density + no_history,
                  family = binomial,
                  data = VScohort.tested)

end_time <- Sys.time()
elapsed_time <- end_time - start_time

print(elapsed_time)


summary(reg.fit)


start_time <- Sys.time()

reg.fit2 <-  glmer(formula = tested ~ vax_status + start_age + gender + race + dir_cert + prior_infections + (1 | schoolname) + (1 | ID) + ns(week, df = 5) + time_since_last + tests_in_14 + adj_test_density + no_history,
                   family = binomial,
                   data = VScohort.tested)

end_time <- Sys.time()
elapsed_time <- end_time - start_time

print(elapsed_time)


summary(reg.fit2)

print("")
print("------------------------------")
print("Step 2: Save Propensity Score Model")
print("------------------------------")
print("")

saveRDS(reg.fit, "/home/amoor53/APSVE_cluster/models/2021_age_5_11_predicted_propensity_scores_model.rds")


prediction_data <- VScohort.tested %>% select(vax_status, start_age, gender, race, dir_cert, prior_infections, schoolname, ID, week, num_prev_test, time_since_last, tests_in_28, tests_in_14, test_density, adj_test_density, no_history)

propensities <- predict(reg.fit, newdata = VScohort.tested, type = "response")
VScohort.tested$propensity <- propensities


print("")
print("------------------------------")
print("Step 3: Save Propensity Score Fitted Values")
print("------------------------------")
print("")

#Save Dataset
write.csv(VScohort.tested, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_predicted_propensity_scores.csv")

write.csv(sample.sizes, "/home/amoor53/APSVE_cluster/results/2021_age_5_11_dataset_workflow_sample_sizes.csv")


print("")
print("")
print("")
print("------------------------------")
print("------------------------------")
print("End File 3: Saving Weekly testing by ID with Propensity Scores")
print("------------------------------")
print("------------------------------")
print("")
print("")
print("")













##### File #4 - Creating TND data sets #####

print("")
print("")
print("------------------------------")
print("Begin File 4: Create Model Fitting Data Sets")
print("------------------------------")
print("")
print("")

print("")
print("------------------------------")
print("Step 1: Subset to no missing weeks")
print("------------------------------")
print("")

VScohort.alltested <- VScohort.tested %>% filter(tested == 1)

print("")
print("------------------------------")
print("Step 2: Adding Additional Covariates")
print("------------------------------")
print("")

#Adding Average Testing Behavior for VScohort.alltested
average_testing_behavior_byID <- VScohort.alltested %>% group_by(ID) %>% 
  summarize(total_tests = sum(tested), avg_time_since_last = mean(time_since_last), 
            avg_tests_in_28 = mean(tests_in_28), avg_tests_in_14 = mean(tests_in_14), 
            avg_test_density = mean(test_density), avg_adj_test_density = mean(adj_test_density))

VScohort.alltested <- merge(x = VScohort.alltested, 
                            y = average_testing_behavior_byID,
                            by = "ID",
                            all.x = TRUE)

print("")
print("------------------------------")
print("Step 3: Subset to Super Tester Cohort")
print("------------------------------")
print("")

#Defining the Super Tester Cohort
VScohort.byID <- VScohort.tested %>% group_by(ID) %>% summarize(num_tests = sum(tested))
max_tests <- max(VScohort.byID$num_tests)
Q3 <- max_tests
n <- nrow(VScohort.byID)

for (i in seq(max_tests, 3, -1)){
  n_above <- nrow(VScohort.byID %>% filter(num_tests >= i))
  perc_above <- round(n_above/n * 100, digits = 2)
  
  if (perc_above < 25.00) {
    Q3 <- i
  }
}
if (verbose) {
  print(paste0("Super Tester Cut Off:", Q3))
}


STcohort.subset.byID <- VScohort.byID %>% filter(num_tests >= Q3)
Supertester.IDs <- STcohort.subset.byID$ID

STcohort.alltested <- VScohort.alltested %>% filter(ID %in% Supertester.IDs)

print("")
print("------------------------------")
print("Step 4: Save Data Sets")
print("------------------------------")
print("")

# Save Datasets
if (cluster) {
  write.csv(VScohort.alltested, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_all_tested.csv")
  write.csv(STcohort.alltested, "/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_ST_all_tested.csv")
} else {
  write.csv(VScohort.alltested, file = here("cleandata", "2021_age_5_11_all_tested.csv"))
  write.csv(STcohort.alltested, file = here("cleandata", "2021_age_5_11_ST_all_tested.csv"))
}


print("")
print("")
print("")
print("------------------------------")
print("------------------------------")
print("End File 4: Saving Weekly testing by ID with Propensity Scores Model Ready")
print("------------------------------")
print("------------------------------")
print("")
print("")
print("")