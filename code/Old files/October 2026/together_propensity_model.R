

model_fit_2021_5_11 <- readRDS("C:/Users/amoor53/OneDrive - Emory University/Documents/Active/Su24 - APS Covid19/APSVE_cluster/models/2021_age_5_11_predicted_propensity_scores_model.rds")
summary(model_fit_2021_5_11)


model_fit_2021_12_18 <- readRDS("C:/Users/amoor53/OneDrive - Emory University/Documents/Active/Su24 - APS Covid19/APSVE_cluster/models/2021_age_12_18_predicted_propensity_scores_model.rds")
summary(model_fit_2021_12_18)




model_fit_2022_5_11 <- readRDS("C:/Users/amoor53/OneDrive - Emory University/Documents/Active/Su24 - APS Covid19/APSVE_cluster/models/2022_age_5_11_predicted_propensity_scores_model.rds")
summary(model_fit_2022_5_11)


model_fit_2022_12_18 <- readRDS("C:/Users/amoor53/OneDrive - Emory University/Documents/Active/Su24 - APS Covid19/APSVE_cluster/models/2022_age_12_18_predicted_propensity_scores_model.rds")
summary(model_fit_2022_12_18)

propensity_model_table <- cbind(summary(model_fit_2021_5_11)$coefficients [,1],
                                summary(model_fit_2022_5_11)$coefficients [,1],
                                summary(model_fit_2021_12_18)$coefficients [,1],
                                summary(model_fit_2022_12_18)$coefficients [,1])

rounded_table <- round(exp(propensity_model_table), digits = 2)

colnames(rounded_table) <- c("2021-22, Ages 5 - 11", 
                             "2022-23, Ages 5 - 11",
                             "2021-22, Ages 12 - 18",
                             "2022-23, Ages 12 - 18")

write.csv(rounded_table, 
          file = here("results", "propensity_model_coef_table.csv"))


# Install the package if you don't have it
install.packages("modelsummary")
library(modelsummary)

# Put your four models into a named list
model_list <- list(
  "Model 1" = model_fit_2021_5_11,
  "Model 2" = model_fit_2022_5_11,
  "Model 3" = model_fit_2021_12_18,
  "Model 4" = model_fit_2022_12_18
)

# Create the side-by-side table
modelsummary(model_list, stars = TRUE)





library(ggeffects)
library(ggplot2)

# each year's school-year start date (edit to your real dates)
start_dates <- tibble(
  year       = c("2021-22", "2022-23"),
  start_date = as.Date(c("2021-09-07", "2022-08-01"))
)

# ---- break weeks differ by year: define as (start, end) study-week ranges ----
breaks_df <- tibble::tibble(
  year   = c("2021-22", "2021-22", "2021-22",
             "2022-23", "2022-23", "2022-23", "2022-23", "2022-23"),
  break_name = c("Thanksgiving", "Christmas", "Spring",
                 "Fall", "Thanksgiving", "Christmas", "Winter", "Spring"),
  start  = c(11, 15, 30, 10, 16, 20, 29, 35),
  end    = c(13, 18, 32, 12, 18, 23, 31, 37)
) %>%
  left_join(start_dates, by = "year") %>%
  mutate(start_date_break = start_date + (start - 1) * 7,
         end_date_break   = start_date + (end - 1) * 7) %>%
  select(year, start_date_break, end_date_break)



# Extract predicted probability across calendar time, other covariates held at
# reference/mean (ggeffects does this automatically)
pred <- as.data.frame(ggpredict(model_fit_2021_5_11, terms = "week [all]")) %>%
  rename(week = x)

study_start <- start_dates$start_date[1]   # edit to your actual Year 1 start date

pred2 <- pred %>%
  mutate(date = study_start + ((week - 1) * 7))

plot_2021_5_11 <- ggplot(pred2, aes(x = date, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#1f77b4", alpha = 0.2) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  geom_rect(data = breaks_df %>% filter(year == "2021-22"), inherit.aes = FALSE,
            aes(xmin = start_date_break, xmax = end_date_break, ymin = -Inf, ymax = Inf),
            fill = "grey70", alpha = 0.25) +
  labs(x = "Study week (calendar time)",
       y = "Predicted probability of testing",
       title = "Partial effect of calendar time on testing propensity") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())



# ggsave("spline_partial_effect.png", width = 9, height = 5, dpi = 300)
