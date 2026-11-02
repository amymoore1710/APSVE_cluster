# =====================================================================
# Figure 4: Forest plot of VE estimates by adjustment method,
#            grouped by subcohort (one tall column), reference line at 0%,
#            propensity weighting highlighted
# =====================================================================

library(dplyr)
library(ggplot2)
library(stringr)
library(here)

# ---- 1. Simulate VE results (replace with your actual Table 3 data) ---

subcohort_order <- c("2021-22, 5-11", "2022-23, 5-11","2021-22, 12-18",  "2022-23, 12-18")
method_order <- c("None", "Demographic info", "Supertester cohort restriction",
                  "Average testing behavior", "Time-varying testing behavior",
                  "Propensity weighting")


sub_cohorts <- data.frame(year = c("2021", "2022", "2021", "2022"),
                          age = c("5_11", "5_11", "12_18", "12_18"))

results_table <- data.frame(X = rep(1, 24),
                            method = NA,
                            nstudents = NA,
                            ntests = NA,
                            VE = NA,
                            pval = NA)

for (j in 1:nrow(sub_cohorts)) {
  year <- sub_cohorts[j,]$year
  age <- sub_cohorts[j,]$age
  
  #Load parameter lists
    # params_list_TND <- read.csv(here("cleandata", paste0(year, "_age_", age, "_TND_params.csv")))
    params_list_TTE <- read.csv(here("cleandata",paste0(year, "_age_", age, "_TTE_params.csv")))
  
  # params_list <- rbind(params_list_TTE, params_list_TND)
  params_list <- params_list_TTE
  
  for (i in 1:nrow(params_list)) {
    method <- params_list$method[i]
      results_table[(6*(j-1) +i),] <- read.csv(here("results", paste0(year, "_age_", age, "_", method, "_model_fit.csv")))
  }
}



ve_data <- results_table %>%
  mutate(
    ve    = as.numeric(str_extract(VE, "^-?[0-9.]+")),
    lower = as.numeric(str_extract(VE, "(?<=\\()-?[0-9.]+")),
    upper = as.numeric(str_extract(VE, "(?<=to )-?[0-9.]+")),
    width = upper - lower,
    subcohort = factor(rep(subcohort_order, each = 6),
                       levels = subcohort_order),
    method = rep(method_order, times = 4),
    highlight = ifelse(method == "Propensity weighting", TRUE, FALSE)
  ) %>%
  select(subcohort, method, ve, width, lower, upper, pval, highlight)

method_include <- c("Demographic info", "Supertester cohort restriction","Propensity weighting")

ve_data <- ve_data %>%
  filter(method %in% method_include)

# =====================================================================
# 2. Plot
# =====================================================================

ggplot(ve_data, aes(x = ve, y = method, color = highlight, linewidth = highlight)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_pointrange(aes(xmin = lower, xmax = upper, shape = highlight, size = highlight)) +
  facet_grid(subcohort ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(limits = rev(method_include)) +
  scale_color_manual(values = c(`FALSE` = "grey40", `TRUE` = "#1f77b4"), guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
  scale_linewidth_manual(values = c(`FALSE` = 0.6, `TRUE` = 1.1), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 0.5, `TRUE` = 0.9), guide = "none") +
  labs(x = "Vaccine Efficacy (%)", y = NULL) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(hjust = 0)
  )

ggsave(here("results", "figure4_forest_plot.png"), width = 8, height = 6, dpi = 300)
