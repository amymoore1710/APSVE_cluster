# 2026-02-26

color_main <- "skyblue3"
color_accent <- "#e9ecef"
color_alt1 <- "tomato"
color_alt2 <- "seagreen3"

# model_2021_5_11 <- readRDS(here("models", "2021_age_5_11_predicted_propensity_scores_model.rds"))
# summary(model_2021_5_11)

preds_2021_5_11 <- read.csv(here("cleandata", "2021_age_5_11_predicted_propensity_scores.csv"))
preds_2021_12_18 <- read.csv(here("cleandata", "2021_age_12_18_predicted_propensity_scores.csv"))
preds_2022_5_11 <- read.csv(here("cleandata", "2022_age_5_11_predicted_propensity_scores.csv"))
preds_2022_12_18 <- read.csv(here("cleandata", "2022_age_12_18_predicted_propensity_scores.csv"))


preds_combined <- rbind(preds_2021_5_11,
                        preds_2021_12_18,
                        preds_2022_5_11,
                        preds_2022_12_18)


plot1 <- ggplot(data = preds_combined, aes(x = propensity, fill = factor(tested, labels = c("Missed", "Tested")))) +
  geom_histogram(binwidth = 0.05, color = color_accent, position = "stack") +
  scale_fill_manual(values = c("Missed" = "gray", "Tested" = color_main)) +
  theme_classic() + theme(legend.position = "none")  + 
  theme(axis.title=element_text(size=20), axis.text.x = element_text(size = 20)) +
  labs(y = "Count", x = "Propensity")
plot1

preds_occured <- preds_combined %>% filter(tested == 1)

preds_vaxed <- preds_occured %>% filter(vax_status == 1)
preds_unvaxed <- preds_occured %>% filter(vax_status == 0)



plot2 <- ggplot() +
  # Top
  geom_histogram(data = preds_vaxed, 
                 aes(x = propensity, y = ..density..), 
                 fill=color_main,
                 binwidth = 0.05, 
                 color = color_accent) +
  # geom_label( aes(x=1, y=0.25, label="variable1"), color=color_main) +
  # Bottom
  geom_histogram(data = preds_unvaxed, 
                 aes(x = propensity, y = -..density..), 
                 fill= color_alt1,
                 binwidth = 0.05, 
                 color = color_accent) +
  # geom_label( aes(x=1, y=-0.25, label="variable2"), color=color_alt1) +
  # theme_ipsum() +
  labs(y = "Density", x = "Propensity") +
  xlim(-0.05,1.05) + ylim(-7,7)

plot2

ggsave(here("results","together_figure4A.png"), plot = plot1, width = 6, height = 4, dpi = 300)
ggsave(here("results","together_figure4B.png"), plot = plot2, width = 6, height = 4, dpi = 300)




