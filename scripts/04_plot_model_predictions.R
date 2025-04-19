# scripts/04_plot_model_predictions.R

library(tidyverse)

# Reload data
wage_gap <- readRDS("data/wage_gap_by_year.rds")

# Plot actual data with trend lines by education
ggplot(wage_gap, aes(x = year, y = wage_gap_ratio, color = education)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.2) +
  labs(
    title = "Gender Wage Gap Trends by Education Level",
    y = "Wage Gap Ratio (Female / Male)",
    x = "Year",
    color = "Education"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14)

