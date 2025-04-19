# scripts/02_plot_wage_gap.R

library(tidyverse)

# Load data
wage_gap <- readRDS("data/wage_gap_by_year.rds")

# Plot wage gap ratio over time by education level
ggplot(wage_gap, aes(x = year, y = wage_gap_ratio, color = education)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Gender Wage Gap Over Time by Education Level",
    x = "Year",
    y = "Wage Gap Ratio (Female / Male)",
    color = "Education Level"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14)
