# scripts/03_regression_wage_gap.R

library(tidyverse)

# Load the cleaned data
wage_gap <- readRDS("data/wage_gap_by_year.rds")

# Convert education to a factor
wage_gap <- wage_gap %>%
  mutate(education = factor(education, levels = unique(education)))

# Model 1: Wage gap over time
model1 <- lm(wage_gap_ratio ~ year, data = wage_gap)
summary(model1)

# Model 2: Add education as a predictor
model2 <- lm(wage_gap_ratio ~ year + education, data = wage_gap)
summary(model2)

# Model 3: Interaction between year and education
model3 <- lm(wage_gap_ratio ~ year * education, data = wage_gap)
summary(model3)

# Optional: View model comparison
anova(model1, model2, model3)
