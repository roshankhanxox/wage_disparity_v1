# scripts/05_model_summary_table.R

library(broom)

model3 <- lm(wage_gap_ratio ~ year * education, data = wage_gap)

tidy(model3) %>%
  mutate(term = str_replace_all(term, "education", "")) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  arrange(p.value) %>%
  print(n = Inf)
