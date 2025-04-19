# scripts/01_clean_wages_by_education.R

library(tidyverse)
library(janitor)

# Step 1: Load data
wages_raw <- read_csv("data/wages_by_education.csv") %>%
  clean_names()

# Step 2: Pivot all non-year columns
wages_long <- wages_raw %>%
  pivot_longer(
    cols = -year,
    names_to = "category",
    values_to = "wage"
  )

# Step 3: Keep only rows that start with men_ or women_
wages_gendered <- wages_long %>%
  filter(str_starts(category, "men_") | str_starts(category, "women_")) %>%
  mutate(
    gender = case_when(
      str_starts(category, "men_") ~ "Male",
      str_starts(category, "women_") ~ "Female"
    ),
    education = category %>%
      str_remove("^men_") %>%
      str_remove("^women_") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  ) %>%
  select(year, gender, education, wage)

# Step 4: Pivot wider to compare Male and Female wages
wage_gap <- wages_gendered %>%
  pivot_wider(names_from = gender, values_from = wage) %>%
  mutate(
    wage_gap_ratio = Female / Male
  ) %>%
  drop_na()

# View output
print(head(wage_gap))

# Step 5: Save cleaned data
saveRDS(wages_gendered, file = "data/cleaned_wages_gendered.rds")
saveRDS(wage_gap, file = "data/wage_gap_by_year.rds")
