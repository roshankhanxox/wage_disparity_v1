# Load libraries
library(readxl)
library(dplyr)

# Load the dataset (skip if already loaded)
employees <- read_excel("data/Employees.xlsx")

# Clean column names to make them easier to reference
employees_clean <- employees %>%
  rename_with(~ gsub(" ", "_", .x))  # replaces spaces with underscores

# Summarize average salary and overtime by gender
summary_by_gender <- employees_clean %>%
  group_by(Gender) %>%
  summarise(
    avg_monthly_salary = mean(Monthly_Salary, na.rm = TRUE),
    avg_overtime_hours = mean(Overtime_Hours, na.rm = TRUE),
    avg_sick_leaves = mean(Sick_Leaves, na.rm = TRUE),
    avg_unpaid_leaves = mean(Unpaid_Leaves, na.rm = TRUE),
    .groups = "drop"
  )

# View the summary
print(summary_by_gender)


# Recode Gender to a binary numeric variable for regression
employees_clean <- employees_clean %>%
  mutate(Gender_bin = ifelse(Gender == "Male", 1, 0))

# Run regression: Monthly Salary ~ Gender + Overtime + Leaves
model <- lm(Monthly_Salary ~ Gender_bin + Overtime_Hours + Sick_Leaves + Unpaid_Leaves, data = employees_clean)

# Show summary
summary(model)
