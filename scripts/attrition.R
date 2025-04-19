# ─────────────────────────────────────────────
# 👥 EMPLOYEE ATTRITION & GENDER ANALYSIS
# Dataset: employee_attrition_dataset_10000.csv
# Author: [Your Name]
# Description: Exploring gender differences in attrition, income & roles
# ─────────────────────────────────────────────

# 1️⃣ Load Required Libraries
library(tidyverse)
setwd("C:/Users/CHHANDAK/Documents/wage_disparity_v1/scripts")
# 2️⃣ Load the Data
employee_data <- read_csv("employee_attrition_dataset_10000.csv")
# ─────────────────────────────────────────────

# 3️⃣ Check for missing values and clean column names
employee_data <- employee_data %>%
  janitor::clean_names()  # lowercase and snake_case for consistency

# ─────────────────────────────────────────────
# 4️⃣ Summary Table: Avg Income & Work Hours by Gender & Attrition

summary_table <- employee_data %>%
  group_by(gender, attrition) %>%
  summarise(
    avg_monthly_income = round(mean(monthly_income, na.rm = TRUE), 2),
    avg_hours_worked = round(mean(average_hours_worked_per_week, na.rm = TRUE), 1),
    avg_overtime_rate = mean(overtime == "Yes", na.rm = TRUE)
  ) %>%
  arrange(gender, attrition)

print(summary_table)

# ─────────────────────────────────────────────
# 5️⃣ Visualization: Average Monthly Income by Gender & Attrition

ggplot(summary_table, aes(x = gender, y = avg_monthly_income, fill = attrition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Monthly Income by Gender and Attrition",
    x = "Gender", y = "Avg Monthly Income ($)"
  ) +
  theme_minimal()

# ─────────────────────────────────────────────
# 6️⃣ Boxplot: Monthly Income Distribution by Gender

ggplot(employee_data, aes(x = gender, y = monthly_income, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribution of Monthly Income by Gender",
    x = "Gender", y = "Monthly Income"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ─────────────────────────────────────────────
# 7️⃣ Optional: Explore Effect of Overtime

ggplot(employee_data, aes(x = gender, y = monthly_income, fill = overtime)) +
  geom_boxplot() +
  facet_wrap(~overtime) +
  labs(
    title = "Monthly Income by Gender and Overtime Status",
    x = "Gender", y = "Monthly Income"
  ) +
  theme_minimal()
# ─────────────────────────────────────────────
# 8️⃣ Plot: Gender vs Attrition Rate

attrition_by_gender <- employee_data %>%
  group_by(gender) %>%
  summarise(attrition_rate = mean(attrition == "Yes") * 100)

ggplot(attrition_by_gender, aes(x = gender, y = attrition_rate, fill = gender)) +
  geom_col() +
  labs(title = "Attrition Rate by Gender", x = "Gender", y = "Attrition Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# ─────────────────────────────────────────────
# 9️⃣ Plot: Income vs Job Role by Gender

job_income_gender <- employee_data %>%
  group_by(job_role, gender) %>%
  summarise(avg_income = mean(monthly_income, na.rm = TRUE)) %>%
  ungroup()

ggplot(job_income_gender, aes(x = reorder(job_role, avg_income), y = avg_income, fill = gender)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Average Monthly Income by Job Role and Gender",
       x = "Job Role", y = "Monthly Income") +
  theme_minimal()

# ─────────────────────────────────────────────
# 🔟 Regression Analysis: Predicting Monthly Income

# Create binary variables
employee_data <- employee_data %>%
  mutate(
    gender_bin = ifelse(gender == "Male", 1, 0),
    attrition_bin = ifelse(attrition == "Yes", 1, 0),
    overtime_bin = ifelse(overtime == "Yes", 1, 0)
  )

# Regression Model
income_model <- lm(
  monthly_income ~ gender_bin + job_level + overtime_bin + performance_rating +
    years_at_company + years_since_last_promotion + training_hours_last_year,
  data = employee_data
)

summary(income_model)

# ─────────────────────────────────────────────
# 🔁 OPTIONAL: Check interaction between Gender & Overtime

interaction_model <- lm(
  monthly_income ~ gender_bin * overtime_bin + job_level + years_at_company,
  data = employee_data
)

summary(interaction_model)

# ─────────────────────────────────────────────
# 1️⃣1️⃣ Plot: Work-Life Balance by Gender

ggplot(employee_data, aes(x = work_life_balance, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Work-Life Balance Ratings by Gender",
       x = "Work-Life Balance (1 = Low, 4 = Excellent)", y = "Count") +
  theme_minimal()

# ─────────────────────────────────────────────
# 1️⃣2️⃣ Plot: Training Hours vs Performance by Gender

ggplot(employee_data, aes(x = performance_rating, y = training_hours_last_year, fill = gender)) +
  geom_boxplot() +
  labs(title = "Training Hours vs Performance Rating by Gender",
       x = "Performance Rating", y = "Training Hours Last Year") +
  theme_minimal()

# 🎯 END OF SCRIPT

# 🎉 END OF SCRIPT
