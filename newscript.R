# Loading necessary libraries
library(tidyverse)
setwd("C:/Users/CHHANDAK/Documents/wage-disparity/datasets")

# Read the data set
wages <- read_csv("wages_by_education.csv")

# 1️⃣ Bar Chart: Men vs Women by Education Level (2022)
latest_data <- wages %>% filter(year == max(year))

gender_wage_data <- tibble(
  Education = c("Less than HS", "High School", "Some College", 
                "Bachelor's Degree", "Advanced Degree"),
  Men = c(latest_data$men_less_than_hs,
          latest_data$men_high_school,
          latest_data$men_some_college,
          latest_data$men_bachelors_degree,
          latest_data$men_advanced_degree),
  Women = c(latest_data$women_less_than_hs,
            latest_data$women_high_school,
            latest_data$women_some_college,
            latest_data$women_bachelors_degree,
            latest_data$women_advanced_degree)
)

gender_wage_long <- gender_wage_data %>%
  pivot_longer(cols = c(Men, Women), names_to = "Gender", values_to = "Wage")

ggplot(gender_wage_long, aes(x = Education, y = Wage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Wages by Gender and Education (2022)",
       x = "Education Level", y = "Average Hourly Wage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 2️⃣ Line Chart: Gender Wage Gap Over Time
wage_gap <- wages %>%
  transmute(
    year,
    `Less than HS` = men_less_than_hs - women_less_than_hs,
    `High School` = men_high_school - women_high_school,
    `Some College` = men_some_college - women_some_college,
    `Bachelor's Degree` = men_bachelors_degree - women_bachelors_degree,
    `Advanced Degree` = men_advanced_degree - women_advanced_degree
  )

wage_gap_long <- wage_gap %>%
  pivot_longer(cols = -year, names_to = "Education", values_to = "Wage_Gap")

ggplot(wage_gap_long, aes(x = year, y = Wage_Gap, color = Education)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Gender Wage Gap Over Time by Education Level",
       x = "Year", y = "Wage Gap (Men - Women, $/hr)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 3️⃣ Bar Plot: Gender Wage Gap as Percentage
gap_percent <- tibble(
  Education = c("Less than HS", "High School", "Some College", 
                "Bachelor's Degree", "Advanced Degree"),
  Gap_Percent = 100 * (c(latest_data$men_less_than_hs, latest_data$men_high_school, latest_data$men_some_college,
                         latest_data$men_bachelors_degree, latest_data$men_advanced_degree) -
                         c(latest_data$women_less_than_hs, latest_data$women_high_school, latest_data$women_some_college,
                           latest_data$women_bachelors_degree, latest_data$women_advanced_degree)) /
    c(latest_data$men_less_than_hs, latest_data$men_high_school, latest_data$men_some_college,
      latest_data$men_bachelors_degree, latest_data$men_advanced_degree)
)

ggplot(gap_percent, aes(x = Education, y = Gap_Percent, fill = Education)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender Wage Gap (Percentage, 2022)",
       y = "Wage Gap (%)", x = "Education Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

# 4️⃣ Heatmap: Wages for Black and Hispanic Women (2022)
#library(tidyverse)
#heat_data <- latest_data %>%
#select(
#black_women_less_than_hs, black_women_high_school, black_women_some_college,
#black_women_bachelors_degree, black_women_advanced_degree,
#hispanic_women_less_than_hs, hispanic_women_high_school, hispanic_women_some_college,
#hispanic_women_bachelors_degree, hispanic_women_advanced_degree
#) %>%
#pivot_longer(cols = everything(), names_to = "Group", values_to = "Wage") %>%
#mutate(
#Ethnicity = ifelse(str_detect(Group, "black"), "Black Women", "Hispanic Women"),
#Education = case_when(
#str_detect(Group, "less_than_hs") ~ "Less than HS",
#str_detect(Group, "high_school") ~ "High School",
#str_detect(Group, "some_college") ~ "Some College",
#str_detect(Group, "bachelors_degree") ~ "Bachelor's Degree",
#str_detect(Group, "advanced_degree") ~ "Advanced Degree"
#)
#)
#ggplot(heat_data, aes(x = Education, y = Ethnicity, fill = Wage)) +
#geom_tile(color = "white") +
#geom_text(aes(label = round(Wage, 2)), size = 3) +  # 💬 adds wage labels
#scale_fill_gradientn(
#colours = c("#fff7bc", "#fec44f", "#d95f0e"),  # Soft yellow to deep orange
#values = scales::rescale(c(min(heat_data$Wage), mean(heat_data$Wage), max(heat_data$Wage))),
#name = "Wage ($/hr)"
#)+
#labs(title = "Wages of Black & Hispanic Women by Education",
# x = "Education Level", y = "Group") +
# theme_minimal() +
# theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 5️⃣ Linear Regression: Predicting Wages from Education & Gender

# Prepare the data for modeling
model_data <- gender_wage_long  # Already has Education, Gender, Wage

# Convert Education and Gender to factors (important for modeling)
model_data <- model_data %>%
  mutate(
    Education = factor(Education, levels = c("Less than HS", "High School", "Some College", "Bachelor's Degree", "Advanced Degree")),
    Gender = factor(Gender)
  )

# Fit the linear model
wage_model <- lm(Wage ~ Education + Gender, data = model_data)

# Show model summary
summary(wage_model)

# Predicted wages from the model
model_data$Predicted_Wage <- predict(wage_model, model_data)

# Plot actual vs predicted wages with labels
ggplot(model_data, aes(x = Education, y = Wage, fill = Gender)) +
  # Bar plot for actual wages
  geom_col(position = position_dodge(width = 0.9), alpha = 0.7, width = 0.7) +
  
  # Actual wage labels on bars
  geom_text(aes(label = round(Wage, 0.5)),
            position = position_dodge(width = 0.9),
            vjust = -1.2, size = 2.3, color = "black") +
  
  # Predicted wage points
  geom_point(aes(y = Predicted_Wage, color = Gender),
             position = position_dodge(width = 0.9), shape = 21, size = 3, stroke = 1.2) +
  
  # Predicted wage labels near points
  geom_text(aes(y = Predicted_Wage, label = round(Predicted_Wage, 1), group = Gender),
            position = position_dodge(width = 0.9),
            vjust = 2.2, size = 2.3, color = "black")

# Plot labels and theme
labs(title = "Actual vs Predicted Wages by Education and Gender (2022)",
     subtitle = "Bars = Actual Wages | Dots = Predicted Wages from Linear Regression",
     y = "Wage ($/hr)", x = "Education Level") +
  scale_fill_manual(values = c("Men" = "#fca5a5", "Women" = "#7dd3fc")) +
  scale_color_manual(values = c("Men" = "#b91c1c", "Women" = "#0369a1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.subtitle = element_text(size = 10, face = "italic"))
