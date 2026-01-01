# Example Datasets for BBC-Style Data Visualization
# This script creates realistic datasets for demonstrating BBC visualization functionality

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(ClinicoPath)

# Set seed for reproducibility
set.seed(20241201)

# Helper function to create realistic trend data
create_trend <- function(start_val, end_val, n_points, noise_sd = 0.1) {
  trend <- seq(start_val, end_val, length.out = n_points)
  trend + rnorm(n_points, 0, abs(trend) * noise_sd)
}

# 1. Economic Indicators Dataset - BBC Economic News Style
economic_indicators_data <- data.frame(
  quarter = rep(c("Q1 2023", "Q2 2023", "Q3 2023", "Q4 2023", "Q1 2024"), 4),
  year = rep(c(2023, 2023, 2023, 2023, 2024), 4),
  region = factor(rep(c("North", "South", "East", "West"), each = 5)),
  country = factor(rep(c("England", "Scotland", "Wales", "N. Ireland"), each = 5)),
  
  # Realistic economic indicators
  gdp_growth = c(
    c(2.1, 2.3, 2.0, 1.8, 2.2),  # North
    c(1.8, 1.9, 1.7, 1.6, 1.9),  # South  
    c(1.5, 2.0, 1.3, 1.4, 1.7),  # East
    c(1.2, 1.4, 1.1, 1.0, 1.3)   # West
  ),
  
  unemployment_rate = c(
    c(3.2, 3.1, 3.0, 3.3, 3.1),  # North
    c(4.1, 4.0, 4.2, 4.3, 4.1),  # South
    c(5.2, 5.1, 5.4, 5.3, 5.0),  # East
    c(4.8, 4.6, 4.9, 5.1, 4.7)   # West
  ),
  
  inflation_rate = round(create_trend(2.0, 2.0, 20, 0.05), 1),
  business_confidence = round(runif(20, 45, 75), 1),
  house_prices_change = round(rnorm(20, 5.2, 2.1), 1)
)

# 2. Election Survey Dataset - BBC Political Coverage Style  
election_survey_data <- data.frame(
  age_group = factor(rep(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), each = 50)),
  gender = factor(rep(c("Male", "Female"), 150)),
  education = factor(sample(c("No qualifications", "GCSE", "A-Level", "University"), 
                           300, replace = TRUE, prob = c(0.15, 0.25, 0.25, 0.35))),
  region = factor(sample(c("London", "South East", "South West", "Midlands", "North", "Scotland"), 
                        300, replace = TRUE)),
  
  # Realistic UK party support
  party_support = factor(sample(c("Conservative", "Labour", "Liberal Democrat", "Green", "Reform", "Other"), 
                               300, replace = TRUE, prob = c(0.22, 0.42, 0.12, 0.08, 0.10, 0.06))),
  
  voting_likelihood = sample(1:10, 300, replace = TRUE, prob = c(0.05, 0.05, 0.05, 0.08, 0.10, 0.12, 0.15, 0.15, 0.15, 0.10)),
  top_issue = factor(sample(c("Economy", "Healthcare", "Immigration", "Climate", "Education", "Housing"), 
                           300, replace = TRUE, prob = c(0.25, 0.20, 0.15, 0.15, 0.12, 0.13))),
  
  pm_approval = sample(1:5, 300, replace = TRUE, prob = c(0.20, 0.25, 0.25, 0.20, 0.10)),
  government_approval = sample(1:5, 300, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.15, 0.05)),
  
  poll_date = sample(seq(as.Date("2024-01-01"), as.Date("2024-06-01"), by = "day"), 300, replace = TRUE),
  sample_weight = runif(300, 0.8, 1.2)
)

# 3. Health Statistics Dataset - BBC Health Coverage Style
health_statistics_data <- data.frame(
  date = rep(seq(as.Date("2020-01-01"), as.Date("2024-05-01"), by = "month"), 2),
  month = rep(1:53, 2),
  year = rep(c(rep(2020, 12), rep(2021, 12), rep(2022, 12), rep(2023, 12), rep(2024, 5)), 2),
  health_system = factor(rep(c("NHS England", "NHS Scotland"), each = 53)),
  
  # Realistic NHS waiting times with COVID impact
  routine_surgery_wait = c(
    # NHS England
    c(15, 16, 18, 35, 42, 38, 35, 33, 30, 28, 26, 25,
      24, 26, 28, 30, 32, 30, 28, 26, 24, 22, 20, 18,
      16, 15, 14, 16, 18, 20, 22, 24, 26, 25, 23, 21,
      19, 18, 17, 19, 21, 23, 25, 24, 22, 20, 18, 16,
      15, 14, 13, 15, 16),
    # NHS Scotland  
    c(12, 13, 14, 28, 35, 32, 28, 25, 22, 20, 18, 17,
      16, 18, 20, 22, 24, 22, 20, 18, 16, 14, 12, 11,
      10, 9, 8, 10, 12, 14, 16, 18, 20, 19, 17, 15,
      13, 12, 11, 13, 15, 17, 19, 18, 16, 14, 12, 10,
      9, 8, 7, 9, 10)
  ),
  
  # Vaccination rollout patterns
  vaccination_rate = c(
    # NHS England
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
      25, 45, 60, 70, 75, 78, 80, 82, 84, 85, 86, 87,
      88, 89, 90, 91, 91, 92, 92, 93, 93, 93, 93, 94,
      94, 94, 95, 95, 95, 95, 96, 96, 96, 96, 96, 96,
      97, 97, 97, 97, 97),
    # NHS Scotland
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8,
      30, 50, 65, 75, 80, 83, 85, 87, 89, 90, 91, 92,
      93, 94, 95, 96, 96, 97, 97, 98, 98, 98, 98, 98,
      98, 98, 98, 98, 99, 99, 99, 99, 99, 99, 99, 99,
      99, 99, 99, 99, 99)
  ),
  
  # COVID hospital admissions
  covid_admissions = c(
    # England  
    c(5, 8, 12, 45, 120, 95, 65, 45, 35, 55, 85, 110,
      95, 75, 55, 35, 25, 20, 15, 12, 18, 25, 35, 55,
      45, 35, 25, 15, 10, 8, 12, 18, 25, 30, 35, 40,
      35, 30, 25, 20, 15, 12, 8, 6, 8, 12, 15, 18,
      15, 12, 8, 6, 5),
    # Scotland
    c(3, 6, 9, 35, 95, 75, 50, 35, 25, 40, 65, 85,
      70, 55, 40, 25, 18, 15, 12, 8, 12, 18, 25, 40,
      35, 25, 18, 12, 8, 6, 9, 12, 18, 22, 25, 30,
      25, 22, 18, 15, 12, 9, 6, 4, 6, 9, 12, 15,
      12, 9, 6, 4, 3)
  )
)

# 4. Sports Performance Dataset - BBC Sports Coverage Style
sports_performance_data <- data.frame(
  team = factor(c("Manchester City", "Arsenal", "Liverpool", "Newcastle", "Manchester United", 
                  "Brighton", "Aston Villa", "Tottenham", "Brentford", "Fulham",
                  "Crystal Palace", "Chelsea", "Wolves", "West Ham", "AFC Bournemouth",
                  "Nottingham Forest", "Everton", "Leicester City", "Leeds United", "Southampton")),
  
  # Premier League 2022-23 season data
  points = c(89, 84, 67, 71, 66, 62, 61, 57, 59, 52, 45, 44, 41, 40, 39, 38, 36, 34, 31, 25),
  goals_scored = c(99, 88, 75, 58, 58, 72, 61, 66, 58, 55, 40, 38, 31, 42, 37, 38, 34, 51, 48, 43),
  goals_conceded = c(31, 43, 28, 33, 43, 53, 51, 40, 46, 53, 49, 47, 58, 58, 71, 68, 57, 68, 78, 67),
  wins = c(28, 26, 19, 22, 19, 18, 18, 17, 15, 15, 11, 11, 11, 11, 11, 9, 8, 9, 7, 7),
  draws = c(5, 6, 10, 5, 9, 8, 7, 6, 14, 7, 12, 11, 8, 7, 6, 11, 12, 7, 10, 4),
  losses = c(5, 6, 9, 11, 10, 12, 13, 15, 9, 16, 15, 16, 19, 20, 21, 18, 18, 22, 21, 27),
  
  possession_percent = round(rnorm(20, 55, 8), 1),
  pass_accuracy = round(rnorm(20, 82, 5), 1),
  position = 1:20,
  season = rep("2022-23", 20),
  league = factor(rep("Premier League", 20))
)

# 5. Climate Environment Dataset - BBC Climate Coverage Style
years <- 1990:2023
n_years <- length(years)

climate_environment_data <- data.frame(
  year = rep(years, 4),
  decade = factor(rep(c(rep("1990s", 10), rep("2000s", 10), rep("2010s", 10), rep("2020s", 4)), 4)),
  region = factor(rep(c("Arctic", "Temperate", "Tropical", "Antarctic"), each = n_years)),
  
  # Temperature anomalies with realistic warming patterns
  temperature_anomaly = c(
    # Arctic - fastest warming
    create_trend(0.5, 3.2, n_years, 0.1),
    # Temperate - moderate warming  
    create_trend(0.2, 1.8, n_years, 0.08),
    # Tropical - smaller warming
    create_trend(0.1, 1.2, n_years, 0.06),
    # Antarctic - variable warming
    create_trend(-0.1, 1.5, n_years, 0.15)
  ),
  
  # Carbon emissions with peak and decline pattern
  carbon_emissions = c(
    # Arctic
    c(create_trend(50, 85, 20, 0.05), create_trend(85, 75, 14, 0.03)),
    # Temperate  
    c(create_trend(800, 1200, 25, 0.02), create_trend(1200, 1100, 9, 0.02)),
    # Tropical
    c(create_trend(200, 450, 30, 0.03), create_trend(450, 420, 4, 0.02)),
    # Antarctic
    rep(c(5, 8, 12, 10), length.out = n_years) + rnorm(n_years, 0, 1)
  ),
  
  # Renewable energy growth  
  renewable_energy_percent = c(
    # Arctic
    create_trend(5, 45, n_years, 0.05),
    # Temperate
    create_trend(8, 35, n_years, 0.04),
    # Tropical - rapid recent growth
    c(create_trend(3, 15, 25, 0.03), create_trend(15, 30, 9, 0.03)),
    # Antarctic
    create_trend(10, 25, n_years, 0.1)
  )
)

# 6. Education Attainment Dataset - BBC Education Coverage Style
education_attainment_data <- data.frame(
  region = factor(rep(c("London", "South East", "South West", "Midlands", "North", "Scotland", "Wales", "N. Ireland"), each = 50)),
  school_type = factor(rep(c("State", "Grammar", "Independent", "Academy"), 100)),
  student_background = factor(sample(c("Free School Meals", "English as Second Language", "Neither", "Both"), 
                                    400, replace = TRUE, prob = c(0.15, 0.12, 0.65, 0.08))),
  
  # GCSE grades with realistic distribution
  gcse_grade = factor(sample(c("9", "8", "7", "6", "5", "4", "3", "2", "1"), 
                           400, replace = TRUE, prob = c(0.08, 0.12, 0.15, 0.18, 0.20, 0.15, 0.08, 0.03, 0.01))),
  a_level_points = round(rnorm(400, 35, 10)),
  
  university_application = factor(sample(c("Applied", "Not Applied"), 400, replace = TRUE, prob = c(0.65, 0.35))),
  university_admission = factor(sample(c("Admitted", "Not Admitted", "Not Applicable"), 
                                      400, replace = TRUE, prob = c(0.45, 0.20, 0.35))),
  
  preferred_subject = factor(sample(c("STEM", "Arts & Humanities", "Social Sciences", "Business", "Medicine", "Other"), 
                                   400, replace = TRUE, prob = c(0.25, 0.20, 0.20, 0.15, 0.08, 0.12))),
  
  literacy_score = round(rnorm(400, 75, 12)),
  numeracy_score = round(rnorm(400, 72, 15)),
  digital_skills_score = round(rnorm(400, 68, 18)),
  
  employment_status = factor(sample(c("Employed", "Further Education", "Apprenticeship", "Unemployed"), 
                                   400, replace = TRUE, prob = c(0.40, 0.35, 0.20, 0.05))),
  year_group = factor(sample(c("Year 11", "Year 12", "Year 13"), 400, replace = TRUE))
)

# 7. Digital Technology Dataset - BBC Technology Coverage Style
# Device ownership by age group
age_groups <- c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
smartphone_by_age <- c(95, 98, 92, 85, 75, 60, 35)
tablet_by_age <- c(60, 70, 75, 70, 65, 55, 40)
laptop_by_age <- c(85, 88, 82, 75, 65, 50, 30)

digital_technology_data <- data.frame(
  age_group = factor(rep(age_groups, each = 30)),
  income_bracket = factor(sample(c("Under £20k", "£20k-£40k", "£40k-£60k", "£60k+"), 
                                210, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15))),
  location = factor(sample(c("Urban", "Suburban", "Rural"), 210, replace = TRUE, prob = c(0.45, 0.35, 0.20))),
  
  # Realistic device ownership patterns
  smartphone_ownership = rep(smartphone_by_age, each = 30),
  tablet_ownership = rep(tablet_by_age, each = 30),
  laptop_ownership = rep(laptop_by_age, each = 30),
  smart_tv_ownership = rep(c(70, 75, 80, 75, 70, 60, 45), each = 30),
  
  # Internet usage patterns
  daily_internet_hours = rep(c(6.5, 5.8, 4.2, 3.1, 2.4, 1.8, 1.2), each = 30) + rnorm(210, 0, 0.5),
  
  # Digital skills by age
  digital_skills_rating = round(rep(c(8.2, 7.8, 6.9, 5.8, 4.5, 3.2, 2.1), each = 30) + rnorm(210, 0, 1), 1),
  
  online_shopping = factor(sample(c("Daily", "Weekly", "Monthly", "Never"), 
                                 210, replace = TRUE, prob = c(0.15, 0.45, 0.30, 0.10))),
  social_media_use = factor(sample(c("Daily", "Weekly", "Monthly", "Never"), 
                                  210, replace = TRUE, prob = c(0.70, 0.20, 0.05, 0.05))),
  
  password_security = factor(sample(c("Strong", "Moderate", "Weak"), 
                                   210, replace = TRUE, prob = c(0.35, 0.45, 0.20))),
  privacy_awareness = round(runif(210, 3, 9), 1)
)

# Export datasets
use_data_multi_format(economic_indicators_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(election_survey_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(health_statistics_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(sports_performance_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(climate_environment_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(education_attainment_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(digital_technology_data, overwrite = TRUE, save_csv = TRUE)

# Generate performance metrics for validation
cat("📊 BBC Visualization Example Datasets Created Successfully!\n\n")

cat("Dataset Summary Statistics:\n")
cat("=========================\n\n")

cat("Economic Indicators Dataset:\n")
cat(sprintf("  - Observations: %d\n", nrow(economic_indicators_data)))
cat(sprintf("  - Regions: %s\n", paste(levels(economic_indicators_data$region), collapse = ", ")))
cat(sprintf("  - GDP Growth Range: %.1f%% to %.1f%%\n", min(economic_indicators_data$gdp_growth), max(economic_indicators_data$gdp_growth)))
cat(sprintf("  - Best for: Column charts, regional comparisons\n\n"))

cat("Election Survey Dataset:\n")
cat(sprintf("  - Respondents: %d\n", nrow(election_survey_data)))
cat(sprintf("  - Age Groups: %d\n", length(levels(election_survey_data$age_group))))
cat(sprintf("  - Parties: %s\n", paste(levels(election_survey_data$party_support), collapse = ", ")))
cat(sprintf("  - Best for: Grouped bar charts, demographic analysis\n\n"))

cat("Health Statistics Dataset:\n")
cat(sprintf("  - Time Points: %d months\n", nrow(health_statistics_data)/2))
cat(sprintf("  - Health Systems: %s\n", paste(levels(health_statistics_data$health_system), collapse = ", ")))
cat(sprintf("  - Vaccination Range: %.0f%% to %.0f%%\n", min(health_statistics_data$vaccination_rate), max(health_statistics_data$vaccination_rate)))
cat(sprintf("  - Best for: Line charts, time series analysis\n\n"))

cat("Sports Performance Dataset:\n")
cat(sprintf("  - Teams: %d\n", nrow(sports_performance_data)))
cat(sprintf("  - Points Range: %d to %d\n", min(sports_performance_data$points), max(sports_performance_data$points)))
cat(sprintf("  - Best for: Point plots, rankings, scatter plots\n\n"))

cat("Climate Environment Dataset:\n")
cat(sprintf("  - Years Covered: %d to %d\n", min(climate_environment_data$year), max(climate_environment_data$year)))
cat(sprintf("  - Regions: %s\n", paste(levels(climate_environment_data$region), collapse = ", ")))
cat(sprintf("  - Temperature Anomaly Range: %.1f°C to %.1f°C\n", min(climate_environment_data$temperature_anomaly), max(climate_environment_data$temperature_anomaly)))
cat(sprintf("  - Best for: Area charts, environmental trends\n\n"))

cat("Education Attainment Dataset:\n")
cat(sprintf("  - Students: %d\n", nrow(education_attainment_data)))
cat(sprintf("  - Regions: %d\n", length(levels(education_attainment_data$region))))
cat(sprintf("  - School Types: %s\n", paste(levels(education_attainment_data$school_type), collapse = ", ")))
cat(sprintf("  - Best for: Stacked charts, educational equity analysis\n\n"))

cat("Digital Technology Dataset:\n")
cat(sprintf("  - Respondents: %d\n", nrow(digital_technology_data)))
cat(sprintf("  - Age Groups: %s\n", paste(levels(digital_technology_data$age_group), collapse = ", ")))
cat(sprintf("  - Internet Usage Range: %.1f to %.1f hours/day\n", min(digital_technology_data$daily_internet_hours), max(digital_technology_data$daily_internet_hours)))
cat(sprintf("  - Best for: Grouped charts, digital divide analysis\n\n"))

cat("🎯 All datasets are optimized for BBC-style visualization!\n")
cat("Use these datasets to demonstrate professional news-quality graphics.\n")
