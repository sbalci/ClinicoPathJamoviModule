#' Example Datasets for BBC-Style Data Visualization
#'
#' These datasets provide realistic examples for demonstrating BBC-style data visualization
#' functionality across different news and reporting scenarios.
#'
#' @name bbcplots_examples
#' @docType data
#' @format Data frames with various structures for different BBC visualization scenarios
#' @author ClinicoPath package team

# Set seed for reproducible data generation
set.seed(20241201)

# 1. Economic News Dataset - For Column/Bar Charts
#' @rdname bbcplots_examples
#' @description \code{economic_indicators_data}: Economic data suitable for BBC news stories,
#' featuring quarterly GDP growth, unemployment rates, and inflation across different regions.
economic_indicators_data <- data.frame(
  # Time periods
  quarter = rep(c("Q1 2023", "Q2 2023", "Q3 2023", "Q4 2023", "Q1 2024"), 4),
  year = rep(c(2023, 2023, 2023, 2023, 2024), 4),
  
  # Geographic regions
  region = factor(rep(c("North", "South", "East", "West"), each = 5)),
  country = factor(rep(c("England", "Scotland", "Wales", "N. Ireland"), each = 5)),
  
  # Economic indicators
  gdp_growth = c(
    # North: Strong growth
    c(2.1, 2.3, 2.0, 1.8, 2.2),
    # South: Moderate growth  
    c(1.8, 1.9, 1.7, 1.6, 1.9),
    # East: Variable growth
    c(1.5, 2.0, 1.3, 1.4, 1.7),
    # West: Lower growth
    c(1.2, 1.4, 1.1, 1.0, 1.3)
  ),
  
  unemployment_rate = c(
    # North: Lower unemployment
    c(3.2, 3.1, 3.0, 3.3, 3.1),
    # South: Moderate unemployment
    c(4.1, 4.0, 4.2, 4.3, 4.1),
    # East: Higher unemployment
    c(5.2, 5.1, 5.4, 5.3, 5.0),
    # West: Variable unemployment
    c(4.8, 4.6, 4.9, 5.1, 4.7)
  ),
  
  inflation_rate = c(
    # Realistic inflation patterns
    c(2.1, 2.3, 2.0, 1.8, 1.9),
    c(2.0, 2.2, 1.9, 1.7, 1.8),
    c(2.2, 2.4, 2.1, 1.9, 2.0),
    c(2.3, 2.5, 2.2, 2.0, 2.1)
  ),
  
  # Additional indicators
  business_confidence = round(runif(20, 45, 75), 1),
  house_prices_change = round(rnorm(20, 5.2, 2.1), 1)
)

# 2. Election Survey Data - For Grouped Charts
#' @rdname bbcplots_examples  
#' @description \code{election_survey_data}: Political polling data across different demographics,
#' suitable for BBC election coverage with party support by age group and region.
election_survey_data <- data.frame(
  # Respondent characteristics
  age_group = factor(rep(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), each = 50)),
  gender = factor(rep(c("Male", "Female"), 150)),
  education = factor(sample(c("No qualifications", "GCSE", "A-Level", "University"), 
                           300, replace = TRUE, prob = c(0.15, 0.25, 0.25, 0.35))),
  region = factor(sample(c("London", "South East", "South West", "Midlands", "North", "Scotland"), 
                        300, replace = TRUE)),
  
  # Party support (realistic UK polling)
  party_support = factor(sample(c("Conservative", "Labour", "Liberal Democrat", "Green", "Reform", "Other"), 
                               300, replace = TRUE, prob = c(0.22, 0.42, 0.12, 0.08, 0.10, 0.06))),
  
  # Voting likelihood and issues
  voting_likelihood = sample(1:10, 300, replace = TRUE, prob = c(0.05, 0.05, 0.05, 0.08, 0.10, 0.12, 0.15, 0.15, 0.15, 0.10)),
  top_issue = factor(sample(c("Economy", "Healthcare", "Immigration", "Climate", "Education", "Housing"), 
                           300, replace = TRUE, prob = c(0.25, 0.20, 0.15, 0.15, 0.12, 0.13))),
  
  # Approval ratings
  pm_approval = sample(1:5, 300, replace = TRUE, prob = c(0.20, 0.25, 0.25, 0.20, 0.10)),
  government_approval = sample(1:5, 300, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.15, 0.05)),
  
  # Survey metadata
  poll_date = sample(seq(as.Date("2024-01-01"), as.Date("2024-06-01"), by = "day"), 300, replace = TRUE),
  sample_weight = runif(300, 0.8, 1.2)
)

# 3. Health News Data - For Line Charts and Time Series
#' @rdname bbcplots_examples
#' @description \code{health_statistics_data}: Health indicators over time suitable for BBC health coverage,
#' including hospital waiting times, vaccination rates, and disease surveillance.
health_statistics_data <- data.frame(
  # Time series
  date = rep(seq(as.Date("2020-01-01"), as.Date("2024-05-01"), by = "month"), 2),
  month = rep(1:53, 2),
  year = rep(c(rep(2020, 12), rep(2021, 12), rep(2022, 12), rep(2023, 12), rep(2024, 5)), 2),
  
  # Health systems
  health_system = factor(rep(c("NHS England", "NHS Scotland"), each = 53)),
  
  # Waiting times (in weeks)
  routine_surgery_wait = c(
    # NHS England - longer waits, COVID impact
    c(15, 16, 18, 35, 42, 38, 35, 33, 30, 28, 26, 25,  # 2020
      24, 26, 28, 30, 32, 30, 28, 26, 24, 22, 20, 18,  # 2021
      16, 15, 14, 16, 18, 20, 22, 24, 26, 25, 23, 21,  # 2022
      19, 18, 17, 19, 21, 23, 25, 24, 22, 20, 18, 16,  # 2023
      15, 14, 13, 15, 16),                              # 2024
    # NHS Scotland - shorter waits, smaller COVID impact
    c(12, 13, 14, 28, 35, 32, 28, 25, 22, 20, 18, 17,  # 2020
      16, 18, 20, 22, 24, 22, 20, 18, 16, 14, 12, 11,  # 2021
      10, 9, 8, 10, 12, 14, 16, 18, 20, 19, 17, 15,    # 2022
      13, 12, 11, 13, 15, 17, 19, 18, 16, 14, 12, 10,  # 2023
      9, 8, 7, 9, 10)                                   # 2024
  ),
  
  # Vaccination rates (percentage of eligible population)
  vaccination_rate = c(
    # NHS England vaccination rates
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,      # 2020
      25, 45, 60, 70, 75, 78, 80, 82, 84, 85, 86, 87, # 2021
      88, 89, 90, 91, 91, 92, 92, 93, 93, 93, 93, 94, # 2022
      94, 94, 95, 95, 95, 95, 96, 96, 96, 96, 96, 96, # 2023
      97, 97, 97, 97, 97),                             # 2024
    # NHS Scotland vaccination rates (slightly higher)
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8,      # 2020
      30, 50, 65, 75, 80, 83, 85, 87, 89, 90, 91, 92, # 2021
      93, 94, 95, 96, 96, 97, 97, 98, 98, 98, 98, 98, # 2022
      98, 98, 98, 98, 99, 99, 99, 99, 99, 99, 99, 99, # 2023
      99, 99, 99, 99, 99)                              # 2024
  ),
  
  # Hospital admissions per 100k population
  covid_admissions = c(
    # England - higher population density impacts
    c(5, 8, 12, 45, 120, 95, 65, 45, 35, 55, 85, 110,     # 2020
      95, 75, 55, 35, 25, 20, 15, 12, 18, 25, 35, 55,     # 2021
      45, 35, 25, 15, 10, 8, 12, 18, 25, 30, 35, 40,      # 2022
      35, 30, 25, 20, 15, 12, 8, 6, 8, 12, 15, 18,        # 2023
      15, 12, 8, 6, 5),                                    # 2024
    # Scotland - similar pattern, slightly lower peaks
    c(3, 6, 9, 35, 95, 75, 50, 35, 25, 40, 65, 85,        # 2020
      70, 55, 40, 25, 18, 15, 12, 8, 12, 18, 25, 40,      # 2021
      35, 25, 18, 12, 8, 6, 9, 12, 18, 22, 25, 30,        # 2022
      25, 22, 18, 15, 12, 9, 6, 4, 6, 9, 12, 15,          # 2023
      12, 9, 6, 4, 3)                                      # 2024
  )
)

# 4. Sports News Data - For Point Plots and Rankings
#' @rdname bbcplots_examples
#' @description \code{sports_performance_data}: Sports statistics suitable for BBC sports coverage,
#' including Premier League performance, Olympic medals, and tennis rankings.
sports_performance_data <- data.frame(
  # Football/Soccer data
  team = factor(c("Manchester City", "Arsenal", "Liverpool", "Newcastle", "Manchester United", 
                  "Brighton", "Aston Villa", "Tottenham", "Brentford", "Fulham",
                  "Crystal Palace", "Chelsea", "Wolves", "West Ham", "AFC Bournemouth",
                  "Nottingham Forest", "Everton", "Leicester City", "Leeds United", "Southampton")),
  
  # League performance
  points = c(89, 84, 67, 71, 66, 62, 61, 57, 59, 52, 45, 44, 41, 40, 39, 38, 36, 34, 31, 25),
  goals_scored = c(99, 88, 75, 58, 58, 72, 61, 66, 58, 55, 40, 38, 31, 42, 37, 38, 34, 51, 48, 43),
  goals_conceded = c(31, 43, 28, 33, 43, 53, 51, 40, 46, 53, 49, 47, 58, 58, 71, 68, 57, 68, 78, 67),
  wins = c(28, 26, 19, 22, 19, 18, 18, 17, 15, 15, 11, 11, 11, 11, 11, 9, 8, 9, 7, 7),
  draws = c(5, 6, 10, 5, 9, 8, 7, 6, 14, 7, 12, 11, 8, 7, 6, 11, 12, 7, 10, 4),
  losses = c(5, 6, 9, 11, 10, 12, 13, 15, 9, 16, 15, 16, 19, 20, 21, 18, 18, 22, 21, 27),
  
  # Additional metrics
  possession_percent = round(rnorm(20, 55, 8), 1),
  pass_accuracy = round(rnorm(20, 82, 5), 1),
  
  # League position
  position = 1:20,
  
  # Season and competition
  season = rep("2022-23", 20),
  league = factor(rep("Premier League", 20))
)

# 5. Climate and Environment Data - For Area Charts
#' @rdname bbcplots_examples
#' @description \code{climate_environment_data}: Environmental data suitable for BBC climate coverage,
#' including temperature anomalies, carbon emissions, and renewable energy adoption.
climate_environment_data <- data.frame(
  # Time periods
  year = rep(1990:2023, 4),
  decade = factor(rep(c("1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s",
                       "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s",
                       "2010s", "2010s", "2010s", "2010s", "2010s", "2010s", "2010s", "2010s", "2010s", "2010s",
                       "2020s", "2020s", "2020s", "2020s"), 4)),
  
  # Geographic regions
  region = factor(rep(c("Arctic", "Temperate", "Tropical", "Antarctic"), each = 34)),
  
  # Temperature anomalies (degrees C above 1961-1990 average)
  temperature_anomaly = c(
    # Arctic - largest warming
    seq(0.5, 3.2, length.out = 34) + rnorm(34, 0, 0.2),
    # Temperate - moderate warming
    seq(0.2, 1.8, length.out = 34) + rnorm(34, 0, 0.15),
    # Tropical - smaller warming
    seq(0.1, 1.2, length.out = 34) + rnorm(34, 0, 0.1),
    # Antarctic - variable warming
    seq(-0.1, 1.5, length.out = 34) + rnorm(34, 0, 0.25)
  ),
  
  # Carbon emissions (million tonnes CO2)
  carbon_emissions = c(
    # Arctic region emissions
    c(seq(50, 85, length.out = 20), seq(85, 75, length.out = 14)) + rnorm(34, 0, 3),
    # Temperate region emissions
    c(seq(800, 1200, length.out = 25), seq(1200, 1100, length.out = 9)) + rnorm(34, 0, 30),
    # Tropical region emissions
    c(seq(200, 450, length.out = 30), seq(450, 420, length.out = 4)) + rnorm(34, 0, 15),
    # Antarctic region emissions
    rep(c(5, 8, 12, 10), length.out = 34) + rnorm(34, 0, 1)
  ),
  
  # Renewable energy percentage
  renewable_energy_percent = c(
    # Arctic - growing renewables
    seq(5, 45, length.out = 34) + rnorm(34, 0, 2),
    # Temperate - steady growth
    seq(8, 35, length.out = 34) + rnorm(34, 0, 1.5),
    # Tropical - rapid recent growth
    c(seq(3, 15, length.out = 25), seq(15, 30, length.out = 9)) + rnorm(34, 0, 1),
    # Antarctic - limited data
    seq(10, 25, length.out = 34) + rnorm(34, 0, 3)
  )
)

# 6. Education News Data - For Stacked and Grouped Charts
#' @rdname bbcplots_examples
#' @description \code{education_attainment_data}: Educational achievement data suitable for BBC education coverage,
#' including GCSE results, university admission rates, and skills assessment by region and background.
education_attainment_data <- data.frame(
  # Student characteristics
  region = factor(rep(c("London", "South East", "South West", "Midlands", "North", "Scotland", "Wales", "N. Ireland"), each = 50)),
  school_type = factor(rep(c("State", "Grammar", "Independent", "Academy"), 100)),
  student_background = factor(sample(c("Free School Meals", "English as Second Language", "Neither", "Both"), 
                                    400, replace = TRUE, prob = c(0.15, 0.12, 0.65, 0.08))),
  
  # Academic achievement
  gcse_grade = factor(sample(c("9", "8", "7", "6", "5", "4", "3", "2", "1"), 
                           400, replace = TRUE, prob = c(0.08, 0.12, 0.15, 0.18, 0.20, 0.15, 0.08, 0.03, 0.01))),
  a_level_points = round(rnorm(400, 35, 10)),  # UCAS points
  
  # University outcomes
  university_application = factor(sample(c("Applied", "Not Applied"), 400, replace = TRUE, prob = c(0.65, 0.35))),
  university_admission = factor(sample(c("Admitted", "Not Admitted", "Not Applicable"), 
                                      400, replace = TRUE, prob = c(0.45, 0.20, 0.35))),
  
  # Subject areas
  preferred_subject = factor(sample(c("STEM", "Arts & Humanities", "Social Sciences", "Business", "Medicine", "Other"), 
                                   400, replace = TRUE, prob = c(0.25, 0.20, 0.20, 0.15, 0.08, 0.12))),
  
  # Skills assessment scores (out of 100)
  literacy_score = round(rnorm(400, 75, 12)),
  numeracy_score = round(rnorm(400, 72, 15)),
  digital_skills_score = round(rnorm(400, 68, 18)),
  
  # Progression indicators
  employment_status = factor(sample(c("Employed", "Further Education", "Apprenticeship", "Unemployed"), 
                                   400, replace = TRUE, prob = c(0.40, 0.35, 0.20, 0.05))),
  year_group = factor(sample(c("Year 11", "Year 12", "Year 13"), 400, replace = TRUE))
)

# 7. Technology and Digital News Data
#' @rdname bbcplots_examples
#' @description \code{digital_technology_data}: Technology adoption and digital trends suitable for BBC technology coverage,
#' including internet usage, device ownership, and digital skills by demographics.
digital_technology_data <- data.frame(
  # Demographics
  age_group = factor(rep(c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"), each = 30)),
  income_bracket = factor(sample(c("Under £20k", "£20k-£40k", "£40k-£60k", "£60k+"), 
                                210, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15))),
  location = factor(sample(c("Urban", "Suburban", "Rural"), 210, replace = TRUE, prob = c(0.45, 0.35, 0.20))),
  
  # Device ownership (percentage)
  smartphone_ownership = c(95, 98, 92, 85, 75, 60, 35),  # By age group
  tablet_ownership = c(60, 70, 75, 70, 65, 55, 40),
  laptop_ownership = c(85, 88, 82, 75, 65, 50, 30),
  smart_tv_ownership = c(70, 75, 80, 75, 70, 60, 45),
  
  # Internet usage (hours per day)
  daily_internet_hours = c(
    rep(c(6.5, 5.8, 4.2, 3.1, 2.4, 1.8, 1.2), each = 30) + rnorm(210, 0, 0.5)
  ),
  
  # Digital skills (self-reported, 1-10 scale)
  digital_skills_rating = round(c(
    rep(c(8.2, 7.8, 6.9, 5.8, 4.5, 3.2, 2.1), each = 30) + rnorm(210, 0, 1)
  ), 1),
  
  # Online activities (frequency: daily, weekly, monthly, never)
  online_shopping = factor(sample(c("Daily", "Weekly", "Monthly", "Never"), 
                                 210, replace = TRUE, prob = c(0.15, 0.45, 0.30, 0.10))),
  social_media_use = factor(sample(c("Daily", "Weekly", "Monthly", "Never"), 
                                  210, replace = TRUE, prob = c(0.70, 0.20, 0.05, 0.05))),
  
  # Security awareness
  password_security = factor(sample(c("Strong", "Moderate", "Weak"), 
                                   210, replace = TRUE, prob = c(0.35, 0.45, 0.20))),
  privacy_awareness = round(runif(210, 3, 9), 1)  # 1-10 scale
)

# Create the data-raw script for generating these datasets
usethis::use_data(economic_indicators_data, overwrite = TRUE)
usethis::use_data(election_survey_data, overwrite = TRUE)
usethis::use_data(health_statistics_data, overwrite = TRUE)
usethis::use_data(sports_performance_data, overwrite = TRUE)
usethis::use_data(climate_environment_data, overwrite = TRUE)
usethis::use_data(education_attainment_data, overwrite = TRUE)
usethis::use_data(digital_technology_data, overwrite = TRUE)

# Print summary of created datasets
cat("✅ Created 7 example datasets for BBC-style visualization:\n\n")

cat("📈 economic_indicators_data (n=20):\n")
cat("   - GDP growth, unemployment, inflation by region and quarter\n")
cat("   - Perfect for column charts showing economic comparisons\n")
cat("   - Demonstrates BBC's signature economic reporting style\n\n")

cat("🗳️ election_survey_data (n=300):\n")
cat("   - Political polling across demographics and regions\n")
cat("   - Ideal for grouped bar charts and stacked visualizations\n") 
cat("   - Shows party support breakdown typical of BBC election coverage\n\n")

cat("🏥 health_statistics_data (n=106):\n")
cat("   - NHS waiting times, vaccination rates, hospital admissions over time\n")
cat("   - Perfect for line charts showing health trends\n")
cat("   - Demonstrates time series visualization BBC health reporting\n\n")

cat("⚽ sports_performance_data (n=20):\n")
cat("   - Premier League team statistics and rankings\n")
cat("   - Ideal for point plots and ranking visualizations\n")
cat("   - Shows BBC's sports data presentation style\n\n")

cat("🌍 climate_environment_data (n=136):\n")
cat("   - Temperature anomalies, emissions, renewable energy by region\n")
cat("   - Perfect for area charts showing environmental trends\n")
cat("   - Demonstrates BBC's climate coverage visualization approach\n\n")

cat("🎓 education_attainment_data (n=400):\n")
cat("   - GCSE grades, university admissions, skills by background\n")
cat("   - Ideal for stacked charts showing educational equity\n")
cat("   - Shows BBC's education reporting data presentation\n\n")

cat("💻 digital_technology_data (n=210):\n")
cat("   - Device ownership, internet usage, digital skills by age\n")
cat("   - Perfect for grouped charts showing digital divide\n")
cat("   - Demonstrates BBC's technology coverage style\n\n")

cat("💡 Usage examples:\n")
cat("   bbcplots(data = economic_indicators_data, y_var = 'gdp_growth', x_var = 'region')\n")
cat("   bbcplots(data = health_statistics_data, y_var = 'vaccination_rate', x_var = 'date',\n")
cat("            chart_type = 'line', group_var = 'health_system')\n")
cat("   bbcplots(data = election_survey_data, y_var = 'voting_likelihood', x_var = 'age_group',\n")
cat("            chart_type = 'grouped_column', group_var = 'party_support')\n")