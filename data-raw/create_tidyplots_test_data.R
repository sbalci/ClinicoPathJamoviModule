# Create comprehensive test datasets for tidyplots function
# This script generates various datasets that showcase different tidyplots features

library(dplyr)
library(tidyr)

set.seed(42)

# ============================================================================
# 1. MEDICAL RESEARCH DATASET
# ============================================================================

# Clinical trial data for biomarker analysis
tidyplots_medical <- expand.grid(
  patient_id = 1:120,
  treatment = c("Control", "Drug_A", "Drug_B"),
  gender = c("Male", "Female"),
  age_group = c("Young", "Middle", "Elderly")
) %>%
  rowwise() %>%
  mutate(
    # Baseline characteristics
    age = case_when(
      age_group == "Young" ~ runif(1, 18, 35),
      age_group == "Middle" ~ runif(1, 36, 55),
      age_group == "Elderly" ~ runif(1, 56, 75)
    ),
    
    # Treatment effects with realistic variability
    treatment_effect = case_when(
      treatment == "Control" ~ 0,
      treatment == "Drug_A" ~ runif(1, 2, 5),
      treatment == "Drug_B" ~ runif(1, 4, 8)
    ),
    
    # Gender effects
    gender_effect = ifelse(gender == "Female", runif(1, 1, 2), 0),
    
    # Age effects
    age_effect = (age - 45) * 0.1,
    
    # Primary outcome (blood pressure reduction)
    bp_reduction = 10 + treatment_effect + gender_effect + age_effect + rnorm(1, 0, 3),
    bp_reduction = pmax(0, bp_reduction), # Ensure non-negative
    
    # Secondary outcomes
    biomarker_1 = 50 + treatment_effect * 2 + rnorm(1, 0, 8),
    biomarker_2 = 100 + treatment_effect * 1.5 + gender_effect * 2 + rnorm(1, 0, 12),
    
    # Quality of life score (0-100)
    qol_score = 60 + treatment_effect * 3 + rnorm(1, 0, 10),
    qol_score = pmin(100, pmax(0, qol_score)),
    
    # Time points
    timepoint = sample(c("Baseline", "Week_4", "Week_8", "Week_12"), 1),
    
    # Hospital site
    hospital = sample(c("Hospital_A", "Hospital_B", "Hospital_C"), 1, 
                     prob = c(0.4, 0.35, 0.25)),
    
    # Response categories
    response = case_when(
      bp_reduction >= 15 ~ "High_Response",
      bp_reduction >= 8 ~ "Moderate_Response",
      TRUE ~ "Low_Response"
    )
  ) %>%
  ungroup() %>%
  select(-treatment_effect, -gender_effect, -age_effect)

# ============================================================================
# 2. EDUCATIONAL RESEARCH DATASET  
# ============================================================================

# Student performance across different teaching methods
tidyplots_education <- expand.grid(
  student_id = 1:200,
  teaching_method = c("Traditional", "Interactive", "Online", "Hybrid"),
  subject = c("Math", "Science", "Literature"),
  school_type = c("Public", "Private")
) %>%
  rowwise() %>%
  mutate(
    # Student characteristics
    grade_level = sample(9:12, 1),
    
    # Baseline ability
    baseline_ability = rnorm(1, 75, 15),
    
    # Method effects
    method_effect = case_when(
      teaching_method == "Traditional" ~ 0,
      teaching_method == "Interactive" ~ runif(1, 3, 8),
      teaching_method == "Online" ~ runif(1, -2, 4),
      teaching_method == "Hybrid" ~ runif(1, 5, 10)
    ),
    
    # Subject difficulty modifiers
    subject_modifier = case_when(
      subject == "Math" ~ runif(1, -5, 0),
      subject == "Science" ~ runif(1, -3, 2),
      subject == "Literature" ~ runif(1, 0, 3)
    ),
    
    # School type effect
    school_effect = ifelse(school_type == "Private", runif(1, 2, 6), 0),
    
    # Final scores
    pre_test_score = pmin(100, pmax(0, baseline_ability + rnorm(1, 0, 8))),
    post_test_score = pmin(100, pmax(0, baseline_ability + method_effect + 
                                   subject_modifier + school_effect + rnorm(1, 0, 10))),
    
    # Improvement score
    improvement = post_test_score - pre_test_score,
    
    # Engagement metrics
    engagement_score = 50 + method_effect * 2 + rnorm(1, 0, 12),
    engagement_score = pmin(100, pmax(0, engagement_score)),
    
    # Time spent studying (hours per week)
    study_hours = 5 + method_effect * 0.5 + rnorm(1, 0, 2),
    study_hours = pmax(0, study_hours),
    
    # Performance categories
    performance_level = case_when(
      post_test_score >= 90 ~ "Excellent",
      post_test_score >= 80 ~ "Good", 
      post_test_score >= 70 ~ "Satisfactory",
      TRUE ~ "Needs_Improvement"
    )
  ) %>%
  ungroup() %>%
  select(-baseline_ability, -method_effect, -subject_modifier, -school_effect)

# ============================================================================
# 3. BUSINESS ANALYTICS DATASET
# ============================================================================

# Sales performance across different marketing strategies
tidyplots_business <- expand.grid(
  salesperson_id = 1:150,
  marketing_strategy = c("Email", "Social_Media", "Direct_Mail", "Phone", "Mixed"),
  product_category = c("Electronics", "Clothing", "Home_Garden", "Sports"),
  region = c("North", "South", "East", "West"),
  quarter = c("Q1", "Q2", "Q3", "Q4")
) %>%
  rowwise() %>%
  mutate(
    # Experience level
    experience_years = sample(1:15, 1),
    
    # Strategy effectiveness
    strategy_mult = case_when(
      marketing_strategy == "Email" ~ runif(1, 0.8, 1.2),
      marketing_strategy == "Social_Media" ~ runif(1, 1.1, 1.5),
      marketing_strategy == "Direct_Mail" ~ runif(1, 0.7, 1.0),
      marketing_strategy == "Phone" ~ runif(1, 1.0, 1.3),
      marketing_strategy == "Mixed" ~ runif(1, 1.2, 1.6)
    ),
    
    # Seasonal effects
    seasonal_mult = case_when(
      quarter == "Q1" ~ runif(1, 0.8, 1.0),
      quarter == "Q2" ~ runif(1, 0.9, 1.1),
      quarter == "Q3" ~ runif(1, 0.7, 0.9),
      quarter == "Q4" ~ runif(1, 1.2, 1.5)
    ),
    
    # Regional effects
    regional_mult = case_when(
      region == "North" ~ runif(1, 0.9, 1.1),
      region == "South" ~ runif(1, 1.0, 1.2),
      region == "East" ~ runif(1, 1.1, 1.3),
      region == "West" ~ runif(1, 0.8, 1.0)
    ),
    
    # Base sales influenced by experience
    base_sales = 50000 + experience_years * 3000 + rnorm(1, 0, 10000),
    
    # Actual sales with all multipliers
    sales_amount = base_sales * strategy_mult * seasonal_mult * regional_mult,
    sales_amount = pmax(0, sales_amount),
    
    # Number of leads generated
    leads_generated = rpois(1, lambda = 20 + experience_years * 2),
    
    # Conversion rate (%)
    conversion_rate = 10 + experience_years * 0.5 + 
                     (strategy_mult - 1) * 20 + rnorm(1, 0, 3),
    conversion_rate = pmin(100, pmax(0, conversion_rate)),
    
    # Customer satisfaction (1-10)
    customer_satisfaction = 6 + experience_years * 0.1 + 
                           (strategy_mult - 1) * 2 + rnorm(1, 0, 1),
    customer_satisfaction = pmin(10, pmax(1, customer_satisfaction)),
    
    # Performance tier
    performance_tier = case_when(
      sales_amount >= quantile(sales_amount, 0.8, na.rm = TRUE) ~ "Top_Performer",
      sales_amount >= quantile(sales_amount, 0.6, na.rm = TRUE) ~ "High_Performer",
      sales_amount >= quantile(sales_amount, 0.4, na.rm = TRUE) ~ "Average_Performer",
      TRUE ~ "Needs_Improvement"
    )
  ) %>%
  ungroup() %>%
  select(-strategy_mult, -seasonal_mult, -regional_mult, -base_sales)

# ============================================================================
# 4. ENVIRONMENTAL MONITORING DATASET
# ============================================================================

# Environmental measurements across different monitoring stations
tidyplots_environmental <- expand.grid(
  station_id = paste0("Station_", LETTERS[1:12]),
  month = month.name,
  measurement_type = c("Temperature", "Humidity", "Air_Quality", "Noise_Level"),
  location_type = c("Urban", "Suburban", "Rural")
) %>%
  rowwise() %>%
  mutate(
    # Month number for seasonal calculations
    month_num = match(month, month.name),
    
    # Seasonal patterns
    seasonal_temp_effect = 20 * sin(2 * pi * (month_num - 1) / 12),
    seasonal_humidity_effect = 15 * sin(2 * pi * (month_num + 3) / 12),
    
    # Location effects
    location_temp_offset = case_when(
      location_type == "Urban" ~ runif(1, 2, 5),
      location_type == "Suburban" ~ runif(1, 0, 2),
      location_type == "Rural" ~ runif(1, -2, 1)
    ),
    
    location_pollution_mult = case_when(
      location_type == "Urban" ~ runif(1, 1.5, 2.5),
      location_type == "Suburban" ~ runif(1, 1.0, 1.5),
      location_type == "Rural" ~ runif(1, 0.3, 0.8)
    ),
    
    # Generate measurements based on type
    value = case_when(
      measurement_type == "Temperature" ~ 
        15 + seasonal_temp_effect + location_temp_offset + rnorm(1, 0, 3),
      measurement_type == "Humidity" ~ 
        50 + seasonal_humidity_effect + rnorm(1, 0, 8),
      measurement_type == "Air_Quality" ~ 
        50 * location_pollution_mult + rnorm(1, 0, 10),
      measurement_type == "Noise_Level" ~ 
        40 + 20 * (location_pollution_mult - 0.5) + rnorm(1, 0, 5)
    ),
    
    # Ensure realistic ranges
    value = case_when(
      measurement_type == "Temperature" ~ pmax(-10, pmin(40, value)),
      measurement_type == "Humidity" ~ pmax(0, pmin(100, value)),
      measurement_type == "Air_Quality" ~ pmax(0, value),
      measurement_type == "Noise_Level" ~ pmax(20, pmin(100, value))
    ),
    
    # Data quality indicators
    data_quality = sample(c("High", "Medium", "Low"), 1, prob = c(0.7, 0.2, 0.1)),
    
    # Alert levels
    alert_level = case_when(
      measurement_type == "Air_Quality" & value > 80 ~ "Critical",
      measurement_type == "Air_Quality" & value > 60 ~ "Warning",
      measurement_type == "Noise_Level" & value > 70 ~ "Warning",
      TRUE ~ "Normal"
    ),
    
    # Season grouping
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring", 
      month_num %in% c(6, 7, 8) ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  ungroup() %>%
  select(-month_num, -seasonal_temp_effect, -seasonal_humidity_effect, 
         -location_temp_offset, -location_pollution_mult)

# ============================================================================
# 5. SIMPLE DEMO DATASET
# ============================================================================

# Simple dataset for basic demos and tutorials
tidyplots_demo <- tibble(
  group = rep(c("Group_A", "Group_B", "Group_C", "Group_D"), each = 25),
  category = rep(c("Category_1", "Category_2"), 50),
  treatment = rep(c("Control", "Treatment"), 50),
  time_point = sample(c("Baseline", "Follow_up"), 100, replace = TRUE)
) %>%
  rowwise() %>%
  mutate(
    # Main outcome variable
    score = case_when(
      group == "Group_A" ~ rnorm(1, 75, 10),
      group == "Group_B" ~ rnorm(1, 80, 12),
      group == "Group_C" ~ rnorm(1, 70, 8),
      group == "Group_D" ~ rnorm(1, 85, 15)
    ),
    
    # Treatment effect
    score = ifelse(treatment == "Treatment", score + runif(1, 5, 15), score),
    
    # Time effect
    score = ifelse(time_point == "Follow_up", score + runif(1, 2, 8), score),
    
    # Additional variables for different plot types
    measurement_1 = score + rnorm(1, 0, 5),
    measurement_2 = score * 1.2 + rnorm(1, 0, 8),
    
    # Binary outcome
    success = score > median(score),
    
    # Count variable
    count_var = rpois(1, lambda = 5)
  ) %>%
  ungroup()

# ============================================================================
# SAVE ALL DATASETS
# ============================================================================

# Save individual datasets
save(tidyplots_medical, file = "data/tidyplots_medical.rda", compress = "xz")
save(tidyplots_education, file = "data/tidyplots_education.rda", compress = "xz")
save(tidyplots_business, file = "data/tidyplots_business.rda", compress = "xz")
save(tidyplots_environmental, file = "data/tidyplots_environmental.rda", compress = "xz")
save(tidyplots_demo, file = "data/tidyplots_demo.rda", compress = "xz")

# Create comprehensive list for examples
tidyplots_datasets <- list(
  medical = tidyplots_medical,
  education = tidyplots_education,
  business = tidyplots_business,
  environmental = tidyplots_environmental,
  demo = tidyplots_demo
)

save(tidyplots_datasets, file = "data/tidyplots_datasets.rda", compress = "xz")

# Print summary information
cat("Created comprehensive tidyplots test datasets:\n")
cat("1. tidyplots_medical:", nrow(tidyplots_medical), "rows\n")
cat("2. tidyplots_education:", nrow(tidyplots_education), "rows\n")
cat("3. tidyplots_business:", nrow(tidyplots_business), "rows\n")
cat("4. tidyplots_environmental:", nrow(tidyplots_environmental), "rows\n")
cat("5. tidyplots_demo:", nrow(tidyplots_demo), "rows\n")
cat("\nTotal datasets: 5\n")
cat("Combined dataset list: tidyplots_datasets\n")