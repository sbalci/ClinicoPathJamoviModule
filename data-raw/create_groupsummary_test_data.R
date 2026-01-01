# Create comprehensive test datasets for groupsummary function
# This script generates datasets specifically designed for testing groupsummary functionality

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(789)

# Load existing medical research data and convert to RDA
if (file.exists("data/medical_research_data.csv")) {
  medical_research_data <- read.csv("data/medical_research_data.csv", stringsAsFactors = FALSE)
  
  # Convert date columns
  medical_research_data$VisitDate <- as.Date(medical_research_data$VisitDate)
  
  # Convert factor columns
  factor_cols <- c("StudyCenter", "AgeGroup", "Gender", "TreatmentGroup", 
                   "DiagnosisPrimary", "DiseaseStage", "BMICategory")
  for (col in factor_cols) {
    if (col %in% names(medical_research_data)) {
      medical_research_data[[col]] <- as.factor(medical_research_data[[col]])
    }
  }
  
  # Order factor levels appropriately
  medical_research_data$AgeGroup <- factor(medical_research_data$AgeGroup, 
                                          levels = c("18-30", "31-45", "46-60", "61-75", ">75"), 
                                          ordered = TRUE)
  medical_research_data$DiseaseStage <- factor(medical_research_data$DiseaseStage,
                                              levels = c("Stage I", "Stage II", "Stage III", "Stage IV"),
                                              ordered = TRUE)
  medical_research_data$BMICategory <- factor(medical_research_data$BMICategory,
                                             levels = c("Underweight", "Normal", "Overweight", "Obese"),
                                             ordered = TRUE)
  
  save(medical_research_data, file = "data/medical_research_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(medical_research_data, "data/medical_research_data.omv")
  message("✓ Created medical_research_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(medical_research_data, "data/medical_research_data.omv")
  message("✓ Created medical_research_data.omv")
}
}

# Load existing hourly admission data and convert to RDA
if (file.exists("data/hospital_admission_hourly.csv")) {
  hospital_admission_hourly <- read.csv("data/hospital_admission_hourly.csv", stringsAsFactors = FALSE)
  
  # Convert date/time columns
  hospital_admission_hourly$AdmissionDate <- as.Date(hospital_admission_hourly$AdmissionDate)
  hospital_admission_hourly$AdmissionTime <- as.POSIXct(hospital_admission_hourly$AdmissionTime)
  
  # Convert factor columns
  hospital_admission_hourly$Department <- as.factor(hospital_admission_hourly$Department)
  
  save(hospital_admission_hourly, file = "data/hospital_admission_hourly.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(hospital_admission_hourly, "data/hospital_admission_hourly.omv")
  message("✓ Created hospital_admission_hourly.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(hospital_admission_hourly, "data/hospital_admission_hourly.omv")
  message("✓ Created hospital_admission_hourly.omv")
}
}

# 1. Simple groupsummary dataset for basic testing
create_groupsummary_simple <- function() {
  n <- 100
  
  data.frame(
    category = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    group = factor(sample(c("Group1", "Group2"), n, replace = TRUE)),
    value1 = round(rnorm(n, 100, 20), 1),
    value2 = round(rexp(n, rate = 1/50), 1),
    count_var = sample(1:10, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# 2. Sales data for business analytics testing
create_groupsummary_sales_data <- function() {
  n <- 300
  
  # Generate dates over 2 years
  start_date <- as.Date("2022-01-01")
  dates <- seq(start_date, start_date + 730, by = "day")
  
  data.frame(
    sale_date = sample(dates, n, replace = TRUE),
    region = factor(sample(c("North", "South", "East", "West"), n, replace = TRUE)),
    product_category = factor(sample(c("Electronics", "Clothing", "Home & Garden", "Sports"), 
                                    n, replace = TRUE)),
    sales_channel = factor(sample(c("Online", "Retail Store", "Phone"), n, replace = TRUE,
                                 prob = c(0.6, 0.3, 0.1))),
    customer_type = factor(sample(c("New", "Returning", "VIP"), n, replace = TRUE,
                                 prob = c(0.3, 0.6, 0.1))),
    
    # Numeric variables to summarize
    sales_amount = round(rgamma(n, shape = 2, scale = 150), 2),
    quantity_sold = sample(1:20, n, replace = TRUE),
    profit_margin = round(runif(n, 0.1, 0.4), 3),
    discount_applied = round(runif(n, 0, 0.3), 3),
    customer_satisfaction = sample(1:5, n, replace = TRUE),
    
    stringsAsFactors = FALSE
  )
}

# 3. Survey data for social science research
create_groupsummary_survey_data <- function() {
  n <- 250
  
  data.frame(
    respondent_id = paste0("R", sprintf("%03d", 1:n)),
    survey_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"), 
                        n, replace = TRUE),
    
    # Demographics
    age_group = factor(sample(c("18-25", "26-35", "36-50", "51-65", "65+"), n, replace = TRUE,
                             prob = c(0.2, 0.25, 0.25, 0.2, 0.1)),
                      levels = c("18-25", "26-35", "36-50", "51-65", "65+"), ordered = TRUE),
    gender = factor(sample(c("Male", "Female", "Other"), n, replace = TRUE, 
                          prob = c(0.48, 0.5, 0.02))),
    education = factor(sample(c("High School", "College", "Graduate"), n, replace = TRUE,
                             prob = c(0.3, 0.5, 0.2)),
                      levels = c("High School", "College", "Graduate"), ordered = TRUE),
    income_bracket = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE,
                                  prob = c(0.4, 0.45, 0.15)),
                           levels = c("Low", "Medium", "High"), ordered = TRUE),
    location = factor(sample(c("Urban", "Suburban", "Rural"), n, replace = TRUE,
                            prob = c(0.4, 0.4, 0.2))),
    
    # Survey responses (numeric scales)
    satisfaction_score = sample(1:10, n, replace = TRUE),
    trust_score = sample(1:7, n, replace = TRUE),
    likelihood_recommend = sample(0:10, n, replace = TRUE),
    time_spent_minutes = round(rexp(n, rate = 1/15), 0),
    number_of_issues = sample(0:5, n, replace = TRUE),
    
    stringsAsFactors = FALSE
  )
}

# 4. Financial data for time series analysis
create_groupsummary_financial_data <- function() {
  n <- 500
  
  # Generate timestamps
  start_time <- as.POSIXct("2023-01-01 09:00:00")
  timestamps <- seq(start_time, by = "hour", length.out = n)
  
  data.frame(
    transaction_time = timestamps,
    market_sector = factor(sample(c("Technology", "Healthcare", "Finance", "Energy", "Consumer"), 
                                 n, replace = TRUE)),
    transaction_type = factor(sample(c("Buy", "Sell", "Hold"), n, replace = TRUE,
                                    prob = c(0.4, 0.4, 0.2))),
    risk_level = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE,
                              prob = c(0.3, 0.5, 0.2)),
                       levels = c("Low", "Medium", "High"), ordered = TRUE),
    
    # Financial metrics
    transaction_amount = round(rlnorm(n, meanlog = log(1000), sdlog = 1.5), 2),
    price_change_percent = round(rnorm(n, 0, 2.5), 3),
    volume_traded = round(rexp(n, rate = 1/1000), 0),
    volatility_index = round(runif(n, 0.1, 3.0), 3),
    portfolio_value = round(rlnorm(n, meanlog = log(50000), sdlog = 0.8), 2),
    
    stringsAsFactors = FALSE
  )
}

# 5. Manufacturing quality control data
create_groupsummary_manufacturing_data <- function() {
  n <- 400
  
  data.frame(
    production_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
                            n, replace = TRUE),
    shift = factor(sample(c("Morning", "Afternoon", "Night"), n, replace = TRUE,
                         prob = c(0.4, 0.4, 0.2))),
    production_line = factor(sample(c("Line A", "Line B", "Line C", "Line D"), n, replace = TRUE)),
    operator_experience = factor(sample(c("Junior", "Intermediate", "Senior"), n, replace = TRUE,
                                       prob = c(0.3, 0.5, 0.2)),
                                levels = c("Junior", "Intermediate", "Senior"), ordered = TRUE),
    machine_type = factor(sample(c("Machine 1", "Machine 2", "Machine 3"), n, replace = TRUE)),
    quality_grade = factor(sample(c("Grade A", "Grade B", "Grade C"), n, replace = TRUE,
                                 prob = c(0.6, 0.3, 0.1)),
                          levels = c("Grade A", "Grade B", "Grade C"), ordered = TRUE),
    
    # Quality metrics
    units_produced = sample(80:120, n, replace = TRUE),
    defect_count = sample(0:15, n, replace = TRUE),
    cycle_time_minutes = round(rnorm(n, 45, 8), 1),
    temperature_celsius = round(rnorm(n, 22, 3), 1),
    pressure_psi = round(rnorm(n, 100, 15), 1),
    efficiency_percent = round(runif(n, 75, 98), 1),
    
    stringsAsFactors = FALSE
  )
}

# 6. Website analytics data
create_groupsummary_web_analytics <- function() {
  n <- 600
  
  # Generate hourly data for 25 days
  start_time <- as.POSIXct("2023-11-01 00:00:00")
  timestamps <- seq(start_time, by = "hour", length.out = n)
  
  data.frame(
    session_time = timestamps,
    traffic_source = factor(sample(c("Organic Search", "Social Media", "Direct", "Email", "Ads"),
                                  n, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0.1, 0.1))),
    device_type = factor(sample(c("Desktop", "Mobile", "Tablet"), n, replace = TRUE,
                               prob = c(0.4, 0.5, 0.1))),
    user_type = factor(sample(c("New Visitor", "Returning Visitor"), n, replace = TRUE,
                             prob = c(0.3, 0.7))),
    country = factor(sample(c("USA", "Canada", "UK", "Germany", "Australia"), n, replace = TRUE,
                           prob = c(0.5, 0.15, 0.15, 0.1, 0.1))),
    page_category = factor(sample(c("Homepage", "Product", "Blog", "Contact", "About"), 
                                 n, replace = TRUE)),
    
    # Web metrics
    page_views = sample(1:50, n, replace = TRUE),
    session_duration_minutes = round(rexp(n, rate = 1/5), 1),
    bounce_rate_percent = round(runif(n, 20, 80), 1),
    conversion_value = round(rgamma(n, shape = 1.5, scale = 50), 2),
    click_through_rate = round(runif(n, 0.01, 0.15), 4),
    
    stringsAsFactors = FALSE
  )
}

# Generate all datasets
message("Creating groupsummary test datasets...")

# Create datasets
groupsummary_simple <- create_groupsummary_simple()
groupsummary_sales_data <- create_groupsummary_sales_data()
groupsummary_survey_data <- create_groupsummary_survey_data()
groupsummary_financial_data <- create_groupsummary_financial_data()
groupsummary_manufacturing_data <- create_groupsummary_manufacturing_data()
groupsummary_web_analytics <- create_groupsummary_web_analytics()

# Save datasets
save(groupsummary_simple, file = "data/groupsummary_simple.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_simple, "data/groupsummary_simple.omv")
  message("✓ Created groupsummary_simple.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_simple, "data/groupsummary_simple.omv")
  message("✓ Created groupsummary_simple.omv")
}
save(groupsummary_sales_data, file = "data/groupsummary_sales_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_sales_data, "data/groupsummary_sales_data.omv")
  message("✓ Created groupsummary_sales_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_sales_data, "data/groupsummary_sales_data.omv")
  message("✓ Created groupsummary_sales_data.omv")
}
save(groupsummary_survey_data, file = "data/groupsummary_survey_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_survey_data, "data/groupsummary_survey_data.omv")
  message("✓ Created groupsummary_survey_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_survey_data, "data/groupsummary_survey_data.omv")
  message("✓ Created groupsummary_survey_data.omv")
}
save(groupsummary_financial_data, file = "data/groupsummary_financial_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_financial_data, "data/groupsummary_financial_data.omv")
  message("✓ Created groupsummary_financial_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_financial_data, "data/groupsummary_financial_data.omv")
  message("✓ Created groupsummary_financial_data.omv")
}
save(groupsummary_manufacturing_data, file = "data/groupsummary_manufacturing_data.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_manufacturing_data, "data/groupsummary_manufacturing_data.omv")
  message("✓ Created groupsummary_manufacturing_data.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_manufacturing_data, "data/groupsummary_manufacturing_data.omv")
  message("✓ Created groupsummary_manufacturing_data.omv")
}
save(groupsummary_web_analytics, file = "data/groupsummary_web_analytics.rda")

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_web_analytics, "data/groupsummary_web_analytics.omv")
  message("✓ Created groupsummary_web_analytics.omv")
}

# Also save as .omv for jamovi
if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  jmvReadWrite::write_omv(groupsummary_web_analytics, "data/groupsummary_web_analytics.omv")
  message("✓ Created groupsummary_web_analytics.omv")
}

# Print dataset summaries
cat("\n=== GROUPSUMMARY TEST DATASETS CREATED ===\n")
cat("1. groupsummary_simple:", nrow(groupsummary_simple), "rows,", ncol(groupsummary_simple), "columns\n")
cat("2. groupsummary_sales_data:", nrow(groupsummary_sales_data), "rows,", ncol(groupsummary_sales_data), "columns\n")
cat("3. groupsummary_survey_data:", nrow(groupsummary_survey_data), "rows,", ncol(groupsummary_survey_data), "columns\n")
cat("4. groupsummary_financial_data:", nrow(groupsummary_financial_data), "rows,", ncol(groupsummary_financial_data), "columns\n")
cat("5. groupsummary_manufacturing_data:", nrow(groupsummary_manufacturing_data), "rows,", ncol(groupsummary_manufacturing_data), "columns\n")
cat("6. groupsummary_web_analytics:", nrow(groupsummary_web_analytics), "rows,", ncol(groupsummary_web_analytics), "columns\n")

if (exists("medical_research_data")) {
  cat("7. medical_research_data:", nrow(medical_research_data), "rows,", ncol(medical_research_data), "columns\n")
}

if (exists("hospital_admission_hourly")) {
  cat("8. hospital_admission_hourly:", nrow(hospital_admission_hourly), "rows,", ncol(hospital_admission_hourly), "columns\n")
}

cat("\nDatasets saved successfully!\n")

# Display test scenarios
cat("\n=== SUGGESTED TESTING SCENARIOS ===\n")

cat("\n1. Simple Grouping (groupsummary_simple):\n")
cat("   Group by: category\n")
cat("   Summarize: value1, value2\n")
cat("   Statistics: sum, mean, n\n")

cat("\n2. Sales Analysis by Region (groupsummary_sales_data):\n")
cat("   Group by: region, product_category\n")
cat("   Summarize: sales_amount, quantity_sold\n")
cat("   Statistics: sum, mean, median\n")

cat("\n3. Monthly Sales Trends (groupsummary_sales_data):\n")
cat("   Group by: sale_date\n")
cat("   Date variable: sale_date\n")
cat("   Time aggregation: month\n")
cat("   Summarize: sales_amount\n")
cat("   Statistics: sum, n\n")

cat("\n4. Survey Analysis by Demographics (groupsummary_survey_data):\n")
cat("   Group by: age_group, gender\n")
cat("   Summarize: satisfaction_score, trust_score\n")
cat("   Statistics: mean, median\n")

cat("\n5. Hourly Financial Activity (groupsummary_financial_data):\n")
cat("   Group by: transaction_time\n")
cat("   Date variable: transaction_time\n")
cat("   Date format: ymd_hms\n")
cat("   Time aggregation: hour\n")
cat("   Summarize: transaction_amount, volume_traded\n")

cat("\n6. Manufacturing Quality by Shift (groupsummary_manufacturing_data):\n")
cat("   Group by: shift, quality_grade\n")
cat("   Summarize: units_produced, defect_count\n")
cat("   Statistics: sum, mean\n")

cat("\n7. Website Traffic Patterns (groupsummary_web_analytics):\n")
cat("   Group by: session_time, traffic_source\n")
cat("   Date variable: session_time\n")
cat("   Time aggregation: day\n")
cat("   Summarize: page_views, session_duration_minutes\n")

cat("\n8. Medical Research Multi-center (medical_research_data):\n")
cat("   Group by: StudyCenter, TreatmentGroup\n")
cat("   Summarize: ClinicalScore, SystolicBP\n")
cat("   Statistics: mean, n\n")

cat("\nAll datasets include various data types suitable for comprehensive testing!\n")
