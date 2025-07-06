# Create comprehensive test datasets for hullplot function
# This script generates datasets specifically designed for testing hullplot functionality

library(dplyr)
library(magrittr)

set.seed(789)

# 1. Customer segmentation data - classic marketing use case
create_hullplot_customer_data <- function() {
  n <- 500
  
  # Create customer segments with realistic patterns
  segments <- c("Budget Conscious", "Premium Customers", "Frequent Buyers", "Occasional Shoppers")
  
  # Generate segment-specific patterns
  data <- data.frame(
    customer_id = paste0("CUST", sprintf("%04d", 1:n)),
    segment = factor(sample(segments, n, replace = TRUE, prob = c(0.3, 0.2, 0.25, 0.25))),
    stringsAsFactors = FALSE
  )
  
  # Generate realistic spending and frequency patterns based on segments
  data <- data %>%
    mutate(
      # Spending patterns vary by segment
      annual_spending = case_when(
        segment == "Budget Conscious" ~ round(rnorm(n, 500, 150)),
        segment == "Premium Customers" ~ round(rnorm(n, 2500, 500)),
        segment == "Frequent Buyers" ~ round(rnorm(n, 1200, 300)),
        segment == "Occasional Shoppers" ~ round(rnorm(n, 300, 100))
      ),
      
      # Purchase frequency varies by segment
      purchase_frequency = case_when(
        segment == "Budget Conscious" ~ round(rnorm(n, 8, 3)),
        segment == "Premium Customers" ~ round(rnorm(n, 15, 4)),
        segment == "Frequent Buyers" ~ round(rnorm(n, 25, 6)),
        segment == "Occasional Shoppers" ~ round(rnorm(n, 4, 2))
      ),
      
      # Demographics
      age = case_when(
        segment == "Budget Conscious" ~ round(rnorm(n, 35, 12)),
        segment == "Premium Customers" ~ round(rnorm(n, 45, 10)),
        segment == "Frequent Buyers" ~ round(rnorm(n, 40, 8)),
        segment == "Occasional Shoppers" ~ round(rnorm(n, 30, 15))
      ),
      
      # Customer satisfaction score
      satisfaction = round(runif(n, 1, 10), 1),
      
      # Loyalty program membership
      loyalty_member = factor(sample(c("Yes", "No"), n, replace = TRUE, 
                                   prob = c(0.6, 0.4))),
      
      # Geographic region
      region = factor(sample(c("North", "South", "East", "West"), n, replace = TRUE)),
      
      # Shopping channel preference
      channel = factor(sample(c("Online", "In-Store", "Mobile", "Catalog"), n, replace = TRUE,
                            prob = c(0.4, 0.35, 0.2, 0.05))),
      
      # Product category preference (for color mapping)
      preferred_category = factor(sample(c("Electronics", "Clothing", "Home", "Sports", "Books"), 
                                       n, replace = TRUE))
    ) %>%
    # Ensure reasonable bounds
    mutate(
      annual_spending = pmax(50, annual_spending),
      purchase_frequency = pmax(1, purchase_frequency),
      age = pmax(18, pmin(80, age))
    )
  
  return(data)
}

# 2. Clinical patient clustering data
create_hullplot_clinical_data <- function() {
  n <- 400
  
  # Patient disease subtypes
  subtypes <- c("Type A", "Type B", "Type C", "Control")
  
  data.frame(
    patient_id = paste0("PT", sprintf("%03d", 1:n)),
    disease_subtype = factor(sample(subtypes, n, replace = TRUE, 
                                   prob = c(0.25, 0.25, 0.25, 0.25))),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Biomarker levels vary by disease subtype
      biomarker_x = case_when(
        disease_subtype == "Type A" ~ rnorm(n, 150, 30),
        disease_subtype == "Type B" ~ rnorm(n, 200, 25),
        disease_subtype == "Type C" ~ rnorm(n, 100, 35),
        disease_subtype == "Control" ~ rnorm(n, 80, 20)
      ),
      
      biomarker_y = case_when(
        disease_subtype == "Type A" ~ rnorm(n, 75, 15),
        disease_subtype == "Type B" ~ rnorm(n, 120, 20),
        disease_subtype == "Type C" ~ rnorm(n, 90, 18),
        disease_subtype == "Control" ~ rnorm(n, 50, 12)
      ),
      
      # Patient demographics
      age = round(rnorm(n, 65, 12)),
      gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
      
      # Disease severity score
      severity_score = round(runif(n, 0, 100), 1),
      
      # Treatment response
      treatment_response = factor(sample(c("Complete", "Partial", "None"), n, replace = TRUE,
                                       prob = c(0.3, 0.4, 0.3))),
      
      # Hospital location
      hospital = factor(sample(c("Hospital A", "Hospital B", "Hospital C"), n, replace = TRUE)),
      
      # Time since diagnosis (months)
      months_since_diagnosis = round(runif(n, 1, 60)),
      
      # Comorbidity count
      comorbidities = sample(0:5, n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.08, 0.02))
    ) %>%
    # Ensure reasonable bounds
    mutate(
      biomarker_x = pmax(10, biomarker_x),
      biomarker_y = pmax(5, biomarker_y),
      age = pmax(18, pmin(95, age))
    )
}

# 3. Research experimental data - multi-dimensional clustering
create_hullplot_experimental_data <- function() {
  n <- 350
  
  # Experimental conditions
  conditions <- c("Control", "Treatment A", "Treatment B", "Treatment C")
  
  data.frame(
    experiment_id = paste0("EXP", sprintf("%03d", 1:n)),
    condition = factor(sample(conditions, n, replace = TRUE)),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Primary response variables
      response_x = case_when(
        condition == "Control" ~ rnorm(n, 50, 10),
        condition == "Treatment A" ~ rnorm(n, 65, 12),
        condition == "Treatment B" ~ rnorm(n, 80, 15),
        condition == "Treatment C" ~ rnorm(n, 45, 8)
      ),
      
      response_y = case_when(
        condition == "Control" ~ rnorm(n, 30, 8),
        condition == "Treatment A" ~ rnorm(n, 55, 10),
        condition == "Treatment B" ~ rnorm(n, 40, 12),
        condition == "Treatment C" ~ rnorm(n, 70, 14)
      ),
      
      # Experimental batch
      batch = factor(sample(paste0("Batch ", 1:5), n, replace = TRUE)),
      
      # Time point
      timepoint = factor(sample(c("Baseline", "Day 7", "Day 14", "Day 28"), n, replace = TRUE)),
      
      # Experimental replicate
      replicate = sample(1:3, n, replace = TRUE),
      
      # Quality control metrics
      quality_score = round(runif(n, 70, 100), 1),
      
      # Dose level (for treatment groups)
      dose_level = case_when(
        condition == "Control" ~ 0,
        condition == "Treatment A" ~ sample(c(1, 2, 3), n, replace = TRUE),
        condition == "Treatment B" ~ sample(c(2, 4, 6), n, replace = TRUE),
        condition == "Treatment C" ~ sample(c(1, 3, 5), n, replace = TRUE)
      ),
      
      # Laboratory technician
      technician = factor(sample(c("Tech A", "Tech B", "Tech C"), n, replace = TRUE)),
      
      # Measurement confidence
      confidence = round(runif(n, 0.8, 1.0), 3)
    )
}

# 4. Social science survey data
create_hullplot_survey_data <- function() {
  n <- 450
  
  # Political orientation groups
  orientations <- c("Liberal", "Conservative", "Moderate", "Independent")
  
  data.frame(
    respondent_id = paste0("R", sprintf("%04d", 1:n)),
    political_orientation = factor(sample(orientations, n, replace = TRUE,
                                        prob = c(0.28, 0.32, 0.25, 0.15))),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Economic views vs social views
      economic_conservatism = case_when(
        political_orientation == "Liberal" ~ round(rnorm(n, 25, 15)),
        political_orientation == "Conservative" ~ round(rnorm(n, 75, 12)),
        political_orientation == "Moderate" ~ round(rnorm(n, 50, 18)),
        political_orientation == "Independent" ~ round(rnorm(n, 45, 20))
      ),
      
      social_conservatism = case_when(
        political_orientation == "Liberal" ~ round(rnorm(n, 20, 12)),
        political_orientation == "Conservative" ~ round(rnorm(n, 80, 10)),
        political_orientation == "Moderate" ~ round(rnorm(n, 50, 15)),
        political_orientation == "Independent" ~ round(rnorm(n, 40, 25))
      ),
      
      # Demographics
      age = round(rnorm(n, 45, 16)),
      education = factor(sample(c("High School", "Bachelor", "Graduate"), n, replace = TRUE,
                              prob = c(0.4, 0.4, 0.2))),
      income_level = factor(sample(c("Low", "Middle", "High"), n, replace = TRUE,
                                 prob = c(0.3, 0.5, 0.2))),
      
      # Geographic region
      region = factor(sample(c("Urban", "Suburban", "Rural"), n, replace = TRUE,
                           prob = c(0.4, 0.35, 0.25))),
      
      # Voting behavior
      voter_turnout = factor(sample(c("Always", "Sometimes", "Rarely", "Never"), n, replace = TRUE,
                                  prob = c(0.35, 0.3, 0.2, 0.15))),
      
      # Media consumption
      news_source = factor(sample(c("Traditional", "Social Media", "Online", "Mixed"), n, replace = TRUE,
                                prob = c(0.25, 0.3, 0.25, 0.2))),
      
      # Trust in institutions (0-100 scale)
      institutional_trust = round(runif(n, 0, 100))
    ) %>%
    # Ensure reasonable bounds
    mutate(
      economic_conservatism = pmax(0, pmin(100, economic_conservatism)),
      social_conservatism = pmax(0, pmin(100, social_conservatism)),
      age = pmax(18, pmin(85, age))
    )
}

# 5. Manufacturing quality control data
create_hullplot_quality_data <- function() {
  n <- 300
  
  # Quality control groups
  qc_groups <- c("Excellent", "Good", "Acceptable", "Poor")
  
  data.frame(
    product_id = paste0("PROD", sprintf("%04d", 1:n)),
    quality_group = factor(sample(qc_groups, n, replace = TRUE,
                                prob = c(0.3, 0.4, 0.25, 0.05))),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Quality dimensions
      dimensional_accuracy = case_when(
        quality_group == "Excellent" ~ rnorm(n, 95, 3),
        quality_group == "Good" ~ rnorm(n, 85, 5),
        quality_group == "Acceptable" ~ rnorm(n, 75, 8),
        quality_group == "Poor" ~ rnorm(n, 60, 12)
      ),
      
      surface_finish = case_when(
        quality_group == "Excellent" ~ rnorm(n, 92, 4),
        quality_group == "Good" ~ rnorm(n, 80, 6),
        quality_group == "Acceptable" ~ rnorm(n, 70, 10),
        quality_group == "Poor" ~ rnorm(n, 55, 15)
      ),
      
      # Production details
      production_line = factor(sample(c("Line 1", "Line 2", "Line 3", "Line 4"), n, replace = TRUE)),
      shift = factor(sample(c("Day", "Night", "Weekend"), n, replace = TRUE,
                          prob = c(0.5, 0.4, 0.1))),
      operator = factor(sample(c("Operator A", "Operator B", "Operator C", "Operator D"), 
                             n, replace = TRUE)),
      
      # Process parameters
      temperature = round(rnorm(n, 180, 10), 1),
      pressure = round(rnorm(n, 100, 8), 1),
      
      # Material properties
      material_grade = factor(sample(c("Grade A", "Grade B", "Grade C"), n, replace = TRUE,
                                   prob = c(0.5, 0.35, 0.15))),
      
      # Inspection results
      defect_count = sample(0:10, n, replace = TRUE, prob = c(0.4, 0.25, 0.15, 0.1, 0.05, 0.03, 0.01, 0.005, 0.003, 0.002, 0.001)),
      
      # Cost metrics
      production_cost = round(rnorm(n, 25, 5), 2)
    ) %>%
    # Ensure reasonable bounds
    mutate(
      dimensional_accuracy = pmax(30, pmin(100, dimensional_accuracy)),
      surface_finish = pmax(30, pmin(100, surface_finish))
    )
}

# Generate all datasets
message("Creating hullplot test datasets...")

# Create datasets
hullplot_customer_data <- create_hullplot_customer_data()
hullplot_clinical_data <- create_hullplot_clinical_data()
hullplot_experimental_data <- create_hullplot_experimental_data()
hullplot_survey_data <- create_hullplot_survey_data()
hullplot_quality_data <- create_hullplot_quality_data()

# Save datasets
save(hullplot_customer_data, file = "data/hullplot_customer_data.rda")
save(hullplot_clinical_data, file = "data/hullplot_clinical_data.rda")
save(hullplot_experimental_data, file = "data/hullplot_experimental_data.rda")
save(hullplot_survey_data, file = "data/hullplot_survey_data.rda")
save(hullplot_quality_data, file = "data/hullplot_quality_data.rda")

# Print dataset summaries
cat("\n=== HULLPLOT TEST DATASETS CREATED ===\n")
cat("1. hullplot_customer_data:", nrow(hullplot_customer_data), "rows,", ncol(hullplot_customer_data), "columns\n")
cat("2. hullplot_clinical_data:", nrow(hullplot_clinical_data), "rows,", ncol(hullplot_clinical_data), "columns\n")
cat("3. hullplot_experimental_data:", nrow(hullplot_experimental_data), "rows,", ncol(hullplot_experimental_data), "columns\n")
cat("4. hullplot_survey_data:", nrow(hullplot_survey_data), "rows,", ncol(hullplot_survey_data), "columns\n")
cat("5. hullplot_quality_data:", nrow(hullplot_quality_data), "rows,", ncol(hullplot_quality_data), "columns\n")

cat("\n=== SUGGESTED TESTING SCENARIOS ===\n")

cat("\n1. Customer Segmentation (hullplot_customer_data):\n")
cat("   X-Axis: annual_spending, Y-Axis: purchase_frequency\n")
cat("   Group by: segment\n")
cat("   Color by: preferred_category, Size by: satisfaction\n")

cat("\n2. Clinical Clustering (hullplot_clinical_data):\n") 
cat("   X-Axis: biomarker_x, Y-Axis: biomarker_y\n")
cat("   Group by: disease_subtype\n")
cat("   Color by: treatment_response, Size by: severity_score\n")

cat("\n3. Experimental Results (hullplot_experimental_data):\n")
cat("   X-Axis: response_x, Y-Axis: response_y\n")
cat("   Group by: condition\n")
cat("   Color by: timepoint, Size by: dose_level\n")

cat("\n4. Survey Analysis (hullplot_survey_data):\n")
cat("   X-Axis: economic_conservatism, Y-Axis: social_conservatism\n")
cat("   Group by: political_orientation\n")
cat("   Color by: region, Size by: institutional_trust\n")

cat("\n5. Quality Control (hullplot_quality_data):\n")
cat("   X-Axis: dimensional_accuracy, Y-Axis: surface_finish\n")
cat("   Group by: quality_group\n")
cat("   Color by: material_grade, Size by: defect_count\n")

cat("\nAll datasets include realistic clustering patterns and appropriate variable types!\n")

# Display data structure examples
cat("\n=== DATASET STRUCTURE EXAMPLES ===\n")
cat("\nCustomer Data Structure:\n")
str(hullplot_customer_data)

cat("\nClinical Data Structure:\n")
str(hullplot_clinical_data)