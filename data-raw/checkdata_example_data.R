#' Example Datasets for Data Quality Assessment
#'
#' These datasets provide realistic examples for demonstrating data quality assessment
#' across different clinical scenarios, from excellent quality to various quality issues
#' commonly encountered in clinical research and healthcare data.
#'
#' @name checkdata_examples
#' @docType data
#' @format Data frames with various structures for different data quality scenarios
#' @author ClinicoPath package team

# Set seed for reproducible data generation
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

set.seed(20241204)

# Helper functions for realistic data generation
generate_clinical_ages <- function(n, study_type = "adult_cancer") {
  if (study_type == "adult_cancer") {
    # Cancer studies typically older adults
    ages <- round(rnorm(n, 63, 12))
    ages[ages < 18] <- sample(18:25, sum(ages < 18), replace = TRUE)
    ages[ages > 95] <- sample(85:95, sum(ages > 95), replace = TRUE)
  } else if (study_type == "pediatric") {
    # Pediatric studies
    ages <- round(rnorm(n, 8, 4))
    ages[ages < 0] <- sample(0:2, sum(ages < 0), replace = TRUE)
    ages[ages > 18] <- sample(15:18, sum(ages > 18), replace = TRUE)
  } else {
    # General population
    ages <- round(rnorm(n, 45, 18))
    ages[ages < 18] <- sample(18:25, sum(ages < 18), replace = TRUE)
    ages[ages > 100] <- sample(85:100, sum(ages > 100), replace = TRUE)
  }
  return(ages)
}

generate_biomarker_values <- function(n, assay_type = "protein", quality = "high") {
  if (quality == "high") {
    # High quality assay with tight CV
    if (assay_type == "protein") {
      # Protein concentration (ng/mL) - log-normal distribution
      values <- rlnorm(n, meanlog = 3.5, sdlog = 0.4)  # CV ~40%
    } else if (assay_type == "gene_expression") {
      # Gene expression (fold change) - log-normal
      values <- rlnorm(n, meanlog = 1.0, sdlog = 0.6)  # CV ~60%
    } else {
      # General biomarker
      values <- rlnorm(n, meanlog = 2.0, sdlog = 0.5)
    }
  } else {
    # Lower quality with higher variability
    if (assay_type == "protein") {
      values <- rlnorm(n, meanlog = 3.5, sdlog = 0.8)  # Higher CV
    } else {
      values <- rlnorm(n, meanlog = 2.0, sdlog = 1.0)
    }
  }
  return(values)
}

introduce_missing_data <- function(data, missing_pct, pattern = "random") {
  n <- length(data)
  n_missing <- round(n * missing_pct / 100)
  
  if (pattern == "random") {
    # Completely random missing
    missing_indices <- sample(1:n, n_missing)
  } else if (pattern == "systematic_high") {
    # Missing more often in high values (MNAR)
    prob_missing <- plogis((data - quantile(data, 0.7, na.rm = TRUE)) / sd(data, na.rm = TRUE))
    missing_indices <- sample(1:n, n_missing, prob = prob_missing)
  } else if (pattern == "systematic_low") {
    # Missing more often in low values 
    prob_missing <- plogis(-(data - quantile(data, 0.3, na.rm = TRUE)) / sd(data, na.rm = TRUE))
    missing_indices <- sample(1:n, n_missing, prob = prob_missing)
  } else {
    # Block missing (equipment failure simulation)
    start_block <- sample(1:(n-n_missing+1), 1)
    missing_indices <- start_block:(start_block + n_missing - 1)
  }
  
  data[missing_indices] <- NA
  return(data)
}

introduce_outliers <- function(data, outlier_pct, severity = "moderate") {
  n <- length(data[!is.na(data)])
  n_outliers <- round(n * outlier_pct / 100)
  
  if (n_outliers > 0) {
    clean_data <- data[!is.na(data)]
    outlier_indices <- sample(which(!is.na(data)), n_outliers)
    
    if (severity == "mild") {
      # 3-4 SD outliers
      outlier_multiplier <- sample(c(-1, 1), n_outliers, replace = TRUE) * runif(n_outliers, 3, 4)
    } else if (severity == "moderate") {
      # 4-6 SD outliers  
      outlier_multiplier <- sample(c(-1, 1), n_outliers, replace = TRUE) * runif(n_outliers, 4, 6)
    } else {
      # Extreme outliers >6 SD
      outlier_multiplier <- sample(c(-1, 1), n_outliers, replace = TRUE) * runif(n_outliers, 6, 10)
    }
    
    data_mean <- mean(clean_data, na.rm = TRUE)
    data_sd <- sd(clean_data, na.rm = TRUE)
    
    data[outlier_indices] <- data_mean + outlier_multiplier * data_sd
  }
  
  return(data)
}

# 1. Excellent Quality Clinical Dataset (Grade A expected)
#' @rdname checkdata_examples
#' @description \code{excellent_quality_data}: High-quality clinical research dataset
#' demonstrating optimal data collection practices and minimal quality issues.
excellent_quality_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("EQ", sprintf("%04d", 1:300)),
  
  # Demographics - high quality, complete
  age = generate_clinical_ages(300, "adult_cancer"),
  sex = factor(sample(c("Male", "Female"), 300, replace = TRUE, prob = c(0.55, 0.45))),
  race = factor(sample(c("White", "Black", "Hispanic", "Asian", "Other"), 300, 
                      replace = TRUE, prob = c(0.65, 0.15, 0.12, 0.06, 0.02))),
  
  # Clinical measurements - excellent precision
  weight_kg = round(rnorm(300, 75, 12), 1),
  height_cm = round(rnorm(300, 170, 10), 0),
  bmi = NA,  # Will calculate
  
  # Laboratory values - high quality assays
  hemoglobin_g_dl = round(rnorm(300, 12.5, 1.8), 1),
  creatinine_mg_dl = round(rlnorm(300, meanlog = 0.1, sdlog = 0.3), 2),
  glucose_mg_dl = round(rnorm(300, 95, 15), 0),
  
  # Biomarker measurements - excellent quality
  biomarker_a = generate_biomarker_values(300, "protein", "high"),
  biomarker_b = generate_biomarker_values(300, "gene_expression", "high"),
  
  # Clinical outcomes
  treatment_response = factor(sample(c("Complete Response", "Partial Response", 
                                     "Stable Disease", "Progressive Disease"), 300,
                                   replace = TRUE, prob = c(0.15, 0.35, 0.35, 0.15))),
  survival_months = round(rexp(300, rate = 1/24) + rnorm(300, 0, 2), 1),
  event_occurred = factor(sample(c("Yes", "No"), 300, replace = TRUE, prob = c(0.3, 0.7)))
)

# Calculate BMI
excellent_quality_data$bmi <- round(excellent_quality_data$weight_kg / 
                                   (excellent_quality_data$height_cm/100)^2, 1)

# Introduce minimal missing data (< 2% - Grade A)
excellent_quality_data$creatinine_mg_dl <- introduce_missing_data(
  excellent_quality_data$creatinine_mg_dl, 1.5, "random")

# 2. Missing Data Challenge Dataset (Grade B-C expected)
#' @rdname checkdata_examples
#' @description \code{missing_data_challenge}: Dataset with various missing data patterns
#' demonstrating different mechanisms and their impact on data quality assessment.
missing_data_challenge <- data.frame(
  # Patient identifiers
  patient_id = paste0("MD", sprintf("%04d", 1:400)),
  
  # Complete baseline data
  age = generate_clinical_ages(400, "adult_cancer"),
  sex = factor(sample(c("Male", "Female"), 400, replace = TRUE)),
  
  # Various missing data scenarios
  # Scenario 1: Low missing (5% - Grade B)
  lab_value_5pct = rnorm(400, 50, 10),
  
  # Scenario 2: Moderate missing (15% - Grade C)
  lab_value_15pct = rnorm(400, 75, 15),
  
  # Scenario 3: High missing (25% - Grade C-D)
  lab_value_25pct = rnorm(400, 100, 20),
  
  # Scenario 4: Very high missing (40% - Grade D)
  lab_value_40pct = rnorm(400, 25, 8),
  
  # Scenario 5: MNAR pattern - high values missing
  expensive_test = rlnorm(400, meanlog = 4, sdlog = 1),
  
  # Scenario 6: MAR pattern - missing depends on age
  cognitive_test = rnorm(400, 85, 15),
  
  # Scenario 7: Block missing (equipment failure)
  daily_measurement = rnorm(400, 10, 3),
  
  # Treatment information
  treatment_arm = factor(sample(c("Control", "Treatment A", "Treatment B"), 400, 
                               replace = TRUE)),
  primary_outcome = rnorm(400, 2.5, 1.2)
)

# Introduce different missing patterns
missing_data_challenge$lab_value_5pct <- introduce_missing_data(
  missing_data_challenge$lab_value_5pct, 5, "random")

missing_data_challenge$lab_value_15pct <- introduce_missing_data(
  missing_data_challenge$lab_value_15pct, 15, "random")

missing_data_challenge$lab_value_25pct <- introduce_missing_data(
  missing_data_challenge$lab_value_25pct, 25, "systematic_high")

missing_data_challenge$lab_value_40pct <- introduce_missing_data(
  missing_data_challenge$lab_value_40pct, 40, "random")

missing_data_challenge$expensive_test <- introduce_missing_data(
  missing_data_challenge$expensive_test, 30, "systematic_high")

# MAR pattern: missing cognitive tests in elderly patients
elderly_indices <- which(missing_data_challenge$age > 75)
n_elderly_missing <- round(length(elderly_indices) * 0.4)
elderly_missing_indices <- sample(elderly_indices, n_elderly_missing)
missing_data_challenge$cognitive_test[elderly_missing_indices] <- NA

# Add some random missing for younger patients too
non_elderly_indices <- which(missing_data_challenge$age <= 75)
n_young_missing <- round(length(non_elderly_indices) * 0.1)
young_missing_indices <- sample(non_elderly_indices, n_young_missing)
missing_data_challenge$cognitive_test[young_missing_indices] <- NA

# Block missing pattern (equipment failure days 150-180)
missing_data_challenge$daily_measurement[150:180] <- NA

# 3. Outlier Detection Dataset (Grade C expected)
#' @rdname checkdata_examples
#' @description \code{outlier_detection_data}: Dataset with various outlier patterns
#' demonstrating statistical vs. clinical outliers and their identification.
outlier_detection_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("OD", sprintf("%04d", 1:250)),
  
  # Demographics
  age = generate_clinical_ages(250, "adult_cancer"),
  
  # Scenario 1: Clean data with few natural outliers
  clean_measurement = rnorm(250, 100, 15),
  
  # Scenario 2: Data with mild outliers (3-4 SD)
  mild_outliers = rnorm(250, 50, 8),
  
  # Scenario 3: Data with moderate outliers (4-6 SD)
  moderate_outliers = rnorm(250, 200, 25),
  
  # Scenario 4: Data with extreme outliers (>6 SD)
  extreme_outliers = rnorm(250, 75, 12),
  
  # Scenario 5: Biomarker with measurement errors
  biomarker_errors = rlnorm(250, meanlog = 2.5, sdlog = 0.7),
  
  # Scenario 6: Skewed data with apparent outliers
  skewed_measurement = rexp(250, rate = 0.1),
  
  # Scenario 7: Bimodal distribution (can appear as outliers)
  bimodal_measure = c(rnorm(125, 20, 3), rnorm(125, 80, 5)),
  
  # Clinical context variables
  diagnosis = factor(sample(c("Type A", "Type B", "Type C"), 250, replace = TRUE)),
  severity = factor(sample(c("Mild", "Moderate", "Severe"), 250, 
                          replace = TRUE, prob = c(0.4, 0.4, 0.2)))
)

# Introduce outliers systematically
outlier_detection_data$mild_outliers <- introduce_outliers(
  outlier_detection_data$mild_outliers, 8, "mild")

outlier_detection_data$moderate_outliers <- introduce_outliers(
  outlier_detection_data$moderate_outliers, 6, "moderate")

outlier_detection_data$extreme_outliers <- introduce_outliers(
  outlier_detection_data$extreme_outliers, 4, "extreme")

# Biomarker measurement errors (instrument drift)
error_indices <- sample(1:250, 15)
outlier_detection_data$biomarker_errors[error_indices] <- 
  outlier_detection_data$biomarker_errors[error_indices] * runif(15, 8, 15)

# 4. Low Variability Dataset (Grade B expected)  
#' @rdname checkdata_examples
#' @description \code{low_variability_data}: Dataset demonstrating low variability issues
#' including repeated values, near-constant measurements, and clustering.
low_variability_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("LV", sprintf("%04d", 1:200)),
  
  # Demographics
  age = generate_clinical_ages(200, "adult_cancer"),
  
  # Scenario 1: Almost constant values (instrument precision issue)
  almost_constant = rep(10.0, 200) + rnorm(200, 0, 0.05),  # Very low variability
  
  # Scenario 2: Repeated measurements (rounding issue)
  rounded_values = round(rnorm(200, 25, 5), 0),  # Integer rounding
  
  # Scenario 3: Limited scale responses  
  likert_scale = sample(1:5, 200, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1)),
  
  # Scenario 4: Binary outcome with extreme imbalance
  rare_event = factor(c(rep("No", 195), rep("Yes", 5))),  # 2.5% positive rate
  
  # Scenario 5: Clustered values (systematic measurement bias)
  clustered_values = c(
    rep(15.0, 50),    # Batch 1
    rep(15.2, 50),    # Batch 2 (slight shift)
    rep(14.8, 50),    # Batch 3 (slight shift other way)
    rep(15.1, 50)     # Batch 4
  ) + rnorm(200, 0, 0.1),
  
  # Scenario 6: Floor effect (many at minimum)
  floor_effect = pmax(0, rnorm(200, 2, 3)),  # Many zero values
  
  # Scenario 7: Ceiling effect (many at maximum)
  ceiling_effect = pmin(100, rnorm(200, 95, 8)),  # Many at 100
  
  # Clinical variables
  treatment_site = factor(sample(c("Site 1", "Site 2", "Site 3"), 200, replace = TRUE)),
  measurement_batch = factor(rep(1:4, each = 50))
)

# 5. Poor Quality Dataset (Grade D expected)
#' @rdname checkdata_examples  
#' @description \code{poor_quality_data}: Dataset with multiple severe quality issues
#' demonstrating Grade D scenarios requiring major intervention.
poor_quality_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("PQ", sprintf("%04d", 1:150)),
  
  # Demographics with issues
  age = generate_clinical_ages(150, "adult_cancer"),
  
  # Scenario 1: Very high missing data (60%)
  severely_missing = rnorm(150, 50, 12),
  
  # Scenario 2: High outlier rate with missing data
  outlier_heavy = rnorm(150, 100, 20),
  
  # Scenario 3: Constant value (no variability)
  constant_value = rep(42, 150),
  
  # Scenario 4: Nearly all missing (85%)
  mostly_missing = rnorm(150, 15, 4),
  
  # Scenario 5: Extreme measurement errors
  measurement_chaos = rnorm(150, 25, 6),
  
  # Scenario 6: Impossible values (negative ages, extreme BMI)
  impossible_values = rnorm(150, 30, 10),
  
  # Clinical outcomes with quality issues
  corrupted_outcome = rnorm(150, 2.0, 1.5)
)

# Introduce severe quality issues
poor_quality_data$severely_missing <- introduce_missing_data(
  poor_quality_data$severely_missing, 60, "systematic_high")

poor_quality_data$outlier_heavy <- introduce_outliers(
  poor_quality_data$outlier_heavy, 15, "extreme")
poor_quality_data$outlier_heavy <- introduce_missing_data(
  poor_quality_data$outlier_heavy, 25, "random")

poor_quality_data$mostly_missing <- introduce_missing_data(
  poor_quality_data$mostly_missing, 85, "block")

# Extreme measurement errors
error_indices <- sample(1:150, 25)
poor_quality_data$measurement_chaos[error_indices] <- 
  sample(c(-999, 999, -888, 888, 0), 25, replace = TRUE)

# Impossible values
poor_quality_data$impossible_values[sample(1:150, 10)] <- c(-50, -25, 500, 750, -100, 
                                                          1000, -200, 2000, -75, 1500)

# 6. Categorical Data Quality Dataset
#' @rdname checkdata_examples
#' @description \code{categorical_quality_data}: Dataset demonstrating categorical variable
#' quality issues including imbalance, missing categories, and encoding problems.
categorical_quality_data <- data.frame(
  # Patient identifiers
  patient_id = paste0("CAT", sprintf("%04d", 1:300)),
  
  # Demographics
  age = generate_clinical_ages(300, "adult_cancer"),
  
  # Scenario 1: Well-balanced categories (Grade A)
  balanced_category = factor(sample(c("Group A", "Group B", "Group C", "Group D"), 300,
                                  replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))),
  
  # Scenario 2: Moderately imbalanced (Grade B)
  moderately_imbalanced = factor(sample(c("Common", "Uncommon", "Rare"), 300,
                                       replace = TRUE, prob = c(0.7, 0.2, 0.1))),
  
  # Scenario 3: Severely imbalanced (Grade C-D)
  severely_imbalanced = factor(sample(c("Prevalent", "Very Rare"), 300,
                                     replace = TRUE, prob = c(0.95, 0.05))),
  
  # Scenario 4: Categories with missing data
  category_with_missing = factor(sample(c("Type 1", "Type 2", "Type 3"), 300,
                                       replace = TRUE)),
  
  # Scenario 5: Many unique categories (high cardinality)
  high_cardinality = factor(paste0("Cat", 1:300)),  # Each value unique
  
  # Scenario 6: Binary with extreme imbalance
  extreme_binary = factor(c(rep("Negative", 295), rep("Positive", 5))),
  
  # Scenario 7: Ordinal categories
  ordinal_scale = factor(sample(c("Poor", "Fair", "Good", "Excellent"), 300,
                               replace = TRUE, prob = c(0.1, 0.2, 0.5, 0.2)),
                        levels = c("Poor", "Fair", "Good", "Excellent"), ordered = TRUE),
  
  # Scenario 8: Categories with data entry issues
  inconsistent_entry = factor(sample(c("Male", "MALE", "male", "M", 
                                      "Female", "FEMALE", "female", "F"), 300,
                                    replace = TRUE))
)

# Add missing data to categorical variables
missing_indices <- sample(1:300, 45)  # 15% missing
categorical_quality_data$category_with_missing[missing_indices] <- NA

# 7. Clinical Trial Specific Dataset
#' @rdname checkdata_examples
#' @description \code{clinical_trial_data}: Dataset simulating clinical trial data
#' with typical quality monitoring scenarios and regulatory requirements.
clinical_trial_data <- data.frame(
  # Study identifiers
  patient_id = paste0("CT", sprintf("%04d", 1:400)),
  site_id = factor(paste0("Site_", rep(sprintf("%02d", 1:8), each = 50))),
  enrollment_date = seq(as.Date("2023-01-01"), as.Date("2024-06-30"), length.out = 400),
  
  # Demographics (high quality required for regulatory)
  age = generate_clinical_ages(400, "adult_cancer"),
  sex = factor(sample(c("Male", "Female"), 400, replace = TRUE)),
  race = factor(sample(c("White", "Black or African American", "Asian", 
                        "American Indian or Alaska Native", "Other"), 400,
                       replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.05, 0.05))),
  
  # Randomization (should be balanced)
  treatment_arm = factor(rep(c("Placebo", "Low Dose", "High Dose"), length.out = 400)),
  
  # Primary endpoint (critical quality)
  primary_endpoint = rnorm(400, 2.5, 1.0),
  
  # Secondary endpoints
  secondary_endpoint_1 = rnorm(400, 15.2, 4.3),
  secondary_endpoint_2 = rexp(400, rate = 0.2),
  
  # Safety measurements (require high precision)
  systolic_bp = round(rnorm(400, 125, 18), 0),
  diastolic_bp = round(rnorm(400, 78, 12), 0),
  heart_rate = round(rnorm(400, 72, 15), 0),
  
  # Laboratory safety parameters
  alt_iu_l = round(rlnorm(400, meanlog = 3.2, sdlog = 0.6), 0),
  creatinine_mg_dl = round(rlnorm(400, meanlog = 0.1, sdlog = 0.3), 2),
  
  # Biomarker measurements (precision critical)
  target_biomarker = generate_biomarker_values(400, "protein", "high"),
  companion_biomarker = generate_biomarker_values(400, "gene_expression", "high"),
  
  # Efficacy assessments
  tumor_response = factor(sample(c("Complete Response", "Partial Response",
                                  "Stable Disease", "Progressive Disease"), 400,
                                replace = TRUE, prob = c(0.1, 0.3, 0.4, 0.2))),
  
  # Time-to-event outcomes
  progression_free_survival_months = round(rexp(400, rate = 1/12), 1),
  overall_survival_months = round(rexp(400, rate = 1/24), 1),
  event_pfs = factor(sample(c("Event", "Censored"), 400, replace = TRUE, prob = c(0.6, 0.4))),
  event_os = factor(sample(c("Event", "Censored"), 400, replace = TRUE, prob = c(0.3, 0.7)))
)

# Introduce site-specific quality issues (simulating real trial challenges)
# Site 3 has higher missing data (training issue)
site_3_patients <- which(clinical_trial_data$site_id == "Site_03")
site_3_missing <- sample(site_3_patients, round(length(site_3_patients) * 0.15))
clinical_trial_data$secondary_endpoint_1[site_3_missing] <- NA

# Site 7 has measurement drift (calibration issue)
site_7_patients <- which(clinical_trial_data$site_id == "Site_07")
clinical_trial_data$target_biomarker[site_7_patients] <- 
  clinical_trial_data$target_biomarker[site_7_patients] * 1.3  # 30% bias

# 8. Longitudinal Data Quality Dataset
#' @rdname checkdata_examples
#' @description \code{longitudinal_quality_data}: Dataset demonstrating quality issues
#' in longitudinal clinical data including dropout patterns and measurement consistency.
longitudinal_quality_data <- data.frame(
  # Patient and visit identifiers
  patient_id = rep(paste0("LG", sprintf("%03d", 1:100)), each = 6),
  visit_number = rep(1:6, 100),
  visit_date = rep(seq(as.Date("2024-01-01"), by = "month", length.out = 6), 100) +
               rep(sample(-5:5, 100, replace = TRUE), each = 6),  # Some visit timing variability
  
  # Time-varying measurements
  primary_measure = NA,
  quality_of_life_score = NA,
  biomarker_level = NA,
  
  # Patient characteristics (constant)
  baseline_age = rep(generate_clinical_ages(100, "adult_cancer"), each = 6),
  treatment_group = rep(factor(sample(c("Control", "Treatment"), 100, replace = TRUE)), each = 6),
  
  # Visit completion status
  visit_completed = factor(sample(c("Complete", "Incomplete", "Missed"), 600,
                                 replace = TRUE, prob = c(0.8, 0.15, 0.05)))
)

# Generate longitudinal trajectories with quality issues
for (i in 1:100) {
  patient_indices <- ((i-1)*6 + 1):(i*6)
  baseline_value <- rnorm(1, 50, 10)
  
  if (longitudinal_quality_data$treatment_group[patient_indices[1]] == "Treatment") {
    # Treatment shows improvement over time
    trajectory <- baseline_value + c(0, -2, -5, -8, -10, -12) + rnorm(6, 0, 3)
  } else {
    # Control shows gradual worsening
    trajectory <- baseline_value + c(0, 1, 3, 6, 8, 10) + rnorm(6, 0, 3)
  }
  
  longitudinal_quality_data$primary_measure[patient_indices] <- trajectory
  
  # QoL scores (0-100 scale)
  qol_baseline <- runif(1, 60, 90)
  qol_trajectory <- qol_baseline + c(0, -3, -5, -7, -8, -10) + rnorm(6, 0, 5)
  qol_trajectory <- pmax(0, pmin(100, qol_trajectory))  # Bound 0-100
  longitudinal_quality_data$quality_of_life_score[patient_indices] <- qol_trajectory
  
  # Biomarker with measurement consistency issues
  biomarker_baseline <- rlnorm(1, meanlog = 3, sdlog = 0.5)
  biomarker_trajectory <- biomarker_baseline * exp(rnorm(6, 0, 0.2))  # Log-normal variation
  longitudinal_quality_data$biomarker_level[patient_indices] <- biomarker_trajectory
}

# Introduce dropout patterns (common quality issue)
# 15% dropout by visit 3, 25% by visit 6
dropout_patients <- sample(1:100, 25)
for (patient in dropout_patients) {
  dropout_visit <- sample(3:6, 1)
  patient_indices <- ((patient-1)*6 + 1):(patient*6)
  dropout_indices <- patient_indices[dropout_visit:6]
  
  longitudinal_quality_data$primary_measure[dropout_indices] <- NA
  longitudinal_quality_data$quality_of_life_score[dropout_indices] <- NA
  longitudinal_quality_data$biomarker_level[dropout_indices] <- NA
  longitudinal_quality_data$visit_completed[dropout_indices] <- "Missed"
}

# Export all datasets
use_data_multi_format(excellent_quality_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(missing_data_challenge, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(outlier_detection_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(low_variability_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(poor_quality_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(categorical_quality_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(clinical_trial_data, overwrite = TRUE, save_csv = TRUE)
use_data_multi_format(longitudinal_quality_data, overwrite = TRUE, save_csv = TRUE)

# Generate comprehensive summary
cat("✅ Created 8 comprehensive datasets for data quality assessment:\n\n")

cat("🏆 excellent_quality_data (n=300):\n")
cat("   - High-quality clinical research dataset (Grade A expected)\n")
cat("   - Minimal missing data (<2%), tight measurement precision\n")
cat("   - Demonstrates optimal data collection practices\n")
cat("   - Complete demographics, laboratory values, and biomarkers\n\n")

cat("❓ missing_data_challenge (n=400):\n")
cat("   - Various missing data patterns (5%-40% missing rates)\n")
cat("   - MCAR, MAR, MNAR scenarios for pattern recognition\n")
cat("   - Block missing patterns (equipment failure simulation)\n")
cat("   - Demonstrates impact of missing data mechanisms\n\n")

cat("📊 outlier_detection_data (n=250):\n")
cat("   - Multiple outlier severity levels (mild, moderate, extreme)\n")
cat("   - Statistical vs. clinical outlier distinction\n")
cat("   - Measurement error simulation and instrument drift\n")
cat("   - Bimodal and skewed distributions with apparent outliers\n\n")

cat("📏 low_variability_data (n=200):\n")
cat("   - Low measurement variability and precision issues\n")
cat("   - Repeated values, clustering, and rounding effects\n")
cat("   - Floor and ceiling effects in measurements\n")
cat("   - Extreme category imbalance scenarios\n\n")

cat("🚨 poor_quality_data (n=150):\n")
cat("   - Multiple severe quality issues (Grade D expected)\n")
cat("   - Very high missing data rates (60-85%)\n")
cat("   - Extreme outliers and impossible values\n")
cat("   - Constant values and measurement chaos\n\n")

cat("🏷️ categorical_quality_data (n=300):\n")
cat("   - Categorical variable quality assessment scenarios\n")
cat("   - Category balance, missing categories, high cardinality\n")
cat("   - Data entry inconsistencies and encoding problems\n")
cat("   - Ordinal scales and binary imbalance patterns\n\n")

cat("🔬 clinical_trial_data (n=400):\n")
cat("   - Regulatory-grade clinical trial simulation\n")
cat("   - Multi-site quality variations and monitoring\n")
cat("   - Primary/secondary endpoints with precision requirements\n")
cat("   - Safety parameters and biomarker measurements\n\n")

cat("📈 longitudinal_quality_data (n=600 observations, 100 patients):\n")
cat("   - Longitudinal data quality patterns over 6 visits\n")
cat("   - Dropout patterns and missing data mechanisms\n")
cat("   - Measurement consistency across time points\n")
cat("   - Treatment effect trajectories with quality issues\n\n")

cat("💡 Usage examples:\n")
cat("   checkdata(data = excellent_quality_data, var = 'age')\n")
cat("   checkdata(data = missing_data_challenge, var = 'lab_value_25pct')\n")
cat("   checkdata(data = outlier_detection_data, var = 'extreme_outliers')\n")
cat("   checkdata(data = low_variability_data, var = 'almost_constant')\n")
cat("   checkdata(data = poor_quality_data, var = 'severely_missing')\n")
cat("   checkdata(data = categorical_quality_data, var = 'severely_imbalanced')\n")
cat("   checkdata(data = clinical_trial_data, var = 'primary_endpoint')\n")
cat("   checkdata(data = longitudinal_quality_data, var = 'primary_measure')\n\n")

cat("🎯 Key features:\n")
cat("   - Comprehensive quality scenarios from Grade A to Grade D\n")
cat("   - Realistic clinical research data patterns\n")
cat("   - Multiple quality dimensions (completeness, accuracy, consistency)\n")
cat("   - Regulatory compliance scenarios and monitoring\n")
cat("   - Edge cases and systematic quality issues\n")
cat("   - Longitudinal and multi-site quality challenges\n")
