# Create comprehensive test data for stagemigration function
# This script generates realistic TNM staging datasets for testing the enhanced stagemigration analysis

library(survival)
library(dplyr)
library(tibble)
set.seed(12345)

# Function to generate realistic survival times based on stage
generate_survival_times <- function(stage, hazard_base = 0.02, stage_multipliers = c(1, 1.5, 2.5, 4)) {
  stage_numeric <- as.numeric(stage)
  hazard <- hazard_base * stage_multipliers[stage_numeric]
  # Generate survival times with exponential distribution
  survival_times <- rexp(length(stage), rate = hazard)
  # Convert to months and add some realistic variation
  survival_times_months <- survival_times * 12 + rnorm(length(stage), 0, 2)
  pmax(survival_times_months, 0.1)  # Ensure positive values
}

# Function to generate censoring with realistic patterns
generate_censoring <- function(survival_times, censoring_rate = 0.3) {
  # Higher censoring for longer survival times (administrative censoring)
  censoring_prob <- pmin(censoring_rate + (survival_times - median(survival_times)) / 100, 0.8)
  censoring_prob <- pmax(censoring_prob, 0.1)
  rbinom(length(survival_times), 1, 1 - censoring_prob)
}

# Function to create stage migration matrix
create_stage_migration <- function(old_stage, migration_prob = 0.25, upstage_bias = 0.6) {
  new_stage <- old_stage
  n <- length(old_stage)
  
  # Determine which patients will migrate
  migrate_indices <- sample(1:n, size = round(n * migration_prob))
  
  for (i in migrate_indices) {
    current_stage <- as.numeric(old_stage[i])
    
    # Migration probabilities (higher probability of upstaging)
    if (current_stage == 1) {
      # Stage I can go to II or III
      new_stage[i] <- sample(c(1, 2, 3), 1, prob = c(0.4, 0.4, 0.2))
    } else if (current_stage == 2) {
      # Stage II can go to I, III, or IV
      new_stage[i] <- sample(c(1, 2, 3, 4), 1, prob = c(0.2, 0.3, 0.3, 0.2))
    } else if (current_stage == 3) {
      # Stage III can go to II or IV
      new_stage[i] <- sample(c(2, 3, 4), 1, prob = c(0.2, 0.4, 0.4))
    } else if (current_stage == 4) {
      # Stage IV rarely downstages
      new_stage[i] <- sample(c(3, 4), 1, prob = c(0.1, 0.9))
    }
  }
  
  factor(new_stage, levels = 1:4, labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
}

# 1. Lung Cancer Dataset (Large, comprehensive)
create_lung_cancer_data <- function(n = 1000) {
  
  # Demographics
  age <- round(rnorm(n, 65, 12))
  age <- pmax(age, 30)  # Minimum age 30
  age <- pmin(age, 90)  # Maximum age 90
  
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.55, 0.45)))
  
  # Smoking history (affects staging and survival)
  smoking_status <- factor(sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.15, 0.50, 0.35)))
  
  # Histology
  histology <- factor(sample(c("Adenocarcinoma", "Squamous Cell", "Large Cell", "Other"), n, replace = TRUE, prob = c(0.45, 0.30, 0.15, 0.10)))
  
  # Performance status
  performance_status <- factor(sample(0:2, n, replace = TRUE, prob = c(0.4, 0.4, 0.2)))
  
  # Old staging system (TNM 7th edition)
  old_stage_numeric <- sample(1:4, n, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.20))
  old_stage <- factor(old_stage_numeric, levels = 1:4, labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
  
  # New staging system (TNM 8th edition) with realistic migration
  new_stage <- create_stage_migration(old_stage, migration_prob = 0.30, upstage_bias = 0.65)
  
  # Generate survival times based on new staging (better prognostic discrimination)
  survival_time <- generate_survival_times(new_stage, hazard_base = 0.015, stage_multipliers = c(0.8, 1.2, 2.0, 3.5))
  
  # Adjust survival based on other factors
  survival_time <- survival_time * ifelse(sex == "Female", 1.1, 1.0)  # Female survival advantage
  survival_time <- survival_time * ifelse(smoking_status == "Never", 1.2, ifelse(smoking_status == "Former", 1.1, 1.0))
  survival_time <- survival_time * ifelse(performance_status == "0", 1.2, ifelse(performance_status == "1", 1.0, 0.8))
  survival_time <- survival_time * (1 + (70 - age) * 0.01)  # Age effect
  
  # Generate events
  event <- generate_censoring(survival_time, censoring_rate = 0.35)
  
  # Create final dataset
  lung_cancer_data <- tibble(
    patient_id = 1:n,
    age = age,
    sex = sex,
    smoking_status = smoking_status,
    histology = histology,
    performance_status = performance_status,
    old_stage = old_stage,
    new_stage = new_stage,
    survival_time = round(survival_time, 1),
    event = event,
    cancer_type = "Lung"
  )
  
  return(lung_cancer_data)
}

# 2. Breast Cancer Dataset (Medium size)
create_breast_cancer_data <- function(n = 600) {
  
  # Demographics (mostly female)
  age <- round(rnorm(n, 58, 15))
  age <- pmax(age, 25)
  age <- pmin(age, 85)
  
  sex <- factor(rep("Female", n))  # 99% female for breast cancer
  sex[sample(1:n, round(n * 0.01))] <- "Male"
  
  # Hormone receptor status
  er_status <- factor(sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.75, 0.25)))
  pr_status <- factor(sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.70, 0.30)))
  her2_status <- factor(sample(c("Positive", "Negative"), n, replace = TRUE, prob = c(0.15, 0.85)))
  
  # Histology
  histology <- factor(sample(c("Invasive Ductal", "Invasive Lobular", "Mixed", "Other"), n, replace = TRUE, prob = c(0.75, 0.15, 0.05, 0.05)))
  
  # Grade
  grade <- factor(sample(1:3, n, replace = TRUE, prob = c(0.20, 0.40, 0.40)))
  
  # Old staging system
  old_stage_numeric <- sample(1:4, n, replace = TRUE, prob = c(0.35, 0.35, 0.20, 0.10))
  old_stage <- factor(old_stage_numeric, levels = 1:4, labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
  
  # New staging system with migration
  new_stage <- create_stage_migration(old_stage, migration_prob = 0.25, upstage_bias = 0.55)
  
  # Generate survival times (better prognosis than lung cancer)
  survival_time <- generate_survival_times(new_stage, hazard_base = 0.008, stage_multipliers = c(0.5, 1.0, 2.0, 4.0))
  
  # Adjust for hormone receptor status
  survival_time <- survival_time * ifelse(er_status == "Positive", 1.3, 1.0)
  survival_time <- survival_time * ifelse(pr_status == "Positive", 1.2, 1.0)
  survival_time <- survival_time * ifelse(her2_status == "Positive", 0.8, 1.0)
  survival_time <- survival_time * ifelse(grade == "1", 1.4, ifelse(grade == "2", 1.1, 1.0))
  
  # Generate events
  event <- generate_censoring(survival_time, censoring_rate = 0.45)
  
  # Create final dataset
  breast_cancer_data <- tibble(
    patient_id = 1:n,
    age = age,
    sex = sex,
    er_status = er_status,
    pr_status = pr_status,
    her2_status = her2_status,
    histology = histology,
    grade = grade,
    old_stage = old_stage,
    new_stage = new_stage,
    survival_time = round(survival_time, 1),
    event = event,
    cancer_type = "Breast"
  )
  
  return(breast_cancer_data)
}

# 3. Colorectal Cancer Dataset (Medium size)
create_colorectal_cancer_data <- function(n = 500) {
  
  # Demographics
  age <- round(rnorm(n, 68, 12))
  age <- pmax(age, 35)
  age <- pmin(age, 90)
  
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.52, 0.48)))
  
  # Tumor location
  location <- factor(sample(c("Colon", "Rectum"), n, replace = TRUE, prob = c(0.70, 0.30)))
  
  # Microsatellite instability
  msi_status <- factor(sample(c("MSI-High", "MSI-Low", "MSS"), n, replace = TRUE, prob = c(0.15, 0.10, 0.75)))
  
  # Differentiation
  differentiation <- factor(sample(c("Well", "Moderate", "Poor"), n, replace = TRUE, prob = c(0.15, 0.65, 0.20)))
  
  # CEA level
  cea_elevated <- factor(sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.35, 0.65)))
  
  # Old staging system
  old_stage_numeric <- sample(1:4, n, replace = TRUE, prob = c(0.20, 0.30, 0.30, 0.20))
  old_stage <- factor(old_stage_numeric, levels = 1:4, labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
  
  # New staging system with migration
  new_stage <- create_stage_migration(old_stage, migration_prob = 0.28, upstage_bias = 0.60)
  
  # Generate survival times
  survival_time <- generate_survival_times(new_stage, hazard_base = 0.012, stage_multipliers = c(0.6, 1.1, 1.8, 3.2))
  
  # Adjust for prognostic factors
  survival_time <- survival_time * ifelse(location == "Colon", 1.1, 1.0)
  survival_time <- survival_time * ifelse(msi_status == "MSI-High", 1.3, 1.0)
  survival_time <- survival_time * ifelse(differentiation == "Well", 1.2, ifelse(differentiation == "Moderate", 1.0, 0.8))
  survival_time <- survival_time * ifelse(cea_elevated == "No", 1.1, 1.0)
  
  # Generate events
  event <- generate_censoring(survival_time, censoring_rate = 0.40)
  
  # Create final dataset
  colorectal_cancer_data <- tibble(
    patient_id = 1:n,
    age = age,
    sex = sex,
    location = location,
    msi_status = msi_status,
    differentiation = differentiation,
    cea_elevated = cea_elevated,
    old_stage = old_stage,
    new_stage = new_stage,
    survival_time = round(survival_time, 1),
    event = event,
    cancer_type = "Colorectal"
  )
  
  return(colorectal_cancer_data)
}

# 4. Small Sample Size Dataset (Edge case testing)
create_small_sample_data <- function(n = 80) {
  
  # Demographics
  age <- round(rnorm(n, 65, 10))
  age <- pmax(age, 40)
  age <- pmin(age, 85)
  
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
  
  # Simple staging
  old_stage_numeric <- sample(1:4, n, replace = TRUE, prob = c(0.30, 0.30, 0.25, 0.15))
  old_stage <- factor(old_stage_numeric, levels = 1:4, labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
  
  # Limited migration for small sample
  new_stage <- create_stage_migration(old_stage, migration_prob = 0.20, upstage_bias = 0.60)
  
  # Generate survival times
  survival_time <- generate_survival_times(new_stage, hazard_base = 0.015, stage_multipliers = c(1.0, 1.5, 2.5, 4.0))
  
  # Generate events
  event <- generate_censoring(survival_time, censoring_rate = 0.30)
  
  # Create final dataset
  small_sample_data <- tibble(
    patient_id = 1:n,
    age = age,
    sex = sex,
    old_stage = old_stage,
    new_stage = new_stage,
    survival_time = round(survival_time, 1),
    event = event,
    cancer_type = "Mixed"
  )
  
  return(small_sample_data)
}

# 5. Large Dataset for Performance Testing
create_large_performance_data <- function(n = 2000) {
  
  # Demographics
  age <- round(rnorm(n, 64, 13))
  age <- pmax(age, 25)
  age <- pmin(age, 95)
  
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
  
  # Multiple cancer types
  cancer_type <- factor(sample(c("Lung", "Breast", "Colorectal", "Prostate", "Other"), n, replace = TRUE, prob = c(0.25, 0.20, 0.15, 0.15, 0.25)))
  
  # Treatment center (for frailty testing)
  treatment_center <- factor(sample(paste0("Center_", LETTERS[1:10]), n, replace = TRUE))
  
  # Old staging system
  old_stage_numeric <- sample(1:4, n, replace = TRUE, prob = c(0.25, 0.30, 0.25, 0.20))
  old_stage <- factor(old_stage_numeric, levels = 1:4, labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
  
  # New staging system with migration
  new_stage <- create_stage_migration(old_stage, migration_prob = 0.27, upstage_bias = 0.58)
  
  # Generate survival times varying by cancer type
  base_hazards <- c(0.015, 0.008, 0.012, 0.006, 0.013)  # Lung, Breast, Colorectal, Prostate, Other
  cancer_hazard <- base_hazards[as.numeric(cancer_type)]
  
  survival_time <- numeric(n)
  for (i in 1:n) {
    survival_time[i] <- generate_survival_times(new_stage[i], hazard_base = cancer_hazard[i], stage_multipliers = c(0.7, 1.1, 2.0, 3.5))
  }
  
  # Generate events
  event <- generate_censoring(survival_time, censoring_rate = 0.38)
  
  # Create final dataset
  large_performance_data <- tibble(
    patient_id = 1:n,
    age = age,
    sex = sex,
    cancer_type = cancer_type,
    treatment_center = treatment_center,
    old_stage = old_stage,
    new_stage = new_stage,
    survival_time = round(survival_time, 1),
    event = event
  )
  
  return(large_performance_data)
}

# 6. Problematic Dataset for Edge Case Testing
create_problematic_data <- function(n = 200) {
  
  # Demographics
  age <- round(rnorm(n, 65, 10))
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE))
  
  # Problematic staging patterns
  old_stage_numeric <- sample(1:4, n, replace = TRUE, prob = c(0.60, 0.20, 0.15, 0.05))  # Heavily skewed
  old_stage <- factor(old_stage_numeric, levels = 1:4, labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
  
  # Minimal migration (difficult to detect improvement)
  new_stage <- create_stage_migration(old_stage, migration_prob = 0.10, upstage_bias = 0.50)
  
  # Similar survival between old and new (minimal improvement)
  survival_time_old <- generate_survival_times(old_stage, hazard_base = 0.015, stage_multipliers = c(1.0, 1.5, 2.0, 3.0))
  survival_time_new <- generate_survival_times(new_stage, hazard_base = 0.015, stage_multipliers = c(1.0, 1.4, 1.9, 2.9))
  
  # Use new staging survival times but with minimal difference
  survival_time <- survival_time_new
  
  # High censoring rate
  event <- generate_censoring(survival_time, censoring_rate = 0.60)
  
  # Create final dataset
  problematic_data <- tibble(
    patient_id = 1:n,
    age = age,
    sex = sex,
    old_stage = old_stage,
    new_stage = new_stage,
    survival_time = round(survival_time, 1),
    event = event,
    cancer_type = "Test"
  )
  
  return(problematic_data)
}

# Generate all datasets
cat("Generating lung cancer dataset...\n")
lung_cancer_data <- create_lung_cancer_data(1000)

cat("Generating breast cancer dataset...\n")
breast_cancer_data <- create_breast_cancer_data(600)

cat("Generating colorectal cancer dataset...\n")
colorectal_cancer_data <- create_colorectal_cancer_data(500)

cat("Generating small sample dataset...\n")
small_sample_data <- create_small_sample_data(80)

cat("Generating large performance dataset...\n")
large_performance_data <- create_large_performance_data(2000)

cat("Generating problematic dataset...\n")
problematic_data <- create_problematic_data(200)

# Save datasets
save(lung_cancer_data, file = "data/stagemigration_lung_cancer.rda")
save(breast_cancer_data, file = "data/stagemigration_breast_cancer.rda")
save(colorectal_cancer_data, file = "data/stagemigration_colorectal_cancer.rda")
save(small_sample_data, file = "data/stagemigration_small_sample.rda")
save(large_performance_data, file = "data/stagemigration_large_performance.rda")
save(problematic_data, file = "data/stagemigration_problematic.rda")

# Create a comprehensive combined dataset
combined_data <- bind_rows(
  lung_cancer_data %>% select(patient_id, age, sex, old_stage, new_stage, survival_time, event, cancer_type),
  breast_cancer_data %>% select(patient_id, age, sex, old_stage, new_stage, survival_time, event, cancer_type),
  colorectal_cancer_data %>% select(patient_id, age, sex, old_stage, new_stage, survival_time, event, cancer_type)
) %>%
  mutate(patient_id = row_number())

save(combined_data, file = "data/stagemigration_combined.rda")

# Create summary statistics
summary_stats <- list(
  lung_cancer = list(
    n = nrow(lung_cancer_data),
    migration_rate = mean(lung_cancer_data$old_stage != lung_cancer_data$new_stage),
    event_rate = mean(lung_cancer_data$event),
    median_survival = median(lung_cancer_data$survival_time)
  ),
  breast_cancer = list(
    n = nrow(breast_cancer_data),
    migration_rate = mean(breast_cancer_data$old_stage != breast_cancer_data$new_stage),
    event_rate = mean(breast_cancer_data$event),
    median_survival = median(breast_cancer_data$survival_time)
  ),
  colorectal_cancer = list(
    n = nrow(colorectal_cancer_data),
    migration_rate = mean(colorectal_cancer_data$old_stage != colorectal_cancer_data$new_stage),
    event_rate = mean(colorectal_cancer_data$event),
    median_survival = median(colorectal_cancer_data$survival_time)
  ),
  small_sample = list(
    n = nrow(small_sample_data),
    migration_rate = mean(small_sample_data$old_stage != small_sample_data$new_stage),
    event_rate = mean(small_sample_data$event),
    median_survival = median(small_sample_data$survival_time)
  ),
  large_performance = list(
    n = nrow(large_performance_data),
    migration_rate = mean(large_performance_data$old_stage != large_performance_data$new_stage),
    event_rate = mean(large_performance_data$event),
    median_survival = median(large_performance_data$survival_time)
  ),
  problematic = list(
    n = nrow(problematic_data),
    migration_rate = mean(problematic_data$old_stage != problematic_data$new_stage),
    event_rate = mean(problematic_data$event),
    median_survival = median(problematic_data$survival_time)
  )
)

save(summary_stats, file = "data/stagemigration_summary_stats.rda")

# Print summary
cat("\n=== DATASET SUMMARY ===\n")
for (dataset_name in names(summary_stats)) {
  stats <- summary_stats[[dataset_name]]
  cat(sprintf("%s: n=%d, migration=%.1f%%, events=%.1f%%, median_survival=%.1f months\n",
              dataset_name, stats$n, stats$migration_rate*100, stats$event_rate*100, stats$median_survival))
}

cat("\nAll datasets created successfully!\n")
cat("Files saved in data/ directory:\n")
cat("- stagemigration_lung_cancer.rda\n")
cat("- stagemigration_breast_cancer.rda\n")
cat("- stagemigration_colorectal_cancer.rda\n")
cat("- stagemigration_small_sample.rda\n")
cat("- stagemigration_large_performance.rda\n")
cat("- stagemigration_problematic.rda\n")
cat("- stagemigration_combined.rda\n")
cat("- stagemigration_summary_stats.rda\n")