# Test data generation for adaptivelasso function
# Creates realistic clinical datasets for Adaptive LASSO Cox regression analysis
# Covers all adaptivelasso options: weight methods, stability selection, diagnostics,
# risk groups, stratification, and edge cases

# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(dplyr)
library(survival)

# Set seed for reproducibility
set.seed(42)

# =============================================================================
# Dataset 1: Main test dataset (n=180, ~55% event rate)
# Clinicopathological study with 12+ predictors, strata, collinearity, and
# a mix of strong, moderate, weak, and null effects
# =============================================================================
create_adaptivelasso_main <- function() {
  n <- 180

  # --- Continuous predictors ---
  age <- round(rnorm(n, mean = 62, sd = 13))
  age[age < 25] <- 25
  age[age > 90] <- 90

  tumor_size <- round(rlnorm(n, meanlog = log(3.5), sdlog = 0.55), 1)
  tumor_size[tumor_size > 15] <- 15

  # ki67 and grade are deliberately correlated (moderate collinearity)
  grade_numeric <- sample(1:3, n, replace = TRUE, prob = c(0.25, 0.45, 0.30))
  ki67_index <- round(pmin(pmax(
    10 * grade_numeric + rnorm(n, mean = 0, sd = 12), 1), 95), 1)

  # Near-zero-effect variables (for selection testing)
  hemoglobin <- round(rnorm(n, mean = 13.0, sd = 1.8), 1)
  albumin <- round(rnorm(n, mean = 4.0, sd = 0.5), 1)

  crp_level <- round(pmax(rlnorm(n, meanlog = log(5), sdlog = 0.9), 0.1), 1)

  # --- Categorical predictors (factors) ---
  gender <- factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.52, 0.48)))

  stage <- factor(
    sample(c("I", "II", "III", "IV"), n, replace = TRUE, prob = c(0.20, 0.30, 0.30, 0.20)),
    levels = c("I", "II", "III", "IV")
  )

  grade <- factor(grade_numeric, levels = 1:3, labels = c("Low", "Moderate", "High"))

  treatment <- factor(
    sample(c("ChemoRT", "Surgery", "Combined"), n, replace = TRUE, prob = c(0.30, 0.35, 0.35)),
    levels = c("ChemoRT", "Surgery", "Combined")
  )

  smoking_status <- factor(
    sample(c("Never", "Former", "Current"), n, replace = TRUE, prob = c(0.40, 0.35, 0.25)),
    levels = c("Never", "Former", "Current")
  )

  # --- Strata variable ---
  center <- factor(
    sample(c("CenterA", "CenterB", "CenterC"), n, replace = TRUE, prob = c(0.40, 0.35, 0.25)),
    levels = c("CenterA", "CenterB", "CenterC")
  )

  # --- True survival model ---
  # True effects: age, stage, ki67, treatment have genuine associations
  # hemoglobin, albumin are near-null
  # crp has a weak effect
  # smoking has a moderate effect
  risk_score <-
    0.02 * (age - 62) +                          # age: weak per-year
    0.35 * (as.numeric(stage) - 1) +              # stage: strong
    0.015 * (ki67_index - 30) +                   # ki67: moderate
    -0.30 * (treatment == "Surgery") +            # surgery protective
    -0.50 * (treatment == "Combined") +           # combined most protective
    0.20 * (smoking_status == "Current") +        # smoking: moderate
    0.10 * (smoking_status == "Former") +          # former smoker: weak
    0.05 * log(crp_level) +                       # crp: weak
    0.005 * (hemoglobin - 13.0) +                 # hemoglobin: near-null
    -0.002 * (albumin - 4.0)                      # albumin: near-null

  # Center-specific baseline hazard shift (for stratification testing)
  center_shift <- ifelse(center == "CenterA", 0,
                  ifelse(center == "CenterB", 0.15, -0.10))

  # Generate survival times using Weibull distribution
  shape_param <- 1.3
  scale_param <- exp(-(risk_score + center_shift) / shape_param) * 36
  survival_times <- rweibull(n, shape = shape_param, scale = scale_param)

  # Censoring: administrative at 60 months + random loss to follow-up
  admin_censor <- 60
  loss_followup <- rexp(n, rate = 0.012)
  censoring_times <- pmin(admin_censor, loss_followup)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time[observed_time < 0.1] <- 0.1
  event_indicator <- as.numeric(survival_times <= censoring_times)

  # Target ~55% event rate; if needed, adjust by tweaking parameters above
  event_factor <- factor(event_indicator, levels = c(0, 1), labels = c("Censored", "Event"))

  # Assemble dataset
  adaptivelasso_test_data <- data.frame(
    patient_id    = paste0("AL_", sprintf("%03d", 1:n)),
    time          = observed_time,
    event         = event_factor,
    age           = age,
    tumor_size    = tumor_size,
    ki67_index    = ki67_index,
    hemoglobin    = hemoglobin,
    crp_level     = crp_level,
    albumin       = albumin,
    gender        = gender,
    stage         = stage,
    grade         = grade,
    treatment     = treatment,
    smoking_status = smoking_status,
    center        = center,
    stringsAsFactors = FALSE
  )

  return(adaptivelasso_test_data)
}

# =============================================================================
# Dataset 2: Small / edge-case dataset (n=40, ~30% event rate = 12 events)
# For testing edge cases: small n, few events, limited predictors
# =============================================================================
create_adaptivelasso_small <- function() {
  n <- 40

  age <- round(rnorm(n, mean = 60, sd = 10))
  age[age < 30] <- 30
  age[age > 85] <- 85

  tumor_size <- round(rlnorm(n, meanlog = log(3.0), sdlog = 0.5), 1)

  gender <- factor(sample(c("Male", "Female"), n, replace = TRUE))

  stage <- factor(
    sample(c("I", "II", "III"), n, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
    levels = c("I", "II", "III")
  )

  biomarker_a <- round(rnorm(n, mean = 0, sd = 1), 3)
  biomarker_b <- round(rnorm(n, mean = 0, sd = 1), 3)

  center <- factor(
    sample(c("CenterA", "CenterB"), n, replace = TRUE),
    levels = c("CenterA", "CenterB")
  )

  # Simple risk model
  risk_score <- 0.3 * (as.numeric(stage) - 1) + 0.2 * biomarker_a + 0.01 * (age - 60)

  # Weibull survival with high censoring to target ~12 events (30%)
  shape_param <- 1.1
  scale_param <- exp(-risk_score / shape_param) * 48
  survival_times <- rweibull(n, shape = shape_param, scale = scale_param)

  # Heavy censoring
  admin_censor <- 24
  early_dropout <- rexp(n, rate = 0.08)
  censoring_times <- pmin(admin_censor, early_dropout)

  observed_time <- round(pmin(survival_times, censoring_times), 1)
  observed_time[observed_time < 0.1] <- 0.1
  event_indicator <- as.numeric(survival_times <= censoring_times)

  event_factor <- factor(event_indicator, levels = c(0, 1), labels = c("Censored", "Event"))

  adaptivelasso_small_data <- data.frame(
    patient_id  = paste0("AS_", sprintf("%02d", 1:n)),
    time        = observed_time,
    event       = event_factor,
    age         = age,
    tumor_size  = tumor_size,
    gender      = gender,
    stage       = stage,
    biomarker_a = biomarker_a,
    biomarker_b = biomarker_b,
    center      = center,
    stringsAsFactors = FALSE
  )

  return(adaptivelasso_small_data)
}


# =============================================================================
# Generate datasets
# =============================================================================
print("Generating test datasets for adaptivelasso function...")

adaptivelasso_test_data  <- create_adaptivelasso_main()
adaptivelasso_small_data <- create_adaptivelasso_small()

# --- Summary reports ---
cat("\nDataset 1: Adaptive LASSO Main Test Data\n")
cat("Dimensions:", nrow(adaptivelasso_test_data), "x", ncol(adaptivelasso_test_data), "\n")
cat("Events:", sum(adaptivelasso_test_data$event == "Event"), "/",
    nrow(adaptivelasso_test_data),
    "(", round(100 * mean(adaptivelasso_test_data$event == "Event"), 1), "%)\n")
cat("Median follow-up:", round(median(adaptivelasso_test_data$time), 1), "months\n")
cat("Predictors (continuous): age, tumor_size, ki67_index, hemoglobin, crp_level, albumin\n")
cat("Predictors (categorical): gender, stage, grade, treatment, smoking_status\n")
cat("Strata variable: center (CenterA/CenterB/CenterC)\n")
cat("Collinear pair: ki67_index ~ grade (moderate correlation by construction)\n")
cat("Near-null predictors: hemoglobin, albumin\n")

cat("\nDataset 2: Adaptive LASSO Small / Edge-Case Data\n")
cat("Dimensions:", nrow(adaptivelasso_small_data), "x", ncol(adaptivelasso_small_data), "\n")
cat("Events:", sum(adaptivelasso_small_data$event == "Event"), "/",
    nrow(adaptivelasso_small_data),
    "(", round(100 * mean(adaptivelasso_small_data$event == "Event"), 1), "%)\n")
cat("Median follow-up:", round(median(adaptivelasso_small_data$time), 1), "months\n")

# --- Verify collinearity between ki67_index and grade ---
cat("\nCollinearity check (ki67 ~ grade):\n")
cat("  Mean ki67 by grade:\n")
tapply(adaptivelasso_test_data$ki67_index, adaptivelasso_test_data$grade, mean) |> print()
cor_val <- cor(adaptivelasso_test_data$ki67_index, as.numeric(adaptivelasso_test_data$grade))
cat("  Pearson r(ki67, grade_numeric):", round(cor_val, 3), "\n")

# =============================================================================
# Save datasets
# =============================================================================

# Save main dataset
save_data_multi_format(adaptivelasso_test_data, "adaptivelasso_test_data", save_csv = TRUE)

# Save small dataset
save_data_multi_format(adaptivelasso_small_data, "adaptivelasso_small_data", save_csv = TRUE)

cat("\nTest datasets created successfully!\n")
cat("Saved as .rda/.omv files in data/ and .csv in data/ directory\n")

# =============================================================================
# Clinical interpretation guide
# =============================================================================
cat("\n", paste(rep("=", 65), collapse = ""), "\n")
cat("CLINICAL INTERPRETATION GUIDE FOR ADAPTIVELASSO TEST DATA\n")
cat(paste(rep("=", 65), collapse = ""), "\n")

cat("\n1. MAIN TEST DATA (n=180, ~55% events)\n")
cat("   Purpose: Comprehensive testing of all adaptivelasso options\n")
cat("   True effects: age (weak), stage (strong), ki67 (moderate),\n")
cat("                 treatment (strong protective), smoking (moderate)\n")
cat("   Near-null: hemoglobin, albumin (should be excluded by LASSO)\n")
cat("   Collinearity: ki67_index and grade are correlated by design\n")
cat("   Strata: center variable creates separate baseline hazards\n")
cat("   Use cases:\n")
cat("     - Standard clinical variable selection\n")
cat("     - Weight method comparison (ridge vs univariate vs cox vs correlation vs equal)\n")
cat("     - Stability selection with bootstrap\n")
cat("     - Proportional hazards testing\n")
cat("     - Risk group survival curves (2-10 groups)\n")
cat("     - Stratified analysis by center\n")
cat("     - Gamma and alpha sensitivity testing\n")

cat("\n2. SMALL / EDGE-CASE DATA (n=40, ~12 events)\n")
cat("   Purpose: Edge case testing with limited data\n")
cat("   Challenges: Low EPV, high censoring, few predictors\n")
cat("   Use cases:\n")
cat("     - Convergence warnings with max_iterations\n")
cat("     - Small CV folds (cv_folds=3)\n")
cat("     - Reduced bootstrap_samples for speed\n")
cat("     - Error handling when LASSO selects no variables\n")
cat("     - Single lambda value testing\n")
