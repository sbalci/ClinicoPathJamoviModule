# Test Script for Will Rogers Features in Stage Migration Analysis
# This script helps validate all Will Rogers phenomenon features

# Test Data Generation Function
generate_test_data <- function(n = 500, migration_rate = 0.15, seed = 123) {
  set.seed(seed)
  
  # Generate TNM staging data with controlled migration
  data <- data.frame(
    patient_id = 1:n,
    
    # Original staging (TNM 7th edition simulation)
    tnm7_stage = sample(c("T1", "T2", "T3", "T4"), n, 
                       prob = c(0.2, 0.35, 0.30, 0.15), replace = TRUE),
    
    # Survival time (months)
    os_months = round(rexp(n, rate = 1/24) + runif(n, 0, 12), 1),
    
    # Event status
    death_status = sample(c("Alive", "Dead"), n, 
                         prob = c(0.6, 0.4), replace = TRUE)
  )
  
  # Create new staging with controlled migration
  data$tnm8_stage <- data$tnm7_stage
  
  # Simulate stage migration (Will Rogers effect)
  migrate_idx <- sample(1:n, size = floor(n * migration_rate))
  
  for (i in migrate_idx) {
    current_stage <- data$tnm7_stage[i]
    
    # Upstage some T2 to T3 (most common pattern)
    if (current_stage == "T2" && runif(1) < 0.6) {
      data$tnm8_stage[i] <- "T3"
      # Patients who migrate tend to have intermediate survival
      data$os_months[i] <- data$os_months[i] * 0.8
    }
    # Downstage some T3 to T2
    else if (current_stage == "T3" && runif(1) < 0.3) {
      data$tnm8_stage[i] <- "T2"
      # These patients tend to have better survival
      data$os_months[i] <- data$os_months[i] * 1.2
    }
    # Upstage some T3 to T4
    else if (current_stage == "T3" && runif(1) < 0.2) {
      data$tnm8_stage[i] <- "T4"
      data$os_months[i] <- data$os_months[i] * 0.7
    }
  }
  
  return(data)
}

# Test 1: Data Generation and Basic Checks
cat("\n=== TEST 1: Data Generation ===\n")
test_data <- generate_test_data(n = 500, migration_rate = 0.15)
cat("Data dimensions:", nrow(test_data), "x", ncol(test_data), "\n")
cat("Migration rate:", mean(test_data$tnm7_stage != test_data$tnm8_stage), "\n")

# Migration matrix
migration_matrix <- table(test_data$tnm7_stage, test_data$tnm8_stage)
cat("\nMigration Matrix:\n")
print(migration_matrix)

# Test 2: Will Rogers Detection Conditions
cat("\n\n=== TEST 2: Will Rogers Phenomenon Conditions ===\n")

# Check if stages that gain patients show improved survival
for (stage in c("T2", "T3", "T4")) {
  # Original system
  original_patients <- test_data[test_data$tnm7_stage == stage, ]
  original_median <- median(original_patients$os_months[original_patients$death_status == "Dead"], na.rm = TRUE)
  
  # New system  
  new_patients <- test_data[test_data$tnm8_stage == stage, ]
  new_median <- median(new_patients$os_months[new_patients$death_status == "Dead"], na.rm = TRUE)
  
  improvement <- new_median - original_median
  
  cat("\nStage", stage, ":\n")
  cat("  Original system: n =", nrow(original_patients), ", median survival =", 
      round(original_median, 1), "months\n")
  cat("  New system: n =", nrow(new_patients), ", median survival =", 
      round(new_median, 1), "months\n")
  cat("  Improvement:", round(improvement, 1), "months",
      ifelse(improvement > 0, "(IMPROVED)", "(WORSENED)"), "\n")
}

# Test 3: Expected Feature Outputs
cat("\n\n=== TEST 3: Expected Feature Outputs ===\n")

# 3.1 Will Rogers Visualization Data
cat("\n3.1 Will Rogers Visualization:\n")
cat("- Should show bar chart comparing median survival\n")
cat("- Should identify T2→T3 as dominant migration pattern\n")
cat("- Should show survival changes for both T2 and T3\n")

# 3.2 Migration Survival Curves
cat("\n3.2 Migration Survival Curves:\n")
cat("- Should display faceted KM curves for T2 and T3\n")
cat("- Red curves (before) vs Blue curves (after)\n")
cat("- Sample sizes should be displayed\n")

# 3.3 Statistical Analysis
cat("\n3.3 Enhanced Statistical Analysis:\n")

# Simulate log-rank test for T2
t2_with_migrated <- test_data[test_data$tnm7_stage == "T2", ]
t2_without_migrated <- test_data[test_data$tnm7_stage == "T2" & test_data$tnm8_stage == "T2", ]

if (nrow(t2_with_migrated) > 5 && nrow(t2_without_migrated) > 5) {
  library(survival)
  
  # Create survival objects
  surv_with <- Surv(t2_with_migrated$os_months, 
                    t2_with_migrated$death_status == "Dead")
  surv_without <- Surv(t2_without_migrated$os_months, 
                       t2_without_migrated$death_status == "Dead")
  
  # Fit and get medians
  fit_with <- survfit(surv_with ~ 1)
  fit_without <- survfit(surv_without ~ 1)
  
  cat("\nT2 (original) analysis:\n")
  cat("- With migrated patients: n =", nrow(t2_with_migrated), 
      ", median =", summary(fit_with)$table["median"], "\n")
  cat("- Without migrated patients: n =", nrow(t2_without_migrated), 
      ", median =", summary(fit_without)$table["median"], "\n")
  cat("- Expected p-value from log-rank test\n")
}

# Test 4: Error Conditions
cat("\n\n=== TEST 4: Error Handling ===\n")

# 4.1 No migration scenario
no_migration_data <- test_data
no_migration_data$tnm8_stage <- no_migration_data$tnm7_stage
cat("\n4.1 No migration scenario:\n")
cat("- Should show 'No stage migration detected' message\n")
cat("- All features should handle gracefully\n")

# 4.2 Small sample size
small_data <- test_data[1:20, ]
cat("\n4.2 Small sample size (n=20):\n")
cat("- Should show 'Insufficient data' messages where appropriate\n")
cat("- Should not crash\n")

# Test 5: Integration Checklist
cat("\n\n=== TEST 5: Integration Checklist ===\n")
cat("\nEnable these options in jamovi:\n")
cat("[ ] Advanced Migration Analysis\n")
cat("[ ] Will Rogers Analysis\n")
cat("[ ] Will Rogers Effect Visualization\n")
cat("[ ] Migration Survival Curve Comparison\n")
cat("[ ] Explanations for Results\n")
cat("[ ] Show Abbreviation Glossary\n")

cat("\nVerify these outputs:\n")
cat("[ ] Enhanced Will Rogers Statistical Analysis table shows log-rank tests\n")
cat("[ ] 95% CI columns are populated\n")
cat("[ ] Overall Assessment shows combined p-value\n")
cat("[ ] Will Rogers visualization plot displays\n")
cat("[ ] Migration survival curves show color-coded KM plots\n")
cat("[ ] Dashboard shows Will Rogers evidence row\n")
cat("[ ] No Turkish locale errors\n")

# Save test data for manual testing
cat("\n\nSaving test data to: test_stage_migration_data.csv\n")
write.csv(test_data, "test_stage_migration_data.csv", row.names = FALSE)

cat("\n=== TEST COMPLETE ===\n")
cat("Use the generated test data file with jamovi to validate all features.\n")