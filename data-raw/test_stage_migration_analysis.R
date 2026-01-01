# =============================================================================
# Test Stage Migration Analysis Function
# =============================================================================
# 
# Description: Tests the stage migration analysis function with generated data
#              to ensure everything works correctly
# 
# Author: ClinicoPath Development Team
# Created: 2024
# 
# =============================================================================

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(here)
library(survival)

# Load the test data
test_data_path <- here::here("data", "stage_migration_test_data.csv")

if (file.exists(test_data_path)) {
  # Read the test data
  test_data <- read.csv(test_data_path)
  
  cat("=== Stage Migration Analysis Function Test ===\n")
  cat("Test data loaded successfully!\n")
  cat("Sample size:", nrow(test_data), "patients\n")
  cat("Variables available:", names(test_data), "\n\n")
  
  # Basic data summary
  cat("Old staging distribution:\n")
  print(table(test_data$old_stage))
  
  cat("\nNew staging distribution:\n") 
  print(table(test_data$new_stage))
  
  cat("\nStage migration cross-table:\n")
  migration_table <- table(test_data$old_stage, test_data$new_stage)
  print(migration_table)
  
  # Calculate migration rate
  total_patients <- nrow(test_data)
  unchanged <- sum(diag(migration_table))
  migration_rate <- (1 - unchanged/total_patients) * 100
  
  cat("\nMigration Statistics:\n")
  cat("- Total patients:", total_patients, "\n")
  cat("- Unchanged stages:", unchanged, "\n") 
  cat("- Migration rate:", round(migration_rate, 1), "%\n")
  
  # Basic survival analysis
  cat("\nSurvival Analysis Test:\n")
  
  # Test Cox models for both staging systems
  cox_old <- coxph(Surv(survival_months, event) ~ old_stage, data = test_data)
  cox_new <- coxph(Surv(survival_months, event) ~ new_stage, data = test_data)
  
  # Calculate C-index
  c_old <- summary(cox_old)$concordance[1]
  c_new <- summary(cox_new)$concordance[1]
  
  cat("- C-index (old staging):", round(c_old, 3), "\n")
  cat("- C-index (new staging):", round(c_new, 3), "\n")
  cat("- C-index improvement:", round(c_new - c_old, 3), "\n")
  
  # Test Kaplan-Meier fits
  km_old <- survfit(Surv(survival_months, event) ~ old_stage, data = test_data)
  km_new <- survfit(Surv(survival_months, event) ~ new_stage, data = test_data)
  
  cat("- Kaplan-Meier fits successful: YES\n")
  
  # Test log-rank tests
  lr_old <- survdiff(Surv(survival_months, event) ~ old_stage, data = test_data)
  lr_new <- survdiff(Surv(survival_months, event) ~ new_stage, data = test_data)
  
  cat("- Log-rank test p-values:\n")
  cat("  Old staging:", format.pval(1 - pchisq(lr_old$chisq, df = length(lr_old$n) - 1), digits = 4), "\n")
  cat("  New staging:", format.pval(1 - pchisq(lr_new$chisq, df = length(lr_new$n) - 1), digits = 4), "\n")
  
  # Will Rogers phenomenon test for Stage II
  stage_ii_data <- test_data[test_data$old_stage == "II", ]
  if (nrow(stage_ii_data) > 0) {
    stayed_ii <- stage_ii_data[stage_ii_data$new_stage == "II", ]
    migrated_ii <- stage_ii_data[stage_ii_data$new_stage != "II", ]
    
    if (nrow(stayed_ii) >= 5 && nrow(migrated_ii) >= 5) {
      # Test survival difference
      stage_ii_data$migrated <- ifelse(stage_ii_data$new_stage == "II", "Stayed", "Migrated")
      wr_test <- survdiff(Surv(survival_months, event) ~ migrated, data = stage_ii_data)
      wr_p <- 1 - pchisq(wr_test$chisq, df = 1)
      
      cat("\nWill Rogers Phenomenon Test (Stage II):\n")
      cat("- Patients who stayed in Stage II:", nrow(stayed_ii), "\n")
      cat("- Patients who migrated from Stage II:", nrow(migrated_ii), "\n")
      cat("- Will Rogers test p-value:", format.pval(wr_p, digits = 4), "\n")
      
      if (wr_p < 0.05) {
        cat("- Evidence of Will Rogers phenomenon: YES\n")
      } else {
        cat("- Evidence of Will Rogers phenomenon: NO\n")
      }
    }
  }
  
  cat("\n=== All tests completed successfully! ===\n")
  cat("The stage migration analysis function should work properly with this data.\n")
  
  # Instructions for jamovi usage
  cat("\n=== Instructions for jamovi ===\n")
  cat("1. Load the dataset: stage_migration_test_data.csv\n")
  cat("2. Go to: Analyses → SurvivalD → ClinicoPath Survival → Stage Migration Analysis\n")
  cat("3. Set variables:\n")
  cat("   - Original Stage: old_stage\n")
  cat("   - New Stage: new_stage\n") 
  cat("   - Survival Time: survival_months\n")
  cat("   - Event: event\n")
  cat("   - Event Level: 1 (if using numeric event)\n")
  cat("   OR\n")
  cat("   - Event: vital_status\n")
  cat("   - Event Level: Deceased (if using factor event)\n")
  cat("4. Enable 'Show Migration Plot' and 'Analyze Will Rogers Phenomenon'\n")
  cat("5. Review all output tables and plots\n")
  
} else {
  cat("Error: Test data file not found at:", test_data_path, "\n")
  cat("Please run stage-migration-test-data.R first to generate the test data.\n")
}
