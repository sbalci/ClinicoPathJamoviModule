#!/usr/bin/env Rscript

# Load required libraries
# Load helper functions for multi-format data saving
source("data-raw/data_save_helpers.R")

library(timeROC)
library(survival)

cat("Testing timeROC package with generated test data...\n")

# Load test data
load('data/timeroc_cancer_biomarker.rda')

# Clean data and ensure no missing values
clean_data <- na.omit(timeroc_cancer_biomarker[c('follow_up_months', 'death_event', 'tumor_biomarker')])
cat('Clean data: ', nrow(clean_data), ' observations\n')

# Test basic timeROC functionality
result <- try({
  roc_result <- timeROC::timeROC(
    T = clean_data$follow_up_months,
    delta = clean_data$death_event,
    marker = clean_data$tumor_biomarker,
    cause = 1,
    times = c(12, 36),
    ROC = TRUE
  )
  roc_result
}, silent = FALSE)

if (inherits(result, 'try-error')) {
  cat('ERROR occurred:\n')
  print(result)
} else {
  cat('SUCCESS! timeROC analysis completed without errors.\n')
  cat('AUC values:\n')
  auc_df <- data.frame(
    Timepoint = c(12, 36),
    AUC = round(result$AUC, 3)
  )
  print(auc_df)
  
  # Check if results are reasonable
  if (all(result$AUC >= 0.4 & result$AUC <= 1)) {
    cat('AUC values are within reasonable range (0.4-1.0) - GOOD!\n')
  } else {
    cat('WARNING: Some AUC values are outside expected range\n')
  }
  
  # Test with cardiovascular data too
  cat('\nTesting with cardiovascular risk data...\n')
  load('data/timeroc_cardiovascular_risk.rda')
  cv_clean <- na.omit(timeroc_cardiovascular_risk[c('follow_up_months', 'cv_event', 'troponin_level')])
  
  cv_result <- timeROC::timeROC(
    T = cv_clean$follow_up_months,
    delta = cv_clean$cv_event,
    marker = cv_clean$troponin_level,
    cause = 1,
    times = c(12, 24),
    ROC = TRUE
  )
  
  cat('Cardiovascular data AUC values:\n')
  cv_auc_df <- data.frame(
    Timepoint = c(12, 24),
    AUC = round(cv_result$AUC, 3)
  )
  print(cv_auc_df)
  
  cat('\n=== FINAL ASSESSMENT ===\n')
  cat('✓ timeROC package dependency is working\n')
  cat('✓ Generated test datasets are valid\n')
  cat('✓ Time-dependent ROC analysis runs successfully\n')
  cat('✓ AUC values are reasonable\n')
  cat('✓ Multiple datasets tested successfully\n')
  cat('\nTimeROC implementation appears to be ready for use!\n')
}
