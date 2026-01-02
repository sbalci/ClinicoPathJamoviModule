# =============================================================================
# COMPREHENSIVE SYSTEMATIC TESTING OF ALL PSYCHOPDAROC ARGUMENTS
# =============================================================================
# Test Date: 2026-01-01
# Purpose: Systematically test each argument to identify and resolve errors
#
# This script tests all 80+ arguments in the psychopdaROC function

library(testthat)

# Source required files
if (!exists(".")) . <- function(x, ...) x
source("R/utils.R", chdir = TRUE)
source("R/psychopdaROC_utilities.R", chdir = TRUE)
source("R/psychopdaROC.h.R", chdir = TRUE)
source("R/psychopdaROC.b.R", chdir = TRUE)

# =============================================================================
# TEST DATA CREATION
# =============================================================================

create_comprehensive_test_data <- function(n = 200, seed = 123, include_na = TRUE) {
  set.seed(seed)

  # Create binary outcome
  outcome <- sample(c("Disease", "Healthy"), n, replace = TRUE, prob = c(0.3, 0.7))

  # Create continuous test variables with different discriminatory abilities
  # Excellent discriminator (AUC ~0.85)
  test1 <- ifelse(outcome == "Disease",
                  rnorm(sum(outcome == "Disease"), mean = 3, sd = 1),
                  rnorm(sum(outcome == "Healthy"), mean = 1, sd = 1))

  # Good discriminator (AUC ~0.75)
  test2 <- ifelse(outcome == "Disease",
                  rnorm(sum(outcome == "Disease"), mean = 2.5, sd = 1.2),
                  rnorm(sum(outcome == "Healthy"), mean = 1.5, sd = 1.2))

  # Moderate discriminator (AUC ~0.65)
  test3 <- ifelse(outcome == "Disease",
                  rnorm(sum(outcome == "Disease"), mean = 2, sd = 1.5),
                  rnorm(sum(outcome == "Healthy"), mean = 1.8, sd = 1.5))

  # Poor discriminator (AUC ~0.55)
  test4 <- rnorm(n, mean = 2, sd = 1)

  # Subgroup variable
  subgroup <- sample(c("Group1", "Group2"), n, replace = TRUE)

  # Study variable for meta-analysis
  study <- sample(c("Study1", "Study2", "Study3"), n, replace = TRUE)

  data <- data.frame(
    outcome = factor(outcome, levels = c("Healthy", "Disease")),
    test1 = test1,
    test2 = test2,
    test3 = test3,
    test4 = test4,
    subgroup = factor(subgroup),
    study = factor(study),
    stringsAsFactors = FALSE
  )

  # Add some NA values if requested
  if (include_na) {
    # Add NAs to outcome (5%)
    na_indices <- sample(1:n, size = floor(n * 0.05))
    data$outcome[na_indices] <- NA

    # Add NAs to test variables (2%)
    for (var in c("test1", "test2", "test3")) {
      na_indices <- sample(1:n, size = floor(n * 0.02))
      data[[var]][na_indices] <- NA
    }
  }

  return(data)
}

# Create test datasets
cat("\n=== Creating Test Data ===\n")
data_with_na <- create_comprehensive_test_data(n = 200, include_na = TRUE)
data_clean <- create_comprehensive_test_data(n = 200, include_na = FALSE)

cat(sprintf("Test data created:\n"))
cat(sprintf("  - Total rows: %d\n", nrow(data_with_na)))
cat(sprintf("  - NA in outcome: %d\n", sum(is.na(data_with_na$outcome))))
cat(sprintf("  - Disease prevalence: %.1f%%\n",
            mean(data_clean$outcome == "Disease", na.rm = TRUE) * 100))

# =============================================================================
# TEST 1: BASIC DATA INPUT ARGUMENTS
# =============================================================================

cat("\n=== TEST 1: Basic Data Input Arguments ===\n")

test_results <- list()

# Test 1.1: Single dependent variable
cat("  Test 1.1: Single dependent variable... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease"
  )
  cat("✓ PASS\n")
  test_results$test_1_1 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_1_1 <<- paste("FAIL:", e$message)
})

# Test 1.2: Multiple dependent variables
cat("  Test 1.2: Multiple dependent variables... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = c("test1", "test2", "test3"),
    classVar = "outcome",
    positiveClass = "Disease"
  )
  cat("✓ PASS\n")
  test_results$test_1_2 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_1_2 <<- paste("FAIL:", e$message)
})

# Test 1.3: Data with NA values (should handle gracefully)
cat("  Test 1.3: Data with NA values... ")
tryCatch({
  result <- psychopdaROC(
    data = data_with_na,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease"
  )
  cat("✓ PASS\n")
  test_results$test_1_3 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_1_3 <<- paste("FAIL:", e$message)
})

# Test 1.4: Subgroup analysis
cat("  Test 1.4: Subgroup analysis... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease",
    subGroup = "subgroup"
  )
  cat("✓ PASS\n")
  test_results$test_1_4 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_1_4 <<- paste("FAIL:", e$message)
})

# =============================================================================
# TEST 2: CUTPOINT METHODS
# =============================================================================

cat("\n=== TEST 2: Cutpoint Optimization Methods ===\n")

cutpoint_methods <- c(
  "maximize_metric",
  "minimize_metric",
  "maximize_loess_metric",
  "minimize_loess_metric",
  "maximize_spline_metric",
  "minimize_spline_metric",
  "maximize_boot_metric",
  "minimize_boot_metric",
  "oc_youden_kernel",
  "oc_youden_normal",
  "oc_manual",
  "oc_cost_ratio",
  "oc_equal_sens_spec",
  "oc_closest_01"
)

for (i in seq_along(cutpoint_methods)) {
  method <- cutpoint_methods[i]
  cat(sprintf("  Test 2.%d: Method '%s'... ", i, method))

  tryCatch({
    if (method == "oc_manual") {
      result <- psychopdaROC(
        data = data_clean,
        dependentVars = "test1",
        classVar = "outcome",
        positiveClass = "Disease",
        method = method,
        specifyCutScore = "2.0"
      )
    } else {
      result <- psychopdaROC(
        data = data_clean,
        dependentVars = "test1",
        classVar = "outcome",
        positiveClass = "Disease",
        method = method
      )
    }
    cat("✓ PASS\n")
    test_results[[paste0("test_2_", i)]] <- "PASS"
  }, error = function(e) {
    cat(sprintf("✗ FAIL: %s\n", e$message))
    test_results[[paste0("test_2_", i)]] <<- paste("FAIL:", e$message)
  })
}

# =============================================================================
# TEST 3: OPTIMIZATION METRICS
# =============================================================================

cat("\n=== TEST 3: Optimization Metrics ===\n")

metrics <- c(
  "youden",
  "accuracy",
  "F1_score",
  "cohens_kappa",
  "sum_sens_spec",
  "prod_sens_spec",
  "abs_d_sens_spec",
  "roc01",
  "ij_cutpoint",
  "misclassification_cost",
  "cost_loss_accuracy",
  "brier_score"
)

for (i in seq_along(metrics)) {
  metric <- metrics[i]
  cat(sprintf("  Test 3.%d: Metric '%s'... ", i, metric))

  tryCatch({
    result <- psychopdaROC(
      data = data_clean,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      method = "maximize_metric",
      metric = metric
    )
    cat("✓ PASS\n")
    test_results[[paste0("test_3_", i)]] <- "PASS"
  }, error = function(e) {
    cat(sprintf("✗ FAIL: %s\n", e$message))
    test_results[[paste0("test_3_", i)]] <<- paste("FAIL:", e$message)
  })
}

# =============================================================================
# TEST 4: STATISTICAL COMPARISON OPTIONS
# =============================================================================

cat("\n=== TEST 4: Statistical Comparison Options ===\n")

# Test 4.1: DeLong test
cat("  Test 4.1: DeLong test... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = c("test1", "test2"),
    classVar = "outcome",
    positiveClass = "Disease",
    delongTest = TRUE
  )
  cat("✓ PASS\n")
  test_results$test_4_1 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_4_1 <<- paste("FAIL:", e$message)
})

# Test 4.2: DeLong test with NA data
cat("  Test 4.2: DeLong test with NA data... ")
tryCatch({
  result <- psychopdaROC(
    data = data_with_na,
    dependentVars = c("test1", "test2"),
    classVar = "outcome",
    positiveClass = "Disease",
    delongTest = TRUE
  )
  cat("✓ PASS\n")
  test_results$test_4_2 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_4_2 <<- paste("FAIL:", e$message)
})

# Test 4.3: IDI calculation
cat("  Test 4.3: IDI calculation... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = c("test1", "test2"),
    classVar = "outcome",
    positiveClass = "Disease",
    calculateIDI = TRUE,
    refVar = "test1",
    idiNriBootRuns = 50
  )
  cat("✓ PASS\n")
  test_results$test_4_3 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_4_3 <<- paste("FAIL:", e$message)
})

# Test 4.4: NRI calculation
cat("  Test 4.4: NRI calculation... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = c("test1", "test2"),
    classVar = "outcome",
    positiveClass = "Disease",
    calculateNRI = TRUE,
    refVar = "test1",
    nriThresholds = "0.3,0.7",
    idiNriBootRuns = 50
  )
  cat("✓ PASS\n")
  test_results$test_4_4 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_4_4 <<- paste("FAIL:", e$message)
})

# =============================================================================
# TEST 5: PLOTTING OPTIONS
# =============================================================================

cat("\n=== TEST 5: Plotting Options ===\n")

# Test 5.1: Basic ROC plot
cat("  Test 5.1: Basic ROC plot... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease",
    plotROC = TRUE
  )
  cat("✓ PASS\n")
  test_results$test_5_1 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_5_1 <<- paste("FAIL:", e$message)
})

# Test 5.2: Combined plots
cat("  Test 5.2: Combined plots... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = c("test1", "test2"),
    classVar = "outcome",
    positiveClass = "Disease",
    plotROC = TRUE,
    combinePlots = TRUE
  )
  cat("✓ PASS\n")
  test_results$test_5_2 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_5_2 <<- paste("FAIL:", e$message)
})

# Test 5.3: Criterion plot
cat("  Test 5.3: Criterion plot... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease",
    showCriterionPlot = TRUE
  )
  cat("✓ PASS\n")
  test_results$test_5_3 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_5_3 <<- paste("FAIL:", e$message)
})

# Test 5.4: Precision-Recall curve
cat("  Test 5.4: Precision-Recall curve... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease",
    precisionRecallCurve = TRUE
  )
  cat("✓ PASS\n")
  test_results$test_5_4 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_5_4 <<- paste("FAIL:", e$message)
})

# =============================================================================
# TEST 6: ADVANCED FEATURES
# =============================================================================

cat("\n=== TEST 6: Advanced Features ===\n")

# Test 6.1: Partial AUC
cat("  Test 6.1: Partial AUC... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease",
    partialAUC = TRUE,
    partialAUCfrom = 0.8,
    partialAUCto = 1.0
  )
  cat("✓ PASS\n")
  test_results$test_6_1 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_6_1 <<- paste("FAIL:", e$message)
})

# Test 6.2: Bootstrap CI
cat("  Test 6.2: Bootstrap confidence intervals... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease",
    bootstrapCI = TRUE,
    bootstrapReps = 50
  )
  cat("✓ PASS\n")
  test_results$test_6_2 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_6_2 <<- paste("FAIL:", e$message)
})

# Test 6.3: Effect size analysis
cat("  Test 6.3: Effect size analysis... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = c("test1", "test2"),
    classVar = "outcome",
    positiveClass = "Disease",
    effectSizeAnalysis = TRUE
  )
  cat("✓ PASS\n")
  test_results$test_6_3 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_6_3 <<- paste("FAIL:", e$message)
})

# Test 6.4: Power analysis
cat("  Test 6.4: Power analysis... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = c("test1", "test2"),
    classVar = "outcome",
    positiveClass = "Disease",
    powerAnalysis = TRUE,
    expectedAUCDifference = 0.1,
    targetPower = 0.8
  )
  cat("✓ PASS\n")
  test_results$test_6_4 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_6_4 <<- paste("FAIL:", e$message)
})

# =============================================================================
# TEST 7: FIXED SENSITIVITY/SPECIFICITY ANALYSIS
# =============================================================================

cat("\n=== TEST 7: Fixed Sensitivity/Specificity Analysis ===\n")

# Test 7.1: Fixed sensitivity
cat("  Test 7.1: Fixed sensitivity analysis... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease",
    fixedSensSpecAnalysis = TRUE,
    fixedAnalysisType = "sensitivity",
    fixedSensitivityValue = 0.9
  )
  cat("✓ PASS\n")
  test_results$test_7_1 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_7_1 <<- paste("FAIL:", e$message)
})

# Test 7.2: Fixed specificity
cat("  Test 7.2: Fixed specificity analysis... ")
tryCatch({
  result <- psychopdaROC(
    data = data_clean,
    dependentVars = "test1",
    classVar = "outcome",
    positiveClass = "Disease",
    fixedSensSpecAnalysis = TRUE,
    fixedAnalysisType = "specificity",
    fixedSpecificityValue = 0.9
  )
  cat("✓ PASS\n")
  test_results$test_7_2 <- "PASS"
}, error = function(e) {
  cat(sprintf("✗ FAIL: %s\n", e$message))
  test_results$test_7_2 <<- paste("FAIL:", e$message)
})

# Test 7.3: Different interpolation methods
interpolation_methods <- c("linear", "nearest", "stepwise")
for (i in seq_along(interpolation_methods)) {
  interp_method <- interpolation_methods[i]
  cat(sprintf("  Test 7.%d: Interpolation '%s'... ", i + 2, interp_method))

  tryCatch({
    result <- psychopdaROC(
      data = data_clean,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      fixedSensSpecAnalysis = TRUE,
      fixedAnalysisType = "sensitivity",
      fixedSensitivityValue = 0.85,
      fixedInterpolation = interp_method
    )
    cat("✓ PASS\n")
    test_results[[paste0("test_7_", i + 2)]] <- "PASS"
  }, error = function(e) {
    cat(sprintf("✗ FAIL: %s\n", e$message))
    test_results[[paste0("test_7_", i + 2)]] <<- paste("FAIL:", e$message)
  })
}

# =============================================================================
# TEST 8: CLINICAL MODE AND PRESETS
# =============================================================================

cat("\n=== TEST 8: Clinical Modes and Presets ===\n")

# Test 8.1: Clinical modes
clinical_modes <- c("basic", "advanced", "comprehensive")
for (i in seq_along(clinical_modes)) {
  mode <- clinical_modes[i]
  cat(sprintf("  Test 8.%d: Clinical mode '%s'... ", i, mode))

  tryCatch({
    result <- psychopdaROC(
      data = data_clean,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      clinicalMode = mode
    )
    cat("✓ PASS\n")
    test_results[[paste0("test_8_", i)]] <- "PASS"
  }, error = function(e) {
    cat(sprintf("✗ FAIL: %s\n", e$message))
    test_results[[paste0("test_8_", i)]] <<- paste("FAIL:", e$message)
  })
}

# Test 8.4: Clinical presets
clinical_presets <- c("screening", "confirmation", "balanced", "research")
for (i in seq_along(clinical_presets)) {
  preset <- clinical_presets[i]
  cat(sprintf("  Test 8.%d: Clinical preset '%s'... ", i + 3, preset))

  tryCatch({
    result <- psychopdaROC(
      data = data_clean,
      dependentVars = "test1",
      classVar = "outcome",
      positiveClass = "Disease",
      clinicalPreset = preset
    )
    cat("✓ PASS\n")
    test_results[[paste0("test_8_", i + 3)]] <- "PASS"
  }, error = function(e) {
    cat(sprintf("✗ FAIL: %s\n", e$message))
    test_results[[paste0("test_8_", i + 3)]] <<- paste("FAIL:", e$message)
  })
}

# =============================================================================
# TEST SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("TEST SUMMARY\n")
cat(paste(rep("=", 80), collapse=""), "\n")

pass_count <- sum(grepl("^PASS$", test_results))
fail_count <- sum(grepl("^FAIL", test_results))
total_count <- length(test_results)

cat(sprintf("\nTotal tests: %d\n", total_count))
cat(sprintf("Passed: %d (%.1f%%)\n", pass_count, pass_count/total_count*100))
cat(sprintf("Failed: %d (%.1f%%)\n", fail_count, fail_count/total_count*100))

if (fail_count > 0) {
  cat("\n=== FAILED TESTS ===\n")
  for (test_name in names(test_results)) {
    if (grepl("^FAIL", test_results[[test_name]])) {
      cat(sprintf("\n%s:\n  %s\n", test_name, test_results[[test_name]]))
    }
  }
}

cat("\n", paste(rep("=", 80), collapse=""), "\n")

# Return test results
invisible(test_results)
