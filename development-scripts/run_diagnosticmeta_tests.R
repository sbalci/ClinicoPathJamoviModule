#!/usr/bin/env Rscript

# ==============================================================================
# Comprehensive Automated Test Suite for diagnosticmeta Function
# ==============================================================================
#
# This script runs a comprehensive suite of automated tests for the
# diagnosticmeta function in the ClinicoPath jamovi module.
#
# Usage:
#   Rscript tests/run_diagnosticmeta_tests.R
#   OR from R console: source("tests/run_diagnosticmeta_tests.R")
#
# Requirements:
#   - ClinicoPath package installed
#   - mada package installed
#   - metafor package installed
#
# ==============================================================================

cat("╔═══════════════════════════════════════════════════════════════╗\n")
cat("║   diagnosticmeta Comprehensive Automated Test Suite          ║\n")
cat("╚═══════════════════════════════════════════════════════════════╝\n\n")

# --- Package Loading ---
cat("Loading required packages...\n")
required_packages <- c("ClinicoPath", "mada", "metafor", "testthat")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("❌ Package '%s' not found. Installing...\n", pkg))
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("✅ All required packages loaded\n\n")

# --- Test Configuration ---
VERBOSE <- TRUE
STOP_ON_ERROR <- FALSE

# Test counter
test_count <- 0
pass_count <- 0
fail_count <- 0
skip_count <- 0

# Test result storage
test_results <- data.frame(
  test_number = integer(),
  test_name = character(),
  status = character(),
  message = character(),
  duration = numeric(),
  stringsAsFactors = FALSE
)

# --- Helper Functions ---

run_test <- function(test_name, test_function, skip = FALSE) {
  test_count <<- test_count + 1

  cat(sprintf("\n[Test %d] %s\n", test_count, test_name))
  cat(strrep("─", 70), "\n")

  if (skip) {
    cat("⏭️  SKIPPED\n")
    skip_count <<- skip_count + 1
    test_results <<- rbind(test_results, data.frame(
      test_number = test_count,
      test_name = test_name,
      status = "SKIPPED",
      message = "Test skipped by configuration",
      duration = 0
    ))
    return(invisible(NULL))
  }

  start_time <- Sys.time()

  result <- tryCatch({
    test_function()
    list(status = "PASS", message = "Test passed successfully")
  }, error = function(e) {
    list(status = "FAIL", message = as.character(e$message))
  }, warning = function(w) {
    list(status = "WARNING", message = as.character(w$message))
  })

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Update counters
  if (result$status == "PASS") {
    pass_count <<- pass_count + 1
    cat("✅ PASSED")
  } else if (result$status == "FAIL") {
    fail_count <<- fail_count + 1
    cat("❌ FAILED")
  }

  cat(sprintf(" (%.2f seconds)\n", duration))

  if (VERBOSE && result$message != "Test passed successfully") {
    cat(sprintf("   Message: %s\n", result$message))
  }

  # Store result
  test_results <<- rbind(test_results, data.frame(
    test_number = test_count,
    test_name = test_name,
    status = result$status,
    message = result$message,
    duration = duration
  ))

  if (STOP_ON_ERROR && result$status == "FAIL") {
    stop("Test failed and STOP_ON_ERROR is TRUE")
  }

  invisible(result)
}

# --- Load Test Data ---
cat("Loading test datasets...\n")

# Load comprehensive test data
data_path_test <- "data/diagnostic_meta_test.csv"
data_path_example <- "data/diagnostic_meta_example.csv"

if (file.exists(data_path_test)) {
  test_data <- read.csv(data_path_test, stringsAsFactors = FALSE)
  cat(sprintf("✅ Loaded diagnostic_meta_test.csv (%d studies)\n", nrow(test_data)))
} else {
  stop(sprintf("❌ Test data not found: %s", data_path_test))
}

if (file.exists(data_path_example)) {
  example_data <- read.csv(data_path_example, stringsAsFactors = FALSE)
  cat(sprintf("✅ Loaded diagnostic_meta_example.csv (%d studies)\n", nrow(example_data)))
} else {
  cat(sprintf("⚠️  Example data not found: %s (non-critical)\n", data_path_example))
  example_data <- NULL
}

cat("\n")

# ==============================================================================
# TEST SUITE
# ==============================================================================

cat("Starting test execution...\n")
cat(strrep("═", 70), "\n")

# ------------------------------------------------------------------------------
# Test 1: Basic Bivariate Meta-Analysis
# ------------------------------------------------------------------------------

run_test("Basic Bivariate Meta-Analysis", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    bivariate_analysis = TRUE
  )

  # Validate result object
  testthat::expect_s3_class(result, "diagnosticmetaResults")

  # Validate bivariate results exist
  testthat::expect_true("bivariateresults" %in% names(result))
  testthat::expect_true(result$bivariateresults$rowCount >= 2)

  # Extract results as data frame
  biv_df <- result$bivariateresults$asDF

  # Validate pooled sensitivity
  sens_row <- biv_df[biv_df$parameter == "Pooled Sensitivity", ]
  testthat::expect_true(nrow(sens_row) == 1)
  testthat::expect_true(sens_row$estimate >= 0 && sens_row$estimate <= 100)

  # Validate pooled specificity
  spec_row <- biv_df[biv_df$parameter == "Pooled Specificity", ]
  testthat::expect_true(nrow(spec_row) == 1)
  testthat::expect_true(spec_row$estimate >= 0 && spec_row$estimate <= 100)

  if (VERBOSE) {
    cat(sprintf("   Pooled Sensitivity: %.2f%% [%.2f, %.2f]\n",
                sens_row$estimate, sens_row$ci_lower, sens_row$ci_upper))
    cat(sprintf("   Pooled Specificity: %.2f%% [%.2f, %.2f]\n",
                spec_row$estimate, spec_row$ci_lower, spec_row$ci_upper))
  }
})

# ------------------------------------------------------------------------------
# Test 2: HSROC Analysis
# ------------------------------------------------------------------------------

run_test("HSROC Analysis", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    hsroc_analysis = TRUE
  )

  testthat::expect_s3_class(result, "diagnosticmetaResults")
  testthat::expect_true("hsrocresults" %in% names(result))
  testthat::expect_true(result$hsrocresults$rowCount >= 1)

  if (VERBOSE) {
    cat(sprintf("   HSROC table rows: %d\n", result$hsrocresults$rowCount))
  }
})

# ------------------------------------------------------------------------------
# Test 3: Heterogeneity Analysis
# ------------------------------------------------------------------------------

run_test("Heterogeneity Analysis", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    bivariate_analysis = TRUE,
    heterogeneity_analysis = TRUE
  )

  testthat::expect_true("heterogeneity" %in% names(result))
  testthat::expect_true(result$heterogeneity$rowCount >= 1)

  het_df <- result$heterogeneity$asDF

  # Validate I² values are percentages (0-100)
  if ("i_squared" %in% names(het_df)) {
    testthat::expect_true(all(het_df$i_squared >= 0 & het_df$i_squared <= 100, na.rm = TRUE))
  }

  if (VERBOSE) {
    cat(sprintf("   Heterogeneity metrics: %d rows\n", nrow(het_df)))
  }
})

# ------------------------------------------------------------------------------
# Test 4: Publication Bias Assessment
# ------------------------------------------------------------------------------

run_test("Publication Bias Assessment (Deeks' Test)", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    publication_bias = TRUE
  )

  testthat::expect_true("publicationbias" %in% names(result))
  testthat::expect_true(result$publicationbias$rowCount >= 1)

  pub_df <- result$publicationbias$asDF

  # Validate p-value exists and is in valid range
  if ("p_value" %in% names(pub_df)) {
    testthat::expect_true(all(pub_df$p_value >= 0 & pub_df$p_value <= 1, na.rm = TRUE))
  }

  if (VERBOSE) {
    cat(sprintf("   Publication bias table rows: %d\n", nrow(pub_df)))
  }
})

# ------------------------------------------------------------------------------
# Test 5: Meta-Regression (Categorical Covariate)
# ------------------------------------------------------------------------------

run_test("Meta-Regression with Categorical Covariate", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    covariate = ai_algorithm,
    bivariate_analysis = TRUE,
    meta_regression = TRUE
  )

  testthat::expect_true("metaregression" %in% names(result))
  testthat::expect_true(result$metaregression$rowCount >= 1)

  if (VERBOSE) {
    cat(sprintf("   Meta-regression rows: %d\n", result$metaregression$rowCount))
  }
})

# ------------------------------------------------------------------------------
# Test 6: Meta-Regression (Continuous Covariate)
# ------------------------------------------------------------------------------

run_test("Meta-Regression with Continuous Covariate", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    covariate = publication_year,
    bivariate_analysis = TRUE,
    meta_regression = TRUE
  )

  testthat::expect_true("metaregression" %in% names(result))
  testthat::expect_true(result$metaregression$rowCount >= 1)

  if (VERBOSE) {
    cat(sprintf("   Meta-regression with continuous covariate: OK\n"))
  }
})

# ------------------------------------------------------------------------------
# Test 7: Forest Plot Generation
# ------------------------------------------------------------------------------

run_test("Forest Plot Generation", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    bivariate_analysis = TRUE,
    forest_plot = TRUE
  )

  testthat::expect_true("forestplot" %in% names(result))
  testthat::expect_false(is.null(result$forestplot$state))

  if (VERBOSE) {
    cat("   Forest plot generated successfully\n")
  }
})

# ------------------------------------------------------------------------------
# Test 8: Summary ROC Plot Generation
# ------------------------------------------------------------------------------

run_test("Summary ROC Plot Generation", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    bivariate_analysis = TRUE,
    sroc_plot = TRUE
  )

  testthat::expect_true("srocplot" %in% names(result))
  testthat::expect_false(is.null(result$srocplot$state))

  if (VERBOSE) {
    cat("   SROC plot generated successfully\n")
  }
})

# ------------------------------------------------------------------------------
# Test 9: Funnel Plot Generation
# ------------------------------------------------------------------------------

run_test("Funnel Plot Generation", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    publication_bias = TRUE,
    funnel_plot = TRUE
  )

  testthat::expect_true("funnelplot" %in% names(result))
  testthat::expect_false(is.null(result$funnelplot$state))

  if (VERBOSE) {
    cat("   Funnel plot generated successfully\n")
  }
})

# ------------------------------------------------------------------------------
# Test 10: Individual Study Results
# ------------------------------------------------------------------------------

run_test("Individual Study Results Table", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    bivariate_analysis = TRUE,
    show_individual_studies = TRUE
  )

  testthat::expect_true("individualstudies" %in% names(result))
  testthat::expect_equal(result$individualstudies$rowCount, nrow(test_data))

  if (VERBOSE) {
    cat(sprintf("   Individual study table: %d studies\n", result$individualstudies$rowCount))
  }
})

# ------------------------------------------------------------------------------
# Test 11: Color Palette Options
# ------------------------------------------------------------------------------

run_test("Color Palette Accessibility Options", function() {
  color_palettes <- c("standard", "colorblind_safe", "high_contrast", "viridis", "plasma")

  for (palette in color_palettes) {
    result <- diagnosticmeta(
      data = test_data,
      study = study_name,
      true_positives = true_positives,
      false_positives = false_positives,
      false_negatives = false_negatives,
      true_negatives = true_negatives,
      bivariate_analysis = TRUE,
      forest_plot = TRUE,
      color_palette = palette
    )

    testthat::expect_s3_class(result, "diagnosticmetaResults")
  }

  if (VERBOSE) {
    cat(sprintf("   Tested %d color palettes successfully\n", length(color_palettes)))
  }
})

# ------------------------------------------------------------------------------
# Test 12: Confidence Level Variation
# ------------------------------------------------------------------------------

run_test("Confidence Level Variation", function() {
  confidence_levels <- c(90, 95, 99)

  for (conf_level in confidence_levels) {
    result <- diagnosticmeta(
      data = test_data,
      study = study_name,
      true_positives = true_positives,
      false_positives = false_positives,
      false_negatives = false_negatives,
      true_negatives = true_negatives,
      bivariate_analysis = TRUE,
      confidence_level = conf_level
    )

    testthat::expect_s3_class(result, "diagnosticmetaResults")
  }

  if (VERBOSE) {
    cat(sprintf("   Tested %d confidence levels successfully\n", length(confidence_levels)))
  }
})

# ------------------------------------------------------------------------------
# Test 13: Estimation Methods
# ------------------------------------------------------------------------------

run_test("Different Estimation Methods", function() {
  methods <- c("reml", "ml", "fixed")

  for (method in methods) {
    result <- diagnosticmeta(
      data = test_data,
      study = study_name,
      true_positives = true_positives,
      false_positives = false_positives,
      false_negatives = false_negatives,
      true_negatives = true_negatives,
      bivariate_analysis = TRUE,
      method = method
    )

    testthat::expect_s3_class(result, "diagnosticmetaResults")
  }

  if (VERBOSE) {
    cat(sprintf("   Tested %d estimation methods successfully\n", length(methods)))
  }
})

# ------------------------------------------------------------------------------
# Test 14: Zero Cell Correction Methods
# ------------------------------------------------------------------------------

run_test("Zero Cell Correction Methods", function() {
  # Create data with zero cells
  test_data_zeros <- test_data
  test_data_zeros$true_positives[1] <- 0  # Introduce zero

  correction_methods <- c("none", "constant", "treatment_arm", "empirical")

  for (correction in correction_methods) {
    result <- diagnosticmeta(
      data = test_data_zeros,
      study = study_name,
      true_positives = true_positives,
      false_positives = false_positives,
      false_negatives = false_negatives,
      true_negatives = true_negatives,
      bivariate_analysis = TRUE,
      zero_cell_correction = correction
    )

    testthat::expect_s3_class(result, "diagnosticmetaResults")
  }

  if (VERBOSE) {
    cat(sprintf("   Tested %d zero-cell correction methods\n", length(correction_methods)))
  }
})

# ------------------------------------------------------------------------------
# Test 15: Comprehensive Analysis (All Options)
# ------------------------------------------------------------------------------

run_test("Comprehensive Analysis with All Options", function() {
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    covariate = ai_algorithm,
    bivariate_analysis = TRUE,
    hsroc_analysis = TRUE,
    meta_regression = TRUE,
    heterogeneity_analysis = TRUE,
    publication_bias = TRUE,
    forest_plot = TRUE,
    sroc_plot = TRUE,
    funnel_plot = TRUE,
    show_individual_studies = TRUE,
    show_interpretation = TRUE,
    show_methodology = TRUE,
    show_analysis_summary = TRUE
  )

  testthat::expect_s3_class(result, "diagnosticmetaResults")

  # Verify all requested outputs exist
  testthat::expect_true("bivariateresults" %in% names(result))
  testthat::expect_true("hsrocresults" %in% names(result))
  testthat::expect_true("metaregression" %in% names(result))
  testthat::expect_true("heterogeneity" %in% names(result))
  testthat::expect_true("publicationbias" %in% names(result))
  testthat::expect_true("forestplot" %in% names(result))
  testthat::expect_true("srocplot" %in% names(result))
  testthat::expect_true("funnelplot" %in% names(result))
  testthat::expect_true("individualstudies" %in% names(result))

  if (VERBOSE) {
    cat("   All comprehensive analysis outputs generated\n")
  }
})

# ------------------------------------------------------------------------------
# Test 16: Numerical Accuracy (Compare with mada package)
# ------------------------------------------------------------------------------

run_test("Numerical Accuracy vs mada Package", function() {
  # Run ClinicoPath analysis
  result <- diagnosticmeta(
    data = test_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    bivariate_analysis = TRUE
  )

  # Run mada directly
  mada_input <- data.frame(
    TP = test_data$true_positives,
    FP = test_data$false_positives,
    FN = test_data$false_negatives,
    TN = test_data$true_negatives
  )

  mada_fit <- mada::reitsma(mada_input, method = "reml")
  mada_summary <- summary(mada_fit, level = 0.95)
  coeff <- mada_summary[["coefficients"]]

  # Extract mada estimates
  pooled_sens_mada <- coeff["sensitivity", "Estimate"]
  pooled_spec_mada <- 1 - coeff["false pos. rate", "Estimate"]

  # Extract ClinicoPath estimates
  biv_df <- result$bivariateresults$asDF
  sens_row <- biv_df[biv_df$parameter == "Pooled Sensitivity", ]
  spec_row <- biv_df[biv_df$parameter == "Pooled Specificity", ]

  pooled_sens_cp <- sens_row$estimate / 100  # Convert percentage to proportion
  pooled_spec_cp <- spec_row$estimate / 100

  # Compare (tolerance of 0.1%)
  testthat::expect_equal(pooled_sens_cp, pooled_sens_mada, tolerance = 0.001)
  testthat::expect_equal(pooled_spec_cp, pooled_spec_mada, tolerance = 0.001)

  if (VERBOSE) {
    cat(sprintf("   ClinicoPath Sens: %.4f, mada Sens: %.4f (match: %.5f)\n",
                pooled_sens_cp, pooled_sens_mada, abs(pooled_sens_cp - pooled_sens_mada)))
    cat(sprintf("   ClinicoPath Spec: %.4f, mada Spec: %.4f (match: %.5f)\n",
                pooled_spec_cp, pooled_spec_mada, abs(pooled_spec_cp - pooled_spec_mada)))
  }
})

# ------------------------------------------------------------------------------
# Test 17: Edge Case - Minimum Studies
# ------------------------------------------------------------------------------

run_test("Edge Case: Minimum Studies (3 studies)", function() {
  minimal_data <- test_data[1:3, ]

  result <- diagnosticmeta(
    data = minimal_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    bivariate_analysis = TRUE
  )

  testthat::expect_s3_class(result, "diagnosticmetaResults")

  if (VERBOSE) {
    cat("   Analysis completed with minimum studies\n")
  }
})

# ------------------------------------------------------------------------------
# Test 18: Example Dataset
# ------------------------------------------------------------------------------

if (!is.null(example_data)) {
  run_test("Example Dataset (IHC Antibody Data)", function() {
    result <- diagnosticmeta(
      data = example_data,
      study = study_name,
      true_positives = true_positives,
      false_positives = false_positives,
      false_negatives = false_negatives,
      true_negatives = true_negatives,
      bivariate_analysis = TRUE,
      forest_plot = TRUE,
      sroc_plot = TRUE
    )

    testthat::expect_s3_class(result, "diagnosticmetaResults")
    testthat::expect_true(result$bivariateresults$rowCount >= 2)

    if (VERBOSE) {
      cat(sprintf("   Example dataset: %d studies analyzed\n", nrow(example_data)))
    }
  })
} else {
  run_test("Example Dataset (IHC Antibody Data)", function() {}, skip = TRUE)
}

# ==============================================================================
# TEST SUMMARY
# ==============================================================================

cat("\n")
cat(strrep("═", 70), "\n")
cat("TEST SUMMARY\n")
cat(strrep("═", 70), "\n\n")

cat(sprintf("Total Tests:  %d\n", test_count))
cat(sprintf("✅ Passed:     %d (%.1f%%)\n", pass_count, 100 * pass_count / test_count))
cat(sprintf("❌ Failed:     %d (%.1f%%)\n", fail_count, 100 * fail_count / test_count))
cat(sprintf("⏭️  Skipped:    %d (%.1f%%)\n", skip_count, 100 * skip_count / test_count))

total_duration <- sum(test_results$duration)
cat(sprintf("\nTotal Duration: %.2f seconds (%.2f minutes)\n",
            total_duration, total_duration / 60))

# Show failed tests if any
if (fail_count > 0) {
  cat("\n")
  cat(strrep("─", 70), "\n")
  cat("FAILED TESTS:\n")
  cat(strrep("─", 70), "\n")

  failed_tests <- test_results[test_results$status == "FAIL", ]
  for (i in 1:nrow(failed_tests)) {
    cat(sprintf("\n[%d] %s\n", failed_tests$test_number[i], failed_tests$test_name[i]))
    cat(sprintf("    Error: %s\n", failed_tests$message[i]))
  }
}

# Overall result
cat("\n")
if (fail_count == 0) {
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║                   🎉 ALL TESTS PASSED! 🎉                    ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n")
  exit_code <- 0
} else {
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║                   ⚠️  SOME TESTS FAILED  ⚠️                   ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n")
  exit_code <- 1
}

# Save test results to file
results_file <- sprintf("tests/results/diagnosticmeta_test_results_%s.csv",
                        format(Sys.time(), "%Y%m%d_%H%M%S"))

dir.create("tests/results", showWarnings = FALSE, recursive = TRUE)
write.csv(test_results, results_file, row.names = FALSE)
cat(sprintf("\n📊 Detailed results saved to: %s\n", results_file))

# Return exit code
quit(status = exit_code, save = "no")
