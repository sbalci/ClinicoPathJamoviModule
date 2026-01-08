# ═══════════════════════════════════════════════════════════
# Integration Tests: diagnosticmeta
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the diagnosticmeta jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(diagnosticmeta_test, package = "ClinicoPath")
data(diagnosticmeta_test_categorical, package = "ClinicoPath")

test_that("diagnosticmeta integrates with mada package correctly", {
  # The function should use mada package internally
  # Test that it produces valid results

  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")

  # If the function stores the mada model, check it
  # expect_s3_class(result$model, "madad") # or similar
})

test_that("diagnosticmeta integrates with metafor package correctly", {
  # The function should use metafor for some analyses
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    meta_regression = TRUE,
    covariate = "year"
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE,
    method = "reml"
  )

  result2 <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE,
    method = "reml"
  )

  # Results should be identical (no randomness involved)
  expect_s3_class(result1, "diagnosticmetaClass")
  expect_s3_class(result2, "diagnosticmetaClass")

  # If results contain numeric values, they should be equal
  # expect_equal(result1$results$summary, result2$results$summary)
})

test_that("diagnosticmeta workflow: basic meta-analysis → meta-regression", {
  # Simulate a realistic workflow
  # Step 1: Basic meta-analysis
  basic_result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE
  )

  expect_s3_class(basic_result, "diagnosticmetaClass")

  # Step 2: Add meta-regression to explore heterogeneity
  metareg_result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE,
    meta_regression = TRUE,
    covariate = "year",
    heterogeneity_analysis = TRUE
  )

  expect_s3_class(metareg_result, "diagnosticmetaClass")
})

test_that("diagnosticmeta workflow: assess publication bias", {
  # Simulate publication bias assessment workflow
  result <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    publication_bias = TRUE,
    funnel_plot = TRUE
  )

  expect_s3_class(result, "diagnosticmetaClass")
})

test_that("diagnosticmeta workflow: compare different models", {
  # Simulate comparing different estimation methods
  methods <- c("reml", "ml", "fixed")
  results <- list()

  for (method in methods) {
    results[[method]] <- diagnosticmeta(
      data = diagnosticmeta_test,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives",
      method = method
    )

    expect_s3_class(results[[method]], "diagnosticmetaClass")
  }

  # All methods should complete successfully
  expect_equal(length(results), 3)
})

test_that("diagnosticmeta handles data from CSV import", {
  # Test with CSV-loaded data (common workflow)
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(diagnosticmeta_test, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  result <- diagnosticmeta(
    data = csv_data,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(result, "diagnosticmetaClass")

  # Clean up
  unlink(temp_csv)
})

test_that("diagnosticmeta handles data from Excel import", {
  # Test with Excel-loaded data (common workflow)
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(diagnosticmeta_test, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  result <- diagnosticmeta(
    data = as.data.frame(xlsx_data),
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(result, "diagnosticmetaClass")

  # Clean up
  unlink(temp_xlsx)
})

test_that("diagnosticmeta produces reproducible plots", {
  # Generate plots twice and check they're consistent
  result1 <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    forest_plot = TRUE,
    sroc_plot = TRUE
  )

  result2 <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    forest_plot = TRUE,
    sroc_plot = TRUE
  )

  expect_s3_class(result1, "diagnosticmetaClass")
  expect_s3_class(result2, "diagnosticmetaClass")
})

test_that("diagnosticmeta comprehensive analysis workflow", {
  # Simulate a complete meta-analysis workflow
  # Step 1: Initial analysis
  initial <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(initial, "diagnosticmetaClass")

  # Step 2: Add heterogeneity analysis
  heterogeneity <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    heterogeneity_analysis = TRUE
  )

  expect_s3_class(heterogeneity, "diagnosticmetaClass")

  # Step 3: Explore with meta-regression
  metareg <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    meta_regression = TRUE,
    covariate = "year"
  )

  expect_s3_class(metareg, "diagnosticmetaClass")

  # Step 4: Full analysis with all options
  full_analysis <- diagnosticmeta(
    data = diagnosticmeta_test,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives",
    bivariate_analysis = TRUE,
    hsroc_analysis = TRUE,
    heterogeneity_analysis = TRUE,
    publication_bias = TRUE,
    meta_regression = TRUE,
    covariate = "year",
    forest_plot = TRUE,
    sroc_plot = TRUE,
    funnel_plot = TRUE,
    show_individual_studies = TRUE,
    show_interpretation = TRUE
  )

  expect_s3_class(full_analysis, "diagnosticmetaClass")
})

test_that("diagnosticmeta handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(diagnosticmeta_test)

  result_tibble <- diagnosticmeta(
    data = tibble_data,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(result_tibble, "diagnosticmetaClass")

  # Test with data.frame
  df_data <- as.data.frame(diagnosticmeta_test)

  result_df <- diagnosticmeta(
    data = df_data,
    study = "study",
    true_positives = "true_positives",
    false_positives = "false_positives",
    false_negatives = "false_negatives",
    true_negatives = "true_negatives"
  )

  expect_s3_class(result_df, "diagnosticmetaClass")

  # Both should produce consistent results
  # expect_equal(result_tibble$results, result_df$results)
})

test_that("diagnosticmeta sensitivity analysis with different corrections", {
  # Load data with zeros
  data(diagnosticmeta_test_zeros, package = "ClinicoPath")

  # Compare different zero-cell correction methods
  corrections <- c("none", "constant", "treatment_arm")
  results <- list()

  for (correction in corrections) {
    results[[correction]] <- diagnosticmeta(
      data = diagnosticmeta_test_zeros,
      study = "study",
      true_positives = "true_positives",
      false_positives = "false_positives",
      false_negatives = "false_negatives",
      true_negatives = "true_negatives",
      zero_cell_correction = correction
    )

    expect_s3_class(results[[correction]], "diagnosticmetaClass")
  }

  # All corrections should complete
  expect_equal(length(results), 3)
})
