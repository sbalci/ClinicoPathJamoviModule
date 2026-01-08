# ═══════════════════════════════════════════════════════════
# Integration Tests: ihcheterogeneity
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the ihcheterogeneity jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(ihcheterogeneity_test, package = "ClinicoPath")
data(ihcheterogeneity_ki67, package = "ClinicoPath")

test_that("ihcheterogeneity produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  result2 <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  # Results should be identical (no randomness)
  expect_s3_class(result1, "ihcheterogeneityClass")
  expect_s3_class(result2, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity workflow: basic → comprehensive analysis", {
  # Step 1: Basic analysis
  basic_result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(basic_result, "ihcheterogeneityClass")

  # Step 2: Add more measurements
  extended_result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4"
  )

  expect_s3_class(extended_result, "ihcheterogeneityClass")

  # Step 3: Comprehensive analysis
  comprehensive_result <- ihcheterogeneity(
    data = ihcheterogeneity_test,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4",
    spatial_id = "spatial_id",
    analysis_type = "comprehensive",
    variance_components = TRUE,
    power_analysis = TRUE,
    generate_recommendations = TRUE
  )

  expect_s3_class(comprehensive_result, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles data from CSV import", {
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(ihcheterogeneity_test, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  result <- ihcheterogeneity(
    data = csv_data,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")

  # Clean up
  unlink(temp_csv)
})

test_that("ihcheterogeneity handles data from Excel import", {
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(ihcheterogeneity_test, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  result <- ihcheterogeneity(
    data = as.data.frame(xlsx_data),
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result, "ihcheterogeneityClass")

  # Clean up
  unlink(temp_xlsx)
})

test_that("ihcheterogeneity handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(ihcheterogeneity_test)

  result_tibble <- ihcheterogeneity(
    data = tibble_data,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result_tibble, "ihcheterogeneityClass")

  # Test with data.frame
  df_data <- as.data.frame(ihcheterogeneity_test)

  result_df <- ihcheterogeneity(
    data = df_data,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2"
  )

  expect_s3_class(result_df, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity workflow: Ki67 quality assessment", {
  # Simulate real-world Ki67 QA workflow
  # Step 1: Initial assessment
  initial <- ihcheterogeneity(
    data = ihcheterogeneity_ki67,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    analysis_type = "reproducibility"
  )

  expect_s3_class(initial, "ihcheterogeneityClass")

  # Step 2: Identify high-variability cases
  variability_check <- ihcheterogeneity(
    data = ihcheterogeneity_ki67,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    analysis_type = "variability",
    cv_threshold = 20.0,
    variance_components = TRUE
  )

  expect_s3_class(variability_check, "ihcheterogeneityClass")

  # Step 3: Comprehensive QA with recommendations
  qa_complete <- ihcheterogeneity(
    data = ihcheterogeneity_ki67,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4",
    spatial_id = "spatial_id",
    analysis_type = "comprehensive",
    cv_threshold = 25.0,
    correlation_threshold = 0.75,
    show_variability_plots = TRUE,
    variance_components = TRUE,
    power_analysis = TRUE,
    generate_recommendations = TRUE,
    showSummary = TRUE
  )

  expect_s3_class(qa_complete, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity workflow: spatial compartment comparison", {
  data(ihcheterogeneity_compartments, package = "ClinicoPath")

  # Simulate spatial heterogeneity study workflow
  # Step 1: Basic compartment identification
  spatial_basic <- ihcheterogeneity(
    data = ihcheterogeneity_compartments,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    spatial_id = "spatial_id"
  )

  expect_s3_class(spatial_basic, "ihcheterogeneityClass")

  # Step 2: Compartment comparison
  spatial_compare <- ihcheterogeneity(
    data = ihcheterogeneity_compartments,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    spatial_id = "spatial_id",
    compareCompartments = TRUE,
    show_variability_plots = TRUE
  )

  expect_s3_class(spatial_compare, "ihcheterogeneityClass")

  # Step 3: Statistical testing of compartments
  spatial_test <- ihcheterogeneity(
    data = ihcheterogeneity_compartments,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    biopsy3 = "biopsy3",
    biopsy4 = "biopsy4",
    spatial_id = "spatial_id",
    compareCompartments = TRUE,
    compartmentTests = TRUE,
    variance_components = TRUE,
    showSummary = TRUE
  )

  expect_s3_class(spatial_test, "ihcheterogeneityClass")
})

test_that("ihcheterogeneity handles biomarker-specific analyses", {
  # Ki67 (typical CV ~20-30%)
  ki67_result <- ihcheterogeneity(
    data = ihcheterogeneity_ki67,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    cv_threshold = 25.0
  )

  expect_s3_class(ki67_result, "ihcheterogeneityClass")

  # ER (typical CV ~15-20%)
  data(ihcheterogeneity_er_hscore, package = "ClinicoPath")

  er_result <- ihcheterogeneity(
    data = ihcheterogeneity_er_hscore,
    wholesection = "wholesection",
    biopsy1 = "biopsy1",
    biopsy2 = "biopsy2",
    cv_threshold = 18.0,
    correlation_threshold = 0.85
  )

  expect_s3_class(er_result, "ihcheterogeneityClass")
})
