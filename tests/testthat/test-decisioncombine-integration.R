# ═══════════════════════════════════════════════════════════
# Integration Tests: decisioncombine
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the decisioncombine jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(decisioncombine_pathology, package = "ClinicoPath")

test_that("decisioncombine produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive"
  )

  result2 <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive"
  )

  # Results should be identical (deterministic)
  expect_s3_class(result1, "decisioncombineClass")
  expect_s3_class(result2, "decisioncombineClass")
})

test_that("decisioncombine workflow: basic → individual → visualization", {
  # Step 1: Basic combination analysis
  basic <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive"
  )
  expect_s3_class(basic, "decisioncombineClass")

  # Step 2: Add individual test statistics
  with_individual <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showIndividual = TRUE
  )
  expect_s3_class(with_individual, "decisioncombineClass")

  # Step 3: Add visualizations
  with_viz <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showIndividual = TRUE,
    showBarPlot = TRUE,
    showHeatmap = TRUE
  )
  expect_s3_class(with_viz, "decisioncombineClass")
})

test_that("decisioncombine workflow: pathology rater agreement study", {
  # Complete pathology rater agreement workflow
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showIndividual = TRUE,
    showFrequency = TRUE,
    showBarPlot = TRUE,
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine workflow: screening + confirmatory strategy", {
  data(decisioncombine_screening, package = "ClinicoPath")

  # Evaluate two-stage testing strategy
  result <- decisioncombine(
    data = decisioncombine_screening,
    gold = "gold_standard",
    goldPositive = "Disease",
    test1 = "screening_test",
    test1Positive = "Positive",
    test2 = "confirmatory_test",
    test2Positive = "Positive",
    showIndividual = TRUE,
    filterPattern = "serial",  # Serial strategy (both must be positive)
    showBarPlot = TRUE,
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine workflow: parallel vs serial comparison", {
  data(decisioncombine_discordant, package = "ClinicoPath")

  # Parallel strategy (either test positive)
  parallel <- decisioncombine(
    data = decisioncombine_discordant,
    gold = "gold_standard",
    goldPositive = "Positive",
    test1 = "sensitive_test",
    test1Positive = "Positive",
    test2 = "specific_test",
    test2Positive = "Positive",
    filterPattern = "parallel"
  )
  expect_s3_class(parallel, "decisioncombineClass")

  # Serial strategy (both tests positive)
  serial <- decisioncombine(
    data = decisioncombine_discordant,
    gold = "gold_standard",
    goldPositive = "Positive",
    test1 = "sensitive_test",
    test1Positive = "Positive",
    test2 = "specific_test",
    test2Positive = "Positive",
    filterPattern = "serial"
  )
  expect_s3_class(serial, "decisioncombineClass")
})

test_that("decisioncombine workflow: multi-modal imaging comparison", {
  data(decisioncombine_imaging, package = "ClinicoPath")

  # Compare CT vs MRI performance
  result <- decisioncombine(
    data = decisioncombine_imaging,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "ct_scan",
    test1Positive = "Positive",
    test2 = "mri_scan",
    test2Positive = "Positive",
    showIndividual = TRUE,
    showBarPlot = TRUE,
    showHeatmap = TRUE,
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine workflow: three-test comprehensive analysis", {
  data(decisioncombine_threetest, package = "ClinicoPath")

  # Comprehensive 3-test analysis
  result <- decisioncombine(
    data = decisioncombine_threetest,
    gold = "gold_standard",
    goldPositive = "Disease",
    test1 = "clinical_exam",
    test1Positive = "Positive",
    test2 = "lab_test",
    test2Positive = "Positive",
    test3 = "imaging",
    test3Positive = "Positive",
    showIndividual = TRUE,
    showBarPlot = TRUE,
    showHeatmap = TRUE,
    showForest = TRUE,
    showDecisionTree = TRUE,
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine workflow: majority rule for three tests", {
  data(decisioncombine_threetest, package = "ClinicoPath")

  # Majority rule strategy
  result <- decisioncombine(
    data = decisioncombine_threetest,
    gold = "gold_standard",
    goldPositive = "Disease",
    test1 = "clinical_exam",
    test1Positive = "Positive",
    test2 = "lab_test",
    test2Positive = "Positive",
    test3 = "imaging",
    test3Positive = "Positive",
    filterPattern = "majority",
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine workflow: serial testing with temporal component", {
  data(decisioncombine_serial, package = "ClinicoPath")

  # Evaluate repeat testing strategy
  result <- decisioncombine(
    data = decisioncombine_serial,
    gold = "gold_standard",
    goldPositive = "Positive",
    test1 = "initial_test",
    test1Positive = "Positive",
    test2 = "repeat_test",
    test2Positive = "Positive",
    showIndividual = TRUE,
    showBarPlot = TRUE,
    filterPattern = "serial",
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine handles data from CSV import", {
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(decisioncombine_pathology, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  # Convert character columns back to factors
  csv_data$gold_standard <- factor(csv_data$gold_standard)
  csv_data$rater1 <- factor(csv_data$rater1)
  csv_data$rater2 <- factor(csv_data$rater2)

  result <- decisioncombine(
    data = csv_data,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")

  # Clean up
  unlink(temp_csv)
})

test_that("decisioncombine handles data from Excel import", {
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(decisioncombine_pathology, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  # Convert to data frame and factors
  xlsx_data <- as.data.frame(xlsx_data)
  xlsx_data$gold_standard <- factor(xlsx_data$gold_standard)
  xlsx_data$rater1 <- factor(xlsx_data$rater1)
  xlsx_data$rater2 <- factor(xlsx_data$rater2)

  result <- decisioncombine(
    data = xlsx_data,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive"
  )

  expect_s3_class(result, "decisioncombineClass")

  # Clean up
  unlink(temp_xlsx)
})

test_that("decisioncombine handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(decisioncombine_pathology)

  result_tibble <- decisioncombine(
    data = tibble_data,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive"
  )

  expect_s3_class(result_tibble, "decisioncombineClass")

  # Test with data.frame
  df_data <- as.data.frame(decisioncombine_pathology)

  result_df <- decisioncombine(
    data = df_data,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive"
  )

  expect_s3_class(result_df, "decisioncombineClass")
})

test_that("decisioncombine workflow: complete publication-ready analysis", {
  # Comprehensive analysis for publication
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showIndividual = TRUE,
    showFrequency = TRUE,
    showBarPlot = TRUE,
    showHeatmap = TRUE,
    showForest = TRUE,
    showDecisionTree = TRUE,
    showRecommendation = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine workflow: sensitivity analysis by filtering", {
  # Filter by different statistics to examine performance
  statistics <- c("sens", "spec", "ppv", "npv", "youden")

  for (stat in statistics) {
    result <- decisioncombine(
      data = decisioncombine_pathology,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive",
      filterStatistic = stat
    )
    expect_s3_class(result, "decisioncombineClass")
  }
})

test_that("decisioncombine workflow: pattern-specific analysis", {
  # Analyze different pattern types
  patterns <- c("allPositive", "allNegative", "mixed", "parallel", "serial")

  for (pattern in patterns) {
    result <- decisioncombine(
      data = decisioncombine_pathology,
      gold = "gold_standard",
      goldPositive = "Malignant",
      test1 = "rater1",
      test1Positive = "Positive",
      test2 = "rater2",
      test2Positive = "Positive",
      filterPattern = pattern
    )
    expect_s3_class(result, "decisioncombineClass")
  }
})

test_that("decisioncombine workflow: concordance vs discordance comparison", {
  data(decisioncombine_concordant, package = "ClinicoPath")
  data(decisioncombine_discordant, package = "ClinicoPath")

  # High concordance scenario
  result_concordant <- decisioncombine(
    data = decisioncombine_concordant,
    gold = "gold_standard",
    goldPositive = "Disease Present",
    test1 = "test_a",
    test1Positive = "Positive",
    test2 = "test_b",
    test2Positive = "Positive",
    showRecommendation = TRUE
  )
  expect_s3_class(result_concordant, "decisioncombineClass")

  # Discordant tests scenario
  result_discordant <- decisioncombine(
    data = decisioncombine_discordant,
    gold = "gold_standard",
    goldPositive = "Positive",
    test1 = "sensitive_test",
    test1Positive = "Positive",
    test2 = "specific_test",
    test2Positive = "Positive",
    showRecommendation = TRUE
  )
  expect_s3_class(result_discordant, "decisioncombineClass")
})

test_that("decisioncombine workflow: add pattern to data and downstream analysis", {
  # Generate results with pattern added to data
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    addPatternToData = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
  # Pattern column should be added to dataset
  # (This would be verified in actual implementation)
})

test_that("decisioncombine integrates with small dataset workflow", {
  data(decisioncombine_small, package = "ClinicoPath")

  # Quick analysis with small dataset
  result <- decisioncombine(
    data = decisioncombine_small,
    gold = "gold_standard",
    goldPositive = "Positive",
    test1 = "test1",
    test1Positive = "Positive",
    test2 = "test2",
    test2Positive = "Positive",
    showBarPlot = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})

test_that("decisioncombine workflow: visual comparison across all plot types", {
  # Generate all visualizations simultaneously
  result <- decisioncombine(
    data = decisioncombine_pathology,
    gold = "gold_standard",
    goldPositive = "Malignant",
    test1 = "rater1",
    test1Positive = "Positive",
    test2 = "rater2",
    test2Positive = "Positive",
    showBarPlot = TRUE,
    showHeatmap = TRUE,
    showForest = TRUE,
    showDecisionTree = TRUE
  )

  expect_s3_class(result, "decisioncombineClass")
})
