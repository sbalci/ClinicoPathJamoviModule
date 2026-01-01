context("Medical Decision Analysis")

# Test utility functions
test_that("sensitivity calculation is correct", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  expect_equal(calculate_sensitivity(tp = 90, fn = 10), 0.9)
  expect_equal(calculate_sensitivity(tp = 0, fn = 10), 0)
  expect_equal(calculate_sensitivity(tp = 50, fn = 50), 0.5)
  expect_true(is.na(calculate_sensitivity(tp = NA, fn = 1)))
})

test_that("specificity calculation is correct", {
  expect_equal(calculate_specificity(tn = 80, fp = 20), 0.8)
  expect_equal(calculate_specificity(tn = 0, fp = 20), 0)
  expect_equal(calculate_specificity(tn = 50, fp = 50), 0.5)
  expect_true(is.na(calculate_specificity(tn = 1, fp = NA)))
})

test_that("AUC calculation is correct", {
  expect_equal(calculate_auc(sens = 0.9, spec = 0.8), 0.53)
  expect_true(calculate_auc(sens = 1, spec = 1) <= 1)
  expect_true(calculate_auc(sens = 0, spec = 0) >= 0)
  expect_true(is.na(calculate_auc(NA, 0.5)))
})

# Test main decision() function
test_that("decision function works with basic parameters", {
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Test basic function call without errors
  expect_no_error({
    result <- decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1"
    )
  })
  
  # Test that function returns expected structure
  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1"
  )
  
  expect_s3_class(result, "Group")
  expect_true(length(result) > 0)
  
  # Test that required output tables exist
  expect_true("cTable" %in% names(result))
  expect_true("nTable" %in% names(result))
  expect_true("ratioTable" %in% names(result))
})

test_that("decision function works with confidence intervals", {
  data("histopathology", package = "ClinicoPath")
  
  # Test with confidence intervals enabled
  expect_no_error({
    result <- decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      ci = TRUE
    )
  })
  
  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1",
    ci = TRUE
  )
  
  # Check that CI tables are included
  expect_true("epirTable_ratio" %in% names(result))
  expect_true("epirTable_number" %in% names(result))
})

test_that("decision function works with prior probability", {
  data("histopathology", package = "ClinicoPath")
  
  # Test with prior probability
  expect_no_error({
    result <- decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      pp = TRUE,
      pprob = 0.05
    )
  })
})

test_that("decision function works with Fagan nomogram", {
  data("histopathology", package = "ClinicoPath")
  
  # Test with Fagan nomogram
  expect_no_error({
    result <- decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      fagan = TRUE
    )
  })
  
  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1",
    fagan = TRUE
  )
  
  # Check that plot is included
  expect_true("plot1" %in% names(result))
})

test_that("decision function works with alternative datasets", {
  # Test with breast cancer data
  data("breast_cancer_data", package = "ClinicoPath")
  
  expect_no_error({
    result <- decision(
      data = breast_cancer_data,
      gold = "cancer_status",
      goldPositive = "1",
      newtest = "mammography",
      testPositive = "1"
    )
  })
  
  # Test with COVID screening data
  data("covid_screening_data", package = "ClinicoPath")
  
  expect_no_error({
    result <- decision(
      data = covid_screening_data,
      gold = "covid_status",
      goldPositive = "1",
      newtest = "rapid_antigen",
      testPositive = "1"
    )
  })
})

test_that("decision function validates parameters correctly", {
  data("histopathology", package = "ClinicoPath")
  
  # Test invalid prior probability
  expect_error({
    decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      pp = TRUE,
      pprob = 1.5  # Invalid: > 1
    )
  })
  
  expect_error({
    decision(
      data = histopathology,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1",
      pp = TRUE,
      pprob = -0.1  # Invalid: < 0
    )
  })
})

test_that("decision function handles missing data appropriately", {
  data("histopathology", package = "ClinicoPath")
  
  # Create dataset with some missing values
  test_data <- histopathology
  test_data[1:5, "New Test"] <- NA
  
  # Function should handle missing data gracefully
  expect_no_error({
    result <- decision(
      data = test_data,
      gold = "Golden Standart",
      goldPositive = "1",
      newtest = "New Test",
      testPositive = "1"
    )
  })
})

test_that("decision function produces correct metrics", {
  # Create a simple known dataset for validation
  test_data <- data.frame(
    gold_standard = c(1, 1, 1, 1, 0, 0, 0, 0),
    new_test = c(1, 1, 0, 0, 1, 0, 0, 0)
  )
  
  result <- decision(
    data = test_data,
    gold = "gold_standard",
    goldPositive = "1",
    newtest = "new_test",
    testPositive = "1"
  )
  
  # Check that result structure is correct
  expect_s3_class(result, "Group")
  expect_s3_class(result$cTable, "Table")
  expect_s3_class(result$ratioTable, "Table")
})

test_that("decision function output structure is complete", {
  testthat::skip_on_cran()

  data("histopathology", package = "ClinicoPath")
  
  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1",
    od = TRUE,
    fnote = TRUE,
    ci = TRUE,
    fagan = TRUE
  )
  
  # Check all expected output components exist
  expected_components <- c("text1", "text2", "cTable", "nTable", "ratioTable", 
                          "epirTable_ratio", "epirTable_number", "plot1")
  
  for (component in expected_components) {
    expect_true(component %in% names(result), 
                info = paste("Missing component:", component))
  }
})

test_that("decision handles NA values without misclassification", {
  testthat::skip_on_cran()

  diagnostic_data <- data.frame(
    gold = factor(c("pos", "pos", "neg", "neg", "pos", NA), levels = c("pos", "neg")),
    assay = factor(c("pos", "neg", "pos", "neg", NA, "pos"), levels = c("pos", "neg"))
  )

  result <- decision(
    data = diagnostic_data,
    gold = "gold",
    goldPositive = "pos",
    newtest = "assay",
    testPositive = "pos"
  )

  c_table <- result$cTable$asDF
  n_table <- result$nTable$asDF

  expect_equal(n_table$TotalPop, 4)
  expect_equal(c_table$GP[c_table$newtest == "Test Positive"], 1)
  expect_equal(c_table$GN[c_table$newtest == "Test Positive"], 1)
  expect_equal(c_table$GP[c_table$newtest == "Test Negative"], 1)
  expect_equal(c_table$GN[c_table$newtest == "Test Negative"], 1)
})

test_that("decision ignores missing values in unrelated variables", {
  testthat::skip_on_cran()

  diagnostic_data <- data.frame(
    gold = factor(c("pos", "pos", "neg", "neg", "pos"), levels = c("pos", "neg")),
    assay = factor(c("pos", "neg", "pos", "neg", "pos"), levels = c("pos", "neg")),
    other_measure = c(1, NA, 3, 4, 5)
  )

  result <- decision(
    data = diagnostic_data,
    gold = "gold",
    goldPositive = "pos",
    newtest = "assay",
    testPositive = "pos"
  )

  n_table <- result$nTable$asDF
  expect_equal(n_table$TotalPop, 5)
})

test_that("decision allows population prevalence with confidence intervals", {
  testthat::skip_on_cran()

  data("histopathology", package = "ClinicoPath")

  result <- decision(
    data = histopathology,
    gold = "Golden Standart",
    goldPositive = "1",
    newtest = "New Test",
    testPositive = "1",
    pp = TRUE,
    pprob = 0.2,
    ci = TRUE
  )

  ratio <- result$ratioTable$asDF
  expect_equal(ratio$PrevalenceD, 0.2, tolerance = 1e-6)
  expect_true("epirTable_ratio" %in% names(result))
})

test_that("natural language summary reports NPV correctly", {
  testthat::skip_on_cran()

  diagnostic_data <- data.frame(
    gold = factor(c("pos", "pos", "pos", "neg", "neg", "neg"), levels = c("pos", "neg")),
    assay = factor(c("pos", "pos", "neg", "pos", "neg", "neg"), levels = c("pos", "neg"))
  )

  result <- decision(
    data = diagnostic_data,
    gold = "gold",
    goldPositive = "pos",
    newtest = "assay",
    testPositive = "pos"
  )

  ratio <- result$ratioTable$asDF
  npv_percent <- sprintf("%.1f%%", ratio$NPV * 100)

  summary_html <- result$naturalLanguageSummary$content
  expect_match(summary_html, paste0("NPV ", npv_percent), fixed = TRUE)
  expect_false(grepl("NPV 33.3%", summary_html, fixed = TRUE))
})

test_that("decision handles absence of gold standard negatives", {
  testthat::skip_on_cran()

  diagnostic_data <- data.frame(
    gold = factor(rep("pos", 5), levels = c("pos", "neg")),
    assay = factor(c("pos", "pos", "pos", "pos", "neg"), levels = c("pos", "neg"))
  )

  result <- decision(
    data = diagnostic_data,
    gold = "gold",
    goldPositive = "pos",
    newtest = "assay",
    testPositive = "pos"
  )

  ratio <- result$ratioTable$asDF
  expect_true(is.na(ratio$Spec))
  expect_true(is.na(ratio$LRP))
  expect_true(is.na(ratio$LRN))
})
