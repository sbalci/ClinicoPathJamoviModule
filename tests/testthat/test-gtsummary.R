## Test file for gtsummary function
## Testing comprehensive publication-ready table generation

test_that("gtsummary loads test data correctly", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  # Load test datasets
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  data(gtsummary_survey_data, package = "ClinicoPath")
  data(gtsummary_laboratory_data, package = "ClinicoPath")
  data(gtsummary_manufacturing_data, package = "ClinicoPath")
  data(gtsummary_cross_data, package = "ClinicoPath")
  
  # Test dataset dimensions
  expect_equal(nrow(gtsummary_clinical_trial), 300)
  expect_equal(ncol(gtsummary_clinical_trial), 26)
  
  expect_equal(nrow(gtsummary_survey_data), 500)
  expect_equal(ncol(gtsummary_survey_data), 19)
  
  expect_equal(nrow(gtsummary_laboratory_data), 400)
  expect_equal(ncol(gtsummary_laboratory_data), 29)
  
  expect_equal(nrow(gtsummary_manufacturing_data), 350)
  expect_equal(ncol(gtsummary_manufacturing_data), 24)
  
  expect_equal(nrow(gtsummary_cross_data), 600)
  expect_equal(ncol(gtsummary_cross_data), 15)
})

test_that("gtsummary creates basic summary tables", {
  # Load test dataset
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  
  # Test basic summary table creation
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "gender", "bmi"),
      tableType = "summary"
    )
  })
})

test_that("gtsummary creates grouped summary tables", {
  # Load test dataset
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  
  # Test grouped summary table
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "gender", "bmi", "diabetes", "disease_stage"),
      byvar = "treatment_group",
      tableType = "summary",
      includeOverall = TRUE,
      addPValue = TRUE
    )
  })
})

test_that("gtsummary creates cross-tabulation tables", {
  # Load test dataset  
  data(gtsummary_cross_data, package = "ClinicoPath")
  
  # Test cross-tabulation table
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_cross_data,
      vars = c("treatment_response", "drug_dosage"),
      tableType = "cross",
      addPValue = TRUE,
      percentType = "row"
    )
  })
})

test_that("gtsummary handles different variable types", {
  # Load test dataset
  data(gtsummary_laboratory_data, package = "ClinicoPath")
  
  # Test with mixed variable types
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_laboratory_data,
      vars = c("glucose", "creatinine", "cholesterol_flag", "department"),
      byvar = "patient_sex",
      tableType = "summary",
      statistics = c("mean_sd", "median_iqr", "n_percent"),
      includeMissing = "ifany"
    )
  })
})

test_that("gtsummary applies formatting options", {
  # Load test dataset
  data(gtsummary_survey_data, package = "ClinicoPath")
  
  # Test formatting options
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_survey_data,
      vars = c("age_group", "income_level", "education_level"),
      byvar = "region",
      tableType = "summary",
      boldLabels = TRUE,
      boldPValues = TRUE,
      italicizeLabels = FALSE,
      pValueThreshold = 0.05,
      digitsOverall = 2,
      digitsPValue = 3
    )
  })
})

test_that("gtsummary handles statistical tests", {
  # Load test dataset
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  
  # Test different statistical tests
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "gender", "bmi_category"),
      byvar = "treatment_group",
      tableType = "summary",
      addPValue = TRUE,
      testMethod = "auto"
    )
  })
  
  # Test specific test method
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "hemoglobin"),
      byvar = "treatment_group",
      tableType = "summary",
      addPValue = TRUE,
      testMethod = "t.test"
    )
  })
})

test_that("gtsummary handles missing values appropriately", {
  # Load test dataset with missing values
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  
  # Test missing value handling
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "education", "biomarker_b"),
      byvar = "treatment_group",
      tableType = "summary",
      includeMissing = "always"
    )
  })
  
  # Test with missing values excluded
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "education", "biomarker_b"),
      byvar = "treatment_group",
      tableType = "summary",
      includeMissing = "no"
    )
  })
})

test_that("gtsummary creates regression tables", {
  # Load test dataset
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  
  # Test regression table creation
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "gender", "bmi", "diabetes"),
      tableType = "regression"
    )
  })
})

test_that("gtsummary handles output formats", {
  # Load test dataset
  data(gtsummary_manufacturing_data, package = "ClinicoPath")
  
  # Test HTML output
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_manufacturing_data,
      vars = c("actual_temperature", "pressure_psi", "overall_grade"),
      byvar = "shift",
      tableType = "summary",
      outputFormat = "html"
    )
  })
  
  # Test markdown output
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_manufacturing_data,
      vars = c("actual_temperature", "pressure_psi", "overall_grade"),
      byvar = "shift",
      tableType = "summary",
      outputFormat = "markdown"
    )
  })
})

test_that("gtsummary handles export and code generation", {
  # Load test dataset
  data(gtsummary_survey_data, package = "ClinicoPath")
  
  # Test with code generation and export
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_survey_data,
      vars = c("age_group", "income_level", "overall_satisfaction"),
      byvar = "region",
      tableType = "summary",
      showCode = TRUE,
      exportTable = TRUE
    )
  })
})

test_that("gtsummary handles advanced formatting features", {
  # Load test dataset
  data(gtsummary_laboratory_data, package = "ClinicoPath")
  
  # Test advanced formatting
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_laboratory_data,
      vars = c("glucose", "creatinine", "glucose_flag"),
      byvar = "department",
      tableType = "summary",
      addPValue = TRUE,
      addQ = TRUE,
      showNHeader = TRUE,
      boldLabels = TRUE,
      boldLevels = TRUE,
      italicizeLabels = FALSE,
      tableTitle = "Laboratory Results by Department",
      footnote = "All values are from routine clinical testing"
    )
  })
})

test_that("gtsummary error handling works correctly", {
  # Load test dataset
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  
  # Test error handling for invalid inputs
  expect_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("nonexistent_variable"),
      tableType = "summary"
    )
  })
  
  # Test error handling for cross table with insufficient variables
  expect_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age"),
      tableType = "cross"
    )
  })
})

test_that("gtsummary validates data structure", {
  # Test with empty data
  expect_error({
    result <- ClinicoPath::gtsummary(
      data = data.frame(),
      vars = c("age"),
      tableType = "summary"
    )
  })
  
  # Test with valid data structure
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "gender"),
      tableType = "summary"
    )
  })
})

test_that("gtsummary summary statistics are comprehensive", {
  # Load test dataset
  data(gtsummary_clinical_trial, package = "ClinicoPath")
  
  # Test all available statistics
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_clinical_trial,
      vars = c("age", "bmi", "gender", "diabetes"),
      byvar = "treatment_group",
      tableType = "summary",
      statistics = c("mean_sd", "median_iqr", "range", "n_percent", "missing"),
      includeMissing = "always"
    )
  })
})

test_that("gtsummary handles complex cross-tabulation scenarios", {
  # Load test dataset
  data(gtsummary_cross_data, package = "ClinicoPath")
  
  # Test complex cross-tabulation
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_cross_data,
      vars = c("treatment_response", "drug_dosage"),
      tableType = "cross",
      addPValue = TRUE,
      percentType = "column"
    )
  })
  
  # Test with different percentage types
  expect_no_error({
    result <- ClinicoPath::gtsummary(
      data = gtsummary_cross_data,
      vars = c("side_effects", "compliance"),
      tableType = "cross",
      percentType = "cell"
    )
  })
})
