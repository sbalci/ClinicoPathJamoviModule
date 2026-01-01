context("Decision Curve Analysis")

# Test main decisioncurve() function
test_that("decisioncurve function works with basic parameters", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  testthat::skip_on_cran()
  
  # Load test data
  data("histopathology", package = "ClinicoPath")
  
  # Create predicted probabilities for testing
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])  # Age as probability
  test_data$measurement_prob <- plogis(scale(test_data$MeasurementA)[,1])  # Measurement as probability
  
  # Test basic function call without errors
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob"
    )
  })
  
  # Test that function returns expected structure
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = "age_prob"
  )
  
  expect_s3_class(result, "Group")
  expect_true(length(result) > 0)
  
  # Test that required output components exist
  expect_true("instructions" %in% names(result))
  expect_true("procedureNotes" %in% names(result))
  expect_true("resultsTable" %in% names(result))
  expect_true("dcaPlot" %in% names(result))
})

test_that("decisioncurve function works with multiple models", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Create multiple predicted probabilities
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  test_data$measurement_prob <- plogis(scale(test_data$MeasurementA)[,1])
  
  # Test with multiple models
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = c("age_prob", "measurement_prob"),
      modelNames = "Age Model, Measurement Model"
    )
  })
  
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = c("age_prob", "measurement_prob"),
    modelNames = "Age Model, Measurement Model"
  )
  
  # Check that results include both models
  expect_s3_class(result, "Group")
  expect_true("resultsTable" %in% names(result))
})

test_that("decisioncurve function works with confidence intervals", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  # Test with confidence intervals enabled
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob",
      confidenceIntervals = TRUE,
      bootReps = 100  # Use fewer reps for faster testing
    )
  })
})

test_that("decisioncurve function works with clinical impact analysis", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  # Test with clinical impact analysis
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob",
      calculateClinicalImpact = TRUE,
      populationSize = 1000
    )
  })
  
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = "age_prob",
    calculateClinicalImpact = TRUE,
    populationSize = 1000
  )
  
  # Check that clinical impact table is included
  expect_true("clinicalImpactTable" %in% names(result))
})

test_that("decisioncurve function works with optimal threshold analysis", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  # Test with optimal threshold analysis
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob",
      showOptimalThreshold = TRUE
    )
  })
  
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = "age_prob",
    showOptimalThreshold = TRUE
  )
  
  # Check that optimal threshold table is included
  expect_true("optimalTable" %in% names(result))
})

test_that("decisioncurve function works with weighted AUC", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  # Test with weighted AUC
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob",
      weightedAUC = TRUE
    )
  })
  
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = "age_prob",
    weightedAUC = TRUE
  )
  
  # Check that weighted AUC table is included
  expect_true("weightedAUCTable" %in% names(result))
})

test_that("clinical decision rule uses patient-level data and matches manual net benefit", {
  testthat::skip_on_cran()

  # Construct small deterministic data
  df <- data.frame(
    outcome = factor(c("1", "1", "0", "0", "0"), levels = c("0", "1")),
    model_prob = c(0.9, 0.7, 0.4, 0.2, 0.1),
    rule_var = factor(c("yes", "yes", "no", "no", "no"), levels = c("no", "yes"))
  )

  res <- decisioncurve(
    data = df,
    outcome = "outcome",
    outcomePositive = "1",
    models = "model_prob",
    clinicalDecisionRule = TRUE,
    decisionRuleVar = "rule_var",
    decisionRulePositive = "yes",
    decisionRuleLabel = "Rule",
    thresholdRange = "custom",
    thresholdMin = 0.2,
    thresholdMax = 0.2,
    thresholdStep = 0.2,
    selectedThresholds = "0.2",
    showTable = TRUE
  )

  tbl <- res$resultsTable$asDF()
  # Manual NB at pt=0.2: model = 0.3, rule = 0.4, treat_all = 0.25
  expect_equal(tbl$treat_all[1], 0.25, tolerance = 1e-6)
  expect_equal(tbl$model_prob[1], 0.3, tolerance = 1e-6)
  expect_equal(tbl$model_Rule[1], 0.4, tolerance = 1e-6)
})

test_that("clinical decision rule participates in complete-case filtering", {
  testthat::skip_on_cran()

  df <- data.frame(
    outcome = factor(c("1", "1", "0", "0", "0"), levels = c("0", "1")),
    model_prob = c(0.9, 0.7, 0.4, 0.2, 0.1),
    rule_var = factor(c("yes", "yes", NA, "no", "no"), levels = c("no", "yes"))
  )

  res <- decisioncurve(
    data = df,
    outcome = "outcome",
    outcomePositive = "1",
    models = "model_prob",
    clinicalDecisionRule = TRUE,
    decisionRuleVar = "rule_var",
    decisionRulePositive = "yes",
    decisionRuleLabel = "Rule",
    thresholdRange = "custom",
    thresholdMin = 0.2,
    thresholdMax = 0.2,
    thresholdStep = 0.2,
    selectedThresholds = "0.2",
    showTable = TRUE
  )

  tbl <- res$resultsTable$asDF()

  # Because rule_var has an NA, complete cases drop to n=4 with prevalence 0.5
  # Treat-all NB at pt=0.2 with prevalence 0.5: 0.5 - 0.5*(0.2/0.8) = 0.375
  expect_equal(tbl$treat_all[1], 0.375, tolerance = 1e-6)
})

test_that("decisioncurve function works with custom threshold ranges", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  # Test with custom threshold range
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob",
      thresholdRange = "custom",
      thresholdMin = 0.10,
      thresholdMax = 0.40,
      thresholdStep = 0.05
    )
  })
})

test_that("decisioncurve function works with interventions avoided plot", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  # Test with interventions avoided
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob",
      showInterventionAvoided = TRUE
    )
  })
  
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = "age_prob",
    showInterventionAvoided = TRUE
  )
  
  # Check that interventions avoided plot is included
  expect_true("interventionsAvoidedPlot" %in% names(result))
})

test_that("decisioncurve function validates parameters correctly", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  # Test invalid threshold range
  expect_error({
    decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob",
      thresholdRange = "custom",
      thresholdMin = 0.8,  # Min > Max
      thresholdMax = 0.2
    )
  })
})

test_that("decisioncurve function handles missing data appropriately", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  # Create dataset with some missing values
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  test_data[1:5, "age_prob"] <- NA
  
  # Function should handle missing data gracefully
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = "age_prob"
    )
  })
})

test_that("decisioncurve function works with model comparison", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  test_data$measurement_prob <- plogis(scale(test_data$MeasurementA)[,1])
  
  # Test with model comparison
  expect_no_error({
    result <- decisioncurve(
      data = test_data,
      outcome = "Death",
      outcomePositive = "1",
      models = c("age_prob", "measurement_prob"),
      compareModels = TRUE
    )
  })
  
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = c("age_prob", "measurement_prob"),
    compareModels = TRUE
  )
  
  # Check that comparison table is included
  expect_true("comparisonTable" %in% names(result))
})

test_that("decisioncurve function output structure is complete", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  test_data$measurement_prob <- plogis(scale(test_data$MeasurementA)[,1])
  
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = c("age_prob", "measurement_prob"),
    showOptimalThreshold = TRUE,
    calculateClinicalImpact = TRUE,
    weightedAUC = TRUE,
    compareModels = TRUE,
    showInterventionAvoided = TRUE
  )
  
  # Check all expected output components exist
  expected_components <- c("instructions", "procedureNotes", "resultsTable", 
                          "optimalTable", "clinicalImpactTable", "comparisonTable",
                          "weightedAUCTable", "dcaPlot", "clinicalImpactPlot",
                          "interventionsAvoidedPlot", "summaryText")
  
  for (component in expected_components) {
    expect_true(component %in% names(result), 
                info = paste("Missing component:", component))
  }
})

test_that("decisioncurve function works with different plot styles", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  # Test with different plot styles
  plot_styles <- c("standard", "detailed")
  
  for (style in plot_styles) {
    expect_no_error({
      result <- decisioncurve(
        data = test_data,
        outcome = "Death",
        outcomePositive = "1",
        models = "age_prob",
        plotStyle = style
      )
    })
  }
})

test_that("decisioncurve function produces correct summary text", {
  testthat::skip_on_cran()
  
  data("histopathology", package = "ClinicoPath")
  
  test_data <- histopathology
  test_data$age_prob <- plogis(scale(test_data$Age)[,1])
  
  result <- decisioncurve(
    data = test_data,
    outcome = "Death",
    outcomePositive = "1",
    models = "age_prob"
  )
  
  # Check that summary text exists and is not empty
  expect_true("summaryText" %in% names(result))
  expect_s3_class(result$summaryText, "Html")
})
