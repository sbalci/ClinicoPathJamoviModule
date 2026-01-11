
library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("likelihoodratio runs without error on binary data", {
  set.seed(123)
  n <- 100
  
  # Create proper binary data
  data <- data.frame(
    testVariable = factor(sample(c("Pos", "Neg"), n, replace = TRUE)),
    referenceStandard = factor(sample(c("Pos", "Neg"), n, replace = TRUE)),
    groupVariable = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  
  # Run with all parameters explicitly provided
  expect_no_error({
    results <- likelihoodratio(
      data = data,
      testVariable = "testVariable",
      referenceStandard = "referenceStandard",
      groupVariable = "groupVariable",
      analysisType = "binary",
      testDirection = "higher",
      cutpointMethod = "youden",
      manualCutpoint = 1,
      costRatio = 1,
      confidenceLevel = 0.95,
      ciMethod = "log",
      bootstrapSamples = 2000,
      calculatePostTest = TRUE,
      prevalenceRange = "clinical",
      customPrevalenceMin = 5,
      customPrevalenceMax = 50,
      specificPrevalences = "10, 25, 50",
      calculateDOR = TRUE,
      calculateNRI = FALSE,
      compareVariable = "groupVariable",
      predictiveValueCurves = TRUE,
      clinicalThresholds = "0.15, 0.85",
      performStratified = FALSE,
      testHomogeneity = FALSE,
      showSummaryTable = TRUE,
      showCrosstabulation = TRUE,
      showLikelihoodRatios = TRUE,
      showPostTestProbs = TRUE,
      showOptimalCutpoint = TRUE,
      showDiagnosticOdds = TRUE,
      showClinicalInterpretation = TRUE,
      plotROC = FALSE,
      plotPredictiveValues = FALSE,
      plotLikelihoodRatios = FALSE,
      plotPostTestProbabilities = FALSE,
      plotDistributions = FALSE,
      correctContinuity = TRUE,
      exactTests = FALSE,
      smoothROC = FALSE,
      randomSeed = 42
    )
  })
})

test_that("likelihoodratio returns results object", {
  set.seed(456)
  n <- 80
  
  data <- data.frame(
    testVariable = factor(sample(c("Pos", "Neg"), n, replace = TRUE)),
    referenceStandard = factor(sample(c("Pos", "Neg"), n, replace = TRUE)),
    groupVariable = factor(sample(c("A", "B"), n, replace = TRUE))
  )
  
  results <- likelihoodratio(
    data = data,
    testVariable = "testVariable",
    referenceStandard = "referenceStandard",
    groupVariable = "groupVariable",
    analysisType = "binary",
    testDirection = "higher",
    cutpointMethod = "youden",
    manualCutpoint = 1,
    costRatio = 1,
    confidenceLevel = 0.95,
    ciMethod = "log",
    bootstrapSamples = 2000,
    calculatePostTest = TRUE,
    prevalenceRange = "clinical",
    customPrevalenceMin = 5,
    customPrevalenceMax = 50,
    specificPrevalences = "10, 25, 50",
    calculateDOR = TRUE,
    calculateNRI = FALSE,
    compareVariable = "groupVariable",
    predictiveValueCurves = TRUE,
    clinicalThresholds = "0.15, 0.85",
    performStratified = FALSE,
    testHomogeneity = FALSE,
    showSummaryTable = TRUE,
    showCrosstabulation = TRUE,
    showLikelihoodRatios = TRUE,
    showPostTestProbs = FALSE,
    showOptimalCutpoint = FALSE,
    showDiagnosticOdds = TRUE,
    showClinicalInterpretation = TRUE,
    plotROC = FALSE,
    plotPredictiveValues = FALSE,
    plotLikelihoodRatios = FALSE,
    plotPostTestProbabilities = FALSE,
    plotDistributions = FALSE,
    correctContinuity = TRUE,
    exactTests = FALSE,
    smoothROC = FALSE,
    randomSeed = 42
  )
  
  expect_true(inherits(results, "likelihoodratioResults"))
})
