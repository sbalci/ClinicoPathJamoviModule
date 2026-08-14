
library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
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

# --- An unusable manual cutpoint must stop the analysis with guidance ---------
# `manualCutpoint` is a Number. Before the fix it carried no `default:`, so it
# arrived as NULL and jamovi threw "missing value where TRUE/FALSE needed" while
# comparing it for clearWith - BEFORE any backend code ran, and for EVERY
# cutpoint method including the default Youden, which never reads it. With a
# numeric default in place the remaining failure mode is a cutpoint that cannot
# split the data: every case lands on one side, the 2x2 table collapses and
# contingency[2,1] threw "subscript out of bounds".
lr_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

lr_fit <- function(cutpointMethod, manualCutpoint) {
    set.seed(3)
    d <- data.frame(
        marker = c(stats::rnorm(30, 5), stats::rnorm(30, 8)),
        gold   = factor(rep(c("neg", "pos"), each = 30))
    )
    likelihoodratio(data = d, testVariable = "marker", referenceStandard = "gold",
                    groupVariable = NULL, compareVariable = NULL,
                    analysisType = "continuous",
                    cutpointMethod = cutpointMethod, manualCutpoint = manualCutpoint)
}

test_that("a manual cutpoint outside the data range is refused, not crashed on", {
    # marker spans roughly 3.3 to 9.7; 0 and 99 both put every case on one side.
    for (bad in c(0, 99)) {
        res <- expect_no_error(lr_fit("manual", bad))
        msg <- lr_txt(res$instructions$content)
        expect_match(msg, "Enter a cutpoint inside the range")
        expect_match(msg, "puts every case on one side")
        # the message must name the usable range so the user can act on it
        expect_match(msg, "ranges from")
    }
})

test_that("a usable manual cutpoint still analyses normally", {
    res <- lr_fit("manual", 6.5)
    expect_no_match(lr_txt(res$instructions$content), "Enter a cutpoint inside the range")
})

test_that("estimated cutpoint methods are unaffected by manualCutpoint", {
    res <- lr_fit("youden", 0)
    expect_no_match(lr_txt(res$instructions$content), "Enter a cutpoint inside the range")
})
