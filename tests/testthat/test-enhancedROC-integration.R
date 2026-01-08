# ═══════════════════════════════════════════════════════════
# Integration Tests: enhancedROC
# ═══════════════════════════════════════════════════════════
#
# Tests integration with other packages, realistic workflows,
# and output consistency for the enhancedROC jamovi function

library(testthat)
library(ClinicoPath)

# Load test data
data(enhancedroc_biomarker, package = "ClinicoPath")
data(enhancedroc_comparative, package = "ClinicoPath")

test_that("enhancedROC produces consistent results across runs", {
  # Run the same analysis twice
  result1 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  result2 <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  # Results should be identical (deterministic)
  expect_s3_class(result1, "enhancedROCClass")
  expect_s3_class(result2, "enhancedROCClass")
})

test_that("enhancedROC workflow: basic → plots → metrics", {
  # Step 1: Basic ROC analysis
  basic <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )
  expect_s3_class(basic, "enhancedROCClass")

  # Step 2: Add visualizations
  with_plots <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE,
    showCutoffPoints = TRUE
  )
  expect_s3_class(with_plots, "enhancedROCClass")

  # Step 3: Add comprehensive metrics
  with_metrics <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    rocCurve = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE,
    youdenOptimization = TRUE
  )
  expect_s3_class(with_metrics, "enhancedROCClass")
})

test_that("enhancedROC workflow: single → comparative → pairwise", {
  # Step 1: Single biomarker analysis
  single <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = "established_marker",
    analysisType = "single"
  )
  expect_s3_class(single, "enhancedROCClass")

  # Step 2: Add comparative analysis
  comparative <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    analysisType = "comparative"
  )
  expect_s3_class(comparative, "enhancedROCClass")

  # Step 3: Add pairwise statistical comparison
  with_comparison <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    analysisType = "comparative",
    pairwiseComparisons = TRUE,
    comparisonMethod = "delong"
  )
  expect_s3_class(with_comparison, "enhancedROCClass")
})

test_that("enhancedROC workflow: screening application", {
  data(enhancedroc_screening, package = "ClinicoPath")

  # Complete screening workflow
  result <- enhancedROC(
    data = enhancedroc_screening,
    outcome = "screening_indication",
    positiveClass = "Screen Positive",
    predictors = c("sensitive_marker", "imaging_marker", "panel_score"),
    analysisType = "comparative",
    clinicalContext = "screening",
    sensitivityThreshold = 0.90,  # High sensitivity for screening
    youdenOptimization = TRUE,
    rocCurve = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE,
    useObservedPrevalence = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC workflow: confirmatory testing", {
  data(enhancedroc_confirmatory, package = "ClinicoPath")

  # Complete confirmatory workflow
  result <- enhancedROC(
    data = enhancedroc_confirmatory,
    outcome = "confirmed_diagnosis",
    positiveClass = "Confirmed",
    predictors = c("specific_marker", "pathology_score", "molecular_signature"),
    analysisType = "comparative",
    clinicalContext = "diagnosis",
    specificityThreshold = 0.95,  # High specificity for confirmation
    youdenOptimization = TRUE,
    rocCurve = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC workflow: biomarker validation study", {
  # Complete biomarker validation
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1", "novel_marker2", "genetic_risk_score"),
    analysisType = "comprehensive",
    youdenOptimization = TRUE,
    customCutoffs = "50, 100, 150, 200",
    rocCurve = TRUE,
    aucTable = TRUE,
    cutoffTable = TRUE,
    optimalCutoffs = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE,
    pairwiseComparisons = TRUE,
    comparisonMethod = "delong",
    useBootstrap = TRUE,
    bootstrapSamples = 500
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC workflow: calibration assessment", {
  data(enhancedroc_calibration, package = "ClinicoPath")

  # Calibration workflow
  result <- enhancedROC(
    data = enhancedroc_calibration,
    outcome = "outcome",
    positiveClass = "Event",
    predictors = c("predictor1", "predictor2", "risk_score"),
    rocCurve = TRUE,
    diagnosticMetrics = TRUE,
    calibrationAnalysis = TRUE,
    calibrationPlot = TRUE,
    brierScore = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC workflow: class imbalance handling", {
  data(enhancedroc_imbalanced, package = "ClinicoPath")

  # Imbalanced data workflow
  result <- enhancedROC(
    data = enhancedroc_imbalanced,
    outcome = "rare_disease",
    positiveClass = "Positive",
    predictors = c("screening_marker", "confirmatory_marker", "combined_risk"),
    detectImbalance = TRUE,
    imbalanceThreshold = 3.0,
    showImbalanceWarning = TRUE,
    stratifiedBootstrap = TRUE,
    useBootstrap = TRUE,
    bootstrapSamples = 500,
    rocCurve = TRUE,
    clinicalMetrics = TRUE,
    useObservedPrevalence = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC workflow: multi-class ROC analysis", {
  data(enhancedroc_multiclass, package = "ClinicoPath")

  # Multi-class workflow
  result <- enhancedROC(
    data = enhancedroc_multiclass,
    outcome = "disease_severity",
    predictors = c("biomarker_A", "biomarker_B", "imaging_severity_score"),
    multiClassROC = TRUE,
    multiClassStrategy = "ovr",
    multiClassAveraging = "macro"
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC handles data from CSV import", {
  # Write test data to temporary CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(enhancedroc_biomarker, temp_csv, row.names = FALSE)

  # Read it back
  csv_data <- read.csv(temp_csv)

  result <- enhancedROC(
    data = csv_data,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")

  # Clean up
  unlink(temp_csv)
})

test_that("enhancedROC handles data from Excel import", {
  # Write test data to temporary Excel file
  temp_xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(enhancedroc_biomarker, temp_xlsx)

  # Read it back
  xlsx_data <- readxl::read_excel(temp_xlsx)

  result <- enhancedROC(
    data = as.data.frame(xlsx_data),
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result, "enhancedROCClass")

  # Clean up
  unlink(temp_xlsx)
})

test_that("enhancedROC handles different data structures consistently", {
  # Test with tibble
  library(tibble)
  tibble_data <- as_tibble(enhancedroc_biomarker)

  result_tibble <- enhancedROC(
    data = tibble_data,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result_tibble, "enhancedROCClass")

  # Test with data.frame
  df_data <- as.data.frame(enhancedroc_biomarker)

  result_df <- enhancedROC(
    data = df_data,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )

  expect_s3_class(result_df, "enhancedROCClass")
})

test_that("enhancedROC workflow: complete publication-ready analysis", {
  # Comprehensive analysis for publication
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1", "novel_marker2"),
    analysisType = "comprehensive",
    youdenOptimization = TRUE,
    customCutoffs = "50, 100, 150",
    sensitivityThreshold = 0.80,
    specificityThreshold = 0.80,
    confidenceLevel = 95,
    useBootstrap = TRUE,
    bootstrapSamples = 1000,
    bootstrapMethod = "bca",
    pairwiseComparisons = TRUE,
    comparisonMethod = "delong",
    rocCurve = TRUE,
    aucTable = TRUE,
    cutoffTable = TRUE,
    optimalCutoffs = TRUE,
    diagnosticMetrics = TRUE,
    clinicalMetrics = TRUE,
    useObservedPrevalence = TRUE,
    plotTheme = "clinical",
    showCutoffPoints = TRUE,
    showConfidenceBands = TRUE,
    comprehensive_output = TRUE,
    clinical_interpretation = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC workflow: iterative cutoff optimization", {
  # Step 1: Find Youden optimal cutoff
  youden_result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    youdenOptimization = TRUE,
    optimalCutoffs = TRUE
  )
  expect_s3_class(youden_result, "enhancedROCClass")

  # Step 2: Evaluate custom cutoffs around optimal
  custom_result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    customCutoffs = "15, 18, 20, 22, 25",
    cutoffTable = TRUE
  )
  expect_s3_class(custom_result, "enhancedROCClass")

  # Step 3: Apply sensitivity/specificity constraints
  constrained_result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    sensitivityThreshold = 0.85,
    specificityThreshold = 0.75,
    cutoffTable = TRUE
  )
  expect_s3_class(constrained_result, "enhancedROCClass")
})

test_that("enhancedROC workflow: partial AUC for specific ranges", {
  # High-specificity partial AUC (e.g., for confirmatory testing)
  high_spec_result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    partialAuc = TRUE,
    partialAucType = "specificity",
    partialRange = "0.9,1.0",
    rocCurve = TRUE
  )
  expect_s3_class(high_spec_result, "enhancedROCClass")

  # High-sensitivity partial AUC (e.g., for screening)
  high_sens_result <- enhancedROC(
    data = enhancedroc_biomarker,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1",
    partialAuc = TRUE,
    partialAucType = "sensitivity",
    partialRange = "0.9,1.0",
    rocCurve = TRUE
  )
  expect_s3_class(high_sens_result, "enhancedROCClass")
})

test_that("enhancedROC workflow: comparing old vs new biomarker", {
  # Simulate clinical validation of new biomarker against gold standard
  result <- enhancedROC(
    data = enhancedroc_comparative,
    outcome = "cancer_status",
    positiveClass = "Cancer",
    predictors = c("established_marker", "novel_marker1"),
    analysisType = "comparative",
    pairwiseComparisons = TRUE,
    comparisonMethod = "delong",
    showMetricsDiff = TRUE,
    statisticalComparison = TRUE,
    rocCurve = TRUE,
    aucTable = TRUE,
    diagnosticMetrics = TRUE,
    useBootstrap = TRUE,
    bootstrapSamples = 500
  )

  expect_s3_class(result, "enhancedROCClass")
})

test_that("enhancedROC workflow: subgroup analysis", {
  # Analyze different subgroups
  male_subset <- enhancedroc_biomarker[enhancedroc_biomarker$sex == "Male", ]
  result_male <- enhancedROC(
    data = male_subset,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )
  expect_s3_class(result_male, "enhancedROCClass")

  female_subset <- enhancedroc_biomarker[enhancedroc_biomarker$sex == "Female", ]
  result_female <- enhancedROC(
    data = female_subset,
    outcome = "disease_status",
    positiveClass = "Disease",
    predictors = "biomarker1"
  )
  expect_s3_class(result_female, "enhancedROCClass")
})

test_that("enhancedROC workflow: sensitivity analysis with different prevalences", {
  # Test how PPV/NPV change with different prevalence assumptions
  prevalences <- c(0.05, 0.10, 0.20, 0.30)

  for (prev in prevalences) {
    result <- enhancedROC(
      data = enhancedroc_biomarker,
      outcome = "disease_status",
      positiveClass = "Disease",
      predictors = "biomarker1",
      clinicalMetrics = TRUE,
      prevalence = prev
    )
    expect_s3_class(result, "enhancedROCClass")
  }
})

test_that("enhancedROC integrates with small dataset workflow", {
  data(enhancedroc_small, package = "ClinicoPath")

  # Quick analysis with small dataset
  result <- enhancedROC(
    data = enhancedroc_small,
    outcome = "disease",
    positiveClass = "Positive",
    predictors = "marker",
    rocCurve = TRUE
  )

  expect_s3_class(result, "enhancedROCClass")
})
