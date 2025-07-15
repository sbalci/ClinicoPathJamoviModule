#!/usr/bin/env Rscript

# =============================================================================
# Comprehensive Unit Tests for Enhanced Tree Function
# =============================================================================

test_that("tree basic functionality works", {
  
  # Load required test data
  data("small_sample_tree", package = "ClinicoPath")
  
  # Test basic analysis with minimal data
  result <- tree(
    data = small_sample_tree,
    vars = c("biomarker_1", "biomarker_2", "age"),
    facs = c("treatment", "stage"),
    target = "outcome",
    targetLevel = "Yes",
    train = "cohort",
    trainLevel = "train"
  )
  
  # Check that result object exists and has expected structure
  expect_true(inherits(result, "treeResults"))
  expect_true("todo" %in% names(result))
  expect_true("modelSummary" %in% names(result))
  expect_true("plot" %in% names(result))
  
  # Check that some basic output is generated
  expect_true(length(result$todo$content) > 0 || length(result$modelSummary$content) > 0)
})

test_that("tree clinical metrics functionality works", {
  
  data("cancer_biomarkers", package = "ClinicoPath")
  
  # Test clinical metrics analysis
  result <- tree(
    data = cancer_biomarkers,
    vars = c("PSA", "age", "tumor_size"),
    facs = c("grade", "stage"),
    target = "diagnosis",
    targetLevel = "cancer",
    train = "cohort",
    trainLevel = "discovery",
    clinicalMetrics = TRUE
  )
  
  # Check that clinical metrics table exists
  expect_true("clinicalMetrics" %in% names(result))
  
  # Check if clinical metrics has content when there's sufficient data
  if (nrow(as.data.frame(result$clinicalMetrics)) > 0) {
    metrics_df <- as.data.frame(result$clinicalMetrics)
    expect_true("metric" %in% colnames(metrics_df))
    expect_true("value" %in% colnames(metrics_df))
    expect_true("interpretation" %in% colnames(metrics_df))
  }
})

test_that("tree feature importance analysis works", {
  
  data("drug_response", package = "ClinicoPath")
  
  # Test feature importance analysis
  result <- tree(
    data = drug_response,
    vars = c("gene_expression_1", "gene_expression_2", "protein_level_a"),
    facs = c("mutation_status", "tumor_stage"),
    target = "drug_response",
    targetLevel = "Responder",
    train = "study_phase",
    trainLevel = "phase1",
    featureImportance = TRUE
  )
  
  # Check that feature importance table exists
  expect_true("featureImportance" %in% names(result))
  
  # Check feature importance content
  if (nrow(as.data.frame(result$featureImportance)) > 0) {
    importance_df <- as.data.frame(result$featureImportance)
    expect_true("feature" %in% colnames(importance_df))
    expect_true("importance" %in% colnames(importance_df))
    expect_true("clinical_relevance" %in% colnames(importance_df))
  }
})

test_that("tree clinical interpretation works", {
  
  data("cardiovascular_risk", package = "ClinicoPath")
  
  # Test clinical interpretation
  result <- tree(
    data = cardiovascular_risk,
    vars = c("systolic_bp", "cholesterol", "age"),
    facs = c("smoking", "diabetes"),
    target = "cv_event",
    targetLevel = "Yes",
    train = "study_cohort",
    trainLevel = "training",
    showInterpretation = TRUE,
    clinicalContext = "screening"
  )
  
  # Check that clinical interpretation exists
  expect_true("clinicalInterpretation" %in% names(result))
  
  # Check that interpretation has some content
  if (length(result$clinicalInterpretation$content) > 0) {
    expect_true(nchar(result$clinicalInterpretation$content) > 0)
  }
})

test_that("tree missing data handling works", {
  
  data("pathology_diagnosis", package = "ClinicoPath")
  
  # Test missing data imputation
  result <- tree(
    data = pathology_diagnosis,
    vars = c("cell_size", "nuclear_area", "ki67_percentage"),
    facs = c("tumor_type", "differentiation"),
    target = "malignancy",
    targetLevel = "Malignant",
    train = "validation_set",
    trainLevel = "internal",
    imputeMissing = TRUE
  )
  
  # Check that analysis completes despite missing data
  expect_true(inherits(result, "treeResults"))
  expect_true("missingDataReport" %in% names(result))
  
  # Check missing data report
  if (nrow(as.data.frame(result$missingDataReport)) > 0) {
    missing_df <- as.data.frame(result$missingDataReport)
    expect_true("variable" %in% colnames(missing_df))
    expect_true("missing_count" %in% colnames(missing_df))
    expect_true("missing_percent" %in% colnames(missing_df))
  }
})

test_that("tree class balancing works", {
  
  data("pediatric_growth", package = "ClinicoPath")
  
  # Test class balancing for imbalanced outcomes
  result <- tree(
    data = pediatric_growth,
    vars = c("height_cm", "weight_kg", "age_months"),
    facs = c("sex", "birth_weight"),
    target = "growth_delay",
    targetLevel = "Delayed",
    train = "study_site",
    trainLevel = "site_A",
    balanceClasses = TRUE
  )
  
  # Check that analysis completes with class balancing
  expect_true(inherits(result, "treeResults"))
  expect_true("modelSummary" %in% names(result))
})

test_that("tree visualization options work", {
  
  data("small_sample_tree", package = "ClinicoPath")
  
  # Test basic plot
  result_plot <- tree(
    data = small_sample_tree,
    vars = c("biomarker_1", "biomarker_2"),
    facs = c("treatment"),
    target = "outcome",
    targetLevel = "Yes",
    showPlot = TRUE
  )
  
  # Check that plot exists
  expect_true("plot" %in% names(result_plot))
  
  # Test partition plot (requires exactly 2 continuous variables)
  result_partition <- tree(
    data = small_sample_tree,
    vars = c("biomarker_1", "biomarker_2"),
    facs = c("treatment"),
    target = "outcome",
    targetLevel = "Yes",
    showPartitionPlot = TRUE
  )
  
  # Check that partition plot exists
  expect_true("partitionPlot" %in% names(result_partition))
})

test_that("tree cross-validation works", {
  
  data("cancer_biomarkers", package = "ClinicoPath")
  
  # Test cross-validation
  result <- tree(
    data = cancer_biomarkers,
    vars = c("PSA", "age"),
    facs = c("grade"),
    target = "diagnosis",
    targetLevel = "cancer",
    crossValidation = TRUE,
    cvFolds = 3  # Use small number for testing
  )
  
  # Check that cross-validation results exist
  expect_true("crossValidationResults" %in% names(result))
  
  # Check CV results content
  if (nrow(as.data.frame(result$crossValidationResults)) > 0) {
    cv_df <- as.data.frame(result$crossValidationResults)
    expect_true("fold" %in% colnames(cv_df))
    expect_true("sensitivity" %in% colnames(cv_df))
    expect_true("specificity" %in% colnames(cv_df))
  }
})

test_that("tree bootstrap validation works", {
  
  data("drug_response", package = "ClinicoPath")
  
  # Test bootstrap validation
  result <- tree(
    data = drug_response,
    vars = c("gene_expression_1", "protein_level_a"),
    facs = c("mutation_status"),
    target = "drug_response",
    targetLevel = "Responder",
    train = "study_phase",
    trainLevel = "phase1",
    bootstrapValidation = TRUE,
    bootstrapSamples = 10  # Use small number for testing
  )
  
  # Check that bootstrap results exist
  expect_true("bootstrapResults" %in% names(result))
  
  # Check bootstrap results content
  if (nrow(as.data.frame(result$bootstrapResults)) > 0) {
    bootstrap_df <- as.data.frame(result$bootstrapResults)
    expect_true("metric" %in% colnames(bootstrap_df))
    expect_true("mean" %in% colnames(bootstrap_df))
    expect_true("ci_lower" %in% colnames(bootstrap_df))
    expect_true("ci_upper" %in% colnames(bootstrap_df))
  }
})

test_that("tree model comparison works", {
  
  data("cardiovascular_risk", package = "ClinicoPath")
  
  # Test model comparison
  result <- tree(
    data = cardiovascular_risk,
    vars = c("systolic_bp", "cholesterol"),
    facs = c("smoking", "diabetes"),
    target = "cv_event",
    targetLevel = "Yes",
    train = "study_cohort",
    trainLevel = "training",
    compareModels = TRUE
  )
  
  # Check that model comparison results exist
  expect_true("modelComparison" %in% names(result))
  
  # Check model comparison content
  if (nrow(as.data.frame(result$modelComparison)) > 0) {
    comparison_df <- as.data.frame(result$modelComparison)
    expect_true("model" %in% colnames(comparison_df))
    expect_true("primary_metric" %in% colnames(comparison_df))
    expect_true("sensitivity" %in% colnames(comparison_df))
    expect_true("specificity" %in% colnames(comparison_df))
  }
})

test_that("tree risk stratification works", {
  
  data("cardiovascular_risk", package = "ClinicoPath")
  
  # Test risk stratification
  result <- tree(
    data = cardiovascular_risk,
    vars = c("systolic_bp", "cholesterol", "age"),
    facs = c("smoking", "diabetes"),
    target = "cv_event",
    targetLevel = "Yes",
    train = "study_cohort",
    trainLevel = "training",
    riskStratification = TRUE
  )
  
  # Check that risk stratification results exist
  expect_true("riskStratification" %in% names(result))
  
  # Check risk stratification content
  if (nrow(as.data.frame(result$riskStratification)) > 0) {
    risk_df <- as.data.frame(result$riskStratification)
    expect_true("risk_group" %in% colnames(risk_df))
    expect_true("n_patients" %in% colnames(risk_df))
    expect_true("event_rate" %in% colnames(risk_df))
    expect_true("clinical_action" %in% colnames(risk_df))
  }
})

test_that("tree spatial analysis works", {
  
  data("pathology_diagnosis", package = "ClinicoPath")
  
  # Test spatial analysis (autocart) - only if package is available
  result <- tree(
    data = pathology_diagnosis,
    vars = c("cell_size", "nuclear_area"),
    facs = c("tumor_type"),
    target = "malignancy",
    targetLevel = "Malignant",
    spatialCoords = c("x_coord", "y_coord"),
    useAutocart = TRUE,
    compareModels = TRUE
  )
  
  # Check that spatial analysis components exist
  expect_true("spatialAnalysis" %in% names(result))
  expect_true("spatialInterpretation" %in% names(result))
  
  # Note: Actual spatial analysis depends on autocart package availability
  # The test should pass even if autocart is not available
})

test_that("tree prevalence adjustment works", {
  
  data("cancer_biomarkers", package = "ClinicoPath")
  
  # Test prevalence adjustment
  result <- tree(
    data = cancer_biomarkers,
    vars = c("PSA", "age"),
    facs = c("grade"),
    target = "diagnosis",
    targetLevel = "cancer",
    train = "cohort",
    trainLevel = "discovery",
    prevalenceAdjustment = TRUE,
    expectedPrevalence = 15
  )
  
  # Check that adjusted metrics exist
  expect_true("adjustedMetrics" %in% names(result))
  
  # Check adjusted metrics content
  if (nrow(as.data.frame(result$adjustedMetrics)) > 0) {
    adjusted_df <- as.data.frame(result$adjustedMetrics)
    expect_true("metric" %in% colnames(adjusted_df))
    expect_true("study_value" %in% colnames(adjusted_df))
    expect_true("adjusted_value" %in% colnames(adjusted_df))
  }
})

test_that("tree confusion matrix works", {
  
  data("small_sample_tree", package = "ClinicoPath")
  
  # Test confusion matrix generation
  result <- tree(
    data = small_sample_tree,
    vars = c("biomarker_1", "biomarker_2"),
    facs = c("treatment"),
    target = "outcome",
    targetLevel = "Yes",
    train = "cohort",
    trainLevel = "train"
  )
  
  # Check that confusion matrix exists
  expect_true("confusionMatrix" %in% names(result))
  
  # Check confusion matrix content
  if (nrow(as.data.frame(result$confusionMatrix)) > 0) {
    cm_df <- as.data.frame(result$confusionMatrix)
    expect_true("predicted" %in% colnames(cm_df))
    expect_true("actual_disease" %in% colnames(cm_df))
    expect_true("actual_control" %in% colnames(cm_df))
    expect_true("clinical_consequence" %in% colnames(cm_df))
  }
})

test_that("tree feature scaling works", {
  
  data("drug_response", package = "ClinicoPath")
  
  # Test feature scaling
  result <- tree(
    data = drug_response,
    vars = c("gene_expression_1", "gene_expression_2", "protein_level_a"),
    facs = c("mutation_status"),
    target = "drug_response",
    targetLevel = "Responder",
    train = "study_phase",
    trainLevel = "phase1",
    scaleFeatures = TRUE
  )
  
  # Check that analysis completes with feature scaling
  expect_true(inherits(result, "treeResults"))
  expect_true("modelSummary" %in% names(result))
})

test_that("tree clinical context options work", {
  
  data("cardiovascular_risk", package = "ClinicoPath")
  
  # Test different clinical contexts
  contexts <- c("diagnosis", "screening", "treatment", "prognosis")
  
  for (context in contexts) {
    result <- tree(
      data = cardiovascular_risk,
      vars = c("systolic_bp", "cholesterol"),
      facs = c("smoking"),
      target = "cv_event",
      targetLevel = "Yes",
      train = "study_cohort",
      trainLevel = "training",
      clinicalContext = context,
      showInterpretation = TRUE
    )
    
    # Check that analysis completes for each context
    expect_true(inherits(result, "treeResults"))
    expect_true("clinicalInterpretation" %in% names(result))
  }
})

test_that("tree error handling works", {
  
  data("small_sample_tree", package = "ClinicoPath")
  
  # Test missing required variables
  expect_error(
    tree(
      data = small_sample_tree,
      vars = c("nonexistent_var"),
      facs = c("treatment"),
      target = "outcome",
      targetLevel = "Yes"
    )
  )
  
  # Test invalid target level
  expect_error(
    tree(
      data = small_sample_tree,
      vars = c("biomarker_1"),
      facs = c("treatment"),
      target = "outcome",
      targetLevel = "nonexistent_level"
    )
  )
  
  # Test empty data
  empty_data <- data.frame()
  expect_error(
    tree(
      data = empty_data,
      vars = c("biomarker_1"),
      facs = c("treatment"),
      target = "outcome",
      targetLevel = "Yes"
    )
  )
})

test_that("tree handles insufficient data gracefully", {
  
  # Create very small dataset
  tiny_data <- data.frame(
    patient_id = c("P1", "P2", "P3"),
    biomarker = c(1, 2, 3),
    treatment = c("A", "B", "A"),
    outcome = c("No", "Yes", "No")
  )
  
  # Should provide appropriate error message for insufficient data
  expect_error(
    tree(
      data = tiny_data,
      vars = c("biomarker"),
      facs = c("treatment"),
      target = "outcome",
      targetLevel = "Yes"
    ),
    "Minimum 50 cases required"
  )
})

test_that("tree data quality report works", {
  
  data("pathology_diagnosis", package = "ClinicoPath")
  
  # Test data quality report generation
  result <- tree(
    data = pathology_diagnosis,
    vars = c("cell_size", "nuclear_area"),
    facs = c("tumor_type"),
    target = "malignancy",
    targetLevel = "Malignant",
    train = "validation_set",
    trainLevel = "internal"
  )
  
  # Check that data quality report exists
  expect_true("dataQuality" %in% names(result))
  
  # Check that data quality report has content
  if (length(result$dataQuality$content) > 0) {
    expect_true(nchar(result$dataQuality$content) > 0)
  }
})

test_that("tree deployment guidelines work", {
  
  data("cancer_biomarkers", package = "ClinicoPath")
  
  # Test deployment guidelines generation
  result <- tree(
    data = cancer_biomarkers,
    vars = c("PSA", "age"),
    facs = c("grade"),
    target = "diagnosis",
    targetLevel = "cancer",
    train = "cohort",
    trainLevel = "discovery",
    clinicalContext = "screening"
  )
  
  # Check that deployment guidelines exist
  expect_true("deploymentGuidelines" %in% names(result))
  
  # Check deployment guidelines content
  if (length(result$deploymentGuidelines$content) > 0) {
    expect_true(nchar(result$deploymentGuidelines$content) > 0)
  }
})

test_that("tree comprehensive analysis works", {
  
  data("cancer_biomarkers", package = "ClinicoPath")
  
  # Test comprehensive analysis with multiple features
  result <- tree(
    data = cancer_biomarkers,
    vars = c("PSA", "age", "tumor_size"),
    facs = c("grade", "stage"),
    target = "diagnosis",
    targetLevel = "cancer",
    train = "cohort",
    trainLevel = "discovery",
    clinicalMetrics = TRUE,
    featureImportance = TRUE,
    showInterpretation = TRUE,
    riskStratification = TRUE,
    prevalenceAdjustment = TRUE,
    expectedPrevalence = 10,
    clinicalContext = "diagnosis",
    scaleFeatures = TRUE,
    imputeMissing = TRUE
  )
  
  # Check that all requested outputs exist
  expect_true("clinicalMetrics" %in% names(result))
  expect_true("featureImportance" %in% names(result))
  expect_true("clinicalInterpretation" %in% names(result))
  expect_true("riskStratification" %in% names(result))
  expect_true("adjustedMetrics" %in% names(result))
  expect_true("confusionMatrix" %in% names(result))
  expect_true("deploymentGuidelines" %in% names(result))
  expect_true("dataQuality" %in% names(result))
  expect_true("modelSummary" %in% names(result))
  
  # Verify analysis completed successfully
  expect_true(inherits(result, "treeResults"))
})

test_that("tree ROC curve plotting works", {
  
  data("cardiovascular_risk", package = "ClinicoPath")
  
  # Test ROC curve plotting
  result <- tree(
    data = cardiovascular_risk,
    vars = c("systolic_bp", "cholesterol"),
    facs = c("smoking"),
    target = "cv_event",
    targetLevel = "Yes",
    train = "study_cohort",
    trainLevel = "training",
    showROCCurve = TRUE
  )
  
  # Check that ROC plot exists
  expect_true("rocPlot" %in% names(result))
})

test_that("tree calibration plotting works", {
  
  data("drug_response", package = "ClinicoPath")
  
  # Test calibration plotting
  result <- tree(
    data = drug_response,
    vars = c("gene_expression_1", "protein_level_a"),
    facs = c("mutation_status"),
    target = "drug_response",
    targetLevel = "Responder",
    train = "study_phase",
    trainLevel = "phase1",
    showCalibrationPlot = TRUE
  )
  
  # Check that calibration plot exists
  expect_true("calibrationPlot" %in% names(result))
})

test_that("tree clinical utility plotting works", {
  
  data("pathology_diagnosis", package = "ClinicoPath")
  
  # Test clinical utility plotting
  result <- tree(
    data = pathology_diagnosis,
    vars = c("cell_size", "nuclear_area"),
    facs = c("tumor_type"),
    target = "malignancy",
    targetLevel = "Malignant",
    train = "validation_set",
    trainLevel = "internal",
    showClinicalUtility = TRUE,
    costRatio = 2.0
  )
  
  # Check that clinical utility plot exists
  expect_true("clinicalUtilityPlot" %in% names(result))
})

test_that("tree dataset integrity", {
  
  # Test all provided datasets load correctly
  datasets <- c("cancer_biomarkers", "cardiovascular_risk", "pathology_diagnosis", 
                "drug_response", "pediatric_growth", "small_sample_tree")
  
  for (dataset in datasets) {
    data(list = dataset, package = "ClinicoPath")
    dataset_obj <- get(dataset)
    
    # Check dataset has required structure
    expect_true(is.data.frame(dataset_obj))
    expect_true(nrow(dataset_obj) > 0)
    expect_true(ncol(dataset_obj) > 0)
    
    # Check patient ID column exists
    expect_true(any(grepl("id", names(dataset_obj), ignore.case = TRUE)))
    
    # Check target variable exists
    if (dataset == "cancer_biomarkers") {
      expect_true("diagnosis" %in% names(dataset_obj))
    } else if (dataset == "cardiovascular_risk") {
      expect_true("cv_event" %in% names(dataset_obj))
    } else if (dataset == "pathology_diagnosis") {
      expect_true("malignancy" %in% names(dataset_obj))
    } else if (dataset == "drug_response") {
      expect_true("drug_response" %in% names(dataset_obj))
    } else if (dataset == "pediatric_growth") {
      expect_true("growth_delay" %in% names(dataset_obj))
    } else if (dataset == "small_sample_tree") {
      expect_true("outcome" %in% names(dataset_obj))
    }
  }
})

test_that("tree summary datasets work", {
  
  # Test summary datasets
  data("tree_datasets_summary", package = "ClinicoPath")
  data("tree_test_scenarios", package = "ClinicoPath")
  
  # Check summary dataset
  expect_true(is.data.frame(tree_datasets_summary))
  expect_true(nrow(tree_datasets_summary) == 6)
  expect_true("Dataset" %in% names(tree_datasets_summary))
  expect_true("N_Patients" %in% names(tree_datasets_summary))
  
  # Check test scenarios
  expect_true(is.data.frame(tree_test_scenarios))
  expect_true(nrow(tree_test_scenarios) == 12)
  expect_true("Scenario" %in% names(tree_test_scenarios))
  expect_true("Analysis_Type" %in% names(tree_test_scenarios))
})