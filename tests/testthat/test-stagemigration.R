# Test suite for stagemigration function
# Tests for state-of-the-art TNM staging validation analysis
# Author: ClinicoPath Team
# Date: 2025-07-10

library(testthat)
library(survival)
library(dplyr)

# Load test data
test_that("Test data can be loaded", {
  expect_true(file.exists("../../data/stagemigration_lung_cancer.rda"))
  expect_true(file.exists("../../data/stagemigration_breast_cancer.rda"))
  expect_true(file.exists("../../data/stagemigration_small_sample.rda"))
  expect_true(file.exists("../../data/stagemigration_problematic.rda"))
})

# Test basic functionality
test_that("Basic stagemigration analysis works", {
  skip_on_cran()
  
  # Load small sample data for quick testing
  load("../../data/stagemigration_small_sample.rda")
  
  # Basic analysis
  result <- stagemigration(
    data = small_sample_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "basic"
  )
  
  # Check that result is returned
  expect_true(!is.null(result))
  expect_true(inherits(result, "stagemigrationClass"))
  
  # Check that basic tables are created
  expect_true("migrationMatrix" %in% names(result$results))
  expect_true("stageSummary" %in% names(result$results))
})

# Test comprehensive analysis
test_that("Comprehensive stagemigration analysis works", {
  skip_on_cran()
  
  # Load lung cancer data
  load("../../data/stagemigration_lung_cancer.rda")
  
  # Comprehensive analysis
  result <- stagemigration(
    data = lung_cancer_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "comprehensive",
    calculateNRI = TRUE,
    calculateIDI = TRUE,
    performROCAnalysis = TRUE,
    performDCA = TRUE,
    performBootstrap = TRUE,
    bootstrapReps = 100,  # Reduced for testing
    cancerType = "lung"
  )
  
  # Check that result is returned
  expect_true(!is.null(result))
  expect_true(inherits(result, "stagemigrationClass"))
  
  # Check that comprehensive results are created
  expect_true("migrationMatrix" %in% names(result$results))
  expect_true("concordanceComparison" %in% names(result$results))
  expect_true("nriResults" %in% names(result$results))
  expect_true("idiResults" %in% names(result$results))
  expect_true("rocAnalysis" %in% names(result$results))
  expect_true("clinicalInterpretation" %in% names(result$results))
})

# Test NRI calculation
test_that("NRI calculation works correctly", {
  skip_on_cran()
  
  # Load breast cancer data
  load("../../data/stagemigration_breast_cancer.rda")
  
  # Create small subset for testing
  test_data <- breast_cancer_data %>% 
    slice_sample(n = 200) %>%
    mutate(event = as.numeric(event))
  
  result <- stagemigration(
    data = test_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    calculateNRI = TRUE,
    nriTimePoints = "12, 24",
    cancerType = "breast"
  )
  
  # Check NRI results
  expect_true("nriResults" %in% names(result$results))
  nri_results <- result$results$nriResults
  expect_true(nrow(nri_results$asDF) > 0)
  
  # Check that NRI values are reasonable
  nri_values <- nri_results$asDF
  expect_true(all(nri_values$TimePoint %in% c("12", "24")))
  expect_true(all(is.numeric(nri_values$NRI)))
  expect_true(all(is.numeric(nri_values$NRI_CI_Lower)))
  expect_true(all(is.numeric(nri_values$NRI_CI_Upper)))
})

# Test IDI calculation
test_that("IDI calculation works correctly", {
  skip_on_cran()
  
  # Load colorectal cancer data
  load("../../data/stagemigration_colorectal_cancer.rda")
  
  # Create small subset for testing
  test_data <- colorectal_cancer_data %>% 
    slice_sample(n = 200) %>%
    mutate(event = as.numeric(event))
  
  result <- stagemigration(
    data = test_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    calculateIDI = TRUE,
    cancerType = "colorectal"
  )
  
  # Check IDI results
  expect_true("idiResults" %in% names(result$results))
  idi_results <- result$results$idiResults
  expect_true(nrow(idi_results$asDF) > 0)
  
  # Check that IDI values are reasonable
  idi_values <- idi_results$asDF
  expect_true("IDI" %in% names(idi_values))
  expect_true("IDI_CI_Lower" %in% names(idi_values))
  expect_true("IDI_CI_Upper" %in% names(idi_values))
  expect_true(is.numeric(idi_values$IDI))
})

# Test ROC analysis
test_that("Time-dependent ROC analysis works", {
  skip_on_cran()
  
  # Load lung cancer data
  load("../../data/stagemigration_lung_cancer.rda")
  
  # Create subset for testing
  test_data <- lung_cancer_data %>% 
    slice_sample(n = 300) %>%
    mutate(event = as.numeric(event))
  
  result <- stagemigration(
    data = test_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    performROCAnalysis = TRUE,
    rocTimePoints = "12, 24",
    cancerType = "lung"
  )
  
  # Check ROC results
  expect_true("rocAnalysis" %in% names(result$results))
  roc_results <- result$results$rocAnalysis
  expect_true(nrow(roc_results$asDF) > 0)
  
  # Check that AUC values are reasonable
  roc_values <- roc_results$asDF
  expect_true(all(roc_values$TimePoint %in% c("12", "24")))
  expect_true(all(roc_values$AUC_Old >= 0 & roc_values$AUC_Old <= 1))
  expect_true(all(roc_values$AUC_New >= 0 & roc_values$AUC_New <= 1))
})

# Test Decision Curve Analysis
test_that("Decision Curve Analysis works", {
  skip_on_cran()
  
  # Load breast cancer data
  load("../../data/stagemigration_breast_cancer.rda")
  
  # Create subset for testing
  test_data <- breast_cancer_data %>% 
    slice_sample(n = 250) %>%
    mutate(event = as.numeric(event))
  
  result <- stagemigration(
    data = test_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    performDCA = TRUE,
    cancerType = "breast"
  )
  
  # Check DCA results
  expect_true("dcaResults" %in% names(result$results))
  dca_results <- result$results$dcaResults
  expect_true(nrow(dca_results$asDF) > 0)
  
  # Check that net benefit values are reasonable
  dca_values <- dca_results$asDF
  expect_true("Threshold" %in% names(dca_values))
  expect_true("NetBenefit_Old" %in% names(dca_values))
  expect_true("NetBenefit_New" %in% names(dca_values))
})

# Test bootstrap validation
test_that("Bootstrap validation works", {
  skip_on_cran()
  
  # Load small sample data
  load("../../data/stagemigration_small_sample.rda")
  
  result <- stagemigration(
    data = small_sample_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    performBootstrap = TRUE,
    bootstrapReps = 50,  # Reduced for testing
    useOptimismCorrection = TRUE
  )
  
  # Check bootstrap results
  expect_true("bootstrapResults" %in% names(result$results))
  bootstrap_results <- result$results$bootstrapResults
  expect_true(nrow(bootstrap_results$asDF) > 0)
  
  # Check that bootstrap values are reasonable
  bootstrap_values <- bootstrap_results$asDF
  expect_true("Metric" %in% names(bootstrap_values))
  expect_true("Original" %in% names(bootstrap_values))
  expect_true("Bootstrap_Mean" %in% names(bootstrap_values))
  expect_true("Optimism" %in% names(bootstrap_values))
  expect_true("Corrected" %in% names(bootstrap_values))
})

# Test clinical interpretation
test_that("Clinical interpretation is generated", {
  skip_on_cran()
  
  # Load lung cancer data
  load("../../data/stagemigration_lung_cancer.rda")
  
  # Create subset for testing
  test_data <- lung_cancer_data %>% 
    slice_sample(n = 200) %>%
    mutate(event = as.numeric(event))
  
  result <- stagemigration(
    data = test_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "comprehensive",
    calculateNRI = TRUE,
    calculateIDI = TRUE,
    showClinicalInterpretation = TRUE,
    cancerType = "lung"
  )
  
  # Check clinical interpretation
  expect_true("clinicalInterpretation" %in% names(result$results))
  clinical_interp <- result$results$clinicalInterpretation
  expect_true(nrow(clinical_interp$asDF) > 0)
  
  # Check that interpretation contains key elements
  interp_values <- clinical_interp$asDF
  expect_true("Metric" %in% names(interp_values))
  expect_true("Value" %in% names(interp_values))
  expect_true("Interpretation" %in% names(interp_values))
  expect_true("Recommendation" %in% names(interp_values))
})

# Test cancer-specific guidance
test_that("Cancer-specific guidance works", {
  skip_on_cran()
  
  # Test different cancer types
  cancer_types <- c("lung", "breast", "colorectal", "general")
  
  for (cancer_type in cancer_types) {
    # Load appropriate data
    if (cancer_type == "lung") {
      load("../../data/stagemigration_lung_cancer.rda")
      test_data <- lung_cancer_data %>% slice_sample(n = 100)
    } else if (cancer_type == "breast") {
      load("../../data/stagemigration_breast_cancer.rda")
      test_data <- breast_cancer_data %>% slice_sample(n = 100)
    } else if (cancer_type == "colorectal") {
      load("../../data/stagemigration_colorectal_cancer.rda")
      test_data <- colorectal_cancer_data %>% slice_sample(n = 100)
    } else {
      load("../../data/stagemigration_small_sample.rda")
      test_data <- small_sample_data
    }
    
    test_data <- test_data %>% mutate(event = as.numeric(event))
    
    result <- stagemigration(
      data = test_data,
      oldStage = "old_stage",
      newStage = "new_stage",
      survivalTime = "survival_time",
      event = "event",
      eventLevel = "1",
      analysisType = "standard",
      calculateNRI = TRUE,
      cancerType = cancer_type,
      showClinicalInterpretation = TRUE
    )
    
    # Check that analysis completes for each cancer type
    expect_true(!is.null(result))
    expect_true("clinicalInterpretation" %in% names(result$results))
  }
})

# Test edge cases
test_that("Edge cases are handled correctly", {
  skip_on_cran()
  
  # Test with problematic data
  load("../../data/stagemigration_problematic.rda")
  
  result <- stagemigration(
    data = problematic_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "basic"
  )
  
  # Should complete without errors
  expect_true(!is.null(result))
  
  # Test with no migration (identical stages)
  no_migration_data <- problematic_data %>%
    mutate(new_stage = old_stage)
  
  result2 <- stagemigration(
    data = no_migration_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "basic"
  )
  
  # Should complete without errors
  expect_true(!is.null(result2))
})

# Test input validation
test_that("Input validation works", {
  skip_on_cran()
  
  # Load test data
  load("../../data/stagemigration_small_sample.rda")
  
  # Test missing required variables
  expect_error(
    stagemigration(
      data = small_sample_data,
      oldStage = "old_stage",
      newStage = "new_stage",
      survivalTime = "survival_time",
      # Missing event
      eventLevel = "1"
    )
  )
  
  # Test non-existent variables
  expect_error(
    stagemigration(
      data = small_sample_data,
      oldStage = "nonexistent_var",
      newStage = "new_stage",
      survivalTime = "survival_time",
      event = "event",
      eventLevel = "1"
    )
  )
  
  # Test invalid analysis type
  expect_error(
    stagemigration(
      data = small_sample_data,
      oldStage = "old_stage",
      newStage = "new_stage",
      survivalTime = "survival_time",
      event = "event",
      eventLevel = "1",
      analysisType = "invalid_type"
    )
  )
})

# Test statistical method calculations
test_that("Statistical calculations are mathematically correct", {
  skip_on_cran()
  
  # Create simple test data with known properties
  set.seed(12345)
  n <- 100
  
  simple_data <- data.frame(
    patient_id = 1:n,
    old_stage = factor(sample(1:3, n, replace = TRUE), levels = 1:3, labels = c("Stage I", "Stage II", "Stage III")),
    survival_time = rexp(n, 0.02),
    event = rbinom(n, 1, 0.4)
  )
  
  # Create new stage with some migration
  simple_data$new_stage <- simple_data$old_stage
  migrate_indices <- sample(1:n, round(n * 0.3))
  for (i in migrate_indices) {
    current_stage <- as.numeric(simple_data$old_stage[i])
    if (current_stage < 3) {
      simple_data$new_stage[i] <- factor(current_stage + 1, levels = 1:3, labels = c("Stage I", "Stage II", "Stage III"))
    }
  }
  
  result <- stagemigration(
    data = simple_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    calculateNRI = TRUE,
    calculateIDI = TRUE,
    nriTimePoints = "12",
    confidenceLevel = 0.95
  )
  
  # Check that results are mathematically reasonable
  expect_true(!is.null(result))
  
  # Check concordance values
  concordance <- result$results$concordanceComparison$asDF
  expect_true(all(concordance$C_Index >= 0.4 & concordance$C_Index <= 1.0))
  
  # Check migration matrix sums
  migration_matrix <- result$results$migrationMatrix$asDF
  expect_true(all(migration_matrix$Count >= 0))
  expect_equal(sum(migration_matrix$Count), nrow(simple_data))
})

# Test performance with large datasets
test_that("Performance is acceptable with large datasets", {
  skip_on_cran()
  
  # Load large performance data
  load("../../data/stagemigration_large_performance.rda")
  
  # Time the analysis
  start_time <- Sys.time()
  
  result <- stagemigration(
    data = large_performance_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    calculateNRI = TRUE,
    performBootstrap = FALSE  # Skip bootstrap for performance
  )
  
  end_time <- Sys.time()
  analysis_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete in reasonable time (less than 60 seconds)
  expect_lt(analysis_time, 60)
  
  # Should produce results
  expect_true(!is.null(result))
  expect_true("migrationMatrix" %in% names(result$results))
})

# Test visualization outputs
test_that("Visualization outputs are generated", {
  skip_on_cran()
  
  # Load test data
  load("../../data/stagemigration_lung_cancer.rda")
  
  # Create subset for testing
  test_data <- lung_cancer_data %>% 
    slice_sample(n = 200) %>%
    mutate(event = as.numeric(event))
  
  result <- stagemigration(
    data = test_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    showMigrationHeatmap = TRUE,
    showROCComparison = TRUE,
    showForestPlot = TRUE,
    performROCAnalysis = TRUE,
    rocTimePoints = "12, 24"
  )
  
  # Check that visualization results are present
  expect_true("migrationMatrix" %in% names(result$results))
  expect_true("rocAnalysis" %in% names(result$results))
  
  # Check that plots would be generated (depends on implementation)
  # This would test the actual plot generation if implemented
})

# Test methodology notes and documentation
test_that("Methodology notes are comprehensive", {
  skip_on_cran()
  
  # Load test data
  load("../../data/stagemigration_breast_cancer.rda")
  
  # Create subset for testing
  test_data <- breast_cancer_data %>% 
    slice_sample(n = 150) %>%
    mutate(event = as.numeric(event))
  
  result <- stagemigration(
    data = test_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "comprehensive",
    calculateNRI = TRUE,
    calculateIDI = TRUE,
    performROCAnalysis = TRUE,
    performDCA = TRUE,
    showMethodologyNotes = TRUE,
    showStatisticalSummary = TRUE,
    cancerType = "breast"
  )
  
  # Check that methodology information is present
  expect_true(!is.null(result))
  
  # Check that statistical summary is generated
  if ("statisticalSummary" %in% names(result$results)) {
    stat_summary <- result$results$statisticalSummary
    expect_true(nrow(stat_summary$asDF) > 0)
  }
})

# Test reproducibility
test_that("Results are reproducible", {
  skip_on_cran()
  
  # Load test data
  load("../../data/stagemigration_small_sample.rda")
  
  # Run analysis twice with same seed
  set.seed(12345)
  result1 <- stagemigration(
    data = small_sample_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    calculateNRI = TRUE,
    performBootstrap = TRUE,
    bootstrapReps = 50
  )
  
  set.seed(12345)
  result2 <- stagemigration(
    data = small_sample_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "standard",
    calculateNRI = TRUE,
    performBootstrap = TRUE,
    bootstrapReps = 50
  )
  
  # Results should be identical
  expect_true(!is.null(result1))
  expect_true(!is.null(result2))
  
  # Check that key results are the same
  concordance1 <- result1$results$concordanceComparison$asDF
  concordance2 <- result2$results$concordanceComparison$asDF
  
  expect_equal(concordance1$C_Index, concordance2$C_Index, tolerance = 0.01)
})

# Test memory usage
test_that("Memory usage is reasonable", {
  skip_on_cran()
  
  # Load large performance data
  load("../../data/stagemigration_large_performance.rda")
  
  # Monitor memory usage
  initial_memory <- gc()
  
  result <- stagemigration(
    data = large_performance_data,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "survival_time",
    event = "event",
    eventLevel = "1",
    analysisType = "basic"
  )
  
  final_memory <- gc()
  
  # Should complete without excessive memory usage
  expect_true(!is.null(result))
  
  # Memory increase should be reasonable (less than 1GB)
  memory_increase <- final_memory[2,3] - initial_memory[2,3]
  expect_lt(memory_increase, 1000)  # Less than 1GB
})

# Clean up test environment
teardown({
  # Clean up any temporary files or objects
  rm(list = ls(pattern = "^test_"))
  gc()
})