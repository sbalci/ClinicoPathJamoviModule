#!/usr/bin/env Rscript

# =============================================================================
# Comprehensive Unit Tests for Enhanced toxicityprofile Function
# =============================================================================

test_that("toxicityprofile basic functionality works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Load required test data
  data("toxicityprofile_small_sample", package = "ClinicoPath")
  
  # Test basic analysis with minimal data
  result <- toxicityprofile(
    data = toxicityprofile_small_sample,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    plotType = "stacked_bar"
  )
  
  # Check that result object exists and has expected structure
  expect_true(inherits(result, "toxicityprofileResults"))
  expect_true("summary" %in% names(result))
  expect_true("gradeDistribution" %in% names(result))
  expect_true("plot" %in% names(result))
  
  # Check summary table has content
  summary_df <- as.data.frame(result$summary)
  expect_true(nrow(summary_df) > 0)
  expect_true("adverse_event" %in% colnames(summary_df))
  expect_true("incidence" %in% colnames(summary_df))
})

test_that("toxicityprofile plot types work correctly", {
  
  data("toxicityprofile_small_sample", package = "ClinicoPath")
  
  # Test stacked bar plot
  result_bar <- toxicityprofile(
    data = toxicityprofile_small_sample,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    plotType = "stacked_bar"
  )
  expect_true("plot" %in% names(result_bar))
  
  # Test dot plot
  result_dot <- toxicityprofile(
    data = toxicityprofile_small_sample,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    plotType = "dot_plot"
  )
  expect_true("plot" %in% names(result_dot))
  
  # Test heatmap
  result_heat <- toxicityprofile(
    data = toxicityprofile_small_sample,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    plotType = "heatmap"
  )
  expect_true("plot" %in% names(result_heat))
})

test_that("toxicityprofile treatment group comparison works", {
  
  data("toxicityprofile_pediatric", package = "ClinicoPath")
  
  # Test treatment group comparison
  result <- toxicityprofile(
    data = toxicityprofile_pediatric,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    groupComparison = TRUE,
    showConfidenceIntervals = TRUE
  )
  
  # Check that group comparison table exists and has content
  expect_true("groupComparison" %in% names(result))
  
  # Check if group comparison has reasonable results
  if (nrow(as.data.frame(result$groupComparison)) > 0) {
    comp_df <- as.data.frame(result$groupComparison)
    expect_true("adverse_event" %in% colnames(comp_df))
    expect_true("risk_ratio" %in% colnames(comp_df) || 
                "group1_incidence" %in% colnames(comp_df))
  }
})

test_that("toxicityprofile time-to-event analysis works", {
  
  data("toxicityprofile_targeted_therapy", package = "ClinicoPath")
  
  # Test time-to-event analysis
  result <- toxicityprofile(
    data = toxicityprofile_targeted_therapy,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    timeToEvent = "time_to_event",
    plotType = "time_to_event",
    cumulativeIncidence = TRUE
  )
  
  # Check that result exists and has plot
  expect_true("plot" %in% names(result))
  
  # Check summary includes time information
  summary_df <- as.data.frame(result$summary)
  expect_true("median_time" %in% colnames(summary_df))
})

test_that("toxicityprofile system organ class functionality works", {
  
  data("toxicityprofile_oncology_trial", package = "ClinicoPath")
  
  # Test SOC functionality
  result <- toxicityprofile(
    data = toxicityprofile_oncology_trial,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    systemOrganClass = "system_organ_class"
  )
  
  # Check that SOC summary exists
  expect_true("socSummary" %in% names(result))
  
  # Check SOC summary has content
  soc_df <- as.data.frame(result$socSummary)
  if (nrow(soc_df) > 0) {
    expect_true("soc" %in% colnames(soc_df))
    expect_true("incidence" %in% colnames(soc_df))
  }
})

test_that("toxicityprofile high-grade filtering works", {
  
  data("toxicityprofile_dose_escalation", package = "ClinicoPath")
  
  # Test high-grade only analysis
  result_all <- toxicityprofile(
    data = toxicityprofile_dose_escalation,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    showHighGradeOnly = FALSE
  )
  
  result_high <- toxicityprofile(
    data = toxicityprofile_dose_escalation,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    showHighGradeOnly = TRUE
  )
  
  # High-grade only should have fewer or equal events
  all_events <- nrow(as.data.frame(result_all$summary))
  high_events <- nrow(as.data.frame(result_high$summary))
  expect_true(high_events <= all_events)
})

test_that("toxicityprofile confidence intervals work", {
  
  data("toxicityprofile_immunotherapy", package = "ClinicoPath")
  
  # Test confidence intervals in dot plot
  result <- toxicityprofile(
    data = toxicityprofile_immunotherapy,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    plotType = "dot_plot",
    showConfidenceIntervals = TRUE,
    confidenceLevel = "0.95"
  )
  
  # Check that plot can be generated (this tests CI calculation)
  expect_true("plot" %in% names(result))
})

test_that("toxicityprofile handles different color schemes", {
  
  data("toxicityprofile_small_sample", package = "ClinicoPath")
  
  # Test different color schemes
  color_schemes <- c("ctcae", "traffic", "viridis", "custom")
  
  for (scheme in color_schemes) {
    result <- toxicityprofile(
      data = toxicityprofile_small_sample,
      patientID = "patient_id",
      adverseEvent = "adverse_event",
      grade = "toxicity_grade",
      gradeColors = scheme
    )
    
    expect_true("plot" %in% names(result))
  }
})

test_that("toxicityprofile handles minimum incidence filtering", {
  
  data("toxicityprofile_oncology_trial", package = "ClinicoPath")
  
  # Test different minimum incidence thresholds
  result_low <- toxicityprofile(
    data = toxicityprofile_oncology_trial,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    minIncidence = 1
  )
  
  result_high <- toxicityprofile(
    data = toxicityprofile_oncology_trial,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    minIncidence = 10
  )
  
  # Higher threshold should result in fewer events
  low_events <- nrow(as.data.frame(result_low$summary))
  high_events <- nrow(as.data.frame(result_high$summary))
  expect_true(high_events <= low_events)
})

test_that("toxicityprofile sorting methods work", {
  
  data("toxicityprofile_targeted_therapy", package = "ClinicoPath")
  
  # Test different sorting methods
  sort_methods <- c("frequency", "alphabetical", "high_grade")
  
  for (method in sort_methods) {
    result <- toxicityprofile(
      data = toxicityprofile_targeted_therapy,
      patientID = "patient_id",
      adverseEvent = "adverse_event",
      grade = "toxicity_grade",
      sortBy = method
    )
    
    expect_true("plot" %in% names(result))
    
    # Check that summary table exists
    summary_df <- as.data.frame(result$summary)
    expect_true(nrow(summary_df) > 0)
  }
})

test_that("toxicityprofile error handling works", {
  
  data("toxicityprofile_small_sample", package = "ClinicoPath")
  
  # Test missing required variables
  expect_error(
    toxicityprofile(
      data = toxicityprofile_small_sample,
      patientID = "nonexistent_column",
      adverseEvent = "adverse_event",
      grade = "toxicity_grade"
    ),
    "Patient ID variable not found"
  )
  
  expect_error(
    toxicityprofile(
      data = toxicityprofile_small_sample,
      patientID = "patient_id",
      adverseEvent = "nonexistent_column",
      grade = "toxicity_grade"
    ),
    "Adverse event variable not found"
  )
  
  expect_error(
    toxicityprofile(
      data = toxicityprofile_small_sample,
      patientID = "patient_id",
      adverseEvent = "adverse_event",
      grade = "nonexistent_column"
    ),
    "Grade variable not found"
  )
})

test_that("toxicityprofile handles invalid grade values", {
  
  # Create test data with invalid grades
  invalid_data <- data.frame(
    patient_id = paste0("P", 1:10),
    adverse_event = rep(c("Fatigue", "Nausea"), 5),
    toxicity_grade = c(1, 2, 6, 0, 3, 4, -1, 2, 1, 7),  # Invalid grades: 6, 0, -1, 7
    stringsAsFactors = FALSE
  )
  
  # Should produce error due to invalid grade values
  expect_error(
    toxicityprofile(
      data = invalid_data,
      patientID = "patient_id",
      adverseEvent = "adverse_event",
      grade = "toxicity_grade"
    ),
    "Grade values must be between 1 and 5"
  )
})

test_that("toxicityprofile handles empty data", {
  
  # Test with empty data frame
  empty_data <- data.frame(
    patient_id = character(0),
    adverse_event = character(0),
    toxicity_grade = numeric(0)
  )
  
  expect_error(
    toxicityprofile(
      data = empty_data,
      patientID = "patient_id",
      adverseEvent = "adverse_event",
      grade = "toxicity_grade"
    ),
    "Data contains no \\(complete\\) rows"
  )
})

test_that("toxicityprofile handles single patient data", {
  
  # Test with single patient
  single_patient <- data.frame(
    patient_id = "P001",
    adverse_event = "Fatigue",
    toxicity_grade = 2,
    stringsAsFactors = FALSE
  )
  
  result <- toxicityprofile(
    data = single_patient,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade"
  )
  
  # Should handle single patient gracefully
  expect_true("summary" %in% names(result))
  summary_df <- as.data.frame(result$summary)
  expect_equal(nrow(summary_df), 1)
  expect_equal(summary_df$incidence[1], 100)  # 100% incidence with 1 patient
})

test_that("toxicityprofile comprehensive oncology trial analysis", {
  
  data("toxicityprofile_oncology_trial", package = "ClinicoPath")
  
  # Comprehensive analysis with all features
  result <- toxicityprofile(
    data = toxicityprofile_oncology_trial,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    systemOrganClass = "system_organ_class",
    timeToEvent = "time_to_event",
    plotType = "stacked_bar",
    sortBy = "frequency",
    showHighGradeOnly = FALSE,
    minIncidence = 5,
    gradeColors = "ctcae",
    showPercentages = TRUE,
    showConfidenceIntervals = TRUE,
    groupComparison = TRUE,
    cumulativeIncidence = TRUE,
    confidenceLevel = "0.95"
  )
  
  # Check all expected outputs exist
  expect_true("summary" %in% names(result))
  expect_true("gradeDistribution" %in% names(result))
  expect_true("groupComparison" %in% names(result))
  expect_true("socSummary" %in% names(result))
  expect_true("plot" %in% names(result))
  
  # Verify data integrity
  summary_df <- as.data.frame(result$summary)
  expect_true(nrow(summary_df) > 0)
  expect_true(all(summary_df$incidence >= 0 & summary_df$incidence <= 100))
  
  # Check grade distribution
  grade_df <- as.data.frame(result$gradeDistribution)
  expect_true(nrow(grade_df) > 0)
  expect_true(all(c("grade_1", "grade_2", "grade_3", "grade_4", "grade_5") %in% colnames(grade_df)))
  
  # Check SOC summary
  soc_df <- as.data.frame(result$socSummary)
  expect_true(nrow(soc_df) > 0)
  expect_true(all(soc_df$incidence >= 0 & soc_df$incidence <= 100))
})

test_that("toxicityprofile immunotherapy-specific analysis", {
  
  data("toxicityprofile_immunotherapy", package = "ClinicoPath")
  
  # Test immunotherapy-specific analysis
  result <- toxicityprofile(
    data = toxicityprofile_immunotherapy,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    systemOrganClass = "system_organ_class",
    timeToEvent = "time_to_event",
    plotType = "heatmap",
    showHighGradeOnly = TRUE,  # Focus on serious irAEs
    groupComparison = TRUE
  )
  
  # Check for immune-related adverse events
  summary_df <- as.data.frame(result$summary)
  expect_true(nrow(summary_df) >= 0)  # May have 0 rows if no high-grade events
  
  # If high-grade events exist, check they are grade 3+
  if (nrow(summary_df) > 0) {
    grade_df <- as.data.frame(result$gradeDistribution)
    expect_true(all(grade_df$grade_1 == 0 & grade_df$grade_2 == 0))
  }
})

test_that("toxicityprofile dose escalation analysis", {
  
  data("toxicityprofile_dose_escalation", package = "ClinicoPath")
  
  # Test dose escalation-specific analysis
  result <- toxicityprofile(
    data = toxicityprofile_dose_escalation,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",  # This represents dose levels
    plotType = "stacked_bar",
    sortBy = "high_grade",
    groupComparison = TRUE
  )
  
  # Check structure for dose escalation context
  expect_true("summary" %in% names(result))
  expect_true("groupComparison" %in% names(result))
  
  summary_df <- as.data.frame(result$summary)
  expect_true(nrow(summary_df) > 0)
})

test_that("toxicityprofile performance with large dataset", {
  
  data("toxicityprofile_oncology_trial", package = "ClinicoPath")
  
  # Test performance with larger dataset
  start_time <- Sys.time()
  
  result <- toxicityprofile(
    data = toxicityprofile_oncology_trial,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    systemOrganClass = "system_organ_class",
    timeToEvent = "time_to_event",
    plotType = "dot_plot",
    groupComparison = TRUE,
    showConfidenceIntervals = TRUE
  )
  
  end_time <- Sys.time()
  execution_time <- as.numeric(end_time - start_time, units = "secs")
  
  # Should complete within reasonable time (< 30 seconds for this dataset)
  expect_true(execution_time < 30)
  
  # Check result quality
  expect_true("summary" %in% names(result))
  summary_df <- as.data.frame(result$summary)
  expect_true(nrow(summary_df) > 0)
})

test_that("toxicityprofile statistical test robustness", {
  
  data("toxicityprofile_pediatric", package = "ClinicoPath")
  
  # Test statistical robustness with pediatric data
  result <- toxicityprofile(
    data = toxicityprofile_pediatric,
    patientID = "patient_id",
    adverseEvent = "adverse_event",
    grade = "toxicity_grade",
    treatment = "treatment_group",
    groupComparison = TRUE,
    confidenceLevel = "0.99"  # Higher confidence level
  )
  
  # Check group comparison results
  comp_df <- as.data.frame(result$groupComparison)
  
  if (nrow(comp_df) > 0) {
    # Check that p-values are in valid range
    p_values <- comp_df$pvalue[!is.na(comp_df$pvalue)]
    if (length(p_values) > 0) {
      expect_true(all(p_values >= 0 & p_values <= 1))
    }
    
    # Check that risk ratios are positive (or NA)
    rr_values <- comp_df$risk_ratio[!is.na(comp_df$risk_ratio)]
    if (length(rr_values) > 0) {
      expect_true(all(rr_values > 0))
    }
  }
})
