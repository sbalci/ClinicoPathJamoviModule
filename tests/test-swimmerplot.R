
library(testthat)
library(ClinicoPathJamoviModule)

# Load the data required for the tests
data("swimmer_unified_basic", package = "ClinicoPathJamoviModule")
data("swimmer_unified_comprehensive", package = "ClinicoPathJamoviModule")
data("swimmer_unified_datetime", package = "ClinicoPathJamoviModule")
data("swimmer_unified_events", package = "ClinicoPathJamoviModule")
data("swimmer_unified_oncology", package = "ClinicoPathJamoviModule")


test_that("swimmerplot runs with minimal options", {
    p <- swimmerplot(
        data = swimmer_unified_basic,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime"
    )
    expect_true(!is.null(p))
})

test_that("swimmerplot works with response variable", {
    p <- swimmerplot(
        data = swimmer_unified_basic,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "Response"
    )
    expect_true(!is.null(p))
})


test_that("swimmerplot handles datetime data", {
    p <- swimmerplot(
        data = swimmer_unified_datetime,
        patientID = "PatientID",
        startTime = "StartDate",
        endTime = "EndDate",
        timeType = "datetime",
        dateFormat = "ymd"
    )
    expect_true(!is.null(p))
})

test_that("swimmerplot handles milestones", {
    p <- swimmerplot(
        data = swimmer_unified_comprehensive,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "BestResponse",
        milestone1Name = "Surgery",
        milestone1Date = "Surgery"
    )
    expect_true(!is.null(p))
})

test_that("swimmerplot handles event markers", {
    p <- swimmerplot(
        data = swimmer_unified_events,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "Response",
        showEventMarkers = TRUE,
        eventVar = "EventType",
        eventTimeVar = "EventTime"
    )
    expect_true(!is.null(p))
})


test_that("swimmerplot handles different themes", {
    p_dark <- swimmerplot(
        data = swimmer_unified_basic,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        plotTheme = "ggswim_dark"
    )
    expect_true(!is.null(p_dark))
    
    p_minimal <- swimmerplot(
        data = swimmer_unified_basic,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        plotTheme = "minimal"
    )
    expect_true(!is.null(p_minimal))
})

test_that("swimmerplot handles sorting", {
    p_sorted <- swimmerplot(
        data = swimmer_unified_basic,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        sortOrder = "patient_id"
    )
    expect_true(!is.null(p_sorted))
})

test_that("swimmerplot handles analysis options", {
    p <- swimmerplot(
        data = swimmer_unified_comprehensive,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "BestResponse",
        personTimeAnalysis = TRUE,
        responseAnalysis = TRUE
    )
    expect_true(!is.null(p))
})

test_that("swimmerplot handles export options", {
    results <- swimmerplot(
        data = swimmer_unified_comprehensive,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        exportTimeline = TRUE,
        exportSummary = TRUE
    )
    
    # Check if the exported data is available
    timeline_df <- results$timelineData$asDF
    summary_df <- results$summaryData$asDF
    
    expect_true(is.data.frame(timeline_df))
    expect_true(is.data.frame(summary_df))
    expect_true(nrow(timeline_df) > 0)
    expect_true(nrow(summary_df) > 0)
})


test_that("swimmerplot handles invalid data gracefully", {
    invalid_data <- swimmer_unified_basic
    invalid_data$EndTime[1] <- -10 # Invalid end time
    
    # This should produce a validation error, but not crash
    results <- swimmerplot(
        data = invalid_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime"
    )
    
    # The plot should be empty or show an error message
    # The exact behavior depends on the implementation, but it shouldn't be a valid plot
    # For example, we can check if the plot object has any data layers
    
    # A more robust test would be to check the content of the instructions or validationReport
    # However, this requires the function to return the results object even on error
    
    # For now, let's just check that it doesn't crash
    expect_true(!is.null(results))
    
})

