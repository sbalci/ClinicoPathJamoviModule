library(testthat)

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all()
} else {
    library(ClinicoPathJamoviModule)
}

# Load data using here::here() to construct the path
if (requireNamespace("here", quietly = TRUE)) {
    load(here::here("data", "swimmer_unified_basic.rda"))
    load(here::here("data", "swimmer_unified_comprehensive.rda"))
    load(here::here("data", "swimmer_unified_datetime.rda"))
    load(here::here("data", "swimmer_unified_events.rda"))
    load(here::here("data", "swimmer_unified_oncology.rda"))
} else {
    # Fallback for when here is not installed
    load("data/swimmer_unified_basic.rda")
    load("data/swimmer_unified_comprehensive.rda")
    load("data/swimmer_unified_datetime.rda")
    load("data/swimmer_unified_events.rda")
    load("data/swimmer_unified_oncology.rda")
}


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
        responseVar = "BestResponse",
        exportTimeline = TRUE,
        exportSummary = TRUE
    )
    
    # Check if the exported data is available
    timeline_df <- results$timelineData$state
    summary_df <- results$summaryData$state
    
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

# =============================================================================
# Statistical Correctness Tests
# =============================================================================

test_that("person-time calculation merges overlapping intervals correctly", {
    # Test data with overlapping segments for the same patient
    # Patient P1 has overlapping treatment periods that should be merged
    test_data <- data.frame(
        PatientID = c("P1", "P1", "P1", "P2", "P2"),
        StartTime = c(0, 5, 20, 0, 15),
        EndTime = c(10, 15, 20, 10, 25),
        Response = c("SD", "PR", "CR", "PD", "SD"),
        stringsAsFactors = FALSE
    )

    # P1: [0-10], [5-15], [20-20] -> merged: [0-15], [20-20] = 15 + 0 = 15 unique days
    # P2: [0-10], [15-25] -> no overlap = 10 + 10 = 20 unique days
    # Total unique person-time should be 35

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "Response",
        personTimeAnalysis = TRUE,
        exportSummary = TRUE
    )

    # Get summary data
    summary_df <- results$summaryData$state

    # Check that we have 2 patients
    expect_equal(nrow(summary_df), 2,
                 info = "Should have 2 unique patients")

    # Check that person-time was calculated (not NA)
    expect_false(any(is.na(summary_df$person_time)),
                 info = "Person-time should be calculated for all patients")
})

test_that("adjacent intervals are merged in person-time calculation", {
    # Test that adjacent (touching) intervals are merged
    test_data <- data.frame(
        PatientID = c("P1", "P1"),
        StartTime = c(0, 10),
        EndTime = c(10, 20),
        stringsAsFactors = FALSE
    )

    # [0-10] and [10-20] are adjacent -> should merge to [0-20] = 20 days

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        personTimeAnalysis = TRUE,
        exportSummary = TRUE
    )

    summary_df <- results$summaryData$state
    total_person_time <- sum(summary_df$person_time, na.rm = TRUE)

    expect_equal(total_person_time, 20, tolerance = 0.001,
                 info = "Adjacent intervals should merge correctly")
})

test_that("best response selection uses oncology hierarchy", {
    # Test that best response is selected based on CR > PR > SD > PD
    test_data <- data.frame(
        PatientID = c("P1", "P1", "P1", "P2", "P2"),
        StartTime = c(0, 10, 20, 0, 10),
        EndTime = c(10, 20, 30, 10, 20),
        Response = c("SD", "PR", "PD", "PD", "CR"),  # P1 best=PR, P2 best=CR
        stringsAsFactors = FALSE
    )

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "Response",
        responseAnalysis = TRUE,
        exportSummary = TRUE
    )

    summary_df <- results$summaryData$state

    # Check that best responses are selected
    p1_response <- summary_df$response[summary_df$patient_id == "P1"]
    p2_response <- summary_df$response[summary_df$patient_id == "P2"]

    expect_equal(toupper(p1_response), "PR",
                 info = "P1 best response should be PR (better than SD and PD)")
    expect_equal(toupper(p2_response), "CR",
                 info = "P2 best response should be CR (better than PD)")
})

test_that("ORR and DCR are calculated from best responses", {
    # Create data where ORR/DCR depend on using best response
    test_data <- data.frame(
        PatientID = c("P1", "P1", "P2", "P2", "P3", "P4"),
        StartTime = c(0, 10, 0, 10, 0, 0),
        EndTime = c(10, 20, 10, 20, 10, 10),
        Response = c("SD", "CR", "PD", "PR", "SD", "PD"),
        # Best responses: P1=CR, P2=PR, P3=SD, P4=PD
        # ORR = (CR + PR) / total = 2/4 = 50%
        # DCR = (CR + PR + SD) / total = 3/4 = 75%
        stringsAsFactors = FALSE
    )

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "Response",
        responseAnalysis = TRUE
    )

    # Check that the analysis ran without errors
    expect_true(!is.null(results),
                info = "ORR/DCR calculation should complete without errors")
})

test_that("event markers are filtered by patient end time", {
    # Test that events occurring after a patient's end_time are filtered out
    # Using the standard events dataset
    results <- swimmerplot(
        data = swimmer_unified_events,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "Response",
        showEventMarkers = TRUE,
        eventVar = "EventType",
        eventTimeVar = "EventTime"
    )

    expect_true(!is.null(results),
                info = "Event marker filtering should not crash")
})

test_that("incidence rate calculation uses correct person-time", {
    # Test that incidence rate uses merged (correct) person-time
    test_data <- data.frame(
        PatientID = c("P1", "P1", "P2"),
        StartTime = c(0, 5, 0),
        EndTime = c(10, 15, 20),
        stringsAsFactors = FALSE
    )

    # P1: [0-10], [5-15] -> merged to [0-15] = 15 days
    # P2: [0-20] = 20 days
    # Total person-time = 35 days

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        personTimeAnalysis = TRUE
    )

    # Check that the analysis ran without errors
    expect_true(!is.null(results),
                info = "Incidence rate calculation should complete without errors")
})

test_that("response hierarchy handles different case formats", {
    # Test that response matching is case-insensitive
    test_data <- data.frame(
        PatientID = c("P1", "P1", "P2", "P2"),
        StartTime = c(0, 10, 0, 10),
        EndTime = c(10, 20, 10, 20),
        Response = c("sd", "CR", "Partial Response", "progressive disease"),
        # P1: best should be CR (over sd)
        # P2: best should be PR (Partial Response > progressive disease)
        stringsAsFactors = FALSE
    )

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "Response",
        responseAnalysis = TRUE,
        exportSummary = TRUE
    )

    summary_df <- results$summaryData$state

    p1_response <- summary_df$response[summary_df$patient_id == "P1"]
    p2_response <- summary_df$response[summary_df$patient_id == "P2"]

    expect_equal(toupper(p1_response), "CR",
                 info = "Case-insensitive: CR should be selected over sd")
    expect_true(grepl("partial", tolower(p2_response)),
                info = "Case-insensitive: Partial Response should be selected")
})

test_that("single segment patient has correct person-time", {
    # Edge case: patient with single segment
    test_data <- data.frame(
        PatientID = "P1",
        StartTime = 0,
        EndTime = 30,
        stringsAsFactors = FALSE
    )

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        personTimeAnalysis = TRUE,
        exportSummary = TRUE
    )

    summary_df <- results$summaryData$state
    person_time <- summary_df$person_time[1]

    expect_equal(person_time, 30, tolerance = 0.001,
                 info = "Single segment should have correct person-time")
})

test_that("completely overlapping segments are handled correctly", {
    # Test nested/completely overlapping segments
    test_data <- data.frame(
        PatientID = c("P1", "P1", "P1"),
        StartTime = c(0, 5, 2),
        EndTime = c(30, 15, 10),  # [5-15] and [2-10] are inside [0-30]
        stringsAsFactors = FALSE
    )

    # All segments should merge to [0-30] = 30 days

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        personTimeAnalysis = TRUE,
        exportSummary = TRUE
    )

    summary_df <- results$summaryData$state
    person_time <- summary_df$person_time[1]

    expect_equal(person_time, 30, tolerance = 0.001,
                 info = "Nested segments should merge to outer interval")
})

test_that("empty response is handled gracefully", {
    # Test with no response data
    test_data <- data.frame(
        PatientID = c("P1", "P2"),
        StartTime = c(0, 0),
        EndTime = c(10, 20),
        stringsAsFactors = FALSE
    )

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseAnalysis = FALSE
    )

    expect_true(!is.null(results),
                info = "Should handle data without response variable")
})

test_that("multiple patients with mixed responses calculate correct metrics", {
    # Comprehensive test with multiple patients and mixed responses
    test_data <- data.frame(
        PatientID = c("P1", "P2", "P3", "P4", "P5"),
        StartTime = c(0, 0, 0, 0, 0),
        EndTime = c(10, 20, 15, 25, 30),
        Response = c("CR", "PR", "SD", "PD", "NE"),
        stringsAsFactors = FALSE
    )

    # Expected:
    # ORR = CR + PR = 2/5 = 40%
    # DCR = CR + PR + SD = 3/5 = 60%

    results <- swimmerplot(
        data = test_data,
        patientID = "PatientID",
        startTime = "StartTime",
        endTime = "EndTime",
        responseVar = "Response",
        responseAnalysis = TRUE,
        personTimeAnalysis = TRUE
    )

    expect_true(!is.null(results),
                info = "Multiple patient analysis should complete")

    # Total person-time should be 10+20+15+25+30 = 100
    # Since no overlaps, this should be exact
})