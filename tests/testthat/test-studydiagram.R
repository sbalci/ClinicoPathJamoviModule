
test_that("studydiagram works with participant_step format", {
    # Create test data
    set.seed(123)
    n <- 100
    data <- data.frame(
        id = 1:n,
        step_excl = sample(c(NA, 1, 2, 3), n, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1)),
        reason = sample(c("Reason A", "Reason B"), n, replace = TRUE)
    )
    
    # Run analysis
    results <- ClinicoPath::studydiagram(
        data = data,
        data_format = "participant_step",
        participant_id = "id",
        step_excluded = "step_excl",
        exclusion_reason_participant = "reason"
    )
    
    # Check results
    expect_s3_class(results, "studydiagramResults")
    expect_true(!is.null(results$summaryTable))
    
    # Check summary table content
    table <- results$summaryTable$asDF()
    expect_true(nrow(table) > 0)
    expect_equal(table$step[1], "Initial")
    expect_equal(table$participants[1], 100)
})

test_that("studydiagram works with step_summary format", {
    # Create summary data
    data <- data.frame(
        step = c("Screening", "Enrollment", "Analysis"),
        count = c(100, 80, 75),
        reasons = c("Ineligible (20)", "Withdrew (5)", "")
    )
    
    # Run analysis
    results <- ClinicoPath::studydiagram(
        data = data,
        data_format = "step_summary",
        step_name = "step",
        participant_count = "count",
        exclusion_reason_summary = "reasons"
    )
    
    # Check results
    expect_s3_class(results, "studydiagramResults")
    
    # Check summary table
    table <- results$summaryTable$asDF()
    expect_equal(nrow(table), 3)
    expect_equal(table$participants[1], 100)
    expect_equal(table$participants[2], 80)
    expect_equal(table$participants[3], 75)
})

test_that("studydiagram works with exclusion_mapping format", {
    # Create mapping data
    data <- data.frame(
        id = 1:50,
        reason = c(rep("Ineligible", 10), rep("Withdrew", 5), rep(NA, 35))
    )
    
    # Run analysis
    results <- ClinicoPath::studydiagram(
        data = data,
        data_format = "exclusion_mapping",
        participant_id_mapping = "id",
        exclusion_reason_mapping = "reason",
        step1_exclusions = "Ineligible",
        step2_exclusions = "Withdrew"
    )
    
    # Check results
    expect_s3_class(results, "studydiagramResults")
    
    # Check summary table
    table <- results$summaryTable$asDF()
    # Initial + 2 steps = 3 rows
    expect_equal(nrow(table), 3)
    expect_equal(table$participants[1], 50)
    # Step 1: 50 - 10 = 40
    expect_equal(table$participants[2], 40)
    # Step 2: 40 - 5 = 35
    expect_equal(table$participants[3], 35)
})

test_that("studydiagram handles different diagram types", {
    data <- data.frame(
        step = c("A", "B"),
        count = c(10, 5),
        reasons = c("Excl", "")
    )
    
    types <- c("consort_standard", "consort_ggplot", "flowchart_standard", "flowchart_ggplot")
    
    for (type in types) {
        results <- ClinicoPath::studydiagram(
            data = data,
            data_format = "step_summary",
            step_name = "step",
            participant_count = "count",
            diagram_type = type
        )
        expect_s3_class(results, "studydiagramResults")
        expect_true(!is.null(results$plot))
    }
})

test_that("studydiagram handles missing data gracefully", {
    # Empty data
    data <- data.frame(id = numeric(0), step = numeric(0))
    
    results <- ClinicoPath::studydiagram(
        data = data,
        data_format = "participant_step",
        participant_id = "id",
        step_excluded = "step"
    )
    
    expect_s3_class(results, "studydiagramResults")
    # Should show welcome message/todo, not crash
})

test_that("studydiagram validates required variables", {
    data <- data.frame(id = 1:10)
    
    # Missing step variable for participant_step format
    results <- ClinicoPath::studydiagram(
        data = data,
        data_format = "participant_step",
        participant_id = "id"
    )
    
    expect_s3_class(results, "studydiagramResults")
    # Should handle missing variable gracefully
})
