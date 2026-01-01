# Tests for consortdiagram function
source("../../R/consortdiagram.h.R")
source("../../R/consortdiagram.b.R")

test_that("consortdiagram works with basic single-arm inputs", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # skip_on_cran()

    # Create simple single-arm trial data
    n <- 100
    test_data <- data.frame(
        id = 1:n,
        screening_fail = c(rep(NA, 90), rep("Protocol violation", 10)),
        enrollment_fail = c(rep(NA, 85), rep("Withdrew consent", 5), rep(NA, 10)),
        stringsAsFactors = FALSE
    )

    # Run analysis
    results <- consortdiagram(
        data = test_data,
        participant_id = "id",
        screening_exclusions = "screening_fail",
        enrollment_exclusions = "enrollment_fail"
    )

    # Check that results object is created
    expect_s3_class(results, "consortdiagramResults")

    # Check that flow summary table exists
    expect_true(!is.null(results$flowSummary))

    # Check basic counts are correct
    flow_table <- results$flowSummary$asDF

    # Initial screening should have 100 participants
    initial_row <- flow_table[flow_table$stage == "Assessed for eligibility", ]
    expect_equal(initial_row$n_remaining, 100)

    # After screening exclusions: 100 - 10 = 90
    screening_row <- flow_table[flow_table$stage == "Enrolled", ]
    expect_equal(screening_row$n_remaining, 90)

    # After enrollment exclusions: 90 - 5 = 85
    enrollment_row <- flow_table[flow_table$stage == "Randomized/Enrolled", ]
    expect_equal(enrollment_row$n_remaining, 85)
})

test_that("consortdiagram handles multi-arm randomized trial correctly", {
    # skip_on_cran()

    # Create multi-arm trial data
    n <- 150
    test_data <- data.frame(
        id = 1:n,
        arm = c(rep("Treatment A", 50), rep("Treatment B", 50), rep("Control", 50)),
        screening_fail = c(rep(NA, 140), rep("Ineligible", 10)),
        allocation_fail = c(rep(NA, 125), rep("Withdrew", 5), rep(NA, 10), rep("Lost", 5), rep(NA, 5)),
        followup_fail = c(rep(NA, 115), rep("Adverse event", 5), rep(NA, 30)),
        stringsAsFactors = FALSE
    )

    # Run analysis with randomization
    results <- consortdiagram(
        data = test_data,
        participant_id = "id",
        screening_exclusions = "screening_fail",
        randomization_var = "arm",
        allocation_exclusions = "allocation_fail",
        followup_exclusions = "followup_fail"
    )

    # Check that results object is created
    expect_s3_class(results, "consortdiagramResults")

    # Check arm comparison table exists
    expect_true(!is.null(results$armSummary))

    arm_table <- results$armSummary$asDF

    # Should have 3 arms
    expect_equal(nrow(arm_table), 3)

    # Check that all arms have positive participant counts
    expect_true(all(arm_table$allocated > 0))
    expect_true(all(arm_table$received >= 0))
    expect_true(all(arm_table$analyzed >= 0))

    # Retention rates should be between 0 and 100
    expect_true(all(arm_table$retention_rate >= 0 & arm_table$retention_rate <= 100))
})

test_that("consortdiagram handles NA correctly (NA means continued)", {
    # skip_on_cran()

    # Create data where NA explicitly means "participant continued"
    test_data <- data.frame(
        id = 1:50,
        screening_fail = c(rep(NA, 45), rep("Excluded", 5)),  # 45 continued, 5 excluded
        enrollment_fail = c(rep(NA, 42), rep("Withdrew", 3), rep(NA, 5))  # Of 45, 42 continued, 3 excluded
    )

    results <- consortdiagram(
        data = test_data,
        participant_id = "id",
        screening_exclusions = "screening_fail",
        enrollment_exclusions = "enrollment_fail"
    )

    flow_table <- results$flowSummary$asDF

    # Initial: 50 participants
    initial_row <- flow_table[1, ]
    expect_equal(initial_row$n_remaining, 50)

    # After screening: 50 - 5 = 45
    screening_row <- flow_table[2, ]
    expect_equal(screening_row$n_remaining, 45)

    # After enrollment: 45 - 3 = 42 (not 45 - 8 if it incorrectly counted NAs from excluded participants)
    enrollment_row <- flow_table[3, ]
    expect_equal(enrollment_row$n_remaining, 42)
})

test_that("consortdiagram prevents double-counting in arm attrition", {
    # skip_on_cran()

    # Create scenario where same participant could be double-counted
    test_data <- data.frame(
        id = 1:60,
        arm = c(rep("Treatment", 30), rep("Control", 30)),
        allocation_fail = c(rep("Withdrew", 5), rep(NA, 25), rep("Ineligible", 3), rep(NA, 27)),
        # Fix followup_fail to have exclusions for survivors (indices 6-30 for Treatment)
        # Indices 1-5 already excluded.
        # Let's exclude 6-7 at followup.
        followup_fail = c(rep(NA, 5), rep("Lost", 2), rep(NA, 23), rep(NA, 30)),
        # Analysis fail: exclude 8 at analysis
        analysis_fail = c(rep(NA, 7), rep("Missing data", 1), rep(NA, 22), rep(NA, 30))
    )

    results <- consortdiagram(
        data = test_data,
        participant_id = "id",
        randomization_var = "arm",
        allocation_exclusions = "allocation_fail",
        followup_exclusions = "followup_fail",
        analysis_exclusions = "analysis_fail"
    )

    arm_table <- results$armSummary$asDF

    # Treatment arm: allocated = 30
    treatment_row <- arm_table[arm_table$arm == "Treatment", ]
    expect_equal(treatment_row$allocated, 30)

    # After allocation: 30 - 5 = 25
    expect_equal(treatment_row$received, 25)

    # After followup: 25 - 2 = 23 (only count those who reached this stage)
    expect_equal(treatment_row$completed_followup, 23)

    # After analysis: 23 - 1 = 22 (only count those who reached this stage)
    expect_equal(treatment_row$analyzed, 22)

    # No counts should be negative
    expect_true(all(treatment_row$received >= 0))
    expect_true(all(treatment_row$completed_followup >= 0))
    expect_true(all(treatment_row$analyzed >= 0))

    # Monotonic decrease: allocated >= received >= completed_followup >= analyzed
    expect_true(treatment_row$allocated >= treatment_row$received)
    expect_true(treatment_row$received >= treatment_row$completed_followup)
    expect_true(treatment_row$completed_followup >= treatment_row$analyzed)
})

test_that("consortdiagram calculates exclusion percentages correctly", {
    # skip_on_cran()

    # Create data with known exclusion rates
    test_data <- data.frame(
        id = 1:100,
        screening_fail = c(rep("Ineligible", 20), rep(NA, 80))  # 20% of 100 entering
    )

    results <- consortdiagram(
        data = test_data,
        participant_id = "id",
        screening_exclusions = "screening_fail",
        show_exclusion_details = TRUE
    )

    excl_table <- results$exclusionBreakdown$asDF

    # Find the exclusion row for "Ineligible"
    ineligible_row <- excl_table[excl_table$reason == "Ineligible", ]

    # Should be 20 participants excluded
    expect_equal(ineligible_row$count, 20)

    # Percentage should be 20/100 = 0.20 (20%)
    # Not 20/80 = 0.25 (25%) which would be wrong denominator
    expect_equal(ineligible_row$percentage, 0.20, tolerance = 0.01)
})

test_that("consortdiagram handles missing required inputs", {
    # skip_on_cran()

    test_data <- data.frame(
        id = 1:50,
        screening_fail = rep(NA, 50)
    )

    # Missing participant_id should handle gracefully
    results <- consortdiagram(
        data = test_data,
        participant_id = NULL,
        screening_exclusions = "screening_fail"
    )

    # Should create results object even if incomplete
    expect_s3_class(results, "consortdiagramResults")
})

test_that("consortdiagram handles all participants excluded scenario", {
    # skip_on_cran()

    # Edge case: everyone excluded at screening
    test_data <- data.frame(
        id = 1:20,
        screening_fail = rep("Failed criteria", 20)
    )

    results <- consortdiagram(
        data = test_data,
        participant_id = "id",
        screening_exclusions = "screening_fail"
    )

    flow_table <- results$flowSummary$asDF

    # After screening, should have 0 remaining
    screening_row <- flow_table[flow_table$stage == "Enrolled", ]
    expect_equal(screening_row$n_remaining, 0)

    # Retention should be 0%
    expect_equal(screening_row$pct_retained, 0)
})

test_that("consortdiagram handles no exclusions scenario", {
    # skip_on_cran()

    # Edge case: no one excluded
    test_data <- data.frame(
        id = 1:50,
        screening_fail = rep(NA, 50),
        enrollment_fail = rep(NA, 50)
    )

    results <- consortdiagram(
        data = test_data,
        participant_id = "id",
        screening_exclusions = "screening_fail",
        enrollment_exclusions = "enrollment_fail"
    )

    flow_table <- results$flowSummary$asDF

    # All stages should have 50 participants
    expect_true(all(flow_table$n_remaining == 50))

    # All retention rates should be 100% (1.0)
    expect_true(all(flow_table$pct_retained == 1))
})

test_that("consortdiagram handles multiple exclusion reasons per stage", {
    # skip_on_cran()

    # Multiple reasons for exclusion at same stage
    test_data <- data.frame(
        id = 1:100,
        reason1 = c(rep("Reason A", 10), rep(NA, 90)),
        reason2 = c(rep(NA, 10), rep("Reason B", 15), rep(NA, 75)),
        reason3 = c(rep(NA, 25), rep("Reason C", 5), rep(NA, 70))
    )

    results <- consortdiagram(
        data = test_data,
        participant_id = "id",
        screening_exclusions = c("reason1", "reason2", "reason3"),
        show_exclusion_details = TRUE
    )

    excl_table <- results$exclusionBreakdown$asDF

    # Should have 3 distinct reasons listed
    expect_equal(nrow(excl_table), 3)

    # Counts should be correct
    reason_a <- excl_table[excl_table$reason == "Reason A", ]
    expect_equal(reason_a$count, 10)

    reason_b <- excl_table[excl_table$reason == "Reason B", ]
    expect_equal(reason_b$count, 15)

    reason_c <- excl_table[excl_table$reason == "Reason C", ]
    expect_equal(reason_c$count, 5)

    # Total excluded should be 30
    flow_table <- results$flowSummary$asDF

    # After screening: 100 - 30 = 70
    screening_row <- flow_table[flow_table$stage == "Enrolled", ]
    expect_equal(screening_row$n_remaining, 70)
})

test_that("consortdiagram handles participant IDs as characters and numbers", {
    # skip_on_cran()

    # Test with character IDs
    test_data_char <- data.frame(
        id = paste0("PAT", sprintf("%03d", 1:50)),
        screening_fail = c(rep(NA, 45), rep("Excluded", 5)),
        stringsAsFactors = FALSE
    )

    results_char <- consortdiagram(
        data = test_data_char,
        participant_id = "id",
        screening_exclusions = "screening_fail"
    )

    expect_s3_class(results_char, "consortdiagramResults")

    # Test with numeric IDs
    test_data_num <- data.frame(
        id = 1:50,
        screening_fail = c(rep(NA, 45), rep("Excluded", 5))
    )

    results_num <- consortdiagram(
        data = test_data_num,
        participant_id = "id",
        screening_exclusions = "screening_fail"
    )

    expect_s3_class(results_num, "consortdiagramResults")

    # Both should give same counts
    flow_char <- results_char$flowSummary$asDF
    flow_num <- results_num$flowSummary$asDF

    expect_equal(flow_char$n_remaining, flow_num$n_remaining)
})
