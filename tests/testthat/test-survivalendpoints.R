test_that("survivalendpoints calculates PFS correctly with actual event dates", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # Test case: Patient who progresses at month 6 and dies at month 12
    # PFS should be 6 months (time to FIRST event), not 12 months

    data <- data.frame(
        patient_id = c("P001", "P002", "P003"),
        start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
        progression_date = as.Date(c("2023-07-01", NA, "2023-09-01")),
        death_date = as.Date(c("2024-01-01", "2023-08-15", NA)),
        last_followup = as.Date(c("2024-06-01", "2023-12-01", "2024-02-01"))
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            progressionDate = "progression_date",
            deathDate = "death_date",
            lastFollowup = "last_followup",
            calculatePFS = TRUE,
            showDerivedData = TRUE
        )

        derived_table <- results$derivedEndpoints$asDF

        # P001: Progressed at month 6, died at month 12
        # PFS should be ~6 months (181 days / 30.44 ≈ 5.95 months), event=1
        expect_true(derived_table$pfs_time[1] >= 5.5 && derived_table$pfs_time[1] <= 6.5)
        expect_equal(derived_table$pfs_event[1], 1)

        # P002: No progression, died at 6.5 months
        # PFS should be ~6.5 months (death is first event), event=1
        expect_true(derived_table$pfs_time[2] >= 6.0 && derived_table$pfs_time[2] <= 7.0)
        expect_equal(derived_table$pfs_event[2], 1)

        # P003: Progressed at 6 months, alive
        # PFS should be ~6 months (progression is first event), event=1
        expect_true(derived_table$pfs_time[3] >= 5.5 && derived_table$pfs_time[3] <= 6.5)
        expect_equal(derived_table$pfs_event[3], 1)
    })
})


test_that("survivalendpoints calculates OS correctly using death date", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # OS should use death date specifically, not progression date

    data <- data.frame(
        patient_id = c("P001", "P002", "P003"),
        start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
        progression_date = as.Date(c("2023-07-01", "2023-05-01", NA)),
        death_date = as.Date(c("2024-01-01", NA, NA)),
        last_followup = as.Date(c("2024-06-01", "2023-12-01", "2024-02-01"))
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            progressionDate = "progression_date",
            deathDate = "death_date",
            lastFollowup = "last_followup",
            calculateOS = TRUE,
            showDerivedData = TRUE
        )

        derived_table <- results$derivedEndpoints$asDF

        # P001: Died at 12 months
        # OS should be ~12 months, event=1
        expect_true(derived_table$os_time[1] >= 11.5 && derived_table$os_time[1] <= 12.5)
        expect_equal(derived_table$os_event[1], 1)

        # P002: Alive (no death date)
        # OS should be censored at last followup (~10 months), event=0
        expect_true(derived_table$os_time[2] >= 9.5 && derived_table$os_time[2] <= 10.5)
        expect_equal(derived_table$os_event[2], 0)

        # P003: Alive (no death date)
        # OS should be censored at last followup (~11 months), event=0
        expect_true(derived_table$os_time[3] >= 10.5 && derived_table$os_time[3] <= 11.5)
        expect_equal(derived_table$os_event[3], 0)
    })
})


test_that("survivalendpoints calculates TTP correctly (censors death without progression)", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # TTP should use progression date only, censoring death without progression

    data <- data.frame(
        patient_id = c("P001", "P002", "P003"),
        start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
        progression_date = as.Date(c("2023-07-01", NA, "2023-09-01")),
        death_date = as.Date(c(NA, "2023-08-15", NA)),
        last_followup = as.Date(c("2024-06-01", "2023-12-01", "2024-02-01"))
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            progressionDate = "progression_date",
            deathDate = "death_date",
            lastFollowup = "last_followup",
            calculateTTP = TRUE,
            showDerivedData = TRUE
        )

        derived_table <- results$derivedEndpoints$asDF

        # P001: Progressed at 6 months (no death)
        # TTP should be ~6 months, event=1
        expect_true(derived_table$ttp_time[1] >= 5.5 && derived_table$ttp_time[1] <= 6.5)
        expect_equal(derived_table$ttp_event[1], 1)

        # P002: Died without progression
        # TTP should be censored at death (~6.5 months), event=0 (death is NOT progression)
        expect_true(derived_table$ttp_time[2] >= 6.0 && derived_table$ttp_time[2] <= 7.0)
        expect_equal(derived_table$ttp_event[2], 0)

        # P003: Progressed at 6 months
        # TTP should be ~6 months, event=1
        expect_true(derived_table$ttp_time[3] >= 5.5 && derived_table$ttp_time[3] <= 6.5)
        expect_equal(derived_table$ttp_event[3], 1)
    })
})


test_that("survivalendpoints calculates DOR correctly using progression date", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # DOR should use progression date, NOT last followup

    data <- data.frame(
        patient_id = c("P001", "P002", "P003"),
        start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
        response_date = as.Date(c("2023-03-01", "2023-04-01", "2023-05-01")),
        progression_date = as.Date(c("2023-09-01", NA, "2023-11-01")),
        last_followup = as.Date(c("2024-06-01", "2024-01-01", "2024-02-01"))
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            responseDate = "response_date",
            progressionDate = "progression_date",
            lastFollowup = "last_followup",
            calculateDOR = TRUE,
            showDerivedData = TRUE
        )

        derived_table <- results$derivedEndpoints$asDF

        # P001: Response at Mar 1, progressed at Sep 1 (6 months)
        # DOR should be ~6 months (NOT 15 months to last followup), event=1
        expect_true(derived_table$dor_time[1] >= 5.5 && derived_table$dor_time[1] <= 6.5)
        expect_equal(derived_table$dor_event[1], 1)

        # P002: Response at Apr 1, no progression
        # DOR should be censored at last followup (~9 months), event=0
        expect_true(derived_table$dor_time[2] >= 8.5 && derived_table$dor_time[2] <= 9.5)
        expect_equal(derived_table$dor_event[2], 0)

        # P003: Response at May 1, progressed at Nov 1 (6 months)
        # DOR should be ~6 months, event=1
        expect_true(derived_table$dor_time[3] >= 5.5 && derived_table$dor_time[3] <= 6.5)
        expect_equal(derived_table$dor_event[3], 1)
    })
})


test_that("survivalendpoints handles legacy event indicators (backward compatibility)", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # Test that module still works with old format (event indicators only, no dates)

    data <- data.frame(
        patient_id = c("P001", "P002", "P003"),
        start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
        last_followup = as.Date(c("2024-06-01", "2023-12-01", "2024-02-01")),
        progression_event = c(1, 0, 1),
        death_event = c(0, 1, 0)
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            lastFollowup = "last_followup",
            progressionEvent = "progression_event",
            deathEvent = "death_event",
            calculatePFS = TRUE,
            calculateOS = TRUE,
            showDerivedData = TRUE
        )

        derived_table <- results$derivedEndpoints$asDF

        # Legacy mode should still calculate times (using lastFollowup as event time)
        # P001: progression=1, death=0
        expect_true(!is.na(derived_table$pfs_time[1]))
        expect_equal(derived_table$pfs_event[1], 1)

        # P002: progression=0, death=1
        expect_true(!is.na(derived_table$os_time[2]))
        expect_equal(derived_table$os_event[2], 1)
    })
})


test_that("survivalendpoints handles missing data gracefully", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # Test edge cases with NA values

    data <- data.frame(
        patient_id = c("P001", "P002", "P003", "P004"),
        start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01")),
        progression_date = as.Date(c(NA, NA, "2023-09-01", NA)),
        death_date = as.Date(c(NA, NA, NA, NA)),
        last_followup = as.Date(c("2024-06-01", "2023-12-01", "2024-02-01", "2024-03-01"))
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            progressionDate = "progression_date",
            deathDate = "death_date",
            lastFollowup = "last_followup",
            calculatePFS = TRUE,
            calculateOS = TRUE,
            showDerivedData = TRUE
        )

        derived_table <- results$derivedEndpoints$asDF

        # All patients should have valid times (no -Inf from pmax bug)
        expect_true(all(!is.infinite(derived_table$pfs_time)))
        expect_true(all(!is.infinite(derived_table$os_time)))

        # Patients with no events should be censored (event=0)
        expect_equal(derived_table$pfs_event[1], 0)
        expect_equal(derived_table$pfs_event[2], 0)
        expect_equal(derived_table$os_event[1], 0)
        expect_equal(derived_table$os_event[2], 0)

        # P003 has progression, so PFS event=1
        expect_equal(derived_table$pfs_event[3], 1)

        # P004 has no events, so censored
        expect_equal(derived_table$pfs_event[4], 0)
    })
})


test_that("survivalendpoints preserves alphanumeric patient IDs", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # Test that patient IDs like "PAT-001", "P123" are preserved (not converted to NA)

    data <- data.frame(
        patient_id = c("PAT-001", "P123", "TRIAL-999"),
        start_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
        progression_date = as.Date(c("2023-07-01", NA, "2023-09-01")),
        death_date = as.Date(c(NA, "2023-08-15", NA)),
        last_followup = as.Date(c("2024-06-01", "2023-12-01", "2024-02-01"))
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            progressionDate = "progression_date",
            deathDate = "death_date",
            lastFollowup = "last_followup",
            calculatePFS = TRUE,
            showDerivedData = TRUE
        )

        derived_table <- results$derivedEndpoints$asDF

        # Patient IDs should be preserved exactly
        expect_equal(derived_table$patientId[1], "PAT-001")
        expect_equal(derived_table$patientId[2], "P123")
        expect_equal(derived_table$patientId[3], "TRIAL-999")

        # None should be NA
        expect_true(all(!is.na(derived_table$patientId)))
    })
})


test_that("survivalendpoints handles numeric time inputs", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # Test with numeric time values instead of dates

    data <- data.frame(
        patient_id = c("P001", "P002", "P003"),
        start_time = c(0, 0, 0),
        progression_time = c(6.2, NA, 8.5),
        death_time = c(12.1, 7.3, NA),
        last_followup_time = c(18.0, 15.0, 20.0)
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_time",
            progressionDate = "progression_time",
            deathDate = "death_time",
            lastFollowup = "last_followup_time",
            inputType = "numeric",
            timeUnit = "months",
            calculatePFS = TRUE,
            calculateOS = TRUE,
            showDerivedData = TRUE
        )

        derived_table <- results$derivedEndpoints$asDF

        # P001: PFS should be 6.2 (progression before death)
        expect_equal(derived_table$pfs_time[1], 6.2, tolerance = 0.1)
        expect_equal(derived_table$pfs_event[1], 1)

        # P001: OS should be 12.1 (death time)
        expect_equal(derived_table$os_time[1], 12.1, tolerance = 0.1)
        expect_equal(derived_table$os_event[1], 1)

        # P002: PFS should be 7.3 (death, no progression)
        expect_equal(derived_table$pfs_time[2], 7.3, tolerance = 0.1)
        expect_equal(derived_table$pfs_event[2], 1)

        # P003: PFS should be 8.5 (progression, alive)
        expect_equal(derived_table$pfs_time[3], 8.5, tolerance = 0.1)
        expect_equal(derived_table$pfs_event[3], 1)
    })
})


test_that("survivalendpoints summary statistics are calculated correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # Test that summary statistics table produces valid median and CI

    data <- data.frame(
        patient_id = paste0("P", sprintf("%03d", 1:30)),
        start_date = as.Date("2023-01-01"),
        # Vary progression dates (days 150-240)
        progression_date = as.Date("2023-01-01") + c(seq(150, 240, length.out = 15), rep(NA, 15)),
        # Vary death dates (days 300-450)
        death_date = as.Date("2023-01-01") + c(seq(300, 450, length.out = 15), rep(NA, 15)),
        last_followup = as.Date("2024-01-01") + seq(0, 29) * 7
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            progressionDate = "progression_date",
            deathDate = "death_date",
            lastFollowup = "last_followup",
            calculatePFS = TRUE,
            calculateOS = TRUE,
            showSummaryStats = TRUE
        )

        summary_table <- results$summaryStats$asDF

        # Should have rows for PFS and OS
        expect_true(nrow(summary_table) >= 2)

        # Check that median times are not NA
        pfs_row <- summary_table[summary_table$endpoint == "PFS", ]
        os_row <- summary_table[summary_table$endpoint == "OS", ]

        expect_true(!is.na(pfs_row$median_time))
        expect_true(!is.na(os_row$median_time))

        # Check that confidence intervals are valid (if available)
        if (!is.na(pfs_row$ci_lower) && !is.na(pfs_row$ci_upper)) {
            expect_true(pfs_row$ci_lower <= pfs_row$median_time)
            expect_true(pfs_row$median_time <= pfs_row$ci_upper)
        }
    })
})


test_that("survivalendpoints event rates table calculates percentages correctly", {
    skip_if_not_installed("jmvcore")
    skip_if_not_installed("survival")

    # Test event rates and censoring percentages

    data <- data.frame(
        patient_id = paste0("P", sprintf("%03d", 1:20)),
        start_date = as.Date("2023-01-01"),
        progression_date = as.Date(c(rep("2023-07-01", 10), rep(NA, 10))),
        death_date = as.Date(c(rep("2024-01-01", 5), rep(NA, 15))),
        last_followup = as.Date("2024-06-01")
    )

    expect_no_error({
        results <- survivalendpoints(
            data = data,
            patientId = "patient_id",
            startDate = "start_date",
            progressionDate = "progression_date",
            deathDate = "death_date",
            lastFollowup = "last_followup",
            calculatePFS = TRUE,
            calculateOS = TRUE,
            showEventRates = TRUE
        )

        event_table <- results$eventRates$asDF

        # Check PFS row
        pfs_row <- event_table[event_table$endpoint == "PFS", ]
        expect_equal(pfs_row$total, 20)
        # PFS events: 10 progressions + 5 deaths (some overlap) = at least 10 events
        expect_true(pfs_row$events >= 10)
        expect_true(pfs_row$event_rate >= 50.0)  # At least 50% event rate
        expect_true(pfs_row$censoring_rate <= 50.0)  # At most 50% censored

        # Check OS row
        os_row <- event_table[event_table$endpoint == "OS", ]
        expect_equal(os_row$total, 20)
        expect_equal(os_row$events, 5)  # 5 deaths
        expect_equal(os_row$event_rate, 25.0, tolerance = 0.1)  # 5/20 = 25%
        expect_equal(os_row$censoring_rate, 75.0, tolerance = 0.1)  # 15/20 = 75%
    })
})
