# ═══════════════════════════════════════════════════════════
# Edge Case Tests: survival
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the survival jamovi function

library(testthat)

# Load test data
data(survival_test, package = "ClinicoPath")

test_that("survival handles missing values in time", {
  test_data_na <- survival_test
  test_data_na$elapsedtime[1:5] <- NA

  result <- run_survival(
    data = test_data_na,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(result, "survivalResults")
  expect_equal(sum(result$medianTable$asDF$records), nrow(test_data_na) - 5)
})

test_that("survival handles missing values in outcome", {
  test_data_na <- survival_test
  test_data_na$outcome[1:5] <- NA

  result <- run_survival(
    data = test_data_na,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_match(result$warnings$content, "Missing outcome", ignore.case = TRUE)
})

test_that("survival handles all censored data", {
  test_data_censored <- survival_test
  test_data_censored$outcome <- 0

  result <- run_survival(
    data = test_data_censored,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_equal(sum(result$medianTable$asDF$events), 0)
  expect_equal(result$coxTable$rowCount, 0)
})

test_that("survival handles all events (no censoring)", {
  test_data_events <- survival_test
  test_data_events$outcome <- 1

  # Should complete but may warn
  result <- run_survival(
    data = test_data_events,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles zero time values", {
  test_data_zero <- survival_test
  test_data_zero$elapsedtime[1:3] <- 0

  result <- run_survival(
    data = test_data_zero,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(result, "survivalResults")
})

test_that("survival handles negative time values", {
  test_data_negative <- survival_test
  test_data_negative$elapsedtime[1:3] <- -5

  # Should error about invalid time values
  expect_error(
    run_survival(
      data = test_data_negative,
      elapsedtime = "elapsedtime",
      outcome = "outcome",
      explanatory = "treatment"
    ),
    regexp = "negative|invalid.*time|positive",
    ignore.case = TRUE
  )
})

test_that("survival handles very small time values", {
  test_data_small <- survival_test
  test_data_small$elapsedtime <- test_data_small$elapsedtime * 0.01  # Very small times

  result <- run_survival(
    data = test_data_small,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles very large time values", {
  test_data_large <- survival_test
  test_data_large$elapsedtime <- test_data_large$elapsedtime * 100  # Very large times

  result <- run_survival(
    data = test_data_large,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival keeps descriptive output for a single observation", {
  single_row <- survival_test[1, ]

  result <- run_survival(
    data = single_row,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(result, "survivalResults")
  expect_gt(result$medianTable$rowCount, 0)
})

test_that("survival handles very small sample size", {
  small_data <- survival_test[1:5, ]

  # May complete but should warn about small sample
  result <- run_survival(
    data = small_data,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival keeps descriptive output for a single group", {
  test_data_one_group <- survival_test
  test_data_one_group$treatment <- "Treatment A"  # All same

  result <- run_survival(
    data = test_data_one_group,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  expect_s3_class(result, "survivalResults")
  expect_gt(result$medianTable$rowCount, 0)
})

test_that("survival handles unbalanced groups", {
  # Create very unbalanced groups
  test_data_unbalanced <- survival_test
  test_data_unbalanced$treatment[1:195] <- "Treatment A"
  test_data_unbalanced$treatment[196:200] <- "Treatment B"

  result <- run_survival(
    data = test_data_unbalanced,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  # May warn about unbalanced groups
  expect_s3_class(result, "survivalResults")
})

test_that("survival handles group with no events", {
  # Create data where one group has no events
  test_data_no_event_group <- survival_test
  test_data_no_event_group$outcome[test_data_no_event_group$treatment == "Treatment B"] <- 0

  result <- run_survival(
    data = test_data_no_event_group,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  # Should complete with warning
  expect_s3_class(result, "survivalResults")
})

test_that("survival handles landmark beyond maximum follow-up gracefully", {
  data(survival_landmark, package = "ClinicoPath")

  expect_error(
    run_survival(
      data = survival_landmark,
      elapsedtime = "elapsedtime",
      outcome = "outcome",
      uselandmark = TRUE,
      landmark = ceiling(max(survival_landmark$elapsedtime, na.rm = TRUE)) + 1,
      explanatory = "treatment"
    ),
    regexp = "Landmark.*beyond|no patients remain",
    ignore.case = TRUE
  )
})

test_that("survival handles invalid date formats", {
  data(survival_dates, package = "ClinicoPath")

  # Use wrong date format
  expect_error(
    run_survival(
      data = survival_dates,
      tint = TRUE,
      dxdate = "dxdate",
      fudate = "fudate",
      outcome = "outcome",
      explanatory = "treatment",
      timetypedata = "mdy",  # Wrong format for YMD data
      timetypeoutput = "months"
    )
  )
})

test_that("survival handles duplicate observations", {
  # Create duplicates
  test_data_dup <- rbind(survival_test, survival_test[1:10, ])

  result <- run_survival(
    data = test_data_dup,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles tied event times", {
  # Create many tied event times
  test_data_ties <- survival_test
  test_data_ties$elapsedtime <- round(test_data_ties$elapsedtime / 10) * 10  # Round to nearest 10

  result <- run_survival(
    data = test_data_ties,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles invalid outcome levels for competing risks", {
  data(survival_competing, package = "ClinicoPath")

  # Specify outcome level that doesn't exist
  expect_error(
    run_survival(
      data = survival_competing,
      elapsedtime = "elapsedtime",
      outcome = "outcome",
      outcomeLevel = "Invalid Level",
      analysistype = "compete",
      explanatory = "treatment"
    ),
    regexp = "level.*not found|invalid.*level",
    ignore.case = TRUE
  )
})

test_that("survival suppresses stratified Cox without a strata variable", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    stratified_cox = TRUE
  )
  expect_equal(result$coxTable$rowCount, 0)
  expect_true(length(result$coxTable$notes) > 0)
})

test_that("survival handles extreme y-axis values for plots", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    ybegin_plot = 0.5,  # Start at 50%
    yend_plot = 0.6     # End at 60% (narrow range)
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles very short plot time", {
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    sc = TRUE,
    endplot = 1  # Only 1 month
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival suppresses Cox for a constant stratification variable", {
  test_data_const <- survival_test
  test_data_const$constant_var <- factor("A")  # All same

  result <- run_survival(
    data = test_data_const,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    stratified_cox = TRUE,
    strata_variable = "constant_var"
  )
  expect_equal(result$coxTable$rowCount, 0)
  expect_true(length(result$coxTable$notes) > 0)
})

test_that("survival handles outcome with more than 4 levels", {
  # Create outcome with 5 levels
  test_data_many_levels <- survival_test
  test_data_many_levels$outcome <- sample(1:5, nrow(survival_test), replace = TRUE)

  result <- run_survival(
    data = test_data_many_levels,
    elapsedtime = "elapsedtime",
    outcome = "outcome"
  )

  # Should handle by converting to binary or warning
  expect_s3_class(result, "survivalResults")
})

test_that("survival handles RMST with tau = 0", {
  # RMST with tau=0 should use default (75th percentile)
  result <- run_survival(
    data = survival_test,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    rmst_analysis = TRUE,
    rmst_tau = 0
  )

  expect_s3_class(result, "survivalResults")
})

test_that("survival handles person-time with zero events in interval", {
  # Create data where one interval has no events
  test_data_no_interval_events <- survival_test
  test_data_no_interval_events$outcome[test_data_no_interval_events$elapsedtime < 12] <- 0

  result <- run_survival(
    data = test_data_no_interval_events,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    person_time = TRUE,
    time_intervals = "12, 24, 36"
  )

  expect_s3_class(result, "survivalResults")
})
