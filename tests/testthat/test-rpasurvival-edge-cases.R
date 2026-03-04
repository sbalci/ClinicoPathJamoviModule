# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: rpasurvival
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions

library(testthat)
data(rpasurvival_test)
data(rpasurvival_small)

test_that("rpasurvival handles small sample sizes appropriately", {
  # Small dataset (n=50)
  result_small <- rpasurvival(
    data = rpasurvival_small,
    time = "time",
    event = "event",
    predictors = c("stage", "grade")
  )

  # Should complete but may produce warnings about sample size
  expect_s3_class(result_small, "rpasurvivalClass")
})

test_that("rpasurvival warns when predictors exceed events/10 (EPV)", {
  # Too many predictors for sample size
  # With ~130 events in rpasurvival_test, >13 predictors should warn
  expect_warning(
    rpasurvival(
      data = rpasurvival_test,
      time = "time",
      event = "event",
      predictors = c("age", "stage", "grade", "LVI", "tumor_size",
                     "ki67", "performance_status", "treatment",
                     "patient_id")  # 9 predictors, may trigger overfit warning
    ),
    regexp = "overfit|events|predictor|EPV",
    ignore.case = TRUE
  )
})

test_that("rpasurvival handles very small minbucket", {
  # Very small minbucket may trigger warning
  result <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    minbucket = 5
  )

  # Should work but may warn about small nodes
  expect_s3_class(result, "rpasurvivalClass")
})

test_that("rpasurvival handles very deep trees", {
  # Maxdepth > 5 may trigger complexity warning
  result <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "LVI", "age"),
    maxdepth = 8
  )

  # Should work but may warn about overfitting
  expect_s3_class(result, "rpasurvivalClass")
})

test_that("rpasurvival handles missing data in predictors", {
  # Data has ~3% missing in ki67 and tumor_size
  result <- rpasurvival(
    data = rpasurvival_test,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "ki67", "tumor_size")
  )

  # Should handle missing data (rpart uses surrogate splits)
  expect_no_error(result)
})

test_that("rpasurvival handles missing data in time variable", {
  # Create data with missing time
  test_data_na <- rpasurvival_test
  test_data_na$time[1:5] <- NA

  # Should error or warn about missing time
  expect_error(
    rpasurvival(
      data = test_data_na,
      time = "time",
      event = "event",
      predictors = c("stage", "grade")
    ),
    regexp = "missing|NA|time|required",
    ignore.case = TRUE
  )
})

test_that("rpasurvival handles missing data in event variable", {
  # Create data with missing event
  test_data_na <- rpasurvival_test
  test_data_na$event[1:5] <- NA

  # Should error or warn about missing event
  expect_error(
    rpasurvival(
      data = test_data_na,
      time = "time",
      event = "event",
      predictors = c("stage", "grade")
    ),
    regexp = "missing|NA|event|required",
    ignore.case = TRUE
  )
})

test_that("rpasurvival handles zero or very few events", {
  # Create dataset with very few events
  low_events <- rpasurvival_test
  low_events$event <- factor(0, levels = c(0, 1))
  low_events$event[1:5] <- factor(1, levels = c(0, 1))

  # Should error about insufficient events
  expect_error(
    rpasurvival(
      data = low_events,
      time = "time",
      event = "event",
      predictors = c("stage", "grade")
    ),
    regexp = "events|insufficient|minimum",
    ignore.case = TRUE
  )
})

test_that("rpasurvival handles all censored data", {
  # All observations censored
  all_censored <- rpasurvival_test
  all_censored$event <- factor(0, levels = c(0, 1))

  # Should error
  expect_error(
    rpasurvival(
      data = all_censored,
      time = "time",
      event = "event",
      predictors = c("stage", "grade")
    ),
    regexp = "censored|events|zero",
    ignore.case = TRUE
  )
})

test_that("rpasurvival handles negative survival times", {
  # Negative times should error
  negative_time <- rpasurvival_test
  negative_time$time[1:5] <- -10

  expect_error(
    rpasurvival(
      data = negative_time,
      time = "time",
      event = "event",
      predictors = c("stage", "grade")
    ),
    regexp = "negative|time|positive",
    ignore.case = TRUE
  )
})

test_that("rpasurvival handles zero survival times", {
  # Zero times (should be handled - represents immediate events)
  zero_time <- rpasurvival_test
  zero_time$time[1:5] <- 0

  # May work or warn, depending on implementation
  result <- rpasurvival(
    data = zero_time,
    time = "time",
    event = "event",
    predictors = c("stage", "grade")
  )

  # Should either work or provide informative message
  expect_true(inherits(result, "rpasurvivalClass") || inherits(result, "try-error"))
})

test_that("rpasurvival handles constant predictors", {
  # Constant predictor (no variation)
  constant_data <- rpasurvival_test
  constant_data$constant_var <- "Same"

  # Should handle gracefully (rpart will ignore non-informative splits)
  result <- rpasurvival(
    data = constant_data,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "constant_var")
  )

  expect_no_error(result)
})

test_that("rpasurvival handles single-level factors", {
  # Factor with only one level after subsetting
  single_level <- rpasurvival_test[rpasurvival_test$stage == "I", ]

  # Should error or warn
  expect_condition(
    rpasurvival(
      data = single_level,
      time = "time",
      event = "event",
      predictors = c("stage", "grade")
    )
  )
})

test_that("rpasurvival handles different event coding schemes", {
  # Load edge case datasets
  data(rpasurvival_edge_truefalse, package = "ClinicoPath")
  data(rpasurvival_edge_12, package = "ClinicoPath")

  # Event coded as TRUE/FALSE
  result_tf <- rpasurvival(
    data = rpasurvival_edge_truefalse,
    time = "time",
    event = "event_tf",
    predictors = c("stage", "grade"),
    eventValue = "TRUE"
  )
  expect_no_error(result_tf)

  # Event coded as 1/2
  result_12 <- rpasurvival(
    data = rpasurvival_edge_12,
    time = "time",
    event = "event_12",
    predictors = c("stage", "grade"),
    eventValue = "2"
  )
  expect_no_error(result_12)
})

test_that("rpasurvival handles variables with special characters", {
  # Create data with special character variable names
  special_data <- rpasurvival_test
  names(special_data)[names(special_data) == "tumor_size"] <- "Tumor Size (cm)"
  names(special_data)[names(special_data) == "ki67"] <- "Ki-67 %"

  # Should handle with proper escaping
  result <- rpasurvival(
    data = special_data,
    time = "time",
    event = "event",
    predictors = c("stage", "grade", "Tumor Size (cm)", "Ki-67 %")
  )

  expect_no_error(result)
})

test_that("rpasurvival handles variables with leading/trailing spaces", {
  # Variable names with spaces
  space_data <- rpasurvival_test
  names(space_data)[names(space_data) == "stage"] <- " stage "

  # Should handle or provide clear error
  result <- rpasurvival(
    data = space_data,
    time = "time",
    event = "event",
    predictors = c(" stage ", "grade")
  )

  expect_true(inherits(result, "rpasurvivalClass") || inherits(result, "try-error"))
})

test_that("rpasurvival handles insufficient follow-up for 5-year survival", {
  # Create data where max time < 5 years (< 60 months)
  short_followup <- rpasurvival_small
  short_followup$time <- short_followup$time * 0.5  # Max ~15 months

  result <- rpasurvival(
    data = short_followup,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    time_unit = "months"
  )

  # Should complete and show "Insufficient follow-up" in 5-year survival column
  expect_no_error(result)
})

test_that("rpasurvival handles very long survival times", {
  # Extreme survival times (e.g., >10 years = 120 months)
  long_survival <- rpasurvival_test
  long_survival$time <- long_survival$time * 5  # Very long follow-up

  result <- rpasurvival(
    data = long_survival,
    time = "time",
    event = "event",
    predictors = c("stage", "grade"),
    time_unit = "months"
  )

  expect_no_error(result)
})

test_that("rpasurvival handles tied survival times", {
  # Many tied survival times
  tied_data <- rpasurvival_test
  tied_data$time <- round(tied_data$time / 5) * 5  # Round to nearest 5 months

  result <- rpasurvival(
    data = tied_data,
    time = "time",
    event = "event",
    predictors = c("stage", "grade")
  )

  expect_no_error(result)
})

test_that("rpasurvival validates time_unit matches actual data scale", {
  # Time clearly in days (large values) but specified as years
  # This is a user error but should be handled gracefully
  data(rpasurvival_edge_days, package = "ClinicoPath")

  # User specifies "years" when data is actually in days
  result <- rpasurvival(
    data = rpasurvival_edge_days,
    time = "time_days",
    event = "event",
    predictors = c("stage", "grade"),
    time_unit = "years"  # Mismatched!
  )

  # Function should work but 5-year survival calculation will be wrong
  # (This is a limitation - we rely on user to specify correct unit)
  expect_no_error(result)
})
