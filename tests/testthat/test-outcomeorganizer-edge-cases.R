# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: outcomeorganizer
# ═══════════════════════════════════════════════════════════
library(testthat)
data(outcomeorganizer_small, package = "ClinicoPath")
data(outcomeorganizer_censored, package = "ClinicoPath")
data(outcomeorganizer_allevents, package = "ClinicoPath")

test_that("outcomeorganizer handles small datasets", {
  result <- outcomeorganizer(
    data = outcomeorganizer_small,
    outcome = "outcome",
    outcomeLevel = "Event",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )
  expect_s3_class(result, "outcomeorganizerResults")
  expect_true(nrow(outcomeorganizer_small) == 15)
})

test_that("outcomeorganizer handles all censored data", {
  expect_true(all(outcomeorganizer_censored$status == "Alive"))
  # A text column holding the single value "Alive" has no "Dead" level to code as the
  # event: the validation error is a jmvcore::reject() now, not a quiet return.
  expect_error(
    outcomeorganizer(
      data = outcomeorganizer_censored,
      outcome = "status",
      outcomeLevel = "Dead",
      recurrenceLevel = NULL,
      dod = NULL,
      dooc = NULL,
      awd = NULL,
      awod = NULL
    ),
    "Outcome variable must have at least 2 different values"
  )
  # With the event level declared, a cohort without events is recoded: every row censored.
  declared <- outcomeorganizer_censored
  declared$status <- factor(declared$status, levels = c("Alive", "Dead"))
  result <- outcomeorganizer(
    data = declared,
    outcome = "status",
    outcomeLevel = "Dead",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    outputTable = TRUE
  )
  expect_s3_class(result, "outcomeorganizerResults")
  counts <- result$outputTable$asDF
  expect_identical(as.integer(counts$outcome), 0L)
  expect_identical(as.integer(counts$count), nrow(outcomeorganizer_censored))
})

test_that("outcomeorganizer handles all events data", {
  result <- outcomeorganizer(
    data = outcomeorganizer_allevents,
    outcome = "status",
    outcomeLevel = "Dead",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )
  expect_s3_class(result, "outcomeorganizerResults")
  expect_true(all(outcomeorganizer_allevents$status == "Dead"))
})

test_that("outcomeorganizer handles missing outcome values", {
  test_data <- outcomeorganizer_small
  test_data$outcome[1:3] <- NA
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    outcomeLevel = "Event",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles missing time values", {
  test_data <- outcomeorganizer_small
  test_data$time[1:2] <- NA
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    outcomeLevel = "Event",
    followupTime = "time",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles zero time values", {
  test_data <- outcomeorganizer_small
  test_data$time[1] <- 0
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    outcomeLevel = "Event",
    followupTime = "time",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles negative time values", {
  test_data <- outcomeorganizer_small
  test_data$time[1] <- -5
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    outcomeLevel = "Event",
    followupTime = "time",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )
  expect_s3_class(result, "outcomeorganizerResults")
})

test_that("outcomeorganizer handles single observation", {
  # Row 1 is "NoEvent" in a text column: one value and no event level among the data, so
  # the validation error is a jmvcore::reject() now, not a quiet return.
  expect_identical(outcomeorganizer_small$outcome[1:2], c("NoEvent", "Event"))
  expect_error(
    outcomeorganizer(
      data = outcomeorganizer_small[1, ],
      outcome = "outcome",
      outcomeLevel = "Event",
      recurrenceLevel = NULL,
      dod = NULL,
      dooc = NULL,
      awd = NULL,
      awod = NULL
    ),
    "Outcome variable must have at least 2 different values"
  )
  # A single observation that carries the selected event level is recoded.
  result <- outcomeorganizer(
    data = outcomeorganizer_small[2, ],
    outcome = "outcome",
    outcomeLevel = "Event",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL,
    outputTable = TRUE
  )
  expect_s3_class(result, "outcomeorganizerResults")
  counts <- result$outputTable$asDF
  expect_identical(as.integer(counts$outcome), 1L)
  expect_identical(as.integer(counts$count), 1L)
})
