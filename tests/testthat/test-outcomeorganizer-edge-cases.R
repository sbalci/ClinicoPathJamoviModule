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
  result <- outcomeorganizer(
    data = outcomeorganizer_censored,
    outcome = "status",
    outcomeLevel = "Dead",
    recurrenceLevel = NULL,
    dod = NULL,
    dooc = NULL,
    awd = NULL,
    awod = NULL
  )
  expect_s3_class(result, "outcomeorganizerResults")
  expect_true(all(outcomeorganizer_censored$status == "Alive"))
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
  test_data <- outcomeorganizer_small[1, ]
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
