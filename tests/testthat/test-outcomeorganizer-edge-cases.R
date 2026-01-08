# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: outcomeorganizer
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(outcomeorganizer_small, package = "ClinicoPath")
data(outcomeorganizer_censored, package = "ClinicoPath")
data(outcomeorganizer_allevents, package = "ClinicoPath")

test_that("outcomeorganizer handles small datasets", {
  result <- outcomeorganizer(
    data = outcomeorganizer_small,
    outcome = "outcome",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
  expect_true(nrow(outcomeorganizer_small) == 15)
})

test_that("outcomeorganizer handles all censored data", {
  result <- outcomeorganizer(
    data = outcomeorganizer_censored,
    outcome = "status",
    time = "time",
    alive = "Alive"
  )
  expect_s3_class(result, "outcomeorganizerClass")
  # All observations should be censored
  expect_true(all(outcomeorganizer_censored$status == "Alive"))
})

test_that("outcomeorganizer handles all events data", {
  result <- outcomeorganizer(
    data = outcomeorganizer_allevents,
    outcome = "status",
    time = "time",
    dead = "Dead"
  )
  expect_s3_class(result, "outcomeorganizerClass")
  # All observations should be events
  expect_true(all(outcomeorganizer_allevents$status == "Dead"))
})

test_that("outcomeorganizer handles missing outcome values", {
  test_data <- outcomeorganizer_small
  test_data$outcome[1:3] <- NA
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles missing time values", {
  test_data <- outcomeorganizer_small
  test_data$time[1:2] <- NA
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles zero time values", {
  test_data <- outcomeorganizer_small
  test_data$time[1] <- 0
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles negative time values", {
  test_data <- outcomeorganizer_small
  test_data$time[1] <- -5
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    time = "time"
  )
  # Should handle or warn about negative times
  expect_s3_class(result, "outcomeorganizerClass")
})

test_that("outcomeorganizer handles single observation", {
  test_data <- outcomeorganizer_small[1, ]
  result <- outcomeorganizer(
    data = test_data,
    outcome = "outcome",
    time = "time"
  )
  expect_s3_class(result, "outcomeorganizerClass")
})
