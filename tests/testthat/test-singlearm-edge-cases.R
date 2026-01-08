# ═══════════════════════════════════════════════════════════
# Edge Cases Tests: singlearm
# ═══════════════════════════════════════════════════════════
library(testthat)
library(ClinicoPath)
data(singlearm_small, package = "ClinicoPath")
data(singlearm_censored, package = "ClinicoPath")
data(singlearm_allevents, package = "ClinicoPath")
data(singlearm_early, package = "ClinicoPath")
data(singlearm_missing, package = "ClinicoPath")
data(singlearm_zerotime, package = "ClinicoPath")
data(singlearm_large, package = "ClinicoPath")
data(singlearm_shortfu, package = "ClinicoPath")
data(singlearm_longfu, package = "ClinicoPath")

test_that("singlearm handles small datasets", {
  result <- singlearm(
    data = singlearm_small,
    elapsedtime = "time_months",
    outcome = "outcome"
  )
  expect_s3_class(result, "singlearmClass")
  expect_true(nrow(singlearm_small) == 15)
})

test_that("singlearm handles all censored data", {
  result <- singlearm(
    data = singlearm_censored,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
  expect_s3_class(result, "singlearmClass")
  # All observations should be censored
  expect_true(all(singlearm_censored$outcome == "Alive"))
})

test_that("singlearm handles all events data", {
  result <- singlearm(
    data = singlearm_allevents,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
  expect_s3_class(result, "singlearmClass")
  # All observations should have events
  expect_true(all(singlearm_allevents$outcome == "Dead"))
})

test_that("singlearm handles very early events", {
  result <- singlearm(
    data = singlearm_early,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
  expect_s3_class(result, "singlearmClass")
  # Check that data has very short follow-up times
  expect_true(mean(singlearm_early$time_months) < 5)
})

test_that("singlearm handles missing outcome values", {
  result <- singlearm(
    data = singlearm_missing,
    elapsedtime = "time_months",
    outcome = "outcome"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles missing time values", {
  result <- singlearm(
    data = singlearm_missing,
    elapsedtime = "time_months",
    outcome = "outcome"
  )
  expect_s3_class(result, "singlearmClass")
  # Should have some NA values
  expect_true(any(is.na(singlearm_missing$time_months)))
})

test_that("singlearm handles zero time values", {
  result <- singlearm(
    data = singlearm_zerotime,
    elapsedtime = "time_months",
    outcome = "outcome"
  )
  expect_s3_class(result, "singlearmClass")
  # Check that data has some zero times
  expect_true(any(singlearm_zerotime$time_months == 0))
})

test_that("singlearm handles large datasets efficiently", {
  result <- singlearm(
    data = singlearm_large,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
  expect_s3_class(result, "singlearmClass")
  expect_true(nrow(singlearm_large) == 500)
})

test_that("singlearm handles very short follow-up", {
  result <- singlearm(
    data = singlearm_shortfu,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles very long follow-up", {
  result <- singlearm(
    data = singlearm_longfu,
    elapsedtime = "time_months",
    outcome = "outcome",
    outcomeLevel = "Dead"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles single observation", {
  test_data <- singlearm_small[1, ]
  result <- singlearm(
    data = test_data,
    elapsedtime = "time_months",
    outcome = "outcome"
  )
  expect_s3_class(result, "singlearmClass")
})

test_that("singlearm handles extreme cutpoints", {
  result <- singlearm(
    data = singlearm_test,
    elapsedtime = "time_months",
    outcome = "outcome",
    cutp = "1, 120, 240"
  )
  expect_s3_class(result, "singlearmClass")
})
