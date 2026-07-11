
test_that('joinpoint detects a trend change and reports APC', {
  skip_if_not_installed('jmvcore')
  skip_if_not_installed('segmented')

  set.seed(2026); yr <- 2000:2020
  rate <- exp(ifelse(yr <= 2010,
                     log(45) - 0.03 * (yr - 2000),
                     log(45) - 0.3 + 0.04 * (yr - 2010)) + rnorm(21, 0, 0.03))
  data <- data.frame(year = yr, incidence_rate = round(rate, 2))

  expect_no_error({
    model <- joinpoint(
      data = data, time = 'year', rate = 'incidence_rate',
      maxJoinpoints = 3, conf_level = 0.95,
      showSegments = TRUE, showAAPC = TRUE, showPlot = TRUE,
      showSummary = TRUE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  seg <- model$results$segmentTable$asDF
  # at least two segments (one joinpoint detected), and APC changes sign
  expect_true(nrow(seg) >= 2)
  expect_true(seg$apc[1] < 0 && seg$apc[nrow(seg)] > 0)
})

test_that('joinpoint handles a series with no change point', {
  skip_if_not_installed('jmvcore')
  skip_if_not_installed('segmented')

  set.seed(5); yr <- 2000:2020
  rate <- exp(log(50) - 0.02 * (yr - 2000) + rnorm(21, 0, 0.03))
  data <- data.frame(year = yr, incidence_rate = round(rate, 2))
  expect_no_error({
    model <- joinpoint(data = data, time = 'year', rate = 'incidence_rate',
                       maxJoinpoints = 3)
  })
  seg <- model$results$segmentTable$asDF
  # single segment (no joinpoint) with a negative APC
  expect_true(nrow(seg) == 1)
  expect_true(seg$apc[1] < 0)
})
