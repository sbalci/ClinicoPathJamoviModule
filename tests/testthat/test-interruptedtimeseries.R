
test_that('interruptedtimeseries segmented regression works', {
  skip_if_not_installed('jmvcore')

  set.seed(2026)
  n <- 48; itime <- 25
  time <- 1:n
  post <- as.integer(time >= itime)
  time_after <- pmax(0, time - itime + 1) * post
  y <- 72 - 0.15 * time + (-8) * post + (-0.5) * time_after + rnorm(n, 0, 1.5)
  data <- data.frame(month = time, turnaround_hours = y)

  expect_no_error({
    model <- interruptedtimeseries(
      data = data,
      time = 'month', outcome = 'turnaround_hours',
      interventionTime = 25, hac = TRUE, lag = 0,
      counterfactual = TRUE, predictAt = 40,
      showDiagnostics = TRUE, showPlot = TRUE,
      showSummary = TRUE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  coefs <- model$results$coefTable$asDF
  # level change (post) should be estimated as clearly negative
  lvl <- coefs$estimate[coefs$term == 'Level change at intervention']
  expect_true(lvl < 0)
  # trend change should be present
  expect_true('Trend change after intervention' %in% coefs$term)
})

test_that('interruptedtimeseries validates intervention time within range', {
  skip_if_not_installed('jmvcore')

  data <- data.frame(month = 1:20, y = rnorm(20))
  expect_no_error({
    model <- interruptedtimeseries(
      data = data, time = 'month', outcome = 'y',
      interventionTime = 999)  # out of range -> handled gracefully
  })
  # no coefficient rows populated when intervention time is invalid
  expect_equal(nrow(model$results$coefTable$asDF), 0)
})
