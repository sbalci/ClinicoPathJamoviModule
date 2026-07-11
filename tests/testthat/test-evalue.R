
test_that('evalue reproduces the VanderWeele-Ding RR example', {
  skip_if_not_installed('jmvcore')

  expect_no_error({
    model <- evalue(
      effectType = 'RR', estimate = 3.9,
      ci_lower = 1.8, ci_upper = 8.7, rare = FALSE,
      showPlot = TRUE, showSummary = TRUE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  res <- model$results$mainTable$asDF
  ev_point <- res$evalue[res$quantity == 'Point estimate']
  expect_equal(round(ev_point, 2), 7.26)
  # CI-limit E-value (lower = 1.8) ~ 3.00
  ev_ci <- res$evalue[grepl('CI', res$quantity)]
  expect_equal(round(ev_ci, 2), 3.00)
})

test_that('evalue returns 1 when the CI crosses the null', {
  skip_if_not_installed('jmvcore')
  model <- evalue(effectType = 'RR', estimate = 1.2, ci_lower = 0.9, ci_upper = 1.6)
  res <- model$results$mainTable$asDF
  ev_ci <- res$evalue[grepl('CI', res$quantity)]
  expect_equal(ev_ci, 1)
})

test_that('evalue converts a common-outcome odds ratio', {
  skip_if_not_installed('jmvcore')
  model <- evalue(effectType = 'OR', estimate = 3.9, rare = FALSE)
  res <- model$results$mainTable$asDF
  # RR scale = sqrt(3.9) = 1.975
  expect_equal(round(res$rr_scale[1], 3), 1.975)
})
