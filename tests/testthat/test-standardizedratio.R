
test_that('standardizedratio computes SIR from rate x person-time', {
  skip_if_not_installed('jmvcore')

  data <- data.frame(
    age_group = c('40-49', '50-59', '60-69'),
    observed = c(20, 15, 10),
    person_years = c(10000, 8000, 5000),
    reference_rate = c(0.001, 0.001, 0.001))

  expect_no_error({
    model <- standardizedratio(
      data = data, inputMode = 'rate',
      observed = 'observed', personTime = 'person_years',
      refRate = 'reference_rate', stratum = 'age_group',
      ratioType = 'sir', conf_level = 0.95,
      perStratum = TRUE, showPlot = TRUE,
      showSummary = TRUE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  res <- model$results$overallTable$asDF
  # O = 45, E = 23 -> SIR ~ 1.96
  expect_equal(res$observed[1], 45)
  expect_equal(round(res$ratio[1], 2), round(45 / 23, 2))
})

test_that('standardizedratio accepts expected-events column', {
  skip_if_not_installed('jmvcore')
  data <- data.frame(obs = c(30, 20), exp = c(25, 25))
  model <- standardizedratio(
    data = data, inputMode = 'expected',
    observed = 'obs', expected = 'exp', ratioType = 'smr')
  res <- model$results$overallTable$asDF
  expect_equal(round(res$ratio[1], 2), 1.00)  # 50 / 50
})
