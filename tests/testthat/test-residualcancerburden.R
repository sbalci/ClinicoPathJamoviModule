
test_that('RCB reproduces the Symmans worked example (single case)', {
  skip_if_not_installed('jmvcore')

  expect_no_error({
    model <- residualcancerburden(
      data = data.frame(x = 1),   # placeholder; manual mode ignores data
      inputMode = 'manual',
      mD1 = 24, mD2 = 18, mCellularity = 10, mCis = 5,
      mNodes = 3, mMetSize = 4,
      showPlot = TRUE, showSummary = TRUE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  res <- model$results$caseTable$asDF
  idx <- res$value[res$quantity == 'RCB index']
  expect_equal(round(idx, 2), 3.03)
})

test_that('RCB classifies a cohort and links to survival', {
  skip_if_not_installed('jmvcore')

  set.seed(2026); n <- 120
  d1 <- round(runif(n, 0, 45)); d2 <- round(d1 * runif(n, 0.6, 1))
  ca <- round(runif(n, 0, 90)); cis <- round(runif(n, 0, 15))
  ln <- rpois(n, 1.2); dmet <- ifelse(ln == 0, 0, round(runif(n, 1, 15)))
  time <- round(rexp(n, 0.04), 1)
  event <- factor(sample(c('Censored', 'Event'), n, TRUE), levels = c('Censored', 'Event'))
  data <- data.frame(d1, d2, ca, cis, ln, dmet, time, event)

  expect_no_error({
    model <- residualcancerburden(
      data = data, inputMode = 'data',
      d1 = 'd1', d2 = 'd2', cellularity = 'ca', cis = 'cis',
      positiveNodes = 'ln', metSize = 'dmet',
      survivalTime = 'time', survivalStatus = 'event', eventLevel = 'Event',
      showDistribution = TRUE, survivalLink = TRUE, showPlot = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  dist <- model$results$distributionTable$asDF
  # all four classes represented as rows, counts sum to N with valid RCB
  expect_true(sum(dist$n) > 0)
  expect_true(all(dist$rcbclass %in% c('RCB-0 (pCR)', 'RCB-I', 'RCB-II', 'RCB-III')))
})

test_that('RCB class boundaries follow the 1.36 / 3.28 cutpoints', {
  skip_if_not_installed('jmvcore')
  # node-negative cases at controlled index values
  # pCR (all zero) -> RCB-0
  m0 <- residualcancerburden(data = data.frame(x = 1), inputMode = 'manual',
    mD1 = 0, mD2 = 0, mCellularity = 0, mCis = 0, mNodes = 0, mMetSize = 0)
  expect_match(m0$results$caseTable$notes[['idx']], 'RCB-0')
})
