
test_that('fragilityindex summary input works', {
  skip_if_not_installed('jmvcore')

  expect_no_error({
    model <- fragilityindex(
      dataFormat = 'summary',
      events1 = 10, n1 = 100,
      events2 = 25, n2 = 100,
      testType = 'fisher', alpha = 0.05,
      showCounts = TRUE, showTrajectory = TRUE,
      showPlot = TRUE, showSummary = TRUE, showExplanation = TRUE,
      outcomeEvent = NULL
    )
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  res <- model$results$mainTable$asDF
  fi <- res$value[res$statistic == 'Fragility Index']
  # known result for this 2x2 (Walsh-style example): FI = 4
  expect_equal(fi, 4)
  fq <- res$value[res$statistic == 'Fragility Quotient (FI / N)']
  expect_equal(round(fq, 3), 0.020)
})

test_that('fragilityindex reverse index for non-significant result', {
  skip_if_not_installed('jmvcore')

  model <- fragilityindex(
    dataFormat = 'summary',
    events1 = 18, n1 = 100, events2 = 25, n2 = 100,
    testType = 'fisher', alpha = 0.05,
    outcomeEvent = NULL)
  res <- model$results$mainTable$asDF
  # baseline non-significant -> reverse fragility index is reported and > 0
  fi <- res$value[grepl('Fragility Index', res$statistic)]
  expect_true(fi > 0)
})

test_that('fragilityindex raw data input works', {
  skip_if_not_installed('jmvcore')

  set.seed(1)
  data <- rbind(
    data.frame(arm = 'Treatment',
               outcome = factor(c(rep('Event', 10), rep('No event', 90)),
                                levels = c('No event', 'Event'))),
    data.frame(arm = 'Control',
               outcome = factor(c(rep('Event', 25), rep('No event', 75)),
                                levels = c('No event', 'Event'))))
  data$arm <- factor(data$arm, levels = c('Control', 'Treatment'))

  expect_no_error({
    model <- fragilityindex(
      data = data, dataFormat = 'raw',
      group = 'arm', outcome = 'outcome', outcomeEvent = 'Event',
      testType = 'fisher', alpha = 0.05)
  })
  res <- model$results$mainTable$asDF
  expect_equal(res$value[res$statistic == 'Fragility Index'], 4)
})
