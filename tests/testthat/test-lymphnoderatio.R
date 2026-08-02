
test_that('LNR computes ratio, adequacy, and survival strata', {
  skip_if_not_installed('jmvcore')

  set.seed(7); n <- 200
  examined <- pmax(1, rpois(n, 12)); risk <- runif(n)
  positive <- pmin(examined, rbinom(n, examined, risk^2))
  time <- round(rexp(n, 0.05 * (0.4 + positive/examined)), 1)
  dead <- factor(sample(c('Alive', 'Dead'), n, TRUE), levels = c('Alive', 'Dead'))
  data <- data.frame(positive, examined, time, dead)

  expect_no_error({
    model <- lymphnoderatio(
      data = data, positiveNodes = 'positive', examinedNodes = 'examined',
      minYield = 12, stratMethod = 'fixed', thresholds = '0.2, 0.5',
      survivalTime = 'time', survivalStatus = 'dead', eventLevel = 'Dead',
      showRatioSummary = TRUE, showStrata = TRUE, showPlot = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  rt <- model$results$ratioTable$asDF
  mean_lnr <- rt$value[rt$quantity == 'Mean LNR']
  expect_true(mean_lnr >= 0 && mean_lnr <= 1)

  st <- model$results$strataTable$asDF
  expect_true(nrow(st) >= 2)               # multiple strata
  expect_equal(st$hr[1], 1)                 # reference stratum HR = 1
})

test_that('LNR optimal-cutpoint stratification runs', {
  skip_if_not_installed('jmvcore')
  set.seed(3); n <- 200
  examined <- pmax(1, rpois(n, 15)); positive <- pmin(examined, rpois(n, 3))
  time <- round(rexp(n, 0.05 * (0.5 + positive/examined)), 1)
  dead <- factor(sample(c('Alive', 'Dead'), n, TRUE), levels = c('Alive', 'Dead'))
  data <- data.frame(positive, examined, time, dead)
  expect_no_error({
    model <- lymphnoderatio(
      data = data, positiveNodes = 'positive', examinedNodes = 'examined',
      stratMethod = 'optimal',
      survivalTime = 'time', survivalStatus = 'dead', eventLevel = 'Dead')
  })
  expect_true(inherits(model, 'jmvcoreClass'))
})

test_that('LNR rejects invalid node counts gracefully', {
  skip_if_not_installed('jmvcore')
  data <- data.frame(positive = c(3, 1), examined = c(0, 0))   # examined = 0
  expect_no_error({
    model <- lymphnoderatio(data = data, positiveNodes = 'positive',
                            examinedNodes = 'examined',
                            eventLevel = NULL)
  })
  expect_true(inherits(model, 'jmvcoreClass'))
})

