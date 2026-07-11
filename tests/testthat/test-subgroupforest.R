
test_that('subgroupforest analysis works', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = runif(n, 1, 100),
    treatment = sample(c('A', 'B'), n, replace = TRUE),
    subgroups1 = sample(c('A', 'B'), n, replace = TRUE),
    subgroups2 = sample(c('A', 'B'), n, replace = TRUE),
    subgroups3 = sample(c('A', 'B'), n, replace = TRUE),
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- subgroupforest(
      data = data,
    outcome = 'outcome',
    treatment = 'treatment',
    subgroups = c('subgroups1', 'subgroups2', 'subgroups3'),
    time = 'time',
    event = 'event',
    outcomeType = 'survival',
    effectMeasure = 'hr',
    confidenceLevel = '0.95',
    showOverall = TRUE,
    showInteraction = TRUE,
    sortBy = 'effect',
    showSampleSizes = TRUE,
    logScale = TRUE,
    nullLine = 1
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'subgroupforest.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})


test_that('subgroupforest binary RR uses modified Poisson and reports heterogeneity', {
  skip_if_not_installed('jmvcore')

  set.seed(42); n <- 400
  trt <- sample(c('Ctrl', 'Tx'), n, replace = TRUE)
  sg  <- sample(c('Low', 'High'), n, replace = TRUE)
  # event risk depends on treatment and subgroup
  p <- plogis(-0.5 + 0.4 * (trt == 'Tx') + 0.5 * (sg == 'High'))
  data <- data.frame(
    outcome = rbinom(n, 1, p),
    treatment = trt, sg = sg)

  # Risk-ratio path (modified Poisson)
  expect_no_error({
    model <- subgroupforest(
      data = data, outcome = 'outcome', treatment = 'treatment',
      subgroups = 'sg', outcomeType = 'binary', effectMeasure = 'rr',
      confidenceLevel = '0.95', showOverall = TRUE, showInteraction = TRUE,
      sortBy = 'none', showSampleSizes = TRUE, logScale = TRUE, nullLine = 1)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  sm <- model$results$summary$asDF
  expect_true(nrow(sm) >= 2)                 # one row per subgroup level
  expect_true(all(sm$estimate > 0))          # RR positive
  # heterogeneity Html populated (two subgroups)
  het <- model$results$heterogeneity$content
  expect_true(grepl('Cochran', het) || grepl('heterogeneity', het, ignore.case = TRUE))
})

test_that('subgroupforest continuous outcome does not fail on negative mean differences', {
  skip_if_not_installed('jmvcore')

  set.seed(7); n <- 300
  trt <- sample(c('Ctrl', 'Tx'), n, replace = TRUE)
  sg  <- sample(c('G1', 'G2', 'G3'), n, replace = TRUE)
  # Tx lowers the outcome (negative mean difference) — old heterogeneity log() would NaN
  y <- 10 - 2 * (trt == 'Tx') + rnorm(n, 0, 3)
  data <- data.frame(outcome = y, treatment = trt, sg = sg)

  expect_no_error({
    model <- subgroupforest(
      data = data, outcome = 'outcome', treatment = 'treatment',
      subgroups = 'sg', outcomeType = 'continuous', effectMeasure = 'md',
      confidenceLevel = '0.95', showOverall = TRUE, showInteraction = FALSE,
      sortBy = 'effect', showSampleSizes = FALSE, logScale = FALSE, nullLine = 0)
  })
  sm <- model$results$summary$asDF
  expect_true(any(sm$estimate < 0))          # negative MDs present
  het <- model$results$heterogeneity$content
  # heterogeneity computed without NaN wipeout
  expect_false(grepl('NaN', het))
})
