
test_that('subgroupforest analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

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

