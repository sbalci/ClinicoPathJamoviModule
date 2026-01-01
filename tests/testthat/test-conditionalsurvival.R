
test_that('conditionalsurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    timeVar = runif(n, 1, 100),
    outcomeVar = sample(c('A', 'B'), n, replace = TRUE),
    conditionVar = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- conditionalsurvival(
      data = data,
    timeVar = 'timeVar',
    outcomeVar = 'outcomeVar',
    conditionVar = 'conditionVar',
    conditionTime = 1,
    method = 'km',
    bandwidth = 1,
    confInt = 0.95,
    plotType = 'curves',
    showTable = TRUE,
    showPlot = TRUE,
    showExplanations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'conditionalsurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

