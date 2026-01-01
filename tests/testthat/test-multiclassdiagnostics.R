
test_that('multiclassdiagnostics analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    predicted = runif(n, 1, 100),
    actual = runif(n, 1, 100),
    predicted2 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- multiclassdiagnostics(
      data = data,
    predicted = 'predicted',
    actual = 'actual',
    confidenceLevel = 0.95,
    showROC = TRUE,
    showConfusion = TRUE,
    showPerClass = TRUE,
    showOverall = TRUE,
    compareModels = FALSE,
    predicted2 = 'predicted2',
    deLongTest = TRUE,
    mcnemarTest = TRUE,
    plotTheme = 'default',
    saveResults = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'multiclassdiagnostics.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

