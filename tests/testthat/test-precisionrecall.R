
test_that('precisionrecall analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    scores1 = runif(n, 1, 100),
    scores2 = runif(n, 1, 100),
    scores3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- precisionrecall(
      data = data,
    outcome = 'outcome',
    scores = c('scores1', 'scores2', 'scores3'),
    interpolation = 'nonlinear',
    showBaseline = TRUE,
    aucMethod = 'trapezoid',
    ci = FALSE,
    ciMethod = 'bootstrap',
    ciSamples = 1000,
    ciWidth = 95,
    comparison = FALSE,
    comparisonMethod = 'bootstrap',
    showROC = FALSE,
    showFScore = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'precisionrecall.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

