
test_that('polychoriccorr analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = sample(c('A', 'B'), n, replace = TRUE),
    vars2 = sample(c('A', 'B'), n, replace = TRUE),
    vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- polychoriccorr(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    corrType = 'polychoric',
    method = 'ml',
    ci = TRUE,
    ciWidth = 95,
    sig = TRUE,
    sigLevel = 0.05,
    matrixPlot = FALSE,
    showFreq = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'polychoriccorr.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

