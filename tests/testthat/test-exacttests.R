
test_that('exacttests analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    rows = sample(c('A', 'B'), n, replace = TRUE),
    cols = sample(c('A', 'B'), n, replace = TRUE),
    counts = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- exacttests(
      data = data,
    rows = 'rows',
    cols = 'cols',
    counts = 'counts',
    testType = 'fisher',
    alternative = 'two.sided',
    ciMethod = 'clopper_pearson',
    ciWidth = 95,
    showCI = TRUE,
    correction = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'exacttests.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

