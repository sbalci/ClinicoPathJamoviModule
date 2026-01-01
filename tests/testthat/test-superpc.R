
test_that('superpc analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    features1 = runif(n, 1, 100),
    features2 = runif(n, 1, 100),
    features3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- superpc(
      data = data,
    time = 'time',
    event = 'event',
    features = c('features1', 'features2', 'features3'),
    threshold = 0.1,
    n_components = 5,
    cv_folds = 10,
    standardize = TRUE,
    screening_method = 'univariate_cox',
    pca_method = 'standard',
    validation_method = 'cv',
    plot_screening = TRUE,
    plot_pca = TRUE,
    plot_survival = TRUE,
    export_components = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'superpc.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

