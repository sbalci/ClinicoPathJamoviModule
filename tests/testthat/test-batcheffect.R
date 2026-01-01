
test_that('batcheffect analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    features1 = runif(n, 1, 100),
    features2 = runif(n, 1, 100),
    features3 = runif(n, 1, 100),
    batch_var = sample(c('A', 'B'), n, replace = TRUE),
    biological_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- batcheffect(
      data = data,
    features = c('features1', 'features2', 'features3'),
    batch_var = 'batch_var',
    biological_var = 'biological_var',
    perform_pca = TRUE,
    perform_combat = TRUE,
    feature_quality = TRUE,
    redundancy_analysis = TRUE,
    variance_threshold = 0.01,
    correlation_threshold = 0.95,
    outlier_method = 'robust',
    outlier_threshold = 3,
    pca_components = 3,
    missing_threshold = 20,
    show_plots = TRUE,
    combat_parametric = TRUE,
    save_corrected = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'batcheffect.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

