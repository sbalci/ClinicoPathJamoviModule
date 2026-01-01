
test_that('timeupdatesurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- timeupdatesurvival(
      data = data,
    time = 'time',
    status = 'status',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    id = 'id',
    method = 'aalen_additive',
    smoothing_method = 'lowess',
    bandwidth = 0.5,
    degrees_freedom = 4,
    confidence_level = 0.95,
    prediction_horizon = 5,
    bootstrap_samples = 50,
    include_residuals = TRUE,
    dynamic_prediction = TRUE,
    individual_profiles = FALSE,
    significance_testing = TRUE,
    model_comparison = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'timeupdatesurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

