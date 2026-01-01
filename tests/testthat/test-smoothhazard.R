
test_that('smoothhazard analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    status_var = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    strata_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- smoothhazard(
      data = data,
    time_var = 'time_var',
    status_var = 'status_var',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    strata_var = 'strata_var',
    method = 'kernel',
    bandwidth = 0,
    bandwidth_method = 'automatic',
    confidence_level = 0.95,
    time_grid = 50,
    boundary_correction = TRUE,
    kernel_type = 'epanechnikov',
    hazard_plot = TRUE,
    cumulative_hazard_plot = FALSE,
    confidence_bands = TRUE,
    comparison_plot = FALSE,
    diagnostic_plots = FALSE,
    hazard_summary = TRUE,
    peak_analysis = FALSE,
    model_comparison = FALSE,
    export_hazard = FALSE,
    bootstrap_ci = FALSE,
    bootstrap_samples = 100
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'smoothhazard.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

