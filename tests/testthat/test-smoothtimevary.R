
test_that('smoothtimevary analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    time_varying_covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- smoothtimevary(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    time_varying_covariates = c('time_varying_covariates1', 'time_varying_covariates2', 'time_varying_covariates3'),
    smoothing_method = 'spline',
    spline_df = 4,
    bandwidth = 1,
    confidence_level = 0.95,
    test_constancy = TRUE,
    residual_analysis = TRUE,
    show_model_summary = TRUE,
    show_effects_table = TRUE,
    show_constancy_tests = TRUE,
    show_smooth_plots = TRUE,
    show_diagnostic_plots = TRUE,
    show_comparison_plots = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'smoothtimevary.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

