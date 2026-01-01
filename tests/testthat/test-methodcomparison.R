
test_that('methodcomparison analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    method1 = runif(n, 1, 100),
    method2 = runif(n, 1, 100),
    grouping = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- methodcomparison(
      data = data,
    method1 = 'method1',
    method2 = 'method2',
    grouping = 'grouping',
    comparison_method = 'bland_altman',
    confidence_level = 0.95,
    bland_altman_options = TRUE,
    limits_method = 'standard',
    proportional_bias = TRUE,
    outlier_detection = TRUE,
    passing_bablok_options = FALSE,
    pb_alpha = 0.05,
    deming_options = FALSE,
    error_ratio = 1,
    correlation_analysis = TRUE,
    concordance_correlation = TRUE,
    regression_comparison = TRUE,
    clinical_limits = FALSE,
    clinical_lower = -10,
    clinical_upper = 10,
    transformation = 'none',
    plots_options = TRUE,
    bland_altman_plot = TRUE,
    scatter_plot = TRUE,
    residual_plots = FALSE,
    mountain_plot = FALSE,
    missing_treatment = 'complete',
    bootstrap_samples = 100
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'methodcomparison.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

