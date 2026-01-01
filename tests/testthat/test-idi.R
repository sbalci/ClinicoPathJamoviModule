
test_that('idi analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    baseline_risk = runif(n, 1, 100),
    new_risk = runif(n, 1, 100),
    time_var = runif(n, 1, 100),
    subgroup_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- idi(
      data = data,
    outcome = 'outcome',
    baseline_risk = 'baseline_risk',
    new_risk = 'new_risk',
    time_var = 'time_var',
    followup_time = 5,
    idi_type = 'standard',
    discrimination_measure = 'mean_diff',
    trim_proportion = 0.1,
    confidence_level = 0.95,
    bootstrap_samples = 100,
    bootstrap_method = 'bca',
    hypothesis_test = TRUE,
    test_method = 'bootstrap',
    decompose_idi = TRUE,
    risk_distribution = TRUE,
    discrimination_slope = TRUE,
    relative_improvement = TRUE,
    cross_validation = FALSE,
    cv_folds = 5,
    sensitivity_analysis = FALSE,
    outlier_detection = FALSE,
    outlier_method = 'iqr',
    show_summary = TRUE,
    show_distributions = TRUE,
    show_discrimination = TRUE,
    show_validation = FALSE,
    plot_risk_distributions = TRUE,
    plot_discrimination = TRUE,
    plot_scatter = TRUE,
    plot_bootstrap = FALSE,
    plot_sensitivity = FALSE,
    plot_outliers = FALSE,
    competing_risks = FALSE,
    stratified_analysis = FALSE,
    subgroup_var = 'subgroup_var',
    missing_handling = 'complete',
    calibration_aware = FALSE,
    time_dependent = FALSE,
    alpha_level = 0.05,
    random_seed = 123,
    relative_idi = FALSE,
    idi_competing_risks = FALSE,
    visual_discrimination_slopes = TRUE,
    idi_trajectory = FALSE,
    prediction_error_curves = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'idi.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

