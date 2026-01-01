
test_that('timedependent analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    id = sample(c('A', 'B'), n, replace = TRUE),
    start_time = runif(n, 1, 100),
    stop_time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    time_dependent_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    time_dependent_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    time_dependent_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    baseline_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    baseline_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    baseline_vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- timedependent(
      data = data,
    id = 'id',
    start_time = 'start_time',
    stop_time = 'stop_time',
    event = 'event',
    time_dependent_vars = c('time_dependent_vars1', 'time_dependent_vars2', 'time_dependent_vars3'),
    baseline_vars = c('baseline_vars1', 'baseline_vars2', 'baseline_vars3'),
    perform_landmark = TRUE,
    prediction_window = 12,
    time_dependent_roc = TRUE,
    roc_method = 'incident_dynamic',
    optimal_cutpoint = TRUE,
    cutpoint_method = 'youden',
    model_type = 'extended_cox',
    time_transform = 'none',
    test_proportional_hazards = TRUE,
    schoenfeld_transform = 'km',
    internal_validation = TRUE,
    cv_folds = 5,
    bootstrap_validation = FALSE,
    n_bootstrap = 100,
    compare_models = FALSE,
    comparison_metric = 'iauc',
    plot_time_varying_effects = TRUE,
    plot_roc_curves = TRUE,
    plot_auc_trajectory = TRUE,
    plot_cutpoint_stability = TRUE,
    plot_landmark_predictions = TRUE,
    plot_schoenfeld_residuals = TRUE,
    confidence_level = 0.95,
    decimals = 3,
    export_predictions = FALSE,
    export_roc_data = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'timedependent.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

