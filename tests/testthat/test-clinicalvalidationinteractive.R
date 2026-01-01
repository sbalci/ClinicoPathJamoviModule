
test_that('clinicalvalidationinteractive analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    time_variable = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- clinicalvalidationinteractive(
      data = data,
    outcome = 'outcome',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    time_variable = 'time_variable',
    clinical_preset = 'custom',
    model_type = 'logistic',
    validation_method = 'bootstrap',
    bootstrap_samples = 100,
    cv_folds = 10,
    cv_repeats = 3,
    holdout_proportion = 0.25,
    stratified_sampling = TRUE,
    clinical_context = 'diagnosis',
    prevalence_adjustment = FALSE,
    population_prevalence = 10,
    cost_matrix = 'equal',
    fn_fp_cost_ratio = 2,
    min_sensitivity = 0.8,
    min_specificity = 0.8,
    min_ppv = 0.7,
    min_npv = 0.9,
    auto_optimize_threshold = FALSE,
    optimization_metric = 'youden',
    performance_metrics = 'all',
    confidence_level = 0.95,
    show_realtime_metrics = TRUE,
    show_parameter_warnings = TRUE,
    show_clinical_guidance = TRUE,
    show_model_summary = TRUE,
    show_performance_table = TRUE,
    show_calibration_plot = TRUE,
    show_roc_curve = TRUE,
    show_threshold_optimization = FALSE,
    show_clinical_interpretation = TRUE,
    missing_data_handling = 'complete_cases',
    set_seed = TRUE,
    seed_value = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicalvalidationinteractive.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

