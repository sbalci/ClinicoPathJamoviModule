
test_that('clinicalvalidation analysis works', {
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
    model <- clinicalvalidation(
      data = data,
    outcome = 'outcome',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    time_variable = 'time_variable',
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
    performance_metrics = 'all',
    confidence_level = 0.95,
    calibration_method = 'all',
    calibration_bins = 10,
    compare_models = FALSE,
    comparison_models = 'logistic_rf',
    show_model_summary = FALSE,
    show_performance_table = FALSE,
    show_calibration_plot = FALSE,
    show_roc_curve = FALSE,
    show_prc_curve = FALSE,
    show_validation_curves = FALSE,
    show_residual_plots = FALSE,
    show_clinical_interpretation = FALSE,
    export_results = FALSE,
    detailed_bootstrap = FALSE,
    missing_data_handling = 'complete_cases',
    imputation_methods = 5,
    parallel_processing = FALSE,
    n_cores = 1,
    set_seed = TRUE,
    seed_value = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'clinicalvalidation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

