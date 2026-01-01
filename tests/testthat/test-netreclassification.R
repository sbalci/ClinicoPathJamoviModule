
test_that('netreclassification analysis works', {
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
    model <- netreclassification(
      data = data,
    outcome = 'outcome',
    baseline_risk = 'baseline_risk',
    new_risk = 'new_risk',
    time_var = 'time_var',
    followup_time = 5,
    nri_type = 'categorical',
    custom_categories = FALSE,
    confidence_level = 0.95,
    bootstrap_samples = 100,
    bootstrap_method = 'bca',
    hypothesis_test = TRUE,
    decompose_nri = TRUE,
    direction_analysis = TRUE,
    show_transitions = TRUE,
    clinical_cutoffs = FALSE,
    treatment_threshold = 0.075,
    cost_effectiveness = FALSE,
    cross_validation = FALSE,
    cv_folds = 5,
    sensitivity_analysis = FALSE,
    show_summary = TRUE,
    show_reclassification = TRUE,
    show_performance = TRUE,
    plot_reclassification = TRUE,
    plot_risk_distribution = TRUE,
    plot_improvement = TRUE,
    plot_sensitivity = FALSE,
    competing_risks = FALSE,
    missing_handling = 'complete',
    stratified_analysis = FALSE,
    subgroup_var = 'subgroup_var',
    alpha_level = 0.05,
    random_seed = 123,
    category_free_nri = FALSE,
    event_specific_nri = FALSE,
    clinical_nri_custom = FALSE,
    reclassification_visualization = TRUE,
    relative_nri = FALSE,
    weighted_nri = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'netreclassification.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

