
test_that('greyzoneroc analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    predictor = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    stratify_by = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- greyzoneroc(
      data = data,
    predictor = 'predictor',
    outcome = 'outcome',
    grey_zone_method = 'fixed_width',
    grey_zone_width = 0.1,
    lower_grey_boundary = 0.45,
    upper_grey_boundary = 0.55,
    confidence_threshold = 0.8,
    cost_false_positive = 1,
    cost_false_negative = 1,
    cost_grey_zone = 0.3,
    calculate_definite_performance = TRUE,
    calculate_all_cases_performance = TRUE,
    grey_zone_characteristics = TRUE,
    optimal_threshold = 'youden',
    clinical_threshold = 0.5,
    prediction_intervals = FALSE,
    bootstrap_grey_zone = TRUE,
    bootstrap_samples = 100,
    confidence_level = 0.95,
    grey_zone_action = 'reflex',
    plot_grey_zone_roc = TRUE,
    plot_threshold_distributions = TRUE,
    plot_grey_zone_size = TRUE,
    plot_uncertainty_map = FALSE,
    plot_cost_surface = FALSE,
    fuzzy_membership = FALSE,
    bayesian_grey_zone = FALSE,
    stratified_grey_zone = FALSE,
    stratify_by = 'stratify_by',
    clinical_scenario = 'general',
    missing_handling = 'complete',
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'greyzoneroc.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

