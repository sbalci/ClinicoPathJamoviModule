
test_that('ordinalroc analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    predictor = runif(n, 1, 100),
    ordinal_outcome = sample(c('A', 'B'), n, replace = TRUE),
    outcome_order1 = runif(n, 1, 100),
    outcome_order2 = runif(n, 1, 100),
    outcome_order3 = runif(n, 1, 100),
    stratify_by = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    additional_predictors1 = runif(n, 1, 100),
    additional_predictors2 = runif(n, 1, 100),
    additional_predictors3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- ordinalroc(
      data = data,
    predictor = 'predictor',
    ordinal_outcome = 'ordinal_outcome',
    outcome_order = c('outcome_order1', 'outcome_order2', 'outcome_order3'),
    roc_method = 'empirical',
    cumulative_direction = 'geq',
    calculate_auc = TRUE,
    auc_ci_method = 'delong',
    bootstrap_samples = 100,
    confidence_level = 0.95,
    category_specific_auc = TRUE,
    test_proportional_odds = TRUE,
    show_coefficients = TRUE,
    partial_proportional_odds = FALSE,
    optimal_thresholds = TRUE,
    threshold_method = 'youden',
    sensitivity_target = 0.8,
    specificity_target = 0.8,
    plot_ordinal_roc = TRUE,
    plot_all_cumulative = TRUE,
    plot_category_distributions = TRUE,
    plot_cumulative_probabilities = FALSE,
    plot_confusion_matrix = FALSE,
    clinical_context = 'general',
    show_interpretation = TRUE,
    stratified_analysis = FALSE,
    stratify_by = 'stratify_by',
    covariate_adjustment = FALSE,
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    compare_predictors = FALSE,
    additional_predictors = c('additional_predictors1', 'additional_predictors2', 'additional_predictors3'),
    auc_comparison_test = TRUE,
    missing_handling = 'complete',
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'ordinalroc.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

