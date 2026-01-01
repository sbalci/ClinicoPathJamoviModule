
test_that('grouplasso analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    strata = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- grouplasso(
      data = data,
    time = 'time',
    event = 'event',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    strata = 'strata',
    group_definition = 'automatic',
    factor_grouping = TRUE,
    penalty_type = 'group_lasso',
    alpha = 0.5,
    group_weights = 'sqrt_size',
    cv_folds = 10,
    cv_measure = 'deviance',
    lambda_sequence = 'auto',
    n_lambda = 10,
    lambda_min_ratio = 1e-6,
    algorithm = 'coordinate',
    max_iterations = 100,
    tolerance = 1e-6,
    selection_threshold = 1e-8,
    stability_selection = FALSE,
    bootstrap_samples = 50,
    stability_threshold = 0.6,
    nested_cv = FALSE,
    inner_cv_folds = 5,
    permutation_test = FALSE,
    n_permutations = 100,
    show_group_summary = TRUE,
    show_coefficients = TRUE,
    show_path_summary = TRUE,
    show_cv_results = TRUE,
    plot_regularization_path = TRUE,
    plot_cv_curve = TRUE,
    plot_group_importance = TRUE,
    plot_stability = FALSE,
    plot_group_structure = FALSE,
    standardize = TRUE,
    center_groups = FALSE,
    adaptive_weights_method = 'ridge',
    warm_start = TRUE,
    parallel_computing = FALSE,
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'grouplasso.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

