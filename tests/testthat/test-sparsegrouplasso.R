
test_that('sparsegrouplasso analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    event_var = sample(c('A', 'B'), n, replace = TRUE),
    pred_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    pred_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    pred_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    pathway_info = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- sparsegrouplasso(
      data = data,
    time_var = 'time_var',
    event_var = 'event_var',
    pred_vars = c('pred_vars1', 'pred_vars2', 'pred_vars3'),
    group_definition = 'factor_based',
    pathway_info = 'pathway_info',
    correlation_threshold = 0.7,
    alpha_sgl = 0.95,
    lambda_sequence = 'auto',
    lambda_min_ratio = 0.001,
    n_lambda = 10,
    selection_criterion = 'cv_deviance',
    cv_folds = 10,
    cv_repeats = 1,
    ebic_gamma = 1,
    weight_type = 'none',
    weight_power = 1,
    standardize_vars = TRUE,
    center_vars = TRUE,
    orthogonalize_groups = FALSE,
    max_iterations = 100,
    convergence_threshold = 1e-8,
    warm_start = TRUE,
    parallel_cv = FALSE,
    seed_value = 42,
    show_summary = TRUE,
    show_coefficients = TRUE,
    show_groups = TRUE,
    show_path = FALSE,
    show_performance = TRUE,
    show_validation = TRUE,
    plot_cv_error = TRUE,
    plot_coefficients = TRUE,
    plot_groups = TRUE,
    plot_sparsity = FALSE,
    plot_stability = FALSE,
    alpha_level = 0.05,
    confidence_intervals = FALSE,
    bootstrap_samples = 100,
    stability_selection = FALSE,
    stability_threshold = 0.8,
    stability_subsample = 0.8,
    showExplanations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'sparsegrouplasso.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

