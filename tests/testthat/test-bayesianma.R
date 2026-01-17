
test_that('bayesianma analysis works', {
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
    pred_vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- bayesianma(
      data = data,
    time_var = 'time_var',
    event_var = 'event_var',
    pred_vars = c('pred_vars1', 'pred_vars2', 'pred_vars3'),
    prior_type = 'uniform',
    prior_inclusion_prob = 0.5,
    beta_alpha = 1,
    beta_beta = 1,
    complexity_penalty = 1,
    mcmc_method = 'mc3',
    mcmc_chains = 3,
    mcmc_iterations = 5000,
    burn_in = 1000,
    thinning = 1,
    convergence_diagnostic = 'gelman_rubin',
    model_selection_method = 'highest_posterior',
    occam_ratio = 20,
    proposal_variance = 1,
    variable_selection_threshold = 0.5,
    uncertainty_quantification = TRUE,
    prediction_intervals = TRUE,
    credible_level = 0.95,
    model_diagnostics = TRUE,
    sensitivity_analysis = FALSE,
    cross_validation = FALSE,
    cv_folds = 5,
    parallel_processing = FALSE,
    seed_value = 42,
    show_summary = TRUE,
    show_model_space = TRUE,
    show_coefficients = TRUE,
    show_inclusion_probs = TRUE,
    show_top_models = TRUE,
    show_diagnostics = TRUE,
    show_predictions = FALSE,
    plot_model_probs = TRUE,
    plot_inclusion_probs = TRUE,
    plot_coefficients = TRUE,
    plot_convergence = TRUE,
    plot_model_space = FALSE,
    plot_predictions = FALSE,
    showExplanations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(inherits(model, "R6"))
  expect_true(inherits(model, 'bayesianmaResults'))

  # Define output path
  omv_path <- file.path('omv_output', 'bayesianma.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  # expect_no_error({
  #   jmvReadWrite::write_omv(model, omv_path)
  # })

  # expect_true(file.exists(omv_path))
})

