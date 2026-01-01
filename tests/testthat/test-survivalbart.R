
test_that('survivalbart analysis works', {
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
    model <- survivalbart(
      data = data,
    time = 'time',
    event = 'event',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    strata = 'strata',
    model_type = 'aft',
    prior_distribution = 'normal',
    n_trees = 200,
    alpha = 0.95,
    beta = 2,
    k = 2,
    q = 0.9,
    nu = 3,
    n_burn = 1000,
    n_post = 2000,
    n_thin = 1,
    n_chains = 1,
    variable_selection = TRUE,
    sparse_prior = FALSE,
    selection_alpha = 1,
    credible_level = 0.95,
    cross_validation = FALSE,
    cv_folds = 5,
    convergence_diagnostics = TRUE,
    posterior_prediction = FALSE,
    show_model_summary = TRUE,
    show_variable_importance = TRUE,
    show_survival_summary = TRUE,
    show_convergence = TRUE,
    plot_survival_curves = TRUE,
    plot_variable_importance = TRUE,
    plot_trace = FALSE,
    plot_posterior_predictive = FALSE,
    plot_partial_dependence = FALSE,
    plot_interactions = FALSE,
    probit_link = FALSE,
    cure_fraction_prior = 'uniform',
    adaptive_scaling = TRUE,
    parallel_chains = FALSE,
    memory_optimization = TRUE,
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'survivalbart.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

