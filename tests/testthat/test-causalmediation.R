
test_that('causalmediation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    treatment = sample(c('A', 'B'), n, replace = TRUE),
    mediator = runif(n, 1, 100),
    mediators1 = runif(n, 1, 100),
    mediators2 = runif(n, 1, 100),
    mediators3 = runif(n, 1, 100),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    multiple_mediators1 = runif(n, 1, 100),
    multiple_mediators2 = runif(n, 1, 100),
    multiple_mediators3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- causalmediation(
      data = data,
    outcome = 'outcome',
    treatment = 'treatment',
    mediator = 'mediator',
    mediators = c('mediators1', 'mediators2', 'mediators3'),
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    mediation_tier = 'basic',
    boot_samples = 1000,
    conf_level = 0.95,
    mediator_model = 'lm',
    outcome_model = 'lm',
    multiple_mediators = c('multiple_mediators1', 'multiple_mediators2', 'multiple_mediators3'),
    show_dag = TRUE,
    cma_estimation = 'rb',
    hd_method = 'hdmax2',
    hd_fdr_threshold = 0.1,
    hd_top_mediators = 20,
    hd_penalty = 'lasso',
    hd_parallel = TRUE,
    show_decomposition = TRUE,
    show_prop_mediated = TRUE,
    sensitivity_analysis = FALSE,
    plot_effects = TRUE,
    plot_hd_manhattan = TRUE,
    plot_hd_volcano = TRUE,
    interaction_term = FALSE,
    random_seed = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'causalmediation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

