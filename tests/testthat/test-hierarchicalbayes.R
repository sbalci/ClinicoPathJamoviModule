
test_that('hierarchicalbayes analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    tp = runif(n, 1, 100),
    fp = runif(n, 1, 100),
    fn = runif(n, 1, 100),
    tn = runif(n, 1, 100),
    study_id = sample(c('A', 'B'), n, replace = TRUE),
    study_covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    study_covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    study_covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- hierarchicalbayes(
      data = data,
    tp = 'tp',
    fp = 'fp',
    fn = 'fn',
    tn = 'tn',
    study_id = 'study_id',
    study_covariates = c('study_covariates1', 'study_covariates2', 'study_covariates3'),
    model_type = 'bivariate_normal',
    correlation_model = 'unstructured',
    prior_specification = 'weakly_informative',
    mcmc_chains = 4,
    mcmc_iterations = 20000,
    warmup_iterations = 10000,
    thin_interval = 1,
    adapt_delta = 0.95,
    max_treedepth = 15,
    credible_level = 0.95,
    prediction_interval = TRUE,
    cross_validation = FALSE,
    posterior_predictive = TRUE,
    convergence_diagnostics = TRUE,
    model_comparison = TRUE,
    forest_plots = TRUE,
    sroc_curves = TRUE,
    trace_plots = FALSE,
    density_plots = TRUE,
    pairs_plots = FALSE,
    shrinkage_plots = FALSE,
    meta_regression = FALSE,
    outlier_detection = TRUE,
    sensitivity_analysis = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'hierarchicalbayes.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

