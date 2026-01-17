
test_that('betabinomialdiagnostic analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    test_result = sample(c('A', 'B'), n, replace = TRUE),
    disease_status = sample(c('A', 'B'), n, replace = TRUE),
    study_id = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- betabinomialdiagnostic(
      data = data,
    test_result = 'test_result',
    disease_status = 'disease_status',
    study_id = 'study_id',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    overdispersion_model = 'beta_binomial',
    correlation_structure = 'independent',
    estimation_method = 'maximum_likelihood',
    heterogeneity_test = TRUE,
    tau_squared_method = 'dersimonian_laird',
    confidence_level = 0.95,
    alpha_prior = 1,
    beta_prior = 1,
    alpha_prior_spec = 1,
    beta_prior_spec = 1,
    mcmc_iterations = 10000,
    burnin_iterations = 2000,
    thin_factor = 5,
    convergence_diagnostics = TRUE,
    prediction_intervals = TRUE,
    forest_plot = TRUE,
    summary_roc_curve = TRUE,
    residual_plots = FALSE,
    influence_diagnostics = FALSE,
    publication_bias = FALSE,
    subgroup_analysis = FALSE,
    robust_variance = FALSE,
    finite_sample_correction = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(inherits(model, 'R6'))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  # omv_path <- file.path('omv_output', 'betabinomialdiagnostic.omv')
  # if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  # expect_no_error({
  #   jmvReadWrite::write_omv(model, omv_path)
  # })

  # expect_true(file.exists(omv_path))
})

