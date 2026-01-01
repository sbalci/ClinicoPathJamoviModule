
test_that('generalizedroc analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictor = runif(n, 1, 100),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- generalizedroc(
      data = data,
    outcome = 'outcome',
    predictor = 'predictor',
    assume_equal_variance = FALSE,
    transformation = 'none',
    distribution_model = 'normal',
    calculate_auc = TRUE,
    confidence_intervals = TRUE,
    ci_method = 'bootstrap',
    bootstrap_samples = 100,
    confidence_level = 0.95,
    show_diagnostics = TRUE,
    variance_test = 'levene',
    optimal_threshold = TRUE,
    plot_roc = TRUE,
    plot_distributions = TRUE,
    plot_diagnostic = FALSE,
    random_seed = 42,
    use_tram = FALSE,
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    tram_model = 'Colr',
    plot_covariate_roc = TRUE,
    plot_auc_vs_covariate = TRUE,
    n_covariate_points = 50,
    tram_constraints = 'none',
    censoring_aware = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'generalizedroc.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

