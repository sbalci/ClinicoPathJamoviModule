
test_that('plscox analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- plscox(
      data = data,
    time = 'time',
    status = 'status',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    pls_components = 5,
    cross_validation = 'k10',
    component_selection = 'cv_loglik',
    scaling_method = 'standardize',
    pls_algorithm = 'nipals',
    max_iterations = 50,
    tolerance = 1e-10,
    bootstrap_validation = FALSE,
    n_bootstrap = 200,
    permutation_test = FALSE,
    n_permutations = 100,
    plot_components = TRUE,
    plot_loadings = TRUE,
    plot_scores = TRUE,
    plot_validation = TRUE,
    plot_survival = TRUE,
    risk_groups = 3,
    confidence_intervals = TRUE,
    feature_importance = TRUE,
    prediction_accuracy = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'plscox.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

