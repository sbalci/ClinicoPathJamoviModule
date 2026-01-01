
test_that('penalizedcox analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    dxdate = runif(n, 1, 100),
    fudate = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- penalizedcox(
      data = data,
    elapsedtime = 'elapsedtime',
    tint = FALSE,
    dxdate = 'dxdate',
    fudate = 'fudate',
    timetypedata = 'ymd',
    timetypeoutput = 'months',
    outcome = 'outcome',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    penalty_type = 'lasso',
    alpha = 0.5,
    lambda_selection = '1se',
    lambda_custom = 0.01,
    cv_folds = 10,
    cv_type = 'deviance',
    variable_selection = TRUE,
    standardize = TRUE,
    include_intercept = FALSE,
    bootstrap_validation = FALSE,
    bootstrap_samples = 50,
    predict_risk = TRUE,
    survival_curves = FALSE,
    risk_groups = '3',
    coefficient_plot = TRUE,
    cv_plot = TRUE,
    variable_importance = FALSE,
    max_variables = 100,
    convergence_threshold = 1e-8,
    show_coefficients = TRUE,
    show_model_metrics = TRUE,
    show_lambda_path = FALSE,
    showSummaries = FALSE,
    showExplanations = FALSE,
    addRiskScore = FALSE,
    addRiskGroup = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'penalizedcox.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

