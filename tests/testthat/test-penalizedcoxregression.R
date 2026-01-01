
test_that('penalizedcoxregression analysis works', {
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
    model <- penalizedcoxregression(
      data = data,
    time = 'time',
    status = 'status',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    penaltyType = 'lasso',
    alphaValue = 0.5,
    crossValidation = TRUE,
    cvFolds = 10,
    lambdaSelection = 'lambda.1se',
    customLambda = 0.01,
    standardize = TRUE,
    plotCoefficientPath = TRUE,
    plotCrossValidation = TRUE,
    variableSelection = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'penalizedcoxregression.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

