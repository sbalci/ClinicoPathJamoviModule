
test_that('finegray analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    survivalTime = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    groupVar = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- finegray(
      data = data,
    survivalTime = 'survivalTime',
    status = 'status',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    groupVar = 'groupVar',
    showCoefficientTable = TRUE,
    exponentiate = TRUE,
    confLevel = 0.95,
    showGrayTest = TRUE,
    showModelFit = TRUE,
    showCIFPlot = TRUE,
    cifPlotBy = 'group',
    showRiskTable = TRUE,
    colorScheme = 'default',
    cifConfInt = FALSE,
    cifConfLevel = 0.95,
    predictCovariatePattern = 'mean',
    showInterpretation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'finegray.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

