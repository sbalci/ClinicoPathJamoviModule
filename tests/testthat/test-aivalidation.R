
test_that('aivalidation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    predictorVars1 = runif(n, 1, 100),
    predictorVars2 = runif(n, 1, 100),
    predictorVars3 = runif(n, 1, 100),
    outcomeVar = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- aivalidation(
      data = data,
    predictorVars = c('predictorVars1', 'predictorVars2', 'predictorVars3'),
    outcomeVar = 'outcomeVar',
    compareModels = FALSE,
    youdensJ = FALSE,
    matthewsCC = FALSE,
    bootstrapCI = FALSE,
    nBootstrap = 1000,
    rocPlot = FALSE,
    crossValidation = 'none',
    stratified = TRUE,
    randomSeed = 42,
    showExplanations = FALSE,
    showSummaries = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'aivalidation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

