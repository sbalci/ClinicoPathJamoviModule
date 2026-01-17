
test_that('bayesianmetaanalysis analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    effectSize = runif(n, 1, 100),
    standardError = runif(n, 1, 100),
    studyId = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- bayesianmetaanalysis(
      data = data,
    effectSize = 'effectSize',
    standardError = 'standardError',
    studyId = 'studyId',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    modelType = 'random_effects',
    outcomeType = 'continuous',
    priorType = 'weakly_informative',
    mcmcChains = 2,
    mcmcIterations = 1000,
    warmupIterations = 500,
    credibleInterval = 0.95,
    publicationBias = FALSE,
    posteriorPredictive = TRUE,
    plotForest = TRUE,
    plotPosterior = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(inherits(model, 'R6'))
  expect_true(inherits(model, 'jmvcoreClass'))
  
  # Define output path
  # omv_path <- file.path('omv_output', 'bayesianmetaanalysis.omv')
  # if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  # expect_no_error({
  #   jmvReadWrite::write_omv(model, omv_path)
  # })

  # expect_true(file.exists(omv_path))
})

