
test_that('timeupdatedsurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    timeVar = runif(n, 1, 100),
    statusVar = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    timeVaryingCovs1 = sample(c('A', 'B'), n, replace = TRUE),
    timeVaryingCovs2 = sample(c('A', 'B'), n, replace = TRUE),
    timeVaryingCovs3 = sample(c('A', 'B'), n, replace = TRUE),
    stratificationVar = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- timeupdatedsurvival(
      data = data,
    timeVar = 'timeVar',
    statusVar = 'statusVar',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    timeVaryingCovs = c('timeVaryingCovs1', 'timeVaryingCovs2', 'timeVaryingCovs3'),
    stratificationVar = 'stratificationVar',
    modelType = 'dynreg',
    bandwidthMethod = 'auto',
    manualBandwidth = 1,
    confidenceLevel = 0.95,
    plotType = 'coefficients',
    showTable = TRUE,
    showPlots = TRUE,
    showGoFTests = TRUE,
    showExplanations = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'timeupdatedsurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

