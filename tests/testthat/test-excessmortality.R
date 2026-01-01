
test_that('excessmortality analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    timeVar = runif(n, 1, 100),
    statusVar = sample(c('A', 'B'), n, replace = TRUE),
    ageVar = runif(n, 1, 100),
    sexVar = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- excessmortality(
      data = data,
    timeVar = 'timeVar',
    statusVar = 'statusVar',
    ageVar = 'ageVar',
    sexVar = 'sexVar',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    expectedRate = 'population',
    splineType = 'bs',
    nknots = 4,
    degree = 3,
    confidenceLevel = 95,
    plotHazard = TRUE,
    plotSurvival = TRUE,
    plotCumHazard = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'excessmortality.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

