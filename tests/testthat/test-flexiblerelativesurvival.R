
test_that('flexiblerelativesurvival analysis works', {
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
    yearVar = runif(n, 1, 100),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- flexiblerelativesurvival(
      data = data,
    timeVar = 'timeVar',
    statusVar = 'statusVar',
    ageVar = 'ageVar',
    sexVar = 'sexVar',
    yearVar = 'yearVar',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    expectedType = 'lifetable',
    smoothingMethod = 'splines',
    knotsTime = 5,
    knotsAge = 3,
    smoothingParameter = 1,
    confidenceLevel = 95,
    plotRelativeSurvival = TRUE,
    plotExcessHazard = TRUE,
    plotLifeExpectancy = FALSE,
    standardizeAge = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'flexiblerelativesurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

