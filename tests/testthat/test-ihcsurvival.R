
test_that('ihcsurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    markers1 = sample(c('A', 'B'), n, replace = TRUE),
    markers2 = sample(c('A', 'B'), n, replace = TRUE),
    markers3 = sample(c('A', 'B'), n, replace = TRUE),
    survivalTime = runif(n, 1, 100),
    survivalEvent = sample(c('A', 'B'), n, replace = TRUE),
    id = sample(c('A', 'B'), n, replace = TRUE),
    centralRegion1 = sample(c('A', 'B'), n, replace = TRUE),
    centralRegion2 = sample(c('A', 'B'), n, replace = TRUE),
    centralRegion3 = sample(c('A', 'B'), n, replace = TRUE),
    invasiveRegion1 = sample(c('A', 'B'), n, replace = TRUE),
    invasiveRegion2 = sample(c('A', 'B'), n, replace = TRUE),
    invasiveRegion3 = sample(c('A', 'B'), n, replace = TRUE),
    multivariateAdjustment1 = sample(c('A', 'B'), n, replace = TRUE),
    multivariateAdjustment2 = sample(c('A', 'B'), n, replace = TRUE),
    multivariateAdjustment3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- ihcsurvival(
      data = data,
    markers = c('markers1', 'markers2', 'markers3'),
    survivalTime = 'survivalTime',
    survivalEvent = 'survivalEvent',
    id = 'id',
    prognosticClustering = TRUE,
    multiRegionAnalysis = FALSE,
    centralRegion = c('centralRegion1', 'centralRegion2', 'centralRegion3'),
    invasiveRegion = c('invasiveRegion1', 'invasiveRegion2', 'invasiveRegion3'),
    riskStratification = 'tertiles',
    coxRegression = TRUE,
    multivariateAdjustment = c('multivariateAdjustment1', 'multivariateAdjustment2', 'multivariateAdjustment3'),
    kaplanMeierPlots = TRUE,
    landmarkAnalysis = FALSE,
    landmarkTime = 60,
    confidenceLevel = 0.95
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'ihcsurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

