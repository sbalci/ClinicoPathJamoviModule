
test_that('ihcimmune analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    immune_markers1 = sample(c('A', 'B'), n, replace = TRUE),
    immune_markers2 = sample(c('A', 'B'), n, replace = TRUE),
    immune_markers3 = sample(c('A', 'B'), n, replace = TRUE),
    checkpoint_markers1 = sample(c('A', 'B'), n, replace = TRUE),
    checkpoint_markers2 = sample(c('A', 'B'), n, replace = TRUE),
    checkpoint_markers3 = sample(c('A', 'B'), n, replace = TRUE),
    id = sample(c('A', 'B'), n, replace = TRUE),
    x_coordinate = runif(n, 1, 100),
    y_coordinate = runif(n, 1, 100),
    tumorRegion = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- ihcimmune(
      data = data,
    immune_markers = c('immune_markers1', 'immune_markers2', 'immune_markers3'),
    checkpoint_markers = c('checkpoint_markers1', 'checkpoint_markers2', 'checkpoint_markers3'),
    id = 'id',
    tilAnalysis = TRUE,
    tilMethod = 'comprehensive',
    immuneContexture = TRUE,
    spatialAnalysis = FALSE,
    x_coordinate = 'x_coordinate',
    y_coordinate = 'y_coordinate',
    tumorRegion = 'tumorRegion',
    hotspotAnalysis = FALSE,
    diversityMetrics = TRUE,
    checkpointScore = FALSE,
    pd1Cutoff = 1,
    pdl1Cutoff = 1,
    cd3Threshold = 10,
    cd8Threshold = 5,
    immuneScoreThreshold = 10
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'ihcimmune.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

