
test_that('ihcbasic analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    markers1 = sample(c('A', 'B'), n, replace = TRUE),
    markers2 = sample(c('A', 'B'), n, replace = TRUE),
    markers3 = sample(c('A', 'B'), n, replace = TRUE),
    id = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- ihcbasic(
      data = data,
    markers = c('markers1', 'markers2', 'markers3'),
    id = 'id',
    computeHScore = FALSE,
    clusterMethod = 'hierarchical',
    nClusters = 3,
    distanceMetric = 'gower',
    linkageMethod = 'ward.D2',
    standardizeData = TRUE,
    showDendrogram = TRUE,
    showHeatmap = TRUE,
    silhouetteAnalysis = TRUE,
    scoringScale = 'standard'
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'ihcbasic.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

