
test_that('variablebiplot analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    groupVar = sample(c('A', 'B'), n, replace = TRUE),
    features1 = sample(c('A', 'B'), n, replace = TRUE),
    features2 = sample(c('A', 'B'), n, replace = TRUE),
    features3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- variablebiplot(
      data = data,
    groupVar = 'groupVar',
    features = c('features1', 'features2', 'features3'),
    method = 'pca',
    showLoadings = FALSE,
    loadingScale = 1.5,
    topContributors = 10,
    pc1 = 1,
    pc2 = 2,
    showContribTable = FALSE,
    showVarianceExplained = FALSE,
    contributionMetric = 'squared',
    showSeparation = FALSE,
    separationMetric = 'silhouette',
    showConfidenceEllipse = FALSE,
    pointSize = 3,
    labelPoints = 'none',
    centerScale = FALSE,
    minVariance = 70,
    biplotType = 'covariance',
    showSummary = FALSE,
    showReport = FALSE,
    showInterpretation = FALSE,
    showRCode = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'variablebiplot.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

