
test_that('survivalfeaturerank analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    survtime = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    features1 = sample(c('A', 'B'), n, replace = TRUE),
    features2 = sample(c('A', 'B'), n, replace = TRUE),
    features3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- survivalfeaturerank(
      data = data,
    survtime = 'survtime',
    event = 'event',
    features = c('features1', 'features2', 'features3'),
    rankBy = 'pvalue',
    showCI = TRUE,
    adjustPValues = FALSE,
    adjustMethod = 'fdr',
    showForestPlot = TRUE,
    forestStyle = 'standard',
    showTopKM = FALSE,
    topN = 5,
    kmLayout = 'separate',
    endplot = 60,
    byplot = 12,
    risktable = FALSE,
    pplot = TRUE,
    showSummary = TRUE,
    alphaLevel = 0.05,
    showFullTable = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'survivalfeaturerank.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

