
test_that('pcacomponenttest analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- pcacomponenttest(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    ncomp = 5,
    nperm = 1000,
    stop_rule = TRUE,
    seed = 123,
    center = TRUE,
    scale = TRUE,
    conflevel = 0.95,
    adjustmethod = 'BH',
    showpercent = TRUE,
    showScreePlot = FALSE,
    showLoadingsPlot = FALSE,
    nLoadings = 5,
    plotwidth = 600,
    plotheight = 450,
    showSummary = FALSE,
    showGuide = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'pcacomponenttest.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

