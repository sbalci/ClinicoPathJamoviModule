
test_that('esquisseauto analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    xvar = runif(n, 1, 100),
    yvar = runif(n, 1, 100),
    colorvar = runif(n, 1, 100),
    fillvar = runif(n, 1, 100),
    sizevar = runif(n, 1, 100),
    groupvar = runif(n, 1, 100),
    facetvar = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- esquisseauto(
      data = data,
    xvar = 'xvar',
    yvar = 'yvar',
    colorvar = 'colorvar',
    fillvar = 'fillvar',
    sizevar = 'sizevar',
    groupvar = 'groupvar',
    facetvar = 'facetvar',
    autoGeom = TRUE,
    manualGeom = 'auto',
    plotTheme = 'minimal',
    legendPosition = 'right',
    xAxisAngle = 0,
    yAxisAngle = 0,
    showCode = FALSE,
    addSmoother = FALSE,
    flipAxes = FALSE,
    showInstructions = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'esquisseauto.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

