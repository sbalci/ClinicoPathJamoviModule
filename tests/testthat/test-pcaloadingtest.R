
test_that('pcaloadingtest analysis works', {
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
    model <- pcaloadingtest(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    ncomp = 3,
    nperm = 1000,
    seed = 123,
    componentfilter = 0,
    center = TRUE,
    scale = TRUE,
    conflevel = 0.95,
    adjustmethod = 'BH',
    plotwidth = 700,
    plotheight = 450
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'pcaloadingtest.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

