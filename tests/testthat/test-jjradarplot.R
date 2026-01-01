
test_that('jjradarplot analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100),
    categoryVar = sample(c('A', 'B'), n, replace = TRUE),
    splitBy = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- jjradarplot(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    categoryVar = 'categoryVar',
    splitBy = 'splitBy',
    scaleData = TRUE,
    aggregationMethod = 'mean',
    alpha = 0.25,
    lineSize = 1,
    pointSize = 2,
    legendPosition = 'right',
    axisLabelSize = 10,
    facetLabelSize = 11,
    excl = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'jjradarplot.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

