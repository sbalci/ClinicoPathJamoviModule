
test_that('groupedbar analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    items1 = sample(c('A', 'B'), n, replace = TRUE),
    items2 = sample(c('A', 'B'), n, replace = TRUE),
    items3 = sample(c('A', 'B'), n, replace = TRUE),
    groups = sample(c('A', 'B'), n, replace = TRUE),
    values = runif(n, 1, 100),
    facetby = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- groupedbar(
      data = data,
    items = c('items1', 'items2', 'items3'),
    groups = 'groups',
    values = 'values',
    facetby = 'facetby',
    statistic = 'mean',
    plottype = 'grouped',
    sortorder = 'highest_first',
    grouporder = 'data',
    showvalues = TRUE,
    valueposition = 'outside',
    showerrorbars = FALSE,
    errortype = 'se',
    showconnectors = FALSE,
    colorscheme = 'default',
    referenceline = 1,
    showlegend = TRUE,
    legendposition = 'right',
    showstatistics = FALSE,
    testtype = 'auto',
    posthoc = FALSE,
    padjust = 'none',
    decimals = 1,
    highlightmax = FALSE,
    highlightmin = FALSE,
    gridlines = TRUE,
    aspectratio = 'auto',
    width = 10,
    height = 8,
    showTable = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'groupedbar.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

