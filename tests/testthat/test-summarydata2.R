
test_that('summarydata2 analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100),
    date_vars1 = runif(n, 1, 100),
    date_vars2 = runif(n, 1, 100),
    date_vars3 = runif(n, 1, 100),
    grvar = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- summarydata2(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    date_vars = c('date_vars1', 'date_vars2', 'date_vars3'),
    distr = FALSE,
    summary_format = 'standard',
    grvar = 'grvar',
    pivot_layout = 'clinical',
    include_confidence = TRUE,
    advanced_metrics = FALSE,
    pivot_export = FALSE,
    summarytools_graphs = TRUE,
    summarytools_round_digits = 2
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'summarydata2.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

