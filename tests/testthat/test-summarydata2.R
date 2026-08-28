
test_that('summarydata2 analysis works', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100),
    date_vars1 = as.numeric(as.Date("2020-01-01") + 1:n),
    date_vars2 = as.numeric(as.Date("2021-01-01") + 1:n),
    date_vars3 = as.numeric(as.Date("2022-01-01") + 1:n),
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

  # Verify results structure
  expect_true(inherits(model, 'summarydata2Results') || inherits(model, 'ResultsElement'))
  expect_true("text" %in% names(model))

  # Define output path
  # omv_path <- file.path('omv_output', 'summarydata2.omv')
  # if (!dir.exists('omv_output')) dir.create('omv_output')
  # jmvReadWrite::write_omv does not support HTML-only output groups
  # jmvReadWrite::write_omv(model, omv_path)
})
