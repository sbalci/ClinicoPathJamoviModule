
test_that('reportcat2 analysis works', {
  skip_if_not_installed('jmvReadWrite')

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = sample(c('A', 'B'), n, replace = TRUE),
    vars2 = sample(c('A', 'B'), n, replace = TRUE),
    vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- reportcat2(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    sumvar_style = FALSE,
    show_proportions = TRUE,
    sort_by_frequency = FALSE
    )
  })

  # Verify results structure
  expect_true(inherits(model, 'reportcat2Results') || inherits(model, 'ResultsElement'))
  expect_true("text1" %in% names(model))

  # Define output path
  # omv_path <- file.path('omv_output', 'reportcat2.omv')
  # if (!dir.exists('omv_output')) dir.create('omv_output')
  # jmvReadWrite::write_omv does not support HTML-only output groups
  # jmvReadWrite::write_omv(model, omv_path)
})

