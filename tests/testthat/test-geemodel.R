
test_that('geemodel analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    cluster_id = sample(c('A', 'B'), n, replace = TRUE),
    time_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- geemodel(
      data = data,
    outcome = 'outcome',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    cluster_id = 'cluster_id',
    time_var = 'time_var',
    family = 'gaussian',
    corstr = 'exchangeable',
    robust_se = TRUE,
    conf_level = 0.95,
    qic = TRUE,
    posthoc = FALSE,
    posthoc_adjust = 'holm',
    diagnostics = TRUE,
    correlation_plot = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'geemodel.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

