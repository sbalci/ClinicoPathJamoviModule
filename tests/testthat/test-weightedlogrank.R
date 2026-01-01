
test_that('weightedlogrank analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    explanatory = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- weightedlogrank(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    explanatory = 'explanatory',
    gehan_wilcoxon = TRUE,
    tarone_ware = TRUE,
    peto_peto = TRUE,
    modified_peto = FALSE,
    standard_logrank = TRUE,
    confidence_level = 0.95,
    show_kaplan_meier = TRUE,
    show_weight_functions = FALSE,
    show_test_statistics = FALSE,
    show_sample_sizes = TRUE,
    show_events = TRUE,
    median_survival = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'weightedlogrank.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

