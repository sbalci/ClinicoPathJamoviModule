
test_that('prauc analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictor = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- prauc(
      data = data,
    outcome = 'outcome',
    predictor = 'predictor',
    prevalence = 0,
    calculate_auc = TRUE,
    calculate_fscore = TRUE,
    confidence_intervals = TRUE,
    ci_method = 'bootstrap',
    bootstrap_samples = 100,
    confidence_level = 0.95,
    compare_to_roc = TRUE,
    baseline_comparison = TRUE,
    interpolation_method = 'step',
    plot_pr_curve = TRUE,
    plot_comparison = FALSE,
    plot_fscore = FALSE,
    min_threshold = 0,
    max_threshold = 1,
    random_seed = 12345
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'prauc.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

