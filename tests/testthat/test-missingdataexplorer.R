
test_that('missingdataexplorer analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    analysis_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    analysis_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    analysis_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    group_var = sample(c('A', 'B'), n, replace = TRUE),
    time_var = runif(n, 1, 100),
    id_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- missingdataexplorer(
      data = data,
    analysis_vars = c('analysis_vars1', 'analysis_vars2', 'analysis_vars3'),
    group_var = 'group_var',
    time_var = 'time_var',
    id_var = 'id_var',
    pattern_analysis = TRUE,
    mechanism_testing = TRUE,
    correlation_analysis = TRUE,
    temporal_analysis = FALSE,
    group_comparison = FALSE,
    mcar_test = 'little',
    min_pattern_freq = 0.05,
    max_patterns_display = 20,
    pattern_plot = TRUE,
    correlation_plot = TRUE,
    temporal_plot = FALSE,
    upset_plot = TRUE,
    cumulative_plot = FALSE,
    monotonic_test = TRUE,
    dropout_analysis = FALSE,
    informative_missingness = TRUE,
    completeness_threshold = 0.8,
    case_completeness = TRUE,
    variable_importance = TRUE,
    chi_square_test = TRUE,
    logistic_regression = FALSE,
    survival_analysis = FALSE,
    detailed_patterns = TRUE,
    summary_statistics = TRUE,
    clinical_interpretation = TRUE,
    export_patterns = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'missingdataexplorer.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

