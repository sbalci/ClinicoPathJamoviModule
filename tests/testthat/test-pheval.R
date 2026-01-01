
test_that('pheval analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    elapsedtime = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratify_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- pheval(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    test_schoenfeld = TRUE,
    test_scaled_schoenfeld = TRUE,
    test_global = TRUE,
    test_correlation = FALSE,
    test_logrank = FALSE,
    test_supremum = FALSE,
    time_transform = 'identity',
    confidence_level = 0.95,
    rho_parameter = 0,
    global_test_method = 'chisquare',
    stratify_variable = 'stratify_variable',
    show_individual_tests = TRUE,
    show_global_tests = TRUE,
    show_residual_plots = TRUE,
    show_diagnostic_plots = TRUE,
    show_time_varying_plots = TRUE,
    show_model_summary = TRUE,
    show_recommendations = TRUE,
    showSummaries = FALSE,
    showExplanations = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'pheval.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

