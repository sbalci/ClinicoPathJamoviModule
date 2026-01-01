
test_that('sigmametrics analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    bias = runif(n, 1, 100),
    cv = runif(n, 1, 100),
    tea = runif(n, 1, 100),
    analyte = sample(c('A', 'B'), n, replace = TRUE),
    laboratory = sample(c('A', 'B'), n, replace = TRUE),
    method = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- sigmametrics(
      data = data,
    bias = 'bias',
    cv = 'cv',
    tea = 'tea',
    analyte = 'analyte',
    laboratory = 'laboratory',
    method = 'method',
    sigma_calculation = 'standard',
    quality_goals = 'clia',
    custom_tea = 10,
    confidence_level = 0.95,
    benchmark_comparison = TRUE,
    process_capability = TRUE,
    quality_goal_index = TRUE,
    method_validation = TRUE,
    improvement_recommendations = TRUE,
    regulatory_compliance = TRUE,
    cost_of_quality = FALSE,
    defect_rates = TRUE,
    control_planning = FALSE,
    sigma_plots = TRUE,
    normalized_plots = TRUE,
    quality_scorecard = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'sigmametrics.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

