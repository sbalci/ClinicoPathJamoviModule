
test_that('assayoptimization analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    response_var = runif(n, 1, 100),
    factors1 = sample(c('A', 'B'), n, replace = TRUE),
    factors2 = sample(c('A', 'B'), n, replace = TRUE),
    factors3 = sample(c('A', 'B'), n, replace = TRUE),
    blocking_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    blocking_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    blocking_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = runif(n, 1, 100),
    covariates2 = runif(n, 1, 100),
    covariates3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- assayoptimization(
      data = data,
    response_var = 'response_var',
    factors = c('factors1', 'factors2', 'factors3'),
    blocking_vars = c('blocking_vars1', 'blocking_vars2', 'blocking_vars3'),
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    design_type = 'factorial',
    optimization_goal = 'maximize_response',
    target_value = 1,
    power_analysis = TRUE,
    alpha_level = 0.05,
    power_level = 0.8,
    effect_size = 0.5,
    replicates = 3,
    randomization = 'complete',
    quality_control = TRUE,
    control_charts = 'xbar_r',
    robust_methods = FALSE,
    response_surface = TRUE,
    contour_plots = TRUE,
    optimization_plots = TRUE,
    export_design = FALSE,
    method_validation = FALSE,
    confidence_level = 0.95
    )
  })

  # Verify and Export OMV
  print(class(model))
  expect_true(inherits(model, "R6"))
  expect_true(inherits(model, 'assayoptimizationResults'))

  # Define output path
  omv_path <- file.path('omv_output', 'assayoptimization.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  # expect_no_error({
  #   jmvReadWrite::write_omv(model, omv_path)
  # })

  # expect_true(file.exists(omv_path))
})

