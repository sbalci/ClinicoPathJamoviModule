
test_that('causespecifichazards analysis works', {
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
    cause_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- causespecifichazards(
      data = data,
    elapsedtime = 'elapsedtime',
    outcome = 'outcome',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    cause_variable = 'cause_variable',
    model_type = 'cox',
    include_se = TRUE,
    include_ci = TRUE,
    conf_level = 0.95,
    cumulative_incidence = TRUE,
    model_comparison = FALSE,
    proportional_hazards_test = FALSE,
    plot_cumulative_incidence = FALSE,
    plot_hazards = FALSE,
    plot_diagnostics = FALSE,
    stratify_plots = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'causespecifichazards.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

