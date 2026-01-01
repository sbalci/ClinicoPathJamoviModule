
test_that('relativesurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    age = runif(n, 1, 100),
    sex = sample(c('A', 'B'), n, replace = TRUE),
    year = runif(n, 1, 100),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- relativesurvival(
      data = data,
    time = 'time',
    status = 'status',
    age = 'age',
    sex = 'sex',
    year = 'year',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    ratetable = 'us',
    method = 'poharperme',
    time_scale = 'years',
    net_survival = TRUE,
    excess_mortality = TRUE,
    crude_probability = TRUE,
    age_standardized = FALSE,
    period_analysis = FALSE,
    regression_model = 'none',
    spline_df = 4,
    plot_observed = TRUE,
    plot_expected = TRUE,
    plot_relative = TRUE,
    plot_excess = TRUE,
    confidence_level = 0.95
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'relativesurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

