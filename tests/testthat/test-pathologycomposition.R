
test_that('pathologycomposition analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome_variable = sample(c('A', 'B'), n, replace = TRUE),
    component1 = sample(c('A', 'B'), n, replace = TRUE),
    component2 = sample(c('A', 'B'), n, replace = TRUE),
    component3 = sample(c('A', 'B'), n, replace = TRUE),
    component4 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- pathologycomposition(
      data = data,
    outcome_variable = 'outcome_variable',
    component1 = 'component1',
    component2 = 'component2',
    component3 = 'component3',
    component4 = 'component4',
    composition_analysis = TRUE,
    optimal_composition = TRUE,
    trend_test = TRUE,
    confidence_level = 0.95,
    low_risk_threshold = 0.05,
    high_risk_threshold = 0.2,
    min_group_size = 10,
    quantitative_categories = 'gastric_cancer',
    composition_plot = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'pathologycomposition.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

