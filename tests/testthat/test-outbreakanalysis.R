
test_that('outbreakanalysis analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    case_status = sample(c('A', 'B'), n, replace = TRUE),
    exposure_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    exposure_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    exposure_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    date_onset = sample(c('A', 'B'), n, replace = TRUE),
    location_var = sample(c('A', 'B'), n, replace = TRUE),
    person_id = sample(c('A', 'B'), n, replace = TRUE),
    age_var = runif(n, 1, 100),
    sex_var = sample(c('A', 'B'), n, replace = TRUE),
    additional_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    additional_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    additional_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    exposure_date_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- outbreakanalysis(
      data = data,
    case_status = 'case_status',
    exposure_vars = c('exposure_vars1', 'exposure_vars2', 'exposure_vars3'),
    date_onset = 'date_onset',
    location_var = 'location_var',
    person_id = 'person_id',
    age_var = 'age_var',
    sex_var = 'sex_var',
    additional_vars = c('additional_vars1', 'additional_vars2', 'additional_vars3'),
    outbreak_type = 'unknown',
    analysis_type = 'combined',
    epidemic_curve_unit = 'auto',
    attack_rate_analysis = TRUE,
    risk_factor_analysis = TRUE,
    dose_response_analysis = FALSE,
    temporal_analysis = TRUE,
    spatial_analysis = FALSE,
    statistical_tests = 'all_tests',
    confidence_level = 0.95,
    multiple_testing_correction = 'holm',
    stratified_analysis = FALSE,
    stratification_vars = c('stratification_vars1', 'stratification_vars2', 'stratification_vars3'),
    case_definition = TRUE,
    incubation_period = FALSE,
    exposure_date_var = 'exposure_date_var',
    epidemic_curve_plot = TRUE,
    attack_rate_plot = TRUE,
    risk_factor_plot = TRUE,
    spatial_plot = FALSE,
    power_calculation = FALSE,
    sample_size_calculation = FALSE,
    sensitivity_analysis = FALSE,
    comprehensive_report = TRUE,
    public_health_summary = TRUE,
    surveillance_indicators = FALSE,
    data_quality_assessment = TRUE,
    strobe_compliance = TRUE,
    export_for_epiinfo = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'outbreakanalysis.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

