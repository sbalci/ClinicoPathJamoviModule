
test_that('screeningevaluation analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    screening_result = sample(c('A', 'B'), n, replace = TRUE),
    disease_status = sample(c('A', 'B'), n, replace = TRUE),
    participant_id = sample(c('A', 'B'), n, replace = TRUE),
    age_var = runif(n, 1, 100),
    sex_var = sample(c('A', 'B'), n, replace = TRUE),
    screening_date = sample(c('A', 'B'), n, replace = TRUE),
    screening_round = sample(c('A', 'B'), n, replace = TRUE),
    risk_factors1 = sample(c('A', 'B'), n, replace = TRUE),
    risk_factors2 = sample(c('A', 'B'), n, replace = TRUE),
    risk_factors3 = sample(c('A', 'B'), n, replace = TRUE),
    location_var = sample(c('A', 'B'), n, replace = TRUE),
    screening_cost_var = runif(n, 1, 100),
    followup_cost_var = runif(n, 1, 100),
    diagnosis_date_var = sample(c('A', 'B'), n, replace = TRUE),
    survival_time_var = runif(n, 1, 100),
    death_indicator = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- screeningevaluation(
      data = data,
    screening_result = 'screening_result',
    disease_status = 'disease_status',
    participant_id = 'participant_id',
    age_var = 'age_var',
    sex_var = 'sex_var',
    screening_date = 'screening_date',
    screening_round = 'screening_round',
    risk_factors = c('risk_factors1', 'risk_factors2', 'risk_factors3'),
    location_var = 'location_var',
    screening_type = 'population_based',
    target_disease = 'cancer',
    screening_test_type = 'laboratory',
    diagnostic_accuracy = TRUE,
    program_coverage = TRUE,
    detection_rates = TRUE,
    interval_cancer_analysis = FALSE,
    lead_time_analysis = FALSE,
    length_bias_analysis = FALSE,
    age_stratified_analysis = TRUE,
    risk_stratified_analysis = FALSE,
    geographic_analysis = FALSE,
    calculate_predictive_values = TRUE,
    likelihood_ratios = TRUE,
    roc_analysis = FALSE,
    optimal_cutpoint = FALSE,
    quality_indicators = TRUE,
    participation_rates = TRUE,
    recall_rates = TRUE,
    completion_rates = TRUE,
    cost_effectiveness = FALSE,
    cost_per_case_detected = FALSE,
    screening_cost_var = 'screening_cost_var',
    followup_cost_var = 'followup_cost_var',
    time_to_diagnosis = FALSE,
    diagnosis_date_var = 'diagnosis_date_var',
    survival_analysis = FALSE,
    survival_time_var = 'survival_time_var',
    death_indicator = 'death_indicator',
    overdiagnosis_analysis = FALSE,
    false_positive_impact = FALSE,
    screening_adherence = FALSE,
    performance_plots = TRUE,
    coverage_plots = TRUE,
    trend_analysis_plots = FALSE,
    comprehensive_report = TRUE,
    quality_assurance_report = TRUE,
    public_health_indicators = TRUE,
    international_standards = TRUE,
    export_for_registry = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'screeningevaluation.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

