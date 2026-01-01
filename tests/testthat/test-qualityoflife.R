
test_that('qualityoflife analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    physical_function_items1 = sample(c('A', 'B'), n, replace = TRUE),
    physical_function_items2 = sample(c('A', 'B'), n, replace = TRUE),
    physical_function_items3 = sample(c('A', 'B'), n, replace = TRUE),
    role_physical_items1 = sample(c('A', 'B'), n, replace = TRUE),
    role_physical_items2 = sample(c('A', 'B'), n, replace = TRUE),
    role_physical_items3 = sample(c('A', 'B'), n, replace = TRUE),
    bodily_pain_items1 = sample(c('A', 'B'), n, replace = TRUE),
    bodily_pain_items2 = sample(c('A', 'B'), n, replace = TRUE),
    bodily_pain_items3 = sample(c('A', 'B'), n, replace = TRUE),
    general_health_items1 = sample(c('A', 'B'), n, replace = TRUE),
    general_health_items2 = sample(c('A', 'B'), n, replace = TRUE),
    general_health_items3 = sample(c('A', 'B'), n, replace = TRUE),
    vitality_items1 = sample(c('A', 'B'), n, replace = TRUE),
    vitality_items2 = sample(c('A', 'B'), n, replace = TRUE),
    vitality_items3 = sample(c('A', 'B'), n, replace = TRUE),
    social_function_items1 = sample(c('A', 'B'), n, replace = TRUE),
    social_function_items2 = sample(c('A', 'B'), n, replace = TRUE),
    social_function_items3 = sample(c('A', 'B'), n, replace = TRUE),
    role_emotional_items1 = sample(c('A', 'B'), n, replace = TRUE),
    role_emotional_items2 = sample(c('A', 'B'), n, replace = TRUE),
    role_emotional_items3 = sample(c('A', 'B'), n, replace = TRUE),
    mental_health_items1 = sample(c('A', 'B'), n, replace = TRUE),
    mental_health_items2 = sample(c('A', 'B'), n, replace = TRUE),
    mental_health_items3 = sample(c('A', 'B'), n, replace = TRUE),
    symptom_items1 = sample(c('A', 'B'), n, replace = TRUE),
    symptom_items2 = sample(c('A', 'B'), n, replace = TRUE),
    symptom_items3 = sample(c('A', 'B'), n, replace = TRUE),
    functional_items1 = sample(c('A', 'B'), n, replace = TRUE),
    functional_items2 = sample(c('A', 'B'), n, replace = TRUE),
    functional_items3 = sample(c('A', 'B'), n, replace = TRUE),
    global_qol_items1 = sample(c('A', 'B'), n, replace = TRUE),
    global_qol_items2 = sample(c('A', 'B'), n, replace = TRUE),
    global_qol_items3 = sample(c('A', 'B'), n, replace = TRUE),
    patient_id = sample(c('A', 'B'), n, replace = TRUE),
    time_var = sample(c('A', 'B'), n, replace = TRUE),
    group_var = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    clinical_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    cost_variables1 = runif(n, 1, 100),
    cost_variables2 = runif(n, 1, 100),
    cost_variables3 = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- qualityoflife(
      data = data,
    physical_function_items = c('physical_function_items1', 'physical_function_items2', 'physical_function_items3'),
    role_physical_items = c('role_physical_items1', 'role_physical_items2', 'role_physical_items3'),
    bodily_pain_items = c('bodily_pain_items1', 'bodily_pain_items2', 'bodily_pain_items3'),
    general_health_items = c('general_health_items1', 'general_health_items2', 'general_health_items3'),
    vitality_items = c('vitality_items1', 'vitality_items2', 'vitality_items3'),
    social_function_items = c('social_function_items1', 'social_function_items2', 'social_function_items3'),
    role_emotional_items = c('role_emotional_items1', 'role_emotional_items2', 'role_emotional_items3'),
    mental_health_items = c('mental_health_items1', 'mental_health_items2', 'mental_health_items3'),
    symptom_items = c('symptom_items1', 'symptom_items2', 'symptom_items3'),
    functional_items = c('functional_items1', 'functional_items2', 'functional_items3'),
    global_qol_items = c('global_qol_items1', 'global_qol_items2', 'global_qol_items3'),
    patient_id = 'patient_id',
    time_var = 'time_var',
    group_var = 'group_var',
    clinical_vars = c('clinical_vars1', 'clinical_vars2', 'clinical_vars3'),
    instrument_type = 'sf36',
    scoring_algorithm = 'standard',
    reference_population = 'general_us',
    missing_domain_threshold = 0.5,
    imputation_method = 'person_mean',
    quality_control = TRUE,
    response_pattern_analysis = TRUE,
    clinical_interpretation = TRUE,
    minimally_important_difference = TRUE,
    ceiling_floor_analysis = TRUE,
    health_utilities = FALSE,
    utility_algorithm = 'sf6d',
    longitudinal_analysis = FALSE,
    change_analysis = 'simple_change',
    baseline_adjustment = TRUE,
    time_to_deterioration = FALSE,
    group_comparisons = FALSE,
    comparison_domains = 'all_domains',
    statistical_test = 't_test',
    multiple_testing_correction = 'fdr',
    domain_correlations = TRUE,
    clinical_correlations = FALSE,
    predictive_modeling = FALSE,
    factor_analysis = FALSE,
    clustering_analysis = FALSE,
    health_economics = FALSE,
    qaly_calculation = FALSE,
    cost_effectiveness = FALSE,
    cost_variables = c('cost_variables1', 'cost_variables2', 'cost_variables3'),
    comprehensive_report = TRUE,
    domain_profiles = FALSE,
    population_norms_comparison = TRUE,
    clinical_cutoffs = TRUE,
    save_domain_scores = FALSE,
    save_utility_scores = FALSE,
    regulatory_documentation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'qualityoflife.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

