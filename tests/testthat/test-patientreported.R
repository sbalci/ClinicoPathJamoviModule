
test_that('patientreported analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    scale_items1 = sample(c('A', 'B'), n, replace = TRUE),
    scale_items2 = sample(c('A', 'B'), n, replace = TRUE),
    scale_items3 = sample(c('A', 'B'), n, replace = TRUE),
    patient_id = sample(c('A', 'B'), n, replace = TRUE),
    time_var = sample(c('A', 'B'), n, replace = TRUE),
    group_var = sample(c('A', 'B'), n, replace = TRUE),
    demographic_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    demographic_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    demographic_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    reverse_coded_items1 = sample(c('A', 'B'), n, replace = TRUE),
    reverse_coded_items2 = sample(c('A', 'B'), n, replace = TRUE),
    reverse_coded_items3 = sample(c('A', 'B'), n, replace = TRUE),
    anchor_variables1 = sample(c('A', 'B'), n, replace = TRUE),
    anchor_variables2 = sample(c('A', 'B'), n, replace = TRUE),
    anchor_variables3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- patientreported(
      data = data,
    scale_items = c('scale_items1', 'scale_items2', 'scale_items3'),
    patient_id = 'patient_id',
    time_var = 'time_var',
    group_var = 'group_var',
    demographic_vars = c('demographic_vars1', 'demographic_vars2', 'demographic_vars3'),
    scale_type = 'generic_qol',
    instrument_name = 'custom',
    scoring_method = 'sum_score',
    reverse_coded_items = c('reverse_coded_items1', 'reverse_coded_items2', 'reverse_coded_items3'),
    response_scale_min = 1,
    response_scale_max = 5,
    reliability_analysis = TRUE,
    validity_analysis = TRUE,
    factor_analysis = FALSE,
    irt_analysis = FALSE,
    dimensionality_test = TRUE,
    measurement_invariance = FALSE,
    missing_data_method = 'pro_rata_scoring',
    min_items_required = 2,
    missing_threshold = 0.5,
    clinical_interpretation = TRUE,
    normative_comparison = FALSE,
    minimal_important_difference = TRUE,
    mid_value = 5,
    ceiling_floor_effects = TRUE,
    longitudinal_analysis = FALSE,
    change_analysis = 'simple_change',
    trajectory_analysis = FALSE,
    time_to_deterioration = FALSE,
    group_comparisons = FALSE,
    comparison_method = 't_test',
    effect_size_analysis = TRUE,
    multiple_comparisons = 'fdr',
    responder_analysis = FALSE,
    responder_threshold = 10,
    anchor_based_analysis = FALSE,
    anchor_variables = c('anchor_variables1', 'anchor_variables2', 'anchor_variables3'),
    distribution_based_analysis = TRUE,
    data_quality_assessment = TRUE,
    response_patterns = TRUE,
    acquiescence_analysis = FALSE,
    detailed_output = TRUE,
    summary_report = TRUE,
    individual_profiles = FALSE,
    save_scores = FALSE,
    regulatory_documentation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'patientreported.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

