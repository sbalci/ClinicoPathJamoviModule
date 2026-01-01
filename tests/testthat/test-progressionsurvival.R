
test_that('progressionsurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    progression_var = sample(c('A', 'B'), n, replace = TRUE),
    death_var = sample(c('A', 'B'), n, replace = TRUE),
    treatment_var = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    patient_id = sample(c('A', 'B'), n, replace = TRUE),
    baseline_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    baseline_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    baseline_vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- progressionsurvival(
      data = data,
    time_var = 'time_var',
    progression_var = 'progression_var',
    death_var = 'death_var',
    treatment_var = 'treatment_var',
    stratification_vars = c('stratification_vars1', 'stratification_vars2', 'stratification_vars3'),
    patient_id = 'patient_id',
    baseline_vars = c('baseline_vars1', 'baseline_vars2', 'baseline_vars3'),
    analysis_type = 'standard_pfs',
    progression_definition = 'recist',
    censoring_mechanism = 'administrative',
    survival_method = 'kaplan_meier',
    comparison_test = 'logrank',
    regression_model = 'cox_ph',
    rmst_tau = 24,
    confidence_level = 0.95,
    alpha_spending = 'obrien_fleming',
    kaplan_meier_curves = TRUE,
    cumulative_incidence = FALSE,
    landmark_analysis_plot = FALSE,
    progression_patterns = FALSE,
    treatment_effect_estimation = TRUE,
    subgroup_analysis = FALSE,
    biomarker_interaction = FALSE,
    sensitivity_analysis = FALSE,
    interim_monitoring = FALSE,
    progression_velocity = FALSE,
    clinical_significance = TRUE,
    regulatory_endpoints = FALSE,
    health_economic_outcomes = FALSE,
    plot_theme = 'publication',
    risk_tables = TRUE,
    median_survival_lines = TRUE,
    confidence_bands = TRUE,
    generate_report = FALSE,
    regulatory_tables = FALSE,
    consort_diagram = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'progressionsurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

