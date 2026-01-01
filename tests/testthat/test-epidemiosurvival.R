
test_that('epidemiosurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_var = runif(n, 1, 100),
    event_var = sample(c('A', 'B'), n, replace = TRUE),
    exposure_var = sample(c('A', 'B'), n, replace = TRUE),
    age_var = runif(n, 1, 100),
    calendar_time = runif(n, 1, 100),
    population_weights = runif(n, 1, 100),
    subcohort_indicator = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    competing_events1 = sample(c('A', 'B'), n, replace = TRUE),
    competing_events2 = sample(c('A', 'B'), n, replace = TRUE),
    competing_events3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- epidemiosurvival(
      data = data,
    time_var = 'time_var',
    event_var = 'event_var',
    exposure_var = 'exposure_var',
    age_var = 'age_var',
    calendar_time = 'calendar_time',
    population_weights = 'population_weights',
    subcohort_indicator = 'subcohort_indicator',
    stratification_vars = c('stratification_vars1', 'stratification_vars2', 'stratification_vars3'),
    competing_events = c('competing_events1', 'competing_events2', 'competing_events3'),
    analysis_type = 'cohort_survival',
    cohort_design = 'prospective',
    sampling_method = 'simple_random',
    survival_method = 'kaplan_meier',
    regression_method = 'cox_robust',
    age_standardization = 'none',
    subcohort_size = 1000,
    case_cohort_method = 'prentice',
    par_method = 'levin',
    confidence_level = 0.95,
    cohort_life_tables = TRUE,
    competing_risks_analysis = FALSE,
    age_period_cohort_effects = FALSE,
    population_impact_measures = FALSE,
    survival_disparities = FALSE,
    trend_analysis = FALSE,
    excess_mortality = FALSE,
    standardized_mortality_ratio = FALSE,
    survival_curves = TRUE,
    cumulative_incidence_plots = FALSE,
    age_specific_rates = FALSE,
    trend_plots = FALSE,
    forest_plots = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'epidemiosurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

