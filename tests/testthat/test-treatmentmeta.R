
test_that('treatmentmeta analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    study_id = sample(c('A', 'B'), n, replace = TRUE),
    year = runif(n, 1, 100),
    mean_treatment = runif(n, 1, 100),
    sd_treatment = runif(n, 1, 100),
    n_treatment = runif(n, 1, 100),
    mean_control = runif(n, 1, 100),
    sd_control = runif(n, 1, 100),
    n_control = runif(n, 1, 100),
    events_treatment = runif(n, 1, 100),
    events_control = runif(n, 1, 100),
    correlation = runif(n, 1, 100),
    sample_size = runif(n, 1, 100),
    effect_size = runif(n, 1, 100),
    standard_error = runif(n, 1, 100),
    ci_lower = runif(n, 1, 100),
    ci_upper = runif(n, 1, 100),
    hazard_ratio = runif(n, 1, 100),
    log_hr_se = runif(n, 1, 100),
    subgroup_var = sample(c('A', 'B'), n, replace = TRUE),
    moderator_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    moderator_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    moderator_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    quality_score = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- treatmentmeta(
      data = data,
    study_id = 'study_id',
    year = 'year',
    outcome_type = 'continuous',
    mean_treatment = 'mean_treatment',
    sd_treatment = 'sd_treatment',
    n_treatment = 'n_treatment',
    mean_control = 'mean_control',
    sd_control = 'sd_control',
    n_control = 'n_control',
    events_treatment = 'events_treatment',
    events_control = 'events_control',
    correlation = 'correlation',
    sample_size = 'sample_size',
    effect_size = 'effect_size',
    standard_error = 'standard_error',
    ci_lower = 'ci_lower',
    ci_upper = 'ci_upper',
    hazard_ratio = 'hazard_ratio',
    log_hr_se = 'log_hr_se',
    effect_measure = 'SMD',
    model_type = 'random',
    heterogeneity_test = TRUE,
    prediction_interval = TRUE,
    subgroup_var = 'subgroup_var',
    subgroup_test = TRUE,
    moderator_vars = c('moderator_vars1', 'moderator_vars2', 'moderator_vars3'),
    sensitivity_analysis = TRUE,
    influence_diagnostics = TRUE,
    publication_bias = TRUE,
    funnel_plot = TRUE,
    eggers_test = TRUE,
    trim_fill = TRUE,
    pcurve = FALSE,
    quality_score = 'quality_score',
    weight_by_quality = FALSE,
    confidence_level = 0.95,
    small_study_correction = TRUE,
    bayesian_analysis = FALSE,
    forest_plot = TRUE,
    baujat_plot = FALSE,
    radial_plot = FALSE,
    cumulative_meta = FALSE,
    show_weights = TRUE,
    show_interpretation = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'treatmentmeta.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

