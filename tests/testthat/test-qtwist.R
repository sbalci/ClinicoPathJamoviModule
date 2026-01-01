
test_that('qtwist analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time_os = runif(n, 1, 100),
    event_os = sample(c('A', 'B'), n, replace = TRUE),
    time_pfs = runif(n, 1, 100),
    event_pfs = sample(c('A', 'B'), n, replace = TRUE),
    treatment = sample(c('A', 'B'), n, replace = TRUE),
    toxicity_duration_var = runif(n, 1, 100),
    toxicity_indicator_var = sample(c('A', 'B'), n, replace = TRUE),
    toxicity_start_var = runif(n, 1, 100),
    toxicity_end_var = runif(n, 1, 100),
    stratify_by1 = sample(c('A', 'B'), n, replace = TRUE),
    stratify_by2 = sample(c('A', 'B'), n, replace = TRUE),
    stratify_by3 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- qtwist(
      data = data,
    time_os = 'time_os',
    event_os = 'event_os',
    time_pfs = 'time_pfs',
    event_pfs = 'event_pfs',
    treatment = 'treatment',
    toxicity_method = 'fixed_window',
    toxicity_window = 3,
    toxicity_probability = 0.5,
    toxicity_duration_var = 'toxicity_duration_var',
    toxicity_indicator_var = 'toxicity_indicator_var',
    toxicity_start_var = 'toxicity_start_var',
    toxicity_end_var = 'toxicity_end_var',
    tau = 24,
    tau_selection = 'user_specified',
    utility_tox = 0.5,
    utility_twist = 1,
    utility_rel = 0.5,
    sensitivity_analysis = TRUE,
    threshold_analysis = FALSE,
    confidence_level = 0.95,
    bootstrap_ci = TRUE,
    bootstrap_samples = 100,
    bootstrap_seed = 2025,
    stratify_by = c('stratify_by1', 'stratify_by2', 'stratify_by3'),
    pooled_analysis = TRUE,
    show_state_partition = TRUE,
    show_qtwist_scores = TRUE,
    show_treatment_difference = TRUE,
    show_sensitivity_table = TRUE,
    show_rmst_components = FALSE,
    show_descriptive_stats = TRUE,
    plot_partitioned_survival = TRUE,
    plot_qtwist_comparison = TRUE,
    plot_sensitivity = TRUE,
    plot_km_curves = FALSE,
    plot_color_scheme = 'clinical',
    showExplanations = TRUE,
    showClinicalGuidance = TRUE,
    showFormulas = FALSE,
    showReferences = TRUE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'qtwist.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

