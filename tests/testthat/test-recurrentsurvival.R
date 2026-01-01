
test_that('recurrentsurvival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    subject_id = sample(c('A', 'B'), n, replace = TRUE),
    event_time = runif(n, 1, 100),
    event_status = sample(c('A', 'B'), n, replace = TRUE),
    terminal_event = runif(n, 1, 100),
    terminal_status = sample(c('A', 'B'), n, replace = TRUE),
    event_type = sample(c('A', 'B'), n, replace = TRUE),
    covariates1 = sample(c('A', 'B'), n, replace = TRUE),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    stratification_vars3 = sample(c('A', 'B'), n, replace = TRUE),
    cluster_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- recurrentsurvival(
      data = data,
    subject_id = 'subject_id',
    event_time = 'event_time',
    event_status = 'event_status',
    terminal_event = 'terminal_event',
    terminal_status = 'terminal_status',
    event_type = 'event_type',
    covariates = c('covariates1', 'covariates2', 'covariates3'),
    stratification_vars = c('stratification_vars1', 'stratification_vars2', 'stratification_vars3'),
    model_type = 'ag_model',
    time_scale = 'gap_time',
    frailty_distribution = 'gamma',
    robust_variance = TRUE,
    cluster_variable = 'cluster_variable',
    baseline_hazard_smooth = FALSE,
    smoothing_bandwidth = 1,
    confidence_level = 0.95,
    max_events = 10,
    terminal_competing = TRUE,
    recurrence_plots = TRUE,
    gap_time_plots = FALSE,
    cumulative_hazard = TRUE,
    model_diagnostics = TRUE,
    goodness_of_fit = TRUE,
    bootstrap_samples = 50,
    convergence_tolerance = 1e-04,
    max_iterations = 50,
    use_reReg = FALSE,
    reReg_model = 'cox_LWYY',
    reReg_se = 'bootstrap',
    reReg_B = 200,
    show_event_plot = TRUE,
    show_mcf_plot = TRUE,
    event_plot_subjects = 20
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'recurrentsurvival.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

