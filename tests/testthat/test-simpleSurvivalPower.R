
test_that('simpleSurvivalPower analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(

  )

  # Run analysis
  expect_no_error({
    model <- simpleSurvivalPower(
      data = data,
    clinical_preset = 'custom',
    analysis_type = 'sample_size',
    test_type = 'log_rank',
    study_design = 'two_arm_parallel',
    primary_endpoint = 'overall_survival',
    effect_size_type = 'hazard_ratio',
    effect_size = 0.75,
    alpha_level = 0.05,
    power_level = 0.8,
    allocation_ratio = 1,
    sample_size_input = 200,
    control_median_survival = 12,
    survival_distribution = 'exponential',
    weibull_shape = 1,
    accrual_period = 24,
    follow_up_period = 12,
    accrual_pattern = 'uniform',
    dropout_rate = 0.05,
    ni_margin = 1.25,
    ni_type = 'relative_margin',
    competing_risk_rate = 0.1,
    competing_risk_hr = 1,
    rmst_tau = 36,
    rmst_difference = 3,
    snp_maf = 0.3,
    genetic_model = 'additive',
    number_of_arms = 3,
    multiple_comparisons = 'dunnett',
    interim_analyses = 0,
    alpha_spending = 'none',
    stratification_factors = 0,
    cluster_size = 50,
    icc = 0.05,
    sensitivity_analysis = FALSE,
    simulation_runs = 10000
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'simpleSurvivalPower.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

