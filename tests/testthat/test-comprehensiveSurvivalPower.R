
test_that('comprehensiveSurvivalPower analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(

  )

  # Run analysis
  expect_no_error({
    model <- comprehensiveSurvivalPower(
      data = data,
    method_category = 'standard',
    calculation_type = 'sample_size',
    statistical_method = 'log_rank_schoenfeld',
    study_design = 'simple',
    alpha = 0.05,
    power = 0.8,
    sample_size = 200,
    allocation_ratio = 1,
    hazard_ratio = 0.75,
    median_survival_control = 12,
    median_survival_treatment = 16,
    accrual_period = 24,
    follow_up_period = 36,
    dropout_rate = 0.05,
    event_rate = 0.6,
    competing_event_rate = 0.2,
    cumulative_incidence_control = 0.4,
    cumulative_incidence_treatment = 0.3,
    rmst_timepoint = 24,
    non_inferiority_margin = 1.25,
    superiority_test = FALSE,
    snp_maf = 0.2,
    genetic_model = 'additive',
    number_of_looks = 1,
    spending_function = 'lan_demets_obf',
    show_detailed_output = TRUE,
    show_sensitivity_analysis = FALSE,
    show_power_curves = FALSE,
    export_study_design = FALSE,
    genetic_effect_size = 1.5,
    cure_rate_control = 0,
    cure_rate_treatment = 0,
    survival_distribution = 'exponential',
    accrual_distribution = 'uniform',
    interim_futility_boundary = 0.2,
    binding_futility = FALSE,
    number_of_covariates = 2,
    covariate_correlation = 0,
    interaction_effect_size = 1,
    covariate_distribution = 'normal',
    adjust_for_confounders = TRUE,
    show_method_comparison = FALSE
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'comprehensiveSurvivalPower.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

