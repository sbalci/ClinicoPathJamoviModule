
test_that('costeffectiveness analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    strategy = sample(c('A', 'B'), n, replace = TRUE),
    cost = runif(n, 1, 100),
    effectiveness = runif(n, 1, 100),
    subgroup_variable = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- costeffectiveness(
      data = data,
    strategy = 'strategy',
    cost = 'cost',
    effectiveness = 'effectiveness',
    effectiveness_type = 'binary',
    calculate_icer = TRUE,
    calculate_incremental = TRUE,
    dominance_analysis = FALSE,
    net_monetary_benefit = FALSE,
    wtp_threshold = 50000,
    multiple_wtp_thresholds = FALSE,
    confidence_intervals = FALSE,
    ci_method = 'bootstrap',
    bootstrap_samples = 100,
    confidence_level = 0.95,
    deterministic_sensitivity = FALSE,
    sensitivity_range_pct = 20,
    probabilistic_sensitivity = FALSE,
    psa_simulations = 1000,
    cost_distribution = 'gamma',
    effect_distribution = 'normal',
    voi_analysis = FALSE,
    evpi_population = 10000,
    evppi_parameters = FALSE,
    evppi_focus = 'both',
    subgroup_analysis = FALSE,
    subgroup_variable = 'subgroup_variable',
    time_horizon = 1,
    discount_costs = FALSE,
    discount_rate_costs = 3,
    discount_effects = FALSE,
    discount_rate_effects = 3,
    plot_ce_plane = TRUE,
    plot_ce_acceptability = FALSE,
    plot_nmb = FALSE,
    plot_tornado = FALSE,
    plot_incremental_frontier = FALSE,
    perspective = 'healthcare',
    include_indirect_costs = FALSE,
    cost_year = 2024,
    handling_missing = 'complete',
    random_seed = 12345
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'costeffectiveness.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

