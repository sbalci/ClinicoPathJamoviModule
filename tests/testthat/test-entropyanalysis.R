
test_that('entropyanalysis analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    probability_vars1 = runif(n, 1, 100),
    probability_vars2 = runif(n, 1, 100),
    probability_vars3 = runif(n, 1, 100),
    predictor_var = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- entropyanalysis(
      data = data,
    outcome = 'outcome',
    probability_vars = c('probability_vars1', 'probability_vars2', 'probability_vars3'),
    predictor_var = 'predictor_var',
    calculate_entropy = TRUE,
    calculate_conditional_entropy = TRUE,
    calculate_mutual_information = TRUE,
    calculate_kl_divergence = FALSE,
    uncertainty_threshold = 0.5,
    normalize_entropy = TRUE,
    binning_method = 'equal_width',
    n_bins = 10,
    show_case_level = FALSE,
    flag_uncertain = TRUE,
    plot_entropy_distribution = TRUE,
    plot_uncertainty_by_class = TRUE,
    plot_mi_heatmap = FALSE,
    random_seed = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'entropyanalysis.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

