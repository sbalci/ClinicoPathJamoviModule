
test_that('mendelianrandomization analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    snp_column = sample(c('A', 'B'), n, replace = TRUE),
    beta_exposure = runif(n, 1, 100),
    se_exposure = runif(n, 1, 100),
    pval_exposure = runif(n, 1, 100),
    beta_outcome = runif(n, 1, 100),
    se_outcome = runif(n, 1, 100),
    pval_outcome = runif(n, 1, 100),
    eaf_column = runif(n, 1, 100)
  )

  # Run analysis
  expect_no_error({
    model <- mendelianrandomization(
      data = data,
    use_same_dataset = TRUE,
    snp_column = 'snp_column',
    beta_exposure = 'beta_exposure',
    se_exposure = 'se_exposure',
    pval_exposure = 'pval_exposure',
    beta_outcome = 'beta_outcome',
    se_outcome = 'se_outcome',
    pval_outcome = 'pval_outcome',
    eaf_column = 'eaf_column',
    pval_threshold = 5e-08,
    clump_r2 = 0.001,
    clump_kb = 10000,
    mr_methods = 'main_three',
    ivw_model = 'random',
    egger_bootstrap = FALSE,
    bootstrap_samples = 100,
    heterogeneity_test = TRUE,
    pleiotropy_test = TRUE,
    leave_one_out = TRUE,
    single_snp_analysis = FALSE,
    mr_presso = FALSE,
    presso_threshold = 0.05,
    plot_forest = TRUE,
    plot_funnel = TRUE,
    plot_scatter = TRUE,
    plot_loo = TRUE,
    harmonize_alleles = TRUE,
    remove_palindromic = TRUE,
    steiger_filtering = FALSE,
    min_snps = 3,
    conf_level = 0.95,
    random_seed = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'mendelianrandomization.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

