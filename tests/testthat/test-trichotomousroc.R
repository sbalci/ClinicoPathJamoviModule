
test_that('trichotomousroc analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    predictor = runif(n, 1, 100),
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    stratify_by = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- trichotomousroc(
      data = data,
    predictor = 'predictor',
    outcome = 'outcome',
    threshold_method = 'youden',
    lower_threshold = 0.33,
    upper_threshold = 0.67,
    cost_fn = 1,
    cost_fp = 1,
    cost_indeterminate = 0.5,
    confidence_level = 0.95,
    bootstrap_samples = 100,
    bootstrap_method = 'bca',
    calculate_vus = TRUE,
    category_sensitivities = TRUE,
    pairwise_comparisons = TRUE,
    confusion_matrix_3x3 = TRUE,
    plot_3d_surface = TRUE,
    plot_threshold_analysis = TRUE,
    plot_category_distributions = TRUE,
    plot_pairwise_rocs = FALSE,
    clinical_context = 'general',
    show_clinical_interpretation = TRUE,
    indeterminate_action = 'test',
    stratified_analysis = FALSE,
    stratify_by = 'stratify_by',
    missing_handling = 'complete',
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'trichotomousroc.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

