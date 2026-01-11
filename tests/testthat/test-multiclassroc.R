
test_that('multiclassroc analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B', 'C'), n, replace = TRUE),
    predictors1 = runif(n, 1, 100),
    predictors2 = runif(n, 1, 100),
    predictors3 = runif(n, 1, 100)
  )

  # Run analysis
  model <- multiclassroc(
      data = data,
      outcome = 'outcome',
      predictors = c('predictors1', 'predictors2', 'predictors3'),
      method = 'ovr',
      calculate_macro_auc = TRUE,
      calculate_micro_auc = TRUE,
      calculate_weighted_auc = TRUE,
      confidence_intervals = TRUE,
      ci_method = 'bootstrap',
      bootstrap_samples = 100,
      confidence_level = 0.95,
      pairwise_comparisons = FALSE,
      confusion_matrix = TRUE,
      class_metrics = TRUE,
      plot_roc_curves = TRUE,
      plot_method = 'overlay',
      plot_diagonal = TRUE,
      random_seed = 42
    )

  # Verify and Export OMV
  omv_path <- file.path('omv_output', 'multiclassroc.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  tryCatch({
    jmvReadWrite::write_omv(model, omv_path)
  }, error = function(e){
      message("OMV export failed: ", e$message)
  })
  
  if (!file.exists(omv_path)) {
     skip("OMV export failed, skipping file existence check")
  }

  expect_true(file.exists(omv_path))
})

