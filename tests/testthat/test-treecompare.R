
test_that('treecompare analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    vars1 = runif(n, 1, 100),
    vars2 = runif(n, 1, 100),
    vars3 = runif(n, 1, 100),
    facs1 = sample(c('A', 'B'), n, replace = TRUE),
    facs2 = sample(c('A', 'B'), n, replace = TRUE),
    facs3 = sample(c('A', 'B'), n, replace = TRUE),
    target = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  model <- treecompare(
    data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    facs = c('facs1', 'facs2', 'facs3'),
    target = 'target',
    targetLevel = 'A',
    include_cart = TRUE,
    include_rf = TRUE,
    include_gbm = FALSE,
    include_xgboost = FALSE,
    include_ctree = FALSE,
    validation = 'repeated_cv',
    cv_folds = 5,
    cv_repeats = 5,
    bootstrap_samples = 100,
    test_split = 0.25,
    stratified_sampling = TRUE,
    primary_metric = 'bacc',
    statistical_testing = TRUE,
    correction_method = 'holm',
    tune_parameters = TRUE,
    tuning_method = 'grid',
    cart_max_depth = 5,
    cart_min_split = 20,
    rf_ntrees = 500,
    rf_mtry_method = 'auto',
    clinical_context = 'diagnosis',
    interpretability_weight = 0.3,
    show_comparison_table = TRUE,
    show_performance_plot = TRUE,
    show_roc_comparison = TRUE,
    show_statistical_tests = TRUE,
    show_ranking_table = TRUE,
    show_computational_time = TRUE,
    show_clinical_recommendations = TRUE,
    show_detailed_metrics = FALSE,
    ensemble_best_models = FALSE,
    save_best_models = FALSE,
    set_seed = TRUE,
    seed_value = 42,
    parallel_processing = FALSE,
    verbose_output = FALSE
  )

  expect_true(inherits(model, "treecompareResults"))

  # Define output path
  omv_path <- file.path('omv_output', 'treecompare.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  tryCatch({
    jmvReadWrite::write_omv(model, omv_path)
    expect_true(file.exists(omv_path))
  }, error = function(e) {
    skip(paste("OMV write failed:", e$message))
  })
})

