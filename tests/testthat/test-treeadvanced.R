
test_that('treeadvanced analysis works', {
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
    target = sample(c('A', 'B'), n, replace = TRUE),
    train = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- treeadvanced(
      data = data,
    vars = c('vars1', 'vars2', 'vars3'),
    facs = c('facs1', 'facs2', 'facs3'),
    target = 'target',
    train = 'train',
    validation = 'repeated_cv',
    cv_folds = 5,
    cv_repeats = 3,
    bootstrap_samples = 100,
    stratified_sampling = TRUE,
    test_split = 0.25,
    hyperparameter_tuning = FALSE,
    tuning_method = 'grid',
    tuning_metric = 'bacc',
    pruning_method = 'one_se',
    custom_cp = 0.01,
    auto_prune = TRUE,
    show_cp_analysis = FALSE,
    splitting_criterion = 'gini',
    surrogate_splits = TRUE,
    max_surrogate = 5,
    competing_splits = 4,
    cost_sensitive = FALSE,
    clinical_loss_preset = 'equal',
    fn_fp_cost_ratio = 2,
    prevalence_adjustment = FALSE,
    population_prevalence = 10,
    feature_selection = FALSE,
    feature_selection_method = 'rfe',
    max_features = 10,
    show_tree_plot = TRUE,
    show_performance_metrics = TRUE,
    show_confusion_matrix = TRUE,
    show_importance_plot = TRUE,
    show_validation_curves = FALSE,
    show_calibration_plot = FALSE,
    show_roc_curve = TRUE,
    bootstrap_confidence = FALSE,
    n_bootstrap = 500,
    clinical_context = 'diagnosis',
    show_clinical_interpretation = TRUE,
    clinical_importance_interpretation = TRUE,
    mdg_threshold = 1,
    show_feature_contributions = FALSE,
    survival_integration = FALSE,
    set_seed = TRUE,
    seed_value = 42
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'treeadvanced.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

