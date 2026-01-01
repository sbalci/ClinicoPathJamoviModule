
test_that('gradientboosting analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    time = runif(n, 1, 100),
    event = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = sample(c('A', 'B'), n, replace = TRUE),
    predictors2 = sample(c('A', 'B'), n, replace = TRUE),
    predictors3 = sample(c('A', 'B'), n, replace = TRUE),
    strata = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- gradientboosting(
      data = data,
    time = 'time',
    event = 'event',
    predictors = c('predictors1', 'predictors2', 'predictors3'),
    strata = 'strata',
    algorithm = 'mboost',
    n_trees = 100,
    learning_rate = 0.1,
    max_depth = 3,
    min_node_size = 10,
    subsample = 1,
    cv_folds = 5,
    early_stopping = TRUE,
    patience = 10,
    reg_alpha = 0,
    reg_lambda = 1,
    variable_selection = TRUE,
    importance_threshold = 0.01,
    show_convergence = TRUE,
    show_importance = TRUE,
    show_predictions = FALSE,
    plot_convergence = TRUE,
    plot_importance = TRUE,
    plot_partial = FALSE,
    plot_survival = FALSE,
    interaction_depth = 1,
    bag_fraction = 0.5,
    random_seed = 123
    )
  })

  # Verify and Export OMV
  expect_true(is.list(model))
  expect_true(inherits(model, 'jmvcoreClass'))

  # Define output path
  omv_path <- file.path('omv_output', 'gradientboosting.omv')
  if (!dir.exists('omv_output')) dir.create('omv_output')

  # Attempt to write OMV
  expect_no_error({
    jmvReadWrite::write_omv(model, omv_path)
  })

  expect_true(file.exists(omv_path))
})

