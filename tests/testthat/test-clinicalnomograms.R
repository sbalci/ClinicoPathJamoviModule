
test_that('clinicalnomograms survival analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation for survival
  set.seed(123)
  n <- 100 # Increased sample size
  data <- data.frame(
    time_var = runif(n, 1, 100),
    status_var = sample(c(0, 1), n, replace = TRUE),
    outcome_var = sample(c(0, 1), n, replace = TRUE),
    covariates1 = runif(n),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE),
    covariates3 = sample(c('Low', 'High'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- clinicalnomograms(
      data = data,
      time_var = 'time_var',
      status_var = 'status_var',
      outcome_var = NULL, # Explicitly pass NULL
      covariates = c('covariates1', 'covariates2', 'covariates3'),
      nomogram_type = 'survival_nomogram',
      model_selection = 'all_variables',
      confidence_level = 0.95,
      validation_method = 'bootstrap',
      bootstrap_samples = 100,
      points_scale = 100,
      calibration_assessment = TRUE,
      discrimination_assessment = TRUE,
      decision_curve_analysis = TRUE,
      risk_groups = TRUE,
      clinical_scenarios = TRUE,
      model_equation = TRUE,
      confidence_intervals = TRUE,
      performance_metrics = TRUE,
      variable_importance = TRUE,
      missing_data_handling = 'complete_case'
    )
  })

  print(class(model))
  expect_true(inherits(model, 'R6'))
  expect_true(inherits(model, 'clinicalnomogramsResults'))
})

test_that('clinicalnomograms logistic analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation for logistic
  set.seed(123)
  n <- 100
  data <- data.frame(
    outcome_var = sample(c(0, 1), n, replace = TRUE),
    covariates1 = runif(n),
    covariates2 = sample(c('A', 'B'), n, replace = TRUE)
  )

  # Run analysis
  expect_no_error({
    model <- clinicalnomograms(
      data = data,
      time_var = NULL, # Explicitly pass NULL
      status_var = NULL, # Explicitly pass NULL
      outcome_var = 'outcome_var',
      covariates = c('covariates1', 'covariates2'),
      nomogram_type = 'logistic_nomogram',
      model_selection = 'all_variables',
      calibration_assessment = TRUE,
      discrimination_assessment = TRUE,
      decision_curve_analysis = TRUE,
      risk_groups = FALSE, # Risk groups mostly for survival?
      variable_importance = TRUE
    )
  })

  print(class(model))
  expect_true(inherits(model, 'R6'))
  expect_true(inherits(model, 'clinicalnomogramsResults'))
})
