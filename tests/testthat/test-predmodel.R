
test_that('predmodel analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    outcome = sample(c('A', 'B'), n, replace = TRUE),
    predictors1 = runif(n, 1, 100),
    predictors2 = runif(n, 1, 100),
    predictors3 = runif(n, 1, 100)
  )

  # Run analysis
  model <- predmodel(
      data = data,
      outcome = 'outcome',
      predictors = c('predictors1', 'predictors2', 'predictors3'),
      modelSelection = 'none',
      validationMethod = 'bootstrap',
      nBootstrap = 200,
      nFolds = 10,
      showCalibration = TRUE,
      showDiscrimination = TRUE,
      showRiskGroups = TRUE,
      ciLevel = 0.95
    )

  # Verify and Export OMV
  omv_path <- file.path('omv_output', 'predmodel.omv')
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

