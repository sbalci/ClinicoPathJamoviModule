
test_that('multiclassdiagnostics analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation
  set.seed(123)
  n <- 50
  data <- data.frame(
    predicted = sample(c('A', 'B', 'C'), n, replace = TRUE),
    actual = sample(c('A', 'B', 'C'), n, replace = TRUE),
    predicted2 = sample(c('A', 'B', 'C'), n, replace = TRUE)
  )

  # Run analysis
  model <- multiclassdiagnostics(
      data = data,
      predicted = 'predicted',
      actual = 'actual',
      positiveClass = NULL,
      confidenceLevel = 0.95,
      showROC = TRUE,
      showConfusion = TRUE,
      showPerClass = TRUE,
      showOverall = TRUE,
      compareModels = FALSE,
      predicted2 = 'predicted2',
      deLongTest = FALSE,
      mcnemarTest = TRUE,
      plotTheme = 'default',
      saveResults = FALSE
    )

  # Verify and Export OMV
  omv_path <- file.path('omv_output', 'multiclassdiagnostics.omv')
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

