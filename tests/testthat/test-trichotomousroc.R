
test_that('trichotomousroc analysis works', {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()

  # Synthetic data generation (Distinct distributions)
  set.seed(123)
  n <- 50
  data <- data.frame(
    predictor = c(rnorm(n, 0), rnorm(n, 2), rnorm(n, 4)),
    outcome = factor(rep(c('Negative', 'Indeterminate', 'Positive'), each = n), 
                     levels = c('Negative', 'Indeterminate', 'Positive'))
  )

  # Run analysis
  model <- trichotomousroc(
      data = data,
      predictor = 'predictor',
      outcome = 'outcome',
      positive_level = 'Positive',
      indeterminate_level = 'Indeterminate',
      negative_level = 'Negative',
      threshold_method = 'youden',
      calculate_vus = TRUE,
      confusion_matrix_3x3 = TRUE,
      stratify_by = NULL
    )

  # Verify Results directly
  # Check VUS Table
  vus_df <- model$vusTable$asDF
  print("VUS Table:")
  print(vus_df)
  
  expect_true(nrow(vus_df) > 0)
  expect_true(!is.na(vus_df$vus[1]))
  expect_true(vus_df$vus[1] > 0.167) # Should be better than random
  
  # Check Thresholds Table
  thresh_df <- model$thresholdsTable$asDF
  print("Thresholds Table:")
  print(thresh_df)
  
  expect_true(nrow(thresh_df) == 2)
  expect_true(thresh_df$value[2] > thresh_df$value[1]) # T2 > T1
  
  # Check Confusion Matrix
  cm_df <- model$confusionMatrix3x3$asDF
  print("Confusion Matrix:")
  print(cm_df)
  expect_true(nrow(cm_df) == 3)
  
  # Export OMV (Optional, can fail)

})

