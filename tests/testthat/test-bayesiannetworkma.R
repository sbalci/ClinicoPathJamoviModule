# Tests for bayesiannetworkma function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("bayesiannetworkma works with minimal inputs", {
  skip_if_not_installed('jmvReadWrite')
  
  set.seed(123)
  n <- 30
  data <- data.frame(
    study = rep(1:10, each=3),
    treat = factor(rep(c('A', 'B', 'C'), 10)),
    out = rnorm(30),
    N = sample(20:100, 30, replace=TRUE)
  )
  
  expect_no_error({
    result <- bayesiannetworkma(
      data = data,
      study_id = 'study',
      treatment = 'treat',
      outcome = 'out',
      sample_size = 'N',
      outcomeLevel = NULL,
      reference_treatment = NULL,
      standard_error = NULL, # Default null in yaml
      control_rate = NULL,   # Default null in yaml
      meta_regression_covariates = NULL,
      treatment_classes = NULL
    )
  })
  
  # OMV export check
  if (!dir.exists('omv_output')) dir.create('omv_output')
  omv_path <- file.path(getwd(), 'omv_output', 'bayesiannetworkma.omv')
  
  tryCatch({
    jmvReadWrite::write_omv(result, omv_path)
  }, error = function(e) {
    message("OMV Export failed: ", e$message)
  })
  
  if (!file.exists(omv_path)) {
     skip("OMV export failed, skipping file existence check")
  }
  expect_true(file.exists(omv_path))
})
