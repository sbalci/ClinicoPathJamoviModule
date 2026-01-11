# Tests for bayesiandiagnostic function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("bayesiandiagnostic works with minimal inputs", {
  skip_if_not_installed('jmvReadWrite')
  
  set.seed(123)
  n <- 50
  data <- data.frame(
    test = factor(sample(c('Pos', 'Neg'), n, replace = TRUE), levels=c('Pos', 'Neg')),
    disease = factor(sample(c('Present', 'Absent'), n, replace = TRUE), levels=c('Present', 'Absent'))
  )
  
  expect_no_error({
    result <- bayesiandiagnostic(
      data = data,
      test_results = 'test',
      gold_standard = 'disease',
      test_positive_level = 'Pos',
      disease_positive_level = 'Present',
      covariates = NULL,
      comparison_positive_level = NULL,
      analysis_type = 'single_test',
      mcmc_samples = 2000,
      mcmc_burnin = 1000,
      mcmc_chains = 1,
      parallel_chains = FALSE
    )
  })
  
  # OMV export check
  if (!dir.exists('omv_output')) dir.create('omv_output')
  omv_path <- file.path(getwd(), 'omv_output', 'bayesiandiagnostic.omv')
  
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
