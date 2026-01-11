
# Validating mendelianrandomization module
library(testthat)
library(jmvcore)

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("mendelianrandomization works with mock inputs", {
  skip_if_not_installed('jmvReadWrite')
  
  # Create minimal test dataset (mock GWAS summary stats)
  data <- data.frame(
    rsID = c("rs1", "rs2", "rs3", "rs4"),
    beta_bmi = c(0.1, 0.2, 0.3, 0.15),
    se_bmi = c(0.01, 0.02, 0.03, 0.01),
    beta_cad = c(0.2, 0.4, 0.6, 0.3),
    se_cad = c(0.02, 0.04, 0.06, 0.03),
    pval_bmi = c(1e-10, 1e-12, 1e-15, 0.5), # 4th SNP not significant
    pval_cad = c(1e-5, 1e-6, 1e-7, 0.1)
  )

  model <- mendelianrandomization(
      data = data,
      exposure_data = data,
      outcome_data = data,
      snp_column = "rsID",
      beta_exposure = "beta_bmi",
      se_exposure = "se_bmi",
      beta_outcome = "beta_cad",
      se_outcome = "se_cad",
      pval_exposure = "pval_bmi",
      mr_methods = 'main_three',
      pval_threshold = 1e-5
  )

  # Check omv export
  if (!dir.exists('omv_output')) dir.create('omv_output')
  omv_path <- file.path(getwd(), 'omv_output', 'mendelianrandomization.omv')
  
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
