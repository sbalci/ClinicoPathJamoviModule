context("Time-Dependent ROC Analysis - Basic Tests")

library(testthat)

# Skip tests if timeROC not available
skip_if_not_installed("timeROC")

# Create simple test data
create_test_data <- function(n = 100) {
  set.seed(12345)
  
  # Generate simple survival data
  biomarker <- rnorm(n, 5, 2)
  hazard <- exp(-2 + 0.3 * scale(biomarker)[,1])
  time <- round(rexp(n, hazard), 1)
  event <- rbinom(n, 1, 0.6)
  
  # Ensure some variation in survival times
  time <- pmax(1, pmin(time, 50))
  
  data.frame(
    time = time,
    event = event,
    biomarker = biomarker
  )
}

test_that("basic timeROC package functionality works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Test direct timeROC functionality first
  test_data <- create_test_data()
  
  # Basic timeROC call
  roc_result <- timeROC::timeROC(
    T = test_data$time,
    delta = test_data$event,
    marker = test_data$biomarker,
    cause = 1,
    times = c(10, 20),
    ROC = TRUE
  )
  
  # Check basic structure
  expect_true(!is.null(roc_result))
  expect_true("AUC" %in% names(roc_result))
  expect_true(length(roc_result$AUC) == 2)  # 2 timepoints
  expect_true(all(roc_result$AUC >= 0 & roc_result$AUC <= 1))
})

test_that("test datasets were created successfully", {
  
  # Check if test datasets exist
  cancer_data_path <- file.path("..", "..", "data", "timeroc_cancer_biomarker.rda")
  expect_true(file.exists(cancer_data_path))
  
  # Load and check structure
  load(cancer_data_path)
  expect_true(exists("timeroc_cancer_biomarker"))
  expect_true(is.data.frame(timeroc_cancer_biomarker))
  expect_true(nrow(timeroc_cancer_biomarker) == 300)
  
  # Check required columns exist
  required_cols <- c("follow_up_months", "death_event", "tumor_biomarker")
  expect_true(all(required_cols %in% names(timeroc_cancer_biomarker)))
  
  # Check data types are reasonable
  expect_true(is.numeric(timeroc_cancer_biomarker$follow_up_months))
  expect_true(is.numeric(timeroc_cancer_biomarker$death_event))
  expect_true(is.numeric(timeroc_cancer_biomarker$tumor_biomarker))
  
  # Check for reasonable values
  expect_true(all(timeroc_cancer_biomarker$death_event %in% c(0, 1)))
  expect_true(all(timeroc_cancer_biomarker$follow_up_months > 0))
  expect_true(all(timeroc_cancer_biomarker$tumor_biomarker > 0))
})

test_that("timeroc datasets have expected characteristics", {
  
  # Load cardiovascular data
  cv_data_path <- file.path("..", "..", "data", "timeroc_cardiovascular_risk.rda")
  if (file.exists(cv_data_path)) {
    load(cv_data_path)
    expect_true(nrow(timeroc_cardiovascular_risk) == 400)
    expect_true("troponin_level" %in% names(timeroc_cardiovascular_risk))
  }
  
  # Load multi-biomarker data
  multi_data_path <- file.path("..", "..", "data", "timeroc_multi_biomarker.rda")
  if (file.exists(multi_data_path)) {
    load(multi_data_path)
    expect_true(nrow(timeroc_multi_biomarker) == 250)
    
    # Should have three biomarkers with different characteristics
    expect_true(all(c("biomarker_alpha", "biomarker_beta", "biomarker_gamma") %in% 
                   names(timeroc_multi_biomarker)))
  }
})

test_that("datasets summary files were created", {
  
  # Check summary files exist
  summary_path <- file.path("..", "..", "data", "timeroc_datasets_summary.rda")
  scenarios_path <- file.path("..", "..", "data", "timeroc_test_scenarios.rda")
  
  expect_true(file.exists(summary_path))
  expect_true(file.exists(scenarios_path))
  
  # Load and check summary
  if (file.exists(summary_path)) {
    load(summary_path)
    expect_true(exists("timeroc_datasets_summary"))
    expect_true(is.data.frame(timeroc_datasets_summary))
    expect_true(nrow(timeroc_datasets_summary) == 6)  # 6 datasets
  }
})
