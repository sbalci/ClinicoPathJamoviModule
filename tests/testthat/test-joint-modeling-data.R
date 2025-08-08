# Tests for Joint Modeling Datasets
# These tests ensure the joint modeling datasets are properly formatted
# and suitable for analysis

library(testthat)
library(ClinicoPath)
library(dplyr)

# Helper function to test dataset structure
test_joint_dataset <- function(data, dataset_name, expected_vars, 
                              id_col, time_col, biomarker_col, 
                              survival_col, event_col) {
  
  test_that(paste(dataset_name, "has correct structure"), {
    
    # Data frame structure
    expect_s3_class(data, "data.frame")
    expect_gt(nrow(data), 0)
    expect_true(all(expected_vars %in% names(data)))
    
    # Required columns exist and have correct types
    expect_true(id_col %in% names(data))
    expect_true(time_col %in% names(data))
    expect_true(biomarker_col %in% names(data))
    expect_true(survival_col %in% names(data))
    expect_true(event_col %in% names(data))
    
    # Time variables are numeric
    expect_true(is.numeric(data[[time_col]]))
    expect_true(is.numeric(data[[survival_col]]))
    expect_true(is.numeric(data[[biomarker_col]]))
    
    # Event status is 0/1
    expect_true(all(data[[event_col]] %in% c(0, 1)))
    
    # No completely missing required variables
    expect_false(all(is.na(data[[id_col]])))
    expect_false(all(is.na(data[[time_col]])))  
    expect_false(all(is.na(data[[survival_col]])))
    expect_false(all(is.na(data[[event_col]])))
    
    # Time variables are non-negative
    expect_true(all(data[[time_col]] >= 0, na.rm = TRUE))
    expect_true(all(data[[survival_col]] >= 0, na.rm = TRUE))
    
    # Biomarker has some variation
    expect_gt(sd(data[[biomarker_col]], na.rm = TRUE), 0)
  })
  
  test_that(paste(dataset_name, "has adequate sample size"), {
    
    n_patients <- length(unique(data[[id_col]]))
    n_observations <- nrow(data)
    avg_visits <- n_observations / n_patients
    
    # Adequate sample size for joint modeling
    expect_gte(n_patients, 50) # At least 50 patients
    expect_gte(n_observations, 150) # At least 150 observations
    expect_gte(avg_visits, 2) # At least 2 visits per patient on average
    
    # Some events occurred
    surv_data <- data[!duplicated(data[[id_col]]), ]
    n_events <- sum(surv_data[[event_col]])
    event_rate <- mean(surv_data[[event_col]])
    
    expect_gte(n_events, 5) # At least 5 events
    expect_gte(event_rate, 0.01) # At least 1% event rate
  })
  
  test_that(paste(dataset_name, "has consistent survival data"), {
    
    # Each patient should have consistent survival time and status
    consistency_check <- data %>%
      group_by(!!sym(id_col)) %>%
      summarise(
        n_unique_surv_time = length(unique(!!sym(survival_col))),
        n_unique_status = length(unique(!!sym(event_col))),
        .groups = 'drop'
      )
    
    expect_true(all(consistency_check$n_unique_surv_time == 1))
    expect_true(all(consistency_check$n_unique_status == 1))
  })
  
  test_that(paste(dataset_name, "has reasonable time relationships"), {
    
    # Visit times should not exceed survival times
    time_check <- data %>%
      group_by(!!sym(id_col)) %>%
      summarise(
        max_visit_time = max(!!sym(time_col), na.rm = TRUE),
        survival_time = first(!!sym(survival_col)),
        .groups = 'drop'
      ) %>%
      mutate(time_consistent = max_visit_time <= survival_time)
    
    # Allow some small numerical differences
    inconsistent_patients <- sum(!time_check$time_consistent)
    total_patients <- nrow(time_check)
    
    expect_lt(inconsistent_patients / total_patients, 0.05) # Less than 5% inconsistent
  })
}

# Test PSA dataset
test_that("PSA joint dataset loads correctly", {
  expect_error(data(psa_joint_data), NA)
  expect_true(exists("psa_joint_data"))
})

test_joint_dataset(
  data = psa_joint_data,
  dataset_name = "PSA dataset", 
  expected_vars = c("patient_id", "age", "stage", "gleason_score", 
                   "visit_time", "psa_level", "survival_time", "death_status"),
  id_col = "patient_id",
  time_col = "visit_time", 
  biomarker_col = "psa_level",
  survival_col = "survival_time",
  event_col = "death_status"
)

# Test CD4 dataset  
test_that("CD4 joint dataset loads correctly", {
  expect_error(data(cd4_joint_data), NA)
  expect_true(exists("cd4_joint_data"))
})

test_joint_dataset(
  data = cd4_joint_data,
  dataset_name = "CD4 dataset",
  expected_vars = c("patient_id", "age", "baseline_viral_load", "art_adherence",
                   "visit_time", "cd4_count", "survival_time", "aids_death_status"),
  id_col = "patient_id",
  time_col = "visit_time",
  biomarker_col = "cd4_count", 
  survival_col = "survival_time",
  event_col = "aids_death_status"
)

# Test kidney dataset
test_that("Kidney joint dataset loads correctly", {
  expect_error(data(kidney_joint_data), NA)
  expect_true(exists("kidney_joint_data"))
})

test_joint_dataset(
  data = kidney_joint_data,
  dataset_name = "Kidney dataset",
  expected_vars = c("patient_id", "age", "diabetes", "hypertension", "baseline_proteinuria",
                   "visit_time", "egfr", "survival_time", "esrd_death_status"),
  id_col = "patient_id",
  time_col = "visit_time",
  biomarker_col = "egfr",
  survival_col = "survival_time", 
  event_col = "esrd_death_status"
)

# Test cardiac dataset
test_that("Cardiac joint dataset loads correctly", {
  expect_error(data(cardiac_joint_data), NA)
  expect_true(exists("cardiac_joint_data"))
})

test_joint_dataset(
  data = cardiac_joint_data,
  dataset_name = "Cardiac dataset",
  expected_vars = c("patient_id", "age", "nyha_class", "baseline_ef",
                   "visit_time", "nt_probnp", "survival_time", "hf_event_status"),
  id_col = "patient_id", 
  time_col = "visit_time",
  biomarker_col = "nt_probnp",
  survival_col = "survival_time",
  event_col = "hf_event_status"
)

# Test simple cancer dataset
test_that("Simple cancer joint dataset loads correctly", {
  expect_error(data(simple_cancer_data), NA)
  expect_true(exists("simple_cancer_data"))
})

test_joint_dataset(
  data = simple_cancer_data,
  dataset_name = "Simple cancer dataset",
  expected_vars = c("patient_id", "age", "treatment", 
                   "visit_time", "tumor_marker", "survival_time", "progression_status"),
  id_col = "patient_id",
  time_col = "visit_time", 
  biomarker_col = "tumor_marker",
  survival_col = "survival_time",
  event_col = "progression_status"
)

# Additional specific tests for dataset characteristics

test_that("PSA dataset has appropriate clinical characteristics", {
  data(psa_joint_data)
  
  # Age should be in reasonable range for prostate cancer
  expect_true(all(psa_joint_data$age >= 50 & psa_joint_data$age <= 90))
  
  # PSA levels should be positive
  expect_true(all(psa_joint_data$psa_level > 0, na.rm = TRUE))
  
  # Gleason scores should be in valid range
  expect_true(all(psa_joint_data$gleason_score >= 6 & psa_joint_data$gleason_score <= 10))
  
  # Tumor stages should be valid
  expect_true(all(psa_joint_data$stage %in% c("T1", "T2", "T3", "T4")))
})

test_that("CD4 dataset has appropriate clinical characteristics", {
  data(cd4_joint_data)
  
  # CD4 counts should be reasonable (can be very low in AIDS)
  expect_true(all(cd4_joint_data$cd4_count >= 0, na.rm = TRUE))
  expect_true(all(cd4_joint_data$cd4_count <= 2000, na.rm = TRUE)) # Upper physiological limit
  
  # Age should be reasonable for HIV population
  expect_true(all(cd4_joint_data$age >= 18 & cd4_joint_data$age <= 80))
  
  # Viral load should be positive
  expect_true(all(cd4_joint_data$baseline_viral_load > 0, na.rm = TRUE))
})

test_that("Simple cancer dataset is suitable for teaching", {
  data(simple_cancer_data)
  
  # Should be smaller dataset for faster computation
  n_patients <- length(unique(simple_cancer_data$patient_id))
  expect_lte(n_patients, 120) # Not too large
  expect_gte(n_patients, 80)  # Not too small
  
  # Should have clear treatment groups
  treatment_counts <- table(simple_cancer_data[!duplicated(simple_cancer_data$patient_id), "treatment"])
  expect_gte(min(treatment_counts), 30) # At least 30 per group
  
  # Tumor marker should be positive
  expect_true(all(simple_cancer_data$tumor_marker > 0, na.rm = TRUE))
})

# Test that datasets can be used together (e.g., for comparative examples)
test_that("All datasets have compatible structure for examples", {
  
  datasets <- list(
    psa = psa_joint_data,
    cd4 = cd4_joint_data, 
    kidney = kidney_joint_data,
    cardiac = cardiac_joint_data,
    simple = simple_cancer_data
  )
  
  # All should have the basic required columns
  required_pattern <- c("patient_id", "visit_time", "survival_time")
  
  for (name in names(datasets)) {
    dataset <- datasets[[name]]
    
    # Check basic ID and time columns exist
    expect_true("patient_id" %in% names(dataset) | 
                any(grepl("patient|id", names(dataset), ignore.case = TRUE)))
    expect_true("visit_time" %in% names(dataset) | 
                any(grepl("time|visit", names(dataset), ignore.case = TRUE)))
    expect_true("survival_time" %in% names(dataset) | 
                any(grepl("survival|followup", names(dataset), ignore.case = TRUE)))
    
    # Should have at least one biomarker column
    biomarker_cols <- setdiff(names(dataset), 
                             c("patient_id", "visit_time", "survival_time", "age"))
    expect_gte(length(biomarker_cols), 3) # At least biomarker, status, and one other
  }
})

# Performance test - datasets should load quickly
test_that("Datasets load with reasonable performance", {
  
  # Test loading time (should be under 1 second each)
  load_times <- c()
  
  datasets <- c("psa_joint_data", "cd4_joint_data", "kidney_joint_data", 
               "cardiac_joint_data", "simple_cancer_data")
  
  for (dataset in datasets) {
    start_time <- Sys.time()
    data(list = dataset, envir = environment())
    end_time <- Sys.time()
    load_time <- as.numeric(end_time - start_time, units = "secs")
    load_times <- c(load_times, load_time)
  }
  
  # All datasets should load quickly
  expect_true(all(load_times < 2.0)) # Less than 2 seconds each
  expect_lt(mean(load_times), 1.0)   # Average less than 1 second
})