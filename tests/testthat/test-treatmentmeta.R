
library(testthat)

test_that("treatmentmeta basic functionality works", {
  # Load package
  devtools::load_all()
  
  # Create synthetic meta-analysis data (continuous)
  set.seed(123)
  n_studies <- 10
  df <- data.frame(
    study = paste("Study", 1:n_studies),
    year = 2010:2019,
    mean_e = rnorm(n_studies, mean = 5, sd = 1),
    sd_e = runif(n_studies, 0.5, 1.5),
    n_e = sample(30:100, n_studies),
    mean_c = rnorm(n_studies, mean = 4, sd = 1),
    sd_c = runif(n_studies, 0.5, 1.5),
    n_c = sample(30:100, n_studies),
    subgroup = sample(c("Group A", "Group B"), n_studies, replace = TRUE)
  )
  
  # Run meta-analysis
  res <- treatmentmeta(
    data = df,
    study_id = "study",
    year = "year",
    outcome_type = "continuous",
    mean_treatment = "mean_e",
    sd_treatment = "sd_e",
    n_treatment = "n_e",
    mean_control = "mean_c",
    sd_control = "sd_c",
    n_control = "n_c",
    effect_measure = "MD",
    model_type = "random"
  )
  
  # Check if results object is created
  expect_true(!is.null(res))
  
  # Check if pooled effects table is populated
  pooled_table <- res$pooled_effects$asDF
  expect_true(nrow(pooled_table) >= 1)
  
  # Check if individual studies table is populated
  studies_table <- res$individual_studies$asDF
  expect_equal(nrow(studies_table), n_studies)
  
  # Check if forest plot exists
  expect_true(!is.null(res$forest_plot))
})

test_that("treatmentmeta binary outcomes work", {
  devtools::load_all()
  
  # Create synthetic meta-analysis data (binary)
  set.seed(456)
  n_studies <- 8
  df <- data.frame(
    study = paste("Study", 1:n_studies),
    events_e = rbinom(n_studies, 50, 0.2),
    n_e = rep(50, n_studies),
    events_c = rbinom(n_studies, 50, 0.1),
    n_c = rep(50, n_studies)
  )
  
  # Run meta-analysis
  res <- treatmentmeta(
    data = df,
    study_id = "study",
    outcome_type = "binary",
    events_treatment = "events_e",
    n_treatment = "n_e",
    events_control = "events_c",
    n_control = "n_c",
    effect_measure = "RR",
    model_type = "random"
  )
  
  # Check if results object is created
  expect_true(!is.null(res))
  
  # Check if pooled effects table is populated
  pooled_table <- res$pooled_effects$asDF
  expect_true(nrow(pooled_table) >= 1)
})
test_that("treatmentmeta correlation outcomes work", {
  devtools::load_all()
  
  # Create synthetic correlation data
  set.seed(789)
  n_studies <- 6
  df <- data.frame(
    study = paste("Study", 1:n_studies),
    cor = runif(n_studies, 0.3, 0.7),
    sample_size = sample(20:100, n_studies)
  )
  
  # Run meta-analysis
  res <- treatmentmeta(
    data = df,
    study_id = "study",
    outcome_type = "correlation",
    correlation = "cor",
    sample_size = "sample_size",
    effect_measure = "COR",
    model_type = "random"
  )
  
  expect_true(!is.null(res))
  expect_true(nrow(res$pooled_effects$asDF) >= 1)
  expect_true(nrow(res$data_summary$asDF[res$data_summary$asDF$metric == "Total observations", ]) >= 1)
})

test_that("treatmentmeta advanced features work", {
  devtools::load_all()
  
  # Comprehensive test data
  set.seed(111)
  n_studies <- 15
  df <- data.frame(
    study = paste("Study", 1:n_studies),
    mean_e = rnorm(n_studies, 5, 1),
    sd_e = runif(n_studies, 1, 2),
    n_e = rep(50, n_studies),
    mean_c = rnorm(n_studies, 4, 1),
    sd_c = runif(n_studies, 1, 2),
    n_c = rep(50, n_studies),
    group = sample(c("X", "Y"), n_studies, replace = TRUE),
    moderator1 = rnorm(n_studies)
  )
  
  # Run meta-analysis with all features enabled
  res <- treatmentmeta(
    data = df,
    study_id = "study",
    outcome_type = "continuous",
    mean_treatment = "mean_e",
    sd_treatment = "sd_e",
    n_treatment = "n_e",
    mean_control = "mean_c",
    sd_control = "sd_c",
    n_control = "n_c",
    subgroup_var = "group",
    moderator_vars = "moderator1",
    sensitivity_analysis = TRUE,
    publication_bias = TRUE,
    trim_fill = TRUE,
    influence_diagnostics = TRUE,
    heterogeneity_test = TRUE
  )
  
  expect_true(!is.null(res))
  
  # Check subgroups
  expect_true(nrow(res$subgroup_analysis$asDF) >= 2)
  expect_true(nrow(res$subgroup_test$asDF) >= 1)
  
  # Check regression
  expect_true(nrow(res$meta_regression$asDF) >= 1)
  
  # Check sensitivity
  expect_true(nrow(res$sensitivity$asDF) == n_studies)
  
  # Check publication bias
  expect_true(nrow(res$publication_bias_tests$asDF) >= 1)
  
  # Check trim and fill
  expect_true(nrow(res$trim_fill_results$asDF) >= 2)
  
  # Check influence
  expect_true(nrow(res$influence$asDF) == n_studies)
})
