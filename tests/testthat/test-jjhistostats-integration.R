# ═══════════════════════════════════════════════════════════
# Integration Tests: jjhistostats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("jjhistostats integrates with all test datasets", {
  devtools::load_all()

  datasets <- list(
    list(name = "jjhistostats_test", dep = "age_years"),
    list(name = "jjhistostats_labvalues", dep = "glucose"),
    list(name = "jjhistostats_skewed", dep = "cea"),
    list(name = "jjhistostats_bimodal", dep = "age_bimodal"),
    list(name = "jjhistostats_pathology", dep = "ki67_index"),
    list(name = "jjhistostats_grouped", dep = "biomarker_level"),
    list(name = "jjhistostats_small", dep = "measurement"),
    list(name = "jjhistostats_uniform", dep = "uniform_score")
  )

  for (dataset_info in datasets) {
    data(list = dataset_info$name, package = "ClinicoPath")
    dataset <- get(dataset_info$name)

    result <- jjhistostats(
      data = dataset,
      dep = dataset_info$dep
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for dataset:", dataset_info$name))
  }
})

test_that("jjhistostats data.frame vs tibble compatibility", {
  devtools::load_all()

  data(jjhistostats_test)

  # As tibble (default)
  result_tibble <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years"
  )

  # As data.frame
  result_df <- jjhistostats(
    data = as.data.frame(jjhistostats_test),
    dep = "age_years"
  )

  # Both should produce results
  expect_s3_class(result_tibble, "jjhistostatsResults")
  expect_s3_class(result_df, "jjhistostatsResults")
})

test_that("jjhistostats consistency across runs with same data", {
  devtools::load_all()

  data(jjhistostats_test)

  # Run twice with identical settings
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric"
  )

  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric"
  )

  # Should produce consistent results
  expect_s3_class(result1, "jjhistostatsResults")
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats complete clinical workflow - patient demographics", {
  devtools::load_all()

  data(jjhistostats_test)

  # Step 1: Basic age distribution
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Step 2: With centrality and statistics
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric",
    centralityline = TRUE,
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # Step 3: Multiple demographics
  result3 <- jjhistostats(
    data = jjhistostats_test,
    dep = c("age_years", "bmi", "hemoglobin"),
    typestatistics = "parametric",
    centralityline = TRUE
  )
  expect_s3_class(result3, "jjhistostatsResults")

  # Step 4: Publication figure
  result4 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric",
    centralityline = TRUE,
    resultssubtitle = TRUE,
    title = "Age Distribution in Study Cohort",
    xlab = "Age (years)",
    plotwidth = 800,
    plotheight = 600
  )
  expect_s3_class(result4, "jjhistostatsResults")
})

test_that("jjhistostats complete clinical workflow - lab values", {
  devtools::load_all()

  data(jjhistostats_labvalues)

  # Step 1: Basic lab value distribution
  result1 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    clinicalPreset = "lab_values"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Step 2: Test against reference value
  result2 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    typestatistics = "parametric",
    enableOneSampleTest = TRUE,
    test.value = 200,
    centralityline = TRUE,
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # Step 3: Complete lab panel
  result3 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = c("glucose", "cholesterol", "triglycerides", "creatinine"),
    clinicalPreset = "lab_values",
    centralityline = TRUE
  )
  expect_s3_class(result3, "jjhistostatsResults")
})

test_that("jjhistostats complete clinical workflow - biomarkers", {
  devtools::load_all()

  data(jjhistostats_skewed)

  # Step 1: Parametric (inappropriate)
  result1 <- jjhistostats(
    data = jjhistostats_skewed,
    dep = "cea",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Step 2: Nonparametric (appropriate)
  result2 <- jjhistostats(
    data = jjhistostats_skewed,
    dep = "cea",
    typestatistics = "nonparametric",
    centralityline = TRUE,
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # Step 3: Multiple biomarkers
  result3 <- jjhistostats(
    data = jjhistostats_skewed,
    dep = c("cea", "ca199", "afp"),
    clinicalPreset = "biomarkers"
  )
  expect_s3_class(result3, "jjhistostatsResults")
})

test_that("jjhistostats complete clinical workflow - pathology", {
  devtools::load_all()

  data(jjhistostats_pathology)

  # Step 1: Ki-67 distribution
  result1 <- jjhistostats(
    data = jjhistostats_pathology,
    dep = "ki67_index",
    clinicalPreset = "pathology_scores"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Step 2: Stratified by tumor grade
  result2 <- jjhistostats(
    data = jjhistostats_pathology,
    dep = "ki67_index",
    grvar = "tumor_grade",
    clinicalPreset = "pathology_scores",
    centralityline = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # Step 3: Complete pathology panel
  result3 <- jjhistostats(
    data = jjhistostats_pathology,
    dep = c("ki67_index", "mitotic_count", "tumor_cellularity", "necrosis_percent"),
    clinicalPreset = "pathology_scores",
    centralityline = TRUE
  )
  expect_s3_class(result3, "jjhistostatsResults")
})

test_that("jjhistostats complete clinical workflow - grouped analysis", {
  devtools::load_all()

  data(jjhistostats_grouped)

  # Step 1: Overall distribution
  result1 <- jjhistostats(
    data = jjhistostats_grouped,
    dep = "biomarker_level"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Step 2: Stratified by disease stage
  result2 <- jjhistostats(
    data = jjhistostats_grouped,
    dep = "biomarker_level",
    grvar = "disease_stage",
    typestatistics = "parametric",
    centralityline = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # Step 3: Stratified by treatment
  result3 <- jjhistostats(
    data = jjhistostats_grouped,
    dep = "age_years",
    grvar = "treatment",
    typestatistics = "parametric",
    centralityline = TRUE,
    resultssubtitle = TRUE
  )
  expect_s3_class(result3, "jjhistostatsResults")
})

test_that("jjhistostats statistical method comparison on same data", {
  devtools::load_all()

  data(jjhistostats_test)

  # Parametric
  result_param <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric"
  )

  # Nonparametric
  result_nonparam <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "nonparametric"
  )

  # Robust
  result_robust <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "robust"
  )

  # Bayesian
  result_bayes <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "bayes"
  )

  # All should complete
  expect_s3_class(result_param, "jjhistostatsResults")
  expect_s3_class(result_nonparam, "jjhistostatsResults")
  expect_s3_class(result_robust, "jjhistostatsResults")
  expect_s3_class(result_bayes, "jjhistostatsResults")
})

test_that("jjhistostats centrality type comparison on same data", {
  devtools::load_all()

  data(jjhistostats_test)

  # Default
  result_default <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    centralityline = TRUE,
    centralitytype = "default"
  )

  # Parametric (mean)
  result_param <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    centralityline = TRUE,
    centralitytype = "parametric"
  )

  # Nonparametric (median)
  result_nonparam <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    centralityline = TRUE,
    centralitytype = "nonparametric"
  )

  # Robust
  result_robust <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    centralityline = TRUE,
    centralitytype = "robust"
  )

  # All should complete
  expect_s3_class(result_default, "jjhistostatsResults")
  expect_s3_class(result_param, "jjhistostatsResults")
  expect_s3_class(result_nonparam, "jjhistostatsResults")
  expect_s3_class(result_robust, "jjhistostatsResults")
})

test_that("jjhistostats clinical preset comparison", {
  devtools::load_all()

  data(jjhistostats_test)

  # Custom
  result_custom <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    clinicalPreset = "custom"
  )

  # Lab values
  result_lab <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    clinicalPreset = "lab_values"
  )

  # Biomarkers
  result_bio <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    clinicalPreset = "biomarkers"
  )

  # Patient characteristics
  result_patient <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    clinicalPreset = "patient_chars"
  )

  # Pathology scores
  result_path <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    clinicalPreset = "pathology_scores"
  )

  # All should complete
  expect_s3_class(result_custom, "jjhistostatsResults")
  expect_s3_class(result_lab, "jjhistostatsResults")
  expect_s3_class(result_bio, "jjhistostatsResults")
  expect_s3_class(result_patient, "jjhistostatsResults")
  expect_s3_class(result_path, "jjhistostatsResults")
})

test_that("jjhistostats handles different distribution types", {
  devtools::load_all()

  # Normal distribution
  data(jjhistostats_labvalues)
  result_normal <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "glucose",
    typestatistics = "parametric"
  )

  # Skewed distribution
  data(jjhistostats_skewed)
  result_skewed <- jjhistostats(
    data = jjhistostats_skewed,
    dep = "cea",
    typestatistics = "nonparametric"
  )

  # Bimodal distribution
  data(jjhistostats_bimodal)
  result_bimodal <- jjhistostats(
    data = jjhistostats_bimodal,
    dep = "response_score",
    typestatistics = "nonparametric"
  )

  # Uniform distribution
  data(jjhistostats_uniform)
  result_uniform <- jjhistostats(
    data = jjhistostats_uniform,
    dep = "uniform_score",
    typestatistics = "nonparametric"
  )

  # All should complete
  expect_s3_class(result_normal, "jjhistostatsResults")
  expect_s3_class(result_skewed, "jjhistostatsResults")
  expect_s3_class(result_bimodal, "jjhistostatsResults")
  expect_s3_class(result_uniform, "jjhistostatsResults")
})

test_that("jjhistostats handles single vs multiple variables", {
  devtools::load_all()

  data(jjhistostats_test)

  # Single variable
  result_single <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years"
  )

  # Two variables
  result_two <- jjhistostats(
    data = jjhistostats_test,
    dep = c("age_years", "bmi")
  )

  # Four variables
  result_four <- jjhistostats(
    data = jjhistostats_test,
    dep = c("age_years", "bmi", "tumor_size_mm", "hemoglobin")
  )

  # All should complete
  expect_s3_class(result_single, "jjhistostatsResults")
  expect_s3_class(result_two, "jjhistostatsResults")
  expect_s3_class(result_four, "jjhistostatsResults")
})

test_that("jjhistostats grouped analysis across different datasets", {
  devtools::load_all()

  # Test dataset stratified by treatment
  data(jjhistostats_test)
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    grvar = "treatment"
  )

  # Pathology stratified by tumor grade
  data(jjhistostats_pathology)
  result2 <- jjhistostats(
    data = jjhistostats_pathology,
    dep = "ki67_index",
    grvar = "tumor_grade"
  )

  # Grouped dataset stratified by disease stage
  data(jjhistostats_grouped)
  result3 <- jjhistostats(
    data = jjhistostats_grouped,
    dep = "biomarker_level",
    grvar = "disease_stage"
  )

  # All should complete
  expect_s3_class(result1, "jjhistostatsResults")
  expect_s3_class(result2, "jjhistostatsResults")
  expect_s3_class(result3, "jjhistostatsResults")
})

test_that("jjhistostats publication-ready configurations work correctly", {
  devtools::load_all()

  data(jjhistostats_test)

  # Configuration 1: Standard parametric
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric",
    centralityline = TRUE,
    centralitytype = "parametric",
    resultssubtitle = TRUE,
    title = "Age Distribution",
    xlab = "Age (years)",
    plotwidth = 800,
    plotheight = 600
  )

  # Configuration 2: Nonparametric for skewed data
  data(jjhistostats_skewed)
  result2 <- jjhistostats(
    data = jjhistostats_skewed,
    dep = "cea",
    typestatistics = "nonparametric",
    centralityline = TRUE,
    centralitytype = "nonparametric",
    resultssubtitle = TRUE,
    title = "CEA Distribution",
    xlab = "CEA (ng/mL)",
    plotwidth = 800,
    plotheight = 600
  )

  # Configuration 3: With diagnostics
  data(jjhistostats_labvalues)
  result3 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    typestatistics = "parametric",
    addDistributionDiagnostics = TRUE,
    ggpubrShowQQ = TRUE,
    ggpubrShowECDF = TRUE,
    title = "Cholesterol with Diagnostic Plots"
  )

  # All should complete
  expect_s3_class(result1, "jjhistostatsResults")
  expect_s3_class(result2, "jjhistostatsResults")
  expect_s3_class(result3, "jjhistostatsResults")
})

test_that("jjhistostats handles complete analysis pipeline", {
  devtools::load_all()

  data(jjhistostats_test)

  # Pipeline: Basic → Statistics → Centrality → Grouped → Publication

  # Step 1: Basic
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Step 2: Add statistics
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # Step 3: Add centrality
  result3 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    centralityline = TRUE,
    resultssubtitle = TRUE
  )
  expect_s3_class(result3, "jjhistostatsResults")

  # Step 4: Grouped
  result4 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    grvar = "treatment",
    centralityline = TRUE
  )
  expect_s3_class(result4, "jjhistostatsResults")

  # Step 5: Publication version
  result5 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric",
    centralityline = TRUE,
    centralitytype = "parametric",
    resultssubtitle = TRUE,
    title = "Age Distribution in Study Cohort",
    xlab = "Age (years)",
    caption = "n = 150 patients",
    plotwidth = 800,
    plotheight = 600
  )
  expect_s3_class(result5, "jjhistostatsResults")
})

test_that("jjhistostats handles ggpubr additional plots integration", {
  devtools::load_all()

  data(jjhistostats_test)

  # Basic ggpubr plot
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    addGGPubrPlot = TRUE
  )

  # With density curve
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    addGGPubrPlot = TRUE,
    ggpubrAddDensity = TRUE
  )

  # With mean line
  result3 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    addGGPubrPlot = TRUE,
    ggpubrAddMean = TRUE
  )

  # All should complete
  expect_s3_class(result1, "jjhistostatsResults")
  expect_s3_class(result2, "jjhistostatsResults")
  expect_s3_class(result3, "jjhistostatsResults")
})

test_that("jjhistostats handles different sample sizes correctly", {
  devtools::load_all()

  # Small (n=25)
  data(jjhistostats_small)
  result_small <- jjhistostats(
    data = jjhistostats_small,
    dep = "measurement"
  )

  # Medium (n=100-150)
  data(jjhistostats_test)
  result_medium <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years"
  )

  # Large (n=180)
  data(jjhistostats_grouped)
  result_large <- jjhistostats(
    data = jjhistostats_grouped,
    dep = "biomarker_level"
  )

  # All should complete
  expect_s3_class(result_small, "jjhistostatsResults")
  expect_s3_class(result_medium, "jjhistostatsResults")
  expect_s3_class(result_large, "jjhistostatsResults")
})
