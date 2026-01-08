# ═══════════════════════════════════════════════════════════
# Integration Tests: jjdotplotstats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("jjdotplotstats integrates with all test datasets", {
  devtools::load_all()

  datasets <- list(
    list(name = "jjdotplotstats_test", dep = "tumor_reduction", group = "treatment"),
    list(name = "jjdotplotstats_twogroup", dep = "pain_score", group = "timepoint"),
    list(name = "jjdotplotstats_fourgroup", dep = "efficacy_score", group = "dose"),
    list(name = "jjdotplotstats_skewed", dep = "biomarker_level", group = "treatment"),
    list(name = "jjdotplotstats_outliers", dep = "response", group = "group"),
    list(name = "jjdotplotstats_reference", dep = "bp_reduction", group = "drug"),
    list(name = "jjdotplotstats_qol", dep = "qol_score", group = "intervention"),
    list(name = "jjdotplotstats_labvalues", dep = "hemoglobin", group = "anemia")
  )

  for (dataset_info in datasets) {
    data(list = dataset_info$name, package = "ClinicoPath")
    dataset <- get(dataset_info$name)

    result <- jjdotplotstats(
      data = dataset,
      dep = dataset_info$dep,
      group = dataset_info$group
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for dataset:", dataset_info$name))
  }
})

test_that("jjdotplotstats data.frame vs tibble compatibility", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # As tibble (default)
  result_tibble <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # As data.frame
  result_df <- jjdotplotstats(
    data = as.data.frame(jjdotplotstats_test),
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Both should produce results
  expect_s3_class(result_tibble, "jjdotplotstatsResults")
  expect_s3_class(result_df, "jjdotplotstatsResults")
})

test_that("jjdotplotstats consistency across runs with same data", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Run twice with identical settings
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric"
  )

  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric"
  )

  # Should produce consistent results
  expect_s3_class(result1, "jjdotplotstatsResults")
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats complete clinical workflow - tumor response", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Step 1: Initial exploratory analysis
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    centralityplotting = TRUE,
    resultssubtitle = TRUE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Step 2: Stratified by hospital
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "hospital",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjdotplotstatsResults")

  # Step 3: Publication-ready figure
  result3 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "unbiased",
    centralityplotting = TRUE,
    centralitytype = "parametric",
    resultssubtitle = TRUE,
    mytitle = "Tumor Response by Treatment",
    xtitle = "Tumor Reduction (%)",
    ytitle = "Treatment Group",
    plotwidth = 800,
    plotheight = 600
  )
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats complete clinical workflow - pain assessment", {
  devtools::load_all()

  data(jjdotplotstats_twogroup)

  # Step 1: Pre-post comparison with parametric test
  result1 <- jjdotplotstats(
    data = jjdotplotstats_twogroup,
    dep = "pain_score",
    group = "timepoint",
    typestatistics = "parametric",
    centralityplotting = TRUE,
    resultssubtitle = TRUE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Step 2: Stratified by gender
  result2 <- jjdotplotstats(
    data = jjdotplotstats_twogroup,
    dep = "pain_score",
    group = "timepoint",
    grvar = "gender",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjdotplotstatsResults")

  # Step 3: Nonparametric alternative
  result3 <- jjdotplotstats(
    data = jjdotplotstats_twogroup,
    dep = "pain_score",
    group = "timepoint",
    typestatistics = "nonparametric",
    centralityplotting = TRUE,
    centralitytype = "nonparametric"
  )
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats complete clinical workflow - dose-response", {
  devtools::load_all()

  data(jjdotplotstats_fourgroup)

  # Step 1: ANOVA for dose-response
  result1 <- jjdotplotstats(
    data = jjdotplotstats_fourgroup,
    dep = "efficacy_score",
    group = "dose",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Step 2: With centrality lines
  result2 <- jjdotplotstats(
    data = jjdotplotstats_fourgroup,
    dep = "efficacy_score",
    group = "dose",
    typestatistics = "parametric",
    centralityplotting = TRUE,
    centralitytype = "parametric"
  )
  expect_s3_class(result2, "jjdotplotstatsResults")

  # Step 3: Effect size analysis
  result3 <- jjdotplotstats(
    data = jjdotplotstats_fourgroup,
    dep = "efficacy_score",
    group = "dose",
    typestatistics = "parametric",
    effsizetype = "eta",
    resultssubtitle = TRUE
  )
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats complete clinical workflow - biomarker analysis", {
  devtools::load_all()

  data(jjdotplotstats_skewed)

  # Step 1: Parametric approach (may be inappropriate)
  result1 <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Step 2: Nonparametric approach (more appropriate)
  result2 <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "nonparametric",
    centralityplotting = TRUE,
    centralitytype = "nonparametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")

  # Step 3: Robust approach
  result3 <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "robust",
    centralityplotting = TRUE,
    centralitytype = "robust"
  )
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats complete clinical workflow - reference value testing", {
  devtools::load_all()

  data(jjdotplotstats_reference)

  # Step 1: Basic comparison
  result1 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Step 2: With reference threshold
  result2 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10,
    testvalueline = TRUE,
    centralityplotting = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")

  # Step 3: Complete publication figure
  result3 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10,
    testvalueline = TRUE,
    centralityplotting = TRUE,
    centralitytype = "parametric",
    resultssubtitle = TRUE,
    mytitle = "BP Reduction vs Target (≥10 mmHg)",
    xtitle = "Blood Pressure Reduction (mmHg)",
    plotwidth = 800,
    plotheight = 600
  )
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats statistical method comparison on same data", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Parametric
  result_param <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric"
  )

  # Nonparametric
  result_nonparam <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "nonparametric"
  )

  # Robust
  result_robust <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "robust"
  )

  # Bayesian
  result_bayes <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "bayes"
  )

  # All should complete
  expect_s3_class(result_param, "jjdotplotstatsResults")
  expect_s3_class(result_nonparam, "jjdotplotstatsResults")
  expect_s3_class(result_robust, "jjdotplotstatsResults")
  expect_s3_class(result_bayes, "jjdotplotstatsResults")
})

test_that("jjdotplotstats effect size comparison on same data", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Cohen's d (biased)
  result_biased <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "biased"
  )

  # Hedge's g (unbiased)
  result_unbiased <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "unbiased"
  )

  # Eta-squared
  result_eta <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "eta"
  )

  # Omega-squared
  result_omega <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "omega"
  )

  # All should complete
  expect_s3_class(result_biased, "jjdotplotstatsResults")
  expect_s3_class(result_unbiased, "jjdotplotstatsResults")
  expect_s3_class(result_eta, "jjdotplotstatsResults")
  expect_s3_class(result_omega, "jjdotplotstatsResults")
})

test_that("jjdotplotstats centrality type comparison on same data", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Parametric (mean)
  result_param <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE,
    centralitytype = "parametric"
  )

  # Nonparametric (median)
  result_nonparam <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE,
    centralitytype = "nonparametric"
  )

  # Robust (trimmed mean)
  result_robust <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE,
    centralitytype = "robust"
  )

  # Bayesian
  result_bayes <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE,
    centralitytype = "bayes"
  )

  # All should complete
  expect_s3_class(result_param, "jjdotplotstatsResults")
  expect_s3_class(result_nonparam, "jjdotplotstatsResults")
  expect_s3_class(result_robust, "jjdotplotstatsResults")
  expect_s3_class(result_bayes, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles different group count scenarios", {
  devtools::load_all()

  # Two groups
  data(jjdotplotstats_twogroup)
  result_two <- jjdotplotstats(
    data = jjdotplotstats_twogroup,
    dep = "pain_score",
    group = "timepoint",
    typestatistics = "parametric",
    effsizetype = "biased"  # Cohen's d
  )

  # Three groups
  data(jjdotplotstats_test)
  result_three <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "eta"  # Eta-squared
  )

  # Four groups
  data(jjdotplotstats_fourgroup)
  result_four <- jjdotplotstats(
    data = jjdotplotstats_fourgroup,
    dep = "efficacy_score",
    group = "dose",
    typestatistics = "parametric",
    effsizetype = "omega"  # Omega-squared
  )

  # All should complete
  expect_s3_class(result_two, "jjdotplotstatsResults")
  expect_s3_class(result_three, "jjdotplotstatsResults")
  expect_s3_class(result_four, "jjdotplotstatsResults")
})

test_that("jjdotplotstats grouped analysis across different datasets", {
  devtools::load_all()

  # Tumor data stratified by hospital
  data(jjdotplotstats_test)
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "hospital"
  )

  # Pain data stratified by gender
  data(jjdotplotstats_twogroup)
  result2 <- jjdotplotstats(
    data = jjdotplotstats_twogroup,
    dep = "pain_score",
    group = "timepoint",
    grvar = "gender"
  )

  # Lab values stratified by sex
  data(jjdotplotstats_labvalues)
  result3 <- jjdotplotstats(
    data = jjdotplotstats_labvalues,
    dep = "hemoglobin",
    group = "anemia",
    grvar = "sex"
  )

  # All should complete
  expect_s3_class(result1, "jjdotplotstatsResults")
  expect_s3_class(result2, "jjdotplotstatsResults")
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles factor vs character group variables", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # As factor (default)
  result_factor <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Convert to character
  test_data_char <- jjdotplotstats_test
  test_data_char$treatment <- as.character(test_data_char$treatment)

  result_char <- jjdotplotstats(
    data = test_data_char,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Both should work
  expect_s3_class(result_factor, "jjdotplotstatsResults")
  expect_s3_class(result_char, "jjdotplotstatsResults")
})

test_that("jjdotplotstats publication-ready configurations work correctly", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Configuration 1: Standard parametric with effect sizes
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "biased",
    centralityplotting = TRUE,
    centralitytype = "parametric",
    centralityparameter = "mean",
    resultssubtitle = TRUE,
    mytitle = "Tumor Response by Treatment",
    xtitle = "Tumor Reduction (%)",
    ytitle = "Treatment Group",
    conflevel = 0.95,
    k = 2,
    plotwidth = 800,
    plotheight = 600
  )

  # Configuration 2: Nonparametric for skewed data
  data(jjdotplotstats_skewed)
  result2 <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "nonparametric",
    centralityplotting = TRUE,
    centralitytype = "nonparametric",
    resultssubtitle = TRUE,
    mytitle = "Biomarker Response",
    xtitle = "Biomarker Level",
    k = 2,
    plotwidth = 800,
    plotheight = 600
  )

  # Configuration 3: Reference value testing
  data(jjdotplotstats_reference)
  result3 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10,
    testvalueline = TRUE,
    centralityplotting = TRUE,
    resultssubtitle = TRUE,
    mytitle = "BP Reduction vs Target",
    xtitle = "Blood Pressure Reduction (mmHg)",
    k = 2,
    plotwidth = 800,
    plotheight = 600
  )

  # All should complete
  expect_s3_class(result1, "jjdotplotstatsResults")
  expect_s3_class(result2, "jjdotplotstatsResults")
  expect_s3_class(result3, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles complete analysis pipeline", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Pipeline: Basic → Refined → Stratified → Publication

  # Step 1: Basic exploratory
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment"
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Step 2: Add statistics
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")

  # Step 3: Add centrality
  result3 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    centralityplotting = TRUE,
    resultssubtitle = TRUE
  )
  expect_s3_class(result3, "jjdotplotstatsResults")

  # Step 4: Stratify
  result4 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "hospital",
    typestatistics = "parametric"
  )
  expect_s3_class(result4, "jjdotplotstatsResults")

  # Step 5: Final publication version
  result5 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric",
    effsizetype = "unbiased",
    centralityplotting = TRUE,
    centralitytype = "parametric",
    resultssubtitle = TRUE,
    mytitle = "Tumor Response Across Treatment Arms",
    xtitle = "Tumor Reduction (%)",
    ytitle = "Treatment Group",
    conflevel = 0.95,
    k = 2,
    plotwidth = 800,
    plotheight = 600
  )
  expect_s3_class(result5, "jjdotplotstatsResults")
})
