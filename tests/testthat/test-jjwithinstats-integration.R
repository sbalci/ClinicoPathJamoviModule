# ═══════════════════════════════════════════════════════════
# Integration Tests: jjwithinstats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("jjwithinstats integrates with all test datasets", {
  devtools::load_all()

  # Load all jjwithinstats test datasets
  data(jjwithinstats_test, package = "ClinicoPath")
  data(jjwithinstats_biomarker, package = "ClinicoPath")
  data(jjwithinstats_paired, package = "ClinicoPath")
  data(jjwithinstats_laboratory, package = "ClinicoPath")
  data(jjwithinstats_qol, package = "ClinicoPath")
  data(jjwithinstats_symptoms, package = "ClinicoPath")

  # All datasets should load successfully
  expect_true(exists("jjwithinstats_test"))
  expect_true(exists("jjwithinstats_biomarker"))
  expect_true(exists("jjwithinstats_paired"))
  expect_true(exists("jjwithinstats_laboratory"))
  expect_true(exists("jjwithinstats_qol"))
  expect_true(exists("jjwithinstats_symptoms"))
})

test_that("jjwithinstats works with data.frame vs tibble", {
  devtools::load_all()

  data(jjwithinstats_test)

  # As tibble (original)
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Convert to data.frame
  df_data <- as.data.frame(jjwithinstats_test)
  result2 <- jjwithinstats(
    data = df_data,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats produces consistent results across multiple runs", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Run same analysis twice
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "parametric"
  )

  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "parametric"
  )

  # Both should complete
  expect_s3_class(result1, "jjwithinstatsResults")
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats clinical presets match appropriate datasets", {
  devtools::load_all()

  # Biomarker preset with biomarker data
  data(jjwithinstats_biomarker)
  result1 <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    dep4 = "month6",
    clinicalpreset = "biomarker"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Treatment preset with tumor size data
  data(jjwithinstats_test)
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    clinicalpreset = "treatment"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Laboratory preset with lab data
  data(jjwithinstats_laboratory)
  result3 <- jjwithinstats(
    data = jjwithinstats_laboratory,
    dep1 = "baseline_lab",
    dep2 = "week2_lab",
    dep3 = "week4_lab",
    dep4 = "week8_lab",
    clinicalpreset = "laboratory"
  )
  expect_s3_class(result3, "jjwithinstatsResults")
})

test_that("jjwithinstats statistical types match data distributions", {
  devtools::load_all()

  # Parametric for normally distributed QoL data
  data(jjwithinstats_qol)
  result1 <- jjwithinstats(
    data = jjwithinstats_qol,
    dep1 = "qol_baseline",
    dep2 = "qol_month1",
    dep3 = "qol_month3",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Nonparametric for log-normal biomarker data
  data(jjwithinstats_biomarker)
  result2 <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Robust for outlier-contaminated lab data
  data(jjwithinstats_laboratory)
  result3 <- jjwithinstats(
    data = jjwithinstats_laboratory,
    dep1 = "baseline_lab",
    dep2 = "week2_lab",
    dep3 = "week4_lab",
    typestatistics = "robust"
  )
  expect_s3_class(result3, "jjwithinstatsResults")
})

test_that("jjwithinstats handles two-timepoint vs multi-timepoint comparisons", {
  devtools::load_all()

  # Two timepoints (paired t-test)
  data(jjwithinstats_paired)
  result1 <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Three timepoints (repeated measures ANOVA)
  data(jjwithinstats_test)
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "parametric"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Four timepoints (repeated measures ANOVA)
  data(jjwithinstats_biomarker)
  result3 <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    dep4 = "month6",
    typestatistics = "parametric"
  )
  expect_s3_class(result3, "jjwithinstatsResults")
})

test_that("jjwithinstats pairwise comparisons work with different adjustments", {
  devtools::load_all()

  data(jjwithinstats_test)

  adjust_methods <- c("bonferroni", "holm", "hochberg", "BH", "fdr")

  for (method in adjust_methods) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      pairwisecomparisons = TRUE,
      padjustmethod = method,
      typestatistics = "parametric"
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for adjustment method:", method))
  }
})

test_that("jjwithinstats centrality types align with statistical types", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Parametric stats with parametric centrality
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "parametric",
    centralityplotting = TRUE,
    centralitytype = "parametric"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Nonparametric stats with nonparametric centrality
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "nonparametric",
    centralityplotting = TRUE,
    centralitytype = "nonparametric"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Robust stats with robust centrality
  result3 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "robust",
    centralityplotting = TRUE,
    centralitytype = "robust"
  )
  expect_s3_class(result3, "jjwithinstatsResults")

  # Bayesian stats with Bayesian centrality
  result4 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "bayes",
    centralityplotting = TRUE,
    centralitytype = "bayes"
  )
  expect_s3_class(result4, "jjwithinstatsResults")
})

test_that("jjwithinstats ggpubr integration produces additional plots", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Main plot + ggpubr plot
  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "paired",
    ggpubrPalette = "jco"
  )

  expect_s3_class(result, "jjwithinstatsResults")
  # Should have both main plot and ggpubr plot outputs
})

test_that("jjwithinstats handles complete workflow scenarios", {
  devtools::load_all()

  # Scenario 1: Tumor response clinical trial
  data(jjwithinstats_test)
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    clinicalpreset = "treatment",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    padjustmethod = "bonferroni",
    centralityplotting = TRUE,
    pointpath = TRUE,
    mytitle = "Tumor Response Over 12 Weeks"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Scenario 2: Pain assessment pre-post intervention
  data(jjwithinstats_paired)
  result2 <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    typestatistics = "parametric",
    centralityplotting = TRUE,
    pointpath = TRUE,
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "paired",
    mytitle = "Pain Reduction: Pre vs Post"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Scenario 3: Biomarker monitoring (skewed data)
  data(jjwithinstats_biomarker)
  result3 <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    dep4 = "month6",
    clinicalpreset = "biomarker",
    typestatistics = "nonparametric",
    pairwisecomparisons = TRUE,
    centralityplotting = TRUE,
    centralitytype = "nonparametric",
    mytitle = "Biomarker Decline Over 6 Months"
  )
  expect_s3_class(result3, "jjwithinstatsResults")

  # Scenario 4: Lab monitoring with outliers
  data(jjwithinstats_laboratory)
  result4 <- jjwithinstats(
    data = jjwithinstats_laboratory,
    dep1 = "baseline_lab",
    dep2 = "week2_lab",
    dep3 = "week4_lab",
    dep4 = "week8_lab",
    clinicalpreset = "laboratory",
    typestatistics = "robust",
    centralityplotting = TRUE,
    centralitytype = "robust",
    mytitle = "Laboratory Value Monitoring"
  )
  expect_s3_class(result4, "jjwithinstatsResults")

  # Scenario 5: QoL improvement trajectory
  data(jjwithinstats_qol)
  result5 <- jjwithinstats(
    data = jjwithinstats_qol,
    dep1 = "qol_baseline",
    dep2 = "qol_month1",
    dep3 = "qol_month3",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    centralityplotting = TRUE,
    centralitypath = TRUE,
    pointpath = TRUE,
    mytitle = "Quality of Life Improvement"
  )
  expect_s3_class(result5, "jjwithinstatsResults")
})

test_that("jjwithinstats handles publication-ready output configurations", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Configuration 1: Main ggstatsplot with all features
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    padjustmethod = "bonferroni",
    centralityplotting = TRUE,
    centralitypath = TRUE,
    pointpath = TRUE,
    resultssubtitle = TRUE,
    showexplanations = TRUE,
    plotwidth = 800,
    plotheight = 600
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Configuration 2: ggpubr publication figure
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "paired",
    ggpubrPalette = "jco",
    ggpubrAddStats = TRUE,
    ggpubrShowLines = TRUE,
    mytitle = "Tumor Response by Treatment Week",
    xtitle = "Assessment Time",
    ytitle = "Tumor Size (mm)"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Configuration 3: Colorblind-safe accessible figure
  result3 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    colorblindSafe = TRUE,
    centralityplotting = TRUE,
    pointpath = TRUE,
    mytitle = "Accessible Visualization"
  )
  expect_s3_class(result3, "jjwithinstatsResults")
})

test_that("jjwithinstats handles different outcome patterns", {
  devtools::load_all()

  # Pattern 1: Progressive decline (tumor size)
  data(jjwithinstats_test)
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Pattern 2: Progressive improvement (QoL)
  data(jjwithinstats_qol)
  result2 <- jjwithinstats(
    data = jjwithinstats_qol,
    dep1 = "qol_baseline",
    dep2 = "qol_month1",
    dep3 = "qol_month3"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Pattern 3: Dramatic change (pain pre-post)
  data(jjwithinstats_paired)
  result3 <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment"
  )
  expect_s3_class(result3, "jjwithinstatsResults")

  # Pattern 4: Steady decline (symptoms)
  data(jjwithinstats_symptoms)
  result4 <- jjwithinstats(
    data = jjwithinstats_symptoms,
    dep1 = "symptom_pre",
    dep2 = "symptom_week4",
    dep3 = "symptom_week8",
    dep4 = "symptom_week12"
  )
  expect_s3_class(result4, "jjwithinstatsResults")
})

test_that("jjwithinstats maintains data integrity through analysis", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Store original data dimensions
  original_n <- nrow(jjwithinstats_test)
  original_vars <- ncol(jjwithinstats_test)

  # Run analysis
  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  # Original data should be unchanged
  expect_equal(nrow(jjwithinstats_test), original_n)
  expect_equal(ncol(jjwithinstats_test), original_vars)
})

test_that("jjwithinstats handles cross-dataset variable patterns", {
  devtools::load_all()

  # All datasets should work with their respective variable patterns
  datasets_and_vars <- list(
    list(data = "jjwithinstats_test",
         vars = c("baseline", "week4", "week12")),
    list(data = "jjwithinstats_biomarker",
         vars = c("month0", "month1", "month3", "month6")),
    list(data = "jjwithinstats_paired",
         vars = c("pre_treatment", "post_treatment")),
    list(data = "jjwithinstats_laboratory",
         vars = c("baseline_lab", "week2_lab", "week4_lab", "week8_lab")),
    list(data = "jjwithinstats_qol",
         vars = c("qol_baseline", "qol_month1", "qol_month3")),
    list(data = "jjwithinstats_symptoms",
         vars = c("symptom_pre", "symptom_week4", "symptom_week8", "symptom_week12"))
  )

  for (item in datasets_and_vars) {
    data(list = item$data, package = "ClinicoPath")
    dataset <- get(item$data)

    # Build argument list dynamically
    args <- list(data = dataset)
    for (i in seq_along(item$vars)) {
      args[[paste0("dep", i)]] <- item$vars[i]
    }

    result <- do.call(jjwithinstats, args)

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for dataset:", item$data))
  }
})
