# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjwithinstats
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjwithinstats function exists and loads", {
  devtools::load_all()

  expect_true(exists("jjwithinstats"))
})

test_that("jjwithinstats runs with minimal required arguments (two timepoints)", {
  devtools::load_all()

  data(jjwithinstats_paired, package = "ClinicoPath")

  # Minimal required arguments (dep1 and dep2)
  result <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats runs with three timepoints", {
  devtools::load_all()

  data(jjwithinstats_test, package = "ClinicoPath")

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats runs with four timepoints", {
  devtools::load_all()

  data(jjwithinstats_biomarker, package = "ClinicoPath")

  result <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    dep4 = "month6"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats produces expected output structure", {
  devtools::load_all()

  data(jjwithinstats_test)

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12"
  )

  # Check for main plot output
  expect_true(!is.null(result$plot))
})

test_that("jjwithinstats handles parametric statistics (repeated measures ANOVA)", {
  devtools::load_all()

  data(jjwithinstats_test)

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles parametric statistics (paired t-test)", {
  devtools::load_all()

  data(jjwithinstats_paired)

  result <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles nonparametric statistics (Friedman test)", {
  devtools::load_all()

  data(jjwithinstats_biomarker)

  result <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    dep4 = "month6",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles nonparametric statistics (Wilcoxon signed-rank)", {
  devtools::load_all()

  data(jjwithinstats_paired)

  result <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles robust statistics", {
  devtools::load_all()

  data(jjwithinstats_laboratory)

  result <- jjwithinstats(
    data = jjwithinstats_laboratory,
    dep1 = "baseline_lab",
    dep2 = "week2_lab",
    dep3 = "week4_lab",
    dep4 = "week8_lab",
    typestatistics = "robust"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles Bayesian statistics", {
  devtools::load_all()

  data(jjwithinstats_test)

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "bayes"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles pairwise comparisons", {
  devtools::load_all()

  data(jjwithinstats_test)

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    pairwisecomparisons = TRUE,
    padjustmethod = "bonferroni"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles point paths (individual trajectories)", {
  devtools::load_all()

  data(jjwithinstats_test)

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    pointpath = TRUE
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles centrality plotting", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Parametric centrality (mean)
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    centralityplotting = TRUE,
    centralitytype = "parametric"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Nonparametric centrality (median)
  result2 <- jjwithinstats(
    data = jjwithinstats_biomarker,
    dep1 = "month0",
    dep2 = "month1",
    dep3 = "month3",
    centralityplotting = TRUE,
    centralitytype = "nonparametric"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Robust centrality (trimmed mean)
  result3 <- jjwithinstats(
    data = jjwithinstats_laboratory,
    dep1 = "baseline_lab",
    dep2 = "week2_lab",
    dep3 = "week4_lab",
    centralityplotting = TRUE,
    centralitytype = "robust"
  )
  expect_s3_class(result3, "jjwithinstatsResults")

  # Bayesian centrality (MAP)
  result4 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    centralityplotting = TRUE,
    centralitytype = "bayes"
  )
  expect_s3_class(result4, "jjwithinstatsResults")
})

test_that("jjwithinstats handles centrality path", {
  devtools::load_all()

  data(jjwithinstats_test)

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    centralityplotting = TRUE,
    centralitypath = TRUE
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles clinical presets", {
  devtools::load_all()

  # Biomarker preset
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

  # Treatment preset
  data(jjwithinstats_test)
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    clinicalpreset = "treatment"
  )
  expect_s3_class(result2, "jjwithinstatsResults")

  # Laboratory preset
  data(jjwithinstats_laboratory)
  result3 <- jjwithinstats(
    data = jjwithinstats_laboratory,
    dep1 = "baseline_lab",
    dep2 = "week2_lab",
    dep3 = "week4_lab",
    clinicalpreset = "laboratory"
  )
  expect_s3_class(result3, "jjwithinstatsResults")
})

test_that("jjwithinstats handles ggpubr plots", {
  devtools::load_all()

  data(jjwithinstats_test)

  # Paired plot type
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "paired",
    ggpubrPalette = "jco"
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Box plot type
  result2 <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "boxplot",
    ggpubrPalette = "lancet"
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats handles custom titles", {
  devtools::load_all()

  data(jjwithinstats_test)

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    mytitle = "Custom Analysis Title",
    xtitle = "Assessment Time",
    ytitle = "Tumor Size (mm)"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats handles different datasets", {
  devtools::load_all()

  datasets <- list(
    jjwithinstats_test = list(dep1 = "baseline", dep2 = "week4", dep3 = "week12"),
    jjwithinstats_biomarker = list(dep1 = "month0", dep2 = "month1", dep3 = "month3"),
    jjwithinstats_paired = list(dep1 = "pre_treatment", dep2 = "post_treatment"),
    jjwithinstats_laboratory = list(dep1 = "baseline_lab", dep2 = "week2_lab", dep3 = "week4_lab"),
    jjwithinstats_qol = list(dep1 = "qol_baseline", dep2 = "qol_month1", dep3 = "qol_month3"),
    jjwithinstats_symptoms = list(dep1 = "symptom_pre", dep2 = "symptom_week4", dep3 = "symptom_week8")
  )

  for (dataset_name in names(datasets)) {
    data(list = dataset_name, package = "ClinicoPath")
    dataset <- get(dataset_name)
    vars <- datasets[[dataset_name]]

    result <- jjwithinstats(
      data = dataset,
      dep1 = vars$dep1,
      dep2 = vars$dep2,
      dep3 = vars$dep3
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for dataset:", dataset_name))
  }
})
