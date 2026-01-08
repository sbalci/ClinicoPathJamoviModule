# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjbetweenstats
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjbetweenstats function exists and loads", {
  devtools::load_all()

  expect_true(exists("jjbetweenstats"))
})

test_that("jjbetweenstats runs with minimal required arguments", {
  devtools::load_all()

  data(jjbetweenstats_test, package = "ClinicoPath")

  # Minimal required arguments (dep and group)
  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats produces expected output structure", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Check for main plot output
  expect_true(!is.null(result$plot))
})

test_that("jjbetweenstats handles three-group comparison (ANOVA)", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles two-group comparison (t-test)", {
  devtools::load_all()

  data(jjbetweenstats_twogroup)

  result <- jjbetweenstats(
    data = jjbetweenstats_twogroup,
    dep = "outcome",
    group = "group",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles four-group comparison", {
  devtools::load_all()

  data(jjbetweenstats_fourgroup)

  result <- jjbetweenstats(
    data = jjbetweenstats_fourgroup,
    dep = "efficacy_score",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles parametric statistics", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "qol_score",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles nonparametric statistics", {
  devtools::load_all()

  data(jjbetweenstats_skewed)

  result <- jjbetweenstats(
    data = jjbetweenstats_skewed,
    dep = "tumor_marker",
    group = "treatment",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles robust statistics", {
  devtools::load_all()

  data(jjbetweenstats_outliers)

  result <- jjbetweenstats(
    data = jjbetweenstats_outliers,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "robust"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles Bayesian statistics", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "bayes"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles split-by variable", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "sex"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles multiple dependent variables", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = c("tumor_reduction", "pain_score", "qol_score"),
    group = "treatment"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles pairwise comparisons", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    pairwisecomparisons = TRUE,
    padjustmethod = "holm"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles centrality plotting", {
  devtools::load_all()

  data(jjbetweenstats_test)

  # Parametric centrality (mean)
  result1 <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "qol_score",
    group = "treatment",
    centralityplotting = TRUE,
    centralitytype = "parametric"
  )
  expect_s3_class(result1, "jjbetweenstatsResults")

  # Nonparametric centrality (median)
  result2 <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "pain_score",
    group = "treatment",
    centralityplotting = TRUE,
    centralitytype = "nonparametric"
  )
  expect_s3_class(result2, "jjbetweenstatsResults")

  # Robust centrality (trimmed mean)
  result3 <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE,
    centralitytype = "robust"
  )
  expect_s3_class(result3, "jjbetweenstatsResults")

  # Bayesian centrality (MAP)
  result4 <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "biomarker_level",
    group = "treatment",
    centralityplotting = TRUE,
    centralitytype = "bayes"
  )
  expect_s3_class(result4, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles equal vs unequal variances", {
  devtools::load_all()

  data(jjbetweenstats_test)

  # Equal variances
  result1 <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    varequal = TRUE
  )
  expect_s3_class(result1, "jjbetweenstatsResults")

  # Unequal variances (Welch)
  result2 <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    varequal = FALSE
  )
  expect_s3_class(result2, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles ggpubr plots", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "boxviolin",
    ggpubrPalette = "jco"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles custom titles", {
  devtools::load_all()

  data(jjbetweenstats_test)

  result <- jjbetweenstats(
    data = jjbetweenstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    mytitle = "Custom Analysis Title",
    xtitle = "Treatment Arm",
    ytitle = "Tumor Reduction (mm)"
  )

  expect_s3_class(result, "jjbetweenstatsResults")
})

test_that("jjbetweenstats handles different continuous outcomes", {
  devtools::load_all()

  data(jjbetweenstats_test)

  outcomes <- c("tumor_reduction", "pain_score", "qol_score",
                "biomarker_level", "age", "bmi")

  for (outcome in outcomes) {
    result <- jjbetweenstats(
      data = jjbetweenstats_test,
      dep = outcome,
      group = "treatment"
    )

    expect_s3_class(result, "jjbetweenstatsResults",
                   info = paste("Failed for outcome:", outcome))
  }
})

test_that("jjbetweenstats handles different grouping variables", {
  devtools::load_all()

  data(jjbetweenstats_test)

  groups <- c("treatment", "tumor_stage", "sex", "age_group")

  for (grp in groups) {
    result <- jjbetweenstats(
      data = jjbetweenstats_test,
      dep = "tumor_reduction",
      group = grp
    )

    expect_s3_class(result, "jjbetweenstatsResults",
                   info = paste("Failed for group:", grp))
  }
})
