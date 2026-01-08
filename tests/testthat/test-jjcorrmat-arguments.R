# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjcorrmat
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(jjcorrmat_test)
data(jjcorrmat_biomarker)
data(jjcorrmat_labvalues)
data(jjcorrmat_mixed)

test_that("jjcorrmat respects all statistical type options", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for typestatistics:", stat_type))
  }
})

test_that("jjcorrmat respects all matrix type options", {
  devtools::load_all()

  matrix_types <- c("upper", "lower", "full")

  for (mat_type in matrix_types) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      matrixtype = mat_type
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for matrixtype:", mat_type))
  }
})

test_that("jjcorrmat respects all matrix method options", {
  devtools::load_all()

  matrix_methods <- c("square", "circle")

  for (method in matrix_methods) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      matrixmethod = method
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for matrixmethod:", method))
  }
})

test_that("jjcorrmat respects all p-value adjustment methods", {
  devtools::load_all()

  adjust_methods <- c("holm", "none", "hochberg", "hommel", "bonferroni", "BH", "BY")

  for (method in adjust_methods) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
      padjustmethod = method
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for padjustmethod:", method))
  }
})

test_that("jjcorrmat respects missing data handling options", {
  devtools::load_all()

  na_methods <- c("listwise", "pairwise")

  for (method in na_methods) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "necrosis_percent"),
      naHandling = method
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for naHandling:", method))
  }
})

test_that("jjcorrmat respects partial correlation option", {
  devtools::load_all()

  # Without partial correlations
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "age"),
    partial = FALSE
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # With partial correlations
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "age"),
    partial = TRUE
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat respects significance level parameter", {
  devtools::load_all()

  sig_levels <- c(0.001, 0.01, 0.05, 0.10)

  for (sig in sig_levels) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      siglevel = sig
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for siglevel:", sig))
  }
})

test_that("jjcorrmat respects confidence level parameter", {
  devtools::load_all()

  conf_levels <- c(0.80, 0.90, 0.95, 0.99)

  for (conf in conf_levels) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      conflevel = conf
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for conflevel:", conf))
  }
})

test_that("jjcorrmat respects decimal places parameter", {
  devtools::load_all()

  k_values <- c(0, 1, 2, 3, 4, 5)

  for (k in k_values) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      k = k
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for k =", k))
  }
})

test_that("jjcorrmat respects custom color parameters", {
  devtools::load_all()

  color_schemes <- list(
    list(low = "blue", mid = "white", high = "red"),
    list(low = "#E69F00", mid = "white", high = "#009E73"),
    list(low = "#0571B0", mid = "white", high = "#CA0020")
  )

  for (i in seq_along(color_schemes)) {
    scheme <- color_schemes[[i]]
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      lowcolor = scheme$low,
      midcolor = scheme$mid,
      highcolor = scheme$high
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for color scheme", i))
  }
})

test_that("jjcorrmat respects custom title parameters", {
  devtools::load_all()

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    title = "Custom Title",
    subtitle = "Custom Subtitle",
    caption = "Custom Caption"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat respects plot dimension parameters", {
  devtools::load_all()

  dimensions <- list(
    list(width = 300, height = 300),
    list(width = 600, height = 450),
    list(width = 1200, height = 800)
  )

  for (dim in dimensions) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      plotwidth = dim$width,
      plotheight = dim$height
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for width =", dim$width, "height =", dim$height))
  }
})

test_that("jjcorrmat respects show explanations option", {
  devtools::load_all()

  # Without explanations
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    showexplanations = FALSE
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # With explanations
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    showexplanations = TRUE
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat handles different variable counts", {
  devtools::load_all()

  # 2 variables (minimum)
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index")
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # 4 variables
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent")
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # 6 variables
  result3 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count",
            "necrosis_percent", "age", "bmi")
  )
  expect_s3_class(result3, "jjcorrmatResults")

  # 8 variables
  result4 <- jjcorrmat(
    data = jjcorrmat_labvalues,
    dep = c("glucose", "cholesterol", "triglycerides", "hdl",
            "ldl", "creatinine", "alt", "ast")
  )
  expect_s3_class(result4, "jjcorrmatResults")
})

test_that("jjcorrmat handles combinations of matrix type and method", {
  devtools::load_all()

  combinations <- expand.grid(
    matrixtype = c("upper", "lower", "full"),
    matrixmethod = c("square", "circle"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      matrixtype = combinations$matrixtype[i],
      matrixmethod = combinations$matrixmethod[i]
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for matrixtype =", combinations$matrixtype[i],
                               "matrixmethod =", combinations$matrixmethod[i]))
  }
})

test_that("jjcorrmat handles combinations of statistical type and adjustment", {
  devtools::load_all()

  combinations <- expand.grid(
    typestatistics = c("parametric", "nonparametric", "robust"),
    padjustmethod = c("none", "holm", "bonferroni", "BH"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
      typestatistics = combinations$typestatistics[i],
      padjustmethod = combinations$padjustmethod[i]
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for typestatistics =", combinations$typestatistics[i],
                               "padjustmethod =", combinations$padjustmethod[i]))
  }
})

test_that("jjcorrmat handles comprehensive argument combinations", {
  devtools::load_all()

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent"),
    typestatistics = "parametric",
    matrixtype = "upper",
    matrixmethod = "circle",
    siglevel = 0.05,
    conflevel = 0.95,
    padjustmethod = "bonferroni",
    k = 2,
    partial = FALSE,
    naHandling = "listwise",
    lowcolor = "#0571B0",
    midcolor = "white",
    highcolor = "#CA0020",
    title = "Comprehensive Test",
    subtitle = "All parameters specified",
    caption = "Test caption",
    showexplanations = TRUE,
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles grouped analysis with different statistical types", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      grvar = "tumor_stage",
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for grouped analysis with typestatistics:", stat_type))
  }
})

test_that("jjcorrmat handles grouped analysis with different matrix types", {
  devtools::load_all()

  matrix_types <- c("upper", "lower", "full")

  for (mat_type in matrix_types) {
    result <- jjcorrmat(
      data = jjcorrmat_biomarker,
      dep = c("cea", "ca199", "ldh", "crp"),
      grvar = "cancer_type",
      matrixtype = mat_type
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for grouped analysis with matrixtype:", mat_type))
  }
})

test_that("jjcorrmat handles partial correlations with different statistical types", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust")

  for (stat_type in stat_types) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count", "age"),
      partial = TRUE,
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for partial correlation with typestatistics:", stat_type))
  }
})

test_that("jjcorrmat handles missing data with different adjustment methods", {
  devtools::load_all()

  combinations <- expand.grid(
    naHandling = c("listwise", "pairwise"),
    padjustmethod = c("none", "holm", "bonferroni"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "necrosis_percent"),
      naHandling = combinations$naHandling[i],
      padjustmethod = combinations$padjustmethod[i]
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for naHandling =", combinations$naHandling[i],
                               "padjustmethod =", combinations$padjustmethod[i]))
  }
})
