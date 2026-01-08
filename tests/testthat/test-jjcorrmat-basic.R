# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjcorrmat
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjcorrmat function exists and loads", {
  devtools::load_all()

  expect_true(exists("jjcorrmat"))
})

test_that("jjcorrmat runs with minimal required arguments", {
  devtools::load_all()

  data(jjcorrmat_test, package = "ClinicoPath")

  # Minimal required arguments (dep only)
  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat produces expected output structure", {
  devtools::load_all()

  data(jjcorrmat_test)

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "necrosis_percent")
  )

  # Check for main plot output
  expect_true(!is.null(result$plot))
})

test_that("jjcorrmat handles parametric statistics (Pearson)", {
  devtools::load_all()

  data(jjcorrmat_test)

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles nonparametric statistics (Spearman)", {
  devtools::load_all()

  data(jjcorrmat_biomarker)

  result <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "afp", "ldh"),
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles robust statistics (percentage bend)", {
  devtools::load_all()

  data(jjcorrmat_vitals)

  result <- jjcorrmat(
    data = jjcorrmat_vitals,
    dep = c("systolic_bp", "diastolic_bp", "heart_rate"),
    typestatistics = "robust"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles Bayesian statistics", {
  devtools::load_all()

  data(jjcorrmat_mixed)

  result <- jjcorrmat(
    data = jjcorrmat_mixed,
    dep = c("var_a", "var_b", "var_c", "var_d"),
    typestatistics = "bayes"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles different matrix types", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Upper triangle
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    matrixtype = "upper"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Lower triangle
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    matrixtype = "lower"
  )
  expect_s3_class(result2, "jjcorrmatResults")

  # Full matrix
  result3 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    matrixtype = "full"
  )
  expect_s3_class(result3, "jjcorrmatResults")
})

test_that("jjcorrmat handles different matrix methods", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Square method
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    matrixmethod = "square"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Circle method
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    matrixmethod = "circle"
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat handles grouped analysis", {
  devtools::load_all()

  data(jjcorrmat_test)

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    grvar = "tumor_stage"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles p-value adjustment methods", {
  devtools::load_all()

  data(jjcorrmat_test)

  adjust_methods <- c("holm", "none", "hochberg", "hommel", "bonferroni", "BH", "BY")

  for (method in adjust_methods) {
    result <- jjcorrmat(
      data = jjcorrmat_test,
      dep = c("tumor_size", "ki67_index", "mitotic_count"),
      padjustmethod = method
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for padjustmethod:", method))
  }
})

test_that("jjcorrmat handles partial correlations", {
  devtools::load_all()

  data(jjcorrmat_test)

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count", "age"),
    partial = TRUE,
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles different missing data strategies", {
  devtools::load_all()

  data(jjcorrmat_test)

  # Listwise deletion
  result1 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "necrosis_percent"),
    naHandling = "listwise"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  # Pairwise deletion
  result2 <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "necrosis_percent"),
    naHandling = "pairwise"
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat handles custom colors", {
  devtools::load_all()

  data(jjcorrmat_test)

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    lowcolor = "blue",
    midcolor = "white",
    highcolor = "red"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles custom titles", {
  devtools::load_all()

  data(jjcorrmat_test)

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    title = "Custom Title",
    subtitle = "Custom Subtitle",
    caption = "Custom Caption"
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles different significance levels", {
  devtools::load_all()

  data(jjcorrmat_test)

  sig_levels <- c(0.01, 0.05, 0.10)

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

test_that("jjcorrmat handles different confidence levels", {
  devtools::load_all()

  data(jjcorrmat_test)

  conf_levels <- c(0.90, 0.95, 0.99)

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

test_that("jjcorrmat handles different decimal places", {
  devtools::load_all()

  data(jjcorrmat_test)

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

test_that("jjcorrmat handles different datasets", {
  devtools::load_all()

  datasets <- list(
    jjcorrmat_test = c("tumor_size", "ki67_index", "mitotic_count"),
    jjcorrmat_biomarker = c("cea", "ca199", "afp", "ldh"),
    jjcorrmat_labvalues = c("glucose", "cholesterol", "triglycerides", "hdl"),
    jjcorrmat_imaging = c("tumor_volume", "tumor_longest_diameter", "suv_max"),
    jjcorrmat_vitals = c("systolic_bp", "diastolic_bp", "heart_rate"),
    jjcorrmat_mixed = c("var_a", "var_b", "var_c", "var_d")
  )

  for (dataset_name in names(datasets)) {
    data(list = dataset_name, package = "ClinicoPath")
    dataset <- get(dataset_name)
    vars <- datasets[[dataset_name]]

    result <- jjcorrmat(
      data = dataset,
      dep = vars
    )

    expect_s3_class(result, "jjcorrmatResults",
                   info = paste("Failed for dataset:", dataset_name))
  }
})

test_that("jjcorrmat handles minimum number of variables (2)", {
  devtools::load_all()

  data(jjcorrmat_test)

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles many variables (8+)", {
  devtools::load_all()

  data(jjcorrmat_labvalues)

  result <- jjcorrmat(
    data = jjcorrmat_labvalues,
    dep = c("glucose", "cholesterol", "triglycerides", "hdl",
            "ldl", "creatinine", "alt", "ast")
  )

  expect_s3_class(result, "jjcorrmatResults")
})

test_that("jjcorrmat handles log-normal distributed data", {
  devtools::load_all()

  data(jjcorrmat_biomarker)

  # Should work with both parametric and nonparametric
  result1 <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "afp"),
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjcorrmatResults")

  result2 <- jjcorrmat(
    data = jjcorrmat_biomarker,
    dep = c("cea", "ca199", "afp"),
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjcorrmatResults")
})

test_that("jjcorrmat handles different plot dimensions", {
  devtools::load_all()

  data(jjcorrmat_test)

  result <- jjcorrmat(
    data = jjcorrmat_test,
    dep = c("tumor_size", "ki67_index", "mitotic_count"),
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjcorrmatResults")
})
