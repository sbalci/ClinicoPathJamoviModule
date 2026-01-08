# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjdotplotstats
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjdotplotstats function exists and loads", {
  devtools::load_all()

  expect_true(exists("jjdotplotstats"))
})

test_that("jjdotplotstats runs with minimal required arguments", {
  devtools::load_all()

  data(jjdotplotstats_test, package = "ClinicoPath")

  # Minimal required arguments (dep and group only)
  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats produces expected output structure", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment"
  )

  # Check for main plot output
  expect_true(!is.null(result$plot))
})

test_that("jjdotplotstats handles parametric statistics (t-test/ANOVA)", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles nonparametric statistics (Mann-Whitney/Kruskal-Wallis)", {
  devtools::load_all()

  data(jjdotplotstats_skewed)

  result <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles robust statistics (trimmed means)", {
  devtools::load_all()

  data(jjdotplotstats_outliers)

  result <- jjdotplotstats(
    data = jjdotplotstats_outliers,
    dep = "response",
    group = "group",
    typestatistics = "robust"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles Bayesian statistics", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "bayes"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles two-group comparisons", {
  devtools::load_all()

  data(jjdotplotstats_twogroup)

  result <- jjdotplotstats(
    data = jjdotplotstats_twogroup,
    dep = "pain_score",
    group = "timepoint",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles three-group comparisons", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles four-group comparisons", {
  devtools::load_all()

  data(jjdotplotstats_fourgroup)

  result <- jjdotplotstats(
    data = jjdotplotstats_fourgroup,
    dep = "efficacy_score",
    group = "dose",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles centrality plotting", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Without centrality lines
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = FALSE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # With centrality lines
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles different centrality types", {
  devtools::load_all()

  data(jjdotplotstats_test)

  centrality_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (cent_type in centrality_types) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      centralityplotting = TRUE,
      centralitytype = cent_type
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for centralitytype:", cent_type))
  }
})

test_that("jjdotplotstats handles different effect size types", {
  devtools::load_all()

  data(jjdotplotstats_test)

  effect_types <- c("biased", "unbiased", "eta", "omega")

  for (eff_type in effect_types) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      typestatistics = "parametric",
      effsizetype = eff_type
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for effsizetype:", eff_type))
  }
})

test_that("jjdotplotstats handles centrality parameter options", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Mean
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE,
    centralityparameter = "mean"
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Median
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    centralityplotting = TRUE,
    centralityparameter = "median"
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles test value testing", {
  devtools::load_all()

  data(jjdotplotstats_reference)

  # With test value
  result <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles test value line", {
  devtools::load_all()

  data(jjdotplotstats_reference)

  # Without reference line
  result1 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10,
    testvalueline = FALSE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # With reference line
  result2 <- jjdotplotstats(
    data = jjdotplotstats_reference,
    dep = "bp_reduction",
    group = "drug",
    testvalue = 10,
    testvalueline = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles grouped analysis", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    grvar = "hospital"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles results subtitle", {
  devtools::load_all()

  data(jjdotplotstats_test)

  # Without subtitle
  result1 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    resultssubtitle = FALSE
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # With subtitle
  result2 <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles Bayes Factor message", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    typestatistics = "bayes",
    bfmessage = TRUE
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles custom titles", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    mytitle = "Custom Title",
    xtitle = "Custom X Label",
    ytitle = "Custom Y Label"
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles different datasets", {
  devtools::load_all()

  datasets <- list(
    jjdotplotstats_test = list(dep = "tumor_reduction", group = "treatment"),
    jjdotplotstats_twogroup = list(dep = "pain_score", group = "timepoint"),
    jjdotplotstats_fourgroup = list(dep = "efficacy_score", group = "dose"),
    jjdotplotstats_skewed = list(dep = "biomarker_level", group = "treatment"),
    jjdotplotstats_outliers = list(dep = "response", group = "group"),
    jjdotplotstats_reference = list(dep = "bp_reduction", group = "drug"),
    jjdotplotstats_qol = list(dep = "qol_score", group = "intervention"),
    jjdotplotstats_labvalues = list(dep = "hemoglobin", group = "anemia")
  )

  for (dataset_name in names(datasets)) {
    data(list = dataset_name, package = "ClinicoPath")
    dataset <- get(dataset_name)
    vars <- datasets[[dataset_name]]

    result <- jjdotplotstats(
      data = dataset,
      dep = vars$dep,
      group = vars$group
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for dataset:", dataset_name))
  }
})

test_that("jjdotplotstats handles different confidence levels", {
  devtools::load_all()

  data(jjdotplotstats_test)

  conf_levels <- c(0.90, 0.95, 0.99)

  for (conf in conf_levels) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      conflevel = conf
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for conflevel:", conf))
  }
})

test_that("jjdotplotstats handles different decimal places", {
  devtools::load_all()

  data(jjdotplotstats_test)

  k_values <- c(0, 1, 2, 3, 4, 5)

  for (k in k_values) {
    result <- jjdotplotstats(
      data = jjdotplotstats_test,
      dep = "tumor_reduction",
      group = "treatment",
      k = k
    )

    expect_s3_class(result, "jjdotplotstatsResults",
                   info = paste("Failed for k =", k))
  }
})

test_that("jjdotplotstats handles different plot dimensions", {
  devtools::load_all()

  data(jjdotplotstats_test)

  result <- jjdotplotstats(
    data = jjdotplotstats_test,
    dep = "tumor_reduction",
    group = "treatment",
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjdotplotstatsResults")
})

test_that("jjdotplotstats handles skewed data with parametric and nonparametric", {
  devtools::load_all()

  data(jjdotplotstats_skewed)

  # Parametric (may violate assumptions)
  result1 <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "parametric"
  )
  expect_s3_class(result1, "jjdotplotstatsResults")

  # Nonparametric (more appropriate)
  result2 <- jjdotplotstats(
    data = jjdotplotstats_skewed,
    dep = "biomarker_level",
    group = "treatment",
    typestatistics = "nonparametric"
  )
  expect_s3_class(result2, "jjdotplotstatsResults")
})
