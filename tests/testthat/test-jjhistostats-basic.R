# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjhistostats
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjhistostats function exists and loads", {

  expect_true(exists("jjhistostats"))
})

test_that("jjhistostats runs with minimal required arguments", {

  data(jjhistostats_test, package = "ClinicoPath")

  # Minimal required arguments (dep only)
  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats produces expected output structure", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years"
  )

  # Check for main plot output
  expect_true(!is.null(result$plot1))
})

test_that("jjhistostats handles parametric statistics (Shapiro-Wilk)", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles nonparametric statistics (Anderson-Darling)", {

  data(jjhistostats_skewed)

  result <- jjhistostats(
    data = jjhistostats_skewed,
    dep = "cea",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles robust statistics", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "tumor_size_mm",
    typestatistics = "robust"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles Bayesian statistics", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "bayes"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles single variable", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "bmi"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles multiple variables", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = c("age_years", "bmi", "hemoglobin")
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles centrality line", {

  data(jjhistostats_test)

  # Without centrality line
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    centralityline = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # With centrality line
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    centralityline = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles different centrality types", {

  data(jjhistostats_test)

  centrality_types <- c("default", "parametric", "nonparametric", "robust", "bayes")

  for (cent_type in centrality_types) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "age_years",
      centralityline = TRUE,
      centralitytype = cent_type
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for centralitytype:", cent_type))
  }
})

test_that("jjhistostats handles results subtitle", {

  data(jjhistostats_test)

  # Without subtitle
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    resultssubtitle = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # With subtitle
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles one-sample test", {

  data(jjhistostats_labvalues)

  # Without one-sample test
  result1 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    enableOneSampleTest = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # With one-sample test
  result2 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    enableOneSampleTest = TRUE,
    test.value = 200
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles grouped analysis", {

  data(jjhistostats_grouped)

  result <- jjhistostats(
    data = jjhistostats_grouped,
    dep = "biomarker_level",
    grvar = "disease_stage"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles bin width customization", {

  data(jjhistostats_test)

  # Automatic bin width
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "tumor_size_mm",
    changebinwidth = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Manual bin width
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "tumor_size_mm",
    changebinwidth = TRUE,
    binwidth = 5.0
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles clinical presets", {

  data(jjhistostats_test)

  presets <- c("custom", "lab_values", "biomarkers", "patient_chars", "pathology_scores")

  for (preset in presets) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "age_years",
      clinicalPreset = preset
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for preset:", preset))
  }
})

test_that("jjhistostats handles Bayes Factor message", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "bayes",
    bf.message = TRUE
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles custom titles", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    title = "Custom Title",
    subtitle = "Custom Subtitle",
    caption = "Custom Caption",
    xlab = "Custom X Label"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles different datasets", {

  datasets <- list(
    jjhistostats_test = "age_years",
    jjhistostats_labvalues = "glucose",
    jjhistostats_skewed = "cea",
    jjhistostats_bimodal = "age_bimodal",
    jjhistostats_pathology = "ki67_index",
    jjhistostats_grouped = "biomarker_level",
    jjhistostats_small = "measurement",
    jjhistostats_uniform = "uniform_score"
  )

  for (dataset_name in names(datasets)) {
    data(list = dataset_name, package = "ClinicoPath")
    dataset <- get(dataset_name)
    var <- datasets[[dataset_name]]

    result <- jjhistostats(
      data = dataset,
      dep = var
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for dataset:", dataset_name))
  }
})

test_that("jjhistostats handles different confidence levels", {

  data(jjhistostats_test)

  conf_levels <- c(0.90, 0.95, 0.99)

  for (conf in conf_levels) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "age_years",
      conf.level = conf
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for conf.level:", conf))
  }
})

test_that("jjhistostats handles different decimal places", {

  data(jjhistostats_test)

  digits_values <- c(0, 1, 2, 3, 4, 5)

  for (d in digits_values) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "age_years",
      resultssubtitle = TRUE,
      digits = d
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for digits =", d))
  }
})

test_that("jjhistostats handles different plot dimensions", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles normally distributed data", {

  data(jjhistostats_labvalues)

  result <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "glucose",
    typestatistics = "parametric",
    centralityline = TRUE,
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles skewed data", {

  data(jjhistostats_skewed)

  result <- jjhistostats(
    data = jjhistostats_skewed,
    dep = "ca199",
    typestatistics = "nonparametric",
    centralityline = TRUE,
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles bimodal distribution", {

  data(jjhistostats_bimodal)

  result <- jjhistostats(
    data = jjhistostats_bimodal,
    dep = "response_score",
    typestatistics = "nonparametric"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles ggpubr plot addition", {

  data(jjhistostats_test)

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    addGGPubrPlot = TRUE,
    ggpubrAddDensity = TRUE,
    ggpubrAddMean = TRUE
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles distribution diagnostics", {

  data(jjhistostats_labvalues)

  result <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    addDistributionDiagnostics = TRUE,
    ggpubrShowQQ = TRUE,
    ggpubrShowECDF = TRUE
  )

  expect_s3_class(result, "jjhistostatsResults")
})
