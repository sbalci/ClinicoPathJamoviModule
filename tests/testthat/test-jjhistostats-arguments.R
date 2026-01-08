# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjhistostats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(jjhistostats_test)
data(jjhistostats_labvalues)
data(jjhistostats_grouped)

test_that("jjhistostats respects all statistical type options", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "age_years",
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for typestatistics:", stat_type))
  }
})

test_that("jjhistostats respects all centrality type options", {
  devtools::load_all()

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

test_that("jjhistostats respects all clinical preset options", {
  devtools::load_all()

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

test_that("jjhistostats respects centrality line option", {
  devtools::load_all()

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

test_that("jjhistostats respects results subtitle option", {
  devtools::load_all()

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

test_that("jjhistostats respects one-sample test option", {
  devtools::load_all()

  # Disabled
  result1 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    enableOneSampleTest = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Enabled
  result2 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    enableOneSampleTest = TRUE,
    test.value = 200
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats respects Bayes Factor message option", {
  devtools::load_all()

  # Without BF message
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "bayes",
    bf.message = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # With BF message
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "bayes",
    bf.message = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats respects bin width customization", {
  devtools::load_all()

  # Automatic
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "tumor_size_mm",
    changebinwidth = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Manual with different widths
  bin_widths <- c(1.0, 2.5, 5.0, 10.0)

  for (bw in bin_widths) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "tumor_size_mm",
      changebinwidth = TRUE,
      binwidth = bw
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for binwidth =", bw))
  }
})

test_that("jjhistostats respects confidence level parameter", {
  devtools::load_all()

  conf_levels <- c(0.80, 0.90, 0.95, 0.99)

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

test_that("jjhistostats respects decimal places parameter", {
  devtools::load_all()

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

test_that("jjhistostats respects custom title parameters", {
  devtools::load_all()

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    title = "Custom Title",
    subtitle = "Custom Subtitle",
    caption = "Custom Caption",
    xlab = "Custom X-axis"
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats respects plot dimension parameters", {
  devtools::load_all()

  dimensions <- list(
    list(width = 300, height = 300),
    list(width = 600, height = 450),
    list(width = 800, height = 600),
    list(width = 1200, height = 800)
  )

  for (dim in dimensions) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "age_years",
      plotwidth = dim$width,
      plotheight = dim$height
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for width =", dim$width, "height =", dim$height))
  }
})

test_that("jjhistostats respects aesthetic parameters", {
  devtools::load_all()

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    binfill = "#0073C2FF",
    bincolor = "#000000",
    binalpha = 0.8
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats respects centrality line aesthetic parameters", {
  devtools::load_all()

  line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

  for (lt in line_types) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "age_years",
      centralityline = TRUE,
      centralitylinecolor = "#E64B35FF",
      centralitylinewidth = 1.5,
      centralitylinetype = lt
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for linetype:", lt))
  }
})

test_that("jjhistostats handles single vs multiple variables", {
  devtools::load_all()

  # Single variable
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years"
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # Two variables
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = c("age_years", "bmi")
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # Four variables
  result3 <- jjhistostats(
    data = jjhistostats_test,
    dep = c("age_years", "bmi", "tumor_size_mm", "hemoglobin")
  )
  expect_s3_class(result3, "jjhistostatsResults")
})

test_that("jjhistostats handles combinations of statistical type and centrality", {
  devtools::load_all()

  combinations <- expand.grid(
    typestatistics = c("parametric", "nonparametric", "robust"),
    centralitytype = c("parametric", "nonparametric", "robust"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- jjhistostats(
      data = jjhistostats_test,
      dep = "age_years",
      typestatistics = combinations$typestatistics[i],
      centralityline = TRUE,
      centralitytype = combinations$centralitytype[i]
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for typestatistics =", combinations$typestatistics[i],
                               "centralitytype =", combinations$centralitytype[i]))
  }
})

test_that("jjhistostats handles comprehensive argument combinations", {
  devtools::load_all()

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "tumor_size_mm",
    typestatistics = "parametric",
    centralityline = TRUE,
    centralitytype = "parametric",
    resultssubtitle = TRUE,
    enableOneSampleTest = TRUE,
    test.value = 30,
    conf.level = 0.95,
    digits = 2,
    changebinwidth = TRUE,
    binwidth = 5.0,
    binfill = "#0073C2FF",
    bincolor = "#000000",
    binalpha = 0.7,
    centralitylinecolor = "#E64B35FF",
    centralitylinewidth = 1.5,
    centralitylinetype = "dashed",
    title = "Comprehensive Test",
    xlab = "Tumor Size (mm)",
    caption = "Test caption",
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles grouped analysis with different statistical types", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjhistostats(
      data = jjhistostats_grouped,
      dep = "biomarker_level",
      grvar = "disease_stage",
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for grouped analysis with typestatistics:", stat_type))
  }
})

test_that("jjhistostats handles grouped analysis with centrality options", {
  devtools::load_all()

  result <- jjhistostats(
    data = jjhistostats_grouped,
    dep = "biomarker_level",
    grvar = "disease_stage",
    typestatistics = "parametric",
    centralityline = TRUE,
    centralitytype = "parametric",
    resultssubtitle = TRUE
  )

  expect_s3_class(result, "jjhistostatsResults")
})

test_that("jjhistostats handles one-sample test with different statistical types", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust")

  for (stat_type in stat_types) {
    result <- jjhistostats(
      data = jjhistostats_labvalues,
      dep = "cholesterol",
      typestatistics = stat_type,
      enableOneSampleTest = TRUE,
      test.value = 200
    )

    expect_s3_class(result, "jjhistostatsResults",
                   info = paste("Failed for one-sample test with typestatistics:", stat_type))
  }
})

test_that("jjhistostats handles ggpubr plot options", {
  devtools::load_all()

  # Without ggpubr plot
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    addGGPubrPlot = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # With ggpubr plot
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    addGGPubrPlot = TRUE,
    ggpubrPalette = "#00AFBB"
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # With density curve
  result3 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    addGGPubrPlot = TRUE,
    ggpubrAddDensity = TRUE
  )
  expect_s3_class(result3, "jjhistostatsResults")

  # With mean line
  result4 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    addGGPubrPlot = TRUE,
    ggpubrAddMean = TRUE
  )
  expect_s3_class(result4, "jjhistostatsResults")
})

test_that("jjhistostats handles distribution diagnostic options", {
  devtools::load_all()

  # Without diagnostics
  result1 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    addDistributionDiagnostics = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # With diagnostics
  result2 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    addDistributionDiagnostics = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")

  # With QQ plot
  result3 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    addDistributionDiagnostics = TRUE,
    ggpubrShowQQ = TRUE
  )
  expect_s3_class(result3, "jjhistostatsResults")

  # With ECDF plot
  result4 <- jjhistostats(
    data = jjhistostats_labvalues,
    dep = "cholesterol",
    addDistributionDiagnostics = TRUE,
    ggpubrShowECDF = TRUE
  )
  expect_s3_class(result4, "jjhistostatsResults")
})

test_that("jjhistostats handles show interpretation option", {
  devtools::load_all()

  # Without interpretation
  result1 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    showInterpretation = FALSE
  )
  expect_s3_class(result1, "jjhistostatsResults")

  # With interpretation
  result2 <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    showInterpretation = TRUE
  )
  expect_s3_class(result2, "jjhistostatsResults")
})

test_that("jjhistostats handles publication-ready configuration", {
  devtools::load_all()

  result <- jjhistostats(
    data = jjhistostats_test,
    dep = "age_years",
    typestatistics = "parametric",
    centralityline = TRUE,
    centralitytype = "parametric",
    resultssubtitle = TRUE,
    binfill = "#0073C2FF",
    bincolor = "#000000",
    binalpha = 0.7,
    centralitylinecolor = "#E64B35FF",
    centralitylinewidth = 1.5,
    centralitylinetype = "dashed",
    title = "Age Distribution in Study Cohort",
    xlab = "Age (years)",
    caption = "n = 150 patients",
    conf.level = 0.95,
    digits = 2,
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjhistostatsResults")
})
