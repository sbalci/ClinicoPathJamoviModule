# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjbarstats
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjbarstats handles all statistical types", {
  devtools::load_all()

  data(jjbarstats_test)

  statistics_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in statistics_types) {
    result <- jjbarstats(
      data = jjbarstats_test,
      dep = "response",
      group = "treatment",
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjbarstatsResults",
                   info = paste("Failed for typestatistics:", stat_type))
  }
})

test_that("jjbarstats handles all clinical presets", {
  devtools::load_all()

  # Custom preset
  data(jjbarstats_test)
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    clinicalpreset = "custom"
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # Diagnostic preset
  data(jjbarstats_diagnostic)
  result2 <- jjbarstats(
    data = jjbarstats_diagnostic,
    dep = "diagnosis",
    group = "test_result",
    clinicalpreset = "diagnostic"
  )
  expect_s3_class(result2, "jjbarstatsResults")

  # Treatment preset
  data(jjbarstats_test)
  result3 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    clinicalpreset = "treatment"
  )
  expect_s3_class(result3, "jjbarstatsResults")

  # Biomarker preset
  data(jjbarstats_biomarker)
  result4 <- jjbarstats(
    data = jjbarstats_biomarker,
    dep = "her2_status",
    group = "subtype",
    clinicalpreset = "biomarker"
  )
  expect_s3_class(result4, "jjbarstatsResults")

  # Risk factor preset
  data(jjbarstats_test)
  result5 <- jjbarstats(
    data = jjbarstats_test,
    dep = "disease_status",
    group = "smoking_status",
    clinicalpreset = "riskfactor"
  )
  expect_s3_class(result5, "jjbarstatsResults")
})

test_that("jjbarstats handles all p-value adjustment methods", {
  devtools::load_all()

  data(jjbarstats_test)

  adjust_methods <- c("holm", "hochberg", "hommel", "bonferroni",
                     "BH", "BY", "fdr", "none")

  for (method in adjust_methods) {
    result <- jjbarstats(
      data = jjbarstats_test,
      dep = "response",
      group = "treatment",
      pairwisecomparisons = TRUE,
      padjustmethod = method
    )

    expect_s3_class(result, "jjbarstatsResults",
                   info = paste("Failed for padjustmethod:", method))
  }
})

test_that("jjbarstats handles all pairwise display modes", {
  devtools::load_all()

  data(jjbarstats_test)

  display_modes <- c("significant", "non-significant", "everything")

  for (mode in display_modes) {
    result <- jjbarstats(
      data = jjbarstats_test,
      dep = "response",
      group = "treatment",
      pairwisecomparisons = TRUE,
      pairwisedisplay = mode
    )

    expect_s3_class(result, "jjbarstatsResults",
                   info = paste("Failed for pairwisedisplay:", mode))
  }
})

test_that("jjbarstats handles split-by variable combinations", {
  devtools::load_all()

  data(jjbarstats_test)

  split_vars <- c("sex", "age_group", "tumor_stage")

  for (grvar in split_vars) {
    result <- jjbarstats(
      data = jjbarstats_test,
      dep = "response",
      group = "treatment",
      grvar = grvar
    )

    expect_s3_class(result, "jjbarstatsResults",
                   info = paste("Failed for grvar:", grvar))
  }
})

test_that("jjbarstats handles proportion test with expected ratios", {
  devtools::load_all()

  data(jjbarstats_test)

  # Equal proportions (3 groups)
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    proportiontest = TRUE,
    ratio = "0.333,0.333,0.334"
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # Unequal expected proportions
  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    proportiontest = TRUE,
    ratio = "0.5,0.3,0.2"
  )
  expect_s3_class(result2, "jjbarstatsResults")

  # Empty ratio (equal proportions by default)
  result3 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    proportiontest = TRUE,
    ratio = ""
  )
  expect_s3_class(result3, "jjbarstatsResults")
})

test_that("jjbarstats handles confidence level variations", {
  devtools::load_all()

  data(jjbarstats_test)

  conf_levels <- c(0.90, 0.95, 0.99)

  for (conf in conf_levels) {
    result <- jjbarstats(
      data = jjbarstats_test,
      dep = "response",
      group = "treatment",
      conflevel = conf
    )

    expect_s3_class(result, "jjbarstatsResults",
                   info = paste("Failed for conflevel:", conf))
  }
})

test_that("jjbarstats handles digit precision options", {
  devtools::load_all()

  data(jjbarstats_test)

  # Different digits for statistics
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    digits = 0
  )
  expect_s3_class(result1, "jjbarstatsResults")

  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    digits = 3
  )
  expect_s3_class(result2, "jjbarstatsResults")

  # Different digits for percentages
  result3 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    digitsperc = 0
  )
  expect_s3_class(result3, "jjbarstatsResults")

  result4 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    digitsperc = 2
  )
  expect_s3_class(result4, "jjbarstatsResults")
})

test_that("jjbarstats handles output display options", {
  devtools::load_all()

  data(jjbarstats_test)

  # All outputs shown
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    showSummary = TRUE,
    showAssumptions = TRUE,
    showInterpretation = TRUE,
    showexplanations = TRUE
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # All outputs hidden
  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    showSummary = FALSE,
    showAssumptions = FALSE,
    showInterpretation = FALSE,
    showexplanations = FALSE
  )
  expect_s3_class(result2, "jjbarstatsResults")
})

test_that("jjbarstats handles theme and styling options", {
  devtools::load_all()

  data(jjbarstats_test)

  # Original GGStatsPlot theme
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    originaltheme = TRUE
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # Show results in subtitle
  result2 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    resultssubtitle = TRUE
  )
  expect_s3_class(result2, "jjbarstatsResults")

  # Bayes Factor message
  result3 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    typestatistics = "parametric",
    bfmessage = TRUE
  )
  expect_s3_class(result3, "jjbarstatsResults")
})

test_that("jjbarstats handles balloon plot options", {
  devtools::load_all()

  data(jjbarstats_test)

  palettes <- c("jco", "lancet", "grey")

  for (palette in palettes) {
    result <- jjbarstats(
      data = jjbarstats_test,
      dep = "response",
      group = "treatment",
      addGGPubrBalloon = TRUE,
      ggpubrBalloonPalette = palette
    )

    expect_s3_class(result, "jjbarstatsResults",
                   info = paste("Failed for balloon palette:", palette))
  }
})

test_that("jjbarstats handles comprehensive parameter combinations", {
  devtools::load_all()

  data(jjbarstats_test)

  # Full feature set
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    grvar = "sex",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    padjustmethod = "bonferroni",
    pairwisedisplay = "significant",
    clinicalpreset = "treatment",
    label = "both",
    digits = 2,
    digitsperc = 1,
    conflevel = 0.95,
    proportiontest = TRUE,
    showSummary = TRUE,
    showAssumptions = TRUE,
    showInterpretation = TRUE,
    originaltheme = FALSE,
    resultssubtitle = FALSE
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles NULL optional parameters", {
  devtools::load_all()

  data(jjbarstats_test)

  # Explicit NULL for optional parameters
  result <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment",
    grvar = NULL,
    counts = NULL,
    ratio = NULL
  )

  expect_s3_class(result, "jjbarstatsResults")
})

test_that("jjbarstats handles factor vs character variables", {
  devtools::load_all()

  data(jjbarstats_test)

  # Convert to character
  char_data <- jjbarstats_test
  char_data$response <- as.character(char_data$response)
  char_data$treatment <- as.character(char_data$treatment)

  result_char <- jjbarstats(
    data = char_data,
    dep = "response",
    group = "treatment"
  )

  # Keep as factor
  result_factor <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment"
  )

  # Both should work
  expect_s3_class(result_char, "jjbarstatsResults")
  expect_s3_class(result_factor, "jjbarstatsResults")
})

test_that("jjbarstats handles different dataset types", {
  devtools::load_all()

  # Comprehensive dataset
  data(jjbarstats_test)
  result1 <- jjbarstats(
    data = jjbarstats_test,
    dep = "response",
    group = "treatment"
  )
  expect_s3_class(result1, "jjbarstatsResults")

  # Diagnostic dataset
  data(jjbarstats_diagnostic)
  result2 <- jjbarstats(
    data = jjbarstats_diagnostic,
    dep = "diagnosis",
    group = "test_result"
  )
  expect_s3_class(result2, "jjbarstatsResults")

  # Paired dataset
  data(jjbarstats_paired)
  result3 <- jjbarstats(
    data = jjbarstats_paired,
    dep = "baseline_status",
    group = "followup_status",
    paired = TRUE
  )
  expect_s3_class(result3, "jjbarstatsResults")

  # Aggregated dataset
  data(jjbarstats_aggregated)
  result4 <- jjbarstats(
    data = jjbarstats_aggregated,
    dep = "response_category",
    group = "treatment_group",
    counts = "count"
  )
  expect_s3_class(result4, "jjbarstatsResults")

  # Biomarker dataset
  data(jjbarstats_biomarker)
  result5 <- jjbarstats(
    data = jjbarstats_biomarker,
    dep = "her2_status",
    group = "subtype"
  )
  expect_s3_class(result5, "jjbarstatsResults")
})
