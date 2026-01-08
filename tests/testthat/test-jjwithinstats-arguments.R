# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjwithinstats
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(jjwithinstats_test)
data(jjwithinstats_paired)
data(jjwithinstats_biomarker)
data(jjwithinstats_laboratory)

test_that("jjwithinstats respects all statistical type options", {
  devtools::load_all()

  stat_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (stat_type in stat_types) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      typestatistics = stat_type
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for typestatistics:", stat_type))
  }
})

test_that("jjwithinstats respects all pairwise adjustment methods", {
  devtools::load_all()

  adjust_methods <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")

  for (method in adjust_methods) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      pairwisecomparisons = TRUE,
      padjustmethod = method
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for padjustmethod:", method))
  }
})

test_that("jjwithinstats respects all centrality types", {
  devtools::load_all()

  centrality_types <- c("parametric", "nonparametric", "robust", "bayes")

  for (cent_type in centrality_types) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      centralityplotting = TRUE,
      centralitytype = cent_type
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for centralitytype:", cent_type))
  }
})

test_that("jjwithinstats respects all clinical presets", {
  devtools::load_all()

  presets <- c("custom", "biomarker", "treatment", "laboratory")

  for (preset in presets) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      clinicalpreset = preset
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for clinicalpreset:", preset))
  }
})

test_that("jjwithinstats respects all ggpubr plot types", {
  devtools::load_all()

  plot_types <- c("boxplot", "violin", "paired")

  for (plot_type in plot_types) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      addGGPubrPlot = TRUE,
      ggpubrPlotType = plot_type
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for ggpubrPlotType:", plot_type))
  }
})

test_that("jjwithinstats respects all ggpubr palettes", {
  devtools::load_all()

  palettes <- c("jco", "npg", "aaas", "lancet", "jama", "nejm", "grey", "default")

  for (palette in palettes) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      addGGPubrPlot = TRUE,
      ggpubrPalette = palette
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for ggpubrPalette:", palette))
  }
})

test_that("jjwithinstats respects point path options", {
  devtools::load_all()

  # With point paths
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    pointpath = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Without point paths
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    pointpath = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats respects centrality path options", {
  devtools::load_all()

  # With centrality path
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    centralityplotting = TRUE,
    centralitypath = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Without centrality path
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    centralityplotting = TRUE,
    centralitypath = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats respects plot element toggles", {
  devtools::load_all()

  # Test violin, boxplot, point combinations
  combinations <- list(
    list(violin = TRUE, boxplot = FALSE, point = FALSE),
    list(violin = FALSE, boxplot = TRUE, point = FALSE),
    list(violin = FALSE, boxplot = FALSE, point = TRUE),
    list(violin = TRUE, boxplot = TRUE, point = FALSE),
    list(violin = TRUE, boxplot = FALSE, point = TRUE),
    list(violin = FALSE, boxplot = TRUE, point = TRUE),
    list(violin = TRUE, boxplot = TRUE, point = TRUE)
  )

  for (combo in combinations) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      violin = combo$violin,
      boxplot = combo$boxplot,
      point = combo$point
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for combination: violin =", combo$violin,
                               "boxplot =", combo$boxplot, "point =", combo$point))
  }
})

test_that("jjwithinstats respects ggpubr statistical comparison options", {
  devtools::load_all()

  # With statistics
  result1 <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    addGGPubrPlot = TRUE,
    ggpubrAddStats = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Without statistics
  result2 <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    addGGPubrPlot = TRUE,
    ggpubrAddStats = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats respects ggpubr point overlay options", {
  devtools::load_all()

  # With points
  result1 <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    addGGPubrPlot = TRUE,
    ggpubrAddPoints = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Without points
  result2 <- jjwithinstats(
    data = jjwithinstats_paired,
    dep1 = "pre_treatment",
    dep2 = "post_treatment",
    addGGPubrPlot = TRUE,
    ggpubrAddPoints = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats respects ggpubr line display options", {
  devtools::load_all()

  # With lines
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "paired",
    ggpubrShowLines = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Without lines
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "paired",
    ggpubrShowLines = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats respects colorblind-safe palette option", {
  devtools::load_all()

  # Colorblind-safe
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    colorblindSafe = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Standard palette
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    colorblindSafe = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats respects Bayes Factor message option", {
  devtools::load_all()

  # With BF message
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "bayes",
    bfmessage = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Without BF message
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "bayes",
    bfmessage = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats respects results subtitle option", {
  devtools::load_all()

  # With subtitle
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    resultssubtitle = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Without subtitle
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    resultssubtitle = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats respects decimal places parameter", {
  devtools::load_all()

  k_values <- c(0, 1, 2, 3, 4, 5)

  for (k in k_values) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      k = k
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for k =", k))
  }
})

test_that("jjwithinstats respects confidence level parameter", {
  devtools::load_all()

  conf_levels <- c(0.90, 0.95, 0.99)

  for (conf in conf_levels) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      conflevel = conf
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for conflevel =", conf))
  }
})

test_that("jjwithinstats respects plot dimension parameters", {
  devtools::load_all()

  dimensions <- list(
    list(width = 300, height = 300),
    list(width = 650, height = 450),
    list(width = 1200, height = 800)
  )

  for (dim in dimensions) {
    result <- jjwithinstats(
      data = jjwithinstats_test,
      dep1 = "baseline",
      dep2 = "week4",
      dep3 = "week12",
      plotwidth = dim$width,
      plotheight = dim$height
    )

    expect_s3_class(result, "jjwithinstatsResults",
                   info = paste("Failed for width =", dim$width, "height =", dim$height))
  }
})

test_that("jjwithinstats respects custom title parameters", {
  devtools::load_all()

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    mytitle = "Custom Main Title",
    xtitle = "Custom X Title",
    ytitle = "Custom Y Title"
  )

  expect_s3_class(result, "jjwithinstatsResults")
})

test_that("jjwithinstats respects show explanations option", {
  devtools::load_all()

  # With explanations
  result1 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    showexplanations = TRUE
  )
  expect_s3_class(result1, "jjwithinstatsResults")

  # Without explanations
  result2 <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    showexplanations = FALSE
  )
  expect_s3_class(result2, "jjwithinstatsResults")
})

test_that("jjwithinstats handles comprehensive argument combinations", {
  devtools::load_all()

  result <- jjwithinstats(
    data = jjwithinstats_test,
    dep1 = "baseline",
    dep2 = "week4",
    dep3 = "week12",
    typestatistics = "parametric",
    pairwisecomparisons = TRUE,
    padjustmethod = "bonferroni",
    centralityplotting = TRUE,
    centralitytype = "parametric",
    pointpath = TRUE,
    centralitypath = TRUE,
    violin = TRUE,
    boxplot = TRUE,
    point = TRUE,
    addGGPubrPlot = TRUE,
    ggpubrPlotType = "paired",
    ggpubrPalette = "jco",
    ggpubrAddStats = TRUE,
    ggpubrShowLines = TRUE,
    colorblindSafe = FALSE,
    resultssubtitle = TRUE,
    showexplanations = TRUE,
    mytitle = "Comprehensive Test",
    xtitle = "Time",
    ytitle = "Value",
    k = 2,
    conflevel = 0.95,
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "jjwithinstatsResults")
})
