# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: linechart
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(linechart_simple)
data(linechart_grouped)
data(linechart_clinical)

test_that("linechart respects all feature combinations", {
  devtools::load_all()

  combinations <- expand.grid(
    confidence = c(TRUE, FALSE),
    trendline = c(TRUE, FALSE),
    points = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- linechart(
      data = linechart_grouped,
      xvar = "time_point",
      yvar = "lab_value",
      groupby = "treatment",
      confidence = combinations$confidence[i],
      trendline = combinations$trendline[i],
      points = combinations$points[i]
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for combination", i))
  }
})

test_that("linechart handles all color palette options", {
  devtools::load_all()

  palettes <- c("default", "colorblind", "viridis", "clinical")

  for (pal in palettes) {
    result <- linechart(
      data = linechart_grouped,
      xvar = "time_point",
      yvar = "lab_value",
      groupby = "treatment",
      colorPalette = pal
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for palette:", pal))
  }
})

test_that("linechart handles all theme options", {
  devtools::load_all()

  themes <- c("default", "minimal", "classic", "publication")

  for (theme in themes) {
    result <- linechart(
      data = linechart_grouped,
      xvar = "time_point",
      yvar = "lab_value",
      groupby = "treatment",
      theme = theme
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for theme:", theme))
  }
})

test_that("linechart handles theme and palette combinations", {
  devtools::load_all()

  combinations <- expand.grid(
    theme = c("default", "minimal", "publication"),
    colorPalette = c("default", "colorblind", "clinical"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- linechart(
      data = linechart_grouped,
      xvar = "time_point",
      yvar = "lab_value",
      groupby = "treatment",
      theme = combinations$theme[i],
      colorPalette = combinations$colorPalette[i]
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for theme:", combinations$theme[i],
                               "palette:", combinations$colorPalette[i]))
  }
})

test_that("linechart handles confidence intervals with different options", {
  devtools::load_all()

  # CI without grouping
  result1 <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    confidence = TRUE
  )
  expect_s3_class(result1, "linechartResults")

  # CI with grouping
  result2 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    confidence = TRUE
  )
  expect_s3_class(result2, "linechartResults")

  # CI with trend line
  result3 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    confidence = TRUE,
    trendline = TRUE
  )
  expect_s3_class(result3, "linechartResults")
})

test_that("linechart handles trend line with different options", {
  devtools::load_all()

  # Trend without grouping
  result1 <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    trendline = TRUE
  )
  expect_s3_class(result1, "linechartResults")

  # Trend with grouping
  result2 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    trendline = TRUE
  )
  expect_s3_class(result2, "linechartResults")

  # Trend with smoothing
  result3 <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    trendline = TRUE,
    smooth = TRUE
  )
  expect_s3_class(result3, "linechartResults")
})

test_that("linechart handles smoothing with different options", {
  devtools::load_all()

  # Smoothing without grouping
  result1 <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    smooth = TRUE
  )
  expect_s3_class(result1, "linechartResults")

  # Smoothing with grouping
  result2 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    smooth = TRUE
  )
  expect_s3_class(result2, "linechartResults")

  # Smoothing with points
  result3 <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    smooth = TRUE,
    points = TRUE
  )
  expect_s3_class(result3, "linechartResults")

  # Smoothing without points
  result4 <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    smooth = TRUE,
    points = FALSE
  )
  expect_s3_class(result4, "linechartResults")
})

test_that("linechart handles reference line with different parameters", {
  devtools::load_all()

  # Reference line alone
  result1 <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    refline = 100
  )
  expect_s3_class(result1, "linechartResults")

  # Reference line with custom label
  result2 <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    refline = 100,
    reflineLabel = "Upper Limit Normal"
  )
  expect_s3_class(result2, "linechartResults")

  # Reference line with grouping
  result3 <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage",
    refline = 100,
    reflineLabel = "ULN"
  )
  expect_s3_class(result3, "linechartResults")
})

test_that("linechart handles custom labels with different combinations", {
  devtools::load_all()

  # Title only
  result1 <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    title = "Custom Title"
  )
  expect_s3_class(result1, "linechartResults")

  # Axis labels only
  result2 <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    xlabel = "Time",
    ylabel = "Value"
  )
  expect_s3_class(result2, "linechartResults")

  # All labels
  result3 <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    xlabel = "Time",
    ylabel = "Value",
    title = "Complete Labels"
  )
  expect_s3_class(result3, "linechartResults")
})

test_that("linechart handles plot dimension parameters", {
  devtools::load_all()

  dimensions <- list(
    list(width = 400, height = 400),
    list(width = 600, height = 450),
    list(width = 800, height = 600),
    list(width = 1200, height = 900)
  )

  for (dim in dimensions) {
    result <- linechart(
      data = linechart_simple,
      xvar = "time_point",
      yvar = "value",
      width = dim$width,
      height = dim$height
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for width =", dim$width, "height =", dim$height))
  }
})

test_that("linechart handles comprehensive argument combinations", {
  devtools::load_all()

  result <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage",
    confidence = TRUE,
    trendline = TRUE,
    points = TRUE,
    refline = 100,
    reflineLabel = "ULN",
    colorPalette = "clinical",
    theme = "publication",
    xlabel = "Week",
    ylabel = "Tumor Marker (ng/mL)",
    title = "Disease Progression",
    width = 1000,
    height = 700
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles minimal configuration", {
  devtools::load_all()

  result <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    points = FALSE,
    confidence = FALSE,
    trendline = FALSE
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles maximal configuration", {
  devtools::load_all()

  result <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    confidence = TRUE,
    trendline = TRUE,
    points = TRUE,
    smooth = TRUE,
    refline = 100,
    reflineLabel = "Reference",
    colorPalette = "viridis",
    theme = "publication",
    xlabel = "Time",
    ylabel = "Value",
    title = "Maximal Configuration"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles different group counts with same parameters", {
  devtools::load_all()

  # Two groups
  result2 <- linechart(
    data = linechart_short,
    xvar = "month",
    yvar = "measurement",
    groupby = "response_group",
    confidence = TRUE,
    trendline = TRUE
  )
  expect_s3_class(result2, "linechartResults")

  # Three groups
  result3 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    confidence = TRUE,
    trendline = TRUE
  )
  expect_s3_class(result3, "linechartResults")

  # Four groups
  result4 <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention",
    confidence = TRUE,
    trendline = TRUE
  )
  expect_s3_class(result4, "linechartResults")
})

test_that("linechart handles factor vs character grouping variables", {
  devtools::load_all()

  # Factor group variable
  test_data_factor <- linechart_grouped
  test_data_factor$treatment <- as.factor(test_data_factor$treatment)

  result_factor <- linechart(
    data = test_data_factor,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment"
  )
  expect_s3_class(result_factor, "linechartResults")

  # Character group variable
  test_data_char <- linechart_grouped
  test_data_char$treatment <- as.character(test_data_char$treatment)

  result_char <- linechart(
    data = test_data_char,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment"
  )
  expect_s3_class(result_char, "linechartResults")
})

test_that("linechart handles publication-ready configuration", {
  devtools::load_all()

  result <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention",
    confidence = TRUE,
    trendline = TRUE,
    points = TRUE,
    refline = 120,
    reflineLabel = "Treatment Goal",
    colorPalette = "colorblind",
    theme = "publication",
    xlabel = "Study Visit",
    ylabel = "Systolic Blood Pressure (mmHg)",
    title = "Antihypertensive Drug Comparison",
    width = 1200,
    height = 800
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles all features for short time series", {
  devtools::load_all()

  result <- linechart(
    data = linechart_short,
    xvar = "month",
    yvar = "measurement",
    groupby = "response_group",
    confidence = TRUE,
    trendline = TRUE,
    points = TRUE,
    colorPalette = "colorblind",
    xlabel = "Month",
    ylabel = "Measurement",
    title = "Short Series with All Features"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles all features for long time series", {
  devtools::load_all()

  result <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    smooth = TRUE,
    points = FALSE,
    theme = "minimal",
    xlabel = "Day of Year",
    ylabel = "Value",
    title = "Long Series with Smoothing"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles irregular intervals with all options", {
  devtools::load_all()

  result <- linechart(
    data = linechart_irregular,
    xvar = "time",
    yvar = "value",
    groupby = "obs_type",
    smooth = TRUE,
    confidence = TRUE,
    colorPalette = "viridis",
    xlabel = "Time (irregular)",
    ylabel = "Value",
    title = "Irregular Time Series"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles different patterns with consistent settings", {
  devtools::load_all()

  result <- linechart(
    data = linechart_patterns,
    xvar = "time_index",
    yvar = "value",
    groupby = "pattern_type",
    confidence = TRUE,
    trendline = TRUE,
    colorPalette = "clinical",
    theme = "publication",
    xlabel = "Time Index",
    ylabel = "Value",
    title = "Trend Pattern Comparison"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles all palettes with grouped data", {
  devtools::load_all()

  palettes <- c("default", "colorblind", "viridis", "clinical")

  for (pal in palettes) {
    result <- linechart(
      data = linechart_grouped,
      xvar = "time_point",
      yvar = "lab_value",
      groupby = "treatment",
      confidence = TRUE,
      colorPalette = pal
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for palette:", pal))
  }
})

test_that("linechart handles all themes with grouped data", {
  devtools::load_all()

  themes <- c("default", "minimal", "classic", "publication")

  for (theme in themes) {
    result <- linechart(
      data = linechart_grouped,
      xvar = "time_point",
      yvar = "lab_value",
      groupby = "treatment",
      confidence = TRUE,
      theme = theme
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for theme:", theme))
  }
})
