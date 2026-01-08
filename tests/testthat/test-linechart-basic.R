# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: linechart
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("linechart function exists and loads", {
  devtools::load_all()

  expect_true(exists("linechart"))
})

test_that("linechart runs with minimal required arguments", {
  devtools::load_all()

  data(linechart_simple, package = "ClinicoPath")

  # Minimal required arguments (xvar, yvar)
  result <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart produces expected output structure", {
  devtools::load_all()

  data(linechart_simple)

  result <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value"
  )

  # Check for main plot output
  expect_true(!is.null(result$plot1))
})

test_that("linechart handles single line visualization", {
  devtools::load_all()

  data(linechart_simple)

  result <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles grouped visualization", {
  devtools::load_all()

  data(linechart_grouped)

  result <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles three groups", {
  devtools::load_all()

  data(linechart_grouped)

  result <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles four groups", {
  devtools::load_all()

  data(linechart_multiple)

  result <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles confidence intervals", {
  devtools::load_all()

  data(linechart_grouped)

  # Without confidence
  result1 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    confidence = FALSE
  )
  expect_s3_class(result1, "linechartResults")

  # With confidence
  result2 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    confidence = TRUE
  )
  expect_s3_class(result2, "linechartResults")
})

test_that("linechart handles trend lines", {
  devtools::load_all()

  data(linechart_grouped)

  # Without trend line
  result1 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    trendline = FALSE
  )
  expect_s3_class(result1, "linechartResults")

  # With trend line
  result2 <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    trendline = TRUE
  )
  expect_s3_class(result2, "linechartResults")
})

test_that("linechart handles point display", {
  devtools::load_all()

  data(linechart_simple)

  # With points
  result1 <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    points = TRUE
  )
  expect_s3_class(result1, "linechartResults")

  # Without points
  result2 <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    points = FALSE
  )
  expect_s3_class(result2, "linechartResults")
})

test_that("linechart handles smoothing", {
  devtools::load_all()

  data(linechart_long)

  # Without smoothing
  result1 <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    smooth = FALSE
  )
  expect_s3_class(result1, "linechartResults")

  # With smoothing
  result2 <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    smooth = TRUE
  )
  expect_s3_class(result2, "linechartResults")
})

test_that("linechart handles reference line", {
  devtools::load_all()

  data(linechart_clinical)

  # Without reference line
  result1 <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage"
  )
  expect_s3_class(result1, "linechartResults")

  # With reference line
  result2 <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage",
    refline = 100,
    reflineLabel = "Normal Range"
  )
  expect_s3_class(result2, "linechartResults")
})

test_that("linechart handles color palettes", {
  devtools::load_all()

  data(linechart_grouped)

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

test_that("linechart handles plot themes", {
  devtools::load_all()

  data(linechart_grouped)

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

test_that("linechart handles custom labels", {
  devtools::load_all()

  data(linechart_simple)

  result <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    xlabel = "Time Point",
    ylabel = "Value",
    title = "Custom Title"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles plot dimensions", {
  devtools::load_all()

  data(linechart_simple)

  result <- linechart(
    data = linechart_simple,
    xvar = "time_point",
    yvar = "value",
    width = 1000,
    height = 700
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles all test datasets", {
  devtools::load_all()

  datasets <- list(
    list(name = "linechart_simple", x = "time_point", y = "value", group = NULL),
    list(name = "linechart_grouped", x = "time_point", y = "lab_value", group = "treatment"),
    list(name = "linechart_clinical", x = "week", y = "tumor_marker", group = "disease_stage"),
    list(name = "linechart_short", x = "month", y = "measurement", group = "response_group"),
    list(name = "linechart_long", x = "day", y = "seasonal_value", group = NULL),
    list(name = "linechart_irregular", x = "time", y = "value", group = "obs_type"),
    list(name = "linechart_multiple", x = "visit", y = "systolic_bp", group = "intervention"),
    list(name = "linechart_patterns", x = "time_index", y = "value", group = "pattern_type")
  )

  for (ds in datasets) {
    data(list = ds$name, package = "ClinicoPath")
    dataset <- get(ds$name)

    if (is.null(ds$group)) {
      result <- linechart(
        data = dataset,
        xvar = ds$x,
        yvar = ds$y
      )
    } else {
      result <- linechart(
        data = dataset,
        xvar = ds$x,
        yvar = ds$y,
        groupby = ds$group
      )
    }

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for dataset:", ds$name))
  }
})

test_that("linechart handles short time series", {
  devtools::load_all()

  data(linechart_short)

  result <- linechart(
    data = linechart_short,
    xvar = "month",
    yvar = "measurement",
    groupby = "response_group"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles long time series", {
  devtools::load_all()

  data(linechart_long)

  result <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "cyclic_value"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles irregular time intervals", {
  devtools::load_all()

  data(linechart_irregular)

  result <- linechart(
    data = linechart_irregular,
    xvar = "time",
    yvar = "value",
    groupby = "obs_type"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles multiple measurements per time point", {
  devtools::load_all()

  data(linechart_multiple)

  result <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention",
    confidence = TRUE
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles different trend patterns", {
  devtools::load_all()

  data(linechart_patterns)

  result <- linechart(
    data = linechart_patterns,
    xvar = "time_index",
    yvar = "value",
    groupby = "pattern_type",
    trendline = TRUE
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles clinical measurements", {
  devtools::load_all()

  data(linechart_clinical)

  result <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "wbc_count",
    groupby = "disease_stage",
    refline = 4.5,
    reflineLabel = "Lower Limit Normal"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles comprehensive feature combination", {
  devtools::load_all()

  data(linechart_grouped)

  result <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    confidence = TRUE,
    trendline = TRUE,
    points = TRUE,
    colorPalette = "colorblind",
    theme = "publication",
    xlabel = "Time Point",
    ylabel = "Lab Value",
    title = "Comprehensive Test"
  )

  expect_s3_class(result, "linechartResults")
})
