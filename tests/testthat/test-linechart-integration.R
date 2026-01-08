# ═══════════════════════════════════════════════════════════
# Integration Tests: linechart
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("linechart integrates with all test datasets", {
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
                   info = paste("Integration failed for dataset:", ds$name))
    expect_true(!is.null(result$plot1),
               info = paste("Plot missing for dataset:", ds$name))
  }
})

test_that("linechart produces consistent results across data formats", {
  devtools::load_all()

  data(linechart_simple)

  # As data.frame
  df_data <- as.data.frame(linechart_simple)
  result_df <- linechart(
    data = df_data,
    xvar = "time_point",
    yvar = "value"
  )

  # As tibble
  tibble_data <- tibble::as_tibble(linechart_simple)
  result_tibble <- linechart(
    data = tibble_data,
    xvar = "time_point",
    yvar = "value"
  )

  # Both should succeed
  expect_s3_class(result_df, "linechartResults")
  expect_s3_class(result_tibble, "linechartResults")
})

test_that("linechart handles complete clinical trial workflow", {
  devtools::load_all()

  data(linechart_clinical)

  # Step 1: Basic visualization
  result_basic <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage"
  )
  expect_s3_class(result_basic, "linechartResults")

  # Step 2: Add confidence intervals
  result_ci <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage",
    confidence = TRUE
  )
  expect_s3_class(result_ci, "linechartResults")

  # Step 3: Add trend lines
  result_trend <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage",
    confidence = TRUE,
    trendline = TRUE
  )
  expect_s3_class(result_trend, "linechartResults")

  # Step 4: Publication-ready figure
  result_pub <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage",
    confidence = TRUE,
    trendline = TRUE,
    refline = 100,
    reflineLabel = "ULN",
    colorPalette = "clinical",
    theme = "publication",
    xlabel = "Week",
    ylabel = "Tumor Marker (ng/mL)",
    title = "Disease Progression"
  )
  expect_s3_class(result_pub, "linechartResults")
})

test_that("linechart handles exploratory time series workflow", {
  devtools::load_all()

  data(linechart_long)

  # Step 1: Raw data view
  result_raw <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    points = TRUE
  )
  expect_s3_class(result_raw, "linechartResults")

  # Step 2: Apply smoothing
  result_smooth <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    smooth = TRUE,
    points = FALSE
  )
  expect_s3_class(result_smooth, "linechartResults")

  # Step 3: Add trend line
  result_trend <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value",
    trendline = TRUE,
    smooth = TRUE
  )
  expect_s3_class(result_trend, "linechartResults")
})

test_that("linechart handles treatment comparison workflow", {
  devtools::load_all()

  data(linechart_multiple)

  # Step 1: Basic comparison
  result_basic <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention"
  )
  expect_s3_class(result_basic, "linechartResults")

  # Step 2: Add variability measure
  result_var <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention",
    confidence = TRUE
  )
  expect_s3_class(result_var, "linechartResults")

  # Step 3: Add treatment goal
  result_goal <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention",
    confidence = TRUE,
    refline = 120,
    reflineLabel = "Goal"
  )
  expect_s3_class(result_goal, "linechartResults")

  # Step 4: Complete analysis
  result_complete <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention",
    confidence = TRUE,
    trendline = TRUE,
    refline = 120,
    reflineLabel = "Treatment Goal",
    colorPalette = "colorblind",
    theme = "publication"
  )
  expect_s3_class(result_complete, "linechartResults")
})

test_that("linechart handles response assessment workflow", {
  devtools::load_all()

  data(linechart_short)

  # Step 1: Visualize response groups
  result_vis <- linechart(
    data = linechart_short,
    xvar = "month",
    yvar = "measurement",
    groupby = "response_group",
    points = TRUE
  )
  expect_s3_class(result_vis, "linechartResults")

  # Step 2: Quantify differences with trends
  result_trend <- linechart(
    data = linechart_short,
    xvar = "month",
    yvar = "measurement",
    groupby = "response_group",
    trendline = TRUE,
    confidence = TRUE
  )
  expect_s3_class(result_trend, "linechartResults")
})

test_that("linechart handles different time scales consistently", {
  devtools::load_all()

  # Short time series (months)
  data(linechart_short)
  result_short <- linechart(
    data = linechart_short,
    xvar = "month",
    yvar = "measurement",
    groupby = "response_group"
  )
  expect_s3_class(result_short, "linechartResults")

  # Medium time series (weeks)
  data(linechart_clinical)
  result_medium <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "tumor_marker",
    groupby = "disease_stage"
  )
  expect_s3_class(result_medium, "linechartResults")

  # Long time series (days)
  data(linechart_long)
  result_long <- linechart(
    data = linechart_long,
    xvar = "day",
    yvar = "seasonal_value"
  )
  expect_s3_class(result_long, "linechartResults")
})

test_that("linechart handles different group counts consistently", {
  devtools::load_all()

  # Two groups
  data(linechart_short)
  result_two <- linechart(
    data = linechart_short,
    xvar = "month",
    yvar = "measurement",
    groupby = "response_group",
    confidence = TRUE
  )
  expect_s3_class(result_two, "linechartResults")

  # Three groups
  data(linechart_grouped)
  result_three <- linechart(
    data = linechart_grouped,
    xvar = "time_point",
    yvar = "lab_value",
    groupby = "treatment",
    confidence = TRUE
  )
  expect_s3_class(result_three, "linechartResults")

  # Four groups
  data(linechart_multiple)
  result_four <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention",
    confidence = TRUE
  )
  expect_s3_class(result_four, "linechartResults")
})

test_that("linechart handles all color palettes consistently", {
  devtools::load_all()

  data(linechart_grouped)

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
                   info = paste("Integration failed for palette:", pal))
  }
})

test_that("linechart handles all plot themes consistently", {
  devtools::load_all()

  data(linechart_grouped)

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
                   info = paste("Integration failed for theme:", theme))
  }
})

test_that("linechart handles combinations of all features", {
  devtools::load_all()

  data(linechart_clinical)

  # Test comprehensive feature integration
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
    title = "Complete Integration Test",
    width = 1000,
    height = 700
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles irregular intervals with smoothing", {
  devtools::load_all()

  data(linechart_irregular)

  # Irregular intervals benefit from smoothing
  result <- linechart(
    data = linechart_irregular,
    xvar = "time",
    yvar = "value",
    groupby = "obs_type",
    smooth = TRUE,
    confidence = TRUE,
    colorPalette = "viridis"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles pattern comparison workflow", {
  devtools::load_all()

  data(linechart_patterns)

  # Step 1: Visual comparison
  result_visual <- linechart(
    data = linechart_patterns,
    xvar = "time_index",
    yvar = "value",
    groupby = "pattern_type"
  )
  expect_s3_class(result_visual, "linechartResults")

  # Step 2: Quantify with trend lines
  result_quant <- linechart(
    data = linechart_patterns,
    xvar = "time_index",
    yvar = "value",
    groupby = "pattern_type",
    trendline = TRUE
  )
  expect_s3_class(result_quant, "linechartResults")

  # Step 3: Complete analysis
  result_complete <- linechart(
    data = linechart_patterns,
    xvar = "time_index",
    yvar = "value",
    groupby = "pattern_type",
    confidence = TRUE,
    trendline = TRUE,
    colorPalette = "colorblind",
    theme = "publication"
  )
  expect_s3_class(result_complete, "linechartResults")
})

test_that("linechart handles progressive feature addition", {
  devtools::load_all()

  data(linechart_grouped)

  # Progressive feature addition
  features_list <- list(
    list(),
    list(confidence = TRUE),
    list(confidence = TRUE, trendline = TRUE),
    list(confidence = TRUE, trendline = TRUE, points = TRUE),
    list(confidence = TRUE, trendline = TRUE, points = TRUE, refline = 100)
  )

  for (i in seq_along(features_list)) {
    args <- c(
      list(
        data = linechart_grouped,
        xvar = "time_point",
        yvar = "lab_value",
        groupby = "treatment"
      ),
      features_list[[i]]
    )

    result <- do.call(linechart, args)

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed for feature set", i))
  }
})

test_that("linechart handles all datasets with consistent parameters", {
  devtools::load_all()

  datasets <- list(
    list(name = "linechart_grouped", x = "time_point", y = "lab_value", group = "treatment"),
    list(name = "linechart_clinical", x = "week", y = "tumor_marker", group = "disease_stage"),
    list(name = "linechart_irregular", x = "time", y = "value", group = "obs_type"),
    list(name = "linechart_multiple", x = "visit", y = "systolic_bp", group = "intervention"),
    list(name = "linechart_patterns", x = "time_index", y = "value", group = "pattern_type")
  )

  # Apply same parameters to all datasets
  for (ds in datasets) {
    data(list = ds$name, package = "ClinicoPath")
    dataset <- get(ds$name)

    result <- linechart(
      data = dataset,
      xvar = ds$x,
      yvar = ds$y,
      groupby = ds$group,
      confidence = TRUE,
      trendline = TRUE,
      colorPalette = "colorblind"
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Consistent parameters failed for:", ds$name))
  }
})

test_that("linechart output structure is consistent across all datasets", {
  devtools::load_all()

  datasets <- c("linechart_simple", "linechart_grouped", "linechart_clinical")

  for (dataset_name in datasets) {
    data(list = dataset_name, package = "ClinicoPath")
    dataset <- get(dataset_name)

    xvar <- switch(dataset_name,
                  "linechart_simple" = "time_point",
                  "linechart_grouped" = "time_point",
                  "linechart_clinical" = "week")

    yvar <- switch(dataset_name,
                  "linechart_simple" = "value",
                  "linechart_grouped" = "lab_value",
                  "linechart_clinical" = "tumor_marker")

    groupby <- switch(dataset_name,
                     "linechart_simple" = NULL,
                     "linechart_grouped" = "treatment",
                     "linechart_clinical" = "disease_stage")

    if (is.null(groupby)) {
      result <- linechart(
        data = dataset,
        xvar = xvar,
        yvar = yvar
      )
    } else {
      result <- linechart(
        data = dataset,
        xvar = xvar,
        yvar = yvar,
        groupby = groupby
      )
    }

    # Check consistent output structure
    expect_s3_class(result, "linechartResults")
    expect_true(!is.null(result$plot1),
               info = paste("Plot missing for:", dataset_name))
  }
})

test_that("linechart handles repeated analyses with same data", {
  devtools::load_all()

  data(linechart_simple)

  # Run same analysis multiple times
  for (i in 1:5) {
    result <- linechart(
      data = linechart_simple,
      xvar = "time_point",
      yvar = "value",
      confidence = TRUE
    )

    expect_s3_class(result, "linechartResults",
                   info = paste("Failed on iteration", i))
  }
})

test_that("linechart handles biomarker monitoring workflow", {
  devtools::load_all()

  data(linechart_clinical)

  # Monitor WBC count with reference line
  result <- linechart(
    data = linechart_clinical,
    xvar = "week",
    yvar = "wbc_count",
    groupby = "disease_stage",
    confidence = TRUE,
    refline = 4.5,
    reflineLabel = "Lower Limit Normal",
    colorPalette = "clinical",
    theme = "publication",
    xlabel = "Week",
    ylabel = "WBC Count (×10⁹/L)",
    title = "White Blood Cell Monitoring"
  )

  expect_s3_class(result, "linechartResults")
})

test_that("linechart handles multiple biomarker workflow", {
  devtools::load_all()

  data(linechart_multiple)

  # Step 1: Blood pressure
  result_bp <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "systolic_bp",
    groupby = "intervention",
    confidence = TRUE,
    refline = 120
  )
  expect_s3_class(result_bp, "linechartResults")

  # Step 2: Cholesterol
  result_chol <- linechart(
    data = linechart_multiple,
    xvar = "visit",
    yvar = "cholesterol",
    groupby = "intervention",
    confidence = TRUE,
    refline = 200
  )
  expect_s3_class(result_chol, "linechartResults")
})
