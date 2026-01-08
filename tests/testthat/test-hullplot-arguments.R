# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: hullplot
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(hullplot_test)
data(hullplot_clusters)
data(hullplot_clinical)

test_that("hullplot respects all hull aesthetic parameters", {
  devtools::load_all()

  combinations <- expand.grid(
    hull_concavity = c(0.5, 1.5),
    hull_alpha = c(0.2, 0.5),
    hull_expand = c(0.05, 0.1),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_concavity = combinations$hull_concavity[i],
      hull_alpha = combinations$hull_alpha[i],
      hull_expand = combinations$hull_expand[i]
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for combination", i))
  }
})

test_that("hullplot respects all point aesthetic parameters", {
  devtools::load_all()

  combinations <- expand.grid(
    point_size = c(2, 3),
    point_alpha = c(0.6, 0.9),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      point_size = combinations$point_size[i],
      point_alpha = combinations$point_alpha[i]
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for combination", i))
  }
})

test_that("hullplot respects all color palette options", {
  devtools::load_all()

  palettes <- c("default", "viridis", "set1", "set2", "dark2", "clinical")

  for (pal in palettes) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      color_palette = pal,
      show_labels = TRUE
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for palette:", pal))
  }
})

test_that("hullplot respects all plot theme options", {
  devtools::load_all()

  themes <- c("minimal", "classic", "light", "dark", "clinical")

  for (theme in themes) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      plot_theme = theme,
      show_labels = TRUE
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for theme:", theme))
  }
})

test_that("hullplot handles combinations of theme and palette", {
  devtools::load_all()

  combinations <- expand.grid(
    plot_theme = c("minimal", "classic", "clinical"),
    color_palette = c("default", "viridis", "clinical"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      plot_theme = combinations$plot_theme[i],
      color_palette = combinations$color_palette[i]
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for theme:", combinations$plot_theme[i],
                               "palette:", combinations$color_palette[i]))
  }
})

test_that("hullplot handles all display option combinations", {
  devtools::load_all()

  combinations <- expand.grid(
    show_labels = c(TRUE, FALSE),
    show_statistics = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(combinations)) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      show_labels = combinations$show_labels[i],
      show_statistics = combinations$show_statistics[i]
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for labels:", combinations$show_labels[i],
                               "stats:", combinations$show_statistics[i]))
  }
})

test_that("hullplot handles optional variables combinations", {
  devtools::load_all()

  # Only size variable
  result1 <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    size_var = "size_var"
  )
  expect_s3_class(result1, "hullplotResults")

  # Only color variable
  result2 <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    color_var = "color_category"
  )
  expect_s3_class(result2, "hullplotResults")

  # Both size and color
  result3 <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    size_var = "size_var",
    color_var = "color_category"
  )
  expect_s3_class(result3, "hullplotResults")
})

test_that("hullplot handles confidence ellipses with different parameters", {
  devtools::load_all()

  # Without ellipses
  result1 <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    confidence_ellipses = FALSE
  )
  expect_s3_class(result1, "hullplotResults")

  # With ellipses and labels
  result2 <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    confidence_ellipses = TRUE,
    show_labels = TRUE
  )
  expect_s3_class(result2, "hullplotResults")

  # With ellipses and statistics
  result3 <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    confidence_ellipses = TRUE,
    show_statistics = TRUE
  )
  expect_s3_class(result3, "hullplotResults")
})

test_that("hullplot handles outlier detection with different options", {
  devtools::load_all()

  # Outlier detection alone
  result1 <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type",
    outlier_detection = TRUE
  )
  expect_s3_class(result1, "hullplotResults")

  # Outlier detection with labels
  result2 <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type",
    outlier_detection = TRUE,
    show_labels = TRUE
  )
  expect_s3_class(result2, "hullplotResults")

  # Outlier detection with size variable
  result3 <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type",
    outlier_detection = TRUE,
    size_var = "severity"
  )
  expect_s3_class(result3, "hullplotResults")
})

test_that("hullplot handles custom labels with different combinations", {
  devtools::load_all()

  # Custom title only
  result1 <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    plot_title = "Custom Title"
  )
  expect_s3_class(result1, "hullplotResults")

  # Custom axis labels only
  result2 <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    x_label = "Tumor Volume (cm³)",
    y_label = "Ki-67 Index (%)"
  )
  expect_s3_class(result2, "hullplotResults")

  # All custom labels
  result3 <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    plot_title = "Clinical Trial Results",
    x_label = "Tumor Volume (cm³)",
    y_label = "Ki-67 Index (%)"
  )
  expect_s3_class(result3, "hullplotResults")
})

test_that("hullplot handles plot dimension parameters", {
  devtools::load_all()

  dimensions <- list(
    list(width = 400, height = 400),
    list(width = 600, height = 450),
    list(width = 800, height = 600),
    list(width = 1200, height = 800)
  )

  for (dim in dimensions) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      plotwidth = dim$width,
      plotheight = dim$height
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for width =", dim$width, "height =", dim$height))
  }
})

test_that("hullplot handles comprehensive argument combinations", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    show_labels = TRUE,
    show_statistics = TRUE,
    confidence_ellipses = TRUE,
    hull_concavity = 1.8,
    hull_alpha = 0.25,
    hull_expand = 0.1,
    point_size = 2.5,
    point_alpha = 0.75,
    color_palette = "clinical",
    plot_theme = "minimal",
    x_label = "Tumor Volume (cm³)",
    y_label = "Ki-67 Index (%)",
    plot_title = "Treatment Response",
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles minimal aesthetics configuration", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    hull_concavity = 2.0,
    hull_alpha = 0.1,
    point_size = 1,
    point_alpha = 0.3,
    show_labels = FALSE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles maximal aesthetics configuration", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    hull_concavity = 0.5,
    hull_alpha = 0.8,
    hull_expand = 0.15,
    point_size = 4,
    point_alpha = 1.0,
    show_labels = TRUE,
    show_statistics = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles publication-ready configuration", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    size_var = "age",
    show_labels = TRUE,
    show_statistics = TRUE,
    confidence_ellipses = TRUE,
    hull_concavity = 1.5,
    hull_alpha = 0.3,
    point_size = 2.5,
    point_alpha = 0.8,
    color_palette = "clinical",
    plot_theme = "clinical",
    x_label = "Tumor Volume (cm³)",
    y_label = "Ki-67 Proliferation Index (%)",
    plot_title = "Treatment Response: Tumor Characteristics",
    plotwidth = 800,
    plotheight = 600,
    show_summary = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles different group counts with same parameters", {
  devtools::load_all()

  # Two groups
  result2 <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    show_labels = TRUE,
    hull_alpha = 0.3
  )
  expect_s3_class(result2, "hullplotResults")

  # Three groups
  result3 <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    show_labels = TRUE,
    hull_alpha = 0.3
  )
  expect_s3_class(result3, "hullplotResults")

  # Four groups
  result4 <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype",
    show_labels = TRUE,
    hull_alpha = 0.3
  )
  expect_s3_class(result4, "hullplotResults")
})

test_that("hullplot handles extreme concavity values", {
  devtools::load_all()

  # Minimum concavity (most concave)
  result_min <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype",
    hull_concavity = 0.5,
    show_labels = TRUE
  )
  expect_s3_class(result_min, "hullplotResults")

  # Maximum concavity (convex hull)
  result_max <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype",
    hull_concavity = 2.0,
    show_labels = TRUE
  )
  expect_s3_class(result_max, "hullplotResults")
})

test_that("hullplot handles extreme transparency values", {
  devtools::load_all()

  # Minimum transparency (most opaque)
  result_opaque <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    hull_alpha = 0.9,
    point_alpha = 1.0
  )
  expect_s3_class(result_opaque, "hullplotResults")

  # Maximum transparency
  result_transparent <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    hull_alpha = 0.1,
    point_alpha = 0.2
  )
  expect_s3_class(result_transparent, "hullplotResults")
})

test_that("hullplot handles summary and assumptions together", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    show_summary = TRUE,
    show_assumptions = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles factor vs character group variables", {
  devtools::load_all()

  # Factor group variable
  test_data_factor <- hullplot_test
  test_data_factor$group <- as.factor(test_data_factor$group)

  result_factor <- hullplot(
    data = test_data_factor,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )
  expect_s3_class(result_factor, "hullplotResults")

  # Character group variable
  test_data_char <- hullplot_test
  test_data_char$group <- as.character(test_data_char$group)

  result_char <- hullplot(
    data = test_data_char,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )
  expect_s3_class(result_char, "hullplotResults")
})

test_that("hullplot handles all clinical configuration options", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    color_var = "response_status",
    size_var = "age",
    show_labels = TRUE,
    show_statistics = TRUE,
    confidence_ellipses = TRUE,
    color_palette = "clinical",
    plot_theme = "clinical",
    x_label = "Tumor Volume (cm³)",
    y_label = "Ki-67 Index (%)",
    plot_title = "Clinical Trial: Treatment Response"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles different expansion values", {
  devtools::load_all()

  expand_values <- c(0.0, 0.05, 0.1, 0.15, 0.2)

  for (expand in expand_values) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_expand = expand,
      show_labels = TRUE
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for hull_expand:", expand))
  }
})

test_that("hullplot handles overlapping groups with full feature set", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_overlap,
    x_var = "x",
    y_var = "y",
    group_var = "treatment",
    show_labels = TRUE,
    show_statistics = TRUE,
    confidence_ellipses = TRUE,
    hull_alpha = 0.2,
    hull_concavity = 1.5,
    point_alpha = 0.6,
    color_palette = "set1"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles all visualizations for same dataset", {
  devtools::load_all()

  # Ensure all visualization options work together
  result <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type",
    size_var = "severity",
    show_labels = TRUE,
    show_statistics = TRUE,
    outlier_detection = TRUE,
    confidence_ellipses = TRUE,
    hull_concavity = 1.0,
    hull_alpha = 0.3,
    point_size = 3,
    plot_title = "Disease Groups with All Features"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles small sample with all features", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_small,
    x_var = "measurement_x",
    y_var = "measurement_y",
    group_var = "category",
    show_labels = TRUE,
    show_statistics = TRUE,
    confidence_ellipses = TRUE,
    hull_alpha = 0.3,
    color_palette = "viridis"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles unbalanced groups with all options", {
  devtools::load_all()

  result <- hullplot(
    data = hullplot_unbalanced,
    x_var = "biomarker1",
    y_var = "biomarker2",
    group_var = "risk_group",
    size_var = "age",
    show_labels = TRUE,
    show_statistics = TRUE,
    hull_concavity = 1.2,
    hull_alpha = 0.3,
    point_alpha = 0.7,
    plot_title = "Risk Groups (Unbalanced)"
  )

  expect_s3_class(result, "hullplotResults")
})
