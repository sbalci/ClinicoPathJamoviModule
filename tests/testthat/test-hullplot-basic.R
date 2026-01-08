# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: hullplot
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("hullplot function exists and loads", {
  devtools::load_all()

  expect_true(exists("hullplot"))
})

test_that("hullplot runs with minimal required arguments", {
  devtools::load_all()

  data(hullplot_test, package = "ClinicoPath")

  # Minimal required arguments (x_var, y_var, group_var)
  result <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot produces expected output structure", {
  devtools::load_all()

  data(hullplot_test)

  result <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  # Check for main plot output
  expect_true(!is.null(result$plot1))
})

test_that("hullplot handles two-group visualization", {
  devtools::load_all()

  data(hullplot_test)

  result <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    show_labels = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles three-group visualization", {
  devtools::load_all()

  data(hullplot_clusters)

  result <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles four-group visualization", {
  devtools::load_all()

  data(hullplot_fourgroup)

  result <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles hull concavity parameter", {
  devtools::load_all()

  data(hullplot_test)

  concavity_values <- c(0.5, 1.0, 1.5, 2.0)

  for (conc in concavity_values) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_concavity = conc
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for hull_concavity:", conc))
  }
})

test_that("hullplot handles hull alpha transparency", {
  devtools::load_all()

  data(hullplot_test)

  alpha_values <- c(0.1, 0.3, 0.5, 0.8)

  for (alpha in alpha_values) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_alpha = alpha
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for hull_alpha:", alpha))
  }
})

test_that("hullplot handles hull expand parameter", {
  devtools::load_all()

  data(hullplot_test)

  expand_values <- c(0.0, 0.05, 0.1, 0.15)

  for (expand in expand_values) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      hull_expand = expand
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for hull_expand:", expand))
  }
})

test_that("hullplot handles group labels", {
  devtools::load_all()

  data(hullplot_test)

  # Without labels
  result1 <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    show_labels = FALSE
  )
  expect_s3_class(result1, "hullplotResults")

  # With labels
  result2 <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    show_labels = TRUE
  )
  expect_s3_class(result2, "hullplotResults")
})

test_that("hullplot handles statistics display", {
  devtools::load_all()

  data(hullplot_clusters)

  # Without statistics
  result1 <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    show_statistics = FALSE
  )
  expect_s3_class(result1, "hullplotResults")

  # With statistics
  result2 <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    show_statistics = TRUE
  )
  expect_s3_class(result2, "hullplotResults")
})

test_that("hullplot handles confidence ellipses", {
  devtools::load_all()

  data(hullplot_overlap)

  # Without ellipses
  result1 <- hullplot(
    data = hullplot_overlap,
    x_var = "x",
    y_var = "y",
    group_var = "treatment",
    confidence_ellipses = FALSE
  )
  expect_s3_class(result1, "hullplotResults")

  # With ellipses
  result2 <- hullplot(
    data = hullplot_overlap,
    x_var = "x",
    y_var = "y",
    group_var = "treatment",
    confidence_ellipses = TRUE
  )
  expect_s3_class(result2, "hullplotResults")
})

test_that("hullplot handles outlier detection", {
  devtools::load_all()

  data(hullplot_outliers)

  # Without outlier detection
  result1 <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type",
    outlier_detection = FALSE
  )
  expect_s3_class(result1, "hullplotResults")

  # With outlier detection
  result2 <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type",
    outlier_detection = TRUE
  )
  expect_s3_class(result2, "hullplotResults")
})

test_that("hullplot handles size variable", {
  devtools::load_all()

  data(hullplot_test)

  # Without size variable
  result1 <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )
  expect_s3_class(result1, "hullplotResults")

  # With size variable
  result2 <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    size_var = "size_var"
  )
  expect_s3_class(result2, "hullplotResults")
})

test_that("hullplot handles separate color variable", {
  devtools::load_all()

  data(hullplot_test)

  result <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    color_var = "color_category"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles point size parameter", {
  devtools::load_all()

  data(hullplot_test)

  point_sizes <- c(1, 2, 3, 4)

  for (ps in point_sizes) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      point_size = ps
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for point_size:", ps))
  }
})

test_that("hullplot handles point alpha parameter", {
  devtools::load_all()

  data(hullplot_test)

  point_alphas <- c(0.3, 0.5, 0.7, 1.0)

  for (pa in point_alphas) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      point_alpha = pa
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for point_alpha:", pa))
  }
})

test_that("hullplot handles color palettes", {
  devtools::load_all()

  data(hullplot_clusters)

  palettes <- c("default", "viridis", "set1", "set2", "dark2", "clinical")

  for (pal in palettes) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      color_palette = pal
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for color_palette:", pal))
  }
})

test_that("hullplot handles plot themes", {
  devtools::load_all()

  data(hullplot_clusters)

  themes <- c("minimal", "classic", "light", "dark", "clinical")

  for (theme in themes) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      plot_theme = theme
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for plot_theme:", theme))
  }
})

test_that("hullplot handles custom axis labels", {
  devtools::load_all()

  data(hullplot_clinical)

  result <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    x_label = "Tumor Volume (cm³)",
    y_label = "Ki-67 Index (%)",
    plot_title = "Clinical Trial Results"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles different datasets", {
  devtools::load_all()

  datasets <- list(
    list(name = "hullplot_test", x = "x", y = "y", group = "group"),
    list(name = "hullplot_clusters", x = "x", y = "y", group = "cluster"),
    list(name = "hullplot_overlap", x = "x", y = "y", group = "treatment"),
    list(name = "hullplot_outliers", x = "x", y = "y", group = "disease_type"),
    list(name = "hullplot_clinical", x = "tumor_volume", y = "ki67_index", group = "treatment_arm"),
    list(name = "hullplot_small", x = "measurement_x", y = "measurement_y", group = "category"),
    list(name = "hullplot_fourgroup", x = "gene_a", y = "gene_b", group = "subtype"),
    list(name = "hullplot_unbalanced", x = "biomarker1", y = "biomarker2", group = "risk_group")
  )

  for (ds in datasets) {
    data(list = ds$name, package = "ClinicoPath")
    dataset <- get(ds$name)

    result <- hullplot(
      data = dataset,
      x_var = ds$x,
      y_var = ds$y,
      group_var = ds$group
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for dataset:", ds$name))
  }
})

test_that("hullplot handles small sample size", {
  devtools::load_all()

  data(hullplot_small)

  result <- hullplot(
    data = hullplot_small,
    x_var = "measurement_x",
    y_var = "measurement_y",
    group_var = "category"
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles overlapping groups", {
  devtools::load_all()

  data(hullplot_overlap)

  result <- hullplot(
    data = hullplot_overlap,
    x_var = "x",
    y_var = "y",
    group_var = "treatment",
    hull_alpha = 0.2,
    show_labels = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles unbalanced groups", {
  devtools::load_all()

  data(hullplot_unbalanced)

  result <- hullplot(
    data = hullplot_unbalanced,
    x_var = "biomarker1",
    y_var = "biomarker2",
    group_var = "risk_group",
    show_labels = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles natural language summary", {
  devtools::load_all()

  data(hullplot_clinical)

  result <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    show_summary = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles assumptions display", {
  devtools::load_all()

  data(hullplot_test)

  result <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    show_assumptions = TRUE
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles plot dimensions", {
  devtools::load_all()

  data(hullplot_test)

  result <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles clinical trial data visualization", {
  devtools::load_all()

  data(hullplot_clinical)

  result <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    show_labels = TRUE,
    show_statistics = TRUE,
    color_palette = "clinical",
    plot_theme = "clinical"
  )

  expect_s3_class(result, "hullplotResults")
})
