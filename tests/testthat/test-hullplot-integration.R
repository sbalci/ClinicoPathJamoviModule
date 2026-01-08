# ═══════════════════════════════════════════════════════════
# Integration Tests: hullplot
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)

test_that("hullplot integrates with all test datasets", {
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
      group_var = ds$group,
      show_labels = TRUE,
      hull_alpha = 0.3
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Integration failed for dataset:", ds$name))
    expect_true(!is.null(result$plot1),
               info = paste("Plot missing for dataset:", ds$name))
  }
})

test_that("hullplot produces consistent results across data formats", {
  devtools::load_all()

  data(hullplot_test)

  # As data.frame
  df_data <- as.data.frame(hullplot_test)
  result_df <- hullplot(
    data = df_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  # As tibble
  tibble_data <- tibble::as_tibble(hullplot_test)
  result_tibble <- hullplot(
    data = tibble_data,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )

  # Both should succeed
  expect_s3_class(result_df, "hullplotResults")
  expect_s3_class(result_tibble, "hullplotResults")
})

test_that("hullplot handles complete clinical trial workflow", {
  devtools::load_all()

  data(hullplot_clinical)

  # Step 1: Basic visualization
  result_basic <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm"
  )
  expect_s3_class(result_basic, "hullplotResults")

  # Step 2: Add labels and statistics
  result_stats <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    show_labels = TRUE,
    show_statistics = TRUE
  )
  expect_s3_class(result_stats, "hullplotResults")

  # Step 3: Add confidence ellipses
  result_ellipses <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    show_labels = TRUE,
    show_statistics = TRUE,
    confidence_ellipses = TRUE
  )
  expect_s3_class(result_ellipses, "hullplotResults")

  # Step 4: Publication-ready figure
  result_pub <- hullplot(
    data = hullplot_clinical,
    x_var = "tumor_volume",
    y_var = "ki67_index",
    group_var = "treatment_arm",
    show_labels = TRUE,
    show_statistics = TRUE,
    confidence_ellipses = TRUE,
    hull_concavity = 1.5,
    hull_alpha = 0.3,
    color_palette = "clinical",
    plot_theme = "clinical",
    x_label = "Tumor Volume (cm³)",
    y_label = "Ki-67 Index (%)",
    plot_title = "Treatment Response",
    show_summary = TRUE
  )
  expect_s3_class(result_pub, "hullplotResults")
})

test_that("hullplot handles exploratory data analysis workflow", {
  devtools::load_all()

  data(hullplot_clusters)

  # Step 1: Quick visualization
  result_quick <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster"
  )
  expect_s3_class(result_quick, "hullplotResults")

  # Step 2: Add size variable for multivariate view
  result_multi <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    size_var = "biomarker"
  )
  expect_s3_class(result_multi, "hullplotResults")

  # Step 3: Fine-tune aesthetics
  result_tuned <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    size_var = "biomarker",
    hull_concavity = 1.0,
    hull_alpha = 0.3,
    show_labels = TRUE
  )
  expect_s3_class(result_tuned, "hullplotResults")
})

test_that("hullplot handles quality control workflow", {
  devtools::load_all()

  data(hullplot_outliers)

  # Step 1: Initial visualization
  result_initial <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type"
  )
  expect_s3_class(result_initial, "hullplotResults")

  # Step 2: Enable outlier detection
  result_outliers <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type",
    outlier_detection = TRUE
  )
  expect_s3_class(result_outliers, "hullplotResults")

  # Step 3: Add severity information
  result_severity <- hullplot(
    data = hullplot_outliers,
    x_var = "x",
    y_var = "y",
    group_var = "disease_type",
    size_var = "severity",
    outlier_detection = TRUE,
    show_labels = TRUE
  )
  expect_s3_class(result_severity, "hullplotResults")
})

test_that("hullplot handles comparative analysis workflow", {
  devtools::load_all()

  data(hullplot_test)
  data(hullplot_clusters)

  # Compare two-group vs three-group visualization
  result_two <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    hull_concavity = 1.5,
    hull_alpha = 0.3
  )
  expect_s3_class(result_two, "hullplotResults")

  result_three <- hullplot(
    data = hullplot_clusters,
    x_var = "x",
    y_var = "y",
    group_var = "cluster",
    hull_concavity = 1.5,
    hull_alpha = 0.3
  )
  expect_s3_class(result_three, "hullplotResults")
})

test_that("hullplot handles different spatial arrangements", {
  devtools::load_all()

  # Well-separated groups
  data(hullplot_test)
  result_separated <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    show_labels = TRUE
  )
  expect_s3_class(result_separated, "hullplotResults")

  # Overlapping groups
  data(hullplot_overlap)
  result_overlap <- hullplot(
    data = hullplot_overlap,
    x_var = "x",
    y_var = "y",
    group_var = "treatment",
    hull_alpha = 0.2,
    show_labels = TRUE
  )
  expect_s3_class(result_overlap, "hullplotResults")

  # Quadrant arrangement
  data(hullplot_fourgroup)
  result_quadrant <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype",
    show_labels = TRUE
  )
  expect_s3_class(result_quadrant, "hullplotResults")
})

test_that("hullplot handles different sample sizes consistently", {
  devtools::load_all()

  # Small sample
  data(hullplot_small)
  result_small <- hullplot(
    data = hullplot_small,
    x_var = "measurement_x",
    y_var = "measurement_y",
    group_var = "category"
  )
  expect_s3_class(result_small, "hullplotResults")

  # Medium sample
  data(hullplot_test)
  result_medium <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group"
  )
  expect_s3_class(result_medium, "hullplotResults")

  # Large sample
  data(hullplot_fourgroup)
  result_large <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype"
  )
  expect_s3_class(result_large, "hullplotResults")
})

test_that("hullplot handles balanced vs unbalanced groups", {
  devtools::load_all()

  # Balanced groups
  data(hullplot_test)
  result_balanced <- hullplot(
    data = hullplot_test,
    x_var = "x",
    y_var = "y",
    group_var = "group",
    show_statistics = TRUE
  )
  expect_s3_class(result_balanced, "hullplotResults")

  # Unbalanced groups
  data(hullplot_unbalanced)
  result_unbalanced <- hullplot(
    data = hullplot_unbalanced,
    x_var = "biomarker1",
    y_var = "biomarker2",
    group_var = "risk_group",
    show_statistics = TRUE
  )
  expect_s3_class(result_unbalanced, "hullplotResults")
})

test_that("hullplot handles all color palettes consistently", {
  devtools::load_all()

  data(hullplot_clusters)

  palettes <- c("default", "viridis", "set1", "set2", "dark2", "clinical")

  for (pal in palettes) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      color_palette = pal,
      show_labels = TRUE,
      hull_alpha = 0.3
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Integration failed for palette:", pal))
  }
})

test_that("hullplot handles all plot themes consistently", {
  devtools::load_all()

  data(hullplot_clusters)

  themes <- c("minimal", "classic", "light", "dark", "clinical")

  for (theme in themes) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      plot_theme = theme,
      show_labels = TRUE,
      hull_alpha = 0.3
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Integration failed for theme:", theme))
  }
})

test_that("hullplot handles combinations of all visual features", {
  devtools::load_all()

  data(hullplot_clinical)

  # Test comprehensive feature integration
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
    hull_concavity = 1.5,
    hull_alpha = 0.3,
    hull_expand = 0.1,
    point_size = 2.5,
    point_alpha = 0.8,
    color_palette = "clinical",
    plot_theme = "clinical",
    x_label = "Tumor Volume (cm³)",
    y_label = "Ki-67 Index (%)",
    plot_title = "Complete Integration Test",
    show_summary = TRUE,
    show_assumptions = TRUE,
    plotwidth = 800,
    plotheight = 600
  )

  expect_s3_class(result, "hullplotResults")
})

test_that("hullplot handles gene expression analysis workflow", {
  devtools::load_all()

  data(hullplot_fourgroup)

  # Step 1: Initial two-gene view
  result_basic <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype"
  )
  expect_s3_class(result_basic, "hullplotResults")

  # Step 2: Add survival information
  result_survival <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype",
    size_var = "survival_months",
    show_labels = TRUE
  )
  expect_s3_class(result_survival, "hullplotResults")

  # Step 3: Publication figure
  result_pub <- hullplot(
    data = hullplot_fourgroup,
    x_var = "gene_a",
    y_var = "gene_b",
    group_var = "subtype",
    size_var = "survival_months",
    show_labels = TRUE,
    show_statistics = TRUE,
    color_palette = "viridis",
    x_label = "Gene A Expression",
    y_label = "Gene B Expression",
    plot_title = "Molecular Subtypes"
  )
  expect_s3_class(result_pub, "hullplotResults")
})

test_that("hullplot handles biomarker analysis workflow", {
  devtools::load_all()

  data(hullplot_unbalanced)

  # Step 1: Risk group visualization
  result_risk <- hullplot(
    data = hullplot_unbalanced,
    x_var = "biomarker1",
    y_var = "biomarker2",
    group_var = "risk_group"
  )
  expect_s3_class(result_risk, "hullplotResults")

  # Step 2: Add patient age context
  result_age <- hullplot(
    data = hullplot_unbalanced,
    x_var = "biomarker1",
    y_var = "biomarker2",
    group_var = "risk_group",
    size_var = "age",
    show_labels = TRUE
  )
  expect_s3_class(result_age, "hullplotResults")

  # Step 3: Clinical presentation
  result_clinical <- hullplot(
    data = hullplot_unbalanced,
    x_var = "biomarker1",
    y_var = "biomarker2",
    group_var = "risk_group",
    size_var = "age",
    show_labels = TRUE,
    show_statistics = TRUE,
    x_label = "Biomarker 1",
    y_label = "Biomarker 2",
    plot_title = "Risk Stratification"
  )
  expect_s3_class(result_clinical, "hullplotResults")
})

test_that("hullplot handles different concavity settings across datasets", {
  devtools::load_all()

  concavity_values <- c(0.5, 1.0, 1.5, 2.0)

  data(hullplot_clusters)

  for (conc in concavity_values) {
    result <- hullplot(
      data = hullplot_clusters,
      x_var = "x",
      y_var = "y",
      group_var = "cluster",
      hull_concavity = conc,
      show_labels = TRUE
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Integration failed for concavity:", conc))
  }
})

test_that("hullplot handles progressive feature addition", {
  devtools::load_all()

  data(hullplot_clinical)

  # Progressive feature addition
  features_list <- list(
    list(show_labels = TRUE),
    list(show_labels = TRUE, show_statistics = TRUE),
    list(show_labels = TRUE, show_statistics = TRUE, confidence_ellipses = TRUE),
    list(show_labels = TRUE, show_statistics = TRUE, confidence_ellipses = TRUE,
         size_var = "age"),
    list(show_labels = TRUE, show_statistics = TRUE, confidence_ellipses = TRUE,
         size_var = "age", color_var = "response_status")
  )

  for (i in seq_along(features_list)) {
    args <- c(
      list(
        data = hullplot_clinical,
        x_var = "tumor_volume",
        y_var = "ki67_index",
        group_var = "treatment_arm"
      ),
      features_list[[i]]
    )

    result <- do.call(hullplot, args)

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed for feature set", i))
  }
})

test_that("hullplot handles all datasets with consistent parameters", {
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

  # Apply same parameters to all datasets
  for (ds in datasets) {
    data(list = ds$name, package = "ClinicoPath")
    dataset <- get(ds$name)

    result <- hullplot(
      data = dataset,
      x_var = ds$x,
      y_var = ds$y,
      group_var = ds$group,
      show_labels = TRUE,
      show_statistics = TRUE,
      hull_concavity = 1.5,
      hull_alpha = 0.3,
      point_size = 2.5,
      color_palette = "viridis"
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Consistent parameters failed for:", ds$name))
  }
})

test_that("hullplot output structure is consistent across all datasets", {
  devtools::load_all()

  datasets <- c("hullplot_test", "hullplot_clusters", "hullplot_clinical")

  for (dataset_name in datasets) {
    data(list = dataset_name, package = "ClinicoPath")
    dataset <- get(dataset_name)

    x_var <- if (dataset_name == "hullplot_clinical") "tumor_volume" else "x"
    y_var <- if (dataset_name == "hullplot_clinical") "ki67_index" else "y"
    group_var <- switch(dataset_name,
                       "hullplot_test" = "group",
                       "hullplot_clusters" = "cluster",
                       "hullplot_clinical" = "treatment_arm")

    result <- hullplot(
      data = dataset,
      x_var = x_var,
      y_var = y_var,
      group_var = group_var,
      show_labels = TRUE
    )

    # Check consistent output structure
    expect_s3_class(result, "hullplotResults")
    expect_true(!is.null(result$plot1),
               info = paste("Plot missing for:", dataset_name))
  }
})

test_that("hullplot handles repeated analyses with same data", {
  devtools::load_all()

  data(hullplot_test)

  # Run same analysis multiple times
  for (i in 1:5) {
    result <- hullplot(
      data = hullplot_test,
      x_var = "x",
      y_var = "y",
      group_var = "group",
      show_labels = TRUE
    )

    expect_s3_class(result, "hullplotResults",
                   info = paste("Failed on iteration", i))
  }
})
