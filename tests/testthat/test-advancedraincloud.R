run_advancedraincloud_analysis <- function(data, ...) {
  opts <- advancedraincloudOptions$new(...)
  analysis <- advancedraincloudClass$new(options = opts, data = data)
  analysis$run()
  analysis
}

test_that("advancedraincloud module loads correctly", {
  expect_true(exists("advancedraincloudClass"))
  expect_true(is.function(advancedraincloud))
})

test_that("advancedraincloud handles basic input validation", {
  # Test with missing required variables
  expect_error(
    advancedraincloud(data = histopathology, y_var = NULL, x_var = "Group"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    advancedraincloud(data = histopathology, y_var = "Age", x_var = NULL),
    NA  # Should not error during initialization, only during run
  )
})

test_that("advancedraincloud works with valid inputs", {
  # Test basic functionality
  result <- advancedraincloud(
    data = histopathology,
    y_var = "Age",
    x_var = "Group"
  )
  
  expect_s3_class(result, "advancedraincloudResults")
  expect_true("Group" %in% names(histopathology))
  expect_true("Age" %in% names(histopathology))
})

test_that("advancedraincloud handles optional variables correctly", {
  # Test with fill variable
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      fill_var = "Sex"
    )
  }, NA)
  
  # Test with ID variable for longitudinal connections
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      id_var = "ID"
    )
  }, NA)
  
  # Test with covariate variable
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "OverallTime",
      x_var = "Group",
      cov_var = "Age"
    )
  }, NA)
})

test_that("advancedraincloud rain positioning works correctly", {
  # Test different rain side positions
  positions <- c("l", "r", "f")
  
  for (position in positions) {
    expect_error({
      result <- advancedraincloud(
        data = histopathology,
        y_var = "Age",
        x_var = "Group",
        rain_side = position
      )
    }, NA, info = paste("rain_side:", position))
  }
})

test_that("advancedraincloud color palettes work correctly", {
  # Test different color palettes
  palettes <- c("clinical", "viridis", "set1", "set2", "pastel", "dark2")
  
  for (palette in palettes) {
    expect_error({
      result <- advancedraincloud(
        data = histopathology,
        y_var = "Age",
        x_var = "Group",
        color_palette = palette
      )
    }, NA, info = paste("color_palette:", palette))
  }
})

test_that("advancedraincloud advanced features work correctly", {
  # Test Likert mode
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Grade",  # Discrete variable
      x_var = "Group",
      likert_mode = TRUE
    )
  }, NA)
  
  # Test longitudinal connections
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      id_var = "ID",
      show_longitudinal = TRUE
    )
  }, NA)
})

test_that("advancedraincloud statistical options work correctly", {
  # Test with statistics enabled
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      show_statistics = TRUE
    )
  }, NA)
  
  # Test with group comparisons
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      show_comparisons = TRUE
    )
  }, NA)
  
  # Test with interpretation guide
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      show_interpretation = TRUE
    )
  }, NA)
})

test_that("advancedraincloud customization parameters work correctly", {
  # Test numeric parameter validation
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      point_size = 2.0,
      point_alpha = 0.8,
      violin_alpha = 0.6,
      boxplot_width = 0.15
    )
  }, NA)
  
  # Test boundary values
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      point_size = 0.1,    # Minimum
      point_alpha = 1.0,   # Maximum
      violin_alpha = 0.0,  # Minimum
      boxplot_width = 1.0  # Maximum
    )
  }, NA)
})

test_that("advancedraincloud handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Age[1:5] <- NA
  test_data$Group[6:10] <- NA
  
  expect_error({
    result <- advancedraincloud(
      data = test_data,
      y_var = "Age",
      x_var = "Group"
    )
  }, NA)
})

test_that("advancedraincloud handles different variable types", {
  # Test with numeric y variable
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "OverallTime",  # Numeric variable
      x_var = "Group"
    )
  }, NA)
  
  # Test with factor x variable
  expect_error({
    test_data <- histopathology
    test_data$Group <- factor(test_data$Group)
    result <- advancedraincloud(
      data = test_data,
      y_var = "Age",
      x_var = "Group"
    )
  }, NA)
})

test_that("advancedraincloud complex combinations work", {
  # Test complex parameter combinations
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "OverallTime",
      x_var = "Group",
      fill_var = "Sex",
      id_var = "ID",
      cov_var = "Age",
      rain_side = "f",
      likert_mode = FALSE,
      show_longitudinal = TRUE,
      point_size = 1.8,
      point_alpha = 0.7,
      violin_alpha = 0.6,
      boxplot_width = 0.12,
      color_palette = "clinical",
      show_statistics = TRUE,
      show_comparisons = TRUE,
      show_interpretation = TRUE
    )
  }, NA)
})

test_that("advancedraincloud ggrain dependency handling", {
  # Test function behavior when ggrain is available
  if (requireNamespace("ggrain", quietly = TRUE)) {
    expect_error({
      result <- advancedraincloud(
        data = histopathology,
        y_var = "Age",
        x_var = "Group"
      )
    }, NA)
  } else {
    # Skip test if ggrain not available
    skip("ggrain package not available")
  }
})

test_that("advancedraincloud handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:10, ]
  
  expect_error({
    result <- advancedraincloud(
      data = small_data,
      y_var = "Age",
      x_var = "Group"
    )
  }, NA)
})

test_that("advancedraincloud works with different continuous variables", {
  # Test with different continuous variables from histopathology
  continuous_vars <- c("Age", "OverallTime", "Grade", "MeasurementA", "MeasurementB")
  
  for (var in continuous_vars) {
    if (var %in% names(histopathology)) {
      expect_error({
        args <- list(
          data = histopathology,
          y_var = var,
          x_var = "Group"
        )
        result <- do.call(advancedraincloud, args)
      }, NA, info = paste("y_var:", var))
    }
  }
})

test_that("advancedraincloud works with different grouping variables", {
  # Test with different categorical variables
  categorical_vars <- c("Group", "Sex", "Grade_Level", "Race")
  
  for (var in categorical_vars) {
    if (var %in% names(histopathology)) {
      expect_error({
        args <- list(
          data = histopathology,
          y_var = "Age",
          x_var = var
        )
        result <- do.call(advancedraincloud, args)
      }, NA, info = paste("x_var:", var))
    }
  }
})

test_that("advancedraincloud plot customization works", {
  # Test plot title and axis labels
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",
      x_var = "Group",
      plot_title = "Custom Title",
      x_label = "Custom X Label",
      y_label = "Custom Y Label"
    )
  }, NA)
})

test_that("advancedraincloud vs standard raincloud compatibility", {
  # Test that advanced raincloud works with similar data as standard raincloud
  expect_error({
    result <- advancedraincloud(
      data = histopathology,
      y_var = "Age",        # Similar to dep_var in standard raincloud
      x_var = "Group"       # Similar to group_var in standard raincloud
    )
  }, NA)
  
  # Test that results object has expected structure
  result <- advancedraincloud(
    data = histopathology,
    y_var = "Age",
    x_var = "Group"
  )
  
  expect_true(exists("plot", envir = result))
  expect_true(exists("statistics", envir = result))
  expect_true(exists("interpretation", envir = result))
})

test_that("advancedraincloud stores accurate comparison statistics", {
  skip_if_not_installed("ggrain")

  analysis <- run_advancedraincloud_analysis(
    data = histopathology,
    y_var = "Age",
    x_var = "Group",
    show_comparisons = TRUE
  )

  private_env <- analysis$.__enclos_env__$private
  comp_stats <- private_env$.comparison_results

  expect_false(is.null(comp_stats))
  expect_equal(comp_stats$method, "Wilcoxon rank-sum test")

  reference_test <- wilcox.test(Age ~ Group, data = histopathology)
  expect_equal(comp_stats$p_value, reference_test$p.value, tolerance = 1e-6)

  formatted_reference <- if (reference_test$p.value < 0.001) "< 0.001" else round(reference_test$p.value, 3)
  expect_equal(comp_stats$label, formatted_reference)
})

test_that("change score analysis respects baseline group and direction", {
  skip_if_not_installed("ggrain")

  data(advancedraincloud_data, package = "ClinicoPath")

  analysis <- run_advancedraincloud_analysis(
    data = advancedraincloud_data,
    y_var = "biomarker_level",
    x_var = "time_point",
    id_var = "patient_id",
    baseline_group = "Baseline",
    show_change_scores = TRUE,
    responder_threshold = 15
  )

  summary <- analysis$.__enclos_env__$private$.change_summary
  expect_false(is.null(summary))
  expect_equal(summary$baseline_group, "Baseline")
  expect_equal(summary$direction, "increase")
  expect_true(summary$n_total > 0)
  expect_true(summary$n_responders <= summary$n_total)
})

test_that("optional ID variables do not remove data when longitudinal features are off", {
  skip_if_not_installed("ggrain")

  synthetic <- data.frame(
    response = c(1, 2, 3, 4),
    cohort = factor(c("A", "A", "B", "B")),
    subject_id = c("s1", NA, "s3", "s4")
  )

  analysis <- run_advancedraincloud_analysis(
    data = synthetic,
    y_var = "response",
    x_var = "cohort",
    id_var = "subject_id",
    show_longitudinal = FALSE,
    show_change_scores = FALSE
  )

  analysis_data <- analysis$.__enclos_env__$private$.analysis_data
  expect_equal(nrow(analysis_data), 4)
})

test_that("p-value placement modes update the plot appropriately", {
  skip_if_not_installed("ggrain")

  analysis <- run_advancedraincloud_analysis(
    data = histopathology,
    y_var = "Age",
    x_var = "Group",
    show_comparisons = TRUE
  )

  private <- analysis$.__enclos_env__$private
  data_used <- private$.analysis_data
  stats <- private$.comparison_results
  y_sym <- rlang::sym(analysis$options$y_var)
  x_sym <- rlang::sym(analysis$options$x_var)

  base_plot <- ggplot2::ggplot(data_used, ggplot2::aes(x = !!x_sym, y = !!y_sym))

  plot_above <- private$.add_p_values(base_plot, data_used, analysis$options$y_var, analysis$options$x_var, "above")
  built <- ggplot2::ggplot_build(plot_above)
  expect_true(any(grepl(paste0("p = ", stats$label), built$data[[1]]$label)))

  plot_legend <- private$.add_p_values(base_plot, data_used, analysis$options$y_var, analysis$options$x_var, "legend")
  expect_true(grepl(stats$label, plot_legend$labels$caption, fixed = TRUE))

  plot_table <- private$.add_p_values(base_plot, data_used, analysis$options$y_var, analysis$options$x_var, "table")
  expect_true(is.null(plot_table$labels$caption))
})

test_that("statistics html output matches snapshot", {
  skip_if_not_installed("ggrain")

  analysis <- run_advancedraincloud_analysis(
    data = histopathology,
    y_var = "Age",
    x_var = "Group",
    show_statistics = TRUE,
    show_missing_info = TRUE,
    show_comparisons = TRUE
  )

  private <- analysis$.__enclos_env__$private
  stats_html <- private$.generate_statistics(
    private$.analysis_data,
    analysis$options$y_var,
    analysis$options$x_var,
    analysis$options$fill_var
  )

  stats_html <- paste0(
    stats_html,
    private$.generate_missing_data_info(
      histopathology,
      private$.analysis_data,
      colnames(private$.analysis_data)
    )
  )

  expect_snapshot(stats_html, cran = TRUE)
})

test_that("change analysis html output matches snapshot", {
  skip_if_not_installed("ggrain")
  data(advancedraincloud_data, package = "ClinicoPath")

  analysis <- run_advancedraincloud_analysis(
    data = advancedraincloud_data,
    y_var = "biomarker_level",
    x_var = "time_point",
    id_var = "patient_id",
    baseline_group = "Baseline",
    show_change_scores = TRUE,
    responder_threshold = 15,
    show_statistics = FALSE
  )

  private <- analysis$.__enclos_env__$private
  change_res <- private$.generate_change_analysis(
    advancedraincloud_data[c("biomarker_level", "time_point", "patient_id")],
    "biomarker_level",
    "time_point",
    "patient_id",
    "Baseline",
    15
  )

  expect_snapshot(change_res$html, cran = TRUE)
})

test_that("effect size html output matches snapshot", {
  skip_if_not_installed("ggrain")

  analysis <- run_advancedraincloud_analysis(
    data = histopathology,
    y_var = "Age",
    x_var = "Group",
    show_effect_size = TRUE,
    show_comparisons = FALSE
  )

  private <- analysis$.__enclos_env__$private
  effect_html <- private$.generate_effect_sizes(
    private$.analysis_data,
    analysis$options$y_var,
    analysis$options$x_var,
    analysis$options$effect_size_type
  )

  expect_snapshot(effect_html, cran = TRUE)
})

test_that("clinical report html output matches snapshot", {
  skip_if_not_installed("ggrain")

  analysis <- run_advancedraincloud_analysis(
    data = histopathology,
    y_var = "Age",
    x_var = "Group",
    generate_report = TRUE,
    population_type = "pp"
  )

  private <- analysis$.__enclos_env__$private
  report_html <- private$.generate_clinical_report(
    private$.analysis_data,
    analysis$options$y_var,
    analysis$options$x_var,
    analysis$options$population_type
  )

  expect_snapshot(report_html, cran = TRUE)
})
