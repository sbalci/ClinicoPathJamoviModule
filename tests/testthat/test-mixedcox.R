test_that("mixedcox exposes only implemented options and populated results", {
  skip_if_not_installed("yaml")

  analysis <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "mixedcox.a.yaml"))
  results <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "mixedcox.r.yaml"))
  ui <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "mixedcox.u.yaml"))

  offered <- vapply(analysis$options, `[[`, "", "name")
  output <- vapply(results$items, `[[`, "", "name")
  unsupported <- c(
    "correlation_structure", "optimization_method", "random_effects_significance",
    "influence_diagnostics", "random_effects_prediction", "n_clusters_plot",
    "confidence_intervals", "bootstrap_variance", "bootstrap_samples",
    "residual_analysis", "fixed_effects_plot", "random_effects_plot",
    "cluster_survival_plot", "variance_components", "show_cluster_summary",
    "showSummaries", "showExplanations", "likelihood_ratio_test"
  )
  expect_false(any(unsupported %in% offered))
  expect_setequal(offered, c(
    "data", "elapsedtime", "tint", "dxdate", "fudate", "timetypedata",
    "timetypeoutput", "outcome", "outcomeLevel", "fixed_effects",
    "continuous_effects", "cluster_var", "random_effects", "random_slope_var",
    "nested_clustering", "nested_cluster_var", "sparse_matrix",
    "icc_calculation", "show_fixed_effects", "show_random_effects",
    "show_model_comparison"
  ))
  expect_setequal(output, c("todo", "modelSummary", "fixedEffectsTable",
                           "randomEffectsSummary", "modelComparison"))
  # No image can be enabled until its renderer actually creates an image.
  expect_false(any(vapply(results$items, function(item) item$type == "Image", FALSE)))
  expect_false(any(grepl("_plot$|residual_analysis", offered)))
  expect_false(grepl("fixed_effects_plot|random_effects_plot|cluster_survival_plot",
                     paste(capture.output(str(ui)), collapse = " ")))
})

test_that("mixedcox fits and unsupported plot switches are rejected", {
  skip_if_not_installed("coxme")
  skip_if_not_installed("jmvcore")

  set.seed(2409)
  n <- 240
  cluster <- factor(rep(seq_len(12), each = n / 12))
  x <- rnorm(n)
  frailty <- rep(rnorm(12, sd = 0.5), each = n / 12)
  event_time <- rexp(n, rate = exp(0.5 * x + frailty) / 20)
  censor_time <- rexp(n, rate = 1 / 35)
  data <- data.frame(
    time = pmin(event_time, censor_time),
    status = factor(ifelse(event_time <= censor_time, "event", "censored"),
                    levels = c("censored", "event")),
    x = x, cluster = cluster
  )

  fit <- mixedcox(data, elapsedtime = "time", outcome = "status",
                  outcomeLevel = "event", continuous_effects = "x",
                  cluster_var = "cluster", icc_calculation = FALSE,
                  show_model_comparison = FALSE)
  expect_true(nrow(as.data.frame(fit$fixedEffectsTable)) >= 1L)
  expect_null(fit$fixedEffectsPlot)
  expect_null(fit$residualPlot)
  expect_null(fit$iccTable)

  with_options <- mixedcox(data, elapsedtime = "time", outcome = "status",
                           outcomeLevel = "event", continuous_effects = "x",
                           cluster_var = "cluster", icc_calculation = TRUE,
                           show_model_comparison = TRUE)
  expect_true(nrow(as.data.frame(with_options$fixedEffectsTable)) >= 1L)
  expect_false(is.null(with_options$modelComparison))
  dense_fit <- mixedcox(data, elapsedtime = "time", outcome = "status",
                        outcomeLevel = "event", continuous_effects = "x",
                        cluster_var = "cluster", sparse_matrix = FALSE,
                        show_model_comparison = FALSE)
  expect_true(nrow(as.data.frame(dense_fit$fixedEffectsTable)) >= 1L)
  expect_error(mixedcox(data, elapsedtime = "time", outcome = "status",
                        outcomeLevel = "event", continuous_effects = "x",
                        cluster_var = "cluster", fixed_effects_plot = TRUE))
})
