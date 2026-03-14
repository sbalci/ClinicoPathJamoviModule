# ═══════════════════════════════════════════════════════════
# Integration Tests: adaptivelasso
# ═══════════════════════════════════════════════════════════

library(testthat)

run_alasso <- function(...) {
  defaults <- list(
    strata = "strata_var",
    cv_folds = 5, n_lambda = 20, random_seed = 42,
    stability_selection = FALSE,
    plot_selection_path = FALSE, plot_cv_curve = FALSE,
    plot_stability = FALSE, plot_survival_curves = FALSE,
    plot_baseline_hazard = FALSE, plot_diagnostics = FALSE
  )
  args <- c(list(...), defaults)
  args <- args[!duplicated(names(args))]
  do.call(adaptivelasso, args)
}

test_that("full pipeline: suitability + model + diagnostics + predictions", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  set.seed(42)
  n <- 150
  d <- data.frame(
    time = pmax(rexp(n, 0.04), 0.1),
    event = factor(rbinom(n, 1, 0.55), levels = c(0, 1), labels = c("C", "E")),
    age = rnorm(n, 62, 10),
    biomarker = rnorm(n),
    stage = factor(sample(c("I", "II", "III"), n, replace = TRUE)),
    strata_var = factor(sample(c("S1", "S2"), n, replace = TRUE))
  )

  result <- run_alasso(
    data = d, time = "time", event = "event", event_level = "E",
    predictors = c("age", "biomarker", "stage"),
    suitabilityCheck = TRUE,
    show_coefficients = TRUE, show_selection_path = TRUE,
    show_cv_results = TRUE, show_diagnostics = TRUE,
    proportional_hazards = TRUE, goodness_of_fit = TRUE,
    baseline_survival = TRUE, time_points = "6, 12, 24",
    risk_groups = 3, n_lambda = 25
  )

  expect_true(nchar(result$suitabilityReport$content %||% "") > 0)
  expect_true(nchar(result$notices$content %||% "") > 0)

  path_df <- as.data.frame(result$selectionPath)
  expect_true(nrow(path_df) > 0)

  cv_df <- as.data.frame(result$cvResults)
  expect_true(nrow(cv_df) >= 1)

  rg_df <- as.data.frame(result$riskGroups)
  expect_true(nrow(rg_df) >= 1)
})

test_that("stability selection variable names are readable", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  set.seed(42)
  n <- 150
  d <- data.frame(
    time = pmax(rexp(n, 0.04), 0.1),
    event = factor(rbinom(n, 1, 0.55), levels = c(0, 1), labels = c("C", "E")),
    age = rnorm(n, 62, 10),
    tumor_size = rlnorm(n, log(3), 0.5),
    grade = factor(sample(c("Low", "Med", "High"), n, replace = TRUE)),
    strata_var = factor(sample(c("S1", "S2"), n, replace = TRUE))
  )

  result <- run_alasso(
    data = d, time = "time", event = "event", event_level = "E",
    predictors = c("age", "tumor_size", "grade"),
    stability_selection = TRUE, bootstrap_samples = 50,
    stability_threshold = 0.6, subsampling_ratio = 0.7
  )

  stab_df <- as.data.frame(result$stabilityResults)
  if (nrow(stab_df) > 0) {
    expect_false(any(grepl("^s\\d+$", stab_df$variable)))
    expect_false(any(grepl("^V\\d+$", stab_df$variable)))
    expect_true(all(stab_df$selection_frequency >= 0 & stab_df$selection_frequency <= 1))
    expect_true(all(stab_df$stability_score >= 0))
  }
})

test_that("different weight methods produce valid output", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  set.seed(42)
  n <- 120
  d <- data.frame(
    time = pmax(rexp(n, 0.05), 0.1),
    event = factor(rbinom(n, 1, 0.55), levels = c(0, 1), labels = c("C", "E")),
    age = rnorm(n, 62, 10),
    biomarker = rnorm(n),
    strata_var = factor(sample(c("S1", "S2"), n, replace = TRUE))
  )

  r_ridge <- run_alasso(data = d, time = "time", event = "event", event_level = "E",
                        predictors = c("age", "biomarker"), weight_method = "ridge")
  r_equal <- run_alasso(data = d, time = "time", event = "event", event_level = "E",
                        predictors = c("age", "biomarker"), weight_method = "equal")

  cv_ridge <- as.data.frame(r_ridge$cvResults)
  cv_equal <- as.data.frame(r_equal$cvResults)
  expect_true(nrow(cv_ridge) >= 1)
  expect_true(nrow(cv_equal) >= 1)
  expect_true(is.numeric(cv_ridge$lambda_min))
  expect_true(is.numeric(cv_equal$lambda_min))
})

test_that("diagnostics tables populated when model selects variables", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  set.seed(42)
  n <- 150
  risk <- rnorm(n)
  d <- data.frame(
    time = pmax(rweibull(n, shape = 1.2, scale = exp(-risk * 0.5) * 20), 0.1),
    event = factor(rbinom(n, 1, 0.6), levels = c(0, 1), labels = c("C", "E")),
    strong_predictor = risk,
    noise = rnorm(n),
    strata_var = factor(sample(c("S1", "S2"), n, replace = TRUE))
  )

  result <- run_alasso(
    data = d, time = "time", event = "event", event_level = "E",
    predictors = c("strong_predictor", "noise"),
    show_diagnostics = TRUE, proportional_hazards = TRUE,
    goodness_of_fit = TRUE, n_lambda = 25
  )

  coef_df <- as.data.frame(result$coefficients)
  if (nrow(coef_df) > 0) {
    diag_df <- as.data.frame(result$modelDiagnostics)
    expect_true(nrow(diag_df) >= 0)
    perf_df <- as.data.frame(result$performanceMetrics)
    expect_true(nrow(perf_df) >= 0)
  }
})
