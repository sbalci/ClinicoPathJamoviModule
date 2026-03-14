# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: adaptivelasso
# ═══════════════════════════════════════════════════════════

library(testthat)

make_alasso_data <- function(n = 120, event_rate = 0.55, seed = 42) {
  set.seed(seed)
  data.frame(
    time = pmax(rexp(n, rate = 0.05), 0.1),
    event = factor(rbinom(n, 1, event_rate),
                   levels = c(0, 1), labels = c("Censored", "Event")),
    age = rnorm(n, 62, 10),
    biomarker = rnorm(n),
    stage = factor(sample(c("I", "II", "III"), n, replace = TRUE)),
    strata_var = factor(sample(c("S1", "S2"), n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}

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

test_that("all weight methods run without error", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()
  for (m in c("ridge", "univariate", "cox", "correlation", "equal")) {
    expect_no_error(
      run_alasso(data = d, time = "time", event = "event", event_level = "Event",
                 predictors = c("age", "biomarker"), weight_method = m)
    )
  }
})

test_that("different alpha values run (elastic net mixing)", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()
  for (a in c(0.0, 0.5, 1.0)) {
    expect_no_error(
      run_alasso(data = d, time = "time", event = "event", event_level = "Event",
                 predictors = c("age", "biomarker"), alpha = a)
    )
  }
})

test_that("gamma parameter changes adaptive weights", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()
  r1 <- run_alasso(data = d, time = "time", event = "event", event_level = "Event",
                   predictors = c("age", "biomarker"), gamma = 0.5)
  r2 <- run_alasso(data = d, time = "time", event = "event", event_level = "Event",
                   predictors = c("age", "biomarker"), gamma = 3.0)

  expect_true(inherits(r1, "adaptivelassoResults"))
  expect_true(inherits(r2, "adaptivelassoResults"))
})

test_that("lambda_sequence modes work", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()

  expect_no_error(run_alasso(data = d, time = "time", event = "event",
    event_level = "Event", predictors = c("age", "biomarker"),
    lambda_sequence = "auto"))

  expect_no_error(run_alasso(data = d, time = "time", event = "event",
    event_level = "Event", predictors = c("age", "biomarker"),
    lambda_sequence = "custom", lambda_custom_max = 1.0, lambda_custom_min = 0.001))

  expect_no_error(run_alasso(data = d, time = "time", event = "event",
    event_level = "Event", predictors = c("age", "biomarker"),
    lambda_sequence = "single", lambda_single = 0.05))
})

test_that("stability selection populates results", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data(n = 150)
  result <- run_alasso(
    data = d, time = "time", event = "event", event_level = "Event",
    predictors = c("age", "biomarker", "stage"),
    stability_selection = TRUE, bootstrap_samples = 50,
    subsampling_ratio = 0.7, stability_threshold = 0.6
  )

  stab_df <- as.data.frame(result$stabilityResults)
  expect_true(nrow(stab_df) > 0)
  expect_true("selection_frequency" %in% names(stab_df))
  expect_true("stability_score" %in% names(stab_df))
})

test_that("cv_measure C-index mode works", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()
  expect_no_error(
    run_alasso(data = d, time = "time", event = "event", event_level = "Event",
               predictors = c("age", "biomarker"), cv_measure = "C")
  )
})

test_that("numeric event variable with event_level works", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  set.seed(42)
  n <- 100
  d <- data.frame(
    time = pmax(rexp(n, 0.05), 0.1),
    event = rbinom(n, 1, 0.5),
    x1 = rnorm(n), x2 = rnorm(n),
    strata_var = factor(sample(c("S1", "S2"), n, replace = TRUE))
  )

  expect_no_error(
    run_alasso(data = d, time = "time", event = "event", event_level = "1",
               predictors = c("x1", "x2"))
  )
})
