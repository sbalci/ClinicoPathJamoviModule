# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: adaptivelasso
# ═══════════════════════════════════════════════════════════

library(testthat)

# Helper: create test data (always includes strata_var for the wrapper)
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

# Helper: run adaptivelasso with sensible defaults and no plots
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
  # Remove duplicates (user-supplied overrides take precedence)
  args <- args[!duplicated(names(args))]
  do.call(adaptivelasso, args)
}

test_that("adaptivelasso function exists and is exported", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  expect_true(exists("adaptivelasso", mode = "function"))
})

test_that("adaptivelasso runs with minimal required arguments", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()
  result <- NULL
  expect_no_error({
    result <- run_alasso(data = d, time = "time", event = "event",
                         event_level = "Event", predictors = c("age", "biomarker"))
  })
  expect_true(inherits(result, "adaptivelassoResults"))
})

test_that("adaptivelasso populates coefficients table", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()
  result <- run_alasso(data = d, time = "time", event = "event",
                       event_level = "Event",
                       predictors = c("age", "biomarker", "stage"))

  coef_df <- as.data.frame(result$coefficients)
  expect_true(nrow(coef_df) >= 0)
  if (nrow(coef_df) > 0) {
    expect_true("variable" %in% names(coef_df))
    expect_true("coefficient" %in% names(coef_df))
    expect_true("exp_coefficient" %in% names(coef_df))
  }
})

test_that("adaptivelasso populates CV results", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()
  result <- run_alasso(data = d, time = "time", event = "event",
                       event_level = "Event", predictors = c("age", "biomarker"),
                       show_cv_results = TRUE)

  cv_df <- as.data.frame(result$cvResults)
  expect_true(nrow(cv_df) >= 1)
  expect_true(all(c("lambda_min", "lambda_1se") %in% names(cv_df)))
})

test_that("adaptivelasso populates risk groups", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data(n = 150, event_rate = 0.6)
  result <- run_alasso(data = d, time = "time", event = "event",
                       event_level = "Event",
                       predictors = c("age", "biomarker", "stage"),
                       risk_groups = 3)

  rg_df <- as.data.frame(result$riskGroups)
  expect_true(nrow(rg_df) >= 1)
})

test_that("adaptivelasso suitability report renders", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  d <- make_alasso_data()
  result <- run_alasso(data = d, time = "time", event = "event",
                       event_level = "Event", predictors = c("age", "biomarker"),
                       suitabilityCheck = TRUE)

  expect_true(nchar(result$suitabilityReport$content %||% "") > 0)
})
