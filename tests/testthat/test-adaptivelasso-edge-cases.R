# ═══════════════════════════════════════════════════════════
# Edge Case and Error Handling Tests: adaptivelasso
# ═══════════════════════════════════════════════════════════

library(testthat)

# Helper: always include strata_var column for the wrapper
make_data <- function(n, ..., seed = 42) {
  set.seed(seed)
  d <- data.frame(..., stringsAsFactors = FALSE)
  d$strata_var <- factor(sample(c("S1", "S2"), n, replace = TRUE))
  d
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

test_that("handles small sample with few events", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  n <- 30
  d <- make_data(n,
    time = pmax(rexp(n, 0.03), 0.1),
    event = factor(c(rep(1, 8), rep(0, n - 8)),
                   levels = c(0, 1), labels = c("C", "E")),
    x1 = rnorm(n), x2 = rnorm(n),
    seed = 99
  )

  expect_no_error(
    run_alasso(data = d, time = "time", event = "event", event_level = "E",
               predictors = c("x1", "x2"), cv_folds = 3)
  )
})

test_that("handles missing data via listwise deletion", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  n <- 120
  set.seed(42)
  d <- make_data(n,
    time = pmax(rexp(n, 0.05), 0.1),
    event = factor(rbinom(n, 1, 0.5), levels = c(0, 1), labels = c("C", "E")),
    x1 = rnorm(n), x2 = rnorm(n)
  )
  d$x1[sample(n, 12)] <- NA

  result <- run_alasso(data = d, time = "time", event = "event", event_level = "E",
                       predictors = c("x1", "x2"))
  expect_true(inherits(result, "adaptivelassoResults"))
})

test_that("handles single predictor", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  n <- 100
  d <- make_data(n,
    time = pmax(rexp(n, 0.05), 0.1),
    event = factor(rbinom(n, 1, 0.5), levels = c(0, 1), labels = c("C", "E")),
    x1 = rnorm(n)
  )

  expect_no_error(
    run_alasso(data = d, time = "time", event = "event", event_level = "E",
               predictors = "x1")
  )
})

test_that("handles all-categorical predictors", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  n <- 120
  d <- make_data(n,
    time = pmax(rexp(n, 0.05), 0.1),
    event = factor(rbinom(n, 1, 0.5), levels = c(0, 1), labels = c("C", "E")),
    grade = factor(sample(c("Low", "Med", "High"), n, replace = TRUE)),
    treatment = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )

  expect_no_error(
    run_alasso(data = d, time = "time", event = "event", event_level = "E",
               predictors = c("grade", "treatment"))
  )
})

test_that("handles many predictors (high-dimensional)", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  n <- 80
  set.seed(42)
  d <- data.frame(
    time = pmax(rexp(n, 0.04), 0.1),
    event = factor(rbinom(n, 1, 0.5), levels = c(0, 1), labels = c("C", "E")),
    strata_var = factor(sample(c("S1", "S2"), n, replace = TRUE))
  )
  for (j in 1:15) d[[paste0("x", j)]] <- rnorm(n)

  expect_no_error(
    run_alasso(data = d, time = "time", event = "event", event_level = "E",
               predictors = paste0("x", 1:15))
  )
})

test_that("handles constant predictor gracefully", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  n <- 100
  d <- make_data(n,
    time = pmax(rexp(n, 0.05), 0.1),
    event = factor(rbinom(n, 1, 0.5), levels = c(0, 1), labels = c("C", "E")),
    x1 = rnorm(n),
    constant = rep(5, n)
  )

  expect_no_error(
    run_alasso(data = d, time = "time", event = "event", event_level = "E",
               predictors = c("x1", "constant"))
  )
})

test_that("handles very few events (EPV < 2)", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  n <- 60
  d <- make_data(n,
    time = pmax(rexp(n, 0.05), 0.1),
    event = factor(c(rep(1, 5), rep(0, n - 5)),
                   levels = c(0, 1), labels = c("C", "E")),
    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n)
  )

  result <- run_alasso(data = d, time = "time", event = "event", event_level = "E",
                       predictors = c("x1", "x2", "x3"), cv_folds = 3)
  expect_true(inherits(result, "adaptivelassoResults"))
})

test_that("random_seed produces reproducible results", {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  if (!exists("adaptivelasso", mode = "function")) skip("not available")

  n <- 100
  set.seed(42)
  d <- make_data(n,
    time = pmax(rexp(n, 0.05), 0.1),
    event = factor(rbinom(n, 1, 0.5), levels = c(0, 1), labels = c("C", "E")),
    x1 = rnorm(n), x2 = rnorm(n)
  )

  r1 <- run_alasso(data = d, time = "time", event = "event", event_level = "E",
                   predictors = c("x1", "x2"), random_seed = 123)
  r2 <- run_alasso(data = d, time = "time", event = "event", event_level = "E",
                   predictors = c("x1", "x2"), random_seed = 123)

  cv1 <- as.data.frame(r1$cvResults)
  cv2 <- as.data.frame(r2$cvResults)
  if (nrow(cv1) > 0 && nrow(cv2) > 0) {
    expect_equal(cv1$lambda_min, cv2$lambda_min, tolerance = 1e-6)
  }
})
