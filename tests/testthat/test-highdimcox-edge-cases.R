# ===============================================================
# Edge Cases and Error Handling Tests: highdimcox
# ===============================================================
#
# Tests boundary conditions, missing data, small samples,
# constant predictors, and error scenarios.
#
# NOTE: jmvcore::enquo captures symbols, so predictor lists must
# be passed inline (not via pre-assigned variables).

library(testthat)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------

skip_highdimcox_deps <- function() {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
}

make_minimal_data <- function(n = 50, p = 10, event_rate = 0.5, seed = 42) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), n, p,
              dimnames = list(NULL, paste0("X", seq_len(p))))
  time <- pmax(0.1, rexp(n, rate = 0.05))
  event <- rbinom(n, 1, event_rate)
  outcome <- factor(event, levels = c(0, 1), labels = c("Alive", "Dead"))

  data.frame(time = time, outcome = outcome, X, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------
# Small sample sizes
# ---------------------------------------------------------------

test_that("highdimcox works at minimum sample size (n=30)", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 30, p = 5, event_rate = 0.5)

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("X1", "X2", "X3", "X4", "X5"),
    cv_folds = 3
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox rejects sample size below minimum (n < 30)", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 20, p = 5)

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("X1", "X2", "X3", "X4", "X5")
  )

  todo_content <- result$todo$content
  expect_true(grepl("30|observations|required", todo_content, ignore.case = TRUE))
})

# ---------------------------------------------------------------
# Missing data handling
# ---------------------------------------------------------------

test_that("highdimcox handles missing values in predictors", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 60, p = 10)

  # Inject missing values
  set.seed(99)
  d$X1[sample(60, 6)] <- NA
  d$X3[sample(60, 3)] <- NA

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("X", 1:10),
    cv_folds = 5
  )

  expect_true(inherits(result, "Group"))
})

test_that("highdimcox handles missing values in time variable", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 60, p = 5)
  d$time[1:5] <- NA

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("X1", "X2", "X3", "X4", "X5"),
    cv_folds = 5
  )

  expect_true(inherits(result, "Group"))
})

# ---------------------------------------------------------------
# Constant predictors
# ---------------------------------------------------------------

test_that("highdimcox removes constant predictors gracefully", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 60, p = 8)
  d$X2 <- 1.0
  d$X5 <- 0.0

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("X", 1:8),
    cv_folds = 5
  )

  expect_true(inherits(result, "Group"))
  todo_content <- result$todo$content
  expect_true(grepl("constant", todo_content, ignore.case = TRUE))
})

test_that("highdimcox rejects if ALL predictors are constant", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 50, p = 3)
  d$X1 <- 5
  d$X2 <- 5
  d$X3 <- 5

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("X1", "X2", "X3")
  )

  todo_content <- result$todo$content
  expect_true(grepl("constant|variance", todo_content, ignore.case = TRUE))
})

# ---------------------------------------------------------------
# Outcome level validation
# ---------------------------------------------------------------

test_that("highdimcox errors when event and censor levels are identical", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 50, p = 5)

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Dead",
    predictors = c("X1", "X2", "X3", "X4", "X5")
  )

  todo_content <- result$todo$content
  expect_true(grepl("different|same", todo_content, ignore.case = TRUE))
})

test_that("highdimcox errors when event level not found in data", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 50, p = 5)

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Deceased",
    censorLevel = "Alive",
    predictors = c("X1", "X2", "X3", "X4", "X5")
  )

  todo_content <- result$todo$content
  expect_true(grepl("No rows|not found|match", todo_content, ignore.case = TRUE))
})

# ---------------------------------------------------------------
# Low event counts
# ---------------------------------------------------------------

test_that("highdimcox warns on very low event counts (< 10)", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 50, p = 5, event_rate = 0.1, seed = 7)

  # Ensure < 10 events
  dead_idx <- which(d$outcome == "Dead")
  if (length(dead_idx) >= 10) {
    d$outcome[dead_idx[6:length(dead_idx)]] <- "Alive"
  }

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = c("X1", "X2", "X3", "X4", "X5"),
    cv_folds = 3
  )

  expect_true(inherits(result, "Group"))
  todo_content <- result$todo$content
  expect_true(grepl("event|unstable", todo_content, ignore.case = TRUE))
})

# ---------------------------------------------------------------
# High-dimensional edge: p >> n
# ---------------------------------------------------------------

test_that("highdimcox handles p >> n scenario (more predictors than obs)", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 40, p = 80, event_rate = 0.6)

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("X", 1:80),
    regularization_method = "lasso",
    cv_folds = 5
  )

  expect_true(inherits(result, "Group"))
})

# ---------------------------------------------------------------
# Ridge retains all variables
# ---------------------------------------------------------------

test_that("highdimcox ridge retains all variables with note", {
  skip_highdimcox_deps()
  d <- make_minimal_data(n = 60, p = 8, event_rate = 0.6)

  result <- highdimcox(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    predictors = paste0("X", 1:8),
    regularization_method = "ridge",
    cv_folds = 5,
    show_coefficients_table = TRUE
  )

  expect_true(inherits(result, "Group"))
})
