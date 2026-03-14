# ===============================================================
# Edge Cases and Error Handling Tests: lassocox
# ===============================================================
#
# Tests data validation, missing data handling, extreme scenarios,
# constant variables, all-censored data, single-variable, and
# special character variable names.
#
# Uses both package datasets and inline helpers for pathological cases.

library(testthat)
library(survival)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------
skip_lassocox_deps <- function() {
  skip_if_not_installed("jmvcore")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("survival")
}

load_small_cohort <- function() {
  data_path <- file.path("../../data", "lassocox_small_cohort.rda")
  if (!file.exists(data_path)) {
    data_path <- system.file("data", "lassocox_small_cohort.rda",
                             package = "ClinicoPath")
  }
  if (data_path == "" || !file.exists(data_path)) {
    skip("lassocox_small_cohort.rda not found")
  }
  env <- new.env()
  load(data_path, envir = env)
  env$lassocox_small_cohort
}

create_minimal_survival <- function(n = 50, p = 5, seed = 123) {
  set.seed(seed)
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(X) <- paste0("var", 1:p)

  true_coef <- c(0.5, -0.3, rep(0, p - 2))
  lp <- X %*% true_coef
  surv_t <- rexp(n, rate = exp(lp))
  cens_t <- rexp(n, rate = 0.3)

  time <- pmin(surv_t, cens_t)
  status <- as.numeric(surv_t <= cens_t)

  data.frame(
    time = time,
    status = factor(status, levels = c(0, 1), labels = c("censored", "event")),
    as.data.frame(X)
  )
}

# ---------------------------------------------------------------
# Tests
# ---------------------------------------------------------------

test_that("lassocox handles missing data in predictors", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 80)

  # Inject ~15% missing in two predictors
  data$var1[sample(80, 12)] <- NA
  data$var3[sample(80, 10)] <- NA

  # Should complete (function does complete-case analysis)
  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = paste0("var", 1:5),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  })
})

test_that("lassocox handles all-censored data gracefully", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 40)
  data$status <- factor(rep("censored", 40), levels = c("censored", "event"))

  # Should produce an error or informative notice (no events)
  expect_error(
    lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = paste0("var", 1:5),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  )
})

test_that("lassocox handles very few events", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 40)
  # Keep only 3 events
  event_idx <- which(data$status == "event")
  if (length(event_idx) > 3) {
    data$status[event_idx[4:length(event_idx)]] <- "censored"
  }

  # Should either error with informative message or run with warning
  expect_condition(
    lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = paste0("var", 1:5),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  )
})

test_that("lassocox handles constant predictor variables", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 60)
  data$var1 <- 5  # constant

  # Should handle by dropping the constant variable
  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = paste0("var", 1:5),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  })
})

test_that("lassocox handles single-level factor variable", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 60)
  data$group <- factor(rep("A", 60))

  # Should handle by dropping the single-level factor

  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = c(paste0("var", 1:3), "group"),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  })
})

test_that("lassocox handles variables with very different scales", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 80)

  # Create extreme scale differences
  data$var1 <- data$var1 * 1e6   # millions
  data$var2 <- data$var2 / 1e6   # millionths

  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = paste0("var", 1:5),
      standardize = TRUE,
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  })
})

test_that("lassocox small cohort dataset runs correctly", {
  skip_lassocox_deps()
  data <- load_small_cohort()

  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "time_months",
      outcome = "event_occurred",
      outcomeLevel = "Yes",
      explanatory = c("age", "gender", "biomarker_a",
                       "biomarker_b", "biomarker_c",
                       "treatment_group", "severity_score"),
      nfolds = 5,
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  })
})

test_that("lassocox handles negative time values appropriately", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 40)
  data$time[1] <- -0.5

  # Should error or handle the negative time
  expect_error(
    lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = paste0("var", 1:5),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  )
})

test_that("lassocox handles zero time values", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 40)
  data$time[1:3] <- 0

  # Function should either handle or error informatively
  expect_condition(
    lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = paste0("var", 1:5),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  )
})

test_that("lassocox handles data with many missing rows", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 100)

  # Make 70% of rows have at least one NA
  for (v in paste0("var", 1:5)) {
    data[[v]][sample(100, 14)] <- NA
  }

  # Complete cases may be very few; should handle gracefully
  n_complete <- sum(complete.cases(data))
  if (n_complete >= 10) {
    expect_no_error({
      result <- lassocox(
        data = data,
        elapsedtime = "time",
        outcome = "status",
        outcomeLevel = "event",
        explanatory = paste0("var", 1:5),
        cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
      )
    })
  } else {
    expect_error(
      lassocox(
        data = data,
        elapsedtime = "time",
        outcome = "status",
        outcomeLevel = "event",
        explanatory = paste0("var", 1:5),
        cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
      )
    )
  }
})

test_that("lassocox handles perfectly correlated predictors", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 60)
  data$var4 <- data$var1  # perfect copy
  data$var5 <- data$var1 * 2 + 3  # linear transform

  # LASSO should handle multicollinearity by selecting one
  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = paste0("var", 1:5),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  })
})

test_that("lassocox handles two-predictor minimum", {
  skip_lassocox_deps()
  data <- create_minimal_survival(n = 60)

  # Minimum 2 predictors required for LASSO
  expect_no_error({
    result <- lassocox(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "event",
      explanatory = c("var1", "var2"),
      cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE
    )
  })
})
