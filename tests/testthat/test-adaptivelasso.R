library(testthat)

test_that("adaptivelasso runs with explicit event level and custom lambda range", {
  skip_if_not_installed("jmvcore")
  if (!exists("adaptivelasso", mode = "function")) {
    skip("adaptivelasso() not available in this test session")
  }

  set.seed(123)
  n <- 180
  data <- data.frame(
    time = rexp(n, rate = 0.05) + 0.1,
    event = factor(sample(c("censored", "event"), n, replace = TRUE, prob = c(0.4, 0.6))),
    age = rnorm(n, mean = 62, sd = 10),
    stage = factor(sample(c("I", "II", "III"), n, replace = TRUE)),
    biomarker = rnorm(n),
    strata = factor(sample(c("A", "B"), n, replace = TRUE))
  )

  model <- NULL
  expect_no_error({
    model <- adaptivelasso(
      data = data,
      time = "time",
      event = "event",
      event_level = "event",
      predictors = c("age", "stage", "biomarker"),
      strata = "strata",
      cv_folds = 5,
      lambda_sequence = "custom",
      lambda_custom_max = 1,
      lambda_custom_min = 0.01,
      n_lambda = 25,
      stability_selection = TRUE,
      bootstrap_samples = 50,
      subsampling_ratio = 0.7,
      baseline_survival = TRUE,
      parallel_computing = FALSE,
      plot_selection_path = FALSE,
      plot_cv_curve = FALSE,
      plot_stability = FALSE,
      plot_survival_curves = FALSE,
      plot_baseline_hazard = FALSE,
      plot_diagnostics = FALSE,
      random_seed = 123
    )
  })

  expect_true(inherits(model, "adaptivelassoResults"))

  cv_df <- as.data.frame(model$cvResults)
  expect_true(nrow(cv_df) >= 1)
  expect_true(all(c("lambda_min", "lambda_1se") %in% names(cv_df)))

  stab_df <- as.data.frame(model$stabilityResults)
  if (nrow(stab_df) > 0) {
    expect_false(any(grepl("^s\\d+$", stab_df$variable)))
  }
})

test_that("adaptivelasso works when strata is omitted and single lambda mode is selected", {
  skip_if_not_installed("jmvcore")
  if (!exists("adaptivelasso", mode = "function")) {
    skip("adaptivelasso() not available in this test session")
  }

  set.seed(321)
  n <- 140
  data <- data.frame(
    time = rexp(n, rate = 0.07) + 0.1,
    event = factor(sample(c("no", "yes"), n, replace = TRUE, prob = c(0.45, 0.55))),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = factor(sample(c("low", "high"), n, replace = TRUE))
  )

  model <- NULL
  expect_no_error({
    model <- adaptivelasso(
      data = data,
      time = "time",
      event = "event",
      event_level = "yes",
      predictors = c("x1", "x2", "x3"),
      lambda_sequence = "single",
      lambda_single = 0.05,
      cv_folds = 5,
      n_lambda = 20,
      stability_selection = FALSE,
      plot_selection_path = FALSE,
      plot_cv_curve = FALSE,
      plot_stability = FALSE,
      plot_survival_curves = FALSE,
      plot_baseline_hazard = FALSE,
      plot_diagnostics = FALSE,
      random_seed = 321
    )
  })

  expect_true(inherits(model, "adaptivelassoResults"))
  coef_df <- as.data.frame(model$coefficients)
  expect_true(nrow(coef_df) >= 1)
})
