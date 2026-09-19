# ===============================================================
# Tests for curemodels (Cure Models for Long-term Survivors)
# ===============================================================
# Covers: mixture/non-mixture/cuRe/npcure, all options,
#         data validation, edge cases, variable escaping
# ===============================================================


# ── Helper ─────────────────────────────────────────────────────

make_cure_data <- function(n = 150, seed = 42, cure_frac = 0.3) {
  set.seed(seed)
  is_cured <- rbinom(n, 1, cure_frac)
  time <- ifelse(is_cured == 1,
    runif(n, 80, 120),
    rexp(n, rate = 0.05)
  )
  data.frame(
    time      = round(pmax(0.5, time), 1),
    status    = as.integer(!is_cured),
    age       = round(pmin(85, pmax(35, rnorm(n, 62, 10)))),
    treatment = factor(sample(c("A", "B"), n, replace = TRUE)),
    stage     = factor(sample(c("I", "II", "III"), n, replace = TRUE)),
    tumor_size = round(pmax(0.5, rnorm(n, 3.5, 1.5)), 1),
    bg_hazard = round(exp(-10 + 0.08 * round(pmin(85, pmax(35, rnorm(n, 62, 10))))), 6),
    stringsAsFactors = FALSE
  )
}


# ===============================================================
# 1. Mixture Cure Model (smcure)
# ===============================================================

test_that("curemodels mixture model runs", {
  skip_if_not_installed("smcure")
  data <- make_cure_data()

  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = c("age", "treatment"),
      model_type = "mixture"
    )
  })
})

test_that("curemodels mixture with AFT model type", {
  skip_if_not_installed("smcure")
  data <- make_cure_data()

  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = "age",
      model_type = "mixture",
      smcure_model_type = "aft"
    )
  })
})

test_that("curemodels mixture with different link functions", {
  skip_if_not_installed("smcure")
  data <- make_cure_data()

  for (link in c("logit", "probit", "cloglog")) {
    expect_no_error({
      curemodels(
        data = data, time = "time", status = "status",
        predictors = "age",
        model_type = "mixture",
        cure_link = link
      )
    })
  }
})

test_that("curemodels mixture with bootstrap CI", {
  skip_if_not_installed("smcure")
  data <- make_cure_data(n = 80)

  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = "age",
      model_type = "mixture",
      bootstrap_ci = TRUE,
      n_bootstrap = 100  # minimum for speed
    )
  })
})


# ===============================================================
# 2. Non-Mixture Cure Model (flexsurvcure)
# ===============================================================

test_that("curemodels non-mixture model runs", {
  skip_if_not_installed("flexsurvcure")
  data <- make_cure_data()

  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = "age",
      model_type = "nonmixture",
      survival_dist = "weibull"
    )
  })
})

test_that("curemodels non-mixture with different distributions", {
  skip_if_not_installed("flexsurvcure")
  data <- make_cure_data()

  for (dist in c("weibull", "exponential", "lognormal")) {
    expect_no_error({
      curemodels(
        data = data, time = "time", status = "status",
        predictors = "age",
        model_type = "nonmixture",
        survival_dist = dist
      )
    })
  }
})


# ===============================================================
# 3. Intercept-Only Model (no predictors)
# ===============================================================

test_that("curemodels runs without predictors", {
  skip_if_not_installed("smcure")
  data <- make_cure_data()

  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = character(0),
      model_type = "mixture"
    )
  })
})


# ===============================================================
# 4. Display Options
# ===============================================================

test_that("curemodels sensitivity analysis uses cure_threshold", {
  skip_if_not_installed("smcure")
  data <- make_cure_data()

  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = "age",
      model_type = "mixture",
      sensitivity_analysis = TRUE,
      cure_threshold = 48
    )
  })
})

test_that("curemodels goodness of fit option", {
  skip_if_not_installed("smcure")
  data <- make_cure_data()

  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = "age",
      model_type = "mixture",
      goodness_of_fit = TRUE
    )
  })
})


# ===============================================================
# 5. Data Validation
# ===============================================================

test_that("curemodels shows welcome when no variables", {
  data <- make_cure_data()

  # Missing time → welcome message, no crash
  expect_no_error({
    curemodels(
      data = data, time = NULL, status = "status",
      predictors = character(0),
      model_type = "mixture"
    )
  })
})

test_that("curemodels rejects insufficient data", {
  small_data <- data.frame(
    time = c(10, 20, 15, 5, 30),
    status = c(1, 0, 1, 0, 1),
    age = c(50, 60, 55, 65, 45)
  )

  # Should not crash — returns early with notice
  expect_no_error({
    curemodels(
      data = small_data, time = "time", status = "status",
      predictors = "age", model_type = "mixture"
    )
  })
})

test_that("curemodels validates negative times", {
  data <- make_cure_data()
  data$time[1] <- -5

  # Should catch via .validateInputData → error notice
  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = "age", model_type = "mixture"
    )
  })
})

test_that("curemodels validates binary status", {
  data <- make_cure_data()
  data$status[1:5] <- 3  # invalid

  expect_no_error({
    curemodels(
      data = data, time = "time", status = "status",
      predictors = "age", model_type = "mixture"
    )
  })
})


# ===============================================================
# 6. Variable Name Escaping
# ===============================================================

test_that("curemodels handles variable names with spaces", {
  skip_if_not_installed("smcure")
  data <- make_cure_data()
  names(data)[1:2] <- c("Follow Up Time", "Event Status")

  expect_no_error({
    curemodels(
      data = data, time = "Follow Up Time", status = "Event Status",
      predictors = "age", model_type = "mixture"
    )
  })
})


# ===============================================================
# 7. Result Structure
# ===============================================================

test_that("curemodels result has all expected outputs", {
  skip_if_not_installed("smcure")
  data <- make_cure_data()

  result <- curemodels(
    data = data, time = "time", status = "status",
    predictors = "age", model_type = "mixture"
  )

  expected <- c("todo", "warnings", "errors", "summary",
                "modelTable", "cureTable", "cureFractionPlot",
                "survivalPlot", "goodnessOfFit", "sensitivityAnalysis",
                "modelComparison", "interpretation")

  for (comp in expected) {
    expect_true(comp %in% names(result),
                info = paste("Missing output:", comp))
  }
})

test_that("the non-mixture cure fraction is on the probability scale (flexsurvcure res, not res.t)", {
  skip_if_not_installed("flexsurvcure")
  data("curemodels_test", package = "ClinicoPath", envir = environment())
  res <- curemodels(data = curemodels_test, time = "FollowUpMonths", status = "Recurrence",
                    model_type = "nonmixture")
  cure <- res$cureTable$asDF
  ref <- flexsurvcure::flexsurvcure(survival::Surv(FollowUpMonths, Recurrence) ~ 1,
                                    data = curemodels_test, dist = "weibullPH", mixture = FALSE)
  expect_equal(cure$cure_fraction, unname(ref$res["theta", "est"]), tolerance = 1e-3)
  expect_true(cure$cure_fraction > 0 && cure$cure_fraction < 1)   # was -0.90 (logit scale)
  expect_equal(c(cure$cure_ci_lower, cure$cure_ci_upper),
               unname(ref$res["theta", c("L95%", "U95%")]), tolerance = 1e-3)
})

test_that("an intercept-only mixture cure model explains that smcure needs a predictor", {
  skip_if_not_installed("smcure")
  data("curemodels_test", package = "ClinicoPath", envir = environment())
  res <- curemodels(data = curemodels_test, time = "FollowUpMonths", status = "Recurrence",
                    model_type = "mixture")
  expect_match(res$errors$content, "needs at least one predictor", fixed = TRUE)
  expect_false(grepl("subscript out of bounds", res$warnings$content, fixed = TRUE))
})
