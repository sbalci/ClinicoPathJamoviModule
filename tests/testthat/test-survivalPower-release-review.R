# Regression tests from the survivalPower release review.
#
# These cover defects found by comparing the module against independent ground
# truth (stats::qweibull, numerical integration, gsDesign) rather than against
# the module's own arithmetic.

# Reach the R6 private environment: survivalPower takes no variables, so the
# public wrapper cannot expose the internal helpers.
sp_private <- function(...) {
  opts <- survivalPowerOptions$new(...)
  analysis <- survivalPowerClass$new(options = opts, data = data.frame(x = 1))
  suppressWarnings(try(analysis$run(), silent = TRUE))
  analysis$.__enclos_env__$private
}


test_that("Weibull scale is the true inverse of the requested median", {
  # Regression: lambda was computed as (log2/m)^(1/shape) but the median it is
  # inverted with is m = (log2/lambda)^(1/shape), which requires log2/m^shape.
  # The two agree only at shape = 1, so the error was invisible in the
  # exponential default and grew with the shape (median 12 -> 2.78 at shape 1.5).
  for (shape in c(0.5, 0.8, 1.0, 1.5, 2.0, 3.0)) {
    p <- sp_private(survival_distribution = "weibull", weibull_shape = shape)$
      .get_distribution_parameters(median_control = 12, hr = 0.7)

    implied_median <- (log(2) / p$lambda_control)^(1 / shape)
    expect_equal(implied_median, 12, tolerance = 1e-8,
                 info = paste("shape =", shape))
  }
})

test_that("Weibull medians match stats::qweibull", {
  # Ours: S(t) = exp(-lambda * t^shape). R's: S(t) = exp(-(t/scale)^shape).
  # So scale = lambda^(-1/shape).
  for (shape in c(0.8, 1.5, 2.5)) {
    p <- sp_private(survival_distribution = "weibull", weibull_shape = shape)$
      .get_distribution_parameters(median_control = 12, hr = 0.7)

    expect_equal(qweibull(0.5, shape = shape, scale = p$lambda_control^(-1 / shape)),
                 p$median_control, tolerance = 1e-8)
    expect_equal(qweibull(0.5, shape = shape, scale = p$lambda_treatment^(-1 / shape)),
                 p$median_treatment, tolerance = 1e-8)
  }
})

test_that("Weibull treatment scale encodes proportional hazards", {
  # h(t) = lambda * shape * t^(shape-1), so a shared shape means the hazard
  # ratio is exactly lambda_treatment / lambda_control. The previous
  # hr^(1/shape) scaling belongs to the S(t) = exp(-(lambda*t)^shape)
  # parameterisation, which is not the one the simulator draws from.
  for (shape in c(0.8, 1.5, 2.5)) {
    for (hr in c(0.5, 0.7, 1.3)) {
      p <- sp_private(survival_distribution = "weibull", weibull_shape = shape)$
        .get_distribution_parameters(median_control = 12, hr = hr)
      expect_equal(p$lambda_treatment / p$lambda_control, hr, tolerance = 1e-10,
                   info = paste("shape =", shape, "hr =", hr))
    }
  }
})

test_that("Simulator accepts the parameter names the getter actually returns", {
  # Regression: the simulator read params$lambda and params$mu. Because the
  # getter returns lambda_control AND lambda_treatment, `$` partial matching was
  # ambiguous and silently yielded NULL -- the error was then swallowed by a
  # tryCatch and the validation table rendered empty with no explanation.
  for (dist in c("exponential", "weibull", "log_normal")) {
    params <- sp_private(survival_distribution = dist, weibull_shape = 1.5)$
      .get_distribution_parameters(median_control = 12, hr = 0.7)

    set.seed(11)
    sim <- .simulate_survival_trial(
      n = 300, distribution = dist, params = params, hr = 0.7,
      allocation_ratio = 1, accrual_period = 24, follow_up_period = 12,
      dropout_rate = 0.05)

    expect_equal(nrow(sim), 300, info = dist)
    expect_true(all(is.finite(sim$time)), info = dist)
    expect_true(sum(sim$event) > 0, info = dist)
  }
})

test_that("Simulated Weibull data recovers the requested median", {
  # End-to-end check that the getter and the simulator share a parameterisation.
  for (shape in c(1.0, 1.5, 2.0)) {
    p <- sp_private(survival_distribution = "weibull", weibull_shape = shape)$
      .get_distribution_parameters(median_control = 12, hr = 0.7)

    set.seed(99)
    draws <- (-log(runif(2e5)) / p$lambda_control)^(1 / shape)
    expect_equal(median(draws), 12, tolerance = 0.05,
                 info = paste("shape =", shape))
  }
})

test_that("Seeded simulation validation is reproducible", {
  # Regression: nothing seeded the Monte Carlo run, so repeated runs of an
  # identical design disagreed by several points and the reported agreement
  # verdict could flip between "Excellent" and "Outside CI" on noise alone.
  params <- list(
    sample_size = 400, distribution = "exponential",
    dist_params = list(lambda_control = log(2) / 12,
                       lambda_treatment = log(2) / 12 * 0.7),
    hr = 0.7, allocation_ratio = 1, accrual_period = 24, follow_up = 12,
    dropout_rate = 0.05, alpha = 0.05)

  runs <- vapply(1:3, function(i) {
    set.seed(42)
    .validate_power_by_simulation(params, n_sims = 200)$simulated_power
  }, numeric(1))

  expect_length(unique(runs), 1)
})

test_that("Simulated power agrees with the analytical power it validates", {
  # If these disagree the validator is not validating anything.
  h <- sp_private()
  event_prob <- h$.overall_event_probability(log(2) / 12, 0.7, 1, 24, 12, 0.05)$overall

  params <- list(
    sample_size = 500, distribution = "exponential",
    dist_params = list(lambda_control = log(2) / 12,
                       lambda_treatment = log(2) / 12 * 0.7),
    hr = 0.7, allocation_ratio = 1, accrual_period = 24, follow_up = 12,
    dropout_rate = 0.05, alpha = 0.05)

  set.seed(42)
  sim <- .validate_power_by_simulation(params, n_sims = 2000)
  analytical <- h$.power_from_events(500 * event_prob, 0.7, 0.05, 1)

  expect_gte(analytical, sim$ci_lower)
  expect_lte(analytical, sim$ci_upper)
})

test_that("Schoenfeld event counts match gsDesign", {
  skip_if_not_installed("gsDesign")
  h <- sp_private()

  for (cfg in list(c(0.75, 0.05, 0.80, 1), c(0.50, 0.05, 0.90, 1),
                   c(0.67, 0.05, 0.80, 2), c(0.80, 0.01, 0.90, 1))) {
    hr <- cfg[1]; alpha <- cfg[2]; power <- cfg[3]; ratio <- cfg[4]
    expect_equal(
      h$.events_needed_log_rank(hr = hr, alpha = alpha, power = power, ratio = ratio),
      ceiling(gsDesign::nEvents(hr = hr, alpha = alpha / 2, beta = 1 - power,
                                ratio = ratio, tbl = FALSE)),
      info = paste("hr =", hr, "alpha =", alpha))
  }
})

test_that("Event probability matches numerical integration over accrual", {
  h <- sp_private()
  for (cfg in list(c(12, 24, 12, 0.05), c(12, 24, 12, 0), c(24, 36, 6, 0.20))) {
    median_ctrl <- cfg[1]; accrual <- cfg[2]; fu <- cfg[3]; dropout <- cfg[4]
    lambda <- log(2) / median_ctrl
    total_hazard <- lambda + if (dropout > 0) -log(1 - dropout) / 12 else 0

    expect_equal(
      h$.event_probability(lambda = lambda, accrual_period = accrual,
                           follow_up_period = fu, dropout_rate = dropout),
      integrate(function(u) {
        (lambda / total_hazard) * (1 - exp(-total_hazard * (fu + accrual - u))) / accrual
      }, 0, accrual)$value,
      tolerance = 1e-8)
  }
})
