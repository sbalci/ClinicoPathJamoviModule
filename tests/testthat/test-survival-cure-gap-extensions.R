# Independent likelihood and probability checks for the cure-review extensions.

.survival_gap_root <- function() {
  candidates <- c(".", "..", "../..")
  found <- candidates[file.exists(file.path(candidates, "R", "utils-multistate-history.R"))]
  testthat::skip_if(!length(found), "Analysis sources are unavailable")
  normalizePath(found[1])
}

.survival_gap_env <- function(wrappers = character()) {
  root <- .survival_gap_root()
  e <- new.env(parent = globalenv())
  for (f in c("intervalsurvival-estimation.R", "utils-multistate-history.R",
              "flexrstpm2-estimation.R")) sys.source(file.path(root, "R", f), e)
  if (any(wrappers %in% c("singlearm", "survival"))) {
    e$`.` <- get(".", asNamespace("jmvcore"))
    e$Surv <- survival::Surv
    e$`%>%` <- magrittr::`%>%`
    for (f in c("utils.R", "utils-formula.R", "utils-followup.R", "utils-eventindicator.R",
                "utils-observed-cif.R", "survival-competingrisks.R"))
      sys.source(file.path(root, "R", f), e)
  }
  if (length(wrappers)) {
    testthat::skip_if_not_installed("jmvcore")
    for (n in wrappers) for (suffix in c(".h.R", ".b.R"))
      sys.source(file.path(root, "R", paste0(n, suffix)), e)
  }
  e
}

.survival_gap_history <- function() {
  data.frame(id = c("A", "A", "B", "C", "C", "D"),
    entry = c(0, 1, 0, 0, 2, 0), exit = c(1, 3, 2, 2, 4, 4),
    from = c(0, 1, 0, 0, 1, 0), to = c(1, 2, 2, 1, 1, 0))
}

.survival_gap_intervals <- function() {
  data.frame(left = c(1, 2, 1, 0, 3, 2, 4, 5),
              right = c(1, 3, Inf, 1, 3, 4, Inf, 6))
}

.survival_gap_right_censored <- function() {
  # Deterministic, smoothly varying event times with an actual censored tail.
  u <- ((1:180) - .5) / 180
  x <- rep(c(0, 1), 90)
  event_time <- (-log(1 - u) / exp(.6 * x))^(1 / 1.4)
  censor <- rep(c(1.2, 2.1, 2.8), 60)
  data.frame(time = pmin(event_time, censor), event = as.integer(event_time <= censor), x = x)
}

test_that("interval preparation retains infinite censoring and exact endpoints", {
  e <- .survival_gap_env()
  d <- .survival_gap_intervals()
  out <- e$.intervalSurvivalData(d$left, d$right)
  expect_equal(out$data, d)
  expect_identical(out$excluded, 0L)
  expect_equal(sum(is.infinite(out$data$right)), 2)
  coded <- e$.intervalSurvivalData(c(1, 2, 3, NA), c(NA, 4, 9, 5), c(0, 1, 2, 3))
  expect_equal(coded$data$left, c(1, 2, 0, 5))
  expect_equal(coded$data$right, c(Inf, 4, 9, 5))
  expect_error(e$.intervalSurvivalData(0, Inf), "no event-time information")
  expect_error(e$.intervalSurvivalData(2, 1), "upper bounds")
})

test_that("icenReg exponential fit matches the full mixed-censoring likelihood", {
  skip_if_not_installed("icenReg")
  e <- .survival_gap_env()
  d <- .survival_gap_intervals()
  negative_loglik <- function(log_rate) {
    rate <- exp(log_rate)
    exact <- d$left == d$right
    right <- is.infinite(d$right)
    interval <- !exact & !right
    contribution <- numeric(nrow(d))
    contribution[exact] <- log_rate - rate * d$left[exact]
    contribution[right] <- -rate * d$left[right]
    contribution[interval] <- -rate * d$left[interval] +
      log(-expm1(-rate * (d$right[interval] - d$left[interval])))
    -sum(contribution)
  }
  oracle <- stats::optimize(negative_loglik, c(-5, 3), tol = 1e-10)
  fit <- e$.intervalSurvivalFit(d, "aft_exponential")
  rate <- exp(-unname(stats::coef(fit)["log_scale"]))
  expect_equal(rate, exp(oracle$minimum), tolerance = 1e-5)
  expect_equal(fit$llk, -oracle$objective, tolerance = 1e-7)
  prediction <- e$.intervalSurvivalPredict(fit, d, c(1, 2, 4))
  expect_equal(prediction, exp(-rate * c(1, 2, 4)), tolerance = 1e-8)
  expect_true(all(is.finite(prediction)))
})

test_that("nonparametric interval fit uses Turnbull support", {
  skip_if_not_installed("icenReg")
  e <- .survival_gap_env()
  d <- .survival_gap_intervals()
  fit <- e$.intervalSurvivalFit(d, "nonparametric")
  times <- c(1, 2, 3, 5)
  predicted <- e$.intervalSurvivalPredict(fit, d, times, nonparametric = TRUE)
  expect_equal(predicted, 1 - icenReg::getFitEsts(fit, q = times), tolerance = 1e-10)
  expect_true(all(diff(predicted) <= 0))
  curves <- icenReg::getSCurves(fit)
  expect_true(ncol(curves$Tbull_ints) == 2)
})

test_that("interval dataframe wrapper retains right-censored observations", {
  skip_if_not_installed("icenReg")
  e <- .survival_gap_env("intervalsurvival")
  d <- .survival_gap_intervals()
  r <- e$intervalsurvival(data = d, left_time = "left", right_time = "right",
    model_type = "aft_exponential", prediction_times = "1,2,4")
  expect_match(r$model_results$content, "full interval likelihood", fixed = TRUE)
  expect_equal(nrow(r$interval_plot$state), nrow(d))
  expect_equal(sum(r$interval_plot$state$censored), 2)
  curve <- r$survival_curves_plot$state
  fit <- e$.intervalSurvivalFit(d, "aft_exponential")
  rate <- exp(-unname(stats::coef(fit)["log_scale"]))
  expect_equal(curve$survival, exp(-rate * curve$time), tolerance = 1e-8)
})

test_that("four-subject histories have correct outgoing exposure and occupation", {
  skip_if_not_installed("survival")
  e <- .survival_gap_env()
  d <- .survival_gap_history()
  h <- e$.multistateHistory(d, "entry", "exit", "from", "to", "id")
  expect_length(h$covariates, 0)
  expect_equal(vapply(h$risk, function(x) sum(x$stop - x$start), 0), c(9, 9, 4))
  expect_equal(vapply(h$risk, function(x) sum(x$event), 0), c(2, 1, 1))
  aj <- e$.multistateAJ(h, c(1, 2, 3, 4))
  oracle <- rbind(c(.75, .25, 0), c(.25, .5, .25), c(.25, .25, .5), c(.25, .25, .5))
  expect_equal(unname(aj$probability), oracle)
  expect_equal(unname(e$.multistateRestricted(aj, 4)), c(2.25, 1, .75))
  expect_equal(unname(rowSums(aj$probability)), rep(1, 4))
  unsupported <- e$.multistateAJ(h, c(10, 20))
  expect_length(unsupported$time, 0)
  expect_equal(dim(unsupported$probability), c(0L, 3L))
})

test_that("broken multistate histories are rejected without dropping rows", {
  e <- .survival_gap_env()
  d <- .survival_gap_history()
  d$entry[2] <- .5
  expect_error(e$.multistateHistory(d, "entry", "exit", "from", "to", "id"), "contiguous")
  d <- .survival_gap_history()
  d$from[2] <- 2
  expect_error(e$.multistateHistory(d, "entry", "exit", "from", "to", "id"), "leaves death")
  d <- .survival_gap_history()
  d$to[2] <- NA_real_
  expect_error(e$.multistateHistory(d, "entry", "exit", "from", "to", "id"), "cannot be dropped")
})

test_that("restricted-state bootstrap resamples complete subject trajectories", {
  skip_if_not_installed("survival")
  e <- .survival_gap_env()
  h <- e$.multistateHistory(.survival_gap_history(), "entry", "exit", "from", "to", "id")
  real_aj <- e$.multistateAJ
  histories_checked <- 0L
  e$.multistateAJ <- function(history, times, ...) {
    d <- history$data
    expect_equal(length(unique(d$id)), 4)
    checked <- e$.multistateHistory(d, "start", "stop", "from", "to", "id")
    expect_equal(nrow(checked$data), nrow(d))
    histories_checked <<- histories_checked + 1L
    real_aj(history, times, ...)
  }
  set.seed(720)
  before <- .Random.seed
  a <- e$.multistateRestrictedBootstrap(h, 4, B = 20L, seed = 88)
  expect_identical(.Random.seed, before)
  expect_identical(histories_checked, 20L)
  b <- e$.multistateRestrictedBootstrap(h, 4, B = 20L, seed = 88)
  expect_equal(a, b)
  valid <- stats::complete.cases(a)
  expect_gt(sum(valid), 10)
  expect_equal(as.numeric(rowSums(a[valid, , drop = FALSE])), rep(4, sum(valid)))
})

test_that("illnessdeath wrapper reports the four-subject oracle", {
  skip_if_not_installed("survival")
  e <- .survival_gap_env("illnessdeath")
  r <- e$illnessdeath(data = .survival_gap_history(), time_entry = "entry", time_exit = "exit",
    state_from = "from", state_to = "to", subject_id = "id", covariates = NULL,
    time_varying_covariates = NULL, stratification_variable = NULL,
    prediction_times = "1,2,3,4", prediction_horizon = 4)
  expect_identical(r$todo$content, "")
  p <- r$stateProbabilities$asDF
  expect_equal(p$prob_healthy, c(.75, .25, .25, .25))
  expect_equal(p$prob_dead, c(0, .25, .5, .5))
  expect_equal(r$sojournAnalysis$asDF$mean_sojourn, c(2.25, 1, .75))
  expect_equal(r$transitionSummary$asDF$total_time, c(9, 9, 4))
})

test_that("parametric multistate wrapper matches a constant-rate matrix oracle", {
  skip_if_not_installed("flexsurv")
  e <- .survival_gap_env("flexmultistate")
  r <- e$flexmultistate(data = .survival_gap_history(), subject_id = "id", time_entry = "entry",
    time = "exit", status = "to", start_state = "from", predictors = NULL,
    shared_effects = NULL, time_varying_effects = NULL, hazard_distribution = "piecewise_exp",
    prediction_times = "1,2,3,4", confidence_intervals = FALSE)
  expect_identical(r$todo$content, "")
  p <- r$stateProbTable$asDF
  # MLE rates: lambda01=2/9, lambda02=1/9, lambda12=1/4.
  t <- 1:4
  p0 <- exp(-t / 3)
  p1 <- (2 / 9) / (1 / 4 - 1 / 3) * (exp(-t / 3) - exp(-t / 4))
  expect_equal(p$probability[p$state == "Event-free"], p0, tolerance = 1e-5)
  expect_equal(p$probability[p$state == "Ill"], p1, tolerance = 1e-5)
  expect_equal(p$probability[p$state == "Dead"], 1 - p0 - p1, tolerance = 1e-5)
  expect_equal(sum(r$sojournTable$asDF$expected_time), 4, tolerance = 1e-5)
})

test_that("singlearm and grouped survival wrappers both populate observed CIF tables", {
  skip_if_not_installed("cmprsk")
  skip_if_not_installed("magrittr")
  e <- .survival_gap_env(c("singlearm", "survival"))
  d <- data.frame(time = rep(1:4, 2),
    out = factor(rep(c("Event", "Competing", "Censored", "Event"), 2),
                 levels = c("Censored", "Event", "Competing")),
    group = factor(rep(c("A", "B"), each = 4)))
  args <- list(data = d, elapsedtime = "time", outcome = "out", outcomeLevel = "Event",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL, cutp = "1,2,3,4", multievent = FALSE)
  single <- do.call(e$singlearm, args)
  expect_true(single$cifTable$visible)
  tab <- single$cifTable$asDF
  expect_equal(nrow(tab), 12)
  expect_equal(tab$estimate[tab$time == 4], c(0, .75, .25))
  expect_equal(tab$n_risk[tab$time == 3], rep(4, 3))
  grouped <- do.call(e$survival, c(args, list(explanatory = "group")))
  expect_true(grouped$cifTable$visible)
  tab <- grouped$cifTable$asDF
  expect_equal(nrow(tab), 24)
  for (g in c("A", "B")) {
    expect_equal(tab$estimate[tab$group == g & tab$time == 4], c(0, .75, .25))
    expect_equal(tab$n_risk[tab$group == g & tab$time == 3], rep(2, 3))
  }
})

test_that("singlearm CIF renderer draws computed risk counts from image state", {
  skip_if_not_installed("cmprsk")
  skip_if_not_installed("survminer")
  skip_if_not_installed("ggpubr")
  e <- .survival_gap_env("singlearm")
  d <- data.frame(time = rep(1:4, 2),
    out = factor(rep(c("Event", "Competing", "Censored", "Event"), 2),
                 levels = c("Censored", "Event", "Competing")))
  make_analysis <- function(risk) {
    options <- e$singlearmOptions$new(elapsedtime = "time", outcome = "out",
      outcomeLevel = "Event", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
      sc = TRUE, risktable = risk, endplot = 5, byplot = 1, cutp = "1,2,3,4")
    analysis <- e$singlearmClass$new(options = options, data = d)
    analysis$run()
    analysis
  }
  real_arrange <- ggpubr::ggarrange
  arranged <- 0L
  risk_data <- NULL
  testthat::local_mocked_bindings(ggarrange = function(...) {
    plots <- list(...)
    risk_data <<- plots[[2]]$data
    arranged <<- arranged + 1L
    real_arrange(...)
  }, .package = "ggpubr")
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({ grDevices::dev.off(); unlink(output) }, add = TRUE)
  with_risk <- make_analysis(TRUE)
  expect_true(with_risk$.__enclos_env__$private$.plotCIF(
    with_risk$results$plot_cif, ggplot2::theme_bw(), NULL))
  expect_identical(arranged, 1L)
  expect_equal(risk_data$time, 0:4)
  expect_equal(risk_data$n, c(8, 8, 6, 4, 2))
  without_risk <- make_analysis(FALSE)
  expect_true(without_risk$.__enclos_env__$private$.plotCIF(
    without_risk$results$plot_cif, ggplot2::theme_bw(), NULL))
  expect_identical(arranged, 1L)
})

test_that("rstpm2 design expands time-varying factors and rejects competing codes", {
  e <- .survival_gap_env()
  d <- .survival_gap_right_censored()
  d$grade <- factor(rep(c("low", "mid", "high"), 60), levels = c("low", "mid", "high"))
  design <- e$.flexRstpmDesign(d, "time", "event", "1", c("grade", "x"), tvc = "grade")
  expect_length(design$columns, 3)
  expect_length(design$tvc, 2)
  expect_equal(design$data$event, d$event)
  d$event[1] <- 2
  expect_error(e$.flexRstpmDesign(d, "time", "event", "1", "x"), "competing")
})

test_that("rstpm2 predictions and uncertainty are real fitted model predictions", {
  skip_if_not_installed("rstpm2")
  e <- .survival_gap_env()
  design <- e$.flexRstpmDesign(.survival_gap_right_censored(), "time", "event", "1", "x")
  d <- design$data
  for (scale in c("hazard", "odds", "normal")) {
    fit <- e$.flexRstpmFit(d, design$columns, df = 2L, scale = scale)
    profile <- d[1, , drop = FALSE]
    profile$x1 <- 0
    times <- c(.3, .7, 1.4)
    predicted <- e$.flexRstpmPredict(fit, profile, times, level = .8)
    nd <- profile[rep(1, length(times)), , drop = FALSE]
    nd$time <- times
    reference <- rstpm2::predict(fit, newdata = nd, type = "surv", se.fit = TRUE, level = .8)
    expect_equal(predicted$estimate, as.numeric(reference$Estimate), tolerance = 1e-10)
    expect_equal(predicted$lower, as.numeric(reference$lower), tolerance = 1e-10)
    expect_equal(predicted$upper, as.numeric(reference$upper), tolerance = 1e-10)
    expect_true(all(diff(predicted$estimate) < 0))
    wide <- e$.flexRstpmPredict(fit, profile, times, level = .95)
    expect_true(all(wide$lower <= predicted$lower & wide$upper >= predicted$upper))
  }
})

test_that("rstpm2 generated wrapper reports fitted predictions and endpoint guards", {
  skip_if_not_installed("rstpm2")
  e <- .survival_gap_env("flexrstpm2")
  d <- .survival_gap_right_censored()
  args <- list(data = d, elapsedtime = "time", outcome = "event", covariates = "x",
    time_varying_covariates = NULL, bhazard = NULL, group_variable = NULL, df = 2L,
    prediction_times = ".3,.7,1.4", extrapolation_time = 3, model_comparison = FALSE)
  r <- do.call(e$flexrstpm2, args)
  expect_identical(r$todo$content, "")
  table <- r$survivalPredictions$asDF
  expect_equal(table$time_point, c(.3, .7, 1.4))
  expect_true(all(is.finite(table$survival_probability) & diff(c(1, table$survival_probability)) < 0))
  r <- do.call(e$flexrstpm2, c(args, list(endpoint_type = "all_cause_death", cure_fraction = TRUE)))
  expect_match(r$todo$content, "not a cure estimand", fixed = TRUE)
  expect_equal(nrow(r$survivalPredictions$asDF), 0)
})

test_that("rstpm2 refuses invalid engine predictions and unsupported inputs", {
  skip_if_not_installed("rstpm2")
  e <- .survival_gap_env()
  profile <- data.frame(time = 1, x1 = 0)
  expect_error(e$.flexRstpmPredict(NULL, profile, 0), "finite and positive")
  expect_error(e$.flexRstpmPredict(NULL, profile, 1, level = 1), "between zero and one")
  value <- NA_real_
  testthat::local_mocked_bindings(predict = function(...) {
    data.frame(Estimate = value, lower = 0, upper = 1)
  }, .package = "rstpm2")
  for (bad in c(NA_real_, -1, 1.1)) {
    value <- bad
    expect_error(e$.flexRstpmPredict(NULL, profile, 1), "invalid predictions")
  }
})

.survival_gap_spline_method <- function(e, scale = "hazard", placement = "quantiles", manual = "",
                                       tvc = FALSE, df = 3L) {
  options <- list(spline_type = scale, spline_df = df, knot_placement = placement,
                  manual_knots = manual, time_varying_effects = tvc)
  f <- e$flexparametricClass$private_methods$.fitSplineModel
  environment(f) <- list2env(list(self = list(options = options)), parent = e)
  f
}

test_that("flexparametric uses the requested spline scale and actual knot basis", {
  skip_if_not_installed("flexsurv")
  e <- .survival_gap_env("flexparametric")
  d <- .survival_gap_right_censored()
  f <- stats::as.formula("Surv(time, event) ~ x", env = asNamespace("survival"))
  bounds <- range(log(d$time[d$event == 1 & d$time > 0]))
  for (scale in c("hazard", "odds", "normal")) {
    actual <- .survival_gap_spline_method(e, scale)(f, d)
    reference <- flexsurv::flexsurvspline(f, data = d, k = 2L, bknots = bounds, scale = scale)
    expect_equal(stats::coef(actual), stats::coef(reference), tolerance = 1e-7)
    expect_equal(actual$loglik, reference$loglik, tolerance = 1e-7)
    expect_equal(actual$aux$scale, scale)
  }
})

test_that("invalid spline requests fail without substituting a distribution", {
  skip_if_not_installed("flexsurv")
  e <- .survival_gap_env("flexparametric")
  d <- .survival_gap_right_censored()
  f <- stats::as.formula("Surv(time, event) ~ x", env = asNamespace("survival"))
  fit <- .survival_gap_spline_method(e, placement = "manual", manual = "1000")
  expect_error(fit(f, d), "strictly inside")
  fit <- .survival_gap_spline_method(e, placement = "manual", manual = "oops")
  expect_error(fit(f, d), "positive times")
  d$time[d$event == 1] <- 1
  expect_error(.survival_gap_spline_method(e)(f, d), "three distinct")
})

test_that("time-varying spline predictions agree with the requested flexsurv model", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("ggplot2")
  e <- .survival_gap_env("flexparametric")
  n <- 240L
  x <- rep(c(0, 1), n / 2)
  u <- (((1:n) * 137) %% n + .5) / n
  event_time <- (-log(1 - u) / exp(.4 * x))^(1 / (1.2 + .6 * x))
  censor <- rep(c(1.5, 2.5, 3.5), n / 3)
  d <- data.frame(time = pmin(event_time, censor), event = as.integer(event_time <= censor), x = x)
  f <- stats::as.formula("Surv(time, event) ~ x", env = asNamespace("survival"))
  actual <- .survival_gap_spline_method(e, scale = "odds", tvc = TRUE, df = 2L)(f, d)
  bounds <- range(log(d$time[d$event == 1]))
  reference <- flexsurv::flexsurvspline(f, data = d, k = 1L, bknots = bounds,
    scale = "odds", anc = list(gamma1 = ~x, gamma2 = ~x))
  expect_equal(stats::coef(actual), stats::coef(reference), tolerance = 1e-7)
  expect_true(all(c("gamma1(x)", "gamma2(x)") %in% names(stats::coef(actual))))
  predictions <- summary(actual, newdata = data.frame(x = c(0, 1)),
                          t = c(.3, .8, 1.5), type = "hazard", ci = FALSE)
  hr <- predictions[[2]]$est / predictions[[1]]$est
  expect_gt(diff(range(hr)), .5)
  # The displayed curve uses the stated mean-covariate profile, not an invented curve.
  renderer <- e$flexparametricClass$private_methods$.createSurvivalPlot
  environment(renderer) <- list2env(list(self = list(options = list(
    plot_time_max = 3, covariates = "x", confidence_level = .95))),
    parent = asNamespace("ggplot2"))
  plot <- suppressWarnings(renderer(list(model = actual, data = d, distribution = "Odds spline")))
  direct <- summary(reference, newdata = data.frame(x = mean(d$x)), t = plot$data$time,
                     type = "survival", ci = FALSE)[[1]]
  expect_equal(plot$data$survival, direct$est, tolerance = 1e-8)
})

test_that("flexparametric plot state is reproducible and preserves caller RNG", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("ggplot2")
  e <- .survival_gap_env("flexparametric")
  for (name in c("ggplot", "aes", "geom_line", "geom_ribbon", "labs",
                 "scale_y_continuous", "theme_minimal", "theme", "element_text"))
    e[[name]] <- get(name, asNamespace("ggplot2"))
  d <- .survival_gap_right_censored()
  f <- stats::as.formula("Surv(time, event) ~ x", env = asNamespace("survival"))
  model <- .survival_gap_spline_method(e, scale = "odds", df = 2L)(f, d)
  fitted <- list(model = model, data = d, distribution = "Odds spline")
  options <- e$flexparametricOptions$new(elapsedtime = "time", outcome = "event",
    covariates = "x", plot_time_max = 4, show_survival_plot = TRUE)
  analysis <- e$flexparametricClass$new(options = options, data = d)
  private <- analysis$.__enclos_env__$private
  set.seed(693)
  before <- .Random.seed
  suppressWarnings(private$.populatePlots(fitted, d))
  expect_identical(.Random.seed, before)
  state <- analysis$results$survivalPlot$state
  expect_named(state, c("time", "estimate", "lower", "upper", "quantity", "observed", "family"))
  expect_equal(state$observed, state$time <= max(d$time))
  expect_true(any(!state$observed))
  suppressWarnings(private$.populatePlots(fitted, d))
  expect_identical(analysis$results$survivalPlot$state, state)
  expect_identical(.Random.seed, before)
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit({ grDevices::dev.off(); unlink(output) }, add = TRUE)
  # Rendering after serialization requires only the compact saved data frame.
  image <- list(state = unserialize(serialize(state, NULL)))
  expect_true(private$.renderCurve(image, ggplot2::theme_bw(), NULL))
  expect_identical(.Random.seed, before)
})
