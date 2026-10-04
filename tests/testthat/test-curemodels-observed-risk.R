# Numerical regressions for cure semantics and observed-time competing risks.
# Source only these analyses: the umbrella has unrelated optional dependencies.

.cure_review_root <- function() {
  candidates <- c(".", "..", "../..")
  found <- candidates[file.exists(file.path(candidates, "R", "curemodels-estimation.R"))]
  testthat::skip_if(!length(found), "Analysis sources are unavailable")
  normalizePath(found[1])
}

.cure_review_env <- function(wrapper = FALSE) {
  root <- .cure_review_root()
  e <- new.env(parent = globalenv())
  files <- c("utils-observed-cif.R", "curemodels-estimation.R")
  if (wrapper) {
    testthat::skip_if_not_installed("jmvcore")
    files <- c(files, "utils-followup.R", "curemodels.h.R", "curemodels.b.R")
  }
  for (f in files) sys.source(file.path(root, "R", f), envir = e)
  e
}

.cure_review_data <- function() {
  e <- .cure_review_env()
  e$.cureSeed(44, {
    n <- 120L
    x <- stats::rbinom(n, 1, .5)
    t <- ifelse(stats::runif(n) < stats::plogis(-.4 + .6 * x), Inf,
                stats::rexp(n, rate = .7))
    censor <- stats::runif(n, 3, 6)
    data.frame(time = pmin(t, censor), status = as.integer(t <= censor), x = x,
               background = rep(.01, n))
  })
}

.cure_review_spec <- function(design, engine = "nonmixture") {
  list(engine = engine, incidence = design$incidence, latency = design$latency,
       latency_explicit = FALSE, link = "logit", distribution = "exponential",
       latency_type = "ph", bandwidth = "large")
}

test_that("explicit status labels survive factor ordering and reject ambiguity", {
  e <- .cure_review_env()
  x <- factor(c("recurrence", "alive", "death"), levels = c("death", "alive", "recurrence"))
  expect_identical(e$.cureStatus(x, "recurrence", "alive", "death"), c(1L, 0L, 2L))
  expect_error(e$.cureStatus(x, "recurrence", "recurrence", "death"), "distinct")
  expect_error(e$.cureStatus(c(0, 1, 2)), "unmapped")
})

test_that("susceptibility logits give complemented profile probabilities", {
  e <- .cure_review_env()
  profiles <- data.frame(inc1 = c(0, 1, 1, 1))
  object <- list(spec = list(engine = "mixture", incidence = "inc1"),
                 fit = list(b = c(log(3), -log(9))))
  expected <- c(.25, .75, .75, .75)
  expect_equal(e$.curePredict(object, profiles), expected, tolerance = 1e-12)
  expect_equal(mean(e$.curePredict(object, profiles)), .625, tolerance = 1e-12)
  expect_gt(abs(mean(expected) - (1 - plogis(mean(c(log(3), rep(-log(3), 3)))))), .001)
})

test_that("categorical incidence uses separate contrasts and explicit profiles", {
  e <- .cure_review_env()
  d <- data.frame(time = 1:6, status = c(0, 1, 0, 1, 0, 1),
                  grade = factor(rep(c("low", "mid", "high"), 2),
                                 levels = c("low", "mid", "high")), age = 21:26)
  names(d)[4] <- "Age (years)"
  design <- e$.cureDesign(d, "time", "status", c("grade", "Age (years)"))
  expect_length(design$incidence, 3)
  expect_equal(as.matrix(design$data[, design$incidence[1:2]]),
               unname(model.matrix(~grade, d)[, -1]), ignore_attr = TRUE)
  expect_identical(nrow(design$profiles), nrow(d))
  ref <- e$.cureDesign(d, "time", "status", c("grade", "Age (years)"),
                       profile = "reference")
  expect_equal(as.numeric(ref$profiles[1, c(ref$incidence, ref$latency)]), rep(0, 6))
  expect_match(ref$profile, "reference levels", fixed = TRUE)
})

test_that("administrative censoring retains subjects and boundary events", {
  e <- .cure_review_env()
  d <- data.frame(.time = c(1, 2, 3, 4), .status = c(1L, 1L, 0L, 1L), id = 1:4)
  out <- e$.cureTruncate(d, 2)
  expect_identical(out$id, d$id)
  expect_equal(out$.time, c(1, 2, 2, 2))
  expect_identical(out$.status, c(1L, 1L, 0L, 0L))
  expect_identical(d$.status, c(1L, 1L, 0L, 1L))
  expect_error(e$.cureTruncate(d, Inf), "finite and positive")
  expect_error(e$.cureTruncate(d, 0), "finite and positive")
})

test_that("observed competing-risk probabilities match hand calculations", {
  skip_if_not_installed("survival")
  e <- .cure_review_env()
  result <- e$.observedCifReport(1:4, c(1, 2, 0, 1), c(4, 1, 2, 3, 5))
  d <- result$table
  expect_equal(d$estimate[d$time == 1], c(.75, .25, 0))
  expect_equal(d$estimate[d$time == 3], c(.5, .25, .25))
  expect_equal(d$estimate[d$time == 4], c(0, .75, .25))
  expect_equal(as.numeric(tapply(d$estimate, d$time, sum)), rep(1, 4))
  expect_equal(d$n_risk[d$time == 3], rep(2, 3))
  expect_equal(d$n_censored[d$time == 3], rep(1, 3))
  expect_equal(d$n_events[d$time == 4], c(3, 2, 1))
  expect_identical(result$omitted, "Overall")
  expect_true(all(is.na(d$lower[d$estimate %in% c(0, 1)])))
})

test_that("observed-risk support is group specific and retains absent causes", {
  skip_if_not_installed("survival")
  e <- .cure_review_env()
  result <- e$.observedCifReport(c(1, 2, 1, 4), c(2, 2, 0, 0), c(2, 3),
                                group = c("A", "A", "B", "B"))
  d <- result$table
  expect_identical(result$omitted, "A")
  expect_equal(d$estimate[d$group == "A"], c(0, 0, 1))
  expect_equal(d$estimate[d$group == "B"], rep(c(1, 0, 0), 2))
  expect_false(any(d$group == "A" & d$time == 3))
})

test_that("single-event fitting rejects competing events and unsupported links", {
  e <- .cure_review_env()
  d <- data.frame(.time = 1:3, .status = c(0L, 1L, 2L), inc1 = 0:2, lat1 = 0:2)
  spec <- list(engine = "mixture", incidence = "inc1", latency = "lat1", link = "logit")
  expect_error(e$.cureFit(d, spec), "competing")
  d$.status <- c(0L, 1L, 0L)
  spec$link <- "probit"
  expect_error(e$.cureFit(d, spec), "logit")
  d$.status <- 0L
  expect_error(e$.cureFit(d, spec), "No events")
})

test_that("real smcure predictions have the verified susceptibility convention", {
  skip_if_not_installed("smcure")
  e <- .cure_review_env()
  d <- .cure_review_data()
  design <- e$.cureDesign(d, "time", "status", "x")
  fit <- e$.cureFit(design$data, .cure_review_spec(design, "mixture"))
  direct <- smcure::predictsmcure(fit$fit,
    newX = as.matrix(design$profiles[, design$latency, drop = FALSE]),
    newZ = as.matrix(design$profiles[, design$incidence, drop = FALSE]), model = "ph")
  expect_equal(unname(e$.curePredict(fit, design$profiles)),
               as.numeric(1 - direct$newuncureprob), tolerance = 1e-10)
})

test_that("real parametric refits use original profiles at each horizon", {
  skip_if_not_installed("flexsurvcure")
  e <- .cure_review_env()
  d <- .cure_review_data()
  design <- e$.cureDesign(d, "time", "status", "x")
  spec <- .cure_review_spec(design)
  full <- e$.cureEstimate(design$data, spec, design$profiles)
  truncated <- e$.cureTruncate(design$data, 2)
  refit <- e$.cureEstimate(truncated, spec, design$profiles)
  beta <- stats::coef(refit$object$fit)
  expected <- mean(plogis(beta["theta"] + design$profiles$inc1 * beta["inc1"]))
  expect_equal(refit$estimate, expected, tolerance = 1e-10)
  expect_gt(abs(full$estimate - refit$estimate), .01)
  expect_equal(nrow(truncated), nrow(design$data))
})

test_that("true bootstrap is deterministic and restores the caller RNG state", {
  skip_if_not_installed("flexsurvcure")
  e <- .cure_review_env()
  d <- .cure_review_data()
  design <- e$.cureDesign(d, "time", "status", "x")
  spec <- .cure_review_spec(design)
  set.seed(927)
  before <- .Random.seed
  a <- e$.cureEstimate(design$data, spec, design$profiles, bootstrap = 20L, seed = 812)
  expect_identical(.Random.seed, before)
  b <- e$.cureEstimate(design$data, spec, design$profiles, bootstrap = 20L, seed = 812)
  expect_equal(c(a$estimate, a$lower, a$upper), c(b$estimate, b$lower, b$upper))
  expect_identical(a$attempted, 20L)
  expect_identical(a$successful, 20L)
  expect_true(is.finite(a$lower) && is.finite(a$upper) && a$lower < a$upper)
  expect_match(a$ci_method, "fixed original prediction profiles", fixed = TRUE)
})

test_that("failed bootstrap fits are counted without unbounded retries", {
  skip_if_not_installed("flexsurvcure")
  e <- .cure_review_env()
  d <- .cure_review_data()
  design <- e$.cureDesign(d, "time", "status", "x")
  real_fit <- e$.cureFit
  calls <- 0L
  e$.cureFit <- function(d, spec) {
    calls <<- calls + 1L
    if (calls %in% c(3L, 5L)) stop("Deliberate numerical fit failure")
    real_fit(d, spec)
  }
  result <- e$.cureEstimate(design$data, .cure_review_spec(design), design$profiles,
                            bootstrap = 5L, seed = 812)
  expect_identical(calls, 6L)
  expect_identical(result$attempted, 5L)
  expect_identical(result$successful, 3L)
  expect_true(is.na(result$lower) && is.na(result$upper))
})

test_that("cuRe and npcure preserve background and empirical profile inputs", {
  skip_if_not_installed("cuRe")
  skip_if_not_installed("npcure")
  e <- .cure_review_env()
  d <- .cure_review_data()
  design <- e$.cureDesign(d, "time", "status", "x", background = "background",
                         np_covariate = "x")
  fit <- e$.cureFit(design$data, .cure_review_spec(design, "cure"))
  pred <- e$.curePredict(fit, design$profiles)
  direct <- stats::predict(fit$fit, newdata = design$profiles,
                           type = "curerate", var.type = "n")
  expect_equal(pred, vapply(direct, function(x) x$Estimate[1], numeric(1)))
  np <- e$.cureFit(design$data, .cure_review_spec(design, "npcure"))
  q <- e$.curePredict(np, design$profiles)
  expect_length(q, nrow(d))
  expect_true(all(is.finite(q) & q >= 0 & q <= 1))
  expect_equal(mean(q), weighted.mean(c(q[which(d$x == 0)[1]], q[which(d$x == 1)[1]]),
                                      c(sum(d$x == 0), sum(d$x == 1))))
})

test_that("generated cure wrapper refits horizons and reports empty late risk sets", {
  skip_if_not_installed("flexsurvcure")
  e <- .cure_review_env(wrapper = TRUE)
  d <- .cure_review_data()
  r <- e$curemodels(data = d, time = "time", status = "status", predictors = "x",
    model_type = "nonmixture", survival_dist = "exponential", analysis_horizons = "1,2,3",
    sensitivity_analysis = TRUE, cure_threshold = 7)
  expect_identical(r$errors$content, "")
  table <- r$sensitivityTable$asDF
  expect_equal(table$horizon, c(1, 2, 3))
  expect_equal(table$n, rep(nrow(d), 3))
  expect_equal(table$events, vapply(c(1, 2, 3), function(h) sum(d$time <= h & d$status == 1), 0L))
  expect_equal(table$recensored, vapply(c(1, 2, 3), function(h) sum(d$time > h), 0L))
  expect_equal(r$followupSupport$asDF$n_risk, 0)
  expect_gt(diff(range(table$estimate)), .01)
})

test_that("generated wrapper blocks literal all-cause and competing-event cure", {
  e <- .cure_review_env(wrapper = TRUE)
  d <- .cure_review_data()
  r <- e$curemodels(data = d, time = "time", status = "status",
                    endpoint_type = "all_cause_death")
  expect_match(r$errors$content, "not a cure estimand", fixed = TRUE)
  expect_equal(nrow(r$cureTable$asDF), 0)
  expect_gt(nrow(r$observedRisk$asDF), 0)
  d$status[which(d$status == 1)[1:10]] <- 2L
  r <- e$curemodels(data = d, time = "time", status = "status", competing_value = "2")
  expect_match(r$errors$content, "no cure fit was produced", fixed = TRUE)
  expect_equal(nrow(r$cureTable$asDF), 0)
  expect_gt(max(r$observedRisk$asDF$estimate[r$observedRisk$asDF$cause == "Competing event(s)"]), 0)
  r <- e$curemodels(data = .cure_review_data(), time = "time", status = "status",
                    endpoint_type = "excess_mortality")
  expect_match(r$errors$content, "requires the cuRe model", fixed = TRUE)
})

test_that("intercept-only cure profiles retain native probability and uncertainty", {
  skip_if_not_installed("flexsurvcure")
  e <- .cure_review_env(wrapper = TRUE)
  fixture <- new.env(parent = emptyenv())
  load(file.path(.cure_review_root(), "data", "curemodels_test.rda"), envir = fixture)
  d <- fixture$curemodels_test
  r <- e$curemodels(data = d, time = "FollowUpMonths", status = "Recurrence",
                    model_type = "nonmixture")
  ref <- flexsurvcure::flexsurvcure(survival::Surv(FollowUpMonths, Recurrence) ~ 1,
                                   data = d, dist = "weibullPH", mixture = FALSE)
  tab <- r$cureTable$asDF
  theta <- unname(ref$res["theta", "est"])
  expect_identical(r$errors$content, "")
  expect_equal(tab$cure_fraction, theta, tolerance = 1e-3)
  expect_true(tab$cure_fraction > 0 && tab$cure_fraction < 1)
  expect_equal(c(tab$cure_ci_lower, tab$cure_ci_upper),
                unname(ref$res["theta", c("L95%", "U95%")]), tolerance = 1e-3)
  object <- list(spec = list(engine = "nonmixture", incidence = character()), fit = ref)
  expect_equal(e$.curePredict(object, d), rep(theta, nrow(d)), tolerance = 1e-8)
})
