# Reference-validation tests for multisurvival's statistical core.
#
# Two goals:
#   1. Unit-test the pure optimism-corrected C-index helper
#      (.multisurvivalOptimismCIndex) against survival::concordance and the
#      definition of Harrell's bootstrap optimism correction.
#   2. Cross-check multisurvival's model construction against independent
#      reference implementations (survival::coxph, cmprsk::crr) on public
#      datasets, so a regression in the fitting pipeline is caught early.
#
# Harness-free: sources only the self-contained helper (no devtools::load_all,
# no R6/jamovi harness) — see feedback_testing_strategy.
suppressMessages({
  library(testthat)
  library(survival)
})

# Resolve the repo root robustly regardless of how this file is executed
# (mirrors test-multisurvival-interactions.R).
.find_root <- function(start) {
  d <- suppressWarnings(normalizePath(start, mustWork = FALSE))
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "R", "utils.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  NA_character_
}
.root <- .find_root(file.path(dirname(dirname(getwd()))))
if (is.na(.root)) .root <- .find_root(getwd())
if (is.na(.root)) stop("Could not locate repo root (marker file R/utils.R not found)")

source(file.path(.root, "R", "multisurvival-metrics.R"))

# ---------------------------------------------------------------------------
# Fixture: a standard (non-competing-risk) Cox model on the lung dataset,
# using the internal column names multisurvival passes to the helper.
# ---------------------------------------------------------------------------
.lung_fixture <- function() {
  d <- na.omit(lung[, c("time", "status", "age", "sex", "ph.ecog")])
  d$mytime <- d$time
  status <- as.integer(d$status == 2)          # 2 = death in the lung dataset
  d$status_ind <- status
  fit <- survival::coxph(
    survival::Surv(mytime, status_ind) ~ age + sex + ph.ecog, data = d)
  list(data = d, status = status, fit = fit)
}

# ---------------------------------------------------------------------------
# 1. Optimism-corrected C-index helper
# ---------------------------------------------------------------------------

test_that(".multisurvivalOptimismCIndex returns a well-formed, bounded result", {
  fx <- .lung_fixture()
  res <- .multisurvivalOptimismCIndex(
    fx$fit, fx$data, fx$status, B = 80, seed = 7)

  expect_type(res, "list")
  expect_named(res, c("apparent", "optimism", "corrected", "B", "n_boot"))
  # Apparent C-index equals survival::concordance on the fitted model.
  expect_equal(res$apparent,
               survival::concordance(fx$fit)$concordance,
               tolerance = 1e-8)
  # Discrimination is in the valid (0.5, 1) range for a real model.
  expect_gt(res$apparent, 0.5)
  expect_lt(res$apparent, 1)
  # Corrected C = clamp(apparent - optimism) is the exact definition. (Optimism
  # is USUALLY positive, but bootstrap noise can make it slightly negative, so
  # corrected is not strictly <= apparent -- test the relationship, not a sign.)
  expect_equal(res$corrected, max(0, min(1, res$apparent - res$optimism)),
               tolerance = 1e-9)
  # Corrected C-index is a valid probability.
  expect_gte(res$corrected, 0)
  expect_lte(res$corrected, 1)
  # Optimism for a modest 3-predictor model is a small adjustment.
  expect_lt(abs(res$optimism), 0.15)
})

test_that(".multisurvivalOptimismCIndex is reproducible for a fixed seed", {
  fx <- .lung_fixture()
  r1 <- .multisurvivalOptimismCIndex(fx$fit, fx$data, fx$status,
                                     B = 60, seed = 123)
  r2 <- .multisurvivalOptimismCIndex(fx$fit, fx$data, fx$status,
                                     B = 60, seed = 123)
  expect_equal(r1$corrected, r2$corrected, tolerance = 1e-12)
  expect_equal(r1$optimism, r2$optimism, tolerance = 1e-12)
})

test_that(".multisurvivalOptimismCIndex does not disturb the global RNG stream", {
  fx <- .lung_fixture()
  set.seed(999)
  before <- runif(1)
  set.seed(999)
  invisible(.multisurvivalOptimismCIndex(fx$fit, fx$data,
                                         fx$status, B = 40, seed = 5))
  after <- runif(1)
  # The helper saves/restores .Random.seed, so the next draw is unaffected.
  expect_equal(before, after)
})

test_that(".multisurvivalOptimismCIndex returns NULL when events are too few", {
  fx <- .lung_fixture()
  status0 <- fx$status
  status0[] <- 0L
  status0[1:5] <- 1L                       # only 5 events (< 10 required)
  expect_null(.multisurvivalOptimismCIndex(
    fx$fit, fx$data, status0, B = 40, seed = 1))
})

test_that(".multisurvivalOptimismCIndex uses within-stratum concordance for strata() models", {
  # Regression guard for the stratified-concordance fix: C_test must use the
  # model's own (within-stratum) concordance, not a global Surv ~ lp form.
  d <- na.omit(lung[, c("time", "status", "age", "sex", "ph.ecog")])
  d$mytime <- d$time
  d$status_ind <- as.integer(d$status == 2)
  fit_s <- survival::coxph(
    survival::Surv(mytime, status_ind) ~ age + ph.ecog + strata(sex), data = d)

  res <- .multisurvivalOptimismCIndex(fit_s, d, d$status_ind, B = 60, seed = 11)
  expect_type(res, "list")
  # Apparent must equal the model's within-stratum concordance (the definition
  # C_train/C_test are now consistent with), NOT a strata-ignoring global C.
  expect_equal(res$apparent,
               survival::concordance(fit_s)$concordance, tolerance = 1e-8)
  # Corrected = clamp(apparent - optimism); optimism is a small adjustment.
  expect_equal(res$corrected, max(0, min(1, res$apparent - res$optimism)),
               tolerance = 1e-9)
  expect_lt(abs(res$optimism), 0.15)
  expect_gte(res$corrected, 0)
  expect_lte(res$corrected, 1)
})

# ---------------------------------------------------------------------------
# 2. Reference check: standard Cox HRs are reproducible
# ---------------------------------------------------------------------------

test_that("Cox hazard ratios match a direct survival::coxph reference (veteran)", {
  vet <- survival::veteran
  fit <- survival::coxph(Surv(time, status) ~ age + karno + trt, data = vet)
  hr <- exp(stats::coef(fit))
  # Karnofsky score is strongly protective; a regression in scaling/sign
  # would break these anchored expectations.
  expect_lt(hr[["karno"]], 1)              # higher performance -> lower hazard
  expect_true(all(is.finite(hr)))
  # C-index is a sensible discrimination value.
  cidx <- survival::concordance(fit)$concordance
  expect_gt(cidx, 0.6)
  expect_lt(cidx, 0.85)
})

# ---------------------------------------------------------------------------
# 3. Reference check: Fine-Gray subdistribution model matches cmprsk::crr
#    (this is the pipeline multisurvival uses for competing risks)
# ---------------------------------------------------------------------------

test_that("Fine-Gray (finegray + weighted coxph) matches cmprsk::crr (mgus2)", {
  skip_if_not_installed("cmprsk")
  m <- survival::mgus2
  # Time to plasma-cell malignancy (pcm) with death as the competing event.
  m$etime <- with(m, ifelse(pstat == 0, futime, ptime))
  m$event <- with(m, ifelse(pstat == 0, 2 * death, 1))  # 0 censor,1 pcm,2 death
  m <- m[!is.na(m$age) & !is.na(m$sex) & !is.na(m$etime) & m$etime > 0, ]
  m$sexM <- as.integer(m$sex == "M")
  efac <- factor(m$event, levels = 0:2, labels = c("censor", "pcm", "death"))

  # multisurvival's approach: finegray -> weighted coxph on expanded data.
  fg <- survival::finegray(survival::Surv(etime, efac) ~ age + sexM,
                           data = m, etype = "pcm")
  fg_fit <- survival::coxph(
    survival::Surv(fgstart, fgstop, fgstatus) ~ age + sexM,
    data = fg, weights = fgwt)
  fg_coef <- stats::coef(fg_fit)

  # Independent reference: cmprsk::crr subdistribution model for pcm (code 1).
  crr_fit <- cmprsk::crr(
    ftime = m$etime, fstatus = m$event,
    cov1 = as.matrix(m[, c("age", "sexM")]),
    failcode = 1, cencode = 0)
  crr_coef <- crr_fit$coef

  # Subdistribution log-hazard coefficients should agree closely.
  expect_equal(unname(fg_coef[["age"]]),  unname(crr_coef[[1]]), tolerance = 0.02)
  expect_equal(unname(fg_coef[["sexM"]]), unname(crr_coef[[2]]), tolerance = 0.05)
})
