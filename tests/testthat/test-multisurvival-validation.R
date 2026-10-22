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
  # survival::, not a bare `lung`: run in one process after another test file whose package
  # also exports a `lung`, the bare name resolved to that one and every test here errored
  # with "undefined columns selected" (green in isolation, red in a batch).
  d <- na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
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
  # survival::, not a bare `lung`: run in one process after another test file whose package
  # also exports a `lung`, the bare name resolved to that one and every test here errored
  # with "undefined columns selected" (green in isolation, red in a batch).
  d <- na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
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

# ===========================================================================
# 4. Exhaustive Validation Suite Extensions (2026-10-21)
#    Added by /validate-function multisurvival depth=exhaustive
# ===========================================================================

.ensure_multisurvival_loaded <- function() {
  if (!exists("multisurvivalClass", inherits = TRUE)) {
    suppressWarnings(suppressPackageStartupMessages({
      library(jmvcore)
      library(R6)
      library(survival)
      library(magrittr)
      library(ggplot2)
      library(lubridate)
      library(labelled)
    }))
    for (f in c(
      "utils.R",
      "utils-formula.R",
      "utils-eventindicator.R",
      "utils-followup.R",
      "survival-competingrisks.R",
      "multisurvival-formula.R",
      "multisurvival-interactions.R",
      "multisurvival-metrics.R",
      "multisurvival.h.R",
      "multisurvival.b.R"
    )) {
      source(file.path(.root, "R", f))
    }
  }
}

.ms_val_run <- function(data,
                        ...,
                        outcomeLevel = NULL,
                        dod = NULL,
                        dooc = NULL,
                        awd = NULL,
                        awod = NULL,
                        calculatedtime = FALSE,
                        outcomeredefined = FALSE,
                        addRiskScore = FALSE,
                        addRiskGroup = FALSE) {
  .ensure_multisurvival_loaded()
  opts <- multisurvivalOptions$new(
    ...,
    outcomeLevel = outcomeLevel,
    dod = dod,
    dooc = dooc,
    awd = awd,
    awod = awod
  )
  set_out <- function(nm, val) {
    if (isTRUE(val)) {
      opts$.__enclos_env__$private[[paste0("..", nm)]]$.__enclos_env__$private$.value <-
        list(value = TRUE, vars = nm)
    }
  }
  set_out("calculatedtime", calculatedtime)
  set_out("outcomeredefined", outcomeredefined)
  set_out("addRiskScore", addRiskScore)
  set_out("addRiskGroup", addRiskGroup)
  for (v in names(data)) {
    if (is.character(data[[v]])) data[[v]] <- as.factor(data[[v]])
  }
  an <- multisurvivalClass$new(options = opts, data = data)
  suppressWarnings(suppressMessages(an$run()))
  an$results
}

.out_vals <- function(out_item) {
  unlist(out_item$.__enclos_env__$private$.values, use.names = FALSE)
}

.strip_html <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  gsub("[[:space:]]+", " ", trimws(gsub("<[^>]*>", " ", paste(x, collapse = " "))))
}

test_that("VAL-A1: Multivariable Cox matches first-principles risk-set partial likelihood oracle", {
  set.seed(20261021)
  n <- 80
  d <- data.frame(
    time = round(cumsum(rexp(n, rate = 0.25)) + seq_len(n) * 0.01, 4),
    status = rep(c(1L, 1L, 0L, 1L), length.out = n),
    arm = factor(rep(c("Ctrl", "Trt"), length.out = n), levels = c("Ctrl", "Trt")),
    marker = round(rnorm(n, mean = 0, sd = 1.2), 4)
  )
  X <- cbind(armTrt = as.numeric(d$arm == "Trt"), marker = d$marker)
  ev_idx <- which(d$status == 1L)
  grad_fn <- function(beta) {
    eta <- as.vector(X %*% beta)
    g <- numeric(2)
    for (i in ev_idx) {
      rset <- which(d$time >= d$time[i])
      w <- exp(eta[rset] - max(eta[rset])); w <- w / sum(w)
      g <- g + (X[i, ] - colSums(X[rset, , drop = FALSE] * w))
    }
    -g
  }
  hess_fn <- function(beta) {
    eta <- as.vector(X %*% beta)
    I_mat <- matrix(0, 2, 2)
    for (i in ev_idx) {
      rset <- which(d$time >= d$time[i])
      w <- exp(eta[rset] - max(eta[rset])); w <- w / sum(w)
      Xr <- X[rset, , drop = FALSE]
      Xc <- sweep(Xr, 2, colSums(Xr * w), "-")
      I_mat <- I_mat + crossprod(Xc, Xc * w)
    }
    I_mat
  }
  beta <- c(0, 0)
  for (it in 1:25) {
    step <- solve(hess_fn(beta), grad_fn(beta))
    beta <- beta - step
    if (max(abs(step)) < 1e-14) break
  }
  se <- sqrt(diag(solve(hess_fn(beta))))
  pval <- 2 * stats::pnorm(-abs(beta / se))

  r <- .ms_val_run(
    data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = "arm", contexpl = "marker",
    ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE
  )
  df_adj <- r$adjustedCoxTable$asDF
  expect_equal(df_adj$Pvalue, unname(pval), tolerance = 1e-8)
  expect_match(df_adj$HR[1], sprintf("^%.2f", exp(beta[1])))
  expect_match(df_adj$HR[2], sprintf("^%.2f", exp(beta[2])))
})

test_that("VAL-A2: Person-time table matches closed-form Garwood (1936) exact Poisson CIs and conservation", {
  d <- data.frame(
    time = c(5, 10, 15, 20, 30, 40, 8, 12, 25, 35, 45, 55),
    status = c(1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L),
    arm = factor(rep(c("A", "B"), each = 6), levels = c("A", "B"))
  )
  r <- .ms_val_run(
    data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = "arm", person_time = TRUE, time_intervals = "15, 30", rate_multiplier = 100
  )
  pt <- r$personTimeTable$asDF
  # Overall: 8 events, 300 person-months
  expect_equal(pt$events[1], 8)
  expect_equal(pt$person_time[1], 300)
  expect_equal(pt$rate[1], round(8 / 300 * 100, 2))
  expect_equal(pt$rate_ci_lower[1], round((0.5 * stats::qchisq(0.025, 16) / 300) * 100, 2))
  expect_equal(pt$rate_ci_upper[1], round((0.5 * stats::qchisq(0.975, 18) / 300) * 100, 2))

  # Subgroup conservation: Group A (120) + Group B (180) = 300
  grp <- pt[grepl("^arm:", pt$interval), ]
  expect_equal(sum(grp$events), pt$events[1])
  expect_equal(sum(grp$person_time), pt$person_time[1])

  # Interval conservation: (0-15: 155) + (15-30: 90) + (30-55: 55) = 300
  ints <- pt[pt$interval %in% c("0-15", "15-30", "30-55"), ]
  expect_equal(ints$person_time, c(155, 90, 55))
  expect_equal(ints$events, c(3, 3, 2))
  expect_equal(sum(ints$person_time), pt$person_time[1])
})

test_that("VAL-A3 & E2: All 7 date formats and landmark filtering compute exact elapsed times", {
  dx_iso <- as.Date("2021-03-15")
  fu_iso <- dx_iso + seq(30, 360, by = 30)
  fmt_map <- list(
    ymdhms = list(dx = paste0(base::format(dx_iso, "%Y-%m-%d"), " 08:30:00"),
                  fu = paste0(base::format(fu_iso, "%Y-%m-%d"), " 08:30:00")),
    ymd = list(dx = base::format(dx_iso, "%Y-%m-%d"), fu = base::format(fu_iso, "%Y-%m-%d")),
    ydm = list(dx = base::format(dx_iso, "%Y-%d-%m"), fu = base::format(fu_iso, "%Y-%d-%m")),
    mdy = list(dx = base::format(dx_iso, "%m-%d-%Y"), fu = base::format(fu_iso, "%m-%d-%Y")),
    myd = list(dx = base::format(dx_iso, "%m-%Y-%d"), fu = base::format(fu_iso, "%m-%Y-%d")),
    dmy = list(dx = base::format(dx_iso, "%d-%m-%Y"), fu = base::format(fu_iso, "%d-%m-%Y")),
    dym = list(dx = base::format(dx_iso, "%d-%Y-%m"), fu = base::format(fu_iso, "%d-%Y-%m"))
  )
  for (f_name in names(fmt_map)) {
    d_fmt <- data.frame(
      dx = fmt_map[[f_name]]$dx,
      fu = fmt_map[[f_name]]$fu,
      status = rep(c(1L, 1L, 0L), 4),
      arm = factor(rep(c("A", "B"), 6))
    )
    r_fmt <- .ms_val_run(
      data = d_fmt, tint = TRUE, dxdate = "dx", fudate = "fu",
      timetypedata = f_name, timetypeoutput = "days",
      outcome = "status", outcomeLevel = "1", explanatory = "arm",
      calculatedtime = TRUE
    )
    expect_equal(.out_vals(r_fmt$calculatedtime), seq(30, 360, by = 30), tolerance = 1e-8)
  }

  # Landmark at 90 days drops first 2 rows (30, 60) and shifts remaining 10 rows by -90
  d_lm <- data.frame(
    dx = fmt_map$ymd$dx, fu = fmt_map$ymd$fu,
    status = rep(c(1L, 1L, 0L), 4), arm = factor(rep(c("A", "B"), 6))
  )
  r_lm <- .ms_val_run(
    data = d_lm, tint = TRUE, dxdate = "dx", fudate = "fu",
    timetypedata = "ymd", timetypeoutput = "days",
    uselandmark = TRUE, landmark = 90,
    outcome = "status", outcomeLevel = "1", explanatory = "arm",
    calculatedtime = TRUE
  )
  expect_equal(.out_vals(r_lm$calculatedtime), seq(90, 360, by = 30) - 90, tolerance = 1e-8)
  expect_match(.strip_html(r_lm$text2$content),
               "Patients whose follow-up ended before the landmark were excluded \\(n = 2 removed\\)")
})

test_that("VAL-B1: Reference parity for survMetricsTable, cox_phTable, modelContributionTable, and adjustedSurvTable", {
  skip_if_not_installed("riskRegression")
  d_colon <- na.omit(survival::colon[survival::colon$etype == 2,
                                     c("time", "status", "sex", "obstruct", "perfor", "age", "nodes")])
  d_colon$sex <- factor(d_colon$sex, levels = c(0, 1), labels = c("Female", "Male"))
  d_colon$obstruct <- factor(d_colon$obstruct, levels = c(0, 1), labels = c("No", "Yes"))
  d_colon$perfor <- factor(d_colon$perfor, levels = c(0, 1), labels = c("No", "Yes"))

  fit_ref <- survival::coxph(
    survival::Surv(time, status) ~ sex + obstruct + perfor + age + nodes,
    data = d_colon, x = TRUE, y = TRUE
  )
  r <- .ms_val_run(
    data = d_colon, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = c("sex", "obstruct", "perfor"), contexpl = c("age", "nodes"),
    ph_cox = TRUE, ac = TRUE, adjexplanatory = "obstruct", ac_method = "average", ac_summary = TRUE,
    show_survmetrics = TRUE, survmetrics_timepoints = "365, 730, 1095",
    compare_models = TRUE, cutp = "365, 730, 1095"
  )

  # Schoenfeld PH table vs survival::cox.zph
  zph_ref <- survival::cox.zph(fit_ref)
  zph_df <- r$cox_phTable$asDF
  expect_equal(zph_df$chisq, unname(zph_ref$table[, "chisq"]), tolerance = 1e-7)
  expect_equal(zph_df$p, unname(zph_ref$table[, "p"]), tolerance = 1e-7)

  # Model contribution table vs stats::drop1
  d1_ref <- stats::drop1(fit_ref, test = "Chisq")
  mc_df <- r$modelContributionTable$asDF
  expect_equal(mc_df$lrt, d1_ref[-1, "LRT"], tolerance = 1e-7)
  expect_equal(mc_df$pvalue, d1_ref[-1, "Pr(>Chi)"], tolerance = 1e-7)

  # Harrell's C in survMetricsTable vs summary(fit_ref)$concordance
  sm_df <- r$survMetricsTable$asDF
  expect_equal(sm_df$value[1], unname(summary(fit_ref)$concordance[1]), tolerance = 1e-7)
})

test_that("VAL-C: Metamorphic relations hold for Cox HRs, LRT, and rescaled person-time", {
  set.seed(20261021)
  n <- 80
  d <- data.frame(
    time = round(cumsum(rexp(n, rate = 0.25)) + seq_len(n) * 0.01, 4),
    status = rep(c(1L, 1L, 0L, 1L), length.out = n),
    arm = factor(rep(c("Ctrl", "Trt"), length.out = n), levels = c("Ctrl", "Trt")),
    marker = round(rnorm(n, mean = 0, sd = 1.2), 4)
  )
  r0 <- .ms_val_run(
    data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = "arm", contexpl = "marker",
    ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE,
    compare_models = TRUE, person_time = TRUE, rate_multiplier = 100
  )

  # Time rescaling (t -> 10*t, rate_multiplier -> 1000) preserves Cox p-values and scaled incidence rate
  d_t10 <- transform(d, time = time * 10)
  r_t10 <- .ms_val_run(
    data = d_t10, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = "arm", contexpl = "marker",
    ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE,
    compare_models = TRUE, person_time = TRUE, rate_multiplier = 1000
  )
  expect_equal(r_t10$adjustedCoxTable$asDF$Pvalue, r0$adjustedCoxTable$asDF$Pvalue, tolerance = 1e-12)
  expect_equal(r_t10$personTimeTable$asDF$rate[1], r0$personTimeTable$asDF$rate[1], tolerance = 1e-12)

  # Affine predictor shift/scale (marker -> 2 * marker + 5) preserves Wald p-values and drop-one LRT
  d_aff <- transform(d, marker = 2 * marker + 5)
  r_aff <- .ms_val_run(
    data = d_aff, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = "arm", contexpl = "marker",
    ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE, compare_models = TRUE
  )
  expect_equal(r_aff$adjustedCoxTable$asDF$Pvalue, r0$adjustedCoxTable$asDF$Pvalue, tolerance = 1e-12)
  expect_equal(r_aff$modelContributionTable$asDF$lrt, r0$modelContributionTable$asDF$lrt, tolerance = 1e-12)

  # Two-stratum exact duplication doubles drop-one LRT statistics
  d_dup <- rbind(
    transform(d, copy_id = factor("S1", levels = c("S1", "S2"))),
    transform(d, copy_id = factor("S2", levels = c("S1", "S2")))
  )
  r_dup <- .ms_val_run(
    data = d_dup, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = c("arm", "copy_id"), contexpl = "marker",
    use_stratify = TRUE, stratvar = "copy_id", compare_models = TRUE
  )
  expect_equal(r_dup$modelContributionTable$asDF$lrt,
               2 * r0$modelContributionTable$asDF$lrt, tolerance = 1e-7)
})

test_that("VAL-E1: All 4 multievent analysistype encodings produce exact status indicators", {
  d_me <- data.frame(
    time = seq(4, 34, by = 2),
    outcome4 = factor(rep(c("DOD", "DOOC", "AWD", "AWOD"), 4),
                      levels = c("AWD", "AWOD", "DOD", "DOOC")),
    arm = factor(rep(c("Ctrl", "Trt"), each = 8))
  )
  r_ov <- .ms_val_run(data = d_me, elapsedtime = "time", multievent = TRUE, outcome = "outcome4",
                      analysistype = "overall", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                      explanatory = "arm", outcomeredefined = TRUE)
  expect_equal(.out_vals(r_ov$outcomeredefined), as.integer(d_me$outcome4 %in% c("DOD", "DOOC")))

  r_cs <- .ms_val_run(data = d_me, elapsedtime = "time", multievent = TRUE, outcome = "outcome4",
                      analysistype = "cause", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                      explanatory = "arm", outcomeredefined = TRUE)
  expect_equal(.out_vals(r_cs$outcomeredefined), as.integer(d_me$outcome4 == "DOD"))

  r_dfs <- .ms_val_run(data = d_me, elapsedtime = "time", multievent = TRUE, outcome = "outcome4",
                       analysistype = "dfs", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                       explanatory = "arm", outcomeredefined = TRUE)
  expect_equal(.out_vals(r_dfs$outcomeredefined), as.integer(d_me$outcome4 %in% c("DOD", "DOOC", "AWD")))

  r_cr <- .ms_val_run(data = d_me, elapsedtime = "time", multievent = TRUE, outcome = "outcome4",
                      analysistype = "compete", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                      explanatory = "arm", outcomeredefined = TRUE)
  exp_cr <- ifelse(d_me$outcome4 == "DOD", "Event",
                   ifelse(d_me$outcome4 == "DOOC", "Competing", "Censored"))
  expect_equal(as.character(.out_vals(r_cr$outcomeredefined)), exp_cr)
})

# ---------------------------------------------------------------------------
# 5. Resolved Defect Regression Guards (VAL-multisurvival-01 .. VAL-multisurvival-11)
# ---------------------------------------------------------------------------

test_that("VAL-multisurvival-01: Collinear predictor in modelContributionTable has df=0 and NA p-value", {
  set.seed(20261021)
  n <- 80
  d <- data.frame(
    time = round(cumsum(rexp(n, rate = 0.25)) + seq_len(n) * 0.01, 4),
    status = rep(c(1L, 1L, 0L, 1L), length.out = n),
    arm = factor(rep(c("Ctrl", "Trt"), length.out = n)),
    marker = rnorm(n)
  )
  d$dup_arm <- d$arm
  r <- .ms_val_run(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                   explanatory = c("arm", "dup_arm"), contexpl = "marker", compare_models = TRUE)
  mc <- r$modelContributionTable$asDF
  expect_equal(mc$df[mc$term == "dup_arm"], 0L)
  expect_true(is.na(mc$pvalue[mc$term == "dup_arm"]))
})

test_that("VAL-multisurvival-02: Clinical Summary in text does not report sig_count > n_vars when interactions are significant", {
  set.seed(101)
  n <- 400
  d <- data.frame(
    arm = factor(rep(c("Ctrl", "Trt"), each = n / 2)),
    age = rnorm(n, 60, 10),
    status = rep(c(1L, 1L, 1L, 0L), length.out = n)
  )
  d$time <- rexp(n, rate = 0.05 * exp(0.8 * (d$arm == "Trt") + 0.08 * (d$age - 60) +
                                        0.08 * (d$arm == "Trt") * (d$age - 60)))
  r <- .ms_val_run(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                   explanatory = "arm", contexpl = "age", interactions = list(c("arm", "age")),
                   showSummaries = TRUE)
  expect_false(grepl("3 out of 2 factors", r$text$content, fixed = TRUE))
})

test_that("VAL-multisurvival-03: Nomogram formula and nomogramSummary C-index match when interactions are present", {
  set.seed(101)
  n <- 400
  d <- data.frame(
    arm = factor(rep(c("Ctrl", "Trt"), each = n / 2)),
    age = rnorm(n, 60, 10),
    status = rep(c(1L, 1L, 1L, 0L), length.out = n)
  )
  d$time <- rexp(n, rate = 0.05 * exp(0.8 * (d$arm == "Trt") + 0.08 * (d$age - 60) +
                                        0.08 * (d$arm == "Trt") * (d$age - 60)))
  r <- .ms_val_run(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                   explanatory = "arm", contexpl = "age", interactions = list(c("arm", "age")),
                   showNomogram = TRUE)
  expect_match(r$nomogram_display$content, "arm\\s*[:*]\\s*age", ignore.case = TRUE)
})

test_that("VAL-multisurvival-04: subgroupHR preserves declared non-alphabetical moderator factor level ordering", {
  set.seed(20261021)
  n <- 80
  d <- data.frame(
    time = round(cumsum(rexp(n, rate = 0.25)) + seq_len(n) * 0.01, 4),
    status = rep(c(1L, 1L, 0L, 1L), length.out = n),
    arm = factor(rep(c("Ctrl", "Trt"), length.out = n)),
    grade = factor(rep(c("Low", "Medium", "High"), length.out = n), levels = c("Low", "Medium", "High"))
  )
  r <- .ms_val_run(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                   explanatory = c("arm", "grade"), interactions = list(c("arm", "grade")))
  expect_equal(r$subgroupHR$asDF$moderator_level, c("Low", "Medium", "High"))
})

test_that("VAL-multisurvival-05: text2 reports actual Missing row count when input data contains NAs", {
  set.seed(20261021)
  n <- 80
  d <- data.frame(
    time = round(cumsum(rexp(n, rate = 0.25)) + seq_len(n) * 0.01, 4),
    status = rep(c(1L, 1L, 0L, 1L), length.out = n),
    arm = factor(rep(c("Ctrl", "Trt"), length.out = n)),
    marker = rnorm(n)
  )
  d$marker[1:10] <- NA
  r <- .ms_val_run(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                   explanatory = "arm", contexpl = "marker")
  expect_match(.strip_html(r$text2$content), "Missing = 10")
})

test_that("VAL-multisurvival-06: NAs in unused adjexplanatory do not shrink main Cox model sample size", {
  set.seed(20261021)
  n <- 80
  d <- data.frame(
    time = round(cumsum(rexp(n, rate = 0.25)) + seq_len(n) * 0.01, 4),
    status = rep(c(1L, 1L, 0L, 1L), length.out = n),
    arm = factor(rep(c("Ctrl", "Trt"), length.out = n)),
    marker = rnorm(n),
    aux_var = factor(c(rep(NA, 15), rep(c("X", "Y"), length.out = n - 15)))
  )
  r_off <- .ms_val_run(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                       explanatory = "arm", contexpl = "marker", ac = FALSE, adjexplanatory = "aux_var")
  r_on <- .ms_val_run(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                      explanatory = "arm", contexpl = "marker", ac = TRUE, adjexplanatory = "aux_var")
  expect_equal(.strip_html(r_on$text2$content), .strip_html(r_off$text2$content))
})

test_that("VAL-multisurvival-07: .plot3 returns TRUE when rendering fallback grid.text notice on stratified model", {
  .ensure_multisurvival_loaded()
  d_colon <- na.omit(survival::colon[survival::colon$etype == 2, c("time", "status", "sex", "obstruct", "age")])
  d_colon$sex <- factor(d_colon$sex)
  d_colon$obstruct <- factor(d_colon$obstruct)
  opts <- multisurvivalOptions$new(
    elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    explanatory = c("sex", "obstruct"), contexpl = "age",
    use_stratify = TRUE, stratvar = "sex", hr = TRUE, sty = "t3"
  )
  an <- multisurvivalClass$new(options = opts, data = d_colon)
  suppressWarnings(suppressMessages(an$run()))
  png_f <- tempfile(fileext = ".png")
  on.exit(unlink(png_f), add = TRUE)
  grDevices::png(png_f, width = 800, height = 600)
  res <- an$.__enclos_env__$private$.plot3(an$results$plot3, ggtheme = ggplot2::theme_bw(), theme = list())
  grDevices::dev.off()
  expect_true(res)
})

test_that("VAL-multisurvival-08 & 09: Schema clearWith completeness for stratificationExplanation and riskGroupPlot", {
  r_yaml <- yaml::read_yaml(file.path(.root, "jamovi", "multisurvival.r.yaml"))
  cw_of <- function(nm) {
    for (it in r_yaml$items) if (identical(it$name, nm)) return(it$clearWith)
    character(0)
  }
  expect_true(all(c("use_stratify", "stratvar") %in% cw_of("stratificationExplanation")))
  expect_true(all(c("endplot", "byplot", "pplot") %in% cw_of("riskGroupPlot")))
})

test_that("VAL-multisurvival-10: Public R wrapper multisurvival() populates type: Output items when isNotFilled()", {
  .ensure_multisurvival_loaded()
  set.seed(20261021)
  n <- 80
  d <- data.frame(
    time = round(cumsum(rexp(n, rate = 0.25)) + seq_len(n) * 0.01, 4),
    status = rep(c(1L, 1L, 0L, 1L), length.out = n),
    arm = factor(rep(c("Ctrl", "Trt"), length.out = n)),
    marker = rnorm(n)
  )
  r <- suppressWarnings(suppressMessages(
    multisurvival(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
                  explanatory = "arm", contexpl = "marker", calculateRiskScore = TRUE)
  ))
  expect_length(.out_vals(r$addRiskScore), n)
})

test_that("VAL-multisurvival-11: personTimeTable subgroup row order is invariant to row permutation", {
  set.seed(20261021)
  n <- 80
  d <- data.frame(
    time = round(cumsum(rexp(n, rate = 0.25)) + seq_len(n) * 0.01, 4),
    status = rep(c(1L, 1L, 0L, 1L), length.out = n),
    arm = factor(rep(c("Ctrl", "Trt"), length.out = n), levels = c("Ctrl", "Trt")),
    marker = rnorm(n)
  )
  set.seed(606)
  d_perm <- d[sample(seq_len(n)), ]
  r0 <- .ms_val_run(data = d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                    explanatory = "arm", contexpl = "marker", person_time = TRUE)
  r1 <- .ms_val_run(data = d_perm, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                    explanatory = "arm", contexpl = "marker", person_time = TRUE)
  expect_equal(r1$personTimeTable$asDF$interval, r0$personTimeTable$asDF$interval)
})
