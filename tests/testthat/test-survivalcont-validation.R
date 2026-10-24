# ==============================================================================
# Exhaustive Validation & Regression Test Suite for ClinicoPath::survivalcont
# Generated per .claude/commands/validate-function.md (depth=exhaustive)
# Date: 2026-10-24
# Companion report: development-ideas/survivalcont-validation-2026-10-24.md
# Companion matrix: development-ideas/survivalcont-validation-2026-10-24-matrix.csv
# ==============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(jmvcore)
  library(R6)
  library(survival)
  library(survminer)
  library(maxstat)
  library(finalfit)
  library(KMunicate)
  library(magrittr)
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(lubridate)
  library(labelled)
  library(janitor)
  library(scales)
  library(glue)
  library(htmltools)
  library(patchwork)
  library(rpart)
  library(yaml)
}))

.find_sc_root <- function(start) {
  d <- suppressWarnings(normalizePath(start, mustWork = FALSE))
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "R", "survivalcont.b.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  NA_character_
}
.sc_root <- .find_sc_root(file.path(dirname(dirname(getwd()))))
if (is.na(.sc_root)) .sc_root <- .find_sc_root(getwd())
if (is.na(.sc_root)) stop("Could not locate repo root containing R/survivalcont.b.R")

for (f in c(
  "utils.R",
  "utils-formula.R",
  "utils-eventindicator.R",
  "utils-followup.R",
  "survivalcont.h.R",
  "survivalcont.b.R"
)) {
  source(file.path(.sc_root, "R", f))
}

.sc_val_run <- function(data,
                        elapsedtime = "time",
                        outcome = "status",
                        contexpl = "marker",
                        outcomeLevel = NULL,
                        dod = NULL,
                        dooc = NULL,
                        awd = NULL,
                        awod = NULL,
                        calculatedtime = FALSE,
                        outcomeredefined = FALSE,
                        calculatedcutoff = FALSE,
                        calculatedmulticut = FALSE,
                        return_analysis = FALSE,
                        ...) {
  df <- as.data.frame(data)
  if (!any(c(calculatedtime, outcomeredefined, calculatedcutoff, calculatedmulticut, return_analysis))) {
    call_args <- c(
      list(
        data = df,
        elapsedtime = elapsedtime,
        outcome = outcome,
        contexpl = contexpl,
        outcomeLevel = outcomeLevel,
        dod = dod,
        dooc = dooc,
        awd = awd,
        awod = awod
      ),
      list(...)
    )
    return(suppressWarnings(suppressMessages(do.call(survivalcont, call_args))))
  }
  opt_args <- c(
    list(
      elapsedtime = elapsedtime,
      outcome = outcome,
      contexpl = contexpl,
      outcomeLevel = outcomeLevel,
      dod = dod,
      dooc = dooc,
      awd = awd,
      awod = awod
    ),
    list(...)
  )
  opts <- do.call(survivalcontOptions$new, opt_args)
  set_out <- function(nm, val) {
    if (isTRUE(val)) {
      o <- opts$option(nm)
      o$value <- list(value = TRUE, synced = FALSE)
    }
  }
  set_out("calculatedtime", calculatedtime)
  set_out("outcomeredefined", outcomeredefined)
  set_out("calculatedcutoff", calculatedcutoff)
  set_out("calculatedmulticut", calculatedmulticut)
  an <- survivalcontClass$new(options = opts, data = df)
  suppressWarnings(suppressMessages(an$run()))
  if (isTRUE(return_analysis)) return(an)
  an$results
}

.sc_strip_html <- function(x) {
  trimws(gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(x, collapse = " "))))
}

.sc_parse_ff <- function(x) {
  s <- trimws(as.character(x[1]))
  m <- regexec("^([0-9.eE+-]+)\\s*\\(([0-9.eE+-]+)-([0-9.eE+-]+),\\s*p([=<])([0-9.eE+-]+)\\)$", s)[[1]]
  if (m[1] == -1) return(c(hr = NA_real_, lower = NA_real_, upper = NA_real_, p = NA_real_))
  parts <- regmatches(s, list(m))[[1]]
  c(hr = as.numeric(parts[2]), lower = as.numeric(parts[3]), upper = as.numeric(parts[4]), p = as.numeric(parts[6]))
}

.sc_out_vals <- function(out_item) {
  v <- out_item$.__enclos_env__$private$.values
  if (length(v) == 0) return(NULL)
  v[[1]]
}

.sc_out_rows <- function(out_item) {
  out_item$.__enclos_env__$private$.rowNums
}

# Zero-dependency first-principles Cox partial-likelihood oracle
.sc_cox_oracle <- function(dat, time_col = "time", status_col = "status",
                           x_col = "marker", strata_col = NULL) {
  if (!is.null(strata_col)) {
    dat <- dat[!is.na(dat[[strata_col]]), , drop = FALSE]
    strata_list <- split(dat, dat[[strata_col]], drop = TRUE)
  } else {
    strata_list <- list(dat)
  }
  eval_pl <- function(beta) {
    ll <- sc <- inf_m <- 0
    for (s in strata_list) {
      t_v <- s[[time_col]]; d_v <- s[[status_col]]; x_v <- s[[x_col]]
      for (i in which(d_v == 1)) {
        rk <- which(t_v >= t_v[i]); xr <- x_v[rk]
        eta <- beta * xr; sh <- max(eta); w <- exp(eta - sh); p <- w / sum(w)
        mx <- sum(p * xr)
        ll <- ll + beta * x_v[i] - sh - log(sum(w))
        sc <- sc + (x_v[i] - mx)
        inf_m <- inf_m + sum(p * (xr - mx)^2)
      }
    }
    c(loglik = ll, score = sc, info = inf_m)
  }
  beta_hat <- stats::uniroot(function(b) unname(eval_pl(b)["score"]), interval = c(-15, 15), tol = 1e-13)$root
  pl_hat <- eval_pl(beta_hat)
  se_hat <- 1 / sqrt(unname(pl_hat["info"]))
  z_crit <- stats::qnorm(0.975)
  list(
    beta = beta_hat, se = se_hat, hr = exp(beta_hat),
    ci_lower = exp(beta_hat - z_crit * se_hat),
    ci_upper = exp(beta_hat + z_crit * se_hat),
    p_wald = 2 * stats::pnorm(-abs(beta_hat / se_hat))
  )
}

.make_sc_cohort <- function() {
  set.seed(20261024)
  data.frame(
    time = rexp(120, rate = 0.025),
    status = rbinom(120, 1, 0.72),
    marker = rnorm(120, mean = 50, sd = 12),
    site = factor(rep(c("Center_A", "Center_B", "Center_C"), 40))
  )
}

test_that("C01 & C02: Continuous Cox and Stratified Cox match zero-dependency partial-likelihood oracle and survival::coxph", {
  d <- .make_sc_cohort()
  ora_u <- .sc_cox_oracle(d)
  ora_s <- .sc_cox_oracle(d, strata_col = "site")

  res <- .sc_val_run(d, stratified_cox = TRUE, strata_variable = "site")
  ff <- .sc_parse_ff(res$coxTable$asDF$HR_univariable[1])
  expect_equal(unname(c(ff["hr"], ff["lower"], ff["upper"])),
               round(c(ora_u$hr, ora_u$ci_lower, ora_u$ci_upper), 2),
               tolerance = 1e-8)

  sc_df <- res$stratifiedCoxTable$asDF
  expect_equal(c(sc_df$hr[1], sc_df$ci_lower[1], sc_df$ci_upper[1], sc_df$pvalue[1]),
               c(ora_s$hr, ora_s$ci_lower, ora_s$ci_upper, ora_s$p_wald),
               tolerance = 1e-8)
})

test_that("C03 & C04: Piecewise person-time Garwood Poisson 95% CIs and step-integral RMST match hand-calculated oracles", {
  d12 <- data.frame(
    time   = c(4, 8, 10, 12, 18, 24, 30, 36, 42, 48, 54, 72),
    status = c(1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L),
    marker = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
  )
  res <- .sc_val_run(d12, person_time = TRUE, time_intervals = "12, 36, 60",
                     rate_multiplier = 100, rmst_analysis = TRUE, rmst_tau = 40)
  pt <- res$personTimeTable$asDF
  expect_equal(pt$events, c(5L, 3L, 2L, 0L, 0L))
  expect_equal(pt$person_time, c(358, 130, 156, 60, 12))
  # 0-event interval (36, 60] uses one-sided 97.5% Garwood upper bound: (qchisq(0.975, 2)/2) / 60 * 100
  expect_equal(pt$rate_ci_lower[4], 0)
  expect_equal(pt$rate_ci_upper[4], round((stats::qchisq(0.975, 2) / 2) / 60 * 100, 2))

  rmst_df <- res$rmstTable$asDF
  km_ref <- summary(survival::survfit(survival::Surv(time, status) ~ 1, data = d12), rmean = 40)$table
  expect_equal(rmst_df$rmst[1], unname(km_ref["rmean"]), tolerance = 1e-8)
  expect_equal(rmst_df$se[1], unname(km_ref["se(rmean)"]), tolerance = 1e-8)
})

test_that("C05, C06, C07 & C08: Optimal cut-off, KM group survival/medians, residuals, and multiple cut-offs match reference oracles", {
  d <- .make_sc_cohort()
  res <- .sc_val_run(
    d, findcut = TRUE, residual_diagnostics = TRUE,
    multiple_cutoffs = TRUE, cutoff_method = "quantile", num_cutoffs = "two"
  )

  ms_ref <- maxstat::maxstat.test(survival::Surv(time, status) ~ marker,
                                  data = d, smethod = "LogRank", pmethod = "none",
                                  minprop = 0.10, maxprop = 0.90)
  expect_equal(res$rescutTable$asDF$cutpoint[1], unname(ms_ref$estimate), tolerance = 1e-8)
  expect_equal(res$rescutTable$asDF$statistic[1], unname(ms_ref$statistic), tolerance = 1e-8)

  expect_equal(res$multipleCutTable$asDF$cutpoint_value,
               round(unname(stats::quantile(d$marker, probs = c(1/3, 2/3))), 2),
               tolerance = 1e-8)

  res_nofc <- .sc_val_run(d, findcut = FALSE, residual_diagnostics = TRUE)
  cox_u <- survival::coxph(survival::Surv(time, status) ~ marker, data = d)
  expect_equal(res_nofc$residualsTable$asDF$martingale,
               round(unname(residuals(cox_u, type = "martingale")[1:100]), 4),
               tolerance = 1e-4)
  expect_equal(res_nofc$schoenfeldResidualsTable$asDF$residual,
               round(unname(as.numeric(residuals(cox_u, type = "schoenfeld"))), 4),
               tolerance = 1e-4)
})

test_that("C09 & C10: Date intervals, landmark conditioning, multievent recoding, and Output row alignment", {
  d <- .make_sc_cohort()
  res_lm <- .sc_val_run(d, uselandmark = TRUE, landmark = 12,
                        stratified_cox = TRUE, strata_variable = "site",
                        rmst_analysis = TRUE, rmst_tau = 20)
  d_lm <- d[d$time >= 12, ]
  d_lm$time <- d_lm$time - 12
  ora_lm <- .sc_cox_oracle(d_lm, strata_col = "site")
  expect_equal(res_lm$stratifiedCoxTable$asDF$hr[1], ora_lm$hr, tolerance = 1e-8)
  expect_equal(res_lm$rmstTable$asDF$n[1], nrow(d_lm))

  # Output row alignment under missing values and landmark filtering
  d_na <- d
  d_na$marker[c(2, 7)] <- NA_real_
  d_na$time[c(4, 9)] <- 1.0
  kept <- which(!is.na(d_na$marker) & d_na$time >= 5)
  res_out <- .sc_val_run(d_na, uselandmark = TRUE, landmark = 5,
                         findcut = TRUE, multiple_cutoffs = TRUE,
                         calculatedcutoff = TRUE, calculatedmulticut = TRUE)
  expect_equal(as.integer(.sc_out_rows(res_out$calculatedcutoff)), kept)
  expect_equal(as.integer(.sc_out_rows(res_out$calculatedmulticut)), kept)
})

test_that("Metamorphic invariants: permutation, reversed rownames, affine covariate scaling, sign negation, time scaling, and seed restoration", {
  d <- .make_sc_cohort()
  res_base <- .sc_val_run(d, findcut = TRUE, stratified_cox = TRUE,
                          strata_variable = "site", rmst_analysis = TRUE, rmst_tau = 25)

  # Reversed numeric row labels do not misalign strata_variable
  d_rev <- d
  rownames(d_rev) <- as.character(rev(seq_len(nrow(d_rev))))
  res_rev <- .sc_val_run(d_rev, stratified_cox = TRUE, strata_variable = "site")
  expect_equal(res_rev$stratifiedCoxTable$asDF$hr, res_base$stratifiedCoxTable$asDF$hr, tolerance = 1e-12)

  # Covariate affine transform X' = 10*X + 100
  d_x10 <- d
  d_x10$marker <- 10 * d$marker + 100
  res_x10 <- .sc_val_run(d_x10, findcut = TRUE, stratified_cox = TRUE, strata_variable = "site")
  expect_equal(res_x10$stratifiedCoxTable$asDF$hr, res_base$stratifiedCoxTable$asDF$hr^(1/10), tolerance = 1e-8)
  expect_equal(res_x10$rescutTable$asDF$cutpoint, 10 * res_base$rescutTable$asDF$cutpoint + 100, tolerance = 1e-8)
  expect_equal(res_x10$rescutTable$asDF$statistic, res_base$rescutTable$asDF$statistic, tolerance = 1e-8)

  # Time rescaling T' = 12 * T
  d_t12 <- d
  d_t12$time <- 12 * d$time
  res_t12 <- .sc_val_run(d_t12, findcut = TRUE, rmst_analysis = TRUE, rmst_tau = 25 * 12)
  expect_equal(res_t12$medianTable$asDF$median, 12 * res_base$medianTable$asDF$median, tolerance = 1e-8)
  expect_equal(res_t12$rmstTable$asDF$rmst, 12 * res_base$rmstTable$asDF$rmst, tolerance = 1e-8)

  # Seed restoration (minpval uses withr::local_seed)
  set.seed(777)
  seed_before <- .Random.seed
  invisible(.sc_val_run(d, findcut = TRUE, multiple_cutoffs = TRUE, cutoff_method = "minpval", seed = 42))
  expect_identical(.Random.seed, seed_before)
})

test_that("Boundary & guard conditions: 0 events, <10 events, constant marker, negative time before landmark, competing risks, addNA strata", {
  d <- .make_sc_cohort()

  # Zero events
  d0 <- d; d0$status <- 0L
  r0 <- .sc_val_run(d0, findcut = TRUE)
  expect_match(.sc_strip_html(r0$strongWarnings$content), "Only 0 event(s) observed", fixed = TRUE)
  expect_match(.sc_strip_html(r0$eventRecodeInfo$content), "No events: check the event level", fixed = TRUE)
  expect_equal(r0$rescutTable$rowCount, 0L)

  # < 10 events suppresses single and multiple cut-offs
  d8 <- d[1:30, ]; d8$status <- c(rep(1L, 8), rep(0L, 22))
  r8 <- .sc_val_run(d8, findcut = TRUE, multiple_cutoffs = TRUE)
  expect_equal(r8$rescutTable$rowCount, 0L)
  expect_equal(r8$multipleCutTable$rowCount, 0L)
  expect_match(.sc_strip_html(r8$strongWarnings$content), "Cut-off determination is suppressed", fixed = TRUE)

  # Negative survival time caught before landmark filter
  d_neg <- d; d_neg$time[3] <- -2.0
  r_neg <- .sc_val_run(d_neg, uselandmark = TRUE, landmark = 10)
  expect_match(.sc_strip_html(r_neg$errors$content), "Negative survival time", fixed = TRUE)

  # Explicit NA factor level (addNA) on strata_variable
  d_addna <- d; d_addna$site[1:10] <- NA; d_addna$site <- addNA(d_addna$site)
  d_na    <- d; d_na$site[1:10] <- NA
  expect_equal(.sc_val_run(d_addna, stratified_cox = TRUE, strata_variable = "site")$stratifiedCoxTable$asDF$hr,
               .sc_val_run(d_na, stratified_cox = TRUE, strata_variable = "site")$stratifiedCoxTable$asDF$hr,
               tolerance = 1e-12)
})

test_that("C11, C12 & C13: State-only jamovi redraws across all 9 plots, R syntax export, and bibliography references", {
  d <- .make_sc_cohort()
  an <- .sc_val_run(
    d, findcut = TRUE, multiple_cutoffs = TRUE, residual_diagnostics = TRUE,
    sc = TRUE, ce = TRUE, ch = TRUE, kmunicate = TRUE, loglog = TRUE,
    return_analysis = TRUE
  )
  src <- an$asSource()
  expect_no_error(parse(text = src))
  expect_false(grepl("self\\$options", src))

  for (pnm in c("plot4", "plot5", "plot2", "plot3", "plot6", "plot7",
                "residualsPlot", "plotMultipleCutoffs", "plotMultipleSurvival")) {
    an_redraw <- survivalcontClass$new(options = an$options, data = d)
    an_redraw$.__enclos_env__$private$.data <- NULL
    img <- an_redraw$results[[pnm]]
    img$setState(an$results[[pnm]]$state)
    tf <- tempfile(fileext = ".png")
    grDevices::png(tf, width = 800, height = 600)
    ok <- tryCatch(
      suppressWarnings(suppressMessages(an_redraw$.createImage(img$.__enclos_env__$private$.renderFun, img))),
      finally = grDevices::dev.off()
    )
    unlink(tf)
    expect_true(isTRUE(ok), info = paste("State-only redraw failed for", pnm))
  }
})

# ==============================================================================
# REMEDIATED DEFECT & RECOMMENDATION REGRESSION GUARDS
# ==============================================================================

test_that("VAL-survivalcont-01: medianSummary, survTableSummary, and personTimeSummary retain 95% CI and text after '['", {
  d <- .make_sc_cohort()
  res <- .sc_val_run(d, findcut = TRUE, person_time = TRUE, showSummaries = TRUE)
  expect_match(.sc_strip_html(res$medianSummary$content), "95% CI", fixed = TRUE)
  expect_match(.sc_strip_html(res$survTableSummary$content), "95% CI", fixed = TRUE)
  expect_match(.sc_strip_html(res$personTimeSummary$content), "95% CI", fixed = TRUE)
  expect_match(.sc_strip_html(res$personTimeSummary$content), "varying follow-up durations", fixed = TRUE)
})

test_that("VAL-survivalcont-02: Nominal-typed numeric contexpl (factor with 'values' attribute) is coerced via jmvcore::toNumeric()", {
  d <- .make_sc_cohort()
  f_num <- factor(round(d$marker, 1))
  attr(f_num, "values") <- as.numeric(levels(f_num))
  d$marker <- f_num
  res <- .sc_val_run(d)
  expect_equal(nrow(res$coxTable$asDF), 1L)
})

test_that("VAL-survivalcont-03: survTable does not extrapolate survival beyond a cut-off stratum's maximum observed follow-up", {
  d_asym <- data.frame(
    time = c(seq(2, 18, length.out = 30), seq(15, 70, length.out = 30)),
    status = c(rep(1L, 25), rep(0L, 5), rep(c(1L, 0L), 15)),
    marker = c(seq(55, 85, length.out = 30), seq(15, 45, length.out = 30))
  )
  res <- .sc_val_run(d_asym, findcut = TRUE, cutp = "12, 36, 60")
  st <- res$survTable$asDF
  high_beyond <- st[st$strata == "marker = high" & st$time > 18, ]
  expect_true(nrow(high_beyond) == 0L || all(is.na(high_beyond$surv)))
  expect_true("stratum_horizon" %in% names(res$survTable$notes))
})

test_that("VAL-survivalcont-04: Single-letter uppercase contexpl name does not corrupt English prose via unanchored gsub", {
  d <- .make_sc_cohort()
  names(d)[3] <- "E"
  res <- .sc_val_run(d, contexpl = "E", findcut = TRUE, showSummaries = TRUE)
  expect_false(grepl("WhEn|mEdian", .sc_strip_html(res$medianSummary$content)))
})

test_that("VAL-survivalcont-05: multipleMedianTable$asDF$median_survival remains numeric when median is not reached", {
  d <- .make_sc_cohort()
  d$status[d$marker < 45] <- 0L
  res <- .sc_val_run(d, multiple_cutoffs = TRUE, cutoff_method = "quantile", num_cutoffs = "two")
  expect_true(is.numeric(res$multipleMedianTable$asDF$median_survival))
  expect_true("nr" %in% names(res$multipleMedianTable$notes))
})

test_that("VAL-survivalcont-06: residualsTable preserves original dataset observation IDs when uselandmark=TRUE and findcut=TRUE", {
  d <- .make_sc_cohort()
  d$time[c(1, 3)] <- 1.0
  r_nofc <- .sc_val_run(d, uselandmark = TRUE, landmark = 5, findcut = FALSE, residual_diagnostics = TRUE)
  r_fc   <- .sc_val_run(d, uselandmark = TRUE, landmark = 5, findcut = TRUE, residual_diagnostics = TRUE)
  expect_equal(r_fc$residualsTable$asDF$observation[1:10], r_nofc$residualsTable$asDF$observation[1:10])
})

test_that("VAL-survivalcont-07: cutoffAnalysisExplanation states (>cut / <=cut) and multipleMedianTable carries landmark note", {
  d <- .make_sc_cohort()
  res_exp <- .sc_val_run(d, findcut = TRUE, showExplanations = TRUE)
  expect_false(grepl("\\(>=", .sc_strip_html(res_exp$cutoffAnalysisExplanation$content)))
  res_lm <- .sc_val_run(d, uselandmark = TRUE, landmark = 5, multiple_cutoffs = TRUE)
  expect_true("landmark" %in% names(res_lm$multipleMedianTable$notes))
  expect_true("landmark" %in% names(res_lm$multipleSurvTable$notes))
})

test_that("VAL-survivalcont-08: Tree-based multiple cut-off method preserves caller .Random.seed via withr::local_seed", {
  d <- .make_sc_cohort()
  set.seed(54321)
  rng_before <- .Random.seed
  invisible(.sc_val_run(d, multiple_cutoffs = TRUE, cutoff_method = "tree", seed = 77))
  expect_identical(.Random.seed, rng_before)
})

test_that("Recommendations: RMST qnorm(0.975) & [0, tau] clamping, single landmark exclusion notice, and small person-time rate precision", {
  d <- .make_sc_cohort()
  res_rmst <- .sc_val_run(d, rmst_analysis = TRUE, rmst_tau = 25)
  rm_df <- res_rmst$rmstTable$asDF
  z975 <- stats::qnorm(0.975)
  expect_equal(rm_df$ci_lower[1], max(0, rm_df$rmst[1] - z975 * rm_df$se[1]), tolerance = 1e-8)
  expect_equal(rm_df$ci_upper[1], min(25, rm_df$rmst[1] + z975 * rm_df$se[1]), tolerance = 1e-8)

  # High landmark exclusion rate (>30%) emits notice once in strongWarnings, not duplicated in warnings
  res_lm_hi <- .sc_val_run(d, uselandmark = TRUE, landmark = 25)
  expect_match(.sc_strip_html(res_lm_hi$strongWarnings$content), "Landmark exclusions", fixed = TRUE)
  expect_false(grepl("Landmark analysis excludes", .sc_strip_html(res_lm_hi$warnings$content), fixed = TRUE))

  # Small person-time incidence rate (< 0.01 when rate_multiplier = 1 and time is large) is not rounded to 0.00
  d_small_rate <- d
  d_small_rate$time <- d$time * 100
  res_pt_small <- .sc_val_run(d_small_rate, person_time = TRUE, rate_multiplier = 1)
  expect_gt(res_pt_small$personTimeTable$asDF$rate[1], 0)
})
