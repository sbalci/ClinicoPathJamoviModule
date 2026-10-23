# ==============================================================================
# Exhaustive Validation & Regression Test Suite for ClinicoPath::singlearm
# Generated per .claude/commands/validate-function.md (depth=exhaustive)
# Date: 2026-10-22
# Companion report: development-ideas/singlearm-validation-2026-10-22.md
# Companion matrix: development-ideas/singlearm-validation-2026-10-22-matrix.csv
# ==============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(jmvcore)
  library(R6)
  library(survival)
  library(cmprsk)
  library(KMunicate)
  library(magrittr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(labelled)
  library(janitor)
  library(scales)
  library(glue)
  library(htmltools)
  library(digest)
  library(yaml)
}))

.find_root <- function(start) {
  d <- suppressWarnings(normalizePath(start, mustWork = FALSE))
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "R", "singlearm.b.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  NA_character_
}
.root <- .find_root(file.path(dirname(dirname(getwd()))))
if (is.na(.root)) .root <- .find_root(getwd())
if (is.na(.root)) stop("Could not locate repo root containing R/singlearm.b.R")

for (f in c(
  "utils.R",
  "utils-formula.R",
  "utils-eventindicator.R",
  "utils-followup.R",
  "utils-observed-cif.R",
  "survival-competingrisks.R",
  "singlearm.h.R",
  "singlearm.b.R"
)) {
  source(file.path(.root, "R", f))
}

for (rda in list.files(file.path(.root, "data"), pattern = "^singlearm_.*\\.rda$", full.names = TRUE)) {
  load(rda)
}

.sa_val_run <- function(data,
                        ...,
                        outcomeLevel = NULL,
                        dod = NULL,
                        dooc = NULL,
                        awd = NULL,
                        awod = NULL,
                        calculatedtime = FALSE,
                        outcomeredefined = FALSE,
                        return_analysis = FALSE) {
  df <- as.data.frame(data)
  if (!isTRUE(calculatedtime) && !isTRUE(outcomeredefined) && !isTRUE(return_analysis)) {
    return(suppressWarnings(suppressMessages(
      singlearm(
        data = df,
        outcomeLevel = outcomeLevel,
        dod = dod,
        dooc = dooc,
        awd = awd,
        awod = awod,
        ...
      )
    )))
  }
  for (v in names(df)) {
    if (is.character(df[[v]])) df[[v]] <- as.factor(df[[v]])
  }
  opts <- singlearmOptions$new(
    outcomeLevel = outcomeLevel,
    dod = dod,
    dooc = dooc,
    awd = awd,
    awod = awod,
    ...
  )
  set_out <- function(nm, val) {
    if (isTRUE(val)) {
      opts$.__enclos_env__$private[[paste0("..", nm)]]$.__enclos_env__$private$.value <-
        list(value = TRUE, vars = nm)
    }
  }
  set_out("calculatedtime", calculatedtime)
  set_out("outcomeredefined", outcomeredefined)
  analysis <- singlearmClass$new(options = opts, data = df)
  suppressWarnings(suppressMessages(analysis$run()))
  if (isTRUE(return_analysis)) return(analysis)
  analysis$results
}

.out_vals <- function(out_item) unlist(out_item$.__enclos_env__$private$.values, use.names = FALSE)
.out_rows <- function(out_item) out_item$.__enclos_env__$private$.rowNums
.strip_html <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  gsub("\\s+", " ", trimws(gsub("<[^>]+>", " ", paste(x, collapse = " "))))
}

# Zero-dependency mathematical oracles
.km_oracle <- function(time, status, eval_times = numeric(0)) {
  ut <- sort(unique(time))
  k <- length(ut)
  n_risk <- d <- c_cens <- integer(k)
  for (i in seq_len(k)) {
    n_risk[i] <- sum(time >= ut[i])
    d[i]      <- sum(time == ut[i] & status == 1)
    c_cens[i] <- sum(time == ut[i] & status == 0)
  }
  surv <- cumprod(1 - d / n_risk)
  gw_term <- ifelse(n_risk > d & d > 0, d / (n_risk * (n_risk - d)), 0)
  cum_gw  <- cumsum(gw_term)
  se_surv <- surv * sqrt(cum_gw)
  z <- stats::qnorm(0.975)
  theta <- log(-log(surv))
  se_theta <- sqrt(cum_gw) / abs(log(surv))
  lower <- ifelse(surv == 1, 1, ifelse(surv == 0, 0, exp(-exp(theta + z * se_theta))))
  upper <- ifelse(surv == 1, 1, ifelse(surv == 0, 0, exp(-exp(theta - z * se_theta))))
  dt <- diff(c(0, ut))
  s_prev <- c(1, surv[-k])
  rmean <- sum(s_prev * dt)
  tail_area <- rev(cumsum(rev(surv * c(diff(ut), 0))))
  se_rmean <- sqrt(sum((tail_area^2) * gw_term))
  idx_le50 <- which(surv <= 0.5 + 1e-12)
  if (length(idx_le50) == 0) {
    med <- NA_real_
  } else {
    i1 <- idx_le50[1]
    if (abs(surv[i1] - 0.5) < 1e-12) {
      i2 <- which(surv < 0.5 - 1e-12)[1]
      med <- if (!is.na(i2)) (ut[i1] + ut[i2]) / 2 else NA_real_
    } else {
      med <- ut[i1]
    }
  }
  med_l <- if (any(lower <= 0.5, na.rm = TRUE)) ut[which(lower <= 0.5)[1]] else NA_real_
  med_u <- if (any(upper <  0.5, na.rm = TRUE)) ut[which(upper <  0.5)[1]] else NA_real_
  eval_df <- NULL
  if (length(eval_times) > 0) {
    n_rt <- n_ev_int <- integer(length(eval_times))
    s_at <- l_at <- u_at <- numeric(length(eval_times))
    prev_t <- 0
    for (j in seq_along(eval_times)) {
      tj <- eval_times[j]
      idx <- which(ut <= tj)
      last_i <- max(idx)
      s_at[j] <- surv[last_i]
      l_at[j] <- lower[last_i]
      u_at[j] <- upper[last_i]
      n_rt[j] <- sum(time >= tj)
      n_ev_int[j] <- sum(time > prev_t & time <= tj & status == 1)
      prev_t <- tj
    }
    eval_df <- data.frame(time = eval_times, n.risk = n_rt, n.event = n_ev_int,
                          surv = s_at, lower = l_at, upper = u_at)
  }
  list(records = length(time), events = sum(status == 1),
       rmean = rmean, se_rmean = se_rmean, median = med,
       x0_95lcl = med_l, x0_95ucl = med_u, eval = eval_df)
}

.aj_oracle <- function(time, status, eval_times = numeric(0)) {
  ut <- sort(unique(time))
  k <- length(ut)
  n_risk <- d1 <- d2 <- integer(k)
  for (i in seq_len(k)) {
    n_risk[i] <- sum(time >= ut[i])
    d1[i] <- sum(time == ut[i] & status == 1)
    d2[i] <- sum(time == ut[i] & status == 2)
  }
  surv_all <- cumprod(1 - (d1 + d2) / n_risk)
  s_prev <- c(1, surv_all[-k])
  cif1 <- cumsum(s_prev * (d1 / n_risk))
  cif2 <- cumsum(s_prev * (d2 / n_risk))
  med_cif1 <- if (any(cif1 >= 0.5)) ut[which(cif1 >= 0.5)[1]] else NA_real_
  s_at <- c1_at <- c2_at <- numeric(length(eval_times))
  for (j in seq_along(eval_times)) {
    li <- max(which(ut <= eval_times[j]))
    s_at[j] <- surv_all[li]; c1_at[j] <- cif1[li]; c2_at[j] <- cif2[li]
  }
  list(median_cif1 = med_cif1,
       eval = data.frame(time = eval_times, event_free = s_at, cif1 = c1_at, cif2 = c2_at))
}

.garwood_oracle <- function(events, pt, mult = 100) {
  rate <- (events / pt) * mult
  lo <- ifelse(events == 0, 0, (stats::qchisq(0.025, 2 * events) / (2 * pt)) * mult)
  hi <- (stats::qchisq(0.975, 2 * (events + 1)) / (2 * pt)) * mult
  list(rate = rate, lo = lo, hi = hi)
}

# ==============================================================================
# 1. Hand-Calculable Oracles & Canonical Reference Parity (C01-C12)
# ==============================================================================

test_that("VAL-A1 & B1: KM survival, Greenwood SE, log-log CI, Brookmeyer-Crowley median CI, and RMST match first-principles oracle", {
  d_a1 <- data.frame(
    time = c(2, 4, 6, 8, 10, 12, 15, 18, 20, 24),
    status = factor(c("Dead", "Dead", "Alive", "Dead", "Alive", "Dead", "Dead", "Alive", "Dead", "Alive"),
                    levels = c("Alive", "Dead"))
  )
  ora <- .km_oracle(d_a1$time, as.integer(d_a1$status == "Dead"), eval_times = c(4, 12, 20))
  res <- .sa_val_run(d_a1, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", cutp = "4, 12, 20")

  expect_equal(c(res$survTable$asDF$surv, res$survTable$asDF$lower, res$survTable$asDF$upper),
               c(ora$eval$surv, ora$eval$lower, ora$eval$upper), tolerance = 1e-8)
  expect_equal(c(res$survTable$asDF$n.risk, res$survTable$asDF$n.event),
               c(ora$eval$n.risk, ora$eval$n.event))
  expect_equal(as.numeric(res$medianTable$asDF[, c("records", "events", "median", "x0_95lcl", "x0_95ucl")]),
               c(ora$records, ora$events, ora$median, ora$x0_95lcl, ora$x0_95ucl), tolerance = 1e-8)
  expect_equal(as.numeric(res$medianTable$asDF[, c("rmean", "se_rmean")]),
               c(ora$rmean, ora$se_rmean), tolerance = 1e-8)

  # Canonical survival::lung parity
  d_lung <- survival::lung
  d_lung$status01 <- ifelse(d_lung$status == 2, 1L, 0L)
  ora_lung <- .km_oracle(d_lung$time, d_lung$status01, eval_times = c(180, 365, 540))
  res_lung <- .sa_val_run(d_lung, elapsedtime = "time", outcome = "status01", outcomeLevel = "1",
                          cutp = "180, 365, 540", timetypeoutput = "days")
  expect_equal(as.numeric(res_lung$medianTable$asDF[, c("records", "events", "median", "x0_95lcl", "x0_95ucl")]),
               c(ora_lung$records, ora_lung$events, ora_lung$median, ora_lung$x0_95lcl, ora_lung$x0_95ucl),
               tolerance = 1e-8)
  expect_equal(as.numeric(res_lung$medianTable$asDF[, c("rmean", "se_rmean")]),
               c(ora_lung$rmean, ora_lung$se_rmean), tolerance = 1e-8)
})

test_that("VAL-A2 & B2: Competing-risks Aalen-Johansen CIF matches zero-dependency oracle and cmprsk::cuminc", {
  d_a2 <- data.frame(
    time = c(2, 4, 5, 8, 10, 12, 14, 18, 20, 24),
    status = factor(c("DOD", "DOOC", "AWOD", "DOD", "DOD", "DOOC", "DOD", "AWD", "DOD", "AWOD"),
                    levels = c("AWOD", "AWD", "DOD", "DOOC"))
  )
  ora_a2 <- .aj_oracle(
    d_a2$time,
    ifelse(d_a2$status == "DOD", 1L, ifelse(d_a2$status == "DOOC", 2L, 0L)),
    eval_times = c(4, 12, 20)
  )
  res_a2 <- .sa_val_run(
    d_a2, elapsedtime = "time", outcome = "status", multievent = TRUE,
    analysistype = "compete", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
    cutp = "4, 12, 20", cif_all_causes = TRUE, show_followup_support = TRUE
  )
  expect_equal(res_a2$survTable$asDF$surv, ora_a2$eval$cif1, tolerance = 1e-8)
  expect_equal(res_a2$medianTable$asDF$median, ora_a2$median_cif1, tolerance = 1e-8)

  # State occupation conservation sum_s P_s(t) == 1
  cif_sums <- as.numeric(tapply(res_a2$cifTable$asDF$estimate, res_a2$cifTable$asDF$time, sum))
  expect_equal(cif_sums, c(1, 1, 1), tolerance = 1e-12)
})

test_that("VAL-A3, A4 & D5: Person-time exposure, Garwood 95% CIs, and piecewise baseline hazard match closed-form chi-square oracle", {
  d_a3 <- data.frame(
    time = c(5, 10, 12, 15, 18, 22, 25, 30, 35, 40),
    status = c(1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L)
  )
  res_pt <- .sa_val_run(d_a3, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                        person_time = TRUE, time_intervals = "15, 30", rate_multiplier = 100)
  tb_pt <- res_pt$personTimeTable$asDF
  gw_ov <- .garwood_oracle(6, 212, 100)
  gw_i1 <- .garwood_oracle(3, 132, 100)
  gw_i2 <- .garwood_oracle(2,  65, 100)
  gw_i3 <- .garwood_oracle(1,  15, 100)

  expect_equal(as.numeric(tb_pt[tb_pt$interval == "Overall (0-max)", c("events", "person_time", "rate", "rate_ci_lower", "rate_ci_upper")]),
               c(6, 212, round(gw_ov$rate, 2), round(gw_ov$lo, 2), round(gw_ov$hi, 2)))
  expect_equal(as.numeric(tb_pt[tb_pt$interval == "0-15", c("events", "person_time", "rate", "rate_ci_lower", "rate_ci_upper")]),
               c(3, 132, round(gw_i1$rate, 2), round(gw_i1$lo, 2), round(gw_i1$hi, 2)))
  expect_equal(as.numeric(tb_pt[tb_pt$interval == "15-30", c("events", "person_time", "rate", "rate_ci_lower", "rate_ci_upper")]),
               c(2, 65, round(gw_i2$rate, 2), round(gw_i2$lo, 2), round(gw_i2$hi, 2)))
  expect_equal(as.numeric(tb_pt[tb_pt$interval == "30+", c("events", "person_time", "rate", "rate_ci_lower", "rate_ci_upper")]),
               c(1, 15, round(gw_i3$rate, 2), round(gw_i3$lo, 2), round(gw_i3$hi, 2)))

  # Piecewise baseline hazard across 3 bins (30 events)
  d_a4 <- do.call(rbind, replicate(5, d_a3, simplify = FALSE))
  res_bh <- .sa_val_run(d_a4, elapsedtime = "time", outcome = "status", outcomeLevel = "1", baseline_hazard = TRUE)
  tb_bh <- res_bh$baselineHazardTable$asDF
  breaks_a4 <- seq(0, 40, length.out = 4)
  exp_bh <- lapply(seq_len(3), function(k) {
    a <- breaks_a4[k]; b <- breaks_a4[k + 1]
    pt_k <- sum(pmax(0, pmin(d_a4$time, b) - a))
    ev_k <- sum(d_a4$status == 1 & d_a4$time <= b & (if (k == 1L) d_a4$time >= a else d_a4$time > a))
    gw_k <- .garwood_oracle(ev_k, pt_k, mult = 1)
    c(round(gw_k$rate, 4), round(gw_k$lo, 4), round(gw_k$hi, 4))
  })
  expect_equal(c(tb_bh$hazard, tb_bh$hazard_lower, tb_bh$hazard_upper),
               c(sapply(exp_bh, `[`, 1), sapply(exp_bh, `[`, 2), sapply(exp_bh, `[`, 3)),
               tolerance = 1e-8)
})

test_that("VAL-A5 & B4: Date intervals, strict landmark conditioning, and Schemper-Smith reverse-KM follow-up", {
  d_a5 <- data.frame(
    dx = rep("2020-01-01", 10),
    fu = c("2020-01-11", "2020-01-21", "2020-02-05", "2020-02-20", "2020-03-11",
           "2020-03-31", "2020-04-20", "2020-05-10", "2020-05-30", "2020-06-19"),
    status = c(1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L)
  )
  exp_days <- as.numeric(as.Date(d_a5$fu) - as.Date(d_a5$dx))
  r_lm <- .sa_val_run(d_a5, tint = TRUE, dxdate = "dx", fudate = "fu",
                      timetypedata = "ymd", timetypeoutput = "days",
                      outcome = "status", outcomeLevel = "1",
                      uselandmark = TRUE, landmark = 35, calculatedtime = TRUE)
  ora_lm <- .km_oracle(exp_days[exp_days > 35] - 35, d_a5$status[exp_days > 35])
  expect_equal(c(r_lm$medianTable$asDF$records, r_lm$medianTable$asDF$events,
                 r_lm$medianTable$asDF$median, r_lm$medianTable$asDF$rmean),
               c(7, 4, ora_lm$median, ora_lm$rmean), tolerance = 1e-8)
  expect_equal(as.numeric(.out_vals(r_lm$calculatedtime)), exp_days, tolerance = 1e-8)

  # Reverse-KM median follow-up on singlearm_test
  res_b4 <- .sa_val_run(singlearm_test, elapsedtime = "time_months", outcome = "outcome",
                        outcomeLevel = "Dead", advancedDiagnostics = TRUE)
  sf_rev <- survival::survfit(survival::Surv(singlearm_test$time_months, as.integer(singlearm_test$outcome != "Dead")) ~ 1)
  rev_med <- round(as.numeric(summary(sf_rev)$table["median"]), 1)
  dq <- res_b4$dataQualityTable$asDF
  expect_true(grepl(paste0(rev_med, " months"), dq$value[dq$metric == "Median Follow-up (reverse KM)"], fixed = TRUE))
})

# ==============================================================================
# 2. Metamorphic Relations, Boundary Guards & Option Coverage (Batteries C, D, E, H, J)
# ==============================================================================

test_that("VAL-C1..C3: Row permutation, positive time scaling, and cohort duplication metamorphic relations hold", {
  d_a1 <- data.frame(
    time = c(2, 4, 6, 8, 10, 12, 15, 18, 20, 24),
    status = factor(c("Dead", "Dead", "Alive", "Dead", "Alive", "Dead", "Dead", "Alive", "Dead", "Alive"),
                    levels = c("Alive", "Dead"))
  )
  r_orig <- .sa_val_run(d_a1, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                        cutp = "4, 12, 20", person_time = TRUE, time_intervals = "4, 12", rate_multiplier = 100)
  set.seed(20261022)
  r_perm <- .sa_val_run(d_a1[sample(nrow(d_a1)), ], elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                        cutp = "4, 12, 20", person_time = TRUE, time_intervals = "4, 12", rate_multiplier = 100)
  expect_equal(r_perm$survTable$asDF, r_orig$survTable$asDF, tolerance = 1e-12)
  expect_equal(r_perm$medianTable$asDF, r_orig$medianTable$asDF, tolerance = 1e-12)
  expect_equal(r_perm$personTimeTable$asDF, r_orig$personTimeTable$asDF, tolerance = 1e-12)

  # Positive time scaling (t -> 10t, cutp -> 10*cutp, rate_multiplier -> 1000)
  d_scaled <- transform(d_a1, time = time * 10)
  r_scaled <- .sa_val_run(d_scaled, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                          cutp = "40, 120, 200", person_time = TRUE, time_intervals = "40, 120", rate_multiplier = 1000)
  expect_equal(r_scaled$survTable$asDF[, c("surv", "lower", "upper")],
               r_orig$survTable$asDF[, c("surv", "lower", "upper")], tolerance = 1e-12)
  expect_equal(as.numeric(r_scaled$medianTable$asDF[, c("rmean", "se_rmean", "median", "x0_95lcl")]),
               10 * as.numeric(r_orig$medianTable$asDF[, c("rmean", "se_rmean", "median", "x0_95lcl")]),
               tolerance = 1e-12)
  expect_equal(r_scaled$personTimeTable$asDF[, c("rate", "rate_ci_lower", "rate_ci_upper")],
               r_orig$personTimeTable$asDF[, c("rate", "rate_ci_lower", "rate_ci_upper")], tolerance = 1e-12)

  # Cohort duplication (2N)
  r_dup <- .sa_val_run(rbind(d_a1, d_a1), elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", cutp = "4, 12, 20")
  expect_equal(r_dup$survTable$asDF$surv, r_orig$survTable$asDF$surv, tolerance = 1e-12)
  expect_equal(r_dup$medianTable$asDF$se_rmean, r_orig$medianTable$asDF$se_rmean / sqrt(2), tolerance = 1e-8)
})

test_that("VAL-D & E: Boundary guards, outcomeorganizer hand-off coding, HTML sweep, and bibliography integrity", {
  # Negative & infinite survival time guards
  d_neg <- data.frame(time = c(-2, 5, 10, 15, 20), status = c(1L, 0L, 1L, 0L, 1L))
  r_neg <- .sa_val_run(d_neg, elapsedtime = "time", outcome = "status", outcomeLevel = "1")
  expect_match(.strip_html(r_neg$errors$content), "time value\\(s\\) are negative")
  expect_equal(nrow(r_neg$medianTable$asDF), 0L)

  d_inf <- data.frame(time = c(Inf, 5, 10, 15, 20), status = c(1L, 0L, 1L, 0L, 1L))
  r_inf <- .sa_val_run(d_inf, elapsedtime = "time", outcome = "status", outcomeLevel = "1")
  expect_match(.strip_html(r_inf$errors$content), "infinite \\(Inf\\)")
  expect_equal(nrow(r_inf$medianTable$asDF), 0L)

  # outcomeorganizer hand-off: Event vs Competing refocusing and Censored rejection
  d_ho <- data.frame(
    time = c(2, 4, 5, 8, 10, 12, 14, 18, 20, 24),
    status_ho = factor(c("Event", "Competing", "Censored", "Event", "Event", "Competing", "Event", "Censored", "Event", "Censored"),
                       levels = c("Censored", "Event", "Competing"))
  )
  r_ho_ev <- .sa_val_run(d_ho, elapsedtime = "time", outcome = "status_ho", outcomeLevel = "Event",
                         multievent = FALSE, cutp = "4, 12, 20")
  r_ho_cp <- .sa_val_run(d_ho, elapsedtime = "time", outcome = "status_ho", outcomeLevel = "Competing",
                         multievent = FALSE, cutp = "4, 12, 20")
  r_ho_ce <- .sa_val_run(d_ho, elapsedtime = "time", outcome = "status_ho", outcomeLevel = "Censored",
                         multievent = FALSE, cutp = "4, 12, 20")
  expect_equal(r_ho_cp$survTable$asDF$surv,
               r_ho_ev$cifTable$asDF$estimate[r_ho_ev$cifTable$asDF$cause == "Competing event(s)"],
               tolerance = 1e-12)
  expect_match(.strip_html(r_ho_ce$errors$content), "'Censored' cannot be selected as the event level")

  # Bibliography integrity
  r_yaml <- yaml::read_yaml(file.path(.root, "jamovi", "singlearm.r.yaml"))
  refs_yaml <- yaml::read_yaml(file.path(.root, "jamovi", "00refs.yaml"))$refs
  all_refs <- unique(c(r_yaml$refs, unlist(lapply(r_yaml$items, function(it) it$refs))))
  expect_length(setdiff(all_refs, names(refs_yaml)), 0L)
})

# ==============================================================================
# 3. Resolved Defect Regression Guards (VAL-singlearm-01 .. VAL-singlearm-07)
# ==============================================================================

test_that("VAL-singlearm-01: cifTable 'Event of interest' 95% CIs match survTable 95% CIs (conf.type='log-log')", {
  d_mgus <- survival::mgus2
  d_mgus$event_state <- factor(
    ifelse(d_mgus$pstat == 1, "PCM", ifelse(d_mgus$death == 1, "Death_No_PCM", "Alive")),
    levels = c("Alive", "PCM", "Death_No_PCM")
  )
  res <- .sa_val_run(
    d_mgus, elapsedtime = "ptime", outcome = "event_state",
    multievent = TRUE, analysistype = "compete",
    dod = "PCM", dooc = "Death_No_PCM", awd = "Alive", awod = "Alive",
    cutp = "12, 60, 120", cif_all_causes = TRUE
  )
  cif_ev <- res$cifTable$asDF[res$cifTable$asDF$cause == "Event of interest", ]
  expect_equal(c(cif_ev$lower, cif_ev$upper),
               c(res$survTable$asDF$lower, res$survTable$asDF$upper), tolerance = 1e-8)
})

test_that("VAL-singlearm-02: Exhausted competing-risk follow-up cutpoint retention and last-time CIF>=50% crossing notice", {
  d_exh <- data.frame(
    time = c(5, 8, 10, 12, 15, 18, 20, 25, 30, 35, 40, 40),
    status = factor(c("DOD", "DOOC", "DOD", "AWOD", "DOD", "DOOC", "DOD", "AWD", "DOD", "DOOC", "DOD", "DOOC"),
                    levels = c("AWOD", "AWD", "DOD", "DOOC"))
  )
  r_exh <- .sa_val_run(
    d_exh, elapsedtime = "time", outcome = "status",
    multievent = TRUE, analysistype = "compete",
    dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
    cutp = "12, 36, 60", showSummaries = TRUE
  )
  expect_equal(sort(unique(r_exh$cifTable$asDF$time)), r_exh$survTable$asDF$time)
  expect_false(grepl("Cumulative incidence did not reach 50%", .strip_html(r_exh$info$content), fixed = TRUE))
  expect_false(grepl("did not exceed 50% within the observed follow-up", .strip_html(r_exh$medianSummary$content), fixed = TRUE))
})

test_that("VAL-singlearm-03: plot and baselineHazardPlot state-only jamovi redraw (private$.data <- NULL) returns TRUE", {
  an_std <- .sa_val_run(
    singlearm_test, elapsedtime = "time_months", outcome = "outcome", outcomeLevel = "Dead",
    sc = TRUE, baseline_hazard = TRUE, return_analysis = TRUE
  )
  df_init <- as.data.frame(singlearm_test)
  for (v in names(df_init)) if (is.character(df_init[[v]])) df_init[[v]] <- as.factor(df_init[[v]])
  for (pnm in c("plot", "baselineHazardPlot")) {
    an_redraw <- singlearmClass$new(options = an_std$options, data = df_init)
    an_redraw$.__enclos_env__$private$.data <- NULL
    img_r <- an_redraw$results[[pnm]]
    img_r$setState(an_std$results[[pnm]]$state)
    expect_true(an_redraw$.createImage(img_r$.__enclos_env__$private$.renderFun, img_r))
  }
})

test_that("VAL-singlearm-04: calculatedtime and outcomeredefined Output columns preserve rowNums on filtered data", {
  d_a5 <- data.frame(
    dx = rep("2020-01-01", 10),
    fu = c("2020-01-11", "2020-01-21", "2020-02-05", "2020-02-20", "2020-03-11",
           "2020-03-31", "2020-04-20", "2020-05-10", "2020-05-30", "2020-06-19"),
    status = c(1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L)
  )
  d_filt <- d_a5[-(1:3), ]
  r_filt <- .sa_val_run(
    d_filt, tint = TRUE, dxdate = "dx", fudate = "fu",
    timetypedata = "ymd", timetypeoutput = "days",
    outcome = "status", outcomeLevel = "1", calculatedtime = TRUE
  )
  expect_equal(as.integer(.out_rows(r_filt$calculatedtime)), as.integer(rownames(d_filt)))
})

test_that("VAL-singlearm-05: Cutpoint validation warning is emitted only once per run in competing-risk mode", {
  d_a2 <- data.frame(
    time = c(2, 4, 5, 8, 10, 12, 14, 18, 20, 24),
    status = factor(c("DOD", "DOOC", "AWOD", "DOD", "DOD", "DOOC", "DOD", "AWD", "DOD", "AWOD"),
                    levels = c("AWOD", "AWD", "DOD", "DOOC"))
  )
  r_dup <- .sa_val_run(
    d_a2, elapsedtime = "time", outcome = "status",
    multievent = TRUE, analysistype = "compete",
    dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
    cutp = "12, abc, 20"
  )
  m <- gregexpr("contain non-numeric value", r_dup$warnings$content, fixed = TRUE)[[1]]
  expect_length(m, 1L)
})

test_that("VAL-singlearm-06: outcomeorganizer competing-risk hand-off hides single-event plots plot2, plot3, plot6", {
  d_ho <- data.frame(
    time = c(2, 4, 5, 8, 10, 12, 14, 18, 20, 24),
    status_ho = factor(c("Event", "Competing", "Censored", "Event", "Event", "Competing", "Event", "Censored", "Event", "Censored"),
                       levels = c("Censored", "Event", "Competing"))
  )
  r_ho <- .sa_val_run(
    d_ho, elapsedtime = "time", outcome = "status_ho", outcomeLevel = "Event",
    multievent = FALSE, sc = TRUE, ce = TRUE, ch = TRUE, kmunicate = TRUE
  )
  expect_false(r_ho$plot2$visible)
  expect_false(r_ho$plot3$visible)
  expect_false(r_ho$plot6$visible)
})

test_that("VAL-singlearm-07: Public R wrapper singlearm() populates calculatedtime and outcomeredefined Output items", {
  d_a5 <- data.frame(
    dx = rep("2020-01-01", 10),
    fu = c("2020-01-11", "2020-01-21", "2020-02-05", "2020-02-20", "2020-03-11",
           "2020-03-31", "2020-04-20", "2020-05-10", "2020-05-30", "2020-06-19"),
    status = c(1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L)
  )
  r_wrap <- .sa_val_run(
    d_a5, tint = TRUE, dxdate = "dx", fudate = "fu",
    timetypedata = "ymd", timetypeoutput = "days",
    outcome = "status", outcomeLevel = "1", calculatedtime = FALSE
  )
  expect_length(.out_vals(r_wrap$calculatedtime), nrow(d_a5))
})
