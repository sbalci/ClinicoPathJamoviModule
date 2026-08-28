library(testthat)

# Shared reference cohort: a real marker -> hazard relationship so that every
# reference quantity below is non-degenerate.
make_survivalcont_cohort <- function(n = 240) {
  set.seed(2026)
  marker <- round(stats::rnorm(n, 50, 12), 1)
  t_ev <- stats::rexp(n, rate = 0.012 * exp(0.045 * (marker - 50)))
  t_cen <- stats::rexp(n, rate = 0.006)
  data.frame(
    time = round(pmin(t_ev, t_cen) + 0.5, 2),
    status = as.integer(t_ev <= t_cen),
    marker = marker,
    site = factor(rep_len(c("A", "B"), n))
  )
}

# finalfit renders the HR as "1.04 (1.02-1.05, p<0.001)".
leading_number <- function(x) as.numeric(sub("^\\s*([0-9.]+).*$", "\\1", x))

test_that("Cox, cut-point, median, S(t), person-time, RMST and stratified Cox match reference packages", {
  skip_if_not_installed("survminer")
  df <- make_survivalcont_cohort()

  res <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, person_time = TRUE, rmst_analysis = TRUE,
    stratified_cox = TRUE, strata_variable = "site",
    timetypeoutput = "months", cutp = "12, 36, 60",
    rate_multiplier = 100, time_intervals = "12, 36, 60"
  )

  ## Cox regression -----------------------------------------------------------
  cox_ref <- survival::coxph(survival::Surv(time, status) ~ marker, data = df)
  hr_ref <- round(unname(summary(cox_ref)$conf.int[1, "exp(coef)"]), 2)
  cox_tbl <- res$coxTable$asDF
  expect_equal(leading_number(cox_tbl$HR_univariable[1]), hr_ref, tolerance = 5e-3)

  ## Maximally selected rank cut-point ----------------------------------------
  cut_ref <- survminer::surv_cutpoint(df, time = "time", event = "status",
                                      variables = "marker", minprop = 0.10)
  cut_tbl <- res$rescutTable$asDF
  expect_equal(cut_tbl$cutpoint[1], summary(cut_ref)$cutpoint)
  expect_equal(cut_tbl$statistic[1], summary(cut_ref)$statistic)

  ## Median survival and survival probabilities by cut-off group --------------
  cat_df <- survminer::surv_categorize(cut_ref)
  fit_g <- survival::survfit(survival::Surv(time, status) ~ marker, data = cat_df)
  tbl_ref <- summary(fit_g)$table
  med_tbl <- res$medianTable$asDF
  expect_equal(nrow(med_tbl), nrow(tbl_ref))
  expect_equal(med_tbl$median, unname(tbl_ref[, "median"]))
  expect_equal(med_tbl$records, unname(tbl_ref[, "records"]))
  expect_equal(med_tbl$events, as.integer(unname(tbl_ref[, "events"])))

  s_ref <- summary(fit_g, times = c(12, 36, 60), extend = TRUE)
  surv_tbl <- res$survTable$asDF
  expect_equal(surv_tbl$surv, s_ref$surv)
  expect_equal(surv_tbl$lower, s_ref$lower)
  expect_equal(surv_tbl$upper, s_ref$upper)
  expect_equal(surv_tbl$n.risk, as.integer(s_ref$n.risk))

  ## Person-time: exact (Garwood) Poisson interval ----------------------------
  pt <- res$personTimeTable$asDF
  tot_t <- sum(df$time)
  tot_e <- sum(df$status)
  expect_equal(pt$person_time[1], round(tot_t, 2))
  expect_equal(pt$events[1], tot_e)
  expect_equal(pt$rate[1], round(tot_e / tot_t * 100, 2))
  expect_equal(pt$rate_ci_lower[1],
               round(stats::qchisq(0.025, 2 * tot_e) / 2 / tot_t * 100, 2))
  expect_equal(pt$rate_ci_upper[1],
               round(stats::qchisq(0.975, 2 * (tot_e + 1)) / 2 / tot_t * 100, 2))

  ## RMST vs survfit(rmean = tau) ---------------------------------------------
  rm_tbl <- res$rmstTable$asDF
  tau <- rm_tbl$tau[1]
  expect_true(is.finite(tau) && tau > 0)
  for (i in seq_len(nrow(rm_tbl))) {
    gd <- cat_df[cat_df$marker == rm_tbl$group[i], ]
    ref <- summary(survival::survfit(survival::Surv(time, status) ~ 1, data = gd),
                   rmean = tau, extend = TRUE)$table
    expect_equal(rm_tbl$rmst[i], unname(ref["rmean"]))
    expect_equal(rm_tbl$se[i], unname(ref["se(rmean)"]))
  }

  ## Stratified Cox -----------------------------------------------------------
  scox <- survival::coxph(survival::Surv(time, status) ~ marker + survival::strata(site),
                          data = df)
  sref <- summary(scox)
  st <- res$stratifiedCoxTable$asDF
  expect_equal(nrow(st), 1L)
  expect_equal(st$hr[1], unname(sref$conf.int[1, "exp(coef)"]))
  expect_equal(st$ci_lower[1], unname(sref$conf.int[1, "lower .95"]))
  expect_equal(st$ci_upper[1], unname(sref$conf.int[1, "upper .95"]))
  expect_equal(st$pvalue[1], unname(sref$coefficients[1, 5]))
})

test_that("Regression: residual diagnostics populate when findcut is enabled", {
  # surv_categorize() output has no `row_names` column, so `as.integer(NULL[i])`
  # returned integer(0) and the is.na() guard threw "argument is of length zero".
  # Both residual tables then showed a single all-NA row plus an error note.
  skip_if_not_installed("survminer")
  df <- make_survivalcont_cohort()

  res <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, residual_diagnostics = TRUE
  )

  rt <- res$residualsTable$asDF
  expect_gt(nrow(rt), 1L)
  expect_false(any(is.na(rt$martingale)))
  expect_false(any(is.na(rt$deviance)))
  expect_null(res$residualsTable$notes$error)
  expect_gt(res$schoenfeldResidualsTable$rowCount, 0L)

  # Values come from the Cox model fitted on the CUT-OFF GROUPS, and the table
  # must say so rather than leaving the reader to guess which model.
  cut_ref <- survminer::surv_cutpoint(df, time = "time", event = "status",
                                      variables = "marker", minprop = 0.10)
  grp_model <- survival::coxph(survival::Surv(time, status) ~ marker,
                               data = survminer::surv_categorize(cut_ref))
  expect_equal(rt$martingale[seq_len(3)],
               round(unname(residuals(grp_model, "martingale"))[seq_len(3)], 4))
  expect_match(res$residualsTable$notes$model$note, "cut-off groups")
  # A 100-row display cap must be disclosed, not silent.
  expect_match(res$residualsTable$notes$truncated$note, "first 100 of 240")
})

test_that("Regression: minimum group size is enforced by every multiple cut-off method", {
  # min_group_size was only honoured by surv_cutpoint's minprop on the single
  # cut-off path. All four multiple cut-off methods could return partitions with
  # groups far below it (observed: 3, 14 and 19 patients out of 240 at 10%).
  df <- make_survivalcont_cohort()
  min_pct <- 10
  min_n <- ceiling(nrow(df) * min_pct / 100)

  for (method in c("quantile", "recursive", "tree", "minpval")) {
    for (k in c("two", "three")) {
      res <- run_survivalcont(
        data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
        multiple_cutoffs = TRUE, cutoff_method = method, num_cutoffs = k,
        min_group_size = min_pct
      )
      sizes <- res$multipleMedianTable$asDF$n_patients
      expect_gt(length(sizes), 1L)
      expect_true(all(sizes >= min_n),
                  info = sprintf("%s/%s produced groups of %s (minimum %d)",
                                 method, k, paste(sizes, collapse = ","), min_n))
      expect_equal(sum(sizes), nrow(df))
      # Cut-points must stay finite, unique and ordered after the reduction.
      cuts <- res$multipleCutTable$asDF$cutpoint_value
      expect_true(all(is.finite(cuts)))
      expect_equal(cuts, sort(unique(cuts)))
      expect_equal(length(cuts) + 1L, length(sizes))
    }
  }

  # Dropping a cut-point must be reported, not silent.
  res <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    multiple_cutoffs = TRUE, cutoff_method = "minpval", num_cutoffs = "three",
    min_group_size = min_pct
  )
  expect_match(strip_survivalcont_html(res$warnings$content),
               "minimum group size", fixed = TRUE)
})

test_that("Regression: asSource() emits each option once and produces runnable R", {
  df <- make_survivalcont_cohort()
  analysis <- run_survivalcont_jamovi(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE
  )
  src <- analysis$asSource()

  # Manual emission of elapsedtime/outcome on top of .asArgs() used to duplicate
  # them, so the snippet died with "matched by multiple actual arguments".
  expect_equal(lengths(regmatches(src, gregexpr("\\belapsedtime\\s*=", src)))[1], 1L)
  expect_equal(lengths(regmatches(src, gregexpr("\\boutcome\\s*=", src)))[1], 1L)
  expect_equal(lengths(regmatches(src, gregexpr("\\bcontexpl\\s*=", src)))[1], 1L)

  call_expr <- parse(text = src)[[1]]
  arg_names <- names(as.list(call_expr))[-1]
  expect_false(any(duplicated(arg_names[nzchar(arg_names)])))

  # eval() is the assertion here: the point of asSource() is to hand the user a
  # snippet they can paste into R, so the only way to test it is to run it. The
  # input is the string this test just generated from a fixture, not user data.
  env <- new.env(parent = globalenv())
  assign("data", df, envir = env)
  expect_no_error(eval(call_expr, envir = env))

  # Names containing quotes or backslashes must survive as valid string literals.
  df2 <- df
  names(df2)[names(df2) == "marker"] <- 'mark"er \\ x'
  src2 <- run_survivalcont_jamovi(
    data = df2, elapsedtime = "time", outcome = "status", contexpl = 'mark"er \\ x'
  )$asSource()
  expect_no_error(parse(text = src2))
  expect_true(grepl('mark\\\\"er', src2))
})

test_that("Regression: survival time points accept whitespace separators and report failures", {
  df <- make_survivalcont_cohort()

  spaced <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, cutp = "6 12 24"
  )
  expect_equal(sort(unique(spaced$survTable$asDF$time)), c(6L, 12L, 24L))

  commas <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, cutp = "6,12,24"
  )
  expect_equal(sort(unique(commas$survTable$asDF$time)), c(6L, 12L, 24L))

  # Unparseable input must fall back visibly, not silently.
  junk <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, cutp = "abc"
  )
  expect_equal(sort(unique(junk$survTable$asDF$time)), c(12L, 36L, 60L))
  expect_match(strip_survivalcont_html(junk$warnings$content),
               "could not be read as a list of time points", fixed = TRUE)
})

test_that("Regression: the short-follow-up warning follows the selected time unit", {
  df <- make_survivalcont_cohort()
  df_years <- transform(df, time = round(time / 12, 3))  # median ~2.8 years

  years <- run_survivalcont(
    data = df_years, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, timetypeoutput = "years"
  )
  warned <- strip_survivalcont_html(years$clinicalWarnings$content)
  expect_false(grepl("Short median follow-up", warned, fixed = TRUE))
  # The interpretation box must never print its own warning flag as content.
  expect_false(grepl("\\bTRUE\\b", warned))

  months <- run_survivalcont(
    data = transform(df, time = time / 20), elapsedtime = "time",
    outcome = "status", contexpl = "marker", findcut = TRUE,
    timetypeoutput = "months"
  )
  expect_match(strip_survivalcont_html(months$clinicalWarnings$content),
               "Short median follow-up", fixed = TRUE)
})

test_that("Regression: plot axis extents follow the selected time unit", {
  # endplot/byplot are Integer options, so their defaults can only be static
  # numbers (60 and 12) that are correct in months alone. On a day scale every
  # curve was clipped at 60 days; on a year scale the axis ran to 60 years.
  # The untouched factory values are now read as 5 years / 1 year in the active
  # scale, the same rule .parseSurvivalTimePoints() applies to cutp.
  df <- make_survivalcont_cohort()
  expected <- list(days = c(1825, 365), weeks = c(260, 52),
                   months = c(60, 12), years = c(5, 1))

  for (unit in names(expected)) {
    a <- run_survivalcont_jamovi(
      data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
      timetypeoutput = unit
    )
    p <- a$.__enclos_env__$private
    expect_equal(p$.plotEndTime(), expected[[unit]][1], info = unit)
    expect_equal(p$.plotBy(), expected[[unit]][2], info = unit)
  }

  # An explicit user value is never rescaled.
  p <- run_survivalcont_jamovi(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    timetypeoutput = "years", endplot = 99, byplot = 7
  )$.__enclos_env__$private
  expect_equal(p$.plotEndTime(), 99)
  expect_equal(p$.plotBy(), 7)

  # Every plot renderer survives a day-scale run at the rescaled extent.
  day_df <- transform(df, time = round(time * 30.44, 2))
  a <- run_survivalcont_jamovi(
    data = day_df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, sc = TRUE, ce = TRUE, ch = TRUE, kmunicate = TRUE,
    loglog = TRUE, timetypeoutput = "days"
  )
  a$run()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (plot_name in c("plot4", "plot5", "plot2", "plot3", "plot6", "plot7")) {
    expect_no_error(
      suppressWarnings(a$.__enclos_env__$private[[paste0(".", plot_name)]](a$results[[plot_name]], NULL, NULL))
    )
  }
})

test_that("Regression: a reduced single cut-off keeps marker-value labels", {
  # Minimum-group-size enforcement can leave one cut-off. .createRiskGroups had
  # no length-1 branch, so it fell through to the generic cut() fallback and
  # relabelled the groups "Group 1"/"Group 2".
  #
  # Exercise the branch directly rather than trying to coax a search into
  # returning one cut-off: which cut-points a method yields is a property of the
  # search, and an earlier version of this test silently depended on a bug in
  # .minPvalueCutoffs to reach the reduced case at all.
  df <- make_survivalcont_cohort()
  private <- run_survivalcont_jamovi(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker"
  )$.__enclos_env__$private

  for (cuts in list(50, c(45, 55), c(40, 50, 60), c(35, 45, 55, 65))) {
    groups <- private$.createRiskGroups(df$marker, cuts)
    expect_s3_class(groups, "factor")
    expect_equal(length(levels(groups)), length(cuts) + 1L,
                 info = paste(length(cuts), "cut-offs"))
    expect_false(any(grepl("^Group [0-9]", levels(groups))),
                 info = paste(length(cuts), "cut-offs ->", paste(levels(groups), collapse = "/")))
    expect_true(all(grepl("marker$", levels(groups))))
    # Levels must be ordered from the lowest marker values upward.
    expect_equal(as.integer(groups[which.min(df$marker)]), 1L)
    expect_equal(as.integer(groups[which.max(df$marker)]), length(levels(groups)))
  }
})

test_that("Regression: the short-follow-up notice is emitted exactly once", {
  # .run() carried a second copy that fired only for months (< 6) and years
  # (< 2), so month-scale data was warned twice in two different boxes while
  # day- and week-scale data was never warned at all.
  df <- make_survivalcont_cohort()
  res <- run_survivalcont(
    data = transform(df, time = time / 20), elapsedtime = "time",
    outcome = "status", contexpl = "marker", findcut = TRUE,
    timetypeoutput = "months"
  )
  hits <- function(x) {
    if (is.null(x)) return(0L)
    lengths(regmatches(x, gregexpr("Short (median )?[Ff]ollow", x)))[1]
  }
  expect_equal(hits(res$clinicalWarnings$content), 1L)
  expect_equal(hits(res$warnings$content), 0L)
})

test_that("Regression: result tables do not duplicate rows across re-runs", {
  # .run() is re-invoked on every option change, including options in no
  # clearWith list. Five tables never cleared their rows, so addRow() appended a
  # second copy each cycle: after three runs survTable held 18 rows, not 6.
  df <- make_survivalcont_cohort(160)
  a <- run_survivalcont_jamovi(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, person_time = TRUE
  )
  tables <- c("coxTable", "rescutTable", "medianTable", "survTable", "personTimeTable")
  a$run()
  first <- vapply(tables, function(t) as.integer(a$results[[t]]$rowCount), 1L)
  expect_true(all(first > 0))
  for (cycle in 2:3) {
    a$run()
    expect_equal(vapply(tables, function(t) as.integer(a$results[[t]]$rowCount), 1L),
                 first, info = paste("run", cycle))
  }
})

test_that("Regression: the Cox table names the predictor", {
  # The row frame's first column was named "contexpl", matching no column
  # declared in the .r.yaml (the key is "Explanatory"), so addRow() dropped it
  # and the variable-name column rendered empty on every run.
  df <- make_survivalcont_cohort()
  tbl <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE
  )$coxTable$asDF
  expect_equal(nrow(tbl), 1L)
  expect_false(is.na(tbl$Explanatory[1]))
  expect_equal(tbl$Explanatory[1], "marker")
})

test_that("Regression: misuse warnings reach the user instead of the hidden todo panel", {
  # They were written into `todo`, which .run() hides one line later whenever the
  # variable requirements are met - the only case in which they are generated.
  df <- make_survivalcont_cohort()
  res <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, multiple_cutoffs = TRUE
  )
  expect_match(strip_survivalcont_html(res$warnings$content), "overfitting", fixed = TRUE)
  expect_false(grepl("overfitting", paste(res$todo$content, collapse = ""), fixed = TRUE))
})

test_that("Regression: unusable date and stratification inputs are diagnosed, not absorbed", {
  set.seed(3)
  n <- 120
  base <- data.frame(status = rbinom(n, 1, 0.6), marker = round(rnorm(n, 50, 10), 1))
  dx <- as.Date("2020-01-01") + sample(0:200, n, TRUE)

  # Transposed dates: the message must name the cause, and a landmark must not
  # pre-empt it with "choose an earlier landmark".
  swapped <- data.frame(base, dxd = as.character(dx), fud = as.character(dx - 100))
  for (landmark in list(list(), list(uselandmark = TRUE, landmark = 1))) {
    res <- do.call(run_survivalcont, c(list(
      data = swapped, elapsedtime = NULL, outcome = "status", contexpl = "marker",
      tint = TRUE, dxdate = "dxd", fudate = "fud", timetypedata = "ymd"
    ), landmark))
    expect_match(strip_survivalcont_html(res$errors$content),
                 "negative follow-up time", fixed = TRUE)
  }

  # Numeric day-count dates are 86400x smaller than the epoch seconds the code
  # assumes; they used to yield an empty analysis with no explanation.
  daycounts <- data.frame(base, dxd = as.numeric(dx), fud = as.numeric(dx + 400))
  res <- run_survivalcont(
    data = daycounts, elapsedtime = NULL, outcome = "status", contexpl = "marker",
    tint = TRUE, dxdate = "dxd", fudate = "fud"
  )
  expect_match(strip_survivalcont_html(res$errors$content),
               "not epoch seconds", fixed = TRUE)

  # A near-continuous stratification variable gave one stratum per patient and
  # was tabulated as a valid HR 1.00 (0, Inf), p = 1.00.
  continuous_strata <- data.frame(
    base, time = round(rexp(n, 0.02) + 1, 2), site = factor(seq_len(n))
  )
  res <- run_survivalcont(
    data = continuous_strata, elapsedtime = "time", outcome = "status",
    contexpl = "marker", stratified_cox = TRUE, strata_variable = "site"
  )
  expect_equal(res$stratifiedCoxTable$rowCount, 0L)
  expect_match(res$stratifiedCoxTable$notes$toomany$note, "Not fitted", fixed = TRUE)
})

test_that("Regression: labels and narratives stay consistent with the tables beside them", {
  df <- make_survivalcont_cohort(160)

  # The RMST narrative must not survive a cut-off pass that produced no table.
  # tau between the smaller group's support and the cohort maximum reaches this.
  cut_ref <- survminer::surv_cutpoint(df, time = "time", event = "status",
                                      variables = "marker", minprop = 0.10)
  cat_df <- survminer::surv_categorize(cut_ref)
  tau <- (min(tapply(cat_df$time, cat_df$marker, max)) + max(df$time)) / 2
  res <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, rmst_analysis = TRUE, showSummaries = TRUE, rmst_tau = tau
  )
  expect_equal(res$rmstTable$rowCount, 0L)
  expect_equal(nchar(strip_survivalcont_html(res$rmstSummary$content)), 0L)

  # The final person-time interval is open-ended; its end used to be printed as
  # max(time) * 1.1, advertising follow-up that does not exist.
  res <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    person_time = TRUE, time_intervals = "12, 36, 60"
  )
  intervals <- res$personTimeTable$asDF$interval
  expect_true(any(grepl("\\+$", intervals)))
  expect_false(any(grepl(as.character(round(max(df$time) * 1.1, 1)), intervals, fixed = TRUE)))

  # "Groups Created" used the "Group 2"/"Group 3" vocabulary that appears nowhere
  # else on screen.
  res <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    multiple_cutoffs = TRUE, num_cutoffs = "two"
  )
  created <- res$multipleCutTable$asDF$group_created
  expect_true(all(created %in% res$multipleMedianTable$asDF$risk_group))

  # The stratified Cox term used the janitor-cleaned column name.
  labelled_df <- df
  names(labelled_df)[names(labelled_df) == "marker"] <- "Ki-67 index"
  labelled_df[["Tumour Grade"]] <- factor(rep_len(c("I", "II", "III"), nrow(df)))
  res <- run_survivalcont(
    data = labelled_df, elapsedtime = "time", outcome = "status",
    contexpl = "Ki-67 index", stratified_cox = TRUE, strata_variable = "Tumour Grade"
  )
  expect_equal(res$stratifiedCoxTable$asDF$term[1], "Ki-67 index")
})

test_that("Regression: a low-cardinality numeric predictor stays continuous in the Cox model", {
  # finalfit's default cont_cut = 5 mutates a numeric explanatory variable with
  # fewer than 5 distinct values into a FACTOR before fitting, so a 4-level
  # integer score was reported as four per-level hazard ratios under a footnote
  # promising "a one-unit increase" -- and every other path in the analysis kept
  # using the untouched numeric column.
  set.seed(5)
  n <- 60
  df <- data.frame(
    time = round(rexp(n, 0.05) + 1, 2),
    status = rbinom(n, 1, 0.6),
    score = sample(0:3, n, TRUE)
  )
  ref <- summary(survival::coxph(survival::Surv(time, status) ~ score, data = df))
  tbl <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "score"
  )$coxTable$asDF

  expect_equal(nrow(tbl), 1L)
  expect_equal(leading_number(tbl$HR_univariable[1]),
               round(unname(ref$conf.int[1, "exp(coef)"]), 2), tolerance = 5e-3)
})

test_that("Regression: survival is not reported beyond the observed follow-up", {
  # extend = TRUE carried the last KM value and its CI forward and relabelled
  # them with the requested time, producing a 5-year survival with a confidence
  # interval from 2.5 years of data - while the multiple-cut-off table printed NA
  # for the same groups, so one result object held two contradictory statements.
  df <- make_survivalcont_cohort()
  capped <- transform(df, time = pmin(time, 30))
  res <- run_survivalcont(
    data = capped, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, cutp = "12, 36, 60", timetypeoutput = "months"
  )
  reported <- unique(res$survTable$asDF$time)
  expect_true(all(reported <= 30))
  expect_false(any(c(36, 60) %in% reported))
  expect_match(res$survTable$notes$horizon$note, "exceed the longest observed follow-up",
               fixed = TRUE)
})

test_that("Regression: tree cut-points keep the strongest splits, not the smallest values", {
  # rpart's `improve` column was never read; sorting cut-points by VALUE and
  # taking the first num_cuts discarded the root split, biasing every
  # tree-derived grouping toward low marker values.
  skip_if_not_installed("rpart")
  set.seed(3)
  m <- 800
  marker <- runif(m, 0, 40)
  hazard <- 0.01 * ifelse(marker > 31, 6, ifelse(marker > 10, 2, ifelse(marker > 5, 1.5, 1)))
  df <- data.frame(
    time = round(rexp(m, hazard) + 1, 2),
    status = rbinom(m, 1, 0.75),
    marker = round(marker, 2)
  )
  cuts <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    multiple_cutoffs = TRUE, cutoff_method = "tree", num_cutoffs = "two",
    min_group_size = 5
  )$multipleCutTable$asDF$cutpoint_value

  # The dominant hazard step is at 31; it must not be truncated away.
  expect_true(any(abs(cuts - 31) < 3),
              info = paste("cut-points:", paste(cuts, collapse = ", ")))
})

test_that("Regression: a date-format mismatch is named rather than blamed on missing data", {
  # lubridate reports a mismatch as a WARNING, so the tryCatch never fired and
  # the failed rows were removed by naOmit and reported as "excluded because
  # ... was missing" - pointing the user at their data instead of the setting.
  set.seed(1)
  k <- 30
  dx <- as.Date("2020-01-01") + sample(0:200, k, TRUE)
  df <- data.frame(
    status = rbinom(k, 1, 0.6), marker = round(rnorm(k, 50, 10), 1),
    dxd = format(dx, "%d/%m/%Y"), fud = format(dx + 300, "%d/%m/%Y")
  )
  res <- run_survivalcont(
    data = df, elapsedtime = NULL, outcome = "status", contexpl = "marker",
    tint = TRUE, dxdate = "dxd", fudate = "fud", timetypedata = "mdy"
  )
  combined <- paste(strip_survivalcont_html(res$errors$content),
                    strip_survivalcont_html(res$strongWarnings$content))
  expect_match(combined, "did not match the selected input time type", fixed = TRUE)
  expect_match(combined, "format mismatch rather than missing data", fixed = TRUE)
})

test_that("Regression: person-time intervals accept whitespace separators", {
  # "[,\\s]+" is comma, backslash or the letter "s" under TRE - not whitespace.
  df <- make_survivalcont_cohort()
  intervals <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    person_time = TRUE, time_intervals = "12 36 60"
  )$personTimeTable$asDF$interval
  expect_true(all(c("0-12", "12-36", "36-60") %in% intervals))
})

test_that("Regression: a landmark discloses its exclusions and its time origin", {
  df <- make_survivalcont_cohort()
  res <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE, uselandmark = TRUE, landmark = 20, timetypeoutput = "months"
  )
  excluded <- sum(df$time < 20)
  notices <- paste(strip_survivalcont_html(res$infoMessages$content),
                   strip_survivalcont_html(res$strongWarnings$content))
  expect_match(notices, "Landmark exclusions", fixed = TRUE)
  expect_match(notices, sprintf("%d of %d patient", excluded, nrow(df)), fixed = TRUE)
  # Every table printing a time on the shifted scale must say so.
  for (tbl in c("medianTable", "survTable"))
    expect_match(res[[tbl]]$notes$landmark$note, "measured from the landmark", fixed = TRUE)
})

test_that("Regression: the completion notice reports what ran, not what was ticked", {
  df <- make_survivalcont_cohort()

  # Fewer than 10 events suppresses cut-off determination; build that explicitly
  # rather than trusting a row slice to land below the threshold.
  low_event <- df[c(which(df$status == 1)[1:5], which(df$status == 0)[1:25]), ]
  expect_lt(sum(low_event$status), 10L)
  suppressed <- run_survivalcont(
    data = low_event, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE
  )
  expect_equal(suppressed$rescutTable$rowCount, 0L)
  expect_false(grepl("cut-off analysis",
                     strip_survivalcont_html(suppressed$infoMessages$content), fixed = TRUE))

  completed <- run_survivalcont(
    data = df, elapsedtime = "time", outcome = "status", contexpl = "marker",
    findcut = TRUE
  )
  expect_match(strip_survivalcont_html(completed$infoMessages$content),
               "cut-off analysis", fixed = TRUE)
})

test_that("Regression: every clearWith entry is a real option name", {
  # A YAML list entry indented deeper than its siblings is folded into the
  # previous scalar ("awod - timetypeoutput - timetypedata"), which parses
  # cleanly but silently destroys the preceding trigger as well.
  skip_if_not_installed("yaml")
  # testthat runs with the working directory set to tests/testthat, so the spec
  # directory has to be resolved relative to the package root.
  candidates <- c(testthat::test_path("..", "..", "jamovi"), "jamovi")
  spec_dir <- Filter(function(d) file.exists(file.path(d, "survivalcont.a.yaml")), candidates)
  skip_if(length(spec_dir) == 0, "jamovi spec directory not available (installed package)")
  spec_dir <- spec_dir[1]

  options_spec <- yaml::yaml.load_file(file.path(spec_dir, "survivalcont.a.yaml"))
  declared <- vapply(options_spec$options, function(o) o$name, character(1))
  results_spec <- yaml::yaml.load_file(file.path(spec_dir, "survivalcont.r.yaml"))

  collect <- function(items) {
    for (item in items) {
      for (entry in (item$clearWith %||% character(0))) {
        expect_true(is.character(entry) && length(entry) == 1L,
                    info = paste(item$name, "clearWith entry is not a scalar"))
        expect_true(entry %in% declared,
                    info = paste0(item$name, ": clearWith entry '", entry,
                                  "' is not a declared option"))
      }
      for (key in c("items", "children"))
        if (is.list(item[[key]])) collect(item[[key]])
    }
  }
  `%||%` <- function(a, b) if (is.null(a)) b else a
  collect(results_spec$items)
})

test_that("Edge cases: small sample size and single/multiple cut-offs handled safely", {
  small_df <- data.frame(
    time = c(12, 24, 36, 48, 60, 72, 84, 96, 108, 120),
    status = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0),
    marker = c(10.5, 12.1, 15.3, 18.0, 22.4, 25.1, 30.2, 35.6, 40.0, 45.2)
  )

  expect_no_error({
    res <- suppressWarnings(run_survivalcont(
      data = small_df, elapsedtime = "time", outcome = "status",
      contexpl = "marker", findcut = TRUE
    ))
  })
  # Fewer than 10 events: cut-off determination must stay suppressed and said so.
  expect_equal(res$rescutTable$rowCount, 0L)
  expect_match(strip_survivalcont_html(res$warnings$content),
               "Cut-off determination is suppressed", fixed = TRUE)
})
