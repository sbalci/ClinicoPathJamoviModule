.msrr_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
  if (.p %in% loadedNamespaces()) {
    .candidate <- asNamespace(.p)
    if (exists("multisurvival", envir = .candidate, inherits = FALSE)) {
      .msrr_ns <- .candidate
      break
    }
  }
}
if (is.null(.msrr_ns)) {
  .r_dir <- NULL
  for (.d in c("R", "../R", "../../R")) {
    if (file.exists(file.path(.d, "multisurvival.b.R"))) {
      .r_dir <- .d
      break
    }
  }
  if (!is.null(.r_dir)) {
    .msrr_ns <- new.env(parent = globalenv())
    .msrr_ns$. <- function(x, ...) x
    for (.f in c("00jmv.R", "utils.R", "utils-Notices.R", "utils-formula.R",
                 "utils-eventindicator.R", "utils-followup.R", "survival-competingrisks.R",
                 "multisurvival-formula.R", "multisurvival-interactions.R",
                 "multisurvival-nomogram.R", "multisurvival.h.R", "multisurvival.b.R")) {
      .fp <- file.path(.r_dir, .f)
      if (file.exists(.fp)) sys.source(.fp, envir = .msrr_ns)
    }
  }
}
if (is.null(.msrr_ns)) {
  for (.p in c("ClinicoPath", "jsurvival")) {
    if (requireNamespace(.p, quietly = TRUE)) {
      .candidate <- asNamespace(.p)
      if (exists("multisurvival", envir = .candidate, inherits = FALSE)) {
        .msrr_ns <- .candidate
        break
      }
    }
  }
}
skip_if(is.null(.msrr_ns), "multisurvival not available in this distribution")

.msrr_quiet <- function(expr) {
  output <- tempfile()
  sink(output)
  on.exit(sink(), add = TRUE)
  suppressWarnings(force(expr))
}

.msrr_run <- function(data, ...) {
  args <- utils::modifyList(
    list(
      data = data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = NULL,
      dod = NULL,
      dooc = NULL,
      awd = NULL,
      awod = NULL
    ),
    list(...)
  )
  .msrr_quiet(do.call(get("multisurvival", envir = .msrr_ns), args))
}

.msrr_pct <- function(x) as.numeric(sub("%", "", x, fixed = TRUE)) / 100

test_that("small nonzero event counts warn but do not block Cox estimation", {
  set.seed(401)
  data <- data.frame(
    time = seq_len(50),
    status = c(rep(1, 5), rep(0, 45)),
    group = factor(rep(c("A", "B"), each = 25))
  )

  result <- .msrr_run(data, explanatory = "group")

  expect_s3_class(result, "multisurvivalResults")
  expect_match(as.character(result$strongWarnings$content), "Low event count")
  expect_match(as.character(result$text$content), "HR (multivariable)", fixed = TRUE)
  expect_false(grepl("No events observed", as.character(result$errors$content), fixed = TRUE))
})

test_that("zero events are refused with an actionable result message", {
  data <- data.frame(
    time = seq_len(30),
    status = rep(0, 30),
    group = factor(rep(c("A", "B"), each = 15))
  )

  result <- .msrr_run(data, explanatory = "group")

  expect_match(as.character(result$errors$content), "No events observed")
  expect_false(grepl("HR (multivariable)", as.character(result$text$content), fixed = TRUE))
})

test_that("Fine-Gray coefficients use the robust reference fit", {
  set.seed(202)
  n <- 240
  group <- factor(sample(c("A", "B"), n, TRUE))
  age <- stats::rnorm(n, 60, 8)
  lp <- 0.7 * (group == "B") + 0.02 * (age - 60)
  event_time <- stats::rexp(n, 0.04 * exp(lp))
  competing_time <- stats::rexp(n, 0.03 * exp(-0.5 * (group == "B")))
  censor_time <- stats::rexp(n, 0.02)
  time <- pmin(event_time, competing_time, censor_time)
  raw_status <- ifelse(
    event_time <= competing_time & event_time <= censor_time,
    "DOD",
    ifelse(competing_time <= censor_time, "DOOC", "AWOD")
  )
  data <- data.frame(
    time = time,
    status = factor(raw_status, levels = c("DOD", "DOOC", "AWOD")),
    group = group,
    age = age
  )

  result <- .msrr_run(
    data,
    explanatory = "group",
    contexpl = "age",
    multievent = TRUE,
    analysistype = "compete",
    dod = "DOD",
    dooc = "DOOC",
    awod = "AWOD",
    ac = TRUE,
    adjexplanatory = "group",
    ac_summary = TRUE,
    cutp = "5, 10"
  )

  ref_data <- data.frame(
    mytime = time,
    myoutcome = factor(
      ifelse(raw_status == "DOD", "Event",
             ifelse(raw_status == "DOOC", "Competing", "Censored")),
      levels = c("Censored", "Event", "Competing")
    ),
    group = group,
    age = age,
    fgid = seq_len(n)
  )
  expanded <- survival::finegray(
    survival::Surv(mytime, myoutcome) ~ group + age + fgid,
    data = ref_data,
    etype = "Event",
    id = fgid
  )
  reference <- survival::coxph(
    survival::Surv(fgstart, fgstop, fgstatus) ~ group + age,
    data = expanded,
    weights = fgwt,
    cluster = fgid,
    x = TRUE,
    y = TRUE,
    model = TRUE
  )
  sm <- summary(reference)$coefficients
  robust_se <- sm[, "robust se"]
  z <- stats::qnorm(0.975)
  expected_effect <- sprintf(
    "%.2f (%.2f-%.2f)",
    exp(sm[, "coef"]),
    exp(sm[, "coef"] - z * robust_se),
    exp(sm[, "coef"] + z * robust_se)
  )

  reported <- as.data.frame(result$adjustedCoxTable)
  expect_equal(reported$HR, unname(expected_effect))
  expect_equal(reported$Pvalue, unname(sm[, "Pr(>|z|)"]), tolerance = 1e-12)
  expect_match(as.character(result$text$content), "sHR (95% CI)", fixed = TRUE)
  expect_match(as.character(result$text$content), expected_effect[[1]], fixed = TRUE)
  expect_match(as.character(result$text$content), "Fine-Gray subdistribution hazards analysis")
  expect_match(as.character(result$adjustedCoxText$content), "Concordance is not reported")
  expect_match(as.character(result$adjustedCoxText$content),
               "correlated pseudo-rows")
  expect_false(grepl("Likelihood ratio test =",
                     as.character(result$adjustedCoxText$content), fixed = TRUE))

  # The numeric adjusted table and the plotted data use the same cumulative-
  # incidence estimand, with display rounding to 0.1 percentage point.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(.msrr_quiet(result$plot_adj$.render()))
  plotted <- ggplot2::last_plot()$data
  table <- as.data.frame(result$adjustedSurvTable)
  for (i in seq_len(nrow(table))) {
    curve <- plotted[
      plotted$group == table$strata[i] & plotted$time <= table$time[i],
      , drop = FALSE
    ]
    if (nrow(curve) == 0) next
    expect_lte(
      abs(curve$cif[nrow(curve)] - .msrr_pct(table$surv[i])),
      5.1e-4
    )
  }
})

test_that("invalid person-time cutpoints are ignored without negative exposure", {
  set.seed(611)
  data <- data.frame(
    time = stats::runif(80, 1, 40),
    status = stats::rbinom(80, 1, 0.45),
    group = factor(sample(c("A", "B"), 80, TRUE))
  )

  result <- .msrr_run(
    data,
    explanatory = "group",
    person_time = TRUE,
    time_intervals = "-2, invalid, 10, 999"
  )
  table <- as.data.frame(result$personTimeTable)

  expect_gt(nrow(table), 0)
  expect_true(all(table$person_time >= 0))
  expect_match(as.character(result$warnings$content),
               "Invalid person-time cutpoints ignored")
  expect_false(any(grepl("-2|999", table$interval)))

  overall <- table[table$interval == "Overall (0-max)", , drop = FALSE]
  events <- sum(data$status)
  exposure <- sum(data$time)
  expect_equal(overall$events, events)
  expect_equal(overall$person_time, round(exposure, 2))
  expect_equal(overall$rate, round(events / exposure * 100, 2))
  expect_equal(
    overall$rate_ci_lower,
    round((stats::qchisq(0.025, 2 * events) / 2) / exposure * 100, 2)
  )
  expect_equal(
    overall$rate_ci_upper,
    round((stats::qchisq(0.975, 2 * (events + 1)) / 2) / exposure * 100, 2)
  )

  interval_rows <- table[grepl("^[0-9.]+-[0-9.]+$", table$interval), , drop = FALSE]
  expect_equal(sum(interval_rows$person_time), exposure, tolerance = 0.02)
})

test_that("an adjusted curve cannot vary a variable absent from the model", {
  set.seed(712)
  data <- data.frame(
    time = stats::rexp(100, 0.08) + 0.1,
    status = stats::rbinom(100, 1, 0.7),
    model_group = factor(sample(c("A", "B"), 100, TRUE)),
    unused_group = factor(sample(c("X", "Y"), 100, TRUE))
  )

  result <- .msrr_run(
    data,
    explanatory = "model_group",
    ac = TRUE,
    adjexplanatory = "unused_group",
    ac_summary = TRUE
  )

  expect_match(as.character(result$warnings$content),
               "Adjustment variable is not in the model")
  expect_equal(nrow(as.data.frame(result$adjustedSurvTable)), 0)
})

test_that("median follow-up in competing-risk mode censors competing events (reverse KM)", {
  set.seed(913)
  n <- 200
  time <- round(stats::rexp(n, 0.05) + 0.5, 1)
  status <- sample(c("DOD", "DOOC", "AWOD"), n, TRUE, prob = c(0.3, 0.3, 0.4))
  data <- data.frame(time = time, status = factor(status), group = factor(sample(c("A", "B"), n, TRUE)))

  result <- .msrr_run(data, explanatory = "group", multievent = TRUE, analysistype = "compete",
                      dod = "DOD", dooc = "DOOC", awod = "AWOD")

  # Only genuinely censored (alive) patients are "events" of the reverse KM;
  # competing deaths are terminal and must not shorten the reported follow-up.
  ref_fit <- survival::survfit(survival::Surv(time, status == "AWOD") ~ 1)
  ref_median <- unname(summary(ref_fit)$table[["median"]])
  wrong_fit <- survival::survfit(survival::Surv(time, status != "DOD") ~ 1)
  wrong_median <- unname(summary(wrong_fit)$table[["median"]])
  expect_false(isTRUE(all.equal(ref_median, wrong_median)))

  info <- as.character(result$infoMessages$content)
  expect_match(info, sprintf("median follow-up of %.1f months \\(reverse Kaplan-Meier\\)", ref_median))
})

test_that("a plot horizon shorter than follow-up is disclosed", {
  set.seed(914)
  data <- data.frame(
    time = c(stats::runif(90, 1, 50), stats::runif(10, 61, 120)),
    status = stats::rbinom(100, 1, 0.6),
    group = factor(sample(c("A", "B"), 100, TRUE))
  )

  truncated <- .msrr_run(data, explanatory = "group", km = TRUE, endplot = 60)
  expect_match(as.character(truncated$infoMessages$content), "Plot horizon shorter than follow-up")

  full <- .msrr_run(data, explanatory = "group", km = TRUE, endplot = 200)
  expect_false(grepl("Plot horizon shorter than follow-up", as.character(full$infoMessages$content), fixed = TRUE))

  no_plot <- .msrr_run(data, explanatory = "group", km = FALSE, ac = FALSE, endplot = 60)
  expect_false(grepl("Plot horizon shorter than follow-up", as.character(no_plot$infoMessages$content), fixed = TRUE))
})

test_that("IR-01..IR-07 regressions: exclusion notes, HTML escaping, stratified summaries/scores, singular zph, person-time multi-explanatory, competing-risk riskGroupPlot options, and fractional time options", {
  set.seed(20261022)
  n <- 120
  df <- data.frame(
    time = round(stats::runif(n, 0.5, 10), 2),
    status = stats::rbinom(n, 1, 0.5),
    sex = factor(sample(c("Male", "Female"), n, TRUE)),
    rx = factor(sample(c("Control", "Experimental<img src=x onerror=alert(1)>"), n, TRUE)),
    age = round(stats::rnorm(n, 60, 10), 1)
  )

  # IR-01a & IR-07: Fractional landmark (2.5) on complete data does not claim missing time/covariate exclusions
  res_lm <- .msrr_run(
    df,
    explanatory = c("sex", "rx"),
    contexpl = "age",
    uselandmark = TRUE,
    landmark = 2.5,
    endplot = 8.5,
    byplot = 0.5
  )
  expect_false(grepl(
    "excluded from the model because the follow-up time or at least one selected covariate was missing",
    as.character(res_lm$infoMessages$content),
    fixed = TRUE
  ))

  # IR-01c: Missing outcome rows are not misattributed to missing time/covariates
  df_out_na <- df
  df_out_na$status[1:5] <- NA_integer_
  res_out_na <- .msrr_run(df_out_na, explanatory = "sex", contexpl = "age")
  expect_false(grepl(
    "A further 5 row(s)",
    as.character(res_out_na$infoMessages$content),
    fixed = TRUE
  ))

  # IR-02: HTML tags in factor levels are escaped in Clinical Summary
  res_xss <- .msrr_run(df, explanatory = c("sex", "rx"), contexpl = "age", showSummaries = TRUE)
  expect_false(grepl("<img src=x onerror=alert(1)>", as.character(res_xss$text$content), fixed = TRUE))

  # IR-03a & IR-03b: Stratified variable is not counted as a multivariable risk factor, and risk scores use sample centering
  res_strat <- .msrr_run(
    df,
    explanatory = c("sex", "rx"),
    contexpl = "age",
    use_stratify = TRUE,
    stratvar = "sex",
    calculateRiskScore = TRUE,
    showSummaries = TRUE
  )
  expect_false(grepl("examined 3 potential risk factors", as.character(res_strat$multivariableCoxSummary$content), fixed = TRUE))
  fit_strat <- survival::coxph(survival::Surv(time, status) ~ rx + age + strata(sex), data = df)
  rk_sample <- stats::predict(fit_strat, type = "risk", reference = "sample")
  q_strat <- stats::quantile(rk_sample, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  grp_strat <- cut(rk_sample, breaks = q_strat, labels = c("Low Risk", "Intermediate-Low Risk", "Intermediate-High Risk", "High Risk"), include.lowest = TRUE)
  expect_equal(
    as.data.frame(res_strat$riskScoreTable)$median_score[1],
    round(stats::median(rk_sample[grp_strat == "Low Risk"]), 3)
  )

  # IR-04: Singular cox.zph in .adjustedCox() does not crash the analysis
  df_tie <- df
  df_tie$time <- ifelse(df_tie$status == 1, 5, 10)
  res_tie <- .msrr_run(
    df_tie,
    explanatory = c("sex", "rx"),
    contexpl = "age",
    ac = TRUE,
    adjexplanatory = "sex",
    ac_summary = TRUE,
    ph_cox = TRUE
  )
  expect_match(as.character(res_tie$adjustedCoxPH$content), "Proportional hazards test could not be computed")

  # IR-05: personTimeTable stratifies all categorical explanatory variables
  res_pt <- .msrr_run(df, explanatory = c("sex", "rx"), person_time = TRUE)
  pt_df <- as.data.frame(res_pt$personTimeTable)
  expect_true(any(grepl("^sex:", pt_df$interval)))
  expect_true(any(grepl("^rx:", pt_df$interval)))

  # IR-01b & IR-06: Competing risks reconciles missing covariate rows and riskGroupPlot honours endplot/ci95
  df_cr <- df
  df_cr$status_cr <- factor(sample(c("DOD", "DOOC", "AWOD"), n, TRUE, prob = c(0.4, 0.2, 0.4)))
  df_cr$age[1:6] <- NA_real_
  res_cr <- .msrr_run(
    df_cr,
    outcome = "status_cr",
    multievent = TRUE,
    analysistype = "compete",
    dod = "DOD",
    dooc = "DOOC",
    awod = "AWOD",
    explanatory = "sex",
    contexpl = "age",
    calculateRiskScore = TRUE,
    plotRiskGroups = TRUE,
    ci95 = TRUE,
    endplot = 6.5,
    byplot = 1.5
  )
  expect_match(as.character(res_cr$infoMessages$content), "A further 6 row\\(s\\)")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(.msrr_quiet(res_cr$riskGroupPlot$.render()))
  p_cr <- ggplot2::last_plot()
  expect_lte(max(p_cr$data$time, na.rm = TRUE), 6.5)
  geom_classes <- vapply(p_cr$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% geom_classes)

  # Welcome screen (no explanatory variables) keeps eventRecodeInfo empty
  res_todo <- .msrr_run(df, explanatory = NULL, contexpl = NULL)
  expect_equal(as.character(res_todo$eventRecodeInfo$content), "<div></div>")
})
