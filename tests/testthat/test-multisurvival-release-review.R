.msrr_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
  if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
    .candidate <- asNamespace(.p)
    if (exists("multisurvival", envir = .candidate, inherits = FALSE)) {
      .msrr_ns <- .candidate
      break
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
