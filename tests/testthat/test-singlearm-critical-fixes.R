# Numerical reference tests for the single-arm survival analysis.
#
# These tests exercise the shipped analysis. They deliberately avoid mock
# implementations of the estimators, because a reimplementation can agree with
# itself while the module remains wrong.

run_singlearm_reference <- function(...) {
  args <- list(...)
  for (nm in c("outcomeLevel", "dod", "dooc", "awd", "awod")) {
    if (is.null(args[[nm]]))
      args[nm] <- list(NULL)
  }
  do.call(singlearm, args)
}

table_number <- function(table, row, column) {
  as.numeric(table$getCell(rowNo = row, column)$value)
}

reference_data <- function() {
  data.frame(
    time = c(1, 2, 3, 4, 5, 6, 7, 8),
    status = factor(
      c("Alive", "Dead", "Alive", "Dead", "Dead", "Alive", "Dead", "Alive"),
      levels = c("Alive", "Dead")
    )
  )
}

test_that("Kaplan-Meier median and time-specific estimates match survival", {
  skip_if_not_installed("survival")
  d <- reference_data()
  result <- run_singlearm_reference(
    data = d,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "Dead",
    cutp = "2, 4, 6"
  )

  reference_fit <- survival::survfit(
    survival::Surv(time, status == "Dead") ~ 1,
    data = d
  )
  reference_summary <- summary(reference_fit, times = c(2, 4, 6), extend = TRUE)

  expect_equal(
    table_number(result$medianTable, 1, "median"),
    unname(summary(reference_fit)$table[["median"]])
  )
  expect_equal(
    vapply(seq_len(result$survTable$rowCount), function(i) {
      table_number(result$survTable, i, "surv")
    }, numeric(1)),
    unname(reference_summary$surv),
    tolerance = 1e-12
  )
  expect_equal(
    vapply(seq_len(result$survTable$rowCount), function(i) {
      table_number(result$survTable, i, "n.risk")
    }, numeric(1)),
    unname(reference_summary$n.risk)
  )
})

test_that("restricted mean equals the area under the Kaplan-Meier curve", {
  skip_if_not_installed("survival")
  d <- reference_data()
  result <- run_singlearm_reference(
    data = d,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "Dead"
  )

  fit <- survival::survfit(
    survival::Surv(time, status == "Dead") ~ 1,
    data = d
  )
  # On [0, max(time)], the Kaplan-Meier curve is right-continuous and constant
  # between observed times. Integrate those rectangles independently of the
  # summary.survfit rmean field used by the module.
  tau <- max(d$time)
  fit_times <- fit$time[fit$time <= tau]
  survival_before <- c(1, utils::head(fit$surv, -1))
  reference_area <- sum(diff(c(0, fit_times)) * survival_before)

  expect_equal(
    table_number(result$medianTable, 1, "rmean"),
    reference_area,
    tolerance = 1e-12
  )
})

test_that("person-time rate and Garwood limits use target events over exposure", {
  d <- reference_data()
  result <- run_singlearm_reference(
    data = d,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "Dead",
    person_time = TRUE,
    time_intervals = "3, 6",
    rate_multiplier = 100
  )

  events <- sum(d$status == "Dead")
  exposure <- sum(d$time)
  expected <- c(
    rate = events / exposure * 100,
    lower = stats::qchisq(0.025, 2 * events) / 2 / exposure * 100,
    upper = stats::qchisq(0.975, 2 * (events + 1)) / 2 / exposure * 100
  )

  expect_equal(table_number(result$personTimeTable, 1, "events"), events)
  expect_equal(table_number(result$personTimeTable, 1, "person_time"), exposure)
  expect_equal(
    c(
      rate = table_number(result$personTimeTable, 1, "rate"),
      lower = table_number(result$personTimeTable, 1, "rate_ci_lower"),
      upper = table_number(result$personTimeTable, 1, "rate_ci_upper")
    ),
    round(expected, 2)
  )
})

test_that("competing-risk table matches the Aalen-Johansen state probability", {
  skip_if_not_installed("survival")
  skip_if_not_installed("cmprsk")
  d <- data.frame(
    time = c(1, 2, 3, 4, 5, 6, 7, 8),
    outcome = factor(
      c("DOD", "DOOC", "AWD", "DOD", "AWOD", "DOOC", "DOD", "AWOD"),
      levels = c("DOD", "DOOC", "AWD", "AWOD")
    )
  )
  result <- run_singlearm_reference(
    data = d,
    elapsedtime = "time",
    outcome = "outcome",
    multievent = TRUE,
    analysistype = "compete",
    dod = "DOD",
    dooc = "DOOC",
    awd = "AWD",
    awod = "AWOD",
    cutp = "2, 4, 6",
    person_time = TRUE
  )

  status <- factor(
    ifelse(d$outcome == "DOD", "event",
           ifelse(d$outcome == "DOOC", "competing", "censored")),
    levels = c("censored", "event", "competing")
  )
  fit <- survival::survfit(survival::Surv(d$time, status) ~ 1)
  reference <- summary(fit, times = c(2, 4, 6), extend = TRUE)
  event_column <- match("event", reference$states)

  observed_cif <- vapply(seq_len(result$survTable$rowCount), function(i) {
    table_number(result$survTable, i, "surv")
  }, numeric(1))
  expect_equal(observed_cif, reference$pstate[, event_column], tolerance = 1e-12)
  expect_equal(table_number(result$medianTable, 1, "events"), sum(d$outcome == "DOD"))
  expect_equal(table_number(result$personTimeTable, 1, "events"), sum(d$outcome == "DOD"))
})

test_that("piecewise hazard rows partition events and person-time exactly", {
  namespace <- environment(singlearm)
  generator <- get("singlearmClass", envir = namespace)
  intervals <- generator$private_methods$.hazardIntervals(
    time = c(1, 2, 3, 4, 5, 6),
    status = c(1L, 0L, 1L, 0L, 1L, 0L),
    target_events = 1L,
    max_bins = 3L
  )

  expect_equal(sum(intervals$events), 3)
  expect_equal(sum(intervals$person_time), sum(c(1, 2, 3, 4, 5, 6)))
  expect_equal(
    intervals$rate,
    intervals$events / intervals$person_time,
    tolerance = 1e-12
  )
  expect_true(all(intervals$lower <= intervals$rate))
  expect_true(all(intervals$upper >= intervals$rate))
})
