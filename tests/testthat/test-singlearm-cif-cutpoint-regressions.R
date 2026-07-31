# Regressions in singlearm's competing-risk and cutpoint handling.
#
# B1  competing-risk table reported 0% for the dominant event
# B2  unsorted cutpoints corrupted the events column and the narrative
# M1  .supportedCutpoints deleted valid zero-survival rows
# M2  .resolveCutpoints overwrote what the user typed
#
# The cutpoint helpers are exercised as the REAL functions taken off the R6
# generator, so the test fails if the source changes -- it does not re-implement
# them. singlearmClass sources standalone because R6 evaluates `inherit` lazily.

library(testthat)

.singlearm_src <- function() {
  for (p in c("../../R/singlearm.b.R", "../R/singlearm.b.R", "R/singlearm.b.R"))
    if (file.exists(p)) return(p)
  NULL
}

.singlearm_stub <- function(timetypeoutput = "months") {
  src <- .singlearm_src()
  skip_if(is.null(src), "R/singlearm.b.R not available (installed-package check)")

  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(src, envir = e)))
  pm <- e$singlearmClass$private_methods

  log <- new.env(parent = emptyenv())
  log$msgs <- character()
  add <- function(kind) function(m) log$msgs <- c(log$msgs, paste0(kind, ": ", m))

  stub <- new.env(parent = globalenv())
  bind <- function(f) { environment(f) <- stub; f }
  stub$self <- list(options = list(timetypeoutput = timetypeoutput))
  stub$private <- list(
    .addInfo    = add("INFO"),
    .addWarning = add("WARNING"),
    .addError   = add("ERROR"),
    .isCompetingRisk = function(...) FALSE,
    .yearInUnits         = bind(pm$.yearInUnits),
    .getDefaultCutpoints = bind(pm$.getDefaultCutpoints),
    .parseNumericList    = bind(pm$.parseNumericList),
    .resolveCutpoints    = bind(pm$.resolveCutpoints),
    .supportedCutpoints  = bind(pm$.supportedCutpoints),
    .ciText              = bind(pm$.ciText)
  )
  stub$log <- log
  stub
}


# B1 -- competing risk -------------------------------------------------------

test_that("B1: numeric mstate status misreads the event of interest (the bug)", {
  skip_if_not_installed("survival")
  # 169 events of interest, 31 competing, ZERO censored.
  d <- data.frame(time = seq(1, 60, length.out = 200),
                  status = c(rep(1L, 169), rep(2L, 31)))

  fit_num <- suppressWarnings(
    survival::survfit(survival::Surv(time, status, type = "mstate") ~ 1, data = d))

  # With no censored subject the LOWEST OBSERVED code (1) becomes censoring:
  # state "1" does not exist, so the old match("1", states) returned NA and the
  # zero-fill branch reported 0% for an event seen in 169 of 200 subjects.
  expect_false("1" %in% fit_num$states)
  expect_true(is.na(match("1", fit_num$states)))
})

test_that("M1: competing-risk extrapolation does not claim the final CIF is zero", {
  s <- .singlearm_stub("months")
  s$private$.isCompetingRisk <- function(...) TRUE
  out <- s$private$.supportedCutpoints(c(12, 60), c(10, 20, 30), c(1L, 2L, 2L))

  expect_equal(out, c(12, 60))
  expect_true(any(grepl("final state probabilities", s$log$msgs, fixed = TRUE)))
  expect_false(any(grepl("0%", s$log$msgs, fixed = TRUE)))
})

test_that("B1: factor status recovers the true cumulative incidence", {
  skip_if_not_installed("survival")
  d <- data.frame(time = seq(1, 60, length.out = 200),
                  status = c(rep(1L, 169), rep(2L, 31)))
  d$myoutcome_mstate <- factor(d$status, levels = c(0, 1, 2),
                               labels = c("censored", "event", "competing"))

  fit <- survival::survfit(survival::Surv(time, myoutcome_mstate) ~ 1, data = d)

  expect_identical(fit$states, c("(s0)", "event", "competing"))
  ev <- match("event", fit$states)
  expect_false(is.na(ev))

  s <- summary(fit, times = 60, extend = TRUE)
  # BEFORE: 0.000 (zero-fill branch).  AFTER: 0.845.
  expect_equal(unname(s$pstate[, ev]), 0.845, tolerance = 1e-6)
})

test_that("B1: factor states are stable when the event of interest never occurs", {
  skip_if_not_installed("survival")
  d <- data.frame(time = seq(1, 60, length.out = 200),
                  status = c(rep(0L, 100), rep(2L, 100)))
  d$myoutcome_mstate <- factor(d$status, levels = c(0, 1, 2),
                               labels = c("censored", "event", "competing"))
  fit <- survival::survfit(survival::Surv(time, myoutcome_mstate) ~ 1, data = d)

  expect_identical(fit$states, c("(s0)", "event", "competing"))
  s <- summary(fit, times = c(12, 60), extend = TRUE)
  # The genuine zero comes out of the fit; no zero-fill branch is needed.
  expect_equal(unname(s$pstate[, match("event", fit$states)]), c(0, 0))
})

test_that("B1: the deprecated numeric mstate fit is gone from the source", {
  src <- .singlearm_src()
  skip_if(is.null(src), "R/singlearm.b.R not available")
  code <- readLines(src, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl('type\\s*=\\s*"mstate"', code)),
               info = 'Surv(..., type="mstate") with a numeric status treats the lowest OBSERVED code as censoring')
  expect_true(any(grepl('match\\("event",\\s*states\\)', code)),
              info = "the CIF column must be selected by state name")
})


# B2 -- cutpoint ordering ----------------------------------------------------

test_that("B2: .resolveCutpoints returns an ascending, de-duplicated vector", {
  s <- .singlearm_stub("months")
  expect_equal(s$private$.resolveCutpoints("12, 36, 60, 24"), c(12, 24, 36, 60))
  expect_equal(s$private$.resolveCutpoints("36, 12, 24"),     c(12, 24, 36))
  expect_equal(s$private$.resolveCutpoints("24, 12, 24"),     c(12, 24))
})

test_that("B2: unsorted cutpoints used to corrupt n.event", {
  skip_if_not_installed("survival")
  set.seed(7)
  d <- data.frame(time = round(runif(120, 1, 70), 1),
                  status = c(rep(1L, 76), rep(0L, 44)))
  fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = d)

  sorted   <- summary(fit, times = c(12, 24, 36, 60), extend = TRUE)
  unsorted <- summary(fit, times = c(12, 36, 60, 24), extend = TRUE)

  # BEFORE (order as typed): 0/1/1/1 against 76 real events.
  expect_equal(as.vector(unsorted$n.event), c(1, 1, 1, 1))
  # AFTER (.resolveCutpoints sorts first): 8/15/18/25, a real partition.
  expect_equal(as.vector(sorted$n.event), c(8, 15, 18, 25))
  expect_equal(sum(sorted$n.event), sum(d$time <= 60 & d$status == 1L))

  # cumsum()/prev_time in the narrative are only meaningful on the sorted run.
  expect_true(all(diff(sorted$time) > 0))
})


# M1 -- zero-survival rows ---------------------------------------------------

test_that("M1: a cutpoint past follow-up is KEPT when the last observation is an event", {
  skip_if_not_installed("survival")
  set.seed(3)
  tt <- round(runif(40, 1, 48), 1)
  st <- rep(1L, 40)                       # every subject had the event
  s <- .singlearm_stub("months")

  # BEFORE: c(12, 36) -- the 60-month row was deleted.
  # AFTER : c(12, 36, 60), because S(60) = 0 exactly.
  expect_equal(s$private$.supportedCutpoints(c(12, 36, 60), tt, st), c(12, 36, 60))
  expect_true(any(grepl("^INFO", s$log$msgs)))
  expect_false(any(grepl("^WARNING", s$log$msgs)))

  fit <- survival::survfit(survival::Surv(tt, st) ~ 1)
  est <- summary(fit, times = 60, extend = TRUE)
  expect_equal(est$surv, 0)               # the number an oncologist wants
  expect_true(is.na(est$lower))           # CI correctly blank
})

test_that("M1: a cutpoint past follow-up is DROPPED when the last observation is censored", {
  set.seed(3)
  tt <- round(runif(40, 1, 48), 1)
  st <- rep(1L, 40)
  st[which.max(tt)] <- 0L                 # longest observation censored
  s <- .singlearm_stub("months")

  expect_equal(s$private$.supportedCutpoints(c(12, 36, 60), tt, st), c(12, 36))
  expect_true(any(grepl("longest observation is censored", s$log$msgs)))
})

test_that("M1: a tie at max follow-up mixing an event and a censoring is undefined", {
  s <- .singlearm_stub("months")
  tt <- c(10, 20, 48, 48)
  st <- c(1L, 1L, 1L, 0L)                 # someone is still event-free at 48
  expect_equal(s$private$.supportedCutpoints(c(12, 60), tt, st), 12)
})

test_that("M1: cutpoints inside follow-up are never touched and emit no notice", {
  s <- .singlearm_stub("months")
  expect_equal(s$private$.supportedCutpoints(c(12, 36), c(10, 40, 50), c(1L, 1L, 0L)),
               c(12, 36))
  expect_length(s$log$msgs, 0)
})

test_that("M1: .ciText blanks an undefined interval instead of printing NA", {
  s <- .singlearm_stub("months")
  out <- s$private$.ciText(c(0.62, NA_real_), c(0.89, NA_real_))
  expect_equal(out[2], "")
  expect_true(grepl("95% CI", out[1]))
})


# M2 -- honouring what the user typed ----------------------------------------

test_that("M2: typed cutpoints are used verbatim under a non-month unit", {
  s <- .singlearm_stub("years")
  # BEFORE: c(1, 3, 5) -- the typed values were discarded.
  # AFTER : c(12, 36, 60), exactly as entered.
  expect_equal(s$private$.resolveCutpoints("12, 36, 60"), c(12, 36, 60))

  msg <- s$log$msgs
  expect_length(msg, 1)
  expect_true(grepl("exactly as entered", msg))
  expect_true(grepl("1, 3, 5", msg))                      # what to type instead
  expect_false(grepl("Enter your own values", msg))       # the untrue sentence
  expect_false(grepl("rescaled", msg))
})

test_that("M2: months unit passes the default through silently", {
  s <- .singlearm_stub("months")
  expect_equal(s$private$.resolveCutpoints("12, 36, 60"), c(12, 36, 60))
  expect_length(s$log$msgs, 0)
})

test_that("M2: an unparseable string still falls back to the unit-aware default", {
  s <- .singlearm_stub("years")
  expect_equal(s$private$.resolveCutpoints(""), c(1, 3, 5))
  expect_length(s$log$msgs, 0)
})

test_that("M2: values other than the default are never commented on", {
  s <- .singlearm_stub("years")
  expect_equal(s$private$.resolveCutpoints("1, 2, 5"), c(1, 2, 5))
  expect_length(s$log$msgs, 0)
})
