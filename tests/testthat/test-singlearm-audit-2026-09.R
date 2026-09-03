# Regressions for the 2026-09 audit remediation of singlearm.
#
# N1  STRONG_WARNING tier was unreachable (dead keyword heuristic in
#     .displayMessages); .addWarning(strong = TRUE) now carries the severity.
# N2  a zero-event / low-event cohort raised no always-visible notice.
# S1  time-specific survival CIs used survfit's default conf.type = "log"
#     (upper limit clipped at 1); .getCachedSurvfit now fits log-log, the
#     same band the median CI inverts.
# S2  survTable rows carried an undeclared std.err column.
# I1  the piecewise-hazard block was hidden with a one-way setVisible(FALSE)
#     under competing risks and never re-shown.
#
# The methods under test are the REAL functions taken off the R6 generator,
# run against recording stubs, so the assertions are about what the shipped
# code writes. Skips only when R/singlearm.b.R is absent (installed-package
# check).

library(testthat)

.audit_src <- function() {
  for (p in c("../../R/singlearm.b.R", "../R/singlearm.b.R", "R/singlearm.b.R"))
    if (file.exists(p)) return(p)
  testthat::skip("R/singlearm.b.R not available (installed-package check)")
}

.audit_methods <- function() {
  src <- .audit_src()
  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(src, envir = e)))
  list(pm = e$singlearmClass$private_methods, env = e)
}

# A stub whose `private` is an ENVIRONMENT, so the real .addWarning() can
# assign private$.warningMessages in place.
.audit_stub <- function(m, options = list(), results = list(), competing = FALSE,
                        recode = list(estimand = "overall survival", event_label = "Dead")) {
  pm <- m$pm
  stub <- new.env(parent = globalenv())
  stub$. <- function(x, ...) x
  stub$.singlearmNoticeHTML <- m$env$.singlearmNoticeHTML
  stub$`%>%` <- magrittr::`%>%`
  bind <- function(f) { environment(f) <- stub; f }
  options$translate <- function(text, n = NULL) text
  stub$self <- list(options = options, results = results)
  priv <- new.env(parent = emptyenv())
  priv$.cache <- new.env(parent = emptyenv())
  priv$.errorMessages <- character(0)
  priv$.warningMessages <- character(0)
  priv$.infoMessages <- character(0)
  priv$.eventRecode <- recode
  priv$.checkpoint <- function(...) invisible(NULL)
  priv$.isCompetingRisk <- function(...) competing
  for (nm in c(".addError", ".addWarning", ".addInfo", ".displayMessages",
               ".eventCountNotice", ".hazardSectionVisibility",
               ".getCachedSurvfit", ".safeExecute", ".estimandMeta",
               ".yearInUnits", ".getDefaultCutpoints", ".resolveCutpoints",
               ".supportedCutpoints", ".parseNumericList", ".ciText",
               ".survTable", ".medianSurv", ".competingRiskCumInc"))
    priv[[nm]] <- bind(pm[[nm]])
  stub$private <- priv
  stub
}

.rec_item <- function(rec, nm) {
  force(nm)   # items are built in for-loops; a lazy promise would see the last name
  list(
  setVisible = function(x) { rec$visible[[nm]] <- x; invisible(NULL) },
  setTitle   = function(x) invisible(NULL),
  setContent = function(x) { rec$content[[nm]] <- paste(x, collapse = " "); invisible(NULL) },
  setNote    = function(key, note) { rec$notes[[key]] <- note; invisible(NULL) },
  addRow     = function(rowKey, values) {
    rec$rows[[length(rec$rows) + 1L]] <- values; invisible(NULL)
  },
  getColumn  = function(name) list(setTitle = function(x) invisible(NULL))
  )
}

.recorder <- function() {
  rec <- new.env(parent = emptyenv())
  rec$visible <- list(); rec$content <- list(); rec$notes <- list(); rec$rows <- list()
  rec
}


# N1 -- severity travels with the message ------------------------------------

test_that("N1: .addWarning(strong = TRUE) renders as STRONG_WARNING, plain as WARNING", {
  m <- .audit_methods()
  rec <- .recorder()
  stub <- .audit_stub(m, results = list(
    errors = .rec_item(rec, "errors"), warnings = .rec_item(rec, "warnings"),
    info = .rec_item(rec, "info")))

  stub$private$.addWarning("ordinary caution")
  stub$private$.addWarning("loud caution", strong = TRUE)
  stub$private$.displayMessages()

  html <- rec$content$warnings
  expect_true(isTRUE(rec$visible$warnings))
  expect_match(html, "<strong> WARNING:</strong> ordinary caution", fixed = TRUE)
  expect_match(html, "<strong> STRONG_WARNING:</strong> loud caution", fixed = TRUE)
  # BEFORE: a keyword grep ("Very few events|critically") no message contained.
  expect_false(grepl("Very few events|critically", m$env$singlearmClass$private_methods$.displayMessages |>
                       deparse() |> paste(collapse = "\n")))
  expect_false(isTRUE(rec$visible$errors))
})


# N2 -- zero / low event notice -----------------------------------------------

.event_notice <- function(n_events, n_total = 40, n_competing = 0, competing = FALSE) {
  m <- .audit_methods()
  stub <- .audit_stub(m, competing = competing)
  stub$private$.eventCountNotice(list(
    n_events = n_events, n_total = n_total, n_competing = n_competing,
    n_censored = n_total - n_events - n_competing))
  stub$private$.warningMessages
}

test_that("N2: zero events raise an always-on STRONG_WARNING naming the count and event level", {
  w <- .event_notice(0, n_total = 40)
  expect_length(w, 1)
  expect_equal(names(w), "STRONG_WARNING")
  expect_match(w, "No events of interest were observed among 40 subjects (40 censored)", fixed = TRUE)
  expect_match(w, 'mapped to the event of interest is "Dead"', fixed = TRUE)
  expect_match(w, "check that it is the intended level", fixed = TRUE)
})

test_that("N2: zero events under competing risks counts the competing events too", {
  w <- .event_notice(0, n_total = 20, n_competing = 8, competing = TRUE)
  expect_match(w, "among 20 subjects (12 censored, 8 competing)", fixed = TRUE)
})

test_that("N2: fewer than 10 events is a quantified STRONG_WARNING; 10 or more is silent", {
  w <- .event_notice(7, n_total = 55)
  expect_equal(names(w), "STRONG_WARNING")
  expect_match(w, "Only 7 event(s) of interest among 55 subjects", fixed = TRUE)
  expect_match(w, "fewer than 10 events", fixed = TRUE)
  expect_length(.event_notice(10), 0)
  expect_length(.event_notice(25), 0)
})


# S1 / S2 -- survTable: log-log band, no std.err column ------------------------

.survtable_run <- function(status, times, cutp = "5, 10, 15") {
  m <- .audit_methods()
  rec <- .recorder()
  results_stub <- list()
  for (nm in c("survTable", "survTableSummary", "survTableHeading",
               "survTableHeading3", "survivalProbabilityExplanation"))
    results_stub[[nm]] <- .rec_item(rec, nm)
  stub <- .audit_stub(m,
    options = list(timetypeoutput = "months", showExplanations = FALSE, cutp = cutp),
    results = results_stub)
  # Spy on the fit so the conf.type the code asks for is observable.
  real_fit <- stub$private$.getCachedSurvfit
  rec$conf_types <- character()
  stub$private$.getCachedSurvfit <- function(formula, data, cache_key_suffix = "", ...) {
    fit <- real_fit(formula, data, cache_key_suffix, ...)
    rec$conf_types <- c(rec$conf_types, fit$conf.type)
    fit
  }
  stub$private$.survTable(list(
    name1time = "mytime", name2outcome = "myoutcome", name3explanatory = "myfactor",
    cleanData = data.frame(mytime = times, myoutcome = as.integer(status),
                           myfactor = "1", stringsAsFactors = FALSE)))
  rec
}

test_that("S1: time-specific survival CIs use the log-log band and stay inside (0, 1)", {
  set.seed(3)
  status <- c(rep(1L, 6), rep(0L, 14))
  times <- c(sort(round(runif(6, 1, 20), 1)), rep(24, 14))
  rec <- .survtable_run(status, times)

  expect_true(all(rec$conf_types == "log-log"))
  expect_length(rec$rows, 3)
  # Independent check against survfit with conf.type = "log-log".
  ref <- survival::survfit(survival::Surv(mytime, myoutcome) ~ 1,
                           data = data.frame(mytime = times, myoutcome = status),
                           conf.type = "log-log")
  ref_s <- summary(ref, times = c(5, 10, 15), extend = TRUE)
  for (i in 1:3) {
    expect_equal(as.numeric(rec$rows[[i]]$surv), ref_s$surv[i], tolerance = 1e-8)
    expect_equal(as.numeric(rec$rows[[i]]$upper), ref_s$upper[i], tolerance = 1e-8)
    expect_lt(as.numeric(rec$rows[[i]]$upper), 1)
  }
})

test_that("S2: survTable rows carry only declared columns (no std.err)", {
  rec <- .survtable_run(c(rep(1L, 6), rep(0L, 14)), c(2, 4, 6, 8, 12, 14, rep(24, 14)))
  expect_false("std.err" %in% names(rec$rows[[1]]))
  expect_setequal(names(rec$rows[[1]]), c("time", "n.risk", "n.event", "surv", "lower", "upper"))
})

test_that("S1: the median section requests the same log-log band", {
  m <- .audit_methods()
  rec <- .recorder()
  results_stub <- list()
  for (nm in c("medianHeading", "medianTable", "medianSummary", "medianHeading3",
               "medianSurvivalExplanation"))
    results_stub[[nm]] <- .rec_item(rec, nm)
  stub <- .audit_stub(m,
    options = list(timetypeoutput = "months", showExplanations = FALSE),
    results = results_stub)
  real_fit <- stub$private$.getCachedSurvfit
  rec$conf_types <- character()
  stub$private$.getCachedSurvfit <- function(formula, data, cache_key_suffix = "", ...) {
    fit <- real_fit(formula, data, cache_key_suffix, ...)
    rec$conf_types <- c(rec$conf_types, fit$conf.type)
    fit
  }
  stub$private$.medianSurv(list(
    name1time = "mytime", name2outcome = "myoutcome", name3explanatory = "myfactor",
    cleanData = data.frame(mytime = c(1:15, rep(20, 5)),
                           myoutcome = c(rep(1L, 15), rep(0L, 5)),
                           myfactor = "1", stringsAsFactors = FALSE)))
  expect_equal(rec$conf_types, "log-log")
  expect_match(rec$content$medianSummary, "conf.type='log-log'", fixed = TRUE)
  expect_false(grepl("conf.type='log'", rec$content$medianSummary, fixed = TRUE))
})


# I1 -- hazard section visibility is symmetric ---------------------------------

.hazard_vis <- function(cr, baseline_hazard = TRUE, hazard_smoothing = FALSE,
                        showSummaries = FALSE, showExplanations = FALSE) {
  m <- .audit_methods()
  rec <- .recorder()
  nms <- c("baselineHazardHeading", "baselineHazardTable", "baselineHazardPlot",
           "smoothedHazardPlot", "baselineHazardSummary", "baselineHazardHeading3",
           "baselineHazardExplanation")
  results_stub <- list()
  for (nm in nms) results_stub[[nm]] <- .rec_item(rec, nm)
  stub <- .audit_stub(m,
    options = list(baseline_hazard = baseline_hazard, hazard_smoothing = hazard_smoothing,
                   showSummaries = showSummaries, showExplanations = showExplanations),
    results = results_stub)
  stub$private$.hazardSectionVisibility(cr)
  rec$visible
}

test_that("I1: competing risks hides the seven hazard elements", {
  v <- .hazard_vis(cr = TRUE, hazard_smoothing = TRUE, showSummaries = TRUE, showExplanations = TRUE)
  expect_length(v, 7)
  expect_true(all(!unlist(v)))
})

test_that("I1: switching back to ordinary survival re-shows them per the option bindings", {
  # BEFORE: nothing ever called setVisible(TRUE); the populated table stayed hidden.
  v <- .hazard_vis(cr = FALSE, hazard_smoothing = TRUE, showSummaries = TRUE, showExplanations = FALSE)
  expect_true(v$baselineHazardHeading)
  expect_true(v$baselineHazardTable)
  expect_true(v$baselineHazardPlot)
  expect_true(v$smoothedHazardPlot)
  expect_true(v$baselineHazardSummary)
  expect_false(v$baselineHazardHeading3)
  expect_false(v$baselineHazardExplanation)

  v2 <- .hazard_vis(cr = FALSE, baseline_hazard = FALSE, hazard_smoothing = TRUE)
  expect_false(v2$baselineHazardTable)
  expect_true(v2$smoothedHazardPlot)
})

test_that("I1: .run() no longer carries a one-way setVisible(FALSE) on the hazard block", {
  src <- readLines(.audit_src())
  expect_false(any(grepl('try(it$setVisible(FALSE), silent = TRUE)', src, fixed = TRUE)))
  expect_true(any(grepl("private$.hazardSectionVisibility(cr)", src, fixed = TRUE)))
  # Stale references to the removed `pplot` option are gone.
  expect_false(any(grepl("options$pplot", src, fixed = TRUE)))
})
