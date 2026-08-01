# Regressions for the median section of singlearm on zero-event cohorts.
#
# L1  the competing-risk branch reported "Median survival"
# L2  a competing-risk cohort with no event of interest errored out of
#     .medianSurv() before populating the median table -- and because
#     .generateClinicalSummary() returns as soon as medianTable$rowCount is 0,
#     the copy-ready clinical summary came out blank as well
#     ... while the KM branch, now reachable with zero events, printed
#     "Median survival is NA [NA - NA, 95% CI] months."
#
# .medianSurv() is run as the REAL method taken off the R6 generator against a
# recording stub, so the assertions are about what the shipped code actually
# writes into the results. Every assertion runs unconditionally.

library(testthat)

.msrc <- function(file) {
  for (p in c(file.path("../../R", file), file.path("../R", file), file.path("R", file)))
    if (file.exists(p)) return(p)
  NULL
}

.median_run <- function(status, times = seq(2, 60, length.out = length(status)),
                        competing = FALSE, timetypeoutput = "months") {
  src <- .msrc("singlearm.b.R")
  if (is.null(src)) stop("R/singlearm.b.R not found")

  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(src, envir = e)))
  pm <- e$singlearmClass$private_methods

  rec <- new.env(parent = emptyenv())
  rec$titles <- list(); rec$content <- list(); rec$notes <- list()
  rec$rows <- list(); rec$msgs <- character()

  item <- function(nm) list(
    setVisible = function(...) invisible(NULL),
    setTitle   = function(x) { rec$titles[[nm]] <- x; invisible(NULL) },
    setContent = function(x) { rec$content[[nm]] <- paste(x, collapse = " "); invisible(NULL) },
    setNote    = function(key, note) { rec$notes[[key]] <- note; invisible(NULL) },
    addRow     = function(rowKey, values) {
      rec$rows[[length(rec$rows) + 1L]] <- values; invisible(NULL)
    },
    getColumn  = function(name) list(setTitle = function(x) invisible(NULL))
  )
  add <- function(kind) function(m) rec$msgs <- c(rec$msgs, paste0(kind, ": ", m))

  stub <- new.env(parent = globalenv())
  bind <- function(f) { environment(f) <- stub; f }
  stub$. <- function(x, ...) x                     # translation shim
  stub$`%>%` <- magrittr::`%>%`
  stub$self <- list(
    options = list(timetypeoutput = timetypeoutput, showExplanations = FALSE),
    results = list(
      medianHeading             = item("medianHeading"),
      medianTable               = item("medianTable"),
      medianSummary             = item("medianSummary"),
      medianHeading3            = item("medianHeading3"),
      medianSurvivalExplanation = item("medianSurvivalExplanation")
    )
  )
  stub$private <- list(
    .cache               = new.env(parent = emptyenv()),
    .checkpoint          = function(...) invisible(NULL),
    .displayMessages     = function() invisible(NULL),
    .isCompetingRisk     = function(...) competing,
    .estimandMeta        = function(...) list(
      probability = "Kaplan-Meier event-free probability",
      median = "Median event-free time",
      median_lower = "median event-free time",
      curve = "Event-Free Probability for the Selected Event"),
    .addInfo             = add("INFO"),
    .addWarning          = add("WARNING"),
    .addError            = add("ERROR"),
    .safeExecute         = bind(pm$.safeExecute),
    .getCachedSurvfit    = bind(pm$.getCachedSurvfit),
    .competingRiskCumInc = bind(pm$.competingRiskCumInc),
    .medianSurv          = bind(pm$.medianSurv)
  )

  results <- list(
    name1time = "mytime", name2outcome = "myoutcome", name3explanatory = "myfactor",
    cleanData = data.frame(mytime = times, myoutcome = as.integer(status),
                           myfactor = "1", stringsAsFactors = FALSE)
  )
  stub$private$.medianSurv(results)
  rec
}


# competing risks, no event of interest --------------------------------------

test_that("L2: a competing-risk cohort with no event of interest still fills the median table", {
  r <- .median_run(c(rep(0L, 10), rep(2L, 10)), competing = TRUE)

  # BEFORE: 'No cumulative incidence found for event of interest' + return(),
  # so no row was ever added and the clinical summary downstream was blank.
  expect_false(any(grepl("^ERROR", r$msgs)))
  expect_length(r$rows, 1)
  expect_equal(as.numeric(r$rows[[1]]$records), 20)
  expect_equal(as.numeric(r$rows[[1]]$events), 0)
  expect_true(is.na(r$rows[[1]]$median))
  expect_true(is.na(r$rows[[1]]$x0_95lcl))
})

test_that("L2: the zero-event competing-risk narrative says why, and is not a survival claim", {
  r <- .median_run(c(rep(0L, 10), rep(2L, 10)), competing = TRUE)
  txt <- r$content$medianSummary

  expect_match(txt, "no event of interest was observed")
  expect_match(txt, "cumulative incidence is 0")
  expect_false(grepl("Median survival", txt, fixed = TRUE))
  expect_false(grepl("NA", txt, fixed = TRUE))
  expect_true(any(grepl("INFO: No event of interest", r$msgs)))
})

test_that("L1: the competing-risk median section is titled after the estimand", {
  r <- .median_run(c(rep(0L, 8), rep(1L, 6), rep(2L, 6)), competing = TRUE)
  expect_match(r$titles$medianTable, "Cumulative Incidence")
  expect_match(r$titles$medianHeading, "Median Time to Event of Interest")
  expect_match(r$titles$medianSummary, "Median Time to Event of Interest")
  expect_false(grepl("Median Survival", r$titles$medianTable, fixed = TRUE))
  # ... and the empty CI cells are explained rather than left ambiguous.
  expect_match(r$notes$cr_ci, "No confidence interval")
})

test_that("L1/L2: a competing-risk cohort WITH events is unchanged by the zero-event branch", {
  # The input this change could newly damage.
  r <- .median_run(c(rep(1L, 14), rep(2L, 3), rep(0L, 3)), competing = TRUE)
  expect_false(any(grepl("^ERROR", r$msgs)))
  expect_length(r$rows, 1)
  expect_equal(as.numeric(r$rows[[1]]$events), 14)
  expect_false(is.na(r$rows[[1]]$median))
  expect_match(r$content$medianSummary, "Median time to event of interest is")
  expect_false(grepl("no event of interest was observed", r$content$medianSummary,
                     fixed = TRUE))
})


# the copy-ready clinical summary that used to come out blank ----------------

.clinical_summary <- function(cells, competing, max_time = 48) {
  src <- .msrc("singlearm.b.R")
  if (is.null(src)) stop("R/singlearm.b.R not found")
  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(src, envir = e)))
  f <- e$singlearmClass$private_methods$.generateClinicalSummary

  rec <- new.env(parent = emptyenv()); rec$html <- NULL
  stub <- new.env(parent = globalenv())
  stub$. <- function(x, ...) x
  stub$self <- list(
    options = list(timetypeoutput = "months"),
    results = list(
      medianTable = list(
        rowCount = 1L,
        getCell  = function(rowNo, ...) list(value = cells[[c(...)[[1]]]])),
      clinicalSummary = list(setContent = function(x) { rec$html <- x; invisible(NULL) })
    ))
  stub$private <- list(
    .isCompetingRisk = function(...) competing,
    .estimandMeta = function(...) list(
      probability = "Kaplan-Meier event-free probability",
      median = "Median event-free time",
      median_lower = "median event-free time",
      curve = "Event-Free Probability for the Selected Event"))
  environment(f) <- stub
  f(list(data_quality = list(max_time = max_time)))
  rec$html
}

test_that("L2: a competing-risk cohort with no event of interest still gets a clinical summary", {
  # BEFORE: .medianSurv() returned early, medianTable$rowCount was 0, and
  # .generateClinicalSummary() returned before writing anything -- a blank
  # panel headed "Copy-Ready for Reports".
  html <- .clinical_summary(list(records = 20, events = 0, median = NA_real_,
                                 x0_95lcl = NA_real_, x0_95ucl = NA_real_),
                            competing = TRUE)
  expect_true(nzchar(html))
  expect_match(html, "median time to the event of interest could not be estimated")
  expect_match(html, "no event of interest was observed")
  expect_false(grepl("Median survival", html, fixed = TRUE))
  expect_false(grepl("NA-NA", html, fixed = TRUE))
})

test_that("L1: a competing-risk median is not called median survival in the clinical summary", {
  html <- .clinical_summary(list(records = 20, events = 9, median = 18.4,
                                 x0_95lcl = NA_real_, x0_95ucl = NA_real_),
                            competing = TRUE)
  expect_match(html, "Median time to event of interest was 18.4 months")
  expect_false(grepl("95% CI: NA", html, fixed = TRUE))
})

test_that("L1: a generic ordinary cohort uses event-free wording, with its CI", {
  # The input the estimand relabelling could newly damage.
  html <- .clinical_summary(list(records = 40, events = 25, median = 18.4,
                                 x0_95lcl = 12.1, x0_95ucl = 26.9),
                            competing = FALSE)
  expect_match(html, "Median event-free time was 18.4 months")
  expect_match(html, "95% CI: 12.1-26.9 months")
})


# Kaplan-Meier, zero events (newly reachable) --------------------------------

test_that("L2: a fully censored KM cohort reports 'not estimable', never 'NA'", {
  r <- .median_run(rep(0L, 20), competing = FALSE)
  txt <- r$content$medianSummary

  expect_length(r$rows, 1)
  expect_equal(as.numeric(r$rows[[1]]$events), 0)
  expect_true(is.na(r$rows[[1]]$median))
  # BEFORE: "Median survival is NA [NA - NA, 95% CI] months."
  expect_match(txt, "no events were observed")
  expect_false(grepl("is NA", txt, fixed = TRUE))
  expect_false(grepl("NA -", txt, fixed = TRUE))
  expect_match(txt, "The median was not reached")
  expect_match(r$titles$medianTable, "Median event-free time Table")
})

test_that("L2: an ordinary KM cohort still reports its median and CI", {
  # The input the narrative rewrite could newly damage.
  set.seed(11)
  r <- .median_run(c(rep(1L, 30), rep(0L, 5)),
                   times = c(sort(round(runif(30, 1, 40), 1)), rep(45, 5)))
  txt <- r$content$medianSummary

  expect_match(txt, "^Median event-free time is [0-9.]+ months")
  expect_match(txt, "95% CI: [0-9.]+ - [0-9.]+")
  expect_false(grepl("not estimable|no events were observed", txt))
  expect_match(txt, "first time at which estimated event-free probability is 50% or lower")
})
