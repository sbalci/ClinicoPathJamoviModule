# Regressions for the 2026-09 survivalcont audit remediation.
#
# I-MEDIUM  the calculatedtime Output exported landmark-shifted, landmark-
#           filtered times under a "time from dxdate to fudate" description
# H-LOW     six bare warning() calls in .multipleCutoffs() duplicated the
#           "Multiple cut-offs unavailable" notice in the Analysis Notes panel
# C-LOW     hand-rolled backtick quoting -> jmvcore::composeTerm
# STAT      narrative promised a multiplicity-adjusted p-value that
#           survminer::surv_cutpoint() (pmethod = "none") never computes
# INTEG     coxSummary receives HTML but was type Preformatted; .init() mirrored
#           every .r.yaml `visible:` rule imperatively; clearWith gaps
#
# The methods are run as the REAL closures taken off the R6 generator against a
# stub, so the assertions are about what the shipped code does.

library(testthat)

.sc_src <- function(file) {
  for (p in c(file.path("../../R", file), file.path("../R", file), file.path("R", file)))
    if (file.exists(p)) return(p)
  testthat::skip(paste0("R/", file, " not available (installed-package check)"))
}

.sc_path <- function(rel) {
  for (p in c(file.path("../..", rel), file.path("..", rel), rel))
    if (file.exists(p)) return(p)
  testthat::skip(paste0(rel, " not available (installed-package check)"))
}

.sc_env <- function() {
  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(.sc_src("survival_utils.R"), envir = e)))
  suppressWarnings(suppressMessages(sys.source(.sc_src("survivalcont.b.R"), envir = e)))
  e
}

# Stub around the real private methods. `private` is an ENVIRONMENT so that
# `private$.x <- v` inside a method mutates shared state, as in a real R6 object.
.sc_stub <- function(e, options, data = NULL) {
  pm <- e$survivalcontClass$private_methods
  rec <- new.env(parent = emptyenv())
  rec$msgs <- list()

  stub <- new.env(parent = e)
  bind <- function(f) { environment(f) <- stub; f }
  stub$. <- function(x, ...) x
  stub$`%>%` <- magrittr::`%>%`
  stub$`:=` <- rlang::`:=`
  stub$`!!` <- rlang::`!!`
  stub$self <- list(options = options, data = data, results = list())
  priv <- new.env(parent = emptyenv())
  priv$.checkpoint <- function(...) invisible(NULL)
  priv$.addHtmlMessage <- function(type, title, message) {
    rec$msgs[[length(rec$msgs) + 1L]] <- list(type = type, title = title, message = message)
    invisible(NULL)
  }
  priv$.eventRecode <- NULL
  priv$.multicutFailReason <- NULL
  for (nm in c(".getData", ".definemytime", ".definemyoutcome", ".definemyfactor",
               ".eventOfInterestIndicator", ".cleandata", ".multipleCutoffs",
               ".escapeVariableNames", ".quantileCutoffs"))
    assign(nm, bind(pm[[nm]]), envir = priv)
  stub$private <- priv
  list(private = priv, rec = rec)
}

.sc_options <- function(...) {
  base <- list(
    elapsedtime = "time", outcome = "status", outcomeLevel = "1", contexpl = "marker",
    dxdate = NULL, fudate = NULL, tint = FALSE, timetypedata = "ymd",
    timetypeoutput = "months", multievent = FALSE, analysistype = "overall",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    uselandmark = FALSE, landmark = 3,
    cutoff_method = "quantile", num_cutoffs = "two", min_group_size = 10
  )
  utils::modifyList(base, list(...))
}


# I-MEDIUM: calculatedtime export -------------------------------------------

test_that("I-MEDIUM: .cleandata() returns the raw interval for every row, separate from the landmark-shifted analysis time", {
  e <- .sc_env()
  data <- data.frame(
    time   = c(1, 2, 5, 8, 10, 12, NA),
    status = c(1, 0, 1, 0, 1, 1, 1),
    marker = c(3.1, 2.2, 5.5, 1.0, 4.4, 6.6, 2.0)
  )
  st <- .sc_stub(e, .sc_options(uselandmark = TRUE, landmark = 3), data = data)

  results <- st$private$.cleandata()
  expect_type(results, "list")

  # Analysis time: rows below the landmark are gone and times are shifted.
  expect_equal(nrow(results$cleanData), 4)
  expect_equal(sort(results$cleanData$time), c(2, 5, 7, 9))

  # Exported time: one row per input row, unshifted, NA where time is missing.
  # BEFORE: the export copied cleanData$CalculatedTime (4 rows, shifted).
  expect_equal(nrow(results$calculated_time), nrow(data))
  expect_equal(results$calculated_time$mytime, data$time, ignore_attr = TRUE)
  expect_equal(results$calculated_time$row_names, rownames(data), ignore_attr = TRUE)
})

test_that("I-MEDIUM: .run() exports calculated_time, not the landmark-shifted analysis column", {
  src <- readLines(.sc_src("survivalcont.b.R"))
  export <- grep("calculatedtime\\$setValues", src, value = TRUE)
  expect_length(export, 1)
  expect_match(export, "results$calculated_time$mytime", fixed = TRUE)
  expect_false(any(grepl("calculatedtime$setValues(results$cleanData", src, fixed = TRUE)))
})


# H-LOW: .multipleCutoffs() failure -> one notice, no console warning ---------

test_that("H-LOW: too few marker values -> NULL, quantified reason, and no warning()", {
  e <- .sc_env()
  st <- .sc_stub(e, .sc_options())
  results <- list(
    name1time = "mytime", analysis_outcome = "myoutcome", name3contexpl = "marker",
    cleanData = data.frame(mytime = c(1, 2, 3, 4, 5), myoutcome = c(1, 0, 1, 0, 1),
                           marker = c(1, 2, NA, 4, 5))
  )
  expect_no_warning(out <- st$private$.multipleCutoffs(results))
  expect_null(out)
  expect_match(st$private$.multicutFailReason, "only 4 non-missing value")
  expect_match(st$private$.multicutFailReason, "at least 10")
})

test_that("H-LOW: a missing marker column is reported as a reason, not a warning()", {
  e <- .sc_env()
  st <- .sc_stub(e, .sc_options())
  results <- list(
    name1time = "mytime", analysis_outcome = "myoutcome", name3contexpl = "nope",
    cleanData = data.frame(mytime = 1:12, myoutcome = rep(c(1, 0), 6), marker = 1:12)
  )
  expect_no_warning(out <- st$private$.multipleCutoffs(results))
  expect_null(out)
  expect_match(st$private$.multicutFailReason, "not found")
})

test_that("H-LOW: .multipleCutoffs() has no bare warning() left and .run() folds the reason in", {
  src <- readLines(.sc_src("survivalcont.b.R"))
  start <- grep("^\\s*\\.multipleCutoffs = function\\(results\\)", src)
  end <- grep("^\\s*\\.quantileCutoffs = function", src)
  expect_length(start, 1); expect_length(end, 1)
  body <- src[start:end]
  expect_false(any(grepl("^\\s*warning\\(", body)))
  expect_true(any(grepl("private$.multicutFailReason <- reason", body, fixed = TRUE)))
  expect_true(any(grepl("reason <- private$.multicutFailReason", src, fixed = TRUE)))
  # Reset every cycle so a stale reason cannot decorate a later success/failure.
  expect_true(any(grepl("private$.multicutFailReason <- NULL", src, fixed = TRUE)))
})


# C-LOW: composeTerm ----------------------------------------------------------

test_that("C-LOW: .escapeVariableNames() delegates to jmvcore::composeTerm", {
  e <- .sc_env()
  st <- .sc_stub(e, .sc_options())
  expect_equal(st$private$.escapeVariableNames("CalculatedTime"), "CalculatedTime")
  expect_equal(st$private$.escapeVariableNames("a b"), "`a b`")
  expect_equal(st$private$.escapeVariableNames(c("x", "y z")), c("x", "`y z`"))
  expect_equal(st$private$.escapeVariableNames("x`y"), jmvcore::composeTerm("x`y"))
})


# STAT: no adjusted p-value is promised ---------------------------------------

test_that("STAT: the cut-off narrative no longer promises a multiplicity-adjusted p-value", {
  src <- paste(readLines(.sc_src("survivalcont.b.R")), collapse = "\n")
  expect_false(grepl("Adjusts p-value for multiple testing", src, fixed = TRUE))
  expect_false(grepl("Multiple testing correction</td>", src, fixed = TRUE))
  expect_false(grepl(">Adjusted p-value</td>", src, fixed = TRUE))
  expect_true(grepl("No multiplicity-adjusted p-value is computed or reported", src, fixed = TRUE))
  expect_true(grepl("no multiplicity-adjusted p-value is reported for this cut-off", src, fixed = TRUE))
  # Tail probabilities: no 1 - pchisq() cancellation left.
  expect_false(grepl("1 - stats::pchisq", src, fixed = TRUE))
  expect_false(grepl("1 - pchisq", src, fixed = TRUE))
  # On-plot log-rank p on the multi-cut-off KM plot carries its caveat.
  expect_true(grepl("log-rank p is exploratory: groups were chosen from these data", src, fixed = TRUE))
})

test_that("STAT: rescutTable cites maxstat, which the notice tells users to cite", {
  r <- yaml::read_yaml(.sc_path("jamovi/survivalcont.r.yaml"))
  items <- setNames(r$items, vapply(r$items, `[[`, "", "name"))
  expect_true("maxstat" %in% unlist(items$rescutTable$refs))
  refs <- yaml::read_yaml(.sc_path("jamovi/00refs.yaml"))
  expect_true("maxstat" %in% names(refs$refs))
})


# INTEG: result schema ---------------------------------------------------------

test_that("INTEG: coxSummary is an Html item (it receives interpretation-box HTML)", {
  r <- yaml::read_yaml(.sc_path("jamovi/survivalcont.r.yaml"))
  items <- setNames(r$items, vapply(r$items, `[[`, "", "name"))
  expect_equal(items$coxSummary$type, "Html")
})

test_that("INTEG: .init() no longer mirrors the .r.yaml visible: rules", {
  src <- readLines(.sc_src("survivalcont.b.R"))
  start <- grep("^\\s*\\.init = function\\(\\)", src)
  end <- grep("^\\s*\\.getData = function\\(\\)", src)
  expect_length(start, 1); expect_length(end, 1)
  expect_false(any(grepl("setVisible", src[start:end])))
})

test_that("INTEG: clearWith covers the data-gating options on the previously stale tables", {
  r <- yaml::read_yaml(.sc_path("jamovi/survivalcont.r.yaml"))
  items <- setNames(r$items, vapply(r$items, `[[`, "", "name"))
  cw <- function(nm) unlist(items[[nm]]$clearWith)
  expect_true(all(c("uselandmark", "landmark", "tint", "dxdate", "fudate",
                    "dod", "dooc", "awd", "awod") %in% cw("stratifiedCoxTable")))
  expect_true("contexpl" %in% cw("personTimeTable"))
  expect_true("contexpl" %in% cw("personTimeSummary"))
  expect_true(all(c("tint", "dxdate", "fudate") %in% cw("schoenfeldResidualsTable")))
  expect_true(all(c("elapsedtime", "tint", "dxdate", "fudate") %in% cw("rmstSummary")))
  expect_false(any(grepl("mydataview", names(items))))
})

test_that("INTEG: dead debug scaffolding and the uncalled competing-risk helper are gone", {
  src <- paste(readLines(.sc_src("survivalcont.b.R")), collapse = "\n")
  expect_false(grepl("mydataview", src, fixed = TRUE))
  expect_false(grepl(".competingRiskCumInc", src, fixed = TRUE))
})
