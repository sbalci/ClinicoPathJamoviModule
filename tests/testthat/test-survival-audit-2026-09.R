# Regressions for the 2026-09 audit remediation of the `survival` analysis.
#
# Behavioural checks run the REAL private methods taken off the R6 generator
# against a recording stub `self`/`private`, so the assertions are about what
# the shipped code writes into the results. Source-level checks pin fixes that
# sit inside methods too large to drive here (calibration, RCS panel, stratified
# Cox text). Everything skips when R/*.b.R is absent (installed-package check).

library(testthat)

.ssrc <- function(file) {
  for (p in c(file.path("../../R", file), file.path("../R", file), file.path("R", file)))
    if (file.exists(p)) return(p)
  testthat::skip(paste0("R/", file, " not available (installed-package check)"))
}
.sjam <- function(file) {
  for (p in c(file.path("../../jamovi", file), file.path("../jamovi", file), file.path("jamovi", file)))
    if (file.exists(p)) return(p)
  testthat::skip(paste0("jamovi/", file, " not available (installed-package check)"))
}

.load_survival <- function() {
  e <- new.env(parent = globalenv())
  for (f in c("utils.R", "survival_utils.R", "survival.b.R"))
    suppressWarnings(suppressMessages(sys.source(.ssrc(f), envir = e)))
  e
}

# A recording stub around the real private methods. `extra_private` lets a test
# add the collaborators its method needs.
.stub <- function(e, options = list(), result_names = character(), extra_private = list()) {
  rec <- new.env(parent = emptyenv())
  rec$notes <- list(); rec$content <- list(); rec$rows <- list()
  rec$titles <- list(); rec$msgs <- character(); rec$explain <- character()

  item <- function(nm) list(
    setVisible = function(...) invisible(NULL),
    setTitle   = function(x) { rec$titles[[nm]] <- x; invisible(NULL) },
    setContent = function(x) { rec$content[[nm]] <- paste(x, collapse = " "); invisible(NULL) },
    setNote    = function(key, note) { rec$notes[[key]] <- note; invisible(NULL) },
    addRow     = function(rowKey, values) { rec$rows[[length(rec$rows) + 1L]] <- values; invisible(NULL) },
    setState   = function(x) { rec$state <- x; invisible(NULL) },
    getColumn  = function(name) list(setTitle = function(x) invisible(NULL))
  )

  stub <- new.env(parent = e)
  bind <- function(f) { environment(f) <- stub; f }
  pm <- e$survivalClass$private_methods
  stub$. <- function(x, ...) x
  stub$`%>%` <- magrittr::`%>%`
  results <- lapply(result_names, item); names(results) <- result_names
  stub$self <- list(options = options, results = results)
  priv <- list(
    .checkpoint          = function(...) invisible(NULL),
    .addHtmlMessage      = function(type, title, message)
      rec$msgs <- c(rec$msgs, paste0(type, ": ", title, " | ", message)),
    .setExplanationContent = function(nm, html) rec$explain <- c(rec$explain, nm),
    .buildSurvFormula    = bind(pm$.buildSurvFormula),
    .competingRiskCumInc = bind(pm$.competingRiskCumInc),
    .calculateResiduals  = bind(pm$.calculateResiduals),
    .medianSurv          = bind(pm$.medianSurv),
    .populateExplanations = bind(pm$.populateExplanations),
    .plot9               = bind(pm$.plot9),
    .isCompetingRisk     = function(...) FALSE,
    .competingRiskPlotRefusal = function(...) FALSE
  )
  for (nm in names(extra_private)) priv[[nm]] <- extra_private[[nm]]
  stub$private <- priv
  list(stub = stub, rec = rec)
}

.surv_data <- function(seed = 11, n = 60, levels = c("A", "B")) {
  set.seed(seed)
  data.frame(
    mytime = round(rexp(n, 1 / 20), 1) + 0.5,
    myoutcome = rbinom(n, 1, 0.6),
    myfactor = factor(sample(levels, n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}


# residual diagnostics -------------------------------------------------------

test_that("residuals table no longer carries an all-NA Schoenfeld column and gains the linear predictor", {
  e <- .load_survival()
  s <- .stub(e, result_names = "residualsTable")
  d <- .surv_data()
  cox <- survival::coxph(survival::Surv(mytime, myoutcome) ~ myfactor, data = d)

  res <- s$stub$private$.calculateResiduals(cox, d)

  expect_s3_class(res, "data.frame")
  expect_false("schoenfeld" %in% names(res))
  expect_true(all(c("observation", "lp", "martingale", "deviance", "score") %in% names(res)))
  expect_equal(nrow(res), nrow(d))
  expect_false(anyNA(res$lp))
  expect_false(anyNA(res$martingale))
  # No note for a single-column score matrix.
  expect_null(s$rec$notes$score_term)
})

test_that("score residuals for a >2-level factor are labelled as first-term only", {
  e <- .load_survival()
  s <- .stub(e, result_names = "residualsTable")
  d <- .surv_data(levels = c("A", "B", "C"))
  cox <- survival::coxph(survival::Surv(mytime, myoutcome) ~ myfactor, data = d)

  res <- s$stub$private$.calculateResiduals(cox, d)
  expect_match(s$rec$notes$score_term, "first model term")
  expect_false(anyNA(res$score))
})

test_that("residual plot draws against the linear predictor and still renders an old lp-less state", {
  skip_if_not_installed("ggplot2")
  e <- .load_survival()
  d <- .surv_data()
  cox <- survival::coxph(survival::Surv(mytime, myoutcome) ~ myfactor, data = d)
  s <- .stub(e, options = list(residual_diagnostics = TRUE))
  res <- s$stub$private$.calculateResiduals(cox, d)

  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  img <- list(state = list(residuals_data = res), setError = function(m) stop(m))
  expect_true(s$stub$private$.plot9(img, ggplot2::theme_minimal(), NULL))

  old <- res; old$lp <- NULL
  img_old <- list(state = list(residuals_data = old), setError = function(m) stop(m))
  expect_true(s$stub$private$.plot9(img_old, ggplot2::theme_minimal(), NULL))
})


# median section: Gray's test and zero-event groups --------------------------

.median_run <- function(e, d, competing = FALSE) {
  s <- .stub(e,
    options = list(timetypeoutput = "months", showExplanations = FALSE, explanatory = "myfactor"),
    result_names = c("medianTable", "medianSummary", "medianSurvivalExplanation"),
    extra_private = list(.isCompetingRisk = function(...) competing))
  results <- list(
    name1time = "mytime", name2outcome = "myoutcome", name3explanatory = "myfactor",
    myexplanatory_labelled = "myfactor", cleanData = d)
  s$stub$private$.medianSurv(results)
  s$rec
}

test_that("competing-risk mode surfaces Gray's test as a median-table note", {
  skip_if_not_installed("cmprsk")
  e <- .load_survival()
  set.seed(3)
  d <- data.frame(
    mytime = round(rexp(80, 1 / 20), 1) + 0.5,
    myoutcome = sample(c(0L, 1L, 2L), 80, replace = TRUE, prob = c(0.3, 0.45, 0.25)),
    myfactor = rep(c("A", "B"), 40), stringsAsFactors = FALSE)
  r <- .median_run(e, d, competing = TRUE)

  expect_match(r$notes$gray, "Gray's test")
  expect_match(r$notes$gray, "chi-square = [0-9.]+, df = 1, p = ")
  expect_match(r$notes$crci, "Confidence intervals")
  expect_length(r$rows, 2)
})

test_that("a group with zero events triggers a named warning; balanced data does not", {
  e <- .load_survival()
  d <- .surv_data()
  d$myoutcome[d$myfactor == "B"] <- 0L
  r <- .median_run(e, d)

  hit <- grep("^warning: No events in one or more groups", r$msgs, value = TRUE)
  expect_length(hit, 1)
  expect_match(hit, "zero events: B")
  expect_false(grepl("A", sub(".*zero events: ", "", hit)))

  r2 <- .median_run(e, .surv_data())
  expect_false(any(grepl("No events in one or more groups", r2$msgs)))
})


# explanation panels ---------------------------------------------------------

test_that(".populateExplanations() no longer overwrites the person-time panel", {
  e <- .load_survival()
  s <- .stub(e, options = list(showExplanations = TRUE))
  s$stub$private$.populateExplanations()

  expect_true("medianSurvivalExplanation" %in% s$rec$explain)
  expect_false("personTimeExplanation" %in% s$rec$explain)
})


# source-level pins ----------------------------------------------------------

test_that("Html sinks escape the stratification and RCS variable names", {
  src <- readLines(.ssrc("survival.b.R"))
  expect_true(any(grepl('paste("Stratified by", htmltools::htmlEscape(strata_var)', src, fixed = TRUE)))
  expect_true(any(grepl("rcs_var_html <- htmltools::htmlEscape(rcs_var)", src, fixed = TRUE)))
  expect_false(any(grepl('"<strong>", rcs_var,', src, fixed = TRUE)))
  expect_false(any(grepl('linear term for ", rcs_var,', src, fixed = TRUE)))
})

test_that("every model formula goes through the allow-listed parser", {
  src <- readLines(.ssrc("survival.b.R"))
  expect_false(any(grepl("stats::as.formula(", src, fixed = TRUE)))
  expect_true(any(grepl('.asSurvivalFormula(paste0("survival::Surv(.time, .status) ~ ", rhs))', src, fixed = TRUE)))
})

test_that("calibration uses the last baseline-hazard jump at or before the time point", {
  src <- readLines(.ssrc("survival.b.R"))
  expect_false(any(grepl("which.min(abs(basehaz_df$time - cal_time))", src, fixed = TRUE)))
  expect_true(any(grepl("bh_idx <- which(basehaz_df$time <= cal_time)", src, fixed = TRUE)))
  expect_true(any(grepl("if (length(bh_idx) == 0) 0 else basehaz_df$hazard[max(bh_idx)]", src, fixed = TRUE)))

  # The semantics the line encodes, on a hand-built step function.
  basehaz_df <- data.frame(time = c(2, 5, 9), hazard = c(0.1, 0.3, 0.7))
  at <- function(cal_time) { i <- which(basehaz_df$time <= cal_time); if (length(i) == 0) 0 else basehaz_df$hazard[max(i)] }
  expect_equal(at(1), 0)      # before first event: H0 = 0, not the 0.1 of the nearest jump
  expect_equal(at(5), 0.3)
  expect_equal(at(8.9), 0.3)  # nearest jump is 9 (0.7); the step function says 0.3
  expect_equal(at(9), 0.7)
})

test_that("pairwise table is never hidden as an error mechanism", {
  src <- readLines(.ssrc("survival.b.R"))
  expect_false(any(grepl("pairwiseTable$setVisible(FALSE)", src, fixed = TRUE)))
  expect_true(any(grepl('pairwiseTable$setNote("twolevels"', src, fixed = TRUE)))
})

test_that("PH-violation banner is a strong warning that carries the p-value", {
  src <- readLines(.ssrc("survival.b.R"))
  expect_true(any(grepl("smallest cox.zph p = {p}", src, fixed = TRUE)))
  i <- grep("smallest cox.zph p = {p}", src, fixed = TRUE)
  expect_true(any(grepl('"strongWarning"', src[(i - 3):i], fixed = TRUE)))
})

test_that("results schema: residuals columns, clearWith completeness, bootstrap enable", {
  r <- yaml::read_yaml(.sjam("survival.r.yaml"))
  items <- r$items; names(items) <- vapply(items, `[[`, "", "name")

  cols <- vapply(items$residualsTable$columns, `[[`, "", "name")
  expect_false("schoenfeld" %in% cols)
  expect_true("lp" %in% cols)

  for (nm in c("coxTable", "coxSummary", "tCoxtext2"))
    expect_true(all(c("stratified_cox", "strata_variable") %in% items[[nm]]$clearWith), info = nm)
  for (nm in c("ageAdjustedCoxTable", "ageInteractionTable"))
    expect_true("age_stratified_cox" %in% items[[nm]]$clearWith, info = nm)

  # timetypedata defines every survival time in date mode: wherever tint clears, so must it.
  for (it in items) {
    cw <- it$clearWith
    if (!is.null(cw) && "tint" %in% cw) expect_true("timetypedata" %in% cw, info = it$name)
  }

  u <- readLines(.sjam("survival.u.yaml"))
  i <- grep("name: bootstrapValN", u, fixed = TRUE)
  expect_true(any(grepl("enable: (bootstrapValidation)", u[i:(i + 4)], fixed = TRUE)))
})
