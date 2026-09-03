# Regressions for the 2026-09 audit remediation of outcomeorganizer.
#
# 1. The "rare event" check used `length(unique(x)) == 2`, which counts NA as a
#    level, so any 2-level outcome with a missing value skipped the check; and its
#    text talked about logistic-regression separation in a survival recoder.
# 2. `addOutcome` was gated on `self$options$addOutcome`, unreachable from the
#    R wrapper; `addAdminTime` on `isNotFilled()`. Both now use `isNotFilled()`.
# 3. rfs/pfs/dfs without a recurrence variable emitted two near-identical
#    WARNINGs; one remains.
# 4. n < 10 is a STRONG_WARNING, not a WARNING.
#
# Runs against the sourced R/ files (no installed package needed) and skips when
# they are absent (installed-package R CMD check).

library(testthat)

.oo_src <- function(file) {
  for (p in c(file.path("../../R", file), file.path("../R", file), file.path("R", file)))
    if (file.exists(p)) return(p)
  testthat::skip(paste0("R/", file, " not available (installed-package check)"))
}

.oo_env <- local({
  env <- NULL
  function() {
    if (!is.null(env)) return(env)
    skip_if_not_installed("jmvcore"); skip_if_not_installed("magrittr")
    skip_if_not_installed("dplyr"); skip_if_not_installed("labelled")
    e <- new.env(parent = globalenv())
    assign(".", jmvcore::., envir = e)
    assign("%>%", magrittr::`%>%`, envir = e)
    for (f in c("utils.R", "survival_utils.R", "outcomeorganizer.h.R", "outcomeorganizer.b.R"))
      suppressWarnings(suppressMessages(sys.source(.oo_src(f), envir = e)))
    env <<- e
    e
  }
})

oo_run <- function(df, ...) {
  e <- .oo_env()
  defaults <- list(outcomeLevel = NULL, recurrenceLevel = NULL,
                   dod = NULL, dooc = NULL, awd = NULL, awod = NULL)
  o <- do.call(e$outcomeorganizerOptions$new, utils::modifyList(defaults, list(...)))
  a <- e$outcomeorganizerClass$new(options = o, data = df)
  f <- tempfile(); sink(f); on.exit(sink(), add = TRUE)
  suppressWarnings(try(a$run(), silent = TRUE))
  a
}
warn_html <- function(a) paste(a$results$warnings$content, collapse = "")
strong_html <- function(a) paste(a$results$strongWarnings$content, collapse = "")

# ------------------------------------------------------------- few events --
test_that("few-events warning fires on a 2-level outcome that has missing values", {
  df <- data.frame(status = factor(c(rep("Dead", 3), rep("Alive", 17), NA),
                                   levels = c("Alive", "Dead")))
  a <- oo_run(df, outcome = "status", outcomeLevel = "Dead", analysistype = "os")
  w <- warn_html(a)
  expect_match(w, "only 3 of 20 non-missing outcomes", fixed = TRUE)
  expect_match(w, "(15%) are 'Dead'", fixed = TRUE)
  expect_match(w, "Kaplan-Meier", fixed = TRUE)
  expect_false(grepl("Logistic regression", w, fixed = TRUE))
})

test_that("few-events warning counts the SELECTED event level, not the rarest one", {
  # 3 Alive / 17 Dead with event = Dead: 17 events, no warning
  df <- data.frame(status = factor(c(rep("Alive", 3), rep("Dead", 17)),
                                   levels = c("Alive", "Dead")))
  a <- oo_run(df, outcome = "status", outcomeLevel = "Dead", analysistype = "os")
  expect_false(grepl("Few events", warn_html(a), fixed = TRUE))
})

# ---------------------------------------------------------- output gating --
test_that("addOutcome is populated by isNotFilled() gating, without the option set", {
  df <- data.frame(status = factor(rep(c("Dead", "Alive"), 10), levels = c("Alive", "Dead")))
  a <- oo_run(df, outcome = "status", outcomeLevel = "Dead", analysistype = "os")
  out <- a$results$addOutcome
  expect_false(out$isNotFilled())
  vals <- out$.__enclos_env__$private$.values[[1]]
  expect_equal(as.integer(vals), rep(c(1L, 0L), 10))
  expect_equal(length(out$.__enclos_env__$private$.rowNums), nrow(df))
})

# ----------------------------------------------------- duplicate warning --
test_that("rfs without a recurrence variable warns exactly once", {
  df <- data.frame(status = factor(rep(c("Dead", "Alive"), 15), levels = c("Alive", "Dead")))
  a <- oo_run(df, outcome = "status", outcomeLevel = "Dead", analysistype = "rfs")
  w <- warn_html(a)
  expect_equal(lengths(regmatches(w, gregexpr("recurrence/progression variable", w, fixed = TRUE))), 1L)
  expect_false(grepl("Currently only outcome is specified", w, fixed = TRUE))
})

# ------------------------------------------------------- n<10 severity --
test_that("n < 10 is a STRONG_WARNING", {
  df <- data.frame(status = factor(c("Dead", "Alive", "Dead", "Alive", "Dead", "Alive"),
                                   levels = c("Alive", "Dead")))
  a <- oo_run(df, outcome = "status", outcomeLevel = "Dead", analysistype = "os")
  expect_match(strong_html(a), "Very small sample size: 6 observations", fixed = TRUE)
  expect_false(grepl("Very small sample size", warn_html(a), fixed = TRUE))
})
