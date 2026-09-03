# Regressions for the 2026-09 audit remediation of oddsratio.
#
# S1  .firthOrPlot() built its formula with a bare stats::as.formula(), the only
#     one of four formula sites that skipped the jmvcore::asFormula allow-list
#     (.asSurvivalFormula). It now routes through the guarded helper, with the
#     escaping done by jmvcore::constructFormula.
# N1  the "possible separation" 2x2 check ran BEFORE two-valued numeric
#     predictors were coerced to factors, so a 0/1 marker with an empty cell
#     got no warning. The coercion now precedes the check.
# M1  complete-case filtering uses jmvcore::naOmit(), which keeps the `label`
#     attribute base row-subsetting stripped.
# M2  the event-level INFO notice was emitted twice per run in two wordings.
#
# .firthOrPlot() is run as the REAL method taken off the R6 generator against a
# stub that records what reaches .asSurvivalFormula(); the other checks are
# source-level because they live inside the 900-line .run().

library(testthat)

.osrc <- function(file) {
  for (p in c(file.path("../../R", file), file.path("../R", file), file.path("R", file)))
    if (file.exists(p)) return(p)
  # R CMD check runs against the INSTALLED package, where R/*.b.R is gone.
  testthat::skip(paste0("R/", file, " not available (installed-package check)"))
}

.oddsratio_private <- function() {
  src <- .osrc("oddsratio.b.R")
  e <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(src, envir = e)))
  e$oddsratioClass$private_methods
}

test_that("S1: Firth forest-plot formula goes through the asFormula-guarded helper", {
  pm <- .oddsratio_private()

  rec <- new.env(parent = emptyenv())
  stub <- new.env(parent = globalenv())
  stub$.asSurvivalFormula <- function(x, env = parent.frame()) {
    rec$formula <- x
    stop("stop before logistf")          # short-circuit inside the method's tryCatch
  }
  stub$private <- list()
  stub$self <- list()
  f <- pm$.firthOrPlot
  environment(f) <- stub

  d <- data.frame(`out come` = factor(c(0, 1, 0, 1)), `x y` = 1:4, z = 4:1,
                  check.names = FALSE)
  res <- f(d, "out come", c("x y", "z"))

  expect_null(res)
  expect_identical(rec$formula, "`out come` ~ `x y` + z")
  # and the guarded helper accepts exactly what the method now hands it
  u <- new.env(parent = globalenv())
  suppressWarnings(suppressMessages(sys.source(.osrc("survival_utils.R"), envir = u)))
  expect_s3_class(u$.asSurvivalFormula(rec$formula), "formula")
})

test_that("S1: no bare stats::as.formula() remains in oddsratio.b.R", {
  src <- readLines(.osrc("oddsratio.b.R"), warn = FALSE)
  expect_false(any(grepl("as.formula(", src, fixed = TRUE)))
  # every logistf / lrm fit is fed a guarded formula
  expect_equal(sum(grepl(".asSurvivalFormula(", src, fixed = TRUE)), 4L)
})

test_that("N1: binary-numeric coercion precedes the separation check in .run()", {
  src <- readLines(.osrc("oddsratio.b.R"), warn = FALSE)
  coerce_line <- grep("length(unique(mydata[[v]])) == 2L", src, fixed = TRUE)
  sep_line    <- grep("Possible separation detected", src, fixed = TRUE)
  expect_length(coerce_line, 1L)
  expect_length(sep_line, 1L)
  expect_lt(coerce_line, sep_line)
})

test_that("M1: complete-case filtering uses jmvcore::naOmit and keeps labels", {
  src <- readLines(.osrc("oddsratio.b.R"), warn = FALSE)
  expect_false(any(grepl("complete.cases(mydata), ,", src, fixed = TRUE)))
  expect_false(any(grepl("complete.cases(diagnostic_data)", src, fixed = TRUE)))
  expect_equal(sum(grepl("jmvcore::naOmit(", src, fixed = TRUE)), 2L)

  d <- data.frame(a = c(1, NA, 3), b = c("x", "y", "z"))
  attr(d$a, "label") <- "Original A"
  expect_identical(attr(jmvcore::naOmit(d)$a, "label"), "Original A")
  expect_equal(nrow(jmvcore::naOmit(d)), 2L)
})

test_that("M2: the event level is announced once per run", {
  src <- readLines(.osrc("oddsratio.b.R"), warn = FALSE)
  expect_false(any(grepl("Outcome variable releveled", src, fixed = TRUE)))
  expect_equal(sum(grepl("Outcome level modeled as the event", src, fixed = TRUE)), 1L)
})
