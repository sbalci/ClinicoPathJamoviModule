# Regression tests from /release-review-function singlearm (2026-09-02).
#
#   * the competing-risk median was read off the CIF with no support check, so a
#     single event landing on an emptied risk set at the last observed time was
#     reported as "median 202.8 months" beside "18% had the event"
#   * the user-facing notice layer was monolingual: 60 message sites now pass
#     through .(), which requires self$options$translate() to exist
#   * .assessDataQuality() never produced warnings, so the relay loop in .run()
#     was dead code

library(testthat)

sa_rr_args <- function(...) {
  defaults <- list(
    data = as.data.frame(get("singlearm_test")),
    elapsedtime = "time_months", outcome = "outcome", outcomeLevel = "Dead",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL
  )
  supplied <- list(...)
  defaults[names(supplied)] <- supplied
  defaults
}

sa_rr_compete <- function(d = as.data.frame(get("singlearm_compete")), ...) {
  args <- list(
    data = d, elapsedtime = "time_months", outcome = "outcome",
    outcomeLevel = NULL, multievent = TRUE, analysistype = "compete",
    dod = "Dead_Disease", dooc = "Dead_Other",
    awd = "Alive_Disease", awod = "Alive_NED"
  )
  supplied <- list(...)
  args[names(supplied)] <- supplied
  do.call(ClinicoPath::singlearm, args)
}

strip_html <- function(x)
  trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " "))))

# --- competing-risk median support guard ------------------------------------

test_that("a CIF crossing 50% only at the last observed time is not a median", {
  d <- as.data.frame(get("singlearm_compete"))
  r <- sa_rr_compete(d)

  # the crossing in this cohort is a single terminal jump on one subject
  st <- ifelse(d$outcome == "Dead_Disease", 1L,
        ifelse(d$outcome == "Dead_Other", 2L, 0L))
  cif <- cmprsk::cuminc(d$time_months, st, cencode = 0)[["1 1"]]
  j <- which(cif$est >= 0.5)[1]
  expect_equal(cif$time[j], max(d$time_months))       # crossing at the boundary
  expect_lt(cif$est[j - 1L], 0.5)                     # below 50% just before
  expect_equal(sum(d$time_months >= cif$time[j]), 1L) # one subject at risk

  expect_true(is.na(r$medianTable$asDF$median))
  msg <- strip_html(r$warnings$content)
  expect_match(msg, "only at the very last observed time")
  expect_match(msg, "1 subject\\(s\\) still at risk")
  # and the narrative must not assert a median
  expect_match(strip_html(r$medianSummary$content), "not reached")
})

test_that("a CIF median attained inside follow-up is still reported", {
  set.seed(7)
  d <- as.data.frame(get("singlearm_compete"))
  n <- nrow(d)
  d$time_months <- pmin(d$time_months, runif(n, 1, 60))
  d$outcome <- factor(
    ifelse(runif(n) < 0.75, "Dead_Disease",
    ifelse(runif(n) < 0.5, "Dead_Other", "Alive_NED")),
    levels = c("Alive_Disease", "Alive_NED", "Dead_Disease", "Dead_Other"))
  r <- sa_rr_compete(d)

  st <- ifelse(d$outcome == "Dead_Disease", 1L,
        ifelse(d$outcome == "Dead_Other", 2L, 0L))
  cif <- cmprsk::cuminc(d$time_months, st, cencode = 0)[["1 1"]]
  j <- which(cif$est >= 0.5)[1]
  expect_lt(cif$time[j], max(d$time_months))   # crossing is genuinely interior

  expect_equal(r$medianTable$asDF$median, cif$time[j])
  expect_false(grepl("only at the very last observed time",
                     strip_html(r$warnings$content)))
})

test_that("the Kaplan-Meier median path is untouched by the guard", {
  r <- do.call(ClinicoPath::singlearm, sa_rr_args())
  d <- as.data.frame(get("singlearm_test"))
  ref <- summary(survival::survfit(
    survival::Surv(d$time_months, as.integer(d$outcome == "Dead")) ~ 1))$table
  expect_equal(r$medianTable$asDF$median,   unname(ref[["median"]]))
  expect_equal(r$medianTable$asDF$x0_95lcl, unname(ref[["0.95LCL"]]))
})

# --- i18n -------------------------------------------------------------------

test_that("user-facing messages go through .() and still render in English", {
  src <- readLines(testthat::test_path("..", "..", "R", "singlearm.b.R"), warn = FALSE)
  calls <- grep("private\\$\\.(addInfo|addWarning|addError)\\(", src)
  unwrapped <- Filter(function(i) {
    chunk <- paste(src[i:min(i + 5, length(src))], collapse = "\n")
    !grepl("\\.\\(\\s*['\"]", substr(chunk, 1, 220))
  }, calls)
  # exactly three pass-throughs remain, and each is translated at its source
  # rather than at the call: user_msg (a .() switch in .safeExecute), the
  # role_errors vector (.() at assignment), and res$error from the shared
  # .defineEventIndicator() in survival_utils.R -- the only genuinely
  # untranslated one, tracked for a module-wide pass.
  expect_equal(length(unwrapped), 3L)

  # no paste0-glued fragments left inside a notice call
  glued <- Filter(function(i) grepl("paste0\\($", trimws(src[i])), calls)
  expect_identical(glued, integer(0))

  # and the messages still reach the user as English text. Plot parameters are
  # validated only when a plot is actually requested, which is why sc = TRUE
  # is needed here -- an invalid endplot with every plot switched off is
  # correctly silent.
  r <- do.call(ClinicoPath::singlearm, sa_rr_args(endplot = 0, sc = TRUE))
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 600, height = 400)
  invisible(try(r$plot$.render(width = 600, height = 400, ppi = 72), silent = TRUE))
  grDevices::dev.off()
  expect_match(strip_html(r$errors$content), "Plot end time must be a finite positive number")
})

test_that(".() works wherever the analysis actually calls it", {
  # every branch below emits at least one translated message; a missing
  # self$options$translate() would surface as "attempt to apply non-function"
  expect_silent(do.call(ClinicoPath::singlearm, sa_rr_args(cutp = "abc")))
  expect_silent(do.call(ClinicoPath::singlearm, sa_rr_args(person_time = TRUE)))
  expect_silent(do.call(ClinicoPath::singlearm, sa_rr_args(baseline_hazard = TRUE)))
  r <- do.call(ClinicoPath::singlearm, sa_rr_args(cutp = "abc, -5, 12"))
  expect_match(strip_html(r$warnings$content), "non-numeric value")
  expect_match(strip_html(r$warnings$content), "finite and zero or positive")
})

# --- dead code --------------------------------------------------------------

test_that(".assessDataQuality no longer advertises a warnings channel", {
  src <- paste(readLines(testthat::test_path("..", "..", "R", "singlearm.b.R"),
                         warn = FALSE), collapse = "\n")
  expect_false(grepl("data_quality\\$warnings", src))
  expect_false(grepl("warnings = warnings", src))
})
