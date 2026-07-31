# Regressions for the zero-event cohort and estimand-naming fixes.
#
# L2  a valid fully censored cohort was REJECTED on the factor path while the
#     numeric path accepted it, and competing-risk data with no event of
#     interest left the median section and the clinical summary blank
# L3  disease-free survival's timing assumption was never stated
# L4  the completion notice's counts did not sum to the total
#
# The helpers are exercised as the REAL functions -- resolved from the package
# namespace when it is loaded, otherwise sourced straight out of R/ -- so these
# fail if the source changes. Nothing here re-implements them, and every
# assertion runs unconditionally: an assertion hidden inside an if() on the very
# wording under change is how a test in this directory recorded as an empty pass
# for weeks.

library(testthat)

.sa_src <- function(file) {
  for (p in c(file.path("../../R", file), file.path("../R", file), file.path("R", file)))
    if (file.exists(p)) return(p)
  NULL
}

.sa_utils <- local({
  env <- NULL
  function() {
    if (!is.null(env)) return(env)
    pkg <- intersect(c("ClinicoPath", "jsurvival"), loadedNamespaces())
    for (p in pkg) {
      ns <- asNamespace(p)
      if (exists(".defineEventIndicator", envir = ns, inherits = FALSE)) {
        env <<- ns
        return(env)
      }
    }
    src <- .sa_src("survival_utils.R")
    if (is.null(src)) return(NULL)
    e <- new.env(parent = globalenv())
    suppressWarnings(suppressMessages(sys.source(src, envir = e)))
    env <<- e
    env
  }
})

define   <- function(...) get(".defineEventIndicator",   envir = .sa_utils())(...)
describe <- function(...) get(".describeEventIndicator", envir = .sa_utils())(...)


# L2 -- a fully censored cohort is a result, not an error --------------------

test_that("L2: a declared but unobserved event level is ACCEPTED as a zero-event cohort", {
  # BEFORE: error "does not occur in the data" -- a legitimate analysis refused.
  x <- factor(c("Alive", "Alive", "Alive"), levels = c("Alive", "Dead"))
  r <- define(x, outcomeLevel = "Dead", outcome_name = "Status")

  expect_null(r$error)
  expect_equal(r$status, c(0L, 0L, 0L))
  expect_equal(r$n_event, 0)
  expect_equal(r$n_censored, 3)
  expect_equal(r$event_label, "Dead")
})

test_that("L2: an event level that is not a level at all is still REJECTED", {
  x <- factor(c("Alive", "Alive"), levels = c("Alive", "Dead"))
  r <- define(x, outcomeLevel = "Deceased", outcome_name = "Status")
  expect_match(r$error, "is not a level of")
  expect_match(r$error, "Alive, Dead")   # tells the user what is available
  expect_null(r$status)
})

test_that("L2: a character outcome has no unused levels, so an absent value is rejected", {
  # A character column carries no declared level set; its observed values are
  # the only levels it has. Accepting an absent value there would silently
  # censor everybody with nothing to check the selection against.
  r <- define(c("Alive", "Alive"), outcomeLevel = "Dead", outcome_name = "Status")
  expect_match(r$error, "is not a level of")
})

test_that("L2: the factor and numeric paths agree on a zero-event cohort", {
  fac <- define(factor(c("Alive", "Alive"), levels = c("Alive", "Dead")),
                outcomeLevel = "Dead")
  num <- define(c(0, 0))
  expect_null(fac$error)
  expect_null(num$error)
  expect_equal(fac$n_event, num$n_event)
  expect_equal(fac$n_event, 0)
})

test_that("L2: a zero-event recode is disclosed loudly, naming the wrong-level risk", {
  # This block is what replaced the rejection. If it goes, the rejection has to
  # come back with it.
  html <- describe(define(factor(c("Alive", "Alive"), levels = c("Alive", "Dead")),
                          outcomeLevel = "Dead", outcome_name = "Status"),
                   "Status")
  expect_match(html, "No events")
  expect_match(html, "wrong event level is selected")
  expect_match(html, "Dead")
  expect_match(html, "fully censored")
})

test_that("L2: a cohort WITH events gets no zero-event block", {
  # The input the change could newly damage: an ordinary analysis must not
  # acquire a red no-events banner.
  html <- describe(define(factor(c("Alive", "Dead", "Dead")), outcomeLevel = "Dead",
                          outcome_name = "Status"),
                   "Status")
  expect_false(grepl("No events", html, fixed = TRUE))
  expect_false(grepl("wrong event level", html, fixed = TRUE))
})

test_that("L2: an already-observed event level is unaffected by the declared-level check", {
  x <- factor(c("Alive", "Dead", "Alive", "Dead"))
  r <- define(x, outcomeLevel = "Dead")
  expect_null(r$error)
  expect_equal(r$status, c(0L, 1L, 0L, 1L))
  expect_equal(r$n_event, 2)
})


# L3 -- the DFS timing assumption --------------------------------------------

test_that("L3: choosing disease-free survival states the time-variable requirement", {
  r <- define(factor(c("DOD", "DOOC", "AWD", "AWOD")), multievent = TRUE,
              analysistype = "dfs", dod = "DOD", dooc = "DOOC",
              awd = "AWD", awod = "AWOD", outcome_name = "Status")
  expect_equal(r$estimand, "disease-free survival")

  html <- describe(r, "Status")
  expect_match(html, "Disease-free survival requires a time to the DFS event")
  expect_match(html, "recurrence")
  expect_match(html, "LAST FOLLOW-UP")
  expect_match(html, "biased upward|over-estimated")
})

test_that("L3: overall survival gets no DFS disclosure", {
  r <- define(factor(c("DOD", "DOOC", "AWD", "AWOD")), multievent = TRUE,
              analysistype = "overall", dod = "DOD", dooc = "DOOC",
              awd = "AWD", awod = "AWOD", outcome_name = "Status")
  expect_false(grepl("Disease-free survival requires", describe(r, "Status"), fixed = TRUE))
})


# L4 -- the counts reconcile --------------------------------------------------

test_that("L4: event / competing / censored counts sum to the analysed total", {
  r <- define(factor(c("DOD", "DOOC", "AWD", "AWOD", "AWOD")), multievent = TRUE,
              analysistype = "compete", dod = "DOD", dooc = "DOOC",
              awd = "AWD", awod = "AWOD", outcome_name = "Status")
  expect_equal(r$n_event + r$n_competing + r$n_censored + r$n_missing, 5)
  expect_equal(r$n_competing, 1)
  # The notice prints n_total - n_events - n_censored as the competing count;
  # that identity is what makes the three numbers add up.
  n_total <- r$n_event + r$n_competing + r$n_censored
  expect_equal(n_total - r$n_event - r$n_censored, r$n_competing)
})

test_that("L4: the completion notice reports competing events", {
  src <- .sa_src("singlearm.b.R")
  expect_false(is.null(src))
  code <- paste(readLines(src, warn = FALSE), collapse = "\n")
  expect_match(code, "competing event\\(s\\), %d censored")
  expect_match(code, "n_competing_total <- data_quality\\$n_total")
})
