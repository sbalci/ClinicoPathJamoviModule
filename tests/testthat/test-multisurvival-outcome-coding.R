# How multisurvival reads a numeric outcome (/fix-function, 2026-09-15).
#
# A numeric outcome coded 0/1 is read as 0 = censored, 1 = event. Any other coding
# -- notably 1/2, which means "2 = dead" in survival::lung but "1 = dead" in other
# exports -- is never guessed: the Event Level must name the event value, and values
# are matched exactly, not by size. The file-level validator used to refuse every
# non-0/1 numeric outcome even when the event value had been chosen.

.oc_quiet <- function(expr) {
  f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
.oc_lung <- function() {
  d <- survival::lung
  d$sex <- factor(d$sex, labels = c("Male", "Female"))
  d
}
.oc_complete <- function(d) d[stats::complete.cases(d[, c("time", "status", "sex", "age")]), ]
.oc_cox <- function(res) res$.__enclos_env__$private$.parent$.__enclos_env__$private$.coxCache
.oc_run <- function(data, level) {
  .oc_quiet(.run_multisurvival(data = data, elapsedtime = "time", outcome = "status",
                               outcomeLevel = level, explanatory = "sex", contexpl = "age"))
}

test_that("a 1/2 numeric outcome with Event Level 2 matches coxph on the same coding", {
  d <- .oc_lung()
  ref <- survival::coxph(survival::Surv(time, status == 2) ~ sex + age, data = .oc_complete(d))
  fit <- .oc_cox(.oc_run(d, "2"))
  expect_equal(fit$nevent, ref$nevent)
  expect_equal(unname(coef(fit)), unname(coef(ref)), tolerance = 1e-8)
})

test_that("values are matched exactly, not by size: Event Level 1 makes 1 the event", {
  d <- .oc_lung()
  res <- .oc_run(d, "1")
  expect_equal(.oc_cox(res)$nevent, sum(.oc_complete(d)$status == 1))
  expect_match(as.character(res$eventRecodeInfo$content), "matched exactly, not by size", fixed = TRUE)
})

test_that("a 1/2 numeric outcome without an Event Level is refused with instructions", {
  err <- tryCatch(.oc_run(.oc_lung(), NULL), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "does not guess which value is the event", fixed = TRUE)
  expect_match(conditionMessage(err), "Nominal in Data > Setup", fixed = TRUE)
})

test_that("a numeric 0/1 outcome needs no Event Level", {
  d <- .oc_lung()
  d$status <- d$status - 1
  expect_equal(.oc_cox(.oc_run(d, NULL))$nevent, sum(.oc_complete(d)$status == 1))
})
