# Regressions for the 2026-10-21 independent review (PR-01..PR-05) and the cross-analysis XA-03.
.rfROC_run <- function(d, vars = "x", ...) {
  args <- utils::modifyList(list(data = d, dependentVars = vars, classVar = "y",
    positiveClass = "positive", negativeClass = "negative", plotROC = FALSE, run = TRUE),
    list(...), keep.null = TRUE)
  suppressMessages(suppressWarnings(do.call(ClinicoPath::psychopdaROC, args)))
}
.rfROC_notes <- function(table) paste(vapply(table$notes, function(n) n$note, ""), collapse = " | ")

.rfROC_cont <- function() {
  set.seed(4242)
  data.frame(y = factor(rep(c("negative", "positive"), each = 25)),
    x = round(c(rnorm(25, 0), rnorm(25, 1.3)), 6))
}

test_that("PR-01 a 'lower = disease' reference keeps its fitted risk model in NRI/IDI", {
  set.seed(20261021); y <- rep(0:1, each = 40)
  od <- data.frame(y = factor(ifelse(y == 1, "positive", "negative")),
    newm = rnorm(80, 1.2 * y), albumin = -rnorm(80, 1.2 * y))
  r <- .rfROC_run(od, "newm", calculateNRI = TRUE, calculateIDI = TRUE, refVar = "albumin",
    idiNriBootRuns = 200)
  # Pencina 2008/2011 on freely fitted one-marker logistic risks
  pn <- fitted(glm(y ~ od$newm, family = binomial()))
  pr <- fitted(glm(y ~ od$albumin, family = binomial()))
  idi <- (mean(pn[y == 1]) - mean(pn[y == 0])) - (mean(pr[y == 1]) - mean(pr[y == 0]))
  nri <- mean(sign(pn - pr)[y == 1]) - mean(sign(pn - pr)[y == 0])
  expect_equal(r$idiTable$asDF$idi, idi, tolerance = 1e-6)    # -0.089, was +0.216
  expect_equal(r$nriTable$asDF$nri, nri, tolerance = 1e-6)    # -0.40, was +0.70
  expect_lt(r$nriTable$asDF$nri, 0)
  for (tab in list(r$idiTable, r$nriTable)) {
    expect_match(.rfROC_notes(tab), "albumin: in these data the proportion of positive cases rises as the values fall", fixed = TRUE)
    expect_false(grepl("newm:", .rfROC_notes(tab), fixed = TRUE))
  }
  expect_true(r$idiTable$asDF$ci_lower <= idi && idi <= r$idiTable$asDF$ci_upper)
  expect_true(r$nriTable$asDF$ci_lower <= nri && nri <= r$nriTable$asDF$ci_upper)
})

test_that("raw_to_prob keeps a negative slope and falls back only for a constant marker", {
  y <- c(0, 0, 0, 1, 0, 1, 1, 1)
  x <- c(8, 7, 6, 5, 4, 3, 2, 1)
  p <- suppressWarnings(ClinicoPath:::raw_to_prob(x, y, ">="))
  expect_equal(as.numeric(p), unname(fitted(glm(y ~ x, family = binomial()))), tolerance = 1e-6)
  expect_equal(as.numeric(suppressWarnings(ClinicoPath:::raw_to_prob(rep(3, 8), y, ">="))), rep(0.5, 8))
})

test_that("PR-02 the tie note names the shown cut's J and the maximum J", {
  d <- data.frame(y = factor(rep(c("negative", "positive"), each = 6)),
    x = c(-2, 0, 1, 1, 3, 7, -1, 1, 2, 4, 4, 8))
  r <- .rfROC_run(d)
  t <- r$resultsTable$get("x")
  expect_equal(t$asDF$cutpoint, 3)
  expect_match(.rfROC_notes(t), "tie for the best youden (0.333): 2, 4. The cut-point shown, 3,", fixed = TRUE)
  expect_match(.rfROC_notes(t), "its youden is 0.167", fixed = TRUE)
})

test_that("PR-03 a fixed-sensitivity cut that misses its target says so", {
  d <- .rfROC_cont()
  lin <- .rfROC_run(d, fixedSensSpecAnalysis = TRUE, fixedSensitivityValue = 0.9)$fixedSensSpecTable
  ach <- lin$asDF$achieved_sensitivity
  expect_lt(ach, 0.9)
  expect_match(.rfROC_notes(lin), sprintf("x: the achieved sensitivity, %s, is below the target of 0.9.",
    signif(ach, 4)), fixed = TRUE)
  st <- .rfROC_run(d, fixedSensSpecAnalysis = TRUE, fixedSensitivityValue = 0.9,
    fixedInterpolation = "stepwise")$fixedSensSpecTable
  expect_gte(st$asDF$achieved_sensitivity, 0.9)
  expect_false(grepl("below the target", .rfROC_notes(st), fixed = TRUE))
})

test_that("PR-04 unit-cost metrics disclose that they reduce to accuracy", {
  d <- .rfROC_cont()
  for (m in c("misclassification_cost", "total_utility")) {
    t <- .rfROC_run(d, metric = m, costratioFP = 5)$resultsTable$get("x")
    expect_match(.rfROC_notes(t), "equal unit cost", fixed = TRUE)
  }
  expect_false(grepl("equal unit cost", .rfROC_notes(.rfROC_run(d)$resultsTable$get("x")), fixed = TRUE))
})

test_that("PR-05 and XA-03: AUC band agrees; a 0.0004 prior is not printed as 0.0%", {
  d <- .rfROC_cont(); d$x <- -d$x
  r <- .rfROC_run(d, usePriorPrev = TRUE, priorPrev = 0.0004)
  ci <- r$clinicalInterpretationTable$asDF
  expect_equal(ci$performance_level, "Below chance")
  expect_match(ci$interpretation_text, "indicating below chance discrimination", fixed = TRUE)
  n <- .rfROC_notes(r$resultsTable$get("x"))
  expect_match(n, "target prevalence (0.040%)", fixed = TRUE)
  expect_match(n, "At a prevalence of 0.040%", fixed = TRUE)
  expect_false(grepl("0.0%", n, fixed = TRUE))
})
