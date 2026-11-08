# Pre-release review of meddecide 1.1.3, 2026-11-07 (delta since 403e034). Each block fails on
# the code before that review's fix. Expected values are hand arithmetic in the comments.

rv_mk <- function(TP, FP, FN, TN)
  data.frame(test = factor(rep(c("pos", "pos", "neg", "neg"), c(TP, FP, FN, TN)), levels = c("pos", "neg")),
             ref  = factor(rep(c("dis", "non", "dis", "non"), c(TP, FP, FN, TN)), levels = c("dis", "non")))
rv_dec <- function(d, ...)
  suppressWarnings(decision(data = d, gold = "ref", goldPositive = "dis", goldNegative = "non",
                            newtest = "test", testPositive = "pos", testNegative = "neg", ...))
rv_text <- function(h) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(h, collapse = " "))))

test_that("decision: a Youden interval that includes 0 is not called poor or worse than chance", {
  # TP 0 FP 1 FN 10 TN 99: J = 0 - 1/100 = -0.010, Agresti-Caffo CI includes 0, so the notice
  # says "no evidence of discrimination, not as poor or as worse than chance".
  r <- rv_dec(rv_mk(0, 1, 10, 99), showNaturalLanguage = TRUE, showClinicalInterpretation = TRUE)
  summary <- rv_text(r$naturalLanguageSummary$content)
  interp <- rv_text(r$clinicalInterpretation$content)
  expect_true(nzchar(summary) && nzchar(interp))
  expect_match(rv_text(r$notices$content), "No evidence that this test discriminates", fixed = TRUE)
  expect_no_match(summary, "worse-than-chance", fixed = TRUE)
  expect_match(summary, "no statistically detectable discrimination", fixed = TRUE)
  expect_no_match(interp, "Worse than chance", fixed = TRUE)
  expect_match(interp, "No evidence of discrimination in this sample", fixed = TRUE)

  # n = 6 (2/1/1/2): J = 0.333 (would be "poor"), CI -0.41 to 0.81.
  r6 <- rv_dec(rv_mk(2, 1, 1, 2), showNaturalLanguage = TRUE)
  expect_no_match(rv_text(r6$naturalLanguageSummary$content), "shows poor discrimination", fixed = TRUE)

  # A clear-cut poor test (CI excludes 0) keeps its band: TP 60 FP 30 FN 40 TN 70, J 0.30.
  rp <- rv_dec(rv_mk(60, 30, 40, 70), showNaturalLanguage = TRUE)
  expect_match(rv_text(rp$naturalLanguageSummary$content), "shows poor discrimination", fixed = TRUE)
})

test_that("decision: the not-estimable LR reason does not claim Se or Sp is unestimable", {
  # TP 99 FP 10 FN 1 TN 0: Se 0.99 and Sp 0 are both estimable; LR- has TN = 0 and a 0.5
  # correction would put it below 1, so it is blank for that reason.
  r <- rv_dec(rv_mk(99, 10, 1, 0), showNaturalLanguage = TRUE)
  s <- rv_text(r$naturalLanguageSummary$content)
  expect_no_match(s, "one of those is not estimable", fixed = TRUE)
  expect_match(s, "true-negative count is zero", fixed = TRUE)
})

test_that("decision: an NPV within 0.05 points of 100% is not printed as 100.0%", {
  ns <- environment(decision)
  a <- ns$decisionClass$new(options = ns$decisionOptions$new(), data = data.frame())
  fmt <- a$.__enclos_env__$private$.fmtPct
  expect_identical(fmt(0.99975), "99.975%")
  expect_identical(fmt(0.999), "99.9%")
  expect_identical(fmt(0.9999999999), ">99.999%")
  expect_identical(fmt(1), "100.0%")
  expect_identical(fmt(0.004), "0.4%")
})

test_that("agreement: the positive category is matched under the default All-category estimates", {
  withr::local_seed(3)
  x <- sample(c("Neg", "Pos"), 80, TRUE, prob = c(.7, .3))
  y <- ifelse(stats::runif(80) < .85, x, ifelse(x == "Pos", "Neg", "Pos"))
  d <- data.frame(R1 = factor(x), R2 = factor(y))
  # Typed in other case with spaces; specificAllCategories is TRUE by default.
  t <- agreement_run(d, vars = c("R1", "R2"), specificAgreement = TRUE,
                     specificPositiveCategory = "  pos ")$results$specificAgreementGroup$specificAgreementTable
  expect_setequal(t$asDF$category, c("Pos (Positive)", "Neg (Negative)"))

  # Whitespace only is no category at all: plain per-category rows.
  t0 <- agreement_run(d, vars = c("R1", "R2"), specificAgreement = TRUE,
                      specificPositiveCategory = "   ")$results$specificAgreementGroup$specificAgreementTable
  expect_setequal(t0$asDF$category, c("Neg", "Pos"))

  # A category that matches nothing is reported under the default too (it was silently ignored).
  am <- agreement_run(d, vars = c("R1", "R2"), specificAgreement = TRUE,
                      specificPositiveCategory = "Malignnt")
  expect_equal(am$results$specificAgreementGroup$specificAgreementTable$rowCount, 0)
  notice <- am$results$items[["agreement_specificAgreementTable"]]
  expect_false(is.null(notice))
  expect_match(notice$content, "not found in data", fixed = TRUE)
})

test_that("agreement: Gwet's AC under unanimous ratings shows no SE-0 interval", {
  lv <- c("a", "b")
  v <- factor(rep(lv, 20), levels = lv)
  t <- agreement_run(data.frame(R1 = v, R2 = v), vars = c("R1", "R2"),
                     gwet = TRUE)$results$gwetGroup$gwetTable
  df <- t$asDF
  expect_equal(df$coefficient, 1)
  expect_true(is.na(df$ci_lower) && is.na(df$ci_upper))
  expect_match(agreement_notes(t)[["se0"]], "standard error is 0", fixed = TRUE)
})

test_that("decisioncalculator: F1 is exactly 0 with no true positive; an undefined PPV has no interval", {
  # TP 0 FP 1 FN 10 TN 99: F1 = 2*0 / (0 + 1 + 10) = 0.
  r <- decisioncalculator(TP = 0, FP = 1, FN = 10, TN = 99)
  expect_identical(r$advancedMetricsTable$asDF$f1Score, 0)
  # TP 0 FP 0 FN 10 TN 99: PPV = 0/0. epiR returns NaN with [0, 1]; the interval is blanked.
  r2 <- decisioncalculator(TP = 0, FP = 0, FN = 10, TN = 99, ci = TRUE)
  num <- r2$epirTable_ratio$asDF
  ppv <- num[gsub('^"|"$', "", rownames(num)) == "pv.pos", , drop = FALSE]
  expect_equal(nrow(ppv), 1)
  expect_true(is.na(ppv$est) && is.na(ppv$lower) && is.na(ppv$upper))
})

test_that("enhancedROC: CI text keeps 3 decimals, McClish below-diagonal sets a kept table note without R warning, and Unit of observation marks default", {
  withr::local_seed(42)
  y <- factor(rep(c("neg", "pos"), each = 40), levels = c("neg", "pos"))
  x <- c(stats::rnorm(40, 0, 1), stats::rnorm(40, 1.4, 1))
  x_inv <- -x
  d <- data.frame(y = y, x = x, x_inv = x_inv)

  res <- expect_no_warning(enhancedROC(
    data = d, outcome = "y", positiveClass = "pos", validationNegative = "neg",
    predictors = c("x", "x_inv"), direction = "higher",
    diagnosticMetrics = TRUE, partialAuc = TRUE,
    partialAucFrom = 0.8, partialAucTo = 1.0,
    showTableNotes = FALSE
  ))

  dp <- agreement_item(res, "diagnosticPerformance")$asDF
  expect_true(all(grepl("^\\([0-9]\\.[0-9]{3}, [0-9]\\.[0-9]{3}\\)$", dp$sensitivity_ci)))
  expect_true(all(grepl("^\\([0-9]\\.[0-9]{3}, [0-9]\\.[0-9]{3}\\)$", dp$specificity_ci)))

  pauc_tbl <- agreement_item(res, "partialAucAnalysis")
  expect_true("mcclish_below_diagonal" %in% names(pauc_tbl$notes))
  expect_match(pauc_tbl$notes$mcclish_below_diagonal$note, "McClish correction", fixed = TRUE)

  res_val <- enhancedROC(
    data = d, outcome = "y", positiveClass = "pos", validationNegative = "neg",
    predictors = "x", direction = "higher",
    validationPanel = TRUE, cohortRole = "development"
  )
  vc <- agreement_item(res_val, "validationContext")$asDF
  unit_row <- vc[vc$setting == "Unit of observation", , drop = FALSE]
  expect_equal(unit_row$value, "slide (default setting)")
})

test_that("psychopdaROC: fixedSensSpecTable missed_target_ note keys do not collide for A B vs A.B", {
  withr::local_seed(7)
  d <- data.frame(
    y = factor(rep(c("neg", "pos"), each = 10), levels = c("neg", "pos")),
    `A B` = c(1:10, 4:13),
    `A.B` = c(1:10, 5:14),
    check.names = FALSE
  )
  res <- psychopdaROC(
    data = d, dependentVars = c("A B", "A.B"),
    classVar = "y", positiveClass = "pos", negativeClass = "neg",
    fixedSensSpecAnalysis = TRUE, fixedAnalysisType = "sensitivity",
    fixedSensitivityValue = 0.95, fixedInterpolation = "linear"
  )
  note_keys <- names(res$fixedSensSpecTable$notes)
  expect_true("missed_target_A B" %in% note_keys)
  expect_true("missed_target_A.B" %in% note_keys)
})

test_that("agreement: hierarchicalKappa and mixedEffectsComparison emit one sub-analysis notice each when required variable is missing", {
  d <- data.frame(
    R1 = c(1, 2, 3, 4, 5, 6),
    R2 = c(1.1, 2.1, 2.9, 4.2, 5.0, 5.8)
  )
  a <- agreement_run(
    d, vars = c("R1", "R2"),
    hierarchicalKappa = TRUE, clusterSpecificKappa = TRUE,
    varianceDecomposition = TRUE, iccHierarchical = TRUE,
    testClusterHomogeneity = TRUE, mixedEffectsComparison = TRUE
  )
  notices <- Filter(function(it) inherits(it, "Notice") && isTRUE(it$visible), a$results$items)
  notice_names <- unname(vapply(notices, function(it) it$name, character(1)))
  expect_identical(
    sort(notice_names[startsWith(notice_names, "agreement_")]),
    c("agreement_hierarchicalOverallTable", "agreement_mixedEffectsTable")
  )
})

test_that("decision: comparatorCases notes truncation when discordant cases exceed maxCasesShow", {
  d <- rv_mk(10, 8, 7, 10)
  r <- rv_dec(d, referenceType = "method", showMisclassified = TRUE, maxCasesShow = 10)
  expect_equal(r$comparatorCases$rowCount, 10)
  expect_true("truncated" %in% names(r$comparatorCases$notes))
  expect_match(r$comparatorCases$notes$truncated$note, "Showing the first 10 of 15 discordant cases.", fixed = TRUE)
})

test_that("decisioncompare: originalCrossTab notes missing rows and cTable1-2 name the reference and test levels", {
  d <- data.frame(
    gold = factor(c(rep("pos", 15), rep("neg", 15)), levels = c("pos", "neg")),
    t1   = factor(c(rep("pos", 12), rep("neg", 3), NA, rep("neg", 14)), levels = c("pos", "neg")),
    t2   = factor(c(rep("pos", 10), rep("neg", 5), rep("pos", 2), rep("neg", 13)), levels = c("pos", "neg"))
  )
  r <- decisioncompare(
    data = d, gold = "gold", goldPositive = "pos", goldNegative = "neg",
    test1 = "t1", test1Positive = "pos", test1Negative = "neg",
    test2 = "t2", test2Positive = "pos", test2Negative = "neg",
    test3Positive = NULL, test3Negative = NULL, od = TRUE
  )
  expect_true("missing" %in% names(r$originalCrossTab$notes))
  expect_match(r$originalCrossTab$notes$missing$note, "t1: 1 of 30 rows", fixed = TRUE)
  expect_true("levels" %in% names(r$cTable1$notes))
  expect_match(r$cTable1$notes$levels$note, 'Reference Positive is "pos"', fixed = TRUE)
})

test_that("decisioncurve: benefitRangeTable flags when the benefit range touches the analyzed window boundary", {
  withr::local_seed(19)
  y <- factor(rep(c("no", "yes"), each = 50), levels = c("no", "yes"))
  p <- c(stats::runif(50, 0.02, 0.35), stats::runif(50, 0.65, 0.98))
  d <- data.frame(y = y, p = p)
  r <- decisioncurve(
    data = d, outcome = "y", outcomePositive = "yes",
    models = "p", decisionRulePositive = NULL, showBenefitRange = TRUE,
    thresholdRange = "custom",
    thresholdMin = 0.20, thresholdMax = 0.80, thresholdStep = 0.05
  )
  expect_true("window_edge" %in% names(r$benefitRangeTable$notes))
  expect_match(r$benefitRangeTable$notes$window_edge$note, "reaches the boundary of the analysed threshold window", fixed = TRUE)
})
