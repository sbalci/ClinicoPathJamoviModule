# Release fixes for decision, 2026-10-21 (development-ideas/decision-independent-review-2026-10-21.md
# D1-D4, D7; meddecide-cross-analysis-review-2026-10-21.md XA-01, XA-03). Expected values are hand
# arithmetic in the comments.

rf_mk <- function(TP, FP, FN, TN, tl = c("pos", "neg"), gl = c("dis", "non"))
  data.frame(test = factor(rep(c(tl[1], tl[1], tl[2], tl[2]), c(TP, FP, FN, TN)), levels = tl),
             ref  = factor(rep(c(gl[1], gl[2], gl[1], gl[2]), c(TP, FP, FN, TN)), levels = gl))
rf_run <- function(d, gn = NULL, tn = NULL, ...)
  suppressWarnings(decision(data = d, gold = "ref", goldPositive = "dis", newtest = "test",
                            testPositive = "pos", goldNegative = gn, testNegative = tn, ...))
rf_text <- function(h) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(h, collapse = " "))))
rf_key <- function(df, key) df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
rf_blank <- function(x) is.na(x) || identical(as.character(x), "")

test_that("XA-01 TP = 0: LR+ and DOR are exactly 0, uncorrected, with [0, U] intervals", {
  # TP 0, FN 10, FP 1, TN 99. LR+ = 0 / (1/100) = 0; DOR = 0*99 / (1*10) = 0.
  # LR- = (10/10) / (99/100) = 1.0101 (observed). The old blanket correction printed 3.06 / 3.16.
  r <- rf_run(rf_mk(0, 1, 10, 99), ci = TRUE, fagan = TRUE)
  expect_identical(r$ratioTable$asDF$LRP, 0)
  expect_equal(r$ratioTable$asDF$LRN, 1 / 0.99, tolerance = 1e-12)
  num <- r$epirTable_number$asDF
  expect_identical(rf_key(num, "lr.pos")$est, 0)
  expect_identical(rf_key(num, "diag.or")$est, 0)
  # Updated 2026-10-21: a zero ratio's interval is [0, U] (U from the +0.5 table), no longer blank.
  expect_identical(rf_key(num, "lr.pos")$lower, 0)
  expect_identical(rf_key(num, "diag.or")$lower, 0)
  expect_true(rf_key(num, "lr.pos")$upper > 0 && rf_key(num, "diag.or")$upper > 0)
  expect_match(r$epirTable_number$notes$zero_ratio$note, "runs from 0 to the upper limit", fixed = TRUE)
  expect_match(r$ratioTable$notes$zero_ratio$note, "exactly 0", fixed = TRUE)
  # Nothing is corrected, so no correction note or notice.
  expect_null(r$ratioTable$notes[["continuity"]])
  notices <- rf_text(r$notices$content)
  expect_no_match(notices, "Continuity correction applied", fixed = TRUE)
  expect_match(notices, "cannot be placed on the nomogram", fixed = TRUE)
  expect_false(isTRUE(r$plot1$state$drawable))
})

test_that("XA-01 FN = 0: LR- is exactly 0; the DOR (zero denominator) is corrected", {
  # TP 10, FP 5, FN 0, TN 95. LR+ = 1 / (5/100) = 20 (observed). LR- = 0.
  # DOR corrected = 10.5 * 95.5 / (5.5 * 0.5) = 364.64.
  r <- rf_run(rf_mk(10, 5, 0, 95), ci = TRUE)
  expect_equal(r$ratioTable$asDF$LRP, 20, tolerance = 1e-12)
  expect_identical(r$ratioTable$asDF$LRN, 0)
  num <- r$epirTable_number$asDF
  expect_identical(rf_key(num, "lr.neg")$est, 0)
  expect_identical(rf_key(num, "lr.neg")$lower, 0)   # was blank; now [0, U] (2026-10-21)
  expect_equal(rf_key(num, "diag.or")$est, 10.5 * 95.5 / (5.5 * 0.5), tolerance = 1e-9)
  expect_match(r$epirTable_number$notes$continuity$note, "need not equal LR+ / LR-", fixed = TRUE)
})

test_that("XA-01 FP = 0: LR+ (zero denominator) is corrected, LR- stays observed", {
  # TP 8, FP 0, FN 2, TN 90. LR+ = (8.5/11) / (0.5/91) = 140.64; LR- = (2/10) / 1 = 0.2.
  r <- rf_run(rf_mk(8, 0, 2, 90), ci = TRUE)
  expect_equal(r$ratioTable$asDF$LRP, (8.5 / 11) / (0.5 / 91), tolerance = 1e-12)
  expect_equal(r$ratioTable$asDF$LRN, 0.2, tolerance = 1e-12)
  expect_match(r$ratioTable$notes$continuity$note, "denominator count is zero", fixed = TRUE)
  expect_null(r$ratioTable$notes[["zero_ratio"]])
  expect_match(rf_text(r$notices$content), "Continuity correction applied", fixed = TRUE)
})

test_that("XA-01 a zero-denominator ratio whose correction would not stay above 1 is blank, not estimable", {
  # TP 1, FP 0, FN 99, TN 1. LR+ = (1/100)/0 is infinite; corrected (1.5/101)/(0.5/2) = 0.059 < 1.
  # DOR = 1*1/(0*99) is infinite; corrected 1.5*1.5/(0.5*99.5) = 0.045 < 1. Both "undefined".
  # LR- = (99/100)/(1/1) = 0.99 (observed).
  r <- rf_run(rf_mk(1, 0, 99, 1), ci = TRUE, fagan = TRUE)
  expect_true(rf_blank(r$ratioTable$asDF$LRP))
  expect_equal(r$ratioTable$asDF$LRN, 0.99, tolerance = 1e-12)
  num <- r$epirTable_number$asDF
  expect_true(rf_blank(rf_key(num, "lr.pos")$est) && rf_blank(rf_key(num, "lr.pos")$lower))
  expect_true(rf_blank(rf_key(num, "diag.or")$est) && rf_blank(rf_key(num, "diag.or")$upper))
  expect_match(r$ratioTable$notes$undefined_ratio$note, "not estimable", fixed = TRUE)
  expect_match(r$epirTable_number$notes$undefined_ratio$note, "not estimable", fixed = TRUE)
  expect_match(rf_text(r$notices$content), "cannot be placed on the nomogram", fixed = TRUE)
  expect_false(isTRUE(r$plot1$state$drawable))
})

test_that("D1/D7 a two-level variable infers its negative and the Reference definition names it", {
  r <- rf_run(rf_mk(30, 5, 10, 55))
  rules <- rf_key(r$referenceDefinition$asDF, "Positive and negative rules")$value
  expect_match(rules, "negative non (inferred: the only other level)", fixed = TRUE)
  expect_match(rules, "negative neg (inferred: the only other level)", fixed = TRUE)
  chosen <- rf_run(rf_mk(30, 5, 10, 55), gn = "non", tn = "neg")
  expect_no_match(rf_key(chosen$referenceDefinition$asDF, "Positive and negative rules")$value,
                  "inferred", fixed = TRUE)
})

test_that("D1 three levels with no negative chosen wait (INFO, nothing computed); positive == negative is refused", {
  d <- rf_mk(30, 5, 10, 55)
  d$ref <- factor(as.character(d$ref), levels = c("dis", "non", "unclear"))
  d$ref[1:3] <- "unclear"
  # An empty negative box is a selection in progress (allowNone starts it empty), so it is a
  # pending INFO like an unset positive level, never a red error before setup is finished.
  r <- rf_run(d)
  expect_match(rf_text(r$notices$content), "cannot be inferred", fixed = TRUE)
  expect_true(all(is.na(r$ratioTable$asDF[, c("Sens", "Spec", "LRP", "LRN")])))
  expect_error(rf_run(rf_mk(30, 5, 10, 55), gn = "dis"),
               "Disease-present and disease-absent levels cannot be the same", fixed = TRUE)
})

test_that("D1 both negative-level options allow None (no client preselection of the first level)", {
  path <- testthat::test_path("..", "..", "jamovi", "decision.a.yaml")
  skip_if_not(file.exists(path))
  opts <- yaml::read_yaml(path)$options
  for (nm in c("goldNegative", "testNegative"))
    expect_true(isTRUE(Filter(function(o) identical(o$name, nm), opts)[[1]]$allowNone), info = nm)
})

test_that("XA-03 a rare prior is printed with two significant figures, not 0.0%", {
  r <- rf_run(rf_mk(80, 10, 20, 90), pp = TRUE, pprob = 0.0004, showReportTemplate = TRUE)
  expect_match(r$ratioTable$notes$prior_ppv$note, "population prior of 0.04%", fixed = TRUE)
  expect_no_match(rf_text(r$reportTemplate$content), "prevalence of 0.0%", fixed = TRUE)
  r2 <- rf_run(rf_mk(80, 10, 20, 90), pp = TRUE, pprob = 0.0014)
  expect_match(r2$ratioTable$notes$prior_ppv$note, "population prior of 0.14%", fixed = TRUE)
  r3 <- rf_run(rf_mk(80, 10, 20, 90), pp = TRUE, pprob = 0.25)
  expect_match(r3$ratioTable$notes$prior_ppv$note, "population prior of 25.0%", fixed = TRUE)
})

test_that("M1 an empty notices panel is cleared with <div></div>, never \"\"", {
  ns <- environment(decision)
  opts <- ns$decisionOptions$new(gold = "ref", goldPositive = "dis", newtest = "test",
                                 testPositive = "pos", goldNegative = NULL, testNegative = NULL)
  a <- ns$decisionClass$new(data = rf_mk(30, 5, 10, 55), options = opts)
  a$init()
  priv <- a$.__enclos_env__$private
  priv$.noticeList <- list()
  priv$.renderNotices()
  expect_identical(a$results$notices$content, "<div></div>")
})

test_that("D3/D4 comparator mode: no prior-adjusted CI warning and no disease wording", {
  d <- rf_mk(30, 5, 10, 55)
  d$test[1] <- NA
  r <- rf_run(d, referenceType = "method", pp = TRUE, pprob = 0.1, ci = TRUE)
  notices <- rf_text(r$notices$content)
  expect_no_match(notices, "Confidence Intervals Interpretation", fixed = TRUE)
  expect_match(notices, "have both a test and a reference value", fixed = TRUE)
  expect_no_match(notices, "gold-standard value", fixed = TRUE)
})

# Upper limit of a zero ratio's interval, by hand: the analysis's own log-scale interval (Simel for
# the LRs, Woolf for the DOR) on the table with 0.5 added to every cell. Lower limit 0, estimate 0.
rf_zero_upper <- function(ratio, TP, FP, FN, TN) {
  a <- TP + .5; b <- FP + .5; c <- FN + .5; d <- TN + .5; z <- qnorm(.975)
  switch(ratio,
    lr.pos = exp(log((a / (a + c)) / (b / (b + d))) + z * sqrt(1 / a - 1 / (TP + FN + 1) + 1 / b - 1 / (FP + TN + 1))),
    lr.neg = exp(log((c / (a + c)) / (d / (b + d))) + z * sqrt(1 / c - 1 / (TP + FN + 1) + 1 / d - 1 / (FP + TN + 1))),
    diag.or = exp(log(a * d / (b * c)) + z * sqrt(1 / a + 1 / b + 1 / c + 1 / d)))
}

test_that("zero ratio interval is [0, U], U on the 0.5-corrected table (hand oracle, 2026-10-21)", {
  # TP 50 FP 5 FN 0 TN 45: LR- = 0; corrected LR- = (0.5/51)/(45.5/51) = 0.010989,
  # var = 1/0.5 - 1/51 + 1/45.5 - 1/51 = 1.98276, U = exp(log(0.010989) + 1.96 * 1.40810) = 0.17360.
  num <- rf_run(rf_mk(50, 5, 0, 45), ci = TRUE)$epirTable_number$asDF
  row <- rf_key(num, "lr.neg")
  expect_identical(c(row$est, row$lower), c(0, 0))
  expect_equal(row$upper, rf_zero_upper("lr.neg", 50, 5, 0, 45), tolerance = 1e-10)
  # TP 0 FP 5 FN 10 TN 85: LR+ = 0 and DOR = 0.
  num <- rf_run(rf_mk(0, 5, 10, 85), ci = TRUE)$epirTable_number$asDF
  for (k in c("lr.pos", "diag.or")) {
    row <- rf_key(num, k)
    expect_identical(c(row$est, row$lower), c(0, 0))
    expect_equal(row$upper, rf_zero_upper(k, 0, 5, 10, 85), tolerance = 1e-10)
  }
})
