# Regression tests from /validate-function decisioncalculator depth=exhaustive (2026-10-06).
# Report: development-ideas/decisioncalculator-validation-2026-10-06.md
# Evidence script: development-scripts/validate_decisioncalculator.R (exact-enumeration coverage included).
#
# Live tests lock in behaviour verified against an independent oracle. The VAL-decisioncalculator-NN
# tests were written skip-guarded with the correct expectation and went live with the fix (2026-10-07).

skip_if_not_installed("epiR")

val_dc <- function(...) suppressWarnings(ClinicoPath::decisioncalculator(...))
val_key <- function(df, key) {
  m <- df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
  stopifnot(nrow(m) == 1L)
  m
}
val_ci <- function(r, key) {
  both <- rbind(r$epirTable_ratio$asDF, r$epirTable_number$asDF)
  unlist(val_key(both, key)[, c("est", "lower", "upper")])
}
val_strip <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(x, collapse = " "))))
val_notices <- function(r) paste(r$notices$content, collapse = " ")
val_cp <- function(x, n) as.numeric(stats::binom.test(x, n)$conf.int)
val_z <- stats::qnorm(0.975)

# ---------------------------------------------------------------------------------- live --

test_that("C11-C25 known answer: TP 9, FP 2, FN 1, TN 8", {
  # Oracle: hand fractions. LR+ .9/.2, LR- .1/.8, DOR 72/2, F1 = 2TP/(2TP+FP+FN), MCC = 70/sqrt(9900)
  r <- val_dc(TP = 9, FP = 2, FN = 1, TN = 8)
  rt <- val_key(r$ratioTable$asDF, "1")
  expect_equal(unlist(rt[, c("Sens", "Spec", "AccurT", "PrevalenceD", "PPV", "NPV", "PostTestProbDisease",
                              "PostTestProbNeg", "LRP", "LRN")]),
               c(9/10, 8/10, 17/20, 10/20, 9/11, 8/9, 9/11, 1/9, 4.5, 0.125),
               tolerance = 1e-10, ignore_attr = TRUE)
  am <- val_key(r$advancedMetricsTable$asDF, "1")
  expect_equal(unlist(am[, c("youdenIndex", "balancedAccuracy", "f1Score", "mcc", "dor")]),
               c(0.7, 0.85, 18/21, 70/sqrt(9900), 36), tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("C26-C38 proportion intervals are Clopper-Pearson at 95%", {
  # Oracle: base-R binom.test (independent of epiR, which the analysis calls)
  r <- val_dc(TP = 9, FP = 2, FN = 1, TN = 8, ci = TRUE)
  rows <- list(ap = c(11, 20), tp = c(10, 20), se = c(9, 10), sp = c(8, 10), diag.ac = c(17, 20),
               pv.pos = c(9, 11), pv.neg = c(8, 9), p.rout = c(9, 20), p.rin = c(11, 20),
               p.tpdn = c(2, 10), p.tndp = c(1, 10), p.dntp = c(2, 11), p.dptn = c(1, 9))
  for (k in names(rows))
    expect_equal(val_ci(r, k), c(rows[[k]][1] / rows[[k]][2], val_cp(rows[[k]][1], rows[[k]][2])),
                 tolerance = 1e-10, ignore_attr = TRUE, info = k)
})

test_that("C41-C43 LR and DOR intervals use the log method on raw counts", {
  # Oracle: Simel 1991 var(log LR+) = 1/TP - 1/n1 + 1/FP - 1/n0; Woolf SE sqrt(sum 1/cell)
  r <- val_dc(TP = 9, FP = 2, FN = 1, TN = 8, ci = TRUE)
  expect_equal(val_ci(r, "lr.pos"), c(4.5, exp(log(4.5) + c(-1, 1) * val_z * sqrt(1/9 - 1/10 + 1/2 - 1/10))),
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(val_ci(r, "lr.neg"), c(0.125, exp(log(0.125) + c(-1, 1) * val_z * sqrt(1 - 1/10 + 1/8 - 1/10))),
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(val_ci(r, "diag.or"), c(36, exp(log(36) + c(-1, 1) * val_z * sqrt(1/9 + 1/2 + 1 + 1/8))),
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("C15-C17 a population prevalence moves PPV/NPV by Bayes and leaves Sens/Spec", {
  # Oracle: Bayes' theorem with sens 90/110, spec 80/110 at p = 0.05
  se <- 90/110; sp <- 80/110; p <- 0.05
  rt <- val_key(val_dc(pp = TRUE, pprob = p)$ratioTable$asDF, "1")
  expect_equal(c(rt$PPV, rt$NPV, rt$PostTestProbDisease, rt$PrevalenceD, rt$Sens),
               c(se * p / (se * p + (1 - sp) * (1 - p)), sp * (1 - p) / (sp * (1 - p) + (1 - se) * p),
                 se * p / (se * p + (1 - sp) * (1 - p)), p, se), tolerance = 1e-12)
})

test_that("C19-C25 a zero cell applies Haldane-Anscombe to LR and DOR points only", {
  # Oracle: the notice's own rule, +0.5 to all four cells; Sens/Spec stay raw
  r <- val_dc(TP = 10, FP = 0, FN = 10, TN = 10, ci = TRUE)
  rt <- val_key(r$ratioTable$asDF, "1")
  expect_equal(c(rt$Sens, rt$Spec, rt$LRP, rt$LRN), c(0.5, 1, (10.5/21) / (0.5/11), (10.5/21) / (10.5/11)), tolerance = 1e-12)
  expect_equal(val_key(r$advancedMetricsTable$asDF, "1")$dor, 10.5 * 10.5 / (0.5 * 10.5), tolerance = 1e-12)
  # the interval table shows the same corrected LR+; FP = 0 leaves its upper limit unbounded (blank)
  lp <- val_ci(r, "lr.pos")
  expect_equal(lp[["est"]], rt$LRP, tolerance = 1e-12)
  expect_true(is.finite(lp[["lower"]]) && is.na(lp[["upper"]]))
  expect_match(r$epirTable_number$notes$rawcounts$note, "0.5 added to every cell", fixed = TRUE)
})

test_that("C56-C62 cut-off comparison rows and the overlap sentence", {
  # Oracle: hand fractions; Agresti-Caffo J intervals from DescTools (independent implementation)
  skip_if_not_installed("DescTools")
  mc <- val_dc(multiplecuts = TRUE)$multipleCutoffTable$asDF
  expect_equal(unlist(val_key(mc, "2")[, c("sensitivity", "specificity", "ppv", "npv", "accuracy", "youden")]),
               c(80/110, 95/110, 80/95, 95/125, 175/220, 80/110 + 95/110 - 1), tolerance = 1e-12, ignore_attr = TRUE)
  j_cur <- DescTools::BinomDiffCI(90, 110, 30, 110, method = "ac")[1, ]
  j_s2  <- DescTools::BinomDiffCI(80, 110, 15, 110, method = "ac")[1, ]
  expect_true(j_s2[["lwr.ci"]] <= j_cur[["upr.ci"]] && j_cur[["lwr.ci"]] <= j_s2[["upr.ci"]])
  expect_identical(val_key(mc, "3")$recommendation,
                   "Higher specificity example has the highest Youden's J, 0.045 above current; its Youden's J interval overlaps the current cut-off's, so these separate intervals do not show a difference.")
})

test_that("C64-C75 workload rows reproduce the study table and project by Bayes", {
  # Oracle: tp = M q sens, fp = M (1 - q)(1 - spec) at M = 1000
  wl <- val_dc(showWorkload = TRUE, pp = TRUE, pprob = 0.05)$screeningWorkload$asDF
  expect_equal(unlist(val_key(wl, "1")[, c("tp", "fp", "fn", "tn", "ppv", "npv")]), c(90, 30, 20, 80, 0.75, 0.8),
               tolerance = 1e-12, ignore_attr = TRUE)
  se <- 90/110; sp <- 80/110; tp <- 1000 * 0.05 * se; fp <- 1000 * 0.95 * (1 - sp)
  expect_equal(unlist(val_key(wl, "2")[, c("tp", "fp", "flags", "ppv", "flags_per_detected", "tests_per_detected")]),
               c(tp, fp, tp + fp, tp / (tp + fp), (tp + fp) / tp, 1000 / tp), tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("C11-C25 metamorphic: swapping test and reference positive levels", {
  # Oracle: sens <-> spec, PPV <-> NPV, LR+ -> 1/LR-; DOR and J unchanged
  a <- val_dc(TP = 47, FP = 3, FN = 13, TN = 137); b <- val_dc(TP = 137, FP = 13, FN = 3, TN = 47)
  ra <- val_key(a$ratioTable$asDF, "1"); rb <- val_key(b$ratioTable$asDF, "1")
  expect_equal(c(rb$Sens, rb$Spec, rb$PPV, rb$NPV, rb$LRP), c(ra$Spec, ra$Sens, ra$NPV, ra$PPV, 1 / ra$LRN), tolerance = 1e-12)
  expect_equal(val_key(b$advancedMetricsTable$asDF, "1")$dor, val_key(a$advancedMetricsTable$asDF, "1")$dor, tolerance = 1e-12)
})

test_that("C83-C84 guard thresholds fire exactly where stated", {
  sparse <- function(d) grepl("Very sparse reference groups", val_notices(val_dc(TP = d - 3, FN = 3, TN = 40, FP = 10)), fixed = TRUE)
  expect_identical(c(sparse(9), sparse(10), sparse(11)), c(TRUE, FALSE, FALSE))
  ext <- function(p) grepl("Extreme prevalence", val_notices(val_dc(pp = TRUE, pprob = p)), fixed = TRUE)
  expect_identical(vapply(c(0.0499, 0.05, 0.95, 0.9501), ext, TRUE), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("C47-C49 nomogram reading sentences equal the table", {
  fs <- val_strip(val_dc(fagan = TRUE)$faganSummary$content)
  expect_match(fs, "Pre-test probability is 50.0%, using the prevalence observed in this study table.", fixed = TRUE)
  expect_match(fs, "A positive result has a likelihood ratio of 3.00 and raises the probability from 50.0% to 75.0%.", fixed = TRUE)
  expect_match(fs, "A negative result has a likelihood ratio of 0.250 and lowers the probability from 50.0% to 20.0%.", fixed = TRUE)
})

test_that("C53 nomogram lines pass through their likelihood-ratio ticks", {
  # Oracle: Bayes in log10 odds; the LR = 1 tick fixes the middle-axis offset
  st <- val_dc(TP = 50, FN = 50, TN = 90, FP = 10, fagan = TRUE)$plot1$state
  p <- ClinicoPath:::nomogrammer(Prevalence = st$Prevalence, Sens = st$Sens, Spec = st$Spec, Detail = TRUE, NullLine = TRUE)
  b <- ggplot2::ggplot_build(p)$data
  ln <- b[[1]]; tk <- b[[3]]; lr <- as.numeric(as.character(tk$label))
  red <- ln$y[ln$colour == "red"]
  expect_equal(mean(red), tk$y[lr == 5], tolerance = 1e-10)
  expect_equal(ln$y[ln$colour == "red" & ln$x == 1], log10(5), tolerance = 1e-10)
})

# --------------------------------- defects fixed by /fix-function 2026-10-07, now live --

test_that("C45 VAL-decisioncalculator-01: NNDx interval contains its estimate and NNDx is never negative", {
  # TP 9 FP 2 FN 1 TN 8: J = 0.7, Youden CI (-0.001, 0.972) spans 0, so NNDx = 1.43 lies in [1/0.972, +Inf)
  nn <- val_ci(val_dc(TP = 9, FP = 2, FN = 1, TN = 8, ci = TRUE), "nndx")
  expect_true(nn[["lower"]] <= nn[["est"]] && (is.na(nn[["upper"]]) || nn[["est"]] <= nn[["upper"]]))
  expect_gte(nn[["lower"]], 1)
  # J = -0.05: no number of patients is negative
  neg <- val_ci(val_dc(TP = 15, FP = 16, FN = 5, TN = 4, ci = TRUE), "nndx")
  expect_true(is.na(neg[["est"]]))
})

test_that("C41-C42 VAL-decisioncalculator-02: no zero-width 95% interval for a likelihood ratio", {
  # TP 0 FP 0 FN 10 TN 10: sens 0 (CP upper 0.31), spec 1 (CP lower 0.69); LR- is not known exactly
  ln <- val_ci(val_dc(TP = 0, FP = 0, FN = 10, TN = 10, ci = TRUE), "lr.neg")
  expect_true(is.na(ln[["lower"]]) || ln[["upper"]] > ln[["lower"]])
  lp <- val_ci(val_dc(TP = 10, FP = 10, FN = 0, TN = 0, ci = TRUE), "lr.pos")
  expect_true(is.na(lp[["lower"]]) || lp[["upper"]] > lp[["lower"]])
})

test_that("C44 VAL-decisioncalculator-03: the Youden '95%' interval has about 95% coverage", {
  skip_on_cran()
  # Exact enumeration over every 2x2 table with 10 reference positives and 10 negatives (121 calls)
  se <- 0.8; sp <- 0.8; J <- se + sp - 1; cov <- 0
  for (tp in 0:10) for (tn in 0:10) {
    y <- val_ci(val_dc(TP = tp, FN = 10 - tp, TN = tn, FP = 10 - tn, ci = TRUE), "youden")
    if (y[["lower"]] <= J && J <= y[["upper"]]) cov <- cov + stats::dbinom(tp, 10, se) * stats::dbinom(tn, 10, sp)
  }
  expect_lt(abs(cov - 0.95), 0.02)
})

test_that("C51-C52 VAL-decisioncalculator-04: LR+ = 1 is uninformative, not 'lowers' or 'raises'", {
  r <- val_dc(TP = 10, FP = 10, FN = 20, TN = 20, fagan = TRUE)   # sens 1/3, spec 2/3: LR+ = 1 exactly
  expect_true(r$plot1$state$drawable)
  expect_false(grepl("lowers", val_strip(r$faganSummary$content), fixed = TRUE))
  expect_false(grepl("here it lowers that probability", val_notices(r), fixed = TRUE))
  r2 <- val_dc(TP = 50, FP = 50, FN = 50, TN = 50, fagan = TRUE)
  expect_false(grepl("raises the probability from 50.0% to 50.0%", val_strip(r2$faganSummary$content), fixed = TRUE))
})

test_that("C48 C78 C83 VAL-decisioncalculator-05: a positive probability is never printed as 0.0%", {
  # Oracle: Bayes at p = 1e-4, sens 90/110, spec 80/110: PPV = 0.00029994 -> "0.03%"
  r <- val_dc(pp = TRUE, pprob = 1e-4, fagan = TRUE, showSummary = TRUE)
  fs <- val_strip(r$faganSummary$content)
  expect_false(grepl("from 0.0% to 0.0%", fs, fixed = TRUE))
  expect_match(fs, "Pre-test probability is 0.01%", fixed = TRUE)
  expect_match(fs, "raises the probability from 0.01% to 0.03%", fixed = TRUE)
  expect_match(val_strip(r$summary$content), "PPV 0.03%", fixed = TRUE)
  expect_match(val_notices(r), "The selected prevalence is 0.01%", fixed = TRUE)
})

test_that("C62 VAL-decisioncalculator-06: the recommendation does not judge a Youden ranking by accuracy intervals", {
  # Same cohort (100 reference positives, 900 negatives): S1 J 0.506 vs current 0.490; accuracy 0.595 vs 0.941
  rec <- val_key(val_dc(TP = 50, FN = 50, TN = 891, FP = 9, multiplecuts = TRUE, cutoff1 = "S1",
                        tp1 = 95, fn1 = 5, tn1 = 500, fp1 = 400, tp2 = 40, fn2 = 60, tn2 = 895, fp2 = 5
                        )$multipleCutoffTable$asDF, "3")$recommendation
  expect_false(grepl("do not overlap", rec, fixed = TRUE))
})

test_that("C63 VAL-decisioncalculator-07: scenarios with a different reference-positive count are flagged", {
  r <- val_dc(multiplecuts = TRUE, tp1 = 110, fn1 = 10, fp1 = 30, tn1 = 70)
  expect_match(val_notices(r), "Cut-offs describe different", fixed = TRUE)
})

test_that("C80 VAL-decisioncalculator-09: the summary discloses a corrected LR beside a raw PPV", {
  s <- val_strip(val_dc(TP = 10, FP = 0, FN = 10, TN = 10, showSummary = TRUE)$summary$content)
  expect_match(s, "Haldane-Anscombe", fixed = TRUE)
})

test_that("C58 VAL-decisioncalculator-08: population PPV/NPV of a scenario without reference positives are blank", {
  s1 <- val_key(val_dc(multiplecuts = TRUE, pp = TRUE, pprob = 0.05, tp1 = 0, fn1 = 0, fp1 = 40, tn1 = 180
                       )$multipleCutoffTable$asDF, "1")
  expect_true(is.na(s1$ppv) && is.na(s1$npv))
})

test_that("C98 VAL-decisioncalculator-10: the interval table cites the DOR, Youden, NNDx and LR-interval sources", {
  spec <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "decisioncalculator.r.yaml"))
  item <- spec$items[[which(vapply(spec$items, `[[`, "", "name") == "epirTable_number")]]
  expect_true(all(c("Simel1991", "Glas2003", "youden1950", "AgrestiCaffo2000", "LinnGrunau2006") %in% unlist(item$refs)))
})

test_that("C41-C43 VAL-decisioncalculator-11: on a zero cell the LR and DOR intervals use the corrected counts", {
  # Oracle: log method on TP 10.5, FP 0.5, FN 10.5, TN 10.5 (the counts the point estimates use)
  # FP = 0: LR+ and the DOR have no finite upper limit (blank); the lower limit is the log method
  r <- val_dc(TP = 10, FP = 0, FN = 10, TN = 10, ci = TRUE)
  a <- 10.5; b <- 0.5; c <- 10.5; d <- 10.5
  lrp <- (a / (a + c)) / (b / (b + d))
  expect_equal(val_ci(r, "lr.pos"), c(lrp, exp(log(lrp) - val_z * sqrt(1/a - 1/(a + c) + 1/b - 1/(b + d))), NA),
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(val_ci(r, "lr.pos")[["est"]], val_key(r$ratioTable$asDF, "1")$LRP, tolerance = 1e-12)
  dor <- a * d / (b * c)
  expect_equal(val_ci(r, "diag.or"), c(dor, exp(log(dor) - val_z * sqrt(1/a + 1/b + 1/c + 1/d)), NA),
               tolerance = 1e-10, ignore_attr = TRUE)
  # LR- has no zero in its own counts' role (FN = 10, TN = 10): both limits on the corrected counts
  lrn <- (c / (a + c)) / (d / (b + d))
  expect_equal(val_ci(r, "lr.neg"), c(lrn, exp(log(lrn) + c(-1, 1) * val_z * sqrt(1/c - 1/(a + c) + 1/d - 1/(b + d)))),
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_match(r$epirTable_number$notes$rawcounts$note, "0.5 added to every cell", fixed = TRUE)
})

test_that("C64 a malformed target-prevalence list leaves the rest of the analysis intact", {
  r <- val_dc(showWorkload = TRUE, prevalenceScenarios = "1%,5%")
  expect_match(val_notices(r), "Invalid target prevalences", fixed = TRUE)
  expect_equal(nrow(r$screeningWorkload$asDF), 1L)                    # observed row only (pp = FALSE)
  expect_equal(val_key(r$ratioTable$asDF, "1")$Sens, 90 / 110, tolerance = 1e-12)
})

# ------------------------------------------- independent review 2026-10-07 (DC-IR-NN) --
# Report: development-ideas/decisioncalculator-independent-review-2026-10-07.md
# Evidence script: development-scripts/audit_decisioncalculator.R (D08 replays jamovi's int32 option path).

test_that("DC-IR-01 integer counts (jamovi sends typed whole numbers as int32) do not overflow", {
  # Oracle: hand MCC on doubles. Margin product 210 * 200 * 300 * 290 = 3.65e9 > 2^31 - 1.
  r <- val_dc(TP = 180L, FN = 20L, FP = 30L, TN = 270L, ci = TRUE, fagan = TRUE)
  am <- val_key(r$advancedMetricsTable$asDF, "1")
  expect_equal(am$mcc, (180 * 270 - 30 * 20) / sqrt(210 * 200 * 300 * 290), tolerance = 1e-12)
  expect_equal(am$dor, 180 * 270 / (20 * 30), tolerance = 1e-12)
  # table() cells are integers as well: 460 subjects, 115 per cell, MCC 0
  tab <- table(test = factor(rep(c(1, 0, 1, 0), each = 115), 1:0),
               ref  = factor(rep(c(1, 1, 0, 0), each = 115), 1:0))
  r2 <- val_dc(TP = tab[1, 1], FP = tab[1, 2], FN = tab[2, 1], TN = tab[2, 2])
  expect_equal(val_key(r2$advancedMetricsTable$asDF, "1")$mcc, 0)
})

test_that("DC-IR-02 a cell with no value is blank and footnoted; limit-less rows are not in the interval table", {
  # jmvcore sends a numeric NA as a double (jamovi prints "NaN") and a logical NA as missing
  # ("."); only "" is a blank cell, and $asDF still reads it as NA
  r <- val_dc(TP = 45, FN = 5, FP = 0, TN = 50, ci = TRUE)
  expect_true(is.na(val_ci(r, "lr.pos")[["upper"]]))
  expect_identical(r$epirTable_number$getCell(rowKey = "lr.pos", col = "upper")$value, "")
  expect_match(r$epirTable_number$getCell(rowKey = "lr.pos", col = "upper")$footnotes, "No finite limit", fixed = TRUE)
  expect_length(r$epirTable_number$getCell(rowKey = "lr.pos", col = "est")$footnotes, 0)
  expect_match(r$epirTable_number$notes$rawcounts$note, "left blank and footnoted", fixed = TRUE)
  expect_false(any(c("bal.acc", "f1.score") %in% gsub('^"|"$', "", rownames(r$epirTable_ratio$asDF))))
  # no positive tests: PPV is undefined in the measures table and in the interval table
  q <- val_dc(TP = 0, FN = 50, FP = 0, TN = 50, ci = TRUE)
  expect_match(q$ratioTable$getCell(rowNo = 1, col = "PPV")$footnotes, "Not estimable", fixed = TRUE)
  expect_match(q$epirTable_ratio$getCell(rowKey = "pv.pos", col = "est")$footnotes, "Not estimable", fixed = TRUE)
  # J = 0: NNDx has no estimate, a finite lower limit and an unbounded upper limit
  j <- val_dc(TP = 50, FN = 50, FP = 50, TN = 50, ci = TRUE)
  expect_match(j$epirTable_number$getCell(rowKey = "nndx", col = "est")$footnotes, "Not estimable", fixed = TRUE)
  expect_length(j$epirTable_number$getCell(rowKey = "nndx", col = "lower")$footnotes, 0)
  expect_match(j$epirTable_number$getCell(rowKey = "nndx", col = "upper")$footnotes, "No finite limit", fixed = TRUE)
  # intervals not computed (fractional counts): every placeholder cell is blank and says why
  f <- val_dc(TP = 40.5, FN = 10, FP = 6, TN = 144, ci = TRUE)
  cell <- f$epirTable_ratio$getCell(rowKey = "se", col = "est")
  expect_identical(cell$value, "")
  expect_match(cell$footnotes, "Not computed for this table", fixed = TRUE)
  # a filled row keeps no placeholder footnote
  expect_length(r$epirTable_ratio$getCell(rowKey = "se", col = "est")$footnotes, 0)
})

test_that("DC-IR-03 the one-independent-patient assumption is stated; the unit defaults to patient", {
  # Genders et al. Radiology 2012;265:910-6: several observations per patient need a clustered method
  r <- val_dc(ci = TRUE, showAbout = TRUE)
  expect_match(r$epirTable_ratio$notes$independence$note, "one independent patient", fixed = TRUE)
  expect_match(r$epirTable_number$notes$independence$note, "one independent patient", fixed = TRUE)
  expect_match(r$assumptions$content, "one independent patient", fixed = TRUE)
  # the cut-off comparison computes Youden intervals even with ci = FALSE
  m <- val_dc(multiplecuts = TRUE)
  expect_match(m$multipleCutoffTable$notes$uncertainty$note, "one independent patient", fixed = TRUE)
  expect_identical(formals(ClinicoPath::decisioncalculator)$analysisUnit, "patient")
})

test_that("DC-IR-06/07 workload column titles are distinct; the sample's reference prevalence says so", {
  r <- val_dc(ci = TRUE, showWorkload = TRUE)
  titles <- vapply(r$screeningWorkload$columns, function(col) col$title, character(1))
  expect_equal(anyDuplicated(titles), 0L)
  expect_equal(val_key(r$epirTable_ratio$asDF, "tp")$statsnames, "Reference-standard prevalence in this sample")
})

# ------------------------------------------------- check-function-full follow-up (2026-10-07) --

test_that("CFF-01 an inverted table is flagged whether or not the nomogram is on, graded by its interval", {
  # Oracle: sens 20/110 + spec 30/110 = 50/110 < 1, Youden -60/110; Agresti-Caffo interval
  # (21/112 - 81/112) +/- z * se lies wholly below 0. Before the fix only fagan = TRUE raised anything.
  for (fagan in c(FALSE, TRUE)) {
    r <- val_dc(TP = 20, FN = 90, FP = 80, TN = 30, fagan = fagan)
    expect_match(val_notices(r), "STRONG WARNING: Test performs worse than chance", fixed = TRUE, info = fagan)
    expect_match(val_notices(r), "Youden's index is -0.545 (95% CI", fixed = TRUE, info = fagan)
  }
  # the jamovi int32 path decides the same way
  expect_match(val_notices(val_dc(TP = 20L, FN = 90L, FP = 80L, TN = 30L)), "worse than chance", fixed = TRUE)
  # J < 0 within sampling variation (3/7/7/3: J = -0.4, n = 10 + 10) and fractional counts are INFO
  for (r in list(val_dc(TP = 3, FN = 7, FP = 7, TN = 3), val_dc(TP = 20.5, FN = 90, FP = 80, TN = 30))) {
    expect_match(val_notices(r), "INFO: Youden's index below 0", fixed = TRUE)
    expect_false(grepl("worse than chance", val_notices(r)))
  }
  # J = 0 exactly is not below 0, including sens 1/3 + spec 2/3, whose double sum is 0.9999999999999999
  for (cells in list(c(50, 50, 50, 50), c(1, 2, 1, 2), c(0, 10, 0, 10)))
    expect_false(grepl("Youden's index below 0|worse than chance", val_notices(
      val_dc(TP = cells[1], FN = cells[2], FP = cells[3], TN = cells[4]))), info = paste(cells, collapse = "/"))
  expect_false(grepl("Youden's index below 0|worse than chance", val_notices(val_dc())))
})

test_that("CFF-01b the STRONG tier fires at about the one-sided 2.5% on a correctly coded null test", {
  # Exact enumeration over all tables, sens = 1 - spec = 0.5, n = 10 + 10. A point-estimate guard
  # fires about half the time here (feedback: guards need a sampling-error gate). Measured 0.0211.
  grade <- ClinicoPath:::decisioncalculatorClass$new(
    options = ClinicoPath:::decisioncalculatorOptions$new(), data = data.frame()
  )$.__enclos_env__$private$.inversionGrade
  p_strong <- 0
  for (x in 0:10) for (y in 0:10)
    if (grade(x, 10 - x, y, 10 - y) == "strong") p_strong <- p_strong + dbinom(x, 10, 0.5) * dbinom(y, 10, 0.5)
  expect_lt(p_strong, 0.03)
  # and whenever it fires, the 0.5-corrected LR+ the nomogram uses agrees that LR+ < 1
  for (n1 in 1:12) for (n0 in 1:12) for (x in 0:n1) for (y in 0:n0)
    if (grade(x, n1 - x, y, n0 - y) == "strong") {
      k <- if (any(c(x, n1 - x, y, n0 - y) == 0)) 0.5 else 0
      expect_lt((x + k) * (n0 + 2 * k), (y + k) * (n1 + 2 * k))
    }
})

test_that("CFF-03 the negative-test post-test probability is 1 - NPV, not the NPV again", {
  # Oracle: Bayes at the displayed prevalence, P(D+ | T-) = p(1 - se) / (p(1 - se) + (1 - p) sp)
  r <- val_dc(pp = TRUE, pprob = 0.05)
  x <- val_key(r$ratioTable$asDF, "1")
  expect_equal(x$PostTestProbNeg, 0.05 * (20/110) / (0.05 * (20/110) + 0.95 * (80/110)), tolerance = 1e-12)
  expect_equal(x$PostTestProbNeg, 1 - x$NPV, tolerance = 1e-12)
  expect_false("PostTestProbHealthy" %in% names(r$ratioTable$asDF))
  titles <- vapply(r$ratioTable$columns, function(col) col$title, character(1))
  expect_true(all(c("Post-test Reference-positive Probability (Positive Test)",
                    "Post-test Reference-positive Probability (Negative Test)") %in% titles))
  # no negative tests: undefined, so blank and footnoted rather than NaN
  z <- val_dc(TP = 90, FN = 0, FP = 30, TN = 0)
  cell <- z$ratioTable$getCell(rowNo = 1, col = "PostTestProbNeg")
  expect_identical(cell$value, "")
  expect_match(cell$footnotes, "Not estimable", fixed = TRUE)
})
