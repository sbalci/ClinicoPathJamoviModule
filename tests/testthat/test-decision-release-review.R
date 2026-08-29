# Regression tests from the `decision` release review.
#
# Every case here corresponds to a defect that was present in the shipped code
# and is verified against an independent reference (epiR, or Bayes' theorem
# worked by hand) rather than against the module's own arithmetic.

skip_if_not_installed("epiR")

# TP = 80, FP = 10, FN = 20, TN = 90  ->  sens .80, spec .90, sample prevalence 50%
mk_2x2 <- function(tp, fp, fn, tn, extra_gold_level = 0L) {
    d <- data.frame(
        gold = c(rep("Present", tp + fn), rep("Absent", fp + tn)),
        test = c(rep("Positive", tp), rep("Negative", fn),
                 rep("Positive", fp), rep("Negative", tn)),
        stringsAsFactors = FALSE)
    if (extra_gold_level > 0)
        d <- rbind(d, data.frame(gold = rep("Equivocal", extra_gold_level),
                                 test = rep("Positive", extra_gold_level),
                                 stringsAsFactors = FALSE))
    d
}

run_decision <- function(dat, ...) {
    do.call(ClinicoPath::decision, utils::modifyList(
        list(data = dat, gold = "gold", goldPositive = "Present",
             newtest = "test", testPositive = "Positive",
             goldNegative = NULL, testNegative = NULL),
        list(...)))
}

strip_html <- function(x) gsub("<[^>]+>", " ", paste(x, collapse = " "))


test_that("point estimates match epiR::epi.tests exactly", {
    r <- run_decision(mk_2x2(80, 10, 20, 90))
    rt <- r$ratioTable$asDF

    ref <- as.data.frame(epiR::epi.tests(
        as.table(matrix(c(80, 10, 20, 90), nrow = 2, byrow = TRUE)))$detail)
    est <- function(s) ref$est[ref$statistic == s]

    expect_equal(rt$Sens[1], est("se"),     tolerance = 1e-8)
    expect_equal(rt$Spec[1], est("sp"),     tolerance = 1e-8)
    expect_equal(rt$PPV[1],  est("pv.pos"), tolerance = 1e-8)
    expect_equal(rt$NPV[1],  est("pv.neg"), tolerance = 1e-8)
    expect_equal(rt$LRP[1],  est("lr.pos"), tolerance = 1e-8)
    expect_equal(rt$LRN[1],  est("lr.neg"), tolerance = 1e-8)
    # 2x2 is not transposed: the sample prevalence is 100/190... here 100/200
    expect_equal(rt$Prevalence[1], 0.5, tolerance = 1e-8)
})


test_that("a supplied prior moves PPV/NPV, not just the prevalence cell", {
    # The shipped code wrote the prior into the Prevalence cell but left PPV/NPV
    # at the raw 2x2 values, so it displayed "Prevalence 5.0%" beside "PPV 88.9%"
    # -- arithmetically impossible for a test with sens .80 / spec .90.
    r <- run_decision(mk_2x2(80, 10, 20, 90), pp = TRUE, pprob = 0.05)
    rt <- r$ratioTable$asDF

    bayes_ppv <- 0.05 * 0.80 / (0.05 * 0.80 + 0.95 * 0.10)   # 0.2963
    bayes_npv <- 0.95 * 0.90 / (0.95 * 0.90 + 0.05 * 0.20)   # 0.9884

    expect_equal(rt$Prevalence[1], 0.05,      tolerance = 1e-8)
    expect_equal(rt$PPV[1],        bayes_ppv, tolerance = 1e-8)
    expect_equal(rt$NPV[1],        bayes_npv, tolerance = 1e-8)

    # sensitivity and specificity are properties of the test, not the population
    expect_equal(rt$Sens[1], 0.80, tolerance = 1e-8)
    expect_equal(rt$Spec[1], 0.90, tolerance = 1e-8)

    # ...and without a prior the sample values are reported unchanged
    rt0 <- run_decision(mk_2x2(80, 10, 20, 90))$ratioTable$asDF
    expect_equal(rt0$PPV[1], 80 / 90, tolerance = 1e-8)
})


test_that("the narrative quotes the same predictive values as the table", {
    r <- run_decision(mk_2x2(80, 10, 20, 90), pp = TRUE, pprob = 0.05,
                      showNaturalLanguage = TRUE, showReportTemplate = TRUE)
    nl  <- strip_html(r$naturalLanguageSummary$content)
    rep <- strip_html(r$reportTemplate$content)

    expect_true(grepl("29.6%", nl, fixed = TRUE))
    expect_false(grepl("88.9%", nl, fixed = TRUE))
    # the prior is not the sample's prevalence and must not be described as such
    expect_false(grepl("with 5.0% disease prevalence", nl, fixed = TRUE))

    # a copy-ready predictive value without its prevalence is not interpretable
    expect_true(grepl("prevalence of 5.0%", rep, fixed = TRUE))
    # ...and it must carry uncertainty
    ci <- as.numeric(stats::binom.test(80, 100)$conf.int)
    expect_true(grepl(sprintf("sensitivity %.1f-%.1f%%", ci[1] * 100, ci[2] * 100),
                      rep, fixed = TRUE))
    # the template used to conclude this for the reader, unconditionally
    expect_false(grepl("may be clinically useful", rep, fixed = TRUE))
})


test_that("a zero cell yields one consistent story across every pane", {
    dz <- mk_2x2(80, 0, 20, 90)          # FP = 0 -> raw LR+ is Inf
    r  <- run_decision(dz, ci = TRUE, fnote = TRUE, fagan = TRUE)

    lrp_main <- r$ratioTable$asDF$LRP[1]
    expect_true(is.finite(lrp_main))     # Haldane-Anscombe applied

    num <- r$epirTable_number$asDF
    lrp_ci <- num$est[num$statsnames == "Positive likelihood ratio"]
    expect_length(lrp_ci, 1L)
    # the CI pane used to run epi.tests on the raw table: Inf, lower bound NaN
    expect_equal(lrp_ci, lrp_main, tolerance = 1e-8)
    expect_true(is.finite(num$lower[num$statsnames == "Positive likelihood ratio"]))
})


test_that("epirTable_number footnotes describe the row they sit on", {
    # Notes were attached at hard-coded rows 1/2/3 while the rows render in the
    # order LR+, LR-, DOR, Youden, NNDx -- so LR+ was labelled as the DOR.
    r <- run_decision(mk_2x2(80, 10, 20, 90), ci = TRUE, fnote = TRUE)
    num   <- r$epirTable_number$asDF
    notes <- r$epirTable_number$footnotes

    expect_equal(num$statsnames[1:3],
                 c("Positive likelihood ratio", "Negative likelihood ratio",
                   "Diagnostic odds ratio"))
    expect_length(notes, nrow(num))
    expect_match(notes[1], "positive result is in a diseased")
    expect_false(grepl("correct diagnosis than an incorrect diagnosis", notes[1]))
    expect_match(notes[3], "correct diagnosis than an incorrect diagnosis")
    expect_match(notes[5], "number of patients that need to be tested", ignore.case = TRUE)
})


test_that("the Fagan nomogram renders when a cell is zero", {
    # Spec is exactly 1 when FP = 0, and nomogrammer rejects the closed bound,
    # so the plot silently failed to draw for exactly the sparse tables that
    # most need a nomogram.
    dz <- mk_2x2(80, 0, 20, 90)
    opts <- ClinicoPath:::decisionOptions$new(
        gold = "gold", goldPositive = "Present", newtest = "test",
        testPositive = "Positive", goldNegative = NULL, testNegative = NULL,
        fagan = TRUE)
    analysis <- ClinicoPath:::decisionClass$new(options = opts, data = dz)
    analysis$run()

    st <- analysis$results$plot1$state
    expect_false(is.null(st))
    expect_true(is.finite(st$Plr))
    expect_true(st$Spec > 0 && st$Spec < 1)   # strictly interior, as nomogrammer demands

    pdf(NULL); on.exit(dev.off(), add = TRUE)
    expect_true(isTRUE(suppressWarnings(
        analysis$.__enclos_env__$private$.plot1(
            analysis$results$plot1, ggtheme = ggplot2::theme_minimal()))))
})


test_that("rows dropped by level are not reported as missing values", {
    # `missingDataSummary` is visible: (od), so od = TRUE is the only state in
    # which a user reads this panel. It used to be written unconditionally, i.e.
    # also into a hidden element; that dead write is gone, so the assertion has
    # to ask for the panel it is inspecting.
    dx <- mk_2x2(80, 10, 20, 90, extra_gold_level = 40L)
    ms <- strip_html(run_decision(dx, goldNegative = "Absent", od = TRUE)$missingDataSummary$content)

    expect_match(ms, "40 case")
    expect_match(ms, "NOT missing values")
    expect_false(grepl("40 case\\(s\\) \\([^)]*\\) removed for missing values", ms))

    # genuinely missing rows are still attributed to missingness
    dm <- mk_2x2(80, 10, 20, 90); dm$test[1:5] <- NA
    ms2 <- strip_html(run_decision(dm, od = TRUE)$missingDataSummary$content)
    expect_match(ms2, "5 case\\(s\\).*removed for missing values")
})

test_that("the level/missing distinction reaches the user without the raw-data panel", {
    # With od = FALSE the panel is hidden, so the notices pane is the only
    # channel that carries this. Both causes must still be told apart there.
    dx <- mk_2x2(80, 10, 20, 90, extra_gold_level = 40L)
    nx <- strip_html(run_decision(dx, goldNegative = "Absent")$notices$content)
    expect_match(nx, "excluded from analysis")
    expect_match(nx, "Equivocal")

    dm <- mk_2x2(80, 10, 20, 90); dm$test[1:5] <- NA
    nm <- strip_html(run_decision(dm)$notices$content)
    expect_match(nm, "missing diagnostic data")
})


test_that("a likelihood ratio of exactly 1 is called uninformative", {
    # sens = spec = 0.5 -> LR+ = LR- = 1. The band was `lr_pos > 1`, so 1.0 fell
    # through to "Decreases probability of disease (test may be flawed)".
    r <- run_decision(mk_2x2(50, 50, 50, 50),
                      showNaturalLanguage = TRUE, showClinicalInterpretation = TRUE)
    txt <- strip_html(c(r$naturalLanguageSummary$content,
                        r$clinicalInterpretation$content))

    expect_match(txt, "[Uu]ninformative")
    expect_false(grepl("Decreases probability of disease", txt))
})
