# Regression tests from the `decisioncompare` release review.
#
# Each case corresponds to a defect confirmed in the shipped code, and is checked
# against an independent reference (epiR, stats::mcnemar.test, DescTools, binom)
# rather than against the module's own arithmetic.

skip_if_not_installed("epiR")

make_fixture <- function(seed = 20260805, n = 300) {
    set.seed(seed)
    gold <- sample(c("pos", "neg"), n, TRUE, c(0.4, 0.6))
    mk <- function(se, sp) ifelse(gold == "pos",
                                  sample(c("pos", "neg"), n, TRUE, c(se, 1 - se)),
                                  sample(c("neg", "pos"), n, TRUE, c(sp, 1 - sp)))
    data.frame(gold = gold, t1 = mk(.85, .90), t2 = mk(.70, .95), t3 = mk(.80, .80),
               stringsAsFactors = FALSE)
}

run_dc <- function(dat, ...) {
    do.call(call_decisioncompare, utils::modifyList(
        list(data = dat, gold = "gold", goldPositive = "pos", goldNegative = NULL,
             test1 = "t1", test1Positive = "pos", test1Negative = NULL,
             test2 = "t2", test2Positive = "pos", test2Negative = NULL,
             test3 = "t3", test3Positive = "pos", test3Negative = NULL,
             stratify = NULL),
        list(...)))
}

strip_html <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " ")))


test_that("per-test metrics match epiR::epi.tests exactly", {
    d <- make_fixture()
    ct <- run_dc(d)$comparisonTable$asDF

    for (v in c("t1", "t2", "t3")) {
        TP <- sum(d[[v]] == "pos" & d$gold == "pos"); FP <- sum(d[[v]] == "pos" & d$gold == "neg")
        FN <- sum(d[[v]] == "neg" & d$gold == "pos"); TN <- sum(d[[v]] == "neg" & d$gold == "neg")
        ref <- as.data.frame(epiR::epi.tests(
            as.table(matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE)))$detail)
        E <- function(s) ref$est[ref$statistic == s]
        r <- ct[ct$test == v, ][1, ]

        expect_equal(r$Sens,   E("se"),      tolerance = 1e-8)
        expect_equal(r$Spec,   E("sp"),      tolerance = 1e-8)
        expect_equal(r$AccurT, E("diag.ac"), tolerance = 1e-8)
        expect_equal(r$PPV,    E("pv.pos"),  tolerance = 1e-8)
        expect_equal(r$NPV,    E("pv.neg"),  tolerance = 1e-8)
        expect_equal(r$LRP,    E("lr.pos"),  tolerance = 1e-6)
        expect_equal(r$LRN,    E("lr.neg"),  tolerance = 1e-6)
    }
})


test_that("tables do not accumulate rows across re-runs", {
    # clearRows() is not a jmvcore Table method. All six calls were wrapped in
    # try(silent = TRUE), so the error was swallowed and nothing was ever cleared:
    # in jamovi, toggling any option re-runs .run() on the same object, and every
    # table grew -- each test appearing 2x, then 3x, then 4x.
    d <- make_fixture()
    opts <- ClinicoPath:::decisioncompareOptions$new(
        gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos", test3 = "t3", test3Positive = "pos",
        stratify = NULL, statComp = TRUE, opa = TRUE, ci = TRUE)
    analysis <- ClinicoPath:::decisioncompareClass$new(options = opts, data = d)

    snap <- function() c(analysis$results$comparisonTable$rowCount,
                         analysis$results$opaTable$rowCount,
                         analysis$results$mcnemarTable$rowCount,
                         analysis$results$diffTable$rowCount,
                         analysis$results$epirTable1$rowCount)
    analysis$run(); first <- snap()
    analysis$run(); second <- snap()
    analysis$run(); third <- snap()

    expect_true(all(first > 0))
    expect_equal(second, first)
    expect_equal(third, first)
})


test_that("McNemar uses diagnostic correctness, and Holm adjusts the pairwise p-values", {
    d <- make_fixture()
    mc <- run_dc(d, statComp = TRUE)$mcnemarTable$asDF

    raw <- vapply(list(c("t1","t2"), c("t1","t3"), c("t2","t3")), function(pr) {
        tb <- table(factor(d[[pr[1]]] == d$gold, c(TRUE, FALSE)),
                    factor(d[[pr[2]]] == d$gold, c(TRUE, FALSE)))
        stats::mcnemar.test(tb)$p.value
    }, numeric(1))
    holm <- stats::p.adjust(raw, "holm")
    nm <- c("t1 vs t2", "t1 vs t3", "t2 vs t3")

    for (i in seq_along(nm)) {
        expect_equal(mc$p[mc$comparison == nm[i]], holm[i], tolerance = 1e-10)
    }
    # the correction must actually bite somewhere in this fixture
    expect_true(any(holm > raw + 1e-12))
})


test_that("Cochran's Q matches DescTools", {
    skip_if_not_installed("DescTools")
    d <- make_fixture()
    mc <- run_dc(d, statComp = TRUE)$mcnemarTable$asDF
    global <- mc[grepl("^Overall", mc$comparison), , drop = FALSE]
    expect_equal(nrow(global), 1L)

    corr <- data.frame(t1 = as.integer(d$t1 == d$gold),
                       t2 = as.integer(d$t2 == d$gold),
                       t3 = as.integer(d$t3 == d$gold))
    long <- data.frame(y = unlist(corr),
                       test = factor(rep(names(corr), each = nrow(d))),
                       subj = factor(rep(seq_len(nrow(d)), times = 3)))
    ref <- DescTools::CochranQTest(y ~ test | subj, data = long)

    expect_equal(global$stat[1], unname(ref$statistic), tolerance = 1e-8)
    expect_equal(global$df[1],   unname(ref$parameter))
    expect_equal(global$p[1],    unname(ref$p.value),   tolerance = 1e-10)
})


test_that("paired differences match Newcombe method 10", {
    d <- make_fixture()
    dt <- run_dc(d, statComp = TRUE)$diffTable$asDF

    hand <- function(x, y) {
        e <- sum(x == 1 & y == 1); f <- sum(x == 1 & y == 0)
        g <- sum(x == 0 & y == 1); h <- sum(x == 0 & y == 0)
        N <- length(x); z <- qnorm(.975)
        wilson <- function(k) {
            p <- k / N; den <- 1 + z^2 / N
            ctr <- (p + z^2 / (2 * N)) / den
            hw <- z * sqrt((p * (1 - p) + z^2 / (4 * N)) / N) / den
            c(max(0, ctr - hw), min(1, ctr + hw))
        }
        px <- (e + f) / N; py <- (e + g) / N
        wx <- wilson(e + f); wy <- wilson(e + g)
        dlx <- px - wx[1]; dux <- wx[2] - px
        dly <- py - wy[1]; duy <- wy[2] - py
        phi_den <- sqrt((e + f) * (g + h) * (e + g) * (f + h))
        phi_num <- e * h - f * g
        if (phi_num > 0) phi_num <- max(phi_num - N / 2, 0)
        phi <- if (phi_den == 0) 0 else phi_num / phi_den
        dd <- (f - g) / N
        c(diff = dd,
          lo = dd - sqrt(dlx^2 - 2 * phi * dlx * duy + duy^2),
          hi = dd + sqrt(dux^2 - 2 * phi * dux * dly + dly^2))
    }
    gp <- d$gold == "pos"; gn <- d$gold == "neg"
    ref_sens <- hand(as.integer(d$t1[gp] == "pos"), as.integer(d$t2[gp] == "pos"))
    ref_spec <- hand(as.integer(d$t1[gn] == "neg"), as.integer(d$t2[gn] == "neg"))

    g <- function(met) dt[dt$comparison == "t1 vs t2" & dt$metric == met, ][1, ]
    expect_equal(g("Sensitivity")$diff,  ref_sens[["diff"]], tolerance = 1e-10)
    expect_equal(g("Sensitivity")$lower, ref_sens[["lo"]],   tolerance = 1e-10)
    expect_equal(g("Sensitivity")$upper, ref_sens[["hi"]],   tolerance = 1e-10)
    expect_equal(g("Specificity")$diff,  ref_spec[["diff"]], tolerance = 1e-10)

    # the sign must follow the label: "t1 vs t2" is t1 - t2
    ct <- run_dc(d)$comparisonTable$asDF
    expect_equal(g("Sensitivity")$diff,
                 ct$Sens[ct$test == "t1"][1] - ct$Sens[ct$test == "t2"][1],
                 tolerance = 1e-10)
})


test_that("Newcombe interval does not collapse with one-sided discordances", {
    d <- data.frame(
        gold = rep("pos", 50),
        t1 = rep("pos", 50),
        t2 = c(rep("pos", 36), rep("neg", 14)),
        stringsAsFactors = FALSE
    )
    res <- call_decisioncompare(
        data = d, gold = "gold", goldPositive = "pos", goldNegative = NULL,
        test1 = "t1", test1Positive = "pos", test1Negative = NULL,
        test2 = "t2", test2Positive = "pos", test2Negative = NULL,
        test3 = NULL, test3Positive = NULL, test3Negative = NULL,
        stratify = NULL, statComp = TRUE)
    row <- res$diffTable$asDF
    row <- row[row$metric == "Sensitivity", , drop = FALSE]
    expect_equal(row$diff, 0.28, tolerance = 1e-10)
    expect_equal(round(row$lower, 4), 0.1528)
    expect_equal(round(row$upper, 4), 0.4167)
    expect_gt(row$upper - row$lower, 0)
})


test_that("OPA confidence intervals honour the selected method", {
    skip_if_not_installed("binom")
    d <- make_fixture()
    conc <- sum(d$t1 == d$gold); n <- nrow(d)

    w <- run_dc(d, opa = TRUE, ciMethod = "wilson")$opaTable$asDF
    w1 <- w[w$test == "t1", ][1, ]
    ref_w <- binom::binom.confint(conc, n, methods = "wilson")
    expect_equal(w1$opa, conc / n, tolerance = 1e-10)
    expect_equal(w1$lower, ref_w$lower, tolerance = 1e-8)
    expect_equal(w1$upper, ref_w$upper, tolerance = 1e-8)

    e <- run_dc(d, opa = TRUE, ciMethod = "exact")$opaTable$asDF
    e1 <- e[e$test == "t1", ][1, ]
    ref_e <- stats::binom.test(conc, n)$conf.int
    expect_equal(e1$lower, ref_e[1], tolerance = 1e-8)
    expect_equal(e1$upper, ref_e[2], tolerance = 1e-8)

    # the two methods must actually differ, or the option is not wired through
    expect_false(isTRUE(all.equal(w1$lower, e1$lower)))
})


test_that("per-test tables hide when their test is not selected", {
    # These used `visible: (!is.null(testN) && testN != "")`. jmvcore routes to its
    # R evaluator only on ^\\([\\$A-Za-z].*\\)$, so a leading '!' returned the raw
    # (truthy) string and the tables were permanently visible -- an empty
    # "Test 3 - Recoded Data" table sat under every two-test analysis.
    opts <- ClinicoPath:::decisioncompareOptions$new(
        gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos", test3 = NULL, test3Positive = NULL,
        stratify = NULL)

    expect_true(isTRUE(opts$eval("(length(test1) > 0)")))
    expect_true(isTRUE(opts$eval("(length(test2) > 0)")))
    expect_true(isFALSE(opts$eval("(length(test3) > 0)")))
    expect_true(isFALSE(opts$eval("(length(stratify) > 0)")))
})


equivocal_fixture <- function() {
    data.frame(
        gold = c(rep("Pos", 20), rep("Neg", 20), rep("Neg", 20)),
        t1   = c(rep("Pos", 18), rep("Neg", 2), rep("Neg", 18), rep("Pos", 2),
                 rep("Equivocal", 20)),
        t2   = c(rep("Pos", 16), rep("Neg", 4), rep("Neg", 17), rep("Pos", 3),
                 rep("Equivocal", 20)),
        stringsAsFactors = FALSE)
}

test_that("excludeIndeterminate refuses to act when no negative level is named", {
    # The filter was `c(positiveLevel, setdiff(levels, positiveLevel))` -- every
    # level -- so the option was a silent no-op and equivocal results were still
    # counted as negatives, inflating specificity exactly as the module's own
    # warning describes. Without a named negative level the analysis cannot tell a
    # genuine negative from an equivocal, so it must say so rather than pretend.
    d <- equivocal_fixture()
    res <- call_decisioncompare(
        data = d, gold = "gold", goldPositive = "Pos", goldNegative = NULL,
        test1 = "t1", test1Positive = "Pos", test1Negative = NULL,
        test2 = "t2", test2Positive = "Pos", test2Negative = NULL,
        test3 = NULL, test3Positive = NULL, test3Negative = NULL, stratify = NULL,
        excludeIndeterminate = TRUE)

    notices <- strip_html(res$notices$content)
    expect_match(notices, "Negative Level Required for Exclusion")
    expect_match(notices, "currently combined as Negative")

    # the inflation it warns about is real: 38/40 with equivocals folded in
    ct <- res$comparisonTable$asDF
    expect_equal(ct$Spec[ct$test == "t1"][1], 38 / 40, tolerance = 1e-10)
})


test_that("excludeIndeterminate drops equivocal rows once a negative level is named", {
    # Exercised through .processSingleTest directly so this holds both before and
    # after jmvtools::prepare() compiles the new *Negative options into the wrapper.
    d <- equivocal_fixture()
    opts <- ClinicoPath:::decisioncompareOptions$new(
        gold = "gold", goldPositive = "Pos", test1 = "t1", test1Positive = "Pos",
        test2 = "t2", test2Positive = "Pos", test3 = NULL, test3Positive = NULL,
        stratify = NULL, excludeIndeterminate = TRUE)
    analysis <- ClinicoPath:::decisioncompareClass$new(options = opts, data = d)
    analysis$run()
    prv <- analysis$.__enclos_env__$private
    prepared <- prv$.prepareData()

    excluded <- prv$.processSingleTest(prepared$data, "t1", "Pos", "gold", "Pos", 1,
                                       testNLevel = "Neg", goldNLevel = "Neg")
    folded <- prv$.processSingleTest(prepared$data, "t1", "Pos", "gold", "Pos", 1)

    # 20 gold-positive + 20 determinate gold-negative; the 20 Equivocal rows go
    expect_equal(excluded$TP + excluded$FP + excluded$FN + excluded$TN, 40)
    expect_equal(folded$TP + folded$FP + folded$FN + folded$TN, 60)

    expect_equal(excluded$metrics$Spec, 18 / 20, tolerance = 1e-10)   # honest
    expect_equal(folded$metrics$Spec,   38 / 40, tolerance = 1e-10)   # inflated
    expect_gt(folded$metrics$Spec, excluded$metrics$Spec)

    # sensitivity is untouched: no gold-positive case was equivocal
    expect_equal(excluded$metrics$Sens, folded$metrics$Sens, tolerance = 1e-10)
})


test_that("a negative level identical to the positive level is rejected", {
    d <- equivocal_fixture()
    opts <- ClinicoPath:::decisioncompareOptions$new(
        gold = "gold", goldPositive = "Pos", test1 = "t1", test1Positive = "Pos",
        test2 = "t2", test2Positive = "Pos", test3 = NULL, test3Positive = NULL,
        stratify = NULL, excludeIndeterminate = TRUE)
    analysis <- ClinicoPath:::decisioncompareClass$new(options = opts, data = d)
    analysis$run()
    prv <- analysis$.__enclos_env__$private
    prepared <- prv$.prepareData()

    expect_error(
        prv$.processSingleTest(prepared$data, "t1", "Pos", "gold", "Pos", 1,
                               testNLevel = "Pos", goldNLevel = "Neg"),
        "Validation failed")
})


test_that("a tie for best test is disclosed rather than broken silently", {
    # Two identical tests score identically; naming one as best is a coin flip.
    d <- data.frame(
        gold = c(rep("pos", 50), rep("neg", 50)),
        t1   = c(rep("pos", 40), rep("neg", 10), rep("neg", 45), rep("pos", 5)),
        stringsAsFactors = FALSE)
    d$t2 <- d$t1

    res <- call_decisioncompare(
        data = d, gold = "gold", goldPositive = "pos", goldNegative = NULL,
        test1 = "t1", test1Positive = "pos", test1Negative = NULL,
        test2 = "t2", test2Positive = "pos", test2Negative = NULL,
        test3 = NULL, test3Positive = NULL, test3Negative = NULL, stratify = NULL,
        showDescriptiveReport = TRUE)

    ct <- res$comparisonTable$asDF
    expect_equal(ct$Sens[ct$test == "t1"][1], ct$Sens[ct$test == "t2"][1])

    notices <- strip_html(res$notices$content)
    expect_match(notices, "Tied Descriptive Ranking")
    expect_match(notices, "not a clinical recommendation")
})


test_that("the difference table names its confidence-interval method", {
    d <- make_fixture()
    res <- run_dc(d, statComp = TRUE)
    # $notes is a list of Note R6 objects keyed by note name
    notes <- paste(vapply(as.list(res$diffTable$notes),
                          function(n) n$note, character(1)), collapse = " ")
    expect_match(notes, "Newcombe's method 10")
    expect_match(notes, "paired correlation")
    # and disambiguates itself from the OPA table's ciMethod option
    expect_match(notes, "CI Method for Agreement")
})


test_that("the manuscript-ready report does not claim a winner it cannot support", {
    d <- make_fixture()   # no pairwise comparison survives Holm in this fixture
    res <- run_dc(d, statComp = TRUE, showDescriptiveReport = TRUE)
    mc <- res$mcnemarTable$asDF
    expect_false(any(mc$p < 0.05, na.rm = TRUE))

    txt <- strip_html(res$clinicalReport$content)
    expect_false(grepl("demonstrated optimal diagnostic performance", txt, fixed = TRUE))
    expect_match(txt, "highest observed balanced accuracy")
    expect_match(txt, "not evidence of superiority or a clinical recommendation")
    expect_match(txt, "should not be reported as evidence that one test outperforms")

    # the placeholder must be gone, replaced by a real Clopper-Pearson interval
    expect_false(grepl("[see confidence interval table]", txt, fixed = TRUE))
    TP <- sum(d$t1 == "pos" & d$gold == "pos"); FN <- sum(d$t1 == "neg" & d$gold == "pos")
    ci <- stats::binom.test(TP, TP + FN)$conf.int
    expect_match(txt, sprintf("95%% CI: %.1f-%.1f%%", ci[1] * 100, ci[2] * 100), fixed = TRUE)
})


test_that("a genuinely significant difference is reported without the hedge", {
    # Guard against over-correcting: when the tests DO separate, the report names
    # the top-ranked test without the "do not read this as evidence" caveat.
    set.seed(4)
    n <- 200
    gold <- sample(c("pos", "neg"), n, TRUE)
    good <- ifelse(runif(n) < 0.97, gold, ifelse(gold == "pos", "neg", "pos"))
    poor <- ifelse(runif(n) < 0.55, gold, ifelse(gold == "pos", "neg", "pos"))
    d <- data.frame(gold = gold, t1 = good, t2 = poor, stringsAsFactors = FALSE)

    res <- call_decisioncompare(
        data = d, gold = "gold", goldPositive = "pos", goldNegative = NULL,
        test1 = "t1", test1Positive = "pos", test1Negative = NULL,
        test2 = "t2", test2Positive = "pos", test2Negative = NULL,
        test3 = NULL, test3Positive = NULL, test3Negative = NULL, stratify = NULL,
        statComp = TRUE, showDescriptiveReport = TRUE)

    expect_lt(res$mcnemarTable$asDF$p[1], 0.05)
    txt <- strip_html(res$clinicalReport$content)
    expect_match(txt, "highest observed balanced accuracy")
    expect_match(txt, "not evidence of superiority or a clinical recommendation")
    expect_false(grepl("should not be reported as evidence", txt, fixed = TRUE))
})


test_that("indeterminate exclusions preserve paired row alignment", {
    d <- data.frame(
        gold = rep(c("Pos", "Neg"), 6),
        t1 = c("Pos", "Equivocal", "Pos", "Equivocal", "Pos", "Neg",
               "Pos", "Neg", "Neg", "Neg", "Pos", "Neg"),
        t2 = c("Pos", "Neg", "Neg", "Neg", "Pos", "Neg",
               "Equivocal", "Neg", "Equivocal", "Pos", "Pos", "Neg"),
        stringsAsFactors = FALSE
    )

    expect_warning(
        res <- call_decisioncompare(
            data = d, gold = "gold", goldPositive = "Pos", goldNegative = "Neg",
            test1 = "t1", test1Positive = "Pos", test1Negative = "Neg",
            test2 = "t2", test2Positive = "Pos", test2Negative = "Neg",
            test3 = NULL, test3Positive = NULL, test3Negative = NULL,
            stratify = NULL, excludeIndeterminate = TRUE, statComp = TRUE),
        NA
    )

    comparison <- res$comparisonTable$asDF
    comparison <- comparison[comparison$test %in% c("t1", "t2"), , drop = FALSE]
    expect_equal(comparison$n, c(10L, 10L))
    expect_equal(comparison$excluded, c(2L, 2L))
    expect_equal(res$mcnemarTable$asDF$n[1], 8L)
    expect_true(all(res$diffTable$asDF$n <= 8L))
})


test_that("stratified predictive values use the requested prevalence and recoding", {
    d <- equivocal_fixture()
    d$site <- rep(c("A", "B"), each = 30)
    res <- call_decisioncompare(
        data = d, gold = "gold", goldPositive = "Pos", goldNegative = "Neg",
        test1 = "t1", test1Positive = "Pos", test1Negative = "Neg",
        test2 = "t2", test2Positive = "Pos", test2Negative = "Neg",
        test3 = NULL, test3Positive = NULL, test3Negative = NULL,
        stratify = "site", excludeIndeterminate = TRUE,
        pp = TRUE, pprob = 0.2)

    st <- res$stratifiedTable$asDF
    expect_true(all(st$n + st$excluded == 30L))
    expect_true(all(st$excludedRate == st$excluded / 30))
    # Bayes PPV at p=0.2; it generally differs from the raw within-stratum PPV.
    row <- st[st$stratum == "A" & st$test == "t1", , drop = FALSE]
    expected <- row$Sens * 0.2 / (row$Sens * 0.2 + (1 - row$Spec) * 0.8)
    expect_equal(row$PPV, expected, tolerance = 1e-10)
})


test_that("duplicate test selections are rejected", {
    d <- equivocal_fixture()
    expect_error(
        call_decisioncompare(
            data = d, gold = "gold", goldPositive = "Pos", goldNegative = NULL,
            test1 = "t1", test1Positive = "Pos", test1Negative = NULL,
            test2 = "t1", test2Positive = "Pos", test2Negative = NULL,
            test3 = NULL, test3Positive = NULL, test3Negative = NULL,
            stratify = NULL),
        "Validation failed"
    )
})


test_that("the reference variable cannot be reused as a test", {
    d <- equivocal_fixture()
    expect_error(
        call_decisioncompare(
            data = d, gold = "gold", goldPositive = "Pos", goldNegative = NULL,
            test1 = "gold", test1Positive = "Pos", test1Negative = NULL,
            test2 = "t2", test2Positive = "Pos", test2Negative = NULL,
            test3 = NULL, test3Positive = NULL, test3Negative = NULL,
            stratify = NULL),
        "Validation failed"
    )
})


test_that("interpretation presets are explicitly examples rather than clinical guides", {
    d <- equivocal_fixture()
    res <- call_decisioncompare(
        data = d, gold = "gold", goldPositive = "Pos", goldNegative = NULL,
        test1 = "t1", test1Positive = "Pos", test1Negative = NULL,
        test2 = "t2", test2Positive = "Pos", test2Negative = NULL,
        test3 = NULL, test3Positive = NULL, test3Negative = NULL,
        stratify = NULL, showExplanations = TRUE)

    about <- strip_html(res$aboutAnalysis$content)
    expect_match(about, "examples only")
    expect_match(about, "not clinical guides")
    notes <- paste(vapply(as.list(res$comparisonTable$notes),
                          function(n) n$note, character(1)), collapse = " ")
    expect_match(notes, "not clinical guides")
})


test_that("missing co-tests and stratifiers do not change standalone metrics", {
    gold <- c(rep("pos", 50), rep("neg", 50))
    t1 <- c(rep("neg", 20), rep("pos", 30), rep("neg", 45), rep("pos", 5))
    complete <- data.frame(
        gold = gold, t1 = t1, t2 = rep(c("pos", "neg"), 50),
        site = rep(c("A", "B"), 50), stringsAsFactors = FALSE)

    missing_cotest <- complete
    missing_cotest$t2[seq_len(20)] <- NA
    missing_stratum <- complete
    missing_stratum$site[seq_len(20)] <- NA

    run_two <- function(dat, stratify = NULL) call_decisioncompare(
        data = dat, gold = "gold", goldPositive = "pos", goldNegative = NULL,
        test1 = "t1", test1Positive = "pos", test1Negative = NULL,
        test2 = "t2", test2Positive = "pos", test2Negative = NULL,
        test3 = NULL, test3Positive = NULL, test3Negative = NULL,
        stratify = stratify, statComp = TRUE)
    metric <- function(res) {
        x <- res$comparisonTable$asDF
        x[x$test == "t1", c("n", "Sens", "Spec", "AccurT")]
    }

    baseline <- metric(run_two(complete))
    expect_equal(metric(run_two(missing_cotest)), baseline)
    expect_equal(metric(run_two(missing_stratum, "site")), baseline)
    expect_equal(run_two(missing_cotest)$mcnemarTable$asDF$n[1], 80L)
})


test_that("narrative panels and OPA criterion are opt in", {
    d <- make_fixture(n = 100)
    base <- run_dc(d, opa = TRUE)
    expect_identical(base$clinicalReport$content, "")
    expect_identical(base$aboutAnalysis$content, "")
    expect_true(all(is.na(base$opaTable$asDF$niMargin)))
    expect_true(all(base$opaTable$asDF$niResult == "Not requested"))

    requested <- run_dc(
        d, opa = TRUE, useOpaCriterion = TRUE, niMargin = 75,
        showExplanations = TRUE, showDescriptiveReport = TRUE)
    expect_true(nzchar(paste(requested$clinicalReport$content, collapse = "")))
    expect_true(nzchar(paste(requested$aboutAnalysis$content, collapse = "")))
    expect_true(all(requested$opaTable$asDF$niMargin == 0.75))
    expect_true(all(requested$opaTable$asDF$niResult %in% c("Yes", "No", "N/A")))
})
