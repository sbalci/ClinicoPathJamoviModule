# Regression tests from /validate-function decisioncompare depth=exhaustive (2026-10-01).
# Report: development-ideas/decisioncompare-validation-2026-10-01.md
# Evidence script: development-scripts/validate_decisioncompare.R
#
# Every expectation below is an ORACLE value (hand arithmetic, a closed form, a published worked
# example or an independent package), never the module's own output. Tests named "VAL-...-NN open
# defect" assert the CORRECT behaviour and are skipped until the defect is fixed; remove the skip
# when the fix lands.

val_dc <- function(data, gold = "gold", goldPositive = "D", goldNegative = NULL,
                   test1 = "t1", test1Positive = "+", test1Negative = NULL,
                   test2 = "t2", test2Positive = "+", test2Negative = NULL,
                   test3 = NULL, test3Positive = NULL, test3Negative = NULL,
                   stratify = NULL, ...) {
    # Every Level argument explicit; variables as strings through do.call (a bare symbol would be
    # read by resolveQuo as the column NAME).
    args <- c(list(data = data, gold = gold, goldPositive = goldPositive, goldNegative = goldNegative,
                   test1 = test1, test1Positive = test1Positive, test1Negative = test1Negative,
                   test2 = test2, test2Positive = test2Positive, test2Negative = test2Negative,
                   test3 = test3, test3Positive = test3Positive, test3Negative = test3Negative,
                   stratify = stratify), list(...))
    suppressWarnings(suppressMessages(do.call(ClinicoPath::decisioncompare, args)))
}
val_expand <- function(spec, lv_gold = c("D", "N"), lv_test = c("+", "-")) {
    idx <- rep(seq_len(nrow(spec)), spec$n)
    out <- spec[idx, setdiff(names(spec), "n"), drop = FALSE]
    rownames(out) <- NULL
    for (nm in names(out))
        out[[nm]] <- factor(out[[nm]], levels = if (nm == "gold") lv_gold else lv_test)
    out
}
val_row <- function(ct, v) ct[!is.na(ct$test) & ct$test == v, , drop = FALSE]
val_text <- function(x) {
    x <- gsub("<[^>]+>", " ", paste(x, collapse = " "))
    for (e in list(c("&#x2F;", "/"), c("&#x27;", "'"), c("&quot;", "\""), c("&lt;", "<"), c("&gt;", ">"),
                   c("&amp;", "&")))
        x <- gsub(e[1], e[2], x, fixed = TRUE)
    trimws(gsub("[[:space:]]+", " ", x))
}
val_foot <- function(tbl, key, col) paste(tbl$getCell(rowKey = key, col = col)$footnotes, collapse = " | ")

# 14-row hand table. rows 1-3 D + + | 4-5 D + - | 6 D - - | 7-10 N - - | 11 N + - | 12 N + + |
# 13 D NA + | 14 NA + -
val_d1 <- function() data.frame(
    gold = factor(c(rep("D", 6), rep("N", 6), "D", NA), levels = c("D", "N")),
    t1 = factor(c("+", "+", "+", "+", "+", "-", "-", "-", "-", "-", "+", "+", NA, "+"), levels = c("+", "-")),
    t2 = factor(c("+", "+", "+", "-", "-", "-", "-", "-", "-", "-", "-", "+", "+", "-"), levels = c("+", "-")))

# Test B done only on 100 hard cases, test A on all 300. Paired: b = 5, c = 25 (B better).
val_d2 <- function() rbind(
    val_expand(data.frame(gold = c("D", "N", "D", "N", "D", "N", "D", "N"),
                          t1 = c("+", "-", "+", "-", "-", "+", "-", "+"),
                          t2 = c("+", "-", "-", "+", "+", "-", "-", "+"),
                          n = c(25, 25, 3, 2, 12, 13, 10, 10))),
    val_expand(data.frame(gold = c("D", "D", "N", "N"), t1 = c("+", "-", "-", "+"), t2 = NA,
                          n = c(98, 2, 98, 2))))

val_d3 <- function() {
    set.seed(20261001)
    n <- 200
    g <- ifelse(runif(n) < 0.35, "D", "N")
    mk <- function(se, sp) ifelse(g == "D", ifelse(runif(n) < se, "+", "-"), ifelse(runif(n) < sp, "-", "+"))
    d <- data.frame(gold = g, t1 = mk(.85, .90), t2 = mk(.70, .95), t3 = mk(.80, .80), stringsAsFactors = FALSE)
    d$t2[sample(n, 10)] <- NA
    d$t3[sample(n, 15)] <- NA
    d$gold[sample(n, 3)] <- NA
    for (nm in names(d)) d[[nm]] <- factor(d[[nm]], levels = if (nm == "gold") c("D", "N") else c("+", "-"))
    d
}

val_2x2 <- function(TP, FP, FN, TN) val_expand(data.frame(
    gold = c("D", "N", "D", "N"), t1 = c("+", "+", "-", "-"), t2 = c("+", "-", "+", "-"), n = c(TP, FP, FN, TN)))

# Newcombe (1998) method 10, transcribed from p. 2639 of the paper.
val_nc10 <- function(e, f, g, h, z = stats::qnorm(0.975)) {
    n <- e + f + g + h
    wil <- function(x) {
        p <- x / n
        c0 <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
        hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
        c(c0 - hw, c0 + hw)
    }
    p2 <- (e + f) / n; p3 <- (e + g) / n; w2 <- wil(e + f); w3 <- wil(e + g)
    dl2 <- p2 - w2[1]; du2 <- w2[2] - p2; dl3 <- p3 - w3[1]; du3 <- w3[2] - p3
    den <- sqrt((e + f) * (g + h) * (e + g) * (f + h))
    num <- e * h - f * g
    if (num > 0) num <- max(num - n / 2, 0)
    phi <- if (den == 0) 0 else num / den
    th <- (f - g) / n
    c(th, th - sqrt(dl2^2 - 2 * phi * dl2 * du3 + du3^2), th + sqrt(du2^2 - 2 * phi * du2 * dl3 + dl3^2))
}


test_that("VAL C01-C12: per-test metrics on a 14-row hand table", {
    ct <- val_dc(val_d1())$comparisonTable$asDF
    # hand: t1 TP5 FN1 FP2 TN4 (rows 1-12); t2 TP4 FN3 FP1 TN5 (rows 1-13)
    r1 <- val_row(ct, "t1"); r2 <- val_row(ct, "t2")
    expect_equal(c(r1$n, r1$excluded), c(12, 2))
    expect_equal(c(r1$Sens, r1$Spec, r1$AccurT, r1$PPV, r1$NPV), c(5 / 6, 4 / 6, 9 / 12, 5 / 7, 4 / 5), tolerance = 1e-12)
    expect_equal(c(r1$LRP, r1$LRN), c(2.5, 0.25), tolerance = 1e-12)
    expect_equal(c(r2$n, r2$excluded), c(13, 1))
    expect_equal(c(r2$Sens, r2$Spec, r2$AccurT, r2$PPV, r2$NPV), c(4 / 7, 5 / 6, 9 / 13, 4 / 5, 5 / 8), tolerance = 1e-12)
    expect_equal(c(r2$LRP, r2$LRN), c(24 / 7, 18 / 35), tolerance = 1e-12)
})

test_that("VAL C17: the 2x2 has test results in rows and the reference in columns", {
    tb <- val_dc(val_d1())$cTable1$asDF
    key <- gsub('^"|"$', "", rownames(tb))
    expect_equal(unlist(tb[key == "Test Positive", c("GP", "GN", "Total")]), c(GP = 5, GN = 2, Total = 7))
    expect_equal(unlist(tb[key == "Test Negative", c("GP", "GN", "Total")]), c(GP = 1, GN = 4, Total = 5))
})

test_that("VAL C35-C39: three discordant pairs give the McNemar mid-p 5/8 (hand)", {
    # b = 2, c = 1: Bin(3, .5) masses 1/8, 3/8, 3/8, 1/8. Fagerland 2013 eq. (5):
    # 2 * (P(X <= 1) - f(1)/2) = 2 * (4/8 - 3/16) = 5/8 (the exact conditional p would be 1)
    res <- val_dc(val_d1(), statComp = TRUE)
    mc <- res$mcnemarTable$asDF
    expect_equal(mc$n[1], 12)
    expect_identical(mc$method[1], "McNemar mid-p")
    expect_equal(mc$p[1], 5 / 8, tolerance = 1e-12)
    expect_equal(mc$p[1], dc_mcnemar_midp(2, 1), tolerance = 1e-12)
    expect_true(is.na(mc$stat[1]) && is.na(mc$df[1]))
    expect_match(val_foot(res$mcnemarTable, "t1 vs t2", "p"), "Only 3 discordant pairs", fixed = TRUE)
})

test_that("VAL C37/C39: 30 discordant pairs, hand mid-p 206368/2^30 and uncorrected chi-square 400/30", {
    # b = 5, c = 25. Mid-p: sum C(30, 0:5) = 174437, C(30, 5) = 142506, so
    # 2 * (174437 - 142506/2) / 2^30 = 206368 / 2^30. Asymptotic: (5 - 25)^2 / 30, no correction.
    mc <- val_dc(val_d2(), statComp = TRUE)$mcnemarTable$asDF
    expect_equal(mc$p[1], 206368 / 2^30, tolerance = 1e-12)
    asy <- val_dc(val_d2(), statComp = TRUE, mcnemarMethod = "asymptotic")$mcnemarTable$asDF
    expect_equal(c(asy$stat[1], asy$df[1]), c(400 / 30, 1), tolerance = 1e-12)
    expect_equal(asy$p[1], 2 * stats::pnorm(-20 / sqrt(30)), tolerance = 1e-12)
})

test_that("VAL C36: no switch at 25 discordant pairs - the same recommended test on both sides", {
    # The old rule changed test at 25 discordant pairs; Fagerland 2013 recommend the mid-p (and the
    # uncorrected asymptotic test) at every sample size, so 24 and 25 pairs get the same test.
    mk <- function(m) {
        gold <- rep(c("D", "N"), 50); t1 <- ifelse(gold == "D", "+", "-"); t2 <- t1
        flip <- function(x) ifelse(x == "+", "-", "+")
        t2[seq_len(m - 3)] <- flip(t2[seq_len(m - 3)])
        t1[50 + 1:3] <- flip(t1[50 + 1:3])
        data.frame(gold = factor(gold, c("D", "N")), t1 = factor(t1, c("+", "-")), t2 = factor(t2, c("+", "-")))
    }
    # b = m - 3 (only t1 right), c = 3 (only t2 right)
    m24 <- val_dc(mk(24), statComp = TRUE)$mcnemarTable$asDF
    m25 <- val_dc(mk(25), statComp = TRUE)$mcnemarTable$asDF
    expect_identical(c(m24$method[1], m25$method[1]), c("McNemar mid-p", "McNemar mid-p"))
    expect_equal(c(m24$p[1], m25$p[1]), c(dc_mcnemar_midp(21, 3), dc_mcnemar_midp(22, 3)), tolerance = 1e-12)
    a24 <- val_dc(mk(24), statComp = TRUE, mcnemarMethod = "asymptotic")$mcnemarTable$asDF
    a25 <- val_dc(mk(25), statComp = TRUE, mcnemarMethod = "asymptotic")$mcnemarTable$asDF
    expect_identical(a24$method[1], a25$method[1])
    expect_equal(c(a24$stat[1], a25$stat[1]), c(18^2 / 24, 19^2 / 25), tolerance = 1e-12)
    expect_equal(c(a24$p[1], a25$p[1]), c(dc_mcnemar_asymptotic(21, 3)[["p"]], dc_mcnemar_asymptotic(22, 3)[["p"]]),
                 tolerance = 1e-12)
})

test_that("VAL C45/C48: paired differences reproduce Newcombe (1998) Table III, method 10", {
    # Stat Med 17:2641-2642, printed to 4 dp. For (1,97,1,1) the method-10 entry 0.8736 is a
    # misprint: eh <= fg there, so methods 8 and 10 coincide by the paper's definition, and the
    # method-8 entry of the same row (0.8737) is used.
    tab3 <- data.frame(
        e = c(36, 20, 18, 36, 35, 18, 2, 1, 0, 2, 1, 0, 54, 53, 30, 29, 28, 27),
        f = c(12, 12, 12, 14, 14, 14, 97, 97, 29, 98, 98, 30, 0, 0, 0, 0, 0, 0),
        g = c(2, 2, 2, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        h = c(0, 16, 18, 0, 1, 18, 0, 1, 0, 0, 1, 0, 0, 1, 24, 25, 26, 27),
        lo = c(0.0569, 0.0562, 0.0562, 0.1528, 0.1461, 0.1441, 0.8721, 0.8737, 0.6666, 0.9178, 0.9171,
               0.8395, -0.0664, -0.0729, -0.0358, -0.0354, -0.0352, -0.0351),
        hi = c(0.3404, 0.3292, 0.3290, 0.4167, 0.4175, 0.3963, 0.9854, 0.9850, 0.9882, 0.9945, 0.9916,
               1.0, 0.0664, 0.0729, 0.0358, 0.0354, 0.0352, 0.0351))
    for (i in seq_len(nrow(tab3))) {
        k <- tab3[i, ]
        d <- val_expand(data.frame(gold = c("D", "D", "D", "D", "N"), t1 = c("+", "+", "-", "-", "-"),
                                   t2 = c("+", "-", "+", "-", "-"), n = c(k$e, k$f, k$g, k$h, 10)))
        r <- val_dc(d, statComp = TRUE)$diffTable$asDF
        r <- r[r$metric == "Sensitivity", , drop = FALSE]
        expect_equal(r$diff, (k$f - k$g) / (k$e + k$f + k$g + k$h), tolerance = 1e-12)
        expect_lte(abs(r$lower - k$lo), 5e-5 + 1e-12)
        expect_lte(abs(r$upper - k$hi), 5e-5 + 1e-12)
    }
})

test_that("VAL C48: paired-difference limits match ratesci::moverpairci (independent Newcombe 10)", {
    skip_if_not_installed("ratesci")
    dt <- val_dc(val_d1(), statComp = TRUE)$diffTable$asDF
    # hand paired cells (e, f, g, h): sensitivity 3,2,0,1; specificity 4,0,1,1; accuracy 7,2,1,2
    cells <- list(Sensitivity = c(3, 2, 0, 1), Specificity = c(4, 0, 1, 1), Accuracy = c(7, 2, 1, 2))
    for (m in names(cells)) {
        k <- cells[[m]]
        ref <- suppressWarnings(ratesci::moverpairci(x = k, contrast = "RD", type = "wilson", corc = TRUE,
                                                     precis = 12))$estimates
        r <- dt[dt$metric == m, , drop = FALSE]
        expect_equal(c(r$lower, r$upper), unname(ref[1, c("lower", "upper")]), tolerance = 1e-9)
        expect_equal(c(r$diff, r$lower, r$upper), val_nc10(k[1], k[2], k[3], k[4]), tolerance = 1e-12)
    }
})

test_that("VAL C09/C10: predictive values at a supplied prevalence follow Bayes (hand fractions)", {
    ct <- val_dc(val_d1(), pp = TRUE, pprob = 0.10)$comparisonTable$asDF
    expect_equal(c(val_row(ct, "t1")$PPV, val_row(ct, "t1")$NPV), c(5 / 23, 36 / 37), tolerance = 1e-12)
    expect_equal(c(val_row(ct, "t2")$PPV, val_row(ct, "t2")$NPV), c(8 / 29, 35 / 37), tolerance = 1e-12)
    expect_equal(val_row(ct, "t1")$Sens, 5 / 6, tolerance = 1e-12)
})

test_that("VAL C21: the per-test CI rows are Clopper-Pearson intervals (binom.test)", {
    skip_if_not_installed("epiR")
    ep <- val_dc(val_d1(), ci = TRUE)$epirTable1$asDF
    key <- gsub('^"|"$', "", rownames(ep))
    counts <- list(ap = c(7, 12), tp = c(6, 12), se = c(5, 6), sp = c(4, 6), diag.ac = c(9, 12), pv.pos = c(5, 7),
                   pv.neg = c(4, 5), p.tpdn = c(2, 6), p.tndp = c(1, 6), p.dntp = c(2, 7), p.dptn = c(1, 5))
    for (k in names(counts)) {
        r <- ep[key == k, , drop = FALSE]
        x <- counts[[k]][1]; n <- counts[[k]][2]
        expect_equal(c(r$est, r$lower, r$upper), c(x / n, stats::binom.test(x, n)$conf.int), tolerance = 1e-7)
    }
})

test_that("VAL C28-C30: OPA intervals match binom for the three methods", {
    skip_if_not_installed("binom")
    d3 <- val_d3()
    x <- sum((d3$t1 == "+") == (d3$gold == "D"), na.rm = TRUE)
    n <- sum(!is.na(d3$t1) & !is.na(d3$gold))
    for (m in c("wilson", "logit", "exact")) {
        o <- val_dc(d3, opa = TRUE, ciMethod = m)$opaTable$asDF
        o <- o[o$test == "t1", , drop = FALSE]
        ref <- binom::binom.confint(x, n, methods = m)
        expect_equal(c(o$opa, o$lower, o$upper), c(x / n, ref$lower, ref$upper), tolerance = 1e-8)
    }
})

test_that("VAL C29: logit at OPA = 100% shows Clopper-Pearson limits (binom's convention)", {
    d <- val_expand(data.frame(gold = c("D", "N"), t1 = c("+", "-"), t2 = c("+", "-"), n = c(15, 15)))
    o <- val_dc(d, opa = TRUE, ciMethod = "logit")$opaTable$asDF[1, ]
    expect_equal(c(o$lower, o$upper), stats::binom.test(30, 30)$conf.int[1:2], tolerance = 1e-10)
})

test_that("VAL C39/C41: Holm-adjusted McNemar p and Cochran's Q match closed forms and DescTools", {
    skip_if_not_installed("DescTools")
    d3 <- val_d3()
    mc <- val_dc(d3, test3 = "t3", test3Positive = "+", statComp = TRUE)$mcnemarTable$asDF
    tv <- c("t1", "t2", "t3")
    correct <- function(v, ok) (d3[[v]][ok] == "+") == (d3$gold[ok] == "D")
    # raw p: the McNemar mid-p (Fagerland 2013 eq. 5/6) on each pair's own complete cases
    raw <- vapply(list(c("t1", "t2"), c("t1", "t3"), c("t2", "t3")), function(pr) {
        ok <- !is.na(d3[[pr[1]]]) & !is.na(d3[[pr[2]]]) & !is.na(d3$gold)
        a <- correct(pr[1], ok); b <- correct(pr[2], ok)
        dc_mcnemar_midp(sum(a & !b), sum(!a & b))
    }, numeric(1))
    expect_equal(mc$p[match(c("t1 vs t2", "t1 vs t3", "t2 vs t3"), mc$comparison)], stats::p.adjust(raw, "holm"),
                 tolerance = 1e-10)
    ok <- !is.na(d3$gold) & !is.na(d3$t1) & !is.na(d3$t2) & !is.na(d3$t3)
    M <- sapply(tv, function(v) as.integer(correct(v, ok)))
    long <- data.frame(y = as.vector(M), test = factor(rep(tv, each = nrow(M))), subj = factor(rep(seq_len(nrow(M)), 3)))
    ref <- DescTools::CochranQTest(y ~ test | subj, data = long)
    q <- mc[mc$method == "Cochran's Q", , drop = FALSE]
    expect_equal(c(q$n, q$stat, q$p), c(nrow(M), unname(ref$statistic), ref$p.value), tolerance = 1e-10)
})

test_that("VAL C52: swapping the two tests negates each difference and mirrors its interval", {
    d3 <- val_d3()
    a <- val_dc(d3, statComp = TRUE)$diffTable$asDF
    b <- val_dc(d3, test1 = "t2", test2 = "t1", statComp = TRUE)$diffTable$asDF
    for (m in c("Sensitivity", "Specificity", "Accuracy")) {
        x <- a[a$metric == m, ]; y <- b[b$metric == m, ]
        expect_equal(c(y$diff, y$lower, y$upper), c(-x$diff, -x$upper, -x$lower), tolerance = 1e-12)
    }
})

test_that("VAL C04: swapping the reference level maps sensitivity to 1 - specificity and inverts the LRs", {
    d3 <- val_d3()
    a <- val_row(val_dc(d3)$comparisonTable$asDF, "t1")
    b <- val_row(val_dc(d3, goldPositive = "N")$comparisonTable$asDF, "t1")
    expect_equal(c(b$Sens, b$Spec, b$PPV, b$NPV, b$AccurT, b$LRP, b$LRN),
                 c(1 - a$Spec, 1 - a$Sens, 1 - a$PPV, 1 - a$NPV, 1 - a$AccurT, 1 / a$LRP, 1 / a$LRN), tolerance = 1e-12)
})

test_that("VAL C33: every stratified row equals the analysis run on that subset", {
    d3 <- val_d3()
    set.seed(7)
    d3$site <- factor(sample(c("A", "B"), nrow(d3), TRUE))
    st <- val_dc(d3, stratify = "site", opa = TRUE)$stratifiedTable$asDF
    for (s in c("A", "B")) {
        sub <- val_dc(d3[d3$site == s, ])$comparisonTable$asDF
        for (v in c("t1", "t2")) {
            o <- st[st$stratum == s & st$test == v, , drop = FALSE]; e <- val_row(sub, v)
            expect_equal(unlist(o[, c("n", "Sens", "Spec", "AccurT", "PPV", "NPV")]),
                         unlist(e[, c("n", "Sens", "Spec", "AccurT", "PPV", "NPV")]), tolerance = 1e-12)
        }
    }
})

test_that("VAL C69: excludeIndeterminate with named negatives drops equivocal rows (hand counts)", {
    eq <- data.frame(
        gold = factor(c(rep("Pos", 20), rep("Neg", 20), rep("Eq", 5)), c("Pos", "Neg", "Eq")),
        t1 = factor(c(rep("Pos", 16), "Neg", "Neg", "Eq", "Eq", rep("Neg", 15), "Pos", "Pos", "Eq", "Neg", "Neg",
                      "Pos", "Neg", "Neg", "Pos", "Eq"), c("Pos", "Neg", "Eq")),
        t2 = factor(c(rep("Pos", 14), "Neg", "Eq", "Pos", "Neg", "Pos", "Neg", rep("Neg", 13), "Pos", "Eq", "Neg",
                      "Pos", "Neg", "Eq", "Pos", "Pos", "Neg", "Pos", "Neg", "Neg"), c("Pos", "Neg", "Eq")))
    res <- val_dc(eq, goldPositive = "Pos", goldNegative = "Neg", test1Positive = "Pos", test1Negative = "Neg",
                  test2Positive = "Pos", test2Negative = "Neg", excludeIndeterminate = TRUE, statComp = TRUE)
    ct <- res$comparisonTable$asDF
    expect_equal(unlist(val_row(ct, "t1")[, c("n", "excluded", "Sens", "Spec")]), c(n = 37, excluded = 8, Sens = 16 / 18, Spec = 17 / 19),
                 tolerance = 1e-12)
    expect_equal(unlist(val_row(ct, "t2")[, c("n", "excluded", "Sens", "Spec")]), c(n = 37, excluded = 8, Sens = 16 / 19, Spec = 15 / 18),
                 tolerance = 1e-12)
    # paired correctness: b = 3, c = 2 (the accuracy cells below); mid-p 2 * (16/32 - 10/64) = 11/16
    expect_equal(c(res$mcnemarTable$asDF$n[1], res$mcnemarTable$asDF$p[1]), c(34, 11 / 16), tolerance = 1e-12)
    acc <- res$diffTable$asDF
    acc <- acc[acc$metric == "Accuracy", , drop = FALSE]
    expect_equal(c(acc$diff, acc$lower, acc$upper), val_nc10(27, 3, 2, 2), tolerance = 1e-12)
})

test_that("VAL C11: FP = 0 applies the documented 0.5 correction to LR+ only", {
    ct <- val_dc(val_2x2(50, 0, 5, 45))$comparisonTable$asDF
    # LR+ = ((50.5/56) / (1 - 45.5/46)) = 2323/28; LR- from raw counts = (5/55)/1
    expect_equal(c(val_row(ct, "t1")$LRP, val_row(ct, "t1")$LRN), c(2323 / 28, 1 / 11), tolerance = 1e-12)
})


# ---- defects fixed by `/fix-function decisioncompare --apply` (2026-10-01) ---------------------
# These were skip()-guarded as open defects in the validation; each asserted the correct behaviour
# and failed on the code before the fix.

test_that("VAL-decisioncompare-01: the 'significantly more accurate' test is the one ahead on the PAIRED cases", {
    res <- val_dc(val_d2(), statComp = TRUE, showSummary = TRUE, showReportSentence = TRUE)
    # paired (100 rows): t2 right where t1 wrong on 25 pairs, the reverse on 5; paired accuracy 55% vs 75%
    sm <- val_text(res$summaryReport$content)
    rp <- val_text(res$reportSentence$content)
    expect_match(sm, "t2 classified significantly more cases correctly than t1", fixed = TRUE)
    expect_match(sm, "on the 100 cases with a determinate result for both tests and the reference standard: accuracy 55.0% vs 75.0%", fixed = TRUE)
    expect_match(sm, "of the 30 cases where exactly one test was correct, 25 favoured t2", fixed = TRUE)
    expect_match(rp, "t2 classified significantly more cases correctly than t1", fixed = TRUE)
    expect_match(rp, "(n = 100 paired cases;", fixed = TRUE)
    expect_match(rp, "the difference, t1 minus t2, was -20.0 percentage points", fixed = TRUE)
    # the verdict follows the data, not the slot order
    swapped <- val_dc(val_d2(), test1 = "t2", test2 = "t1", statComp = TRUE, showReportSentence = TRUE)
    expect_match(val_text(swapped$reportSentence$content), "t2 classified significantly more cases correctly than t1",
                 fixed = TRUE)
})

test_that("VAL-decisioncompare-02: LR+ is not estimable when the test is never positive (and LR- when never negative)", {
    never_pos <- val_dc(val_2x2(0, 0, 10, 80))
    ct <- never_pos$comparisonTable$asDF
    expect_true(is.na(val_row(ct, "t1")$LRP))
    expect_equal(val_row(ct, "t1")$LRN, 1, tolerance = 1e-12)    # (10/10) / (80/80)
    expect_false(grepl("positive evidence", ct$test[2], fixed = TRUE))
    expect_match(val_foot(never_pos$comparisonTable, "test_1", "LRP"), "never positive", fixed = TRUE)
    expect_match(never_pos$notices$content, "Likelihood Ratio Not Estimable", fixed = TRUE)
    never_neg <- val_dc(val_2x2(40, 5, 0, 0))$comparisonTable$asDF
    expect_true(is.na(val_row(never_neg, "t1")$LRN))
    expect_false(grepl("negative evidence", never_neg$test[2], fixed = TRUE))
    # the legitimate correction (FP = 0 with TP > 0) is unchanged: 2323/28
    expect_equal(val_row(val_dc(val_2x2(50, 0, 5, 45))$comparisonTable$asDF, "t1")$LRP, 2323 / 28, tolerance = 1e-12)
})

test_that("VAL-decisioncompare-03: exported R syntax parses and re-runs for awkward names and levels", {
    d <- val_d1()
    d$site <- factor(rep(c("A", "B"), 7))
    d$t1 <- factor(ifelse(is.na(d$t1), NA, ifelse(d$t1 == "+", "pos \"x\"", "neg\\y")),
                   levels = c("pos \"x\"", "neg\\y"))
    names(d) <- c("Gréade (ref)", "Test \"A\"", "back\\slash B", "site")
    opts <- ClinicoPath:::decisioncompareOptions$new(
        gold = "Gréade (ref)", goldPositive = "D", goldNegative = "N",
        test1 = "Test \"A\"", test1Positive = "pos \"x\"", test1Negative = "neg\\y",
        test2 = "back\\slash B", test2Positive = "+", test2Negative = "-",
        test3 = NULL, test3Positive = NULL, stratify = "site", statComp = TRUE)
    a <- ClinicoPath:::decisioncompareClass$new(options = opts, data = d)
    suppressWarnings(a$run())
    src <- a$asSource()
    expect_error(parse(text = src), NA)
    env <- new.env()
    env$data <- d
    rerun <- suppressWarnings(eval(parse(text = src), envir = env))
    expect_equal(rerun$comparisonTable$asDF, a$results$comparisonTable$asDF)
    expect_equal(rerun$stratifiedTable$asDF, a$results$stratifiedTable$asDF)
})

test_that("VAL-decisioncompare-04: copy-ready text never prints a p-value as 0.000", {
    hp <- ClinicoPath::histopathology
    d <- data.frame(gold = factor(as.character(hp[["Golden Standart"]]), c("0", "1")),
                    t1 = factor(as.character(hp[["New Test"]]), c("0", "1")),
                    t2 = factor(as.character(hp[["Rater 1"]]), c("0", "1")))
    res <- val_dc(d, goldPositive = "1", test1Positive = "1", test2Positive = "1", statComp = TRUE,
                  showSummary = TRUE, showReportSentence = TRUE)
    sm <- val_text(res$summaryReport$content)
    rp <- val_text(res$reportSentence$content)
    expect_false(grepl("p = 0.000", sm, fixed = TRUE))
    expect_false(grepl("p = 0.000", rp, fixed = TRUE))
    expect_match(rp, "p < 0.001", fixed = TRUE)
    # the formatter's edges: a significant p never prints as 0.050
    a <- ClinicoPath:::decisioncompareClass$new(
        options = ClinicoPath:::decisioncompareOptions$new(gold = "gold", goldPositive = "D", test1 = "t1",
            test1Positive = "+", test2 = "t2", test2Positive = "+", test3 = NULL, test3Positive = NULL, stratify = NULL),
        data = val_d1())
    f <- a$.__enclos_env__$private$.formatP
    expect_identical(vapply(c(0.0004, 0.0123, 0.0496, 0.04996, 0.0504, 1), f, ""),
                     c("&lt; 0.001", "= 0.012", "= 0.0496", "&lt; 0.05", "= 0.050", "= 1.000"))
})

test_that("VAL-decisioncompare-05: copy-ready Methods name every test run, and the report sentence carries N and the CI", {
    d3 <- val_d3()
    res <- val_dc(d3, test3 = "t3", test3Positive = "+", statComp = TRUE, showDescriptiveReport = TRUE,
                  showReportSentence = TRUE)
    txt <- val_text(res$clinicalReport$content)
    # the Methods paragraph only: the Results paragraph says "McNemar's/Cochran's test" anyway
    methods <- sub(".*Methods Section: (.*?) Results Section:.*", "\\1", txt, perl = TRUE)
    expect_match(methods, "Cochran", fixed = TRUE)
    expect_match(methods, "Holm", fixed = TRUE)
    expect_match(methods, "Newcombe", fixed = TRUE)
    q <- res$mcnemarTable$asDF
    expect_match(val_text(res$reportSentence$content),
                 sprintf("(n = %d cases with all three results;", q$n[q$method == "Cochran's Q"]), fixed = TRUE)
    two <- val_dc(d3, statComp = TRUE, showReportSentence = TRUE, showDescriptiveReport = TRUE)
    rp <- val_text(two$reportSentence$content)
    expect_match(rp, sprintf("(n = %d paired cases;", two$mcnemarTable$asDF$n[1]), fixed = TRUE)
    acc <- two$diffTable$asDF
    acc <- acc[acc$metric == "Accuracy", , drop = FALSE]
    expect_match(rp, sprintf("was %.1f percentage points (95%% CI %.1f to %.1f;", 100 * acc$diff, 100 * acc$lower,
                             100 * acc$upper), fixed = TRUE)
    # two tests: no Cochran's Q and no Holm adjustment to describe
    m2 <- sub(".*Methods Section: (.*?) Results Section:.*", "\\1", val_text(two$clinicalReport$content), perl = TRUE)
    expect_false(grepl("Cochran", m2, fixed = TRUE))
    expect_false(grepl("Holm", m2, fixed = TRUE))
})

test_that("VAL-decisioncompare-06: a logit row computed by the Clopper-Pearson fallback is labelled as such", {
    d <- val_expand(data.frame(gold = c("D", "N"), t1 = c("+", "-"), t2 = c("+", "-"), n = c(15, 15)))
    res <- val_dc(d, opa = TRUE, ciMethod = "logit")
    expect_match(val_foot(res$opaTable, "test_1", "lower"), "Clopper-Pearson", fixed = TRUE)
    # an interior row keeps the logit interval and no footnote
    inner <- val_dc(val_d1(), opa = TRUE, ciMethod = "logit")
    expect_identical(val_foot(inner$opaTable, "test_1", "lower"), "")
})

test_that("VAL-decisioncompare-07: with a supplied prevalence, the PPV/NPV footnotes do not give the sample formula", {
    res <- val_dc(val_d1(), pp = TRUE, pprob = 0.10, fnote = TRUE)
    expect_false(grepl("TP/(TP+FP)", val_foot(res$comparisonTable, "test_1", "PPV"), fixed = TRUE))
    expect_false(grepl("TN/(TN+FN)", val_foot(res$comparisonTable, "test_1", "NPV"), fixed = TRUE))
    expect_match(val_foot(res$comparisonTable, "test_1", "PPV"), "Bayes", fixed = TRUE)
    # without a supplied prevalence the sample formula stays
    expect_match(val_foot(val_dc(val_d1(), fnote = TRUE)$comparisonTable, "test_1", "PPV"), "TP/(TP+FP)", fixed = TRUE)
})

test_that("VAL-decisioncompare-08: no balanced-accuracy ranking is claimed when none was estimable", {
    d <- val_expand(data.frame(gold = c("D", "D", "D", "D"), t1 = c("+", "+", "-", "-"), t2 = c("+", "-", "+", "-"),
                               n = c(30, 10, 4, 6)))
    res <- val_dc(d, statComp = TRUE, showDescriptiveReport = TRUE)
    txt <- val_text(res$clinicalReport$content)
    expect_false(grepl("had the highest observed balanced accuracy", txt, fixed = TRUE))
    expect_match(txt, "No test could be ranked by balanced accuracy", fixed = TRUE)
    # three tests, all reference-positive, Cochran's Q significant: Summary and Report do not rank
    d3 <- data.frame(gold = factor(rep("D", 60), c("D", "N")),
                     t1 = factor(rep("+", 60), c("+", "-")),
                     t2 = factor(rep(c("+", "-"), c(30, 30)), c("+", "-")),
                     t3 = factor(rep(c("+", "-"), c(35, 25)), c("+", "-")))
    r3 <- val_dc(d3, test3 = "t3", test3Positive = "+", statComp = TRUE, showSummary = TRUE, showReportSentence = TRUE)
    expect_lt(r3$mcnemarTable$asDF$p[r3$mcnemarTable$asDF$method == "Cochran's Q"], 0.05)
    expect_false(grepl("highest observed balanced accuracy", val_text(r3$summaryReport$content), fixed = TRUE))
    expect_false(grepl("highest observed balanced accuracy", val_text(r3$reportSentence$content), fixed = TRUE))
    expect_match(val_text(r3$reportSentence$content), "so no test is ranked", fixed = TRUE)
})

test_that("VAL-decisioncompare-09: an LR exactly on a band edge falls where Jaeschke 1994 puts it", {
    # Oracle: the cited bands, "> 10 or < 0.1" large and "5 to 10" / "0.1 to 0.2" moderate
    # (Jaeschke, Guyatt & Sackett 1994, quoted in Peirce & Gerkin 2003, PMC1494809), so an LR
    # exactly 10 or 0.1 is moderate, as in `decision`. Reference audit 2026-10-01.
    at10 <- val_dc(val_2x2(10, 1, 10, 19))$comparisonTable$asDF   # Sens 1/2, Spec 19/20: LR+ = 10
    expect_equal(val_row(at10, "t1")$LRP, 10, tolerance = 1e-14)
    expect_match(at10$test[2], "Moderate positive evidence", fixed = TRUE)
    expect_false(grepl("Strong positive evidence", at10$test[2], fixed = TRUE))
    at01 <- val_dc(val_2x2(19, 10, 1, 10))$comparisonTable$asDF   # Sens 19/20, Spec 1/2: LR- = 0.1
    expect_match(at01$test[2], "Moderate negative evidence", fixed = TRUE)
    expect_false(grepl("Strong negative evidence", at01$test[2], fixed = TRUE))
    # past the edge is large: LR+ = 0.5 / 0.025 = 20 and LR- = 0.025 / 0.5 = 0.05
    above <- val_dc(val_2x2(20, 1, 20, 39))$comparisonTable$asDF  # Sens 20/40, Spec 39/40
    expect_match(above$test[2], "Strong positive evidence", fixed = TRUE)
    beyond <- val_dc(val_2x2(39, 20, 1, 20))$comparisonTable$asDF # Sens 39/40, Spec 20/40
    expect_match(beyond$test[2], "Strong negative evidence", fixed = TRUE)
    # just inside the moderate band stays moderate: LR+ = 9.5
    below <- val_dc(val_2x2(19, 2, 21, 38))$comparisonTable$asDF  # Sens 19/40, Spec 38/40: (0.475)/(0.05) = 9.5
    expect_match(below$test[2], "Moderate positive evidence", fixed = TRUE)
})

test_that("VAL-decisioncompare-10: the radar plot scores LR+ = r and LR- = 1/r alike, continuously", {
    a <- ClinicoPath:::decisioncompareClass$new(
        options = ClinicoPath:::decisioncompareOptions$new(gold = "gold", goldPositive = "D", test1 = "t1",
            test1Positive = "+", test2 = "t2", test2Positive = "+", test3 = NULL, test3Positive = NULL, stratify = NULL),
        data = val_d1())
    p <- a$.__enclos_env__$private
    th <- p$LR_CLINICAL_THRESHOLDS
    pos <- function(x) p$.scaleLikelihoodRatioPositive(x, th)
    neg <- function(x) p$.scaleLikelihoodRatioNegative(x, th)
    expect_equal(c(pos(1), pos(1.04), pos(1.5), pos(2), pos(5), pos(10), pos(20)), c(0, 2, 25, 50, 75, 100, 100),
                 tolerance = 1e-12)
    for (r in c(1.04, 1.5, 2.5, 7)) expect_equal(neg(1 / r), pos(r), tolerance = 1e-12)
    expect_equal(c(neg(1), neg(1.2), neg(0)), c(0, 0, 100))
    expect_true(is.na(pos(NA)) && is.na(neg(NA)))
})

test_that("VAL-decisioncompare-11: the difference table says which test is subtracted", {
    res <- val_dc(val_d1(), statComp = TRUE)
    notes <- paste(vapply(as.list(res$diffTable$notes), function(n) n$note, character(1)), collapse = " ")
    expect_match(notes, "first-named test minus the second", fixed = TRUE)
})

test_that("VAL-decisioncompare-12: an unused factor level does not raise the multi-level warning", {
    d <- val_d1()
    d$gold <- factor(as.character(d$gold), levels = c("D", "N", "Unused"))
    res <- val_dc(d)
    expect_false(grepl("Multi-Level Results Combined", res$notices$content, fixed = TRUE))
    # a level that IS used still raises it
    d$gold[14] <- "Unused"
    expect_match(val_dc(d)$notices$content, "Multi-Level Results Combined", fixed = TRUE)
})

test_that("VAL-decisioncompare-13: the heatmap legend keeps its labels under jamovi's own theme", {
    gt <- jmvcore:::getGlobalTheme("default", "jmv")
    a <- ClinicoPath:::decisioncompareClass$new(
        options = ClinicoPath:::decisioncompareOptions$new(gold = "gold", goldPositive = "D", test1 = "t1",
            test1Positive = "+", test2 = "t2", test2Positive = "+", test3 = NULL, test3Positive = NULL,
            stratify = NULL, heatmap = TRUE),
        data = val_d1())
    suppressWarnings(a$run())
    grDevices::png(tempfile(fileext = ".png"))
    on.exit(grDevices::dev.off(), add = TRUE)
    suppressWarnings(suppressMessages(
        a$.__enclos_env__$private$.plotHeatmap(a$results$plotHeatmap, ggtheme = gt$ggtheme, theme = gt$theme)))
    sc <- ggplot2::ggplot_build(ggplot2::last_plot())$plot$scales$get_scales("fill")
    # as.character(): get_labels() carries a "pos" attribute
    expect_identical(as.character(sc$get_labels()), c("Negative", "Positive", "Indeterminate / excluded"))
    expect_identical(as.character(sc$name), "Result")
    expect_identical(unname(sc$map(c("0", "1", "2"))), c("white", "black", "#f59e0b"))
})


# ---- Recommended McNemar variants (Fagerland 2013) ----------------------------------------------
# Fagerland, Lydersen & Laake (BMC Med Res Methodol 2013;13:91) recommend the McNemar mid-p test "for
# use in any situation" and the asymptotic test without continuity correction where small excesses
# over the nominal level are acceptable; the exact conditional and continuity-corrected tests they
# recommend in no situation. Oracles: their Table 6 (4 dp), the eq. (5)/(6) and z-form formulas in
# helper-decisioncompare.R, hand fractions, exact2x2 and coin.

# One row per case from per-test correctness flags. The reference alternates D/N so both classes are
# present, and a test reads "+" exactly when it is right about a D case or wrong about an N case.
val_from_correct <- function(ok) {
    gold <- rep_len(c("D", "N"), length(ok[[1]]))
    out <- data.frame(gold = factor(gold, c("D", "N")))
    for (nm in names(ok)) out[[nm]] <- factor(ifelse(ok[[nm]] == (gold == "D"), "+", "-"), c("+", "-"))
    out
}
# Two tests from correctness counts: a both right, b only t1 right, c only t2 right, d both wrong.
val_mc <- function(a, b, c, d) val_from_correct(list(t1 = rep(c(TRUE, TRUE, FALSE, FALSE), c(a, b, c, d)),
                                                     t2 = rep(c(TRUE, FALSE, TRUE, FALSE), c(a, b, c, d))))
val_methods <- function(res)
    sub(".*Methods Section: (.*?) Results Section:.*", "\\1", val_text(res$clinicalReport$content), perl = TRUE)
val_chisq <- intToUtf8(c(0x3C7, 0xB2))   # the chi-squared sign the panels print, built from code points
# The Results Section paragraph of the Descriptive Report (the same text also opens the panel).
val_results <- function(res)
    sub(".*Results Section: (.*?) Performance Summary.*", "\\1", val_text(res$clinicalReport$content), perl = TRUE)
# Tags dropped WITHOUT padding, as a browser runs inline markup together: a space missing around
# <b> or <span> shows here, while val_text() pads every tag with a space and hides it.
val_inline <- function(x) gsub("<[^>]+>", "", paste(x, collapse = " "))


test_that("Recommended McNemar variants (Fagerland 2013): Table 6, Bentur and Cavo, through the full analysis", {
    # Bentur et al. (their Table 1): 1, 1 / 7, 12 of 21 pairs; Cavo et al. (Table 2): 59, 6 / 16, 80 of 161.
    # Table 6 to 4 dp: mid-p 0.0391 and 0.0347, asymptotic 0.0339 and 0.0330.
    tab6 <- data.frame(a = c(1, 59), b = c(1, 6), c = c(7, 16), d = c(12, 80), n = c(21, 161),
                       midp = c(0.0391, 0.0347), asym = c(0.0339, 0.0330),
                       midp_exact = c(10 / 256, 0.0346896648406982), chisq = c(36 / 8, 100 / 22))
    for (i in seq_len(nrow(tab6))) {
        k <- tab6[i, ]
        d <- val_mc(k$a, k$b, k$c, k$d)
        mid <- val_dc(d, statComp = TRUE, mcnemarMethod = "midp")$mcnemarTable$asDF
        asy <- val_dc(d, statComp = TRUE, mcnemarMethod = "asymptotic")$mcnemarTable$asDF
        expect_equal(c(mid$n, asy$n), c(k$n, k$n))
        expect_lte(abs(mid$p - k$midp), 5e-5 + 1e-12)
        expect_lte(abs(asy$p - k$asym), 5e-5 + 1e-12)
        expect_equal(c(mid$p, dc_mcnemar_midp(k$b, k$c)), rep(k$midp_exact, 2), tolerance = 1e-12)
        expect_equal(c(asy$stat, asy$df), c(k$chisq, 1), tolerance = 1e-12)
        expect_equal(asy$p, dc_mcnemar_asymptotic(k$b, k$c)[["p"]], tolerance = 1e-12)
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): both variants match exact values over a grid of (b, c)", {
    # mid-p values are exact binary fractions (1/32, 1/1024, 3/256, 697/32768, ...); the grid spans
    # the old 25-pair switch. Chi-squared is (b - c)^2 / (b + c) with no correction.
    grid <- data.frame(b = c(0, 0, 1, 3, 10, 24, 25, 100), c = c(5, 10, 9, 12, 20, 25, 40, 130),
                       midp = c(0.03125, 0.0009765625, 0.01171875, 0.021270751953125, 0.0707555459812284,
                                0.887724827340783, 0.0640175041372256, 0.0481630534407445),
                       asym = c(0.0253473186774682, 0.00156540225800255, 0.0114120363860017, 0.0201367515503464,
                                0.0678891548618294, 0.886403006367064, 0.0628118475160987, 0.0479127550084477))
    for (i in seq_len(nrow(grid))) {
        g <- grid[i, ]
        d <- val_mc(3, g$b, g$c, 4)
        mid <- val_dc(d, statComp = TRUE)$mcnemarTable$asDF
        asy <- val_dc(d, statComp = TRUE, mcnemarMethod = "asymptotic")$mcnemarTable$asDF
        expect_equal(c(mid$p, dc_mcnemar_midp(g$b, g$c)), rep(g$midp, 2), tolerance = 1e-12)
        expect_equal(c(asy$p, dc_mcnemar_asymptotic(g$b, g$c)[["p"]]), rep(g$asym, 2), tolerance = 1e-12)
        expect_equal(asy$stat, (g$b - g$c)^2 / (g$b + g$c), tolerance = 1e-12)
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): eq. (6) at a tie, and p = 1 without discordant pairs", {
    # eq. (6), 1 - f(b | 2b)/2: b = c = 1 -> 1 - (2/4)/2; 2 -> 1 - (6/16)/2; 3 -> 1 - (20/64)/2; 5 -> 1 - (252/1024)/2
    for (k in list(c(1, 3 / 4), c(2, 13 / 16), c(3, 27 / 32), c(5, 898 / 1024))) {
        d <- val_mc(4, k[1], k[1], 4)
        mid <- val_dc(d, statComp = TRUE)$mcnemarTable$asDF
        asy <- val_dc(d, statComp = TRUE, mcnemarMethod = "asymptotic")$mcnemarTable$asDF
        expect_equal(c(mid$p, dc_mcnemar_midp(k[1], k[1])), rep(k[2], 2), tolerance = 1e-12)
        expect_equal(c(asy$stat, asy$df, asy$p), c(0, 1, 1))
    }
    # b + c = 0: both tests right on exactly the same cases. p = 1 for both variants by convention, no
    # statistic (the raw cells are NULL, which jamovi shows empty), and no verdict: the panels say the
    # test is not informative rather than "did not detect". Paired accuracy is 10/14 for both tests, and
    # Newcombe's method 10 on the accuracy cells (10, 0, 0, 4) gives 0 (-15.5 to 15.5) percentage points.
    expect_equal(round(100 * val_nc10(10, 0, 0, 4), 1), c(0, -15.5, 15.5))
    labels <- c(midp = "McNemar mid-p", asymptotic = "McNemar asymptotic (no continuity correction)")
    not_informative <- "Not informative: no discordant pairs (p = 1 by convention)"
    no_pairs_foot <- "No discordant pairs: both tests were correct on exactly the same cases, so McNemar's test has no information here; p is set to 1 and no test statistic is computed."
    for (m in names(labels)) {
        res <- val_dc(val_mc(10, 0, 0, 4), statComp = TRUE, mcnemarMethod = m, showSummary = TRUE,
                      showReportSentence = TRUE, showDescriptiveReport = TRUE)
        mc <- res$mcnemarTable$asDF
        expect_identical(mc$method, labels[[m]])
        expect_equal(mc$p, 1)
        expect_true(is.na(mc$stat) && is.na(mc$df))
        expect_null(res$mcnemarTable$getCell(rowKey = "t1 vs t2", col = "stat")$value)
        expect_null(res$mcnemarTable$getCell(rowKey = "t1 vs t2", col = "df")$value)
        expect_identical(mc$interpretation, not_informative)
        expect_identical(val_foot(res$mcnemarTable, "t1 vs t2", "p"), no_pairs_foot)
        # The Descriptive Report's Results Section says so too, and neither reports a null result nor
        # discounts the ranking as "not statistically significant".
        rs <- val_results(res)
        expect_false(grepl("Methods Section:", rs, fixed = TRUE))   # the section was found
        expect_match(rs, "Statistical comparison was not informative here: none of the pairs of tests that could be compared had discordant pairs, so McNemar's test produced no test result (detailed results in the comparison tables).",
                     fixed = TRUE)
        cr <- val_text(res$clinicalReport$content)
        # "Because the differences in overall accuracy": the wording since IR-decisioncompare-02,
        # so this absence check still guards the sentence it was written for.
        for (bad in c("did not reveal", "Because the differences in overall accuracy"))
            expect_false(grepl(bad, cr, fixed = TRUE), info = paste(m, bad))
        sm <- val_text(res$summaryReport$content)
        rp <- val_text(res$reportSentence$content)
        expect_match(sm, paste0(labels[[m]], ": not informative here. No case was classified correctly by exactly one of the two tests, so McNemar's test has no discordant pairs to compare and p is shown as 1 only by convention. Read the accuracy difference and its paired confidence interval instead."),
                     fixed = TRUE)
        expect_match(rp, paste("No discordant pairs: none of the 14 paired cases was classified correctly by only one of t1 and t2, so McNemar's test has no information about a difference in accuracy (its p-value is set to 1 by convention, not computed).",
                               "On these paired cases accuracy was 71.4% for t1 and 71.4% for t2; the difference, t1 minus t2, was 0.0 percentage points (95% CI -15.5 to 15.5; Newcombe's method 10).",
                               "Identical correctness on these cases does not establish that the two tests perform equally; the confidence interval shows how large a difference remains compatible with these data."),
                     fixed = TRUE)
        for (bad in c("did not detect", "No significant difference detected", "non-significant McNemar result",
                      "p = 1.000", val_chisq))
            expect_false(grepl(bad, paste(sm, rp), fixed = TRUE), info = paste(m, bad))
    }
    # three tests with one such pair: see "a pair without discordant pairs is left out of the Holm adjustment"
})

test_that("Recommended McNemar variants (Fagerland 2013): under 10 discordant pairs a footnote flags power, the test still runs", {
    # The footnote depends on m = b + c and the variant only. The smallest p either test can give with m pairs
    # is at the split (m, 0): mid-p 2 * (f(0) - f(m)/2) = 0.5^m, asymptotic P(chi-squared(1) > m). While even
    # that is >= 0.05 - mid-p to m = 4 (1/16; 1/32 at m = 5), asymptotic to m = 3 (0.083; 0.0455 at m = 4) - no
    # split reaches p < 0.05 and the footnote says so; above that it flags low power until m = 10. One pair
    # takes a sentence of its own (.() has no plural forms). Each m runs at the split (m, 0) and near-balanced.
    cannot <- function(m) if (m == 1)
        "Only 1 discordant pair: with one, McNemar's test cannot reach p < 0.05, so its non-significant result says nothing about whether the tests differ; read the accuracy difference and its paired confidence interval." else
        sprintf("Only %d discordant pairs: with so few, McNemar's test cannot reach p < 0.05 whatever their split, so its non-significant result says nothing about whether the tests differ; read the accuracy difference and its paired confidence interval.", m)
    little <- function(m) sprintf("Only %d discordant pairs, so McNemar's test has little power here; read the accuracy difference and its paired confidence interval.", m)
    oracle_p <- function(m, b, c) if (m == "midp") dc_mcnemar_midp(b, c) else dc_mcnemar_asymptotic(b, c)[["p"]]
    # hand: one pair (b = 1, c = 0), mid-p 2 * (1/2 - (1/2)/2) = 1/2, asymptotic chi-squared 1, p = 2 * pnorm(-1);
    # the cut, mid-p 1/16 at m = 4 and 1/32 at m = 5
    expect_equal(c(oracle_p("midp", 1, 0), oracle_p("asymptotic", 1, 0)), c(1 / 2, 2 * stats::pnorm(-1)), tolerance = 1e-12)
    expect_equal(c(oracle_p("midp", 4, 0), oracle_p("midp", 5, 0)), c(1 / 16, 1 / 32), tolerance = 1e-12)
    last_cannot <- c(midp = 4, asymptotic = 3)
    for (m in names(last_cannot)) {
        k <- last_cannot[[m]]
        # "whatever their split": every split of k pairs stays >= 0.05, while (k + 1, 0) gets below it
        expect_gte(min(vapply(0:k, function(b) oracle_p(m, b, k - b), numeric(1))), 0.05)
        expect_lt(oracle_p(m, k + 1, 0), 0.05)
        for (n_disc in 1:10) for (b in unique(c(n_disc, n_disc %/% 2))) {
            info <- paste(m, b, n_disc - b)
            tb <- val_dc(val_mc(5, b, n_disc - b, 6), statComp = TRUE, mcnemarMethod = m)$mcnemarTable
            expect_identical(val_foot(tb, "t1 vs t2", "p"),
                             if (n_disc <= k) cannot(n_disc) else if (n_disc < 10) little(n_disc) else "", info = info)
            expect_equal(tb$asDF$p, oracle_p(m, b, n_disc - b), tolerance = 1e-12, info = info)
        }
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): mid-p is the default; label and statistic per variant", {
    expect_identical(formals(ClinicoPath::decisioncompare)$mcnemarMethod, "midp")
    opts <- ClinicoPath:::decisioncompareOptions$new(gold = "gold", goldPositive = "D", test1 = "t1", test1Positive = "+",
        test2 = "t2", test2Positive = "+", test3 = NULL, test3Positive = NULL, stratify = NULL)
    expect_identical(opts$mcnemarMethod, "midp")
    d <- val_mc(1, 1, 7, 12)
    def <- val_dc(d, statComp = TRUE)$mcnemarTable$asDF   # option left out
    mid <- val_dc(d, statComp = TRUE, mcnemarMethod = "midp")$mcnemarTable$asDF
    asy <- val_dc(d, statComp = TRUE, mcnemarMethod = "asymptotic")$mcnemarTable$asDF
    expect_equal(def, mid)
    expect_identical(mid$method, "McNemar mid-p")
    expect_true(is.na(mid$stat) && is.na(mid$df))   # the mid-p test has no test statistic
    expect_identical(asy$method, "McNemar asymptotic (no continuity correction)")
    expect_equal(c(asy$stat, asy$df), c(4.5, 1))
    # the exact conditional and continuity-corrected tests are not offered
    expect_error(val_dc(d, statComp = TRUE, mcnemarMethod = "exact"), "must be one of", fixed = TRUE)
})

test_that("Recommended McNemar variants (Fagerland 2013): with three tests the pairwise p are Holm-adjusted oracle values", {
    # correctness pattern (t1 t2 t3) -> count. Hand discordant counts per pair: t1-t2 b = 6 + 2, c = 1 + 1;
    # t1-t3 b = 8 + 2, c = 1 + 3; t2-t3 b = 8 + 1, c = 6 + 3 (a tie)
    cnt <- c("111" = 20, "101" = 6, "011" = 1, "110" = 8, "001" = 3, "100" = 2, "010" = 1, "000" = 5)
    bits <- rep(names(cnt), cnt)
    d3 <- val_from_correct(stats::setNames(lapply(1:3, function(j) substr(bits, j, j) == "1"), c("t1", "t2", "t3")))
    bc <- list(c(8, 2), c(10, 4), c(9, 9))
    pairs <- c("t1 vs t2", "t1 vs t3", "t2 vs t3")
    raw_mid <- vapply(bc, function(x) dc_mcnemar_midp(x[1], x[2]), numeric(1))
    expect_equal(raw_mid, c(67 / 1024, 1941 / 16384, 1 - 24310 / 262144), tolerance = 1e-12)   # hand
    raw_asy <- vapply(bc, function(x) dc_mcnemar_asymptotic(x[1], x[2])[["p"]], numeric(1))
    run3 <- function(m) val_dc(d3, test3 = "t3", test3Positive = "+", statComp = TRUE, mcnemarMethod = m)$mcnemarTable
    tabs <- list(midp = run3("midp"), asymptotic = run3("asymptotic"))
    mid <- tabs$midp$asDF
    asy <- tabs$asymptotic$asDF
    # every pair has discordant pairs, so all three are tested and none is set aside
    for (m in names(tabs)) {
        expect_false("mcnemar_uninformative_excluded" %in% names(tabs[[m]]$notes), info = m)
        expect_match(tabs[[m]]$notes[["mcnemar_multiplicity"]]$note, "Holm-Bonferroni adjusted across the 3 pairwise comparisons",
                     fixed = TRUE, info = m)
    }
    holm <- stats::p.adjust(raw_mid, "holm")
    expect_gt(holm[1], raw_mid[1])   # the adjustment bites
    expect_equal(mid$p[match(pairs, mid$comparison)], holm, tolerance = 1e-12)
    expect_equal(asy$p[match(pairs, asy$comparison)], stats::p.adjust(raw_asy, "holm"), tolerance = 1e-12)
    expect_equal(asy$stat[match(pairs, asy$comparison)], c(36 / 10, 36 / 14, 0), tolerance = 1e-12)
    # Cochran's Q stays the global test for either variant. Hand: correct per test 36, 30, 30; per case
    # 3 (20), 2 (15), 1 (6), 0 (5): Q = 2 * (3 * 3096 - 96^2) / (3 * 96 - 246) = 24/7. Exact tail: sum multinomial(6; a) * multinomial(15; b)
    # over sum((a + 15 - b)^2) >= 456, divided by 3^21: 807196867/3486784401.
    for (q in list(mid, asy)) {
        g <- q[q$method == "Cochran's Q", , drop = FALSE]
        expect_equal(c(g$n, g$stat, g$df, g$p), c(46, 24 / 7, NA, 807196867 / 3486784401), tolerance = 1e-12)
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): a pair without discordant pairs is left out of the Holm adjustment", {
    # Correctness (t1 t2 t3): 111 x 10, 110 x 3, 001 x 1, 000 x 4, so t1 and t2 are right on the same cases
    # (b = c = 0) and each pair with t3 has b = 3, c = 1. Hand mid-p (3, 1): 2 * (P(X <= 1) - f(3)/2) =
    # 2 * (5/16 - 2/16) = 3/8; asymptotic chi-squared (3 - 1)^2 / 4 = 1, p = 2 * pnorm(-1). Holm runs over the two
    # pairs that were tested, 2 * 3/8 = 3/4 each for mid-p; the untested pair keeps its conventional p = 1,
    # unadjusted, states no verdict and claims no correction.
    cnt <- c("111" = 10, "110" = 3, "001" = 1, "000" = 4)
    bits <- rep(names(cnt), cnt)
    d3 <- val_from_correct(stats::setNames(lapply(1:3, function(j) substr(bits, j, j) == "1"), c("t1", "t2", "t3")))
    raw <- c(midp = 3 / 8, asymptotic = 2 * stats::pnorm(-1))
    expect_equal(c(dc_mcnemar_midp(3, 1), dc_mcnemar_asymptotic(3, 1)[["p"]]), unname(raw), tolerance = 1e-12)
    expect_equal(stats::p.adjust(rep(raw[["midp"]], 2), "holm"), c(3 / 4, 3 / 4), tolerance = 1e-12)
    pairs <- c("t1 vs t2", "t1 vs t3", "t2 vs t3")
    not_informative <- "Not informative: no discordant pairs (p = 1 by convention)"
    tested <- "No significant difference detected at alpha = 0.05; this does not establish that the tests perform equally (Holm-Bonferroni corrected)"
    no_pairs_foot <- "No discordant pairs: both tests were correct on exactly the same cases, so McNemar's test has no information here; p is set to 1 and no test statistic is computed."
    four_foot <- c(midp = "Only 4 discordant pairs: with so few, McNemar's test cannot reach p < 0.05 whatever their split, so its non-significant result says nothing about whether the tests differ; read the accuracy difference and its paired confidence interval.",
                   asymptotic = "Only 4 discordant pairs, so McNemar's test has little power here; read the accuracy difference and its paired confidence interval.")
    excluded <- "Comparisons without discordant pairs are left out of the Holm-Bonferroni adjustment: their p = 1 is a convention, not a test result."
    cell <- function(tb, col) vapply(pairs, function(k) tb$getCell(rowKey = k, col = col)$value, if (col == "p") numeric(1) else "")
    for (m in names(raw)) {
        tb <- val_dc(d3, test3 = "t3", test3Positive = "+", statComp = TRUE, mcnemarMethod = m)$mcnemarTable
        expect_equal(unname(cell(tb, "p")), c(1, stats::p.adjust(rep(raw[[m]], 2), "holm")), tolerance = 1e-12, info = m)
        if (m == "midp") expect_equal(unname(cell(tb, "p")), c(1, 3 / 4, 3 / 4), tolerance = 1e-12)
        expect_identical(unname(cell(tb, "interpretation")), c(not_informative, tested, tested), info = m)
        expect_identical(unname(vapply(pairs, function(k) val_foot(tb, k, "p"), "")),
                         c(no_pairs_foot, four_foot[[m]], four_foot[[m]]), info = m)
        expect_identical(tb$notes[["mcnemar_uninformative_excluded"]]$note, excluded, info = m)
        expect_match(tb$notes[["mcnemar_multiplicity"]]$note, "Holm-Bonferroni adjusted across the 2 pairwise comparisons that were tested (those with discordant pairs).",
                     fixed = TRUE, info = m)
    }
    # two tests without discordant pairs: nothing was tested, so nothing was set aside either
    expect_false("mcnemar_uninformative_excluded" %in% names(val_dc(val_mc(10, 0, 0, 4), statComp = TRUE)$mcnemarTable$notes))
})

test_that("Recommended McNemar variants (Fagerland 2013): a last pair never observed together is reported as not computed, not as a failed test", {
    # Rows 1-20 carry t1 and t2 (paired cells 8, 2, 6, 4), rows 21-40 t1 and t3 (9, 7, 1, 3); t2 and t3 are never
    # both present, so the last of the three pairs cannot be tested and Cochran's Q has no complete case. Hand
    # mid-p: (2, 6) 2 * (1 + 8 + 28)/256 - 28/256 = 46/256; (7, 1) 2 * (1 + 8)/256 - 8/256 = 10/256, so Holm over
    # the two gives 46/256 and 20/256.
    blk <- function(a, b, c, d) list(x = rep(c(TRUE, TRUE, FALSE, FALSE), c(a, b, c, d)),
                                     y = rep(c(TRUE, FALSE, TRUE, FALSE), c(a, b, c, d)))
    b1 <- blk(8, 2, 6, 4); b2 <- blk(9, 7, 1, 3)
    d <- val_from_correct(list(t1 = c(b1$x, b2$x), t2 = c(b1$y, rep(TRUE, 20)), t3 = c(rep(TRUE, 20), b2$y)))
    d$t3[1:20] <- NA; d$t2[21:40] <- NA
    expect_equal(c(dc_mcnemar_midp(2, 6), dc_mcnemar_midp(7, 1)), c(46, 10) / 256, tolerance = 1e-12)
    for (m in c("midp", "asymptotic")) {
        res <- val_dc(d, test3 = "t3", test3Positive = "+", statComp = TRUE, mcnemarMethod = m)
        tb <- res$mcnemarTable
        expect_identical(unlist(tb$rowKeys), c("t1 vs t2", "t1 vs t3"), info = m)
        raw <- if (m == "midp") c(46, 10) / 256 else c(dc_mcnemar_asymptotic(2, 6)[["p"]], dc_mcnemar_asymptotic(7, 1)[["p"]])
        expect_equal(tb$asDF$p, stats::p.adjust(raw, "holm"), tolerance = 1e-12, info = m)
        if (m == "midp") expect_equal(tb$asDF$p, c(46, 20) / 256, tolerance = 1e-12)
        nt <- val_text(res$notices$content)
        expect_match(nt, "Pairwise Comparison Not Computed t2 vs t3 could not be tested: no case has a determinate result for both tests and the reference standard",
                     fixed = TRUE)
        for (bad in c("McNemar Test Unavailable", "subscript out of bounds", "Could not perform McNemar's test"))
            expect_false(grepl(bad, nt, fixed = TRUE), info = paste(m, bad))
    }
    # The same layout with no discordant pair in either computed pair (cells 8, 0, 0, 4 and 9, 0, 0, 3): t2 and t3
    # were never compared, so the Results Section must not say that the tests were right on the same cases.
    b1 <- blk(8, 0, 0, 4); b2 <- blk(9, 0, 0, 3)
    d0 <- val_from_correct(list(t1 = c(b1$x, b2$x), t2 = c(b1$y, rep(TRUE, 12)), t3 = c(rep(TRUE, 12), b2$y)))
    d0$t3[1:12] <- NA; d0$t2[13:24] <- NA
    for (m in c("midp", "asymptotic")) {
        r0 <- val_dc(d0, test3 = "t3", test3Positive = "+", statComp = TRUE, mcnemarMethod = m, showDescriptiveReport = TRUE)
        rs <- val_results(r0)
        expect_false(grepl("Methods Section:", rs, fixed = TRUE))   # the section was found
        expect_false(grepl("the tests were correct on exactly the same paired cases", rs, fixed = TRUE))
        # No test ran (both computed pairs are uninformative, t2 vs t3 and Cochran's Q never ran): no
        # copy-ready verdict, no "limited power" caution, and the Methods still name the test that was applied.
        expect_match(rs, "Statistical comparison was not informative here: none of the pairs of tests that could be compared had discordant pairs, so McNemar's test produced no test result (detailed results in the comparison tables).", fixed = TRUE, info = m)
        cr <- val_text(r0$clinicalReport$content)
        for (bad in c("did not reveal", "Because the differences in overall accuracy", "Caution: No statistically significant",
                      "No paired statistical comparison between the tests could be computed"))
            expect_false(grepl(bad, cr, fixed = TRUE), info = paste(m, bad))
        expect_match(val_methods(r0), if (m == "midp") "Diagnostic correctness was compared with the McNemar mid-p test" else
                     "Diagnostic correctness was compared with the asymptotic McNemar test without continuity correction", fixed = TRUE, info = m)
    }
})

test_that("VAL round 5: with one tested pair the notes say so and no Holm exclusion is claimed", {
    # Rows 1-12: t1 and t2 observed, no discordant pair (8, 0, 0, 4). Rows 13-32: t1 and t3 observed,
    # b = 7, c = 1. t2 vs t3 are never observed together. Only t1 vs t3 is tested, so no Holm.
    blk <- function(a, b, c, d) list(x = rep(c(TRUE, TRUE, FALSE, FALSE), c(a, b, c, d)),
                                     y = rep(c(TRUE, FALSE, TRUE, FALSE), c(a, b, c, d)))
    b1 <- blk(8, 0, 0, 4); b2 <- blk(9, 7, 1, 3)
    d <- val_from_correct(list(t1 = c(b1$x, b2$x), t2 = c(b1$y, rep(TRUE, 20)), t3 = c(rep(TRUE, 12), b2$y)))
    d$t3[1:12] <- NA; d$t2[13:32] <- NA
    for (m in c("midp", "asymptotic")) {
        res <- val_dc(d, test3 = "t3", test3Positive = "+", statComp = TRUE, mcnemarMethod = m)
        tb <- res$mcnemarTable
        raw <- if (m == "midp") dc_mcnemar_midp(7, 1) else dc_mcnemar_asymptotic(7, 1)[["p"]]
        expect_equal(tb$asDF$p[unlist(tb$rowKeys) == "t1 vs t3"], raw, tolerance = 1e-12, info = m)   # unadjusted
        expect_equal(tb$asDF$p[unlist(tb$rowKeys) == "t1 vs t2"], 1, info = m)
        expect_identical(tb$notes[["mcnemar_multiplicity"]]$note,
                         "Only one pairwise comparison was tested (had discordant pairs), so no multiplicity adjustment applies and the p-value shown is the unadjusted one.")
        expect_null(tb$notes[["mcnemar_uninformative_excluded"]])
        expect_identical(res$diffTable$notes[["diff_multiplicity"]]$note,
                         "Only one pairwise comparison was tested by McNemar's test, so neither this interval nor the McNemar p-value is adjusted for multiplicity.")
        expect_false(any(grepl("Holm-Bonferroni corrected", tb$asDF$interpretation, fixed = TRUE)), info = m)
    }
})

test_that("VAL round 5: paired accuracies round half away from zero, as the jamovi table does", {
    # val_mc(21, 15, 6, 6): t1 right on 36/48 = 75.0%, t2 on 27/48 = 56.25% -> 56.3 (sprintf alone gives 56.2).
    expect_identical(sprintf("%.1f", 27 / 48 * 100), "56.2")   # the trap this guards against
    res <- val_dc(val_mc(21, 15, 6, 6), statComp = TRUE, showSummary = TRUE, showReportSentence = TRUE)
    expect_match(val_text(res$summaryReport$content), "accuracy 75.0% vs 56.3%.", fixed = TRUE)
    expect_match(val_text(res$reportSentence$content), "accuracy was 75.0% for t1 and 56.3% for t2", fixed = TRUE)
    expect_false(grepl("56.2%", val_text(c(res$summaryReport$content, res$reportSentence$content)), fixed = TRUE))
})

test_that("Recommended McNemar variants (Fagerland 2013): Summary, Report, Methods and glossary name the test that ran", {
    d <- val_mc(1, 1, 7, 12)   # Bentur et al.: mid-p 0.039; asymptotic chi-squared 4.50, p 0.034
    run <- function(m) val_dc(d, statComp = TRUE, mcnemarMethod = m, showSummary = TRUE, showReportSentence = TRUE,
                              showDescriptiveReport = TRUE, showExplanations = TRUE)
    mid <- run("midp")
    sm <- val_text(mid$summaryReport$content)
    rp <- val_text(mid$reportSentence$content)
    expect_match(sm, "McNemar mid-p: p = 0.039 - Significant difference detected.", fixed = TRUE)
    expect_match(rp, "The McNemar mid-p test comparing t1 and t2 showed a significant difference in diagnostic accuracy (n = 21 paired cases; p = 0.039).",
                 fixed = TRUE)
    expect_false(grepl(val_chisq, paste(sm, rp), fixed = TRUE))
    # The variant's sentence follows the frame directly: the "Paired comparisons used the common
    # determinate rows." opener it used to repeat is gone.
    expect_match(val_methods(mid), "and overall accuracy. Diagnostic correctness was compared with the McNemar mid-p test, which Fagerland et al. (2013) recommend for use in any situation.",
                 fixed = TRUE)
    expect_false(grepl("asymptotic", val_methods(mid), fixed = TRUE))
    asy <- run("asymptotic")
    expect_match(val_text(asy$summaryReport$content),
                 paste0("McNemar asymptotic (no continuity correction): ", val_chisq, " = 4.50, p = 0.034 - Significant difference detected."),
                 fixed = TRUE)
    expect_match(val_text(asy$reportSentence$content),
                 paste0("The asymptotic McNemar test without continuity correction comparing t1 and t2 showed a significant difference in diagnostic accuracy (n = 21 paired cases; ",
                        val_chisq, "(1) = 4.50, p = 0.034)."), fixed = TRUE)
    expect_match(val_methods(asy), "and overall accuracy. Diagnostic correctness was compared with the asymptotic McNemar test without continuity correction, which Fagerland et al. (2013) recommend when small excesses over the nominal significance level are acceptable.",
                 fixed = TRUE)
    expect_false(grepl("mid-p", val_methods(asy), fixed = TRUE))
    for (res in list(mid, asy)) {
        expect_match(val_text(res$explanationsContent$content),
                     "The p-value comes from the McNemar mid-p test or, if chosen, the asymptotic test without continuity correction, the two versions recommended by Fagerland et al. (2013)",
                     fixed = TRUE)
        expect_false(grepl("Paired comparisons used the common determinate rows", val_methods(res), fixed = TRUE))
        expect_identical(lengths(regmatches(val_methods(res), gregexpr("common determinate rows", val_methods(res), fixed = TRUE))), 1L)
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): the non-significant Summary line and Report sentence in full, per variant", {
    # val_d1(): 12 paired cases, b = 2 (rows 4-5, only t1 right), c = 1 (row 11, only t2 right); accuracy
    # 9/12 vs 8/12. Mid-p 5/8 (VAL C35-C39). Asymptotic chi-squared (2 - 1)^2 / 3 = 1/3 on 1 df, p =
    # 2 * pnorm(-1 / sqrt(3)) = 0.5637. Newcombe 10 on the accuracy cells (7, 2, 1, 2): 8.3 (-22.5 to 37.3).
    expect_equal(round(100 * val_nc10(7, 2, 1, 2), 1), c(8.3, -22.5, 37.3))
    expect_equal(round(2 * stats::pnorm(-1 / sqrt(3)), 3), 0.564)
    rest <- paste("On these paired cases accuracy was 75.0% for t1 and 66.7% for t2; the difference, t1 minus t2, was 8.3 percentage points (95% CI -22.5 to 37.3; Newcombe's method 10).",
                  "A non-significant McNemar result does not establish that the two tests perform equally; the test uses only the discordant pairs and may lack power to detect a clinically important difference.")
    # The Summary line as a reader sees it: val_inline() keeps the spacing around <b> and <span>.
    caveat <- "No significant difference detected. This is not evidence that the two tests are equivalent: McNemar's test uses only the discordant pairs, so with few discordances it has little power to detect a real difference. Check the confidence interval for the accuracy difference to see how large a difference remains compatible with these data."
    mid_res <- val_dc(val_d1(), statComp = TRUE, showSummary = TRUE, showReportSentence = TRUE)
    expect_match(val_inline(mid_res$summaryReport$content), paste("McNemar mid-p: p = 0.625 -", caveat), fixed = TRUE)
    mid <- val_text(mid_res$reportSentence$content)
    expect_match(mid, paste("The McNemar mid-p test comparing t1 and t2 did not detect a significant difference in diagnostic accuracy (n = 12 paired cases; p = 0.625).",
                            rest), fixed = TRUE)
    asy_res <- val_dc(val_d1(), statComp = TRUE, mcnemarMethod = "asymptotic", showSummary = TRUE, showReportSentence = TRUE)
    expect_match(val_inline(asy_res$summaryReport$content),
                 paste0("McNemar asymptotic (no continuity correction): ", val_chisq, " = 0.33, p = 0.564 - ", caveat), fixed = TRUE)
    asy <- val_text(asy_res$reportSentence$content)
    expect_match(asy, paste0("The asymptotic McNemar test without continuity correction comparing t1 and t2 did not detect a significant difference in diagnostic accuracy (n = 12 paired cases; ",
                             val_chisq, "(1) = 0.33, p = 0.564). ", rest), fixed = TRUE)
})

test_that("Recommended McNemar variants (Fagerland 2013): the Report and the Summary say when McNemar's verdict and the Newcombe interval disagree", {
    borderline <- "This significant result is borderline: the 95 percent confidence interval for the accuracy difference comes from Newcombe's method 10, a different procedure from McNemar's test, and it includes 0, the value of no difference."
    disagree <- "The 95 percent confidence interval for the accuracy difference comes from Newcombe's method 10, a different procedure from McNemar's test, and it excludes 0, the value of no difference; the two procedures disagree on these data, so report both rather than either alone."
    # val_mc() cells (a, b, c, d), the variant, and the sentence expected; the accuracy cells are the same
    # (a, b, c, d), so val_nc10() gives the interval. The 2-test Summary carries the same sentence under
    # the same condition, as a paragraph of its own.
    fx <- list(list(k = c(1, 0, 4, 5), m = "asymptotic", want = "borderline"),   # chi-squared 16/4 = 4, p 0.0455
               list(k = c(2, 4, 12, 37), m = "midp", want = "borderline"),       # p 0.04904, upper limit 0.000198
               list(k = c(4, 0, 4, 2), m = "midp", want = "disagree"),           # p 1/16, interval excludes 0
               list(k = c(1, 0, 4, 5), m = "midp", want = "neither"),            # p 1/16, interval includes 0
               list(k = c(4, 0, 4, 2), m = "asymptotic", want = "neither"),      # p 0.0455, interval excludes 0
               list(k = c(1, 1, 7, 12), m = "midp", want = "neither"),           # Bentur: p 10/256, excludes 0
               list(k = c(1, 1, 7, 12), m = "asymptotic", want = "neither"),     # Bentur: p 0.0339
               # lower limit 0.00029 > 0 but 0.0 to 1 dp: deciding on rounded limits would flip both rows
               list(k = c(9, 4, 0, 5), m = "midp", want = "disagree"),           # p 1/16
               list(k = c(9, 4, 0, 5), m = "asymptotic", want = "neither"))      # p 0.0455
    for (f in fx) {
        k <- f$k
        info <- paste(paste(k, collapse = ","), f$m)
        # the verdict from the oracles alone: the helper p-value and the transcribed Newcombe limits
        p <- if (f$m == "midp") dc_mcnemar_midp(k[2], k[3]) else dc_mcnemar_asymptotic(k[2], k[3])[["p"]]
        ci <- val_nc10(k[1], k[2], k[3], k[4])[2:3]
        excludes_0 <- ci[1] > 0 || ci[2] < 0
        expect_identical(if (p < 0.05 && !excludes_0) "borderline" else if (p >= 0.05 && excludes_0) "disagree" else "neither",
                         f$want, info = info)
        res <- val_dc(do.call(val_mc, as.list(k)), statComp = TRUE, mcnemarMethod = f$m, showSummary = TRUE,
                      showReportSentence = TRUE)
        rp <- val_text(res$reportSentence$content)
        sm <- res$summaryReport$content
        expect_identical(c(grepl(borderline, rp, fixed = TRUE), grepl(disagree, rp, fixed = TRUE)),
                         c(f$want == "borderline", f$want == "disagree"), info = info)
        expect_identical(c(grepl(paste0("<p>", borderline, "</p>"), sm, fixed = TRUE),
                           grepl(paste0("<p>", disagree, "</p>"), sm, fixed = TRUE)),
                         c(f$want == "borderline", f$want == "disagree"), info = paste("Summary", info))
    }
    # The UNROUNDED limits decide (the printed ones are in the near-zero-limit test below):
    # (2, 4, 12, 37) has upper limit 0.000198 and (9, 4, 0, 5) lower limit 0.00029.
    expect_true(val_nc10(2, 4, 12, 37)[3] > 0 && val_nc10(2, 4, 12, 37)[3] < 5e-4)
    expect_true(val_nc10(9, 4, 0, 5)[2] > 0 && val_nc10(9, 4, 0, 5)[2] < 5e-4)
    # never without discordant pairs
    for (m in c("midp", "asymptotic")) {
        res <- val_dc(val_mc(10, 0, 0, 4), statComp = TRUE, mcnemarMethod = m, showSummary = TRUE,
                      showReportSentence = TRUE)
        txt <- val_text(c(res$summaryReport$content, res$reportSentence$content))
        expect_false(grepl(borderline, txt, fixed = TRUE) || grepl(disagree, txt, fixed = TRUE), info = m)
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): the Report prints the difference and its limits by one rule", {
    # One decimal, except that a nonzero value within 0.05 percentage points of 0 keeps two significant digits
    # (one decimal would print it as 0.0 or -0.0 and contradict the sentence beside it), and 0 prints as 0.0.
    # Values from val_nc10(), in percentage points: (9, 4, 0, 5) 22.22 (0.0291 to 41.07), its mirror (9, 0, 4, 5),
    # (2, 4, 12, 37) -14.55 (-28.65 to 0.0198), (5, 3, 3, 5) exactly 0 (-29.10 to 29.10) and (1000, 0, 1, 1000)
    # -100/2001 = -0.04998 (-0.188 to 0.088). Accuracy, hand: (9, 4, 0, 5) 13/18 = 72.2% vs 9/18 = 50.0%;
    # (2, 4, 12, 37) 6/55 = 10.9% vs 14/55 = 25.5%; (5, 3, 3, 5) 8/16 both; (1000, 0, 1, 1000) 49.98% vs 50.02%.
    printed <- function(k) vapply(100 * do.call(val_nc10, as.list(k)), function(x)
        if (x == 0) "0.0" else if (abs(x) >= 0.05) sprintf("%.1f", x) else sprintf(paste0("%.", 1 - floor(log10(abs(x))), "f"), x), "")
    expect_identical(printed(c(9, 4, 0, 5)), c("22.2", "0.029", "41.1"))
    expect_identical(printed(c(9, 0, 4, 5)), c("-22.2", "-41.1", "-0.029"))
    expect_identical(printed(c(2, 4, 12, 37)), c("-14.5", "-28.6", "0.020"))
    expect_identical(printed(c(5, 3, 3, 5)), c("0.0", "-29.1", "29.1"))
    expect_identical(printed(c(1000, 0, 1, 1000)), c("-0.050", "-0.2", "0.1"))
    expect_identical(printed(c(7, 2, 1, 2)), c("8.3", "-22.5", "37.3"))   # away from 0: 1 decimal (val_d1())
    expect_identical(val_nc10(5, 3, 3, 5)[1], 0)
    shown <- list(
        list(k = c(9, 4, 0, 5), s = "On these paired cases accuracy was 72.2% for t1 and 50.0% for t2; the difference, t1 minus t2, was 22.2 percentage points (95% CI 0.029 to 41.1; Newcombe's method 10)."),
        list(k = c(9, 0, 4, 5), s = "On these paired cases accuracy was 50.0% for t1 and 72.2% for t2; the difference, t1 minus t2, was -22.2 percentage points (95% CI -41.1 to -0.029; Newcombe's method 10)."),
        list(k = c(5, 3, 3, 5), s = "On these paired cases accuracy was 50.0% for t1 and 50.0% for t2; the difference, t1 minus t2, was 0.0 percentage points (95% CI -29.1 to 29.1; Newcombe's method 10)."),
        list(k = c(1000, 0, 1, 1000), s = "On these paired cases accuracy was 50.0% for t1 and 50.0% for t2; the difference, t1 minus t2, was -0.050 percentage points (95% CI -0.2 to 0.1; Newcombe's method 10)."))
    for (m in c("midp", "asymptotic")) {
        for (x in shown) {
            info <- paste(m, paste(x$k, collapse = ","))
            rp <- val_text(val_dc(do.call(val_mc, as.list(x$k)), statComp = TRUE, mcnemarMethod = m,
                                  showReportSentence = TRUE)$reportSentence$content)
            expect_match(rp, x$s, fixed = TRUE, info = info)
            # no number prints as -0.0 (a -0.050 is not one)
            expect_false(grepl("-0\\.0(?![0-9])", rp, perl = TRUE), info = info)
        }
    }
    rp <- val_text(val_dc(val_mc(2, 4, 12, 37), statComp = TRUE, showReportSentence = TRUE)$reportSentence$content)
    expect_match(rp, paste("(n = 55 paired cases; p = 0.049). On these paired cases accuracy was 10.9% for t1 and 25.5% for t2; the difference, t1 minus t2, was -14.5 percentage points (95% CI -28.6 to 0.020; Newcombe's method 10).",
                           "This significant result is borderline: the 95 percent confidence interval for the accuracy difference comes from Newcombe's method 10, a different procedure from McNemar's test, and it includes 0, the value of no difference."),
                 fixed = TRUE)
})

test_that("Recommended McNemar variants (Fagerland 2013): copy-ready p rounds half up, as the jamovi table does", {
    # val_mc(5, 4, 0, 5): b = 4, c = 0, mid-p 2 * (f(0) - f(4)/2) = 2 * (1/16 - 1/32) = 1/16 = 0.0625, an exact
    # binary tie at the third decimal. The table shows 0.063 (round half up); sprintf("%.3f") alone rounds the
    # tie to even and printed 0.062 in the Summary and Report.
    res <- val_dc(val_mc(5, 4, 0, 5), statComp = TRUE, showSummary = TRUE, showReportSentence = TRUE)
    expect_identical(res$mcnemarTable$asDF$p, 1 / 16)
    sm <- val_text(res$summaryReport$content)
    rp <- val_text(res$reportSentence$content)
    expect_match(sm, "McNemar mid-p: p = 0.063 - No significant difference detected.", fixed = TRUE)
    expect_match(rp, "The McNemar mid-p test comparing t1 and t2 did not detect a significant difference in diagnostic accuracy (n = 14 paired cases; p = 0.063).",
                 fixed = TRUE)
    expect_false(grepl("0.062", paste(sm, rp), fixed = TRUE))
    # the exact binary ties at the third decimal are k/16 with k odd; half up: 0.0625 -> 0.063, ..., 0.9375 -> 0.938
    a <- ClinicoPath:::decisioncompareClass$new(
        options = ClinicoPath:::decisioncompareOptions$new(gold = "gold", goldPositive = "D", test1 = "t1",
            test1Positive = "+", test2 = "t2", test2Positive = "+", test3 = NULL, test3Positive = NULL, stratify = NULL),
        data = val_d1())
    f <- a$.__enclos_env__$private$.formatP
    expect_identical(vapply(seq(1, 15, 2) / 16, f, ""),
                     c("= 0.063", "= 0.188", "= 0.313", "= 0.438", "= 0.563", "= 0.688", "= 0.813", "= 0.938"))
})

test_that("Recommended McNemar variants (Fagerland 2013): mid-p rows leave the statistic and df cells empty, and a note says why", {
    note <- "The McNemar mid-p test is computed directly from the binomial distribution of the discordant pairs, so it has no chi-squared statistic or degrees of freedom; those cells are empty in its rows."
    d <- val_mc(1, 1, 7, 12)
    mid <- val_dc(d, statComp = TRUE)$mcnemarTable
    asy <- val_dc(d, statComp = TRUE, mcnemarMethod = "asymptotic")$mcnemarTable
    expect_identical(mid$notes[["mcnemar_midp_nostat"]]$note, note)
    expect_false("mcnemar_midp_nostat" %in% names(asy$notes))
    # the raw cells are NULL, which jamovi renders empty; asDF shows them as NA
    expect_null(mid$getCell(rowKey = "t1 vs t2", col = "stat")$value)
    expect_null(mid$getCell(rowKey = "t1 vs t2", col = "df")$value)
    expect_true(is.na(mid$asDF$stat) && is.na(mid$asDF$df))
    expect_equal(c(asy$getCell(rowKey = "t1 vs t2", col = "stat")$value, asy$getCell(rowKey = "t1 vs t2", col = "df")$value),
                 c(36 / 8, 1), tolerance = 1e-12)
    # no case has both tests, so no pair is computable and there is no mid-p row to explain
    nopair <- val_mc(5, 2, 3, 6)
    nopair$t1[1:8] <- NA
    nopair$t2[9:16] <- NA
    none <- val_dc(nopair, statComp = TRUE)$mcnemarTable
    expect_equal(none$rowCount, 0)
    expect_false("mcnemar_midp_nostat" %in% names(none$notes))
})

test_that("Recommended McNemar variants (Fagerland 2013): with three tests the post-hoc sentence names the variant that ran", {
    # Hand: correct per test 35, 20, 20 (T = 75); per case 3 right (20 cases), 1 (15), 0 (5), so Cochran's
    # Q = 2 * (3 * (35^2 + 20^2 + 20^2) - 75^2) / (3 * 75 - (20 * 9 + 15)) = 900 / 30 = 30. Exact tail: all 15 correct results in one column, 3/3^15.
    d3 <- val_from_correct(list(t1 = rep(c(TRUE, TRUE, FALSE), c(20, 15, 5)),
                                t2 = rep(c(TRUE, FALSE, FALSE), c(20, 15, 5)),
                                t3 = rep(c(TRUE, FALSE, FALSE), c(20, 15, 5))))
    post <- c(midp = "Post-hoc pairwise comparisons used the McNemar mid-p test with Holm-Bonferroni correction to identify specific differences between tests.",
              asymptotic = "Post-hoc pairwise comparisons used the asymptotic McNemar test without continuity correction, with Holm-Bonferroni correction, to identify specific differences between tests.")
    for (m in names(post)) {
        res <- val_dc(d3, test3 = "t3", test3Positive = "+", statComp = TRUE, mcnemarMethod = m, showReportSentence = TRUE)
        mc <- res$mcnemarTable$asDF
        q <- mc[mc$method == "Cochran's Q", , drop = FALSE]
        expect_equal(c(q$n, q$stat, q$df), c(40, 30, NA), tolerance = 1e-12)
        expect_equal(q$p, 3^(-14), tolerance = 1e-10)
        rp <- val_text(res$reportSentence$content)
        expect_match(rp, paste0("Cochran's Q test revealed a significant difference in diagnostic accuracy among the three tests (n = 40 cases with all three results; ",
                                "Q = 30.00, p < 0.001). ", post[[m]]), fixed = TRUE)
        expect_false(grepl(post[[setdiff(names(post), m)]], rp, fixed = TRUE), info = m)
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): a test named 'Cochrane' is not read as the Cochran's Q row", {
    # 60 cases in three blocks of 20, each block missing one test, so no case has all three results and
    # Cochran's Q is not computed. The first row is then the pair "Cochrane vs t2"; matching "cochran" in
    # the row key took it for the Cochran row, which crashed under mid-p (empty statistic cell) and, under
    # the asymptotic test, reported the pairwise result as Cochran's Q. Paired cells per block (both right,
    # only the first right, only the second right, both wrong): t2 vs t3 on rows 1-20 (8, 2, 6, 4);
    # Cochrane vs t3 on rows 21-40 (9, 7, 1, 3); Cochrane vs t2 on rows 41-60 (6, 5, 5, 4).
    blk <- function(a, b, c, d) list(x = rep(c(TRUE, TRUE, FALSE, FALSE), c(a, b, c, d)),
                                     y = rep(c(TRUE, FALSE, TRUE, FALSE), c(a, b, c, d)))
    b1 <- blk(8, 2, 6, 4); b2 <- blk(9, 7, 1, 3); b3 <- blk(6, 5, 5, 4)
    d <- val_from_correct(list(Cochrane = c(rep(TRUE, 20), b2$x, b3$x), t2 = c(b1$x, rep(TRUE, 20), b3$y),
                               t3 = c(b1$y, b2$y, rep(TRUE, 20))))
    d$Cochrane[1:20] <- NA; d$t2[21:40] <- NA; d$t3[41:60] <- NA
    pairs <- c("Cochrane vs t2", "Cochrane vs t3", "t2 vs t3")
    bc <- list(c(5, 5), c(7, 1), c(2, 6))
    for (m in c("midp", "asymptotic")) {
        res <- NULL
        expect_no_error(res <- val_dc(d, test1 = "Cochrane", test2 = "t2", test3 = "t3", test3Positive = "+",
                                      statComp = TRUE, mcnemarMethod = m, showSummary = TRUE, showReportSentence = TRUE))
        if (is.null(res)) next   # the crash is the failure; the other variant is still checked
        # the three pairwise rows, each on its own 20 complete cases, Holm-adjusted over 3; no Cochran row
        tb <- res$mcnemarTable
        expect_identical(unlist(tb$rowKeys), pairs, info = m)
        raw <- vapply(bc, function(x) if (m == "midp") dc_mcnemar_midp(x[1], x[2]) else dc_mcnemar_asymptotic(x[1], x[2])[["p"]],
                      numeric(1))
        expect_equal(tb$asDF$n, rep(20, 3))
        expect_equal(tb$asDF$p, stats::p.adjust(raw, "holm"), tolerance = 1e-12)
        sm <- val_text(res$summaryReport$content)
        rp <- val_text(res$reportSentence$content)
        expect_match(sm, "The global Cochran's Q test could not be computed for these data; the reason is given in the Important Information panel. The pairwise comparisons and their confidence intervals in the tables above are unaffected.",
                     fixed = TRUE)
        expect_match(rp, "The global Cochran's Q test could not be computed for these data; the reason is given in the Important Information panel. Report the pairwise comparisons and their confidence intervals instead.",
                     fixed = TRUE)
        expect_match(val_text(res$notices$content), "Cochran's Q was not computed: no case has a determinate result for all three tests and the reference standard.",
                     fixed = TRUE)
        for (bad in c("Overall comparison", "Cochran's Q test (", "Cochran's Q test revealed", "Cochran's Q test did not detect",
                      "cases with all three results", "highest observed balanced accuracy", val_chisq))
            expect_false(grepl(bad, paste(sm, rp), fixed = TRUE), info = paste(m, bad))
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): no displayed text describes the retired exact / corrected rule", {
    shown <- function(res) {
        tb <- res$mcnemarTable
        cells <- unlist(lapply(tb$rowKeys, function(k) lapply(c("comparison", "method", "interpretation", "p"), function(col) {
            cell <- tb$getCell(rowKey = k, col = col)
            c(as.character(cell$value), cell$footnotes)
        })))
        notes <- vapply(c(as.list(tb$notes), as.list(res$diffTable$notes)), function(n) n$note, character(1))
        val_text(c(cells, notes, res$summaryReport$content, res$reportSentence$content, res$clinicalReport$content,
                   res$explanationsContent$content, res$aboutAnalysis$content, res$notices$content))
    }
    panels <- list(statComp = TRUE, showSummary = TRUE, showReportSentence = TRUE, showDescriptiveReport = TRUE,
                   showExplanations = TRUE)
    # 8 discordant pairs, none, 30 (b = 5, c = 25: the old chi-square zone), and three tests (Holm, Cochran)
    fixtures <- list(list(data = val_mc(1, 1, 7, 12)), list(data = val_mc(10, 0, 0, 4)), list(data = val_d2()),
                     list(data = val_d3(), test3 = "t3", test3Positive = "+"))
    for (m in c("midp", "asymptotic")) {
        for (fx in fixtures) {
            txt <- shown(do.call(val_dc, c(fx, panels, list(mcnemarMethod = m))))
            expect_match(txt, "Fagerland et al. (2013)", fixed = TRUE)
            for (bad in c("exact p", "exact binomial", "25 discordant", "continuity corrected", "continuity-corrected"))
                expect_false(grepl(bad, txt, ignore.case = TRUE), info = paste(m, bad))
        }
    }
})

test_that("Recommended McNemar variants (Fagerland 2013): mid-p agrees with exact2x2 away from ties and with coin at ties", {
    skip_if_not_installed("exact2x2")
    skip_if_not_installed("coin")
    # exact2x2's two-sided mid-p follows the central rule and returns 1 at a tie, where eq. (6) gives
    # 1 - f/2; coin's midpvalue() follows eq. (6), so it is the reference for the ties.
    for (bc in list(c(1, 7), c(6, 16), c(0, 5), c(3, 12), c(10, 20), c(24, 25), c(100, 130))) {
        mc <- val_dc(val_mc(3, bc[1], bc[2], 4), statComp = TRUE)$mcnemarTable$asDF
        ref <- exact2x2::exact2x2(matrix(c(3, bc[2], bc[1], 4), 2), paired = TRUE, midp = TRUE)$p.value
        expect_equal(mc$p, ref, tolerance = 1e-10)
    }
    for (k in c(1, 3, 9)) {
        mc <- val_dc(val_mc(3, k, k, 4), statComp = TRUE)$mcnemarTable$asDF
        tb <- as.table(matrix(c(3, k, k, 4), 2, dimnames = list(t1 = c("right", "wrong"), t2 = c("right", "wrong"))))
        expect_equal(mc$p, as.numeric(coin::midpvalue(coin::mh_test(tb, distribution = "exact"))), tolerance = 1e-10)
    }
})

test_that("the static notes of the McNemar and OPA tables do not end in whitespace", {
    # The notes compiled from the .r.yaml (a folded block that keeps its final newline would end in one).
    r <- ClinicoPath:::decisioncompareResults$new(options = ClinicoPath:::decisioncompareOptions$new(
        gold = "gold", goldPositive = "D", test1 = "t1", test1Positive = "+", test2 = "t2", test2Positive = "+",
        test3 = NULL, test3Positive = NULL, stratify = NULL))
    for (tb in list(mcnemarTable = r$mcnemarTable, opaTable = r$opaTable)) {
        expect_identical(names(tb$notes), "note", info = tb$name)
        txt <- tb$notes$note$note
        expect_true(is.character(txt) && length(txt) == 1 && nzchar(txt), info = tb$name)
        expect_false(grepl("[[:space:]]$", txt), info = tb$name)
    }
})


test_that("the Differences table footnote names the four paired cells it counts", {
    # Shown when n < 30 or a cell is under 5. Cells (both, first only, second only, neither), hand: val_d1()
    # sensitivity 3, 2, 0, 1 (n = 6), specificity 4, 0, 1, 1 (n = 6), accuracy 7, 2, 1, 2 (n = 12); val_d2() on
    # its 100 paired rows sensitivity 25, 3, 12, 10 and specificity 25, 2, 13, 10 (n = 50 each), accuracy
    # 50, 5, 25, 20 (n = 100, no cell under 5, so no footnote).
    # Wording since t6 (2026-10-02): the footnote reports the counts and no longer claims the interval
    # is "unstable" -- a cell below 5 can be a concordant one, and few discordant pairs do not by
    # themselves make Newcombe's interval wide.
    foot <- function(n, k) sprintf("Small paired sample or a cell below 5 (n=%d; paired counts both, first only, second only, neither: %s). The interval rests on these counts.",
                                   n, paste(k, collapse = ", "))
    dt <- val_dc(val_d1(), statComp = TRUE)$diffTable
    expect_identical(val_foot(dt, "t1 vs t2_Sens", "diff"),
                     "Small paired sample or a cell below 5 (n=6; paired counts both, first only, second only, neither: 3, 2, 0, 1). The interval rests on these counts.")
    expect_identical(val_foot(dt, "t1 vs t2_Spec", "diff"), foot(6, c(4, 0, 1, 1)))
    expect_identical(val_foot(dt, "t1 vs t2_AccurT", "diff"), foot(12, c(7, 2, 1, 2)))
    dt2 <- val_dc(val_d2(), statComp = TRUE)$diffTable
    expect_identical(val_foot(dt2, "t1 vs t2_Sens", "diff"), foot(50, c(25, 3, 12, 10)))
    expect_identical(val_foot(dt2, "t1 vs t2_Spec", "diff"), foot(50, c(25, 2, 13, 10)))
    expect_identical(val_foot(dt2, "t1 vs t2_AccurT", "diff"), "")
})


# ---- defects found while testing round 3, fixed in round 4 (skip()-guarded until then) ------------

test_that("VAL round 3 (fixed in round 4): a nonzero Newcombe limit under 0.0005 percentage points prints as a positive number", {
    # val_mc(21, 15, 6, 6): mid-p 0.0525, not significant, and the Newcombe lower limit is +0.000174
    # percentage points, so the interval excludes 0 and the Report says so; the printed lower limit
    # must then be a positive number, not 0.000. Two significant digits: 0.00017. val_nc10(): difference
    # 9/48 = 18.75, upper limit 35.78.
    nc <- 100 * val_nc10(21, 15, 6, 6)
    expect_true(nc[2] > 0 && nc[2] < 5e-4)
    expect_identical(c(sprintf("%.1f", nc[1]), sprintf("%.5f", nc[2]), sprintf("%.1f", nc[3])), c("18.8", "0.00017", "35.8"))
    expect_gt(dc_mcnemar_midp(15, 6), 0.05)
    for (m in c("midp", "asymptotic")) {
        rp <- val_text(val_dc(val_mc(21, 15, 6, 6), statComp = TRUE, mcnemarMethod = m,
                              showReportSentence = TRUE)$reportSentence$content)
        expect_match(rp, "the difference, t1 minus t2, was 18.8 percentage points (95% CI 0.00017 to 35.8; Newcombe's method 10).",
                     fixed = TRUE, info = m)
        # Anchored on the accuracy sentence ("...; Newcombe's method 10)"): since IR-decisioncompare-02
        # the Report also prints the sensitivity and specificity intervals, so the last "(95% CI" in
        # the paragraph is no longer the accuracy one.
        expect_gt(as.numeric(sub(".*\\(95% CI ([^ ]+) to [^;]+; Newcombe's method 10\\).*", "\\1", rp)), 0)
        # not significant under mid-p, so that Report states the disagreement with the interval
        if (m == "midp") expect_match(rp, "and it excludes 0, the value of no difference;", fixed = TRUE)
    }
})

test_that("VAL round 3 (fixed in round 4): without discordant pairs the Performance Summary does not report a non-significant result", {
    # The Results Section of the same panel says the comparison was not informative (b + c = 0); the
    # Caution paragraph below it must not then call the result non-significant or blame limited power.
    for (m in c("midp", "asymptotic")) {
        cr <- val_text(val_dc(val_mc(10, 0, 0, 4), statComp = TRUE, mcnemarMethod = m,
                              showDescriptiveReport = TRUE)$clinicalReport$content)
        expect_match(cr, "Statistical comparison was not informative here", fixed = TRUE)
        expect_false(grepl("No statistically significant difference in overall accuracy was detected", cr, fixed = TRUE), info = m)
        expect_false(grepl("non-significant result may simply reflect limited power", cr, fixed = TRUE), info = m)
    }
})
