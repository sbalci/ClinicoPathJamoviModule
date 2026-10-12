# Validation regression tests for decisioncombine (/validate-function, 2026-09-27).
#
# Each test carries the claim ID of development-ideas/decisioncombine-validation-2026-09-27.md
# and names its oracle. Expected values are hand arithmetic written out in the comments, or an
# independent implementation (stats::prop.test, epiR::epi.tests -- which decisioncombine does
# not call). The VAL- tests at the end were written skip-guarded against the open defects and
# went live when the fixes landed (2026-09-28).

decisioncombineClass <- getFromNamespace("decisioncombineClass", "ClinicoPath")
decisioncombineOptions <- getFromNamespace("decisioncombineOptions", "ClinicoPath")

# Patient-level data from pattern counts; factors typed as jamovi delivers them.
.val_mk <- function(cD, cN, pos = "pos", neg = "neg") {
    rows <- list()
    for (g in c("D", "N")) {
        cnt <- if (g == "D") cD else cN
        for (p in names(cnt)) {
            n <- cnt[[p]]
            if (n == 0) next
            s <- strsplit(p, "/", fixed = TRUE)[[1]]
            m <- as.data.frame(matrix(rep(ifelse(s == "+", pos, neg), each = n), nrow = n),
                               stringsAsFactors = FALSE)
            names(m) <- paste0("t", seq_along(s))
            rows[[length(rows) + 1L]] <- cbind(gold = rep(g, n), m, stringsAsFactors = FALSE)
        }
    }
    d <- do.call(rbind, rows)
    d$gold <- factor(d$gold, levels = c("D", "N"))
    for (j in grep("^t", names(d), value = TRUE)) d[[j]] <- factor(d[[j]], levels = c(pos, neg))
    rownames(d) <- NULL
    d
}

# Every Level argument passed explicitly; do.call() so string values are not read as symbols.
.val_run <- function(d, gold = "gold", gp = "D", t1 = "t1", t1p = "pos", t2 = "t2", t2p = "pos",
                     t3 = NULL, t3p = NULL, ...) {
    args <- c(list(data = d, gold = gold, goldPositive = gp, test1 = t1, test1Positive = t1p,
                   test2 = t2, test2Positive = t2p, test3 = t3, test3Positive = t3p), list(...))
    suppressWarnings(suppressMessages(do.call(ClinicoPath::decisioncombine, args)))
}

.val_row <- function(df, key) {
    k <- gsub('^"|"$', "", rownames(df))
    r <- df[k == key, , drop = FALSE]
    if (nrow(r) != 1L) stop("row key '", key, "' matched ", nrow(r), " rows")
    r
}

.val_titles <- function(res) {
    h <- res$notices$content
    if (is.null(h) || !nzchar(h)) return(character())
    s <- regmatches(h, gregexpr("<strong>[^<]*</strong>", h))[[1]]
    s <- gsub("&gt;", ">", gsub("&amp;", "&", gsub("</?strong>", "", s), fixed = TRUE), fixed = TRUE)
    sub("^[^:]*: ", "", s)
}

.val_text <- function(res) {
    h <- res$notices$content
    if (is.null(h)) return("")
    gsub("&gt;", ">", gsub("&amp;", "&", gsub("<[^>]+>", " ", h), fixed = TRUE), fixed = TRUE)
}

# F1: n = 20. Cells (tp fp fn tn): +/+ 4 1 6 9 | +/- 4 2 6 8 | -/+ 1 1 9 9 | -/- 1 6 9 4
#     Test 1 alone (+/+ and +/-) 8 3 2 7 | Test 2 alone (+/+ and -/+) 5 2 5 8
#     Parallel 9 4 1 6 | Serial = +/+
.F1 <- list(cD = c("+/+" = 4, "+/-" = 4, "-/+" = 1, "-/-" = 1),
            cN = c("+/+" = 1, "+/-" = 2, "-/+" = 1, "-/-" = 6))
# F2: k = 3, n = 80; zero cells in "+/-/+" (fp = 0) and "-/+/-" (tp = 0)
.F2 <- list(cD = c("+/+/+" = 12, "+/+/-" = 5, "+/-/+" = 4, "+/-/-" = 2, "-/+/+" = 3, "-/+/-" = 0, "-/-/+" = 1, "-/-/-" = 3),
            cN = c("+/+/+" = 1, "+/+/-" = 2, "+/-/+" = 0, "+/-/-" = 4, "-/+/+" = 2, "-/+/-" = 5, "-/-/+" = 6, "-/-/-" = 30))
# F4: realistic two-test sample, n = 200, no zero cell in any row
.F4 <- list(cD = c("+/+" = 41, "+/-" = 9, "-/+" = 7, "-/-" = 3),
            cN = c("+/+" = 4, "+/-" = 11, "-/+" = 15, "-/-" = 110))
PAR <- "Parallel (>=1 pos)"; SER <- "Serial (all pos)"; MAJ <- "Majority (>=2/3 pos)"


test_that("C03-C14 known answers on a 20-patient two-test table (hand arithmetic)", {
    tb <- .val_run(.val_mk(.F1$cD, .F1$cN))$combinationTable$asDF
    # patterns, then each test alone, then the strategies; all on the same 20 joint cases
    expect_equal(gsub('^"|"$', "", rownames(tb)),
                 c("+/+", "+/-", "-/+", "-/-", "Test 1 alone", "Test 2 alone", PAR, SER))
    expect_equal(tb$rowType, c(rep("Pattern", 4), "Single test", "Single test", "Strategy", "Strategy"))
    # Test 1 alone: tp 4+4 = 8, fp 1+2 = 3, fn 1+1 = 2, tn 1+6 = 7 -> sens .8, spec .7, PPV 8/11,
    #   NPV 7/9, J .5, LR+ .8/.3 = 8/3, LR- .2/.7 = 2/7, DOR 8*7/(3*2) = 28/3
    # Test 2 alone: tp 4+1 = 5, fp 1+1 = 2, fn 4+1 = 5, tn 2+6 = 8 -> sens .5, spec .8, PPV 5/7,
    #   NPV 8/13, J .3, LR+ .5/.2 = 2.5, LR- .5/.8 = .625, DOR 5*8/(2*5) = 4
    expect_equal(tb$tp, c(4, 4, 1, 1, 8, 5, 9, 4))
    expect_equal(tb$fp, c(1, 2, 1, 6, 3, 2, 4, 1))
    expect_equal(tb$sens, c(.4, .4, .1, .1, .8, .5, .9, .4), tolerance = 1e-12)
    expect_equal(tb$spec, c(.9, .8, .9, .4, .7, .8, .6, .9), tolerance = 1e-12)
    expect_equal(tb$ppv, c(4/5, 4/6, 1/2, 1/7, 8/11, 5/7, 9/13, 4/5), tolerance = 1e-12)
    expect_equal(tb$npv, c(9/15, 8/14, 9/18, 4/13, 7/9, 8/13, 6/7, 9/15), tolerance = 1e-12)
    expect_equal(tb$youden, c(.3, .2, 0, -.5, .5, .3, .5, .3), tolerance = 1e-12)
    expect_equal(tb$lrPos, c(4, 2, 1, 1/6, 8/3, 2.5, 2.25, 4), tolerance = 1e-12)
    expect_equal(tb$lrNeg, c(2/3, .75, 1, 2.25, 2/7, .625, 1/6, 2/3), tolerance = 1e-12)
    expect_equal(tb$dor, c(6, 8/3, 1, 2/27, 28/3, 4, 13.5, 6), tolerance = 1e-12)
})

test_that("C17 Wilson intervals equal stats::prop.test(correct = FALSE) on every proportion", {
    res <- .val_run(.val_mk(.F4$cD, .F4$cN))
    tb <- res$combinationTable$asDF
    ci <- res$combinationTableCI$asDF
    for (k in gsub('^"|"$', "", rownames(tb))) {
        rw <- .val_row(tb, k)
        xn <- list(sens = c(rw$tp, rw$tp + rw$fn), spec = c(rw$tn, rw$fp + rw$tn),
                   ppv = c(rw$tp, rw$tp + rw$fp), npv = c(rw$tn, rw$fn + rw$tn),
                   acc = c(rw$tp + rw$tn, rw$tp + rw$fp + rw$fn + rw$tn))
        for (st in names(xn)) {
            obs <- unlist(.val_row(ci, paste0(k, "_", st))[, c("lower", "upper")])
            expect_equal(unname(obs), stats::prop.test(xn[[st]][1], xn[[st]][2], correct = FALSE)$conf.int[1:2],
                         tolerance = 1e-9, label = paste(k, st))
        }
    }
})

test_that("C18-C20 LR+, LR- and DOR with log-scale CIs equal epiR::epi.tests on uncorrected rows", {
    skip_if_not_installed("epiR")
    # epiR is independent here: decisioncombine no longer calls it. Simel et al. 1991 (LR),
    # Woolf (DOR). F4 has no zero cell, so no continuity convention is involved.
    res <- .val_run(.val_mk(.F4$cD, .F4$cN))
    tb <- res$combinationTable$asDF
    rt <- res$combinationTableCIRatios$asDF
    for (k in gsub('^"|"$', "", rownames(tb))) {
        rw <- .val_row(tb, k)
        et <- epiR::epi.tests(as.table(matrix(c(rw$tp, rw$fp, rw$fn, rw$tn), 2, byrow = TRUE)),
                              method = "wilson", digits = 4)$detail
        for (m in list(c("lrPos", "lr.pos"), c("lrNeg", "lr.neg"), c("dor", "diag.or"))) {
            obs <- unlist(.val_row(rt, paste0(k, "_", m[1]))[, c("estimate", "lower", "upper")])
            expect_equal(unname(obs), unname(unlist(et[et$statistic == m[2], c("est", "lower", "upper")])),
                         tolerance = 1e-8, label = paste(k, m[1]))
        }
    }
})

test_that("C15 Haldane-Anscombe 0.5 correction on zero-cell rows, proportions kept observed", {
    tb <- .val_run(.val_mk(.F2$cD, .F2$cN), t3 = "t3", t3p = "pos")$combinationTable$asDF
    # "+/-/+": tp 4 fp 0 fn 26 tn 50 -> 4.5 0.5 26.5 50.5
    r <- .val_row(tb, "+/-/+")
    expect_equal(c(r$lrPos, r$lrNeg, r$dor),
                 c(229.5 / 15.5, (26.5 / 31) / (50.5 / 51), 4.5 * 50.5 / (0.5 * 26.5)), tolerance = 1e-12)
    expect_equal(c(r$sens, r$spec), c(4/30, 1), tolerance = 1e-12)
})

test_that("C16/C21 a pattern no patient shows: LR+ and DOR blank, LR- exactly 1 without an interval", {
    # test 2 copies test 1, so "+/-" is empty: tp 0 fp 0 fn 20 tn 30
    res <- .val_run(.val_mk(c("+/+" = 15, "+/-" = 0, "-/+" = 0, "-/-" = 5),
                            c("+/+" = 3, "+/-" = 0, "-/+" = 0, "-/-" = 27)))
    r <- .val_row(res$combinationTable$asDF, "+/-")
    expect_true(is.na(r$lrPos)); expect_true(is.na(r$dor)); expect_true(is.na(r$ppv))
    expect_equal(r$lrNeg, 1)
    ci <- .val_row(res$combinationTableCIRatios$asDF, "+/-_lrNeg")
    expect_true(is.na(ci$lower) && is.na(ci$upper))
    expect_match(res$combinationTableCIRatios$notes$nonestimable_ci$note, "2 row(s) (+/-, -/+)", fixed = TRUE)
})

test_that("C02 Majority rule by hand, and Majority of (t1, t2, copy of t2) equals Test 2 alone", {
    tb2 <- .val_run(.val_mk(.F2$cD, .F2$cN), t3 = "t3", t3p = "pos")$combinationTable$asDF
    # Majority = {+/+/+, +/+/-, +/-/+, -/+/+}: D 12+5+4+3 = 24, N 1+2+0+2 = 5
    m <- .val_row(tb2, MAJ)
    expect_equal(c(m$tp, m$fp, m$fn, m$tn), c(24, 5, 6, 45))
    expect_equal(c(m$lrPos, m$lrNeg, m$dor), c(8, 2/9, 36), tolerance = 1e-12)
    d4 <- .val_mk(.F4$cD, .F4$cN); d4$t3 <- d4$t2
    m4 <- .val_row(.val_run(d4, t3 = "t3", t3p = "pos")$combinationTable$asDF, MAJ)
    expect_equal(c(m4$tp, m4$fp, m4$fn, m4$tn), c(48, 19, 12, 121))
})

test_that("C12 swapping every positive level maps Parallel onto Serial (sens<->spec, PPV<->NPV, LR+ = 1/LR-)", {
    d4 <- .val_mk(.F4$cD, .F4$cN)
    p <- .val_row(.val_run(d4)$combinationTable$asDF, PAR)
    s <- .val_row(.val_run(d4, gp = "N", t1p = "neg", t2p = "neg")$combinationTable$asDF, SER)
    expect_equal(c(s$sens, s$spec, s$ppv, s$npv, s$lrPos, s$lrNeg, s$dor),
                 c(p$spec, p$sens, p$npv, p$ppv, 1 / p$lrNeg, 1 / p$lrPos, p$dor), tolerance = 1e-12)
})

test_that("C18 duplicating every row shrinks the log-scale ratio CI by exactly sqrt(2)", {
    d4 <- .val_mk(.F4$cD, .F4$cN)
    w <- function(res) {
        r <- .val_row(res$combinationTableCIRatios$asDF, paste0(PAR, "_lrPos"))
        log(r$upper) - log(r$lower)
    }
    expect_equal(w(.val_run(d4)) / w(.val_run(rbind(d4, d4))), sqrt(2), tolerance = 1e-10)
})

test_that("C24 individual tests use their own pairwise-complete cases; the combination uses joint cases", {
    d1 <- .val_mk(.F1$cD, .F1$cN)
    extra <- data.frame(gold = factor(c("D", "D", "N"), levels = c("D", "N")),
                        t1 = factor(c("pos", "pos", "neg"), levels = c("pos", "neg")),
                        t2 = factor(c(NA, NA, NA), levels = c("pos", "neg")))
    res <- .val_run(rbind(d1, extra), showIndividual = TRUE)
    # Test 1 alone on 23 cases: tp 10 fp 3 fn 2 tn 8
    expect_equal(res$individualTest1$test1Stats$asDF$estimate, c(10/12, 8/11, 10/13, 8/10), tolerance = 1e-12)
    # the combination table keeps the 20 joint cases, its "Test 1 alone" row included:
    # tp 4+4 = 8 there against 8+2 = 10 in the individual table (the extra rows lack Test 2)
    expect_equal(res$combinationTable$asDF$tp, c(4, 4, 1, 1, 8, 5, 9, 4))
    expect_match(.val_text(res), "uses 20 of 23 cases (87.0%)", fixed = TRUE)
})

test_that("C38 an addNA() level is missing, not negative", {
    d4 <- .val_mk(.F4$cD, .F4$cN); d4$t2[c(3, 50, 150)] <- NA
    d4l <- d4; d4l$t2 <- addNA(d4l$t2)
    expect_identical(.val_run(d4)$combinationTable$asDF, .val_run(d4l)$combinationTable$asDF)
})

test_that("C03 integer-coded jamovi factors, character columns and non-ASCII names give the same table", {
    d4 <- .val_mk(.F4$cD, .F4$cN)
    ref <- .val_run(d4)$combinationTable$asDF
    jf <- function(x) { f <- factor(ifelse(x == levels(x)[1], "1", "0"), levels = c("1", "0")); attr(f, "values") <- c(1L, 0L); f }
    dj <- data.frame(gold = jf(d4$gold), t1 = jf(d4$t1), t2 = jf(d4$t2))
    expect_identical(.val_run(dj, gp = "1", t1p = "1", t2p = "1")$combinationTable$asDF, ref)
    dc <- d4; for (v in names(dc)) dc[[v]] <- as.character(dc[[v]])
    expect_identical(.val_run(dc)$combinationTable$asDF, ref)
    du <- d4; names(du) <- c("Gold standart (biyopsi)", "IHC \u00f6l\u00e7\u00fcm", "Ki-67 \u2265 20%")
    expect_identical(.val_run(du, gold = names(du)[1], t1 = names(du)[2], t2 = names(du)[3])$combinationTable$asDF, ref)
})

test_that("C36 the written-back pattern column lands on the right rows under a filter", {
    d <- .val_mk(.F4$cD, .F4$cN); d$t1[c(25, 33)] <- NA; d$gold[40] <- NA
    sub <- d[-(1:20), ]
    a <- decisioncombineClass$new(
        options = decisioncombineOptions$new(gold = "gold", goldPositive = "D", test1 = "t1", test1Positive = "pos",
                                             test2 = "t2", test2Positive = "pos", addedPattern = TRUE),
        data = sub)
    suppressWarnings(suppressMessages({a$init(); a$run()}))
    op <- a$results$addedPattern$.__enclos_env__$private
    # a pattern needs only the tests: every row with t1 and t2 observed, so row 40 (reference
    # missing) gets its pattern and rows 25 and 33 (t1 missing) stay blank
    keep <- stats::complete.cases(sub[, c("t1", "t2")])
    expect_true(40L %in% op$.rowNums)
    expect_false(any(c(25L, 33L) %in% op$.rowNums))
    expect_identical(op$.rowNums, as.integer(rownames(sub))[keep])
    expect_identical(as.character(op$.values[[1]]),
                     paste0(ifelse(sub$t1[keep] == "pos", "+", "-"), "/", ifelse(sub$t2[keep] == "pos", "+", "-")))
})

test_that("C29/C31/C32 ranking sentence on the three-test table (hand values)", {
    rec <- .val_run(.val_mk(.F2$cD, .F2$cN), t3 = "t3", t3p = "pos", showRecommendation = TRUE)$recommendationTable$asDF
    expect_equal(rec$pattern, MAJ)
    # J = tp/30 - fp/50 on F2 (30 diseased, 50 not); every row is eligible and J > 0 for:
    #   Test 1 alone 23/30 - 7/50 = .627 | Test 2 alone 20/30 - 10/50 = .467 | Test 3 alone 20/30 - 9/50 = .487
    #   Serial 1+2 17/30 - 3/50 = .507 | Parallel 1+2 26/30 - 14/50 = .587 | Serial 1+3 16/30 - 1/50 = .513
    #   Parallel 1+3 27/30 - 15/50 = .600 | Serial 2+3 15/30 - 3/50 = .440 | Parallel 2+3 25/30 - 16/50 = .513
    #   Parallel 27/30 - 20/50 = .500 | Serial 12/30 - 1/50 = .380 | Majority 24/30 - 5/50 = .700
    #   patterns +/+/- 5/30 - 2/50 = .127, +/-/+ 4/30 - 0 = .133, -/+/+ 3/30 - 2/50 = .060
    # (+/+/+ is dropped as Serial's twin; the other four patterns have J < 0): 12 + 3 = 15
    expect_match(rec$rationale, "descriptive ranking of 15 candidate rule(s)", fixed = TRUE)
    z <- stats::qnorm(0.975)
    wl <- function(x, n) (x + z^2 / 2 - z * sqrt(x * (n - x) / n + z^2 / 4)) / (n + z^2)
    lower <- base::format(round(wl(24, 30) + wl(45, 50) - 1, 3), nsmall = 3)
    expect_match(rec$rationale, paste0("conservative 95% lower bound for this rule's Youden's J (", lower,
                                       ", from the lower Wilson limits"), fixed = TRUE)
    # runner-up: the best rule with a 2x2 other than Majority's, Test 1 alone at J = .627
    expect_match(rec$rationale,
                 "falls at or below the point estimate of the best rule with a different 2-by-2 table (0.627)",
                 fixed = TRUE)
    # the positive patterns +/+/+, +/+/-, +/-/+, -/+/+ ARE the Majority rule: (12+5+4+3)/30 -
    # (1+2+0+2)/50 = .700, no gain over the winner, so no "unlisted rule" sentence
    expect_false(grepl("No listed rule reaches", rec$rationale, fixed = TRUE))
    # sensitivity exactly 0.80 is not "above 80%"
    expect_match(rec$rationale, "both above 70%", fixed = TRUE)
})

test_that("C41-C44 guard thresholds fire on the stated side of each boundary", {
    one <- function(tpD, fnD, fpN, tnN) .val_run(.val_mk(c("+" = tpD, "-" = fnD), c("+" = fpN, "-" = tnN)), t2 = NULL, t2p = NULL)
    expect_true("Very small sample: n = 19 complete cases" %in% .val_titles(one(6, 4, 3, 6)))
    expect_true(any(grepl("^Small sample: n = 20", .val_titles(one(6, 4, 4, 6)))))
    expect_true("Only 9 cases in the smaller reference-standard group" %in% .val_titles(one(6, 3, 24, 36)))
    expect_false(any(grepl("smaller reference-standard", .val_titles(one(6, 4, 24, 36)))))
    expect_false("Extreme Disease Prevalence" %in% .val_titles(one(3, 2, 38, 57)))     # 5/100
    expect_true("Extreme Disease Prevalence" %in% .val_titles(one(3, 1, 38, 58)))      # 4/100
    expect_false("Sparse Cell Counts" %in% .val_titles(one(5, 5, 5, 5)))
    expect_true("Sparse Cell Counts" %in% .val_titles(one(4, 6, 5, 5)))
})


# ---- VAL- defects found by the validation, fixed 2026-09-28 -------------------------------

test_that("VAL-decisioncombine-01 (C39) one-class sample: NPV / PPV blank, as the notice says", {
    d4 <- .val_mk(.F4$cD, .F4$cN)
    dD <- d4[d4$gold == "D", ]; dD$gold <- factor(as.character(dD$gold), levels = c("D", "N"))
    dN <- d4[d4$gold == "N", ]; dN$gold <- factor(as.character(dN$gold), levels = c("D", "N"))
    expect_true(all(is.na(.val_run(dD)$combinationTable$asDF$npv)))
    expect_true(all(is.na(.val_run(dN)$combinationTable$asDF$ppv)))
})

test_that("VAL-decisioncombine-02 (C29/C30/C34) distinct rules sharing a 2x2 are counted and named as ties", {
    # D shows +/- or -/+ (6 each), N shows +/+ or -/- (6 each): Parallel, +/- and -/+ all have J = 0.5
    rx <- .val_run(.val_mk(c("+/+" = 0, "+/-" = 6, "-/+" = 6, "-/-" = 0), c("+/+" = 6, "+/-" = 0, "-/+" = 0, "-/-" = 6)),
                   showRecommendation = TRUE)
    rat <- rx$recommendationTable$asDF$rationale
    # each test alone: tp 6, fp 6 of 12 and 12 -> J = 0, so not ranked; Serial = +/+ has J = -.5
    expect_match(rat, "3 rules tie on Youden's J", fixed = TRUE)
    expect_match(rat, "-/+", fixed = TRUE)
    expect_match(rat, "ranking of 3 candidate rule(s)", fixed = TRUE)
    # named rules before patterns, patterns in table order
    expect_match(rat, "(Parallel (>=1 pos), +/-, -/+)", fixed = TRUE)
    # and the reason given for the rule shown is that ordering
    expect_match(rat, paste0("\"Parallel (>=1 pos)\" is displayed only because it comes first (named rules ",
                             "are listed before exact patterns, and rules that use fewer tests come first)."),
                 fixed = TRUE)
    # the two positive-J patterns together are "exactly one test positive": tp 6+6 = 12 of 12,
    # fp 0 of 12 -> J = 1.000, which is 1.000 - 0.500 = 0.500 above the displayed rule
    expect_match(rat, "calling positive exactly the patterns +/-, -/+ would give J = 1.000, 0.500 above the highest-ranked rule",
                 fixed = TRUE)
    # no information: every row has J = 0 exactly (patterns 5/20 + 15/20 - 1, each test alone
    # 10/20 + 10/20 - 1, Parallel 15/20 + 5/20 - 1). 4 patterns + 2 tests alone + Parallel +
    # Serial = 8 rows, less +/+ as Serial's twin = 7 candidates
    rn <- .val_run(.val_mk(c("+/+" = 5, "+/-" = 5, "-/+" = 5, "-/-" = 5), c("+/+" = 5, "+/-" = 5, "-/+" = 5, "-/-" = 5)))
    expect_match(.val_text(rn), "None of the 7 candidate rule(s)", fixed = TRUE)
})

test_that("VAL-decisioncombine-03 (C59) exported R syntax parses for non-syntactic column names", {
    d <- .val_mk(.F4$cD, .F4$cN); names(d) <- c("Golden Standart", "New Test", "Rater 1")
    a <- decisioncombineClass$new(
        options = decisioncombineOptions$new(gold = "Golden Standart", goldPositive = "D", test1 = "New Test",
                                             test1Positive = "pos", test2 = "Rater 1", test2Positive = "pos"),
        data = d)
    suppressWarnings(suppressMessages({a$init(); a$run()}))
    src <- a$asSource()
    expect_silent(parse(text = src))
    data <- d
    # eval() of the analysis's own exported syntax is what is under test
    res <- suppressWarnings(suppressMessages(eval(parse(text = src))))
    expect_identical(res$combinationTable$asDF, a$results$combinationTable$asDF)
})

test_that("VAL-decisioncombine-04 (C56) forest plot draws ratios on a log axis with a null line at 1", {
    d <- .val_mk(.F4$cD, .F4$cN)
    a <- decisioncombineClass$new(
        options = decisioncombineOptions$new(gold = "gold", goldPositive = "D", test1 = "t1", test1Positive = "pos",
                                             test2 = "t2", test2Positive = "pos", showForest = TRUE),
        data = d)
    suppressWarnings(suppressMessages({a$init(); a$run()}))
    built <- a$.__enclos_env__$private$.buildForestPanels(a$results$forestPlot$state)
    expect_named(built$panels, c("proportions", "ratios"))
    geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")
    # ratios: log10 axis and a reference line at exactly 1
    expect_identical(built$panels$ratios$scales$get_scales("x")$get_transformation()$name, "log-10")
    vl <- built$panels$ratios$layers[[which(geoms(built$panels$ratios) == "GeomVline")]]
    expect_equal(vl$data$xintercept, 1)
    # proportions: linear, no reference line
    expect_false("GeomVline" %in% geoms(built$panels$proportions))
    # whiskers are the two CI tables, bound for bound
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    # select the whisker layer by geom: a separator line and the null line come before it
    eb_layer <- function(p) which(geoms(p) == "GeomErrorbar")
    ebp <- ggplot2::layer_data(built$panels$proportions, eb_layer(built$panels$proportions))
    ebr <- ggplot2::layer_data(built$panels$ratios, eb_layer(built$panels$ratios))
    cip <- a$results$combinationTableCI$asDF
    cir <- a$results$combinationTableCIRatios$asDF
    expect_equal(sort(c(ebp$xmin, ebp$xmax)), sort(c(cip$lower, cip$upper)), tolerance = 1e-10)
    # built data on a log10 scale are log10 values: back-transform before comparing
    expect_equal(sort(10^c(ebr$xmin, ebr$xmax)), sort(c(cir$lower, cir$upper)), tolerance = 1e-10)
    # the image height grows with the rows so labels do not overprint (8 statistics x 8 rules)
    expect_gte(a$results$forestPlot$height, 1000)
})

test_that("VAL-decisioncombine-05 (C35) a single inverted test is flagged even with a small reference group", {
    res <- .val_run(.val_mk(c("+" = 2, "-" = 6), c("+" = 9, "-" = 3)), t2 = NULL, t2p = NULL)
    expect_true(any(c("Positive Levels May Be Inverted", "No Rule Performs Better Than Chance") %in% .val_titles(res)))
})


# ---- VAL-04 follow-up: defects found by the adversarial forest-plot probe, fixed 2026-09-28 ----
# Each was reproduced by an independent agent and judged a real defect before it was fixed.

.val_obj <- function(d, ...) {
    base <- list(gold = "gold", goldPositive = "D", test1 = "t1", test1Positive = "pos",
                 test2 = if ("t2" %in% names(d)) "t2" else NULL,
                 test2Positive = if ("t2" %in% names(d)) "pos" else NULL,
                 test3 = if ("t3" %in% names(d)) "t3" else NULL,
                 test3Positive = if ("t3" %in% names(d)) "pos" else NULL, showForest = TRUE)
    opts <- do.call(decisioncombineOptions$new, utils::modifyList(base, list(...), keep.null = TRUE))
    a <- decisioncombineClass$new(options = opts, data = d)
    suppressWarnings(suppressMessages({a$init(); a$run()}))
    a
}
.val_forest <- function(a) a$.__enclos_env__$private$.buildForestPanels(a$results$forestPlot$state)
.val_ticks <- function(p, k) {
    pp <- ggplot2::ggplot_build(p)$layout$panel_params[[k]]$x
    b <- pp$get_breaks()
    pp$get_labels()[!is.na(b)]
}
.F3 <- list(cD = c("+/+" = 15, "+/-" = 0, "-/+" = 0, "-/-" = 5),
            cN = c("+/+" = 3, "+/-" = 0, "-/+" = 0, "-/-" = 27))
.PERFECT <- list(cD = c("+/+" = 20, "+/-" = 0, "-/+" = 0, "-/-" = 0),
                 cN = c("+/+" = 0, "+/-" = 0, "-/+" = 0, "-/-" = 30))

test_that("VAL-04 (C56) log-axis ticks print their true value: no '0', no thousands space, decimal symbol honoured", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    # scales::label_number() rounded 0.001 to "0" (beside a DOR of 0.0004) and printed "1 000"
    b <- .val_forest(.val_obj(.val_mk(.PERFECT$cD, .PERFECT$cN)))
    labs <- unlist(lapply(seq_len(nlevels(b$panels$ratios$data$statistic)),
                          function(k) .val_ticks(b$panels$ratios, k)))
    expect_false(any(labs == "0"))
    expect_false(any(grepl(" ", labs)))
    expect_true(all(c("0.001", "1", "1000") %in% labs))
    # jmvcore sets options(OutDec = decSymbol) around the renderer
    op <- options(OutDec = ","); on.exit(options(op), add = TRUE)
    b4 <- .val_forest(.val_obj(.val_mk(.F4$cD, .F4$cN)))
    expect_true(any(grepl("^0,[0-9]+$", .val_ticks(b4$panels$ratios, 2))))
})

test_that("VAL-04 (C58) a blank or partial forest plot is explained by a notice; normal data raises none", {
    # PPV is blank in the TABLE for both selected rows: one notice covers every plot
    a <- .val_obj(.val_mk(.F3$cD, .F3$cN), filterStatistic = "ppv", filterPattern = "mixed")
    expect_null(.val_forest(a))
    expect_true("Selected Statistic Is Blank" %in% .val_titles(a$results))
    expect_false("Forest Plot Has Nothing to Draw" %in% .val_titles(a$results))
    # LR- has an estimate (exactly 1) but no interval on those rows: only the forest is blank
    al <- .val_obj(.val_mk(.F3$cD, .F3$cN), filterStatistic = "lrNeg", filterPattern = "mixed")
    expect_null(.val_forest(al))
    expect_true("Forest Plot Has Nothing to Draw" %in% .val_titles(al$results))
    expect_false("Selected Statistic Is Blank" %in% .val_titles(al$results))
    a2 <- .val_obj(.val_mk(.F3$cD, .F3$cN), filterStatistic = "all", filterPattern = "mixed")
    expect_true("Statistics Omitted from the Forest Plot" %in% .val_titles(a2$results))
    expect_match(.val_text(a2$results), "PPV, LR+, LR-, DOR: no selected row has both", fixed = TRUE)
    a4 <- .val_obj(.val_mk(.F4$cD, .F4$cN))
    expect_false(any(c("Forest Plot Has Nothing to Draw", "Statistics Omitted from the Forest Plot")
                     %in% .val_titles(a4$results)))
})

test_that("VAL-04 (C56) both panels keep table row order, and a ratio with no interval is not drawn as a bare point", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    b <- .val_forest(.val_obj(.val_mk(.F3$cD, .F3$cN)))
    table_order <- c("+/+", "+/-", "-/+", "-/-", "Test 1 alone", "Test 2 alone",
                     "Parallel (>=1 pos)", "Serial (all pos)")
    for (p in b$panels) {
        y <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
        expect_identical(rev(y), table_order)          # discrete y is drawn bottom-up
    }
    # "+/-" and "-/+" have LR- exactly 1 with a blank interval: not drawn
    expect_true(all(is.finite(b$panels$ratios$data$lower) & is.finite(b$panels$ratios$data$upper)))
    expect_false(any(b$panels$ratios$data$pattern %in% c("+/-", "-/+")))
})

test_that("VAL-04 (C56) every proportion facet has its own labelled 0-100% axis", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    p <- .val_forest(.val_obj(.val_mk(.F4$cD, .F4$cN)))$panels$proportions
    expect_true(isTRUE(p$facet$params$free$x))
    n_panels <- length(ggplot2::ggplot_build(p)$layout$panel_params)
    for (k in seq_len(n_panels)) expect_identical(.val_ticks(p, k), c("0%", "25%", "50%", "75%", "100%"))
})

test_that("VAL-04 (C56) a narrow ratio facet is labelled on both sides of 1, and 1 itself is labelled", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    b <- .val_forest(.val_obj(.val_mk(.F2$cD, .F2$cN), filterPattern = "mixed"))
    lrn <- as.numeric(.val_ticks(b$panels$ratios, which(levels(b$panels$ratios$data$statistic) == "LR-")))
    expect_true(any(lrn > 1) && any(lrn < 1) && any(lrn == 1))
})

test_that("VAL-04 (C56) the exported / redrawn forest plot keeps the live size (set in .init, not .run)", {
    # jamovi export: fresh object, init(), state restored, no .run()
    # height = max(400, 170 + stats * (45 + 22 * rules)); rules = 2^k + k + 2 (+ 7 pairwise and
    # Majority rows when k = 3) = 20 for k = 3, 8 for k = 2; 1 for a single test
    #   k = 3, all 8 statistics: 170 + 8 * (45 + 22 * 20) = 170 + 8 * 485 = 4050
    #   k = 2, all 8 statistics: 170 + 8 * (45 + 22 * 8)  = 170 + 8 * 221 = 1938
    #   k = 3, DOR only:         170 + 1 * (45 + 22 * 20) = 655
    #   single test:             170 + 8 * (45 + 22 * 1)  = 170 + 8 * 67  = 706
    cases <- list(list(.val_mk(.F2$cD, .F2$cN), list(test3 = "t3", test3Positive = "pos"), 4050),
                  list(.val_mk(.F4$cD, .F4$cN), list(), 1938),
                  list(.val_mk(.F2$cD, .F2$cN), list(test3 = "t3", test3Positive = "pos", filterStatistic = "dor"), 655),
                  list(.val_mk(c("+" = 8, "-" = 2), c("+" = 3, "-" = 7)), list(), 706))
    for (cs in cases) {
        d <- cs[[1]]
        a <- do.call(.val_obj, c(list(d), cs[[2]]))
        expect_equal(a$results$forestPlot$height, cs[[3]])
        fresh <- decisioncombineClass$new(options = a$options, data = NULL)
        fresh$.setReadDatasetHeaderSource(function(vars) d[0, intersect(unlist(vars), names(d)), drop = FALSE])
        suppressWarnings(fresh$init())
        fresh$results$forestPlot$setState(a$results$forestPlot$state)
        expect_equal(fresh$results$forestPlot$size$height, cs[[3]])
    }
})

# ---- second review round (adversarial patch review), fixed 2026-09-28 ----------------------

.val_min_gap <- function(p) {
    b <- ggplot2::ggplot_build(p)
    min(vapply(seq_along(b$layout$panel_params), function(k) {
        pp <- b$layout$panel_params[[k]]$x
        br <- sort(pp$get_breaks()[!is.na(pp$get_breaks())])
        if (length(br) < 2) Inf else min(diff(br)) / diff(pp$continuous_range)
    }, numeric(1)))
}

test_that("VAL-04 (C56) log-axis ticks never crowd together, including one-sided facets", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    # a facet whose intervals all lie on one side of 1 reaches past 1 only by the padding;
    # the fallback once put 2-4 labels there, printed as "0.960.981"
    single <- .val_forest(.val_obj(.val_mk(c("+" = 40, "-" = 10), c("+" = 12, "-" = 38))))
    expect_gte(.val_min_gap(single$panels$ratios), 0.07)
    lrp <- .val_ticks(single$panels$ratios, 1)
    expect_true("1" %in% lrp)
    allpos <- .val_forest(.val_obj(.val_mk(.F2$cD, .F2$cN), filterPattern = "allPositive"))
    expect_gte(.val_min_gap(allpos$panels$ratios), 0.07)
})

test_that("VAL-04 (C58) omitted statistics leave blank space below, not stretched facets", {
    d <- .val_mk(.F4$cD, .F4$cN)
    dN <- d[d$gold == "N", ]; dN$gold <- factor(as.character(dN$gold), levels = c("D", "N"))
    b <- .val_forest(.val_obj(dN))
    expect_equal(b$heights, 1)                 # Specificity only (Accuracy is blank one-class)
    expect_equal(b$rows, c(1.6, 7))            # drawn panel + blank row for the 7 omitted facets
    expect_equal(.val_forest(.val_obj(d))$rows, c(5.6, 3.6))
})

test_that("VAL-04 (C58) the forest notices name both causes and list partly empty rules", {
    d <- .val_mk(.F4$cD, .F4$cN)
    dN <- d[d$gold == "N", ]; dN$gold <- factor(as.character(dN$gold), levels = c("D", "N"))
    aN <- .val_obj(dN)
    expect_match(.val_text(aN$results), "or a sample with only one gold-standard outcome), so the forest plot omits", fixed = TRUE)
    a3 <- .val_obj(.val_mk(.F3$cD, .F3$cN))
    expect_true("Rules Left Empty in the Forest Plot" %in% .val_titles(a3$results))
    expect_match(.val_text(a3$results), "PPV: +/-, -/+; LR+: +/-, -/+; LR-: +/-, -/+; DOR: +/-, -/+", fixed = TRUE)
    expect_false("Rules Left Empty in the Forest Plot" %in% .val_titles(.val_obj(d)$results))
})

test_that("VAL-04 (C56) the exported figure separates pattern rows from single tests and strategies, and marks corrected ratios", {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")
    b4 <- .val_forest(.val_obj(.val_mk(.F4$cD, .F4$cN)))
    # the 4 non-pattern rows (Test 1 alone, Test 2 alone, Parallel, Serial) sort to the bottom
    # (y positions 1-4): a line at 4 + 0.5 = 4.5 above them
    hl <- b4$panels$proportions$layers[[which(geoms(b4$panels$proportions) == "GeomHline")]]
    expect_equal(hl$data$yintercept, 4.5)
    expect_match(b4$panels$ratios$labels$caption, "exact result patterns", fixed = TRUE)
    # a pattern filter leaves only pattern rows (no single test, no strategy): no line, no pattern caption
    bm <- .val_forest(.val_obj(.val_mk(.F4$cD, .F4$cN), filterPattern = "mixed"))
    expect_false("GeomHline" %in% geoms(bm$panels$proportions))
    # F2 "+/-/+" (fp = 0) carries the 0.5 correction: an open point in the ratio panel
    b2 <- .val_forest(.val_obj(.val_mk(.F2$cD, .F2$cN)))
    pts <- ggplot2::layer_data(b2$panels$ratios, which(geoms(b2$panels$ratios) == "GeomPoint"))
    ylab <- rev(levels(b2$panels$ratios$data$pattern))
    corrected_rows <- b2$panels$ratios$data$pattern %in% c("+/-/+", "-/+/-")
    expect_true(all(pts$shape[corrected_rows] == 21))
    expect_true(all(pts$shape[!corrected_rows] == 16))
    expect_match(b2$panels$ratios$labels$caption, "continuity correction", fixed = TRUE)
})

# ---- open follow-ups closed 2026-09-28: one-class Accuracy and the sparse-cell notice ----------

test_that("VAL-01 (C39) one-class sample: Accuracy is blank too (it only repeated sens or spec)", {
    d <- .val_mk(.F4$cD, .F4$cN)
    for (g in c("D", "N")) {
        d1 <- d[d$gold == g, ]; d1$gold <- factor(as.character(d1$gold), levels = c("D", "N"))
        res <- .val_run(d1)
        expect_true(all(is.na(res$combinationTable$asDF$acc)))
        ci <- res$combinationTableCI$asDF
        acc_rows <- grepl("_acc\"?$", rownames(ci))
        expect_true(all(is.na(ci$estimate[acc_rows]) & is.na(ci$lower[acc_rows]) & is.na(ci$upper[acc_rows])))
        expect_match(.val_text(res), "Accuracy only repeats the", fixed = TRUE)
    }
    # two-class data keep their accuracy
    expect_false(anyNA(.val_run(d)$combinationTable$asDF$acc))
})

test_that("VAL-01 (C44) the sparse-cell notice ignores a one-class sample's structural zeros", {
    d <- .val_mk(.F4$cD, .F4$cN)
    dN <- d[d$gold == "N", ]; dN$gold <- factor(as.character(dN$gold), levels = c("D", "N"))
    res <- .val_run(dN)
    expect_true("Gold Standard Has Only One Outcome" %in% .val_titles(res))
    expect_false("Sparse Cell Counts" %in% .val_titles(res))
    # sampling zeros in two-class data are still flagged (F2 "+/-/+" has fp = 0)
    expect_true("Sparse Cell Counts" %in% .val_titles(.val_run(.val_mk(.F2$cD, .F2$cN), t3 = "t3", t3p = "pos")))
})

# ---- one-class notice path, closed 2026-09-28 after an adversarial check of the fixes above ----

test_that("VAL-01 (C39) one-class is decided on the dichotomised outcome: a 3-level reference with no positive case is flagged", {
    d <- .val_mk(.F4$cD, .F4$cN)
    g <- as.character(d$gold); g[g == "D"] <- "X"
    d$gold <- factor(g, levels = c("D", "N", "X"))       # "D" declared, never observed
    res <- .val_run(d)
    expect_true("Gold Standard Has Only One Outcome" %in% .val_titles(res))
    expect_match(.val_text(res), 'No complete case used for the combination analysis has gold standard "D"', fixed = TRUE)
    expect_true(all(is.na(res$combinationTable$asDF$sens)))
})

test_that("VAL-01 (C39) the one-class notice names every blank column and the pairwise tables", {
    d <- .val_mk(.F4$cD, .F4$cN)
    dD <- d[d$gold == "D", ]; dD$gold <- factor(as.character(dD$gold), levels = c("D", "N"))
    txt <- .val_text(.val_run(dD))
    expect_match(txt, "Balanced Accuracy and Youden's J, which combine sensitivity with specificity, cannot be formed", fixed = TRUE)
    expect_match(txt, "Individual-test tables use their own pairwise-complete cases", fixed = TRUE)
    expect_match(txt, "no disease-absent cases", fixed = TRUE)
})

test_that("VAL-01 (C39) 'PPV/NPV Interpretation' appears only when some row can show a PPV or NPV", {
    d <- .val_mk(.F4$cD, .F4$cN)
    dN <- d[d$gold == "N", ]; dN$gold <- factor(as.character(dN$gold), levels = c("D", "N"))
    expect_false("PPV/NPV Interpretation" %in% .val_titles(.val_run(dN)))
    expect_true("PPV/NPV Interpretation" %in% .val_titles(.val_run(d)))
})

test_that("VAL-04 (C58) a selected statistic blank in every row gets one notice for all plots", {
    d <- .val_mk(.F4$cD, .F4$cN)
    dN <- d[d$gold == "N", ]; dN$gold <- factor(as.character(dN$gold), levels = c("D", "N"))
    a <- .val_obj(dN, showBarPlot = TRUE, showHeatmap = TRUE, filterStatistic = "acc")
    titles <- .val_titles(a$results)
    expect_true("Selected Statistic Is Blank" %in% titles)
    expect_false("Forest Plot Has Nothing to Draw" %in% titles)     # not said twice
    # a statistic that is present raises no such notice
    expect_false("Selected Statistic Is Blank" %in% .val_titles(.val_obj(dN, showBarPlot = TRUE, filterStatistic = "spec")$results))
})

test_that("VAL-04 (C58) each blank-plot cause gets exactly one notice (no contradicting pair)", {
    blank_titles <- c("No Rows Match the Pattern Filter", "Selected Statistic Is Blank",
                      "Forest Plot Not Available for Selected Statistic", "Forest Plot Has Nothing to Draw")
    d <- .val_mk(.F4$cD, .F4$cN)
    dD <- d[d$gold == "D", ]; dD$gold <- factor(as.character(dD$gold), levels = c("D", "N"))
    # one-class, Youden's J (no CI anyway, and blank in every row): the blank-statistic notice only
    t1 <- .val_titles(.val_obj(dD, filterStatistic = "youden")$results)
    expect_equal(sum(blank_titles %in% t1), 1L)
    expect_true("Selected Statistic Is Blank" %in% t1)
    # single test + a pattern filter: 'No Rows Match' only, whatever the statistic
    t2 <- .val_titles(.val_obj(.val_mk(c("+" = 40, "-" = 10), c("+" = 12, "-" = 38)),
                               filterStatistic = "prevalence", filterPattern = "mixed")$results)
    expect_equal(sum(blank_titles %in% t2), 1L)
    expect_true("No Rows Match the Pattern Filter" %in% t2)
    # two-class, Youden's J: the forest-only note, which is then true
    t3 <- .val_titles(.val_obj(d, filterStatistic = "youden")$results)
    expect_equal(sum(blank_titles %in% t3), 1L)
    expect_true("Forest Plot Not Available for Selected Statistic" %in% t3)
})
