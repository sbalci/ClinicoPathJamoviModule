# Regression tests for the decisioncombine independent review (2026-09-29).
#
# Purpose: pin the fixes for findings IR-01 to IR-07 of
#   development-ideas/decisioncombine-independent-review-2026-09-29.md
# and, as IR-R2-1 to IR-R2-8, the second review round of the same day: fixtures built so that
# a plausible regression of each fix (which.max() for the winner, a second inversion notice,
# an ungraded chance note, the exact rather than the rounded optimum, a stale Turkish
# catalog) fails a test.
# One test_that() per claim; each description starts with its finding ID.
#
# Every expected value is either hand arithmetic from the fixture design (written out in the
# comments) or an independent implementation: stats::prop.test (Wilson score interval), the
# Agresti-Caffo interval in closed form (.ir_ac below -- the analysis itself calls
# DescTools::BinomDiffCI, so the oracle must not), DescTools::BinomRatioCI (Katz log) and
# DescTools::OddsRatio (Woolf). None is copied from the output of the code under test.
#
# The analysis is called by its unqualified names (decisioncombine, decisioncombineClass,
# decisioncombineOptions), so the same file runs against any package that provides them,
# e.g. after devtools::load_all("../meddecide").

# ---- local helpers ---------------------------------------------------------------------

# Patient-level data from per-pattern counts. cD / cN: counts per pattern ("+/-", ...) among
# the disease-present (D) and disease-absent (N) patients. Rows are D first, then N, each in
# the order the counts are given. Tests are factors t1..tk with levels c(pos, neg).
.ir_mk <- function(cD, cN, pos = "pos", neg = "neg") {
    pats <- c(names(cD), names(cN))
    counts <- c(cD, cN)
    x <- rep(pats, counts)
    d <- data.frame(gold = factor(rep(rep(c("D", "N"), c(length(cD), length(cN))), counts),
                                  levels = c("D", "N")))
    for (j in seq_len(lengths(strsplit(pats[1], "/", fixed = TRUE)))) {
        sign <- substr(x, 2 * j - 1, 2 * j - 1)
        d[[paste0("t", j)]] <- factor(ifelse(sign == "+", pos, neg), levels = c(pos, neg))
    }
    d
}

# The wrapper, with every Level argument passed explicitly (NULL when unused) and variable
# names as strings; do.call() keeps the strings from being read as symbols.
.ir_run <- function(d, t1 = "t1", t2 = "t2", t3 = NULL, t1p = "pos",
                    t2p = if (is.null(t2)) NULL else "pos",
                    t3p = if (is.null(t3)) NULL else "pos", ...) {
    args <- c(list(data = d, gold = "gold", goldPositive = "D",
                   test1 = t1, test1Positive = t1p,
                   test2 = t2, test2Positive = t2p,
                   test3 = t3, test3Positive = t3p), list(...))
    suppressMessages(do.call(decisioncombine, args))
}

# The analysis object itself, for what the wrapper does not return: .init() on its own and the
# private plot builders. run = FALSE stops after .init().
.ir_object <- function(d, t2 = "t2", t3 = NULL, run = TRUE, ...) {
    options <- decisioncombineOptions$new(
        gold = "gold", goldPositive = "D", test1 = "t1", test1Positive = "pos",
        test2 = t2, test2Positive = if (is.null(t2)) NULL else "pos",
        test3 = t3, test3Positive = if (is.null(t3)) NULL else "pos", ...)
    a <- decisioncombineClass$new(options = options, data = d)
    suppressMessages({
        a$init()
        if (run) a$run()
    })
    a
}

.ir_keys <- function(df) gsub('^"|"$', "", rownames(df))

.ir_row <- function(df, key) {
    r <- df[.ir_keys(df) == key, , drop = FALSE]
    if (nrow(r) != 1L) stop("row key '", key, "' matched ", nrow(r), " rows")
    r
}

.ir_cells <- function(row) unname(unlist(row[, c("tp", "fp", "fn", "tn")]))

.ir_unescape <- function(x) {
    gsub("&amp;", "&", gsub("&gt;", ">", gsub("&lt;", "<", x, fixed = TRUE), fixed = TRUE),
         fixed = TRUE)
}

# Notices as rendered: one row per notice, with the severity word shown to the user
# ("Serious warning", "Warning", "Note"), the title and the body text.
.ir_notices <- function(res) {
    h <- res$notices$content
    blocks <- if (is.null(h)) character() else
        regmatches(h, gregexpr("<strong>[^<]*</strong><br><span[^>]*>[^<]*</span>", h))[[1]]
    heads <- .ir_unescape(sub("^<strong>([^<]*)</strong>.*$", "\\1", blocks))
    data.frame(
        severity = sub("^[^ ]* ", "", sub(": .*$", "", heads)),  # drop the icon
        title = sub("^[^:]*: ", "", heads),
        content = .ir_unescape(sub("^.*<span[^>]*>([^<]*)</span>$", "\\1", blocks)),
        stringsAsFactors = FALSE)
}

.ir_notice <- function(res, title) {
    nt <- .ir_notices(res)
    nt$content[nt$title == title]
}

# Three decimals, the display rule for Youden's J and its bounds.
.ir_f3 <- function(x) base::format(round(x, 3), nsmall = 3)

# Agresti-Caffo 95% interval for p1 - p2 in closed form: add one success and one failure to each
# group, then Wald. Same layout as DescTools::BinomDiffCI (row 1: est, lwr.ci, upr.ci), whose
# "ac" method it reproduces exactly (e.g. 2/8 vs 9/12: -0.7840 to -0.0446).
.ir_ac <- function(x1, n1, x2, n2, conf.level = 0.95, method = "ac") {
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    p1 <- (x1 + 1) / (n1 + 2)
    p2 <- (x2 + 1) / (n2 + 2)
    half <- z * sqrt(p1 * (1 - p1) / (n1 + 2) + p2 * (1 - p2) / (n2 + 2))
    matrix(c(x1 / n1 - x2 / n2, p1 - p2 - half, p1 - p2 + half), nrow = 1,
           dimnames = list(NULL, c("est", "lwr.ci", "upr.ci")))
}

# ---- fixtures --------------------------------------------------------------------------

# K2: 40 diseased, 60 non-diseased.
.IR_P2 <- c("+/+", "+/-", "-/+", "-/-")
.IR_K2 <- list(cD = setNames(c(20, 8, 6, 6), .IR_P2),
               cN = setNames(c(3, 7, 10, 40), .IR_P2))
.IR_K2_RULES <- c(.IR_P2, "Test 1 alone", "Test 2 alone", "Parallel (>=1 pos)",
                  "Serial (all pos)")
# The patterns (indices into .IR_P2) each rule calls positive.
.IR_K2_SETS <- list(1, 2, 3, 4, c(1, 2), c(1, 3), 1:3, 1)

# K3: 60 diseased, 90 non-diseased.
.IR_P3 <- c("+/+/+", "+/+/-", "+/-/+", "+/-/-", "-/+/+", "-/+/-", "-/-/+", "-/-/-")
.IR_K3 <- list(cD = setNames(c(20, 8, 6, 5, 7, 4, 3, 7), .IR_P3),
               cN = setNames(c(2, 3, 2, 8, 1, 9, 10, 55), .IR_P3))
.IR_K3_RULES <- c(.IR_P3, "Test 1 alone", "Test 2 alone", "Test 3 alone",
                  "Serial 1+2 (both pos)", "Parallel 1+2 (>=1 pos)",
                  "Serial 1+3 (both pos)", "Parallel 1+3 (>=1 pos)",
                  "Serial 2+3 (both pos)", "Parallel 2+3 (>=1 pos)",
                  "Parallel (>=1 pos)", "Serial (all pos)", "Majority (>=2/3 pos)")
# Pattern index: 1 +/+/+, 2 +/+/-, 3 +/-/+, 4 +/-/-, 5 -/+/+, 6 -/+/-, 7 -/-/+, 8 -/-/-.
.IR_K3_SETS <- c(as.list(1:8), list(
    1:4, c(1, 2, 5, 6), c(1, 3, 5, 7),  # Test k alone: test k positive
    1:2, 1:6,                           # 1+2: both positive; not (-/-/+ or -/-/-)
    c(1, 3), c(1:5, 7),                 # 1+3: both positive; not (-/+/- or -/-/-)
    c(1, 5), c(1:3, 5:7),               # 2+3: both positive; not (+/-/- or -/-/-)
    1:7, 1, c(1, 2, 3, 5)))             # any positive; all positive; at least two positive

# ---- IR-01: single tests, pairwise rules and the ranking --------------------------------

test_that("IR-01a a strong and a weak test: each test alone is a row, and Test 1 alone wins", {
    # Test 1: sens = spec = .90; Test 2: .60/.60; 100 diseased, 100 non-diseased.
    cD <- c("+/+" = 54, "+/-" = 36, "-/+" = 6, "-/-" = 4)
    cN <- c("+/+" = 4, "+/-" = 6, "-/+" = 36, "-/-" = 54)
    res <- .ir_run(.ir_mk(cD, cN), showRecommendation = TRUE)
    tb <- res$combinationTable$asDF

    one <- .ir_row(tb, "Test 1 alone")
    two <- .ir_row(tb, "Test 2 alone")
    # Test 1 positive = "+/+" or "+/-": tp = 54 + 36 = 90, fp = 4 + 6 = 10
    expect_equal(.ir_cells(one), c(90, 10, 10, 90))
    # Test 2 positive = "+/+" or "-/+": tp = 54 + 6 = 60, fp = 4 + 36 = 40
    expect_equal(.ir_cells(two), c(60, 40, 40, 60))
    expect_identical(c(one$rowType, two$rowType), c("Single test", "Single test"))
    expect_equal(c(one$youden, two$youden), c(0.9 + 0.9 - 1, 0.6 + 0.6 - 1), tolerance = 1e-12)

    rec <- res$recommendationTable$asDF
    expect_identical(rec$pattern, "Test 1 alone")
    expect_equal(c(rec$youden, rec$sens, rec$spec), c(0.8, 0.9, 0.9), tolerance = 1e-12)
    expect_match(res$recommendationTable$notes$scope$note,
                 "ranking of exact-pattern rules, each test alone and named testing strategies",
                 fixed = TRUE)
})

test_that("IR-01b two tests: 8 rows in order on the same cases, and Test 1 alone wins", {
    res <- .ir_run(.ir_mk(.IR_K2$cD, .IR_K2$cN), showRecommendation = TRUE)
    tb <- res$combinationTable$asDF
    expect_identical(.ir_keys(tb), .IR_K2_RULES)
    expect_identical(tb$pattern, .IR_K2_RULES)
    expect_identical(tb$rowType, rep(c("Pattern", "Single test", "Strategy"), c(4, 2, 2)))

    # tp = sum of the rule's diseased pattern counts, fp = its non-diseased counts.
    tp <- vapply(.IR_K2_SETS, function(s) sum(.IR_K2$cD[s]), 0)
    fp <- vapply(.IR_K2_SETS, function(s) sum(.IR_K2$cN[s]), 0)
    expect_equal(tp, c(20, 8, 6, 6, 28, 26, 34, 20))  # the arithmetic, spelled out
    expect_equal(fp, c(3, 7, 10, 40, 10, 13, 20, 3))
    expect_equal(tb$tp, tp)
    expect_equal(tb$fp, fp)
    expect_equal(tb$fn, 40 - tp)
    expect_equal(tb$tn, 60 - fp)

    expect_identical(.ir_keys(res$combinationTableCI$asDF),
                     paste0(rep(.IR_K2_RULES, each = 5), c("_sens", "_spec", "_ppv", "_npv", "_acc")))
    expect_identical(.ir_keys(res$combinationTableCIRatios$asDF),
                     paste0(rep(.IR_K2_RULES, each = 3), c("_lrPos", "_lrNeg", "_dor")))

    rec <- res$recommendationTable$asDF
    expect_identical(rec$pattern, "Test 1 alone")
    expect_equal(rec$youden, 28 / 40 + 50 / 60 - 1, tolerance = 1e-12)
    # Candidates with J > 0: Test 1 alone .533, Test 2 alone .433, Parallel .517, Serial .450,
    # "+/-" .083 ("+/+" merges into its twin Serial; "-/+" -.017 and "-/-" -.517 are out): 5.
    expect_match(rec$rationale, "This is a descriptive ranking of 5 candidate rule(s)", fixed = TRUE)
    # Runner-up with a different 2x2: Parallel, J = 34/40 + 40/60 - 1 = 0.517. The winner's
    # conservative bound is the sum of the lower Wilson limits minus 1.
    runner_up <- 34 / 40 + 40 / 60 - 1
    lower <- stats::prop.test(28, 40, correct = FALSE)$conf.int[1] +
        stats::prop.test(50, 60, correct = FALSE)$conf.int[1] - 1
    expect_lte(lower, runner_up)
    expect_match(rec$rationale, paste0(
        "Its advantage is not established: a conservative 95% lower bound for this rule's ",
        "Youden's J (", .ir_f3(lower), ", from the lower Wilson limits of its sensitivity and ",
        "specificity) falls at or below the point estimate of the best rule with a different ",
        "2-by-2 table (", .ir_f3(runner_up), ")"), fixed = TRUE)
    # The positive patterns "+/+" (.450) and "+/-" (.083) make up Test 1 alone: nothing higher.
    expect_false(grepl("No listed rule reaches", rec$rationale, fixed = TRUE))
})

test_that("IR-01c three tests: 20 rows in order, pairwise rules, Majority is the J-optimum", {
    cD <- .IR_K3$cD
    cN <- .IR_K3$cN
    res <- .ir_run(.ir_mk(cD, cN), t3 = "t3", showRecommendation = TRUE)
    tb <- res$combinationTable$asDF
    expect_identical(.ir_keys(tb), .IR_K3_RULES)
    expect_identical(tb$pattern, .IR_K3_RULES)
    expect_identical(tb$rowType, rep(c("Pattern", "Single test", "Strategy"), c(8, 3, 9)))

    tp <- vapply(.IR_K3_SETS, function(s) sum(cD[s]), 0)
    fp <- vapply(.IR_K3_SETS, function(s) sum(cN[s]), 0)
    expect_equal(tb$tp, tp)
    expect_equal(tb$fp, fp)
    expect_equal(tb$fn, 60 - tp)
    expect_equal(tb$tn, 90 - fp)
    # T1 AND T2 = "+/+/+" + "+/+/-": D 20 + 8, N 2 + 3
    expect_equal(.ir_cells(.ir_row(tb, "Serial 1+2 (both pos)")), c(28, 5, 32, 85))
    # T1 OR T2 = all but "-/-/+" and "-/-/-": D 60 - 3 - 7, N 90 - 10 - 55
    expect_equal(.ir_cells(.ir_row(tb, "Parallel 1+2 (>=1 pos)")), c(50, 25, 10, 65))

    expect_identical(.ir_keys(res$combinationTableCI$asDF),
                     paste0(rep(.IR_K3_RULES, each = 5), c("_sens", "_spec", "_ppv", "_npv", "_acc")))
    expect_identical(.ir_keys(res$combinationTableCIRatios$asDF),
                     paste0(rep(.IR_K3_RULES, each = 3), c("_lrPos", "_lrNeg", "_dor")))

    # The new rows use the same interval methods: Wilson (prop.test), Katz log (BinomRatioCI),
    # Woolf (OddsRatio).
    ci <- res$combinationTableCI$asDF
    for (st in list(c("_sens", 28, 60), c("_spec", 85, 90))) {
        got <- .ir_row(ci, paste0("Serial 1+2 (both pos)", st[1]))
        expect_equal(c(got$lower, got$upper),
                     stats::prop.test(as.numeric(st[2]), as.numeric(st[3]), correct = FALSE)$conf.int[1:2],
                     tolerance = 1e-9, label = st[1])
    }
    ratios <- res$combinationTableCIRatios$asDF
    lr_pos <- DescTools::BinomRatioCI(50, 60, 25, 90, method = "katz.log")
    lr_neg <- DescTools::BinomRatioCI(10, 60, 65, 90, method = "katz.log")
    dor <- DescTools::OddsRatio(matrix(c(50, 25, 10, 65), 2, byrow = TRUE),
                                conf.level = 0.95, method = "wald")
    for (chk in list(list("_lrPos", lr_pos), list("_lrNeg", lr_neg), list("_dor", dor))) {
        got <- .ir_row(ratios, paste0("Parallel 1+2 (>=1 pos)", chk[[1]]))
        expect_equal(c(got$estimate, got$lower, got$upper), unname(as.numeric(chk[[2]])[1:3]),
                     tolerance = 1e-9, label = chk[[1]])
    }

    rec <- res$recommendationTable$asDF
    # Pattern J = D/60 - N/90 is positive for exactly +/+/+, +/+/-, +/-/+ and -/+/+, which is
    # the Majority rule, so Majority is the J-optimum over every rule built from these tests.
    expect_identical(names(which(cD / 60 - cN / 90 > 0)), .IR_P3[c(1, 2, 3, 5)])
    expect_identical(rec$pattern, "Majority (>=2/3 pos)")
    # Majority: D 20 + 8 + 6 + 7 = 41, N 2 + 3 + 2 + 1 = 8
    expect_equal(rec$youden, 41 / 60 + 82 / 90 - 1, tolerance = 1e-12)
    expect_false(grepl("No listed rule reaches", rec$rationale, fixed = TRUE))
})

test_that("IR-01d a mixed rule that no row lists is named when it beats the best listed rule", {
    # 100 diseased and 100 non-diseased, so pattern J = (D - N) / 100:
    #   +/+/+ .30, +/+/- .10, +/-/+ .15, +/-/- -.20, -/+/+ -.08, -/+/- -.02, -/-/+ -.05, -/-/- -.20
    # The optimum is Test 1 AND (Test 2 OR Test 3) = +/+/+, +/+/-, +/-/+: J = .55.
    cD <- setNames(c(32, 14, 18, 5, 4, 5, 7, 15), .IR_P3)
    cN <- setNames(c(2, 4, 3, 25, 12, 7, 12, 35), .IR_P3)
    pattern_j <- (cD - cN) / 100
    listed_j <- vapply(.IR_K3_SETS, function(s) sum(pattern_j[s]), 0)
    # By hand: Majority .47 (= .55 - .08) is the best listed rule, Serial 1+3 .45 next.
    expect_equal(max(listed_j), 0.47, tolerance = 1e-12)
    expect_identical(.IR_K3_RULES[which.max(listed_j)], "Majority (>=2/3 pos)")

    res <- .ir_run(.ir_mk(cD, cN), t3 = "t3", showRecommendation = TRUE)
    rec <- res$recommendationTable$asDF
    expect_identical(rec$pattern, "Majority (>=2/3 pos)")
    # Majority: D 32 + 14 + 18 + 4 = 68, N 2 + 4 + 3 + 12 = 21
    expect_equal(rec$youden, 68 / 100 + 79 / 100 - 1, tolerance = 1e-12)

    optimum <- 0.30 + 0.10 + 0.15
    expect_match(rec$rationale, paste0(
        "No listed rule reaches the highest Youden's J these results allow: calling positive ",
        "exactly the patterns +/+/+, +/+/-, +/-/+ would give J = ", .ir_f3(optimum), ", ",
        .ir_f3(round(optimum, 3) - round(0.47, 3)), " above the highest-ranked rule."), fixed = TRUE)
    expect_identical(.ir_f3(optimum), "0.550")
    expect_identical(.ir_f3(round(optimum, 3) - round(0.47, 3)), "0.080")
    # The patterns named are exactly those with J > 0.
    expect_identical(names(pattern_j)[pattern_j > 0], c("+/+/+", "+/+/-", "+/-/+"))
})

test_that("IR-01e a tie goes to the rule using fewer tests, and an identical 2x2 is no runner-up", {
    # Test 2 is positive only where Test 1 is ("-/+" is empty), so Parallel = Test 1 alone.
    cD <- c("+/+" = 40, "+/-" = 50, "-/+" = 0, "-/-" = 10)  # 100 diseased
    cN <- c("+/+" = 2, "+/-" = 3, "-/+" = 0, "-/-" = 95)    # 100 non-diseased
    res <- .ir_run(.ir_mk(cD, cN), showRecommendation = TRUE)
    tb <- res$combinationTable$asDF
    # Test 1 alone and Parallel: tp 40 + 50, fp 2 + 3
    expect_equal(.ir_cells(.ir_row(tb, "Test 1 alone")), c(90, 5, 10, 95))
    expect_equal(.ir_cells(.ir_row(tb, "Parallel (>=1 pos)")), c(90, 5, 10, 95))

    rec <- res$recommendationTable$asDF
    expect_identical(rec$pattern, "Test 1 alone")
    expect_equal(rec$youden, 0.90 + 0.95 - 1, tolerance = 1e-12)
    expect_match(rec$rationale, paste0(
        "2 rules tie on Youden's J (Test 1 alone, Parallel (>=1 pos)); \"Test 1 alone\" is ",
        "displayed only because it comes first (named rules are listed before exact patterns, ",
        "and rules that use fewer tests come first)."),
        fixed = TRUE)
    # J > 0: Test 1 alone .85, Test 2 alone .38, Parallel .85, Serial .38, "+/-" .47 -> 5
    # ("+/+" merges into Serial; "-/+" is exactly 0; "-/-" is -.85).
    expect_match(rec$rationale, "This is a descriptive ranking of 5 candidate rule(s)", fixed = TRUE)
    # Best rule with a DIFFERENT 2x2: "+/-", J = 50/100 + 97/100 - 1 = .47. The winner's
    # conservative bound is above it, so its advantage is not called unestablished.
    lower <- stats::prop.test(90, 100, correct = FALSE)$conf.int[1] +
        stats::prop.test(95, 100, correct = FALSE)$conf.int[1] - 1
    expect_gt(lower, 50 / 100 + 97 / 100 - 1)
    expect_false(grepl("Its advantage is not established", rec$rationale, fixed = TRUE))
})

test_that("IR-01f the forest plot is sized in .init() for every rule the table can hold", {
    d <- .ir_mk(.IR_K3$cD, .IR_K3$cN)
    size <- function(t2 = "t2", t3 = "t3", ...) {
        # .init() only: export, resize and .omv reopen never call .run().
        img <- .ir_object(d, t2 = t2, t3 = t3, run = FALSE, ...)$results$forestPlot
        c(img$width, img$height)
    }
    # height = max(400, 170 + n_stats * (45 + 22 * n_rules)); 8 statistics in the default set.
    # One test, 1 rule:                          170 + 8 * (45 + 22 * 1)  =  706
    expect_equal(size(t2 = NULL, t3 = NULL), c(800, 706))
    # Two tests, 4 + 2 + 2 = 8 rules:            170 + 8 * (45 + 22 * 8)  = 1938
    expect_equal(size(t3 = NULL), c(800, 1938))
    # Three tests, 8 + 3 + 6 + 3 = 20 rules:     170 + 8 * (45 + 22 * 20) = 4050
    expect_equal(size(), c(800, 4050))
    # Mixed patterns only, 2^3 - 2 = 6 rules:    170 + 8 * (45 + 22 * 6)  = 1586
    expect_equal(size(filterPattern = "mixed"), c(800, 1586))
    # One statistic, one rule: 170 + 45 + 22 = 237, below the 400 floor
    expect_equal(size(filterPattern = "allPositive", filterStatistic = "sens"), c(800, 400))
})

test_that("IR-01g a single-test analysis has one 'Test 1' row and no row-kind footnote", {
    res <- .ir_run(.ir_mk(c("+" = 30, "-" = 10), c("+" = 8, "-" = 32)), t2 = NULL)
    tb <- res$combinationTable$asDF
    expect_identical(tb$pattern, "Test 1")
    expect_identical(tb$rowType, "Single test")
    expect_equal(.ir_cells(tb), c(30, 8, 10, 32))
    expect_false("row_kinds" %in% names(res$combinationTable$notes))
    # Control: with pattern rows the footnote is there.
    two <- .ir_run(.ir_mk(.IR_K2$cD, .IR_K2$cN))
    expect_true("row_kinds" %in% names(two$combinationTable$notes))
})

test_that("IR-01h the About panel says what is scored instead of 'every way'", {
    res <- .ir_run(.ir_mk(.IR_K2$cD, .IR_K2$cN), showAbout = TRUE)
    about <- .ir_unescape(res$about$content)
    expect_match(about, paste0(
        "</h3><p>This analysis scores two or three diagnostic tests against the reference ",
        "standard on the same patients: every exact result pattern, each test alone, and the ",
        "usual ways of combining them"), fixed = TRUE)
    expect_false(grepl("every way", about, fixed = TRUE))
})

test_that("IR-01i the forest plot draws the rules in table order, with the line below the patterns", {
    grDevices::pdf(NULL)
    withr::defer(grDevices::dev.off())
    a <- .ir_object(.ir_mk(.IR_K3$cD, .IR_K3$cN), t3 = "t3", showForest = TRUE)
    built <- a$.__enclos_env__$private$.buildForestPanels(a$results$forestPlot$state)
    expect_identical(names(built$panels), c("proportions", "ratios"))
    for (p in built$panels) {
        # A discrete y axis runs bottom to top, so the first rule is the last limit.
        expect_identical(ggplot2::layer_scales(p)$y$get_limits(), rev(.IR_K3_RULES))
        hline <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
        expect_length(hline, 1L)
        # 3 single tests + 6 pairwise rules + 3 strategies below the 8 patterns: 12 + 0.5
        expect_equal(unique(ggplot2::layer_data(p, hline)$yintercept), 12.5)
    }
})

test_that("IR-01j bar chart and heatmap keep table order; the decision space labels each point once", {
    grDevices::pdf(NULL)
    withr::defer(grDevices::dev.off())
    a <- .ir_object(.ir_mk(.IR_K2$cD, .IR_K2$cN), showBarPlot = TRUE, showHeatmap = TRUE,
                    showDecisionTree = TRUE)
    private <- a$.__enclos_env__$private
    theme <- ggplot2::theme_grey()

    expect_true(private$.plotBarChart(a$results$barPlot, ggtheme = theme))
    expect_identical(ggplot2::layer_scales(ggplot2::last_plot())$x$get_limits(), .IR_K2_RULES)
    expect_true(private$.plotHeatmap(a$results$heatmapPlot, ggtheme = theme))
    expect_identical(ggplot2::layer_scales(ggplot2::last_plot())$y$get_limits(), rev(.IR_K2_RULES))

    expect_true(private$.plotDecisionTree(a$results$decisionTreePlot, ggtheme = theme))
    p <- ggplot2::last_plot()
    # Serial (all pos) and "+/+" share sens 20/40, spec 57/60; every other rule has its own point.
    expect_identical(ggplot2::layer_data(p, 2)$label,
                     c("+/+ = Serial (all pos)", "+/-", "-/+", "-/-", "Test 1 alone",
                       "Test 2 alone", "Parallel (>=1 pos)"))
    expect_identical(p$labels$colour, "Row type")
    expect_identical(ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$get_limits(),
                     c("Pattern", "Single test", "Strategy"))
})

test_that("IR-01k the 'alone' rows use joint complete cases; individual-test tables stay pairwise", {
    d <- .ir_mk(.IR_K2$cD, .IR_K2$cN)
    # Rows: D 1-40 (+/+ 1-20, +/- 21-28, -/+ 29-34, -/- 35-40);
    #       N 41-100 (+/+ 41-43, +/- 44-50, -/+ 51-60, -/- 61-100).
    d$gold[c(30, 45)] <- NA
    d$t1[50] <- NA
    d$t2[70] <- NA
    res <- .ir_run(d, showIndividual = TRUE)
    # Joint complete cases drop rows 30, 45, 50 and 70. Test 1 alone:
    #   D (row 30 is "-/+"): tp = 20 + 8 = 28, fn = 39 - 28 = 11
    #   N (rows 45 and 50 are "+/-", 70 is "-/-"): fp = 3 + 7 - 2 = 8, tn = 57 - 8 = 49
    expect_equal(.ir_cells(.ir_row(res$combinationTable$asDF, "Test 1 alone")), c(28, 8, 11, 49))
    # Individual Test 1 needs only the reference and Test 1, so it keeps row 70: tn = 50.
    ct <- res$individualTest1$test1Contingency$asDF
    expect_equal(c(.ir_row(ct, "Positive")$goldPos, .ir_row(ct, "Positive")$goldNeg,
                   .ir_row(ct, "Negative")$goldPos, .ir_row(ct, "Negative")$goldNeg),
                 c(28, 8, 11, 50))
})

# ---- IR-02: the inversion guard ---------------------------------------------------------

test_that("IR-02a a reversed positive level on the weaker test is named, with its Agresti-Caffo interval", {
    d2 <- .ir_mk(.IR_K2$cD, .IR_K2$cN)
    nt <- .ir_notices(.ir_run(d2, t2p = "neg"))
    inv <- nt[nt$title == "Positive Levels May Be Inverted", ]
    expect_equal(nrow(inv), 1L)
    expect_identical(inv$severity, "Serious warning")
    # Test 2 with "neg" as positive: tp = D(+/-) + D(-/-) = 8 + 6 = 14 of 40,
    # fp = N(+/-) + N(-/-) = 7 + 40 = 47 of 60; J = 14/40 + 13/60 - 1 = -0.433.
    ci <- .ir_ac(14, 40, 47, 60, conf.level = 0.95, method = "ac")
    expect_lt(ci[1, "upr.ci"], 0)
    expect_true(startsWith(inv$content, paste0(
        "Test 2 classifies worse than chance in this sample, beyond sampling variation: ",
        "Youden's J = -0.433 (Agresti-Caffo 95% CI ", .ir_f3(ci[1, "lwr.ci"]), " to ",
        .ir_f3(ci[1, "upr.ci"]), ") on the 100 complete cases analysed here.")))
    expect_identical(.ir_f3(14 / 40 + 13 / 60 - 1), "-0.433")
    expect_match(inv$content, "Every combination that uses such a test is affected as well.",
                 fixed = TRUE)
    expect_false(grepl("Test 2 alone", inv$content, fixed = TRUE))
    expect_false(any(c("Test Performs at Chance Level", "No Rule Performs Better Than Chance") %in%
                         nt$title))

    # Three tests, Test 3 reversed: tp = D(+/+/-, +/-/-, -/+/-, -/-/-) = 8 + 5 + 4 + 7 = 24 of 60,
    # fp = N of the same = 3 + 8 + 9 + 55 = 75 of 90; J = 24/60 + 15/90 - 1 = -0.433.
    d3 <- .ir_mk(.IR_K3$cD, .IR_K3$cN)
    inv3 <- .ir_notice(.ir_run(d3, t3 = "t3", t3p = "neg"), "Positive Levels May Be Inverted")
    ci3 <- .ir_ac(24, 60, 75, 90, conf.level = 0.95, method = "ac")
    expect_length(inv3, 1L)
    expect_true(startsWith(inv3, paste0(
        "Test 3 classifies worse than chance in this sample, beyond sampling variation: ",
        "Youden's J = -0.433 (Agresti-Caffo 95% CI ", .ir_f3(ci3[1, "lwr.ci"]), " to ",
        .ir_f3(ci3[1, "upr.ci"]), ") on the 150 complete cases analysed here.")))

    # Controls: the correctly coded tables raise no inversion warning.
    expect_false("Positive Levels May Be Inverted" %in% .ir_notices(.ir_run(d2))$title)
    expect_false("Positive Levels May Be Inverted" %in% .ir_notices(.ir_run(d3, t3 = "t3"))$title)
})

test_that("IR-02b a correctly coded useless test gets a chance-level note, not an inversion warning", {
    cD <- c("+/+" = 45, "+/-" = 45, "-/+" = 5, "-/-" = 5)   # 100 diseased
    cN <- c("+/+" = 5, "+/-" = 5, "-/+" = 46, "-/-" = 44)   # 100 non-diseased
    d <- .ir_mk(cD, cN)
    nt <- .ir_notices(.ir_run(d))
    expect_false("Positive Levels May Be Inverted" %in% nt$title)
    chance <- nt[nt$title == "Test Performs at Chance Level", ]
    expect_equal(nrow(chance), 1L)
    expect_identical(chance$severity, "Note")
    # Test 2: tp = 45 + 5 = 50 of 100, fp = 5 + 46 = 51 of 100; J = .50 + .49 - 1 = -0.010.
    ci <- .ir_ac(50, 100, 51, 100, conf.level = 0.95, method = "ac")
    expect_gte(ci[1, "upr.ci"], 0)
    expect_identical(.ir_f3(50 / 100 + 49 / 100 - 1), "-0.010")
    expect_true(startsWith(chance$content, paste0(
        "Youden's J is below 0 but within sampling variation of 0 for Test 2 (J = -0.010, 95% CI ",
        .ir_f3(ci[1, "lwr.ci"]), " to ", .ir_f3(ci[1, "upr.ci"]),
        "), on the 200 complete cases analysed here.")))

    # The same test analysed alone (>= 10 cases in both groups) gets the same quiet note. It
    # used to get the serious "No Rule Performs Better Than Chance" instead, fired on the bare
    # point estimate, which called a J within sampling variation of 0 anti-predictive
    # (check-function-full, 2026-09-30). Its interval lies wholly below 0.40, so the graded
    # discrimination notice calls it poor.
    nt1 <- .ir_notices(.ir_run(d, t1 = "t2", t2 = NULL))
    chance1 <- nt1[nt1$title == "Test Performs at Chance Level", ]
    expect_equal(nrow(chance1), 1L)
    expect_identical(chance1$severity, "Note")
    expect_true(startsWith(chance1$content, paste0(
        "Youden's J is below 0 but within sampling variation of 0 for Test 1 (J = -0.010, 95% CI ",
        .ir_f3(ci[1, "lwr.ci"]), " to ", .ir_f3(ci[1, "upr.ci"]),
        "), on the 200 complete cases analysed here.")))
    expect_identical(nt1$severity[nt1$title == "Poor Discrimination"], "Serious warning")
    expect_false(any(c("Positive Levels May Be Inverted", "No Rule Performs Better Than Chance") %in%
                         nt1$title))
})

test_that("IR-02c a small single-test sample below chance beyond sampling error is flagged as inverted", {
    # D: 2 positive, 6 negative; N: 9 positive, 3 negative. J = 2/8 + 3/12 - 1 = -0.500.
    nt <- .ir_notices(.ir_run(.ir_mk(c("+" = 2, "-" = 6), c("+" = 9, "-" = 3)), t2 = NULL))
    inv <- nt[nt$title == "Positive Levels May Be Inverted", ]
    expect_equal(nrow(inv), 1L)
    expect_identical(inv$severity, "Serious warning")
    ci <- .ir_ac(2, 8, 9, 12, conf.level = 0.95, method = "ac")
    expect_lt(ci[1, "upr.ci"], 0)
    expect_true(startsWith(inv$content, paste0(
        "Test 1 classifies worse than chance in this sample, beyond sampling variation: ",
        "Youden's J = -0.500 (Agresti-Caffo 95% CI ", .ir_f3(ci[1, "lwr.ci"]), " to ",
        .ir_f3(ci[1, "upr.ci"]), ") on the 20 complete cases analysed here.")))
    # No pattern rows, so no sentence about combinations.
    expect_false(grepl("Every combination", inv$content, fixed = TRUE))
    expect_false("Test Performs at Chance Level" %in% nt$title)
})

test_that("IR-02d an exact null: no rule beats chance, no inversion warning, 7 candidates for two tests", {
    # Every pattern 10 times in each group: every rule has sens = a/40 and spec = (40 - a)/40,
    # so J = 0 exactly (quarters are exact in binary).
    each <- c("+/+" = 10, "+/-" = 10, "-/+" = 10, "-/-" = 10)
    res <- .ir_run(.ir_mk(each, each))
    expect_identical(res$combinationTable$asDF$youden, rep(0, 8))
    nt <- .ir_notices(res)
    none <- nt[nt$title == "No Rule Performs Better Than Chance", ]
    expect_equal(nrow(none), 1L)
    # 8 rows, "+/+" merged into its twin Serial (all pos): 7 candidates.
    expect_true(startsWith(none$content,
                           "None of the 7 candidate rule(s) that could be scored has a Youden's J above zero"))
    expect_false(any(c("Positive Levels May Be Inverted", "Test Performs at Chance Level") %in%
                         nt$title))
})

test_that("IR-02e several reversed tests are listed together in one warning", {
    # K3 with Tests 2 and 3 reversed.
    #   Test 2 "neg": tp = D(+/-/+, +/-/-, -/-/+, -/-/-) = 6 + 5 + 3 + 7 = 21 of 60,
    #                 fp = 2 + 8 + 10 + 55 = 75 of 90; J = 21/60 + 15/90 - 1 = -0.483
    #   Test 3 "neg": tp = 24 of 60, fp = 75 of 90 (IR-02a); J = -0.433
    res <- .ir_run(.ir_mk(.IR_K3$cD, .IR_K3$cN), t2p = "neg", t3 = "t3", t3p = "neg")
    inv <- .ir_notice(res, "Positive Levels May Be Inverted")
    expect_length(inv, 1L)
    ci2 <- .ir_ac(21, 60, 75, 90, conf.level = 0.95, method = "ac")
    ci3 <- .ir_ac(24, 60, 75, 90, conf.level = 0.95, method = "ac")
    expect_true(startsWith(inv, paste0(
        "Several tests classify worse than chance in this sample, beyond sampling variation, ",
        "on the 150 complete cases analysed here: ",
        "Test 2 (J = -0.483, 95% CI ", .ir_f3(ci2[1, "lwr.ci"]), " to ", .ir_f3(ci2[1, "upr.ci"]), "); ",
        "Test 3 (J = -0.433, 95% CI ", .ir_f3(ci3[1, "lwr.ci"]), " to ", .ir_f3(ci3[1, "upr.ci"]), ").")))
    expect_match(inv, "Every combination that uses such a test is affected as well.", fixed = TRUE)
})

test_that("IR-02f a J closer to 0 than 0.0005 keeps its sign in the chance-level note", {
    # Test 2: 44 of 45 diseased and 45 of 46 non-diseased positive.
    # J = 44/45 - 45/46 = (2024 - 2025) / 2070 = -1/2070 = -0.000483 -> "-0.00048" (2 s.f.)
    cD <- c("+/+" = 40, "+/-" = 1, "-/+" = 4, "-/-" = 0)
    cN <- c("+/+" = 5, "+/-" = 0, "-/+" = 40, "-/-" = 1)
    chance <- .ir_notice(.ir_run(.ir_mk(cD, cN)), "Test Performs at Chance Level")
    expect_length(chance, 1L)
    expect_match(chance, "for Test 2 (J = -0.00048, 95% CI ", fixed = TRUE)
})

# ---- IR-03 to IR-07 -----------------------------------------------------------------------

test_that("IR-03 the sparse-cell notice calls the ratio intervals approximate, not conservative", {
    # IR-01a table. Smallest cells: "+/+" fp 4, "-/-" tp 4, Parallel fn 4, Serial fp 4;
    # "+/-" and "-/+" have 6, the single tests 10.
    cD <- c("+/+" = 54, "+/-" = 36, "-/+" = 6, "-/-" = 4)
    cN <- c("+/+" = 4, "+/-" = 6, "-/+" = 36, "-/-" = 54)
    res <- .ir_run(.ir_mk(cD, cN))
    sparse <- .ir_notice(res, "Sparse Cell Counts")
    expect_length(sparse, 1L)
    # A plain warning (check-function-full, 2026-09-30): as a serious one it fired on every
    # bundled example dataset, and it concerns only the ratio columns.
    nt <- .ir_notices(res)
    expect_identical(nt$severity[nt$title == "Sparse Cell Counts"], "Warning")
    expect_true(startsWith(sparse, paste0(
        "These rows have a 2-by-2 cell count below 5 (smallest cell 4): ",
        "+/+, -/-, Parallel (>=1 pos), Serial (all pos).")))
    expect_match(sparse, paste0(
        "the log-scale intervals for these ratios are approximate: their actual coverage can be ",
        "above or below the nominal 95%, and where the 0.5 continuity correction was applied it ",
        "can fall below it."), fixed = TRUE)
    expect_false(grepl("conservative", sparse, fixed = TRUE))
})

test_that("IR-04 the Assumptions panel requires one row per patient", {
    res <- .ir_run(.ir_mk(.IR_K2$cD, .IR_K2$cN), showAbout = TRUE)
    txt <- .ir_unescape(res$assumptions$content)
    expect_match(txt, "<strong>One row per patient.</strong> Every row must be a different patient.",
                 fixed = TRUE)
    expect_match(txt, paste0("is given in the warning \"Removed N case(s) with missing values\" ",
                             "in the notices at the top of the results"), fixed = TRUE)
})

test_that("IR-05 the pattern column covers every case with all tests observed, reference or not", {
    d <- .ir_mk(.IR_K2$cD, .IR_K2$cN)
    # Each row's own pattern, from the fixture design (rows 1-40 diseased, 41-100 not).
    own <- setNames(rep(c(.IR_P2, .IR_P2), c(.IR_K2$cD, .IR_K2$cN)), 1:100)
    d$gold[c(30, 45)] <- NA  # reference missing
    d$t1[50] <- NA           # a test missing
    d$t2[70] <- NA
    shown <- d[21:100, ]     # a filter hides rows 1-20; row names keep the source numbers

    op <- .ir_run(shown)$addedPattern$.__enclos_env__$private
    rows <- setdiff(21:100, c(50L, 70L))
    expect_identical(op$.rowNums, rows)
    expect_identical(as.character(op$.values[[1]]), unname(own[as.character(rows)]))
    expect_identical(unname(own[c("30", "45")]), c("-/+", "+/-"))  # the reference-missing rows

    # One test: "+" or "-", and a missing Test 2 no longer matters.
    op1 <- .ir_run(shown, t2 = NULL)$addedPattern$.__enclos_env__$private
    rows1 <- setdiff(21:100, 50L)
    expect_identical(op1$.rowNums, rows1)
    expect_identical(as.character(op1$.values[[1]]), unname(substr(own[as.character(rows1)], 1, 1)))
})

test_that("IR-06 test columns named like internal working columns do not change the table", {
    plain <- .ir_mk(.IR_K2$cD, .IR_K2$cN, pos = "Positive", neg = "Negative")
    ref <- .ir_run(plain, t1p = "Positive", t2p = "Positive")$combinationTable$asDF
    clash <- plain
    names(clash) <- c("gold", "goldVariable2", "test1Variable2")
    got <- .ir_run(clash, t1 = "goldVariable2", t1p = "Positive",
                   t2 = "test1Variable2", t2p = "Positive")$combinationTable$asDF
    expect_identical(got, ref)
    # "+/+": 20 of 40 diseased, 3 of 60 non-diseased
    expect_equal(.ir_cells(.ir_row(got, "+/+")), c(20, 3, 20, 57))
})

test_that("IR-07 the 0.5 correction is added to all four cells; Haldane-Anscombe names the odds-ratio case", {
    # No non-diseased "+/+" patient: "+/+" and Serial have fp = 0 and no empty margin.
    cD <- c("+/+" = 20, "+/-" = 8, "-/+" = 6, "-/-" = 6)   # 40 diseased
    cN <- c("+/+" = 0, "+/-" = 7, "-/+" = 10, "-/-" = 43)  # 60 non-diseased
    a <- .ir_object(.ir_mk(cD, cN), showForest = TRUE)
    res <- a$results

    note <- res$combinationTable$notes$haldane$note
    expect_match(note, paste0(
        "LR+, LR- and the diagnostic odds ratio are computed with 0.5 added to all four cells ",
        "when a cell is zero, so they stay finite (for the odds ratio this is the ",
        "Haldane-Anscombe correction; the likelihood ratios use the same adjustment)"), fixed = TRUE)
    continuity <- .ir_notice(res, "Continuity Correction")
    expect_length(continuity, 1L)
    expect_true(startsWith(continuity, paste0(
        "A continuity correction of 0.5, added to all four cells, was applied to 2 row(s) with at ",
        "least one zero cell (+/+, Serial (all pos)). For the diagnostic odds ratio this is the ",
        "Haldane-Anscombe correction; the likelihood ratios use the same adjustment.")))

    # Corrected cells 20.5, 0.5, 20.5, 60.5: LR+ = (20.5/41) / (0.5/61) = 61, DOR = 121.
    row <- .ir_row(res$combinationTable$asDF, "+/+")
    expect_equal(c(row$lrPos, row$dor), c(61, 121), tolerance = 1e-12)

    grDevices::pdf(NULL)
    withr::defer(grDevices::dev.off())
    built <- a$.__enclos_env__$private$.buildForestPanels(res$forestPlot$state)
    caption <- gsub("\\s+", " ", built$panels[[length(built$panels)]]$labels$caption)
    expect_match(caption, paste0(
        "Open points: rows with a zero cell, whose LR+, LR- and DOR use a 0.5 continuity ",
        "correction; without it at least one of the three would be 0 or infinite."), fixed = TRUE)
})

# ---- IR-R2: second review round -----------------------------------------------------------

# How many times a notice title occurs in the rendered notices HTML.
.ir_count <- function(res, title) {
    h <- res$notices$content
    if (is.null(h)) 0L else lengths(regmatches(h, gregexpr(title, h, fixed = TRUE)))
}

test_that("IR-R2-1 a near-tie between different 2x2 tables goes to the rule listed first", {
    # 100 diseased, 40 non-diseased.
    cD <- c("+/+" = 82, "+/-" = 5, "-/+" = 5, "-/-" = 8)
    cN <- c("+/+" = 2, "+/-" = 0, "-/+" = 2, "-/-" = 36)
    # Test 1 alone (+/+, +/-): tp 82 + 5 = 87, fp 2 + 0 = 2; J = 87/100 + 38/40 - 1 = 0.82
    # Parallel (all but -/-):  tp 100 - 8 = 92, fp 40 - 36 = 4; J = 92/100 + 36/40 - 1 = 0.82
    res <- .ir_run(.ir_mk(cD, cN), showRecommendation = TRUE)
    tb <- res$combinationTable$asDF
    one <- .ir_row(tb, "Test 1 alone")
    par <- .ir_row(tb, "Parallel (>=1 pos)")
    expect_equal(.ir_cells(one), c(87, 2, 13, 38))
    expect_equal(.ir_cells(par), c(92, 4, 8, 36))
    expect_equal(c(one$youden, par$youden), c(0.82, 0.82), tolerance = 1e-12)
    # Equal by hand, but in binary floating point Parallel comes out about 2e-16 higher. The
    # test guards the tie-break only while that holds: if this fails, pick a new fixture.
    expect_gt(par$youden, one$youden)

    rec <- res$recommendationTable$asDF
    # which.max() would crown Parallel, the later row, on that 2e-16.
    expect_identical(rec$pattern, "Test 1 alone")
    expect_match(rec$rationale, paste0(
        "2 rules tie on Youden's J (Test 1 alone, Parallel (>=1 pos)); \"Test 1 alone\" is ",
        "displayed only because it comes first (named rules are listed before exact patterns, ",
        "and rules that use fewer tests come first)."), fixed = TRUE)
    # Parallel is tied but has a different 2-by-2 table, so it is the runner-up: the
    # comparison is with 0.820, not with the next J down (Test 2 alone and Serial, 0.770).
    lower <- stats::prop.test(87, 100, correct = FALSE)$conf.int[1] +
        stats::prop.test(38, 40, correct = FALSE)$conf.int[1] - 1
    expect_match(rec$rationale, paste0(
        "a conservative 95% lower bound for this rule's Youden's J (", .ir_f3(lower),
        ", from the lower Wilson limits of its sensitivity and specificity) falls at or below the ",
        "point estimate of the best rule with a different 2-by-2 table (0.820)"),
        fixed = TRUE)
    # The positive patterns +/+ (0.77) and +/- (0.05) together are Test 1 alone: nothing higher.
    expect_false(grepl("No listed rule reaches", rec$rationale, fixed = TRUE))
})

test_that("IR-R2-2 both positive levels swapped: one inversion warning, naming both tests", {
    # K2 with "neg" chosen as positive for both tests.
    #   Test 1: tp = D(-/+, -/-) = 6 + 6 = 12 of 40, fp = N(-/+, -/-) = 10 + 40 = 50 of 60;
    #           J = 12/40 + 10/60 - 1 = -0.533
    #   Test 2: tp = 14 of 40, fp = 47 of 60 (IR-02a); J = -0.433
    # The recoded "-/-" is the original "+/+" (D 20 of 40, N 3 of 60; J = 0.450) and tops the
    # table, so the collective signature holds too: it must not add a second notice.
    res <- .ir_run(.ir_mk(.IR_K2$cD, .IR_K2$cN), t1p = "neg", t2p = "neg")
    tb <- res$combinationTable$asDF
    expect_equal(.ir_row(tb, "-/-")$youden, 20 / 40 + 57 / 60 - 1, tolerance = 1e-12)
    expect_identical(.ir_keys(tb)[which.max(tb$youden)], "-/-")

    expect_equal(.ir_count(res, "Positive Levels May Be Inverted"), 1L)
    ci1 <- .ir_ac(12, 40, 50, 60, conf.level = 0.95, method = "ac")
    ci2 <- .ir_ac(14, 40, 47, 60, conf.level = 0.95, method = "ac")
    expect_lt(max(ci1[1, "upr.ci"], ci2[1, "upr.ci"]), 0)
    expect_true(startsWith(.ir_notice(res, "Positive Levels May Be Inverted"), paste0(
        "Several tests classify worse than chance in this sample, beyond sampling variation, ",
        "on the 100 complete cases analysed here: ",
        "Test 1 (J = -0.533, 95% CI ", .ir_f3(ci1[1, "lwr.ci"]), " to ", .ir_f3(ci1[1, "upr.ci"]), "); ",
        "Test 2 (J = -0.433, 95% CI ", .ir_f3(ci2[1, "lwr.ci"]), " to ", .ir_f3(ci2[1, "upr.ci"]), ").")))
    expect_identical(.ir_f3(12 / 40 + 10 / 60 - 1), "-0.533")
})

test_that("IR-R2-3 the collective signature is graded: it fires only when its pattern clears chance", {
    generic <- paste0(
        "The rule that separates the two groups best in this sample is one that calls a patient ",
        "positive when the tests are negative; or every named rule (each test alone and every ",
        "strategy) performs at or below chance while an exact result pattern does not.")

    # (i) Noise, not a reversal. Every named rule is below chance and a pattern is above it, but
    # every J is within sampling variation of 0. Ungraded, this raised the serious warning, as it
    # did on about 27% of samples of two tests unrelated to the disease; graded, it must not.
    # 100 diseased, 100 non-diseased, so pattern J = (D - N) / 100:
    #   +/+ -0.08, +/- 0.02, -/+ 0.04, -/- 0.02
    #   Test 1 alone (+/+, +/-) -0.06 | Test 2 alone (+/+, -/+) -0.04 | Parallel -0.02 | Serial -0.08
    cD <- c("+/+" = 20, "+/-" = 22, "-/+" = 24, "-/-" = 34)
    cN <- c("+/+" = 28, "+/-" = 20, "-/+" = 20, "-/-" = 32)
    # Test 1 positive in 20 + 22 = 42 diseased and 28 + 20 = 48 non-diseased; Test 2 in 44 and 48.
    ci1 <- .ir_ac(42, 100, 48, 100, conf.level = 0.95, method = "ac")
    ci2 <- .ir_ac(44, 100, 48, 100, conf.level = 0.95, method = "ac")
    # Neither test is below chance beyond sampling variation: the per-test warning cannot fire.
    expect_gte(min(ci1[1, "upr.ci"], ci2[1, "upr.ci"]), 0)
    # No pattern clears chance either: the best, "-/+", is 24 vs 20 of 100.
    for (p in list(c(22, 20), c(24, 20), c(34, 32))) {
        expect_lte(.ir_ac(p[1], 100, p[2], 100)[1, "lwr.ci"], 0)
    }
    res <- .ir_run(.ir_mk(cD, cN), showRecommendation = TRUE)
    expect_equal(res$combinationTable$asDF$youden,
                 c(-0.08, 0.02, 0.04, 0.02, -0.06, -0.04, -0.02, -0.08), tolerance = 1e-12)
    expect_identical(res$recommendationTable$asDF$pattern, "-/+")
    expect_equal(.ir_count(res, "Positive Levels May Be Inverted"), 0L)
    # The two tests below 0 within sampling variation get the quiet note, one item per test.
    expect_true(startsWith(.ir_notice(res, "Test Performs at Chance Level"), paste0(
        "Youden's J is below 0 but within sampling variation of 0 for ",
        "Test 1 (J = -0.060, 95% CI ", .ir_f3(ci1[1, "lwr.ci"]), " to ", .ir_f3(ci1[1, "upr.ci"]), "); ",
        "Test 2 (J = -0.040, 95% CI ", .ir_f3(ci2[1, "lwr.ci"]), " to ", .ir_f3(ci2[1, "upr.ci"]), "), ",
        "on the 200 complete cases analysed here.")))

    # (ii) The all-negative pattern tops the table, clearly above chance, while a named rule is
    # above chance.
    #   +/+ 0.10, +/- -0.15, -/+ -0.15, -/- 0.20
    #   Test 1 alone -0.05 | Test 2 alone -0.05 | Parallel -0.20 | Serial 0.10
    # Serial is above chance, so only the first half of the signature holds.
    cD2 <- c("+/+" = 30, "+/-" = 10, "-/+" = 10, "-/-" = 50)
    cN2 <- c("+/+" = 20, "+/-" = 25, "-/+" = 25, "-/-" = 30)
    # Each test positive in 30 + 10 = 40 diseased and 20 + 25 = 45 non-diseased.
    ci <- .ir_ac(40, 100, 45, 100, conf.level = 0.95, method = "ac")
    expect_gte(ci[1, "upr.ci"], 0)
    # "-/-": 50 of 100 diseased against 30 of 100 non-diseased clears chance (lower limit > 0).
    expect_gt(.ir_ac(50, 100, 30, 100)[1, "lwr.ci"], 0)
    res2 <- .ir_run(.ir_mk(cD2, cN2), showRecommendation = TRUE)
    expect_equal(res2$combinationTable$asDF$youden,
                 c(0.10, -0.15, -0.15, 0.20, -0.05, -0.05, -0.20, 0.10), tolerance = 1e-12)
    expect_identical(res2$recommendationTable$asDF$pattern, "-/-")
    expect_equal(.ir_count(res2, "Positive Levels May Be Inverted"), 1L)
    expect_true(startsWith(.ir_notice(res2, "Positive Levels May Be Inverted"), generic))
})

test_that("IR-R2-4 a single test clearly below chance gets the inversion warning instead of the chance notice", {
    # D: 10 positive, 30 negative; N: 30 positive, 10 negative. J = 10/40 + 10/40 - 1 = -0.500.
    # 40 per group passes the ranking's 10-per-group gate and no rule has J > 0. The specific
    # warning is raised, and it silences both the chance note and the graded discrimination
    # notice that a single test with J <= 0 otherwise gets.
    nt <- .ir_notices(.ir_run(.ir_mk(c("+" = 10, "-" = 30), c("+" = 30, "-" = 10)), t2 = NULL))
    ci <- .ir_ac(10, 40, 30, 40, conf.level = 0.95, method = "ac")
    expect_lt(ci[1, "upr.ci"], 0)
    inv <- nt[nt$title == "Positive Levels May Be Inverted", ]
    expect_equal(nrow(inv), 1L)
    expect_identical(inv$severity, "Serious warning")
    expect_true(startsWith(inv$content, paste0(
        "Test 1 classifies worse than chance in this sample, beyond sampling variation: ",
        "Youden's J = -0.500 (Agresti-Caffo 95% CI ", .ir_f3(ci[1, "lwr.ci"]), " to ",
        .ir_f3(ci[1, "upr.ci"]), ") on the 80 complete cases analysed here.")))
    expect_false(any(c("No Rule Performs Better Than Chance", "Test Performs at Chance Level") %in%
                         nt$title))
})

test_that("IR-R2-5 a single test below 0 within sampling variation in a small group gets the note", {
    # D: 3 positive, 5 negative (8 diseased, under 10); N: 7 positive, 5 negative.
    # J = 3/8 + 5/12 - 1 = -0.208, within sampling variation of 0: the quiet note, as for a
    # larger single test (IR-02b). Below the 10-per-group gate there is no discrimination notice.
    nt <- .ir_notices(.ir_run(.ir_mk(c("+" = 3, "-" = 5), c("+" = 7, "-" = 5)), t2 = NULL))
    ci <- .ir_ac(3, 8, 7, 12, conf.level = 0.95, method = "ac")
    expect_gte(ci[1, "upr.ci"], 0)
    expect_identical(.ir_f3(3 / 8 + 5 / 12 - 1), "-0.208")
    chance <- nt[nt$title == "Test Performs at Chance Level", ]
    expect_equal(nrow(chance), 1L)
    expect_identical(chance$severity, "Note")
    expect_true(startsWith(chance$content, paste0(
        "Youden's J is below 0 but within sampling variation of 0 for Test 1 (J = -0.208, 95% CI ",
        .ir_f3(ci[1, "lwr.ci"]), " to ", .ir_f3(ci[1, "upr.ci"]),
        "), on the 20 complete cases analysed here.")))
    expect_false(any(c("Positive Levels May Be Inverted", "No Rule Performs Better Than Chance") %in%
                         nt$title))
})

test_that("IR-R2-6 the forest height set in .init() follows the pattern and statistic filters", {
    d <- .ir_mk(.IR_K3$cD, .IR_K3$cN)
    height <- function(t3, pattern, statistic) {
        .ir_object(d, t3 = t3, run = FALSE, filterPattern = pattern,
                   filterStatistic = statistic)$results$forestPlot$height
    }
    # height = max(400, 170 + n_stats * (45 + 22 * n_rules)); n_stats is 8 for "all", else 1.
    #   All negative, 1 rule (any k):  170 + 8 * (45 + 22)  =  706 | 170 + 45 + 22  = 237 -> 400
    #   Mixed, k = 2, 2^2 - 2 = 2:     170 + 8 * (45 + 44)  =  882 | 170 + 45 + 44  = 259 -> 400
    #   Mixed, k = 3, 2^3 - 2 = 6:     170 + 8 * (45 + 132) = 1586 | 170 + 45 + 132 = 347 -> 400
    expect_equal(height(NULL, "allNegative", "all"), 706)
    expect_equal(height(NULL, "allNegative", "sens"), 400)
    expect_equal(height(NULL, "mixed", "all"), 882)
    expect_equal(height(NULL, "mixed", "sens"), 400)
    expect_equal(height("t3", "allNegative", "all"), 706)
    expect_equal(height("t3", "allNegative", "sens"), 400)
    expect_equal(height("t3", "mixed", "all"), 1586)
    expect_equal(height("t3", "mixed", "sens"), 400)
})

test_that("IR-R2-7 the 'No listed rule reaches' sentence is decided and worded on rounded values", {
    # 2000 diseased, 3000 non-diseased, so pattern J = D/2000 - N/3000. With 900 diseased
    # patients each, +/- and -/+ are the positive patterns and "exactly one test positive"
    # ({+/-, -/+}), which no row lists, is the J-optimum. Parallel adds +/+ to it.
    cD <- c("+/+" = 2, "+/-" = 900, "-/+" = 900, "-/-" = 198)
    optimum <- (900 / 2000 - 150 / 3000) + (900 / 2000 - 149 / 3000)  # 0.800333 -> 0.800

    # (a) +/+ would be neutral at 2 of 2000 and 3 of 3000; one more non-diseased patient there
    # (4 of 3000) gives it J = -1/3000, so Parallel = 1802/2000 + 2697/3000 - 1 = 0.800000.
    # The optimum is higher by 0.00033, invisible at three decimals: no sentence.
    best_a <- 1802 / 2000 + 2697 / 3000 - 1
    expect_gt(optimum - best_a, 0)
    expect_lt(optimum - best_a, 0.0005)
    expect_identical(round(optimum, 3), round(best_a, 3))
    rec_a <- .ir_run(.ir_mk(cD, c("+/+" = 4, "+/-" = 150, "-/+" = 149, "-/-" = 2697)),
                     showRecommendation = TRUE)$recommendationTable$asDF
    expect_identical(rec_a$pattern, "Parallel (>=1 pos)")
    expect_equal(rec_a$youden, best_a, tolerance = 1e-12)
    expect_false(grepl("No listed rule reaches", rec_a$rationale, fixed = TRUE))

    # (b) 11 non-diseased +/+ patients: Parallel = 1802/2000 + 2690/3000 - 1 = 0.797667 -> 0.798,
    # so the sentence says 0.800 - 0.798 = 0.002. The unrounded gain, 0.00267, rounds to 0.003
    # and would not match the two J values printed beside it.
    best_b <- 1802 / 2000 + 2690 / 3000 - 1
    expect_identical(.ir_f3(round(optimum - best_b, 3)), "0.003")
    rec_b <- .ir_run(.ir_mk(cD, c("+/+" = 11, "+/-" = 150, "-/+" = 149, "-/-" = 2690)),
                     showRecommendation = TRUE)$recommendationTable$asDF
    expect_identical(rec_b$pattern, "Parallel (>=1 pos)")
    expect_equal(rec_b$youden, best_b, tolerance = 1e-12)
    expect_identical(.ir_f3(round(optimum, 3) - round(best_b, 3)), "0.002")
    expect_match(rec_b$rationale, paste0(
        "No listed rule reaches the highest Youden's J these results allow: calling positive ",
        "exactly the patterns +/-, -/+ would give J = ", .ir_f3(optimum), ", ",
        .ir_f3(round(optimum, 3) - round(best_b, 3)), " above the highest-ranked rule."),
        fixed = TRUE)
})

# Entries of a gettext catalogue: msgid, msgstr, the #: references and whether the entry is
# obsolete (#~). A string may continue over several "..." lines; \" \\ \n and \t are
# unescaped. A line the parser does not recognise is an error, so a format change fails loudly
# instead of selecting nothing. The string work is vectorised and the loop only assembles the
# entries: done line by line, it took 14 s on the 6 MB umbrella catalogue.
.ir_po_entries <- function(path) {
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    kind <- ifelse(grepl('^(msgctxt|msgid|msgstr) "', lines), sub(" .*$", "", lines), "?")
    kind[startsWith(lines, "\"")] <- "\""
    kind[startsWith(lines, "#")] <- "#"
    kind[startsWith(lines, "#:")] <- "#:"
    kind[startsWith(lines, "#~")] <- "#~"
    kind[startsWith(lines, "#,")] <- "#,"
    kind[!nzchar(trimws(lines))] <- ""
    text <- sub('^[^"]*"(.*)"[[:space:]]*$', "\\1", lines)
    esc <- kind %in% c("msgctxt", "msgid", "msgstr", "\"") & grepl("\\", text, fixed = TRUE)
    m <- gregexpr("\\\\.", text[esc])
    unescaped <- text[esc]
    regmatches(unescaped, m) <- lapply(regmatches(unescaped, m), function(e) {
        ch <- substring(e, 2)
        ifelse(ch == "n", "\n", ifelse(ch == "t", "\t", ch))
    })
    text[esc] <- unescaped

    entries <- list()
    cur <- NULL
    field <- NULL
    flush <- function() {
        if (!is.null(cur$msgid) || isTRUE(cur$obsolete)) {
            if (is.null(cur$msgstr)) cur$msgstr <- ""
            entries[[length(entries) + 1L]] <<- cur
        }
        cur <<- NULL
        field <<- NULL
    }
    for (i in seq_along(lines)) {
        k <- kind[i]
        if (k == "") {
            flush()
            next
        }
        # A comment or keyword after a complete msgstr starts the next entry.
        if (!is.null(cur$msgstr) && k != "\"") flush()
        if (is.null(cur)) cur <- list(refs = character(), obsolete = FALSE, fuzzy = FALSE)
        if (k == "#~") {
            cur$obsolete <- TRUE
        } else if (k == "#,") {
            # jamovi's catalogue conversion (po2json, fuzzy: false) drops fuzzy translations,
            # so a fuzzy entry ships in English and must count as untranslated.
            cur$fuzzy <- isTRUE(cur$fuzzy) || grepl("fuzzy", lines[i], fixed = TRUE)
        } else if (k == "#:") {
            cur$refs <- c(cur$refs, strsplit(trimws(substring(lines[i], 3)), "[[:space:]]+")[[1]])
        } else if (k %in% c("msgctxt", "msgid", "msgstr")) {
            field <- k
            cur[[k]] <- text[i]
        } else if (k == "\"" && !is.null(field)) {
            cur[[field]] <- paste0(cur[[field]], text[i])
        } else if (k != "#") {  # "#" = translator, extracted-comment and flag lines
            stop("unparsed line in ", basename(path), ": ", lines[i])
        }
    }
    flush()
    entries
}

.ir_placeholders <- function(s) {
    sort(unique(gsub("[[:space:]]", "", regmatches(s, gregexpr("\\{[^{}]*\\}", s))[[1]])))
}

.ir_msgids <- function(entries) vapply(entries, function(e) e$msgid, "")

test_that("IR-R2-8a every Turkish entry of the analysis is translated and keeps its placeholders", {
    po <- testthat::test_path("..", "..", "jamovi", "i18n", "tr.po")
    skip_if_not(file.exists(po))
    # The parser itself, on the escapes and the continuation lines it must handle.
    tmp <- withr::local_tempfile(fileext = ".po")
    writeLines(c('#: R/x.b.R', 'msgid ""', '"say \\"{n}\\" "', '"a\\\\b"', 'msgstr "x"', '"y"', '',
                 '#~ msgid "gone"', '#~ msgstr ""', '',
                 '#: R/x.b.R', '#, fuzzy', 'msgid "old"', 'msgstr "eski"'), tmp)
    parsed <- .ir_po_entries(tmp)
    expect_identical(parsed[[1]]$msgid, "say \"{n}\" a\\b")
    expect_identical(parsed[[1]]$msgstr, "xy")
    expect_identical(parsed[[1]]$refs, "R/x.b.R")
    expect_identical(vapply(parsed, function(e) e$obsolete, TRUE), c(FALSE, TRUE, FALSE))
    expect_identical(vapply(parsed, function(e) e$fuzzy, TRUE), c(FALSE, FALSE, TRUE))

    ours <- Filter(function(e) !e$obsolete &&
                       any(grepl("(^|/)decisioncombine[./]", e$refs)), .ir_po_entries(po))
    # 275 entries on 2026-09-29; none found would mean the file or the parser is broken.
    expect_gt(length(ours), 100L)
    expect_identical(.ir_msgids(Filter(function(e) !nzchar(e$msgstr) || e$fuzzy, ours)),
                     character())
    expect_identical(
        .ir_msgids(Filter(function(e) nzchar(e$msgstr) &&
                              !identical(.ir_placeholders(e$msgid), .ir_placeholders(e$msgstr)),
                          ours)),
        character())
})

test_that("IR-R2-8b no translatable string of the analysis is missing from the Turkish catalogue", {
    # IR-R2-8a sees only the entries the catalogue has. A string the catalogue lacks -- such as
    # the per-test "{test} (J = {youden}, 95% CI {lower} to {upper})" item, which exists so that
    # this part of a Turkish notice is not left in English -- is shown in English, and only a
    # check from the source side finds it. The strings are the literal arguments of .() and
    # jmvcore::.() in the backend.
    po <- testthat::test_path("..", "..", "jamovi", "i18n", "tr.po")
    src <- testthat::test_path("..", "..", "R", "decisioncombine.b.R")
    skip_if_not(file.exists(po) && file.exists(src))
    strings <- character()
    walk <- function(x) {
        if (is.call(x) && paste(deparse(x[[1]]), collapse = "") %in% c(".", "jmvcore::.") &&
            length(x) >= 2L && is.character(x[[2]])) {
            strings <<- c(strings, x[[2]])
        }
        if (is.call(x) || is.expression(x) || is.pairlist(x)) {
            for (i in seq_along(x)) {
                # The empty symbol of a missing argument, as in x[i, , drop = FALSE], is skipped.
                if (!is.symbol(x[[i]]) || nzchar(as.character(x[[i]]))) walk(x[[i]])
            }
        }
    }
    walk(parse(src, keep.source = FALSE))
    strings <- unique(strings)
    # 182 on 2026-09-29, the three strings reworded that day among them.
    expect_gt(length(strings), 100L)
    expect_true("{test} (J = {youden}, 95% CI {lower} to {upper})" %in% strings)
    translated <- .ir_msgids(Filter(function(e) !e$obsolete && !e$fuzzy &&
                                        is.null(e$msgctxt) && nzchar(e$msgstr),
                                    .ir_po_entries(po)))
    expect_identical(setdiff(strings, translated), character())
})
