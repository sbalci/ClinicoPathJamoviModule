# Regression tests for the `/review-function decisioncombine` fix pass (2026-10-01).
# One test_that() per claim; each description starts with its item ID (RF-n).

dc_options <- function(...) getFromNamespace("decisioncombineOptions", "ClinicoPath")$new(...)
dc_class <- function() getFromNamespace("decisioncombineClass", "ClinicoPath")

# A private environment of an analysis object that has not been run, for calling the
# renderers directly with a hand-built state.
dc_private <- function() {
    dc_class()$new(
        options = dc_options(gold = NULL, goldPositive = NULL, test1 = NULL,
                             test1Positive = NULL, test2 = NULL, test2Positive = NULL,
                             test3 = NULL, test3Positive = NULL),
        data = data.frame(a = 1))$.__enclos_env__$private
}

heatmap_fills <- function(stat, values) {
    state <- list(
        valid = TRUE, filterStatistic = stat, filterPattern = "all",
        data = data.frame(pattern = paste0("r", seq_along(values)), rowType = "Strategy",
                          prevalence = 0.3, sens = 0.8, spec = 0.7, ppv = 0.5, npv = 0.9,
                          acc = 0.7, balancedAccuracy = 0.75, youden = 0.5,
                          lrPos = values, lrNeg = values, dor = values))
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expect_true(dc_private()$.plotHeatmap(list(state = state),
                                          ggtheme = ggplot2::theme_grey(), theme = NULL))
    built <- ggplot2::ggplot_build(ggplot2::last_plot())
    tiles <- built$data[[1]]
    toupper(tiles$fill[order(-tiles$y)])          # table order, top row first
}

GOOD <- "#74ADD1"   # the blue pole
BAD <- "#EF8A8A"    # the red pole
MID <- "#F7F7F7"    # neutral, with no jamovi theme supplied

test_that("RF-02a ratios are coloured on a log scale: 0.1 and 10 sit at opposite poles, 1 is neutral", {
    expect_identical(heatmap_fills("lrPos", c(0.1, 1, 10)), c(BAD, MID, GOOD))
    expect_identical(heatmap_fills("dor", c(0.1, 1, 10)), c(BAD, MID, GOOD))
    # On the old linear scale centred on 1, a strong test's LR+ of 13.7 was painted almost
    # neutral beside 60.7, and an LR+ of 0.02 looked like 1.
    fills <- heatmap_fills("lrPos", c(0.02, 60.7, 13.7))
    expect_identical(fills[2], GOOD)
    rgb <- grDevices::col2rgb(fills)
    expect_gt(rgb["red", 1], rgb["blue", 1])        # 0.02: clearly on the red side
    expect_gt(rgb["blue", 3] - rgb["red", 3], 40)   # 13.7: clearly blue, not near-neutral
})

test_that("RF-02b a small LR- is the good end: the best LR- is blue, the worst red", {
    # Measured before the fix: the worst LR- (3.63) took the blue pole and the best (0.024)
    # a pale pink.
    expect_identical(heatmap_fills("lrNeg", c(0.1, 1, 10)), c(GOOD, MID, BAD))
})

test_that("RF-02c a single tile or identical tiles show their direction and their size", {
    # With zero-width limits ggplot painted every tile in the mid colour, so the one tile of a
    # single-test analysis read as a ratio of 1 whatever its value. Stretching the range to the
    # midpoint instead painted every such tile at the full pole (LR+ 1.15 = LR+ 60). They now sit
    # on the metric's natural scale.
    blueness <- function(fill) { rgb <- grDevices::col2rgb(fill); rgb["blue", ] - rgb["red", ] }
    weak <- heatmap_fills("lrPos", 1.15)
    strong <- heatmap_fills("lrPos", 5)
    expect_gt(blueness(strong), blueness(weak))
    expect_gt(blueness(weak), 0)
    expect_false(identical(strong, GOOD))
    two <- heatmap_fills("lrPos", c(2, 2, 2))
    expect_identical(length(unique(two)), 1L)
    expect_gt(blueness(two[1]), 0)
    expect_gt(blueness(heatmap_fills("lrNeg", 0.2)), 0)      # a small LR- is the good end
    expect_identical(heatmap_fills("prevalence", c(0.3, 0.3, 0.3)), rep(MID, 3))
    # Blank cells stay blank (grey), with no error.
    expect_length(heatmap_fills("lrPos", c(NA_real_, NA_real_)), 2)
})

test_that("RF-04 every plot column is named through .data$, so no global binding is assumed", {
    # Bare aes() names gave R CMD check 12 'no visible binding' NOTEs, and a bare
    # aes(shape = corrected) also matched a same-named vector in the enclosing frame.
    methods <- c(dc_class()$private_methods, dc_class()$public_methods)
    ns <- asNamespace("ClinicoPath")
    messages <- unlist(lapply(names(methods), function(nm) {
        f <- methods[[nm]]
        if (!is.function(f)) return(character())
        environment(f) <- ns
        utils::capture.output(codetools::checkUsage(
            f, name = nm, all = TRUE, suppressLocalUnused = FALSE,
            suppressParamUnused = TRUE))
    }))
    messages <- messages[!grepl("global variable .(self|private|super).|changed by assignment",
                                messages)]
    expect_identical(messages, character(0))
})

# ---- shared fixtures for RF-01, RF-03 and RF-05 --------------------------------------------

# A data set from result-pattern counts, e.g. c("+/+" = 4, "+/-" = 3, ...), per reference group.
dc_counts <- function(cD, cN) {
    rows <- function(counts, gold) {
        signs <- strsplit(rep(names(counts), counts), "/", fixed = TRUE)
        k <- length(signs[[1]])
        out <- data.frame(gold = rep(gold, sum(counts)))
        for (j in seq_len(k)) {
            out[[paste0("t", j)]] <- ifelse(vapply(signs, `[`, "", j) == "+", "pos", "neg")
        }
        out
    }
    rbind(rows(cD, "D"), rows(cN, "N"))
}

dc_run <- function(d, k = 2, ...) {
    do.call(ClinicoPath::decisioncombine, c(list(
        data = d, gold = "gold", goldPositive = "D", test1 = "t1", test1Positive = "pos",
        test2 = if (k >= 2) "t2" else NULL, test2Positive = if (k >= 2) "pos" else NULL,
        test3 = if (k >= 3) "t3" else NULL, test3Positive = if (k >= 3) "pos" else NULL),
        list(...)))
}

dc_notices <- function(res) {
    html <- gsub("<br>", " || ", res$notices$content)
    blocks <- regmatches(html, gregexpr("<div style='background-color[^>]*>.*?</div>", html,
                                        perl = TRUE))[[1]]
    vapply(blocks, function(b) gsub("[[:space:]]+", " ", gsub("<[^>]+>", "", b)), "",
           USE.NAMES = FALSE)
}

# Three tests with known accuracy, 600 patients, prevalence 0.3.
dc_three <- function(seed = 7, n = 600) {
    set.seed(seed)
    disease <- stats::runif(n) < 0.3
    test <- function(se, sp) ifelse(disease, ifelse(stats::runif(n) < se, "pos", "neg"),
                                    ifelse(stats::runif(n) < 1 - sp, "pos", "neg"))
    data.frame(gold = ifelse(disease, "D", "N"), t1 = test(0.85, 0.80),
               t2 = test(0.80, 0.95), t3 = test(0.75, 0.85))
}

STRONG_TITLE <- "Missing Results Follow Another Result"
WEAK_TITLE <- "Missing Results Partly Follow Another Result"

test_that("RF-01a reflex testing is reported with its counts and what it does to the table", {
    # Measured before the fix: 351 Test 1-negative cases dropped, "Test 1 alone" 100% / 0%
    # (truth 89% / 78%), Serial sensitivity 70.9% -> 79.7%, and only "Removed 351 case(s)".
    d <- dc_three()
    d$t2[d$t1 == "neg"] <- NA
    text <- dc_notices(dc_run(d))
    strong <- text[grepl(STRONG_TITLE, text, fixed = TRUE)]
    expect_length(strong, 1L)
    n_neg <- sum(d$t1 == "neg")
    # Counts per reference group, the scale on which the severity is judged.
    neg_d <- sum(d$t1 == "neg" & d$gold == "D")
    neg_h <- sum(d$t1 == "neg" & d$gold == "N")
    expect_match(strong, sprintf(paste0("Test 2 was recorded, among disease-present cases, for 0 of %d ",
                                        "with Test 1 negative (0.0%%)"), neg_d), fixed = TRUE)
    expect_match(strong, sprintf("among disease-absent cases for 0 of %d with Test 1 negative (0.0%%)",
                                 neg_h), fixed = TRUE)
    expect_equal(neg_d + neg_h, n_neg)
    expect_match(strong, "The combination table therefore contains no case with Test 1 negative",
                 fixed = TRUE)
    expect_match(strong, "\"Test 1 alone\" row reads sensitivity 100% and specificity 0%",
                 fixed = TRUE)
    # Causes are stated conditionally, never as a finding.
    expect_false(grepl("reflex testing was used", strong, fixed = TRUE))
    # A few random losses on top of the reflex design do not soften the verdict.
    d$t2[sample(which(d$t1 == "pos"), 8)] <- NA
    expect_true(any(grepl(STRONG_TITLE, dc_notices(dc_run(d)), fixed = TRUE)))
})

test_that("RF-01b a referee test read only on disagreement is caught by the joint predictor", {
    d <- dc_three()
    d$t3[d$t1 == d$t2] <- NA
    strong <- grep(STRONG_TITLE, dc_notices(dc_run(d, 3)), fixed = TRUE, value = TRUE)
    expect_length(strong, 1L)
    expect_match(strong, "where Test 1 and Test 2 agreed (0.0%)", fixed = TRUE)
    expect_match(strong, "contains only cases where Test 1 and Test 2 disagreed", fixed = TRUE)
})

test_that("RF-01c partial verification names the reference standard and cites Begg and Greenes", {
    d <- dc_three()
    d$gold[d$t1 == "neg" & stats::runif(nrow(d)) < 0.7] <- NA
    res <- dc_run(d)
    strong <- grep(STRONG_TITLE, dc_notices(res), fixed = TRUE, value = TRUE)
    expect_length(strong, 1L)
    expect_match(strong, "The reference standard was recorded for", fixed = TRUE)
    expect_match(strong, "These data cannot tell the two apart", fixed = TRUE)
    expect_true("Begg1983" %in% res$notices$getRefs())
})

test_that("RF-01d a reflex on an equivocal level names the level, though it is folded into 'negative'", {
    # HER2: ISH after IHC 2+, with IHC positive = 3+. The untested cases fall in BOTH arms of
    # the dichotomised test, so a purity rule missed it; the recording-rate ratio does not.
    d <- dc_three()
    disease <- d$gold == "D"
    d$t1 <- ifelse(disease, sample(c("0", "eq", "pos"), nrow(d), TRUE, c(0.1, 0.4, 0.5)),
                   sample(c("0", "eq", "pos"), nrow(d), TRUE, c(0.6, 0.3, 0.1)))
    d$t2[d$t1 != "eq"] <- NA
    strong <- grep(STRONG_TITLE, dc_notices(dc_run(d)), fixed = TRUE, value = TRUE)
    expect_length(strong, 1L)
    expect_match(strong, "It was recorded only when Test 1 was \"eq\".", fixed = TRUE)
})

test_that("RF-01e missingness that follows disease status alone, or chance, raises nothing", {
    # Sensitivity and specificity are unbiased then; the crude test would have blamed any
    # accurate test, which is why a test's p is stratified by the reference standard.
    d <- dc_three()
    disease <- d$gold == "D"
    d$t1 <- ifelse(disease, ifelse(stats::runif(nrow(d)) < 0.95, "pos", "neg"),
                   ifelse(stats::runif(nrow(d)) < 0.03, "pos", "neg"))
    d$t2[!disease & stats::runif(nrow(d)) < 0.2] <- NA
    text <- dc_notices(dc_run(d))
    expect_false(any(grepl("Missing Results", text, fixed = TRUE)))
    m <- dc_three()
    m$t2[stats::runif(nrow(m)) < 0.2] <- NA
    m$gold[stats::runif(nrow(m)) < 0.1] <- NA
    expect_false(any(grepl("Missing Results", dc_notices(dc_run(m)), fixed = TRUE)))
})

test_that("RF-01f a moderate imbalance is a plain warning, and a partition is explained", {
    d <- dc_three()
    recorded <- ifelse(d$t1 == "pos", stats::runif(nrow(d)) < 0.9, stats::runif(nrow(d)) < 0.6)
    d$t2[!recorded] <- NA
    text <- dc_notices(dc_run(d))
    expect_true(any(grepl(WEAK_TITLE, text, fixed = TRUE)))
    expect_false(any(grepl(paste0(STRONG_TITLE, " ||"), text, fixed = TRUE)))
    # Test 2 only after a negative and Test 3 only after a positive Test 1: no case has every
    # result, and the exit now says why.
    p <- dc_three()
    p$t2[p$t1 == "pos"] <- NA
    p$t3[p$t1 == "neg"] <- NA
    text <- dc_notices(dc_run(p, 3))
    expect_true(any(grepl("No Complete Cases", text, fixed = TRUE)))
    strong <- grep(STRONG_TITLE, text, fixed = TRUE, value = TRUE)
    expect_length(strong, 1L)
    # No table was computed, so the notice must not describe one (it used to, twice, with
    # contradictory claims about which cases the table held).
    expect_false(grepl("The combination table therefore contains", strong, fixed = TRUE))
    expect_match(strong, "The analysis stopped before the combination table was computed", fixed = TRUE)
})

test_that("RF-01g a large reflex or referee design runs: the exact test's interval cannot stop it", {
    # mantelhaen.test(exact = TRUE) also fits a conditional MLE with uniroot(), which underflowed
    # from about 1,700 cases of pure reflex and stopped the whole analysis.
    d <- dc_three(seed = 1, n = 2000)
    d$t2[d$t1 == "neg"] <- NA
    res <- dc_run(d)
    expect_equal(res$combinationTable$rowCount, 8L)
    expect_true(any(grepl(STRONG_TITLE, dc_notices(res), fixed = TRUE)))
    r <- dc_three(seed = 2, n = 3000)
    r$t3[r$t1 == r$t2] <- NA
    res3 <- dc_run(r, 3)
    expect_equal(res3$combinationTable$rowCount, 20L)
    expect_true(any(grepl(STRONG_TITLE, dc_notices(res3), fixed = TRUE)))
})

test_that("RF-03 the always-visible notes are short, and the About panel keeps the details", {
    d <- dc_three()
    res <- dc_run(d, showAbout = TRUE)
    notes <- res$combinationTable$notes
    expect_setequal(names(notes), c("row_kinds", "haldane", "youden_ci", "ppv_npv"))
    words <- sum(vapply(notes, function(n) length(strsplit(n$note, "[[:space:]]+")[[1]]), 1))
    expect_lt(words, 240)   # was 613
    # The per-branch rule stays beside the number it explains.
    expect_match(notes$haldane$note, "its LR- is exactly 1", fixed = TRUE)
    about <- res$about$content
    expect_match(about, "left BLANK rather than corrected, while LR- is exactly 1", fixed = TRUE)
    expect_match(about, "pairwise-complete cases", fixed = TRUE)
    assumptions <- res$assumptions$content
    expect_false(grepl("This analysis performs neither", assumptions, fixed = TRUE))
    expect_match(assumptions, "Combinations vs each test alone", fixed = TRUE)
    expect_false(any(grepl("PPV/NPV Interpretation", dc_notices(res), fixed = TRUE)))
})

test_that("RF-05a unticking the interval tables hides them without emptying the forest plot", {
    d <- dc_three()
    res <- dc_run(d, showCITables = FALSE, showForest = TRUE)
    expect_false(res$combinationTableCI$visible)
    expect_false(res$combinationTableCIRatios$visible)
    expect_gt(res$combinationTableCI$rowCount, 0)
    expect_true(isTRUE(res$forestPlot$state$valid))
    expect_gt(nrow(res$forestPlot$state$proportions), 0)
    expect_true(dc_run(d)$combinationTableCI$visible)
})

wilson <- function(x, n) {
    r <- binom::binom.confint(x, n, methods = "wilson")
    c(r$lower, r$upper)
}

test_that("RF-05b nested rules: one zero discordant cell, a signed Wilson interval and no p", {
    skip_if_not_installed("binom")
    cD <- c("+/+" = 40, "+/-" = 10, "-/+" = 6, "-/-" = 14)
    cN <- c("+/+" = 4, "+/-" = 20, "-/+" = 9, "-/-" = 97)
    res <- dc_run(dc_counts(cD, cN), showPairedComparison = TRUE)
    tab <- res$pairedComparisonTable$asDF
    expect_identical(tab$rule, rep(c("Parallel (>=1 pos)", "Serial (all pos)"), each = 2))
    row <- function(rule, test) tab[tab$rule == rule & tab$comparator == test, ]
    # Parallel vs Test 1 gains the "-/+" diseased (6 of 70) and loses the "-/+" non-diseased
    # (9 of 130); nothing moves the other way.
    p1 <- row("Parallel (>=1 pos)", "Test 1 alone")
    expect_equal(c(p1$sensGained, p1$sensLost, p1$specGained, p1$specLost), c(6, 0, 0, 9))
    expect_equal(p1$sensChange, 6 / 70)
    expect_equal(c(p1$sensLower, p1$sensUpper), wilson(6, 70), tolerance = 1e-10)
    expect_equal(c(p1$specLower, p1$specUpper), -rev(wilson(9, 130)), tolerance = 1e-10)
    expect_true(all(is.na(c(tab$sensP, tab$specP))))
    # Serial vs Test 2 reclassifies the same patients the other way.
    s2 <- row("Serial (all pos)", "Test 2 alone")
    expect_equal(c(s2$sensChange, s2$sensLower, s2$sensUpper),
                 -c(p1$sensChange, p1$sensUpper, p1$sensLower), tolerance = 1e-12)
    expect_setequal(names(res$pairedComparisonTable$notes), c("method", "nested"))
    # With two tests every row is nested: none of the paired-test references applies.
    expect_length(res$pairedComparisonTable$getRefs(), 0L)
})

test_that("RF-05c Majority is a paired contrast: Newcombe interval and McNemar mid-p", {
    d <- dc_three()
    res <- dc_run(d, 3, showPairedComparison = TRUE)
    tab <- res$pairedComparisonTable$asDF
    expect_equal(nrow(tab), 21L)
    expect_true("majority" %in% names(res$pairedComparisonTable$notes))
    expect_setequal(res$pairedComparisonTable$getRefs(),
                    c("newcombe1998paired", "mcnemar1947", "Fagerland2013"))
    D <- d$gold == "D"
    P <- cbind(d$t1 == "pos", d$t2 == "pos", d$t3 == "pos")
    majority <- rowSums(P) >= 2
    for (i in 1:3) {
        rule <- majority[D]
        test <- P[D, i]
        g <- sum(rule & !test)
        l <- sum(!rule & test)
        m <- tab[tab$rule == "Majority (>=2/3 pos)" & tab$comparator == paste("Test", i, "alone"), ]
        mid_p <- 2 * (stats::pbinom(min(g, l), g + l, 0.5) - 0.5 * stats::dbinom(min(g, l), g + l, 0.5))
        expect_equal(c(m$sensGained, m$sensLost), c(g, l))
        expect_equal(m$sensP, min(1, mid_p), tolerance = 1e-12)
        # Newcombe method 10 by hand (Newcombe 1998), independent of the shared helper.
        n <- sum(D)
        a <- sum(rule & test); b <- g; cc <- l; dd <- n - a - b - cc
        w1 <- wilson(a + b, n); w2 <- wilson(a + cc, n)
        p1 <- (a + b) / n; p2 <- (a + cc) / n
        A <- a * dd - b * cc
        phi <- (if (A > 0) max(A - n / 2, 0) else A) / sqrt((a + b) * (cc + dd) * (a + cc) * (b + dd))
        lower <- (b - cc) / n - sqrt((p1 - w1[1])^2 - 2 * phi * (p1 - w1[1]) * (w2[2] - p2) + (w2[2] - p2)^2)
        upper <- (b - cc) / n + sqrt((w1[2] - p1)^2 - 2 * phi * (w1[2] - p1) * (p2 - w2[1]) + (p2 - w2[1])^2)
        expect_equal(c(m$sensLower, m$sensUpper), c(lower, upper), tolerance = 1e-10)
    }
})

test_that("RF-05d an empty reference group gives blanks, not 'no difference'", {
    d <- dc_three()
    d$gold <- "D"
    tab <- dc_run(d, showPairedComparison = TRUE)$pairedComparisonTable$asDF
    expect_true(all(is.na(c(tab$specGained, tab$specChange, tab$specLower, tab$specP))))
    expect_true(all(!is.na(tab$sensChange)))
})

test_that("RF-05e the shared Newcombe helper no longer overflows at a few hundred cases", {
    # sum() of a logical is an integer; the four-margin product exceeded the integer range
    # and `if (NA)` stopped decisioncompare whenever its paired differences were on.
    helper <- getFromNamespace("pairedProportionDifference", "ClinicoPath")
    x <- rep(c(1, 1, 0, 0), c(200, 100, 0, 200))
    y <- rep(c(1, 0, 1, 0), c(200, 100, 0, 200))
    out <- helper(x, y)
    expect_equal(out$diff, 100 / 500)
    expect_true(all(is.finite(c(out$lower, out$upper))))
    expect_identical(out$counts, c(both_pos = 200L, x_only = 100L, y_only = 0L, both_neg = 200L))
    expect_type(out$n, "integer")
})

test_that("RF-05f decisioncompare's paired differences run on 2,000 cases", {
    set.seed(4)
    n <- 2000
    disease <- stats::runif(n) < 0.35
    test <- function(se, sp) ifelse(disease, ifelse(stats::runif(n) < se, "pos", "neg"),
                                    ifelse(stats::runif(n) < 1 - sp, "pos", "neg"))
    d <- data.frame(gold = ifelse(disease, "dis", "non"), a = test(0.85, 0.8),
                    b = test(0.75, 0.9), c = test(0.9, 0.7))
    res <- ClinicoPath::decisioncompare(
        data = d, gold = "gold", goldPositive = "dis", goldNegative = "non",
        test1 = "a", test1Positive = "pos", test1Negative = "neg",
        test2 = "b", test2Positive = "pos", test2Negative = "neg",
        test3 = "c", test3Positive = "pos", test3Negative = "neg", statComp = TRUE)
    expect_equal(res$diffTable$rowCount, 9L)
    expect_true(all(is.finite(res$diffTable$asDF$lower)))
})

# An independent patient-level bootstrap of the whole selection: rules rebuilt from the test
# columns, resampled within each reference group, Harrell's plain estimator.
patient_optimism <- function(d, boots, seed) {
    D <- d$gold == "D"
    P <- cbind(d$t1 == "pos", d$t2 == "pos", d$t3 == "pos")
    rules <- list(P[, 1], P[, 2], P[, 3])
    for (pr in list(c(1, 2), c(1, 3), c(2, 3))) {
        rules <- c(rules, list(P[, pr[1]] & P[, pr[2]], P[, pr[1]] | P[, pr[2]]))
    }
    rules <- c(rules, list(rowSums(P) >= 1, rowSums(P) == 3, rowSums(P) >= 2))
    grid <- as.matrix(expand.grid(rep(list(c(TRUE, FALSE)), 3))[, 3:1])
    for (i in 2:8) {   # every pattern except the all-positive twin of Serial
        rules <- c(rules, list(apply(P, 1, function(r) all(r == grid[i, ]))))
    }
    flags <- do.call(cbind, rules)
    youden <- function(idx) colMeans(flags[idx[D[idx]], , drop = FALSE]) -
        colMeans(flags[idx[!D[idx]], , drop = FALSE])
    j_orig <- youden(seq_along(D))
    set.seed(seed)
    pos <- which(D)
    neg <- which(!D)
    mean(replicate(boots, {
        idx <- c(sample(pos, length(pos), TRUE), sample(neg, length(neg), TRUE))
        j <- youden(idx)
        pick <- which(j >= max(j) - 1e-9)[1]
        j[pick] - j_orig[pick]
    }))
}

test_that("RF-05g the optimism correction matches a patient-level bootstrap of the whole selection", {
    d <- dc_three(n = 240)
    seed_before <- .Random.seed
    res <- dc_run(d, 3, showRecommendation = TRUE, optimismCorrection = TRUE, nBoot = 4000, seed = 42)
    expect_identical(.Random.seed, seed_before)          # the engine's RNG is untouched
    row <- res$recommendationTable$asDF
    expect_gte(row$optimism, 0)
    expect_equal(row$youdenCorrected, row$youden - row$optimism, tolerance = 1e-12)
    # One rule dominates here, so the true optimism is tiny: only a bound is informative.
    reference <- patient_optimism(d, boots = 4000, seed = 1)
    expect_lt(abs(row$optimism - reference), 0.005)
    expect_lt(row$optimism, 0.005)
    # Same seed, same numbers; the seed and the method are shown only when it ran.
    again <- dc_run(d, 3, showRecommendation = TRUE, optimismCorrection = TRUE, nBoot = 4000, seed = 42)
    expect_identical(again$recommendationTable$asDF, res$recommendationTable$asDF)
    expect_match(res$recommendationTable$notes$seed$note, "Random seed: 42", fixed = TRUE)
    expect_true("harrell2015" %in% res$recommendationTable$getRefs())
    off <- dc_run(d, 3, showRecommendation = TRUE)
    # Off: the two columns are hidden (asDF leaves out hidden columns) and nothing is cited.
    expect_false(any(c("youdenCorrected", "optimism") %in% names(off$recommendationTable$asDF)))
    expect_null(off$recommendationTable$notes$seed)
    expect_length(off$recommendationTable$getRefs(), 0L)
})

test_that("RF-05i near-tied rules: the optimism matches the exact bootstrap value", {
    # 10 + 10 cases, best rule Test 1 alone (J = 0.400). Enumerating all 286^2 stratified bootstrap
    # outcomes gives an exact optimism of 0.121475 and stability 0.654 (verification workflow,
    # 2026-10-01). With 10,000 resamples the Monte Carlo standard error is about 0.002.
    cD <- c("+/+" = 4, "+/-" = 3, "-/+" = 1, "-/-" = 2)
    cN <- c("+/+" = 1, "+/-" = 2, "-/+" = 2, "-/-" = 5)
    res <- dc_run(dc_counts(cD, cN), showRecommendation = TRUE, optimismCorrection = TRUE,
                  nBoot = 10000, seed = 42)
    row <- res$recommendationTable$asDF
    expect_identical(row$pattern, "Test 1 alone")
    expect_equal(row$youden, 0.4, tolerance = 1e-12)
    expect_lt(abs(row$optimism - 0.121475), 0.008)
    picked <- as.numeric(sub(".*picked again in ([0-9.]+)%.*", "\\1",
                             res$recommendationTable$notes$optimism$note))
    expect_lt(abs(picked - 65.4), 1.5)
})

test_that("RF-05h a corrected J at or below 0 is said in words", {
    cD <- c("+/+" = 4, "+/-" = 3, "-/+" = 1, "-/-" = 4)
    cN <- c("+/+" = 2, "+/-" = 4, "-/+" = 4, "-/-" = 2)
    row <- dc_run(dc_counts(cD, cN), showRecommendation = TRUE,
                  optimismCorrection = TRUE)$recommendationTable$asDF
    expect_lte(round(row$youdenCorrected, 3), 0)
    expect_match(row$rationale, "Once the choice among", fixed = TRUE)
    expect_match(row$rationale, "there is no evidence that the highest-ranked rule", fixed = TRUE)
})
