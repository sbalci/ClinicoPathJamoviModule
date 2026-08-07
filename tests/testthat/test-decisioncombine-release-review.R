# Regression tests from the `decisioncombine` release review.
#
# Every case corresponds to a defect confirmed in the shipped code, checked against an
# independent reference (epiR, binom, or a hand-derived identity) rather than against the
# module's own arithmetic.

skip_if_not_installed("epiR")

dcomb_fixture <- function(seed = 20260807, n = 400) {
    set.seed(seed)
    gold <- sample(c("pos", "neg"), n, TRUE, c(0.35, 0.65))
    mk <- function(se, sp) ifelse(gold == "pos",
                                  sample(c("pos", "neg"), n, TRUE, c(se, 1 - se)),
                                  sample(c("neg", "pos"), n, TRUE, c(sp, 1 - sp)))
    data.frame(gold = gold, t1 = mk(.80, .85), t2 = mk(.70, .92), t3 = mk(.90, .60),
               stringsAsFactors = FALSE)
}

run_dcomb <- function(dat, ...) {
    do.call(ClinicoPath::decisioncombine, utils::modifyList(
        list(data = dat, gold = "gold", goldPositive = "pos",
             test1 = "t1", test1Positive = "pos",
             test2 = "t2", test2Positive = "pos",
             test3 = "t3", test3Positive = "pos"), list(...)))
}

notices_of <- function(res)
    gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " ")))

# reference 2x2 for an arbitrary "call this positive" rule
ref_2x2 <- function(flag, gold) {
    TP <- sum(flag & gold == "pos"); FP <- sum(flag & gold == "neg")
    FN <- sum(!flag & gold == "pos"); TN <- sum(!flag & gold == "neg")
    e <- as.data.frame(epiR::epi.tests(
        as.table(matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE)))$detail)
    g <- function(s) e$est[e$statistic == s]
    list(TP = TP, FP = FP, FN = FN, TN = TN,
         sens = g("se"), spec = g("sp"), ppv = g("pv.pos"), npv = g("pv.neg"),
         acc = g("diag.ac"))
}


test_that("pattern and strategy statistics match epiR::epi.tests", {
    d <- dcomb_fixture()
    ct <- run_dcomb(d)$combinationTable$asDF
    row_of <- function(p) ct[ct$pattern == p, ][1, ]

    p1 <- d$t1 == "pos"; p2 <- d$t2 == "pos"; p3 <- d$t3 == "pos"

    for (case in list(list("+/+/+", p1 & p2 & p3),
                      list("Parallel (>=1 pos)", p1 | p2 | p3),
                      list("Majority (>=2/3 pos)",
                           (as.integer(p1) + as.integer(p2) + as.integer(p3)) >= 2))) {
        m <- row_of(case[[1]]); r <- ref_2x2(case[[2]], d$gold)
        expect_equal(m$tp, r$TP); expect_equal(m$fp, r$FP)
        expect_equal(m$fn, r$FN); expect_equal(m$tn, r$TN)
        expect_equal(m$sens, r$sens, tolerance = 1e-8)
        expect_equal(m$spec, r$spec, tolerance = 1e-8)
        expect_equal(m$ppv,  r$ppv,  tolerance = 1e-8)
        expect_equal(m$npv,  r$npv,  tolerance = 1e-8)
        expect_equal(m$acc,  r$acc,  tolerance = 1e-8)
        expect_equal(m$youden, m$sens + m$spec - 1, tolerance = 1e-10)
    }
})


test_that("the OR/AND strategy identities hold", {
    # These are mathematical facts about the rules, not estimates: any violation is a bug.
    d <- dcomb_fixture()
    ct <- run_dcomb(d)$combinationTable$asDF
    row_of <- function(p) ct[ct$pattern == p, ][1, ]
    p1 <- d$t1 == "pos"; p2 <- d$t2 == "pos"; p3 <- d$t3 == "pos"

    ind <- sapply(list(p1, p2, p3), function(f) {
        r <- ref_2x2(f, d$gold); c(sens = r$sens, spec = r$spec)
    })
    par <- row_of("Parallel (>=1 pos)")
    ser <- row_of("+/+/+")                    # Serial (AND) is the all-positive pattern
    maj <- row_of("Majority (>=2/3 pos)")

    expect_gte(par$sens, max(ind["sens", ]) - 1e-12)
    expect_lte(par$spec, min(ind["spec", ]) + 1e-12)
    expect_lte(ser$sens, min(ind["sens", ]) + 1e-12)
    expect_gte(ser$spec, max(ind["spec", ]) - 1e-12)
    expect_lte(ser$sens, maj$sens + 1e-12)
    expect_lte(maj$sens, par$sens + 1e-12)

    # the 8 exhaustive patterns must partition the sample exactly once
    pats <- c("+/+/+","+/+/-","+/-/+","+/-/-","-/+/+","-/+/-","-/-/+","-/-/-")
    expect_true(all(pats %in% ct$pattern))
    expect_equal(sum(vapply(pats, function(p) { r <- row_of(p); r$tp + r$fp }, numeric(1))),
                 nrow(d))
})


test_that("Wilson intervals match binom::binom.confint at the boundaries", {
    skip_if_not_installed("binom")
    d <- dcomb_fixture()
    prv <- ClinicoPath:::decisioncombineClass$new(
        options = ClinicoPath:::decisioncombineOptions$new(
            gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
            test2 = "t2", test2Positive = "pos", test3 = "t3", test3Positive = "pos"),
        data = d)$.__enclos_env__$private

    for (xy in list(c(7, 20), c(0, 15), c(15, 15), c(1, 3), c(340, 400))) {
        got <- prv$.calcWilsonCI(xy[1], xy[2])
        ref <- binom::binom.confint(xy[1], xy[2], methods = "wilson")
        expect_equal(got[1], ref$lower, tolerance = 1e-9)
        expect_equal(got[2], ref$upper, tolerance = 1e-9)
    }
    expect_true(all(is.na(prv$.calcWilsonCI(0, 0))))
})


test_that("the recommendation and all four plots run", {
    # `asDF` is an R6 ACTIVE BINDING on jmvcore::Table, so `tbl$asDF` already returns the
    # data.frame. The code called `tbl$asDF()`, invoking that data.frame as a function ->
    # "attempt to apply non-function". Five call sites: the recommendation and every plot,
    # i.e. five of the seven optional outputs were completely non-functional.
    d <- dcomb_fixture()
    expect_no_error(res <- run_dcomb(d, showRecommendation = TRUE))
    expect_equal(res$recommendationTable$rowCount, 1L)

    opts <- ClinicoPath:::decisioncombineOptions$new(
        gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos", test3 = "t3", test3Positive = "pos",
        showBarPlot = TRUE, showHeatmap = TRUE, showForest = TRUE, showDecisionTree = TRUE)
    analysis <- ClinicoPath:::decisioncombineClass$new(options = opts, data = d)
    analysis$run()
    prv <- analysis$.__enclos_env__$private

    pdf(NULL); on.exit(dev.off(), add = TRUE)
    for (nm in c("plotBarChart", "plotHeatmap", "plotForest", "plotDecisionTree")) {
        img <- switch(nm,
            plotBarChart = analysis$results$barPlot,
            plotHeatmap  = analysis$results$heatmapPlot,
            plotForest   = analysis$results$forestPlot,
            plotDecisionTree = analysis$results$decisionTreePlot)
        expect_true(isTRUE(suppressWarnings(
            prv[[paste0(".", nm)]](img, ggtheme = ggplot2::theme_minimal()))),
            info = nm)
    }
})


test_that("tables do not accumulate rows across re-runs", {
    # Nothing in the file called deleteRows(), and jamovi re-runs .run() on the SAME object
    # whenever an option changes. Rows doubled and tripled (5 -> 10 -> 15), and the
    # duplicated rowKeys then made $asDF fail outright with
    # "duplicate 'row.names' are not allowed", taking down run 2 entirely.
    d <- dcomb_fixture()
    opts <- ClinicoPath:::decisioncombineOptions$new(
        gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos", test3 = "t3", test3Positive = "pos",
        showRecommendation = TRUE, showFrequency = TRUE)
    analysis <- ClinicoPath:::decisioncombineClass$new(options = opts, data = d)

    snap <- function() c(analysis$results$combinationTable$rowCount,
                         analysis$results$combinationTableCI$rowCount,
                         analysis$results$goldFreqTable$rowCount,
                         analysis$results$crossTabTable$rowCount)
    expect_no_error(analysis$run()); first <- snap()
    expect_no_error(analysis$run()); second <- snap()
    expect_no_error(analysis$run()); third <- snap()

    expect_true(all(first > 0))
    expect_equal(second, first)
    expect_equal(third, first)
})


test_that("cases dropped for missing data are disclosed", {
    d <- dcomb_fixture()
    d$gold[1:37] <- NA
    res <- run_dcomb(d)

    expect_match(notices_of(res), "Removed 37 case\\(s\\) with missing values")
    expect_match(notices_of(res), "Complete-case analysis uses 363 of 400")
    ct <- res$combinationTable$asDF
    expect_equal(ct$tp[1] + ct$fp[1] + ct$fn[1] + ct$tn[1], 363)
})


test_that("notices survive every early return in .run()", {
    # .renderNotices() sat after three early returns, so the notice explaining WHY the
    # analysis stopped was collected and then thrown away: the user saw a blank analysis
    # with no message at all.
    d <- dcomb_fixture()
    res <- run_dcomb(d, goldPositive = "NotALevel")
    expect_match(notices_of(res), "Missing Level")
    expect_match(notices_of(res), "NotALevel")
    expect_equal(res$combinationTable$rowCount, 0L)

    d2 <- dcomb_fixture(); d2$gold <- NA
    res2 <- run_dcomb(d2)
    expect_match(notices_of(res2), "No Complete Cases")
})


test_that("the recommendation discloses that it is an uncorrected argmax", {
    # It ranks 5 (2 tests) or 10 (3 tests) candidate rules with no interval and no test,
    # so on data with no signal it still names a winner and called it "optimal".
    set.seed(99); n <- 300
    noise <- data.frame(gold = sample(c("pos", "neg"), n, TRUE),
                        t1 = sample(c("pos", "neg"), n, TRUE),
                        t2 = sample(c("pos", "neg"), n, TRUE),
                        stringsAsFactors = FALSE)
    rec <- ClinicoPath::decisioncombine(
        data = noise, gold = "gold", goldPositive = "pos",
        test1 = "t1", test1Positive = "pos", test2 = "t2", test2Positive = "pos",
        test3 = NULL, test3Positive = NULL,
        showRecommendation = TRUE)$recommendationTable$asDF

    rationale <- as.character(rec$rationale[1])
    expect_match(rationale, "candidate rule")
    expect_match(rationale, "no significance test or multiplicity correction")
    # on pure noise the winner must not be presented as established
    expect_match(rationale, "advantage is not established")
    expect_false(grepl("Excellent discriminatory performance", rationale, fixed = TRUE))
})


test_that("a multi-level variable is flagged rather than silently dichotomised", {
    d <- dcomb_fixture()
    set.seed(2)
    d$t1[sample(nrow(d), 60)] <- "equivocal"
    res <- run_dcomb(d)

    expect_match(notices_of(res), "has 3 levels")
    expect_match(notices_of(res), "counted as NEGATIVE")
    expect_match(notices_of(res), "inflates specificity and NPV")
})


test_that("the pattern filter selects correctly and does not fall back to unfiltered", {
    d <- dcomb_fixture()
    prv <- ClinicoPath:::decisioncombineClass$new(
        options = ClinicoPath:::decisioncombineOptions$new(
            gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
            test2 = "t2", test2Positive = "pos", test3 = "t3", test3Positive = "pos"),
        data = d)$.__enclos_env__$private

    df <- data.frame(pattern = c("+/+/+","+/+/-","+/-/+","+/-/-",
                                 "-/+/+","-/+/-","-/-/+","-/-/-",
                                 "Parallel (>=1 pos)","Majority (>=2/3 pos)"),
                     stringsAsFactors = FALSE)

    expect_equal(prv$.applyPatternFilter(df, "allPositive")$pattern, "+/+/+")
    expect_equal(prv$.applyPatternFilter(df, "allNegative")$pattern, "-/-/-")
    # "mixed" used to exclude anything STARTING with "+/+" or "-/-", dropping the
    # genuinely mixed "+/+/-" and "-/-/+"
    expect_setequal(prv$.applyPatternFilter(df, "mixed")$pattern,
                    c("+/+/-","+/-/+","+/-/-","-/+/+","-/+/-","-/-/+"))
    expect_equal(nrow(prv$.applyPatternFilter(df, "all")), nrow(df))

    # no match must yield nothing, not the whole unfiltered table
    two_test <- data.frame(pattern = c("+/+", "+/-"), stringsAsFactors = FALSE)
    expect_equal(nrow(prv$.applyPatternFilter(two_test, "allNegative")), 0L)
})


test_that("a gold standard with one outcome is flagged and yields NA, not a silent number", {
    d <- dcomb_fixture()
    d$gold <- "pos"
    res <- run_dcomb(d)

    expect_match(notices_of(res), "Gold Standard Has Only One Outcome")
    expect_match(notices_of(res), "no disease-absent cases")
    expect_true(all(is.na(res$combinationTable$asDF$spec)))
})


# ── follow-up: the three limitations closed after the first review pass ──────────────

test_that("Serial (AND) has its own named row", {
    # Serial was numerically identical to the all-positive pattern and so was omitted
    # entirely -- a reader had to know that "+/+/+" WAS the serial rule to find it.
    d <- dcomb_fixture()
    ct <- run_dcomb(d)$combinationTable$asDF

    expect_true("Serial (all pos)" %in% ct$pattern)
    serial <- ct[ct$pattern == "Serial (all pos)", ]
    allpos <- ct[ct$pattern == "+/+/+", ]

    # it must be the same rule, not a differently-computed one
    expect_equal(c(serial$tp, serial$fp, serial$fn, serial$tn),
                 c(allpos$tp, allpos$fp, allpos$fn, allpos$tn))
    expect_equal(serial$sens, allpos$sens, tolerance = 1e-12)
    expect_equal(serial$spec, allpos$spec, tolerance = 1e-12)

    # ...and the two-test case names it too
    d2 <- d[, c("gold", "t1", "t2")]
    ct2 <- do.call(ClinicoPath::decisioncombine, list(
        data = d2, gold = "gold", goldPositive = "pos",
        test1 = "t1", test1Positive = "pos", test2 = "t2", test2Positive = "pos",
        test3 = NULL, test3Positive = NULL))$combinationTable$asDF
    expect_true("Serial (all pos)" %in% ct2$pattern)
})


test_that("the recommendation does not double-count the Serial/all-positive twin", {
    # They are the same 2x2 under two labels; counting both would manufacture a tie and
    # inflate the "n candidate rules" disclosure.
    d <- dcomb_fixture()
    rec <- run_dcomb(d, showRecommendation = TRUE)$recommendationTable$asDF
    rationale <- as.character(rec$rationale[1])

    n_claimed <- as.integer(sub(".*ranking ([0-9]+) candidate rule.*", "\\1", rationale))
    ct <- run_dcomb(d)$combinationTable$asDF
    stable <- ct[pmin(ct$tp, ct$fp, ct$fn, ct$tn) >= 5, ]
    n_distinct <- length(unique(paste(stable$tp, stable$fp, stable$fn, stable$tn)))

    expect_equal(n_claimed, n_distinct)
    # the twin must not surface as a tie
    expect_false(grepl("Serial \\(all pos\\), \\+/\\+", rationale))
})


test_that("proportions and ratios are reported in separate tables", {
    # One `estimate` column cannot carry both a 0-1 proportion and an unbounded odds
    # ratio; combinationTable showed sensitivity as a percentage while the CI table
    # showed the same quantity as 0.813.
    d <- dcomb_fixture()
    res <- run_dcomb(d)

    props <- res$combinationTableCI$asDF
    expect_setequal(unique(as.character(props$statistic)),
                    c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy"))
    expect_true(all(props$estimate >= 0 & props$estimate <= 1, na.rm = TRUE))

    ratios <- tryCatch(res$combinationTableCIRatios$asDF, error = function(e) NULL)
    skip_if(is.null(ratios), "combinationTableCIRatios not compiled yet - run jmvtools::prepare()")
    expect_setequal(unique(as.character(ratios$statistic)), c("LR+", "LR-", "DOR"))
    expect_true(any(ratios$estimate > 1, na.rm = TRUE))   # genuinely unbounded
})


test_that("the forest plot renders with both tables and free x-scales", {
    d <- dcomb_fixture()
    opts <- ClinicoPath:::decisioncombineOptions$new(
        gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos", test3 = "t3", test3Positive = "pos",
        showForest = TRUE)
    analysis <- ClinicoPath:::decisioncombineClass$new(options = opts, data = d)
    analysis$run()

    pdf(NULL); on.exit(dev.off(), add = TRUE)
    expect_true(isTRUE(suppressWarnings(
        analysis$.__enclos_env__$private$.plotForest(
            analysis$results$forestPlot, ggtheme = ggplot2::theme_minimal()))))
})


test_that("no result-item description spans multiple lines", {
    # A .r.yaml result-item description: is emitted verbatim into a roxygen \tabular{}
    # cell in the generated .h.R. A folded scalar (>) leaves a newline there, which
    # orphans the \cr and makes the .h.R -- and therefore the whole package -- unparseable
    # after jmvtools::prepare().
    skip_if_not_installed("yaml")
    spec <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.r.yaml"))

    collect <- function(items) {
        out <- character(0)
        for (it in items %||% list()) {
            if (!is.null(it$description) && is.character(it$description) &&
                grepl("\n", trimws(it$description))) out <- c(out, it$name)
            out <- c(out, collect(it$items))
        }
        out
    }
    `%||%` <- function(a, b) if (is.null(a)) b else a
    expect_equal(collect(spec$items), character(0))
})
