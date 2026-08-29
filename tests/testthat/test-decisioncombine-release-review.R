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


test_that("sparse cells are warned about without disqualifying the rule", {
    # BEHAVIOUR CHANGE (release review): sparsity and ranking eligibility were coupled --
    # min(tp,fp,fn,tn) >= 5 gated the Youden ranking, which excluded rules for being
    # highly sensitive or highly specific. Sparsity now warns about the RATIO columns
    # only; eligibility is gated on the two reference-group sizes. Here both groups are
    # exactly 10, so a perfectly concordant rule is ranked rather than refused, and the
    # tiny sample is covered by the sample-size ladder instead.
    d <- data.frame(
        gold = rep(c("pos", "neg"), each = 10),
        t1 = rep(c("pos", "neg"), each = 10),
        t2 = rep(c("pos", "neg"), each = 10),
        t3 = rep(c("pos", "neg"), each = 10),
        stringsAsFactors = FALSE
    )
    res <- run_dcomb(d, showRecommendation = TRUE)
    notices <- notices_of(res)

    expect_match(notices, "Sparse Cell Counts")
    expect_match(notices, "cell count below 5")
    # the warning must NOT claim the row is dropped from the ranking any more
    expect_false(grepl("excluded from the candidate-rule ranking", notices, fixed = TRUE))
    # a small sample is disclosed on its own terms (n = 20 sits on the second rung:
    # the ladder's first rung is n < 20, so this is "Small sample", not "Very small")
    expect_match(notices, "Small sample")

    rec <- as.data.frame(res$recommendationTable$asDF)
    expect_false(is.na(rec$pattern[1]))
    expect_equal(rec$youden[1], 1, tolerance = 1e-9)
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


test_that("invalid positive levels stop before individual statistics are populated", {
    d <- dcomb_fixture()
    res <- run_dcomb(
        d,
        test2Positive = "NotALevel",
        showIndividual = TRUE
    )

    expect_match(notices_of(res), "Missing Level")
    expect_match(notices_of(res), "NotALevel")
    expect_equal(res$combinationTable$rowCount, 0L)
    expect_true(all(is.na(res$individualTest2$test2Stats$asDF$estimate)))
})


test_that("fewer than four joint complete cases stop the analysis", {
    d <- data.frame(
        gold = c("pos", "neg", "pos", "neg", "pos", "neg"),
        t1 = c("pos", "neg", "pos", "neg", "pos", "neg"),
        t2 = c("pos", "neg", "pos", NA, NA, NA),
        stringsAsFactors = FALSE
    )
    res <- ClinicoPath::decisioncombine(
        data = d,
        gold = "gold",
        goldPositive = "pos",
        test1 = "t1",
        test1Positive = "pos",
        test2 = "t2",
        test2Positive = "pos",
        test3 = NULL,
        test3Positive = NULL,
        showIndividual = TRUE
    )

    expect_match(notices_of(res), "Insufficient Complete Cases")
    expect_match(notices_of(res), "only 3 of 6 cases remain")
    expect_equal(res$combinationTable$rowCount, 0L)
    expect_true(all(is.na(res$individualTest1$test1Stats$asDF$estimate)))
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
    expect_match(notices_of(res), "can bias sensitivity, specificity")
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

    n_claimed <- as.integer(sub(
        ".*ranking of ([0-9]+) candidate rule.*",
        "\\1",
        rationale
    ))
    ct <- run_dcomb(d)$combinationTable$asDF
    # Eligibility is: at least 10 disease-present (tp+fn) and 10 disease-absent (fp+tn)
    # cases, an estimable Youden's J, and J > 0. The gate is on REFERENCE-GROUP size, not
    # on the smallest cell -- min(tp,fp,fn,tn) >= 5 excluded rules for being highly
    # sensitive or highly specific. Recomputed here from the published table so this stays
    # an independent check of the disclosed count, not a restatement of it.
    stable <- ct[(ct$tp + ct$fn) >= 10 & (ct$fp + ct$tn) >= 10 &
                     is.finite(ct$youden) & ct$youden > 0, ]
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


test_that("optional positive-level options have explicit R defaults", {
    skip_if_not_installed("yaml")
    spec <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.a.yaml")
    )
    options <- stats::setNames(spec$options, vapply(spec$options, `[[`, "", "name"))

    expect_null(options$test2Positive$default)
    expect_null(options$test3Positive$default)
    expect_true(isTRUE(options$test2Positive$allowNone))
    expect_true(isTRUE(options$test3Positive$allowNone))
})


# ---------------------------------------------------------------------------
# Regressions from the 2026-08-28 deep audit (/check-function-full).
# ---------------------------------------------------------------------------

# run_dcomb() routes through modifyList(), and modifyList(x, list(a = NULL)) DELETES `a`
# rather than setting it to NULL. Every positive-level option is `type: Level`, which the
# jamovi compiler forbids a default on, so the generated wrapper makes all four REQUIRED
# arguments -- dropping them raises "argument \"test3Positive\" is missing, with no
# default" before any module code runs. Two-test cases therefore call the wrapper directly.
run_dcomb2 <- function(dat, ...) {
    do.call(ClinicoPath::decisioncombine, utils::modifyList(
        list(data = dat, gold = "gold", goldPositive = "pos",
             test1 = "t1", test1Positive = "pos",
             test2 = "t2", test2Positive = "pos",
             test3 = NULL, test3Positive = NULL), list(...)))
}

test_that("no rendered notice contains an unsubstituted {placeholder}", {
    # jmvcore::format()'s placeholder regex does not match underscores, so a name like
    # {runner_up} shipped to the user as literal braces with no warning at all. This
    # guards the whole class, not just the one occurrence that was found.
    res <- run_dcomb(dcomb_fixture(), showIndividual = TRUE, showFrequency = TRUE,
                     showRecommendation = TRUE)

    html <- res$notices$content
    expect_false(grepl("\\{[A-Za-z_][A-Za-z0-9_]*\\}", html))

    rationale <- as.data.frame(res$recommendationTable$asDF)$rationale
    expect_false(any(grepl("\\{[A-Za-z_][A-Za-z0-9_]*\\}", stats::na.omit(rationale))))
})


test_that("jmvcore::format ignores underscored placeholders", {
    # The property the test above defends against, pinned so the reason stays visible.
    expect_identical(
        jmvcore::format("a {ok} b {not_ok}", ok = "1", not_ok = "2"),
        "a 1 b {not_ok}"
    )
})


test_that("notices render most-severe first, regardless of emission order", {
    # A STRONG_WARNING emitted after four INFO notices used to be rendered below them,
    # so the reason to distrust the numbers sat under the reassuring notes.
    dat <- dcomb_fixture()
    dat$gold[dat$gold == "pos"][1:130] <- "neg"   # push prevalence low -> STRONG_WARNING
    res <- run_dcomb(dat, showIndividual = TRUE, showRecommendation = TRUE)

    html <- res$notices$content
    colours <- regmatches(html, gregexpr("rgba\\([0-9]+, [0-9]+, [0-9]+", html))[[1]]
    known <- c("rgba(220, 38, 38", "rgba(234, 88, 12",
               "rgba(202, 138, 4", "rgba(37, 99, 235")
    rank <- match(colours, known)

    # An unrecognised colour would make rank NA, and expect_false(NA) ERRORS rather than
    # failing informatively -- assert the mapping is total before relying on it.
    expect_false(anyNA(rank), info = paste("unmapped notice colour:",
                                           paste(setdiff(colours, known), collapse = ", ")))
    # Guard against the test going vacuous: it only proves anything if the run actually
    # produced a notice more severe than INFO for the sort to have lifted.
    expect_true(any(rank < 4L))
    expect_false(is.unsorted(rank))
})


test_that("only one continuity-correction notice is emitted, listing every pattern", {
    set.seed(11)
    n <- 60
    gold <- rep(c("pos", "neg"), each = n / 2)
    dat <- data.frame(gold = gold,
                      t1 = ifelse(gold == "pos", "pos", "neg"),
                      t2 = ifelse(gold == "pos", "pos", "neg"),
                      t3 = c(rep("pos", 5), rep("neg", n - 5)),
                      stringsAsFactors = FALSE)
    res <- run_dcomb(dat)

    hits <- gregexpr("Continuity Correction", res$notices$content)[[1]]
    expect_equal(sum(hits > 0), 1L)
})


test_that("a rule that cannot beat chance is never ranked", {
    # An argmax over Youden's J always names a winner. Reaching the guard takes exact
    # construction: the exhaustive pattern rows partition the diseased and non-diseased
    # cases separately, so their Youden values sum to EXACTLY zero -- some pattern has
    # J > 0 unless every one is precisely 0. And since the eligibility gate is on the two
    # reference-group sizes, which are identical for every row, no row can be excluded
    # individually. So the only way in is a sample where the tests carry no information
    # at all: the same joint pattern distribution in both arms, giving J = 0 everywhere.
    per_arm <- c("+/+" = 25, "+/-" = 25, "-/+" = 25, "-/-" = 25)
    mk_arm <- function(g) do.call(rbind, lapply(names(per_arm), function(pat) {
        toks <- strsplit(pat, "/", fixed = TRUE)[[1]]
        data.frame(gold = g,
                   t1 = ifelse(toks[1] == "+", "pos", "neg"),
                   t2 = ifelse(toks[2] == "+", "pos", "neg"),
                   stringsAsFactors = FALSE)[rep(1, per_arm[[pat]]), ]
    }))
    dat <- rbind(mk_arm("pos"), mk_arm("neg"))

    res <- run_dcomb2(dat, showRecommendation = TRUE)
    ct  <- as.data.frame(res$combinationTable$asDF)
    expect_true(all(abs(ct$youden) < 1e-12))          # no rule beats chance, by construction

    best <- as.data.frame(res$recommendationTable$asDF)$pattern[1]
    expect_true(is.na(best) || !nzchar(best))
    expect_match(notices_of(res), "better than chance", fixed = TRUE)

    # and the ordinary case still names a winner
    ok <- run_dcomb2(dcomb_fixture(), showRecommendation = TRUE)
    expect_true(nzchar(as.character(as.data.frame(
        ok$recommendationTable$asDF)$pattern[1])))
})


test_that("the named strategy wins over its identical exact-pattern twin", {
    # "Serial (all pos)" and "+/+" have the same 2x2 by construction. The pattern label
    # describes a group of patients; the strategy label names a rule you can apply.
    res <- run_dcomb2(dcomb_fixture(), showRecommendation = TRUE)
    tbl <- as.data.frame(res$combinationTable$asDF)

    serial <- tbl[grepl("Serial", tbl$pattern), c("tp", "fp", "fn", "tn")]
    allpos <- tbl[tbl$pattern == "+/+", c("tp", "fp", "fn", "tn")]
    expect_equal(unname(unlist(serial)), unname(unlist(allpos)))

    best <- as.data.frame(res$recommendationTable$asDF)$pattern[1]
    if (!is.na(best) && identical(unname(unlist(allpos)),
                                  unname(unlist(tbl[tbl$pattern == best,
                                                    c("tp", "fp", "fn", "tn")])))) {
        expect_false(identical(best, "+/+"))
    }
})


test_that("extreme prevalence raises a strong warning", {
    dat <- dcomb_fixture()
    dat$gold <- "neg"
    dat$gold[1:8] <- "pos"                      # 2% prevalence
    res <- run_dcomb2(dat)
    expect_match(res$notices$content, "Extreme Disease Prevalence", fixed = TRUE)

    balanced <- run_dcomb2(dcomb_fixture())
    expect_false(grepl("Extreme Disease Prevalence", balanced$notices$content, fixed = TRUE))
})


test_that("the forest plot honours the pattern filter it declares", {
    # forestPlot listed filterPattern in clearWith but never carried it into its state,
    # so choosing a pattern type cleared the plot and redrew an identical image.
    res <- run_dcomb(dcomb_fixture(), showForest = TRUE, filterPattern = "allPositive")
    st <- res$forestPlot$state
    expect_false(is.null(st$filterPattern))
    expect_identical(st$filterPattern, "allPositive")

    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(res$forestPlot$.render())
})


test_that("the decision-space plot does not declare filters it ignores", {
    skip_if_not_installed("yaml")
    spec <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.r.yaml"))
    items <- stats::setNames(spec$items, vapply(spec$items, `[[`, "", "name"))

    expect_false("filterPattern" %in% items$decisionTreePlot$clearWith)
    expect_false("filterStatistic" %in% items$decisionTreePlot$clearWith)
    expect_true("filterPattern" %in% items$forestPlot$clearWith)
})


test_that("the filter controls are disabled when no plot consumes them", {
    skip_if_not_installed("yaml")
    ui <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.u.yaml"))

    combos <- list()
    walk <- function(node) {
        for (ch in node$children %||% list()) {
            if (identical(ch$type, "ComboBox")) combos[[ch$name]] <<- ch
            walk(ch)
        }
    }
    `%||%` <- function(a, b) if (is.null(a)) b else a
    walk(ui)

    for (nm in c("filterStatistic", "filterPattern")) {
        expect_true(!is.null(combos[[nm]]$enable))
        # A jamovi UI expression must start with "(" + a letter; a leading "!" makes
        # jmvcore return the raw string, which is truthy, so the control is always on.
        expect_match(combos[[nm]]$enable, "^\\([A-Za-z]")
        expect_match(combos[[nm]]$enable, "showBarPlot", fixed = TRUE)
    }
})


test_that("an explicit-NA factor level is treated as missing, not as negative", {
    # addNA() / factor(exclude = NULL) makes NA a real LEVEL. Such values are NOT
    # is.na(), so they survive stats::complete.cases() AND jmvcore::naOmit() -- but
    # as.character() maps them back to NA. .prepareData's case_when() leads with an
    # is.na() branch that therefore does not match, and the row fell through to
    # TRUE ~ "Negative": a genuinely missing result was COUNTED AS NEGATIVE, for the
    # reference standard as well as for each test, with no dropped-case disclosure.
    d <- dcomb_fixture()
    truth <- run_dcomb2(d)$combinationTable$asDF

    # Blank 40 test-1 results, encoded the addNA way rather than as plain NA.
    holed <- d
    holed$t1[1:40] <- NA
    holed$t1 <- addNA(factor(holed$t1))
    expect_false(any(is.na(holed$t1)))          # the trap: not is.na()

    res <- run_dcomb2(holed)
    got <- res$combinationTable$asDF

    # Those 40 cases must be excluded, not silently recoded. Every row of the table --
    # pattern or strategy -- scores the same complete-case set, so each row's four cells
    # must sum to the reduced denominator, and the exclusion must be disclosed.
    expect_true(all(got$tp + got$fp + got$fn + got$tn == nrow(d) - 40))
    expect_match(notices_of(res), "Removed 40 case")

    # And a plain-NA encoding of the same holes must give identical results.
    plain <- d
    plain$t1[1:40] <- NA
    expect_equal(as.data.frame(run_dcomb2(plain)$combinationTable$asDF),
                 as.data.frame(got))

    # sanity: the holed run really did differ from the untouched one
    expect_false(isTRUE(all.equal(as.data.frame(truth), as.data.frame(got))))
})


test_that("an explicit-NA level does not blank an individual test's 2x2", {
    # Same root cause in .analyzeIndividualTest: complete.cases() kept the rows, the
    # ifelse() recode produced an all-NA column, and the 2x2 came back all zeros --
    # which the "All Zero Counts" guard then reported as "no valid observations".
    d <- dcomb_fixture()
    d$t3[1:30] <- NA
    d$t3 <- addNA(factor(d$t3))

    res <- run_dcomb(d, showIndividual = TRUE)
    cont <- as.data.frame(res$individualTest3$test3Contingency$asDF)

    expect_false(any(is.na(cont$total)))
    expect_gt(cont$total[cont$total == max(cont$total)][1], 0)
    expect_equal(max(cont$total), nrow(d) - 30)
    expect_false(grepl("All Zero Counts", notices_of(res), fixed = TRUE))
})


test_that("the ranking gate does not exclude a rule for being highly specific", {
    # Youden's J = sens + spec - 1, so its precision depends on the two reference-group
    # sizes, not on the smallest cell. A min(tp,fp,fn,tn) >= 5 gate inverted that: a rule
    # is sparse in fp precisely BECAUSE it is specific and sparse in fn precisely BECAUSE
    # it is sensitive, so the two best rules were dropped and a mediocre one was crowned.
    n <- 300
    gold <- rep(c("pos", "neg"), c(120, 180))
    # t1 & t2 both positive -> highly specific rule with only 3 false positives
    t1 <- c(rep("pos", 100), rep("neg", 20), rep("pos", 5), rep("neg", 175))
    t2 <- c(rep("pos", 78), rep("neg", 42), rep("pos", 3), rep("neg", 177))
    dat <- data.frame(gold = gold, t1 = t1, t2 = t2, stringsAsFactors = FALSE)

    res <- run_dcomb2(dat, showRecommendation = TRUE)
    ct  <- as.data.frame(res$combinationTable$asDF)
    rec <- as.data.frame(res$recommendationTable$asDF)

    best_overall <- ct$youden[which.max(ct$youden)]
    expect_true(is.finite(best_overall) && best_overall > 0)

    # the crowned rule must be the best one, not one that merely had >=5 in every cell
    expect_equal(rec$youden[1], best_overall, tolerance = 1e-9)

    # and a specific rule with a small fp cell must still be eligible
    sparse_but_good <- ct[pmin(ct$tp, ct$fp, ct$fn, ct$tn) < 5 &
                              (ct$tp + ct$fn) >= 10 & (ct$fp + ct$tn) >= 10 &
                              is.finite(ct$youden) & ct$youden > 0, ]
    if (nrow(sparse_but_good)) {
        expect_gte(rec$youden[1], max(sparse_but_good$youden) - 1e-9)
    }
})


test_that("a pattern no patient exhibits reports no likelihood ratio", {
    # tp = 0 and fp = 0 is routine when two tests agree closely. The Haldane correction
    # would add 0.5 to every cell and manufacture a finite LR+ and DOR for a row that
    # contains no patients at all.
    n <- 200
    gold <- rep(c("pos", "neg"), c(80, 120))
    t1 <- c(rep("pos", 60), rep("neg", 20), rep("pos", 10), rep("neg", 110))
    dat <- data.frame(gold = gold, t1 = t1, t2 = t1, stringsAsFactors = FALSE)  # identical
    res <- run_dcomb2(dat)
    ct  <- as.data.frame(res$combinationTable$asDF)

    empty <- ct[(ct$tp + ct$fp) == 0, , drop = FALSE]
    expect_gt(nrow(empty), 0)                       # "+/-" and "-/+" are empty here

    # With no predicted positives sens = 0 and spec = 1, so LR+ is 0/0 and the diagnostic
    # odds ratio is undefined -- but LR- is exactly (1-0)/1 = 1, which IS defined and
    # worth showing: it says a "not this pattern" result does not move the odds. Blanking
    # it too would be over-correction.
    expect_true(all(is.na(empty$lrPos)))
    expect_true(all(is.na(empty$dor)))
    expect_equal(unname(empty$lrNeg), rep(1, nrow(empty)), tolerance = 1e-12)
})


test_that("the heatmap does not paint Youden's J on a 0.5-centred scale", {
    # J runs -1..1 with chance at 0; the proportions run 0..1 with a midpoint of 0.5.
    # They cannot share one diverging scale, so J is not in the default panel and gets
    # midpoint 0 when selected on its own.
    src <- readLines(testthat::test_path("..", "..", "R", "decisioncombine.b.R"))
    panel <- grep('"balancedAccuracy"\\)$', src, value = TRUE)
    expect_true(length(panel) > 0)
    expect_false(any(grepl('"youden"', panel)))
    expect_true(any(grepl('identical\\(stat_filter, "youden"\\)\\) 0', src)))
})


test_that("the Output element's enabled state resolves to a real option", {
    # jmvcore::Output$enabled does options$get(<element name>), so an Output result item
    # requires an OPTION of the SAME NAME and of type Output. Driven by a differently
    # named Bool, the backend computed and stored 100 pattern values while `enabled`
    # stayed FALSE and jamovi never wrote the column -- invisible to any R-side test that
    # only checks isNotFilled(), which is exactly what the earlier tests did.
    skip_if_not_installed("yaml")
    a <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.a.yaml"))
    r <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.r.yaml"))
    opts <- stats::setNames(a$options, vapply(a$options, `[[`, "", "name"))
    outs <- Filter(function(i) identical(i$type, "Output"), r$items)

    for (o in outs) {
        expect_true(!is.null(opts[[o$name]]),
                    info = paste("Output item", o$name, "has no option of that name"))
        expect_identical(opts[[o$name]]$type, "Output",
                         info = paste("option", o$name, "must be type Output"))
    }

    # The schema pairing above is the enforceable half. `enabled` itself is driven by the
    # Output control in the jamovi GUI and CANNOT be set through the R wrapper -- a
    # `type: Output` option is not a wrapper argument (confirmed: it is absent from
    # formals(decisioncombine), exactly as categorize's `addtodata` is). So from R the
    # right assertion is that the values are computed and stored; jamovi decides whether
    # to materialise the column.
    expect_false("addedPattern" %in% names(formals(ClinicoPath::decisioncombine)))
    d <- dcomb_fixture()
    res <- run_dcomb2(d)
    expect_false(res$addedPattern$isNotFilled())
    expect_equal(length(as.character(res$addedPattern$.__enclos_env__$private$.values[[1]])),
                 nrow(d))
})


test_that("the cross-tabulation is hidden, not left empty, for a single test", {
    # A cross-tabulation of test PATTERNS needs two tests. With only test 1 selected the
    # populate method returned early and the user was shown a fully empty "Test Results
    # Cross-Tabulation": headers, zero rows, nothing saying why.
    d <- dcomb_fixture()
    one <- do.call(ClinicoPath::decisioncombine, list(
        data = d, gold = "gold", goldPositive = "pos",
        test1 = "t1", test1Positive = "pos",
        test2 = NULL, test2Positive = NULL, test3 = NULL, test3Positive = NULL,
        showFrequency = TRUE))
    expect_false(one$crossTabTable$visible)
    expect_gt(one$goldFreqTable$rowCount, 0L)          # the applicable one still shows

    two <- run_dcomb2(d, showFrequency = TRUE)
    expect_true(two$crossTabTable$visible)
    expect_gt(two$crossTabTable$rowCount, 0L)
})


test_that("the generated pattern order matches the hand-written lists it replaced", {
    # The 4- and 8-pattern condition lists were written out three times; they are now
    # generated once. Row order IS the table's row order and its rowKeys, so the generator
    # must reproduce the original sequence exactly: binary counting with "+" before "-"
    # and test 1 most significant, i.e. the last test varying fastest.
    d <- dcomb_fixture()
    two   <- run_dcomb2(d)$combinationTable$asDF
    three <- run_dcomb(d)$combinationTable$asDF

    pat <- function(ct) as.character(ct$pattern[ct$rowType == "Pattern"])
    expect_identical(pat(two),   c("+/+", "+/-", "-/+", "-/-"))
    expect_identical(pat(three), c("+/+/+", "+/+/-", "+/-/+", "+/-/-",
                                   "-/+/+", "-/+/-", "-/-/+", "-/-/-"))

    # the cross-tabulation must use the same labels, in the same order
    ftwo <- run_dcomb2(d, showFrequency = TRUE)$crossTabTable$asDF
    expect_identical(as.character(ftwo$testCombo), pat(two))
})


test_that("an all-negative winner is flagged as a probable inverted positive level", {
    # Relabelling symmetry: flipping every test's positive level reproduces the same 2x2s
    # with reversed labels, so "-/-/-" wins while every named strategy goes negative. The
    # headline then reads "call the patient positive when the tests are negative", which
    # is not a rule anyone can apply.
    d <- dcomb_fixture()
    flip <- function(x) ifelse(x == "pos", "neg", "pos")
    inv <- d; inv$t1 <- flip(d$t1); inv$t2 <- flip(d$t2)

    res <- run_dcomb2(inv, showRecommendation = TRUE)
    expect_match(notices_of(res), "Positive Levels May Be Inverted")

    # the correctly-labelled analysis must NOT raise it
    ok <- run_dcomb2(d, showRecommendation = TRUE)
    expect_false(grepl("Positive Levels May Be Inverted", notices_of(ok), fixed = TRUE))
})


test_that("the recommendation table is hidden rather than showing a blank row", {
    # rows: 1 is a fixed schema, so the seeded all-NA row survives every early return.
    d <- dcomb_fixture()
    bad <- do.call(ClinicoPath::decisioncombine, list(
        data = d, gold = "gold", goldPositive = "pos",
        test1 = "t1", test1Positive = "pos",
        test2 = "t1", test2Positive = "pos",        # same variable twice -> validation error
        test3 = NULL, test3Positive = NULL, showRecommendation = TRUE))
    expect_match(notices_of(bad), "Variables Must Be Distinct")
    expect_false(bad$recommendationTable$visible)

    good <- run_dcomb2(d, showRecommendation = TRUE)
    expect_true(good$recommendationTable$visible)
    expect_false(is.na(as.data.frame(good$recommendationTable$asDF)$pattern[1]))
})


test_that("the About and Assumptions panels render, and cover the three design biases", {
    # Every sibling meddecide analysis ships an explanatory panel; this one had none, and
    # nothing anywhere in the analysis mentioned verification, spectrum or incorporation
    # bias -- the three that dominate real pathology accuracy studies and none of which is
    # detectable from the data in front of the analysis.
    skip_if_not("showAbout" %in% names(formals(ClinicoPath::decisioncombine)),
                "jmvtools::prepare() has not compiled the showAbout option yet")
    d <- dcomb_fixture()
    on_  <- run_dcomb2(d, showAbout = TRUE)
    off_ <- run_dcomb2(d, showAbout = FALSE)

    expect_true(nzchar(on_$about$content))
    expect_true(nzchar(on_$assumptions$content))
    expect_false(nzchar(off_$about$content))

    txt <- paste(on_$about$content, on_$assumptions$content)
    for (term in c("Verification", "Spectrum", "Incorporation",
                   "conditionally independent", "McNemar",
                   "Balanced Accuracy = (J + 1) / 2")) {
        expect_true(grepl(term, txt, fixed = TRUE), info = term)
    }

    # theme-safe: translucent tint + explicit foreground, no opaque hex background
    expect_true(grepl("rgba(", txt, fixed = TRUE))
    expect_true(grepl("color: inherit", txt, fixed = TRUE))
    expect_false(grepl("background-color: #", txt, fixed = TRUE))
})


test_that("the About panel survives a validation early-return", {
    # It is static educational content. A user who ticks the box while still choosing
    # variables should get the explanation, not an empty pane.
    skip_if_not("showAbout" %in% names(formals(ClinicoPath::decisioncombine)),
                "jmvtools::prepare() has not compiled the showAbout option yet")
    d <- dcomb_fixture()
    bad <- do.call(ClinicoPath::decisioncombine, list(
        data = d, gold = "gold", goldPositive = "pos",
        test1 = "t1", test1Positive = "pos",
        test2 = "t1", test2Positive = "pos",        # duplicate variable -> validation error
        test3 = NULL, test3Positive = NULL, showAbout = TRUE))
    expect_match(notices_of(bad), "Variables Must Be Distinct")
    expect_true(nzchar(bad$about$content))
})


test_that("the explanatory panels do not contradict the code or the bias literature", {
    # A clinical review of this text found five substantive errors, each verified
    # numerically. These pin the corrections so they cannot regress.
    skip_if_not("showAbout" %in% names(formals(ClinicoPath::decisioncombine)),
                "jmvtools::prepare() has not compiled the showAbout option yet")
    res <- run_dcomb2(dcomb_fixture(), showAbout = TRUE)
    txt <- paste(res$about$content, res$assumptions$content,
                 paste(unlist(res$combinationTable$notes), collapse = " "))

    # A: case-control sampling DEFLATES NPV; "inflates every metric" was false.
    #    sens=spec=0.90 -> 50% prev: PPV .90 NPV .90 | 5% prev: PPV .32 NPV .99
    expect_false(grepl("inflates every metric", txt, fixed = TRUE))
    expect_true(grepl("NPV is DEFLATED", txt, fixed = TRUE))

    # B: the missing TRUE negatives are what deflate specificity -- previously unstated
    expect_true(grepl("missing true negatives deflate specificity", txt, fixed = TRUE))

    # C: the panel claimed the ratios always stay finite, contradicting the code's own
    #    empty-margin guard, which leaves such a row blank
    expect_true(grepl("left BLANK rather than corrected", txt, fixed = TRUE) ||
                grepl("left blank instead of corrected", txt, fixed = TRUE))

    # D: LR+/LR- are deterministic functions of sens/spec, so they cannot be "more stable"
    expect_false(grepl("which is why LR+ and LR- are often the more portable", txt, fixed = TRUE))
    expect_true(grepl("inherit exactly the same dependence", txt, fixed = TRUE))

    # E: a single McNemar compares positivity rates, not accuracy
    expect_true(grepl("SEPARATELY within the diseased patients", txt, fixed = TRUE))

    # F: Accuracy/NPV collapse toward 1-prevalence on a rare pattern row
    expect_true(grepl("close to 1 minus the prevalence", txt, fixed = TRUE))
})


test_that("a rare pattern row really does report high Accuracy at near-zero Youden", {
    # The numeric claim the panel now makes, checked against the module's own output
    # rather than restated: 5/50 diseased and 10/150 non-diseased show the pattern.
    # Both tests must exhibit both levels: a level named as positive that never occurs is
    # rejected by .validateInputs with "Missing Level" -- correctly -- and the table comes
    # back empty. The complement rows therefore carry t2 = "pos".
    mk <- function(g, a, b, k)
        data.frame(gold = g, t1 = a, t2 = b, stringsAsFactors = FALSE)[rep(1, k), ]
    dat <- rbind(mk("pos", "pos", "neg",   5),   # "+/-"  tp
                 mk("neg", "pos", "neg",  10),   # "+/-"  fp
                 mk("pos", "neg", "pos",  45),   # "-/+"
                 mk("neg", "neg", "pos", 140))   # "-/+"
    ct <- as.data.frame(run_dcomb2(dat)$combinationTable$asDF)
    row <- ct[ct$pattern == "+/-", ]

    expect_equal(row$tp, 5); expect_equal(row$fp, 10)
    expect_equal(row$acc,    0.725, tolerance = 1e-9)   # looks 73% accurate
    expect_equal(row$npv,    140 / 185, tolerance = 1e-9)
    expect_equal(row$youden, 1 / 30, tolerance = 1e-9)  # ...and is worthless
    expect_lt(row$youden, 0.05)
})
