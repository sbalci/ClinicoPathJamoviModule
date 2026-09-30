# Regression tests for the /check-function-full decisioncombine pass (audit 2026-09-29, fixed
# 2026-09-30; findings in TODO.md under "### decisioncombine"): the notices panel above the
# tables, "Sparse Cell Counts" as a plain warning (tested in the independent-review file),
# Youden's J with its Agresti-Caffo interval, the graded discrimination notice for the
# top-ranked rule, the single-test ranking note, the blank decision-space notice, the forest
# note for Youden's J, and the level-selector labels.

# Patient-level data from pattern counts; factors typed as jamovi delivers them.
.cf_mk <- function(cD, cN, pos = "pos", neg = "neg") {
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

# Every Level argument passed explicitly; do.call() keeps the strings from being read as symbols.
.cf_run <- function(d, t2 = "t2", ...) {
    args <- c(list(data = d, gold = "gold", goldPositive = "D", test1 = "t1",
                   test1Positive = "pos", test2 = t2,
                   test2Positive = if (is.null(t2)) NULL else "pos",
                   test3 = NULL, test3Positive = NULL), list(...))
    suppressMessages(do.call(ClinicoPath::decisioncombine, args))
}

# The analysis object itself, for the private discrimination check.
.cf_object <- function(d) {
    Options <- getFromNamespace("decisioncombineOptions", "ClinicoPath")
    Class <- getFromNamespace("decisioncombineClass", "ClinicoPath")
    options <- Options$new(gold = "gold", goldPositive = "D", test1 = "t1",
                           test1Positive = "pos", test2 = "t2", test2Positive = "pos",
                           test3 = NULL, test3Positive = NULL)
    a <- Class$new(options = options, data = d)
    suppressMessages(a$run())
    a
}

.cf_unescape <- function(x) {
    gsub("&amp;", "&", gsub("&gt;", ">", gsub("&lt;", "<", x, fixed = TRUE), fixed = TRUE),
         fixed = TRUE)
}

# Notices as rendered: severity word shown to the user, title, body.
.cf_notices <- function(res) {
    h <- res$notices$content
    blocks <- if (is.null(h)) character() else
        regmatches(h, gregexpr("<strong>[^<]*</strong><br><span[^>]*>[^<]*</span>", h))[[1]]
    heads <- .cf_unescape(sub("^<strong>([^<]*)</strong>.*$", "\\1", blocks))
    data.frame(
        severity = sub("^[^ ]* ", "", sub(": .*$", "", heads)),
        title = sub("^[^:]*: ", "", heads),
        content = .cf_unescape(sub("^.*<span[^>]*>([^<]*)</span>$", "\\1", blocks)),
        stringsAsFactors = FALSE)
}

# Agresti-Caffo 95% interval for p1 - p2 in closed form (one success and one failure added
# per group, then Wald), clamped to [-1, 1] as DescTools::BinomDiffCI clamps it.
.cf_ac <- function(x1, n1, x2, n2, z = stats::qnorm(0.975)) {
    p1 <- (x1 + 1) / (n1 + 2)
    p2 <- (x2 + 1) / (n2 + 2)
    h <- z * sqrt(p1 * (1 - p1) / (n1 + 2) + p2 * (1 - p2) / (n2 + 2))
    list(lower = pmax(-1, p1 - p2 - h), upper = pmin(1, p1 - p2 + h))
}

# Two weak tests, 150 diseased and 150 not. Test 1 and Test 2 each: sensitivity 60/150,
# false-positive rate 30/150, J = 0.200. Parallel: 90/150 - 50/150 = 0.267, the top rule.
.CF_WEAK2 <- list(cD = c("+/+" = 30, "+/-" = 30, "-/+" = 30, "-/-" = 60),
                  cN = c("+/+" = 10, "+/-" = 20, "-/+" = 20, "-/-" = 100))


test_that("CF-01 the notices panel is the second results item, above every table", {
    res <- .cf_run(.cf_mk(.CF_WEAK2$cD, .CF_WEAK2$cN))
    expect_identical(names(res$items)[1:3], c("welcome", "notices", "combinationTable"))

    skip_if_not_installed("yaml")
    spec <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.r.yaml"))
    items <- stats::setNames(spec$items, vapply(spec$items, `[[`, "", "name"))
    expect_identical(names(items)[1:3], c("welcome", "notices", "combinationTable"))
    # The &analysisInputs anchor moved up with the panel (an alias must follow its anchor);
    # every item that aliases it still clears on the eight inputs.
    inputs <- c("gold", "goldPositive", "test1", "test1Positive", "test2", "test2Positive",
                "test3", "test3Positive")
    for (nm in c("notices", "combinationTable", "combinationTableCI",
                 "combinationTableCIRatios", "recommendationTable")) {
        expect_identical(unlist(items[[nm]]$clearWith), inputs, info = nm)
    }
})

test_that("CF-02 every row carries the Agresti-Caffo 95% interval for Youden's J", {
    res <- .cf_run(.cf_mk(.CF_WEAK2$cD, .CF_WEAK2$cN))
    tab <- res$combinationTable$asDF
    ac <- .cf_ac(tab$tp, tab$tp + tab$fn, tab$fp, tab$fp + tab$tn)
    expect_equal(tab$youdenLower, ac$lower, tolerance = 1e-12)
    expect_equal(tab$youdenUpper, ac$upper, tolerance = 1e-12)
    # Parallel: (91/152 - 51/152) -/+ 1.96 * sqrt(q1(1-q1)/152 + q2(1-q2)/152) = 0.155 to 0.371
    par <- tab[tab$pattern == "Parallel (>=1 pos)", ]
    expect_equal(round(c(par$youdenLower, par$youdenUpper), 3), c(0.155, 0.371))
    # The method is named; the ranking's own (Wilson-sum) bound is explained on the ranking
    # table, next to the sentence that quotes it (CF-14), not on every combination table.
    note <- res$combinationTable$notes$youden_ci$note
    expect_match(note, "Agresti-Caffo interval", fixed = TRUE)
    expect_false(grepl("Wilson", note, fixed = TRUE))

    # Blank where J is undefined: every complete case disease-present.
    d <- .cf_mk(.CF_WEAK2$cD, .CF_WEAK2$cN)
    one_class <- .cf_run(d[d$gold == "D", ])$combinationTable$asDF
    expect_true(all(is.na(one_class$youdenLower)))
    expect_true(all(is.na(one_class$youdenUpper)))
})

test_that("CF-03 the inversion warning quotes the interval shown in the table", {
    # J = 5/40 - 30/60 = -0.375; AC interval -0.521 to -0.194, wholly below 0.
    res <- .cf_run(.cf_mk(c("+" = 5, "-" = 35), c("+" = 30, "-" = 30)), t2 = NULL)
    nt <- .cf_notices(res)
    inv <- nt$content[nt$title == "Positive Levels May Be Inverted"]
    expect_length(inv, 1L)
    tab <- res$combinationTable$asDF
    f3 <- function(x) base::format(round(x, 3), nsmall = 3)
    expect_match(inv, paste0("Youden's J = -0.375 (Agresti-Caffo 95% CI ", f3(tab$youdenLower),
                             " to ", f3(tab$youdenUpper), ")"), fixed = TRUE)
    expect_identical(f3(c(tab$youdenLower, tab$youdenUpper)), c("-0.521", "-0.194"))
    # The swapped level is the diagnosis; no discrimination notice beside it.
    expect_false(any(grepl("Discrimination", nt$title, fixed = TRUE)))
})

test_that("CF-04 one test: discrimination graded by its interval, serious only when poor is established", {
    one <- function(a, n1, b, n0) {
        .cf_notices(.cf_run(.cf_mk(c("+" = a, "-" = n1 - a), c("+" = b, "-" = n0 - b)),
                            t2 = NULL))
    }
    # J = 120/200 - 80/200 = 0.200; AC 95% CI 0.102 to 0.294 lies wholly below 0.40.
    nt <- one(120, 200, 80, 200)
    expect_identical(nt$severity[nt$title == "Poor Discrimination"], "Serious warning")
    expect_match(nt$content[nt$title == "Poor Discrimination"], paste0(
        "Youden's J for Test 1 is 0.200 (Agresti-Caffo 95% CI 0.102 to 0.294; equivalent AUC ",
        "0.600). The whole interval lies below 0.40"), fixed = TRUE)
    # J = 33/60 - 27/60 = 0.100; CI -0.078 to 0.272 includes 0 but also lies below 0.40:
    # poor discrimination is established, so still the serious warning.
    nt <- one(33, 60, 27, 60)
    expect_identical(nt$severity[nt$title == "Poor Discrimination"], "Serious warning")
    # J = 12/20 - 7/20 = 0.250; CI -0.060 to 0.515 includes 0 AND reaches 0.40: inconclusive,
    # a plain warning (as a serious one it fired on a third of acceptable tests at this size).
    nt <- one(12, 20, 7, 20)
    expect_identical(nt$severity[nt$title == "Discrimination Not Established"], "Warning")
    expect_match(nt$content[nt$title == "Discrimination Not Established"], paste0(
        "Youden's J for Test 1 is 0.250 (Agresti-Caffo 95% CI -0.060 to 0.515; equivalent AUC ",
        "0.625). The interval includes 0"), fixed = TRUE)
    # J = 35/50 - 18/50 = 0.340; CI 0.146 to 0.508 excludes 0 and reaches 0.40.
    nt <- one(35, 50, 18, 50)
    expect_identical(nt$severity[nt$title == "Discrimination May Be Poor"], "Warning")
    expect_match(nt$content[nt$title == "Discrimination May Be Poor"],
                 "(Agresti-Caffo 95% CI 0.146 to 0.508; equivalent AUC 0.670)", fixed = TRUE)
    expect_match(nt$content[nt$title == "Discrimination May Be Poor"],
                 "a larger sample could settle it", fixed = TRUE)
    # J = 30/50 - 10/50 = 0.400 as printed (0.39999... in floating point): acceptable, silent.
    nt <- one(30, 50, 10, 50)
    expect_false(any(grepl("Discrimination", nt$title, fixed = TRUE)))
})

test_that("CF-05 with two tests the discrimination notice names the top-ranked rule", {
    res <- .cf_run(.cf_mk(.CF_WEAK2$cD, .CF_WEAK2$cN), showRecommendation = TRUE)
    nt <- .cf_notices(res)
    poor <- nt$content[nt$title == "Poor Discrimination"]
    expect_length(poor, 1L)
    # Parallel: J 0.267, equivalent AUC (1 + 0.267) / 2 = 0.6335. Every rule's interval is
    # below 0.40 (the widest reaches 0.371), so the claim about all of them holds.
    expect_match(poor, paste0("No rule scored here reaches acceptable discrimination. The highest ",
                              "Youden's J is 0.267, for Parallel (>=1 pos) (Agresti-Caffo 95% CI ",
                              "0.155 to 0.371; equivalent AUC 0.6335), and the 95% confidence ",
                              "interval of every rule lies wholly below 0.40"), fixed = TRUE)
    tab <- res$combinationTable$asDF
    expect_true(all(round(tab$youdenUpper, 3) < 0.4))
    expect_identical(res$recommendationTable$asDF$pattern, "Parallel (>=1 pos)")
})

test_that("CF-06 a raised inversion warning silences the discrimination notice", {
    a <- .cf_object(.cf_mk(.CF_WEAK2$cD, .CF_WEAK2$cN))
    priv <- a$.__enclos_env__$private
    best <- a$results$combinationTable$asDF
    best <- best[best$pattern == "Parallel (>=1 pos)", , drop = FALSE]
    scored <- a$results$combinationTable$asDF
    priv$.noticeList <- list()
    priv$.assessTopRuleDiscrimination(best, scored, inversion_flagged = TRUE, multi_test = TRUE)
    expect_length(priv$.noticeList, 0L)
    priv$.assessTopRuleDiscrimination(best, scored, inversion_flagged = FALSE, multi_test = TRUE)
    expect_identical(vapply(priv$.noticeList, `[[`, "", "title"), "Poor Discrimination")
    # The notice carries its own reference; nothing else in this run cites it.
    expect_identical(priv$.noticeList[[1]]$refs, "HosmerLemeshow2013")
})

test_that("CF-07 one test: no ranking table, a note saying why, and the guards still run", {
    d <- .cf_mk(c("+" = 40, "-" = 10), c("+" = 10, "-" = 40))
    res <- .cf_run(d, t2 = NULL, showRecommendation = TRUE)
    nt <- .cf_notices(res)
    expect_identical(nt$severity[nt$title == "Ranking Needs Two or More Tests"], "Note")
    expect_false(res$recommendationTable$visible)
    # No promise about cells that a one-class sample leaves blank.
    one_class <- d[d$gold == "D", ]
    oc <- .cf_notices(.cf_run(one_class, t2 = NULL, showRecommendation = TRUE))
    expect_false(grepl("confidence interval", oc$content[oc$title == "Ranking Needs Two or More Tests"],
                       fixed = TRUE))
    # Not said when no ranking was asked for.
    expect_false("Ranking Needs Two or More Tests" %in% .cf_notices(.cf_run(d, t2 = NULL))$title)
    # Small reference groups: this note, not "Strategy Ranking Unavailable".
    small <- .cf_notices(.cf_run(.cf_mk(c("+" = 6, "-" = 2), c("+" = 2, "-" = 6)), t2 = NULL,
                                 showRecommendation = TRUE))$title
    expect_true("Ranking Needs Two or More Tests" %in% small)
    expect_false("Strategy Ranking Unavailable" %in% small)
    # With two tests and small groups the ranking's own warning is unchanged.
    two <- .cf_notices(.cf_run(.cf_mk(c("+/+" = 4, "+/-" = 2, "-/+" = 1, "-/-" = 1),
                                      c("+/+" = 1, "+/-" = 1, "-/+" = 2, "-/-" = 4)),
                               showRecommendation = TRUE))$title
    expect_true("Strategy Ranking Unavailable" %in% two)
    expect_false("Ranking Needs Two or More Tests" %in% two)
    # A single test with J exactly 0 on eligible groups is still flagged, now by the graded
    # discrimination notice (interval -0.295 to 0.295, wholly below 0.40), not by the ungraded
    # "No Rule Performs Better Than Chance" (CF-15).
    zero <- .cf_notices(.cf_run(.cf_mk(c("+" = 10, "-" = 10), c("+" = 10, "-" = 10)),
                                t2 = NULL, showRecommendation = TRUE))
    expect_identical(zero$severity[zero$title == "Poor Discrimination"], "Serious warning")
    expect_false("No Rule Performs Better Than Chance" %in% zero$title)
    expect_false("Test Performs at Chance Level" %in% zero$title)   # J is 0, not below it
})

test_that("CF-08 a blank decision-space plot is explained", {
    d <- .cf_mk(.CF_WEAK2$cD, .CF_WEAK2$cN)
    one_class <- d[d$gold == "D", ]
    nt <- .cf_notices(.cf_run(one_class, showDecisionTree = TRUE))
    expect_identical(nt$severity[nt$title == "Decision-Space Plot Is Blank"], "Warning")
    expect_false("Decision-Space Plot Is Blank" %in% .cf_notices(.cf_run(one_class))$title)
    expect_false("Decision-Space Plot Is Blank" %in%
                     .cf_notices(.cf_run(d, showDecisionTree = TRUE))$title)
})

test_that("CF-09 the forest-plot note gives Youden's J its own reason", {
    d <- .cf_mk(.CF_WEAK2$cD, .CF_WEAK2$cN)
    title <- "Forest Plot Not Available for Selected Statistic"
    j <- .cf_notices(.cf_run(d, showForest = TRUE, filterStatistic = "youden"))
    expect_match(j$content[j$title == title],
                 "Its 95% confidence interval is in the combination table", fixed = TRUE)
    expect_false(grepl("does not calculate a confidence interval", j$content[j$title == title],
                       fixed = TRUE))
    p <- .cf_notices(.cf_run(d, showForest = TRUE, filterStatistic = "prevalence"))
    expect_match(p$content[p$title == title], "does not calculate a confidence interval",
                 fixed = TRUE)
})

test_that("CF-10 level selectors: sentence-case labels; Test 2's enabled only with Test 2", {
    skip_if_not_installed("yaml")
    ui <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.u.yaml"))
    sel <- list()
    walk <- function(node) {
        for (ch in node$children) {
            if (identical(ch$type, "LevelSelector")) sel[[ch$name]] <<- ch
            walk(ch)
        }
    }
    walk(ui)
    tests <- c("test1Positive", "test2Positive", "test3Positive")
    expect_identical(unname(vapply(sel[tests], `[[`, "", "label")), rep("Positive level", 3))
    expect_identical(sel$test2Positive$enable, "(test2)")
    expect_identical(sel$test3Positive$enable, "(test3)")
})

test_that("CF-11 the heatmap is described as it is drawn", {
    skip_if_not_installed("yaml")
    r <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.r.yaml"))
    items <- stats::setNames(r$items, vapply(r$items, `[[`, "", "name"))
    expect_identical(items$heatmapPlot$title, "Heatmap - Metrics by Pattern")
    expect_match(items$heatmapPlot$description, "balanced accuracy for each pattern by default",
                 fixed = TRUE)
    a <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.a.yaml"))
    opts <- stats::setNames(a$options, vapply(a$options, `[[`, "", "name"))
    ui_text <- gsub("\\s+", " ", opts$filterStatistic$description$ui)
    expect_match(ui_text, "the heatmap also includes prevalence and balanced accuracy.",
                 fixed = TRUE)
})

test_that("CF-12 'Not Established' needs every rule's interval to include 0", {
    # Codex's counterexample (20 + 20): the top rule, Test 1 alone (J 0.300), has an interval of
    # -0.010 to 0.556, but Test 2 alone (J 0.250) has 0.022 to 0.433, which excludes 0. So the
    # sample DOES show that a rule beats chance; the verdict is the plain "May Be Poor".
    res <- .cf_run(.cf_mk(c("+/+" = 5, "+/-" = 7, "-/+" = 0, "-/-" = 8),
                          c("+/+" = 0, "+/-" = 6, "-/+" = 0, "-/-" = 14)))
    tab <- res$combinationTable$asDF
    t2 <- tab[tab$pattern == "Test 2 alone", ]
    expect_equal(round(c(t2$youdenLower, t2$youdenUpper), 3), c(0.022, 0.433))
    nt <- .cf_notices(res)
    expect_false("Discrimination Not Established" %in% nt$title)
    expect_identical(nt$severity[nt$title == "Discrimination May Be Poor"], "Warning")
    expect_match(nt$content[nt$title == "Discrimination May Be Poor"],
                 "The 95% confidence interval of at least one rule reaches 0.40", fixed = TRUE)
})

test_that("CF-13 'Poor Discrimination' needs every rule's interval below 0.40", {
    # 80 + 80. Test 1 (sensitivity 0.30, no false positive) is the top rule, J 0.300, with a
    # narrow interval 0.190 to 0.395. Test 2 (0.70 against 0.425) has J 0.275 but a wider
    # interval, 0.122 to 0.415, that reaches 0.40: poor discrimination is not established for
    # every rule, so no serious warning.
    res <- .cf_run(.cf_mk(c("+/+" = 24, "+/-" = 0, "-/+" = 32, "-/-" = 24),
                          c("+/+" = 0, "+/-" = 0, "-/+" = 34, "-/-" = 46)))
    tab <- res$combinationTable$asDF
    t1 <- tab[tab$pattern == "Test 1 alone", ]
    t2 <- tab[tab$pattern == "Test 2 alone", ]
    expect_equal(round(t1$youdenUpper, 3), 0.395)
    expect_equal(round(t2$youdenUpper, 3), 0.415)
    nt <- .cf_notices(res)
    expect_false("Poor Discrimination" %in% nt$title)
    expect_identical(nt$severity[nt$title == "Discrimination May Be Poor"], "Warning")
})

test_that("CF-14 the notice reference and the ranking-bound note appear only when used", {
    # Weak tests (CF-05): the discrimination notice cites Hosmer-Lemeshow-Sturdivant, and the
    # ranking quotes its Wilson-sum bound, so both appear.
    weak <- .cf_run(.cf_mk(.CF_WEAK2$cD, .CF_WEAK2$cN), showRecommendation = TRUE)
    expect_true("HosmerLemeshow2013" %in% weak$notices$getRefs())
    expect_match(weak$recommendationTable$asDF$rationale, "lower Wilson limits", fixed = TRUE)
    expect_match(weak$recommendationTable$notes$bound$note, "Agresti-Caffo interval", fixed = TRUE)
    # A strong Test 1 (J 0.850) clearly ahead of the next rule with a different table (0.575):
    # no discrimination notice, no reference, no bound sentence and no bound note.
    strong <- .cf_run(.cf_mk(c("+/+" = 90, "+/-" = 90, "-/+" = 5, "-/-" = 15),
                             c("+/+" = 5, "+/-" = 5, "-/+" = 60, "-/-" = 130)),
                      showRecommendation = TRUE)
    expect_false(any(grepl("Discrimination", .cf_notices(strong)$title, fixed = TRUE)))
    expect_length(strong$notices$getRefs(), 0L)
    expect_false(grepl("Wilson", strong$recommendationTable$asDF$rationale, fixed = TRUE))
    expect_null(strong$recommendationTable$notes$bound)
    # The analysis-wide references no longer carry the notice-only source.
    skip_if_not_installed("yaml")
    spec <- yaml::yaml.load_file(
        testthat::test_path("..", "..", "jamovi", "decisioncombine.r.yaml"))
    expect_false("HosmerLemeshow2013" %in% unlist(spec$refs))
})

test_that("CF-15 one test with J at or below 0 within sampling variation: graded, not called anti-predictive", {
    # 15 + 15: 7 diseased and 8 non-diseased test-positive, J = 7/15 - 8/15 = -0.067. A sample
    # a weak but correctly coded test (true J 0.15) gives about a quarter of the time. Its
    # Agresti-Caffo interval, -0.394 to 0.277, includes 0.
    res <- .cf_run(.cf_mk(c("+" = 7, "-" = 8), c("+" = 8, "-" = 7)), t2 = NULL)
    nt <- .cf_notices(res)
    # It used to get the serious "No Rule Performs Better Than Chance", which calls the test
    # anti-predictive and asks for the positive level to be reviewed.
    expect_false("No Rule Performs Better Than Chance" %in% nt$title)
    expect_false("Positive Levels May Be Inverted" %in% nt$title)
    # Now: the quiet chance-level note (as with two or three tests) ...
    expect_identical(nt$severity[nt$title == "Test Performs at Chance Level"], "Note")
    expect_match(nt$content[nt$title == "Test Performs at Chance Level"],
                 "for Test 1 (J = -0.067, 95% CI -0.394 to 0.277)", fixed = TRUE)
    # ... and the graded discrimination notice: the whole interval lies below 0.40.
    expect_identical(nt$severity[nt$title == "Poor Discrimination"], "Serious warning")
    expect_match(nt$content[nt$title == "Poor Discrimination"],
                 "Youden's J for Test 1 is -0.067 (Agresti-Caffo 95% CI -0.394 to 0.277", fixed = TRUE)

    # A reversal beyond sampling variation keeps its own serious warning, which silences both.
    inv <- .cf_notices(.cf_run(.cf_mk(c("+" = 10, "-" = 30), c("+" = 30, "-" = 10)), t2 = NULL))
    expect_identical(inv$severity[inv$title == "Positive Levels May Be Inverted"], "Serious warning")
    expect_false(any(c("Poor Discrimination", "Discrimination Not Established",
                       "Test Performs at Chance Level", "No Rule Performs Better Than Chance") %in%
                         inv$title))

    # With two tests the warning is unchanged: it needs every rule's J to be exactly 0.
    each <- c("+/+" = 10, "+/-" = 10, "-/+" = 10, "-/-" = 10)
    null2 <- .cf_notices(.cf_run(.cf_mk(each, each)))
    expect_identical(null2$severity[null2$title == "No Rule Performs Better Than Chance"],
                     "Serious warning")
})
