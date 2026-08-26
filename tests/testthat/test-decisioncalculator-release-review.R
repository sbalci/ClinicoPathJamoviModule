# Regression tests from the `decisioncalculator` release review.
#
# Each case corresponds to a defect confirmed in the shipped code. Statistics are checked
# against epiR::epi.tests or derived by hand, never against the module's own arithmetic.

skip_if_not_installed("epiR")

# TP = 90, FP = 30, FN = 20, TN = 80 -- deliberately asymmetric so a transposed 2x2 shows.
TP0 <- 90; TN0 <- 80; FP0 <- 30; FN0 <- 20

epir_ref <- function(TP, FP, FN, TN) {
    e <- as.data.frame(epiR::epi.tests(
        as.table(matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE)))$detail)
    function(stat) e$est[e$statistic == stat]
}

notices_of <- function(res)
    gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " ")))


test_that("point estimates match epiR::epi.tests and the 2x2 is not transposed", {
    g <- epir_ref(TP0, FP0, FN0, TN0)
    r <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0)$ratioTable$asDF

    expect_equal(r$Sens[1],   g("se"),      tolerance = 1e-10)
    expect_equal(r$Spec[1],   g("sp"),      tolerance = 1e-10)
    expect_equal(r$PPV[1],    g("pv.pos"),  tolerance = 1e-10)
    expect_equal(r$NPV[1],    g("pv.neg"),  tolerance = 1e-10)
    expect_equal(r$LRP[1],    g("lr.pos"),  tolerance = 1e-8)
    expect_equal(r$LRN[1],    g("lr.neg"),  tolerance = 1e-8)
    # sensitivity is TP/(TP+FN), not TP/(TP+FP) -- these differ on this table
    expect_equal(r$Sens[1], TP0 / (TP0 + FN0), tolerance = 1e-12)
    expect_false(isTRUE(all.equal(r$Sens[1], TP0 / (TP0 + FP0))))
})


test_that("a supplied prior moves PPV and NPV, and the table says which prevalence is used", {
    # This is the CRITICAL the sibling `decision` had: the Prevalence cell was overwritten
    # with the prior while PPV/NPV stayed at the study-prevalence values.
    g <- epir_ref(TP0, FP0, FN0, TN0)
    se <- g("se"); sp <- g("sp"); p <- 0.05

    r <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0,
                            pp = TRUE, pprob = p)$ratioTable$asDF

    expect_equal(r$PrevalenceD[1], p, tolerance = 1e-12)
    expect_equal(r$PPV[1], p * se / (p * se + (1 - p) * (1 - sp)), tolerance = 1e-10)
    expect_equal(r$NPV[1], (1 - p) * sp / ((1 - p) * sp + p * (1 - se)), tolerance = 1e-10)
    # sensitivity and specificity are properties of the test, not the population
    expect_equal(r$Sens[1], se, tolerance = 1e-12)
    expect_equal(r$Spec[1], sp, tolerance = 1e-12)

    # ...and without a prior it reports the study prevalence with the raw 2x2 values
    r0 <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0)$ratioTable$asDF
    expect_equal(r0$PrevalenceD[1], (TP0 + FN0) / (TP0 + TN0 + FP0 + FN0), tolerance = 1e-12)
    expect_equal(r0$PPV[1], TP0 / (TP0 + FP0), tolerance = 1e-12)
})


test_that("confidence intervals match epiR on every bound", {
    ci <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0,
                             ci = TRUE)$epirTable_ratio$asDF
    e <- as.data.frame(epiR::epi.tests(
        as.table(matrix(c(TP0, FP0, FN0, TN0), nrow = 2, byrow = TRUE)))$detail)

    pick <- function(label) ci[ci$statsnames == label, ][1, ]
    for (pair in list(c("Test sensitivity", "se"), c("Test specificity", "sp"),
                      c("Positive predictive value", "pv.pos"),
                      c("Negative predictive value", "pv.neg"))) {
        got <- pick(pair[1]); ref <- e[e$statistic == pair[2], ]
        expect_equal(got$est,   ref$est,   tolerance = 1e-10, info = pair[1])
        expect_equal(got$lower, ref$lower, tolerance = 1e-10, info = pair[1])
        expect_equal(got$upper, ref$upper, tolerance = 1e-10, info = pair[1])
    }
    # statistics with no interval are blank, not given a fabricated one
    ba <- ci[grepl("Balanced accuracy", ci$statsnames), ][1, ]
    expect_true(is.na(ba$lower) && is.na(ba$upper))
})


test_that("tables do not accumulate rows across re-runs", {
    # jmvcore's addRow() with an existing rowKey DUPLICATES rather than replacing (verified
    # on a bare Table: rowCount 2 -> 4 -> 6 over three passes, after which $asDF fails with
    # "duplicate 'row.names' are not allowed"). jamovi re-runs .run() on the SAME object on
    # every option change, so any table rebuilt with addRow() must call deleteRows() first.
    tbl <- jmvcore::Table$new(options = jmvcore::Options$new(), name = "t", title = "t",
                              columns = list(list(`name` = "a", `type` = "number")))
    for (i in 1:2) tbl$addRow(rowKey = i, values = list(a = i))
    expect_equal(tbl$rowCount, 2L)
    for (i in 1:2) tbl$addRow(rowKey = i, values = list(a = i))
    expect_equal(tbl$rowCount, 4L, label = "addRow duplicates on a repeated key")

    # The CI tables are rebuilt with addRow() on each run, so they must clear first.
    # multipleCutoffTable is different: its three fixed rows are created once in .init()
    # and .run() updates them with setRow(), which cannot accumulate duplicate rows.
    src <- readLines(testthat::test_path("..", "..", "R", "decisioncalculator.b.R"))
    for (nm in c("epirTable_ratio", "epirTable_number")) {
        expect_true(any(grepl(paste0(nm, "\\$deleteRows\\(\\)"), src)),
                    info = paste(nm, "must be cleared before addRow"))
    }
    expect_equal(sum(grepl("multipleCutoffTable\\$addRow\\(", src)), 3L)
    expect_equal(sum(grepl("multipleCutoffTable\\$setRow\\(", src)), 3L)
})


test_that("the option-only Fagan plot does not require dataset rows", {
    spec <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi",
                                                "decisioncalculator.r.yaml"))
    item_names <- vapply(spec$items, function(item) item$name, character(1))
    plot <- spec$items[[which(item_names == "plot1")]]

    expect_identical(plot$type, "Image")
    expect_identical(plot$requiresData, FALSE)
})


test_that("the Fagan nomogram renders on a zero cell", {
    # nomogrammer rejects a sensitivity or specificity of exactly 0 or 1, which any zero
    # cell produces, so the plot silently failed for precisely the sparse tables that most
    # need one. The LRs it receives are Haldane-Anscombe corrected, so the proportions now
    # come from the same corrected table.
    pdf(NULL); on.exit(dev.off(), add = TRUE)

    render_with <- function(...) {
        args <- utils::modifyList(
            list(TP = TP0, TN = TN0, FP = FP0, FN = FN0, fagan = TRUE), list(...))
        r <- do.call(decisioncalculator, args)
        o <- do.call(ClinicoPath:::decisioncalculatorOptions$new, args)
        a <- ClinicoPath:::decisioncalculatorClass$new(options = o)
        a$results$plot1$setState(r$plot1$state)
        suppressWarnings(
            a$.__enclos_env__$private$.plot1(a$results$plot1,
                                             ggtheme = ggplot2::theme_minimal()))
    }

    expect_true(isTRUE(render_with()))               # normal
    expect_true(isTRUE(render_with(FP = 0)))         # specificity == 1
    expect_true(isTRUE(render_with(FN = 0)))         # sensitivity == 1
    expect_true(isTRUE(render_with(pp = TRUE, pprob = 0.05)))

    # a test whose positive result argues AGAINST disease cannot be drawn on a nomogram;
    # it must decline with an explanation rather than crash
    expect_false(isTRUE(render_with(TP = 0)))
    expect_match(notices_of(decisioncalculator(TP = 0, TN = TN0, FP = FP0, FN = FN0,
                                               fagan = TRUE)),
                 "Fagan nomogram not drawn")
})


test_that("the cut-off comparison names the best alternative, not the first", {
    # The verdict was an if/else-if chain, so when BOTH alternatives beat the current
    # cut-off only cutoff1 was ever named -- even when cutoff2 was far better.
    r <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0, multiplecuts = TRUE,
                            tp1 = 95,  fp1 = 28, tn1 = 82,  fn1 = 15,   # a little better
                            tp2 = 105, fp2 = 10, tn2 = 100, fn2 = 5)    # much better
    rec <- as.character(r$multipleCutoffTable$asDF$recommendation[3])

    expect_match(rec, "Aggressive")          # the cutoff2 label
    expect_false(grepl("Conservative", rec)) # not the cutoff1 label
})


test_that("a trivial cut-off advantage is not reported as better performance", {
    r <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0, multiplecuts = TRUE,
                            tp1 = 91, fp1 = 30, tn1 = 80, fn1 = 19,
                            tp2 = 90, fp2 = 31, tn2 = 79, fn2 = 20)
    rec <- as.character(r$multipleCutoffTable$asDF$recommendation[3])

    expect_match(rec, "too small to distinguish")
    expect_false(grepl("performs better than current", rec, fixed = TRUE))
})


test_that("cut-offs describing different cohort sizes are flagged", {
    # Moving a threshold on one cohort cannot change how many patients there are, so
    # differing totals mean these are separate studies, not thresholds.
    r <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0, multiplecuts = TRUE,
                            tp1 = 95,  fp1 = 28, tn1 = 82,  fn1 = 15,
                            tp2 = 150, fp2 = 20, tn2 = 200, fn2 = 30)
    expect_match(notices_of(r), "different numbers of patients")

    # matched totals produce no such notice
    ok <- decisioncalculator(TP = 90, TN = 80, FP = 30, FN = 20, multiplecuts = TRUE,
                             tp1 = 95, fp1 = 25, tn1 = 85, fn1 = 15,
                             tp2 = 85, fp2 = 35, tn2 = 75, fn2 = 25)
    expect_false(grepl("different numbers of patients", notices_of(ok)))
})


test_that("invalid counts are reported through notices, not silently computed", {
    for (case in list(
        list(args = list(TP = -10, TN = 80, FP = 20, FN = 10), notice = "Negative Counts Detected"),
        list(args = list(TP = 0, TN = 0, FP = 0, FN = 0),      notice = "All Counts Zero"),
        list(args = list(TP = Inf, TN = 80, FP = 20, FN = 10), notice = "Non-Finite Counts"),
        list(args = list(TP = 0, TN = 100, FP = 50, FN = 0),   notice = "No Diseased Subjects"),
        list(args = list(TP = 100, TN = 0, FP = 0, FN = 50),   notice = "No Healthy Subjects"))) {
        res <- do.call(decisioncalculator, case$args)
        expect_match(notices_of(res), case$notice, info = case$notice)
    }
})


test_that("pprob is bounded by the wrapper, so the backend's own check is unreachable", {
    # jamovi/decisioncalculator.a.yaml declares min: 0.001 / max: 0.999, enforced by jmvcore
    # in the generated wrapper before .run() is reached.
    expect_error(decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0,
                                    pp = TRUE, pprob = 0),
                 "pprob must be between")
    expect_error(decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0,
                                    pp = TRUE, pprob = 1),
                 "pprob must be between")
})


test_that("a zero cell continuity-corrects the ratios and says so", {
    r <- decisioncalculator(TP = TP0, TN = TN0, FP = 0, FN = FN0)
    x <- r$ratioTable$asDF

    expect_true(is.finite(x$LRP[1]))            # not Inf
    expect_equal(x$Spec[1], 1, tolerance = 1e-12)  # the table keeps the uncorrected value
    expect_match(notices_of(r), "Continuity Correction Applied")
    # and it discloses that the intervals are NOT corrected
    expect_match(notices_of(r), "not continuity-corrected")
})


# ── follow-up: limitations closed after the first review pass ────────────────────────

test_that("the Fagan reading is shown in jamovi, not printed to the console", {
    # nomogrammer prints its summary (prevalence, LRs, post-test probabilities) to stdout
    # under Verbose = TRUE. jamovi never shows stdout, so the most clinically useful part
    # of the figure was invisible. It is now rendered beside the plot, at the tables'
    # precision rather than nomogrammer's whole percents.
    r <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0, fagan = TRUE)
    panel <- tryCatch(paste(r$faganSummary$content, collapse = ""), error = function(e) NULL)
    skip_if(is.null(panel) || !nzchar(panel),
            "faganSummary not compiled yet - run jmvtools::prepare()")

    txt <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", panel))
    x <- r$ratioTable$asDF
    expect_match(txt, "Pre-test probability")
    expect_match(txt, sprintf("%.1f%%", 100 * x$PostTestProbDisease[1]), fixed = TRUE)
    expect_match(txt, sprintf("%.1f%%", 100 * (1 - x$PostTestProbHealthy[1])), fixed = TRUE)
})


test_that("the Fagan reading tracks the supplied prior and matches Bayes", {
    r <- decisioncalculator(TP = TP0, TN = TN0, FP = FP0, FN = FN0,
                            fagan = TRUE, pp = TRUE, pprob = 0.05)
    x <- r$ratioTable$asDF

    # whatever the panel prints, the underlying quantities must be the odds-form Bayes
    # update from the DISPLAYED prevalence -- this holds with or without regeneration
    pre_odds <- x$PrevalenceD[1] / (1 - x$PrevalenceD[1])
    expect_equal(x$PostTestProbDisease[1],
                 (pre_odds * x$LRP[1]) / (1 + pre_odds * x$LRP[1]), tolerance = 1e-10)
    expect_equal(1 - x$PostTestProbHealthy[1],
                 (pre_odds * x$LRN[1]) / (1 + pre_odds * x$LRN[1]), tolerance = 1e-10)
    expect_equal(x$PrevalenceD[1], 0.05, tolerance = 1e-12)

    panel <- tryCatch(paste(r$faganSummary$content, collapse = ""), error = function(e) NULL)
    skip_if(is.null(panel) || !nzchar(panel), "faganSummary not compiled yet")
    txt <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", panel))
    expect_match(txt, "population prevalence you supplied")
})


test_that("the cut-off verdict reports whether the accuracy intervals overlap", {
    # A formal paired test is impossible from four marginal counts per scenario, but a
    # Wilson interval on each accuracy is computable, and overlap is a conservative signal
    # that the counts do not separate the cut-offs.
    overlapping <- decisioncalculator(
        TP = TP0, TN = TN0, FP = FP0, FN = FN0, multiplecuts = TRUE,
        tp1 = 100, fp1 = 25, tn1 = 85, fn1 = 10,
        tp2 = 95,  fp2 = 28, tn2 = 82, fn2 = 15)
    rec1 <- as.character(overlapping$multipleCutoffTable$asDF$recommendation[3])
    expect_match(rec1, "overlaps the current cut-off")
    expect_match(rec1, "not established")

    separated <- decisioncalculator(
        TP = TP0, TN = TN0, FP = FP0, FN = FN0, multiplecuts = TRUE,
        tp1 = 108, fp1 = 4,  tn1 = 106, fn1 = 2,
        tp2 = 95,  fp2 = 28, tn2 = 82,  fn2 = 15)
    rec2 <- as.character(separated$multipleCutoffTable$asDF$recommendation[3])
    expect_match(rec2, "do not overlap")

    # and the table says why a formal test is not offered
    notes <- paste(vapply(as.list(separated$multipleCutoffTable$notes),
                          function(n) n$note, character(1)), collapse = " ")
    expect_match(notes, "four summary counts per scenario cannot supply that")
})
