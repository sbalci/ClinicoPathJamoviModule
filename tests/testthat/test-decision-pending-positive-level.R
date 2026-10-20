# An unchosen positive level is a pending state in decision and decisioncombine.
#
# Without allowNone the jamovi client fills a Level option with the variable's FIRST level,
# usually the negative one ("No", "0", "Benign"), so a new analysis silently computed
# sensitivity and specificity for the wrong event. R tests never saw it: they always pass the
# level. Every positive-level option now allows none; until it is chosen the analysis lists the
# levels found in an INFO notice and computes nothing. A non-empty level that is not in the data
# is still an error. Model: decisioncurve (test-decisioncurve-release-review.R, RR-01).
#
# Level options have no default, so they are required wrapper arguments: these tests pass NULL
# explicitly (args[nm] <- list(NULL); modifyList() would DROP the argument instead).

skip_if_not_installed("yaml")

pl_level_options <- function(analysis) {
    path <- testthat::test_path("..", "..", "jamovi", paste0(analysis, ".a.yaml"))
    skip_if_not(file.exists(path))
    Filter(function(o) identical(o$type, "Level"), yaml::read_yaml(path)$options)
}

pl_with <- function(args, extra) {
    for (nm in names(extra)) args[nm] <- list(extra[[nm]])
    args
}

# Level order puts the negative level first, as jamovi's alphabetical order does.
# Reference Present 30 / Absent 50; test TP 24, FN 6, FP 5, TN 45: sens 0.80, spec 0.90.
pl_decision_data <- function() data.frame(
    gold = factor(c(rep("Present", 30), rep("Absent", 50)), levels = c("Absent", "Present")),
    test = factor(c(rep("Positive", 24), rep("Negative", 6), rep("Positive", 5), rep("Negative", 45)),
                  levels = c("Negative", "Positive")))

pl_decision_args <- function(...) pl_with(list(
    data = pl_decision_data(), gold = "gold", goldPositive = "Present",
    newtest = "test", testPositive = "Positive", goldNegative = NULL, testNegative = NULL), list(...))

pl_decision <- function(...) do.call(ClinicoPath::decision, pl_decision_args(...))

pl_combine_data <- function() data.frame(
    gold = factor(c(rep("pos", 40), rep("neg", 60)), levels = c("neg", "pos")),
    t1 = factor(c(rep("pos", 32), rep("neg", 8), rep("pos", 9), rep("neg", 51)), levels = c("neg", "pos")),
    t2 = factor(c(rep("pos", 28), rep("neg", 12), rep("pos", 3), rep("neg", 57)), levels = c("neg", "pos")),
    t3 = factor(c(rep(c("pos", "neg"), 20), rep(c("neg", "pos", "neg"), 20)), levels = c("neg", "pos")))

pl_combine_args <- function(...) pl_with(list(
    data = pl_combine_data(), gold = "gold", goldPositive = "pos",
    test1 = "t1", test1Positive = "pos", test2 = "t2", test2Positive = "pos",
    test3 = "t3", test3Positive = "pos", showIndividual = TRUE), list(...))

pl_combine <- function(...) do.call(ClinicoPath::decisioncombine, pl_combine_args(...))

pl_error_style <- "rgba(220, 38, 38"

test_that("every positive Level option allows none and has no default (schema)", {
    for (case in list(list("decision", c("goldPositive", "testPositive")),
                      list("decisioncombine", c("goldPositive", "test1Positive",
                                                "test2Positive", "test3Positive")))) {
        lv <- pl_level_options(case[[1]])
        positive <- Filter(function(o) endsWith(o$name, "Positive"), lv)
        expect_setequal(vapply(positive, `[[`, "", "name"), case[[2]])
        for (o in positive) {
            expect_true(isTRUE(o$allowNone), info = paste(case[[1]], o$name))
            expect_null(o$default, info = paste(case[[1]], o$name))
        }
    }
})

test_that("decision: an unset positive level is an INFO pending state with counts", {
    slots <- list(
        goldPositive = list(title = "Choose the Reference Positive Level",
                            counts = "Absent (n = 50), Present (n = 30)"),
        testPositive = list(title = "Choose the Test Positive Level",
                            counts = "Negative (n = 51), Positive (n = 29)"))
    for (slot in names(slots)) for (unset in list(NULL, "", NA_character_)) {
        info <- paste(slot, deparse(unset))
        expect_no_error(res <- do.call(ClinicoPath::decision,
                                       do.call(pl_decision_args, stats::setNames(list(unset), slot))))
        notices <- res$notices$content
        expect_match(notices, slots[[slot]]$title, fixed = TRUE, info = info)
        expect_match(notices, slots[[slot]]$counts, fixed = TRUE, info = info)
        expect_no_match(notices, pl_error_style, fixed = TRUE, info = info)
        expect_no_match(notices, "not found", fixed = TRUE, info = info)
        expect_true(all(is.na(res$ratioTable$asDF[, c("Sens", "Spec", "PPV", "NPV", "LRP", "LRN")])), info = info)
        expect_true(all(is.na(res$cTable$asDF[, c("GP", "GN", "Total")])), info = info)
        expect_true(all(is.na(res$nTable$asDF$TotalPop)), info = info)
        expect_null(res$plot1$state, info = info)
    }
    # Only the slot left unset is reported.
    res <- pl_decision(testPositive = NULL)
    expect_no_match(res$notices$content, "Choose the Reference Positive Level", fixed = TRUE)
})

test_that("decision: ready -> pending -> ready on one object leaves no stale numbers", {
    d <- pl_decision_data()
    opts <- ClinicoPath:::decisionOptions$new(
        gold = "gold", goldPositive = "Present", newtest = "test", testPositive = "Positive",
        goldNegative = NULL, testNegative = NULL, ci = TRUE, fagan = TRUE, od = TRUE,
        showNaturalLanguage = TRUE, showReportTemplate = TRUE, showMisclassified = TRUE)
    output <- opts$option("saveClassifications")
    output$value <- list(value = TRUE)
    a <- ClinicoPath:::decisionClass$new(options = opts, data = d)
    r <- a$results
    saved <- function() as.character(r$saveClassifications$.__enclos_env__$private$.values[[1]])

    a$run()
    expect_equal(r$ratioTable$asDF$Sens, 0.8)
    expect_equal(r$cTable$asDF$GP, c(24, 6, 30))
    first <- list(ratio = r$ratioTable$asDF, ctable = r$cTable$asDF, raw = r$rawContingency$asDF,
                  epir = r$epirTable_ratio$asDF, cm = r$confusionMatrixSummary$asDF,
                  fp = r$falsePositiveTable$asDF, saved = saved())
    expect_false(is.null(r$plot1$state))
    expect_true(nzchar(r$naturalLanguageSummary$content))
    expect_equal(sum(first$saved == "True Positive"), 24)

    gp <- opts$option("goldPositive")
    gp$value <- NULL
    expect_no_error(a$run())
    expect_match(r$notices$content, "Choose the Reference Positive Level", fixed = TRUE)
    expect_no_match(r$notices$content, pl_error_style, fixed = TRUE)
    for (tbl in c("cTable", "nTable", "ratioTable", "epirTable_ratio", "epirTable_number",
                  "rawContingency", "confusionMatrixSummary")) {
        df <- r[[tbl]]$asDF
        numeric_cols <- vapply(df, is.numeric, logical(1))
        expect_true(all(is.na(df[, numeric_cols])), info = tbl)
        expect_length(r[[tbl]]$notes, 0)
    }
    expect_equal(r$rawContingency$rowCount, 3L)          # scaffold kept, values blanked
    expect_equal(r$confusionMatrixSummary$rowCount, 4L)
    for (tbl in c("rawCounts", "falsePositiveTable", "falseNegativeTable"))
        expect_equal(r[[tbl]]$rowCount, 0L, info = tbl)
    expect_null(r$plot1$state)
    for (html in c("naturalLanguageSummary", "reportTemplate", "misclassificationInterpretation",
                   "misclassifiedHeading", "missingDataSummary"))
        expect_identical(r[[html]]$content, "", info = html)
    expect_length(saved(), nrow(d))
    expect_true(all(is.na(saved())))

    gp$value <- "Present"
    expect_no_error(a$run())
    expect_no_match(r$notices$content, "Choose the Reference Positive Level", fixed = TRUE)
    expect_equal(r$ratioTable$asDF, first$ratio)
    expect_equal(r$cTable$asDF, first$ctable)
    expect_equal(r$rawContingency$asDF, first$raw)
    expect_equal(r$epirTable_ratio$asDF, first$epir)
    expect_equal(r$confusionMatrixSummary$asDF, first$cm)
    expect_equal(r$falsePositiveTable$asDF, first$fp)
    expect_identical(saved(), first$saved)
    expect_false(is.null(r$plot1$state))
})

test_that("decision: a non-empty level that is not in the data is still an error", {
    res <- do.call(diagnostic_error_results,
        c(list(analysis = "decision"), pl_decision_args(goldPositive = "Malignant")))
    expect_match(res$notices$content, "not found in gold standard variable", fixed = TRUE)
    expect_match(res$notices$content, pl_error_style, fixed = TRUE)
    expect_no_match(res$notices$content, "Choose the Reference Positive Level", fixed = TRUE)
    res <- do.call(diagnostic_error_results,
        c(list(analysis = "decision"), pl_decision_args(testPositive = "Positiv")))
    expect_match(res$notices$content, "not found in test variable", fixed = TRUE)
    expect_true(is.na(res$ratioTable$asDF$Sens))
})

test_that("decision: the syntax of a pending analysis is runnable R", {
    d <- pl_decision_data()
    opts <- ClinicoPath:::decisionOptions$new(
        gold = "gold", goldPositive = NULL, newtest = "test", testPositive = NULL,
        goldNegative = NULL, testNegative = NULL)
    a <- ClinicoPath:::decisionClass$new(options = opts, data = d)
    a$run()
    src <- a$asSource()
    expect_match(src, "goldPositive = NULL", fixed = TRUE)
    expect_match(src, "testPositive = NULL", fixed = TRUE)
    # Evaluates the module's own generated syntax (no user input) to prove it runs.
    expect_no_error(res <- eval(parse(text = src)[[1]], envir = list(data = d)))
    expect_match(res$notices$content, "Choose the Test Positive Level", fixed = TRUE)
})

test_that("decisioncombine: every unset positive level of a selected variable is pending", {
    slots <- list(
        goldPositive = list(title = "Choose the Disease Present Level", counts = "neg (n = 60), pos (n = 40)"),
        test1Positive = list(title = "Choose the Positive Level for Test 1", counts = "neg (n = 59), pos (n = 41)"),
        test2Positive = list(title = "Choose the Positive Level for Test 2", counts = "neg (n = 69), pos (n = 31)"),
        test3Positive = list(title = "Choose the Positive Level for Test 3", counts = "neg (n = 60), pos (n = 40)"))
    for (slot in names(slots)) for (unset in list(NULL, "", NA_character_)) {
        info <- paste(slot, deparse(unset))
        expect_no_error(res <- do.call(ClinicoPath::decisioncombine,
                                       do.call(pl_combine_args, stats::setNames(list(unset), slot))))
        notices <- res$notices$content
        expect_match(notices, slots[[slot]]$title, fixed = TRUE, info = info)
        expect_match(notices, slots[[slot]]$counts, fixed = TRUE, info = info)
        expect_no_match(notices, pl_error_style, fixed = TRUE, info = info)
        expect_equal(res$combinationTable$rowCount, 0L, info = info)
        expect_true(all(is.na(res$individualTest1$test1Stats$asDF$estimate)), info = info)
        expect_true(all(is.na(res$individualTest1$test1Contingency$asDF$goldPos)), info = info)
    }
    # Two slots unset: both are named.
    res <- pl_combine(goldPositive = NULL, test3Positive = NULL)
    expect_match(res$notices$content, "Choose the Disease Present Level", fixed = TRUE)
    expect_match(res$notices$content, "Choose the Positive Level for Test 3", fixed = TRUE)
})

test_that("decisioncombine: an optional test that is not selected never makes it pending", {
    res <- pl_combine(test2 = NULL, test2Positive = NULL, test3 = NULL, test3Positive = NULL)
    expect_no_match(res$notices$content, "Positive Level", fixed = TRUE)
    expect_equal(res$individualTest1$test1Stats$asDF$estimate[1], 32 / 40)
    res <- pl_combine(test3 = NULL, test3Positive = NULL)
    expect_no_match(res$notices$content, "Positive Level", fixed = TRUE)
    expect_gt(res$combinationTable$rowCount, 0L)
})

test_that("decisioncombine: ready -> pending -> ready on one object leaves no stale numbers", {
    opts <- ClinicoPath:::decisioncombineOptions$new(
        gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos", test3 = "t3", test3Positive = "pos",
        showIndividual = TRUE, showRecommendation = TRUE, showFrequency = TRUE, showBarPlot = TRUE)
    a <- ClinicoPath:::decisioncombineClass$new(options = opts, data = pl_combine_data())
    r <- a$results

    a$run()
    first <- list(comb = r$combinationTable$asDF, ci = r$combinationTableCI$asDF,
                  stats = r$individualTest2$test2Stats$asDF, rec = r$recommendationTable$asDF)
    expect_gt(nrow(first$comb), 0)
    expect_true(isTRUE(r$barPlot$state$valid))

    t2 <- opts$option("test2Positive")
    t2$value <- NULL
    expect_no_error(a$run())
    expect_match(r$notices$content, "Choose the Positive Level for Test 2", fixed = TRUE)
    for (tbl in c("combinationTable", "combinationTableCI", "combinationTableCIRatios",
                  "goldFreqTable", "crossTabTable"))
        expect_equal(r[[tbl]]$rowCount, 0L, info = tbl)
    for (i in 1:3) {
        group <- r[[paste0("individualTest", i)]]
        expect_true(all(is.na(group[[paste0("test", i, "Stats")]]$asDF$estimate)), info = i)
    }
    expect_true(all(is.na(r$recommendationTable$asDF$youden)))
    expect_false(isTRUE(r$barPlot$state$valid))

    t2$value <- "pos"
    expect_no_error(a$run())
    expect_no_match(r$notices$content, "Choose the Positive Level for Test 2", fixed = TRUE)
    expect_equal(r$combinationTable$asDF, first$comb)
    expect_equal(r$combinationTableCI$asDF, first$ci)
    expect_equal(r$individualTest2$test2Stats$asDF, first$stats)
    expect_equal(r$recommendationTable$asDF, first$rec)
    expect_true(isTRUE(r$barPlot$state$valid))
})

test_that("decisioncombine: a non-empty level that is not in the data is still an error", {
    res <- do.call(diagnostic_error_results,
        c(list(analysis = "decisioncombine"), pl_combine_args(test1Positive = "Positive")))
    expect_match(res$notices$content, "Missing Level", fixed = TRUE)
    expect_match(res$notices$content, pl_error_style, fixed = TRUE)
    expect_no_match(res$notices$content, "Choose the Positive Level for Test 1", fixed = TRUE)
    expect_equal(res$combinationTable$rowCount, 0L)
})

test_that("decisioncombine: the syntax of a pending analysis is runnable R", {
    d <- pl_combine_data()
    opts <- ClinicoPath:::decisioncombineOptions$new(
        gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = NULL,
        test2 = NULL, test2Positive = NULL, test3 = NULL, test3Positive = NULL)
    a <- ClinicoPath:::decisioncombineClass$new(options = opts, data = d)
    a$run()
    src <- a$asSource()
    for (nm in c("test1Positive", "test2Positive", "test3Positive"))
        expect_match(src, paste0(nm, " = NULL"), fixed = TRUE)
    # Evaluates the module's own generated syntax (no user input) to prove it runs.
    expect_no_error(res <- eval(parse(text = src)[[1]], envir = list(data = d)))
    expect_match(res$notices$content, "Choose the Positive Level for Test 1", fixed = TRUE)
})
