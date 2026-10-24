# Regression tests from the `psychopdaROC` release review.
#
# Each case corresponds to a defect confirmed in the shipped code. AUC, the DeLong test and the
# optimal cutpoint are checked against pROC and cutpointr, not against the module's own
# arithmetic.

ps_data <- function() {
    data(psychopdaROC_test, package = "ClinicoPath", envir = environment())
    get("psychopdaROC_test", envir = environment())
}

run_ps <- function(...) {
    args <- utils::modifyList(
        list(classVar = "disease_status", positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL),
        list(...))
    if (!("run" %in% names(args))) args$run <- TRUE
    do.call(ClinicoPath::psychopdaROC, args)
}

# read a keyed table note, or "(none)"
tnote <- function(table, key) {
    n <- table$.__enclos_env__$private$.notes
    if (!(key %in% names(n))) return("(none)")
    get("note", envir = n[[key]])
}

ps_private <- function(...) {
    o <- do.call(ClinicoPath:::psychopdaROCOptions$new, utils::modifyList(list(run = TRUE), list(...), keep.null = TRUE))
    a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = ps_data())
    a$init()
    list(a = a, p = a$.__enclos_env__$private)
}

# 3-level recode of the bundled data: half of the Disease cases become "Severe"
ps_data3 <- function() {
    d <- ps_data()
    lv <- as.character(d$disease_status)
    set.seed(5)
    idx <- which(lv == "Disease")
    lv[sample(idx, length(idx) %/% 2)] <- "Severe"
    d$ds3 <- factor(lv, levels = c("Healthy", "Disease", "Severe"))
    d
}

# Mann-Whitney AUC of x for the rows where `case` is TRUE against those where it is FALSE; ties
# count one half. Rows with NA in either are dropped. An independent oracle for the AUC column.
mw_auc <- function(x, case) {
    ok <- !is.na(x) & !is.na(case)
    cs <- x[ok & case]
    ct <- x[ok & !case]
    mean(outer(cs, ct, ">") + 0.5 * outer(cs, ct, "=="))
}


test_that("AUC and the DeLong test match pROC", {
    skip_if_not_installed("pROC")
    d <- ps_data()
    set.seed(7)
    d$marker2 <- d$biomarker + rnorm(nrow(d), 0, 1.5)

    res <- run_ps(data = d, dependentVars = c("biomarker", "marker2"), delongTest = TRUE)
    auc <- res$aucSummaryTable$asDF

    for (v in c("biomarker", "marker2")) {
        pr <- suppressMessages(pROC::roc(d$disease_status, d[[v]],
                                         levels = c("Healthy", "Disease"),
                                         direction = "<", quiet = TRUE))
        expect_equal(auc$auc[auc$variable == v], as.numeric(pr$auc), tolerance = 1e-6, label = v)
    }

    r1 <- suppressMessages(pROC::roc(d$disease_status, d$biomarker,
                                     levels = c("Healthy", "Disease"), direction = "<", quiet = TRUE))
    r2 <- suppressMessages(pROC::roc(d$disease_status, d$marker2,
                                     levels = c("Healthy", "Disease"), direction = "<", quiet = TRUE))
    ref <- suppressMessages(pROC::roc.test(r1, r2, method = "delong"))
    dl <- res$delongComparisonTable$asDF
    expect_equal(as.numeric(dl$z[1]), as.numeric(ref$statistic), tolerance = 1e-6)
    expect_equal(as.numeric(dl$p[1]), ref$p.value, tolerance = 1e-6)
})


test_that("tables do not accumulate rows across re-runs", {
    # 21 addRow() calls against 2 deleteRows(). jamovi re-runs .run() on the SAME object on every
    # option change, so 14 of the 16 tables doubled each time -- measured 1 -> 2 -> 3 rows per
    # predictor and the decision curve going 40 -> 80 -> 120.
    d <- ps_data()
    set.seed(7)
    d$marker2 <- d$biomarker + rnorm(nrow(d), 0, 1.5)
    o <- ClinicoPath:::psychopdaROCOptions$new(
        run = TRUE, dependentVars = c("biomarker", "marker2"), classVar = "disease_status",
        positiveClass = "Disease", negativeClass = "Healthy", refVar = "biomarker",
        sensSpecTable = TRUE, delongTest = TRUE, partialAUC = TRUE,
        clinicalUtilityAnalysis = TRUE, effectSizeAnalysis = TRUE, powerAnalysis = TRUE)
    a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = d)
    p <- a$.__enclos_env__$private
    a$init()

    tbls <- Filter(function(n) inherits(try(a$results[[n]], silent = TRUE), "Table"),
                   names(a$results))
    counts <- lapply(1:3, function(i) {
        p$.run()
        vapply(tbls, function(t) tryCatch(a$results[[t]]$rowCount, error = function(e) NA_real_),
               numeric(1))
    })
    expect_equal(counts[[2]], counts[[1]])
    expect_equal(counts[[3]], counts[[1]])
    expect_gt(sum(counts[[1]], na.rm = TRUE), 0)   # something was actually populated
})


test_that("manual run mode keeps its results when an option changes", {
    # The table clearing must sit AFTER the manual-run gate. jamovi still calls .run() on every
    # option change in manual mode, so clearing before the gate would empty the user's computed
    # tables and then return without recomputing -- the opposite of what manual mode is for.
    d <- ps_data()
    o <- ClinicoPath:::psychopdaROCOptions$new(
        dependentVars = "biomarker", classVar = "disease_status",
        positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, manualRun = TRUE, run = TRUE)
    a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = d)
    p <- a$.__enclos_env__$private
    a$init()

    p$.run()                                   # user clicked Run
    filled <- a$results$aucSummaryTable$rowCount
    expect_gt(filled, 0)

    op <- o$option("run"); op$value <- FALSE    # user now edits an option; Run not clicked again
    p$.run()
    expect_equal(a$results$aucSummaryTable$rowCount, filled)

    # and the source order is the guarantee, so pin it
    src <- readLines("../../R/psychopdaROC.b.R", warn = FALSE)
    gate <- grep("MANUAL RUN GATE", src)[1]
    clear <- grep("private\\$\\.clearTables\\(\\)", src)[1]
    expect_true(clear > gate)
})


test_that("an unset positive class waits for a choice; no level is guessed", {
    # The fallback first took the FIRST level -- for Healthy/Disease, Negative/Positive and
    # Control/Case that is the NEGATIVE group, so the whole analysis ran inverted (AUC 0.1001
    # where naming Disease gives 0.8999) -- and later guessed the last level. Neither order means
    # anything. An unset class is now the normal first step: the Outcome coding table lists the
    # levels, an INFO panel asks for the choice, nothing is computed, and nothing is rejected.
    d <- ps_data()
    expect_equal(levels(d$disease_status), c("Healthy", "Disease"))

    unset <- expect_no_error(run_ps(data = d, dependentVars = "biomarker", positiveClass = "", negativeClass = "Healthy"))
    expect_equal(unset$aucSummaryTable$rowCount, 0)
    oc <- unset$outcomeCoding$asDF
    expect_equal(oc$level, c("Healthy", "Disease"))
    expect_equal(as.integer(oc$n), as.integer(table(d$disease_status)))
    expect_equal(oc$role, rep("Not assigned yet", 2))
    expect_match(tnote(unset$outcomeCoding, "unset"), "levels are never chosen from their order", fixed = TRUE)
    expect_equal(tnote(unset$outcomeCoding, "positive"), "(none)")
    expect_match(unset$runSummary$content, "Choose the positive and negative classes", fixed = TRUE)
    expect_match(unset$runSummary$content, "<p>", fixed = TRUE)   # inherits the result theme

    # naming a level runs it; naming the other level legitimately inverts it (the user's choice)
    named <- run_ps(data = d, dependentVars = "biomarker", positiveClass = "Disease", negativeClass = "Healthy")
    inv <- run_ps(data = d, dependentVars = "biomarker", positiveClass = "Healthy", negativeClass = "Disease")
    expect_equal(named$aucSummaryTable$asDF$auc[1],
                 mw_auc(d$biomarker, d$disease_status == "Disease"), tolerance = 1e-9)
    expect_equal(inv$aucSummaryTable$asDF$auc[1],
                 1 - named$aucSummaryTable$asDF$auc[1], tolerance = 1e-9)
    # the retired "a positive class was assumed" note never appears
    for (r in list(unset, named, inv))
        expect_equal(tnote(r$aucSummaryTable, "assumed_positive_class"), "(none)")
})


test_that("the positive class is resolved in exactly one place", {
    # .prepareVarData, .run() and the procedure notes each worked it out separately. When they
    # disagreed the data was dichotomised one way and scored the other, giving an AUC of 0.
    backend <- paste(readLines("../../R/psychopdaROC.b.R", warn = FALSE), collapse = "\n")
    # no surviving "first level" fallbacks
    expect_false(grepl("levels(factor(classVar))[1]", backend, fixed = TRUE))
    expect_equal(length(gregexpr(".resolveOutcomeClasses = function", backend, fixed = TRUE)[[1]]), 1L)

    # and it never guesses: an unset class stays unset, a chosen level is used as chosen
    cls <- factor(c("Healthy", "Disease"), levels = c("Healthy", "Disease"))
    unset <- ps_private(dependentVars = "biomarker", classVar = "disease_status",
                        positiveClass = "", negativeClass = "Healthy", refVar = NULL)$p$.resolveOutcomeClasses(cls)
    expect_identical(unset$status, "unset")
    expect_null(unset$positive)
    chosen <- ps_private(dependentVars = "biomarker", classVar = "disease_status",
                         positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL)$p$.resolveOutcomeClasses(cls)
    expect_identical(chosen$status, "selected")
    expect_identical(chosen$positive, "Disease")
})


test_that("the reported cutpoint is the one that maximises the metric", {
    # tol_metric defaulted to 0.05 while the underlying cutpointr package uses 1e-06. Every
    # cutpoint within 0.05 Youden of the best was treated as equivalent and averaged: on this
    # data 39 of 200 thresholds spanning 52.1 to 67.7, giving a cutpoint with 84.5% sensitivity
    # in place of the 94.4% available at the true optimum.
    skip_if_not_installed("cutpointr")
    d <- ps_data()

    ref <- cutpointr::cutpointr(d, biomarker, disease_status, pos_class = "Disease",
                                method = cutpointr::maximize_metric,
                                metric = cutpointr::youden, tol_metric = 1e-06, silent = TRUE)

    got <- run_ps(data = d, dependentVars = "biomarker", tol_metric = 0)$resultsTable[[1]]$asDF
    expect_equal(as.numeric(got$cutpoint[1]), as.numeric(ref$optimal_cutpoint), tolerance = 1e-6)
    expect_equal(as.numeric(got$sensitivity[1]), as.numeric(ref$sensitivity), tolerance = 1e-6)
    expect_equal(as.numeric(got$specificity[1]), as.numeric(ref$specificity), tolerance = 1e-6)

    # the shipped default must not be the wide tolerance any more
    a_yaml <- paste(readLines("../../jamovi/psychopdaROC.a.yaml", warn = FALSE), collapse = "\n")
    blk <- regmatches(a_yaml, regexpr("(?s)    - name: tol_metric\\n.*?(?=\\n    - name: )",
                                      a_yaml, perl = TRUE))
    expect_match(blk, "default: 0.000001", fixed = TRUE)
})


test_that("a metric tolerance that averages cutpoints says so", {
    d <- ps_data()
    wide <- run_ps(data = d, dependentVars = "biomarker", tol_metric = 0.05)
    expect_match(tnote(wide$resultsTable[[1]], "metric_tolerance"), "not</i> necessarily")
    expect_match(tnote(wide$resultsTable[[1]], "metric_tolerance"), "maximizes the metric")

    tight <- run_ps(data = d, dependentVars = "biomarker", tol_metric = 0)
    expect_equal(tnote(tight$resultsTable[[1]], "metric_tolerance"), "(none)")
})


test_that("sensitivity, specificity, PPV and NPV agree with the 2x2 at the reported cutpoint", {
    d <- ps_data()
    rt <- run_ps(data = d, dependentVars = "biomarker", tol_metric = 0)$resultsTable[[1]]$asDF
    th <- as.numeric(rt$cutpoint[1])
    y <- d$disease_status == "Disease"
    pred <- d$biomarker >= th
    tp <- sum(pred & y); fp <- sum(pred & !y); fn <- sum(!pred & y); tn <- sum(!pred & !y)

    expect_equal(as.numeric(rt$sensitivity[1]), tp / (tp + fn), tolerance = 1e-9)
    expect_equal(as.numeric(rt$specificity[1]), tn / (tn + fp), tolerance = 1e-9)
    expect_equal(as.numeric(rt$ppv[1]),         tp / (tp + fp), tolerance = 1e-9)
    expect_equal(as.numeric(rt$npv[1]),         tn / (tn + fn), tolerance = 1e-9)
})


test_that("refVar is an optional wrapper argument with a NULL default", {
    # Reference Test for IDI/NRI is a type: Variable option (default null). It used to be a Level of
    # the class variable: a required argument that could never name a test column.
    expect_true("refVar" %in% names(formals(ClinicoPath::psychopdaROC)))
    expect_null(formals(ClinicoPath::psychopdaROC)$refVar)

    d <- ps_data()
    expect_no_error(ClinicoPath::psychopdaROC(data = d, dependentVars = "biomarker",
                                              classVar = "disease_status", positiveClass = "Disease", negativeClass = "Healthy"))
    expect_no_error(run_ps(data = d, dependentVars = "biomarker"))
})


test_that("a multi-level class variable waits for a positive class: no guess, no error", {
    # There is no defensible guess: on this 3-level recode of the bundled data, taking "Disease"
    # yields different estimands. Both selected levels are explicit; unselected levels are excluded.
    d <- ps_data3()
    pending <- expect_no_error(run_ps(data = d, dependentVars = "biomarker", classVar = "ds3",
                                      positiveClass = "", negativeClass = "Healthy"))
    expect_equal(pending$aucSummaryTable$rowCount, 0)
    expect_equal(pending$outcomeCoding$asDF$role, rep("Not assigned yet", 3))
    expect_equal(as.integer(pending$outcomeCoding$asDF$n), as.integer(table(d$ds3)))
    expect_match(pending$runSummary$content, "Choose the positive and negative classes", fixed = TRUE)

    severe <- expect_no_error(run_ps(data = d, dependentVars = "biomarker", classVar = "ds3",
                                     positiveClass = "Severe", negativeClass = "Healthy"))
    expect_equal(severe$aucSummaryTable$asDF$auc[1], mw_auc(d$biomarker[d$ds3 != "Disease"], d$ds3[d$ds3 != "Disease"] == "Severe"),
                 tolerance = 1e-9)
})


test_that("the positive class is never taken from data row order", {
    # A fallback inside .deLongTest used unique(classVar)[1] -- the first value in DATA ORDER, so
    # which class counted as positive depended on how the rows happened to be sorted, and it
    # disagreed with every other resolution in the analysis. It also warned via warning(), which
    # the jamovi GUI never displays.
    # strip comments first -- the fix's own explanatory comment names the old pattern
    code <- sub("#.*$", "", readLines("../../R/psychopdaROC.b.R", warn = FALSE))
    code <- paste(code, collapse = "\n")
    expect_false(grepl("unique(classVar)[1]", code, fixed = TRUE))
    expect_false(grepl("Using first unique value instead", code, fixed = TRUE))

    # row order must not change the answer
    d <- ps_data()
    a <- run_ps(data = d, dependentVars = "biomarker")$aucSummaryTable$asDF$auc[1]
    b <- run_ps(data = d[order(d$disease_status, decreasing = TRUE), ],
                dependentVars = "biomarker")$aucSummaryTable$asDF$auc[1]
    expect_equal(a, b, tolerance = 1e-12)
})


test_that("psychopdaROC agrees with enhancedROC once both are told the same direction", {
    # The two analyses sit in the same menu and their direction defaults differ -- enhancedROC
    # defaults to data-driven "auto", psychopdaROC to a fixed ">=". On an inverted marker the
    # out-of-the-box answers are 0.899880 and 0.100120 for the same data and the same
    # positiveClass. That is a defaults mismatch, not an arithmetic one: told the same thing they
    # agree exactly, which is what this pins.
    d <- ps_data()
    d$inv <- -d$biomarker

    e <- ClinicoPath::enhancedROC(data = d, outcome = "disease_status",
                                  positiveClass = "Disease", predictors = "inv",
                                  direction = "lower")$results$aucSummary$asDF$auc[1]
    p <- run_ps(data = d, dependentVars = "inv", direction = "<=")$aucSummaryTable$asDF$auc[1]
    expect_equal(e, p, tolerance = 1e-9)

    e2 <- ClinicoPath::enhancedROC(data = d, outcome = "disease_status",
                                   positiveClass = "Disease", predictors = "biomarker",
                                   direction = "higher")$results$aucSummary$asDF$auc[1]
    p2 <- run_ps(data = d, dependentVars = "biomarker", direction = ">=")$aucSummaryTable$asDF$auc[1]
    expect_equal(e2, p2, tolerance = 1e-9)
    expect_equal(e2, 0.899880, tolerance = 1e-5)
})


test_that("the Hanley-McNeil fallback does not promise conservative intervals", {
    # Width in one bundled fixture does not establish coverage or a universal ordering.
    backend <- paste(readLines("../../R/psychopdaROC.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("may produce narrower confidence intervals", backend, fixed = TRUE))
    expect_true(grepl("intervals need not be conservative", backend, fixed = TRUE))

    # and the underlying formula really is wider here
    d <- ps_data()
    auc <- run_ps(data = d, dependentVars = "biomarker")$aucSummaryTable$asDF$auc[1]
    n_pos <- sum(d$disease_status == "Disease")
    n_neg <- sum(d$disease_status != "Disease")
    q1 <- auc / (2 - auc)
    q2 <- 2 * auc^2 / (1 + auc)
    hm <- sqrt((auc * (1 - auc) + (n_pos - 1) * (q1 - auc^2) +
                (n_neg - 1) * (q2 - auc^2)) / (n_pos * n_neg))
    skip_if_not_installed("pROC")
    dl <- sqrt(pROC::var(suppressMessages(pROC::roc(d$disease_status, d$biomarker,
                levels = c("Healthy", "Disease"), direction = "<", quiet = TRUE))))
    expect_gt(hm, dl)
})


test_that("the output states which way the marker was read", {
    # enhancedROC and psychopdaROC sit in the same menu and their direction defaults differ, so
    # the same column comes back as AUC 0.8999 in one and 0.1001 in the other. Neither is wrong,
    # but nothing on screen said why. Both now carry the same plain sentence naming the reading.
    d <- ps_data()
    d$inv <- -d$biomarker

    n <- tnote(run_ps(data = d, dependentVars = "inv")$aucSummaryTable, "direction_used")
    expect_match(n, "HIGHER values were taken to indicate Disease")
    expect_match(n, "Classification Direction")
    expect_match(n, "reversed")

    n2 <- tnote(run_ps(data = d, dependentVars = "inv", direction = "<=")$aucSummaryTable,
                "direction_used")
    expect_match(n2, "LOWER values were taken to indicate Disease")

    # the below-0.5 warning names the current setting and the remedy
    w <- tnote(run_ps(data = d, dependentVars = "inv")$aucSummaryTable, "auc_below_chance")
    expect_match(w, "currently")
    expect_match(w, "1 minus the value shown")
    expect_false(grepl("Verify the 'Classification Direction' option", w, fixed = TRUE))

    # Both send the user to Positive Class BEFORE Direction: naming the wrong level reverses the
    # reading as well. Flipping Direction re-estimates the cutpoint, sensitivity and specificity
    # (they are not "swapped"), and notes show entities literally, so symbols stay raw.
    expect_match(n, "first check Positive Class: is Disease the level", fixed = TRUE)
    expect_match(w, "First check Positive Class: is Disease the level", fixed = TRUE)
    expect_lt(regexpr("check Positive Class", n, fixed = TRUE),
              regexpr("switch Classification Direction", n, fixed = TRUE))
    expect_lt(regexpr("check Positive Class", w, fixed = TRUE),
              regexpr("switching it to", w, fixed = TRUE))
    for (txt in c(n, w)) {
        expect_match(txt, "re-estimate", fixed = TRUE)
        expect_false(grepl("swapped|assumed|&lt;|&gt;|&amp;", txt))
    }
    expect_match(w, "switching it to \"<=\"", fixed = TRUE)
})


test_that("every active option is read and retired options are hidden", {
    a_yaml <- readLines("../../jamovi/psychopdaROC.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    schema <- yaml::read_yaml("../../jamovi/psychopdaROC.a.yaml")
    retired <- c("correlationROCs", "metaAnalysisMethod", "heterogeneityTest",
      "forestPlot", "overrideMetaAnalysisWarning")
    for (option in Filter(function(x) x$name %in% retired, schema$options))
        expect_true(isTRUE(option$hidden), info = option$name)
    declared <- setdiff(declared, c("data", retired))
    backend <- paste(readLines("../../R/psychopdaROC.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})


test_that("the DeLong fallback preserves the declared score direction", {
    # A fallback must preserve the same below-chance estimand as the primary path.
    skip_if_not_installed("pROC")
    set.seed(42); n <- 120
    d <- data.frame(cls = rep(c("pos", "neg"), each = n / 2))
    d$good <- c(rnorm(n / 2, 1), rnorm(n / 2, 0))   # higher in positives
    d$flip <- c(rnorm(n / 2, 0), rnorm(n / 2, 1))   # lower  in positives -> AUC < 0.5

    o <- ClinicoPath:::psychopdaROCOptions$new(run = TRUE, dependentVars = c("good", "flip"),
             classVar = "cls", positiveClass = "pos", negativeClass = "neg", direction = ">=", delongTest = TRUE)
    a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = d)
    res <- a$.__enclos_env__$private$.deLongTest(
        data = d[, c("good", "flip")], classVar = d$cls, positiveClass = "pos")

    expect_equal(res$inverted, character(0))

    honest <- as.numeric(pROC::auc(pROC::roc(d$cls, d$flip, levels = c("neg", "pos"),
                                             direction = "<", quiet = TRUE)))
    expect_lt(honest, 0.5)                                   # the marker really is inverted
    expect_equal(unname(res$AUC[2, "AUC"]), honest, tolerance = 1e-6)

    # a marker that needs no flip is not named
    res2 <- a$.__enclos_env__$private$.deLongTest(
        data = d[, c("good", "good")], classVar = d$cls, positiveClass = "pos")
    expect_equal(res2$inverted, character(0))

    primary <- a$.__enclos_env__$private$.enhancedDelongTest(
        data = d[, c("good", "flip")], classVar = d$cls, pos_class = "pos")
    expect_equal(res$difference[, "AUC Difference"], primary$difference[, "AUC Difference"], tolerance = 1e-12)
    expect_equal(res$difference[, "P.Value"], primary$difference[, "P.Value"], tolerance = 1e-12)
    expect_equal(res$covariance, primary$covariance, tolerance = 1e-12, ignore_attr = TRUE)

})


test_that("DeLong's test warns when a class is too small for its asymptotics", {
    # DeLong's variance is a large-sample approximation. With few cases in a class the
    # p-value and CI are too narrow, and the table used to print them with no caveat.
    # The guard sits in the render path so it covers the pROC path and the fallback alike.
    skip_if_not_installed("pROC")
    note_keys <- function(npos, nneg) {
        set.seed(7)
        d <- data.frame(cls = c(rep("pos", npos), rep("neg", nneg)))
        d$m1 <- c(rnorm(npos, 1),   rnorm(nneg, 0))
        d$m2 <- c(rnorm(npos, 0.6), rnorm(nneg, 0))
        o <- ClinicoPath:::psychopdaROCOptions$new(run = TRUE, dependentVars = c("m1", "m2"),
                 classVar = "cls", positiveClass = "pos", negativeClass = "neg", direction = ">=", delongTest = TRUE)
        a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = d)
        a$init(); a$.__enclos_env__$private$.run()
        names(a$results$delongComparisonTable$notes)
    }
    expect_true("delong_small_sample"  %in% note_keys(8, 40))
    expect_true("delong_small_sample"  %in% note_keys(9, 40))
    expect_true("delong_small_sample"  %in% note_keys(40, 8))   # the negative class counts too
    expect_false("delong_small_sample" %in% note_keys(10, 40))  # boundary is exclusive
    expect_false("delong_small_sample" %in% note_keys(40, 40))
    # the methodology note is always present, so an empty note set would not fake a pass
    expect_true("delong_method" %in% note_keys(40, 40))
})


# --- 2026-09-03 /fix-function: stale per-run state, late warnings, NULL positive class, NA strata ---

ps_private_data <- function(d, ...) {
    o <- do.call(ClinicoPath:::psychopdaROCOptions$new, utils::modifyList(list(run = TRUE), list(...), keep.null = TRUE))
    a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = d)
    a$init()
    list(a = a, p = a$.__enclos_env__$private)
}

test_that("a second run with smoothing, the threshold table and the combined criterion plot does not crash", {
    skip_if_not_installed("pROC")
    pr <- ps_private(dependentVars = "biomarker", classVar = "disease_status", positiveClass = "Disease", negativeClass = "Healthy",
                     refVar = NULL, rocSmoothingMethod = "binormal", showThresholdTable = TRUE,
                     combinePlots = TRUE, showCriterionPlot = TRUE)
    expect_no_error(pr$p$.run())
    n1 <- nrow(pr$a$results$thresholdTable$asDF)
    # used to fail with "numbers of columns of arguments do not match": the previous run's
    # biomarker_smooth frame was still in private$.rocDataList when the criterion plot rbind() ran
    expect_no_error(pr$p$.run())
    expect_setequal(names(pr$p$.rocDataList), "biomarker")
    expect_setequal(names(pr$p$.smoothedRocDataList), "biomarker")
    expect_equal(nrow(pr$a$results$thresholdTable$asDF), n1)
})

test_that("no positive class selected (NULL): the analysis waits instead of crashing or guessing", {
    # ps_private, not run_ps: modifyList() would drop a NULL. NULL is an empty Positive Class
    # (allowNone), and the documented contract is that it leaves the analysis waiting.
    pr <- ps_private(dependentVars = "biomarker", classVar = "disease_status", positiveClass = NULL, refVar = NULL)
    expect_no_error(pr$p$.run())   # was: argument is of length zero; then a guessed last level
    expect_equal(pr$a$results$aucSummaryTable$rowCount, 0)
    expect_equal(pr$a$results$outcomeCoding$asDF$role, rep("Not assigned yet", 2))
    expect_match(pr$a$results$runSummary$content, "Choose the positive and negative classes", fixed = TRUE)
    expect_equal(tnote(pr$a$results$aucSummaryTable, "assumed_positive_class"), "(none)")
})

test_that("a missing value in the subgroup variable drops that row, not the whole analysis", {
    d <- ps_data()
    d$grp <- factor(rep(c("A", "B"), length.out = nrow(d)))
    d$grp[1] <- NA
    pr <- ps_private_data(d, dependentVars = "biomarker", classVar = "disease_status",
                          positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, subGroup = "grp")
    expect_no_error(pr$p$.run())   # was: "Insufficient data ... found: 0" from a "biomarker ::: NA" stratum
    expect_setequal(pr$a$results$resultsTable$itemKeys, c("biomarker ::: A", "biomarker ::: B"))
    expect_match(pr$a$results$runSummary$content, "1 row(s) with a missing value", fixed = TRUE)
})

test_that("warnings raised after the Analysis Status box is built still reach it", {
    pr <- ps_private(dependentVars = "biomarker", classVar = "disease_status", positiveClass = "Disease", negativeClass = "Healthy",
                     refVar = NULL, partialAUC = TRUE)
    env <- pr$p
    unlockBinding(".checkPackageDependencies", env)
    assign(".checkPackageDependencies",
           function(...) { env$.warnUser("LATE-WARNING-PROBE"); FALSE }, envir = env)
    pr$p$.run()
    expect_true("LATE-WARNING-PROBE" %in% pr$p$.userWarnings)
    expect_match(pr$a$results$runSummary$content, "LATE-WARNING-PROBE", fixed = TRUE)
})

test_that("Analysis Status warnings are HTML-escaped once", {
    pr <- ps_private(dependentVars = "biomarker", classVar = "disease_status", positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL)
    pr$p$.run()
    pr$p$.warnUser("<b>x</b> & y")
    pr$p$.renderRunSummary()
    html <- pr$a$results$runSummary$content
    expect_match(html, "&lt;b&gt;x&lt;/b&gt; &amp; y", fixed = TRUE)
    expect_false(grepl("<b>x</b>", html, fixed = TRUE))
})

test_that("a cutpointr failure is disclosed, not silently replaced by the Youden fallback", {
    d <- ps_data()
    pr <- ps_private(dependentVars = "biomarker", classVar = "disease_status", positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL)
    r <- pr$p$.runCutpointrMetrics(dependentVar = d$biomarker, classVar = d$disease_status,
                                   positiveClass = "Disease", method = function(...) stop("boom"),
                                   score = NULL, metric = cutpointr::youden, direction = ">=",
                                   tol_metric = 0, boot_runs = 0, break_ties = mean, var_label = "biomarker")
    expect_true(is.finite(r$results$AUC))
    expect_match(pr$p$.userWarnings, "cutpointr could not analyze biomarker (boom)", fixed = TRUE)
})


# --- 2026-09-03 /fix-function (second pass): effect-size labels, power curve, DCA plot, subgroup messages ---

test_that("effect-size table gives each marker's d and labels the differences as differences", {
    d <- ps_data()
    set.seed(3)
    d$m2 <- d$biomarker + rnorm(nrow(d), 0, 20)
    pr <- ps_private_data(d, dependentVars = c("biomarker", "m2"), classVar = "disease_status",
                          positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, effectSizeAnalysis = TRUE)
    pr$p$.run()
    es <- pr$a$results$effectSizeTable$asDF
    expect_equal(nrow(es), 1)
    pooled_d <- function(x) {
        pos <- x[d$disease_status == "Disease"]; neg <- x[d$disease_status == "Healthy"]
        sp <- sqrt(((length(pos) - 1) * var(pos) + (length(neg) - 1) * var(neg)) / (length(pos) + length(neg) - 2))
        (mean(pos) - mean(neg)) / sp
    }
    d1 <- pooled_d(d$biomarker); d2 <- pooled_d(d$m2)
    expect_equal(es$d_first, d1, tolerance = 1e-8)
    expect_equal(es$d_second, d2, tolerance = 1e-8)
    expect_equal(es$cohens_d, d2 - d1, tolerance = 1e-8)
    J <- 1 - 3 / (4 * (nrow(d) - 2) - 1)
    expect_equal(es$hedges_g, J * d2 - J * d1, tolerance = 1e-8)   # difference of two Hedges' g
    expect_match(tnote(pr$a$results$effectSizeTable, "definition"), "second marker minus first marker", fixed = TRUE)
})

test_that("power curve plot uses the table's Hanley-McNeil power and renders", {
    skip_if_not_installed("pROC")
    pr <- ps_private(dependentVars = "biomarker", classVar = "disease_status", positiveClass = "Disease", negativeClass = "Healthy",
                     refVar = NULL, powerAnalysis = TRUE, powerAnalysisType = "sample_size",
                     expectedAUCDifference = 0.2, targetPower = 0.8, significanceLevel = 0.05)
    pr$p$.run()
    tab <- pr$a$results$powerAnalysisTable$asDF
    st <- pr$a$results$powerCurvePlot$get(key = "biomarker")$state
    expect_equal(st$auc_target, 0.7)
    expect_equal(st$required_n, tab$required_n)
    # the required N is the smallest n reaching the target on the SAME power function the plot draws
    powr <- function(n) pr$p$.hanleyMcNeilPower(0.7, n, st$pos_frac, 0.05)
    expect_gte(powr(tab$required_n), 0.8)
    expect_lt(powr(tab$required_n - 1), 0.8)
    expect_true(pr$p$.plotPowerCurves(pr$a$results$powerCurvePlot$get(key = "biomarker"), ggplot2::theme_minimal(), NULL))
})

test_that("decision-curve plot state uses the complete-case prevalence and the table's risk fit", {
    skip_if_not_installed("pROC")
    d <- ps_data()
    d$biomarker[1:20] <- NA                      # missingness in the marker
    d$sep <- ifelse(d$disease_status == "Disease", 1, 0) + runif(nrow(d), 0, 0.1)   # complete separation
    pr <- ps_private_data(d, dependentVars = c("biomarker", "sep"), classVar = "disease_status",
                          positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, clinicalUtilityAnalysis = TRUE)
    pr$p$.run()
    st <- pr$a$results$decisionCurvePlot$get(key = "biomarker")$state
    expect_equal(st$prevalence, mean(st$outcome))               # was: all-rows prevalence
    expect_equal(length(st$risk), length(st$outcome))
    st2 <- pr$a$results$decisionCurvePlot$get(key = "sep")$state
    expect_false(is.null(st2$risk))                              # was: NULL (warning caught) -> blank plot
    expect_true(pr$p$.plotDecisionCurve(pr$a$results$decisionCurvePlot$get(key = "sep"), ggplot2::theme_minimal(), NULL))
})

test_that("subgroup analysis: DeLong is an explicit error, IDI/NRI say they use the full sample", {
    skip_if_not_installed("pROC")
    d <- ps_data()
    set.seed(5)
    d$m2 <- d$biomarker + rnorm(nrow(d), 0, 20)
    pr <- ps_private_data(d, dependentVars = c("biomarker", "m2"), classVar = "disease_status",
                          positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, subGroup = "sex", delongTest = TRUE)
    expect_error(pr$p$.run(), "DeLong")     # .validateInputs(): actionable, not a silent skip
    pr <- ps_private_data(d, dependentVars = c("biomarker", "m2"), classVar = "disease_status",
                          positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, subGroup = "sex",
                          calculateIDI = TRUE, calculateNRI = TRUE)
    expect_no_error(pr$p$.run())
    html <- pr$a$results$runSummary$content
    expect_match(html, "IDI/NRI are computed on the full sample", fixed = TRUE)
    expect_false(grepl("IDI/NRI disabled", html, fixed = TRUE))   # was: claimed disabled while computing them
    expect_gt(nrow(pr$a$results$idiTable$asDF), 0)
    expect_match(tnote(pr$a$results$idiTable, "subgroup_warning"), "full sample", fixed = TRUE)
})


# --- 2026-10-09: the positive class is chosen, never inferred; the Outcome coding table ---

# every table note, Html/Preformatted text and character cell of a results tree
ps_text <- function(item) {
    if (inherits(item, "Table")) {
        df <- tryCatch(item$asDF, error = function(e) data.frame())
        return(c(vapply(item$notes, function(z) z$note, ""),
                 unlist(lapply(df, function(col) if (is.character(col)) col))))
    }
    if (inherits(item, "Html") || inherits(item, "Preformatted")) return(as.character(item$content))
    if (inherits(item, "Array")) return(unlist(lapply(item$items, ps_text)))
    if (inherits(item, "Group")) return(unlist(lapply(item$itemNames, function(nm) ps_text(item$get(nm)))))
    character(0)
}
oc_roles <- function(res) {
    df <- res$outcomeCoding$asDF
    setNames(df$role, df$level)
}

test_that("Positive Class can be left empty (allowNone), so jamovi never pre-selects a level", {
    # Without allowNone the client pre-selects the first level - usually the NEGATIVE one - and
    # the whole analysis ran reversed.
    a <- yaml::read_yaml(test_path("..", "..", "jamovi", "psychopdaROC.a.yaml"))
    o <- Filter(function(x) identical(x$name, "positiveClass"), a$options)[[1]]
    expect_identical(o$type, "Level")
    expect_true(isTRUE(o$allowNone))
})

test_that("Outcome coding: a binary class names its positive and its negative level", {
    d <- ps_data()
    res <- run_ps(data = d, dependentVars = "biomarker")
    oc <- res$outcomeCoding$asDF
    expect_equal(oc$level, c("Healthy", "Disease"))
    expect_equal(as.integer(oc$n), as.integer(table(d$disease_status)))
    expect_equal(oc$role, c("Negative (condition absent)", "Positive (condition present)"))
    expect_match(tnote(res$outcomeCoding, "positive"), "Positive class: Disease.", fixed = TRUE)
    expect_match(tnote(res$outcomeCoding, "positive"), "Negative class: Healthy.", fixed = TRUE)
    expect_identical(tnote(res$outcomeCoding, "source"), "Chosen in Positive Class and Negative Class.")
    expect_match(tnote(res$outcomeCoding, "n"), "N counts every row with each outcome value", fixed = TRUE)
    expect_equal(tnote(res$outcomeCoding, "unset"), "(none)")
    # the reading of the marker sits next to the coding, word for word the AUC table's note
    expect_match(tnote(res$outcomeCoding, "reading"), "HIGHER values were taken to indicate Disease", fixed = TRUE)
    expect_identical(tnote(res$outcomeCoding, "reading"), tnote(res$aucSummaryTable, "direction_used"))
})

test_that("Outcome coding: with three levels the unselected level is excluded", {
    d <- ps_data3()
    res <- run_ps(data = d, dependentVars = "biomarker", classVar = "ds3", positiveClass = "Severe", negativeClass = "Healthy")
    expect_equal(oc_roles(res), c(Healthy = "Negative (condition absent)",
      Disease = "Excluded: unselected outcome level", Severe = "Positive (condition present)"))
    expect_equal(as.integer(res$outcomeCoding$asDF$n), as.integer(table(d$ds3)))
    expect_match(tnote(res$outcomeCoding, "positive"),
                 "Other outcome levels are excluded.", fixed = TRUE)
    expect_equal(res$aucSummaryTable$asDF$auc[1], mw_auc(d$biomarker[d$ds3 != "Disease"], d$ds3[d$ds3 != "Disease"] == "Severe"), tolerance = 1e-9)
})

test_that("Outcome coding: a level literally named 'Other' can be the positive class", {
    # The pooled negative side used to be labelled "Other", so choosing a level called Other
    # failed with 'factor level [2] is duplicated'.
    d <- ps_data3()
    d$dsOther <- factor(sub("Severe", "Other", as.character(d$ds3), fixed = TRUE),
                        levels = c("Healthy", "Other", "Disease"))
    for (pos in c("Other", "Disease")) {
        res <- expect_no_error(run_ps(data = d, dependentVars = "biomarker", classVar = "dsOther",
                                      positiveClass = pos, negativeClass = "Healthy"))
        expect_equal(res$aucSummaryTable$asDF$auc[1], mw_auc(d$biomarker[d$dsOther %in% c(pos, "Healthy")],
                       d$dsOther[d$dsOther %in% c(pos, "Healthy")] == pos),
                     tolerance = 1e-9, label = pos)
        expect_identical(unname(oc_roles(res)[pos]), "Positive (condition present)")
        expect_equal(sum(oc_roles(res) == "Negative (condition absent)"), 1)
    }
})

test_that("Outcome coding: an explicit NA level is a '(missing)' row, not a negative case", {
    # addNA() makes NA a real level that is.na() does not see, so it used to be pooled with the
    # negatives. Two Malignant rows are moved to it; the AUC is the one on the 18 classed rows.
    d <- data.frame(status = c(rep("Benign", 10), rep("Malignant", 10)),
                    marker = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 10,   3, 5, 6, 8, 9, 11, 12, 13, 14, 15))
    d$status <- addNA(factor(replace(d$status, 19:20, NA), levels = c("Benign", "Malignant")))
    res <- run_ps(data = d, dependentVars = "marker", classVar = "status", positiveClass = "Malignant", negativeClass = "Benign")
    oc <- res$outcomeCoding$asDF
    expect_equal(oc$level, c("Benign", "Malignant", "(missing)"))
    expect_equal(as.integer(oc$n), c(10L, 8L, 2L))
    expect_equal(oc$role[3], "Excluded: missing outcome")
    expect_equal(res$aucSummaryTable$asDF$auc[1],
                 mw_auc(d$marker, as.character(d$status) == "Malignant"), tolerance = 1e-12)   # 62.5 / 80
})

test_that("a subgroup or a test variable without the positive class is refused precisely", {
    # It used to surface as 'Need at least 2 negative cases, found: 0' - the wrong class, no name.
    d <- ps_data()
    d$grp <- factor(ifelse(d$disease_status == "Healthy" & seq_len(nrow(d)) %% 5 == 0, "C",
                           ifelse(seq_len(nrow(d)) %% 2 == 0, "A", "B")))
    expect_equal(sum(d$grp == "C" & d$disease_status == "Disease"), 0)
    e <- expect_error(run_ps(data = d, dependentVars = "biomarker", subGroup = "grp"),
                      "Subgroup C has no case of the positive class (Disease) with a value of biomarker",
                      fixed = TRUE)
    expect_false(grepl("negative cases", conditionMessage(e), fixed = TRUE))

    d$bm2 <- ifelse(d$disease_status == "Disease", NA, d$biomarker)
    expect_error(run_ps(data = d, dependentVars = c("biomarker", "bm2")),
                 "No case of the positive class (Disease) has a value of bm2", fixed = TRUE)
})

test_that("no output says a positive class was assumed, guessed or taken from level order", {
    d <- ps_data()
    d$inv <- -d$biomarker    # one marker reads below 0.5 either way, so the below-chance note shows
    for (pos in c("Disease", "Healthy")) {
        res <- run_ps(data = d, dependentVars = c("biomarker", "inv"), positiveClass = pos, negativeClass = if (pos == "Disease") "Healthy" else "Disease",
                      sensSpecTable = TRUE, showThresholdTable = TRUE, delongTest = TRUE)
        txt <- ps_text(res)
        expect_gt(length(txt), 20)
        expect_equal(unname(grep("assumed|guess|last level", txt, ignore.case = TRUE, value = TRUE)),
                     character(0), label = pos)
        expect_match(tnote(res$aucSummaryTable, "auc_below_chance"),
                     paste0("First check Positive Class: is ", pos, " the level"), fixed = TRUE)
        expect_match(res$procedureNotes$content, paste0("Positive Class: ", pos), fixed = TRUE)
    }
})

test_that("confusion matrices and dot plots name the actual levels", {
    # They read only "Positive"/"Negative", so nothing said which level was which.
    d <- ps_data()
    res <- run_ps(data = d, dependentVars = "biomarker", sensSpecTable = TRUE, showDotPlot = TRUE,
                  combinePlots = FALSE)
    # library-audit 2026-10-23 meddecide [HIGH] DONE: the matrices are Tables; levels name the columns and rows
    cm <- res$sensSpecTable$get(key = "biomarker")
    expect_identical(cm$getColumn("pos")$title, "Positive (Disease)")
    expect_identical(cm$getColumn("neg")$title, "Negative (Healthy)")
    expect_identical(cm$asDF$decision[1:2], c("Positive (Disease)", "Negative (Healthy)"))
    st <- res$dotPlot$get(key = "biomarker")$state
    expect_equal(as.vector(table(st$label)[c("Negative (Healthy)", "Positive (Disease)")]),
                 as.vector(table(d$disease_status)))

    three <- run_ps(data = ps_data3(), dependentVars = "biomarker", classVar = "ds3",
                    positiveClass = "Severe", negativeClass = "Healthy", sensSpecTable = TRUE)
    cm3 <- three$sensSpecTable$get(key = "biomarker")
    expect_identical(cm3$getColumn("pos")$title, "Positive (Severe)")
    expect_identical(cm3$getColumn("neg")$title, "Negative (Healthy)")

    # level names are user data: escaped inside the Html
    d$lt <- factor(ifelse(d$disease_status == "Disease", "Ca <b>", "Benign & co"),
                   levels = c("Benign & co", "Ca <b>"))
    esc <- run_ps(data = d, dependentVars = "biomarker", classVar = "lt", positiveClass = "Ca <b>", negativeClass = "Benign & co",
                  sensSpecTable = TRUE)
    # Table titles and cells are HTML-parsed by the client: a "<" before a letter is broken (.rocLabel)
    cme <- esc$sensSpecTable$get(key = "biomarker")
    expect_identical(cme$getColumn("pos")$title, "Positive (Ca < b>)")
    expect_identical(cme$asDF$decision[2], "Negative (Benign & co)")
    expect_false(grepl("Ca <b>", paste(unlist(cme$asDF), collapse = " "), fixed = TRUE))
})

test_that("the interactive ROC plot draws one path per marker", {
    # Without a group, geom_line sorted every marker's points together by x and drew a single
    # zigzag: 11 places where the drawn curve dropped on this data.
    d <- ps_data()
    d$b2 <- round(d$biomarker)
    pr <- ps_private_data(d, dependentVars = c("biomarker", "b2"), classVar = "disease_status",
                          positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, plotROC = TRUE, interactiveROC = TRUE)
    pr$p$.run()
    expect_true(pr$p$.plotInteractiveROC(pr$a$results$interactivePlot, ggplot2::theme_grey(), NULL))
    ld <- ggplot2::layer_data(ggplot2::last_plot(), 2)   # layer 1 is the chance diagonal
    expect_equal(length(unique(ld$group)), 2)
    drops <- vapply(split(ld$y, ld$group), function(y) sum(diff(y) < -1e-9), numeric(1))
    expect_equal(unname(drops), c(0, 0))
})


# --- 2026-10-09 (second pass): one regression test per fix ---
# Each test below fails on R/psychopdaROC.b.R as it was before these fixes (checked against a saved copy).

# 10 Benign, 10 Malignant; Youden J = 0.5 is tied at the observed cut-points 8, 9 and 11
ps_d1 <- function() data.frame(status = factor(rep(c("Benign", "Malignant"), each = 10)),
                               marker = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 10, 3, 5, 6, 8, 9, 11, 12, 13, 14, 15))

test_that("no usable cut-point: the note sends the user to Positive Class first, naming the level", {
    res <- run_ps(data = ps_data(), dependentVars = "biomarker", positiveClass = "Healthy", negativeClass = "Disease")
    n <- tnote(res$resultsTable$get(key = "biomarker"), "no_usable_cutpoint")
    expect_match(n, "First check Positive Class: is Healthy the level that means the condition is present?", fixed = TRUE)
    expect_lt(regexpr("First check Positive Class", n, fixed = TRUE), regexpr("check Classification Direction", n, fixed = TRUE))
    expect_match(n, "in the Area Under the ROC Curve table are unaffected", fixed = TRUE)   # that table is below, not 'above'
    expect_identical(tnote(run_ps(data = ps_data(), dependentVars = "biomarker")$resultsTable$get(key = "biomarker"),
                           "no_usable_cutpoint"), "(none)")
})

test_that("a cut-point between observed values is read with its own rule, and tied optima are disclosed", {
    # break_ties = mean shows 9.333, the mean of the tied optima 8, 9 and 11. The row printed the metrics of the
    # NEAREST observed value (x >= 9: sensitivity 0.6), not of x >= 9.333 (0.5).
    d <- ps_d1()
    res <- run_ps(data = d, dependentVars = "marker", classVar = "status", positiveClass = "Malignant", negativeClass = "Benign", sensSpecTable = TRUE)
    rt <- res$resultsTable$get(key = "marker")
    row <- rt$asDF
    cut <- row$cutpoint[1]
    expect_equal(cut, mean(c(8, 9, 11)), tolerance = 1e-9)
    case <- d$status == "Malignant"
    se <- mean(d$marker[case] >= cut)
    sp <- mean(d$marker[!case] < cut)
    expect_equal(c(row$sensitivity, row$specificity, row$youden), c(se, sp, se + sp - 1), tolerance = 1e-12)
    expect_match(tnote(rt, "tied_optima"), "Several cut-points tie for the best youden (0.5): 8, 9, 11.", fixed = TRUE)
    cm <- res$sensSpecTable$get(key = "marker")$asDF    # the confusion matrix uses the same rule
    expect_equal(cm$pos[1], sum(d$marker[case] >= cut))   # TP
    expect_equal(cm$pos[2], sum(d$marker[case] < cut))    # FN
})

test_that("'All tied cutpoints' gives one confusion matrix per tie and a dot plot, without an error", {
    res <- expect_no_error(run_ps(data = ps_d1(), dependentVars = "marker", classVar = "status", positiveClass = "Malignant", negativeClass = "Benign",
                                  break_ties = "c", sensSpecTable = TRUE, showDotPlot = TRUE, combinePlots = FALSE))
    expect_equal(sort(res$resultsTable$get(key = "marker")$asDF$cutpoint), c(8, 9, 11))
    cm <- res$sensSpecTable$get(key = "marker")$asDF    # two rows per tied cut-point
    expect_equal(nrow(cm), 6)
    expect_equal(sort(unique(cm$cutpoint)), c(8, 9, 11))
    expect_equal(nrow(res$dotPlot$get(key = "marker")$state), 20)
})

test_that("power analysis counts only rows with a class value; an explicit NA level is missing, not negative", {
    set.seed(1)
    g <- rep(0:1, length.out = 60)
    y <- factor(ifelse(g == 1, "Disease", "Healthy"), levels = c("Healthy", "Disease"))
    y[c(2, 4, 5, 8, 10, 12, 14, 16, 18, 20)] <- NA                          # 10 rows without a class
    d <- data.frame(y = y, m = round(g * 1.2 + rnorm(60), 2), m2 = round(-g * 0.8 + rnorm(60), 2))
    res <- run_ps(data = d, classVar = "y", dependentVars = "m", powerAnalysis = TRUE, powerAnalysisType = "sample_size")
    expect_match(res$powerAnalysisTable$asDF$recommendation[1], "(current n=50)", fixed = TRUE)
    # addNA() makes the 10 rows a level that is.na() does not see: the class indicator must still drop them
    es <- function(dd) run_ps(data = dd, classVar = "y", dependentVars = c("m", "m2"), effectSizeAnalysis = TRUE)$effectSizeTable$asDF
    d2 <- d
    d2$y <- addNA(d$y)
    expect_equal(es(d2), es(d), tolerance = 1e-12)
})

test_that("Analysis Status and Procedure Notes are emptied and hidden when the state that wrote them is gone", {
    # Neither clears with an option, and jamovi hands back an Html item's previous content: after the positive
    # class was unset, 'Positive Class: Disease' stayed on screen.
    mk <- function(...) {
        a <- ClinicoPath:::psychopdaROCClass$new(options = do.call(ClinicoPath:::psychopdaROCOptions$new, utils::modifyList(list(run = TRUE), list(...), keep.null = TRUE)), data = ps_data())
        a$init()
        a
    }
    full <- mk(classVar = "disease_status", positiveClass = "Disease", negativeClass = "Healthy", dependentVars = "biomarker", refVar = NULL)
    suppressMessages(full$run())
    expect_match(full$results$procedureNotes$content, "Positive Class: Disease", fixed = TRUE)
    unset <- mk(classVar = "disease_status", positiveClass = NULL, dependentVars = "biomarker", refVar = NULL)
    for (nm in c("runSummary", "procedureNotes"))
        unset$results$get(nm)$fromProtoBuf(list(stale = FALSE, state = raw(),
            html = list(content = full$results$get(nm)$content, scripts = character(), stylesheets = character())),
            oChanges = "positiveClass", vChanges = character())
    unset$postInit()
    suppressMessages(unset$run())
    # library-audit 2026-10-23 meddecide [MEDIUM] DONE: the old notes are cleared ("<div></div>"), not hidden
    expect_identical(unset$results$procedureNotes$content, "<div></div>")
    expect_match(unset$results$runSummary$content, "Choose the positive and negative classes", fixed = TRUE)
})

test_that("the direction notes emphasise with <i>, not <b>", {
    # R/psychopdaROC.b.R: the client's table-note renderer keeps only em, i, sub and sup
    res <- run_ps(data = ps_data(), dependentVars = "biomarker")
    for (n in c(tnote(res$aucSummaryTable, "direction_used"), tnote(res$outcomeCoding, "reading"))) {
        expect_match(n, "<i>HIGHER values were taken to indicate Disease</i>", fixed = TRUE)
        expect_false(grepl("<b>", n, fixed = TRUE))
    }
})

test_that("a Positive Class that no row has is explained neutrally, including the '- None -' case", {
    e <- expect_error(run_ps(data = ps_data(), dependentVars = "biomarker", classVar = "sex"),
                      "Positive Class is set to Disease, but no row of sex has that level. Choose an observed level in Positive Class.",
                      fixed = TRUE)
    expect_match(conditionMessage(e), "After changing the class variable or filtering rows, reselect both outcome levels.",
                 fixed = TRUE)
})

test_that("tables computed on the pooled sample say so when a subgroup splits the ROC tables", {
    d <- ps_data()
    set.seed(3)
    d$m2 <- d$biomarker + rnorm(nrow(d), 0, 20)
    sg <- run_ps(data = d, dependentVars = c("biomarker", "m2"), subGroup = "sex", effectSizeAnalysis = TRUE,
                 calculateNRI = TRUE, idiNriBootRuns = 100)
    expect_match(tnote(sg$effectSizeTable, "subgroup_scope"), "Computed on the full sample", fixed = TRUE)
    expect_match(tnote(sg$nriTable, "subgroup_warning"), "computed on the full sample, not per subgroup", fixed = TRUE)
    plain <- run_ps(data = d, dependentVars = c("biomarker", "m2"), effectSizeAnalysis = TRUE, calculateNRI = TRUE, idiNriBootRuns = 100)
    expect_identical(tnote(plain$effectSizeTable, "subgroup_scope"), "(none)")
    expect_identical(tnote(plain$nriTable, "subgroup_warning"), "(none)")
})

test_that("a subgroup or a marker with fewer than 2 cases of a class is refused by name and side", {
    d <- ps_data()
    d$grp <- factor(ifelse(seq_len(nrow(d)) %% 2 == 0, "A", "B"), levels = c("A", "B", "D"))
    d$grp[c(which(d$disease_status == "Disease")[1], which(d$disease_status == "Healthy")[1:3])] <- "D"   # 1 Disease, 3 Healthy
    expect_error(run_ps(data = d, dependentVars = "biomarker", subGroup = "grp"),
                 "Subgroup D has 1 case(s) of the positive class (Disease) with a value of biomarker", fixed = TRUE)
    d$bm1 <- ifelse(d$disease_status == "Disease", NA, d$biomarker)
    d$bm1[which(d$disease_status == "Disease")[1]] <- 50                       # one Disease value
    expect_error(run_ps(data = d, dependentVars = "bm1"),
                 "bm1 has a value for 1 case(s) of the positive class (Disease); at least 2 are needed", fixed = TRUE)
})

test_that("the DeLong fallback reads the markers in the chosen Classification Direction", {
    # Healthy is LOWER on both markers, so read '<=' both AUCs are about 0.9 and nothing needs inverting. The
    # fallback counted higher values as positive, got 1 - AUC, then reported both markers as inverted.
    d <- ps_data()
    set.seed(5)
    S <- data.frame(biomarker = d$biomarker, m2 = d$biomarker + rnorm(nrow(d), 0, 15))
    o <- ClinicoPath:::psychopdaROCOptions$new(run = TRUE, dependentVars = names(S), classVar = "cls", positiveClass = "Healthy", negativeClass = "Disease",
                                               direction = "<=", refVar = NULL)
    a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = cbind(S, cls = d$disease_status))
    r <- a$.__enclos_env__$private$.deLongTest(data = S, classVar = as.character(d$disease_status), positiveClass = "Healthy")
    expect_identical(r$inverted, character(0))
    healthy <- d$disease_status == "Healthy"
    expect_equal(unname(r$AUC[, "AUC"]), c(mw_auc(-S$biomarker, healthy), mw_auc(-S$m2, healthy)), tolerance = 1e-9)
})

test_that("legacy meta-analysis overrides cannot emit invalid pooled inference", {
    data(psychopdaROC_advanced, package = "ClinicoPath", envir = environment())
    res <- run_ps(data = psychopdaROC_advanced, classVar = "diagnosis", positiveClass = "Positive", negativeClass = "Negative",
                  dependentVars = c("marker_excellent", "marker_good", "marker_moderate"),
                  metaAnalysis = TRUE, overrideMetaAnalysisWarning = TRUE, forestPlot = TRUE)
    expect_false(res$metaAnalysisTable$visible)
    expect_true(all(is.na(res$metaAnalysisTable$asDF$pooled_auc)))
    expect_length(res$metaAnalysisForestPlot$itemKeys, 0)
    expect_match(res$metaAnalysisWarning$content, "override is ignored")
})

# --- 2026-10-09 (third pass): one regression test per fix ---
# Each test below fails on R/psychopdaROC.b.R and R/utils.R as they were before these fixes (checked against
# saved copies). The line marked "guard" passes on both.

test_that("Outcome coding cells show a level name as it is; only the notes map [] to ()", {
    d <- ps_d1()
    d$status <- factor(ifelse(d$status == "Malignant", "Grade [II]", "Grade [I]"), levels = c("Grade [I]", "Grade [II]"))
    res <- run_ps(data = d, dependentVars = "marker", classVar = "status", positiveClass = "Grade [II]", negativeClass = "Grade [I]")
    expect_identical(res$outcomeCoding$asDF$level, c("Grade [I]", "Grade [II]"))
    expect_match(tnote(res$outcomeCoding, "positive"), "Positive class: Grade (II).", fixed = TRUE)
})

test_that("a blank class value is missing, as an NA is, in the Outcome coding table and in the analysis", {
    # R path only (jamovi stores an empty cell as missing). The table already counted "" as missing while
    # the analysis still pooled those rows with the negatives.
    d <- ps_data()
    d$disease_status <- as.character(d$disease_status)
    blank <- c(1:5, 101:105)
    d$disease_status[blank] <- ""
    d$biomarker[blank] <- max(d$biomarker) + 1          # as negatives these rows would lower the AUC
    res <- run_ps(data = d, dependentVars = "biomarker")
    keep <- d$disease_status != ""
    expect_equal(res$aucSummaryTable$asDF$auc[1], mw_auc(d$biomarker[keep], d$disease_status[keep] == "Disease"), tolerance = 1e-9)
    oc <- res$outcomeCoding$asDF
    expect_equal(as.integer(oc$n[oc$level == "(missing)"]), 10L)
})

test_that("the Clinical Interpretation text says that ties count one half", {
    it <- run_ps(data = ps_data(), dependentVars = "biomarker")$clinicalInterpretationTable$asDF$interpretation_text[1]
    expect_match(it, "of the time (ties count one half), in the classification direction in use.", fixed = TRUE)
})

test_that("Reference Test for IDI/NRI can be any numeric column, also beside a single test variable", {
    # refVar was a Level of the class variable, so it could never name a test column; it is a numeric Variable now.
    d <- ps_data()
    set.seed(3)
    d$ref <- d$biomarker + rnorm(nrow(d), 0, 30)
    d$m2 <- d$biomarker + rnorm(nrow(d), 0, 20)
    one <- run_ps(data = d, dependentVars = "biomarker", refVar = "ref", calculateIDI = TRUE, idiNriBootRuns = 100)
    expect_identical(one$idiTable$asDF$refVar, "ref")
    two <- run_ps(data = d, dependentVars = c("biomarker", "m2"), refVar = "ref", calculateNRI = TRUE, idiNriBootRuns = 100)
    expect_identical(two$nriTable$asDF$refVar, c("ref", "ref"))
    dflt <- run_ps(data = d, dependentVars = c("biomarker", "m2"), calculateIDI = TRUE, idiNriBootRuns = 100)
    expect_match(tnote(dflt$idiTable, "default_ref"),
                 "Reference test: biomarker, the first test variable, because Reference Test for IDI/NRI is empty.", fixed = TRUE)
    # guard: a non-numeric reference is refused before .run() (permitted: numeric), so the backend needs no check
    expect_error(run_ps(data = d, dependentVars = "biomarker", refVar = "sex", calculateIDI = TRUE), "requires a numeric variable", fixed = TRUE)
})

test_that("the class variable itself cannot be the Reference Test for IDI/NRI", {
    # jamovi's integer-nominal 0/1 class carries a 'values' attribute, so the numeric check lets it through
    d <- ps_d1()
    d$out <- factor(as.integer(d$status == "Malignant"))
    attr(d$out, "values") <- c(0L, 1L)
    expect_error(run_ps(data = d, dependentVars = "marker", classVar = "out", positiveClass = "1", negativeClass = "0", refVar = "out", calculateIDI = TRUE),
                 "Reference Test for IDI/NRI holds the class variable, the gold standard itself.", fixed = TRUE)
})

test_that("no usable cut-point although some separate: the note names the method or Metric Tolerance, not the marker", {
    # J = 0.5 at the best cut-points of ps_d1(); a tolerance of 0.55 counts calling everyone negative (+Inf) as tied
    rt <- run_ps(data = ps_d1(), dependentVars = "marker", classVar = "status", positiveClass = "Malignant", negativeClass = "Benign",
                 tol_metric = 0.55)$resultsTable$get(key = "marker")
    expect_equal(rt$rowCount, 0)
    n <- tnote(rt, "no_usable_cutpoint")
    expect_match(n, "so every patient would be classified negative, although some cut-points do separate the groups better than chance", fixed = TRUE)
    expect_match(n, "If a cut-point is needed, lower Metric Tolerance or choose another Cutpoint Method or Optimization Metric.", fixed = TRUE)
    expect_false(grepl("no threshold separates", n, fixed = TRUE))
})
