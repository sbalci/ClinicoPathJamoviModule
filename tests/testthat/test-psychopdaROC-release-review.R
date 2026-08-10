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
        list(classVar = "disease_status", positiveClass = "Disease", refVar = NULL),
        list(...))
    do.call(ClinicoPath::psychopdaROC, args)
}

# read a keyed table note, or "(none)"
tnote <- function(table, key) {
    n <- table$.__enclos_env__$private$.notes
    if (!(key %in% names(n))) return("(none)")
    get("note", envir = n[[key]])
}

ps_private <- function(...) {
    o <- do.call(ClinicoPath:::psychopdaROCOptions$new, list(...))
    a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = ps_data())
    a$init()
    list(a = a, p = a$.__enclos_env__$private)
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
        dependentVars = c("biomarker", "marker2"), classVar = "disease_status",
        positiveClass = "Disease", refVar = "biomarker",
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
        positiveClass = "Disease", refVar = NULL, manualRun = TRUE, run = TRUE)
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
    src <- readLines("../../R/psychopdaroc.b.R", warn = FALSE)
    gate <- grep("MANUAL RUN GATE", src)[1]
    clear <- grep("private\\$\\.clearTables\\(\\)", src)[1]
    expect_true(clear > gate)
})


test_that("an unset positive class no longer runs the analysis backwards", {
    # The fallback took the FIRST level. For the usual codings -- Healthy/Disease,
    # Negative/Positive, Control/Case -- that is the NEGATIVE group, so the whole analysis ran
    # inverted: AUC 0.1001 on this data where naming the positive class gives 0.8999, silently.
    d <- ps_data()
    expect_equal(levels(d$disease_status), c("Healthy", "Disease"))

    unset <- run_ps(data = d, dependentVars = "biomarker", positiveClass = "")
    named <- run_ps(data = d, dependentVars = "biomarker", positiveClass = "Disease")
    expect_equal(unset$aucSummaryTable$asDF$auc[1],
                 named$aucSummaryTable$asDF$auc[1], tolerance = 1e-9)
    expect_gt(unset$aucSummaryTable$asDF$auc[1], 0.5)

    # and the guess is disclosed where the numbers are read
    expect_match(tnote(unset$aucSummaryTable, "assumed_positive_class"), "was assumed")
    expect_match(tnote(unset$aucSummaryTable, "assumed_positive_class"), "reversed")
    expect_equal(tnote(named$aucSummaryTable, "assumed_positive_class"), "(none)")

    # naming the other level legitimately inverts it -- that is the user's choice, not a guess
    inv <- run_ps(data = d, dependentVars = "biomarker", positiveClass = "Healthy")
    expect_equal(inv$aucSummaryTable$asDF$auc[1],
                 1 - named$aucSummaryTable$asDF$auc[1], tolerance = 1e-9)
    expect_equal(tnote(inv$aucSummaryTable, "assumed_positive_class"), "(none)")
})


test_that("the positive class is resolved in exactly one place", {
    # .prepareVarData, .run() and the procedure notes each worked it out separately. When they
    # disagreed the data was dichotomised one way and scored the other, giving an AUC of 0.
    backend <- paste(readLines("../../R/psychopdaroc.b.R", warn = FALSE), collapse = "\n")
    # no surviving "first level" fallbacks
    expect_false(grepl("levels(factor(classVar))[1]", backend, fixed = TRUE))
    expect_equal(length(gregexpr(".resolvePositiveClass = function", backend, fixed = TRUE)[[1]]), 1L)

    # and it behaves: an unset class resolves to the last level
    prv <- ps_private(dependentVars = "biomarker", classVar = "disease_status",
                      positiveClass = "", refVar = NULL)$p
    expect_equal(prv$.resolvePositiveClass(factor(c("Healthy", "Disease"),
                                                  levels = c("Healthy", "Disease"))), "Disease")
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
    a_yaml <- paste(readLines("../../jamovi/psychopdaroc.a.yaml", warn = FALSE), collapse = "\n")
    blk <- regmatches(a_yaml, regexpr("(?s)    - name: tol_metric\\n.*?(?=\\n    - name: )",
                                      a_yaml, perl = TRUE))
    expect_match(blk, "default: 0.000001", fixed = TRUE)
})


test_that("a metric tolerance that averages cutpoints says so", {
    d <- ps_data()
    wide <- run_ps(data = d, dependentVars = "biomarker", tol_metric = 0.05)
    expect_match(tnote(wide$resultsTable[[1]], "metric_tolerance"), "not</i> necessarily")
    expect_match(tnote(wide$resultsTable[[1]], "metric_tolerance"), "maximises the metric")

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


test_that("refVar is a required wrapper argument and NULL is accepted", {
    # refVar is a type: Level option, so the jamovi compiler forbids a default and it compiles
    # to a bare parameter. Callers must pass it; NULL is the value when no reference is wanted.
    expect_true("refVar" %in% names(formals(ClinicoPath::psychopdaROC)))
    expect_true(is.symbol(formals(ClinicoPath::psychopdaROC)$refVar))   # i.e. no default

    d <- ps_data()
    expect_error(ClinicoPath::psychopdaROC(data = d, dependentVars = "biomarker",
                                           classVar = "disease_status", positiveClass = "Disease"),
                 "refVar")
    expect_no_error(run_ps(data = d, dependentVars = "biomarker"))
})


test_that("a multi-level class variable is refused rather than guessed at", {
    # With two levels, assuming the last is a defensible guess and is disclosed. With three there
    # is no defensible guess: on a 3-level recode of the bundled data, assuming "Disease" gives
    # AUC 0.801152 and assuming "Severe" gives 0.826320. enhancedROC already declines a
    # multi-level outcome and asks for a positive class; this now matches it.
    d <- ps_data()
    lv <- as.character(d$disease_status)
    set.seed(5)
    idx <- which(lv == "Disease")
    lv[sample(idx, length(idx) %/% 2)] <- "Severe"
    d$ds3 <- factor(lv, levels = c("Healthy", "Disease", "Severe"))

    expect_error(run_ps(data = d, dependentVars = "biomarker", classVar = "ds3",
                        positiveClass = ""),
                 "cannot be guessed")
    # naming one explicitly still works
    expect_no_error(run_ps(data = d, dependentVars = "biomarker", classVar = "ds3",
                           positiveClass = "Severe"))
})


test_that("the positive class is never taken from data row order", {
    # A fallback inside .deLongTest used unique(classVar)[1] -- the first value in DATA ORDER, so
    # which class counted as positive depended on how the rows happened to be sorted, and it
    # disagreed with every other resolution in the analysis. It also warned via warning(), which
    # the jamovi GUI never displays.
    # strip comments first -- the fix's own explanatory comment names the old pattern
    code <- sub("#.*$", "", readLines("../../R/psychopdaroc.b.R", warn = FALSE))
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


test_that("the Hanley-McNeil fallback footnote states the right direction", {
    # The footnote said the approximation "may produce narrower confidence intervals than
    # appropriate". On the shipped data its SE is 0.025863 against DeLong's 0.021092 -- 22.6%
    # WIDER, i.e. conservative, the opposite of what the note warned about.
    backend <- paste(readLines("../../R/psychopdaroc.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("may produce narrower confidence intervals", backend, fixed = TRUE))
    expect_true(grepl("wider</i> than DeLong", backend, fixed = TRUE))

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
})


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/psychopdaroc.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    declared <- setdiff(declared, "data")
    backend <- paste(readLines("../../R/psychopdaroc.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})


test_that("the DeLong fallback reports the score inversion it performs", {
    # `.enhancedDelongTest` (the primary path) passes the user's `direction` straight to pROC
    # and reports the honest AUC, which may be below 0.5. The `.deLongTest` fallback instead
    # FLIPS any marker whose AUC came out below 0.5 -- standard for DeLong, but the flipped
    # value is what gets displayed. Measured below: the same column reads 0.206 in the main
    # AUC table and 0.794 in the DeLong output. Previously the only signal was warning(),
    # which never reaches a jamovi user.
    skip_if_not_installed("pROC")
    set.seed(42); n <- 120
    d <- data.frame(cls = rep(c("pos", "neg"), each = n / 2))
    d$good <- c(rnorm(n / 2, 1), rnorm(n / 2, 0))   # higher in positives
    d$flip <- c(rnorm(n / 2, 0), rnorm(n / 2, 1))   # lower  in positives -> AUC < 0.5

    o <- ClinicoPath:::psychopdaROCOptions$new(dependentVars = c("good", "flip"),
             classVar = "cls", positiveClass = "pos", direction = ">=", delongTest = TRUE)
    a <- ClinicoPath:::psychopdaROCClass$new(options = o, data = d)
    res <- a$.__enclos_env__$private$.deLongTest(
        data = d[, c("good", "flip")], classVar = d$cls, positiveClass = "pos")

    expect_equal(res$inverted, "flip")

    honest <- as.numeric(pROC::auc(pROC::roc(d$cls, d$flip, levels = c("neg", "pos"),
                                             direction = "<", quiet = TRUE)))
    expect_lt(honest, 0.5)                                   # the marker really is inverted
    expect_equal(unname(res$AUC[2, "AUC"]), 1 - honest, tolerance = 1e-6)   # and it was flipped

    # a marker that needs no flip is not named
    res2 <- a$.__enclos_env__$private$.deLongTest(
        data = d[, c("good", "good")], classVar = d$cls, positiveClass = "pos")
    expect_equal(res2$inverted, character(0))

    # the render path turns $inverted into visible text, not a silent warning()
    src <- paste(readLines("../../R/psychopdaroc.b.R", warn = FALSE), collapse = "\n")
    expect_match(src, "delongResults$inverted", fixed = TRUE)
    expect_match(src, "REVERSING the score direction for", fixed = TRUE)
    expect_match(src, "delong_inverted", fixed = TRUE)
    # and the helper no longer relies on warning() alone
    expect_false(grepl("Test direction was inverted for these variables", src, fixed = TRUE))
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
        o <- ClinicoPath:::psychopdaROCOptions$new(dependentVars = c("m1", "m2"),
                 classVar = "cls", positiveClass = "pos", direction = ">=", delongTest = TRUE)
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
