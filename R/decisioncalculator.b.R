#' @title Decision Calculator
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom utils data
#'
#' @return An \code{R6} class generator object for the \code{decisioncalculatorClass} backend; used internally by the jamovi analysis wrapper and not called directly.

decisioncalculatorClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class("decisioncalculatorClass",
        inherit = decisioncalculatorBase, private = list(
            # Clinical interpretation thresholds (evidence-based cutoffs)
            # Youden's Index thresholds based on diagnostic test literature
            .YOUDEN_EXCELLENT = 0.8, # J > 0.8: Excellent discriminatory ability
            .YOUDEN_GOOD = 0.6, # J > 0.6: Good discriminatory ability
            .YOUDEN_FAIR = 0.4, # J > 0.4: Fair discriminatory ability
            .ACCURACY_EXCELLENT = 0.9, # Accuracy > 0.9: Excellent overall performance
            .ACCURACY_GOOD = 0.8, # Accuracy > 0.8: Good overall performance

            # TODO [meddecide audit 2026-05-14] - see docs/audit/MODULE_AUDIT_REPORT_20260514-1847.md
            #   [hygiene/notices] uses a plain-text Preformatted "notices" output via private$.addNotice()/.renderNotices(); NOT jmvcore::Notice objects (avoids the insert(999, Notice) serialization error)
            #   [i18n] 0 .() wraps despite excellent notice content; high priority - /prepare-translation decisioncalculator
            #   [statistical-validation] /review-function decisioncalculator - confirm Bayes prior-override math
            #   [hygiene/notices] add INFO methodology summary at end of .run() (currently absent)
            #   [testing] no tests/testthat/test-decisioncalculator.R

            # Notice collection helpers. A single Preformatted (plain-text) output item:
            # avoids BOTH the jmvcore::Notice serialization error from
            # self$results$insert(999, Notice) AND any HTML in notices (project convention:
            # notice content must be plain text). ====
            .noticeList = list(),
            # A results item that may not exist in the compiled .h.R yet. jmvcore raises
            # rather than returning NULL, so a bare self$results$x would crash every run
            # between the .r.yaml edit and the next jmvtools::prepare().
            .resultsItem = function(name) tryCatch(self$results[[name]], error = function(e) NULL),

            .addNotice = function(type, title, content) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
                # Render immediately so early-return validation aborts still display the notice
                private$.renderNotices()
            },
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    self$results$notices$setContent("")
                    return()
                }

                # Plain text only - notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                blocks <- vapply(private$.noticeList, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = "ERROR: ",
                        STRONG_WARNING = "WARNING: ",
                        WARNING        = "WARNING: ",
                        INFO           = "INFO: ",
                        ""
                    )
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },
            .init = function() {
                # Welcome message
                welcome_html <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #333; padding: 20px; margin-bottom: 20px; color: inherit;'>
                <h2 style='margin: 0 0 10px 0; font-size: 18px; color: #333;'>Medical Decision Calculator</h2>
                <p style='margin: 0; font-size: 14px; color: #666;'>
                Comprehensive diagnostic test evaluation for clinical decision-making
                </p>
                </div>

                <div style='font-size: 14px; color: #333;'>
                <p><strong>What this tool does:</strong></p>
                <p>Evaluates diagnostic test performance by calculating sensitivity, specificity,
                predictive values, likelihood ratios, and advanced metrics from a 2\u{00D7}2 confusion matrix.</p>

                <p><strong>To get started:</strong></p>
                <ol style='margin: 10px 0; padding-left: 25px;'>
                    <li>Enter your four counts: TP (True Positives), FP (False Positives), TN (True Negatives), FN (False Negatives)</li>
                    <li>Choose whether to calculate confidence intervals (recommended)</li>
                    <li>Optionally enable summary, glossary, or about panels for additional guidance</li>
                </ol>

                <div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid #ccc; padding: 12px; margin: 15px 0; color: inherit;'>
                    <p style='margin: 0; font-size: 13px;'><strong>Quick Example:</strong>
                    If you tested 200 patients (100 diseased, 100 healthy) and your test correctly
                    identified 90 diseased (TP=90) and 80 healthy (TN=80), you have FN=10 and FP=20.</p>
                </div>
                </div>
            </div>
            "

                self$results$welcome$setContent(welcome_html)

                cTable <- self$results$cTable

                cTable$addRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = "Test Positive"
                    )
                )


                cTable$addRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = "Test Negative"
                    )
                )


                cTable$addRow(
                    rowKey = "Total",
                    values = list(
                        newtest = "Total"
                    )
                )

                # The cut-off comparison always has exactly these three rows -- the two
                # user-named scenarios and the current cut-off. Creating them here keeps the
                # structure stable and leaves .run() setting only the computed cells.
                multipleCutoffTable <- self$results$multipleCutoffTable
                multipleCutoffTable$addRow(
                    rowKey = 1,
                    values = list(cutoffName = self$options$cutoff1)
                )
                multipleCutoffTable$addRow(
                    rowKey = 2,
                    values = list(cutoffName = self$options$cutoff2)
                )
                multipleCutoffTable$addRow(
                    rowKey = 3,
                    values = list(cutoffName = "Current (Reference)")
                )
            },
            .run = function() {
                private$.noticeList <- list()

                # Read numbers from input ----
                TP <- self$options$TP
                FP <- self$options$FP
                TN <- self$options$TN
                FN <- self$options$FN

                # Read analysis options
                pp <- self$options$pp
                pprob <- self$options$pprob
                ci <- self$options$ci

                # Input validation ----
                # Enforce mutual exclusion of CI and custom prevalence (epiR limitation)
                if (ci && pp) {
                    private$.addNotice("ERROR", "CI and Population Prevalence Conflict", 'Confidence intervals are unavailable when using population prevalence. \u{2022} epiR::epi.tests() computes CIs from the study sample, not an externally supplied prevalence. \u{2022} Disable either "95% Confidence Intervals" or "Use Known Population Prevalence" to proceed. \u{2022} Point estimates will still be Bayes-adjusted when prevalence is supplied.')
                    return()
                }

                # Inform user that CIs are unavailable when using population prevalence
                if (!ci && pp) {
                    private$.addNotice("INFO", "CI Unavailable With Population Prevalence", "Confidence intervals are not calculated when using population prevalence. \u{2022} epiR::epi.tests() provides CIs only for sample-based prevalence. \u{2022} Point estimates shown here are Bayes-adjusted to the supplied prevalence without CIs.")
                }

                # Validate prevalence when provided programmatically
                if (pp && (is.na(pprob) || pprob <= 0 || pprob >= 1)) {
                    private$.addNotice("ERROR", "Invalid Prior Probability", 'Invalid prior probability. \u{2022} Prior probability must be between 0 and 1 (exclusive). \u{2022} Update the "Prior Probability (prevalence)" value.')
                    return()
                }

                # Reject non-finite inputs
                if (any(!is.finite(c(TP, FP, TN, FN)))) {
                    private$.addNotice("ERROR", "Non-Finite Counts", "Non-finite counts detected. \u{2022} TP, FP, TN, and FN must be finite numbers. \u{2022} Please check your input values.")
                    return()
                }

                # Check for non-negative values
                if (TP < 0 || FP < 0 || TN < 0 || FN < 0) {
                    private$.addNotice("ERROR", "Negative Counts Detected", "Negative counts detected. \u{2022} All counts (TP, FP, TN, FN) must be non-negative. \u{2022} Please check your input values for errors.")
                    return()
                }

                # Check for at least some data
                if (TP + FP + TN + FN == 0) {
                    private$.addNotice("ERROR", "All Counts Zero", "All counts are zero. \u{2022} Please provide valid diagnostic test data. \u{2022} Ensure TP, FP, TN, and FN values are entered correctly.")
                    return()
                }

                # Check for diseased subjects
                if (TP + FN == 0) {
                    private$.addNotice("ERROR", "No Diseased Subjects", "No diseased subjects detected (TP + FN = 0). \u{2022} Cannot calculate sensitivity and related metrics. \u{2022} Ensure your confusion matrix includes cases with disease present.")
                    return()
                }

                # Check for healthy subjects
                if (TN + FP == 0) {
                    private$.addNotice("ERROR", "No Healthy Subjects", "No healthy subjects detected (TN + FP = 0). \u{2022} Cannot calculate specificity and related metrics. \u{2022} Ensure your confusion matrix includes cases without disease.")
                    return()
                }

                # Check for positive tests
                if (TP + FP == 0) {
                    private$.addNotice("WARNING", "No Positive Tests", "No positive test results detected (TP + FP = 0). \u{2022} Positive Predictive Value (PPV) is undefined. \u{2022} Ensure your confusion matrix includes both positive and negative test results.")
                }

                # Check for negative tests
                if (TN + FN == 0) {
                    private$.addNotice("WARNING", "No Negative Tests", "No negative test results detected (TN + FN = 0). \u{2022} Negative Predictive Value (NPV) is undefined. \u{2022} Ensure your confusion matrix includes both positive and negative test results.")
                }


                # Create confusion matrix ----

                table2 <- matrix(c(TP, FP, FN, TN),
                    nrow = 2, ncol = 2, byrow = TRUE,
                    dimnames = list(c("Positive", "Negative"), c("Positive", "Negative"))
                )

                table3 <- as.table(table2)

                names(attributes(table3)$dimnames) <- c("Test", "Golden Standard")

                # Prior Probability ----
                # (pp and pprob already read at top of function for validation)


                # Cross Table in jamovi style ----

                cTable <- self$results$cTable


                cTable$setRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = "Test Positive",
                        GP = TP,
                        GN = FP,
                        Total = TP + FP
                    )
                )


                cTable$setRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = "Test Negative",
                        GP = FN,
                        GN = TN,
                        Total = FN + TN
                    )
                )

                cTable$setRow(
                    rowKey = "Total",
                    values = list(
                        newtest = "Total",
                        GP = TP + FN,
                        GN = FP + TN,
                        Total = TP + FP + FN + TN
                    )
                )


                # Self Calculations ----

                # Self Calculation https://cran.r-project.org/web/packages/caret/caret.pdf
                # https://online.stat.psu.edu/stat509/node/150/

                # https://en.wikipedia.org/wiki/Sensitivity_and_specificity

                TotalPop <- TP + TN + FP + FN

                DiseaseP <- TP + FN

                DiseaseN <- TN + FP

                TestP <- TP + FP

                TestN <- TN + FN

                TestT <- TP + TN

                TestW <- FP + FN

                # Warn on non-integer counts (allowed but unusual)
                if (any(abs(c(TP, FP, TN, FN) - round(c(TP, FP, TN, FN))) > 1e-6)) {
                    private$.addNotice("WARNING", "Non-Integer Counts", "Non-integer counts detected. \u{2022} Diagnostic test counts are typically whole numbers. \u{2022} Proceeding with calculations, but verify your inputs.")
                }

                # Continuity correction for zero-cell issues (stabilizes the LR/DOR point
                # estimates only; epi.tests() below is fed the raw table3, so the CIs are
                # uncorrected)
                zero_cell <- any(c(TP, FP, TN, FN) == 0)
                TP_cc <- TP
                FP_cc <- FP
                TN_cc <- TN
                FN_cc <- FN

                if (zero_cell) {
                    TP_cc <- TP + 0.5
                    FP_cc <- FP + 0.5
                    TN_cc <- TN + 0.5
                    FN_cc <- FN + 0.5

                    private$.addNotice("WARNING", "Continuity Correction Applied", "Zero cells detected. Applied Haldane-Anscombe 0.5 continuity correction to the likelihood ratio and diagnostic odds ratio point estimates to avoid infinite or undefined values. \u{2022} The 95% confidence intervals come from epiR::epi.tests() on the raw (uncorrected) counts, so they are not continuity-corrected and may be undefined for these statistics.")
                }

                # Calculate metrics with safe division
                Sens <- if (DiseaseP > 0) TP / DiseaseP else 0
                Spec <- if (DiseaseN > 0) TN / DiseaseN else 0
                AccurT <- if (TotalPop > 0) TestT / TotalPop else 0
                PrevalenceD <- if (TotalPop > 0) DiseaseP / TotalPop else 0

                # Determine which prevalence to use
                if (pp) {
                    # Known prior probability from population
                    PriorProb <- pprob
                } else {
                    # From ConfusionMatrix
                    PriorProb <- PrevalenceD
                }

                # CRITICAL FIX: Calculate PPV and NPV using the selected prevalence
                # When pp=TRUE, PPV/NPV must use population prevalence via Bayes' theorem,
                # NOT the sample-based TP/(TP+FP) calculation
                if (pp) {
                    # Population-adjusted PPV/NPV using Bayes' theorem
                    PPV <- if (!is.na(Sens) && !is.na(Spec)) {
                        numerator <- Sens * PriorProb
                        denominator <- (Sens * PriorProb) + ((1 - Spec) * (1 - PriorProb))
                        if (denominator > 0) numerator / denominator else NA
                    } else {
                        NA
                    }

                    NPV <- if (!is.na(Sens) && !is.na(Spec)) {
                        numerator <- Spec * (1 - PriorProb)
                        denominator <- (Spec * (1 - PriorProb)) + ((1 - Sens) * PriorProb)
                        if (denominator > 0) numerator / denominator else NA
                    } else {
                        NA
                    }
                } else {
                    # Sample-based PPV/NPV
                    PPV <- if (TestP > 0) TP / TestP else NA
                    NPV <- if (TestN > 0) TN / TestN else NA
                }

                # Post-test probabilities (same as PPV/NPV when using population prevalence)
                PostTestProbDisease <- if (!is.na(Sens) && !is.na(Spec)) {
                    numerator <- PriorProb * Sens
                    denominator <- (PriorProb * Sens) + ((1 - PriorProb) * (1 - Spec))
                    if (denominator > 0) numerator / denominator else NA
                } else {
                    NA
                }

                PostTestProbHealthy <- if (!is.na(Sens) && !is.na(Spec)) {
                    numerator <- (1 - PriorProb) * Spec
                    denominator <- ((1 - PriorProb) * Spec) + (PriorProb * (1 - Sens))
                    if (denominator > 0) numerator / denominator else NA
                } else {
                    NA
                }


                # Calculate likelihood ratios with safe division (corrected if needed)
                LRP <- if ((1 - (TN_cc / (TN_cc + FP_cc))) > 0) {
                    (TP_cc / (TP_cc + FN_cc)) / (1 - (TN_cc / (TN_cc + FP_cc)))
                } else {
                    Inf
                }

                LRN <- if ((TN_cc / (TN_cc + FP_cc)) > 0) {
                    (1 - (TP_cc / (TP_cc + FN_cc))) / (TN_cc / (TN_cc + FP_cc))
                } else {
                    0
                }

                # Diagnostic Odds Ratio (Haldane-Anscombe corrected counts).
                # Displayed in advancedMetricsTable; confidence intervals for all
                # metrics are supplied by epiR::epi.tests() (see the ci block below).
                if (FN_cc > 0 && FP_cc > 0) {
                    DOR <- (TP_cc * TN_cc) / (FN_cc * FP_cc)
                } else {
                    DOR <- NA
                }

                # Youden's Index (optimal cut-off criterion)
                YoudenIndex <- Sens + Spec - 1

                # Balanced Accuracy (useful when dealing with imbalanced data)
                BalancedAccuracy <- (Sens + Spec) / 2

                # F1 Score (harmonic mean of sensitivity and PPV)
                if (Sens > 0 && PPV > 0) {
                    F1Score <- 2 * (Sens * PPV) / (Sens + PPV)
                } else {
                    F1Score <- 0
                }

                # Matthews Correlation Coefficient (MCC)
                mcc_numerator <- (TP * TN) - (FP * FN)
                mcc_denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
                if (mcc_denominator > 0) {
                    MCC <- mcc_numerator / mcc_denominator
                } else {
                    MCC <- 0
                }


                # nTable Populate Table ----

                nTable <- self$results$nTable
                nTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "n",
                        TotalPop = TotalPop,
                        DiseaseP = DiseaseP,
                        DiseaseN = DiseaseN,
                        TestP = TestP,
                        TestN = TestN,
                        TestT = TestT,
                        TestW = TestW
                    )
                )

                # ratioTable Populate Table ----


                ratioTable <- self$results$ratioTable
                ratioTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "Ratios",
                        Sens = Sens,
                        Spec = Spec,
                        AccurT = AccurT,
                        PrevalenceD = PriorProb,
                        PPV = PPV,
                        NPV = NPV,
                        PostTestProbDisease = PostTestProbDisease,
                        PostTestProbHealthy = PostTestProbHealthy,
                        LRP = LRP,
                        LRN = LRN
                    )
                )


                # nTable footnotes ----

                if (self$options$fnote) {
                    # nTable$addFootnote(rowKey = "1", col = "TotalPop", "Total Population")

                    nTable$addFootnote(rowNo = 1, col = "TotalPop", "Total Number of Subjects")

                    nTable$addFootnote(rowNo = 1, col = "DiseaseP", "Total Number of Subjects with Disease")

                    nTable$addFootnote(rowNo = 1, col = "DiseaseN", "Total Number of Healthy Subjects")

                    nTable$addFootnote(rowNo = 1, col = "TestP", "Total Number of Positive Tests")

                    nTable$addFootnote(rowNo = 1, col = "TestN", "Total Number of Negative Tests")

                    nTable$addFootnote(rowNo = 1, col = "TestT", "Total Number of True Test Results")

                    nTable$addFootnote(rowNo = 1, col = "TestW", "Total Number of Wrong Test Results")
                }


                # ratioTable footnotes ----


                if (self$options$fnote) {
                    ratioTable$addFootnote(rowNo = 1, col = "Sens", "Sensitivity (True Positives among Diseased)")

                    ratioTable$addFootnote(rowNo = 1, col = "Spec", "Specificity (True Negatives among Healthy)")

                    ratioTable$addFootnote(rowNo = 1, col = "AccurT", sprintf(
                        "Accuracy (proportion of all test results that were correct). Unlike sensitivity and specificity, accuracy depends on disease prevalence: it is computed here at the prevalence observed in this sample (%.1f%%)%s, and it will differ in a population with a different case mix.",
                        PrevalenceD * 100,
                        if (pp) ", not at the population prevalence shown in the Prevalence column" else ""
                    ))

                    prev_note <- if (pp) {
                        "Prevalence used: the user-supplied population prevalence (prior probability), not the prevalence observed in this study sample."
                    } else {
                        "Disease prevalence observed in this study sample."
                    }
                    ratioTable$addFootnote(rowNo = 1, col = "PrevalenceD", prev_note)

                    ppv_note <- if (pp) {
                        "Positive Predictive Value (Probability of disease after a positive test using supplied population prevalence)"
                    } else {
                        "Positive Predictive Value (Probability of having disease after a positive test using this study population)"
                    }
                    npv_note <- if (pp) {
                        "Negative Predictive Value (Probability of being healthy after a negative test using supplied population prevalence)"
                    } else {
                        "Negative Predictive Value (Probability of being healthy after a negative test using this study population)"
                    }

                    ratioTable$addFootnote(rowNo = 1, col = "PPV", ppv_note)

                    ratioTable$addFootnote(rowNo = 1, col = "NPV", npv_note)

                    ratioTable$addFootnote(rowNo = 1, col = "PostTestProbDisease", "Post-test Probability of Having Disease (Probability of having disease after a positive test using the prevalence above). Mathematically identical to the PPV shown here.")

                    ratioTable$addFootnote(rowNo = 1, col = "PostTestProbHealthy", "Post-test Probability of Being Healthy (Probability of being healthy after a negative test using the prevalence above). Mathematically identical to the NPV shown here.")

                    ratioTable$addFootnote(rowNo = 1, col = "LRP", "Positive Likelihood Ratio: How much more likely a positive result is in diseased vs healthy patients. >10 = strong evidence, >5 = moderate, >2 = weak but potentially useful.")

                    ratioTable$addFootnote(rowNo = 1, col = "LRN", "Negative Likelihood Ratio: How much more likely a negative result is in diseased vs healthy patients. <0.1 = strong evidence against disease, <0.2 = moderate, <0.5 = weak.")

                    if (zero_cell) {
                        ratioTable$addFootnote(rowNo = 1, col = "LRP", "A Haldane-Anscombe 0.5 continuity correction was applied to the likelihood ratios in this table because a zero cell was present. The epiR confidence-interval table below uses raw (uncorrected) counts, so its LR point estimates may differ.")
                    }
                }


                # Populate advanced metrics table ----
                advancedMetricsTable <- self$results$advancedMetricsTable
                advancedMetricsTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "Advanced Metrics",
                        youdenIndex = YoudenIndex,
                        balancedAccuracy = BalancedAccuracy,
                        f1Score = F1Score,
                        mcc = MCC,
                        dor = DOR
                    )
                )

                # Add footnotes for advanced metrics
                if (self$options$fnote) {
                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "youdenIndex",
                        "Youden's Index: Discriminatory ability independent of prevalence. >0.8 excellent, 0.6-0.8 good, 0.4-0.6 fair, <0.4 poor."
                    )

                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "balancedAccuracy",
                        "Balanced Accuracy: Average of sensitivity and specificity. Useful for imbalanced datasets."
                    )

                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "f1Score",
                        "F1 Score: Harmonic mean of sensitivity and PPV. Ranges 0-1, higher is better."
                    )

                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "mcc",
                        "Matthews Correlation Coefficient: Overall test quality measure. Ranges -1 to +1. >0.8 excellent, 0.6-0.8 good, 0.4-0.6 fair."
                    )

                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "dor",
                        "Diagnostic Odds Ratio: Overall discriminatory performance. >25 strong, 5-25 moderate, 2-5 weak, <2 poor."
                    )
                }


                # 95% CI ----

                ci <- self$options$ci

                if (ci) {
                    # Check if epiR package is available
                    if (!requireNamespace("epiR", quietly = TRUE)) {
                        private$.addNotice("ERROR", "epiR Package Missing", 'epiR package is required for confidence intervals. \u{2022} Install with install.packages("epiR"). \u{2022} Or disable "95% Confidence Intervals" option.')
                        return()
                    }

                    # epiR ----

                    epirresult <- epiR::epi.tests(dat = table3)
                    # self$results$text3$setContent(epirresult)


                    epirresult2 <- summary(epirresult)
                    epirresult2 <- as.data.frame(epirresult2) %>%
                        tibble::rownames_to_column(.data = ., var = "statsabv")


                    # Map epiR statistic codes to display names via a named lookup keyed on
                    # the `statistic` column. Robust to epiR version changes in row count or
                    # order; the previous positional c(<18 names>) assignment errored
                    # ("replacement has 18 rows") or mislabelled if the summary changed shape.
                    # Codes not listed here are dropped downstream by the ratiorows/numberrows
                    # filters, so they need no label.
                    stat_labels <- c(
                        "ap"      = "Apparent prevalence",
                        "tp"      = "True prevalence",
                        "se"      = "Test sensitivity",
                        "sp"      = "Test specificity",
                        "diag.ac" = "Diagnostic accuracy",
                        "diag.or" = "Diagnostic odds ratio",
                        "nndx"    = "Number needed to diagnose",
                        "youden"  = "Youden's index",
                        "pv.pos"  = "Positive predictive value",
                        "pv.neg"  = "Negative predictive value",
                        "lr.pos"  = "Likelihood ratio of a positive test",
                        "lr.neg"  = "Likelihood ratio of a negative test",
                        "p.tpdn"  = "Proportion of subjects with the outcome ruled out",
                        "p.tndp"  = "Proportion of subjects with the outcome ruled in",
                        "p.dntp"  = "Proportion of false positives",
                        "p.dptn"  = "Proportion of false negative"
                    )
                    epirresult2$statsnames <- unname(stat_labels[epirresult2$statistic])

                    ratiorows <- c(
                        "ap",
                        "tp",
                        "se",
                        "sp",
                        "diag.ac",
                        "pv.pos",
                        "pv.neg",
                        "p.tpdn",
                        "p.tndp",
                        "p.dntp",
                        "p.dptn"
                    )


                    numberrows <- c(
                        "diag.or",
                        "nndx",
                        "youden",
                        "lr.pos",
                        "lr.neg"
                    )

                    epirresult_number <- epirresult2[epirresult2$statistic %in% numberrows, ]

                    epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, ]

                    # Enhanced metrics from DiagROC - add to existing results
                    # Add Balanced Accuracy
                    balanced_acc_row <- data.frame(
                        statistic = "bal.acc",
                        est = BalancedAccuracy,
                        lower = NA_real_,
                        upper = NA_real_,
                        statsabv = "bal.acc",
                        statsnames = "Balanced accuracy (CI not computed)",
                        stringsAsFactors = FALSE
                    )

                    # Add F1 Score
                    f1_row <- data.frame(
                        statistic = "f1.score",
                        est = F1Score,
                        lower = NA_real_, # CI not computed
                        upper = NA_real_,
                        statsabv = "f1.score",
                        statsnames = "F1 score (CI not computed)",
                        stringsAsFactors = FALSE
                    )

                    # Combine enhanced metrics with existing epiR results.
                    # Balanced accuracy and F1 are proportions in [0, 1], so they render
                    # correctly in this percent-formatted (format: pc) table.
                    # MCC is intentionally excluded: it ranges [-1, +1], so pc formatting would
                    # display it misleadingly (e.g. 0.4 as "40%", or a negative value as "-30%").
                    # MCC is shown as a plain number in advancedMetricsTable instead.
                    epirresult_ratio <- rbind(epirresult_ratio, balanced_acc_row, f1_row)


                    # epirTable_ratio -----

                    epirTable_ratio <- self$results$epirTable_ratio
                    epirTable_ratio$deleteRows()

                    data_frame <- epirresult_ratio
                    for (i in seq_along(data_frame[, 1, drop = T])) {
                        epirTable_ratio$addRow(rowKey = i, values = c(data_frame[i, ])) # This code produces a named vector/list, which is what the values argument expects
                    }


                    # epirTable_number ----


                    epirTable_number <- self$results$epirTable_number
                    epirTable_number$deleteRows()

                    data_frame <- epirresult_number
                    for (i in seq_along(data_frame[, 1, drop = T])) {
                        epirTable_number$addRow(rowKey = i, values = c(data_frame[i, ]))
                    }
                }

                # Multiple cut-off evaluation (DiagROC inspired)
                if (self$options$multiplecuts) {
                    multipleCutoffTable <- self$results$multipleCutoffTable

                    # Helper function to calculate metrics for a cut-off
                    calculate_cutoff_metrics <- function(tp, fp, tn, fn, cutoff_name) {
                        # Validate inputs and guard against zero/NA division
                        if (any(is.na(c(tp, fp, tn, fn))) || any(c(tp, fp, tn, fn) < 0)) {
                            private$.addNotice("ERROR", "Invalid Cut-off Inputs", sprintf('Invalid inputs for cut-off "%s". \u{2022} All values (TP=%s, FP=%s, TN=%s, FN=%s) must be non-negative numbers. \u{2022} Check your input values for errors.', cutoff_name, tp, fp, tn, fn))
                            return(NULL)
                        }

                        total <- tp + fp + tn + fn
                        diseased <- tp + fn
                        healthy <- tn + fp

                        # Validate that we have cases to analyze
                        if (total == 0) {
                            private$.addNotice("ERROR", "No Cases For Cut-off", sprintf('No cases for cut-off "%s". \u{2022} Total cases (TP+FP+TN+FN) = 0. \u{2022} Check your confusion matrix inputs.', cutoff_name))
                            return(NULL)
                        }

                        # Safe division with appropriate handling for zero denominators
                        sens <- if (diseased > 0) tp / diseased else NA_real_
                        spec <- if (healthy > 0) tn / healthy else NA_real_
                        ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
                        npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
                        accuracy <- if (total > 0) (tp + tn) / total else NA_real_

                        # Youden index only defined when both sens and spec are available
                        youden <- if (!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA_real_

                        # Warn if metrics are undefined
                        if (diseased == 0 || healthy == 0) {
                            msg <- sprintf('Cut-off "%s" has incomplete data.', cutoff_name)
                            if (diseased == 0) msg <- paste0(msg, " \u{2022} No diseased cases (TP+FN=0): Sensitivity is undefined.")
                            if (healthy == 0) msg <- paste0(msg, " \u{2022} No healthy cases (TN+FP=0): Specificity is undefined.")
                            msg <- paste0(msg, " \u{2022} The metrics shown for this cut-off are computed from incomplete counts.")
                            private$.addNotice("WARNING", "Incomplete Cut-off Data", msg)
                        }

                        # Descriptive grade of the point estimates in THIS sample.
                        # Not a recommendation: the counts carry no interval, and the
                        # cut-offs are graded on the same data that produced them.
                        if (is.na(youden) || is.na(accuracy)) {
                            recommendation <- "Incomplete data - cannot be graded"
                        } else if (youden > private$.YOUDEN_EXCELLENT && accuracy > private$.ACCURACY_EXCELLENT) {
                            recommendation <- "Excellent in this sample"
                        } else if (youden > private$.YOUDEN_GOOD && accuracy > private$.ACCURACY_GOOD) {
                            recommendation <- "Good in this sample"
                        } else if (youden > private$.YOUDEN_FAIR) {
                            recommendation <- "Fair in this sample"
                        } else {
                            recommendation <- "Poor in this sample"
                        }

                        return(list(
                            cutoffName = cutoff_name,
                            sensitivity = sens,
                            specificity = spec,
                            ppv = ppv,
                            npv = npv,
                            accuracy = accuracy,
                            youden = youden,
                            recommendation = recommendation
                        ))
                    }

                    # Calculate metrics for both cut-offs
                    cutoff1_metrics <- calculate_cutoff_metrics(
                        self$options$tp1, self$options$fp1,
                        self$options$tn1, self$options$fn1,
                        self$options$cutoff1
                    )

                    cutoff2_metrics <- calculate_cutoff_metrics(
                        self$options$tp2, self$options$fp2,
                        self$options$tn2, self$options$fn2,
                        self$options$cutoff2
                    )

                    # Skip table population if validation failed
                    if (is.null(cutoff1_metrics) || is.null(cutoff2_metrics)) {
                        private$.addNotice("ERROR", "Cut-off Validation Failed", "Cut-off comparison cannot be performed due to invalid inputs. \u{2022} Check the error messages above for specific issues. \u{2022} Ensure all TP, FP, TN, FN values are non-negative numbers.")
                        return()
                    }

                    # Rows are created in .init(); only the computed cells are set here.
                    multipleCutoffTable$setRow(
                        rowKey = 1,
                        values = cutoff1_metrics
                    )

                    multipleCutoffTable$setRow(
                        rowKey = 2,
                        values = cutoff2_metrics
                    )

                    # Optimal cut-off recommendation.
                    #
                    # Three defects were fixed here:
                    #  1. The chain was `if (cutoff1 beats current) ... else if (cutoff2 ...)`,
                    #     so when BOTH alternatives beat the current one only cutoff1 was ever
                    #     named -- even when cutoff2 was far better. Now the best of the three
                    #     is chosen on Youden's J.
                    #  2. The verdict was a bare point-estimate comparison with no uncertainty:
                    #     an advantage of 0.001 in both Youden and accuracy printed
                    #     "performs better than current". It now says how large the margin is
                    #     and warns when it is slight.
                    #  3. A cut-off comparison only means anything if the scenarios are
                    #     thresholds on the SAME cohort, so their totals must match. Nothing
                    #     checked that; a 220-case study could be compared with a 300-case one
                    #     as though they were two thresholds.
                    current_youden <- YoudenIndex
                    current_accuracy <- AccurT

                    n_current <- TP + FP + TN + FN
                    n1 <- self$options$tp1 + self$options$fp1 + self$options$tn1 + self$options$fn1
                    n2 <- self$options$tp2 + self$options$fp2 + self$options$tn2 + self$options$fn2
                    if (length(unique(c(n_current, n1, n2))) > 1) {
                        private$.addNotice(
                            "WARNING",
                            "Cut-offs describe different numbers of patients",
                            sprintf("The three scenarios total %g, %g and %g cases. Moving a cut-off on one cohort cannot change how many patients there are, so these are different studies rather than different thresholds, and comparing them here treats between-study variation as if it were a threshold effect. Check the counts, or interpret each row on its own.",
                                    n_current, n1, n2)
                        )
                    }

                    cand <- list(
                        list(name = "Current", youden = current_youden, accuracy = current_accuracy),
                        list(name = cutoff1_metrics$cutoffName, youden = cutoff1_metrics$youden,
                             accuracy = cutoff1_metrics$accuracy),
                        list(name = cutoff2_metrics$cutoffName, youden = cutoff2_metrics$youden,
                             accuracy = cutoff2_metrics$accuracy)
                    )
                    ys <- vapply(cand, function(x)
                        if (is.null(x$youden) || !is.finite(x$youden)) NA_real_ else x$youden,
                        numeric(1))

                    # Uncertainty. A formal test of two cut-offs would need the paired
                    # discordance between them, which four marginal counts per scenario
                    # cannot supply. What IS computable is a Wilson interval on each
                    # scenario's ACCURACY, so the reader can see whether the intervals
                    # overlap -- overlapping intervals mean the counts do not separate the
                    # cut-offs, whatever the point estimates suggest.
                    wilson <- function(x, n, conf = 0.95) {
                        if (!is.finite(x) || !is.finite(n) || n <= 0)
                            return(c(NA_real_, NA_real_))
                        z <- stats::qnorm(1 - (1 - conf) / 2)
                        ph <- x / n
                        den <- 1 + z^2 / n
                        ctr <- (ph + z^2 / (2 * n)) / den
                        hw <- z * sqrt((ph * (1 - ph) + z^2 / (4 * n)) / n) / den
                        c(max(0, ctr - hw), min(1, ctr + hw))
                    }
                    acc_ci <- list(
                        wilson(TP + TN, n_current),
                        wilson(self$options$tp1 + self$options$tn1, n1),
                        wilson(self$options$tp2 + self$options$tn2, n2)
                    )

                    optimal_msg <- "Current cut-off appears optimal"
                    if (any(is.finite(ys))) {
                        best <- which.max(replace(ys, !is.finite(ys), -Inf))
                        margin <- ys[best] - ys[1]                      # vs the current cut-off
                        runner <- sort(ys[is.finite(ys)], decreasing = TRUE)
                        gap <- if (length(runner) > 1) runner[1] - runner[2] else NA_real_

                        # do the best and current accuracy intervals overlap?
                        ov <- NA
                        if (best != 1L && all(is.finite(c(acc_ci[[best]], acc_ci[[1]]))))
                            ov <- acc_ci[[best]][1] <= acc_ci[[1]][2] &&
                                  acc_ci[[1]][1] <= acc_ci[[best]][2]

                        if (best == 1L || !is.finite(margin) || margin <= 0) {
                            optimal_msg <- sprintf("Current cut-off has the highest Youden's J (%.3f) of the three", ys[1])
                        } else if (margin < 0.05) {
                            optimal_msg <- sprintf("%s is higher by only %.3f Youden's J - too small to distinguish these cut-offs on these counts alone",
                                                   cand[[best]]$name, margin)
                        } else {
                            optimal_msg <- sprintf("%s has the highest Youden's J, %.3f above current%s%s",
                                                   cand[[best]]$name, margin,
                                                   if (is.finite(gap) && gap < 0.02)
                                                       " (but barely ahead of the next cut-off)" else "",
                                                   if (isTRUE(ov))
                                                       "; its accuracy interval still overlaps the current cut-off's, so the difference is not established"
                                                   else if (isFALSE(ov))
                                                       "; their accuracy intervals do not overlap"
                                                   else "")
                        }
                    }

                    multipleCutoffTable$setNote(
                        "sameData",
                        jmvcore::.("This column holds two different kinds of statement. On the two named cut-off rows it is a grade of that scenario's point estimates in this sample; the good and excellent grades require Youden's index AND accuracy to reach the band together, so a grade can be lower than either column on its own would suggest. On the Current (Reference) row it is not a grade at all: it reports which of the three cut-offs had the highest Youden's J, by how much, and whether that cut-off's accuracy interval still overlaps the current one's. Because all three cut-offs are judged on the same counts used to evaluate them, whichever row comes out ahead is optimistically biased; performance in independent data is generally lower.")
                    )

                    multipleCutoffTable$setNote(
                        "uncertainty",
                        jmvcore::.("Cut-offs are compared on point estimates only. A formal test would need to know, for each patient, how the two thresholds classified them; four summary counts per scenario cannot supply that. The accuracy intervals referred to above are Wilson 95% intervals computed separately per scenario, so overlap is a conservative signal that the counts do not separate the cut-offs.")
                    )

                    multipleCutoffTable$setRow(
                        rowKey = 3,
                        values = list(
                            cutoffName = "Current (Reference)",
                            sensitivity = Sens,
                            specificity = Spec,
                            ppv = PPV,
                            npv = NPV,
                            accuracy = AccurT,
                            youden = YoudenIndex,
                            recommendation = optimal_msg
                        )
                    )
                }

                # Generate Summary, About, and Glossary panels ----

                # Summary panel
                if (self$options$showSummary) {
                    summary_html <- private$.createSummary(
                        Sens, Spec, PPV, NPV, LRP, LRN,
                        YoudenIndex, AccurT, PriorProb
                    )
                    self$results$summary$setContent(summary_html)
                }

                # About and Assumptions panels
                if (self$options$showAbout) {
                    about_html <- private$.createAboutPanel()
                    self$results$about$setContent(about_html)

                    assumptions_html <- private$.createAssumptionsPanel(TP, TN, FP, FN, PriorProb)
                    self$results$assumptions$setContent(assumptions_html)
                }

                # Glossary panel
                if (self$options$showGlossary) {
                    glossary_html <- private$.createGlossary()
                    self$results$glossary$setContent(glossary_html)
                }

                # Send Data to Plot ----


                # A zero cell puts sensitivity or specificity at exactly 0 or 1, and
                # nomogrammer rejects the closed bounds outright ("must be between 0 and 1
                # (exclusive)"), so the whole nomogram silently failed to draw for precisely
                # the sparse tables that most need one. Confirmed for FP=0, FN=0 and TP=0.
                # The likelihood ratios handed to it are already Haldane-Anscombe corrected
                # (TP_cc..FN_cc above), so pass the proportions from that same corrected
                # table -- the plot is then self-consistent rather than clamped to an
                # arbitrary epsilon. The tables keep the uncorrected values.
                sens_plot <- TP_cc / (TP_cc + FN_cc)
                spec_plot <- TN_cc / (TN_cc + FP_cc)

                # nomogrammer also refuses LR+ < 1 ("should be >= 1 for an informative
                # test"), which is a fair objection -- a positive result that argues AGAINST
                # disease inverts the nomogram's meaning -- but it arrived as an unexplained
                # crash. Detect it here, where a notice can still be rendered, and tell the
                # plot to decline instead.
                fagan_ok <- is.finite(LRP) && is.finite(LRN) && LRP >= 1
                if (isTRUE(self$options$fagan) && !fagan_ok) {
                    private$.addNotice(
                        "WARNING",
                        "Fagan nomogram not drawn",
                        sprintf("The positive likelihood ratio is %.3f. A nomogram assumes a positive result raises the probability of disease (LR+ >= 1); here a positive result lowers it, so the plot would be misleading and has been omitted. This usually means the test's coding is inverted -- check that TP and FP are not swapped -- or that the test genuinely performs worse than chance.",
                                LRP)
                    )
                }

                plotData1 <- list(
                    "Prevalence" = PriorProb,
                    "Sens" = sens_plot,
                    "Spec" = spec_plot,
                    "Plr" = LRP,
                    "Nlr" = LRN,
                    "drawable" = fagan_ok
                )

                image1 <- self$results$plot1
                image1$setState(plotData1)

                # nomogrammer prints a summary block to the R console when Verbose = TRUE --
                # prevalence, the two likelihood ratios, and the post-test probabilities.
                # jamovi never shows stdout, so that reading was invisible to every user. It
                # is the most clinically useful part of the figure, so render it beside the
                # plot instead, at the tables' precision rather than nomogrammer's whole
                # percents.
                fagan_item <- private$.resultsItem("faganSummary")
                if (!is.null(fagan_item) && isTRUE(self$options$fagan)) {
                    if (!fagan_ok) {
                        fagan_item$setContent(paste0(
                            "<div style='padding:12px;border-left:4px solid #c00;background-color: rgba(255, 88, 88, 0.06); color: inherit;'>",
                            "<p>", .("No nomogram is drawn for this table."), " ",
                            sprintf(.("The positive likelihood ratio is %.3f, so a positive result lowers the probability of disease rather than raising it."), LRP),
                            "</p></div>"))
                    } else {
                        pre <- PriorProb
                        post_pos <- PostTestProbDisease
                        post_neg <- 1 - PostTestProbHealthy
                        src <- if (isTRUE(self$options$pp))
                            .("the population prevalence you supplied") else .("this study's own prevalence")
                        fagan_item$setContent(paste0(
                            "<div style='padding:12px;border-left:4px solid #1565c0;background-color: rgba(88, 155, 255, 0.06); color: inherit;'>",
                            "<p><b>", .("Pre-test probability"), ":</b> ",
                            sprintf("%.1f%%", 100 * pre), " \u{2014} ", src, ".</p>",
                            "<p><b>", .("If the test is POSITIVE"), ":</b> ",
                            sprintf(.("likelihood ratio %.2f raises the probability from %.1f%% to <b>%.1f%%</b>."),
                                    LRP, 100 * pre, 100 * post_pos), "</p>",
                            "<p><b>", .("If the test is NEGATIVE"), ":</b> ",
                            sprintf(.("likelihood ratio %.3f lowers the probability from %.1f%% to <b>%.1f%%</b>."),
                                    LRN, 100 * pre, 100 * post_neg), "</p>",
                            "<p style='font-size:90%;color:#555;'>",
                            .("Read the nomogram by drawing a line from the pre-test probability on the left, through the likelihood ratio in the middle, to the post-test probability on the right. Sensitivity and specificity are properties of the test; the pre-test probability is not, so the same test moves a patient to a different endpoint in a different population."),
                            "</p></div>"))
                    }
                }

                # plotData2 <- plotData1
                #
                # image2 <- self$results$plot2
                # image2$setState(plotData2)

                # Render collected notices once at the end of a successful run. This also
                # clears the panel when no notice was raised this cycle (.noticeList was
                # reset at the top of .run()), preventing a stale notice from persisting
                # after the user corrects an input not covered by the notices clearWith.
                private$.renderNotices()
            },
            .plot1 = function(image1, ggtheme, ...) {
                plotData1 <- image1$state
                if (is.null(plotData1)) return(FALSE)
                # Set in .run() when LR+ < 1; the explanatory notice is raised there, because
                # notices are rendered before any plot function runs.
                if (identical(plotData1$drawable, FALSE)) return(FALSE)

                plot1 <- nomogrammer(
                    Prevalence = plotData1$Prevalence,
                    Sens = plotData1$Sens,
                    Spec = plotData1$Spec,
                    Plr = plotData1$Plr,
                    Nlr = plotData1$Nlr,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 / 5),
                    Verbose = TRUE
                )

                print(plot1)
                TRUE
            },

            # Private helper methods for summaries ----

            .createSummary = function(Sens, Spec, PPV, NPV, LRP, LRN, Youden, Accuracy, Prevalence) {
                # Clinical interpretation of performance
                performance <- if (Youden > private$.YOUDEN_EXCELLENT && Accuracy > private$.ACCURACY_EXCELLENT) {
                    "excellent discriminatory ability"
                } else if (Youden > private$.YOUDEN_GOOD && Accuracy > private$.ACCURACY_GOOD) {
                    "good discriminatory ability"
                } else if (Youden > private$.YOUDEN_FAIR) {
                    "fair discriminatory ability"
                } else {
                    "limited discriminatory ability"
                }

                # LR interpretation
                lr_interp <- if (LRP > 10) {
                    "strong evidence for disease when test positive"
                } else if (LRP > 5) {
                    "moderate evidence for disease when test positive"
                } else {
                    "weak evidence for disease when test positive"
                }

                # NLR interpretation
                nlr_interp <- if (LRN < 0.1) {
                    "strong evidence against disease when test negative"
                } else if (LRN < 0.2) {
                    "moderate evidence against disease when test negative"
                } else {
                    "weak evidence against disease when test negative"
                }

                # Descriptive performance summary (not a recommendation)
                recommendation <- private$.getRecommendation(Youden, Accuracy, LRP, LRN)

                sprintf(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #333; padding: 15px; margin-bottom: 15px; color: inherit;'>
                <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>Diagnostic Test Performance Summary</h3>
                </div>

                <div style='font-size: 14px; color: #333;'>
                    <p style='margin: 10px 0;'><strong>Overall Assessment:</strong> This test demonstrates %s (Youden index: %.3f, Accuracy: %.1f%%). The good and excellent grades require Youden's index and accuracy to reach that band together, so the grade can be lower than either number on its own would suggest. Accuracy is computed at the prevalence observed in this sample and will differ where the case mix differs; Youden's index does not.</p>

                    <table style='width: 100%%; border-collapse: collapse; margin: 15px 0;'>
                    <tr>
                        <td style='border: 1px solid #ccc; padding: 10px; background-color: rgba(155, 155, 155, 0.06); color: inherit;'>
                        <strong>Sensitivity</strong><br>
                        <span style='font-size: 18px;'>%.1f%%</span><br>
                        <span style='font-size: 12px; color: #666;'>True positive rate</span>
                        </td>
                        <td style='border: 1px solid #ccc; padding: 10px; background-color: rgba(155, 155, 155, 0.06); color: inherit;'>
                        <strong>Specificity</strong><br>
                        <span style='font-size: 18px;'>%.1f%%</span><br>
                        <span style='font-size: 12px; color: #666;'>True negative rate</span>
                        </td>
                    </tr>
                    <tr>
                        <td style='border: 1px solid #ccc; padding: 10px; background-color: rgba(155, 155, 155, 0.06); color: inherit;'>
                        <strong>PPV</strong><br>
                        <span style='font-size: 18px;'>%.1f%%</span><br>
                        <span style='font-size: 12px; color: #666;'>At %.1f%% prevalence</span>
                        </td>
                        <td style='border: 1px solid #ccc; padding: 10px; background-color: rgba(155, 155, 155, 0.06); color: inherit;'>
                        <strong>NPV</strong><br>
                        <span style='font-size: 18px;'>%.1f%%</span><br>
                        <span style='font-size: 12px; color: #666;'>At %.1f%% prevalence</span>
                        </td>
                    </tr>
                    </table>

                    <p style='margin: 10px 0;'><strong>Clinical Utility:</strong></p>
                    <ul style='margin: 10px 0; padding-left: 25px;'>
                    <li>The positive likelihood ratio of %.2f indicates %s.</li>
                    <li>The negative likelihood ratio of %.3f indicates %s.</li>
                    </ul>

                    <div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid #ccc; padding: 12px; margin: 15px 0; color: inherit;'>
                        <p style='margin: 0; font-weight: bold;'>Performance Summary</p>
                        <p style='margin: 5px 0 0 0;'>%s</p>
                    </div>
                </div>
                </div>",
                    performance, Youden, Accuracy * 100,
                    Sens * 100, Spec * 100,
                    PPV * 100, Prevalence * 100,
                    NPV * 100, Prevalence * 100,
                    LRP, lr_interp,
                    LRN, nlr_interp,
                    recommendation
                )
            },
            .createAboutPanel = function() {
                "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
            <div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #333; padding: 15px; margin-bottom: 15px; color: inherit;'>
            <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>About Diagnostic Test Evaluation</h3>
            </div>

            <div style='font-size: 14px; color: #333;'>
            <p><strong>What does this analysis do?</strong></p>
            <p>This function evaluates the performance of a diagnostic test by comparing test results
            against a gold standard (reference test). It calculates sensitivity, specificity, predictive values,
            and likelihood ratios to help determine how well the test identifies disease.</p>

            <p><strong>When to use it:</strong></p>
            <ul style='margin: 10px 0; padding-left: 25px;'>
            <li>Validating a new diagnostic test against established gold standard</li>
            <li>Comparing different diagnostic methods</li>
            <li>Determining optimal test cut-off values</li>
            <li>Clinical decision-making about test utility</li>
            </ul>

            <p><strong>Key Outputs:</strong></p>
            <ul style='margin: 10px 0; padding-left: 25px;'>
            <li><strong>Sensitivity:</strong> Ability to detect disease when present (avoid false negatives)</li>
            <li><strong>Specificity:</strong> Ability to confirm absence when healthy (avoid false positives)</li>
            <li><strong>PPV/NPV:</strong> Post-test probability after positive/negative result (depends on prevalence)</li>
            <li><strong>Likelihood Ratios:</strong> How much test result changes disease probability</li>
            <li><strong>Youden Index:</strong> Overall discriminatory power (optimal cut-off criterion)</li>
            <li><strong>Advanced Metrics:</strong> Balanced Accuracy, F1 Score, MCC, DOR</li>
            </ul>

            <p><strong>References:</strong></p>
            <ul style='margin: 10px 0; padding-left: 25px; font-size: 13px;'>
            <li>Altman DG, Bland JM. Diagnostic tests. 1: Sensitivity and specificity. BMJ. 1994 Jun 11;308(6943):1552. doi: 10.1136/bmj.308.6943.1552. PMID: 8019315; PMCID: PMC2540489.</li>
            <li>Deeks JJ, Altman DG. Diagnostic tests 4: likelihood ratios. BMJ 2004;329:168-169</li>
            <li>epiR package documentation: <a href='https://cran.r-project.org/package=epiR' target='_blank'>CRAN</a></li>
            </ul>
            </div>
            </div>"
            },
            .createAssumptionsPanel = function(TP, TN, FP, FN, prev) {
                warnings <- character()

                # Check sample size adequacy
                if (TP < 10 || TN < 10) {
                    warnings <- c(warnings, sprintf(
                        "<li style='color: #d9534f;'><strong>Small sample size:</strong> TP=%d, TN=%d.
                    Confidence intervals may be unreliable. Consider n >= 30 per group.</li>",
                        TP, TN
                    ))
                }

                # Check for extreme prevalence
                if (prev < 0.05 || prev > 0.95) {
                    warnings <- c(warnings, sprintf(
                        "<li style='color: #f0ad4e;'><strong>Extreme prevalence:</strong> %.1f%%.
                    PPV/NPV estimates may be unstable. Verify in target population.</li>",
                        prev * 100
                    ))
                }

                # Check for zero cells
                if (FP == 0 || FN == 0) {
                    warnings <- c(
                        warnings,
                        "<li style='color: #f0ad4e;'><strong>Zero cells detected:</strong>
                    Perfect sensitivity or specificity. May indicate overfitting or insufficient validation.</li>"
                    )
                }

                # Check for very small error counts
                if ((FP > 0 && FP < 5) || (FN > 0 && FN < 5)) {
                    warnings <- c(
                        warnings,
                        "<li style='color: #f0ad4e;'><strong>Very few errors:</strong>
                    Small counts in FP or FN cells may lead to unstable estimates.</li>"
                    )
                }

                warning_html <- if (length(warnings) > 0) {
                    sprintf("<div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; margin: 10px 0; border-left: 4px solid #f0ad4e; color: inherit;'>
                <h4 style='margin-top: 0; color: #856404;'> Warnings</h4>
                <ul style='margin: 10px 0; padding-left: 20px;'>%s</ul>
                </div>", paste(warnings, collapse = "\n"))
                } else {
                    "<div style='background-color: rgba(33, 162, 64, 0.19); padding: 15px; margin: 10px 0; border-left: 4px solid #28a745; color: inherit;'>
                <p style='margin: 0; color: #155724;'><strong> No issues detected</strong> - Sample size and distribution appear adequate.</p>
                </div>"
                }

                sprintf(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #333; padding: 15px; margin-bottom: 15px; color: inherit;'>
                <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>Assumptions & Caveats</h3>
                </div>

                <div style='font-size: 14px; color: #333;'>
                <p><strong>Key Assumptions:</strong></p>
                <ul style='margin: 10px 0; padding-left: 25px;'>
                <li><strong>Gold standard validity:</strong> Reference test must be highly accurate (near 100%% sensitivity/specificity)</li>
                <li><strong>Independent assessment:</strong> Test and gold standard should be evaluated independently (blinded)</li>
                <li><strong>Representative sample:</strong> Study population should match intended clinical use population</li>
                <li><strong>Disease spectrum:</strong> Include appropriate mix of disease severity (avoid spectrum bias)</li>
                <li><strong>Prevalence dependence:</strong> PPV/NPV vary with disease prevalence; verify in target setting</li>
                </ul>

                <p><strong>Common Pitfalls:</strong></p>
                <ul style='margin: 10px 0; padding-left: 25px;'>
                <li><strong>Verification bias:</strong> Not all test-positive patients receive gold standard confirmation</li>
                <li><strong>Incorporation bias:</strong> Gold standard includes results of the test being evaluated</li>
                <li><strong>Spectrum bias:</strong> Study population has more severe disease than clinical practice</li>
                <li><strong>Prevalence extrapolation:</strong> Applying PPV/NPV from high-prevalence study to low-prevalence screening</li>
                </ul>

                %s

                <p><strong>Sample Size Guidance:</strong></p>
                <ul style='margin: 10px 0; padding-left: 25px;'>
                <li>Minimum 30-50 diseased cases (for sensitivity estimation)</li>
                <li>Minimum 30-50 healthy controls (for specificity estimation)</li>
                <li>For rare diseases (prevalence < 5%%), consider n >= 200 total</li>
                <li>Larger samples needed for precise CI estimation</li>
                </ul>
                </div>
                </div>",
                    warning_html
                )
            },
            .createGlossary = function() {
                "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
            <div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid #333; padding: 15px; margin-bottom: 15px; color: inherit;'>
            <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>Clinical Terms Glossary</h3>
            </div>

            <div style='font-size: 14px; color: #333;'>
            <dl style='margin: 0;'>
            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>Sensitivity (True Positive Rate)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Proportion of diseased patients correctly identified. <em>Clinical use:</em> How good is this test at catching disease?</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>Specificity (True Negative Rate)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Proportion of healthy patients correctly identified. <em>Clinical use:</em> How good is this test at confirming health?</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>PPV (Positive Predictive Value)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Probability of disease given positive test. <em>Clinical use:</em> If test is positive, how likely is disease? <strong>Depends on prevalence.</strong></dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>NPV (Negative Predictive Value)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Probability of health given negative test. <em>Clinical use:</em> If test is negative, how likely is patient healthy? <strong>Depends on prevalence.</strong></dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>LR+ (Positive Likelihood Ratio)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>How much positive test increases odds of disease. <em>Interpretation:</em> >10 = strong evidence, 5-10 = moderate, 2-5 = weak, <2 = minimal.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>LR- (Negative Likelihood Ratio)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>How much negative test decreases odds of disease. <em>Interpretation:</em> <0.1 = strong evidence against, 0.1-0.2 = moderate, 0.2-0.5 = weak, >0.5 = minimal.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>Youden Index (J)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Sensitivity + Specificity - 1. Range: -1 to +1. <em>Clinical use:</em> Optimal cut-off selection. >0.8 = excellent, 0.6-0.8 = good, 0.4-0.6 = fair, <0.4 = poor.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>Balanced Accuracy</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Average of sensitivity and specificity. Better than raw accuracy for imbalanced datasets. >0.9 = excellent, 0.8-0.9 = good.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>F1 Score</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Harmonic mean of sensitivity and PPV. Useful when false negatives and false positives are equally costly. >0.8 = excellent.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>MCC (Matthews Correlation Coefficient)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Balanced measure accounting for class imbalance. Range: -1 to +1. >0.8 = excellent, 0.6-0.8 = good, 0.4-0.6 = fair.</dd>

            <dt style='font-weight: bold; margin-top: 15px; color: #333;'>DOR (Diagnostic Odds Ratio)</dt>
            <dd style='margin-left: 20px; margin-bottom: 10px;'>Odds of positive test in diseased vs healthy. <em>Interpretation:</em> >25 = strong, 5-25 = moderate, 2-5 = weak, <2 = poor discrimination.</dd>
            </dl>
            </div>
            </div>"
            },
            # Describes where the point estimates fall. Deliberately issues no
            # clinical verdict: the inputs are one 2x2 table at one cut-off, with
            # no interval, no validation cohort and no account of spectrum or
            # verification bias.
            .getRecommendation = function(Youden, Accuracy, LRP, LRN) {
                # PPV/NPV are Bayes-adjusted to the supplied prior when pp is on, so the
                # prevalence they depend on is not this sample's; accuracy always is.
                prev_source <- if (isTRUE(self$options$pp))
                    "the population prevalence you supplied" else "the prevalence of this sample"
                caveat <- paste0("These are single-cohort point estimates at one cut-off. Accuracy depends on the prevalence of this sample, and the predictive values on ", prev_source, ". They do not on their own establish how the test would perform elsewhere; the confidence intervals, the representativeness of the sample, and the quality and blinding of the reference standard all bear on that.")
                band <- if (Youden > private$.YOUDEN_EXCELLENT && Accuracy > private$.ACCURACY_EXCELLENT && LRP > 10 && LRN < 0.1) {
                    "Youden's index, accuracy and both likelihood ratios all fall in the highest bands in this sample."
                } else if (Youden > private$.YOUDEN_GOOD && Accuracy > private$.ACCURACY_GOOD) {
                    "Youden's index and accuracy both reach at least the good band in this sample."
                } else if (Youden > private$.YOUDEN_FAIR) {
                    "Youden's index reaches at least the fair band in this sample, but Youden's index and accuracy do not both reach the good band."
                } else {
                    "Youden's index falls in the lowest band in this sample, so positive and negative results separate diseased from healthy patients only weakly."
                }
                paste(band, caveat)
            }
        )
    )
}
