#' @title Decision Calculator
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom jmvcore .
#' @importFrom utils data
#'
#' @return An \code{R6} class generator object for the \code{decisioncalculatorClass} backend; used internally by the jamovi analysis wrapper and not called directly.

decisioncalculatorClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class("decisioncalculatorClass",
        inherit = decisioncalculatorBase, private = list(
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

                severity_order <- c(
                    ERROR = 1L,
                    STRONG_WARNING = 2L,
                    WARNING = 3L,
                    INFO = 4L
                )
                priorities <- unname(severity_order[vapply(
                    private$.noticeList,
                    `[[`,
                    character(1),
                    "type"
                )])
                priorities[is.na(priorities)] <- 5L
                ordered_notices <- private$.noticeList[order(priorities)]

                # Plain text only - notices avoid HTML by project convention; the Preformatted
                # output item renders this literally (no markup, no injection surface).
                blocks <- vapply(ordered_notices, function(notice) {
                    prefix <- switch(notice$type,
                        ERROR          = paste0(.("ERROR"), ": "),
                        STRONG_WARNING = paste0(.("STRONG WARNING"), ": "),
                        WARNING        = paste0(.("WARNING"), ": "),
                        INFO           = paste0(.("INFO"), ": "),
                        ""
                    )
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },
            .init = function() {
                # Welcome message
                welcome_html <- paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid currentColor; padding: 20px; margin-bottom: 20px; color: inherit;'>",
                    "<h2 style='margin: 0 0 10px 0; font-size: 18px;'>",
                    .("Medical decision calculator"),
                    "</h2><p style='margin: 0; font-size: 14px;'>",
                    .("Educational description of diagnostic-test performance"),
                    "</p></div><div style='font-size: 14px;'><p><strong>",
                    .("What this tool does:"),
                    "</strong></p><p>",
                    .("Evaluates diagnostic-test performance by calculating sensitivity, specificity, predictive values, likelihood ratios, and advanced metrics from a 2×2 confusion matrix."),
                    "</p><p><strong>",
                    .("To get started:"),
                    "</strong></p><ol style='margin: 10px 0; padding-left: 25px;'><li>",
                    .("Enter the four counts: TP (true positive), FP (false positive), TN (true negative), and FN (false negative)."),
                    "</li><li>",
                    .("Choose whether to calculate confidence intervals."),
                    "</li><li>",
                    .("Optionally enable the summary, glossary, or about panels for additional guidance."),
                    "</li></ol><div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid #ccc; padding: 12px; margin: 15px 0; color: inherit;'>",
                    "<p style='margin: 0; font-size: 13px;'><strong>",
                    .("Illustrative example, not clinical guidance:"),
                    "</strong> ",
                    .("If 200 participants include 100 reference-positive and 100 reference-negative observations, with TP=90 and TN=80, then FN=10 and FP=20."),
                    "</p></div></div></div>"
                )

                self$results$welcome$setContent(welcome_html)

                cTable <- self$results$cTable

                cTable$addRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = .("Test positive")
                    )
                )


                cTable$addRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = .("Test negative")
                    )
                )


                cTable$addRow(
                    rowKey = "Total",
                    values = list(
                        newtest = .("Total")
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
                    values = list(cutoffName = .("Current (reference)"))
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
                # epiR estimates intervals from the study 2x2 table. When an external
                # prevalence is supplied, those intervals remain valid for sensitivity,
                # specificity, accuracy, likelihood ratios, DOR and Youden's J, but not
                # for the Bayes-adjusted predictive values. The CI tables therefore omit
                # prevalence-dependent epiR rows in that option combination.
                if (ci && pp) {
                    private$.addNotice(
                        "INFO",
                        .("Partial confidence intervals with population prevalence"),
                        .("Confidence intervals are shown for measures estimable from the study 2x2 table. Predictive values are adjusted to the supplied population prevalence, but their confidence intervals are not computed because the uncertainty of that external prevalence was not supplied.")
                    )
                }

                # Explain prevalence adjustment when intervals were not requested.
                if (!ci && pp) {
                    private$.addNotice(
                        "INFO",
                        .("Population-adjusted predictive values"),
                        .("Positive and negative predictive values are adjusted to the supplied population prevalence. No confidence intervals were requested.")
                    )
                }

                # Reject non-finite inputs
                if (any(!is.finite(c(TP, FP, TN, FN)))) {
                    private$.addNotice("ERROR", .("Non-finite counts"), .("TP, FP, TN, and FN must be finite numbers. Check the input values."))
                    return()
                }

                # Check for non-negative values
                if (TP < 0 || FP < 0 || TN < 0 || FN < 0) {
                    private$.addNotice("ERROR", .("Negative counts detected"), .("All counts (TP, FP, TN, FN) must be non-negative. Check the input values."))
                    return()
                }

                # Check for at least some data
                if (TP + FP + TN + FN == 0) {
                    private$.addNotice("ERROR", .("All counts are zero"), .("Provide diagnostic-test data with at least one non-zero count."))
                    return()
                }

                # Check for diseased subjects
                if (TP + FN == 0) {
                    private$.addNotice("ERROR", .("No reference-positive subjects"), .("TP + FN equals zero, so sensitivity and related measures cannot be calculated."))
                    return()
                }

                # Check for healthy subjects
                if (TN + FP == 0) {
                    private$.addNotice("ERROR", .("No reference-negative subjects"), .("TN + FP equals zero, so specificity and related measures cannot be calculated."))
                    return()
                }

                # Check for positive tests
                if (TP + FP == 0) {
                    private$.addNotice("WARNING", .("No positive test results"), .("TP + FP equals zero, so positive predictive value is undefined."))
                }

                # Check for negative tests
                if (TN + FN == 0) {
                    private$.addNotice("WARNING", .("No negative test results"), .("TN + FN equals zero, so negative predictive value is undefined."))
                }


                # Create confusion matrix ----

                table2 <- matrix(c(TP, FP, FN, TN),
                    nrow = 2, ncol = 2, byrow = TRUE,
                    dimnames = list(c("Positive", "Negative"), c("Positive", "Negative"))
                )

                table3 <- as.table(table2)

                names(attributes(table3)$dimnames) <- c("Test", "Reference standard")

                # Prior Probability ----
                # (pp and pprob already read at top of function for validation)


                # Cross Table in jamovi style ----

                cTable <- self$results$cTable


                cTable$setRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = .("Test positive"),
                        GP = TP,
                        GN = FP,
                        Total = TP + FP
                    )
                )


                cTable$setRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = .("Test negative"),
                        GP = FN,
                        GN = TN,
                        Total = FN + TN
                    )
                )

                cTable$setRow(
                    rowKey = "Total",
                    values = list(
                        newtest = .("Total"),
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

                # Fractional frequencies can represent weighted data, but binomial
                # confidence intervals require integer event and trial counts unless an
                # effective sample size or survey design is supplied.
                fractional_counts <- any(
                    abs(c(TP, FP, TN, FN) - round(c(TP, FP, TN, FN))) > 1e-6
                )
                if (fractional_counts) {
                    private$.addNotice(
                        "WARNING",
                        .("Weighted or fractional counts"),
                        .("Fractional frequencies are used for point estimates only. Confidence intervals are omitted because no effective sample size or weighting design was supplied.")
                    )
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

                    private$.addNotice("WARNING", .("Continuity correction applied"), .("A Haldane-Anscombe 0.5 correction was applied to likelihood-ratio and diagnostic-odds-ratio point estimates because a zero cell was present. Confidence intervals use the raw counts and are not continuity-corrected."))
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

                if (PriorProb < 0.05 || PriorProb > 0.95) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Extreme prevalence"),
                        jmvcore::format(
                            .("The selected prevalence is {prevalence}. Predictive values may be unstable or poorly transportable; verify them in the intended population and report the prevalence used."),
                            prevalence = sprintf("%.1f%%", 100 * PriorProb)
                        )
                    )
                }

                if (DiseaseP < 10 || DiseaseN < 10) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Very sparse reference groups"),
                        jmvcore::format(
                            .("Only {positive} reference-positive and {negative} reference-negative observations are available. Estimates will be imprecise; inspect confidence intervals and justify sample size for the intended precision. This warning is not a clinical adequacy threshold."),
                            positive = DiseaseP,
                            negative = DiseaseN
                        )
                    )
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
                if (is.finite(Sens) && is.finite(PPV) && (Sens + PPV) > 0) {
                    F1Score <- 2 * (Sens * PPV) / (Sens + PPV)
                } else {
                    F1Score <- NA_real_
                }

                # Matthews Correlation Coefficient (MCC)
                mcc_numerator <- (TP * TN) - (FP * FN)
                mcc_denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
                if (mcc_denominator > 0) {
                    MCC <- mcc_numerator / mcc_denominator
                } else {
                    MCC <- NA_real_
                }


                # nTable Populate Table ----

                nTable <- self$results$nTable
                nTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = .("Counts"),
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
                        tablename = .("Measures"),
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

                    nTable$addFootnote(rowNo = 1, col = "TotalPop", .("Total number of subjects"))

                    nTable$addFootnote(rowNo = 1, col = "DiseaseP", .("Total number of reference-positive subjects"))

                    nTable$addFootnote(rowNo = 1, col = "DiseaseN", .("Total number of reference-negative subjects"))

                    nTable$addFootnote(rowNo = 1, col = "TestP", .("Total number of positive tests"))

                    nTable$addFootnote(rowNo = 1, col = "TestN", .("Total number of negative tests"))

                    nTable$addFootnote(rowNo = 1, col = "TestT", .("Total number of concordant test results"))

                    nTable$addFootnote(rowNo = 1, col = "TestW", .("Total number of discordant test results"))
                }


                # ratioTable footnotes ----


                if (self$options$fnote) {
                    ratioTable$addFootnote(rowNo = 1, col = "Sens", .("Sensitivity: true positives among reference-positive subjects"))

                    ratioTable$addFootnote(rowNo = 1, col = "Spec", .("Specificity: true negatives among reference-negative subjects"))

                    accuracy_note <- if (pp) {
                        jmvcore::format(
                            .("Sample accuracy is the proportion of concordant results. It is computed at the prevalence observed in this sample ({prevalence}), not at the supplied population prevalence, and changes with case mix."),
                            prevalence = sprintf("%.1f%%", PrevalenceD * 100)
                        )
                    } else {
                        jmvcore::format(
                            .("Sample accuracy is the proportion of concordant results. It is computed at the prevalence observed in this sample ({prevalence}) and changes with case mix."),
                            prevalence = sprintf("%.1f%%", PrevalenceD * 100)
                        )
                    }
                    ratioTable$addFootnote(rowNo = 1, col = "AccurT", accuracy_note)

                    prev_note <- if (pp) {
                        .("The user-supplied population prevalence is used, not the prevalence observed in this sample.")
                    } else {
                        .("Prevalence observed in this study sample.")
                    }
                    ratioTable$addFootnote(rowNo = 1, col = "PrevalenceD", prev_note)

                    ppv_note <- if (pp) {
                        .("Positive predictive value using the supplied population prevalence.")
                    } else {
                        .("Positive predictive value using this study sample's prevalence.")
                    }
                    npv_note <- if (pp) {
                        .("Negative predictive value using the supplied population prevalence.")
                    } else {
                        .("Negative predictive value using this study sample's prevalence.")
                    }

                    ratioTable$addFootnote(rowNo = 1, col = "PPV", ppv_note)

                    ratioTable$addFootnote(rowNo = 1, col = "NPV", npv_note)

                    ratioTable$addFootnote(rowNo = 1, col = "PostTestProbDisease", .("Probability of a reference-positive outcome after a positive test, using the displayed prevalence. It is identical to the PPV shown here."))

                    ratioTable$addFootnote(rowNo = 1, col = "PostTestProbHealthy", .("Probability of a reference-negative outcome after a negative test, using the displayed prevalence. It is identical to the NPV shown here."))

                    ratioTable$addFootnote(
                        rowNo = 1, col = "LRP",
                        .("Positive likelihood ratio: the factor by which a positive result multiplies pre-test odds. Its practical importance depends on the clinical context and starting probability.")
                    )

                    ratioTable$addFootnote(
                        rowNo = 1, col = "LRN",
                        .("Negative likelihood ratio: the factor by which a negative result multiplies pre-test odds. Its practical importance depends on the clinical context and starting probability.")
                    )

                    if (zero_cell) {
                        ratioTable$addFootnote(rowNo = 1, col = "LRP", .("A Haldane-Anscombe 0.5 correction was applied to the likelihood ratios because a zero cell was present. The epiR confidence-interval table uses raw counts, so its point estimates may differ."))
                    }
                }


                # Populate advanced metrics table ----
                advancedMetricsTable <- self$results$advancedMetricsTable
                advancedMetricsTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = .("Advanced measures"),
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
                        .("Youden's index equals sensitivity plus specificity minus one. It weights sensitivity and specificity equally and is descriptive, not a clinical decision threshold.")
                    )

                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "balancedAccuracy",
                        .("Balanced accuracy is the unweighted average of sensitivity and specificity. It is descriptive and does not encode the consequences of false-positive and false-negative results.")
                    )

                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "f1Score",
                        .("F1 score is the harmonic mean of sensitivity and positive predictive value. When an external prevalence is supplied, this value uses the prevalence-adjusted positive predictive value.")
                    )

                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "mcc",
                        .("Matthews correlation coefficient summarizes all four cells and ranges from -1 to +1. Interpretation is context-dependent; no universal clinical cut-off is assumed.")
                    )

                    advancedMetricsTable$addFootnote(
                        rowNo = 1, col = "dor",
                        .("Diagnostic odds ratio compares the odds of a positive result between reference-positive and reference-negative groups. Its magnitude is descriptive and has no universal clinical cut-off.")
                    )
                }


                # 95% CI ----

                ci <- self$options$ci

                if (ci) {
                    epirTable_ratio <- self$results$epirTable_ratio
                    epirTable_ratio$deleteRows()
                    epirTable_number <- self$results$epirTable_number
                    epirTable_number$deleteRows()

                    if (!fractional_counts) {
                        if (!requireNamespace("epiR", quietly = TRUE)) {
                            private$.addNotice(
                                "ERROR",
                                .("epiR package missing"),
                                .("The epiR package is required for confidence intervals. Install it or disable confidence intervals.")
                            )
                            return()
                        }

                        epirresult2 <- epiR::epi.tests(dat = table3) |>
                            summary() |>
                            as.data.frame() |>
                            tibble::rownames_to_column(var = "statsabv")

                        stat_labels <- c(
                            "ap"      = .("Apparent prevalence"),
                            "tp"      = .("True prevalence"),
                            "se"      = .("Test sensitivity"),
                            "sp"      = .("Test specificity"),
                            "diag.ac" = .("Diagnostic accuracy"),
                            "diag.or" = .("Diagnostic odds ratio"),
                            "nndx"    = .("Number needed to diagnose"),
                            "youden"  = .("Youden's index"),
                            "pv.pos"  = .("Positive predictive value"),
                            "pv.neg"  = .("Negative predictive value"),
                            "lr.pos"  = .("Likelihood ratio of a positive test"),
                            "lr.neg"  = .("Likelihood ratio of a negative test"),
                            "p.rout"  = .("Proportion of subjects with outcome ruled out"),
                            "p.rin"   = .("Proportion of subjects with outcome ruled in"),
                            "p.tpdn"  = .("False-positive rate among outcome-negative subjects"),
                            "p.tndp"  = .("False-negative rate among outcome-positive subjects"),
                            "p.dntp"  = .("False-discovery proportion among test-positive subjects"),
                            "p.dptn"  = .("False-omission proportion among test-negative subjects")
                        )
                        epirresult2$statsnames <- unname(stat_labels[epirresult2$statistic])

                        ratiorows <- if (pp) {
                            c("se", "sp", "diag.ac")
                        } else {
                            c(
                                "ap", "tp", "se", "sp", "diag.ac", "pv.pos", "pv.neg",
                                "p.rout", "p.rin", "p.tpdn", "p.tndp", "p.dntp", "p.dptn"
                            )
                        }
                        numberrows <- c("diag.or", "nndx", "youden", "lr.pos", "lr.neg")

                        epirresult_ratio <- epirresult2[
                            epirresult2$statistic %in% ratiorows &
                                !is.na(epirresult2$statsnames), , drop = FALSE
                        ]
                        epirresult_number <- epirresult2[
                            epirresult2$statistic %in% numberrows &
                                !is.na(epirresult2$statsnames), , drop = FALSE
                        ]

                        extra_rows <- data.frame(
                            statistic = c("bal.acc", "f1.score"),
                            est = c(BalancedAccuracy, F1Score),
                            lower = NA_real_,
                            upper = NA_real_,
                            statsabv = c("bal.acc", "f1.score"),
                            statsnames = c(
                                .("Balanced accuracy (CI not computed)"),
                                .("F1 score at selected prevalence (CI not computed)")
                            ),
                            stringsAsFactors = FALSE
                        )
                        epirresult_ratio <- rbind(epirresult_ratio, extra_rows)

                        for (i in seq_len(nrow(epirresult_ratio))) {
                            epirTable_ratio$addRow(
                                rowKey = i,
                                values = c(epirresult_ratio[i, ])
                            )
                        }
                        for (i in seq_len(nrow(epirresult_number))) {
                            epirTable_number$addRow(
                                rowKey = i,
                                values = c(epirresult_number[i, ])
                            )
                        }
                    }
                }

                # Multiple cut-off evaluation (DiagROC inspired)
                if (self$options$multiplecuts) {
                    multipleCutoffTable <- self$results$multipleCutoffTable

                    # Helper function to calculate metrics for a cut-off
                    calculate_cutoff_metrics <- function(tp, fp, tn, fn, cutoff_name) {
                        # Validate inputs and guard against non-finite/negative values.
                        cutoff_values <- c(tp, fp, tn, fn)
                        if (any(!is.finite(cutoff_values)) || any(cutoff_values < 0)) {
                            private$.addNotice(
                                "ERROR",
                                .("Invalid cut-off inputs"),
                                jmvcore::format(
                                    .('All values for cut-off "{cutoff}" must be non-negative finite numbers (TP={tp}, FP={fp}, TN={tn}, FN={fn}).'),
                                    cutoff = cutoff_name,
                                    tp = tp,
                                    fp = fp,
                                    tn = tn,
                                    fn = fn
                                )
                            )
                            return(NULL)
                        }

                        total <- tp + fp + tn + fn
                        diseased <- tp + fn
                        healthy <- tn + fp

                        # Validate that we have cases to analyze
                        if (total == 0) {
                            private$.addNotice(
                                "ERROR",
                                .("No cases for cut-off"),
                                jmvcore::format(
                                    .('All four frequencies for cut-off "{cutoff}" are zero.'),
                                    cutoff = cutoff_name
                                )
                            )
                            return(NULL)
                        }

                        # Safe division with appropriate handling for zero denominators
                        sens <- if (diseased > 0) tp / diseased else NA_real_
                        spec <- if (healthy > 0) tn / healthy else NA_real_
                        if (pp && is.finite(sens) && is.finite(spec)) {
                            ppv_den <- sens * PriorProb + (1 - spec) * (1 - PriorProb)
                            npv_den <- spec * (1 - PriorProb) + (1 - sens) * PriorProb
                            ppv <- if (ppv_den > 0) sens * PriorProb / ppv_den else NA_real_
                            npv <- if (npv_den > 0) spec * (1 - PriorProb) / npv_den else NA_real_
                        } else {
                            ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
                            npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
                        }
                        accuracy <- if (total > 0) (tp + tn) / total else NA_real_

                        # Youden index only defined when both sens and spec are available
                        youden <- if (!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA_real_

                        # Warn if metrics are undefined
                        if (diseased == 0 || healthy == 0) {
                            if (diseased == 0) {
                                msg <- jmvcore::format(
                                    .('Cut-off "{cutoff}" has no reference-positive observations, so sensitivity is undefined.'),
                                    cutoff = cutoff_name
                                )
                            } else {
                                msg <- jmvcore::format(
                                    .('Cut-off "{cutoff}" has no reference-negative observations, so specificity is undefined.'),
                                    cutoff = cutoff_name
                                )
                            }
                            private$.addNotice("WARNING", .("Incomplete cut-off data"), msg)
                        }

                        if (is.na(youden) || is.na(accuracy)) {
                            recommendation <- .("Incomplete data; some measures are undefined")
                        } else {
                            recommendation <- .("Illustrative point estimates; not clinical guidance")
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
                        private$.addNotice("ERROR", .("Cut-off validation failed"), .("The cut-off comparison cannot be performed because at least one scenario has invalid frequencies."))
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
                            .("Cut-offs describe different numbers of patients"),
                            jmvcore::format(
                                .("The three scenarios total {current}, {scenario1}, and {scenario2} cases. Moving a cut-off on one cohort cannot change its size, so these rows cannot be interpreted as thresholds applied to the same participants."),
                                current = n_current,
                                scenario1 = n1,
                                scenario2 = n2
                            )
                        )
                    }

                    cand <- list(
                        list(name = .("Current"), youden = current_youden, accuracy = current_accuracy),
                        list(name = cutoff1_metrics$cutoffName, youden = cutoff1_metrics$youden,
                             accuracy = cutoff1_metrics$accuracy),
                        list(name = cutoff2_metrics$cutoffName, youden = cutoff2_metrics$youden,
                             accuracy = cutoff2_metrics$accuracy)
                    )
                    ys <- vapply(cand, function(x) {
                        if (is.null(x$youden) || !is.finite(x$youden)) NA_real_ else x$youden
                    },
                        numeric(1))

                    # Uncertainty. A formal test of two cut-offs would need the paired
                    # discordance between them, which four marginal counts per scenario
                    # cannot supply. What IS computable is a Wilson interval on each
                    # scenario's ACCURACY, so the reader can see whether the intervals
                    # overlap -- overlapping intervals mean the counts do not separate the
                    # cut-offs, whatever the point estimates suggest.
                    wilson <- function(x, n, integer_counts, conf = 0.95) {
                        if (!integer_counts || !is.finite(x) || !is.finite(n) || n <= 0)
                            return(c(NA_real_, NA_real_))
                        z <- stats::qnorm(1 - (1 - conf) / 2)
                        ph <- x / n
                        den <- 1 + z^2 / n
                        ctr <- (ph + z^2 / (2 * n)) / den
                        hw <- z * sqrt((ph * (1 - ph) + z^2 / (4 * n)) / n) / den
                        c(max(0, ctr - hw), min(1, ctr + hw))
                    }
                    integer_scenarios <- c(
                        !fractional_counts,
                        !any(abs(c(self$options$tp1, self$options$fp1,
                                   self$options$tn1, self$options$fn1) -
                                 round(c(self$options$tp1, self$options$fp1,
                                         self$options$tn1, self$options$fn1))) > 1e-6),
                        !any(abs(c(self$options$tp2, self$options$fp2,
                                   self$options$tn2, self$options$fn2) -
                                 round(c(self$options$tp2, self$options$fp2,
                                         self$options$tn2, self$options$fn2))) > 1e-6)
                    )
                    acc_ci <- list(
                        wilson(TP + TN, n_current, integer_scenarios[1]),
                        wilson(self$options$tp1 + self$options$tn1, n1,
                               integer_scenarios[2]),
                        wilson(self$options$tp2 + self$options$tn2, n2,
                               integer_scenarios[3])
                    )
                    if (any(!integer_scenarios)) {
                        private$.addNotice(
                            "WARNING",
                            .("Cut-off intervals omitted for fractional counts"),
                            .("Accuracy intervals are not computed for scenarios containing fractional frequencies because no effective sample size or weighting design was supplied.")
                        )
                    }

                    optimal_msg <- .("Descriptive comparison unavailable")
                    if (any(is.finite(ys))) {
                        best <- which.max(replace(ys, !is.finite(ys), -Inf))
                        margin <- ys[best] - ys[1]                      # vs the current cut-off
                        # do the best and current accuracy intervals overlap?
                        ov <- NA
                        if (best != 1L && all(is.finite(c(acc_ci[[best]], acc_ci[[1]]))))
                            ov <- acc_ci[[best]][1] <= acc_ci[[1]][2] &&
                                  acc_ci[[1]][1] <= acc_ci[[best]][2]

                        if (best == 1L || !is.finite(margin) || margin <= 0) {
                            optimal_msg <- jmvcore::format(
                                .("Current cut-off has the highest Youden's J ({youden}) of the three."),
                                youden = sprintf("%.3f", ys[1])
                            )
                        } else if (isTRUE(ov)) {
                            optimal_msg <- jmvcore::format(
                                .("{cutoff} has the highest Youden's J, {margin} above current; its sample-accuracy interval overlaps the current cut-off's, so the difference is not established."),
                                cutoff = cand[[best]]$name,
                                margin = sprintf("%.3f", margin)
                            )
                        } else if (isFALSE(ov)) {
                            optimal_msg <- jmvcore::format(
                                .("{cutoff} has the highest Youden's J, {margin} above current; their sample-accuracy intervals do not overlap."),
                                cutoff = cand[[best]]$name,
                                margin = sprintf("%.3f", margin)
                            )
                        } else {
                            optimal_msg <- jmvcore::format(
                                .("{cutoff} has the highest Youden's J, {margin} above current; paired uncertainty cannot be assessed from these summary counts."),
                                cutoff = cand[[best]]$name,
                                margin = sprintf("%.3f", margin)
                            )
                        }
                    }

                    multipleCutoffTable$setNote(
                        "sameData",
                        jmvcore::.("The named scenarios are illustrative examples, not clinical guides. The current row reports which scenario has the highest Youden's J and whether its separately calculated accuracy interval overlaps the current row. Choosing a cut-off on the same data used to assess it is optimistically biased and does not establish clinical utility.")
                    )

                    multipleCutoffTable$setNote(
                        "uncertainty",
                        jmvcore::.("Cut-offs are compared on point estimates only. A formal test would need to know, for each patient, how the two thresholds classified them; four summary counts per scenario cannot supply that. The accuracy intervals referred to above are Wilson 95% intervals computed separately per scenario, so overlap is a conservative signal that the counts do not separate the cut-offs.")
                    )

                    multipleCutoffTable$setRow(
                        rowKey = 3,
                        values = list(
                            cutoffName = .("Current (reference)"),
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
                        .("Fagan nomogram not drawn"),
                        jmvcore::format(
                            .("The positive likelihood ratio is {lr}. A nomogram assumes that a positive result raises the probability of the outcome; here it lowers that probability, so the plot is omitted. Check the test coding."),
                            lr = sprintf("%.3f", LRP)
                        )
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
                        not_drawn_reason <- jmvcore::format(
                            .("The positive likelihood ratio is {lr}, so a positive result lowers the probability of a reference-positive outcome rather than raising it."),
                            lr = sprintf("%.3f", LRP)
                        )
                        fagan_item$setContent(paste0(
                            "<div style='padding:12px;border-left:4px solid #c00;background-color: rgba(255, 88, 88, 0.06); color: inherit;'>",
                            "<p>", .("No nomogram is drawn for this table."), " ",
                            not_drawn_reason,
                            "</p></div>"))
                    } else {
                        pre <- PriorProb
                        post_pos <- PostTestProbDisease
                        post_neg <- 1 - PostTestProbHealthy
                        pretest_sentence <- if (isTRUE(self$options$pp)) {
                            jmvcore::format(
                                .("Pre-test probability is {prevalence}, using the population prevalence you supplied."),
                                prevalence = sprintf("%.1f%%", 100 * pre)
                            )
                        } else {
                            jmvcore::format(
                                .("Pre-test probability is {prevalence}, using the prevalence observed in this study table."),
                                prevalence = sprintf("%.1f%%", 100 * pre)
                            )
                        }
                        positive_sentence <- jmvcore::format(
                            .("A positive result has a likelihood ratio of {lr} and raises the probability from {prevalence} to {posttest}."),
                            lr = sprintf("%.2f", LRP),
                            prevalence = sprintf("%.1f%%", 100 * pre),
                            posttest = sprintf("%.1f%%", 100 * post_pos)
                        )
                        negative_sentence <- jmvcore::format(
                            .("A negative result has a likelihood ratio of {lr} and lowers the probability from {prevalence} to {posttest}."),
                            lr = sprintf("%.3f", LRN),
                            prevalence = sprintf("%.1f%%", 100 * pre),
                            posttest = sprintf("%.1f%%", 100 * post_neg)
                        )
                        fagan_item$setContent(paste0(
                            "<div style='padding:12px;border-left:4px solid #1565c0;background-color: rgba(88, 155, 255, 0.06); color: inherit;'>",
                            "<p>", pretest_sentence, "</p>",
                            "<p>", positive_sentence, "</p>",
                            "<p>", negative_sentence, "</p>",
                            "<p style='font-size:90%;color:inherit;'>",
                            .("Read the nomogram by drawing a line from the pre-test probability on the left, through the likelihood ratio in the middle, to the post-test probability on the right. Sensitivity and specificity describe agreement with the reference standard; the pre-test probability depends on population context, so the same test can lead to a different endpoint in another population."),
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

                plot1 <- plot1 + ggtheme

                print(plot1)
                TRUE
            },

            # Private helper methods for summaries ----

            .createSummary = function(Sens, Spec, PPV, NPV, LRP, LRN, Youden, Accuracy, Prevalence) {
                descriptive_result <- jmvcore::format(
                    .("Youden's index is {youden} and sample accuracy is {accuracy}. These values describe the entered study table; they are not clinical grades or decision thresholds."),
                    youden = sprintf("%.3f", Youden),
                    accuracy = sprintf("%.1f%%", Accuracy * 100)
                )
                prevalence_label <- jmvcore::format(
                    .("At {prevalence} prevalence"),
                    prevalence = sprintf("%.1f%%", Prevalence * 100)
                )
                positive_update <- jmvcore::format(
                    .("A positive result multiplies pre-test odds by {lr}."),
                    lr = sprintf("%.2f", LRP)
                )
                negative_update <- jmvcore::format(
                    .("A negative result multiplies pre-test odds by {lr}."),
                    lr = sprintf("%.3f", LRN)
                )

                paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid currentColor; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    "<h3 style='margin: 0 0 5px 0; font-size: 16px;'>",
                    .("Diagnostic-test performance summary"),
                    "</h3></div><div style='font-size: 14px;'>",
                    "<p style='margin: 10px 0;'><strong>", .("Descriptive results:"),
                    "</strong> ", descriptive_result, "</p>",
                    "<table style='width: 100%; border-collapse: collapse; margin: 15px 0;'><tr>",
                    "<td style='border: 1px solid #ccc; padding: 10px; background-color: rgba(155, 155, 155, 0.06); color: inherit;'><strong>",
                    .("Sensitivity"), "</strong><br><span style='font-size: 18px;'>",
                    sprintf("%.1f%%", Sens * 100), "</span><br><span style='font-size: 12px;'>",
                    .("True-positive rate"), "</span></td>",
                    "<td style='border: 1px solid #ccc; padding: 10px; background-color: rgba(155, 155, 155, 0.06); color: inherit;'><strong>",
                    .("Specificity"), "</strong><br><span style='font-size: 18px;'>",
                    sprintf("%.1f%%", Spec * 100), "</span><br><span style='font-size: 12px;'>",
                    .("True-negative rate"), "</span></td></tr><tr>",
                    "<td style='border: 1px solid #ccc; padding: 10px; background-color: rgba(155, 155, 155, 0.06); color: inherit;'><strong>",
                    .("PPV"), "</strong><br><span style='font-size: 18px;'>",
                    sprintf("%.1f%%", PPV * 100), "</span><br><span style='font-size: 12px;'>",
                    prevalence_label, "</span></td>",
                    "<td style='border: 1px solid #ccc; padding: 10px; background-color: rgba(155, 155, 155, 0.06); color: inherit;'><strong>",
                    .("NPV"), "</strong><br><span style='font-size: 18px;'>",
                    sprintf("%.1f%%", NPV * 100), "</span><br><span style='font-size: 12px;'>",
                    prevalence_label, "</span></td></tr></table>",
                    "<p style='margin: 10px 0;'><strong>", .("Likelihood-ratio update:"),
                    "</strong></p><ul style='margin: 10px 0; padding-left: 25px;'><li>",
                    positive_update, "</li><li>", negative_update, "</li></ul>",
                    "<div style='background-color: rgba(155, 155, 155, 0.06); border: 1px solid #ccc; padding: 12px; margin: 15px 0; color: inherit;'>",
                    "<p style='margin: 0; font-weight: bold;'>", .("Interpretation caveat"),
                    "</p><p style='margin: 5px 0 0 0;'>",
                    .("These are single-cohort estimates. Their practical meaning depends on intended use, error consequences, prevalence, patient spectrum, and reference-standard quality. This educational summary is not a clinical guide."),
                    "</p></div></div></div>"
                )
            },
            .createAboutPanel = function() {
                paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid currentColor; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    "<h3 style='margin: 0 0 5px 0; font-size: 16px;'>",
                    .("About diagnostic-test evaluation"),
                    "</h3></div><div style='font-size: 14px;'><p><strong>",
                    .("What does this analysis do?"),
                    "</strong></p><p>",
                    .("This function evaluates a diagnostic test against a stated reference standard. It calculates sensitivity, specificity, predictive values, and likelihood ratios. Reference standards can be imperfect, so disagreement may reflect error in either method."),
                    "</p><p><strong>", .("When to use it:"),
                    "</strong></p><ul style='margin: 10px 0; padding-left: 25px;'><li>",
                    .("Describe a test against a stated reference standard."), "</li><li>",
                    .("Explore diagnostic methods or illustrative cut-off scenarios."), "</li><li>",
                    .("Demonstrate how prevalence changes predictive values."), "</li><li>",
                    .("Support, but do not replace, a clinical validation plan."),
                    "</li></ul><p><strong>", .("Key outputs:"),
                    "</strong></p><ul style='margin: 10px 0; padding-left: 25px;'><li>",
                    .("Sensitivity is the proportion of reference-positive observations with a positive test result."),
                    "</li><li>",
                    .("Specificity is the proportion of reference-negative observations with a negative test result."),
                    "</li><li>",
                    .("PPV and NPV are post-test probabilities and depend on prevalence."),
                    "</li><li>",
                    .("Likelihood ratios describe how a test result changes pre-test odds."),
                    "</li><li>",
                    .("Youden's index and the advanced metrics are descriptive summaries, not clinical decision thresholds."),
                    "</li></ul><p><strong>", .("References:"),
                    "</strong></p><ul style='margin: 10px 0; padding-left: 25px; font-size: 13px;'>",
                    "<li>Altman DG, Bland JM. Diagnostic tests. 1: Sensitivity and specificity. BMJ. 1994;308:1552.</li>",
                    "<li>Deeks JJ, Altman DG. Diagnostic tests 4: likelihood ratios. BMJ. 2004;329:168-169.</li>",
                    "<li><a href='https://cran.r-project.org/package=epiR' target='_blank'>epiR package documentation</a></li>",
                    "</ul><p><strong>", .("Scope:"), "</strong> ",
                    .("Presets and scenarios are illustrative examples, not clinical guides. A cut-off requires external validation and consideration of harms, benefits, workflow, and the intended population."),
                    "</p></div></div>"
                )
            },
            .createAssumptionsPanel = function(TP, TN, FP, FN, prev) {
                warnings <- character()

                # Check for extreme prevalence
                if (prev < 0.05 || prev > 0.95) {
                    warnings <- c(
                        warnings,
                        jmvcore::format(
                            .("Extreme prevalence is {prevalence}. PPV and NPV estimates may be unstable; verify them in the target population."),
                            prevalence = sprintf("%.1f%%", prev * 100)
                        )
                    )
                }

                # Check for zero cells
                if (FP == 0 || FN == 0) {
                    warnings <- c(
                        warnings,
                        .("Zero cells were detected. An estimated sensitivity or specificity of 100% may reflect limited validation data.")
                    )
                }

                # Check for very small error counts
                if ((FP > 0 && FP < 5) || (FN > 0 && FN < 5)) {
                    warnings <- c(
                        warnings,
                        .("Very few errors were observed. Small FP or FN counts may produce unstable estimates.")
                    )
                }

                warning_html <- if (length(warnings) > 0) {
                    paste0(
                        "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; margin: 10px 0; border-left: 4px solid #f0ad4e; color: inherit;'>",
                        "<h4 style='margin-top: 0;'>", .("Warnings"),
                        "</h4><ul style='margin: 10px 0; padding-left: 20px;'><li>",
                        paste(warnings, collapse = "</li><li>"),
                        "</li></ul></div>"
                    )
                } else {
                    paste0(
                        "<div style='background-color: rgba(33, 162, 64, 0.19); padding: 15px; margin: 10px 0; border-left: 4px solid #28a745; color: inherit;'>",
                        "<p style='margin: 0;'><strong>", .("No obvious sparse-cell warning."),
                        "</strong> ",
                        .("Assess adequacy from the precision required for the intended use."),
                        "</p></div>"
                    )
                }

                paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid currentColor; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    "<h3 style='margin: 0 0 5px 0; font-size: 16px;'>",
                    .("Assumptions and caveats"),
                    "</h3></div><div style='font-size: 14px;'><p><strong>",
                    .("Key assumptions:"),
                    "</strong></p><ul style='margin: 10px 0; padding-left: 25px;'><li>",
                    .("An imperfect reference standard can bias every estimated measure; document its limitations and consider latent-class or discrepant-resolution methods where appropriate."),
                    "</li><li>",
                    .("Interpret the index test and reference standard independently and use blinding where possible."),
                    "</li><li>",
                    .("The study population should represent the intended-use population and relevant disease spectrum."),
                    "</li><li>",
                    .("PPV and NPV vary with prevalence and should be verified in the target setting."),
                    "</li></ul><p><strong>", .("Common pitfalls:"),
                    "</strong></p><ul style='margin: 10px 0; padding-left: 25px;'><li>",
                    .("Verification bias occurs when reference-standard verification differs according to the index-test result."),
                    "</li><li>",
                    .("Incorporation bias occurs when the reference standard includes the test being evaluated."),
                    "</li><li>",
                    .("Spectrum bias occurs when the study population differs materially from clinical practice."),
                    "</li><li>",
                    .("Prevalence extrapolation can misstate PPV and NPV when study and target settings differ."),
                    "</li></ul>", warning_html,
                    "<p><strong>", .("Precision-based sample-size guidance:"),
                    "</strong></p><ul style='margin: 10px 0; padding-left: 25px;'><li>",
                    .("Choose the maximum acceptable confidence-interval half-width for sensitivity and specificity before collecting data."),
                    "</li><li>",
                    .("Use anticipated sensitivity, specificity, prevalence, confidence level, and expected loss or exclusions to calculate required reference-positive and reference-negative counts."),
                    "</li><li>",
                    .("Rare outcomes often require a larger total sample, but no universal total is adequate for every intended use."),
                    "</li><li>",
                    .("Report achieved confidence intervals because a nominal sample-size threshold does not guarantee adequate precision."),
                    "</li></ul></div></div>"
                )
            },
            .createGlossary = function() {
                entries <- list(
                    c(
                        .("Sensitivity (true-positive rate)"),
                        .("Proportion of reference-positive observations with a positive test result.")
                    ),
                    c(
                        .("Specificity (true-negative rate)"),
                        .("Proportion of reference-negative observations with a negative test result.")
                    ),
                    c(
                        .("PPV (positive predictive value)"),
                        .("Probability of a reference-positive outcome after a positive test; it depends on prevalence.")
                    ),
                    c(
                        .("NPV (negative predictive value)"),
                        .("Probability of a reference-negative outcome after a negative test; it depends on prevalence.")
                    ),
                    c(
                        .("LR+ (positive likelihood ratio)"),
                        .("Factor by which a positive result multiplies pre-test odds; practical importance depends on the starting probability and intended use.")
                    ),
                    c(
                        .("LR- (negative likelihood ratio)"),
                        .("Factor by which a negative result multiplies pre-test odds; practical importance depends on the starting probability and intended use.")
                    ),
                    c(
                        .("Youden's index (J)"),
                        .("Sensitivity plus specificity minus one; it weights both equally and is not a measure of clinical utility or a universal cut-off.")
                    ),
                    c(
                        .("Balanced accuracy"),
                        .("Unweighted average of sensitivity and specificity; it does not encode different consequences for the two error types.")
                    ),
                    c(
                        .("F1 score"),
                        .("Harmonic mean of sensitivity and PPV; because PPV depends on prevalence, F1 also changes with the selected prevalence.")
                    ),
                    c(
                        .("MCC (Matthews correlation coefficient)"),
                        .("Summary measure using all four cells, ranging from -1 to +1; interpretation is context-dependent.")
                    ),
                    c(
                        .("DOR (diagnostic odds ratio)"),
                        .("Odds of a positive result in reference-positive versus reference-negative observations; no universal clinical cut-off is assumed.")
                    )
                )
                terms <- vapply(entries, function(entry) {
                    paste0(
                        "<dt style='font-weight: bold; margin-top: 15px;'>",
                        entry[[1]],
                        "</dt><dd style='margin-left: 20px; margin-bottom: 10px;'>",
                        entry[[2]],
                        "</dd>"
                    )
                }, character(1))

                paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid currentColor; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    "<h3 style='margin: 0 0 5px 0; font-size: 16px;'>",
                    .("Diagnostic-test glossary"),
                    "</h3></div><div style='font-size: 14px;'><dl style='margin: 0;'>",
                    paste(terms, collapse = ""),
                    "</dl></div></div>"
                )
            }
        )
    )
}
