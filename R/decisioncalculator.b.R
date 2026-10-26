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
            # jmvcore's OptionString$valueAsSource quotes without escaping: a name containing `"`
            # broke the syntax pane, and a crafted one became R code that runs when pasted into R.
            .sourcifyOption = function(option) {
                value <- option$value
                if (option$name %in% c("cutoff1", "cutoff2", "prevalenceScenarios", "analysisUnit") &&
                    is.character(value) && length(value) == 1L && !identical(value, option$default))
                    return(paste0(option$name, " = ", encodeString(value, quote = '"')))
                super$.sourcifyOption(option)
            },

            # Notice collection helpers. A single Preformatted (plain-text) output item,
            # so the whole set renders in severity order with no HTML (project convention:
            # notice content must be plain text). NOTE (corrected 2026-09-22): jmvcore::Notice
            # itself serializes fine; the "attempt to apply non-function" crash blamed on it
            # was Group$insert() having no bounds check. Use $add(), never insert(999, ...).
            # Invalid counts are reported HERE rather than by rejecting the option, which is
            # why jamovi/decisioncalculator.a.yaml gives the counts no `min:`. ====
            .noticeList = list(),
            # A user-typed label (scenario name, unit) placed in a table text cell. jamovi
            # parses cells as HTML, so "<LOD" or "<high>" rendered as an empty cell. Same
            # rule as decision.b.R .richText(); apply at the cell only, never to a value
            # used for matching.
            .richText = function(x) gsub("<(?=[A-Za-z/!?])", "< ", as.character(x), perl = TRUE),

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
                        ERROR          = paste0(.("Error"), ": "),
                        STRONG_WARNING = paste0(.("Strong warning"), ": "),
                        WARNING        = paste0(.("Warning"), ": "),
                        INFO           = paste0(.("Info"), ": "),
                        ""
                    )
                    paste0(prefix, notice$title, "\n", notice$content)
                }, character(1))

                self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
            },
            # The epiR tables report a fixed statistic set - fixed per option, which .init() can
            # already see - so their rows are scaffolded here and .run() only fills values.
            .epirStatLabels = function() {
                c("ap"      = .("Apparent prevalence"),
                  "tp"      = .("Reference-standard prevalence in this sample"),
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
                  "p.dptn"  = .("False-omission proportion among test-negative subjects"))
            },
            # Balanced accuracy and F1 have no interval here, so they are not rows of an interval
            # table (their limits printed "NaN"); advancedMetricsTable shows both.
            .epirRatioStats = function() {
                if (isTRUE(self$options$pp)) c("se", "sp", "diag.ac")
                else c("ap", "tp", "se", "sp", "diag.ac", "pv.pos", "pv.neg",
                       "p.rout", "p.rin", "p.tpdn", "p.tndp", "p.dntp", "p.dptn")
            },
            .epirNumberStats = function() c("lr.pos", "lr.neg", "diag.or", "youden", "nndx"),

            # Youden's index = TP/n1 - FP/n0, a difference of two INDEPENDENT binomial
            # proportions. Agresti & Caffo (2000): add one success and one failure to each
            # group, Wald interval of the adjusted difference, clamped to [-1, 1]; the
            # estimate stays the observed index. Same code and rationale as decision.b.R:
            # epiR's interval adds the two Clopper-Pearson bounds and covered 0.997 under a
            # "95%" label (exact enumeration, validation 2026-10-06).
            .youdenCI = function(tp, n1, fp, n0, z = stats::qnorm(0.975)) {
                q1 <- (tp + 1) / (n1 + 2)
                q2 <- (fp + 1) / (n0 + 2)
                se <- sqrt(q1 * (1 - q1) / (n1 + 2) + q2 * (1 - q2) / (n0 + 2))
                c(est = tp / n1 - fp / n0,
                  lower = max(-1, q1 - q2 - z * se),
                  upper = min(1, q1 - q2 + z * se))
            },

            # "Inverted test" guard on the raw counts: "none", "info" or "strong". A point estimate
            # J < 0 occurs about half the time for an uninformative but correctly coded test, so
            # the STRONG_WARNING needs the whole Agresti-Caffo interval below 0 (one-sided 2.5%);
            # J < 0 within sampling variation, or with fractional counts (no interval), is INFO.
            # J < 0 <=> TP * n0 < FP * n1: exact for integer counts while the products stay below
            # 2^53, i.e. any realistic table; no tolerance, so J = 0 (1/3 + 2/3) is not "below 0".
            # When J < 0 and the interval is below 0, the 0.5-corrected LR+ is below 1 as well (the
            # shrunk difference is linear in the shrinkage), so this never contradicts the nomogram.
            .inversionGrade = function(tp, fn, fp, tn) {
                n1 <- tp + fn
                n0 <- fp + tn
                if (!(as.numeric(tp) * n0 < as.numeric(fp) * n1)) return("none")
                counts <- c(tp, fn, fp, tn)
                if (any(abs(counts - round(counts)) > 1e-6)) return("info")
                if (private$.youdenCI(tp, n1, fp, n0)[["upper"]] < 0) "strong" else "info"
            },

            # Number needed to diagnose = 1 / J. The confidence set for 1/J is
            # {1/j : j in (L, U), j > 0}: (1/U, 1/L) when L > 0, (1/U, +Inf) when L <= 0 < U.
            # NA where undefined or unbounded. epiR's min/max(1/bounds) excluded its own
            # estimate whenever the Youden interval crossed 0, and printed negative NNDx.
            .nndxFromYouden = function(y) {
                est <- if (is.finite(y[["est"]]) && y[["est"]] > 0) 1 / y[["est"]] else NA_real_
                lower <- if (is.finite(y[["upper"]]) && y[["upper"]] > 0) 1 / y[["upper"]] else NA_real_
                upper <- if (is.finite(est) && is.finite(y[["lower"]]) && y[["lower"]] > 0) 1 / y[["lower"]] else NA_real_
                c(est = est, lower = lower, upper = upper)
            },

            # Percent text with one decimal, and more only when one decimal would print a
            # probability strictly between 0 and 1 as "0.0%" or "100.0%" (prevalence 0.01%
            # read "0.0%", and "raises the probability from 0.0% to 0.0%").
            .pct = function(p) {
                if (!is.finite(p)) return(NA_character_)
                x <- 100 * p
                for (d in 1:4) {
                    s <- sprintf("%.*f", d, x)
                    v <- as.numeric(s)
                    if (!(x > 0 && v == 0) && !(x < 100 && v == 100)) return(paste0(s, "%"))
                }
                if (x < 50) "<0.0001%" else ">99.9999%"
            },

            # An NA cell is never blank in jamovi: jmvcore sends a numeric NA as a double, printed
            # "NaN", and a logical NA as missing, printed ".". An empty string is the only blank
            # cell, and $asDF reads it back as NA. So each NA cell becomes "" with a footnote
            # saying why. Call it after setRow(), which clears a cell's footnotes (no duplicates).
            .footnoteNA = function(table, values, note, rowNo = NA, rowKey = NULL) {
                for (col in names(values))
                    if (isTRUE(is.na(values[[col]]))) {
                        table$setCell(col = col, value = "", rowNo = rowNo, rowKey = rowKey)
                        table$addFootnote(col = col, note = note, rowNo = rowNo, rowKey = rowKey)
                    }
            },

            .init = function() {
                if (isTRUE(self$options$ci)) {
                    labels <- private$.epirStatLabels()
                    for (key in private$.epirRatioStats())
                        self$results$epirTable_ratio$addRow(rowKey = key,
                            values = list(statsnames = unname(labels[[key]])))
                    for (key in private$.epirNumberStats())
                        self$results$epirTable_number$addRow(rowKey = key,
                            values = list(statsnames = unname(labels[[key]])))
                }

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
                    .("Evaluates diagnostic-test performance by calculating sensitivity, specificity, predictive values, likelihood ratios, and advanced metrics from a 2\u00d72 confusion matrix."),
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
                    .("If 220 participants include 110 reference-positive and 110 reference-negative observations, with TP=90 and TN=80, then FN=20 and FP=30."),
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
                    values = list(cutoffName = private$.richText(self$options$cutoff1))
                )
                multipleCutoffTable$addRow(
                    rowKey = 2,
                    values = list(cutoffName = private$.richText(self$options$cutoff2))
                )
                multipleCutoffTable$addRow(
                    rowKey = 3,
                    values = list(cutoffName = .("Current (reference)"))
                )

                # library-audit 2026-10-23 meddecide [MEDIUM] DONE: the workload rows (observed + one per
                #   target prevalence) follow from the options, so they are laid down here and .run() fills them.
                if (isTRUE(self$options$showWorkload)) {
                    workload <- self$results$screeningWorkload
                    .deleteTableRows(workload)
                    for (i in seq_len(1 + length(private$.workloadTargets()$targets)))
                        workload$addRow(rowKey = as.character(i), values = list(
                            source = if (i == 1) .("Observed") else .("Projected"),
                            unit = private$.richText(self$options$analysisUnit)))
                }
            },
            # Target prevalences of the workload table, from the options alone, so .init() and .run()
            # build the same row keys. A list that does not parse gives no projected rows.
            .workloadTargets = function() {
                parsed <- tryCatch(
                    list(values = .dvNumbers(self$options$prevalenceScenarios, 0, 1, fallback = NULL), error = FALSE),
                    error = function(e) list(values = numeric(), error = TRUE))
                targets <- parsed$values
                if (self$options$pp) targets <- unique(c(self$options$pprob, targets))
                list(targets = targets, error = parsed$error, empty = is.null(parsed$values))
            },
            .run = function() {
                private$.noticeList <- list()
                on.exit(private$.renderNotices(), add = TRUE)

                # Read numbers from input ----
                # jamovi sends a typed whole number as int32, so these usually arrive as R
                # integers (table() counts in R do too). They stay integers here, so the count
                # tables keep integer cells; every PRODUCT of counts below is taken in double
                # (TP_cc..FN_cc, MCC), because the four-margin product overflows 2^31 - 1 from
                # about 430 subjects ("missing value where TRUE/FALSE needed").
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
                    .diagRejectInputErrors(private$.noticeList)
                }

                # Check for non-negative values
                if (TP < 0 || FP < 0 || TN < 0 || FN < 0) {
                    private$.addNotice("ERROR", .("Negative counts detected"), .("All counts (TP, FP, TN, FN) must be non-negative. Check the input values."))
                    .diagRejectInputErrors(private$.noticeList)
                }

                # Check for at least some data
                if (TP + FP + TN + FN == 0) {
                    private$.addNotice("ERROR", .("All counts are zero"), .("Provide diagnostic-test data with at least one non-zero count."))
                    .diagRejectInputErrors(private$.noticeList)
                }

                # Check for diseased subjects
                if (TP + FN == 0) {
                    private$.addNotice("ERROR", .("No reference-positive subjects"), .("TP + FN equals zero, so sensitivity and related measures cannot be calculated."))
                    .diagRejectInputErrors(private$.noticeList)
                }

                # Check for healthy subjects
                if (TN + FP == 0) {
                    private$.addNotice("ERROR", .("No reference-negative subjects"), .("TN + FP equals zero, so specificity and related measures cannot be calculated."))
                    .diagRejectInputErrors(private$.noticeList)
                }

                # Check for positive tests
                if (TP + FP == 0) {
                    private$.addNotice("WARNING", .("No positive test results"), .("TP + FP equals zero: the test was never positive. The positive predictive value, the positive likelihood ratio and the diagnostic odds ratio are not estimable, and the test cannot separate the two groups. The negative likelihood ratio is exactly 1: a negative result carries no information."))
                }

                # Check for negative tests
                if (TN + FN == 0) {
                    private$.addNotice("WARNING", .("No negative test results"), .("TN + FN equals zero: the test was never negative. The negative predictive value, the negative likelihood ratio and the diagnostic odds ratio are not estimable, and the test cannot separate the two groups. The positive likelihood ratio is exactly 1: a positive result carries no information."))
                }


                # Restored Html can retain old content despite clearWith, so the narrative panels
                # are cleared once validation has passed; invalid counts reject() above, and under
                # the error jamovi greys the previous run's panels in place.
                # library-audit 2026-10-23 meddecide [MEDIUM] DONE: no on.exit() visibility rule and no blank
                #   before reject(); visible: decides. "<div></div>" clears: the client skips "" (guide section 5).
                for (name in c("summary", "faganSummary", "assumptions", "about", "glossary"))
                    self$results[[name]]$setContent("<div></div>")

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

                # Zero cells, .diagRatioRules() (shared with decision, decisioncompare, decisioncombine):
                # a ratio is 0.5-corrected (all four cells + 0.5, Haldane-Anscombe) only when its
                # DENOMINATOR cell is empty and its numerator is not; an empty numerator gives an exact
                # 0 with interval [0, U] (.diagZeroRatioUpper); 0/0 is not estimable. Correcting a zero numerator too
                # printed LR+ 3.06 and DOR 3.16 for TP=0 FN=10 FP=1 TN=99, a test that found no
                # diseased patient (review 2026-10-21, DC2-01). Proportions, and epi.tests() on the
                # raw table3, use observed counts.
                zero_cell <- any(c(TP, FP, TN, FN) == 0)
                rules <- .diagRatioRules(TP, FP, FN, TN)
                # .diagRatioRules() leaves a correction that would land at or below 1 "undefined"
                # (TP=99 FN=1 FP=10 TN=0 gave LR- 0.33 for a test with specificity 0). Say why here:
                # an empty denominator with a non-empty numerator is that case.
                reversed <- c(
                    lrp = rules$lrp == "undefined" && FP == 0 && TP > 0,
                    lrn = rules$lrn == "undefined" && TN == 0 && FN > 0,
                    dor = rules$dor == "undefined" && (FP == 0 || FN == 0) && TP > 0 && TN > 0)
                ratio_names <- c(lrp = .("positive likelihood ratio"), lrn = .("negative likelihood ratio"),
                                 dor = .("diagnostic odds ratio"))
                for (k in names(reversed)[reversed]) {
                    private$.addNotice("WARNING", .("Zero-cell ratio not estimable"), .fmt(
                        .("The {ratio} has an empty denominator cell, so its observed value is infinite. Adding 0.5 to every cell would put it at or below 1 and reverse its direction, because the empty cell lies in the smaller reference group, so it is left blank."),
                        ratio = ratio_names[[k]]))
                }
                lrp_corr <- rules$lrp == "corrected"
                lrn_corr <- rules$lrn == "corrected"
                dor_corr <- rules$dor == "corrected"
                # Doubles: the DOR and the nomogram's cross products multiply these.
                cc <- function(x, on) if (on) x + 0.5 else as.numeric(x)
                TP_cc <- cc(TP, dor_corr)
                FP_cc <- cc(FP, dor_corr)
                TN_cc <- cc(TN, dor_corr)
                FN_cc <- cc(FN, dor_corr)

                TP_lrp <- cc(TP, lrp_corr)
                FP_lrp <- cc(FP, lrp_corr)
                TN_lrp <- cc(TN, lrp_corr)
                FN_lrp <- cc(FN, lrp_corr)

                TP_lrn <- cc(TP, lrn_corr)
                FP_lrn <- cc(FP, lrn_corr)
                TN_lrn <- cc(TN, lrn_corr)
                FN_lrn <- cc(FN, lrn_corr)

                if (zero_cell) {
                    private$.addNotice("WARNING",
                        if (lrp_corr || lrn_corr || dor_corr) .("Continuity correction applied") else .("Zero cell in the table"),
                        .("A zero cell was present. A likelihood ratio or the diagnostic odds ratio is corrected only when its denominator cell is empty and its numerator is not (FP for LR+, TN for LR-, FP or FN for the diagnostic odds ratio): 0.5 is then added to all four cells for that ratio and its confidence interval (Haldane-Anscombe correction). When its numerator cell is empty (TP for LR+, FN for LR-, TP or TN for the diagnostic odds ratio) the ratio is exactly 0, and its confidence interval runs from 0 to the upper limit of the same log-scale interval computed with 0.5 added to every cell; when both are empty it is not estimable. Sensitivity, specificity, predictive values and their intervals use the observed counts. The corrected diagnostic odds ratio need not equal the ratio of the displayed likelihood ratios."))
                }
                # Footnotes for a corrected ratio and for a ratio that is exactly 0.
                zc_note <- .("The denominator cell of this ratio is zero, so 0.5 was added to every cell (Haldane-Anscombe correction). Its confidence interval uses the same corrected counts.")
                zero_note <- .("The numerator cell of this ratio is zero, so the ratio is exactly 0 and is not corrected. Its confidence interval runs from 0 to the upper limit of the same log-scale interval computed with 0.5 added to every cell (Haldane-Anscombe correction).")

                # The early returns above guarantee DiseaseP, DiseaseN > 0. The fallbacks here and
                # for AccurT/PrevalenceD are NA_real_ (a footnoted blank), never a fabricated 0%,
                # should a guard ever move (review guide section 20).
                Sens <- if (DiseaseP > 0) TP / DiseaseP else NA_real_
                Spec <- if (DiseaseN > 0) TN / DiseaseN else NA_real_
                # Footnote for any table cell that has no value (see .footnoteNA)
                na_note <- .("Not estimable for this table: a denominator is zero or the quantity is undefined.")
                if (self$options$showWorkload) {
                    # A parse error used to abort .run() and discard every table for one mistyped
                    # scenario ("1%,5%"); it now gives no projected rows and this notice.
                    targets <- private$.workloadTargets()
                    if (targets$error)
                        private$.addNotice("ERROR", .("Invalid target prevalences"),
                            .("Target prevalences must be at most 100 comma-separated proportions between 0 and 1, for example 0.001,0.01. The projected rows for this list are omitted; every other result is unaffected."))
                    if (targets$empty)
                        private$.addNotice("INFO", .("No target prevalences"),
                            .("The target-prevalence list is empty, so no projected rows are added from it. Enter comma-separated proportions, for example 0.001,0.01."))
                    prev <- c(DiseaseP / (DiseaseP + DiseaseN), targets$targets)
                    # Rows laid down by .init() from the same targets; filled here.
                    for (i in seq_along(prev)) {
                        w <- .dvWorkload(Sens, Spec, prev[i],
                            if (i == 1) DiseaseP+DiseaseN else self$options$workloadPopulationSize)
                        w$net_benefit <- NULL
                        .fillRow(self$results$screeningWorkload, rowKey = as.character(i), values = c(
                            list(source = if (i == 1) .("Observed") else .("Projected"),
                                 unit = private$.richText(self$options$analysisUnit)), as.list(w)))
                        private$.footnoteNA(self$results$screeningWorkload, as.list(w), na_note,
                            rowKey = as.character(i))
                    }
                    self$results$screeningWorkload$setNote("source", .("Projected fractional counts assume sensitivity and specificity transport to the target population. These are flagged observations, not measured extra review time or unique patients. The projections use the point estimates of sensitivity and specificity and assume no uncertainty in them or in the target prevalence."))
                }

                AccurT <- if (TotalPop > 0) TestT / TotalPop else NA_real_
                PrevalenceD <- if (TotalPop > 0) DiseaseP / TotalPop else NA_real_

                # Determine which prevalence to use
                if (pp) {
                    # Known prior probability from population
                    PriorProb <- pprob
                } else {
                    # From ConfusionMatrix
                    PriorProb <- PrevalenceD
                }

                # A supplied population prevalence this far from 50% is the intended use (screening;
                # the workload defaults are 0.0001-0.01), so it is INFO about how strongly the
                # predictive values depend on the error rates. A sample prevalence that extreme stays
                # a STRONG_WARNING.
                if (pp && (PriorProb < 0.05 || PriorProb > 0.95)) {
                    private$.addNotice(
                        "INFO",
                        .("Extreme prevalence"),
                        .fmt(
                            .("The supplied prevalence is {prevalence}. At a prevalence this far from 50%, the predictive values depend strongly on the error rates: at low prevalence a small change in specificity moves the PPV considerably, and at high prevalence a small change in sensitivity moves the NPV."),
                            prevalence = private$.pct(PriorProb)
                        )
                    )
                } else if (PriorProb < 0.05 || PriorProb > 0.95) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Extreme prevalence"),
                        .fmt(
                            .("The selected prevalence is {prevalence}. Predictive values may be unstable or poorly transportable; verify them in the intended population and report the prevalence used."),
                            prevalence = private$.pct(PriorProb)
                        )
                    )
                }

                if (DiseaseP < 10 || DiseaseN < 10) {
                    private$.addNotice(
                        "STRONG_WARNING",
                        .("Very sparse reference groups"),
                        .fmt(
                            .("Only {positive} reference-positive and {negative} reference-negative observations are available. Estimates will be imprecise; inspect confidence intervals and justify sample size for the intended precision. This warning is not a clinical adequacy threshold."),
                            positive = base::format(DiseaseP, scientific = FALSE),
                            negative = base::format(DiseaseN, scientific = FALSE)
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

                # Probability of a reference-positive outcome after a NEGATIVE test (1 - NPV): the
                # nomogram's Post(-). The column used to report P(reference-negative | negative
                # test), which is the NPV again; 1 - NPV appeared only as epiR's false-omission row,
                # with ci on and the sample prevalence.
                PostTestProbNeg <- if (!is.na(Sens) && !is.na(Spec)) {
                    numerator <- PriorProb * (1 - Sens)
                    denominator <- (PriorProb * (1 - Sens)) + ((1 - PriorProb) * Spec)
                    if (denominator > 0) numerator / denominator else NA
                } else {
                    NA
                }


                # Likelihood ratios and the DOR by their zero-cell rule (above): exactly 0 for an empty
                # numerator, NA (blank, footnoted) when not estimable, else on the observed or the
                # all-cells + 0.5 counts. Its interval is computed in the ci block on the same counts.
                # A never-positive (never-negative) test makes LR+ (LR-) and the DOR 0/0: the old
                # correction turned them into LR+ = (n0 + 1) / (n1 + 1) under a "raises" nomogram.
                LRP <- switch(rules$lrp, undefined = NA_real_, zero = 0,
                    (TP_lrp / (TP_lrp + FN_lrp)) / (FP_lrp / (TN_lrp + FP_lrp)))
                LRN <- switch(rules$lrn, undefined = NA_real_, zero = 0,
                    (FN_lrn / (TP_lrn + FN_lrn)) / (TN_lrn / (TN_lrn + FP_lrn)))
                DOR <- switch(rules$dor, undefined = NA_real_, zero = 0,
                    (TP_cc * TN_cc) / (FN_cc * FP_cc))
                no_pos_tests <- TestP == 0
                no_neg_tests <- TestN == 0

                # Youden's Index (optimal cut-off criterion)
                YoudenIndex <- Sens + Spec - 1

                # sens + spec < 1: a positive result is rarer among reference-positive than among
                # reference-negative subjects, often TP/FN or FP/TN typed in swapped positions. Only
                # the Fagan path used to say so, so with the nomogram off 20/90/80/30 raised nothing.
                # Graded, see .inversionGrade(); the interval and n are quoted, as in decision.
                inversion <- private$.inversionGrade(TP, FN, FP, TN)
                if (inversion != "none") {
                    yci <- private$.youdenCI(TP, DiseaseP, FP, DiseaseN)
                    youden_txt <- sprintf("%.3f", YoudenIndex)
                    if (inversion == "strong") {
                        # base::format(): jmvcore::format() printed a round double count as "1e+05" (R
                        # wrapper), and a bare format() here IS jmvcore::format (@import jmvcore).
                        private$.addNotice("STRONG_WARNING", .("Test performs worse than chance"),
                            .fmt(.("Youden's index is {youden} (95% CI {lower} to {upper}; {npos} reference-positive and {nneg} reference-negative observations), and the whole interval is below 0: a positive result is less common among reference-positive than among reference-negative subjects. Check whether TP and FN, or FP and TN, were entered in swapped positions."),
                                youden = youden_txt, lower = sprintf("%.3f", yci[["lower"]]),
                                upper = sprintf("%.3f", yci[["upper"]]), npos = base::format(DiseaseP, scientific = FALSE),
                                nneg = base::format(DiseaseN, scientific = FALSE)))
                    } else if (fractional_counts) {
                        private$.addNotice("INFO", .("Youden's index below 0"),
                            .fmt(.("Youden's index is {youden}, below 0. No interval is computed for fractional counts, so whether this differs from chance is not assessed. If the counts are not coded as intended, check whether TP and FN, or FP and TN, were swapped."),
                                youden = youden_txt))
                    } else {
                        private$.addNotice("INFO", .("Youden's index below 0"),
                            .fmt(.("Youden's index is {youden} (95% CI {lower} to {upper}), below 0 but within sampling variation of 0, so this table does not show that the test discriminates better than chance. If the counts are not coded as intended, check whether TP and FN, or FP and TN, were swapped."),
                                youden = youden_txt, lower = sprintf("%.3f", yci[["lower"]]),
                                upper = sprintf("%.3f", yci[["upper"]])))
                    }
                }

                # Balanced Accuracy (useful when dealing with imbalanced data)
                BalancedAccuracy <- (Sens + Spec) / 2

                # F1 Score (harmonic mean of sensitivity and PPV)
                if (is.finite(Sens) && is.finite(PPV) && (Sens + PPV) > 0) {
                    F1Score <- 2 * (Sens * PPV) / (Sens + PPV)
                } else {
                    F1Score <- NA_real_
                }

                # Matthews Correlation Coefficient (MCC)
                # In double (see "Read numbers from input"); raw counts, not the corrected ones.
                mcc_numerator <- (as.numeric(TP) * TN) - (as.numeric(FP) * FN)
                mcc_denominator <- sqrt(as.numeric(TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
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
                ratio_values <- list(
                    tablename = .("Measures"),
                    Sens = Sens,
                    Spec = Spec,
                    AccurT = AccurT,
                    PrevalenceD = PriorProb,
                    PPV = PPV,
                    NPV = NPV,
                    PostTestProbDisease = PostTestProbDisease,
                    PostTestProbNeg = PostTestProbNeg,
                    LRP = LRP,
                    LRN = LRN
                )
                ratioTable$setRow(rowNo = 1, values = ratio_values)
                private$.footnoteNA(ratioTable, ratio_values, na_note, rowNo = 1)
                # Always visible (footnotes are off by default): the sample's prevalence is
                # an estimate of anything only for a cross-sectional sample.
                ratioTable$setNote("design", if (pp) NULL else
                    .("Prevalence, PPV and NPV (and their intervals) use this sample's own prevalence, which estimates the population prevalence only for a cross-sectional (consecutive) sample. For a case-control sample, supply the population prevalence instead."))


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
                        .fmt(
                            .("Sample accuracy is the proportion of concordant results. It is computed at the prevalence observed in this sample ({prevalence}), not at the supplied population prevalence, and changes with case mix."),
                            prevalence = private$.pct(PrevalenceD)
                        )
                    } else {
                        .fmt(
                            .("Sample accuracy is the proportion of concordant results. It is computed at the prevalence observed in this sample ({prevalence}) and changes with case mix."),
                            prevalence = private$.pct(PrevalenceD)
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

                    ratioTable$addFootnote(rowNo = 1, col = "PostTestProbNeg", .("Probability of a reference-positive outcome after a negative test, using the displayed prevalence. It equals 1 minus the NPV shown here."))

                    ratioTable$addFootnote(
                        rowNo = 1, col = "LRP",
                        .("Positive likelihood ratio: the factor by which a positive result multiplies pre-test odds. Its practical importance depends on the clinical context and starting probability.")
                    )

                    ratioTable$addFootnote(
                        rowNo = 1, col = "LRN",
                        .("Negative likelihood ratio: the factor by which a negative result multiplies pre-test odds. Its practical importance depends on the clinical context and starting probability.")
                    )

                    if (lrp_corr) ratioTable$addFootnote(rowNo = 1, col = "LRP", zc_note)
                    if (lrn_corr) ratioTable$addFootnote(rowNo = 1, col = "LRN", zc_note)
                    if (rules$lrp == "zero") ratioTable$addFootnote(rowNo = 1, col = "LRP", zero_note)
                    if (rules$lrn == "zero") ratioTable$addFootnote(rowNo = 1, col = "LRN", zero_note)
                }


                # Populate advanced metrics table ----
                advancedMetricsTable <- self$results$advancedMetricsTable
                advanced_values <- list(
                    tablename = .("Advanced measures"),
                    youdenIndex = YoudenIndex,
                    balancedAccuracy = BalancedAccuracy,
                    f1Score = F1Score,
                    mcc = MCC,
                    dor = DOR
                )
                advancedMetricsTable$setRow(rowNo = 1, values = advanced_values)
                private$.footnoteNA(advancedMetricsTable, advanced_values, na_note, rowNo = 1)

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
                    if (dor_corr) advancedMetricsTable$addFootnote(rowNo = 1, col = "dor", zc_note)
                    if (rules$dor == "zero") advancedMetricsTable$addFootnote(rowNo = 1, col = "dor", zero_note)
                }


                # 95% CI ----

                ci <- self$options$ci

                if (ci) {
                    epirTable_ratio <- self$results$epirTable_ratio
                    epirTable_number <- self$results$epirTable_number
                    # The rows are the fixed statistic set scaffolded in .init(); deleting them here
                    # would make the table appear only once epiR returns. Blank the values instead,
                    # footnoted: fill() below replaces each row it fills (setRow clears footnotes),
                    # so the footnote survives only on a row left empty - fractional counts, no
                    # epiR, an epiR error, or a statistic a future epiR no longer reports.
                    blank <- list(est = NA_real_, lower = NA_real_, upper = NA_real_)
                    blank_note <- .("Not computed for this table; the notices at the top say why.")
                    for (key in private$.epirRatioStats()) {
                        epirTable_ratio$setRow(rowKey = key, values = blank)
                        private$.footnoteNA(epirTable_ratio, blank, blank_note, rowKey = key)
                    }
                    for (key in private$.epirNumberStats()) {
                        epirTable_number$setRow(rowKey = key, values = blank)
                        private$.footnoteNA(epirTable_number, blank, blank_note, rowKey = key)
                    }

                    # Confidence intervals are an OPTIONAL extra. A missing epiR, or an
                    # error inside it, used to `return()` out of .run() -- which threw away
                    # the summary/about/assumptions/glossary panels and left plot1 without a
                    # state, so the Fagan nomogram came up blank with no explanation, even
                    # though every table above had already been filled successfully. Both
                    # now only skip the proportion intervals, the only ones epiR supplies.
                    epir_ok <- !fractional_counts &&
                        requireNamespace("epiR", quietly = TRUE)

                    if (!fractional_counts && !epir_ok) {
                        private$.addNotice(
                            "ERROR",
                            .("epiR package missing"),
                            .("The epiR package is required for the intervals of sensitivity, specificity and the other proportions. Install it or disable confidence intervals. That interval table is left blank; the likelihood-ratio, odds-ratio and Youden's index intervals and every other result on this page are unaffected.")
                        )
                    }

                    epirresult2 <- NULL
                    if (epir_ok) {
                        # Wrap ONLY the third-party call, so that a failure inside epiR
                        # cannot discard the tables already populated above. Nothing here
                        # raises jmvcore::reject() or a checkpoint restart.
                        epirresult2 <- tryCatch(
                            epiR::epi.tests(dat = table3) |>
                                summary() |>
                                as.data.frame(),
                            error = function(e) {
                                private$.addNotice(
                                    "ERROR",
                                    .("Confidence intervals could not be computed"),
                                    .fmt(
                                        .("epiR could not compute the intervals of the proportions for this table: {error}. That interval table is left blank; the likelihood-ratio, odds-ratio and Youden's index intervals and the point estimates above are unaffected."),
                                        error = conditionMessage(e)
                                    )
                                )
                                NULL
                            }
                        )
                    }

                    # Writes each row of df whose `statistic` is in keys (as.character: a factor
                    # would not match a string rowKey). Returns the number of rows it filled.
                    inf_note <- .("No finite limit: the interval is unbounded on this side.")
                    fill <- function(table, df, keys) {
                        stats <- as.character(df$statistic)
                        filled <- 0L
                        for (i in seq_len(nrow(df)))
                            if (stats[i] %in% keys) {
                                # epiR reports a zero cell as est = Inf with lower = NaN
                                # (verified: TP=90 FP=0 TN=80 FN=10 gives lr.pos and
                                # diag.or = Inf, lower = NaN). NaN is a floating-point
                                # artifact, not a confidence bound, and Inf is not an
                                # estimate; send NA and footnote the cell. A missing limit in
                                # a row with any finite value is an unbounded side (LR, DOR,
                                # and NNDx when only the Youden upper limit is above 0).
                                v <- c(df$est[i], df$lower[i], df$upper[i])
                                v[!is.finite(v)] <- NA_real_
                                table$setRow(rowKey = stats[i],
                                    values = list(est = v[1], lower = v[2],
                                                  upper = v[3]))
                                private$.footnoteNA(table, list(est = v[1]), na_note, rowKey = stats[i])
                                private$.footnoteNA(table, list(lower = v[2], upper = v[3]),
                                    if (all(is.na(v))) na_note else inf_note, rowKey = stats[i])
                                filled <- filled + 1L
                            }
                        filled
                    }
                    # Every interval here is binomial: one independent subject per count.
                    # Pathology counts are often slides, blocks or lesions, several per
                    # patient (Genders et al., Radiology 2012;265:910-6).
                    independence_note <- .("These intervals treat each count as one independent patient. If a patient contributes several slides, blocks or lesions, they can be too narrow: count one result per patient, or use a method for clustered data.")

                    # epirTable_number is not epiR's: every row is computed here, so it does not
                    # wait on epiR (an epiR error or a renamed statistic used to blank it).
                    # LR+, LR- and the DOR are the values the
                    # tables above show (all cells + 0.5 only when the ratio's denominator cell is
                    # empty, .diagRatioRules; none for an exact 0, whose interval is [0, U] with U from the
                    # +0.5 table, .diagZeroRatioUpper) with log-scale intervals on those same counts (Simel 1991; Woolf for
                    # the DOR), finite on both sides; without a zero cell this is exactly
                    # epi.tests(), and with one it is what decision and decisioncombine print.
                    # Known ceiling, accepted 2026-10-21 for agreement between the three
                    # analyses: a +0.5 bound can lie below the true ratio at extreme accuracy
                    # (true LR+ 900 at specificity 0.999 lay above every possible upper limit at
                    # 10 + 10). Until then this table left the upper limit blank when the RAW
                    # denominator count was 0 and set the lower to 0 when the raw numerator was
                    # 0; exact enumeration of that rule over six designs (10 + 10 to 100 + 100,
                    # sens/spec 0.5-0.999) gave mean coverage 0.94-0.96 for the LRs and
                    # 0.96-0.98 for the DOR. Restore it here AND in decision/decisioncombine if
                    # coverage at extreme accuracy matters more than the shared definition.
                    # Youden: Agresti-Caffo on the observed counts.
                    if (!fractional_counts) {
                        z <- stats::qnorm(0.975)
                        # Finite Haldane-Anscombe limits on the corrected counts, as decision and
                        # decisioncombine print for the same table (maintainer decision 2026-10-21;
                        # the raw-count blank/0 limits used to differ between the three analyses).
                        log_ci <- function(est, v) {
                            if (!is.finite(est)) return(c(NA_real_, NA_real_))   # not estimable
                            if (est > 0 && is.finite(v) && v > 0) exp(log(est) + c(-1, 1) * z * sqrt(v))
                            else c(NA_real_, NA_real_)
                        }
                        youden_ci <- private$.youdenCI(TP, TP + FN, FP, FP + TN, z)
                        # Simel variance of log(LR) = log(x / (x + y)) - log(u / (u + w)).
                        lr_v <- function(x, y, u, w) 1 / x - 1 / (x + y) + 1 / u - 1 / (u + w)
                        # Each LR's variance on the counts its point estimate uses (_lrp/_lrn),
                        # not another ratio's set: TP=25 FN=5 FP=0 TN=30 put LR- 0.1667 on raw
                        # counts inside an interval built from corrected ones. Only where those
                        # counts give variance 0 - the other ratio of a never-positive or
                        # never-negative test, e.g. LR- with TP = FP = 0 - would that be the
                        # zero-width [LR, LR] epiR prints, so that one interval comes from the
                        # all-cells +0.5 table.
                        v_or_cc <- function(v, v_cc) if (is.finite(v) && v > 0) v else v_cc
                        own <- rbind(
                            lr.pos = c(LRP, log_ci(LRP, v_or_cc(lr_v(TP_lrp, FN_lrp, FP_lrp, TN_lrp),
                                                                lr_v(TP + 0.5, FN + 0.5, FP + 0.5, TN + 0.5)))),
                            lr.neg = c(LRN, log_ci(LRN, v_or_cc(lr_v(FN_lrn, TP_lrn, TN_lrn, FP_lrn),
                                                                lr_v(FN + 0.5, TP + 0.5, TN + 0.5, FP + 0.5)))),
                            diag.or = c(DOR, log_ci(DOR, 1 / TP_cc + 1 / FP_cc + 1 / FN_cc + 1 / TN_cc)),
                            youden = unname(youden_ci),
                            nndx = unname(private$.nndxFromYouden(youden_ci)))
                        # An exact 0: [0, U], U from the +0.5 table (same helper as decision, decisioncombine).
                        zero_keys <- c(lr.pos = "lrp", lr.neg = "lrn", diag.or = "dor")
                        zero_rows <- names(zero_keys)[unlist(rules[zero_keys]) == "zero"]
                        for (k in zero_rows)
                            own[k, ] <- c(0, 0, .diagZeroRatioUpper(zero_keys[[k]], TP, FP, FN, TN))
                        fill(epirTable_number,
                             data.frame(statistic = rownames(own), est = own[, 1], lower = own[, 2], upper = own[, 3]),
                             private$.epirNumberStats())
                        for (k in zero_rows) epirTable_number$addFootnote(rowKey = k, col = "upper", zero_note)

                        # Always on: a reader cannot reproduce an interval without its method.
                        epirTable_number$setNote("method", .("Likelihood ratios: approximate log-scale 95% interval (Simel, Samsa and Matchar 1991), which can fall short of 95% when sensitivity or specificity is near 100% in a small study. Diagnostic odds ratio: log-scale (Woolf) interval. Youden's index: Agresti-Caffo interval for the difference between the true-positive and false-positive rates. Number needed to diagnose = 1 / Youden's index; its interval is the inverse of the positive part of the Youden interval."))
                        epirTable_number$setNote("independence", independence_note)
                        epirTable_number$setNote("rawcounts", if (zero_cell)
                            .("A zero cell was present. As in the tables above, a ratio whose denominator cell is empty (FP for LR+, TN for LR-, FP or FN for the diagnostic odds ratio) while its numerator cell is not uses the table with 0.5 added to every cell (Haldane-Anscombe), and so does its interval; these corrected limits are approximate. If that correction would put the ratio at or below 1 it is left blank, because it would reverse the direction of an infinite observed value. A ratio whose numerator cell is empty and denominator cell is not is exactly 0, and its interval runs from 0 to the upper limit of the same log-scale interval computed with 0.5 added to every cell; one with both cells empty (0/0) is not estimable. Every other ratio, Youden's index and the number needed to diagnose use the observed counts.")
                            else NULL)
                        # An empty NNDx cell needs its reason on screen (decision.b.R wording).
                        nn <- own["nndx", ]
                        epirTable_number$setNote("nndx",
                            if (!is.finite(nn[1]) && !is.finite(nn[2]))
                                .("The number needed to diagnose is not estimable: Youden's index and its whole confidence interval are at or below 0.")
                            else if (!is.finite(nn[1]))
                                .("Youden's index is 0 or below in this sample, so the number needed to diagnose has no point estimate. Its confidence interval starts at the lower limit shown and has no upper bound.")
                            else if (!is.finite(nn[3]))
                                .("The number needed to diagnose has no finite upper limit: the Youden's index interval includes 0, so the interval for its inverse has no upper bound.")
                            else NULL)
                    }

                    if (!is.null(epirresult2)) {
                        # The join is by epiR's own `statistic` strings, against the key set
                        # .init() scaffolded. A future epiR that renames them (it has before)
                        # would leave those rows blank with nothing on screen to say why.
                        n_filled <- fill(epirTable_ratio, epirresult2, private$.epirRatioStats())
                        if (n_filled == 0L) {
                            private$.addNotice(
                                "ERROR",
                                .("Confidence intervals could not be matched"),
                                .("No interval for sensitivity, specificity or the other proportions could be matched to the statistics epiR reported. The installed version of epiR labels its output differently from the version this analysis expects. That interval table is left blank; the likelihood-ratio, odds-ratio and Youden's index intervals and the point estimates above are unaffected.")
                            )
                        } else {
                            if (n_filled < length(private$.epirRatioStats()))
                                private$.addNotice(
                                    "WARNING",
                                    .("Some confidence intervals could not be matched"),
                                    .("Some rows of the interval table for the proportions are empty because the installed version of epiR does not report those statistics. The values that are shown are unaffected.")
                                )
                            epirTable_ratio$setNote("method", .("Exact Clopper-Pearson 95% intervals, computed by epiR from the observed counts."))
                            epirTable_ratio$setNote("independence", independence_note)
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
                                .fmt(
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
                                .fmt(
                                    .('All four frequencies for cut-off "{cutoff}" are zero.'),
                                    cutoff = cutoff_name
                                )
                            )
                            return(NULL)
                        }

                        # Safe division with appropriate handling for zero denominators
                        sens <- if (diseased > 0) tp / diseased else NA_real_
                        spec <- if (healthy > 0) tn / healthy else NA_real_
                        if (pp) {
                            # Bayes at the supplied prevalence needs both sens and spec; the
                            # sample formula is not a substitute (it printed PPV 0 / NPV 1).
                            ppv <- npv <- NA_real_
                            if (is.finite(sens) && is.finite(spec)) {
                                ppv_den <- sens * PriorProb + (1 - spec) * (1 - PriorProb)
                                npv_den <- spec * (1 - PriorProb) + (1 - sens) * PriorProb
                                if (ppv_den > 0) ppv <- sens * PriorProb / ppv_den
                                if (npv_den > 0) npv <- spec * (1 - PriorProb) / npv_den
                            }
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
                                msg <- .fmt(
                                    .('Cut-off "{cutoff}" has no reference-positive observations, so sensitivity is undefined.'),
                                    cutoff = cutoff_name
                                )
                            } else {
                                msg <- .fmt(
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

                        # No cutoffName: .init() wrote the (cell-safe) name, and setRow()
                        # leaves cells it is not given.
                        return(list(
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

                    # The cut-off scenarios are an OPTIONAL extra. This used to `return()`
                    # out of .run(), so one bad scenario frequency also blanked the
                    # summary/about/assumptions/glossary panels and left plot1 without a
                    # state (an empty Fagan nomogram) -- with nothing on screen to say why.
                    # Skip only the comparison; calculate_cutoff_metrics() has already raised a
                    # notice naming the offending scenario and its frequencies. The rows that can
                    # be computed are still filled: unset .init() cells render "." in jamovi, so a
                    # single bad scenario used to blank the current row as well.
                    if (is.null(cutoff1_metrics) || is.null(cutoff2_metrics)) {
                        bad <- c(self$options$cutoff1, self$options$cutoff2)[
                            c(is.null(cutoff1_metrics), is.null(cutoff2_metrics))]
                        private$.addNotice(
                            "ERROR",
                            .("Cut-off validation failed"),
                            .fmt(
                                .("The cut-off comparison is omitted because these scenarios have invalid frequencies: {scenarios}. Their rows are left blank; the other rows and the rest of the analysis are unaffected."),
                                scenarios = paste(bad, collapse = ", ")
                            )
                        )
                        invalid_row <- list(sensitivity = "", specificity = "", ppv = "", npv = "",
                                            accuracy = "", youden = "",
                                            recommendation = .("Not computed: invalid frequencies"))
                        rows <- list(cutoff1_metrics %||% invalid_row, cutoff2_metrics %||% invalid_row,
                                     list(sensitivity = Sens, specificity = Spec, ppv = PPV, npv = NPV,
                                          accuracy = AccurT, youden = YoudenIndex,
                                          recommendation = .("Descriptive comparison unavailable")))
                        for (k in c(1, 2, 3)) {   # double keys, as .init() created them
                            multipleCutoffTable$setRow(rowKey = k, values = rows[[k]])
                            private$.footnoteNA(multipleCutoffTable, rows[[k]], na_note, rowKey = k)
                        }
                    }
                }

                if (self$options$multiplecuts &&
                    !is.null(cutoff1_metrics) && !is.null(cutoff2_metrics)) {
                    # Rows are created in .init(); only the computed cells are set here.
                    multipleCutoffTable$setRow(rowKey = 1, values = cutoff1_metrics)
                    private$.footnoteNA(multipleCutoffTable, cutoff1_metrics, na_note, rowKey = 1)
                    multipleCutoffTable$setRow(rowKey = 2, values = cutoff2_metrics)
                    private$.footnoteNA(multipleCutoffTable, cutoff2_metrics, na_note, rowKey = 2)

                    # Optimal cut-off recommendation: the best of the three on Youden's J, its
                    # margin over the current cut-off, and an uncertainty statement about that
                    # SAME quantity. It used to rank on J and judge by Wilson ACCURACY
                    # intervals, so a winner with far lower accuracy (0.595 vs 0.941) read
                    # "their sample-accuracy intervals do not overlap" (validation 2026-10-06).
                    #
                    # A comparison means anything only for thresholds on the SAME cohort, and
                    # moving a threshold changes neither the reference-positive nor the
                    # reference-negative count. The guard used to compare totals only, so
                    # 120/100 and 110/110 cases passed as one cohort.
                    o <- self$options
                    cell <- list(c(TP, FN, FP, TN), c(o$tp1, o$fn1, o$fp1, o$tn1),
                                 c(o$tp2, o$fn2, o$fp2, o$tn2))
                    n_pos <- vapply(cell, function(x) x[1] + x[2], numeric(1))
                    n_neg <- vapply(cell, function(x) x[3] + x[4], numeric(1))
                    if (length(unique(n_pos)) > 1 || length(unique(n_neg)) > 1) {
                        # vapply(numeric(1)) made even jamovi's int32 counts doubles, and paste0()
                        # printed a group of exactly 100000 as "1e+05".
                        num <- function(x) base::format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
                        cohort <- paste0(num(n_pos), "/", num(n_neg))
                        private$.addNotice(
                            "WARNING",
                            .("Cut-offs describe different cohorts"),
                            .fmt(
                                .("The current row and the two scenarios have {current}, {scenario1} and {scenario2} reference-positive/reference-negative cases. Moving a cut-off on one cohort changes neither count, so these rows cannot be interpreted as thresholds applied to the same participants."),
                                current = cohort[1],
                                scenario1 = cohort[2],
                                scenario2 = cohort[3]
                            )
                        )
                    }

                    # Moving one threshold of one marker trades sensitivity against specificity;
                    # a row better on BOTH than another is a different test, not a cut-off.
                    cand_names <- c(.("Current"), o$cutoff1, o$cutoff2)
                    se3 <- c(Sens, cutoff1_metrics$sensitivity, cutoff2_metrics$sensitivity)
                    sp3 <- c(Spec, cutoff1_metrics$specificity, cutoff2_metrics$specificity)
                    dominant <- which(outer(se3, se3, ">") & outer(sp3, sp3, ">"), arr.ind = TRUE)
                    if (nrow(dominant) > 0) {
                        private$.addNotice(
                            "WARNING",
                            .("Rows are not thresholds of one marker"),
                            .fmt(
                                .("{better} has both higher sensitivity and higher specificity than {worse}. Moving a single threshold on the same patients trades one for the other, so these rows describe different tests or cohorts, not cut-offs of one marker."),
                                better = cand_names[dominant[1, 1]],
                                worse = cand_names[dominant[1, 2]]
                            )
                        )
                    }
                    ys <- c(YoudenIndex, cutoff1_metrics$youden, cutoff2_metrics$youden)
                    ys[!is.finite(ys)] <- NA_real_

                    # Uncertainty: the Agresti-Caffo interval of each scenario's J, computed
                    # separately. A formal test would need each patient's classification at
                    # both thresholds, which summary counts cannot supply; because the
                    # scenarios classify the same patients, non-overlap of separate intervals
                    # is a conservative signal of a difference, and overlap establishes none.
                    integer_scenarios <- vapply(cell, function(x) all(abs(x - round(x)) <= 1e-6), logical(1))
                    j_ci <- lapply(seq_along(cell), function(i) {
                        x <- cell[[i]]
                        if (!integer_scenarios[i] || x[1] + x[2] <= 0 || x[3] + x[4] <= 0)
                            return(c(NA_real_, NA_real_))
                        unname(private$.youdenCI(x[1], x[1] + x[2], x[3], x[3] + x[4])[c("lower", "upper")])
                    })
                    if (any(!integer_scenarios)) {
                        private$.addNotice(
                            "WARNING",
                            .("Cut-off intervals omitted for fractional counts"),
                            .("Youden's index intervals are not computed for scenarios containing fractional frequencies because no effective sample size or weighting design was supplied.")
                        )
                    }

                    optimal_msg <- .("Descriptive comparison unavailable")
                    if (any(is.finite(ys))) {
                        best <- which.max(replace(ys, !is.finite(ys), -Inf))
                        margin <- ys[best] - ys[1]                      # vs the current cut-off
                        ov <- NA                                        # do the J intervals overlap?
                        if (best != 1L && all(is.finite(c(j_ci[[best]], j_ci[[1]]))))
                            ov <- j_ci[[best]][1] <= j_ci[[1]][2] && j_ci[[1]][1] <= j_ci[[best]][2]

                        if (best == 1L || !is.finite(margin) || margin <= 0) {
                            optimal_msg <- .fmt(
                                .("Current cut-off has the highest Youden's J ({youden}) of the three."),
                                youden = sprintf("%.3f", ys[1])
                            )
                        } else if (isTRUE(ov)) {
                            optimal_msg <- .fmt(
                                .("{cutoff} has the highest Youden's J, {margin} above current; its Youden's J interval overlaps the current cut-off's, so these separate intervals do not show a difference."),
                                cutoff = cand_names[best],
                                margin = sprintf("%.3f", margin)
                            )
                        } else if (isFALSE(ov)) {
                            optimal_msg <- .fmt(
                                .("{cutoff} has the highest Youden's J, {margin} above current; their Youden's J intervals do not overlap."),
                                cutoff = cand_names[best],
                                margin = sprintf("%.3f", margin)
                            )
                        } else {
                            optimal_msg <- .fmt(
                                .("{cutoff} has the highest Youden's J, {margin} above current; paired uncertainty cannot be assessed from these summary counts."),
                                cutoff = cand_names[best],
                                margin = sprintf("%.3f", margin)
                            )
                        }
                    }

                    multipleCutoffTable$setNote(
                        "sameData",
                        jmvcore::.("The named scenarios are illustrative examples, not clinical guides. The current row reports which scenario has the highest Youden's J and whether its separately calculated Youden's J interval overlaps the current row. Choosing a cut-off on the same data used to assess it is optimistically biased and does not establish clinical utility.")
                    )

                    multipleCutoffTable$setNote(
                        "uncertainty",
                        jmvcore::.("Cut-offs are compared on point estimates; no paired test of the difference is performed. The intervals referred to above are Agresti-Caffo 95% intervals for each scenario's Youden's J, computed separately. For thresholds applied to the same patients, non-overlap is a conservative signal of a difference, and overlap does not show that there is none. Like every interval here, they treat each count as one independent patient and can be too narrow when a patient contributes several slides, blocks or lesions.")
                    )

                    current_values <- list(
                        cutoffName = .("Current (reference)"),
                        sensitivity = Sens,
                        specificity = Spec,
                        ppv = PPV,
                        npv = NPV,
                        accuracy = AccurT,
                        youden = YoudenIndex,
                        recommendation = private$.richText(optimal_msg)   # quotes a scenario name
                    )
                    multipleCutoffTable$setRow(rowKey = 3, values = current_values)
                    private$.footnoteNA(multipleCutoffTable, current_values, na_note, rowKey = 3)
                }

                # Generate Summary, About, and Glossary panels ----

                # Summary panel
                if (self$options$showSummary) {
                    summary_html <- private$.createSummary(
                        Sens, Spec, PPV, NPV, LRP, LRN,
                        YoudenIndex, AccurT, PriorProb, lrp_corr || lrn_corr
                    )
                    self$results$summary$setContent(summary_html)
                }

                # About and Assumptions panels
                if (self$options$showAbout) {
                    about_html <- private$.createAboutPanel()
                    self$results$about$setContent(about_html)

                    assumptions_html <- private$.createAssumptionsPanel(TP, TN, FP, FN, PriorProb, supplied = pp)
                    self$results$assumptions$setContent(assumptions_html)
                }

                # Glossary panel
                if (self$options$showGlossary) {
                    glossary_html <- private$.createGlossary()
                    self$results$glossary$setContent(glossary_html)
                }

                # Send Data to Plot ----


                # nomogrammer refuses LR+ < 1 ("should be >= 1 for an informative test"), which is a
                # fair objection -- a positive result that argues AGAINST disease inverts the
                # nomogram's meaning -- but it arrived as an unexplained crash. Detect it here, where
                # a notice can still be rendered, and tell the plot to decline instead.
                # Decided on the counts, not on the derived double: sens 1/3, spec 2/3 is an
                # LR+ of exactly 1 but is stored as 0.9999999999999999, and `LRP >= 1` then
                # declined the plot and told the user the test coding was inverted.
                # LR+ >= 1  <=>  TP (TN + FP) >= FP (TP + FN), on the counts LR+ uses.
                # Half-integer cross products are exact in floating point; the relative
                # tolerance only guards fractional or very large counts. Each ratio gets its
                # own test: LR+ within rounding of 1 does not make LR- 1.
                lr_pos_cross <- TP_lrp * (TN_lrp + FP_lrp) - FP_lrp * (TP_lrp + FN_lrp)   # sign of LR+ - 1
                lr_neg_cross <- FN_lrn * (TN_lrn + FP_lrn) - TN_lrn * (TP_lrn + FN_lrn)   # sign of LR- - 1
                lr_pos_one <- abs(lr_pos_cross) <= 1e-12 * TP_lrp * (TN_lrp + FP_lrp)
                lr_neg_one <- abs(lr_neg_cross) <= 1e-12 * TN_lrn * (TP_lrn + FN_lrn)
                # The nomogram is a log-odds scale: a likelihood ratio of exactly 0 (an empty
                # numerator cell) or one that is not estimable cannot be placed on it. NULL = drawable.
                fagan_reason <- if (no_pos_tests)
                    .("The test was never positive in this table, so the positive likelihood ratio is not estimable.")
                else if (no_neg_tests)
                    .("The test was never negative in this table, so the negative likelihood ratio is not estimable.")
                else if (rules$lrp == "zero")
                    .("No reference-positive subject tested positive (TP = 0), so the positive likelihood ratio is exactly 0. A ratio of 0 cannot be placed on the nomogram's logarithmic scale.")
                else if (rules$lrn == "zero")
                    .("No reference-positive subject tested negative (FN = 0), so the negative likelihood ratio is exactly 0. A ratio of 0 cannot be placed on the nomogram's logarithmic scale.")
                else if (!is.finite(LRP) || !is.finite(LRN))
                    .("A likelihood ratio is not estimable for this table (see the notices above), so it cannot be placed on the nomogram.")
                else if (!(lr_pos_cross > 0 || lr_pos_one)) .fmt(
                    .("The positive likelihood ratio is {lr}, so a positive result lowers the probability of a reference-positive outcome rather than raising it. A nomogram assumes that a positive result raises it. Check the test coding."),
                    lr = sprintf("%.3f", LRP))
                # nomogrammer rejects sensitivity or specificity of exactly 0 or 1, and recomputes the
                # ratios from them. Without a zero cell the observed pair is strictly inside (0, 1).
                # With one, at least one LR is corrected: draw the pair whose ratios are exactly the
                # table's LRP and LRN, so plot, panel and tables quote one number (the all-cells
                # corrected pair drew LR- 0.969 beside a table LR- of 1.010, DC2-02).
                sens_plot <- Sens
                spec_plot <- Spec
                if (is.null(fagan_reason) && zero_cell) {
                    sp_b <- (LRP - 1) / (LRP - LRN)
                    se_b <- LRP * (1 - sp_b)
                    if (all(is.finite(c(se_b, sp_b))) && se_b > 0 && se_b < 1 && sp_b > 0 && sp_b < 1) {
                        sens_plot <- se_b
                        spec_plot <- sp_b
                    } else {
                        fagan_reason <- .("The two likelihood ratios in the table cannot be drawn together on one nomogram.")
                    }
                }
                fagan_ok <- is.null(fagan_reason)
                if (isTRUE(self$options$fagan) && !fagan_ok) {
                    private$.addNotice("WARNING", .("Fagan nomogram not drawn"),
                        paste(fagan_reason, .("The nomogram is omitted.")))
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
                fagan_item <- self$results$faganSummary
                if (isTRUE(self$options$fagan)) {
                    if (!fagan_ok) {
                        not_drawn_reason <- fagan_reason
                        fagan_item$setContent(paste0(
                            "<div style='padding:12px;border-left:4px solid #c00;background-color: rgba(255, 88, 88, 0.06); color: inherit;'>",
                            "<p>", .("No nomogram is drawn for this table."), " ",
                            not_drawn_reason,
                            "</p></div>"))
                    } else {
                        pre <- PriorProb
                        # These sentences quote LRP/LRN (Haldane-Anscombe corrected when a
                        # denominator cell is empty) and sit directly under a nomogram drawn from
                        # proportions with exactly those ratios. PostTestProbDisease is Bayes on the
                        # UNcorrected Sens/Spec, so with a zero cell the panel printed a
                        # post-test probability the quoted likelihood ratio cannot produce
                        # (100.0% beside LR+ = 145.16, against the plot's 99.5%). Take the
                        # estimate and its update factor from one computation. Without a
                        # zero cell these are algebraically identical to the old values.
                        # PriorProb is strictly inside (0, 1): DiseaseP == 0 and
                        # DiseaseN == 0 both early-return above, and pprob is bounded.
                        pre_odds <- pre / (1 - pre)
                        post_pos <- (pre_odds * LRP) / (1 + pre_odds * LRP)
                        post_neg <- (pre_odds * LRN) / (1 + pre_odds * LRN)
                        pretest_sentence <- if (isTRUE(self$options$pp)) {
                            .fmt(
                                .("Pre-test probability is {prevalence}, using the population prevalence you supplied."),
                                prevalence = private$.pct(pre)
                            )
                        } else {
                            .fmt(
                                .("Pre-test probability is {prevalence}, using the prevalence observed in this study table."),
                                prevalence = private$.pct(pre)
                            )
                        }
                        # An uninformative result neither raises nor lowers the probability.
                        positive_sentence <- if (lr_pos_one) .fmt(
                                .("A positive result has a likelihood ratio of {lr} and leaves the probability at {prevalence}."),
                                lr = sprintf("%.2f", LRP), prevalence = private$.pct(pre))
                            else .fmt(
                                .("A positive result has a likelihood ratio of {lr} and raises the probability from {prevalence} to {posttest}."),
                                lr = sprintf("%.2f", LRP), prevalence = private$.pct(pre), posttest = private$.pct(post_pos))
                        # The verb follows the sign of LR- - 1 (DC2-02 said "lowers" beside LR- 1.010).
                        negative_sentence <- if (lr_neg_one) .fmt(
                                .("A negative result has a likelihood ratio of {lr} and leaves the probability at {prevalence}."),
                                lr = sprintf("%.3f", LRN), prevalence = private$.pct(pre))
                            else if (lr_neg_cross > 0) .fmt(
                                .("A negative result has a likelihood ratio of {lr} and raises the probability from {prevalence} to {posttest}."),
                                lr = sprintf("%.3f", LRN), prevalence = private$.pct(pre), posttest = private$.pct(post_neg))
                            else .fmt(
                                .("A negative result has a likelihood ratio of {lr} and lowers the probability from {prevalence} to {posttest}."),
                                lr = sprintf("%.3f", LRN), prevalence = private$.pct(pre), posttest = private$.pct(post_neg))
                        # Disclose which table the probabilities above were read off.
                        # They now follow the corrected likelihood ratios (and the plot);
                        # the Measures table's PPV/NPV are raw-count Bayes, so with a zero
                        # cell the two panels legitimately differ by a fraction of a point.
                        correction_paragraph <- if (lrp_corr || lrn_corr) {
                            paste0("<p style='font-size:90%;color:inherit;'>",
                                .("A likelihood ratio whose denominator cell is empty uses the Haldane-Anscombe 0.5 correction, and so do the probabilities read from the nomogram. The predictive values and post-test probabilities in the Measures table are computed from the raw counts and can differ, by a wide margin in a small table."),
                                "</p>")
                        } else {
                            ""
                        }
                        fagan_item$setContent(paste0(
                            "<div style='padding:12px;border-left:4px solid #1565c0;background-color: rgba(88, 155, 255, 0.06); color: inherit;'>",
                            "<p>", pretest_sentence, "</p>",
                            "<p>", positive_sentence, "</p>",
                            "<p>", negative_sentence, "</p>",
                            correction_paragraph,
                            "<p style='font-size:90%;color:inherit;'>",
                            .("Read the nomogram by drawing a line from the pre-test probability on the left, through the likelihood ratio in the middle, to the post-test probability on the right. Sensitivity and specificity describe agreement with the reference standard; the pre-test probability depends on population context, so the same test can lead to a different endpoint in another population."),
                            "</p></div>"))
                    }
                }

                # Render collected notices once at the end of a successful run. This also
                # clears the panel when no notice was raised this cycle (.noticeList was
                # reset at the top of .run()), preventing a stale notice from persisting
                # after the user corrects an input not covered by the notices clearWith.
                private$.renderNotices()
            },
            .plot1 = function(image1, ggtheme, theme = NULL, ...) {
                plotData1 <- image1$state
                if (is.null(plotData1)) return(FALSE)
                # Set in .run() when LR+ < 1; the explanatory notice is raised there, because
                # notices are rendered before any plot function runs.
                if (identical(plotData1$drawable, FALSE)) return(FALSE)

                # Pass ONE input pair. nomogrammer treats both pairs as a conflict: it
                # warns (invisible in jamovi), discards Plr/Nlr and recomputes them from
                # Sens/Spec -- but the discarded pair still runs through its likelihood-ratio
                # validator, which aborts with a raw R error when LR+ == LR- ("uninformative
                # test"). That fires for any Youden's J == 0 table, e.g. 50/50/50/50, which
                # fagan_ok lets through because LR+ == 1 satisfies LR+ >= 1. Sens/Spec here
                # are the pair whose ratios are the table's LRP/LRN (observed, or back-solved
                # from a corrected LR in .run()), so nomogrammer recomputes exactly those.
                plot1 <- nomogrammer(
                    Prevalence = plotData1$Prevalence,
                    Sens = plotData1$Sens,
                    Spec = plotData1$Spec,
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = (14 / 5),
                    # Verbose only cat()s a summary block to stdout, which jamovi never
                    # shows; that reading is rendered as faganSummary instead.
                    Verbose = FALSE,
                    # jamovi's theme as the base, the nomogram's structural theme on top (inside
                    # nomogrammer), and the figure's text translated here, as decision and cotest do.
                    Theme = ggtheme,
                    Title = .("Fagan Nomogram"),
                    PriorLabel = .("Prior\nProb.\n(%)"),
                    PosteriorLabel = .("Posterior\nProb.\n(%)"),
                    DetailLabels = c(prevalence = .("Prevalence"), plr = .("PLR"), nlr = .("NLR"),
                                     post_pos = .("Post(+)"), post_neg = .("Post(-)")),
                    Colors = .faganColors(self$options$faganColors, theme),
                    LegendLabels = c(pos = .("Positive result"), neg = .("Negative result"))
                )

                print(plot1)
                TRUE
            },

            # Private helper methods for summaries ----

            .createSummary = function(Sens, Spec, PPV, NPV, LRP, LRN, Youden, Accuracy, Prevalence, lr_corrected = FALSE) {
                # PPV is NA when TP + FP == 0 and NPV when TN + FN == 0. Both are only
                # WARNINGs, not early returns, so this panel is still built and
                # sprintf("%.1f%%", NA) printed the literal "NA%" in 18px type while the
                # matching ratioTable cell was correctly blank. Say the same thing twice.
                na_label <- .("not estimable")
                fmtPct <- function(x) if (is.finite(x)) private$.pct(x) else na_label
                descriptive_result <- .fmt(
                    .("Youden's index is {youden} and sample accuracy is {accuracy}. These values describe the entered study table; they are not clinical grades or decision thresholds."),
                    youden = sprintf("%.3f", Youden),
                    accuracy = fmtPct(Accuracy)
                )
                positive_update <- if (is.finite(LRP)) .fmt(
                    .("A positive result multiplies pre-test odds by {lr}."),
                    lr = sprintf("%.2f", LRP)
                ) else .("The positive likelihood ratio is not estimable for this table; the notices above say why.")
                negative_update <- if (is.finite(LRN)) .fmt(
                    .("A negative result multiplies pre-test odds by {lr}."),
                    lr = sprintf("%.3f", LRN)
                ) else .("The negative likelihood ratio is not estimable for this table; the notices above say why.")

                paste0(
                    "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                    "<div style='background-color: rgba(88, 88, 88, 0.06); border: 2px solid currentColor; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                    "<h3 style='margin: 0 0 5px 0; font-size: 16px;'>",
                    .("Diagnostic-test performance summary"),
                    "</h3></div><div style='font-size: 14px;'>",
                    "<p style='margin: 10px 0;'><strong>", .("Descriptive results:"),
                    "</strong> ", descriptive_result, "</p>",
                    # library-audit 2026-10-23 meddecide [HIGH] DONE: no bordered grid here; sensitivity,
                    #   specificity, PPV and NPV are in ratioTable, a native Table (guide section 26).
                    "<p style='margin: 10px 0;'><strong>", .("Likelihood-ratio update:"),
                    "</strong></p><ul style='margin: 10px 0; padding-left: 25px;'><li>",
                    positive_update, "</li><li>", negative_update, "</li></ul>",
                    # The LRs above are Haldane-Anscombe corrected on a zero cell and the PPV /
                    # NPV are not, so "PPV 100.0%" sat beside "multiplies pre-test odds by
                    # 11.00" with nothing to reconcile them.
                    if (isTRUE(lr_corrected)) paste0("<p style='margin: 10px 0; font-size: 13px;'>",
                        .("A likelihood ratio whose denominator cell is empty uses the Haldane-Anscombe 0.5 correction, while the predictive values above use the observed counts. Multiplying the pre-test odds by these likelihood ratios therefore need not reproduce the predictive values exactly."),
                        "</p>") else "",
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
            .createAssumptionsPanel = function(TP, TN, FP, FN, prev, supplied = FALSE) {
                warnings <- character()

                # Check for extreme prevalence. A supplied population prevalence is the intended use
                # and is explained by an INFO notice, so it is not listed as a warning here.
                if (!supplied && (prev < 0.05 || prev > 0.95)) {
                    warnings <- c(
                        warnings,
                        .fmt(
                            .("Extreme prevalence is {prevalence}. PPV and NPV estimates may be unstable; verify them in the target population."),
                            prevalence = private$.pct(prev)
                        )
                    )
                }

                # Check for zero cells. All four are zero cells, all four trigger the
                # Haldane-Anscombe correction in .run(); testing only FP and FN gave a
                # TP == 0 or TN == 0 table the green "no sparse-cell warning" box directly
                # under a notice saying a continuity correction had been applied.
                if (TP == 0 || TN == 0 || FP == 0 || FN == 0) {
                    warnings <- c(
                        warnings,
                        .("A zero cell was detected. Estimates pinned at 0% or 100% may reflect limited validation data, and a Haldane-Anscombe correction was applied to the likelihood ratios and diagnostic odds ratio.")
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
                    .("Each count is one independent patient. If a patient contributes several slides, blocks or lesions, count one result per patient or use a method for clustered data; otherwise the confidence intervals can be too narrow."),
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
