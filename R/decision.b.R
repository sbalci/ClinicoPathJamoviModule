#' @title Medical Decision Analysis
#' @description Backend of the jamovi \code{decision} analysis: the diagnostic accuracy of
#'   one binary test against a binary reference standard, from the 2x2 table formed by the
#'   positive and negative levels chosen for each of two categorical variables.
#' @details Reports sensitivity, specificity, sample accuracy, prevalence, predictive values
#'   (at the sample prevalence, or by Bayes' theorem at a supplied population prevalence)
#'   and likelihood ratios. Optional outputs: 95 percent confidence intervals
#'   (Clopper-Pearson and log-scale intervals from \code{epiR::epi.tests()}, Agresti-Caffo
#'   for Youden's index), a Fagan nomogram, narrative panels, and the false-positive and
#'   false-negative cases.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats binom.test
#' @importFrom dplyr %>% mutate case_when
#' @importFrom forcats as_factor
#' @importFrom epiR epi.tests
#' @return An \code{R6} class generator object for the \code{decisionClass} backend; used internally by the jamovi analysis wrapper and not called directly.

decisionClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisionClass",
        inherit = decisionBase,
        private = list(
            .n_complete_cases = NA_integer_,
            .n_level_excluded = 0L,
            # Sign of the observed Youden's index; set in .run() before any narrative is
            # built and read by every .lrBand() call (NULL outside a run = no guard).
            .youdenDirection = NULL,
            # Constants for maintainability
            NOMOGRAM_LABEL_SIZE = 14/5,

            # i18n: after rewording any .() string here, run jmvtools::i18nUpdate() and fill
            # its Turkish msgstr in jamovi/i18n/tr.po (every string had one on 2026-09-27).
            #
            # The 2026-05-14 audit TODO that stood here was re-verified 2026-08-29:
            # its report file is gone and 5 of its 6 items were already false or
            # obsolete. See git history for the retired text.

            # The epiR tables always report the same statistics in the same order, so their rows
            # are known before any data is seen. .init() seeds them; .run() only fills values.
            .epirStatLabels = function() {
                c(se = .("Sensitivity"),
                  sp = .("Specificity"),
                  `pv.pos` = .("Positive predictive value"),
                  `pv.neg` = .("Negative predictive value"),
                  `lr.pos` = .("Positive likelihood ratio"),
                  `lr.neg` = .("Negative likelihood ratio"),
                  `diag.or` = .("Diagnostic odds ratio"),
                  youden = .("Youden's index"),
                  nndx = .("Number needed to diagnose"))
            },
            .epirRatioStats = function() c("se", "sp", "pv.pos", "pv.neg"),
            .epirNumberStats = function() c("lr.pos", "lr.neg", "diag.or", "youden", "nndx"),

            # One likelihood-ratio band for every panel. The Clinical Summary, the
            # Clinical Interpretation panel and the copy-ready report used to band the
            # same number three ways (> 10 in one, >= 10 in two), and all three tested
            # `lr == 1` on a derived double, so a table that is exactly uninformative in
            # rationals (sens 1/3, spec 2/3 -> LR+ 0.99999999999999989) was reported as
            # "evidence AGAINST disease ... level inverted". Bands follow Jaeschke et al.
            # (1994): > 10 / < 0.1 large, 5-10 / 0.1-0.2 moderate, 2-5 / 0.2-0.5 small,
            # 1-2 / 0.5-1 minimal. `tol` keeps a derived value that is a boundary in
            # rationals on the boundary's side.
            #
            # The band is decided on the value AS PRINTED (.fmtLR, the precision of every
            # narrative panel), so "2.00" can never sit beside the band below 2.
            # `direction` is the sign of the observed Youden's index (TP*TN - FP*FN): a
            # ratio computed from the zero-cell corrected table can land on the other side
            # of 1 from the observed data (TP 0, FP 1, FN 5, TN 19 gives LR+ 1.17 while
            # sensitivity is 0), and that is reported as "unstable", not as a direction.
            .lrBand = function(lr, side = c("pos", "neg"), direction = NULL) {
                side <- match.arg(side)
                if (length(lr) != 1 || !is.finite(lr)) return("na")
                lr <- as.numeric(private$.fmtLR(lr))
                tol <- 1e-8
                if (abs(lr - 1) <= tol) return("none")
                if (!is.null(direction) && length(direction) == 1 && is.finite(direction)) {
                    points_up <- if (side == "pos") lr > 1 else lr < 1
                    if ((direction > 0 && !points_up) || (direction < 0 && points_up) || direction == 0)
                        return("unstable")
                }
                if (side == "pos") {
                    if (lr < 1) return("against")
                    if (lr > 10 * (1 + tol)) return("large")
                    if (lr >= 5 * (1 - tol)) return("moderate")
                    if (lr >= 2 * (1 - tol)) return("small")
                    return("minimal")
                }
                if (lr > 1) return("against")
                if (lr < 0.1 * (1 - tol)) return("large")
                if (lr <= 0.2 * (1 + tol)) return("moderate")
                if (lr <= 0.5 * (1 + tol)) return("small")
                "minimal"
            },

            # Every likelihood ratio a panel prints, and the value .lrBand() bands: three
            # significant figures. Two decimals (the previous rule) kept 0.1% relative
            # precision at LR+ 10 but only 5% at LR- 0.1, so LR- 0.0969 printed "0.10" and was
            # banded moderate while its reciprocal, 10.3, was large (independent review
            # 2026-09-25, F1). Same precision as jamovi's default Number format (3 significant
            # figures) and the nomogram. The table cell follows the user's global Number format:
            # jamovi 28.3 reads no dp:/sf: token from a column's `format:`, so none is set.
            # formatC "fg" never switches to scientific notation; "#" keeps trailing zeros
            # ("5.00"); the trailing point it leaves on 3+ integer digits ("126.") goes.
            # decimal.mark: formatC follows getOption("OutDec"), and "0,0969" is NA to
            # as.numeric(), which .lrBand() then fails on.
            # ponytail: reciprocal bands can still differ inside windows up to ~0.45% wide at a
            # boundary (LR+ 10.005-10.05 prints 10.0, moderate; its reciprocal 0.0996 is large).
            # Any fixed precision has such windows; three significant figures keeps them small
            # and equally wide on both sides of 1.
            .fmtLR = function(lr) {
                if (length(lr) != 1 || !is.finite(lr)) return(NA_character_)
                sub("\\.$", "", formatC(signif(lr, 3), digits = 3, format = "fg", flag = "#",
                                         decimal.mark = "."))
            },

            # Every interval here treats each row as a separate patient. Pathology series often
            # hold several cores, blocks or lesions per patient; duplicated rows narrow every
            # interval with nothing on screen to show it. One sentence, used in the About panel
            # and as an always-on note under the counts.
            .independenceText = function() {
                .("Each row must be a different, independent patient. If patients contribute several specimens (cores, blocks, lesions), the confidence intervals are too narrow: analyse one specimen per patient or use a method for clustered data.")
            },

            # "Equivalent AUC" for a binary test is (1 + Youden) / 2. It is printed as the exact
            # image of the Youden's index shown beside it (three decimals), so the two numbers and
            # the discrimination band (.discriminationBand, decided on that Youden) always agree:
            # Youden 0.399 prints AUC 0.6995 with the band "poor", never "0.700". Four decimals
            # at most; a trailing zero is dropped ("0.690").
            .fmtAUC = function(youden) {
                j <- as.numeric(sprintf("%.3f", youden))
                sub("0$", "", sprintf("%.4f", (1 + j) / 2))
            },

            # A user-supplied label (variable name, level) placed in a table note. Two things
            # in jamovi 28.3 bend it, both measured by the review of this fix:
            # - setNote() runs the finished text through translate() again, and an untranslated
            #   string ending in " [...]" is read as a msgctxt and cut there ("Histology [final]"
            #   dropped the rest of the note). Brackets become parentheses, as agreement's
            #   .noteSafe() already does.
            # - the note renderer escapes "&" itself before setting innerHTML, so an entity
            #   would show literally ("&lt;20%"); plain "<20%" renders as written. Only a "<"
            #   that could open a tag (followed by a letter, "/", "!" or "?") is broken apart.
            .noteText = function(x) {
                private$.richText(chartr("[]", "()", as.character(x)))
            },

            # A user-supplied label (variable name, level) placed in a text cell, a column
            # title or a superTitle. These use the same client renderer as notes (bt(): parsed
            # as HTML; em/i/sub/sup kept without attributes, other tags unwrapped, script/style
            # dropped). A "<" before a letter opens a tag, so a level "<LOD" (unclosed tag,
            # discarded at end of input) or "<Negative>" (empty element) rendered as an empty
            # cell. They are not re-translated (Column$setTitle and Cell$asProtoBuf never call
            # translate()), so brackets stay as they are.
            .richText = function(x) {
                gsub("<(?=[A-Za-z/!?])", "< ", as.character(x), perl = TRUE)
            },

            # One discrimination band for the summary word, the interpretation panel and
            # the notices. For a binary test the area under the ROC curve is
            # (sens + spec) / 2 = (1 + Youden) / 2, so the conventional AUC bands of
            # Hosmer, Lemeshow & Sturdivant (2013, section 5.2.4) apply directly:
            # 0.5 none, below 0.7 poor, 0.7 acceptable, 0.8 excellent, 0.9 outstanding.
            # The summary word used to be "moderate" whenever sens OR spec reached 0.70,
            # which called a worse-than-chance test (sens .75, spec .20) "moderate".
            .discriminationBand = function(sens, spec) {
                if (length(sens) != 1 || length(spec) != 1 || is.na(sens) || is.na(spec)) return("na")
                tol <- 1e-8
                # Banded as printed (three decimals, as in the Clinical Interpretation
                # panel and the notices), so a printed 0.400 is never called poor.
                j <- as.numeric(sprintf("%.3f", sens + spec - 1))
                if (j < -tol) return("worse")
                if (abs(j) <= tol) return("none")
                auc <- (1 + j) / 2
                if (auc >= 0.9 - tol) return("outstanding")
                if (auc >= 0.8 - tol) return("excellent")
                if (auc >= 0.7 - tol) return("acceptable")
                "poor"
            },

            # Youden's index = TP/n1 - FP/n0 is a difference between two INDEPENDENT
            # binomial proportions (true-positive rate in the diseased, false-positive rate
            # in the non-diseased). Interval: Agresti & Caffo (2000) - add one success and
            # one failure to each group and take the Wald interval of the adjusted
            # difference, clamped to [-1, 1]; the estimate stays the observed index.
            # Chosen by exact coverage (enumeration over all tables): epiR's sum of the two
            # Clopper-Pearson bounds covers about 99.7% under a 95% label; Newcombe's hybrid
            # score interval (tried first) falls to 0.88 when sensitivity and specificity
            # are both high and the groups small (0.98/0.98, n 20+20) - the region accurate
            # tests occupy; Agresti-Caffo stays at or above 0.935 over sensitivity and
            # specificity 0.50-0.98, n 15-50, mildly conservative when both are high.
            .youdenCI = function(tp, n1, fp, n0, z = stats::qnorm(0.975)) {
                q1 <- (tp + 1) / (n1 + 2)
                q2 <- (fp + 1) / (n0 + 2)
                se <- sqrt(q1 * (1 - q1) / (n1 + 2) + q2 * (1 - q2) / (n0 + 2))
                c(est = tp / n1 - fp / n0,
                  lower = max(-1, q1 - q2 - z * se),
                  upper = min(1, q1 - q2 + z * se))
            },

            # Number needed to diagnose = 1 / Youden's index. 1/J is decreasing on J > 0
            # and undefined at J <= 0, so the interval is the inverse of the Youden
            # interval only while that interval is above 0. epiR took
            # min/max(1/lower, 1/upper), which for a Youden interval crossing 0 printed
            # e.g. 6.67 (-3.08 to 1.73): an interval excluding its own estimate.
            # Returns NA where the quantity is undefined or unbounded.
            # The confidence set for 1/J is {1/j : j in (L, U), j > 0}: (1/U, 1/L) when
            # L > 0, and (1/U, +Inf) when L <= 0 < U - still a valid lower bound even when
            # the point estimate J <= 0 has no inverse.
            .nndxFromYouden = function(y) {
                est <- if (is.finite(y[["est"]]) && y[["est"]] > 0) 1 / y[["est"]] else NA_real_
                lower <- if (is.finite(y[["upper"]]) && y[["upper"]] > 0) 1 / y[["upper"]] else NA_real_
                upper <- if (is.finite(est) && is.finite(y[["lower"]]) && y[["lower"]] > 0) 1 / y[["lower"]] else NA_real_
                c(est = est, lower = lower, upper = upper)
            },

            # Log-scale likelihood-ratio interval (Simel, Samsa & Matchar 1991), the same
            # formula epiR::epi.tests() uses, for the copy-ready report when the CI
            # tables are not requested. a = TP, b = FP, c = FN, d = TN.
            .lrPosCI = function(a, b, c, d, z = stats::qnorm(0.975)) {
                est <- (a / (a + c)) / (b / (b + d))
                se <- sqrt(1 / a - 1 / (a + c) + 1 / b - 1 / (b + d))
                if (!is.finite(est) || !is.finite(se) || est <= 0) return(NULL)
                exp(log(est) + c(-1, 1) * z * se)
            },

            # library-audit 2026-09-16 meddecide [LOW] DONE (same class): rawContingency has a fixed row set, so .init()
            # scaffolds the rows and .run() fills them with setRow()
            # One row for the positive test level, one for the (single or pooled) negative test level and
            # a Total row. The labels are level names: the positive level and an explicit negative level
            # come from the options, so .init() can show them; a negative level inferred from the data is
            # known only in .run(), which passes it in.
            .rawContingencyLabels = function(test_negative_label = self$options$testNegative) {
                level_label <- function(x)
                    if (length(x) > 0 && !is.na(x[1]) && nzchar(x[1])) private$.richText(x[1]) else ""
                c(test_pos = level_label(self$options$testPositive),
                  test_neg = level_label(test_negative_label),
                  total = .("Total"))
            },

            .init = function() {
                labels <- private$.epirStatLabels()
                for (key in private$.epirRatioStats())
                    self$results$epirTable_ratio$addRow(rowKey = key,
                        values = list(statsnames = unname(labels[[key]])))
                for (key in private$.epirNumberStats())
                    self$results$epirTable_number$addRow(rowKey = key,
                        values = list(statsnames = unname(labels[[key]])))

                raw_labels <- private$.rawContingencyLabels()
                for (key in names(raw_labels))
                    self$results$rawContingency$addRow(rowKey = key,
                        values = list(test_level = unname(raw_labels[[key]])))

                cTable <- self$results$cTable
                cTable$addRow(rowKey = "Test Positive", values = list(newtest = .("Test Positive")))
                cTable$addRow(rowKey = "Test Negative", values = list(newtest = .("Test Negative")))
                cTable$addRow(rowKey = "Total", values = list(newtest = .("Total")))

                summary_table <- self$results$confusionMatrixSummary
                summary_table$addRow(rowKey = 1, values = list(classification = .("True Positive")))
                summary_table$addRow(rowKey = 2, values = list(classification = .("False Positive")))
                summary_table$addRow(rowKey = 3, values = list(classification = .("False Negative")))
                summary_table$addRow(rowKey = 4, values = list(classification = .("True Negative")))

                # Populate welcome message
                self$results$welcome$setContent(paste0(
                    "<div style='padding: 20px; background-color: rgba(74, 144, 226, 0.08); border-radius: 8px; border-left: 4px solid #4A90E2; color: inherit;'>",
                    "<h3 style='margin-top: 0; color: inherit;'>", .("Medical Decision Analysis"), "</h3>",
                    "<p style='font-size: 15px; color: inherit;'>", .("Evaluate diagnostic test performance with sensitivity, specificity, and predictive values."), "</p>",

                    "<h4 style='color: inherit; margin-top: 20px;'>", .("Quick Start"), ":</h4>",
                    "<ol style='font-size: 14px; color: inherit; line-height: 1.8;'>",
                    "<li><strong>", .("Select Reference Standard (Gold Standard)"), ":</strong> ", .("Choose the reference-standard variable that defines disease status in this analysis (e.g., biopsy result, final diagnosis)"), "</li>",
                    "<li><strong>", .("Select Disease present level"), ":</strong> ", .("Choose which level indicates disease is present"), "</li>",
                    "<li><strong>", .("Select Test Under Evaluation"), ":</strong> ", .("Choose the diagnostic test you want to evaluate"), "</li>",
                    "<li><strong>", .("Select Test positive level"), ":</strong> ", .("Choose which level represents a positive test result"), "</li>",
                    "</ol>",

                    "<div style='background-color: rgba(255, 255, 255, 0.06); padding: 15px; border-radius: 5px; margin-top: 15px; color: inherit;'>",
                    "<h4 style='margin-top: 0; color: inherit;'>", .("What You'll Get"), ":</h4>",
                    "<ul style='font-size: 13px; color: inherit; line-height: 1.6;'>",
                    "<li><strong>", .("Sensitivity"), " &amp; ", .("Specificity"), ":</strong> ", .("How well the test identifies disease presence and absence"), "</li>",
                    "<li><strong>", .("Predictive Values"), ":</strong> ", .("Probability of disease after a positive result (PPV) and of no disease after a negative result (NPV); both depend on prevalence"), "</li>",
                    "<li><strong>", .("Likelihood Ratios"), ":</strong> ", .("How much test results change disease probability"), "</li>",
                    "<li><strong>", .("Confidence Intervals"), ":</strong> ", .("95% intervals for sensitivity, specificity, predictive values at the sample prevalence, likelihood ratios, diagnostic odds ratio and Youden's index (tick 95% confidence intervals)"), "</li>",
                    "<li><strong>", .("Fagan Nomogram"), ":</strong> ", .("Visual representation of probability changes"), "</li>",
                    "<li><strong>", .("Misclassification Analysis"), ":</strong> ", .("Detailed examination of false positives and false negatives"), "</li>",
                    "</ul>",
                    "</div>",
                    "</div>"
                ))

                # No setVisible() here. `welcome` carries a declarative visible:
                # expression in jamovi/decision.r.yaml, and an imperative call on every
                # run silently overrode it -- editing the yaml expression had no effect,
                # and the two spellings had already drifted apart. One mechanism only:
                # the yaml binding.
            },

            # Initialize notice collection list
            .noticeList = list(),

            # HTML sanitization for security
            .safeHtmlOutput = function(text) {
                if (is.null(text) || length(text) == 0) return("")
                text <- as.character(text)
                # Sanitize potentially dangerous characters
                text <- gsub("&", "&amp;", text, fixed = TRUE)
                text <- gsub("<", "&lt;", text, fixed = TRUE)
                text <- gsub(">", "&gt;", text, fixed = TRUE)
                text <- gsub("\"", "&quot;", text, fixed = TRUE)
                text <- gsub("'", "&#x27;", text, fixed = TRUE)
                text <- gsub("/", "&#x2F;", text, fixed = TRUE)
                return(text)
            },

            # Add a notice to the collection
            # `refs`: the 00refs keys the notice's own text relies on. The notices item is
            # always visible, so .renderNotices() lists only the sources of the notices shown.
            .addNotice = function(type, title, content, refs = character(0)) {
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content,
                    refs = refs
                )
            },

            # Render collected notices as HTML
            .renderNotices = function() {
                # The references in the yaml would otherwise be listed on every run, beside
                # notices that are not shown (a user copying the analysis references would
                # cite Haldane-Anscombe with no zero cell). setRefs() is sent with each run's
                # results and never restored from a saved one, so this is what jamovi lists.
                self$results$notices$setRefs(unique(unlist(lapply(private$.noticeList, `[[`, "refs"))))
                if (length(private$.noticeList) == 0) {
                    # Clear, do not just return: jamovi keeps the previous content, so a
                    # warning from an earlier run would sit beside numbers that no longer
                    # support it. Matches decisioncalculator.b.R:32.
                    self$results$notices$setContent("")
                    return()
                }

                # Map notice types to a tint, a border colour and a severity word
                typeStyles <- list(
                    # Translucent rgba tints, not opaque pastels: they composite over
                    # either jamovi theme instead of painting a white block into the
                    # dark one. Same palette as the reference implementation in
                    # waterfall.b.R.
                    #
                    # `label` carries the severity in WORDS. The `color` field and the
                    # coloured <strong> wrapper were removed (rightly -- an opaque hex is
                    # not theme-safe) but nothing replaced the signal, leaving an ERROR
                    # that explains why every table is empty looking like an INFO to any
                    # reader, and identical to a STRONG_WARNING for a colour-blind one.
                    # A word is theme-safe, translatable and legible at a glance.
                    ERROR = list(bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5", label = .("Error")),
                    STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74", label = .("Important warning")),
                    WARNING = list(bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047", label = .("Warning")),
                    INFO = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd", label = .("Note"))
                )

                html <- "<div style='margin: 10px 0;'>"

                # Most severe first. Notices are raised in code order, so the worse-than-chance
                # ERROR (.validateDiscrimination, late in .run) sat below every missing-data and
                # sample-size note. order() is stable: same-severity notices keep their order.
                severity <- c(ERROR = 1, STRONG_WARNING = 2, WARNING = 3, INFO = 4)
                rank <- severity[vapply(private$.noticeList, `[[`, "", "type")]
                for (notice in private$.noticeList[order(rank)]) {
                    style <- typeStyles[[notice$type]] %||% typeStyles$INFO

                    html <- paste0(html,
                        "<div style='background-color: ", style$bgcolor, "; ",
                        "border-left: 4px solid ", style$border, "; ",
                        "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                        "<strong>",
                        # No leading space: `style$icon, " "` with an empty icon emitted
                        # one inside every <strong>.
                        private$.safeHtmlOutput(style$label), ": ",
                        private$.safeHtmlOutput(notice$title), "</strong><br>",
                        "<span style='color: inherit;'>", private$.safeHtmlOutput(notice$content), "</span>",
                        "</div>"
                    )
                }

                html <- paste0(html, "</div>")

                self$results$notices$setContent(html)
            },

            # Enhanced input validation for categorical diagnostic data
            .validateCategoricalInputs = function() {
                # No "variable not selected" branches here: .run() returns before
                # calling this method unless gold, newtest, goldPositive and
                # testPositive are all set (each is a scalar option, so its length
                # check summing to 4 means all four are present). The unselected
                # state is handled by the `welcome` panel instead.

                # Check data availability
                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("No data available for analysis"),
                        content = .("Please ensure your data is loaded. Check that your dataset contains observations.")
                    )
                    return(FALSE)
                }

                # Validate data has enough cases
                if (nrow(self$data) < 4) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.("Insufficient data: {n} cases found"), n = nrow(self$data)),
                        content = .("At least 4 cases are required for diagnostic test analysis.")
                    )
                    return(FALSE)
                }

                # No pprob range check: min 0.001 / max 0.999 in decision.a.yaml are enforced by
                # the option check before .run(), in jamovi and through the R wrapper alike.

                # Warn about CI interpretation when using population prevalence
                if (self$options$pp && self$options$ci) {
                    private$.addNotice(
                        type = "WARNING",
                        refs = c("AltmanBland1994b"),
                        title = .("Confidence Intervals Interpretation"),
                        content = .("The 95% confidence intervals are calculated from your study sample. They do NOT apply to the PPV and NPV in the main table, which are recomputed by Bayes' theorem at the population prevalence you supplied.")
                    )
                }

                # Validate that selected levels actually exist in the data.
                # The option value IS the column name. A constructFormula() ->
                # decomposeFormula() round-trip read "\" as an escape, so a column named
                # "IHC\score" became "IHCscore", self$data[[...]] was NULL, and the run
                # failed with a false "level not found" error.
                goldVar <- self$options$gold
                testVar <- self$options$newtest

                # Get actual levels from data
                gold_levels <- if (is.factor(self$data[[goldVar]])) {
                    levels(self$data[[goldVar]])
                } else {
                    sort(unique(as.character(self$data[[goldVar]])))
                }

                test_levels <- if (is.factor(self$data[[testVar]])) {
                    levels(self$data[[testVar]])
                } else {
                    sort(unique(as.character(self$data[[testVar]])))
                }

                # Validate gold standard positive level
                if (!(self$options$goldPositive %in% gold_levels)) {
                    available_levels <- if (length(gold_levels) <= 10) {
                        paste(gold_levels, collapse = ", ")
                    } else {
                        paste(c(gold_levels[1:10], "..."), collapse = ", ")
                    }
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.('Disease-present level "{lvl}" not found in gold standard variable'), lvl = self$options$goldPositive),
                        content = .fmt(.('Available levels: {lvls}. Check for typos or select the correct level from the dropdown.'), lvls = available_levels)
                    )
                    return(FALSE)
                }

                # Validate test positive level
                if (!(self$options$testPositive %in% test_levels)) {
                    available_levels <- if (length(test_levels) <= 10) {
                        paste(test_levels, collapse = ", ")
                    } else {
                        paste(c(test_levels[1:10], "..."), collapse = ", ")
                    }
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.('Test-positive level "{lvl}" not found in test variable'), lvl = self$options$testPositive),
                        content = .fmt(.('Available levels: {lvls}. Check for typos or select the correct level from the dropdown.'), lvls = available_levels)
                    )
                    return(FALSE)
                }

                # Validate gold standard negative level if specified
                if (length(self$options$goldNegative) > 0 && nchar(self$options$goldNegative) > 0) {
                    if (!(self$options$goldNegative %in% gold_levels)) {
                        available_levels <- if (length(gold_levels) <= 10) {
                            paste(gold_levels, collapse = ", ")
                        } else {
                            paste(c(gold_levels[1:10], "..."), collapse = ", ")
                        }
                        private$.addNotice(
                            type = "ERROR",
                            title = .fmt(.('Disease-absent level "{lvl}" not found in gold standard variable'), lvl = self$options$goldNegative),
                            content = .fmt(.('Available levels: {lvls}. Check for typos or select the correct level from the dropdown.'), lvls = available_levels)
                        )
                        return(FALSE)
                    }

                    # Check that positive and negative levels are different
                    if (self$options$goldNegative == self$options$goldPositive) {
                        private$.addNotice(
                            type = "ERROR",
                            title = .("Disease-present and disease-absent levels cannot be the same"),
                            content = .("Select different levels for positive and negative outcomes.")
                        )
                        return(FALSE)
                    }
                }

                # Validate test negative level if specified
                if (length(self$options$testNegative) > 0 && nchar(self$options$testNegative) > 0) {
                    if (!(self$options$testNegative %in% test_levels)) {
                        available_levels <- if (length(test_levels) <= 10) {
                            paste(test_levels, collapse = ", ")
                        } else {
                            paste(c(test_levels[1:10], "..."), collapse = ", ")
                        }
                        private$.addNotice(
                            type = "ERROR",
                            title = .fmt(.('Test-negative level "{lvl}" not found in test variable'), lvl = self$options$testNegative),
                            content = .fmt(.('Available levels: {lvls}. Check for typos or select the correct level from the dropdown.'), lvls = available_levels)
                        )
                        return(FALSE)
                    }

                    # Check that positive and negative levels are different
                    if (self$options$testNegative == self$options$testPositive) {
                        private$.addNotice(
                            type = "ERROR",
                            title = .("Test-positive and test-negative levels cannot be the same"),
                            content = .("Select different levels for positive and negative outcomes.")
                        )
                        return(FALSE)
                    }
                }

                return(TRUE)
            },

            # Prepare analysis data with efficient processing
            .prepareAnalysisData = function() {
                # The option values are the column names (see .validateCategoricalInputs).
                testVar <- self$options$newtest
                goldVar <- self$options$gold

                vars_needed <- unique(c(testVar, goldVar))
                if (length(vars_needed) < 2) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("The gold standard and the test are the same variable"),
                        content = .("Select a different variable for the test under evaluation. Comparing a variable against itself always yields perfect agreement and tells you nothing about the test.")
                    )
                    return(NULL)
                }

                # Restrict case removal to variables used in the diagnostic table
                # Track original row indices BEFORE filtering
                subset_data <- self$data[, vars_needed, drop = FALSE]
                # Spreadsheet row identity, not a position in the filtered frame: the
                # FP/FN tables render this under a column titled "Row", and with a jamovi
                # row filter active a positional index points at the wrong patient.
                # rownames(self$data) carries the original row numbers through the filter.
                subset_data$original_row_position <- seq_len(nrow(subset_data))
                subset_data$original_row_index <- suppressWarnings(as.integer(rownames(self$data)))
                if (anyNA(subset_data$original_row_index))
                    subset_data$original_row_index <- seq_len(nrow(subset_data))

                mydata <- jmvcore::naOmit(subset_data)
                # Rows are dropped three times: here for missingness, below for an
                # explicit addNA() "missing" level, and later for levels that are neither
                # the positive nor the negative one. .n_complete_cases is the baseline
                # that .n_level_excluded is measured against, so it must be captured
                # AFTER the explicit-NA drop (see below) -- capturing it here charged
                # explicit-missing rows to level exclusion, and the Data Quality Summary
                # then told the pathologist those cases had an unselected level while a
                # WARNING notice on the same screen said they were explicitly missing.
                private$.n_complete_cases <- nrow(mydata)

                if (nrow(mydata) < nrow(self$data)) {
                    removed <- nrow(self$data) - nrow(mydata)
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Removed {n} row(s) with missing diagnostic data"), n = removed),
                        content = .fmt(.("{used} of {total} cases have both a test and a gold-standard value; any further exclusions are listed below. Consider investigating patterns of missingness."),
                                                  used = nrow(mydata), total = nrow(self$data))
                    )
                }
                
                # Convert to factors and recode in single pipeline
                # droplevels(): a level emptied by the missing-value removal above stayed in
                # levels(), was then inferred as the negative level, and the run was refused
                # as "not dichotomous" although both variables are binary. (An explicit-NA
                # level with members survives droplevels() and is handled just below.)
                mydata[[testVar]] <- droplevels(forcats::as_factor(mydata[[testVar]]))
                mydata[[goldVar]] <- droplevels(forcats::as_factor(mydata[[goldVar]]))

                # An explicit NA level (addNA()) survives naOmit and is.na(), but means
                # "status unknown" -- scoring it as disease-absent silently biases every
                # metric. Drop those rows and disclose it.
                explicit_na <- is.na(as.character(mydata[[goldVar]])) | is.na(as.character(mydata[[testVar]]))
                if (any(explicit_na)) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Removed {n} case(s) with an explicit missing level"), n = sprintf("%d", sum(explicit_na))),
                        content = .("These cases carry a level that codes missingness rather than a result. They are excluded rather than counted as negative, which would have biased sensitivity and specificity.")
                    )
                    mydata <- mydata[!explicit_na, , drop = FALSE]
                    mydata[[goldVar]] <- droplevels(mydata[[goldVar]])
                    mydata[[testVar]] <- droplevels(mydata[[testVar]])
                    # Re-baseline: these rows are missingness, not level exclusion.
                    private$.n_complete_cases <- nrow(mydata)
                }

                # Get actual levels for validation
                gold_actual_levels <- levels(mydata[[goldVar]])
                test_actual_levels <- levels(mydata[[testVar]])

                # Determine negative levels (explicit or implicit)
                has_gold_negative <- length(self$options$goldNegative) > 0 && nchar(self$options$goldNegative) > 0
                has_test_negative <- length(self$options$testNegative) > 0 && nchar(self$options$testNegative) > 0

                # Exactly two levels take part: the positive one and the negative one.
                # Everything else is EXCLUDED, never pooled into the negative arm.
                #
                # Pooling was the old default and it silently changed the estimates: an
                # "Equivocal" level counted as disease-absent enlarges the specificity
                # denominator with cases the test was never right about. On a 50/40/10
                # gold standard that moved specificity from 0.875 to 0.780 with nothing
                # on screen to say why. An indeterminate result is not a negative result.
                #
                # The level is inferred only when the variable is dichotomous, where there
                # is exactly one candidate and therefore no choice to get wrong. With three
                # or more levels the user must name it.
                #
                # NA_character_ rather than NULL as the unset sentinel: case_when()
                # evaluates every branch, and `x == NULL` yields logical(0), which fails
                # the size check even on an unreachable branch.
                gold_candidates <- setdiff(gold_actual_levels, self$options$goldPositive)
                test_candidates <- setdiff(test_actual_levels, self$options$testPositive)

                gold_negative_level <- if (has_gold_negative) self$options$goldNegative
                                       else if (length(gold_candidates) == 1) gold_candidates[[1]]
                                       else NA_character_
                test_negative_level <- if (has_test_negative) self$options$testNegative
                                       else if (length(test_candidates) == 1) test_candidates[[1]]
                                       else NA_character_

                # Zero candidates and several candidates are different failures. With zero
                # (every other level emptied by the exclusions above) the old message still
                # said the variable had "more than one level besides" the positive one and
                # listed "()".
                if (is.na(gold_negative_level)) {
                    if (length(gold_candidates) == 0)
                        private$.addNotice(
                            type = "ERROR",
                            title = .("The gold standard has only one level among the analysed cases"),
                            content = .fmt(.('Every analysed case has the gold-standard level "{pos}", so there are no disease-absent cases to estimate specificity from. Check the data, any exclusions reported in the warnings, and the level selection.'),
                                           pos = self$options$goldPositive)
                        )
                    else
                        private$.addNotice(
                            type = "ERROR",
                            title = .("Choose which gold-standard level means disease absent"),
                            content = .fmt(.('The gold standard has more than one level besides "{pos}" ({lvls}), so the disease-absent level cannot be inferred. Select it under Disease absent level. Levels you do not name are excluded from the analysis rather than counted as disease-absent.'),
                                           pos = self$options$goldPositive,
                                           lvls = paste(gold_candidates, collapse = ", "))
                        )
                    return(NULL)
                }
                if (is.na(test_negative_level)) {
                    if (length(test_candidates) == 0)
                        private$.addNotice(
                            type = "ERROR",
                            title = .("The test has only one level among the analysed cases"),
                            content = .fmt(.('Every analysed case has the test level "{pos}", so there are no negative test results to compare. Check the data, any exclusions reported in the warnings, and the level selection.'),
                                           pos = self$options$testPositive)
                        )
                    else
                        private$.addNotice(
                            type = "ERROR",
                            title = .("Choose which test level means a negative result"),
                            content = .fmt(.('The test has more than one level besides "{pos}" ({lvls}), so the test-negative level cannot be inferred. Select it under Test negative level. Levels you do not name are excluded from the analysis rather than counted as negative.'),
                                           pos = self$options$testPositive,
                                           lvls = paste(test_candidates, collapse = ", "))
                        )
                    return(NULL)
                }

                # Check for levels that will be excluded (not positive, not negative)
                gold_used_levels <- c(self$options$goldPositive, gold_negative_level)
                test_used_levels <- c(self$options$testPositive, test_negative_level)

                gold_excluded <- setdiff(gold_actual_levels, gold_used_levels)
                test_excluded <- setdiff(test_actual_levels, test_used_levels)

                if (length(gold_excluded) > 0) {
                    excluded_str <- if (length(gold_excluded) <= 5) {
                        paste(gold_excluded, collapse = ", ")
                    } else {
                        paste(c(gold_excluded[1:5], "..."), collapse = ", ")
                    }
                    private$.addNotice(
                        type = "WARNING",
                        refs = c("STARD2015"),
                        title = .fmt(.('Gold standard levels excluded from analysis: {lvls}'), lvls = excluded_str),
                        content = .fmt(.('Only "{pos}" (disease-present) and "{neg}" (disease-absent) take part. Cases at any other level are removed, not counted as disease-absent. Removing them can bias the estimates. Report how many were removed (STARD 2015).'), pos = self$options$goldPositive, neg = gold_negative_level)
                    )
                }

                if (length(test_excluded) > 0) {
                    excluded_str <- if (length(test_excluded) <= 5) {
                        paste(test_excluded, collapse = ", ")
                    } else {
                        paste(c(test_excluded[1:5], "..."), collapse = ", ")
                    }
                    private$.addNotice(
                        type = "WARNING",
                        refs = c("Schuetz2012"),
                        title = .fmt(.('Test variable levels excluded from analysis: {lvls}'), lvls = excluded_str),
                        content = .fmt(.('Only "{pos}" (test-positive) and "{neg}" (test-negative) take part. Cases at any other level are removed, not counted as negative. If these are inconclusive results, removing them can make the test look more accurate than it is (Schuetz et al. 2012). Report how many were removed; the Data Quality Summary under Raw data tables shows the count.'), pos = self$options$testPositive, neg = test_negative_level)
                    )
                }

                # Efficient recoding with explicit negative level handling
                mydata <- mydata %>%
                    dplyr::mutate(
                        testVariable2 = dplyr::case_when(
                            is.na(.data[[testVar]]) ~ NA_character_,
                            .data[[testVar]] == self$options$testPositive ~ "Positive",
                            .data[[testVar]] == test_negative_level ~ "Negative",
                            TRUE ~ NA_character_  # Explicit negative specified, others become NA (filtered)
                        ),
                        goldVariable2 = dplyr::case_when(
                            is.na(.data[[goldVar]]) ~ NA_character_,
                            .data[[goldVar]] == self$options$goldPositive ~ "Positive",
                            .data[[goldVar]] == gold_negative_level ~ "Negative",
                            TRUE ~ NA_character_  # Explicit negative specified, others become NA (filtered)
                        )
                    ) %>%
                    dplyr::mutate(
                        # intersect(), not fct_relevel(): a cohort filtered down to a
                        # single disease category has no "Positive" level, and
                        # fct_relevel() warns about it straight into jamovi's Analysis
                        # Notes as raw package chatter -- on exactly the run that
                        # otherwise shows nothing. Same ordering and same one-level
                        # result; the structural 2x2 check below still raises the
                        # real error.
                        testVariable2 = factor(testVariable2,
                                               levels = intersect(c("Positive", "Negative"), testVariable2)),
                        goldVariable2 = factor(goldVariable2,
                                               levels = intersect(c("Positive", "Negative"), goldVariable2))
                    )

                # Remove rows with NA in recoded variables (excluded levels when explicit negative specified)
                mydata <- mydata %>% dplyr::filter(!is.na(testVariable2), !is.na(goldVariable2))
                # The recoded levels were fixed BEFORE this joint filter, so dropping the other
                # variable's unselected levels could empty a margin and still leave a 2x2 table
                # with a zero row or column: gold N,N,N,N,P against test P,N,P,N,E loses its only
                # disease-present case with E, and the run showed a blank sensitivity beside a
                # specificity (no positive test at all printed LR+ = LR- = 1.00 from the corrected
                # table). Dropping the emptied level sends that case to the check below.
                mydata$testVariable2 <- droplevels(mydata$testVariable2)
                mydata$goldVariable2 <- droplevels(mydata$goldVariable2)
                private$.n_level_excluded <- max(0, private$.n_complete_cases - nrow(mydata))
                
                # Validate contingency table structure after data preparation
                test_table <- table(mydata$testVariable2, mydata$goldVariable2)
                
                # Check for empty cells that would cause problems
                if (any(dim(test_table) != c(2, 2))) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .("No analysed cases at one of the positive or negative levels"),
                        content = .("After missing values and unselected levels are removed, the test or the gold standard has no cases at its positive or its negative level, so a 2x2 table cannot be formed. Check the data, any exclusions reported in the warnings, and the level selection.")
                    )
                    return(NULL)
                }

                # The "at least 4 cases" rule in .validateCategoricalInputs() counts raw
                # rows, before missing values and unselected levels are removed, so a
                # 6-row sheet with 3 Equivocal rows reached the tables with n = 3.
                if (nrow(mydata) < 4) {
                    private$.addNotice(
                        type = "ERROR",
                        title = .fmt(.("Insufficient data after exclusions: {n} cases analysed"),
                                     n = sprintf("%d", nrow(mydata))),
                        content = .("At least 4 cases are required for diagnostic test analysis once missing values and unselected levels have been removed.")
                    )
                    return(NULL)
                }

                # A zero CELL. A zero margin (no case with some test result or reference
                # result) already returned above, so every proportion is defined; the notice
                # used to describe blank predictive values, which that check now prevents.
                if (any(test_table == 0)) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = .("Zero counts detected in contingency table"),
                        content = .("A zero count makes a proportion exactly 0% or 100%. Read the 95% confidence intervals (95% confidence intervals option), not the point estimate. For the likelihood ratios, see Continuity correction applied.")
                    )
                }
                
                return(list(data = mydata, testVar = testVar, goldVar = goldVar))
            },

            # Enhanced diagnostic accuracy interpretation helper
            .getDiagnosticInterpretation = function(lr_pos, lr_neg, sens, spec) {
                # Likelihood ratio interpretations: Jaeschke et al. (1994) bands, via the
                # one classifier every panel shares (.lrBand).
                lr_pos_interp <- switch(private$.lrBand(lr_pos, "pos", private$.youdenDirection),
                    na = .("Positive likelihood ratio unavailable due to data limitations"),
                    unstable = .("No reliable direction: this likelihood ratio rests on a zero count, and the continuity correction puts it on the other side of 1 from the observed data"),
                    large = .("Large and often conclusive increase in probability of disease"),
                    moderate = .("Moderate increase in probability of disease"),
                    small = .("Small but potentially important increase in probability"),
                    minimal = .("Minimal increase in probability of disease"),
                    none = .("Uninformative: a positive result leaves the probability of disease unchanged"),
                    against = .("A positive result decreases the probability of disease: the level chosen as test-positive may be inverted"))

                lr_neg_interp <- switch(private$.lrBand(lr_neg, "neg", private$.youdenDirection),
                    na = .("Negative likelihood ratio unavailable due to data limitations"),
                    unstable = .("No reliable direction: this likelihood ratio rests on a zero count, and the continuity correction puts it on the other side of 1 from the observed data"),
                    large = .("Large and often conclusive decrease in probability of disease"),
                    moderate = .("Moderate decrease in probability of disease"),
                    small = .("Small but potentially important decrease in probability"),
                    minimal = .("Minimal decrease in probability of disease"),
                    none = .("Uninformative: a negative result leaves the probability of disease unchanged"),
                    against = .("A negative result increases the probability of disease: the level chosen as test-positive may be inverted"))

                # Discrimination: Hosmer, Lemeshow & Sturdivant (2013) AUC bands, applied
                # through AUC = (1 + Youden) / 2. These bands used to be Youden cut-offs
                # 0.8 / 0.6 / 0.4 named Excellent / Good / Fair / Poor with no source.
                youden_index <- if (is.na(sens) || is.na(spec)) NA_real_ else sens + spec - 1
                test_utility <- switch(private$.discriminationBand(sens, spec),
                    na = .("Insufficient data to evaluate discriminatory power"),
                    # Half-open ranges: "0.80 to 0.89" left 0.795 outside every label.
                    outstanding = .("Outstanding discrimination (equivalent AUC 0.90 or above; Youden's index 0.80 or above)"),
                    excellent = .("Excellent discrimination (equivalent AUC 0.80 to below 0.90; Youden's index 0.60 to below 0.80)"),
                    acceptable = .("Acceptable discrimination (equivalent AUC 0.70 to below 0.80; Youden's index 0.40 to below 0.60)"),
                    poor = .("Poor discrimination (equivalent AUC above 0.50 and below 0.70; Youden's index above 0 and below 0.40)"),
                    none = .("No discrimination (Youden's index 0, equivalent AUC 0.50)"),
                    worse = .("Worse than chance (Youden's index below 0): check the level chosen as test-positive"))

                return(list(
                    lr_pos_interp = lr_pos_interp,
                    lr_neg_interp = lr_neg_interp,
                    youden_index = youden_index,
                    test_utility = test_utility
                ))
            },
            
            # Basic missing data summary
            .analyzeMissingData = function(original_data, processed_data) {
                n_orig <- nrow(original_data)
                dropped <- n_orig - nrow(processed_data)
                if (dropped <= 0) {
                    return(.("No cases excluded."))
                }

                level_excluded <- private$.n_level_excluded
                if (!is.numeric(level_excluded) || length(level_excluded) != 1 || is.na(level_excluded))
                    level_excluded <- 0
                level_excluded <- min(max(level_excluded, 0), dropped)
                missing_count <- dropped - level_excluded

                pct <- function(k) round((k / n_orig) * 100, 1)
                parts <- character(0)
                if (missing_count > 0)
                    parts <- c(parts, .fmt(.("{n} case(s) ({pct}) removed for missing values on the test or gold-standard variable"),
                                           n = sprintf("%d", missing_count),
                                           pct = paste0(base::format(pct(missing_count)), "%")))
                if (level_excluded > 0)
                    parts <- c(parts, .fmt(.("{n} case(s) ({pct}) removed because their level was neither the positive nor the negative level you selected (these are NOT missing values)"),
                                           n = sprintf("%d", level_excluded),
                                           pct = paste0(base::format(pct(level_excluded)), "%")))

                .fmt(
                    .("Case exclusion summary: {details}. {analysed} of {total} cases analysed."),
                    details = paste(parts, collapse = "; "),
                    analysed = nrow(processed_data),
                    total = n_orig)
            },
            
            # Generate natural language summary for clinical use
            .generateNaturalLanguageSummary = function(sens, spec, ppv, npv, lr_pos, lr_neg,
                                                      prevalence, total_pop, test_name, gold_name) {
                format_percent <- function(value, fallback_label) {
                    if (is.na(value)) return(fallback_label)
                    sprintf("%.1f%%", value * 100)
                }

                # Get clinical benchmarks for enhanced interpretation
                benchmarks <- private$.addClinicalBenchmarks(sens, spec, lr_pos, lr_neg)

                # Test quality word: the same discrimination band as the Clinical
                # Interpretation panel and the notices (.discriminationBand). It used to be
                # "moderate" whenever sens OR spec reached 0.70, so a test that labels
                # nearly everyone positive - worse than chance - read as "moderate".
                test_quality <- switch(private$.discriminationBand(sens, spec),
                    outstanding = .("outstanding"),
                    excellent = .("excellent"),
                    acceptable = .("acceptable"),
                    poor = .("poor"),
                    # Not the bare word "no": jjdotplotstats uses that msgid for another role, and
                    # one Turkish word cannot serve both (the Turkish "zero" leaked into its sentence).
                    none = .("chance-level"),
                    worse = .("worse-than-chance"),
                    na = .("undetermined"))

                # Describe the discrimination profile (no use recommendation is made)
                primary_utility <- dplyr::case_when(
                    is.na(sens) || is.na(spec) ~ .("sensitivity or specificity could not be computed in this sample, so the discrimination profile is incomplete"),
                    sens >= 0.9 && spec < 0.8 ~ .("sensitivity is high (0.90 or above) while specificity is below 0.80 in this sample"),
                    spec >= 0.9 && sens < 0.8 ~ .("specificity is high (0.90 or above) while sensitivity is below 0.80 in this sample"),
                    sens >= 0.8 && spec >= 0.8 ~ .("sensitivity and specificity are both 0.80 or above in this sample"),
                    TRUE ~ .("sensitivity and specificity were not both 0.80 or above in this sample")
                )

                prevalence_text <- format_percent(prevalence, .("not reported"))
                if (isTRUE(self$options$pp))
                    prevalence_text <- paste(prevalence_text, .("(the population prevalence you supplied)"))
                sens_text <- format_percent(sens, .("not calculated"))
                spec_text <- format_percent(spec, .("not calculated"))
                ppv_text <- format_percent(ppv, .("not calculated"))
                npv_text <- format_percent(npv, .("not calculated"))
                # Only the complement of NPV adds anything: the post-test probability of
                # disease after a NEGATIVE result. The other two locals were PPV and NPV
                # under different names.
                post_neg_disease_text <- format_percent(1 - npv, .("not available"))

                sample_text <- if (!is.na(total_pop)) .fmt(.("{n} cases analyzed"), n = total_pop) else .("Sample size not available")

                summary_template <- .("<div style='margin: 15px; padding: 15px; border-left: 5px solid #4CAF50; background-color: rgba(114, 184, 33, 0.1); color: inherit;'><h3 style='color: inherit; margin-top: 0;'>Clinical Summary</h3><p style='font-size: 16px;'><strong>Analysis:</strong> Diagnostic test performance evaluation comparing {testname} against the reference standard {goldname}.</p><p><strong>Sample:</strong> {sample}. Predictive values below are computed at a disease prevalence of {prev}.</p><p><strong>Test Performance:</strong> By the conventional rule of thumb for the area under the ROC curve (Hosmer, Lemeshow and Sturdivant 2013), the test shows <strong>{quality}</strong> discrimination (equivalent AUC {auc}), with sensitivity of <strong>{sens}</strong> (<em>{sensnote}</em>) and specificity of <strong>{spec}</strong> (<em>{specnote}</em>).</p><p><strong>Discrimination Profile:</strong> {profile}.</p><p><strong>Likelihood Ratios:</strong> Positive LR: {lrpos} (<em>{lrposnote}</em>), Negative LR: {lrneg} (<em>{lrnegnote}</em>). Likelihood-ratio bands are a rule of thumb (Jaeschke, Guyatt and Sackett 1994).</p><p><strong>Key Findings:</strong> Predictive values are post-test probabilities. After a positive result the probability of disease is the positive predictive value (PPV {ppv}). After a negative result the probability of disease is <strong>{postneg}</strong>, and the probability of being disease-free is the negative predictive value (NPV {npv}).</p></div>")

                # Only the NA arm can fire: a zero cell triggers the Haldane-Anscombe 0.5
                # correction before the LRs are formed, so neither LR is ever Inf here,
                # and both are NA exactly when sensitivity or specificity is NA.
                lr_pos_safe <- if (is.na(lr_pos)) .("not calculated (needs both sensitivity and specificity)")
                    else private$.fmtLR(lr_pos)
                lr_neg_safe <- if (is.na(lr_neg)) .("not calculated (needs both sensitivity and specificity)")
                    else private$.fmtLR(lr_neg)

                # Escape user-derived variable names before HTML interpolation
                test_name_safe <- private$.safeHtmlOutput(test_name)
                gold_name_safe <- private$.safeHtmlOutput(gold_name)

                summary <- .fmt(
                    summary_template,
                    testname = test_name_safe, goldname = gold_name_safe,
                    sample = sample_text, prev = prevalence_text,
                    quality = test_quality,
                    auc = if (is.na(sens) || is.na(spec)) .("not available") else private$.fmtAUC(sens + spec - 1),
                    sens = sens_text, sensnote = benchmarks$sens_quality,
                    spec = spec_text, specnote = benchmarks$spec_quality,
                    profile = primary_utility,
                    lrpos = lr_pos_safe, lrposnote = benchmarks$lr_pos_interpretation,
                    lrneg = lr_neg_safe, lrnegnote = benchmarks$lr_neg_interpretation,
                    ppv = ppv_text, postneg = post_neg_disease_text, npv = npv_text
                )

                return(summary)
            },
            
            # Generate copy-ready report template
            .generateReportTemplate = function(sens, spec, ppv, npv, lr_pos, lr_neg,
                                             sens_ci = NULL, spec_ci = NULL, test_name, gold_name,
                                             prevalence = NA_real_, n_total = NA_real_,
                                             n_diseased = NA_real_, n_healthy = NA_real_,
                                             ppv_ci = NULL, npv_ci = NULL, lr_pos_ci = NULL) {
                # Create confidence interval text if available
                ci_text <- if (!is.null(sens_ci) && !is.null(spec_ci)) {
                    .fmt(.("(95% CI: sensitivity {sens}, specificity {spec})"),
                         sens = sprintf("%.1f-%.1f%%", sens_ci[1]*100, sens_ci[2]*100),
                         spec = sprintf("%.1f-%.1f%%", spec_ci[1]*100, spec_ci[2]*100))
                } else {
                    ""
                }
                # STARD 2015 item 24: estimates of diagnostic accuracy and their precision. No square
                # brackets here: jmvcore's translator splits a string at " [" (msgctxt).
                pct_ci <- function(ci) if (is.null(ci) || length(ci) != 2 || anyNA(ci)) ""
                                       else sprintf(" (95%% CI %.1f-%.1f%%)", 100 * ci[1], 100 * ci[2])
                lr_ci_text <- if (is.null(lr_pos_ci) || length(lr_pos_ci) != 2 || anyNA(lr_pos_ci)) ""
                              else paste0(" (95% CI ", private$.fmtLR(lr_pos_ci[1]), "-",
                                          private$.fmtLR(lr_pos_ci[2]), ")")

                # Band from the shared classifier (.lrBand), so this sentence, the Clinical
                # Summary and the Clinical Interpretation panel always agree.
                interpretation <- switch(private$.lrBand(lr_pos, "pos", private$.youdenDirection),
                    na = .("likelihood ratio unavailable due to data limitations"),
                    unstable = .("no reliable evidence either way, because it rests on a zero count and the continuity correction puts it on the other side of 1 from the observed data"),
                    large = .("strong evidence for disease when positive"),
                    moderate = .("moderate evidence for disease when positive"),
                    small = .("weak evidence for disease when positive"),
                    minimal = .("minimal evidence for disease when positive"),
                    none = .("no change in the probability of disease"),
                    # An LR+ below 1 points the other way. Calling that "minimal evidence
                    # for disease" in text a clinician pastes into a chart inverts the
                    # finding.
                    against = .("evidence against disease when positive"))

                # Generate template. It states N and both arms (STARD 2015 item 23: the cross
                # tabulation behind the estimates): a manuscript sentence
                # without its denominator cannot be checked (STARD 2015).
                template_string <- .("<div style='margin: 15px; padding: 15px; border: 2px dashed #2196F3; background-color: rgba(33, 152, 239, 0.13); color: inherit;'><h3 style='color: inherit; margin-top: 0;'>Copy-Ready Results Paragraph</h3><div style='background: rgba(255, 255, 255, 0.06); color: inherit; padding: 10px; border-radius: 5px; font-family: Arial, sans-serif;'><p><strong>DIAGNOSTIC TEST EVALUATION</strong></p><p>We evaluated the diagnostic performance of {testname} compared with the reference standard {goldname} in {n} cases ({npos} with and {nneg} without the target condition). The test demonstrated a sensitivity of {sens} and specificity of {spec} {ci}. At a disease prevalence of {prev}, the positive predictive value was {ppv}{ppvci} and the negative predictive value was {npv}{npvci}. Judged on its point estimate, the positive likelihood ratio of {lr}{lrci} suggests {interp}. Likelihood-ratio bands follow Jaeschke et al. (1994).</p></div><p style='font-size: 12px; color: inherit; opacity: 0.75;'><em>Copy the text above into a manuscript or study report, not a patient report, and edit it to fit your study.</em></p></div>")

                # Escape user-derived variable names before HTML interpolation
                test_name_safe <- private$.safeHtmlOutput(test_name)
                gold_name_safe <- private$.safeHtmlOutput(gold_name)

                template <- .fmt(
                    template_string,
                    testname = test_name_safe, goldname = gold_name_safe,
                    n = sprintf("%d", as.integer(n_total)),
                    npos = sprintf("%d", as.integer(n_diseased)),
                    nneg = sprintf("%d", as.integer(n_healthy)),
                    sens = sprintf("%.1f%%", sens * 100),
                    spec = sprintf("%.1f%%", spec * 100),
                    ci = ci_text,
                    # Predictive values are meaningless without the prevalence they
                    # were computed at; the sentence quoted them bare.
                    prev = sprintf("%.1f%%", if (is.finite(prevalence)) prevalence * 100 else NA_real_),
                    ppv = sprintf("%.1f%%", ppv * 100),
                    ppvci = pct_ci(ppv_ci),
                    npv = sprintf("%.1f%%", npv * 100),
                    npvci = pct_ci(npv_ci),
                    # .fmtLR, as in the Clinical Summary: at one decimal LR+ 1.96
                    # printed "2.0" beside the band for LR+ below 2.
                    lr = private$.fmtLR(lr_pos),
                    lrci = lr_ci_text,
                    interp = interpretation
                )

                return(template)
            },
            
            # Misuse detection.
            #
            # These used to be assembled into an HTML panel prepended to
            # clinicalInterpretation, which is visible: (showClinicalInterpretation)
            # and defaults to FALSE -- so the warnings a clinician most needs were
            # the ones hidden behind an opt-in checkbox. They now go to the
            # always-visible notices pane.
            #
            # The small-cell (< 5) and small-sample checks that used to live here
            # were duplicates of .validateSampleSize(), which already covers both
            # and already writes to notices. Only the three checks unique to this
            # function remain.
            .detectMisuse = function(conf_table, prevalence, n_total) {
                if (!is.na(prevalence) && prevalence < 0.05) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        refs = c("AltmanBland1994b", "UsherSmith2016"),
                        title = .fmt(.("Very low disease prevalence observed in this sample ({pct})"),
                                                pct = sprintf("%.1f%%", 100 * prevalence)),
                        # No "consider supplying a prior" when one is already supplied, and the
                        # prevalence-independence of Se/Sp stated as the assumption it is
                        # (it holds arithmetically; case mix can still change them).
                        content = if (isTRUE(self$options$pp))
                            .("Positive predictive value is unstable at this prevalence. The main table uses the population prevalence you supplied. Sensitivity and specificity do not depend on prevalence arithmetically, but they can differ with case mix.")
                        else
                            .("Positive predictive value is unstable at this prevalence and will not transfer to a population with a different one. Sensitivity and specificity do not depend on prevalence arithmetically, but they can differ with case mix. Consider supplying a population prior under Population Prevalence Settings.")
                    )
                }

                if (!is.na(prevalence) && prevalence > 0.95) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        refs = c("AltmanBland1994b"),
                        title = .fmt(.("Very high disease prevalence observed in this sample ({pct})"),
                                                pct = sprintf("%.1f%%", 100 * prevalence)),
                        content = .("Negative predictive value is unstable at this prevalence. Verify that the level selected as disease-present is the one you meant.")
                    )
                }

                pos_ratio <- sum(conf_table[1, ]) / n_total
                if (!is.na(pos_ratio) && (pos_ratio < 0.1 || pos_ratio > 0.9)) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Highly unbalanced test results ({pct} positive)"),
                                                pct = sprintf("%.1f%%", 100 * pos_ratio)),
                        content = .("Check that the level selected under Test positive level is the one you meant: this analysis has no numeric cut-point, it simply treats that level as a positive result.")
                    )
                }

                invisible(NULL)
            },
            
            # Generate About This Analysis content
            .generateAboutAnalysis = function() {
                about_content <- paste0(
                    "<div style='margin: 15px; padding: 15px; background-color: rgba(88, 88, 88, 0.06); border-radius: 8px; color: inherit;'>",
                    "<h3 style='color: inherit; margin-top: 0;'>", .("About Diagnostic Test Evaluation"), "</h3>",

                    "<h4 style='color: inherit;'>", .("What This Analysis Does"), "</h4>",
                    "<p>", .("This analysis evaluates diagnostic test performance by comparing test results to a gold standard (reference). It calculates key diagnostic accuracy measures including sensitivity, specificity, predictive values, and likelihood ratios."), "</p>",

                    "<h4 style='color: inherit;'>", .("When to Use This Analysis"), "</h4>",
                    "<ul>",
                    "<li>", .("Validating new tests"), "</li>",
                    "<li>", .("Clinical validation studies"), "</li>",
                    "<li>", .("Evaluating one test. To compare two tests done on the same cases, use Compare Medical Decision Tests, which accounts for the pairing."), "</li>",
                    "<li>", .("Quality assurance for laboratory tests"), "</li>",
                    "<li>", .("Medical device evaluation"), "</li>",
                    "</ul>",

                    "<h4 style='color: inherit;'>", .("Data Requirements"), "</h4>",
                    # The old text said "preferably 30+" while the sample-size notices said
                    # 100; neither figure had a source. What decides precision is the number
                    # of diseased cases (for sensitivity) and disease-free cases (for
                    # specificity), which is what Buderer (1996) plans from.
                    "<p>", .("Required data: Cases with both a test result and a reference-standard result (the Gold Standard variable). Both variables must be categorical (factor), each with a positive and a negative level; any other level is excluded, and at least 4 cases must remain after exclusions."), "</p>",
                    # Independent review 2026-09-25 (F3): nothing in the data can reveal several
                    # specimens per patient, so the assumption has to be stated.
                    "<p>", private$.independenceText(), "</p>",
                    "<p>", .("How many cases are enough depends on the precision you need: sensitivity is estimated from the diseased cases only and specificity from the disease-free cases only, so plan the sample size from the confidence-interval width you need at the expected prevalence (Buderer 1996), and read the confidence intervals rather than the point estimates."), "</p>",

                    "<h4 style='color: inherit;'>", .("Key Output Measures"), "</h4>",
                    "<ul>",
                    "<li><strong>", .("Sensitivity"), ":</strong> ", .("Proportion of diseased patients correctly identified (true positive rate). When it is high, a negative result argues against disease, provided specificity is not low."), "</li>",
                    "<li><strong>", .("Specificity"), ":</strong> ", .("Proportion of patients without the disease correctly identified (true negative rate). When it is high, a positive result argues for disease, provided sensitivity is not low."), "</li>",
                    "<li><strong>", .("PPV (Positive Predictive Value)"), ":</strong> ", .("Probability of disease given a positive test. Depends on prevalence, sensitivity and specificity."), "</li>",
                    "<li><strong>", .("NPV (Negative Predictive Value)"), ":</strong> ", .("Probability of being free of the target condition given a negative test. Depends on prevalence, sensitivity and specificity."), "</li>",
                    "<li><strong>", .("LR+ (Positive Likelihood Ratio)"), ":</strong> ", .("How much a positive test increases disease odds. LR+ >10 strong evidence FOR disease, LR+ 5-10 moderate, LR+ 2-5 weak."), "</li>",
                    "<li><strong>", .("LR- (Negative Likelihood Ratio)"), ":</strong> ", .("How much a negative test decreases disease odds. LR- <0.1 strong evidence AGAINST disease, LR- 0.1-0.2 moderate, LR- 0.2-0.5 weak."), "</li>",
                    "</ul>",
                    
                    "<h4 style='color: inherit;'>", .("Clinical Interpretation Guidelines"), "</h4>",
                    "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                    # "Excellent Tests" / "excellent for ruling OUT" stated SnNout and SpPin
                    # unqualified, and "excellent" now names an AUC band as well.
                    "<strong>", .("Rules of thumb"), ":</strong><br>",
                    "\u{2022} ", .("Sensitivity above 90%: a negative result argues against disease, provided specificity is not low"), "<br>",
                    "\u{2022} ", .("Specificity above 90%: a positive result argues for disease, provided sensitivity is not low"), "<br>",
                    "\u{2022} ", .("LR+ >10 strong evidence FOR disease, LR+ 5-10 moderate, LR+ 2-5 weak but sometimes useful"), "<br>",
                    "\u{2022} ", .("LR- <0.1 strong evidence AGAINST disease, LR- 0.1-0.2 moderate, LR- 0.2-0.5 weak"), "<br>",
                    "</div>",
                    # SnNout / SpPin stated without their caveat invite ruling out on a
                    # sensitive but non-specific test (Pewsner et al. 2004).
                    "<p>", .("The sensitivity and specificity rules of thumb (SnNout: a negative result on a highly sensitive test rules out; SpPin: a positive result on a highly specific test rules in) can mislead when the other property is low. Judge how far a result moves the probability from its likelihood ratio instead (Pewsner et al. 2004)."), "</p>",
                    "<p>", .("Likelihood-ratio bands follow Jaeschke, Guyatt and Sackett (1994). Discrimination bands follow Hosmer, Lemeshow and Sturdivant (2013), applied to a binary test through AUC = (sensitivity + specificity) / 2."), "</p>",

                    "<h4 style='color: inherit;'>", .("Analysis Options Explained"), "</h4>",
                    "<ul>",
                    "<li><strong>", .("95% Confidence Intervals"), ":</strong> ", .("Provides uncertainty estimates: exact Clopper-Pearson intervals for sensitivity, specificity and the predictive values and log-scale intervals for the likelihood ratios and the diagnostic odds ratio (computed by the epiR package), and the Agresti-Caffo interval for Youden's index, from which the number needed to diagnose interval follows. These intervals describe the observed sample; when a population prevalence is supplied they do not apply to the prior-adjusted predictive values in the main table."), "</li>",
                    "<li><strong>", .("Explanatory Footnotes"), ":</strong> ", .("Adds definitions and interpretation notes to the totals table, the main statistics table and the two confidence-interval tables."), "</li>",
                    "<li><strong>", .("Raw Data Tables"), ":</strong> ", .("Displays the analysed contingency table and combination counts, and a summary of excluded cases, for verification."), "</li>",
                    "<li><strong>", .("Population Prevalence"), ":</strong> ", .("Use when your study sample doesn't represent the target population prevalence. Affects PPV/NPV calculations using Bayes' theorem. Enter as proportion (e.g., 0.05 for 5%, 0.15 for 15%)."), "</li>",
                    "<li><strong>", .("Fagan Nomogram"), ":</strong> ", .("Visual tool showing how test results change disease probability. Shows relationship between pre-test probability, likelihood ratios, and post-test probability."), "</li>",
                    "</ul>",

                    "<h4 style='color: inherit;'>", .("Common Issues"), "</h4>",
                    "<ul>",
                    "<li>", .("Small cell counts (<5) may cause unstable results"), "</li>",
                    "<li>", .("Extreme prevalence affects predictive values"), "</li>",
                    "<li>", .("Predictive values depend on disease prevalence in your population"), "</li>",
                    "<li>", .("Consider clinical consequences of false positives vs false negatives"), "</li>",
                    "<li>", .("Results are only as good as your gold standard"), "</li>",
                    "</ul>",

                    "</div>"
                )
                
                return(about_content)
            },

            # Check data size and provide performance warnings
            .checkDataSize = function(data) {
                n_rows <- nrow(data)
                if (n_rows > 100000) {
                    private$.addNotice(
                        type = "WARNING",
                        title = .fmt(.("Very large dataset detected ({n} rows)"), n = n_rows),
                        content = .("Analysis may take longer than usual.")
                    )
                } else if (n_rows > 10000) {
                    private$.addNotice(
                        type = "INFO",
                        title = .fmt(.("Large dataset detected ({n} rows)"), n = n_rows),
                        content = .("Analysis may take a moment to complete.")
                    )
                }
            },

            # Validate sample size for diagnostic test evaluation
            .validateSampleSize = function(conf_table) {
                total_n <- sum(conf_table)
                min_cell <- min(conf_table)

                # Clinical best practices for diagnostic tests
                if (total_n < 20) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        refs = c("Buderer1996"),
                        title = .fmt(.("Very small sample size: n = {n} (< 20 cases)"), n = total_n),
                        content = .("With fewer than 20 cases each proportion rests on a handful of patients, so one reclassified case moves sensitivity or specificity by several percentage points and the 95% confidence intervals (tick 95% confidence intervals) will be very wide. Read the intervals rather than the point estimates. How many cases are enough depends on the precision you need at the expected prevalence (Buderer 1996), not on a fixed total.")
                    )
                } else if (total_n < 50) {
                    private$.addNotice(
                        type = "WARNING",
                        refs = c("Buderer1996"),
                        title = .fmt(.("Small sample size: n = {n} (< 50 cases)"), n = total_n),
                        content = .("Interpret results with caution: the confidence intervals will be wide. Whether the sample is large enough depends on the precision you need for sensitivity and specificity at the expected prevalence (Buderer 1996); read the intervals.")
                    )
                } else if (total_n < 100) {
                    private$.addNotice(
                        type = "INFO",
                        refs = c("Buderer1996"),
                        title = .fmt(.("Sample size: n = {n}"), n = total_n),
                        content = .("Check the confidence intervals: sensitivity rests on the diseased cases only and specificity on the disease-free cases only, so either can be imprecise even when the total looks adequate (Buderer 1996).")
                    )
                }

                if (min_cell < 5) {
                    private$.addNotice(
                        type = "WARNING",
                        refs = c("ClopperPearson1934"),
                        title = .fmt(.("Small cell count detected (minimum = {n}, < 5)"), n = min_cell),
                        content = .("Statistical estimates may be unstable. Use the 95% confidence intervals option and read the intervals: those for sensitivity, specificity and the predictive values are exact (Clopper-Pearson) and stay valid with small cells; the others are approximate.")
                    )
                }

                # Total n says nothing about the denominators the two headline proportions
                # actually rest on. table(test, gold) puts the gold-standard levels in the
                # columns, positive first, so column 1 is the sensitivity denominator and
                # column 2 the specificity denominator. An enriched series can pass the
                # n >= 50 check and still estimate sensitivity from six patients.
                disease_p <- sum(conf_table[, 1])
                disease_n <- sum(conf_table[, 2])
                if (disease_p < 10 || disease_n < 10) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = .fmt(.("Few cases in one arm: {dpos} disease-present and {dneg} disease-free"),
                                     dpos = sprintf("%d", as.integer(disease_p)),
                                     dneg = sprintf("%d", as.integer(disease_n))),
                        content = .("Sensitivity is estimated from the disease-present cases only and specificity from the disease-free cases only, so these are the numbers that matter, not the total sample size. With fewer than ten cases in an arm one reclassified patient moves that proportion by more than ten percentage points. Enable 95% confidence intervals and report each estimate with its interval.")
                    )
                }
            },

            # Discrimination check.
            #
            # For a binary test the area under the ROC curve is (Sens + Spec) / 2, so
            # Youden's index (Sens + Spec - 1) is 2*AUC - 1 and the usual AUC thresholds
            # carry over directly. Nothing here used to fire at all: the only
            # inverted-test language lived in .addClinicalBenchmarks() and
            # .getDiagnosticInterpretation(), which render only into panels gated behind
            # showClinicalInterpretation / showNaturalLanguage, both default false. A
            # pathologist who had picked the wrong level as test-positive saw a fully
            # populated table, a nomogram, and an empty Important Information pane.
            .validateDiscrimination = function(sens, spec, youden_ci) {
                if (is.na(sens) || is.na(spec)) return(invisible(NULL))

                youden <- sens + spec - 1
                auc <- (sens + spec) / 2
                # Same classifier as the Clinical Summary word and the Clinical
                # Interpretation panel, so the three can never disagree about one test.
                band <- private$.discriminationBand(sens, spec)

                # Severity follows the Agresti-Caffo 95% interval (.youdenCI), not the sign of
                # the point estimate. A coin-flip test at Youden -0.005 (n = 201, interval
                # -0.142 to 0.132) drew the ERROR "check the level selection before reading any
                # number" while +0.005 drew a warning. Only an interval wholly below 0 says the
                # levels are probably inverted; one that includes 0 says there is no evidence of
                # discrimination, whichever side of 0 the estimate falls.
                ci_lower <- sprintf("%.3f", youden_ci[["lower"]])
                ci_upper <- sprintf("%.3f", youden_ci[["upper"]])
                below_chance <- youden_ci[["upper"]] < 0
                indistinct <- youden_ci[["lower"]] <= 0 && youden_ci[["upper"]] >= 0

                if (band == "worse" && below_chance) {
                    private$.addNotice(
                        type = "ERROR",
                        refs = c("youden1950", "AgrestiCaffo2000"),
                        title = .fmt(.("This test performs worse than chance in this sample (Youden's index {j}, equivalent AUC {auc})"),
                                     j = sprintf("%.3f", youden), auc = private$.fmtAUC(youden)),
                        content = .fmt(.("Sensitivity plus specificity is below 1, and the 95% confidence interval for Youden's index ({lower} to {upper}) lies entirely below 0, so this is not sampling noise: a positive result argues AGAINST disease and a negative result argues for it. The usual cause is a wrong choice under Test positive level or Disease present level; swapping it would give the mirror-image performance. Check the level selection before reading any number in these tables."),
                                       lower = ci_lower, upper = ci_upper)
                    )
                } else if (band %in% c("worse", "poor") && indistinct) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        refs = c("youden1950", "AgrestiCaffo2000"),
                        title = .fmt(.("No evidence that this test discriminates in this sample (Youden's index {j}, 95% CI {lower} to {upper})"),
                                     j = sprintf("%.3f", youden), lower = ci_lower, upper = ci_upper),
                        content = .("The 95% confidence interval for Youden's index includes 0, so this sample cannot tell the test apart from chance, whichever side of 0 the estimate falls. Report it as showing no evidence of discrimination, not as poor or as worse than chance. If you expected the test to work, check the levels chosen under Test positive level and Disease present level, and whether the sample gives the precision you need.")
                    )
                } else if (band == "none") {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        refs = c("youden1950"),
                        title = .("This test is uninformative in this sample (Youden's index 0.000, equivalent AUC 0.500)"),
                        content = .("Sensitivity plus specificity equals 1 to three decimal places: the test calls about the same share of disease-present and disease-free cases positive, which is what a coin toss achieves. When positive or negative results are rare, a likelihood ratio can still differ from 1, so check both likelihood ratios.")
                    )
                } else if (band == "poor") {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        refs = c("HosmerLemeshow2013", "youden1950"),
                        title = .fmt(.("Poor discrimination (Youden's index {j}, equivalent AUC {auc})"),
                                     j = sprintf("%.3f", youden), auc = private$.fmtAUC(youden)),
                        content = .("An equivalent area under the curve below 0.70 is conventionally read as poor discrimination (Hosmer, Lemeshow and Sturdivant 2013). Confirm that the level chosen under Test positive level is the one you meant, then use each result's likelihood ratio to judge how far it moves the probability of disease.")
                    )
                }

                invisible(NULL)
            },

            # Add clinical performance benchmarks for interpretation
            .addClinicalBenchmarks = function(sens, spec, lr_pos, lr_neg) {
                benchmarks <- list()

                # Sensitivity describes the false-negative side of the 2x2: the share of
                # gold-standard positive cases this test called negative, stated exactly.
                # (The old bands gave one bound only, so 0% sensitivity read "more than 1
                # in 5 missed".) It is the complement of the sensitivity as displayed,
                # so the two always sum to 100.0%. The is.na() guard is required --
                # sensitivity is NA when no case is gold-standard positive.
                benchmarks$sens_quality <- if (is.na(sens)) .("not estimable: no gold-standard positive cases")
                                           else .fmt(.("{pct} of diseased cases missed in this sample"),
                                                     pct = sprintf("%.1f%%", 100 - as.numeric(sprintf("%.1f", 100 * sens))))

                # Specificity describes the false-positive side: how many gold-standard
                # negative cases this test called positive. Same NA guard, for a cohort
                # with no gold-standard negative cases.
                benchmarks$spec_quality <- if (is.na(spec)) .("not estimable: no gold-standard negative cases")
                                           else .fmt(.("{pct} of disease-free cases flagged positive in this sample"),
                                                     pct = sprintf("%.1f%%", 100 - as.numeric(sprintf("%.1f", 100 * spec))))

                # Likelihood ratio benchmarks: the shared classifier (.lrBand). This block
                # used > 10 / > 5 / > 2 where the other two panels used >= 10 / >= 5 / >= 2,
                # so LR+ = 10 was "Moderate" here and "strong" in the report.
                benchmarks$lr_pos_interpretation <- switch(private$.lrBand(lr_pos, "pos", private$.youdenDirection),
                    na = .("Cannot be calculated: LR+ is sensitivity / (1 - specificity), and one of those is not estimable here"),
                    unstable = .("Unreliable: rests on a zero count, and the continuity correction points the other way from the observed data"),
                    large = .("Strong evidence for disease"),
                    moderate = .("Moderate evidence for disease"),
                    small = .("Weak evidence for disease"),
                    minimal = .("Minimal evidence for disease"),
                    none = .("Uninformative: a positive result does not change the probability of disease"),
                    against = .("Points AGAINST disease when positive: the test-positive level may be inverted"))

                benchmarks$lr_neg_interpretation <- switch(private$.lrBand(lr_neg, "neg", private$.youdenDirection),
                    na = .("Cannot be calculated: LR- is (1 - sensitivity) / specificity, and one of those is not estimable here"),
                    unstable = .("Unreliable: rests on a zero count, and the continuity correction points the other way from the observed data"),
                    large = .("Strong evidence against disease"),
                    moderate = .("Moderate evidence against disease"),
                    small = .("Weak evidence against disease"),
                    minimal = .("Minimal evidence against disease"),
                    none = .("Uninformative: a negative result does not change the probability of disease"),
                    against = .("Points TOWARD disease when negative: the test-positive level may be inverted"))

                return(benchmarks)
            },

            # Centralized footnote management with clinical interpretation
            .addFootnotes = function() {
                if (!self$options$fnote) return()

                # nTable footnotes
                nTable <- self$results$nTable
                footnotes_n <- list(
                    TotalPop = .("Number of cases analysed. Excluded: cases with a missing test or reference result, or with a level other than the selected positive and negative levels."),
                    DiseaseP = .("Number of cases with the target condition (Reference Positive)"),
                    DiseaseN = .("Number of cases without the target condition (Reference Negative); they may have other diseases"),
                    TestP = .("Total Number of Positive Test Results"),
                    TestN = .("Total Number of Negative Test Results"),
                    TestT = .("Number of test results that agree with the reference standard (TP + TN)"),
                    TestW = .("Number of test results that disagree with the reference standard (FP + FN)")
                )

                # Vectorized footnote application for better performance
                if (length(footnotes_n) > 0) {
                    mapply(function(col, note) nTable$addFootnote(rowNo = 1, col = col, note),
                           names(footnotes_n), footnotes_n)
                }

                # ratioTable footnotes with clinical interpretation
                ratioTable <- self$results$ratioTable
                footnotes_ratio <- list(
                    Sens = .("Sensitivity: Proportion of diseased patients correctly identified (TP rate). A negative result on a highly sensitive test may not rule out disease if specificity is low; the negative likelihood ratio shows how much a negative result changes the probability of disease (Pewsner et al. 2004)."),
                    Spec = .("Specificity: Proportion of patients without the disease correctly identified (TN rate). A positive result on a highly specific test may not rule in disease if sensitivity is low; the positive likelihood ratio shows how much a positive result changes the probability of disease (Pewsner et al. 2004)."),
                    PrevalenceD = if (isTRUE(self$options$pp)) {
                        # This previously claimed the predictive values were computed
                        # from the prior while the code left them at study prevalence.
                        # They now are, so the statement is true - but say plainly
                        # that this cell is not the sample's own prevalence.
                        .("Prevalence: the user-supplied population prevalence (Known population prevalence, under Population Prevalence Settings), NOT this sample's observed prevalence. Predictive values, which are the post-test probabilities, are computed from this prior; sensitivity and specificity are not affected by it.")
                    } else {
                        .("Disease Prevalence: Observed proportion with disease in this sample. Affects predictive values.")
                    },
                    PPV = .("Positive Predictive Value: Probability of disease given a positive test. This IS the post-test probability of disease. Depends on prevalence, sensitivity and specificity."),
                    NPV = .("Negative Predictive Value: Probability of not having the target condition given a negative test. 1 - NPV is the post-test probability of the condition. Depends on prevalence, sensitivity and specificity."),
                    LRP = .("Positive Likelihood Ratio: How much more likely a positive result is in patients with vs without the disease. Above 10 = strong evidence, 5-10 = moderate, 2-5 = weak but potentially useful (Jaeschke et al. 1994)."),
                    LRN = .("Negative Likelihood Ratio: the chance of a negative result in patients with the disease divided by the chance in patients without it. Below 0.1 = strong evidence against disease, 0.1-0.2 = moderate, 0.2-0.5 = weak (Jaeschke et al. 1994).")
                )

                # Vectorized footnote application for better performance
                if (length(footnotes_ratio) > 0) {
                    mapply(function(col, note) ratioTable$addFootnote(rowNo = 1, col = col, note),
                           names(footnotes_ratio), footnotes_ratio)
                }
            },

            # Consolidated content generation for improved performance
            .generateAllContent = function(sens, spec, ppv, npv, lr_pos, lr_neg,
                                         prior_prob, total_pop, test_name, gold_name,
                                         sens_ci = NULL, spec_ci = NULL,
                                         continuity_used = FALSE,
                                         report_extras = list()) {

                results <- list(
                    clinical_summary = "",
                    about_content = "",
                    natural_summary = "",
                    report_template = ""
                )

                format_percent <- function(value, default = .("not available")) {
                    if (is.na(value)) return(default)
                    sprintf("%.1f%%", value * 100)
                }

                # Generate clinical interpretation
                results$clinical_summary <- tryCatch({
                    interpretation <- private$.getDiagnosticInterpretation(lr_pos, lr_neg, sens, spec)

                    sens_text <- format_percent(sens)
                    spec_text <- format_percent(spec)
                    youden_text <- if (is.na(interpretation$youden_index)) .("not available") else sprintf("%.3f", interpretation$youden_index)
                    lr_pos_text <- if (is.na(lr_pos)) .("undefined") else private$.fmtLR(lr_pos)
                    lr_neg_text <- if (is.na(lr_neg)) .("undefined") else private$.fmtLR(lr_neg)

                    paste0(
                        "<div style='margin: 15px; padding: 10px; border-left: 4px solid #2196F3; background-color: rgba(138, 155, 172, 0.06); color: inherit;'>",
                        "<h4 style='color: inherit; margin-top: 0;'>", .("Clinical Interpretation"), "</h4>",
                        "<p><strong>", .("Test Performance Summary"), ":</strong></p>",
                        "<ul>",
                        "<li><strong>", .("Sensitivity"), ":</strong> ", sens_text, " - ",
                        if (is.na(sens)) .("not estimable: no gold-standard positive cases") else .fmt(.("{fn} of {n} diseased cases missed here"), fn = sprintf("%d", as.integer(round(report_extras$n_diseased * (1 - sens)))), n = sprintf("%d", as.integer(report_extras$n_diseased))),
                        "</li>",
                        "<li><strong>", .("Specificity"), ":</strong> ", spec_text, " - ",
                        if (is.na(spec)) .("not estimable: no gold-standard negative cases") else .fmt(.("{fp} of {n} disease-free cases flagged positive here"), fp = sprintf("%d", as.integer(round(report_extras$n_healthy * (1 - spec)))), n = sprintf("%d", as.integer(report_extras$n_healthy))),
                        "</li>",
                        "<li><strong>", .("Youden's Index"), ":</strong> ", youden_text, " - ", interpretation$test_utility, "</li>",
                        "</ul>",
                        "<p><strong>", .("Likelihood Ratio Interpretation (point estimates; rule of thumb, Jaeschke et al. 1994)"), ":</strong></p>",
                        "<ul>",
                        # paste0, not jmvcore::format: lr_pos_text/lr_neg_text can be
                        # .("undefined"), and feeding a TRANSLATED value into a {placeholder}
                        # can hang jmvcore::format forever (it re-scans after substituting).
                        "<li><strong>", .("Positive LR"), " (", lr_pos_text, "):</strong> ", interpretation$lr_pos_interp, "</li>",
                        "<li><strong>", .("Negative LR"), " (", lr_neg_text, "):</strong> ", interpretation$lr_neg_interp, "</li>",
                        "</ul>",
                        "<p><strong>", .("Pre-test and Post-test Probabilities"), ":</strong></p>",
                        "<ul>",
                        "<li>", if (isTRUE(report_extras$pp)) .("Pre-test probability of disease (population prevalence you supplied)") else .("Pre-test probability of disease (prevalence in this sample)"), ": <strong>", format_percent(prior_prob, .("not provided")), "</strong></li>",
                        "<li>", .("Post-test probability of disease, test positive"), ": <strong>", format_percent(ppv, .("not available")), "</strong> ", .("(this is the PPV)"), "</li>",
                        "<li>", .("Post-test probability of disease, test negative"), ": <strong>", format_percent(1 - npv, .("not available")), "</strong> ", .("(this is 1 - NPV)"), "</li>",
                        "</ul></div>"
                    )
                }, error = function(e) {
                    paste0(
                        "<div style='margin: 15px; padding: 10px; border-left: 4px solid #ff9800; background-color: rgba(255, 169, 33, 0.14); color: inherit;'>",
                        "<h4 style='color: inherit; margin-top: 0;'>", .("Clinical Interpretation"), "</h4>",
                        "<p>", .("The detailed clinical interpretation could not be generated."), "</p>",
                        "<p><strong>", .("Basic Results"), ":</strong> ",
                        # paste0, not jmvcore::format: both values fall back to
                        # .("not available"), a translated string -- see the note above.
                        .("Sensitivity"), ": ", format_percent(sens, .("not available")), ", ",
                        .("Specificity"), ": ", format_percent(spec, .("not available")),
                        "</p>",
                        "</div>"
                    )
                })

                # Generate about content
                results$about_content <- tryCatch({
                    private$.generateAboutAnalysis()
                }, error = function(e) {
                    paste0("<div>", .("About analysis content unavailable"), "</div>")
                })

                # Generate natural language summary
                results$natural_summary <- tryCatch({
                    private$.generateNaturalLanguageSummary(
                        sens, spec, ppv, npv, lr_pos, lr_neg,
                        prior_prob, total_pop, test_name, gold_name
                    )
                }, error = function(e) {
                    fallback_template <- .("<div style='margin: 15px; padding: 15px; border-left: 5px solid #FF9800; background-color: rgba(255, 169, 33, 0.14); color: inherit;'><h3 style='color: inherit; margin-top: 0;'>Clinical Summary</h3><p>Basic diagnostic test evaluation completed with {n} cases.</p><p><strong>Results:</strong> Sensitivity {sens}, Specificity {spec}</p></div>")

                    .fmt(
                        fallback_template,
                        n = sprintf("%d", total_pop),
                        sens = sprintf("%.1f%%", sens * 100),
                        spec = sprintf("%.1f%%", spec * 100)
                    )
                })

                # Generate report template
                results$report_template <- tryCatch({
                    private$.generateReportTemplate(
                        sens, spec, ppv, npv, lr_pos, lr_neg,
                        sens_ci = sens_ci, spec_ci = spec_ci,
                        test_name = test_name,
                        gold_name = gold_name,
                        prevalence = prior_prob,
                        n_total = total_pop,
                        n_diseased = report_extras$n_diseased %||% NA_real_,
                        n_healthy = report_extras$n_healthy %||% NA_real_,
                        ppv_ci = report_extras$ppv_ci,
                        npv_ci = report_extras$npv_ci,
                        lr_pos_ci = report_extras$lr_pos_ci
                    )
                }, error = function(e) {
                    fallback_template <- .("<div style='margin: 15px; padding: 15px; border: 2px dashed #2196F3; background-color: rgba(33, 152, 239, 0.13); color: inherit;'><h3 style='color: inherit; margin-top: 0;'>Copy-Ready Results Paragraph</h3><p>Diagnostic test evaluation shows sensitivity of {sens} and specificity of {spec}.</p></div>")

                    .fmt(
                        fallback_template,
                        sens = sprintf("%.1f%%", sens * 100),
                        spec = sprintf("%.1f%%", spec * 100)
                    )
                })

                # The likelihood ratio quoted in these two blocks can come from the
                # Haldane-Anscombe corrected table on a zero cell, while the sensitivity and
                # specificity beside it come from the observed counts. The report block is
                # labelled "Copy-Ready Clinical Report", so the qualifying sentences have to
                # sit INSIDE the copied box: appended after it they were below "Copy the text
                # above" and never reached the manuscript. Spliced in with sub(), not
                # interpolated: a translated sentence inside a {placeholder} can hang
                # jmvcore::format. If a translation changed the markup, fall back to appending.
                in_report_box <- function(html, extra) {
                    anchor <- "</p></div><p style='font-size: 12px"
                    if (grepl(anchor, html, fixed = TRUE))
                        sub(anchor, paste0("</p>", extra, "</div><p style='font-size: 12px"), html, fixed = TRUE)
                    else paste0(html, extra)
                }
                para <- function(text) paste0("<p style='font-size: 13px; color: inherit;'><em>",
                                              private$.safeHtmlOutput(text), "</em></p>")
                # Excluded cases, in the copied text itself (a denominator with no account of
                # the cases left out cannot be checked). One whole sentence per case, so the
                # translation never splices fragments.
                n_miss <- report_extras$n_missing %||% 0
                n_lev <- report_extras$n_level %||% 0
                excl_text <- if (n_miss > 0 && n_lev > 0)
                    .fmt(.("{excluded} of the {total} cases were excluded before analysis: {missing} with a missing test or reference-standard result, and {level} with a test or reference-standard level other than the two selected."),
                         excluded = sprintf("%d", as.integer(n_miss + n_lev)), total = sprintf("%d", as.integer(report_extras$n_rows)),
                         missing = sprintf("%d", as.integer(n_miss)), level = sprintf("%d", as.integer(n_lev)))
                else if (n_miss > 0)
                    .fmt(.("{missing} of the {total} cases were excluded before analysis because the test or reference-standard result was missing."),
                         missing = sprintf("%d", as.integer(n_miss)), total = sprintf("%d", as.integer(report_extras$n_rows)))
                else if (n_lev > 0)
                    .fmt(.("{level} of the {total} cases were excluded before analysis because their test or reference-standard level was neither of the two selected (for example, an indeterminate result)."),
                         level = sprintf("%d", as.integer(n_lev)), total = sprintf("%d", as.integer(report_extras$n_rows)))
                if (!is.null(excl_text))
                    results$report_template <- in_report_box(results$report_template,
                        paste0("<p>", private$.safeHtmlOutput(excl_text), "</p>"))

                if (isTRUE(continuity_used)) {
                    cc_sentence <- para(.("A cell of the 2x2 table was zero. A likelihood ratio that would be zero or infinite on the observed counts is computed from the Haldane-Anscombe corrected table (0.5 added to every cell); sensitivity, specificity and the predictive values above are not corrected."))
                    results$report_template <- in_report_box(results$report_template, cc_sentence)
                    results$natural_summary <- paste0(results$natural_summary, cc_sentence)
                    results$clinical_summary <- paste0(results$clinical_summary, cc_sentence)
                }

                # With a population prior the report's PPV/NPV are Bayes values at that
                # prior, which carry no interval here (the sample intervals belong to the
                # sample-prevalence values). Said inside the copied text.
                if (isTRUE(report_extras$pp))
                    results$report_template <- in_report_box(results$report_template, para(
                        .("The predictive values above are computed by Bayes' theorem at the assumed population prevalence stated above, not at the prevalence observed in this sample, so no confidence interval is given for them.")))

                return(results)
            }




            ,
            .run = function() {
                # Reset accumulated notices at the start of every run cycle.
                # The R6 instance is reused across runs, so the `.noticeList = list()`
                # field default (evaluated once at instantiation) is not enough: without
                # this reset each .addNotice() message re-appends and renders N times over
                # N runs (same pattern fixed in survival.b.R).
                private$.noticeList <- list()

                # Render whatever was collected however this method exits. Five
                # early return()s sit between here and the end of .run(), and each
                # one used to discard every notice the validators had raised -- a
                # failed run showed empty tables and no explanation at all. on.exit
                # also fires while an error unwinds, so the ERROR notices added
                # inside the confusion-matrix handler reach the user too.
                on.exit(private$.renderNotices(), add = TRUE)

                # Early return if variables not selected
                if (length(self$options$testPositive) + length(self$options$newtest) +
                    length(self$options$goldPositive) + length(self$options$gold) < 4)
                    return()

                # Consolidated input validation
                if (!private$.validateCategoricalInputs()) {
                    return()
                }

                # Efficient data preparation with missing data analysis
                original_data <- self$data
                prepared_data <- private$.prepareAnalysisData()

                # Check if data preparation failed
                if (is.null(prepared_data)) {
                    return()
                }

                mydata <- prepared_data$data
                testVariable <- prepared_data$testVar
                goldVariable <- prepared_data$goldVar
                
                # Check data size for performance warnings
                private$.checkDataSize(mydata)

                # Level labels. A negative level inferred from a dichotomous variable is
                # known only here, and the 2x2 table note below needs it whether or not
                # the raw data tables are shown.
                test_levels <- if (is.factor(mydata[[testVariable]])) {
                    levels(mydata[[testVariable]])
                } else {
                    sort(unique(as.character(mydata[[testVariable]])))
                }

                gold_levels <- if (is.factor(mydata[[goldVariable]])) {
                    levels(mydata[[goldVariable]])
                } else {
                    sort(unique(as.character(mydata[[goldVariable]])))
                }

                # Determine gold negative level (explicit or infer)
                has_gold_negative <- length(self$options$goldNegative) > 0 && nchar(self$options$goldNegative) > 0
                # Exactly one negative level takes part (explicit, or inferred when the
                # variable is dichotomous); .prepareAnalysisData has already errored out if
                # it was ambiguous, so no pooling can reach here.
                gold_negative_members <- if (has_gold_negative) self$options$goldNegative
                                         else setdiff(gold_levels, self$options$goldPositive)[1]
                gold_negative_label <- gold_negative_members[1]

                # Determine test negative level (explicit or infer)
                has_test_negative <- length(self$options$testNegative) > 0 && nchar(self$options$testNegative) > 0
                test_negative_members <- if (has_test_negative) self$options$testNegative
                                         else setdiff(test_levels, self$options$testPositive)[1]
                test_negative_label <- test_negative_members[1]

                # Raw data tables ----
                # rawContingency, rawCounts and missingDataSummary are all visible: (od).
                # Nothing is written to them unless the user asked for them: the
                # combination loop alone can add 200 rows.
                if (self$options$od) {
                    self$results$missingDataSummary$setContent(
                        private$.analyzeMissingData(original_data, mydata))

                    results1 <- mydata %>%
                        dplyr::select(dplyr::all_of(c(testVariable, goldVariable))) %>%
                        table()

                    # Level-combination counts for the rawCounts table
                    combination_counts <- mydata %>%
                        dplyr::count(.data[[testVariable]], .data[[goldVariable]]) %>%
                        dplyr::ungroup()

                    # Populate raw contingency jamovi table (using user's selected levels, not lexicographic order)
                    raw_contingency <- self$results$rawContingency
                    # Rows are scaffolded in .init() (see .rawContingencyLabels); they are relabelled and
                    # blanked below, never deleted, so the table keeps its shape while this runs.

                    results_matrix <- as.matrix(results1)

                    # Set column headers using USER'S selections, not lexicographic order.
                    # Titles and superTitles are rich-rendered: .richText() (see its comment).
                    if (!is.null(raw_contingency$getColumn("test_level"))) {
                        raw_contingency$getColumn("test_level")$setTitle(private$.richText(testVariable))
                        raw_contingency$getColumn("test_level")$setSuperTitle("")
                    }
                    if (!is.null(raw_contingency$getColumn("gold_pos"))) {
                        # Use user's goldPositive selection
                        raw_contingency$getColumn("gold_pos")$setTitle(private$.richText(self$options$goldPositive))
                        raw_contingency$getColumn("gold_pos")$setSuperTitle(private$.richText(goldVariable))
                    }
                    if (!is.null(raw_contingency$getColumn("gold_neg"))) {
                        # Use user's goldNegative selection (or inferred)
                        raw_contingency$getColumn("gold_neg")$setTitle(private$.richText(gold_negative_label))
                        raw_contingency$getColumn("gold_neg")$setSuperTitle(private$.richText(goldVariable))
                    }
                    if (!is.null(raw_contingency$getColumn("row_total"))) {
                        raw_contingency$getColumn("row_total")$setTitle(.("Total"))
                        raw_contingency$getColumn("row_total")$setSuperTitle("")
                    }

                    row_names <- rownames(results_matrix)
                    col_names <- colnames(results_matrix)

                    # Populate rows in order: positive test first, then negative test
                    # One positive row, one pooled negative row. Iterating a synthetic label
                    # would skip the negative row entirely (it is not a real level name).
                    ordered_test_groups <- list(
                        list(key = "test_pos", members = self$options$testPositive),
                        list(key = "test_neg", members = test_negative_members))

                    # Label every scaffolded row (the negative level may only be known now) and blank its
                    # counts, so a group absent from the data shows an empty row, not a stale count.
                    raw_labels <- private$.rawContingencyLabels(test_negative_label)
                    for (key in names(raw_labels))
                        raw_contingency$setRow(rowKey = key, values = list(
                            test_level = unname(raw_labels[[key]]),
                            gold_pos = NA_real_, gold_neg = NA_real_, row_total = NA_real_))

                    if (!is.null(test_levels) && length(test_levels) > 0 &&
                        !is.null(gold_levels) && length(gold_levels) > 0) {

                        for (grp in ordered_test_groups) {
                            present <- intersect(grp$members, test_levels)
                            if (length(present) == 0) next

                            row_vector <- if (!is.null(row_names)) {
                                rows_in <- intersect(present, row_names)
                                if (length(rows_in) == 0) matrix(0, nrow = 1, ncol = length(col_names))
                                else matrix(colSums(results_matrix[rows_in, , drop = FALSE]), nrow = 1)
                            } else {
                                matrix(0, nrow = 1, ncol = length(col_names))
                            }

                            row_values <- as.numeric(row_vector)
                            if (is.null(col_names) && length(row_values) == length(gold_levels)) {
                                names(row_values) <- gold_levels
                            } else if (!is.null(col_names)) {
                                names(row_values) <- col_names
                            }

                            # Use user's selected positive/negative levels
                            val_pos <- if (self$options$goldPositive %in% names(row_values)) {
                                row_values[[self$options$goldPositive]]
                            } else {
                                NA_real_
                            }

                            neg_in <- intersect(gold_negative_members, names(row_values))
                            val_neg <- if (length(neg_in) > 0) sum(row_values[neg_in], na.rm = TRUE) else NA_real_

                            row_total <- sum(row_values, na.rm = TRUE)

                            raw_contingency$setRow(
                                rowKey = grp$key,
                                values = list(
                                    gold_pos = val_pos,
                                    gold_neg = val_neg,
                                    row_total = row_total
                                )
                            )
                        }

                        col_totals <- if (!is.null(col_names) && length(col_names) > 0) {
                            colSums(results_matrix)
                        } else {
                            rep(sum(results_matrix), length(gold_levels))
                        }
                        if (is.null(names(col_totals)) && length(gold_levels) == length(col_totals)) {
                            names(col_totals) <- gold_levels
                        }

                        # Use user's selected levels for totals
                        total_pos <- if (self$options$goldPositive %in% names(col_totals)) {
                            col_totals[[self$options$goldPositive]]
                        } else {
                            NA_real_
                        }

                        neg_tot_in <- intersect(gold_negative_members, names(col_totals))
                        total_neg <- if (length(neg_tot_in) > 0) {
                            sum(col_totals[neg_tot_in], na.rm = TRUE)
                        } else {
                            NA_real_
                        }

                        raw_contingency$setRow(
                            rowKey = "total",
                            values = list(
                                gold_pos = total_pos,
                                gold_neg = total_neg,
                                row_total = sum(results_matrix)
                            )
                        )
                    }

                    # Populate raw combination count jamovi table
                    raw_counts_table <- self$results$rawCounts
                    # Clear existing rows - jamovi tables use deleteRows(), not clear()
                    try(raw_counts_table$deleteRows(), silent = TRUE)

                    if (!is.null(raw_counts_table$getColumn("test_level"))) {
                        raw_counts_table$getColumn("test_level")$setTitle(private$.richText(testVariable))
                        raw_counts_table$getColumn("test_level")$setSuperTitle("")
                    }
                    if (!is.null(raw_counts_table$getColumn("gold_level"))) {
                        raw_counts_table$getColumn("gold_level")$setTitle(private$.richText(goldVariable))
                        raw_counts_table$getColumn("gold_level")$setSuperTitle("")
                    }

                    combo_for_table <- combination_counts %>%
                        dplyr::mutate(
                            test_level = as.character(.data[[testVariable]]),
                            gold_level = as.character(.data[[goldVariable]]),
                            count = as.integer(.data$n)
                        ) %>%
                        dplyr::select(test_level, gold_level, count) %>%
                        dplyr::arrange(test_level, gold_level)

                    if (nrow(combo_for_table) > 0) {
                        # addRow is O(n^2) in jmvcore; a high-cardinality pair can produce
                        # thousands of combinations and lock the UI. Cap and disclose.
                        combo_cap <- min(nrow(combo_for_table), 200L)
                        if (nrow(combo_for_table) > combo_cap)
                            raw_counts_table$setNote("truncated", .fmt(
                                .("Showing the first {shown} of {total} level combinations."),
                                shown = sprintf("%d", combo_cap), total = sprintf("%d", nrow(combo_for_table))))
                        for (i in seq_len(combo_cap)) {
                            private$.checkpoint(flush = FALSE)
                            raw_counts_table$addRow(
                                rowKey = paste0("row_", i),
                                values = list(
                                    test_level = private$.richText(combo_for_table$test_level[i]),
                                    gold_level = private$.richText(combo_for_table$gold_level[i]),
                                    count = combo_for_table$count[i]
                                )
                            )
                        }
                    }
                }


                # conf_table ----
                # Data is already efficiently recoded in .prepareAnalysisData()
                conf_table <- table(mydata[["testVariable2"]], mydata[["goldVariable2"]])

                # Validate sample size and provide clinical guidance
                private$.validateSampleSize(conf_table)

                # Apply Haldane-Anscombe correction for zero cells to stabilize LR/OR.
                # CONDITIONAL use: 0.5 is added to every cell only when some cell is zero.
                # Haldane (1956) and Anscombe (1956) add 1/2 unconditionally to logits;
                # applying it to likelihood ratios and the DOR on a zero cell is the
                # convention Glas et al. (2003, p. 1131) describe, not Haldane's own result.
                conf_table_cc <- conf_table
                continuity_used <- FALSE
                if (any(conf_table == 0)) {
                    conf_table_cc <- conf_table + 0.5
                    continuity_used <- TRUE
                }
                # Haldane (1956) and Anscombe (1956) are cited only for this correction.
                if (!continuity_used)
                    for (item in c("ratioTable", "epirTable_number", "naturalLanguageSummary", "reportTemplate",
                                   "clinicalInterpretation")) {
                        element <- self$results$get(item)
                        element$setRefs(setdiff(element$getRefs(), c("haldane1956", "anscombe1956")))
                    }


                # .prepareAnalysisData() returned only when this is a 2x2 table with
                # "Positive" first on both margins and at least one case in every row and
                # column, so the four cells are plain counts and every margin used as a
                # denominator below (TP + FN, FP + TN, TP + FP, FN + TN) is positive.
                TP <- conf_table[1, 1]
                FP <- conf_table[1, 2]
                FN <- conf_table[2, 1]
                TN <- conf_table[2, 2]
                TPc <- conf_table_cc[1, 1]
                FPc <- conf_table_cc[1, 2]
                FNc <- conf_table_cc[2, 1]
                TNc <- conf_table_cc[2, 2]




                # Cross Table in jamovi style ----

                cTable <- self$results$cTable
                # The row and column labels are generic, so say which levels they are.
                # Inverting BOTH level choices mirrors every headline (sensitivity reads
                # specificity, PPV reads NPV) and leaves Youden and the DOR unchanged, so no
                # check can catch it; only the level names on screen can (independent review
                # 2026-09-25, F4; STARD 2015 asks for the definition of test positivity).
                cTable$setNote("levels", .fmt(
                    .('Reference Positive is "{gpos}" and Reference Negative is "{gneg}" ({gold}); Test Positive is "{tpos}" and Test Negative is "{tneg}" ({test}). Sensitivity is the proportion of "{gpos}" cases that the test calls "{tpos}".'),
                    gpos = private$.noteText(self$options$goldPositive),
                    gneg = private$.noteText(gold_negative_label),
                    gold = private$.noteText(goldVariable),
                    tpos = private$.noteText(self$options$testPositive),
                    tneg = private$.noteText(test_negative_label),
                    test = private$.noteText(testVariable)))

                cTable$setRow(
                    rowKey = "Test Positive",
                    values = list(
                        newtest = .("Test Positive"),
                        GP = TP,
                        GN = FP,
                        Total = TP + FP
                    )
                )


                cTable$setRow(
                    rowKey = "Test Negative",
                    values = list(
                        newtest = .("Test Negative"),
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

                # Every margin is positive (see the counts above), so no division is by 0.
                Sens <- TP / DiseaseP        # true-positive rate
                Spec <- TN / DiseaseN        # true-negative rate
                AccurT <- TestT / TotalPop
                PrevalenceD <- DiseaseP / TotalPop
                PPV <- TP / TestP
                NPV <- TN / TestN

                pp <- self$options$pp
                pprob <- self$options$pprob

                if (pp) {
                    # Known prior probability from population
                    PriorProb <- pprob
                } else {
                    # From ConfusionMatrix
                    PriorProb <- PrevalenceD
                }


                # Post-test probability calculations using Bayes' theorem.
                #
                # WRONG BEFORE: with a zero cell these used the Haldane-Anscombe
                # CORRECTED proportions while the Sensitivity/Specificity cells printed
                # on the same row stayed on the OBSERVED counts, so a single row read
                # "Sens 83.3% | Spec 100.0% | Prevalence 10.0% | PPV 77.3%" -- which
                # cannot all be true at once, because a specificity of exactly 1 forces
                # PPV = 1 at any prior.
                #
                # One row, one computation: sensitivity, specificity and the predictive
                # values all come from the observed counts. The continuity correction is
                # confined to LR+/LR-/DOR and to the Fagan nomogram (nomogrammer rejects
                # a proportion of exactly 0 or 1 outright), and that is now stated in the
                # notice and in the table note rather than left to be inferred.
                # Neither denominator can be 0: that needs Sens 0 with Spec 1 (no positive
                # test) or Sens 1 with Spec 0 (no negative test), and the prior is in (0, 1).
                PostTestProbDisease <- (PriorProb * Sens) / ((PriorProb * Sens) + ((1 - PriorProb) * (1 - Spec)))
                # NPV at the prior: the probability of no disease given a negative test
                PostTestProbHealthy <- ((1 - PriorProb) * Spec) / (((1 - PriorProb) * Spec) + (PriorProb * (1 - Sens)))




                # Likelihood ratios. On a zero cell only a ratio whose OWN formula contains
                # the zero count is taken from the Haldane-Anscombe corrected table: LR+ uses
                # TP and FP, LR- uses FN and TN. Correcting a ratio that is well defined on
                # the observed counts moved it for nothing - TP 20, FP 2, FN 0, TN 19 has an
                # observed LR+ of 10.5 ("large") that the blanket correction turned into 8.59
                # ("moderate"), under a note saying it would otherwise be undefined.
                lrp_cc_used <- isTRUE(continuity_used) && (TP == 0 || FP == 0)
                lrn_cc_used <- isTRUE(continuity_used) && (FN == 0 || TN == 0)
                # The DOR is always taken from the corrected table on a zero cell, so when only
                # ONE likelihood ratio is corrected the displayed DOR and LR+/LR- come from two
                # different tables and need not satisfy DOR = LR+/LR- (TP 20, FP 0, FN 5, TN 15:
                # DOR 115.5 against 25.2/0.20 = 126.2). Both notes that state the identity say so.
                mixed_lr_correction <- xor(lrp_cc_used, lrn_cc_used)
                # A ratio that is not corrected has no zero in its own formula, so it is finite.
                LRP <- if (lrp_cc_used) (TPc / (TPc + FNc)) / (FPc / (FPc + TNc)) else Sens / (1 - Spec)
                LRN <- if (lrn_cc_used) (FNc / (TPc + FNc)) / (TNc / (FPc + TNc)) else (1 - Sens) / Spec
                # Sign of the observed Youden's index, from the counts (exact integers): the
                # direction every likelihood-ratio sentence must agree with (.lrBand).
                youden_direction <- sign(TP * TN - FP * FN)
                private$.youdenDirection <- youden_direction

                # .validateLikelihoodRatios() used to run here. It replaced a
                # non-finite LR with a fabricated finite one (sens / max(1 - spec, 0.001),
                # i.e. a silently invented number that looks exactly like a measured one)
                # and announced it as an adjustment "for statistical validity". Every
                # branch of it was also unreachable: a zero cell triggers the correction
                # above, so 0 < sens_cc < 1 and 0 < spec_cc < 1 and both LRs are finite
                # and positive; and the 2x2 structure check in .prepareAnalysisData()
                # means Sens/Spec are never NA here. Deleted rather than left one guard
                # relaxation away from printing an invented likelihood ratio.

                if (continuity_used) {
                    private$.addNotice(
                        type = "INFO",
                        refs = c("haldane1956", "anscombe1956"),
                        title = .("Continuity correction applied"),
                        content = .("A cell of the 2x2 table is zero. Each likelihood ratio that would be zero, infinite or undefined on the observed counts, and the diagnostic odds ratio, are computed from the Haldane-Anscombe corrected table (0.5 added to every cell). Sensitivity, specificity, accuracy, the predictive values and any other likelihood ratio are not corrected. The Fagan nomogram shows the table's likelihood ratios when one is above 1 and the other below 1; otherwise it takes both from the corrected table. The two sets therefore do not reconcile exactly: a non-zero, finite likelihood ratio can sit beside a sensitivity or specificity of 0% or 100%.")
                    )
                }






                # nTable Populate Table ----

                nTable <- self$results$nTable
                nTable$setNote("independence", private$.independenceText())
                nTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "",
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
                # PPV/NPV must belong to the prevalence the table reports.
                #
                # PPV *is* the post-test probability of disease given a positive
                # test. With pp=TRUE the Prevalence cell was switched to the user's
                # prior while PPV/NPV stayed at the raw 2x2 values computed at the
                # STUDY prevalence, so the row read "Prevalence 5.0%" beside
                # "PPV 88.9%" - arithmetically impossible for a test with sens 0.80
                # / spec 0.90, where Bayes gives 29.6%. The footnote made it worse
                # by asserting the predictive values had been prior-adjusted. Report
                # the values that actually correspond to the stated prevalence, and
                # keep the observed study prevalence visible either way.
                PPV_report <- PPV
                NPV_report <- NPV
                if (isTRUE(pp)) {
                    PPV_report <- PostTestProbDisease
                    NPV_report <- PostTestProbHealthy
                    ratioTable$setNote("prior_ppv", .fmt(
                        .("Predictive values are computed by Bayes' theorem at the population prior of {prior} that you supplied, NOT at this sample's observed prevalence of {observed}. Sensitivity and specificity are carried over unchanged, which assumes the target population has the same case mix (spectrum) as this sample; PPV and NPV are recomputed."),
                        prior = sprintf("%.1f%%", 100 * PriorProb),
                        observed = sprintf("%.1f%%", 100 * PrevalenceD)))
                } else {
                    ratioTable$setNote("study_ppv", .fmt(
                        .("Predictive values are computed at this sample's observed prevalence of {prevalence}. If the sample was enriched or case-control, that prevalence is not the clinical one and these predictive values do not transfer - tick Known population prevalence under Population Prevalence Settings to obtain values for your setting."),
                        prevalence = sprintf("%.1f%%", 100 * PrevalenceD)))
                }

                # The likelihood ratios on this row come from a different table than the
                # proportions beside them whenever a cell is zero. That has to be said on
                # the table, not only in the notices pane: a reader who sees Spec 100.0%
                # next to a finite LR+ will otherwise assume one of them is a typo.
                if (isTRUE(continuity_used))
                    ratioTable$setNote("continuity", .("A cell of the 2x2 table is zero. A likelihood ratio that would be zero, infinite or undefined on the observed counts is computed from the Haldane-Anscombe corrected table (0.5 added to every cell) and reflects that correction, not the observed data: the positive likelihood ratio when sensitivity is 0% or specificity is 100%, the negative likelihood ratio when sensitivity is 100% or specificity is 0%. Sensitivity, specificity, accuracy, the predictive values and any other likelihood ratio are not corrected."))

                # Sample accuracy stays on the observed 2x2 even when a population
                # prior is supplied -- it is a property of THIS sample's case mix, not a
                # quantity that can be standardised to another prevalence without also
                # restating what "accuracy" means. decisioncalculator and decisioncompare
                # already settled this the same way (jamovi/decisioncalculator.r.yaml:145,
                # R/decisioncompare.b.R:2284), and recomputing it here would make the two
                # analyses print different accuracies for the same 2x2 and the same prior.
                # setNote, not addFootnote: .addFootnotes() returns early unless the user
                # ticks Explanatory footnotes, and a value this easy to misread cannot have
                # its only explanation behind an off-by-default switch.
                ratioTable$setNote("sample_accuracy", .fmt(
                    if (isTRUE(pp))
                        .("Sample Accuracy is the proportion of concordant results at the prevalence observed in this sample ({prevalence}), NOT at the population prior you supplied. Unlike the predictive values it is not standardised to that prior, and it changes with case mix.")
                    else
                        .("Sample Accuracy is the proportion of concordant results at the prevalence observed in this sample ({prevalence}), and it changes with case mix."),
                    prevalence = sprintf("%.1f%%", PrevalenceD * 100)))

                ratioTable$setRow(
                    rowNo = 1,
                    values = list(
                        tablename = "",
                        Sens = Sens,
                        Spec = Spec,
                        AccurT = AccurT,
                        PrevalenceD = PriorProb,
                        PPV = PPV_report,
                        NPV = NPV_report,
                        LRP = LRP,
                        LRN = LRN
                    )
                )

                # Consolidated content generation with enhanced error handling.
                # Pass RAW variable names here: the content generators
                # (.generateNaturalLanguageSummary / .generateReportTemplate) escape them
                # internally via .safeHtmlOutput. Escaping here too would double-encode names
                # containing &, <, >, or quotes (e.g. `A&B` -> `A&amp;B`).
                test_label <- if (length(self$options$newtest) > 0) {
                    paste(self$options$newtest, collapse = ", ")
                } else {
                    "Test"
                }

                gold_label <- if (length(self$options$gold) > 0) {
                    paste(self$options$gold, collapse = ", ")
                } else {
                    "Reference"
                }

                # Generate content only if requested by user
                content_results <- NULL
                if (self$options$showNaturalLanguage || self$options$showClinicalInterpretation ||
                    self$options$showReportTemplate || self$options$showAboutAnalysis) {
                    # PPV_report/NPV_report, not PPV/NPV: with pp=TRUE the narrative
                    # must quote the predictive values at the prior it also prints.
                    # A copy-ready sentence without an interval invites a clinician to
                    # quote a point estimate as if it were exact. Clopper-Pearson, to
                    # match epiR::epi.tests()'s default.
                    exact_ci <- function(x, n) {
                        if (!is.finite(x) || !is.finite(n) || n <= 0 || x < 0 || x > n) return(NULL)
                        tryCatch(as.numeric(stats::binom.test(round(x), round(n))$conf.int),
                                 error = function(e) NULL)
                    }
                    sens_ci <- exact_ci(TP, TP + FN)
                    spec_ci <- exact_ci(TN, TN + FP)
                    # Report extras (STARD 2015): N, both arms, and an interval for every
                    # quoted estimate. PPV/NPV intervals exist only at the sample
                    # prevalence; with a population prior the report says so instead. The
                    # LR+ interval uses the same (corrected, when a cell is zero) counts as
                    # the LR+ it sits beside.
                    # Cases excluded before the 2x2 (STARD 2015 items 15-16 and 20): missing or
                    # explicit-missing results, and levels other than the two selected. The
                    # paragraph quoted only the analysed n, while the exclusion notices told the
                    # user to report these. Same split as the Data Quality Summary.
                    n_dropped <- nrow(self$data) - TotalPop
                    n_level <- min(max(private$.n_level_excluded, 0), n_dropped)
                    report_extras <- list(
                        n_rows = nrow(self$data),
                        n_missing = n_dropped - n_level,
                        n_level = n_level,
                        n_diseased = DiseaseP,
                        n_healthy = DiseaseN,
                        pp = isTRUE(pp),
                        ppv_ci = if (isTRUE(pp)) NULL else exact_ci(TP, TP + FP),
                        npv_ci = if (isTRUE(pp)) NULL else exact_ci(TN, TN + FN),
                        lr_pos_ci = if (lrp_cc_used) private$.lrPosCI(TPc, FPc, FNc, TNc)
                                    else private$.lrPosCI(TP, FP, FN, TN))

                    content_results <- private$.generateAllContent(Sens, Spec, PPV_report, NPV_report, LRP, LRN,
                                                                  PriorProb, TotalPop, test_label, gold_label,
                                                                  report_extras = report_extras,
                                                                  sens_ci = sens_ci, spec_ci = spec_ci,
                                                                  continuity_used = continuity_used)
                }

                # Populate content outputs based on user selections
                if (self$options$showClinicalInterpretation && !is.null(content_results) &&
                    "clinicalInterpretation" %in% names(self$results)) {
                    self$results$clinicalInterpretation$setContent(content_results$clinical_summary)
                }

                if (self$options$showAboutAnalysis && !is.null(content_results) &&
                    "aboutAnalysis" %in% names(self$results)) {
                    self$results$aboutAnalysis$setContent(content_results$about_content)
                }

                if (self$options$showNaturalLanguage && !is.null(content_results) &&
                    "naturalLanguageSummary" %in% names(self$results)) {
                    self$results$naturalLanguageSummary$setContent(content_results$natural_summary)
                }

                if (self$options$showReportTemplate && !is.null(content_results) &&
                    "reportTemplate" %in% names(self$results)) {
                    self$results$reportTemplate$setContent(content_results$report_template)
                }
                
                # Detect misuse. Emits notices directly, so there is nothing to
                # splice into an opt-in HTML panel any more.
                private$.detectMisuse(conf_table, PrevalenceD, TotalPop)
                private$.validateDiscrimination(Sens, Spec, private$.youdenCI(TP, TP + FN, FP, FP + TN))

                # Misclassified Cases Analysis and Output
                tryCatch({
                    # Helper to check if output variable is properly specified
                    # isTRUE, not nchar(): an Output option's value is a LOGICAL, and
                    # nchar(FALSE) is 5, so the old guard was satisfied whether or not the
                    # user asked for the column.
                    has_output_var <- isTRUE(self$options$saveClassifications)

                    # Always create classifications if output is requested or analysis is shown
                    if (self$options$showMisclassified || has_output_var) {
                        private$.analyzeMisclassifiedCases(mydata, goldVariable, testVariable)
                    }
                }, error = function(e) {
                    # .analyzeMisclassifiedCases() calls private$.checkpoint() inside both
                    # addRow loops, and .checkpoint() signals its restart by stop()ing with
                    # a condition carrying code == "restart". Caught here it became a red
                    # "Technical details: restarting" panel over half-filled tables, and the
                    # restart never reached the engine, so the run also finished against the
                    # options the user had just changed. Control flow, not a failure:
                    # re-raise it before anything is rendered.
                    if (identical(e$code, "restart")) stop(e)
                    private$.addNotice(
                        type = "ERROR",
                        title = .("Error in misclassified cases analysis"),
                        content = sprintf(.("Technical details: %s Please report this issue if it persists."), e$message)
                    )
                })

                # Add footnotes using centralized method
                private$.addFootnotes()





                # 95% CI ----

                ci <- self$options$ci

                if (ci) {
                    # addRow() appends and accepts a duplicate rowKey, and clearWith
                    # cannot see a variable change that leaves pp/pprob untouched, so
                    # without this the CI tables list every statistic twice on re-run.
                    # Cleared before the tryCatch so a failed epiR call leaves the
                    # tables empty rather than showing the previous run's numbers.
                    # The rows are the fixed statistic set scaffolded in .init(); clearing them here
                    # would delete the scaffold and make the table appear only once epiR returns.
                    # Blank the values instead, so a failed epiR leaves empty cells, not a missing table.
                    #
                    # statsnames is re-set with the same label it already holds, purely for
                    # its side effect: Cell$setValue() resets that cell's footnotes. .init()
                    # runs before the previous run's results are restored from protobuf, and
                    # Cell$fromProtoBuf() brings the old footnotes back, so the statsnames
                    # footnotes added below would otherwise pile up one copy per run and would
                    # never disappear when the user unticks Explanatory footnotes (fnote is
                    # not in this table's clearWith). The est/lower/upper cells already
                    # self-clear this way; statsnames was the only column left out.
                    epir_labels <- private$.epirStatLabels()
                    for (key in private$.epirRatioStats())
                        self$results$epirTable_ratio$setRow(rowKey = key,
                            values = list(statsnames = unname(epir_labels[[key]]),
                                          est = NA_real_, lower = NA_real_, upper = NA_real_))
                    for (key in private$.epirNumberStats())
                        self$results$epirTable_number$setRow(rowKey = key,
                            values = list(statsnames = unname(epir_labels[[key]]),
                                          est = NA_real_, lower = NA_real_, upper = NA_real_))
                    # Conditional notes are re-derived below on every run; a note from a
                    # previous run must not outlive the numbers it explained.
                    for (key in c("nndx_undefined", "nndx_unbounded"))
                        self$results$epirTable_number$setNote(key, NULL)

                    # With a population prior supplied, the main table's PPV/NPV are Bayes
                    # values at that prior while these rows are exact binomial quantities
                    # at the SAMPLE prevalence. Both are labelled "Positive predictive
                    # value" and both render as percentages, so a reader will otherwise
                    # quote the headline estimate with this interval - an interval that
                    # does not contain it. setNote, not addFootnote: addFootnote only runs
                    # when the off-by-default Explanatory footnotes box is ticked.
                    if (isTRUE(self$options$pp))
                        self$results$epirTable_ratio$setNote("pv_prevalence", .fmt(
                            .("The predictive values in this table are computed at the prevalence observed in this sample ({observed}), not at the population prior of {prior} used for the predictive values in the main table above. The intervals here belong to these estimates, not to those. Sensitivity and specificity are the same in both tables."),
                            observed = sprintf("%.1f%%", 100 * PrevalenceD),
                            prior = sprintf("%.1f%%", 100 * PriorProb)))

                    # epiR confidence intervals with error handling
                    epir_success <- FALSE
                    epir_error_msg <- ""
                    epirresult_ratio <- NULL
                    epirresult_number <- NULL
                    epirresult_ratio_stats <- character(0)
                    epirresult_number_stats <- character(0)

                    tryCatch({
                        epirresult <- epiR::epi.tests(dat = conf_table)
                        # LR+/LR-/DOR in the main tables are computed from Haldane-Anscombe
                        # corrected counts when a cell is zero. epi.tests on the raw table
                        # returns Inf with a NaN lower bound, so the CI pane disagreed with
                        # the estimate it sat next to. Sens/spec/PPV/NPV stay on raw counts.
                        epir_detail_cc <- if (continuity_used) {
                            tryCatch(as.data.frame(epiR::epi.tests(dat = conf_table_cc)$detail,
                                                   stringsAsFactors = FALSE),
                                     error = function(e) NULL)
                        } else NULL

                        # Check if epiR returned valid results
                        if (!is.null(epirresult)) {
                            epir_detail <- epirresult$detail

                            if (!is.null(epir_detail) && nrow(epir_detail) > 0) {
                                epir_detail <- as.data.frame(epir_detail, stringsAsFactors = FALSE)

                                stat_map <- private$.epirStatLabels()

                                selected_stats <- names(stat_map)
                                epir_detail <- epir_detail[epir_detail$statistic %in% selected_stats, , drop = FALSE]

                                if (nrow(epir_detail) > 0) {
                                    order_index <- match(epir_detail$statistic, selected_stats)
                                    epir_detail <- epir_detail[order(order_index), , drop = FALSE]
                                    epir_detail$statsnames <- unname(stat_map[match(epir_detail$statistic, selected_stats)])

                                    ratio_stats <- private$.epirRatioStats()
                                    number_stats <- private$.epirNumberStats()

                                    epir_ratio <- epir_detail[epir_detail$statistic %in% ratio_stats, , drop = FALSE]
                                    epir_number <- epir_detail[epir_detail$statistic %in% number_stats, , drop = FALSE]

                                    # Only the ratio-of-ratios statistics blow up on a zero
                                    # cell (Inf estimate, NaN bound). Youden and NNDx stay
                                    # finite on the raw table, and splicing corrected values
                                    # into them made this pane contradict both the Se/Sp in
                                    # the table above it and the Youden in the Clinical
                                    # Interpretation panel (Se+Sp-1).
                                    # Only the rows whose own formula contains the zero count
                                    # (the same rule as the main table: LR+ on TP/FP, LR- on
                                    # FN/TN; the DOR uses every cell).
                                    cc_stats <- c(if (lrp_cc_used) "lr.pos", if (lrn_cc_used) "lr.neg", "diag.or")
                                    if (!is.null(epir_detail_cc) && nrow(epir_number) > 0) {
                                        m <- match(epir_number$statistic, epir_detail_cc$statistic)
                                        keep <- !is.na(m) & epir_number$statistic %in% cc_stats
                                        if (any(keep))
                                            epir_number[keep, c("est", "lower", "upper")] <-
                                                epir_detail_cc[m[keep], c("est", "lower", "upper")]
                                    }

                                    # Youden's index and NNDx: not epiR's. epiR's Youden interval
                                    # adds the two Clopper-Pearson bounds (99.8% coverage in
                                    # simulation, labelled 95%), and its NNDx interval takes
                                    # min/max of 1/bounds, which excludes its own estimate
                                    # whenever the Youden interval crosses 0 (6.67 shown as
                                    # -3.08 to 1.73) and prints a negative "number" for an
                                    # inverted test. Agresti-Caffo for Youden (a difference
                                    # of two independent proportions, see .youdenCI), NNDx by
                                    # inversion, NA where undefined or unbounded. Observed
                                    # counts: the zero-cell correction is for ratios only.
                                    youden_ci <- private$.youdenCI(TP, TP + FN, FP, FP + TN)
                                    nndx_ci <- private$.nndxFromYouden(youden_ci)
                                    iy <- which(epir_number$statistic == "youden")
                                    if (length(iy) == 1)
                                        epir_number[iy, c("est", "lower", "upper")] <- as.list(unname(youden_ci))
                                    ix <- which(epir_number$statistic == "nndx")
                                    if (length(ix) == 1)
                                        epir_number[ix, c("est", "lower", "upper")] <- as.list(unname(nndx_ci))

                                    # Values are written by rowKey, so each row's statistic has to
                                    # travel with the data.
                                    # as.character: epiR returns `statistic` as a factor, and a factor
                                    # element does not match a string rowKey.
                                    epirresult_ratio_stats <- as.character(epir_ratio$statistic)
                                    epirresult_number_stats <- as.character(epir_number$statistic)

                                    epir_ratio <- epir_ratio[, c("statsnames", "est", "lower", "upper"), drop = FALSE]
                                    epir_number <- epir_number[, c("statsnames", "est", "lower", "upper"), drop = FALSE]

                                    epirresult_ratio <- epir_ratio
                                    epirresult_number <- epir_number

                                    epir_success <- nrow(epir_ratio) > 0 || nrow(epir_number) > 0
                                }
                                # No `else` branches any more: every way of not producing
                                # intervals now falls through to the single notice below.
                            }
                        }

                    }, error = function(e) {
                        if (identical(e$code, "restart")) stop(e)
                        # `<<-`, not `<-`: an assignment inside an error handler writes to
                        # the HANDLER's frame, so the old `epir_success <- FALSE` here was
                        # dead code and the message was thrown away with it.
                        epir_success <<- FALSE
                        epir_error_msg <<- conditionMessage(e)
                    })

                    # The two CI tables are visible: (ci), so a failure left a pathologist
                    # looking at nine statistic names with every numeric cell blank and
                    # nothing on screen saying whether the intervals had failed or the data
                    # was unsuitable. The three "silently skip" branches above said so in
                    # comments; say it to the user instead.
                    if (!epir_success) {
                        private$.addNotice(
                            type = "WARNING",
                            title = .("95% confidence intervals could not be computed"),
                            # paste0, not .fmt: an epiR message is arbitrary runtime text
                            # and a stray brace in it would be re-scanned as a placeholder.
                            content = if (nzchar(epir_error_msg))
                                          paste0(.("The epiR package could not produce intervals for this table, so the two confidence-interval tables are empty."),
                                                 " ", .("Reported reason"), ": ", epir_error_msg)
                                      else
                                          .("The epiR package returned no interval estimates for this table, so the two confidence-interval tables are empty.")
                        )
                    }

                    # Only populate tables if we have valid data
                    if (epir_success) {
                        # epirTable_ratio -----
                        epirTable_ratio <- self$results$epirTable_ratio

                        if (!is.null(epirresult_ratio) && nrow(epirresult_ratio) > 0) {
                            data_frame <- epirresult_ratio
                            # rows were scaffolded in .init(): fill by key, never add
                            for (i in seq_len(nrow(data_frame))) {
                                key <- epirresult_ratio_stats[i]
                                if (key %in% private$.epirRatioStats())
                                    epirTable_ratio$setRow(rowKey = key,
                                        values = list(est = data_frame$est[i],
                                                      lower = data_frame$lower[i],
                                                      upper = data_frame$upper[i]))
                            }

                            # epirTable_ratio footnotes ----
                            if (self$options$fnote) {
                                add_ratio_note <- function(key, col, text) {
                                    epirTable_ratio$addFootnote(rowKey = key, col = col, note = text)
                                }

                                add_ratio_note("se", "statsnames", .("Proportion of diseased patients correctly identified (TP rate). When sensitivity is high, a negative result argues against disease, provided specificity is not low (Pewsner et al. 2004)."))
                                add_ratio_note("sp", "statsnames", .("Proportion of patients without the disease correctly identified (TN rate). When specificity is high, a positive result argues for disease, provided sensitivity is not low (Pewsner et al. 2004)."))
                                add_ratio_note("pv.pos", "statsnames", .("Probability of disease given a positive test. Depends on prevalence, sensitivity and specificity."))
                                add_ratio_note("pv.neg", "statsnames", .("Probability of being free of the target condition given a negative test. Depends on prevalence, sensitivity and specificity."))
                                add_ratio_note("se", "est", .("Confidence intervals for sensitivity, specificity, and predictive values are Clopper-Pearson exact intervals, computed as in epiR::epi.tests() with its default settings (method = \"exact\")."))
                                # The pv.pos/pv.neg prior-vs-sample disclosure used to live
                                # here. It is now an unconditional setNote below: two
                                # different numbers for "Positive predictive value" on one
                                # screen cannot have their only explanation behind a
                                # checkbox that is off by default (same reasoning as the
                                # sample_accuracy note on ratioTable).
                            }
                        }

                        # epirTable_number -----
                        epirTable_number <- self$results$epirTable_number

                        if (!is.null(epirresult_number) && nrow(epirresult_number) > 0) {
                            data_frame <- epirresult_number
                            for (i in seq_len(nrow(data_frame))) {
                                key <- epirresult_number_stats[i]
                                if (key %in% private$.epirNumberStats())
                                    epirTable_number$setRow(rowKey = key,
                                        values = list(est = data_frame$est[i],
                                                      lower = data_frame$lower[i],
                                                      upper = data_frame$upper[i]))
                            }

                            # Always-on (setNote, not the off-by-default footnotes): the
                            # Youden and NNDx intervals are not epiR's, and a blank NNDx
                            # cell needs its reason on screen.
                            epirTable_number$setNote("youden_method", .("Youden's index interval: Agresti-Caffo interval for the difference between the true-positive and false-positive rates (one positive and one negative result added to each group). Number needed to diagnose = 1 / Youden's index; its interval is the inverse of the positive part of the Youden interval."))
                            nndx_row <- epirresult_number[epirresult_number_stats == "nndx", , drop = FALSE]
                            nndx_key <- if (nrow(nndx_row) != 1) "none"
                                        else if (is.na(nndx_row$est) && is.na(nndx_row$lower)) "absent"
                                        else if (is.na(nndx_row$est)) "no_estimate"
                                        else if (is.na(nndx_row$upper)) "unbounded"
                                        else "none"
                            epirTable_number$setNote("nndx_undefined", switch(nndx_key,
                                absent = .("The number needed to diagnose is not shown: Youden's index and its whole confidence interval are at or below 0."),
                                no_estimate = .("Youden's index is 0 or below in this sample, so the number needed to diagnose has no point estimate. Its confidence interval starts at the lower limit shown and has no upper bound."),
                                NULL))
                            epirTable_number$setNote("nndx_unbounded", if (identical(nndx_key, "unbounded"))
                                .("The upper limit of the number needed to diagnose is not shown: the Youden's index interval includes 0, so the interval for its inverse has no upper bound.") else NULL)
                            # The LR / DOR rows of THIS table come from the corrected table on a
                            # zero cell too; the note used to live only on the main ratio table.
                            epirTable_number$setNote("continuity",
                                if (isTRUE(mixed_lr_correction))
                                    .("A cell of the 2x2 table is zero. A likelihood ratio that would be zero, infinite or undefined on the observed counts, and the diagnostic odds ratio, are computed from the Haldane-Anscombe corrected table (0.5 added to every cell). The other rows use the observed counts. Because only one likelihood ratio is corrected, the diagnostic odds ratio shown need not equal LR+ / LR- as displayed.")
                                else if (isTRUE(continuity_used))
                                    .("A cell of the 2x2 table is zero. A likelihood ratio whose formula contains the zero count, and the diagnostic odds ratio, are computed from the Haldane-Anscombe corrected table (0.5 added to every cell), because on the observed counts they would be zero or infinite. The other rows use the observed counts.")
                                else NULL)

                            if (self$options$fnote) {
                                # These rows are ordered LR+, LR-, DOR, Youden, NNDx. Attaching by
                                # row number described LR+ as the diagnostic odds ratio.
                                number_notes <- c(
                                    `lr.pos` = .("How much more likely a positive result is in a patient with the disease than in one without it. >10 is strong evidence FOR disease, 5-10 moderate, 2-5 weak."),
                                    `lr.neg` = .("The chance of a negative result in patients with the disease divided by the chance in patients without it. <0.1 is strong evidence AGAINST disease, 0.1-0.2 moderate, 0.2-0.5 weak."),
                                    # The old text (epiR's help wording) described TP/FN - the odds
                                    # of a correct result in the diseased only - not the DOR.
                                    `diag.or` = if (isTRUE(mixed_lr_correction))
                                        .("Diagnostic odds ratio: the odds of a positive test in patients with the disease divided by the odds of a positive test in patients without it. On one 2x2 table it equals LR+ / LR- (Glas et al. 2003); here it and only one of the likelihood ratios come from the zero-cell corrected table, so the displayed values need not satisfy that identity.")
                                    else
                                        .("Diagnostic odds ratio: the odds of a positive test in patients with the disease divided by the odds of a positive test in patients without it, equal to LR+ / LR- (Glas et al. 2003)."),
                                    # epiR's wording ("to give one correct positive test") is not
                                    # the definition, and "patients WITH the disease examined per
                                    # correct detection" (Linn & Grunau's abstract shorthand) is
                                    # 1/sensitivity. NND = 1/J: n * sens - n * (1 - spec) = 1 at
                                    # n = NND. Origin Bandolier (1996); Linn & Grunau (2006) note
                                    # that it ignores prevalence.
                                    `nndx` = .("Number needed to diagnose = 1 / Youden's index: testing this many patients with the disease and the same number without it yields, on average, one more true-positive than false-positive result (Linn and Grunau 2006). It does not depend on prevalence, so it says little about how the test performs in a particular clinical setting."),
                                    `youden` = .("Youden's index is the difference between the true positive rate and the false positive rate. Youden's index ranges from -1 to +1 with values closer to 1 if both sensitivity and specificity are high (i.e. close to 1).")
                                )

                                for (key in names(number_notes))
                                    if (key %in% private$.epirNumberStats())
                                        epirTable_number$addFootnote(rowKey = key, col = "statsnames",
                                                                     note = number_notes[[key]])
                            }
                        }
                    }
                }





                # Prepare Fagan Nomogram Data ----
                if (self$options$fagan) {
                    # A zero cell makes Sens or Spec exactly 1, and nomogrammer rejects
                    # the closed bounds outright ("must be between 0 and 1 (exclusive)"),
                    # so the whole nomogram silently failed to render. The LRs handed to
                    # it are already Haldane-Anscombe corrected; pass the proportions
                    # from the same corrected table so the plot is self-consistent
                    # rather than clamping to an arbitrary epsilon.
                    sens_plot <- Sens
                    spec_plot <- Spec
                    if (isTRUE(continuity_used)) {
                        # nomogrammer re-derives both ratios from Sens/Spec and rejects 0 and
                        # 1. Back-solve the proportions whose ratios ARE the table's LR+ and
                        # LR- (spec = (LR+ - 1)/(LR+ - LR-), sens = LR+ (1 - spec)), so the
                        # figure quotes the same likelihood ratios as the tables; fall back
                        # to the corrected proportions when no such pair exists.
                        sp_b <- (LRP - 1) / (LRP - LRN)
                        se_b <- LRP * (1 - sp_b)
                        ok_b <- all(is.finite(c(se_b, sp_b))) && se_b > 0 && se_b < 1 && sp_b > 0 && sp_b < 1
                        sens_plot <- if (ok_b) se_b else TPc / (TPc + FNc)
                        spec_plot <- if (ok_b) sp_b else TNc / (TNc + FPc)
                    }

                    plotData1 <- list(
                        "Prevalence" = PriorProb,
                        "Sens" = sens_plot,
                        "Spec" = spec_plot,
                        "Plr" = LRP,
                        "Nlr" = LRN
                    )

                    image1 <- self$results$plot1
                    image1$setState(plotData1)
                }

                # Notices are rendered by the on.exit() handler registered at the
                # top of .run(), so every exit path gets them -- not just this one.

            },

            # Robust plot data validation
            .validatePlotState = function(state) {
                required_fields <- c("Prevalence", "Plr", "Nlr", "Sens", "Spec")

                if (is.null(state)) {
                    return(NULL)
                }

                missing_fields <- setdiff(required_fields, names(state))
                if (length(missing_fields) > 0) {
                    return(NULL)
                }

                # Validate numeric ranges
                numeric_fields <- c("Prevalence", "Sens", "Spec")
                for (field in numeric_fields) {
                    if (!is.numeric(state[[field]]) ||
                        state[[field]] < 0 || state[[field]] > 1) {
                        # Plot state validation - returns NULL to prevent plotting
                        # warning(paste("Invalid", field, "value:", state[[field]]))
                        state[[field]] <- max(0, min(1, as.numeric(state[[field]])))
                    }
                }

                return(state)
            }

            ,
            .plot1 = function(image1, ggtheme, ...) {
                # Validate plot state data structure
                plotData1 <- private$.validatePlotState(image1$state)

                if (is.null(plotData1)) {
                    # Return FALSE to prevent plot rendering
                    return(FALSE)
                }

                # nomogrammer() lives in R/utils-nomogrammer.R and ships with every module
                # that ships this analysis, so there is no fallback plot to maintain.
                plot1 <- nomogrammer(
                    Prevalence = plotData1$Prevalence,
                    Sens = plotData1$Sens,
                    Spec = plotData1$Spec,
                    # Plr/Nlr are deliberately omitted: nomogrammer ignores them when
                    # Sens/Spec are supplied and warns on every render. It derives the
                    # same ratios from the (continuity-consistent) proportions above.
                    Detail = TRUE,
                    NullLine = TRUE,
                    LabelSize = private$NOMOGRAM_LABEL_SIZE,
                    # FALSE: Verbose = TRUE cat()s a seven-line untranslated English
                    # block to stdout on every render, resize and .omv reopen. The same
                    # numbers are already drawn on the nomogram.
                    Verbose = FALSE,
                    # jamovi's global theme (nomogrammer layers its structural theme on
                    # top of it), and the figure's own text translated here: nomogrammer
                    # is file-level, so `.()` cannot run inside it. Same as cotest.b.R.
                    Theme = ggtheme,
                    Title = .("Fagan Nomogram"),
                    PriorLabel = .("Prior\nProb.\n(%)"),
                    PosteriorLabel = .("Posterior\nProb.\n(%)"),
                    DetailLabels = c(prevalence = .("Prevalence"), plr = .("PLR"), nlr = .("NLR"),
                                     post_pos = .("Post(+)"), post_neg = .("Post(-)"))
                )

                print(plot1)
                TRUE

            },

            # Misclassified Cases Analysis ----
            # Inspired by Orange Data Mining's interactive confusion matrix
            # Adapted for static jamovi output

            .analyzeMisclassifiedCases = function(mydata2, gold_var, test_var) {

                # Get levels
                gold_pos <- self$options$goldPositive
                test_pos <- self$options$testPositive

                # Create classification groups for all data rows (not just complete cases)
                # Initialize with NA for all rows in original dataset
                classification_vector <- rep(NA_character_, nrow(self$data))

                # Map complete case indices back to original data positions.
                # Use the purpose-built `original_row_index` column (captured before naOmit
                # in .prepareAnalysisData), NOT rownames(): dplyr/factor operations upstream
                # can reset data-frame row names to 1:n, which would write the saved TP/FP/FN/TN
                # column to the wrong original rows when missing values are interspersed.
                # The FP/FN display tables already use original_row_index for the same reason.
                # Positional index into self$data -- deliberately NOT original_row_index,
                # which now carries absolute spreadsheet row numbers for display. The
                # Output column is written positionally and aligned by setRowNums().
                complete_indices <- if (!is.null(mydata2$original_row_position)) {
                    mydata2$original_row_position
                } else {
                    as.numeric(rownames(mydata2))
                }

                # Create classification groups for complete cases
                mydata2$classification_group <- NA_character_

                # True Positive: Test+ and Disease+
                tp_idx <- mydata2[[test_var]] == test_pos & mydata2[[gold_var]] == gold_pos
                mydata2$classification_group[tp_idx] <- "True Positive"

                # False Positive: Test+ but Disease-
                fp_idx <- mydata2[[test_var]] == test_pos & mydata2[[gold_var]] != gold_pos
                mydata2$classification_group[fp_idx] <- "False Positive"

                # False Negative: Test- but Disease+
                fn_idx <- mydata2[[test_var]] != test_pos & mydata2[[gold_var]] == gold_pos
                mydata2$classification_group[fn_idx] <- "False Negative"

                # True Negative: Test- and Disease-
                tn_idx <- mydata2[[test_var]] != test_pos & mydata2[[gold_var]] != gold_pos
                mydata2$classification_group[tn_idx] <- "True Negative"

                # Map complete case classifications back to original data positions
                if (length(complete_indices) > 0) {
                    classification_vector[complete_indices] <- mydata2$classification_group
                }

                # Save classifications to dataset if output variable is specified
                if (isTRUE(self$options$saveClassifications)) {
                    output <- self$results$saveClassifications
                    if (!is.null(output) && is.function(output$setValues)) {
                        # setRowNums BEFORE setValues. classification_vector is built
                        # positionally over self$data, which under an active jamovi row
                        # filter is a SUBSET of the spreadsheet -- without the row
                        # identities the TP/FP/FN/TN labels are written to the wrong
                        # patients. Same pattern as categorize.b.R:982 and agreement.b.R:8506.
                        output$setRowNums(rownames(self$data))
                        output$setValues(classification_vector)
                    }
                }

                # Only populate tables if user requested to see misclassified cases
                if (!self$options$showMisclassified) {
                    return()
                }

                # Populate the misclassified-cases section heading/intro block.
                # (Declared in decision.r.yaml as `misclassifiedHeading`, visible when
                # showMisclassified; previously never set anywhere in the backend.)
                self$results$misclassifiedHeading$setContent(paste0(
                    "<div style='margin: 15px; padding: 12px; border-left: 4px solid #607D8B; background-color: rgba(33, 68, 92, 0.09); color: inherit;'>",
                    "<h3 style='color: inherit; margin-top: 0;'>", .("Misclassified Cases Analysis"), "</h3>",
                    "<p>", .("Cases where the diagnostic test disagreed with the gold standard are examined below. False positives (test positive, disease absent) and false negatives (test negative, disease present) are listed with their original row numbers so individual records can be reviewed."), "</p>",
                    "</div>"
                ))

                # Summary counts
                n_total <- nrow(mydata2)
                n_tp <- sum(tp_idx, na.rm = TRUE)
                n_fp <- sum(fp_idx, na.rm = TRUE)
                n_fn <- sum(fn_idx, na.rm = TRUE)
                n_tn <- sum(tn_idx, na.rm = TRUE)

                # Populate confusion matrix summary
                summary_table <- self$results$confusionMatrixSummary
                summary_table$setRow(rowKey = 1, values = list(
                    count = n_tp,
                    percentage = n_tp / n_total
                ))
                summary_table$setRow(rowKey = 2, values = list(
                    count = n_fp,
                    percentage = n_fp / n_total
                ))
                summary_table$setRow(rowKey = 3, values = list(
                    count = n_fn,
                    percentage = n_fn / n_total
                ))
                summary_table$setRow(rowKey = 4, values = list(
                    count = n_tn,
                    percentage = n_tn / n_total
                ))

                # False Positive cases table
                fp_table <- self$results$falsePositiveTable
                try(fp_table$deleteRows(), silent = TRUE)
                if (n_fp > 0) {
                    fp_cases <- mydata2[fp_idx, ]
                    # Use original row indices from the dataset, not filtered indices
                    fp_cases$row_id <- fp_cases$original_row_index

                    max_show <- min(self$options$maxCasesShow, nrow(fp_cases))

                    for (i in seq_len(max_show)) {
                        # flush = FALSE: the default re-serialises the whole results tree
                        # on every row (up to 500 per table) only to poll for a restart.
                        private$.checkpoint(flush = FALSE)
                        fp_table$addRow(rowKey = i, values = list(
                            case_id = fp_cases$row_id[i],
                            gold_value = private$.richText(fp_cases[[gold_var]][i]),
                            test_value = private$.richText(fp_cases[[test_var]][i])
                        ))
                    }

                    if (nrow(fp_cases) > max_show) {
                        note_text <- .fmt(.("Showing first {shown} of {total} false positive cases"),
                                           shown = max_show, total = nrow(fp_cases))
                        fp_table$setNote("truncated", note_text)
                    }
                }

                # False Negative cases table
                fn_table <- self$results$falseNegativeTable
                try(fn_table$deleteRows(), silent = TRUE)
                if (n_fn > 0) {
                    fn_cases <- mydata2[fn_idx, ]
                    # Use original row indices from the dataset, not filtered indices
                    fn_cases$row_id <- fn_cases$original_row_index

                    max_show <- min(self$options$maxCasesShow, nrow(fn_cases))

                    for (i in seq_len(max_show)) {
                        private$.checkpoint(flush = FALSE)
                        fn_table$addRow(rowKey = i, values = list(
                            case_id = fn_cases$row_id[i],
                            gold_value = private$.richText(fn_cases[[gold_var]][i]),
                            test_value = private$.richText(fn_cases[[test_var]][i])
                        ))
                    }

                    if (nrow(fn_cases) > max_show) {
                        note_text <- .fmt(.("Showing first {shown} of {total} false negative cases"),
                                           shown = max_show, total = nrow(fn_cases))
                        fn_table$setNote("truncated", note_text)
                    }
                }

                # Interpretation
                private$.generateMisclassificationInterpretation(n_tp, n_fp, n_fn, n_tn)
            },

            .generateMisclassificationInterpretation = function(n_tp, n_fp, n_fn, n_tn) {

                total_errors <- n_fp + n_fn
                error_rate <- (total_errors / (n_tp + n_fp + n_fn + n_tn)) * 100

                fp_proportion <- if (total_errors > 0) (n_fp / total_errors) * 100 else 0
                fn_proportion <- if (total_errors > 0) (n_fn / total_errors) * 100 else 0

                html <- paste0("<h3>", .("Understanding Misclassifications"), "</h3>")
                if (total_errors == 0) {
                    self$results$misclassificationInterpretation$setContent(
                        paste0(html, "<p>", .("The test and the reference standard agree on every analysed case."), "</p>"))
                    return(invisible(NULL))
                }

                html <- paste0(html,
                    "<p><b>", .("Error Summary"), ":</b> ",
                    .fmt(.("{n} total misclassifications ({pct} error rate)"),
                                    n = total_errors, pct = sprintf("%.1f%%", error_rate)),
                    "</p>")

                html <- paste0(html, "<ul>")
                html <- paste0(html,
                    "<li><b>", .("False Positives"), ":</b> ",
                    .fmt(.("{n} cases ({pct} of errors) - Test incorrectly predicts disease"),
                                    n = n_fp, pct = sprintf("%.1f%%", fp_proportion)),
                    "</li>")
                html <- paste0(html,
                    "<li><b>", .("False Negatives"), ":</b> ",
                    .fmt(.("{n} cases ({pct} of errors) - Test misses actual disease"),
                                    n = n_fn, pct = sprintf("%.1f%%", fn_proportion)),
                    "</li>")
                html <- paste0(html, "</ul>")

                # Error balance (descriptive only)
                if (n_fp > n_fn) {
                    html <- paste0(html,
                        "<p><b>", .("Error Balance"), ":</b> ",
                        .("False positives outnumbered false negatives in this sample."), "</p>")
                } else if (n_fn > n_fp) {
                    html <- paste0(html,
                        "<p><b>", .("Error Balance"), ":</b> ",
                        .("False negatives outnumbered false positives in this sample."), "</p>")
                } else {
                    html <- paste0(html,
                        "<p><b>", .("Error Balance"), ":</b> ",
                        .("False positives and false negatives occurred equally often in this sample."), "</p>")
                }

                # Follow-up checks on the analysis itself
                html <- paste0(html, "<p><b>", .("Follow-up checks on this analysis"), ":</b></p><ul>")

                if (n_fp > 0) {
                    html <- paste0(html,
                        "<li>", .("Review false positive cases to identify common characteristics"), "</li>",
                        "<li>", .("Check that the level you chose under Test positive level is the one you meant: this analysis has no numeric cutpoint, it simply treats that level as a positive result, so swapping the test positive and test negative levels exchanges true positives with false negatives, and false positives with true negatives"), "</li>")
                }

                if (n_fn > 0) {
                    html <- paste0(html,
                        "<li>", .("Review false negative cases to understand what the test misses"), "</li>")
                }

                html <- paste0(html, "</ul>")

                self$results$misclassificationInterpretation$setContent(html)
            }


        )
    )
