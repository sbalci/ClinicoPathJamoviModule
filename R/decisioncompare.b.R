#' @title Compare Medical Decision Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#'
#' @return An \code{R6} class generator object for the \code{decisioncompareClass} backend; used internally by the jamovi analysis wrapper and not called directly.

decisioncompareClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "decisioncompareClass",
        inherit = decisioncompareBase,
        private = list(
            # Illustrative thresholds used only to scale the radar plot (the Jaeschke et al. 1994
            # bands). LR- is scored as 1/LR- against the same edges, so 0.5, 0.2 and 0.1 land
            # where 2, 5 and 10 do. These are examples, not clinical guides or patient-care
            # decision thresholds.
            LR_CLINICAL_THRESHOLDS = list(
                excellent_pos = 10,
                good_pos = 5,
                fair_pos = 2
            ),

            # Statistical significance thresholds
            P_THRESHOLD_STRONG = 0.001, # Strong statistical evidence
            P_THRESHOLD_SIGNIFICANT = 0.05, # Conventional alpha level

            # Sample size adequacy thresholds
            MIN_DISCORDANT_PAIRS = 10, # Below this a footnote discloses McNemar's low power (not a validity cut-off)
            ZERO_CELL_CONTINUITY = 0.5, # Continuity correction for LR stability

            # Cache for test results to avoid redundant calculations
            .cached_test_results = NULL,
            # Cache for processed data (needed by heatmap)
            .cached_processed_data = NULL,

            # Whether any statistical comparison reached significance (set in
            # .performStatisticalComparisons); used for honest report wording.
            .any_significant_comparison = NULL,
            # Cochran's Q global p-value (set in .performCochranQ for 3+ tests).
            .cochran_pvalue = NULL,
            .cochran_method_note = "",
            # Whether the pairwise McNemar p-values were Holm-adjusted (more than one computable
            # pair); the copy-ready Methods sentence must say so when they were.
            .holm_applied = NULL,
            # FALSE when no test had an estimable balanced accuracy (set by .findBestTest), so no
            # sentence may name a "highest balanced accuracy".
            .best_test_ranked = NULL,
            # TRUE when every computed McNemar pair had no discordant pairs: p = 1 there is a
            # convention, so the copy-ready Results section must not report a verdict.
            .mcnemar_uninformative = NULL,
            # One record per pair (first, second, b, c, n, informative, displayed p), set in
            # .performStatisticalComparisons, so every narrative panel can say which test a
            # significant comparison favours (IR-decisioncompare-01).
            .pair_results = NULL,
            # Paired differences and unadjusted per-class McNemar p-values per pair, set in
            # .calculateDifferences, for the copy-ready Report (IR-decisioncompare-02).
            .diff_results = NULL,

            # Escape variable names for safe display and table keys
            # Read an option that may not exist in the compiled .h.R yet. jmvcore
            # raises an error rather than returning NULL for an undeclared option,
            # so a bare self$options$goldNegative would crash every run between the
            # .a.yaml edit and the next jmvtools::prepare().
            .opt = function(name, fallback = NULL) {
                value <- tryCatch(self$options[[name]], error = function(e) NULL)
                if (is.null(value) || (is.character(value) && length(value) == 1 && !nzchar(value)))
                    return(fallback)
                value
            },

            # jmvcore's OptionVariable$valueAsSource returns the column name verbatim, so the
            # exported syntax read `gold = Golden Standart` -- not parseable R for any name with a
            # space or symbol, including the bundled histopathology example
            # (VAL-decisioncompare-03). Emit escaped character literals instead (the wrapper's
            # resolveQuo() resolves a string to the same column); level labels likewise, which
            # jmvcore quotes but does not escape. Same override as decisioncombine.
            .sourcifyOption = function(option) {
                quoted <- c("gold", "test1", "test2", "test3", "stratify",
                            "goldPositive", "goldNegative",
                            "test1Positive", "test1Negative", "test2Positive", "test2Negative",
                            "test3Positive", "test3Negative")
                value <- option$value
                if (option$name %in% quoted && is.character(value) &&
                    length(value) == 1L && nzchar(value)) {
                    return(paste0(option$name, " = ", encodeString(value, quote = '"')))
                }
                # Free-text options: jmvcore does not escape quotes or backslashes, so
                # `Consensus of 3 "expert" pathologists` would export unparseable syntax.
                free_text <- c("referenceDescription", "targetDefinition", "cohortRole",
                               "samplingDesign", "thresholdSource", "modelVersion", "analysisUnit")
                if (option$name %in% free_text && is.character(value) && length(value) == 1L &&
                    nzchar(value) && !identical(value, option$default)) {
                    return(paste0(option$name, " = ", encodeString(value, quote = '"')))
                }
                super$.sourcifyOption(option)
            },

            # Ensure that requested positive level exists in data.
            # The user's column name is shown RAW. .escapeVar() used to run
            # make.names(gsub("[^A-Za-z0-9_. -]", "_", x)) over it first, which rewrote
            # "Golden Standart" as "Golden.Standart" and destroyed any non-ASCII name
            # (Turkish "Sonuc" with a cedilla became "Sonu__"), so the one message whose
            # job is to point at the offending variable named a variable that does not
            # exist in the dataset. The escaping was also redundant: .renderNotices()
            # passes every title and content through .safeHtmlOutput().
            .assertLevelExists = function(x, level, var_name, test_name = NULL) {
                if (is.null(level) || level == "") {
                    missing_for <- if (is.null(test_name)) var_name
                        else paste0(test_name, " (", var_name, ")")

                    # i18n: the msgid must be a literal, so the variable name is
                    # interpolated outside it via a jmvcore::format placeholder.
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("Missing Positive Level"),
                        content = .fmt(jmvcore::.("No positive level supplied for {target}. Please select the level that represents a positive result in the variable selection panel."),
                            target = missing_for)
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                if (!level %in% levels(x)) {
                    label <- if (is.null(test_name)) var_name
                        else paste0(test_name, " (", var_name, ")")

                    # Placeholder names must not prefix one another (jmvcore::format
                    # partial-matches), hence `available` rather than `levels`.
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("Invalid Positive Level"),
                        content = .fmt(jmvcore::.("The positive level \"{chosen}\" was not found in {target}. Check spelling and capitalisation. Available levels: {available}."),
                            chosen = level, target = label,
                            available = paste(levels(x), collapse = ", "))
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }
            },

            # Build a complete 2x2 confusion matrix with zero-filled cells
            .buildConfusionMatrix = function(test_values, gold_values) {
                lvls <- c("Positive", "Negative")
                tab <- table(test_values, gold_values, useNA = "no")
                matrix <- matrix(0,
                    nrow = 2, ncol = 2,
                    dimnames = list(test = lvls, gold = lvls)
                )
                common_rows <- intersect(rownames(tab), lvls)
                common_cols <- intersect(colnames(tab), lvls)
                if (length(common_rows) > 0 && length(common_cols) > 0) {
                    matrix[common_rows, common_cols] <- tab[common_rows, common_cols, drop = FALSE]
                }
                return(matrix)
            },

            # Safe proportion helper that reports issues once per test/metric
            .computeRate = function(numerator, denominator, metric_label, test_label, warn = TRUE) {
                if (is.na(denominator) || denominator == 0) {
                    if (isTRUE(warn)) {
                        private$.addNotice(
                            type = "WARNING",
                            title = jmvcore::.("Metric Unavailable"),
                            content = .fmt(jmvcore::.("Unable to compute {metric} for {test}: denominator is zero. The result is set to NA."),
                                metric = metric_label, test = test_label
                            )
                        )
                    }
                    return(NA_real_)
                }
                return(numerator / denominator)
            },

            # Extract aligned test and gold vectors for pairwise comparisons
            .extractComparisonVectors = function(test_results, test1, test2) {
                list(
                    test1 = test_results[[test1]]$test_results,
                    test2 = test_results[[test2]]$test_results,
                    gold = test_results[[test1]]$gold_reference
                )
            },

            # Accuracy of two tests on the rows determinate for both tests and the reference
            # standard: the rows McNemar's test and the paired differences use. The winner
            # sentence must come from these rows. Each test's standalone accuracy uses its own
            # rows, and when the tests are missing on different cases the two can point in
            # opposite directions (VAL-decisioncompare-01). NULL when no row is paired.
            .pairedAccuracy = function(test_results, test1, test2) {
                comp <- private$.extractComparisonVectors(test_results, test1, test2)
                ok <- !is.na(comp$test1) & !is.na(comp$test2) & !is.na(comp$gold)
                if (!any(ok)) return(NULL)
                correct1 <- comp$test1[ok] == comp$gold[ok]
                correct2 <- comp$test2[ok] == comp$gold[ok]
                list(
                    n = sum(ok),
                    acc1 = mean(correct1),
                    acc2 = mean(correct2),
                    only1 = sum(correct1 & !correct2),
                    only2 = sum(!correct1 & correct2),
                    # The share of reference-positive cases among these rows: the weight overall
                    # accuracy gives to sensitivity (IR-decisioncompare-02).
                    prevalence = mean(comp$gold[ok] == "Positive"),
                    difference = pairedProportionDifference(x = as.integer(correct1), y = as.integer(correct2))
                )
            },

            # IR-decisioncompare-01. The pairs whose McNemar p, as the table shows it (Holm-adjusted
            # whenever more than one pair was tested), is below alpha. A pair without discordant pairs
            # has no test result and is never listed.
            .significantPairs = function() {
                Filter(function(x) isTRUE(x$informative) && isTRUE(x$p < private$P_THRESHOLD_SIGNIFICANT),
                       private$.pair_results %||% list())
            },

            # One sentence per significant pair, naming the test its discordant pairs favour, with the
            # p-value the McNemar table shows. character(0) when no pair is significant.
            .pairDirectionSentences = function() {
                holm <- isTRUE(private$.holm_applied)
                vapply(private$.significantPairs(), function(x) {
                    first_better <- x$b > x$c
                    better <- private$.safeHtmlOutput(if (first_better) x$first else x$second)
                    worse <- private$.safeHtmlOutput(if (first_better) x$second else x$first)
                    if (holm)
                        sprintf(jmvcore::.("%1$s classified significantly more cases correctly than %2$s: %3$d of the %4$d cases where exactly one of the two was correct (Holm-adjusted p %5$s)."),
                            better, worse, max(x$b, x$c), x$b + x$c, private$.formatP(x$p))
                    else
                        sprintf(jmvcore::.("%1$s classified significantly more cases correctly than %2$s: %3$d of the %4$d cases where exactly one of the two was correct (p %5$s)."),
                            better, worse, max(x$b, x$c), x$b + x$c, private$.formatP(x$p))
                }, character(1))
            },

            # TRUE when Cochran's Q was computed and was not significant: the pairwise results are then
            # exploratory, and every panel that reports them says so.
            .globalTestNotSignificant = function() {
                !is.null(private$.cochran_pvalue) &&
                    isTRUE(private$.cochran_pvalue >= private$P_THRESHOLD_SIGNIFICANT)
            },

            # IR-decisioncompare-01/02. The pairwise findings as one passage, the same in every panel:
            # an exploratory lead-in when the global test was computed but not significant, one
            # direction sentence per significant pair, and the case-mix caveat every overall-accuracy
            # verdict needs. "" when no pair is significant.
            .pairwiseFindingsText = function() {
                pair_sentences <- private$.pairDirectionSentences()
                if (!length(pair_sentences)) return("")
                lead <- if (private$.globalTestNotSignificant()) {
                    # .() has no plural forms: the singular is its own msgid.
                    if (length(pair_sentences) == 1)
                        jmvcore::.("Although the global test was not significant, this pairwise comparison reached p < 0.05 after Holm-Bonferroni adjustment; treat it as exploratory.")
                    else
                        jmvcore::.("Although the global test was not significant, these pairwise comparisons reached p < 0.05 after Holm-Bonferroni adjustment; treat them as exploratory.")
                }
                paste(c(lead, pair_sentences,
                        jmvcore::.("Overall accuracy weights sensitivity and specificity by the prevalence of the target condition among the compared cases, so which test is more accurate overall can change with the case mix, and under case-control sampling it does not estimate population accuracy.")),
                      collapse = " ")
            },

            # IR2-decisioncompare-02. "" when every test was evaluated on the same cases; otherwise
            # the case set the ranking used (or why no ranking was possible), because the per-test
            # figures printed beside the ranking use each test's own available cases.
            .rankingBasisSentence = function() {
                n <- private$.ranking_common_n
                if (is.null(n)) return("")
                if (isTRUE(private$.best_test_ranked))
                    .fmt(jmvcore::.("The tests were evaluated on different sets of cases, so this ranking compares them on the {n} cases with a determinate result for every test; the per-test figures use each test's own available cases."),
                        n = n)
                else
                    .fmt(jmvcore::.("The tests were evaluated on different sets of cases, and the {n} cases with a determinate result for every test do not include both reference-standard classes, so the tests cannot be ranked on common cases."),
                        n = n)
            },

            # IR-decisioncompare-01. The ranking uses balanced accuracy, which weights sensitivity and
            # specificity equally; McNemar's test compares overall accuracy, which weights them by the
            # prevalence in the sample, so the two can point in opposite directions. When a significant
            # comparison has a named test on the losing side, say so rather than naming it alone. Two
            # tests tied for the highest balanced accuracy do not disagree with the ranking when one is
            # more accurate overall, so they get their own sentence. exploratory_note adds the global
            # test's verdict for a panel that does not carry the pairwise passage itself. "" when there
            # is no such comparison.
            .rankingConflictSentence = function(best_test, exploratory_note = FALSE) {
                if (!isTRUE(private$.best_test_ranked) || is.null(best_test)) return("")
                named <- private$.tiedBest() %||% best_test
                losers <- list()
                tie_lines <- character(0)
                for (x in private$.significantPairs()) {
                    first_better <- x$b > x$c
                    loser <- if (first_better) x$second else x$first
                    winner <- if (first_better) x$first else x$second
                    if (!(loser %in% named)) next
                    if (winner %in% named)
                        tie_lines <- c(tie_lines, sprintf(
                            jmvcore::.("%1$s and %2$s were tied for the highest observed balanced accuracy, but %2$s classified significantly more cases correctly."),
                            private$.safeHtmlOutput(loser), private$.safeHtmlOutput(winner)))
                    else
                        losers[[loser]] <- c(losers[[loser]], winner)
                }
                if (!length(losers) && !length(tie_lines)) return("")
                lines <- vapply(names(losers), function(l) sprintf(
                    jmvcore::.("%1$s had the highest observed balanced accuracy but classified significantly fewer cases correctly than %2$s."),
                    private$.safeHtmlOutput(l), paste(private$.safeHtmlOutput(losers[[l]]), collapse = ", ")),
                    character(1))
                paste(c(unname(lines), tie_lines,
                        if (length(losers))
                            jmvcore::.("Balanced accuracy weights sensitivity and specificity equally, whereas overall accuracy, which McNemar's test compares, weights them by the prevalence in this sample, so the two rankings can disagree; compare sensitivity and specificity directly in the Differences with 95% Confidence Intervals table."),
                        if (isTRUE(exploratory_note) && private$.globalTestNotSignificant())
                            jmvcore::.("These pairwise results are exploratory because the global Cochran's Q test was not significant.")),
                      collapse = " ")
            },

            # One print rule for a paired difference and its limits in percentage points: one decimal,
            # half-up; a nonzero value that would print as 0.0 keeps two significant digits; zero never
            # prints as -0.0 (VAL round 4; shared by the Report and the Descriptive report).
            .fmtPP = function(v) {
                x <- v * 100
                if (!isTRUE(is.finite(x))) return(sprintf("%.1f", x))
                if (x == 0) return("0.0")
                if (abs(x) < 0.05) return(formatC(x, digits = 2, format = "fg", flag = "#"))
                sprintf("%.1f", private$.halfUp1(x))
            },

            # IR-decisioncompare-02. The paired sensitivity and specificity differences of pair k with
            # their per-class McNemar p, as one copy-ready sentence; "" when either class has no paired
            # cases. A class without discordant pairs has no test result, and the sentence says so
            # instead of printing the conventional p = 1 as if it had been computed.
            .classDifferenceSentence = function(k) {
                pr <- if (length(private$.pair_results) >= k) private$.pair_results[[k]] else NULL
                dr <- if (length(private$.diff_results) >= k) private$.diff_results[[k]] else NULL
                if (is.null(pr) || is.null(dr) || is.null(dr$Sens) || is.null(dr$Spec)) return("")
                p_phrase <- function(res, p) {
                    if (sum(res$counts[c("x_only", "y_only")]) == 0)
                        jmvcore::.("no discordant pairs, so no McNemar test")
                    else
                        sprintf(jmvcore::.("McNemar p %s, unadjusted"), private$.formatP(p))
                }
                sprintf(jmvcore::.("The paired differences, %1$s minus %2$s, were %3$s percentage points in sensitivity (95%% CI %4$s to %5$s; %6$s) and %7$s percentage points in specificity (95%% CI %8$s to %9$s; %10$s)."),
                    private$.safeHtmlOutput(pr$first), private$.safeHtmlOutput(pr$second),
                    private$.fmtPP(dr$Sens$diff), private$.fmtPP(dr$Sens$lower), private$.fmtPP(dr$Sens$upper),
                    p_phrase(dr$Sens, dr$p_sens),
                    private$.fmtPP(dr$Spec$diff), private$.fmtPP(dr$Spec$lower), private$.fmtPP(dr$Spec$upper),
                    p_phrase(dr$Spec, dr$p_spec))
            },

            # t1: every test that shares the highest balanced accuracy, or NULL when one test leads.
            .tiedBest = function() {
                tied <- private$.best_test_tied
                if (isTRUE(private$.best_test_ranked) && length(tied) > 1) tied else NULL
            },

            # A p-value for copy-ready HTML text, as the relation and the number: "< 0.001" below
            # 0.001 (never "= 0.000", VAL-decisioncompare-04), otherwise three decimals, or four
            # when three would print 0.050 for a p that is below 0.05. The "<" is written as an
            # entity because every caller pastes the result into an Html item.
            # Rounded half away from zero, as the jamovi table does (JavaScript toFixed); sprintf
            # alone rounds an exact binary tie such as 56.25 to even. Every number pasted into
            # copy-ready text goes through these, so a panel cannot print 81.2 where another prints
            # 81.3 for the same value (IR-decisioncompare-04). Decisions use the unrounded value.
            .halfUp = function(x, digits) sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits,
            .halfUp1 = function(x) private$.halfUp(x, 1L),
            # Translated text for the shared .diag* helpers (they cannot call .() themselves).
            .diagText = function() {
                utils::modifyList(.diagText, list(
                    ref_interpretation = jmvcore::.("Reference interpretation"),
                    ref_description = jmvcore::.("Comparator description"),
                    ref_target = jmvcore::.("Target definition"),
                    ref_rules = jmvcore::.("Positive and negative rules"),
                    ref_population = jmvcore::.("Analysis population"),
                    ref_unit = jmvcore::.("Sampling unit"),
                    ref_unit_value = jmvcore::.("Independent cases; repeated reads require a reader/cluster analysis"),
                    not_specified = jmvcore::.("Not specified"),
                    mode_legacy = jmvcore::.("Not specified"),
                    mode_disease = jmvcore::.("Disease reference"),
                    mode_consensus = jmvcore::.("Expert consensus"),
                    mode_method = jmvcore::.("Comparative method"),
                    note_legacy = jmvcore::.("Reference interpretation is not specified. Existing diagnostic wording is retained for compatibility; it requires a suitable disease reference. Choose consensus or comparative method when measuring agreement."),
                    note_comparator = jmvcore::.("Agreement with this comparator does not establish disease status. PPA/NPA condition on the comparator; symmetric positive/negative specific agreement uses different denominators. Population-prevalence adjustment and disease-risk outputs are not used."),
                    note_disease = jmvcore::.("Diagnostic interpretation assumes the selected reference establishes the target disease status."),
                    metric_ppa = jmvcore::.("Positive percent agreement (PPA)"),
                    metric_npa = jmvcore::.("Negative percent agreement (NPA)"),
                    metric_opa = jmvcore::.("Overall percent agreement (OPA)"),
                    metric_rpos = jmvcore::.("Reference positive given positive call"),
                    metric_rneg = jmvcore::.("Reference negative given negative call"),
                    metric_psa = jmvcore::.("Positive specific agreement (symmetric)"),
                    metric_nsa = jmvcore::.("Negative specific agreement (symmetric)"),
                    method_not_requested = jmvcore::.("Not requested"),
                    method_no_denominator = jmvcore::.("No eligible denominator"),
                    method_exact = jmvcore::.("Exact binomial (independent cases)"),
                    method_symmetric = jmvcore::.("Symmetric ratio; binomial CI not applicable"),
                    call_positive = jmvcore::.("Positive"),
                    call_negative = jmvcore::.("Negative"),
                    note_denominators = jmvcore::.("PPA/NPA condition on reference-positive/reference-negative cases. Reverse conditional agreement is not a patient's probability of disease. Symmetric specific agreement is not PPA/NPA. All binary estimates use determinate reference/test pairs under the displayed recoding rule."),
                    bin_missing_positive = jmvcore::.("Select an explicit positive level. Both positive and negative levels are required; no level is inferred."),
                    bin_missing_negative = jmvcore::.("Select an explicit negative level. Both positive and negative levels are required; no level is inferred."),
                    bin_unavailable_positive = jmvcore::.("The positive level \"%1$s\" is not available. Available levels: %2$s."),
                    bin_unavailable_negative = jmvcore::.("The negative level \"%1$s\" is not available. Available levels: %2$s."),
                    bin_identical = jmvcore::.("Choose different positive and negative levels."),
                    bin_other = jmvcore::.("Observed levels outside the selected positive/negative pair: %s. Select 'Selected positive and negative levels only' to omit these results, or explicitly recode the variable before analysis. Other levels are never treated as negative."),
                    report_rate = jmvcore::.("%1$.1f%% (%2$d/%3$d)"),
                    report_unavailable = jmvcore::.("unavailable (denominator 0)"),
                    report_test = jmvcore::.("%1$s: PPA %2$s, NPA %3$s; %4$d analyzed cases, %5$d excluded."),
                    report_close = jmvcore::.("These quantities describe agreement with the selected comparator. The tables state the denominators and interval methods. Differences between tests require paired comparisons on common cases."),
                    plot_test = jmvcore::.("Test"),
                    plot_row = jmvcore::.("Selected row"),
                    plot_call = jmvcore::.("Call / reference"),
                    plot_agreement = jmvcore::.("Agreement"),
                    plot_gaps = jmvcore::.("Not estimable (denominator 0), not drawn: %s"),
                    class_pos_match = jmvcore::.("Positive match"),
                    class_pos_call = jmvcore::.("Positive call / negative reference"),
                    class_neg_call = jmvcore::.("Negative call / positive reference"),
                    class_neg_match = jmvcore::.("Negative match"),
                    class_excluded = jmvcore::.("Excluded")
                ))
            },

            .dvText = function() {
                list(
                    cohortRole = jmvcore::.("Cohort role"),
                    samplingDesign = jmvcore::.("Sampling design"),
                    thresholdSource = jmvcore::.("Threshold source"),
                    modelVersion = jmvcore::.("Model version"),
                    analysisUnit = jmvcore::.("Unit of observation"),
                    referenceType = jmvcore::.("Reference interpretation"),
                    referenceDescription = jmvcore::.("Comparator description"),
                    not_specified = jmvcore::.("Not specified"),
                    legacy = jmvcore::.("Not specified"),
                    disease = jmvcore::.("Disease reference"),
                    consensus = jmvcore::.("Expert consensus"),
                    method = jmvcore::.("Comparative method"),
                    note = jmvcore::.("User-declared context does not verify independence or external validation. Mapping and exclusion ledgers define the analyzed pairs; unknown levels must not silently become negative.")
                )
            },

            # Percent text for a proportion: one decimal, unless that would print a value strictly
            # between 0 and 1 as 0.0 or 100.0 (rare-disease prevalence, PPV at such a prevalence);
            # then two significant digits of the distance to the edge.
            .pctAdaptive = function(p) {
                pct <- p * 100
                edge <- min(pct, 100 - pct)
                digits <- if (!is.finite(edge) || edge <= 0 || edge >= 0.05) 1L
                    else min(10L, 1L - as.integer(floor(log10(edge))))
                formatC(private$.halfUp(pct, digits), format = "f", digits = digits)
            },

            .formatP = function(p) {
                if (length(p) != 1L || is.na(p)) return("= NA")
                if (p < 0.001) return("&lt; 0.001")
                # Round half up, as the jamovi table does (JavaScript toFixed); sprintf alone rounds
                # an exact binary tie such as the mid-p 0.0625 to even, printing 0.062 beside 0.063.
                shown <- sprintf("%.3f", floor(p * 1000 + 0.5) / 1000)
                # A significant p must not print as 0.050: try four decimals, then say "< 0.05".
                if (p < 0.05 && as.numeric(shown) >= 0.05) shown <- sprintf("%.4f", floor(p * 10000 + 0.5) / 10000)
                if (p < 0.05 && as.numeric(shown) >= 0.05) return("&lt; 0.05")
                paste0("= ", shown)
            },

            # ======================================================================
            # Notice Management Helper Methods (HTML output to avoid serialization)
            # ======================================================================

            # Initialize notice collection list
            .noticeList = list(),

            # Names of tests tied for best, if any (set by .findBestTest)
            .best_test_tied = NULL,
            # Number of cases the ranking used when the tests were evaluated on different
            # case sets (set by .findBestTest); NULL when every test had the same cases.
            .ranking_common_n = NULL,

            # Add a notice to the collection.
            # .findBestTest() is reached independently from three display toggles
            # (Summary, Report sentence, Descriptive report), so the identical "Tied
            # Descriptive Ranking" warning was appended two or three times in a row and
            # read as three separate problems. An identical (type, title, content)
            # triple is the same message; keep the first.
            .addNotice = function(type, title, content) {
                for (existing in private$.noticeList) {
                    if (identical(existing$type, type) &&
                        identical(existing$title, title) &&
                        identical(existing$content, content))
                        return(invisible(NULL))
                }
                private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                    type = type,
                    title = title,
                    content = content
                )
            },

            # Render collected notices as HTML
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    return()
                }

                # Map notice types to colors and icons
                typeStyles <- list(
                    ERROR = list(bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5", icon = ""),
                    STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74", icon = ""),
                    WARNING = list(bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047", icon = ""),
                    INFO = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd", icon = "")
                )

                html <- "<div style='margin: 10px 0;'>"

                severity <- c("ERROR", "STRONG_WARNING", "WARNING", "INFO")
                priority <- vapply(private$.noticeList, function(notice)
                    match(notice$type, severity, nomatch = length(severity)), integer(1))
                for (notice in private$.noticeList[order(priority)]) {
                    style <- typeStyles[[notice$type]] %||% typeStyles$INFO

                    html <- paste0(
                        html,
                        "<div style='background-color: ", style$bgcolor, "; ",
                        "border-left: 4px solid ", style$border, "; ",
                        "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                        "<strong>",
                        style$icon, " ", private$.safeHtmlOutput(notice$title), "</strong><br>",
                        "<span style='color: inherit;'>", private$.safeHtmlOutput(notice$content), "</span>",
                        "</div>"
                    )
                }

                html <- paste0(html, "</div>")

                self$results$notices$setContent(html)
            },

            # HTML sanitization for security
            .safeHtmlOutput = function(text) {
                if (is.null(text) || length(text) == 0) {
                    return("")
                }
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

            # Initialization - visibility now handled by .r.yaml
            .init = function() {
                # Initialize table rows for dynamic population
                private$.initializeTables()
                # Option-only tables get their rows here so they never render as an empty
                # header that then restructures; .run()/.validateMappings() refill the same keys.
                .dvProvenance(self$results$validationContext, self$options, private$.dvText())
                .diagReferenceDefinition(self$results, self$options,
                    jmvcore::.("Select reference and test levels; binary recoding is displayed after validation"),
                    jmvcore::.("Per-test available binary pairs; differences use common cases"),
                    private$.diagText())
            },
            # Main analysis orchestrator - refactored for clarity
            .runComparator = function() {
                tests <- private$.getTestVariables()
                positives <- private$.getTestPositives()
                negatives <- private$.getTestNegatives()
                reference <- private$.mapBinary(self$data[[self$options$gold]],
                    self$options$goldPositive, private$.opt("goldNegative"), self$options$gold)
                comparisons <- stats::setNames(lapply(tests, function(nm) list(
                    call = private$.mapBinary(self$data[[nm]], positives[[nm]],
                        negatives[[nm]], nm), reference = reference)), tests)
                # A test with no determinate pair stops, as on the disease path, before any
                # comparator table is filled.
                n_pairs <- vapply(comparisons, function(z)
                    sum(!is.na(z$call) & !is.na(z$reference)), integer(1))
                if (any(n_pairs == 0L)) {
                    private$.addNotice(type = "ERROR",
                        title = jmvcore::.("No Determinate Cases"),
                        content = .fmt(jmvcore::.("No cases remain for {test} after applying the selected positive and negative levels."),
                            test = paste(names(n_pairs)[n_pairs == 0L], collapse = ", ")))
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }
                text <- private$.diagText()
                summaries <- .diagFillComparator(self$results, comparisons, self$options$ci, text)
                gold_raw <- as.character(self$data[[self$options$gold]])
                gold_missing <- is.na(gold_raw)
                gold_other <- !gold_missing & is.na(reference)
                for (i in seq_along(tests)) {
                    nm <- tests[i]
                    k <- summaries[[nm]]$counts
                    positives_n <- k[["a"]] + k[["c"]]
                    negatives_n <- k[["b"]] + k[["d"]]
                    if (min(positives_n, negatives_n) < 30)
                        private$.addNotice(type = "STRONG_WARNING",
                            title = jmvcore::.("Sparse Comparator Class"),
                            content = if (isTRUE(self$options$ci))
                                .fmt(jmvcore::.("{test}: PPA rests on {positives} comparator-positive cases and NPA on {negatives} comparator-negative cases. A percentage computed on a denominator this small is far less precise than the overall analyzed N suggests. Read the 95% CI in the Agreement with comparator table before quoting either figure."),
                                    test = nm, positives = positives_n, negatives = negatives_n)
                            else
                                .fmt(jmvcore::.("{test}: PPA rests on {positives} comparator-positive cases and NPA on {negatives} comparator-negative cases. A percentage computed on a denominator this small is far less precise than the overall analyzed N suggests. Enable the 95% CI option and read the interval before quoting either figure."),
                                    test = nm, positives = positives_n, negatives = negatives_n))
                    # Split the excluded pairs: a row with an unselected non-NA level in either
                    # the comparator or the test is counted under other; missing counts rows
                    # with no unselected level and at least one NA.
                    test_raw <- as.character(self$data[[nm]])
                    test_missing <- is.na(test_raw)
                    test_other <- !test_missing & is.na(comparisons[[nm]]$call)
                    other_n <- sum(gold_other | test_other)
                    missing_n <- sum(!gold_other & !test_other & (gold_missing | test_missing))
                    self$results$comparatorPopulation$setRow(rowKey = i, values = list(
                        missing = missing_n, other = other_n))
                }
                self$results$comparatorReport$setContent(.diagComparatorReport(summaries, text))
                if (self$options$statComp) {
                    rows <- list()
                    informative <- logical(0)
                    for (pair in utils::combn(tests, 2L, simplify = FALSE)) {
                        x <- comparisons[[pair[1]]]$call
                        y <- comparisons[[pair[2]]]$call
                        ok <- !is.na(x) & !is.na(y) & !is.na(reference)
                        for (metric in c("PPA", "NPA", "OPA")) {
                            take <- ok & if (metric == "PPA") reference else
                                if (metric == "NPA") !reference else TRUE
                            take[is.na(take)] <- FALSE
                            a <- x[take]; b <- y[take]
                            if (metric == "NPA") { a <- !a; b <- !b }
                            if (metric == "OPA") {
                                a <- a == reference[take]; b <- b == reference[take]
                            }
                            d <- pairedProportionDifference(a, b)
                            row <- list(comparison = .diagCellText(sprintf(jmvcore::.("%1$s minus %2$s"),
                                    pair[1], pair[2])),
                                metric = metric, n = sum(take), diff = NULL, lower = NULL,
                                upper = NULL, p = NULL, adjusted = NULL,
                                method = jmvcore::.("No eligible paired cases"))
                            if (!is.null(d)) {
                                discordant <- d$counts[c("x_only", "y_only")]
                                test <- private$.mcnemarTest(discordant[1], discordant[2],
                                    self$options$mcnemarMethod)
                                row$diff <- d$diff
                                if (self$options$ci) { row$lower <- d$lower; row$upper <- d$upper }
                                row$p <- test$p.value
                                row$method <- paste(c(test$method,
                                    if (self$options$ci) jmvcore::.("Newcombe paired 95% CI"),
                                    if (!sum(discordant)) jmvcore::.("no discordance: p=1 convention")),
                                    collapse = "; ")
                            }
                            informative <- c(informative, !is.null(d) && sum(discordant) > 0)
                            rows[[length(rows) + 1L]] <- row
                        }
                    }
                    # Holm over the informative tests only, as the disease path does: a
                    # no-discordance row carries p = 1 by convention and would only inflate the family.
                    p <- vapply(rows, function(z) if (is.null(z$p)) NA_real_ else z$p, numeric(1))
                    adjusted <- rep(NA_real_, length(p))
                    if (any(informative))
                        adjusted[informative] <- stats::p.adjust(p[informative], "holm")
                    for (i in seq_along(rows)) {
                        rows[[i]]$adjusted <- if (is.na(adjusted[i])) NULL else adjusted[i]
                        self$results$comparatorDifferences$addRow(rowKey = i, values = rows[[i]])
                    }
                    self$results$comparatorDifferences$setNote("paired", jmvcore::.("First minus second on common cases. PPA uses reference-positive cases; NPA uses reference-negative cases; OPA compares agreement with the reference. Holm adjustment covers the displayed pair/metric tests that have discordant pairs; a test without discordance keeps p = 1 and is not adjusted. Intervals are pointwise, not multiplicity-adjusted. Independent cases are required."))
                }
                stratum <- private$.opt("stratify")
                if (!is.null(stratum)) {
                    groups <- as.character(self$data[[stratum]])
                    strata <- private$.strataOrder(self$data[[stratum]])
                    n_missing_stratum <- sum(is.na(groups))
                    if (n_missing_stratum > 0)
                        private$.addNotice(type = "INFO",
                            title = jmvcore::.("Missing Stratifier Values"),
                            content = .fmt(jmvcore::.("{rows} row(s) have no value for {variable}. They remain in overall per-test results but are omitted from subgroup rows."),
                                rows = n_missing_stratum, variable = stratum))
                    any_pairs <- FALSE
                    for (level in strata) for (nm in tests) {
                        take <- !is.na(groups) & groups == level
                        m <- .diagComparatorMetrics(comparisons[[nm]]$call[take],
                            reference[take], FALSE, text)$metrics
                        any_pairs <- any_pairs || m$denominator[3] > 0
                        for (i in seq_len(nrow(m))) self$results$comparatorStrata$addRow(
                            rowKey = paste(match(level, strata), match(nm, tests), i), values =
                            list(stratum = .diagCellText(level), test = .diagCellText(nm), metric = m$metric[i],
                                numerator = m$numerator[i], denominator = m$denominator[i],
                                estimate = if (is.na(m$estimate[i])) NULL else m$estimate[i]))
                    }
                    if (!any_pairs)
                        private$.addNotice(type = "WARNING",
                            title = jmvcore::.("Stratification Empty"),
                            content = .fmt(jmvcore::.("No analyzable cases were found in any observed stratum of '{variable}'. Check subgroup values and test/reference availability."),
                                variable = stratum))
                }
                states <- .diagComparatorPlotState(comparisons, summaries, text)
                if (self$options$plot) self$results$comparatorPlot$setState(states$bar)
                if (self$options$radarplot) self$results$comparatorRadar$setState(states$bar)
                if (self$options$heatmap) self$results$comparatorHeatmap$setState(states$heatmap)
                self$results$comparatorPopulation$setNote("plot", jmvcore::.("Heatmaps display at most the first 1,000 selected rows; tables use all eligible rows. Missing stratification values are omitted only from stratified results."))
            },

            .plotComparatorBar = function(image, ggtheme, ...) {
                .diagComparatorPlot(image, ggtheme, "bar", private$.diagText())
            },
            .plotComparatorRadar = function(image, ggtheme, ...) {
                .diagComparatorPlot(image, ggtheme, "radar", private$.diagText())
            },
            .plotComparatorHeatmap = function(image, ggtheme, ...) {
                .diagComparatorPlot(image, ggtheme, "heatmap", private$.diagText())
            },

            .run = function() {
                # Reset per-run state so notices/flags do not accumulate or leak
                # across successive runs when jamovi reuses the R6 instance.
                private$.noticeList <- list()
                .deleteTableRows(self$results$levelMapping)
                .diagReferenceDefinition(self$results, self$options,
                    jmvcore::.("Select reference and test levels; binary recoding is displayed after validation"),
                    jmvcore::.("Per-test available binary pairs; differences use common cases"),
                    private$.diagText())
                for (nm in c("comparatorMetrics", "comparatorCounts", "comparatorPopulation",
                             "comparatorDifferences", "comparatorStrata"))
                    .deleteTableRows(self$results[[nm]])
                self$results$comparatorReport$setContent("")
                for (nm in c("comparatorPlot", "comparatorRadar", "comparatorHeatmap"))
                    self$results[[nm]]$setState(NULL)
                private$.any_significant_comparison <- NULL
                private$.cochran_pvalue <- NULL
                private$.cochran_method_note <- ""
                private$.mcnemar_uninformative <- NULL
                private$.holm_applied <- NULL
                private$.best_test_tied <- NULL
                private$.best_test_ranked <- NULL
                private$.ranking_common_n <- NULL
                private$.cached_test_results <- NULL
                private$.cached_processed_data <- NULL
                private$.pair_results <- NULL
                private$.diff_results <- NULL

                # Dynamic results must not survive an invalid rerun. clearWith handles
                # ordinary option changes; this explicit reset also covers validation
                # failures and analyses whose required variables have just been removed.
                for (table_name in c("comparisonTable", "opaTable", "stratifiedTable", "mcnemarTable", "diffTable")) {
                    table <- tryCatch(self$results[[table_name]], error = function(e) NULL)
                    if (!is.null(table)) .deleteTableRows(table)
                }
                # Deleting rows does not remove table notes from a previous method or dataset.
                for (key in c("mcnemar_multiplicity", "mcnemar_uninformative_excluded",
                              "mcnemar_midp_nostat", "cochran_q_failed", "cochran_method"))
                    self$results$mcnemarTable$setNote(key, NULL)
                self$results$diffTable$setNote("diff_multiplicity", NULL)
                # Preserve the fixed row scaffold, but blank values before validation can fail.
                for (i in 1:3) {
                    ctable <- self$results[[paste0("cTable", i)]]
                    for (key in c("Test Positive", "Test Negative", "Total"))
                        ctable$setRow(rowKey = key,
                            values = list(GP = "", GN = "", Total = ""))  # "" is blank; NA_real_ shows NaN
                    epitable <- self$results[[paste0("epirTable", i)]]
                    for (key in private$.epirRatioStats())
                        epitable$setRow(rowKey = key,
                            values = list(est = "", lower = "", upper = ""))
                }
                self$results$text1$setContent("")
                self$results$text2$setContent("<div></div>")  # Html: the client skips ""
                for (plot_name in c("plot1", "plotRadar", "plotHeatmap"))
                    self$results[[plot_name]]$setState(NULL)
                for (html_name in c("summaryReport", "reportSentence", "explanationsContent",
                                    "clinicalReport", "aboutAnalysis", "notices")) {
                    html <- tryCatch(self$results[[html_name]], error = function(e) NULL)
                    # "<div></div>", not "": the client skips an empty Html update, so a run
                    # that stops early (pending level box) would leave the old reports on screen.
                    if (!is.null(html)) html$setContent("<div></div>")
                }

                # Step 1: Check if minimum required variables are selected
                if (!private$.hasRequiredVars()) {
                    # A reference plus one test is a common setup that would otherwise stay
                    # silently empty; say what is missing.
                    gold <- private$.opt("gold")
                    if (!is.null(gold) && length(setdiff(private$.getTestVariables(), gold)) == 1L) {
                        private$.addNotice(type = "INFO",
                            title = jmvcore::.("Second Test Required"),
                            content = jmvcore::.("This analysis compares two or three tests against the same reference. Select a second test variable in another test slot; for a single test use the Medical Decision analysis."))
                        private$.renderNotices()
                    }
                    return() # Wait for user to select required variables
                }

                tryCatch(
                    {
                        # Step 2: Comprehensive input validation. TRUE = a level box is
                        # still empty: an INFO names it and the run stops without an error.
                        if (isTRUE(private$.validateInputs())) {
                            private$.renderNotices()
                            return(invisible(NULL))
                        }

                        if (.diagReferenceMode(self$options$referenceType)$comparator) {
                            private$.runComparator()
                            private$.renderNotices()
                            return(invisible(NULL))
                        }

                        # Step 3: Process and clean data
                        processed_data <- private$.prepareData()

                        # Step 4: Process each test and calculate metrics
                        test_results <- private$.processAllTests(processed_data)

                        # Cache test results and processed data to avoid redundant calculations
                        private$.cached_test_results <- test_results
                        private$.cached_processed_data <- processed_data

                        # Step 5: Populate comparison table
                        private$.populateComparisonTable(test_results)

                        # Step 5b: Populate OPA table if requested
                        if (self$options$opa) {
                            private$.populateOPATable(test_results)
                        }

                        # Step 5c: Populate stratified table if requested
                        if (!is.null(self$options$stratify) && self$options$stratify != "") {
                            private$.populateStratifiedTable(processed_data, test_results)
                        }

                        # Step 6: Handle original data display if requested
                        if (self$options$od) {
                            private$.displayOriginalData(processed_data)
                        }

                        # Step 7: Perform statistical comparisons if requested
                        if (self$options$statComp && length(test_results) >= 2) {
                            private$.performStatisticalComparisons(test_results)
                        }

                        # Step 8a: Generate summary if requested
                        if (self$options$showSummary && self$options$statComp && length(test_results) >= 2) {
                            private$.generateSummary(test_results)
                        }

                        # Step 8b: Generate report sentence if requested
                        if (self$options$showReportSentence && self$options$statComp && length(test_results) >= 2) {
                            private$.generateReportSentence(test_results)
                        }

                        # Step 8c: Generate explanations if requested
                        if (self$options$showExplanations) {
                            private$.generateExplanations()
                            private$.generateAboutPanel()
                        }

                        # Step 8: Setup visualizations if requested
                        if (self$options$plot || self$options$radarplot || self$options$heatmap) {
                            private$.setupVisualizations(test_results)
                        }

                        # Step 9: Generate the longer descriptive report only when requested
                        if (isTRUE(private$.opt("showDescriptiveReport", FALSE))) {
                            private$.generateClinicalReport(test_results, processed_data)
                        }

                        # Step 10: Success completion notice
                        n_tests <- length(test_results)
                        n_cases <- nrow(processed_data$data)
                        analyzed <- vapply(test_results, function(x) x$n_analyzed, integer(1))
                        analyzed_text <- paste0(names(analyzed), " n=", analyzed, collapse = "; ")

                        private$.addNotice(
                            type = "INFO",
                            title = jmvcore::.("Analysis Completed Successfully"),
                            content = .fmt(jmvcore::.("{tests} diagnostic tests compared among {rows} selected rows. Per-test available determinate denominators: {denominators}. Paired comparisons use only rows determinate for both tests and the reference standard."),
                                tests = n_tests, rows = n_cases, denominators = analyzed_text
                            )
                        )
                    },
                    error = function(e) {
                        # A .checkpoint() restart is CONTROL FLOW, not a failure. It arrives
                        # here as an error carrying code = "restart" and fires often during a
                        # long run, so it must leave before anything is rendered -- otherwise
                        # every restart serializes the whole notices panel and then throws the
                        # run away that was about to be restarted anyway.
                        if (identical(e$code, "restart"))
                            stop(e)
                        # Render collected notices (including validation ERROR notices)
                        # BEFORE re-throwing, otherwise the detailed, actionable notice
                        # HTML is discarded and the user sees only the bare error string.
                        private$.renderNotices()
                        # stop(conditionMessage(e)) rebuilds a bare simpleError and throws the
                        # condition's class and $code away, so re-raise anything carrying a
                        # $code untouched. NOTE: a plain jmvcore::reject() called WITHOUT
                        # code= has $code NULL, exactly like stop() -- it falls through to the
                        # message path below, which preserves the message the user sees.
                        if (!is.null(e$code))
                            stop(e)
                        stop(conditionMessage(e), call. = FALSE)
                    }
                )

                # Step 11: Render all collected notices as HTML (success path)
                private$.renderNotices()
            },

            # Check if minimum required variables are selected
            .hasRequiredVars = function() {
                # Gold standard is required
                if (is.null(self$options$gold) || length(self$options$gold) == 0) {
                    return(FALSE)
                }

                # At least 2 tests are required
                test_count <- 0
                if (!is.null(self$options$test1) && self$options$test1 != "") test_count <- test_count + 1
                if (!is.null(self$options$test2) && self$options$test2 != "") test_count <- test_count + 1
                if (!is.null(self$options$test3) && self$options$test3 != "") test_count <- test_count + 1

                if (test_count < 2) {
                    return(FALSE)
                }

                return(TRUE)
            },


            # One mapping/validation path for disease and comparator analyses.
            .mapBinary = function(values, positive, negative, variable) {
                tryCatch(
                    .diagComparatorBinary(values, positive, negative,
                        self$options$excludeIndeterminate, private$.diagText()),
                    error = function(e) {
                        message <- paste0(variable, ": ", conditionMessage(e))
                        # One title fits every mapping failure: a missing level, identical
                        # levels, an unavailable level, or extra observed levels.
                        private$.addNotice(type = "ERROR",
                            title = jmvcore::.("Level Mapping Problem"),
                            content = message)
                        stop(sprintf(jmvcore::.("Validation failed: %s"), message), call. = FALSE)
                    }
                )
            },

            .validateMappings = function() {
                tests <- private$.getTestVariables()
                variables <- c(self$options$gold, tests)
                positives <- c(list(self$options$goldPositive),
                    unname(private$.getTestPositives()))
                negatives <- c(list(private$.opt("goldNegative")),
                    unname(private$.getTestNegatives()))
                rows <- vector("list", length(variables))
                rules <- character(length(variables))
                # Check every variable first and report all mapping problems in one notice, so a
                # new analysis is not fixed one variable per run.
                problems <- character(0)
                text <- private$.diagText()
                for (i in seq_along(variables)) {
                    nm <- variables[i]
                    if (!nm %in% names(self$data))
                        stop(sprintf(jmvcore::.("Variable not found: %s"), nm), call. = FALSE)
                    problem <- tryCatch({
                        .diagComparatorBinary(self$data[[nm]], positives[[i]], negatives[[i]],
                            self$options$excludeIndeterminate, text)
                        NULL
                    }, error = function(e) paste0(nm, ": ", conditionMessage(e)))
                    problems <- c(problems, problem)
                }
                if (length(problems)) {
                    private$.addNotice(type = "ERROR",
                        title = jmvcore::.("Level Mapping Problem"),
                        content = paste(problems, collapse = " | "))
                    stop(sprintf(jmvcore::.("Validation failed: %s"), paste(problems, collapse = " | ")),
                        call. = FALSE)
                }
                for (i in seq_along(variables)) {
                    nm <- variables[i]
                    raw <- as.character(self$data[[nm]])
                    other <- !is.na(raw) & !raw %in% c(positives[[i]], negatives[[i]])
                    counts <- table(raw[other])
                    # .diagCellText() only on displayed labels; matching above used the raw values.
                    rows[[i]] <- list(variable = .diagCellText(nm),
                        positive = .diagCellText(positives[[i]]),
                        negative = .diagCellText(negatives[[i]]),
                        positiveN = sum(raw == positives[[i]], na.rm = TRUE),
                        negativeN = sum(raw == negatives[[i]], na.rm = TRUE),
                        otherN = sum(other), missingN = sum(is.na(raw)),
                        otherLevels = if (length(counts)) .diagCellText(paste0(names(counts), " (n=",
                            as.integer(counts), ")", collapse = "; ")) else jmvcore::.("None"))
                    # sprintf, not .fmt: .fmt rewrites braces inside values, and the rule must show
                    # the exact level labels.
                    rules[i] <- sprintf(jmvcore::.("%1$s: positive = %2$s; negative = %3$s"),
                        nm, positives[[i]], negatives[[i]])
                }
                # Validate every mapping before publishing any result.
                for (i in seq_along(rows))
                    self$results$levelMapping$addRow(rowKey = i, values = rows[[i]])
                self$results$levelMapping$setNote("policy", jmvcore::.("Positive and negative mean exactly the selected levels, not ordinal cutoffs. Other observed levels are never negative. Column counts may overlap across variables. Missing results are excluded per test; paired comparisons use common eligible cases."))
                population <- if (self$options$excludeIndeterminate)
                    jmvcore::.("Per-test selected positive/negative pairs; differences use common paired cases; missing results excluded; other observed levels explicitly excluded")
                else
                    jmvcore::.("Per-test selected positive/negative pairs; differences use common paired cases; missing results excluded")
                if (self$options$excludeIndeterminate) {
                    if (any(vapply(rows, function(x) x$otherN > 0L, logical(1)))) {
                        private$.addNotice(type = "STRONG_WARNING",
                            title = jmvcore::.("Estimates Conditional on Selected Levels"),
                            content = jmvcore::.("Results outside the selected positive/negative pairs were excluded. Review their labels and counts in the Selected Levels and Exclusions table and report exclusions alongside performance. These estimates are conditional on determinate results, not performance on all attempted tests."))
                    }
                }
                .diagReferenceDefinition(self$results, self$options,
                    paste(rules, collapse = " | "), population, private$.diagText())
                invisible(NULL)
            },

            # Comprehensive input validation with informative error messages
            .validateInputs = function() {
                # Check for valid data
                if (is.null(self$data) || nrow(self$data) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("No Data Provided"),
                        content = jmvcore::.("No data provided for analysis. Please ensure your dataset contains data. Load a dataset with diagnostic test variables and a gold standard reference.")
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                selected_tests <- private$.getTestVariables()
                if (anyDuplicated(selected_tests)) {
                    duplicates <- unique(selected_tests[duplicated(selected_tests)])
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("Duplicate Test Variables"),
                        content = .fmt(jmvcore::.("Each test slot must contain a different variable. Remove the duplicate selection(s): {duplicates}."),
                            duplicates = paste(duplicates, collapse = ", ")
                        )
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }
                if (!is.null(self$options$gold) && self$options$gold %in% selected_tests) {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("Reference Reused as a Test"),
                        content = jmvcore::.("The gold-standard variable cannot also occupy a test slot. Select a different variable for each test.")
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }
                # Stratifying by the reference or a compared test gives subgroup rows that follow
                # from the definition (PPA 100% / NPA 0% within the test's own call), not the data.
                stratum <- private$.opt("stratify")
                if (!is.null(stratum) && stratum %in% c(self$options$gold, selected_tests)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("Stratifier Reused"),
                        content = jmvcore::.("The stratification variable cannot also be the reference or one of the compared tests. Choose a different subgroup variable.")
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                # Level options have no default (allowNone), so a variable whose positive or
                # negative level box is still empty is the normal unconfigured state, not an
                # error: name the empty boxes and stop quietly (IR2-decisioncompare-04). The
                # caller returns before any result is filled, so no level is ever inferred.
                level_boxes <- list(
                    list(var = self$options$gold, level = self$options$goldPositive, box = jmvcore::.("Reference positive level")),
                    list(var = self$options$gold, level = private$.opt("goldNegative"), box = jmvcore::.("Reference negative level")),
                    list(var = self$options$test1, level = self$options$test1Positive, box = jmvcore::.("Test 1 positive level")),
                    list(var = self$options$test1, level = private$.opt("test1Negative"), box = jmvcore::.("Test 1 negative level")),
                    list(var = self$options$test2, level = self$options$test2Positive, box = jmvcore::.("Test 2 positive level")),
                    list(var = self$options$test2, level = private$.opt("test2Negative"), box = jmvcore::.("Test 2 negative level")),
                    list(var = self$options$test3, level = self$options$test3Positive, box = jmvcore::.("Test 3 positive level")),
                    list(var = self$options$test3, level = private$.opt("test3Negative"), box = jmvcore::.("Test 3 negative level"))
                )
                empty_boxes <- character(0)
                for (b in level_boxes) {
                    if (!is.null(b$var) && b$var != "" &&
                        (length(b$level) != 1L || is.na(b$level) || !nzchar(b$level)))
                        empty_boxes <- c(empty_boxes, b$box)
                }
                if (length(empty_boxes) > 0) {
                    private$.addNotice(
                        type = "INFO",
                        title = jmvcore::.("Levels Not Yet Selected"),
                        content = .fmt(jmvcore::.("To finish setting up the analysis, choose: {boxes}. The reference and every test need both a positive and a negative level; no level is inferred."),
                            boxes = paste(empty_boxes, collapse = ", "))
                    )
                    return(TRUE)
                }

                private$.validateMappings()

                # Prevalence belongs to the disease path. Consensus/method ignore pp, which keeps
                # its last value while its checkbox is disabled.
                if (.diagReferenceMode(self$options$referenceType)$comparator)
                    return(invisible(NULL))

                # Validate prevalence setting
                if (self$options$pp && (self$options$pprob <= 0 || self$options$pprob >= 1)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("Invalid Prevalence Value"),
                        content = .fmt(jmvcore::.("Prior probability must be between 0 and 1 (exclusive). Current value: {value}. Enter a decimal value like 0.15 for 15% prevalence."),
                            value = self$options$pprob)
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                # Check for conflicting options
                if (self$options$pp && self$options$ci) {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("Conflicting Options"),
                        content = jmvcore::.("Prior probability and confidence intervals cannot both be enabled. Please disable either the \"Prior Probability\" or the \"95% CI\" option. Use CI for sample-based estimates or custom prevalence for population-based PPV/NPV.")
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                # Inform user about prevalence source
                if (self$options$pp) {
                    private$.addNotice(
                        type = "INFO",
                        title = jmvcore::.("Prevalence Source"),
                        content = jmvcore::.("PPV/NPV will be calculated using the supplied population prevalence (pp=TRUE). Confidence intervals are unavailable in this mode.")
                    )
                }
                invisible(FALSE)
            },

            # Data preparation and cleaning
            .prepareData = function() {
                # Extract variable names first
                goldVariable <- self$options$gold
                goldPLevel <- self$options$goldPositive

                if (is.null(goldVariable) || goldVariable == "") {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("No Gold Standard"),
                        content = jmvcore::.("Gold standard variable must be specified. Select a reference test variable that represents the true disease status.")
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                # Get test variables
                testVariables <- private$.getTestVariables()

                if (length(testVariables) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("No Test Variables"),
                        content = jmvcore::.("At least one test variable must be specified. Select diagnostic test variables to compare against the gold standard.")
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                # Keep every selected row aligned. Standalone metrics use available
                # reference/test pairs; paired comparisons use the common determinate
                # rows for the two tests being compared. A missing co-test or
                # stratifier therefore cannot change another test's standalone metric.
                stratVariable <- private$.opt("stratify")
                all_vars <- unique(c(goldVariable, testVariables, stratVariable))
                all_vars <- all_vars[!is.na(all_vars) & nzchar(all_vars)]
                mydata <- self$data[, all_vars, drop = FALSE]

                if (nrow(mydata) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("No Data"),
                        content = jmvcore::.("The dataset contains no rows to analyze.")
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                # Convert to factor and validate positive level
                mydata[[goldVariable]] <- forcats::as_factor(mydata[[goldVariable]])
                private$.assertLevelExists(mydata[[goldVariable]], goldPLevel, goldVariable)

                # as.character(): an explicit NA level (addNA()) is a level, so is.na() on the factor
                # does not see it as missing (IR-decisioncompare-06).
                missing_counts <- vapply(mydata, function(x) sum(is.na(as.character(x))), integer(1))
                if (any(missing_counts > 0)) {
                    missing_text <- paste0(
                        names(missing_counts)[missing_counts > 0], "=",
                        missing_counts[missing_counts > 0], collapse = "; "
                    )
                    private$.addNotice(
                        type = "WARNING",
                        title = jmvcore::.("Available-Case Missing-Data Handling"),
                        content = .fmt(jmvcore::.("Missing values were retained for transparent, test-specific handling ({counts}). Each standalone test uses rows observed for that test and the reference standard; paired comparisons use rows observed for both tests and the reference standard. Missing stratifier values affect only subgroup results. Report this handling and assess whether missingness could bias accuracy estimates (STARD 2015)."),
                            counts = missing_text
                        )
                    )
                }

                # Sample-size notices use only the selected reference-positive/negative levels.
                gold_chars <- as.character(mydata[[goldVariable]])
                gold_determinate <- gold_chars %in% c(goldPLevel, private$.opt("goldNegative"))
                n_gold_eligible <- sum(gold_determinate)
                if (n_gold_eligible < 50) {
                    very_small <- n_gold_eligible < 30
                    # Two complete sentences rather than splicing a translated adjective
                    # into an English frame: word order differs between languages.
                    private$.addNotice(
                        type = if (very_small) "STRONG_WARNING" else "WARNING",
                        title = if (very_small) jmvcore::.("Very Small Sample Size") else jmvcore::.("Small Sample Size"),
                        content = if (very_small)
                            .fmt(jmvcore::.("Very small eligible reference-standard sample (n={n}). Confidence intervals may be wide and estimates unstable, especially when either gold-standard class is sparse. Interpret the interval widths and class-specific denominators; no universal sample-size cutoff guarantees adequate precision."), n = n_gold_eligible)
                        else
                            .fmt(jmvcore::.("Small eligible reference-standard sample (n={n}). Confidence intervals may be wide and estimates unstable, especially when either gold-standard class is sparse. Interpret the interval widths and class-specific denominators; no universal sample-size cutoff guarantees adequate precision."), n = n_gold_eligible)
                    )
                }

                # Prevalence uses the selected reference pair only.
                n_diseased <- sum(gold_chars == goldPLevel, na.rm = TRUE)
                prevalence <- if (n_gold_eligible > 0) n_diseased / n_gold_eligible else NA_real_

                if (!is.na(prevalence) && (prevalence < 0.05 || prevalence > 0.95)) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = jmvcore::.("Extreme Disease Prevalence"),
                        content = .fmt(jmvcore::.("Extreme disease prevalence among eligible reference results: {pct}% ({diseased}/{observed} cases). PPV and NPV are highly sensitive to prevalence and may not generalize to populations with different disease rates. Sensitivity and specificity can also vary across settings and case mix."),
                            pct = private$.pctAdaptive(prevalence), diseased = n_diseased, observed = n_gold_eligible)
                    )
                }

                return(list(
                    data = mydata,
                    goldVariable = goldVariable,
                    goldPLevel = goldPLevel,
                    goldNLevel = private$.opt("goldNegative")
                ))
            },

            # Initialize table structures
            # The epiR tables report the same statistic list for every test, so their rows are
            # known before any data is seen: .init() lays them down and .run() only fills values.
            .epirStatLabels = function() {
                c(ap = jmvcore::.("Apparent prevalence"),
                  tp = jmvcore::.("True prevalence"),
                  se = jmvcore::.("Test sensitivity"),
                  sp = jmvcore::.("Test specificity"),
                  diag.ac = jmvcore::.("Diagnostic accuracy"),
                  diag.or = jmvcore::.("Diagnostic odds ratio"),
                  nndx = jmvcore::.("Number needed to diagnose"),
                  youden = jmvcore::.("Youden's index"),
                  pv.pos = jmvcore::.("Positive predictive value"),
                  pv.neg = jmvcore::.("Negative predictive value"),
                  lr.pos = jmvcore::.("Likelihood ratio of a positive test"),
                  lr.neg = jmvcore::.("Likelihood ratio of a negative test"),
                  p.rout = jmvcore::.("Proportion of subjects with the outcome ruled out"),
                  p.rin = jmvcore::.("Proportion of subjects with the outcome ruled in"),
                  # FP/(FP+TN) and FN/(TP+FN): rates among reference-negative and reference-positive
                  # cases. "Proportion of false positives" read just as well as the false discovery
                  # rate two rows below (IR-decisioncompare-05).
                  p.tpdn = jmvcore::.("False positive rate (1 - specificity)"),
                  p.tndp = jmvcore::.("False negative rate (1 - sensitivity)"),
                  p.dntp = jmvcore::.("False Discovery Rate"),
                  p.dptn = jmvcore::.("False Omission Rate"))
            },
            .epirRatioStats = function() {
                c("ap", "tp", "se", "sp", "diag.ac", "pv.pos", "pv.neg",
                  "p.tpdn", "p.tndp", "p.dntp", "p.dptn")
            },

            .initializeTables = function() {
                # epiR confidence-interval tables: fixed statistic set, one table per test
                labels <- private$.epirStatLabels()
                for (i in 1:3) {
                    table_name <- paste0("epirTable", i)
                    if (table_name %in% names(self$results))
                        for (key in private$.epirRatioStats())
                            self$results[[table_name]]$addRow(rowKey = key,
                                values = list(statsnames = unname(labels[[key]])))
                }

                # Add rows to contingency tables
                for (i in 1:3) {
                    table_name <- paste0("cTable", i)
                    if (table_name %in% names(self$results)) {
                        cTable <- self$results[[table_name]]
                        # Displayed labels are translated; the rowKeys stay English so
                        # .populateContingencyTable()'s setRow() calls keep matching.
                        cTable$addRow(rowKey = "Test Positive", values = list(newtest = jmvcore::.("Test Positive")))
                        cTable$addRow(rowKey = "Test Negative", values = list(newtest = jmvcore::.("Test Negative")))
                        cTable$addRow(rowKey = "Total", values = list(newtest = jmvcore::.("Total")))
                    }
                }
            },

            # Display original data tables
            .displayOriginalData = function(processed_data) {
                # Bound the original level cross-product before table() allocates it.
                # Dropping unused levels here affects display only, not diagnostic recoding.
                mydata <- droplevels(processed_data$data)
                goldVariable <- processed_data$goldVariable
                test_vars <- private$.getTestVariables()
                n_gold <- nlevels(as.factor(mydata[[goldVariable]]))
                n_test <- vapply(test_vars, function(v) {
                    nlevels(as.factor(mydata[[v]]))
                }, integer(1))
                if (any(as.double(n_gold) * n_test > 10000)) {
                    private$.addNotice(
                        type = "WARNING",
                        title = jmvcore::.("Original Data Display Omitted"),
                        content = jmvcore::.("An original-data cross-tabulation would exceed 10,000 cells. The original-data display was omitted; diagnostic estimates and comparisons are unaffected.")
                    )
                    return(invisible(NULL))
                }

                # Create frequency table for gold standard
                freq_table <- table(mydata[[goldVariable]])
                self$results$text1$setContent(freq_table)

                # Create cross-tabulation tables
                if (length(test_vars) > 0) {
                    html_tables <- ""
                    for (test_var in test_vars) {
                        cross_tab <- table(mydata[[test_var]], mydata[[goldVariable]])
                        html_table <- knitr::kable(
                            cross_tab,
                            format = "html",
                            caption = .fmt(jmvcore::.("Cross-tabulation of {test} and {gold}"),
                                test = private$.safeHtmlOutput(test_var),
                                gold = private$.safeHtmlOutput(goldVariable)
                            )
                        )
                        html_tables <- paste(html_tables, html_table, "<br><br>")
                    }
                    self$results$text2$setContent(html_tables)
                }
            },

            # Get valid test variables
            .getTestVariables = function() {
                testVariables <- c(self$options$test1, self$options$test2, self$options$test3)
                testVariables <- testVariables[!is.null(testVariables) & testVariables != ""]
                return(testVariables)
            },

            # Get test positive levels mapping
            .getTestPositives = function() {
                testPositives <- list()
                if (!is.null(self$options$test1) && self$options$test1 != "") {
                    testPositives[[self$options$test1]] <- self$options$test1Positive
                }
                if (!is.null(self$options$test2) && self$options$test2 != "") {
                    testPositives[[self$options$test2]] <- self$options$test2Positive
                }
                if (!is.null(self$options$test3) && self$options$test3 != "") {
                    testPositives[[self$options$test3]] <- self$options$test3Positive
                }
                return(testPositives)
            },

            # Strata in the stratifier's level order (grade, stage), not in row order.
            .strataOrder = function(x) {
                values <- as.character(x)
                values <- values[!is.na(values)]
                if (is.factor(x)) intersect(levels(x), values) else unique(values)
            },

            # Explicit negative levels, keyed by test variable name (mirrors
            # .getTestPositives). Absent entries stay NULL.
            .getTestNegatives = function() {
                testNegatives <- list()
                for (i in 1:3) {
                    testVar <- private$.opt(paste0("test", i))
                    negLevel <- private$.opt(paste0("test", i, "Negative"))
                    if (!is.null(testVar))
                        testNegatives[testVar] <- list(negLevel)
                }
                return(testNegatives)
            },

            # Process all tests and calculate metrics
            .processAllTests = function(processed_data) {
                mydata <- processed_data$data
                goldVariable <- processed_data$goldVariable
                goldPLevel <- processed_data$goldPLevel

                testVariables <- private$.getTestVariables()
                testPositives <- private$.getTestPositives()
                testNegatives <- private$.getTestNegatives()

                # cTable<i>/epirTable<i> are gated in .r.yaml on SLOT test<i>
                # (visible: (length(test2) > 0) and so on), so the table index must be
                # the slot the variable came from -- NOT its position in the compacted
                # testVariables vector, which skips empty slots. With Test 1 empty the
                # two indexings disagreed by one and test 3's 2x2 and CIs were written
                # under the static heading "Test 2 - Recoded Data" while "Test 3" stayed
                # blank. Duplicate selections are already rejected in .validateInputs(),
                # so match() is unambiguous here.
                slotVariables <- vapply(1:3, function(k) {
                    v <- private$.opt(paste0("test", k))
                    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
                }, character(1))

                test_results <- list()

                for (i in seq_along(testVariables)) {
                    # Each test rebuilds a 2x2, its metrics and (optionally) an epiR CI
                    # table; without a checkpoint jamovi cannot interrupt or show progress.
                    private$.checkpoint()

                    testVariable <- testVariables[i]
                    testPLevel <- testPositives[[testVariable]]
                    slot_index <- match(testVariable, slotVariables)
                    if (is.na(slot_index)) slot_index <- i

                    # Every selected test and reference uses its explicit positive/negative pair.
                    result <- private$.processSingleTest(
                        mydata, testVariable, testPLevel, goldVariable, goldPLevel, slot_index,
                        testNLevel = testNegatives[[testVariable]],
                        goldNLevel = private$.opt("goldNegative")
                    )

                    test_results[[testVariable]] <- result
                }

                return(test_results)
            },

            # Process a single test and calculate all metrics
            .processSingleTest = function(mydata, testVariable, testPLevel, goldVariable, goldPLevel, test_index,
                                          testNLevel = NULL, goldNLevel = NULL) {
                test_values <- forcats::as_factor(mydata[[testVariable]])
                gold_values <- forcats::as_factor(mydata[[goldVariable]])
                # The SLOT label, not the variable name a second time: passing
                # testVariable as both arguments produced the doubled "VAR (VAR)" in
                # every level error.
                test_label <- sprintf(jmvcore::.("Test %d"), test_index)
                private$.assertLevelExists(test_values, testPLevel, testVariable, test_label)

                exclude <- isTRUE(self$options$excludeIndeterminate)
                as_binary_factor <- function(values) {
                    factor(ifelse(values, "Positive", "Negative"),
                        levels = c("Positive", "Negative"))
                }
                test_binary <- as_binary_factor(private$.mapBinary(
                    test_values, testPLevel, testNLevel, testVariable))
                gold_binary <- as_binary_factor(private$.mapBinary(
                    gold_values, goldPLevel, goldNLevel, goldVariable))
                test_raw_na <- is.na(as.character(test_values))
                gold_raw_na <- is.na(as.character(gold_values))
                other_row <- (!test_raw_na & is.na(test_binary)) | (!gold_raw_na & is.na(gold_binary))
                missing_row <- !other_row & (test_raw_na | gold_raw_na)
                determinate <- !is.na(test_binary) & !is.na(gold_binary)
                n_total <- length(determinate)
                n_analyzed <- sum(determinate)
                n_excluded <- n_total - n_analyzed
                n_missing <- sum(missing_row)
                n_indeterminate <- sum(other_row)

                if (exclude && n_analyzed == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = jmvcore::.("No Determinate Cases"),
                        content = .fmt(jmvcore::.("No cases remain for {test} after applying the selected positive and negative levels."),
                            test = testVariable)
                    )
                    stop(jmvcore::.("Validation failed"), call. = FALSE)
                }

                if (n_indeterminate > 0) {
                    private$.addNotice(
                        type = "INFO",
                        title = jmvcore::.("Other Results Excluded"),
                        content = .fmt(jmvcore::.("{test}: {excluded} of {total} rows have observed results outside the selected positive/negative pairs. Estimates are conditional on the selected levels; exclusions are reported separately from missing results."),
                            test = testVariable, excluded = n_indeterminate, total = n_total)
                    )
                }

                if (n_missing > 0) {
                    private$.addNotice(
                        type = "INFO",
                        title = jmvcore::.("Missing Results Excluded Per Test"),
                        content = .fmt(jmvcore::.("{test}: {missing} of {total} rows lacked this test result or the reference result and were excluded only from analyses requiring those values."),
                            test = testVariable, missing = n_missing, total = n_total
                        )
                    )
                }

                # Create confusion table with zero-filled cells
                conf_table <- private$.buildConfusionMatrix(test_binary[determinate], gold_binary[determinate])

                # Extract confusion matrix values
                TP <- conf_table["Positive", "Positive"]
                FP <- conf_table["Positive", "Negative"]
                FN <- conf_table["Negative", "Positive"]
                TN <- conf_table["Negative", "Negative"]

                # A comfortable total N can still rest on a tiny gold-standard class.
                # The sample-size notice keys on the TOTAL observed reference denominator
                # and the prevalence notice on overall prevalence, so n=120 with 9
                # diseased passed both in silence while the printed sensitivity rested on
                # 9 patients. Disclose the two class denominators.
                n_gold_pos <- TP + FN
                n_gold_neg <- TN + FP
                if (min(n_gold_pos, n_gold_neg) < 30) {
                    # The 95% CI option is disabled while a population prevalence is supplied, so
                    # the advice must say how to reach it.
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = jmvcore::.("Sparse Reference-Standard Class"),
                        content = if (isTRUE(self$options$pp))
                            .fmt(jmvcore::.("{test}: sensitivity rests on {positives} reference-positive cases and specificity on {negatives} reference-negative cases. A percentage computed on a denominator this small is far less precise than the overall analyzed N suggests. Switch off the supplied prevalence and enable the 95% CI option to read the interval before quoting either figure."),
                                test = testVariable, positives = n_gold_pos, negatives = n_gold_neg)
                        else
                            .fmt(jmvcore::.("{test}: sensitivity rests on {positives} reference-positive cases and specificity on {negatives} reference-negative cases. A percentage computed on a denominator this small is far less precise than the overall analyzed N suggests. Enable the 95% CI option and read the interval before quoting either figure."),
                                test = testVariable, positives = n_gold_pos, negatives = n_gold_neg)
                    )
                }

                # Calculate basic metrics
                metrics <- private$.calculateDiagnosticMetrics(TP, FP, FN, TN, testVariable)

                # Populate contingency table
                private$.populateContingencyTable(test_index, testVariable, TP, FP, FN, TN)

                # Calculate confidence intervals if requested
                if (self$options$ci) {
                    private$.populateConfidenceIntervals(test_index, testVariable, conf_table)
                }

                return(list(
                    variable = testVariable,
                    metrics = metrics,
                    conf_table = conf_table,
                    test_results = test_binary,
                    gold_reference = gold_binary,
                    determinate = determinate,
                    n_total = as.integer(n_total),
                    n_analyzed = as.integer(n_analyzed),
                    n_excluded = as.integer(n_excluded),
                    n_missing = as.integer(n_missing),
                    n_indeterminate = as.integer(n_indeterminate),
                    excluded_rate = if (n_total > 0) n_excluded / n_total else NA_real_,
                    TP = TP, FP = FP, FN = FN, TN = TN
                ))
            },

            # Helper function to calculate diagnostic metrics with prevalence logic
            .calculateDiagnosticMetrics = function(TP, FP, FN, TN, test_label, add_notices = TRUE) {
                TotalPop <- TP + TN + FP + FN
                DiseaseP <- TP + FN
                DiseaseN <- TN + FP
                TestP <- TP + FP
                TestN <- TN + FN

                # Basic metrics
                rate <- function(x, n, metric) {
                    private$.computeRate(x, n, metric, test_label, warn = add_notices)
                }
                Sens <- rate(TP, DiseaseP, jmvcore::.("sensitivity"))
                Spec <- rate(TN, DiseaseN, jmvcore::.("specificity"))
                AccurT <- rate(TP + TN, TotalPop, jmvcore::.("accuracy"))

                # Calculate PPV and NPV based on prevalence setting
                if (self$options$pp && !is.na(self$options$pprob)) {
                    # Use provided prevalence with Bayes' theorem
                    PriorProb <- self$options$pprob
                    if (is.na(Sens) || is.na(Spec)) {
                        PPV <- NA_real_
                        NPV <- NA_real_
                    } else {
                        denom_ppv <- (Sens * PriorProb) + ((1 - Spec) * (1 - PriorProb))
                        denom_npv <- ((1 - Sens) * PriorProb) + (Spec * (1 - PriorProb))
                        PPV <- rate(Sens * PriorProb, denom_ppv, jmvcore::.("positive predictive value"))
                        NPV <- rate(Spec * (1 - PriorProb), denom_npv, jmvcore::.("negative predictive value"))
                    }
                } else {
                    # Use sample-based calculation
                    PPV <- rate(TP, TestP, jmvcore::.("positive predictive value"))
                    NPV <- rate(TN, TestN, jmvcore::.("negative predictive value"))
                    PriorProb <- rate(DiseaseP, TotalPop, jmvcore::.("prevalence"))
                }

                # Likelihood ratios.
                # The 0.5 continuity correction belongs ONLY to the ratio whose denominator
                # is empty while its numerator is not (.diagRatioRules): LR+ when FP == 0 and
                # TP > 0, LR- when TN == 0 and FN > 0. An empty numerator gives an exact 0.
                # Correcting on any zero cell (commit 9f0e394ca, IR2-decisioncompare-01)
                # printed LR+ = 1.09 for TP=0, FP=3, FN=10, TN=80, whose exact LR+ is 0: the
                # +0.5 weighs more in the smaller diseased group and moved a test that never
                # found a diseased case to the rule-in side of 1, irreproducible from the raw
                # Sens and Spec printed beside it.
                zero_cells <- any(c(TP, FP, FN, TN) == 0)
                cc <- private$ZERO_CELL_CONTINUITY
                both_classes <- !is.na(Sens) && !is.na(Spec)
                # A test that never called positive (TP + FP == 0) has LR+ = 0/0, and one that
                # never called negative (FN + TN == 0) has LR- = 0/0. Neither ratio is infinite,
                # so the correction does not apply: on 0/0 it returned (TN+1)/(FN+1) and
                # banded a test with no positive call as "Moderate positive evidence"
                # (VAL-decisioncompare-02). Both are reported as not estimable.
                ratio_rules <- if (both_classes) .diagRatioRules(TP, FP, FN, TN)
                    else list(lrp = "undefined", lrn = "undefined")
                lrp_undefined <- both_classes && ratio_rules$lrp == "undefined"
                lrn_undefined <- both_classes && ratio_rules$lrn == "undefined"
                lrp_corrected <- ratio_rules$lrp == "corrected"
                lrn_corrected <- ratio_rules$lrn == "corrected"

                # Each ratio is formed from its two class rates, each from its own counts:
                # FP/(FP+TN), not 1 - Spec, whose cancellation turned an exact LR+ of 10 into
                # 9.999999999999991 and moved it out of the ">= 10" band
                # (VAL-decisioncompare-09). Algebraically identical to Sens/(1 - Spec) and
                # (1 - Sens)/Spec, and to the corrected forms with 0.5 added to every cell.
                # A "zero" ratio (empty numerator) takes the raw form, which is exactly 0.
                LRP <- if (!both_classes || lrp_undefined) {
                    NA_real_
                } else if (lrp_corrected) {
                    ((TP + cc) / (TP + FN + 2 * cc)) / ((FP + cc) / (FP + TN + 2 * cc))
                } else {
                    (TP / (TP + FN)) / (FP / (FP + TN))
                }
                LRN <- if (!both_classes || lrn_undefined) {
                    NA_real_
                } else if (lrn_corrected) {
                    ((FN + cc) / (TP + FN + 2 * cc)) / ((TN + cc) / (TN + FP + 2 * cc))
                } else {
                    (FN / (TP + FN)) / (TN / (TN + FP))
                }

                # "undefined" also covers an infinite ratio whose 0.5-corrected value would fall
                # to or below 1 (.diagRatioRules): the correction would reverse its direction.
                lrp_infinite <- lrp_undefined && (TP + FP) > 0
                lrn_infinite <- lrn_undefined && (FN + TN) > 0
                if (isTRUE(add_notices) && (lrp_undefined || lrn_undefined)) {
                    undefined_text <- c(
                        if (lrp_undefined && !lrp_infinite)
                            .fmt(jmvcore::.("{test}: the test was never positive in these cases, so LR+ is 0/0 and is left blank (its sensitivity is 0 and its specificity 1 here). A continuity correction cannot create rule-in evidence for a test that made no positive call."),
                                test = test_label),
                        if (lrp_infinite)
                            .fmt(jmvcore::.("{test}: no false positive was observed, so LR+ is infinite, and adding 0.5 to every cell would give a value of 1 or less, reversing its direction; LR+ is left blank."),
                                test = test_label),
                        if (lrn_undefined && !lrn_infinite)
                            .fmt(jmvcore::.("{test}: the test was never negative in these cases, so LR- is 0/0 and is left blank (its sensitivity is 1 and its specificity 0 here). A continuity correction cannot create rule-out evidence for a test that made no negative call."),
                                test = test_label),
                        if (lrn_infinite)
                            .fmt(jmvcore::.("{test}: no true negative was observed, so LR- is infinite, and adding 0.5 to every cell would give a value of 1 or less, reversing its direction; LR- is left blank."),
                                test = test_label))
                    private$.addNotice(
                        type = "WARNING",
                        title = jmvcore::.("Likelihood Ratio Not Estimable"),
                        content = paste(undefined_text, collapse = " ")
                    )
                }

                lr_corrected <- isTRUE(lrp_corrected) || isTRUE(lrn_corrected)
                if (lr_corrected && isTRUE(add_notices)) {
                    # Name the ratio that was corrected and the value it produced: the
                    # old notice said only that "a correction was applied" and gave no
                    # number, so a reviewer recomputing from the printed 2x2 got a
                    # different figure with nothing to explain the gap.
                    corrected_label <- if (isTRUE(lrp_corrected) && isTRUE(lrn_corrected))
                        jmvcore::.("LR+ and LR-")
                    else if (isTRUE(lrp_corrected))
                        jmvcore::.("LR+")
                    else
                        jmvcore::.("LR-")
                    # sprintf, not format(): this package @imports jmvcore, whose
                    # format() masks base::format() inside the namespace.
                    corrected_value <- paste(
                        sprintf("%.3f", c(if (isTRUE(lrp_corrected)) LRP, if (isTRUE(lrn_corrected)) LRN)),
                        collapse = ", ")
                    private$.addNotice(
                        type = "INFO",
                        title = jmvcore::.("Zero Cell Continuity Correction"),
                        content = .fmt(jmvcore::.("{test}: {ratio} would be undefined because the cell in its denominator is empty, so it was computed after adding {correction} to all four cells of the 2x2, giving {shown}. Every other statistic in the same row, including the likelihood ratio that was not affected, uses the raw counts. Interpret the corrected value cautiously."),
                            test = test_label, ratio = corrected_label,
                            correction = cc, shown = corrected_value)
                    )
                }

                return(list(
                    Sens = Sens, Spec = Spec, AccurT = AccurT,
                    PPV = PPV, NPV = NPV, LRP = LRP, LRN = LRN,
                    PriorProb = PriorProb,
                    zero_cells = zero_cells,
                    lr_corrected = lr_corrected,
                    lrp_corrected = isTRUE(lrp_corrected),
                    lrn_corrected = isTRUE(lrn_corrected),
                    lrp_undefined = isTRUE(lrp_undefined),
                    lrn_undefined = isTRUE(lrn_undefined),
                    lrp_infinite = isTRUE(lrp_infinite),
                    lrn_infinite = isTRUE(lrn_infinite)
                ))
            },

            # Populate contingency table for a specific test
            .populateContingencyTable = function(test_index, testVariable, TP, FP, FN, TN) {
                table_name <- paste0("cTable", test_index)
                if (table_name %in% names(self$results)) {
                    cTable <- self$results[[table_name]]

                    cTable$setRow(
                        rowKey = "Test Positive",
                        values = list(
                            newtest = jmvcore::.("Test Positive"),
                            GP = TP, GN = FP, Total = TP + FP
                        )
                    )

                    cTable$setRow(
                        rowKey = "Test Negative",
                        values = list(
                            newtest = jmvcore::.("Test Negative"),
                            GP = FN, GN = TN, Total = FN + TN
                        )
                    )

                    cTable$setRow(
                        rowKey = "Total",
                        values = list(
                            newtest = jmvcore::.("Total"),
                            GP = TP + FN, GN = FP + TN, Total = TP + FP + FN + TN
                        )
                    )

                    # Add footnotes if requested
                    if (self$options$fnote) {
                        private$.addContingencyTableFootnotes(cTable)
                        if (any(c(TP, FP, FN, TN) == 0)) {
                            # The correction is no longer applied to both ratios on any
                            # zero cell, so this footnote must no longer claim it was.
                            cTable$addFootnote(rowKey = "Total", col = "Total", jmvcore::.("Zero cell detected. A 0.5 continuity correction is applied only to a likelihood ratio whose own denominator cell is empty while its numerator is not (FP = 0 with TP > 0 for LR+, TN = 0 with FN > 0 for LR-), and only when the corrected value stays above 1; an empty numerator gives an exact 0, a ratio that is 0/0 or whose correction would fall to 1 or below is left blank, and every other statistic uses these raw counts. Interpret cautiously."))
                        }
                    }
                }
            },

            # Add footnotes to contingency tables
            .addContingencyTableFootnotes = function(cTable) {
                cTable$addFootnote(rowKey = "Test Positive", col = "GP", jmvcore::.("True Positive (TP)"))
                cTable$addFootnote(rowKey = "Test Positive", col = "GN", jmvcore::.("False Positive (FP)"))
                cTable$addFootnote(rowKey = "Test Negative", col = "GP", jmvcore::.("False Negative (FN)"))
                cTable$addFootnote(rowKey = "Test Negative", col = "GN", jmvcore::.("True Negative (TN)"))
            },

            # Populate comparison table with all test results
            .populateComparisonTable = function(test_results) {
                comparisonTable <- self$results$comparisonTable

                .deleteTableRows(comparisonTable)
                comparisonTable$setNote(
                    "interpretation_scope",
                    jmvcore::.("Qualitative performance labels are illustrative summaries of the observed sample only. They are not clinical guides, validated decision rules, or recommendations for patient care.")
                )

                for (test_index in seq_along(test_results)) {
                    test_name <- names(test_results)[test_index]
                    result <- test_results[[test_name]]
                    metrics <- result$metrics

                    # Index-based keys cannot collide when display labels contain the
                    # same punctuation or normalize to the same R name.
                    row_key <- paste0("test_", test_index)


                    comparisonTable$addRow(
                        rowKey = row_key,
                        values = list(
                            test = test_name,
                            n = result$n_analyzed,
                            excluded = result$n_excluded,
                            excludedRate = result$excluded_rate,
                            Sens = metrics$Sens,
                            Spec = metrics$Spec,
                            AccurT = metrics$AccurT,
                            PPV = metrics$PPV,
                            NPV = metrics$NPV,
                            # A not-estimable ratio is "" (blank, footnoted below): NA renders
                            # as "NaN" in jamovi.
                            LRP = if (is.na(metrics$LRP)) "" else metrics$LRP,
                            LRN = if (is.na(metrics$LRN)) "" else metrics$LRN
                        )
                    )

                    if (isTRUE(metrics$lrp_corrected) || isTRUE(metrics$lrn_corrected)) {
                        # Always shown, independent of the footnote option: this cell was
                        # computed by a different rule from the counts in the 2x2.
                        corrected_cols <- c(if (isTRUE(metrics$lrp_corrected)) "LRP",
                                            if (isTRUE(metrics$lrn_corrected)) "LRN")
                        for (col in corrected_cols)
                            comparisonTable$addFootnote(
                                rowKey = row_key, col = col,
                                .fmt(jmvcore::.("Denominator cell empty; computed after adding {correction} to all four cells of the 2x2. Not reproducible from the sensitivity and specificity in this row, which use the raw counts."),
                                    correction = private$ZERO_CELL_CONTINUITY)
                            )
                    }

                    if (isTRUE(metrics$lrp_undefined))
                        comparisonTable$addFootnote(rowKey = row_key, col = "LRP",
                            if (isTRUE(metrics$lrp_infinite))
                                jmvcore::.("Not estimable: FP = 0 makes LR+ infinite, and the 0.5 correction would reverse its direction.")
                            else
                                jmvcore::.("Not estimable: the test was never positive, so LR+ is 0/0."))
                    if (isTRUE(metrics$lrn_undefined))
                        comparisonTable$addFootnote(rowKey = row_key, col = "LRN",
                            if (isTRUE(metrics$lrn_infinite))
                                jmvcore::.("Not estimable: TN = 0 makes LR- infinite, and the 0.5 correction would reverse its direction.")
                            else
                                jmvcore::.("Not estimable: the test was never negative, so LR- is 0/0."))

                    # Add clinical interpretation
                    clinical_interpretation <- private$.generateClinicalInterpretation(metrics)
                    lr_suffix <- if (isTRUE(metrics$lrp_undefined) || isTRUE(metrics$lrn_undefined))
                        jmvcore::.("(a likelihood ratio is not estimable)")
                    else if (isTRUE(metrics$zero_cells))
                        jmvcore::.("(zero cell; LR may be unstable)")
                    else ""
                    comparisonTable$addFormat(rowKey = row_key, col = "test", format = jmvcore::Cell.BEGIN_GROUP)
                    comparisonTable$addRow(
                        rowKey = paste0(row_key, "_interp"),
                        values = list(
                            test = paste0("  \u{2192} ", clinical_interpretation,
                                if (nzchar(lr_suffix)) paste0(" ", lr_suffix) else ""),
                            n = "", excluded = "", excludedRate = "",
                            Sens = "", Spec = "", AccurT = "", PPV = "", NPV = "", LRP = "", LRN = ""
                        )
                    )
                    comparisonTable$addFormat(rowKey = paste0(row_key, "_interp"), col = "test", format = jmvcore::Cell.END_GROUP)

                    # Add footnotes if requested
                    if (self$options$fnote) {
                        private$.addComparisonTableFootnotes(comparisonTable, row_key)
                        # Two complete alternative sentences; the previous form spliced a
                        # parenthetical fragment into an English frame ("PPV uses" + frag).
                        comparisonTable$addFootnote(
                            rowKey = row_key, col = "PPV",
                            if (self$options$pp)
                                jmvcore::.("PPV uses the supplied population prevalence.")
                            else
                                jmvcore::.("PPV uses the prevalence observed in this sample.")
                        )
                        comparisonTable$addFootnote(
                            rowKey = row_key, col = "NPV",
                            if (self$options$pp)
                                jmvcore::.("NPV uses the supplied population prevalence.")
                            else
                                jmvcore::.("NPV uses the prevalence observed in this sample.")
                        )
                    }
                }
            },

            # Compute confidence interval for a proportion (Wilson, logit, or exact)
            .proportionCI = function(x, n, method = "wilson", alpha = 0.05) {
                if (n == 0) {
                    return(list(est = NA_real_, lower = NA_real_, upper = NA_real_))
                }
                p_hat <- x / n
                z <- qnorm(1 - alpha / 2)

                if (method == "logit") {
                    if (p_hat == 0 || p_hat == 1) {
                        # Boundary: the logit interval is undefined, so fall back to exact
                        # (Clopper-Pearson) and say which method produced the row.
                        bt <- binom.test(x, n, conf.level = 1 - alpha)
                        return(list(est = p_hat, lower = bt$conf.int[1], upper = bt$conf.int[2],
                                    method_used = "exact"))
                    }
                    logit_p <- log(p_hat / (1 - p_hat))
                    se_logit <- sqrt(1 / (n * p_hat * (1 - p_hat)))
                    logit_lower <- logit_p - z * se_logit
                    logit_upper <- logit_p + z * se_logit
                    lower <- 1 / (1 + exp(-logit_lower))
                    upper <- 1 / (1 + exp(-logit_upper))
                } else if (method == "exact") {
                    bt <- binom.test(x, n, conf.level = 1 - alpha)
                    lower <- bt$conf.int[1]
                    upper <- bt$conf.int[2]
                } else {
                    # Wilson score (default)
                    denom <- 1 + z^2 / n
                    center <- (p_hat + z^2 / (2 * n)) / denom
                    margin <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * n)) / n) / denom
                    lower <- max(0, center - margin)
                    upper <- min(1, center + margin)
                }
                list(est = p_hat, lower = lower, upper = upper, method_used = method)
            },

            # Populate OPA table with confidence intervals and noninferiority result
            .populateOPATable = function(test_results) {
                opaTable <- self$results$opaTable
                .deleteTableRows(opaTable)

                ci_method <- self$options$ciMethod
                use_criterion <- isTRUE(private$.opt("useOpaCriterion", FALSE))
                opa_criterion <- if (use_criterion) self$options$niMargin / 100 else NA_real_

                # Update table note to reflect CI method
                method_label <- switch(ci_method,
                    wilson = jmvcore::.("Wilson score"),
                    logit = jmvcore::.("Logit"),
                    exact = jmvcore::.("Clopper-Pearson (exact)"),
                    jmvcore::.("Wilson score")
                )
                # Each piece is a complete sentence with its own msgid; the separators
                # live outside the msgids so no translation carries trailing padding.
                criterion_note <- if (use_criterion) {
                    .fmt(jmvcore::.("User-requested minimum OPA criterion: {pct}%. 'Criterion met' means the lower confidence bound exceeds this descriptive threshold."),
                        pct = self$options$niMargin)
                } else {
                    jmvcore::.("No minimum OPA criterion was requested.")
                }
                opaTable$setNote("note", paste(
                    .fmt(jmvcore::.("OPA = (TP + TN) / analyzed N. {method} 95% confidence intervals."),
                        method = method_label),
                    criterion_note,
                    jmvcore::.("Any criterion is descriptive, not a noninferiority analysis against a comparator, regulatory guidance, or clinical guidance. OPA does not correct for chance agreement and should not be used alone to characterize diagnostic performance.")
                ))

                for (test_index in seq_along(test_results)) {
                    test_name <- names(test_results)[test_index]
                    result <- test_results[[test_name]]
                    concordant <- result$TP + result$TN
                    total <- result$TP + result$FP + result$FN + result$TN

                    ci <- private$.proportionCI(concordant, total, method = ci_method)

                    criterion_result <- if (!use_criterion) {
                        jmvcore::.("Not requested")
                    } else if (is.na(ci$lower)) {
                        jmvcore::.("N/A")
                    } else if (ci$lower > opa_criterion) {
                        jmvcore::.("Yes")
                    } else {
                        jmvcore::.("No")
                    }

                    opaTable$addRow(
                        rowKey = paste0("test_", test_index),
                        values = list(
                            test = test_name,
                            concordant = as.integer(concordant),
                            total = as.integer(total),
                            excluded = result$n_excluded,
                            opa = ci$est,
                            lower = ci$lower,
                            upper = ci$upper,
                            niMargin = opa_criterion,
                            niResult = criterion_result
                        )
                    )
                    # The note names the requested method; a row computed by the logit
                    # fallback must say it is not a logit interval (VAL-decisioncompare-06).
                    if (identical(ci_method, "logit") && identical(ci$method_used, "exact"))
                        opaTable$addFootnote(
                            rowKey = paste0("test_", test_index), col = "lower",
                            jmvcore::.("The logit interval is undefined at 0% or 100% agreement, so this row shows Clopper-Pearson (exact) limits.")
                        )
                }
            },

            # Populate stratified comparison table
            .populateStratifiedTable = function(processed_data, test_results) {
                stratTable <- self$results$stratifiedTable
                .deleteTableRows(stratTable)

                strat_var <- self$options$stratify
                mydata <- processed_data$data
                if (is.null(strat_var) || !nzchar(strat_var) || !strat_var %in% colnames(mydata)) return()

                stratum_values <- as.character(mydata[[strat_var]])
                strata <- private$.strataOrder(mydata[[strat_var]])
                n_missing_stratum <- sum(is.na(stratum_values))
                if (n_missing_stratum > 0) {
                    private$.addNotice(
                        type = "INFO",
                        title = jmvcore::.("Missing Stratifier Values"),
                        content = .fmt(jmvcore::.("{rows} row(s) have no value for {variable}. They remain in overall per-test results but are omitted from subgroup rows."),
                            rows = n_missing_stratum, variable = strat_var
                        )
                    )
                }

                row_idx <- 0
                for (stratum in strata) {
                    # One 2x2 plus a full metric set per test per stratum; checkpoint so a
                    # many-level stratifier stays interruptible.
                    private$.checkpoint()

                    stratum_idx <- !is.na(stratum_values) & stratum_values == stratum
                    n_stratum <- sum(stratum_idx)
                    if (n_stratum == 0) next

                    for (tv in names(test_results)) {
                        row_idx <- row_idx + 1
                        result <- test_results[[tv]]
                        valid <- stratum_idx & result$determinate
                        n_analyzed <- sum(valid)
                        n_excluded <- n_stratum - n_analyzed

                        conf <- private$.buildConfusionMatrix(
                            result$test_results[valid], result$gold_reference[valid]
                        )
                        TP <- conf["Positive", "Positive"]
                        FP <- conf["Positive", "Negative"]
                        FN <- conf["Negative", "Positive"]
                        TN <- conf["Negative", "Negative"]
                        total <- TP + FP + FN + TN
                        metrics <- private$.calculateDiagnosticMetrics(
                            TP, FP, FN, TN,
                            .fmt(jmvcore::.("{test} in {stratum}"), test = tv, stratum = stratum),
                            add_notices = FALSE
                        )
                        opa_val <- if (total > 0) (TP + TN) / total else NA_real_

                        stratTable$addRow(
                            rowKey = row_idx,
                            values = list(
                                stratum = .diagCellText(stratum),
                                n = as.integer(n_analyzed),
                                excluded = as.integer(n_excluded),
                                excludedRate = if (n_stratum > 0) n_excluded / n_stratum else NA_real_,
                                test = tv,
                                Sens = metrics$Sens,
                                Spec = metrics$Spec,
                                AccurT = metrics$AccurT,
                                PPV = metrics$PPV,
                                NPV = metrics$NPV,
                                opa = opa_val
                            )
                        )
                    }
                }

                if (row_idx == 0) {
                    private$.addNotice(
                        type = "WARNING",
                        title = jmvcore::.("Stratification Empty"),
                        content = .fmt(jmvcore::.("No analyzable cases were found in any observed stratum of '{variable}'. Check subgroup values and test/reference availability."),
                            variable = strat_var)
                    )
                }
            },

            # Generate clinical interpretation for test performance
            .generateClinicalInterpretation = function(metrics) {
                sens_pct <- metrics$Sens * 100
                spec_pct <- metrics$Spec * 100
                lrp <- metrics$LRP
                lrn <- metrics$LRN

                # Determine primary clinical strength
                interpretations <- c()

                # The bands are inclusive. A small tolerance keeps a value that sits exactly on an
                # edge in rational arithmetic inside its band despite floating point
                # (VAL-decisioncompare-09). isTRUE() also guards against NA (Sens/Spec are NA when
                # a gold class has zero cases), which would otherwise crash with "missing value
                # where TRUE/FALSE needed".
                at_least <- function(x, edge) isTRUE(x >= edge - 1e-9)
                at_most <- function(x, edge) isTRUE(x <= edge + 1e-9)
                if (at_least(sens_pct, 95) && at_least(spec_pct, 95)) {
                    interpretations <- c(interpretations, jmvcore::.("High sensitivity and high specificity in this sample (both >=95%)"))
                } else if (at_least(sens_pct, 95)) {
                    interpretations <- c(interpretations, jmvcore::.("High sensitivity (>=95%): few false negatives in this sample"))
                } else if (at_least(spec_pct, 95)) {
                    interpretations <- c(interpretations, jmvcore::.("High specificity (>=95%): few false positives in this sample"))
                } else if (at_least(sens_pct, 85) && at_least(spec_pct, 85)) {
                    interpretations <- c(interpretations, jmvcore::.("Moderately high sensitivity and specificity in this sample (both >=85%)"))
                } else if (at_least(sens_pct, 85)) {
                    interpretations <- c(interpretations, jmvcore::.("Moderately high sensitivity (>=85%) in this sample"))
                } else if (at_least(spec_pct, 85)) {
                    interpretations <- c(interpretations, jmvcore::.("Moderately high specificity (>=85%) in this sample"))
                }

                # Likelihood ratio bands as cited (Jaeschke et al. 1994, the radar note's source):
                # > 10 / < 0.1 large, 5-10 / 0.1-0.2 moderate. Exactly 10 or 0.1 is moderate,
                # as in `decision`'s .lrBand(); the strong edge is strict, with the same tolerance.
                above <- function(x, edge) isTRUE(x > edge + 1e-9)
                below <- function(x, edge) isTRUE(x < edge - 1e-9)
                if (above(lrp, 10)) {
                    interpretations <- c(interpretations, jmvcore::.("Strong positive evidence"))
                } else if (at_least(lrp, 5)) {
                    interpretations <- c(interpretations, jmvcore::.("Moderate positive evidence"))
                }

                if (below(lrn, 0.1)) {
                    interpretations <- c(interpretations, jmvcore::.("Strong negative evidence"))
                } else if (at_most(lrn, 0.2)) {
                    interpretations <- c(interpretations, jmvcore::.("Moderate negative evidence"))
                }

                # Combine interpretations or provide fallback. The fallback also fires when
                # every band test is NA (.computeRate returns NA when a gold class has no
                # cases), so separate "measured and below the bands" from "not estimable".
                if (length(interpretations) > 0) {
                    return(paste(interpretations, collapse = "; "))
                } else if (is.na(sens_pct) || is.na(spec_pct)) {
                    return(jmvcore::.("Sensitivity and/or specificity could not be computed here: one gold-standard class has no cases, so that rate has an empty denominator and is reported as blank rather than as a low value. The 2x2 counts behind this row are in the Recoded Data table for this test"))
                } else if (isTRUE(self$options$pp)) {
                    # The 95% CI option is disabled while a prevalence is supplied.
                    return(jmvcore::.("In this sample neither sensitivity nor specificity reached 85%, and neither likelihood ratio reached LR+ >= 5 or LR- <= 0.2. That describes these counts at this operating point, not the test in general; switch off the supplied prevalence and enable the 95% CI option to see how precisely each rate is pinned down by this many cases"))
                } else {
                    return(jmvcore::.("In this sample neither sensitivity nor specificity reached 85%, and neither likelihood ratio reached LR+ >= 5 or LR- <= 0.2. That describes these counts at this operating point, not the test in general; the Confidence Intervals table for this test (95% CI option) shows how precisely each rate is pinned down by this many cases"))
                }
            },

            # Add footnotes to comparison table
            .addComparisonTableFootnotes = function(comparisonTable, test_name) {
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "Sens",
                    jmvcore::.("Sensitivity = TP/(TP+FN) = Probability of positive test among diseased")
                )
                comparisonTable$addFootnote(
                    rowKey = test_name, col = "Spec",
                    jmvcore::.("Specificity = TN/(TN+FP) = Probability of negative test among healthy")
                )
                # With a supplied prevalence the cells hold Bayes values, so the sample formula
                # TP/(TP+FP) would contradict the number beside it (VAL-decisioncompare-07). Same
                # condition as the branch in .calculateDiagnosticMetrics().
                if (isTRUE(self$options$pp) && !is.na(self$options$pprob)) {
                    comparisonTable$addFootnote(
                        rowKey = test_name, col = "PPV",
                        jmvcore::.("Positive Predictive Value = Probability of disease when test is positive, by Bayes' theorem at the supplied prevalence p: Sens x p / (Sens x p + (1 - Spec) x (1 - p))")
                    )
                    comparisonTable$addFootnote(
                        rowKey = test_name, col = "NPV",
                        jmvcore::.("Negative Predictive Value = Probability of being healthy when test is negative, by Bayes' theorem at the supplied prevalence p: Spec x (1 - p) / ((1 - Sens) x p + Spec x (1 - p))")
                    )
                } else {
                    comparisonTable$addFootnote(
                        rowKey = test_name, col = "PPV",
                        jmvcore::.("Positive Predictive Value = TP/(TP+FP) = Probability of disease when test is positive")
                    )
                    comparisonTable$addFootnote(
                        rowKey = test_name, col = "NPV",
                        jmvcore::.("Negative Predictive Value = TN/(TN+FN) = Probability of being healthy when test is negative")
                    )
                }
            },

            # Populate confidence intervals for a specific test
            .populateConfidenceIntervals = function(test_index, testVariable, conf_table) {
                table_name <- paste0("epirTable", test_index)
                if (table_name %in% names(self$results)) {
                    epirTable <- self$results[[table_name]]

                    # Check if epiR package is available
                    if (!requireNamespace("epiR", quietly = TRUE)) {
                        private$.addNotice(
                            type = "ERROR",
                            title = jmvcore::.("Missing epiR Package"),
                            content = jmvcore::.("The epiR package is required for confidence intervals. Install it with install.packages(\"epiR\"), or disable the \"95% CI\" option.")
                        )
                        return()
                    }

                    # Clear any rows left from a previous run before repopulating
                    # rows are scaffolded in .init(); blank the values rather than delete them, so a
                    # failed epiR call leaves empty cells instead of a table that vanishes
                    for (key in private$.epirRatioStats())
                        epirTable$setRow(rowKey = key,
                            values = list(est = NA_real_, lower = NA_real_, upper = NA_real_))

                    # Map epiR statistic codes to display labels. Keying on the `statistic`
                    # code (not row position) keeps labels correct even if epiR changes the
                    # order or length of its summary output.

                    # epi.tests() fits the full statistic set; checkpoint before it so the
                    # per-test CI pass stays interruptible.
                    private$.checkpoint()

                    # The table names its interval method (IR-decisioncompare-03); the method is passed
                    # explicitly so a change of epiR default cannot change the intervals silently.
                    epirTable$setNote(
                        "ci_method",
                        jmvcore::.("Exact (Clopper-Pearson) 95% confidence intervals for each proportion, computed by epiR::epi.tests().")
                    )

                    tryCatch(
                        {
                            epirresult <- epiR::epi.tests(dat = conf_table, method = "exact", conf.level = 0.95)
                            epirresult2 <- as.data.frame(summary(epirresult))

                            # Rows to display (proportions on the 0-1 scale)
                            ratiorows <- c(
                                "ap", "tp", "se", "sp", "diag.ac", "pv.pos", "pv.neg",
                                "p.tpdn", "p.tndp", "p.dntp", "p.dptn"
                            )
                            epirresult_ratio <- epirresult2[epirresult2$statistic %in% ratiorows, , drop = FALSE]

                            # Label rows explicitly by statistic code; fall back to the raw
                            # code for any unmapped statistic rather than silently mislabeling.
                            # as.character: epiR returns `statistic` as a factor, which does not
                            # match a string rowKey.
                            codes <- as.character(epirresult_ratio$statistic)

                            for (i in seq_len(nrow(epirresult_ratio))) {
                                if (!codes[i] %in% private$.epirRatioStats()) next
                                epirTable$setRow(
                                    rowKey = codes[i],
                                    values = list(
                                        est = epirresult_ratio$est[i],
                                        lower = epirresult_ratio$lower[i],
                                        upper = epirresult_ratio$upper[i]
                                    )
                                )
                            }
                        },
                        error = function(e) {
                            n_sample <- sum(conf_table)
                            enhanced_msg <- .fmt(jmvcore::.("Could not calculate confidence intervals for {test} (n={n}). This may be due to insufficient sample size or extreme values. Error: {error}"),
                                test = testVariable, n = n_sample, error = conditionMessage(e)
                            )
                            # Surface the failure so the empty CI table is not silent.
                            private$.addNotice(
                                type = "WARNING",
                                title = jmvcore::.("Confidence Intervals Unavailable"),
                                content = enhanced_msg
                            )
                        }
                    )
                }
            },

            # Perform statistical comparisons between tests with progress reporting
            .performStatisticalComparisons = function(test_results) {
                mcnemarTable <- self$results$mcnemarTable
                diffTable <- self$results$diffTable

                .deleteTableRows(mcnemarTable)
                .deleteTableRows(diffTable)

                # This caveat used to live only inside the showExplanations HTML panel, so a
                # clinician who read the table alone saw a p-value with no hint of what it
                # does and does not test. It belongs on the table that carries the p-value.
                # IR-decisioncompare-02: overall accuracy weights sensitivity and specificity by the
                # prevalence among the compared cases, so the verdict can reverse with the case mix
                # (a significant result as much as a non-significant one).
                mcnemarTable$setNote(
                    "mcnemar_scope",
                    jmvcore::.("McNemar's test here compares overall accuracy, which weights sensitivity and specificity by the prevalence of the target condition among the compared cases. Its verdict can therefore change with the case mix, and under case-control sampling it does not estimate population accuracy. The Differences with 95% Confidence Intervals table also compares sensitivity and specificity separately, each with its own McNemar p-value; those two comparisons do not depend on prevalence.")
                )

                test_names <- names(test_results)
                n_tests <- length(test_names)

                # For 3+ tests, perform Cochran's Q test first
                if (n_tests >= 3) {
                    private$.performCochranQ(test_results, mcnemarTable)
                }

                # Generate all pairwise combinations
                test_pairs <- combn(test_names, 2, simplify = FALSE)
                n_comparisons <- length(test_pairs)

                # Calculate p-value adjustment for multiple comparisons
                # Use Holm-Bonferroni method (less conservative than Bonferroni)
                p_values <- rep(NA_real_, n_comparisons)
                # comparison_names are row KEYS and stay "A vs B" in every language;
                # comparison_labels are what the tables and notices show, from one complete
                # translatable template rather than a bare "vs" fragment.
                comparison_names <- character(n_comparisons)
                comparison_labels <- character(n_comparisons)

                # First pass: compute each McNemar test once and cache it (avoids
                # recomputing the table/test in the display pass below).
                mcnemar_cache <- vector("list", n_comparisons)
                for (i in seq_along(test_pairs)) {
                    # One McNemar test per pair.
                    private$.checkpoint()

                    pair <- test_pairs[[i]]
                    test1 <- pair[1]
                    test2 <- pair[2]
                    comparison_names[i] <- paste(test1, "vs", test2)
                    comparison_labels[i] <- .fmt(jmvcore::.("{first} vs {second}"), first = test1, second = test2)

                    mc <- private$.computeMcNemar(test_results, test1, test2)
                    # [i] <- list(mc), not [[i]] <- mc: assigning NULL with [[ deletes the slot, so a
                    # trailing pair that could not be computed was later read out of bounds.
                    mcnemar_cache[i] <- list(mc)
                    # A pair that cannot be tested must NOT enter p.adjust(): padding it
                    # with a synthetic p = 1 raised m from 2 to 3 and over-corrected the
                    # comparisons that could be computed (a raw p = 0.02 was reported as
                    # 0.06 and read as "no significant difference"). The pair is also
                    # dropped from the table with no row, so disclose it here.
                    if (is.null(mc)) {
                        private$.addNotice(
                            type = "WARNING",
                            title = jmvcore::.("Pairwise Comparison Not Computed"),
                            content = .fmt(jmvcore::.("{comparison} could not be tested: no case has a determinate result for both tests and the reference standard, or the paired table was empty. This pair has no row in the McNemar table, and it was excluded from the multiplicity correction so that the remaining comparisons are not over-corrected."),
                                comparison = comparison_labels[i])
                        )
                    } else {
                        p_values[i] <- mc$result$p.value
                    }
                }

                # Row keys must be unique: test names such as "X vs Y" selected beside X and Y would
                # otherwise build the same key for two different pairs.
                if (anyDuplicated(comparison_names))
                    comparison_names <- make.unique(comparison_names, sep = " #")

                # Holm-Bonferroni over the comparisons that were actually tested: computed pairs
                # with discordant pairs. A pair without discordant pairs keeps its conventional
                # p = 1 unadjusted; counting it in m would over-correct the pairs that were tested,
                # the same reason uncomputable pairs are left out.
                computable <- !is.na(p_values)
                informative <- computable & vapply(seq_len(n_comparisons), function(k)
                    !is.null(mcnemar_cache[[k]]) && mcnemar_cache[[k]]$n_discordant > 0, logical(1))
                n_effective <- sum(informative)
                # Some pair could be compared but none had discordant pairs: McNemar produced no
                # test result, so no copy-ready text may report a verdict.
                private$.mcnemar_uninformative <- any(computable) && !any(informative)
                p_adjusted <- p_values
                if (n_effective > 0)
                    p_adjusted[informative] <- stats::p.adjust(p_values[informative], method = "holm")

                # With a single comparison Holm returns the raw p-value unchanged, so the
                # "(Holm-Bonferroni corrected)" label asserted an adjustment that was
                # never made -- and contradicted the glossary, which says the correction
                # applies from three tests. Label it only when m > 1.
                holm_applied <- n_effective > 1
                private$.holm_applied <- holm_applied

                # One record per pair for the narrative panels: the discordant counts give the
                # direction (b = only the first test correct, c = only the second), and p is the value
                # the table displays, so no sentence can disagree with the table
                # (IR-decisioncompare-01).
                private$.pair_results <- lapply(seq_len(n_comparisons), function(k) {
                    mc <- mcnemar_cache[[k]]
                    list(first = test_pairs[[k]][1], second = test_pairs[[k]][2],
                         informative = isTRUE(informative[k]),
                         b = if (is.null(mc)) NA_integer_ else as.integer(mc$table[1, 2]),
                         c = if (is.null(mc)) NA_integer_ else as.integer(mc$table[2, 1]),
                         n = if (is.null(mc)) NA_integer_ else as.integer(mc$n),
                         p = p_adjusted[k])
                })

                # The p-values are adjusted for multiplicity and the paired Newcombe
                # intervals in diffTable are not; nothing said so, and the two panels can
                # therefore appear to contradict each other. Cochran's Q is not part of the Holm
                # family, and the note now says so.
                if (holm_applied) {
                    mcnemarTable$setNote(
                        "mcnemar_multiplicity",
                        .fmt(jmvcore::.("The pairwise p-values in this table are Holm-Bonferroni adjusted across the {comparisons} pairwise comparisons that were tested (those with discordant pairs). Cochran's Q is a single global test, and its p-value is not adjusted. The confidence intervals and the sensitivity and specificity p-values in the Differences with 95% Confidence Intervals table are not adjusted for multiplicity, so an adjusted p above 0.05 can sit beside an interval that excludes zero."),
                            comparisons = n_effective)
                    )
                    diffTable$setNote(
                        "diff_multiplicity",
                        jmvcore::.("Each interval, and each sensitivity or specificity p-value, is for a single comparison and is not adjusted for multiplicity, whereas the pairwise p-values in the McNemar table are Holm-Bonferroni adjusted.")
                    )
                } else if (n_effective == 1) {
                    mcnemarTable$setNote(
                        "mcnemar_multiplicity",
                        jmvcore::.("Only one pairwise comparison was tested (had discordant pairs), so no multiplicity adjustment applies and the p-value shown is the unadjusted one.")
                    )
                    diffTable$setNote(
                        "diff_multiplicity",
                        jmvcore::.("Only one pairwise comparison was tested by McNemar's test, so neither this interval nor the McNemar p-value is adjusted for multiplicity.")
                    )
                }

                if (holm_applied && any(computable & !informative)) {
                    mcnemarTable$setNote(
                        "mcnemar_uninformative_excluded",
                        jmvcore::.("Comparisons without discordant pairs are left out of the Holm-Bonferroni adjustment: their p = 1 is a convention, not a test result.")
                    )
                }

                # Mid-p has no test statistic; say why its Chi-squared and df cells are empty.
                if (n_effective > 0 && !identical(self$options$mcnemarMethod, "asymptotic")) {
                    mcnemarTable$setNote(
                        "mcnemar_midp_nostat",
                        jmvcore::.("The McNemar mid-p test is computed directly from the binomial distribution of the discordant pairs, so it has no chi-squared statistic or degrees of freedom; those cells are empty in its rows.")
                    )
                }

                # Record whether any comparison reached significance (for honest report
                # wording in the always-visible clinical summary panel). Three states:
                # TRUE (a test ran and reached significance), FALSE (a test ran and did
                # not), NA (NO comparison was computable at all). NA must not collapse to
                # FALSE: any(NA < 0.05, na.rm = TRUE) is FALSE, and the report panels then
                # printed "did not reveal a statistically significant difference" about a
                # test that was never performed.
                # Only tests that ran count: a pair without discordant pairs has p = 1 by
                # convention and must not turn "no test result" into "not significant".
                sig_pvals <- p_adjusted[informative]
                if (!is.null(private$.cochran_pvalue)) {
                    sig_pvals <- c(sig_pvals, private$.cochran_pvalue)
                }
                private$.any_significant_comparison <- if (length(sig_pvals) == 0 || !any(!is.na(sig_pvals))) {
                    NA
                } else {
                    any(sig_pvals < private$P_THRESHOLD_SIGNIFICANT, na.rm = TRUE)
                }

                # Second pass: populate table with adjusted p-values, reusing cached tests
                for (i in seq_along(test_pairs)) {
                    # .calculateDifferences() runs three paired Newcombe intervals per pair.
                    private$.checkpoint()

                    pair <- test_pairs[[i]]
                    test1 <- pair[1]
                    test2 <- pair[2]
                    comparison_name <- comparison_names[i]

                    # McNemar's test with adjusted p-value (cached from first pass)
                    private$.performMcNemarTest(
                        test_results, test1, test2, comparison_name,
                        mcnemarTable, p_adjusted[i], cached = mcnemar_cache[[i]],
                        holm_applied = holm_applied, comparison_label = comparison_labels[i]
                    )

                    # Confidence intervals for differences
                    private$.calculateDifferences(test_results, test1, test2, comparison_name, diffTable,
                                                  comparison_label = comparison_labels[i], pair_index = i)
                }

            },

            # Exact conditional tail for three tests. Each informative row has three
            # equiprobable permutations conditional on its sum (one or two correct).
            # Convolve column-1/column-2 totals; column 3 follows from the fixed total.
            # The caller bounds m at 30: O(m^3) work and O(m^2) storage, no RNG.
            .cochranExactP = function(correctness) {
                totals <- rowSums(correctness)
                informative <- correctness[totals > 0 & totals < 3, , drop = FALSE]
                m <- nrow(informative)
                if (m == 0) return(NA_real_)
                stopifnot(ncol(informative) == 3, m <= 30)
                row_totals <- rowSums(informative)
                prob <- matrix(1, 1, 1)
                for (r in row_totals) {
                    size <- nrow(prob)
                    next_prob <- matrix(0, size + 1L, size + 1L)
                    ix <- seq_len(size)
                    next_prob[ix + 1L, ix] <- next_prob[ix + 1L, ix] + prob / 3
                    next_prob[ix, ix + 1L] <- next_prob[ix, ix + 1L] + prob / 3
                    shift <- if (r == 1) 0L else 1L
                    next_prob[ix + shift, ix + shift] <-
                        next_prob[ix + shift, ix + shift] + prob / 3
                    prob <- next_prob
                }
                # With fixed row sums, Q is monotone in the sum of squared column totals.
                # Using this integer-valued score avoids floating-point tail-tie errors.
                observed <- sum(colSums(informative)^2)
                score <- outer(0:m, 0:m, function(a, b) {
                    a^2 + b^2 + (sum(row_totals) - a - b)^2
                })
                min(1, sum(prob[score >= observed]))
            },

            # Perform Cochran's Q test for 3+ tests (global test)
            # CRITICAL: Compare CORRECTNESS relative to gold standard, not raw positivity rates
            .performCochranQ = function(test_results, mcnemarTable) {
                # Builds an n x k correctness matrix over the whole cohort before the
                # statistic; checkpoint before that work rather than after it.
                private$.checkpoint()

                tryCatch(
                    {
                        # Extract test results and gold standard
                        test_names <- names(test_results)
                        n_tests <- length(test_names)

                        # Get common valid cases
                        gold <- test_results[[test_names[1]]]$gold_reference

                        # Build matrix of CORRECTNESS (not raw positivity)
                        all_tests_matrix <- sapply(test_names, function(tn) {
                            test_result <- test_results[[tn]]$test_results
                            # 1 if correct (matches gold), 0 if wrong
                            ifelse(test_result == gold, 1, 0)
                        })

                        valid_idx <- complete.cases(all_tests_matrix) & !is.na(gold)
                        if (sum(valid_idx) == 0) {
                            # Returning silently left the Summary and Manuscript-Ready
                            # panels containing nothing but their headings, which reads
                            # as a broken module rather than uninformative data.
                            private$.addNotice(
                                type = "INFO",
                                title = jmvcore::.("Global Test Not Computed"),
                                content = jmvcore::.("Cochran's Q was not computed: no case has a determinate result for all three tests and the reference standard. The pairwise comparisons below each use their own complete cases.")
                            )
                            return()
                        }

                        all_tests_matrix <- all_tests_matrix[valid_idx, , drop = FALSE]

                        # Cochran's Q statistic
                        # Q = (k-1) * [k*sum(Ci^2) - (sum(Ci))^2] / [k*sum(Ri) - sum(Ri^2)]
                        k <- n_tests
                        n <- nrow(all_tests_matrix)

                        Ci <- colSums(all_tests_matrix) # Column sums (correct per test)
                        Ri <- rowSums(all_tests_matrix) # Row sums (correct per subject)

                        numerator <- k * sum(Ci^2) - (sum(Ci))^2
                        denominator <- k * sum(Ri) - sum(Ri^2)

                        if (denominator == 0) {
                            # Every subject was classified the same way by all three
                            # tests, so the statistic has no within-subject variation to
                            # work with. Uninformative data, not a failure -- say so.
                            private$.addNotice(
                                type = "INFO",
                                title = jmvcore::.("Global Test Not Computed"),
                                content = jmvcore::.("Cochran's Q is undefined for these data: every case was classified identically by all three tests (all correct, or all incorrect), leaving no discordance for the statistic to use. This means the data cannot separate the tests, not that the analysis failed.")
                            )
                            return()
                        }

                        Q <- (k - 1) * numerator / denominator
                        df <- k - 1
                        n_informative <- sum(Ri > 0 & Ri < k)
                        exact <- k == 3 && n_informative <= 30
                        if (exact) {
                            p_value <- private$.cochranExactP(all_tests_matrix)
                            df <- NA_integer_
                            private$.cochran_method_note <- .fmt(jmvcore::.("Cochran's Q used an exact conditional p-value based on {n} informative cases, conditioning on each case's number of correct tests and assuming exchangeability of test labels under the null. The statistic column reports Q; no chi-square degrees of freedom apply."), n = n_informative)
                        } else {
                            p_value <- stats::pchisq(Q, df, lower.tail = FALSE)
                            private$.cochran_method_note <- .fmt(jmvcore::.("Cochran's Q used the chi-square approximation with {n} informative cases. This is a large-sample approximation; the total complete-case count includes concordant cases that contribute no information to this test."), n = n_informative)
                        }
                        mcnemarTable$setNote("cochran_method", private$.cochran_method_note)
                        # Store the global p-value for honest report wording
                        private$.cochran_pvalue <- p_value

                        interpretation <- dplyr::case_when(
                            p_value < 0.001 ~ jmvcore::.("Highly significant overall difference among tests (p<0.001)"),
                            p_value < 0.01 ~ jmvcore::.("Significant overall difference among tests (p<0.01)"),
                            p_value < 0.05 ~ jmvcore::.("Statistically significant overall difference among tests (p<0.05)"),
                            TRUE ~ jmvcore::.("No significant overall difference detected among tests (p>=0.05); this does not establish that the tests perform equally")
                        )

                        mcnemarTable$addRow(
                            rowKey = "cochran_q_global",
                            values = list(
                                comparison = sprintf(jmvcore::.("Overall (%d tests)"), n_tests),
                                n = n,
                                method = jmvcore::.("Cochran's Q"),
                                stat = Q,
                                df = df,
                                p = p_value,
                                interpretation = interpretation
                            )
                        )

                        if (p_value >= private$P_THRESHOLD_SIGNIFICANT) {
                            mcnemarTable$addFootnote(
                                rowKey = "cochran_q_global",
                                col = "interpretation",
                                jmvcore::.("Cochran's Q is a single test of whether the tests differ in accuracy anywhere among them. It did not reach p < 0.05 here, which means these data did not separate the tests, not that the tests agree. The per-pair rows in this table are exploratory in that situation: read them alongside the paired differences and their 95% confidence intervals in the Differences with 95% Confidence Intervals table, which show how large a real difference is still compatible with this sample.")
                            )
                        }
                    },
                    error = function(e) {
                        error_msg <- conditionMessage(e)
                        # Add visible error row to table
                        mcnemarTable$addRow(
                            rowKey = "cochran_q_error",
                            values = list(
                                comparison = sprintf(jmvcore::.("Overall (%d tests) - ERROR"), n_tests),
                                n = NA,
                                method = jmvcore::.("Cochran's Q"),
                                stat = NA,
                                df = NA,
                                p = NA,
                                # Leading separator kept OUTSIDE the msgid: padding inside a
                                # msgid is routinely lost in translation.
                                interpretation = paste0(" ", sprintf(
                                    jmvcore::.("Could not calculate: %s. Check that all tests have valid paired data."),
                                    error_msg
                                ))
                            )
                        )

                        # setNote(key, note): the first argument is the KEY, and `symbol`
                        # is not a parameter -- the old call passed the message as the key
                        # with no note and raised from inside this error handler.
                        mcnemarTable$setNote(
                            "cochran_q_failed",
                            jmvcore::.("Cochran's Q test failed. Pairwise comparisons may still be valid but should be interpreted cautiously.")
                        )
                        # The Summary and Report point to the Important Information panel for the reason.
                        private$.addNotice(
                            type = "WARNING",
                            title = jmvcore::.("Global Test Not Computed"),
                            content = jmvcore::.("Cochran's Q test failed. Pairwise comparisons may still be valid but should be interpreted cautiously.")
                        )
                    }
                )
            },

            # McNemar test on the discordant counts b (only test 1 correct) and c (only test 2
            # correct), in the two versions Fagerland, Lydersen & Laake recommend at every sample
            # size (BMC Med Res Methodol 2013;13:91; Statistical Analysis of Contingency Tables,
            # 2017, Table 8.15). The exact conditional test and the continuity-corrected test are
            # not used: they "did not perform well for any of the considered scenarios" (2013).
            .mcnemarTest = function(b, c, method = "midp") {
                m <- b + c
                if (identical(method, "asymptotic")) {
                    label <- jmvcore::.("McNemar asymptotic (no continuity correction)")
                    # Undefined without discordant pairs; p = 1 as in Fagerland et al.'s evaluation.
                    if (m == 0)
                        return(list(statistic = NA_real_, parameter = NA_integer_, p.value = 1, method = label))
                    stat <- (b - c)^2 / m
                    return(list(statistic = stat, parameter = 1L,
                                p.value = stats::pchisq(stat, df = 1, lower.tail = FALSE), method = label))
                }
                # Mid-p, eq. (5): the two-sided exact conditional p minus the point probability of the
                # observed count; at a tie, eq. (6): 1 - f/2. Without discordant pairs eq. (6) would
                # give 0.5, so p = 1 there (no information), as for the asymptotic test.
                p <- if (m == 0) {
                    1
                } else if (b == c) {
                    1 - 0.5 * stats::dbinom(b, m, 0.5)
                } else {
                    k <- min(b, c)
                    min(1, 2 * stats::pbinom(k, m, 0.5)) - stats::dbinom(k, m, 0.5)
                }
                list(statistic = NA_real_, parameter = NA_integer_, p.value = p,
                     method = jmvcore::.("McNemar mid-p"))
            },

            # The smallest p the chosen McNemar test can give with m discordant pairs: the split
            # with all of them on one side, 0.5^m for mid-p (eq. 5 at n12 = 0) and
            # P(chi-squared(1) > m) for the asymptotic test.
            .mcnemarMinP = function(m) {
                if (identical(self$options$mcnemarMethod, "asymptotic"))
                    stats::pchisq(m, df = 1, lower.tail = FALSE)
                else
                    0.5^m
            },

            # Compute the paired McNemar test of diagnostic CORRECTNESS (agreement with the
            # gold standard, NOT raw positivity). Returns list(result, table) or NULL when it
            # cannot be computed. Shared by the Holm p-value pass and the display pass so the
            # test is only computed once per pair.
            .computeMcNemar = function(test_results, test1, test2) {
                test1_results <- test_results[[test1]]$test_results
                test2_results <- test_results[[test2]]$test_results
                gold_results <- test_results[[test1]]$gold_reference # Same for both tests

                tryCatch(
                    {
                        valid_idx <- !is.na(test1_results) & !is.na(test2_results) & !is.na(gold_results)
                        if (sum(valid_idx) == 0) {
                            return(NULL)
                        }

                        # Compare CORRECTNESS: does each test match the gold standard?
                        test1_correct <- (test1_results[valid_idx] == gold_results[valid_idx])
                        test2_correct <- (test2_results[valid_idx] == gold_results[valid_idx])

                        # McNemar table of correctness (not raw test results)
                        mcnemar_table <- table(
                            factor(test1_correct, levels = c(TRUE, FALSE)),
                            factor(test2_correct, levels = c(TRUE, FALSE)),
                            useNA = "no"
                        )

                        if (sum(mcnemar_table) == 0) {
                            return(NULL)
                        }

                        n_b <- unname(mcnemar_table[1, 2])
                        n_c <- unname(mcnemar_table[2, 1])
                        n_discordant <- n_b + n_c
                        mcnemar_result <- private$.mcnemarTest(n_b, n_c, self$options$mcnemarMethod)
                        list(
                            result = mcnemar_result,
                            table = mcnemar_table,
                            n = sum(valid_idx),
                            n_discordant = n_discordant
                        )
                    },
                    error = function(e) {
                        NULL
                    }
                )
            },

            # Perform McNemar's test with clinical interpretation
            # CRITICAL: Compare CORRECTNESS relative to gold standard, not raw positivity rates
            .performMcNemarTest = function(test_results, test1, test2, comparison_name, mcnemarTable, p_adjusted = NULL, cached = NULL, holm_applied = FALSE,
                                           comparison_label = comparison_name) {
                test1_results <- test_results[[test1]]$test_results
                test2_results <- test_results[[test2]]$test_results
                gold_results <- test_results[[test1]]$gold_reference # Same for both tests

                tryCatch(
                    {
                        if (!is.null(cached)) {
                            # Reuse the McNemar test computed in the Holm p-value pass
                            mcnemar_table <- cached$table
                            mcnemar_result <- cached$result
                        } else {
                            cached <- private$.computeMcNemar(test_results, test1, test2)
                            if (is.null(cached)) return()
                            mcnemar_table <- cached$table
                            mcnemar_result <- cached$result
                        }

                        # Check sample size adequacy (number of discordant pairs)
                        # n_b = Test1 correct, Test2 wrong; n_c = Test1 wrong, Test2 correct
                        n_b <- mcnemar_table[1, 2] # TRUE, FALSE
                        n_c <- mcnemar_table[2, 1] # FALSE, TRUE
                        n_discordant <- n_b + n_c

                        # Use adjusted p-value if provided, otherwise use raw p-value
                        p_to_interpret <- if (!is.null(p_adjusted) && !is.na(p_adjusted)) p_adjusted else mcnemar_result$p.value

                        # Clinical interpretation of p-value (using constants). Without discordant
                        # pairs p = 1 is a convention, so there is no verdict to state.
                        interpretation <- if (n_discordant == 0) {
                            jmvcore::.("Not informative: no discordant pairs (p = 1 by convention)")
                        } else dplyr::case_when(
                            p_to_interpret < private$P_THRESHOLD_STRONG ~ jmvcore::.("Highly significant difference (p<0.001)"),
                            p_to_interpret < 0.01 ~ jmvcore::.("Significant difference (p<0.01)"),
                            p_to_interpret < private$P_THRESHOLD_SIGNIFICANT ~ jmvcore::.("Statistically significant difference (p<0.05)"),
                            TRUE ~ jmvcore::.("No significant difference detected at alpha = 0.05; this does not establish that the tests perform equally")
                        )

                        # Add suffix to interpretation if adjusted. The separating space is
                        # outside the msgid so translation cannot drop or duplicate it.
                        # Claim the correction only when one was actually applied.
                        if (isTRUE(holm_applied) && n_discordant > 0) {
                            interpretation <- paste0(interpretation, " ", jmvcore::.("(Holm-Bonferroni corrected)"))
                        }

                        mcnemarTable$addRow(
                            rowKey = comparison_name,
                            values = list(
                                comparison = comparison_label,
                                n = cached$n,
                                method = mcnemar_result$method,
                                # NULL, not NA, so jamovi shows an empty cell: mid-p has no statistic.
                                stat = if (is.na(mcnemar_result$statistic)) NULL else mcnemar_result$statistic,
                                df = if (is.na(mcnemar_result$parameter)) NULL else mcnemar_result$parameter,
                                p = p_to_interpret,
                                interpretation = interpretation
                            )
                        )

                        # Disclose how little information few discordant pairs carry. This is not a
                        # validity cut-off: both recommended tests hold at any count.
                        if (n_discordant == 0) {
                            mcnemarTable$addFootnote(
                                rowKey = comparison_name,
                                col = "p",
                                jmvcore::.("No discordant pairs: both tests were correct on exactly the same cases, so McNemar's test has no information here; p is set to 1 and no test statistic is computed.")
                            )
                        } else if (private$.mcnemarMinP(n_discordant) >= private$P_THRESHOLD_SIGNIFICANT) {
                            # Even the most lopsided split (all b or all c) stays above 0.05, so a
                            # non-significant p is guaranteed: mid-p 0.5^m (m <= 4), asymptotic
                            # pchisq(m) (m <= 3). .() has no plural forms: the singular is its own msgid.
                            mcnemarTable$addFootnote(
                                rowKey = comparison_name,
                                col = "p",
                                if (n_discordant == 1)
                                    jmvcore::.("Only 1 discordant pair: with one, McNemar's test cannot reach p < 0.05, so its non-significant result says nothing about whether the tests differ; read the accuracy difference and its paired confidence interval.")
                                else
                                    sprintf(
                                        jmvcore::.("Only %d discordant pairs: with so few, McNemar's test cannot reach p < 0.05 whatever their split, so its non-significant result says nothing about whether the tests differ; read the accuracy difference and its paired confidence interval."),
                                        n_discordant
                                    )
                            )
                        } else if (n_discordant < private$MIN_DISCORDANT_PAIRS) {
                            mcnemarTable$addFootnote(
                                rowKey = comparison_name,
                                col = "p",
                                sprintf(
                                    jmvcore::.("Only %d discordant pairs, so McNemar's test has little power here; read the accuracy difference and its paired confidence interval."),
                                    n_discordant
                                )
                            )
                        }
                    },
                    error = function(e) {
                        n1 <- length(test1_results)
                        n2 <- length(test2_results)
                        comparison <- comparison_label
                        error <- conditionMessage(e)
                        enhanced_msg <- .fmt(jmvcore::.("Could not perform McNemar's test for {comparison} (n1={n1}, n2={n2}). This may be due to insufficient discordant pairs or identical test results. Error: {error}"),
                            comparison = comparison, n1 = n1, n2 = n2, error = error
                        )
                        private$.addNotice(
                            type = "WARNING",
                            title = jmvcore::.("McNemar Test Unavailable"),
                            content = enhanced_msg
                        )
                    }
                )
            },

            # Calculate confidence intervals for paired metric differences
            .calculateDifferences = function(test_results, test1, test2, comparison_name, diffTable,
                                             comparison_label = comparison_name, pair_index = NULL) {
                # The paired score method is distinct from the single-proportion CI
                # method used for OPA; name it explicitly in the output.
                diffTable$setNote(
                    "ci_method",
                    jmvcore::.("Differences are paired (within-subject). Confidence intervals use Newcombe's method 10, combining Wilson score intervals with the paired correlation and its recommended continuity correction. They are not affected by the \"CI Method for Agreement\" option, which applies only to overall percent agreement.")
                )
                # The row label "A vs B" does not say which way the difference runs
                # (VAL-decisioncompare-11).
                diffTable$setNote(
                    "direction",
                    jmvcore::.("Each difference is the first-named test minus the second: for 'A vs B', a positive value favours A and a negative value favours B.")
                )

                comp <- private$.extractComparisonVectors(test_results, test1, test2)
                t1 <- comp$test1
                t2 <- comp$test2
                gold <- comp$gold

                common_idx <- !is.na(t1) & !is.na(t2) & !is.na(gold)

                if (sum(common_idx) == 0) {
                    private$.addNotice(
                        type = "WARNING",
                        title = jmvcore::.("Paired Difference Unavailable"),
                        content = .fmt(jmvcore::.("Unable to compute paired differences for {comparison}: no observations with complete data across both tests and the gold standard."),
                            comparison = comparison_label
                        )
                    )
                    return()
                }

                # IR-decisioncompare-02: sensitivity and specificity are compared by the same McNemar
                # test as accuracy, applied to the discordant pairs within each reference class. Unlike
                # overall accuracy, what they compare does not depend on prevalence. They stay
                # unadjusted, like the Newcombe intervals beside them, and the note says so; overall
                # accuracy keeps its own (Holm-adjusted) test in the McNemar table, so its p cell here
                # is left empty rather than repeating an unadjusted value beside an adjusted one.
                mcnemar_method <- self$options$mcnemarMethod
                diffTable$setNote(
                    "class_p",
                    if (identical(mcnemar_method, "asymptotic"))
                        jmvcore::.("The p-values for sensitivity and specificity come from the asymptotic McNemar test without continuity correction on the discordant pairs among reference-positive and reference-negative cases respectively. Without discordant pairs p is 1 by convention. They are not adjusted for multiplicity. Overall accuracy is tested in the Statistical Comparison of Test Accuracy table, so its p cell here is empty.")
                    else
                        jmvcore::.("The p-values for sensitivity and specificity come from the McNemar mid-p test on the discordant pairs among reference-positive and reference-negative cases respectively. Without discordant pairs p is 1 by convention. They are not adjusted for multiplicity. Overall accuracy is tested in the Statistical Comparison of Test Accuracy table, so its p cell here is empty.")
                )
                class_p <- function(result) {
                    if (is.null(result)) return(NULL)
                    private$.mcnemarTest(unname(result$counts[["x_only"]]), unname(result$counts[["y_only"]]),
                                         mcnemar_method)$p.value
                }

                add_diff_row <- function(result, metric_label, metric_key, p = NULL) {
                    if (is.null(result)) {
                        private$.addNotice(
                            type = "WARNING",
                            title = jmvcore::.("Metric Difference Unavailable"),
                            content = .fmt(jmvcore::.("Unable to compute {metric} difference for {comparison}: insufficient paired data."),
                                metric = metric_label, comparison = comparison_label
                            )
                        )
                        return()
                    }
                    row_key <- paste(comparison_name, metric_key, sep = "_")
                    diffTable$addRow(
                        rowKey = row_key,
                        values = list(
                            comparison = comparison_label,
                            metric = metric_label,
                            n = result$n,
                            diff = result$diff,
                            lower = result$lower,
                            upper = result$upper,
                            p = p
                        )
                    )
                    # Small-sample note. It reports the counts and claims nothing about the interval's
                    # width: a cell below 5 can be a concordant cell, and few discordant pairs do not by
                    # themselves make Newcombe's interval wide (t6).
                    if (!is.null(result$n) && (result$n < 30 || any(result$counts < 5))) {
                        diffTable$addFootnote(
                            rowKey = row_key,
                            col = "diff",
                            .fmt(jmvcore::.("Small paired sample or a cell below 5 (n={n}; paired counts both, first only, second only, neither: {counts}). The interval rests on these counts."),
                                n = result$n,
                                counts = paste(result$counts, collapse = ", ")
                            )
                        )
                    }
                }

                # Sensitivity difference (gold positive)
                sens_idx <- common_idx & gold == "Positive"
                sens_diff <- if (sum(sens_idx) == 0) {
                    NULL
                } else {
                    pairedProportionDifference(
                        x = ifelse(t1[sens_idx] == "Positive", 1, 0),
                        y = ifelse(t2[sens_idx] == "Positive", 1, 0)
                    )
                }
                sens_p <- class_p(sens_diff)
                add_diff_row(sens_diff, jmvcore::.("Sensitivity"), "Sens", p = sens_p)

                # Specificity difference (gold negative)
                spec_idx <- common_idx & gold == "Negative"
                spec_diff <- if (sum(spec_idx) == 0) {
                    NULL
                } else {
                    pairedProportionDifference(
                        x = ifelse(t1[spec_idx] == "Negative", 1, 0),
                        y = ifelse(t2[spec_idx] == "Negative", 1, 0)
                    )
                }
                spec_p <- class_p(spec_diff)
                add_diff_row(spec_diff, jmvcore::.("Specificity"), "Spec", p = spec_p)

                # Accuracy difference (all subjects)
                acc_diff <- pairedProportionDifference(
                    x = ifelse(t1[common_idx] == gold[common_idx], 1, 0),
                    y = ifelse(t2[common_idx] == gold[common_idx], 1, 0)
                )
                add_diff_row(acc_diff, jmvcore::.("Accuracy"), "AccurT")

                # For the copy-ready Report (IR-decisioncompare-02).
                if (!is.null(pair_index))
                    private$.diff_results[[pair_index]] <- list(
                        Sens = sens_diff, Spec = spec_diff, AccurT = acc_diff,
                        p_sens = sens_p, p_spec = spec_p)
            },

            # Setup visualizations with improved data structure
            .setupVisualizations = function(test_results) {
                plotData <- list()

                for (test_name in names(test_results)) {
                    result <- test_results[[test_name]]
                    metrics <- result$metrics

                    plotData[[test_name]] <- list(
                        test = test_name,
                        Sens = metrics$Sens,
                        Spec = metrics$Spec,
                        AccurT = metrics$AccurT, # Include accuracy for radar plot
                        PPV = metrics$PPV,
                        NPV = metrics$NPV,
                        LRP = metrics$LRP,
                        LRN = metrics$LRN
                    )
                }

                if (self$options$plot) {
                    self$results$plot1$setState(plotData)
                }

                if (self$options$radarplot) {
                    self$results$plotRadar$setState(plotData)
                }

                if (self$options$heatmap) {
                    heatmapState <- private$.buildHeatmapData()
                    if (!is.null(heatmapState)) {
                        self$results$plotHeatmap$setState(heatmapState)
                    }
                }
            },

            # Optimized bar plot data building - eliminates inefficient rbind operations
            .buildBarPlotData = function(plotData) {
                if (is.null(plotData) || length(plotData) == 0) {
                    return(data.frame())
                }

                # Pre-calculate total rows needed
                n_tests <- length(plotData)
                n_metrics <- 4 # Sens, Spec, PPV, NPV
                total_rows <- n_tests * n_metrics

                # Pre-allocate vectors for efficiency
                test_names <- character(total_rows)
                metric_names <- character(total_rows)
                values <- numeric(total_rows)

                # Fill vectors efficiently
                row_idx <- 1
                metrics <- c(jmvcore::.("Sensitivity"), jmvcore::.("Specificity"),
                             jmvcore::.("PPV"), jmvcore::.("NPV"))
                metric_keys <- c("Sens", "Spec", "PPV", "NPV")

                for (test_name in names(plotData)) {
                    test_data <- plotData[[test_name]]
                    for (i in seq_along(metrics)) {
                        test_names[row_idx] <- test_name
                        metric_names[row_idx] <- metrics[i]
                        values[row_idx] <- test_data[[metric_keys[i]]]
                        row_idx <- row_idx + 1
                    }
                }

                return(data.frame(
                    test = test_names,
                    metric = metric_names,
                    value = values,
                    stringsAsFactors = FALSE
                ))
            },

            # Radar axis labels, translated once and shared by the data builder and the
            # renderer. The sibling bar plot already translated the same metric names, so
            # a Turkish session showed translated labels on one plot and bare English
            # ones on the plot beside it. The renderer re-levels the factor, so both must
            # use the SAME strings or every point would be dropped.
            .radarMetricLabels = function() {
                c(jmvcore::.("Sensitivity"), jmvcore::.("Specificity"), jmvcore::.("Accuracy"),
                  jmvcore::.("PPV"), jmvcore::.("NPV"),
                  jmvcore::.("LR+ Quality"), jmvcore::.("LR- Quality"))
            },

            # Optimized radar plot data building with clinical scaling
            .buildRadarPlotData = function(plotData) {
                if (is.null(plotData) || length(plotData) == 0) {
                    return(data.frame())
                }

                # Pre-calculate dimensions
                n_tests <- length(plotData)
                n_metrics <- 7 # 5 percentage metrics + 2 LR quality metrics
                total_rows <- n_tests * n_metrics

                # Pre-allocate vectors
                test_names <- character(total_rows)
                metric_names <- character(total_rows)
                values <- numeric(total_rows)
                scaled_values <- numeric(total_rows)

                # Metric keys stay English; the displayed labels are translated.
                metric_labels <- private$.radarMetricLabels()
                pct_keys <- c("Sens", "Spec", "AccurT", "PPV", "NPV")

                row_idx <- 1
                thresholds <- private$LR_CLINICAL_THRESHOLDS

                for (test_name in names(plotData)) {
                    test_data <- plotData[[test_name]]

                    # Process percentage metrics (direct 0-100 scaling)
                    for (mi in seq_along(pct_keys)) {
                        metric_key <- pct_keys[mi]
                        test_names[row_idx] <- test_name
                        metric_names[row_idx] <- metric_labels[mi]
                        values[row_idx] <- test_data[[metric_key]]
                        scaled_values[row_idx] <- test_data[[metric_key]] * 100
                        row_idx <- row_idx + 1
                    }

                    # Process LR+ with clinical thresholds
                    lrp_value <- test_data$LRP
                    lrp_scaled <- private$.scaleLikelihoodRatioPositive(lrp_value, thresholds)

                    test_names[row_idx] <- test_name
                    metric_names[row_idx] <- metric_labels[6]
                    values[row_idx] <- lrp_value
                    scaled_values[row_idx] <- lrp_scaled
                    row_idx <- row_idx + 1

                    # Process LR- with clinical thresholds
                    lrn_value <- test_data$LRN
                    lrn_scaled <- private$.scaleLikelihoodRatioNegative(lrn_value, thresholds)

                    test_names[row_idx] <- test_name
                    metric_names[row_idx] <- metric_labels[7]
                    values[row_idx] <- lrn_value
                    scaled_values[row_idx] <- lrn_scaled
                    row_idx <- row_idx + 1
                }

                df <- data.frame(
                    test = test_names,
                    metric = factor(metric_names, levels = metric_labels),
                    value = values,
                    scaled_value = scaled_values,
                    stringsAsFactors = FALSE
                )

                return(df)
            },

            # Illustrative 0-100 score for a likelihood ratio, continuous and symmetric. LR+ <= 1
            # carries no rule-in evidence and scores 0; the band edges 2, 5 and 10 score 50, 75
            # and 100, linear in between. LR- is scored as 1/LR- on the same scale, so an LR+ of r
            # and an LR- of 1/r score alike, as Jaeschke's bands treat them. The old scale started
            # LR+ just above 1 at 25 but LR- just below 1 at 0, so an uninformative test looked
            # better at ruling in than out (VAL-decisioncompare-10). A missing ratio stays
            # missing, and the radar drops that point rather than drawing it at 0.
            .scaleLikelihoodRatioPositive = function(lrp, thresholds) {
                if (is.na(lrp)) return(NA_real_)
                if (lrp <= 1) return(0)
                if (lrp >= thresholds$excellent_pos) return(100)
                if (lrp >= thresholds$good_pos)
                    return(75 + (lrp - thresholds$good_pos) / (thresholds$excellent_pos - thresholds$good_pos) * 25)
                if (lrp >= thresholds$fair_pos)
                    return(50 + (lrp - thresholds$fair_pos) / (thresholds$good_pos - thresholds$fair_pos) * 25)
                (lrp - 1) / (thresholds$fair_pos - 1) * 50
            },

            .scaleLikelihoodRatioNegative = function(lrn, thresholds) {
                if (is.na(lrn)) return(NA_real_)
                if (lrn <= 0) return(100)
                private$.scaleLikelihoodRatioPositive(1 / lrn, thresholds)
            },

            # Generate comprehensive clinical report with copy-ready sentences
            .generateClinicalReport = function(test_results, processed_data) {
                if (length(test_results) == 0) {
                    return()
                }

                # Select the highest observed balanced accuracy for a descriptive
                # summary only; this is not a superiority or clinical recommendation.
                best_test <- private$.findBestTest(test_results)
                best_metrics <- test_results[[best_test]]$metrics

                # Generate report sections
                methods_section <- private$.generateMethodsSection(test_results, processed_data)
                results_section <- private$.generateResultsSection(test_results, best_test, best_metrics)
                clinical_recommendations <- private$.generateClinicalRecommendations(test_results, best_test)

                # Create comprehensive HTML report
                report_html <- paste0(
                    '<div style="font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px;">',
                    '<h2 style="color: inherit; border-bottom: 2px solid #3498db;"> ',
                    jmvcore::.("Descriptive Performance Summary"), "</h2>",
                    results_section,
                    '<h3 style="color: inherit; margin-top: 30px;"> ',
                    jmvcore::.("Report Sentences"), "</h3>",
                    '<div style="background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #28a745; margin: 15px 0; color: inherit;">',
                    '<h4 style="margin-top: 0;">', jmvcore::.("Methods Section:"), "</h4>",
                    '<p style="font-style: italic; line-height: 1.6;">', methods_section, "</p>",
                    "</div>",
                    '<div style="background-color: rgba(33, 149, 188, 0.1); padding: 15px; border-left: 4px solid #3498db; margin: 15px 0; color: inherit;">',
                    '<h4 style="margin-top: 0;">', jmvcore::.("Results Section:"), "</h4>",
                    '<p style="font-style: italic; line-height: 1.6;">', results_section, "</p>",
                    "</div>",
                    '<h3 style="color: inherit; margin-top: 30px;"> ',
                    jmvcore::.("Performance Summary"), "</h3>",
                    clinical_recommendations,
                    "</div>"
                )

                self$results$clinicalReport$setContent(report_html)
            },

            # Find the highest observed balanced accuracy for descriptive reporting.
            # IR2-decisioncompare-02: the ranking compares the tests on the SAME cases. With
            # partial testing (an assay not done on the hardest cases) each test's own rows
            # differ, and ranking those standalone figures named as best a test that was
            # significantly less sensitive on the common cases. When the case sets differ the
            # ranking uses the cases determinate for every test; the standalone figures in
            # the tables and report are unchanged, and .rankingBasisSentence() says so.
            .findBestTest = function(test_results) {
                common <- Reduce(`&`, lapply(test_results, function(x) x$determinate))
                same_cases <- all(vapply(test_results,
                    function(x) identical(x$determinate, common), logical(1)))
                private$.ranking_common_n <- if (same_cases) NULL else sum(common)
                gold_common <- test_results[[1]]$gold_reference[common]
                scores <- vapply(names(test_results), function(test_name) {
                    if (same_cases) {
                        m <- test_results[[test_name]]$metrics
                        score <- (m$Sens + m$Spec) / 2
                    } else {
                        call <- test_results[[test_name]]$test_results[common]
                        score <- (mean(call[gold_common == "Positive"] == "Positive") +
                                  mean(call[gold_common == "Negative"] == "Negative")) / 2
                    }
                    if (length(score) != 1 || !is.finite(score)) NA_real_ else score
                }, numeric(1))

                if (all(is.na(scores))) {
                    # No balanced accuracy is estimable (one reference-standard class has no
                    # cases), so nothing is ranked; callers word their text on this flag
                    # (VAL-decisioncompare-08). The first test is returned only so that the
                    # per-test figures that follow have a subject.
                    private$.best_test_ranked <- FALSE
                    return(names(test_results)[1])
                }
                private$.best_test_ranked <- TRUE

                best_score <- max(scores, na.rm = TRUE)
                # Ties were previously broken by whichever test came first, silently --
                # so a coin flip decided which test the report named as best.
                tied <- names(scores)[!is.na(scores) & abs(scores - best_score) < 1e-9]
                private$.best_test_tied <- tied

                if (length(tied) > 1) {
                    private$.addNotice(
                        type = "WARNING",
                        title = jmvcore::.("Tied Descriptive Ranking"),
                        content = .fmt(jmvcore::.("{tests} had identical observed balanced accuracy. {first} appears first only because it was selected first; this descriptive tie is not a clinical recommendation."),
                            tests = paste(tied, collapse = ", "), first = tied[1]
                        )
                    )
                }
                return(tied[1])
            },

            # Generate methods section for manuscripts
            .generateMethodsSection = function(test_results, processed_data) {
                n_tests <- length(test_results)
                # CRITICAL FIX: processed_data is a list, need to access $data element
                n_cases <- nrow(processed_data$data)
                test_names <- names(test_results)
                analyzed_n <- vapply(test_results, function(x) x$n_analyzed, integer(1))
                safe_analyzed_names <- private$.safeHtmlOutput(names(analyzed_n))
                denominator_text <- if (length(unique(analyzed_n)) == 1) {
                    sprintf(jmvcore::.("Each test had %d determinate results."), analyzed_n[1])
                } else {
                    .fmt(jmvcore::.("Determinate denominators were {denominators}."),
                        denominators = paste0(safe_analyzed_names, " n=", analyzed_n, collapse = "; "))
                }

                # Each %s slot receives a COMPLETE sentence (or a list of names), never an
                # English grammar fragment, so a translator can reorder the frame freely.
                methods <- sprintf(
                    jmvcore::.("We compared the diagnostic performance of %1$s tests (%2$s) against the reference standard using diagnostic accuracy analysis. The dataset contained %3$d selected rows. %4$s Standalone estimates used available reference/test pairs, and paired comparisons used common determinate rows. Performance metrics included sensitivity, specificity, positive and negative predictive values, likelihood ratios, and overall accuracy. %5$s"),
                    n_tests,
                    paste(private$.safeHtmlOutput(test_names), collapse = ", "),
                    n_cases,
                    denominator_text,
                    # A methods sentence must not describe a test that never ran: when no
                    # pair was computable (.any_significant_comparison is NA) say so instead.
                    if (!(n_tests >= 2 && self$options$statComp))
                        ""
                    else if (identical(private$.any_significant_comparison, NA) && !isTRUE(private$.mcnemar_uninformative))
                        jmvcore::.("No paired statistical comparison between the tests could be computed for these data, so none is reported.")
                    else
                        # Name every procedure behind the displayed numbers: the p-values are
                        # Holm-adjusted whenever more than one pair was tested, and Cochran's Q
                        # is reported for three tests (VAL-decisioncompare-05). One msgid per
                        # complete sentence; the optional ones drop out.
                        paste(c(
                            if (identical(self$options$mcnemarMethod, "asymptotic"))
                                jmvcore::.("Diagnostic correctness was compared with the asymptotic McNemar test without continuity correction, which Fagerland et al. (2013) recommend when small excesses over the nominal significance level are acceptable.")
                            else
                                jmvcore::.("Diagnostic correctness was compared with the McNemar mid-p test, which Fagerland et al. (2013) recommend for use in any situation."),
                            if (!is.null(private$.cochran_pvalue))
                                jmvcore::.("An overall difference among the three tests was tested with Cochran's Q test."),
                            if (isTRUE(private$.holm_applied))
                                jmvcore::.("Pairwise McNemar p-values were adjusted for multiple comparisons with the Holm-Bonferroni method; the confidence intervals for the paired differences were not."),
                            jmvcore::.("Paired differences in sensitivity, specificity and accuracy are reported with 95% confidence intervals by Newcombe's method 10 for paired proportions."),
                            # IR-decisioncompare-02
                            jmvcore::.("Sensitivity and specificity were also compared with the same McNemar test applied separately to the reference-positive and reference-negative cases; those p-values are not adjusted for multiplicity.")
                        ), collapse = " ")
                )

                # IR-decisioncompare-03: the Results paragraph prints exact intervals for sensitivity
                # and specificity whatever the CI option, so the Methods must name the method.
                paste(trimws(methods), private$.cochran_method_note,
                      jmvcore::.("Sensitivity and specificity are given with exact (Clopper-Pearson) 95% confidence intervals."))
            },

            # Generate results section with key findings
            .generateResultsSection = function(test_results, best_test, best_metrics) {
                fmt_pct <- function(x) {
                    if (length(x) != 1 || !is.finite(x)) return(jmvcore::.("not estimable"))
                    # Translatable: Turkish writes the percent sign before the number. Half-up, as the
                    # tables and the Summary round (IR-decisioncompare-04).
                    sprintf(jmvcore::.("%.1f%%"), private$.halfUp1(x * 100))
                }
                fmt_number <- function(x) {
                    if (length(x) != 1 || !is.finite(x)) return(jmvcore::.("not estimable"))
                    sprintf("%.2f", private$.halfUp(x, 2L))
                }
                sens_pct <- fmt_pct(best_metrics$Sens)
                spec_pct <- fmt_pct(best_metrics$Spec)
                acc_pct <- fmt_pct(best_metrics$AccurT)
                ppv_pct <- fmt_pct(best_metrics$PPV)
                npv_pct <- fmt_pct(best_metrics$NPV)

                # Report the ACTUAL statistical result (do not claim significance
                # unconditionally). .any_significant_comparison is set in
                # .performStatisticalComparisons from the McNemar/Cochran p-values.
                # Sentences carry no leading space any more: padding inside a msgid is
                # routinely lost in translation, so the pieces are joined below instead.
                significance_note <- if (self$options$statComp && length(test_results) >= 2) {
                    if (isTRUE(private$.mcnemar_uninformative) && is.null(private$.cochran_pvalue)) {
                        jmvcore::.("Statistical comparison was not informative here: none of the pairs of tests that could be compared had discordant pairs, so McNemar's test produced no test result (detailed results in the comparison tables).")
                    } else if (isTRUE(private$.any_significant_comparison)) {
                        # "Overall accuracy", not "test performance": the per-class sensitivity and
                        # specificity tests in the Differences table can differ from this verdict
                        # (IR-decisioncompare-02).
                        jmvcore::.("Statistical comparison of overall accuracy (McNemar's/Cochran's test) revealed a statistically significant difference (detailed results in the comparison tables).")
                    } else if (isFALSE(private$.any_significant_comparison)) {
                        jmvcore::.("Statistical comparison of overall accuracy (McNemar's/Cochran's test) did not reveal a statistically significant difference (detailed results in the comparison tables).")
                    } else if (identical(private$.any_significant_comparison, NA)) {
                        # NA = no pair was computable, so there is no result to report.
                        # A null result here would assert a test that never ran. The cause
                        # is not asserted: a pair drops out when no case is determinate for
                        # both tests and the reference standard, but also if the test call
                        # itself fails, and the notice panel carries the actual reason.
                        jmvcore::.("No statistical comparison between the tests could be computed for these data, so no McNemar's or Cochran's test result is available; the reason is given in the Important Information panel. These data therefore say nothing about whether the tests differ.")
                    } else {
                        jmvcore::.("Statistical comparisons between tests are reported in the comparison tables above.")
                    }
                } else {
                    ""
                }

                # This text is offered as manuscript-ready. Two things it must not do:
                # claim one test is "optimal" when the comparison could not distinguish
                # the tests, and print "95% CI: [see confidence interval table]" -- a
                # placeholder pointing at a table that does not even exist unless the
                # user enabled the CI option.
                best <- test_results[[best_test]]
                fmt_ci <- function(x, n) {
                    ci <- private$.proportionCI(x, n, method = "exact")
                    if (is.na(ci$lower)) return("")
                    # Separator outside the msgid.
                    paste0(" ", sprintf(jmvcore::.("(95%% CI: %1$.1f-%2$.1f%%)"),
                                        private$.halfUp1(ci$lower * 100), private$.halfUp1(ci$upper * 100)))
                }
                sens_ci <- fmt_ci(best$TP, best$TP + best$FN)
                spec_ci <- fmt_ci(best$TN, best$TN + best$FP)

                # A ranking is only worth reading as a finding once a test actually
                # separates from the rest; otherwise the caveat sentence at the end fires.
                ranked <- isTRUE(private$.best_test_ranked)
                indistinguishable <- ranked && length(test_results) >= 2 &&
                    self$options$statComp && isFALSE(private$.any_significant_comparison) &&
                    !isTRUE(private$.mcnemar_uninformative)

                # With one reference-standard class empty no balanced accuracy exists, so the
                # sentence must not name a "highest" one (VAL-decisioncompare-08). A tie names every
                # tied test, not only the one selected first (t1).
                tied <- private$.tiedBest()
                lead <- if (ranked && length(tied) > 1)
                    sprintf(
                        jmvcore::.("Among the tests evaluated, %1$s were tied for the highest observed balanced accuracy; the figures below describe %2$s, the first of them. This is a descriptive ranking only and not evidence of superiority or a clinical recommendation."),
                        paste(private$.safeHtmlOutput(tied), collapse = ", "), private$.safeHtmlOutput(best_test))
                else if (ranked)
                    sprintf(
                        jmvcore::.("Among the tests evaluated, %s had the highest observed balanced accuracy; this is a descriptive ranking only and not evidence of superiority or a clinical recommendation."),
                        private$.safeHtmlOutput(best_test))
                else if (!is.null(private$.ranking_common_n))
                    # Different case sets: each test's own figures exist; only the common
                    # cases lack a reference class (the basis sentence says so).
                    sprintf(
                        jmvcore::.("No test could be ranked on the cases common to all tests; the figures below describe %s, the first test selected."),
                        private$.safeHtmlOutput(best_test))
                else
                    sprintf(
                        jmvcore::.("No test could be ranked by balanced accuracy because one reference-standard class has no cases; the figures below describe %s, the first test selected."),
                        private$.safeHtmlOutput(best_test))

                # PPV/NPV are prevalence-dependent, and when the user supplies a
                # population prevalence (pp) .calculateDiagnosticMetrics computes them by
                # Bayes at THAT value while accuracy stays at the prevalence of this
                # sample. The two therefore cannot be reported under one prevalence.
                n_total_best <- best$TP + best$FN + best$TN + best$FP
                sample_prev_pct <- if (isTRUE(n_total_best > 0)) (best$TP + best$FN) / n_total_best * 100 else NA_real_
                use_pp <- isTRUE(self$options$pp) && !is.na(self$options$pprob)

                predictive_sentence <- if (use_pp) {
                    # pprob may be 1e-9 .. 1-1e-9: give enough decimals that neither end prints
                    # as 0.0% or 100.0% (default 0.3 still reads 30.0).
                    fmt_edge <- function(x) if (length(x) != 1 || !is.finite(x))
                        jmvcore::.("not estimable") else sprintf(jmvcore::.("%s%%"), private$.pctAdaptive(x))
                    sprintf(
                        jmvcore::.("At the %1$s%% population prevalence supplied for this analysis, positive predictive value was %2$s and negative predictive value was %3$s; overall accuracy was %4$s at the prevalence of this sample. Predictive values and accuracy change with prevalence and would need to be recomputed for a population with a different disease rate."),
                        private$.pctAdaptive(self$options$pprob), fmt_edge(best_metrics$PPV),
                        fmt_edge(best_metrics$NPV), acc_pct)
                } else if (is.na(sample_prev_pct)) {
                    # Two complete alternative sentences rather than splicing a translated
                    # noun phrase ("the X% prevalence observed...") into an English frame.
                    sprintf(
                        jmvcore::.("At the disease prevalence observed in this sample, %1$s positive predictive value, %2$s negative predictive value, and %3$s overall accuracy were observed; predictive values and accuracy change with prevalence and would need to be recomputed for a population with a different disease rate."),
                        ppv_pct, npv_pct, acc_pct)
                } else if (isTRUE(best$n_analyzed == best$n_total)) {
                    sprintf(
                        jmvcore::.("At the %1$s%% disease prevalence observed in this sample, %2$s positive predictive value, %3$s negative predictive value, and %4$s overall accuracy were observed; predictive values and accuracy change with prevalence and would need to be recomputed for a population with a different disease rate."),
                        private$.pctAdaptive(sample_prev_pct / 100), ppv_pct, npv_pct, acc_pct)
                } else {
                    # IR2-decisioncompare-03: these figures come from the test's own analysed
                    # cases, which are not the sample when some of its results are missing or
                    # excluded; name that denominator rather than "this sample".
                    sprintf(
                        jmvcore::.("At the %1$s%% disease prevalence among the %2$d cases analysed for %3$s, %4$s positive predictive value, %5$s negative predictive value, and %6$s overall accuracy were observed; predictive values and accuracy change with prevalence and would need to be recomputed for a population with a different disease rate."),
                        private$.pctAdaptive(sample_prev_pct / 100), as.integer(n_total_best),
                        private$.safeHtmlOutput(best_test), ppv_pct, npv_pct, acc_pct)
                }

                # IR-decisioncompare-01: a "significant difference" sentence must say which way it
                # runs (with the exploratory framing when the global test was not significant), and
                # must say so when the test named above is on the losing side.
                direction_note <- if (self$options$statComp && length(test_results) >= 2 &&
                                      isTRUE(private$.any_significant_comparison)) {
                    findings <- private$.pairwiseFindingsText()
                    if (nzchar(findings)) findings
                    else jmvcore::.("No pairwise comparison reached statistical significance after Holm-Bonferroni adjustment; the significant result is the global Cochran's Q test, which does not show which tests differ.")
                } else ""
                conflict_note <- if (ranked) private$.rankingConflictSentence(best_test) else ""

                # IR-decisioncompare-02: the Methods name the per-class tests, so the Results report
                # them -- always for two tests, and for three tests the pairs where sensitivity or
                # specificity differs at p < 0.05 (unadjusted, as the sentence says).
                class_note <- if (self$options$statComp && length(test_results) >= 2) {
                    ks <- seq_along(private$.pair_results %||% list())
                    sentences <- vapply(ks, function(k) {
                        dr <- if (length(private$.diff_results) >= k) private$.diff_results[[k]] else NULL
                        show <- length(test_results) == 2 ||
                            isTRUE(dr$p_sens < private$P_THRESHOLD_SIGNIFICANT) ||
                            isTRUE(dr$p_spec < private$P_THRESHOLD_SIGNIFICANT)
                        if (show) private$.classDifferenceSentence(k) else ""
                    }, character(1))
                    paste(sentences[nzchar(sentences)], collapse = " ")
                } else ""

                # Built from complete sentences joined by a space, instead of one frame
                # that spliced a clause ("%s, with %s sensitivity...") no translator could
                # reorder. nzchar() drops the optional pieces without leaving double spaces.
                pieces <- c(
                    lead,
                    private$.rankingBasisSentence(),
                    sprintf(jmvcore::.("Sensitivity was %1$s%2$s and specificity was %3$s%4$s."),
                        sens_pct, sens_ci, spec_pct, spec_ci),
                    predictive_sentence,
                    significance_note,
                    direction_note,
                    class_note,
                    conflict_note,
                    sprintf(jmvcore::.("The likelihood ratio for positive results was %1$s and for negative results was %2$s."),
                        fmt_number(best_metrics$LRP), fmt_number(best_metrics$LRN)),
                    if (indistinguishable)
                        jmvcore::.("Because the differences in overall accuracy were not statistically significant, this ranking reflects the observed sample and should not be reported as evidence that one test outperforms the others.")
                    else ""
                )

                return(paste(pieces[nzchar(pieces)], collapse = " "))
            },

            # Summarise the highest-ranked test's measured performance. This panel
            # describes what was computed; it does not tell the user what to do with it.
            .generateClinicalRecommendations = function(test_results, best_test) {
                best_metrics <- test_results[[best_test]]$metrics
                sens_pct <- best_metrics$Sens * 100
                spec_pct <- best_metrics$Spec * 100
                # Printed values are rounded half-up like every other panel (IR-decisioncompare-04);
                # the band tests below use the unrounded rates. A rate just under 95% would round to
                # "95.0" beside "reached neither 95%", so at that edge it keeps two decimals,
                # truncated, as .formatP() does at 0.05.
                show_rate <- function(x) {
                    s <- private$.halfUp1(x)
                    if (isTRUE(x < 95 && s >= 95)) sprintf("%.2f", floor(x * 100) / 100) else sprintf("%.1f", s)
                }
                sens_show <- show_rate(sens_pct)
                spec_show <- show_rate(spec_pct)
                best_test_safe <- private$.safeHtmlOutput(best_test)
                ranked <- isTRUE(private$.best_test_ranked)

                recommendations <- '<div style="background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 8px; color: inherit;">'

                # t1: a tie names every tied test, as the Results lead does.
                tied <- private$.tiedBest()
                if (length(tied) > 1)
                    recommendations <- paste0(recommendations, "<p>", sprintf(
                        jmvcore::.("%1$s were tied for the highest observed balanced accuracy; this panel describes %2$s, the first of them."),
                        paste(private$.safeHtmlOutput(tied), collapse = ", "), best_test_safe), "</p>")

                # The test name used to be pasted in front of a sentence fragment
                # (" had high sensitivity..."), which no catalog can translate. Each branch
                # is now one complete sentence with the name as its first %s.
                # A missing rate is checked FIRST: with sensitivity 96% and specificity not
                # estimable, the sensitivity branch used to print "lower specificity (NA%)".
                if (is.na(sens_pct) || is.na(spec_pct)) {
                    # Do not state a numeric negative about a rate that has no denominator.
                    recommendations <- paste0(
                        recommendations,
                        sprintf(jmvcore::.("<p><strong>Measured performance:</strong> sensitivity and/or specificity could not be computed for %s because one gold-standard class has no cases, so that rate has an empty denominator. This test is named here only because the ranking had nothing else to compare; check the 2x2 counts in the Recoded Data table before reading any other panel.</p>"),
                            best_test_safe)
                    )
                } else if (sens_pct >= 95 && spec_pct >= 95) {
                    recommendations <- paste0(
                        recommendations,
                        sprintf(jmvcore::.("<p><strong>Measured performance:</strong> %1$s had both high sensitivity (%2$s%%) and high specificity (%3$s%%) in this sample: few false negatives and few false positives.</p>"),
                            best_test_safe, sens_show, spec_show)
                    )
                } else if (sens_pct >= 95) {
                    recommendations <- paste0(
                        recommendations,
                        sprintf(jmvcore::.("<p><strong>Measured performance:</strong> %1$s had high sensitivity (%2$s%%) but lower specificity (%3$s%%) in this sample: few false negatives, more false positives.</p>"),
                            best_test_safe, sens_show, spec_show)
                    )
                } else if (spec_pct >= 95) {
                    recommendations <- paste0(
                        recommendations,
                        sprintf(jmvcore::.("<p><strong>Measured performance:</strong> %1$s had high specificity (%2$s%%) but lower sensitivity (%3$s%%) in this sample: few false positives, more false negatives.</p>"),
                            best_test_safe, spec_show, sens_show)
                    )
                } else {
                    recommendations <- paste0(
                        recommendations,
                        sprintf(jmvcore::.("<p><strong>Measured performance:</strong> %1$s reached neither 95%% sensitivity (%2$s%%) nor 95%% specificity (%3$s%%) in this sample, so at this operating point it produces both false negatives and false positives at a rate you can read off the Decision Test Comparison table. These are the rates observed in these particular cases, not a fixed property of the test.</p>"),
                            best_test_safe, sens_show, spec_show)
                    )
                }

                # IR-decisioncompare-01: the paragraph above names the balanced-accuracy leader. When a
                # significant comparison found it LESS accurate overall, say so here as well.
                conflict <- if (ranked) private$.rankingConflictSentence(best_test, exploratory_note = TRUE) else ""
                if (nzchar(conflict))
                    recommendations <- paste0(
                        recommendations,
                        '<p style="background-color: rgba(216, 33, 50, 0.18); padding: 10px; border-radius: 4px; color: inherit;"><strong>',
                        jmvcore::.("Caution:"), "</strong> ", conflict, "</p>")

                # The panel above names one test. Say so plainly when the analysis could
                # not actually separate it from the others -- otherwise a ranking driven
                # by sampling noise reads as a finding.
                # Both caution paragraphs say the test "had the highest observed balanced
                # accuracy"; with nothing ranked the measured-performance paragraph above already
                # says why this test is named (VAL-decisioncompare-08).
                if (ranked && length(test_results) >= 2 && self$options$statComp &&
                    isFALSE(private$.any_significant_comparison) && !isTRUE(private$.mcnemar_uninformative)) {
                    recommendations <- paste0(
                        recommendations,
                        '<p style="background-color: rgba(216, 33, 50, 0.18); padding: 10px; border-radius: 4px; color: inherit;">',
                        sprintf(jmvcore::.("<strong>Caution:</strong> No statistically significant difference in overall accuracy was detected between the tests compared. %s is named here only because it had the highest observed balanced accuracy in this sample; the data neither establish that it outperforms the others nor establish that the tests are equivalent, since a non-significant result may simply reflect limited power. The confidence intervals above show how large a real difference remains compatible with these data.</p>"),
                            best_test_safe)
                    )
                } else if (ranked && length(test_results) >= 2 && self$options$statComp &&
                    identical(private$.any_significant_comparison, NA)) {
                    # No comparison was computable. Silence here would leave the panel
                    # naming a single test with nothing to say that it was never tested
                    # against the others.
                    recommendations <- paste0(
                        recommendations,
                        '<p style="background-color: rgba(216, 33, 50, 0.18); padding: 10px; border-radius: 4px; color: inherit;">',
                        sprintf(jmvcore::.("<strong>Caution:</strong> No statistical comparison between the tests could be computed for these data, so no test of one test against another was performed; the reason is given in the Important Information panel. %s is named here only because it had the highest observed balanced accuracy in this sample, and these data say nothing about whether the tests differ.</p>"),
                            best_test_safe)
                    )
                }

                # Which prevalence PPV/NPV were computed at depends on the pp option --
                # the same branch .calculateDiagnosticMetrics uses. Accuracy is always at
                # the prevalence of this sample.
                recommendations <- paste0(
                    recommendations,
                    "<p><strong>", jmvcore::.("Scope of these estimates:"), "</strong> ",
                    jmvcore::.("PPV, NPV and overall accuracy all depend on disease prevalence."), " ",
                    if (isTRUE(self$options$pp) && !is.na(self$options$pprob))
                        jmvcore::.("PPV and NPV were computed at the population prevalence you supplied; overall accuracy reflects the prevalence of this sample.")
                    else
                        jmvcore::.("They were all computed at the prevalence of this sample."),
                    " ",
                    jmvcore::.("These estimates are apparent performance in this dataset and have not been externally validated."),
                    "</p></div>"
                )

                return(recommendations)
            },

            # Generate comprehensive about this analysis panel
            .generateAboutPanel = function() {
                about_html <- paste0(
                    '<div style="font-family: Arial, sans-serif; max-width: 900px; margin: 0 auto; padding: 20px;">',
                    '<h2 style="color: inherit; text-align: center; border-bottom: 2px solid #3498db; padding-bottom: 10px;">',
                    paste0(" ", jmvcore::.("About Medical Decision Test Comparison"), "</h2>"),

                    # What This Analysis Does
                    '<div style="background-color: rgba(33, 150, 243, 0.10); padding: 20px; border-radius: 10px; margin: 20px 0; color: inherit;">',
                    paste0("<h3 style=\"color: inherit; margin-top: 0;\"> ", jmvcore::.("What This Analysis Does"), "</h3>"),
                    '<p style="line-height: 1.6; color: inherit;">',
                    jmvcore::.("This tool compares the diagnostic performance of multiple medical tests against a gold standard reference. It systematically evaluates sensitivity, specificity, predictive values, likelihood ratios, and overall accuracy to support transparent comparison of observed performance. It does not select a test for clinical use."),
                    "</p>",
                    "</div>",

                    # When to Use
                    '<div style="background-color: rgba(114, 184, 33, 0.1); border: 1px solid #8bc34a; padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;">',
                    paste0("<h3 style=\"color: inherit; margin-top: 0;\"> ", jmvcore::.("When to Use This Analysis"), "</h3>"),
                    '<ul style="line-height: 1.8; color: inherit;">',
                    jmvcore::.("<li><strong>Test Validation:</strong> Comparing new diagnostic methods against established standards</li>"),
                    jmvcore::.("<li><strong>Method Comparison:</strong> Describing and testing differences among paired tests</li>"),
                    jmvcore::.("<li><strong>Clinical Research:</strong> Validating biomarkers, imaging techniques, or clinical assessments</li>"),
                    jmvcore::.("<li><strong>Quality Assessment:</strong> Measuring agreement between different raters or methods</li>"),
                    jmvcore::.("<li><strong>Protocol Development:</strong> Optimizing diagnostic workflows</li>"),
                    "</ul>",
                    "</div>",

                    # How to Use
                    '<div style="background-color: rgba(255, 169, 33, 0.14); border: 1px solid #ff9800; padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;">',
                    paste0("<h3 style=\"color: inherit; margin-top: 0;\"> ", jmvcore::.("How to Use This Analysis"), "</h3>"),
                    '<ol style="line-height: 1.8; color: inherit;">',
                    jmvcore::.("<li><strong>Select Gold Standard:</strong> Choose your most reliable reference test (e.g., biopsy, expert consensus)</li>"),
                    jmvcore::.("<li><strong>Choose Tests to Compare:</strong> Select 2-3 diagnostic tests you want to evaluate</li>"),
                    jmvcore::.("<li><strong>Define Positive and Negative Levels:</strong> Select both exact levels for the reference and every test; explicitly exclude other observed levels or recode them before analysis</li>"),
                    jmvcore::.("<li><strong>Configure Options:</strong> Enable statistical comparisons, confidence intervals, or visualizations as needed</li>"),
                    jmvcore::.("<li><strong>Run Analysis:</strong> Review estimates, denominators, exclusions, and statistical comparisons</li>"),
                    jmvcore::.("<li><strong>Copy Report:</strong> Use the auto-generated sentences for your documentation</li>"),
                    "</ol>",
                    "</div>",

                    # Key Metrics Explained
                    '<div style="background-color: rgba(153, 33, 170, 0.12); border: 1px solid #9c27b0; padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;">',
                    paste0("<h3 style=\"color: inherit; margin-top: 0;\"> ", jmvcore::.("Key Metrics Explained"), "</h3>"),
                    '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px; color: inherit;">',
                    "<div>",
                    jmvcore::.("<p><strong>Sensitivity:</strong> Probability test is positive when disease present (rule-out ability)</p>"),
                    jmvcore::.("<p><strong>Specificity:</strong> Probability test is negative when disease absent (rule-in ability)</p>"),
                    jmvcore::.("<p><strong>PPV:</strong> Probability of disease when test positive</p>"),
                    jmvcore::.("<p><strong>NPV:</strong> Probability of no disease when test negative</p>"),
                    "</div>",
                    "<div>",
                    jmvcore::.("<p><strong>LR+:</strong> How much positive test increases odds of disease</p>"),
                    jmvcore::.("<p><strong>LR-:</strong> How much negative test decreases odds of disease</p>"),
                    jmvcore::.("<p><strong>Accuracy:</strong> Overall probability of correct classification</p>"),
                    jmvcore::.("<p><strong>McNemar Test:</strong> Statistical comparison between paired tests</p>"),
                    "</div>",
                    "</div>",
                    "</div>",

                    # Illustrative examples (not clinical guidance)
                    '<div style="background-color: rgba(33, 159, 33, 0.1); border: 1px solid #4caf50; padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;">',
                    paste0("<h3 style=\"color: inherit; margin-top: 0;\"> ", jmvcore::.("Illustrative Interpretation Examples"), "</h3>"),
                    jmvcore::.("<p><strong>Important:</strong> These thresholds and use cases are examples only. They are not clinical guides, validated decision rules, or recommendations for patient care.</p>"),
                    jmvcore::.("<p>The radar plot's LR scale uses familiar likelihood-ratio evidence categories described in the Users' Guides to the Medical Literature (Jaeschke et al., 1994) solely as an illustrative visualization. Appropriate thresholds and consequences depend on the clinical setting, population, harms, benefits, and decision pathway.</p>"),
                    '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 15px; color: inherit;">',
                    "<div>",
                    paste0("<h4 style=\"margin-bottom: 5px;\">", jmvcore::.("Screening Tests (Rule-Out):"), "</h4>"),
                    paste0("<p style=\"margin-top: 0;\">", jmvcore::.("\u2022 Example emphasis: sensitivity and NPV<br>\u2022 Example threshold: 95%<br>\u2022 Study-specific aim: reduce false negatives"), "</p>"),
                    "</div>",
                    "<div>",
                    paste0("<h4 style=\"margin-bottom: 5px;\">", jmvcore::.("Confirmatory Tests (Rule-In):"), "</h4>"),
                    paste0("<p style=\"margin-top: 0;\">", jmvcore::.("\u2022 Example emphasis: specificity and PPV<br>\u2022 Example thresholds: 95% and 90%<br>\u2022 Study-specific aim: reduce false positives"), "</p>"),
                    "</div>",
                    "</div>",
                    "</div>",

                    # Assumptions and Limitations
                    '<div style="background-color: rgba(255, 203, 33, 0.14); border: 1px solid #ffc107; padding: 20px; border-radius: 8px; margin: 20px 0; color: inherit;">',
                    paste0("<h3 style=\"color: inherit; margin-top: 0;\"> ", jmvcore::.("Important Assumptions & Limitations"), "</h3>"),
                    '<ul style="line-height: 1.6; color: inherit;">',
                    jmvcore::.("<li><strong>Gold Standard:</strong> Assumes your reference test is truly accurate</li>"),
                    jmvcore::.("<li><strong>Sample Size:</strong> Results more reliable with larger, representative samples</li>"),
                    jmvcore::.("<li><strong>Prevalence Dependency:</strong> PPV and NPV vary with disease prevalence</li>"),
                    jmvcore::.("<li><strong>McNemar Test:</strong> Requires paired/matched data for statistical comparisons</li>"),
                    jmvcore::.("<li><strong>Missing Data:</strong> Standalone estimates use available reference/test pairs; paired comparisons use common determinate rows. Missing-data handling and counts should be reported.</li>"),
                    # Describe only the intervals this analysis actually renders.
                    # The per-test table is filtered to `ratiorows` above, which
                    # excludes lr.pos/lr.neg, and the LRP/LRN columns in
                    # decisioncompare.r.yaml carry no lower/upper -- so no
                    # likelihood-ratio interval is displayed anywhere here. An
                    # earlier version of this sentence advertised Simel (1991) LR
                    # intervals that the analysis never computes or shows.
                    jmvcore::.("<li><strong>Confidence Intervals:</strong> The per-test CI tables report Clopper-Pearson exact intervals for the proportions (sensitivity, specificity, PPV, NPV, accuracy and prevalence), as computed by epiR::epi.tests() with its default settings. Likelihood ratios are reported as point estimates only, without confidence intervals. The Overall Percent Agreement (OPA) table uses the selected single-proportion method. Paired differences use Newcombe's method-10 score interval for correlated proportions.</li>"),
                    "</ul>",
                    "</div>",
                    "</div>"
                )

                self$results$aboutAnalysis$setContent(about_html)
            },
            .plot1 = function(image1, ggtheme, ...) {
                plotData <- image1$state

                if (is.null(plotData))
                    return(FALSE)

                # Optimized data frame building - pre-allocate and batch create
                df <- private$.buildBarPlotData(plotData)

                # .buildBarPlotData() returns a COLUMN-LESS data.frame() when the state
                # is empty, and aes() below cannot resolve its variables against that.
                if (nrow(df) == 0)
                    return(FALSE)

                # Create plot
                plot <- ggplot2::ggplot(df, ggplot2::aes(x = metric, y = value, fill = test)) +
                    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
                    ggplot2::coord_flip() +
                    ggplot2::scale_y_continuous(labels = scales::percent) +
                    ggplot2::labs(
                        title = jmvcore::.("Comparison of Tests"),
                        x = "",
                        y = jmvcore::.("Value"),
                        fill = jmvcore::.("Test")
                    ) +
                    ggtheme +
                    ggplot2::theme(legend.position = "bottom")

                print(plot)
                TRUE
            },
            .plotRadar = function(imageRadar, ggtheme, ...) {
                plotData <- imageRadar$state

                if (is.null(plotData))
                    return(FALSE)

                # Optimized radar plot data building with clinical thresholds
                df <- private$.buildRadarPlotData(plotData)

                # .buildRadarPlotData() returns a COLUMN-LESS data.frame() when the
                # state is empty, so aes(x = metric, ...) below could not resolve its
                # variables. A NULL state and an empty state are different things.
                if (nrow(df) == 0)
                    return(FALSE)

                # Ensure factor ordering for consistent radar plot. Same translated
                # label vector the data builder used -- English literals here would drop
                # every point in a translated session.
                df$metric <- factor(df$metric, levels = private$.radarMetricLabels())

                # Create radar plot using ggplot2
                plot <- ggplot2::ggplot(df, ggplot2::aes(
                    x = metric, y = scaled_value,
                    group = test, color = test
                )) +
                    ggplot2::geom_line(linewidth = 1.2) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::coord_polar() +
                    ggplot2::scale_y_continuous(
                        limits = c(0, 100),
                        breaks = c(0, 25, 50, 75, 100),
                        labels = c("0%", "25%", "50%", "75%", "100%")
                    ) +
                    ggplot2::labs(
                        title = jmvcore::.("Radar Plot: Decision Test Statistics Comparison"),
                        subtitle = jmvcore::.("All metrics scaled 0-100%; LR scaling is illustrative only, not clinical guidance"),
                        x = "",
                        y = "",
                        color = jmvcore::.("Test")
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_text(size = 10),
                        axis.text.y = ggplot2::element_text(size = 8),
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9)
                    )

                print(plot)
                TRUE
            },

            # Build per-case heatmap data from cached processed data
            # Returns a plain data.frame suitable for setState (protobuf-safe)
            .buildHeatmapData = function() {
                pd <- private$.cached_processed_data
                if (is.null(pd)) {
                    return(NULL)
                }

                test_results <- private$.cached_test_results
                if (is.null(test_results) || length(test_results) == 0) return(NULL)
                testVariables <- names(test_results)

                encode <- function(x) {
                    ifelse(is.na(x), 2L, ifelse(x == "Positive", 1L, 0L))
                }
                # Reuse the exact recoding used by tables and paired comparisons.
                gold_binary <- encode(test_results[[1]]$gold_reference)

                # Sort cases: negatives first, positives second
                sort_order <- order(gold_binary)
                gold_sorted <- gold_binary[sort_order]

                n_cases <- length(gold_sorted)

                # Build long-format data.frame: (case_order, source, label, value). Every row is keyed
                # by a generated ID ("reference", "test_1", ...), never by a name: a test variable
                # called "Gold Standard" used to share the reference row's key, so its tiles were
                # drawn over the reference (IR-decisioncompare-07). The display name sits in `label`;
                # the reference label is translated at draw time.
                source_keys <- c("reference", paste0("test_", seq_along(testVariables)))
                source_labels <- c("", testVariables)
                n_sources <- length(source_keys)
                total_rows <- n_cases * n_sources

                case_order_vec <- integer(total_rows)
                source_vec <- character(total_rows)
                label_vec <- character(total_rows)
                value_vec <- integer(total_rows)

                idx <- 1
                # Gold standard row
                for (j in seq_len(n_cases)) {
                    case_order_vec[idx] <- j
                    source_vec[idx] <- source_keys[1]
                    label_vec[idx] <- source_labels[1]
                    value_vec[idx] <- gold_sorted[j]
                    idx <- idx + 1
                }

                # Test rows
                for (k in seq_along(testVariables)) {
                    # One row per case per test: O(n_cases) work for every test.
                    private$.checkpoint()

                    test_binary <- encode(test_results[[testVariables[k]]]$test_results)
                    test_sorted <- test_binary[sort_order]

                    for (j in seq_len(n_cases)) {
                        case_order_vec[idx] <- j
                        source_vec[idx] <- source_keys[k + 1]
                        label_vec[idx] <- source_labels[k + 1]
                        value_vec[idx] <- test_sorted[j]
                        idx <- idx + 1
                    }
                }

                # Return plain data.frame (protobuf-safe for setState)
                return(as.data.frame(list(
                    case_order = case_order_vec,
                    source = source_vec,
                    label = label_vec,
                    value = value_vec
                ), stringsAsFactors = FALSE))
            },

            # Render concordance heatmap
            .plotHeatmap = function(imageHeatmap, ggtheme, ...) {
                df_long <- imageHeatmap$state
                if (is.null(df_long) || nrow(df_long) == 0) {
                    return(FALSE)
                }

                # Rows are keyed by generated IDs ("reference", "test_1", ...) with the display name
                # in `label` (IR-decisioncompare-07). A state saved by an earlier version has no
                # `label` column and used the names themselves as keys, "Gold Standard" for the
                # reference; map it onto the same scheme.
                if (is.null(df_long$label)) {
                    df_long$label <- df_long$source
                    df_long$source[df_long$source == "Gold Standard"] <- "reference"
                }
                test_keys <- setdiff(unique(df_long$source), "reference")
                n_cases <- max(df_long$case_order)
                # Only the reference label is translated, here at draw time, so a state saved in one
                # language still draws correctly in another; user variable names are shown as given.
                source_labels <- vapply(split(df_long$label, df_long$source), `[`, character(1), 1)
                source_labels[["reference"]] <- jmvcore::.("Gold Standard")

                # Order: Gold Standard at top, then tests below
                df_long$source <- factor(df_long$source,
                    levels = rev(c("reference", test_keys))
                )

                p <- ggplot2::ggplot(df_long, ggplot2::aes(
                    x = case_order, y = source, fill = factor(value)
                )) +
                    ggplot2::geom_tile(color = "grey90", linewidth = 0.1) +
                    ggplot2::labs(
                        title = jmvcore::.("Concordance Heatmap: Per-Case Test Results vs Gold Standard"),
                        subtitle = sprintf(jmvcore::.("Cases sorted by gold standard result (n=%d)"), n_cases),
                        x = jmvcore::.("Sample"),
                        y = ""
                    ) +
                    ggtheme +
                    # After ggtheme: jamovi's ggtheme carries its own discrete fill scale, which
                    # replaced this one when it came first, leaving an untitled "0 1 2" legend
                    # in palette colours (VAL-decisioncompare-13). ggplot2 then prints "Scale for
                    # fill is already present"; that is expected. An UNNAMED labels vector is
                    # applied POSITIONALLY to the levels present in the data, so whenever one of
                    # the three encoded values was absent every label shifted onto the wrong
                    # colour -- black (Positive) tiles were labelled "Negative". Named labels
                    # plus explicit breaks bind each label to its value.
                    ggplot2::scale_fill_manual(
                        values = c("0" = "white", "1" = "black", "2" = "#f59e0b"),
                        breaks = c("0", "1", "2"),
                        labels = c("0" = jmvcore::.("Negative"), "1" = jmvcore::.("Positive"),
                                   "2" = jmvcore::.("Indeterminate / excluded")),
                        name = jmvcore::.("Result")
                    ) +
                    ggplot2::scale_y_discrete(labels = function(k) unname(source_labels[k])) +
                    ggplot2::theme(
                        panel.grid = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        plot.title = ggplot2::element_text(hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 9)
                    )

                print(p)
                TRUE
            },

            # Generate natural language summary of comparison results
            .generateSummary = function(test_results) {
                n_tests <- length(test_results)
                test_names <- names(test_results)

                html <- "<div style='background-color: rgba(155, 155, 155, 0.06); border-left:4px solid #2196F3; padding:15px; margin:10px 0; color: inherit;'>"
                html <- paste0(html, "<h4 style='margin-top:0;'> ", jmvcore::.("Summary"), "</h4>")

                if (n_tests == 3) {
                    # Check if Cochran's Q was performed. The Cochran row's KEY is
                    # "cochran_q_global" (the word "Overall" only appears in the displayed
                    # comparison cell), so match the key, not the cell value.
                    mcnemar_table <- self$results$mcnemarTable
                    # Exact key: pairwise row keys are built from user variable names, so a test
                    # named "Cochrane" must not be read as the Cochran row.
                    is_q <- length(mcnemar_table$rowKeys) > 0 &&
                        identical(as.character(mcnemar_table$rowKeys[[1]]), "cochran_q_global")
                    q_row <- if (is_q) mcnemar_table$rowKeys[[1]] else NULL
                    q_stat <- if (is_q) mcnemar_table$getCell(rowKey = q_row, col = "stat")$value else NA
                    q_p <- if (is_q) mcnemar_table$getCell(rowKey = q_row, col = "p")$value else NA
                    if (is_q && length(q_stat) == 1 && !is.na(q_stat) && length(q_p) == 1 && !is.na(q_p)) {
                        # Wrapped in .(), so the chi-squared escapes must be the 4-hex-digit
                        # form: \u{XXXX} is resolved by R at parse time but stored literally in
                        # the catalog, and the lookup could then never match. Each outcome is ONE
                        # complete sentence: the verb used to be a separate msgid pasted between
                        # the test and "among the three tests", which a verb-final language such
                        # as Turkish cannot reorder.
                        if (q_p < private$P_THRESHOLD_SIGNIFICANT) {
                            # A significant global test is evidence that the accuracies are not all
                            # equal; it does not show that any particular test differs from the
                            # others, which is what the pairwise rows are for.
                            # The closing "The pairwise comparisons above show which pairs differ." is
                            # gone: the paragraph below now says which pairs differ, or that none does,
                            # and the old sentence contradicted the second case.
                            html <- paste0(html, sprintf(
                                jmvcore::.("<p><b>Overall comparison:</b> Cochran's Q test (Q = %1$.2f, p %2$s) <span style='color:inherit;'><b>found significant differences</b></span> among the three tests: these data are evidence that the tests do not all have the same accuracy.</p>"),
                                private$.halfUp(q_stat, 2L), private$.formatP(q_p)
                            ))

                            # IR-decisioncompare-01: name the pairs that differ and the direction, or
                            # say that none does after adjustment.
                            findings <- private$.pairwiseFindingsText()
                            html <- paste0(html, "<p><b>", jmvcore::.("Pairwise comparisons:"), "</b> ",
                                if (nzchar(findings)) findings
                                else jmvcore::.("No pairwise comparison reached p < 0.05 after Holm-Bonferroni adjustment, so these data do not show which tests differ."),
                                "</p>")

                            # Descriptive balanced-accuracy ranking; not a superiority test.
                            best_test <- private$.findBestTest(test_results)
                            best_acc <- test_results[[best_test]]$metrics$AccurT
                            tied <- private$.tiedBest()

                            # One msgid per complete sentence, with the test name as %s
                            # inside it, instead of a name pasted onto a bare predicate. A tie
                            # names every tied test (t1).
                            html <- paste0(html, if (isTRUE(private$.best_test_ranked) && length(tied) > 1) sprintf(
                                jmvcore::.("<p><b>Descriptive interpretation:</b> %1$s were tied for the highest observed balanced accuracy in this sample. This descriptive ranking is not a superiority test or clinical recommendation. Review the pairwise comparisons above for inferential results.</p>"),
                                paste(private$.safeHtmlOutput(tied), collapse = ", ")
                            ) else if (isTRUE(private$.best_test_ranked)) sprintf(
                                jmvcore::.("<p><b>Descriptive interpretation:</b> <span style='color:inherit;'><b>%1$s</b></span> had the highest observed balanced accuracy in this sample (overall accuracy %2$.1f%%). This descriptive ranking is not a superiority test or clinical recommendation. Review the pairwise comparisons above for inferential results.</p>"),
                                private$.safeHtmlOutput(best_test), private$.halfUp1(best_acc * 100)
                            ) else if (!is.null(private$.ranking_common_n)) jmvcore::.("<p><b>Descriptive interpretation:</b> no test is ranked on the cases common to all tests. Review the pairwise comparisons above for inferential results.</p>")
                            else jmvcore::.("<p><b>Descriptive interpretation:</b> balanced accuracy could not be estimated for any test because one reference-standard class has no cases, so no test is ranked. Review the pairwise comparisons above for inferential results.</p>"))
                            basis <- private$.rankingBasisSentence()
                            if (nzchar(basis))
                                html <- paste0(html, "<p>", basis, "</p>")

                            conflict <- private$.rankingConflictSentence(best_test)
                            if (nzchar(conflict))
                                html <- paste0(html, "<p>", conflict, "</p>")
                        } else {
                            html <- paste0(html, sprintf(
                                jmvcore::.("<p><b>Overall comparison:</b> Cochran's Q test (Q = %1$.2f, p %2$s) <span style='color:inherit;'><b>did not detect a significant difference</b></span> among the three tests. This is not evidence that they perform equally: with this sample size a clinically important accuracy difference could go undetected, and no test of one test against another across a pre-specified equivalence margin was performed. (If requested, the criterion column in the Overall Percent Agreement table compares each test with a user-specified descriptive OPA threshold; it is not a noninferiority comparison between tests or clinical guidance.) The confidence intervals for the paired differences in the Differences with 95%% Confidence Intervals table show how large a difference remains compatible with these data.</p>"),
                                private$.halfUp(q_stat, 2L), private$.formatP(q_p)
                            ))
                            # Cochran's Q and each pair use different rows, so a pair can reach
                            # significance when the global test does not; report it as exploratory
                            # rather than leaving it out (IR-decisioncompare-01). The passage carries
                            # the exploratory lead-in itself.
                            findings <- private$.pairwiseFindingsText()
                            if (nzchar(findings))
                                html <- paste0(html, "<p><b>", jmvcore::.("Pairwise comparisons:"), "</b> ",
                                               findings, "</p>")
                        }
                    } else if (length(mcnemar_table$rowKeys) == 0) {
                        # Nothing in the table at all: no Cochran row AND no pairwise row,
                        # so the sentence below would point at comparisons that do not exist.
                        html <- paste0(html, "<p>", jmvcore::.("No statistical comparison between the tests could be computed for these data: neither the global Cochran's Q test nor any pairwise McNemar test produced a result. The reason is given in the Important Information panel, and these data say nothing about whether the tests differ."), "</p>")
                    } else {
                        # Without this the whole 3-test branch was skipped and the box was
                        # emitted containing only its heading.
                        html <- paste0(html, "<p>", jmvcore::.("The global Cochran's Q test could not be computed for these data; the reason is given in the Important Information panel. The pairwise comparisons and their confidence intervals in the tables above are unaffected."), "</p>")
                        findings <- private$.pairwiseFindingsText()
                        if (nzchar(findings))
                            html <- paste0(html, "<p><b>", jmvcore::.("Pairwise comparisons:"), "</b> ",
                                           findings, "</p>")
                    }
                } else if (n_tests == 2) {
                    # Extract McNemar results
                    mcnemar_table <- self$results$mcnemarTable
                    test1_name <- test_names[1]
                    test2_name <- test_names[2]
                    # The rows McNemar's test used. The verdict and the accuracies printed beside it
                    # come from them: each test's standalone accuracy uses its own rows, and when
                    # the tests are missing on different cases the two can point in opposite
                    # directions (VAL-decisioncompare-01).
                    paired <- private$.pairedAccuracy(test_results, test1_name, test2_name)
                    if (length(mcnemar_table$rowKeys) > 0 && !is.null(paired)) {
                        mcn_row <- mcnemar_table$rowKeys[[1]]
                        mcn_stat <- mcnemar_table$getCell(rowKey = mcn_row, col = "stat")$value
                        mcn_p <- mcnemar_table$getCell(rowKey = mcn_row, col = "p")$value
                        mcn_method <- mcnemar_table$getCell(rowKey = mcn_row, col = "method")$value
                        name1 <- private$.safeHtmlOutput(test1_name)
                        name2 <- private$.safeHtmlOutput(test2_name)

                        html <- paste0(html, sprintf(
                            jmvcore::.("<p><b>Comparison:</b> %1$s vs %2$s on the %3$d cases with a determinate result for both tests and the reference standard: accuracy %4$.1f%% vs %5$.1f%%.</p>"),
                            name1, name2, paired$n, private$.halfUp1(paired$acc1 * 100), private$.halfUp1(paired$acc2 * 100)
                        ))

                        test_text <- if (length(mcn_stat) == 0 || is.na(mcn_stat)) {
                            paste0(sprintf(jmvcore::.("<p><b>%1$s:</b> p %2$s"), mcn_method, private$.formatP(mcn_p)), " - ")
                        } else {
                            paste0(sprintf(jmvcore::.("<p><b>%1$s:</b> \u03C7\u00B2 = %2$.2f, p %3$s"), mcn_method, private$.halfUp(mcn_stat, 2L), private$.formatP(mcn_p)), " - ")
                        }

                        if ((paired$only1 + paired$only2) == 0) {
                            # No discordant pairs: p = 1 is a convention, not a test result (the
                            # table footnote says so), so no verdict is printed.
                            html <- paste0(html, sprintf(
                                jmvcore::.("<p><b>%1$s:</b> not informative here. No case was classified correctly by exactly one of the two tests, so McNemar's test has no discordant pairs to compare and p is shown as 1 only by convention. Read the accuracy difference and its paired confidence interval instead.</p>"),
                                mcn_method))
                        } else if (mcn_p < private$P_THRESHOLD_SIGNIFICANT) {
                            html <- paste0(html, test_text)
                            # McNemar tests diagnostic CORRECTNESS on the discordant pairs, so the
                            # direction is theirs: b (only test 1 correct) against c (only test 2).
                            # A significant result implies b != c.
                            first_better <- paired$only1 > paired$only2
                            html <- paste0(html, sprintf(
                                jmvcore::.("<span style='color:inherit;'><b>Significant difference detected.</b></span> %1$s classified significantly more cases correctly than %2$s: of the %3$d cases where exactly one test was correct, %4$d favoured %1$s. McNemar's test compares overall correctness only; the two tests may still differ in the sensitivity/specificity balance.</p>"),
                                if (first_better) name1 else name2,
                                if (first_better) name2 else name1,
                                paired$only1 + paired$only2,
                                max(paired$only1, paired$only2)
                            ))
                        } else {
                            html <- paste0(
                                html, test_text,
                                "<span style='color:inherit;'><b>", jmvcore::.("No significant difference detected."), "</b></span> ",
                                jmvcore::.("This is not evidence that the two tests are equivalent: McNemar's test uses only the discordant pairs, so with few discordances it has little power to detect a real difference. Check the confidence interval for the accuracy difference to see how large a difference remains compatible with these data.</p>")
                            )
                        }

                        # Same disclosure as the Report when the p-value and the Newcombe interval
                        # disagree near p = 0.05 (unrounded limits).
                        if ((paired$only1 + paired$only2) > 0) {
                            sig <- isTRUE(mcn_p < private$P_THRESHOLD_SIGNIFICANT)
                            ci_excludes_0 <- isTRUE(paired$difference$lower > 0 || paired$difference$upper < 0)
                            if (sig && !ci_excludes_0) {
                                html <- paste0(html, "<p>", jmvcore::.("This significant result is borderline: the 95 percent confidence interval for the accuracy difference comes from Newcombe's method 10, a different procedure from McNemar's test, and it includes 0, the value of no difference."), "</p>")
                            } else if (!sig && ci_excludes_0) {
                                html <- paste0(html, "<p>", jmvcore::.("The 95 percent confidence interval for the accuracy difference comes from Newcombe's method 10, a different procedure from McNemar's test, and it excludes 0, the value of no difference; the two procedures disagree on these data, so report both rather than either alone."), "</p>")
                            }
                            # IR-decisioncompare-02: which test is more accurate overall depends on the
                            # prevalence among these cases.
                            if (isTRUE(is.finite(paired$prevalence)))
                                html <- paste0(html, "<p>", sprintf(
                                    jmvcore::.("Overall accuracy weights sensitivity and specificity by the prevalence of the target condition among these %1$d cases (%2$s%%), so which test is more accurate overall can change with the case mix; the Differences with 95%% Confidence Intervals table compares sensitivity and specificity separately."),
                                    paired$n, private$.pctAdaptive(paired$prevalence)), "</p>")
                        }
                    } else {
                        # No McNemar row means the pair was not computable at all. Without
                        # this the box was emitted containing only its heading -- the same
                        # failure the three-test branch above guards against.
                        html <- paste0(html, "<p>", jmvcore::.("No McNemar result is available for these two tests; the reason is given in the Important Information panel. No statistical comparison was performed, so these data say nothing about whether the tests differ."), "</p>")
                    }
                }

                html <- paste0(html, "</div>")
                if (nzchar(private$.cochran_method_note))
                    html <- paste0(html, "<p>", private$.cochran_method_note, "</p>")
                self$results$summaryReport$setContent(html)
            },

            # Generate manuscript-ready report sentence
            .generateReportSentence = function(test_results) {
                n_tests <- length(test_results)
                test_names <- names(test_results)

                html <- "<div style='background-color: rgba(33, 33, 33, 0.07); padding:15px; border:1px solid #ccc; margin:10px 0; color: inherit;'>"
                html <- paste0(html, "<h4 style='margin-top:0;'> ", jmvcore::.("Manuscript-Ready Report"), "</h4>")
                html <- paste0(
                    html, "<p style='font-size:10pt; color:inherit; margin-bottom:10px;'>",
                    jmvcore::.("Copy and adapt to your manuscript. Verify all statistical values and add clinical context."), "</p>"
                )

                html <- paste0(html, "<div style='background-color: rgba(255, 255, 255, 0.06); padding:12px; font-family:serif; font-size:11pt; line-height:1.8; color: inherit;'>")

                if (n_tests == 3) {
                    # Match the Cochran row by its KEY ("cochran_q_global"), not the cell value.
                    mcnemar_table <- self$results$mcnemarTable
                    is_q <- length(mcnemar_table$rowKeys) > 0 &&
                        identical(as.character(mcnemar_table$rowKeys[[1]]), "cochran_q_global")
                    q_row <- if (is_q) mcnemar_table$rowKeys[[1]] else NULL
                    q_stat <- if (is_q) mcnemar_table$getCell(rowKey = q_row, col = "stat")$value else NA
                    q_p <- if (is_q) mcnemar_table$getCell(rowKey = q_row, col = "p")$value else NA
                    q_n <- if (is_q) mcnemar_table$getCell(rowKey = q_row, col = "n")$value else NA
                    if (is_q && length(q_stat) == 1 && !is.na(q_stat) && length(q_p) == 1 && !is.na(q_p)) {
                        # Two complete sentences instead of splicing the verb ("revealed" /
                        # "did not detect") through %s, which no language can reorder. Copy-ready
                        # text carries its N (VAL-decisioncompare-05) and never "p = 0.000".
                        report <- paste0(if (q_p < private$P_THRESHOLD_SIGNIFICANT)
                            sprintf(jmvcore::.("Cochran's Q test revealed a significant difference in diagnostic accuracy among the three tests (n = %1$d cases with all three results; Q = %2$.2f, p %3$s)."),
                                q_n, private$.halfUp(q_stat, 2L), private$.formatP(q_p))
                        else
                            sprintf(jmvcore::.("Cochran's Q test did not detect a significant difference in diagnostic accuracy among the three tests (n = %1$d cases with all three results; Q = %2$.2f, p %3$s)."),
                                q_n, private$.halfUp(q_stat, 2L), private$.formatP(q_p)), " ")
                        findings <- private$.pairwiseFindingsText()
                        if (q_p >= private$P_THRESHOLD_SIGNIFICANT) {
                            report <- paste0(
                                report,
                                paste0(jmvcore::.("This does not establish that the tests perform equally; no test of one test against another across a pre-specified equivalence margin was performed. (If requested, the OPA criterion column compares each test with a user-specified descriptive threshold; it is not a noninferiority comparison between tests or clinical guidance.)"), " ")
                            )
                            # A pair can be significant when the global test is not (they use
                            # different rows); the passage reports it as exploratory
                            # (IR-decisioncompare-01).
                            if (nzchar(findings))
                                report <- paste0(report, findings, " ")
                        }

                        if (q_p < private$P_THRESHOLD_SIGNIFICANT) {
                            report <- paste0(
                                report,
                                if (identical(self$options$mcnemarMethod, "asymptotic"))
                                    jmvcore::.("Post-hoc pairwise comparisons used the asymptotic McNemar test without continuity correction, with Holm-Bonferroni correction, to identify specific differences between tests.")
                                else
                                    jmvcore::.("Post-hoc pairwise comparisons used the McNemar mid-p test with Holm-Bonferroni correction to identify specific differences between tests."),
                                " "
                            )
                            # IR-decisioncompare-01: the specific differences themselves, with their
                            # direction, or a statement that none survived the adjustment.
                            report <- paste0(report,
                                if (nzchar(findings)) findings
                                else jmvcore::.("No pairwise comparison reached statistical significance after Holm-Bonferroni adjustment."),
                                " ")

                            # Descriptive balanced-accuracy ranking only. A tie names every tied
                            # test (t1).
                            best_test <- private$.findBestTest(test_results)
                            tied <- private$.tiedBest()

                            report <- paste0(report, if (isTRUE(private$.best_test_ranked) && length(tied) > 1) sprintf(
                                jmvcore::.("%s were tied for the highest observed balanced accuracy; this descriptive ranking was not tested as a superiority claim and is not a clinical recommendation."),
                                paste(private$.safeHtmlOutput(tied), collapse = ", ")
                            ) else if (isTRUE(private$.best_test_ranked)) sprintf(
                                jmvcore::.("%s had the highest observed balanced accuracy; this descriptive ranking was not tested as a superiority claim and is not a clinical recommendation."),
                                private$.safeHtmlOutput(best_test)
                            ) else if (!is.null(private$.ranking_common_n)) jmvcore::.("No test is ranked on the cases common to all tests.")
                            else jmvcore::.("Balanced accuracy could not be estimated for any test because one reference-standard class has no cases, so no test is ranked."))
                            basis <- private$.rankingBasisSentence()
                            if (nzchar(basis))
                                report <- paste(report, basis)

                            conflict <- private$.rankingConflictSentence(best_test)
                            if (nzchar(conflict))
                                report <- paste(report, conflict)
                        }

                        html <- paste0(html, trimws(report))
                    } else if (length(mcnemar_table$rowKeys) == 0) {
                        # No row at all: do not tell the author to report pairwise
                        # comparisons that were never computed.
                        html <- paste0(html, jmvcore::.("No statistical comparison between the tests could be computed for these data: neither the global Cochran's Q test nor any pairwise McNemar test produced a result; the reason is given in the Important Information panel. Report the per-test performance only."))
                    } else {
                        # Same empty-box failure as in .generateSummary().
                        html <- paste0(html, jmvcore::.("The global Cochran's Q test could not be computed for these data; the reason is given in the Important Information panel. Report the pairwise comparisons and their confidence intervals instead."))
                        findings <- private$.pairwiseFindingsText()
                        if (nzchar(findings))
                            html <- paste(html, findings)
                    }
                } else if (n_tests == 2) {
                    mcnemar_table <- self$results$mcnemarTable
                    test1_name <- test_names[1]
                    test2_name <- test_names[2]
                    # The rows McNemar's test used; see .generateSummary() (VAL-decisioncompare-01).
                    paired <- private$.pairedAccuracy(test_results, test1_name, test2_name)
                    if (length(mcnemar_table$rowKeys) > 0 && !is.null(paired)) {
                        mcn_row <- mcnemar_table$rowKeys[[1]]
                        mcn_stat <- mcnemar_table$getCell(rowKey = mcn_row, col = "stat")$value
                        mcn_df <- mcnemar_table$getCell(rowKey = mcn_row, col = "df")$value
                        mcn_p <- mcnemar_table$getCell(rowKey = mcn_row, col = "p")$value

                        # The verb was spliced through %s ("%s a significant difference");
                        # each outcome now has its own complete sentence, one per McNemar
                        # variant. Copy-ready text carries the paired N (VAL-decisioncompare-05)
                        # and never "p = 0.000".
                        significant <- isTRUE(mcn_p < private$P_THRESHOLD_SIGNIFICANT)
                        name1 <- private$.safeHtmlOutput(test1_name)
                        name2 <- private$.safeHtmlOutput(test2_name)
                        p_text <- private$.formatP(mcn_p)
                        # The same rows as McNemar's table (VAL-decisioncompare-01): no discordant
                        # pairs means p = 1 by convention, not a test result.
                        no_discordant <- (paired$only1 + paired$only2) == 0
                        report <- if (no_discordant) {
                            sprintf(jmvcore::.("No discordant pairs: none of the %1$d paired cases was classified correctly by only one of %2$s and %3$s, so McNemar's test has no information about a difference in accuracy (its p-value is set to 1 by convention, not computed)."),
                                paired$n, name1, name2)
                        } else if (length(mcn_stat) == 0 || is.na(mcn_stat)) {
                            if (significant)
                                sprintf(jmvcore::.("The McNemar mid-p test comparing %1$s and %2$s showed a significant difference in diagnostic accuracy (n = %3$d paired cases; p %4$s)."),
                                    name1, name2, paired$n, p_text)
                            else
                                sprintf(jmvcore::.("The McNemar mid-p test comparing %1$s and %2$s did not detect a significant difference in diagnostic accuracy (n = %3$d paired cases; p %4$s)."),
                                    name1, name2, paired$n, p_text)
                        } else {
                            if (significant)
                                sprintf(jmvcore::.("The asymptotic McNemar test without continuity correction comparing %1$s and %2$s showed a significant difference in diagnostic accuracy (n = %3$d paired cases; \u03C7\u00B2(%4$d) = %5$.2f, p %6$s)."),
                                    name1, name2, paired$n, mcn_df, private$.halfUp(mcn_stat, 2L), p_text)
                            else
                                sprintf(jmvcore::.("The asymptotic McNemar test without continuity correction comparing %1$s and %2$s did not detect a significant difference in diagnostic accuracy (n = %3$d paired cases; \u03C7\u00B2(%4$d) = %5$.2f, p %6$s)."),
                                    name1, name2, paired$n, mcn_df, private$.halfUp(mcn_stat, 2L), p_text)
                        }

                        # The estimate and interval behind the p-value: the paired accuracies and
                        # their difference, the same figures as the Differences table's Accuracy row.
                        acc_diff <- paired$difference
                        # One rule for the difference and both limits (private$.fmtPP): one decimal,
                        # but a nonzero value that would print as 0.0 keeps two significant digits,
                        # and zero never prints as -0.0, so the printed numbers cannot contradict the
                        # sentences below.
                        fmt_pp <- private$.fmtPP
                        report <- paste(report, sprintf(
                            jmvcore::.("On these paired cases accuracy was %1$.1f%% for %2$s and %3$.1f%% for %4$s; the difference, %2$s minus %4$s, was %5$s percentage points (95%% CI %6$s to %7$s; Newcombe's method 10)."),
                            private$.halfUp1(paired$acc1 * 100), name1, private$.halfUp1(paired$acc2 * 100), name2,
                            fmt_pp(acc_diff$diff), fmt_pp(acc_diff$lower), fmt_pp(acc_diff$upper)
                        ))

                        # The p-value and the Newcombe interval are different procedures and can
                        # disagree near p = 0.05; say so instead of letting the paragraph contradict
                        # itself. The unrounded limits decide (0.000198 prints as 0.0).
                        ci_excludes_0 <- isTRUE(acc_diff$lower > 0 || acc_diff$upper < 0)
                        if (!no_discordant && significant && !ci_excludes_0) {
                            report <- paste(report, jmvcore::.("This significant result is borderline: the 95 percent confidence interval for the accuracy difference comes from Newcombe's method 10, a different procedure from McNemar's test, and it includes 0, the value of no difference."))
                        } else if (!no_discordant && !significant && ci_excludes_0) {
                            report <- paste(report, jmvcore::.("The 95 percent confidence interval for the accuracy difference comes from Newcombe's method 10, a different procedure from McNemar's test, and it excludes 0, the value of no difference; the two procedures disagree on these data, so report both rather than either alone."))
                        }

                        if (no_discordant) {
                            report <- paste(report, jmvcore::.("Identical correctness on these cases does not establish that the two tests perform equally; the confidence interval shows how large a difference remains compatible with these data."))
                        } else if (significant) {
                            # The direction of the discordant pairs McNemar tested, not of the
                            # standalone accuracies (VAL-decisioncompare-01).
                            first_better <- paired$only1 > paired$only2
                            report <- paste(report, sprintf(
                                jmvcore::.("%1$s classified significantly more cases correctly than %2$s; McNemar's test compares overall correctness only."),
                                if (first_better) name1 else name2,
                                if (first_better) name2 else name1
                            ))
                        } else {
                            report <- paste(
                                report,
                                jmvcore::.("A non-significant McNemar result does not establish that the two tests perform equally; the test uses only the discordant pairs and may lack power to detect a clinically important difference.")
                            )
                        }

                        # IR-decisioncompare-02: the accuracy verdict belongs to this case mix, and
                        # the prevalence-free comparisons of sensitivity and specificity go beside it.
                        if (!no_discordant && isTRUE(is.finite(paired$prevalence)))
                            report <- paste(report, sprintf(
                                jmvcore::.("Overall accuracy weights sensitivity and specificity by the prevalence of the target condition among these cases (%1$s%%), so this comparison applies to that case mix; under case-control sampling it does not estimate population accuracy."),
                                private$.pctAdaptive(paired$prevalence)))
                        class_sentence <- if (!no_discordant) private$.classDifferenceSentence(1) else ""
                        if (nzchar(class_sentence))
                            report <- paste(report, class_sentence)

                        html <- paste0(html, report)
                    } else {
                        # Same empty-box failure as in .generateSummary(): no McNemar row
                        # means the comparison was never computed, and this panel is
                        # offered as manuscript text, so it must say so rather than
                        # render blank.
                        html <- paste0(html, jmvcore::.("No statistical comparison between these two tests could be computed; the reason is given in the Important Information panel. Report the per-test performance only."))
                    }
                }

                html <- paste0(html, "</div></div>")
                if (nzchar(private$.cochran_method_note))
                    html <- paste0(html, "<p>", private$.cochran_method_note, "</p>")
                self$results$reportSentence$setContent(html)
            },

            # Generate explanations and glossary content
            .generateExplanations = function() {
                html <- "<div style='font-family: Arial, sans-serif; line-height:1.6;'>"

                # Glossary section
                html <- paste0(html, "<h4 style='color:inherit; border-bottom:2px solid #3498db;'> ", jmvcore::.("Statistical Glossary"), "</h4>")
                html <- paste0(html, "<dl style='margin-left:15px;'>")

                html <- paste0(
                    html,
                    jmvcore::.("<dt><b>McNemar's Test</b></dt>"),
                    paste0("<dd style='margin-bottom:12px;'>", jmvcore::.("<strong>Compares diagnostic CORRECTNESS</strong> of two tests relative to the gold standard. Focuses on <i>discordant pairs</i> (cases where one test is correct and the other is wrong). Tests whether Test A correctly classifies significantly more/fewer cases than Test B. Appropriate for paired/matched data only. P < 0.05 indicates tests differ significantly in accuracy."), "</dd>")
                )

                html <- paste0(
                    html,
                    jmvcore::.("<dt><b>Cochran's Q Test</b></dt>"),
                    paste0("<dd style='margin-bottom:12px;'>", jmvcore::.("Extension of McNemar's for 3+ tests. <strong>Tests whether diagnostic correctness differs</strong> among tests relative to the gold standard. If significant (p < 0.05), at least one test is significantly more/less accurate than the others. Proceed to pairwise comparisons with multiple comparison correction to identify which tests differ."), "</dd>")
                )

                html <- paste0(
                    html,
                    jmvcore::.("<dt><b>Holm-Bonferroni Correction</b></dt>"),
                    paste0("<dd style='margin-bottom:12px;'>", jmvcore::.("Adjusts p-values when making multiple comparisons to control Type I error (false positives). More powerful than standard Bonferroni correction. Applied to overall-accuracy pairwise p-values when more than one pair has discordant observations. Unavailable pairs and pairs with no discordance are excluded from that adjustment; sensitivity and specificity p-values remain unadjusted."), "</dd>")
                )

                html <- paste0(
                    html,
                    jmvcore::.("<dt><b>Discordant Pairs</b></dt>"),
                    paste0("<dd style='margin-bottom:12px;'>", jmvcore::.("<strong>Cases where Test A and Test B have different CORRECTNESS</strong> relative to the reference standard (e.g., Test A correct but Test B wrong, or vice versa). McNemar's test examines whether the imbalance between these discordant types is statistically significant. The p-value comes from the McNemar mid-p test or, if chosen, the asymptotic test without continuity correction, the two versions recommended by Fagerland et al. (2013); precision and power still depend on the number of discordant pairs."), "</dd>")
                )

                html <- paste0(
                    html,
                    jmvcore::.("<dt><b>Sensitivity</b></dt>"),
                    paste0("<dd style='margin-bottom:12px;'>", jmvcore::.("Proportion of diseased cases correctly identified (True Positive Rate). High sensitivity means few false negatives. Important when missing disease is costly."), "</dd>")
                )

                html <- paste0(
                    html,
                    jmvcore::.("<dt><b>Specificity</b></dt>"),
                    paste0("<dd style='margin-bottom:12px;'>", jmvcore::.("Proportion of non-diseased cases correctly identified (True Negative Rate). High specificity means few false positives. Important when false alarms are problematic."), "</dd>")
                )

                html <- paste0(
                    html,
                    jmvcore::.("<dt><b>PPV (Positive Predictive Value)</b></dt>"),
                    paste0("<dd style='margin-bottom:12px;'>", jmvcore::.("Probability that a positive test result truly indicates disease. Depends on disease prevalence - higher in populations with higher disease rates."), "</dd>")
                )

                html <- paste0(
                    html,
                    jmvcore::.("<dt><b>NPV (Negative Predictive Value)</b></dt>"),
                    paste0("<dd style='margin-bottom:12px;'>", jmvcore::.("Probability that a negative test result truly indicates absence of disease. Also depends on prevalence."), "</dd>")
                )

                html <- paste0(html, "</dl>")

                # Assumptions section
                html <- paste0(html, "<h4 style='color:inherit; border-bottom:2px solid #3498db; margin-top:25px;'> ", jmvcore::.("Assumptions & Requirements"), "</h4>")
                html <- paste0(html, "<ul style='margin-left:15px;'>")
                html <- paste0(
                    html,
                    jmvcore::.("<li><b>Paired Data:</b> All tests must be performed on the same patients/samples (not independent groups)</li>"),
                    jmvcore::.("<li><b>Independent Cases:</b> Tests are paired within each case, but cases must be independent across rows. Multiple specimens or lesions from the same patient require a method that accounts for clustering; this analysis does not adjust for repeated observations.</li>"),
                    jmvcore::.("<li><b>Adequate Information:</b> Precision and power depend strongly on the number of discordant pairs</li>"),
                    jmvcore::.("<li><b>Explicit Binary Mapping:</b> Select distinct positive and negative levels for the reference and every test. Other observed levels require explicit exclusion or prior recoding; they never become negative automatically. Estimates after exclusion are conditional on determinate pairs.</li>"),
                    jmvcore::.("<li><b>Gold Standard:</b> Reference test must represent true disease status (e.g., biopsy, final diagnosis)</li>"),
                    jmvcore::.("<li><b>Available Cases:</b> Standalone metrics use each test's observed reference/test pairs; paired comparisons use common determinate rows</li>")
                )
                html <- paste0(html, "</ul>")

                # When to use section
                html <- paste0(html, "<h4 style='color:inherit; border-bottom:2px solid #3498db; margin-top:25px;'> ", jmvcore::.("When to Use This Analysis"), "</h4>")
                html <- paste0(html, "<ul style='margin-left:15px;'>")
                html <- paste0(
                    html,
                    jmvcore::.("<li>Comparing diagnostic accuracy of 2-3 tests performed on same patients</li>"),
                    jmvcore::.("<li>Evaluating if a new test is significantly better/worse than standard test</li>"),
                    jmvcore::.("<li>Optimizing test selection based on performance metrics</li>"),
                    jmvcore::.("<li>Validating diagnostic tools in clinical or pathology practice</li>")
                )
                html <- paste0(html, "</ul>")

                # Limitations section
                html <- paste0(html, "<h4 style='color:inherit; border-bottom:2px solid #e74c3c; margin-top:25px;'> ", jmvcore::.("Limitations"), "</h4>")
                html <- paste0(html, "<ul style='margin-left:15px; color:inherit;'>")
                html <- paste0(
                    html,
                    jmvcore::.("<li>The Statistical Comparison of Test Accuracy table compares overall accuracy. The Differences with 95% Confidence Intervals table also reports separate McNemar tests for sensitivity among reference-positive cases and specificity among reference-negative cases; these class-specific p-values are not adjusted for multiplicity.</li>"),
                    jmvcore::.("<li>Requires paired observations - cannot compare tests performed on different patient groups</li>"),
                    jmvcore::.("<li>This function cannot handle continuous test results directly for statistical comparison. For continuous diagnostic tests, please use the dedicated ROC (Receiver Operating Characteristic) analysis functions for comparing curves (e.g., DeLong's test).</li>"),
                    jmvcore::.("<li>Small number of discordant pairs reduces statistical power and reliability</li>"),
                    jmvcore::.("<li>P-values do not indicate clinical importance - consider effect sizes and practical implications</li>")
                )
                html <- paste0(html, "</ul>")

                html <- paste0(html, "</div>")

                self$results$explanationsContent$setContent(html)
            }
        )
    )
}
