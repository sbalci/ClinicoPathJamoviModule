#' @title Benford's Law Analysis
#' @description This function performs a Benford's Law analysis on a numeric variable to detect unusual digit patterns that may indicate data quality issues.
#' It returns the Benford's Law distribution and a list of potential suspects with clinical interpretation.
#' @details The Benford's Law analysis is a statistical test to determine if the distribution of the first digits of a numeric variable follows Benford's Law.
#' This is used in clinical research as a screen for systematic recording patterns such as rounding, preferred values, or truncation.
#' The analysis returns structured results with clinical interpretation and actionable guidance.
#' @importFrom benford.analysis benford getSuspects
#' @importFrom glue glue
#' @importFrom jmvcore toNumeric
#' @importFrom graphics par
#' @importFrom utils capture.output
#'
#'
#' @returns A comprehensive Benford's Law analysis with clinical interpretation.
#' @keywords internal
#'

# NOTE on the utils tag above - keep it, and keep it INSIDE the roxygen block.
# capture.output is called bare in .generateEnhancedSuspectsOutput(). It resolves
# at run time because utils is attached in every R session, but a bare call
# contributes nothing to NAMESPACE, and each submodule's DESCRIPTION Imports are
# synced FROM its own NAMESPACE - so ClinicoPathDescriptives, the module that
# actually ships this analysis, had no entry for it. Same reasoning as the
# graphics tag.
#
# There is deliberately NO tag for format.pval, despite it being called bare
# alongside capture.output. It lives in BASE, not stats: getNamespaceExports(
# "stats") does not contain it and stats::format.pval errors outright. base is
# always on the search path and never needs an import, so R CMD check does not
# flag it. An `@importFrom stats format.pval` is not merely redundant here, it
# is wrong, and roxygen refuses it with "Excluding unknown export from stats"
# while leaving NAMESPACE untouched - which reads exactly like the tag having
# been applied. Verify an added import by `git diff NAMESPACE`, not by grepping
# for the symbol: another file may already import it.

benfordClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "benfordClass",
    inherit = benfordBase,
    private = list(

        # Notice collection helpers. A single Preformatted (plain-text) output
        # item: avoids BOTH the jmvcore::Notice serialization error from
        # self$results$insert(999, Notice) AND any HTML in notices (project
        # convention: notice content is plain text). ====
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            duplicate <- vapply(private$.noticeList, function(notice) {
                identical(notice$type, type) &&
                    identical(notice$title, title) &&
                    identical(notice$content, content)
            }, logical(1))
            if (any(duplicate))
                return()

            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content)
            # Render immediately so early-return validation aborts still display
            private$.renderNotices()
        },

        .renderNotices = function() {
            # Visibility is set on BOTH branches from this one place, so an
            # empty "Important Information" heading never sits over a clean run
            # and a notice is never written to an item that has been hidden.
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                self$results$notices$setVisible(FALSE)
                return()
            }
            self$results$notices$setVisible(TRUE)
            # Most severe first. Notices are appended in whatever order the code
            # reaches them, so an analysis-stopping ERROR could sit below an
            # advisory WARNING: a variable holding Inf values AND fewer than 30
            # valid observations adds the non-finite WARNING first and the stop
            # ERROR second. severity_rank is a local DISPLAY rank keyed by the
            # type NAME - deliberately NOT jmvcore::NoticeType's integer codes,
            # and nothing maps an integer back to a name, so the off-by-one that
            # has bitten other files here cannot reappear. The seq_along key
            # sorts stably, preserving insertion order within one level.
            severity_rank <- c(ERROR = 1L, WARNING = 2L, INFO = 3L)
            ranks <- severity_rank[
                vapply(private$.noticeList, function(n) n$type, character(1))]
            ordered_notices <- private$.noticeList[
                order(ranks, seq_along(ranks), na.last = TRUE)]
            blocks <- vapply(ordered_notices, function(notice) {
                # ERROR / WARNING / INFO are the three levels this analysis
                # actually uses and each renders distinctly. STRONG_WARNING was
                # retired rather than given a louder prefix: it rendered as the
                # literal "WARNING: " too, so the level was invisible, and its two
                # call sites say the METHOD DOES NOT APPLY (short range, small
                # sample) rather than that the data are bad. This module reports
                # what it measured and does not grade, so a louder prefix on those
                # two would have been the wrong register.
                # The switch KEYS are internal type codes and stay English; the
                # values are printed. ": " is composed outside .() so no
                # translatable string carries trailing punctuation.
                prefix <- switch(notice$type,
                    ERROR   = .("ERROR"),
                    WARNING = .("WARNING"),
                    INFO    = .("NOTE"),
                    "")
                if (nzchar(prefix))
                    prefix <- paste0(prefix, ": ")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))
            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        # No p-value is exactly zero, so never print one. "%.4f" renders any p
        # below 5e-5 as the literal "0.0000", which is a misstatement of the
        # result - the Preformatted output in this same file already avoids it
        # with format.pval(eps = 0.0001).
        .fmtP = function(p) {
            if (length(p) != 1 || !is.finite(p)) return("NA")
            if (p < 0.0001) return("< 0.0001")
            sprintf("%.4f", p)
        },

        .validate = function() {
            if (is.null(self$options$var)) {
                return(FALSE)
            }

            if (nrow(self$data) == 0) {
                html <- paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'> No Data Available</h4>",
                    "<p>", .("Data contains no (complete) rows."), "</p>",
                    "<p>", .("Please check your data for missing values or filtering issues."), "</p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                self$results$dataWarning$setVisible(TRUE)
                private$.addNotice("ERROR", .("Analysis stopped: no rows to analyze"),
                    .("The dataset passed to this analysis contains zero rows, so no leading digits could be extracted and nothing below was computed. Check whether a row filter is excluding every case, or whether the selected variable is missing for all rows."))
                return(FALSE)
            }

            # Enhanced validation with clinical guidance
            var_data <- jmvcore::toNumeric(self$data[[self$options$var]])

            # Non-finite values (Inf, -Inf, NaN) must be removed BEFORE any of
            # the screens below. Inf passes `any(non_na_data <= 0)`, and it makes
            # log10(max/min) infinite so the order-of-magnitude gate passes too.
            # It then reaches benford.analysis::benford(), which gives it its own
            # digit bin: the distribution vector becomes length 91 instead of 90
            # and every observed-minus-expected difference is silently recycled,
            # corrupting the MAD, the chi-square statistic and the whole digit
            # distribution (verified: one Inf among 200 values -> nrow(bfd) = 91,
            # five recycling warnings). R warnings never reach the jamovi results
            # pane, so nothing would tell the user.
            n_nonfinite <- sum(!is.na(var_data) & !is.finite(var_data))
            var_data[!is.finite(var_data)] <- NA
            if (n_nonfinite > 0)
                private$.addNotice("WARNING", .("Non-finite values excluded"),
                    jmvcore::format(.("{n} value(s) in the selected variable are infinite or not a number (Inf, -Inf or NaN). A leading digit is undefined for these, so they were excluded from the analysis and from every count reported below. Values like these arise routinely from computed variables, for example a ratio with a zero denominator; check how the variable was derived."),
                            n = n_nonfinite))

            valid_count <- sum(!is.na(var_data))

            # Check minimum sample size
            if (valid_count < 30) {
                html <- paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'> Insufficient Data</h4>",
                    "<p>", .("Benford's Law analysis requires at least <strong>30 valid observations</strong> for meaningful results."), "</p>",
                    "<p><strong>", .("Current data:"), "</strong> ", valid_count, " ", .("valid observations"), "</p>",
                    "<hr style='border-color: #dc3545;'>",
                    "<p><strong>", .("Recommendations:"), "</strong></p>",
                    "<ol style='margin-left: 20px;'>",
                    "<li>", .("Combine data from multiple sources or time periods"), "</li>",
                    "<li>", .("Use a different variable with more observations"), "</li>",
                    "<li>", .("Consider alternative data quality checks for small samples"), "</li>",
                    "</ol>",
                    "<p style='margin-top: 10px;'><em>", .("Note: Ideally, 100-1000+ observations are recommended for reliable Benford's Law analysis."), "</em></p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                self$results$dataWarning$setVisible(TRUE)
                private$.addNotice("ERROR", .("Analysis stopped: too few valid observations"),
                    jmvcore::format(.("Only {n} valid observations are available; a leading-digit distribution needs at least 30 (and 100 or more before the tests carry useful power), so nothing below was computed. Select a variable with more recorded values, or pool comparable measurements before running this analysis."),
                            n = valid_count))
                return(FALSE)
            }

            # Check for positive values only (Benford's Law requirement)
            non_na_data <- var_data[!is.na(var_data)]
            if (any(non_na_data <= 0)) {
                zero_count <- sum(non_na_data == 0)
                negative_count <- sum(non_na_data < 0)

                html <- paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'> Invalid Values Detected</h4>",
                    "<p><strong>", .("Benford's Law only applies to positive numbers."), "</strong></p>",
                    "<p>", .("Your data contains:"), "</p>",
                    "<ul style='margin-left: 20px;'>",
                    if (zero_count > 0) paste0("<li>", zero_count, " ", .("zero values"), "</li>") else "",
                    if (negative_count > 0) paste0("<li>", negative_count, " ", .("negative values"), "</li>") else "",
                    "</ul>",
                    "<hr style='border-color: #dc3545;'>",
                    "<p><strong>", .("Solutions:"), "</strong></p>",
                    "<ol style='margin-left: 20px;'>",
                    "<li><strong>", .("Filter data:"), "</strong> ", .("Remove or exclude zero/negative values before analysis"), "</li>",
                    "<li><strong>", .("Transform data:"), "</strong> ", .("If analyzing deltas/changes, use absolute values or analyze increases separately from decreases"), "</li>",
                    "<li><strong>", .("Select different variable:"), "</strong> ", .("Choose a naturally positive variable (e.g., lab values, measurements, counts)"), "</li>",
                    "</ol>",
                    "<p style='margin-top: 10px;'><em>", .("Example valid data: lab test values, patient ages, tumor sizes, cell counts"), "</em></p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                self$results$dataWarning$setVisible(TRUE)
                private$.addNotice("ERROR", .("Analysis stopped: non-positive values present"),
                    jmvcore::format(.("The selected variable contains {zeros} zero and {negatives} negative values. A leading digit is undefined for these, so nothing below was computed. Filter them out, analyse increases and decreases separately if the variable is a change score, or choose a naturally positive measurement."),
                            zeros = zero_count, negatives = negative_count))
                return(FALSE)
            }

            # Check for multiple orders of magnitude (Benford's Law requirement)
            #
            # The gate used to be `< 1`, which tests the wrong boundary. Data
            # drawn as 10^U over an INTEGER number of decades is exactly Benford
            # by construction; the regime that manufactures false alarms is a
            # NON-INTEGER span between 1 and 2 decades, which the old gate let
            # through in silence. Measured on exactly-Benford 10^U data, n = 250,
            # 2 digits, 250 reps, chi-square rejection rate at a nominal 0.05:
            # span 0.8 -> 0.87, 1.0 -> 0.05, 1.3 -> 0.38, 1.5 -> 0.17, 1.7 ->
            # 0.08, 2.0 -> 0.06, 2.5 -> 0.05, 3.5 -> 0.02. The inflation is
            # confined to spans below about 2 decades, so that is the threshold.
            # Nigrini (2012) and Durtschi et al. (2004) likewise require several
            # orders of magnitude.
            min_val <- min(non_na_data, na.rm = TRUE)
            max_val <- max(non_na_data, na.rm = TRUE)
            magnitude_range <- log10(max_val / min_val)

            if (magnitude_range < 2) {
                html <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'> Limited Data Range</h4>",
                    "<p><strong>", .("Warning: Data spans less than two orders of magnitude."), "</strong></p>",
                    "<p>", .("Benford's Law works best when data spans multiple orders of magnitude (e.g., values ranging from 10 to 1000+)."), "</p>",
                    # One sentence, one .(). The range used to be assembled from
                    # the fragments .("to") and .("orders of magnitude") glued
                    # around bare numbers, which gives a translator three
                    # unconnected pieces in fixed English word order.
                    # The decade count is always rendered with two decimals: a
                    # span of exactly one decade printed as the bare "1" read
                    # "1 orders of magnitude". %.2f also matches the wording the
                    # Assessment row already uses for the same quantity.
                    "<p><strong>",
                    jmvcore::format(.("Your data range: {min} to {max} ({decades} orders of magnitude)"),
                                    min = base::format(round(min_val, 2)),
                                    max = base::format(round(max_val, 2)),
                                    decades = sprintf("%.2f", magnitude_range)),
                    "</strong></p>",
                    "<hr style='border-color: #ffc107;'>",
                    "<p><strong>", .("This analysis may not be meaningful because:"), "</strong></p>",
                    "<ul style='margin-left: 20px;'>",
                    "<li>", .("Over a narrow range the leading-digit frequencies are set by where the range starts and stops, not by Benford's Law"), "</li>",
                    "<li>", .("Departures reported below can therefore be large without indicating anything about how the data were recorded"), "</li>",
                    "<li>", .("Clinical interpretation will be unreliable"), "</li>",
                    "</ul>",
                    "<p><strong>", .("Recommendations:"), "</strong></p>",
                    "<ol style='margin-left: 20px;'>",
                    "<li>", .("Use variables that naturally vary widely (e.g., protein levels, gene expression, population counts)"), "</li>",
                    "<li>", .("Combine data across different scales or contexts"), "</li>",
                    "<li>", .("Consider alternative data quality checks for narrow-range data"), "</li>",
                    "</ol>",
                    "<p style='margin-top: 10px;'><em>", .("Proceeding with analysis, but results should be interpreted with caution."), "</em></p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                self$results$dataWarning$setVisible(TRUE)
                private$.addNotice("WARNING", .("Data span less than two orders of magnitude"),
                    jmvcore::format(.("Values run from {min} to {max}, a range of {decades} orders of magnitude. Benford's Law describes data spanning several orders of magnitude; between one and two decades the leading-digit frequencies are dominated by where the range starts and stops rather than by Benford's Law, so the tests below are not calibrated and can report a large departure for data that were recorded perfectly (on simulated conforming data spanning 1.3 decades the chi-square test rejected in 38 percent of runs at a nominal 5 percent). The statistics are shown for completeness, but the leading-digit finding is reported as \u{201C}Not assessable\u{201D} for this variable rather than being interpreted."),
                            min = base::format(round(min_val, 2)),
                            max = base::format(round(max_val, 2)),
                            decades = sprintf("%.2f", magnitude_range)))
                # Continue with analysis but user is warned
            } else {
                # Clear warnings if validation passes. Hiding the item as well
                # keeps an empty "Data Validation" heading off a clean run.
                self$results$dataWarning$setContent("")
                self$results$dataWarning$setVisible(FALSE)
            }

            return(TRUE)
        },

        # Expected MAD when the data ARE Benford, i.e. the deviation produced by
        # sampling noise alone. Each digit bin count is Binomial(n, p_i), and for
        # a binomial proportion E|p_hat - p| ~= sqrt(2 p (1 - p) / (pi n)), so the
        # mean absolute deviation across bins has a closed form. Checked against
        # simulation on exactly-Benford data (10^U): analytic vs simulated MAD
        # agree to ~1% for n >= 1000 at 1, 2 and 3 digits.
        .expectedMadUnderNull = function(n, digits) {
            if (!is.finite(n) || n <= 0) return(NA_real_)
            lo <- 10^(digits - 1)
            hi <- 10^digits - 1
            p <- log10(1 + 1 / (lo:hi))
            mean(sqrt(2 * p * (1 - p) / (pi * n)))
        },

        # Nigrini's MAD cut-offs, as applied by benford.analysis::MAD.conformity.
        .madNonconformityCutoff = function(digits) {
            switch(as.character(digits), "1" = 0.015, "2" = 0.0022, "3" = 0.0005,
                   NA_real_)
        },

        # Smallest n at which the nonconformity cut-off clears the noise floor by
        # the factor of 2 that .madLabelIsReliable() requires. Solving
        # mean(sqrt(2 p (1-p) / (pi n))) = cutoff / 2 for n gives a closed form,
        # since the only n-dependence is the 1/sqrt(n) factor.
        .minNForMadLabel = function(digits) {
            cutoff <- private$.madNonconformityCutoff(digits)
            if (is.na(cutoff)) return(NA_real_)
            # .expectedMadUnderNull(1, digits) is the constant multiplying 1/sqrt(n)
            k <- private$.expectedMadUnderNull(1, digits)
            (2 * k / cutoff)^2
        },

        # Is the MAD conformity LABEL meaningful at this sample size?
        #
        # Nigrini's cut-offs were derived for large accounting populations, and MAD
        # is strongly biased upward at small n: across 90 first-two-digit bins the
        # noise floor alone is 0.0079 at n = 100 and 0.0025 at n = 1000, against a
        # "Nonconformity" cut-off of 0.0022. So with the default 2-digit setting,
        # data that is EXACTLY Benford is labelled "Nonconformity" in 20/20
        # simulated runs at n = 100, 300 and 1000 - and the analysis then told the
        # user their data showed "potential manipulation" and required "IMMEDIATE
        # REVIEW". The label only becomes informative once the cut-off clears the
        # noise floor, and it has to clear it with room to spare: the comparison
        # below is against the MEAN noise MAD, but MAD has spread around that
        # mean, and three lower cut-offs (Close / Acceptable / Marginally
        # acceptable) feed the verdict as well as the top one. A bare
        # `cutoff > floor` let the whole ladder fire on noise just under the
        # boundary. Measured on exactly-Benford 10^U data, share of runs given a
        # verdict above "Low", 200 reps: 1 digit n = 300 -> 64% (30% of them the
        # full "High" / "does NOT conform ... or manipulation" text), n = 500 ->
        # 25%; 2 digits n = 2000 -> 40%. Requiring the cut-off to clear twice the
        # noise floor - the same "twice the noise floor" criterion this file
        # already applies to mad_ratio - puts every one of those cells back on the
        # 2-5% baseline the chi-square branch produces. Thresholds are therefore
        # n > 981 (1 digit), n > 5204 (2 digits), n > 10200 (3 digits).
        #
        # The chi-square test does not have this problem: it is not compared
        # against fixed cut-offs, so below the MAD threshold the verdict is based
        # on it instead of on the MAD label. It is NOT, however, exact at every n.
        # It relies on a large-sample approximation that needs adequate expected
        # cell counts, and with 90 or 900 digit bins those counts are small.
        # Measured on exactly-Benford 10^U data, rejection rate at a nominal 5%:
        # 2 digits n = 150 -> 7.7%, n = 300 -> 5.5%, n = 1000 -> 4.3%;
        # 3 digits n = 150 -> 13.9%, n = 300 -> 8.4%, n = 1200 -> 6.5%,
        # n = 5000 -> 4.1%. .interpretResults() discloses this whenever the
        # smallest expected cell count falls below 1.
        .madLabelIsReliable = function(n, digits) {
            cutoff <- private$.madNonconformityCutoff(digits)
            floor_mad <- private$.expectedMadUnderNull(n, digits)
            if (is.na(cutoff) || is.na(floor_mad)) return(FALSE)
            cutoff > 2 * floor_mad
        },

        .interpretResults = function(benford_obj, suspects_obj, var_data) {
            # Count only finite, positive observations so the reported N matches
            # the analyzed set (var_cleaned uses the same is.finite & > 0 filter);
            # this prevents Inf slipping past validation from inflating N.
            n_total <- length(var_data[!is.na(var_data) & is.finite(var_data) & var_data > 0])
            n_suspects <- if (!is.null(suspects_obj) && nrow(suspects_obj) > 0) nrow(suspects_obj) else 0
            suspicion_rate <- round((n_suspects / n_total) * 100, 1)

            # Extract ACTUAL statistical evidence from benford object
            # These are scientifically validated measures, not arbitrary thresholds
            mad_value <- benford_obj$MAD
            mad_conformity <- as.character(benford_obj$MAD.conformity)

            # Extract chi-square test results
            chisq_stat <- benford_obj$stats$chisq$statistic
            chisq_df <- benford_obj$stats$chisq$parameter
            chisq_pvalue <- benford_obj$stats$chisq$p.value

            # Extract Mantissa Arc Test results
            mat_stat <- benford_obj$stats$mantissa.arc.test$statistic
            mat_df <- benford_obj$stats$mantissa.arc.test$parameter
            mat_pvalue <- benford_obj$stats$mantissa.arc.test$p.value

            # CRITICAL: Add guardrails for small samples
            # Benford's Law requires adequate sample size for statistical validity
            n_digits <- benford_obj$info$number.of.digits
            n_used <- benford_obj$info$n
            mad_floor <- private$.expectedMadUnderNull(n_total, n_digits)
            mad_label_reliable <- private$.madLabelIsReliable(n_total, n_digits)

            # Range is a PRECONDITION of the method, not a caveat on a result.
            # Over less than two decades the leading-digit frequencies are set by
            # where the range starts and stops, so a departure from Benford's Law
            # is expected arithmetically and says nothing about how the values
            # were recorded. Measured on ordinary, correctly-recorded clinical
            # variables at N=400: platelet counts (1.14 decades) and serum
            # creatinine (1.61) both produced the largest departure this analysis
            # can report. .validate() already warns; this makes the finding row
            # agree with that warning instead of contradicting it.
            positive_vals <- var_data[!is.na(var_data) & is.finite(var_data) & var_data > 0]
            magnitude_range <- if (length(positive_vals) > 0)
                log10(max(positive_vals) / min(positive_vals)) else NA_real_
            range_ok <- is.finite(magnitude_range) && magnitude_range >= 2

            # Pearson's chi-square is a large-sample approximation and needs
            # adequate expected cell counts. Here the smallest expected count is
            # n * log10(1 + 1/(10^d - 1)), which is far below 1 at clinical
            # sample sizes: 0.13 at n = 300 with 3 digits. Measured rejection
            # rates on exactly-Benford data are in the comment on
            # .madLabelIsReliable(); they return to nominal once the smallest
            # expected count clears about 1 (2 digits: n >= 764; 3 digits:
            # n >= 2304). Disclose the sparsity in that regime rather than
            # letting the p-value be read as exact - it is the sole basis of the
            # verdict whenever the MAD label is unusable.
            expected_counts <- as.numeric(n_used * benford_obj$bfd$benford.dist)
            min_expected <- if (length(expected_counts) > 0) min(expected_counts) else NA_real_
            if (is.finite(min_expected) && min_expected < 1)
                private$.addNotice("WARNING", .("Chi-square p-value is approximate at this sample size"),
                    jmvcore::format(.("The {digits}-digit analysis spreads {n} observations over {bins} digit bins. The smallest expected count is {minexp} and {sparse} percent of the bins have an expected count below 5, so the chi-square approximation is only approximate here and rejects more often than its nominal rate: on simulated conforming data the rejection rate in this regime ran between 6 and 14 percent against a nominal 5 percent, worst at the 3-digit setting. Read the p-value as indicative rather than exact. A 1-digit analysis uses 9 bins and puts far more observations in each."),
                            digits = n_digits, n = n_used,
                            bins = length(expected_counts),
                            minexp = sprintf("%.2f", min_expected),
                            sparse = sprintf("%.0f", 100 * mean(expected_counts < 5))))

            if (!range_ok) {
                finding <- .("Not assessable")
                clinical_interpretation <- jmvcore::format(
                    .("The values span {decades} orders of magnitude. Benford's Law describes data spanning several orders of magnitude, so below two decades the leading-digit frequencies are determined by where the range starts and stops rather than by how the values were recorded. The measured MAD of {mad} and chi-square p of {p} are reported above, but for this variable they carry no information about recording quality and are not interpreted here."),
                    decades = sprintf("%.2f", magnitude_range),
                    mad = sprintf("%.4f", mad_value), p = private$.fmtP(chisq_pvalue))
                considerations <- .("Leading-digit analysis is not informative for a variable with this range. Range, duplicate, precision and missingness checks address recording quality directly and do not depend on the data spanning multiple orders of magnitude.")

            } else if (n_total < 100) {
                private$.addNotice("WARNING", .("Sample small: only a very large departure would be detected"),
                    jmvcore::format(.("Only {n} observations were analysed. The digit-frequency tests below are computed, but at this size they resolve only very large departures, and the MAD conformity label is biased upward by sampling noise. Neither a departure nor its absence is established at this sample size."),
                            n = n_total))
                clinical_interpretation <- jmvcore::format(
                    .("Only {n} observations were analysed. At this size the digit-frequency tests detect only very large departures, so neither a departure nor its absence is established here. The statistics above are reported for completeness."),
                    n = n_total)
                considerations <- .("A larger sample, typically several hundred observations or more, is needed before a leading-digit result carries much weight either way.")
                finding <- .("Limited evidence")

            } else if (!is.na(mad_conformity) && !mad_label_reliable) {
                private$.addNotice("INFO", .("MAD conformity label not usable at this sample size"),
                    jmvcore::format(.("Nigrini's MAD cut-off for {digits}-digit analysis is {cutoff}, but sampling noise alone produces a MAD of about {floor} at N={n}, so the label '{label}' cannot separate a real departure from noise. It needs N above {needed}. The assessment below is taken instead from the chi-square goodness-of-fit test, together with the size of the MAD relative to that noise level."),
                            digits = n_digits,
                            cutoff = base::format(private$.madNonconformityCutoff(n_digits)),
                            floor = base::format(signif(mad_floor, 3)),
                            n = n_total, label = mad_conformity,
                            needed = ceiling(private$.minNForMadLabel(n_digits))))
                # The MAD cut-offs cannot separate signal from sampling noise at
                # this n and digit setting, so the conformity label is reported but
                # NOT converted into a data-integrity verdict. The chi-square test
                # is not tied to fixed cut-offs, so the verdict comes from it
                # instead - together with the sparse-expected-count notice above,
                # which says when its p-value is only approximate.
                if (!is.na(chisq_pvalue) && chisq_pvalue < 0.05) {
                    # Significance alone must not set the severity: at large n a
                    # trivial departure is still significant. The chi-square test
                    # establishes THAT there is a departure; the size of the MAD
                    # relative to the noise floor establishes whether it is large
                    # enough to be worth acting on. A MAD of twice the noise floor
                    # is the point at which the deviation is unambiguously beyond
                    # what sampling explains, and that ratio is reported so the
                    # basis for the verdict is visible rather than implied.
                    mad_ratio <- mad_value / mad_floor
                    clinical_interpretation <- jmvcore::format(
                        .("Chi-square goodness-of-fit test indicates a departure from Benford's Law (p={p}). MAD = {mad}, which is {ratio} times the deviation expected from sampling noise alone at N={n} with {digits}-digit analysis ({floor}). The '{label}' label is not informative at this sample size, so this conclusion rests on the chi-square test and the size of the deviation relative to that noise level."),
                        p = private$.fmtP(chisq_pvalue),
                        mad = sprintf("%.4f", mad_value),
                        ratio = sprintf("%.1f", mad_ratio),
                        n = n_total, digits = n_digits,
                        floor = sprintf("%.4f", mad_floor), label = mad_conformity
                    )
                    finding <- .("Departure detected")
                    if (mad_ratio >= 2) {
                        considerations <- .("The deviation is larger than sampling noise accounts for. Leading-digit departures arise from systematic rounding, preferred or repeated values, a subset of records entered differently, and other recording patterns; this test does not distinguish among them. The leading-digit bin listing below shows which digit combinations carry the deviation.")
                    } else {
                        considerations <- jmvcore::format(
                            .("The deviation is detectable but modest relative to sampling noise. The leading-digit bin listing below shows which digit combinations carry it. Collecting at least {needed} observations would additionally make the MAD conformity classification usable."),
                            needed = ceiling(private$.minNForMadLabel(n_digits)))
                    }
                } else {
                    clinical_interpretation <- jmvcore::format(
                        .("No evidence of departure from Benford's Law (chi-square p={p}). MAD = {mad}, which is within the range expected from sampling noise alone at N={n} with {digits}-digit analysis (about {floor}), so the '{label}' label is not informative at this sample size."),
                        p = private$.fmtP(chisq_pvalue),
                        mad = sprintf("%.4f", mad_value), n = n_total,
                        digits = n_digits, floor = sprintf("%.4f", mad_floor),
                        label = mad_conformity
                    )
                    considerations <- jmvcore::format(
                        .("No departure was detected. That is not evidence the data are free of errors: this test examines only leading-digit frequencies. To additionally use the MAD conformity classification, {needed} observations are needed for {digits}-digit analysis, or switch to 1-digit analysis, which needs far fewer."),
                        needed = ceiling(private$.minNForMadLabel(n_digits)),
                        digits = n_digits)
                    finding <- .("No departure detected")
                }

            } else if (is.na(mad_conformity)) {
                # benford.analysis returns MAD.conformity = NA when
                # number.of.digits > 3. digits is capped at 3 in benford.a.yaml,
                # but guard defensively so an NA conformity reports MAD
                # numerically instead of crashing on if(NA || NA).
                clinical_interpretation <- jmvcore::format(
                    .("MAD = {mad}. A conformity classification is not available for this digit setting; interpret the MAD and chi-square test (p={p}) directly. Larger MAD values indicate greater deviation from Benford's Law."),
                    mad = sprintf("%.4f", mad_value), p = private$.fmtP(chisq_pvalue)
                )
                considerations <- .("No conformity label is available for this digit configuration. The numeric MAD and the chi-square result above are the evidence; a 1-3 digit setting additionally provides a classified label.")
                finding <- if (!is.na(chisq_pvalue) && chisq_pvalue < 0.05)
                    .("Departure detected") else .("No departure detected")

            } else {
                # Use EVIDENCE-BASED interpretation from the package's MAD
                # conformity classification. NOTE: the illustrative cutoffs below
                # are Nigrini's FIRST-DIGIT thresholds; benford.analysis applies
                # digit-count-specific cutoffs internally (e.g. the 2-digit
                # nonconformity threshold is far tighter). Always trust the
                # package's MAD.conformity label rather than these numbers.
                # Nigrini, M. (2012). Benford's Law: Applications for Forensic Accounting
                #   (1-digit) MAD < 0.006: Close conformity
                #   (1-digit) MAD 0.006-0.012: Acceptable conformity
                #   (1-digit) MAD 0.012-0.015: Marginally acceptable conformity
                #   (1-digit) MAD > 0.015: Nonconformity

                if (mad_conformity == "Close conformity" || mad_conformity == "Acceptable conformity") {
                    # The two measures answer different questions and can point
                    # opposite ways: MAD measures the SIZE of the departure and
                    # is compared against fixed cut-offs, while chi-square tests
                    # whether ANY departure is detectable and gains power with n.
                    # At large n a departure far too small to move the MAD label
                    # is still detectable - which is exactly the signature of a
                    # small systematic artefact such as rounding or preferred
                    # values affecting part of the data. Reporting the conforming
                    # label on its own would issue an all-clear over a departure
                    # the analysis did detect (verified: n=20000, 2 digits, a
                    # rounding artefact in 1/12 of values gives MAD = 0.00083,
                    # "Close conformity", chi-square p = 4e-10).
                    if (!is.na(chisq_pvalue) && chisq_pvalue < 0.05) {
                        mad_ratio <- mad_value / mad_floor
                        clinical_interpretation <- jmvcore::format(
                            .("MAD = {mad} falls in the '{label}' band, but the chi-square goodness-of-fit test detects a departure from Benford's Law (p={p}). The MAD is {ratio} times the deviation expected from sampling noise alone at N={n} with {digits}-digit analysis ({floor}), so the departure is statistically detectable but small in magnitude. The two measures disagree because the MAD cut-offs are fixed while the chi-square test gains power as N grows."),
                            mad = sprintf("%.4f", mad_value), label = mad_conformity,
                            p = private$.fmtP(chisq_pvalue),
                            ratio = sprintf("%.1f", mad_ratio), n = n_total,
                            digits = n_digits, floor = sprintf("%.4f", mad_floor)
                        )
                        considerations <- .("A departure this small relative to the MAD cut-offs, yet detectable by the chi-square test, is the pattern a localized cause produces: systematic rounding, preferred values, or a subset of records entered differently. The leading-digit bin listing below shows which digit combinations carry it.")
                        finding <- .("Departure detected")
                    } else {
                        clinical_interpretation <- jmvcore::format(
                            .("Leading-digit distribution is consistent with Benford's Law (MAD={mad}, {label}; chi-square p={p}). Neither measure detected a departure. Absence of a detected departure is not evidence that the data contain no errors - this analysis examines only leading-digit frequencies."),
                            mad = sprintf("%.4f", mad_value), label = mad_conformity,
                            p = private$.fmtP(chisq_pvalue)
                        )
                        considerations <- .("Neither measure detected a departure. This test does not assess accuracy, completeness, units, or transcription of individual values, so it does not substitute for the usual range, duplicate, and missingness checks.")
                        finding <- .("No departure detected")
                    }

                } else if (mad_conformity == "Marginally acceptable conformity") {
                    clinical_interpretation <- jmvcore::format(
                        .("Data shows marginally acceptable conformity to Benford's Law (MAD={mad}). Chi-square test: p={p}. Consider reviewing data collection procedures."),
                        mad = sprintf("%.4f", mad_value), p = private$.fmtP(chisq_pvalue)
                    )
                    considerations <- .("The deviation sits in Nigrini's marginal band. Leading-digit departures of this size are produced by systematic rounding and by preferred values, among other recording patterns; this test does not distinguish among them.")
                    finding <- .("Departure detected")

                } else {  # "Nonconformity"
                    clinical_interpretation <- jmvcore::format(
                        .("The leading-digit distribution departs from Benford's Law (MAD={mad}, {label}; chi-square p={p}). The deviation exceeds Nigrini's nonconformity cut-off for this digit setting. Benford's Law describes how leading digits are distributed in data spanning several orders of magnitude; a departure indicates the values do not follow that pattern and does not by itself identify a cause."),
                        mad = sprintf("%.4f", mad_value), label = mad_conformity,
                        p = private$.fmtP(chisq_pvalue)
                    )
                    considerations <- .("Recording patterns that produce leading-digit departures include systematic rounding, preferred or repeated values, truncation at a detection limit, and a subset of records entered differently. This test does not distinguish among them, and a departure can also arise where the variable simply does not follow Benford's Law. The leading-digit bin listing below shows which digit combinations carry the deviation.")
                    finding <- .("Departure detected")
                }
            }

            return(list(
                total_observations = n_total,
                suspicious_count = n_suspects,
                suspicion_rate = suspicion_rate,
                # Statistical evidence
                mad_value = mad_value,
                mad_conformity = mad_conformity,
                mad_floor = mad_floor,
                mad_label_reliable = mad_label_reliable,
                n_digits = n_digits,
                n_used = n_used,
                chisq_statistic = chisq_stat,
                chisq_df = chisq_df,
                chisq_pvalue = chisq_pvalue,
                mat_statistic = mat_stat,
                mat_df = mat_df,
                mat_pvalue = mat_pvalue,
                # Clinical interpretation (now evidence-based)
                magnitude_range = magnitude_range,
                range_ok = range_ok,
                clinical_interpretation = clinical_interpretation,
                considerations = considerations,
                finding = finding
            ))
        },
        
        .generateClinicalExplanation = function() {
            explanation <- glue::glue("
            <div style='padding: 15px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; margin-bottom: 20px; color: inherit;'>
                <h4 style='color: #007bff; margin-top: 0;'>{title}</h4>
                <p><strong>{what_title}</strong> {what_text}</p>
                <p><strong>{when_title}</strong> {when_text}</p>
                <p><strong>{tests_title}</strong> {tests_text}</p>
                <p><strong>{interpret_title}</strong> {interpret_text}</p>
                <p><strong>{action_title}</strong> {action_text}</p>
            </div>
            ",
            title = .("Understanding Benford's Law Analysis"),
            what_title = .("What it does:"),
            what_text = .("Compares the distribution of leading digits in your data against Benford's Law, which describes how leading digits are distributed in data spanning several orders of magnitude. A departure indicates the values do not follow that pattern; it does not identify why."),
            when_title = .("When to use:"),
            when_text = .("Use with naturally occurring numerical data (lab values, measurements, counts) that span multiple orders of magnitude. Requires at least 100 observations for reliable results. Not suitable for artificial ranges, assigned IDs, or categorical data."),
            tests_title = .("Statistical tests performed:"),
            tests_text = .("(1) MAD (Mean Absolute Deviation): Primary measure of conformity with validated thresholds. (2) Chi-square goodness-of-fit test: Tests overall distribution fit. (3) Mantissa Arc Test: Tests for subtle distributional anomalies. All tests are from published Benford's Law literature (Nigrini, 2012)."),
            interpret_title = .("How to interpret:"),
            interpret_text = .("Two measures are read together. The MAD (Mean Absolute Deviation) conformity label from the benford.analysis package measures the SIZE of the departure against Nigrini's digit-count-specific cutoffs, and its label is only used once the sample is large enough for those cutoffs to exceed the deviation sampling noise alone produces (about 1000 observations at 1 digit, 5200 at 2, 10200 at 3); below that the summary table says so and the verdict comes from the chi-square test instead. The chi-square goodness-of-fit test asks whether ANY departure is detectable and gains power as N grows, so at large N it can flag a departure too small to move the MAD label - reported here as a detectable but small departure rather than as an all-clear. The flagged-observation count is bin membership, not an outlier count, and is descriptive only."),
            action_title = .("Reading the finding:"),
            action_text = .("The Assessment row reports what the tests found, not how concerned to be - that judgement depends on how the variable was collected and belongs to you. \u{201C}No departure detected\u{201D} means the leading-digit frequencies are consistent with Benford's Law; it does not establish that the data are free of errors, because this test looks only at leading digits. \u{201C}Departure detected\u{201D} means the frequencies differ from Benford's Law by more than sampling noise explains; systematic rounding, preferred or repeated values, truncation at a detection limit, and a subset of records entered differently all produce this, as does a variable that simply does not follow Benford's Law, and the test does not distinguish among them. \u{201C}Not assessable\u{201D} means the data span less than two orders of magnitude, where the method does not apply. \u{201C}Limited evidence\u{201D} means the sample is too small for the tests to resolve anything but a very large departure.")
            )
            return(explanation)
        },
        
        .generateReportSentence = function(interpretation_results, digits) {
            # Format summary based on statistical evidence, not just suspect counts
            if (interpretation_results$total_observations < 100) {
                # jmvcore::format, not glue::glue, for the two templates in this
                # method: both are .() strings, so their contents come from the
                # .po catalog at run time, and glue EVALUATES whatever sits
                # inside {} as R code. A translator's typo is then an eval rather
                # than a substitution. jmvcore::format does plain replacement and
                # renders an unrecognised placeholder as an ellipsis instead of
                # raising, which is also the right failure mode for a results
                # pane. (The HTML skeletons elsewhere in this file stay on glue:
                # those templates are hardcoded English, only their VALUES are
                # translated.)
                #
                # No raw "<" in a string bound for an Html item: the renderer
                # reads it as an opening tag and swallows everything up to the
                # next ">", which silently ate this entire sentence after
                # "observations (N". Same trap as .fmtP's "< 0.0001" below.
                summary_text <- jmvcore::format(
                    .("Benford's Law analysis of {n} observations, fewer than the 100 this analysis treats as a working minimum. At this size the digit-frequency tests detect only very large departures, so this run does not establish either a departure or its absence."),
                    n = interpretation_results$total_observations
                )
            } else {
                summary_text <- jmvcore::format(
                    .("Benford's Law analysis of {n} observations using {d}-digit analysis: MAD = {mad} ({conformity}), chi-square p = {pval}. Finding: {level}."),
                    n = interpretation_results$total_observations,
                    d = digits,
                    mad = sprintf("%.4f", interpretation_results$mad_value),
                    conformity = interpretation_results$mad_conformity,
                    # .fmtP returns the literal "< 0.0001" for a very small p,
                    # and this panel is an Html item: the bare "<" opened what
                    # the renderer read as a tag and swallowed everything up to
                    # the next ">", which is the closing </strong>. The whole
                    # verdict - "< 0.0001. Assessment: High concern for data
                    # quality issues." - disappeared from the Clinical Report,
                    # and only in the high-concern cases where p is smallest.
                    # &lt; is one of the five named entities jamovi renders.
                    pval = gsub("<", "&lt;", private$.fmtP(interpretation_results$chisq_pvalue), fixed = TRUE),
                    level = interpretation_results$finding
                )
            }

            report <- glue::glue("
            <div style='padding: 15px; background-color: rgba(33, 159, 33, 0.1); border: 1px solid #28a745; border-radius: 5px; color: inherit;'>
                <h4 style='color: #28a745; margin-top: 0;'>{title}</h4>
                <p style='font-size: 16px; margin-bottom: 10px;'>
                    <strong>{summary}</strong>
                </p>
                <p style='margin-bottom: 0;'>
                    <em>{recommendation}</em>
                </p>
            </div>
            ",
            title = .("Statistical Summary"),
            summary = summary_text,
            recommendation = interpretation_results$considerations
            )
            return(report)
        },
        
        .init = function() {
            # The summary table always carries the same six statistics, so its
            # row structure is built here rather than in .run(): otherwise the
            # table appears empty and then visibly restructures on every run.
            # .run() fills only the computed cells (setRow).
            statistics <- c(
                .("Sample Size"),
                .("MAD (Mean Absolute Deviation)"),
                .("Chi-square Test"),
                .("Mantissa Arc Test"),
                .("Flagged Observations"),
                .("Assessment"))

            for (i in seq_along(statistics))
                self$results$summary$addRow(rowKey=i, values=list(
                    statistic=statistics[i]))
        },

        # Blank the computed cells of every summary row, leaving the labels that
        # .init() wrote. Driven off the table's OWN rowKeys rather than a
        # hardcoded 1:6, which was repeated at two call sites (the pre-validation
        # reset and the error handler) and would silently have skipped a seventh
        # row added to .init(). Reusing the keys also sidesteps the rowKey
        # type-strictness that makes setRow(1) miss a row added as 1L.
        .blankSummaryRows = function() {
            for (key in self$results$summary$rowKeys)
                self$results$summary$setRow(rowKey=key, values=list(
                    value="", interpretation=""))
        },

        # Show/hide every computed output in one place. With no variable
        # selected the six labelled summary rows, the empty Preformatted and
        # Html sections and the empty plot box would otherwise frame the
        # "Getting Started" panel that is trying to explain what to select.
        # This is option-driven visibility (variable chosen or not), set
        # explicitly on BOTH branches, not a failure signal.
        .setComputedVisible = function(visible) {
            r <- self$results
            for (item in list(r$summary, r$explanation, r$dataWarning, r$todo,
                              r$text, r$text2, r$reportSentence, r$plot))
                item$setVisible(visible)
        },

        .run = function() {
            # Notices are collected in a private field that survives across run
            # cycles, so reset it first or the same notice accumulates N times
            # over N runs.
            private$.noticeList <- list()
            private$.renderNotices()

            # Welcome message when no variable selected
            if (is.null(self$options$var)) {
                welcome_html <- glue::glue("
                <div style='padding: 20px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; margin: 20px 0; color: inherit;'>
                    <h3 style='color: #007bff; margin-top: 0;'>{heading}</h3>
                    <p><strong>{getting_started}</strong></p>
                    <ol style='margin: 10px 0;'>
                        <li>{step1}</li>
                        <li>{step2}</li>
                        <li>{step3}</li>
                    </ol>
                    <p><strong>{best_suited}</strong></p>
                    <ul style='margin: 10px 0;'>
                        <li>{use1}</li>
                        <li>{use2}</li>
                        <li>{use3}</li>
                    </ul>
                    <p style='margin-top: 15px;'><strong>{note_title}</strong> {note_text}</p>
                </div>
                ",
                heading = .("Benford's Law Analysis"),
                getting_started = .("Getting Started:"),
                step1 = .("Select a numeric variable containing naturally occurring numbers"),
                step2 = .("Choose number of digits to analyze (1-3, default: 2)"),
                step3 = .("Review the MAD and chi-square results in the Analysis Summary"),
                best_suited = .("Best suited for:"),
                use1 = .("Financial data (invoices, expenses, revenues)"),
                use2 = .("Scientific measurements spanning orders of magnitude"),
                use3 = .("Screening for systematic recording patterns such as rounding or preferred values"),
                note_title = .("Note:"),
                note_text = .("Requires 100+ observations for reliable results. Data should span at least two orders of magnitude.")
                )
                self$results$welcome$setContent(welcome_html)
                self$results$welcome$setVisible(TRUE)
                private$.setComputedVisible(FALSE)
                return()
            }
            self$results$welcome$setVisible(FALSE)
            private$.setComputedVisible(TRUE)

            # Clear every computed output BEFORE validation. clearWith lists only
            # var and digits, so a change to the DATA alone - a row filter, a
            # cell edit - re-enters .run() with last run's text, report, suspect
            # listing and plot state still in place. Without this reset a hard
            # stop ("Insufficient Data", "Invalid Values Detected") or an
            # exception would be rendered directly above a stale all-clear and a
            # stale plot from the previous, valid data.
            self$results$text$setContent("")
            self$results$text2$setContent("")
            self$results$reportSentence$setContent("")
            self$results$plot$setState(NULL)
            private$.blankSummaryRows()

            # Set clinical explanation
            explanation <- private$.generateClinicalExplanation()
            self$results$explanation$setContent(explanation)
            
            # Guidelines
            doclink <- .("Package documentation")
            guidelines <- glue::glue("
                <div style='padding: 10px; background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; border-radius: 3px; color: inherit;'>
                    <p><strong>{guidelines_title}</strong></p>
                    <ul style='margin-bottom: 10px;'>
                        <li>{guideline1}</li>
                        <li>{guideline2}</li>
                        <li>{guideline3}</li>
                    </ul>
                    <p style='margin-bottom: 0; font-size: 14px;'>
                        {more_info} <a href='https://github.com/carloscinelli/benford.analysis' target='_blank'>{doclink}</a>
                    </p>
                </div>
                ",
                guidelines_title = .("Analysis Guidelines"),
                guideline1 = .("Ensure data represents naturally occurring numbers (not artificial ranges)"),
                guideline2 = .("Minimum 100-1000 observations recommended for reliable results"),
                guideline3 = .("1-digit analysis has only 9 bins, so its MAD conformity label becomes usable at about 1000 observations against about 5200 for 2 digits; 2-digit analysis is more sensitive but needs the larger sample"),
                more_info = .("For technical details, see"),
                doclink = doclink
            )

            self$results$todo$setContent(guidelines)

            # Validate inputs
            if (!private$.validate())
                return()

            # Get data and show sample size warning if needed
            mydata <- self$data
            var <- jmvcore::toNumeric(mydata[[self$options$var]])
            # Same non-finite screen as .validate() (which reports the count).
            # benford() below is deliberately passed `var` IN FULL so that its
            # lines.used keeps indexing original row positions for the bin
            # listing, so a non-finite value has to become NA here rather than be
            # dropped - dropping it would shift every row number in that listing
            # onto the wrong patient. benford() itself discards NA.
            var[!is.finite(var)] <- NA
            var_cleaned <- var[!is.na(var) & is.finite(var) & var > 0]
            valid_count <- length(var_cleaned)

            # NOTE: neither data-quality condition is reported from here.
            # The order-of-magnitude range check is performed once in
            # .validate() (which reports it via the dataWarning item), and the
            # small-sample condition (valid_count < 100) is reported once by
            # .interpretResults(), which raises the "Sample small" notice AND
            # sets the Assessment row to "Limited evidence". A second copy used
            # to be appended to the Guidelines panel here, so n < 100 produced
            # the same finding in two places in two different registers - the
            # very duplication the range check was kept out of this block to
            # avoid. One condition, one message.

            # Get number of digits parameter (with default)
            digits <- self$options$digits %||% 2
            
            # Perform Benford analysis with error handling
            private$.checkpoint()
            tryCatch({
                # Run Benford analysis
                bfd.cp <- benford.analysis::benford(data = var,
                                                   number.of.digits = digits)

                # ENHANCED TEXT OUTPUT with digit distribution table (from benford2)
                enhanced_text <- private$.generateEnhancedTextOutput(bfd.cp, var_cleaned, digits)
                self$results$text$setContent(enhanced_text)
                
                # Get suspects - CRITICAL: Only extract the selected variable to prevent PHI leakage
                # getSuspects returns the entire data frame with all columns, which could expose PHI
                # We only need the selected variable values, not patient IDs, dates, etc.
                var_name <- self$options$var
                suspects_full <- benford.analysis::getSuspects(bfd = bfd.cp,
                                                              data = data.frame(row = seq_along(var),
                                                                                value = var))

                # Extract only the selected variable column - do NOT expose other columns
                # This prevents PHI leakage from unselected variables
                if (!is.null(suspects_full) && nrow(suspects_full) > 0) {
                    # Carry the row number THROUGH getSuspects rather than
                    # reconstructing it afterwards. getSuspects() ends in
                    # data[bfd$data$lines.used[...], , drop = FALSE], so a `row`
                    # column added to the input frame comes back already aligned
                    # to the rows it selected.
                    #
                    # Two earlier attempts got this wrong. First, rownames were
                    # read back with as.numeric(rownames(suspects_full)); the
                    # returned object is a data.table whose rownames are RESET to
                    # 1..nrow, so the "indices" were always 1, 2, 3, ... Then the
                    # row numbers were recovered with match(suspect_values, var),
                    # but match() returns only the FIRST position of each value,
                    # so every repeated value sent all of its flagged rows to the
                    # same wrong row number - and repeated values are the norm in
                    # rounded clinical measurements (verified on n=1000: 67 of 172
                    # row numbers wrong). Under a heading telling the user to
                    # check these rows against source records, that dispatches
                    # them to the wrong patient.
                    suspect_values <- as.numeric(suspects_full$value)
                    suspect_indices <- as.integer(suspects_full$row)

                    # Create safe output with only the selected variable values
                    suspects_safe <- data.frame(
                        Index = suspect_indices,
                        Value = suspect_values,
                        stringsAsFactors = FALSE
                    )
                    # The user's own column name, verbatim. text2 is a
                    # Preformatted item and the jamovi client renders those with
                    # `innerText = content` (jmv-results-preformatted in
                    # client/dist/assets), so it is not a raw-HTML sink and needs
                    # no escaping; print.data.frame reproduces any name faithfully.
                    # Escaping here only mangled the header, showing a column named
                    # 'Serum Na+ (mmol/L) <lab>' as 'Serum_Na_mmol_L_lab_'.
                    colnames(suspects_safe) <- c(.("Row"), var_name)
                } else {
                    suspects_safe <- NULL
                }

                # Format the leading-digit bin listing with its expected share
                if (!is.null(suspects_safe) && nrow(suspects_safe) > 0) {
                    suspects_text <- private$.generateEnhancedSuspectsOutput(suspects_safe, valid_count, bfd.cp)
                } else {
                    suspects_text <- .("No observations fell in the two most-deviating leading-digit bins. This does not by itself establish conformity to Benford's Law; see the MAD and chi-square results in the summary table.")
                }
                
                self$results$text2$setContent(suspects_text)

                # Generate clinical interpretation with safe suspect data
                interpretation <- private$.interpretResults(bfd.cp, suspects_safe, var)

                # Populate summary table with TRANSPARENT statistical evidence
                # First, show sample size
                self$results$summary$setRow(rowKey=1L, values=list(
                    value=as.character(interpretation$total_observations),
                    interpretation=.("Number of observations analyzed")
                ))

                # Second, show PRIMARY statistical evidence (MAD)
                # The bare conformity label sat here unqualified, so the table could
                # read "Conformity: Nonconformity" on one row and "Assessment: Low"
                # on the next. Where the label is below its usable sample size, say
                # so on the row that shows it.
                mad_note <- if (isTRUE(interpretation$mad_label_reliable) ||
                                is.na(interpretation$mad_conformity)) {
                    jmvcore::format(.("Conformity: {label}"),
                                    label = interpretation$mad_conformity)
                } else {
                    jmvcore::format(
                        .("Conformity: {label} - not reliable at N={n} for {digits}-digit analysis (sampling noise alone gives MAD ~ {floor}; needs N > {needed})"),
                        label = interpretation$mad_conformity,
                        n = interpretation$total_observations,
                        digits = interpretation$n_digits,
                        floor = sprintf("%.4f", interpretation$mad_floor),
                        needed = ceiling(private$.minNForMadLabel(interpretation$n_digits)))
                }
                self$results$summary$setRow(rowKey=2L, values=list(
                    value=sprintf("%.4f", interpretation$mad_value),
                    interpretation=mad_note
                ))

                # Third, show Chi-square goodness-of-fit test
                self$results$summary$setRow(rowKey=3L, values=list(
                    value=sprintf("X\u{00B2} = %.2f, df = %d", interpretation$chisq_statistic, interpretation$chisq_df),
                    interpretation=jmvcore::format(.("p-value = {p}. This is the test the Assessment row is based on."),
                                                   p = private$.fmtP(interpretation$chisq_pvalue))
                ))

                # Fourth, show Mantissa Arc Test.
                # benford.analysis:::mantissa.arc.test returns statistic = L2 =
                # mean(cos 2*pi*m)^2 + mean(sin 2*pi*m)^2 and p = exp(-n * L2),
                # which is the upper tail of a chi-square on 2 df evaluated at
                # 2*n*L2, NOT at L2. Printing "L2 = 0.0021, df = 2" beside
                # p = 0.121 invited the reader to check pchisq(0.0021, 2,
                # lower = FALSE) = 0.999 and find a three-order-of-magnitude
                # contradiction on one line. Print the statistic on the scale its
                # df refers to (verified n=1000: L2 = 0.0021147, 2nL2 = 4.229,
                # pchisq(4.229, 2, lower = FALSE) = 0.1206652, identical to the
                # package p-value).
                #
                # The row carries an explicit "not used by the Assessment"
                # qualifier. This test examines the mantissa distribution, not
                # the leading-digit frequencies, and the verdict logic in
                # .interpretResults() never reads it - so on data that conform to
                # Benford's Law it prints p < 0.05 at its nominal rate (measured
                # on exactly-Benford 10^U data, 2000 reps: 0.052 at n=100, 0.050
                # at n=300, 0.054 at n=1000, 0.054 at n=5000 - correctly
                # calibrated, and therefore significant about one clean run in
                # twenty). Unqualified, that put a bare significant p-value two
                # rows above "Assessment: No departure detected", which a reader
                # scanning a results table reads as a contradiction. Verified:
                # 3-digit, n=300, exactly-Benford -> "2nL2 = 9.68, df = 2,
                # p-value = 0.0079" directly above "No departure detected".
                self$results$summary$setRow(rowKey=4L, values=list(
                    value=sprintf("2nL\u{00B2} = %.2f, df = %d",
                                  2 * interpretation$n_used * interpretation$mat_statistic,
                                  interpretation$mat_df),
                    interpretation=jmvcore::format(.("p-value = {p}. Supplementary: this test examines the mantissa distribution rather than the leading digits, and the Assessment row is not based on it. On data that follow Benford's Law it reaches p below 0.05 about once in twenty runs, like any other test, so a small p-value here alongside a chi-square p-value that is not small is an ordinary result rather than a contradiction."),
                                                   p = private$.fmtP(interpretation$mat_pvalue))
                ))

                # Fifth, show suspect counts (descriptive, not primary evidence)
                self$results$summary$setRow(rowKey=5L, values=list(
                    value=sprintf("%d (%.1f%%)", interpretation$suspicious_count, interpretation$suspicion_rate),
                    interpretation=.("Observations falling in the 2 most-deviating leading-digit bins. This is bin membership, not a count of outliers - compare it with the share those bins hold under Benford's Law, shown in the listing below.")
                ))

                # Sixth, show EVIDENCE-BASED clinical assessment
                self$results$summary$setRow(rowKey=6L, values=list(
                    value=interpretation$finding,
                    interpretation=interpretation$clinical_interpretation
                ))
                
                # Generate clinical report sentence
                report_sentence <- private$.generateReportSentence(interpretation, digits)
                self$results$reportSentence$setContent(report_sentence)
                
                # Prepare Data for Plot.
                # bfd.cp$data and bfd.cp$s.o.data are data.tables with ONE ROW PER
                # OBSERVATION. jmvcore serializes state with saveRDS + memCompress
                # and warns past 500000 bytes, which the untrimmed object crosses
                # at n ~ 25000 (measured 513,158 bytes; trimmed 7,728, and flat in
                # n). No panel the renderer draws reads either table: every panel
                # reads the pre-aggregated $bfd (one row per digit bin) except the
                # mantissa panel, which is excluded by the `except` pinned in
                # .plot(). getSuspects() has already run above. Verified
                # byte-identical PNGs at the declared 700x500 for 1, 2 and 3 digits.
                #
                # REMOVED rather than emptied on purpose: if a future
                # plot.Benford does read one, NULL raises a hard error that
                # .plot()'s tryCatch turns into a missing plot, whereas a zero-row
                # table would silently draw an empty panel that reads like a real
                # result. Anything added here needing the per-observation rows
                # (getDuplicates, for one) must run BEFORE this point.
                plotData <- bfd.cp
                plotData$data <- NULL
                plotData$s.o.data <- NULL
                image <- self$results$plot
                image$setState(plotData)
                
            }, error = function(e) {
                # User-friendly error messages with clinical context
                if (grepl("NA|NaN", e$message)) {
                    error_msg <- .("Error: Variable contains missing or non-numeric values that cannot be analyzed. Please ensure your selected variable contains valid numeric data.")
                } else if (grepl("insufficient", e$message, ignore.case = TRUE)) {
                    error_msg <- .("Error: Insufficient data for Benford's Law analysis. This test requires at least 30-50 valid observations. Consider combining data or using a different variable.")
                } else {
                    # jmvcore::format rather than glue::glue: the template is a
                    # translated .() string and glue evaluates {} contents as R
                    # code. This is also the one template whose VALUE is derived
                    # from the data (an upstream error message), so the safest
                    # substitution primitive is the right one to use here.
                    error_msg <- jmvcore::format(
                        .("Analysis error: {msg}. Please check your data and try again."),
                        msg = e$message)
                }

                # Surface fatal errors via the dataWarning Html item (which has
                # clearWith) rather than as a summary-table row. This keeps error
                # text from co-mingling with the statistical rows, and avoids
                # leaving stale values if the error was thrown after some rows
                # had already been filled. The rows themselves are created once
                # in .init(), so they are blanked here rather than deleted -
                # deleting them would make a later setRow() (e.g. when a
                # checkpoint restart re-enters .run()) fail with "rowKey not found".
                private$.blankSummaryRows()
                # Escape the (potentially data-derived) error message before it
                # enters the Html item, so it cannot inject markup.
                error_msg_html <- as.character(error_msg)
                error_msg_html <- gsub("&", "&amp;", error_msg_html, fixed = TRUE)
                error_msg_html <- gsub("<", "&lt;", error_msg_html, fixed = TRUE)
                error_msg_html <- gsub(">", "&gt;", error_msg_html, fixed = TRUE)
                error_html <- paste0(
                    "<div style='background-color: rgba(216, 33, 50, 0.18); border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='margin-top: 0;'>", .("Analysis Error"), "</h4>",
                    "<p>", error_msg_html, "</p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(error_html)
                self$results$dataWarning$setVisible(TRUE)
                private$.addNotice("ERROR", .("Benford analysis could not be completed"),
                    paste0(as.character(error_msg), " ",
                           .("No results below were produced for this variable.")))
            })
        },

        .generateEnhancedTextOutput = function(benford_obj, cleaned_data, digits) {
            # Enhanced text output with digit distribution table (from benford2)

            # Extract distribution data
            observed_props <- benford_obj$bfd$data.dist
            expected_props <- benford_obj$bfd$benford.dist

            # Create digit distribution table
            dist_table <- paste0(
                "\n",
                paste(rep("=", 50), collapse = ""), "\n",
                jmvcore::format(.("DIGIT DISTRIBUTION ANALYSIS ({digits}-digit)"),
                                digits = digits), "\n",
                paste(rep("=", 50), collapse = ""), "\n"
            )

            if (digits == 1) {
                # Show full table for 1-digit analysis
                digit_labels <- 1:9
                dist_table <- paste0(dist_table,
                    sprintf("%-8s | %-10s | %-10s | %-10s\n", "Digit", "Expected %", "Observed %", "Deviation"),
                    paste(rep("-", 50), collapse = ""), "\n"
                )

                for (i in seq_along(observed_props)) {
                    dist_table <- paste0(dist_table,
                        sprintf("%-8d | %9.1f%% | %9.1f%% | %+9.1f%%\n",
                               digit_labels[i],
                               expected_props[i] * 100,
                               observed_props[i] * 100,
                               (observed_props[i] - expected_props[i]) * 100)
                    )
                }
            } else {
                # For 2+ digits a full 90- or 900-row table is unreadable, but
                # printing only the MAD and a bin count left this section almost
                # empty at the DEFAULT setting. Show the bins that actually drive
                # the deviation instead.
                dist_table <- paste0(dist_table,
                    jmvcore::format(.("Mean Absolute Deviation (MAD): {mad}"),
                                    mad = sprintf("%.6f", benford_obj$MAD)), "\n",
                    jmvcore::format(.("Number of combinations analyzed: {bins}"),
                                    bins = length(observed_props)), "\n",
                    "\n", .("Most-deviating digit combinations:"), "\n",
                    sprintf("%-8s | %-10s | %-10s | %-10s\n", "Digits", "Expected %", "Observed %", "Deviation"),
                    paste(rep("-", 50), collapse = ""), "\n"
                )
                bfd_df <- as.data.frame(benford_obj$bfd)
                top <- utils::head(order(bfd_df$absolute.diff, decreasing = TRUE), 10)
                for (i in top) {
                    dist_table <- paste0(dist_table,
                        sprintf("%-8s | %9.2f%% | %9.2f%% | %+9.2f%%\n",
                                as.character(bfd_df$digits[i]),
                                expected_props[i] * 100,
                                observed_props[i] * 100,
                                (observed_props[i] - expected_props[i]) * 100)
                    )
                }
            }

            dist_table <- paste0(dist_table, paste(rep("=", 50), collapse = ""), "\n")

            # Add key statistics
            enhanced_text <- paste0(
                "\n", .("DATA SUMMARY:"), "\n",
                "  ", jmvcore::format(.("Total observations: {n}"),
                              n = base::format(length(cleaned_data), big.mark = ",")), "\n",
                "  ", jmvcore::format(.("Data range: {min} to {max}"),
                              min = base::format(min(cleaned_data), big.mark = ","),
                              max = base::format(max(cleaned_data), big.mark = ",")), "\n",
                "  ", jmvcore::format(.("Range ratio: {ratio}x"),
                              ratio = base::format(round(max(cleaned_data)/min(cleaned_data), 2), big.mark = ",")), "\n",
                dist_table,
                "\n",
                .("STATISTICAL TESTS:"), "\n",
                # MAD.conformity is benford.analysis's own English label, and is
                # interpolated here as DATA, not as a translatable literal.
                "  ", jmvcore::format(.("Chi-square: {stat} (p = {p})"),
                              stat = round(benford_obj$stats$chisq$statistic, 4),
                              p = format.pval(benford_obj$stats$chisq$p.value, digits = 4, eps = 0.0001)), "\n",
                "  ", jmvcore::format(.("MAD: {mad} ({label})"),
                              mad = round(benford_obj$MAD, 6),
                              label = benford_obj$MAD.conformity), "\n",
                "  ", jmvcore::format(.("Mantissa Arc Test: L\u{00B2} = {stat} (p = {p})"),
                              stat = round(benford_obj$stats$mantissa.arc.test$statistic, 4),
                              p = format.pval(benford_obj$stats$mantissa.arc.test$p.value, digits = 4, eps = 0.0001)), "\n"
            )

            return(enhanced_text)
        },

        # What this panel is NOT: a list of outliers.
        #
        # getSuspects() takes the 2 digit bins whose observed frequency deviates
        # most from Benford's Law and returns EVERY observation whose leading
        # digits fall in them. The count is therefore bin membership, and it
        # tracks how much of the data those bins hold - roughly 2/9 of anything
        # at 1 digit - not how far the data depart from Benford's Law.
        #
        # This panel used to convert that count into a HIGH / MEDIUM / LOW RISK
        # ladder at 10 / 5 / 2 percent, printed under "SUSPICIOUS DATA POINTS
        # IDENTIFIED" and "FRAUD DETECTION INDICATORS". On data drawn to be
        # exactly Benford, 1-digit analysis gives suspect rates of 14 to 44
        # percent, so the ladder read "HIGH RISK" every time while chi-square
        # p was 0.19 to 0.83. Printing the share those same bins would hold
        # under Benford's Law next to the observed share is what makes the
        # number readable: on conforming data the two agree (verified n=1000,
        # 1 digit: 38.5 percent observed against 39.8 percent expected).
        .generateEnhancedSuspectsOutput = function(suspects_safe, total_count, benford_obj) {

            n_suspects <- nrow(suspects_safe)
            suspect_rate <- round((n_suspects / total_count) * 100, 2)

            # Expected share of the data in the same 2 bins under Benford's Law.
            # getSuspects() selects by absolute.diff and takes 2 bins by default,
            # so reproduce exactly that selection here.
            bins_text <- ""
            expected_text <- .("not available for this digit setting")
            bfd_df <- try(as.data.frame(benford_obj$bfd), silent = TRUE)
            if (!inherits(bfd_df, "try-error") &&
                all(c("digits", "benford.dist", "absolute.diff") %in% names(bfd_df))) {
                top <- utils::head(order(bfd_df$absolute.diff, decreasing = TRUE), 2)
                bins_text <- paste(bfd_df$digits[top], collapse = ", ")
                expected_text <- sprintf("%.1f%%", 100 * sum(bfd_df$benford.dist[top]))
            }

            # Two descriptive counts used to print here: values that are
            # multiples of 100, and repeated values. Both were computed only over
            # the 2 SELECTED BINS rather than the variable, so "repeated values"
            # answered a question nobody asked, and "multiples of 100" is a poor
            # rounding probe for clinical data, which is far more often recorded
            # to the nearest 5, 10, 0.5 or a fixed number of decimals. They were
            # removed rather than repaired: the panel already reports the bins
            # selected, the share of the data they hold and the share expected,
            # which is what makes the listing readable. (An earlier "threshold
            # avoidance" test - proximity to 1000 / 5000 / 10000 - was an
            # accounting reporting-threshold check with no meaning for lab
            # values, tumour sizes or cell counts, and went the same way.)
            # Cap the listing. Membership scales with the data: 1820 rows at
            # n=5000 with 1 digit. Rendering every row into the results pane
            # also serialises all of them into the saved .omv on every run.
            max_listed <- 100
            listing <- utils::head(suspects_safe, max_listed)
            listing_text <- paste(capture.output(print(listing, row.names = FALSE)),
                                  collapse = "\n")
            if (n_suspects > max_listed)
                listing_text <- paste0(listing_text, "\n",
                    jmvcore::format(.("... and {more} more (showing the first {shown} of {total})"),
                            more = n_suspects - max_listed, shown = max_listed,
                            total = n_suspects))

            # The two explanatory paragraphs below used to be seven separate
            # .() calls, each holding the fragment that happened to fit on one
            # 70-column source line ("...deviates most" / "from Benford's Law
            # were selected, and every observation falling in" / ...). Each
            # fragment became its own msgid, so a translator received half
            # sentences in fixed English word order with no way to reorder them.
            # They are now one .() per sentence, wrapped at render time by
            # private$.wrapText(), which lays out whatever the translation is.
            suspects_text <- paste0(
                .("LEADING-DIGIT BIN MEMBERSHIP"), "\n",
                paste(rep("=", 50), collapse = ""), "\n\n",
                .("WHAT THIS LIST IS:"), "\n",
                private$.wrapText(.("The 2 leading-digit bins whose observed frequency deviates most from Benford's Law were selected, and every observation falling in them is listed below. An observation appears here because of its leading digits, not because it is individually unusual.")), "\n\n",
                if (nzchar(bins_text)) paste0("  ",
                    jmvcore::format(.("Bins selected: {bins}"), bins = bins_text), "\n") else "",
                "  ", jmvcore::format(.("Observations in those bins: {n} / {total} ({pct}%)"),
                                      n = n_suspects, total = total_count,
                                      pct = suspect_rate), "\n",
                "  ", jmvcore::format(.("Share expected in those same bins under Benford's Law: {pct}"),
                                      pct = expected_text), "\n\n",
                private$.wrapText(.("A percentage close to the expected share is what conforming data looks like. Whether the data depart from Benford's Law is answered by the MAD and chi-square results in the summary table, not by this count.")), "\n\n",
                .("OBSERVATIONS IN THOSE BINS:"), "\n",
                listing_text, "\n\n",
                # The Row column holds positions in the ANALYSIS dataset, i.e.
                # the rows left after any jamovi row filter, not spreadsheet row
                # numbers. The panel invites the reader to go and look these
                # observations up, so it has to say which numbering it is using.
                # Wording matches the note checkdata.b.R sets on its outlier
                # table for the same reason.
                private$.wrapText(.("Row numbers refer to the rows included in this analysis. If a row filter is active they will not match the spreadsheet row numbers.")), "\n\n",
                .("(Only the selected variable is shown; other columns are never listed.)"), "\n"
            )

            return(suspects_text)
        },

        # Wrap one complete sentence for the Preformatted panels. Keeping the
        # sentence whole in the source is what makes it translatable; the line
        # breaks are cosmetic and belong here rather than in the .() literal.
        .wrapText = function(text, indent = "  ", width = 74) {
            paste(paste0(indent, strwrap(text, width = width)), collapse = "\n")
        },

        .plot = function(image, ggtheme, theme, ...) {

            # Get plot data from state. State is only set when the analysis in
            # .run() succeeded, and clearWith invalidates it when var/digits
            # change, so a non-NULL state is a sufficient guard. We intentionally
            # do NOT re-run .validate() here: it re-reads self$data and rewrites
            # the dataWarning item as a side effect, which a render function
            # should not do.
            plotData <- image$state

            # renderFun must return a logical: TRUE if something was drawn.
            if (is.null(plotData))
                return(FALSE)

            tryCatch({
                # benford.analysis:::plot.Benford is BASE graphics, not ggplot2,
                # so the supplied ggtheme cannot be applied and the default black
                # axes, labels and titles are unreadable on jamovi's dark
                # background. Take the foreground colour from the theme jamovi
                # passed in and set the base-graphics equivalents. plot.Benford
                # saves par(no.readonly) on entry and restores it on exit, so it
                # will preserve these while it draws its panels; on.exit puts the
                # session's own settings back afterwards.
                # `graphics` carries an @importFrom tag on this file even though every
                # call here is namespaced. A bare pkg:: call contributes nothing to
                # NAMESPACE, and each submodule's DESCRIPTION Imports are synced FROM
                # its own NAMESPACE. Without the tag, `graphics` reached the umbrella
                # NAMESPACE only via files that do not ship in ClinicoPathDescriptives,
                # so the generated submodule called graphics::par() with no Imports
                # entry -- which jamovi cannot resolve on a user's machine.
                fg <- tryCatch(theme$color[[1]], error = function(e) NULL)
                if (is.null(fg) || is.na(fg) || !nzchar(fg))
                    fg <- graphics::par("fg")
                op <- graphics::par(fg = fg, col = fg, col.axis = fg,
                                    col.lab = fg, col.main = fg, col.sub = fg)
                on.exit(graphics::par(op), add = TRUE)

                # plot.Benford draws as a side effect and returns the saved par
                # list invisibly, so the old `plot <- plot(x); print(plot)` drew
                # the figure once and then printed a par list to stdout.
                # `except` names the two panels NOT drawn. Pinned rather than
                # left to plot.Benford's own default (the same pair today, so this
                # is byte-identical to plot(plotData)) because the excluded
                # mantissa panel is the sole reader of the per-observation $data
                # table that .run() drops from the state. Stating it here keeps the
                # trim and the panel list from drifting apart.
                plot(plotData, except = c("mantissa", "abs diff"))
                TRUE
            }, error = function(e) {
                # If plot fails, return FALSE silently
                FALSE
            })
        }
    )
)
