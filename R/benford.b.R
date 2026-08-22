#' @title Benford's Law Analysis
#' @description This function performs a Benford's Law analysis on a numeric variable to detect unusual digit patterns that may indicate data quality issues.
#' It returns the Benford's Law distribution and a list of potential suspects with clinical interpretation.
#' @details The Benford's Law analysis is a statistical test to determine if the distribution of the first digits of a numeric variable follows Benford's Law.
#' This is commonly used in clinical research to detect data entry errors, fraud, or other quality issues.
#' The analysis returns structured results with clinical interpretation and actionable guidance.
#' @importFrom benford.analysis benford getSuspects
#' @importFrom glue glue
#' @importFrom jmvcore toNumeric
#' @importFrom graphics par
#'
#'
#' @returns A comprehensive Benford's Law analysis with clinical interpretation.
#' @keywords internal
#'


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
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = "ERROR: ",
                    STRONG_WARNING = "WARNING: ",
                    WARNING        = "WARNING: ",
                    INFO           = "NOTE: ",
                    "")
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
                    sprintf(.("%d value(s) in the selected variable are infinite or not a number (Inf, -Inf or NaN). A leading digit is undefined for these, so they were excluded from the analysis and from every count reported below. Values like these arise routinely from computed variables, for example a ratio with a zero denominator; check how the variable was derived."),
                            n_nonfinite))

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
                    sprintf(.("Only %d valid observations are available; a leading-digit distribution needs at least 30 (and 100 or more before the tests carry useful power), so nothing below was computed. Select a variable with more recorded values, or pool comparable measurements before running this analysis."),
                            valid_count))
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
                    sprintf(.("The selected variable contains %d zero and %d negative values. A leading digit is undefined for these, so nothing below was computed. Filter them out, analyse increases and decreases separately if the variable is a change score, or choose a naturally positive measurement."),
                            zero_count, negative_count))
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
                    "<p><strong>", .("Your data range:"), "</strong> ", round(min_val, 2), " ", .("to"), " ", round(max_val, 2),
                    " (", round(magnitude_range, 2), " ", .("orders of magnitude"), ")</p>",
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
                private$.addNotice("STRONG_WARNING", .("Data span less than two orders of magnitude"),
                    sprintf(.("Values run from %s to %s, a range of %s orders of magnitude. Benford's Law describes data spanning several orders of magnitude; between one and two decades the leading-digit frequencies are dominated by where the range starts and stops rather than by Benford's Law, so the tests below are not calibrated and can report a large departure for data that were recorded perfectly (on simulated conforming data spanning 1.3 decades the chi-square test rejected in 38 percent of runs at a nominal 5 percent). Results are shown but the conformity assessment is not meaningful for this variable."),
                            format(round(min_val, 2)), format(round(max_val, 2)),
                            format(round(magnitude_range, 2))))
                # Continue with analysis but user is warned
            } else {
                # Clear warnings if validation passes. Hiding the item as well
                # keeps an empty "Data Validation" heading off a clean run.
                self$results$dataWarning$setContent("")
                self$results$dataWarning$setVisible(FALSE)
            }

            return(TRUE)
        },

        .escapeVar = function(x) {
            # Escape variable names for safe use in outputs and column names
            # Handles spaces, punctuation, and special characters
            if (is.null(x) || length(x) == 0) return(x)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
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

        # Smallest n at which the nonconformity cut-off clears the noise floor.
        # Solving mean(sqrt(2 p (1-p) / (pi n))) = cutoff for n gives a closed
        # form, since the only n-dependence is the 1/sqrt(n) factor.
        .minNForMadLabel = function(digits) {
            cutoff <- private$.madNonconformityCutoff(digits)
            if (is.na(cutoff)) return(NA_real_)
            # .expectedMadUnderNull(1, digits) is the constant multiplying 1/sqrt(n)
            k <- private$.expectedMadUnderNull(1, digits)
            (k / cutoff)^2
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
        # noise floor: n > 246 (1 digit), n > 1301 (2 digits), n > 2550 (3 digits).
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
            cutoff > floor_mad
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
                    sprintf(.("The %d-digit analysis spreads %d observations over %d digit bins. The smallest expected count is %.2f and %.0f%% of the bins have an expected count below 5, so the chi-square approximation is only approximate here and rejects more often than its nominal rate: on simulated conforming data the rejection rate in this regime ran between 6 and 14 percent against a nominal 5 percent, worst at the 3-digit setting. Read the p-value as indicative rather than exact. A 1-digit analysis uses 9 bins and puts far more observations in each."),
                            n_digits, n_used, length(expected_counts), min_expected,
                            100 * mean(expected_counts < 5)))

            if (n_total < 100) {
                private$.addNotice("STRONG_WARNING", .("Sample too small for a dependable verdict"),
                    sprintf(.("Only %d observations were analysed. The digit-frequency tests below are computed, but at this size they detect only very large departures, and the MAD conformity label is biased upward by sampling noise. Treat the assessment as provisional and gather more observations before drawing conclusions from it."),
                            n_total))
                clinical_interpretation <- .("CAUTION: Sample size too small for reliable Benford's Law analysis. With fewer than 100 observations, statistical tests lack power and results should NOT be used for clinical decision-making or data quality assessment.")
                recommendation <- .("Increase sample size to at least 100-1000 observations before drawing conclusions. Consider alternative data quality checks for small datasets.")
                concern_level <- .("Unreliable (N<100)")

            } else if (!is.na(mad_conformity) && !mad_label_reliable) {
                private$.addNotice("INFO", .("MAD conformity label not usable at this sample size"),
                    sprintf(.("Nigrini's MAD cut-off for %d-digit analysis is %s, but sampling noise alone produces a MAD of about %s at N=%d, so the label '%s' cannot separate a real departure from noise. It needs N above %d. The assessment below is taken instead from the chi-square goodness-of-fit test, together with the size of the MAD relative to that noise level."),
                            n_digits, format(private$.madNonconformityCutoff(n_digits)),
                            format(signif(mad_floor, 3)), n_total, mad_conformity,
                            ceiling(private$.minNForMadLabel(n_digits))))
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
                    clinical_interpretation <- sprintf(
                        .("Chi-square goodness-of-fit test indicates a departure from Benford's Law (p=%s). MAD = %.4f, which is %.1f times the deviation expected from sampling noise alone at N=%d with %d-digit analysis (%.4f). The '%s' label is not informative at this sample size, so this conclusion rests on the chi-square test and the size of the deviation relative to that noise level."),
                        private$.fmtP(chisq_pvalue), mad_value, mad_ratio, n_total, n_digits, mad_floor, mad_conformity
                    )
                    if (mad_ratio >= 2) {
                        recommendation <- .("Investigate data sources, collection methods, and validation procedures. Check for systematic rounding, preferred values, or data entry errors, and review the leading-digit bin listing below for repeated or rounded values.")
                        concern_level <- .("High")
                    } else {
                        recommendation <- sprintf(
                            .("Review data collection and entry procedures, and check for systematic rounding or preferred values. Collect at least %d observations before relying on the MAD conformity classification."),
                            ceiling(private$.minNForMadLabel(n_digits))
                        )
                        concern_level <- .("Moderate")
                    }
                } else {
                    clinical_interpretation <- sprintf(
                        .("No evidence of departure from Benford's Law (chi-square p=%s). MAD = %.4f, which is within the range expected from sampling noise alone at N=%d with %d-digit analysis (about %.4f), so the '%s' label is not informative at this sample size."),
                        private$.fmtP(chisq_pvalue), mad_value, n_total, n_digits, mad_floor, mad_conformity
                    )
                    recommendation <- sprintf(
                        .("No action indicated by this analysis. To use the MAD conformity classification, collect at least %d observations for %d-digit analysis, or switch to 1-digit analysis, which needs far fewer."),
                        ceiling(private$.minNForMadLabel(n_digits)), n_digits
                    )
                    concern_level <- .("Low")
                }

            } else if (is.na(mad_conformity)) {
                # benford.analysis returns MAD.conformity = NA when
                # number.of.digits > 3. digits is capped at 3 in benford.a.yaml,
                # but guard defensively so an NA conformity reports MAD
                # numerically instead of crashing on if(NA || NA).
                clinical_interpretation <- sprintf(
                    .("MAD = %.4f. A conformity classification is not available for this digit setting; interpret the MAD and chi-square test (p=%s) directly. Larger MAD values indicate greater deviation from Benford's Law."),
                    mad_value, private$.fmtP(chisq_pvalue)
                )
                recommendation <- .("No conformity label is available for this digit configuration. Review the numeric MAD and chi-square results directly, and consider using 1-3 digit analysis for a classified assessment.")
                concern_level <- .("Not classified")

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
                        clinical_interpretation <- sprintf(
                            .("MAD = %.4f falls in the '%s' band, but the chi-square goodness-of-fit test detects a departure from Benford's Law (p=%s). The MAD is %.1f times the deviation expected from sampling noise alone at N=%d with %d-digit analysis (%.4f), so the departure is statistically detectable but small in magnitude. The two measures disagree because the MAD cut-offs are fixed while the chi-square test gains power as N grows."),
                            mad_value, mad_conformity, private$.fmtP(chisq_pvalue),
                            mad_ratio, n_total, n_digits, mad_floor
                        )
                        recommendation <- .("Check the digit distribution for a localized cause such as systematic rounding, preferred values, or a subset of records entered differently, and review data collection and entry procedures for that subset.")
                        concern_level <- .("Moderate")
                    } else {
                        clinical_interpretation <- sprintf(
                            .("Leading-digit distribution is consistent with Benford's Law (MAD=%.4f, %s; chi-square p=%s). Neither measure detected a departure. Absence of a detected departure is not evidence that the data contain no errors - this analysis examines only leading-digit frequencies."),
                            mad_value, mad_conformity, private$.fmtP(chisq_pvalue)
                        )
                        recommendation <- .("No departure from Benford's Law was detected. This test does not assess accuracy, completeness, units, or transcription of individual values, so it does not substitute for the usual range, duplicate, and missingness checks.")
                        concern_level <- .("Low")
                    }

                } else if (mad_conformity == "Marginally acceptable conformity") {
                    clinical_interpretation <- sprintf(
                        .("Data shows marginally acceptable conformity to Benford's Law (MAD=%.4f). Chi-square test: p=%s. Consider reviewing data collection procedures."),
                        mad_value, private$.fmtP(chisq_pvalue)
                    )
                    recommendation <- .("Review data entry and collection procedures. Investigate any known systematic biases or rounding practices.")
                    concern_level <- .("Moderate")

                } else {  # "Nonconformity"
                    clinical_interpretation <- sprintf(
                        .("Data does NOT conform to Benford's Law (MAD=%.4f, %s). Chi-square test: p=%s. The deviation exceeds Nigrini's nonconformity cut-off for this digit setting, which is consistent with systematic data quality issues, bias in how values were recorded, or manipulation - the test does not distinguish between these."),
                        mad_value, mad_conformity, private$.fmtP(chisq_pvalue)
                    )
                    recommendation <- .("Investigate data sources, collection methods, and validation procedures. Check for systematic rounding, preferred values, or data entry errors, and review the leading-digit bin listing below.")
                    concern_level <- .("High")
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
                clinical_interpretation = clinical_interpretation,
                recommendation = recommendation,
                concern_level = concern_level
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
            what_text = .("Analyzes the distribution of first digits in your data using statistical tests to detect unusual patterns that may indicate data quality issues, systematic bias, entry errors, or fraud."),
            when_title = .("When to use:"),
            when_text = .("Use with naturally occurring numerical data (lab values, measurements, counts) that span multiple orders of magnitude. Requires at least 100 observations for reliable results. Not suitable for artificial ranges, assigned IDs, or categorical data."),
            tests_title = .("Statistical tests performed:"),
            tests_text = .("(1) MAD (Mean Absolute Deviation): Primary measure of conformity with validated thresholds. (2) Chi-square goodness-of-fit test: Tests overall distribution fit. (3) Mantissa Arc Test: Tests for subtle distributional anomalies. All tests are from published Benford's Law literature (Nigrini, 2012)."),
            interpret_title = .("How to interpret:"),
            interpret_text = .("Two measures are read together. The MAD (Mean Absolute Deviation) conformity label from the benford.analysis package measures the SIZE of the departure against Nigrini's digit-count-specific cutoffs, and its label is only used once the sample is large enough for those cutoffs to exceed the deviation sampling noise alone produces (about 250 observations at 1 digit, 1300 at 2, 2550 at 3); below that the summary table says so and the verdict comes from the chi-square test instead. The chi-square goodness-of-fit test asks whether ANY departure is detectable and gains power as N grows, so at large N it can flag a departure too small to move the MAD label - reported here as a detectable but small departure rather than as an all-clear. The flagged-observation count is bin membership, not an outlier count, and is descriptive only."),
            action_title = .("What to do with results:"),
            action_text = .("Low concern: no departure from Benford's Law was detected, which does not establish that the data are free of errors - this test looks only at leading-digit frequencies. Moderate concern: a departure was detected but is small relative to sampling noise, or the sample is too small for the MAD label; worth reviewing data collection and entry procedures. High concern: the departure is large relative to sampling noise; investigate data sources, systematic rounding or preferred values, and how the values were recorded.")
            )
            return(explanation)
        },
        
        .generateReportSentence = function(interpretation_results, digits) {
            # Format summary based on statistical evidence, not just suspect counts
            if (interpretation_results$total_observations < 100) {
                summary_text <- glue::glue(
                    .("Benford's Law analysis of {n} observations (N<100): Results unreliable due to insufficient sample size. Statistical tests require at least 100 observations for valid interpretation."),
                    n = interpretation_results$total_observations
                )
            } else {
                summary_text <- glue::glue(
                    .("Benford's Law analysis of {n} observations using {d}-digit analysis: MAD = {mad} ({conformity}), Chi-square p = {pval}. Assessment: {level} concern for data quality issues."),
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
                    level = interpretation_results$concern_level
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
            recommendation = interpretation_results$recommendation
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
                    <h3 style='color: #007bff; margin-top: 0;'>Benford's Law Analysis</h3>
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
                getting_started = .("Getting Started:"),
                step1 = .("Select a numeric variable containing naturally occurring numbers"),
                step2 = .("Choose number of digits to analyze (1-3, default: 2)"),
                step3 = .("Review the MAD and chi-square results in the Analysis Summary"),
                best_suited = .("Best suited for:"),
                use1 = .("Financial data (invoices, expenses, revenues)"),
                use2 = .("Scientific measurements spanning orders of magnitude"),
                use3 = .("Fraud detection and data quality assessment"),
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
            for (i in 1:6)
                self$results$summary$setRow(rowKey=i, values=list(
                    value="", interpretation=""))

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
                guideline3 = .("1-digit analysis has only 9 bins, so its MAD conformity label becomes usable at about 250 observations against about 1300 for 2 digits; 2-digit analysis is more sensitive but needs the larger sample"),
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

            # Data quality warnings
            warnings_html <- ""

            if (valid_count < 100) {
                warning_title <- .("Warning:")
                warning_msg <- .("Small sample size detected. Results may be less reliable with fewer than 100 observations.")
                warnings_html <- paste0(warnings_html,
                    "<div style='margin-top: 10px;'><strong>",
                    warning_title, "</strong> ", warning_msg, "</div>")
            }

            # NOTE: the order-of-magnitude range check is performed once in
            # .validate() (which reports it via the dataWarning item); it is not
            # duplicated here to avoid emitting two warnings for one condition.

            if (nchar(warnings_html) > 0) {
                guidelines <- paste(guidelines, warnings_html)
                self$results$todo$setContent(guidelines)
            }
            
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
                var_name_safe <- private$.escapeVar(var_name)  # Escape for safe column naming
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
                    # Use escaped name for safe column naming
                    colnames(suspects_safe) <- c(.("Row"), var_name_safe)
                } else {
                    suspects_safe <- NULL
                }

                # Format suspects output with clinical context and fraud indicators
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
                    sprintf(.("Conformity: %s"), interpretation$mad_conformity)
                } else {
                    sprintf(
                        .("Conformity: %s - not reliable at N=%d for %d-digit analysis (sampling noise alone gives MAD ~ %.4f; needs N > %d)"),
                        interpretation$mad_conformity, interpretation$total_observations,
                        interpretation$n_digits, interpretation$mad_floor,
                        ceiling(private$.minNForMadLabel(interpretation$n_digits)))
                }
                self$results$summary$setRow(rowKey=2L, values=list(
                    value=sprintf("%.4f", interpretation$mad_value),
                    interpretation=mad_note
                ))

                # Third, show Chi-square goodness-of-fit test
                self$results$summary$setRow(rowKey=3L, values=list(
                    value=sprintf("X\u{00B2} = %.2f, df = %d", interpretation$chisq_statistic, interpretation$chisq_df),
                    interpretation=sprintf(.("p-value = %s"), private$.fmtP(interpretation$chisq_pvalue))
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
                self$results$summary$setRow(rowKey=4L, values=list(
                    value=sprintf("2nL\u{00B2} = %.2f, df = %d",
                                  2 * interpretation$n_used * interpretation$mat_statistic,
                                  interpretation$mat_df),
                    interpretation=sprintf(.("p-value = %s"), private$.fmtP(interpretation$mat_pvalue))
                ))

                # Fifth, show suspect counts (descriptive, not primary evidence)
                self$results$summary$setRow(rowKey=5L, values=list(
                    value=sprintf("%d (%.1f%%)", interpretation$suspicious_count, interpretation$suspicion_rate),
                    interpretation=.("Observations falling in the 2 most-deviating leading-digit bins. This is bin membership, not a count of outliers - compare it with the share those bins hold under Benford's Law, shown in the listing below.")
                ))

                # Sixth, show EVIDENCE-BASED clinical assessment
                self$results$summary$setRow(rowKey=6L, values=list(
                    value=interpretation$concern_level,
                    interpretation=interpretation$clinical_interpretation
                ))
                
                # Generate clinical report sentence
                report_sentence <- private$.generateReportSentence(interpretation, digits)
                self$results$reportSentence$setContent(report_sentence)
                
                # Prepare Data for Plot
                plotData <- bfd.cp
                image <- self$results$plot
                image$setState(plotData)
                
            }, error = function(e) {
                # User-friendly error messages with clinical context
                if (grepl("NA|NaN", e$message)) {
                    error_msg <- .("Error: Variable contains missing or non-numeric values that cannot be analyzed. Please ensure your selected variable contains valid numeric data.")
                } else if (grepl("insufficient", e$message, ignore.case = TRUE)) {
                    error_msg <- .("Error: Insufficient data for Benford's Law analysis. This test requires at least 30-50 valid observations. Consider combining data or using a different variable.")
                } else {
                    error_template <- .("Analysis error: {msg}. Please check your data and try again.")
                    error_msg <- glue::glue(error_template, msg = e$message)
                }

                # Surface fatal errors via the dataWarning Html item (which has
                # clearWith) rather than as a summary-table row. This keeps error
                # text from co-mingling with the statistical rows, and avoids
                # leaving stale values if the error was thrown after some rows
                # had already been filled. The six rows themselves are created
                # once in .init(), so they are blanked here rather than deleted -
                # deleting them would make a later setRow() (e.g. when a
                # checkpoint restart re-enters .run()) fail with "rowKey not found".
                for (i in 1:6)
                    self$results$summary$setRow(rowKey=i, values=list(
                        value="", interpretation=""))
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
                "DIGIT DISTRIBUTION ANALYSIS (", digits, "-digit)\n",
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
                    sprintf("Mean Absolute Deviation (MAD): %.6f\n", benford_obj$MAD),
                    sprintf("Number of combinations analyzed: %d\n", length(observed_props)),
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
                "\nDATA SUMMARY:\n",
                "  Total observations: ", base::format(length(cleaned_data), big.mark = ","), "\n",
                "  Data range: ", base::format(min(cleaned_data), big.mark = ","),
                " to ", base::format(max(cleaned_data), big.mark = ","), "\n",
                "  Range ratio: ", base::format(round(max(cleaned_data)/min(cleaned_data), 2), big.mark = ","), "x\n",
                dist_table,
                "\n",
                "STATISTICAL TESTS:\n",
                "  Chi-square: ", round(benford_obj$stats$chisq$statistic, 4),
                " (p = ", format.pval(benford_obj$stats$chisq$p.value, digits = 4, eps = 0.0001), ")\n",
                "  MAD: ", round(benford_obj$MAD, 6), " (", benford_obj$MAD.conformity, ")\n",
                "  Mantissa Arc Test: L\u{00B2} = ", round(benford_obj$stats$mantissa.arc.test$statistic, 4),
                " (p = ", format.pval(benford_obj$stats$mantissa.arc.test$p.value, digits = 4, eps = 0.0001), ")\n"
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
            suspect_values <- suspects_safe[[2]]  # Get the value column

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

            # Descriptive digit-pattern notes. Deliberately NOT labelled fraud
            # indicators: rounded and repeated values are ordinary in clinical
            # measurement, and the old "threshold avoidance" test (proximity to
            # 1000 / 5000 / 10000) is an accounting reporting-threshold check
            # with no meaning for lab values, tumour sizes or cell counts, so it
            # has been removed rather than reworded.
            round_numbers <- sum(suspect_values %% 100 == 0, na.rm = TRUE)
            repeated_values <- n_suspects - length(unique(suspect_values))

            # Cap the listing. Membership scales with the data: 1820 rows at
            # n=5000 with 1 digit. Rendering every row into the results pane
            # also serialises all of them into the saved .omv on every run.
            max_listed <- 100
            listing <- utils::head(suspects_safe, max_listed)
            listing_text <- paste(capture.output(print(listing, row.names = FALSE)),
                                  collapse = "\n")
            if (n_suspects > max_listed)
                listing_text <- paste0(listing_text, "\n",
                    sprintf(.("... and %d more (showing the first %d of %d)"),
                            n_suspects - max_listed, max_listed, n_suspects))

            suspects_text <- paste0(
                .("LEADING-DIGIT BIN MEMBERSHIP"), "\n",
                paste(rep("=", 50), collapse = ""), "\n\n",
                .("WHAT THIS LIST IS:"), "\n",
                "  ", .("The 2 leading-digit bins whose observed frequency deviates most"), "\n",
                "  ", .("from Benford's Law were selected, and every observation falling in"), "\n",
                "  ", .("them is listed below. An observation appears here because of its"), "\n",
                "  ", .("leading digits, not because it is individually unusual."), "\n\n",
                if (nzchar(bins_text)) paste0("  ", .("Bins selected:"), " ", bins_text, "\n") else "",
                "  ", .("Observations in those bins:"), " ", n_suspects, " / ", total_count,
                " (", suspect_rate, "%)\n",
                "  ", .("Share expected in those same bins under Benford's Law:"), " ", expected_text, "\n\n",
                "  ", .("A percentage close to the expected share is what conforming data"), "\n",
                "  ", .("looks like. Whether the data depart from Benford's Law is answered"), "\n",
                "  ", .("by the MAD and chi-square results in the summary table, not by this"), "\n",
                "  ", .("count."), "\n\n",
                .("DIGIT-PATTERN NOTES (DESCRIPTIVE):"), "\n",
                "  ", .("Values that are multiples of 100:"), " ", round_numbers, "\n",
                "  ", .("Repeated values among all observations in those bins:"), " ", repeated_values, "\n",
                "  ", .("Rounding and repeated values are expected in clinical measurement"), "\n",
                "  ", .("(counts and sizes are routinely recorded to a fixed precision) and"), "\n",
                "  ", .("are not by themselves evidence of manipulation."), "\n\n",
                .("OBSERVATIONS IN THOSE BINS:"), "\n",
                listing_text, "\n\n",
                .("(Only the selected variable is shown; other columns are never listed.)"), "\n"
            )

            return(suspects_text)
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
                plot(plotData)
                TRUE
            }, error = function(e) {
                # If plot fails, return FALSE silently
                FALSE
            })
        }
    )
)
