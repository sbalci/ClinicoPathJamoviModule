#' @title Benford's Law Analysis
#' @description This function performs a Benford's Law analysis on a numeric variable to detect unusual digit patterns that may indicate data quality issues.
#' It returns the Benford's Law distribution and a list of potential suspects with clinical interpretation.
#' @details The Benford's Law analysis is a statistical test to determine if the distribution of the first digits of a numeric variable follows Benford's Law.
#' This is commonly used in clinical research to detect data entry errors, fraud, or other quality issues.
#' The analysis returns structured results with clinical interpretation and actionable guidance.
#' @param var The numeric variable to analyze.
#' @param digits Number of first digits to analyze (default: 2).
#' @return A comprehensive analysis with clinical interpretation and actionable recommendations.
#' @importFrom benford.analysis benford getSuspects
#' @importFrom glue glue
#' @importFrom jmvcore composeTerm constructFormula toNumeric
#'
#'
#' @returns A comprehensive Benford's Law analysis with clinical interpretation.
#' @export benfordClass
#'


benfordClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "benfordClass",
    inherit = benfordBase,
    private = list(
        .validate = function() {
            if (is.null(self$options$var)) {
                return(FALSE)
            }

            if (nrow(self$data) == 0) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>⚠️ No Data Available</h4>",
                    "<p style='color: #721c24;'>", .("Data contains no (complete) rows."), "</p>",
                    "<p>", .("Please check your data for missing values or filtering issues."), "</p>",
                    "</div>"
                )
                self$results$dataWarning$setContent(html)
                return(FALSE)
            }

            # Enhanced validation with clinical guidance
            var_data <- jmvcore::toNumeric(self$data[[self$options$var]])
            valid_count <- sum(!is.na(var_data))

            # Check minimum sample size
            if (valid_count < 30) {
                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>⚠️ Insufficient Data</h4>",
                    "<p style='color: #721c24;'>", .("Benford's Law analysis requires at least <strong>30 valid observations</strong> for meaningful results."), "</p>",
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
                return(FALSE)
            }

            # Check for positive values only (Benford's Law requirement)
            non_na_data <- var_data[!is.na(var_data)]
            if (any(non_na_data <= 0)) {
                zero_count <- sum(non_na_data == 0)
                negative_count <- sum(non_na_data < 0)

                html <- paste0(
                    "<div style='background-color: #f8d7da; border-left: 4px solid #dc3545; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #721c24;'>⚠️ Invalid Values Detected</h4>",
                    "<p style='color: #721c24;'><strong>", .("Benford's Law only applies to positive numbers."), "</strong></p>",
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
                return(FALSE)
            }

            # Check for multiple orders of magnitude (Benford's Law requirement)
            min_val <- min(non_na_data, na.rm = TRUE)
            max_val <- max(non_na_data, na.rm = TRUE)
            magnitude_range <- log10(max_val / min_val)

            if (magnitude_range < 1) {
                html <- paste0(
                    "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 15px; margin: 10px 0;'>",
                    "<h4 style='margin-top: 0; color: #856404;'>⚠️ Limited Data Range</h4>",
                    "<p style='color: #856404;'><strong>", .("Warning: Data spans less than one order of magnitude."), "</strong></p>",
                    "<p>", .("Benford's Law works best when data spans multiple orders of magnitude (e.g., values ranging from 10 to 1000+)."), "</p>",
                    "<p><strong>", .("Your data range:"), "</strong> ", round(min_val, 2), " ", .("to"), " ", round(max_val, 2),
                    " (", round(magnitude_range, 2), " ", .("orders of magnitude"), ")</p>",
                    "<hr style='border-color: #ffc107;'>",
                    "<p><strong>", .("This analysis may not be meaningful because:"), "</strong></p>",
                    "<ul style='margin-left: 20px;'>",
                    "<li>", .("Narrow ranges (e.g., 45-55) don't exhibit Benford's Law patterns"), "</li>",
                    "<li>", .("Results will likely show false deviations"), "</li>",
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
                # Continue with analysis but user is warned
            } else {
                # Clear warnings if validation passes
                self$results$dataWarning$setContent("")
            }

            return(TRUE)
        },

        .escapeVar = function(x) {
            # Escape variable names for safe use in outputs and column names
            # Handles spaces, punctuation, and special characters
            if (is.null(x) || length(x) == 0) return(x)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        .interpretResults = function(benford_obj, suspects_obj, var_data) {
            n_total <- length(var_data[!is.na(var_data)])
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
            if (n_total < 100) {
                clinical_interpretation <- .("CAUTION: Sample size too small for reliable Benford's Law analysis. With fewer than 100 observations, statistical tests lack power and results should NOT be used for clinical decision-making or data quality assessment.")
                recommendation <- .("Increase sample size to at least 100-1000 observations before drawing conclusions. Consider alternative data quality checks for small datasets.")
                concern_level <- .("Unreliable (N<100)")
            } else {
                # Use EVIDENCE-BASED interpretation from MAD conformity levels
                # These thresholds are from published Benford's Law literature:
                # Nigrini, M. (2012). Benford's Law: Applications for Forensic Accounting
                #   MAD < 0.006: Close conformity
                #   MAD 0.006-0.012: Acceptable conformity
                #   MAD 0.012-0.015: Marginally acceptable conformity
                #   MAD > 0.015: Nonconformity

                if (mad_conformity == "Close conformity" || mad_conformity == "Acceptable conformity") {
                    clinical_interpretation <- sprintf(
                        .("Data conforms to Benford's Law (MAD=%.4f, %s). Chi-square test: p=%.4f. Low concern for systematic data quality issues."),
                        mad_value, mad_conformity, chisq_pvalue
                    )
                    recommendation <- .("Data quality appears good. No immediate action required. Continue routine monitoring.")
                    concern_level <- .("Low")

                } else if (mad_conformity == "Marginally acceptable conformity") {
                    clinical_interpretation <- sprintf(
                        .("Data shows marginally acceptable conformity to Benford's Law (MAD=%.4f). Chi-square test: p=%.4f. Consider reviewing data collection procedures."),
                        mad_value, chisq_pvalue
                    )
                    recommendation <- .("Review data entry and collection procedures. Investigate any known systematic biases or rounding practices.")
                    concern_level <- .("Moderate")

                } else {  # "Nonconformity"
                    clinical_interpretation <- sprintf(
                        .("Data does NOT conform to Benford's Law (MAD=%.4f, %s). Chi-square test: p=%.4f. Significant deviation suggests potential data quality issues, systematic bias, or data manipulation."),
                        mad_value, mad_conformity, chisq_pvalue
                    )
                    recommendation <- sprintf(
                        .("IMMEDIATE REVIEW REQUIRED: Investigate data sources, collection methods, and validation procedures. Check for systematic rounding, data entry errors, or potential manipulation. Verify %d flagged observations against source records."),
                        n_suspects
                    )
                    concern_level <- .("High")
                }
            }

            return(list(
                total_observations = n_total,
                valid_observations = n_total,
                suspicious_count = n_suspects,
                suspicion_rate = suspicion_rate,
                # Statistical evidence
                mad_value = mad_value,
                mad_conformity = mad_conformity,
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
            <div style='padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin-bottom: 20px;'>
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
            interpret_text = .("Results are based on MAD conformity levels: Close/Acceptable (<0.012) = Low concern; Marginally acceptable (0.012-0.015) = Moderate concern; Nonconformity (>0.015) = High concern. These are evidence-based thresholds, not arbitrary cutoffs."),
            action_title = .("What to do with results:"),
            action_text = .("Low concern: Continue routine monitoring. Moderate concern: Review data collection procedures. High concern: Immediate investigation of data sources, systematic bias, and individual flagged observations required.")
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
                    pval = sprintf("%.4f", interpretation_results$chisq_pvalue),
                    level = interpretation_results$concern_level
                )
            }

            report <- glue::glue("
            <div style='padding: 15px; background-color: #e8f5e8; border: 1px solid #28a745; border-radius: 5px;'>
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
        
        .run = function() {
            # Welcome message when no variable selected
            if (is.null(self$options$var)) {
                welcome_html <- glue::glue("
                <div style='padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; margin: 20px 0;'>
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
                    <p style='color: #856404; margin-top: 15px;'><strong>{note_title}</strong> {note_text}</p>
                </div>
                ",
                getting_started = .("Getting Started:"),
                step1 = .("Select a numeric variable containing naturally occurring numbers"),
                step2 = .("Choose number of digits to analyze (1-4, default: 2)"),
                step3 = .("Review statistical evidence and fraud detection indicators"),
                best_suited = .("Best suited for:"),
                use1 = .("Financial data (invoices, expenses, revenues)"),
                use2 = .("Scientific measurements spanning orders of magnitude"),
                use3 = .("Fraud detection and data quality assessment"),
                note_title = .("Note:"),
                note_text = .("Requires 100+ observations for reliable results. Data should span at least one order of magnitude.")
                )
                self$results$welcome$setContent(welcome_html)
                return()
            }

            # Set clinical explanation
            explanation <- private$.generateClinicalExplanation()
            self$results$explanation$setContent(explanation)
            
            # Guidelines
            doclink <- .("Package documentation")
            guidelines <- glue::glue("
                <div style='padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 3px;'>
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
                guideline3 = .("Use 1-digit analysis for initial screening, 2-digit for detailed investigation"),
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
            var_cleaned <- var[!is.na(var) & is.finite(var) & var > 0]
            valid_count <- length(var_cleaned)

            # Data quality warnings
            warnings_html <- ""

            if (valid_count < 100) {
                warning_title <- .("Warning:")
                warning_msg <- .("Small sample size detected. Results may be less reliable with fewer than 100 observations.")
                warnings_html <- paste0(warnings_html,
                    "<div style='color: #856404; margin-top: 10px;'><strong>",
                    warning_title, "</strong> ", warning_msg, "</div>")
            }

            # DATA RANGE VALIDATION (from benford2)
            # Check if data spans at least one order of magnitude
            if (valid_count > 0) {
                data_range <- max(var_cleaned) / min(var_cleaned)
                if (data_range < 10) {
                    range_warning_title <- .("Data Range Warning:")
                    range_warning_msg <- sprintf(.("Data spans less than one order of magnitude (range ratio = %.2f). Benford's Law may not apply naturally to constrained datasets."),
                                                data_range)
                    warnings_html <- paste0(warnings_html,
                        "<div style='color: #856404; margin-top: 10px;'><strong>",
                        range_warning_title, "</strong> ", range_warning_msg, "</div>")
                }
            }

            if (nchar(warnings_html) > 0) {
                guidelines <- paste(guidelines, warnings_html)
                self$results$todo$setContent(guidelines)
            }
            
            # Get number of digits parameter (with default)
            digits <- self$options$digits %||% 2
            
            # Perform Benford analysis with error handling
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
                                                              data = data.frame(value = var))

                # Extract only the selected variable column - do NOT expose other columns
                # This prevents PHI leakage from unselected variables
                if (!is.null(suspects_full) && nrow(suspects_full) > 0) {
                    # Get indices of suspects from the full dataset
                    suspect_indices <- as.numeric(rownames(suspects_full))
                    # Extract only the suspicious VALUES from the selected variable
                    suspect_values <- var[suspect_indices]

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
                    suspects_text <- private$.generateEnhancedSuspectsOutput(suspects_safe, valid_count)
                } else {
                    suspects_text <- .("No suspicious data points identified. Data follows expected Benford's Law distribution.")
                }
                
                self$results$text2$setContent(suspects_text)

                # Generate clinical interpretation with safe suspect data
                interpretation <- private$.interpretResults(bfd.cp, suspects_safe, var)

                # Populate summary table with TRANSPARENT statistical evidence
                # First, show sample size
                self$results$summary$addRow(rowKey=1, values=list(
                    statistic=.("Sample Size"),
                    value=as.character(interpretation$total_observations),
                    interpretation=.("Number of observations analyzed")
                ))

                # Second, show PRIMARY statistical evidence (MAD)
                self$results$summary$addRow(rowKey=2, values=list(
                    statistic=.("MAD (Mean Absolute Deviation)"),
                    value=sprintf("%.4f", interpretation$mad_value),
                    interpretation=sprintf(.("Conformity: %s"), interpretation$mad_conformity)
                ))

                # Third, show Chi-square goodness-of-fit test
                self$results$summary$addRow(rowKey=3, values=list(
                    statistic=.("Chi-square Test"),
                    value=sprintf("X² = %.2f, df = %d", interpretation$chisq_statistic, interpretation$chisq_df),
                    interpretation=sprintf(.("p-value = %.4f"), interpretation$chisq_pvalue)
                ))

                # Fourth, show Mantissa Arc Test
                self$results$summary$addRow(rowKey=4, values=list(
                    statistic=.("Mantissa Arc Test"),
                    value=sprintf("L² = %.4f, df = %d", interpretation$mat_statistic, interpretation$mat_df),
                    interpretation=sprintf(.("p-value = %.4e"), interpretation$mat_pvalue)
                ))

                # Fifth, show suspect counts (descriptive, not primary evidence)
                self$results$summary$addRow(rowKey=5, values=list(
                    statistic=.("Flagged Observations"),
                    value=sprintf("%d (%.1f%%)", interpretation$suspicious_count, interpretation$suspicion_rate),
                    interpretation=.("Individual outliers identified by algorithm")
                ))

                # Sixth, show EVIDENCE-BASED clinical assessment
                self$results$summary$addRow(rowKey=6, values=list(
                    statistic=.("Assessment"),
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
                
                self$results$summary$addRow(rowKey=1, values=list(
                    statistic=.("Error"),
                    value="",
                    interpretation=error_msg
                ))
            })
        },

        .generateEnhancedTextOutput = function(benford_obj, cleaned_data, digits) {
            # Enhanced text output with digit distribution table (from benford2)

            # Extract distribution data
            observed_props <- benford_obj$bfd$data.dist
            expected_props <- benford_obj$bfd$benford.dist

            # Generate digit labels based on number of digits
            if (digits == 1) {
                digit_labels <- 1:9
            } else if (digits == 2) {
                digit_labels <- 10:99
            } else {
                digit_labels <- paste0("First ", digits, " digits")
            }

            # Create digit distribution table
            dist_table <- paste0(
                "\n",
                paste(rep("=", 50), collapse = ""), "\n",
                "DIGIT DISTRIBUTION ANALYSIS (", digits, "-digit)\n",
                paste(rep("=", 50), collapse = ""), "\n"
            )

            if (digits == 1) {
                # Show full table for 1-digit analysis
                dist_table <- paste0(dist_table,
                    sprintf("%-8s | %-10s | %-10s | %-10s\n", "Digit", "Expected %", "Observed %", "Deviation"),
                    paste(rep("-", 50), collapse = ""), "\n"
                )

                for (i in 1:length(observed_props)) {
                    dist_table <- paste0(dist_table,
                        sprintf("%-8d | %9.1f%% | %9.1f%% | %+9.1f%%\n",
                               digit_labels[i],
                               expected_props[i] * 100,
                               observed_props[i] * 100,
                               (observed_props[i] - expected_props[i]) * 100)
                    )
                }
            } else {
                # For 2+ digits, show summary stats only
                dist_table <- paste0(dist_table,
                    sprintf("Mean Absolute Deviation (MAD): %.6f\n", benford_obj$MAD),
                    sprintf("Number of combinations analyzed: %d\n", length(observed_props))
                )
            }

            dist_table <- paste0(dist_table, paste(rep("=", 50), collapse = ""), "\n")

            # Add key statistics
            enhanced_text <- paste0(
                "\nDATA SUMMARY:\n",
                "  Total observations: ", format(length(cleaned_data), big.mark = ","), "\n",
                "  Data range: ", format(min(cleaned_data), big.mark = ","),
                " to ", format(max(cleaned_data), big.mark = ","), "\n",
                "  Range ratio: ", format(round(max(cleaned_data)/min(cleaned_data), 2), big.mark = ","), "x\n",
                dist_table,
                "\n",
                "STATISTICAL TESTS:\n",
                "  Chi-square: ", round(benford_obj$stats$chisq$statistic, 4),
                " (p = ", format.pval(benford_obj$stats$chisq$p.value, digits = 4, eps = 0.0001), ")\n",
                "  MAD: ", round(benford_obj$MAD, 6), " (", benford_obj$MAD.conformity, ")\n",
                "  Mantissa Arc Test: L² = ", round(benford_obj$stats$mantissa.arc.test$statistic, 4),
                " (p = ", format.pval(benford_obj$stats$mantissa.arc.test$p.value, digits = 4, eps = 0.0001), ")\n"
            )

            return(enhanced_text)
        },

        .generateEnhancedSuspectsOutput = function(suspects_safe, total_count) {
            # Enhanced suspect analysis with fraud detection indicators (from benford2)

            n_suspects <- nrow(suspects_safe)
            suspect_rate <- round((n_suspects / total_count) * 100, 2)
            suspect_values <- suspects_safe[[2]]  # Get the value column

            # FRAUD DETECTION INDICATORS (from benford2)
            round_numbers <- sum(suspect_values %% 100 == 0, na.rm = TRUE)
            threshold_avoid <- sum(abs(suspect_values - rep(c(1000, 5000, 10000), each = length(suspect_values))) < 50, na.rm = TRUE)
            clustering <- length(unique(suspect_values)) < length(suspect_values) * 0.8

            # Risk categorization
            risk_category <- if (suspect_rate > 10) {
                "HIGH RISK"
            } else if (suspect_rate > 5) {
                "MEDIUM RISK"
            } else if (suspect_rate > 2) {
                "LOW-MEDIUM RISK"
            } else {
                "LOW RISK"
            }

            # Create enhanced output
            suspects_text <- paste0(
                "SUSPICIOUS DATA POINTS IDENTIFIED\n",
                paste(rep("=", 50), collapse = ""), "\n\n",
                "SUSPECT SUMMARY:\n",
                "  Found: ", n_suspects, " suspicious data points out of ", total_count, " total observations\n",
                "  Suspect rate: ", suspect_rate, "%\n",
                "  Risk category: ", risk_category, "\n\n",
                "FRAUD DETECTION INDICATORS:\n",
                "  Round number preference: ", if (round_numbers > 0) paste0("DETECTED (", round_numbers, " values)") else "Not detected", "\n",
                "  Threshold avoidance: ", if (threshold_avoid > 0) "POSSIBLE" else "Not detected", "\n",
                "  Clustering patterns: ", if (clustering) "DETECTED" else "Not detected", "\n\n",
                "SUSPECT DETAILS:\n",
                paste(capture.output(print(suspects_safe, row.names = FALSE)), collapse = "\n"), "\n\n",
                "(Only showing selected variable values for privacy protection)\n",
                "Note: These points deviate significantly from expected Benford's Law patterns and should be verified against source data.\n"
            )

            return(suspects_text)
        },

        .plot = function(image, ggtheme, theme, ...) {

            # Use shared validation method
            if (!private$.validate())
                return()

            # Get plot data from state
            plotData <- image$state
            
            if (is.null(plotData))
                return()
            
            # Create plot with error handling and improved styling
            tryCatch({
                # Create base plot
                plot <- plot(plotData)
                
                # Print with enhanced styling if possible
                print(plot)
                TRUE
            }, error = function(e) {
                # If plot fails, return FALSE silently
                FALSE
            })
        }
    )
)