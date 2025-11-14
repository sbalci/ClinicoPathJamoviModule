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
        
        .interpretResults = function(benford_obj, suspects_obj, var_data) {
            n_total <- length(var_data[!is.na(var_data)])
            n_suspects <- if (!is.null(suspects_obj) && nrow(suspects_obj) > 0) nrow(suspects_obj) else 0
            suspicion_rate <- round((n_suspects / n_total) * 100, 1)

            # CRITICAL: Add guardrails for small samples
            # Prevent authoritative clinical recommendations on statistically invalid data
            if (n_total < 100) {
                clinical_interpretation <- .("CAUTION: Sample size too small for reliable Benford's Law interpretation. Statistical validity is questionable with fewer than 100 observations. Results should NOT be used for clinical decision-making.")
                recommendation <- .("Increase sample size to at least 100-1000 observations before drawing conclusions. Consider alternative data quality checks for small datasets.")
                concern_level <- .("Unreliable (Small Sample)")
            } else {
                # Clinical interpretation based on suspicion rate (only for adequate sample sizes)
                if (suspicion_rate < 5) {
                    clinical_interpretation <- .("Data follows expected Benford's Law pattern. Low concern for data quality issues.")
                    recommendation <- .("Data appears reliable. No immediate action required.")
                    concern_level <- .("Low")
                } else if (suspicion_rate < 15) {
                    clinical_interpretation <- .("Moderate deviation from expected pattern. Review recommended.")
                    recommendation <- .("Consider reviewing data collection and entry procedures.")
                    concern_level <- .("Moderate")
                } else {
                    clinical_interpretation <- .("High deviation from expected pattern. Significant data quality concerns.")
                    recommendation <- .("Immediate review of data sources and validation procedures strongly recommended.")
                    concern_level <- .("High")
                }
            }

            return(list(
                total_observations = n_total,
                valid_observations = n_total,
                suspicious_count = n_suspects,
                suspicion_rate = suspicion_rate,
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
                <p><strong>{interpret_title}</strong> {interpret_text}</p>
                <p><strong>{action_title}</strong> {action_text}</p>
            </div>
            ",
            title = .("Understanding Benford's Law Analysis"),
            what_title = .("What it does:"),
            what_text = .("Analyzes the distribution of first digits in your data to detect unusual patterns that may indicate data quality issues, entry errors, or fraud."),
            when_title = .("When to use:"),
            when_text = .("Use with naturally occurring numerical data (lab values, measurements, counts) that span multiple orders of magnitude. Not suitable for artificial ranges or categorical data."),
            interpret_title = .("How to interpret:"),
            interpret_text = .("Natural datasets typically follow Benford's Law where digit '1' appears about 30% of the time, '2' about 18%, etc. Significant deviations may indicate problems."),
            action_title = .("What to do with results:"),
            action_text = .("High deviation rates (>15%) warrant investigation of data collection procedures. Individual suspicious values should be verified against source documents.")
            )
            return(explanation)
        },
        
        .generateReportSentence = function(interpretation_results, digits) {
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
            title = .("Clinical Summary"),
            summary = glue::glue(.("Benford's Law analysis of {n} observations using {d}-digit analysis found {suspects} suspicious data points ({rate}% of total), indicating {level} concern for data quality issues."),
                               n = interpretation_results$total_observations,
                               d = digits,
                               suspects = interpretation_results$suspicious_count,
                               rate = interpretation_results$suspicion_rate,
                               level = tolower(interpretation_results$concern_level)),
            recommendation = interpretation_results$recommendation
            )
            return(report)
        },
        
        .run = function() {
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
            valid_count <- sum(!is.na(var))
            
            if (valid_count < 100) {
                warning_title <- .("Warning:")
                warning_msg <- .("Small sample size detected. Results may be less reliable with fewer than 100 observations.")
                warning_div <- paste0("<div style='color: #856404; margin-top: 10px;'><strong>", 
                                     warning_title, "</strong> ", warning_msg, "</div>")
                guidelines <- paste(guidelines, warning_div)
                self$results$todo$setContent(guidelines)
            }
            
            # Get number of digits parameter (with default)
            digits <- self$options$digits %||% 2
            
            # Perform Benford analysis with error handling
            tryCatch({
                # Run Benford analysis
                bfd.cp <- benford.analysis::benford(data = var, 
                                                   number.of.digits = digits)
                
                # Store detailed results (hidden by default)
                self$results$text$setContent(bfd.cp)
                
                # Get suspects - CRITICAL: Only extract the selected variable to prevent PHI leakage
                # getSuspects returns the entire data frame with all columns, which could expose PHI
                # We only need the selected variable values, not patient IDs, dates, etc.
                var_name <- self$options$var
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
                    colnames(suspects_safe) <- c(.("Row"), var_name)
                } else {
                    suspects_safe <- NULL
                }

                # Format suspects output with clinical context
                if (!is.null(suspects_safe) && nrow(suspects_safe) > 0) {
                    suspects_text <- glue::glue("
                    {header}

                    {found_text} {n_suspects} {suspicious_text} {total_text} {n_total} {observations_text}

                    {details_header}
                    {suspects_output}

                    {security_note}
                    {note_text}
                    ",
                    header = .("SUSPICIOUS DATA POINTS IDENTIFIED"),
                    found_text = .("Found"),
                    n_suspects = nrow(suspects_safe),
                    suspicious_text = .("suspicious data points out of"),
                    total_text = "",
                    n_total = valid_count,
                    observations_text = .("total observations"),
                    details_header = .("Details:"),
                    suspects_output = paste(capture.output(print(suspects_safe, row.names = FALSE)), collapse = "\n"),
                    security_note = .("(Only showing selected variable values for privacy protection)"),
                    note_text = .("Note: These points deviate significantly from expected Benford's Law patterns and should be verified against source data.")
                    )
                } else {
                    suspects_text <- .("No suspicious data points identified. Data follows expected Benford's Law distribution.")
                }
                
                self$results$text2$setContent(suspects_text)

                # Generate clinical interpretation with safe suspect data
                interpretation <- private$.interpretResults(bfd.cp, suspects_safe, var)
                
                # Populate summary table using addRow method
                self$results$summary$addRow(rowKey=1, values=list(
                    statistic=.("Total Observations"),
                    value=as.character(interpretation$total_observations),
                    interpretation=.("Number of data points analyzed")
                ))

                self$results$summary$addRow(rowKey=2, values=list(
                    statistic=.("Valid Observations"),
                    value=as.character(interpretation$valid_observations),
                    interpretation=.("Non-missing numeric values")
                ))

                self$results$summary$addRow(rowKey=3, values=list(
                    statistic=.("Suspicious Points"),
                    value=as.character(interpretation$suspicious_count),
                    interpretation=.("Data points deviating from expected pattern")
                ))

                self$results$summary$addRow(rowKey=4, values=list(
                    statistic=.("Suspicion Rate"),
                    value=paste0(interpretation$suspicion_rate, "%"),
                    interpretation=.("Percentage of suspicious observations")
                ))

                self$results$summary$addRow(rowKey=5, values=list(
                    statistic=.("Concern Level"),
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