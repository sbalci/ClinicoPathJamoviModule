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
                stop(.("Data contains no (complete) rows"))
            }
            
            # Enhanced validation with clinical guidance
            var_data <- jmvcore::toNumeric(self$data[[self$options$var]])
            valid_count <- sum(!is.na(var_data))
            
            if (valid_count < 30) {
                stop(.("Insufficient data: Benford's Law requires at least 30 valid observations for meaningful results"))
            }
            
            return(TRUE)
        },
        
        .interpretResults = function(benford_obj, suspects_obj, var_data) {
            n_total <- length(var_data[!is.na(var_data)])
            n_suspects <- if (!is.null(suspects_obj) && nrow(suspects_obj) > 0) nrow(suspects_obj) else 0
            suspicion_rate <- round((n_suspects / n_total) * 100, 1)
            
            # Clinical interpretation based on suspicion rate
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
                
                # Get suspects
                suspects <- benford.analysis::getSuspects(bfd = bfd.cp, 
                                                         data = mydata)
                
                # Format suspects output with clinical context
                if (nrow(suspects) > 0) {
                    suspects_text <- glue::glue("
                    {header}
                    
                    {found_text} {n_suspects} {suspicious_text} {total_text} {n_total} {observations_text}
                    
                    {details_header}
                    {suspects_output}
                    
                    {note_text}
                    ",
                    header = .("SUSPICIOUS DATA POINTS IDENTIFIED"),
                    found_text = .("Found"),
                    n_suspects = nrow(suspects),
                    suspicious_text = .("suspicious data points out of"),
                    total_text = "",
                    n_total = valid_count,
                    observations_text = .("total observations"),
                    details_header = .("Details:"),
                    suspects_output = paste(capture.output(print(suspects)), collapse = "\n"),
                    note_text = .("Note: These points deviate significantly from expected Benford's Law patterns and should be verified against source data.")
                    )
                } else {
                    suspects_text <- .("No suspicious data points identified. Data follows expected Benford's Law distribution.")
                }
                
                self$results$text2$setContent(suspects_text)
                
                # Generate clinical interpretation
                interpretation <- private$.interpretResults(bfd.cp, suspects, var)
                
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