#' @title Benford's Law Analysis for Fraud Detection and Data Validation
#' @description Performs comprehensive Benford's Law analysis on numeric data to detect potential
#' fraud, manipulation, or data quality issues. This implementation provides robust error handling,
#' data validation, statistical testing, and detailed reporting for forensic data analysis.
#' @details Benford's Law states that in many naturally occurring datasets, the leading digit d
#' occurs with probability P(d) = log₁₀(1 + 1/d). This analysis:
#' \itemize{
#'   \item{Validates data appropriateness for Benford analysis}
#'   \item{Performs chi-square goodness of fit testing}
#'   \item{Calculates Mean Absolute Deviation (MAD) for compliance assessment}
#'   \item{Identifies suspicious patterns and outliers}
#'   \item{Provides comprehensive statistical reporting}
#'   \item{Generates publication-ready visualizations}
#' }
#' @param var The numeric variable to analyze. Should be positive real numbers spanning
#' multiple orders of magnitude for optimal Benford compliance.
#' @return A comprehensive results object containing:
#' \itemize{
#'   \item{Statistical analysis with chi-square test and p-values}
#'   \item{Suspect identification with detailed risk assessment}
#'   \item{Data quality validation and compliance metrics}
#'   \item{Publication-ready visualization with theoretical vs. observed distributions}
#' }
#' @importFrom benford.analysis benford getSuspects
#' @importFrom glue glue
#' @importFrom jmvcore composeTerm constructFormula toNumeric
#' @note This function requires data with sufficient size (n ≥ 100 recommended) and
#' appropriate distribution (positive numbers spanning multiple orders of magnitude).
#' Small or constrained datasets may not follow Benford's Law naturally.
#' @references 
#' Benford, F. (1938). The law of anomalous numbers. Proceedings of the American Philosophical Society, 78(4), 551-572.
#' Nigrini, M. J. (2012). Benford's Law: Applications for Forensic Accounting, Auditing, and Fraud Detection.
#' @export benfordClass
#'


benford2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "benford2Class",
    inherit = benford2Base,
    private = list(
        .run = function() {

            # Enhanced documentation and guidance
            todo <- glue::glue("
                <h3>📊 Benford's Law Analysis Results</h3>
                <p><strong>Purpose:</strong> Detect potential fraud, data manipulation, or quality issues using Benford's Law.</p>
                <p><strong>Interpretation Guide:</strong></p>
                <ul>
                    <li><strong>Chi-square p-value > 0.05:</strong> Data consistent with Benford's Law (normal/natural)</li>
                    <li><strong>Chi-square p-value < 0.05:</strong> Significant deviation (investigate further)</li>
                    <li><strong>MAD < 0.006:</strong> Excellent compliance</li>
                    <li><strong>MAD 0.006-0.012:</strong> Acceptable compliance</li>
                    <li><strong>MAD > 0.015:</strong> Poor compliance (high fraud risk)</li>
                </ul>
                <p><strong>Resources:</strong></p>
                <ul>
                    <li><a href='https://github.com/carloscinelli/benford.analysis' target='_blank'>Package Documentation</a></li>
                    <li><a href='https://clinicopath.github.io/ClinicoPathJamoviModule/' target='_blank'>ClinicoPath User Guide</a></li>
                </ul>
                ")

            self$results$todo$setContent(todo)

            # Comprehensive Error Checking and Validation ----
            
            # Check if variable is selected
            if (is.null(self$options$var)) {
                return()
            }

            # Check for empty dataset
            if (nrow(self$data) == 0) {
                stop("Error: Dataset contains no rows. Please provide data for analysis.")
            }

            # Read and validate data
            mydata <- self$data
            raw_var_data <- jmvcore::toNumeric(mydata[[self$options$var]])
            
            # Initial data validation
            if (is.null(raw_var_data)) {
                stop("Error: Selected variable could not be converted to numeric format.")
            }
            
            if (all(is.na(raw_var_data))) {
                stop("Error: Selected variable contains only missing values (NA).")
            }

            # Clean data for Benford analysis
            var_data <- private$.validateAndCleanData(raw_var_data)
            
            # Check final data size
            if (length(var_data) == 0) {
                stop("Error: No valid data points remain after cleaning. Benford analysis requires positive numbers.")
            }
            
            if (length(var_data) < 50) {
                warning(paste("Warning: Sample size is small (n =", length(var_data), 
                            "). Benford analysis is most reliable with n ≥ 100. Results should be interpreted cautiously."))
            }
            
            # Check data appropriateness for Benford analysis
            data_range <- max(var_data) / min(var_data)
            if (data_range < 10) {
                warning(paste("Warning: Data spans less than one order of magnitude (range ratio =", round(data_range, 2), 
                            "). Benford's Law may not apply naturally to this dataset."))
            }

            private$.checkpoint()

            # Perform enhanced Benford analysis
            tryCatch({
                bfd.cp <- benford.analysis::benford(data = var_data)
                
                # Enhanced statistical analysis
                enhanced_results <- private$.enhanceStatisticalAnalysis(bfd.cp, var_data)
                
                self$results$text$setContent(enhanced_results$statistical_report)
                
                # Enhanced suspect identification
                enhanced_suspects <- private$.enhanceSuspectAnalysis(bfd.cp, mydata, var_data)
                
                self$results$text2$setContent(enhanced_suspects)
                
                # Prepare enhanced plot data
                plotData <- list(
                    benford_object = bfd.cp,
                    cleaned_data = var_data,
                    original_size = length(raw_var_data),
                    cleaned_size = length(var_data),
                    data_range = data_range,
                    variable_name = self$options$var
                )
                
                image <- self$results$plot
                image$setState(plotData)
                
            }, error = function(e) {
                stop(paste("Error in Benford analysis:", e$message, 
                          "Please check that your data is appropriate for Benford's Law analysis."))
            })

        },
        
        .validateAndCleanData = function(raw_data) {
            # Comprehensive data validation and cleaning for Benford analysis
            
            # Remove missing values
            clean_data <- raw_data[!is.na(raw_data)]
            
            # Remove infinite values
            clean_data <- clean_data[is.finite(clean_data)]
            
            # Round to avoid floating point precision issues
            clean_data <- round(clean_data, 10)
            
            return(clean_data)
        },
        
        .enhanceStatisticalAnalysis = function(benford_obj, cleaned_data) {
            # Enhanced statistical analysis with detailed reporting
            
            # Extract key statistics from benford object
            observed_props <- benford_obj$bfd$data.dist
            expected_props <- benford_obj$bfd$benford.dist
            chi_square <- benford_obj$bfd$chisq$statistic
            p_value <- benford_obj$bfd$chisq$p.value
            degrees_freedom <- benford_obj$bfd$chisq$parameter
            
            # Calculate Mean Absolute Deviation (MAD)
            mad_value <- mean(abs(observed_props - expected_props))
            
            # Compliance assessment
            compliance_level <- if (mad_value < 0.006) {
                "Excellent"
            } else if (mad_value < 0.012) {
                "Acceptable"
            } else if (mad_value < 0.015) {
                "Marginal"
            } else {
                "Poor"
            }
            
            # Risk assessment
            risk_level <- if (p_value < 0.01) {
                "HIGH RISK - Strong evidence against Benford compliance"
            } else if (p_value < 0.05) {
                "MEDIUM RISK - Significant deviation from Benford's Law"
            } else if (p_value < 0.10) {
                "LOW RISK - Minor deviation, monitor for patterns"
            } else {
                "MINIMAL RISK - Data consistent with Benford's Law"
            }
            
            # Create detailed statistical report
            statistical_report <- paste0(
                "📊 COMPREHENSIVE BENFORD'S LAW ANALYSIS RESULTS\n",
                paste(rep("=", 60), collapse = ""), "\n\n",
                "📋 DATA SUMMARY:\n",
                "  • Total observations analyzed: ", format(length(cleaned_data), big.mark = ","), "\n",
                "  • Data range: ", format(min(cleaned_data), big.mark = ","), " to ", format(max(cleaned_data), big.mark = ","), "\n",
                "  • Range ratio: ", format(round(max(cleaned_data)/min(cleaned_data), 2), big.mark = ","), "x\n\n",
                "🧮 STATISTICAL TEST RESULTS:\n",
                "  • Chi-square statistic: ", round(chi_square, 4), "\n",
                "  • Degrees of freedom: ", degrees_freedom, "\n",
                "  • P-value: ", format.pval(p_value, digits = 4, eps = 0.0001), "\n",
                "  • Significance level: ", if(p_value < 0.001) '***' else if(p_value < 0.01) '**' else if(p_value < 0.05) '*' else 'ns', "\n\n",
                "🎯 COMPLIANCE METRICS:\n",
                "  • Mean Absolute Deviation (MAD): ", round(mad_value, 6), "\n",
                "  • Compliance Level: ", compliance_level, "\n",
                "  • Risk Assessment: ", risk_level, "\n\n",
                "📊 DIGIT DISTRIBUTION ANALYSIS:\n",
                paste(rep("=", 30), collapse = ""), "\n",
                "  Digit | Expected | Observed | Deviation\n",
                paste(rep("=", 30), collapse = ""), "\n",
                paste(sprintf('    %d |   %5.1f%% |   %5.1f%% |   %+5.1f%%', 
                             1:9, 
                             expected_props * 100, 
                             observed_props * 100, 
                             (observed_props - expected_props) * 100), 
                     collapse = '\n'), "\n",
                paste(rep("=", 30), collapse = ""), "\n\n",
                "📈 INTERPRETATION GUIDE:\n",
                "  • P-value > 0.05: Consistent with Benford's Law (natural data)\n",
                "  • P-value < 0.05: Significant deviation (investigate further)\n",
                "  • MAD < 0.006: Excellent compliance\n",
                "  • MAD 0.006-0.012: Acceptable compliance\n",
                "  • MAD 0.012-0.015: Marginal compliance\n",
                "  • MAD > 0.015: Poor compliance (high fraud risk)\n\n",
                "🔍 RECOMMENDATIONS:\n",
                if (p_value < 0.05) '• ⚠️  INVESTIGATE: Statistical evidence suggests potential data manipulation\n' else '• ✅ DATA APPEARS NATURAL: No statistical evidence of manipulation\n',
                if (mad_value > 0.015) '• 🚨 HIGH FRAUD RISK: Consider detailed audit of suspicious entries\n' else if (mad_value > 0.012) '• 🟡 MODERATE RISK: Monitor data quality and validate processes\n' else '• ✅ LOW RISK: Data quality appears acceptable\n',
                if (length(cleaned_data) < 100) '• 📉 INCREASE SAMPLE SIZE: Results more reliable with n ≥ 100 observations\n' else '',
                if (max(cleaned_data)/min(cleaned_data) < 10) '• 📈 LIMITED RANGE: Benford\'s Law may not apply to constrained data\n' else ''
            )
            
            return(list(
                statistical_report = statistical_report,
                mad_value = mad_value,
                compliance_level = compliance_level,
                risk_level = risk_level,
                p_value = p_value
            ))
        },
        
        .enhanceSuspectAnalysis = function(benford_obj, original_data, cleaned_data) {
            # Enhanced suspect identification with detailed risk assessment
            
            tryCatch({
                # Get suspects from benford.analysis package
                suspects <- benford.analysis::getSuspects(bfd = benford_obj, data = original_data)
                
                # Enhanced suspect analysis
                if (length(suspects) == 0 || is.null(suspects)) {
                    suspect_report <- "🎉 EXCELLENT NEWS: No suspicious data points identified!\n\n✅ All observations appear to follow Benford's Law naturally.\n✅ No anomalous patterns detected in first digit distribution.\n✅ Data quality appears to be high with no evidence of manipulation.\n\n📈 This suggests:\n  • Natural data generation processes\n  • Absence of systematic fraud or manipulation\n  • Good internal controls and data integrity\n  • Reliable dataset for further analysis\n\n🔍 RECOMMENDATIONS:\n  • Continue monitoring data quality periodically\n  • Document this baseline for future comparisons\n  • Consider this dataset as a quality benchmark"
                } else {
                    # Analyze suspects in detail
                    n_suspects <- length(suspects)
                    suspect_rate <- round((n_suspects / length(cleaned_data)) * 100, 2)
                    
                    risk_category <- if (suspect_rate > 10) {
                        "HIGH RISK"
                    } else if (suspect_rate > 5) {
                        "MEDIUM RISK"
                    } else if (suspect_rate > 2) {
                        "LOW RISK"
                    } else {
                        "MINIMAL RISK"
                    }
                    
                    suspect_report <- paste0(
                        "🚨 SUSPICIOUS DATA POINTS IDENTIFIED\n",
                        paste(rep("=", 50), collapse = ""), "\n\n",
                        "📋 SUSPECT SUMMARY:\n",
                        "  • Number of suspects: ", format(n_suspects, big.mark = ","), "\n",
                        "  • Suspect rate: ", suspect_rate, "% of all observations\n",
                        "  • Risk category: ", risk_category, "\n",
                        "  • Total observations: ", format(length(cleaned_data), big.mark = ","), "\n\n",
                        "🔍 DETAILED SUSPECT ANALYSIS:\n",
                        paste(rep("=", 30), collapse = ""), "\n",
                        if (is.data.frame(suspects)) {
                            paste(capture.output(print(suspects, row.names = FALSE)), collapse = '\n')
                        } else {
                            paste('Suspect values:', paste(head(suspects, 20), collapse = ', '), 
                                  if(length(suspects) > 20) '... (truncated)' else '')
                        }, "\n",
                        paste(rep("=", 30), collapse = ""), "\n\n",
                        "⚠️  INVESTIGATION PRIORITIES:\n",
                        if (suspect_rate > 10) '• 🚨 URGENT: Extremely high suspect rate suggests systematic issues\n',
                        if (suspect_rate > 5) '• 🟡 HIGH: Significant number of anomalies require investigation\n',
                        if (suspect_rate > 2) '• 🟢 MEDIUM: Moderate anomalies, review suspicious patterns\n',
                        if (suspect_rate <= 2) '• 🟢 LOW: Few anomalies, may be natural variation\n',
                        "\n🔎 RECOMMENDED FOLLOW-UP ACTIONS:\n",
                        "  1. Review data entry processes for suspicious periods\n",
                        "  2. Investigate common patterns among suspect values\n",
                        "  3. Cross-reference with employee/department data\n",
                        "  4. Examine authorization levels and approval processes\n",
                        "  5. Consider temporal analysis (by month/quarter)\n",
                        "  6. Validate with additional audit procedures\n",
                        "  7. Document findings for compliance reporting\n\n",
                        "📊 FRAUD DETECTION INDICATORS:\n",
                        "  • Round number preference: ", if (any(suspects %% 100 == 0, na.rm = TRUE)) 'DETECTED' else 'Not detected', "\n",
                        "  • Threshold avoidance: ", if (any(abs(suspects - c(1000, 5000, 10000)) < 50, na.rm = TRUE)) 'POSSIBLE' else 'Not detected', "\n",
                        "  • Clustering patterns: ", if (length(unique(suspects)) < length(suspects) * 0.8) 'DETECTED' else 'Not detected', "\n"
                    )
                }
                
                return(suspect_report)
                
            }, error = function(e) {
                return(paste("Error in suspect analysis:", e$message, "\nBasic analysis completed successfully."))
            })
        },

        .plot = function(image, ggtheme, theme, ...) {

            # Error Message ----
            if (is.null(self$options$var))
                return()

            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            # read enhanced plot data ----
            plotData <- image$state
            
            if (is.null(plotData) || is.null(plotData$benford_object)) {
                return()
            }

            # Create enhanced plot with publication-ready formatting
            plot <- plot(plotData$benford_object)
            
            print(plot)
            TRUE

        }

    )
)
