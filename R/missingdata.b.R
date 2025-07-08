#' @title Missing Data Analysis and Multiple Imputation
#' @description
#' Comprehensive missing data analysis and multiple imputation using mice and ggmice packages.
#' This function provides a complete workflow for analyzing missing data patterns, performing
#' multiple imputation by chained equations (MICE), and evaluating imputation quality.
#' Designed specifically for clinical research applications where missing data is common
#' and proper handling is critical for valid statistical inference.
#'
#' @details
#' The missing data analysis function provides three main analysis types:
#' 1. **Pattern Analysis**: Explores missing data structure and patterns
#' 2. **Multiple Imputation**: Performs MICE imputation with convergence diagnostics
#' 3. **Complete Analysis**: Combines pattern analysis and imputation
#'
#' Key features include:
#' - Visual and tabular missing data pattern analysis
#' - Multiple imputation methods (PMM, Bayesian regression, logistic regression)
#' - Convergence diagnostics with trace plots
#' - Quality evaluation comparing observed vs imputed data
#' - Flexible parameter customization
#' - Clinical research focused interpretations
#'
#' Common clinical applications:
#' - Data quality assessment for clinical trials
#' - Missing data handling in observational studies
#' - Regulatory compliance for pharmaceutical research
#' - Sensitivity analysis for missing data assumptions
#'
#' @examples
#' \dontrun{
#' # Basic pattern analysis
#' result <- missingdata(
#'   data = clinical_data,
#'   analysis_vars = c("age", "bmi", "biomarker"),
#'   analysis_type = "pattern"
#' )
#'
#' # Multiple imputation
#' result <- missingdata(
#'   data = clinical_data,
#'   analysis_vars = c("age", "bmi", "biomarker"),
#'   analysis_type = "imputation",
#'   n_imputations = 10,
#'   imputation_method = "pmm"
#' )
#'
#' # Complete analysis
#' result <- missingdata(
#'   data = clinical_data,
#'   analysis_vars = c("age", "bmi", "biomarker"),
#'   analysis_type = "complete",
#'   n_imputations = 5,
#'   max_iterations = 10
#' )
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom mice mice md.pattern flux complete
#' @importFrom ggmice plot_pattern plot_corr plot_flux plot_trace
#' @importFrom ggmice ggmice densityplot stripplot xyplot
#' @importFrom ggplot2 ggplot aes labs theme_minimal ggtitle
#' @importFrom dplyr summarise group_by mutate select
#' @importFrom htmltools HTML

# Null-coalescing operator helper (defined at top for better organization)
`%||%` <- function(x, y) if (is.null(x)) y else x

missingdataClass <- if (requireNamespace("jmvcore")) R6::R6Class("missingdataClass",
    inherit = missingdataBase,
    private = list(

        # Store imputation results for reuse
        .mice_results = NULL,
        .original_data = NULL,
        .analysis_metadata = NULL,
        .performance_metrics = NULL,

        .run = function() {

            # Check if variables are selected
            if (is.null(self$options$analysis_vars) || length(self$options$analysis_vars) == 0) {
                
                intro_msg <- "
                <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #2e7d32; margin-top: 0;'>🔍 Welcome to Missing Data Analysis!</h3>
                <p><strong>Comprehensive missing data analysis and multiple imputation</strong></p>
                <p>Analyze missing data patterns and perform multiple imputation using mice and ggmice packages</p>
                
                <h4 style='color: #2e7d32;'>Required Variables:</h4>
                <ol>
                <li><strong>Variables for Analysis:</strong> Select variables to include in missing data analysis</li>
                </ol>
                
                <h4 style='color: #2e7d32;'>Analysis Types Available:</h4>
                <ul>
                <li><strong>Missing Data Pattern Analysis:</strong> Explore missing data structure and patterns</li>
                <li><strong>Multiple Imputation:</strong> Perform MICE imputation with convergence diagnostics</li>
                <li><strong>Complete Analysis:</strong> Combined pattern analysis and imputation</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Key Features:</h4>
                <ul>
                <li><strong>Pattern Visualization:</strong> Visual and tabular missing data pattern analysis</li>
                <li><strong>Correlation Analysis:</strong> Understand relationships between incomplete variables</li>
                <li><strong>Multiple Imputation:</strong> MICE algorithm with customizable parameters</li>
                <li><strong>Convergence Diagnostics:</strong> Trace plots and convergence assessment</li>
                <li><strong>Quality Evaluation:</strong> Compare observed vs imputed data distributions</li>
                <li><strong>Flexible Methods:</strong> Auto-selection or manual specification of imputation methods</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Clinical Research Applications:</h4>
                <ul>
                <li><strong>Data Quality Assessment:</strong> Understand missing data mechanisms</li>
                <li><strong>Study Design:</strong> Plan for missing data in prospective studies</li>
                <li><strong>Statistical Analysis:</strong> Proper handling of missing data for valid inference</li>
                <li><strong>Regulatory Compliance:</strong> Meet standards for missing data handling</li>
                <li><strong>Sensitivity Analysis:</strong> Assess impact of different imputation strategies</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Imputation Methods:</h4>
                <ul>
                <li><strong>Predictive Mean Matching (PMM):</strong> Robust for continuous variables</li>
                <li><strong>Bayesian Regression:</strong> Parametric approach for normal data</li>
                <li><strong>Logistic Regression:</strong> For binary variables</li>
                <li><strong>Polytomous Regression:</strong> For categorical variables</li>
                <li><strong>Auto-selection:</strong> Automatic method selection based on variable types</li>
                </ul>
                
                <h4 style='color: #2e7d32;'>Output Visualizations:</h4>
                <ul>
                <li><strong>Pattern Plots:</strong> Visual representation of missing data patterns</li>
                <li><strong>Correlation Matrices:</strong> Relationships between variables</li>
                <li><strong>Flux Analysis:</strong> Influx and outflux of missing patterns</li>
                <li><strong>Trace Plots:</strong> Convergence diagnostics for MICE algorithm</li>
                <li><strong>Distribution Comparisons:</strong> Observed vs imputed data distributions</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Professional missing data analysis for clinical research data quality and statistical validity</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no rows. Please check your data and try again.")
            }

            # Enhanced package dependency checking
            package_status <- private$.check_package_dependencies()
            if (!package_status$all_available) {
                self$results$interpretation$setContent(package_status$error_message)
                return()
            }

            # Enhanced data validation and preparation
            validation_result <- private$.validate_and_prepare_data()
            if (!validation_result$valid) {
                self$results$pattern_table$setContent(validation_result$error_message)
                return()
            }
            
            analysis_data <- validation_result$data
            private$.original_data <- analysis_data
            
            # Store analysis metadata
            private$.analysis_metadata <- list(
                n_observations = nrow(analysis_data),
                n_variables = ncol(analysis_data),
                variable_names = names(analysis_data),
                variable_types = sapply(analysis_data, class),
                missing_counts = sapply(analysis_data, function(x) sum(is.na(x))),
                analysis_timestamp = Sys.time(),
                analysis_type = self$options$analysis_type,
                seed_value = self$options$seed_value
            )

            # Generate pattern analysis outputs
            if (self$options$analysis_type %in% c("pattern", "complete")) {
                if (self$options$show_pattern_table) {
                    pattern_html <- private$.generate_pattern_table(analysis_data)
                    self$results$pattern_table$setContent(pattern_html)
                }
            }

            # Perform imputation if requested
            if (self$options$analysis_type %in% c("imputation", "complete")) {
                
                # Set random seed for reproducibility
                set.seed(self$options$seed_value)
                
                # Prepare imputation
                mice_results <- private$.perform_imputation(analysis_data)
                private$.mice_results <- mice_results
                
                if (!is.null(mice_results)) {
                    if (self$options$show_imputation_summary) {
                        summary_html <- private$.generate_imputation_summary(mice_results)
                        self$results$imputation_summary$setContent(summary_html)
                    }
                }
            }

            # Generate interpretation guide
            if (self$options$show_interpretation) {
                interpretation_html <- private$.generate_interpretation_guide()
                self$results$interpretation$setContent(interpretation_html)
            }

        },

        .plot_pattern = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.original_data)) {
                return()
            }

            if (!requireNamespace("ggmice", quietly = TRUE)) {
                return()
            }

            tryCatch({
                plot <- ggmice::plot_pattern(private$.original_data) +
                    ggplot2::ggtitle("Missing Data Pattern") +
                    ggtheme
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_correlation = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.original_data)) {
                return()
            }

            if (!requireNamespace("ggmice", quietly = TRUE)) {
                return()
            }

            tryCatch({
                plot <- ggmice::plot_corr(private$.original_data) +
                    ggplot2::ggtitle("Variable Correlations") +
                    ggtheme
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_flux = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.original_data)) {
                return()
            }

            if (!requireNamespace("ggmice", quietly = TRUE)) {
                return()
            }

            tryCatch({
                plot <- ggmice::plot_flux(private$.original_data) +
                    ggplot2::ggtitle("Missing Data Influx/Outflux") +
                    ggtheme
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_trace = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.mice_results)) {
                return()
            }

            if (!requireNamespace("ggmice", quietly = TRUE)) {
                return()
            }

            tryCatch({
                plot <- ggmice::plot_trace(private$.mice_results) +
                    ggplot2::ggtitle("MICE Convergence Diagnostics") +
                    ggtheme
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_density = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.mice_results)) {
                return()
            }

            if (!requireNamespace("ggmice", quietly = TRUE)) {
                return()
            }

            tryCatch({
                plot <- ggmice::ggmice(private$.mice_results, 
                                      ggplot2::aes(x = .value, group = .imp)) +
                    ggmice::geom_density() +
                    ggplot2::ggtitle("Observed vs Imputed Data Distributions") +
                    ggtheme
                
                print(plot)
                TRUE
            }, error = function(e) {
                # Fallback to densityplot if ggmice doesn't work
                plot <- ggmice::densityplot(private$.mice_results) +
                    ggplot2::ggtitle("Density Comparison: Observed vs Imputed") +
                    ggtheme
                
                print(plot)
                TRUE
            })
        },

        .plot_strip = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.mice_results)) {
                return()
            }

            if (!requireNamespace("ggmice", quietly = TRUE)) {
                return()
            }

            tryCatch({
                plot <- ggmice::stripplot(private$.mice_results) +
                    ggplot2::ggtitle("Strip Plot: Observed vs Imputed Values") +
                    ggtheme
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .plot_scatter = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.mice_results)) {
                return()
            }

            if (!requireNamespace("ggmice", quietly = TRUE)) {
                return()
            }

            tryCatch({
                plot <- ggmice::xyplot(private$.mice_results) +
                    ggplot2::ggtitle("Scatter Plot: Variable Relationships") +
                    ggtheme
                
                print(plot)
                TRUE
            }, error = function(e) {
                return()
            })
        },

        .perform_imputation = function(data) {
            
            tryCatch({
                # Start performance monitoring
                start_time <- private$.monitor_performance("imputation")
                
                # Determine imputation methods
                if (self$options$imputation_method == "auto") {
                    # Let mice auto-select methods
                    method_spec <- NULL
                } else {
                    # Use specified method for all variables
                    method_spec <- rep(self$options$imputation_method, ncol(data))
                    names(method_spec) <- names(data)
                }
                
                # Enhanced parameter validation
                n_imputations <- max(1, min(50, self$options$n_imputations))
                max_iterations <- max(1, min(100, self$options$max_iterations))
                
                # Perform MICE imputation with enhanced error handling
                mice_result <- mice::mice(
                    data = data,
                    m = n_imputations,
                    maxit = max_iterations,
                    method = method_spec,
                    seed = self$options$seed_value,
                    printFlag = FALSE
                )
                
                # End performance monitoring
                imputation_duration <- private$.monitor_performance("imputation", start_time)
                
                # Store imputation metadata
                mice_result$analysis_metadata <- list(
                    duration_seconds = imputation_duration,
                    n_variables_imputed = sum(mice_result$method != ""),
                    convergence_achieved = TRUE,  # We'll enhance this later
                    imputation_timestamp = Sys.time()
                )
                
                return(mice_result)
                
            }, error = function(e) {
                error_msg <- paste0(
                    "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
                    "<h4>❌ Imputation Error</h4>",
                    "<p><strong>Error message:</strong> ", e$message, "</p>",
                    "<p><strong>Possible solutions:</strong></p>",
                    "<ul>",
                    "<li>Try different imputation methods</li>",
                    "<li>Reduce the number of imputations</li>",
                    "<li>Check for perfect collinearity between variables</li>",
                    "<li>Remove variables with excessive missing data</li>",
                    "</ul></div>"
                )
                self$results$imputation_summary$setContent(error_msg)
                return(NULL)
            })
        },

        .generate_pattern_table = function(data) {
            
            tryCatch({
                # Generate missing pattern table
                pattern_matrix <- mice::md.pattern(data, plot = FALSE)
                
                # Calculate missing data statistics
                n_total <- nrow(data)
                n_complete <- sum(complete.cases(data))
                n_incomplete <- n_total - n_complete
                pct_incomplete <- round(n_incomplete / n_total * 100, 1)
                
                # Variable-wise missing counts
                var_missing <- colSums(is.na(data))
                var_pct_missing <- round(var_missing / n_total * 100, 1)
                
                # Create HTML table
                pattern_html <- paste0(
                    "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px;'>",
                    "<h3 style='color: #495057; margin-top: 0;'>📊 Missing Data Pattern Analysis</h3>",
                    
                    "<h4 style='color: #495057;'>Overall Summary:</h4>",
                    "<table style='width: 100%; border-collapse: collapse; margin-bottom: 20px;'>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Total Observations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_total, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Complete Cases:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_complete, " (", round(n_complete/n_total*100, 1), "%)</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Incomplete Cases:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_incomplete, " (", pct_incomplete, "%)</td></tr>",
                    "</table>",
                    
                    "<h4 style='color: #495057;'>Variable-wise Missing Data:</h4>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<thead><tr style='background-color: #6c757d; color: white;'>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Variable</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Missing Count</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Missing %</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Complete Count</th>",
                    "</tr></thead><tbody>"
                )
                
                for (i in 1:length(var_missing)) {
                    var_name <- names(var_missing)[i]
                    missing_count <- var_missing[i]
                    missing_pct <- var_pct_missing[i]
                    complete_count <- n_total - missing_count
                    
                    row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                    if (missing_pct > 50) row_bg <- "#ffebee"  # Highlight high missing
                    
                    pattern_html <- paste0(pattern_html,
                        "<tr style='background-color: ", row_bg, ";'>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>", var_name, "</strong></td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", missing_count, "</td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", missing_pct, "%</td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", complete_count, "</td>",
                        "</tr>"
                    )
                }
                
                pattern_html <- paste0(pattern_html,
                    "</tbody></table>",
                    "</div>"
                )
                
                return(pattern_html)
                
            }, error = function(e) {
                return("<p>Error generating missing data pattern table.</p>")
            })
        },

        .generate_imputation_summary = function(mice_result) {
            
            tryCatch({
                # Extract imputation information
                n_imputations <- mice_result$m
                n_iterations <- mice_result$iteration
                n_vars <- ncol(mice_result$data)
                n_obs <- nrow(mice_result$data)
                
                # Get imputation methods used
                methods_used <- mice_result$method
                methods_used <- methods_used[methods_used != ""]
                
                summary_html <- paste0(
                    "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px;'>",
                    "<h3 style='color: #1976d2; margin-top: 0;'>📈 Multiple Imputation Summary</h3>",
                    
                    "<h4 style='color: #1976d2;'>Imputation Parameters:</h4>",
                    "<table style='width: 100%; border-collapse: collapse; margin-bottom: 15px;'>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Number of Imputations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_imputations, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>MICE Iterations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_iterations, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Variables Imputed:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", length(methods_used), " of ", n_vars, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Observations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_obs, "</td></tr>",
                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Random Seed:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", self$options$seed_value, "</td></tr>",
                    "</table>",
                    
                    "<h4 style='color: #1976d2;'>Imputation Methods Used:</h4>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<thead><tr style='background-color: #1976d2; color: white;'>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Variable</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Method</th>",
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>Description</th>",
                    "</tr></thead><tbody>"
                )
                
                # Method descriptions
                method_descriptions <- list(
                    "pmm" = "Predictive Mean Matching",
                    "norm" = "Bayesian Linear Regression",
                    "logreg" = "Logistic Regression",
                    "polyreg" = "Polytomous Logistic Regression",
                    "polr" = "Proportional Odds Model"
                )
                
                for (var_name in names(methods_used)) {
                    method <- methods_used[var_name]
                    description <- method_descriptions[[method]] %||% method
                    
                    summary_html <- paste0(summary_html,
                        "<tr>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>", var_name, "</strong></td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", method, "</td>",
                        "<td style='padding: 8px; border: 1px solid #dee2e6;'>", description, "</td>",
                        "</tr>"
                    )
                }
                
                summary_html <- paste0(summary_html,
                    "</tbody></table>",
                    
                    "<h4 style='color: #1976d2;'>Next Steps:</h4>",
                    "<ul>",
                    "<li>Review convergence diagnostics in the trace plot</li>",
                    "<li>Compare observed vs imputed distributions</li>",
                    "<li>Validate imputation quality with domain knowledge</li>",
                    "<li>Use imputed datasets for subsequent analysis</li>",
                    "</ul>",
                    "</div>"
                )
                
                return(summary_html)
                
            }, error = function(e) {
                return("<p>Error generating imputation summary.</p>")
            })
        },

        .generate_interpretation_guide = function() {
            
            analysis_type <- self$options$analysis_type
            
            interpretation_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📚 Missing Data Analysis Guide</h3>",
                
                "<h4 style='color: #7b1fa2;'>Understanding Missing Data Mechanisms:</h4>",
                "<ul>",
                "<li><strong>Missing Completely at Random (MCAR):</strong> Missing data is unrelated to observed or unobserved variables</li>",
                "<li><strong>Missing at Random (MAR):</strong> Missing data can be explained by observed variables</li>",
                "<li><strong>Missing Not at Random (MNAR):</strong> Missing data depends on unobserved values</li>",
                "</ul>",
                
                "<h4 style='color: #7b1fa2;'>Multiple Imputation Advantages:</h4>",
                "<ul>",
                "<li><strong>Preserves Sample Size:</strong> Uses all available data rather than complete case analysis</li>",
                "<li><strong>Accounts for Uncertainty:</strong> Multiple imputations capture imputation uncertainty</li>",
                "<li><strong>Unbiased Estimates:</strong> Provides valid statistical inference under MAR assumption</li>",
                "<li><strong>Flexible Methods:</strong> Different imputation methods for different variable types</li>",
                "</ul>",
                
                "<h4 style='color: #7b1fa2;'>Interpreting Visualizations:</h4>",
                "<ul>",
                "<li><strong>Pattern Plot:</strong> Shows which combinations of variables have missing data</li>",
                "<li><strong>Correlation Plot:</strong> Identifies variables that might predict missingness</li>",
                "<li><strong>Trace Plot:</strong> Assesses MICE algorithm convergence - should show stable, overlapping lines</li>",
                "<li><strong>Density Plot:</strong> Compares distributions - imputed data should be plausible</li>",
                "</ul>"
            )
            
            if (analysis_type %in% c("imputation", "complete")) {
                interpretation_html <- paste0(interpretation_html,
                    "<h4 style='color: #7b1fa2;'>Convergence Assessment:</h4>",
                    "<ul>",
                    "<li><strong>Good Convergence:</strong> Trace lines overlap and show no trends</li>",
                    "<li><strong>Poor Convergence:</strong> Distinct chains, trending patterns, or non-overlapping lines</li>",
                    "<li><strong>Solutions:</strong> Increase iterations, change imputation methods, or check model specification</li>",
                    "</ul>",
                    
                    "<h4 style='color: #7b1fa2;'>Quality Checks:</h4>",
                    "<ul>",
                    "<li><strong>Plausibility:</strong> Imputed values should be realistic and within expected ranges</li>",
                    "<li><strong>Distributional Similarity:</strong> Imputed data should follow similar patterns to observed data</li>",
                    "<li><strong>Logical Consistency:</strong> Relationships between variables should be preserved</li>",
                    "<li><strong>Domain Knowledge:</strong> Imputed values should make clinical/scientific sense</li>",
                    "</ul>"
                )
            }
            
            interpretation_html <- paste0(interpretation_html,
                "<h4 style='color: #7b1fa2;'>Clinical Research Best Practices:</h4>",
                "<ul>",
                "<li><strong>Missing Data Plan:</strong> Pre-specify missing data handling in study protocols</li>",
                "<li><strong>Sensitivity Analysis:</strong> Compare results across different imputation strategies</li>",
                "<li><strong>Reporting:</strong> Describe missing data patterns and imputation methods used</li>",
                "<li><strong>Validation:</strong> Check imputation quality with domain experts</li>",
                "</ul>",
                
                "<h4 style='color: #7b1fa2;'>When to Use Multiple Imputation:</h4>",
                "<ul>",
                "<li><strong>Recommended:</strong> Missing data >5% and MAR assumption reasonable</li>",
                "<li><strong>Alternative Approaches:</strong> Complete case analysis (if MCAR), model-based methods</li>",
                "<li><strong>Avoid:</strong> Simple imputation methods (mean/mode) for analysis datasets</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #7b1fa2; margin-top: 15px;'>",
                "<em>🔍 Professional missing data analysis powered by mice and ggmice packages</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        },
        
        # Enhanced package dependency checking
        .check_package_dependencies = function() {
            required_packages <- c("mice", "ggmice")
            missing_packages <- c()
            version_issues <- c()
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                } else {
                    # Check for minimum version compatibility
                    if (pkg == "mice") {
                        tryCatch({
                            mice_version <- packageVersion("mice")
                            if (mice_version < "3.0.0") {
                                version_issues <- c(version_issues, 
                                    paste0(pkg, " (", mice_version, ") - requires >= 3.0.0"))
                            }
                        }, error = function(e) {
                            version_issues <- c(version_issues, paste0(pkg, " (version check failed)"))
                        })
                    }
                }
            }
            
            if (length(missing_packages) > 0 || length(version_issues) > 0) {
                error_msg <- paste0(
                    "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
                    "<h4>📦 Package Dependencies Required</h4>"
                )
                
                if (length(missing_packages) > 0) {
                    error_msg <- paste0(error_msg,
                        "<p><strong>Missing packages:</strong> ", paste(missing_packages, collapse = ", "), "</p>",
                        "<p>Please install using: <code>install.packages(c('", 
                        paste(missing_packages, collapse = "', '"), "'))</code></p>"
                    )
                }
                
                if (length(version_issues) > 0) {
                    error_msg <- paste0(error_msg,
                        "<p><strong>Version issues:</strong></p><ul>",
                        paste0("<li>", version_issues, "</li>", collapse = ""),
                        "</ul>"
                    )
                }
                
                error_msg <- paste0(error_msg, "</div>")
                
                return(list(all_available = FALSE, error_message = error_msg))
            }
            
            return(list(all_available = TRUE, error_message = NULL))
        },
        
        # Enhanced data validation and preparation
        .validate_and_prepare_data = function() {
            analysis_vars <- self$options$analysis_vars
            
            # Check if variables exist in dataset
            missing_vars <- setdiff(analysis_vars, names(self$data))
            if (length(missing_vars) > 0) {
                error_msg <- paste0(
                    "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
                    "<h4>❌ Variables Not Found</h4>",
                    "<p>The following variables were not found in the dataset: ",
                    paste(missing_vars, collapse = ", "), "</p>",
                    "<p>Please check variable names and try again.</p>",
                    "</div>"
                )
                return(list(valid = FALSE, error_message = error_msg))
            }
            
            # Extract analysis data
            analysis_data <- self$data[analysis_vars]
            
            # Check for completely empty variables
            empty_vars <- names(analysis_data)[sapply(analysis_data, function(x) all(is.na(x)))]
            if (length(empty_vars) > 0) {
                error_msg <- paste0(
                    "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
                    "<h4>❌ Empty Variables Detected</h4>",
                    "<p>The following variables contain only missing values: ",
                    paste(empty_vars, collapse = ", "), "</p>",
                    "<p>Please select variables with at least some observed values.</p>",
                    "</div>"
                )
                return(list(valid = FALSE, error_message = error_msg))
            }
            
            # Check if there is any missing data
            if (sum(is.na(analysis_data)) == 0) {
                no_missing_msg <- "
                <div style='background-color: #fff3cd; padding: 20px; border-radius: 8px;'>
                <h4 style='color: #856404;'>✅ No Missing Data Detected</h4>
                <p>The selected variables contain no missing values. Missing data analysis is not needed for this dataset.</p>
                <p><strong>Suggestions:</strong></p>
                <ul>
                <li>Select different variables that may contain missing values</li>
                <li>Check your data preprocessing steps</li>
                <li>Verify that this is the correct dataset for missing data analysis</li>
                </ul>
                </div>"
                return(list(valid = FALSE, error_message = no_missing_msg))
            }
            
            # Check for variables with excessive missing data
            missing_percentages <- sapply(analysis_data, function(x) sum(is.na(x)) / length(x) * 100)
            high_missing_vars <- names(missing_percentages)[missing_percentages > 90]
            
            if (length(high_missing_vars) > 0) {
                warning_msg <- paste0(
                    "<div style='background-color: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 20px;'>",
                    "<h4 style='color: #856404;'>⚠️ High Missing Data Warning</h4>",
                    "<p>The following variables have >90% missing data: ",
                    paste(high_missing_vars, collapse = ", "), "</p>",
                    "<p>Consider removing these variables or interpreting results cautiously.</p>",
                    "</div>"
                )
                # Add warning but continue analysis
            }
            
            # Data type validation and conversion
            for (var_name in names(analysis_data)) {
                var_data <- analysis_data[[var_name]]
                
                # Convert character to factor if appropriate
                if (is.character(var_data)) {
                    n_unique <- length(unique(var_data[!is.na(var_data)]))
                    if (n_unique <= 20) {  # Reasonable number of levels
                        analysis_data[[var_name]] <- factor(var_data)
                    }
                }
            }
            
            return(list(valid = TRUE, data = analysis_data, error_message = NULL))
        },
        
        # Enhanced performance monitoring
        .monitor_performance = function(operation_name, start_time = NULL) {
            if (is.null(start_time)) {
                return(Sys.time())
            } else {
                end_time <- Sys.time()
                duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
                
                if (is.null(private$.performance_metrics)) {
                    private$.performance_metrics <- list()
                }
                
                private$.performance_metrics[[operation_name]] <- duration
                return(duration)
            }
        }

    )
)