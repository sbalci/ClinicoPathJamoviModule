#' @title Advanced Outlier Detection with easystats
#' 
#' @description
#' Advanced outlier detection using multiple statistical methods from the easystats performance package.
#' This function provides comprehensive outlier detection through univariate methods (Z-scores, IQR, confidence intervals),
#' multivariate methods (Mahalanobis distance, MCD, OPTICS, LOF), and composite scoring across multiple algorithms.
#' Complements existing data quality assessment modules with state-of-the-art outlier detection capabilities.
#' Perfect for clinical research data quality control and preprocessing.
#'
#' @details
#' The outlier detection module supports four main categories of methods:
#' 
#' \strong{Univariate Methods:}
#' \itemize{
#'   \item \strong{Robust Z-Score (MAD-based):} Uses median absolute deviation for robust standardization
#'   \item \strong{Standard Z-Score:} Classical z-score based on mean and standard deviation
#'   \item \strong{Interquartile Range (IQR):} Tukey's method using quartiles and IQR multiplier
#'   \item \strong{Equal-Tailed Interval (ETI):} Symmetric confidence interval approach
#'   \item \strong{Highest Density Interval (HDI):} Bayesian credible interval method
#' }
#' 
#' \strong{Multivariate Methods:}
#' \itemize{
#'   \item \strong{Mahalanobis Distance:} Classical multivariate distance accounting for covariance
#'   \item \strong{Robust Mahalanobis Distance:} Robust version using minimum covariance determinant
#'   \item \strong{Minimum Covariance Determinant (MCD):} Robust covariance estimation
#'   \item \strong{OPTICS Clustering:} Density-based clustering approach
#'   \item \strong{Local Outlier Factor (LOF):} Local density deviation method
#' }
#' 
#' \strong{Composite Methods:} Combine multiple algorithms for robust detection with adjustable thresholds
#' 
#' \strong{All Methods:} Comprehensive analysis using all available techniques
#'
#' @section Method Selection Guidelines:
#' \itemize{
#'   \item \strong{Univariate:} When analyzing variables independently, simple interpretation needed
#'   \item \strong{Multivariate:} When variable relationships matter, detecting complex outlier patterns
#'   \item \strong{Composite:} When robust detection across different data patterns is needed
#'   \item \strong{All:} For comprehensive analysis and method comparison
#' }
#'
#' @section Threshold Recommendations:
#' \itemize{
#'   \item \strong{Z-Score:} 3.29 (99.9% confidence, ~0.1% outliers)
#'   \item \strong{IQR Multiplier:} 1.7 (more conservative than Tukey's 1.5)
#'   \item \strong{Confidence Level:} 0.999 (99.9% for interval methods)
#'   \item \strong{Composite Threshold:} 0.5 (outliers detected by ≥50% of methods)
#' }
#'
#' @section Clinical Applications:
#' \itemize{
#'   \item \strong{Laboratory Data:} CBC, chemistry panels, liver function tests
#'   \item \strong{Anthropometric Data:} Height, weight, BMI measurements
#'   \item \strong{Physiological Data:} Blood pressure, heart rate, temperature
#'   \item \strong{Biomarker Data:} Protein levels, genetic markers, metabolites
#'   \item \strong{Quality Control:} Data entry errors, instrument malfunctions
#' }
#'
#' @section Output Components:
#' \itemize{
#'   \item \strong{Outlier Table:} Detailed results with outlier scores and classifications
#'   \item \strong{Method Comparison:} Performance across different detection algorithms
#'   \item \strong{Exclusion Summary:} Recommendations for data cleaning procedures
#'   \item \strong{Visualization:} Plots showing outlier patterns and distributions
#'   \item \strong{Interpretation:} Detailed guidance on results and methodology
#' }
#'
#' @section Statistical Considerations:
#' \itemize{
#'   \item \strong{Sample Size:} Minimum 30 observations recommended for robust results
#'   \item \strong{Distribution:} Robust methods handle non-normal distributions better
#'   \item \strong{Missing Data:} Complete cases analysis performed automatically
#'   \item \strong{Correlations:} Multivariate methods account for variable relationships
#'   \item \strong{False Positives:} Conservative thresholds reduce over-detection
#' }
#'
#' @section References:
#' \itemize{
#'   \item Lüdecke, D., Ben-Shachar, M., Patil, I., Waggoner, P., & Makowski, D. (2021). 
#'         performance: An R Package for Assessment, Comparison and Testing of Statistical Models. 
#'         Journal of Open Source Software, 6(60), 3139. https://doi.org/10.21105/joss.03139
#'   \item Rousseeuw, P. J., & Hubert, M. (2018). Anomaly detection by robust statistics. 
#'         Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 8(2), e1236.
#'   \item Breunig, M. M., Kriegel, H. P., Ng, R. T., & Sander, J. (2000). LOF: identifying 
#'         density-based local outliers. ACM sigmod record, 29(2), 93-104.
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic univariate outlier detection
#' # Load clinical data
#' data(clinical_data)
#' 
#' # Detect outliers using robust z-score
#' outlierdetection(
#'   data = clinical_data,
#'   vars = c("hemoglobin", "glucose", "creatinine"),
#'   method_category = "univariate",
#'   univariate_methods = "zscore_robust",
#'   zscore_threshold = 3.29,
#'   show_outlier_table = TRUE,
#'   show_visualization = TRUE
#' )
#' 
#' # Example 2: Multivariate outlier detection
#' # Detect multivariate outliers in biomarker data
#' outlierdetection(
#'   data = biomarker_data,
#'   vars = c("protein_1", "protein_2", "protein_3"),
#'   method_category = "multivariate",
#'   multivariate_methods = "mahalanobis",
#'   show_method_comparison = TRUE,
#'   show_exclusion_summary = TRUE
#' )
#' 
#' # Example 3: Composite outlier detection
#' # Robust detection using multiple methods
#' outlierdetection(
#'   data = patient_data,
#'   vars = c("age", "weight", "height", "bmi"),
#'   method_category = "composite",
#'   composite_threshold = 0.6,
#'   show_outlier_table = TRUE,
#'   show_interpretation = TRUE
#' )
#' 
#' # Example 4: Comprehensive analysis
#' # Compare all available methods
#' outlierdetection(
#'   data = lab_data,
#'   vars = c("alt", "ast", "bilirubin", "albumin"),
#'   method_category = "all",
#'   show_method_comparison = TRUE,
#'   show_exclusion_summary = TRUE,
#'   show_visualization = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link[performance]{check_outliers}} for the underlying outlier detection functions
#' 
#' @keywords outlier detection, data quality, clinical research, statistical analysis
#' @concept data preprocessing
#' @concept quality control
#' @concept robust statistics
#' @concept multivariate analysis
#' 
#' @return A jamovi analysis object containing outlier detection results with tables, 
#'         plots, and interpretation based on selected options
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text
#' @importFrom ggplot2 geom_point geom_hline scale_color_manual
#' @importFrom performance check_outliers
#' @importFrom dplyr mutate select row_number
#' @importFrom htmltools HTML
#' @importFrom stringr str_to_title
#' @importFrom dbscan optics lof
#' @importFrom robustbase covMcd

outlierdetectionClass <- if (requireNamespace("jmvcore")) R6::R6Class("outlierdetectionClass",
    inherit = outlierdetectionBase,
    private = list(
        
        .messages = NULL,
        
        .accumulateMessage = function(msg) {
            private$.messages <- c(private$.messages, msg)
        },
        
        .resetMessages = function() {
            private$.messages <- NULL
            self$results$warnings$setContent("")
        },

        .createHTMLSection = function(title, content, style = "info", icon = NULL) {
            # Helper function to create consistent HTML sections
            styles <- list(
                info = "background-color: #e3f2fd; color: #1976d2;",
                warning = "background-color: #fff3e0; color: #f57c00;",
                error = "background-color: #ffebee; color: #d32f2f;",
                success = "background-color: #e8f5e9; color: #388e3c;",
                neutral = "background-color: #f5f5f5; color: #333;"
            )

            icon_html <- if (!is.null(icon)) paste0(icon, " ") else ""

            paste0(
                "<div style='", styles[[style]], " padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                "<h4 style='margin-top: 0;'>", icon_html, title, "</h4>",
                content,
                "</div>"
            )
        },

        .run = function() {
            
            # Reset messages
            private$.resetMessages()

            # Check if required variables have been selected
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>🔍 Welcome to Advanced Outlier Detection!</h3>
                <p><strong>Comprehensive outlier detection using easystats performance package</strong></p>
                <p>Complements existing ClinicoPath data quality modules with state-of-the-art detection methods</p>
                
                <h4 style='color: #1976d2;'>Required Variables:</h4>
                <ol>
                <li><strong>Variables for Analysis:</strong> Select continuous variables to analyze for outliers</li>
                </ol>
                
                <h4 style='color: #1976d2;'>Detection Methods Available:</h4>
                <ul>
                <li><strong>Univariate Methods:</strong> Z-scores (robust/standard), IQR, confidence intervals</li>
                <li><strong>Multivariate Methods:</strong> Mahalanobis distance, MCD, OPTICS, LOF</li>
                <li><strong>Composite Scoring:</strong> Combines multiple methods for robust detection</li>
                <li><strong>All Methods:</strong> Comprehensive analysis using all available techniques</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Clinical Data Quality:</strong> Identify problematic observations in patient data</li>
                <li><strong>Research Preprocessing:</strong> Clean datasets before statistical analysis</li>
                <li><strong>Exploratory Analysis:</strong> Understand data distribution and extreme values</li>
                <li><strong>Method Comparison:</strong> Compare different outlier detection approaches</li>
                <li><strong>Publication Preparation:</strong> Document outlier handling procedures</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Key Features:</h4>
                <ul>
                <li><strong>Multiple Algorithms:</strong> 10+ detection methods from easystats ecosystem</li>
                <li><strong>Robust Thresholds:</strong> Conservative defaults based on research literature</li>
                <li><strong>Comprehensive Output:</strong> Tables, plots, and exclusion recommendations</li>
                <li><strong>Method Documentation:</strong> Detailed interpretation and citation guidance</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>State-of-the-art outlier detection for clinical research and data quality control</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                error_msg <- "
                <div style='color: #721c24; background-color: #f8d7da; padding: 20px; border-radius: 8px;'>
                <h4>📊 Dataset Error</h4>
                <p><strong>Problem:</strong> The provided dataset contains no complete rows.</p>
                <h5>Possible Solutions:</h5>
                <ul>
                <li><strong>Check Data Import:</strong> Ensure your data was imported correctly</li>
                <li><strong>Missing Data:</strong> Your dataset may contain only missing values</li>
                <li><strong>Filter Issues:</strong> Check if any applied filters excluded all data</li>
                <li><strong>Variable Selection:</strong> Try selecting different variables</li>
                </ul>
                <p><em>💡 Tip: Use the data exploration tools to examine your dataset structure before running outlier detection.</em></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Safely require performance package
            if (!requireNamespace("performance", quietly = TRUE)) {
                error_msg <- "
                <div style='color: #721c24; background-color: #f8d7da; padding: 20px; border-radius: 8px;'>
                <h4>📦 Required Package Missing</h4>
                <p><strong>Problem:</strong> The 'performance' package is required for advanced outlier detection functionality.</p>
                <h5>Solution:</h5>
                <ol>
                <li><strong>Install Performance Package:</strong></li>
                <ul>
                <li>In R Console: <code style='background-color: #f1f1f1; padding: 2px;'>install.packages('performance')</code></li>
                <li>Or use RStudio's Package tab to install 'performance'</li>
                </ul>
                <li><strong>Restart R Session:</strong> After installation, restart R/RStudio</li>
                <li><strong>Try Analysis Again:</strong> Re-run the outlier detection</li>
                </ol>
                <h5>About the Performance Package:</h5>
                <p>The <strong>performance</strong> package is part of the easystats ecosystem and provides state-of-the-art outlier detection methods including:</p>
                <ul>
                <li>Robust Z-scores and IQR methods</li>
                <li>Mahalanobis distance calculations</li>
                <li>Local Outlier Factor (LOF) algorithms</li>
                <li>Composite outlier scoring</li>
                </ul>
                <p><em>💡 Tip: The performance package is well-maintained and regularly updated with new statistical methods.</em></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }
            
            # Check for additional dependencies based on method selection
            method_category <- self$options$method_category
            if (method_category %in% c("multivariate", "composite", "all")) {
                multivariate_method <- self$options$multivariate_methods
                
                # Check for dbscan package for clustering methods
                if (multivariate_method %in% c("optics", "lof") && 
                    !requireNamespace("dbscan", quietly = TRUE)) {
                    dependency_msg <- paste0("
                    <div style='color: #856404; background-color: #fff3cd; padding: 20px; border-radius: 8px;'>
                    <h4>📦 Optional Dependency Missing</h4>
                    <p><strong>Method Limitation:</strong> The selected method requires the 'dbscan' package.</p>
                    <p><strong>Selected Method:</strong> ", stringr::str_to_title(multivariate_method), "</p>
                    <h5>Quick Fix:</h5>
                    <ol>
                    <li><strong>Install dbscan:</strong> <code style='background-color: #f1f1f1; padding: 2px;'>install.packages('dbscan')</code></li>
                    <li><strong>Alternative:</strong> Select a different multivariate method (Mahalanobis Distance or MCD)</li>
                    </ol>
                    <p><em>💡 The analysis will continue with available methods only.</em></p>
                    </div>")
                    self$results$interpretation$setContent(dependency_msg)
                }
                
                # Check for robustbase package for robust methods  
                if (multivariate_method == "mcd" && 
                    !requireNamespace("robustbase", quietly = TRUE)) {
                    dependency_msg <- "
                    <div style='color: #856404; background-color: #fff3cd; padding: 20px; border-radius: 8px;'>
                    <h4>📦 Optional Dependency Missing</h4>
                    <p><strong>Method Limitation:</strong> The MCD method requires the 'robustbase' package.</p>
                    <h5>Quick Fix:</h5>
                    <ol>
                    <li><strong>Install robustbase:</strong> <code style='background-color: #f1f1f1; padding: 2px;'>install.packages('robustbase')</code></li>
                    <li><strong>Alternative:</strong> Select a different multivariate method (Mahalanobis Distance)</li>
                    </ol>
                    <p><em>💡 The analysis will continue with available methods only.</em></p>
                    </div>"
                    self$results$interpretation$setContent(dependency_msg)
                }
            }

            # Get data and variables
            dataset <- self$data
            selected_vars <- self$options$vars
            
            # Prepare analysis data
            if (length(selected_vars) == 0) {
                return()
            }
            
            # Perform comprehensive input validation
            validation_results <- private$.validateInputs(dataset, selected_vars)
            
            # Show validation summary if there are issues
            if (length(validation_results$warnings) > 0 || length(validation_results$info) > 0) {
                validation_html <- private$.generateValidationSummary(validation_results)
                self$results$interpretation$setContent(validation_html)
            }
            
            # Stop if critical errors found
            if (validation_results$should_stop) {
                return()
            }
            
            analysis_data <- dataset[selected_vars]

            # Handle single variable case - ensure it's a data frame
            if (is.vector(analysis_data) || is.factor(analysis_data)) {
                analysis_data <- data.frame(var = analysis_data)
                names(analysis_data) <- selected_vars
            }

            analysis_data <- analysis_data[complete.cases(analysis_data), , drop = FALSE]

            # CRITICAL FIX: Preserve original dataset size before sampling
            original_n <- nrow(analysis_data)
            
            # Clinical Assumption Checklist
            # 1. Sample Size Check
            if (original_n < 30) {
                private$.accumulateMessage(sprintf("Sample size is small (N=%d). Outlier detection methods, especially multivariate ones, may be unreliable. Recommended N >= 30.", original_n))
            }
            
            # 2. Skewness Check for Classical Methods
            # Only relevant if using standard Z-score or Mahalanobis (but good to warn generally)
            if (self$options$method_category %in% c("univariate", "multivariate", "composite")) {
                 for (col in names(analysis_data)) {
                     if (is.numeric(analysis_data[[col]])) {
                         # Simple skewness check
                         x <- analysis_data[[col]]
                         n <- length(x)
                         m3 <- sum((x - mean(x))^3) / n
                         s3 <- sd(x)^3
                         skew <- m3 / s3
                         
                         if (!is.na(skew) && abs(skew) > 2) {
                             private$.accumulateMessage(sprintf("Variable '%s' is highly skewed (skewness=%.2f). Standard methods (Z-score, Mahalanobis) may flag valid extreme values as outliers. Consider using Robust Z-score or MCD.", col, skew))
                         }
                     }
                 }
            }

            # Performance optimization for large datasets
            if (nrow(analysis_data) > 5000) {
                performance_msg <- NULL

                # For very large datasets, offer sampling
                if (nrow(analysis_data) > 10000) {
                    sample_size <- 5000
                    set.seed(123)  # For reproducibility
                    sample_idx <- sample(nrow(analysis_data), sample_size)
                    # Keep original_n preserved from above
                    analysis_data <- analysis_data[sample_idx, , drop = FALSE]

                    performance_msg <- private$.createHTMLSection(
                        "Performance Optimization",
                        paste0(
                            "<p><strong>Large dataset detected:</strong> ",
                            "Your dataset contains ", original_n, " observations. ",
                            "For faster analysis, we've sampled ", sample_size, " observations.</p>",
                            "<p><strong>Clinical note:</strong> ",
                            "Outlier detection on a representative sample is often sufficient for data quality assessment. ",
                            "The sampled results should identify systematic issues effectively.</p>",
                            "<p><em>Tip: For full dataset analysis, consider processing variables in smaller batches.</em></p>"
                        ),
                        style = "info",
                        icon = "⚡"
                    )
                } else {
                    # For moderately large datasets, just notify
                    performance_msg <- private$.createHTMLSection(
                        "Processing Large Dataset",
                        paste0(
                            "<p>Analyzing ", nrow(analysis_data), " observations. ",
                            "This may take a moment for complex multivariate methods.</p>",
                            "<p><em>Tip: For faster results, consider univariate methods or analyze fewer variables at once.</em></p>"
                        ),
                        style = "info",
                        icon = "⏱️"
                    )
                }

                # Store performance message to display later
                if (!is.null(performance_msg)) {
                    self$results$interpretation$setContent(performance_msg)
                }
            }

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                error_msg <- "
                <div style='color: #721c24; background-color: #f8d7da; padding: 20px; border-radius: 8px;'>
                <h4>📋 No Complete Cases Found</h4>
                <p><strong>Problem:</strong> None of the selected variables have complete data (all values are missing).</p>
                <h5>Possible Causes & Solutions:</h5>
                <ul>
                <li><strong>High Missing Data:</strong> Selected variables may have excessive missing values</li>
                <ul><li>Solution: Choose variables with less missing data</li></ul>
                <li><strong>Data Import Issues:</strong> Variables might not have been imported correctly</li>
                <ul><li>Solution: Check data import process and variable types</li></ul>
                <li><strong>Variable Names:</strong> Selected variable names might not match data columns</li>
                <ul><li>Solution: Verify variable names in your dataset</li></ul>
                <li><strong>Data Filtering:</strong> Applied filters might exclude all observations</li>
                <ul><li>Solution: Review and adjust any data filters</li></ul>
                </ul>
                <h5>Recommendations:</h5>
                <ol>
                <li><strong>Examine Data:</strong> Use descriptive statistics to check variable completeness</li>
                <li><strong>Select Different Variables:</strong> Choose variables with more complete data</li>
                <li><strong>Handle Missing Data:</strong> Consider imputation methods if appropriate</li>
                <li><strong>Check Data Quality:</strong> Review original data source for issues</li>
                </ol>
                <p><em>💡 Tip: Outlier detection requires complete cases. Aim for variables with &lt;20% missing data for reliable results.</em></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Convert to numeric with safe variable access
            for (var in selected_vars) {
                analysis_data[[var]] <- as.numeric(analysis_data[[var]])
            }

            # Initialize outlier_results variable
            outlier_results <- NULL

            # Perform outlier detection with proper error handling
            outlier_results <- tryCatch({
                result <- private$.perform_outlier_detection(analysis_data)
                result  # Return the result from tryCatch

            }, error = function(e) {
                error_msg <- paste0("
                <div style='color: #721c24; background-color: #f8d7da; padding: 20px; border-radius: 8px;'>
                <h4>⚠️ Outlier Detection Error</h4>
                <p><strong>Error:</strong> ", as.character(e$message), "</p>
                <p><strong>Method:</strong> ", self$options$method_category, "</p>
                <p><strong>Variables:</strong> ", ncol(analysis_data), " variable(s)</p>
                <p><strong>Observations:</strong> ", nrow(analysis_data), "</p>
                <h5>Common Solutions:</h5>
                <ul>
                <li>Try a different detection method (Robust Z-Score is most reliable)</li>
                <li>Check for infinite or extremely large values</li>
                <li>Ensure sufficient sample size (n ≥ 30 recommended)</li>
                <li>For multivariate methods, try with fewer variables</li>
                </ul>
                </div>")
                self$results$interpretation$setContent(error_msg)
                NULL  # Return NULL on error
            })

            # Check if outlier detection was successful
            if (is.null(outlier_results)) {
                # If outlier detection failed, don't proceed with generating outputs
                return()
            }

            # Generate plain-language summary
            plain_summary <- private$.generate_plain_summary(outlier_results, analysis_data)

            # Generate outputs with original dataset size
            if (self$options$show_outlier_table) {
                table_html <- private$.generate_outlier_table(outlier_results, analysis_data, original_n)
                # Combine plain summary with technical table
                combined_html <- paste0(plain_summary, table_html)
                self$results$outlier_table$setContent(combined_html)
            }

            if (self$options$show_method_comparison && self$options$method_category %in% c("composite", "all")) {
                comparison_html <- private$.generate_method_comparison(outlier_results)
                self$results$method_comparison$setContent(comparison_html)
            }

            if (self$options$show_exclusion_summary) {
                exclusion_html <- private$.generate_exclusion_summary(outlier_results, analysis_data, original_n)
                self$results$exclusion_summary$setContent(exclusion_html)
            }
            
            if (self$options$show_interpretation) {
                interpretation_html <- private$.generate_interpretation_guide()
                self$results$interpretation$setContent(interpretation_html)
            }

            # Store plot data for visualization
            if (self$options$show_visualization) {
                plotData <- list(
                    outlier_results = outlier_results,
                    analysis_data = analysis_data
                )
                self$results$plot$setState(plotData)
            }
            
            # Populate warnings panel
            if (!is.null(private$.messages) && length(private$.messages) > 0) {
                self$results$warnings$setContent(paste(
                    "<div class='alert alert-warning'>",
                    "<h6>Analysis Messages</h6>",
                    "<ul>",
                    paste(paste0("<li>", private$.messages, "</li>"), collapse = ""),
                    "</ul></div>",
                    sep = ""
                ))
            }

        },

        .plot = function(image, ggtheme, theme, ...) {
            # Simple, reliable plot function

            plotData <- image$state

            if (is.null(plotData) || is.null(plotData$outlier_results) || is.null(plotData$analysis_data)) {
                return(FALSE)
            }

            outlier_results <- plotData$outlier_results
            analysis_data <- plotData$analysis_data

            # CRITICAL FIX: Extract detailed data from result list
            if (is.list(outlier_results) && "outlier_data" %in% names(outlier_results)) {
                outlier_logical <- outlier_results$outlier_logical
                outlier_data <- outlier_results$outlier_data

                # Calculate proportion outlier from detailed data
                if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                    outlier_cols <- grep("^Outlier", names(outlier_data), value = TRUE)
                    if (length(outlier_cols) > 0) {
                        outlier_score <- rowMeans(outlier_data[, outlier_cols, drop = FALSE], na.rm = TRUE)
                    } else {
                        outlier_score <- as.numeric(outlier_logical)
                    }
                } else {
                    outlier_score <- as.numeric(outlier_logical)
                }

                # Create plot data with composite scores
                plot_data <- data.frame(
                    row_id = 1:length(outlier_score),
                    outlier_score = outlier_score,
                    is_outlier = outlier_score >= self$options$composite_threshold
                )

                # Create scatter plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = row_id, y = outlier_score, color = is_outlier)) +
                    ggplot2::geom_point(size = 2, alpha = 0.7) +
                    ggplot2::geom_hline(yintercept = self$options$composite_threshold, linetype = "dashed", color = "red") +
                    ggplot2::scale_color_manual(
                        name = "Status",
                        values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"),
                        labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
                    ) +
                    ggplot2::labs(
                        title = "Outlier Detection Results",
                        x = "Observation Index",
                        y = "Outlier Score"
                    ) +
                    ggplot2::theme_minimal()

            } else {
                # Binary results
                plot_data <- data.frame(
                    row_id = 1:nrow(analysis_data),
                    is_outlier = as.logical(outlier_results)
                )

                # Create binary plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = row_id, y = as.numeric(is_outlier), color = is_outlier)) +
                    ggplot2::geom_jitter(size = 2, alpha = 0.7, width = 0.2, height = 0.05) +
                    ggplot2::scale_color_manual(
                        name = "Status",
                        values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"),
                        labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
                    ) +
                    ggplot2::scale_y_continuous(
                        breaks = c(0, 1),
                        labels = c("Normal", "Outlier"),
                        limits = c(-0.1, 1.1)
                    ) +
                    ggplot2::labs(
                        title = "Outlier Detection Results",
                        x = "Observation Index",
                        y = "Classification"
                    ) +
                    ggplot2::theme_minimal()
            }

            # Apply jamovi theme
            p <- p + ggtheme

            print(p)
            return(TRUE)
        },

        .escapeVar = function(var) {
            if (is.character(var)) {
                var <- gsub("`", "", var, fixed = TRUE)
                var <- paste0("`", var, "`")
            }
            return(var)
        },

        .perform_outlier_detection = function(data) {
            
            method_category <- self$options$method_category
            
            # Check data validity before proceeding
            if (is.null(data) || nrow(data) == 0) {
                stop("No data available for outlier detection")
            }

            if (is.null(data) || ncol(data) == 0) {
                stop("No variables available for outlier detection")
            }
            
            # Check for completely missing variables
            all_na_vars <- sapply(data, function(x) all(is.na(x)))
            if (any(all_na_vars)) {
                stop(paste("Variables with all missing values:", paste(names(data)[all_na_vars], collapse = ", ")))
            }
            
            # Check for constant variables  
            constant_vars <- sapply(data, function(x) {
                non_na_x <- x[!is.na(x)]
                length(unique(non_na_x)) <= 1
            })
            if (any(constant_vars)) {
                stop(paste("Variables with constant values (no variation):", paste(names(data)[constant_vars], collapse = ", ")))
            }
            
            # Set up method and threshold based on category
            if (method_category == "univariate") {
                method <- self$options$univariate_methods
                threshold <- private$.get_univariate_threshold(method)
                
            } else if (method_category == "multivariate") {
                # Check if multivariate methods are applicable
                if (ncol(data) == 1) {
                    stop("Multivariate methods require multiple variables. Please select more variables or use univariate methods.")
                }
                
                method <- self$options$multivariate_methods
                threshold <- NULL  # Use default thresholds
                
                # Special checks for specific multivariate methods
                if (method %in% c("optics", "lof")) {
                    if (!requireNamespace("dbscan", quietly = TRUE)) {
                        stop(paste("Method", method, "requires the 'dbscan' package. Please install it using: install.packages('dbscan')"))
                    }
                }
                
                if (method == "mcd") {
                    if (!requireNamespace("robustbase", quietly = TRUE)) {
                        stop("MCD method requires the 'robustbase' package. Please install it using: install.packages('robustbase')")
                    }
                }
                
            } else if (method_category == "composite") {
                method <- c("zscore_robust", "iqr", "mahalanobis")
                threshold <- NULL
                
                # For composite methods, check if we have enough variables for multivariate component
                if (ncol(data) == 1) {
                    method <- c("zscore_robust", "iqr")  # Remove mahalanobis for single variable
                }
                
            } else if (method_category == "all") {
                method <- "all"
                threshold <- NULL
            }
            
            # Perform outlier detection
            outlier_result <- performance::check_outliers(
                data,
                method = method,
                threshold = threshold
            )

            # Check if result is valid
            if (is.null(outlier_result)) {
                stop("Outlier detection returned no results")
            }

            # CRITICAL FIX: Extract detailed data from attributes
            # performance::check_outliers() returns a logical vector with
            # detailed information stored in attr(result, "data")
            outlier_data <- attr(outlier_result, "data")

            # If detailed data exists, augment the result
            if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                # Create a comprehensive result object with both the logical vector
                # and the detailed per-method scores/probabilities
                result_list <- list(
                    outlier_logical = as.logical(outlier_result),
                    outlier_data = outlier_data,
                    method = method,
                    threshold = threshold,
                    n_obs = nrow(data)
                )

                return(result_list)
            } else {
                # Fallback for older performance package versions
                result_list <- list(
                    outlier_logical = as.logical(outlier_result),
                    outlier_data = NULL,
                    method = method,
                    threshold = threshold,
                    n_obs = nrow(data)
                )

                return(result_list)
            }
        },

        .get_univariate_threshold = function(method) {
            if (method %in% c("zscore", "zscore_robust")) {
                return(self$options$zscore_threshold)
            } else if (method == "iqr") {
                return(self$options$iqr_multiplier)
            } else if (method %in% c("eti", "hdi")) {
                return(self$options$confidence_level)
            }
            return(NULL)
        },

        .get_method_description = function() {
            method_category <- self$options$method_category
            
            if (method_category == "univariate") {
                method_name <- switch(self$options$univariate_methods,
                    "zscore_robust" = "Robust Z-Score (MAD-based)",
                    "zscore" = "Standard Z-Score",
                    "iqr" = "Interquartile Range (IQR)",
                    "eti" = "Equal-Tailed Interval",
                    "hdi" = "Highest Density Interval"
                )
            } else if (method_category == "multivariate") {
                method_name <- switch(self$options$multivariate_methods,
                    "mahalanobis" = "Mahalanobis Distance",
                    "mahalanobis_robust" = "Robust Mahalanobis Distance",
                    "mcd" = "Minimum Covariance Determinant",
                    "optics" = "OPTICS Clustering",
                    "lof" = "Local Outlier Factor"
                )
            } else if (method_category == "composite") {
                method_name = "Composite (Multiple Methods)"
            } else {
                method_name = "All Available Methods"
            }
            
            return(method_name)
        },

        .generate_plain_summary = function(outlier_results, data) {
            # Generate plain-language summary for clinical users

            n_total <- nrow(data)
            n_vars <- ncol(data)

            # CRITICAL FIX: Extract detailed data from result list
            if (is.list(outlier_results) && "outlier_logical" %in% names(outlier_results)) {
                outlier_logical <- outlier_results$outlier_logical
                outlier_data <- outlier_results$outlier_data

                # Calculate proportion outlier from detailed data
                if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                    outlier_cols <- grep("^Outlier", names(outlier_data), value = TRUE)
                    if (length(outlier_cols) > 0) {
                        proportion_outlier <- rowMeans(outlier_data[, outlier_cols, drop = FALSE], na.rm = TRUE)
                    } else {
                        proportion_outlier <- as.numeric(outlier_logical)
                    }
                } else {
                    proportion_outlier <- as.numeric(outlier_logical)
                }

                # Apply composite threshold
                threshold <- self$options$composite_threshold
                n_outliers <- sum(proportion_outlier >= threshold, na.rm = TRUE)
            } else {
                # Legacy fallback
                n_outliers <- sum(as.logical(outlier_results), na.rm = TRUE)
            }

            outlier_pct <- round(n_outliers / n_total * 100, 1)

            # Create clinical context interpretation
            clinical_context <- if (n_outliers == 0) {
                "No unusual values were detected in your data. This suggests good data quality with consistent measurements."
            } else if (outlier_pct < 1) {
                "A very small number of unusual values were found. These are likely genuine extreme cases or rare clinical presentations."
            } else if (outlier_pct < 5) {
                "A modest number of unusual values were identified. Review these cases to determine if they represent data entry errors or genuine clinical variation."
            } else if (outlier_pct < 10) {
                "Several unusual values were detected. This warrants careful review to distinguish between data quality issues and true biological variation."
            } else {
                "A substantial proportion of unusual values were found. Consider reviewing data collection procedures and checking for systematic issues."
            }

            # Create action recommendations
            action_text <- if (n_outliers == 0) {
                "Your data appears ready for statistical analysis."
            } else if (outlier_pct < 5) {
                paste0("Review the ", n_outliers, " flagged observation(s) below. ",
                       "In clinical data, outliers may represent: ",
                       "(1) data entry errors, (2) equipment malfunction, ",
                       "(3) genuine extreme cases, or (4) rare disease presentations.")
            } else {
                paste0("Carefully examine the ", n_outliers, " flagged observations. ",
                       "The high outlier rate suggests possible systematic issues. ",
                       "Consider: (1) reviewing data collection protocols, ",
                       "(2) checking measurement equipment calibration, ",
                       "(3) verifying data entry procedures.")
            }

            # Method description in plain language with specific details
            method_desc <- switch(self$options$method_category,
                "univariate" = {
                    specific_method <- switch(self$options$univariate_methods,
                        "zscore" = "Z-score analysis",
                        "zscore_robust" = "Robust Z-score analysis",
                        "iqr" = "Interquartile Range (IQR) method",
                        "eti" = "Equal-Tailed Interval method",
                        "hdi" = "Highest Density Interval method",
                        "univariate analysis"
                    )
                    paste0(specific_method, " (analyzing each variable independently)")
                },
                "multivariate" = {
                    specific_method <- switch(self$options$multivariate_methods,
                        "mahalanobis" = "Mahalanobis distance",
                        "mahalanobis_robust" = "Robust Mahalanobis distance",
                        "mcd" = "Minimum Covariance Determinant (MCD)",
                        "ics" = "Invariant Coordinate Selection (ICS)",
                        "optics" = "OPTICS clustering method",
                        "lof" = "Local Outlier Factor (LOF)",
                        "multivariate analysis"
                    )
                    paste0(specific_method, " (considering relationships between variables)")
                },
                "composite" = "multiple detection methods combined for robust identification (Z-score, IQR, and Mahalanobis distance)",
                "all" = "comprehensive analysis using all available detection methods",
                "standard statistical methods"
            )

            # Create the summary HTML
            summary_html <- private$.createHTMLSection(
                "Plain Language Summary",
                paste0(
                    "<p><strong>What we found:</strong> ",
                    "In your dataset of ", n_total, " observations across ", n_vars, " variable(s), ",
                    "we identified ", n_outliers, " potential outlier(s) (", outlier_pct, "%) using ",
                    method_desc, ".</p>",
                    "<p><strong>Clinical interpretation:</strong> ", clinical_context, "</p>",
                    "<p><strong>Recommended action:</strong> ", action_text, "</p>"
                ),
                style = if(n_outliers == 0) "success" else if(outlier_pct < 5) "info" else "warning",
                icon = "📊"
            )

            # Add copy-ready report sentence
            report_sentence <- sprintf(
                "Outlier detection analysis was performed on %d observations across %d variable(s) using %s. A total of %d outlier(s) were identified (%.1f%% of observations). %s",
                n_total, n_vars, method_desc,
                n_outliers, outlier_pct,
                if(n_outliers == 0) "No data quality issues were detected."
                else if(outlier_pct < 5) "The low outlier rate suggests acceptable data quality."
                else "Further data review is recommended."
            )

            report_html <- private$.createHTMLSection(
                "Report Sentence",
                paste0(
                    "<div style='background-color: white; padding: 10px; border: 1px solid #ddd; ",
                    "border-radius: 4px; font-family: monospace; font-size: 12px;'>",
                    report_sentence,
                    "</div>",
                    "<p style='margin-top: 10px; font-style: italic; color: #666; font-size: 11px;'>",
                    "Copy the text above for use in clinical reports or documentation.</p>"
                ),
                style = "neutral",
                icon = "📝"
            )

            return(paste0(summary_html, report_html))
        },

        .generate_outlier_table = function(outlier_results, data, original_n = NULL) {

            # CRITICAL FIX: Extract detailed data from result list
            if (is.list(outlier_results) && "outlier_data" %in% names(outlier_results)) {
                outlier_logical <- outlier_results$outlier_logical
                outlier_data <- outlier_results$outlier_data

                # If we have detailed per-method data, use it
                if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                    outlier_df <- outlier_data

                    # CRITICAL FIX: Calculate composite score as percentage of methods
                    # that flagged each observation (not just a binary yes/no)
                    if (ncol(outlier_df) > 0) {
                        # Count how many methods flagged each observation
                        # Columns like "Outlier_zscore_robust", "Outlier_iqr", etc.
                        outlier_cols <- grep("^Outlier", names(outlier_df), value = TRUE)

                        if (length(outlier_cols) > 0) {
                            # Calculate proportion of methods flagging each case
                            outlier_df$Proportion_Outlier <- rowMeans(outlier_df[, outlier_cols, drop = FALSE], na.rm = TRUE)
                        } else {
                            # Fallback to logical vector
                            outlier_df$Proportion_Outlier <- as.numeric(outlier_logical)
                        }
                    } else {
                        outlier_df$Proportion_Outlier <- as.numeric(outlier_logical)
                    }
                } else {
                    # Fallback: use logical vector
                    outlier_df <- data.frame(
                        Outlier = outlier_logical,
                        Proportion_Outlier = as.numeric(outlier_logical)
                    )
                }
            } else {
                # Legacy fallback
                outlier_df <- data.frame(
                    Outlier = as.logical(outlier_results),
                    Proportion_Outlier = as.numeric(as.logical(outlier_results))
                )
            }

            # Add row indices
            outlier_df$Row <- 1:nrow(outlier_df)

            # CRITICAL FIX: Apply composite threshold to proportion, not binary flag
            threshold <- self$options$composite_threshold
            n_outliers <- sum(outlier_df$Proportion_Outlier >= threshold, na.rm = TRUE)

            # CRITICAL FIX: Use original dataset size if provided
            total_n <- if (!is.null(original_n)) original_n else nrow(data)
            outlier_rate <- round(n_outliers / nrow(outlier_df) * 100, 2)

            # Add sampling notice if original_n differs from current data
            sampling_notice <- ""
            if (!is.null(original_n) && original_n != nrow(outlier_df)) {
                sampling_notice <- paste0(
                    "<p style='color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 4px;'>",
                    "<strong>⚠️ Sampling Applied:</strong> Analysis performed on ", nrow(outlier_df),
                    " randomly sampled observations from the original ", original_n, " observations. ",
                    "Outlier counts and rates shown below refer to the sampled subset.</p>"
                )
            }

            table_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>🔍 Outlier Detection Results</h3>",
                "<p><strong>Method:</strong> ", private$.get_method_description(), "</p>",
                "<p><strong>Total Observations:</strong> ", total_n,
                if (!is.null(original_n) && original_n != nrow(outlier_df))
                    paste0(" (analyzed ", nrow(outlier_df), " sampled)") else "", "</p>",
                "<p><strong>Outliers Detected:</strong> ", n_outliers, " (", outlier_rate,
                "% of ", if (!is.null(original_n) && original_n != nrow(outlier_df)) "sampled " else "", "observations)</p>",
                sampling_notice,
                "</div>"
            )

            # Add detailed table if available
            if (nrow(outlier_df) <= 100) {  # Only show table for reasonable size
                table_html <- paste0(table_html,
                    "<div style='background-color: #ffffff; padding: 15px; border-radius: 8px; margin-top: 20px;'>",
                    "<h4>Detailed Results (First 100 observations)</h4>",
                    private$.format_outlier_table(outlier_df),
                    "</div>"
                )
            }

            return(table_html)
        },

        .format_outlier_table = function(outlier_df) {
            
            # Limit to first 100 rows for display
            display_df <- head(outlier_df, 100)
            
            table_html <- "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>"
            table_html <- paste0(table_html,
                "<thead><tr style='background-color: #6c757d; color: white;'>",
                "<th style='padding: 8px; border: 1px solid #dee2e6;'>Row</th>"
            )
            
            # Add column headers
            for (col in names(display_df)[names(display_df) != "Row"]) {
                table_html <- paste0(table_html,
                    "<th style='padding: 8px; border: 1px solid #dee2e6;'>", col, "</th>"
                )
            }
            table_html <- paste0(table_html, "</tr></thead><tbody>")
            
            # Add data rows
            for (i in 1:nrow(display_df)) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f8f9fa"
                
                # Highlight outlier rows
                if ("Outlier" %in% names(display_df) && display_df$Outlier[i]) {
                    row_bg <- "#ffebee"
                }
                
                table_html <- paste0(table_html,
                    "<tr style='background-color: ", row_bg, ";'>",
                    "<td style='padding: 8px; border: 1px solid #dee2e6;'>", display_df$Row[i], "</td>"
                )
                
                for (col in names(display_df)[names(display_df) != "Row"]) {
                    value <- display_df[[col]][i]
                    if (is.logical(value)) {
                        value <- if (value) "✓" else "✗"
                    } else if (is.numeric(value)) {
                        value <- round(value, 4)
                    }
                    
                    table_html <- paste0(table_html,
                        "<td style='padding: 8px; border: 1px solid #dee2e6; text-align: center;'>", value, "</td>"
                    )
                }
                table_html <- paste0(table_html, "</tr>")
            }
            
            table_html <- paste0(table_html, "</tbody></table>")
            return(table_html)
        },

        .generate_method_comparison = function(outlier_results) {
            
            # Extract detailed method data
            detailed_data <- outlier_results$outlier_data
            
            comparison_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📊 Method Comparison & Composite Breakdown</h3>"
            )

            if (!is.null(detailed_data) && is.data.frame(detailed_data)) {
                # Look for columns starting with "Z_Score", "Mahalanobis", "IQR", etc. 
                # or typically they are just the method names in performance package depending on version.
                # Actually performance::check_outliers data usually has columns like 'Z_Score_Robust', 'IQR', 'Mahalanobis', etc.
                
                # Filter for numeric/logical columns that strictly look like method flags or scores
                # Note: They are often probabilities or standardized scores. 
                # Let's try to summarize how many outliers each method found.
                
                method_cols <- setdiff(names(detailed_data), c("Row", "Outlier", "n_Outliers"))
                
                # Clean up column names filter
                # We want columns that are numeric or logical
                valid_cols <- c()
                for (col in method_cols) {
                    if (is.numeric(detailed_data[[col]]) || is.logical(detailed_data[[col]])) {
                        valid_cols <- c(valid_cols, col)
                    }
                }
                
                if (length(valid_cols) > 0) {
                     # Create a summary table of agreement
                     comparison_html <- paste0(comparison_html,
                        "<p>The table below shows how many observations were flagged by each individual method included in the composite score.</p>",
                        "<table style='width: 100%; border-collapse: collapse; margin-top: 15px;'>",
                        "<tr style='background-color: #4caf50; color: white;'>",
                        "<th style='padding: 10px; border: 1px solid #ddd;'>Method</th>",
                        "<th style='padding: 10px; border: 1px solid #ddd;'>Outliers Detected</th>",
                        "<th style='padding: 10px; border: 1px solid #ddd;'>Agreement Rate (%)</th>",
                        "</tr>"
                     )
                     
                     n_total <- nrow(detailed_data)
                     
                     for (col in valid_cols) {
                         # Assuming these are probabilities or 0/1 scores. 
                         # Usually standard in performance is probability or distance converted.
                         # Roughly, > threshold or some cut-off. 
                         # For logical columns acts as 0/1. For numeric, it's often a probability.
                         
                         vals <- detailed_data[[col]]
                         if (is.logical(vals)) {
                             n_flagged <- sum(vals, na.rm=TRUE)
                         } else {
                             # Assume numeric score > threshold is flag, but threshold varies.
                             # Actually check_outliers standardizes to roughly [0,1] or flags.
                             # If we can't be sure, we count non-zero? 
                             # Let's check if it's binary-like.
                             if (all(vals %in% c(0, 1, NA))) {
                                  n_flagged <- sum(vals == 1, na.rm=TRUE)
                             } else {
                                  # Continuous score - harder to count "flags" without specific threshold
                                  # Just report "Score Range" maybe?
                                  # Or use the global composite threshold as proxy?
                                  n_flagged <- sum(vals >= self$options$composite_threshold, na.rm=TRUE) 
                             }
                         }
                         
                         pct <- round(n_flagged / n_total * 100, 1)
                         
                         comparison_html <- paste0(comparison_html,
                             "<tr>",
                             "<td style='padding: 8px; border: 1px solid #ddd;'><strong>", col, "</strong></td>",
                             "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", n_flagged, "</td>",
                             "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", pct, "%</td>",
                             "</tr>"
                         )
                     }
                     comparison_html <- paste0(comparison_html, "</table>")
                }
            }
            
            comparison_html <- paste0(comparison_html,
                "<p style='margin-top: 15px;'><strong>Composite Logic:</strong> An observation is classified as a composite outlier if it is flagged by at least ", 
                round(self$options$composite_threshold * 100), "% of the methods used.</p>",
                "</div>"
            )
            
            return(comparison_html)
        },

        .generate_exclusion_summary = function(outlier_results, data, original_n = NULL) {

            # CRITICAL FIX: Extract detailed data from result list
            if (is.list(outlier_results) && "outlier_logical" %in% names(outlier_results)) {
                outlier_logical <- outlier_results$outlier_logical
                outlier_data <- outlier_results$outlier_data

                # Calculate proportion outlier from detailed data
                if (!is.null(outlier_data) && is.data.frame(outlier_data)) {
                    outlier_cols <- grep("^Outlier", names(outlier_data), value = TRUE)
                    if (length(outlier_cols) > 0) {
                        proportion_outlier <- rowMeans(outlier_data[, outlier_cols, drop = FALSE], na.rm = TRUE)
                    } else {
                        proportion_outlier <- as.numeric(outlier_logical)
                    }
                } else {
                    proportion_outlier <- as.numeric(outlier_logical)
                }

                # Apply composite threshold
                threshold <- self$options$composite_threshold
                n_outliers <- sum(proportion_outlier >= threshold, na.rm = TRUE)
            } else {
                # Legacy fallback
                n_outliers <- sum(as.logical(outlier_results), na.rm = TRUE)
            }

            # CRITICAL FIX: Use original dataset size if provided
            n_total <- if (!is.null(original_n)) original_n else nrow(data)
            n_analyzed <- nrow(data)  # May be sampled
            n_remaining <- n_analyzed - n_outliers
            exclusion_rate <- round(n_outliers / n_analyzed * 100, 2)
            retention_rate <- round(n_remaining / n_analyzed * 100, 2)
            
            # Add sampling notice if applicable
            sampling_note <- ""
            if (!is.null(original_n) && original_n != n_analyzed) {
                sampling_note <- paste0(
                    "<p style='color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 10px;'>",
                    "<strong>⚠️ Note:</strong> Statistics below refer to the ", n_analyzed,
                    " sampled observations from the original ", original_n, " observations.</p>"
                )
            }

            exclusion_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #ef6c00; margin-top: 0;'>⚠️ Exclusion Recommendations</h3>",
                sampling_note,
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Total Observations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_total,
                if (!is.null(original_n) && original_n != n_analyzed) paste0(" (analyzed ", n_analyzed, " sampled)") else "", "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Outliers Detected:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_outliers, " (", exclusion_rate,
                "% of ", if (!is.null(original_n) && original_n != n_analyzed) "sampled " else "", "observations)</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Observations Retained:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_remaining, " (", retention_rate,
                "% of ", if (!is.null(original_n) && original_n != n_analyzed) "sampled " else "", "observations)</td></tr>",
                "</table>",
                "<h4 style='color: #ef6c00;'>Recommendations:</h4>",
                "<ul>"
            )
            
            if (exclusion_rate < 5) {
                exclusion_html <- paste0(exclusion_html,
                    "<li>Low outlier rate (< 5%) - acceptable for most analyses</li>",
                    "<li>Consider retaining outliers unless strong theoretical justification for exclusion</li>"
                )
            } else if (exclusion_rate < 10) {
                exclusion_html <- paste0(exclusion_html,
                    "<li>Moderate outlier rate (5-10%) - investigate causes</li>",
                    "<li>Consider robust statistical methods or sensitivity analyses</li>"
                )
            } else {
                exclusion_html <- paste0(exclusion_html,
                    "<li>High outlier rate (> 10%) - investigate data quality issues</li>",
                    "<li>Review data collection procedures and measurement instruments</li>"
                )
            }
            
            exclusion_html <- paste0(exclusion_html,
                "<li>Document outlier handling procedures for transparency</li>",
                "<li>Report analyses both with and without outliers when feasible</li>",
                "</ul></div>"
            )
            
            return(exclusion_html)
        },

        .generate_interpretation_guide = function() {
            
            method_category <- self$options$method_category
            
            interpretation_html <- paste0(
                "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #1976d2; margin-top: 0;'>🔍 Analysis Interpretation Guide</h3>",
                
                "<h4 style='color: #1976d2;'>Current Method: ", private$.get_method_description(), "</h4>"
            )
            
            if (method_category == "univariate") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>Univariate methods</strong> analyze each variable separately and detect observations that are extreme on at least one variable.</p>",
                    "<p><strong>Advantages:</strong> Simple to interpret, computationally efficient</p>",
                    "<p><strong>Limitations:</strong> May miss multivariate outliers, liberal with high-dimensional data</p>"
                )
            } else if (method_category == "multivariate") {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>Multivariate methods</strong> consider relationships between variables and detect observations that are unusual in the multivariate space.</p>",
                    "<p><strong>Advantages:</strong> Detect complex outlier patterns, account for variable correlations</p>",
                    "<p><strong>Limitations:</strong> More complex to interpret, computationally intensive</p>"
                )
            } else {
                interpretation_html <- paste0(interpretation_html,
                    "<p><strong>Composite methods</strong> combine multiple algorithms to provide robust outlier detection with reduced false positive rates.</p>",
                    "<p><strong>Advantages:</strong> Robust across different data patterns, comprehensive coverage</p>",
                    "<p><strong>Threshold:</strong> ", self$options$composite_threshold, " means outliers detected by ≥ ", round(self$options$composite_threshold * 100), "% of methods</p>"
                )
            }
            
            interpretation_html <- paste0(interpretation_html,
                "<h4 style='color: #1976d2;'>Key Considerations:</h4>",
                "<ul>",
                "<li><strong>Clinical Context:</strong> Consider whether outliers represent data errors or genuine biological variation</li>",
                "<li><strong>Sample Size:</strong> Outlier impact is greater in smaller samples</li>",
                "<li><strong>Study Design:</strong> Repeated measures may require different outlier handling</li>",
                "<li><strong>Analysis Goals:</strong> Descriptive vs. inferential statistics may warrant different approaches</li>",
                "</ul>",
                
                "<h4 style='color: #1976d2;'>Reporting Guidelines:</h4>",
                "<ul>",
                "<li>Document outlier detection method and thresholds used</li>",
                "<li>Report number and percentage of outliers detected</li>",
                "<li>Justify outlier handling decisions with theoretical rationale</li>",
                "<li>Consider sensitivity analyses with/without outliers</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #1976d2; margin-top: 15px;'>",
                "<em>📚 Citation: Lüdecke et al. (2021). performance: An R package for assessment, comparison and testing of statistical models. Journal of Open Source Software, 6(60), 3139.</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        },

        .validateInputs = function(dataset, selected_vars) {
            validation_results <- list(
                errors = character(0),
                warnings = character(0),
                info = character(0),
                should_stop = FALSE
            )
            
            # Check dataset validity
            if (is.null(dataset) || !is.data.frame(dataset)) {
                validation_results$errors <- c(validation_results$errors, "Dataset is not a valid data frame")
                validation_results$should_stop <- TRUE
                return(validation_results)
            }
            
            if (nrow(dataset) == 0) {
                validation_results$errors <- c(validation_results$errors, "Dataset contains no rows")
                validation_results$should_stop <- TRUE
                return(validation_results)
            }
            
            # Check variable selection
            if (length(selected_vars) == 0) {
                validation_results$errors <- c(validation_results$errors, "No variables selected for analysis")
                validation_results$should_stop <- TRUE
                return(validation_results)
            }
            
            # Check if selected variables exist in dataset
            missing_vars <- setdiff(selected_vars, names(dataset))
            if (length(missing_vars) > 0) {
                validation_results$errors <- c(validation_results$errors, 
                    paste("Variables not found in dataset:", paste(missing_vars, collapse = ", ")))
                validation_results$should_stop <- TRUE
                return(validation_results)
            }
            
            # Check sample size
            if (nrow(dataset) < 30) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Small sample size (n =", nrow(dataset), "). Results may be unreliable. Recommend n ≥ 30."))
            }
            
            # Check number of variables
            if (length(selected_vars) == 1) {
                validation_results$info <- c(validation_results$info,
                    "Single variable selected. Multivariate methods will not be applicable.")
            } else if (length(selected_vars) > 10) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Many variables selected (", length(selected_vars), "). Consider reducing dimensionality for better performance."))
            }
            
            # Check variable types and data quality
            for (var in selected_vars) {
                var_data <- dataset[[var]]
                
                # Check if variable exists
                if (is.null(var_data)) {
                    validation_results$errors <- c(validation_results$errors, 
                        paste("Variable", var, "is NULL"))
                    validation_results$should_stop <- TRUE
                    next
                }
                
                # Check if variable is numeric or can be converted
                if (!is.numeric(var_data)) {
                    if (is.character(var_data) || is.factor(var_data)) {
                        # Try to convert to numeric
                        numeric_conversion <- suppressWarnings(as.numeric(as.character(var_data)))
                        if (all(is.na(numeric_conversion))) {
                            validation_results$errors <- c(validation_results$errors,
                                paste("Variable", var, "cannot be converted to numeric"))
                            validation_results$should_stop <- TRUE
                            next
                        } else {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste("Variable", var, "converted from character/factor to numeric"))
                        }
                    } else {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "is not numeric. Attempting conversion."))
                    }
                }
                
                # Check missing data
                n_missing <- sum(is.na(var_data))
                if (n_missing > 0) {
                    missing_pct <- round(n_missing / length(var_data) * 100, 1)
                    if (missing_pct > 50) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "has", missing_pct, "% missing data. Consider exclusion."))
                    } else if (missing_pct > 20) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "has", missing_pct, "% missing data. Results may be affected."))
                    } else if (missing_pct > 0) {
                        validation_results$info <- c(validation_results$info,
                            paste("Variable", var, "has", missing_pct, "% missing data (within acceptable range)."))
                    }
                }
                
                # Check for constant values
                numeric_var <- suppressWarnings(as.numeric(as.character(var_data)))
                if (!all(is.na(numeric_var))) {
                    unique_values <- unique(numeric_var[!is.na(numeric_var)])
                    if (length(unique_values) == 1) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "has only one unique value. Outlier detection not meaningful."))
                    } else if (length(unique_values) == 2) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Variable", var, "has only two unique values. Consider if outlier detection is appropriate."))
                    }
                }
                
                # Check for infinite values
                if (any(is.infinite(numeric_var), na.rm = TRUE)) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Variable", var, "contains infinite values. These will be excluded."))
                }
                
                # Check for extreme values that might indicate data entry errors
                if (length(unique_values) > 2) {
                    q75 <- quantile(numeric_var, 0.75, na.rm = TRUE)
                    q25 <- quantile(numeric_var, 0.25, na.rm = TRUE)
                    iqr <- q75 - q25
                    
                    extreme_low <- q25 - 10 * iqr
                    extreme_high <- q75 + 10 * iqr
                    
                    n_extreme <- sum(numeric_var < extreme_low | numeric_var > extreme_high, na.rm = TRUE)
                    if (n_extreme > 0) {
                        validation_results$info <- c(validation_results$info,
                            paste("Variable", var, "has", n_extreme, "potentially extreme values that may represent data entry errors."))
                    }
                }
            }
            
            # Check method compatibility
            method_category <- self$options$method_category
            if (method_category == "multivariate" && length(selected_vars) == 1) {
                validation_results$warnings <- c(validation_results$warnings,
                    "Multivariate methods selected but only one variable provided. Consider univariate methods.")
            }
            
            # Check threshold settings
            if (method_category %in% c("univariate", "composite", "all")) {
                zscore_threshold <- self$options$zscore_threshold
                if (zscore_threshold < 2 || zscore_threshold > 5) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Z-score threshold", zscore_threshold, "is outside recommended range (2-5)."))
                }
                
                iqr_multiplier <- self$options$iqr_multiplier
                if (iqr_multiplier < 1 || iqr_multiplier > 3) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("IQR multiplier", iqr_multiplier, "is outside recommended range (1-3)."))
                }
                
                confidence_level <- self$options$confidence_level
                if (confidence_level < 0.9 || confidence_level > 0.999) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Confidence level", confidence_level, "is outside recommended range (0.9-0.999)."))
                }
            }
            
            if (method_category %in% c("composite", "all")) {
                composite_threshold <- self$options$composite_threshold
                if (composite_threshold < 0.1 || composite_threshold > 1.0) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Composite threshold", composite_threshold, "is outside valid range (0.1-1.0)."))
                }
            }
            
            # Add success message if no major issues
            if (length(validation_results$errors) == 0 && length(validation_results$warnings) == 0) {
                validation_results$info <- c(validation_results$info,
                    "✓ Data validation passed. Analysis can proceed.")
            }
            
            return(validation_results)
        },

        .generateValidationSummary = function(validation_results) {
            html_content <- "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;'>"
            html_content <- paste0(html_content, "<h4 style='color: #495057; margin-top: 0;'>📋 Data Validation Summary</h4>")
            
            # Add errors
            if (length(validation_results$errors) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: #f8d7da; padding: 10px; border-radius: 4px; margin: 10px 0;'>")
                html_content <- paste0(html_content, "<h5 style='color: #721c24; margin-top: 0;'>❌ Errors (Analysis Stopped)</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (error in validation_results$errors) {
                    html_content <- paste0(html_content, "<li style='color: #721c24;'>", error, "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }
            
            # Add warnings
            if (length(validation_results$warnings) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: #fff3cd; padding: 10px; border-radius: 4px; margin: 10px 0;'>")
                html_content <- paste0(html_content, "<h5 style='color: #856404; margin-top: 0;'>⚠️ Warnings</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (warning in validation_results$warnings) {
                    html_content <- paste0(html_content, "<li style='color: #856404;'>", warning, "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }
            
            # Add info messages
            if (length(validation_results$info) > 0) {
                html_content <- paste0(html_content, "<div style='background-color: #d1ecf1; padding: 10px; border-radius: 4px; margin: 10px 0;'>")
                html_content <- paste0(html_content, "<h5 style='color: #0c5460; margin-top: 0;'>ℹ️ Information</h5>")
                html_content <- paste0(html_content, "<ul>")
                for (info in validation_results$info) {
                    html_content <- paste0(html_content, "<li style='color: #0c5460;'>", info, "</li>")
                }
                html_content <- paste0(html_content, "</ul></div>")
            }
            
            html_content <- paste0(html_content, "</div>")
            return(html_content)
        }

    )
)