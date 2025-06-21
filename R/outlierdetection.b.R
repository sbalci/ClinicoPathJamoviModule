#' @title Advanced Outlier Detection with easystats
#' @return Advanced outlier detection using performance package
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text
#' @importFrom ggplot2 geom_point geom_hline scale_color_manual
#' @importFrom performance check_outliers
#' @importFrom dplyr mutate select row_number
#' @importFrom htmltools HTML

outlierdetectionClass <- if (requireNamespace("jmvcore")) R6::R6Class("outlierdetectionClass",
    inherit = outlierdetectionBase,
    private = list(

        .run = function() {

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
                stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
            }

            # Safely require performance package
            if (!requireNamespace("performance", quietly = TRUE)) {
                error_msg <- "
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>Performance Package Required</h4>
                <p>The performance package is required for advanced outlier detection functionality.</p>
                <p>Please install it using: <code>install.packages('performance')</code></p>
                </div>"
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Get data and variables
            dataset <- self$data
            selected_vars <- self$options$vars
            
            # Prepare analysis data
            if (length(selected_vars) == 0) {
                return()
            }
            
            analysis_data <- dataset[selected_vars]
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                stop("Error: No complete cases found for the selected variables.")
            }

            # Convert to numeric
            for (var in selected_vars) {
                analysis_data[[var]] <- as.numeric(analysis_data[[var]])
            }

            # Perform outlier detection based on method category
            outlier_results <- private$.perform_outlier_detection(analysis_data)
            
            # Generate outputs
            if (self$options$show_outlier_table) {
                table_html <- private$.generate_outlier_table(outlier_results, analysis_data)
                self$results$outlier_table$setContent(table_html)
            }
            
            if (self$options$show_method_comparison && self$options$method_category %in% c("composite", "all")) {
                comparison_html <- private$.generate_method_comparison(outlier_results)
                self$results$method_comparison$setContent(comparison_html)
            }
            
            if (self$options$show_exclusion_summary) {
                exclusion_html <- private$.generate_exclusion_summary(outlier_results, analysis_data)
                self$results$exclusion_summary$setContent(exclusion_html)
            }
            
            if (self$options$show_interpretation) {
                interpretation_html <- private$.generate_interpretation_guide()
                self$results$interpretation$setContent(interpretation_html)
            }

            # Store data for plotting
            private$.outlier_results <- outlier_results
            private$.analysis_data <- analysis_data

        },

        .plot = function(image, ggtheme, theme, ...) {
            
            # Check if analysis was performed
            if (is.null(private$.outlier_results) || is.null(private$.analysis_data)) {
                return()
            }
            
            outlier_results <- private$.outlier_results
            analysis_data <- private$.analysis_data
            
            # Create outlier visualization
            if (is.data.frame(outlier_results)) {
                # For composite methods with outlier scores
                plot_data <- analysis_data %>%
                    dplyr::mutate(
                        row_id = dplyr::row_number(),
                        outlier_score = if ("Outlier_score" %in% names(outlier_results)) outlier_results$Outlier_score else as.numeric(outlier_results),
                        is_outlier = outlier_score > self$options$composite_threshold
                    )
                
                # Create scatter plot with outlier scores
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = row_id, y = outlier_score, color = is_outlier)) +
                    ggplot2::geom_point(size = 2, alpha = 0.7) +
                    ggplot2::geom_hline(yintercept = self$options$composite_threshold, linetype = "dashed", color = "red") +
                    ggplot2::scale_color_manual(
                        name = "Outlier Status",
                        values = c("FALSE" = "#2E86AB", "TRUE" = "#A23B72"),
                        labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
                    ) +
                    ggplot2::labs(
                        title = "Advanced Outlier Detection Results",
                        subtitle = paste("Method:", private$.get_method_description()),
                        x = "Observation Index",
                        y = "Outlier Score"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.position = "bottom"
                    )
                
            } else {
                # For binary outlier results
                plot_data <- analysis_data %>%
                    dplyr::mutate(
                        row_id = dplyr::row_number(),
                        is_outlier = as.logical(outlier_results)
                    )
                
                # Create simple outlier indicator plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = row_id, y = as.numeric(is_outlier), color = is_outlier)) +
                    ggplot2::geom_point(size = 2, alpha = 0.7) +
                    ggplot2::scale_color_manual(
                        name = "Outlier Status",
                        values = c("FALSE" = "#2E86AB", "TRUE" = "#A23B72"),
                        labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
                    ) +
                    ggplot2::labs(
                        title = "Advanced Outlier Detection Results",
                        subtitle = paste("Method:", private$.get_method_description()),
                        x = "Observation Index",
                        y = "Outlier Classification"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
                        plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
                        axis.title = ggplot2::element_text(size = 12),
                        legend.position = "bottom"
                    )
            }
            
            print(p)
            TRUE
        },

        .perform_outlier_detection = function(data) {
            
            method_category <- self$options$method_category
            
            if (method_category == "univariate") {
                method <- self$options$univariate_methods
                threshold <- private$.get_univariate_threshold(method)
                
            } else if (method_category == "multivariate") {
                method <- self$options$multivariate_methods
                threshold <- NULL  # Use default thresholds
                
            } else if (method_category == "composite") {
                method <- c("zscore_robust", "iqr", "mahalanobis")
                threshold <- NULL
                
            } else if (method_category == "all") {
                method <- "all"
                threshold <- NULL
            }
            
            # Perform outlier detection
            outlier_result <- performance::check_outliers(
                data, 
                method = method,
                threshold = threshold,
                verbose = FALSE
            )
            
            return(outlier_result)
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

        .generate_outlier_table = function(outlier_results, data) {
            
            # Convert outlier results to data frame
            if (is.data.frame(outlier_results)) {
                outlier_df <- outlier_results
            } else {
                outlier_df <- as.data.frame(outlier_results)
            }
            
            # Add row indices
            outlier_df$Row <- 1:nrow(outlier_df)
            
            # Count outliers
            if ("Outlier" %in% names(outlier_df)) {
                n_outliers <- sum(outlier_df$Outlier, na.rm = TRUE)
                outlier_rate <- round(n_outliers / nrow(outlier_df) * 100, 2)
            } else {
                n_outliers <- sum(as.logical(outlier_results), na.rm = TRUE)
                outlier_rate <- round(n_outliers / length(outlier_results) * 100, 2)
            }
            
            table_html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #495057; margin-top: 0;'>🔍 Outlier Detection Results</h3>",
                "<p><strong>Method:</strong> ", private$.get_method_description(), "</p>",
                "<p><strong>Total Observations:</strong> ", nrow(data), "</p>",
                "<p><strong>Outliers Detected:</strong> ", n_outliers, " (", outlier_rate, "%)</p>",
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
            
            comparison_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📊 Method Comparison</h3>",
                "<p>Composite outlier detection combines multiple algorithms for robust identification.</p>",
                "<p>The composite score represents the probability of being classified as an outlier by at least one method.</p>",
                "<p><strong>Threshold:</strong> ", self$options$composite_threshold, " (observations with scores ≥ this value are classified as outliers)</p>",
                "</div>"
            )
            
            return(comparison_html)
        },

        .generate_exclusion_summary = function(outlier_results, data) {
            
            # Calculate exclusion statistics
            if (is.data.frame(outlier_results) && "Outlier" %in% names(outlier_results)) {
                n_outliers <- sum(outlier_results$Outlier, na.rm = TRUE)
            } else {
                n_outliers <- sum(as.logical(outlier_results), na.rm = TRUE)
            }
            
            n_total <- nrow(data)
            n_remaining <- n_total - n_outliers
            exclusion_rate <- round(n_outliers / n_total * 100, 2)
            retention_rate <- round(n_remaining / n_total * 100, 2)
            
            exclusion_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #ef6c00; margin-top: 0;'>⚠️ Exclusion Recommendations</h3>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Total Observations:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_total, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Outliers Detected:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_outliers, " (", exclusion_rate, "%)</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Observations Retained:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", n_remaining, " (", retention_rate, "%)</td></tr>",
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
        }

    )
)

# Store analysis data for plotting
.outlier_results <- NULL
.analysis_data <- NULL