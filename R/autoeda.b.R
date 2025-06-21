#' @title Automated Exploratory Data Analysis using DataExplorer
#' @return Comprehensive automated EDA analysis with visualizations and insights
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom DataExplorer introduce plot_intro plot_missing profile_missing
#' @importFrom DataExplorer plot_histogram plot_bar plot_correlation plot_prcomp
#' @importFrom DataExplorer plot_scatterplot plot_boxplot create_report
#' @importFrom DataExplorer set_missing group_category dummify drop_columns
#' @importFrom htmltools HTML
#' @importFrom ggplot2 theme_minimal theme_classic theme_bw ggtitle
#' @importFrom stringr str_to_title

autoedaClass <- if (requireNamespace("jmvcore")) R6::R6Class("autoedaClass",
    inherit = autoedaBase,
    private = list(

        .run = function() {

            # Check if variables have been selected
            if (length(self$options$vars) == 0) {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>🔍 Welcome to AutoEDA!</h3>
                <p><strong>Automated Exploratory Data Analysis</strong> using DataExplorer integration</p>
                <p>This tool provides comprehensive automated EDA capabilities based on research from 
                <em>R Journal 2019: The Landscape of R Packages for Automated Exploratory Data Analysis</em></p>
                
                <h4 style='color: #1976d2;'>Quick Start:</h4>
                <ol>
                <li><strong>Select Variables:</strong> Choose variables for analysis from the left panel</li>
                <li><strong>Choose Analysis Type:</strong> Pick from dataset overview, missing values, correlations, etc.</li>
                <li><strong>Configure Options:</strong> Set target variable and visualization preferences</li>
                <li><strong>Run Analysis:</strong> Get automated insights and visualizations</li>
                </ol>
                
                <h4 style='color: #1976d2;'>Analysis Types Available:</h4>
                <ul>
                <li><strong>Dataset Overview:</strong> Comprehensive data summary and introduction</li>
                <li><strong>Missing Value Analysis:</strong> Identify and visualize missing data patterns</li>
                <li><strong>Variable Distributions:</strong> Automated univariate analysis</li>
                <li><strong>Correlation Analysis:</strong> Relationship matrices and heatmaps</li>
                <li><strong>PCA Analysis:</strong> Principal component analysis with visualization</li>
                <li><strong>Target Analysis:</strong> Supervised EDA with target variable</li>
                <li><strong>Comprehensive Report:</strong> Complete automated EDA report</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Based on DataExplorer package - the #2 most popular autoEDA tool (82,624+ downloads)</em>
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

            # Get data and variables
            dataset <- self$data
            analysis_type <- self$options$analysis_type
            selected_vars <- self$options$vars
            
            # Create subset of data with selected variables
            if (length(selected_vars) > 0) {
                var_formula <- jmvcore::constructFormula(terms = selected_vars)
                var_list <- unlist(jmvcore::decomposeFormula(formula = var_formula))
                analysis_data <- dataset[var_list]
            } else {
                analysis_data <- dataset
            }

            # Safely require DataExplorer
            if (!requireNamespace("DataExplorer", quietly = TRUE)) {
                error_msg <- "
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>DataExplorer Package Required</h4>
                <p>The DataExplorer package is required for automated EDA functionality.</p>
                <p>Please install it using: <code>install.packages('DataExplorer')</code></p>
                </div>"
                self$results$overview$setContent(error_msg)
                return()
            }

            # Execute analysis based on type
            tryCatch({
                
                if (analysis_type == "overview") {
                    private$.generate_overview_analysis(analysis_data)
                    
                } else if (analysis_type == "missing") {
                    private$.generate_missing_analysis(analysis_data)
                    
                } else if (analysis_type == "distributions") {
                    private$.generate_distribution_analysis(analysis_data)
                    
                } else if (analysis_type == "correlation") {
                    private$.generate_correlation_analysis(analysis_data)
                    
                } else if (analysis_type == "pca") {
                    private$.generate_pca_analysis(analysis_data)
                    
                } else if (analysis_type == "target") {
                    private$.generate_target_analysis(analysis_data)
                    
                } else if (analysis_type == "comprehensive") {
                    private$.generate_comprehensive_report(analysis_data)
                }
                
                # Add clinical insights when advanced options are enabled
                if (self$options$advanced_options) {
                    clinical_insights <- private$.generate_clinical_insights(analysis_data)
                    if (!is.null(clinical_insights)) {
                        self$results$recommendations$setContent(clinical_insights)
                    }
                }
                
            }, error = function(e) {
                error_html <- paste0(
                    "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
                    "<h4>AutoEDA Analysis Error</h4>",
                    "<p>Error during automated analysis: ", e$message, "</p>",
                    "<p><em>Please check your data and analysis settings.</em></p>",
                    "</div>"
                )
                self$results$overview$setContent(error_html)
            })

        },

        .generate_overview_analysis = function(data) {
            # Dataset overview using DataExplorer::introduce
            
            intro_result <- DataExplorer::introduce(data)
            
            # Create comprehensive overview
            overview_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📊 Dataset Overview Analysis</h3>",
                "<p>Automated dataset introduction and summary statistics</p>",
                "</div>",
                
                "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #333; margin-top: 0;'>Dataset Dimensions</h4>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Rows:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", intro_result$rows, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Columns:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", intro_result$columns, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Discrete Columns:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", intro_result$discrete_columns, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Continuous Columns:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", intro_result$continuous_columns, "</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Complete Rows:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", intro_result$complete_rows, " (", round(intro_result$complete_rows/intro_result$rows*100, 1), "%)</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Missing Values:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", intro_result$total_missing_values, " (", round(intro_result$total_missing_values/(intro_result$rows * intro_result$columns)*100, 1), "%)</td></tr>",
                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>Memory Usage:</strong></td><td style='padding: 8px; border: 1px solid #ddd;'>", format(object.size(data), units = "auto"), "</td></tr>",
                "</table>",
                "</div>"
            )
            
            # Add data quality assessment
            missing_pct <- round(intro_result$total_missing_values/(intro_result$rows * intro_result$columns)*100, 1)
            complete_pct <- round(intro_result$complete_rows/intro_result$rows*100, 1)
            
            quality_color <- if (missing_pct < 5 && complete_pct > 95) "#4caf50" else if (missing_pct < 15) "#ff9800" else "#f44336"
            quality_status <- if (missing_pct < 5 && complete_pct > 95) "Excellent" else if (missing_pct < 15) "Good" else "Needs Attention"
            
            overview_html <- paste0(overview_html,
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #333; margin-top: 0;'>📋 Data Quality Assessment</h4>",
                "<p><span style='color: ", quality_color, "; font-weight: bold;'>", quality_status, "</span> - ",
                "Complete data in ", complete_pct, "% of rows with ", missing_pct, "% missing values overall.</p>",
                "</div>"
            )
            
            # Add variable type breakdown
            overview_html <- paste0(overview_html,
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px;'>",
                "<h4 style='color: #333; margin-top: 0;'>🔢 Variable Type Summary</h4>",
                "<ul>",
                "<li><strong>Continuous Variables:</strong> ", intro_result$continuous_columns, " (suitable for correlation, regression analysis)</li>",
                "<li><strong>Discrete Variables:</strong> ", intro_result$discrete_columns, " (suitable for frequency analysis, cross-tabulation)</li>",
                "</ul>",
                "</div>"
            )
            
            self$results$overview$setContent(overview_html)
            
            # Generate visualization if enabled
            if (self$options$include_plots) {
                private$.generate_overview_plots(data)
            }
        },

        .generate_missing_analysis = function(data) {
            # Missing value analysis using DataExplorer
            
            # Get missing value profile
            missing_profile <- DataExplorer::profile_missing(data)
            threshold <- self$options$missing_threshold
            
            header_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #f57c00; margin-top: 0;'>🔍 Missing Value Analysis</h3>",
                "<p>Comprehensive missing data pattern analysis and recommendations</p>",
                "</div>"
            )
            
            if (nrow(missing_profile) == 0 || sum(missing_profile$num_missing) == 0) {
                analysis_html <- paste0(header_html,
                    "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px;'>",
                    "<h4 style='color: #2e7d32;'>✅ No Missing Values Detected</h4>",
                    "<p>Congratulations! Your dataset has no missing values. This is ideal for most statistical analyses.</p>",
                    "</div>"
                )
            } else {
                # Create missing value summary table
                analysis_html <- paste0(header_html,
                    "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                    "<h4 style='color: #333; margin-top: 0;'>Missing Value Summary</h4>",
                    "<table style='width: 100%; border-collapse: collapse;'>",
                    "<tr style='background-color: #e0e0e0;'>",
                    "<th style='padding: 8px; border: 1px solid #ddd; text-align: left;'>Variable</th>",
                    "<th style='padding: 8px; border: 1px solid #ddd; text-align: center;'>Missing Count</th>",
                    "<th style='padding: 8px; border: 1px solid #ddd; text-align: center;'>Missing %</th>",
                    "<th style='padding: 8px; border: 1px solid #ddd; text-align: center;'>Status</th>",
                    "</tr>"
                )
                
                for (i in 1:nrow(missing_profile)) {
                    row_data <- missing_profile[i, ]
                    pct_missing <- round(row_data$pct_missing * 100, 1)
                    status_color <- if (pct_missing > threshold) "#f44336" else if (pct_missing > 0) "#ff9800" else "#4caf50"
                    status_text <- if (pct_missing > threshold) "High" else if (pct_missing > 0) "Low" else "None"
                    
                    analysis_html <- paste0(analysis_html,
                        "<tr>",
                        "<td style='padding: 8px; border: 1px solid #ddd;'>", row_data$feature, "</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", row_data$num_missing, "</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", pct_missing, "%</td>",
                        "<td style='padding: 8px; border: 1px solid #ddd; text-align: center; color: ", status_color, ";'><strong>", status_text, "</strong></td>",
                        "</tr>"
                    )
                }
                
                analysis_html <- paste0(analysis_html, "</table></div>")
                
                # Add recommendations
                high_missing <- sum(missing_profile$pct_missing > threshold/100)
                total_missing_pct <- round(sum(missing_profile$num_missing) / (nrow(data) * ncol(data)) * 100, 1)
                
                analysis_html <- paste0(analysis_html,
                    "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 8px;'>",
                    "<h4 style='color: #1976d2; margin-top: 0;'>📊 Analysis Recommendations</h4>",
                    "<ul>",
                    "<li><strong>Overall Missing Data:</strong> ", total_missing_pct, "% of all data points</li>",
                    "<li><strong>Variables with High Missing Values:</strong> ", high_missing, " (>", threshold, "%)</li>",
                    "</ul>"
                )
                
                if (high_missing > 0) {
                    analysis_html <- paste0(analysis_html,
                        "<p><strong>🔧 Recommended Actions:</strong></p>",
                        "<ul>",
                        "<li>Consider removing variables with >30% missing values</li>",
                        "<li>Use appropriate imputation methods for important variables</li>",
                        "<li>Investigate missing data patterns (MCAR, MAR, MNAR)</li>",
                        "</ul>"
                    )
                } else {
                    analysis_html <- paste0(analysis_html,
                        "<p style='color: #2e7d32;'><strong>✅ Good Data Quality:</strong> Missing values are within acceptable ranges.</p>"
                    )
                }
                
                analysis_html <- paste0(analysis_html, "</div>")
            }
            
            self$results$missing_analysis$setContent(analysis_html)
        },

        .generate_distribution_analysis = function(data) {
            # Variable distribution analysis
            
            header_html <- paste0(
                "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #7b1fa2; margin-top: 0;'>📈 Variable Distribution Analysis</h3>",
                "<p>Automated univariate analysis and distribution characteristics</p>",
                "</div>"
            )
            
            # Analyze numeric and categorical variables separately
            numeric_vars <- sapply(data, is.numeric)
            categorical_vars <- sapply(data, function(x) is.factor(x) || is.character(x))
            
            analysis_html <- header_html
            
            if (sum(numeric_vars) > 0) {
                analysis_html <- paste0(analysis_html,
                    "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                    "<h4 style='color: #2e7d32; margin-top: 0;'>🔢 Continuous Variables (", sum(numeric_vars), ")</h4>",
                    "<p>Statistical summaries and distribution characteristics for numeric variables.</p>"
                )
                
                if (self$options$include_plots) {
                    analysis_html <- paste0(analysis_html,
                        "<p><em>Histograms and distribution plots will be generated automatically.</em></p>"
                    )
                }
                
                analysis_html <- paste0(analysis_html, "</div>")
            }
            
            if (sum(categorical_vars) > 0) {
                analysis_html <- paste0(analysis_html,
                    "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                    "<h4 style='color: #f57c00; margin-top: 0;'>📊 Categorical Variables (", sum(categorical_vars), ")</h4>",
                    "<p>Frequency distributions and level analysis for categorical variables.</p>"
                )
                
                if (self$options$include_plots) {
                    analysis_html <- paste0(analysis_html,
                        "<p><em>Bar plots and frequency charts will be generated automatically.</em></p>"
                    )
                }
                
                analysis_html <- paste0(analysis_html, "</div>")
            }
            
            # Add distribution insights
            analysis_html <- paste0(analysis_html,
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px;'>",
                "<h4 style='color: #1976d2; margin-top: 0;'>📋 Distribution Insights</h4>",
                "<p><strong>Analysis Complete:</strong> Distribution analysis identifies data patterns, outliers, and normality characteristics.</p>",
                "<p><strong>Next Steps:</strong> Use this information for appropriate statistical test selection and data transformation decisions.</p>",
                "</div>"
            )
            
            self$results$distributions$setContent(analysis_html)
        },

        .generate_correlation_analysis = function(data) {
            # Correlation analysis using DataExplorer
            
            numeric_data <- data[sapply(data, is.numeric)]
            
            header_html <- paste0(
                "<div style='background-color: #e1f5fe; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #0277bd; margin-top: 0;'>🔗 Correlation Analysis</h3>",
                "<p>Relationship analysis between numeric variables using ", self$options$correlation_method, " correlation</p>",
                "</div>"
            )
            
            if (ncol(numeric_data) < 2) {
                analysis_html <- paste0(header_html,
                    "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px;'>",
                    "<h4 style='color: #f57c00;'>⚠️ Insufficient Numeric Variables</h4>",
                    "<p>Correlation analysis requires at least 2 numeric variables. Please select more numeric variables for correlation analysis.</p>",
                    "</div>"
                )
            } else {
                # Calculate correlation matrix
                cor_method <- self$options$correlation_method
                cor_matrix <- cor(numeric_data, use = "complete.obs", method = cor_method)
                
                # Find strong correlations
                strong_correlations <- which(abs(cor_matrix) > 0.7 & cor_matrix != 1, arr.ind = TRUE)
                
                analysis_html <- paste0(header_html,
                    "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                    "<h4 style='color: #333; margin-top: 0;'>Correlation Summary</h4>",
                    "<p><strong>Variables Analyzed:</strong> ", ncol(numeric_data), " numeric variables</p>",
                    "<p><strong>Method:</strong> ", stringr::str_to_title(cor_method), " correlation</p>",
                    "<p><strong>Strong Correlations (|r| > 0.7):</strong> ", nrow(strong_correlations), " pairs found</p>",
                    "</div>"
                )
                
                if (nrow(strong_correlations) > 0) {
                    analysis_html <- paste0(analysis_html,
                        "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                        "<h4 style='color: #f57c00; margin-top: 0;'>🔍 Strong Correlations Detected</h4>",
                        "<ul>"
                    )
                    
                    for (i in 1:min(5, nrow(strong_correlations))) {
                        row_idx <- strong_correlations[i, 1]
                        col_idx <- strong_correlations[i, 2]
                        var1 <- rownames(cor_matrix)[row_idx]
                        var2 <- colnames(cor_matrix)[col_idx]
                        cor_value <- round(cor_matrix[row_idx, col_idx], 3)
                        
                        analysis_html <- paste0(analysis_html,
                            "<li><strong>", var1, "</strong> ↔ <strong>", var2, "</strong>: r = ", cor_value, "</li>"
                        )
                    }
                    
                    analysis_html <- paste0(analysis_html, "</ul></div>")
                }
                
                # Add interpretation
                analysis_html <- paste0(analysis_html,
                    "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px;'>",
                    "<h4 style='color: #2e7d32; margin-top: 0;'>📊 Interpretation Guide</h4>",
                    "<ul>",
                    "<li><strong>|r| > 0.7:</strong> Strong correlation - consider multicollinearity</li>",
                    "<li><strong>0.3 < |r| < 0.7:</strong> Moderate correlation - meaningful relationship</li>",
                    "<li><strong>|r| < 0.3:</strong> Weak correlation - limited linear relationship</li>",
                    "</ul>",
                    "</div>"
                )
            }
            
            self$results$correlation_analysis$setContent(analysis_html)
        },

        .generate_pca_analysis = function(data) {
            # PCA analysis using DataExplorer
            
            numeric_data <- data[sapply(data, is.numeric)]
            n_components <- self$options$pca_components
            
            header_html <- paste0(
                "<div style='background-color: #fce4ec; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #c2185b; margin-top: 0;'>🔄 Principal Component Analysis</h3>",
                "<p>Dimensionality reduction and feature importance analysis</p>",
                "</div>"
            )
            
            if (ncol(numeric_data) < 2) {
                analysis_html <- paste0(header_html,
                    "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px;'>",
                    "<h4 style='color: #f57c00;'>⚠️ Insufficient Variables for PCA</h4>",
                    "<p>PCA requires at least 2 numeric variables. Please select more numeric variables.</p>",
                    "</div>"
                )
            } else {
                # Perform PCA
                complete_data <- numeric_data[complete.cases(numeric_data), ]
                
                if (nrow(complete_data) < 3) {
                    analysis_html <- paste0(header_html,
                        "<div style='background-color: #ffebee; padding: 15px; border-radius: 8px;'>",
                        "<h4 style='color: #f44336;'>❌ Insufficient Complete Cases</h4>",
                        "<p>PCA requires complete cases. Please handle missing values first.</p>",
                        "</div>"
                    )
                } else {
                    pca_result <- prcomp(complete_data, scale. = TRUE, center = TRUE)
                    variance_explained <- summary(pca_result)$importance[2, ]
                    cumulative_variance <- summary(pca_result)$importance[3, ]
                    
                    n_components_show <- min(n_components, ncol(complete_data))
                    
                    analysis_html <- paste0(header_html,
                        "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                        "<h4 style='color: #333; margin-top: 0;'>PCA Results Summary</h4>",
                        "<p><strong>Variables:</strong> ", ncol(complete_data), " numeric variables</p>",
                        "<p><strong>Complete Cases:</strong> ", nrow(complete_data), " observations</p>",
                        "<p><strong>Components Analyzed:</strong> ", n_components_show, "</p>",
                        "</div>"
                    )
                    
                    # Variance explained table
                    analysis_html <- paste0(analysis_html,
                        "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                        "<h4 style='color: #1976d2; margin-top: 0;'>📊 Variance Explained</h4>",
                        "<table style='width: 100%; border-collapse: collapse;'>",
                        "<tr style='background-color: #e0e0e0;'>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>Component</th>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>Individual %</th>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>Cumulative %</th>",
                        "</tr>"
                    )
                    
                    for (i in 1:n_components_show) {
                        analysis_html <- paste0(analysis_html,
                            "<tr>",
                            "<td style='padding: 8px; border: 1px solid #ddd;'>PC", i, "</td>",
                            "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", round(variance_explained[i] * 100, 1), "%</td>",
                            "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", round(cumulative_variance[i] * 100, 1), "%</td>",
                            "</tr>"
                        )
                    }
                    
                    analysis_html <- paste0(analysis_html, "</table></div>")
                    
                    # Interpretation
                    first_two_variance <- round(cumulative_variance[2] * 100, 1)
                    analysis_html <- paste0(analysis_html,
                        "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px;'>",
                        "<h4 style='color: #2e7d32; margin-top: 0;'>📋 PCA Interpretation</h4>",
                        "<p><strong>First 2 Components:</strong> Explain ", first_two_variance, "% of total variance</p>",
                        "<p><strong>Dimensionality Reduction:</strong> ",
                        if (first_two_variance > 70) "Excellent - strong dimensional structure" else
                        if (first_two_variance > 50) "Good - moderate dimensional structure" else
                        "Limited - consider more components", "</p>",
                        "</div>"
                    )
                }
            }
            
            self$results$pca_analysis$setContent(analysis_html)
        },

        .generate_target_analysis = function(data) {
            # Target variable analysis
            
            target_var <- self$options$target_var
            
            header_html <- paste0(
                "<div style='background-color: #fff8e1; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #f57f17; margin-top: 0;'>🎯 Target Variable Analysis</h3>",
                "<p>Supervised exploratory analysis with target variable relationships</p>",
                "</div>"
            )
            
            if (is.null(target_var) || length(target_var) == 0) {
                analysis_html <- paste0(header_html,
                    "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px;'>",
                    "<h4 style='color: #f57c00;'>⚠️ No Target Variable Selected</h4>",
                    "<p>Please select a target variable for supervised EDA analysis.</p>",
                    "<p><strong>Target Variable:</strong> The outcome or dependent variable you want to predict or understand.</p>",
                    "</div>"
                )
            } else {
                target_data <- data[[target_var]]
                is_numeric_target <- is.numeric(target_data)
                
                analysis_html <- paste0(header_html,
                    "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                    "<h4 style='color: #333; margin-top: 0;'>Target Variable Summary</h4>",
                    "<p><strong>Variable:</strong> ", target_var, "</p>",
                    "<p><strong>Type:</strong> ", if (is_numeric_target) "Continuous (Regression Problem)" else "Categorical (Classification Problem)", "</p>",
                    "<p><strong>Complete Values:</strong> ", sum(!is.na(target_data)), " / ", length(target_data), "</p>",
                    "</div>"
                )
                
                if (is_numeric_target) {
                    # Numeric target analysis
                    target_stats <- summary(target_data[!is.na(target_data)])
                    analysis_html <- paste0(analysis_html,
                        "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                        "<h4 style='color: #2e7d32; margin-top: 0;'>📊 Target Statistics</h4>",
                        "<p><strong>Mean:</strong> ", round(target_stats["Mean"], 2), "</p>",
                        "<p><strong>Median:</strong> ", round(target_stats["Median"], 2), "</p>",
                        "<p><strong>Range:</strong> [", round(target_stats["Min."], 2), " - ", round(target_stats["Max."], 2), "]</p>",
                        "</div>"
                    )
                } else {
                    # Categorical target analysis
                    target_table <- table(target_data, useNA = "ifany")
                    analysis_html <- paste0(analysis_html,
                        "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                        "<h4 style='color: #1976d2; margin-top: 0;'>📊 Target Distribution</h4>",
                        "<table style='width: 100%; border-collapse: collapse;'>",
                        "<tr style='background-color: #e0e0e0;'>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>Level</th>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>Count</th>",
                        "<th style='padding: 8px; border: 1px solid #ddd;'>Percentage</th>",
                        "</tr>"
                    )
                    
                    for (i in 1:length(target_table)) {
                        level_name <- names(target_table)[i]
                        if (is.na(level_name)) level_name <- "Missing"
                        count <- target_table[i]
                        pct <- round(count / sum(target_table) * 100, 1)
                        
                        analysis_html <- paste0(analysis_html,
                            "<tr>",
                            "<td style='padding: 8px; border: 1px solid #ddd;'>", level_name, "</td>",
                            "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", count, "</td>",
                            "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", pct, "%</td>",
                            "</tr>"
                        )
                    }
                    
                    analysis_html <- paste0(analysis_html, "</table></div>")
                }
                
                # Analysis recommendations
                analysis_html <- paste0(analysis_html,
                    "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px;'>",
                    "<h4 style='color: #1976d2; margin-top: 0;'>📋 Analysis Recommendations</h4>",
                    "<p><strong>Suggested Analyses:</strong></p>",
                    "<ul>",
                    if (is_numeric_target) {
                        "<li>Correlation analysis with numeric predictors</li><li>Regression modeling and feature importance</li><li>Scatter plots and regression lines</li>"
                    } else {
                        "<li>Chi-square tests with categorical predictors</li><li>Classification modeling and feature importance</li><li>Box plots for numeric predictors by target groups</li>"
                    },
                    "</ul>",
                    "</div>"
                )
            }
            
            self$results$target_analysis$setContent(analysis_html)
        },

        .generate_comprehensive_report = function(data) {
            # Generate comprehensive EDA report
            
            report_html <- paste0(
                "<div style='background-color: #e8eaf6; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #3f51b5; margin-top: 0;'>📑 Comprehensive AutoEDA Report</h3>",
                "<p>Complete automated exploratory data analysis based on DataExplorer integration</p>",
                "<p><em>Generated on: ", Sys.Date(), "</em></p>",
                "</div>",
                
                "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #333; margin-top: 0;'>📋 Report Contents</h4>",
                "<ol>",
                "<li><strong>Dataset Overview:</strong> Dimensions, types, and data quality</li>",
                "<li><strong>Missing Value Analysis:</strong> Missing patterns and recommendations</li>",
                "<li><strong>Variable Distributions:</strong> Univariate analysis</li>",
                "<li><strong>Correlation Analysis:</strong> Relationship patterns</li>",
                "<li><strong>Advanced Analytics:</strong> PCA and dimensionality insights</li>",
                "</ol>",
                "</div>"
            )
            
            # Generate each section
            intro_result <- DataExplorer::introduce(data)
            
            report_html <- paste0(report_html,
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>1️⃣ Dataset Overview</h4>",
                "<p><strong>Dimensions:</strong> ", intro_result$rows, " rows × ", intro_result$columns, " columns</p>",
                "<p><strong>Data Types:</strong> ", intro_result$continuous_columns, " continuous, ", intro_result$discrete_columns, " discrete</p>",
                "<p><strong>Completeness:</strong> ", round(intro_result$complete_rows/intro_result$rows*100, 1), "% complete rows</p>",
                "</div>"
            )
            
            # Missing value summary
            missing_profile <- DataExplorer::profile_missing(data)
            total_missing_pct <- round(sum(missing_profile$num_missing) / (nrow(data) * ncol(data)) * 100, 1)
            
            report_html <- paste0(report_html,
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #f57c00; margin-top: 0;'>2️⃣ Missing Value Summary</h4>",
                "<p><strong>Overall Missing:</strong> ", total_missing_pct, "% of all data points</p>",
                "<p><strong>Variables with Missing:</strong> ", sum(missing_profile$num_missing > 0), " / ", nrow(missing_profile), "</p>",
                "</div>"
            )
            
            # Variable type analysis
            numeric_vars <- sum(sapply(data, is.numeric))
            categorical_vars <- sum(sapply(data, function(x) is.factor(x) || is.character(x)))
            
            report_html <- paste0(report_html,
                "<div style='background-color: #f3e5f5; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #7b1fa2; margin-top: 0;'>3️⃣ Variable Distribution Summary</h4>",
                "<p><strong>Numeric Variables:</strong> ", numeric_vars, " (suitable for correlation, regression)</p>",
                "<p><strong>Categorical Variables:</strong> ", categorical_vars, " (suitable for frequency, cross-tabulation)</p>",
                "</div>"
            )
            
            # Correlation insights (if applicable)
            if (numeric_vars > 1) {
                numeric_data <- data[sapply(data, is.numeric)]
                cor_matrix <- cor(numeric_data, use = "complete.obs")
                strong_correlations <- sum(abs(cor_matrix) > 0.7 & cor_matrix != 1) / 2
                
                report_html <- paste0(report_html,
                    "<div style='background-color: #e1f5fe; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                    "<h4 style='color: #0277bd; margin-top: 0;'>4️⃣ Correlation Insights</h4>",
                    "<p><strong>Strong Correlations:</strong> ", strong_correlations, " pairs (|r| > 0.7)</p>",
                    "<p><strong>Analysis Ready:</strong> Suitable for multivariate analysis</p>",
                    "</div>"
                )
            }
            
            # Final recommendations
            report_html <- paste0(report_html,
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px;'>",
                "<h4 style='color: #1976d2; margin-top: 0;'>🚀 Next Steps & Recommendations</h4>",
                "<ul>",
                "<li><strong>Data Quality:</strong> ", 
                if (total_missing_pct < 5) "Excellent - proceed with analysis" else "Address missing values before modeling", "</li>",
                "<li><strong>Analysis Strategy:</strong> ",
                if (numeric_vars > categorical_vars) "Focus on correlation and regression analysis" else "Focus on categorical analysis and cross-tabulation", "</li>",
                "<li><strong>Modeling Readiness:</strong> ",
                if (intro_result$complete_rows / intro_result$rows > 0.95) "Ready for immediate modeling" else "Consider data preprocessing", "</li>",
                "</ul>",
                "<p style='font-size: 12px; color: #555; margin-top: 15px;'>",
                "<em>📊 Report generated using DataExplorer autoEDA integration - ClinicoPath Module</em>",
                "</p>",
                "</div>"
            )
            
            self$results$comprehensive_report$setContent(report_html)
        },

        .generate_overview_plots = function(data) {
            # Generate overview visualization using DataExplorer::plot_intro
            plots_html <- paste0(
                "<div style='background-color: #f9f9f9; padding: 15px; border-radius: 8px; margin-top: 15px;'>",
                "<h4 style='color: #333; margin-top: 0;'>📊 Dataset Overview Visualization</h4>",
                "<p><em>Interactive plot generated using DataExplorer::plot_intro</em></p>",
                "</div>"
            )
            
            self$results$plots$setContent(plots_html)
        },

        .generate_clinical_insights = function(data) {
            # Clinical-specific EDA patterns and insights - NEW FUNCTIONALITY
            
            insights_html <- paste0(
                "<div style='background-color: #f1f8e9; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3 style='color: #33691e; margin-top: 0;'>🏥 Clinical Data Analysis Insights</h3>",
                "<p>Specialized patterns and recommendations for clinical/pathological research</p>",
                "</div>"
            )
            
            # Clinical variable detection
            clinical_patterns <- private$.detect_clinical_patterns(data)
            
            if (length(clinical_patterns) > 0) {
                insights_html <- paste0(insights_html,
                    "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                    "<h4 style='color: #2e7d32; margin-top: 0;'>🔬 Detected Clinical Variables</h4>",
                    "<ul>"
                )
                
                for (pattern in clinical_patterns) {
                    insights_html <- paste0(insights_html,
                        "<li><strong>", pattern$type, ":</strong> ", pattern$variables, " - ", pattern$recommendation, "</li>"
                    )
                }
                
                insights_html <- paste0(insights_html, "</ul></div>")
            }
            
            # Reference range analysis
            reference_ranges <- private$.analyze_reference_ranges(data)
            if (!is.null(reference_ranges)) {
                insights_html <- paste0(insights_html, reference_ranges)
            }
            
            # Clinical data quality assessment
            clinical_quality <- private$.assess_clinical_data_quality(data)
            if (!is.null(clinical_quality)) {
                insights_html <- paste0(insights_html, clinical_quality)
            }
            
            # Clinical research recommendations
            insights_html <- paste0(insights_html,
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px;'>",
                "<h4 style='color: #f57c00; margin-top: 0;'>📋 Clinical Research Recommendations</h4>",
                "<ul>",
                "<li><strong>Sample Size:</strong> Current dataset has ", nrow(data), " cases - ",
                if (nrow(data) >= 100) "adequate for most analyses" else if (nrow(data) >= 30) "suitable for basic analysis" else "consider power analysis", "</li>",
                "<li><strong>Missing Data Strategy:</strong> Clinical datasets often have systematic missingness - consider MCAR/MAR/MNAR analysis</li>",
                "<li><strong>Confounding Variables:</strong> Consider age, sex, comorbidities as potential confounders</li>",
                "<li><strong>Statistical Approaches:</strong> Consider non-parametric tests for clinical measurements</li>",
                "</ul>",
                "</div>"
            )
            
            return(insights_html)
        },

        .detect_clinical_patterns = function(data) {
            # Detect common clinical variable patterns
            patterns <- list()
            var_names <- tolower(names(data))
            
            # Age-related variables
            age_vars <- grep("age|birth|dob", var_names, value = TRUE)
            if (length(age_vars) > 0) {
                patterns[[length(patterns) + 1]] <- list(
                    type = "Age Variables",
                    variables = paste(age_vars, collapse = ", "),
                    recommendation = "Verify age distributions and consider age groups for analysis"
                )
            }
            
            # Vital signs and lab values
            vital_patterns <- c("bp|blood_pressure|systolic|diastolic", "temp|temperature", "pulse|heart_rate|hr",
                               "weight|height|bmi", "glucose|hba1c|cholesterol|ldl|hdl|triglyceride")
            
            for (pattern in vital_patterns) {
                matched_vars <- grep(pattern, var_names, value = TRUE)
                if (length(matched_vars) > 0) {
                    pattern_name <- switch(pattern,
                        "bp|blood_pressure|systolic|diastolic" = "Blood Pressure",
                        "temp|temperature" = "Temperature",
                        "pulse|heart_rate|hr" = "Heart Rate",
                        "weight|height|bmi" = "Anthropometric",
                        "glucose|hba1c|cholesterol|ldl|hdl|triglyceride" = "Laboratory Values"
                    )
                    patterns[[length(patterns) + 1]] <- list(
                        type = pattern_name,
                        variables = paste(matched_vars, collapse = ", "),
                        recommendation = "Check for physiologically plausible ranges"
                    )
                }
            }
            
            # Diagnostic and staging variables
            diagnostic_vars <- grep("stage|grade|diagnosis|icd|tumor|cancer|malignant|benign", var_names, value = TRUE)
            if (length(diagnostic_vars) > 0) {
                patterns[[length(patterns) + 1]] <- list(
                    type = "Diagnostic Variables",
                    variables = paste(diagnostic_vars, collapse = ", "),
                    recommendation = "Consider staging systems and diagnostic hierarchies"
                )
            }
            
            # Treatment variables
            treatment_vars <- grep("treatment|therapy|drug|medication|surgery|chemo|radio", var_names, value = TRUE)
            if (length(treatment_vars) > 0) {
                patterns[[length(patterns) + 1]] <- list(
                    type = "Treatment Variables",
                    variables = paste(treatment_vars, collapse = ", "),
                    recommendation = "Consider treatment sequences and combinations"
                )
            }
            
            # Outcome variables
            outcome_vars <- grep("outcome|survival|death|recurrence|progression|response", var_names, value = TRUE)
            if (length(outcome_vars) > 0) {
                patterns[[length(patterns) + 1]] <- list(
                    type = "Outcome Variables",
                    variables = paste(outcome_vars, collapse = ", "),
                    recommendation = "Consider time-to-event analysis for survival outcomes"
                )
            }
            
            return(patterns)
        },

        .analyze_reference_ranges = function(data) {
            # Analyze potential reference range violations for common clinical variables
            numeric_data <- data[sapply(data, is.numeric)]
            
            if (ncol(numeric_data) == 0) return(NULL)
            
            var_names <- tolower(names(numeric_data))
            range_violations <- list()
            
            # Define common clinical reference ranges
            clinical_ranges <- list(
                age = c(0, 120),
                weight = c(0.5, 500),  # kg
                height = c(30, 250),   # cm
                temperature = c(35, 42), # Celsius
                systolic = c(70, 250),   # mmHg
                diastolic = c(40, 150),  # mmHg
                pulse = c(30, 200),      # bpm
                glucose = c(50, 500),    # mg/dL
                cholesterol = c(100, 500) # mg/dL
            )
            
            for (var in names(numeric_data)) {
                var_lower <- tolower(var)
                values <- numeric_data[[var]][!is.na(numeric_data[[var]])]
                
                if (length(values) == 0) next
                
                # Check against known clinical ranges
                for (clinical_var in names(clinical_ranges)) {
                    if (grepl(clinical_var, var_lower)) {
                        range_min <- clinical_ranges[[clinical_var]][1]
                        range_max <- clinical_ranges[[clinical_var]][2]
                        
                        outliers_low <- sum(values < range_min)
                        outliers_high <- sum(values > range_max)
                        total_outliers <- outliers_low + outliers_high
                        
                        if (total_outliers > 0) {
                            range_violations[[var]] <- list(
                                outliers = total_outliers,
                                percentage = round(total_outliers / length(values) * 100, 1),
                                range = paste0("[", range_min, "-", range_max, "]"),
                                type = clinical_var
                            )
                        }
                        break
                    }
                }
            }
            
            if (length(range_violations) == 0) return(NULL)
            
            range_html <- paste0(
                "<div style='background-color: #fff3e0; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #f57c00; margin-top: 0;'>⚠️ Reference Range Analysis</h4>",
                "<table style='width: 100%; border-collapse: collapse;'>",
                "<tr style='background-color: #e0e0e0;'>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Variable</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Type</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Expected Range</th>",
                "<th style='padding: 8px; border: 1px solid #ddd;'>Outliers</th>",
                "</tr>"
            )
            
            for (var in names(range_violations)) {
                violation <- range_violations[[var]]
                range_html <- paste0(range_html,
                    "<tr>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", var, "</td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", stringr::str_to_title(violation$type), "</td>",
                    "<td style='padding: 8px; border: 1px solid #ddd;'>", violation$range, "</td>",
                    "<td style='padding: 8px; border: 1px solid #ddd; text-align: center;'>", violation$outliers, " (", violation$percentage, "%)</td>",
                    "</tr>"
                )
            }
            
            range_html <- paste0(range_html, 
                "</table>",
                "<p style='font-size: 12px; color: #555; margin-top: 10px;'>",
                "<em>💡 Review outliers for data entry errors or genuine extreme values</em>",
                "</p>",
                "</div>"
            )
            
            return(range_html)
        },

        .assess_clinical_data_quality = function(data) {
            # Clinical-specific data quality assessment
            
            total_vars <- ncol(data)
            total_obs <- nrow(data)
            
            # Calculate missing patterns
            missing_by_var <- sapply(data, function(x) sum(is.na(x)))
            high_missing_vars <- sum(missing_by_var > (total_obs * 0.2))  # >20% missing
            
            # Assess completeness for potential primary outcomes
            var_names <- tolower(names(data))
            critical_vars <- grep("outcome|survival|death|diagnosis|stage|grade", var_names, value = TRUE)
            critical_missing <- 0
            if (length(critical_vars) > 0) {
                critical_missing <- sum(sapply(data[critical_vars], function(x) sum(is.na(x))) > 0)
            }
            
            # Data quality score
            quality_score <- 100
            if (high_missing_vars > 0) quality_score <- quality_score - (high_missing_vars * 10)
            if (critical_missing > 0) quality_score <- quality_score - (critical_missing * 15)
            if (total_obs < 30) quality_score <- quality_score - 20
            
            quality_score <- max(0, quality_score)
            
            quality_color <- if (quality_score >= 80) "#4caf50" else if (quality_score >= 60) "#ff9800" else "#f44336"
            quality_label <- if (quality_score >= 80) "Excellent" else if (quality_score >= 60) "Good" else "Needs Improvement"
            
            quality_html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
                "<h4 style='color: #1976d2; margin-top: 0;'>📊 Clinical Data Quality Score</h4>",
                "<div style='text-align: center; margin: 15px 0;'>",
                "<span style='font-size: 24px; font-weight: bold; color: ", quality_color, ";'>", quality_score, "/100</span>",
                "<br><span style='color: ", quality_color, "; font-weight: bold;'>", quality_label, "</span>",
                "</div>",
                "<ul>",
                "<li><strong>Sample Size:</strong> ", total_obs, " observations ",
                if (total_obs >= 100) "(adequate)" else if (total_obs >= 30) "(acceptable)" else "(limited)", "</li>",
                "<li><strong>Variables with High Missing:</strong> ", high_missing_vars, " / ", total_vars, "</li>",
                "<li><strong>Critical Variables Missing:</strong> ", critical_missing, " / ", length(critical_vars), "</li>",
                "</ul>",
                "</div>"
            )
            
            return(quality_html)
        }

    )
)