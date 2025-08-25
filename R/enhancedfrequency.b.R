# Enhanced Frequency Analysis with BlueSky Integration
# This module provides comprehensive frequency analysis capabilities
# inspired by BlueSky's BSkyFrequency function with enhanced features

#' @title Enhanced Frequency Analysis Class
#' @importFrom R6 R6Class
enhancedfrequencyClass <- R6::R6Class(
    "enhancedfrequencyClass",
    inherit = enhancedfrequencyBase,
    private = list(
        .init = function() {
            # Initialize instructions
            private$.updateInstructions()
        },
        
        .run = function() {
            # Get data and options
            data <- self$data
            vars <- self$options$vars
            
            if (is.null(vars) || length(vars) == 0) {
                private$.setError("Please select at least one variable for analysis.")
                return()
            }
            
            # Validate data
            if (nrow(data) == 0) {
                private$.setError("Dataset is empty.")
                return()
            }
            
            # Create dataset overview
            if (self$options$showDatasetOverview) {
                private$.createDatasetOverview(data, vars)
            }
            
            # Create variable summary
            if (self$options$showVariableSummary) {
                private$.createVariableSummary(data, vars)
            }
            
            # Create individual frequency tables
            if (self$options$showFrequencyTables) {
                private$.createFrequencyTables(data, vars)
            }
            
            # Create combined summary
            if (self$options$showCombinedSummary) {
                private$.createCombinedSummary(data, vars)
            }
            
            # Create data quality assessment
            if (self$options$dataQualityAssessment) {
                private$.createDataQualityAssessment(data, vars)
            }
            
            # Create categorical diagnostics
            if (self$options$categoricalDiagnostics) {
                private$.createCategoricalDiagnostics(data, vars)
            }
            
            # Create comprehensive summary
            if (self$options$bluesky_integration && self$options$comprehensive_output) {
                private$.createComprehensiveAnalysisSummary(data, vars)
            }
            
            # Update clinical interpretation
            if (self$options$clinical_interpretation) {
                private$.updateClinicalInterpretation()
            }
            
            # Update methods explanation
            if (self$options$comprehensive_output) {
                private$.updateMethodsExplanation()
            }
        },
        
        .createDatasetOverview = function(data, vars) {
            table <- self$results$datasetOverview
            
            total_vars <- ncol(data)
            selected_vars <- length(vars)
            total_obs <- nrow(data)
            
            # Count factor/categorical variables
            factor_vars <- sum(sapply(data[vars], function(x) is.factor(x) || is.character(x)))
            
            rows <- list(
                list(characteristic = "Dataset Name", value = "Current Dataset", description = "Name of the analyzed dataset"),
                list(characteristic = "Total Variables", value = as.character(total_vars), description = "Total number of variables in dataset"),
                list(characteristic = "Selected Variables", value = as.character(selected_vars), description = "Number of variables selected for analysis"),
                list(characteristic = "Total Observations", value = as.character(total_obs), description = "Total number of rows in dataset"),
                list(characteristic = "Categorical Variables", value = as.character(factor_vars), description = "Number of categorical/factor variables selected")
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$characteristic, values = row)
            }
        },
        
        .createVariableSummary = function(data, vars) {
            table <- self$results$variableSummary
            
            for (var in vars) {
                var_data <- data[[var]]
                
                # Convert to factor if not already
                if (!is.factor(var_data)) {
                    var_data <- as.factor(var_data)
                }
                
                # Calculate statistics
                valid_data <- var_data[!is.na(var_data)]
                valid_n <- length(valid_data)
                missing_n <- sum(is.na(var_data))
                missing_pct <- (missing_n / length(var_data)) * 100
                
                categories <- length(levels(var_data))
                
                # Find most and least common categories
                freq_table <- table(valid_data)
                if (length(freq_table) > 0) {
                    most_common <- names(freq_table)[which.max(freq_table)]
                    most_common_freq <- max(freq_table)
                    least_common <- names(freq_table)[which.min(freq_table)]
                    least_common_freq <- min(freq_table)
                } else {
                    most_common <- "None"
                    most_common_freq <- 0
                    least_common <- "None"
                    least_common_freq <- 0
                }
                
                row <- list(
                    variable = var,
                    type = class(data[[var]])[1],
                    categories = categories,
                    valid_n = valid_n,
                    missing_n = missing_n,
                    missing_pct = missing_pct,
                    most_common = most_common,
                    most_common_freq = most_common_freq,
                    least_common = least_common,
                    least_common_freq = least_common_freq
                )
                
                table$addRow(rowKey = var, values = row)
            }
        },
        
        .createFrequencyTables = function(data, vars) {
            # This creates individual frequency tables for each variable
            # In a full implementation, this would create separate tables for each variable
            # For now, we'll create a simplified version
            
            for (i in seq_along(vars)) {
                var <- vars[i]
                private$.createSingleFrequencyTable(data, var, i)
            }
        },
        
        .createSingleFrequencyTable = function(data, var, index) {
            # Create individual frequency table (this would be a separate table in full implementation)
            var_data <- data[[var]]
            
            # Convert to factor if not already
            if (!is.factor(var_data)) {
                var_data <- as.factor(var_data)
            }
            
            # Apply BlueSky ordering logic
            var_data <- private$.applyOrdering(var_data)
            
            # Create frequency table with missing values if requested
            if (self$options$includeMissing) {
                freq_table <- table(var_data, useNA = "always")
                total_n <- length(var_data)
            } else {
                freq_table <- table(var_data[!is.na(var_data)])
                total_n <- sum(!is.na(var_data))
            }
            
            # Calculate percentages
            percentages <- (freq_table / total_n) * 100
            
            # Calculate valid percentages (excluding missing)
            valid_freq_table <- table(var_data[!is.na(var_data)])
            valid_n <- sum(!is.na(var_data))
            valid_percentages <- if (valid_n > 0) (valid_freq_table / valid_n) * 100 else numeric(0)
            
            # Calculate cumulative percentages
            cumulative_pct <- cumsum(percentages)
            valid_cumulative_pct <- if (length(valid_percentages) > 0) cumsum(valid_percentages) else numeric(0)
            
            # Store results (in full implementation, this would populate individual tables)
        },
        
        .applyOrdering = function(var_data) {
            order_by <- self$options$orderBy
            
            if (order_by == "var_asc") {
                # Order by variable value ascending
                if (is.factor(var_data)) {
                    var_data <- as.character(var_data)
                }
                var_data <- var_data[order(var_data, decreasing = FALSE)]
                var_data <- factor(var_data, levels = unique(var_data))
            } else if (order_by == "var_desc") {
                # Order by variable value descending
                if (is.factor(var_data)) {
                    var_data <- as.character(var_data)
                }
                var_data <- var_data[order(var_data, decreasing = TRUE)]
                var_data <- factor(var_data, levels = unique(var_data))
            }
            # For frequency ordering, we handle this in the table creation
            # For "none", we keep the original order
            
            return(var_data)
        },
        
        .createCombinedSummary = function(data, vars) {
            table <- self$results$combinedSummary
            
            for (var in vars) {
                var_data <- data[[var]]
                
                # Convert to factor if not already
                if (!is.factor(var_data)) {
                    var_data <- as.character(var_data)
                    var_data <- factor(var_data)
                }
                
                # Apply ordering
                var_data <- private$.applyOrdering(var_data)
                
                # Create frequency tables
                if (self$options$includeMissing) {
                    freq_table <- table(var_data, useNA = "always")
                    total_n <- length(var_data)
                } else {
                    freq_table <- table(var_data[!is.na(var_data)])
                    total_n <- sum(!is.na(var_data))
                }
                
                # Handle frequency ordering
                if (self$options$orderBy %in% c("freq_desc", "freq_asc")) {
                    decreasing <- self$options$orderBy == "freq_desc"
                    order_idx <- order(freq_table, decreasing = decreasing)
                    freq_table <- freq_table[order_idx]
                }
                
                # Calculate percentages
                percentages <- (freq_table / total_n) * 100
                
                # Calculate valid statistics
                valid_data <- var_data[!is.na(var_data)]
                valid_n <- length(valid_data)
                
                if (valid_n > 0) {
                    valid_freq_table <- table(valid_data)
                    if (self$options$orderBy %in% c("freq_desc", "freq_asc")) {
                        decreasing <- self$options$orderBy == "freq_desc"
                        order_idx <- order(valid_freq_table, decreasing = decreasing)
                        valid_freq_table <- valid_freq_table[order_idx]
                    }
                    valid_percentages <- (valid_freq_table / valid_n) * 100
                } else {
                    valid_freq_table <- integer(0)
                    valid_percentages <- numeric(0)
                }
                
                # Add rows for each category
                cumulative_pct <- 0
                valid_cumulative_pct <- 0
                
                for (i in seq_along(freq_table)) {
                    category <- names(freq_table)[i]
                    frequency <- as.integer(freq_table[i])
                    percentage <- percentages[i]
                    cumulative_pct <- cumulative_pct + percentage
                    
                    # Handle valid percentages
                    if (category %in% names(valid_percentages)) {
                        valid_pct <- valid_percentages[category]
                        valid_cumulative_pct <- valid_cumulative_pct + valid_pct
                    } else {
                        valid_pct <- NA
                        # Don't add to valid cumulative for missing values
                    }
                    
                    # Format missing value label
                    display_category <- if (is.na(category)) self$options$missingLabel else category
                    
                    row <- list(
                        variable = var,
                        category = display_category,
                        frequency = frequency,
                        percentage = round(percentage, self$options$percentageDecimals),
                        valid_percentage = if (!is.na(valid_pct)) round(valid_pct, self$options$percentageDecimals) else NA,
                        cumulative_percentage = round(cumulative_pct, self$options$percentageDecimals),
                        valid_cumulative_percentage = if (!is.na(valid_pct)) round(valid_cumulative_pct, self$options$percentageDecimals) else NA
                    )
                    
                    table$addRow(rowKey = paste(var, category, sep = "_"), values = row)
                }
            }
        },
        
        .createDataQualityAssessment = function(data, vars) {
            table <- self$results$dataQualityReport
            
            for (var in vars) {
                var_data <- data[[var]]
                
                # Calculate quality metrics
                total_n <- length(var_data)
                missing_n <- sum(is.na(var_data))
                completeness <- ((total_n - missing_n) / total_n)
                
                # Convert to factor for analysis
                if (!is.factor(var_data)) {
                    var_data <- factor(var_data)
                }
                
                valid_data <- var_data[!is.na(var_data)]
                unique_values <- length(unique(valid_data))
                uniqueness <- if (length(valid_data) > 0) unique_values / length(valid_data) else 0
                
                # Category balance analysis
                if (length(valid_data) > 0) {
                    freq_table <- table(valid_data)
                    max_freq <- max(freq_table)
                    min_freq <- min(freq_table)
                    balance_ratio <- if (max_freq > 0) min_freq / max_freq else 0
                    
                    if (balance_ratio > 0.7) {
                        balance <- "Well-balanced"
                    } else if (balance_ratio > 0.3) {
                        balance <- "Moderately balanced"
                    } else {
                        balance <- "Imbalanced"
                    }
                } else {
                    balance <- "No valid data"
                    balance_ratio <- 0
                }
                
                # Overall quality score (weighted average)
                quality_score <- (completeness * 0.4) + (uniqueness * 0.3) + (balance_ratio * 0.3)
                
                # Generate recommendations
                recommendations <- private$.generateQualityRecommendations(completeness, uniqueness, balance_ratio)
                
                row <- list(
                    variable = var,
                    quality_score = round(quality_score, 3),
                    completeness = round(completeness, 3),
                    uniqueness = round(uniqueness, 3),
                    balance = balance,
                    recommendations = recommendations
                )
                
                table$addRow(rowKey = var, values = row)
            }
        },
        
        .generateQualityRecommendations = function(completeness, uniqueness, balance_ratio) {
            recommendations <- character(0)
            
            if (completeness < 0.9) {
                recommendations <- c(recommendations, "Consider missing value imputation")
            }
            
            if (uniqueness > 0.9) {
                recommendations <- c(recommendations, "High uniqueness may indicate identifier variable")
            }
            
            if (balance_ratio < 0.3) {
                recommendations <- c(recommendations, "Consider category grouping for imbalanced data")
            }
            
            if (length(recommendations) == 0) {
                recommendations <- "Data quality appears good"
            } else {
                recommendations <- paste(recommendations, collapse = "; ")
            }
            
            return(recommendations)
        },
        
        .createCategoricalDiagnostics = function(data, vars) {
            table <- self$results$categoricalDiagnostics
            
            for (var in vars) {
                var_data <- data[[var]]
                
                # Convert to factor
                if (!is.factor(var_data)) {
                    var_data <- factor(var_data)
                }
                
                valid_data <- var_data[!is.na(var_data)]
                
                if (length(valid_data) == 0) {
                    next
                }
                
                # Calculate diagnostic metrics
                freq_table <- table(valid_data)
                proportions <- freq_table / sum(freq_table)
                
                # Entropy
                entropy <- -sum(proportions * log2(proportions + 1e-10))
                
                # Gini impurity
                gini_impurity <- 1 - sum(proportions^2)
                
                # Concentration ratio (proportion of most frequent category)
                dominant_category_pct <- max(proportions) * 100
                
                # Concentration ratio (Herfindahl-Hirschman Index)
                hhi <- sum(proportions^2)
                concentration_ratio <- hhi
                
                # Count rare categories (less than 5% of data)
                rare_threshold <- 0.05
                rare_categories_count <- sum(proportions < rare_threshold)
                
                # Generate interpretation
                interpretation <- private$.generateDiagnosticInterpretation(entropy, gini_impurity, dominant_category_pct, rare_categories_count)
                
                row <- list(
                    variable = var,
                    entropy = round(entropy, 3),
                    gini_impurity = round(gini_impurity, 3),
                    concentration_ratio = round(concentration_ratio, 3),
                    dominant_category_pct = round(dominant_category_pct, 1),
                    rare_categories_count = rare_categories_count,
                    interpretation = interpretation
                )
                
                table$addRow(rowKey = var, values = row)
            }
        },
        
        .generateDiagnosticInterpretation = function(entropy, gini_impurity, dominant_pct, rare_count) {
            interpretations <- character(0)
            
            # Entropy interpretation
            if (entropy > 2.5) {
                interpretations <- c(interpretations, "High diversity")
            } else if (entropy < 1.0) {
                interpretations <- c(interpretations, "Low diversity")
            } else {
                interpretations <- c(interpretations, "Moderate diversity")
            }
            
            # Dominance interpretation
            if (dominant_pct > 80) {
                interpretations <- c(interpretations, "Highly dominated by one category")
            } else if (dominant_pct > 60) {
                interpretations <- c(interpretations, "Moderately dominated")
            }
            
            # Rare categories
            if (rare_count > 3) {
                interpretations <- c(interpretations, "Many rare categories present")
            }
            
            return(paste(interpretations, collapse = "; "))
        },
        
        .createComprehensiveAnalysisSummary = function(data, vars) {
            table <- self$results$comprehensiveAnalysisSummary
            
            total_observations <- nrow(data)
            total_variables <- length(vars)
            
            # Calculate overall statistics
            total_missing <- sum(sapply(data[vars], function(x) sum(is.na(x))))
            total_cells <- total_observations * total_variables
            overall_completeness <- ((total_cells - total_missing) / total_cells) * 100
            
            # Average categories per variable
            avg_categories <- mean(sapply(data[vars], function(x) length(unique(x[!is.na(x)]))))
            
            rows <- list(
                list(
                    measure = "Total Observations",
                    value = as.character(total_observations),
                    interpretation = "Number of cases in dataset",
                    clinical_significance = "Adequate sample size important for reliable frequency estimates"
                ),
                list(
                    measure = "Variables Analyzed",
                    value = as.character(total_variables),
                    interpretation = "Number of categorical variables examined",
                    clinical_significance = "Multiple variables allow comprehensive categorical profiling"
                ),
                list(
                    measure = "Overall Completeness",
                    value = paste0(round(overall_completeness, 1), "%"),
                    interpretation = if (overall_completeness > 95) "Excellent data quality" else if (overall_completeness > 85) "Good data quality" else "Data quality concerns",
                    clinical_significance = "High completeness ensures reliable clinical insights"
                ),
                list(
                    measure = "Average Categories",
                    value = round(avg_categories, 1),
                    interpretation = if (avg_categories > 10) "High categorical complexity" else if (avg_categories > 5) "Moderate complexity" else "Low complexity",
                    clinical_significance = "Category complexity affects interpretability and statistical power"
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$measure, values = row)
            }
        },
        
        .updateInstructions = function() {
            html <- self$results$instructions
            
            content <- "
            <h3>Enhanced Frequency Analysis with BlueSky Integration</h3>
            <p>This module provides comprehensive frequency analysis for categorical variables with advanced features:</p>
            <ul>
                <li><strong>Flexible Ordering:</strong> Sort by frequency, variable value, or natural order</li>
                <li><strong>Comprehensive Percentages:</strong> Valid percentages, cumulative statistics, and missing value handling</li>
                <li><strong>Data Quality Assessment:</strong> Completeness, uniqueness, and balance metrics</li>
                <li><strong>Categorical Diagnostics:</strong> Entropy, Gini impurity, and concentration measures</li>
                <li><strong>BlueSky Integration:</strong> Enhanced statistical features inspired by BlueSky R environment</li>
            </ul>
            <p><strong>Instructions:</strong></p>
            <ol>
                <li>Select categorical variables for analysis</li>
                <li>Choose ordering method (frequency or variable value)</li>
                <li>Configure output options and percentage calculations</li>
                <li>Review results and quality assessments</li>
            </ol>
            "
            
            html$setContent(content)
        },
        
        .updateClinicalInterpretation = function() {
            html <- self$results$clinicalInterpretationGuide
            
            content <- "
            <h3>Clinical Application Guidance</h3>
            
            <h4>Interpreting Frequency Analysis Results</h4>
            <ul>
                <li><strong>Category Distribution:</strong> Examine balance between categories for clinical relevance</li>
                <li><strong>Missing Data:</strong> High missing rates may indicate data collection issues</li>
                <li><strong>Rare Categories:</strong> Low-frequency categories may need grouping for analysis</li>
                <li><strong>Data Quality:</strong> Completeness and balance affect statistical reliability</li>
            </ul>
            
            <h4>Clinical Decision Making</h4>
            <ul>
                <li><strong>Sample Size:</strong> Ensure adequate representation in each category</li>
                <li><strong>Category Grouping:</strong> Consider combining rare categories for meaningful analysis</li>
                <li><strong>Missing Values:</strong> Understand patterns and implications of missing data</li>
                <li><strong>Distribution Balance:</strong> Highly imbalanced data may require special statistical approaches</li>
            </ul>
            
            <h4>Quality Metrics Interpretation</h4>
            <ul>
                <li><strong>Entropy:</strong> Higher values indicate more diverse distributions</li>
                <li><strong>Gini Impurity:</strong> Measures category impurity (0 = pure, 0.5 = maximum impurity for binary)</li>
                <li><strong>Concentration Ratio:</strong> Proportion of dominant category</li>
                <li><strong>Completeness:</strong> Proportion of non-missing values</li>
            </ul>
            "
            
            html$setContent(content)
        },
        
        .updateMethodsExplanation = function() {
            html <- self$results$methodsExplanation
            
            content <- "
            <h3>Statistical Methods and References</h3>
            
            <h4>Frequency Analysis Methods</h4>
            <ul>
                <li><strong>Basic Frequencies:</strong> Simple count tabulation with percentage calculations</li>
                <li><strong>Valid Percentages:</strong> Percentages calculated excluding missing values</li>
                <li><strong>Cumulative Statistics:</strong> Running totals of frequencies and percentages</li>
                <li><strong>Ordering Methods:</strong> Frequency-based or value-based sorting algorithms</li>
            </ul>
            
            <h4>Quality Assessment Metrics</h4>
            <ul>
                <li><strong>Shannon Entropy:</strong> H = -Σ(p_i * log₂(p_i)) - measures information content</li>
                <li><strong>Gini Impurity:</strong> G = 1 - Σ(p_i²) - measures probability of misclassification</li>
                <li><strong>Herfindahl-Hirschman Index:</strong> HHI = Σ(p_i²) - concentration measure</li>
                <li><strong>Completeness Ratio:</strong> (Total - Missing) / Total - data completeness measure</li>
            </ul>
            
            <h4>BlueSky Integration Features</h4>
            <ul>
                <li><strong>Enhanced Ordering:</strong> Flexible sorting options inspired by BSkyFrequency</li>
                <li><strong>Missing Value Handling:</strong> Comprehensive missing data analysis</li>
                <li><strong>Statistical Diagnostics:</strong> Advanced categorical variable assessment</li>
                <li><strong>Quality Metrics:</strong> Data quality scoring and recommendations</li>
            </ul>
            
            <h4>References</h4>
            <ul>
                <li>Shannon, C.E. (1948). A mathematical theory of communication. Bell System Technical Journal.</li>
                <li>Gini, C. (1912). Measurement of Inequality of Incomes. The Economic Journal.</li>
                <li>BlueSky Statistics Documentation - BSkyFrequency Function</li>
                <li>R Core Team. R: A Language and Environment for Statistical Computing</li>
            </ul>
            "
            
            html$setContent(content)
        },
        
        .setError = function(message) {
            # Set error state
            html <- self$results$instructions
            error_content <- paste0("<div style='color: red;'><strong>Error:</strong> ", message, "</div>")
            html$setContent(error_content)
        }
    )
)