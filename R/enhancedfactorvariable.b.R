# Enhanced Factor Variable Analysis with BlueSky Integration
# This module provides comprehensive factor variable analysis capabilities
# inspired by BlueSky's BSkyFactorVariableAnalysis function with enhanced features

#' @title Enhanced Factor Variable Analysis Class
#' @importFrom R6 R6Class
enhancedfactorvariableClass <- R6::R6Class(
    "enhancedfactorvariableClass",
    inherit = enhancedfactorvariableBase,
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
                private$.setError("Please select at least one variable for factor analysis.")
                return()
            }
            
            # Validate data
            if (nrow(data) == 0) {
                private$.setError("Dataset is empty.")
                return()
            }
            
            # Check if any variables are factor or can be converted
            non_factor_vars <- private$.checkFactorVariables(data, vars)
            if (length(non_factor_vars) > 0) {
                warning_msg <- paste("Converting non-factor variables to factors:", 
                                   paste(non_factor_vars, collapse = ", "))
                # Note: In a full implementation, we could show this as a warning in the UI
            }
            
            # Create dataset overview
            if (self$options$showDatasetOverview) {
                private$.createDatasetOverview(data, vars)
            }
            
            # Create nominal variable summary
            if (self$options$showNominalSummary) {
                private$.createNominalSummary(data, vars)
            }
            
            # Create detailed levels analysis
            if (self$options$showDetailedLevels) {
                private$.createDetailedLevelsAnalysis(data, vars)
            }
            
            # Create combined analysis
            if (self$options$showCombinedAnalysis) {
                private$.createCombinedFactorAnalysis(data, vars)
            }
            
            # Create complexity analysis
            if (self$options$factorComplexityAnalysis) {
                private$.createComplexityAnalysis(data, vars)
            }
            
            # Create balance analysis
            if (self$options$levelBalanceAnalysis) {
                private$.createLevelBalanceAnalysis(data, vars)
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
        
        .checkFactorVariables = function(data, vars) {
            non_factor_vars <- character(0)
            
            for (var in vars) {
                if (!is.factor(data[[var]])) {
                    non_factor_vars <- c(non_factor_vars, var)
                }
            }
            
            return(non_factor_vars)
        },
        
        .createDatasetOverview = function(data, vars) {
            table <- self$results$datasetOverview
            
            total_vars <- ncol(data)
            selected_vars <- length(vars)
            total_obs <- nrow(data)
            
            # Count factor variables in dataset
            factor_vars_all <- sum(sapply(data, is.factor))
            # Count factor variables in selected
            factor_vars_selected <- sum(sapply(data[vars], function(x) is.factor(x) || is.character(x)))
            
            rows <- list(
                list(
                    characteristic = "Dataset Name",
                    value = "Current Dataset", 
                    description = "Name of the analyzed dataset"
                ),
                list(
                    characteristic = "Total Variables",
                    value = as.character(total_vars),
                    description = "Total number of variables in dataset"
                ),
                list(
                    characteristic = "Selected Variables", 
                    value = as.character(selected_vars),
                    description = "Number of variables selected for analysis"
                ),
                list(
                    characteristic = "Total Observations",
                    value = as.character(total_obs),
                    description = "Total number of rows in dataset"
                ),
                list(
                    characteristic = "Factor Variables (Dataset)",
                    value = as.character(factor_vars_all),
                    description = "Number of factor variables in entire dataset"
                ),
                list(
                    characteristic = "Factor Variables (Selected)",
                    value = as.character(factor_vars_selected),
                    description = "Number of factor/categorical variables selected"
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$characteristic, values = row)
            }
        },
        
        .createNominalSummary = function(data, vars) {
            table <- self$results$nominalSummary
            
            for (var in vars) {
                var_data <- data[[var]]
                
                # Convert to factor if not already
                if (!is.factor(var_data)) {
                    var_data <- as.factor(var_data)
                }
                
                # Calculate basic statistics
                observations <- length(var_data)
                non_nas <- sum(!is.na(var_data))
                nas <- sum(is.na(var_data))
                missing_percent <- (nas / observations) * 100
                factor_levels <- length(levels(var_data))
                
                # Find most and least frequent levels
                if (non_nas > 0) {
                    level_counts <- table(var_data[!is.na(var_data)])
                    most_frequent <- names(level_counts)[which.max(level_counts)]
                    most_frequent_count <- max(level_counts)
                    least_frequent <- names(level_counts)[which.min(level_counts)]
                    least_frequent_count <- min(level_counts)
                } else {
                    most_frequent <- "None"
                    most_frequent_count <- 0
                    least_frequent <- "None"
                    least_frequent_count <- 0
                }
                
                row <- list(
                    variable = var,
                    observations = observations,
                    non_nas = non_nas,
                    nas = nas,
                    missing_percent = round(missing_percent, 1),
                    factor_levels = factor_levels,
                    most_frequent = most_frequent,
                    most_frequent_count = most_frequent_count,
                    least_frequent = least_frequent,
                    least_frequent_count = least_frequent_count
                )
                
                table$addRow(rowKey = var, values = row)
            }
        },
        
        .createDetailedLevelsAnalysis = function(data, vars) {
            table <- self$results$detailedLevelsAnalysis
            
            for (var in vars) {
                var_data <- data[[var]]
                
                # Convert to factor if not already
                if (!is.factor(var_data)) {
                    var_data <- as.factor(var_data)
                }
                
                # Create frequency table
                if (self$options$includeMissing) {
                    freq_table <- table(var_data, useNA = "always")
                    total_n <- length(var_data)
                } else {
                    freq_table <- table(var_data[!is.na(var_data)])
                    total_n <- sum(!is.na(var_data))
                }
                
                # Apply BlueSky sorting and filtering
                processed_table <- private$.processFactorTable(freq_table, var_data)
                
                # Add rows for each level
                cumulative_count <- 0
                cumulative_pct <- 0
                rank <- 1
                
                for (level in names(processed_table)) {
                    count <- as.integer(processed_table[level])
                    percentage <- (count / total_n) * 100
                    
                    # Calculate valid percentage (excluding missing)
                    valid_n <- sum(!is.na(var_data))
                    if (!is.na(level) && valid_n > 0) {
                        valid_pct <- (count / valid_n) * 100
                    } else {
                        valid_pct <- NA
                    }
                    
                    cumulative_count <- cumulative_count + count
                    cumulative_pct <- cumulative_pct + percentage
                    
                    # Handle missing value display
                    display_level <- if (is.na(level)) self$options$missingLabel else level
                    
                    # Apply filters
                    if (private$.shouldIncludeLevel(count, percentage)) {
                        row <- list(
                            variable = var,
                            factor_level = display_level,
                            count = count,
                            percentage = round(percentage, 1),
                            valid_percentage = if (!is.na(valid_pct)) round(valid_pct, 1) else NA,
                            cumulative_count = if (self$options$includeCumulativeStats) cumulative_count else NA,
                            cumulative_percentage = if (self$options$includeCumulativeStats) round(cumulative_pct, 1) else NA,
                            rank = rank
                        )
                        
                        table$addRow(rowKey = paste(var, level, sep = "_"), values = row)
                        rank <- rank + 1
                    }
                }
            }
        },
        
        .processFactorTable = function(freq_table, original_data) {
            # Apply BlueSky-style processing
            
            # Convert to data frame for easier manipulation
            df <- data.frame(
                level = names(freq_table),
                count = as.integer(freq_table),
                stringsAsFactors = FALSE
            )
            
            # Apply sorting based on BlueSky algorithm
            if (self$options$sortingMethod == "freq_desc") {
                df <- df[order(df$count, decreasing = TRUE), ]
            } else if (self$options$sortingMethod == "freq_asc") {
                df <- df[order(df$count, decreasing = FALSE), ]
            } else if (self$options$sortingMethod == "name_asc") {
                df <- df[order(df$level), ]
            } else if (self$options$sortingMethod == "name_desc") {
                df <- df[order(df$level, decreasing = TRUE), ]
            }
            # For "original", keep the original order
            
            # Apply top N filtering (BlueSky feature)
            if (self$options$showOnlyTopFactors) {
                max_rows <- min(nrow(df), self$options$maxTopFactors)
                df <- df[1:max_rows, ]
            }
            
            # Convert back to named vector
            result <- setNames(df$count, df$level)
            return(result)
        },
        
        .shouldIncludeLevel = function(count, percentage) {
            # Apply minimum filters
            if (count < self$options$minimumCount) return(FALSE)
            if (percentage < self$options$minimumPercentage) return(FALSE)
            
            return(TRUE)
        },
        
        .createCombinedFactorAnalysis = function(data, vars) {
            table <- self$results$combinedFactorAnalysis
            
            global_rank <- 1
            all_levels <- list()
            
            # Collect all levels from all variables
            for (var in vars) {
                var_data <- data[[var]]
                if (!is.factor(var_data)) {
                    var_data <- as.factor(var_data)
                }
                
                freq_table <- table(var_data, useNA = ifelse(self$options$includeMissing, "always", "no"))
                processed_table <- private$.processFactorTable(freq_table, var_data)
                
                for (level in names(processed_table)) {
                    all_levels[[length(all_levels) + 1]] <- list(
                        variable = var,
                        level = level,
                        count = processed_table[level],
                        percentage = (processed_table[level] / length(var_data)) * 100
                    )
                }
            }
            
            # Sort all levels globally if requested
            if (self$options$sortingMethod %in% c("freq_desc", "freq_asc")) {
                decreasing <- self$options$sortingMethod == "freq_desc"
                all_levels <- all_levels[order(sapply(all_levels, function(x) x$count), decreasing = decreasing)]
            }
            
            # Add rows
            for (item in all_levels) {
                # Calculate rank within variable
                var_levels <- all_levels[sapply(all_levels, function(x) x$variable == item$variable)]
                category_rank <- which(sapply(var_levels, function(x) identical(x, item)))
                
                display_level <- if (is.na(item$level)) self$options$missingLabel else item$level
                
                row <- list(
                    variable = item$variable,
                    level = display_level,
                    count = as.integer(item$count),
                    percentage = round(item$percentage, 1),
                    category_rank = category_rank,
                    global_rank = global_rank
                )
                
                table$addRow(rowKey = paste(item$variable, item$level, global_rank, sep = "_"), values = row)
                global_rank <- global_rank + 1
            }
        },
        
        .createComplexityAnalysis = function(data, vars) {
            table <- self$results$complexityAnalysis
            
            for (var in vars) {
                var_data <- data[[var]]
                if (!is.factor(var_data)) {
                    var_data <- as.factor(var_data)
                }
                
                valid_data <- var_data[!is.na(var_data)]
                if (length(valid_data) == 0) next
                
                freq_table <- table(valid_data)
                proportions <- freq_table / sum(freq_table)
                
                # Shannon Entropy
                shannon_entropy <- -sum(proportions * log2(proportions + 1e-10))
                
                # Simpson Diversity Index
                simpson_diversity <- 1 - sum(proportions^2)
                
                # Berger-Parker Dominance Index
                berger_parker <- max(proportions)
                
                # Effective number of categories (exponential of Shannon entropy)
                effective_categories <- 2^shannon_entropy
                
                # Complexity score (normalized combination of metrics)
                complexity_score <- (shannon_entropy / log2(length(proportions)) * 0.4 + 
                                   simpson_diversity * 0.3 + 
                                   (1 - berger_parker) * 0.3)
                
                # Generate interpretation
                interpretation <- private$.interpretComplexity(shannon_entropy, simpson_diversity, berger_parker, length(proportions))
                
                row <- list(
                    variable = var,
                    shannon_entropy = round(shannon_entropy, 3),
                    simpson_diversity = round(simpson_diversity, 3),
                    berger_parker_dominance = round(berger_parker, 3),
                    effective_categories = round(effective_categories, 1),
                    complexity_score = round(complexity_score, 3),
                    interpretation = interpretation
                )
                
                table$addRow(rowKey = var, values = row)
            }
        },
        
        .interpretComplexity = function(entropy, simpson, berger_parker, n_levels) {
            interpretations <- character(0)
            
            # Entropy interpretation
            max_entropy <- log2(n_levels)
            entropy_ratio <- entropy / max_entropy
            
            if (entropy_ratio > 0.8) {
                interpretations <- c(interpretations, "High diversity")
            } else if (entropy_ratio < 0.3) {
                interpretations <- c(interpretations, "Low diversity")
            } else {
                interpretations <- c(interpretations, "Moderate diversity")
            }
            
            # Dominance interpretation
            if (berger_parker > 0.8) {
                interpretations <- c(interpretations, "Highly dominated")
            } else if (berger_parker < 0.4) {
                interpretations <- c(interpretations, "Well-balanced")
            }
            
            # Simpson interpretation
            if (simpson > 0.8) {
                interpretations <- c(interpretations, "Complex distribution")
            }
            
            return(paste(interpretations, collapse = "; "))
        },
        
        .createLevelBalanceAnalysis = function(data, vars) {
            table <- self$results$levelBalanceAnalysis
            
            for (var in vars) {
                var_data <- data[[var]]
                if (!is.factor(var_data)) {
                    var_data <- as.factor(var_data)
                }
                
                valid_data <- var_data[!is.na(var_data)]
                if (length(valid_data) == 0) next
                
                freq_table <- table(valid_data)
                counts <- as.numeric(freq_table)
                proportions <- counts / sum(counts)
                
                # Gini Coefficient
                gini_coeff <- private$.calculateGini(counts)
                
                # Coefficient of Variation
                cv <- sd(counts) / mean(counts)
                
                # Balance Ratio (min/max)
                balance_ratio <- min(counts) / max(counts)
                
                # Dominant level percentage
                dominant_level_pct <- max(proportions) * 100
                
                # Count rare levels (< 5%)
                rare_levels_count <- sum(proportions < 0.05)
                
                # Balance category
                balance_category <- private$.categorizeBalance(gini_coeff, balance_ratio, dominant_level_pct)
                
                # Recommendation
                recommendation <- private$.generateBalanceRecommendation(balance_category, rare_levels_count, length(counts))
                
                row <- list(
                    variable = var,
                    gini_coefficient = round(gini_coeff, 3),
                    coefficient_variation = round(cv, 3),
                    balance_ratio = round(balance_ratio, 3),
                    dominant_level_pct = round(dominant_level_pct, 1),
                    rare_levels_count = rare_levels_count,
                    balance_category = balance_category,
                    recommendation = recommendation
                )
                
                table$addRow(rowKey = var, values = row)
            }
        },
        
        .calculateGini = function(x) {
            # Calculate Gini coefficient
            x <- sort(x)
            n <- length(x)
            index <- 1:n
            gini <- (2 * sum(index * x)) / (n * sum(x)) - (n + 1) / n
            return(gini)
        },
        
        .categorizeBalance = function(gini, balance_ratio, dominant_pct) {
            if (gini < 0.3 && balance_ratio > 0.5 && dominant_pct < 50) {
                return("Well-balanced")
            } else if (gini > 0.7 || balance_ratio < 0.1 || dominant_pct > 80) {
                return("Highly imbalanced")
            } else {
                return("Moderately balanced")
            }
        },
        
        .generateBalanceRecommendation = function(balance_category, rare_count, total_levels) {
            if (balance_category == "Highly imbalanced") {
                if (rare_count > total_levels * 0.5) {
                    return("Consider grouping rare categories; highly imbalanced distribution")
                } else {
                    return("Consider stratified sampling or weighted analysis")
                }
            } else if (rare_count > 3) {
                return("Consider grouping rare categories for statistical analysis")
            } else {
                return("Distribution suitable for standard analysis")
            }
        },
        
        .createComprehensiveAnalysisSummary = function(data, vars) {
            table <- self$results$comprehensiveAnalysisSummary
            
            total_observations <- nrow(data)
            total_variables <- length(vars)
            
            # Calculate overall statistics
            total_levels <- sum(sapply(data[vars], function(x) {
                if (!is.factor(x)) x <- factor(x)
                length(levels(x))
            }))
            
            avg_levels_per_var <- total_levels / total_variables
            
            # Calculate overall missing rate
            total_missing <- sum(sapply(data[vars], function(x) sum(is.na(x))))
            total_cells <- total_observations * total_variables
            overall_completeness <- ((total_cells - total_missing) / total_cells) * 100
            
            rows <- list(
                list(
                    measure = "Total Observations",
                    value = as.character(total_observations),
                    interpretation = "Sample size for factor analysis",
                    clinical_significance = "Adequate sample size important for reliable factor level estimates"
                ),
                list(
                    measure = "Factor Variables Analyzed", 
                    value = as.character(total_variables),
                    interpretation = "Number of categorical variables examined",
                    clinical_significance = "Multiple factors allow comprehensive categorical profiling"
                ),
                list(
                    measure = "Total Factor Levels",
                    value = as.character(total_levels),
                    interpretation = "Sum of all categories across variables",
                    clinical_significance = "High level count may require category grouping for analysis"
                ),
                list(
                    measure = "Average Levels per Variable",
                    value = round(avg_levels_per_var, 1),
                    interpretation = if (avg_levels_per_var > 10) "High categorical complexity" else if (avg_levels_per_var > 5) "Moderate complexity" else "Low complexity",
                    clinical_significance = "Complexity affects statistical power and interpretability"
                ),
                list(
                    measure = "Overall Completeness",
                    value = paste0(round(overall_completeness, 1), "%"),
                    interpretation = if (overall_completeness > 95) "Excellent data quality" else if (overall_completeness > 85) "Good data quality" else "Data quality concerns",
                    clinical_significance = "High completeness ensures reliable categorical analysis"
                )
            )
            
            for (row in rows) {
                table$addRow(rowKey = row$measure, values = row)
            }
        },
        
        .updateInstructions = function() {
            html <- self$results$instructions
            
            content <- "
            <h3>Enhanced Factor Variable Analysis with BlueSky Integration</h3>
            <p>This module provides comprehensive analysis of categorical/factor variables with advanced features:</p>
            <ul>
                <li><strong>Top N Display:</strong> Show only the most frequent factor levels (BlueSky feature)</li>
                <li><strong>Flexible Sorting:</strong> Sort by frequency, level name, or original order</li>
                <li><strong>Detailed Statistics:</strong> Counts, percentages, cumulative statistics, and rankings</li>
                <li><strong>Complexity Analysis:</strong> Shannon entropy, Simpson diversity, and dominance measures</li>
                <li><strong>Balance Assessment:</strong> Gini coefficient, balance ratios, and distribution analysis</li>
                <li><strong>Clinical Context:</strong> Factor-specific recommendations for medical research</li>
            </ul>
            <p><strong>Instructions:</strong></p>
            <ol>
                <li>Select factor/categorical variables for analysis</li>
                <li>Configure top N display and sorting preferences</li>
                <li>Choose statistical options and output formats</li>
                <li>Review detailed level analysis and complexity metrics</li>
                <li>Use balance analysis for sampling and analysis planning</li>
            </ol>
            "
            
            html$setContent(content)
        },
        
        .updateClinicalInterpretation = function() {
            html <- self$results$clinicalInterpretationGuide
            
            content <- "
            <h3>Clinical Application Guidance for Factor Variable Analysis</h3>
            
            <h4>Interpreting Factor Analysis Results</h4>
            <ul>
                <li><strong>Level Distribution:</strong> Examine balance of categories for adequate representation</li>
                <li><strong>Missing Data:</strong> High missing rates may indicate data collection issues or systematic bias</li>
                <li><strong>Rare Categories:</strong> Low-frequency levels may need grouping or special handling</li>
                <li><strong>Complexity Metrics:</strong> High entropy indicates diverse categories; low entropy suggests dominance</li>
            </ul>
            
            <h4>Clinical Decision Making</h4>
            <ul>
                <li><strong>Sample Size Planning:</strong> Ensure adequate cases in each category for statistical power</li>
                <li><strong>Category Grouping:</strong> Consider combining rare categories for meaningful clinical analysis</li>
                <li><strong>Stratification:</strong> Use balance metrics to plan stratified analyses</li>
                <li><strong>Variable Selection:</strong> High complexity variables may require careful interpretation</li>
            </ul>
            
            <h4>Balance Analysis Interpretation</h4>
            <ul>
                <li><strong>Well-balanced:</strong> Suitable for standard statistical analyses</li>
                <li><strong>Moderately balanced:</strong> May benefit from weighted analysis or stratification</li>
                <li><strong>Highly imbalanced:</strong> Consider specialized methods for imbalanced data</li>
                <li><strong>Gini Coefficient:</strong> 0 = perfect equality, 1 = maximum inequality</li>
            </ul>
            
            <h4>Complexity Metrics for Clinical Research</h4>
            <ul>
                <li><strong>Shannon Entropy:</strong> Higher values indicate more information content in categories</li>
                <li><strong>Simpson Diversity:</strong> Probability that two randomly selected cases are from different categories</li>
                <li><strong>Effective Categories:</strong> Number of equally frequent categories that would give the same entropy</li>
                <li><strong>Dominance Index:</strong> Proportion of most frequent category (clinical significance varies by context)</li>
            </ul>
            "
            
            html$setContent(content)
        },
        
        .updateMethodsExplanation = function() {
            html <- self$results$methodsExplanation
            
            content <- "
            <h3>Statistical Methods and References</h3>
            
            <h4>BlueSky Integration Methods</h4>
            <ul>
                <li><strong>Factor Level Analysis:</strong> Direct adaptation of BSkyFactorVariableAnalysis algorithms</li>
                <li><strong>Top N Display:</strong> BlueSky's show.only.top.factors with max.number.top.factors</li>
                <li><strong>Sorting Algorithms:</strong> Frequency-based and alphabetical sorting methods</li>
                <li><strong>Data Processing:</strong> BlueSky's factor validation and conversion methods</li>
            </ul>
            
            <h4>Diversity and Complexity Measures</h4>
            <ul>
                <li><strong>Shannon Entropy:</strong> H = -Σ(p_i * log₂(p_i)) - measures category information content</li>
                <li><strong>Simpson Diversity:</strong> D = 1 - Σ(p_i²) - probability of different category selection</li>
                <li><strong>Berger-Parker Dominance:</strong> d = max(p_i) - proportion of most abundant category</li>
                <li><strong>Effective Categories:</strong> exp(H) - number of equally frequent categories</li>
            </ul>
            
            <h4>Balance and Inequality Measures</h4>
            <ul>
                <li><strong>Gini Coefficient:</strong> G = (2Σ(i×x_i))/(n×Σx_i) - (n+1)/n - inequality measure</li>
                <li><strong>Coefficient of Variation:</strong> CV = σ/μ - relative variability measure</li>
                <li><strong>Balance Ratio:</strong> min(counts)/max(counts) - relative frequency balance</li>
                <li><strong>Rare Category Threshold:</strong> Categories with <5% frequency (adjustable)</li>
            </ul>
            
            <h4>Clinical Applications</h4>
            <ul>
                <li><strong>Pathology Grading:</strong> Balance analysis for grade distribution assessment</li>
                <li><strong>Biomarker Categories:</strong> Complexity analysis for biomarker classification systems</li>
                <li><strong>Treatment Groups:</strong> Balance assessment for clinical trial stratification</li>
                <li><strong>Diagnostic Categories:</strong> Diversity measures for diagnostic classification evaluation</li>
            </ul>
            
            <h4>References</h4>
            <ul>
                <li>Shannon, C.E. (1948). A Mathematical Theory of Communication. Bell System Technical Journal.</li>
                <li>Simpson, E.H. (1949). Measurement of Diversity. Nature, 163, 688.</li>
                <li>Berger, W.H. & Parker, F.L. (1970). Diversity of Planktonic Foraminifera. Science, 168, 1345-1347.</li>
                <li>Gini, C. (1912). Measurement of Inequality of Incomes. The Economic Journal, 22, 124-126.</li>
                <li>BlueSky Statistics Documentation - BSkyFactorVariableAnalysis Function Reference</li>
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