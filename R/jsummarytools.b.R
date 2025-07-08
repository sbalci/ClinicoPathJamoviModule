
# This file is a generated template, your changes will not be overwritten

# Comprehensive descriptive statistics using summarytools package
# Following jamovi naming convention with j-prefix to avoid namespace conflicts

jsummarytoolsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jsummarytoolsClass",
    inherit = jsummarytoolsBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .cached_results = NULL,
        
        .init = function() {
            # Initialize with instructions
            self$results$instructions$setVisible(TRUE)
            self$results$summary_output$setVisible(TRUE)
            self$results$interpretation$setVisible(TRUE)
            
            # Set visibility based on analysis type
            analysis_type <- self$options$analysis_type
            self$results$data_summary_table$setVisible(analysis_type == "dfsummary")
            self$results$frequency_table$setVisible(analysis_type == "freq")
            self$results$descriptive_stats$setVisible(analysis_type == "descr")
            self$results$crosstab_output$setVisible(analysis_type == "ctable")
        },
        
        # Performance optimization methods
        .calculateDataHash = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(NULL)
            }
            
            # Create hash based on relevant data and analysis type
            analysis_type <- self$options$analysis_type
            vars <- self$options$vars
            group_var <- self$options$group_var
            weights_var <- self$options$weights_var
            cross_var1 <- self$options$cross_var1
            cross_var2 <- self$options$cross_var2
            
            # Determine relevant variables based on analysis type
            relevant_vars <- switch(analysis_type,
                "dfsummary" = if (length(vars) > 0) vars else names(self$data),
                "freq" = if (length(vars) > 0) vars else names(self$data),
                "descr" = if (length(vars) > 0) vars else names(self$data)[sapply(self$data, is.numeric)],
                "ctable" = c(cross_var1, cross_var2),
                names(self$data)
            )
            
            # Add grouping and weight variables if specified
            if (!is.null(group_var)) relevant_vars <- c(relevant_vars, group_var)
            if (!is.null(weights_var)) relevant_vars <- c(relevant_vars, weights_var)
            
            # Remove NULLs and ensure variables exist
            relevant_vars <- relevant_vars[!sapply(relevant_vars, is.null)]
            relevant_vars <- relevant_vars[relevant_vars %in% names(self$data)]
            
            if (length(relevant_vars) == 0) {
                return(NULL)
            }
            
            # Create hash string
            data_summary <- paste(
                analysis_type,
                nrow(self$data),
                ncol(self$data),
                paste(relevant_vars, collapse = "_"),
                paste(sapply(relevant_vars, function(var) {
                    if (is.numeric(self$data[[var]])) {
                        paste(range(self$data[[var]], na.rm = TRUE), collapse = "_")
                    } else {
                        paste(length(unique(self$data[[var]])), "levels")
                    }
                }), collapse = "_"),
                sep = "_"
            )
            
            return(data_summary)
        },
        
        .calculateOptionsHash = function() {
            # Create hash of all relevant options
            options_list <- list(
                analysis_type = self$options$analysis_type,
                vars = paste(self$options$vars, collapse = "_"),
                group_var = self$options$group_var,
                weights_var = self$options$weights_var,
                cross_var1 = self$options$cross_var1,
                cross_var2 = self$options$cross_var2,
                show_labels = self$options$show_labels,
                show_variable_numbers = self$options$show_variable_numbers,
                show_graphs = self$options$show_graphs,
                show_valid_counts = self$options$show_valid_counts,
                show_na_counts = self$options$show_na_counts,
                round_digits = self$options$round_digits,
                max_distinct_values = self$options$max_distinct_values,
                include_cumulative = self$options$include_cumulative,
                report_missing = self$options$report_missing,
                transpose_output = self$options$transpose_output,
                stats_to_include = self$options$stats_to_include,
                cross_proportions = self$options$cross_proportions,
                output_style = self$options$output_style,
                plain_ascii = self$options$plain_ascii
            )
            
            return(paste(options_list, collapse = "_"))
        },
        
        .canUseCache = function() {
            current_data_hash <- private$.calculateDataHash()
            current_options_hash <- private$.calculateOptionsHash()
            
            return(!is.null(private$.cached_results) &&
                   !is.null(private$.data_hash) &&
                   !is.null(private$.options_hash) &&
                   !is.null(current_data_hash) &&
                   !is.null(current_options_hash) &&
                   current_data_hash == private$.data_hash &&
                   current_options_hash == private$.options_hash)
        },
        
        .prepareData = function() {
            current_hash <- private$.calculateDataHash()
            
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                # Data has changed, prepare new data
                data <- self$data
                
                if (is.null(data) || nrow(data) == 0) {
                    private$.prepared_data <- NULL
                    private$.data_hash <- current_hash
                    return(NULL)
                }
                
                private$.prepared_data <- data
                private$.data_hash <- current_hash
            }
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            current_hash <- private$.calculateOptionsHash()
            
            if (is.null(private$.options_hash) || private$.options_hash != current_hash) {
                private$.prepared_options <- self$options
                private$.options_hash <- current_hash
                
                # Clear cached results when options change
                private$.cached_results <- NULL
            }
            
            return(private$.prepared_options)
        },
        
        .run = function() {
            
            # Check if summarytools package is available
            if (!requireNamespace('summarytools', quietly = TRUE)) {
                stop('The summarytools package is required but not installed. Please install it using install.packages("summarytools")')
            }
            
            # Performance optimization: check if we can use cached results
            if (private$.canUseCache()) {
                # Use cached results
                if (!is.null(private$.cached_results$summary_output)) {
                    self$results$summary_output$setContent(private$.cached_results$summary_output)
                }
                if (!is.null(private$.cached_results$data_summary_table)) {
                    private$.populateDataSummaryTable(private$.cached_results$data_summary_table)
                }
                if (!is.null(private$.cached_results$frequency_table)) {
                    private$.populateFrequencyTable(private$.cached_results$frequency_table)
                }
                if (!is.null(private$.cached_results$descriptive_stats)) {
                    private$.populateDescriptiveStats(private$.cached_results$descriptive_stats)
                }
                if (!is.null(private$.cached_results$crosstab_output)) {
                    self$results$crosstab_output$setContent(private$.cached_results$crosstab_output)
                }
                return()
            }
            
            # Prepare data and options with caching
            data <- private$.prepareData()
            options <- private$.prepareOptions()
            
            if (is.null(data) || nrow(data) == 0) {
                return()
            }
            
            # Set instructions
            self$results$instructions$setContent(private$.generateInstructions())
            
            # Execute analysis based on type
            analysis_type <- options$analysis_type
            
            tryCatch({
                cached_results <- list()
                
                if (analysis_type == 'dfsummary') {
                    result <- private$.runDfSummary(data, options)
                    cached_results$summary_output <- result$html
                    cached_results$data_summary_table <- result$table_data
                    
                } else if (analysis_type == 'freq') {
                    result <- private$.runFrequency(data, options)
                    cached_results$summary_output <- result$html
                    cached_results$frequency_table <- result$table_data
                    
                } else if (analysis_type == 'descr') {
                    result <- private$.runDescriptive(data, options)
                    cached_results$summary_output <- result$html
                    cached_results$descriptive_stats <- result$table_data
                    
                } else if (analysis_type == 'ctable') {
                    result <- private$.runCrossTable(data, options)
                    cached_results$summary_output <- result$html
                    cached_results$crosstab_output <- result$html
                }
                
                # Cache the results
                private$.cached_results <- cached_results
                
                # Set interpretation
                if (options$show_interpretation) {
                    self$results$interpretation$setContent(private$.generateInterpretation(analysis_type))
                }
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in summarytools analysis:', e$message))
            })
        },
        
        .runDfSummary = function(data, options) {
            
            # Select variables
            if (length(options$vars) > 0) {
                data_subset <- data[options$vars]
            } else {
                data_subset <- data
            }
            
            # Configure summarytools options
            old_options <- summarytools::st_options()
            on.exit(do.call(summarytools::st_options, old_options))
            
            summarytools::st_options(
                plain.ascii = options$plain_ascii,
                style = options$output_style,
                dfSummary.style = options$output_style,
                dfSummary.varnumbers = options$show_variable_numbers,
                dfSummary.labels.col = options$show_labels,
                dfSummary.graph.col = options$show_graphs,
                dfSummary.valid.col = options$show_valid_counts,
                dfSummary.na.col = options$show_na_counts,
                dfSummary.graph.magnif = 0.8,
                bootstrap.css = options$bootstrap_css,
                headings = options$headings,
                escape.pipe = options$escape_pipe
            )
            
            # Generate dfSummary
            if (!is.null(options$group_var) && options$group_var %in% names(data)) {
                df_summary <- summarytools::stby(data_subset, 
                                               data[[options$group_var]], 
                                               summarytools::dfSummary)
            } else {
                df_summary <- summarytools::dfSummary(data_subset)
            }
            
            # Convert to HTML
            html_output <- summarytools::view(df_summary, 
                                            method = "render",
                                            headings = options$headings,
                                            bootstrap.css = options$bootstrap_css)
            
            # Create table data for jamovi table
            table_data <- private$.extractDfSummaryTableData(df_summary, data_subset)
            
            # Set HTML output
            self$results$summary_output$setContent(as.character(html_output))
            
            # Populate table
            if (!is.null(table_data)) {
                private$.populateDataSummaryTable(table_data)
            }
            
            return(list(html = as.character(html_output), table_data = table_data))
        },
        
        .runFrequency = function(data, options) {
            
            # Select variables for frequency analysis
            if (length(options$vars) > 0) {
                freq_vars <- options$vars
            } else {
                # Use categorical/factor variables by default
                freq_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
                if (length(freq_vars) == 0) {
                    freq_vars <- names(data)[1:min(3, ncol(data))] # Use first 3 variables if no categorical
                }
            }
            
            if (length(freq_vars) == 0) {
                jmvcore::reject("No suitable variables found for frequency analysis")
                return(NULL)
            }
            
            # Configure summarytools options
            old_options <- summarytools::st_options()
            on.exit(do.call(summarytools::st_options, old_options))
            
            summarytools::st_options(
                plain.ascii = options$plain_ascii,
                style = options$output_style,
                freq.cumul = options$include_cumulative,
                freq.report.nas = options$report_missing,
                headings = options$headings,
                bootstrap.css = options$bootstrap_css
            )
            
            # Generate frequency tables
            freq_list <- list()
            html_parts <- character(0)
            
            for (var in freq_vars[1:min(length(freq_vars), 5)]) {  # Limit to 5 variables
                if (var %in% names(data)) {
                    if (!is.null(options$group_var) && options$group_var %in% names(data)) {
                        freq_result <- summarytools::stby(data[[var]], 
                                                        data[[options$group_var]], 
                                                        summarytools::freq)
                    } else {
                        freq_result <- summarytools::freq(data[[var]])
                    }
                    
                    freq_list[[var]] <- freq_result
                    
                    # Convert to HTML
                    html_output <- summarytools::view(freq_result, 
                                                    method = "render",
                                                    headings = options$headings,
                                                    bootstrap.css = options$bootstrap_css)
                    html_parts <- c(html_parts, as.character(html_output))
                }
            }
            
            # Combine HTML outputs
            combined_html <- paste(html_parts, collapse = "<br><br>")
            
            # Set HTML output
            self$results$summary_output$setContent(combined_html)
            
            # Create table data for first variable
            if (length(freq_list) > 0) {
                first_var <- names(freq_list)[1]
                table_data <- private$.extractFrequencyTableData(freq_list[[first_var]], first_var)
                private$.populateFrequencyTable(table_data)
            }
            
            return(list(html = combined_html, table_data = if (length(freq_list) > 0) table_data else NULL))
        },
        
        .runDescriptive = function(data, options) {
            
            # Select numeric variables
            if (length(options$vars) > 0) {
                numeric_vars <- options$vars[sapply(data[options$vars], is.numeric)]
            } else {
                numeric_vars <- names(data)[sapply(data, is.numeric)]
            }
            
            if (length(numeric_vars) == 0) {
                jmvcore::reject("No numeric variables found for descriptive analysis")
                return(NULL)
            }
            
            data_subset <- data[numeric_vars]
            
            # Configure summarytools options
            old_options <- summarytools::st_options()
            on.exit(do.call(summarytools::st_options, old_options))
            
            summarytools::st_options(
                plain.ascii = options$plain_ascii,
                style = options$output_style,
                descr.stats = private$.getSelectedStats(options),
                descr.transpose = options$transpose_output,
                headings = options$headings,
                bootstrap.css = options$bootstrap_css
            )
            
            # Generate descriptive statistics
            if (!is.null(options$group_var) && options$group_var %in% names(data)) {
                descr_result <- summarytools::stby(data_subset, 
                                                 data[[options$group_var]], 
                                                 summarytools::descr)
            } else {
                descr_result <- summarytools::descr(data_subset)
            }
            
            # Convert to HTML
            html_output <- summarytools::view(descr_result, 
                                            method = "render",
                                            headings = options$headings,
                                            bootstrap.css = options$bootstrap_css)
            
            # Set HTML output
            self$results$summary_output$setContent(as.character(html_output))
            
            # Create table data
            table_data <- private$.extractDescriptiveTableData(descr_result, numeric_vars)
            private$.populateDescriptiveStats(table_data)
            
            return(list(html = as.character(html_output), table_data = table_data))
        },
        
        .runCrossTable = function(data, options) {
            
            # Check required variables
            if (is.null(options$cross_var1) || is.null(options$cross_var2)) {
                jmvcore::reject("Both cross-tabulation variables must be specified")
                return(NULL)
            }
            
            if (!options$cross_var1 %in% names(data) || !options$cross_var2 %in% names(data)) {
                jmvcore::reject("Cross-tabulation variables not found in data")
                return(NULL)
            }
            
            # Configure summarytools options
            old_options <- summarytools::st_options()
            on.exit(do.call(summarytools::st_options, old_options))
            
            summarytools::st_options(
                plain.ascii = options$plain_ascii,
                style = options$output_style,
                ctable.prop = options$cross_proportions,
                headings = options$headings,
                bootstrap.css = options$bootstrap_css
            )
            
            # Generate cross-tabulation
            if (!is.null(options$weights_var) && options$weights_var %in% names(data)) {
                ctable_result <- summarytools::ctable(data[[options$cross_var1]], 
                                                     data[[options$cross_var2]],
                                                     weights = data[[options$weights_var]])
            } else {
                ctable_result <- summarytools::ctable(data[[options$cross_var1]], 
                                                     data[[options$cross_var2]])
            }
            
            # Convert to HTML
            html_output <- summarytools::view(ctable_result, 
                                            method = "render",
                                            headings = options$headings,
                                            bootstrap.css = options$bootstrap_css)
            
            # Set both HTML outputs
            self$results$summary_output$setContent(as.character(html_output))
            self$results$crosstab_output$setContent(as.character(html_output))
            
            return(list(html = as.character(html_output)))
        },
        
        .getSelectedStats = function(options) {
            
            if (options$stats_to_include == "all") {
                return("all")
            } else if (options$stats_to_include == "basic") {
                return(c("mean", "sd", "min", "max", "n.valid"))
            } else if (options$stats_to_include == "central") {
                return(c("mean", "median", "mode"))
            } else if (options$stats_to_include == "dispersion") {
                return(c("sd", "var", "range", "iqr"))
            } else if (options$stats_to_include == "custom") {
                stats <- character(0)
                if (options$include_mean) stats <- c(stats, "mean")
                if (options$include_median) stats <- c(stats, "median")
                if (options$include_mode) stats <- c(stats, "mode")
                if (options$include_sd) stats <- c(stats, "sd")
                if (options$include_var) stats <- c(stats, "var")
                if (options$include_range) stats <- c(stats, "min", "max", "range")
                if (options$include_quartiles) stats <- c(stats, "q1", "q3", "iqr")
                if (options$include_skewness) stats <- c(stats, "skewness")
                if (options$include_kurtosis) stats <- c(stats, "kurtosis")
                
                return(if (length(stats) > 0) stats else "all")
            }
            
            return("all")
        },
        
        .extractDfSummaryTableData = function(df_summary, data) {
            
            # This is a simplified extraction - in practice, you'd parse the dfSummary object
            # For now, create basic summary data
            
            table_data <- list()
            
            for (i in 1:ncol(data)) {
                var_name <- names(data)[i]
                var_data <- data[[i]]
                
                # Determine variable type
                var_type <- if (is.numeric(var_data)) {
                    "Numeric"
                } else if (is.factor(var_data)) {
                    "Factor"
                } else if (is.character(var_data)) {
                    "Character"
                } else if (is.logical(var_data)) {
                    "Logical"
                } else {
                    class(var_data)[1]
                }
                
                # Basic statistics/values
                if (is.numeric(var_data)) {
                    stats <- paste0("Mean: ", round(mean(var_data, na.rm = TRUE), 2),
                                   " | SD: ", round(sd(var_data, na.rm = TRUE), 2))
                    freqs <- paste0("Min: ", round(min(var_data, na.rm = TRUE), 2),
                                   " | Max: ", round(max(var_data, na.rm = TRUE), 2))
                } else {
                    top_values <- head(sort(table(var_data, useNA = "no"), decreasing = TRUE), 3)
                    stats <- paste(names(top_values), collapse = ", ")
                    freqs <- paste(as.numeric(top_values), collapse = ", ")
                }
                
                # Valid and missing counts
                valid_count <- sum(!is.na(var_data))
                missing_count <- sum(is.na(var_data))
                
                table_data[[i]] <- list(
                    variable = var_name,
                    type = var_type,
                    label = var_name,  # Could be enhanced with actual labels
                    stats = stats,
                    freqs = freqs,
                    valid = paste0(valid_count, " (", round(100 * valid_count / length(var_data), 1), "%)"),
                    missing = paste0(missing_count, " (", round(100 * missing_count / length(var_data), 1), "%)")
                )
            }
            
            return(table_data)
        },
        
        .extractFrequencyTableData = function(freq_result, var_name) {
            
            # Extract frequency data from summarytools freq object
            # This is a simplified version - would need to be enhanced based on actual freq object structure
            
            if (is.null(freq_result)) return(NULL)
            
            # For now, create dummy data structure
            # In practice, you'd extract from the actual freq_result object
            table_data <- list()
            
            # This would need to be implemented based on the actual structure of summarytools freq objects
            # For now, return empty list
            return(list())
        },
        
        .extractDescriptiveTableData = function(descr_result, vars) {
            
            # Extract descriptive statistics from summarytools descr object
            # This is a simplified version
            
            if (is.null(descr_result)) return(NULL)
            
            table_data <- list()
            
            # This would need to be implemented based on actual descr object structure
            # For now, return empty list
            return(list())
        },
        
        .populateDataSummaryTable = function(table_data) {
            if (is.null(table_data)) return()
            
            for (row_data in table_data) {
                self$results$data_summary_table$addRow(rowKey = row_data$variable, values = row_data)
            }
        },
        
        .populateFrequencyTable = function(table_data) {
            if (is.null(table_data) || length(table_data) == 0) return()
            
            # Implementation would depend on the actual structure of frequency data
        },
        
        .populateDescriptiveStats = function(table_data) {
            if (is.null(table_data) || length(table_data) == 0) return()
            
            # Implementation would depend on the actual structure of descriptive data
        },
        
        .generateInstructions = function() {
            
            analysis_type <- self$options$analysis_type
            
            instructions <- switch(analysis_type,
                "dfsummary" = paste(
                    "<h3>Data Frame Summary (dfSummary)</h3>",
                    "<p>This analysis provides a comprehensive overview of your dataset including:</p>",
                    "<ul>",
                    "<li>Variable types and basic statistics</li>",
                    "<li>Frequency distributions and histograms</li>",
                    "<li>Missing data patterns</li>",
                    "<li>Unique value counts</li>",
                    "</ul>",
                    "<p>Use this for initial data exploration and quality assessment.</p>"
                ),
                
                "freq" = paste(
                    "<h3>Frequency Tables</h3>",
                    "<p>This analysis shows frequency distributions for categorical variables including:</p>",
                    "<ul>",
                    "<li>Count and percentage for each category</li>",
                    "<li>Valid and total percentages</li>",
                    "<li>Cumulative frequencies (if enabled)</li>",
                    "<li>Missing value information</li>",
                    "</ul>",
                    "<p>Use this to understand the distribution of categorical variables.</p>"
                ),
                
                "descr" = paste(
                    "<h3>Descriptive Statistics</h3>",
                    "<p>This analysis provides detailed descriptive statistics for numeric variables:</p>",
                    "<ul>",
                    "<li>Central tendency measures (mean, median)</li>",
                    "<li>Dispersion measures (SD, variance, range)</li>",
                    "<li>Distribution shape (skewness, kurtosis)</li>",
                    "<li>Quantiles and percentiles</li>",
                    "</ul>",
                    "<p>Use this for comprehensive numeric variable analysis.</p>"
                ),
                
                "ctable" = paste(
                    "<h3>Cross-tabulation</h3>",
                    "<p>This analysis creates cross-tabulation tables between two categorical variables:</p>",
                    "<ul>",
                    "<li>Joint frequency distributions</li>",
                    "<li>Row, column, or total proportions</li>",
                    "<li>Chi-square test of association</li>",
                    "<li>Statistical significance tests</li>",
                    "</ul>",
                    "<p>Use this to examine relationships between categorical variables.</p>"
                ),
                
                "<h3>Summarytools Analysis</h3><p>Select an analysis type to begin.</p>"
            )
            
            return(instructions)
        },
        
        .generateInterpretation = function(analysis_type) {
            
            interpretation <- switch(analysis_type,
                "dfsummary" = paste(
                    "<h3>Interpreting Data Frame Summary</h3>",
                    "<h4>Variable Types:</h4>",
                    "<ul>",
                    "<li><strong>Numeric:</strong> Continuous variables with mean, SD, and histograms</li>",
                    "<li><strong>Factor:</strong> Categorical variables with level counts</li>",
                    "<li><strong>Character:</strong> Text variables with unique value counts</li>",
                    "<li><strong>Logical:</strong> Boolean variables with TRUE/FALSE counts</li>",
                    "</ul>",
                    "<h4>Key Indicators:</h4>",
                    "<ul>",
                    "<li><strong>Missing Data:</strong> Check for patterns in missing values</li>",
                    "<li><strong>Distribution Shape:</strong> Look for skewness in histograms</li>",
                    "<li><strong>Outliers:</strong> Extreme values visible in min/max</li>",
                    "<li><strong>Data Quality:</strong> Unexpected values or data types</li>",
                    "</ul>"
                ),
                
                "freq" = paste(
                    "<h3>Interpreting Frequency Tables</h3>",
                    "<h4>Key Metrics:</h4>",
                    "<ul>",
                    "<li><strong>Frequency:</strong> Raw count of observations in each category</li>",
                    "<li><strong>% Valid:</strong> Percentage excluding missing values</li>",
                    "<li><strong>% Total:</strong> Percentage including missing values</li>",
                    "<li><strong>% Cumulative:</strong> Running percentage total</li>",
                    "</ul>",
                    "<h4>What to Look For:</h4>",
                    "<ul>",
                    "<li><strong>Balanced Categories:</strong> Roughly equal frequencies</li>",
                    "<li><strong>Dominant Categories:</strong> One category much more frequent</li>",
                    "<li><strong>Rare Categories:</strong> Categories with very few observations</li>",
                    "<li><strong>Missing Data:</strong> Proportion of missing values</li>",
                    "</ul>"
                ),
                
                "descr" = paste(
                    "<h3>Interpreting Descriptive Statistics</h3>",
                    "<h4>Central Tendency:</h4>",
                    "<ul>",
                    "<li><strong>Mean:</strong> Average value, sensitive to outliers</li>",
                    "<li><strong>Median:</strong> Middle value, robust to outliers</li>",
                    "<li><strong>Mode:</strong> Most frequent value</li>",
                    "</ul>",
                    "<h4>Dispersion:</h4>",
                    "<ul>",
                    "<li><strong>Standard Deviation:</strong> Average distance from mean</li>",
                    "<li><strong>IQR:</strong> Range of middle 50% of data</li>",
                    "<li><strong>Range:</strong> Difference between min and max</li>",
                    "</ul>",
                    "<h4>Distribution Shape:</h4>",
                    "<ul>",
                    "<li><strong>Skewness > 0:</strong> Right-skewed (tail extends right)</li>",
                    "<li><strong>Skewness < 0:</strong> Left-skewed (tail extends left)</li>",
                    "<li><strong>Kurtosis > 3:</strong> More peaked than normal distribution</li>",
                    "<li><strong>Kurtosis < 3:</strong> Flatter than normal distribution</li>",
                    "</ul>"
                ),
                
                "ctable" = paste(
                    "<h3>Interpreting Cross-tabulation</h3>",
                    "<h4>Frequency Interpretation:</h4>",
                    "<ul>",
                    "<li><strong>Cell Frequencies:</strong> Number of observations in each combination</li>",
                    "<li><strong>Row Proportions:</strong> Distribution within each row category</li>",
                    "<li><strong>Column Proportions:</strong> Distribution within each column category</li>",
                    "<li><strong>Total Proportions:</strong> Each cell as proportion of total</li>",
                    "</ul>",
                    "<h4>Statistical Tests:</h4>",
                    "<ul>",
                    "<li><strong>Chi-square Test:</strong> Tests independence of variables</li>",
                    "<li><strong>p < 0.05:</strong> Significant association between variables</li>",
                    "<li><strong>Cramér's V:</strong> Strength of association (0 = no association, 1 = perfect association)</li>",
                    "</ul>"
                ),
                
                "<h3>Analysis Interpretation</h3><p>Select an analysis type to see interpretation guidance.</p>"
            )
            
            return(interpretation)
        }
    )
)
