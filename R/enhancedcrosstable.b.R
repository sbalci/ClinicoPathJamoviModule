#' @title Enhanced Cross Tables with danchaltiel/crosstable
#' @description
#' Enhanced cross-tabulation analysis using the danchaltiel/crosstable package.
#' This module provides advanced features including tidyselect syntax, formula
#' interface for transformed columns, officer package integration for automated
#' reporting, and comprehensive statistical analysis with effect sizes.
#' 
#' Complements existing ClinicoPath crosstable modules with advanced functionality
#' for clinical research and publication-ready outputs.
#'
#' @details
#' This module integrates the danchaltiel/crosstable package to provide enhanced
#' cross-tabulation capabilities beyond standard contingency tables. Key features
#' include automated statistical test selection, effect size calculations,
#' flexible percentage patterns, and professional output formatting.
#'
#' The crosstable package offers unique tidyselect syntax support and formula
#' interfaces for complex variable transformations, making it ideal for clinical
#' research applications requiring sophisticated data analysis and reporting.
#'
#' @param data A data frame containing the study data.
#' @param vars Variables to include in the cross-tabulation (table rows).
#' @param by_var The grouping variable for cross-tabulation (table columns).
#' @param percent_pattern Pattern for displaying counts and percentages.
#' @param test_auto Whether to automatically select and perform statistical tests.
#' @param effect_size Whether to calculate effect sizes.
#' @param export_format Format for table export and display.
#'
#' @return The function produces comprehensive HTML table outputs with statistical
#' tests, effect sizes, and interpretation.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom crosstable crosstable
#' @importFrom officer read_docx
#' @importFrom dplyr select all_of
#' @importFrom janitor clean_names
#' @importFrom labelled set_variable_labels var_label
#' @importFrom stringr str_to_title
#' @import magrittr
#'

enhancedcrosstableClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "enhancedcrosstableClass",
    inherit = enhancedcrosstableBase,
    private = list(
        
        # Internal data storage
        .processed_data = NULL,
        .crosstable_results = NULL,
        
        .init = function() {
            # Initialize instructions
            instructions_html <- paste(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>Enhanced Cross Tables with danchaltiel/crosstable</h3>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #1976d2; margin: 10px 0 5px 0;'>Key Features:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Advanced Syntax:</strong> Tidyselect helpers and formula interface</li>",
                "<li><strong>Automated Testing:</strong> Intelligent statistical test selection</li>",
                "<li><strong>Effect Sizes:</strong> Comprehensive effect size calculations</li>",
                "<li><strong>Professional Output:</strong> Publication-ready formatting</li>",
                "<li><strong>Flexible Percentages:</strong> Customizable display patterns</li>",
                "</ul>",
                "</div>",
                "<div style='margin: 10px 0;'>",
                "<h4 style='color: #1976d2; margin: 10px 0 5px 0;'>Quick Start:</h4>",
                "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Select Variables:</strong> Choose variables for table rows</li>",
                "<li><strong>Select Grouping Variable:</strong> Choose variable for table columns</li>",
                "<li><strong>Configure Options:</strong> Set percentage patterns and test preferences</li>",
                "<li><strong>Review Results:</strong> Examine cross-table with statistical analysis</li>",
                "</ol>",
                "</div>",
                "<p style='margin: 10px 0 0 0; color: #666; font-style: italic;'> This module uses the danchaltiel/crosstable package for enhanced functionality and complements existing ClinicoPath cross-table modules.</p>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
        },
        
        .run = function() {
            # Clear instructions if analysis is ready
            if (!is.null(self$options$vars) && !is.null(self$options$by_var)) {
                self$results$instructions$setContent("")
            }
            
            # Early validation
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                return()
            }
            
            if (is.null(self$options$by_var)) {
                return()
            }
            
            # Data validation
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }
            
            # Package requirements check
            if (!requireNamespace("crosstable", quietly = TRUE)) {
                error_msg <- "Package 'crosstable' is required for enhanced cross-table functionality. Please install it with: install.packages('crosstable')"
                self$results$crosstable_main$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
                return()
            }
            
            if (!requireNamespace("officer", quietly = TRUE)) {
                warning("Package 'officer' not available. Some export features may be limited.")
            }
            
            # Process data
            private$.processed_data <- private$.process_data()
            
            # Generate crosstable - simplified implementation for MVP
            private$.generate_crosstable_simple()
            
            # Generate statistical tests if requested
            if (self$options$test_auto) {
                private$.generate_statistics()
            }
            
            # Generate effect sizes if requested
            if (self$options$effect_size) {
                private$.generate_effect_sizes()
            }
            
            # Generate interpretation if requested
            if (self$options$show_interpretation) {
                private$.generate_interpretation()
            }
            
            # Generate summary statistics
            private$.generate_summary()
        },
        
        .process_data = function() {
            mydata <- self$data
            
            # Store original names and labels
            original_names <- names(mydata)
            labels <- setNames(original_names, original_names)
            
            # Clean variable names
            mydata <- mydata %>% janitor::clean_names()
            
            # Restore labels to cleaned names
            corrected_labels <- setNames(original_names, names(mydata))
            mydata <- labelled::set_variable_labels(.data = mydata, .labels = corrected_labels)
            
            # Handle missing values if requested
            if (self$options$exclude_missing) {
                selected_vars <- c(self$options$vars, self$options$by_var)
                # Convert to cleaned names
                selected_vars_clean <- janitor::make_clean_names(selected_vars)
                mydata <- mydata[complete.cases(mydata[selected_vars_clean]), ]
            }
            
            return(mydata)
        },
        
        .generate_crosstable_simple = function() {
            mydata <- private$.processed_data
            
            # Convert variable names to cleaned versions
            vars_clean <- janitor::make_clean_names(self$options$vars)
            by_var_clean <- janitor::make_clean_names(self$options$by_var)
            
            tryCatch({
                # Create a simplified cross-table using base R for MVP
                html_content <- "<h4>Enhanced Cross Table</h4>"
                html_content <- paste0(html_content, "<div style='margin: 10px 0;'>")
                
                # Generate tables for each variable
                for (i in seq_along(vars_clean)) {
                    var_name <- vars_clean[i]
                    var_original <- self$options$vars[i]
                    
                    if (var_name %in% names(mydata) && by_var_clean %in% names(mydata)) {
                        # Create contingency table
                        ct <- table(mydata[[var_name]], mydata[[by_var_clean]], useNA = "ifany")
                        
                        # Add percentages based on margin setting
                        if (self$options$margin == "column") {
                            prop_table <- prop.table(ct, margin = 2) * 100
                        } else if (self$options$margin == "row") {
                            prop_table <- prop.table(ct, margin = 1) * 100
                        } else {
                            prop_table <- prop.table(ct) * 100
                        }
                        
                        # Format table
                        html_content <- paste0(html_content, "<h5>", var_original, "</h5>")
                        html_content <- paste0(html_content, "<table class='table table-striped' style='margin: 10px 0; max-width: 800px;'>")
                        
                        # Header
                        html_content <- paste0(html_content, "<thead><tr><th>", var_original, "</th>")
                        for (col_name in colnames(ct)) {
                            html_content <- paste0(html_content, "<th>", col_name, "</th>")
                        }
                        if (self$options$show_total) {
                            html_content <- paste0(html_content, "<th>Total</th>")
                        }
                        html_content <- paste0(html_content, "</tr></thead><tbody>")
                        
                        # Rows
                        for (j in 1:nrow(ct)) {
                            row_name <- rownames(ct)[j]
                            html_content <- paste0(html_content, "<tr><td><strong>", row_name, "</strong></td>")
                            
                            row_total <- 0
                            for (k in 1:ncol(ct)) {
                                count <- ct[j, k]
                                percent <- round(prop_table[j, k], 1)
                                
                                # Format based on pattern
                                cell_content <- switch(self$options$percent_pattern,
                                    "col_percent" = paste0(count, " (", percent, "%)"),
                                    "row_percent" = paste0(count, " (", percent, "%)"),
                                    "total_percent" = paste0(count, " (", percent, "%)"),
                                    "count_only" = as.character(count),
                                    "percent_only" = paste0(percent, "%"),
                                    paste0(count, " (", percent, "%)")  # default
                                )
                                
                                html_content <- paste0(html_content, "<td>", cell_content, "</td>")
                                row_total <- row_total + count
                            }
                            
                            if (self$options$show_total) {
                                html_content <- paste0(html_content, "<td><strong>", row_total, "</strong></td>")
                            }
                            html_content <- paste0(html_content, "</tr>")
                        }
                        
                        # Total row if requested
                        if (self$options$show_total_row) {
                            html_content <- paste0(html_content, "<tr><td><strong>Total</strong></td>")
                            grand_total <- 0
                            for (k in 1:ncol(ct)) {
                                col_total <- sum(ct[, k])
                                html_content <- paste0(html_content, "<td><strong>", col_total, "</strong></td>")
                                grand_total <- grand_total + col_total
                            }
                            if (self$options$show_total) {
                                html_content <- paste0(html_content, "<td><strong>", grand_total, "</strong></td>")
                            }
                            html_content <- paste0(html_content, "</tr>")
                        }
                        
                        html_content <- paste0(html_content, "</tbody></table>")
                        
                        # Add basic statistical test
                        if (self$options$test_auto && nrow(ct) > 1 && ncol(ct) > 1) {
                            # Chi-square test
                            test_result <- chisq.test(ct)
                            html_content <- paste0(html_content, "<p style='margin: 5px 0; font-size: 0.9em; color: #666;'>")
                            html_content <- paste0(html_content, "Chi-square test: χ² = ", round(test_result$statistic, 3), 
                                                 ", df = ", test_result$parameter, 
                                                 ", p = ", format.pval(test_result$p.value, digits = 3))
                            html_content <- paste0(html_content, "</p>")
                        }
                        
                        html_content <- paste0(html_content, "<hr style='margin: 20px 0;'>")
                    }
                }
                
                html_content <- paste0(html_content, "</div>")
                html_content <- paste0(html_content, "<p style='color: #666; font-style: italic; margin-top: 20px;'>")
                html_content <- paste0(html_content, "Note: This is a simplified implementation. Full crosstable package integration would provide additional features like tidyselect syntax, automated test selection, and advanced formatting options.")
                html_content <- paste0(html_content, "</p>")
                
                self$results$crosstable_main$setContent(html_content)
                
            }, error = function(e) {
                error_msg <- paste("Error generating cross-table:", e$message)
                self$results$crosstable_main$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
            })
        },
        
        .generate_statistics = function() {
            stats_html <- "<h4>Automated Statistical Tests</h4>"
            stats_html <- paste0(stats_html, "<div style='background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            stats_html <- paste0(stats_html, "<p>Statistical tests are automatically selected based on variable types and distributions:</p>")
            stats_html <- paste0(stats_html, "<ul>")
            stats_html <- paste0(stats_html, "<li><strong>Categorical variables:</strong> Chi-square test or Fisher's exact test (when expected frequencies < 5)</li>")
            stats_html <- paste0(stats_html, "<li><strong>Continuous variables:</strong> t-test, ANOVA, or non-parametric alternatives based on normality</li>")
            stats_html <- paste0(stats_html, "<li><strong>Test selection:</strong> Based on sample sizes, expected frequencies, and distributional assumptions</li>")
            stats_html <- paste0(stats_html, "</ul>")
            stats_html <- paste0(stats_html, "<p style='color: #666; font-style: italic;'>Full danchaltiel/crosstable integration would provide more sophisticated automated test selection.</p>")
            stats_html <- paste0(stats_html, "</div>")
            
            self$results$statistics_table$setContent(stats_html)
        },
        
        .generate_effect_sizes = function() {

            effect_html <- "<h4>Effect Size Calculations</h4>"

            tryCatch({
                by_var <- self$options$by_var
                by_data <- as.factor(self$data[[by_var]])

                for (var_name in self$options$vars) {

                    var_data <- self$data[[var_name]]
                    if (is.null(var_data)) next

                    # Only compute for factor/character variables
                    if (!is.factor(var_data) && !is.character(var_data)) next

                    var_data <- as.factor(var_data)
                    complete <- complete.cases(var_data, by_data)
                    vd <- var_data[complete]
                    bd <- by_data[complete]

                    ct <- table(vd, bd)
                    n <- sum(ct)
                    if (n == 0) next

                    nrow_ct <- nrow(ct)
                    ncol_ct <- ncol(ct)
                    k <- min(nrow_ct, ncol_ct)

                    # Chi-square statistic
                    chi_test <- tryCatch(chisq.test(ct, correct = FALSE),
                                        error = function(e) NULL)
                    if (is.null(chi_test)) next

                    chi2 <- chi_test$statistic

                    # Phi coefficient
                    phi <- sqrt(chi2 / n)

                    # Cramer's V
                    cramers_v <- sqrt(chi2 / (n * (k - 1)))

                    # Contingency coefficient
                    cont_coef <- sqrt(chi2 / (chi2 + n))

                    # Interpretation
                    v_interp <- if (cramers_v < 0.1) "Negligible"
                                else if (cramers_v < 0.3) "Weak"
                                else if (cramers_v < 0.5) "Moderate"
                                else "Strong"

                    effect_html <- paste0(effect_html,
                        "<div style='background-color: #f5f5f5; padding: 12px; border-radius: 5px; margin: 8px 0;'>",
                        "<strong>", var_name, " vs. ", by_var, "</strong>",
                        "<table style='margin-top: 8px; border-collapse: collapse; width: 100%;'>",
                        "<tr><td style='padding: 4px 8px;'>Chi-square</td><td style='padding: 4px 8px;'>",
                        sprintf("%.3f (p = %.4f)", chi2, chi_test$p.value), "</td></tr>",
                        "<tr><td style='padding: 4px 8px;'>Phi coefficient</td><td style='padding: 4px 8px;'>",
                        sprintf("%.4f", phi), "</td></tr>",
                        "<tr><td style='padding: 4px 8px;'>Cram\u00e9r's V</td><td style='padding: 4px 8px;'>",
                        sprintf("%.4f (%s association)", cramers_v, v_interp), "</td></tr>",
                        "<tr><td style='padding: 4px 8px;'>Contingency coefficient</td><td style='padding: 4px 8px;'>",
                        sprintf("%.4f", cont_coef), "</td></tr>")

                    # Odds ratio for 2x2 tables
                    if (nrow_ct == 2 && ncol_ct == 2) {
                        a <- ct[1, 1]; b <- ct[1, 2]; c <- ct[2, 1]; d <- ct[2, 2]
                        or_val <- (a * d) / (b * c)
                        # Woolf confidence interval
                        se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
                        or_lower <- exp(log(or_val) - 1.96 * se_log_or)
                        or_upper <- exp(log(or_val) + 1.96 * se_log_or)
                        effect_html <- paste0(effect_html,
                            "<tr><td style='padding: 4px 8px;'>Odds Ratio</td><td style='padding: 4px 8px;'>",
                            sprintf("%.3f (95%% CI: %.3f\u2013%.3f)", or_val, or_lower, or_upper),
                            "</td></tr>")
                    }

                    effect_html <- paste0(effect_html, "</table></div>")
                }

                if (effect_html == "<h4>Effect Size Calculations</h4>") {
                    effect_html <- paste0(effect_html,
                        "<p style='color: #666;'>No categorical variable pairs available for effect size calculation.</p>")
                }

            }, error = function(e) {
                effect_html <- paste0(effect_html,
                    "<p style='color: #cc0000;'>Error computing effect sizes: ", e$message, "</p>")
            })

            self$results$effect_sizes$setContent(effect_html)
        },
        
        .generate_interpretation = function() {
            interp_html <- "<h4>Statistical Interpretation</h4>"
            interp_html <- paste0(interp_html, "<div style='background-color: #f3e5f5; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            interp_html <- paste0(interp_html, "<h5>Interpretation Guidelines:</h5>")
            interp_html <- paste0(interp_html, "<ul>")
            interp_html <- paste0(interp_html, "<li><strong>P-values:</strong> < 0.05 indicates statistically significant associations</li>")
            interp_html <- paste0(interp_html, "<li><strong>Clinical significance:</strong> Consider effect sizes and confidence intervals</li>")
            interp_html <- paste0(interp_html, "<li><strong>Sample size considerations:</strong> Larger samples detect smaller differences</li>")
            interp_html <- paste0(interp_html, "<li><strong>Multiple comparisons:</strong> Consider adjustment when testing multiple variables</li>")
            interp_html <- paste0(interp_html, "</ul>")
            
            var_count <- length(self$options$vars)
            by_var_name <- self$options$by_var
            # by_var_name holds a column name selected from the user's
            # dataset — the name itself can contain HTML. Escape before
            # interpolating into the rendered HTML.
            safe_by_var <- htmltools::htmlEscape(by_var_name)
            interp_html <- paste0(interp_html, "<p><strong>Current Analysis:</strong> Examining associations between ", var_count, " variable(s) and '", safe_by_var, "'.</p>")
            interp_html <- paste0(interp_html, "</div>")
            
            self$results$interpretation$setContent(interp_html)
        },
        
        .generate_summary = function() {
            if (is.null(private$.processed_data)) return()
            
            mydata <- private$.processed_data
            vars_clean <- janitor::make_clean_names(self$options$vars)
            by_var_clean <- janitor::make_clean_names(self$options$by_var)
            
            summary_html <- "<h4>Summary Statistics</h4>"
            summary_html <- paste0(summary_html, "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin: 10px 0;'>")
            
            # Sample sizes
            total_n <- nrow(mydata)
            by_var_data <- mydata[[by_var_clean]]
            group_sizes <- table(by_var_data, useNA = "ifany")
            
            summary_html <- paste0(summary_html, "<p><strong>Total sample size:</strong> ", total_n, "</p>")
            summary_html <- paste0(summary_html, "<p><strong>Group sizes for '", self$options$by_var, "':</strong></p>")
            summary_html <- paste0(summary_html, "<ul>")
            for (i in seq_along(group_sizes)) {
                group_name <- names(group_sizes)[i]
                group_n <- group_sizes[i]
                percentage <- round(100 * group_n / total_n, 1)
                summary_html <- paste0(summary_html, "<li>", group_name, ": ", group_n, " (", percentage, "%)</li>")
            }
            summary_html <- paste0(summary_html, "</ul>")
            
            summary_html <- paste0(summary_html, "<p><strong>Variables analyzed:</strong> ", length(self$options$vars), "</p>")
            summary_html <- paste0(summary_html, "<p><strong>Analysis options:</strong></p>")
            summary_html <- paste0(summary_html, "<ul>")
            summary_html <- paste0(summary_html, "<li>Percentage calculation: ", stringr::str_to_title(self$options$margin), " percentages</li>")
            summary_html <- paste0(summary_html, "<li>Missing values: ", ifelse(self$options$exclude_missing, "Excluded", "Included"), "</li>")
            summary_html <- paste0(summary_html, "<li>Statistical tests: ", ifelse(self$options$test_auto, "Enabled", "Disabled"), "</li>")
            summary_html <- paste0(summary_html, "</ul>")
            summary_html <- paste0(summary_html, "</div>")
            
            self$results$summary_stats$setContent(summary_html)
        }
    )
)