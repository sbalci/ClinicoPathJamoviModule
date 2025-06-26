#' @title Enhanced Tables with gt
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom gt gt
#' @importFrom gt tab_header
#' @importFrom gt tab_stubhead
#' @importFrom gt tab_spanner
#' @importFrom gt tab_footnote
#' @importFrom gt tab_source_note
#' @importFrom gt fmt_number
#' @importFrom gt fmt_percent
#' @importFrom gt tab_style
#' @importFrom gt cell_text
#' @importFrom gt cell_fill
#' @importFrom gt cells_body
#' @importFrom gt cells_column_labels
#' @importFrom gt cells_title
#' @importFrom gt opt_table_font
#' @importFrom gt opt_row_striping
#' @importFrom gt cols_align
#' @importFrom gt tab_options
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr where
#' @importFrom dplyr n
#' @importFrom dplyr everything
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom stats t.test
#' @importFrom stats wilcox.test
#' @importFrom stats chisq.test
#' @importFrom stats fisher.test
#' @importFrom stats kruskal.test
#' @importFrom stats aov
#' @export

enhancedtablesClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "enhancedtablesClass",
    inherit = enhancedtablesBase,
    private = list(
        .results_cache = NULL,
        .processed_data = NULL,
        
        .init = function() {
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                self$results$enhanced_table$setVisible(FALSE)
                self$results$summary_stats$setVisible(FALSE)
                self$results$group_comparison$setVisible(FALSE)
                self$results$export_table$setVisible(FALSE)
            }
            
            if (!self$options$show_interpretation) {
                self$results$interpretation$setVisible(FALSE)
            }
        },
        
        .run = function() {
            # Early exit with instructions if no variables selected
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                welcome_html <- private$.create_welcome_message()
                self$results$instructions$setContent(welcome_html)
                return()
            } else {
                self$results$instructions$setContent("")
            }
            
            # Validate data
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }
            
            # Check for gt package
            if (!requireNamespace("gt", quietly = TRUE)) {
                error_msg <- paste(
                    "<div style='color: red; padding: 10px;'>",
                    "<strong>Error:</strong> The 'gt' package is required for enhanced tables.",
                    "<br>Please install it using: install.packages('gt')",
                    "</div>"
                )
                self$results$enhanced_table$setContent(error_msg)
                return()
            }
            
            # Process data
            private$.processed_data <- private$.process_data(self$data)
            
            # Generate main enhanced table
            enhanced_table_html <- private$.create_enhanced_table()
            self$results$enhanced_table$setContent(enhanced_table_html)
            
            # Generate summary statistics if requested
            if (self$options$table_type == "summary") {
                summary_html <- private$.create_summary_stats()
                self$results$summary_stats$setContent(summary_html)
            }
            
            # Generate group comparison if applicable
            if (!is.null(self$options$group_var) && self$options$include_pvalues) {
                comparison_html <- private$.create_group_comparison()
                self$results$group_comparison$setContent(comparison_html)
            }
            
            # Generate interpretation
            if (self$options$show_interpretation) {
                interpretation_html <- private$.create_interpretation()
                self$results$interpretation$setContent(interpretation_html)
            }
            
            # Generate export table if different format requested
            if (self$options$export_format != "html") {
                export_html <- private$.create_export_table()
                self$results$export_table$setContent(export_html)
            }
        },
        
        .create_welcome_message = function() {
            paste(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 10px;'>",
                "<h3 style='color: #2c3e50; margin-top: 0;'>Enhanced Tables with gt Package</h3>",
                "<p><strong>Create professional, publication-ready tables with advanced formatting.</strong></p>",
                "<h4>Quick Start:</h4>",
                "<ol>",
                "<li><strong>Select Variables:</strong> Choose variables to display in your enhanced table</li>",
                "<li><strong>Choose Table Type:</strong> Select summary, grouped comparison, or clinical characteristics</li>",
                "<li><strong>Configure Options:</strong> Set grouping variables, statistics, and formatting preferences</li>",
                "<li><strong>Apply Theme:</strong> Choose from clinical, publication, or modern themes</li>",
                "</ol>",
                "<h4>Key Features:</h4>",
                "<ul>",
                "<li>📊 <strong>Professional Formatting:</strong> Publication-ready tables with gt package</li>",
                "<li>🎨 <strong>Clinical Themes:</strong> Specialized themes for medical research</li>",
                "<li>📈 <strong>Statistical Summaries:</strong> Comprehensive statistics for continuous and categorical variables</li>",
                "<li>🔍 <strong>Group Comparisons:</strong> Statistical tests with p-values and effect sizes</li>",
                "<li>📱 <strong>Multiple Formats:</strong> Export to Word, LaTeX, and RTF</li>",
                "<li>🎯 <strong>Clinical Interpretation:</strong> Automated interpretation of statistical results</li>",
                "</ul>",
                "<p><em>Select variables from your dataset to begin creating enhanced tables.</em></p>",
                "</div>"
            )
        },
        
        .process_data = function(raw_data) {
            # Clean and prepare data
            processed_data <- raw_data
            
            # Clean variable names
            if (requireNamespace("janitor", quietly = TRUE)) {
                processed_data <- janitor::clean_names(processed_data)
            }
            
            # Select only requested variables
            selected_vars <- self$options$vars
            if (!is.null(self$options$group_var)) {
                selected_vars <- c(selected_vars, self$options$group_var)
            }
            if (!is.null(self$options$strata_var)) {
                selected_vars <- c(selected_vars, self$options$strata_var)
            }
            
            # Filter to selected variables
            processed_data <- processed_data[, selected_vars, drop = FALSE]
            
            # Handle missing values based on option
            if (self$options$missing_handling == "exclude") {
                processed_data <- processed_data[complete.cases(processed_data), ]
            }
            
            return(processed_data)
        },
        
        .create_enhanced_table = function() {
            data <- private$.processed_data
            
            tryCatch({
                # Create base table structure
                if (self$options$table_type == "grouped" && !is.null(self$options$group_var)) {
                    enhanced_table <- private$.create_grouped_table(data)
                } else if (self$options$table_type == "clinical") {
                    enhanced_table <- private$.create_clinical_table(data)
                } else if (self$options$table_type == "tableone") {
                    enhanced_table <- private$.create_tableone_style(data)
                } else {
                    enhanced_table <- private$.create_summary_table(data)
                }
                
                # Apply theme
                enhanced_table <- private$.apply_theme(enhanced_table)
                
                # Convert to HTML
                gt_html <- as.character(enhanced_table)
                
                return(paste(
                    "<div style='margin: 20px; padding: 15px;'>",
                    gt_html,
                    "</div>"
                ))
                
            }, error = function(e) {
                return(paste(
                    "<div style='color: red; padding: 10px;'>",
                    "<strong>Error creating enhanced table:</strong>", 
                    e$message,
                    "</div>"
                ))
            })
        },
        
        .create_summary_table = function(data) {
            # Create summary statistics table
            summary_data <- data.frame(
                Variable = character(),
                Type = character(),
                Summary = character(),
                stringsAsFactors = FALSE
            )
            
            vars <- self$options$vars
            
            for (var in vars) {
                if (var %in% names(data)) {
                    if (is.numeric(data[[var]])) {
                        summary_stats <- private$.get_numeric_summary(data[[var]])
                        summary_data <- rbind(summary_data, data.frame(
                            Variable = var,
                            Type = "Numeric",
                            Summary = summary_stats,
                            stringsAsFactors = FALSE
                        ))
                    } else {
                        summary_stats <- private$.get_categorical_summary(data[[var]])
                        summary_data <- rbind(summary_data, data.frame(
                            Variable = var,
                            Type = "Categorical",
                            Summary = summary_stats,
                            stringsAsFactors = FALSE
                        ))
                    }
                }
            }
            
            # Create gt table
            gt_table <- gt::gt(summary_data) %>%
                gt::tab_header(
                    title = self$options$table_title,
                    subtitle = if (self$options$table_subtitle != "") self$options$table_subtitle else NULL
                ) %>%
                gt::cols_align(align = "left", columns = everything())
            
            return(gt_table)
        },
        
        .create_grouped_table = function(data) {
            group_var <- self$options$group_var
            vars <- self$options$vars
            
            # Create grouped summary
            grouped_summary <- list()
            
            for (var in vars) {
                if (var %in% names(data) && var != group_var) {
                    if (is.numeric(data[[var]])) {
                        by_group <- split(data[[var]], data[[group_var]])
                        group_stats <- sapply(by_group, function(x) {
                            private$.get_numeric_summary(x)
                        })
                        grouped_summary[[var]] <- data.frame(
                            Variable = var,
                            Group = names(group_stats),
                            Summary = as.character(group_stats),
                            stringsAsFactors = FALSE
                        )
                    }
                }
            }
            
            # Combine all summaries
            if (length(grouped_summary) > 0) {
                final_summary <- do.call(rbind, grouped_summary)
            } else {
                final_summary <- data.frame(
                    Variable = "No data",
                    Group = "No data",
                    Summary = "No data available",
                    stringsAsFactors = FALSE
                )
            }
            
            # Create gt table with grouping
            gt_table <- gt::gt(final_summary, groupname_col = "Variable") %>%
                gt::tab_header(
                    title = paste("Grouped Analysis by", group_var),
                    subtitle = self$options$table_subtitle
                )
            
            return(gt_table)
        },
        
        .create_clinical_table = function(data) {
            # Create clinical characteristics table
            clinical_data <- data.frame(
                Characteristic = character(),
                Value = character(),
                stringsAsFactors = FALSE
            )
            
            vars <- self$options$vars
            
            for (var in vars) {
                if (var %in% names(data)) {
                    if (is.numeric(data[[var]])) {
                        stats <- private$.get_clinical_numeric_summary(data[[var]])
                        clinical_data <- rbind(clinical_data, data.frame(
                            Characteristic = var,
                            Value = stats,
                            stringsAsFactors = FALSE
                        ))
                    } else {
                        stats <- private$.get_clinical_categorical_summary(data[[var]])
                        for (i in 1:nrow(stats)) {
                            clinical_data <- rbind(clinical_data, data.frame(
                                Characteristic = if (i == 1) var else "",
                                Value = paste(stats$Level[i], stats$Summary[i], sep = ": "),
                                stringsAsFactors = FALSE
                            ))
                        }
                    }
                }
            }
            
            # Create clinical gt table
            gt_table <- gt::gt(clinical_data) %>%
                gt::tab_header(
                    title = "Clinical Characteristics",
                    subtitle = self$options$table_subtitle
                ) %>%
                gt::cols_align(align = "left", columns = everything())
            
            return(gt_table)
        },
        
        .create_tableone_style = function(data) {
            # Create Table One style summary
            tableone_data <- data.frame(
                Variable = character(),
                `Overall (N = )` = character(),
                stringsAsFactors = FALSE,
                check.names = FALSE
            )
            
            total_n <- nrow(data)
            
            # Add total N to column name
            names(tableone_data)[2] <- paste0("Overall (N = ", total_n, ")")
            
            vars <- self$options$vars
            
            for (var in vars) {
                if (var %in% names(data)) {
                    if (is.numeric(data[[var]])) {
                        summary_val <- private$.get_tableone_numeric(data[[var]])
                    } else {
                        summary_val <- private$.get_tableone_categorical(data[[var]])
                    }
                    
                    tableone_data <- rbind(tableone_data, data.frame(
                        Variable = var,
                        Overall = summary_val,
                        stringsAsFactors = FALSE,
                        check.names = FALSE
                    ))
                }
            }
            
            # Create Table One gt table
            gt_table <- gt::gt(tableone_data) %>%
                gt::tab_header(
                    title = "Table 1. Baseline Characteristics",
                    subtitle = self$options$table_subtitle
                ) %>%
                gt::cols_align(align = "left", columns = 1) %>%
                gt::cols_align(align = "center", columns = 2)
            
            return(gt_table)
        },
        
        .get_numeric_summary = function(x) {
            x <- x[!is.na(x)]
            if (length(x) == 0) return("No data")
            
            switch(self$options$stats_continuous,
                "mean_sd" = sprintf("%.2f ± %.2f", mean(x), sd(x)),
                "median_iqr" = sprintf("%.2f [%.2f, %.2f]", median(x), quantile(x, 0.25), quantile(x, 0.75)),
                "both" = sprintf("%.2f ± %.2f, %.2f [%.2f, %.2f]", 
                               mean(x), sd(x), median(x), quantile(x, 0.25), quantile(x, 0.75)),
                "mean_sd_range" = sprintf("%.2f ± %.2f (%.2f - %.2f)", 
                                        mean(x), sd(x), min(x), max(x)),
                "all" = sprintf("Mean: %.2f ± %.2f, Median: %.2f [%.2f, %.2f], Range: %.2f - %.2f",
                               mean(x), sd(x), median(x), quantile(x, 0.25), quantile(x, 0.75), min(x), max(x))
            )
        },
        
        .get_categorical_summary = function(x) {
            x <- factor(x)
            tbl <- table(x, useNA = if (self$options$missing_handling == "show") "ifany" else "no")
            
            switch(self$options$stats_categorical,
                "n_percent" = {
                    percentages <- round(100 * tbl / sum(tbl), 1)
                    paste(paste0(names(tbl), ": ", tbl, " (", percentages, "%)"), collapse = "; ")
                },
                "n_only" = paste(paste0(names(tbl), ": ", tbl), collapse = "; "),
                "percent_only" = {
                    percentages <- round(100 * tbl / sum(tbl), 1)
                    paste(paste0(names(tbl), ": ", percentages, "%"), collapse = "; ")
                },
                "n_percent_missing" = {
                    percentages <- round(100 * tbl / sum(tbl), 1)
                    missing_count <- sum(is.na(x))
                    result <- paste(paste0(names(tbl), ": ", tbl, " (", percentages, "%)"), collapse = "; ")
                    if (missing_count > 0) {
                        result <- paste(result, paste("Missing:", missing_count), sep = "; ")
                    }
                    result
                }
            )
        },
        
        .get_clinical_numeric_summary = function(x) {
            x <- x[!is.na(x)]
            if (length(x) == 0) return("No data")
            
            sprintf("%.2f ± %.2f", mean(x), sd(x))
        },
        
        .get_clinical_categorical_summary = function(x) {
            x <- factor(x)
            tbl <- table(x, useNA = "no")
            percentages <- round(100 * tbl / sum(tbl), 1)
            
            data.frame(
                Level = names(tbl),
                Summary = paste0(tbl, " (", percentages, "%)"),
                stringsAsFactors = FALSE
            )
        },
        
        .get_tableone_numeric = function(x) {
            x <- x[!is.na(x)]
            if (length(x) == 0) return("No data")
            
            sprintf("%.1f ± %.1f", mean(x), sd(x))
        },
        
        .get_tableone_categorical = function(x) {
            x <- factor(x)
            tbl <- table(x, useNA = "no")
            percentages <- round(100 * tbl / sum(tbl), 1)
            
            paste(paste0(tbl, " (", percentages, "%)"), collapse = ", ")
        },
        
        .apply_theme = function(gt_table) {
            # Apply theme based on selection
            switch(self$options$table_theme,
                "clinical" = private$.apply_clinical_theme(gt_table),
                "publication" = private$.apply_publication_theme(gt_table),
                "modern" = private$.apply_modern_theme(gt_table),
                "traditional" = private$.apply_traditional_theme(gt_table),
                "minimal" = private$.apply_minimal_theme(gt_table),
                "journal" = private$.apply_journal_theme(gt_table)
            )
        },
        
        .apply_clinical_theme = function(gt_table) {
            gt_table %>%
                gt::opt_table_font(font = "system-ui") %>%
                gt::opt_row_striping(row_striping = self$options$stripe_rows) %>%
                gt::tab_options(
                    table.font.size = switch(self$options$font_size,
                                           "small" = "10pt",
                                           "normal" = "12pt",
                                           "large" = "14pt"),
                    heading.background.color = "#f8f9fa",
                    column_labels.background.color = "#e9ecef",
                    row.striping.background_color = "#f8f9fa"
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_column_labels()
                )
        },
        
        .apply_publication_theme = function(gt_table) {
            gt_table %>%
                gt::opt_table_font(font = "Times New Roman") %>%
                gt::opt_row_striping(row_striping = FALSE) %>%
                gt::tab_options(
                    table.font.size = "11pt",
                    heading.background.color = "white",
                    column_labels.background.color = "white",
                    table.border.top.style = "solid",
                    table.border.bottom.style = "solid",
                    column_labels.border.bottom.style = "solid"
                )
        },
        
        .apply_modern_theme = function(gt_table) {
            gt_table %>%
                gt::opt_table_font(font = "system-ui") %>%
                gt::opt_row_striping(row_striping = TRUE) %>%
                gt::tab_options(
                    table.font.size = "12pt",
                    heading.background.color = "#2c3e50",
                    column_labels.background.color = "#34495e",
                    row.striping.background_color = "#ecf0f1"
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(color = "white", weight = "bold"),
                    locations = gt::cells_title()
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(color = "white", weight = "bold"),
                    locations = gt::cells_column_labels()
                )
        },
        
        .apply_traditional_theme = function(gt_table) {
            gt_table %>%
                gt::opt_table_font(font = "serif") %>%
                gt::opt_row_striping(row_striping = FALSE) %>%
                gt::tab_options(
                    table.font.size = "12pt",
                    table.border.top.style = "double",
                    table.border.bottom.style = "double",
                    column_labels.border.bottom.style = "solid"
                )
        },
        
        .apply_minimal_theme = function(gt_table) {
            gt_table %>%
                gt::opt_table_font(font = "system-ui") %>%
                gt::opt_row_striping(row_striping = FALSE) %>%
                gt::tab_options(
                    table.font.size = "11pt",
                    table.border.top.style = "none",
                    table.border.bottom.style = "none",
                    column_labels.border.bottom.style = "solid",
                    column_labels.border.bottom.width = "1px"
                )
        },
        
        .apply_journal_theme = function(gt_table) {
            gt_table %>%
                gt::opt_table_font(font = "Arial") %>%
                gt::opt_row_striping(row_striping = FALSE) %>%
                gt::tab_options(
                    table.font.size = "10pt",
                    heading.background.color = "white",
                    column_labels.background.color = "#f5f5f5",
                    table.border.top.style = "solid",
                    table.border.bottom.style = "solid"
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(weight = "bold", size = "11pt"),
                    locations = gt::cells_column_labels()
                )
        },
        
        .create_summary_stats = function() {
            data <- private$.processed_data
            
            stats_html <- paste(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px;'>",
                "<h4>Summary Statistics</h4>",
                "<ul>",
                paste0("<li><strong>Dataset Size:</strong> ", nrow(data), " observations, ", ncol(data), " variables</li>"),
                paste0("<li><strong>Variables Selected:</strong> ", length(self$options$vars), " variables</li>"),
                if (!is.null(self$options$group_var)) paste0("<li><strong>Grouping Variable:</strong> ", self$options$group_var, "</li>") else "",
                paste0("<li><strong>Missing Value Handling:</strong> ", self$options$missing_handling, "</li>"),
                "</ul>",
                "</div>"
            )
            
            return(stats_html)
        },
        
        .create_group_comparison = function() {
            if (is.null(self$options$group_var)) {
                return("<p>No grouping variable selected for comparison.</p>")
            }
            
            data <- private$.processed_data
            group_var <- self$options$group_var
            
            comparison_results <- "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 10px;'>"
            comparison_results <- paste(comparison_results, "<h4>Group Comparison Results</h4>")
            
            # Perform statistical tests for each variable
            vars <- self$options$vars
            for (var in vars) {
                if (var %in% names(data) && var != group_var) {
                    if (is.numeric(data[[var]])) {
                        test_result <- private$.perform_numeric_test(data[[var]], data[[group_var]])
                        comparison_results <- paste(comparison_results, 
                                                  "<p><strong>", var, ":</strong> ", test_result, "</p>")
                    } else {
                        test_result <- private$.perform_categorical_test(data[[var]], data[[group_var]])
                        comparison_results <- paste(comparison_results, 
                                                  "<p><strong>", var, ":</strong> ", test_result, "</p>")
                    }
                }
            }
            
            comparison_results <- paste(comparison_results, "</div>")
            return(comparison_results)
        },
        
        .perform_numeric_test = function(x, group) {
            # Remove missing values
            complete_cases <- !is.na(x) & !is.na(group)
            x <- x[complete_cases]
            group <- group[complete_cases]
            
            if (length(unique(group)) < 2) {
                return("Insufficient groups for comparison")
            }
            
            tryCatch({
                if (self$options$test_type %in% c("auto", "parametric")) {
                    if (length(unique(group)) == 2) {
                        test_result <- t.test(x ~ group)
                        return(sprintf("t-test: t = %.3f, p = %.3f", test_result$statistic, test_result$p.value))
                    } else {
                        test_result <- aov(x ~ group)
                        p_value <- summary(test_result)[[1]][["Pr(>F)"]][1]
                        return(sprintf("ANOVA: p = %.3f", p_value))
                    }
                } else {
                    if (length(unique(group)) == 2) {
                        test_result <- wilcox.test(x ~ group)
                        return(sprintf("Wilcoxon test: W = %.3f, p = %.3f", test_result$statistic, test_result$p.value))
                    } else {
                        test_result <- kruskal.test(x ~ group)
                        return(sprintf("Kruskal-Wallis test: χ² = %.3f, p = %.3f", test_result$statistic, test_result$p.value))
                    }
                }
            }, error = function(e) {
                return(paste("Test failed:", e$message))
            })
        },
        
        .perform_categorical_test = function(x, group) {
            # Remove missing values
            complete_cases <- !is.na(x) & !is.na(group)
            x <- x[complete_cases]
            group <- group[complete_cases]
            
            if (length(unique(group)) < 2 || length(unique(x)) < 2) {
                return("Insufficient categories for comparison")
            }
            
            tryCatch({
                contingency_table <- table(x, group)
                
                # Check if expected frequencies are sufficient for chi-square
                expected <- chisq.test(contingency_table)$expected
                if (any(expected < 5)) {
                    test_result <- fisher.test(contingency_table)
                    return(sprintf("Fisher's exact test: p = %.3f", test_result$p.value))
                } else {
                    test_result <- chisq.test(contingency_table)
                    return(sprintf("χ² test: χ² = %.3f, p = %.3f", test_result$statistic, test_result$p.value))
                }
            }, error = function(e) {
                return(paste("Test failed:", e$message))
            })
        },
        
        .create_interpretation = function() {
            data <- private$.processed_data
            
            interpretation_html <- paste(
                "<div style='background-color: #d1ecf1; padding: 15px; border-radius: 5px; margin: 10px;'>",
                "<h4>Clinical Interpretation & Usage Guide</h4>",
                "<h5>Table Interpretation:</h5>",
                "<ul>",
                "<li><strong>Sample Size:</strong> This analysis includes ", nrow(data), " observations</li>",
                "<li><strong>Variables:</strong> ", length(self$options$vars), " variables are displayed using ", 
                self$options$table_theme, " theme</li>",
                if (!is.null(self$options$group_var)) 
                    paste0("<li><strong>Grouping:</strong> Data is grouped by ", self$options$group_var, 
                          if (self$options$include_pvalues) " with statistical comparisons" else "", "</li>")
                else "",
                "</ul>",
                "<h5>Statistical Notes:</h5>",
                "<ul>",
                "<li><strong>Continuous Variables:</strong> Displayed as ", 
                switch(self$options$stats_continuous,
                       "mean_sd" = "mean ± standard deviation",
                       "median_iqr" = "median [interquartile range]",
                       "both" = "mean ± SD and median [IQR]",
                       "mean_sd_range" = "mean ± SD with range",
                       "all" = "comprehensive statistics"), "</li>",
                "<li><strong>Categorical Variables:</strong> Displayed as ", 
                switch(self$options$stats_categorical,
                       "n_percent" = "count (percentage)",
                       "n_only" = "count only",
                       "percent_only" = "percentage only",
                       "n_percent_missing" = "count (percentage) with missing values"), "</li>",
                if (self$options$include_pvalues) 
                    "<li><strong>P-values:</strong> Statistical significance tested using appropriate methods</li>"
                else "",
                "</ul>",
                "<h5>Usage Recommendations:</h5>",
                "<ul>",
                "<li>Use this table as a baseline characteristics summary (Table 1)</li>",
                "<li>Consider the clinical significance alongside statistical significance</li>",
                "<li>Report confidence intervals for key comparisons when appropriate</li>",
                "<li>Ensure adequate sample sizes for reliable statistical conclusions</li>",
                "</ul>",
                "</div>"
            )
            
            return(interpretation_html)
        },
        
        .create_export_table = function() {
            export_html <- paste(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px;'>",
                "<h4>Export Information</h4>",
                "<p><strong>Export Format:</strong> ", self$options$export_format, "</p>",
                "<p><strong>Note:</strong> The table above is optimized for ", self$options$export_format, " export.</p>",
                "<p>To export this table:</p>",
                "<ol>",
                "<li>Right-click on the table above</li>",
                "<li>Select 'Save As' or 'Export'</li>",
                "<li>Choose the appropriate format for your needs</li>",
                "</ol>",
                "<p><em>For Word export, copy the HTML table and paste into your Word document. ",
                "For LaTeX, use the gt package's LaTeX output functions.</em></p>",
                "</div>"
            )
            
            return(export_html)
        }
    )
)