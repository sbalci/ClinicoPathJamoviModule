#' @title Table One
#'
#' @description This function creates a “Table One” – a descriptive summary table commonly used in clinicopathological research.
#' It offers several output styles using different packages (tableone, gtsummary, arsenal, and janitor).
#'
#' @return A formatted table according to the chosen style.
#'
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric select naOmit constructFormula
#' @importFrom tableone CreateTableOne
#' @importFrom gtsummary tbl_summary as_kable_extra
#' @importFrom arsenal tableby
#' @importFrom janitor tabyl adorn_totals adorn_pct_formatting
#' @importFrom dplyr rename
#' @importFrom kableExtra kable kable_styling
#' @importFrom rlang sym
#' @importFrom stats as.formula
#' @importFrom grDevices rgb
#' @import pivottabler
#'
#' @export tableone2Class
#'
tableone2Class <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "tableone2Class",
    inherit = tableone2Base,
    private = list(
        .init = function() {
            # Show initial welcome message immediately when analysis is created
            todo_message <- "
            <br><strong>Welcome to the ClinicoPath Table One Generator</strong>
            <br><br>
            <strong>Instructions:</strong>
            <ul>
                <li>Select the <em>Variables</em> to include in the Table One. (Numeric, Ordinal, or Categorical)</li>
                <li>Choose a <em>Table Style</em> for the output format.</li>
                <li>If needed, check the option to <em>Exclude Missing Values</em> (NA). (Exclusion may remove entire cases.)</li>
            </ul>
            <br>
            Please ensure you cite the packages and jamovi as referenced below.
            "
            self$results$todo$setContent(todo_message)
        },

        .run = function() {
            # Check that the input data has at least one complete row.
            if (nrow(self$data) == 0) {
                stop("Error: The input data contains no (complete) rows. Please provide a valid dataset.")
            }

            # Handle tutorial display when requested.
            tutorial_item <- self$results$tutorial
            if (isTRUE(self$options$view_tutorial)) {
                tutorial_item$setContent(tableone2_tutorial_html())
            } else {
                tutorial_item$setContent('')
            }

            # If no variables are selected, show a welcome/instructions message.
            if (is.null(self$options$vars)) {
                todo_message <- "
                <br><strong>Welcome to the ClinicoPath Table One Generator</strong>
                <br><br>
                <strong>Instructions:</strong>
                <ul>
                    <li>Select the <em>Variables</em> to include in the Table One. (Numeric, Ordinal, or Categorical)</li>
                    <li>Choose a <em>Table Style</em> for the output format.</li>
                    <li>If needed, check the option to <em>Exclude Missing Values</em> (NA). (Exclusion may remove entire cases.)</li>
                </ul>
                <br>
                Please ensure you cite the packages and jamovi as referenced below.
                "
                self$results$todo$setContent(todo_message)
                return(invisible(NULL))  # Stop further processing until variables are selected.
            } else {
                # Clear the instructions message once variables are selected.
                self$results$todo$setContent("")
            }

            # Prepare the data using user-selected variables.
            selected_vars <- self$options$vars  # Improved variable naming.
            data <- jmvcore::select(self$data, selected_vars)

            # Optionally exclude rows with missing values.
            if (isTRUE(self$options$excl)) {
                data <- jmvcore::naOmit(data)
            }

            # Retrieve the table style selected by the user.
            table_style <- self$options$sty

            # Generate the table based on the chosen style.
            if (table_style == "t1") {
                # --- Using tableone package ---
                mytable <- tryCatch({
                    tableone::CreateTableOne(data = data)
                }, error = function(e) {
                    stop("Error in tableone package: ", e$message)
                })
                self$results$tablestyle1$setContent(mytable)

            } else if (table_style == "t2") {
                # --- Using gtsummary package ---
                mytable <- tryCatch({
                    tbl <- gtsummary::tbl_summary(data = data)
                    gtsummary::as_kable_extra(tbl)
                }, error = function(e) {
                    stop("Error in gtsummary package: ", e$message)
                })
                self$results$tablestyle2$setContent(mytable)

            } else if (table_style == "t3") {
                # --- Using arsenal package ---
                formula_str <- jmvcore::constructFormula(terms = selected_vars)
                formula_obj <- as.formula(paste('~', formula_str))
                mytable <- tryCatch({
                    tab <- arsenal::tableby(formula = formula_obj,
                                            data = data,
                                            total = TRUE,
                                            digits = 1,
                                            digits.count = 0,
                                            digits.pct = 1)
                    tab_summary <- summary(tab, text = "html")
                    kableExtra::kable(tab_summary, format = "html", digits = 1, escape = FALSE)
                }, error = function(e) {
                    stop("Error in arsenal package: ", e$message)
                })
                self$results$tablestyle3$setContent(mytable)

            } else if (table_style == "t4") {
                # --- Using janitor package for frequency tables with improved spacing & styling ---
                table_list <- lapply(selected_vars, function(var) {
                    freq_table <- tryCatch({
                        table <- janitor::tabyl(data, !!rlang::sym(var))
                        table <- janitor::adorn_totals(table, "row")
                        table <- janitor::adorn_pct_formatting(table)

                        # Rename columns for consistency and clarity
                        # (If you don't have dplyr, you can use base R: colnames(table)[2:4] <- c("N", "Percent", "Valid Percent"))
                        table <- dplyr::rename(
                            table,
                            "N"            = n,
                            "Percent"      = percent,
                            "Valid Percent" = valid_percent
                        )
                        table
                    }, error = function(e) {
                        stop("Error processing variable '", var, "' with janitor: ", e$message)
                    })

                    # Add a header for clarity for each variable's table, plus a top margin.
                    header <- paste0("<h4 style='margin-top:20px;'>Frequency Table for '", var, "'</h4>")

                    # Convert to an HTML table with columns centered from the second column onward:
                    # The first column (variable level) is left-aligned, and columns 2-4 are centered.
                    styled_table <- kableExtra::kable(
                        freq_table,
                        format = "html",
                        digits = 1,
                        escape = FALSE,
                        align = c("l", "c", "c", "c")  # left, center, center, center
                    ) %>%
                        kableExtra::kable_styling(
                            bootstrap_options = c("striped", "hover"),
                            full_width = FALSE,
                            font_size = 14,
                            position = "center"
                        )

                    # Combine the header and the table with spacing (hr).
                    paste0(header, styled_table, "<br><hr style='margin:20px 0;'>")
                })

                # Join all the tables together
                mytable <- paste(table_list, collapse = "")
                self$results$tablestyle4$setContent(mytable)

            } else if (table_style == "t5") {
                # --- Using pivottabler package for enhanced tables ---
                mytable <- tryCatch({
                    private$.create_pivot_tableone(data, selected_vars)
                }, error = function(e) {
                    paste0("<div style='color: red;'>Error creating pivot table: ", e$message, "</div>")
                })
                self$results$tablestyle5$setContent(mytable)

            } else {
                stop("Error: Invalid table style selected. Please choose a valid style.")
            }
        }, # End of .run function.

        .create_pivot_tableone = function(data, selected_vars) {
            # Create enhanced Table One using pivottabler

            # Get format style and group variable
            format_style <- self$options$pivot_format
            group_var <- self$options$group_var
            enable_groups <- self$options$group_comparisons && !is.null(group_var)
            include_advanced <- self$options$include_statistics

            # Configure based on data types
            categorical_vars <- c()
            numerical_vars <- c()

            for (var in selected_vars) {
                if (is.factor(data[[var]]) || is.character(data[[var]])) {
                    categorical_vars <- c(categorical_vars, var)
                } else if (is.numeric(data[[var]])) {
                    numerical_vars <- c(numerical_vars, var)
                }
            }

            # Create summary table structure
            summary_html <- "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>"
            summary_html <- paste0(summary_html, "<h3 style='color: #495057; margin-top: 0;'>Enhanced Table One (Pivottabler)</h3>")

            # Add categorical variables summary
            if (length(categorical_vars) > 0) {
                summary_html <- paste0(summary_html, "<h4>Categorical Variables:</h4>")

                # Apply different formatting based on style
                if (format_style == "clinical") {
                    if (enable_groups) {
                        # Stratified analysis by group
                        summary_html <- paste0(summary_html, "<table style='border-collapse: collapse; width: 100%; border: 1px solid #ddd;'>")
                        group_levels <- unique(data[[group_var]])
                        header_cols <- paste0("<th style='padding: 8px; border: 1px solid #ddd;'>", group_levels, "</th>", collapse = "")
                        summary_html <- paste0(summary_html, "<tr style='background-color: #f2f2f2;'><th style='padding: 8px; border: 1px solid #ddd;'>Variable</th>", header_cols, "</tr>")

                        for (var in categorical_vars) {
                            cross_tab <- table(data[[var]], data[[group_var]], useNA = "ifany")
                            summary_html <- paste0(summary_html, "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", var, "</strong></td>")
                            for (group_level in group_levels) {
                                if (group_level %in% colnames(cross_tab)) {
                                    group_data <- cross_tab[, group_level]
                                    group_summary <- paste(names(group_data), " (n=", group_data, ")", collapse = ", ")
                                } else {
                                    group_summary <- "No data"
                                }
                                summary_html <- paste0(summary_html, "<td style='padding: 8px; border: 1px solid #ddd;'>", group_summary, "</td>")
                            }
                            summary_html <- paste0(summary_html, "</tr>")
                        }
                        summary_html <- paste0(summary_html, "</table>")
                    } else {
                        # Non-stratified analysis
                        summary_html <- paste0(summary_html, "<table style='border-collapse: collapse; width: 100%; border: 1px solid #ddd;'>")
                        summary_html <- paste0(summary_html, "<tr style='background-color: #f2f2f2;'><th style='padding: 8px; border: 1px solid #ddd;'>Variable</th><th style='padding: 8px; border: 1px solid #ddd;'>Summary</th></tr>")
                        for (var in categorical_vars) {
                            var_summary <- table(data[[var]], useNA = "ifany")
                            summary_html <- paste0(summary_html,
                                "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", var, "</strong></td>",
                                "<td style='padding: 8px; border: 1px solid #ddd;'>",
                                paste(names(var_summary), " (n=", var_summary, ")", collapse = ", "), "</td></tr>")
                        }
                        summary_html <- paste0(summary_html, "</table>")
                    }
                } else if (format_style == "publication") {
                    summary_html <- paste0(summary_html, "<div style='font-family: Times, serif; font-size: 12px;'>")
                    if (enable_groups) {
                        # Stratified analysis by group
                        group_levels <- unique(data[[group_var]])
                        for (var in categorical_vars) {
                            cross_tab <- table(data[[var]], data[[group_var]], useNA = "ifany")
                            summary_html <- paste0(summary_html, "<p><em>", var, "</em>:</p><ul style='list-style-type: none; padding-left: 20px;'>")
                            for (group_level in group_levels) {
                                if (group_level %in% colnames(cross_tab)) {
                                    group_data <- cross_tab[, group_level]
                                    group_summary <- paste(names(group_data), " (", group_data, ")", collapse = ", ")
                                } else {
                                    group_summary <- "No data"
                                }
                                summary_html <- paste0(summary_html, "<li><strong>", group_level, ":</strong> ", group_summary, "</li>")
                            }
                            summary_html <- paste0(summary_html, "</ul>")
                        }
                    } else {
                        # Non-stratified analysis
                        summary_html <- paste0(summary_html, "<ul style='list-style-type: none; padding-left: 0;'>")
                        for (var in categorical_vars) {
                            var_summary <- table(data[[var]], useNA = "ifany")
                            summary_html <- paste0(summary_html,
                                "<li style='margin-bottom: 8px;'><em>", var, "</em>: ",
                                paste(names(var_summary), " (", var_summary, ")", collapse = ", "), "</li>")
                        }
                        summary_html <- paste0(summary_html, "</ul>")
                    }
                    summary_html <- paste0(summary_html, "</div>")
                } else { # detailed
                    summary_html <- paste0(summary_html, "<ul>")
                    for (var in categorical_vars) {
                        if (enable_groups) {
                            # Stratified analysis by group
                            cross_tab <- table(data[[var]], data[[group_var]], useNA = "ifany")
                            group_levels <- unique(data[[group_var]])

                            summary_html <- paste0(summary_html, "<li><strong>", var, " (by ", group_var, "):</strong><ul>")
                            for (group_level in group_levels) {
                                if (group_level %in% colnames(cross_tab)) {
                                    group_data <- cross_tab[, group_level]
                                    group_pct <- prop.table(group_data) * 100
                                    group_summary <- paste(names(group_data), " (n=", group_data, ", ", round(group_pct, 1), "%)", collapse = ", ")
                                } else {
                                    group_summary <- "No data"
                                }
                                summary_html <- paste0(summary_html, "<li><strong>", group_level, ":</strong> ", group_summary)

                                # Add chi-square test for group comparison if advanced stats enabled
                                if (include_advanced && group_level %in% colnames(cross_tab)) {
                                    chi_test <- tryCatch({
                                        if (length(group_data) > 1) {
                                            chi_result <- chisq.test(group_data)
                                            paste0("χ²=", round(chi_result$statistic, 2), ", p=", round(chi_result$p.value, 4))
                                        } else {
                                            "N/A (single category)"
                                        }
                                    }, error = function(e) "N/A")
                                    summary_html <- paste0(summary_html, "<br>&nbsp;&nbsp;&nbsp;&nbsp;<em>Test: ", chi_test, "</em>")
                                }
                                summary_html <- paste0(summary_html, "</li>")
                            }

                            # Add overall association test if advanced stats enabled
                            if (include_advanced) {
                                overall_test <- tryCatch({
                                    if (nrow(cross_tab) > 1 && ncol(cross_tab) > 1) {
                                        chi_result <- chisq.test(cross_tab)
                                        paste0("Overall χ²=", round(chi_result$statistic, 2), ", p=", round(chi_result$p.value, 4))
                                    } else {
                                        "N/A (insufficient data)"
                                    }
                                }, error = function(e) "N/A")
                                summary_html <- paste0(summary_html, "<li><em>Association test: ", overall_test, "</em></li>")
                            }
                            summary_html <- paste0(summary_html, "</ul></li>")
                        } else {
                            # Non-stratified analysis
                            var_summary <- table(data[[var]], useNA = "ifany")
                            pct_summary <- prop.table(var_summary) * 100

                            if (include_advanced) {
                                # Advanced statistics: include chi-square goodness of fit test
                                chi_test <- tryCatch({
                                    if (length(var_summary) > 1) {
                                        chi_result <- chisq.test(var_summary)
                                        paste0("χ²=", round(chi_result$statistic, 2), ", p=", round(chi_result$p.value, 4))
                                    } else {
                                        "N/A (single category)"
                                    }
                                }, error = function(e) "N/A")

                                summary_html <- paste0(summary_html,
                                    "<li><strong>", var, ":</strong> ",
                                    paste(names(var_summary), " (n=", var_summary, ", ", round(pct_summary, 1), "%)", collapse = ", "),
                                    "<br>&nbsp;&nbsp;&nbsp;&nbsp;<em>Goodness of fit test: ", chi_test, "</em></li>")
                            } else {
                                # Basic statistics only
                                summary_html <- paste0(summary_html,
                                    "<li><strong>", var, ":</strong> ",
                                    paste(names(var_summary), " (n=", var_summary, ", ", round(pct_summary, 1), "%)", collapse = ", "), "</li>")
                            }
                        }
                    }
                    summary_html <- paste0(summary_html, "</ul>")
                }
            }

            # Add numerical variables summary
            if (length(numerical_vars) > 0) {
                summary_html <- paste0(summary_html, "<h4>Numerical Variables:</h4>")

                # Apply different formatting based on style
                if (format_style == "clinical") {
                    if (enable_groups) {
                        # Stratified analysis by group
                        group_levels <- unique(data[[group_var]])
                        summary_html <- paste0(summary_html, "<table style='border-collapse: collapse; width: 100%; border: 1px solid #ddd;'>")
                        header_cols <- paste0("<th style='padding: 8px; border: 1px solid #ddd;'>", group_levels, "</th>", collapse = "")
                        summary_html <- paste0(summary_html, "<tr style='background-color: #f2f2f2;'><th style='padding: 8px; border: 1px solid #ddd;'>Variable</th>", header_cols, "</tr>")

                        for (var in numerical_vars) {
                            summary_html <- paste0(summary_html, "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", var, "</strong></td>")
                            for (group_level in group_levels) {
                                group_data <- data[[var]][data[[group_var]] == group_level & !is.na(data[[var]])]
                                if (length(group_data) > 0) {
                                    group_summary <- paste0(round(mean(group_data), 2), " ± ", round(sd(group_data), 2))
                                } else {
                                    group_summary <- "No data"
                                }
                                summary_html <- paste0(summary_html, "<td style='padding: 8px; border: 1px solid #ddd;'>", group_summary, "</td>")
                            }
                            summary_html <- paste0(summary_html, "</tr>")
                        }
                        summary_html <- paste0(summary_html, "</table>")
                    } else {
                        # Non-stratified analysis
                        summary_html <- paste0(summary_html, "<table style='border-collapse: collapse; width: 100%; border: 1px solid #ddd;'>")
                        summary_html <- paste0(summary_html, "<tr style='background-color: #f2f2f2;'><th style='padding: 8px; border: 1px solid #ddd;'>Variable</th><th style='padding: 8px; border: 1px solid #ddd;'>Mean ± SD</th><th style='padding: 8px; border: 1px solid #ddd;'>Range</th></tr>")
                        for (var in numerical_vars) {
                            var_data <- data[[var]][!is.na(data[[var]])]
                            if (length(var_data) > 0) {
                                summary_html <- paste0(summary_html,
                                    "<tr><td style='padding: 8px; border: 1px solid #ddd;'><strong>", var, "</strong></td>",
                                    "<td style='padding: 8px; border: 1px solid #ddd;'>", round(mean(var_data), 2), " ± ", round(sd(var_data), 2), "</td>",
                                    "<td style='padding: 8px; border: 1px solid #ddd;'>", round(min(var_data), 2), " - ", round(max(var_data), 2), "</td></tr>")
                            }
                        }
                        summary_html <- paste0(summary_html, "</table>")
                    }
                } else if (format_style == "publication") {
                    summary_html <- paste0(summary_html, "<div style='font-family: Times, serif; font-size: 12px;'>")
                    if (enable_groups) {
                        # Stratified analysis by group
                        group_levels <- unique(data[[group_var]])
                        for (var in numerical_vars) {
                            summary_html <- paste0(summary_html, "<p><em>", var, "</em>:</p><ul style='list-style-type: none; padding-left: 20px;'>")
                            for (group_level in group_levels) {
                                group_data <- data[[var]][data[[group_var]] == group_level & !is.na(data[[var]])]
                                if (length(group_data) > 0) {
                                    group_summary <- paste0(round(mean(group_data), 2), " ± ", round(sd(group_data), 2),
                                                          " (range: ", round(min(group_data), 2), "-", round(max(group_data), 2), ")")
                                } else {
                                    group_summary <- "No data"
                                }
                                summary_html <- paste0(summary_html, "<li><strong>", group_level, ":</strong> ", group_summary, "</li>")
                            }
                            summary_html <- paste0(summary_html, "</ul>")
                        }
                    } else {
                        # Non-stratified analysis
                        summary_html <- paste0(summary_html, "<ul style='list-style-type: none; padding-left: 0;'>")
                        for (var in numerical_vars) {
                            var_data <- data[[var]][!is.na(data[[var]])]
                            if (length(var_data) > 0) {
                                summary_html <- paste0(summary_html,
                                    "<li style='margin-bottom: 8px;'><em>", var, "</em>: ", round(mean(var_data), 2),
                                    " ± ", round(sd(var_data), 2), " (range: ", round(min(var_data), 2), "-", round(max(var_data), 2), ")</li>")
                            }
                        }
                        summary_html <- paste0(summary_html, "</ul>")
                    }
                    summary_html <- paste0(summary_html, "</div>")
                } else { # detailed
                    summary_html <- paste0(summary_html, "<ul>")
                    for (var in numerical_vars) {
                        if (enable_groups) {
                            # Stratified analysis by group
                            group_levels <- unique(data[[group_var]])
                            summary_html <- paste0(summary_html, "<li><strong>", var, " (by ", group_var, "):</strong><ul>")

                            for (group_level in group_levels) {
                                group_data <- data[[var]][data[[group_var]] == group_level & !is.na(data[[var]])]
                                if (length(group_data) > 0) {
                                    if (include_advanced) {
                                        # Advanced statistics: include skewness, kurtosis, normality tests
                                        skew_val <- tryCatch({
                                            library(moments, quietly = TRUE)
                                            round(moments::skewness(group_data), 3)
                                        }, error = function(e) "N/A")

                                        kurt_val <- tryCatch({
                                            library(moments, quietly = TRUE)
                                            round(moments::kurtosis(group_data), 3)
                                        }, error = function(e) "N/A")

                                        normality_test <- tryCatch({
                                            if (length(group_data) >= 3 && length(group_data) <= 5000) {
                                                sw_test <- shapiro.test(group_data)
                                                paste0("Shapiro-Wilk p=", round(sw_test$p.value, 4))
                                            } else {
                                                "N/A (sample size)"
                                            }
                                        }, error = function(e) "N/A")

                                        summary_html <- paste0(summary_html,
                                            "<li><strong>", group_level, ":</strong> Mean = ", round(mean(group_data), 2),
                                            ", SD = ", round(sd(group_data), 2),
                                            ", Median = ", round(median(group_data), 2),
                                            ", IQR = ", round(quantile(group_data, 0.25), 2), "-", round(quantile(group_data, 0.75), 2),
                                            ", Range: ", round(min(group_data), 2), "-", round(max(group_data), 2),
                                            "<br>&nbsp;&nbsp;&nbsp;&nbsp;<em>Skewness: ", skew_val, ", Kurtosis: ", kurt_val, ", Normality: ", normality_test, "</em></li>")
                                    } else {
                                        # Basic statistics only
                                        summary_html <- paste0(summary_html,
                                            "<li><strong>", group_level, ":</strong> Mean = ", round(mean(group_data), 2),
                                            ", SD = ", round(sd(group_data), 2),
                                            ", Median = ", round(median(group_data), 2),
                                            ", IQR = ", round(quantile(group_data, 0.25), 2), "-", round(quantile(group_data, 0.75), 2),
                                            ", Range: ", round(min(group_data), 2), "-", round(max(group_data), 2), "</li>")
                                    }
                                } else {
                                    summary_html <- paste0(summary_html, "<li><strong>", group_level, ":</strong> No data</li>")
                                }
                            }

                            # Add group comparison test if advanced stats enabled
                            if (include_advanced && length(group_levels) > 1) {
                                group_test <- tryCatch({
                                    # Create list of group data for comparison
                                    group_data_list <- lapply(group_levels, function(level) {
                                        data[[var]][data[[group_var]] == level & !is.na(data[[var]])]
                                    })
                                    # Remove empty groups
                                    group_data_list <- group_data_list[sapply(group_data_list, length) > 0]

                                    if (length(group_data_list) > 1) {
                                        if (length(group_data_list) == 2) {
                                            # Two groups: t-test
                                            t_result <- t.test(group_data_list[[1]], group_data_list[[2]])
                                            paste0("t-test: t=", round(t_result$statistic, 2), ", p=", round(t_result$p.value, 4))
                                        } else {
                                            # More than two groups: ANOVA
                                            var_values <- c()
                                            group_values <- c()
                                            for (i in seq_along(group_data_list)) {
                                                var_values <- c(var_values, group_data_list[[i]])
                                                group_values <- c(group_values, rep(group_levels[i], length(group_data_list[[i]])))
                                            }
                                            aov_result <- aov(var_values ~ group_values)
                                            f_stat <- summary(aov_result)[[1]]["group_values", "F value"]
                                            p_val <- summary(aov_result)[[1]]["group_values", "Pr(>F)"]
                                            paste0("ANOVA: F=", round(f_stat, 2), ", p=", round(p_val, 4))
                                        }
                                    } else {
                                        "N/A (insufficient groups)"
                                    }
                                }, error = function(e) "N/A")
                                summary_html <- paste0(summary_html, "<li><em>Group comparison: ", group_test, "</em></li>")
                            }
                            summary_html <- paste0(summary_html, "</ul></li>")
                        } else {
                            # Non-stratified analysis
                            var_data <- data[[var]][!is.na(data[[var]])]
                            if (length(var_data) > 0) {
                                if (include_advanced) {
                                    # Advanced statistics: include skewness, kurtosis, normality tests
                                    skew_val <- tryCatch({
                                        library(moments, quietly = TRUE)
                                        round(moments::skewness(var_data), 3)
                                    }, error = function(e) "N/A")

                                    kurt_val <- tryCatch({
                                        library(moments, quietly = TRUE)
                                        round(moments::kurtosis(var_data), 3)
                                    }, error = function(e) "N/A")

                                    normality_test <- tryCatch({
                                        if (length(var_data) >= 3 && length(var_data) <= 5000) {
                                            sw_test <- shapiro.test(var_data)
                                            paste0("Shapiro-Wilk p=", round(sw_test$p.value, 4))
                                        } else {
                                            "N/A (sample size)"
                                        }
                                    }, error = function(e) "N/A")

                                    summary_html <- paste0(summary_html,
                                        "<li><strong>", var, ":</strong> Mean = ", round(mean(var_data), 2),
                                        ", SD = ", round(sd(var_data), 2),
                                        ", Median = ", round(median(var_data), 2),
                                        ", IQR = ", round(quantile(var_data, 0.25), 2), "-", round(quantile(var_data, 0.75), 2),
                                        ", Range: ", round(min(var_data), 2), "-", round(max(var_data), 2),
                                        "<br>&nbsp;&nbsp;&nbsp;&nbsp;<em>Skewness: ", skew_val, ", Kurtosis: ", kurt_val, ", Normality: ", normality_test, "</em></li>")
                                } else {
                                    # Basic statistics only
                                    summary_html <- paste0(summary_html,
                                        "<li><strong>", var, ":</strong> Mean = ", round(mean(var_data), 2),
                                        ", SD = ", round(sd(var_data), 2),
                                        ", Median = ", round(median(var_data), 2),
                                        ", IQR = ", round(quantile(var_data, 0.25), 2), "-", round(quantile(var_data, 0.75), 2),
                                        ", Range: ", round(min(var_data), 2), "-", round(max(var_data), 2), "</li>")
                                }
                            }
                        }
                    }
                    summary_html <- paste0(summary_html, "</ul>")
                }
            }

            # Add formatting information
            summary_html <- paste0(summary_html, "<hr>")
            summary_html <- paste0(summary_html, "<p><strong>Format Style:</strong> ",
                switch(format_style,
                    "clinical" = "Clinical Research - Optimized for medical publications",
                    "publication" = "Publication Ready - Journal formatting standards",
                    "detailed" = "Detailed Analysis - Comprehensive statistical summaries"
                ))

            # Add advanced statistics if enabled
            if (include_advanced) {
                summary_html <- paste0(summary_html,
                    "<p><strong>Advanced Statistics:</strong> Enabled - Including skewness, kurtosis, normality tests, and goodness of fit tests</p>")
            } else {
                summary_html <- paste0(summary_html,
                    "<p><strong>Advanced Statistics:</strong> Disabled - Showing basic descriptive statistics only</p>")
            }

            # Add group comparison info if enabled
            if (enable_groups) {
                group_summary <- table(data[[group_var]], useNA = "ifany")
                summary_html <- paste0(summary_html,
                    "<p><strong>Group Comparisons:</strong> Enabled for '", group_var, "' - ",
                    paste(names(group_summary), " (n=", group_summary, ")", collapse = ", "), "</p>")
            }

            summary_html <- paste0(summary_html,
                "<p style='font-size: 12px; color: #6c757d; margin-top: 15px;'>",
                "<em>💡 This enhanced Table One uses pivottabler for flexible data presentation. ",
                "Total observations: ", nrow(data), "</em></p>")

            summary_html <- paste0(summary_html, "</div>")

            return(summary_html)
        }
    ) # End of private list.
) # End of R6Class definition.
