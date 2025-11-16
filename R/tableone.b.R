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
#' @importFrom htmltools htmlEscape
#'
#' @export tableoneClass
#'
tableoneClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .run = function() {
            # NOTE: This function uses HTML outputs for messages instead of jmvcore::Notice
            # due to serialization constraints in jamovi's protobuf system.
            # When jamovi framework supports Notice serialization, migrate to:
            # - NoticeType$ERROR for missing data/variables
            # - NoticeType$STRONG_WARNING for data quality issues
            # - NoticeType$WARNING for recommendations
            # - NoticeType$INFO for confirmations

            # Check that the input data has at least one complete row.
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$todo$setContent("
                    <br><strong>No Data Available</strong>
                    <br><br>
                    <ul>
                        <li>Please load a dataset before using Table One.</li>
                        <li>Check that your data file is properly imported.</li>
                    </ul>
                ")
                private$.setAboutContent()
                return(invisible(NULL))
            }

            # If no variables are selected, show a welcome/instructions message.
            if (is.null(self$options$vars)) {
                self$results$todo$setContent(private$.buildWelcomeMessage())
                private$.setAboutContent()
                return(invisible(NULL))  # Stop further processing until variables are selected.
            } else {
                # Clear the instructions message once variables are selected.
                self$results$todo$setContent("")
            }

            # Prepare the data using user-selected variables.
            selected_vars <- self$options$vars  # Improved variable naming.

            # Checkpoint before data preparation (potentially expensive for large datasets)
            private$.checkpoint()

            data <- jmvcore::select(self$data, selected_vars)

            # CRITICAL FIX: Capture original data stats BEFORE naOmit
            # This ensures we report actual missingness, not post-exclusion stats
            original_data <- data
            original_n <- nrow(original_data)
            original_complete <- sum(complete.cases(original_data))

            # Optionally exclude rows with missing values.
            excluded_n <- 0
            if (isTRUE(self$options$excl)) {
                data <- jmvcore::naOmit(data)
                excluded_n <- original_n - nrow(data)
            }

            # Retrieve the table style selected by the user.
            table_style <- self$options$sty

            # Control visibility of educational outputs based on user options
            self$results$summary$setVisible(isTRUE(self$options$showSummary))
            self$results$about$setVisible(isTRUE(self$options$showAbout))
            self$results$reportSentence$setVisible(isTRUE(self$options$showReportSentence))

            # Generate clinical summaries and data quality checks
            # Only populate if visible (saves computation)
            if (isTRUE(self$options$showSummary)) {
                private$.generateSummary(data, selected_vars, original_data, excluded_n)
            }

            if (isTRUE(self$options$showAbout)) {
                private$.setAboutContent()
            }

            if (isTRUE(self$options$showReportSentence)) {
                private$.setReportSentence(data, selected_vars, original_data, excluded_n)
            }

            private$.checkDataQuality(data, selected_vars, original_data)

            # Generate the table based on the chosen style.
            if (table_style == "t1") {
                # --- Using tableone package ---
                # Checkpoint before expensive statistical computation
                private$.checkpoint()

                mytable <- tryCatch({
                    tableone::CreateTableOne(data = data)
                }, error = function(e) {
                    if (grepl("insufficient", tolower(e$message))) {
                        stop("Insufficient data for Table One analysis. Ensure you have at least 2 complete cases and check for missing values. Try selecting different variables or disabling 'Exclude Missing Values'.")
                    } else {
                        stop("Error creating Table One: ", e$message, ". Check that variables have valid data and appropriate types. Categorical variables should be factors. Numeric variables should contain valid numbers.")
                    }
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle1$setContent(mytable)

            } else if (table_style == "t2") {
                # --- Using gtsummary package ---
                # Checkpoint before expensive gtsummary computation
                private$.checkpoint()

                mytable <- tryCatch({
                    tbl <- gtsummary::tbl_summary(data = data)
                    gtsummary::as_kable_extra(tbl)
                }, error = function(e) {
                    stop("Error creating gtsummary table: ", e$message, ". Check that variables have valid data and appropriate types. gtsummary requires properly formatted variables for summarization.")
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle2$setContent(mytable)

            } else if (table_style == "t3") {
                # --- Using arsenal package ---
                # Checkpoint before expensive arsenal computation
                private$.checkpoint()

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
                    stop("Error creating arsenal table: ", e$message, ". Arsenal requires properly formatted variables. Check that categorical variables are factors and numeric variables contain valid numbers.")
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle3$setContent(mytable)

            } else if (table_style == "t4") {
                # --- Using janitor package for frequency tables with improved spacing & styling ---
                # Checkpoint before starting the variable loop
                private$.checkpoint()

                # Wrap entire janitor operation in tryCatch for error handling
                result <- tryCatch({
                    table_list <- lapply(selected_vars, function(var) {
                    # Checkpoint for each variable processing (for incremental results)
                    private$.checkpoint(flush = FALSE)
                    
                    freq_table <- tryCatch({
                        # Check if variable exists and has data
                        if (!var %in% names(data)) {
                            stop("Variable '", htmltools::htmlEscape(var), "' not found in data")
                        }

                        # Remove missing values for this variable to avoid issues
                        var_data <- data[!is.na(data[[var]]), ]

                        if (nrow(var_data) == 0) {
                            stop("Variable '", htmltools::htmlEscape(var), "' has no non-missing values")
                        }
                        
                        # Create tabyl table
                        table <- janitor::tabyl(var_data, !!rlang::sym(var))
                        
                        # Add totals
                        table <- janitor::adorn_totals(table, "row")
                        
                        # Add percentage formatting - but handle the case where it might fail
                        table <- tryCatch({
                            janitor::adorn_pct_formatting(table)
                        }, error = function(e) {
                            # If pct formatting fails, just return the table with totals
                            table
                        })

                        # Get the actual column names to handle different janitor output formats
                        col_names <- names(table)
                        
                        # Rename columns for consistency - use more flexible approach
                        if (length(col_names) >= 2) {
                            # First column is typically the variable values, second is counts
                            names(table)[2] <- "N"
                        }
                        if (length(col_names) >= 3) {
                            names(table)[3] <- "Percent"
                        }
                        if (length(col_names) >= 4) {
                            names(table)[4] <- "Valid Percent"
                        }
                        
                        table
                    }, error = function(e) {
                        # Provide more detailed error information
                        safe_var <- htmltools::htmlEscape(var)
                        stop("Error processing variable '", safe_var, "' with janitor: ", e$message,
                             " (Variable type: ", class(data[[var]])[1],
                             ", Non-missing values: ", sum(!is.na(data[[var]])), ")")
                    })

                    # Add a header for clarity for each variable's table, plus a top margin.
                    # Use escaped variable name for safe HTML rendering
                    safe_var_name <- htmltools::htmlEscape(var)
                    header <- paste0("<h4 style='margin-top:20px;'>Frequency Table for '", safe_var_name, "'</h4>")

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
                    paste(table_list, collapse = "")
                }, error = function(e) {
                    stop("Error creating frequency tables with janitor: ", e$message, ". Check that variables have valid data. Janitor works best with categorical or discrete variables.")
                })

                # Checkpoint after expensive operation to allow UI update
                private$.checkpoint()
                self$results$tablestyle4$setContent(result)
            } else {
                stop("Invalid table style selected. Please choose a valid table style from the options (tableone, gtsummary, arsenal, janitor).")
            }
        }, # End of .run function.

        # ========================================================================
        # HTML Builder Helper Functions
        # ========================================================================
        # These helpers extract HTML string building logic for maintainability.
        # When jamovi supports Notice serialization, these can be migrated to
        # Notice objects with appropriate NoticeType.

        .buildWelcomeMessage = function() {
            "
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
        },

        .buildDataQualityHtml = function(warnings, recommendations) {
            # Build HTML for data quality warnings and recommendations
            # Returns empty string if no issues detected
            if (length(warnings) == 0 && length(recommendations) == 0) {
                return("")
            }

            html <- paste0(
                "<div style='background-color: #fff8dc; padding: 15px; border-left: 4px solid #ffa500; margin: 10px 0;'>",
                "<h4>Data Quality & Assumptions</h4>"
            )

            if (length(warnings) > 0) {
                html <- paste0(html,
                    "<p><strong>Warnings:</strong></p><ul>",
                    paste0("<li>", warnings, "</li>", collapse = ""),
                    "</ul>"
                )
            }

            if (length(recommendations) > 0) {
                html <- paste0(html,
                    "<p><strong>Recommendations:</strong></p><ul>",
                    paste0("<li>", recommendations, "</li>", collapse = ""),
                    "</ul>"
                )
            }

            paste0(html, "</div>")
        },

        .buildDataQualityOkHtml = function(n_final, missing_pct_original) {
            # Build HTML for successful data quality check
            paste0(
                "<div style='background-color: #e8f5e9; padding: 15px; border-left: 4px solid #4caf50; margin: 10px 0;'>",
                "<h4>Data Quality Check ✓</h4>",
                "<p><strong>Sample size:</strong> N = ", n_final, "</p>",
                "<p><strong>Complete cases:</strong> ", round(100 - missing_pct_original, 1), "%</p>",
                "<p><em>No major data quality issues detected.</em></p>",
                "</div>"
            )
        },
        
        .setAboutContent = function() {
            about_text <- "
            <div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                <h4>About Table One</h4>
                <p><strong>Purpose:</strong> Table One is a standardized descriptive table used in medical research to summarize baseline characteristics and demographic information of study participants.</p>
                
                <p><strong>When to use:</strong></p>
                <ul>
                    <li>Describing patient demographics and clinical characteristics</li>
                    <li>Summarizing baseline features of your study population</li>
                    <li>Presenting lab values, vital signs, or biomarker data</li>
                    <li>Creating manuscript-ready descriptive summary tables</li>
                </ul>
                
                <p><strong>Variable types:</strong></p>
                <ul>
                    <li><em>Continuous:</em> Age, weight, lab values (shown as mean ± SD or median [IQR])</li>
                    <li><em>Categorical:</em> Sex, diagnosis, treatment groups (shown as N (%))</li>
                    <li><em>Ordinal:</em> Tumor grade, ECOG status (shown as N (%) by level)</li>
                </ul>
                
                <p><strong>Output styles:</strong></p>
                <ul>
                    <li><strong>tableone:</strong> Standard medical format, suitable for most clinical papers</li>
                    <li><strong>gtsummary:</strong> Publication-ready formatting with professional styling</li>
                    <li><strong>arsenal:</strong> Comprehensive descriptive tables with detailed summaries</li>
                    <li><strong>janitor:</strong> Simple frequency tables, good for data exploration</li>
                </ul>
            </div>"
            self$results$about$setContent(about_text)
        },
        
        .generateSummary = function(data, vars, original_data, excluded_n) {
            # CRITICAL FIX: Report statistics from ORIGINAL data to show true missingness
            n_original <- nrow(original_data)
            n_final <- nrow(data)
            n_vars <- length(vars)

            # Calculate missing data from ORIGINAL dataset
            n_complete_original <- sum(complete.cases(original_data))
            missing_pct_original <- round(100 * (1 - n_complete_original / n_original), 1)

            # Variable type analysis (on final data for consistency)
            var_types <- sapply(data, function(x) {
                if (is.numeric(x)) "Numeric"
                else if (is.factor(x)) "Categorical"
                else if (is.logical(x)) "Logical"
                else "Other"
            })
            type_summary <- table(var_types)
            type_text <- paste(names(type_summary), ":", type_summary, collapse = "; ")

            # Per-variable missing counts (from ORIGINAL data)
            var_missing <- sapply(vars, function(v) sum(is.na(original_data[[v]])))
            high_missing_vars <- vars[var_missing > n_original * 0.2]  # >20% missing
            # Escape variable names for safe HTML display
            high_missing_vars_safe <- sapply(high_missing_vars, htmltools::htmlEscape)

            # Build summary text with transparent reporting
            summary_text <- paste0(
                "<div style='background-color: #e8f4fd; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>",
                "<h4>Analysis Summary</h4>",

                # Original dataset info
                "<p><strong>Original dataset:</strong> ", n_original, " cases with ", n_vars, " selected variables</p>",
                "<p><strong>Complete cases (original):</strong> ", n_complete_original, " (",
                round(100 * n_complete_original / n_original, 1), "%)</p>",

                # Missing data transparency
                if (missing_pct_original > 0) {
                    paste0("<p><strong>Missing data (original):</strong> ", missing_pct_original,
                           "% of cases have at least one missing value",
                           if (length(high_missing_vars_safe) > 0) {
                               paste0(" <br><em>Variables with >20% missing: ",
                                      paste(high_missing_vars_safe, collapse = ", "), "</em>")
                           } else "",
                           "</p>")
                } else "",

                # Exclusion warning if applicable
                if (excluded_n > 0) {
                    paste0("<p style='color: #d9534f;'><strong>⚠️ Case exclusion:</strong> ",
                           excluded_n, " cases (", round(100 * excluded_n / n_original, 1),
                           "%) excluded due to missing values. <strong>Final N = ", n_final,
                           "</strong></p>",
                           "<p style='color: #856404; background-color: #fff3cd; padding: 8px; border-radius: 4px;'>",
                           "<em>Note: Listwise deletion was applied. The table below shows statistics for the ",
                           n_final, " complete cases only. Per-variable denominators may differ if variables have different missing patterns.</em></p>")
                } else {
                    paste0("<p><strong>Analysis sample:</strong> ", n_final, " cases (no exclusions applied)</p>",
                           if (missing_pct_original > 0) {
                               "<p style='color: #856404; background-color: #fff3cd; padding: 8px; border-radius: 4px;'><em>⚠️ Note: Missing values are present but NOT excluded. Different variables may have different sample sizes (denominators) in the table below. Consider enabling 'Exclude Missing Values' for consistent denominators.</em></p>"
                           } else "")
                },

                "<p><strong>Variable types:</strong> ", type_text, "</p>",
                "<p><em>This Table One summarizes baseline characteristics commonly reported in clinical research manuscripts.</em></p>",
                "</div>"
            )
            self$results$summary$setContent(summary_text)
        },
        
        .checkDataQuality = function(data, vars, original_data) {
            # NOTE: Data quality thresholds align with clinical research standards:
            # - STRONG_WARNING thresholds: N<10, missing>50%, exclusion>30%
            # - WARNING thresholds: N<30, missing>20%, exclusion>10%
            # These would map to NoticeType when Notice serialization is supported.

            # CRITICAL FIX: Check sample size on FINAL data (after exclusions)
            n_final <- nrow(data)
            n_original <- nrow(original_data)

            warnings <- c()
            recommendations <- c()

            # Check sample size (clinical thresholds)
            if (n_final < 10) {
                # STRONG_WARNING: Very small sample
                warnings <- c(warnings, paste0("<strong>⚠️ Very small final sample size (N = ", n_final, ").</strong> Results may be unreliable with fewer than 10 cases. Consider collecting more data or using exact tests."))
            } else if (n_final < 30) {
                # WARNING: Small sample
                recommendations <- c(recommendations, paste0("<em>Small final sample size (N = ", n_final, ").</em> Consider reporting exact values rather than summary statistics. Confidence intervals may be wide."))
            }

            # Check missing data from ORIGINAL dataset
            missing_pct_original <- round(100 * (1 - sum(complete.cases(original_data)) / n_original), 1)
            if (missing_pct_original > 50) {
                # STRONG_WARNING: High missing data
                warnings <- c(warnings, paste0("<strong>⚠️ High missing data rate in original dataset (", missing_pct_original, "%).</strong> More than half of cases have at least one missing value. Results may not be representative of the full population. Consider data cleaning, imputation, or reporting missing data patterns."))
            } else if (missing_pct_original > 20) {
                # WARNING: Moderate missing data
                recommendations <- c(recommendations, paste0("<em>Moderate missing data in original dataset (", missing_pct_original, "%).</em> Consider reporting missing data patterns or using multiple imputation. Compare characteristics of complete vs. incomplete cases."))
            }

            # Warn if large proportion excluded
            if (n_original > n_final) {
                excluded_pct <- round(100 * (n_original - n_final) / n_original, 1)
                if (excluded_pct > 30) {
                    # STRONG_WARNING: Large exclusion
                    warnings <- c(warnings, paste0("<strong>⚠️ Large case loss due to missing data (", excluded_pct, "% excluded).</strong> Excluded: ", n_original - n_final, " cases | Retained: ", n_final, " cases. Results may not be representative of the full sample. Consider multiple imputation or sensitivity analyses."))
                } else if (excluded_pct > 10) {
                    # WARNING: Notable exclusion
                    recommendations <- c(recommendations, paste0("<em>Notable case loss (", excluded_pct, "% excluded).</em> Excluded: ", n_original - n_final, " cases | Retained: ", n_final, " cases. Compare characteristics of excluded vs. included cases to assess potential bias."))
                }
            }

            # Check variable types and unusual patterns
            for (var in vars) {
                if (var %in% names(data)) {
                    var_data <- data[[var]]
                    n_unique <- length(unique(var_data[!is.na(var_data)]))
                    n_valid <- sum(!is.na(var_data))

                    if (is.numeric(var_data) && n_unique < 5 && n_valid > 10) {
                        # INFO: Variable type recommendation
                        recommendations <- c(recommendations, sprintf("<em>Variable '%s' has few unique values (%d).</em> Consider treating as categorical.", htmltools::htmlEscape(var), n_unique))
                    }

                    if (is.character(var_data) && n_unique > n_valid * 0.8) {
                        # INFO: Variable type recommendation
                        recommendations <- c(recommendations, sprintf("<em>Variable '%s' has many unique text values.</em> Consider grouping categories.", htmltools::htmlEscape(var)))
                    }
                }
            }

            # Build and set assumptions HTML output using helper
            if (length(warnings) > 0 || length(recommendations) > 0) {
                assumptions_html <- private$.buildDataQualityHtml(warnings, recommendations)
                self$results$assumptions$setContent(assumptions_html)
            } else {
                # INFO: All good - data quality check passed
                if (n_final >= 30 && missing_pct_original <= 20 && n_original == n_final) {
                    ok_html <- private$.buildDataQualityOkHtml(n_final, missing_pct_original)
                    self$results$assumptions$setContent(ok_html)
                } else {
                    self$results$assumptions$setContent("")
                }
            }
        },

        .setReportSentence = function(data, vars, original_data, excluded_n) {
            n_final <- nrow(data)
            n_original <- nrow(original_data)
            n_vars <- length(vars)

            missing_pct <- round(100 * (1 - sum(complete.cases(original_data)) / n_original), 1)

            # Build variable list description
            var_list <- if (n_vars <= 3) {
                paste(vars, collapse = ", ")
            } else {
                paste0(paste(head(vars, 3), collapse = ", "), ", and ", n_vars - 3, " other variable", if (n_vars - 3 > 1) "s" else "")
            }

            # Build missing data clause
            missing_clause <- if (missing_pct == 0) {
                "Complete data were available for all cases."
            } else if (missing_pct < 5) {
                sprintf("Minimal missing data were detected (%.1f%% of cases with at least one missing value).", missing_pct)
            } else if (missing_pct < 20) {
                sprintf("Moderate missing data were observed (%.1f%% of cases incomplete).", missing_pct)
            } else {
                sprintf("Substantial missing data were present (%.1f%% of cases with at least one missing value).", missing_pct)
            }

            # Build exclusion clause
            exclusion_clause <- if (excluded_n > 0) {
                excluded_pct <- round(100 * excluded_n / n_original, 1)
                sprintf(" After listwise deletion, %d cases (%.1f%%) were analyzed.", n_final, 100 - excluded_pct)
            } else {
                ""
            }

            # Construct final sentence
            report_text <- sprintf(
                "Table One summarizes baseline characteristics of %d %s. Variables included %s. %s%s",
                n_original,
                if (n_original == 1) "patient" else "patients",
                var_list,
                missing_clause,
                exclusion_clause
            )

            # Format with copy button styling
            html_output <- paste0(
                "<div style='background-color: #f0f8ff; border: 2px solid #4682b4; border-radius: 5px; padding: 15px; margin: 10px 0;'>",
                "<h4 style='margin-top: 0; color: #2c5aa0;'>📋 Copy-Ready Report Sentence</h4>",
                "<p style='font-family: Georgia, serif; font-size: 14px; line-height: 1.6; color: #333;'>",
                htmltools::htmlEscape(report_text),
                "</p>",
                "<p style='margin-bottom: 0; font-size: 12px; color: #666;'>",
                "<em>Select and copy the text above for your manuscript. Edit as needed for your specific reporting requirements.</em>",
                "</p>",
                "</div>"
            )

            self$results$reportSentence$setContent(html_output)
        }
  ) # End of private list.
) # End of R6Class definition.
