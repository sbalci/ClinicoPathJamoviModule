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
#'
#' @export tableoneClass
#'
tableoneClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .run = function() {
            # Check that the input data has at least one complete row.
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$todo$setContent("No data available. Please load a dataset first.")
                private$.setAboutContent()
                return(invisible(NULL))
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

            # Optionally exclude rows with missing values.
            if (isTRUE(self$options$excl)) {
                data <- jmvcore::naOmit(data)
            }

            # Retrieve the table style selected by the user.
            table_style <- self$options$sty

            # Generate clinical summaries and data quality checks
            private$.generateSummary(data, selected_vars)
            private$.checkDataQuality(data, selected_vars)

            # Generate the table based on the chosen style.
            if (table_style == "t1") {
                # --- Using tableone package ---
                # Checkpoint before expensive statistical computation
                private$.checkpoint()
                
                mytable <- tryCatch({
                    tableone::CreateTableOne(data = data)
                }, error = function(e) {
                    if (grepl("insufficient", tolower(e$message))) {
                        stop("Insufficient data for analysis. Ensure you have at least 2 complete cases and check for missing values. Try selecting different variables or checking data quality.")
                    } else {
                        stop("Error creating Table One: ", e$message, ". Try selecting different variables or checking your data format.")
                    }
                })
                self$results$tablestyle1$setContent(mytable)

            } else if (table_style == "t2") {
                # --- Using gtsummary package ---
                # Checkpoint before expensive gtsummary computation
                private$.checkpoint()
                
                mytable <- tryCatch({
                    tbl <- gtsummary::tbl_summary(data = data)
                    gtsummary::as_kable_extra(tbl)
                }, error = function(e) {
                    stop("Error creating gtsummary table: ", e$message, ". Check that your variables have valid data and appropriate types for statistical summarization.")
                })
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
                self$results$tablestyle3$setContent(mytable)

            } else if (table_style == "t4") {
                # --- Using janitor package for frequency tables with improved spacing & styling ---
                # Checkpoint before starting the variable loop
                private$.checkpoint()
                
                table_list <- lapply(selected_vars, function(var) {
                    # Checkpoint for each variable processing (for incremental results)
                    private$.checkpoint(flush = FALSE)
                    
                    freq_table <- tryCatch({
                        # Check if variable exists and has data
                        if (!var %in% names(data)) {
                            stop("Variable '", var, "' not found in data")
                        }
                        
                        # Remove missing values for this variable to avoid issues
                        var_data <- data[!is.na(data[[var]]), ]
                        
                        if (nrow(var_data) == 0) {
                            stop("Variable '", var, "' has no non-missing values")
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
                        stop("Error processing variable '", var, "' with janitor: ", e$message, 
                             " (Variable type: ", class(data[[var]])[1], 
                             ", Non-missing values: ", sum(!is.na(data[[var]])), ")")
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
            } else {
                stop("Error: Invalid table style selected. Please choose a valid style.")
            }
        }, # End of .run function.
        
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
        
        .generateSummary = function(data, vars) {
            n_total <- nrow(data)
            n_vars <- length(vars)
            n_complete <- sum(complete.cases(data))
            missing_pct <- round(100 * (1 - n_complete / n_total), 1)
            
            # Variable type analysis
            var_types <- sapply(data, function(x) {
                if (is.numeric(x)) "Numeric" 
                else if (is.factor(x)) "Categorical"
                else if (is.logical(x)) "Logical"
                else "Other"
            })
            type_summary <- table(var_types)
            type_text <- paste(names(type_summary), ":", type_summary, collapse = "; ")
            
            summary_text <- paste0(
                "<div style='background-color: #e8f4fd; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>",
                "<h4>Analysis Summary</h4>",
                "<p><strong>Dataset:</strong> ", n_total, " cases with ", n_vars, " selected variables</p>",
                "<p><strong>Complete cases:</strong> ", n_complete, " (", round(100 * n_complete / n_total, 1), "%)</p>",
                if (missing_pct > 0) paste0("<p><strong>Missing data:</strong> ", missing_pct, "% - consider checking data quality</p>") else "",
                "<p><strong>Variable types:</strong> ", type_text, "</p>",
                "<p><em>This Table One summarizes baseline characteristics commonly reported in clinical research manuscripts.</em></p>",
                "</div>"
            )
            self$results$summary$setContent(summary_text)
        },
        
        .checkDataQuality = function(data, vars) {
            warnings <- c()
            recommendations <- c()
            
            # Check sample size
            n_total <- nrow(data)
            if (n_total < 10) {
                warnings <- c(warnings, "Very small sample size (N < 10). Results may be unreliable.")
            } else if (n_total < 30) {
                recommendations <- c(recommendations, "Small sample size (N < 30). Consider reporting exact values rather than summary statistics.")
            }
            
            # Check missing data
            missing_pct <- round(100 * (1 - sum(complete.cases(data)) / n_total), 1)
            if (missing_pct > 50) {
                warnings <- c(warnings, paste0("High missing data rate (", missing_pct, "%). Consider data cleaning or imputation."))
            } else if (missing_pct > 20) {
                recommendations <- c(recommendations, paste0("Moderate missing data (", missing_pct, "%). Consider reporting missing data patterns."))
            }
            
            # Check variable types and unusual patterns
            for (var in vars) {
                if (var %in% names(data)) {
                    var_data <- data[[var]]
                    n_unique <- length(unique(var_data[!is.na(var_data)]))
                    n_valid <- sum(!is.na(var_data))
                    
                    if (is.numeric(var_data) && n_unique < 5 && n_valid > 10) {
                        recommendations <- c(recommendations, paste0("Variable '", var, "' has few unique values (", n_unique, "). Consider treating as categorical."))
                    }
                    
                    if (is.character(var_data) && n_unique > n_valid * 0.8) {
                        recommendations <- c(recommendations, paste0("Variable '", var, "' has many unique text values. Consider grouping categories."))
                    }
                }
            }
            
            # Generate output
            if (length(warnings) > 0 || length(recommendations) > 0) {
                quality_text <- "<div style='margin: 10px 0;'>"
                
                if (length(warnings) > 0) {
                    quality_text <- paste0(quality_text, 
                        "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; border-radius: 4px; margin-bottom: 10px;'>",
                        "<h5 style='color: #856404; margin-top: 0;'>⚠️ Data Quality Warnings</h5>",
                        "<ul style='margin-bottom: 0; color: #856404;'>",
                        paste0("<li>", warnings, "</li>", collapse = ""),
                        "</ul></div>"
                    )
                }
                
                if (length(recommendations) > 0) {
                    quality_text <- paste0(quality_text,
                        "<div style='background-color: #d1ecf1; border: 1px solid #bee5eb; padding: 10px; border-radius: 4px;'>",
                        "<h5 style='color: #0c5460; margin-top: 0;'>💡 Recommendations</h5>",
                        "<ul style='margin-bottom: 0; color: #0c5460;'>",
                        paste0("<li>", recommendations, "</li>", collapse = ""),
                        "</ul></div>"
                    )
                }
                
                quality_text <- paste0(quality_text, "</div>")
                self$results$assumptions$setContent(quality_text)
            } else {
                self$results$assumptions$setContent(
                    "<div style='background-color: #d4edda; border: 1px solid #c3e6cb; padding: 10px; border-radius: 4px; margin: 10px 0;'>
                     <h5 style='color: #155724; margin-top: 0;'>✅ Data Quality Check</h5>
                     <p style='margin-bottom: 0; color: #155724;'>Data quality looks good. No major issues detected.</p>
                     </div>"
                )
            }
        }
    ) # End of private list.
) # End of R6Class definition.
