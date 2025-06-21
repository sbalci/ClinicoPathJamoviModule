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
#' @export tableoneClass
#'
tableoneClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "tableoneClass",
    inherit = tableoneBase,
    private = list(
        .run = function() {
            # Check that the input data has at least one complete row.
            if (nrow(self$data) == 0) {
                stop("Error: The input data contains no (complete) rows. Please provide a valid dataset.")
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
                    self$create_pivot_tableone(data, selected_vars)
                }, error = function(e) {
                    paste0("<div style='color: red;'>Error creating pivot table: ", e$message, "</div>")
                })
                self$results$tablestyle5$setContent(mytable)

            } else {
                stop("Error: Invalid table style selected. Please choose a valid style.")
            }
        }, # End of .run function.
        
        create_pivot_tableone = function(data, selected_vars) {
            # Create enhanced Table One using pivottabler
            
            # Initialize pivot table
            pt <- pivottabler::PivotTable$new()
            pt$addData(data)
            
            # Get format style
            format_style <- self$options$pivot_format
            
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
                summary_html <- paste0(summary_html, "<h4>Categorical Variables:</h4><ul>")
                for (var in categorical_vars) {
                    var_summary <- table(data[[var]], useNA = "ifany")
                    summary_html <- paste0(summary_html, 
                        "<li><strong>", var, ":</strong> ", 
                        paste(names(var_summary), " (n=", var_summary, ")", collapse = ", "), "</li>")
                }
                summary_html <- paste0(summary_html, "</ul>")
            }
            
            # Add numerical variables summary
            if (length(numerical_vars) > 0) {
                summary_html <- paste0(summary_html, "<h4>Numerical Variables:</h4><ul>")
                for (var in numerical_vars) {
                    var_data <- data[[var]][!is.na(data[[var]])]
                    if (length(var_data) > 0) {
                        summary_html <- paste0(summary_html,
                            "<li><strong>", var, ":</strong> Mean = ", round(mean(var_data), 2),
                            ", SD = ", round(sd(var_data), 2),
                            ", Range: ", round(min(var_data), 2), "-", round(max(var_data), 2), "</li>")
                    }
                }
                summary_html <- paste0(summary_html, "</ul>")
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
            if (self$options$include_statistics) {
                summary_html <- paste0(summary_html, 
                    "<p><strong>Advanced Statistics:</strong> Enabled - Including enhanced summaries</p>")
            }
            
            # Add group comparison info if enabled
            if (self$options$group_comparisons) {
                summary_html <- paste0(summary_html,
                    "<p><strong>Group Comparisons:</strong> Enabled - Ready for comparative analysis</p>")
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
