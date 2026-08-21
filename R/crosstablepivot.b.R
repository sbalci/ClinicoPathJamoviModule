#' @title Enhanced Cross Tables with Pivot Functionality
#' @description Creates professional cross tables using pivottabler library
#' for flexible clinical data analysis. Provides advanced formatting and statistics
#' while maintaining compatibility with existing crosstable functionality.
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export
#' @return An \code{R6} class generator object for the \code{crosstablepivotClass} backend; used internally by the jamovi analysis wrapper and not called directly.
# The unfinished pivot-table path below uses qualified calls, so it does not require
# package-wide namespace imports.

crosstablepivotClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "crosstablepivotClass",
    inherit = crosstablepivotBase,
    private = list(
        .init = function() {
            # Initialize instructions
            instructions_html <- paste(
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                "<h4 style='color: #495057; margin-top: 0;'>Enhanced Cross Tables with Pivot Functionality:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px;'>",
                "<li><strong>Select Variables (Rows):</strong> Choose variables to display in table rows</li>",
                "<li><strong>Select Grouping Variable (Columns):</strong> Choose variable for table columns</li>",
                "<li><strong>Format Options:</strong> Clinical, Publication, or Standard formatting</li>",
                "<li><strong>Statistics:</strong> Automatic counts, percentages, and totals</li>",
                "<li><strong>Excel Export:</strong> Direct export capability for publications</li>",
                "</ul>",
                "<p style='margin: 5px 0; color: #6c757d;'><em> Tip: This module uses pivottabler for enhanced flexibility and professional formatting</em></p>",
                "</div>"
            )
            
            self$results$instructions$setContent(instructions_html)
        },
        
        .run = function() {
            # Check if required variables are selected
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                self$results$instructions$setContent(
                    "<p>Please select at least one variable for table rows.</p>"
                )
                return()
            }
            
            if (is.null(self$options$group)) {
                self$results$instructions$setContent(
                    "<p>Please select a grouping variable for table columns.</p>"
                )
                return()
            }
            
            # Get data
            data <- self$data
            if (nrow(data) == 0) {
                return()
            }
            
            # TODO (stub): pivottabler integration is unfinished. populate_basic_table() is a
            #   placeholder ("Status: Basic implementation - pivot functionality to be added");
            #   populate_pivot_table just delegates to populate_basic_table. Wire up real pivot logic.
            # NOTE (unused options): `show_totals` is never read anywhere in this backend, and
            #   `format_style` is read only inside get_table_styling(), which is currently never
            #   called. Wire both into the real pivot logic when it lands.
            # Simplified implementation for testing - will be enhanced later
            tryCatch({
                # Basic table creation without pivottabler for now
                private$populate_basic_table()

                # Generate summary statistics
                if (self$options$statistics) {
                    private$generate_summary_stats()
                }

                # Export information
                if (self$options$export_excel) {
                    private$show_export_info()
                }

            }, error = function(e) {
                self$results$instructions$setContent(
                    paste("<p style='color: red;'>Error creating table:",
                          htmltools::htmlEscape(e$message), "</p>")
                )
            })
        },
        
        get_table_styling = function() {
            # Return styling based on format_style option
            style <- switch(self$options$format_style,
                "clinical" = list(
                    fontName = "Arial",
                    fontSize = 12,
                    headerBackgroundColor = "#f8f9fa",
                    headerFontColor = "#495057"
                ),
                "publication" = list(
                    fontName = "Times New Roman", 
                    fontSize = 11,
                    headerBackgroundColor = "#ffffff",
                    headerFontColor = "#000000"
                ),
                "standard" = list(
                    fontName = "Arial",
                    fontSize = 10,
                    headerBackgroundColor = "#e9ecef",
                    headerFontColor = "#343a40"
                )
            )
            return(style)
        },
        
        populate_basic_table = function() {
            # Basic table implementation to test the infrastructure
            pivot_table <- self$results$pivot_table
            
            # Add sample data showing variables and group
            vars_text <- paste(self$options$vars, collapse = ", ")
            group_text <- self$options$group
            
            # Clear any existing rows
            pivot_table$deleteRows()
            
            # Add basic summary information
            pivot_table$addRow(rowKey = 1, values = list(
                variable = "Variables (Rows)",
                value = vars_text
            ))
            
            pivot_table$addRow(rowKey = 2, values = list(
                variable = "Grouping Variable (Columns)",
                value = group_text
            ))
            
            pivot_table$addRow(rowKey = 3, values = list(
                variable = "Data Rows",
                value = as.character(nrow(self$data))
            ))
            
            pivot_table$addRow(rowKey = 4, values = list(
                variable = "Status",
                value = "Basic implementation - pivot functionality to be added"
            ))
        },
        
        populate_pivot_table = function(pt) {
            # Future implementation for full pivottabler integration
            # Currently using populate_basic_table for testing
            private$populate_basic_table()
        },
        
        generate_summary_stats = function() {
            summary_table <- self$results$summary_stats

            # Clear any existing rows (table declares no fixed `rows:`, so use addRow)
            summary_table$deleteRows()

            # Add summary statistics
            summary_table$addRow(rowKey = 1, values = list(
                variable = "Total Observations",
                statistic = "Count",
                value = nrow(self$data)
            ))

            summary_table$addRow(rowKey = 2, values = list(
                variable = "Variables Analyzed",
                statistic = "Count",
                value = length(self$options$vars)
            ))
        },
        
        show_export_info = function() {
            export_html <- paste(
                "<div style='background-color: rgba(33, 162, 64, 0.19); padding: 10px; border-radius: 5px; color: inherit;'>",
                "<h5 style='color: #155724; margin-top: 0;'>Excel Export Ready:</h5>",
                "<p style='margin: 5px 0;'>Your pivot table can be exported to Excel format.</p>",
                "<p style='margin: 5px 0; font-size: 12px;'>Note: Excel export functionality requires additional implementation.</p>",
                "</div>"
            )
            
            self$results$export_info$setContent(export_html)
        }
    )
)
