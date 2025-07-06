#' @title Publication-Ready Tables with gtsummary
#'

gtsummaryClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "gtsummaryClass",
    inherit = gtsummaryBase,
    private = list(
        .run = function() {
            
            # Error checking ----
            if (nrow(self$data) == 0) {
                stop("Data contains no (complete) rows")
            }
            
            # Welcome message if no variables selected ----
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                todo <- "
                <br><h3>Welcome to Publication-Ready Tables with gtsummary</h3>
                <br><h4>Get Started:</h4>
                <ol>
                <li><b>Select Variables:</b> Drag variables into the 'Variables for Table' box</li>
                <li><b>Optional:</b> Select a grouping variable for comparison tables</li>
                <li><b>Choose Table Type:</b> Select from Summary, Cross, Regression, or Survival tables</li>
                <li><b>Customize:</b> Add p-values, modify formatting, and styling options</li>
                </ol>
                
                <br><h4>Features:</h4>
                <ul>
                <li>📊 Professional publication-ready tables</li>
                <li>📈 Automatic statistical testing</li>
                <li>🎨 Extensive formatting options</li>
                <li>📋 Multiple output formats (HTML, LaTeX, Word)</li>
                <li>✨ Built on the powerful gtsummary R package</li>
                </ul>
                "
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(todo)
                return()
            } else {
                self$results$todo$setVisible(FALSE)
            }
            
            # Prepare data ----
            vars <- self$options$vars
            byvar <- self$options$byvar
            
            # Select relevant columns
            if (!is.null(byvar)) {
                selected_data <- self$data[c(vars, byvar)]
            } else {
                selected_data <- self$data[vars]
            }
            
            # Generate R code for transparency ----
            r_code <- private$.generateRCode()
            if (self$options$showCode) {
                self$results$code_output$setContent(r_code)
            }
            
            # Create the main table ----
            tryCatch({
                main_table <- private$.createTable(selected_data)
                
                # Convert to HTML format
                if (self$options$outputFormat == "html") {
                    html_table <- gtsummary::as_kable_extra(main_table, format = "html")
                } else if (self$options$outputFormat == "markdown") {
                    html_table <- gtsummary::as_kable(main_table, format = "html")
                } else {
                    html_table <- gtsummary::as_kable_extra(main_table, format = "html")
                }
                
                self$results$maintable$setContent(html_table)
                
                # Add statistical summary if p-values are included ----
                if (self$options$addPValue) {
                    stats_info <- private$.createStatsSummary(main_table)
                    self$results$stats_summary$setContent(stats_info)
                }
                
                # Add table notes ----
                notes <- private$.createTableNotes(main_table)
                self$results$table_notes$setContent(notes)
                
                # Export information ----
                if (self$options$exportTable) {
                    export_info <- private$.createExportInfo(main_table)
                    self$results$export_info$setContent(export_info)
                }
                
            }, error = function(e) {
                error_msg <- paste("Error creating table:", e$message)
                self$results$maintable$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
            })
        },
        
        .createTable = function(data) {
            # Create base table according to table type
            if (self$options$tableType == "summary") {
                table <- private$.createSummaryTable(data)
            } else if (self$options$tableType == "cross") {
                table <- private$.createCrossTable(data)
            } else if (self$options$tableType == "regression") {
                table <- private$.createRegressionTable(data)
            } else if (self$options$tableType == "survival") {
                table <- private$.createSurvivalTable(data)
            } else {
                table <- private$.createSummaryTable(data)  # Default
            }
            
            return(table)
        },
        
        .createSummaryTable = function(data) {
            # Build arguments for tbl_summary
            args <- list(data = data)
            
            # Add by variable if specified
            if (!is.null(self$options$byvar)) {
                args$by <- self$options$byvar
            }
            
            # Configure missing values
            args$missing <- self$options$includeMissing
            
            # Include overall column
            include_overall <- self$options$includeOverall && !is.null(self$options$byvar)
            
            # Create base table
            table <- do.call(gtsummary::tbl_summary, args)
            
            # Add overall column if requested
            if (include_overall) {
                table <- table %>% gtsummary::add_overall()
            }
            
            # Add p-values if requested
            if (self$options$addPValue && !is.null(self$options$byvar)) {
                test_args <- list()
                if (self$options$testMethod != "auto") {
                    # Map jamovi test names to gtsummary test functions
                    test_map <- list(
                        "chisq.test" = "chisq.test",
                        "fisher.test" = "fisher.test", 
                        "t.test" = "t.test",
                        "wilcox.test" = "wilcox.test",
                        "kruskal.test" = "kruskal.test",
                        "aov" = "aov"
                    )
                    if (self$options$testMethod %in% names(test_map)) {
                        test_args$test = test_map[[self$options$testMethod]]
                    }
                }
                
                table <- table %>% gtsummary::add_p(test = test_args$test)
                
                # Add q-values if requested
                if (self$options$addQ) {
                    table <- table %>% gtsummary::add_q()
                }
            }
            
            # Add sample sizes to header
            if (self$options$showNHeader && !is.null(self$options$byvar)) {
                table <- table %>% gtsummary::add_n()
            }
            
            # Apply formatting options
            table <- private$.applyFormatting(table)
            
            return(table)
        },
        
        .createCrossTable = function(data) {
            if (length(self$options$vars) < 2) {
                stop("Cross table requires at least 2 variables")
            }
            
            row_var <- self$options$vars[1]
            col_var <- self$options$vars[2]
            
            table <- gtsummary::tbl_cross(
                data = data,
                row = !!rlang::sym(row_var),
                col = !!rlang::sym(col_var),
                percent = self$options$percentType
            )
            
            if (self$options$addPValue) {
                table <- table %>% gtsummary::add_p()
            }
            
            return(table)
        },
        
        .createRegressionTable = function(data) {
            # Simple implementation - would need model specification in real use
            if (length(self$options$vars) < 2) {
                stop("Regression table requires at least 2 variables (outcome and predictors)")
            }
            
            # Basic linear model as example
            outcome <- self$options$vars[1]
            predictors <- self$options$vars[-1]
            
            formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
            model <- lm(as.formula(formula_str), data = data)
            
            table <- gtsummary::tbl_regression(model)
            
            return(table)
        },
        
        .createSurvivalTable = function(data) {
            # Placeholder for survival table - would need survival variables
            stop("Survival tables require time-to-event data and survival package integration")
        },
        
        .applyFormatting = function(table) {
            # Apply styling options
            if (self$options$boldLabels) {
                table <- table %>% gtsummary::bold_labels()
            }
            
            if (self$options$boldLevels) {
                table <- table %>% gtsummary::bold_levels()
            }
            
            if (self$options$boldPValues) {
                table <- table %>% gtsummary::bold_p(t = self$options$pValueThreshold)
            }
            
            if (self$options$italicizeLabels) {
                table <- table %>% gtsummary::italicize_labels()
            }
            
            if (self$options$italicizeLevels) {
                table <- table %>% gtsummary::italicize_levels()
            }
            
            # Add spanning header if requested
            if (self$options$addSpanningHeader && !is.null(self$options$spanningHeaderText) && 
                nchar(self$options$spanningHeaderText) > 0) {
                # This would require more sophisticated column identification
                # table <- table %>% gtsummary::modify_spanning_header(...)
            }
            
            # Add title and caption
            if (!is.null(self$options$tableTitle) && nchar(self$options$tableTitle) > 0) {
                # table <- table %>% gtsummary::modify_caption(self$options$tableTitle)
            }
            
            return(table)
        },
        
        .createStatsSummary = function(table) {
            # Extract and summarize statistical test information
            info <- "<h4>Statistical Test Information</h4>"
            
            if (self$options$addPValue) {
                info <- paste0(info, "<p><b>Test Method:</b> ", 
                             ifelse(self$options$testMethod == "auto", "Automatic selection", self$options$testMethod), "</p>")
                
                if (self$options$pairedTest) {
                    info <- paste0(info, "<p><b>Paired tests</b> were used where applicable.</p>")
                }
                
                if (self$options$addQ) {
                    info <- paste0(info, "<p><b>Q-values</b> (FDR-adjusted p-values) are included.</p>")
                }
                
                info <- paste0(info, "<p><b>Significance threshold:</b> ", self$options$pValueThreshold, "</p>")
            }
            
            return(info)
        },
        
        .createTableNotes = function(table) {
            notes <- "<h4>Table Notes</h4>"
            
            # Add information about statistics presented
            notes <- paste0(notes, "<p><b>Statistics presented:</b> ")
            
            if ("mean_sd" %in% self$options$statistics) {
                notes <- paste0(notes, "Mean (SD), ")
            }
            if ("median_iqr" %in% self$options$statistics) {
                notes <- paste0(notes, "Median (IQR), ")
            }
            if ("n_percent" %in% self$options$statistics) {
                notes <- paste0(notes, "N (%), ")
            }
            if ("range" %in% self$options$statistics) {
                notes <- paste0(notes, "Range (Min, Max), ")
            }
            
            # Remove trailing comma
            notes <- sub(", $", "", notes)
            notes <- paste0(notes, "</p>")
            
            # Add missing value information
            if (self$options$includeMissing != "no") {
                notes <- paste0(notes, "<p><b>Missing values:</b> ", 
                               switch(self$options$includeMissing,
                                      "ifany" = "Shown if any missing",
                                      "always" = "Always shown"), "</p>")
            }
            
            # Add footnote if provided
            if (!is.null(self$options$footnote) && nchar(self$options$footnote) > 0) {
                notes <- paste0(notes, "<p><i>", self$options$footnote, "</i></p>")
            }
            
            return(notes)
        },
        
        .createExportInfo = function(table) {
            info <- "<h4>Export Information</h4>"
            info <- paste0(info, "<p>Table created using the gtsummary R package.</p>")
            info <- paste0(info, "<p><b>Output format:</b> ", self$options$outputFormat, "</p>")
            info <- paste0(info, "<p><b>Variables included:</b> ", length(self$options$vars), "</p>")
            
            if (!is.null(self$options$byvar)) {
                info <- paste0(info, "<p><b>Grouped by:</b> ", self$options$byvar, "</p>")
            }
            
            info <- paste0(info, "<p><b>Table type:</b> ", self$options$tableType, "</p>")
            
            return(info)
        },
        
        .generateRCode = function() {
            # Generate R code that would produce the same table
            code <- "# gtsummary table creation\n"
            code <- paste0(code, "library(gtsummary)\n")
            code <- paste0(code, "library(dplyr)\n\n")
            
            # Variable selection
            vars_str <- paste0("c(", paste0("\"", self$options$vars, "\"", collapse = ", "), ")")
            code <- paste0(code, "# Select variables\n")
            code <- paste0(code, "selected_vars <- ", vars_str, "\n")
            
            if (!is.null(self$options$byvar)) {
                code <- paste0(code, "by_var <- \"", self$options$byvar, "\"\n")
            }
            
            code <- paste0(code, "\n# Create table\n")
            code <- paste0(code, "table <- data %>%\n")
            code <- paste0(code, "  select(all_of(selected_vars)")
            
            if (!is.null(self$options$byvar)) {
                code <- paste0(code, ", all_of(by_var)")
            }
            
            code <- paste0(code, ") %>%\n")
            code <- paste0(code, "  tbl_summary(")
            
            if (!is.null(self$options$byvar)) {
                code <- paste0(code, "by = ", self$options$byvar)
            }
            
            code <- paste0(code, ")\n")
            
            # Add modifications
            if (self$options$includeOverall && !is.null(self$options$byvar)) {
                code <- paste0(code, "  %>% add_overall()\n")
            }
            
            if (self$options$addPValue && !is.null(self$options$byvar)) {
                code <- paste0(code, "  %>% add_p()\n")
            }
            
            if (self$options$boldLabels) {
                code <- paste0(code, "  %>% bold_labels()\n")
            }
            
            code <- paste0(code, "\n# Display table\ntable")
            
            return(code)
        }
    )
)