#' @title Modern Table Formatting with TinyTable
#' @return Modern, publication-ready tables using tinytable package
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom tinytable tt style_tt group_tt format_tt theme_tt
#' @importFrom dplyr select group_by summarise n across where all_of
#' @importFrom dplyr mutate case_when
#' @importFrom stats sd median quantile
#' @importFrom htmltools HTML
#' @importFrom stringr str_to_title
#' @importFrom rlang .data

tinytableClass <- if (requireNamespace("jmvcore")) R6::R6Class("tinytableClass",
    inherit = tinytableBase,
    private = list(

        .run = function() {

            # Check if required variables have been selected
            if (is.null(self$options$vars) || length(self$options$vars) == 0) {
                intro_msg <- "
                <div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 20px 0;'>
                <h3 style='color: #1976d2; margin-top: 0;'>📋 Welcome to Modern Table Formatting!</h3>
                <p><strong>Publication-ready tables with tinytable</strong> - zero dependencies, maximum flexibility</p>
                <p>Create beautiful, modern tables for clinical research and data presentation</p>
                
                <h4 style='color: #1976d2;'>Required Selection:</h4>
                <ol>
                <li><strong>Variables to Display:</strong> Choose the variables you want to include in your table</li>
                </ol>
                
                <h4 style='color: #1976d2;'>What Modern Table Formatting Offers:</h4>
                <ul>
                <li><strong>Zero Dependencies:</strong> Lightweight, fast table generation</li>
                <li><strong>Multiple Formats:</strong> HTML, PDF, Word, LaTeX, Markdown output</li>
                <li><strong>Modern Styling:</strong> Clean, contemporary table aesthetics</li>
                <li><strong>Flexible Layouts:</strong> Descriptive statistics, grouped summaries, raw data</li>
                <li><strong>Publication Ready:</strong> Professional formatting for journals</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Perfect For:</h4>
                <ul>
                <li><strong>Clinical Research:</strong> Professional data presentation for publications</li>
                <li><strong>Statistical Reports:</strong> Clean summary tables with modern styling</li>
                <li><strong>Data Exploration:</strong> Quick, beautiful table generation for analysis</li>
                <li><strong>Presentations:</strong> Publication-quality tables for slides and reports</li>
                <li><strong>Multi-format Output:</strong> Tables that work across different document types</li>
                </ul>
                
                <h4 style='color: #1976d2;'>Table Types Available:</h4>
                <ul>
                <li><strong>Data Summary:</strong> Quick overview of your selected variables</li>
                <li><strong>Descriptive Statistics:</strong> Detailed statistical summaries</li>
                <li><strong>Grouped Summary:</strong> Statistics organized by grouping variable</li>
                <li><strong>Raw Data Display:</strong> Clean presentation of your data</li>
                <li><strong>Custom Format:</strong> Flexible formatting for specific needs</li>
                </ul>
                
                <p style='font-size: 12px; color: #555; margin-top: 20px;'>
                💡 <em>Modern alternative to traditional table packages - lightweight, flexible, and beautiful</em>
                </p>
                </div>"
                
                self$results$todo$setContent(intro_msg)
                return()
            } else {
                self$results$todo$setContent("")
            }

            # Validate dataset
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
            }

            # Enhanced package checking with better error messages
            missing_packages <- character(0)
            
            if (!requireNamespace("tinytable", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "tinytable")
            }
            if (!requireNamespace("stringr", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "stringr")
            }
            if (!requireNamespace("rlang", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "rlang")
            }
            
            if (length(missing_packages) > 0) {
                pkg_list <- paste(missing_packages, collapse = ", ")
                error_msg <- paste0("
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>Missing Required Packages</h4>
                <p>The following packages are required for modern table formatting: <strong>", pkg_list, "</strong></p>
                <p>Please install them using:</p>
                <pre><code>install.packages(c('", paste(missing_packages, collapse = "', '"), "'))</code></pre>
                <p>For tinytable, you can also use R-universe:</p>
                <pre><code>install.packages('tinytable', repos = c('https://vincentarelbundock.r-universe.dev', 'https://cloud.r-project.org'))</code></pre>
                </div>")
                self$results$interpretation$setContent(error_msg)
                return()
            }

            # Get data and variables
            dataset <- self$data
            vars <- self$options$vars
            group_var <- self$options$group_var
            table_type <- self$options$table_type

            # Prepare analysis data
            if (!is.null(group_var) && group_var != "") {
                analysis_data <- dataset[c(vars, group_var)]
            } else {
                analysis_data <- dataset[vars]
            }
            
            # Remove rows with all missing values for selected variables
            analysis_data <- analysis_data[rowSums(!is.na(analysis_data[vars])) > 0, ]
            
            if (nrow(analysis_data) == 0) {
                stop("Error: No data available for the selected variables.")
            }

            # Generate table based on type with error handling
            tryCatch({
                table_html <- private$.generate_table(analysis_data, vars, group_var, table_type)
                self$results$table$setContent(table_html)
                
                # Generate interpretation guide
                if (self$options$show_interpretation) {
                    interpretation_html <- private$.generate_interpretation_guide(analysis_data, vars, group_var, table_type)
                    self$results$interpretation$setContent(interpretation_html)
                }
            }, error = function(e) {
                error_msg <- paste0("
                <div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>
                <h4>Table Generation Error</h4>
                <p><strong>Error:</strong> ", e$message, "</p>
                
                <h5>Troubleshooting Tips:</h5>
                <ul>
                <li>Ensure selected variables have compatible data types for the chosen table type</li>
                <li>Check that grouping variables are categorical (factor) types</li>
                <li>Verify numeric variables have sufficient non-missing values</li>
                <li>Try a different table type if the current one fails</li>
                </ul>
                
                <h5>Common Solutions:</h5>
                <ul>
                <li><strong>For grouped tables:</strong> Ensure grouping variable is a factor</li>
                <li><strong>For descriptive statistics:</strong> Include at least one numeric variable</li>
                <li><strong>For missing value errors:</strong> Choose variables with more complete data</li>
                </ul>
                </div>")
                self$results$interpretation$setContent(error_msg)
                
                # Set a simple error message for the table
                self$results$table$setContent("<p style='color: red;'>Table generation failed. See interpretation section for details.</p>")
            })

        },

        .generate_table = function(data, vars, group_var, table_type) {
            
            # Create table based on type
            if (table_type == "summary") {
                table_data <- private$.create_summary_table(data, vars, group_var)
            } else if (table_type == "descriptive") {
                table_data <- private$.create_descriptive_table(data, vars, group_var)
            } else if (table_type == "grouped") {
                table_data <- private$.create_grouped_table(data, vars, group_var)
            } else if (table_type == "raw") {
                table_data <- private$.create_raw_table(data, vars, group_var)
            } else {
                table_data <- private$.create_custom_table(data, vars, group_var)
            }
            
            # Convert to tinytable
            tt_obj <- tinytable::tt(table_data, 
                                    caption = self$options$table_title,
                                    notes = if (self$options$table_notes != "") self$options$table_notes else NULL,
                                    width = self$options$column_width)
            
            # Apply theme
            tt_obj <- private$.apply_theme(tt_obj)
            
            # Apply styling
            tt_obj <- private$.apply_styling(tt_obj, table_data)
            
            # Convert to HTML for display
            html_output <- as.character(tt_obj)
            
            return(html_output)
        },

        .create_summary_table = function(data, vars, group_var) {
            
            summary_data <- data.frame(
                Variable = character(0),
                Type = character(0),
                Summary = character(0),
                stringsAsFactors = FALSE
            )
            
            for (var in vars) {
                var_data <- data[[var]]
                var_type <- class(var_data)[1]
                
                if (is.numeric(var_data)) {
                    n_valid <- sum(!is.na(var_data))
                    mean_val <- round(mean(var_data, na.rm = TRUE), self$options$precision_digits)
                    sd_val <- round(sd(var_data, na.rm = TRUE), self$options$precision_digits)
                    
                    summary_text <- paste0("n=", n_valid, ", Mean=", mean_val, " (SD=", sd_val, ")")
                    if (self$options$show_missing && any(is.na(var_data))) {
                        missing_count <- sum(is.na(var_data))
                        summary_text <- paste0(summary_text, ", Missing=", missing_count)
                    }
                    
                } else {
                    var_table <- table(var_data, useNA = if (self$options$show_missing) "ifany" else "no")
                    if (self$options$show_counts) {
                        summary_parts <- paste0(names(var_table), ": ", var_table, 
                                              " (", round(100 * var_table / sum(var_table), 1), "%)")
                        summary_text <- paste(summary_parts, collapse = "; ")
                    } else {
                        summary_text <- paste(names(var_table), collapse = ", ")
                    }
                }
                
                summary_data <- rbind(summary_data, data.frame(
                    Variable = var,
                    Type = var_type,
                    Summary = summary_text,
                    stringsAsFactors = FALSE
                ))
            }
            
            return(summary_data)
        },

        .create_descriptive_table = function(data, vars, group_var) {
            
            if (!is.null(group_var) && group_var != "") {
                # Group-wise descriptive statistics
                numeric_vars <- vars[sapply(vars, function(v) is.numeric(data[[v]]))]
                
                if (length(numeric_vars) > 0) {
                    desc_data <- data %>%
                        dplyr::group_by(.data[[group_var]]) %>%
                        dplyr::summarise(
                            dplyr::across(dplyr::all_of(numeric_vars), list(
                                n = ~ sum(!is.na(.)),
                                mean = ~ round(mean(., na.rm = TRUE), self$options$precision_digits),
                                sd = ~ round(sd(., na.rm = TRUE), self$options$precision_digits),
                                median = ~ round(median(., na.rm = TRUE), self$options$precision_digits)
                            ), .names = "{.col}_{.fn}"),
                            .groups = 'drop'
                        )
                } else {
                    desc_data <- data.frame(Message = "No numeric variables selected for descriptive statistics")
                }
            } else {
                # Overall descriptive statistics
                numeric_vars <- vars[sapply(vars, function(v) is.numeric(data[[v]]))]
                
                if (length(numeric_vars) > 0) {
                    desc_data <- data.frame(
                        Variable = numeric_vars,
                        N = sapply(numeric_vars, function(v) sum(!is.na(data[[v]]))),
                        Mean = sapply(numeric_vars, function(v) round(mean(data[[v]], na.rm = TRUE), self$options$precision_digits)),
                        SD = sapply(numeric_vars, function(v) round(sd(data[[v]], na.rm = TRUE), self$options$precision_digits)),
                        Median = sapply(numeric_vars, function(v) round(median(data[[v]], na.rm = TRUE), self$options$precision_digits)),
                        Min = sapply(numeric_vars, function(v) round(min(data[[v]], na.rm = TRUE), self$options$precision_digits)),
                        Max = sapply(numeric_vars, function(v) round(max(data[[v]], na.rm = TRUE), self$options$precision_digits)),
                        stringsAsFactors = FALSE
                    )
                } else {
                    desc_data <- data.frame(Message = "No numeric variables selected for descriptive statistics")
                }
            }
            
            return(desc_data)
        },

        .create_grouped_table = function(data, vars, group_var) {
            
            if (is.null(group_var) || group_var == "") {
                return(data.frame(Message = "Grouping variable required for grouped summary"))
            }
            
            # Create grouped summary
            grouped_data <- data %>%
                dplyr::group_by(.data[[group_var]]) %>%
                dplyr::summarise(
                    Count = dplyr::n(),
                    dplyr::across(dplyr::where(is.numeric), list(
                        mean = ~ round(mean(., na.rm = TRUE), self$options$precision_digits),
                        sd = ~ round(sd(., na.rm = TRUE), self$options$precision_digits)
                    ), .names = "{.col}_{.fn}"),
                    .groups = 'drop'
                )
            
            return(grouped_data)
        },

        .create_raw_table = function(data, vars, group_var) {
            
            if (!is.null(group_var) && group_var != "") {
                display_vars <- c(group_var, vars)
            } else {
                display_vars <- vars
            }
            
            # Limit to reasonable number of rows for display
            max_rows <- min(100, nrow(data))
            raw_data <- data[1:max_rows, display_vars, drop = FALSE]
            
            # Round numeric columns
            for (var in names(raw_data)) {
                if (is.numeric(raw_data[[var]])) {
                    raw_data[[var]] <- round(raw_data[[var]], self$options$precision_digits)
                }
            }
            
            return(raw_data)
        },

        .create_custom_table = function(data, vars, group_var) {
            
            # Create a flexible custom format
            custom_data <- data[vars]
            
            # Apply custom formatting based on user preferences
            for (var in vars) {
                if (is.numeric(custom_data[[var]])) {
                    custom_data[[var]] <- round(custom_data[[var]], self$options$precision_digits)
                }
            }
            
            return(custom_data)
        },

        .apply_theme = function(tt_obj) {
            
            theme_name <- self$options$table_theme
            
            # Apply theme based on selection
            if (theme_name == "clinical") {
                # Clinical theme with professional styling
                tt_obj <- tt_obj %>%
                    tinytable::style_tt(
                        i = 0,  # Header row
                        background = "#f8f9fa",
                        color = "#495057",
                        bold = TRUE
                    )
            } else if (theme_name == "modern") {
                # Modern theme with contemporary styling
                tt_obj <- tt_obj %>%
                    tinytable::style_tt(
                        i = 0,
                        background = "#6c757d",
                        color = "white",
                        bold = TRUE
                    )
            } else if (theme_name == "publication") {
                # Publication theme for journals
                tt_obj <- tt_obj %>%
                    tinytable::style_tt(
                        i = 0,
                        background = "#e9ecef",
                        color = "#212529",
                        bold = TRUE
                    )
            } else if (theme_name == "minimal") {
                # Minimal theme
                tt_obj <- tt_obj %>%
                    tinytable::style_tt(
                        i = 0,
                        bold = TRUE
                    )
            } else if (theme_name == "bootstrap") {
                # Bootstrap-style theme
                tt_obj <- tt_obj %>%
                    tinytable::style_tt(
                        i = 0,
                        background = "#007bff",
                        color = "white",
                        bold = TRUE
                    )
            }
            
            return(tt_obj)
        },

        .apply_styling = function(tt_obj, table_data) {
            
            # Apply alternating row colors if requested
            if (self$options$style_alternating && nrow(table_data) > 1) {
                even_rows <- seq(2, nrow(table_data), by = 2)
                if (length(even_rows) > 0) {
                    tt_obj <- tt_obj %>%
                        tinytable::style_tt(
                            i = even_rows,
                            background = "#f8f9fa"
                        )
                }
            }
            
            # Apply font size
            font_size <- switch(self$options$font_size,
                "small" = "0.9em",
                "normal" = "1em", 
                "large" = "1.1em"
            )
            
            if (font_size != "1em") {
                tt_obj <- tt_obj %>%
                    tinytable::style_tt(
                        i = 1:nrow(table_data),  # All data rows
                        fontsize = font_size
                    )
            }
            
            # Apply border styling
            border_style <- self$options$style_borders
            if (border_style == "horizontal") {
                tt_obj <- tt_obj %>%
                    tinytable::style_tt(
                        i = 0,  # Header
                        line = "b"  # Bottom border
                    ) %>%
                    tinytable::style_tt(
                        i = nrow(table_data),  # Last row
                        line = "b"  # Bottom border
                    )
            } else if (border_style == "minimal") {
                tt_obj <- tt_obj %>%
                    tinytable::style_tt(
                        i = 0,  # Header only
                        line = "b"
                    )
            } else if (border_style == "none") {
                # No additional borders (tinytable default handles this)
            }
            # "all" borders is the default tinytable behavior
            
            return(tt_obj)
        },

        .generate_interpretation_guide = function(data, vars, group_var, table_type) {
            
            n_vars <- length(vars)
            n_rows <- nrow(data)
            table_theme <- self$options$table_theme
            output_format <- self$options$output_format
            
            interpretation_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>",
                "<h3 style='color: #2e7d32; margin-top: 0;'>📋 Modern Table Formatting Guide</h3>",
                
                "<h4 style='color: #2e7d32;'>Current Table Configuration:</h4>",
                "<ul>",
                "<li><strong>Table Type:</strong> ", stringr::str_to_title(gsub("_", " ", table_type)), "</li>",
                "<li><strong>Variables:</strong> ", n_vars, " variables displayed</li>",
                "<li><strong>Observations:</strong> ", n_rows, " rows of data</li>",
                "<li><strong>Theme:</strong> ", stringr::str_to_title(table_theme), " styling</li>",
                "<li><strong>Output Format:</strong> ", toupper(output_format), " optimized</li>",
                if (!is.null(group_var) && group_var != "") paste0("<li><strong>Grouping:</strong> By ", group_var, "</li>") else "",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>TinyTable Features:</h4>",
                "<ul>",
                "<li><strong>Zero Dependencies:</strong> Lightweight, fast table generation</li>",
                "<li><strong>Multiple Formats:</strong> HTML, PDF, Word, LaTeX, Markdown ready</li>",
                "<li><strong>Modern Design:</strong> Clean, contemporary aesthetics</li>",
                "<li><strong>Flexible Styling:</strong> Professional themes and customization</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Table Types Explained:</h4>",
                switch(table_type,
                    "summary" = "<p><strong>Data Summary:</strong> Quick overview with variable types and basic statistics. Perfect for initial data exploration.</p>",
                    "descriptive" = "<p><strong>Descriptive Statistics:</strong> Detailed statistical summaries including means, standard deviations, and quartiles for numeric variables.</p>",
                    "grouped" = "<p><strong>Grouped Summary:</strong> Statistics organized by your grouping variable, ideal for comparing across categories.</p>",
                    "raw" = "<p><strong>Raw Data Display:</strong> Clean presentation of your actual data values with professional formatting.</p>",
                    "<p><strong>Custom Format:</strong> Flexible table layout with user-specified formatting and styling options.</p>"
                ),
                
                "<h4 style='color: #2e7d32;'>Advantages over Traditional Tables:</h4>",
                "<ul>",
                "<li><strong>Lightweight:</strong> No heavy dependencies, faster loading</li>",
                "<li><strong>Modern Aesthetics:</strong> Contemporary design principles</li>",
                "<li><strong>Format Flexibility:</strong> Works across document types</li>",
                "<li><strong>Publication Ready:</strong> Professional quality for journals</li>",
                "<li><strong>Customizable:</strong> Extensive styling and theming options</li>",
                "</ul>",
                
                "<h4 style='color: #2e7d32;'>Clinical Research Applications:</h4>",
                "<ul>",
                "<li><strong>Baseline Characteristics:</strong> Patient demographics and clinical variables</li>",
                "<li><strong>Results Tables:</strong> Statistical outcomes and effect sizes</li>",
                "<li><strong>Descriptive Analysis:</strong> Summary statistics for publications</li>",
                "<li><strong>Data Exploration:</strong> Quick, beautiful data summaries</li>",
                "</ul>",
                
                "<p style='font-size: 12px; color: #2e7d32; margin-top: 15px;'>",
                "<em>📋 Modern table formatting that enhances your research presentation with zero dependency overhead</em>",
                "</p></div>"
            )
            
            return(interpretation_html)
        }

    )
)

# Store analysis data
.analysis_data <- NULL