#' @title Summary of Continuous Variables with Distribution Diagnostics
#' @return Text and an HTML summary table (with optional distribution diagnostics)
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gt gt tab_header fmt_number cols_label md cell_fill cells_column_labels cell_text tab_style opt_stylize tab_options
#' @importFrom gtExtras gt_plt_summary
#' @importFrom htmltools HTML
#' @import moments
#' @importFrom utils packageVersion

summarydataClass <- if (requireNamespace("jmvcore")) R6::R6Class("summarydataClass",
    inherit = summarydataBase, private = list(
        
        .run = function() {


        # Check if variables have been selected. If not, display a welcoming message with instructions.
        if (length(self$options$vars) == 0) {
            intro_msg <- "
          <h3>Welcome to ClinicoPath Descriptives!</h3>
          <p>This tool helps you generate descriptive statistics for your numeric variables.
          Please select one or more continuous variables from the options panel.</p>
          <p>If you want to inspect distribution characteristics, enable the 'Distribution Diagnostics' option.</p>"
            self$results$todo$setContent(intro_msg)
            return()
        } else {
            # Clear any introductory message if variables are selected.
            self$results$todo$setContent("")

            # Validate that the dataset contains complete rows.
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
            }

            # Retrieve the data and construct the list of variables.
            dataset <- self$data
            var_formula <- jmvcore::constructFormula(terms = self$options$vars)
            var_list <- unlist(jmvcore::decomposeFormula(formula = var_formula))

            # mysummary function with optimized calculations
            mysummary <- function(myvar) {
                # Cache numeric conversion to avoid repeated calls
                numeric_data <- jmvcore::toNumeric(dataset[[myvar]])
                
                # Calculate all statistics at once with specified decimal places
                decimal_places <- if (!is.null(self$options$decimal_places)) self$options$decimal_places else 1
                
                stats <- round(c(
                    mean = mean(numeric_data, na.rm = TRUE),
                    sd = sd(numeric_data, na.rm = TRUE),
                    median = median(numeric_data, na.rm = TRUE),
                    min = min(numeric_data, na.rm = TRUE),
                    max = max(numeric_data, na.rm = TRUE)
                ), digits = decimal_places)
                
                mean_x <- stats["mean"]
                sd_x <- stats["sd"]
                median_x <- stats["median"]
                min_x <- stats["min"]
                max_x <- stats["max"]



                dist_text <- ""

                # If the distribution diagnostics option is enabled, add additional tests.
                if (self$options$distr) {
                    # Shapiro-Wilk test (only valid if 3 <= sample size <= 5000)
                    # Use already cached numeric_data
                    valid_data <- na.omit(numeric_data)
                    if (length(valid_data) >= 3 && length(valid_data) <= 5000) {
                        sw_test <- shapiro.test(valid_data)
                        p_val <- round(sw_test$p.value, 3)
                    } else {
                        p_val <- NA
                    }

                    # Calculate skewness and kurtosis using the moments package.
                    skew_val <- round(moments::skewness(numeric_data, na.rm = TRUE), 2)
                    kurt_val <- round(moments::kurtosis(numeric_data, na.rm = TRUE), 2)

                    # Interpret normality based on the Shapiro-Wilk p-value.
                    norm_status <- if (!is.na(p_val)) {
                        if (p_val > 0.05) "appears to be normally distributed" else "does not appear to be normally distributed. Please use relevant visualisation and tests to verify the characteristics of distribution."
                    } else {
                        "Normality test not applicable due to sample size"
                    }

                    dist_text <- paste0(
                        "<br><em>Distribution Diagnostics for ", myvar ,":</em> Shapiro-Wilk p-value = ", p_val,
                        "; Skewness = ", skew_val, "; Kurtosis = ", kurt_val,
                        " (Data ", norm_status, ")."
                    )
                }

                    # Return the summary text with distribution diagnostics.
                    paste0("Mean of <strong>", myvar, "</strong> is: ", mean_x, " &plusmn; ", sd_x,
                           ". (Median: ", median_x, " [Min: ", min_x, " - ", "Max: ",
                           max_x, "]) <br>", dist_text, "<br><br>", collapse = " ")

            }

            results <- purrr::map(.x = var_list, .f = mysummary)
            results <- unlist(results)
            self$results$text$setContent(results)


            # CORRECT IMPLEMENTATION: Use gtExtras as intended by the package
            plot_dataset <- tryCatch({
                # Filter to numeric variables for gtExtras
                numeric_vars <- var_list[sapply(dataset[var_list], is.numeric)]
                
                # Debug: Show which variables were found
                debug_info <- paste0(
                    "<div style='background-color: #e3f2fd; border-left: 4px solid #2196f3; padding: 10px; margin: 5px 0;'>",
                    "<small><strong>Debug:</strong> Found ", length(numeric_vars), " numeric variables: ",
                    paste(numeric_vars, collapse = ", "), "</small>",
                    "</div>"
                )
                
                if (length(numeric_vars) > 0) {
                    clean_data <- dataset[numeric_vars]
                    
                    # Ensure proper data types
                    clean_data <- as.data.frame(lapply(clean_data, function(x) {
                        if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
                    }))
                    
                    # Use gtExtras with default styling as intended
                    summary_table <- clean_data %>% 
                        gtExtras::gt_plt_summary()
                    
                    # Convert to HTML using multiple methods for compatibility
                    html_result <- tryCatch({
                        # Method 1: Direct as_raw_html conversion (most reliable)
                        html_output <- as.character(gt::as_raw_html(summary_table))
                        return(html_output)
                    }, error = function(e2) {
                        # Method 2: Print method fallback
                        print_table <- print(summary_table)
                        if (is.list(print_table) && "children" %in% names(print_table)) {
                            return(print_table[["children"]][[2]])
                        } else {
                            # Method 3: Direct print output
                            return(as.character(print_table))
                        }
                    })
                    
                    # Combine debug info with result
                    return(htmltools::HTML(paste0(debug_info, html_result)))
                } else {
                    return(htmltools::HTML("<p>No numeric variables selected for gtExtras summary table.</p>"))
                }
            }, error = function(e) {
                # Debug information and fallback to simple table if gtExtras fails
                warning_msg <- paste0(
                    "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; margin: 10px 0; border-radius: 3px;'>",
                    "<strong>gtExtras Note:</strong> Using fallback table format. ",
                    "Error: ", e$message, 
                    "</div>"
                )
                simple_table <- private$.create_simple_summary_table(dataset, var_list)
                return(htmltools::HTML(paste0(warning_msg, as.character(simple_table))))
            })
            
            # COMMENTED OUT: Fallback methods for testing original gtExtra functions
            # plot_dataset <- tryCatch({
            #     # Primary approach: Direct gtExtras call (version compatibility verified)
            #     private$.create_summary_table(dataset, var_list)
            # }, error = function(e) {
            #     # Fallback: Custom gt table with comprehensive statistics
            #     tryCatch({
            #         private$.gtExtras_style_fallback(dataset, var_list)
            #     }, error = function(e2) {
            #         # Final fallback: Error message with diagnostics
            #         htmltools::HTML(paste0(
            #             "<div style='padding: 20px; border-left: 4px solid #dc3545; background-color: #f8d7da; margin: 10px 0;'>",
            #             "<h5 style='color: #721c24; margin-top: 0;'>Summary Table Error</h5>",
            #             "<p><strong>Primary error:</strong> ", e$message, "</p>",
            #             "<p><strong>Fallback error:</strong> ", e2$message, "</p>",
            #             "<p>Ensure variables are numeric and gtExtras package is properly installed.</p>",
            #             "</div>"
            #         ))
            #     })
            # })
            
            self$results$text1$setContent(plot_dataset)

        }
        },

        # Simple summary table without resource-intensive gtExtras
        .create_simple_summary_table = function(dataset, var_list) {
            # Filter to numeric variables only
            numeric_vars <- var_list[sapply(dataset[var_list], is.numeric)]
            
            if (length(numeric_vars) == 0) {
                return(htmltools::HTML("<p>No numeric variables selected for summary table.</p>"))
            }
            
            # Create simple HTML table
            html <- "<table style='border-collapse: collapse; margin: 10px 0; width: 100%;'>"
            html <- paste0(html, "<tr style='background-color: #f8f9fa;'>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Variable</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>N</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Mean</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>SD</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Min</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Max</th>")
            html <- paste0(html, "</tr>")
            
            for (var in numeric_vars) {
                data_col <- dataset[[var]]
                data_col <- as.numeric(data_col[!is.na(data_col)])
                
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; font-weight: bold;'>", var, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", length(data_col), "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", round(mean(data_col, na.rm = TRUE), 2), "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", round(sd(data_col, na.rm = TRUE), 2), "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", round(min(data_col, na.rm = TRUE), 2), "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", round(max(data_col, na.rm = TRUE), 2), "</td>")
                html <- paste0(html, "</tr>")
            }
            
            html <- paste0(html, "</table>")
            return(htmltools::HTML(html))
        },

        # Simplified gtExtras wrapper (compatibility verified)
        .create_summary_table = function(dataset, var_list) {
            # Filter to numeric variables only
            numeric_vars <- var_list[sapply(dataset[var_list], is.numeric)]
            
            if (length(numeric_vars) == 0) {
                return(htmltools::HTML("<p>No numeric variables selected for summary table.</p>"))
            }
            
            # Prepare clean dataset with only numeric variables
            clean_data <- dataset[numeric_vars]
            
            # Ensure proper data types
            clean_data <- as.data.frame(lapply(clean_data, function(x) {
                if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
            }))
            
            # Use gtExtras with default styling
            summary_table <- clean_data %>% 
                gtExtras::gt_plt_summary()
            
            # Convert to HTML
            print_table <- print(summary_table)
            return(htmltools::HTML(print_table[["children"]][[2]]))
        },

        # Fallback with gtExtras-style appearance
        .gtExtras_style_fallback = function(dataset, var_list) {
            # Get numeric variables only
            numeric_vars <- var_list[sapply(dataset[var_list], is.numeric)]
            
            if (length(numeric_vars) == 0) {
                return(htmltools::HTML("<p>No numeric variables available for summary table.</p>"))
            }
            
            # Calculate comprehensive summary statistics
            summary_stats <- data.frame(
                Variable = numeric_vars,
                Type = rep("numeric", length(numeric_vars)),
                N = sapply(dataset[numeric_vars], function(x) sum(!is.na(x))),
                Missing = sapply(dataset[numeric_vars], function(x) sum(is.na(x))),
                Mean = sapply(dataset[numeric_vars], function(x) round(mean(x, na.rm = TRUE), 2)),
                SD = sapply(dataset[numeric_vars], function(x) round(sd(x, na.rm = TRUE), 2)),
                Min = sapply(dataset[numeric_vars], function(x) round(min(x, na.rm = TRUE), 2)),
                Q25 = sapply(dataset[numeric_vars], function(x) round(quantile(x, 0.25, na.rm = TRUE), 2)),
                Median = sapply(dataset[numeric_vars], function(x) round(median(x, na.rm = TRUE), 2)),
                Q75 = sapply(dataset[numeric_vars], function(x) round(quantile(x, 0.75, na.rm = TRUE), 2)),
                Max = sapply(dataset[numeric_vars], function(x) round(max(x, na.rm = TRUE), 2)),
                stringsAsFactors = FALSE
            )
            
            # Create gtExtras-style table
            gt_table <- summary_stats %>%
                gt::gt() %>%
                gt::tab_header(
                    title = gt::md("**Dataset Summary**"),
                    subtitle = gt::md("*Comprehensive statistics for numeric variables*")
                ) %>%
                gt::fmt_number(
                    columns = c("Mean", "SD", "Min", "Q25", "Median", "Q75", "Max"),
                    decimals = 2
                ) %>%
                gt::cols_label(
                    Variable = "Variable",
                    Type = "Type",
                    N = "N",
                    Missing = "Missing",
                    Mean = "Mean",
                    SD = "SD",
                    Min = "Min",
                    Q25 = "Q25",
                    Median = "Median",
                    Q75 = "Q75",
                    Max = "Max"
                ) %>%
                gt::tab_style(
                    style = gt::cell_fill(color = "#f8f9fa"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::opt_stylize(style = 6, color = "blue") %>%
                gt::tab_options(
                    table.font.size = 12,
                    heading.title.font.size = 16,
                    heading.subtitle.font.size = 12
                )
            
            # Convert to HTML
            print_table <- print(gt_table)
            return(htmltools::HTML(print_table[["children"]][[2]]))
        }


    ))
