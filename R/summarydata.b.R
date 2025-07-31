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

            # mysummary function
            mysummary <- function(myvar) {

                mean_x <- round(mean(jmvcore::toNumeric(dataset[[myvar]]),
                  na.rm = TRUE), digits = 1)

                sd_x <- round(sd(x = jmvcore::toNumeric(dataset[[myvar]]),
                  na.rm = TRUE), digits = 1)

                median_x <- round(median(jmvcore::toNumeric(dataset[[myvar]]),
                  na.rm = TRUE), digits = 1)

                min_x <- round(min(jmvcore::toNumeric(dataset[[myvar]]), na.rm = TRUE),
                  digits = 1)

                max_x <- round(max(jmvcore::toNumeric(dataset[[myvar]]), na.rm = TRUE),
                  digits = 1)



                dist_text <- ""

                # If the distribution diagnostics option is enabled, add additional tests.
                if (self$options$distr) {
                    # Shapiro-Wilk test (only valid if 3 <= sample size <= 5000)

                    numeric_data <- jmvcore::toNumeric(dataset[[myvar]])

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

                    # Append the distribution diagnostics to the summary text.
                    print(paste0("Mean of <strong>", myvar, "</strong> is: ", mean_x, " \U00B1 ", sd_x,
                                 ". (Median: ", median_x, " [Min: ", min_x, " - ", "Max: ",
                                 max_x, "]) <br>", dist_text, "<br><br>", collapse = " "))

            }

            results <- purrr::map(.x = var_list, .f = mysummary)
            results <- unlist(results)
            self$results$text$setContent(results)


            # Version-compatible gtExtras implementation with fallbacks
            plot_dataset <- tryCatch({
                # Primary approach: Use gtExtras with version compatibility
                private$.safe_gt_plt_summary(dataset, var_list)
            }, error = function(e) {
                # Secondary approach: Custom gt table with gtExtras-like styling
                tryCatch({
                    private$.gtExtras_style_fallback(dataset, var_list)
                }, error = function(e2) {
                    # Final fallback: Simple HTML message
                    htmltools::HTML(paste0(
                        "<div style='padding: 20px; border-left: 4px solid #dc3545; background-color: #f8d7da; margin: 10px 0;'>",
                        "<h5 style='color: #721c24; margin-top: 0;'>Summary Table Error</h5>",
                        "<p><strong>gtExtras error:</strong> ", e$message, "</p>",
                        "<p><strong>Fallback error:</strong> ", e2$message, "</p>",
                        "<p>Please check your data types and ensure variables are numeric.</p>",
                        "</div>"
                    ))
                })
            })
            
            self$results$text1$setContent(plot_dataset)

        }
        },

        # Version-compatible gtExtras wrapper
        .safe_gt_plt_summary = function(dataset, var_list) {
            # Filter to numeric variables only
            numeric_vars <- var_list[sapply(dataset[var_list], is.numeric)]
            
            if (length(numeric_vars) == 0) {
                return(htmltools::HTML("<p>No numeric variables selected for gtExtras summary.</p>"))
            }
            
            # Prepare clean dataset with only numeric variables
            clean_data <- dataset[numeric_vars]
            
            # Ensure proper data types
            clean_data <- as.data.frame(lapply(clean_data, function(x) {
                if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
            }))
            
            # Check gt version and handle missing value formatting
            gt_version <- utils::packageVersion("gt")
            
            # Try gtExtras with version compatibility
            if (gt_version >= "0.6.0") {
                # For newer gt versions, gtExtras should handle sub_missing internally
                summary_table <- clean_data %>% gtExtras::gt_plt_summary()
            } else {
                # For older gt versions, use fmt_missing approach
                summary_table <- clean_data %>% gtExtras::gt_plt_summary()
            }
            
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
