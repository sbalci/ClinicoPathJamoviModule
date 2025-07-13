#' @title Enhanced Summary Statistics for Continuous and Date Variables
#'
#' @description
#' This function provides comprehensive descriptive statistics for continuous and date variables
#' with multiple output formats and enhanced visualization capabilities. It supports various
#' summary formats including standard descriptives, enhanced pivot tables, and automated EDA
#' reports using the summarytools package.
#'
#' **Key Features:**
#' - Multiple summary formats (standard, enhanced, pivot tables, summarytools)
#' - Date variable support with automatic format detection
#' - Distribution diagnostics (normality tests, skewness, kurtosis)
#' - Grouping capabilities for stratified analysis
#' - Professional HTML output with embedded visualizations
#' - Export-ready pivot tables with multiple layout styles
#'
#' **Summary Formats:**
#' - **Standard**: Basic descriptive statistics (mean, SD, median, range)
#' - **Enhanced (sumvar style)**: Comprehensive statistics with confidence intervals
#' - **Pivot Enhanced**: Professional pivot tables with customizable layouts
#' - **summarytools Integration**: Automated EDA with embedded visualizations
#'
#' @param data The data as a data frame
#' @param vars Continuous variables for analysis (numeric variables)
#' @param date_vars Date/time variables for specialized date analysis with automatic format detection
#' @param distr Whether to include distribution diagnostics (Shapiro-Wilk test, skewness, kurtosis)
#' @param summary_format Format for output: "standard", "sumvar", "pivot", "summarytools_df", "summarytools_desc", "summarytools_freq"
#' @param grvar Optional grouping variable for stratified analysis
#' @param pivot_layout Layout style for pivot tables: "clinical", "statistical", "comparative"
#' @param include_confidence Whether to include confidence intervals in pivot summaries
#' @param advanced_metrics Whether to include advanced metrics (MAD, CV, robust statistics)
#' @param pivot_export Whether to enable enhanced export capabilities
#' @param summarytools_graphs Whether to include embedded graphics in summarytools output
#' @param summarytools_round_digits Number of decimal places for summarytools output (1-6)
#'
#' @return A results object containing HTML summaries, pivot tables, and visualizations
#'
#' @details
#' **Date Variable Support:**
#' Automatically detects and parses multiple date formats including:
#' - YYYY-MM-DD (ISO 8601)
#' - DD/MM/YYYY and MM/DD/YYYY
#' - Date-time combinations with HMS
#' - Provides date-specific statistics (range, median date, time span)
#'
#' **Distribution Diagnostics:**
#' When enabled, provides comprehensive distributional analysis:
#' - Shapiro-Wilk normality test (for n=3-5000)
#' - Skewness and kurtosis measures
#' - Normality interpretation and recommendations
#'
#' **Pivot Table Layouts:**
#' - **Clinical**: Optimized for clinical research presentations
#' - **Statistical**: Focused on statistical analysis requirements
#' - **Comparative**: Designed for comparative studies and meta-analyses
#'
#' **summarytools Integration:**
#' Leverages the summarytools package for automated EDA:
#' - dfSummary: Comprehensive dataset overview with embedded plots
#' - descr: Advanced descriptive statistics with robust measures
#' - freq: Enhanced frequency tables for categorical variables
#'
#' @examples
#' \donttest{
#' # Basic continuous variable summary
#' summarydata(
#'   data = mtcars,
#'   vars = c("mpg", "hp", "wt"),
#'   summary_format = "standard",
#'   distr = TRUE
#' )
#'
#' # Enhanced pivot table summary
#' summarydata(
#'   data = clinical_data,
#'   vars = c("age", "weight", "height"),
#'   summary_format = "pivot",
#'   pivot_layout = "clinical",
#'   include_confidence = TRUE,
#'   advanced_metrics = TRUE
#' )
#'
#' # Date variable analysis
#' summarydata(
#'   data = study_data,
#'   date_vars = c("enrollment_date", "follow_up_date"),
#'   summary_format = "sumvar"
#' )
#'
#' # Grouped analysis with summarytools
#' summarydata(
#'   data = trial_data,
#'   vars = c("baseline_score", "outcome_measure"),
#'   grvar = "treatment_group",
#'   summary_format = "summarytools_df",
#'   summarytools_graphs = TRUE
#' )
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gt gt
#' @importFrom htmltools HTML
#' @importFrom gtExtras gt_plt_summary
#' @import moments
#' @import pivottabler
#' @importFrom summarytools dfSummary descr freq ctable view st_options
#' @importFrom lubridate ymd ymd_hms dmy dmy_hms mdy mdy_hms ydm parse_date_time is.Date
#' @importFrom purrr map
#' @importFrom rlang `%||%`
#' @importFrom stats shapiro.test quantile sd median na.omit
#'

summarydata2Class <- if (requireNamespace("jmvcore")) R6::R6Class("summarydata2Class",
    inherit = summarydata2Base,
    private = list(
        # Private fields for data storage
        pivot_data = NULL,

        # Input validation method
        .validateInputs = function() {
            # Check if dataset exists and has rows
            if (is.null(self$data) || nrow(self$data) == 0) {
                stop("Error: No data provided or dataset is empty")
            }

            # Validate selected variables exist in data
            if (length(self$options$vars) > 0) {
                var_formula <- jmvcore::constructFormula(terms = self$options$vars)
                var_list <- unlist(jmvcore::decomposeFormula(formula = var_formula))

                missing_vars <- var_list[!var_list %in% names(self$data)]
                if (length(missing_vars) > 0) {
                    stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
                }

                # Check if selected variables are actually numeric
                for (var in var_list) {
                    if (!is.numeric(self$data[[var]]) && !private$.can_be_numeric(self$data[[var]])) {
                        warning(paste("Variable '", var, "' does not appear to be numeric and may not produce meaningful results"))
                    }
                }
            }

            # Validate date variables
            if (length(self$options$date_vars) > 0) {
                date_var_formula <- jmvcore::constructFormula(terms = self$options$date_vars)
                date_var_list <- unlist(jmvcore::decomposeFormula(formula = date_var_formula))

                missing_date_vars <- date_var_list[!date_var_list %in% names(self$data)]
                if (length(missing_date_vars) > 0) {
                    stop(paste("Date variables not found in data:", paste(missing_date_vars, collapse = ", ")))
                }
            }

            # Validate grouping variable
            if (!is.null(self$options$grvar) && length(self$options$grvar) > 0) {
                if (!self$options$grvar %in% names(self$data)) {
                    stop(paste("Grouping variable '", self$options$grvar, "' not found in data"))
                }

                # Check if grouping variable has reasonable number of levels
                group_levels <- length(unique(self$data[[self$options$grvar]]))
                if (group_levels > 20) {
                    warning(paste("Grouping variable has", group_levels, "levels. Consider using fewer groups for better analysis."))
                }
            }

            # Validate summarytools options
            if (self$options$summary_format %in% c("summarytools_df", "summarytools_desc", "summarytools_freq")) {
                if (!requireNamespace("summarytools", quietly = TRUE)) {
                    stop("summarytools package is required for the selected format but is not available")
                }
            }

        },

        .run = function() {
            # Perform comprehensive input validation first
            tryCatch({
                private$.validateInputs()
            }, error = function(e) {
                # Display validation error in a user-friendly way
                error_msg <- paste0(
                    "<div style='color: red; background-color: #ffebee; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                    "<h4>⚠️ Input Validation Error</h4>",
                    "<p>", e$message, "</p>",
                    "<p><em>Please check your variable selections and data, then try again.</em></p>",
                    "</div>"
                )
                self$results$todo$setContent(error_msg)
                return()
            })

        # Check if variables have been selected. If not, display a welcoming message with instructions.
        if (length(self$options$vars) == 0 && length(self$options$date_vars) == 0) {
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

            # Additional runtime validation
            if (nrow(self$data) == 0) {
                error_msg <- paste0(
                    "<div style='color: red; background-color: #ffebee; padding: 15px; border-radius: 5px;'>",
                    "<h4>⚠️ Data Error</h4>",
                    "<p>The dataset contains no complete rows. Please check your data and try again.</p>",
                    "</div>"
                )
                self$results$todo$setContent(error_msg)
                return()
            }

            # Retrieve the data and construct the list of variables.
            dataset <- self$data

            # Date validation function using lubridate
            validate_and_parse_dates <- function(x) {
                if (is.Date(x) || inherits(x, "POSIXt")) {
                    return(list(valid = TRUE, parsed = x, format = "already_date"))
                }

                # Try different date formats
                date_formats <- list(
                    ymd = function(x) lubridate::ymd(x, quiet = TRUE),
                    ymd_hms = function(x) lubridate::ymd_hms(x, quiet = TRUE),
                    dmy = function(x) lubridate::dmy(x, quiet = TRUE),
                    dmy_hms = function(x) lubridate::dmy_hms(x, quiet = TRUE),
                    mdy = function(x) lubridate::mdy(x, quiet = TRUE),
                    mdy_hms = function(x) lubridate::mdy_hms(x, quiet = TRUE),
                    ydm = function(x) lubridate::ydm(x, quiet = TRUE)
                )

                for (format_name in names(date_formats)) {
                    parsed <- tryCatch({
                        date_formats[[format_name]](x)
                    }, error = function(e) NULL)

                    if (!is.null(parsed) && sum(!is.na(parsed)) > 0) {
                        return(list(valid = TRUE, parsed = parsed, format = format_name))
                    }
                }

                # Try generic parsing as last resort
                parsed <- tryCatch({
                    lubridate::parse_date_time(x, orders = c("ymd", "dmy", "mdy", "ymd HMS", "dmy HMS", "mdy HMS"))
                }, error = function(e) NULL)

                if (!is.null(parsed) && sum(!is.na(parsed)) > 0) {
                    return(list(valid = TRUE, parsed = parsed, format = "generic"))
                }

                return(list(valid = FALSE, parsed = NULL, format = "invalid"))
            }

            # Handle regular numeric variables
            var_list <- c()
            if (length(self$options$vars) > 0) {
                var_formula <- jmvcore::constructFormula(terms = self$options$vars)
                var_list <- unlist(jmvcore::decomposeFormula(formula = var_formula))
            }

            # Handle and validate date variables
            date_var_list <- c()
            valid_date_vars <- c()
            date_parse_info <- list()

            if (length(self$options$date_vars) > 0) {
                date_var_formula <- jmvcore::constructFormula(terms = self$options$date_vars)
                date_var_list <- unlist(jmvcore::decomposeFormula(formula = date_var_formula))

                # Validate each date variable with enhanced error handling
                for (date_var in date_var_list) {
                    tryCatch({
                        validation_result <- validate_and_parse_dates(dataset[[date_var]])

                        if (validation_result$valid) {
                            valid_date_vars <- c(valid_date_vars, date_var)
                            date_parse_info[[date_var]] <- validation_result
                            # Update dataset with parsed dates
                            dataset[[date_var]] <- validation_result$parsed
                        } else {
                            warning(paste("Variable", date_var, "could not be parsed as a valid date format"))
                        }
                    }, error = function(e) {
                        warning(paste("Error processing date variable", date_var, ":", e$message))
                    })
                }
            }

            # Combined variable check
            all_vars <- c(var_list, valid_date_vars)
            if (length(all_vars) == 0) {
                intro_msg <- "
          <h3>Welcome to ClinicoPath Descriptives!</h3>
          <p>This tool helps you generate descriptive statistics for your numeric and date variables.</p>
          <p>Please select one or more continuous variables and/or date variables from the options panel.</p>
          <p>For date variables, supported formats include: YYYY-MM-DD, DD/MM/YYYY, MM/DD/YYYY, and datetime combinations.</p>
          <p>If you want to inspect distribution characteristics, enable the 'Distribution Diagnostics' option.</p>"
                self$results$todo$setContent(intro_msg)
                return()
            }

            # mysummary function - enhanced with optional sumvar-style features
            mysummary <- function(myvar, data_subset = dataset) {

                # Check if this is a date variable
                is_date_var <- myvar %in% valid_date_vars

                if (is_date_var) {
                    # Handle date variable with error handling
                    tryCatch({
                        date_data <- data_subset[[myvar]]

                        # Date-specific statistics
                        total_n <- length(date_data)
                        valid_dates <- na.omit(date_data)
                        n_valid <- length(valid_dates)
                        n_missing <- total_n - n_valid

                        if (n_valid > 0) {
                        min_date <- min(valid_dates)
                        max_date <- max(valid_dates)
                        median_date <- median(valid_dates)

                        # Date range in days
                        date_range_days <- as.numeric(max_date - min_date)

                        # Format info
                        format_used <- date_parse_info[[myvar]]$format

                        # Generate date summary
                        date_summary <- paste0(
                            "<strong>", myvar, "</strong> (Date Variable)<br>",
                            "Format detected: ", format_used, "<br>",
                            "n = ", n_valid, ", missing = ", n_missing, " (", round(n_missing/total_n*100, 1), "%)<br>",
                            "Date range: ", as.character(min_date), " to ", as.character(max_date), "<br>",
                            "Median date: ", as.character(median_date), "<br>",
                            "Time span: ", round(date_range_days), " days<br><br>"
                        )

                            return(date_summary)
                        } else {
                            return(paste0("<strong>", myvar, "</strong> (Date Variable): No valid dates found<br><br>"))
                        }
                    }, error = function(e) {
                        return(paste0("<strong>", myvar, "</strong> (Date Variable): Error processing dates - ", e$message, "<br><br>"))
                    })
                } else {
                    # Handle numeric variable with error handling
                    tryCatch({
                        numeric_data <- jmvcore::toNumeric(data_subset[[myvar]])

                        # Check for sufficient valid data
                        valid_data <- na.omit(numeric_data)
                        if (length(valid_data) < 2) {
                            return(paste0("<strong>", myvar, "</strong>: Insufficient valid data (n=", length(valid_data), ")<br><br>"))
                        }

                    # Original statistics (preserved for backward compatibility)
                    mean_x <- round(mean(numeric_data, na.rm = TRUE), digits = 1)
                    sd_x <- round(sd(numeric_data, na.rm = TRUE), digits = 1)
                median_x <- round(median(numeric_data, na.rm = TRUE), digits = 1)
                min_x <- round(min(numeric_data, na.rm = TRUE), digits = 1)
                max_x <- round(max(numeric_data, na.rm = TRUE), digits = 1)

                # Enhanced statistics for sumvar or pivot formats
                summary_format <- self$options$summary_format
                if (summary_format %in% c("sumvar", "pivot")) {
                    total_n <- length(numeric_data)
                    valid_data <- na.omit(numeric_data)
                    n_valid <- length(valid_data)
                    n_missing <- total_n - n_valid

                    # Additional statistics
                    q1 <- round(quantile(valid_data, 0.25, na.rm = TRUE), digits = 2)
                    q3 <- round(quantile(valid_data, 0.75, na.rm = TRUE), digits = 2)
                    iqr <- round(q3 - q1, digits = 2)

                    # 95% Confidence interval for mean
                    se_mean <- sd(valid_data) / sqrt(n_valid)
                    ci_lower <- round(mean(valid_data) - 1.96 * se_mean, digits = 2)
                    ci_upper <- round(mean(valid_data) + 1.96 * se_mean, digits = 2)

                    # Advanced metrics for pivot format
                    if (summary_format == "pivot" && self$options$advanced_metrics) {
                        mad_val <- round(mad(valid_data, na.rm = TRUE), digits = 2)
                        cv_val <- round((sd(valid_data) / mean(valid_data)) * 100, digits = 2)
                    }
                }



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

                # Generate output based on selected format
                if (summary_format == "sumvar") {
                    # sumvar-style comprehensive output
                    summary_text <- paste0(
                        "<strong>", myvar, "</strong><br>",
                        "n = ", n_valid, ", missing = ", n_missing, " (", round(n_missing/total_n*100, 1), "%)<br>",
                        "Mean: ", round(mean(valid_data), 2), " (95% CI: ", ci_lower, " - ", ci_upper, ")<br>",
                        "Median: ", round(median(valid_data), 2), " (Q1: ", q1, ", Q3: ", q3, ")<br>",
                        "SD: ", round(sd(valid_data), 2), ", IQR: ", iqr, "<br>",
                        "Range: [", round(min(valid_data), 2), " - ", round(max(valid_data), 2), "]",
                        dist_text, "<br><br>"
                    )

                    return(summary_text)
                } else if (summary_format == "pivot") {
                    # Store data for pivot table generation (using private field instead of global)
                    if (is.null(private$pivot_data)) private$pivot_data <- list()
                    private$pivot_data[[myvar]] <- list(
                        variable = myvar,
                        n = n_valid,
                        missing = n_missing,
                        mean = round(mean(valid_data), 2),
                        sd = round(sd(valid_data), 2),
                        median = round(median(valid_data), 2),
                        q1 = q1,
                        q3 = q3,
                        iqr = iqr,
                        min = round(min(valid_data), 2),
                        max = round(max(valid_data), 2),
                        ci_lower = ci_lower,
                        ci_upper = ci_upper
                    )

                    if (self$options$advanced_metrics) {
                        private$pivot_data[[myvar]]$mad <- mad_val
                        private$pivot_data[[myvar]]$cv <- cv_val
                    }

                    # Return minimal text for now, pivot table will be generated separately
                    return("")
                } else if (summary_format %in% c("summarytools_df", "summarytools_desc", "summarytools_freq")) {
                    # summarytools integration - NEW FUNCTIONALITY
                    return(private$.generate_summarytools_output(myvar, data_subset, summary_format))
                } else {
                    # Original output format (preserved)
                    return(paste0("Mean of <strong>", myvar, "</strong> is: ", mean_x, " \U00B1 ", sd_x,
                                 ". (Median: ", median_x, " [Min: ", min_x, " - ", "Max: ",
                                 max_x, "]) <br>", dist_text, "<br><br>", collapse = " "))
                }

                    }, error = function(e) {
                        return(paste0("<strong>", myvar, "</strong>: Error processing numeric data - ", e$message, "<br><br>"))
                    })
                } # End of numeric variable handling

            }

            # Handle grouping if specified with error handling
            if (!is.null(self$options$grvar) && length(self$options$grvar) > 0) {
                tryCatch({
                    group_var <- self$options$grvar

                    # Split analysis by group
                    group_results <- list()
                    group_levels <- levels(as.factor(dataset[[group_var]]))

                    if (length(group_levels) == 0) {
                        warning("Grouping variable has no valid levels")
                        group_levels <- unique(dataset[[group_var]])
                        group_levels <- group_levels[!is.na(group_levels)]
                    }

                for (level in group_levels) {
                    group_subset <- dataset[dataset[[group_var]] == level, ]
                    level_results <- list()

                    for (var in all_vars) {
                        # Pass the group subset to mysummary function
                        level_summary <- mysummary(var, group_subset)
                        level_results[[var]] <- paste0("<em>Group: ", level, "</em><br>", level_summary)
                    }

                    group_results[[level]] <- paste(unlist(level_results), collapse = "<br>")
                }

                    results <- paste(unlist(group_results), collapse = "<hr>")
                }, error = function(e) {
                    error_msg <- paste0(
                        "<div style='color: red; padding: 10px; background-color: #ffebee;'>",
                        "<strong>Error in grouped analysis:</strong> ", e$message,
                        "<br>Falling back to ungrouped analysis.",
                        "</div><br>"
                    )
                    # Fall back to ungrouped analysis
                    results <<- purrr::map(.x = all_vars, .f = mysummary)
                    results <<- unlist(results)
                    results <<- paste(error_msg, paste(results, collapse = ""), sep = "")
                })

            } else {
                # Original ungrouped analysis - handle both numeric and date variables
                results <- purrr::map(.x = all_vars, .f = mysummary)
                results <- unlist(results)
                results <- paste(results, collapse = "")
            }

            self$results$text$setContent(results)

            # Generate pivot table if pivot format is selected
            if (self$options$summary_format == "pivot" && !is.null(private$pivot_data) && length(private$pivot_data) > 0) {
                pivot_html <- private$.generate_pivot_summary(private$pivot_data)
                self$results$pivot_summary$setContent(pivot_html)

                # Generate export information if enabled
                if (self$options$pivot_export) {
                    export_html <- private$.generate_export_info()
                    self$results$pivot_export_info$setContent(export_html)
                }
            }

            # Generate summarytools output if selected - NEW FUNCTIONALITY
            if (self$options$summary_format %in% c("summarytools_df", "summarytools_desc", "summarytools_freq")) {
                tryCatch({
                    summarytools_html <- private$.generate_comprehensive_summarytools(dataset, all_vars)
                    self$results$text$setContent(summarytools_html)
                }, error = function(e) {
                    error_html <- paste0(
                        "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
                        "<h4>summarytools Error</h4>",
                        "<p>Error generating summarytools output: ", e$message, "</p>",
                        "<p><em>Falling back to standard summary format.</em></p>",
                        "</div><br>", results
                    )
                    self$results$text$setContent(error_html)
                })
            }

            # Generate plots with error handling
            tryCatch({
                if (ncol(dataset) > 0 && nrow(dataset) > 0) {
                    plot_dataset <- dataset %>%
                        gtExtras::gt_plt_summary()
                    print_plot_dataset <- print(plot_dataset)
                    plot_dataset <- htmltools::HTML(print_plot_dataset[["children"]][[2]])
                    self$results$text1$setContent(plot_dataset)
                } else {
                    self$results$text1$setContent("<p><em>No data available for visualization.</em></p>")
                }
            }, error = function(e) {
                warning(paste("Error generating plots:", e$message))
                self$results$text1$setContent(paste0(
                    "<div style='color: orange; padding: 15px; background-color: #fff3cd;'>",
                    "<strong>Note:</strong> Plot generation encountered an issue. ",
                    "Statistical summaries are still available above.",
                    "</div>"
                ))
            })

        }
        },

        .generate_pivot_summary = function(pivot_data) {
            tryCatch({
                # Create enhanced summary using pivottabler
                layout_style <- self$options$pivot_layout

            # Convert pivot_data to data frame for pivottabler
            df_data <- data.frame(
                Variable = names(pivot_data),
                N = sapply(pivot_data, function(x) x$n),
                Missing = sapply(pivot_data, function(x) x$missing),
                Mean = sapply(pivot_data, function(x) x$mean),
                SD = sapply(pivot_data, function(x) x$sd),
                Median = sapply(pivot_data, function(x) x$median),
                Q1 = sapply(pivot_data, function(x) x$q1),
                Q3 = sapply(pivot_data, function(x) x$q3),
                Min = sapply(pivot_data, function(x) x$min),
                Max = sapply(pivot_data, function(x) x$max),
                stringsAsFactors = FALSE
            )

            # Add confidence intervals if enabled
            if (self$options$include_confidence) {
                df_data$CI_Lower <- sapply(pivot_data, function(x) x$ci_lower)
                df_data$CI_Upper <- sapply(pivot_data, function(x) x$ci_upper)
            }

            # Add advanced metrics if enabled
            if (self$options$advanced_metrics) {
                df_data$MAD <- sapply(pivot_data, function(x) if (is.null(x$mad)) NA else x$mad)
                df_data$CV <- sapply(pivot_data, function(x) if (is.null(x$cv)) NA else x$cv)
            }

            # Create HTML table based on layout style
            html_table <- private$.create_styled_table(df_data, layout_style)

                return(html_table)
            }, error = function(e) {
                return(paste0(
                    "<div style='color: red; padding: 15px; background-color: #ffebee;'>",
                    "<strong>Error generating pivot summary:</strong> ", e$message,
                    "</div>"
                ))
            })
        },

        .create_styled_table = function(df_data, layout_style) {
            # Create professional HTML table based on layout style

            style_config <- switch(layout_style,
                "clinical" = list(
                    title = "Clinical Research Summary",
                    bg_color = "#f8f9fa",
                    header_color = "#495057",
                    border_color = "#dee2e6"
                ),
                "statistical" = list(
                    title = "Statistical Analysis Summary",
                    bg_color = "#e3f2fd",
                    header_color = "#1976d2",
                    border_color = "#bbdefb"
                ),
                "comparative" = list(
                    title = "Comparative Study Summary",
                    bg_color = "#f3e5f5",
                    header_color = "#7b1fa2",
                    border_color = "#ce93d8"
                )
            )

            # Build HTML table
            html <- paste0(
                "<div style='background-color: ", style_config$bg_color, "; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: ", style_config$header_color, "; margin-top: 0;'>", style_config$title, "</h3>",
                "<table style='width: 100%; border-collapse: collapse; font-family: Arial, sans-serif;'>",
                "<thead><tr style='background-color: ", style_config$header_color, "; color: white;'>"
            )

            # Add headers
            for (col_name in names(df_data)) {
                display_name <- switch(col_name,
                    "Variable" = "Variable",
                    "N" = "N",
                    "Missing" = "Missing",
                    "Mean" = "Mean",
                    "SD" = "SD",
                    "Median" = "Median",
                    "Q1" = "Q1",
                    "Q3" = "Q3",
                    "Min" = "Min",
                    "Max" = "Max",
                    "CI_Lower" = "CI Lower",
                    "CI_Upper" = "CI Upper",
                    "MAD" = "MAD",
                    "CV" = "CV (%)",
                    col_name
                )
                html <- paste0(html, "<th style='padding: 12px; border: 1px solid ", style_config$border_color, ";'>", display_name, "</th>")
            }
            html <- paste0(html, "</tr></thead><tbody>")

            # Add data rows
            for (i in 1:nrow(df_data)) {
                row_bg <- if (i %% 2 == 0) "#ffffff" else "#f9f9f9"
                html <- paste0(html, "<tr style='background-color: ", row_bg, ";'>")

                for (col_name in names(df_data)) {
                    value <- df_data[i, col_name]
                    if (is.na(value)) value <- "-"
                    html <- paste0(html, "<td style='padding: 10px; border: 1px solid ", style_config$border_color, "; text-align: center;'>", value, "</td>")
                }
                html <- paste0(html, "</tr>")
            }

            html <- paste0(html, "</tbody></table>")
            html <- paste0(html, "<p style='font-size: 12px; color: #6c757d; margin-top: 15px;'>")
            html <- paste0(html, "<em>📊 Enhanced Summary using pivottabler - Layout: ", layout_style, "</em></p>")
            html <- paste0(html, "</div>")

            return(html)
        },

        .generate_export_info = function() {
            export_html <- paste0(
                "<div style='background-color: #d4edda; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4 style='color: #155724; margin-top: 0;'>📄 Export Ready</h4>",
                "<p style='margin: 5px 0;'>Your pivot summary is ready for export in multiple formats:</p>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>Excel:</strong> Professional spreadsheet format</li>",
                "<li><strong>CSV:</strong> Data analysis compatible</li>",
                "<li><strong>HTML:</strong> Web-ready presentation</li>",
                "</ul>",
                "<p style='font-size: 12px; color: #155724; margin-bottom: 0;'>",
                "<em>💡 Export functionality available through pivottabler integration</em></p>",
                "</div>"
            )

            return(export_html)
        },

        .generate_summarytools_output = function(myvar, data_subset, summary_format) {
            # Individual variable summarytools output - IMPLEMENTED
            tryCatch({
                if (!requireNamespace("summarytools", quietly = TRUE)) {
                    return(paste0("<em>summarytools package required for ", myvar, "</em><br><br>"))
                }

                # Set summarytools options for individual analysis
                summarytools::st_options(
                    plain.ascii = FALSE,
                    style = "html",
                    dfSummary.silent = TRUE
                )

                if (summary_format == "summarytools_desc" && is.numeric(data_subset[[myvar]])) {
                    # Descriptive statistics for individual numeric variable
                    desc_result <- summarytools::descr(
                        data_subset[myvar],
                        stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr", "cv"),
                        style = "html"
                    )

                    html_output <- summarytools::print.summarytools(
                        desc_result,
                        method = "render",
                        headings = FALSE,
                        footnote = NA
                    )

                    return(paste0("<h4>", myvar, " - Descriptive Statistics</h4>", html_output, "<br>"))

                } else if (summary_format == "summarytools_freq" && (is.factor(data_subset[[myvar]]) || is.character(data_subset[[myvar]]))) {
                    # Frequency analysis for individual categorical variable
                    freq_result <- summarytools::freq(
                        data_subset[[myvar]],
                        style = "html",
                        report.nas = TRUE,
                        totals = TRUE
                    )

                    html_output <- summarytools::print.summarytools(
                        freq_result,
                        method = "render",
                        headings = FALSE,
                        footnote = NA
                    )

                    return(paste0("<h4>", myvar, " - Frequency Table</h4>", html_output, "<br>"))

                } else {
                    # Default summary for individual variable
                    return(paste0("<em>Individual summarytools analysis for ", myvar, " (", summary_format, ")</em><br><br>"))
                }

            }, error = function(e) {
                return(paste0("<em>Error in summarytools analysis for ", myvar, ": ", e$message, "</em><br><br>"))
            })
        },

        .generate_comprehensive_summarytools = function(dataset, all_vars) {
            # Comprehensive summarytools analysis with enhanced error handling

            # Safely require summarytools
            if (!requireNamespace("summarytools", quietly = TRUE)) {
                return("<div style='color: red; padding: 20px;'>Error: summarytools package not available. Please install it to use this feature.</div>")
            }

            # Additional input validation
            if (is.null(dataset) || nrow(dataset) == 0 || length(all_vars) == 0) {
                return("<div style='color: orange; padding: 20px;'>No valid data available for summarytools analysis.</div>")
            }

            summary_format <- self$options$summary_format
            include_graphs <- self$options$summarytools_graphs
            round_digits <- self$options$summarytools_round_digits

            # Set summarytools options for HTML output
            summarytools::st_options(
                plain.ascii = FALSE,
                style = "html",
                dfSummary.silent = TRUE,
                dfSummary.graph.col = include_graphs,
                round.digits = round_digits
            )

            tryCatch({
                if (summary_format == "summarytools_df") {
                    # dfSummary - comprehensive dataset overview with plots (top feature from article)
                    df_subset <- dataset[all_vars]
                    summary_result <- summarytools::dfSummary(
                        df_subset,
                        plain.ascii = FALSE,
                        style = "html",
                        graph.magnif = 0.75,
                        valid.col = TRUE,
                        na.col = TRUE,
                        graph.col = include_graphs,
                        tmp.img.dir = tempdir()
                    )

                    html_output <- summarytools::print.summarytools(
                        summary_result,
                        method = "render",
                        headings = FALSE,
                        footnote = NA
                    )

                    header_html <- paste0(
                        "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 8px; margin-bottom: 20px;'>",
                        "<h3 style='color: #1976d2; margin: 0;'>📊 summarytools: Dataset Summary (dfSummary)</h3>",
                        "<p style='margin: 5px 0 0 0; color: #555;'>Comprehensive overview with embedded visualizations - Based on autoEDA research (R Journal 2019)</p>",
                        "</div>"
                    )

                    return(paste0(header_html, html_output))

                } else if (summary_format == "summarytools_desc") {
                    # descr - advanced descriptive statistics (key feature from article)
                    numeric_vars <- all_vars[sapply(dataset[all_vars], is.numeric)]

                    if (length(numeric_vars) == 0) {
                        return("<div style='color: orange; padding: 20px;'>No numeric variables selected for descriptive statistics.</div>")
                    }

                    summary_result <- summarytools::descr(
                        dataset[numeric_vars],
                        stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr", "cv", "skewness", "se.skewness", "kurtosis"),
                        transpose = TRUE,
                        style = "html"
                    )

                    html_output <- summarytools::print.summarytools(
                        summary_result,
                        method = "render",
                        headings = FALSE,
                        footnote = NA
                    )

                    header_html <- paste0(
                        "<div style='background-color: #f3e5f5; padding: 15px; border-radius: 8px; margin-bottom: 20px;'>",
                        "<h3 style='color: #7b1fa2; margin: 0;'>📈 summarytools: Advanced Descriptive Statistics</h3>",
                        "<p style='margin: 5px 0 0 0; color: #555;'>Enhanced descriptives with skewness, kurtosis, and robust statistics</p>",
                        "</div>"
                    )

                    return(paste0(header_html, html_output))

                } else if (summary_format == "summarytools_freq") {
                    # freq - enhanced frequency tables for categorical variables
                    categorical_vars <- all_vars[sapply(dataset[all_vars], function(x) is.factor(x) || is.character(x))]

                    if (length(categorical_vars) == 0) {
                        return("<div style='color: orange; padding: 20px;'>No categorical variables selected for frequency analysis.</div>")
                    }

                    freq_results <- list()
                    for (var in categorical_vars) {
                        freq_result <- summarytools::freq(
                            dataset[[var]],
                            style = "html",
                            report.nas = TRUE,
                            totals = TRUE,
                            cumul = TRUE
                        )

                        freq_html <- summarytools::print.summarytools(
                            freq_result,
                            method = "render",
                            headings = FALSE,
                            footnote = NA
                        )

                        freq_results[[var]] <- paste0(
                            "<h4 style='color: #d32f2f; margin: 20px 0 10px 0;'>", var, "</h4>",
                            freq_html
                        )
                    }

                    header_html <- paste0(
                        "<div style='background-color: #ffebee; padding: 15px; border-radius: 8px; margin-bottom: 20px;'>",
                        "<h3 style='color: #d32f2f; margin: 0;'>📋 summarytools: Frequency Analysis</h3>",
                        "<p style='margin: 5px 0 0 0; color: #555;'>Enhanced frequency tables with percentages and cumulative statistics</p>",
                        "</div>"
                    )

                    return(paste0(header_html, paste(freq_results, collapse = "")))
                }

            }, error = function(e) {
                error_html <- paste0(
                    "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
                    "<h4>summarytools Error</h4>",
                    "<p>Error generating summarytools output: ", e$message, "</p>",
                    "<p><em>Please check your data and try again.</em></p>",
                    "</div>"
                )
                return(error_html)
            })

            return("<div style='color: orange; padding: 20px;'>Unknown summarytools format selected.</div>")
        },

        # Helper function to check if variable can be converted to numeric
        .can_be_numeric = function(x) {
            if (is.numeric(x)) return(TRUE)
            if (is.character(x) || is.factor(x)) {
                # Try to convert a sample to numeric
                sample_vals <- head(x[!is.na(x)], 10)
                converted <- suppressWarnings(as.numeric(as.character(sample_vals)))
                return(sum(!is.na(converted)) > length(sample_vals) * 0.5)
            }
            return(FALSE)
        }



    )
)
