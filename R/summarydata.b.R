#' @title Summary of Continuous Variables with Distribution Diagnostics
#' @return Text and an HTML summary table (with optional distribution diagnostics)
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gt gt
#' @importFrom htmltools HTML
#' @importFrom gtExtras gt_plt_summary
#' @import moments
#' @importFrom lubridate ymd ymd_hms dmy dmy_hms mdy mdy_hms ydm parse_date_time is.Date

summarydataClass <- if (requireNamespace("jmvcore")) R6::R6Class("summarydataClass",
    inherit = summarydataBase, private = list(.run = function() {


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
                
                # Validate each date variable
                for (date_var in date_var_list) {
                    validation_result <- validate_and_parse_dates(dataset[[date_var]])
                    
                    if (validation_result$valid) {
                        valid_date_vars <- c(valid_date_vars, date_var)
                        date_parse_info[[date_var]] <- validation_result
                        # Update dataset with parsed dates
                        dataset[[date_var]] <- validation_result$parsed
                    } else {
                        warning(paste("Variable", date_var, "could not be parsed as a valid date format"))
                    }
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
                    # Handle date variable
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
                } else {
                    # Handle numeric variable (original logic)
                    numeric_data <- jmvcore::toNumeric(data_subset[[myvar]])
                    
                    # Original statistics (preserved for backward compatibility)
                    mean_x <- round(mean(numeric_data, na.rm = TRUE), digits = 1)
                    sd_x <- round(sd(numeric_data, na.rm = TRUE), digits = 1)
                median_x <- round(median(numeric_data, na.rm = TRUE), digits = 1)
                min_x <- round(min(numeric_data, na.rm = TRUE), digits = 1)
                max_x <- round(max(numeric_data, na.rm = TRUE), digits = 1)
                
                # Enhanced sumvar-style statistics (when option is enabled)
                if (self$options$sumvar_style) {
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

                # Generate output based on selected options
                if (self$options$sumvar_style) {
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
                } else {
                    # Original output format (preserved)
                    return(paste0("Mean of <strong>", myvar, "</strong> is: ", mean_x, " \U00B1 ", sd_x,
                                 ". (Median: ", median_x, " [Min: ", min_x, " - ", "Max: ",
                                 max_x, "]) <br>", dist_text, "<br><br>", collapse = " "))
                }
                
                } # End of numeric variable handling

            }

            # Handle grouping if specified
            if (!is.null(self$options$grvar) && length(self$options$grvar) > 0) {
                group_var <- self$options$grvar
                
                # Split analysis by group
                group_results <- list()
                group_levels <- levels(as.factor(dataset[[group_var]]))
                
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
                
            } else {
                # Original ungrouped analysis - handle both numeric and date variables
                results <- purrr::map(.x = all_vars, .f = mysummary)
                results <- unlist(results)
                results <- paste(results, collapse = "")
            }
            
            self$results$text$setContent(results)


            plot_dataset <- dataset %>%
                gtExtras::gt_plt_summary()
            print_plot_dataset <- print(plot_dataset)
            plot_dataset <- htmltools::HTML(print_plot_dataset[["children"]][[2]])
            self$results$text1$setContent(plot_dataset)

        }


    }))
