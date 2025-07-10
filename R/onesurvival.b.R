# from https://github.com/AlbertoAlvarezIglesias2019/SimpleSurvival

#' @title Kaplan-Meier Survival Analysis for Single Group
#' @description
#' Performs Kaplan-Meier survival analysis for a single group of subjects.
#' This function estimates survival probabilities over time and provides
#' summary statistics including median survival time with confidence intervals.
#' It generates both tabular results and Kaplan-Meier survival plots.
#'
#' @details
#' The function performs the following analyses:
#' \itemize{
#'   \item Kaplan-Meier survival estimation using survival package
#'   \item Calculation of median survival time with 95% confidence intervals
#'   \item Summary statistics (number of subjects, number of events)
#'   \item Kaplan-Meier survival curve visualization
#'   \item Optional confidence interval bands on plots
#' }
#'
#' The function requires two variables:
#' \itemize{
#'   \item Time variable: Time to event or censoring (continuous, positive values)
#'   \item Status variable: Event indicator (0 = censored, 1 = event occurred)
#' }
#'
#' @section Data Requirements:
#' For valid survival analysis, ensure:
#' \itemize{
#'   \item Time values are non-negative and numeric
#'   \item Status values are binary (0/1 or equivalent)
#'   \item At least some events have occurred (not all censored)
#'   \item Sufficient sample size for reliable estimates
#' }
#'
#' @examples
#' \dontrun{
#' # Basic survival analysis
#' result <- oneSurvival(
#'   data = lung_data,
#'   times = "survival_time",
#'   status = "death_indicator"
#' )
#'
#' # With confidence intervals and time units
#' result <- oneSurvival(
#'   data = clinical_data,
#'   times = "days_to_event",
#'   status = "event_occurred",
#'   ciyn = TRUE,
#'   timeunits = "Days"
#' )
#' }
#'
#' @references
#' Kaplan, E. L., & Meier, P. (1958). Nonparametric estimation from incomplete observations.
#' Journal of the American Statistical Association, 53(282), 457-481.
#'
#' @author ClinicoPath Development Team
#' @seealso \code{\link{survival}}, \code{\link{survfit}}
#'
#' @importFrom R6 R6Class
#' @import jmvcore

oneSurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "oneSurvivalClass",
    inherit = oneSurvivalBase,
    private = list(

        # Enhanced input validation for survival data quality and user inputs
        # Returns validation results with errors, warnings, and informational messages
        .validateInputs = function(data, time_var, status_var) {
            validation_results <- list(
                errors = character(0),
                warnings = character(0),
                info = character(0),
                should_stop = FALSE
            )
            
            # 1. Check if variables exist in data
            if (!is.null(time_var) && !time_var %in% names(data)) {
                validation_results$errors <- c(validation_results$errors,
                    paste("Time variable '", time_var, "' not found in dataset.", sep=""))
                validation_results$should_stop <- TRUE
            }
            
            if (!is.null(status_var) && !status_var %in% names(data)) {
                validation_results$errors <- c(validation_results$errors,
                    paste("Status variable '", status_var, "' not found in dataset.", sep=""))
                validation_results$should_stop <- TRUE
            }
            
            # Stop here if variables don't exist
            if (validation_results$should_stop) {
                return(validation_results)
            }
            
            # 2. Time variable validation
            if (!is.null(time_var) && time_var %in% names(data)) {
                time_data <- data[[time_var]]
                time_data_clean <- time_data[!is.na(time_data)]
                
                if (length(time_data_clean) == 0) {
                    validation_results$errors <- c(validation_results$errors,
                        "Time variable contains no non-missing values.")
                    validation_results$should_stop <- TRUE
                } else {
                    # Check if time data is numeric
                    if (!is.numeric(time_data_clean)) {
                        validation_results$errors <- c(validation_results$errors,
                            "Time variable must be numeric (representing time to event or censoring).")
                        validation_results$should_stop <- TRUE
                    } else {
                        # Check for negative times
                        negative_times <- sum(time_data_clean < 0, na.rm = TRUE)
                        if (negative_times > 0) {
                            validation_results$errors <- c(validation_results$errors,
                                paste("Time variable contains ", negative_times, " negative values. Survival times must be non-negative.", sep=""))
                            validation_results$should_stop <- TRUE
                        }
                        
                        # Check for zero times (potential issue)
                        zero_times <- sum(time_data_clean == 0, na.rm = TRUE)
                        if (zero_times > 0) {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste("Time variable contains ", zero_times, " zero values. Consider if these represent immediate events or data errors.", sep=""))
                        }
                        
                        # Time range information
                        time_range <- range(time_data_clean, na.rm = TRUE)
                        validation_results$info <- c(validation_results$info,
                            paste("Time range: ", round(time_range[1], 2), " to ", round(time_range[2], 2), sep=""))
                    }
                }
            }
            
            # 3. Status variable validation
            if (!is.null(status_var) && status_var %in% names(data)) {
                status_data <- data[[status_var]]
                status_data_clean <- status_data[!is.na(status_data)]
                
                if (length(status_data_clean) == 0) {
                    validation_results$errors <- c(validation_results$errors,
                        "Status variable contains no non-missing values.")
                    validation_results$should_stop <- TRUE
                } else {
                    # Check unique values in status
                    unique_status <- unique(status_data_clean)
                    status_counts <- table(status_data_clean)
                    
                    if (length(unique_status) < 2) {
                        if (all(unique_status == 0) || all(unique_status == FALSE)) {
                            validation_results$errors <- c(validation_results$errors,
                                "All subjects are censored (status = 0). Survival analysis requires at least some events.")
                            validation_results$should_stop <- TRUE
                        } else if (all(unique_status == 1) || all(unique_status == TRUE)) {
                            validation_results$warnings <- c(validation_results$warnings,
                                "All subjects experienced the event (no censoring). Results may be limited.")
                        } else {
                            validation_results$errors <- c(validation_results$errors,
                                "Status variable must have at least 2 different values (event vs. censored).")
                            validation_results$should_stop <- TRUE
                        }
                    } else if (length(unique_status) > 2) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Status variable has ", length(unique_status), " unique values: ", paste(unique_status, collapse=", "), ". Expected binary (0/1). Will convert to binary.", sep=""))
                    }
                    
                    # Check if status can be converted to binary
                    if (!validation_results$should_stop) {
                        # Attempt to identify binary pattern
                        if (all(unique_status %in% c(0, 1))) {
                            event_count <- sum(status_data_clean == 1, na.rm = TRUE)
                            censored_count <- sum(status_data_clean == 0, na.rm = TRUE)
                            validation_results$info <- c(validation_results$info,
                                paste("Events: ", event_count, ", Censored: ", censored_count, " (", round(event_count/(event_count + censored_count) * 100, 1), "% event rate)", sep=""))
                        } else if (all(unique_status %in% c(FALSE, TRUE))) {
                            event_count <- sum(status_data_clean == TRUE, na.rm = TRUE)
                            censored_count <- sum(status_data_clean == FALSE, na.rm = TRUE)
                            validation_results$info <- c(validation_results$info,
                                paste("Events: ", event_count, ", Censored: ", censored_count, " (", round(event_count/(event_count + censored_count) * 100, 1), "% event rate)", sep=""))
                        } else {
                            validation_results$warnings <- c(validation_results$warnings,
                                "Status variable values don't follow standard 0/1 or FALSE/TRUE pattern. Will attempt automatic conversion.")
                        }
                    }
                }
            }
            
            # 4. Combined data checks
            if (!validation_results$should_stop && !is.null(time_var) && !is.null(status_var)) {
                complete_cases <- complete.cases(data[, c(time_var, status_var)])
                total_rows <- nrow(data)
                complete_rows <- sum(complete_cases)
                missing_proportion <- (total_rows - complete_rows) / total_rows
                
                if (missing_proportion > 0.1) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Large amount of missing data: ", round(missing_proportion * 100, 1), "% of rows will be removed (", total_rows - complete_rows, " out of ", total_rows, " rows).", sep=""))
                } else if (missing_proportion > 0) {
                    validation_results$info <- c(validation_results$info,
                        paste("Missing data: ", round(missing_proportion * 100, 1), "% of rows will be removed (", total_rows - complete_rows, " out of ", total_rows, " rows).", sep=""))
                }
                
                if (complete_rows < 10) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Very small sample size: ", complete_rows, " complete observations. Results may be unreliable.", sep=""))
                } else if (complete_rows < 30) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Small sample size: ", complete_rows, " complete observations. Consider larger sample for more reliable estimates.", sep=""))
                }
            }
            
            return(validation_results)
        },

        # Safely converts status variable to binary numeric format
        # Handles various input formats and provides clear feedback on conversion
        .convertStatusToBinary = function(status_data, original_var_name) {
            if (is.null(status_data) || length(status_data) == 0) {
                return(NULL)
            }
            
            # Remove missing values for analysis
            non_missing_data <- status_data[!is.na(status_data)]
            unique_vals <- unique(non_missing_data)
            
            # Standard binary conversions
            if (all(unique_vals %in% c(0, 1))) {
                # Already 0/1 - convert to numeric if needed
                return(as.numeric(status_data))
            } else if (all(unique_vals %in% c(FALSE, TRUE))) {
                # TRUE/FALSE - convert to 1/0
                return(as.numeric(status_data))
            } else if (all(unique_vals %in% c("0", "1"))) {
                # String "0"/"1" - convert to numeric
                return(as.numeric(as.character(status_data)))
            } else if (length(unique_vals) == 2) {
                # Two unique values - assume higher value is event
                sorted_vals <- sort(unique_vals)
                result <- ifelse(status_data == sorted_vals[2], 1, 0)
                warning(paste("Status variable '", original_var_name, "' converted: '", sorted_vals[1], "' → 0 (censored), '", sorted_vals[2], "' → 1 (event)", sep=""))
                return(result)
            } else {
                # Multiple values - try to convert to numeric and use threshold
                numeric_attempt <- suppressWarnings(as.numeric(as.character(status_data)))
                if (!all(is.na(numeric_attempt))) {
                    # Use 0 as censored, anything > 0 as event
                    result <- ifelse(numeric_attempt > 0, 1, 0)
                    warning(paste("Status variable '", original_var_name, "' converted: 0 → censored, >0 → event", sep=""))
                    return(result)
                } else {
                    stop(paste("Cannot convert status variable '", original_var_name, "' to binary format. Please ensure it contains binary values (0/1, TRUE/FALSE) or contact support.", sep=""))
                }
            }
        },

        .run = function() {

            # Initial Message when no variables selected
            if (is.null(self$options$times) || is.null(self$options$status)) {
                
                todo <- glue::glue("
                    <br>Welcome to ClinicoPath Survival Analysis
                    <br><br>
                        This tool performs Kaplan-Meier survival analysis for a single group.
                    <br><br>
                        <b>Required variables:</b>
                        <br>• Time variable: Time to event or censoring (positive numeric values)
                        <br>• Status variable: Event indicator (0 = censored, 1 = event occurred)
                    <br><br>
                        <b>What you'll get:</b>
                        <br>• Median survival time with 95% confidence intervals
                        <br>• Number of subjects and events
                        <br>• Kaplan-Meier survival curve
                    <br><br>
                        <b>Data requirements:</b>
                        <br>• Time values must be non-negative
                        <br>• Status should be binary (0/1, TRUE/FALSE, or similar)
                        <br>• At least some events must have occurred
                    <br><br>
                        This function uses the survival package for analysis.
                    <br><br>
                    ")

                html <- self$results$text
                html$setContent(todo)
                return()
            }

            # Check for empty data
            if (nrow(self$data) == 0) {
                error_msg <- paste(
                    "<div style='background-color: #f8d7da; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                    "<b>❌ Data Error:</b> No data available for survival analysis<br><br>",
                    "<b>💡 Possible reasons:</b><br>",
                    "• Dataset has no rows<br>",
                    "• All rows contain missing values<br>",
                    "• Data filtering has removed all observations<br><br>",
                    "<b>🔧 Solutions:</b><br>",
                    "• Check your data import process<br>",
                    "• Verify variable selections<br>",
                    "• Review data quality and missing value patterns<br>",
                    "• Ensure your dataset contains complete observations",
                    "</div>",
                    collapse = ""
                )
                stop(error_msg)
            }

            # Get variable names
            times <- self$options$times
            status <- self$options$status
            tmpDat <- self$data

            # Perform input validation
            validation_results <- private$.validateInputs(tmpDat, times, status)
            
            # Handle validation errors - stop execution if critical errors found
            if (validation_results$should_stop) {
                error_msg <- paste(
                    "<div style='background-color: #f8d7da; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                    "<b>❌ Critical Error(s) Detected:</b><br>",
                    paste(validation_results$errors, collapse = "<br>"),
                    "<br><br><b>💡 Suggestions:</b><br>",
                    "• Verify that selected variables exist in your dataset<br>",
                    "• Ensure time variable contains positive numeric values<br>",
                    "• Ensure status variable is binary (0/1) or can be converted<br>",
                    "• Check that at least some events have occurred (not all censored)",
                    "</div>",
                    collapse = ""
                )
                stop(error_msg)
            }

            # Create validation summary for display
            validation_summary <- ""
            if (length(validation_results$warnings) > 0) {
                validation_summary <- paste0(validation_summary, 
                    "<div style='background-color: #fff3cd; padding: 10px; margin: 10px 0; border-radius: 5px;'>",
                    "<b>⚠️ Warnings:</b><br>", 
                    paste(validation_results$warnings, collapse = "<br>"),
                    "</div>")
            }
            if (length(validation_results$info) > 0) {
                validation_summary <- paste0(validation_summary,
                    "<div style='background-color: #d1ecf1; padding: 10px; margin: 10px 0; border-radius: 5px;'>",
                    "<b>ℹ️ Information:</b><br>", 
                    paste(validation_results$info, collapse = "<br>"),
                    "</div>")
            }

            # Safely convert status variable to binary
            tmpDat[[status]] <- private$.convertStatusToBinary(tmpDat[[status]], status)

            # Remove missing values
            complete_cases <- complete.cases(tmpDat[, c(times, status)])
            tmpDat <- tmpDat[complete_cases, ]

            # Create survival formula
            form <- paste("survival::Surv(", times, ",", status, ") ~ 1")
            form <- as.formula(form)

            # Fit survival model with error handling
            tryCatch({
                fit <- survival::survfit(form, data = tmpDat)
                temp <- as.list(summary(fit)$table)
                
                # Populate results table
                table1 <- self$results$onesurvTable1
                table1$setRow(rowNo=1, values=list(
                  n=temp$records,
                  nevents = temp$events,
                  median=temp$median,
                  cilb=temp$`0.95LCL`,
                  ciub=temp$`0.95UCL`
                ))

                # Set plot data
                image <- self$results$onesurvPlot1
                image$setState(fit)

                # Display validation summary
                if (nchar(validation_summary) > 0) {
                    html <- self$results$text
                    html$setContent(validation_summary)
                }

            }, error = function(e) {
                detailed_error <- paste(
                    "<div style='background-color: #f8d7da; padding: 15px; border-radius: 8px; margin: 10px 0;'>",
                    "<b>🔧 Survival Analysis Error:</b><br>",
                    paste("Technical error:", e$message), "<br><br>",
                    "<b>💡 Common causes and solutions:</b><br>",
                    "• Invalid survival times: Check for negative or non-numeric time values<br>",
                    "• All subjects censored: Ensure some events have occurred<br>",
                    "• Data format issues: Verify time and status variables are correctly formatted<br>",
                    "• Insufficient data: Need adequate sample size for survival analysis<br><br>",
                    "<b>🔄 Suggested next steps:</b><br>",
                    "• Review your time and status variable definitions<br>",
                    "• Check data quality and completeness<br>",
                    "• Ensure proper coding of event indicator (0 = censored, 1 = event)<br>",
                    "• Contact support if problem persists",
                    "</div>",
                    collapse = ""
                )
                stop(detailed_error)
            })

          },
          .plot=function(image, ...) {

            times <- self$options$times
            status <- self$options$status
            tmpDat <- self$data

            if (is.null(times) || is.null(status))
              return()

            plotData <- image$state
            conf.int <- self$options$ciyn
            xlab <- ifelse(self$options$timeunits=="None","Time",paste("Time (",self$options$timeunits,")",sep=""))
            
            # Use public autoplot function instead of internal :::autoplot.survfit
            plot <- ggfortify::autoplot(plotData,
                                        xlab=xlab,
                                        ylab = "Survival Probabilities",
                                        conf.int = conf.int,
                                        yScale = "frac")
            print(plot)
            TRUE
          }

        )
)
