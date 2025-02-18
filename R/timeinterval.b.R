#' @title Time Interval Calculator
#' @importFrom R6 R6Class
#' @import jmvcore
#'

timeintervalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "timeintervalClass",
    inherit = timeintervalBase,
    private = list(
        # Calculate time intervals ----
        .calculate_survival_time = function(data,
                                            dx_date = NULL,
                                            fu_date = NULL,
                                            time_format = "ymd",
                                            output_unit = "months",
                                            landmark_time = NULL) {

            # Input validation
            if (!is.data.frame(data)) {
                stop("Input must be a data frame")
            }

            # Create a copy of input data
            result_data <- data

                # Validate date columns exist
                if (!dx_date %in% names(data) || !fu_date %in% names(data)) {
                    stop("Specified date columns not found in data")
                }

                # Parse dates based on format
                date_parser <- switch(time_format,
                                      "ymdhms" = lubridate::ymd_hms,
                                      "ymd" = lubridate::ymd,
                                      "ydm" = lubridate::ydm,
                                      "mdy" = lubridate::mdy,
                                      "myd" = lubridate::myd,
                                      "dmy" = lubridate::dmy,
                                      "dym" = lubridate::dym,
                                      stop("Unsupported date format"))

                tryCatch({
                    start_dates <- date_parser(data[[dx_date]])
                    end_dates <- date_parser(data[[fu_date]])
                }, error = function(e) {
                    stop(paste("Error parsing dates:", e$message))
                })

                # Calculate intervals
                intervals <- lubridate::interval(start_dates, end_dates)

                # Convert to specified time unit
                calculated_time <- lubridate::time_length(intervals, output_unit)



            # Apply landmark analysis if specified
            if (!is.null(landmark_time)) {
                if (!is.numeric(landmark_time) || landmark_time < 0) {
                    stop("Landmark time must be a non-negative number")
                }

                # Filter out cases before landmark time and adjust times
                valid_cases <- calculated_time >= landmark_time
                calculated_time <- calculated_time - landmark_time
                result_data <- result_data[valid_cases, ]
                calculated_time <- calculated_time[valid_cases]
            }

            return(calculated_time)
        },

        # Run analysis ----
        .run = function() {
            # Validate required inputs
            if (is.null(self$options$dx_date) || is.null(self$options$fu_date)) {
                # Show initial message
                todo <- "
                    <br>Welcome to Time Interval Calculator
                    <br><br>
                    This tool helps you calculate time intervals from:
                    <br>- Pre-calculated time values
                    <br>- Date columns using various date formats
                    <br><br>
                    Please select your input method and variables."

                html <- self$results$todo
                html$setContent(todo)
                return()
            }

            # Calculate time intervals
            calculated_times <- private$.calculate_survival_time(
                data = self$data,
                dx_date = self$options$dx_date,
                fu_date = self$options$fu_date,
                time_format = self$options$time_format,
                output_unit = self$options$output_unit,
                landmark_time = self$options$landmark_time
            )

            # Add calculated times to results if requested
            if (self$options$add_times && !is.null(calculated_times)) {
                self$results$calculated_time$setRowNums(rownames(self$data))
                self$results$calculated_time$setValues(calculated_times)
            }

            # Generate summary statistics
            if (!is.null(calculated_times)) {
                summary_stats <- list(
                    n = length(calculated_times),
                    mean = mean(calculated_times, na.rm = TRUE),
                    median = median(calculated_times, na.rm = TRUE),
                    sd = sd(calculated_times, na.rm = TRUE),
                    min = min(calculated_times, na.rm = TRUE),
                    max = max(calculated_times, na.rm = TRUE),
                    missing = sum(is.na(calculated_times)),
                    negative = sum(calculated_times < 0, na.rm = TRUE)
                )

                # if (any(time_data$time < 0, na.rm = TRUE)) {
                #     warnings$negative_intervals <- "Negative intervals found (end date before start date) and removed"
                #     time_data$time[time_data$time < 0] <- NA
                # }

                # Apply landmark time if specified
                # if (!is.null(landmark)) {
                #     if (!is.numeric(landmark) || landmark < 0) {
                #         stop("Landmark time must be a non-negative number")
                #     }
                #
                #     original_n <- nrow(time_data)
                #     time_data <- time_data[time_data$time >= landmark, ]
                #     time_data$time <- time_data$time - landmark
                #
                #     if (nrow(time_data) < original_n) {
                #         warnings$landmark_filtering <- sprintf(
                #             "%d observations removed due to landmark time of %f %s",
                #             original_n - nrow(time_data),
                #             landmark,
                #             output_units
                #         )
                #     }
                # }



                # Create summary text
                summary_text <- glue::glue("
                    <br><b>Time Interval Summary ({self$options$output_unit})</b><br>
                    Number of observations: {summary_stats$n}<br>
                    Mean time: {round(summary_stats$mean, 2)}<br>
                    Median time: {round(summary_stats$median, 2)}<br>
                    Standard deviation: {round(summary_stats$sd, 2)}<br>
                    Range: {round(summary_stats$min, 2)} to {round(summary_stats$max, 2)}<br>
                    Missing values: {summary_stats$missing}<br>
                    {if(summary_stats$negative > 0) paste('Warning:', summary_stats$negative, 'negative time intervals detected') else ''}
                ")

                self$results$summary$setContent(summary_text)
            }
        }
    )
)
