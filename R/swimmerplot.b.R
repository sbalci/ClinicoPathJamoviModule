#' @title Swimmer Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import lubridate
#'
swimmerplotClass <- if(requireNamespace("jmvcore")) R6::R6Class(
    "swimmerplotClass",
    inherit = swimmerplotBase,
    private = list(

        .parseDates = function(dates, format) {
            # Function to parse dates based on format
            parsed <- try({
                switch(format,
                       "ymdhms" = lubridate::ymd_hms(dates),
                       "ymd" = lubridate::ymd(dates),
                       "ydm" = lubridate::ydm(dates),
                       "mdy" = lubridate::mdy(dates),
                       "myd" = lubridate::myd(dates),
                       "dmy" = lubridate::dmy(dates),
                       "dym" = lubridate::dym(dates),
                       lubridate::ymd(dates)  # default to ymd
                )
            }, silent = TRUE)

            if (inherits(parsed, "try-error")) {
                stop("Error parsing dates. Please check your date format.")
            }

            return(parsed)
        },

        .processTimes = function(start_data, end_data) {
            if (self$options$timetype == "raw") {
                # For raw numeric input
                start_times <- jmvcore::toNumeric(start_data)
                end_times <- jmvcore::toNumeric(end_data)
                durations <- end_times - start_times

                # Convert to requested output unit
                unit_multiplier <- switch(self$options$timetypeoutput,
                                          "days" = 1,
                                          "weeks" = 1/7,
                                          "months" = 1/30.44,
                                          "years" = 1/365.25,
                                          1)
                durations <- durations * unit_multiplier
            } else {
                # Parse dates
                start_times <- private$.parseDates(as.character(start_data),
                                                   self$options$timetypedata)
                end_times <- private$.parseDates(as.character(end_data),
                                                 self$options$timetypedata)

                # Always calculate durations from original dates
                intervals <- lubridate::interval(start_times, end_times)
                durations <- lubridate::time_length(intervals, unit = self$options$timetypeoutput)

                if (self$options$startType == "relative") {
                    # For display purposes only
                    display_start <- rep(0, length(durations))
                    display_end <- durations

                    return(list(
                        start = display_start,
                        end = display_end,
                        durations = durations  # Original durations preserved
                    ))
                } else {
                    return(list(
                        start = start_times,
                        end = end_times,
                        durations = durations  # Original durations preserved
                    ))
                }
            }

            return(list(
                start = start_times,
                end = end_times,
                durations = durations
            ))
        },

        .calculateDuration = function(start, end, unit) {
            # Create an interval and calculate duration in specified unit
            interval <- lubridate::interval(start, end)
            lubridate::time_length(interval, unit = unit)
        },

        .createIntervals = function(starts, ends) {
            # Create intervals for vector of start and end times
            intervals <- lubridate::interval(starts, ends)
            return(intervals)
        },

        .calculateStats = function(df) {
            if (self$options$timetype == "datetime") {
                if (self$options$startType == "absolute") {
                    # For absolute times, calculate intervals directly from dates
                    intervals <- lubridate::interval(df$Start, df$End)
                } else {
                    # For relative times, we already have the durations
                    # Don't recalculate from displayed values
                    return(private$.calculateStatsFromDurations(df$End))  # End contains durations
                }
                # Calculate durations in requested unit
                durations <- lubridate::time_length(intervals, unit = self$options$timetypeoutput)
                return(private$.calculateStatsFromDurations(durations))
            } else {
                # For raw numeric data
                durations <- df$End - df$Start
                return(private$.calculateStatsFromDurations(durations))
            }
        },

        .calculateStatsFromDurations = function(durations) {
            stats <- list(
                median = median(durations, na.rm=TRUE),
                mean = mean(durations, na.rm=TRUE),
                sd = sd(durations, na.rm=TRUE),
                min = min(durations, na.rm=TRUE),
                max = max(durations, na.rm=TRUE),
                range = diff(range(durations, na.rm=TRUE))
            )
            return(stats)
        },

        .run = function() {
            if (is.null(self$options$patientID) ||
                is.null(self$options$start) ||
                is.null(self$options$end)) {

                todo <- "
        <br>Welcome to ClinicoPath Swimmer Plot Analysis
        <br><br>
        This tool creates a swimmer plot to visualize patient timelines:
        <br><br>
        <b>Required variables:</b>
        <br>- Patient ID: Unique identifier for each patient
        <br>- Start Time: When observation/treatment began
        <br>- End Time: When observation/treatment ended
        <br><br>
        <b>Optional variables:</b>
        <br>- Event Type: Clinical events (e.g., CR, PR, PD)
        <br>- Sort Variable: Any variable to sort the timelines
        <br><br>
        <b>Time format options:</b>
        <br>Select the appropriate time format in your data and desired output unit
        <hr>
        "
                self$results$todo$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")

            # Parse dates using selected format
            data <- self$data



            # Process time data
            time_data <- private$.processTimes(
                data[[self$options$start]],
                data[[self$options$end]]
            )

            # Create base dataframe
            df <- data.frame(
                Patient = data[[self$options$patientID]],
                Start = if(self$options$startType == "relative") 0 else time_data$start,
                End = if(self$options$startType == "relative") time_data$durations
                else time_data$end
            )

            # Add events if specified
            if (!is.null(self$options$event)) {
                df$Event <- as.factor(data[[self$options$event]])
            }

            # Sort data if sort variable specified
            if (!is.null(self$options$sortVariable)) {
                sort_values <- data[[self$options$sortVariable]]
                df <- df[order(sort_values, decreasing = TRUE),]
                df$Patient <- factor(df$Patient, levels = df$Patient)
            } else {
                # Default sort by duration
                durations <- df$End - df$Start
                df$Patient <- factor(df$Patient,
                                     levels = df$Patient[order(durations,
                                                               decreasing = TRUE)])
            }

            # Calculate summary statistics
            stats <- private$.calculateStats(df)

            # Update summary table
            self$results$summary$addRow(rowKey=1, values=list(
                metric = "Median Duration",
                value = stats$median
            ))
            self$results$summary$addRow(rowKey=2, values=list(
                metric = "Mean Duration",
                value = stats$mean
            ))
            self$results$summary$addRow(rowKey=3, values=list(
                metric = "Standard Deviation",
                value = stats$sd
            ))
            self$results$summary$addRow(rowKey=4, values=list(
                metric = "Range",
                value = stats$range
            ))
            self$results$summary$addRow(rowKey=5, values=list(
                metric = "Minimum",
                value = stats$min
            ))
            self$results$summary$addRow(rowKey=6, values=list(
                metric = "Maximum",
                value = stats$max
            ))

            # Save plot data
            plotData <- list(
                data = df,
                options = list(
                    timeUnit = self$options$timetypeoutput,
                    barHeight = self$options$barHeight,
                    timeType = self$options$timetype,
                    startType = self$options$startType
                ),
                stats = stats
            )

            image <- self$results$plot
            image$setState(plotData)


        },


        .plot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            if (is.null(plotData)) return()

            df <- plotData$data
            opts <- plotData$options
            stats <- plotData$stats

            # Create base plot
            p <- ggplot2::ggplot(df, ggplot2::aes(
                x = Start,
                xend = End,
                y = Patient,
                yend = Patient
            )) +
                ggplot2::geom_segment(
                    size = opts$barHeight,
                    color = "steelblue"
                )

            # Configure x-axis based on time type
            if (opts$timeType == "datetime") {
                if (opts$startType == "absolute") {
                    # For absolute dates, use date breaks with rotated labels
                    p <- p + ggplot2::scale_x_date(
                        date_breaks = "3 months",  # Increased frequency of breaks
                        date_labels = "%Y-%m"
                    ) +
                        ggplot2::theme(
                            axis.text.x = ggplot2::element_text(
                                angle = 45,  # Diagonal labels
                                hjust = 1,   # Align to right edge
                                vjust = 1    # Align vertically
                            )
                        )
                } else {
                    # For relative times, use numeric breaks
                    p <- p + ggplot2::scale_x_continuous(
                        breaks = scales::pretty_breaks(n = 6)
                    )
                }
            } else {
                # For raw numeric data
                p <- p + ggplot2::scale_x_continuous(
                    breaks = scales::pretty_breaks(n = 6)
                )
            }

            # Add labels
            p <- p + ggplot2::labs(
                x = paste0("Time (", opts$timeUnit, ")"),
                y = "Patient ID",
                title = "Patient Timeline Analysis",
                subtitle = sprintf(
                    "%s data (%s start): Median duration: %.1f %s (Range: %.1f - %.1f)",
                    ifelse(opts$timeType == "raw", "Raw", "Date/time"),
                    ifelse(opts$startType == "relative", "relative", "absolute"),
                    stats$median,
                    opts$timeUnit,
                    stats$min,
                    stats$max
                )
            )

            # Add event markers if present
            if ("Event" %in% names(df)) {
                p <- p +
                    ggplot2::geom_point(
                        mapping = ggplot2::aes(
                            x = End,
                            color = Event
                        ),
                        size = 3
                    ) +
                    ggplot2::scale_color_brewer(palette = "Set1")
            }

            # Add theme with rotated x-axis labels
            p <- p +
                ggtheme +
                ggplot2::theme(
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor.y = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(
                        angle = 45,
                        hjust = 1,
                        vjust = 1
                    )
                )

            print(p)
            TRUE
        }






            )
)
