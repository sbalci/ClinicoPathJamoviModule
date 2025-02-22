#' @title Swimmer Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import lubridate
#'
swimmerplotClass <- if(requireNamespace("jmvcore")) R6::R6Class(
    "swimmerplotClass",
    inherit = swimmerplotBase,  # Ensure swimmerplotBase is defined appropriately
    private = list(

        # Parse dates from character input based on the provided format.
        .parseDates = function(dates, format) {
            # If dates are already Date or POSIXct, return them directly.
            if (inherits(dates, "Date") || inherits(dates, "POSIXct"))
                return(dates)

            parsed <- try({
                switch(format,
                       "ymdhms" = lubridate::ymd_hms(dates),
                       "ymd"    = lubridate::ymd(dates),
                       "ydm"    = lubridate::ydm(dates),
                       "mdy"    = lubridate::mdy(dates),
                       "myd"    = lubridate::myd(dates),
                       "dmy"    = lubridate::dmy(dates),
                       "dym"    = lubridate::dym(dates),
                       lubridate::ymd(dates)  # default to ymd
                )
            }, silent = TRUE)

            if (inherits(parsed, "try-error")) {
                stop("Error parsing dates. Please check your date format.")
            }
            return(parsed)
        },

        # Process the start and end time data.
        .processTimes = function(start_data, end_data) {
            if (self$options$timetype == "raw") {
                # For raw numeric input, convert to numbers.
                start_times <- jmvcore::toNumeric(start_data)
                end_times   <- jmvcore::toNumeric(end_data)

                # Validate that every end time is not earlier than its start time.
                if (any(end_times < start_times, na.rm = TRUE)) {
                    stop("Error: Some end times are earlier than start times.")
                }
                durations <- end_times - start_times

                # Convert durations using the appropriate unit multiplier.
                unit_multiplier <- switch(self$options$timetypeoutput,
                                          "days"   = 1,
                                          "weeks"  = 1/7,
                                          "months" = 1/30.44,
                                          "years"  = 1/365.25,
                                          1)
                durations <- durations * unit_multiplier
                return(list(
                    start = start_times,
                    end = end_times,
                    durations = durations
                ))
            } else {
                # For datetime input, first parse the dates.
                start_times <- private$.parseDates(as.character(start_data), self$options$timetypedata)
                end_times   <- private$.parseDates(as.character(end_data), self$options$timetypedata)

                # Validate that start dates come before end dates.
                if (any(end_times < start_times, na.rm = TRUE)) {
                    stop("Error: Some end dates occur before start dates.")
                }

                intervals <- lubridate::interval(start_times, end_times)
                durations <- lubridate::time_length(intervals, unit = self$options$timetypeoutput)

                if (self$options$startType == "relative") {
                    # For relative display, shift all start times to zero.
                    display_start <- rep(0, length(durations))
                    display_end   <- durations
                    return(list(
                        start = display_start,
                        end = display_end,
                        durations = durations
                    ))
                } else {
                    return(list(
                        start = start_times,
                        end = end_times,
                        durations = durations
                    ))
                }
            }
        },

        # Calculate summary statistics from a vector of durations.
        .calculateStatsFromDurations = function(durations) {
            stats <- list(
                median = median(durations, na.rm = TRUE),
                mean   = mean(durations, na.rm = TRUE),
                sd     = sd(durations, na.rm = TRUE),
                min    = min(durations, na.rm = TRUE),
                max    = max(durations, na.rm = TRUE),
                range  = diff(range(durations, na.rm = TRUE))
            )
            return(stats)
        },

        # Calculate summary stats based on the type of time input.
        .calculateStats = function(df) {
            if (self$options$timetype == "datetime") {
                if (self$options$startType == "absolute") {
                    intervals <- lubridate::interval(df$Start, df$End)
                } else {
                    # When relative, the 'End' column already represents duration.
                    return(private$.calculateStatsFromDurations(df$End))
                }
                durations <- lubridate::time_length(intervals, unit = self$options$timetypeoutput)
                return(private$.calculateStatsFromDurations(durations))
            } else {
                durations <- df$End - df$Start
                return(private$.calculateStatsFromDurations(durations))
            }
        },

        .run = function() {
            # Check that required options are provided.
            if (is.null(self$options$patientID) ||
                is.null(self$options$start) ||
                is.null(self$options$end)) {
                todo <- "
        <br>Welcome to ClinicoPath Swimmer Plot Analysis
        <br><br>
        This tool creates a swimmer plot to visualize patient timelines.
        <br><br>
        <b>Required variables:</b>
        <br>- Patient ID: Unique identifier for each patient.
        <br>- Start Time: When observation/treatment began.
        <br>- End Time: When observation/treatment ended.
        <br><br>
        <b>Optional variables:</b> Event Type, Sort Variable, Milestone settings, etc.
        <hr>"
                self$results$todo$setContent(todo)
                return()
            }

            if (nrow(self$data) == 0)
                stop("Data contains no (complete) rows")

            data <- self$data

            # Process the time columns (raw or datetime)
            time_data <- private$.processTimes(
                data[[self$options$start]],
                data[[self$options$end]]
            )

            # Construct a data frame for plotting.
            df <- data.frame(
                Patient = data[[self$options$patientID]],
                Start = if(self$options$startType == "relative") 0 else time_data$start,
                End   = if(self$options$startType == "relative") time_data$durations else time_data$end
            )

            # Optionally add event markers if provided.
            if (!is.null(self$options$event)) {
                df$Event <- as.factor(data[[self$options$event]])
            }

            # Sort the data: if sortVariable is provided, sort accordingly;
            # otherwise, sort by duration (descending).
            if (!is.null(self$options$sortVariable)) {
                sort_values <- data[[self$options$sortVariable]]
                df <- df[order(sort_values, decreasing = TRUE), ]
                df$Patient <- factor(df$Patient, levels = df$Patient)
            } else {
                durations <- df$End - df$Start
                df$Patient <- factor(df$Patient, levels = df$Patient[order(durations, decreasing = TRUE)])
            }

            # Calculate summary statistics for durations.
            stats <- private$.calculateStats(df)

            # Update the summary table result.
            self$results$summary$addRow(rowKey = 1, values = list(
                metric = "Median Duration",
                value = stats$median
            ))
            self$results$summary$addRow(rowKey = 2, values = list(
                metric = "Mean Duration",
                value = stats$mean
            ))
            self$results$summary$addRow(rowKey = 3, values = list(
                metric = "Standard Deviation",
                value = stats$sd
            ))
            self$results$summary$addRow(rowKey = 4, values = list(
                metric = "Range",
                value = stats$range
            ))
            self$results$summary$addRow(rowKey = 5, values = list(
                metric = "Minimum",
                value = stats$min
            ))
            self$results$summary$addRow(rowKey = 6, values = list(
                metric = "Maximum",
                value = stats$max
            ))

            # Prepare state for plotting.
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

            self$results$plot$setState(plotData)
        },

        .plot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            if (is.null(plotData)) return()

            df <- plotData$data
            opts <- plotData$options
            stats <- plotData$stats

            # Base plot: draw horizontal segments for each patient.
            p <- ggplot2::ggplot(df, ggplot2::aes(
                x = Start,
                xend = End,
                y = Patient,
                yend = Patient
            )) +
                ggplot2::geom_segment(size = opts$barHeight, color = "steelblue")

            # Configure x-axis scale based on timetype.
            if (opts$timeType == "datetime") {
                if (opts$startType == "absolute") {
                    p <- p + ggplot2::scale_x_date(
                        date_breaks = "3 months",
                        date_labels = "%Y-%m"
                    ) +
                        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
                } else {
                    p <- p + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
                }
            } else {
                p <- p + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
            }

            # Add labels and title.
            p <- p + ggplot2::labs(
                x = paste0("Time (", opts$timeUnit, ")"),
                y = "Patient ID",
                title = "Patient Timeline Analysis",
                subtitle = sprintf("%s data (%s start): Median duration: %.1f %s (Range: %.1f - %.1f)",
                                   ifelse(opts$timeType == "raw", "Raw", "Date/Time"),
                                   ifelse(opts$startType == "relative", "relative", "absolute"),
                                   stats$median, opts$timeUnit, stats$min, stats$max)
            )

            # Add event markers if an Event variable exists.
            if ("Event" %in% names(df)) {
                p <- p + ggplot2::geom_point(mapping = ggplot2::aes(x = End, color = Event), size = 3) +
                    ggplot2::scale_color_brewer(palette = "Set1")
            }

            # Add milestone events if provided.
            if (!is.null(self$options$milestoneVariable) &&
                !is.null(self$options$milestoneTime)) {
                milestones <- data.frame(
                    Event = self$data[[self$options$milestoneVariable]],
                    Time = self$data[[self$options$milestoneTime]]
                )
                p <- p + ggplot2::geom_vline(
                    data = milestones,
                    mapping = ggplot2::aes(xintercept = Time, color = Event),
                    linetype = "dashed",
                    alpha = 0.5
                ) +
                    ggplot2::scale_color_brewer(name = "Milestones", palette = "Set2")
            }

            # Add reference lines if the option is set.
            if (self$options$referenceLines != "none") {
                ref_line <- switch(self$options$referenceLines,
                                   "median" = stats$median,
                                   "protocol" = c(6, 12, 24),  # Consider making these customizable
                                   "custom" = self$options$customReferenceTime
                )
                p <- p + ggplot2::geom_vline(xintercept = ref_line, linetype = "dotted", color = "darkgray") +
                    ggplot2::annotate("text", x = ref_line, y = -1, label = paste(ref_line, opts$timeUnit),
                                      angle = 90, hjust = -0.2)
            }

            # Apply additional theme settings.
            p <- p + ggtheme +
                ggplot2::theme(
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor.y = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
                )

            print(p)
            TRUE
        }
    )
)
