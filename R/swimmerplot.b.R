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
        .processTimes = function(start_data, end_data) {
            if (self$options$timetype == "raw") {
                # For raw numeric input
                start_times <- jmvcore::toNumeric(start_data)
                end_times <- jmvcore::toNumeric(end_data)
                durations <- end_times - start_times

                # Convert to requested output unit if different
                unit_multiplier <- switch(self$options$timetypeoutput,
                                          "days" = 1,
                                          "weeks" = 1/7,
                                          "months" = 1/30.44,
                                          "years" = 1/365.25,
                                          1  # default no conversion
                )
                durations <- durations * unit_multiplier

            } else {
                # For datetime input
                start_times <- private$.parseDates(start_data, self$options$timetypedata)
                end_times <- private$.parseDates(end_data, self$options$timetypedata)
                durations <- private$.calculateDuration(start_times, end_times,
                                                        self$options$timetypeoutput)
            }

            return(list(
                start = start_times,
                end = end_times,
                durations = durations
            ))
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
                Start = 0,  # Always start at 0 for relative time
                End = time_data$durations
            )

            # Add events if specified
            if (!is.null(self$options$event)) {
                df$Event <- as.factor(data[[self$options$event]])
            }

            # Sort data if sort variable specified
            if (!is.null(self$options$sortVariable)) {
                sort_values <- data[[self$options$sortVariable]]
                df <- df[order(sort_values),]
            }

            # Calculate summary statistics
            stats <- list(
                median = median(df$End, na.rm=TRUE),
                mean = mean(df$End, na.rm=TRUE),
                sd = sd(df$End, na.rm=TRUE),
                min = min(df$End, na.rm=TRUE),
                max = max(df$End, na.rm=TRUE),
                range = diff(range(df$End, na.rm=TRUE))
            )

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
                    timeType = self$options$timetype
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
                y = reorder(Patient, End),
                yend = reorder(Patient, End)
            )) +
                ggplot2::geom_segment(
                    size = opts$barHeight,
                    color = "steelblue"
                ) +
                ggplot2::labs(
                    x = paste0("Time (", opts$timeUnit, ")"),
                    y = "Patient ID",
                    title = "Patient Timeline Analysis",
                    subtitle = sprintf(
                        "%s data: Median duration: %.1f %s (Range: %.1f - %.1f)",
                        ifelse(plotData$options$timeType == "raw", "Raw", "Date/time"),
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

            # Add theme
            p <- p +
                ggtheme +
                ggplot2::theme(
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor.y = ggplot2::element_blank()
                )

            print(p)
            TRUE
        }
    )
)
