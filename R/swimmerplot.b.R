#' @title Swimmer Plot
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import lubridate
#' @importFrom ggswim geom_swim_lane geom_swim_marker geom_swim_arrow scale_marker_discrete theme_ggswim
#' @description Creates swimmer plots for visualizing patient timelines, treatments, and clinical events
#'
swimmerplotClass <- if(requireNamespace("jmvcore")) R6::R6Class(
    "swimmerplotClass",
    inherit = swimmerplotBase,
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
                        durations = durations,
                        orig_start = start_times,  # Keep original dates for milestone alignment
                        orig_end = end_times
                    ))
                } else {
                    return(list(
                        start = start_times,
                        end = end_times,
                        durations = durations,
                        orig_start = start_times,
                        orig_end = end_times
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
                range  = diff(range(durations, na.rm = TRUE)),
                q1     = stats::quantile(durations, 0.25, na.rm = TRUE),
                q3     = stats::quantile(durations, 0.75, na.rm = TRUE)
            )

            # Add clinical metrics
            stats$ci_lower <- stats$mean - 1.96 * (stats$sd / sqrt(length(durations)))
            stats$ci_upper <- stats$mean + 1.96 * (stats$sd / sqrt(length(durations)))

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

        # Process milestone data with validation and formatting
        .processMilestone = function(milestone_name, milestone_dates, original_start_dates) {
            # Skip if no data
            if (is.null(milestone_dates) || all(is.na(milestone_dates)))
                return(NULL)

            # Process dates according to time type
            if (self$options$timetype == "datetime") {
                milestone_dates <- private$.parseDates(
                    as.character(milestone_dates),
                    self$options$timetypedata
                )
            } else {
                milestone_dates <- jmvcore::toNumeric(milestone_dates)
            }

            # For relative start, adjust milestone dates
            if (self$options$startType == "relative") {
                if (self$options$timetype == "datetime") {
                    # Calculate time difference between start date and milestone
                    adjusted_dates <- numeric(length(milestone_dates))
                    for (i in 1:length(milestone_dates)) {
                        if (!is.na(milestone_dates[i]) && !is.na(original_start_dates[i])) {
                            interval <- lubridate::interval(
                                original_start_dates[i],
                                milestone_dates[i]
                            )
                            adjusted_dates[i] <- lubridate::time_length(
                                interval,
                                unit = self$options$timetypeoutput
                            )
                        } else {
                            adjusted_dates[i] <- NA
                        }
                    }
                    return(adjusted_dates)
                } else {
                    # For raw numeric data, simply subtract start times
                    return(milestone_dates - original_start_dates)
                }
            } else {
                # For absolute time display, return original dates
                return(milestone_dates)
            }
        },

        .calculatePersonTimeMetrics = function(df, event_data = NULL) {
            # Calculate total person-time across all subjects
            if (self$options$timetype == "datetime" && self$options$startType == "absolute") {
                intervals <- lubridate::interval(df$Start, df$End)
                durations <- lubridate::time_length(intervals, unit = self$options$timetypeoutput)
            } else {
                durations <- df$End - df$Start
            }
            
            # Calculate key person-time metrics
            metrics <- list(
                total_person_time = sum(durations, na.rm = TRUE),
                n_subjects = nrow(df),
                mean_follow_up = mean(durations, na.rm = TRUE)
            )
            
            # Calculate incidence rates if event data is available
            if (!is.null(event_data)) {
                # Get unique event types
                event_types <- unique(event_data)
                event_types <- event_types[!is.na(event_types)]
                
                # For each event type, calculate incidence rate
                for (event_type in event_types) {
                    n_events <- sum(event_data == event_type, na.rm = TRUE)
                    incidence_rate <- n_events / metrics$total_person_time * 100
                    metrics[[paste0("incidence_", event_type)]] <- incidence_rate
                }
            }
            
            # Calculate time-varying cumulative rates
            # Sort durations in ascending order
            sorted_durations <- sort(durations)
            cumulative_time <- cumsum(sorted_durations)
            
            # Generate time points for cumulative person-time curve
            time_points <- seq(0, max(durations, na.rm = TRUE), length.out = 100)
            cumulative_person_time <- numeric(length(time_points))
            
            for (i in seq_along(time_points)) {
                t <- time_points[i]
                # Count how many subjects have follow-up time >= t
                cumulative_person_time[i] <- sum(durations >= t, na.rm = TRUE)
            }
            
            metrics$time_points <- time_points
            metrics$cumulative_person_time <- cumulative_person_time
            
            return(metrics)
        },



        .run = function() {
            # Check that required options are provided.
            if (is.null(self$options$patientID) ||
                is.null(self$options$start) ||
                is.null(self$options$end)) {
                todo <- "
        <br>Welcome to ClinicoPath Swimmer Plot Analysis
        <br><br>
        This tool creates a swimmer plot to visualize patient timelines and clinical events.
        <br><br>
        <b>Required variables:</b>
        <br>- Patient ID: Unique identifier for each patient.
        <br>- Start Time: When observation/treatment began.
        <br>- End Time: When observation/treatment ended.
        <br><br>
        <b>Optional variables:</b>
        <br>- Event Type: Clinical outcome or status (e.g., CR, PR, SD, PD)
        <br>- Sort Variable: Variable to order patients (e.g., survival time, response)
        <br><br>
        <b>Using ggswim package:</b>
        <br>The plot is created using ggswim which provides specialized visualizations for:
        <br>- Lanes: Horizontal bars showing patient timelines
        <br>- Markers: Symbols for events along the timeline
        <br>- Styling: Clinical-oriented themes and colors
        <hr>
        <br>Clinical application: Swimmer plots are valuable for visualizing individual patient journeys through treatment and follow-up, allowing clinicians to identify patterns in treatment response, progression, and outcomes.
        <br><br>
        <b>Person-time analysis:</b> The tool also calculates person-time metrics for epidemiological analysis, including total follow-up time, average follow-up, and incidence rates when event data is available.
        "
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

            # Record the original start times (needed for milestone alignment)
            original_start_times <- if("orig_start" %in% names(time_data)) {
                time_data$orig_start
            } else {
                time_data$start
            }

            # Optionally add event markers if provided.
            event_data <- NULL
            if (!is.null(self$options$event)) {
                df$Event <- as.factor(data[[self$options$event]])
                event_data <- df$Event
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

            # Calculate person-time metrics
            person_time_metrics <- private$.calculatePersonTimeMetrics(df, event_data)

            # Prepare markers data for ggswim if event type is provided
            markers_data <- NULL
            if (!is.null(self$options$event)) {
                # Create a markers dataframe for geom_swim_marker
                markers_data <- data.frame(
                    Patient = df$Patient,
                    x = df$End,
                    label = df$Event,
                    stringsAsFactors = TRUE
                )

                # Define glyphs for different event types (using ggswim defaults or suitable symbols)
                event_levels <- levels(df$Event)

                # Use clinical glyphs appropriate for common response types
                glyphs <- rep("⬤", length(event_levels))  # Default glyph is a circle

                # Define colors for response types
                colors <- RColorBrewer::brewer.pal(
                    min(length(event_levels), 9),
                    "Set1"
                )
            }





            # Milestone data will be processed during plotting for efficiency

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
            self$results$summary$addRow(rowKey = 7, values = list(
                metric = "25th Percentile",
                value = stats$q1
            ))
            self$results$summary$addRow(rowKey = 8, values = list(
                metric = "75th Percentile",
                value = stats$q3
            ))


            # Add person-time metrics to summary table
            self$results$summary$addRow(rowKey = 9, values = list(
                metric = "Total Person-Time",
                value = person_time_metrics$total_person_time
            ))
            self$results$summary$addRow(rowKey = 10, values = list(
                metric = "Number of Subjects",
                value = person_time_metrics$n_subjects
            ))
            self$results$summary$addRow(rowKey = 11, values = list(
                metric = "Mean Follow-up Time",
                value = person_time_metrics$mean_follow_up
            ))


            # Add clinical metrics if event data exists
            if (!is.null(self$options$event)) {
                # Count patients with each event type
                event_counts <- table(df$Event)
                event_pct <- prop.table(event_counts) * 100

                # Add to summary for clinically relevant metrics
                for (i in seq_along(event_counts)) {
                    event_name <- names(event_counts)[i]
                    event_n <- event_counts[i]
                    event_percent <- event_pct[i]

                    self$results$summary$addRow(rowKey = 8 + i, values = list(
                        metric = paste0(event_name, " Rate"),
                        value = event_percent
                    ))

                # Add incidence rate per 100 person-time units if available
                if (paste0("incidence_", event_name) %in% names(person_time_metrics)) {
                        self$results$summary$addRow(rowKey = 11 + length(event_counts) + i, values = list(
                            metric = paste0(event_name, " Incidence (per 100 ", self$options$timetypeoutput, ")"),
                            value = person_time_metrics[[paste0("incidence_", event_name)]]
                        ))
                    }

                }
            }

            # Prepare state for plotting.
            plotData <- list(
                data = df,
                markers_data = markers_data,
                original_start_times = original_start_times,
                options = list(
                    timeUnit = self$options$timetypeoutput,
                    barHeight = self$options$barHeight,
                    timeType = self$options$timetype,
                    startType = self$options$startType,
                    referenceLines = self$options$referenceLines,
                    customReferenceTime = self$options$customReferenceTime
                ),
                stats = stats,
                person_time = person_time_metrics
            )

            self$results$plot$setState(plotData)
        },

        .plot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            if (is.null(plotData)) return()

            df <- plotData$data
            markers_data <- plotData$markers_data
            original_start_times <- plotData$original_start_times
            opts <- plotData$options
            stats <- plotData$stats
            person_time <- plotData$person_time


            if (self$options$useggswim) {

            # Create the base plot with ggswim's geom_swim_lane
            p <- ggplot2::ggplot(df)

            # Add lanes using ggswim's geom_swim_lane
            if (!is.null(self$options$event)) {
                # If event data exists, color lanes by event
                p <- p + ggswim::geom_swim_lane(
                    mapping = ggplot2::aes(
                        x = Start,
                        xend = End,
                        y = Patient,
                        colour = Event
                    ),
                    linewidth = opts$barHeight
                )
            } else {
                # Otherwise, use default styling
                p <- p + ggswim::geom_swim_lane(
                    mapping = ggplot2::aes(
                        x = Start,
                        xend = End,
                        y = Patient
                    ),
                    linewidth = opts$barHeight,
                    colour = "steelblue"
                )
            }

            } else {


            # Base plot: draw horizontal segments for each patient.
            p <- ggplot2::ggplot(df, ggplot2::aes(
                x = Start,
                xend = End,
                y = Patient,
                yend = Patient
            )) +
                ggplot2::geom_segment(size = opts$barHeight, color = "steelblue")

            }



            # Configure x-axis scale and formatting based on timetype.
            if (opts$timeType == "datetime" && opts$startType == "absolute") {
                p <- p + ggplot2::scale_x_date(
                    date_breaks = "3 months",
                    date_labels = "%Y-%m"
                ) +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            } else {
                p <- p + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
            }





            if (self$options$useggswim) {
                    # Add event markers if Event variable exists using ggswim's geom_swim_marker
                    if (!is.null(self$options$event) && !is.null(markers_data)) {
                        # Define clinically relevant colors for common response types
                        clinical_colors <- c(
                            "CR" = "#008000",  # Complete Response - Green
                            "PR" = "#4169E1",  # Partial Response - Royal Blue
                            "SD" = "#FFA500",  # Stable Disease - Orange
                            "PD" = "#FF0000",  # Progressive Disease - Red
                            "NE" = "#808080"   # Not Evaluable - Gray
                        )

                        # Create a markers dataframe for geom_swim_marker
                        event_levels <- levels(df$Event)

                        # Define glyphs and colors based on event types
                        if (all(event_levels %in% names(clinical_colors))) {
                            # Use clinical colors for known response types
                            colors <- clinical_colors[event_levels]
                            # Use specific glyphs for clinical responses
                            glyphs <- c("✓", "◔", "■", "✗", "?")[1:length(event_levels)]
                        } else {
                            # For other event types, use a color palette
                            colors <- RColorBrewer::brewer.pal(
                                min(length(event_levels), 9),
                                "Set1"
                            )
                            # Default glyph is a circle
                            glyphs <- rep("⬤", length(event_levels))
                        }
                        names(glyphs) <- event_levels
                        names(colors) <- event_levels

                        # Add markers using ggswim's geom_swim_marker
                        p <- p + ggswim::geom_swim_marker(
                            data = markers_data,
                            mapping = ggplot2::aes(
                                x = x,
                                y = Patient,
                                marker = label
                            ),
                            size = self$options$markerSize
                        )

                        # Configure marker appearance using scale_marker_discrete
                        p <- p + ggswim::scale_marker_discrete(
                            name = "Response",
                            glyphs = glyphs,
                            colours = colors,
                            limits = event_levels
                        )
                    }
            } else {



            # Add event markers if an Event variable exists.
            if ("Event" %in% names(df)) {
                # Define clinically relevant colors for common response types
                clinical_colors <- c(
                    "CR" = "#008000",  # Complete Response - Green
                    "PR" = "#4169E1",  # Partial Response - Royal Blue
                    "SD" = "#FFA500",  # Stable Disease - Orange
                    "PD" = "#FF0000",  # Progressive Disease - Red
                    "NE" = "#808080"   # Not Evaluable - Gray
                )

                # Create a color mapping, defaulting to Set1 palette for unknown events
                if (all(levels(df$Event) %in% names(clinical_colors))) {
                    # Use clinical colors for known response types
                    event_colors <- clinical_colors[levels(df$Event)]
                } else {
                    # Use RColorBrewer for other event types
                    event_colors <- RColorBrewer::brewer.pal(
                        min(length(levels(df$Event)), 9),
                        "Set1"
                    )
                    names(event_colors) <- levels(df$Event)
                }

                p <- p +
                    ggplot2::geom_point(
                        mapping = ggplot2::aes(x = End, color = Event),
                        size = 4
                    ) +
                    ggplot2::scale_color_manual(
                        name = "Response",
                        values = event_colors
                    )
            }

            }



            # Process multiple milestone variables during plotting
            milestone_data <- data.frame()
            
            # Check for each milestone (1-5) and add if it exists
            for (i in 1:5) {
                milestone_name_opt <- paste0("milestone", i, "Name")
                milestone_date_opt <- paste0("milestone", i, "Date")
                
                # Only process if the date variable is provided
                if (!is.null(self$options[[milestone_date_opt]])) {
                    milestone_name <- self$options[[milestone_name_opt]]
                    
                    # Skip if milestone name is empty
                    if (is.null(milestone_name) || milestone_name == "")
                        next
                        
                    milestone_dates <- self$data[[self$options[[milestone_date_opt]]]]
                    
                    # Skip if all NA
                    if (all(is.na(milestone_dates)))
                        next
                        
                    # Process dates according to time type
                    if (self$options$timetype == "datetime") {
                        milestone_dates <- private$.parseDates(
                            as.character(milestone_dates),
                            self$options$timetypedata
                        )
                    } else {
                        milestone_dates <- jmvcore::toNumeric(milestone_dates)
                    }
                    
                    # Create temporary data frame for this milestone
                    temp_df <- data.frame(
                        Patient = df$Patient,
                        Date = milestone_dates,
                        Type = milestone_name,
                        stringsAsFactors = FALSE
                    )
                    
                    # Append to milestone data
                    milestone_data <- rbind(milestone_data, temp_df)
                }
            }
            
            # Add milestone markers if we have any valid data
            if (nrow(milestone_data) > 0) {
                # Remove NA dates
                milestone_data <- milestone_data[!is.na(milestone_data$Date), ]
                
                if (nrow(milestone_data) > 0) {
                    # For relative start, adjust milestone dates
                    if (self$options$startType == "relative" && self$options$timetype == "datetime") {
                        # Get the earliest start date for each patient
                        patient_start_dates <- tapply(
                            as.Date(self$data[[self$options$start]]), 
                            self$data[[self$options$patientID]], 
                            min, na.rm = TRUE
                        )
                        
                        # For each row in milestone_data, calculate the relative time
                        milestone_data$RelativeDate <- numeric(nrow(milestone_data))
                        
                        for (j in 1:nrow(milestone_data)) {
                            patient_id <- as.character(milestone_data$Patient[j])
                            if (patient_id %in% names(patient_start_dates)) {
                                start_date <- patient_start_dates[patient_id]
                                milestone_date <- milestone_data$Date[j]
                                
                                # Calculate time difference in the specified output unit
                                interval <- lubridate::interval(start_date, milestone_date)
                                milestone_data$RelativeDate[j] <- lubridate::time_length(
                                    interval, 
                                    unit = self$options$timetypeoutput
                                )
                            }
                        }
                        
                        # Use the relative dates for plotting
                        milestone_data$PlotDate <- milestone_data$RelativeDate
                    } else {
                        # For absolute start or raw data, use original dates
                        milestone_data$PlotDate <- milestone_data$Date
                    }
                    
                    # Add milestone markers to plot
                    unique_types <- unique(milestone_data$Type)
                    milestone_shapes <- c(15, 16, 17, 18, 19)[1:length(unique_types)]
                    names(milestone_shapes) <- unique_types
                    
                    milestone_colors <- RColorBrewer::brewer.pal(
                        max(3, min(length(unique_types), 8)),
                        "Set2"
                    )
                    names(milestone_colors) <- unique_types
                    
                    p <- p + 
                        ggplot2::geom_point(
                            data = milestone_data,
                            mapping = ggplot2::aes(
                                x = PlotDate,
                                y = Patient,
                                color = Type,
                                shape = Type
                            ),
                            size = 4
                        ) +
                        ggplot2::scale_color_manual(
                            name = "Milestones",
                            values = milestone_colors
                        ) +
                        ggplot2::scale_shape_manual(
                            name = "Milestones",
                            values = milestone_shapes
                        )
                }
            }

            # Add reference lines based on selected option
            if (!is.null(opts$referenceLines) && opts$referenceLines != "none") {
                if (opts$referenceLines == "median") {
                    p <- p +
                        ggplot2::geom_vline(
                            xintercept = stats$median,
                            linetype = "dashed",
                            color = "darkgray"
                        ) +
                        ggplot2::annotate(
                            "text",
                            x = stats$median,
                            y = min(as.numeric(df$Patient)) - 0.5,
                            label = paste("Median:", round(stats$median, 1), opts$timeUnit),
                            hjust = -0.1,
                            vjust = 0.5,
                            angle = 90,
                            size = 3
                        )
                } else if (opts$referenceLines == "protocol") {
                    # Add common clinical protocol times (6, 12, 24 months/cycles)
                    protocol_times <- c(6, 12, 24)

                    # Filter to only show protocol times within data range
                    max_time <- max(df$End, na.rm = TRUE)
                    protocol_times <- protocol_times[protocol_times <= max_time * 1.1]

                    for (t in protocol_times) {
                        p <- p +
                            ggplot2::geom_vline(
                                xintercept = t,
                                linetype = "dotted",
                                color = "darkgray"
                            ) +
                            ggplot2::annotate(
                                "text",
                                x = t,
                                y = min(as.numeric(df$Patient)) - 0.5,
                                label = paste(t, opts$timeUnit),
                                hjust = -0.1,
                                vjust = 0.5,
                                angle = 90,
                                size = 3
                            )
                    }
                } else if (opts$referenceLines == "custom" && !is.null(opts$customReferenceTime)) {
                    p <- p +
                        ggplot2::geom_vline(
                            xintercept = opts$customReferenceTime,
                            linetype = "dashed",
                            color = "darkgray"
                        ) +
                        ggplot2::annotate(
                            "text",
                            x = opts$customReferenceTime,
                            y = min(as.numeric(df$Patient)) - 0.5,
                            label = paste(opts$customReferenceTime, opts$timeUnit),
                            hjust = -0.1,
                            vjust = 0.5,
                            angle = 90,
                            size = 3
                        )
                }
            }

            # Add labels and title with clinical context
            plot_subtitle <- sprintf(
                "%s data (%s start): Median duration: %.1f %s (Range: %.1f to %.1f %s)",
                ifelse(opts$timeType == "raw", "Raw", "Date/Time"),
                ifelse(opts$startType == "relative", "relative", "absolute"),
                stats$median,
                opts$timeUnit,
                stats$min,
                stats$max,
                opts$timeUnit
            )

            # Add person-time information to subtitle
            person_time_info <- sprintf(
                "Total person-time: %.1f %s | Avg follow-up: %.1f %s",
                person_time$total_person_time,
                opts$timeUnit,
                person_time$mean_follow_up,
                opts$timeUnit
            )
            
            plot_subtitle <- paste(plot_subtitle, person_time_info, sep = "\n")

            p <- p + ggplot2::labs(
                x = paste0("Time (", opts$timeUnit, ")"),
                y = "Patient ID",
                title = "Patient Timeline Analysis",
                subtitle = plot_subtitle
            )

            # Create a second plot with cumulative person-time curve
            # This data represents how many patients are still being followed at each time point
            time_data <- data.frame(
                Time = person_time$time_points,
                Remaining = person_time$cumulative_person_time
            )
            
            p2 <- ggplot2::ggplot(time_data, ggplot2::aes(x = Time, y = Remaining)) +
                ggplot2::geom_line(color = "steelblue", size = 1) +
                ggplot2::labs(
                    x = paste0("Time (", opts$timeUnit, ")"),
                    y = "Number of patients at risk",
                    title = "Cumulative Person-Time Analysis"
                ) +
                ggplot2::theme_minimal()


            if (self$options$useggswim) {

            # Apply the ggswim theme
            p <- p + ggswim::theme_ggswim()

            } else {

                # Apply additional theme settings.
            p <- p + ggtheme +
                ggplot2::theme(
                    panel.grid.major.y = ggplot2::element_blank(),
                    panel.grid.minor.y = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                    legend.position = "right",
                    legend.title = ggplot2::element_text(face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 9)
                )

                }

            # Create a multiplot layout with the main swimmer plot and the person-time curve
            # Using gridExtra to arrange the plots
            if (requireNamespace("gridExtra", quietly = TRUE)) {
                combined_plot <- gridExtra::grid.arrange(
                    p, p2, 
                    ncol = 1,
                    heights = c(3, 1)  # Main plot is 3x the height of the person-time curve
                )
                print(combined_plot)
            } else {
                # If gridExtra is not available, just print the main plot
                print(p)
                message("Install 'gridExtra' package to see the combined swimmer plot with person-time curve")
            }
            




            TRUE
        }
    )
)
