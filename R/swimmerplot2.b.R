#' @title Swimmer Plot 2
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggswim
#' @importFrom ggplot2 ggplot aes scale_colour_brewer scale_fill_brewer labs xlab ylab
#' @importFrom dplyr mutate filter select group_by summarize left_join arrange n
#' @importFrom tibble tibble
#' @importFrom rlang .data sym

swimmerplot2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "swimmerplot2Class",
    inherit = swimmerplot2Base,
    private = list(
        .run = function() {

            # Instructions ----
            todo <- "
                <br>Welcome to the Swimmer Plot Function
                <br><br>
                This function creates swimmer plots to visualize subject response over time.
                <br><br>
                <b>Required inputs:</b>
                <ul>
                <li>Subject ID: Unique identifier for each subject</li>
                <li>Start Time: Start time of each lane segment</li>
                <li>End Time: End time of each lane segment</li>
                <li>Status: Response or status to color the lanes</li>
                </ul>

                <b>Optional inputs:</b>
                <ul>
                <li>Marker data: Add events at specific time points</li>
                <li>Arrow indicators: Show ongoing status for selected subjects</li>
                </ul>

                <hr>
                "
            html <- self$results$todo
            html$setContent(todo)

            # Check Data ----
            if (is.null(self$options$patientID) ||
                is.null(self$options$startTime) ||
                is.null(self$options$endTime) ||
                is.null(self$options$status)) {
                return()
            }

            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }

            # Process Data ----
            df <- self$data
            patientID <- self$options$patientID
            startTime <- self$options$startTime
            endTime <- self$options$endTime
            status <- self$options$status

            # Prepare lane data - ensure numeric types for time fields
            plotData <- df %>%
                dplyr::select(
                    pt_id = !!patientID,
                    start_time = !!startTime,
                    end_time = !!endTime,
                    status = !!status
                ) %>%
                dplyr::mutate(
                    # Convert to numeric in case these are factors
                    start_time = as.numeric(as.character(.data$start_time)),
                    end_time = as.numeric(as.character(.data$end_time))
                ) %>%
                dplyr::filter(!is.na(.data$pt_id) & !is.na(.data$start_time) &
                                  !is.na(.data$end_time) & !is.na(.data$status))

            # Sort subjects based on user selection
            if (self$options$sortSubjects == "duration") {
                # Sort by duration (longest to shortest)
                plotData <- plotData %>%
                    dplyr::group_by(.data$pt_id) %>%
                    dplyr::summarize(max_time = max(.data$end_time, na.rm = TRUE)) %>%
                    dplyr::arrange(dplyr::desc(.data$max_time)) %>%
                    dplyr::select(.data$pt_id) %>%
                    dplyr::left_join(plotData, by = "pt_id")
            } else if (self$options$sortSubjects == "status") {
                # Sort by status
                plotData <- plotData %>%
                    dplyr::arrange(.data$status, .data$pt_id)
            } else {
                # Default: sort by ID
                plotData <- plotData %>%
                    dplyr::arrange(.data$pt_id)
            }

            # Status summary for table
            statusSummary <- plotData %>%
                dplyr::group_by(.data$status) %>%
                dplyr::summarize(n = dplyr::n()) %>%
                dplyr::mutate(
                    percent = .data$n / sum(.data$n)
                )

            # Add results to summary table
            for (i in seq_len(nrow(statusSummary))) {
                self$results$summary$addRow(rowKey=i, values=list(
                    status = statusSummary$status[i],
                    n = statusSummary$n[i],
                    percent = statusSummary$percent[i]
                ))
            }

            # Marker Data - use event_type from the same dataset ----
            markerData <- NULL
            if (isTRUE(self$options$markerData)) {
                # If marker variable is not specified but marker data is requested,
                # try to find an appropriate column
                markerVar <- self$options$markerVar
                if (is.null(markerVar)) {
                    # Try common column names for event types
                    possibleMarkerVars <- c("event_type", "event", "marker_label", "event_label")
                    for (var in possibleMarkerVars) {
                        if (var %in% names(df)) {
                            markerVar <- var
                            break
                        }
                    }
                    # If still null, create a default label
                    if (is.null(markerVar)) {
                        # Create a temporary column for markers
                        df$temp_marker_label <- "Event"
                        markerVar <- "temp_marker_label"
                    }
                }

                # Use start_time as marker position if markerTime is not specified
                markerTime <- self$options$markerTime
                if (is.null(markerTime)) {
                    markerTime <- startTime
                }

                markerData <- df %>%
                    dplyr::select(
                        pt_id = !!patientID,
                        time = !!markerTime,
                        label = !!markerVar
                    ) %>%
                    dplyr::mutate(
                        # Ensure time is numeric
                        time = as.numeric(as.character(.data$time))
                    ) %>%
                    dplyr::filter(!is.na(.data$pt_id) & !is.na(.data$time) & !is.na(.data$label))

                # Default glyphs and colors based on unique marker labels
                markerLabels <- unique(markerData$label)
                markerGlyphs <- rep(c("⬤", "■", "▲", "◆", "✚", "✖", "⬟", "★"), length.out = length(markerLabels))
                markerColors <- rainbow(length(markerLabels))

                markerData <- markerData %>%
                    dplyr::mutate(
                        glyph = markerGlyphs[match(.data$label, markerLabels)],
                        colour = markerColors[match(.data$label, markerLabels)]
                    )
            }

            # Arrow Data - identify final timepoint for each patient ----
            arrowData <- NULL
            if (isTRUE(self$options$showArrows)) {
                if (is.null(self$options$arrowFilter)) {
                    # If no filter is provided, create arrows for all subjects at their last timepoint
                    # Use plotData which already has numeric end_time
                    arrowData <- plotData %>%
                        dplyr::group_by(.data$pt_id) %>%
                        dplyr::summarize(end_time = max(.data$end_time, na.rm = TRUE))
                } else {
                    # Use the arrow filter variable to determine which subjects get arrows
                    arrowFilter <- self$options$arrowFilter
                    arrowData <- df %>%
                        dplyr::select(
                            pt_id = !!patientID,
                            end_time = !!endTime,
                            filter = !!arrowFilter
                        ) %>%
                        dplyr::mutate(
                            # Ensure end_time is numeric
                            end_time = as.numeric(as.character(.data$end_time))
                        ) %>%
                        dplyr::filter(!is.na(.data$filter) & .data$filter == TRUE) %>%
                        dplyr::group_by(.data$pt_id) %>%
                        dplyr::summarize(end_time = max(.data$end_time, na.rm = TRUE))
                }
            }

            # Data Preview ----
            previewData <- list(
                "plot_data" = head(plotData, 10),
                "marker_data" = if (!is.null(markerData)) head(markerData, 10) else NULL,
                "arrow_data" = if (!is.null(arrowData)) head(arrowData, 10) else NULL
            )

            self$results$dataView$setContent(previewData)

            # Save data for plotting ----
            plotState <- list(
                "plot_data" = plotData,
                "marker_data" = markerData,
                "arrow_data" = arrowData,
                "lane_width" = self$options$laneWidth %||% 2,
                "marker_size" = self$options$markerSize %||% 5,
                "use_dark_theme" = isTRUE(self$options$useDarkTheme),
                "title" = self$options$customTitle %||% "Swimmer Plot",
                "xlab" = self$options$xLabel %||% "Time",
                "ylab" = self$options$yLabel %||% "Subject ID"
            )

            image <- self$results$plot
            image$setState(plotState)
        }

        ,

        .plot = function(image, ggtheme, theme, ...) {
            # Get the plot state
            plotState <- image$state

            # Extract data
            plot_data <- plotState$plot_data
            marker_data <- plotState$marker_data
            arrow_data <- plotState$arrow_data

            # Create base plot with swim lanes using explicit column names
            p <- ggplot2::ggplot() +
                ggswim::geom_swim_lane(
                    data = plot_data,
                    mapping = ggplot2::aes(
                        x = .data[["start_time"]],
                        xend = .data[["end_time"]],
                        y = .data[["pt_id"]],
                        colour = .data[["status"]]
                    ),
                    linewidth = plotState$lane_width
                )

            # Add markers if available
            if (!is.null(marker_data) && nrow(marker_data) > 0) {
                p <- p +
                    ggswim::geom_swim_marker(
                        data = marker_data,
                        mapping = ggplot2::aes(
                            x = .data[["time"]],
                            y = .data[["pt_id"]],
                            marker = .data[["label"]]
                        ),
                        size = plotState$marker_size
                    ) +
                    ggswim::scale_marker_discrete(
                        name = "Events",
                        glyphs = marker_data$glyph,
                        colours = marker_data$colour,
                        limits = unique(marker_data$label)
                    )
            }

            # Add arrows if available
            if (!is.null(arrow_data) && nrow(arrow_data) > 0) {
                p <- p +
                    ggswim::geom_swim_arrow(
                        data = arrow_data,
                        mapping = ggplot2::aes(
                            xend = .data[["end_time"]],
                            y = .data[["pt_id"]]
                        )
                    )
            }

            # Add theming and labels
            p <- p +
                ggplot2::scale_colour_brewer(
                    name = "Response Status",
                    palette = "Set1"
                ) +
                ggplot2::labs(
                    title = plotState$title
                ) +
                ggplot2::xlab(plotState$xlab) +
                ggplot2::ylab(plotState$ylab)

            # Apply appropriate theme - using isTRUE to safely handle NULL values
            if (isTRUE(plotState$use_dark_theme)) {
                p <- p + ggswim::theme_ggswim_dark()
            } else {
                p <- p + ggswim::theme_ggswim()
            }

            print(p)
            TRUE
        }



        )
)
