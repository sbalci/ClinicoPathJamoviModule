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
                <br>Welcome to the Enhanced Swimmer Plot Function
                <br><br>
                This function creates swimmer plots to visualize subject response over time with robust data handling.
                <br><br>
                <b>Required inputs:</b>
                <ul>
                <li>Subject ID: Unique identifier for each subject</li>
                <li>Start Time: Start time of each lane segment (will be converted to numeric)</li>
                <li>End Time: End time of each lane segment (will be converted to numeric)</li>
                <li>Status: Response or status to color the lanes</li>
                </ul>
                
                <b>Optional inputs:</b>
                <ul>
                <li>Marker data: Add events at specific time points</li>
                <li>Arrow indicators: Show ongoing status for selected subjects</li>
                <li>Sorting options: Sort by ID, duration, or status</li>
                <li>Theming: Light or dark theme options</li>
                </ul>
                
                <b>Data handling improvements:</b>
                <ul>
                <li>Automatic conversion of factor time variables to numeric</li>
                <li>Robust handling of missing values</li>
                <li>Smart detection of marker variables</li>
                <li>Safe maximum calculations with NA handling</li>
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
            
            # Enhanced data validation
            tryCatch({
                # Prepare lane data with robust type conversion
                plotData <- df %>%
                    dplyr::select(
                        pt_id = !!patientID,
                        start_time = !!startTime,
                        end_time = !!endTime,
                        status = !!status
                    ) %>%
                    dplyr::mutate(
                        # Robust conversion to numeric - handles factors, characters, and numeric types
                        start_time = suppressWarnings(as.numeric(as.character(.data$start_time))),
                        end_time = suppressWarnings(as.numeric(as.character(.data$end_time))),
                        # Ensure pt_id is character for consistency
                        pt_id = as.character(.data$pt_id),
                        # Ensure status is factor for proper coloring
                        status = as.factor(.data$status)
                    ) %>%
                    # Filter out rows with invalid data
                    dplyr::filter(
                        !is.na(.data$pt_id) & 
                        !is.na(.data$start_time) & 
                        !is.na(.data$end_time) & 
                        !is.na(.data$status) &
                        .data$start_time >= 0 &  # Ensure positive times
                        .data$end_time >= 0 &
                        .data$end_time >= .data$start_time  # End time should be >= start time
                    )
                
                # Check if we have valid data after filtering
                if (nrow(plotData) == 0) {
                    stop("No valid data rows remain after data cleaning. Please check your time variables contain valid numeric values.")
                }
                
            }, error = function(e) {
                stop(paste("Error processing data:", e$message, 
                          "\nPlease ensure time variables contain numeric values and all required variables are selected."))
            })

            # Sort subjects based on user selection with robust sorting
            tryCatch({
                if (self$options$sortSubjects == "duration") {
                    # Sort by duration (longest to shortest)
                    subject_order <- plotData %>%
                        dplyr::group_by(.data$pt_id) %>%
                        dplyr::summarize(
                            max_time = max(.data$end_time, na.rm = TRUE),
                            .groups = 'drop'
                        ) %>%
                        dplyr::arrange(dplyr::desc(.data$max_time)) %>%
                        dplyr::pull(.data$pt_id)
                    
                    # Reorder plotData based on subject order
                    plotData <- plotData %>%
                        dplyr::mutate(pt_id = factor(.data$pt_id, levels = subject_order)) %>%
                        dplyr::arrange(.data$pt_id) %>%
                        dplyr::mutate(pt_id = as.character(.data$pt_id))
                        
                } else if (self$options$sortSubjects == "status") {
                    # Sort by status, then by ID
                    plotData <- plotData %>%
                        dplyr::arrange(.data$status, .data$pt_id)
                } else {
                    # Default: sort by ID
                    plotData <- plotData %>%
                        dplyr::arrange(.data$pt_id)
                }
            }, error = function(e) {
                warning(paste("Error in sorting, using default order:", e$message))
                plotData <- plotData %>%
                    dplyr::arrange(.data$pt_id)
            })
            
            # Status summary for table with error handling
            tryCatch({
                statusSummary <- plotData %>%
                    dplyr::group_by(.data$status) %>%
                    dplyr::summarize(n = dplyr::n(), .groups = 'drop') %>%
                    dplyr::mutate(percent = .data$n / sum(.data$n))
                
                # Clear existing rows and add new ones
                self$results$summary$deleteRows()
                for (i in seq_len(nrow(statusSummary))) {
                    self$results$summary$addRow(rowKey=i, values=list(
                        status = as.character(statusSummary$status[i]),
                        n = statusSummary$n[i],
                        percent = round(statusSummary$percent[i], 3)
                    ))
                }
            }, error = function(e) {
                warning(paste("Error creating status summary:", e$message))
            })
                
            # Enhanced Marker Data handling ----
            markerData <- NULL
            if (isTRUE(self$options$markerData)) {
                tryCatch({
                    # Smart detection of marker variable
                    markerVar <- self$options$markerVar
                    if (is.null(markerVar)) {
                        # Try common column names for event types
                        possibleMarkerVars <- c("event_type", "event", "marker_label", "event_label", 
                                              "marker", "event_name", "type", "category")
                        for (var in possibleMarkerVars) {
                            if (var %in% names(df)) {
                                markerVar <- var
                                break
                            }
                        }
                        # If still null, create a default label
                        if (is.null(markerVar)) {
                            df$temp_marker_label <- "Event"
                            markerVar <- "temp_marker_label"
                        }
                    }
                    
                    # Smart detection of marker time variable
                    markerTime <- self$options$markerTime
                    if (is.null(markerTime)) {
                        markerTime <- startTime  # Use start_time as default
                    }
                    
                    markerData <- df %>%
                        dplyr::select(
                            pt_id = !!patientID,
                            time = !!markerTime,
                            label = !!markerVar
                        ) %>%
                        dplyr::mutate(
                            # Robust type conversion
                            pt_id = as.character(.data$pt_id),
                            time = suppressWarnings(as.numeric(as.character(.data$time))),
                            label = as.character(.data$label)
                        ) %>%
                        dplyr::filter(
                            !is.na(.data$pt_id) & 
                            !is.na(.data$time) & 
                            !is.na(.data$label) &
                            .data$time >= 0  # Ensure positive times
                        )
                        
                    # Only proceed if we have valid marker data
                    if (nrow(markerData) > 0) {
                        # Enhanced glyph and color assignment
                        markerLabels <- unique(markerData$label)
                        markerGlyphs <- rep(c("⬤", "■", "▲", "◆", "✚", "✖", "⬟", "★", "●", "◼"), 
                                          length.out = length(markerLabels))
                        
                        # Use distinct colors
                        if (length(markerLabels) <= 8) {
                            markerColors <- RColorBrewer::brewer.pal(max(3, length(markerLabels)), "Set2")[1:length(markerLabels)]
                        } else {
                            markerColors <- rainbow(length(markerLabels))
                        }
                        
                        markerData <- markerData %>%
                            dplyr::mutate(
                                glyph = markerGlyphs[match(.data$label, markerLabels)],
                                colour = markerColors[match(.data$label, markerLabels)]
                            )
                    } else {
                        markerData <- NULL
                    }
                    
                }, error = function(e) {
                    warning(paste("Error processing marker data:", e$message))
                    markerData <- NULL
                })
            }
                
            # Enhanced Arrow Data handling ----
            arrowData <- NULL
            if (isTRUE(self$options$showArrows)) {
                tryCatch({
                    arrowFilter <- self$options$arrowFilter
                    
                    if (is.null(arrowFilter)) {
                        # Create arrows for all subjects at their last timepoint
                        # Use plotData which already has numeric end_time
                        arrowData <- plotData %>%
                            dplyr::group_by(.data$pt_id) %>%
                            dplyr::summarize(
                                end_time = max(.data$end_time, na.rm = TRUE),
                                .groups = 'drop'
                            ) %>%
                            dplyr::filter(!is.na(.data$end_time) & !is.infinite(.data$end_time))
                            
                    } else {
                        # Use the arrow filter variable
                        arrowData <- df %>%
                            dplyr::select(
                                pt_id = !!patientID,
                                end_time = !!endTime,
                                filter = !!arrowFilter
                            ) %>%
                            dplyr::mutate(
                                pt_id = as.character(.data$pt_id),
                                end_time = suppressWarnings(as.numeric(as.character(.data$end_time))),
                                filter = as.logical(.data$filter)
                            ) %>%
                            dplyr::filter(
                                !is.na(.data$filter) & 
                                .data$filter == TRUE &
                                !is.na(.data$end_time) &
                                .data$end_time >= 0
                            ) %>%
                            dplyr::group_by(.data$pt_id) %>%
                            dplyr::summarize(
                                end_time = max(.data$end_time, na.rm = TRUE),
                                .groups = 'drop'
                            ) %>%
                            dplyr::filter(!is.na(.data$end_time) & !is.infinite(.data$end_time))
                    }
                    
                    # Ensure we have valid arrow data
                    if (nrow(arrowData) == 0) {
                        arrowData <- NULL
                    }
                    
                }, error = function(e) {
                    warning(paste("Error processing arrow data:", e$message))
                    arrowData <- NULL
                })
            }

            # Enhanced Data Preview ----
            previewData <- list(
                "plot_data" = head(plotData, 10),
                "marker_data" = if (!is.null(markerData)) head(markerData, 10) else NULL,
                "arrow_data" = if (!is.null(arrowData)) head(arrowData, 10) else NULL,
                "data_summary" = list(
                    "total_subjects" = length(unique(plotData$pt_id)),
                    "total_observations" = nrow(plotData),
                    "time_range" = paste(round(min(plotData$start_time), 2), "-", round(max(plotData$end_time), 2)),
                    "status_levels" = length(unique(plotData$status)),
                    "marker_events" = if (!is.null(markerData)) nrow(markerData) else 0,
                    "arrows_shown" = if (!is.null(arrowData)) nrow(arrowData) else 0
                )
            )
            
            self$results$dataView$setContent(previewData)
            
            # Save data for plotting with enhanced options ----
            plotState <- list(
                "plot_data" = plotData,
                "marker_data" = markerData,
                "arrow_data" = arrowData,
                "lane_width" = self$options$laneWidth %||% 2,
                "marker_size" = self$options$markerSize %||% 5,
                "use_dark_theme" = isTRUE(self$options$useDarkTheme),
                "title" = self$options$customTitle %||% "Swimmer Plot",
                "xlab" = self$options$xLabel %||% "Time",
                "ylab" = self$options$yLabel %||% "Subject ID",
                "show_legend" = isTRUE(self$options$showLegend %||% TRUE)
            )
            
            image <- self$results$plot
            image$setState(plotState)
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            plotState <- image$state
            
            # Validate plot state
            if (is.null(plotState) || is.null(plotState$plot_data)) {
                return(FALSE)
            }
            
            # Extract data with validation
            plot_data <- plotState$plot_data
            marker_data <- plotState$marker_data
            arrow_data <- plotState$arrow_data
            
            # Ensure we have data to plot
            if (nrow(plot_data) == 0) {
                return(FALSE)
            }
            
            tryCatch({
                # Create base plot with enhanced swim lanes
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
                        )
                    
                    # Add marker scale if we have unique markers
                    unique_labels <- unique(marker_data$label)
                    if (length(unique_labels) > 0) {
                        p <- p + ggswim::scale_marker_discrete(
                            name = "Events",
                            glyphs = unique(marker_data$glyph),
                            colours = unique(marker_data$colour),
                            limits = unique_labels
                        )
                    }
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
                
                # Enhanced theming and labels
                p <- p +
                    ggplot2::scale_colour_brewer(
                        name = "Response Status",
                        palette = "Set1"
                    ) +
                    ggplot2::labs(
                        title = plotState$title,
                        subtitle = sprintf("Showing %d subjects with %d observations", 
                                         length(unique(plot_data$pt_id)), nrow(plot_data))
                    ) +
                    ggplot2::xlab(plotState$xlab) + 
                    ggplot2::ylab(plotState$ylab)
                
                # Apply theme with error handling
                if (isTRUE(plotState$use_dark_theme)) {
                    if (requireNamespace("ggswim", quietly = TRUE)) {
                        p <- p + ggswim::theme_ggswim_dark()
                    } else {
                        p <- p + ggplot2::theme_dark()
                    }
                } else {
                    if (requireNamespace("ggswim", quietly = TRUE)) {
                        p <- p + ggswim::theme_ggswim()
                    } else {
                        p <- p + ggplot2::theme_minimal()
                    }
                }
                
                # Legend handling
                if (!isTRUE(plotState$show_legend)) {
                    p <- p + ggplot2::theme(legend.position = "none")
                }
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                warning(paste("Error creating plot:", e$message))
                
                # Create a simple fallback plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = start_time, y = pt_id)) +
                    ggplot2::geom_point() +
                    ggplot2::labs(
                        title = "Swimmer Plot (Simplified)",
                        subtitle = paste("Error in enhanced plot:", e$message)
                    ) +
                    ggplot2::theme_minimal()
                
                print(p)
                return(TRUE)
            })
        }
    )
)