#' @title Swimmer Plot
#' @description 
#' Comprehensive swimmer plot function with full ggswim integration.
#' Creates swimmer plots for visualizing patient timelines, treatments, milestones, and clinical events.
#'
#' @importFrom R6 R6Class  
#' @import jmvcore
#' @import ggswim
#' @importFrom ggplot2 ggplot aes labs theme element_text element_blank
#' @importFrom dplyr mutate filter select group_by summarize left_join arrange n bind_rows
#' @importFrom lubridate ymd_hms ymd ydm mdy myd dmy dym interval time_length
#' @importFrom tibble tibble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales pretty_breaks
#' @importFrom gridExtra grid.arrange

swimmerplotClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "swimmerplotClass",
    inherit = swimmerplotBase,
    private = list(
        
        # Enhanced date parsing with better error handling
        .parseDates = function(dates, format) {
            if (inherits(dates, c("Date", "POSIXct"))) return(dates)
            
            parsed <- tryCatch({
                switch(format,
                    "ymdhms" = lubridate::ymd_hms(dates),
                    "ymd"    = lubridate::ymd(dates),
                    "ydm"    = lubridate::ydm(dates),
                    "mdy"    = lubridate::mdy(dates),
                    "myd"    = lubridate::myd(dates),
                    "dmy"    = lubridate::dmy(dates),
                    "dym"    = lubridate::dym(dates),
                    lubridate::ymd(dates)
                )
            }, error = function(e) NULL)
            
            if (is.null(parsed)) {
                stop(paste("Error parsing dates with format", format, ". Please check your date format."))
            }
            return(parsed)
        },
        
        # Enhanced data validation with detailed error reporting
        .validateAndProcessData = function() {
            required_vars <- c("patientID", "startTime", "endTime")
            missing_vars <- required_vars[sapply(required_vars, function(x) is.null(self$options[[x]]))]
            
            if (length(missing_vars) > 0) {
                stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
            }
            
            df <- self$data
            
            # Extract and process core variables
            patient_data <- tryCatch({
                data.frame(
                    patient_id = as.character(df[[self$options$patientID]]),
                    start_time = df[[self$options$startTime]],
                    end_time = df[[self$options$endTime]],
                    stringsAsFactors = FALSE
                )
            }, error = function(e) {
                stop(paste("Error processing core variables:", e$message))
            })
            
            # Enhanced time processing
            if (self$options$timeType == "datetime") {
                patient_data$start_time <- private$.parseDates(
                    as.character(patient_data$start_time), 
                    self$options$dateFormat
                )
                patient_data$end_time <- private$.parseDates(
                    as.character(patient_data$end_time), 
                    self$options$dateFormat
                )
                
                # Handle relative vs absolute time display
                if (self$options$timeDisplay == "relative") {
                    intervals <- lubridate::interval(patient_data$start_time, patient_data$end_time)
                    durations <- lubridate::time_length(intervals, unit = self$options$timeUnit)
                    
                    patient_data$original_start <- patient_data$start_time
                    patient_data$original_end <- patient_data$end_time
                    patient_data$start_time <- 0
                    patient_data$end_time <- durations
                }
            } else {
                # Raw numeric processing with robust conversion
                patient_data$start_time <- suppressWarnings(as.numeric(as.character(patient_data$start_time)))
                patient_data$end_time <- suppressWarnings(as.numeric(as.character(patient_data$end_time)))
            }
            
            # Add response/status variable if provided
            if (!is.null(self$options$responseVar)) {
                patient_data$response <- as.factor(df[[self$options$responseVar]])
            }
            
            # Data validation
            valid_rows <- !is.na(patient_data$patient_id) & 
                         !is.na(patient_data$start_time) & 
                         !is.na(patient_data$end_time) &
                         patient_data$end_time >= patient_data$start_time
            
            if (sum(valid_rows) == 0) {
                stop("No valid data rows after validation. Check that end times are >= start times and all required data is present.")
            }
            
            patient_data <- patient_data[valid_rows, ]
            
            return(patient_data)
        },
        
        # Process milestone data with enhanced handling
        .processMilestones = function(patient_data) {
            milestone_data <- data.frame()
            
            for (i in 1:self$options$maxMilestones) {
                name_opt <- paste0("milestone", i, "Name")
                date_opt <- paste0("milestone", i, "Date")
                
                if (!is.null(self$options[[date_opt]]) && 
                    !is.null(self$options[[name_opt]]) && 
                    self$options[[name_opt]] != "") {
                    
                    milestone_dates <- self$data[[self$options[[date_opt]]]]
                    
                    # Skip if all NA
                    if (all(is.na(milestone_dates))) next
                    
                    # Process dates
                    if (self$options$timeType == "datetime") {
                        milestone_dates <- private$.parseDates(
                            as.character(milestone_dates),
                            self$options$dateFormat
                        )
                        
                        # Adjust for relative display
                        if (self$options$timeDisplay == "relative") {
                            # Calculate relative time from original start
                            adjusted_dates <- numeric(length(milestone_dates))
                            for (j in seq_along(milestone_dates)) {
                                if (!is.na(milestone_dates[j]) && j <= nrow(patient_data)) {
                                    if ("original_start" %in% names(patient_data)) {
                                        interval <- lubridate::interval(
                                            patient_data$original_start[j], 
                                            milestone_dates[j]
                                        )
                                        adjusted_dates[j] <- lubridate::time_length(
                                            interval, 
                                            unit = self$options$timeUnit
                                        )
                                    }
                                }
                            }
                            milestone_dates <- adjusted_dates
                        }
                    } else {
                        milestone_dates <- suppressWarnings(as.numeric(as.character(milestone_dates)))
                        
                        # Adjust for relative display
                        if (self$options$timeDisplay == "relative" && "start_time" %in% names(patient_data)) {
                            milestone_dates <- milestone_dates - patient_data$start_time[seq_along(milestone_dates)]
                        }
                    }
                    
                    # Create milestone dataframe
                    temp_milestone <- data.frame(
                        patient_id = patient_data$patient_id[seq_along(milestone_dates)],
                        time = milestone_dates,
                        label = self$options[[name_opt]],
                        milestone_type = paste0("milestone_", i),
                        stringsAsFactors = FALSE
                    )
                    
                    # Remove NA rows
                    temp_milestone <- temp_milestone[!is.na(temp_milestone$time), ]
                    
                    if (nrow(temp_milestone) > 0) {
                        milestone_data <- rbind(milestone_data, temp_milestone)
                    }
                }
            }
            
            return(milestone_data)
        },
        
        # Process ongoing status arrows
        .processOngoingStatus = function(patient_data) {
            arrow_data <- NULL
            
            # Detect ongoing treatments (those with missing end times or end at study cutoff)
            if ("response" %in% names(patient_data)) {
                ongoing_patients <- patient_data[
                    !is.na(patient_data$patient_id) & 
                    !is.na(patient_data$end_time) &
                    patient_data$response %in% c("CR", "PR", "SD"), # Responding patients more likely ongoing
                ]
                
                if (nrow(ongoing_patients) > 0) {
                    arrow_data <- data.frame(
                        patient_id = ongoing_patients$patient_id,
                        xend = ongoing_patients$end_time,
                        stringsAsFactors = FALSE
                    )
                }
            }
            
            return(arrow_data)
        },
        
        # Process event markers with enhanced icon support
        .processEventMarkers = function(patient_data) {
            event_data <- NULL
            
            if (self$options$showEventMarkers) {
                # Smart event variable detection
                event_var <- self$options$eventVar
                event_time_var <- self$options$eventTimeVar %||% self$options$startTime
                
                if (!is.null(event_var)) {
                    event_data <- tryCatch({
                        data.frame(
                            patient_id = as.character(self$data[[self$options$patientID]]),
                            time = self$data[[event_time_var]],
                            label = as.character(self$data[[event_var]]),
                            stringsAsFactors = FALSE
                        )
                    }, error = function(e) {
                        warning(paste("Error processing event markers:", e$message))
                        return(NULL)
                    })
                    
                    if (!is.null(event_data)) {
                        # Process event times
                        if (self$options$timeType == "datetime") {
                            event_data$time <- private$.parseDates(
                                as.character(event_data$time),
                                self$options$dateFormat
                            )
                            
                            # Adjust for relative display
                            if (self$options$timeDisplay == "relative") {
                                # Similar processing as milestones
                                adjusted_times <- numeric(nrow(event_data))
                                for (j in seq_len(nrow(event_data))) {
                                    if (!is.na(event_data$time[j])) {
                                        # Find corresponding patient start time
                                        patient_idx <- match(event_data$patient_id[j], patient_data$patient_id)
                                        if (!is.na(patient_idx) && "original_start" %in% names(patient_data)) {
                                            interval <- lubridate::interval(
                                                patient_data$original_start[patient_idx],
                                                event_data$time[j]
                                            )
                                            adjusted_times[j] <- lubridate::time_length(
                                                interval,
                                                unit = self$options$timeUnit
                                            )
                                        }
                                    }
                                }
                                event_data$time <- adjusted_times
                            }
                        } else {
                            event_data$time <- suppressWarnings(as.numeric(as.character(event_data$time)))
                        }
                        
                        # Filter valid events
                        event_data <- event_data[!is.na(event_data$time) & 
                                               !is.na(event_data$label) & 
                                               event_data$time >= 0, ]
                    }
                }
            }
            
            return(event_data)
        },
        
        # Calculate comprehensive summary statistics
        .calculateSummaryStats = function(patient_data) {
            # Calculate durations
            durations <- patient_data$end_time - patient_data$start_time
            
            # Basic statistics
            stats <- list(
                n_patients = length(unique(patient_data$patient_id)),
                n_observations = nrow(patient_data),
                median_duration = median(durations, na.rm = TRUE),
                mean_duration = mean(durations, na.rm = TRUE),
                sd_duration = sd(durations, na.rm = TRUE),
                min_duration = min(durations, na.rm = TRUE),
                max_duration = max(durations, na.rm = TRUE),
                q1_duration = quantile(durations, 0.25, na.rm = TRUE),
                q3_duration = quantile(durations, 0.75, na.rm = TRUE)
            )
            
            # Person-time analysis
            stats$total_person_time <- sum(durations, na.rm = TRUE)
            stats$mean_follow_up <- stats$total_person_time / stats$n_patients
            
            # Response analysis if available
            if ("response" %in% names(patient_data)) {
                response_summary <- table(patient_data$response)
                response_pct <- prop.table(response_summary) * 100
                
                stats$response_counts <- as.list(response_summary)
                stats$response_percentages <- as.list(response_pct)
            }
            
            return(stats)
        },
        
        # Generate comprehensive clinical interpretation
        .generateClinicalInterpretation = function(stats, patient_data) {
            interpretation <- list()
            
            # Timeline interpretation
            interpretation$timeline <- sprintf(
                "Study included %d patients with %d timeline observations. Median follow-up was %.1f %s (range: %.1f to %.1f %s).",
                stats$n_patients,
                stats$n_observations,
                stats$median_duration,
                self$options$timeUnit,
                stats$min_duration,
                stats$max_duration,
                self$options$timeUnit
            )
            
            # Person-time analysis
            interpretation$person_time <- sprintf(
                "Total person-time: %.1f %s. Average follow-up per patient: %.1f %s.",
                stats$total_person_time,
                self$options$timeUnit,
                stats$mean_follow_up,
                self$options$timeUnit
            )
            
            # Response interpretation if available
            if (!is.null(stats$response_counts)) {
                best_response <- names(stats$response_counts)[which.max(stats$response_counts)]
                best_pct <- stats$response_percentages[[best_response]]
                
                interpretation$response <- sprintf(
                    "Most common response was %s (%.1f%% of observations). Response distribution shows clinical patterns suitable for efficacy analysis.",
                    best_response,
                    best_pct
                )
            }
            
            return(interpretation)
        },
        
        .run = function() {
            # Enhanced instructions with comprehensive guidance
            if (is.null(self$options$patientID) || 
                is.null(self$options$startTime) || 
                is.null(self$options$endTime)) {
                
                instructions <- private$.generateInstructions()
                self$results$instructions$setContent(instructions)
                return()
            }
            
            # Validate and process data with comprehensive error handling
            tryCatch({
                patient_data <- private$.validateAndProcessData()
                milestone_data <- private$.processMilestones(patient_data)
                event_data <- private$.processEventMarkers(patient_data)
                arrow_data <- private$.processOngoingStatus(patient_data)
                
                # Calculate comprehensive statistics
                stats <- private$.calculateSummaryStats(patient_data)
                interpretation <- private$.generateClinicalInterpretation(stats, patient_data)
                
                # Update summary table
                private$.updateSummaryTable(stats)
                
                # Prepare plot data with all enhancements
                plot_state <- list(
                    patient_data = patient_data,
                    milestone_data = milestone_data,
                    event_data = event_data,
                    arrow_data = arrow_data,
                    stats = stats,
                    interpretation = interpretation,
                    options = list(
                        timeUnit = self$options$timeUnit,
                        laneWidth = self$options$laneWidth,
                        markerSize = self$options$markerSize,
                        theme = self$options$plotTheme,
                        showLegend = self$options$showLegend,
                        referenceLines = self$options$referenceLines,
                        customReferenceTime = self$options$customReferenceTime
                    )
                )
                
                self$results$plot$setState(plot_state)
                
                # Generate clinical interpretation if requested
                if (self$options$showInterpretation) {
                    private$.generateInterpretationOutput(interpretation)
                }
                
            }, error = function(e) {
                error_msg <- paste(
                    "<div style='color: red; padding: 10px; border: 1px solid red; border-radius: 5px;'>",
                    "<h4>Error in Swimmer Plot Analysis</h4>",
                    "<p><strong>Error:</strong>", e$message, "</p>",
                    "<p><strong>Suggestions:</strong></p>",
                    "<ul>",
                    "<li>Ensure all required variables are selected</li>",
                    "<li>Check that time variables contain valid numeric or date values</li>",
                    "<li>Verify that end times are greater than or equal to start times</li>",
                    "<li>For date/time data, ensure correct format is selected</li>",
                    "</ul>",
                    "</div>"
                )
                self$results$instructions$setContent(error_msg)
            })
        },
        
        .generateInstructions = function() {
            paste0(
                "<div style='background-color: #e1f5fe; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
                "<h3 style='color: #0277bd; margin-top: 0;'>🏊 Swimmer Plot Analysis</h3>",
                "<p>Create comprehensive swimmer plots for visualizing patient timelines, treatments, and clinical events using the advanced ggswim package.</p>",
                
                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>📋 Required Variables:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Patient ID:</strong> Unique identifier for each patient</li>",
                "<li><strong>Start Time:</strong> Treatment/observation start time</li>",
                "<li><strong>End Time:</strong> Treatment/observation end time</li>",
                "</ul>",
                "</div>",
                
                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>⚡ Enhanced Features:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Complete ggswim Integration:</strong> Professional clinical visualization</li>",
                "<li><strong>Milestone Support:</strong> Track key clinical events (surgery, progression, etc.)</li>",
                "<li><strong>Event Markers:</strong> Show specific events along patient timelines</li>",
                "<li><strong>Person-time Analysis:</strong> Epidemiological metrics and follow-up analysis</li>",
                "<li><strong>Clinical Interpretation:</strong> Automated insights for research</li>",
                "<li><strong>Enhanced Data Validation:</strong> Robust error handling and type conversion</li>",
                "</ul>",
                "</div>",
                
                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>🎨 Visualization Options:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Swim Lanes:</strong> Horizontal patient timelines with response coloring</li>",
                "<li><strong>Event Markers:</strong> Custom glyphs for clinical events</li>",
                "<li><strong>Status Arrows:</strong> Ongoing treatment indicators</li>",
                "<li><strong>Reference Lines:</strong> Protocol times, median values, custom timepoints</li>",
                "<li><strong>Clinical Themes:</strong> Professional styling for publications</li>",
                "</ul>",
                "</div>",
                
                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>📊 Data Formats Supported:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                "<li><strong>Raw Numeric:</strong> Days, weeks, months from treatment start</li>",
                "<li><strong>Date/Time:</strong> Actual calendar dates with multiple format support</li>",
                "<li><strong>Relative vs Absolute:</strong> Timeline display options</li>",
                "<li><strong>Multiple Time Units:</strong> Days, weeks, months, years</li>",
                "</ul>",
                "</div>",
                
                "<div style='background-color: #fff3e0; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #f57c00;'><strong>Clinical Research Applications:</strong> Ideal for oncology trials, treatment response visualization, progression tracking, and regulatory submissions.</p>",
                "</div>",
                
                "<div style='background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
                "<p style='margin: 0; color: #7b1fa2;'><strong>✨ Enhanced Features:</strong> Complete ggswim package integration with swim lanes, event markers, status arrows, and professional clinical themes for maximum flexibility and publication-ready output.</p>",
                "</div>",
                
                "</div>"
            )
        },
        
        .updateSummaryTable = function(stats) {
            # Clear existing rows
            self$results$summary$deleteRows()
            
            # Add basic statistics
            self$results$summary$addRow(rowKey = 1, values = list(
                metric = "Number of Patients",
                value = stats$n_patients
            ))
            
            self$results$summary$addRow(rowKey = 2, values = list(
                metric = "Total Observations",
                value = stats$n_observations
            ))
            
            self$results$summary$addRow(rowKey = 3, values = list(
                metric = "Median Duration",
                value = round(stats$median_duration, 2)
            ))
            
            self$results$summary$addRow(rowKey = 4, values = list(
                metric = "Mean Duration",
                value = round(stats$mean_duration, 2)
            ))
            
            self$results$summary$addRow(rowKey = 5, values = list(
                metric = "Total Person-Time",
                value = round(stats$total_person_time, 2)
            ))
            
            self$results$summary$addRow(rowKey = 6, values = list(
                metric = "Mean Follow-up",
                value = round(stats$mean_follow_up, 2)
            ))
            
            # Add response statistics if available
            if (!is.null(stats$response_counts)) {
                row_idx <- 7
                for (response in names(stats$response_counts)) {
                    self$results$summary$addRow(rowKey = row_idx, values = list(
                        metric = paste0(response, " Rate (%)"),
                        value = round(stats$response_percentages[[response]], 1)
                    ))
                    row_idx <- row_idx + 1
                }
            }
        },
        
        .generateInterpretationOutput = function(interpretation) {
            interp_html <- paste0(
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4>Clinical Interpretation</h4>",
                "<div style='margin: 10px 0;'>",
                "<h5 style='color: #2e7d32;'>Timeline Analysis:</h5>",
                "<p>", interpretation$timeline, "</p>",
                "</div>",
                "<div style='margin: 10px 0;'>",
                "<h5 style='color: #2e7d32;'>Person-Time Analysis:</h5>",
                "<p>", interpretation$person_time, "</p>",
                "</div>"
            )
            
            if (!is.null(interpretation$response)) {
                interp_html <- paste0(interp_html,
                    "<div style='margin: 10px 0;'>",
                    "<h5 style='color: #2e7d32;'>Response Pattern Analysis:</h5>",
                    "<p>", interpretation$response, "</p>",
                    "</div>"
                )
            }
            
            interp_html <- paste0(interp_html, "</div>")
            
            self$results$interpretation$setContent(interp_html)
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            plot_state <- image$state
            if (is.null(plot_state)) return(FALSE)
            
            patient_data <- plot_state$patient_data
            milestone_data <- plot_state$milestone_data
            event_data <- plot_state$event_data
            arrow_data <- plot_state$arrow_data
            stats <- plot_state$stats
            opts <- plot_state$options
            
            tryCatch({
                # Create enhanced ggswim plot
                p <- private$.createGgswimPlot(patient_data, milestone_data, event_data, arrow_data, opts, stats)
                
                print(p)
                return(TRUE)
                
            }, error = function(e) {
                warning(paste("Error creating plot:", e$message))
                
                # Create fallback plot
                p_fallback <- private$.createFallbackPlot(patient_data, e$message)
                print(p_fallback)
                return(TRUE)
            })
        },
        
        .createGgswimPlot = function(patient_data, milestone_data, event_data, arrow_data, opts, stats) {
            # Create base plot with swim lanes
            p <- ggplot2::ggplot()
            
            # Add swim lanes with enhanced styling
            if ("response" %in% names(patient_data)) {
                p <- p + ggswim::geom_swim_lane(
                    data = patient_data,
                    mapping = ggplot2::aes(
                        x = start_time,
                        xend = end_time,
                        y = patient_id,
                        colour = response
                    ),
                    linewidth = opts$laneWidth
                )
            } else {
                p <- p + ggswim::geom_swim_lane(
                    data = patient_data,
                    mapping = ggplot2::aes(
                        x = start_time,
                        xend = end_time,
                        y = patient_id
                    ),
                    linewidth = opts$laneWidth,
                    colour = "steelblue"
                )
            }
            
            # Add event markers if available
            if (!is.null(event_data) && nrow(event_data) > 0) {
                # Create enhanced marker mappings with clinical icons
                unique_labels <- unique(event_data$label)
                
                # Enhanced clinical glyphs with medical symbols
                clinical_glyphs <- private$.getEnhancedClinicalGlyphs(unique_labels)
                clinical_colors <- RColorBrewer::brewer.pal(max(3, min(length(unique_labels), 9)), "Set2")
                
                names(clinical_colors) <- unique_labels
                
                p <- p + ggswim::geom_swim_marker(
                    data = event_data,
                    mapping = ggplot2::aes(
                        x = time,
                        y = patient_id,
                        marker = label
                    ),
                    size = opts$markerSize
                )
                
                p <- p + ggswim::scale_marker_discrete(
                    name = "Clinical Events",
                    glyphs = clinical_glyphs,
                    colours = clinical_colors,
                    limits = unique_labels
                )
            }
            
            # Add milestone markers if available  
            if (!is.null(milestone_data) && nrow(milestone_data) > 0) {
                unique_milestones <- unique(milestone_data$label)
                milestone_shapes <- c(15, 16, 17, 18, 19)[1:length(unique_milestones)]
                milestone_colors <- RColorBrewer::brewer.pal(max(3, min(length(unique_milestones), 8)), "Dark2")
                
                names(milestone_shapes) <- unique_milestones
                names(milestone_colors) <- unique_milestones
                
                p <- p + ggplot2::geom_point(
                    data = milestone_data,
                    mapping = ggplot2::aes(
                        x = time,
                        y = patient_id,
                        shape = label,
                        color = label
                    ),
                    size = opts$markerSize + 1
                ) +
                ggplot2::scale_shape_manual(
                    name = "Milestones",
                    values = milestone_shapes
                ) +
                ggplot2::scale_color_manual(
                    name = "Milestones",
                    values = milestone_colors
                )
            }
            
            # Add ongoing status arrows using ggswim::geom_swim_arrow()
            if (!is.null(arrow_data) && nrow(arrow_data) > 0) {
                p <- p + ggswim::geom_swim_arrow(
                    data = arrow_data,
                    mapping = ggplot2::aes(
                        xend = xend,
                        y = patient_id
                    ),
                    colour = "darkgreen",
                    size = 1.5,
                    alpha = 0.7
                )
            }
            
            # Add reference lines
            if (!is.null(opts$referenceLines) && opts$referenceLines != "none") {
                p <- private$.addReferenceLines(p, opts, stats)
            }
            
            # Apply theme and styling
            if (opts$theme == "ggswim") {
                p <- p + ggswim::theme_ggswim()
            } else if (opts$theme == "ggswim_dark") {
                p <- p + ggswim::theme_ggswim_dark()
            } else {
                p <- p + ggplot2::theme_minimal()
            }
            
            # Add labels with clinical context
            p <- p + ggplot2::labs(
                title = "Patient Timeline Analysis",
                subtitle = sprintf("N=%d patients | Median follow-up: %.1f %s | Total person-time: %.1f %s",
                                 stats$n_patients, stats$median_duration, self$options$timeUnit,
                                 stats$total_person_time, self$options$timeUnit),
                x = paste0("Time (", self$options$timeUnit, ")"),
                y = "Patient ID"
            )
            
            # Legend handling
            if (!opts$showLegend) {
                p <- p + ggplot2::theme(legend.position = "none")
            }
            
            return(p)
        },
        
        .addReferenceLines = function(p, opts, stats) {
            if (opts$referenceLines == "median") {
                p <- p + ggplot2::geom_vline(
                    xintercept = stats$median_duration,
                    linetype = "dashed",
                    color = "darkgray",
                    alpha = 0.7
                ) +
                ggplot2::annotate(
                    "text",
                    x = stats$median_duration,
                    y = 1,
                    label = paste("Median:", round(stats$median_duration, 1)),
                    hjust = -0.1,
                    vjust = 0,
                    angle = 90,
                    size = 3
                )
            } else if (opts$referenceLines == "protocol") {
                protocol_times <- c(6, 12, 24, 36)
                max_time <- max(stats$max_duration, na.rm = TRUE)
                protocol_times <- protocol_times[protocol_times <= max_time * 1.1]
                
                for (t in protocol_times) {
                    p <- p + ggplot2::geom_vline(
                        xintercept = t,
                        linetype = "dotted",
                        color = "darkgray",
                        alpha = 0.5
                    )
                }
            } else if (opts$referenceLines == "custom" && !is.null(opts$customReferenceTime)) {
                p <- p + ggplot2::geom_vline(
                    xintercept = opts$customReferenceTime,
                    linetype = "dashed",
                    color = "red",
                    alpha = 0.7
                )
            }
            
            return(p)
        },
        
        .createFallbackPlot = function(patient_data, error_message) {
            ggplot2::ggplot(patient_data, ggplot2::aes(x = start_time, y = patient_id)) +
                ggplot2::geom_point(size = 2, color = "steelblue") +
                ggplot2::labs(
                    title = "Swimmer Plot (Simplified)",
                    subtitle = paste("Error in enhanced plot:", error_message),
                    x = "Time",
                    y = "Patient ID"
                ) +
                ggplot2::theme_minimal()
        },
        
        # Enhanced clinical glyph mapping for event markers
        .getEnhancedClinicalGlyphs = function(event_labels) {
            # Define clinical icon mappings
            clinical_mapping <- list(
                # Treatment events
                "treatment" = "💊", "therapy" = "💊", "drug" = "💊", "medication" = "💊",
                "infusion" = "💉", "injection" = "💉", "dose" = "💉",
                "surgery" = "🏥", "operation" = "🏥", "procedure" = "🏥",
                
                # Response events  
                "response" = "📊", "assessment" = "📊", "evaluation" = "📊",
                "progression" = "📈", "recurrence" = "🔄", "relapse" = "🔄",
                "remission" = "✅", "complete response" = "✅", "cr" = "✅",
                "partial response" = "🟡", "pr" = "🟡",
                "stable disease" = "🟠", "sd" = "🟠",
                "progressive disease" = "🔴", "pd" = "🔴",
                
                # Adverse events
                "adverse event" = "⚠️", "ae" = "⚠️", "toxicity" = "⚠️",
                "death" = "💀", "mortality" = "💀",
                
                # Follow-up events
                "follow-up" = "📅", "visit" = "📅", "appointment" = "📅",
                "scan" = "🔬", "imaging" = "🔬", "ct" = "🔬", "mri" = "🔬",
                
                # Generic events
                "event" = "⚫", "milestone" = "🎯", "endpoint" = "🏁"
            )
            
            # Create glyph vector
            glyphs <- character(length(event_labels))
            names(glyphs) <- event_labels
            
            # Map each label to appropriate glyph
            for (i in seq_along(event_labels)) {
                label <- tolower(event_labels[i])
                
                # Try exact match first
                if (label %in% names(clinical_mapping)) {
                    glyphs[i] <- clinical_mapping[[label]]
                } else {
                    # Try partial matches
                    matches <- sapply(names(clinical_mapping), function(pattern) {
                        grepl(pattern, label, fixed = TRUE)
                    })
                    
                    if (any(matches)) {
                        first_match <- names(clinical_mapping)[which(matches)[1]]
                        glyphs[i] <- clinical_mapping[[first_match]]
                    } else {
                        # Fallback to default symbols
                        default_symbols <- c("⬤", "■", "▲", "◆", "✚", "✖", "⬟", "★", "●", "◼")
                        glyphs[i] <- default_symbols[((i - 1) %% length(default_symbols)) + 1]
                    }
                }
            }
            
            return(glyphs)
        }
    )
)