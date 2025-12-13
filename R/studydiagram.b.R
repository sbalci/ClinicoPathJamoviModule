#' @title Study Diagram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom grDevices dev.off png svg
#' @importFrom ggplot2 ggplot aes geom_rect geom_text theme_void labs ggsave coord_cartesian
#' @importFrom base64enc base64encode

studydiagramClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "studydiagramClass",
    inherit = studydiagramBase,
    private = list(
        .processedData = NULL,
        .flowSummary = NULL,

        # Plot Style Constants ----
        .CONSORT_STYLE = list(
            border_width = 1.2,
            text_size = 3.8,
            arrow_size = 1.0,
            box_height = 0.35,
            box_width = 0.4
        ),

        .FLOWCHART_STYLE = list(
            border_width = 0.8,
            text_size = 3.2,
            arrow_size = 0.6,
            box_height = 0.30,
            box_width = 0.35
        ),

        .COLOR_SCHEMES = list(
            gray = list(
                main = "#f0f0f0",
                border = "#333333",
                exclusion = "#ffe6e6",
                excl_border = "#cc0000"
            ),
            blue = list(
                main = "#e7f3ff",
                border = "#0066cc",
                exclusion = "#ffe6e6",
                excl_border = "#cc0000"
            ),
            minimal = list(
                main = "#ffffff",
                border = "#000000",
                exclusion = "#f5f5f5",
                excl_border = "#666666"
            )
        ),

        # Variable name escaping utility ----
        .escapeVar = function(x) {
            if (is.null(x) || length(x) == 0) return(character(0))
            vapply(x, function(var) {
                gsub("[^A-Za-z0-9_]+", "_", make.names(var))
            }, character(1), USE.NAMES = FALSE)
        },

        # Get style parameters based on diagram type ----
        .getStyleParams = function(diagram_type) {
            is_consort <- grepl("consort", diagram_type)
            if (is_consort) private$.CONSORT_STYLE else private$.FLOWCHART_STYLE
        },

        .init = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$todo$setContent(private$.generateWelcomeMessage())
                return()
            }
        },

        .run = function() {
            # Check for required data
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$todo$setContent(private$.generateWelcomeMessage())
                return()
            }

            # 1. Validation
            if (is.null(self$options$data_format)) return()

            data <- self$data
            format <- self$options$data_format
            
            # Helper to add warning
            add_warning <- function(msg) {
                 self$results$diagram$setContent(paste0(self$results$diagram$content, "<div style='color:orange;'>Warning: ", msg, "</div>"))
            }
            
            # Data Preparation & Attrition Logic
            flow_data <- NULL # Structure: step, n, excluded, retention_pct
            
            if (format == "participant_step") {
                 # Not fully implemented in this pass - focus on the other two common ones
                 self$results$diagram$setContent("Participant Tracking format is under construction.")
                 return()
                 
            } else if (format == "step_summary") {
                 # Check vars
                 if (is.null(self$options$step_name) || is.null(self$options$participant_count)) return()
                 
                 step_col <- self$options$step_name
                 count_col <- self$options$participant_count
                 
                 # Ensure proper types
                 if (!is.numeric(data[[count_col]])) {
                      add_warning("Participant count must be numeric.")
                      return()
                 }
                 
                 # Extract standard flow
                 flow_data <- data.frame(
                      step = as.character(data[[step_col]]),
                      n = as.numeric(data[[count_col]]),
                      stringsAsFactors = FALSE
                 )
                 
                 # Calc exclusions sequentially
                 flow_data$excluded <- c(0, -diff(flow_data$n))
                 
                 # Check logic
                 if (any(flow_data$excluded < 0)) {
                      add_warning("Participant counts increase between steps. Exclusions clamped to 0.")
                      flow_data$excluded[flow_data$excluded < 0] <- 0
                 }
                 
                 # Reasons?
                 if (!is.null(self$options$exclusion_reason_summary)) {
                      flow_data$reason <- as.character(data[[self$options$exclusion_reason_summary]])
                 } else {
                      flow_data$reason <- ""
                 }

            } else if (format == "exclusion_mapping") {
                 # Sequential Exclusion Logic
                 if (is.null(self$options$participant_id_mapping) || is.null(self$options$exclusion_reason_mapping)) return()
                 
                 pid_col <- self$options$participant_id_mapping
                 reason_col <- self$options$exclusion_reason_mapping
                 
                 # Unique IDs check
                 if (any(duplicated(data[[pid_col]]))) {
                      add_warning("Duplicate Participant IDs found. Analysis assumes one row per participant.")
                 }
                 
                 current_data <- data
                 n_initial <- nrow(current_data)
                 
                 # Steps
                 steps <- list()
                 # Step 1 (Initial)
                 steps[[1]] <- list(name=self$options$step1_label, n=n_initial, excluded=0, reason="")
                 
                 # Defined Exclusion Steps
                 verify_step <- function(label, levels, current_df) {
                      if (is.null(levels) || length(levels) == 0) return(list(n=nrow(current_df), excluded=0, kept_df=current_df))
                      
                      # Identify excluded IDs
                      is_excluded <- current_df[[reason_col]] %in% levels
                      n_excl <- sum(is_excluded)
                      kept_df <- current_df[!is_excluded, ]
                      
                      return(list(n=nrow(kept_df), excluded=n_excl, kept_df=kept_df))
                 }
                 
                 # Step 2
                 res2 <- verify_step(self$options$step2_label, self$options$step1_exclusions, current_data) # Exclusions happen BETWEEN 1 and 2 usually, or AT step 1 leading to 2.
                 # Convention: Step 1 is "Assessed". Step 1 Exclusions remove people from Step 2 "Enrolled".
                 steps[[2]] <- list(name=self$options$step2_label, n=res2$n, excluded=res2$excluded, reason=paste(self$options$step1_exclusions, collapse=", "))
                 current_data <- res2$kept_df
                 
                 # Step 3
                 res3 <- verify_step(self$options$step3_label, self$options$step2_exclusions, current_data)
                 steps[[3]] <- list(name=self$options$step3_label, n=res3$n, excluded=res3$excluded, reason=paste(self$options$step2_exclusions, collapse=", "))
                 current_data <- res3$kept_df
                 
                 # Step 4
                 res4 <- verify_step(self$options$step4_label, self$options$step3_exclusions, current_data)
                 steps[[4]] <- list(name=self$options$step4_label, n=res4$n, excluded=res4$excluded, reason=paste(self$options$step3_exclusions, collapse=", "))
                 current_data <- res4$kept_df
                 
                 # Step 5
                 res5 <- verify_step(self$options$step5_label, self$options$step4_exclusions, current_data)
                 steps[[5]] <- list(name=self$options$step5_label, n=res5$n, excluded=res5$excluded, reason=paste(self$options$step4_exclusions, collapse=", "))
                 current_data <- res5$kept_df

                 # Convert to DF
                 flow_data <- do.call(rbind, lapply(steps, as.data.frame))
            }
            
            # --- Results Populating ---
            private$.processedData <- flow_data
            
            # Text Description
            if (self$options$show_interpretation) {
                 n_start <- flow_data$n[1]
                 n_end <- tail(flow_data$n, 1)
                 retention <- round(n_end/n_start * 100, 1)
                 
                 desc <- paste0(
                      "<h4>Flow Summary</h4>",
                      "<ul>",
                      "<li><b>Initial N:</b> ", n_start, "</li>",
                      "<li><b>Final N:</b> ", n_end, "</li>",
                      "<li><b>Retention:</b> ", retention, "%</li>",
                      "</ul>"
                 )
                 self$results$diagram$setContent(desc)
            } else {
                 self$results$diagram$setContent("")
            }
            
            # Table
            table <- self$results$summary
            for(i in 1:nrow(flow_data)) {
                 row <- list(
                      step = flow_data$step[i],
                      count = flow_data$n[i],
                      excluded = flow_data$excluded[i],
                      retention = paste0(round(flow_data$n[i] / flow_data$n[1] * 100, 1), "%")
                 )
                 table$addRow(rowKey = i, values = row)
            }
            
            # Trigger plot
            self$results$plot$setState(flow_data)

        },



        .validateVariables = function() {
            data_format <- self$options$data_format

            if (data_format == "participant_step") {
                required_vars <- c("participant_id", "step_excluded")
                missing_vars <- c()

                if (is.null(self$options$participant_id) || length(self$options$participant_id) == 0) {
                    missing_vars <- c(missing_vars, "Participant ID")
                }
                if (is.null(self$options$step_excluded) || length(self$options$step_excluded) == 0) {
                    missing_vars <- c(missing_vars, "Step Excluded")
                }

            } else if (data_format == "step_summary") {
                missing_vars <- c()

                if (is.null(self$options$step_name) || length(self$options$step_name) == 0) {
                    missing_vars <- c(missing_vars, "Step Name")
                }
                if (is.null(self$options$participant_count) || length(self$options$participant_count) == 0) {
                    missing_vars <- c(missing_vars, "Participant Count")
                }

            } else if (data_format == "exclusion_mapping") {
                missing_vars <- c()

                if (is.null(self$options$participant_id_mapping) || length(self$options$participant_id_mapping) == 0) {
                    missing_vars <- c(missing_vars, "Participant ID")
                }
                if (is.null(self$options$exclusion_reason_mapping) || length(self$options$exclusion_reason_mapping) == 0) {
                    missing_vars <- c(missing_vars, "Exclusion Reason")
                }
            }

            if (length(missing_vars) > 0) {
                message <- paste0(
                    "<div style='background: #f9f9f9; padding: 15px; margin: 10px; border: 1px solid #ddd;'>",
                    "<h4>Missing Required Variables</h4>",
                    "<p>Please select the following required variables for ", data_format, " format:</p>",
                    "<ul>",
                    paste0("<li>", missing_vars, "</li>", collapse = ""),
                    "</ul>",
                    "</div>"
                )
                self$results$todo$setContent(message)
                return(FALSE)
            }

            # Warn if variables from other formats are selected
            warnings <- character(0)

            if (data_format == "participant_step") {
                # Check if user selected step_summary or exclusion_mapping variables
                if (!is.null(self$options$step_name) || !is.null(self$options$participant_count)) {
                    warnings <- c(warnings, "Note: 'Step Name' and 'Participant Count' are for 'Step summary' format, not 'Participant tracking' format.")
                }
                if (!is.null(self$options$participant_id_mapping) || !is.null(self$options$exclusion_reason_mapping)) {
                    warnings <- c(warnings, "Note: Mapping variables are for 'Exclusion mapping' format, not 'Participant tracking' format.")
                }
            } else if (data_format == "step_summary") {
                # Check if user selected participant_step or exclusion_mapping variables
                if (!is.null(self$options$participant_id) || !is.null(self$options$step_excluded)) {
                    warnings <- c(warnings, "Note: 'Participant ID' and 'Step Excluded' are for 'Participant tracking' format, not 'Step summary' format.")
                }
                if (!is.null(self$options$participant_id_mapping) || !is.null(self$options$exclusion_reason_mapping)) {
                    warnings <- c(warnings, "Note: Mapping variables are for 'Exclusion mapping' format, not 'Step summary' format.")
                }
            } else if (data_format == "exclusion_mapping") {
                # Check if user selected other format variables
                if (!is.null(self$options$participant_id) || !is.null(self$options$step_excluded)) {
                    warnings <- c(warnings, "Note: 'Participant ID' and 'Step Excluded' are for 'Participant tracking' format, not 'Exclusion mapping' format.")
                }
                if (!is.null(self$options$step_name) || !is.null(self$options$participant_count)) {
                    warnings <- c(warnings, "Note: 'Step Name' and 'Participant Count' are for 'Step summary' format, not 'Exclusion mapping' format.")
                }
            }

            if (length(warnings) > 0) {
                warning_msg <- paste0(
                    "<div style='background: #fff3cd; padding: 15px; margin: 10px; border: 1px solid #ffc107;'>",
                    "<h4>⚠️ Variable Selection Warning</h4>",
                    paste0("<p>", warnings, "</p>", collapse = ""),
                    "<p>Please verify you have selected the correct data format.</p>",
                    "</div>"
                )
                # Show warning but continue processing
                self$results$todo$setContent(warning_msg)
            }

            return(TRUE)
        },

        .processParticipantStep = function() {
            data <- self$data
            id_var <- private$.escapeVar(self$options$participant_id)
            step_var <- private$.escapeVar(self$options$step_excluded)
            reason_var <- if (!is.null(self$options$exclusion_reason_participant)) {
                private$.escapeVar(self$options$exclusion_reason_participant)
            } else {
                NULL
            }

            # Count total participants
            total_participants <- nrow(data)

            # Get unique steps (excluding NA)
            # Respect factor levels if available, otherwise sort
            if (is.factor(data[[step_var]])) {
                steps <- levels(data[[step_var]])
                # Only keep levels that actually exist in the data (and are not NA)
                steps <- steps[steps %in% unique(data[[step_var]][!is.na(data[[step_var]])])]
            } else {
                steps <- sort(unique(data[[step_var]][!is.na(data[[step_var]])]))
            }

            # Calculate participants remaining at each step
            flow_data <- list()
            remaining <- total_participants

            # Initial step (all participants)
            flow_data[[1]] <- list(
                step = "Initial",
                participants = total_participants,
                excluded = 0,
                percent_retained = 100,
                exclusion_reasons = ""
            )

            # Process each exclusion step
            for (i in seq_along(steps)) {
                step_num <- steps[i]
                excluded_at_step <- sum(data[[step_var]] == step_num, na.rm = TRUE)
                remaining <- remaining - excluded_at_step

                # Get exclusion reasons for this step
                reasons <- ""
                if (!is.null(reason_var) && length(reason_var) > 0) {
                    step_reasons <- data[[reason_var]][data[[step_var]] == step_num & !is.na(data[[step_var]])]
                    if (length(step_reasons) > 0) {
                        reason_table <- table(step_reasons)
                        reason_table <- reason_table[reason_table > 0] # Filter zero counts
                        if (length(reason_table) > 0) {
                            reasons <- paste(names(reason_table), "(", reason_table, ")", collapse = "; ")
                        }
                    }
                }

                flow_data[[i + 1]] <- list(
                    step = paste("After Step", step_num),
                    participants = remaining,
                    excluded = excluded_at_step,
                    percent_retained = round((remaining / total_participants) * 100, 1),
                    exclusion_reasons = reasons
                )
            }

            private$.processedData <- flow_data
            private$.flowSummary <- list(
                format = "participant_step",
                total_initial = total_participants,
                total_final = remaining,
                total_excluded = total_participants - remaining,
                retention_rate = round((remaining / total_participants) * 100, 1)
            )
        },

        .processStepSummary = function() {
            data <- self$data
            step_var <- private$.escapeVar(self$options$step_name)
            count_var <- private$.escapeVar(self$options$participant_count)
            reason_var <- if (!is.null(self$options$exclusion_reason_summary)) {
                private$.escapeVar(self$options$exclusion_reason_summary)
            } else {
                NULL
            }

            # Sort data by participant count (descending)
            data <- data[order(data[[count_var]], decreasing = TRUE), ]

            flow_data <- list()

            for (i in 1:nrow(data)) {
                step_name <- as.character(data[[step_var]][i])
                count <- data[[count_var]][i]

                # Calculate excluded count
                excluded <- 0
                if (i < nrow(data)) {
                    excluded <- count - data[[count_var]][i + 1]
                }

                # Get exclusion reasons
                reasons <- ""
                if (!is.null(reason_var) && length(reason_var) > 0 && !is.na(data[[reason_var]][i])) {
                    reasons <- as.character(data[[reason_var]][i])
                }

                # Calculate percentage retained
                initial_count <- data[[count_var]][1]
                percent_retained <- round((count / initial_count) * 100, 1)

                flow_data[[i]] <- list(
                    step = step_name,
                    participants = count,
                    excluded = excluded,
                    percent_retained = percent_retained,
                    exclusion_reasons = reasons
                )
            }

            private$.processedData <- flow_data
            private$.flowSummary <- list(
                format = "step_summary",
                total_initial = data[[count_var]][1],
                total_final = data[[count_var]][nrow(data)],
                total_excluded = data[[count_var]][1] - data[[count_var]][nrow(data)],
                retention_rate = round((data[[count_var]][nrow(data)] / data[[count_var]][1]) * 100, 1)
            )
        },

        .processExclusionMapping = function() {
            data <- self$data
            id_var <- private$.escapeVar(self$options$participant_id_mapping)
            reason_var <- private$.escapeVar(self$options$exclusion_reason_mapping)

            total_participants <- nrow(data)

            # Get step exclusion mappings
            step_mappings <- list(
                step1 = self$options$step1_exclusions,
                step2 = self$options$step2_exclusions,
                step3 = self$options$step3_exclusions,
                step4 = self$options$step4_exclusions,
                step5 = self$options$step5_exclusions
            )

            # Get step labels
            step_labels <- c(
                self$options$step1_label,
                self$options$step2_label,
                self$options$step3_label,
                self$options$step4_label,
                self$options$step5_label
            )

            flow_data <- list()
            remaining <- total_participants

            # Initial step
            flow_data[[1]] <- list(
                step = "Initial",
                participants = total_participants,
                excluded = 0,
                percent_retained = 100,
                exclusion_reasons = ""
            )

            # Process each step
            for (i in 1:5) {
                step_exclusions <- step_mappings[[i]]

                if (length(step_exclusions) > 0) {
                    # Count participants excluded at this step
                    excluded_at_step <- sum(data[[reason_var]] %in% step_exclusions, na.rm = TRUE)
                    remaining <- remaining - excluded_at_step

                    # Create exclusion reason summary
                    if (excluded_at_step > 0) {
                        reason_counts <- table(data[[reason_var]][data[[reason_var]] %in% step_exclusions])
                        reason_counts <- reason_counts[reason_counts > 0] # Filter zero counts
                        if (length(reason_counts) > 0) {
                            reasons <- paste(names(reason_counts), "(", reason_counts, ")", collapse = "; ")
                        } else {
                            reasons <- ""
                        }
                    } else {
                        reasons <- ""
                    }

                    flow_data[[i + 1]] <- list(
                        step = step_labels[i],
                        participants = remaining,
                        excluded = excluded_at_step,
                        percent_retained = round((remaining / total_participants) * 100, 1),
                        exclusion_reasons = reasons
                    )
                } else {
                    # No exclusions defined for this step, skip it
                    next
                }
            }

            private$.processedData <- flow_data
            private$.flowSummary <- list(
                format = "exclusion_mapping",
                total_initial = total_participants,
                total_final = remaining,
                total_excluded = total_participants - remaining,
                retention_rate = round((remaining / total_participants) * 100, 1)
            )
        },


        .populateSummaryTable = function() {
            if (is.null(private$.processedData)) return()

            # Clear existing rows
            for (key in self$results$summary$rowKeys) {
                self$results$summary$removeRow(rowKey = key)
            }

            # Add rows to summary table
            for (i in seq_along(private$.processedData)) {
                step_data <- private$.processedData[[i]]
                self$results$summary$addRow(
                    rowKey = i,
                    values = list(
                        step = step_data$step,
                        participants = step_data$participants,
                        excluded = step_data$excluded,
                        percent_retained = step_data$percent_retained,
                        exclusion_reasons = step_data$exclusion_reasons
                    )
                )
            }
        },

        .generateDiagram = function() {
            if (is.null(private$.processedData)) return()

            diagram_type <- self$options$diagram_type

            if (diagram_type == "consort_standard") {
                html_content <- private$.generateConsortStandardHtml()
            } else if (diagram_type == "consort_ggplot") {
                html_content <- private$.generateConsortGgplotHtml()
            } else if (diagram_type == "flowchart_standard") {
                html_content <- private$.generateFlowchartStandardHtml()
            } else if (diagram_type == "flowchart_ggplot") {
                html_content <- private$.generateFlowchartGgplotHtml()
            } else {
                html_content <- "<p>Diagram generation not yet implemented for this type.</p>"
            }

            self$results$diagram$setContent(html_content)
        },

        .generateConsortStandardHtml = function() {
            # DiagrammeR/Viz.js doesn't work in jamovi HTML output
            # Provide informative message to use the plot output instead
            return(paste0(
                "<div style='text-align: center; padding: 20px; background: #f0f8ff; border: 1px solid #ccc;'>",
                "<h4>📊 CONSORT Diagram Available in Plot Output</h4>",
                "<p>The interactive diagram is displayed in the <strong>Plot</strong> section above.</p>",
                "<p>You can customize the appearance using the options panel:</p>",
                "<ul style='text-align: left; display: inline-block;'>",
                "<li>Direction: Top-to-Bottom or Left-to-Right</li>",
                "<li>Color Scheme: Gray, Blue, or Minimal</li>",
                "<li>Show/Hide exclusion boxes and percentages</li>",
                "</ul>",
                "<p style='margin-top: 15px;'><em>Right-click the plot to save as image</em></p>",
                "</div>"
            ))
        },

        .generateConsortGgplotHtml = function() {
            # ggconsort rendering handled in plot output
            return(paste0(
                "<div style='text-align: center; padding: 20px; background: #f0f8ff; border: 1px solid #ccc;'>",
                "<h4>📊 CONSORT Diagram Available in Plot Output</h4>",
                "<p>The diagram is displayed in the <strong>Plot</strong> section above using ggplot2.</p>",
                "<p style='margin-top: 15px;'><em>Right-click the plot to save as image</em></p>",
                "</div>"
            ))
        },

        .generateFlowchartStandardHtml = function() {
            # consort package rendering handled in plot output
            return(paste0(
                "<div style='text-align: center; padding: 20px; background: #f0f8ff; border: 1px solid #ccc;'>",
                "<h4>📊 Flow Diagram Available in Plot Output</h4>",
                "<p>The diagram is displayed in the <strong>Plot</strong> section above.</p>",
                "<p style='margin-top: 15px;'><em>Right-click the plot to save as image</em></p>",
                "</div>"
            ))
        },

        .generateFlowchartGgplotHtml = function() {
            # ggplot rendering handled in plot output
            return(paste0(
                "<div style='text-align: center; padding: 20px; background: #f0f8ff; border: 1px solid #ccc;'>",
                "<h4>📊 Flow Diagram Available in Plot Output</h4>",
                "<p>The diagram is displayed in the <strong>Plot</strong> section above using ggplot2.</p>",
                "<p style='margin-top: 15px;'><em>Right-click the plot to save as image</em></p>",
                "</div>"
            ))
        },

        .generatePlot = function() {
            # Set plot state for jamovi plotting
            if (!is.null(private$.processedData)) {
                plot <- self$results$plot
                plot$setState(list(
                    flow_data = private$.processedData,
                    diagram_type = self$options$diagram_type,
                    show_percentages = self$options$show_percentages,
                    show_exclusion_boxes = self$options$show_exclusion_boxes,
                    direction = self$options$direction,
                    color_scheme = self$options$color_scheme
                ))
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state)) return(FALSE)

            flow_data <- image$state$flow_data
            diagram_type <- image$state$diagram_type
            show_percentages <- image$state$show_percentages
            show_exclusion_boxes <- image$state$show_exclusion_boxes
            direction <- image$state$direction
            color_scheme <- image$state$color_scheme

            if (is.null(flow_data) || length(flow_data) == 0) return(FALSE)

            library(ggplot2)

            # Get style parameters and color schemes
            style <- private$.getStyleParams(diagram_type)
            colors <- private$.COLOR_SCHEMES[[color_scheme]]
            if (is.null(colors)) colors <- private$.COLOR_SCHEMES$gray

            # Extract style parameters
            box_border_width <- style$border_width
            text_size <- style$text_size
            arrow_size <- style$arrow_size
            box_height <- style$box_height
            box_width <- style$box_width

            # Determine plot orientation
            if (direction == "LR") {
                # Left to right
                n_stages <- length(flow_data)
                plot_data <- data.frame(
                    x = seq(1, n_stages),
                    y = rep(1, n_stages),
                    label = vapply(flow_data, function(s) {
                        lbl <- paste0(s$step, "\nn = ", s$participants)
                        if (show_percentages && s$percent_retained < 100) {
                            lbl <- paste0(lbl, "\n(", s$percent_retained, "% retained)")
                        }
                        lbl
                    }, character(1)),
                    stringsAsFactors = FALSE
                )

                p <- ggplot(plot_data, aes(x = x, y = y)) +
                    geom_rect(aes(xmin = x - box_width, xmax = x + box_width,
                                 ymin = y - box_height, ymax = y + box_height),
                             fill = colors$main, color = colors$border, size = box_border_width) +
                    geom_text(aes(label = label), size = text_size, lineheight = 0.9) +
                    theme_void() +
                    coord_cartesian(xlim = c(0.4, n_stages + 0.6), ylim = c(0.5, 1.5))

            } else {
                # Top to bottom (default)
                n_stages <- length(flow_data)
                plot_data <- data.frame(
                    x = rep(1, n_stages),
                    y = seq(n_stages, 1, by = -1),
                    label = vapply(flow_data, function(s) {
                        lbl <- paste0(s$step, "\nn = ", s$participants)
                        if (show_percentages && s$percent_retained < 100) {
                            lbl <- paste0(lbl, "\n(", s$percent_retained, "% retained)")
                        }
                        lbl
                    }, character(1)),
                    excluded = vapply(flow_data, function(s) s$excluded, numeric(1)),
                    exclusion_text = vapply(flow_data, function(s) {
                        if (s$excluded > 0 && s$exclusion_reasons != "") {
                            wrapped_text <- paste(strwrap(s$exclusion_reasons, width = 30), collapse = "\n")
                            paste0("Excluded: n=", s$excluded, "\n", wrapped_text)
                        } else if (s$excluded > 0) {
                            paste0("Excluded: n=", s$excluded)
                        } else {
                            ""
                        }
                    }, character(1)),
                    stringsAsFactors = FALSE
                )

                # Main flow boxes with diagram-specific styling
                p <- ggplot(plot_data, aes(x = x, y = y)) +
                    geom_rect(aes(xmin = x - box_width, xmax = x + box_width,
                                 ymin = y - box_height, ymax = y + box_height),
                             fill = colors$main, color = colors$border, size = box_border_width) +
                    geom_text(aes(label = label), size = text_size, lineheight = 0.9)

                # Add exclusion boxes if requested
                if (show_exclusion_boxes) {
                    excl_data <- plot_data[plot_data$excluded > 0, ]
                    if (nrow(excl_data) > 0) {
                        p <- p +
                            geom_rect(data = excl_data,
                                     aes(xmin = x + 0.6, xmax = x + 1.4,
                                         ymin = y - 0.25, ymax = y + 0.25),
                                     fill = colors$exclusion, color = colors$excl_border,
                                     size = 0.8) +
                            geom_text(data = excl_data,
                                     aes(x = x + 1.0, label = exclusion_text),
                                     size = 2.8, lineheight = 0.8)
                    }
                }

                # Add flow arrows
                if (n_stages > 1) {
                    arrow_data <- data.frame(
                        x = rep(1, n_stages - 1),
                        y = seq(n_stages, 2, by = -1),
                        xend = rep(1, n_stages - 1),
                        yend = seq(n_stages - 1, 1, by = -1) + box_height,
                        stringsAsFactors = FALSE
                    )

                    p <- p +
                        geom_segment(data = arrow_data,
                                    aes(x = x, y = y - box_height, xend = xend, yend = yend),
                                    arrow = arrow(length = unit(0.15, "inches"), type = "closed"),
                                    color = colors$border, size = arrow_size)
                }

                p <- p +
                    theme_void() +
                    coord_cartesian(xlim = c(0.4, if(show_exclusion_boxes) 2.0 else 1.6),
                                   ylim = c(0.5, n_stages + 0.5)) +
                    theme(plot.margin = margin(20, 20, 20, 20))
            }

            print(p)
            return(TRUE)
        },

        .generateInterpretation = function() {
            if (is.null(private$.flowSummary)) return()

            summary <- private$.flowSummary

            html_content <- paste0(
                "<div style='background: #f9f9f9; padding: 15px; margin: 10px; border: 1px solid #ddd;'>",
                "<h4>Study Flow Summary</h4>",
                "<p><strong>Data format:</strong> ", summary$format, "</p>",
                "<p><strong>Initial participants:</strong> ", summary$total_initial, "</p>",
                "<p><strong>Final participants:</strong> ", summary$total_final, "</p>",
                "<p><strong>Total excluded:</strong> ", summary$total_excluded, "</p>",
                "<p><strong>Retention rate:</strong> ", summary$retention_rate, "%</p>",
                "</div>",

                "<div style='background: #f0f8ff; padding: 15px; margin: 10px; border: 1px solid #ccc;'>",
                "<h4>Interpretation</h4>",
                "<p>This diagram shows the flow of participants through your study.</p>",
                "<ul>",
                "<li>Each box represents a stage in the study process</li>",
                "<li>Numbers in parentheses show participant counts</li>",
                "<li>Percentages show retention from initial enrollment</li>",
                if (self$options$show_exclusion_boxes) "<li>Red boxes show exclusions with reasons</li>" else "",
                "</ul>",
                "</div>"
            )

            self$results$interpretation$setContent(html_content)
        },

        .generateWelcomeMessage = function() {
            return(paste0(
                "<div style='background: #f9f9f9; padding: 20px; margin: 10px; border: 1px solid #ddd;'>",
                "<h3>Study Diagram Generator</h3>",
                "<p>Create professional study flow diagrams including CONSORT diagrams for clinical trials.</p>",

                "<h4>Supported Data Formats:</h4>",
                "<ul>",
                "<li><strong>Participant tracking:</strong> Each row is a participant with step excluded</li>",
                "<li><strong>Step summary:</strong> Each row is a study step with participant counts</li>",
                "<li><strong>Exclusion mapping:</strong> Map exclusion reasons to specific study steps</li>",
                "</ul>",

                "<h4>Getting Started:</h4>",
                "<ol>",
                "<li>Load your data</li>",
                "<li>Select your data format</li>",
                "<li>Choose diagram type (CONSORT for trials, Flowchart for general studies)</li>",
                "<li>Map your variables to the required fields</li>",
                "</ol>",

                "<p>The tool will automatically generate publication-ready diagrams with participant counts, exclusion reasons, and retention percentages.</p>",
                "</div>"
            ))
        },

        # Clinician-Friendly Output Generation ----

        .checkDataQuality = function() {
            if (is.null(private$.flowSummary)) return()

            summary <- private$.flowSummary
            warnings <- character()

            # Check for high attrition
            if (!is.null(summary$retention_rate)) {
                retention_pct <- summary$retention_rate

                if (retention_pct < 50) {
                    warnings <- c(warnings, sprintf(
                        "⚠️ <b>High Attrition:</b> Only %.1f%% of participants reached final analysis (%d/%d). Consider discussing attrition bias in your manuscript and performing sensitivity analyses.",
                        retention_pct, summary$total_final, summary$total_initial
                    ))
                }
            }

            # Check for missing exclusion reasons
            if (!is.null(private$.processedData)) {
                flow_data <- private$.processedData
                missing_reasons <- vapply(flow_data, function(s) {
                    s$excluded > 0 && (is.null(s$exclusion_reasons) || s$exclusion_reasons == "")
                }, logical(1))

                if (any(missing_reasons)) {
                    n_missing <- sum(missing_reasons)
                    warnings <- c(warnings, sprintf(
                        "⚠️ <b>Missing Exclusion Reasons:</b> %d stage(s) have exclusions without documented reasons. CONSORT 2010 requires reporting all exclusion criteria and reasons.",
                        n_missing
                    ))
                }

                # Check for single-stage studies
                if (length(flow_data) == 1) {
                    warnings <- c(warnings,
                        "ℹ️ <b>Single Stage:</b> Only one stage detected. Flow diagrams are most useful for multi-stage studies."
                    )
                }
            }

            # Generate warnings or success message
            if (length(warnings) > 0) {
                html <- paste0(
                    "<div style='background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 12px; margin: 10px 0;'>",
                    paste(warnings, collapse = "<br/><br/>"),
                    "</div>"
                )
                self$results$warnings$setContent(html)
            } else {
                self$results$warnings$setContent(
                    paste0(
                    "<div style='background-color: #d4edda; border-left: 4px solid #28a745; padding: 12px; margin: 10px 0;'>" ,
                    "✅ <b>No data quality issues detected.</b> All stages have documented exclusions and retention is adequate." ,
                    "</div>"
                    )
                )
            }
        },

        .generateClinicalOutputs = function() {
            if (is.null(private$.flowSummary)) return()

            private$.generateClinicalSummary()
            private$.generateReportSentence()
            private$.generateAboutAnalysis()
            private$.generateCaveatsAssumptions()
        },

        .generateClinicalSummary = function() {
            summary <- private$.flowSummary

            initial_n <- summary$total_initial
            final_n <- summary$total_final
            total_excluded <- summary$total_excluded
            retention_pct <- summary$retention_rate

            # Count exclusion stages
            n_stages <- if (!is.null(private$.processedData)) length(private$.processedData) - 1 else 0

            summary_html <- sprintf(
                paste0(
                    "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; border-radius: 5px;'>" ,
                    "<h4 style='margin-top: 0;'>📊 Study Flow Summary</h4>" ,
                    "<p><b>Initial participants:</b> %d</p>" ,
                    "<p><b>Exclusion stages:</b> %d</p>" ,
                    "<p><b>Total excluded:</b> %d (%.1f%%)</p>" ,
                    "<p><b>Final analysis cohort:</b> %d (<span style='color: %s; font-weight: bold;'>%.1f%% retained</span>)</p>" ,
                    "<p style='margin-bottom: 0; font-style: italic; color: #6c757d;'>" ,
                    "A retention rate of %.1f%% indicates %s attrition for this study design." ,
                    "</p></div>"
                ),
                initial_n,
                n_stages,
                total_excluded, (total_excluded / initial_n) * 100,
                final_n,
                if (retention_pct >= 70) "#28a745" else if (retention_pct >= 50) "#ffc107" else "#dc3545",
                retention_pct,
                retention_pct,
                if (retention_pct >= 70) "acceptable" else if (retention_pct >= 50) "moderate" else "high"
            )

            self$results$clinicalSummary$setContent(summary_html)
        },

        .generateReportSentence = function() {
            summary <- private$.flowSummary

            initial_n <- summary$total_initial
            final_n <- summary$total_final
            retention_pct <- round(summary$retention_rate, 1)

            # Generate exclusion summary from flow data
            exclusion_details <- character()
            if (!is.null(private$.processedData)) {
                flow_data <- private$.processedData
                for (stage in flow_data[-1]) {  # Skip first stage (initial)
                    if (stage$excluded > 0) {
                        exclusion_details <- c(exclusion_details,
                            sprintf("%s (n=%d)", stage$step, stage$excluded))
                    }
                }
            }

            report_html <- sprintf(
                paste0(
                    "<div style='background-color: #e7f3ff; border-left: 4px solid #0066cc; padding: 15px; margin: 10px 0;'>" ,
                    "<h4 style='margin-top: 0;'>📝 Copy-Ready Report Sentence</h4>" ,
                    "<p style='font-family: \"Times New Roman\", serif; font-size: 14px; line-height: 1.6;' id='reportText'>" ,
                    "Figure X shows the participant flow through the study. Of %d individuals assessed, " ,
                    "%d (%s%%) completed all study stages and were included in the final analysis.%s" ,
                    "</p>" ,
                    "<button onclick='navigator.clipboard.writeText(document.getElementById(\"reportText\").innerText)' " ,
                    "style='background-color: #0066cc; color: white; border: none; padding: 8px 16px; " ,
                    "border-radius: 4px; cursor: pointer; font-size: 12px; margin-top: 10px;'>" ,
                    "📋 Copy to Clipboard</button>" ,
                    "</div>"
                ),
                initial_n,
                final_n, retention_pct,
                if (length(exclusion_details) > 0)
                    sprintf(" Exclusions occurred at the following stages: %s.", paste(exclusion_details, collapse = ", "))
                else
                    ""
            )

            self$results$reportSentence$setContent(report_html)
        },

        .generateAboutAnalysis = function() {
            diagram_type <- self$options$diagram_type
            is_consort <- grepl("consort", diagram_type)

            about_html <- sprintf(
                paste0(
                    "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>" ,
                    "<h4 style='margin-top: 0;'>ℹ️ About This Analysis</h4>" ,
                    "<p><b>Analysis Type:</b> %s</p>" ,
                    "<p><b>Purpose:</b> %s</p>" ,
                    "<p><b>When to Use:</b> %s</p>" ,
                    "<p><b>Key Outputs:</b></p>" ,
                    "<ul style='margin-bottom: 0;'>" ,
                    "<li>Flow diagram showing participant progression through study stages</li>" ,
                    "<li>Exclusion counts and reasons at each stage</li>" ,
                    "<li>Retention percentages from baseline to final analysis</li>" ,
                    "<li>Data quality warnings for high attrition or missing documentation</li>" ,
                    "</ul>" ,
                    "<p style='margin-top: 10px; font-size: 12px; color: #6c757d;'>" ,
                    "📚 <a href='https://www.consort-statement.org/' target='_blank' style='color: #0066cc;'>CONSORT 2010 Guidelines</a> | " ,
                    "<a href='https://www.riinu.me/2024/02/consort/' target='_blank' style='color: #0066cc;'>Implementation Guide</a>" ,
                    "</p></div>"
                ),
                if (is_consort) "CONSORT Flow Diagram" else "Study Flowchart",
                if (is_consort)
                    "Create publication-ready flow diagrams compliant with CONSORT 2010 reporting standards for clinical trials."
                else
                    "Visualize participant flow through observational studies, cohort analyses, or diagnostic accuracy studies.",
                if (is_consort)
                    "Required for randomized controlled trials (RCTs) submitted to major medical journals. Documents screening, enrollment, allocation, follow-up, and analysis stages."
                else
                    "Recommended for any study with sequential inclusion/exclusion criteria. Helps readers understand selection bias and generalizability."
            )

            self$results$aboutAnalysis$setContent(about_html)
        },

        .generateCaveatsAssumptions = function() {
            summary <- private$.flowSummary

            caveats <- c(
                "<b>Data Requirements:</b>",
                "<ul>",
                "<li>Each participant should be counted only once</li>",
                "<li>Exclusion categories should be mutually exclusive</li>",
                "<li>All exclusion reasons should be documented</li>",
                "<li>Participants flow sequentially through stages (no re-entry)</li>",
                "</ul>",
                "<b>Assumptions:</b>",
                "<ul>",
                "<li>Numbers reported are final counts, not interim assessments</li>",
                "<li>Exclusions occur at discrete, identifiable stages</li>",
                "<li>Exclusion criteria are applied consistently</li>",
                "</ul>",
                "<b>Common Pitfalls:</b>",
                "<ul>",
                "<li>⚠️ Overlapping exclusion criteria (participants excluded for multiple reasons)</li>",
                "<li>⚠️ Missing data on exclusion timing (unclear which stage exclusion occurred)</li>",
                "<li>⚠️ Inconsistent definitions across study sites in multi-center trials</li>",
                "<li>⚠️ Confusing 'screened' with 'eligible' in initial counts</li>",
                "</ul>"
            )

            # Add context-specific warnings
            if (!is.null(summary$retention_rate) && summary$retention_rate < 50) {
                caveats <- c(caveats,
                    "<div style='background-color: #fff3cd; padding: 10px; margin-top: 10px; border-radius: 4px;'>",
                    "<b>⚠️ High Attrition Alert:</b> Less than 50% of initial participants reached final analysis. ",
                    "Consider:",
                    "<ul style='margin-bottom: 0;'>",
                    "<li>Comparing baseline characteristics of included vs. excluded participants</li>",
                    "<li>Performing sensitivity analyses (e.g., multiple imputation, inverse probability weighting)</li>",
                    "<li>Discussing potential selection bias in limitations section</li>",
                    "<li>Calculating E-value to assess robustness to unmeasured confounding</li>",
                    "</ul></div>"
                )
            }

            caveats_html <- paste0(
                "<div style='background-color: #fff8e1; border-left: 4px solid #ff9800; padding: 15px;'>",
                "<h4 style='margin-top: 0;'>⚠️ Caveats & Assumptions</h4>",
                paste(caveats, collapse = "\n"),
                "</div>"
            )

            self$results$caveatsAssumptions$setContent(caveats_html)
        }
    )
)
