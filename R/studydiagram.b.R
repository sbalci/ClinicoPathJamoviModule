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

            # Check for required variables based on format
            if (!private$.validateVariables()) {
                return()
            }

            # Process data based on selected format
            data_format <- self$options$data_format

            if (data_format == "participant_step") {
                private$.processParticipantStep()
            } else if (data_format == "step_summary") {
                private$.processStepSummary()
            } else if (data_format == "exclusion_mapping") {
                private$.processExclusionMapping()
            }

            # Generate outputs if data processing succeeded
            if (!is.null(private$.processedData)) {
                private$.populateSummaryTable()
                private$.generateDiagram()
                private$.generatePlot()

                if (self$options$show_interpretation) {
                    private$.generateInterpretation()
                }
            }
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

            return(TRUE)
        },

        .processParticipantStep = function() {
            data <- self$data
            id_var <- self$options$participant_id
            step_var <- self$options$step_excluded
            reason_var <- self$options$exclusion_reason_participant

            # Count total participants
            total_participants <- nrow(data)

            # Get unique steps (excluding NA)
            steps <- sort(unique(data[[step_var]][!is.na(data[[step_var]])]))

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
                        reasons <- paste(names(reason_table), "(", reason_table, ")", collapse = "; ")
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
            step_var <- self$options$step_name
            count_var <- self$options$participant_count
            reason_var <- self$options$exclusion_reason_summary

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
            id_var <- self$options$participant_id_mapping
            reason_var <- self$options$exclusion_reason_mapping

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
                        reasons <- paste(names(reason_counts), "(", reason_counts, ")", collapse = "; ")
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
            # Generate CONSORT diagram using DiagrammeR
            flow_data <- private$.processedData

            # Create DOT notation for CONSORT diagram
            dot_lines <- c("digraph CONSORT {")
            dot_lines <- c(dot_lines,
                "graph [rankdir=TB, bgcolor=white, nodesep=0.5, ranksep=1.2];",
                "node [shape=rectangle, style=\"filled\", fillcolor=\"#f0f0f0\", color=\"#333333\", fontname=\"Arial\", fontsize=10];"
            )

            # Add main flow nodes
            for (i in seq_along(flow_data)) {
                step_data <- flow_data[[i]]
                label <- paste0(step_data$step, "\\n(n = ", step_data$participants, ")")

                if (self$options$show_percentages && i > 1) {
                    label <- paste0(label, "\\n[", step_data$percent_retained, "% retained]")
                }

                dot_lines <- c(dot_lines,
                    sprintf("node%d [label=\"%s\"];", i, label)
                )
            }

            # Add exclusion boxes if requested
            if (self$options$show_exclusion_boxes) {
                for (i in 2:length(flow_data)) {
                    step_data <- flow_data[[i]]
                    if (step_data$excluded > 0) {
                        exc_label <- paste0("Excluded\\n(n = ", step_data$excluded, ")")
                        if (step_data$exclusion_reasons != "") {
                            exc_label <- paste0(exc_label, "\\n", gsub(";", "\\n", step_data$exclusion_reasons))
                        }

                        dot_lines <- c(dot_lines,
                            sprintf("exc%d [label=\"%s\", fillcolor=\"#ffe6e6\", color=\"#cc0000\"];", i-1, exc_label)
                        )
                    }
                }
            }

            # Add main flow edges
            for (i in 1:(length(flow_data)-1)) {
                dot_lines <- c(dot_lines,
                    sprintf("node%d -> node%d;", i, i+1)
                )
            }

            # Add exclusion edges
            if (self$options$show_exclusion_boxes) {
                for (i in 2:length(flow_data)) {
                    step_data <- flow_data[[i]]
                    if (step_data$excluded > 0) {
                        dot_lines <- c(dot_lines,
                            sprintf("node%d -> exc%d [style=dashed, color=\"#cc0000\"];", i-1, i-1)
                        )
                    }
                }
            }

            dot_lines <- c(dot_lines, "}")
            dot_code <- paste(dot_lines, collapse = "\n")

            # Generate HTML with embedded SVG
            html_content <- paste0(
                "<div style='text-align: center; padding: 20px;'>",
                "<div id='diagram'></div>",
                "<script>",
                "var dot = `", dot_code, "`;",
                "var svg = Viz(dot, {format: 'svg'});",
                "document.getElementById('diagram').innerHTML = svg;",
                "</script>",
                "</div>"
            )

            return(html_content)
        },

        .generateConsortGgplotHtml = function() {
            # Generate CONSORT diagram using ggconsort package
            flow_data <- private$.processedData

            # Check if ggconsort is available
            if (!requireNamespace("ggconsort", quietly = TRUE)) {
                return("<p>Error: ggconsort package not available. Install with: remotes::install_github('tgerke/ggconsort')</p>")
            }

            tryCatch({
                # Convert flow data to ggconsort format
                library(ggconsort)
                library(dplyr)
                library(ggplot2)

                # Create a simple cohort structure for ggconsort
                # Note: This is a simplified implementation - real use would require more complex data preparation
                consort_data <- data.frame(
                    step = sapply(flow_data, function(x) x$step),
                    n = sapply(flow_data, function(x) x$participants),
                    excluded = sapply(flow_data, function(x) x$excluded),
                    stringsAsFactors = FALSE
                )

                # Create a basic ggplot CONSORT-style visualization
                # This is a simplified version - full ggconsort integration would be more sophisticated
                p <- ggplot(consort_data, aes(x = 1, y = rev(seq_along(step)))) +
                    geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = rev(seq_along(step)) - 0.3, ymax = rev(seq_along(step)) + 0.3),
                              fill = "#f0f0f0", color = "#333333") +
                    geom_text(aes(label = paste0(step, "\n(n = ", n, ")")), size = 3) +
                    theme_void() +
                    labs(title = "Study Flow Diagram") +
                    coord_cartesian(xlim = c(0.5, 1.5), ylim = c(0.5, length(consort_data$step) + 0.5))

                # Convert ggplot to HTML
                temp_file <- tempfile(fileext = ".png")
                ggsave(temp_file, p, width = 8, height = 6, dpi = 150)

                # Read image and convert to base64
                img_base64 <- base64enc::base64encode(temp_file)
                unlink(temp_file)

                html_content <- paste0(
                    "<div style='text-align: center; padding: 20px;'>",
                    "<img src='data:image/png;base64,", img_base64, "' style='max-width: 100%; height: auto;'/>",
                    "<p><em>Generated using ggconsort-style visualization</em></p>",
                    "</div>"
                )

                return(html_content)

            }, error = function(e) {
                return(paste0("<p>Error generating ggconsort diagram: ", e$message, "</p>"))
            })
        },

        .generateFlowchartStandardHtml = function() {
            # Generate flowchart using consort package
            flow_data <- private$.processedData

            # Check if consort is available
            if (!requireNamespace("consort", quietly = TRUE)) {
                return("<p>Error: consort package not available. Install with: install.packages('consort')</p>")
            }

            tryCatch({
                library(consort)

                # Convert flow data to consort format
                consort_data <- data.frame(
                    step = sapply(flow_data, function(x) x$step),
                    n = sapply(flow_data, function(x) x$participants),
                    excluded = sapply(flow_data, function(x) x$excluded),
                    exclusion_reason = sapply(flow_data, function(x) x$exclusion_reasons),
                    stringsAsFactors = FALSE
                )

                # Create consort diagram
                # Note: This is a basic implementation - real consort integration would use patient-level data
                g <- consort_plot(
                    data = consort_data,
                    orders = consort_data$step,
                    side_box = if (self$options$show_exclusion_boxes) consort_data$exclusion_reason else NULL,
                    allocation = NULL,
                    labels = consort_data$step,
                    text_width = 50
                )

                # Convert to HTML
                temp_file <- tempfile(fileext = ".svg")
                svg(temp_file, width = 8, height = 6)
                plot(g)
                dev.off()

                # Read SVG content
                svg_content <- readLines(temp_file, warn = FALSE)
                svg_content <- paste(svg_content, collapse = "\n")
                unlink(temp_file)

                html_content <- paste0(
                    "<div style='text-align: center; padding: 20px;'>",
                    svg_content,
                    "<p><em>Generated using consort package</em></p>",
                    "</div>"
                )

                return(html_content)

            }, error = function(e) {
                # Fallback to a simple HTML table representation
                html_content <- "<div style='text-align: center; padding: 20px;'>"
                html_content <- paste0(html_content, "<h3>Study Flow Summary</h3>")
                html_content <- paste0(html_content, "<table style='margin: 0 auto; border-collapse: collapse;'>")

                for (i in seq_along(flow_data)) {
                    step_data <- flow_data[[i]]
                    html_content <- paste0(html_content,
                        "<tr><td style='border: 1px solid #ccc; padding: 10px; text-align: center; background: #f9f9f9;'>",
                        "<strong>", step_data$step, "</strong><br/>",
                        "n = ", step_data$participants,
                        if (self$options$show_percentages && i > 1) paste0("<br/>(", step_data$percent_retained, "% retained)") else "",
                        "</td></tr>"
                    )

                    if (self$options$show_exclusion_boxes && step_data$excluded > 0) {
                        html_content <- paste0(html_content,
                            "<tr><td style='border: 1px solid #ccc; padding: 5px; text-align: center; background: #ffe6e6; font-size: 0.9em;'>",
                            "Excluded: n = ", step_data$excluded,
                            if (step_data$exclusion_reasons != "") paste0("<br/>", step_data$exclusion_reasons) else "",
                            "</td></tr>"
                        )
                    }
                }

                html_content <- paste0(html_content, "</table>")
                html_content <- paste0(html_content, "<p><em>Fallback visualization (consort package error: ", e$message, ")</em></p>")
                html_content <- paste0(html_content, "</div>")

                return(html_content)
            })
        },

        .generateFlowchartGgplotHtml = function() {
            # Generate flowchart using ggflowchart or flowchart packages
            flow_data <- private$.processedData

            # Try ggflowchart first, then fallback to flowchart package
            if (requireNamespace("ggflowchart", quietly = TRUE)) {
                return(private$.generateGgflowchartHtml())
            } else if (requireNamespace("flowchart", quietly = TRUE)) {
                return(private$.generateFlowchartPackageHtml())
            } else {
                return("<p>Error: Neither ggflowchart nor flowchart packages available.<br/>Install with: install.packages('ggflowchart') or install.packages('flowchart')</p>")
            }
        },

        .generateGgflowchartHtml = function() {
            tryCatch({
                library(ggflowchart)
                library(ggplot2)

                flow_data <- private$.processedData

                # Create edge data for ggflowchart
                edges <- data.frame(
                    from = character(0),
                    to = character(0),
                    stringsAsFactors = FALSE
                )

                # Create nodes data
                nodes <- data.frame(
                    name = character(0),
                    label = character(0),
                    stringsAsFactors = FALSE
                )

                # Populate nodes and edges
                for (i in seq_along(flow_data)) {
                    step_data <- flow_data[[i]]
                    node_name <- paste0("step", i)
                    label <- paste0(step_data$step, "\n(n = ", step_data$participants, ")")

                    if (self$options$show_percentages && i > 1) {
                        label <- paste0(label, "\n[", step_data$percent_retained, "% retained]")
                    }

                    nodes <- rbind(nodes, data.frame(
                        name = node_name,
                        label = label,
                        stringsAsFactors = FALSE
                    ))

                    # Add edges (from previous step to current)
                    if (i > 1) {
                        edges <- rbind(edges, data.frame(
                            from = paste0("step", i-1),
                            to = node_name,
                            stringsAsFactors = FALSE
                        ))
                    }
                }

                # Create ggflowchart
                p <- ggflowchart(edges, node_data = nodes) +
                    theme_void() +
                    labs(title = "Study Flow Diagram")

                # Convert ggplot to HTML
                temp_file <- tempfile(fileext = ".png")
                ggsave(temp_file, p, width = 10, height = 8, dpi = 150)

                # Read image and convert to base64
                img_base64 <- base64enc::base64encode(temp_file)
                unlink(temp_file)

                html_content <- paste0(
                    "<div style='text-align: center; padding: 20px;'>",
                    "<img src='data:image/png;base64,", img_base64, "' style='max-width: 100%; height: auto;'/>",
                    "<p><em>Generated using ggflowchart package</em></p>",
                    "</div>"
                )

                return(html_content)

            }, error = function(e) {
                return(paste0("<p>Error generating ggflowchart diagram: ", e$message, "</p>"))
            })
        },

        .generateFlowchartPackageHtml = function() {
            tryCatch({
                library(flowchart)

                flow_data <- private$.processedData

                # Create data frame for flowchart package
                fc_data <- data.frame(
                    id = seq_along(flow_data),
                    step = sapply(flow_data, function(x) x$step),
                    n = sapply(flow_data, function(x) x$participants),
                    excluded = sapply(flow_data, function(x) x$excluded),
                    stringsAsFactors = FALSE
                )

                # Use flowchart package to create diagram
                fc_obj <- fc_data %>%
                    as_fc(label = "step") %>%
                    fc_draw()

                # Save to temporary file
                temp_file <- tempfile(fileext = ".png")
                png(temp_file, width = 800, height = 600, res = 150)
                print(fc_obj)
                dev.off()

                # Read image and convert to base64
                img_base64 <- base64enc::base64encode(temp_file)
                unlink(temp_file)

                html_content <- paste0(
                    "<div style='text-align: center; padding: 20px;'>",
                    "<img src='data:image/png;base64,", img_base64, "' style='max-width: 100%; height: auto;'/>",
                    "<p><em>Generated using flowchart package</em></p>",
                    "</div>"
                )

                return(html_content)

            }, error = function(e) {
                return(paste0("<p>Error generating flowchart diagram: ", e$message, "</p>"))
            })
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

            # For now, return a simple placeholder
            # Full plotting implementation will be added in Phase 2
            return(FALSE)
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
        }
    )
)
