#' @title CONSORT Flowchart
#' @importFrom R6 R6Class
#' @import jmvcore

jconsortClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jconsortClass",
    inherit = jconsortBase,
    private = list(
        .init = function() {
            private$.consortValidation <- NULL
        },
        
        .run = function() {
            # Handle both manual entry and data-driven modes
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(private$.runManualMode())
            } else {
                return(private$.runDataMode())
            }
        },
        
        .runManualMode = function() {
            # Original manual entry mode
            if (self$options$initialN == 0) {
                todo <- "
                <br>Welcome to CONSORT Flowchart Generator
                <br><br>
                This tool helps create CONSORT 2010 compliant flow diagrams for clinical trials.
                <br><br>
                <strong>Two modes available:</strong>
                <br>
                <strong>1. Manual Entry Mode (Current):</strong>
                <br>Enter numbers for each stage manually:
                <br>• Enrollment: Initial participants and exclusions
                <br>• Randomization: Number randomized and allocated to each arm
                <br>• Follow-up: Numbers lost and analyzed in each arm
                <br><br>
                <strong>2. Data-Driven Mode:</strong>
                <br>Load participant-level data and assign variables:
                <br>• Participant ID variable
                <br>• Randomization status variable  
                <br>• Treatment group variable
                <br>• Completion status variable
                <br>• Primary outcome variable
                <br><br>
                The diagram will update automatically as you enter data.
                <br><br>
                <em>💡 For complex trials with multiple exclusion reasons and validation, use Data-Driven mode!</em>
                <hr>
                "
                self$results$todo$setContent(todo)
                return()
            }

            return(private$.runManualEntry())
        },
        
        .runManualEntry = function() {
            # Prepare data for summary table
            summaryData <- list(
                stages = c("Assessed for eligibility",
                           "Not eligible",
                           "Randomized",
                           paste(self$options$arm1Label, "allocated"),
                           paste(self$options$arm2Label, "allocated")),
                numbers = c(self$options$initialN,
                            self$options$notEligibleN,
                            self$options$randomizedN,
                            self$options$arm1N,
                            self$options$arm2N)
            )

            # Calculate percentages
            summaryData$percents <- c(
                100,
                round(self$options$notEligibleN / self$options$initialN * 100, 1),
                round(self$options$randomizedN / self$options$initialN * 100, 1),
                round(self$options$arm1N / self$options$randomizedN * 100, 1),
                round(self$options$arm2N / self$options$randomizedN * 100, 1)
            )

            # Add rows to summary table
            for(i in seq_along(summaryData$stages)) {
                self$results$summary$addRow(rowKey=i, values=list(
                    stage = summaryData$stages[i],
                    n = summaryData$numbers[i],
                    percent = summaryData$percents[i]
                ))
            }

            # Generate text summary
            textSummary <- sprintf("
            CONSORT Flow Diagram Summary
            ---------------------------
            Enrollment:
            - Initially assessed: %d
            - Not eligible: %d (%d%%)
            - Reasons not eligible: %s

            Randomization:
            - Total randomized: %d (%d%%)

            Allocation:
            %s arm:
            - Allocated: %d
            - Received intervention: %d
            - Lost to follow-up: %d
            - Analyzed: %d

            %s arm:
            - Allocated: %d
            - Received intervention: %d
            - Lost to follow-up: %d
            - Analyzed: %d

            Additional notes:
            %s
            ",
                                   self$options$initialN,
                                   self$options$notEligibleN,
                                   round(self$options$notEligibleN/self$options$initialN*100),
                                   self$options$notEligibleText,
                                   self$options$randomizedN,
                                   round(self$options$randomizedN/self$options$initialN*100),
                                   self$options$arm1Label,
                                   self$options$arm1N,
                                   self$options$arm1ReceivedN,
                                   self$options$arm1LostN,
                                   self$options$arm1AnalyzedN,
                                   self$options$arm2Label,
                                   self$options$arm2N,
                                   self$options$arm2ReceivedN,
                                   self$options$arm2LostN,
                                   self$options$arm2AnalyzedN,
                                   self$options$excludedText
            )

            self$results$text$setContent(textSummary)

            # Save data for plot
            plotData <- list(
                mode = "manual",
                initial = list(
                    n = self$options$initialN,
                    excluded = self$options$notEligibleN,
                    reasons = self$options$notEligibleText
                ),
                randomized = self$options$randomizedN,
                arms = list(
                    list(
                        label = self$options$arm1Label,
                        allocated = self$options$arm1N,
                        received = self$options$arm1ReceivedN,
                        lost = self$options$arm1LostN,
                        analyzed = self$options$arm1AnalyzedN
                    ),
                    list(
                        label = self$options$arm2Label,
                        allocated = self$options$arm2N,
                        received = self$options$arm2ReceivedN,
                        lost = self$options$arm2LostN,
                        analyzed = self$options$arm2AnalyzedN
                    )
                ),
                excluded_reasons = self$options$excludedText
            )

            image <- self$results$plot
            image$setState(plotData)
        },
        
        .runDataMode = function() {
            # Data-driven CONSORT flowchart from participant-level data
            if (is.null(self$options$participant_var) || self$options$participant_var == "") {
                self$results$todo$setContent("
                    <br><strong>Data-Driven CONSORT Mode</strong>
                    <br><br>
                    Please assign the <strong>Participant ID variable</strong> to get started.
                    <br><br>
                    Required variables for CONSORT compliance:
                    <br>• Participant ID (required)
                    <br>• Randomization status 
                    <br>• Treatment group/arm
                    <br>• Study completion status
                    <br>• Primary outcome
                    <br><br>
                    Optional for enhanced reporting:
                    <br>• Exclusion reasons
                    <br>• Intervention received status
                    <br>• ITT analysis population
                    <br>• Per-protocol analysis population
                    <hr>
                ")
                return()
            }
            
            # Process the clinical trial data
            flow_data <- private$.processConsortData()
            
            # Create CONSORT flowchart using flowchart package
            if (requireNamespace('flowchart', quietly = TRUE)) {
                fc_obj <- private$.createConsortFlowchart(flow_data)
                
                # Generate summary statistics
                private$.generateConsortSummary(flow_data)
                
                # Save flowchart object for plotting
                image <- self$results$plot
                image$setState(list(
                    mode = "data",
                    flowchart = fc_obj,
                    data = flow_data
                ))
                
                # Add CONSORT validation if enabled
                if (self$options$validate_consort) {
                    validation_results <- private$.validateConsortStandards(fc_obj, flow_data)
                    private$.consortValidation <- validation_results
                    
                    # Add validation to text output
                    validation_html <- private$.generateConsortValidationHtml(validation_results)
                    current_text <- self$results$text$content
                    self$results$text$setContent(paste0(current_text, validation_html))
                }
                
            } else {
                stop("The 'flowchart' package is required for data-driven CONSORT diagrams. Please install it with: install.packages('flowchart')")
            }
            
        },
        
        .processConsortData = function() {
            # Extract and validate participant data for CONSORT compliance
            data <- self$data
            
            # Basic validation
            if (is.null(data) || nrow(data) == 0) {
                stop("No data provided")
            }
            
            # Collect variable assignments
            participant_var <- self$options$participant_var
            randomization_var <- self$options$randomization_var
            trial_group_var <- self$options$trial_group_var  
            completion_var <- self$options$completion_var
            outcome_var <- self$options$outcome_var
            exclusion_var <- self$options$exclusion_var
            
            # Calculate key statistics
            total_participants <- nrow(data)
            
            # Count randomized participants
            randomized_count <- if (!is.null(randomization_var) && randomization_var != "") {
                sum(!is.na(data[[randomization_var]]) & data[[randomization_var]] == 1, na.rm = TRUE)
            } else {
                total_participants
            }
            
            # Count by treatment groups
            group_counts <- if (!is.null(trial_group_var) && trial_group_var != "") {
                table(data[[trial_group_var]], useNA = "no")
            } else {
                NULL
            }
            
            # Count completers
            completion_count <- if (!is.null(completion_var) && completion_var != "") {
                sum(!is.na(data[[completion_var]]) & data[[completion_var]] == 1, na.rm = TRUE)
            } else {
                randomized_count
            }
            
            # Exclusion analysis
            exclusions <- if (!is.null(exclusion_var) && exclusion_var != "") {
                table(data[[exclusion_var]], useNA = "no")
            } else {
                NULL
            }
            
            return(list(
                data = data,
                total_participants = total_participants,
                randomized_count = randomized_count,
                group_counts = group_counts,
                completion_count = completion_count,
                exclusions = exclusions,
                participant_var = participant_var,
                randomization_var = randomization_var,
                trial_group_var = trial_group_var,
                completion_var = completion_var,
                outcome_var = outcome_var,
                exclusion_var = exclusion_var,
                consort_labels = self$options$consort_labels,
                show_reasons = self$options$show_reasons,
                validate_consort = self$options$validate_consort
            ))
        },
        
        .createConsortFlowchart = function(flow_data) {
            # Create CONSORT-compliant flowchart following CONSORT 2010 guidelines
            
            fc_data <- flow_data$data
            consort_labels <- if (!is.null(flow_data$consort_labels) && flow_data$consort_labels != "") {
                trimws(strsplit(flow_data$consort_labels, ",")[[1]])
            } else {
                c("Assessed for eligibility", "Randomized", "Allocated", "Analyzed")
            }
            
            # Initial assessment label
            initial_label <- if (length(consort_labels) >= 1) {
                paste(consort_labels[1], "(N =", flow_data$total_participants, ")")
            } else {
                paste("Assessed for eligibility (N =", flow_data$total_participants, ")")
            }
            
            # Initialize flowchart
            fc_obj <- fc_data %>% flowchart::as_fc(label = initial_label)
            
            # CONSORT Step 1: Randomization filter
            if (!is.null(flow_data$randomization_var) && flow_data$randomization_var != "") {
                randomized_label <- if (length(consort_labels) >= 2) {
                    consort_labels[2]
                } else {
                    "Randomized"
                }
                
                fc_obj <- fc_obj %>%
                    flowchart::fc_filter(
                        !is.na(!!rlang::sym(flow_data$randomization_var)) & !!rlang::sym(flow_data$randomization_var) == 1,
                        label = randomized_label,
                        show_exc = self$options$fc_show_exclusions,
                        round_digits = self$options$fc_round_digits,
                        offset = self$options$fc_offset,
                        offset_exc = self$options$fc_offset_exc
                    )
            }
            
            # CONSORT Step 2: Allocation/Assignment by treatment group
            if (!is.null(flow_data$trial_group_var) && flow_data$trial_group_var != "") {
                fc_obj <- fc_obj %>%
                    flowchart::fc_split(
                        !!rlang::sym(flow_data$trial_group_var),
                        show_zero = self$options$fc_show_zero,
                        round_digits = self$options$fc_round_digits,
                        offset = self$options$fc_offset
                    )
            }
            
            # CONSORT Step 3: Analysis/Completion filter
            if (!is.null(flow_data$completion_var) && flow_data$completion_var != "") {
                analysis_label <- if (length(consort_labels) >= 4) {
                    consort_labels[4]
                } else {
                    "Analyzed"
                }
                
                fc_obj <- fc_obj %>%
                    flowchart::fc_filter(
                        !is.na(!!rlang::sym(flow_data$completion_var)) & !!rlang::sym(flow_data$completion_var) == 1,
                        label = analysis_label,
                        show_exc = self$options$fc_show_exclusions,
                        round_digits = self$options$fc_round_digits,
                        offset = self$options$fc_offset,
                        offset_exc = self$options$fc_offset_exc
                    )
            }

            # Apply additional filter conditions if specified
            if (!is.null(self$options$fc_filter_condition) && self$options$fc_filter_condition != "") {
                tryCatch({
                    expr <- rlang::parse_expr(self$options$fc_filter_condition)
                    fc_obj <- fc_obj %>%
                        flowchart::fc_filter(
                            !!expr,
                            label = "Custom Filter",
                            show_exc = self$options$fc_show_exclusions,
                            round_digits = self$options$fc_round_digits,
                            offset = self$options$fc_offset,
                            offset_exc = self$options$fc_offset_exc
                        )
                }, error = function(e) {
                    warning(paste("Invalid filter condition:", self$options$fc_filter_condition))
                })
            }

            # Apply additional split variable if specified
            if (!is.null(self$options$fc_split_variable) && self$options$fc_split_variable != "") {
                fc_obj <- fc_obj %>%
                    flowchart::fc_split(
                        !!rlang::sym(self$options$fc_split_variable),
                        show_zero = self$options$fc_show_zero,
                        round_digits = self$options$fc_round_digits,
                        offset = self$options$fc_offset
                    )
            }

            # Apply enhanced theming with font customization
            fc_obj <- private$.applyFlowchartTheme(fc_obj)

            return(fc_obj)
        },
        
        .generateConsortSummary = function(flow_data) {
            # Generate summary table for data-driven mode
            
            # Clear existing rows
            for (i in self$results$summary$rowKeys) {
                self$results$summary$removeRow(rowKey = i)
            }
            
            # Add summary rows
            self$results$summary$addRow(rowKey = 1, values = list(
                stage = "Assessed for eligibility",
                n = flow_data$total_participants,
                percent = 100.0
            ))
            
            if (flow_data$randomized_count < flow_data$total_participants) {
                self$results$summary$addRow(rowKey = 2, values = list(
                    stage = "Excluded (not randomized)",
                    n = flow_data$total_participants - flow_data$randomized_count,
                    percent = round((flow_data$total_participants - flow_data$randomized_count) / flow_data$total_participants * 100, 1)
                ))
            }
            
            self$results$summary$addRow(rowKey = 3, values = list(
                stage = "Randomized",
                n = flow_data$randomized_count,
                percent = round(flow_data$randomized_count / flow_data$total_participants * 100, 1)
            ))
            
            # Add treatment group summaries
            if (!is.null(flow_data$group_counts)) {
                row_key <- 4
                for (group in names(flow_data$group_counts)) {
                    self$results$summary$addRow(rowKey = row_key, values = list(
                        stage = paste("Allocated to", group),
                        n = as.numeric(flow_data$group_counts[group]),
                        percent = round(as.numeric(flow_data$group_counts[group]) / flow_data$randomized_count * 100, 1)
                    ))
                    row_key <- row_key + 1
                }
            }
            
            # Text summary
            exclusion_text <- if (!is.null(flow_data$exclusions) && length(flow_data$exclusions) > 0) {
                paste(names(flow_data$exclusions), "(n=", flow_data$exclusions, ")", collapse = ", ")
            } else {
                "Not specified"
            }
            
            textSummary <- sprintf("
            CONSORT 2010 Flow Diagram Summary (Data-Driven)
            ----------------------------------------------
            Total participants assessed: %d
            Randomized: %d (%.1f%%)
            Excluded before randomization: %d (%.1f%%)
            Study completed: %d (%.1f%%)
            
            Treatment group allocation:
            %s
            
            Exclusion reasons: %s
            ",
                flow_data$total_participants,
                flow_data$randomized_count,
                flow_data$randomized_count / flow_data$total_participants * 100,
                flow_data$total_participants - flow_data$randomized_count,
                (flow_data$total_participants - flow_data$randomized_count) / flow_data$total_participants * 100,
                flow_data$completion_count,
                flow_data$completion_count / flow_data$randomized_count * 100,
                if (!is.null(flow_data$group_counts)) {
                    paste(names(flow_data$group_counts), "=", flow_data$group_counts, collapse = ", ")
                } else {
                    "Not specified"
                },
                exclusion_text
            )
            
            self$results$text$setContent(textSummary)
        },
        
        .validateConsortStandards = function(fc_obj, flow_data) {
            # Validate against CONSORT 2010 checklist requirements
            validation_results <- list(
                warnings = c(),
                notes = c(),
                status = "pass"
            )
            
            # Check for required elements
            if (is.null(flow_data$randomization_var) || flow_data$randomization_var == "") {
                validation_results$warnings <- c(validation_results$warnings, 
                    "No randomization variable specified")
                validation_results$status <- "warning"
            }
            
            if (is.null(flow_data$trial_group_var) || flow_data$trial_group_var == "") {
                validation_results$warnings <- c(validation_results$warnings, 
                    "No trial group/arm variable specified")
                validation_results$status <- "warning"
            }
            
            if (is.null(flow_data$completion_var) || flow_data$completion_var == "") {
                validation_results$notes <- c(validation_results$notes, 
                    "No completion/analysis variable specified")
                if (validation_results$status == "pass") validation_results$status <- "note"
            }
            
            # Check for outcome tracking
            if (is.null(flow_data$outcome_var) || flow_data$outcome_var == "") {
                validation_results$notes <- c(validation_results$notes, 
                    "No primary outcome variable specified")
                if (validation_results$status == "pass") validation_results$status <- "note"
            }
            
            # Check sample size adequacy
            if (flow_data$total_participants < 30) {
                validation_results$notes <- c(validation_results$notes,
                    "Small sample size (n < 30) - consider adequacy for analysis")
                if (validation_results$status == "pass") validation_results$status <- "note"
            }
            
            return(validation_results)
        },
        
        .generateConsortValidationHtml = function(validation_results) {
            # Generate HTML for CONSORT validation results
            
            status_color <- switch(validation_results$status,
                "pass" = "#28a745",
                "warning" = "#ffc107", 
                "note" = "#17a2b8"
            )
            
            status_icon <- switch(validation_results$status,
                "pass" = "✅",
                "warning" = "⚠️",
                "note" = "ℹ️"
            )
            
            status_text <- switch(validation_results$status,
                "pass" = "All recommended CONSORT 2010 elements present",
                "warning" = "CONSORT validation warnings found",
                "note" = "CONSORT validation notes"
            )
            
            html <- paste0(
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid ", status_color, "'>",
                "<h4>", status_icon, " CONSORT 2010 Validation</h4>",
                "<p><strong>Status:</strong> ", status_text, "</p>"
            )
            
            if (length(validation_results$warnings) > 0) {
                html <- paste0(html, "<h5>⚠️ Warnings:</h5><ul>")
                for (warning in validation_results$warnings) {
                    html <- paste0(html, "<li>", warning, "</li>")
                }
                html <- paste0(html, "</ul>")
            }
            
            if (length(validation_results$notes) > 0) {
                html <- paste0(html, "<h5>ℹ️ Notes:</h5><ul>")
                for (note in validation_results$notes) {
                    html <- paste0(html, "<li>", note, "</li>")
                }
                html <- paste0(html, "</ul>")
            }
            
            html <- paste0(html, "</div>")
            return(html)
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Get data from state
            plotData <- image$state
            if (is.null(plotData)) return()

            # Check for advanced visualization options
            render_engine <- self$options$render_engine

            if (render_engine == "html_css" || render_engine == "clinical_flow") {
                # Use enhanced HTML/CSS rendering (migrated from flowchart)
                return(private$.plotHtmlConsort(plotData))
            } else if (plotData$mode == "data") {
                # Data-driven mode: use flowchart package
                return(private$.plotDataDriven(plotData))
            } else {
                # Manual mode: use enhanced DiagrammeR with new options
                return(private$.plotEnhancedManual(plotData))
            }
        },
        
        .plotDataDriven = function(plotData) {
            # Render flowchart using flowchart package
            if (requireNamespace('flowchart', quietly = TRUE)) {
                fc_obj <- plotData$flowchart
                if (!is.null(fc_obj)) {
                    # Use flowchart package's built-in plotting
                    plot(fc_obj)
                    return(TRUE)
                }
            }
            return(FALSE)
        },
        
        .plotManual = function(plotData) {
            # Original DiagrammeR-based manual plotting
            
            # Create nodes list
            nodes <- DiagrammeR::create_node_df(
                n = 9,
                label = c(
                    sprintf("Assessed for eligibility\n(n=%d)", plotData$initial$n),
                    sprintf("Excluded (n=%d)\n%s",
                            plotData$initial$excluded,
                            plotData$initial$reasons),
                    sprintf("Randomized\n(n=%d)", plotData$randomized),
                    sprintf("%s\nAllocated (n=%d)\nReceived (n=%d)",
                            plotData$arms[[1]]$label,
                            plotData$arms[[1]]$allocated,
                            plotData$arms[[1]]$received),
                    sprintf("%s\nAllocated (n=%d)\nReceived (n=%d)",
                            plotData$arms[[2]]$label,
                            plotData$arms[[2]]$allocated,
                            plotData$arms[[2]]$received),
                    sprintf("Lost (n=%d)\n%s",
                            plotData$arms[[1]]$lost,
                            plotData$excluded_reasons),
                    sprintf("Lost (n=%d)\n%s",
                            plotData$arms[[2]]$lost,
                            plotData$excluded_reasons),
                    sprintf("Analyzed\n(n=%d)", plotData$arms[[1]]$analyzed),
                    sprintf("Analyzed\n(n=%d)", plotData$arms[[2]]$analyzed)
                ),
                shape = "rectangle",
                width = 2.5,
                height = 1,
                color = "#4472C4",
                fillcolor = "#E6F2FF",
                fontname = "Arial",
                fontsize = 10,
                x = c(2, 4, 2, 1, 3, 1, 3, 1, 3),
                y = c(1, 1, 2, 3, 3, 4, 4, 5, 5)
            )

            # Create edges list
            edges <- DiagrammeR::create_edge_df(
                from = c(1, 1, 3, 3, 4, 5, 6, 7),
                to = c(2, 3, 4, 5, 6, 7, 8, 9),
                color = "#666666"
            )

            # Create and render graph
            graph <- DiagrammeR::create_graph(
                nodes_df = nodes,
                edges_df = edges
            ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "rankdir",
                    value = "TB",
                    attr_type = "graph"
                ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "bgcolor",
                    value = "white",
                    attr_type = "graph"
                )

            DiagrammeR::render_graph(graph)
            TRUE
        },

        .plotHtmlConsort = function(plotData) {
            # Advanced HTML/CSS CONSORT diagram (migrated from flowchart clinical trial mode)
            cat("Generating HTML/CSS CONSORT diagram\n")

            # Determine if this is manual or data mode
            if (plotData$mode == "manual") {
                html_content <- private$.createHtmlConsortManual(plotData)
            } else {
                html_content <- private$.createHtmlConsortData(plotData)
            }

            # Apply the proven vartree HTML wrapper pattern
            wrapped_html <- paste0(
                '<html><head><style>',
                '#myDIV {width: 100%; max-width: 1200px; height: auto; min-height: 600px; overflow: auto; margin: 0 auto;}',
                '.consort-container { font-family: "Arial", "Helvetica", sans-serif; }',
                '.consort-title { text-align: center; color: #2c3e50; margin-bottom: 30px; font-weight: 600; border-bottom: 2px solid #007bff; padding-bottom: 10px; }',
                '.consort-node { min-width: 300px; max-width: 500px; padding: 20px; border-radius: 8px; text-align: center; margin: 15px auto; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }',
                '.enrollment { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: 3px solid #5a67d8; }',
                '.randomization { background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; border: 3px solid #e91e63; }',
                '.allocation { background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); color: white; border: 3px solid #2196f3; }',
                '.followup { background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); color: white; border: 3px solid #4caf50; }',
                '.analysis { background: linear-gradient(135deg, #fa709a 0%, #fee140 100%); color: white; border: 3px solid #ff9800; }',
                '.exclusion { background: #fff3cd; border: 2px solid #ffc107; color: #856404; font-size: 12px; font-weight: 600; margin: 10px auto; padding: 8px 16px; border-radius: 6px; max-width: 250px; }',
                '.arrow { width: 0; height: 0; border-left: 15px solid transparent; border-right: 15px solid transparent; border-top: 20px solid #007bff; margin: 15px auto; filter: drop-shadow(0 2px 4px rgba(0,0,0,0.2)); }',
                '.parallel-arms { display: flex; justify-content: space-around; max-width: 800px; margin: 0 auto; gap: 40px; }',
                '.arm { flex: 1; min-width: 300px; }',
                '.consort-stats { margin-top: 30px; padding: 20px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #28a745; }',
                '</style></head><body><div id="myDIV">',
                html_content,
                '</div></body></html>'
            )

            # Set content using Html result type (like vartree)
            self$results$consortDiagram$setContent(wrapped_html)
            return(FALSE) # Indicate we're not using standard plotting
        },

        .createHtmlConsortManual = function(plotData) {
            # Create HTML CONSORT diagram from manual entry data
            html_parts <- c()

            # Title
            if (self$options$include_title) {
                html_parts <- c(html_parts, paste0(
                    "<h2 class='consort-title'>", self$options$flowchart_title, "</h2>"
                ))
            }

            html_parts <- c(html_parts, "<div class='consort-container'>")

            # Enrollment
            initial_n <- plotData$initial$n
            excluded_n <- plotData$initial$excluded
            pct_excluded <- if (initial_n > 0) round((excluded_n / initial_n) * 100, 1) else 0

            html_parts <- c(html_parts, paste0(
                "<div class='consort-node enrollment'>",
                "<div style='font-size: 16px; font-weight: 600; margin-bottom: 8px;'>Assessed for eligibility</div>",
                "<div style='font-size: 24px; font-weight: 700;'>n = ", format(initial_n, big.mark = ","), "</div>",
                "</div>"
            ))

            # Exclusion
            if (excluded_n > 0) {
                html_parts <- c(html_parts, paste0(
                    "<div class='exclusion'>Excluded: ", format(excluded_n, big.mark = ","),
                    if (self$options$show_percentages) paste0(" (", pct_excluded, "%)") else "",
                    if (self$options$show_reasons && !is.null(plotData$initial$reasons) && plotData$initial$reasons != "")
                        paste0("<br>", plotData$initial$reasons) else "",
                    "</div>"
                ))
            }

            html_parts <- c(html_parts, "<div class='arrow'></div>")

            # Randomization
            randomized_n <- plotData$randomized
            pct_randomized <- if (initial_n > 0) round((randomized_n / initial_n) * 100, 1) else 0

            html_parts <- c(html_parts, paste0(
                "<div class='consort-node randomization'>",
                "<div style='font-size: 16px; font-weight: 600; margin-bottom: 8px;'>Randomized</div>",
                "<div style='font-size: 24px; font-weight: 700;'>n = ", format(randomized_n, big.mark = ","), "</div>",
                if (self$options$show_percentages) paste0("<div style='font-size: 14px; opacity: 0.9;'>(", pct_randomized, "%)</div>") else "",
                "</div>"
            ))

            html_parts <- c(html_parts, "<div class='arrow'></div>")

            # Parallel arms
            html_parts <- c(html_parts, "<div class='parallel-arms'>")

            for (i in 1:2) {
                arm <- plotData$arms[[i]]
                html_parts <- c(html_parts, paste0(
                    "<div class='arm'>",
                    "<div class='consort-node allocation'>",
                    "<div style='font-size: 16px; font-weight: 600; margin-bottom: 8px;'>", arm$label, "</div>",
                    "<div style='font-size: 18px; font-weight: 700;'>Allocated: ", format(arm$allocated, big.mark = ","), "</div>",
                    "<div style='font-size: 16px; margin-top: 5px;'>Received: ", format(arm$received, big.mark = ","), "</div>",
                    "</div>",
                    if (arm$lost > 0) paste0(
                        "<div class='exclusion' style='margin: 15px auto;'>Lost to follow-up: ",
                        format(arm$lost, big.mark = ","), "</div>"
                    ) else "",
                    "<div class='arrow'></div>",
                    "<div class='consort-node analysis'>",
                    "<div style='font-size: 16px; font-weight: 600; margin-bottom: 8px;'>Analyzed</div>",
                    "<div style='font-size: 22px; font-weight: 700;'>n = ", format(arm$analyzed, big.mark = ","), "</div>",
                    "</div>",
                    "</div>"
                ))
            }

            html_parts <- c(html_parts, "</div>") # Close parallel-arms

            # Summary statistics
            if (self$options$show_statistics) {
                total_analyzed <- sum(sapply(plotData$arms, function(x) x$analyzed))
                retention_rate <- if (randomized_n > 0) round((total_analyzed / randomized_n) * 100, 1) else 0

                html_parts <- c(html_parts, paste0(
                    "<div class='consort-stats'>",
                    "<h4 style='color: #28a745; margin-top: 0; margin-bottom: 15px;'>📊 CONSORT Summary</h4>",
                    "<div style='display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 15px; text-align: center;'>",
                    "<div>",
                    "<div style='font-size: 24px; font-weight: 700; color: #007bff;'>", format(initial_n, big.mark = ","), "</div>",
                    "<div style='font-size: 12px; color: #6c757d; font-weight: 600;'>ASSESSED</div>",
                    "</div>",
                    "<div>",
                    "<div style='font-size: 24px; font-weight: 700; color: #e91e63;'>", format(randomized_n, big.mark = ","), "</div>",
                    "<div style='font-size: 12px; color: #6c757d; font-weight: 600;'>RANDOMIZED</div>",
                    "</div>",
                    "<div>",
                    "<div style='font-size: 24px; font-weight: 700; color: #28a745;'>", format(total_analyzed, big.mark = ","), "</div>",
                    "<div style='font-size: 12px; color: #6c757d; font-weight: 600;'>ANALYZED</div>",
                    "</div>",
                    "</div>",
                    "<div style='text-align: center; margin-top: 15px; padding-top: 15px; border-top: 1px solid #dee2e6;'>",
                    "<span style='font-size: 18px; font-weight: 600; color: ",
                    if (retention_rate >= 80) "#28a745" else if (retention_rate >= 60) "#ffc107" else "#dc3545",
                    ";'>", retention_rate, "% retention rate</span>",
                    "</div>",
                    "</div>"
                ))
            }

            html_parts <- c(html_parts, "</div>") # Close consort-container
            return(paste(html_parts, collapse = ""))
        },

        .createHtmlConsortData = function(plotData) {
            # Create HTML CONSORT diagram from data-driven analysis
            # This would implement data-driven CONSORT visualization
            # For now, return a placeholder
            return(paste0(
                "<div class='consort-container'>",
                "<h3>Data-Driven CONSORT (HTML Mode)</h3>",
                "<p>Advanced HTML visualization for data-driven CONSORT diagrams will be implemented here.</p>",
                "<p>Current data: ", length(plotData), " participants analyzed</p>",
                "</div>"
            ))
        },

        .plotEnhancedManual = function(plotData) {
            # Enhanced DiagrammeR plotting with new styling options
            color_scheme <- self$options$color_scheme
            node_width <- self$options$node_width / 100  # Convert to DiagrammeR units
            node_height <- self$options$node_height / 100
            font_size <- self$options$font_size

            # Color scheme selection
            colors <- switch(color_scheme,
                "clinical" = list(fill = "#E6F2FF", border = "#4472C4"),
                "neutral" = list(fill = "#F5F5F5", border = "#666666"),
                "accessible" = list(fill = "#FFF7E6", border = "#CC8800"),
                "publication" = list(fill = "#FFFFFF", border = "#000000"),
                list(fill = "#E6F2FF", border = "#4472C4") # default
            )

            # Create enhanced nodes with new options
            nodes <- DiagrammeR::create_node_df(
                n = 9,
                label = c(
                    sprintf("Assessed for eligibility%s\n(n=%d)",
                            if (self$options$show_percentages) "\n(100%)" else "",
                            plotData$initial$n),
                    sprintf("Excluded (n=%d)%s\n%s",
                            plotData$initial$excluded,
                            if (self$options$show_percentages && plotData$initial$n > 0)
                                sprintf("\n(%.1f%%)", (plotData$initial$excluded/plotData$initial$n)*100) else "",
                            if (self$options$show_reasons) plotData$initial$reasons else ""),
                    sprintf("Randomized\n(n=%d)%s", plotData$randomized,
                            if (self$options$show_percentages && plotData$initial$n > 0)
                                sprintf("\n(%.1f%%)", (plotData$randomized/plotData$initial$n)*100) else ""),
                    sprintf("%s\nAllocated (n=%d)\nReceived (n=%d)",
                            plotData$arms[[1]]$label,
                            plotData$arms[[1]]$allocated,
                            plotData$arms[[1]]$received),
                    sprintf("%s\nAllocated (n=%d)\nReceived (n=%d)",
                            plotData$arms[[2]]$label,
                            plotData$arms[[2]]$allocated,
                            plotData$arms[[2]]$received),
                    sprintf("Lost (n=%d)\n%s",
                            plotData$arms[[1]]$lost,
                            if (self$options$show_reasons) plotData$excluded_reasons else ""),
                    sprintf("Lost (n=%d)\n%s",
                            plotData$arms[[2]]$lost,
                            if (self$options$show_reasons) plotData$excluded_reasons else ""),
                    sprintf("Analyzed\n(n=%d)", plotData$arms[[1]]$analyzed),
                    sprintf("Analyzed\n(n=%d)", plotData$arms[[2]]$analyzed)
                ),
                shape = "rectangle",
                width = node_width,
                height = node_height,
                color = colors$border,
                fillcolor = colors$fill,
                fontname = "Arial",
                fontsize = font_size,
                x = c(2, 4, 2, 1, 3, 1, 3, 1, 3),
                y = c(1, 1, 2, 3, 3, 4, 4, 5, 5)
            )

            # Create edges with enhanced styling
            edges <- DiagrammeR::create_edge_df(
                from = c(1, 1, 3, 3, 4, 5, 6, 7),
                to = c(2, 3, 4, 5, 6, 7, 8, 9),
                color = colors$border,
                penwidth = 2
            )

            # Create graph with title if requested
            graph <- DiagrammeR::create_graph(
                nodes_df = nodes,
                edges_df = edges
            ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "rankdir",
                    value = "TB",
                    attr_type = "graph"
                ) %>%
                DiagrammeR::add_global_graph_attrs(
                    attr = "bgcolor",
                    value = "white",
                    attr_type = "graph"
                )

            # Add title if requested
            if (self$options$include_title) {
                graph <- graph %>%
                    DiagrammeR::add_global_graph_attrs(
                        attr = "label",
                        value = self$options$flowchart_title,
                        attr_type = "graph"
                    ) %>%
                    DiagrammeR::add_global_graph_attrs(
                        attr = "labelloc",
                        value = "t",
                        attr_type = "graph"
                    )
            }

            DiagrammeR::render_graph(graph)
            TRUE
        },

        .applyFlowchartTheme = function(fc_obj) {
            # Apply enhanced theming and font customization to flowchart object

            # Check if flowchart package supports theming
            if (requireNamespace("flowchart", quietly = TRUE)) {

                tryCatch({
                    # Apply custom title if specified
                    if (!is.null(self$options$fc_title) && self$options$fc_title != "") {
                        # Set title using flowchart package capabilities
                        attr(fc_obj, "title") <- self$options$fc_title
                    }

                    # Apply font customization through theme modifications
                    theme_options <- list(
                        text_fface = self$options$fc_text_fface,
                        text_ffamily = self$options$fc_text_ffamily,
                        text_padding = self$options$fc_text_padding
                    )

                    # Store theme options as attributes for later use in plotting
                    attr(fc_obj, "theme_options") <- theme_options

                }, error = function(e) {
                    warning("Could not apply flowchart theme: ", e$message)
                })
            }

            return(fc_obj)
        },

        .exportFlowchartPackage = function(fc_obj) {
            # Export flowchart using flowchart package export capabilities

            if (requireNamespace("flowchart", quietly = TRUE)) {
                tryCatch({
                    # Use flowchart package export function if available
                    export_params <- list(
                        format = self$options$fc_export_format,
                        width = self$options$fc_export_width,
                        height = self$options$fc_export_height
                    )

                    # Apply export settings
                    if (exists("fc_export", where = asNamespace("flowchart"))) {
                        fc_obj %>% flowchart::fc_export(
                            format = export_params$format,
                            width = export_params$width,
                            height = export_params$height
                        )
                    }

                }, error = function(e) {
                    warning("Could not export flowchart: ", e$message)
                })
            }

            return(fc_obj)
        }
    )
)
