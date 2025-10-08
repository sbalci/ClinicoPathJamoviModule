#' @title CONSORT Flow Diagram
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom grDevices png svg pdf dev.off
#' @export

consortdiagramClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "consortdiagramClass",
    inherit = consortdiagramBase,
    private = list(

        # Private fields ----
        .flowData = NULL,
        .consortPlot = NULL,
        .armData = NULL,

        # Variable name escaping utility ----
        .escapeVar = function(x) {
            if (is.null(x) || length(x) == 0) return(character(0))
            vapply(x, function(var) {
                gsub("[^A-Za-z0-9_]+", "_", make.names(var))
            }, character(1), USE.NAMES = FALSE)
        },

        # init ----
        .init = function() {
            # Check for data
            if (is.null(self$data) || nrow(self$data) == 0) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(private$.generateWelcomeMessage())
                return()
            }

            # Check for required package
            if (!requireNamespace("consort", quietly = TRUE)) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(
                    "<div style='background: #fff3cd; padding: 20px; margin: 10px; border: 1px solid #ffc107;'>
                    <h3>⚠️ Required Package Missing</h3>
                    <p>The <strong>consort</strong> package is required for creating CONSORT diagrams.</p>
                    <p>Please install it using:</p>
                    <pre style='background: #f5f5f5; padding: 10px;'>install.packages('consort')</pre>
                    </div>"
                )
                return()
            }

            # Hide welcome message when data is present
            self$results$todo$setVisible(FALSE)
        },

        # run ----
        .run = function() {
            # Checkpoint before validation
            private$.checkpoint()

            # Validate inputs
            if (!private$.validateInputs()) {
                return()
            }

            # Checkpoint before data processing
            private$.checkpoint()

            # Process participant data
            tryCatch({
                private$.processParticipantFlow()
                private$.populateFlowSummary()

                # Checkpoint before arm processing
                private$.checkpoint()

                if (!is.null(self$options$randomization_var)) {
                    private$.processArmData()
                    private$.populateArmSummary()
                }

                if (self$options$show_exclusion_details) {
                    private$.populateExclusionBreakdown()
                }

                # Checkpoint before optional outputs
                private$.checkpoint()

                if (self$options$include_interpretation) {
                    private$.generateInterpretation()
                    private$.generateClinicalSummary()
                    private$.generateAboutAnalysis()
                    private$.generateCaveatsAssumptions()
                }

                if (self$options$consort_validation) {
                    private$.generateConsortValidation()
                }

                private$.generateExportInfo()

            }, error = function(e) {
                # Show error in todo section
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(paste0(
                    "<div style='background: #f8d7da; padding: 20px; margin: 10px; border: 1px solid #f5c6cb;'>
                    <h3>❌ Error Processing Data</h3>
                    <p>", e$message, "</p>
                    <p>Please check your variable selections and data format.</p>
                    </div>"
                ))
            })
        },

        # Validation ----
        .validateInputs = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(FALSE)
            }

            # Check for participant ID
            if (is.null(self$options$participant_id) || length(self$options$participant_id) == 0) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(
                    "<div style='background: #f9f9f9; padding: 20px; margin: 10px; border: 1px solid #ddd;'>
                    <h3>Getting Started</h3>
                    <p>Please select the <strong>Participant ID</strong> variable to begin.</p>
                    <p>Then add exclusion variables for each stage of your study.</p>
                    </div>"
                )
                return(FALSE)
            }

            # Check that at least one exclusion variable is selected
            has_exclusions <- FALSE
            if (!is.null(self$options$screening_exclusions) && length(self$options$screening_exclusions) > 0) has_exclusions <- TRUE
            if (!is.null(self$options$enrollment_exclusions) && length(self$options$enrollment_exclusions) > 0) has_exclusions <- TRUE
            if (!is.null(self$options$allocation_exclusions) && length(self$options$allocation_exclusions) > 0) has_exclusions <- TRUE
            if (!is.null(self$options$followup_exclusions) && length(self$options$followup_exclusions) > 0) has_exclusions <- TRUE
            if (!is.null(self$options$analysis_exclusions) && length(self$options$analysis_exclusions) > 0) has_exclusions <- TRUE

            if (!has_exclusions) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(
                    "<div style='background: #fff3cd; padding: 20px; margin: 10px; border: 1px solid #ffc107;'>
                    <h3>⚠️ No Exclusion Variables Selected</h3>
                    <p>Please select at least one exclusion variable to create the flow diagram.</p>
                    <p>Exclusion variables should have non-NA values for excluded participants.</p>
                    </div>"
                )
                return(FALSE)
            }

            return(TRUE)
        },

        # Process participant flow ----
        .processParticipantFlow = function() {
            data <- jmvcore::naOmit(self$data)
            id_var <- private$.escapeVar(self$options$participant_id)

            # Get total participants
            total_n <- nrow(data)

            # Initialize flow data
            flow_data <- list()

            # Track remaining participants
            excluded_ids <- character(0)

            # Initial assessment
            flow_data[["initial"]] <- list(
                stage = self$options$screening_label,
                n_excluded = 0,
                n_remaining = total_n,
                pct_retained = 100,
                exclusion_details = "",
                exclusion_breakdown = list()
            )

            # Screening
            result <- private$.countExclusionsAtStage(
                self$options$screening_exclusions,
                self$options$enrollment_label,
                data, id_var, excluded_ids
            )
            excluded_ids <- c(excluded_ids, result$new_excluded_ids)
            flow_data[["screening"]] <- result

            # Enrollment
            result <- private$.countExclusionsAtStage(
                self$options$enrollment_exclusions,
                "Randomized/Enrolled",
                data, id_var, excluded_ids
            )
            excluded_ids <- c(excluded_ids, result$new_excluded_ids)
            flow_data[["enrollment"]] <- result

            # Allocation (post-randomization)
            result <- private$.countExclusionsAtStage(
                self$options$allocation_exclusions,
                self$options$allocation_label,
                data, id_var, excluded_ids
            )
            excluded_ids <- c(excluded_ids, result$new_excluded_ids)
            flow_data[["allocation"]] <- result

            # Follow-up
            result <- private$.countExclusionsAtStage(
                self$options$followup_exclusions,
                self$options$followup_label,
                data, id_var, excluded_ids
            )
            excluded_ids <- c(excluded_ids, result$new_excluded_ids)
            flow_data[["followup"]] <- result

            # Analysis
            result <- private$.countExclusionsAtStage(
                self$options$analysis_exclusions,
                self$options$analysis_label,
                data, id_var, excluded_ids
            )
            excluded_ids <- c(excluded_ids, result$new_excluded_ids)
            flow_data[["analysis"]] <- result

            private$.flowData <- flow_data
        },

        # Process arm-specific data ----
        .processArmData = function() {
            if (is.null(self$options$randomization_var)) {
                return()
            }

            data <- jmvcore::naOmit(self$data)
            id_var <- private$.escapeVar(self$options$participant_id)
            arm_var <- private$.escapeVar(self$options$randomization_var)

            # Get unique arms
            arms <- unique(data[[arm_var]])
            arms <- arms[!is.na(arms)]

            arm_data <- list()

            for (arm in arms) {
                arm_mask <- data[[arm_var]] == arm & !is.na(data[[arm_var]])
                arm_ids <- as.character(data[[id_var]][arm_mask])

                n_allocated <- length(arm_ids)

                # Count exclusions at each stage for this arm
                excluded_allocation <- private$.countArmExclusions(arm_ids, self$options$allocation_exclusions)
                excluded_followup <- private$.countArmExclusions(arm_ids, self$options$followup_exclusions)
                excluded_analysis <- private$.countArmExclusions(arm_ids, self$options$analysis_exclusions)

                n_received <- n_allocated - excluded_allocation
                n_completed_followup <- n_received - excluded_followup
                n_analyzed <- n_completed_followup - excluded_analysis

                arm_data[[as.character(arm)]] <- list(
                    arm = as.character(arm),
                    allocated = n_allocated,
                    received = n_received,
                    completed_followup = n_completed_followup,
                    analyzed = n_analyzed,
                    retention_rate = round(n_analyzed / n_allocated * 100, 1)
                )
            }

            private$.armData <- arm_data
        },

        .countArmExclusions = function(arm_ids, exclusion_vars) {
            if (is.null(exclusion_vars) || length(exclusion_vars) == 0) {
                return(0)
            }

            data <- jmvcore::naOmit(self$data)
            id_var <- private$.escapeVar(self$options$participant_id)

            excluded_count <- 0
            for (var in exclusion_vars) {
                var_escaped <- private$.escapeVar(var)
                mask <- (data[[id_var]] %in% arm_ids) & !is.na(data[[var_escaped]])
                excluded_count <- excluded_count + sum(mask)
            }

            return(excluded_count)
        },

        # Populate tables ----
        .populateFlowSummary = function() {
            if (is.null(private$.flowData)) return()

            table <- self$results$flowSummary

            # Clear existing rows
            for (key in table$rowKeys) {
                table$removeRow(rowKey = key)
            }

            row_num <- 1
            for (stage_key in names(private$.flowData)) {
                stage_data <- private$.flowData[[stage_key]]

                table$addRow(rowKey = row_num, values = list(
                    stage = stage_data$stage,
                    n_remaining = stage_data$n_remaining,
                    n_excluded = stage_data$n_excluded,
                    pct_retained = stage_data$pct_retained / 100,
                    exclusion_details = stage_data$exclusion_details
                ))

                row_num <- row_num + 1
            }
        },

        .populateArmSummary = function() {
            if (is.null(private$.armData)) return()

            table <- self$results$armSummary

            # Clear existing rows
            for (key in table$rowKeys) {
                table$removeRow(rowKey = key)
            }

            row_num <- 1
            for (arm_key in names(private$.armData)) {
                arm_data <- private$.armData[[arm_key]]

                table$addRow(rowKey = row_num, values = list(
                    arm = arm_data$arm,
                    allocated = arm_data$allocated,
                    received = arm_data$received,
                    completed_followup = arm_data$completed_followup,
                    analyzed = arm_data$analyzed,
                    retention_rate = arm_data$retention_rate / 100
                ))

                row_num <- row_num + 1
            }
        },

        .populateExclusionBreakdown = function() {
            if (is.null(private$.flowData)) return()

            table <- self$results$exclusionBreakdown

            # Clear existing rows
            for (key in table$rowKeys) {
                table$removeRow(rowKey = key)
            }

            row_num <- 1
            for (stage_key in names(private$.flowData)) {
                stage_data <- private$.flowData[[stage_key]]

                if (length(stage_data$exclusion_breakdown) > 0) {
                    for (var_name in names(stage_data$exclusion_breakdown)) {
                        counts <- stage_data$exclusion_breakdown[[var_name]]

                        for (reason in names(counts)) {
                            count_val <- counts[reason]
                            pct_val <- round(count_val / stage_data$n_remaining * 100, 1)

                            table$addRow(rowKey = row_num, values = list(
                                stage = stage_data$stage,
                                reason = reason,
                                count = count_val,
                                percentage = pct_val / 100
                            ))

                            row_num <- row_num + 1
                        }
                    }
                }
            }
        },

        # Generate interpretation ----
        .generateInterpretation = function() {
            if (is.null(private$.flowData)) return()

            total_n <- private$.flowData[["initial"]]$n_remaining
            final_n <- private$.flowData[["analysis"]]$n_remaining
            total_excluded <- total_n - final_n
            retention_rate <- round(final_n / total_n * 100, 1)

            html <- paste0(
                "<div style='background: #f0f8ff; padding: 15px; margin: 10px; border: 1px solid #ccc;'>",
                "<h4>📊 Participant Flow Summary</h4>",
                "<ul>",
                "<li><strong>Initial participants:</strong> ", total_n, "</li>",
                "<li><strong>Final analyzed:</strong> ", final_n, "</li>",
                "<li><strong>Total excluded:</strong> ", total_excluded, " (", round(total_excluded / total_n * 100, 1), "%)</li>",
                "<li><strong>Overall retention rate:</strong> ", retention_rate, "%</li>",
                "</ul>",
                "</div>"
            )

            # Add arm-specific interpretation if applicable
            if (!is.null(private$.armData) && length(private$.armData) > 0) {
                html <- paste0(html,
                    "<div style='background: #f9f9f9; padding: 15px; margin: 10px; border: 1px solid #ddd;'>",
                    "<h4>Treatment Arms</h4>",
                    "<ul>"
                )

                for (arm_key in names(private$.armData)) {
                    arm_data <- private$.armData[[arm_key]]
                    html <- paste0(html,
                        "<li><strong>", arm_data$arm, ":</strong> ",
                        arm_data$analyzed, "/", arm_data$allocated, " analyzed (",
                        arm_data$retention_rate, "% retention)</li>"
                    )
                }

                html <- paste0(html, "</ul></div>")
            }

            # Interpretation guidance
            html <- paste0(html,
                "<div style='background: #fff3cd; padding: 15px; margin: 10px; border: 1px solid #ffc107;'>",
                "<h4>💡 Interpretation Guidance</h4>",
                "<p><strong>Retention Rate Assessment:</strong></p>",
                "<ul>",
                "<li>≥80%: Excellent retention, minimal risk of bias</li>",
                "<li>60-79%: Good retention, low risk of bias if MAR</li>",
                "<li>40-59%: Moderate retention, potential for bias</li>",
                "<li><40%: Poor retention, high risk of bias</li>",
                "</ul>",
                "<p><strong>Considerations:</strong></p>",
                "<ul>",
                "<li>Compare retention rates across treatment arms</li>",
                "<li>Assess patterns in exclusion reasons</li>",
                "<li>Consider intention-to-treat vs per-protocol analyses</li>",
                "</ul>",
                "</div>"
            )

            self$results$interpretation$setContent(html)
        },

        # Generate CONSORT validation ----
        .generateConsortValidation = function() {
            checklist <- list(
                required = list(
                    "Participants assessed for eligibility" = !is.null(self$options$screening_exclusions),
                    "Participants excluded before randomization" = !is.null(self$options$enrollment_exclusions),
                    "Participants randomized to interventions" = !is.null(self$options$randomization_var),
                    "Participants who received allocated intervention" = !is.null(self$options$allocation_exclusions),
                    "Participants lost to follow-up" = !is.null(self$options$followup_exclusions),
                    "Participants analyzed for primary outcome" = !is.null(self$options$analysis_exclusions)
                ),
                recommended = list(
                    "Reasons for exclusions specified" = self$options$show_exclusion_details,
                    "Flow diagram includes title" = nchar(self$options$study_title) > 0,
                    "Multiple treatment arms shown separately" = !is.null(private$.armData) && length(private$.armData) > 1
                )
            )

            html <- paste0(
                "<div style='background: #f9f9f9; padding: 15px; margin: 10px; border: 1px solid #ddd;'>",
                "<h4>✓ CONSORT 2010 Compliance Checklist</h4>",
                "<p><strong>Required Elements:</strong></p>",
                "<ul>"
            )

            for (item in names(checklist$required)) {
                status <- if (checklist$required[[item]]) "✅" else "❌"
                html <- paste0(html, "<li>", status, " ", item, "</li>")
            }

            html <- paste0(html,
                "</ul>",
                "<p><strong>Recommended Elements:</strong></p>",
                "<ul>"
            )

            for (item in names(checklist$recommended)) {
                status <- if (checklist$recommended[[item]]) "✅" else "⚠️"
                html <- paste0(html, "<li>", status, " ", item, "</li>")
            }

            html <- paste0(html,
                "</ul>",
                "<p style='font-size: 0.9em; color: #666;'>",
                "<em>Based on CONSORT 2010 Statement (Schulz et al., 2010)</em>",
                "</p>",
                "</div>"
            )

            self$results$consortValidation$setContent(html)
        },

        # Generate export info ----
        .generateExportInfo = function() {
            format <- self$options$export_format
            width <- self$options$diagram_width
            height <- self$options$diagram_height

            format_desc <- switch(format,
                png = "PNG (High-resolution raster image, 300 DPI)",
                svg = "SVG (Scalable vector graphics, editable)",
                pdf = "PDF (Publication-ready, vector format)",
                "Unknown format"
            )

            html <- paste0(
                "<div style='background: #e7f3ff; padding: 10px; margin: 10px; border: 1px solid #b3d9ff;'>",
                "<p><strong>Export Settings:</strong></p>",
                "<ul>",
                "<li>Format: ", format_desc, "</li>",
                "<li>Dimensions: ", width, " × ", height, " pixels</li>",
                "<li>Text wrap: ", self$options$text_width, " characters</li>",
                "</ul>",
                "<p style='font-size: 0.9em;'>",
                "Right-click the diagram to save or copy the image.",
                "</p>",
                "</div>"
            )

            self$results$exportInfo$setContent(html)
        },

        # Generate clinical summary ----
        .generateClinicalSummary = function() {
            if (is.null(private$.flowData)) return()

            total_n <- private$.flowData[["initial"]]$n_remaining
            final_n <- private$.flowData[["analysis"]]$n_remaining
            total_excluded <- total_n - final_n
            retention_rate <- round(final_n / total_n * 100, 1)

            # Check for data quality issues
            quality_warnings <- character(0)

            # High attrition warning
            if (retention_rate < 60) {
                quality_warnings <- c(quality_warnings,
                    paste0("⚠️ High attrition rate (", 100 - retention_rate, "%) may introduce bias"))
            }

            # Check for arm imbalance
            if (!is.null(private$.armData) && length(private$.armData) > 1) {
                retention_rates <- sapply(private$.armData, function(x) x$retention_rate)
                if (max(retention_rates) - min(retention_rates) > 15) {
                    quality_warnings <- c(quality_warnings,
                        "⚠️ Differential retention between treatment arms detected")
                }
            }

            # Build manuscript-ready summary sentence
            summary_sentence <- paste0(
                "Of the ", total_n, " participants initially assessed for eligibility, ",
                total_excluded, " (", round(total_excluded / total_n * 100, 1), "%) were excluded, ",
                "resulting in ", final_n, " participants analyzed (retention rate: ", retention_rate, "%)."
            )

            if (!is.null(private$.armData) && length(private$.armData) > 0) {
                arm_text <- character(0)
                for (arm_key in names(private$.armData)) {
                    arm_data <- private$.armData[[arm_key]]
                    arm_text <- c(arm_text, paste0(
                        arm_data$arm, " (n=", arm_data$analyzed, ", ", arm_data$retention_rate, "% retention)"
                    ))
                }
                summary_sentence <- paste0(
                    summary_sentence, " Participants were distributed across treatment arms as follows: ",
                    paste(arm_text, collapse = "; "), "."
                )
            }

            html <- paste0(
                "<div style='background: #f0f8ff; padding: 15px; margin: 10px; border: 1px solid #4a90e2;'>",
                "<h4>📋 Manuscript-Ready Summary</h4>",
                "<div style='background: white; padding: 12px; margin: 10px 0; border-left: 3px solid #4a90e2;'>",
                "<p style='font-family: \"Times New Roman\", serif; line-height: 1.6;'>",
                summary_sentence,
                "</p>",
                "</div>",
                "<p style='font-size: 0.85em; color: #666;'>",
                "<em>📎 Copy the text above for your manuscript's methods or results section.</em>",
                "</p>"
            )

            # Add quality warnings if any
            if (length(quality_warnings) > 0) {
                html <- paste0(html,
                    "<div style='background: #fff3cd; padding: 10px; margin: 10px 0; border: 1px solid #ffc107;'>",
                    "<p><strong>Data Quality Alerts:</strong></p>",
                    "<ul style='margin: 5px 0;'>"
                )
                for (warning in quality_warnings) {
                    html <- paste0(html, "<li>", warning, "</li>")
                }
                html <- paste0(html, "</ul></div>")
            }

            html <- paste0(html, "</div>")

            self$results$clinicalSummary$setContent(html)
        },

        # Generate about analysis ----
        .generateAboutAnalysis = function() {
            html <- paste0(
                "<div style='background: #f9f9f9; padding: 15px; margin: 10px; border: 1px solid #ddd;'>",
                "<h4>📚 About CONSORT Flow Diagrams</h4>",

                "<p><strong>What is a CONSORT diagram?</strong></p>",
                "<p style='margin-left: 15px;'>",
                "The CONSORT (Consolidated Standards of Reporting Trials) flow diagram is a standardized ",
                "visual representation of participant flow through each stage of a randomized controlled trial (RCT). ",
                "It was developed to improve transparency and reproducibility in clinical trial reporting.",
                "</p>",

                "<p><strong>When to use this analysis:</strong></p>",
                "<ul style='margin-left: 15px;'>",
                "<li><strong>Randomized controlled trials</strong> - Required for RCT publications</li>",
                "<li><strong>Observational cohort studies</strong> - Adapted to show enrollment and follow-up</li>",
                "<li><strong>Quality improvement projects</strong> - Track participant retention and attrition</li>",
                "<li><strong>Regulatory submissions</strong> - Required by many journals and agencies</li>",
                "</ul>",

                "<p><strong>Key components:</strong></p>",
                "<ul style='margin-left: 15px;'>",
                "<li><strong>Enrollment</strong> - Participants assessed and screened</li>",
                "<li><strong>Allocation</strong> - Randomization to treatment arms (RCTs only)</li>",
                "<li><strong>Follow-up</strong> - Participant retention and losses</li>",
                "<li><strong>Analysis</strong> - Final participants included in statistical analysis</li>",
                "<li><strong>Exclusions</strong> - Detailed reasons for participant removal at each stage</li>",
                "</ul>",

                "<p><strong>Clinical interpretation:</strong></p>",
                "<ul style='margin-left: 15px;'>",
                "<li>High retention (≥80%) minimizes risk of attrition bias</li>",
                "<li>Balanced exclusion rates across arms suggests unbiased treatment allocation</li>",
                "<li>Clear exclusion reasons support transparent reporting</li>",
                "<li>Intention-to-treat analysis typically uses allocated participants</li>",
                "<li>Per-protocol analysis uses participants who completed the study</li>",
                "</ul>",

                "<p><strong>📖 References:</strong></p>",
                "<ul style='margin-left: 15px; font-size: 0.9em;'>",
                "<li>Schulz KF, Altman DG, Moher D. (2010). CONSORT 2010 Statement. <em>BMJ</em> 340:c332.</li>",
                "<li>Ots R. (2024). <a href='https://www.riinu.me/2024/02/consort/' target='_blank'>",
                "CONSORT diagrams in R using patient-level data</a></li>",
                "<li><a href='http://www.consort-statement.org/' target='_blank'>",
                "CONSORT Statement Official Website</a></li>",
                "</ul>",
                "</div>"
            )

            self$results$aboutAnalysis$setContent(html)
        },

        # Generate caveats and assumptions ----
        .generateCaveatsAssumptions = function() {
            if (is.null(private$.flowData)) return()

            # Analyze data quality
            total_n <- private$.flowData[["initial"]]$n_remaining
            final_n <- private$.flowData[["analysis"]]$n_remaining
            retention_rate <- round(final_n / total_n * 100, 1)

            # Identify specific issues
            issues <- list()
            strengths <- list()

            # Check retention
            if (retention_rate >= 80) {
                strengths <- c(strengths, "✅ Excellent participant retention (≥80%)")
            } else if (retention_rate < 60) {
                issues <- c(issues, "⚠️ Low retention rate may introduce attrition bias")
            }

            # Check for exclusion reasons documentation
            has_detailed_reasons <- FALSE
            for (stage_data in private$.flowData) {
                if (nchar(stage_data$exclusion_details) > 0) {
                    has_detailed_reasons <- TRUE
                    break
                }
            }

            if (has_detailed_reasons) {
                strengths <- c(strengths, "✅ Exclusion reasons documented")
            } else {
                issues <- c(issues, "⚠️ Exclusion reasons not specified - consider enabling detailed breakdown")
            }

            # Check for randomization
            if (!is.null(private$.armData) && length(private$.armData) > 0) {
                strengths <- c(strengths, "✅ Randomized trial design with treatment arms")

                # Check for differential attrition
                retention_rates <- sapply(private$.armData, function(x) x$retention_rate)
                if (max(retention_rates) - min(retention_rates) > 15) {
                    issues <- c(issues, "⚠️ Differential attrition between arms may indicate treatment tolerability issues")
                }
            }

            # Build HTML
            html <- paste0(
                "<div style='background: #fff8e1; padding: 15px; margin: 10px; border: 1px solid #ffa726;'>",
                "<h4>⚠️ Caveats & Assumptions</h4>",

                "<p><strong>Key Assumptions:</strong></p>",
                "<ul style='margin-left: 15px;'>",
                "<li><strong>Patient-level data:</strong> Each row represents one unique participant</li>",
                "<li><strong>Exclusion coding:</strong> Non-NA values indicate exclusion at that stage</li>",
                "<li><strong>Sequential exclusions:</strong> Participants excluded at earlier stages ",
                "are not re-counted at later stages</li>",
                "<li><strong>Complete data:</strong> Missing values (NA) indicate participant continued to next stage</li>",
                "</ul>",

                "<p><strong>Data Quality Assessment:</strong></p>"
            )

            # Add strengths
            if (length(strengths) > 0) {
                html <- paste0(html, "<div style='background: #d4edda; padding: 8px; margin: 5px 0; border-radius: 3px;'>")
                for (strength in strengths) {
                    html <- paste0(html, "<div style='margin: 3px 0;'>", strength, "</div>")
                }
                html <- paste0(html, "</div>")
            }

            # Add issues
            if (length(issues) > 0) {
                html <- paste0(html, "<div style='background: #fff3cd; padding: 8px; margin: 5px 0; border-radius: 3px;'>")
                for (issue in issues) {
                    html <- paste0(html, "<div style='margin: 3px 0;'>", issue, "</div>")
                }
                html <- paste0(html, "</div>")
            }

            html <- paste0(html,
                "<p><strong>Important Considerations:</strong></p>",
                "<ul style='margin-left: 15px;'>",
                "<li><strong>Missing data patterns:</strong> Assess whether missingness is random (MAR) or systematic</li>",
                "<li><strong>Intention-to-treat:</strong> Consider analyzing all randomized participants regardless of protocol adherence</li>",
                "<li><strong>Sensitivity analysis:</strong> Compare results with/without excluded participants</li>",
                "<li><strong>Reporting transparency:</strong> Document all exclusion criteria and reasons in detail</li>",
                "<li><strong>CONSORT compliance:</strong> Enable 'CONSORT 2010 Compliance Check' to verify required elements</li>",
                "</ul>",

                "<p style='font-size: 0.85em; color: #666; margin-top: 10px;'>",
                "<strong>💡 Tip:</strong> High-quality CONSORT diagrams enhance manuscript acceptance rates ",
                "and improve study reproducibility.",
                "</p>",
                "</div>"
            )

            self$results$caveatsAssumptions$setContent(html)
        },

        # Helper: Count exclusions at stage (extracted from inline) ----
        .countExclusionsAtStage = function(exclusion_vars, stage_name, data, id_var, excluded_ids) {
            if (is.null(exclusion_vars) || length(exclusion_vars) == 0) {
                total_n <- nrow(data)
                return(list(
                    stage = stage_name,
                    n_excluded = 0,
                    n_remaining = total_n - length(excluded_ids),
                    pct_retained = round((total_n - length(excluded_ids)) / total_n * 100, 1),
                    exclusion_details = "",
                    exclusion_breakdown = list(),
                    new_excluded_ids = character(0)
                ))
            }

            total_n <- nrow(data)
            newly_excluded <- character(0)
            exclusion_breakdown <- list()

            for (var in exclusion_vars) {
                var_escaped <- private$.escapeVar(var)
                excluded_mask <- !is.na(data[[var_escaped]])
                excluded_at_var <- as.character(data[[id_var]][excluded_mask])

                # Only count if not already excluded
                new_at_var <- setdiff(excluded_at_var, excluded_ids)

                if (length(new_at_var) > 0) {
                    newly_excluded <- c(newly_excluded, new_at_var)

                    # Get exclusion reason details
                    reasons <- data[[var_escaped]][excluded_mask & (data[[id_var]] %in% new_at_var)]
                    reason_counts <- table(reasons)
                    exclusion_breakdown[[var]] <- reason_counts
                }
            }

            # Get unique newly excluded
            newly_excluded <- unique(newly_excluded)

            # Format exclusion details
            details_text <- ""
            if (length(exclusion_breakdown) > 0) {
                detail_parts <- character(0)
                for (var_name in names(exclusion_breakdown)) {
                    counts <- exclusion_breakdown[[var_name]]
                    for (reason in names(counts)) {
                        detail_parts <- c(detail_parts, paste0(reason, " (n=", counts[reason], ")"))
                    }
                }
                details_text <- paste(detail_parts, collapse = "; ")
            }

            # Update excluded IDs list
            updated_excluded_ids <- c(excluded_ids, newly_excluded)

            return(list(
                stage = stage_name,
                n_excluded = length(newly_excluded),
                n_remaining = total_n - length(updated_excluded_ids),
                pct_retained = round((total_n - length(updated_excluded_ids)) / total_n * 100, 1),
                exclusion_details = details_text,
                exclusion_breakdown = exclusion_breakdown,
                new_excluded_ids = newly_excluded
            ))
        },

        # Welcome message ----
        .generateWelcomeMessage = function() {
            return(paste0(
                "<div style='background: #f9f9f9; padding: 20px; margin: 10px; border: 1px solid #ddd;'>",
                "<h3>📊 CONSORT Flow Diagram Generator</h3>",
                "<p>Create CONSORT 2010 compliant flow diagrams using the <strong>consort</strong> R package.</p>",

                "<h4>How It Works:</h4>",
                "<ol>",
                "<li><strong>Prepare your data:</strong> Each row = one participant</li>",
                "<li><strong>Create exclusion columns:</strong> Add columns for each exclusion stage",
                "<ul style='margin-top: 5px;'>",
                "<li>Non-NA value = participant excluded at that stage</li>",
                "<li>NA = participant continued to next stage</li>",
                "<li>Column values can describe exclusion reason</li>",
                "</ul>",
                "</li>",
                "<li><strong>Load data and select variables:</strong>",
                "<ul style='margin-top: 5px;'>",
                "<li>Participant ID (required)</li>",
                "<li>Exclusion variables for each stage</li>",
                "<li>Treatment arm (for RCTs)</li>",
                "</ul>",
                "</li>",
                "<li><strong>Customize and export</strong> publication-ready diagram</li>",
                "</ol>",

                "<h4>Example Data Structure:</h4>",
                "<table style='border-collapse: collapse; margin: 10px 0;'>",
                "<tr style='background: #f0f0f0;'>",
                "<th style='border: 1px solid #ccc; padding: 5px;'>ID</th>",
                "<th style='border: 1px solid #ccc; padding: 5px;'>age_exclusion</th>",
                "<th style='border: 1px solid #ccc; padding: 5px;'>consent_exclusion</th>",
                "<th style='border: 1px solid #ccc; padding: 5px;'>treatment_arm</th>",
                "<th style='border: 1px solid #ccc; padding: 5px;'>followup_loss</th>",
                "</tr>",
                "<tr>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>001</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>Too old</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "</tr>",
                "<tr>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>002</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>Treatment</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "</tr>",
                "<tr>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>003</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>Refused</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "<td style='border: 1px solid #ccc; padding: 5px;'>NA</td>",
                "</tr>",
                "</table>",

                "<p style='margin-top: 15px;'><strong>📚 Resources:</strong></p>",
                "<ul>",
                "<li><a href='https://www.riinu.me/2024/02/consort/' target='_blank'>CONSORT diagrams in R tutorial by Riinu Ots</a></li>",
                "<li><a href='http://www.consort-statement.org/' target='_blank'>CONSORT 2010 Statement</a></li>",
                "</ul>",
                "</div>"
            ))
        },

        # Plot function ----
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.flowData)) {
                return(FALSE)
            }

            # Check for consort package
            if (!requireNamespace("consort", quietly = TRUE)) {
                # Return placeholder
                plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(1, 1, "consort package not installed", cex = 1.5, col = "red")
                return(TRUE)
            }

            tryCatch({
                # Prepare data for consort package
                # The consort package expects patient-level data with exclusion columns
                data <- jmvcore::naOmit(self$data)

                # Create a combined exclusion order list for consort_plot
                # This maps the stages to the exclusion variables
                exclusion_order <- list()

                if (!is.null(self$options$screening_exclusions) && length(self$options$screening_exclusions) > 0) {
                    exclusion_order[[self$options$screening_label]] <- self$options$screening_exclusions
                }
                if (!is.null(self$options$enrollment_exclusions) && length(self$options$enrollment_exclusions) > 0) {
                    exclusion_order[[self$options$enrollment_label]] <- self$options$enrollment_exclusions
                }
                if (!is.null(self$options$allocation_exclusions) && length(self$options$allocation_exclusions) > 0) {
                    exclusion_order[[self$options$allocation_label]] <- self$options$allocation_exclusions
                }
                if (!is.null(self$options$followup_exclusions) && length(self$options$followup_exclusions) > 0) {
                    exclusion_order[[self$options$followup_label]] <- self$options$followup_exclusions
                }
                if (!is.null(self$options$analysis_exclusions) && length(self$options$analysis_exclusions) > 0) {
                    exclusion_order[[self$options$analysis_label]] <- self$options$analysis_exclusions
                }

                # Get all exclusion columns
                all_exclusion_cols <- unlist(exclusion_order)

                # Create consort plot
                g <- consort::consort_plot(
                    data = data,
                    orders = exclusion_order,
                    side_box = if (self$options$show_exclusion_details) all_exclusion_cols else NULL,
                    allocation = self$options$randomization_var,
                    labels = names(exclusion_order),
                    cex = 0.8,
                    text_width = self$options$text_width
                )

                # Plot it
                print(g)

                return(TRUE)

            }, error = function(e) {
                # Fallback: Create a simple ggplot diagram
                library(ggplot2)

                # Create simple flow diagram using rectangles
                plot_data <- data.frame(
                    x = 1,
                    y = seq(length(private$.flowData), 1),
                    label = sapply(private$.flowData, function(x) {
                        paste0(x$stage, "\nn = ", x$n_remaining)
                    }),
                    stringsAsFactors = FALSE
                )

                p <- ggplot(plot_data, aes(x = x, y = y)) +
                    geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3,
                                 ymin = y - 0.3, ymax = y + 0.3),
                             fill = "#f0f0f0", color = "#333333") +
                    geom_text(aes(label = label), size = 3) +
                    theme_void() +
                    labs(title = self$options$study_title) +
                    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

                print(p)
                return(TRUE)
            })
        }
    )
)
