#' @title Q-TWiST Analysis
#' @description Quality-adjusted Time Without Symptoms or Toxicity Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import ggplot2
#' @importFrom dplyr mutate filter select group_by summarise n
#' @importFrom tidyr pivot_longer
#' @importFrom stats quantile sd median
#' @export

qtwistClass <- R6::R6Class(
    "qtwistClass",
    inherit = qtwistBase,
    private = list(

        # Store calculated results
        .state_partition_results = NULL,
        .qtwist_results = NULL,
        .bootstrap_results = NULL,
        .sensitivity_results = NULL,

        # ========================================
        # INITIALIZATION
        # ========================================
        .init = function() {

            # Populate welcome message
            private$.populateWelcomeMessage()

            # Populate methodology explanation if requested
            if (self$options$showExplanations) {
                private$.populateMethodologyExplanation()
            }

            # Populate clinical guidance if requested
            if (self$options$showClinicalGuidance) {
                private$.populateClinicalGuidance()
            }

            # Populate formulas if requested
            if (self$options$showFormulas) {
                private$.populateFormulas()
            }

            # Populate references if requested
            if (self$options$showReferences) {
                private$.populateReferences()
            }
        },

        # ========================================
        # WELCOME MESSAGE & TODO
        # ========================================
        .populateWelcomeMessage = function() {

            # Check variable selection status
            has_os_time <- !is.null(self$options$time_os)
            has_os_event <- !is.null(self$options$event_os)
            has_pfs_time <- !is.null(self$options$time_pfs)
            has_pfs_event <- !is.null(self$options$event_pfs)
            has_treatment <- !is.null(self$options$treatment)

            # Determine toxicity status
            tox_method <- self$options$toxicity_method
            has_toxicity <- if (tox_method == "fixed_window") {
                TRUE  # Always available for fixed window
            } else if (tox_method == "individual_duration") {
                !is.null(self$options$toxicity_duration_var)
            } else if (tox_method == "time_period") {
                !is.null(self$options$toxicity_start_var) && !is.null(self$options$toxicity_end_var)
            } else {
                FALSE
            }

            html_content <- paste0(
                "<div style='font-family: Arial, sans-serif; padding: 20px;'>",

                "<h2 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>",
                "Q-TWiST Analysis: Quality-Adjusted Survival</h2>",

                "<div style='background-color: #e8f4f8; padding: 15px; border-left: 4px solid #3498db; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #2c3e50;'>📊 What is Q-TWiST?</h3>",
                "<p><strong>Q-TWiST</strong> (Quality-adjusted Time Without Symptoms or Toxicity) ",
                "partitions overall survival into three health states with different quality-of-life weights:</p>",
                "<ul style='margin: 10px 0;'>",
                "<li><strong style='color: #e74c3c;'>TOX</strong>: Time with treatment toxicity (typically μ = 0.3-0.7)</li>",
                "<li><strong style='color: #27ae60;'>TWiST</strong>: Time without symptoms or toxicity (μ = 1.0, perfect health)</li>",
                "<li><strong style='color: #f39c12;'>REL</strong>: Time after disease relapse/progression (typically μ = 0.3-0.6)</li>",
                "</ul>",
                "<p style='margin-top: 10px;'><strong>Formula:</strong> ",
                "Q-TWiST = μ<sub>TOX</sub> × E[TOX] + μ<sub>TWiST</sub> × E[TWiST] + μ<sub>REL</sub> × E[REL]</p>",
                "</div>",

                "<div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #856404;'>⚠️ IMPORTANT: Time Variables Required</h3>",
                "<p><strong>Q-TWiST analysis requires PRECALCULATED time durations</strong> (in months), not dates.</p>",

                "<h4 style='color: #856404; margin-top: 15px;'>If you have DATES instead of durations:</h4>",
                "<ol style='margin: 10px 0; line-height: 1.8;'>",
                "<li>Use the <strong style='color: #d63031;'>Time Interval Calculator</strong> function FIRST</li>",
                "<li>Navigate to: <em>Data → Time Interval Calculator</em></li>",
                "<li>Calculate time differences between dates (e.g., enrollment to death, enrollment to progression)</li>",
                "<li>Select <strong>months</strong> as the output unit for consistency</li>",
                "<li>Save the calculated time variables in your dataset</li>",
                "<li>Then return to Q-TWiST analysis and select these time variables</li>",
                "</ol>",

                "<div style='background-color: #f8f9fa; padding: 10px; margin: 10px 0; border-radius: 5px;'>",
                "<strong>Example:</strong>",
                "<ul style='margin: 5px 0; list-style-type: none;'>",
                "<li>📅 <em>Have:</em> enrollment_date, death_date, progression_date</li>",
                "<li>⚙️ <em>Calculate:</em> os_months = (death_date - enrollment_date) / 30.44</li>",
                "<li>⚙️ <em>Calculate:</em> pfs_months = (progression_date - enrollment_date) / 30.44</li>",
                "<li>✅ <em>Use:</em> os_months and pfs_months in Q-TWiST analysis</li>",
                "</ul>",
                "</div>",
                "</div>",

                "<h3 style='color: #2c3e50; margin-top: 20px;'>📋 Variable Selection Checklist:</h3>",
                "<table style='width: 100%; border-collapse: collapse; margin: 10px 0;'>",
                "<tr style='background-color: #f8f9fa;'>",
                "<th style='text-align: left; padding: 8px; border: 1px solid #dee2e6;'>Variable Type</th>",
                "<th style='text-align: center; padding: 8px; border: 1px solid #dee2e6;'>Status</th>",
                "<th style='text-align: left; padding: 8px; border: 1px solid #dee2e6;'>Notes</th>",
                "</tr>",

                "<tr>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Overall Survival Time</strong></td>",
                "<td style='text-align: center; padding: 8px; border: 1px solid #dee2e6;'>",
                ifelse(has_os_time, "✅", "❌"),
                "</td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Time to death or censoring (months)</td>",
                "</tr>",

                "<tr style='background-color: #f8f9fa;'>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>OS Event Indicator</strong></td>",
                "<td style='text-align: center; padding: 8px; border: 1px solid #dee2e6;'>",
                ifelse(has_os_event, "✅", "❌"),
                "</td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>1 = death, 0 = censored (or factor)</td>",
                "</tr>",

                "<tr>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>PFS Time</strong></td>",
                "<td style='text-align: center; padding: 8px; border: 1px solid #dee2e6;'>",
                ifelse(has_pfs_time, "✅", "❌"),
                "</td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Time to progression/death (months)</td>",
                "</tr>",

                "<tr style='background-color: #f8f9fa;'>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>PFS Event Indicator</strong></td>",
                "<td style='text-align: center; padding: 8px; border: 1px solid #dee2e6;'>",
                ifelse(has_pfs_event, "✅", "❌"),
                "</td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>1 = progression/death, 0 = censored</td>",
                "</tr>",

                "<tr>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Treatment Variable</strong></td>",
                "<td style='text-align: center; padding: 8px; border: 1px solid #dee2e6;'>",
                ifelse(has_treatment, "✅", "❌"),
                "</td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>Exactly 2 treatment groups required</td>",
                "</tr>",

                "<tr style='background-color: #f8f9fa;'>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'><strong>Toxicity Definition</strong></td>",
                "<td style='text-align: center; padding: 8px; border: 1px solid #dee2e6;'>",
                ifelse(has_toxicity, "✅", "❌"),
                "</td>",
                "<td style='padding: 8px; border: 1px solid #dee2e6;'>",
                "Method: ", tox_method,
                "</td>",
                "</tr>",
                "</table>",

                "<div style='background-color: #d4edda; padding: 15px; border-left: 4px solid #28a745; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #155724;'>✨ Key Features:</h3>",
                "<ul style='margin: 10px 0; line-height: 1.6;'>",
                "<li>Accounts for both <strong>quantity</strong> and <strong>quality</strong> of survival time</li>",
                "<li>Three toxicity definition methods (fixed window, individual durations, time periods)</li>",
                "<li>Sensitivity analysis across utility weight assumptions</li>",
                "<li>Bootstrap confidence intervals for robust inference</li>",
                "<li>Visual comparison with partitioned survival plots</li>",
                "<li>Threshold analysis for clinical decision-making</li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #d1ecf1; padding: 15px; border-left: 4px solid #17a2b8; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #0c5460;'>📚 Clinical Applications:</h3>",
                "<ul style='margin: 10px 0; line-height: 1.6;'>",
                "<li><strong>Adjuvant Therapy:</strong> Balance survival benefit vs. toxicity burden</li>",
                "<li><strong>Treatment Comparison:</strong> When OS similar but quality differs</li>",
                "<li><strong>Shared Decision-Making:</strong> Patient-relevant quality metrics</li>",
                "<li><strong>Regulatory Submissions:</strong> FDA/EMA recognized endpoint</li>",
                "<li><strong>Cost-Effectiveness:</strong> QALY-like measure for economic evaluation</li>",
                "</ul>",
                "</div>",

                ifelse(has_os_time && has_os_event && has_pfs_time && has_pfs_event && has_treatment && has_toxicity,
                    paste0(
                        "<div style='background-color: #d4edda; padding: 15px; border: 2px solid #28a745; margin: 15px 0; border-radius: 5px;'>",
                        "<h3 style='margin-top: 0; color: #155724;'>✅ Ready to Analyze!</h3>",
                        "<p style='margin: 0;'>All required variables are selected. ",
                        "Configure utility weights and analysis options, then click <strong>Run Analysis</strong>.</p>",
                        "</div>"
                    ),
                    paste0(
                        "<div style='background-color: #f8d7da; padding: 15px; border: 2px solid #dc3545; margin: 15px 0; border-radius: 5px;'>",
                        "<h3 style='margin-top: 0; color: #721c24;'>⚠️ Incomplete Selection</h3>",
                        "<p style='margin: 0;'>Please select all required variables (marked with ❌ above) to begin analysis.</p>",
                        "</div>"
                    )
                ),

                "<hr style='margin: 20px 0; border: none; border-top: 1px solid #dee2e6;'>",
                "<p style='font-size: 0.9em; color: #6c757d; margin: 10px 0;'>",
                "<strong>Need Help?</strong> See the <em>Methodology Explanation</em> section below ",
                "or consult Gelber RD, Goldhirsch A (1986) <em>J Clin Oncol</em> 4(12):1772-1779.",
                "</p>",
                "</div>"
            )

            self$results$welcomeMessage$setContent(html_content)
        },

        # ========================================
        # METHODOLOGY EXPLANATION
        # ========================================
        .populateMethodologyExplanation = function() {

            html_content <- paste0(
                "<div style='font-family: Arial, sans-serif; padding: 20px;'>",

                "<h2 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>",
                "Q-TWiST Methodology</h2>",

                "<h3 style='color: #2c3e50;'>Statistical Foundation</h3>",
                "<p>Q-TWiST was developed by Gelber and Goldhirsch (1986) to evaluate cancer treatments ",
                "that may extend survival but with different quality-of-life profiles. The method recognizes ",
                "that not all survival time is equivalent in quality.</p>",

                "<h3 style='color: #2c3e50;'>Three Health States</h3>",

                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #e74c3c; margin-bottom: 5px;'>1. TOX (Toxicity State)</h4>",
                "<p style='margin-left: 20px;'>Time experiencing significant treatment-related toxicity ",
                "(typically grade 3-4 adverse events). Usually assessed during a fixed window ",
                "(e.g., first 3-6 months of treatment).</p>",
                "<p style='margin-left: 20px;'><strong>Utility weight (μ<sub>TOX</sub>):</strong> ",
                "Typically 0.3-0.7, representing reduced quality of life during toxic treatment.</p>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #27ae60; margin-bottom: 5px;'>2. TWiST (Time Without Symptoms or Toxicity)</h4>",
                "<p style='margin-left: 20px;'>The 'good quality' survival time - patient is alive, ",
                "disease-free, and not experiencing significant treatment toxicity. This is the most ",
                "valuable health state.</p>",
                "<p style='margin-left: 20px;'><strong>Utility weight (μ<sub>TWiST</sub>):</strong> ",
                "Typically 1.0 (perfect health), used as the reference state.</p>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4 style='color: #f39c12; margin-bottom: 5px;'>3. REL (Relapse/Progression State)</h4>",
                "<p style='margin-left: 20px;'>Time after disease relapse or progression. ",
                "Patient experiences symptomatic disease, may be on salvage therapy.</p>",
                "<p style='margin-left: 20px;'><strong>Utility weight (μ<sub>REL</sub>):</strong> ",
                "Typically 0.3-0.6, representing reduced quality due to disease symptoms.</p>",
                "</div>",

                "<h3 style='color: #2c3e50; margin-top: 25px;'>Mathematical Framework</h3>",

                "<div style='background-color: #f8f9fa; padding: 15px; margin: 15px 0; border-left: 4px solid #6c757d;'>",
                "<h4 style='margin-top: 0;'>Step 1: Partition Overall Survival</h4>",
                "<p>Using restricted mean survival time (RMST) methodology:</p>",
                "<ul>",
                "<li><strong>E[OS]</strong> = RMST for overall survival up to time τ</li>",
                "<li><strong>E[PFS]</strong> = RMST for progression-free survival up to time τ</li>",
                "<li><strong>E[TOX]</strong> = Mean time in toxicity state</li>",
                "</ul>",
                "<p><strong>Partition relationship:</strong></p>",
                "<p style='margin-left: 20px; font-family: monospace;'>",
                "E[TOX] + E[TWiST] + E[REL] = E[OS]</p>",
                "<p>Where:</p>",
                "<ul>",
                "<li>E[REL] = E[OS] - E[PFS] (time after progression)</li>",
                "<li>E[TWiST] = E[PFS] - E[TOX] (good quality time)</li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #e8f4f8; padding: 15px; margin: 15px 0; border-left: 4px solid #3498db;'>",
                "<h4 style='margin-top: 0;'>Step 2: Apply Utility Weights</h4>",
                "<p style='font-family: monospace; font-size: 1.1em; margin: 10px 0;'>",
                "Q-TWiST = μ<sub>TOX</sub> × E[TOX] + μ<sub>TWiST</sub> × E[TWiST] + μ<sub>REL</sub> × E[REL]",
                "</p>",
                "<p>Each health state's duration is weighted by its quality-of-life utility, ",
                "producing a single quality-adjusted survival metric in months.</p>",
                "</div>",

                "<div style='background-color: #fff3cd; padding: 15px; margin: 15px 0; border-left: 4px solid #ffc107;'>",
                "<h4 style='margin-top: 0;'>Step 3: Compare Treatments</h4>",
                "<p style='font-family: monospace;'>Δ Q-TWiST = Q-TWiST<sub>Treatment A</sub> - Q-TWiST<sub>Treatment B</sub></p>",
                "<p>Positive Δ Q-TWiST indicates Treatment A provides more quality-adjusted survival.</p>",
                "<p><strong>Bootstrap confidence intervals</strong> provide robust inference about treatment differences.</p>",
                "</div>",

                "<h3 style='color: #2c3e50; margin-top: 25px;'>Toxicity Assessment Methods</h3>",

                "<div style='margin: 15px 0;'>",
                "<h4>Method 1: Fixed Time Window (Most Common)</h4>",
                "<p style='margin-left: 20px;'>All patients assessed for toxicity during the same time period ",
                "(e.g., first 3 months). Simple and clinically interpretable.</p>",
                "<p style='margin-left: 20px;'><strong>E[TOX] = </strong> Duration of window × Probability of grade 3-4 toxicity</p>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4>Method 2: Individual Toxicity Durations</h4>",
                "<p style='margin-left: 20px;'>Each patient has specific toxicity duration recorded. ",
                "More precise but requires detailed toxicity tracking.</p>",
                "<p style='margin-left: 20px;'><strong>E[TOX] = </strong> Mean of individual toxicity durations</p>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4>Method 3: Time Period with Start/End</h4>",
                "<p style='margin-left: 20px;'>Toxicity periods defined by start and end times for each patient. ",
                "Allows for varying toxicity timing.</p>",
                "<p style='margin-left: 20px;'><strong>E[TOX] = </strong> Mean of (toxicity_end - toxicity_start)</p>",
                "</div>",

                "<h3 style='color: #2c3e50; margin-top: 25px;'>Sensitivity Analysis</h3>",
                "<p>Because utility weights are subjective, Q-TWiST analysis includes sensitivity analysis ",
                "to examine how results vary across different utility assumptions. This shows:</p>",
                "<ul>",
                "<li><strong>Robustness:</strong> Do conclusions hold across reasonable utility ranges?</li>",
                "<li><strong>Threshold values:</strong> At what utility weights does treatment preference change?</li>",
                "<li><strong>Clinical relevance:</strong> Even if statistically significant, is Δ Q-TWiST clinically meaningful?</li>",
                "</ul>",

                "<h3 style='color: #2c3e50; margin-top: 25px;'>Interpretation Guidelines</h3>",

                "<div style='background-color: #d4edda; padding: 15px; margin: 15px 0; border-left: 4px solid #28a745;'>",
                "<h4 style='margin-top: 0; color: #155724;'>Clinical Significance</h4>",
                "<p>A difference of <strong>1-2 months</strong> in Q-TWiST is generally considered ",
                "clinically meaningful, though this depends on:</p>",
                "<ul>",
                "<li>Disease setting (adjuvant vs. metastatic)</li>",
                "<li>Overall survival duration</li>",
                "<li>Treatment burden and costs</li>",
                "<li>Patient preferences</li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #d1ecf1; padding: 15px; margin: 15px 0; border-left: 4px solid #17a2b8;'>",
                "<h4 style='margin-top: 0; color: #0c5460;'>Statistical Inference</h4>",
                "<ul>",
                "<li><strong>Bootstrap CIs:</strong> Recommended over parametric methods due to complex RMST calculations</li>",
                "<li><strong>1000+ bootstrap samples:</strong> Provides stable confidence interval estimates</li>",
                "<li><strong>Two-sided tests:</strong> Tests whether Δ Q-TWiST differs from zero</li>",
                "</ul>",
                "</div>",

                "<h3 style='color: #2c3e50; margin-top: 25px;'>Key References</h3>",
                "<ol style='line-height: 1.8;'>",
                "<li>Gelber RD, Goldhirsch A (1986). A new endpoint for the assessment of adjuvant therapy ",
                "in postmenopausal women with operable breast cancer. <em>J Clin Oncol</em>, 4(12):1772-1779.</li>",

                "<li>Glasziou PP, Simes RJ, Gelber RD (1990). Quality adjusted survival analysis. ",
                "<em>Statistics in Medicine</em>, 9(11):1259-1276.</li>",

                "<li>Cole BF, Gelber RD, Goldhirsch A (2004). Cox regression models for quality adjusted survival analysis. ",
                "<em>Statistics in Medicine</em>, 23(21):3319-3337.</li>",

                "<li>Revicki DA, Feeny D, Hunt TL, Cole BF (2006). Analyzing oncology clinical trial data using ",
                "the Q-TWiST method: clinical importance and sources of information. ",
                "<em>Quality of Life Research</em>, 15(3):411-423.</li>",
                "</ol>",

                "</div>"
            )

            self$results$methodologyExplanation$setContent(html_content)
        },

        # ========================================
        # MAIN RUN FUNCTION
        # ========================================
        .run = function() {

            # Check if required variables are selected
            if (is.null(self$options$time_os) ||
                is.null(self$options$event_os) ||
                is.null(self$options$time_pfs) ||
                is.null(self$options$event_pfs) ||
                is.null(self$options$treatment)) {
                return()
            }

            # Get data and validate
            data <- private$.prepareData()
            if (is.null(data)) {
                return()
            }

            # Validate treatment has exactly 2 levels
            if (length(unique(data$treatment)) != 2) {
                stop("Treatment variable must have exactly 2 groups for comparison. ",
                     "Current number of groups: ", length(unique(data$treatment)))
            }

            # Get tau (time horizon)
            tau <- private$.determineTau(data)

            # Calculate state partitions for each treatment arm
            state_results <- private$.calculateStatePartitions(data, tau)
            if (is.null(state_results)) {
                return()
            }

            # Calculate Q-TWiST scores
            qtwist_scores <- private$.calculateQTWISTScores(state_results)

            # Store results for use in plots
            private$.state_partition_results <- state_results
            private$.qtwist_results <- qtwist_scores

            # Populate output tables
            if (self$options$show_descriptive_stats) {
                private$.populateDescriptiveStats(data)
            }

            if (self$options$show_rmst_components) {
                private$.populateRMSTComponents(state_results)
            }

            if (self$options$show_state_partition) {
                private$.populateStatePartition(state_results)
            }

            if (self$options$show_qtwist_scores) {
                private$.populateQTWISTScoresTable(qtwist_scores)
            }

            if (self$options$show_treatment_difference) {
                # Calculate treatment difference
                diff_results <- private$.calculateTreatmentDifference(state_results, qtwist_scores, data, tau)
                private$.populateTreatmentDifference(diff_results)
                private$.populateStateDifferences(state_results)
            }

            # Sensitivity analysis
            if (self$options$sensitivity_analysis && self$options$show_sensitivity_table) {
                sens_results <- private$.performSensitivityAnalysis(state_results)
                private$.populateSensitivityTable(sens_results)
                private$.sensitivity_results <- sens_results
            }

            # Threshold analysis
            if (self$options$threshold_analysis && self$options$sensitivity_analysis) {
                threshold_results <- private$.performThresholdAnalysis(state_results)
                private$.populateThresholdAnalysis(threshold_results)
            }
        },

        # ========================================
        # DATA PREPARATION
        # ========================================
        .prepareData = function() {

            tryCatch({
                # Get raw data
                raw_data <- self$data

                # Extract variables
                time_os <- jmvcore::toNumeric(raw_data[[self$options$time_os]])
                event_os <- raw_data[[self$options$event_os]]
                time_pfs <- jmvcore::toNumeric(raw_data[[self$options$time_pfs]])
                event_pfs <- raw_data[[self$options$event_pfs]]
                treatment <- raw_data[[self$options$treatment]]

                # Handle event indicators (factor or numeric)
                if (is.factor(event_os)) {
                    if (!is.null(self$options$event_os_level)) {
                        event_os_binary <- as.numeric(event_os == self$options$event_os_level)
                    } else {
                        stop("Please specify the death event level for the OS event variable")
                    }
                } else {
                    event_os_binary <- as.numeric(event_os)
                }

                if (is.factor(event_pfs)) {
                    if (!is.null(self$options$event_pfs_level)) {
                        event_pfs_binary <- as.numeric(event_pfs == self$options$event_pfs_level)
                    } else {
                        stop("Please specify the progression/death event level for the PFS event variable")
                    }
                } else {
                    event_pfs_binary <- as.numeric(event_pfs)
                }

                # Create clean dataset
                clean_data <- data.frame(
                    time_os = time_os,
                    event_os = event_os_binary,
                    time_pfs = time_pfs,
                    event_pfs = event_pfs_binary,
                    treatment = as.factor(treatment),
                    stringsAsFactors = FALSE
                )

                # Remove rows with missing values
                clean_data <- clean_data[complete.cases(clean_data), ]

                # Validate data
                if (nrow(clean_data) < 10) {
                    stop("Insufficient data after removing missing values. At least 10 complete cases required.")
                }

                # Validate PFS <= OS
                pfs_gt_os <- clean_data$time_pfs > clean_data$time_os
                if (any(pfs_gt_os, na.rm = TRUE)) {
                    n_violations <- sum(pfs_gt_os, na.rm = TRUE)
                    warning(paste(n_violations, "patients have PFS time > OS time. ",
                                "These will be corrected by setting PFS = OS."))
                    clean_data$time_pfs[pfs_gt_os] <- clean_data$time_os[pfs_gt_os]
                }

                # Validate all times are positive
                if (any(clean_data$time_os <= 0, na.rm = TRUE) || any(clean_data$time_pfs <= 0, na.rm = TRUE)) {
                    stop("All survival times must be positive (> 0)")
                }

                return(clean_data)

            }, error = function(e) {
                stop(paste("Error preparing data:", e$message))
            })
        },

        # ========================================
        # DETERMINE TAU (Time Horizon)
        # ========================================
        .determineTau = function(data) {

            tau_method <- self$options$tau_selection

            if (tau_method == "user_specified") {
                tau <- self$options$tau
            } else if (tau_method == "auto_percentile") {
                # Use 75th percentile of OS
                tau <- quantile(data$time_os, 0.75, na.rm = TRUE)
            } else if (tau_method == "max_followup") {
                # Use maximum observed follow-up
                tau <- max(data$time_os, na.rm = TRUE)
            } else {
                tau <- self$options$tau
            }

            # Validate tau
            if (tau <= 0) {
                stop("Time horizon (tau) must be positive")
            }

            if (tau > max(data$time_os, na.rm = TRUE)) {
                warning(paste("Specified tau (", round(tau, 2),
                            ") exceeds maximum observed follow-up (",
                            round(max(data$time_os, na.rm = TRUE), 2), ")"))
            }

            return(tau)
        }

        # Additional functions will continue...
        # (To be continued in next part due to length)

        ,

        # ========================================
        # RMST CALCULATION
        # ========================================
        .calculateRMST = function(time, event, tau, treatment_arm = NULL) {

            tryCatch({
                # Restrict data to tau
                time_restricted <- pmin(time, tau)
                event_restricted <- ifelse(time > tau, 0, event)

                # Fit Kaplan-Meier curve
                km_fit <- survival::survfit(
                    survival::Surv(time_restricted, event_restricted) ~ 1
                )

                # Calculate RMST as area under KM curve up to tau
                # RMST = integral of S(t) from 0 to tau

                # Get survival times and probabilities
                times <- c(0, km_fit$time)
                surv <- c(1, km_fit$surv)

                # Restrict to tau
                times <- times[times <= tau]
                surv <- surv[1:length(times)]

                # Add tau if not already present
                if (max(times) < tau) {
                    times <- c(times, tau)
                    # Last survival probability extends to tau
                    surv <- c(surv, surv[length(surv)])
                }

                # Calculate RMST as area under curve (trapezoidal rule)
                rmst <- 0
                for (i in 1:(length(times) - 1)) {
                    width <- times[i + 1] - times[i]
                    height <- (surv[i] + surv[i + 1]) / 2
                    rmst <- rmst + width * height
                }

                # Calculate SE using Greenwood's formula
                # SE(RMST) = sqrt(Var(RMST))
                # Using numerical integration of variance

                # Get Greenwood variance from survfit
                if (length(km_fit$std.err) > 0) {
                    # Approximate SE of RMST
                    # This is a simplified approach; full calculation is complex
                    se_rmst <- sqrt(sum(km_fit$std.err^2 * diff(c(0, km_fit$time, tau))^2, na.rm = TRUE))
                } else {
                    se_rmst <- NA
                }

                return(list(
                    rmst = rmst,
                    se = se_rmst,
                    tau = tau,
                    km_fit = km_fit,
                    n_events = sum(event_restricted),
                    n_total = length(time)
                ))

            }, error = function(e) {
                stop(paste("Error calculating RMST:", e$message))
            })
        },

        # ========================================
        # TOXICITY CALCULATION
        # ========================================
        .handleToxicity = function(data, tau) {

            tox_method <- self$options$toxicity_method

            if (tox_method == "fixed_window") {
                # Method 1: Fixed time window for all patients
                tox_window <- self$options$toxicity_window
                tox_prob <- self$options$toxicity_probability

                # E[TOX] = duration of window × probability of grade 3-4 toxicity
                # But only count toxicity that occurs before tau
                effective_window <- min(tox_window, tau)
                e_tox <- effective_window * tox_prob

                # Same for all patients
                data$tox_duration <- effective_window * tox_prob

            } else if (tox_method == "individual_duration") {
                # Method 2: Individual patient-specific durations
                if (is.null(self$options$toxicity_duration_var)) {
                    stop("Toxicity duration variable must be specified for individual duration method")
                }

                tox_duration_var <- jmvcore::toNumeric(self$data[[self$options$toxicity_duration_var]])
                data$tox_duration <- tox_duration_var[complete.cases(data)]

                # Cap toxicity duration at tau
                data$tox_duration <- pmin(data$tox_duration, tau)

                # If toxicity indicator provided, weight by it
                if (!is.null(self$options$toxicity_indicator_var)) {
                    tox_indicator <- self$data[[self$options$toxicity_indicator_var]]

                    if (is.factor(tox_indicator)) {
                        if (!is.null(self$options$toxicity_indicator_level)) {
                            tox_binary <- as.numeric(tox_indicator == self$options$toxicity_indicator_level)
                        } else {
                            stop("Please specify the toxicity present level")
                        }
                    } else {
                        tox_binary <- as.numeric(tox_indicator)
                    }

                    tox_binary <- tox_binary[complete.cases(data)]
                    data$tox_duration <- data$tox_duration * tox_binary
                }

                e_tox <- mean(data$tox_duration, na.rm = TRUE)

            } else if (tox_method == "time_period") {
                # Method 3: Time periods with start and end times
                if (is.null(self$options$toxicity_start_var) || is.null(self$options$toxicity_end_var)) {
                    stop("Toxicity start and end time variables must be specified for time period method")
                }

                tox_start <- jmvcore::toNumeric(self$data[[self$options$toxicity_start_var]])
                tox_end <- jmvcore::toNumeric(self$data[[self$options$toxicity_end_var]])

                tox_start <- tox_start[complete.cases(data)]
                tox_end <- tox_end[complete.cases(data)]

                # Calculate duration, ensuring positive and within tau
                tox_duration <- pmax(0, tox_end - tox_start)
                tox_duration <- pmin(tox_duration, tau)

                data$tox_duration <- tox_duration
                e_tox <- mean(tox_duration, na.rm = TRUE)

            } else {
                stop("Invalid toxicity method specified")
            }

            # Validate E[TOX] is reasonable
            if (e_tox < 0) {
                stop("Toxicity duration cannot be negative")
            }
            if (e_tox > tau) {
                warning("Mean toxicity duration exceeds tau; capping at tau")
                e_tox <- tau
            }

            return(list(
                e_tox = e_tox,
                tox_duration_by_patient = data$tox_duration,
                method = tox_method
            ))
        },

        # ========================================
        # STATE PARTITION CALCULATION
        # ========================================
        .calculateStatePartitions = function(data, tau) {

            tryCatch({
                # Get treatment levels
                treatment_levels <- levels(data$treatment)
                if (length(treatment_levels) != 2) {
                    treatment_levels <- unique(data$treatment)
                }

                results <- list()

                for (trt in treatment_levels) {
                    # Subset data for this treatment
                    trt_data <- data[data$treatment == trt, ]

                    # Calculate RMST for OS
                    rmst_os <- private$.calculateRMST(
                        time = trt_data$time_os,
                        event = trt_data$event_os,
                        tau = tau
                    )

                    # Calculate RMST for PFS
                    rmst_pfs <- private$.calculateRMST(
                        time = trt_data$time_pfs,
                        event = trt_data$event_pfs,
                        tau = tau
                    )

                    # Calculate toxicity duration
                    tox_results <- private$.handleToxicity(trt_data, tau)

                    # Q-TWiST partition:
                    # E[TOX] = mean toxicity duration
                    # E[REL] = E[OS] - E[PFS] (time after progression)
                    # E[TWiST] = E[PFS] - E[TOX] (good quality time)

                    e_tox <- tox_results$e_tox
                    e_rel <- rmst_os$rmst - rmst_pfs$rmst
                    e_twist <- rmst_pfs$rmst - e_tox

                    # Validate partition
                    total_rmst <- e_tox + e_twist + e_rel
                    partition_error <- abs(total_rmst - rmst_os$rmst)

                    if (partition_error > 0.001) {
                        warning(paste("Partition validation warning for", trt,
                                    ": E[TOX] + E[TWiST] + E[REL] =", round(total_rmst, 3),
                                    "vs E[OS] =", round(rmst_os$rmst, 3)))
                    }

                    # Ensure non-negative values
                    if (e_twist < 0) {
                        warning(paste("Negative TWiST detected for", trt,
                                    "(E[TWiST] =", round(e_twist, 3), "). ",
                                    "This may indicate toxicity duration exceeds PFS. ",
                                    "Setting E[TWiST] to 0 and adjusting E[TOX]."))
                        e_tox <- rmst_pfs$rmst
                        e_twist <- 0
                    }

                    if (e_rel < 0) {
                        warning(paste("Negative REL detected for", trt,
                                    "(E[REL] =", round(e_rel, 3), "). ",
                                    "This suggests PFS > OS, which should have been corrected earlier."))
                        e_rel <- 0
                    }

                    # Store results for this treatment
                    results[[as.character(trt)]] <- list(
                        treatment = trt,
                        n = nrow(trt_data),

                        # RMST components
                        rmst_os = rmst_os$rmst,
                        rmst_os_se = rmst_os$se,
                        rmst_pfs = rmst_pfs$rmst,
                        rmst_pfs_se = rmst_pfs$se,

                        # State partition
                        e_tox = e_tox,
                        e_twist = e_twist,
                        e_rel = e_rel,
                        total_rmst = e_tox + e_twist + e_rel,

                        # Proportions
                        tox_percent = e_tox / rmst_os$rmst,
                        twist_percent = e_twist / rmst_os$rmst,
                        rel_percent = e_rel / rmst_os$rmst,

                        # Event counts
                        os_events = sum(trt_data$event_os),
                        pfs_events = sum(trt_data$event_pfs),
                        os_event_rate = mean(trt_data$event_os),
                        pfs_event_rate = mean(trt_data$event_pfs),

                        # Median survival
                        median_os = median(trt_data$time_os),
                        median_pfs = median(trt_data$time_pfs),

                        # Store KM fits for plotting
                        km_os = rmst_os$km_fit,
                        km_pfs = rmst_pfs$km_fit,

                        # Tau
                        tau = tau
                    )
                }

                return(results)

            }, error = function(e) {
                stop(paste("Error calculating state partitions:", e$message))
            })
        },

        # ========================================
        # Q-TWiST SCORE CALCULATION
        # ========================================
        .calculateQTWISTScores = function(state_results,
                                         utility_tox = NULL,
                                         utility_twist = NULL,
                                         utility_rel = NULL) {

            # Use provided utilities or default to options
            if (is.null(utility_tox)) utility_tox <- self$options$utility_tox
            if (is.null(utility_twist)) utility_twist <- self$options$utility_twist
            if (is.null(utility_rel)) utility_rel <- self$options$utility_rel

            qtwist_results <- list()

            for (trt in names(state_results)) {
                state <- state_results[[trt]]

                # Calculate Q-TWiST
                # Q-TWiST = μ_TOX × E[TOX] + μ_TWiST × E[TWiST] + μ_REL × E[REL]
                qtwist <- (utility_tox * state$e_tox) +
                         (utility_twist * state$e_twist) +
                         (utility_rel * state$e_rel)

                # Calculate SE of Q-TWiST (simplified)
                # More accurate SE would require bootstrap
                # For now, use propagation of error from RMST SEs
                se_qtwist <- sqrt(
                    (utility_twist^2 * state$rmst_pfs_se^2) +
                    (utility_rel^2 * (state$rmst_os_se^2 + state$rmst_pfs_se^2))
                )

                # Calculate CI (using normal approximation)
                z_value <- qnorm(1 - (1 - self$options$confidence_level) / 2)
                ci_lower <- qtwist - z_value * se_qtwist
                ci_upper <- qtwist + z_value * se_qtwist

                qtwist_results[[trt]] <- list(
                    treatment = trt,
                    qtwist = qtwist,
                    se = se_qtwist,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    utility_tox = utility_tox,
                    utility_twist = utility_twist,
                    utility_rel = utility_rel,

                    # Component contributions
                    qtwist_tox = utility_tox * state$e_tox,
                    qtwist_twist = utility_twist * state$e_twist,
                    qtwist_rel = utility_rel * state$e_rel
                )
            }

            return(qtwist_results)
        },

        # ========================================
        # TREATMENT DIFFERENCE CALCULATION
        # ========================================
        .calculateTreatmentDifference = function(state_results, qtwist_scores, data, tau) {

            treatment_names <- names(state_results)
            if (length(treatment_names) != 2) {
                stop("Treatment difference requires exactly 2 treatment groups")
            }

            # Get treatment groups (first vs. second)
            trt1 <- treatment_names[1]
            trt2 <- treatment_names[2]

            # Q-TWiST difference
            delta_qtwist <- qtwist_scores[[trt2]]$qtwist - qtwist_scores[[trt1]]$qtwist

            # SE of difference
            se_diff <- sqrt(qtwist_scores[[trt1]]$se^2 + qtwist_scores[[trt2]]$se^2)

            # Bootstrap CI if requested
            if (self$options$bootstrap_ci) {
                boot_results <- private$.bootstrapQTWIST(data, tau)
                ci_lower <- boot_results$ci_lower
                ci_upper <- boot_results$ci_upper
                p_value <- boot_results$p_value
            } else {
                # Normal approximation
                z_value <- qnorm(1 - (1 - self$options$confidence_level) / 2)
                ci_lower <- delta_qtwist - z_value * se_diff
                ci_upper <- delta_qtwist + z_value * se_diff

                # Two-sided p-value
                z_stat <- delta_qtwist / se_diff
                p_value <- 2 * (1 - pnorm(abs(z_stat)))
            }

            # Determine which treatment is favored
            favors <- if (delta_qtwist > 0) {
                paste(trt2, "better")
            } else if (delta_qtwist < 0) {
                paste(trt1, "better")
            } else {
                "No difference"
            }

            # Clinical significance
            clinical_sig <- if (abs(delta_qtwist) >= 2.0) {
                "Large difference (≥2 months)"
            } else if (abs(delta_qtwist) >= 1.0) {
                "Moderate difference (≥1 month)"
            } else if (abs(delta_qtwist) >= 0.5) {
                "Small difference (≥0.5 months)"
            } else {
                "Minimal difference (<0.5 months)"
            }

            # State-specific differences
            delta_tox <- state_results[[trt2]]$e_tox - state_results[[trt1]]$e_tox
            delta_twist <- state_results[[trt2]]$e_twist - state_results[[trt1]]$e_twist
            delta_rel <- state_results[[trt2]]$e_rel - state_results[[trt1]]$e_rel

            return(list(
                comparison = paste(trt2, "vs.", trt1),
                trt1 = trt1,
                trt2 = trt2,
                delta_qtwist = delta_qtwist,
                se = se_diff,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                p_value = p_value,
                favors = favors,
                clinical_significance = clinical_sig,

                # State-specific differences
                delta_tox = delta_tox,
                delta_twist = delta_twist,
                delta_rel = delta_rel
            ))
        },

        # ========================================
        # BOOTSTRAP CONFIDENCE INTERVALS
        # ========================================
        .bootstrapQTWIST = function(data, tau) {

            n_boot <- self$options$bootstrap_samples
            set.seed(self$options$bootstrap_seed)

            treatment_names <- levels(data$treatment)
            trt1 <- treatment_names[1]
            trt2 <- treatment_names[2]

            boot_deltas <- numeric(n_boot)

            for (i in 1:n_boot) {
                # Bootstrap resample
                boot_idx <- sample(1:nrow(data), nrow(data), replace = TRUE)
                boot_data <- data[boot_idx, ]

                # Calculate state partitions for bootstrap sample
                boot_state <- private$.calculateStatePartitions(boot_data, tau)

                # Calculate Q-TWiST scores
                boot_qtwist <- private$.calculateQTWISTScores(boot_state)

                # Store difference
                boot_deltas[i] <- boot_qtwist[[trt2]]$qtwist - boot_qtwist[[trt1]]$qtwist
            }

            # Calculate percentile CI
            alpha <- 1 - self$options$confidence_level
            ci_lower <- quantile(boot_deltas, alpha / 2, na.rm = TRUE)
            ci_upper <- quantile(boot_deltas, 1 - alpha / 2, na.rm = TRUE)

            # Bootstrap p-value (proportion of resamples where delta crosses 0)
            p_value <- min(
                2 * mean(boot_deltas >= 0, na.rm = TRUE),
                2 * mean(boot_deltas <= 0, na.rm = TRUE)
            )
            p_value <- min(p_value, 1.0)

            return(list(
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                p_value = p_value,
                boot_deltas = boot_deltas,
                n_boot = n_boot
            ))
        },

        # ========================================
        # SENSITIVITY ANALYSIS
        # ========================================
        .performSensitivityAnalysis = function(state_results) {

            # Parse utility ranges
            tox_range <- as.numeric(strsplit(self$options$utility_range_tox, ",")[[1]])
            rel_range <- as.numeric(strsplit(self$options$utility_range_rel, ",")[[1]])

            # Always keep TWiST at 1.0 (reference state)
            utility_twist <- 1.0

            sens_results <- list()
            idx <- 1

            treatment_names <- names(state_results)
            trt1 <- treatment_names[1]
            trt2 <- treatment_names[2]

            for (u_tox in tox_range) {
                for (u_rel in rel_range) {
                    # Calculate Q-TWiST scores with these utilities
                    qtwist_sens <- private$.calculateQTWISTScores(
                        state_results,
                        utility_tox = u_tox,
                        utility_twist = utility_twist,
                        utility_rel = u_rel
                    )

                    # Calculate difference
                    delta_qtwist <- qtwist_sens[[trt2]]$qtwist - qtwist_sens[[trt1]]$qtwist

                    # Determine favors
                    favors <- if (delta_qtwist > 0) {
                        trt2
                    } else if (delta_qtwist < 0) {
                        trt1
                    } else {
                        "Neither"
                    }

                    # Clinical significance
                    clin_sig <- if (abs(delta_qtwist) >= 1.0) {
                        "Yes"
                    } else {
                        "No"
                    }

                    sens_results[[idx]] <- list(
                        utility_tox = u_tox,
                        utility_rel = u_rel,
                        qtwist_arm1 = qtwist_sens[[trt1]]$qtwist,
                        qtwist_arm2 = qtwist_sens[[trt2]]$qtwist,
                        delta_qtwist = delta_qtwist,
                        favors = favors,
                        clinically_significant = clin_sig
                    )

                    idx <- idx + 1
                }
            }

            return(sens_results)
        },

        # ========================================
        # THRESHOLD ANALYSIS
        # ========================================
        .performThresholdAnalysis = function(state_results) {

            treatment_names <- names(state_results)
            trt1 <- treatment_names[1]
            trt2 <- treatment_names[2]

            # Find threshold utility where treatment preference changes
            # This is a simplified implementation

            # For TOX utility (holding REL constant)
            threshold_tox <- tryCatch({
                # Difference in TOX durations
                delta_tox <- state_results[[trt2]]$e_tox - state_results[[trt1]]$e_tox
                delta_twist <- state_results[[trt2]]$e_twist - state_results[[trt1]]$e_twist
                delta_rel <- state_results[[trt2]]$e_rel - state_results[[trt1]]$e_rel

                u_rel <- self$options$utility_rel

                # Solve for u_tox where delta_qtwist = 0
                # 0 = u_tox * delta_tox + 1.0 * delta_twist + u_rel * delta_rel
                # u_tox = -(delta_twist + u_rel * delta_rel) / delta_tox

                if (abs(delta_tox) > 0.001) {
                    u_tox_threshold <- -(delta_twist + u_rel * delta_rel) / delta_tox
                    # Constrain to [0, 1]
                    u_tox_threshold <- max(0, min(1, u_tox_threshold))
                } else {
                    u_tox_threshold <- NA
                }

                u_tox_threshold
            }, error = function(e) NA)

            # For REL utility (holding TOX constant)
            threshold_rel <- tryCatch({
                delta_tox <- state_results[[trt2]]$e_tox - state_results[[trt1]]$e_tox
                delta_twist <- state_results[[trt2]]$e_twist - state_results[[trt1]]$e_twist
                delta_rel <- state_results[[trt2]]$e_rel - state_results[[trt1]]$e_rel

                u_tox <- self$options$utility_tox

                # Solve for u_rel where delta_qtwist = 0
                if (abs(delta_rel) > 0.001) {
                    u_rel_threshold <- -(delta_twist + u_tox * delta_tox) / delta_rel
                    u_rel_threshold <- max(0, min(1, u_rel_threshold))
                } else {
                    u_rel_threshold <- NA
                }

                u_rel_threshold
            }, error = function(e) NA)

            threshold_results <- list(
                list(
                    parameter = "μ_TOX (Toxicity Utility)",
                    threshold_value = threshold_tox,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = if (!is.na(threshold_tox)) {
                        paste0("Treatment preference changes when toxicity utility crosses ",
                              round(threshold_tox, 3))
                    } else {
                        "Threshold not identifiable (no TOX duration difference)"
                    }
                ),
                list(
                    parameter = "μ_REL (Relapse Utility)",
                    threshold_value = threshold_rel,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = if (!is.na(threshold_rel)) {
                        paste0("Treatment preference changes when relapse utility crosses ",
                              round(threshold_rel, 3))
                    } else {
                        "Threshold not identifiable (no REL duration difference)"
                    }
                )
            )

            return(threshold_results)
        },

        # ========================================
        # CLINICAL GUIDANCE
        # ========================================
        .populateClinicalGuidance = function() {

            html_content <- paste0(
                "<div style='font-family: Arial, sans-serif; padding: 20px;'>",

                "<h2 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>",
                "Clinical Interpretation of Q-TWiST Results</h2>",

                "<div style='background-color: #d4edda; padding: 15px; border-left: 4px solid #28a745; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #155724;'>📊 Interpreting Q-TWiST Differences</h3>",

                "<h4>What is a Clinically Meaningful Difference?</h4>",
                "<ul style='line-height: 1.8;'>",
                "<li><strong>Large Difference (≥2 months):</strong> Strongly favors one treatment; ",
                "likely to influence clinical decision-making regardless of utility assumptions</li>",

                "<li><strong>Moderate Difference (1-2 months):</strong> Clinically relevant; ",
                "treatment choice may depend on patient preferences and toxicity tolerance</li>",

                "<li><strong>Small Difference (0.5-1 month):</strong> May be meaningful in some settings; ",
                "consider treatment burden, costs, and patient-specific factors</li>",

                "<li><strong>Minimal Difference (<0.5 months):</strong> Unlikely to be clinically important; ",
                "treatments may be considered equivalent in quality-adjusted benefit</li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #d1ecf1; padding: 15px; border-left: 4px solid #17a2b8; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #0c5460;'>🔍 Analyzing Health State Contributions</h3>",

                "<p><strong>Look at the state-specific differences table to understand WHY treatments differ:</strong></p>",

                "<h4>Scenario 1: Treatment A has more TWiST</h4>",
                "<p style='margin-left: 20px;'>→ Treatment A provides better quality survival<br>",
                "→ Strong argument for Treatment A in most patients</p>",

                "<h4>Scenario 2: Treatment A has longer OS but more TOX</h4>",
                "<p style='margin-left: 20px;'>→ Survival benefit comes with toxicity burden<br>",
                "→ Q-TWiST accounts for this trade-off<br>",
                "→ Patient preferences critical</p>",

                "<h4>Scenario 3: Similar Q-TWiST but different state distributions</h4>",
                "<p style='margin-left: 20px;'>→ Treatments achieve similar quality-adjusted survival through different paths<br>",
                "→ Consider patient-specific tolerance for toxicity vs. risk of relapse</p>",
                "</div>",

                "<div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #856404;'>⚖️ Role of Sensitivity Analysis</h3>",

                "<p><strong>Sensitivity analysis is essential because utility weights are subjective.</strong></p>",

                "<h4>Robust Treatment Advantage</h4>",
                "<p style='margin-left: 20px;'>If one treatment is favored across ALL reasonable utility combinations, ",
                "the conclusion is robust to subjective assumptions.</p>",

                "<h4>Utility-Dependent Preference</h4>",
                "<p style='margin-left: 20px;'>If treatment preference changes based on utilities, ",
                "identify the threshold values. This helps with shared decision-making:</p>",
                "<ul style='margin-left: 40px;'>",
                "<li>Patients valuing toxicity avoidance highly → lower μ<sub>TOX</sub></li>",
                "<li>Patients tolerating toxicity well → higher μ<sub>TOX</sub></li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #e8f4f8; padding: 15px; border-left: 4px solid #3498db; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #0c5460;'>💡 Clinical Application Examples</h3>",

                "<h4>Adjuvant Chemotherapy Decision</h4>",
                "<p style='margin-left: 20px;'><em>Scenario:</em> Standard chemo vs. dose-dense regimen</p>",
                "<ul style='margin-left: 40px;'>",
                "<li>Dose-dense: More toxicity (higher E[TOX]) but potentially more TWiST</li>",
                "<li>Q-TWiST quantifies whether survival benefit justifies toxicity burden</li>",
                "<li>Threshold analysis: At what toxicity tolerance is dose-dense preferred?</li>",
                "</ul>",

                "<h4>Maintenance Therapy</h4>",
                "<p style='margin-left: 20px;'><em>Scenario:</em> Maintenance vs. observation</p>",
                "<ul style='margin-left: 40px;'>",
                "<li>Maintenance: Extended PFS but ongoing low-grade toxicity</li>",
                "<li>Q-TWiST shows whether delayed progression offsets reduced quality during treatment</li>",
                "</ul>",

                "<h4>Targeted Therapy vs. Chemotherapy</h4>",
                "<p style='margin-left: 20px;'><em>Scenario:</em> Newer targeted agent vs. standard chemo</p>",
                "<ul style='margin-left: 40px;'>",
                "<li>Targeted: Different toxicity profile, may have more TWiST despite similar OS</li>",
                "<li>Q-TWiST can demonstrate quality-of-life advantage even without OS benefit</li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #f8d7da; padding: 15px; border-left: 4px solid #dc3545; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #721c24;'>⚠️ Important Caveats</h3>",

                "<ul style='line-height: 1.8;'>",
                "<li><strong>Utility weights are subjective:</strong> Always perform sensitivity analysis</li>",

                "<li><strong>Time horizon matters:</strong> Results may differ at different tau values</li>",

                "<li><strong>Toxicity definition:</strong> Ensure consistent grading (typically grade 3-4 only)</li>",

                "<li><strong>Patient heterogeneity:</strong> Consider stratified analysis by risk groups</li>",

                "<li><strong>Not a replacement for OS:</strong> Q-TWiST complements, not replaces, OS analysis</li>",

                "<li><strong>Statistical significance ≠ clinical importance:</strong> ",
                "A statistically significant difference of 0.2 months may not be clinically meaningful</li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #d4edda; padding: 15px; border-left: 4px solid #28a745; margin: 15px 0;'>",
                "<h3 style='margin-top: 0; color: #155724;'>✅ Reporting Q-TWiST Results</h3>",

                "<p><strong>A complete Q-TWiST report should include:</strong></p>",
                "<ol style='line-height: 1.8;'>",
                "<li>Overall survival and PFS estimates for context</li>",
                "<li>State partition table showing E[TOX], E[TWiST], E[REL] for each arm</li>",
                "<li>Q-TWiST scores with primary utility assumptions</li>",
                "<li>Treatment difference (Δ Q-TWiST) with 95% CI and p-value</li>",
                "<li>Sensitivity analysis across reasonable utility ranges</li>",
                "<li>Clinical interpretation of magnitude and direction of difference</li>",
                "<li>Statement about robustness of conclusions</li>",
                "</ol>",
                "</div>",

                "</div>"
            )

            self$results$clinicalInterpretation$setContent(html_content)
        },

        # ========================================
        # STATISTICAL FORMULAS
        # ========================================
        .populateFormulas = function() {

            html_content <- paste0(
                "<div style='font-family: Arial, sans-serif; padding: 20px;'>",

                "<h2 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>",
                "Q-TWiST Statistical Formulas</h2>",

                "<div style='background-color: #f8f9fa; padding: 15px; margin: 15px 0;'>",
                "<h3 style='color: #2c3e50;'>1. Restricted Mean Survival Time (RMST)</h3>",

                "<p><strong>Definition:</strong> Area under the Kaplan-Meier survival curve up to time τ</p>",

                "<p style='font-family: monospace; font-size: 1.1em; margin: 15px 0;'>",
                "RMST(τ) = ∫<sub>0</sub><sup>τ</sup> S(t) dt</p>",

                "<p>Where S(t) is the Kaplan-Meier survival probability at time t</p>",

                "<p><strong>Computation:</strong> Trapezoidal rule for numerical integration</p>",
                "</div>",

                "<div style='background-color: #e8f4f8; padding: 15px; margin: 15px 0;'>",
                "<h3 style='color: #2c3e50;'>2. Health State Partition</h3>",

                "<p><strong>Overall Survival RMST:</strong></p>",
                "<p style='font-family: monospace; margin-left: 20px;'>E[OS] = RMST<sub>OS</sub>(τ)</p>",

                "<p><strong>Progression-Free Survival RMST:</strong></p>",
                "<p style='font-family: monospace; margin-left: 20px;'>E[PFS] = RMST<sub>PFS</sub>(τ)</p>",

                "<p><strong>Toxicity Duration:</strong></p>",
                "<p style='font-family: monospace; margin-left: 20px;'>E[TOX] = Mean toxicity duration (method-dependent)</p>",

                "<p><strong>Relapse/Progression Time:</strong></p>",
                "<p style='font-family: monospace; margin-left: 20px;'>E[REL] = E[OS] - E[PFS]</p>",

                "<p><strong>Time Without Symptoms or Toxicity:</strong></p>",
                "<p style='font-family: monospace; margin-left: 20px;'>E[TWiST] = E[PFS] - E[TOX]</p>",

                "<p><strong>Partition Identity:</strong></p>",
                "<p style='font-family: monospace; font-size: 1.1em; margin: 15px 20px;'>",
                "E[TOX] + E[TWiST] + E[REL] = E[OS]</p>",
                "</div>",

                "<div style='background-color: #fff3cd; padding: 15px; margin: 15px 0;'>",
                "<h3 style='color: #2c3e50;'>3. Q-TWiST Calculation</h3>",

                "<p style='font-family: monospace; font-size: 1.2em; margin: 15px 0; padding: 10px; background-color: white;'>",
                "Q-TWiST = μ<sub>TOX</sub> × E[TOX] + μ<sub>TWiST</sub> × E[TWiST] + μ<sub>REL</sub> × E[REL]</p>",

                "<p>Where:</p>",
                "<ul>",
                "<li>μ<sub>TOX</sub> ∈ [0, 1]: Utility weight for toxicity state</li>",
                "<li>μ<sub>TWiST</sub> = 1.0: Reference utility for good quality state</li>",
                "<li>μ<sub>REL</sub> ∈ [0, 1]: Utility weight for relapse state</li>",
                "</ul>",
                "</div>",

                "<div style='background-color: #d4edda; padding: 15px; margin: 15px 0;'>",
                "<h3 style='color: #2c3e50;'>4. Treatment Comparison</h3>",

                "<p><strong>Q-TWiST Difference:</strong></p>",
                "<p style='font-family: monospace; margin: 15px 20px;'>",
                "Δ Q-TWiST = Q-TWiST<sub>B</sub> - Q-TWiST<sub>A</sub></p>",

                "<p><strong>Standard Error (parametric):</strong></p>",
                "<p style='font-family: monospace; margin: 15px 20px;'>",
                "SE(Δ Q-TWiST) = √[SE(Q-TWiST<sub>A</sub>)<sup>2</sup> + SE(Q-TWiST<sub>B</sub>)<sup>2</sup>]</p>",

                "<p><strong>Test Statistic:</strong></p>",
                "<p style='font-family: monospace; margin: 15px 20px;'>",
                "Z = Δ Q-TWiST / SE(Δ Q-TWiST)</p>",

                "<p><strong>Two-sided p-value:</strong></p>",
                "<p style='font-family: monospace; margin: 15px 20px;'>",
                "p = 2 × [1 - Φ(|Z|)]</p>",

                "<p>Where Φ is the standard normal CDF</p>",
                "</div>",

                "<div style='background-color: #d1ecf1; padding: 15px; margin: 15px 0;'>",
                "<h3 style='color: #2c3e50;'>5. Bootstrap Confidence Intervals</h3>",

                "<p><strong>Algorithm:</strong></p>",
                "<ol style='line-height: 1.8;'>",
                "<li>Resample data with replacement (B bootstrap samples)</li>",
                "<li>For each resample b = 1, ..., B:",
                "<ul>",
                "<li>Calculate state partitions</li>",
                "<li>Calculate Q-TWiST scores</li>",
                "<li>Store Δ Q-TWiST<sub>b</sub></li>",
                "</ul>",
                "</li>",
                "<li>Calculate percentile CI from bootstrap distribution</li>",
                "</ol>",

                "<p><strong>Percentile CI (α = 0.05 for 95% CI):</strong></p>",
                "<p style='font-family: monospace; margin: 15px 20px;'>",
                "CI = [Δ Q-TWiST<sub>(α/2)</sub>, Δ Q-TWiST<sub>(1-α/2)</sub>]</p>",
                "</div>",

                "</div>"
            )

            self$results$statisticalFormulas$setContent(html_content)
        },

        # ========================================
        # REFERENCES
        # ========================================
        .populateReferences = function() {

            html_content <- paste0(
                "<div style='font-family: Arial, sans-serif; padding: 20px;'>",

                "<h2 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>",
                "Key References for Q-TWiST Analysis</h2>",

                "<div style='line-height: 1.8;'>",

                "<h3 style='color: #2c3e50;'>Original Q-TWiST Methodology</h3>",
                "<ol style='line-height: 2.0;'>",

                "<li>Gelber RD, Goldhirsch A. <strong>A new endpoint for the assessment of adjuvant therapy ",
                "in postmenopausal women with operable breast cancer.</strong> ",
                "<em>Journal of Clinical Oncology</em>, 1986; 4(12):1772-1779. ",
                "<a href='https://doi.org/10.1200/JCO.1986.4.12.1772' target='_blank'>DOI: 10.1200/JCO.1986.4.12.1772</a>",
                "<br><em>The seminal paper introducing Q-TWiST methodology.</em></li>",

                "<li>Glasziou PP, Simes RJ, Gelber RD. <strong>Quality adjusted survival analysis.</strong> ",
                "<em>Statistics in Medicine</em>, 1990; 9(11):1259-1276. ",
                "<a href='https://doi.org/10.1002/sim.4780091106' target='_blank'>DOI: 10.1002/sim.4780091106</a>",
                "<br><em>Statistical foundations and extensions of Q-TWiST.</em></li>",

                "</ol>",

                "<h3 style='color: #2c3e50;'>Statistical Methods & Extensions</h3>",
                "<ol start='3' style='line-height: 2.0;'>",

                "<li>Cole BF, Gelber RD, Goldhirsch A. <strong>Cox regression models for quality adjusted survival analysis.</strong> ",
                "<em>Statistics in Medicine</em>, 2004; 23(21):3319-3337. ",
                "<a href='https://doi.org/10.1002/sim.1915' target='_blank'>DOI: 10.1002/sim.1915</a>",
                "<br><em>Regression-based Q-TWiST for covariate adjustment.</em></li>",

                "<li>Revicki DA, Feeny D, Hunt TL, Cole BF. <strong>Analyzing oncology clinical trial data using ",
                "the Q-TWiST method: clinical importance and sources of information.</strong> ",
                "<em>Quality of Life Research</em>, 2006; 15(3):411-423. ",
                "<a href='https://doi.org/10.1007/s11136-005-1579-9' target='_blank'>DOI: 10.1007/s11136-005-1579-9</a>",
                "<br><em>Practical guidance for Q-TWiST implementation in clinical trials.</em></li>",

                "</ol>",

                "<h3 style='color: #2c3e50;'>Clinical Applications</h3>",
                "<ol start='5' style='line-height: 2.0;'>",

                "<li>Goldhirsch A, Gelber RD, Simes RJ, Glasziou P, Coates AS. <strong>Costs and benefits of ",
                "adjuvant therapy in breast cancer: a quality-adjusted survival analysis.</strong> ",
                "<em>Journal of Clinical Oncology</em>, 1989; 7(1):36-44. ",
                "<a href='https://doi.org/10.1200/JCO.1989.7.1.36' target='_blank'>DOI: 10.1200/JCO.1989.7.1.36</a>",
                "<br><em>Landmark application of Q-TWiST in breast cancer adjuvant therapy.</em></li>",

                "<li>Gelber RD, Cole BF, Goldhirsch A, et al. <strong>Adjuvant chemotherapy plus tamoxifen compared ",
                "with tamoxifen alone for postmenopausal breast cancer: meta-analysis of quality-adjusted survival.</strong> ",
                "<em>The Lancet</em>, 1996; 347(9008):1066-1071. ",
                "<a href='https://doi.org/10.1016/S0140-6736(96)90281-X' target='_blank'>DOI: 10.1016/S0140-6736(96)90281-X</a>",
                "<br><em>Meta-analysis using Q-TWiST methodology.</em></li>",

                "</ol>",

                "<h3 style='color: #2c3e50;'>RMST Methodology (Underlying Calculation)</h3>",
                "<ol start='7' style='line-height: 2.0;'>",

                "<li>Royston P, Parmar MKB. <strong>Restricted mean survival time: an alternative to the hazard ratio ",
                "for the design and analysis of randomized trials with a time-to-event outcome.</strong> ",
                "<em>BMC Medical Research Methodology</em>, 2013; 13:152. ",
                "<a href='https://doi.org/10.1186/1471-2288-13-152' target='_blank'>DOI: 10.1186/1471-2288-13-152</a>",
                "<br><em>Comprehensive review of RMST methods.</em></li>",

                "<li>Uno H, Claggett B, Tian L, et al. <strong>Moving beyond the hazard ratio in quantifying the ",
                "between-group difference in survival analysis.</strong> ",
                "<em>Journal of Clinical Oncology</em>, 2014; 32(22):2380-2385. ",
                "<a href='https://doi.org/10.1200/JCO.2014.55.2208' target='_blank'>DOI: 10.1200/JCO.2014.55.2208</a>",
                "<br><em>Practical application of RMST in clinical trials.</em></li>",

                "</ol>",

                "<h3 style='color: #2c3e50;'>Regulatory Perspective</h3>",
                "<ol start='9' style='line-height: 2.0;'>",

                "<li>U.S. Food and Drug Administration. <strong>Clinical Trial Endpoints for the Approval of Cancer Drugs ",
                "and Biologics: Guidance for Industry.</strong> 2018. ",
                "<a href='https://www.fda.gov/regulatory-information/search-fda-guidance-documents/' target='_blank'>FDA Guidance</a>",
                "<br><em>FDA perspective on quality-of-life adjusted endpoints.</em></li>",

                "<li>European Medicines Agency. <strong>Guideline on the evaluation of anticancer medicinal products in man.</strong> ",
                "2017. EMA/CHMP/205/95 Rev.5. ",
                "<a href='https://www.ema.europa.eu/en/documents/scientific-guideline/' target='_blank'>EMA Guideline</a>",
                "<br><em>EMA perspective on quality-adjusted survival endpoints.</em></li>",

                "</ol>",

                "<div style='background-color: #e8f4f8; padding: 15px; margin-top: 30px; border-left: 4px solid #3498db;'>",
                "<h3 style='margin-top: 0; color: #0c5460;'>📚 Additional Resources</h3>",

                "<p><strong>R Packages:</strong></p>",
                "<ul>",
                "<li><strong>survival:</strong> Kaplan-Meier estimation and survival analysis</li>",
                "<li><strong>survRM2:</strong> RMST calculation and inference</li>",
                "<li><strong>QTWiST:</strong> Dedicated Q-TWiST package (if available on CRAN)</li>",
                "</ul>",

                "<p><strong>Tutorials and Reviews:</strong></p>",
                "<ul>",
                "<li>Gelber RD, Bonetti M, Castiglione-Gertsch M, et al. Quality of life in clinical trials. ",
                "<em>Annals of Oncology</em>, 2001; 12(Suppl 3):S3-S8.</li>",
                "<li>Hwang SS, Chang VT, Fairclough DL, et al. Development of a cancer-specific health-related quality of life measure. ",
                "<em>Quality of Life Research</em>, 2003; 12(5):547-558.</li>",
                "</ul>",
                "</div>",

                "</div>",
                "</div>"
            )

            self$results$keyReferences$setContent(html_content)
        },

        # ========================================
        # TABLE POPULATION FUNCTIONS
        # ========================================

        .populateDescriptiveStats = function(data) {

            treatment_levels <- levels(data$treatment)
            table <- self$results$descriptiveStats

            for (trt in treatment_levels) {
                trt_data <- data[data$treatment == trt, ]

                row <- list(
                    Treatment = trt,
                    N = nrow(trt_data),
                    Median_OS = median(trt_data$time_os, na.rm = TRUE),
                    OS_Events = sum(trt_data$event_os),
                    OS_EventRate = mean(trt_data$event_os),
                    Median_PFS = median(trt_data$time_pfs, na.rm = TRUE),
                    PFS_Events = sum(trt_data$event_pfs),
                    PFS_EventRate = mean(trt_data$event_pfs)
                )

                table$addRow(rowKey = trt, values = row)
            }
        },

        .populateRMSTComponents = function(state_results) {

            table <- self$results$rmstComponents

            for (trt in names(state_results)) {
                state <- state_results[[trt]]

                row <- list(
                    Treatment = trt,
                    RMST_OS = state$rmst_os,
                    RMST_OS_SE = state$rmst_os_se,
                    RMST_PFS = state$rmst_pfs,
                    RMST_PFS_SE = state$rmst_pfs_se,
                    Tau = state$tau
                )

                table$addRow(rowKey = trt, values = row)
            }
        },

        .populateStatePartition = function(state_results) {

            table <- self$results$statePartition

            for (trt in names(state_results)) {
                state <- state_results[[trt]]

                row <- list(
                    Treatment = trt,
                    N = state$n,
                    E_TOX = state$e_tox,
                    E_TWIST = state$e_twist,
                    E_REL = state$e_rel,
                    Total_RMST = state$total_rmst,
                    TOX_Percent = state$tox_percent,
                    TWIST_Percent = state$twist_percent,
                    REL_Percent = state$rel_percent
                )

                table$addRow(rowKey = trt, values = row)
            }
        },

        .populateQTWISTScoresTable = function(qtwist_scores) {

            table <- self$results$qtwistScores

            for (trt in names(qtwist_scores)) {
                qtwist <- qtwist_scores[[trt]]

                row <- list(
                    Treatment = trt,
                    QTWIST = qtwist$qtwist,
                    QTWIST_SE = qtwist$se,
                    QTWIST_CI_Lower = qtwist$ci_lower,
                    QTWIST_CI_Upper = qtwist$ci_upper,
                    Utility_TOX = qtwist$utility_tox,
                    Utility_TWIST = qtwist$utility_twist,
                    Utility_REL = qtwist$utility_rel
                )

                table$addRow(rowKey = trt, values = row)
            }
        },

        .populateTreatmentDifference = function(diff_results) {

            table <- self$results$treatmentDifference

            row <- list(
                Comparison = diff_results$comparison,
                Delta_QTWIST = diff_results$delta_qtwist,
                SE = diff_results$se,
                CI_Lower = diff_results$ci_lower,
                CI_Upper = diff_results$ci_upper,
                p_value = diff_results$p_value,
                Favors = diff_results$favors,
                ClinicalSignificance = diff_results$clinical_significance
            )

            table$addRow(rowKey = "difference", values = row)
        },

        .populateStateDifferences = function(state_results) {

            treatment_names <- names(state_results)
            trt1 <- treatment_names[1]
            trt2 <- treatment_names[2]

            # Calculate state-specific differences
            delta_tox <- state_results[[trt2]]$e_tox - state_results[[trt1]]$e_tox
            delta_twist <- state_results[[trt2]]$e_twist - state_results[[trt1]]$e_twist
            delta_rel <- state_results[[trt2]]$e_rel - state_results[[trt1]]$e_rel

            # Simple SE estimates (simplified - could use bootstrap for better estimates)
            se_tox <- sqrt(state_results[[trt1]]$rmst_pfs_se^2 + state_results[[trt2]]$rmst_pfs_se^2) * 0.5
            se_twist <- sqrt(state_results[[trt1]]$rmst_pfs_se^2 + state_results[[trt2]]$rmst_pfs_se^2)
            se_rel <- sqrt(
                (state_results[[trt1]]$rmst_os_se^2 + state_results[[trt2]]$rmst_os_se^2) +
                (state_results[[trt1]]$rmst_pfs_se^2 + state_results[[trt2]]$rmst_pfs_se^2)
            )

            table <- self$results$stateDifferences

            # TOX row
            z_tox <- delta_tox / se_tox
            p_tox <- 2 * (1 - pnorm(abs(z_tox)))
            interp_tox <- if (delta_tox > 0) {
                paste(trt2, "has more toxicity time")
            } else if (delta_tox < 0) {
                paste(trt1, "has more toxicity time")
            } else {
                "No difference in toxicity time"
            }

            table$addRow(rowKey = "TOX", values = list(
                HealthState = "TOX (Toxicity)",
                Difference = delta_tox,
                SE = se_tox,
                CI_Lower = delta_tox - 1.96 * se_tox,
                CI_Upper = delta_tox + 1.96 * se_tox,
                p_value = p_tox,
                Interpretation = interp_tox
            ))

            # TWiST row
            z_twist <- delta_twist / se_twist
            p_twist <- 2 * (1 - pnorm(abs(z_twist)))
            interp_twist <- if (delta_twist > 0) {
                paste(trt2, "has more good quality time")
            } else if (delta_twist < 0) {
                paste(trt1, "has more good quality time")
            } else {
                "No difference in good quality time"
            }

            table$addRow(rowKey = "TWIST", values = list(
                HealthState = "TWiST (Good Quality)",
                Difference = delta_twist,
                SE = se_twist,
                CI_Lower = delta_twist - 1.96 * se_twist,
                CI_Upper = delta_twist + 1.96 * se_twist,
                p_value = p_twist,
                Interpretation = interp_twist
            ))

            # REL row
            z_rel <- delta_rel / se_rel
            p_rel <- 2 * (1 - pnorm(abs(z_rel)))
            interp_rel <- if (delta_rel > 0) {
                paste(trt2, "has more relapse time")
            } else if (delta_rel < 0) {
                paste(trt1, "has more relapse time")
            } else {
                "No difference in relapse time"
            }

            table$addRow(rowKey = "REL", values = list(
                HealthState = "REL (Relapse)",
                Difference = delta_rel,
                SE = se_rel,
                CI_Lower = delta_rel - 1.96 * se_rel,
                CI_Upper = delta_rel + 1.96 * se_rel,
                p_value = p_rel,
                Interpretation = interp_rel
            ))
        },

        .populateSensitivityTable = function(sens_results) {

            table <- self$results$sensitivityTable

            for (i in seq_along(sens_results)) {
                sens <- sens_results[[i]]

                row <- list(
                    Utility_TOX = sens$utility_tox,
                    Utility_REL = sens$utility_rel,
                    QTWIST_Arm1 = sens$qtwist_arm1,
                    QTWIST_Arm2 = sens$qtwist_arm2,
                    Delta_QTWIST = sens$delta_qtwist,
                    Favors = sens$favors,
                    ClinicallySignificant = sens$clinically_significant
                )

                table$addRow(rowKey = paste0("sens_", i), values = row)
            }
        },

        .populateThresholdAnalysis = function(threshold_results) {

            table <- self$results$thresholdAnalysis

            for (i in seq_along(threshold_results)) {
                threshold <- threshold_results[[i]]

                row <- list(
                    Parameter = threshold$parameter,
                    ThresholdValue = threshold$threshold_value,
                    CI_Lower = threshold$ci_lower,
                    CI_Upper = threshold$ci_upper,
                    Interpretation = threshold$interpretation
                )

                table$addRow(rowKey = paste0("threshold_", i), values = row)
            }
        },

        # ========================================
        # PLOTTING FUNCTIONS
        # ========================================

        .plotPartitionedSurvival = function(image, ...) {

            if (is.null(private$.state_partition_results)) {
                return(FALSE)
            }

            state_results <- private$.state_partition_results
            treatment_names <- names(state_results)

            # Get color scheme
            color_scheme <- self$options$plot_color_scheme
            if (color_scheme == "clinical") {
                colors <- c("TOX" = "#e74c3c", "TWiST" = "#27ae60", "REL" = "#f39c12")
            } else if (color_scheme == "colorblind") {
                colors <- c("TOX" = "#0173b2", "TWiST" = "#029e73", "REL" = "#de8f05")
            } else if (color_scheme == "grayscale") {
                colors <- c("TOX" = "#404040", "TWiST" = "#bfbfbf", "REL" = "#7f7f7f")
            } else if (color_scheme == "viridis") {
                colors <- c("TOX" = "#440154", "TWiST" = "#21908c", "REL" = "#fde725")
            } else {
                colors <- c("TOX" = "#e74c3c", "TWiST" = "#27ae60", "REL" = "#f39c12")
            }

            # Create data for stacked area plot
            plot_data <- data.frame()

            for (trt in treatment_names) {
                state <- state_results[[trt]]

                # Create cumulative values for stacking
                df <- data.frame(
                    Treatment = trt,
                    Time = c(0, state$tau/2, state$tau),
                    TOX = c(state$e_tox, state$e_tox, state$e_tox),
                    TWIST = c(state$e_tox + state$e_twist,
                            state$e_tox + state$e_twist,
                            state$e_tox + state$e_twist),
                    Total = c(state$total_rmst, state$total_rmst, state$total_rmst)
                )

                plot_data <- rbind(plot_data, df)
            }

            # Reshape for ggplot
            plot_data_long <- tidyr::pivot_longer(
                plot_data,
                cols = c("TOX", "TWIST", "Total"),
                names_to = "State",
                values_to = "Cumulative"
            )

            # Create plot
            p <- ggplot2::ggplot() +
                # Add areas for each state
                ggplot2::geom_ribbon(
                    data = plot_data_long[plot_data_long$State == "TOX", ],
                    ggplot2::aes(x = Time, ymin = 0, ymax = Cumulative, fill = "TOX", group = Treatment),
                    alpha = 0.7
                ) +
                ggplot2::geom_ribbon(
                    data = plot_data_long[plot_data_long$State == "TWIST", ],
                    ggplot2::aes(x = Time, ymin = TOX, ymax = Cumulative, fill = "TWiST", group = Treatment),
                    alpha = 0.7
                ) +
                ggplot2::geom_ribbon(
                    data = plot_data_long[plot_data_long$State == "Total", ],
                    ggplot2::aes(x = Time, ymin = TWIST, ymax = Cumulative, fill = "REL", group = Treatment),
                    alpha = 0.7
                ) +
                ggplot2::facet_wrap(~ Treatment, ncol = 2) +
                ggplot2::scale_fill_manual(
                    name = "Health State",
                    values = colors,
                    labels = c("TOX" = "TOX (Toxicity)",
                              "TWiST" = "TWiST (Good Quality)",
                              "REL" = "REL (Relapse)")
                ) +
                ggplot2::labs(
                    title = "Partitioned Survival by Health State",
                    subtitle = paste("Time horizon (τ) =", round(state_results[[1]]$tau, 1), "months"),
                    x = "Time (months)",
                    y = "Cumulative Survival Time (months)"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 14),
                    legend.position = "bottom",
                    strip.background = ggplot2::element_rect(fill = "lightblue", color = "black"),
                    strip.text = ggplot2::element_text(face = "bold")
                )

            print(p)
            TRUE
        },

        .plotQTWISTComparison = function(image, ...) {

            if (is.null(private$.qtwist_results)) {
                return(FALSE)
            }

            qtwist_results <- private$.qtwist_results
            state_results <- private$.state_partition_results

            # Prepare data for stacked bar chart
            plot_data <- data.frame()

            for (trt in names(qtwist_results)) {
                qtwist <- qtwist_results[[trt]]
                state <- state_results[[trt]]

                df <- data.frame(
                    Treatment = trt,
                    Component = c("TOX", "TWiST", "REL"),
                    Duration = c(state$e_tox, state$e_twist, state$e_rel),
                    QualityAdjusted = c(qtwist$qtwist_tox, qtwist$qtwist_twist, qtwist$qtwist_rel),
                    Utility = c(qtwist$utility_tox, qtwist$utility_twist, qtwist$utility_rel)
                )

                plot_data <- rbind(plot_data, df)
            }

            plot_data$Component <- factor(plot_data$Component, levels = c("REL", "TWiST", "TOX"))

            # Get colors
            color_scheme <- self$options$plot_color_scheme
            if (color_scheme == "clinical") {
                colors <- c("TOX" = "#e74c3c", "TWiST" = "#27ae60", "REL" = "#f39c12")
            } else if (color_scheme == "colorblind") {
                colors <- c("TOX" = "#0173b2", "TWiST" = "#029e73", "REL" = "#de8f05")
            } else {
                colors <- c("TOX" = "#440154", "TWiST" = "#21908c", "REL" = "#fde725")
            }

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Treatment, y = QualityAdjusted, fill = Component)) +
                ggplot2::geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
                ggplot2::scale_fill_manual(
                    name = "Health State",
                    values = colors,
                    labels = c("TOX" = "TOX (Toxicity)",
                              "TWiST" = "TWiST (Good Quality)",
                              "REL" = "REL (Relapse)")
                ) +
                ggplot2::labs(
                    title = "Q-TWiST Component Comparison",
                    subtitle = sprintf("Utility weights: μ_TOX=%.2f, μ_TWiST=%.2f, μ_REL=%.2f",
                                     plot_data$Utility[plot_data$Component == "TOX"][1],
                                     plot_data$Utility[plot_data$Component == "TWiST"][1],
                                     plot_data$Utility[plot_data$Component == "REL"][1]),
                    x = "Treatment",
                    y = "Quality-Adjusted Survival Time (months)",
                    caption = "Higher bars indicate better quality-adjusted survival"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 14),
                    legend.position = "bottom"
                )

            print(p)
            TRUE
        },

        .plotSensitivity = function(image, ...) {

            if (!self$options$sensitivity_analysis || is.null(private$.sensitivity_results)) {
                return(FALSE)
            }

            sens_results <- private$.sensitivity_results

            # Prepare data for heatmap
            plot_data <- data.frame()

            for (i in seq_along(sens_results)) {
                sens <- sens_results[[i]]

                plot_data <- rbind(plot_data, data.frame(
                    Utility_TOX = sens$utility_tox,
                    Utility_REL = sens$utility_rel,
                    Delta_QTWIST = sens$delta_qtwist,
                    Favors = sens$favors,
                    Significant = sens$clinically_significant == "Yes"
                ))
            }

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Utility_TOX, y = Utility_REL, fill = Delta_QTWIST)) +
                ggplot2::geom_tile(color = "white", linewidth = 0.5) +
                ggplot2::geom_contour(ggplot2::aes(z = Delta_QTWIST), color = "black", linewidth = 0.3, alpha = 0.5) +
                ggplot2::geom_hline(yintercept = self$options$utility_rel, linetype = "dashed", color = "red") +
                ggplot2::geom_vline(xintercept = self$options$utility_tox, linetype = "dashed", color = "red") +
                ggplot2::scale_fill_gradient2(
                    name = "Δ Q-TWiST\n(months)",
                    low = "#d73027",
                    mid = "#ffffbf",
                    high = "#1a9850",
                    midpoint = 0
                ) +
                ggplot2::labs(
                    title = "Sensitivity Analysis: Q-TWiST Difference Across Utility Weights",
                    subtitle = "Red lines indicate primary analysis utilities",
                    x = "Toxicity Utility (μ_TOX)",
                    y = "Relapse Utility (μ_REL)",
                    caption = "Green = Treatment 2 better; Red = Treatment 1 better; Yellow = Similar"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 12),
                    legend.position = "right"
                )

            print(p)
            TRUE
        },

        .plotKMCurves = function(image, ...) {

            if (is.null(private$.state_partition_results)) {
                return(FALSE)
            }

            state_results <- private$.state_partition_results
            treatment_names <- names(state_results)

            # Extract KM fits
            km_data_os <- data.frame()
            km_data_pfs <- data.frame()

            for (trt in treatment_names) {
                state <- state_results[[trt]]

                # OS curve
                km_os <- state$km_os
                df_os <- data.frame(
                    Time = c(0, km_os$time),
                    Survival = c(1, km_os$surv),
                    Treatment = trt,
                    Type = "Overall Survival (OS)"
                )
                km_data_os <- rbind(km_data_os, df_os)

                # PFS curve
                km_pfs <- state$km_pfs
                df_pfs <- data.frame(
                    Time = c(0, km_pfs$time),
                    Survival = c(1, km_pfs$surv),
                    Treatment = trt,
                    Type = "Progression-Free Survival (PFS)"
                )
                km_data_pfs <- rbind(km_data_pfs, df_pfs)
            }

            km_data_combined <- rbind(km_data_os, km_data_pfs)

            p <- ggplot2::ggplot(km_data_combined, ggplot2::aes(x = Time, y = Survival, color = Treatment)) +
                ggplot2::geom_step(linewidth = 1) +
                ggplot2::facet_wrap(~ Type, ncol = 2) +
                ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::labs(
                    title = "Kaplan-Meier Survival Curves",
                    subtitle = "Reference curves for Q-TWiST calculation",
                    x = "Time (months)",
                    y = "Survival Probability"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 14),
                    legend.position = "bottom",
                    strip.background = ggplot2::element_rect(fill = "lightblue", color = "black"),
                    strip.text = ggplot2::element_text(face = "bold")
                )

            print(p)
            TRUE
        },

        .plotForest = function(image, ...) {

            # Simplified forest plot - would need stratified analysis implementation
            # This is a placeholder for future stratified analysis feature

            if (is.null(self$options$stratify_by) || length(self$options$stratify_by) == 0) {
                return(FALSE)
            }

            # For now, just return FALSE as stratified analysis not yet implemented
            return(FALSE)
        }

    )
)
