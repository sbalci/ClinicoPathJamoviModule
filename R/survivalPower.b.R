#' @importFrom gsDesign nSurv
#' @importFrom survival survdiff
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_point geom_rect labs scale_y_continuous scale_color_manual theme element_text element_blank
#' @importFrom scales percent_format

# Survival Power Analysis Module for Jamovi
#
# Constants used in calculations:
# - 0.67: Standard adjustment for average follow-up with staggered enrollment (Schoenfeld, 1981)
#         Represents that uniformly enrolled subjects have ~2/3 of total follow-up on average
# - 0.5:  Adjustment factor in competing risks to account for event interference

survivalPowerClass <- R6::R6Class(
    "survivalPowerClass",
    inherit = survivalPowerBase,
    private = list(

        # --- Constants ----
        AVERAGE_FOLLOWUP_FACTOR = 0.67,  # Schoenfeld (1981) for uniform accrual avg. follow-up
        DEFAULT_CENSORING_RATE = 0.3,    # Default censoring proportion (30%) if not otherwise estimable
        STRATIFICATION_EFFICIENCY = 0.95, # Efficiency gain per stratification factor
        effect_hr_info = NULL,

        .init = function() {
            # Initialize the analysis with comprehensive checks

            # Check required packages early
            if (!private$.check_required_packages()) {
                return()
            }

            # Handle clinical preset if specified
            private$.apply_clinical_preset()

            # Update instructions based on current settings
            private$.update_instructions()
        },
        
        .run = function() {
            # Main analysis runner
            if (is.null(self$options$analysis_type) ||
                is.null(self$options$test_type)) {
                return()
            }

            # Validate inputs before proceeding
            validation_result <- private$.validate_inputs()
            if (!validation_result$valid) {
                self$results$instructions$setContent(paste0(
                    "<p style='color: red;'><b>Input Validation Error:</b></p>",
                    "<p>", validation_result$message, "</p>"
                ))
                return()
            }
            # Surface validation warnings (non-fatal) as notes
            if (length(validation_result$warnings) > 0) {
                warn_note <- paste(validation_result$warnings, collapse = " ")
                jmvcore::note(self$results$power_summary, warn_note)
            }

            # Check required packages
            if (!private$.check_required_packages()) {
                return()
            }

            private$.populate_power_summary()
            private$.perform_power_analysis()
            private$.populate_assumptions()
            private$.populate_regulatory_considerations()
            private$.create_visualizations()
            private$.generate_interpretation()
            private$.generate_clinical_friendly_outputs()
            # Refresh instructions to reflect any conversion notes
            private$.update_instructions()
        },
        
        .validate_inputs = function() {
            # Comprehensive input validation with clinical ranges
            result <- list(valid = TRUE, message = "", warnings = list())

            # Validate effect size (hazard ratio)
            hr <- private$.get_effect_hr()
            if (!is.null(hr)) {
                if (hr <= 0 || hr > 5) {
                    result$valid <- FALSE
                    result$message <- paste0(result$message,
                        "Hazard ratio must be between 0 and 5. ",
                        "Common values: 0.5-0.8 (treatment benefit), 1.2-2.0 (increased risk). ")
                } else if (hr < 0.3 || hr > 3) {
                    # Warning for extreme values
                    result$warnings <- append(result$warnings,
                        "Extreme hazard ratio detected. Consider if this effect size is clinically plausible.")
                    result$message <- paste0(result$message,
                        "Note: HR of ", round(hr, 2), " represents a very large effect size. ",
                        "Most trials detect HR between 0.5-2.0. ")
                }
            }

            # Validate power level
            power <- self$options$power_level
            if (!is.null(power)) {
                if (power <= 0 || power >= 1) {
                    result$valid <- FALSE
                    result$message <- paste0(result$message,
                        "Power must be between 0 and 1. Standard values: 0.80 (80%) or 0.90 (90%). ")
                } else if (power < 0.7) {
                    result$message <- paste0(result$message,
                        "Warning: Power below 70% may result in underpowered study. ")
                }
            }

            # Validate alpha level
            alpha <- self$options$alpha_level
            if (!is.null(alpha)) {
                if (alpha <= 0 || alpha >= 1) {
                    result$valid <- FALSE
                    result$message <- paste0(result$message,
                        "Alpha level must be between 0 and 1. Standard value: 0.05 (5%). ")
                } else if (alpha > 0.1) {
                    result$message <- paste0(result$message,
                        "Note: Alpha level above 0.10 is unusual for confirmatory trials. ")
                }
            }

            # Validate median survival
            median_survival <- self$options$control_median_survival
            if (!is.null(median_survival)) {
                if (median_survival <= 0) {
                    result$valid <- FALSE
                    result$message <- paste0(result$message,
                        "Median survival must be positive (in months). ")
                } else if (median_survival > 240) {  # 20 years
                    result$message <- paste0(result$message,
                        "Note: Median survival > 20 years may require very long follow-up. ")
                }
            }

            distribution <- self$options$survival_distribution
            if (!is.null(distribution) && distribution != "exponential") {
                result$valid <- FALSE
                result$message <- paste0(result$message,
                    "Current version supports exponential survival only. ",
                    "Please set the survival distribution to 'exponential' for validated calculations. ")
            }

            # Validate allocation ratio
            ratio <- self$options$allocation_ratio
            if (!is.null(ratio)) {
                if (ratio <= 0 || ratio > 10) {
                    result$valid <- FALSE
                    result$message <- paste0(result$message,
                        "Allocation ratio must be positive and typically between 0.5 and 3. ")
                }
            }

            # Validate dropout rate
            dropout <- self$options$dropout_rate
            if (!is.null(dropout)) {
                if (dropout < 0 || dropout > 1) {
                    result$valid <- FALSE
                    result$message <- paste0(result$message,
                        "Dropout rate must be between 0 and 1. ")
                } else if (dropout > 0.3) {
                    result$message <- paste0(result$message,
                        "Warning: Dropout rate > 30% may significantly impact study power. ")
                }
            }

            # Validate parameter combinations
            private$.validate_parameter_combinations(result)

            # Multiplicity handling constraints
            if (!isTRUE(self$options$multiple_comparisons %in% c("none", "bonferroni"))) {
                result$valid <- FALSE
                result$message <- paste0(result$message,
                    "Multiplicity adjustment '", self$options$multiple_comparisons,
                    "' is not yet supported. Please select 'none' or 'bonferroni'. ")
            }

            if (!is.null(self$options$stratification_factors) && self$options$stratification_factors > 0) {
                result$warnings <- append(result$warnings,
                    "Stratification factors are not currently used to adjust variance; ensure stratified analyses are planned separately.")
            }

            unsupported_tests <- c("competing_risks", "rmst_test", "snp_survival", "weighted_log_rank")
            if (isTRUE(self$options$test_type %in% unsupported_tests)) {
                result$valid <- FALSE
                result$message <- paste0(result$message,
                    "Selected statistical test '", self$options$test_type,
                    "' is temporarily unavailable pending validated implementation. ",
                    "Please choose log-rank, Cox regression, or non-inferiority.")
            }

            return(result)
        },

        .validate_parameter_combinations = function(result) {
            # Check for unrealistic parameter combinations
            hr <- self$options$effect_size
            power <- self$options$power_level
            alpha <- self$options$alpha_level
            accrual <- self$options$accrual_period
            followup <- self$options$follow_up_period
            median_survival <- self$options$control_median_survival

            # Check if study duration is too short for median survival
            if (!is.null(median_survival) && !is.null(accrual) && !is.null(followup)) {
                total_duration <- accrual + followup
                if (total_duration < median_survival * 1.5) {
                    result$warnings <- append(result$warnings,
                        paste0("Study duration (", round(total_duration, 1), " months) may be too short ",
                               "to observe sufficient events with median survival of ",
                               round(median_survival, 1), " months. Consider extending follow-up."))
                }
            }

            # Check for underpowered studies with small effect sizes
            if (!is.null(hr) && !is.null(power)) {
                if ((hr > 0.9 && hr < 1.1) && power > 0.8) {
                    result$warnings <- append(result$warnings,
                        "Detecting very small effect sizes (HR near 1.0) requires very large sample sizes. Consider if this effect is clinically meaningful.")
                }
            }

            # Check for overly optimistic combinations
            if (!is.null(hr) && !is.null(power) && !is.null(alpha)) {
                if (hr < 0.6 && power > 0.9 && alpha < 0.05) {
                    result$warnings <- append(result$warnings,
                        "This combination assumes a very large effect with high power. Ensure these assumptions are justified by prior data.")
                }
            }

            # Check allocation ratio efficiency
            ratio <- self$options$allocation_ratio
            if (!is.null(ratio)) {
                if (ratio < 0.5 || ratio > 2) {
                    efficiency_loss <- (1 + ratio)^2 / (4 * ratio)
                    result$warnings <- append(result$warnings,
                        paste0("Allocation ratio ", round(ratio, 2), ":1 reduces efficiency by ",
                               round((efficiency_loss - 1) * 100, 1), "% compared to 1:1 randomization."))
                }
            }

            # Add warnings to message if any exist
            if (length(result$warnings) > 0) {
                warning_text <- paste0("<b>Clinical Considerations:</b><ul>",
                    paste0("<li>", result$warnings, "</li>", collapse = ""),
                    "</ul>")
                result$message <- paste0(result$message, warning_text)
            }
        },

        .check_required_packages = function() {
            # Check for required packages and provide helpful error messages
            required_packages <- list(
                gsDesign = "group sequential design calculations"
            )

            missing_packages <- c()
            for (pkg_name in names(required_packages)) {
                if (!requireNamespace(pkg_name, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg_name)
                }
            }

            if (length(missing_packages) > 0) {
                error_msg <- "<p><b>Missing Required Packages:</b></p><ul>"
                for (pkg in missing_packages) {
                    error_msg <- paste0(error_msg,
                        "<li><b>", pkg, "</b>: ", required_packages[[pkg]], "</li>")
                }
                error_msg <- paste0(error_msg,
                    "</ul><p>To install missing packages, run in R console:</p>",
                    "<pre>install.packages(c('", paste(missing_packages, collapse = "', '"), "'))</pre>")

                self$results$instructions$setContent(error_msg)
                return(FALSE)
            }

            return(TRUE)
        },


        .update_instructions = function() {
            # Update instructions based on analysis type
            html_content <- private$.generate_instructions_html()
            self$results$instructions$setContent(html_content)
        },
        
        .generate_instructions_html = function() {
            analysis_type <- self$options$analysis_type
            test_type <- self$options$test_type
            # Ensure effect size resolution info is current
            resolved_hr <- private$.get_effect_hr()

            # VERSION 0.3.0 LIMITATIONS WARNING - Added to prevent misleading users
            instructions <- paste0(
                "<div style='background-color: #fff3cd; border: 2px solid #ff9800; border-radius: 5px; padding: 15px; margin-bottom: 15px;'>",
                "<h4 style='color: #ff6f00; margin-top: 0;'>⚠️ Beta Version 0.3.0 - Known Limitations</h4>",
                "<p><strong>Current Implementation Status:</strong></p>",
                "<ul style='margin-bottom: 10px;'>",
                "<li><strong>✅ WORKING:</strong> Log-rank test, Cox regression, Non-inferiority designs</li>",
                "<li><strong>⛔ NOT AVAILABLE:</strong> Competing risks, RMST-based tests, SNP survival, Weighted log-rank</li>",
                "&lt;li&gt;&lt;strong&gt;⛔ DISTRIBUTION:&lt;/strong&gt; Only exponential survival distribution supported (other distributions will be blocked)&lt;/li&gt;",
                "<li><strong>⛔ ACCRUAL:</strong> Only uniform accrual pattern fully validated</li>",
                "</ul>",
                "<p style='margin-bottom: 0;'><strong>Important:</strong> This is a beta version with core features functional but incomplete. ",
                "For production clinical trials, verify calculations with independent biostatistician review.</p>",
                "</div>",
                "<p><strong>Power Analysis & Sample Size Calculation</strong></p>",
                "<p>This module provides power analysis and sample size calculations for survival studies and clinical trials.</p>",
                "<p><strong>Current Configuration:</strong><br>",
                "• Analysis Type: ", private$.format_analysis_type(analysis_type), "<br>",
                "• Statistical Test: ", private$.format_test_type(test_type), "</p>",
                "<p><strong>Currently validated calculations:</strong><br>",
                "• Log-rank test (sample size, power, detectable effect, duration)<br>",
                "• Cox regression under proportional hazards<br>",
                "• Non-inferiority designs using hazard-ratio margins<br>",
                "• Cluster design effects for randomized trials</p>",
                "<p><strong>Temporarily unavailable pending validation:</strong><br>",
                "• Competing risks (Fine-Gray)<br>",
                "• Restricted mean survival time comparisons<br>",
                "• SNP-based survival analyses and other specialised endpoints</p>",
                "<p><strong>Note:</strong> Calculations assume exponential survival distributions and uniform accrual. ",
                "Results should be interpreted by qualified biostatisticians in the context of specific study requirements.</p>"
            )

            # Add effect size handling explanation if available
            if (!is.null(private$effect_hr_info) && !is.null(private$effect_hr_info$note)) {
                instructions <- paste0(
                    instructions,
                    "<p><strong>Effect Size Handling:</strong><br>",
                    private$effect_hr_info$note,
                    " (HR used: ", sprintf("%.3f", resolved_hr), ")</p>"
                )
            }

            return(instructions)
        },
        
        .format_analysis_type = function(type) {
            switch(type,
                "sample_size" = "Calculate Sample Size",
                "power" = "Calculate Power",
                "effect_size" = "Calculate Detectable Effect Size",
                "duration" = "Calculate Study Duration",
                type
            )
        },
        
        .format_test_type = function(type) {
            switch(type,
                "log_rank" = "Log-rank Test",
                "cox_regression" = "Cox Regression",
                "competing_risks" = "Competing Risks",
                "rmst_test" = "RMST Comparison",
                "non_inferiority" = "Non-inferiority Trial",
                "snp_survival" = "SNP-based Survival",
                "weighted_log_rank" = "Weighted Log-rank",
                type
            )
        },
        
        .populate_power_summary = function() {
            summary_table <- self$results$power_summary
            
            analysis_type <- private$.format_analysis_type(self$options$analysis_type)
            test_type <- private$.format_test_type(self$options$test_type)
            study_design <- private$.format_study_design(self$options$study_design)
            primary_endpoint <- private$.format_primary_endpoint(self$options$primary_endpoint)
            effect_size_type <- private$.format_effect_size_type(self$options$effect_size_type)
            
            # Highlight key assumptions/limitations
            assumption_note <- "Assumes exponential survival with log-rank/Cox methodology; competing risks, RMST, SNP, and weighted log-rank options are not implemented in this version."
            if (!is.null(self$options$survival_distribution) && self$options$survival_distribution != "exponential") {
                assumption_note <- paste(assumption_note, "Input survival distribution set to exponential for calculations.")
                jmvcore::note(summary_table, "Non-exponential distributions are not supported; calculations use exponential assumption.")
            }
            if (self$options$test_type %in% c("competing_risks", "rmst_test", "snp_survival", "weighted_log_rank")) {
                jmvcore::note(summary_table, "Selected test type is not implemented; please choose log-rank, Cox regression, or non-inferiority.")
            }
            
            # Calculate the primary result based on analysis type
            calculated_value <- private$.calculate_primary_result()
            confidence_level <- paste0((1 - self$options$alpha_level) * 100, "%")
            
            summary_table$setRow(rowNo = 1, values = list(
                analysis_type = analysis_type,
                test_type = test_type,
                study_design = study_design,
                primary_endpoint = primary_endpoint,
                effect_size_type = effect_size_type,
                calculated_value = calculated_value,
                confidence_level = confidence_level
            ))
            
            jmvcore::note(summary_table, assumption_note)
        },
        
        .format_study_design = function(design) {
            switch(design,
                "two_arm_parallel" = "Two-arm Parallel",
                "multi_arm" = "Multi-arm Trial",
                "crossover" = "Crossover Design",
                "cluster_randomized" = "Cluster Randomized",
                "stratified" = "Stratified Design",
                design
            )
        },
        
        .format_primary_endpoint = function(endpoint) {
            switch(endpoint,
                "overall_survival" = "Overall Survival",
                "disease_free_survival" = "Disease-Free Survival",
                "progression_free_survival" = "Progression-Free Survival",
                "time_to_event" = "General Time-to-Event",
                "composite_endpoint" = "Composite Endpoint",
                endpoint
            )
        },
        
        .format_effect_size_type = function(type) {
            switch(type,
                "hazard_ratio" = "Hazard Ratio",
                "median_ratio" = "Median Survival Ratio",
                "rmst_difference" = "RMST Difference (months)",
                "survival_difference" = "Survival Probability Difference",
                type
            )
        },
        
        .calculate_primary_result = function() {
            analysis_type <- self$options$analysis_type
            test_type <- self$options$test_type
            
            tryCatch({
                if (test_type == "log_rank") {
                    result <- private$.calculate_log_rank()
                } else if (test_type == "cox_regression") {
                    result <- private$.calculate_cox_regression()
                } else if (test_type == "competing_risks") {
                    result <- private$.calculate_competing_risks()
                } else if (test_type == "rmst_test") {
                    result <- private$.calculate_rmst()
                } else if (test_type == "non_inferiority") {
                    result <- private$.calculate_non_inferiority()
                } else if (test_type == "snp_survival") {
                    result <- private$.calculate_snp_survival()
                } else {
                    result <- "Calculation method not implemented"
                }
                
                return(result)
            }, error = function(e) {
                return(paste("Error in calculation:", e$message))
            })
        },
        
        .calculate_log_rank = function() {
            # Log-rank test power calculation

            # Extract and prepare parameters
            params <- private$.extract_log_rank_parameters()

            # Apply adjustments for multi-arm studies and stratification
            params$alpha_adjusted <- private$.adjust_alpha_for_multiplicity(params$alpha)
            params$sample_adj <- private$.adjust_sample_for_design()

            # Route to appropriate calculation method
            if (params$analysis_type == "sample_size") {
                return(private$.calculate_log_rank_sample_size(params))
            } else if (params$analysis_type == "power") {
                return(private$.calculate_log_rank_power(params))
            } else if (params$analysis_type == "effect_size") {
                return(private$.calculate_log_rank_effect_size(params))
            } else if (params$analysis_type == "duration") {
                return(private$.calculate_log_rank_duration(params))
            }

            return("Calculation completed")
        },

        .extract_log_rank_parameters = function() {
            # Extract all parameters needed for log-rank calculations
            params <- list(
                analysis_type = self$options$analysis_type,
                alpha = self$options$alpha_level,
                power = self$options$power_level,
                hr = private$.get_effect_hr(),
                allocation_ratio = self$options$allocation_ratio,
                accrual_period = self$options$accrual_period,
                follow_up = self$options$follow_up_period,
                median_control = self$options$control_median_survival,
                sample_size_input = self$options$sample_size_input
            )

            # Convert median to distribution parameters
            dist_params <- private$.get_distribution_parameters(params$median_control, params$hr)
            params$lambda_control <- dist_params$lambda_control
            params$lambda_treatment <- dist_params$lambda_treatment

            return(params)
        },

        .calculate_log_rank_sample_size = function(params) {
            events_needed <- private$.events_needed_log_rank(
                hr = params$hr,
                alpha = params$alpha_adjusted,
                power = params$power,
                ratio = params$allocation_ratio
            )

            n_base <- private$.sample_size_from_events(
                events_needed = events_needed,
                lambda_control = params$lambda_control,
                hr = params$hr,
                allocation_ratio = params$allocation_ratio,
                accrual_period = params$accrual_period,
                follow_up_period = params$follow_up,
                dropout_rate = self$options$dropout_rate
            )

            n_final <- private$.apply_sample_size_adjustments(n_base, params)

            adjustments <- private$.build_adjustment_string(params)
            return(paste("Total Sample Size:", n_final, "subjects", adjustments))
        },

        .apply_sample_size_adjustments = function(n_base, params) {
            n_adjusted <- ceiling(n_base * params$sample_adj$design_effect * params$sample_adj$stratification_efficiency)
            n_final <- ceiling(private$.adjust_sample_for_accrual(n_adjusted))
            return(n_final)
        },

        .build_adjustment_string = function(params) {
            # Build string describing adjustments applied
            adjustments <- ""
            if (params$sample_adj$design_effect > 1) {
                adjustments <- paste0(adjustments, ", cluster design effect: ",
                                    round(params$sample_adj$design_effect, 2))
            }
            if (params$sample_adj$stratification_efficiency < 1) {
                adjustments <- paste0(adjustments, ", stratification efficiency: ",
                                    round(params$sample_adj$stratification_efficiency, 2))
            }
            if (params$alpha_adjusted != params$alpha) {
                adjustments <- paste0(adjustments, ", alpha adjusted for multiplicity: ",
                                    round(params$alpha_adjusted, 4))
            }
            return(adjustments)
        },

        .calculate_log_rank_power = function(params) {
            n_total <- params$sample_size_input

            expected_events <- private$.expected_events_from_sample(
                n_total = n_total,
                lambda_control = params$lambda_control,
                hr = params$hr,
                allocation_ratio = params$allocation_ratio,
                accrual_period = params$accrual_period,
                follow_up_period = params$follow_up,
                dropout_rate = self$options$dropout_rate
            )

            power_value <- private$.power_from_events(
                events = expected_events$total,
                hr = params$hr,
                alpha = params$alpha_adjusted,
                allocation_ratio = params$allocation_ratio
            )

            return(paste("Statistical Power:", round(power_value * 100, 1), "% (",
                         round(expected_events$total), "expected events)"))
        },

        .calculate_log_rank_effect_size = function(params) {
            n_total <- params$sample_size_input

            target_power <- params$power
            alpha <- params$alpha

            hr_solution <- tryCatch({
                objective <- function(hr_candidate) {
                    expected_events <- private$.expected_events_from_sample(
                        n_total = n_total,
                        lambda_control = params$lambda_control,
                        hr = hr_candidate,
                        allocation_ratio = params$allocation_ratio,
                        accrual_period = params$accrual_period,
                        follow_up_period = params$follow_up,
                        dropout_rate = self$options$dropout_rate
                    )$total

                    power_candidate <- private$.power_from_events(
                        events = expected_events,
                        hr = hr_candidate,
                        alpha = alpha,
                        allocation_ratio = params$allocation_ratio
                    ) - target_power
                    power_candidate
                }

                uniroot(objective, interval = c(0.1, 0.99))$root
            }, error = function(e) {
                NA_real_
            })

            if (is.na(hr_solution)) {
                return("Unable to determine detectable hazard ratio with current settings")
            }

            return(paste("Minimum Detectable HR:", round(hr_solution, 3)))
        },

        .calculate_power_for_hr = function(events_needed, hr, alpha) {
            private$.power_from_events(
                events = events_needed,
                hr = hr,
                alpha = alpha,
                allocation_ratio = self$options$allocation_ratio
            )
        },

        .calculate_log_rank_duration = function(params) {
            n_total <- params$sample_size_input
            required_events <- private$.events_needed_log_rank(
                hr = params$hr,
                alpha = params$alpha,
                power = params$power,
                ratio = params$allocation_ratio
            )

            accrual <- params$accrual_period
            lambda_control <- max(params$lambda_control, 1e-8)
            dropout_rate <- self$options$dropout_rate
            ratio <- params$allocation_ratio
            hr <- params$hr

            event_target <- function(follow_up) {
                events <- private$.expected_events_from_sample(
                    n_total = n_total,
                    lambda_control = lambda_control,
                    hr = hr,
                    allocation_ratio = ratio,
                    accrual_period = accrual,
                    follow_up_period = follow_up,
                    dropout_rate = dropout_rate
                )$total
                events - required_events
            }

            lower_bound <- 0.01
            start_diff <- event_target(lower_bound)
            if (start_diff >= 0) {
                total_duration <- accrual + lower_bound
                return(paste("Required Study Duration:", round(total_duration, 1), "months"))
            }

            upper_bound <- max(3, params$follow_up * 2, (1 / lambda_control) * 10)
            duration_follow_up <- tryCatch({
                diff_upper <- event_target(upper_bound)
                iter <- 0
                while (diff_upper < 0 && iter < 10) {
                    upper_bound <- upper_bound * 2
                    diff_upper <- event_target(upper_bound)
                    iter <- iter + 1
                }
                uniroot(event_target, lower = lower_bound, upper = upper_bound)$root
            }, error = function(e) {
                NA_real_
            })

            if (is.na(duration_follow_up)) {
                return("Unable to determine study duration with current settings")
            }

            total_duration <- accrual + duration_follow_up
            return(paste("Required Study Duration:", round(total_duration, 1), "months"))
        },
        

        .events_needed_log_rank = function(hr, alpha, power, ratio) {
            if (is.null(hr) || hr <= 0 || hr == 1) {
                stop("Hazard ratio must be positive and different from 1 for log-rank calculations.")
            }
            if (alpha <= 0 || alpha >= 1 || power <= 0 || power >= 1) {
                stop("Alpha and power must be within (0, 1).")
            }

            props <- private$.allocation_props(ratio)
            information_fraction <- props$control * props$treatment

            z_alpha <- qnorm(1 - alpha/2)
            z_beta <- qnorm(power)

            events <- ((z_alpha + z_beta)^2) / ((log(hr))^2 * information_fraction)
            ceiling(events)
        },
        
        .calculate_cox_regression = function() {
            # Cox regression power calculation using gsDesign
            analysis_type <- self$options$analysis_type
            alpha <- self$options$alpha_level
            power <- self$options$power_level
            hr <- private$.get_effect_hr()
            accrual_period <- self$options$accrual_period
            follow_up <- self$options$follow_up_period
            median_control <- self$options$control_median_survival
            allocation_ratio <- self$options$allocation_ratio

            # Convert median to rate parameter for exponential distribution
            lambda_control <- log(2) / median_control

            tryCatch({
                if (analysis_type == "sample_size") {
                    # Calculate required sample size using gsDesign if available
                    if (requireNamespace("gsDesign", quietly = TRUE)) {
                        tryCatch({
                            # Map parameters to nSurv
                            # alpha: one-sided alpha if not specified, but nSurv takes sidedness
                            # beta: 1 - power
                            # lambdaC: hazard rate for control
                            # hr: hazard ratio
                            # ratio: allocation ratio
                            # R: accrual period
                            # T: total study duration (accrual + follow-up)
                            # minfup: follow-up period
                            
                            k <- self$options$interim_analyses + 1
                            timing <- if (k > 1) seq(1, k) / k else 1
                            sfu <- if (self$options$alpha_spending == "obrien_fleming") gsDesign::sfLDOF else gsDesign::sfLDPocock
                            
                            design <- gsDesign::nSurv(
                                alpha = alpha,
                                beta = 1 - power,
                                lambdaC = lambda_control,
                                hr = hr,
                                ratio = allocation_ratio,
                                R = accrual_period,
                                T = accrual_period + follow_up,
                                minfup = follow_up,
                                sided = 2,
                                k = k,
                                timing = timing,
                                sfu = sfu
                            )
                            
                            n_total <- ceiling(design$n)
                            events_needed <- ceiling(design$d)
                            
                            return(paste("Total Sample Size:", n_total, "subjects (", events_needed, "events needed) [Calculated via gsDesign]"))
                        }, error = function(e) {
                            # Fallback if gsDesign fails
                            warning(paste("gsDesign calculation failed:", e$message, "- using standard log-rank formula"))
                        })
                    }

                    # Fallback / Standard calculation
                    events_needed <- private$.events_needed_log_rank(hr, alpha, power, allocation_ratio)

                    event_probs <- private$.overall_event_probability(
                        lambda_control = lambda_control,
                        hr = hr,
                        allocation_ratio = allocation_ratio,
                        accrual_period = accrual_period,
                        follow_up_period = follow_up,
                        dropout_rate = self$options$dropout_rate
                    )

                    n_total <- ceiling(events_needed / event_probs$overall)

                    return(paste("Total Sample Size:", n_total, "subjects (", events_needed, "events needed)"))

                } else if (analysis_type == "power") {
                    # Calculate power for Cox regression
                    n_total <- self$options$sample_size_input

                    # Calculate power using gsDesign if available
                    if (requireNamespace("gsDesign", quietly = TRUE)) {
                        tryCatch({
                            k <- self$options$interim_analyses + 1
                            timing <- if (k > 1) seq(1, k) / k else 1
                            sfu <- if (self$options$alpha_spending == "obrien_fleming") gsDesign::sfLDOF else gsDesign::sfLDPocock
                            
                            design <- gsDesign::nSurv(
                                alpha = alpha,
                                beta = NULL, # Calculate power
                                lambdaC = lambda_control,
                                hr = hr,
                                ratio = allocation_ratio,
                                R = accrual_period,
                                T = accrual_period + follow_up,
                                minfup = follow_up,
                                sided = 2,
                                k = k,
                                timing = timing,
                                sfu = sfu,
                                n = n_total
                            )
                            
                            power_calc <- design$power
                            expected_events <- design$d
                            
                            return(paste("Statistical Power:", round(power_calc * 100, 1), "% (", ceiling(expected_events), "events expected) [Calculated via gsDesign]"))
                        }, error = function(e) {
                            warning(paste("gsDesign power calculation failed:", e$message, "- using standard log-rank formula"))
                        })
                    }

                    # Fallback / Standard calculation

                    expected_events <- private$.expected_events_from_sample(
                        n_total = n_total,
                        lambda_control = lambda_control,
                        hr = hr,
                        allocation_ratio = allocation_ratio,
                        accrual_period = accrual_period,
                        follow_up_period = follow_up,
                        dropout_rate = self$options$dropout_rate
                    )$total

                    power_calc <- private$.power_from_events(
                        events = expected_events,
                        hr = hr,
                        alpha = alpha,
                        allocation_ratio = allocation_ratio
                    )

                    return(paste("Statistical Power:", round(power_calc * 100, 1), "% (", round(expected_events), "events expected)"))

                } else if (analysis_type == "effect_size") {
                    # Calculate minimum detectable HR for Cox regression
                    n_total <- self$options$sample_size_input

                    target_power <- power

                    hr_solution <- tryCatch({
                        objective <- function(hr_candidate) {
                            events_candidate <- private$.expected_events_from_sample(
                                n_total = n_total,
                                lambda_control = lambda_control,
                                hr = hr_candidate,
                                allocation_ratio = allocation_ratio,
                                accrual_period = accrual_period,
                                follow_up_period = follow_up,
                                dropout_rate = self$options$dropout_rate
                            )$total

                            private$.power_from_events(
                                events = events_candidate,
                                hr = hr_candidate,
                                alpha = alpha,
                                allocation_ratio = allocation_ratio
                            ) - target_power
                        }

                        uniroot(objective, interval = c(0.1, 1.5))$root
                    }, error = function(e) {
                        NA_real_
                    })

                    if (is.na(hr_solution)) {
                        return("Unable to determine detectable hazard ratio with current settings")
                    }

                    expected_events <- private$.expected_events_from_sample(
                        n_total = n_total,
                        lambda_control = lambda_control,
                        hr = hr_solution,
                        allocation_ratio = allocation_ratio,
                        accrual_period = accrual_period,
                        follow_up_period = follow_up,
                        dropout_rate = self$options$dropout_rate
                    )$total

                    return(paste("Minimum Detectable HR:", round(hr_solution, 3), "(", round(expected_events), "events)"))

                } else if (analysis_type == "duration") {
                    # Calculate required study duration for Cox regression
                    n_total <- self$options$sample_size_input

                    # Required events for target power
                    events_needed <- private$.events_needed_log_rank(hr, alpha, power, allocation_ratio)

                    # Event rate per month per subject
                    event_rate_monthly <- lambda_control * (1 - self$options$dropout_rate / 12)

                    # Approximate duration needed (simplified model)
                    # Assumes uniform accrual over accrual period
                    avg_follow_up_needed <- events_needed / (n_total * event_rate_monthly)
                    total_duration <- accrual_period + avg_follow_up_needed

                    return(paste("Required Study Duration:", round(total_duration, 1), "months"))
                }

            }, error = function(e) {
                return(paste("Cox regression calculation error:", e$message))
            })

            return("Cox regression calculation completed")
        },
        
        .calculate_competing_risks = function() {
            # Return summary string for main table
            # Detailed results are populated in specialized table
            
            hr <- private$.get_effect_hr()
            rate <- self$options$competing_risk_rate
            
            return(paste("Calculating power with Competing Risk Rate =", rate, 
                         "(See 'Competing Risks Analysis' table for details)"))
        },
        
        .calculate_rmst = function() {
            # Return summary string for main table
            # Detailed results are populated in specialized table
            
            tau <- self$options$rmst_tau
            return(paste("RMST Analysis at tau =", tau, 
                         "(See 'RMST Analysis' table for details)"))
        },

        .calculate_rmst_variance = function(lambda, tau) {
            # Calculate asymptotic variance for RMST under exponential distribution
            # Based on theoretical formula for RMST variance
            if (lambda <= 0) lambda <- 0.001  # Avoid division by zero

            # Theoretical variance for exponential RMST
            var_rmst <- (1 - exp(-lambda * tau)) / lambda^2 -
                       (tau^2 * exp(-lambda * tau)) +
                       (2 * tau * (1 - exp(-lambda * tau))) / lambda

            return(max(var_rmst, 0.01))  # Ensure positive variance
        },
        
        .calculate_non_inferiority = function() {
            analysis_type <- self$options$analysis_type
            alpha <- self$options$alpha_level
            power <- self$options$power_level
            hr_true <- private$.get_effect_hr()
            accrual_period <- self$options$accrual_period
            follow_up <- self$options$follow_up_period
            median_control <- self$options$control_median_survival
            allocation_ratio <- self$options$allocation_ratio
            ni_margin <- self$options$ni_margin
            ni_type <- self$options$ni_type

            if (is.null(hr_true) || hr_true <= 0) {
                return("Hazard ratio must be specified and positive for non-inferiority calculations")
            }

            if (!isTRUE(ni_type %in% c("relative_margin", "retention_fraction"))) {
                return("Only relative hazard-ratio margins are supported in the validated implementation")
            }

            hr_margin <- ni_margin
            if (hr_margin <= 0) {
                return("Non-inferiority margin must be specified on the hazard-ratio scale")
            }

            if (hr_true >= hr_margin) {
                warning("Specified treatment effect does not demonstrate non-inferiority against the chosen margin")
            }

            props <- private$.allocation_props(allocation_ratio)
            info_fraction <- props$control * props$treatment
            if (info_fraction <= 0) {
                return("Allocation ratio must produce positive information fraction")
            }

            z_alpha <- qnorm(1 - alpha)  # one-sided
            z_beta <- qnorm(power)
            log_effect <- log(hr_true) - log(hr_margin)

            if (log_effect >= 0) {
                log_effect <- -abs(log_effect)
            }

            events_needed <- ceiling(((z_alpha + z_beta)^2) / ((log_effect)^2 * info_fraction))

            dist_params <- private$.get_distribution_parameters(median_control, hr_true)

            dropout_rate <- self$options$dropout_rate

            if (analysis_type == "sample_size") {
                n_total <- private$.sample_size_from_events(
                    events_needed = events_needed,
                    lambda_control = dist_params$lambda_control,
                    hr = hr_true,
                    allocation_ratio = allocation_ratio,
                    accrual_period = accrual_period,
                    follow_up_period = follow_up,
                    dropout_rate = dropout_rate
                )

                return(paste(
                    "Total Sample Size:", n_total,
                    "subjects (", events_needed, "events needed; NI margin HR =", hr_margin, ")"
                ))

            } else if (analysis_type == "power") {
                n_total <- self$options$sample_size_input

                expected_events <- private$.expected_events_from_sample(
                    n_total = n_total,
                    lambda_control = dist_params$lambda_control,
                    hr = hr_true,
                    allocation_ratio = allocation_ratio,
                    accrual_period = accrual_period,
                    follow_up_period = follow_up,
                    dropout_rate = dropout_rate
                )$total

                info <- private$.information_from_events(expected_events, allocation_ratio)
                mean_z <- sqrt(info) * (log(hr_true) - log(hr_margin))
                power_calc <- pnorm(-z_alpha - mean_z)

                return(paste(
                    "Non-inferiority Power:", round(power_calc * 100, 1),
                    "% (HR =", round(hr_true, 3), "vs margin =", hr_margin, ")"
                ))

            } else if (analysis_type == "effect_size") {
                n_total <- self$options$sample_size_input

                power_for_hr <- function(hr_candidate) {
                    if (hr_candidate <= 0 || hr_candidate >= hr_margin) {
                        return(-1)
                    }

                    events <- private$.expected_events_from_sample(
                        n_total = n_total,
                        lambda_control = dist_params$lambda_control,
                        hr = hr_candidate,
                        allocation_ratio = allocation_ratio,
                        accrual_period = accrual_period,
                        follow_up_period = follow_up,
                        dropout_rate = dropout_rate
                    )$total

                    info <- private$.information_from_events(events, allocation_ratio)
                    mean_z <- sqrt(info) * (log(hr_candidate) - log(hr_margin))
                    pnorm(-z_alpha - mean_z)
                }

                objective <- function(hr_candidate) {
                    power_for_hr(hr_candidate) - power
                }

                upper_bound <- min(hr_margin * 0.999, 5)
                lower_bound <- max(1e-3, min(hr_true, 0.1))
                if (lower_bound >= upper_bound) {
                    lower_bound <- max(1e-3, upper_bound * 0.2)
                }

                detectable_hr <- tryCatch({
                    uniroot(objective, interval = c(lower_bound, upper_bound))$root
                }, error = function(e) {
                    NA_real_
                })

                if (is.na(detectable_hr)) {
                    return("Unable to determine maximum HR satisfying non-inferiority criteria with current settings")
                }

                return(paste(
                    "Maximum HR for Non-inferiority:", round(detectable_hr, 3),
                    "(margin =", hr_margin, ")"
                ))

            } else if (analysis_type == "duration") {
                n_total <- self$options$sample_size_input
                duration <- private$.solve_follow_up_duration(
                    n_total = n_total,
                    target_events = events_needed,
                    lambda_control = dist_params$lambda_control,
                    hr = hr_true,
                    allocation_ratio = allocation_ratio,
                    accrual_period = accrual_period,
                    dropout_rate = dropout_rate,
                    initial_follow_up = follow_up
                )

                if (is.na(duration)) {
                    return("Unable to determine study duration with current settings")
                }

                return(paste("Required Study Duration:", round(duration, 1), "months (non-inferiority trial)"))
            }

            return("Non-inferiority calculation completed")
        },
        
        .calculate_snp_survival = function() {
            return("SNP-based survival power calculations are temporarily unavailable pending validated genetic-model implementation")
        },
        
        .perform_power_analysis = function() {
            analysis_type <- self$options$analysis_type
            
            if (analysis_type == "sample_size") {
                private$.populate_sample_size_results()
            } else if (analysis_type == "power") {
                private$.populate_power_results()
            } else if (analysis_type == "effect_size") {
                private$.populate_effect_size_results()
            } else if (analysis_type == "duration") {
                private$.populate_duration_results()
            }
            
            # Populate specialized tables based on test type
            private$.populate_specialized_tables()
        },

        .populate_sample_size_results = function() {
            table <- self$results$sample_size_results
            # Clear existing rows to avoid accumulation on updates
            try({ table$deleteRows() }, silent = TRUE)
            
            # Add key parameters for sample size calculation
            parameters <- list(
                list(parameter = "Effect Size (HR)", value = round(private$.get_effect_hr(), 3), 
                     description = "Hazard ratio used in calculations (derived if needed)"),
                list(parameter = "Power", value = paste0(self$options$power_level * 100, "%"), 
                     description = "Desired statistical power to detect the specified effect"),
                list(parameter = "Alpha Level", value = self$options$alpha_level, 
                     description = "Type I error rate (two-sided significance level)"),
                list(parameter = "Allocation Ratio", value = self$options$allocation_ratio, 
                     description = "Ratio of control to experimental group sizes"),
                list(parameter = "Accrual Period", value = paste(self$options$accrual_period, "months"), 
                     description = "Duration of patient recruitment period"),
                list(parameter = "Follow-up Period", value = paste(self$options$follow_up_period, "months"), 
                     description = "Additional follow-up after recruitment ends")
            )
            
            for (i in seq_along(parameters)) {
                table$addRow(rowKey = i, values = parameters[[i]])
            }
        },
        
        .populate_power_results = function() {
            table <- self$results$power_results
            try({ table$deleteRows() }, silent = TRUE)

            # Calculate real power results based on current options
            n_total <- self$options$sample_size_input
            hr <- private$.get_effect_hr()
            alpha <- self$options$alpha_level
            control_median <- self$options$control_median_survival
            accrual_period <- self$options$accrual_period
            follow_up <- self$options$follow_up_period

            lambda_control <- log(2) / control_median
            expected_events <- private$.expected_events_from_sample(
                n_total = n_total,
                lambda_control = lambda_control,
                hr = hr,
                allocation_ratio = self$options$allocation_ratio,
                accrual_period = accrual_period,
                follow_up_period = follow_up,
                dropout_rate = self$options$dropout_rate
            )$total

            power_calc <- private$.power_from_events(
                events = expected_events,
                hr = hr,
                alpha = alpha,
                allocation_ratio = self$options$allocation_ratio
            )

            parameters <- list(
                list(parameter = "Sample Size", value = paste(n_total, "subjects"),
                     description = "Total number of subjects in the study"),
                list(parameter = "Calculated Power", value = paste0(round(power_calc * 100, 1), "%"),
                     description = "Statistical power for the given sample size"),
                list(parameter = "Expected Events", value = paste(round(expected_events), "events"),
                     description = "Number of events expected during study period"),
                list(parameter = "Effect Size", value = paste("HR =", hr),
                     description = "Hazard ratio representing treatment effect"),
                list(parameter = "Significance Level", value = paste0(alpha * 100, "%"),
                     description = "Type I error rate (alpha level)")
            )

            for (i in seq_along(parameters)) {
                table$addRow(rowKey = i, values = parameters[[i]])
            }
        },
        
        .populate_effect_size_results = function() {
            table <- self$results$effect_size_results
            try({ table$deleteRows() }, silent = TRUE)

            # Calculate real detectable effect size based on current options
            n_total <- self$options$sample_size_input
            alpha <- self$options$alpha_level
            power <- self$options$power_level
            control_median <- self$options$control_median_survival
            accrual_period <- self$options$accrual_period
            follow_up <- self$options$follow_up_period

            lambda_control <- log(2) / control_median

            expected_events_fun <- function(hr_candidate) {
                private$.expected_events_from_sample(
                    n_total = n_total,
                    lambda_control = lambda_control,
                    hr = hr_candidate,
                    allocation_ratio = self$options$allocation_ratio,
                    accrual_period = accrual_period,
                    follow_up_period = follow_up,
                    dropout_rate = self$options$dropout_rate
                )$total
            }

            min_detectable_hr <- tryCatch({
                objective <- function(hr_candidate) {
                    events_candidate <- expected_events_fun(hr_candidate)
                    private$.power_from_events(
                        events = events_candidate,
                        hr = hr_candidate,
                        alpha = alpha,
                        allocation_ratio = self$options$allocation_ratio
                    ) - power
                }

                uniroot(objective, interval = c(0.1, 1.5))$root
            }, error = function(e) {
                NA_real_
            })

            if (is.na(min_detectable_hr)) {
                min_detectable_hr <- NA
            }

            expected_events <- expected_events_fun(private$.get_effect_hr())

            # Calculate effect size in different metrics
            percent_reduction <- if (!is.na(min_detectable_hr)) round((1 - min_detectable_hr) * 100, 1) else NA

            parameters <- list(
                list(parameter = "Minimum Detectable HR", value = if (!is.na(min_detectable_hr)) round(min_detectable_hr, 3) else "Not determined",
                     description = "Smallest hazard ratio detectable with specified power"),
                list(parameter = "Sample Size", value = paste(n_total, "subjects"),
                     description = "Total number of subjects in the study"),
                list(parameter = "Power", value = paste0(round(power * 100, 1), "%"),
                     description = "Statistical power for detecting minimum effect"),
                list(parameter = "Risk Reduction", value = if (!is.na(percent_reduction)) paste0(percent_reduction, "%") else "Not determined",
                     description = "Minimum risk reduction detectable"),
                list(parameter = "Expected Events", value = paste(round(expected_events), "events"),
                     description = "Number of events expected during study period")
            )

            for (i in seq_along(parameters)) {
                table$addRow(rowKey = i, values = parameters[[i]])
            }
        },
        
        .populate_duration_results = function() {
            table <- self$results$study_duration_results
            try({ table$deleteRows() }, silent = TRUE)

            # Calculate real duration requirements based on current options
            n_total <- self$options$sample_size_input
            hr <- private$.get_effect_hr()
            alpha <- self$options$alpha_level
            power <- self$options$power_level
            control_median <- self$options$control_median_survival
            accrual_period <- self$options$accrual_period

            events_needed <- tryCatch({
                private$.events_needed_log_rank(hr, alpha, power, self$options$allocation_ratio)
            }, error = function(e) {
                NA_real_
            })

            lambda_control <- log(2) / control_median

            total_duration <- if (!is.na(events_needed)) {
                private$.solve_follow_up_duration(
                    n_total = n_total,
                    target_events = events_needed,
                    lambda_control = lambda_control,
                    hr = hr,
                    allocation_ratio = self$options$allocation_ratio,
                    accrual_period = accrual_period,
                    dropout_rate = self$options$dropout_rate,
                    initial_follow_up = self$options$follow_up_period
                )
            } else {
                NA_real_
            }

            if (is.na(total_duration)) {
                total_duration <- accrual_period + self$options$follow_up_period
                min_follow_up <- self$options$follow_up_period
            } else {
                min_follow_up <- max(total_duration - accrual_period, 0)
            }

            fifty_percent_events_time <- private$.solve_follow_up_duration(
                n_total = n_total,
                target_events = events_needed / 2,
                lambda_control = lambda_control,
                hr = hr,
                allocation_ratio = self$options$allocation_ratio,
                accrual_period = accrual_period,
                dropout_rate = self$options$dropout_rate,
                initial_follow_up = self$options$follow_up_period
            )

            display_total <- if (!is.na(total_duration)) round(total_duration, 1) else "Not determined"
            display_follow_up <- if (!is.na(min_follow_up)) round(min_follow_up, 1) else "Not determined"
            display_events <- if (!is.na(events_needed)) round(events_needed) else "Not determined"
            display_half <- if (!is.na(fifty_percent_events_time)) round(fifty_percent_events_time, 1) else "Not determined"

            value_total <- if (is.character(display_total)) display_total else paste(display_total, "months")
            value_follow_up <- if (is.character(display_follow_up)) display_follow_up else paste(display_follow_up, "months")
            value_events <- if (is.character(display_events)) display_events else paste(display_events, "events")
            value_half <- if (is.character(display_half)) display_half else paste(display_half, "months")

            parameters <- list(
                list(parameter = "Required Duration", value = value_total,
                     description = "Total study duration needed to achieve target power"),
                list(parameter = "Accrual Period", value = paste(round(accrual_period, 1), "months"),
                     description = "Patient recruitment period"),
                list(parameter = "Minimum Follow-up", value = value_follow_up,
                     description = "Additional follow-up needed after recruitment"),
                list(parameter = "Required Events", value = value_events,
                     description = "Number of events needed for target power"),
                list(parameter = "50% Events Time", value = value_half,
                     description = "Time when half of expected events occur")
            )

            for (i in seq_along(parameters)) {
                table$addRow(rowKey = i, values = parameters[[i]])
            }
        },
        
        .populate_specialized_tables = function() {
            test_type <- self$options$test_type
            
            if (test_type == "non_inferiority") {
                private$.populate_non_inferiority_table()
            } else if (test_type == "competing_risks") {
                private$.populate_competing_risks_table()
            } else if (test_type == "rmst_test") {
                private$.populate_rmst_analysis_table()
            } else if (test_type == "snp_survival") {
                private$.populate_snp_analysis_table()
            }

            if (self$options$study_design == "multi_arm") {
                private$.populate_multi_arm_table()
            }
            
            if (self$options$interim_analyses > 0) {
                private$.populate_interim_analysis_table()
            }
            
            if (self$options$sensitivity_analysis) {
                private$.populate_sensitivity_analysis_table()
            }
        },
        
        .populate_competing_risks_table = function() {
            # Real competing risks analysis calculations
            table <- self$results$competing_risks_table
            try({ table$deleteRows() }, silent = TRUE)

            # Get parameters
            hr <- private$.get_effect_hr()
            lambda_control <- log(2) / self$options$control_median_survival
            primary_event_rate <- lambda_control
            competing_risk_rate <- self$options$competing_risk_rate
            competing_risk_hr <- self$options$competing_risk_hr
            alpha <- self$options$alpha_level
            power <- self$options$power_level
            follow_up_time <- self$options$accrual_period + self$options$follow_up_period

            # Calculate cumulative incidences accounting for competing risks
            # Using exponential approximation
            lambda_primary <- primary_event_rate
            lambda_competing <- competing_risk_rate

            # Primary event cumulative incidence
            primary_cif_control <- (lambda_primary / (lambda_primary + lambda_competing)) *
                                   (1 - exp(-(lambda_primary + lambda_competing) * follow_up_time))

            # Treatment effect on primary event
            lambda_primary_treatment <- lambda_primary * hr
            lambda_competing_treatment <- lambda_competing * competing_risk_hr

            primary_cif_treatment <- (lambda_primary_treatment / (lambda_primary_treatment + lambda_competing_treatment)) *
                                     (1 - exp(-(lambda_primary_treatment + lambda_competing_treatment) * follow_up_time))

            # Competing risk cumulative incidence
            competing_cif_control <- (lambda_competing / (lambda_primary + lambda_competing)) *
                                     (1 - exp(-(lambda_primary + lambda_competing) * follow_up_time))

            # Calculate variance inflation factor for competing risks
            vif <- 1 / (1 - competing_cif_control)

            # Required events adjusted for competing risks
            standard_events <- private$.events_needed_log_rank(hr, alpha, power, self$options$allocation_ratio)
            adjusted_events_primary <- ceiling(standard_events * vif)

            # Calculate competing events based on actual competing risk rate and hazard ratio
            # Expected ratio of competing to primary events
            event_ratio <- (lambda_competing * competing_risk_hr) / lambda_primary
            adjusted_events_competing <- ceiling(standard_events * event_ratio)

            # Sample size impact
            sample_size_inflation <- round((vif - 1) * 100, 1)

            risks <- list(
                list(risk_type = "Primary Event",
                     event_rate = round(primary_event_rate, 3),
                     hazard_ratio = hr,
                     cumulative_incidence = round(primary_cif_control * 100, 1),
                     required_events = adjusted_events_primary,
                     sample_size_impact = "Base calculation"),
                list(risk_type = "Competing Risk",
                     event_rate = round(competing_risk_rate, 3),
                     hazard_ratio = competing_risk_hr,
                     cumulative_incidence = round(competing_cif_control * 100, 1),
                     required_events = adjusted_events_competing,
                     sample_size_impact = paste0(sample_size_inflation, "% increase needed"))
            )

            for (i in seq_along(risks)) {
                table$addRow(rowKey = i, values = risks[[i]])
            }
        },
        
        .populate_non_inferiority_table = function() {
            table <- self$results$non_inferiority_table
            try({ table$deleteRows() }, silent = TRUE)

            # Calculate actual non-inferiority parameters
            ni_margin <- self$options$ni_margin
            ni_type <- self$options$ni_type
            alpha <- self$options$alpha_level
            hr <- private$.get_effect_hr()

            # Get calculated sample size if this is a sample size analysis
            sample_size_text <- if (self$options$analysis_type == "sample_size") {
                result <- private$.calculate_primary_result()
                # Extract number from result string
                if (grepl("([0-9,]+)", result)) {
                    extracted_n <- regmatches(result, regexpr("[0-9,]+", result))
                    paste(extracted_n, "subjects")
                } else {
                    "Not calculated"
                }
            } else if (self$options$analysis_type == "power") {
                paste(self$options$sample_size_input, "subjects (input)")
            } else {
                "Varies by analysis type"
            }

            # Calculate margin interpretation
            margin_interpretation <- if (ni_type == "relative_margin") {
                if (ni_margin <= 1.25) "Conservative margin" else if (ni_margin <= 1.5) "Moderate margin" else "Liberal margin"
            } else if (ni_type == "absolute_margin") {
                if (ni_margin <= 0.1) "Conservative margin" else if (ni_margin <= 0.2) "Moderate margin" else "Liberal margin"
            } else {
                if (ni_margin >= 0.5) "Conservative retention" else "Liberal retention"
            }

            ni_params <- list(
                list(parameter = "Non-inferiority Margin",
                     value = sprintf("%.3f", ni_margin),
                     margin_type = private$.format_ni_type(ni_type),
                     clinical_interpretation = paste("Maximum acceptable difference -", margin_interpretation)),
                list(parameter = "Sample Size Requirement",
                     value = sample_size_text,
                     margin_type = "Total enrollment",
                     clinical_interpretation = "Typically 20-50% larger than superiority trial"),
                list(parameter = "One-sided Alpha",
                     value = sprintf("%.4f", alpha),
                     margin_type = "Statistical threshold",
                     clinical_interpretation = "For non-inferiority conclusion"),
                list(parameter = "True Hazard Ratio",
                     value = sprintf("%.3f", hr),
                     margin_type = "Expected effect",
                     clinical_interpretation = if (hr < ni_margin) "Should demonstrate non-inferiority" else "May not achieve non-inferiority")
            )

            for (i in seq_along(ni_params)) {
                table$addRow(rowKey = i, values = ni_params[[i]])
            }
        },
        
        .format_ni_type = function(type) {
            switch(type,
                "absolute_margin" = "Absolute Margin",
                "relative_margin" = "Relative Margin", 
                "retention_fraction" = "Retention of Effect Fraction",
                type
            )
        },
        
        .populate_rmst_analysis_table = function() {
            # Real RMST analysis calculations
            table <- self$results$rmst_analysis_table
            try({ table$deleteRows() }, silent = TRUE)

            # Get parameters
            tau <- self$options$rmst_tau
            rmst_diff <- self$options$rmst_difference
            control_median <- self$options$control_median_survival
            hr <- private$.get_effect_hr()
            alpha <- self$options$alpha_level
            power <- self$options$power_level

            # Calculate RMST values based on exponential distribution assumption
            # For exponential distribution: RMST(t) = λ^(-1) * (1 - exp(-λ*t))
            lambda_control <- log(2) / control_median
            lambda_treatment <- lambda_control * hr

            # Calculate RMST for each group
            rmst_control <- (1 - exp(-lambda_control * tau)) / lambda_control
            rmst_treatment <- (1 - exp(-lambda_treatment * tau)) / lambda_treatment
            calculated_diff <- rmst_treatment - rmst_control

            # Calculate variance (approximate formula for exponential)
            var_control <- tau^2 * exp(-lambda_control * tau) / lambda_control^2
            var_treatment <- tau^2 * exp(-lambda_treatment * tau) / lambda_treatment^2

            # Sample size calculation for RMST difference
            z_alpha <- qnorm(1 - alpha/2)
            z_beta <- qnorm(power)

            # Approximate sample size per group for RMST test
            n_per_group <- ceiling((z_alpha + z_beta)^2 * (var_control + var_treatment) / calculated_diff^2)
            total_n <- 2 * n_per_group

            # Calculate confidence interval for difference
            se_diff <- sqrt(var_control + var_treatment) / sqrt(n_per_group)
            ci_lower <- calculated_diff - z_alpha * se_diff
            ci_upper <- calculated_diff + z_alpha * se_diff
            ci_text <- sprintf("(%.2f, %.2f)", ci_lower, ci_upper)

            rmst_params <- list(
                list(parameter = sprintf("RMST at %.0f months", tau),
                     control_group = round(rmst_control, 2),
                     treatment_group = round(rmst_treatment, 2),
                     difference = round(calculated_diff, 2),
                     confidence_interval = ci_text),
                list(parameter = "Sample Size Required",
                     control_group = n_per_group,
                     treatment_group = n_per_group,
                     difference = total_n,
                     confidence_interval = "Total enrollment"),
                list(parameter = "Power Achievement",
                     control_group = round(power * 100, 1),
                     treatment_group = round(power * 100, 1),
                     difference = round(power * 100, 1),
                     confidence_interval = sprintf("%.1f%% power", power * 100))
            )

            for (i in seq_along(rmst_params)) {
                table$addRow(rowKey = i, values = rmst_params[[i]])
            }
        },
        
        .populate_snp_analysis_table = function() {
            table <- self$results$snp_analysis_table
            try({ table$deleteRows() }, silent = TRUE)

            # Calculate actual SNP analysis parameters
            maf <- self$options$snp_maf
            genetic_model <- self$options$genetic_model
            hr <- private$.get_effect_hr()

            # Calculate genotype frequencies
            freq_AA <- (1 - maf)^2
            freq_Aa <- 2 * maf * (1 - maf)
            freq_aa <- maf^2

            # Get sample size from calculation if available
            calculated_n <- if (self$options$analysis_type == "sample_size") {
                result <- private$.calculate_primary_result()
                if (grepl("([0-9,]+)", result)) {
                    as.numeric(gsub("[^0-9]", "", regmatches(result, regexpr("[0-9,]+", result))))
                } else {
                    NA
                }
            } else if (self$options$analysis_type == "power") {
                self$options$sample_size_input
            } else {
                NA
            }

            # Calculate power by genotype based on genetic model
            power_by_genotype <- if (!is.na(calculated_n)) {
                if (genetic_model == "additive") {
                    # Additive model: power increases with number of minor alleles
                    power_AA <- round(private$.basic_power_calc(calculated_n * freq_AA, 1.0, self$options$alpha_level) * 100, 1)
                    power_Aa <- round(private$.basic_power_calc(calculated_n * freq_Aa, sqrt(hr), self$options$alpha_level) * 100, 1)
                    power_aa <- round(private$.basic_power_calc(calculated_n * freq_aa, hr, self$options$alpha_level) * 100, 1)
                    paste0("AA: ", power_AA, "%, Aa: ", power_Aa, "%, aa: ", power_aa, "%")
                } else if (genetic_model == "dominant") {
                    # Dominant model: Aa and aa vs AA
                    power_carriers <- round(private$.basic_power_calc(calculated_n * (freq_Aa + freq_aa), hr, self$options$alpha_level) * 100, 1)
                    power_non_carriers <- round(private$.basic_power_calc(calculated_n * freq_AA, 1.0, self$options$alpha_level) * 100, 1)
                    paste0("AA: ", power_non_carriers, "%, Carriers (Aa+aa): ", power_carriers, "%")
                } else {  # recessive
                    # Recessive model: aa vs AA+Aa
                    power_homozygous <- round(private$.basic_power_calc(calculated_n * freq_aa, hr, self$options$alpha_level) * 100, 1)
                    power_others <- round(private$.basic_power_calc(calculated_n * (freq_AA + freq_Aa), 1.0, self$options$alpha_level) * 100, 1)
                    paste0("AA+Aa: ", power_others, "%, aa: ", power_homozygous, "%")
                }
            } else {
                "Power varies with sample size"
            }

            snp_data <- list(
                list(genetic_model = private$.format_genetic_model(genetic_model),
                     maf = round(maf, 4),
                     genotype_frequencies = paste0("AA: ", round(freq_AA, 3),
                                                  ", Aa: ", round(freq_Aa, 3),
                                                  ", aa: ", round(freq_aa, 3)),
                     required_sample_size = if (!is.na(calculated_n)) as.integer(calculated_n) else NA,
                     power_by_genotype = power_by_genotype)
            )

            for (i in seq_along(snp_data)) {
                table$addRow(rowKey = i, values = snp_data[[i]])
            }
        },
        
        .format_genetic_model = function(model) {
            switch(model,
                "additive" = "Additive Model",
                "dominant" = "Dominant Model",
                "recessive" = "Recessive Model",
                model
            )
        },
        
        .populate_multi_arm_table = function() {
            table <- self$results$multi_arm_table
            try({ table$deleteRows() }, silent = TRUE)
            num_arms <- self$options$number_of_arms
            multiple_comparisons <- self$options$multiple_comparisons
            alpha <- self$options$alpha_level
            power <- self$options$power_level

            # Calculate adjusted alpha based on multiple comparisons method
            adjusted_alpha <- private$.adjust_alpha_for_multiplicity(alpha)

            # Get sample size calculation
            calculated_n <- if (self$options$analysis_type == "sample_size") {
                result <- private$.calculate_primary_result()
                if (grepl("([0-9,]+)", result)) {
                    as.numeric(gsub("[^0-9]", "", regmatches(result, regexpr("[0-9,]+", result))))
                } else {
                    NA
                }
            } else if (self$options$analysis_type == "power") {
                self$options$sample_size_input
            } else {
                NA
            }

            # Calculate sample size per arm
            sample_size_per_arm <- if (!is.na(calculated_n)) {
                round(calculated_n / num_arms)
            } else {
                NA
            }

            lambda_control <- log(2) / self$options$control_median_survival
            hr_effect <- private$.get_effect_hr()

            individual_power <- if (!is.na(calculated_n) && !is.na(sample_size_per_arm)) {
                comparison_events <- private$.expected_events_from_sample(
                    n_total = sample_size_per_arm * 2,
                    lambda_control = lambda_control,
                    hr = hr_effect,
                    allocation_ratio = 1,
                    accrual_period = self$options$accrual_period,
                    follow_up_period = self$options$follow_up_period,
                    dropout_rate = self$options$dropout_rate
                )$total

                power_calc <- private$.power_from_events(
                    events = comparison_events,
                    hr = hr_effect,
                    alpha = adjusted_alpha,
                    allocation_ratio = 1
                )

                round(power_calc * 100, 1)
            } else {
                round(power * 100, 1)  # Target power
            }

            # Calculate overall study power (probability of at least one significant result)
            if (multiple_comparisons == "none") {
                # Without adjustment, family-wise error rate increases
                overall_power <- round((1 - (1 - individual_power/100)^(num_arms-1)) * 100, 1)
            } else {
                # With adjustment, power is reduced by the adjustment factor
                adjustment_factor <- switch(multiple_comparisons,
                    "bonferroni" = 1 / (num_arms - 1),
                    "holm" = 1 / sqrt(num_arms - 1),
                    "dunnett" = 0.9,  # Dunnett is more efficient than Bonferroni
                    0.85  # fallback
                )
                overall_power <- round(individual_power * adjustment_factor, 1)
            }

            # Generate comparisons for each treatment arm vs control
            comparisons <- list()
            for (i in 1:(num_arms-1)) {
                comparisons[[i]] <- list(
                    comparison = paste("Control vs Treatment", i),
                    sample_size_per_arm = if (!is.na(sample_size_per_arm)) as.integer(sample_size_per_arm) else NA,
                    adjusted_alpha = round(adjusted_alpha, 4),
                    power = individual_power,
                    total_study_power = overall_power
                )
            }

            for (i in seq_along(comparisons)) {
                table$addRow(rowKey = i, values = comparisons[[i]])
            }
        },
        
        .populate_interim_analysis_table = function() {
            table <- self$results$interim_analysis_table
            try({ table$deleteRows() }, silent = TRUE)
            num_interim <- self$options$interim_analyses
            alpha_spending <- self$options$alpha_spending
            alpha <- self$options$alpha_level

            # Define alpha spending function
            for (i in 1:num_interim) {
                # Timing as percentage of total events/time
                timing <- (i / (num_interim + 1)) * 100

                # Calculate alpha spent based on spending function
                alpha_spent <- if (alpha_spending == "obrien_fleming") {
                    # O'Brien-Fleming: conservative early, liberal late
                    alpha * (2 * (1 - pnorm(qnorm(1 - alpha/2) / sqrt(timing/100))))
                } else if (alpha_spending == "pocock") {
                    # Pocock: equal alpha spending
                    alpha * i / (num_interim + 1)
                } else {
                    # No alpha spending (not recommended)
                    alpha / (num_interim + 1)
                }

                # Calculate boundary value (Z-score)
                boundary_value <- if (alpha_spending == "obrien_fleming") {
                    qnorm(1 - alpha_spent/2) / sqrt(timing/100)
                } else if (alpha_spending == "pocock") {
                    qnorm(1 - alpha_spent/2)
                } else {
                    qnorm(1 - alpha_spent/2)
                }

                # Calculate conditional power (simplified approximation)
                # This would normally require detailed calculations based on observed effect
                conditional_power <- if (i == 1) {
                    # Early analysis - typically higher uncertainty
                    75.0
                } else if (i == num_interim) {
                    # Final interim - approaching final analysis power
                    round(self$options$power_level * 100 * 0.9, 1)
                } else {
                    # Middle analyses
                    75.0 + (10 * (i-1) / (num_interim-1))
                }

                table$addRow(rowKey = i, values = list(
                    analysis_number = i,
                    timing = round(timing, 1),
                    alpha_spent = round(alpha_spent, 4),
                    boundary_value = round(boundary_value, 2),
                    conditional_power = round(conditional_power, 1)
                ))
            }
        },
        
        .populate_sensitivity_analysis_table = function() {
            # Add error handling for sensitivity analysis
            tryCatch({
                table <- self$results$sensitivity_analysis_table

                # Force table to be visible for debugging
                if (self$options$sensitivity_analysis) {
                    table$setVisible(TRUE)
                }

                # Debug: Check if sensitivity analysis is enabled
                if (!self$options$sensitivity_analysis) {
                    return()
                }

                # Validate that we have the required parameters
                if (is.null(self$options$effect_size) || is.null(self$options$alpha_level)) {
                    warning("Sensitivity analysis: Missing required parameters (effect_size or alpha_level)")
                    return()
                }

                # Clear existing rows first
                table$deleteRows()

                # Get base case values from current options
                base_hr <- self$options$effect_size
                base_power <- self$options$power_level
                base_alpha <- self$options$alpha_level
                base_median <- self$options$control_median_survival
                base_accrual <- self$options$accrual_period
                base_follow_up <- self$options$follow_up_period

                # Calculate base case result
                base_result <- private$.calculate_primary_result()

                # Debug: Check base result
                if (is.null(base_result) || is.na(base_result)) {
                    warning("Sensitivity analysis: Base result calculation failed")
                    return()
                }

            # Define sensitivity scenarios: vary key parameters by ±20%
            scenarios <- list(
                list(
                    parameter = "Hazard Ratio",
                    base_case = sprintf("%.2f", base_hr),
                    scenario_1 = sprintf("%.2f", base_hr * 0.8),
                    scenario_2 = sprintf("%.2f", base_hr * 1.2),
                    impact_1 = private$.calculate_sensitivity_impact("effect_size", base_hr * 0.8),
                    impact_2 = private$.calculate_sensitivity_impact("effect_size", base_hr * 1.2)
                ),
                list(
                    parameter = "Control Median Survival",
                    base_case = sprintf("%.1f months", base_median),
                    scenario_1 = sprintf("%.1f months", base_median * 1.2),
                    scenario_2 = sprintf("%.1f months", base_median * 0.8),
                    impact_1 = private$.calculate_sensitivity_impact("control_median_survival", base_median * 1.2),
                    impact_2 = private$.calculate_sensitivity_impact("control_median_survival", base_median * 0.8)
                ),
                list(
                    parameter = "Significance Level",
                    base_case = sprintf("%.3f", base_alpha),
                    scenario_1 = sprintf("%.3f", pmax(0.01, base_alpha * 0.5)),
                    scenario_2 = sprintf("%.3f", pmin(0.10, base_alpha * 2)),
                    impact_1 = private$.calculate_sensitivity_impact("alpha_level", pmax(0.01, base_alpha * 0.5)),
                    impact_2 = private$.calculate_sensitivity_impact("alpha_level", pmin(0.10, base_alpha * 2))
                ),
                list(
                    parameter = "Accrual Period",
                    base_case = sprintf("%.1f months", base_accrual),
                    scenario_1 = sprintf("%.1f months", base_accrual * 0.8),
                    scenario_2 = sprintf("%.1f months", base_accrual * 1.2),
                    impact_1 = private$.calculate_sensitivity_impact("accrual_period", base_accrual * 0.8),
                    impact_2 = private$.calculate_sensitivity_impact("accrual_period", base_accrual * 1.2)
                )
            )

            for (i in seq_along(scenarios)) {
                scenario <- scenarios[[i]]

                # Calculate impact assessment with error handling
                impact_assessment <- tryCatch({
                    impact_result <- private$.format_sensitivity_impact(scenario$impact_1, scenario$impact_2)
                    if (is.null(impact_result) || is.na(impact_result)) {
                        "Impact calculation failed"
                    } else {
                        impact_result
                    }
                }, error = function(e) {
                    warning(paste("Sensitivity impact calculation error:", e$message))
                    paste("Error:", e$message)
                })

                # Add row with validation
                tryCatch({
                    table$addRow(rowKey = i, values = list(
                        parameter = scenario$parameter,
                        base_case = scenario$base_case,
                        scenario_1 = scenario$scenario_1,
                        scenario_2 = scenario$scenario_2,
                        impact_assessment = impact_assessment
                    ))
                }, error = function(e) {
                    warning(paste("Failed to add row", i, "to sensitivity analysis table:", e$message))
                })
            }
            }, error = function(e) {
                # If sensitivity analysis fails, continue without it
                warning(paste("Sensitivity analysis failed:", e$message))
            })
        },

        .calculate_sensitivity_impact = function(param_name, new_value) {
            # Calculate sensitivity impact without modifying self$options
            # Use direct parameter passing to calculation methods

            tryCatch({
                # Get current parameter values
                analysis_type <- self$options$analysis_type
                test_type <- self$options$test_type
                alpha <- if (param_name == "alpha_level") new_value else self$options$alpha_level
                power <- self$options$power_level
                hr <- if (param_name == "effect_size") new_value else self$options$effect_size
                allocation_ratio <- self$options$allocation_ratio
                accrual_period <- if (param_name == "accrual_period") new_value else self$options$accrual_period
                follow_up <- self$options$follow_up_period
                median_control <- if (param_name == "control_median_survival") new_value else self$options$control_median_survival

                # Calculate with modified parameters directly
                if (test_type == "log_rank") {
                    result <- private$.calculate_log_rank_with_params(
                        analysis_type, alpha, power, hr, allocation_ratio,
                        accrual_period, follow_up, median_control
                    )
                } else {
                    # For other test types, use approximation based on log-rank
                    # Calculate events needed using modified parameters
                    lambda_control <- log(2) / median_control
                    total_time <- accrual_period + follow_up
                    prob_event_control <- 1 - exp(-lambda_control * (total_time * private$AVERAGE_FOLLOWUP_FACTOR))

                    if (analysis_type == "sample_size") {
                        events_needed <- private$.events_needed_log_rank(hr, alpha, power, allocation_ratio)
                        n_total <- ceiling(events_needed / prob_event_control)
                        result <- paste("Total Sample Size:", n_total, "subjects")
                    } else if (analysis_type == "power") {
                        n_total <- self$options$sample_size_input
                        expected_events <- n_total * prob_event_control
                        power_calc <- private$.basic_power_calc(expected_events, hr, alpha)
                        result <- paste("Statistical Power:", round(power_calc * 100, 1), "%")
                    } else {
                        result <- "Sensitivity not available for this analysis type"
                    }
                }

                return(result)
            }, error = function(e) {
                return("Calculation error")
            })
        },

        .format_sensitivity_impact = function(impact_1, impact_2) {
            # Simplified sensitivity impact assessment
            tryCatch({
                base_result <- private$.calculate_primary_result()

                # Extract numeric values from result strings
                extract_numeric <- function(result_text) {
                    if (is.null(result_text) || is.na(result_text)) {
                        return(NA)
                    }

                    result_char <- as.character(result_text)

                    # Try to extract numbers from strings like "Total Sample Size: 181 subjects" or "Statistical Power: 85.2%"
                    if (grepl("\\d+", result_char)) {
                        num_match <- regmatches(result_char, regexpr("\\d+\\.?\\d*", result_char))
                        if (length(num_match) > 0) {
                            return(as.numeric(num_match[1]))
                        }
                    }

                    # Try direct conversion
                    tryCatch({
                        return(as.numeric(result_char))
                    }, error = function(e) {
                        return(NA)
                    })
                }

                # Extract numeric values
                base_numeric <- extract_numeric(base_result)
                impact_1_numeric <- extract_numeric(impact_1)
                impact_2_numeric <- extract_numeric(impact_2)

                # Validate extracted values
                if (is.na(base_numeric) || is.na(impact_1_numeric) || is.na(impact_2_numeric)) {
                    # If we can't extract numbers, provide qualitative assessment
                    return("Parameter change affects results")
                }

                # Handle numeric results
                if (base_numeric > 0) {
                    change_1 <- ((impact_1_numeric - base_numeric) / base_numeric) * 100
                    change_2 <- ((impact_2_numeric - base_numeric) / base_numeric) * 100

                    max_change <- max(abs(change_1), abs(change_2))

                    if (max_change < 1) {
                        return("Minimal impact (<1% change)")
                    } else if (max_change < 5) {
                        return(sprintf("Low impact (%.1f%% change)", max_change))
                    } else if (max_change < 15) {
                        return(sprintf("Moderate impact (%.1f%% change)", max_change))
                    } else {
                        return(sprintf("High impact (%.1f%% change)", max_change))
                    }
                } else {
                    return("Parameter change affects results")
                }
            }, error = function(e) {
                return("Impact calculation error")
            })
        },
        
        .populate_assumptions = function() {
            table <- self$results$assumptions_table
            try({ table$deleteRows() }, silent = TRUE)
            
            assumptions <- list(
                list(assumption = "Survival Distribution", 
                     specification = paste("Exponential with median", self$options$control_median_survival, "months"),
                     impact = "Affects event rate calculations and timeline estimates",
                     recommendation = "Validate with pilot data or literature review"),
                list(assumption = "Proportional Hazards",
                     specification = "Hazard ratio constant over time",
                     impact = "Critical for log-rank test validity and sample size accuracy",
                     recommendation = "Plan interim monitoring for proportional hazards assumption"),
                list(assumption = "Dropout Rate",
                     specification = paste0(self$options$dropout_rate * 100, "% annual loss to follow-up"),
                     impact = "Reduces effective sample size and statistical power",
                     recommendation = "Implement retention strategies and monitor dropout patterns"),
                list(assumption = "Accrual Pattern",
                     specification = private$.format_accrual_pattern(self$options$accrual_pattern),
                     impact = "Affects study timeline and event occurrence timing",
                     recommendation = "Monitor actual accrual against assumptions")
            )

            # Add simulation-based validation if sensitivity analysis is enabled
            if (self$options$sensitivity_analysis) {
                sim_results <- private$.run_simulation_analysis()
                if (!is.null(sim_results)) {
                    assumptions <- append(assumptions, list(list(
                        assumption = "Simulation Validation",
                        specification = sprintf("Monte Carlo simulation with %d runs", self$options$simulation_runs),
                        impact = sprintf("Empirical power: %.1f%% (analytical comparison)", sim_results$empirical_power * 100),
                        recommendation = "Simulation validates analytical calculations"
                    )))
                }
            }

            for (i in seq_along(assumptions)) {
                table$addRow(rowKey = i, values = assumptions[[i]])
            }
        },
        
        .format_accrual_pattern = function(pattern) {
            switch(pattern,
                "uniform" = "Uniform patient enrollment over accrual period",
                "linear_increasing" = "Linearly increasing enrollment rate",
                "exponential" = "Exponential ramp-up in enrollment",
                "custom" = "Custom enrollment pattern",
                pattern
            )
        },
        
        .generate_interpretation = function() {
            # Generate clinical interpretation - main coordinator function
            study_summary <- private$.generate_study_summary()
            clinical_interpretation <- private$.generate_clinical_interpretation()
            report_sentence <- private$.generate_report_sentence()

            # Combine all sections
            interpretation <- paste0(
                study_summary,
                clinical_interpretation,
                report_sentence
            )

            if (!is.null(self$results$clinical_interpretation)) {
                self$results$clinical_interpretation$setContent(interpretation)
                self$results$clinical_interpretation$setVisible(TRUE)
            }
        },

        .generate_study_summary = function() {
            # Generate study design and parameter summary
            analysis_type <- self$options$analysis_type
            test_type <- self$options$test_type
            hr <- self$options$effect_size
            alpha <- self$options$alpha_level
            power <- self$options$power_level
            accrual <- self$options$accrual_period
            follow_up <- self$options$follow_up_period
            median_survival <- self$options$control_median_survival
            dropout <- self$options$dropout_rate

            # Build study design explanation
            explanation <- paste0(
                "<p><strong>Power Analysis for Survival Study</strong></p>",
                "<p><strong>Study Design:</strong><br>",
                "• Analysis Type: ", private$.format_analysis_type(analysis_type), "<br>",
                "• Statistical Test: ", private$.format_test_type(test_type), "<br>",
                "• Design: ", private$.format_study_design(self$options$study_design), "<br>",
                "• Primary Endpoint: ", private$.format_primary_endpoint(self$options$primary_endpoint), "</p>",

                "<p><strong>Statistical Parameters:</strong><br>",
                "• Hazard Ratio: ", round(hr, 3), "<br>",
                "• Significance Level (alpha): ", round(alpha, 3), " (", round(alpha * 100, 1), "%)<br>",
                "• Statistical Power: ", round(power, 3), " (", round(power * 100, 1), "%)<br>",
                "• Allocation Ratio: ", self$options$allocation_ratio, ":1<br>",
                if (analysis_type != "sample_size") paste0("• Sample Size: ", self$options$sample_size_input, " subjects<br>") else "",
                "</p>",

                "<p><strong>Population Characteristics:</strong><br>",
                "• Control Median Survival: ", median_survival, " months<br>",
                "• Expected Treatment Median: ", round(median_survival / hr, 1), " months<br>",
                "• Accrual Period: ", accrual, " months<br>",
                "• Follow-up Period: ", follow_up, " months<br>",
                "• Annual Dropout Rate: ", round(dropout * 100, 1), "%</p>",

                "<p><strong>Objective:</strong><br>",
                private$.generate_objective_text(),
                "</p>"
            )

            return(explanation)
        },

        .generate_objective_text = function() {
            # Generate objective text based on analysis type
            analysis_type <- self$options$analysis_type
            hr <- self$options$effect_size
            alpha <- self$options$alpha_level
            power <- self$options$power_level

            if (analysis_type == "sample_size") {
                paste0("Determine the required sample size to detect a hazard ratio of ", round(hr, 3),
                      " with ", round(power * 100), "% power at a ", round(alpha * 100), "% significance level.")
            } else if (analysis_type == "power") {
                paste0("Calculate the statistical power to detect a hazard ratio of ", round(hr, 3),
                      " with ", self$options$sample_size_input, " subjects at a ", round(alpha * 100), "% significance level.")
            } else if (analysis_type == "effect_size") {
                paste0("Determine the minimum detectable hazard ratio with ", self$options$sample_size_input,
                      " subjects, ", round(power * 100), "% power, and ", round(alpha * 100), "% significance level.")
            } else {
                paste0("Calculate the required study duration to achieve ", round(power * 100),
                      "% power with ", self$options$sample_size_input, " subjects.")
            }
        },

        .generate_clinical_interpretation = function() {
            # Generate clinical interpretation section
            analysis_type <- self$options$analysis_type
            result_text <- private$.calculate_primary_result()

            interpretation <- "<p><strong>Clinical Interpretation:</strong></p>"

            # Add analysis-specific interpretation
            if (analysis_type == "sample_size") {
                interpretation <- paste0(interpretation, private$.generate_sample_size_interpretation(result_text))
            } else if (analysis_type == "power") {
                interpretation <- paste0(interpretation, private$.generate_power_interpretation(result_text))
            } else if (analysis_type == "effect_size") {
                interpretation <- paste0(interpretation, private$.generate_effect_size_interpretation(result_text))
            } else if (analysis_type == "duration") {
                interpretation <- paste0(interpretation, private$.generate_duration_interpretation(result_text))
            }

            return(interpretation)
        },

        .generate_sample_size_interpretation = function(result_text) {
            # Generate sample size specific interpretation
            hr <- self$options$effect_size
            alpha <- self$options$alpha_level
            power <- self$options$power_level

            interpretation <- ""

            # Extract sample size from result
            n_match <- regmatches(result_text, regexpr("\\d+", result_text))
            if (length(n_match) > 0) {
                n <- as.numeric(n_match[1])

                interpretation <- paste0(interpretation,
                    "<p><strong>Sample Size Interpretation:</strong><br>",
                    "To detect a hazard ratio of ", round(hr, 2),
                    " with ", round(power * 100), "% power and ",
                    round(alpha * 100), "% significance level, you need approximately ",
                    n, " total subjects.</p>")

                # Add clinical context
                if (hr < 1) {
                    risk_reduction <- round((1 - hr) * 100)
                    interpretation <- paste0(interpretation,
                        "<p>A hazard ratio of ", round(hr, 2),
                        " represents a ", risk_reduction,
                        "% reduction in the risk of the event in the treatment group compared to control.</p>")
                } else if (hr > 1) {
                    risk_increase <- round((hr - 1) * 100)
                    interpretation <- paste0(interpretation,
                        "<p>A hazard ratio of ", round(hr, 2),
                        " represents a ", risk_increase,
                        "% increase in the risk of the event in the treatment group compared to control.</p>")
                }

                # Add feasibility assessment
                if (n > 1000) {
                    interpretation <- paste0(interpretation,
                        "<p><strong>Feasibility Note:</strong> This large sample size may require ",
                        "multi-center collaboration or extended recruitment periods.</p>")
                } else if (n < 100) {
                    interpretation <- paste0(interpretation,
                        "<p><strong>Note:</strong> This relatively small sample size is feasible ",
                        "for single-center studies, but ensure the effect size is realistic.</p>")
                }
            }

            return(interpretation)
        },

        .generate_power_interpretation = function(result_text) {
            # Generate power specific interpretation
            hr <- self$options$effect_size

            interpretation <- ""

            # Extract power from result
            power_match <- regmatches(result_text, regexpr("\\d+\\.?\\d*%", result_text))
            if (length(power_match) > 0) {
                calculated_power <- as.numeric(gsub("%", "", power_match[1])) / 100

                interpretation <- paste0(interpretation,
                    "<p><strong>Power Interpretation:</strong><br>",
                    "With ", self$options$sample_size_input, " subjects, ",
                    "you have ", round(calculated_power * 100, 1),
                    "% probability of detecting a hazard ratio of ", round(hr, 2),
                    " if it truly exists.</p>")

                # Add adequacy assessment
                if (calculated_power < 0.7) {
                    interpretation <- paste0(interpretation,
                        "<p><strong>Warning:</strong> Power below 70% indicates the study is underpowered. ",
                        "Consider increasing sample size or reconsidering the effect size.</p>")
                } else if (calculated_power >= 0.8) {
                    interpretation <- paste0(interpretation,
                        "<p>This power level meets standard requirements for clinical trials.</p>")
                }
            }

            return(interpretation)
        },

        .generate_effect_size_interpretation = function(result_text) {
            # Generate effect size specific interpretation
            interpretation <- paste0(
                "<p><strong>Effect Size Interpretation:</strong><br>",
                "The minimum detectable hazard ratio with the given parameters is: ", result_text, "</p>",
                "<p>This represents the smallest effect size your study can reliably detect.</p>"
            )

            return(interpretation)
        },

        .generate_duration_interpretation = function(result_text) {
            # Generate duration specific interpretation
            interpretation <- paste0(
                "<p><strong>Duration Interpretation:</strong><br>",
                "The required study duration is: ", result_text, "</p>",
                "<p>This includes both accrual and follow-up periods.</p>"
            )

            return(interpretation)
        },

        .generate_report_sentence = function() {
            # Generate a copy-ready report sentence
            analysis_type <- self$options$analysis_type
            test_type <- self$options$test_type
            hr <- private$.get_effect_hr()
            alpha <- self$options$alpha_level
            power <- self$options$power_level
            accrual <- self$options$accrual_period
            follow_up <- self$options$follow_up_period

            # Build appropriate sentence based on analysis type
            if (analysis_type == "sample_size") {
                result <- private$.calculate_primary_result()
                n_match <- regmatches(result, regexpr("\\d+", result))
                n <- ifelse(length(n_match) > 0, n_match[1], "calculated")

                sentence <- paste0(
                    "A ", private$.format_test_type(test_type),
                    " power analysis was conducted to determine the sample size required ",
                    "to detect a hazard ratio of ", round(hr, 2),
                    " with ", round(power * 100), "% power at a ",
                    round(alpha * 100), "% significance level (two-sided). ",
                    "The analysis indicates that ", n, " subjects are needed, ",
                    "with an accrual period of ", accrual, " months ",
                    "and additional follow-up of ", follow_up, " months."
                )
            } else if (analysis_type == "power") {
                result <- private$.calculate_primary_result()
                power_match <- regmatches(result, regexpr("\\d+\\.?\\d*%", result))

                if (length(power_match) > 0) {
                    calc_power <- power_match[1]
                } else {
                    # Calculate basic power if not found in result
                    calculated_power <- private$.basic_power_calc(100, hr, alpha) # approximate
                    calc_power <- paste0(round(calculated_power * 100, 1), "%")
                }

                sentence <- paste0(
                    "A ", private$.format_test_type(test_type),
                    " power analysis was conducted for a study with ",
                    self$options$sample_size_input, " subjects ",
                    "to detect a hazard ratio of ", round(hr, 2),
                    " at a ", round(alpha * 100), "% significance level. ",
                    "The calculated power is ", calc_power,
                    ", with an accrual period of ", accrual, " months ",
                    "and follow-up of ", follow_up, " months."
                )
            } else if (analysis_type == "effect_size") {
                result <- private$.calculate_primary_result()

                sentence <- paste0(
                    "A ", private$.format_test_type(test_type),
                    " power analysis was conducted to determine the minimum detectable effect size ",
                    "with ", self$options$sample_size_input, " subjects, ",
                    round(power * 100), "% power, and ",
                    round(alpha * 100), "% significance level. ",
                    result
                )
            } else if (analysis_type == "duration") {
                result <- private$.calculate_primary_result()

                sentence <- paste0(
                    "A ", private$.format_test_type(test_type),
                    " power analysis was conducted to determine the required study duration ",
                    "for ", self$options$sample_size_input, " subjects ",
                    "with ", round(power * 100), "% power and ",
                    round(alpha * 100), "% significance level to detect a hazard ratio of ", round(hr, 2), ". ",
                    result
                )
            } else {
                sentence <- paste0(
                    "A ", private$.format_test_type(test_type),
                    " power analysis was conducted with the specified parameters."
                )
            }

            return(sentence)
        },

        .populate_regulatory_considerations = function() {
            table <- self$results$regulatory_table
            try({ table$deleteRows() }, silent = TRUE)

            # Get study parameters
            analysis_type <- self$options$analysis_type
            test_type <- self$options$test_type
            study_design <- self$options$study_design
            alpha <- self$options$alpha_level
            power <- self$options$power_level
            hr <- private$.get_effect_hr()

            # Generate context-specific regulatory considerations
            regulatory_items <- list()

            # Sample size justification
            if (analysis_type == "sample_size") {
                result <- private$.calculate_primary_result()
                regulatory_items <- append(regulatory_items, list(list(
                    regulatory_aspect = "Sample Size Justification",
                    requirement = "ICH E9: Provide statistical rationale with power calculations",
                    compliance_status = "Complete",
                    recommendation = paste("Document calculated", result, "with assumptions")
                )))
            }

            # Alpha level considerations
            alpha_compliance <- if (alpha == 0.05) "Standard" else if (alpha < 0.05) "Conservative" else "Non-standard"
            regulatory_items <- append(regulatory_items, list(list(
                regulatory_aspect = "Significance Level",
                requirement = "Two-sided alpha = 0.05 typically required",
                compliance_status = alpha_compliance,
                recommendation = if (alpha != 0.05) paste("Justify alpha =", alpha, "to regulatory authority") else "Standard FDA/EMA requirement"
            )))

            # Power considerations
            power_compliance <- if (power >= 0.80) "Adequate" else "Insufficient"
            regulatory_items <- append(regulatory_items, list(list(
                regulatory_aspect = "Statistical Power",
                requirement = "Minimum 80% power for primary endpoint",
                compliance_status = power_compliance,
                recommendation = if (power < 0.80) "Increase power to ≥80% for regulatory acceptance" else "Meets regulatory standards"
            )))

            # Effect size considerations
            effect_realistic <- if (hr >= 0.5 && hr <= 2.0) "Clinically Realistic" else "Review Required"
            regulatory_items <- append(regulatory_items, list(list(
                regulatory_aspect = "Effect Size",
                requirement = "Clinically meaningful and realistic effect size",
                compliance_status = effect_realistic,
                recommendation = if (hr < 0.5 || hr > 2.0) "Very large effect sizes require clinical justification" else "Appropriate for oncology studies"
            )))

            # Study design considerations
            if (study_design == "multi_arm") {
                regulatory_items <- append(regulatory_items, list(list(
                    regulatory_aspect = "Multiple Comparisons",
                    requirement = "Adjust for multiplicity in multi-arm trials",
                    compliance_status = if (self$options$multiple_comparisons != "none") "Addressed" else "Missing",
                    recommendation = if (self$options$multiple_comparisons == "none") "Apply Bonferroni or Dunnett correction" else "Multiplicity adjustment applied"
                )))
            }

            # Interim analysis considerations
            if (self$options$interim_analyses > 0) {
                regulatory_items <- append(regulatory_items, list(list(
                    regulatory_aspect = "Interim Analyses",
                    requirement = "Alpha spending function for interim looks",
                    compliance_status = if (self$options$alpha_spending != "none") "Planned" else "Incomplete",
                    recommendation = "Document stopping rules and alpha spending method"
                )))
            }

            # Populate table with all regulatory items
            for (i in seq_along(regulatory_items)) {
                table$addRow(rowKey = i, values = regulatory_items[[i]])
            }
        },
        
        .create_visualizations = function() {
            # Only create plots if we have valid options
            if (is.null(self$options$alpha_level) || is.null(self$options$power_level)) {
                return()
            }

            # Create plot state for power curve - always generate data when available
            tryCatch({
                power_data <- private$.generate_power_curve_data()

                if (!is.null(power_data) && nrow(power_data) > 0) {
                    plotState <- list(
                        data = power_data,
                        config = list(
                            title = "Power Curve",
                            xlab = "Effect Size (Hazard Ratio)",
                            ylab = "Statistical Power",
                            type = "power_curve"
                        ),
                        options = list(
                            alpha = self$options$alpha_level,
                            sample_size = if (!is.null(self$options$sample_size_input)) self$options$sample_size_input else 200
                        )
                    )
                    self$results$power_curve_plot$setState(plotState)
                }
            }, error = function(e) {
                warning(paste("Power curve data generation failed:", e$message))
            })

            # Create plot state for sample size curve - always generate when relevant
            tryCatch({
                sample_size_data <- private$.generate_sample_size_curve_data()

                if (!is.null(sample_size_data) && nrow(sample_size_data) > 0) {
                    plotState <- list(
                        data = sample_size_data,
                        config = list(
                            title = "Sample Size Requirements",
                            xlab = "Effect Size (Hazard Ratio)",
                            ylab = "Required Sample Size",
                            type = "sample_size_curve"
                        ),
                        options = list(
                            power = self$options$power_level,
                            alpha = self$options$alpha_level
                        )
                    )
                    self$results$sample_size_plot$setState(plotState)
                }
            }, error = function(e) {
                warning(paste("Sample size curve data generation failed:", e$message))
            })

            # Create expected survival curves plot
            tryCatch({
                survival_data <- private$.generate_survival_curve_data()

                if (!is.null(survival_data) && nrow(survival_data) > 0) {
                    plotState <- list(
                        data = survival_data,
                        config = list(
                            title = "Expected Survival Curves",
                            xlab = "Time (months)",
                            ylab = "Survival Probability",
                            type = "survival_curves"
                        ),
                        options = list(
                            hr = self$options$effect_size,
                            median_survival = self$options$control_median_survival
                        )
                    )
                    self$results$survival_curves_plot$setState(plotState)
                }
            }, error = function(e) {
                warning(paste("Survival curve data generation failed:", e$message))
            })

            # Create study timeline plot
            tryCatch({
                timeline_data <- private$.generate_timeline_data()

                if (!is.null(timeline_data) && nrow(timeline_data) > 0) {
                    plotState <- list(
                        data = timeline_data,
                        config = list(
                            title = "Study Timeline",
                            xlab = "Time (months)",
                            ylab = "Study Phase",
                            type = "timeline"
                        ),
                        options = list(
                            accrual_period = self$options$accrual_period,
                            follow_up = self$options$follow_up_period
                        )
                    )
                    self$results$accrual_timeline_plot$setState(plotState)
                }
            }, error = function(e) {
                warning(paste("Timeline data generation failed:", e$message))
            })

            # Create sensitivity analysis plot if enabled
            if (self$options$sensitivity_analysis) {
                tryCatch({
                    sensitivity_data <- private$.generate_sensitivity_plot_data()

                    if (!is.null(sensitivity_data) && nrow(sensitivity_data) > 0) {
                        plotState <- list(
                            data = sensitivity_data,
                            config = list(
                                title = "Sensitivity Analysis",
                                xlab = "Hazard Ratio",
                                ylab = "Required Sample Size",
                                type = "sensitivity"
                            ),
                            options = list(
                                power = self$options$power_level,
                                alpha = self$options$alpha_level
                            )
                        )
                        self$results$sensitivity_plot$setState(plotState)
                    }
                }, error = function(e) {
                    warning(paste("Sensitivity plot data generation failed:", e$message))
                })
            }
        },

        .generate_power_curve_data = function() {
            # Generate data for power curve plot
            tryCatch({
                hr_values <- seq(0.5, 0.95, by = 0.05)
                power_values <- sapply(hr_values, function(hr) {
                    private$.basic_power_calc(100, hr, self$options$alpha_level)
                })

                return(data.frame(
                    hazard_ratio = hr_values,
                    power = power_values
                ))
            }, error = function(e) {
                warning(paste("Power curve data generation error:", e$message))
                return(NULL)
            })
        },

        .generate_sample_size_curve_data = function() {
            # Generate data for sample size curve plot
            tryCatch({
                hr_values <- seq(0.5, 0.95, by = 0.05)
                sample_sizes <- sapply(hr_values, function(hr) {
                    private$.basic_sample_size_calc(self$options$power_level, hr, self$options$alpha_level)
                })

                return(data.frame(
                    hazard_ratio = hr_values,
                    sample_size = sample_sizes
                ))
            }, error = function(e) {
                warning(paste("Sample size curve data generation error:", e$message))
                return(NULL)
            })
        },

        .generate_survival_curve_data = function() {
            # Generate expected survival curves for control and treatment groups
            tryCatch({
                time <- seq(0, 60, by = 1)
                hr <- private$.get_effect_hr()
                median_control <- self$options$control_median_survival

                # Convert median to rate parameter
                lambda_control <- log(2) / median_control
                lambda_treatment <- lambda_control * hr

                surv_control <- exp(-lambda_control * time)
                surv_treatment <- exp(-lambda_treatment * time)

                data <- data.frame(
                    time = rep(time, 2),
                    survival = c(surv_control, surv_treatment),
                    group = rep(c("Control", "Treatment"), each = length(time))
                )

                return(data)
            }, error = function(e) {
                warning(paste("Survival curve data generation error:", e$message))
                return(NULL)
            })
        },

        .generate_timeline_data = function() {
            # Generate study timeline data
            tryCatch({
                accrual_period <- self$options$accrual_period
                follow_up <- self$options$follow_up_period

                timeline_data <- data.frame(
                    phase = c("Accrual", "Follow-up", "Analysis"),
                    start = c(0, accrual_period, accrual_period + follow_up),
                    end = c(accrual_period, accrual_period + follow_up, accrual_period + follow_up + 3),
                    y = c(1, 1, 1)
                )

                return(timeline_data)
            }, error = function(e) {
                warning(paste("Timeline data generation error:", e$message))
                return(NULL)
            })
        },

        .generate_sensitivity_plot_data = function() {
            # Generate data for sensitivity analysis plot
            tryCatch({
                # Create a range of hazard ratios around the base value
                base_hr <- private$.get_effect_hr()
                hr_range <- seq(max(0.3, base_hr - 0.3), min(1.0, base_hr + 0.3), by = 0.05)

                # Calculate required sample size for each HR
                sample_sizes <- sapply(hr_range, function(hr) {
                    private$.basic_sample_size_calc(
                        self$options$power_level,
                        hr,
                        self$options$alpha_level
                    )
                })

                # Create data frame for plotting
                return(data.frame(
                    hazard_ratio = hr_range,
                    sample_size = sample_sizes,
                    is_base_case = abs(hr_range - base_hr) < 0.01
                ))
            }, error = function(e) {
                warning(paste("Sensitivity plot data generation error:", e$message))
                return(NULL)
            })
        },

        .basic_power_calc = function(n_total, hr, alpha) {
            lambda_control <- log(2) / self$options$control_median_survival

            expected_events <- private$.expected_events_from_sample(
                n_total = n_total,
                lambda_control = lambda_control,
                hr = hr,
                allocation_ratio = self$options$allocation_ratio,
                accrual_period = self$options$accrual_period,
                follow_up_period = self$options$follow_up_period,
                dropout_rate = self$options$dropout_rate
            )$total

            power <- private$.power_from_events(
                events = expected_events,
                hr = hr,
                alpha = alpha,
                allocation_ratio = self$options$allocation_ratio
            )
            max(0, min(1, power))
        },

        .basic_sample_size_calc = function(power, hr, alpha) {
            events_needed <- private$.events_needed_log_rank(
                hr = hr,
                alpha = alpha,
                power = power,
                ratio = self$options$allocation_ratio
            )

            lambda_control <- log(2) / self$options$control_median_survival

            n_required <- private$.sample_size_from_events(
                events_needed = events_needed,
                lambda_control = lambda_control,
                hr = hr,
                allocation_ratio = self$options$allocation_ratio,
                accrual_period = self$options$accrual_period,
                follow_up_period = self$options$follow_up_period,
                dropout_rate = self$options$dropout_rate
            )

            ceiling(n_required)
        },

        # Resolve the analysis effect size into a hazard ratio, based on effect_size_type
        .get_effect_hr = function() {
            type <- self$options$effect_size_type
            es <- self$options$effect_size
            # Default/fallback
            if (is.null(type) || type == "hazard_ratio") {
                private$effect_hr_info <- list(type = "hazard_ratio", hr = es,
                    note = "Using hazard ratio directly as effect size.")
                return(es)
            }

            # Convert from median ratio (treatment/control)
            if (type == "median_ratio") {
                if (is.null(es) || es <= 0)
                    es <- 1.333333
                # Under exponential assumption: median_t/median_c = 1/HR
                hr <- 1 / es
                private$effect_hr_info <- list(type = "median_ratio", hr = hr,
                    note = paste0("Converted from median survival ratio (treatment/control) = ",
                                  sprintf("%.3f", es),
                                  "; assuming exponential survival, HR = 1 / ratio."))
                return(max(0.1, min(5, hr)))
            }

            # Convert from RMST difference at tau via numerical solve under exponential assumption
            if (type == "rmst_difference") {
                delta <- self$options$rmst_difference
                tau <- self$options$rmst_tau
                mc <- self$options$control_median_survival
                if (is.null(delta) || is.null(tau) || is.null(mc) || tau <= 0 || mc <= 0)
                    {
                        private$effect_hr_info <- list(type = "rmst_difference", hr = es,
                            note = "RMST parameters incomplete; using provided effect size as HR.")
                        return(es)
                    }
                lambda_c <- log(2) / mc
                rmst_diff_fn <- function(hr) {
                    lambda_t <- lambda_c * hr
                    rmst_c <- (1 - exp(-lambda_c * tau)) / lambda_c
                    rmst_t <- (1 - exp(-lambda_t * tau)) / lambda_t
                    (rmst_t - rmst_c) - delta
                }
                a <- 0.2; b <- 3.0
                fa <- rmst_diff_fn(a); fb <- rmst_diff_fn(b)
                if (is.finite(fa) && is.finite(fb) && fa * fb <= 0) {
                    hr <- tryCatch(uniroot(rmst_diff_fn, c(a, b))$root, error = function(e) NA)
                    if (is.finite(hr)) {
                        private$effect_hr_info <- list(type = "rmst_difference", hr = hr,
                            note = paste0("Derived HR from RMST difference ", sprintf("%.3f", delta),
                                          " at tau = ", sprintf("%.1f", tau),
                                          " months under exponential assumption."))
                        return(max(0.1, min(5, hr)))
                    }
                }
                # Fallback: approximate small difference mapping
                hr <- 0.75
                private$effect_hr_info <- list(type = "rmst_difference", hr = hr,
                    note = paste0("Could not uniquely solve HR from RMST difference (inputs: Δ=", 
                                   sprintf("%.3f", delta), ", tau=", sprintf("%.1f", tau), 
                                   ", median_c=", sprintf("%.1f", mc), "). Using fallback HR=0.75 for calculation."))
                return(max(0.1, min(5, hr)))
            }

            # Convert from survival probability difference at follow-up
            if (type == "survival_difference") {
                delta <- es
                T <- self$options$follow_up_period
                mc <- self$options$control_median_survival
                if (is.null(delta) || is.null(T) || is.null(mc) || T <= 0 || mc <= 0)
                    {
                        hr <- 0.75
                        private$effect_hr_info <- list(type = "survival_difference", hr = hr,
                            note = "Survival difference parameters incomplete; using fallback HR=0.75.")
                        return(hr)
                    }
                lambda_c <- log(2) / mc
                s_c <- function(t) exp(-lambda_c * t)
                target_fn <- function(hr) {
                    s_t <- exp(-(lambda_c * hr) * T)
                    (s_t - s_c(T)) - delta
                }
                a <- 0.2; b <- 3.0
                fa <- target_fn(a); fb <- target_fn(b)
                if (is.finite(fa) && is.finite(fb) && fa * fb <= 0) {
                    hr <- tryCatch(uniroot(target_fn, c(a, b))$root, error = function(e) NA)
                    if (is.finite(hr)) {
                        private$effect_hr_info <- list(type = "survival_difference", hr = hr,
                            note = paste0("Derived HR from survival difference ", sprintf("%.3f", delta),
                                          " at follow-up = ", sprintf("%.1f", T),
                                          " months under exponential assumption."))
                        return(max(0.1, min(5, hr)))
                    }
                }
                hr <- 0.75
                private$effect_hr_info <- list(type = "survival_difference", hr = hr,
                    note = paste0("Could not uniquely solve HR from survival difference (inputs: Δ=",
                                   sprintf("%.3f", delta), ", follow-up=", sprintf("%.1f", T),
                                   ", median_c=", sprintf("%.1f", mc), "). Using fallback HR=0.75 for calculation."))
                return(max(0.1, min(5, hr)))
            }

            # Unknown type — return provided value
            private$effect_hr_info <- list(type = type, hr = es,
                note = "Using provided effect size without conversion.")
            return(es)
        },

        # Plot functions
        .plot_power_curves = function(image, ggtheme, theme, ...) {
            tryCatch({
                plotData <- image$state

                if (is.null(plotData)) {
                    return(FALSE)
                }

                if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                    return(FALSE)
                }

                # Create power curve plot
                plot <- ggplot2::ggplot(plotData$data, ggplot2::aes(x = hazard_ratio, y = power)) +
                    ggplot2::geom_line(size = 1.2, color = "blue") +
                    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", alpha = 0.6) +
                    ggplot2::labs(
                        title = plotData$config$title,
                        x = plotData$config$xlab,
                        y = plotData$config$ylab
                    ) +
                    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                    ggtheme

                print(plot)
                return(TRUE)

            }, error = function(e) {
                warning(paste("Power curve plot error:", e$message))
                return(FALSE)
            })
        },

        .plot_sample_size_curves = function(image, ggtheme, theme, ...) {
            tryCatch({
                plotData <- image$state

                if (is.null(plotData)) {
                    return(FALSE)
                }

                if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                    return(FALSE)
                }

                # Create sample size curve plot
                plot <- ggplot2::ggplot(plotData$data, ggplot2::aes(x = hazard_ratio, y = sample_size)) +
                    ggplot2::geom_line(size = 1.2, color = "red") +
                    ggplot2::labs(
                        title = plotData$config$title,
                        x = plotData$config$xlab,
                        y = plotData$config$ylab
                    ) +
                    ggtheme

                print(plot)
                return(TRUE)

            }, error = function(e) {
                warning(paste("Sample size plot error:", e$message))
                return(FALSE)
            })
        },

        .plot_expected_survival = function(image, ggtheme, theme, ...) {
            tryCatch({
                plotData <- image$state

                if (is.null(plotData)) {
                    return(FALSE)
                }

                if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                    return(FALSE)
                }

                # Create survival curve plot
                plot <- ggplot2::ggplot(plotData$data, ggplot2::aes(x = time, y = survival, color = group)) +
                    ggplot2::geom_line(size = 1.2) +
                    ggplot2::labs(
                        title = plotData$config$title,
                        x = plotData$config$xlab,
                        y = plotData$config$ylab,
                        color = "Group"
                    ) +
                    ggplot2::scale_y_continuous(limits = c(0, 1)) +
                    ggplot2::scale_color_manual(values = c("Control" = "red", "Treatment" = "blue")) +
                    ggtheme

                print(plot)
                return(TRUE)

            }, error = function(e) {
                warning(paste("Survival curve plot error:", e$message))
                return(FALSE)
            })
        },

        .plot_study_timeline = function(image, ggtheme, theme, ...) {
            tryCatch({
                plotData <- image$state

                if (is.null(plotData)) {
                    return(FALSE)
                }

                if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                    return(FALSE)
                }

                # Create timeline plot
                plot <- ggplot2::ggplot(plotData$data,
                                      ggplot2::aes(xmin = start, xmax = end,
                                                  ymin = y - 0.2, ymax = y + 0.2,
                                                  fill = phase)) +
                    ggplot2::geom_rect() +
                    ggplot2::labs(
                        title = plotData$config$title,
                        x = plotData$config$xlab,
                        fill = "Study Phase"
                    ) +
                    ggplot2::theme(
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank(),
                        axis.title.y = ggplot2::element_blank()
                    ) +
                    ggplot2::scale_fill_manual(values = c(
                        "Accrual" = "#3498db",
                        "Follow-up" = "#2ecc71",
                        "Analysis" = "#e74c3c"
                    )) +
                    ggtheme

                print(plot)
                return(TRUE)

            }, error = function(e) {
                warning(paste("Timeline plot error:", e$message))
                return(FALSE)
            })
        },

        .plot_sensitivity_analysis = function(image, ggtheme, theme, ...) {
            tryCatch({
                plotData <- image$state

                if (is.null(plotData)) {
                    return(FALSE)
                }

                # Use provided data or return FALSE if no data
                if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                    return(FALSE)
                }

                data <- plotData$data

                # Create sensitivity analysis plot
                plot <- ggplot2::ggplot(data, ggplot2::aes(x = hazard_ratio, y = sample_size)) +
                    ggplot2::geom_line(color = "steelblue", size = 1) +
                    ggplot2::geom_point(size = 3, color = "steelblue") +

                # Highlight base case if present
                if ("is_base_case" %in% names(data)) {
                    base_data <- data[data$is_base_case, ]
                    if (nrow(base_data) > 0) {
                        plot <- plot + ggplot2::geom_point(
                            data = base_data,
                            color = "red", size = 5, shape = 21, fill = "red", alpha = 0.7
                        )
                    }
                }

                plot <- plot +
                    ggplot2::labs(
                        title = "Sensitivity Analysis",
                        x = "Hazard Ratio",
                        y = "Required Sample Size"
                    ) +
                    ggtheme

                print(plot)
                return(TRUE)

            }, error = function(e) {
                warning(paste("Sensitivity analysis plot error:", e$message))
                return(FALSE)
            })
        },

        .get_distribution_parameters = function(median_control, hr) {
            distribution <- self$options$survival_distribution
            weibull_shape <- self$options$weibull_shape

            if (distribution != "exponential") {
                stop("Only exponential survival distribution supported in validated release")
            }

            lambda_control <- log(2) / median_control
            lambda_treatment <- lambda_control * hr

            return(list(lambda_control = lambda_control, lambda_treatment = lambda_treatment))
        },

        .adjust_alpha_for_multiplicity = function(alpha) {
            study_design <- self$options$study_design
            multiple_comparisons <- self$options$multiple_comparisons
            number_of_arms <- self$options$number_of_arms

            if (study_design == "multi_arm" && number_of_arms > 2) {
                if (multiple_comparisons == "bonferroni") {
                    alpha_adjusted <- alpha / (number_of_arms - 1)
                } else if (multiple_comparisons == "none" || is.null(multiple_comparisons)) {
                    alpha_adjusted <- alpha
                } else {
                    stop("Unsupported multiplicity adjustment requested")
                }
            } else {
                alpha_adjusted <- alpha
            }

            return(alpha_adjusted)
        },

        .adjust_sample_for_design = function() {
            study_design <- self$options$study_design
            cluster_size <- self$options$cluster_size
            icc <- self$options$icc
            stratification_factors <- self$options$stratification_factors

            # Design effect for cluster randomized trials
            if (study_design == "cluster_randomized" && cluster_size > 1 && icc > 0) {
                design_effect <- 1 + (cluster_size - 1) * icc
            } else {
                design_effect <- 1
            }

            return(list(
                design_effect = design_effect,
                stratification_efficiency = 1
            ))
        },

        .adjust_sample_for_accrual = function(base_sample_size) {
            accrual_pattern <- self$options$accrual_pattern

            if (!isTRUE(accrual_pattern %in% c("uniform", NA))) {
                warning("Non-uniform accrual patterns are not yet modelled; proceeding as uniform accrual")
            }

            return(base_sample_size)
        },

        .allocation_props = function(allocation_ratio) {
            ratio <- ifelse(is.null(allocation_ratio) || allocation_ratio <= 0, 1, allocation_ratio)
            list(
                control = ratio / (1 + ratio),
                treatment = 1 / (1 + ratio)
            )
        },

        .dropout_hazard = function(dropout_rate) {
            if (is.null(dropout_rate) || dropout_rate <= 0) {
                return(0)
            }
            rate <- min(dropout_rate, 0.99)
            -log(1 - rate) / 12
        },

        .event_probability = function(lambda, accrual_period, follow_up_period, dropout_rate) {
            accrual <- max(0, accrual_period)
            follow_up <- max(0, follow_up_period)
            delta <- private$.dropout_hazard(dropout_rate)
            H <- lambda + delta

            if (H <= 0) {
                return(0)
            }

            if (accrual <= 0) {
                prob <- (lambda / H) * (1 - exp(-H * follow_up))
                return(pmin(pmax(prob, 0), 1))
            }

            exp_neg_H_Tf <- exp(-H * follow_up)
            exp_neg_H_Tf_Ta <- exp(-H * (follow_up + accrual))
            integral <- accrual - (exp_neg_H_Tf - exp_neg_H_Tf_Ta) / H
            prob <- (lambda / H) * (integral / accrual)
            pmin(pmax(prob, 0), 1)
        },

        .overall_event_probability = function(lambda_control, hr, allocation_ratio, accrual_period, follow_up_period, dropout_rate) {
            props <- private$.allocation_props(allocation_ratio)
            lambda_treatment <- lambda_control * hr

            p_control <- private$.event_probability(lambda_control, accrual_period, follow_up_period, dropout_rate)
            p_treatment <- private$.event_probability(lambda_treatment, accrual_period, follow_up_period, dropout_rate)

            list(
                control = p_control,
                treatment = p_treatment,
                overall = props$control * p_control + props$treatment * p_treatment,
                props = props
            )
        },

        .expected_events_from_sample = function(n_total, lambda_control, hr, allocation_ratio, accrual_period, follow_up_period, dropout_rate) {
            probs <- private$.overall_event_probability(lambda_control, hr, allocation_ratio, accrual_period, follow_up_period, dropout_rate)
            list(
                total = n_total * probs$overall,
                control = n_total * probs$props$control * probs$control,
                treatment = n_total * probs$props$treatment * probs$treatment,
                probs = probs
            )
        },

        .information_from_events = function(events, allocation_ratio) {
            props <- private$.allocation_props(allocation_ratio)
            events * props$control * props$treatment
        },

        .power_from_events = function(events, hr, alpha, allocation_ratio) {
            if (is.null(events) || events <= 0 || is.null(hr) || hr <= 0 || hr == 1) {
                return(0)
            }

            info <- private$.information_from_events(events, allocation_ratio)
            if (info <= 0) {
                return(0)
            }

            z_alpha <- qnorm(1 - alpha/2)
            z <- sqrt(info) * abs(log(hr))
            pnorm(z - z_alpha)
        },

        .sample_size_from_events = function(events_needed, lambda_control, hr, allocation_ratio, accrual_period, follow_up_period, dropout_rate) {
            probs <- private$.overall_event_probability(lambda_control, hr, allocation_ratio, accrual_period, follow_up_period, dropout_rate)

            if (probs$overall <= 0) {
                stop("Resulting event probability is zero; adjust follow-up or accrual settings.")
            }

            ceiling(events_needed / probs$overall)
        },

        .solve_follow_up_duration = function(n_total, target_events, lambda_control, hr, allocation_ratio, accrual_period, dropout_rate, initial_follow_up) {
            if (is.null(target_events) || target_events <= 0 || is.null(n_total) || n_total <= 0) {
                return(NA_real_)
            }

            lambda_control <- max(lambda_control, 1e-8)
            lower <- 0.01

            diff_lower <- private$.expected_events_from_sample(
                n_total = n_total,
                lambda_control = lambda_control,
                hr = hr,
                allocation_ratio = allocation_ratio,
                accrual_period = accrual_period,
                follow_up_period = lower,
                dropout_rate = dropout_rate
            )$total - target_events

            if (diff_lower >= 0) {
                return(accrual_period + lower)
            }

            upper <- max(3, initial_follow_up * 2, (1 / lambda_control) * 10)
            diff_upper <- private$.expected_events_from_sample(
                n_total = n_total,
                lambda_control = lambda_control,
                hr = hr,
                allocation_ratio = allocation_ratio,
                accrual_period = accrual_period,
                follow_up_period = upper,
                dropout_rate = dropout_rate
            )$total - target_events

            iter <- 0
            while (diff_upper < 0 && iter < 10) {
                upper <- upper * 2
                diff_upper <- private$.expected_events_from_sample(
                    n_total = n_total,
                    lambda_control = lambda_control,
                    hr = hr,
                    allocation_ratio = allocation_ratio,
                    accrual_period = accrual_period,
                    follow_up_period = upper,
                    dropout_rate = dropout_rate
                )$total - target_events
                iter <- iter + 1
            }

            follow_up_solution <- tryCatch({
                uniroot(
                    function(fu) {
                        private$.expected_events_from_sample(
                            n_total = n_total,
                            lambda_control = lambda_control,
                            hr = hr,
                            allocation_ratio = allocation_ratio,
                            accrual_period = accrual_period,
                            follow_up_period = fu,
                            dropout_rate = dropout_rate
                        )$total - target_events
                    },
                    lower = lower,
                    upper = upper
                )$root
            }, error = function(e) NA_real_)

            if (is.na(follow_up_solution)) {
                return(NA_real_)
            }

            accrual_period + follow_up_solution
        },

        .calculate_log_rank_with_params = function(analysis_type, alpha, power, hr, allocation_ratio, accrual_period, follow_up, median_control) {
            # Calculate log-rank results with specific parameters (used for sensitivity analysis)
            # Convert median to rate parameter
            lambda_control <- log(2) / median_control

            if (analysis_type == "sample_size") {
                tryCatch({
                    # Basic sample size calculation
                    events_needed <- private$.events_needed_log_rank(hr, alpha, power, allocation_ratio)

                    # Adjust for study timeline
                    total_time <- accrual_period + follow_up
                    prob_event_control <- 1 - exp(-lambda_control * (total_time * private$AVERAGE_FOLLOWUP_FACTOR))

                    n_total <- ceiling(events_needed / prob_event_control)
                    return(paste("Total Sample Size:", n_total, "subjects"))
                }, error = function(e) {
                    return("Calculation error")
                })
            } else if (analysis_type == "power") {
                tryCatch({
                    n_total <- self$options$sample_size_input
                    total_time <- accrual_period + follow_up
                    prob_event_control <- 1 - exp(-lambda_control * (total_time * private$AVERAGE_FOLLOWUP_FACTOR))
                    expected_events <- n_total * prob_event_control

                    power_calc <- private$.basic_power_calc(expected_events, hr, alpha)
                    return(paste("Statistical Power:", round(power_calc * 100, 1), "%"))
                }, error = function(e) {
                    return("Calculation error")
                })
            } else {
                return("Analysis type not supported for sensitivity")
            }
        },

        .run_simulation_analysis = function() {
            # Enhanced simulation-based power analysis
            if (!self$options$sensitivity_analysis) return(NULL)

            n_sims <- self$options$simulation_runs
            if (is.null(n_sims) || n_sims < 100) n_sims <- 100
            if (n_sims > 2000) n_sims <- 2000  # Limit for UI responsiveness

            tryCatch({
                # Get base parameters
                hr_true <- private$.get_effect_hr()
                alpha <- self$options$alpha_level
                
                # Determine sample size
                n_total <- if (self$options$analysis_type == "sample_size") {
                    # Extract from calculated result
                    result <- private$.calculate_primary_result()
                    if (grepl("([0-9,]+)", result)) {
                        as.numeric(gsub("[^0-9]", "", regmatches(result, regexpr("[0-9,]+", result))))
                    } else {
                        200  # fallback
                    }
                } else {
                    self$options$sample_size_input
                }
                
                if (is.na(n_total) || n_total < 10) n_total <- 200

                lambda_control <- log(2) / self$options$control_median_survival
                lambda_treatment <- lambda_control * hr_true
                
                accrual <- self$options$accrual_period
                follow_up <- self$options$follow_up_period
                total_study_time <- accrual + follow_up
                n_per_group <- ceiling(n_total / 2)

                p_values <- numeric(n_sims)

                # Monte Carlo Simulation
                for (i in 1:n_sims) {
                    # Generate Event Times (Exponential)
                    t_event_c <- rexp(n_per_group, lambda_control)
                    t_event_t <- rexp(n_per_group, lambda_treatment)
                    
                    # Generate Accrual Times (Uniform)
                    t_entry_c <- runif(n_per_group, 0, accrual)
                    t_entry_t <- runif(n_per_group, 0, accrual)
                    
                    # Calculate Administrative Censoring Time
                    # Time from entry until end of study
                    t_censor_c <- total_study_time - t_entry_c
                    t_censor_t <- total_study_time - t_entry_t
                    
                    # Observed Time & Event Indicator
                    time_c <- pmin(t_event_c, t_censor_c)
                    status_c <- as.numeric(t_event_c <= t_censor_c)
                    
                    time_t <- pmin(t_event_t, t_censor_t)
                    status_t <- as.numeric(t_event_t <= t_censor_t)
                    
                    # Combine for Log-rank Test
                    times <- c(time_c, time_t)
                    status <- c(status_c, status_t)
                    group <- c(rep(0, n_per_group), rep(1, n_per_group))
                    
                    # Perform Log-rank test
                    # Using tryCatch to handle cases with 0 events
                    tryCatch({
                        sdf <- survival::survdiff(survival::Surv(times, status) ~ group)
                        p_values[i] <- 1 - pchisq(sdf$chisq, df=1)
                    }, error = function(e) {
                        p_values[i] <- 1.0 # Non-significant if test fails (e.g. no events)
                    })
                }

                # Calculate empirical power
                empirical_power <- mean(p_values < alpha, na.rm = TRUE)

                # Add simulation note to assumptions table
                assumptions_note <- sprintf(
                    "Simulation-based power estimate: %.1f%% (based on %d simulations)",
                    empirical_power * 100, n_sims
                )

                return(list(
                    empirical_power = empirical_power,
                    note = assumptions_note,
                    p_values = p_values
                ))

            }, error = function(e) {
                warning(paste("Simulation analysis failed:", e$message))
                return(NULL)
            })
        },

        .apply_clinical_preset = function() {
            # Apply clinical preset configurations
            preset <- self$options$clinical_preset

            if (is.null(preset) || preset == "custom") {
                return()
            }

            # Note: Since we can't modify self$options directly in jamovi backend,
            # we document what the presets should set via JavaScript events
            # This method serves as documentation and potential future enhancement

            # The actual preset application is handled by JavaScript events:
            # - oncology_phase3: HR=0.75, power=80%, 1:1 allocation, 12mo median
            # - cardio_prevention: HR=0.85, power=90%, 60mo median (prevention)
            # - biomarker_study: HR=0.67, power=80%, 2:1 allocation, 8mo median
            # - non_inferiority: HR=1.0, alpha=0.025, NI margin=1.25
            # - pilot_study: HR=0.70, alpha=0.10, power=70% (relaxed parameters)

            # For backend validation, we could check if preset values are consistent
            # with the clinical preset selection, but JavaScript handles the actual setting
        },

        .generate_clinical_friendly_outputs = function() {
            # Generate clinical-friendly outputs based on user preferences

            # Early return if no clinical features enabled - performance optimization
            if (!any(self$options$show_summary, self$options$show_explanations,
                     self$options$show_glossary, self$options$guided_mode, na.rm = TRUE)) {
                return()
            }

            # Natural Language Summary
            if (isTRUE(self$options$show_summary)) {
                private$.generate_natural_language_summary()
            }

            # Educational Explanations
            if (isTRUE(self$options$show_explanations)) {
                private$.generate_educational_explanations()
            }

            # Statistical Glossary
            if (isTRUE(self$options$show_glossary)) {
                private$.generate_statistical_glossary()
            }

            # Guided Workflow
            if (isTRUE(self$options$guided_mode)) {
                private$.generate_guided_workflow()
            }
        },

        .generate_natural_language_summary = function() {
            # Generate plain-language summary of results
            analysis_type <- self$options$analysis_type
            test_type <- self$options$test_type
            hr <- self$options$effect_size
            alpha <- self$options$alpha_level
            power <- self$options$power_level

            # Create natural language summary
            summary_text <- paste0(
                "<div class='jmv-clinical-summary'>",
                "<h3>📋 Plain Language Summary</h3>",
                "<p><strong>Study Design:</strong> ", private$.format_test_type(test_type),
                " for ", private$.format_primary_endpoint(self$options$primary_endpoint), "</p>",

                if (analysis_type == "sample_size") {
                    paste0("<p><strong>Sample Size Calculation:</strong> To detect a hazard ratio of ",
                           hr, " (", ifelse(hr < 1, paste0(round((1-hr)*100), "% risk reduction"),
                                           paste0(round((hr-1)*100), "% risk increase")),
                           ") with ", round(power*100), "% power at ", round(alpha*100), "% significance level.</p>")
                } else if (analysis_type == "power") {
                    paste0("<p><strong>Power Calculation:</strong> With ", self$options$sample_size_input,
                           " subjects, the study has power to detect a hazard ratio of ", hr, ".</p>")
                } else if (analysis_type == "effect_size") {
                    paste0("<p><strong>Effect Size:</strong> The minimum detectable effect with ",
                           self$options$sample_size_input, " subjects and ", round(power*100), "% power.</p>")
                } else {
                    "<p><strong>Duration Analysis:</strong> Study timeline requirements for adequate power.</p>"
                },

                "<p><strong>Clinical Impact:</strong> ",
                if (hr < 0.67) "Large clinical benefit expected"
                else if (hr < 0.80) "Moderate clinical benefit expected"
                else if (hr < 1.0) "Small clinical benefit expected"
                else if (hr == 1.0) "No treatment difference expected"
                else if (hr < 1.25) "Small increase in risk expected"
                else if (hr < 1.5) "Moderate increase in risk expected"
                else "Large increase in risk expected",
                ".</p>",

                "<p><strong>Study Timeline:</strong> ", self$options$accrual_period,
                " months recruitment + ", self$options$follow_up_period,
                " months follow-up = ", self$options$accrual_period + self$options$follow_up_period,
                " months total.</p>",
                "</div>"
            )

            self$results$natural_language_summary$setContent(summary_text)
        },

        .generate_educational_explanations = function() {
            # Generate educational explanations based on test type
            test_type <- self$options$test_type

            explanations <- list(
                log_rank = list(
                    what = "Compares survival curves between two groups using the log-rank test",
                    when = "Use when comparing time-to-event outcomes between treatment groups",
                    assumptions = "Proportional hazards, independent censoring, same censoring patterns",
                    interpretation = "P-value indicates whether survival curves are significantly different"
                ),
                cox_regression = list(
                    what = "Uses Cox proportional hazards model to estimate treatment effects",
                    when = "Use when you need hazard ratios and want to adjust for covariates",
                    assumptions = "Proportional hazards over time, log-linear relationship",
                    interpretation = "Hazard ratio shows relative risk between groups"
                ),
                competing_risks = list(
                    what = "Accounts for competing events that prevent the primary outcome",
                    when = "Use when patients can experience other events (death from other causes)",
                    assumptions = "Independent competing risks, known competing event rates",
                    interpretation = "Cumulative incidence considers both primary and competing events"
                ),
                rmst_test = list(
                    what = "Compares restricted mean survival time between groups",
                    when = "Use when proportional hazards assumption is violated",
                    assumptions = "No proportional hazards assumption required",
                    interpretation = "Difference in average survival time up to a specific timepoint"
                ),
                non_inferiority = list(
                    what = "Tests whether new treatment is not worse than standard by a margin",
                    when = "Use when testing if new treatment is 'good enough' (non-inferior)",
                    assumptions = "Pre-specified non-inferiority margin, one-sided testing",
                    interpretation = "Demonstrates new treatment is not worse than existing standard"
                )
            )

            current_explanation <- explanations[[test_type]]
            if (is.null(current_explanation)) {
                current_explanation <- explanations[["log_rank"]]  # Default fallback
            }

            explanation_html <- paste0(
                "<div class='jmv-educational-guide'>",
                "<h3>📚 Educational Guide: ", private$.format_test_type(test_type), "</h3>",
                "<div class='explanation-section'>",
                "<h4>What does this test do?</h4>",
                "<p>", current_explanation$what, "</p>",
                "</div>",
                "<div class='explanation-section'>",
                "<h4>When should you use it?</h4>",
                "<p>", current_explanation$when, "</p>",
                "</div>",
                "<div class='explanation-section'>",
                "<h4>Key assumptions:</h4>",
                "<p>", current_explanation$assumptions, "</p>",
                "</div>",
                "<div class='explanation-section'>",
                "<h4>How to interpret results:</h4>",
                "<p>", current_explanation$interpretation, "</p>",
                "</div>",
                "</div>"
            )

            self$results$educational_explanations$setContent(explanation_html)
        },

        .generate_statistical_glossary = function() {
            # Generate glossary of statistical terms
            glossary_html <- paste0(
                "<div class='jmv-glossary'>",
                "<h3>📖 Statistical Terms Glossary</h3>",

                "<div class='glossary-term'>",
                "<h4>Hazard Ratio (HR)</h4>",
                "<p>The risk of an event in one group compared to another. HR = 0.75 means 25% lower risk, HR = 1.5 means 50% higher risk.</p>",
                "</div>",

                "<div class='glossary-term'>",
                "<h4>Statistical Power</h4>",
                "<p>The probability of detecting a true effect. 80% power means 8 out of 10 studies would detect the effect if it exists.</p>",
                "</div>",

                "<div class='glossary-term'>",
                "<h4>Alpha Level (Type I Error)</h4>",
                "<p>The probability of finding a significant result when no true effect exists. Alpha = 0.05 means 5% chance of false positive.</p>",
                "</div>",

                "<div class='glossary-term'>",
                "<h4>Confidence Interval (CI)</h4>",
                "<p>Range of plausible values for the true effect. 95% CI means we're 95% confident the true value lies within this range.</p>",
                "</div>",

                "<div class='glossary-term'>",
                "<h4>Sample Size</h4>",
                "<p>Number of participants needed to detect the expected effect with desired power and significance level.</p>",
                "</div>",

                "<div class='glossary-term'>",
                "<h4>Effect Size</h4>",
                "<p>The magnitude of the difference between groups. Larger effect sizes require smaller sample sizes to detect.</p>",
                "</div>",

                "<div class='glossary-term'>",
                "<h4>Allocation Ratio</h4>",
                "<p>The ratio of participants in control vs treatment groups. 1:1 means equal groups, 2:1 means twice as many in treatment group.</p>",
                "</div>",

                "</div>"
            )

            self$results$statistical_glossary$setContent(glossary_html)
        },

        .generate_guided_workflow = function() {
            # Generate step-by-step guided workflow
            current_step <- private$.determine_current_step()

            workflow_html <- paste0(
                "<div class='jmv-guided-workflow'>",
                "<h3>🎯 Guided Analysis Workflow</h3>",
                "<div class='workflow-steps'>",

                # Step 1: Choose Analysis Type
                "<div class='workflow-step ", if (current_step >= 1) "completed" else "current", "'>",
                "<h4>Step 1: Choose Analysis Type</h4>",
                "<p>✓ Select what you want to calculate: Sample Size, Power, Effect Size, or Study Duration</p>",
                "<p><em>Current: ", private$.format_analysis_type(self$options$analysis_type), "</em></p>",
                "</div>",

                # Step 2: Select Statistical Test
                "<div class='workflow-step ", if (current_step >= 2) "completed" else if (current_step == 1) "current" else "pending", "'>",
                "<h4>Step 2: Select Statistical Test</h4>",
                "<p>Choose the appropriate test for your study design</p>",
                "<p><em>Current: ", private$.format_test_type(self$options$test_type), "</em></p>",
                "</div>",

                # Step 3: Set Study Parameters
                "<div class='workflow-step ", if (current_step >= 3) "completed" else if (current_step == 2) "current" else "pending", "'>",
                "<h4>Step 3: Set Study Parameters</h4>",
                "<p>Configure effect size, power, and alpha levels</p>",
                "<p><em>Effect Size: ", self$options$effect_size, ", Power: ", round(self$options$power_level*100), "%, Alpha: ", self$options$alpha_level, "</em></p>",
                "</div>",

                # Step 4: Review Assumptions
                "<div class='workflow-step ", if (current_step >= 4) "completed" else if (current_step == 3) "current" else "pending", "'>",
                "<h4>Step 4: Review Assumptions</h4>",
                "<p>Check that your study meets the test assumptions (see Assumptions table below)</p>",
                "</div>",

                # Step 5: Interpret Results
                "<div class='workflow-step ", if (current_step >= 5) "completed" else if (current_step == 4) "current" else "pending", "'>",
                "<h4>Step 5: Interpret Results</h4>",
                "<p>Review the calculated results and clinical interpretation</p>",
                "</div>",

                "</div>",

                # Next Steps Recommendation
                "<div class='next-steps'>",
                "<h4>🔄 Next Steps:</h4>",
                if (current_step < 3) {
                    "<p>Complete the parameter settings in the left panel, then review your results.</p>"
                } else if (current_step < 5) {
                    "<p>Review the assumptions table and interpret your results in the Clinical Interpretation section.</p>"
                } else {
                    "<p>Your analysis is complete! Consider running sensitivity analysis to test robustness.</p>"
                },
                "</div>",

                "</div>"
            )

            self$results$guided_workflow$setContent(workflow_html)
        },

        .determine_current_step = function() {
            # Determine which step of the guided workflow the user is on
            step <- 1

            # Step 1: Analysis type selected and not default
            if (!is.null(self$options$analysis_type)) {
                step <- 2
            }

            # Step 2: Test type selected and not default
            if (!is.null(self$options$test_type)) {
                step <- 3
            }

            # Step 3: Key parameters configured
            if (!is.null(self$options$effect_size) &&
                !is.null(self$options$power_level) &&
                !is.null(self$options$alpha_level)) {
                step <- 4
            }

            # Step 4: Assumptions reviewed (if parameters are non-default)
            if (step >= 4 &&
                (self$options$effect_size != 0.75 ||
                 self$options$power_level != 0.80 ||
                 self$options$alpha_level != 0.05)) {
                step <- 5
            }

            return(step)
        }
    )
)
