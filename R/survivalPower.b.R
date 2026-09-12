#' @importFrom gsDesign gsSurv sfLDOF sfLDPocock
#' @importFrom survival survdiff
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_point geom_rect labs scale_y_continuous scale_color_manual theme element_text element_blank
#' @importFrom scales percent_format
#' @importFrom withr local_seed
# Survival Power Analysis Module for Jamovi

survivalPowerClass <- R6::R6Class(
    "survivalPowerClass",
    inherit = survivalPowerBase,
    private = list(
        effect_hr_info = NULL,
        calculated_sample_size = NULL,
        # Monte Carlo result for the current run. Two callers ask for it
        # (.populate_simulation_comparison and .populate_assumptions), and an
        # unseeded 10,000-run resimulation would both double the wait and report
        # two different empirical powers in the same output.
        simulation_cache = NULL,
        primary_result_cache = NULL,
        # Numbers behind the primary result (n, events, power, hr_detectable,
        # duration). Consumers read these instead of regex-parsing the display
        # string, which already broke once (a space before '%' silently dropped
        # the Power Interpretation) and would break on every translated string.
        primary_numbers = NULL,
        # rowKey -> label for tables whose rows depend only on options. .init()
        # creates these rows; .run() fills them with setRow() under the same keys.
        fixed_rows = list(
            sample_size_results = c(
                hr = "Effect Size (HR)", power = "Power", alpha = "Alpha Level",
                ratio = "Allocation Ratio", accrual = "Accrual Period", follow_up = "Follow-up Period"
            ),
            power_results = c(
                n = "Sample Size", power = "Calculated Power", events = "Expected Events",
                hr = "Effect Size", alpha = "Significance Level"
            ),
            effect_size_results = c(
                hr = "Minimum Detectable HR", n = "Sample Size", power = "Power",
                reduction = "Hazard Reduction", events = "Expected Events"
            ),
            study_duration_results = c(
                total = "Required Duration", accrual = "Accrual Period", follow_up = "Minimum Follow-up",
                events = "Required Events", half = "50% Events Time"
            ),
            non_inferiority_table = c(
                margin = "Non-inferiority Margin", n = "Sample Size Requirement",
                alpha = "One-sided Alpha", hr = "True Hazard Ratio"
            ),
            sensitivity_analysis_table = c(
                hr = "Hazard Ratio", median = "Control Median Survival",
                alpha = "Significance Level", accrual = "Accrual Period"
            )
        ),

        # Notice collection (single Preformatted plain-text output item; avoids the
        # jmvcore::Notice serialization error from self$results$insert(999, Notice)).
        .noticeList = list(),
        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content
            )
            private$.renderNotices()
        },
        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR = "ERROR: ",
                    STRONG_WARNING = "WARNING: ",
                    WARNING = "WARNING: ",
                    ""
                )
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))
            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },
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

            private$.init_table_rows()
        },
        # Row structure for every table whose rows depend only on options. jamovi
        # renders .init() before .run(), so rows added in .run() made each table
        # appear empty and then restructure on every change. Every key .run()
        # passes to setRow() must be created here on the same option path, or
        # setRow() aborts the analysis.
        .init_table_rows = function() {
            opts <- self$options
            add_rows <- function(table, keys, label_col, labels) {
                for (i in seq_along(keys)) {
                    values <- list()
                    values[[label_col]] <- labels[[i]]
                    table$addRow(rowKey = keys[[i]], values = values)
                }
            }
            fixed <- function(item, active) {
                if (isTRUE(active)) {
                    rows <- private$fixed_rows[[item]]
                    add_rows(self$results$get(item), names(rows), "parameter", unname(rows))
                }
            }
            fixed("sample_size_results", opts$analysis_type == "sample_size")
            fixed("power_results", opts$analysis_type == "power")
            fixed("effect_size_results", opts$analysis_type == "effect_size")
            fixed("study_duration_results", opts$analysis_type == "duration")
            fixed("non_inferiority_table", opts$test_type == "non_inferiority")
            fixed("sensitivity_analysis_table", opts$sensitivity_analysis)

            if (isTRUE(opts$run_simulation_validation) && private$.simulation_applicable()) {
                add_rows(
                    self$results$simulation_validation_table,
                    c("power", "events"),
                    "metric",
                    c("Statistical Power", "Expected Events")
                )
            }

            assumption_labels <- c(
                distribution = "Survival Distribution", ph = "Proportional Hazards",
                dropout = "Dropout Rate", accrual = "Accrual Pattern", simulation = "Simulation Validation"
            )
            keys <- private$.assumption_row_keys()
            add_rows(self$results$assumptions_table, keys, "assumption", unname(assumption_labels[keys]))

            regulatory_labels <- c(
                sample_size = "Sample Size Justification", alpha = "Significance Level",
                power = "Statistical Power", effect = "Effect Size",
                multiplicity = "Multiple Comparisons", interim = "Interim Analyses"
            )
            keys <- private$.regulatory_row_keys()
            add_rows(self$results$regulatory_table, keys, "regulatory_aspect", unname(regulatory_labels[keys]))

            if (isTRUE(opts$interim_analyses > 0) &&
                isTRUE(opts$alpha_spending %in% c("obrien_fleming", "pocock"))) {
                keys <- as.character(seq_len(opts$interim_analyses))
                add_rows(self$results$interim_analysis_table, keys, "analysis_number", as.list(seq_along(keys)))
            }
            if (isTRUE(opts$study_design == "multi_arm") &&
                isTRUE(opts$analysis_type %in% c("sample_size", "power"))) {
                keys <- as.character(seq_len(opts$number_of_arms - 1))
                add_rows(self$results$multi_arm_table, keys, "comparison", paste("Control vs Treatment", seq_along(keys)))
            }
        },
        .assumption_row_keys = function() {
            keys <- c("distribution", "ph", "dropout", "accrual")
            # Filled only when the Monte Carlo run is both requested and applicable.
            if (isTRUE(self$options$sensitivity_analysis) &&
                isTRUE(self$options$run_simulation_validation) &&
                private$.simulation_applicable()) {
                keys <- c(keys, "simulation")
            }
            keys
        },
        .regulatory_row_keys = function() {
            c(
                if (isTRUE(self$options$analysis_type == "sample_size")) "sample_size",
                "alpha", "power", "effect",
                if (isTRUE(self$options$study_design == "multi_arm")) "multiplicity",
                if (isTRUE(self$options$interim_analyses > 0)) "interim"
            )
        },
        .run = function() {
            private$.noticeList <- list()
            private$.renderNotices()
            private$simulation_cache <- NULL
            private$primary_result_cache <- NULL
            private$primary_numbers <- list()
            private$calculated_sample_size <- NULL

            # Html restores its content even when clearWith marks it unfilled.
            # Clear requested narratives before validation can return early.
            html_outputs <- c(
                show_interpretation = "clinical_interpretation",
                show_summary = "natural_language_summary",
                show_explanations = "educational_explanations",
                show_glossary = "statistical_glossary",
                guided_mode = "guided_workflow"
            )
            for (option in names(html_outputs)) {
                if (isTRUE(self$options[[option]])) {
                    self$results$get(html_outputs[[option]])$setContent("")
                }
            }

            # Main analysis runner
            if (is.null(self$options$analysis_type) ||
                is.null(self$options$test_type)) {
                return()
            }
            private$.update_instructions()

            # Validate inputs (notices are added via private$.addNotice)
            validation_result <- private$.validate_inputs()

            # Stop if validation failed
            if (!validation_result$valid) {
                return()
            }

            # Check required packages
            if (!private$.check_required_packages()) {
                return()
            }

            # Perform analysis
            private$.populate_power_summary()
            result_key <- switch(self$options$analysis_type,
                sample_size = "n", power = "power", effect_size = "hr_detectable",
                duration = "duration"
            )
            value <- private$primary_numbers[[result_key]]
            if (length(value) != 1L || !is.finite(value)) {
                private$.addNotice(
                    "ERROR", "Calculation Not Available", private$.calculate_primary_result()
                )
                return()
            }
            private$.perform_power_analysis()
            private$.populate_simulation_comparison() # New: simulation validation
            private$.populate_assumptions()
            private$.populate_regulatory_considerations()
            private$.create_visualizations()
            if (isTRUE(self$options$show_interpretation)) {
                private$.generate_interpretation()
            }
            private$.generate_clinical_friendly_outputs()

            # Add completion notice at bottom
            private$.addNotice("INFO", "Analysis Complete", sprintf(
                "Power analysis completed: %s with %s endpoint \u{2022} Assumes exponential survival and uniform accrual",
                private$.format_test_type(self$options$test_type),
                tolower(private$.format_primary_endpoint(self$options$primary_endpoint))
            ))

            # Refresh instructions
            private$.update_instructions()
        },
        .validate_inputs = function() {
            # Comprehensive input validation - returns list(valid)
            # Notices are emitted via private$.addNotice (plain-text Preformatted output)
            valid <- TRUE

            # Validate effect size (hazard ratio)
            hr <- private$.get_effect_hr()
            if (!is.null(hr)) {
                if (!is.finite(hr)) {
                    private$.addNotice(
                        "ERROR", "Invalid Effect Size",
                        "Unable to derive a valid hazard ratio from the selected effect size inputs \u{2022} Check effect size type and related parameters"
                    )
                    valid <- FALSE
                } else if (hr <= 0 || hr > 5) {
                    private$.addNotice("ERROR", "Invalid Hazard Ratio", sprintf(
                        "Hazard ratio must be between 0 and 5 (current: %.2f) \u{2022} Common values: 0.5-0.8 (treatment benefit), 1.2-2.0 (increased risk)",
                        hr
                    ))
                    valid <- FALSE
                } else if (hr < 0.3 || hr > 3) {
                    private$.addNotice("STRONG_WARNING", "Extreme Hazard Ratio", sprintf(
                        "Extreme hazard ratio detected (%.2f) \u{2022} Most trials detect HR between 0.5-2.0 \u{2022} Verify this effect size is clinically plausible",
                        hr
                    ))
                }
            }

            # A survival difference has to be read at some time point; it is read at
            # the Additional Follow-up value, so changing follow-up changes the effect.
            if (isTRUE(self$options$effect_size_type == "survival_difference") && is.finite(hr)) {
                private$.addNotice("INFO", "Survival Difference Landmark", sprintf(
                    "The survival difference is read at %s months after entry (the Additional Follow-up value), giving HR %.3f \u{2022} Set Additional Follow-up to the time point the difference refers to",
                    format(self$options$follow_up_period), hr
                ))
            }

            # Validate power level
            power <- self$options$power_level
            if (!is.null(power)) {
                if (power <= 0 || power >= 1) {
                    private$.addNotice("ERROR", "Invalid Power", sprintf(
                        "Power must be between 0 and 1 (current: %.2f) \u{2022} Standard values: 0.80 (80%%) or 0.90 (90%%)",
                        power
                    ))
                    valid <- FALSE
                } else if (power < 0.7) {
                    private$.addNotice("WARNING", "Low Power", sprintf(
                        "Power below 70%% (current: %.0f%%) may result in underpowered study \u{2022} Consider increasing to 80%% or 90%%",
                        power * 100
                    ))
                }
            }

            # Validate alpha level
            alpha <- self$options$alpha_level
            if (!is.null(alpha)) {
                if (alpha <= 0 || alpha >= 1) {
                    private$.addNotice("ERROR", "Invalid Alpha", sprintf(
                        "Alpha level must be between 0 and 1 (current: %.3f) \u{2022} Standard value: 0.05 (5%%)",
                        alpha
                    ))
                    valid <- FALSE
                } else if (alpha > 0.1) {
                    private$.addNotice("WARNING", "High Alpha", sprintf(
                        "Alpha level above 0.10 (current: %.3f) is unusual for confirmatory trials",
                        alpha
                    ))
                }
            }

            # Validate median survival
            median_survival <- self$options$control_median_survival
            if (!is.null(median_survival)) {
                if (median_survival <= 0) {
                    private$.addNotice("ERROR", "Invalid Median Survival", "Median survival must be positive (in months)")
                    valid <- FALSE
                } else if (median_survival > 240) {
                    private$.addNotice("INFO", "Long Median Survival", sprintf(
                        "Median survival > 20 years (current: %.0f months = %.1f years) may require very long follow-up",
                        median_survival, median_survival / 12
                    ))
                }
            }

            # Validate accrual pattern. Only uniform entry is modelled: the event
            # probability integral in .event_probability() assumes it. Other
            # patterns used to raise an R warning -- invisible in jamovi -- and
            # proceed as uniform, while the assumptions table printed the chosen
            # pattern as though it had been applied. Refuse, as the distribution
            # gate below does, rather than return a number the formula cannot
            # justify.
            accrual_pattern <- self$options$accrual_pattern
            if (!is.null(accrual_pattern) && accrual_pattern != "uniform") {
                private$.addNotice(
                    "ERROR", "Accrual Pattern Not Supported",
                    'Only uniform accrual is modelled in this release \u{2022} Select "Uniform Accrual" to proceed'
                )
                valid <- FALSE
            }

            # Validate survival distribution
            distribution <- self$options$survival_distribution
            if (!is.null(distribution) && distribution != "exponential") {
                private$.addNotice(
                    "ERROR", "Distribution Not Supported",
                    'Only exponential survival distribution is validated in this release \u{2022} Select "Exponential" to proceed'
                )
                valid <- FALSE
            }

            # Validate allocation ratio
            ratio <- self$options$allocation_ratio
            if (!is.null(ratio)) {
                if (ratio <= 0 || ratio > 10) {
                    private$.addNotice("ERROR", "Invalid Allocation Ratio", sprintf(
                        "Allocation ratio must be positive and typically between 0.5 and 3 (current: %.2f)",
                        ratio
                    ))
                    valid <- FALSE
                }
            }

            if (!is.null(ratio) && is.finite(ratio) && ratio > 0 && abs(log(ratio)) > log(1.2)) {
                private$.addNotice(
                    "INFO", "Unequal Allocation Approximation",
                    "With unequal allocation the Schoenfeld approximation used here (as in gsDesign) can overstate power by a few percentage points, about 2 points at 2:1 and HR 0.75 against simulation \u{2022} Confirm with Simulation validation"
                )
            }

            # Validate dropout rate
            dropout <- self$options$dropout_rate
            if (!is.null(dropout)) {
                if (dropout < 0 || dropout > 1) {
                    private$.addNotice("ERROR", "Invalid Dropout Rate", sprintf("Dropout rate must be between 0 and 1 (current: %.2f)", dropout))
                    valid <- FALSE
                } else if (dropout > 0.3) {
                    private$.addNotice("STRONG_WARNING", "High Dropout Rate", sprintf(
                        "Dropout rate > 30%% (current: %.0f%%) may significantly impact study power \u{2022} Review retention strategies",
                        dropout * 100
                    ))
                }
            }

            # Validate parameter combinations (emits additional notices)
            private$.validate_parameter_combinations()

            # Multiplicity handling
            if (!isTRUE(self$options$multiple_comparisons %in% c("none", "bonferroni", "holm", "dunnett"))) {
                private$.addNotice("ERROR", "Unsupported Multiplicity Adjustment", sprintf(
                    'Multiplicity adjustment "%s" not yet supported \u{2022} Please select "none", "bonferroni", "holm", or "dunnett"',
                    self$options$multiple_comparisons
                ))
                valid <- FALSE
            }

            if (isTRUE(self$options$study_design == "multi_arm") &&
                isTRUE(self$options$multiple_comparisons %in% c("holm", "dunnett"))) {
                private$.addNotice(
                    "WARNING", "Multiplicity Approximation",
                    "Holm/Dunnett adjustments are approximated conservatively using Bonferroni in current calculations"
                )
            }

            # Non-inferiority margin type validation
            if (isTRUE(self$options$test_type == "non_inferiority") &&
                !isTRUE(self$options$ni_type %in% c("relative_margin"))) {
                private$.addNotice(
                    "ERROR", "Non-inferiority Margin Type Not Supported",
                    "Non-inferiority calculations currently support only relative hazard-ratio margins"
                )
                valid <- FALSE
            }

            if (isTRUE(self$options$test_type == "non_inferiority")) {
                # An assumed effect at or beyond the margin makes the design
                # infeasible at any sample size. Previously this only raised an R
                # warning and the sign of the log effect was flipped, so the
                # analysis returned a confident sample size for a trial that can
                # never succeed.
                ni_hr <- private$.get_effect_hr()
                ni_margin <- self$options$ni_margin
                if (is.finite(ni_hr) && !is.null(ni_margin) && ni_hr >= ni_margin) {
                    private$.addNotice("ERROR", "Effect Not Below Non-inferiority Margin", sprintf(
                        "Assumed hazard ratio (%.3f) is not below the non-inferiority margin (%.3f) \u{2022} No sample size can demonstrate non-inferiority \u{2022} Lower the assumed HR or raise the margin",
                        ni_hr, ni_margin
                    ))
                    valid <- FALSE
                }

                # alpha_level is entered and displayed as two-sided, but the
                # non-inferiority test is one-sided by construction.
                private$.addNotice("INFO", "One-sided Alpha for Non-inferiority", sprintf(
                    "Non-inferiority is tested one-sided at alpha = %.3f \u{2022} Regulatory submissions conventionally use one-sided 0.025 (enter 0.025 here)",
                    private$.one_sided_alpha()
                ))
            }

            # Study designs with no calculation branch of their own
            if (isTRUE(self$options$study_design == "crossover")) {
                private$.addNotice(
                    "ERROR", "Crossover Design Not Supported",
                    "The calculation assumes independent parallel groups \u{2022} Time-to-event endpoints rarely suit crossover because an event ends follow-up \u{2022} Select a parallel design"
                )
                valid <- FALSE
            }
            if (isTRUE(self$options$study_design == "stratified")) {
                private$.addNotice(
                    "INFO", "Stratified Design Sized as Unstratified",
                    "Stratified randomisation does not change the Schoenfeld sample size when the hazard ratio is common across strata \u{2022} The sample size shown is the unstratified calculation \u{2022} Analyse with a stratified log-rank test or Cox model"
                )
            }

            # Unsupported tests
            unsupported_tests <- c("competing_risks", "rmst_test", "snp_survival", "weighted_log_rank")
            if (isTRUE(self$options$test_type %in% unsupported_tests)) {
                private$.addNotice("ERROR", "Unsupported Test Type", sprintf(
                    'Test "%s" temporarily unavailable pending validation \u{2022} Choose log-rank, Cox regression, or non-inferiority',
                    self$options$test_type
                ))
                valid <- FALSE
            }

            return(list(valid = valid))
        },
        .validate_parameter_combinations = function() {
            # Check for unrealistic parameter combinations - emits notices via .addNotice
            hr <- private$.get_effect_hr()
            if (!is.finite(hr)) {
                hr <- self$options$effect_size
            }
            if (!is.finite(hr)) {
                hr <- 1
            }
            power <- self$options$power_level
            alpha <- self$options$alpha_level
            accrual <- self$options$accrual_period
            followup <- self$options$follow_up_period
            median_survival <- self$options$control_median_survival

            # Check if study duration is too short for median survival
            if (!is.null(median_survival) && !is.null(accrual) && !is.null(followup)) {
                total_duration <- accrual + followup
                if (total_duration < median_survival * 1.5) {
                    private$.addNotice("WARNING", "Short Study Duration", sprintf(
                        "Study duration (%.1f mo) may be too short for median survival %.1f mo \u{2022} Consider extending follow-up to at least %.1f mo",
                        total_duration, median_survival, median_survival * 1.5
                    ))
                }
            }

            # Check for underpowered studies with small effect sizes
            if (!is.null(hr) && !is.null(power)) {
                if ((hr > 0.9 && hr < 1.1) && power > 0.8) {
                    private$.addNotice("WARNING", "Small Effect Size", sprintf(
                        "Detecting very small effect (HR=%.2f near 1.0) with %.0f%% power requires very large sample \u{2022} Consider clinical meaningfulness",
                        hr, power * 100
                    ))
                }
            }

            # Check for overly optimistic combinations
            if (!is.null(hr) && !is.null(power) && !is.null(alpha)) {
                if (hr < 0.6 && power > 0.9 && alpha < 0.05) {
                    private$.addNotice("STRONG_WARNING", "Optimistic Assumptions", sprintf(
                        "Assumes large effect (HR=%.2f) with high power (%.0f%%) \u{2022} Ensure assumptions justified by prior data",
                        hr, power * 100
                    ))
                }
            }

            # Check allocation ratio efficiency
            ratio <- self$options$allocation_ratio
            if (!is.null(ratio)) {
                if (ratio < 0.5 || ratio > 2) {
                    efficiency_loss <- (1 + ratio)^2 / (4 * ratio)
                    private$.addNotice("WARNING", "Allocation Ratio Inefficiency", sprintf(
                        "Allocation ratio %.2f:1 reduces efficiency by %.1f%% vs 1:1 randomization \u{2022} Consider balancing if feasible",
                        ratio, (efficiency_loss - 1) * 100
                    ))
                }
            }

            invisible(NULL)
        },
        .check_required_packages = function() {
            # Check for required packages and insert Notice if missing
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
                private$.addNotice("ERROR", "Missing Packages", sprintf(
                    'Missing required R package: %s \u{2022} Install with: install.packages("%s")',
                    paste(missing_packages, collapse = ", "),
                    paste(missing_packages, collapse = '", "')
                ))
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
                "<div style='background-color: rgba(255, 202, 33, 0.23); border: 2px solid #ff9800; border-radius: 5px; padding: 15px; margin-bottom: 15px; color: inherit;'>",
                "<h4 style='color: inherit; margin-top: 0;'>Beta Version 0.4.0 - Known Limitations</h4>",
                "<p><strong>Current Implementation Status:</strong></p>",
                "<ul style='margin-bottom: 10px;'>",
                "<li><strong>WORKING:</strong> Log-rank test, Cox regression, non-inferiority, multi-arm (shared control) and group-sequential designs</li>",
                "<li><strong>NOT AVAILABLE:</strong> Competing risks, RMST-based tests, SNP survival, Weighted log-rank</li>",
                "<li><strong>DISTRIBUTION:</strong> Only exponential survival distribution supported (other distributions will be blocked)</li>",
                "<li><strong>ACCRUAL:</strong> Only uniform accrual is supported (other patterns are blocked)</li>",
                "</ul>",
                "<p style='margin-bottom: 0;'><strong>Important:</strong> This is a beta version with core features functional but incomplete. ",
                "For production clinical trials, verify calculations with independent biostatistician review.</p>",
                "</div>",
                "<p><strong>Survival Power Analysis</strong></p>",
                "<p>This module provides power analysis and sample size calculations for survival studies and clinical trials.</p>",
                "<p><strong>Current Configuration:</strong><br>",
                "\u{2022} Analysis Type: ", private$.format_analysis_type(analysis_type), "<br>",
                "\u{2022} Statistical Test: ", private$.format_test_type(test_type), "</p>",
                "<p><strong>Currently validated calculations:</strong><br>",
                "\u{2022} Log-rank test (sample size, power, detectable effect, duration)<br>",
                "\u{2022} Cox regression under proportional hazards<br>",
                "\u{2022} Non-inferiority designs using hazard-ratio margins<br>",
                "\u{2022} Cluster design effects for randomized trials<br>",
                "\u{2022} Multi-arm designs sharing one control group<br>",
                "\u{2022} Group-sequential designs with O'Brien-Fleming or Pocock spending (gsDesign)</p>",
                "<p><strong>Temporarily unavailable pending validation:</strong><br>",
                "\u{2022} Competing risks (Fine-Gray)<br>",
                "\u{2022} Restricted mean survival time comparisons<br>",
                "\u{2022} SNP-based survival analyses and other specialised endpoints</p>",
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

            # Note: Assumption notes now handled by validation Notices
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
            # Four consumers ask for this in one run (summary table, specialized
            # table, clinical interpretation, report sentence). Since the gsDesign
            # power branch inverts the design with uniroot, recomputing it each
            # time multiplied the solve count for no benefit.
            if (!is.null(private$primary_result_cache)) {
                return(private$primary_result_cache)
            }

            test_type <- self$options$test_type

            tryCatch(
                {
                    if (test_type == "log_rank") {
                        result <- private$.calculate_log_rank()
                    } else if (test_type == "cox_regression") {
                        result <- private$.calculate_cox_regression()
                    } else if (test_type == "non_inferiority") {
                        result <- private$.calculate_non_inferiority()
                    } else {
                        result <- "Calculation method not implemented"
                    }

                    private$primary_result_cache <- result
                    return(result)
                },
                error = function(e) {
                    if (identical(e$code, "restart")) stop(e)
                    private$primary_result_cache <- paste("Error in calculation:", e$message)
                    return(private$primary_result_cache)
                }
            )
        },
        .calculate_log_rank = function() {
            # Log-rank test power calculation

            # Extract and prepare parameters
            params <- private$.extract_log_rank_parameters()


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

            # Multiplicity, design multipliers and sequential inflation, shared by
            # every log-rank branch and every caller of this function. Sample size
            # multiplies by design_factor; power, detectable effect and duration
            # divide the available n by it. Those three used to ignore it, so a
            # 3-arm trial sized at 1059 for 80 percent reported 93.8 percent power.
            params$alpha_adjusted <- private$.adjust_alpha_for_multiplicity(params$alpha)
            params$sample_adj <- private$.adjust_sample_for_design()
            params$design_factor <- params$sample_adj$design_effect * params$sample_adj$arm_factor
            params$seq_inflation <- private$.sequential_inflation(params$power)

            return(params)
        },
        .calculate_log_rank_sample_size = function(params) {
            events_needed <- private$.events_needed_log_rank(
                hr = params$hr,
                alpha = params$alpha_adjusted,
                power = params$power,
                ratio = params$allocation_ratio
            )
            # Interim looks need more information than the fixed design;
            # previously only the Cox path accounted for them.
            events_needed <- ceiling(events_needed * params$seq_inflation)

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
            private$calculated_sample_size <- n_final
            private$primary_numbers$n <- n_final
            private$primary_numbers$events <- events_needed

            adjustments <- private$.build_adjustment_string(params)
            return(paste("Total Sample Size:", n_final, "subjects", adjustments))
        },
        .apply_sample_size_adjustments = function(n_base, params) {
            n_adjusted <- ceiling(
                n_base * params$sample_adj$design_effect *
                    params$sample_adj$arm_factor
            )
            n_final <- ceiling(private$.adjust_sample_for_accrual(n_adjusted))
            return(n_final)
        },
        .build_adjustment_string = function(params) {
            # Build string describing adjustments applied
            adjustments <- ""
            if (params$sample_adj$design_effect > 1) {
                adjustments <- paste0(
                    adjustments, ", cluster design effect: ",
                    round(params$sample_adj$design_effect, 2)
                )
            }
            if (params$sample_adj$arm_factor > 1) {
                adjustments <- paste0(
                    adjustments, ", ", self$options$number_of_arms,
                    " arms sharing one control (x", round(params$sample_adj$arm_factor, 2), ")"
                )
            }
            if (isTRUE(params$seq_inflation > 1)) {
                adjustments <- paste0(
                    adjustments, ", ", self$options$interim_analyses + 1, "-look ",
                    private$.format_alpha_spending(self$options$alpha_spending),
                    " design (information x", round(params$seq_inflation, 3), ")"
                )
            }
            if (params$alpha_adjusted != params$alpha) {
                adjustments <- paste0(
                    adjustments, ", alpha adjusted for multiplicity: ",
                    round(params$alpha_adjusted, 4)
                )
            }
            return(adjustments)
        },
        .calculate_log_rank_power = function(params) {
            n_total <- params$sample_size_input / params$design_factor

            expected_events <- private$.expected_events_from_sample(
                n_total = n_total,
                lambda_control = params$lambda_control,
                hr = params$hr,
                allocation_ratio = params$allocation_ratio,
                accrual_period = params$accrual_period,
                follow_up_period = params$follow_up,
                dropout_rate = self$options$dropout_rate
            )

            power_value <- private$.sequential_power(function(f) {
                private$.power_from_events(
                    events = expected_events$total / f,
                    hr = params$hr,
                    alpha = params$alpha_adjusted,
                    allocation_ratio = params$allocation_ratio
                )
            })

            private$primary_numbers$power <- power_value
            private$primary_numbers$events <- expected_events$total

            return(sprintf(
                "Statistical Power: %.1f%% (%d expected events)",
                power_value * 100, round(expected_events$total)
            ))
        },
        .solve_log_rank_detectable_hr = function(params) {
            n_total <- params$sample_size_input / params$design_factor

            target_power <- params$power
            alpha <- params$alpha_adjusted
            direction_hr <- params$hr
            if (!is.finite(direction_hr)) {
                direction_hr <- 0.75
            }
            interval <- if (direction_hr < 1) c(0.1, 0.99) else c(1.01, 5.0)

            hr_solution <- tryCatch(
                {
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
                            events = expected_events / params$seq_inflation,
                            hr = hr_candidate,
                            alpha = alpha,
                            allocation_ratio = params$allocation_ratio
                        ) - target_power
                        power_candidate
                    }

                    f_lower <- objective(interval[1])
                    f_upper <- objective(interval[2])
                    if (!is.finite(f_lower) || !is.finite(f_upper) || (f_lower * f_upper > 0)) {
                        return(NA_real_)
                    }
                    uniroot(objective, interval = interval)$root
                },
                error = function(e) {
                    NA_real_
                }
            )

            hr_solution
        },
        .calculate_log_rank_effect_size = function(params) {
            hr_solution <- private$.solve_log_rank_detectable_hr(params)
            if (is.na(hr_solution)) {
                return("Unable to determine detectable hazard ratio with current settings")
            }
            private$primary_numbers$hr_detectable <- hr_solution
            return(paste("Minimum Detectable HR:", round(hr_solution, 3)))
        },
        .calculate_log_rank_duration = function(params) {
            n_total <- params$sample_size_input / params$design_factor
            required_events <- private$.events_needed_log_rank(
                hr = params$hr,
                alpha = params$alpha_adjusted,
                power = params$power,
                ratio = params$allocation_ratio
            )
            required_events <- ceiling(required_events * params$seq_inflation)
            private$primary_numbers$events <- required_events

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
                private$primary_numbers$duration <- total_duration
                return(paste("Required Study Duration:", round(total_duration, 1), "months"))
            }

            upper_bound <- max(3, params$follow_up * 2, (1 / lambda_control) * 10)
            duration_follow_up <- tryCatch(
                {
                    diff_upper <- event_target(upper_bound)
                    iter <- 0
                    while (diff_upper < 0 && iter < 10) {
                        upper_bound <- upper_bound * 2
                        diff_upper <- event_target(upper_bound)
                        iter <- iter + 1
                    }
                    uniroot(event_target, lower = lower_bound, upper = upper_bound)$root
                },
                error = function(e) {
                    NA_real_
                }
            )

            if (is.na(duration_follow_up)) {
                # Almost always because the events simply cannot be reached: with
                # dropout the per-subject event probability tops out below 1, so a
                # fixed n has a hard ceiling on events no amount of follow-up beats.
                ceiling_events <- n_total * private$.overall_event_probability(
                    lambda_control = lambda_control, hr = hr,
                    allocation_ratio = ratio, accrual_period = accrual,
                    follow_up_period = 1e6, dropout_rate = dropout_rate
                )$overall

                if (is.finite(ceiling_events) && ceiling_events < required_events) {
                    return(sprintf(
                        "No duration achieves %.0f events: %d subjects yield at most %.0f events at %.0f%% annual dropout. Increase the sample size or reduce dropout.",
                        required_events * params$design_factor, params$sample_size_input,
                        ceiling_events * params$design_factor, dropout_rate * 100
                    ))
                }
                return("Unable to determine study duration with current settings")
            }

            total_duration <- accrual + duration_follow_up
            private$primary_numbers$duration <- total_duration
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

            z_alpha <- qnorm(1 - alpha / 2)
            z_beta <- qnorm(power)

            events <- ((z_alpha + z_beta)^2) / ((log(hr))^2 * information_fraction)
            ceiling(events)
        },
        .calculate_cox_regression = function() {
            # Cox regression power calculation using gsDesign
            analysis_type <- self$options$analysis_type
            power <- self$options$power_level
            hr <- private$.get_effect_hr()
            accrual_period <- self$options$accrual_period
            follow_up <- self$options$follow_up_period
            median_control <- self$options$control_median_survival
            allocation_ratio <- self$options$allocation_ratio

            # Cox previously read alpha_level raw and applied no design factors,
            # so a multi-arm or cluster-randomised Cox design silently ignored the
            # multiplicity adjustment and the design effect that the log-rank path
            # applies to the same inputs.
            alpha <- private$.adjust_alpha_for_multiplicity(self$options$alpha_level)
            sample_adj <- private$.adjust_sample_for_design()
            n_scale <- sample_adj$design_effect * sample_adj$arm_factor

            # Convert median to rate parameter for exponential distribution
            lambda_control <- log(2) / median_control

            tryCatch(
                {
                    if (analysis_type == "sample_size") {
                        design <- private$.gs_design(beta = 1 - power)
                        if (!is.null(design)) {
                            n_total <- ceiling(private$.gs_total_n(design) * n_scale)
                            events_needed <- ceiling(max(design$n.I))
                            private$calculated_sample_size <- n_total
                            private$primary_numbers$n <- n_total
                            private$primary_numbers$events <- events_needed

                            looks <- nrow(design$eNC)
                            provenance <- if (looks > 1) {
                                sprintf(
                                    ") [gsDesign group-sequential, %d looks, %s boundaries]",
                                    looks,
                                    private$.format_alpha_spending(self$options$alpha_spending)
                                )
                            } else {
                                ") [Calculated via gsDesign]"
                            }

                            return(paste0(
                                "Total Sample Size: ", n_total, " subjects (",
                                events_needed, " events needed", provenance
                            ))
                        }

                        # Fallback / Standard calculation
                        events_needed <- private$.events_needed_log_rank(hr, alpha, power, allocation_ratio)
                        events_needed <- ceiling(events_needed * private$.sequential_inflation(power))

                        event_probs <- private$.overall_event_probability(
                            lambda_control = lambda_control,
                            hr = hr,
                            allocation_ratio = allocation_ratio,
                            accrual_period = accrual_period,
                            follow_up_period = follow_up,
                            dropout_rate = self$options$dropout_rate
                        )

                        n_total <- ceiling(events_needed / event_probs$overall * n_scale)
                        private$calculated_sample_size <- n_total
                        private$primary_numbers$n <- n_total
                        private$primary_numbers$events <- events_needed

                        return(paste("Total Sample Size:", n_total, "subjects (", events_needed, "events needed)"))
                    } else if (analysis_type == "power") {
                        # Calculate power for Cox regression
                        n_total <- self$options$sample_size_input

                        # gsSurv designs to a target power rather than reporting the
                        # power of a given n, so invert it: find the beta whose
                        # design enrols exactly n_total. Costs ~10 gsSurv calls
                        # (~0.1 s) and is the only way to credit the sequential
                        # boundaries when interim analyses are planned.
                        power_seq <- tryCatch(
                            {
                                target_n <- function(beta) {
                                    private$.gs_total_n(private$.gs_design(beta)) * n_scale - n_total
                                }
                                lo <- target_n(0.01)
                                hi <- target_n(0.60)
                                if (!is.finite(lo) || !is.finite(hi) || lo * hi > 0) {
                                    NA_real_
                                } else {
                                    1 - uniroot(target_n, c(0.01, 0.60))$root
                                }
                            },
                            error = function(e) NA_real_
                        )

                        if (is.finite(power_seq)) {
                            design <- private$.gs_design(1 - power_seq)
                            private$primary_numbers$power <- power_seq
                            private$primary_numbers$events <- max(design$n.I)
                            looks <- nrow(design$eNC)
                            provenance <- if (looks > 1) {
                                sprintf(
                                    " [gsDesign group-sequential, %d looks, %s boundaries]",
                                    looks,
                                    private$.format_alpha_spending(self$options$alpha_spending)
                                )
                            } else {
                                " [Calculated via gsDesign]"
                            }
                            return(paste0(
                                "Statistical Power: ", round(power_seq * 100, 1), "% (",
                                ceiling(max(design$n.I)), " events expected)", provenance
                            ))
                        }

                        # Fallback / Standard calculation

                        expected_events <- private$.expected_events_from_sample(
                            n_total = n_total / n_scale,
                            lambda_control = lambda_control,
                            hr = hr,
                            allocation_ratio = allocation_ratio,
                            accrual_period = accrual_period,
                            follow_up_period = follow_up,
                            dropout_rate = self$options$dropout_rate
                        )$total

                        power_calc <- private$.sequential_power(function(f) {
                            private$.power_from_events(
                                events = expected_events / f,
                                hr = hr,
                                alpha = alpha,
                                allocation_ratio = allocation_ratio
                            )
                        })

                        private$primary_numbers$power <- power_calc
                        private$primary_numbers$events <- expected_events

                        return(sprintf(
                            "Statistical Power: %.1f%% (%d events expected)",
                            power_calc * 100, round(expected_events)
                        ))
                    } else if (analysis_type == "effect_size") {
                        # Identical arithmetic to the log-rank solver, which also
                        # picks the bracket from the direction of the assumed
                        # effect. The duplicate here hardcoded c(0.1, 1.5) and
                        # never checked the sign at the endpoints, so ordinary
                        # inputs (n = 400, default design) returned "unable to
                        # determine" where log-rank returned HR 0.705.
                        params <- private$.extract_log_rank_parameters()
                        return(private$.calculate_log_rank_effect_size(params))
                    } else if (analysis_type == "duration") {
                        # Calculate required study duration for Cox regression
                        n_total <- self$options$sample_size_input / n_scale

                        # Required events for target power, with any interim looks
                        events_needed <- ceiling(
                            private$.events_needed_log_rank(hr, alpha, power, allocation_ratio) *
                                private$.sequential_inflation(power)
                        )
                        private$primary_numbers$events <- events_needed

                        # Solve the accrual-aware event-accumulation curve for the
                        # follow-up that yields events_needed, exactly as the
                        # log-rank and non-inferiority paths do. The previous
                        # closed form (lambda * (1 - dropout/12)) mixed an annual
                        # dropout proportion into a monthly hazard, used the
                        # control hazard for both arms, and ignored staggered
                        # entry -- so Cox and log-rank disagreed on identical input.
                        total_duration <- private$.solve_follow_up_duration(
                            n_total = n_total,
                            target_events = events_needed,
                            lambda_control = lambda_control,
                            hr = hr,
                            allocation_ratio = allocation_ratio,
                            accrual_period = accrual_period,
                            dropout_rate = self$options$dropout_rate,
                            initial_follow_up = follow_up
                        )

                        if (is.na(total_duration)) {
                            return("Unable to determine study duration with current settings")
                        }

                        private$primary_numbers$duration <- total_duration
                        return(paste("Required Study Duration:", round(total_duration, 1), "months"))
                    }
                },
                error = function(e) {
                    return(paste("Cox regression calculation error:", e$message))
                }
            )

            return("Cox regression calculation completed")
        },
        .calculate_non_inferiority = function() {
            analysis_type <- self$options$analysis_type
            alpha <- private$.one_sided_alpha()
            power <- self$options$power_level
            hr_true <- private$.get_effect_hr()
            accrual_period <- self$options$accrual_period
            follow_up <- self$options$follow_up_period
            median_control <- self$options$control_median_survival
            allocation_ratio <- self$options$allocation_ratio
            ni_margin <- self$options$ni_margin
            ni_type <- self$options$ni_type

            if (is.null(hr_true) || !is.finite(hr_true) || hr_true <= 0) {
                return("Hazard ratio must be specified and positive for non-inferiority calculations")
            }

            if (!isTRUE(ni_type %in% c("relative_margin"))) {
                return("Only relative hazard-ratio margins are supported in the validated implementation")
            }

            hr_margin <- ni_margin
            if (hr_margin <= 0) {
                return("Non-inferiority margin must be specified on the hazard-ratio scale")
            }

            # Defence in depth: .validate_inputs() stops the run before this point.
            if (hr_true >= hr_margin) {
                return(paste0(
                    "No design can demonstrate non-inferiority: assumed HR (",
                    round(hr_true, 3), ") is not below the margin (", hr_margin, ")"
                ))
            }

            props <- private$.allocation_props(allocation_ratio)
            info_fraction <- props$control * props$treatment
            if (info_fraction <= 0) {
                return("Allocation ratio must produce positive information fraction")
            }

            z_alpha <- qnorm(1 - alpha) # one-sided
            z_beta <- qnorm(power)
            log_effect <- log(hr_true) - log(hr_margin)

            events_needed <- ceiling(((z_alpha + z_beta)^2) / ((log_effect)^2 * info_fraction))

            dist_params <- private$.get_distribution_parameters(median_control, hr_true)

            dropout_rate <- self$options$dropout_rate

            # Design multipliers and interim looks, as for superiority. Every
            # non-inferiority design was previously sized as a plain two-arm fixed
            # trial.
            sample_adj <- private$.adjust_sample_for_design()
            design_factor <- sample_adj$design_effect * sample_adj$arm_factor
            events_needed <- ceiling(events_needed * private$.sequential_inflation(power))

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
                n_total <- ceiling(n_total * design_factor)

                private$primary_numbers$n <- n_total
                private$primary_numbers$events <- events_needed
                private$calculated_sample_size <- n_total

                return(paste(
                    "Total Sample Size:", n_total,
                    "subjects (", events_needed, "events needed; NI margin HR =", hr_margin, ")"
                ))
            } else if (analysis_type == "power") {
                n_total <- self$options$sample_size_input / design_factor

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
                power_calc <- private$.sequential_power(function(f) {
                    pnorm(-z_alpha - sqrt(info / f) * (log(hr_true) - log(hr_margin)))
                })

                private$primary_numbers$power <- power_calc
                private$primary_numbers$events <- expected_events

                return(sprintf(
                    "Non-inferiority Power: %.1f%% (HR = %.3f vs margin = %s)",
                    power_calc * 100, hr_true, format(hr_margin)
                ))
            } else if (analysis_type == "effect_size") {
                n_total <- self$options$sample_size_input / design_factor

                infl <- private$.sequential_inflation(power)
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

                    info <- private$.information_from_events(events, allocation_ratio) / infl
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

                detectable_hr <- tryCatch(
                    {
                        uniroot(objective, interval = c(lower_bound, upper_bound))$root
                    },
                    error = function(e) {
                        NA_real_
                    }
                )

                if (is.na(detectable_hr)) {
                    return("Unable to determine maximum HR satisfying non-inferiority criteria with current settings")
                }

                private$primary_numbers$hr_detectable <- detectable_hr

                return(paste(
                    "Maximum HR for Non-inferiority:", round(detectable_hr, 3),
                    "(margin =", hr_margin, ")"
                ))
            } else if (analysis_type == "duration") {
                n_total <- self$options$sample_size_input / design_factor
                private$primary_numbers$events <- events_needed
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

                private$primary_numbers$duration <- duration
                return(paste("Required Study Duration:", round(duration, 1), "months (non-inferiority trial)"))
            }

            return("Non-inferiority calculation completed")
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

            # Simulation validation is handled in .populate_simulation_comparison()
        },
        .populate_simulation_comparison = function() {
            # Populate simulation validation comparison table
            if (!self$options$run_simulation_validation) {
                return()
            }

            # Run simulation analysis
            sim_results <- private$.run_simulation_analysis()

            if (is.null(sim_results)) {
                return()
            }

            # Get analytical power for comparison
            analytical_power <- if (self$options$analysis_type == "sample_size") {
                self$options$power_level
            } else if (self$options$analysis_type == "power") {
                # Calculate analytical power
                n_total <- self$options$sample_size_input
                hr <- private$.get_effect_hr()
                alpha <- self$options$alpha_level
                control_median <- self$options$control_median_survival
                lambda_control <- log(2) / control_median

                expected_events <- private$.expected_events_from_sample(
                    n_total = n_total,
                    lambda_control = lambda_control,
                    hr = hr,
                    allocation_ratio = self$options$allocation_ratio,
                    accrual_period = self$options$accrual_period,
                    follow_up_period = self$options$follow_up_period,
                    dropout_rate = self$options$dropout_rate
                )$total

                private$.power_from_events(
                    events = expected_events,
                    hr = hr,
                    alpha = alpha,
                    allocation_ratio = self$options$allocation_ratio
                )
            } else {
                return() # Only support sample_size and power analysis types
            }

            # Populate comparison table
            table <- self$results$simulation_validation_table

            # Calculate agreement status
            diff <- abs(analytical_power - sim_results$empirical_power)
            agreement <- if (diff < 0.02) {
                "Excellent (< 2%)"
            } else if (diff < 0.05) {
                "Good (< 5%)"
            } else if (diff < 0.10) {
                "Fair (< 10%)"
            } else {
                "Poor (>= 10%)"
            }

            power_values <- list(
                metric = "Statistical Power",
                analytical = analytical_power,
                simulated = sim_results$empirical_power,
                ci_lower = sim_results$ci_lower,
                ci_upper = sim_results$ci_upper,
                agreement = agreement
            )
            table$setRow(rowKey = "power", values = power_values)

            # Add expected events comparison
            # Calculate analytical expected events
            n_total <- if (self$options$analysis_type == "sample_size") {
                private$.calculate_primary_result()
                private$primary_numbers$n
            } else {
                self$options$sample_size_input
            }

            if (!is.null(n_total)) {
                hr <- private$.get_effect_hr()
                control_median <- self$options$control_median_survival
                lambda_control <- log(2) / control_median

                analytical_events <- private$.expected_events_from_sample(
                    n_total = n_total,
                    lambda_control = lambda_control,
                    hr = hr,
                    allocation_ratio = self$options$allocation_ratio,
                    accrual_period = self$options$accrual_period,
                    follow_up_period = self$options$follow_up_period,
                    dropout_rate = self$options$dropout_rate
                )$total

                # Calculate agreement for events
                events_diff <- abs(analytical_events - sim_results$avg_events)
                events_pct_diff <- events_diff / analytical_events

                events_agreement <- if (events_pct_diff < 0.05) {
                    "Excellent (< 5%)"
                } else if (events_pct_diff < 0.10) {
                    "Good (< 10%)"
                } else if (events_pct_diff < 0.15) {
                    "Fair (< 15%)"
                } else {
                    "Poor (>= 15%)"
                }

                events_values <- list(
                    metric = "Expected Events",
                    analytical = analytical_events,
                    simulated = sim_results$avg_events,
                    ci_lower = NA,
                    ci_upper = NA,
                    agreement = events_agreement
                )
                table$setRow(rowKey = "events", values = events_values)
            }

            # Add table notes with convergence diagnostics
            convergence_note <- sprintf(
                "Simulation Diagnostics:\n\u{2022} Monte Carlo SE: %.4f (target: %.4f)\n\u{2022} %d simulations completed\n\u{2022} %s\n\u{2022} Recommendation: %s",
                sim_results$convergence$mc_se,
                sim_results$convergence$target_mc_se,
                sim_results$n_sims,
                sim_results$convergence$message,
                sim_results$convergence$recommendation
            )

            table$setNote("convergence", convergence_note)

            # Add distribution note
            dist_note <- sprintf(
                "Distribution: %s | Validation Method: Monte Carlo simulation with log-rank test",
                private$.format_distribution(self$options$survival_distribution)
            )

            table$setNote("distribution", dist_note)
        },
        .format_distribution = function(dist) {
            switch(dist,
                "exponential" = "Exponential",
                "weibull" = sprintf("Weibull (shape = %.2f)", self$options$weibull_shape),
                "log_normal" = sprintf("Log-normal (sigma = %.2f)", self$options$weibull_shape),
                "piecewise_exponential" = "Piecewise Exponential",
                dist
            )
        },
        .populate_sample_size_results = function() {
            table <- self$results$sample_size_results

            # Add key parameters for sample size calculation
            parameters <- list(
                list(
                    parameter = "Effect Size (HR)", value = round(private$.get_effect_hr(), 3),
                    description = "Hazard ratio used in calculations (derived if needed)"
                ),
                list(
                    parameter = "Power", value = paste0(self$options$power_level * 100, "%"),
                    description = "Desired statistical power to detect the specified effect"
                ),
                list(
                    parameter = "Alpha Level", value = self$options$alpha_level,
                    description = if (isTRUE(self$options$test_type == "non_inferiority")) {
                        "Type I error rate (applied one-sided for non-inferiority)"
                    } else {
                        "Type I error rate (two-sided significance level)"
                    }
                ),
                list(
                    parameter = "Allocation Ratio", value = self$options$allocation_ratio,
                    description = "Ratio of control to experimental group sizes"
                ),
                list(
                    parameter = "Accrual Period", value = paste(self$options$accrual_period, "months"),
                    description = "Duration of patient recruitment period"
                ),
                list(
                    parameter = "Follow-up Period", value = paste(self$options$follow_up_period, "months"),
                    description = "Additional follow-up after recruitment ends"
                )
            )

            keys <- names(private$fixed_rows$sample_size_results)
            for (i in seq_along(parameters)) {
                table$setRow(rowKey = keys[[i]], values = parameters[[i]])
            }
        },
        .populate_power_results = function() {
            table <- self$results$power_results
            # Read the numbers the headline was computed from. This table used to
            # recompute power with the unadjusted alpha and no design multipliers,
            # so for a 3-arm trial it printed 96.6 percent beside a 93.8 percent
            # headline, under "Significance Level 5%" where 0.025 was used.
            private$.calculate_primary_result()
            n_total <- self$options$sample_size_input
            power_calc <- private$primary_numbers$power
            expected_events <- private$primary_numbers$events
            hr <- private$.get_effect_hr()
            sided <- if (isTRUE(self$options$test_type == "non_inferiority")) "one-sided" else "two-sided"

            parameters <- list(
                list(
                    parameter = "Sample Size", value = paste(n_total, "subjects"),
                    description = "Total number of subjects in the study"
                ),
                list(
                    parameter = "Calculated Power",
                    value = if (is.null(power_calc)) "Not determined" else paste0(round(power_calc * 100, 1), "%"),
                    description = "Statistical power for the given sample size"
                ),
                list(
                    parameter = "Expected Events",
                    value = if (is.null(expected_events)) "Not determined" else paste(round(expected_events), "events"),
                    description = "Events informing the treatment comparison"
                ),
                list(
                    parameter = "Effect Size", value = paste("HR =", round(hr, 3)),
                    description = "Hazard ratio representing treatment effect"
                ),
                list(
                    parameter = "Significance Level", value = paste0(private$.alpha_pct(), "% (", sided, ")"),
                    description = "Type I error rate used, after any multiplicity adjustment"
                )
            )

            keys <- names(private$fixed_rows$power_results)
            for (i in seq_along(parameters)) {
                table$setRow(rowKey = keys[[i]], values = parameters[[i]])
            }
        },
        .populate_effect_size_results = function() {
            table <- self$results$effect_size_results

            # Calculate real detectable effect size based on current options
            n_total <- self$options$sample_size_input
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

            # Third copy of the same solver, with its own hardcoded c(0.1, 1.5)
            # bracket and no sign check: this table printed "Not determined" for
            # designs where the summary above it reported a value. Use the one
            # solver that picks its bracket from the direction of the effect.
            # Read the headline's solution so this table matches it for every test
            # type and design (non-inferiority, multi-arm, interim looks).
            private$.calculate_primary_result()
            min_detectable_hr <- private$primary_numbers$hr_detectable
            if (is.null(min_detectable_hr)) min_detectable_hr <- NA_real_
            is_ni <- isTRUE(self$options$test_type == "non_inferiority")

            if (is.na(min_detectable_hr)) {
                min_detectable_hr <- NA
            }

            expected_events <- expected_events_fun(private$.get_effect_hr())

            # Calculate effect size in different metrics
            percent_reduction <- if (!is.na(min_detectable_hr)) round((1 - min_detectable_hr) * 100, 1) else NA

            parameters <- list(
                list(
                    parameter = if (is_ni) "Maximum HR for Non-inferiority" else "Minimum Detectable HR", value = if (!is.na(min_detectable_hr)) round(min_detectable_hr, 3) else "Not determined",
                    description = "Smallest hazard ratio detectable with specified power"
                ),
                list(
                    parameter = "Sample Size", value = paste(n_total, "subjects"),
                    description = "Total number of subjects in the study"
                ),
                list(
                    parameter = "Power", value = paste0(round(power * 100, 1), "%"),
                    description = "Statistical power for detecting minimum effect"
                ),
                list(
                    parameter = "Hazard Reduction", value = if (is_ni) "Not applicable" else if (!is.na(percent_reduction)) paste0(percent_reduction, "%") else "Not determined",
                    description = "Minimum relative reduction in the hazard detectable with the specified power"
                ),
                list(
                    parameter = "Expected Events", value = paste(round(expected_events), "events"),
                    description = "Number of events expected during study period"
                )
            )

            keys <- names(private$fixed_rows$effect_size_results)
            for (i in seq_along(parameters)) {
                table$setRow(rowKey = keys[[i]], values = parameters[[i]])
            }
        },
        .populate_duration_results = function() {
            table <- self$results$study_duration_results
            # Read the design the headline duration was solved for (adjusted alpha,
            # design multipliers, interim looks). This table used to re-derive the
            # events with the raw alpha, and printed the planned duration as
            # "Required Duration" when none could be found.
            private$.calculate_primary_result()
            accrual_period <- self$options$accrual_period
            total_duration <- private$primary_numbers$duration
            events_needed <- private$primary_numbers$events
            if (is.null(total_duration)) total_duration <- NA_real_
            if (is.null(events_needed)) events_needed <- NA_real_
            min_follow_up <- if (is.na(total_duration)) NA_real_ else max(total_duration - accrual_period, 0)

            sample_adj <- private$.adjust_sample_for_design()
            n_effective <- self$options$sample_size_input / (sample_adj$design_effect * sample_adj$arm_factor)
            half_time <- if (is.na(events_needed)) {
                NA_real_
            } else {
                private$.solve_follow_up_duration(
                    n_total = n_effective,
                    target_events = events_needed / 2,
                    lambda_control = log(2) / self$options$control_median_survival,
                    hr = private$.get_effect_hr(),
                    allocation_ratio = self$options$allocation_ratio,
                    accrual_period = accrual_period,
                    dropout_rate = self$options$dropout_rate,
                    initial_follow_up = self$options$follow_up_period
                )
            }

            months <- function(x) if (is.na(x)) "Not determined" else paste(round(x, 1), "months")
            parameters <- list(
                list(
                    parameter = "Required Duration", value = months(total_duration),
                    description = "Total study duration needed to achieve target power"
                ),
                list(
                    parameter = "Accrual Period", value = paste(round(accrual_period, 1), "months"),
                    description = "Patient recruitment period"
                ),
                list(
                    parameter = "Minimum Follow-up", value = months(min_follow_up),
                    description = "Additional follow-up needed after recruitment"
                ),
                list(
                    parameter = "Required Events",
                    value = if (is.na(events_needed)) "Not determined" else paste(round(events_needed), "events"),
                    description = "Events needed for target power"
                ),
                list(
                    parameter = "50% Events Time", value = months(half_time),
                    description = "Time when half of the required events have occurred"
                )
            )

            keys <- names(private$fixed_rows$study_duration_results)
            for (i in seq_along(parameters)) {
                table$setRow(rowKey = keys[[i]], values = parameters[[i]])
            }
        },
        .populate_specialized_tables = function() {
            test_type <- self$options$test_type

            if (test_type == "non_inferiority") {
                private$.populate_non_inferiority_table()
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
        .populate_non_inferiority_table = function() {
            table <- self$results$non_inferiority_table

            # Calculate actual non-inferiority parameters
            ni_margin <- self$options$ni_margin
            ni_type <- self$options$ni_type
            alpha <- private$.one_sided_alpha()
            hr <- private$.get_effect_hr()

            # Get calculated sample size if this is a sample size analysis
            sample_size_text <- if (self$options$analysis_type == "sample_size") {
                private$.calculate_primary_result()
                n <- private$primary_numbers$n
                if (is.null(n)) "Not calculated" else paste(n, "subjects")
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
                list(
                    parameter = "Non-inferiority Margin",
                    value = sprintf("%.3f", ni_margin),
                    margin_type = private$.format_ni_type(ni_type),
                    clinical_interpretation = paste("Maximum acceptable difference -", margin_interpretation)
                ),
                list(
                    parameter = "Sample Size Requirement",
                    value = sample_size_text,
                    margin_type = "Total enrollment",
                    # Derived, not asserted: the old fixed "20-50%" was wrong for
                    # this module's own default non-inferiority design, which needs
                    # 66% more events than the matching superiority trial.
                    clinical_interpretation = private$.ni_inflation_text(hr, ni_margin)
                ),
                list(
                    parameter = "One-sided Alpha",
                    value = sprintf("%.4f", alpha),
                    margin_type = "Statistical threshold",
                    clinical_interpretation = "For non-inferiority conclusion"
                ),
                list(
                    parameter = "True Hazard Ratio",
                    value = sprintf("%.3f", hr),
                    margin_type = "Expected effect",
                    clinical_interpretation = if (hr < ni_margin) "Should demonstrate non-inferiority" else "May not achieve non-inferiority"
                )
            )

            keys <- names(private$fixed_rows$non_inferiority_table)
            for (i in seq_along(ni_params)) {
                table$setRow(rowKey = keys[[i]], values = ni_params[[i]])
            }
        },
        # Events for non-inferiority scale with 1/log(HR/margin)^2; a superiority
        # trial powered on the same assumed HR scales with 1/log(HR_alt)^2. Report
        # the ratio the user's own inputs imply rather than a fixed range.
        .ni_inflation_text = function(hr, margin) {
            sup_hr <- self$options$effect_size
            if (!is.finite(hr) || !is.finite(margin) || hr >= margin) {
                return("Non-inferiority designs are larger than the matching superiority trial")
            }
            if (!is.finite(sup_hr) || sup_hr <= 0 || abs(log(sup_hr)) < 1e-8) {
                sup_hr <- 0.75
            }
            ratio <- (log(sup_hr) / (log(hr) - log(margin)))^2
            if (!is.finite(ratio) || ratio <= 0) {
                return("Non-inferiority designs are larger than the matching superiority trial")
            }
            sprintf(
                "Requires %.0f%% of the events a superiority trial at HR %.2f would need",
                ratio * 100, sup_hr
            )
        },
        .format_ni_type = function(type) {
            switch(type,
                "absolute_margin" = "Absolute Margin",
                "relative_margin" = "Relative Margin",
                "retention_fraction" = "Retention of Effect Fraction",
                type
            )
        },
        .populate_multi_arm_table = function() {
            table <- self$results$multi_arm_table
            num_arms <- self$options$number_of_arms
            ratio <- self$options$allocation_ratio
            if (is.null(ratio) || !is.finite(ratio) || ratio <= 0) ratio <- 1

            adjusted_alpha <- private$.adjust_alpha_for_multiplicity(self$options$alpha_level)

            calculated_n <- if (self$options$analysis_type == "sample_size") {
                private$.calculate_primary_result()
                n <- private$primary_numbers$n
                if (is.null(n)) NA_real_ else n
            } else if (self$options$analysis_type == "power") {
                self$options$sample_size_input
            } else {
                NA_real_
            }

            if (is.na(calculated_n) || calculated_n <= 0) {
                return()
            }

            # Split the trial back into its arms. Total enrolment is
            # n_control + (k-1) * n_exp with n_control / n_exp = ratio, so
            # n_exp = N / (ratio + k - 1). The old code divided the total by the
            # number of arms, which is only right at ratio = 1 -- and was applied
            # to a total that was itself just one pairwise comparison.
            n_exp <- calculated_n / (ratio + num_arms - 1)
            n_control <- n_exp * ratio

            hr_effect <- private$.get_effect_hr()

            # Each comparison uses the shared control plus its own arm.
            pairwise_power <- private$.basic_power_calc(n_control + n_exp, hr_effect, adjusted_alpha)
            individual_power <- round(pairwise_power * 100, 1)

            # Disjunctive power: P(at least one comparison rejects) when every
            # experimental arm truly has the assumed hazard ratio. The test
            # statistics are correlated because they share the control arm --
            # rho = n_exp / (n_control + n_exp) under equal experimental arms
            # -- so the previous 1 - (1 - p)^(k-1) (independence) and the
            # "adjustment factor" multipliers (1/(k-1), 1/sqrt(k-1), 0.9, 0.85)
            # were both fabrications rather than probabilities.
            overall_power <- private$.disjunctive_power(
                pairwise_power = pairwise_power,
                alpha = adjusted_alpha,
                n_comparisons = num_arms - 1,
                rho = n_exp / (n_control + n_exp)
            )

            comparisons <- list()
            for (i in 1:(num_arms - 1)) {
                comparisons[[i]] <- list(
                    comparison = paste("Control vs Treatment", i),
                    sample_size_per_arm = as.integer(round(n_exp)),
                    adjusted_alpha = round(adjusted_alpha, 4),
                    power = individual_power,
                    total_study_power = if (is.na(overall_power)) NA_real_ else round(overall_power * 100, 1)
                )
            }

            table$setNote(
                "multiarm",
                sprintf(
                    "Control arm: %d subjects, shared by all %d comparisons; %d per experimental arm. Power is per comparison at the adjusted alpha; total study power is the probability that at least one comparison is significant when every experimental arm truly has HR %.3f, accounting for the correlation induced by the shared control.",
                    round(n_control), num_arms - 1, round(n_exp), hr_effect
                )
            )

            for (i in seq_along(comparisons)) {
                table$setRow(rowKey = as.character(i), values = comparisons[[i]])
            }
        },
        .populate_interim_analysis_table = function() {
            table <- self$results$interim_analysis_table
            alpha_spending <- self$options$alpha_spending

            if (!isTRUE(alpha_spending %in% c("obrien_fleming", "pocock"))) {
                private$.addNotice(
                    "WARNING", "No Alpha Spending Function",
                    "Interim analyses are planned but no spending function is selected \u{2022} Boundaries and the sample-size inflation they require cannot be derived \u{2022} Select O'Brien-Fleming or Pocock"
                )
                return()
            }

            # Boundaries come from the same information-scale design whose inflation
            # sizes the study, for every test type. The earlier hand-rolled version
            # multiplied the O'Brien-Fleming spending function by alpha a second time
            # (boundary 5.14 where 2.77 is correct); a later gsSurv-based version
            # left the non-inferiority table blank.
            design <- private$.gs_info_design()
            if (is.null(design)) {
                private$.addNotice(
                    "WARNING", "Interim Boundaries Unavailable",
                    "The group-sequential design could not be derived for the current parameters \u{2022} Interim boundaries are not shown"
                )
                return()
            }

            k <- design$k
            is_ni <- isTRUE(self$options$test_type == "non_inferiority")
            # design$alpha is one-sided; report it the way alpha_level is entered.
            sides <- if (is_ni) 1 else 2
            cumulative_spend <- design$upper$sf(design$alpha, design$timing, design$upper$param)$spend * sides

            # Conditional power under the design alternative (Lan-Wittes B-value):
            # CP(t) = Phi((theta - b_final) / sqrt(1 - t)), with theta this design's
            # drift, delta * sqrt(max information) -- not the fixed-design
            # z_alpha + z_beta, which understates it.
            drift <- design$delta * sqrt(max(design$n.I))
            final_bound <- design$upper$bound[k]

            for (i in seq_len(k - 1)) {
                t_i <- design$timing[i]
                table$setRow(rowKey = as.character(i), values = list(
                    analysis_number = i,
                    timing = round(t_i * 100, 1),
                    alpha_spent = round(cumulative_spend[i], 5),
                    boundary_value = round(design$upper$bound[i], 3),
                    conditional_power = round(pnorm((drift - final_bound) / sqrt(1 - t_i)) * 100, 1)
                ))
            }

            events <- private$primary_numbers$events
            table$setNote(
                "boundaries",
                sprintf(
                    "Cumulative %s alpha spent and efficacy boundaries of a %d-look %s design (gsDesign; efficacy only, no futility bound). Final boundary: %.3f.%s Conditional power assumes the trend continues at the planned hazard ratio.",
                    if (is_ni) "one-sided" else "two-sided", k,
                    private$.format_alpha_spending(alpha_spending), final_bound,
                    if (is.null(events)) "" else sprintf(" Maximum events: %.0f.", ceiling(events))
                )
            )
        },
        .format_alpha_spending = function(spending) {
            switch(spending,
                "obrien_fleming" = "O'Brien-Fleming",
                "pocock" = "Pocock",
                "none" = "no spending function",
                spending
            )
        },
        .populate_sensitivity_analysis_table = function() {
            # Add error handling for sensitivity analysis
            tryCatch(
                {
                    table <- self$results$sensitivity_analysis_table

                    # Debug: Check if sensitivity analysis is enabled
                    if (!self$options$sensitivity_analysis) {
                        return()
                    }

                    # Validate that we have the required parameters
                    if (is.null(self$options$effect_size) || is.null(self$options$alpha_level)) {
                        private$.addNotice("WARNING", "Sensitivity Analysis Skipped", "Effect size or alpha level is missing \u{2022} Sensitivity scenarios cannot be built")
                        return()
                    }

                    # Get base case values from current options. This must be the
                    # hazard ratio the analysis actually used, not the raw effect
                    # size box: under effect_size_type "median_ratio" a value of
                    # 1.5 means HR 0.667, so perturbing 1.5 varied the wrong
                    # quantity around the wrong centre.
                    base_hr <- private$.get_effect_hr()
                    base_alpha <- self$options$alpha_level
                    base_median <- self$options$control_median_survival
                    base_accrual <- self$options$accrual_period

                    # Calculate base case result
                    base_result <- private$.calculate_primary_result()

                    # Debug: Check base result
                    if (is.null(base_result) || is.na(base_result)) {
                        private$.addNotice("WARNING", "Sensitivity Analysis Skipped", "The base-case design could not be computed \u{2022} Scenarios have nothing to be compared against")
                        return()
                    }

                    # Scenarios: hazard ratio, control median and accrual +/- 20 percent;
                    # alpha halved and doubled.
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
                        impact_assessment <- tryCatch(
                            {
                                impact_result <- private$.format_sensitivity_impact(scenario$impact_1, scenario$impact_2)
                                if (is.null(impact_result) || is.na(impact_result)) {
                                    "Impact calculation failed"
                                } else {
                                    impact_result
                                }
                            },
                            error = function(e) {
                                private$.addNotice("WARNING", "Sensitivity Scenario Failed", sprintf("A sensitivity scenario could not be evaluated: %s", conditionMessage(e)))
                                paste("Error:", e$message)
                            }
                        )

                        # Add row with validation
                        tryCatch(
                            {
                                table$setRow(rowKey = names(private$fixed_rows$sensitivity_analysis_table)[[i]], values = list(
                                    parameter = scenario$parameter,
                                    base_case = scenario$base_case,
                                    scenario_1 = scenario$scenario_1,
                                    scenario_2 = scenario$scenario_2,
                                    impact_assessment = impact_assessment
                                ))
                            },
                            error = function(e) {
                                private$.addNotice("WARNING", "Sensitivity Row Dropped", sprintf("Sensitivity row %d could not be added: %s", i, conditionMessage(e)))
                            }
                        )
                    }
                },
                error = function(e) {
                    # If sensitivity analysis fails, continue without it
                    private$.addNotice("WARNING", "Sensitivity Analysis Unavailable", sprintf("Sensitivity analysis could not be completed: %s \u{2022} The numeric results are unaffected", conditionMessage(e)))
                }
            )
        },
        # Sample size (sample-size analyses) or power (power analyses) for one
        # sensitivity scenario. The base case goes through this same function, so a
        # scenario that changes nothing reports no change. Scenarios previously used
        # a crude 0.67 average-follow-up approximation with no dropout while the
        # base came from the exact headline calculation, so the table reported a
        # "change" produced by the two formulas rather than by the parameter.
        # Design factors must also enter power scenarios: they do not cancel
        # inside the normal probability function.
        .sensitivity_value = function(hr = private$.get_effect_hr(),
                                      alpha = self$options$alpha_level,
                                      accrual = self$options$accrual_period,
                                      median = self$options$control_median_survival) {
            analysis_type <- self$options$analysis_type
            if (!isTRUE(analysis_type %in% c("sample_size", "power"))) {
                return(NA_real_)
            }
            if (!is.finite(hr) || hr <= 0 || !is.finite(median) || median <= 0) {
                return(NA_real_)
            }
            power <- self$options$power_level
            ratio <- self$options$allocation_ratio
            follow_up <- self$options$follow_up_period
            dropout <- self$options$dropout_rate
            n_input <- self$options$sample_size_input
            lambda_c <- log(2) / median
            props <- private$.allocation_props(ratio)
            info_fraction <- props$control * props$treatment
            adjustment <- private$.adjust_sample_for_design()
            design_factor <- adjustment$design_effect * adjustment$arm_factor
            inflation <- private$.sequential_inflation(power, alpha)
            alpha_adj <- private$.adjust_alpha_for_multiplicity(alpha)
            n_input <- n_input / design_factor

            if (isTRUE(self$options$test_type == "non_inferiority")) {
                margin <- self$options$ni_margin
                if (!is.finite(margin) || hr >= margin) {
                    return(NA_real_)
                }
                z_alpha <- qnorm(1 - alpha_adj)
                log_effect <- log(hr) - log(margin)
                if (analysis_type == "sample_size") {
                    events <- ceiling((z_alpha + qnorm(power))^2 / (log_effect^2 * info_fraction))
                    events <- ceiling(events * inflation)
                    n <- private$.sample_size_from_events(events, lambda_c, hr, ratio,
                                                          accrual, follow_up, dropout)
                    return(ceiling(n * design_factor))
                }
                events <- private$.expected_events_from_sample(n_input, lambda_c, hr, ratio, accrual, follow_up, dropout)$total
                return(private$.sequential_power(function(f) {
                    pnorm(-z_alpha - sqrt(private$.information_from_events(events, ratio) / f) * log_effect)
                }, alpha))
            }

            if (hr == 1) {
                return(NA_real_)
            }
            if (analysis_type == "sample_size") {
                events <- private$.events_needed_log_rank(hr, alpha_adj, power, ratio)
                events <- ceiling(events * inflation)
                n <- private$.sample_size_from_events(events, lambda_c, hr, ratio,
                                                      accrual, follow_up, dropout)
                return(ceiling(n * design_factor))
            }
            events <- private$.expected_events_from_sample(n_input, lambda_c, hr, ratio, accrual, follow_up, dropout)$total
            private$.sequential_power(function(f) {
                private$.power_from_events(events / f, hr, alpha_adj, ratio)
            }, alpha)
        },
        .calculate_sensitivity_impact = function(param_name, new_value) {
            tryCatch(
                switch(param_name,
                    effect_size = private$.sensitivity_value(hr = new_value),
                    alpha_level = private$.sensitivity_value(alpha = new_value),
                    accrual_period = private$.sensitivity_value(accrual = new_value),
                    control_median_survival = private$.sensitivity_value(median = new_value),
                    NA_real_
                ),
                error = function(e) NA_real_
            )
        },
        .format_sensitivity_impact = function(impact_1, impact_2) {
            base <- tryCatch(private$.sensitivity_value(), error = function(e) NA_real_)
            values <- c(base, impact_1, impact_2)
            if (length(values) != 3 || any(!is.finite(values)) || base <= 0) {
                return("Parameter change affects results")
            }
            max_change <- max(abs(c(impact_1, impact_2) - base)) / base * 100
            if (max_change < 1) {
                "Minimal impact (<1% change)"
            } else if (max_change < 5) {
                sprintf("Low impact (%.1f%% change)", max_change)
            } else if (max_change < 15) {
                sprintf("Moderate impact (%.1f%% change)", max_change)
            } else {
                sprintf("High impact (%.1f%% change)", max_change)
            }
        },
        .populate_assumptions = function() {
            table <- self$results$assumptions_table

            assumptions <- list(
                list(
                    assumption = "Survival Distribution",
                    specification = paste("Exponential with median", self$options$control_median_survival, "months"),
                    impact = "Affects event rate calculations and timeline estimates",
                    recommendation = "Validate with pilot data or literature review"
                ),
                list(
                    assumption = "Proportional Hazards",
                    specification = "Hazard ratio constant over time",
                    impact = "Critical for log-rank test validity and sample size accuracy",
                    recommendation = "Plan interim monitoring for proportional hazards assumption"
                ),
                list(
                    assumption = "Dropout Rate",
                    specification = paste0(self$options$dropout_rate * 100, "% annual loss to follow-up"),
                    impact = "Reduces effective sample size and statistical power",
                    recommendation = "Implement retention strategies and monitor dropout patterns"
                ),
                list(
                    assumption = "Accrual Pattern",
                    specification = private$.format_accrual_pattern(self$options$accrual_pattern),
                    impact = "Affects study timeline and event occurrence timing",
                    recommendation = "Monitor actual accrual against assumptions"
                )
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

            keys <- private$.assumption_row_keys()
            for (i in seq_along(assumptions)) {
                table$setRow(rowKey = keys[[i]], values = assumptions[[i]])
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
            }
        },
        .generate_study_summary = function() {
            # Generate study design and parameter summary
            analysis_type <- self$options$analysis_type
            test_type <- self$options$test_type
            hr <- private$.get_effect_hr()
            if (!is.finite(hr)) {
                hr <- self$options$effect_size
            }
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
                "\u{2022} Analysis Type: ", private$.format_analysis_type(analysis_type), "<br>",
                "\u{2022} Statistical Test: ", private$.format_test_type(test_type), "<br>",
                "\u{2022} Design: ", private$.format_study_design(self$options$study_design), "<br>",
                "\u{2022} Primary Endpoint: ", private$.format_primary_endpoint(self$options$primary_endpoint), "</p>",
                "<p><strong>Statistical Parameters:</strong><br>",
                "\u{2022} Hazard Ratio: ", round(hr, 3), "<br>",
                "\u{2022} Significance Level (alpha): ", round(alpha, 3), " (", round(alpha * 100, 1), "%)<br>",
                "\u{2022} Statistical Power: ", round(power, 3), " (", round(power * 100, 1), "%)<br>",
                "\u{2022} Allocation Ratio: ", self$options$allocation_ratio, ":1<br>",
                if (analysis_type != "sample_size") paste0("\u{2022} Sample Size: ", self$options$sample_size_input, " subjects<br>") else "",
                "</p>",
                "<p><strong>Population Characteristics:</strong><br>",
                "\u{2022} Control Median Survival: ", median_survival, " months<br>",
                "\u{2022} Expected Treatment Median: ",
                if (is.finite(hr) && hr > 0) round(median_survival / hr, 1) else "NA",
                " months<br>",
                "\u{2022} Accrual Period: ", accrual, " months<br>",
                "\u{2022} Follow-up Period: ", follow_up, " months<br>",
                "\u{2022} Annual Dropout Rate: ", round(dropout * 100, 1), "%</p>",
                "<p><strong>Objective:</strong><br>",
                private$.generate_objective_text(),
                "</p>"
            )

            return(explanation)
        },
        .generate_objective_text = function() {
            # Generate objective text based on analysis type
            analysis_type <- self$options$analysis_type
            hr <- private$.get_effect_hr()
            if (!is.finite(hr)) {
                hr <- self$options$effect_size
            }
            power <- self$options$power_level

            if (analysis_type == "sample_size") {
                paste0(
                    "Determine the required sample size to detect a hazard ratio of ", round(hr, 3),
                    " with ", round(power * 100), "% power at a ", private$.alpha_pct(), "% significance level."
                )
            } else if (analysis_type == "power") {
                paste0(
                    "Calculate the statistical power to detect a hazard ratio of ", round(hr, 3),
                    " with ", self$options$sample_size_input, " subjects at a ", private$.alpha_pct(), "% significance level."
                )
            } else if (analysis_type == "effect_size") {
                paste0(
                    "Determine the minimum detectable hazard ratio with ", self$options$sample_size_input,
                    " subjects, ", round(power * 100), "% power, and ", private$.alpha_pct(), "% significance level."
                )
            } else {
                paste0(
                    "Calculate the required study duration to achieve ", round(power * 100),
                    "% power with ", self$options$sample_size_input, " subjects."
                )
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
            hr <- private$.get_effect_hr()
            if (!is.finite(hr)) {
                hr <- self$options$effect_size
            }
            power <- self$options$power_level

            interpretation <- ""

            private$.calculate_primary_result()
            n <- private$primary_numbers$n
            if (!is.null(n)) {

                interpretation <- paste0(
                    interpretation,
                    "<p><strong>Sample Size Interpretation:</strong><br>",
                    "To detect a hazard ratio of ", round(hr, 2),
                    " with ", round(power * 100), "% power and ",
                    private$.alpha_pct(), "% significance level, you need approximately ",
                    n, " total subjects.</p>"
                )

                # Add clinical context
                if (hr < 1) {
                    risk_reduction <- round((1 - hr) * 100)
                    interpretation <- paste0(
                        interpretation,
                        "<p>A hazard ratio of ", round(hr, 2),
                        " represents a ", risk_reduction,
                        "% lower hazard of the event in the treatment group compared to control.</p>"
                    )
                } else if (hr > 1) {
                    risk_increase <- round((hr - 1) * 100)
                    interpretation <- paste0(
                        interpretation,
                        "<p>A hazard ratio of ", round(hr, 2),
                        " represents a ", risk_increase,
                        "% higher hazard of the event in the treatment group compared to control.</p>"
                    )
                }

                # Add feasibility assessment
                if (n > 1000) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Feasibility Note:</strong> This large sample size may require ",
                        "multi-center collaboration or extended recruitment periods.</p>"
                    )
                } else if (n < 100) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Note:</strong> This relatively small sample size is feasible ",
                        "for single-center studies, but ensure the effect size is realistic.</p>"
                    )
                }
            }

            return(interpretation)
        },
        .generate_power_interpretation = function(result_text) {
            # Generate power specific interpretation
            hr <- private$.get_effect_hr()

            interpretation <- ""

            private$.calculate_primary_result()
            calculated_power <- private$primary_numbers$power
            if (!is.null(calculated_power)) {

                interpretation <- paste0(
                    interpretation,
                    "<p><strong>Power Interpretation:</strong><br>",
                    "With ", self$options$sample_size_input, " subjects, ",
                    "you have ", round(calculated_power * 100, 1),
                    "% probability of detecting a hazard ratio of ", round(hr, 2),
                    " if it truly exists.</p>"
                )

                # Add adequacy assessment
                if (calculated_power < 0.7) {
                    interpretation <- paste0(
                        interpretation,
                        "<p><strong>Warning:</strong> Power below 70% indicates the study is underpowered. ",
                        "Consider increasing sample size or reconsidering the effect size.</p>"
                    )
                } else if (calculated_power >= 0.8) {
                    interpretation <- paste0(
                        interpretation,
                        "<p>This power level meets standard requirements for clinical trials.</p>"
                    )
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
        # How the alpha actually entered the calculation. The report sentence is
        # meant to be pasted into a protocol, so it must not hardcode
        # "(two-sided)" over a non-inferiority test that used a one-sided alpha,
        # nor round 0.025 to "2%".
        # Alpha as a percentage that does not lose the decimal: round(0.025 * 100)
        # printed "2%" for a one-sided 2.5% test in six places.
        .alpha_pct = function() {
            alpha <- private$.adjust_alpha_for_multiplicity(self$options$alpha_level)
            formatC(alpha * 100, format = "fg", digits = 4)
        },
        .format_alpha_phrase = function() {
            alpha <- private$.adjust_alpha_for_multiplicity(self$options$alpha_level)
            sided <- if (isTRUE(self$options$test_type == "non_inferiority")) "one-sided" else "two-sided"
            pct <- formatC(alpha * 100, format = "fg", digits = 4)
            sprintf("%s%% significance level (%s)", pct, sided)
        },
        # Non-inferiority is not a test that "detects" a hazard ratio; it rules
        # out the margin. Superiority designs keep the usual wording.
        .format_objective_phrase = function(hr) {
            if (isTRUE(self$options$test_type == "non_inferiority")) {
                sprintf(
                    "to rule out a hazard ratio of %s or worse, assuming a true hazard ratio of %.2f",
                    format(self$options$ni_margin), hr
                )
            } else {
                sprintf("to detect a hazard ratio of %.2f", hr)
            }
        },
        .generate_report_sentence = function() {
            # Generate a copy-ready report sentence
            analysis_type <- self$options$analysis_type
            test_type <- self$options$test_type
            hr <- private$.get_effect_hr()
            power <- self$options$power_level
            accrual <- self$options$accrual_period
            follow_up <- self$options$follow_up_period

            # Build appropriate sentence based on analysis type
            if (analysis_type == "sample_size") {
                private$.calculate_primary_result()
                n <- private$primary_numbers$n
                if (is.null(n)) n <- "an undetermined number of"

                sentence <- paste0(
                    "A ", private$.format_test_type(test_type),
                    " power analysis was conducted to determine the sample size required ",
                    private$.format_objective_phrase(hr),
                    " with ", round(power * 100), "% power at a ",
                    private$.format_alpha_phrase(), ". ",
                    "The analysis indicates that ", n, " subjects are needed, ",
                    "with an accrual period of ", accrual, " months ",
                    "and additional follow-up of ", follow_up, " months."
                )
            } else if (analysis_type == "power") {
                private$.calculate_primary_result()
                p <- private$primary_numbers$power
                calc_power <- if (is.null(p)) "not determined" else sprintf("%.1f%%", p * 100)

                sentence <- paste0(
                    "A ", private$.format_test_type(test_type),
                    " power analysis was conducted for a study with ",
                    self$options$sample_size_input, " subjects ",
                    private$.format_objective_phrase(hr),
                    " at a ", private$.format_alpha_phrase(), ". ",
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
                    round(power * 100), "% power, and a ",
                    private$.format_alpha_phrase(), ". ",
                    result
                )
            } else if (analysis_type == "duration") {
                result <- private$.calculate_primary_result()

                sentence <- paste0(
                    "A ", private$.format_test_type(test_type),
                    " power analysis was conducted to determine the required study duration ",
                    "for ", self$options$sample_size_input, " subjects ",
                    "with ", round(power * 100), "% power and ",
                    private$.alpha_pct(), "% significance level to detect a hazard ratio of ", round(hr, 2), ". ",
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
                    # This tool computes a sample size; it cannot certify that a
                    # submission satisfies ICH E9.
                    compliance_status = "Calculation available",
                    recommendation = paste("Document calculated", result, "with all assumptions and their sources")
                )))
            }

            # Alpha level considerations. Non-inferiority is tested one-sided, and
            # one-sided 0.025 is the convention -- the previous row flagged exactly
            # that as "Conservative" and told the user to justify it to the
            # regulator, which is the opposite of the guidance.
            is_ni <- isTRUE(test_type == "non_inferiority")
            if (is_ni) {
                alpha_compliance <- if (isTRUE(all.equal(alpha, 0.025))) "Standard" else "Non-standard"
                regulatory_items <- append(regulatory_items, list(list(
                    regulatory_aspect = "Significance Level",
                    requirement = "One-sided alpha = 0.025 conventional for non-inferiority",
                    compliance_status = alpha_compliance,
                    recommendation = if (alpha_compliance == "Standard") {
                        "Matches the usual non-inferiority convention"
                    } else {
                        paste0("One-sided alpha = ", format(alpha), " departs from the usual 0.025; justify in the protocol")
                    }
                )))
            } else {
                alpha_compliance <- if (isTRUE(all.equal(alpha, 0.05))) "Standard" else if (alpha < 0.05) "Conservative" else "Non-standard"
                regulatory_items <- append(regulatory_items, list(list(
                    regulatory_aspect = "Significance Level",
                    requirement = "Two-sided alpha = 0.05 typically required",
                    compliance_status = alpha_compliance,
                    recommendation = if (alpha_compliance != "Standard") paste("Justify alpha =", format(alpha), "in the protocol") else "Standard FDA/EMA requirement"
                )))
            }

            # Power considerations
            power_compliance <- if (power >= 0.80) "Adequate" else "Insufficient"
            regulatory_items <- append(regulatory_items, list(list(
                regulatory_aspect = "Statistical Power",
                requirement = "Minimum 80% power for primary endpoint",
                compliance_status = power_compliance,
                recommendation = if (power < 0.80) "Increase power to >=80%, which is the usual expectation for a primary endpoint" else "Meets the usual 80% expectation"
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
            keys <- private$.regulatory_row_keys()
            for (i in seq_along(regulatory_items)) {
                table$setRow(rowKey = keys[[i]], values = regulatory_items[[i]])
            }
        },
        .create_visualizations = function() {
            # Only create plots if we have valid options
            if (is.null(self$options$alpha_level) || is.null(self$options$power_level)) {
                return()
            }

            # Create plot state for power curve - always generate data when available
            tryCatch(
                {
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
                },
                error = function(e) {
                    private$.addNotice("WARNING", "Power Curve Unavailable", sprintf("The power curve could not be built: %s \u{2022} The numeric results are unaffected", conditionMessage(e)))
                }
            )

            # Create plot state for sample size curve - always generate when relevant
            tryCatch(
                {
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
                },
                error = function(e) {
                    private$.addNotice("WARNING", "Sample Size Curve Unavailable", sprintf("The sample size curve could not be built: %s \u{2022} The numeric results are unaffected", conditionMessage(e)))
                }
            )

            # Create expected survival curves plot
            tryCatch(
                {
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
                                hr = private$.get_effect_hr(),
                                median_survival = self$options$control_median_survival
                            )
                        )
                        self$results$survival_curves_plot$setState(plotState)
                    }
                },
                error = function(e) {
                    private$.addNotice("WARNING", "Survival Curve Unavailable", sprintf("The expected survival curve could not be built: %s \u{2022} The numeric results are unaffected", conditionMessage(e)))
                }
            )

            # Create study timeline plot
            tryCatch(
                {
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
                },
                error = function(e) {
                    private$.addNotice("WARNING", "Timeline Plot Unavailable", sprintf("The accrual timeline could not be built: %s \u{2022} The numeric results are unaffected", conditionMessage(e)))
                }
            )

            # Create sensitivity analysis plot if enabled
            if (self$options$sensitivity_analysis) {
                tryCatch(
                    {
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
                    },
                    error = function(e) {
                        private$.addNotice("WARNING", "Sensitivity Plot Unavailable", sprintf("The sensitivity plot could not be built: %s \u{2022} The numeric results are unaffected", conditionMessage(e)))
                    }
                )
            }
        },
        .generate_power_curve_data = function() {
            # Generate data for power curve plot
            tryCatch(
                {
                    base_hr <- private$.get_effect_hr()
                    if (!is.finite(base_hr)) {
                        base_hr <- 0.75
                    }
                    hr_values <- private$.hr_grid(base_hr)
                    n_total <- if (isTRUE(self$options$analysis_type == "sample_size") &&
                        !is.null(private$calculated_sample_size)) {
                        private$calculated_sample_size
                    } else {
                        self$options$sample_size_input
                    }
                    if (is.null(n_total) || !is.finite(n_total) || n_total <= 0) {
                        n_total <- 200
                    }
                    # Same alpha and design scaling the headline uses, so the curve
                    # passes through the reported design point instead of
                    # contradicting it (multi-arm read 583 where the headline said 706).
                    alpha_adj <- private$.adjust_alpha_for_multiplicity(self$options$alpha_level)
                    scale <- private$.design_scale()
                    power_values <- sapply(hr_values, function(hr) {
                        private$.basic_power_calc(n_total / scale, hr, alpha_adj)
                    })

                    valid <- is.finite(power_values) & !is.na(power_values)
                    if (!any(valid)) {
                        return(NULL)
                    }

                    return(data.frame(
                        hazard_ratio = hr_values[valid],
                        power = power_values[valid]
                    ))
                },
                error = function(e) {
                    return(NULL)
                }
            )
        },
        .generate_sample_size_curve_data = function() {
            # Generate data for sample size curve plot
            tryCatch(
                {
                    base_hr <- private$.get_effect_hr()
                    if (!is.finite(base_hr)) {
                        base_hr <- 0.75
                    }
                    hr_values <- private$.hr_grid(base_hr)
                    alpha_adj <- private$.adjust_alpha_for_multiplicity(self$options$alpha_level)
                    scale <- private$.design_scale()
                    sample_sizes <- sapply(hr_values, function(hr) {
                        res <- private$.basic_sample_size_calc(self$options$power_level, hr, alpha_adj)
                        if (is.finite(res)) ceiling(res * scale) else NA_real_
                    })

                    valid <- is.finite(sample_sizes) & !is.na(sample_sizes) & sample_sizes > 0
                    if (!any(valid)) {
                        return(NULL)
                    }

                    return(data.frame(
                        hazard_ratio = hr_values[valid],
                        sample_size = sample_sizes[valid]
                    ))
                },
                error = function(e) {
                    return(NULL)
                }
            )
        },
        .generate_survival_curve_data = function() {
            # Generate expected survival curves for control and treatment groups
            tryCatch(
                {
                    # Span the study, not a fixed 60 months: a 36-month trial was
                    # drawn out to 60 and a 108-month one was cut short.
                    horizon <- max(12, self$options$accrual_period + self$options$follow_up_period)
                    time <- seq(0, horizon, length.out = 121)
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
                },
                error = function(e) {
                    return(NULL)
                }
            )
        },
        .generate_timeline_data = function() {
            # Generate study timeline data
            tryCatch(
                {
                    accrual_period <- self$options$accrual_period
                    follow_up <- self$options$follow_up_period

                    timeline_data <- data.frame(
                        phase = c("Accrual", "Follow-up", "Analysis"),
                        start = c(0, accrual_period, accrual_period + follow_up),
                        end = c(accrual_period, accrual_period + follow_up, accrual_period + follow_up + 3),
                        y = c(1, 1, 1)
                    )

                    return(timeline_data)
                },
                error = function(e) {
                    return(NULL)
                }
            )
        },
        .generate_sensitivity_plot_data = function() {
            # Generate data for sensitivity analysis plot
            tryCatch(
                {
                    # Create a range of hazard ratios around the base value
                    base_hr <- private$.get_effect_hr()
                    hr_range <- private$.hr_grid(base_hr)
                    alpha_adj <- private$.adjust_alpha_for_multiplicity(self$options$alpha_level)
                    scale <- private$.design_scale()

                    # Calculate required sample size for each HR
                    sample_sizes <- sapply(hr_range, function(hr) {
                        res <- private$.basic_sample_size_calc(
                            self$options$power_level,
                            hr,
                            alpha_adj
                        )
                        if (is.finite(res)) ceiling(res * scale) else NA_real_
                    })

                    valid <- is.finite(sample_sizes) & !is.na(sample_sizes) & sample_sizes > 0
                    if (!any(valid)) {
                        return(NULL)
                    }

                    # Create data frame for plotting
                    return(data.frame(
                        hazard_ratio = hr_range[valid],
                        sample_size = sample_sizes[valid],
                        is_base_case = abs(hr_range[valid] - base_hr) < 0.01
                    ))
                },
                error = function(e) {
                    return(NULL)
                }
            )
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

            if (isTRUE(self$options$test_type == "non_inferiority")) {
                hr_margin <- self$options$ni_margin
                if (is.null(hr) || !is.finite(hr) || hr <= 0 || hr >= hr_margin) {
                    return(0)
                }
                z_alpha <- qnorm(1 - alpha)
                info <- private$.information_from_events(expected_events, self$options$allocation_ratio)
                power <- private$.sequential_power(function(f) {
                    pnorm(-z_alpha - sqrt(info / f) * (log(hr) - log(hr_margin)))
                })
            } else {
                power <- private$.sequential_power(function(f) {
                    private$.power_from_events(
                        events = expected_events / f,
                        hr = hr,
                        alpha = alpha,
                        allocation_ratio = self$options$allocation_ratio
                    )
                })
            }
            max(0, min(1, power))
        },
        .basic_sample_size_calc = function(power, hr, alpha) {
            if (isTRUE(self$options$test_type == "non_inferiority")) {
                hr_margin <- self$options$ni_margin
                if (is.null(hr) || !is.finite(hr) || hr <= 0 || hr >= hr_margin) {
                    return(NA_real_)
                }
                props <- private$.allocation_props(self$options$allocation_ratio)
                info_fraction <- props$control * props$treatment
                z_alpha <- qnorm(1 - alpha)
                z_beta <- qnorm(power)
                log_effect <- log(hr) - log(hr_margin)
                events_needed <- ceiling(((z_alpha + z_beta)^2) / ((log_effect)^2 * info_fraction))
                events_needed <- ceiling(events_needed * private$.sequential_inflation(power))
            } else {
                if (is.null(hr) || !is.finite(hr) || hr <= 0 || abs(hr - 1) < 1e-6) {
                    return(NA_real_)
                }
                events_needed <- private$.events_needed_log_rank(
                    hr = hr,
                    alpha = alpha,
                    power = power,
                    ratio = self$options$allocation_ratio
                )
                events_needed <- ceiling(events_needed * private$.sequential_inflation(power))
            }

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
                private$effect_hr_info <- list(
                    type = "hazard_ratio", hr = es,
                    note = "Using hazard ratio directly as effect size."
                )
                return(es)
            }

            # Convert from median ratio (treatment/control)
            if (type == "median_ratio") {
                if (is.null(es) || es <= 0) {
                    private$effect_hr_info <- list(
                        type = "median_ratio",
                        hr = NA_real_,
                        note = "Median survival ratio must be positive to derive a hazard ratio."
                    )
                    return(NA_real_)
                }
                # Under exponential assumption: median_t/median_c = 1/HR
                hr <- 1 / es
                private$effect_hr_info <- list(
                    type = "median_ratio", hr = hr,
                    note = paste0(
                        "Converted from median survival ratio (treatment/control) = ",
                        sprintf("%.3f", es),
                        "; assuming exponential survival, HR = 1 / ratio."
                    )
                )
                return(hr)
            }

            # Convert from RMST difference at tau via numerical solve under exponential assumption
            if (type == "rmst_difference") {
                delta <- self$options$rmst_difference
                tau <- self$options$rmst_tau
                mc <- self$options$control_median_survival
                if (is.null(delta) || is.null(tau) || is.null(mc) || tau <= 0 || mc <= 0) {
                    private$effect_hr_info <- list(
                        type = "rmst_difference",
                        hr = NA_real_,
                        note = "RMST parameters incomplete; unable to derive hazard ratio."
                    )
                    return(NA_real_)
                }
                lambda_c <- log(2) / mc
                rmst_diff_fn <- function(hr) {
                    lambda_t <- lambda_c * hr
                    rmst_c <- (1 - exp(-lambda_c * tau)) / lambda_c
                    rmst_t <- (1 - exp(-lambda_t * tau)) / lambda_t
                    (rmst_t - rmst_c) - delta
                }
                a <- 0.2
                b <- 3.0
                fa <- rmst_diff_fn(a)
                fb <- rmst_diff_fn(b)
                if (is.finite(fa) && is.finite(fb) && fa * fb <= 0) {
                    hr <- tryCatch(uniroot(rmst_diff_fn, c(a, b))$root, error = function(e) NA)
                    if (is.finite(hr)) {
                        private$effect_hr_info <- list(
                            type = "rmst_difference", hr = hr,
                            note = paste0(
                                "Derived HR from RMST difference ", sprintf("%.3f", delta),
                                " at tau = ", sprintf("%.1f", tau),
                                " months under exponential assumption."
                            )
                        )
                        return(hr)
                    }
                }
                private$effect_hr_info <- list(
                    type = "rmst_difference",
                    hr = NA_real_,
                    note = paste0(
                        "Could not solve HR from RMST difference (inputs: \u{0394}=",
                        sprintf("%.3f", delta), ", tau=", sprintf("%.1f", tau),
                        ", median_c=", sprintf("%.1f", mc), ")."
                    )
                )
                return(NA_real_)
            }

            # Convert from survival probability difference at follow-up
            if (type == "survival_difference") {
                delta <- es
                time_pt <- self$options$follow_up_period
                mc <- self$options$control_median_survival
                if (is.null(delta) || is.null(time_pt) || is.null(mc) || time_pt <= 0 || mc <= 0) {
                    private$effect_hr_info <- list(
                        type = "survival_difference",
                        hr = NA_real_,
                        note = "Survival difference parameters incomplete; unable to derive hazard ratio."
                    )
                    return(NA_real_)
                }
                lambda_c <- log(2) / mc
                s_c <- function(t) exp(-lambda_c * t)
                target_fn <- function(hr) {
                    s_t <- exp(-(lambda_c * hr) * time_pt)
                    (s_t - s_c(time_pt)) - delta
                }
                a <- 0.2
                b <- 3.0
                fa <- target_fn(a)
                fb <- target_fn(b)
                if (is.finite(fa) && is.finite(fb) && fa * fb <= 0) {
                    hr <- tryCatch(uniroot(target_fn, c(a, b))$root, error = function(e) NA)
                    if (is.finite(hr)) {
                        private$effect_hr_info <- list(
                            type = "survival_difference", hr = hr,
                            note = paste0(
                                "Derived HR from survival difference ", sprintf("%.3f", delta),
                                " at follow-up = ", sprintf("%.1f", time_pt),
                                " months under exponential assumption."
                            )
                        )
                        return(hr)
                    }
                }
                private$effect_hr_info <- list(
                    type = "survival_difference",
                    hr = NA_real_,
                    note = paste0(
                        "Could not solve HR from survival difference (inputs: \u{0394}=",
                        sprintf("%.3f", delta), ", follow-up=", sprintf("%.1f", time_pt),
                        ", median_c=", sprintf("%.1f", mc), ")."
                    )
                )
                return(NA_real_)
            }

            # Unknown type - return provided value
            private$effect_hr_info <- list(
                type = type, hr = es,
                note = "Using provided effect size without conversion."
            )
            return(es)
        },

        # Plot functions
        .plot_power_curves = function(image, ggtheme, theme, ...) {
            tryCatch(
                {
                    plotData <- image$state

                    if (is.null(plotData)) {
                        return(FALSE)
                    }

                    if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                        return(FALSE)
                    }

                    # Create power curve plot
                    plot <- ggplot2::ggplot(plotData$data, ggplot2::aes(x = .data$hazard_ratio, y = .data$power)) +
                        ggplot2::geom_line(linewidth = 1.2, color = "blue") +
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
                },
                error = function(e) {
                    return(FALSE)
                }
            )
        },
        .plot_sample_size_curves = function(image, ggtheme, theme, ...) {
            tryCatch(
                {
                    plotData <- image$state

                    if (is.null(plotData)) {
                        return(FALSE)
                    }

                    if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                        return(FALSE)
                    }

                    # Create sample size curve plot
                    plot <- ggplot2::ggplot(plotData$data, ggplot2::aes(x = .data$hazard_ratio, y = .data$sample_size)) +
                        ggplot2::geom_line(linewidth = 1.2, color = "red") +
                        ggplot2::labs(
                            title = plotData$config$title,
                            x = plotData$config$xlab,
                            y = plotData$config$ylab
                        ) +
                        ggtheme

                    print(plot)
                    return(TRUE)
                },
                error = function(e) {
                    return(FALSE)
                }
            )
        },
        .plot_expected_survival = function(image, ggtheme, theme, ...) {
            tryCatch(
                {
                    plotData <- image$state

                    if (is.null(plotData)) {
                        return(FALSE)
                    }

                    if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                        return(FALSE)
                    }

                    # Create survival curve plot
                    plot <- ggplot2::ggplot(plotData$data, ggplot2::aes(x = .data$time, y = .data$survival, color = .data$group)) +
                        ggplot2::geom_line(linewidth = 1.2) +
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
                },
                error = function(e) {
                    return(FALSE)
                }
            )
        },
        .plot_study_timeline = function(image, ggtheme, theme, ...) {
            tryCatch(
                {
                    plotData <- image$state

                    if (is.null(plotData)) {
                        return(FALSE)
                    }

                    if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                        return(FALSE)
                    }

                    # Create timeline plot
                    plot <- ggplot2::ggplot(
                        plotData$data,
                        ggplot2::aes(
                            xmin = .data$start, xmax = .data$end,
                            ymin = .data$y - 0.2, ymax = .data$y + 0.2,
                            fill = .data$phase
                        )
                    ) +
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
                },
                error = function(e) {
                    return(FALSE)
                }
            )
        },
        # Built separately from the renderer so its layer structure is testable
        # without a graphics device.
        .build_sensitivity_plot = function(data) {
            plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$hazard_ratio, y = .data$sample_size)) +
                ggplot2::geom_line(color = "steelblue", linewidth = 1) +
                ggplot2::geom_point(size = 3, color = "steelblue")
            # The highlight used to sit inside `+ if (...) { plot <- plot + ... }`,
            # where `plot` was not yet the ggplot, so the base-case point was
            # silently dropped: 2 layers instead of 3.
            if ("is_base_case" %in% names(data) && any(data$is_base_case)) {
                plot <- plot + ggplot2::geom_point(
                    data = data[data$is_base_case, , drop = FALSE],
                    color = "red", size = 5, shape = 21, fill = "red", alpha = 0.7
                )
            }
            plot + ggplot2::labs(title = "Sensitivity Analysis", x = "Hazard Ratio", y = "Required Sample Size")
        },
        .plot_sensitivity_analysis = function(image, ggtheme, theme, ...) {
            tryCatch(
                {
                    plotData <- image$state

                    if (is.null(plotData)) {
                        return(FALSE)
                    }

                    if (is.null(plotData$data) || nrow(plotData$data) == 0) {
                        return(FALSE)
                    }

                    print(private$.build_sensitivity_plot(plotData$data) + ggtheme)
                    return(TRUE)
                },
                error = function(e) {
                    return(FALSE)
                }
            )
        },
        .get_distribution_parameters = function(median_control, hr) {
            distribution <- self$options$survival_distribution
            weibull_shape <- self$options$weibull_shape

            if (distribution == "exponential") {
                # Exponential distribution (original implementation)
                lambda_control <- log(2) / median_control
                lambda_treatment <- lambda_control * hr

                return(list(
                    distribution = "exponential",
                    lambda_control = lambda_control,
                    lambda_treatment = lambda_treatment,
                    median_control = median_control,
                    median_treatment = median_control / hr
                ))
            } else if (distribution == "weibull") {
                # Weibull distribution support
                # S(t) = exp(-lambda * t^shape), so H(t) = lambda * t^shape
                # Median: m = (log(2)/lambda)^(1/shape)  ->  lambda = log(2)/m^shape
                # Hazard h(t) = lambda*shape*t^(shape-1), so under PH the shape is
                # shared and only the scale moves: lambda_treatment = lambda_control * hr
                # (These reduce to the exponential case at shape = 1.)

                if (is.null(weibull_shape) || weibull_shape <= 0) {
                    stop("Weibull shape parameter must be positive (default: 1.0 for exponential)")
                }

                # Calculate scale parameter from median
                lambda_control <- log(2) / median_control^weibull_shape

                # Treatment group scale under proportional hazards
                lambda_treatment <- lambda_control * hr

                # Calculate treatment median
                median_treatment <- (log(2) / lambda_treatment)^(1 / weibull_shape)

                return(list(
                    distribution = "weibull",
                    lambda_control = lambda_control,
                    lambda_treatment = lambda_treatment,
                    shape = weibull_shape,
                    median_control = median_control,
                    median_treatment = median_treatment
                ))
            } else if (distribution == "log_normal") {
                # Log-normal distribution support
                # log(T) ~ N(mu, sigma^2)
                # Median: m = exp(mu)
                # Approximate HR effect: mu_treatment = mu_control - log(hr)

                # Use weibull_shape parameter as sigma for log-normal
                sigma <- if (!is.null(weibull_shape) && weibull_shape > 0) weibull_shape else 1.0

                mu_control <- log(median_control)
                mu_treatment <- mu_control - log(hr)
                median_treatment <- exp(mu_treatment)

                return(list(
                    distribution = "lognormal",
                    mu_control = mu_control,
                    mu_treatment = mu_treatment,
                    sigma = sigma,
                    median_control = median_control,
                    median_treatment = median_treatment
                ))
            } else if (distribution == "piecewise_exponential") {
                # Piecewise exponential - requires custom specification
                # For now, return error with guidance
                stop(paste(
                    "Piecewise exponential distribution requires custom interval specification.",
                    "Please use exponential or Weibull distribution, or contact package maintainer",
                    "for guidance on implementing piecewise exponential models."
                ))
            } else {
                stop(paste(
                    "Unsupported distribution:", distribution,
                    "\u{2022} Supported: exponential, weibull, log_normal",
                    "\u{2022} For piecewise exponential, contact package maintainer"
                ))
            }
        },
        .adjust_alpha_for_multiplicity = function(alpha) {
            study_design <- self$options$study_design
            multiple_comparisons <- self$options$multiple_comparisons
            number_of_arms <- self$options$number_of_arms

            if (study_design == "multi_arm" && number_of_arms > 2) {
                if (multiple_comparisons == "bonferroni") {
                    alpha_adjusted <- alpha / (number_of_arms - 1)
                } else if (multiple_comparisons == "holm") {
                    # Conservative Holm approximation
                    alpha_adjusted <- alpha / (number_of_arms - 1)
                } else if (multiple_comparisons == "dunnett") {
                    # Conservative Dunnett approximation
                    alpha_adjusted <- alpha / (number_of_arms - 1)
                } else if (multiple_comparisons == "none" || is.null(multiple_comparisons)) {
                    alpha_adjusted <- alpha
                } else {
                    alpha_adjusted <- alpha
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

            # Design effect for cluster randomized trials
            if (study_design == "cluster_randomized" && cluster_size > 1 && icc > 0) {
                design_effect <- 1 + (cluster_size - 1) * icc
            } else {
                design_effect <- 1
            }

            return(list(
                design_effect = design_effect,
                arm_factor = private$.multi_arm_factor()
            ))
        },
        # Enrolment multiplier that turns one pairwise comparison into a
        # multi-arm trial sharing a single control group.
        #
        # The pairwise machinery sizes ONE comparison: n_control + n_exp = N_pair,
        # split by r = allocation_ratio (control:experimental). A k-arm trial
        # keeps that control group and adds k-1 experimental arms of the same
        # size, so total = n_control + (k-1) * n_exp = N_pair * (r + k - 1)/(1 + r).
        # It reduces to 1 at k = 2.
        #
        # Without this the headline "Total Sample Size" for a 3-arm trial was the
        # size of a single pairwise comparison -- a third to a half of the trial --
        # and every comparison came out under-powered (60.9% when 80% was asked
        # for) once the multi-arm table divided that total across the arms.
        .multi_arm_factor = function() {
            if (!isTRUE(self$options$study_design == "multi_arm")) {
                return(1)
            }

            k <- self$options$number_of_arms
            if (is.null(k) || !is.finite(k) || k <= 2) {
                return(1)
            }

            r <- self$options$allocation_ratio
            if (is.null(r) || !is.finite(r) || r <= 0) r <- 1

            (r + k - 1) / (1 + r)
        },
        .adjust_sample_for_accrual = function(base_sample_size) {

            # Non-uniform accrual is refused in .validate_inputs(); by the time a
            # sample size is being adjusted the pattern is known to be uniform.
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
            if (is.null(events) || events <= 0 || is.null(hr) || hr <= 0) {
                return(0)
            }

            info <- private$.information_from_events(events, allocation_ratio)
            if (info <= 0) {
                return(0)
            }

            z_alpha <- qnorm(1 - alpha / 2)
            z <- sqrt(info) * abs(log(hr))
            # Both rejection tails are needed near the null: HR = 1 has power
            # alpha, and the opposite tail remains nonzero for small effects.
            pnorm(z - z_alpha) + pnorm(-z - z_alpha)
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

            follow_up_solution <- tryCatch(
                {
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
                },
                error = function(e) NA_real_
            )

            if (is.na(follow_up_solution)) {
                return(NA_real_)
            }

            accrual_period + follow_up_solution
        },
        # Enrolment multiplier the headline sample size carries (cluster design
        # effect, multi-arm shared control). Curves that ignore it
        # do not pass through the design point they are drawn to illustrate.
        .design_scale = function() {
            adj <- private$.adjust_sample_for_design()
            # Sequential information is handled once, inside the calculation helpers.
            adj$design_effect * adj$arm_factor
        },
        # Hazard-ratio grid for the curves, centred on the assumed effect and
        # always on one side of 1. HR = 1 has no finite sample size (the log-rank
        # solver stops there), and a grid that merely touched 1.0 took the whole
        # plot down: seq(max(0.3, hr - 0.3), min(1.0, hr + 0.3), 0.05) hit exactly
        # 1.0 for every HR >= 0.70, sapply threw, and the plot rendered blank with
        # no message.
        .hr_grid = function(base_hr, span = 0.3, step = 0.05) {
            if (is.null(base_hr) || !is.finite(base_hr) || base_hr <= 0) {
                base_hr <- 0.75
            }
            if (isTRUE(self$options$test_type == "non_inferiority")) {
                hr_margin <- self$options$ni_margin
                if (is.null(hr_margin) || !is.finite(hr_margin) || hr_margin <= 1) {
                    hr_margin <- 1.25
                }
                lo <- max(0.1, base_hr - span)
                hi <- min(hr_margin - 0.01, base_hr + span)
                if (hi <= lo) hi <- min(hr_margin - 0.01, lo + step)
                grid <- seq(lo, hi, by = step)
                grid <- grid[grid < hr_margin]
                return(unique(sort(c(grid, base_hr))))
            }
            if (base_hr < 1) {
                lo <- max(0.05, base_hr - span)
                hi <- min(0.95, base_hr + span)
                if (hi <= lo) hi <- min(0.95, lo + step)
            } else {
                lo <- max(1.05, base_hr - span)
                hi <- min(5, base_hr + span)
                if (hi <= lo) hi <- min(5, lo + step)
            }
            grid <- seq(lo, hi, by = step)
            grid <- grid[abs(grid - 1) > 1e-8]
            if (abs(base_hr - 1) > 1e-8) {
                unique(sort(c(grid, base_hr)))
            } else {
                unique(sort(grid))
            }
        },
        # P(at least one of n_comparisons one-sided tests rejects) when every
        # experimental arm truly has the assumed effect. The statistics share a
        # control arm, so they are equicorrelated at rho; mvtnorm integrates the
        # multivariate normal orthant. Falls back to the independence value only
        # if mvtnorm is unavailable, and to NA if that fails too.
        .disjunctive_power = function(pairwise_power, alpha, n_comparisons, rho) {
            if (!is.finite(pairwise_power) || n_comparisons < 1) {
                return(NA_real_)
            }
            if (n_comparisons == 1) {
                return(pairwise_power)
            }

            z_crit <- qnorm(1 - alpha / 2)
            # Non-centrality that reproduces the per-comparison power.
            ncp <- z_crit + qnorm(pairwise_power)

            if (!requireNamespace("mvtnorm", quietly = TRUE)) {
                return(1 - (1 - pairwise_power)^n_comparisons)
            }

            rho <- min(max(rho, 0), 0.999)
            sigma <- matrix(rho, n_comparisons, n_comparisons)
            diag(sigma) <- 1

            # 1 - P(all below the boundary)
            tryCatch(
                1 - as.numeric(mvtnorm::pmvnorm(
                    upper = rep(z_crit, n_comparisons),
                    mean = rep(ncp, n_comparisons),
                    sigma = sigma
                )),
                error = function(e) NA_real_
            )
        },
        # Group-sequential (or fixed) survival design from gsDesign, for the
        # current options at the supplied type II error rate. Returns NULL when
        # gsDesign is unavailable or refuses the design; every caller falls back
        # to the module's own Schoenfeld path, which agrees with gsDesign to
        # within a few subjects for the fixed design.
        #
        # gsSurv's `ratio` is experimental:control; allocation_ratio is
        # control:experimental (see .allocation_props), hence the reciprocal.
        # Alpha is one-sided (half the two-sided level for superiority), so k = 1
        # reproduces .events_needed_log_rank's qnorm(1 - alpha/2).
        # One-sided alpha of the test being planned: half the multiplicity-adjusted
        # two-sided level for superiority, alpha_level itself for non-inferiority.
        .one_sided_alpha = function(alpha = self$options$alpha_level) {
            alpha <- private$.adjust_alpha_for_multiplicity(alpha)
            if (isTRUE(self$options$test_type == "non_inferiority")) {
                alpha
            } else {
                alpha / 2
            }
        },
        # Information-scale group-sequential design (n.fix = 1). Boundaries, alpha
        # spending and the information inflation depend only on the looks, the
        # spending function, one-sided alpha and power -- not on the survival
        # model -- so this single object serves every test type, including
        # non-inferiority, where a gsSurv call at the assumed HR of 1 has no effect
        # to design for and failed. Efficacy-only (test.type = 1): the options
        # expose an alpha-spending function and no futility rule.
        .gs_info_design = function(power = self$options$power_level, alpha = self$options$alpha_level) {
            if (!requireNamespace("gsDesign", quietly = TRUE)) {
                return(NULL)
            }
            if (!isTRUE(self$options$interim_analyses > 0) ||
                !isTRUE(self$options$alpha_spending %in% c("obrien_fleming", "pocock"))) {
                return(NULL)
            }
            one_sided <- private$.one_sided_alpha(alpha)
            if (!is.finite(power) || power <= one_sided || power >= 1) {
                return(NULL)
            }
            k <- as.integer(self$options$interim_analyses) + 1L
            tryCatch(
                gsDesign::gsDesign(
                    k = k, test.type = 1, alpha = one_sided, beta = 1 - power, n.fix = 1,
                    timing = seq_len(k) / k,
                    sfu = if (self$options$alpha_spending == "obrien_fleming") gsDesign::sfLDOF else gsDesign::sfLDPocock
                ),
                error = function(e) NULL
            )
        },
        # Factor by which the sequential design's maximum information exceeds the
        # fixed design's at the same alpha and power; 1 without interim looks.
        .sequential_inflation = function(power = self$options$power_level, alpha = self$options$alpha_level) {
            design <- private$.gs_info_design(power, alpha)
            if (is.null(design)) 1 else max(design$n.I)
        },
        # Power of the sequential design, given fixed_power(f): the fixed-design
        # power when the available information is divided by f. The inflation
        # itself depends on the power being solved for, so solve
        # p = fixed_power(inflation(p)).
        .sequential_power = function(fixed_power, alpha = self$options$alpha_level) {
            if (is.null(private$.gs_info_design(alpha = alpha))) {
                return(fixed_power(1))
            }
            lo <- private$.one_sided_alpha(alpha) + 0.01
            hi <- 0.999
            gap <- function(p) fixed_power(private$.sequential_inflation(p, alpha)) - p
            g_lo <- gap(lo)
            g_hi <- gap(hi)
            if (!is.finite(g_lo) || !is.finite(g_hi)) {
                return(NA_real_)
            }
            if (g_lo <= 0) {
                return(fixed_power(private$.sequential_inflation(lo, alpha)))
            }
            if (g_hi >= 0) {
                return(fixed_power(private$.sequential_inflation(hi, alpha)))
            }
            uniroot(gap, c(lo, hi))$root
        },
        .gs_design = function(beta) {
            if (!requireNamespace("gsDesign", quietly = TRUE)) {
                return(NULL)
            }

            hr <- private$.get_effect_hr()
            median_control <- self$options$control_median_survival
            if (!is.finite(hr) || hr <= 0 || is.null(median_control) || median_control <= 0) {
                return(NULL)
            }
            if (!is.finite(beta) || beta <= 0 || beta >= 1) {
                return(NULL)
            }

            spending <- self$options$alpha_spending
            k <- as.integer(self$options$interim_analyses) + 1L
            # "No spending function" cannot be honoured sequentially -- treat it
            # as the fixed design rather than inventing a boundary.
            if (k > 1L && !isTRUE(spending %in% c("obrien_fleming", "pocock"))) {
                k <- 1L
            }

            ratio <- self$options$allocation_ratio
            if (is.null(ratio) || !is.finite(ratio) || ratio <= 0) ratio <- 1

            args <- list(
                k = k,
                # Efficacy-only (test.type = 1) at one-sided alpha. gsSurv's default
                # test.type = 4 adds a non-binding futility bound that the options
                # never expose and the interim table never reports; it inflated the
                # 3-look O'Brien-Fleming sample size to 618 where efficacy-only
                # needs 588.
                test.type = 1,
                alpha = private$.one_sided_alpha(),
                sided = 1,
                beta = beta,
                lambdaC = log(2) / median_control,
                hr = hr,
                eta = private$.dropout_hazard(self$options$dropout_rate),
                ratio = 1 / ratio,
                R = self$options$accrual_period,
                T = self$options$accrual_period + self$options$follow_up_period,
                minfup = self$options$follow_up_period
            )

            if (k > 1L) {
                args$timing <- seq_len(k) / k
                args$sfu <- if (spending == "obrien_fleming") gsDesign::sfLDOF else gsDesign::sfLDPocock
            }

            tryCatch(do.call(gsDesign::gsSurv, args), error = function(e) NULL)
        },
        # Total enrolment implied by a gsSurv design: the expected accrual in
        # both arms by the final analysis.
        .gs_total_n = function(design) {
            if (is.null(design)) {
                return(NA_real_)
            }
            k <- nrow(design$eNC)
            sum(design$eNC[k, ]) + sum(design$eNE[k, ])
        },
        .simulation_applicable = function() {
            isTRUE(self$options$analysis_type %in% c("sample_size", "power")) &&
                isTRUE(self$options$test_type %in% c("log_rank", "cox_regression")) &&
                isTRUE(self$options$survival_distribution == "exponential") &&
                isTRUE(self$options$study_design %in% c("two_arm_parallel", "stratified")) &&
                isTRUE(self$options$interim_analyses == 0)
        },
        .run_simulation_analysis = function() {
            # Enhanced simulation-based power analysis with convergence diagnostics
            if (!self$options$run_simulation_validation) {
                return(NULL)
            }

            # The simulator runs a two-sided superiority log-rank test, and the
            # comparison table's analytical value comes from the superiority
            # formula. That is the right comparator for log-rank and Cox (same
            # Schoenfeld arithmetic) but not for non-inferiority, where the table
            # agreed with itself while disagreeing with the one-sided NI power
            # reported above it.
            if (!private$.simulation_applicable()) {
                private$.addNotice(
                    "INFO", "Simulation Validation Not Applicable",
                    "Monte Carlo validation requires a fixed two-arm log-rank or Cox sample-size or power calculation with exponential survival. Multi-arm, cluster-randomized, group-sequential and non-inferiority designs are not simulated."
                )
                return(NULL)
            }

            if (!is.null(private$simulation_cache)) {
                return(private$simulation_cache)
            }

            # Seed so the reported empirical power is reproducible across runs of an
            # unchanged design; without it the agreement verdict flips on noise alone.
            # withr::local_seed sets the seed locally and restores session RNG on exit.
            sim_seed <- self$options$simulation_seed
            if (is.null(sim_seed) || !is.finite(sim_seed)) sim_seed <- 42
            withr::local_seed(as.integer(sim_seed))

            n_sims <- self$options$simulation_runs
            if (is.null(n_sims) || n_sims < 100) n_sims <- 1000 # Increased default
            if (n_sims > 100000) n_sims <- 100000 # Allow more simulations

            tryCatch(
                {
                    # Get base parameters
                    hr_true <- private$.get_effect_hr()
                    alpha <- self$options$alpha_level

                    n_total <- if (self$options$analysis_type == "sample_size") {
                        private$.calculate_primary_result()
                        private$primary_numbers$n
                    } else {
                        self$options$sample_size_input
                    }
                    if (is.null(n_total)) n_total <- NA_real_

                    if (is.na(n_total) || n_total < 10) n_total <- 200

                    # Get distribution parameters
                    dist_params <- private$.get_distribution_parameters(
                        median_control = self$options$control_median_survival,
                        hr = hr_true
                    )

                    accrual <- self$options$accrual_period
                    follow_up <- self$options$follow_up_period
                    dropout_rate <- self$options$dropout_rate
                    allocation_ratio <- self$options$allocation_ratio

                    # Allocation
                    props <- private$.allocation_props(allocation_ratio)
                    n_control <- round(n_total * props$control)
                    n_treatment <- n_total - n_control

                    p_values <- numeric(n_sims)
                    event_counts <- numeric(n_sims)

                    # Monte Carlo Simulation
                    for (i in 1:n_sims) {
                        # Up to 100,000 simulated trials with a survdiff fit each ran for
                        # 10-60 s with no way to stop them: changing an option could not
                        # interrupt the run. Checked outside the survdiff tryCatch below,
                        # which catches every error and would convert a restart into a
                        # non-significant replicate. flush = FALSE: nothing new to send
                        # mid-loop, so skip re-serialising the results each time.
                        if (i %% 50 == 0) private$.checkpoint(flush = FALSE)

                        # Simulate trial data using distribution-specific function
                        sim_data <- private$.simulate_trial_data(
                            n_control = n_control,
                            n_treatment = n_treatment,
                            dist_params = dist_params,
                            accrual_period = accrual,
                            follow_up_period = follow_up,
                            dropout_rate = dropout_rate
                        )

                        # Perform Log-rank test
                        tryCatch(
                            {
                                sdf <- survival::survdiff(
                                    survival::Surv(sim_data$time, sim_data$event) ~ sim_data$group
                                )
                                p_values[i] <- 1 - pchisq(sdf$chisq, df = 1)
                                event_counts[i] <- sum(sim_data$event)
                            },
                            error = function(e) {
                                # <<- : a plain <- would assign into the handler's own
                                # frame, leaving p_values[i] at its 0 initial value, which
                                # counts a failed test as significant and inflates power.
                                p_values[i] <<- 1.0 # Non-significant if test fails
                                event_counts[i] <<- 0
                            }
                        )
                    }

                    # Calculate empirical power
                    empirical_power <- mean(p_values < alpha, na.rm = TRUE)

                    # Calculate Monte Carlo standard error
                    mc_se <- sqrt(empirical_power * (1 - empirical_power) / n_sims)

                    # 95% Confidence interval for simulated power
                    ci_lower <- max(0, empirical_power - 1.96 * mc_se)
                    ci_upper <- min(1, empirical_power + 1.96 * mc_se)

                    # Calculate average number of events
                    avg_events <- mean(event_counts, na.rm = TRUE)

                    # Convergence diagnostics
                    convergence <- private$.assess_simulation_convergence(
                        power_estimate = empirical_power,
                        n_sims = n_sims,
                        mc_se = mc_se
                    )

                    private$simulation_cache <- list(
                        empirical_power = empirical_power,
                        mc_se = mc_se,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper,
                        n_sims = n_sims,
                        avg_events = avg_events,
                        convergence = convergence,
                        p_values = p_values,
                        event_counts = event_counts
                    )
                    return(private$simulation_cache)
                },
                error = function(e) {
                    # A jamovi restart arrives as simpleError with code "restart"
                    # (jmvcore::createError("restarting", "restart")). Re-raise it:
                    # reporting it as a failed simulation would both mislabel the
                    # user's option change and keep this stale run going.
                    if (identical(e$code, "restart")) stop(e)
                    private$.addNotice("WARNING", "Simulation Validation Unavailable", sprintf("Monte Carlo validation could not be completed: %s \u{2022} The analytical results are unaffected", conditionMessage(e)))
                    return(NULL)
                }
            )
        },
        .simulate_trial_data = function(n_control, n_treatment, dist_params,
                                        accrual_period, follow_up_period, dropout_rate) {
            # Simulate a single trial dataset based on distribution

            total_study_time <- accrual_period + follow_up_period
            n_total <- n_control + n_treatment

            # Generate accrual times (uniform)
            accrual_time <- runif(n_total, 0, accrual_period)

            # Generate event times based on distribution
            if (dist_params$distribution == "exponential") {
                event_time_control <- rexp(n_control, rate = dist_params$lambda_control)
                event_time_treatment <- rexp(n_treatment, rate = dist_params$lambda_treatment)
            } else if (dist_params$distribution == "weibull") {
                # Weibull random generation
                lambda_c <- dist_params$lambda_control
                lambda_t <- dist_params$lambda_treatment
                shape <- dist_params$shape

                event_time_control <- (-log(runif(n_control)) / lambda_c)^(1 / shape)
                event_time_treatment <- (-log(runif(n_treatment)) / lambda_t)^(1 / shape)
            } else if (dist_params$distribution == "lognormal") {
                # Log-normal random generation
                event_time_control <- rlnorm(n_control,
                    meanlog = dist_params$mu_control,
                    sdlog = dist_params$sigma
                )
                event_time_treatment <- rlnorm(n_treatment,
                    meanlog = dist_params$mu_treatment,
                    sdlog = dist_params$sigma
                )
            } else {
                stop("Unsupported distribution for simulation")
            }

            event_time <- c(event_time_control, event_time_treatment)

            # Generate dropout times (exponential)
            dropout_hazard <- -log(1 - dropout_rate) / 12 # Annual to monthly
            dropout_time <- rexp(n_total, rate = dropout_hazard)

            # Administrative censoring time
            admin_censor_time <- total_study_time - accrual_time

            # Observed time = minimum of event, dropout, admin censoring
            observed_time <- pmin(event_time, dropout_time, admin_censor_time)
            event <- (event_time <= dropout_time) & (event_time <= admin_censor_time)

            # Create dataset
            data.frame(
                group = c(rep(0, n_control), rep(1, n_treatment)),
                time = observed_time,
                event = as.numeric(event),
                accrual_time = accrual_time
            )
        },
        .assess_simulation_convergence = function(power_estimate, n_sims, mc_se) {
            # Assess whether simulation has converged

            # Target MC SE: aim for < 0.01 (1 percentage point)
            target_mc_se <- 0.01

            # Calculate recommended number of simulations for target precision
            if (power_estimate > 0 && power_estimate < 1) {
                recommended_n_sims <- ceiling(
                    power_estimate * (1 - power_estimate) / (target_mc_se^2)
                )
            } else {
                recommended_n_sims <- n_sims
            }

            # Determine convergence status
            converged <- mc_se <= target_mc_se

            # Generate convergence message
            if (converged) {
                message <- sprintf(
                    "Simulation converged (MC SE = %.4f, target = %.4f)",
                    mc_se, target_mc_se
                )
                recommendation <- "Simulation precision is adequate"
            } else {
                message <- sprintf(
                    "Simulation may need more runs (MC SE = %.4f, target = %.4f)",
                    mc_se, target_mc_se
                )
                recommendation <- sprintf(
                    "Consider increasing to %d simulations for better precision",
                    recommended_n_sims
                )
            }

            list(
                converged = converged,
                mc_se = mc_se,
                target_mc_se = target_mc_se,
                recommended_n_sims = recommended_n_sims,
                message = message,
                recommendation = recommendation
            )
        },
        .apply_clinical_preset = function() {
            # Apply clinical preset configurations
            preset <- self$options$clinical_preset

            if (is.null(preset) || preset == "custom") {
                return()
            }

            # self$options is read-only here, so a preset cannot be applied from
            # the backend: it has to write into the controls before the analysis
            # runs. jamovi/js/survivalPower.events.js does that and is the single
            # source of truth for the preset values.
            #
            # This block used to restate those values in prose. A stale copy of
            # the same table also sat in jamovi/survivalPower.events -- R source
            # under a name jamovi never loads -- and had drifted from the live
            # handler on median survival and the non-inferiority alpha. Both
            # copies are gone; do not reintroduce one. Read the .js file.
        },
        .generate_clinical_friendly_outputs = function() {
            # Generate clinical-friendly outputs based on user preferences

            # Early return if no clinical features enabled - performance optimization
            if (!any(self$options$show_summary, self$options$show_explanations,
                self$options$show_glossary, self$options$guided_mode,
                na.rm = TRUE
            )) {
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
            # The hazard ratio the analysis used. Reading the raw effect_size box
            # here made the summary state the opposite direction of effect
            # whenever effect_size_type was not "hazard_ratio": a median ratio of
            # 1.5 (HR 0.667, a benefit) was reported as "Large increase in risk".
            hr <- private$.get_effect_hr()
            power <- self$options$power_level

            # Create natural language summary
            summary_text <- paste0(
                "<div class='jmv-clinical-summary'>",
                "<h3> Plain Language Summary</h3>",
                "<p><strong>Study Design:</strong> ", private$.format_test_type(test_type),
                " for ", private$.format_primary_endpoint(self$options$primary_endpoint), "</p>",
                if (analysis_type == "sample_size") {
                    paste0(
                        "<p><strong>Sample Size Calculation:</strong> To detect a hazard ratio of ",
                        round(hr, 3), " (", ifelse(hr < 1, paste0(round((1 - hr) * 100), "% lower hazard"),
                            paste0(round((hr - 1) * 100), "% higher hazard")
                        ),
                        ") with ", round(power * 100), "% power at ", private$.alpha_pct(), "% significance level.</p>"
                    )
                } else if (analysis_type == "power") {
                    paste0(
                        "<p><strong>Power Calculation:</strong> With ", self$options$sample_size_input,
                        " subjects, the study has power to detect a hazard ratio of ", round(hr, 3), ".</p>"
                    )
                } else if (analysis_type == "effect_size") {
                    paste0(
                        "<p><strong>Effect Size:</strong> The minimum detectable effect with ",
                        self$options$sample_size_input, " subjects and ", round(power * 100), "% power.</p>"
                    )
                } else {
                    "<p><strong>Duration Analysis:</strong> Study timeline requirements for adequate power.</p>"
                },
                "<p><strong>Clinical Impact:</strong> ",
                # Describes the size of the assumed hazard ratio, not the clinical
                # value of the treatment: whether a given hazard reduction matters
                # depends on the absolute survival it buys and on the disease.
                if (hr < 0.67) {
                    "Large assumed effect"
                } else if (hr < 0.80) {
                    "Moderate assumed effect"
                } else if (hr < 1 - 1e-8) {
                    "Small assumed effect"
                } else if (hr < 1 + 1e-8) {
                    "No assumed difference between arms"
                } else if (hr < 1.25) {
                    "Small assumed increase in hazard"
                } else if (hr < 1.5) {
                    "Moderate assumed increase in hazard"
                } else {
                    "Large assumed increase in hazard"
                },
                ". Whether this is clinically worthwhile depends on the absolute survival gain and the disease setting.</p>",
                # An HR derived from a median ratio, RMST difference or survival
                # difference is not the number in the effect size box; say so
                # rather than leaving the reader to reconcile the two.
                if (!isTRUE(self$options$effect_size_type == "hazard_ratio") &&
                    !is.null(private$effect_hr_info$note)) {
                    paste0("<p><strong>Effect Size Conversion:</strong> ",
                           private$effect_hr_info$note, "</p>")
                } else {
                    ""
                },
                "<p><strong>Study Timeline:</strong> ", self$options$accrual_period,
                " months recruitment + ", self$options$follow_up_period,
                " months follow-up = ", self$options$accrual_period + self$options$follow_up_period,
                " months total.</p>",
                "<div class='references' style='margin-top: 15px; padding-top: 10px; border-top: 1px solid #ddd; font-size: 0.9em;'>",
                "<p><strong>Methods:</strong> Sample size calculations based on ",
                if (test_type == "log_rank") {
                    "Schoenfeld (1983) log-rank test formula"
                } else if (test_type == "cox_regression") {
                    "gsDesign package (Anderson 2022) for Cox proportional hazards"
                } else if (test_type == "non_inferiority") {
                    "non-inferiority margin approach (Rothmann et al. 2003)"
                } else {
                    "standard survival analysis methods"
                },
                ". Exponential survival distribution assumed with uniform accrual pattern.</p>",
                "</div>",
                "</div>"
            )

            self$results$natural_language_summary$setContent(summary_text)
        },
        .generate_educational_explanations = function() {
            test_type <- self$options$test_type

            # Every entry shares this: the analysis plans a trial, it does not test one.
            reading <- "The result tells you how many subjects (or how much power) the design needs under these assumptions; it does not predict the trial's outcome."

            explanations <- list(
                log_rank = list(
                    what = "Compares the full survival curves of two groups using the log-rank test.",
                    when = "Use when the primary comparison is time to event between two randomised arms.",
                    assumptions = "Non-informative censoring. Proportional hazards is not required for the test to be valid, but the sample-size formula and the test's power assume it; with crossing or delayed curves the design is underpowered.",
                    interpretation = reading
                ),
                cox_regression = list(
                    what = "Sizes the treatment comparison in a Cox proportional hazards model.",
                    when = "Use when the analysis will report a hazard ratio. The calculation covers the two-arm treatment effect; covariate adjustment is not modelled.",
                    assumptions = "Proportional hazards over follow-up and non-informative censoring.",
                    interpretation = paste("The hazard ratio compares instantaneous event rates between the groups; it is not a relative risk.", reading)
                ),
                non_inferiority = list(
                    what = "Tests whether a new treatment is not unacceptably worse than the standard, judged against a pre-specified margin.",
                    when = "Use when the new treatment offers other advantages (toxicity, cost, convenience) and a small loss of efficacy is acceptable.",
                    assumptions = "A margin justified from historical evidence, a one-sided test, and a control effect that is stable over time (constancy).",
                    interpretation = paste("A successful trial shows the new treatment is not worse than the standard by more than the pre-specified margin; it does not show equivalence or superiority.", reading)
                )
            )

            current_explanation <- explanations[[test_type]]
            if (is.null(current_explanation)) {
                current_explanation <- explanations[["log_rank"]]
            }

            explanation_html <- paste0(
                "<div class='jmv-educational-guide'>",
                "<h3>Educational Guide: ", private$.format_test_type(test_type), "</h3>",
                "<div class='explanation-section'><h4>What does this test do?</h4><p>", current_explanation$what, "</p></div>",
                "<div class='explanation-section'><h4>When should you use it?</h4><p>", current_explanation$when, "</p></div>",
                "<div class='explanation-section'><h4>Key assumptions:</h4><p>", current_explanation$assumptions, "</p></div>",
                "<div class='explanation-section'><h4>How to read the result:</h4><p>", current_explanation$interpretation, "</p></div>",
                "</div>"
            )

            self$results$educational_explanations$setContent(explanation_html)
        },
        .generate_statistical_glossary = function() {
            # Generate glossary of statistical terms
            glossary_html <- paste0(
                "<div class='jmv-glossary'>",
                "<h3> Statistical Terms Glossary</h3>",
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
                "<p>Range of plausible values for the true effect. Over many repeated studies, 95% of the intervals constructed this way would contain the true value.</p>",
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
                "<p>The ratio of participants in the control group to each treatment group. 1:1 means equal groups; 2:1 means twice as many in the <em>control</em> group. To put more participants in the treatment group, enter a value below 1 (0.5 gives twice as many on treatment).</p>",
                "</div>",
                "</div>"
            )

            self$results$statistical_glossary$setContent(glossary_html)
        },
        # A static checklist. The previous version tracked "progress" by testing
        # options that always have defaults, so steps 1-3 could never show and
        # "Your analysis is complete!" appeared whenever a parameter left its
        # default. jamovi does not tell an analysis which steps a user has
        # reviewed, so this shows the current settings instead of pretending.
        .generate_guided_workflow = function() {
            step <- function(n, title, text) {
                paste0("<div class='workflow-step'><h4>Step ", n, ": ", title, "</h4><p>", text, "</p></div>")
            }
            workflow_html <- paste0(
                "<div class='jmv-guided-workflow'>",
                "<h3>Guided Analysis Workflow</h3>",
                step(1, "Choose what to calculate", paste0(
                    "Sample size, power, detectable effect, or study duration. Current: ",
                    private$.format_analysis_type(self$options$analysis_type), "."
                )),
                step(2, "Choose the statistical test", paste0(
                    "Match the test to the planned primary analysis. Current: ",
                    private$.format_test_type(self$options$test_type), "."
                )),
                step(3, "Set the design assumptions", paste0(
                    "Hazard ratio ", round(private$.get_effect_hr(), 3),
                    ", power ", round(self$options$power_level * 100), "%, alpha ",
                    self$options$alpha_level, ". Justify each from prior data or the literature."
                )),
                step(4, "Review the assumptions table",
                     "Check exponential survival, proportional hazards, dropout and accrual against your setting."),
                step(5, "Test robustness",
                     "Turn on sensitivity analysis to see how the result moves when the assumptions change."),
                "</div>"
            )
            self$results$guided_workflow$setContent(workflow_html)
        }
    )
)
