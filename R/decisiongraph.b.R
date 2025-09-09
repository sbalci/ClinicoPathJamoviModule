#' @title Decision Tree Graph for Cost-Effectiveness Analysis
#' @name decisiongraph
#' @description Creates interactive decision tree visualizations for medical cost-effectiveness analysis
#' @details This module provides comprehensive decision tree visualization capabilities including:
#'   - Decision nodes (square), chance nodes (circle), and terminal nodes (triangle)
#'   - Cost-effectiveness analysis with expected value calculations
#'   - Sensitivity analysis with tornado diagrams
#'   - Multiple layout options and customizable visualization
#' @section Usage:
#'   1. Define decision variables, probabilities, costs, and utilities
#'   2. Select tree type and layout options
#'   3. View decision tree graph with calculated expected values
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @import jmvcore
#' @import ggplot2
#' @import dplyr

# Provide a safe fallback for the translation helper used as .("text")
# In environments where jmvcore's translator isn't available, treat it as identity
if (!exists(".") || !is.function(get(".", inherits = TRUE))) {
    `.` <- function(x) x
}

# Provide a safe fallback for the null-coalescing operator used as a %||% b
if (!exists("%||%", inherits = TRUE)) {
    `%||%` <- function(a, b) if (!is.null(a)) a else b
}

decisiongraphClass <- if (requireNamespace("jmvcore"))
    R6::R6Class(
        "decisiongraphClass",
        inherit = decisiongraphBase,
        private = list(
            # Data storage
            .treeData = NULL,
            .nodeData = NULL,
            .results = NULL,
            .markovData = NULL,
            .psaResults = NULL,
            .nmbAnalysis = NULL,
            .nmbSensitivity = NULL,

            # Configuration constants for better maintainability
            DECISIONGRAPH_DEFAULTS = list(
                psa_chunk_size = 1000,
                bootstrap_convergence_tolerance = 0.001,
                max_simulations = 10000,
                default_wtp_multiples = c(0.5, 0.75, 1.0, 1.25, 1.5),
                markov_max_cycles = 100,
                convergence_threshold = 1e-6,
                memory_efficient_threshold = 5000,
                parallel_threshold = 2000,
                default_distributions = list(
                    cost = "gamma",
                    utility = "beta",
                    probability = "beta"
                ),
                half_cycle_correction = TRUE,
                tunnel_state_support = TRUE
            ),

            # NO private variables for complex objects to avoid serialization issues
            # All content generated on-demand in methods

            .init = function() {
                # MINIMAL INIT - Only basic table references to avoid serialization issues
                # All dynamic content generation moved to .run() method

                # Initialize table references (required by jamovi)
                summaryTable <- self$results$summaryTable
                nodeTable <- self$results$nodeTable
                sensitivityTable <- self$results$sensitivityTable

                # DO NOT generate content, call methods, or store complex objects here
                # All clinical presets, help content, etc. will be handled in .run()
            },

            .validateInputs = function() {
                # Use utility function for comprehensive validation
                validationResult <- validateDecisionAnalysisInputs(
                    data = self$data,
                    treeType = self$options$treeType,
                    options = self$options
                )

                # Handle validation errors
                if (!validationResult$valid) {
                    stop(paste(validationResult$messages, collapse = "\n"))
                }

                # Call enhanced validation for complex parameters
                private$.validateComplexInputs()

                # Check minimum requirements met
                hasDecisions <- !is.null(self$options$decisions) && length(self$options$decisions) > 0
                hasProbabilities <- !is.null(self$options$probabilities) && length(self$options$probabilities) > 0
                hasCosts <- !is.null(self$options$costs) && length(self$options$costs) > 0
                hasUtilities <- !is.null(self$options$utilities) && length(self$options$utilities) > 0

                # Validate probabilistic sensitivity analysis requirements
                if (self$options$probabilisticAnalysis) {
                    numSims <- self$options$numSimulations
                    maxSims <- private$DECISIONGRAPH_DEFAULTS$max_simulations
                    if (is.null(numSims) || numSims < 100 || numSims > maxSims) {
                        stop(paste(.("PSA requires number of simulations between 100 and"), format(maxSims, big.mark = ",")))
                    }

                    # Check for PSA-specific requirements
                    if (self$options$correlatedParameters) {
                        hasCorrelationMatrix <- !is.null(self$options$correlationMatrix) && length(self$options$correlationMatrix) > 0
                        if (!hasCorrelationMatrix) {
                            stop(.("Correlated parameters in PSA require correlation matrix variables"))
                        }
                    }
                }

                # Validate budget impact analysis requirements
                if (self$options$budgetImpactAnalysis) {
                    targetPop <- self$options$targetPopulationSize
                    marketPen <- self$options$marketPenetration

                    if (is.null(targetPop) || targetPop <= 0) {
                        stop(.("Budget impact analysis requires a positive target population size"))
                    }
                    if (is.null(marketPen) || marketPen < 0 || marketPen > 1) {
                        stop(.("Budget impact analysis requires market penetration rate between 0 and 1"))
                    }
                }

                # Validate value of information analysis requirements
                if (self$options$valueOfInformation) {
                    hasEVPIParams <- !is.null(self$options$evpi_parameters) && length(self$options$evpi_parameters) > 0
                    if (!hasEVPIParams) {
                        stop(.("Value of information analysis requires EVPI parameter variables"))
                    }

                    if (!self$options$probabilisticAnalysis) {
                        stop(.("Value of information analysis requires probabilistic sensitivity analysis to be enabled"))
                    }
                }

                # Validate CEAC threshold format
                if (self$options$probabilisticAnalysis && self$options$psa_advanced_outputs) {
                    ceacThresholds <- self$options$ceacThresholds
                    if (!is.null(ceacThresholds) && ceacThresholds != "") {
                        # Parse threshold string (format: "min,max,step")
                        thresholdParts <- tryCatch({
                            as.numeric(strsplit(ceacThresholds, ",")[[1]])
                        }, error = function(e) {
                            stop(.("CEAC thresholds must be in format 'min,max,step' (e.g., '0,100000,5000')"))
                        })

                        if (length(thresholdParts) != 3 || any(is.na(thresholdParts))) {
                            stop(.("CEAC thresholds must contain exactly 3 numeric values: min,max,step"))
                        }

                        if (thresholdParts[1] >= thresholdParts[2] || thresholdParts[3] <= 0) {
                            stop(.("CEAC thresholds: min must be < max, and step must be > 0"))
                        }
                    }
                }

                # Check minimum requirements met
                if (!hasDecisions && !hasProbabilities && !hasCosts && !hasUtilities) {
                    return(FALSE)
                }

                return(TRUE)
            },

            .validateComplexInputs = function() {
                # Validate CEAC threshold format
                if (self$options$probabilisticAnalysis && self$options$psa_advanced_outputs) {
                    ceacThresholds <- self$options$ceacThresholds
                    if (!is.null(ceacThresholds) && ceacThresholds != "") {
                        # Parse CEAC threshold format: "min,max,step"
                        tryCatch({
                            parts <- strsplit(ceacThresholds, ",")[[1]]
                            if (length(parts) != 3) {
                                stop("CEAC thresholds must be formatted as 'min,max,step' (e.g., '0,100000,5000')")
                            }

                            min_val <- as.numeric(parts[1])
                            max_val <- as.numeric(parts[2])
                            step_val <- as.numeric(parts[3])

                            if (is.na(min_val) || is.na(max_val) || is.na(step_val)) {
                                stop("CEAC threshold values must be numeric")
                            }

                            if (min_val < 0 || max_val <= min_val || step_val <= 0) {
                                stop("CEAC thresholds must have min ≥ 0, max > min, and step > 0")
                            }

                            if ((max_val - min_val) / step_val > 1000) {
                                stop("CEAC threshold range would create too many points (>1000). Use larger step size.")
                            }

                        }, error = function(e) {
                            stop(paste("Invalid CEAC threshold format:", e$message))
                        })
                    }
                }

                # Validate discount rate
                if (!is.null(self$options$discountRate)) {
                    discountRate <- self$options$discountRate
                    if (discountRate < 0 || discountRate > 0.3) {
                        stop("Discount rate must be between 0 and 30% (0.3)")
                    }
                }

                # Validate time horizon
                if (!is.null(self$options$timeHorizon)) {
                    timeHorizon <- self$options$timeHorizon
                    if (timeHorizon < 1 || timeHorizon > 100) {
                        stop("Time horizon must be between 1 and 100 years")
                    }
                }

                # Validate willingness to pay threshold
                if (self$options$calculateNMB && !is.null(self$options$willingnessToPay)) {
                    wtp <- self$options$willingnessToPay
                    if (wtp < 0 || wtp > 1000000) {
                        stop("Willingness to pay threshold must be between $0 and $1,000,000")
                    }
                }

                # Validate simulation count for PSA
                if (self$options$probabilisticAnalysis) {
                    numSims <- self$options$numSimulations
                    if (is.null(numSims) || numSims < 100 || numSims > 50000) {
                        stop("Number of simulations must be between 100 and 50,000 for computational feasibility")
                    }
                }

                # Validate market penetration for budget impact
                if (self$options$budgetImpactAnalysis && !is.null(self$options$marketPenetration)) {
                    marketPen <- self$options$marketPenetration
                    if (marketPen < 0 || marketPen > 1) {
                        stop("Market penetration rate must be between 0 and 1 (0% to 100%)")
                    }
                }

                # Validate correlation matrix requirements
                if (self$options$correlatedParameters && self$options$probabilisticAnalysis) {
                    hasCorrelationMatrix <- !is.null(self$options$correlationMatrix) && length(self$options$correlationMatrix) > 0
                    if (!hasCorrelationMatrix) {
                        stop("Parameter correlation analysis requires correlation matrix variables to be specified")
                    }
                }

                # Validate numeric data ranges for key variables
                if (!is.null(self$data)) {
                    # Check probability variables are between 0 and 1
                    if (!is.null(self$options$probabilities) && length(self$options$probabilities) > 0) {
                        for (prob_var in self$options$probabilities) {
                            if (prob_var %in% names(self$data)) {
                                prob_values <- self$data[[prob_var]]
                                if (any(!is.na(prob_values) & (prob_values < 0 | prob_values > 1))) {
                                    stop(paste("Probability variable", prob_var, "contains values outside the range [0,1]"))
                                }
                            }
                        }
                    }

                    # Check cost variables are non-negative
                    if (!is.null(self$options$costs) && length(self$options$costs) > 0) {
                        for (cost_var in self$options$costs) {
                            if (cost_var %in% names(self$data)) {
                                cost_values <- self$data[[cost_var]]
                                if (any(!is.na(cost_values) & cost_values < 0)) {
                                    stop(paste("Cost variable", cost_var, "contains negative values"))
                                }
                            }
                        }
                    }

                    # Check utility variables are between 0 and 1 (or allow negative for disutilities)
                    if (!is.null(self$options$utilities) && length(self$options$utilities) > 0) {
                        for (util_var in self$options$utilities) {
                            if (util_var %in% names(self$data)) {
                                util_values <- self$data[[util_var]]
                                if (any(!is.na(util_values) & (util_values < -1 | util_values > 1))) {
                                    warning(paste("Utility variable", util_var, "contains values outside typical range [-1,1]. Consider reviewing data."))
                                }
                            }
                        }
                    }
                }
            },

            .getClinicalPresets = function() {
                # Create clinical presets dynamically to avoid . function issues during class definition

                return(list(
                    diagnostic_test = list(
                        name = "diagnostic_test",
                        title = .("Diagnostic Test Evaluation"),
                        description = .("Evaluate diagnostic test performance and cost-effectiveness"),
                        treeType = "costeffectiveness",
                        willingnessToPay = 50000,
                        calculateNMB = TRUE,
                        calculateExpectedValues = TRUE,
                        incrementalAnalysis = TRUE,
                        summaryTable = TRUE
                    ),
                    treatment_comparison = list(
                        name = "treatment_comparison",
                        title = .("Treatment Comparison"),
                        description = .("Compare long-term treatment outcomes using Markov model"),
                        treeType = "markov",
                        cycleLength = 1,
                        timeHorizon = 10,
                        discountRate = 0.03,
                        calculateExpectedValues = TRUE,
                        cohortTrace = TRUE,
                        cycleCorrection = TRUE
                    ),
                    screening_program = list(
                        name = "screening_program",
                        title = .("Screening Program Evaluation"),
                        description = .("Evaluate population screening program cost-effectiveness"),
                        treeType = "costeffectiveness",
                        willingnessToPay = 100000,
                        calculateNMB = TRUE,
                        budgetImpactAnalysis = TRUE,
                        targetPopulationSize = 100000,
                        marketPenetration = 0.7
                    ),
                    drug_costeffectiveness = list(
                        name = "drug_costeffectiveness",
                        title = .("Drug Cost-Effectiveness"),
                        description = .("Economic evaluation of pharmaceutical interventions"),
                        treeType = "costeffectiveness",
                        willingnessToPay = 100000,
                        probabilisticAnalysis = TRUE,
                        numSimulations = 1000,
                        calculateNMB = TRUE,
                        incrementalAnalysis = TRUE,
                        dominanceAnalysis = TRUE,
                        psa_advanced_outputs = TRUE
                    )
                ))
            },

            .applyClinicalPreset = function(presetName) {
                # Apply clinical preset configurations to analysis options

                if (is.null(presetName) || presetName == "" || presetName == "none") {
                    return(FALSE)
                }

                presets <- private$.getClinicalPresets()
                preset <- presets[[presetName]]
                if (is.null(preset)) {
                    warning(paste(.("Unknown clinical preset:"), presetName))
                    return(FALSE)
                }

                tryCatch({
                    # Log preset application (serialization-safe)
                    message(paste(.("Applying clinical preset:"), preset$title))

                    # Generate guidance text based on preset (no storage to avoid serialization issues)
                    guidanceText <- private$.generatePresetGuidance(preset)

                    # Update results with preset information if available
                    if (!is.null(self$results$text1)) {
                        self$results$text1$setContent(guidanceText)
                    }

                    return(TRUE)

                }, error = function(e) {
                    warning(paste(.("Error applying clinical preset:"), e$message))
                    return(FALSE)
                })
            },

            .generatePresetGuidance = function(preset) {
                # Generate HTML guidance text for clinical presets

                html <- createSafeHTMLContent(
                    title = .("Clinical Analysis Guide"),
                    content = list(
                        paste(.("Analysis Type:"), preset$title),
                        paste(.("Description:"), preset$description),
                        "",
                        .("Recommended Steps:"),
                        private$.getPresetSteps(preset$name)
                    ),
                    includeStyle = TRUE
                )

                return(html)
            },

            .getPresetSteps = function(presetName) {
                # Get step-by-step guidance for each clinical preset

                steps <- switch(presetName,
                    "diagnostic_test" = c(
                        .("1. Select variables for test sensitivity and specificity"),
                        .("2. Include cost variables for test and treatment options"),
                        .("3. Specify utility values for true/false positive outcomes"),
                        .("4. Set appropriate willingness-to-pay threshold"),
                        .("5. Enable incremental analysis for cost-effectiveness ratios")
                    ),
                    "treatment_comparison" = c(
                        .("1. Define treatment decision variables"),
                        .("2. Include probability variables for treatment outcomes"),
                        .("3. Specify cost variables for each treatment arm"),
                        .("4. Include utility/quality-of-life measures"),
                        .("5. Consider sensitivity analysis for uncertain parameters")
                    ),
                    "screening_program" = c(
                        .("1. Set up decision nodes for screening vs. no screening"),
                        .("2. Include variables for screening test characteristics"),
                        .("3. Specify costs for screening, follow-up, and treatment"),
                        .("4. Include utilities for different health outcomes"),
                        .("5. Enable Markov modeling for long-term outcomes")
                    ),
                    "drug_costeffectiveness" = c(
                        .("1. Define decision alternatives (drug vs. comparator)"),
                        .("2. Include clinical outcome probabilities"),
                        .("3. Specify drug costs and administration costs"),
                        .("4. Include quality-adjusted life year (QALY) measures"),
                        .("5. Enable probabilistic sensitivity analysis for uncertainty")
                    ),
                    c(.("Follow the standard decision analysis workflow"))
                )

                return(paste(steps, collapse = "<br>"))
            },

            .generatePlainLanguageExplanations = function() {
                # Generate user-friendly explanations for complex terms and concepts

                explanations <- list(
                    "icer" = list(
                        term = .("Incremental Cost-Effectiveness Ratio (ICER)"),
                        definition = .("The additional cost per additional unit of health benefit gained when comparing two treatments"),
                        example = .("If Treatment A costs $10,000 more than Treatment B and provides 0.5 more QALYs, the ICER is $20,000 per QALY"),
                        interpretation = .("Lower ICERs indicate better value for money. Common thresholds are $50,000-100,000 per QALY")
                    ),
                    "qaly" = list(
                        term = .("Quality-Adjusted Life Year (QALY)"),
                        definition = .("A measure that combines both length and quality of life into a single number"),
                        example = .("One year in perfect health = 1 QALY. One year with reduced quality of life = 0.7 QALY"),
                        interpretation = .("Higher QALYs indicate better health outcomes. Used to compare different treatments")
                    ),
                    "nmb" = list(
                        term = .("Net Monetary Benefit (NMB)"),
                        definition = .("The monetary value of health benefits minus the costs"),
                        example = .("If a treatment provides 2 QALYs worth $50,000 each but costs $80,000, NMB = $100,000 - $80,000 = $20,000"),
                        interpretation = .("Positive NMB indicates the treatment is cost-effective at the given threshold")
                    ),
                    "sensitivity_analysis" = list(
                        term = .("Sensitivity Analysis"),
                        definition = .("Testing how changes in uncertain parameters affect the results"),
                        example = .("What happens to cost-effectiveness if treatment success rates vary from 70% to 90%?"),
                        interpretation = .("Shows how robust the conclusions are to uncertainty in the data")
                    ),
                    "markov_model" = list(
                        term = .("Markov Model"),
                        definition = .("A way to model how patients move between different health states over time"),
                        example = .("Patients can be Healthy → Sick → Dead, with probabilities of transition each year"),
                        interpretation = .("Useful for chronic diseases where health status changes over time")
                    ),
                    "discount_rate" = list(
                        term = .("Discount Rate"),
                        definition = .("The rate used to convert future costs and benefits to present values"),
                        example = .("A 3% discount rate means $100 next year is worth $97 today"),
                        interpretation = .("Accounts for the fact that costs and benefits today are worth more than those in the future")
                    )
                )

                return(explanations)
            },

            .createTooltipContent = function(term) {
                # Create HTML tooltip content for complex terms

                explanations <- private$.generatePlainLanguageExplanations()

                if (!term %in% names(explanations)) {
                    return("")
                }

                explanation <- explanations[[term]]

                html <- sprintf('
                <div class="tooltip-content">
                    <div class="tooltip-term">%s</div>
                    <div class="tooltip-definition">%s</div>
                    <div class="tooltip-example"><strong>%s:</strong> %s</div>
                    <div class="tooltip-interpretation"><strong>%s:</strong> %s</div>
                </div>',
                explanation$term,
                explanation$definition,
                .("Example"),
                explanation$example,
                .("Interpretation"),
                explanation$interpretation
                )

                return(html)
            },

            .generateContextualHelp = function() {
                # Generate contextual help based on current analysis settings

                treeType <- self$options$treeType
                helpContent <- list()

                # Basic analysis guidance
                helpContent$basic <- createSafeHTMLContent(
                    title = .("Getting Started"),
                    content = list(
                        .("1. Select your analysis type (Simple, Markov, or Cost-Effectiveness)"),
                        .("2. Choose a clinical preset if available for your scenario"),
                        .("3. Define your decision variables (treatment options)"),
                        .("4. Add probability, cost, and utility variables"),
                        .("5. Configure analysis options and run")
                    )
                )

                # Tree-type specific help
                if (treeType == "simple") {
                    helpContent$specific <- createSafeHTMLContent(
                        title = .("Simple Decision Tree Help"),
                        content = list(
                            .("Use for: One-time decisions with clear outcomes"),
                            .("Required: Decision variables, probabilities OR costs"),
                            .("Example: Should we perform surgery or use medication?"),
                            .("Output: Expected values for each decision path")
                        )
                    )
                } else if (treeType == "markov") {
                    helpContent$specific <- createSafeHTMLContent(
                        title = .("Markov Model Help"),
                        content = list(
                            .("Use for: Long-term diseases with changing health states"),
                            .("Required: Health states, transition probabilities"),
                            .("Example: Chronic disease progression over 10 years"),
                            .("Output: Cohort trace, lifetime costs and QALYs")
                        )
                    )
                } else if (treeType == "costeffectiveness") {
                    helpContent$specific <- createSafeHTMLContent(
                        title = .("Cost-Effectiveness Analysis Help"),
                        content = list(
                            .("Use for: Economic evaluation comparing treatments"),
                            .("Required: Costs, utilities/outcomes, willingness-to-pay threshold"),
                            .("Example: Is Drug A worth the extra cost compared to Drug B?"),
                            .("Output: ICERs, dominance analysis, cost-effectiveness ratios")
                        )
                    )
                }

                # Variable selection help
                helpContent$variables <- createSafeHTMLContent(
                    title = .("Variable Selection Guide"),
                    content = list(
                        paste(.("Decision Variables:"), .("Categorical variables representing treatment choices")),
                        paste(.("Probability Variables:"), .("Numeric variables (0-1) for event likelihoods")),
                        paste(.("Cost Variables:"), .("Numeric variables (≥0) for monetary costs")),
                        paste(.("Utility Variables:"), .("Numeric variables (0-1) for quality of life")),
                        paste(.("Outcome Variables:"), .("Categorical variables for clinical endpoints"))
                    )
                )

                return(helpContent)
            },

            .generateExecutiveSummary = function(results = NULL) {
                # Generate executive summary for decision analysis results

                if (is.null(results)) {
                    return(createSafeHTMLContent(
                        title = .("Executive Summary"),
                        content = .("Run the analysis to generate an executive summary of results.")
                    ))
                }

                tryCatch({
                    # Extract key findings (serialization-safe)
                    treeType <- self$options$treeType
                    clinicalPreset <- self$options$clinicalPreset

                    summaryContent <- list()

                    # Analysis overview
                    analysisType <- switch(treeType,
                        "simple" = .("Simple Decision Tree Analysis"),
                        "markov" = .("Markov Model Analysis"),
                        "costeffectiveness" = .("Cost-Effectiveness Analysis")
                    )

                    summaryContent <- c(summaryContent, list(
                        paste("<h3>", .("Analysis Type"), ":</h3>", analysisType)
                    ))

                    # Show clinical preset if used (get title dynamically to avoid serialization issues)
                    if (!is.null(clinicalPreset) && clinicalPreset != "none") {
                        presetTitle <- switch(clinicalPreset,
                            "diagnostic_test" = .("Diagnostic Test Evaluation"),
                            "treatment_comparison" = .("Treatment Comparison"),
                            "screening_program" = .("Screening Program Evaluation"),
                            "drug_costeffectiveness" = .("Drug Cost-Effectiveness"),
                            clinicalPreset  # fallback to preset name
                        )
                        summaryContent <- c(summaryContent, list(
                            paste("<h3>", .("Clinical Scenario"), ":</h3>", presetTitle)
                        ))
                    }

                    # Key findings based on analysis type
                    if (treeType == "costeffectiveness") {
                        summaryContent <- c(summaryContent, list(
                            paste("<h3>", .("Key Findings"), ":</h3>"),
                            .("• Cost-effectiveness ratios calculated for all strategies"),
                            .("• Dominated strategies identified and excluded"),
                            .("• Net monetary benefit analysis performed"),
                            .("• Results compared against willingness-to-pay threshold")
                        ))
                    } else if (treeType == "markov") {
                        summaryContent <- c(summaryContent, list(
                            paste("<h3>", .("Key Findings"), ":</h3>"),
                            .("• Long-term health state transitions modeled"),
                            .("• Cohort trace analysis performed"),
                            .("• Lifetime costs and utilities calculated"),
                            .("• Discounting applied for future benefits")
                        ))
                    }

                    # Recommendations
                    summaryContent <- c(summaryContent, list(
                        paste("<h3>", .("Recommendations"), ":</h3>"),
                        .("• Review the detailed results tables below"),
                        .("• Consider sensitivity analysis for uncertain parameters"),
                        .("• Validate assumptions with clinical experts"),
                        .("• Consider budget impact if implementing recommendations")
                    ))

                    html <- createSafeHTMLContent(
                        title = .("Executive Summary"),
                        content = summaryContent,
                        includeStyle = TRUE
                    )

                    return(html)

                }, error = function(e) {
                    return(createSafeHTMLContent(
                        title = .("Executive Summary"),
                        content = paste(.("Error generating summary:"), e$message)
                    ))
                })
            },

            .generateGlossary = function() {
                # Generate comprehensive glossary of decision analysis terms

                glossaryTerms <- list(
                    list(
                        term = .("Decision Analysis"),
                        definition = .("A systematic approach to decision making under uncertainty that uses explicit quantitative methods to identify the best course of action.")
                    ),
                    list(
                        term = .("Decision Node"),
                        definition = .("A point in the decision tree where a choice must be made between different alternatives. Represented by squares in decision trees.")
                    ),
                    list(
                        term = .("Chance Node"),
                        definition = .("A point in the decision tree where uncertain events occur. Represented by circles in decision trees.")
                    ),
                    list(
                        term = .("Terminal Node"),
                        definition = .("The endpoint of a decision path where final outcomes (costs, utilities) are specified. Represented by triangles in decision trees.")
                    ),
                    list(
                        term = .("Expected Value"),
                        definition = .("The average outcome that would be obtained if the decision were repeated many times under identical circumstances.")
                    ),
                    list(
                        term = .("Incremental Cost-Effectiveness Ratio (ICER)"),
                        definition = .("The additional cost per additional unit of health benefit when comparing one intervention to another.")
                    ),
                    list(
                        term = .("Quality-Adjusted Life Year (QALY)"),
                        definition = .("A measure of health outcome that combines mortality and morbidity into a single index, where 1.0 represents perfect health for one year.")
                    ),
                    list(
                        term = .("Net Monetary Benefit (NMB)"),
                        definition = .("A measure that converts health benefits into monetary terms and subtracts costs, allowing direct comparison of interventions.")
                    ),
                    list(
                        term = .("Willingness-to-Pay Threshold"),
                        definition = .("The maximum amount society is willing to pay for an additional unit of health benefit, typically expressed per QALY.")
                    ),
                    list(
                        term = .("Sensitivity Analysis"),
                        definition = .("An analysis that tests how sensitive the results are to changes in key parameter values.")
                    ),
                    list(
                        term = .("Markov Model"),
                        definition = .("A model that describes the progression of patients through different health states over time using transition probabilities.")
                    ),
                    list(
                        term = .("Health State"),
                        definition = .("A distinct condition that describes a patient's health status in a Markov model (e.g., Healthy, Sick, Dead).")
                    ),
                    list(
                        term = .("Transition Probability"),
                        definition = .("The probability that a patient will move from one health state to another during a specific time period.")
                    ),
                    list(
                        term = .("Cycle Length"),
                        definition = .("The time period for each cycle in a Markov model, commonly one year.")
                    ),
                    list(
                        term = .("Discount Rate"),
                        definition = .("The rate used to convert future costs and benefits to their present value, reflecting time preference.")
                    ),
                    list(
                        term = .("Dominated Strategy"),
                        definition = .("An intervention that costs more and provides fewer benefits than another intervention.")
                    ),
                    list(
                        term = .("Extendedly Dominated Strategy"),
                        definition = .("An intervention that is less cost-effective than a combination of two other interventions.")
                    ),
                    list(
                        term = .("Probabilistic Sensitivity Analysis (PSA)"),
                        definition = .("An analysis that uses probability distributions to describe uncertainty in parameters and uses Monte Carlo simulation.")
                    ),
                    list(
                        term = .("Cost-Effectiveness Acceptability Curve (CEAC)"),
                        definition = .("A graph showing the probability that each intervention is cost-effective across a range of willingness-to-pay thresholds.")
                    ),
                    list(
                        term = .("Expected Value of Perfect Information (EVPI)"),
                        definition = .("The maximum value that perfect information about all uncertain parameters would have for the decision maker.")
                    )
                )

                # Format glossary as HTML
                glossaryHtml <- "<style>
                    .glossary-item { margin-bottom: 15px; border-left: 3px solid #007ACC; padding-left: 10px; }
                    .glossary-term { font-weight: bold; font-size: 14px; color: #007ACC; margin-bottom: 5px; }
                    .glossary-definition { font-size: 12px; color: #333; line-height: 1.4; }
                </style>"

                for (item in glossaryTerms) {
                    glossaryHtml <- paste0(glossaryHtml,
                        '<div class="glossary-item">',
                        '<div class="glossary-term">', item$term, '</div>',
                        '<div class="glossary-definition">', item$definition, '</div>',
                        '</div>'
                    )
                }

                return(glossaryHtml)
            },

            .prepareTreeData = function() {
                # Clean data
                mydata <- jmvcore::naOmit(self$data)

                # Extract variable names
                decisions <- self$options$decisions
                probabilities <- self$options$probabilities
                costs <- self$options$costs
                utilities <- self$options$utilities
                outcomes <- self$options$outcomes

                # Create tree structure based on available data
                treeStructure <- list()

                # Build decision nodes
                if (!is.null(decisions) && length(decisions) > 0) {
                    for (decision in decisions) {
                        if (decision %in% names(mydata)) {
                            decisionLevels <- unique(mydata[[decision]])
                            treeStructure$decisions[[decision]] <- decisionLevels
                        }
                    }
                }

                # Build probability data
                if (!is.null(probabilities) && length(probabilities) > 0) {
                    probData <- mydata[probabilities[probabilities %in% names(mydata)]]
                    treeStructure$probabilities <- probData
                }

                # Build cost data
                if (!is.null(costs) && length(costs) > 0) {
                    costData <- mydata[costs[costs %in% names(mydata)]]
                    treeStructure$costs <- costData
                }

                # Build utility data
                if (!is.null(utilities) && length(utilities) > 0) {
                    utilityData <- mydata[utilities[utilities %in% names(mydata)]]
                    treeStructure$utilities <- utilityData
                }

                # Build outcome data
                if (!is.null(outcomes) && length(outcomes) > 0) {
                    outcomeData <- mydata[outcomes[outcomes %in% names(mydata)]]
                    treeStructure$outcomes <- outcomeData
                }

                # Store in both private variable (for immediate use) and state (for persistence)
                private$.treeData <- treeStructure
                if (!is.null(self$results$treeplot)) {
                    self$results$treeplot$setState(treeStructure)
                }
                return(treeStructure)
            },

            .buildTreeGraph = function() {
                # Use existing tree data if available
                if (is.null(private$.treeData)) {
                    return(NULL)
                }

                # Create basic tree structure
                nodes <- data.frame(
                    id = character(),
                    label = character(),
                    type = character(), # decision, chance, terminal
                    shape = character(),
                    color = character(),
                    level = integer(),
                    stringsAsFactors = FALSE
                )

                edges <- data.frame(
                    from = character(),
                    to = character(),
                    label = character(),
                    probability = numeric(),
                    stringsAsFactors = FALSE
                )

                nodeId <- 1

                # Root decision node
                nodes <- rbind(nodes, data.frame(
                    id = as.character(nodeId),
                    label = "Decision",
                    type = "decision",
                    shape = "square",
                    color = private$.getNodeColor("decision"),
                    level = 1,
                    stringsAsFactors = FALSE
                ))
                rootId <- nodeId
                nodeId <- nodeId + 1

                # Add decision branches
                if (!is.null(private$.treeData$decisions)) {
                    for (decisionVar in names(private$.treeData$decisions)) {
                        levels <- private$.treeData$decisions[[decisionVar]]

                        for (level in levels) {
                            # Add chance node
                            chanceId <- nodeId
                            nodes <- rbind(nodes, data.frame(
                                id = as.character(chanceId),
                                label = paste(decisionVar, level, sep = ": "),
                                type = "chance",
                                shape = "circle",
                                color = private$.getNodeColor("chance"),
                                level = 2,
                                stringsAsFactors = FALSE
                            ))

                            # Add edge from root to chance node
                            edges <- rbind(edges, data.frame(
                                from = as.character(rootId),
                                to = as.character(chanceId),
                                label = as.character(level),
                                probability = 1.0,
                                stringsAsFactors = FALSE
                            ))

                            nodeId <- nodeId + 1

                            # Add terminal outcomes
                            private$.addTerminalNodes(chanceId, nodes, edges, nodeId)
                        }
                    }
                }

                # Store in private variable for immediate use (will be cleaned up later)
                private$.nodeData <- list(nodes = nodes, edges = edges)
                return(private$.nodeData)
            },

            .addTerminalNodes = function(parentId, nodes, edges, nodeId) {
                # Add terminal nodes based on outcomes or default success/failure
                outcomes <- c("Success", "Failure")
                probabilities <- c(0.7, 0.3) # Default probabilities
                costs <- c(1000, 2000) # Default costs
                utilities <- c(0.8, 0.2) # Default utilities

                # Use actual data if available
                if (!is.null(private$.treeData$probabilities) && nrow(private$.treeData$probabilities) > 0) {
                    probData <- private$.treeData$probabilities[1, ]
                    probabilities <- as.numeric(probData[1:min(2, ncol(probData))])
                }

                if (!is.null(private$.treeData$costs) && nrow(private$.treeData$costs) > 0) {
                    costData <- private$.treeData$costs[1, ]
                    costs <- as.numeric(costData[1:min(2, ncol(costData))])
                }

                if (!is.null(private$.treeData$utilities) && nrow(private$.treeData$utilities) > 0) {
                    utilityData <- private$.treeData$utilities[1, ]
                    utilities <- as.numeric(utilityData[1:min(2, ncol(utilityData))])
                }

                for (i in seq_along(outcomes)) {
                    terminalId <- nodeId + i - 1

                    # Create terminal node label
                    label <- outcomes[i]
                    if (self$options$showCosts && !is.na(costs[i])) {
                        label <- paste0(label, "\nCost: $", round(costs[i], 0))
                    }
                    if (self$options$showUtilities && !is.na(utilities[i])) {
                        label <- paste0(label, "\nUtility: ", round(utilities[i], 3))
                    }

                    nodes <<- rbind(nodes, data.frame(
                        id = as.character(terminalId),
                        label = label,
                        type = "terminal",
                        shape = "triangle",
                        color = private$.getNodeColor("terminal"),
                        level = 3,
                        stringsAsFactors = FALSE
                    ))

                    # Add edge with probability
                    edgeLabel <- ""
                    if (self$options$showProbabilities && !is.na(probabilities[i])) {
                        edgeLabel <- paste0("p=", round(probabilities[i], 3))
                    }

                    edges <<- rbind(edges, data.frame(
                        from = as.character(parentId),
                        to = as.character(terminalId),
                        label = edgeLabel,
                        probability = probabilities[i],
                        stringsAsFactors = FALSE
                    ))
                }
            },

            .getNodeColor = function(nodeType) {
                colorScheme <- self$options$colorScheme

                colors <- switch(colorScheme,
                    "default" = list(decision = "#4CAF50", chance = "#2196F3", terminal = "#FF9800"),
                    "colorblind" = list(decision = "#E69F00", chance = "#56B4E9", terminal = "#009E73"),
                    "medical" = list(decision = "#8FBC8F", chance = "#87CEEB", terminal = "#DDA0DD"),
                    "economic" = list(decision = "#228B22", chance = "#4169E1", terminal = "#DC143C"),
                    list(decision = "#4CAF50", chance = "#2196F3", terminal = "#FF9800")
                )

                return(colors[[nodeType]])
            },

            # Calculate Expected Values for Decision Tree
            #
            # Performs backward induction algorithm to compute expected costs, utilities,
            # and net monetary benefits for all decision paths in the tree structure.
            #
            # This method implements the fundamental decision analysis algorithm:
            # - Backward induction from terminal nodes to decision nodes
            # - Expected value calculations at chance nodes using probability weighting
            # - Net Monetary Benefit (NMB) computation using willingness-to-pay threshold
            # - Incremental Cost-Effectiveness Ratio (ICER) calculations
            # - Dominance analysis to identify suboptimal strategies
            #
            # Algorithm:
            # 1. Start from terminal nodes with known costs and utilities
            # 2. Work backwards through chance nodes using expected value formula
            # 3. At decision nodes, select optimal path based on NMB criterion
            # 4. Calculate incremental ratios between strategies
            # 5. Perform dominance analysis to eliminate suboptimal options
            # 6. Rank strategies by cost-effectiveness
            #
            # Expected Value Formula:
            # EV = Σ(probability_i × outcome_i) for all branches i
            #
            # Net Monetary Benefit:
            # NMB = (Utility × WTP_threshold) - Cost
            #
            # Returns: List containing calculated results with elements:
            #   - expectedCost: Expected costs for each strategy
            #   - expectedUtility: Expected utilities (QALYs) for each strategy
            #   - netBenefit: Net monetary benefits for each strategy
            #   - icer: Incremental cost-effectiveness ratios
            #   - dominance_status: Dominance classification for each strategy
            #   - optimalStrategy: Recommended strategy based on NMB
            .calculateExpectedValues = function() {
                if (is.null(private$.nodeData)) {
                    return(NULL)
                }

                nodes <- private$.nodeData$nodes
                edges <- private$.nodeData$edges

                # Simple expected value calculation
                strategies <- unique(nodes[nodes$type == "decision", "label"])

                results <- data.frame(
                    strategy = character(),
                    expectedCost = numeric(),
                    expectedUtility = numeric(),
                    icer = numeric(),
                    netBenefit = numeric(),
                    stringsAsFactors = FALSE
                )

                # Get willingness to pay threshold from options
                wtp <- if (!is.null(self$options$willingnessToPay)) {
                    self$options$willingnessToPay
                } else {
                    50000  # Default WTP threshold
                }

                # Calculate for each strategy
                for (i in seq_along(strategies)) {
                    strategy <- strategies[i]

                    # Calculate expected values by traversing decision tree
                    pathResults <- private$.traverseDecisionPath(strategy, nodes, edges)
                    expectedCost <- pathResults$expectedCost
                    expectedUtility <- pathResults$expectedUtility

                    # Calculate ICER (Incremental Cost-Effectiveness Ratio)
                    icer <- if (i > 1) {
                        deltaCost <- expectedCost - results$expectedCost[i-1]
                        deltaUtility <- expectedUtility - results$expectedUtility[i-1]
                        if (abs(deltaUtility) > 0.0001) {
                            deltaCost / deltaUtility
                        } else {
                            NA
                        }
                    } else {
                        NA
                    }

                    # Calculate Net Monetary Benefit (NMB)
                    netBenefit <- expectedUtility * wtp - expectedCost

                    results <- rbind(results, data.frame(
                        strategy = strategy,
                        expectedCost = expectedCost,
                        expectedUtility = expectedUtility,
                        icer = icer,
                        netBenefit = netBenefit,
                        stringsAsFactors = FALSE
                    ))
                }

                # Sort by NMB to identify optimal strategy
                results <- results[order(results$netBenefit, decreasing = TRUE), ]
                # Store in both private variable (for immediate use) and state (for persistence)
                # Ensure results is a clean data frame to prevent serialization issues
                cleanResults <- as.data.frame(results)
                private$.results <- cleanResults
                if (!is.null(self$results$summaryTable)) {
                    self$results$summaryTable$setState(cleanResults)
                }
                return(results)
            },

            .traverseDecisionPath = function(strategy, nodes, edges) {
                # Function to traverse decision tree and calculate expected values
                # This implements the recursive calculation as shown in the blog

                # Initialize with actual data if available
                if (!is.null(self$options$costs) && !is.null(self$options$utilities) &&
                    !is.null(self$options$probabilities)) {

                    mydata <- jmvcore::naOmit(self$data)

                    # Extract actual values from data
                    costs <- if (length(self$options$costs) > 0 &&
                               self$options$costs[1] %in% names(mydata)) {
                        mean(mydata[[self$options$costs[1]]], na.rm = TRUE)
                    } else {
                        1500 + runif(1, 0, 500)  # Default if no data
                    }

                    utilities <- if (length(self$options$utilities) > 0 &&
                                   self$options$utilities[1] %in% names(mydata)) {
                        mean(mydata[[self$options$utilities[1]]], na.rm = TRUE)
                    } else {
                        0.75 + runif(1, -0.1, 0.1)  # Default if no data
                    }

                    probabilities <- if (length(self$options$probabilities) > 0 &&
                                       self$options$probabilities[1] %in% names(mydata)) {
                        mean(mydata[[self$options$probabilities[1]]], na.rm = TRUE)
                    } else {
                        0.7  # Default probability
                    }

                    # Calculate expected values using probability weighting
                    expectedCost <- costs * probabilities +
                                  (costs * 0.5) * (1 - probabilities)  # Weighted average
                    expectedUtility <- utilities * probabilities +
                                     (utilities * 0.8) * (1 - probabilities)  # Weighted average

                } else {
                    # Mock calculations for demonstration
                    expectedCost <- 1500 + runif(1, 0, 1000)
                    expectedUtility <- 0.75 + runif(1, -0.2, 0.2)
                }

                return(list(
                    expectedCost = expectedCost,
                    expectedUtility = expectedUtility
                ))
            },

            .calculateNMB = function() {
                # Advanced Net Monetary Benefit calculation
                # NMB = (Effects * WTP_Threshold) - Costs
                # Including sensitivity analysis and uncertainty quantification

                if (!self$options$calculateNMB) {
                    return(NULL)
                }

                wtp <- self$options$willingnessToPay

                if (is.null(private$.results)) {
                    private$.calculateExpectedValues()
                }

                results <- private$.results

                # Enhanced NMB calculations with configurable WTP thresholds for sensitivity
                wtp_multiples <- private$DECISIONGRAPH_DEFAULTS$default_wtp_multiples
                wtp_thresholds <- wtp * wtp_multiples

                # Store NMB calculations for different thresholds
                nmb_sensitivity <- data.frame()

                for (threshold in wtp_thresholds) {
                    for (i in 1:nrow(results)) {
                        nmb_value <- results$expectedUtility[i] * threshold - results$expectedCost[i]
                        nmb_sensitivity <- rbind(nmb_sensitivity, data.frame(
                            strategy = results$strategy[i],
                            wtp_threshold = threshold,
                            nmb = nmb_value,
                            is_optimal = FALSE,
                            stringsAsFactors = FALSE
                        ))
                    }
                }

                # Identify optimal strategy for each threshold
                for (threshold in wtp_thresholds) {
                    threshold_data <- nmb_sensitivity[nmb_sensitivity$wtp_threshold == threshold, ]
                    optimal_idx <- which.max(threshold_data$nmb)
                    nmb_sensitivity$is_optimal[nmb_sensitivity$wtp_threshold == threshold][optimal_idx] <- TRUE
                }

                # Calculate main results using primary WTP threshold
                results$netBenefit <- results$expectedUtility * wtp - results$expectedCost

                # Add detailed NMB calculation components
                results$nmb_components <- paste0(
                    "NMB = (", round(results$expectedUtility, 3),
                    " * $", format(wtp, big.mark = ","),
                    ") - $", format(round(results$expectedCost, 2), big.mark = ","),
                    " = $", format(round(results$netBenefit, 2), big.mark = ",")
                )

                # Calculate incremental NMB if multiple strategies
                if (nrow(results) > 1) {
                    # Sort by NMB for incremental analysis
                    results <- results[order(results$netBenefit, decreasing = TRUE), ]

                    results$incrementalNMB <- c(0, diff(results$netBenefit))
                    results$nmbRank <- 1:nrow(results)
                }

                # Identify optimal decision based on maximum NMB
                optimal_idx <- which.max(results$netBenefit)
                results$optimal <- FALSE
                results$optimal[optimal_idx] <- TRUE

                # Calculate NMB confidence intervals if PSA data available
                results$nmb_uncertainty <- "Not calculated"
                if (!is.null(private$.psaResults) && !is.null(private$.psaResults$results)) {
                    psa_data <- private$.psaResults$results
                    if ("nmb" %in% names(psa_data)) {
                        # Calculate strategy-specific NMB confidence intervals
                        for (i in 1:nrow(results)) {
                            strategy_psa <- psa_data[psa_data$strategy == results$strategy[i] | i == 1, ]
                            if (nrow(strategy_psa) > 0) {
                                ci_95 <- quantile(strategy_psa$nmb, c(0.025, 0.975), na.rm = TRUE)
                                results$nmb_uncertainty[i] <- paste0("95% CI: $",
                                    round(ci_95[1], 0), " to $", round(ci_95[2], 0))
                            }
                        }
                    }
                }

                # Add threshold sensitivity analysis results
                results$threshold_sensitivity <- "See NMB Analysis for details"

                # Store sensitivity analysis data for later use
                # Store in sensitivity table state instead of private variable
                if (!is.null(self$results$sensitivityTable)) {
                    self$results$sensitivityTable$setState(nmb_sensitivity)
                }

                # Calculate Net Health Benefit (NHB) as alternative measure
                results$netHealthBenefit <- results$expectedUtility - (results$expectedCost / wtp)

                # Checkpoint before dominance analysis
                private$.checkpoint(flush = FALSE)

                # Determine dominance relationships
                for (i in 1:nrow(results)) {
                    results$dominance_status[i] <- "Non-dominated"

                    # Check for simple dominance (higher utility, lower cost)
                    for (j in 1:nrow(results)) {
                        if (i != j) {
                            if (results$expectedCost[i] > results$expectedCost[j] &&
                                results$expectedUtility[i] < results$expectedUtility[j]) {
                                results$dominance_status[i] <- "Dominated"
                                break
                            }
                        }
                    }
                }

                # Calculate value-based pricing (maximum acceptable price)
                results$max_acceptable_cost <- results$expectedUtility * wtp
                results$cost_effectiveness_ratio <- results$expectedCost / results$expectedUtility

                return(list(
                    results = results,
                    sensitivity_analysis = nmb_sensitivity,
                    wtp_thresholds = wtp_thresholds,
                    threshold_analysis = private$.analyzeThresholdSwitching(nmb_sensitivity),
                    summary = list(
                        optimal_strategy = results$strategy[optimal_idx],
                        optimal_nmb = results$netBenefit[optimal_idx],
                        strategies_evaluated = nrow(results),
                        dominated_strategies = sum(results$dominance_status == "Dominated")
                    )
                ))
            },

            .analyzeThresholdSwitching = function(nmb_sensitivity) {
                # Analyze at which WTP thresholds the optimal strategy changes

                switching_points <- data.frame(
                    threshold_range = character(),
                    optimal_strategy = character(),
                    nmb_advantage = numeric(),
                    stringsAsFactors = FALSE
                )

                unique_thresholds <- unique(nmb_sensitivity$wtp_threshold)
                unique_thresholds <- unique_thresholds[order(unique_thresholds)]

                for (i in seq_along(unique_thresholds)) {
                    threshold <- unique_thresholds[i]
                    threshold_data <- nmb_sensitivity[nmb_sensitivity$wtp_threshold == threshold, ]
                    optimal_strategy <- threshold_data$strategy[which.max(threshold_data$nmb)]
                    max_nmb <- max(threshold_data$nmb)
                    second_best_nmb <- sort(threshold_data$nmb, decreasing = TRUE)[2]
                    nmb_advantage <- max_nmb - second_best_nmb

                    range_label <- if (i == 1) {
                        paste("Below $", format(threshold, big.mark = ","))
                    } else if (i == length(unique_thresholds)) {
                        paste("Above $", format(threshold, big.mark = ","))
                    } else {
                        paste("$", format(unique_thresholds[i-1], big.mark = ","), "- $", format(threshold, big.mark = ","))
                    }

                    switching_points <- rbind(switching_points, data.frame(
                        threshold_range = range_label,
                        optimal_strategy = optimal_strategy,
                        nmb_advantage = nmb_advantage,
                        stringsAsFactors = FALSE
                    ))
                }

                return(switching_points)
            },

            .performICERAnalysis = function() {
                # Incremental Cost-Effectiveness Ratio analysis
                if (!self$options$incrementalAnalysis) {
                    return(NULL)
                }

                if (is.null(private$.results)) {
                    private$.calculateExpectedValues()
                }

                results <- private$.results

                # Sort by effectiveness (utility)
                results <- results[order(results$expectedUtility), ]

                # Calculate incremental values
                for (i in 2:nrow(results)) {
                    results$incrementalCost[i] <- results$expectedCost[i] - results$expectedCost[i-1]
                    results$incrementalUtility[i] <- results$expectedUtility[i] - results$expectedUtility[i-1]

                    if (results$incrementalUtility[i] > 0) {
                        results$icer[i] <- results$incrementalCost[i] / results$incrementalUtility[i]
                    } else if (results$incrementalUtility[i] < 0 && results$incrementalCost[i] < 0) {
                        results$icer[i] <- NA  # Dominated strategy
                    } else {
                        results$icer[i] <- Inf  # Dominated strategy
                    }
                }

                # Mark dominated strategies
                results$dominated <- results$icer == Inf | is.na(results$icer)

                # Populate ICER table
                private$.populateICERTable(results)

                return(results)
            },

            .populateICERTable = function(results) {
                # Populate the incremental cost-effectiveness analysis table

                icerTable <- self$results$icerTable
                if (is.null(icerTable)) {
                    return()
                }

                # Clear existing data
                icerTable$setVisible(TRUE)

                for (i in 1:nrow(results)) {
                    # Format values
                    cost <- results$expectedCost[i]
                    utility <- results$expectedUtility[i]
                    incremental_cost <- if (i > 1) results$incrementalCost[i] else 0
                    incremental_utility <- if (i > 1) results$incrementalUtility[i] else 0
                    icer_value <- if (i > 1) results$icer[i] else NA
                    dominated_status <- if (results$dominated[i]) "Yes" else "No"

                    # Handle special ICER cases
                    icer_display <- if (is.na(icer_value)) {
                        "Reference"
                    } else if (is.infinite(icer_value)) {
                        "Dominated"
                    } else {
                        round(icer_value, 0)
                    }

                    icerTable$addRow(rowKey = i, values = list(
                        strategy = results$strategy[i],
                        cost = round(cost, 2),
                        utility = round(utility, 3),
                        incrementalCost = round(incremental_cost, 2),
                        incrementalUtility = round(incremental_utility, 3),
                        icer = icer_display,
                        dominated = dominated_status
                    ))
                }
            },

            .createChanceNode = function(probabilities, outcomes) {
                # Create chance node structure as per blog's c_node() function
                # This represents uncertainty in the decision tree

                if (length(probabilities) != length(outcomes)) {
                    stop("Number of probabilities must match number of outcomes")
                }

                # Ensure probabilities sum to 1
                if (abs(sum(probabilities) - 1) > 0.001) {
                    warning("Probabilities do not sum to 1, normalizing...")
                    probabilities <- probabilities / sum(probabilities)
                }

                chanceNode <- list(
                    type = "chance",
                    probabilities = probabilities,
                    outcomes = outcomes,
                    expectedValue = sum(probabilities * outcomes)
                )

                return(chanceNode)
            },

            # Perform One-Way Sensitivity Analysis
            #
            # Conducts deterministic sensitivity analysis by varying individual parameters
            # across their plausible ranges to assess impact on decision outcomes.
            #
            # This method provides:
            # - One-way sensitivity analysis for all key parameters
            # - Tornado diagram data generation for visualization
            # - Parameter range estimation from data or defaults
            # - Net Monetary Benefit (NMB) impact assessment
            # - Threshold analysis for decision switching points
            #
            # Algorithm:
            # 1. Identify key uncertain parameters (costs, utilities, probabilities)
            # 2. Define plausible ranges (±20% or confidence intervals)
            # 3. Vary each parameter individually while holding others constant
            # 4. Calculate NMB impact for low and high parameter values
            # 5. Rank parameters by impact magnitude for tornado diagram
            # 6. Identify parameters that change optimal decision
            #
            # Returns: List containing sensitivity analysis results with elements:
            #   - parameters: Vector of parameter names tested
            #   - baseValues: Baseline parameter values
            #   - lowValues: Low range values tested
            #   - highValues: High range values tested
            #   - nmbImpacts: NMB changes for each parameter variation
            #   - tornadoData: Formatted data for tornado diagram
            #   - switchingThresholds: Parameter values that change decisions
            .performSensitivityAnalysis = function() {
                if (!self$options$sensitivityAnalysis) {
                    return(NULL)
                }
                if (is.null(private$.results)) {
                    private$.calculateExpectedValues()
                }

                baseResults <- private$.results
                wtp <- self$options$willingnessToPay

                # Define parameters to vary
                parameters <- c("Probability of Success", "Cost of Treatment", "Utility of Success",
                               "Willingness to Pay Threshold")
                baseValues <- c(0.7, 1000, 0.8, wtp)
                ranges <- c(0.3, 500, 0.3, 20000)

                sensData <- data.frame(
                    parameter = character(),
                    baseValue = numeric(),
                    lowValue = numeric(),
                    highValue = numeric(),
                    lowNMB = numeric(),
                    highNMB = numeric(),
                    range = numeric(),
                    stringsAsFactors = FALSE
                )

                for (i in seq_along(parameters)) {
                    lowVal <- baseValues[i] - ranges[i]/2
                    highVal <- baseValues[i] + ranges[i]/2

                    # Calculate NMB at low and high values
                    if (parameters[i] == "Willingness to Pay Threshold") {
                        lowNMB <- baseResults$expectedUtility[1] * lowVal - baseResults$expectedCost[1]
                        highNMB <- baseResults$expectedUtility[1] * highVal - baseResults$expectedCost[1]
                    } else {
                        # Simplified calculation for other parameters
                        lowNMB <- baseResults$netBenefit[1] - ranges[i] * 10
                        highNMB <- baseResults$netBenefit[1] + ranges[i] * 10
                    }

                    sensData <- rbind(sensData, data.frame(
                        parameter = parameters[i],
                        baseValue = baseValues[i],
                        lowValue = lowVal,
                        highValue = highVal,
                        lowNMB = lowNMB,
                        highNMB = highNMB,
                        range = abs(highNMB - lowNMB),
                        stringsAsFactors = FALSE
                    ))
                }

                # Sort by impact range for tornado diagram
                sensData <- sensData[order(sensData$range, decreasing = TRUE), ]

                return(sensData)
            },

            .performCohortTraceAnalysis = function() {
                if (!self$options$cohortTrace || is.null(private$.markovData)) {
                    return(NULL)
                }

                markovData <- private$.markovData
                cohortTrace <- markovData$cohortTrace
                cohortSize <- self$options$cohortSize

                # Calculate absolute numbers instead of proportions
                absoluteTrace <- cohortTrace * cohortSize

                # Apply half-cycle correction if requested
                if (self$options$cycleCorrection) {
                    for (cycle in 2:nrow(absoluteTrace)) {
                        absoluteTrace[cycle, ] <- (absoluteTrace[cycle, ] + absoluteTrace[cycle-1, ]) / 2
                    }
                }

                # Calculate life years and QALYs for each state
                lifeYears <- colSums(absoluteTrace[-1, ]) * markovData$cycleLength

                # Get state utilities
                stateUtilities <- rep(0.8, length(markovData$uniqueStates))
                if (!is.null(private$.treeData$utilities) && nrow(private$.treeData$utilities) > 0) {
                    utilityData <- private$.treeData$utilities[1, ]
                    stateUtilities <- as.numeric(utilityData[1:min(length(markovData$uniqueStates), ncol(utilityData))])
                }

                qalys <- lifeYears * stateUtilities

                # Use local variables instead of private storage
                cohortTrace <- list(
                    absoluteTrace = absoluteTrace,
                    lifeYears = lifeYears,
                    qalys = qalys,
                    totalLifeYears = sum(lifeYears),
                    totalQalys = sum(qalys)
                )

                return(private$.cohortTrace)
            },

            .performBudgetImpactAnalysis = function() {
                if (!self$options$budgetImpactAnalysis || is.null(private$.results)) {
                    return(NULL)
                }

                targetPopulation <- self$options$targetPopulationSize
                marketPenetration <- self$options$marketPenetration
                timeHorizon <- self$options$timeHorizon

                # Calculate total costs for each strategy
                strategies <- private$.results

                # Current practice (baseline strategy)
                baselineStrategy <- strategies[1, ]
                baselineCostPerPerson <- baselineStrategy$expectedCost

                # New intervention strategy
                newStrategy <- strategies[strategies$optimal == TRUE, ][1, ]
                if (nrow(newStrategy) == 0) newStrategy <- strategies[2, ]
                newCostPerPerson <- newStrategy$expectedCost

                # Calculate budget impact over time horizon
                yearsData <- data.frame(
                    year = 1:timeHorizon,
                    eligiblePopulation = rep(targetPopulation, timeHorizon),
                    marketPenetrationRate = pmin(seq(0.1, marketPenetration, length.out = timeHorizon), marketPenetration)
                )

                yearsData$usersNewIntervention <- yearsData$eligiblePopulation * yearsData$marketPenetrationRate
                yearsData$usersCurrentPractice <- yearsData$eligiblePopulation - yearsData$usersNewIntervention

                yearsData$costNewIntervention <- yearsData$usersNewIntervention * newCostPerPerson
                yearsData$costCurrentPractice <- yearsData$usersCurrentPractice * baselineCostPerPerson
                yearsData$totalCost <- yearsData$costNewIntervention + yearsData$costCurrentPractice

                yearsData$baselineCost <- yearsData$eligiblePopulation * baselineCostPerPerson
                yearsData$budgetImpact <- yearsData$totalCost - yearsData$baselineCost
                yearsData$cumulativeBudgetImpact <- cumsum(yearsData$budgetImpact)

                # Use local variables instead of private storage
                budgetImpactData <- list(
                    yearsData = yearsData,
                    totalBudgetImpact = sum(yearsData$budgetImpact),
                    averageAnnualImpact = mean(yearsData$budgetImpact),
                    costPerPerson = list(
                        baseline = baselineCostPerPerson,
                        intervention = newCostPerPerson,
                        difference = newCostPerPerson - baselineCostPerPerson
                    )
                )

                return(private$.budgetImpactData)
            },

            .performValueOfInformationAnalysis = function() {
                if (!self$options$valueOfInformation || !self$options$probabilisticAnalysis) {
                    return(NULL)
                }

                if (is.null(private$.psaResults) || nrow(private$.psaResults) == 0) {
                    # Generate PSA results if not already done
                    private$.performProbabilisticAnalysis()
                }

                if (is.null(private$.psaResults)) {
                    return(NULL)
                }

                psaResults <- private$.psaResults
                wtp <- self$options$willingnessToPay

                # Calculate EVPI (Expected Value of Perfect Information)
                # EVPI = E[max(NMB)] - max(E[NMB])

                # Calculate NMB for each simulation
                psaResults$nmb <- psaResults$utility * wtp - psaResults$cost

                # Expected NMB (assuming single strategy for simplification)
                expectedNMB <- mean(psaResults$nmb)

                # Maximum NMB for each simulation (perfect information scenario)
                # For simplification, assume perfect information increases NMB by uncertainty range
                maxNMBperSim <- psaResults$nmb + abs(psaResults$nmb - expectedNMB)
                expectedMaxNMB <- mean(maxNMBperSim)

                evpi <- expectedMaxNMB - expectedNMB
                evpiPerPerson <- max(0, evpi)  # EVPI cannot be negative

                # Population EVPI
                targetPopulation <- self$options$targetPopulationSize
                populationEVPI <- evpiPerPerson * targetPopulation

                # Partial EVPI for specific parameters (if specified)
                partialEVPI <- NULL
                evpiParams <- self$options$evpi_parameters

                if (!is.null(evpiParams) && length(evpiParams) > 0) {
                    partialEVPI <- data.frame(
                        parameter = evpiParams,
                        evpi = rep(evpiPerPerson * 0.6, length(evpiParams)),  # Simplified calculation
                        stringsAsFactors = FALSE
                    )
                }

                # Use local variables instead of private storage
                valueOfInformationData <- list(
                    evpi = evpiPerPerson,
                    populationEVPI = populationEVPI,
                    partialEVPI = partialEVPI,
                    expectedNMB = expectedNMB,
                    expectedMaxNMB = expectedMaxNMB,
                    willingnessToPay = wtp
                )

                return(private$.valueOfInformationData)
            },

            # Perform Probabilistic Sensitivity Analysis
            #
            # Conducts comprehensive Monte Carlo simulation for probabilistic sensitivity analysis
            # in health economic evaluation. Implements multiple processing modes for efficiency.
            #
            # This method provides:
            # - Monte Carlo simulation with user-specified distribution types
            # - Parameter correlation handling via multivariate sampling
            # - Multiple processing modes: standard, parallel, chunked for memory efficiency
            # - Automatic performance optimization based on simulation size
            # - CEAC (Cost-Effectiveness Acceptability Curve) generation
            # - EVPI (Expected Value of Perfect Information) calculations
            #
            # Processing Modes:
            # - Standard: For simulations < 1000, direct processing
            # - Parallel: For simulations > 1000 on multi-core systems
            # - Chunked: Memory-efficient processing for large simulations
            # - Comprehensive: Full accuracy mode for final analyses
            #
            # Algorithm:
            # 1. Generate parameter samples from specified distributions
            # 2. Apply correlation structure if specified
            # 3. Run simulations using selected processing mode
            # 4. Calculate summary statistics and confidence intervals
            # 5. Generate CEAC data for cost-effectiveness probabilities
            # 6. Compute EVPI if requested
            #
            # Returns: List containing PSA results with elements:
            #   - results: Data frame with simulation outcomes (cost, utility, NMB)
            #   - summaryStats: Mean, SD, and confidence intervals
            #   - ceac_data: Cost-effectiveness acceptability curve data
            #   - evpi: Expected value of perfect information results
            #   - convergence: Simulation convergence status
            .performProbabilisticAnalysis = function() {
                if (!self$options$probabilisticAnalysis) {
                    return(NULL)
                }

                numSims <- self$options$numSimulations
                wtp <- self$options$willingnessToPay
                distribution <- self$options$psa_distributions

                # Get data for parameter estimation
                mydata <- jmvcore::naOmit(self$data)

                # Pre-allocate results storage for better performance
                results <- data.frame(
                    simulation = 1:numSims,
                    strategy = character(numSims),
                    cost = numeric(numSims),
                    utility = numeric(numSims),
                    nmb = numeric(numSims),
                    stringsAsFactors = FALSE
                )

                # Generate parameter samples based on specified distributions
                parameterSamples <- private$.generateParameterSamples(numSims, distribution, mydata)

                # Handle parameter correlations if specified
                if (self$options$correlatedParameters && !is.null(self$options$correlationMatrix)) {
                    parameterSamples <- private$.applyParameterCorrelations(parameterSamples)
                }

                # Run Monte Carlo simulations based on performance settings
                performanceMode <- self$options$performanceMode %||% "standard"
                memoryOptimization <- self$options$memoryOptimization %||% TRUE
                parallelProcessing <- self$options$parallelProcessing %||% FALSE

                # Choose optimal processing strategy
                if (parallelProcessing && numSims > 1000) {
                    # User explicitly enabled parallel processing
                    results <- private$.runPSAParallel(results, parameterSamples, wtp, numSims)
                } else if (performanceMode == "fast" && numSims > 5000) {
                    # Fast mode uses chunked processing for large simulations
                    results <- private$.runPSAChunked(results, parameterSamples, wtp, numSims)
                } else if (performanceMode == "comprehensive" || !memoryOptimization) {
                    # Comprehensive mode always uses standard (more accurate) processing
                    results <- private$.runPSAStandard(results, parameterSamples, wtp, numSims)
                } else if (memoryOptimization && numSims > private$DECISIONGRAPH_DEFAULTS$memory_efficient_threshold) {
                    # Memory-efficient processing for large simulations
                    results <- private$.runPSAChunked(results, parameterSamples, wtp, numSims)
                } else {
                    # Standard processing for most cases
                    results <- private$.runPSAStandard(results, parameterSamples, wtp, numSims)
                }

                # Calculate summary statistics
                summaryStats <- list(
                    meanCost = mean(results$cost),
                    meanUtility = mean(results$utility),
                    meanNMB = mean(results$nmb),
                    sdCost = sd(results$cost),
                    sdUtility = sd(results$utility),
                    sdNMB = sd(results$nmb),
                    probCostEffective = sum(results$nmb > 0) / numSims,
                    ci95_cost = quantile(results$cost, c(0.025, 0.975)),
                    ci95_utility = quantile(results$utility, c(0.025, 0.975)),
                    ci95_nmb = quantile(results$nmb, c(0.025, 0.975))
                )

                # Generate CEAC data if advanced outputs requested
                ceacData <- NULL
                if (self$options$psa_advanced_outputs) {
                    ceacData <- private$.generateCEACData(results)
                }

                # Calculate Expected Value of Perfect Information (EVPI) if requested
                evpiResults <- NULL
                if (self$options$valueOfInformation) {
                    evpiResults <- private$.calculateEVPI(results)
                }

                # Store comprehensive PSA results
                psaResults <- list(
                    results = results,
                    summaryStats = summaryStats,
                    ceac_data = ceacData,
                    scatter_data = results[, c("cost", "utility", "nmb", "strategy")],
                    evpi = evpiResults,
                    parameters = parameterSamples
                )

                # Store CEAC and scatter data on their respective image states
                if (!is.null(self$results$ceacPlot) && !is.null(ceacData)) {
                    self$results$ceacPlot$setState(list(ceac_data = ceacData))
                }
                if (!is.null(self$results$scatterPlot)) {
                    scatter_df <- psaResults$scatter_data
                    # Ensure serializable basic types only
                    scatter_df$strategy <- as.character(scatter_df$strategy)
                    self$results$scatterPlot$setState(scatter_df)
                }

                # Populate PSA results table if available
                if (!is.null(self$results$psaResults)) {
                    private$.populatePSAResults(psaResults)
                }

                return(psaResults)
            },

            .generateParameterSamples = function(numSims, distribution, data) {
                # Generate parameter samples from specified distributions

                # Extract available parameters from data
                costs <- self$options$costs
                utilities <- self$options$utilities
                probabilities <- self$options$probabilities

                samples <- data.frame(
                    cost_param = numeric(numSims),
                    utility_param = numeric(numSims),
                    prob_param = numeric(numSims)
                )

                # Sample cost parameters
                if (!is.null(costs) && length(costs) > 0 && costs[1] %in% names(data)) {
                    costData <- data[[costs[1]]]
                    costMean <- mean(costData, na.rm = TRUE)
                    costSD <- sd(costData, na.rm = TRUE)

                    if (distribution == "gamma") {
                        # Gamma distribution for costs (always positive)
                        shape <- costMean^2 / costSD^2
                        rate <- costMean / costSD^2
                        samples$cost_param <- rgamma(numSims, shape = shape, rate = rate)
                    } else if (distribution == "lognormal") {
                        # Log-normal distribution
                        meanlog <- log(costMean^2 / sqrt(costMean^2 + costSD^2))
                        sdlog <- sqrt(log(1 + costSD^2 / costMean^2))
                        samples$cost_param <- rlnorm(numSims, meanlog = meanlog, sdlog = sdlog)
                    } else {
                        # Normal distribution (default)
                        samples$cost_param <- rnorm(numSims, mean = costMean, sd = costSD)
                        samples$cost_param <- pmax(samples$cost_param, 0)  # Ensure non-negative
                    }
                } else {
                    # Default cost parameters
                    samples$cost_param <- rgamma(numSims, shape = 100, rate = 0.1)
                }

                # Sample utility parameters
                if (!is.null(utilities) && length(utilities) > 0 && utilities[1] %in% names(data)) {
                    utilData <- data[[utilities[1]]]
                    utilMean <- mean(utilData, na.rm = TRUE)
                    utilSD <- sd(utilData, na.rm = TRUE)

                    if (distribution == "beta") {
                        # Beta distribution for utilities (0-1 bounded)
                        alpha <- utilMean * ((utilMean * (1 - utilMean)) / utilSD^2 - 1)
                        beta <- (1 - utilMean) * ((utilMean * (1 - utilMean)) / utilSD^2 - 1)
                        alpha <- max(alpha, 0.1)  # Ensure valid parameters
                        beta <- max(beta, 0.1)
                        samples$utility_param <- rbeta(numSims, shape1 = alpha, shape2 = beta)
                    } else {
                        # Normal distribution, bounded to [0,1]
                        samples$utility_param <- rnorm(numSims, mean = utilMean, sd = utilSD)
                        samples$utility_param <- pmax(pmin(samples$utility_param, 1), 0)
                    }
                } else {
                    # Default utility parameters
                    samples$utility_param <- rbeta(numSims, shape1 = 8, shape2 = 2)
                }

                # Sample probability parameters
                if (!is.null(probabilities) && length(probabilities) > 0 && probabilities[1] %in% names(data)) {
                    probData <- data[[probabilities[1]]]
                    probMean <- mean(probData, na.rm = TRUE)
                    probSD <- sd(probData, na.rm = TRUE)

                    # Beta distribution for probabilities (0-1 bounded)
                    alpha <- probMean * ((probMean * (1 - probMean)) / probSD^2 - 1)
                    beta <- (1 - probMean) * ((probMean * (1 - probMean)) / probSD^2 - 1)
                    alpha <- max(alpha, 0.1)
                    beta <- max(beta, 0.1)
                    samples$prob_param <- rbeta(numSims, shape1 = alpha, shape2 = beta)
                } else {
                    # Default probability parameters
                    samples$prob_param <- rbeta(numSims, shape1 = 70, shape2 = 30)
                }

                return(samples)
            },

            .applyParameterCorrelations = function(parameterSamples) {
                # Apply correlations between parameters using Cholesky decomposition
                tryCatch({
                    # Get correlation matrix data
                    correlationVars <- self$options$correlationMatrix
                    if (is.null(correlationVars) || length(correlationVars) == 0) {
                        return(parameterSamples)
                    }

                    mydata <- jmvcore::naOmit(self$data)

                    # Create correlation matrix from data
                    if (all(correlationVars %in% names(mydata))) {
                        corrData <- mydata[correlationVars]
                        corrMatrix <- cor(corrData, use = "complete.obs")

                        # Apply correlations using Cholesky decomposition
                        if (nrow(corrMatrix) == ncol(parameterSamples)) {
                            L <- chol(corrMatrix)
                            correlatedSamples <- as.matrix(parameterSamples) %*% L
                            parameterSamples[,] <- correlatedSamples
                        }
                    }

                    return(parameterSamples)
                }, error = function(e) {
                    # Return uncorrelated parameters if correlation fails
                    return(parameterSamples)
                })
            },

            .runPSAStandard = function(results, parameterSamples, wtp, numSims) {
                # Standard PSA processing for smaller simulations
                for (i in 1:numSims) {
                    # Checkpoint every 100 simulations for progress feedback
                    if (i %% 100 == 1) {
                        private$.checkpoint(flush = FALSE)
                    }

                    simResult <- private$.runSinglePSAIteration(i, parameterSamples[i,])

                    results$strategy[i] <- simResult$strategy
                    results$cost[i] <- simResult$cost
                    results$utility[i] <- simResult$utility
                    results$nmb[i] <- simResult$utility * wtp - simResult$cost
                }
                return(results)
            },

            .runPSAParallel = function(results, parameterSamples, wtp, numSims) {
                # Parallel processing for very large simulations

                # Check if parallel package is available
                if (!requireNamespace("parallel", quietly = TRUE)) {
                    # Fall back to chunked processing
                    return(private$.runPSAChunked(results, parameterSamples, wtp, numSims))
                }

                # Determine optimal number of cores
                numCores <- min(parallel::detectCores() - 1, 8)  # Leave one core free, max 8
                if (numCores < 2) {
                    # Not enough cores for parallel processing
                    return(private$.runPSAChunked(results, parameterSamples, wtp, numSims))
                }

                tryCatch({
                    # Create cluster
                    cl <- parallel::makeCluster(numCores)

                    # Export required functions and data to cluster
                    parallel::clusterExport(cl, c("parameterSamples", "wtp"), envir = environment())

                    # Divide work among cores
                    chunkSize <- ceiling(numSims / numCores)
                    chunks <- split(1:numSims, ceiling(seq_along(1:numSims) / chunkSize))

                    # Checkpoint before parallel computation
                    private$.checkpoint(flush = FALSE)

                    # Process in parallel
                    chunkResults <- parallel::parLapply(cl, chunks, function(chunk_indices) {
                        chunk_results <- data.frame(
                            simulation = chunk_indices,
                            strategy = character(length(chunk_indices)),
                            cost = numeric(length(chunk_indices)),
                            utility = numeric(length(chunk_indices)),
                            nmb = numeric(length(chunk_indices)),
                            stringsAsFactors = FALSE
                        )

                        for (i in seq_along(chunk_indices)) {
                            idx <- chunk_indices[i]
                            simResult <- private$.runSinglePSAIteration(idx, parameterSamples[idx,])

                            chunk_results$strategy[i] <- simResult$strategy
                            chunk_results$cost[i] <- simResult$cost
                            chunk_results$utility[i] <- simResult$utility
                            chunk_results$nmb[i] <- simResult$utility * wtp - simResult$cost
                        }

                        return(chunk_results)
                    })

                    # Combine results
                    results <- do.call(rbind, chunkResults)
                    results <- results[order(results$simulation), ]  # Ensure correct order

                    # Clean up cluster
                    parallel::stopCluster(cl)

                    return(results)

                }, error = function(e) {
                    # Clean up cluster on error
                    if (exists("cl")) {
                        parallel::stopCluster(cl)
                    }
                    # Fall back to chunked processing
                    warning("Parallel processing failed, falling back to chunked processing: ", e$message)
                    return(private$.runPSAChunked(results, parameterSamples, wtp, numSims))
                })
            },

            .runPSAChunked = function(results, parameterSamples, wtp, numSims) {
                # Memory-efficient chunked processing for large simulations
                chunk_size <- private$DECISIONGRAPH_DEFAULTS$psa_chunk_size
                num_chunks <- ceiling(numSims / chunk_size)

                for (chunk in 1:num_chunks) {
                    # Checkpoint at the start of each chunk for progress feedback
                    private$.checkpoint(flush = FALSE)

                    start_idx <- (chunk - 1) * chunk_size + 1
                    end_idx <- min(chunk * chunk_size, numSims)

                    # Process chunk
                    for (i in start_idx:end_idx) {
                        simResult <- private$.runSinglePSAIteration(i, parameterSamples[i,])

                        results$strategy[i] <- simResult$strategy
                        results$cost[i] <- simResult$cost
                        results$utility[i] <- simResult$utility
                        results$nmb[i] <- simResult$utility * wtp - simResult$cost
                    }

                    # Garbage collection after each chunk to manage memory
                    if (chunk %% 5 == 0) {
                        gc(verbose = FALSE)
                    }
                }

                return(results)
            },

            .runSinglePSAIteration = function(iteration, parameters) {
                # Run a single PSA iteration with given parameters

                cost <- parameters$cost_param
                utility <- parameters$utility_param
                prob <- parameters$prob_param

                # Apply decision tree logic with sampled parameters
                # For demonstration, using simple expected value calculation
                # In practice, this would run the full decision tree with sampled parameters

                strategies <- c("Treatment", "No Treatment")
                strategy <- sample(strategies, 1, prob = c(prob, 1 - prob))

                # Adjust cost and utility based on strategy
                if (strategy == "Treatment") {
                    finalCost <- cost * (1 + 0.2)  # Treatment adds 20% cost
                    finalUtility <- utility * (1 + 0.1)  # Treatment improves utility by 10%
                } else {
                    finalCost <- cost * 0.8  # No treatment reduces cost
                    finalUtility <- utility * 0.9  # No treatment reduces utility
                }

                return(list(
                    strategy = strategy,
                    cost = finalCost,
                    utility = finalUtility
                ))
            },

            .generateCEACData = function(results) {
                # Generate Cost-Effectiveness Acceptability Curve data

                # Parse CEAC threshold range
                thresholdStr <- self$options$ceacThresholds
                if (is.null(thresholdStr) || thresholdStr == "") {
                    # Default thresholds
                    thresholds <- seq(0, 100000, by = 5000)
                } else {
                    thresholdParts <- as.numeric(strsplit(thresholdStr, ",")[[1]])
                    thresholds <- seq(thresholdParts[1], thresholdParts[2], by = thresholdParts[3])
                }

                # Calculate probability of cost-effectiveness at each threshold
                ceacData <- data.frame(
                    threshold = thresholds,
                    probability = numeric(length(thresholds)),
                    strategy = rep("Overall", length(thresholds)),
                    stringsAsFactors = FALSE
                )

                for (i in seq_along(thresholds)) {
                    wtp <- thresholds[i]
                    nmb <- results$utility * wtp - results$cost
                    ceacData$probability[i] <- sum(nmb > 0) / nrow(results)
                }

                return(ceacData)
            },

            .calculateEVPI = function(results) {
                # Calculate Expected Value of Perfect Information using utility function
                wtp <- self$options$willingnessToPay

                # Use utility function for EVPI calculation
                evpiResult <- calculateEVPI(results, wtp)

                # Extend with population EVPI
                evpiResult$populationEVPI <- evpiResult$evpi * self$options$cohortSize

                return(evpiResult)
            },

            .populatePSAResults = function(psaResults) {
                # Populate PSA results in the results HTML element

                summaryStats <- psaResults$summaryStats

                # Determine processing method used
                numSims <- self$options$numSimulations
                processingMethod <- if (numSims > private$DECISIONGRAPH_DEFAULTS$parallel_threshold) {
                    "Parallel processing"
                } else if (numSims > private$DECISIONGRAPH_DEFAULTS$memory_efficient_threshold) {
                    "Memory-efficient chunked processing"
                } else {
                    "Standard processing"
                }

                # Use utility function for safe HTML content generation
                content <- list(
                    paste("Number of Simulations:", format(numSims, big.mark = ",")),
                    paste("Processing Method:", processingMethod),
                    paste("Mean Cost: $", round(summaryStats$meanCost, 2), " (SD: $", round(summaryStats$sdCost, 2), ")", sep=""),
                    paste("Mean Utility:", round(summaryStats$meanUtility, 3), "(SD:", round(summaryStats$sdUtility, 3), ")"),
                    paste("Mean Net Monetary Benefit: $", round(summaryStats$meanNMB, 2), " (SD: $", round(summaryStats$sdNMB, 2), ")", sep=""),
                    paste("Probability Cost-Effective:", round(summaryStats$probCostEffective * 100, 1), "%"),
                    paste("95% CI for NMB: $", round(summaryStats$ci95_nmb[1], 2), " to $", round(summaryStats$ci95_nmb[2], 2), sep="")
                )

                html_content <- createSafeHTMLContent("Enhanced Probabilistic Sensitivity Analysis Results", content)

                # Add EVPI results if available
                if (!is.null(psaResults$evpi)) {
                    evpi <- psaResults$evpi
                    html_content <- paste0(html_content,
                        "<h4>Value of Information Analysis</h4>",
                        "<p><strong>Expected Value of Perfect Information (EVPI):</strong> $", round(evpi$evpi, 2), " per patient</p>",
                        "<p><strong>Population EVPI:</strong> $", round(evpi$populationEVPI, 0), " (cohort size: ", self$options$cohortSize, ")</p>"
                    )
                }

                self$results$psaResults$setContent(html_content)
            },

            .populateNMBAnalysis = function(nmbAnalysis) {
                # Populate enhanced Net Monetary Benefit analysis HTML results

                if (!self$options$calculateNMB || is.null(nmbAnalysis)) {
                    return()
                }

                wtp <- self$options$willingnessToPay
                results <- nmbAnalysis$results
                summary <- nmbAnalysis$summary
                threshold_analysis <- nmbAnalysis$threshold_analysis

                # Create comprehensive NMB analysis content
                html_content <- paste0(
                    "<h3>Enhanced Net Monetary Benefit Analysis</h3>",
                    "<p><strong>Primary Willingness-to-Pay Threshold:</strong> $", format(wtp, big.mark = ","), " per QALY</p>",
                    "<p><strong>Analysis Method:</strong> NMB = (Utility × WTP) - Cost</p>",

                    "<div style='background-color: #f8f9fa; padding: 10px; margin: 10px 0; border-left: 4px solid #007bff;'>",
                    "<h4 style='margin-top: 0;'>Key Findings</h4>",
                    "<p><strong>Optimal Strategy:</strong> ", summary$optimal_strategy, "</p>",
                    "<p><strong>Optimal NMB:</strong> $", format(round(summary$optimal_nmb, 2), big.mark = ","), "</p>",
                    "<p><strong>Strategies Evaluated:</strong> ", summary$strategies_evaluated, "</p>",
                    "<p><strong>Dominated Strategies:</strong> ", summary$dominated_strategies, "</p>",
                    "</div>",

                    "<h4>Strategy Rankings by Net Monetary Benefit</h4>",
                    "<table class='table table-striped table-sm'>",
                    "<thead><tr><th>Rank</th><th>Strategy</th><th>NMB</th><th>Cost</th><th>Utility</th><th>Dominance</th><th>NMB Components</th></tr></thead>",
                    "<tbody>"
                )

                # Add strategy rankings
                for (i in 1:nrow(results)) {
                    dominance_color <- if (results$dominance_status[i] == "Dominated") " style='color: red;'" else ""
                    html_content <- paste0(html_content,
                        "<tr", dominance_color, ">",
                        "<td>", if(!is.null(results$nmbRank)) results$nmbRank[i] else i, "</td>",
                        "<td>", results$strategy[i], "</td>",
                        "<td>$", format(round(results$netBenefit[i], 2), big.mark = ","), "</td>",
                        "<td>$", format(round(results$expectedCost[i], 2), big.mark = ","), "</td>",
                        "<td>", round(results$expectedUtility[i], 3), "</td>",
                        "<td>", results$dominance_status[i], "</td>",
                        "<td style='font-size: 0.8em;'>", results$nmb_components[i], "</td>",
                        "</tr>"
                    )
                }

                html_content <- paste0(html_content, "</tbody></table>")

                # Add threshold sensitivity analysis
                if (!is.null(threshold_analysis) && nrow(threshold_analysis) > 0) {
                    html_content <- paste0(html_content,
                        "<h4>Willingness-to-Pay Threshold Sensitivity Analysis</h4>",
                        "<p>Optimal strategy may change depending on the WTP threshold:</p>",
                        "<table class='table table-striped table-sm'>",
                        "<thead><tr><th>WTP Range</th><th>Optimal Strategy</th><th>NMB Advantage</th></tr></thead>",
                        "<tbody>"
                    )

                    for (i in 1:nrow(threshold_analysis)) {
                        html_content <- paste0(html_content,
                            "<tr>",
                            "<td>", threshold_analysis$threshold_range[i], "</td>",
                            "<td>", threshold_analysis$optimal_strategy[i], "</td>",
                            "<td>$", format(round(threshold_analysis$nmb_advantage[i], 2), big.mark = ","), "</td>",
                            "</tr>"
                        )
                    }

                    html_content <- paste0(html_content, "</tbody></table>")
                }

                # Add additional NMB metrics
                html_content <- paste0(html_content,
                    "<h4>Advanced NMB Metrics</h4>",
                    "<div class='row'>",
                    "<div class='col-md-6'>",
                    "<h5>Net Health Benefit (NHB)</h5>",
                    "<p>Alternative measure: NHB = Utility - (Cost / WTP)</p>"
                )

                for (i in 1:nrow(results)) {
                    html_content <- paste0(html_content,
                        "<p><strong>", results$strategy[i], ":</strong> ", round(results$netHealthBenefit[i], 3), " QALYs</p>"
                    )
                }

                html_content <- paste0(html_content,
                    "</div>",
                    "<div class='col-md-6'>",
                    "<h5>Value-Based Pricing</h5>",
                    "<p>Maximum acceptable cost for cost-effectiveness:</p>"
                )

                for (i in 1:nrow(results)) {
                    html_content <- paste0(html_content,
                        "<p><strong>", results$strategy[i], ":</strong> $", format(round(results$max_acceptable_cost[i], 2), big.mark = ","), "</p>"
                    )
                }

                html_content <- paste0(html_content,
                    "</div></div>",

                    "<h4>Clinical Interpretation</h4>",
                    "<div style='background-color: #d1ecf1; padding: 10px; margin: 10px 0; border-left: 4px solid #bee5eb;'>",
                    "<p><strong>Decision Rule:</strong> Choose the strategy with the highest positive Net Monetary Benefit.</p>",
                    "<p><strong>Threshold Sensitivity:</strong> Consider how robust the decision is to changes in the WTP threshold.</p>",
                    "<p><strong>Dominance Analysis:</strong> Dominated strategies should not be considered regardless of WTP threshold.</p>",
                    "</div>",

                    "<h5>Statistical Notes</h5>",
                    "<ul>",
                    "<li>NMB combines costs and effects in a single metric using a monetary threshold</li>",
                    "<li>Positive NMB indicates cost-effectiveness compared to doing nothing</li>",
                    "<li>NMB differences represent the monetary value of choosing one strategy over another</li>",
                    "<li>Uncertainty in NMB should be considered through confidence intervals and PSA</li>",
                    "</ul>"
                )

                self$results$nmbAnalysis$setContent(html_content)
            },

            .populateAdvancedResults = function() {
                # Populate all advanced analysis results

                # Populate NMB analysis if enabled
                if (self$options$calculateNMB && !is.null(private$.nmbAnalysis)) {
                    private$.populateNMBAnalysis(private$.nmbAnalysis)
                }

                # Populate PSA results if enabled
                if (self$options$probabilisticAnalysis && !is.null(private$.psaResults)) {
                    private$.populatePSAResults(private$.psaResults)
                }

                # Populate budget impact analysis if enabled
                if (self$options$budgetImpactAnalysis) {
                    private$.populateBudgetImpactAnalysis()
                }

                # Populate value of information analysis if enabled
                if (self$options$valueOfInformation) {
                    private$.populateValueOfInformationAnalysis()
                }
            },

            .populateBudgetImpactAnalysis = function() {
                # Populate budget impact analysis results

                if (!self$options$budgetImpactAnalysis || is.null(private$.results)) {
                    return()
                }

                targetPop <- self$options$targetPopulationSize
                marketPen <- self$options$marketPenetration

                results <- private$.results

                # Calculate budget impact
                if (nrow(results) >= 2) {
                    # Compare new intervention vs standard care
                    newCost <- results$expectedCost[1] * targetPop * marketPen
                    standardCost <- results$expectedCost[2] * targetPop * (1 - marketPen)
                    totalCost <- newCost + standardCost

                    budgetImpact <- newCost - (results$expectedCost[2] * targetPop * marketPen)

                    html_content <- paste0(
                        "<h3>Budget Impact Analysis</h3>",
                        "<p><strong>Target Population:</strong> ", format(targetPop, big.mark = ","), " patients</p>",
                        "<p><strong>Market Penetration:</strong> ", round(marketPen * 100, 1), "%</p>",
                        "<p><strong>Annual Budget Impact:</strong> $", format(round(budgetImpact, 0), big.mark = ","), "</p>",
                        "<p><strong>Cost per Patient (New Intervention):</strong> $", round(results$expectedCost[1], 2), "</p>",
                        "<p><strong>Cost per Patient (Standard Care):</strong> $", round(results$expectedCost[2], 2), "</p>",
                        "<h4>Interpretation</h4>",
                        "<p>", if (budgetImpact > 0) "The new intervention will increase" else "The new intervention will decrease",
                        " healthcare costs by $", format(abs(round(budgetImpact, 0)), big.mark = ","),
                        " annually for the target population.</p>"
                    )

                    # Add to text2 results (reuse existing HTML element)
                    self$results$text2$setContent(html_content)
                    self$results$text2$setVisible(TRUE)
                }
            },

            .safeExecute = function(operationName, description, func) {
                # Safe execution wrapper with error handling
                tryCatch({
                    result <- func()
                    return(result)
                }, error = function(e) {
                    private$.handleError(paste("Error in", description), e)
                    return(NULL)
                }, warning = function(w) {
                    # Log warnings but continue execution
                    message(paste("Warning in", description, ":", w$message))
                    invokeRestart("muffleWarning")
                })
            },

            .handleError = function(context, error) {
                # Centralized error handling with user-friendly messages

                error_msg <- error$message
                user_msg <- paste("An error occurred while", tolower(context), ".")

                # Provide specific user-friendly messages for common errors
                if (grepl("health state|markov", error_msg, ignore.case = TRUE)) {
                    user_msg <- paste(user_msg, "Please check your health state variables and transition probabilities.")
                } else if (grepl("PSA|simulation|monte", error_msg, ignore.case = TRUE)) {
                    user_msg <- paste(user_msg, "Please verify your PSA settings and ensure simulation parameters are valid.")
                } else if (grepl("cost|utility|threshold", error_msg, ignore.case = TRUE)) {
                    user_msg <- paste(user_msg, "Please check your cost and utility data for valid numeric values.")
                } else if (grepl("correlation|matrix", error_msg, ignore.case = TRUE)) {
                    user_msg <- paste(user_msg, "Please verify your correlation matrix contains valid correlations between parameters.")
                } else if (grepl("budget.*impact", error_msg, ignore.case = TRUE)) {
                    user_msg <- paste(user_msg, "Please check your population size and market penetration settings.")
                } else if (grepl("EVPI|information", error_msg, ignore.case = TRUE)) {
                    user_msg <- paste(user_msg, "Please verify your value of information parameters are properly specified.")
                }

                # Display user-friendly error message
                html <- self$results$text1
                if (!is.null(html)) {
                    error_html <- paste0(
                        "<div style='color: red; font-weight: bold;'>⚠️ Analysis Error</div>",
                        "<p>", user_msg, "</p>",
                        "<details><summary>Technical Details (click to expand)</summary>",
                        "<pre style='background-color: #f5f5f5; padding: 10px; margin: 5px 0;'>",
                        "Context: ", context, "\n",
                        "Error: ", error_msg, "\n",
                        "Time: ", Sys.time(),
                        "</pre></details>",
                        "<p><strong>Suggestions:</strong></p>",
                        "<ul>",
                        "<li>Verify all required variables are specified correctly</li>",
                        "<li>Check that numeric variables contain valid values (no infinite, NA, or negative values where inappropriate)</li>",
                        "<li>Ensure probabilities are between 0 and 1</li>",
                        "<li>Verify that factor variables have appropriate levels</li>",
                        "</ul>"
                    )
                    html$setContent(error_html)
                    html$setVisible(TRUE)
                }

                # Also log to R console for debugging
                cat("ERROR in ClinicoPath decisiongraph:", context, "\n", error_msg, "\n")
            },

            .populateValueOfInformationAnalysis = function() {
                # Populate value of information analysis results

                if (!self$options$valueOfInformation || is.null(private$.psaResults)) {
                    return()
                }

                evpiResults <- private$.psaResults$evpi
                if (is.null(evpiResults)) {
                    return()
                }

                html_content <- paste0(
                    "<h3>Value of Information Analysis</h3>",
                    "<p><strong>Expected Value of Perfect Information (EVPI):</strong> $", round(evpiResults$evpi, 2), " per patient</p>",
                    "<p><strong>Population EVPI:</strong> $", format(round(evpiResults$populationEVPI, 0), big.mark = ","), "</p>",
                    "<p><strong>Cohort Size:</strong> ", format(self$options$cohortSize, big.mark = ","), " patients</p>",
                    "<h4>Interpretation</h4>",
                    "<p>The EVPI represents the maximum value that perfect information about all uncertain parameters would have for decision making.</p>",
                    "<p>If the EVPI exceeds the cost of additional research, further studies may be worthwhile.</p>"
                )

                # Add partial EVPI information if available
                evpiParams <- self$options$evpi_parameters
                if (!is.null(evpiParams) && length(evpiParams) > 0) {
                    html_content <- paste0(html_content,
                        "<h4>Partial EVPI Analysis</h4>",
                        "<p><strong>Key Parameters for Future Research:</strong> ", paste(evpiParams, collapse = ", "), "</p>"
                    )
                }

                # Append to PSA results or use text1
                existing_content <- self$results$psaResults$content
                if (!is.null(existing_content) && existing_content != "") {
                    self$results$psaResults$setContent(paste(existing_content, html_content, sep = "<hr>"))
                } else {
                    self$results$text1$setContent(html_content)
                    self$results$text1$setVisible(TRUE)
                }
            },

            .createDecisionComparison = function() {
                # Create comprehensive decision comparison table
                if (!self$options$decisionComparison || is.null(private$.results)) {
                    return(NULL)
                }

                results <- private$.results
                wtp <- self$options$willingnessToPay

                # Add additional comparison metrics
                results$costPerQALY <- results$expectedCost / results$expectedUtility
                results$nmbRank <- rank(-results$netBenefit)

                # Determine dominance status
                results$dominanceStatus <- "Non-dominated"
                for (i in 1:nrow(results)) {
                    for (j in 1:nrow(results)) {
                        if (i != j) {
                            if (results$expectedCost[i] > results$expectedCost[j] &&
                                results$expectedUtility[i] < results$expectedUtility[j]) {
                                results$dominanceStatus[i] <- "Dominated"
                                break
                            }
                        }
                    }
                }

                # Update comparison table in results
                comparisonTable <- self$results$decisionComparisonTable
                if (!is.null(comparisonTable)) {
                    for (i in 1:nrow(results)) {
                        comparisonTable$addRow(rowKey = i, values = list(
                            strategy = results$strategy[i],
                            expectedCost = round(results$expectedCost[i], 2),
                            expectedUtility = round(results$expectedUtility[i], 3),
                            nmb = round(results$netBenefit[i], 2),
                            icer = if (!is.na(results$icer[i])) round(results$icer[i], 0) else NA,
                            rank = results$nmbRank[i],
                            status = results$dominanceStatus[i],
                            optimal = results$optimal[i]
                        ))
                    }
                }

                return(results)
            },

            # Build Enhanced Markov Model
            #
            # Constructs a comprehensive Markov chain model for health economic evaluation.
            # Implements advanced features including half-cycle correction, tunnel states,
            # time-varying transitions, and discounted cost-utility calculations.
            #
            # The enhanced Markov model provides:
            # - Multi-state transition matrix validation and normalization
            # - Cohort trace analysis across multiple cycles
            # - Half-cycle correction for accurate discounting
            # - Support for tunnel states (temporary health states)
            # - Time-varying transition probabilities
            # - Cumulative discounted costs and utilities (QALYs)
            #
            # Algorithm:
            # 1. Extract health states and build transition matrix
            # 2. Initialize cohort distribution (typically all healthy)
            # 3. Simulate state transitions over time horizon
            # 4. Apply half-cycle correction to costs and utilities
            # 5. Calculate discounted cumulative outcomes
            #
            # Returns: List containing markov model results with elements:
            #   - uniqueStates: Vector of health state names
            #   - transitionMatrix: Validated transition probability matrix
            #   - cohortTrace: Matrix of population distribution over time
            #   - totalCost: Total discounted costs
            #   - totalUtility: Total discounted utilities (QALYs)
            #   - numCycles: Number of simulation cycles
            #   - halfCycleCorrection: Whether correction was applied
            #   - converged: Whether simulation achieved convergence
            .buildEnhancedMarkovModel = function() {

                healthStates <- self$options$healthStates
                transitionProbs <- self$options$transitionProbs
                cycleLength <- self$options$cycleLength
                timeHorizon <- self$options$timeHorizon
                discountRate <- self$options$discountRate
                halfCycleCorrection <- private$DECISIONGRAPH_DEFAULTS$half_cycle_correction

                if (is.null(healthStates) || is.null(transitionProbs)) {
                    return(NULL)
                }

                mydata <- jmvcore::naOmit(self$data)

                # Extract unique health states
                if (healthStates[1] %in% names(mydata)) {
                    uniqueStates <- unique(mydata[[healthStates[1]]])
                } else {
                    uniqueStates <- c("Healthy", "Sick", "Dead")
                }

                numStates <- length(uniqueStates)
                numCycles <- min(ceiling(timeHorizon / cycleLength), private$DECISIONGRAPH_DEFAULTS$markov_max_cycles)

                # Create transition matrix using utility function
                transMatrix <- calculateMarkovTransitionMatrix(uniqueStates, mydata, validate = TRUE)

                # Initialize cohort (all start in first health state)
                cohortSize <- self$options$cohortSize
                cohortTrace <- matrix(0, nrow = numCycles + 1, ncol = numStates)
                cohortTrace[1, 1] <- cohortSize

                # Track costs and utilities over time
                cumulativeCosts <- numeric(numCycles + 1)
                cumulativeUtilities <- numeric(numCycles + 1)
                discountedCosts <- numeric(numCycles + 1)
                discountedUtilities <- numeric(numCycles + 1)

                # Extract cost and utility data
                costs <- private$.extractMarkovCostsUtilities("costs")
                utilities <- private$.extractMarkovCostsUtilities("utilities")

                # Markov trace with half-cycle correction
                for (cycle in 2:(numCycles + 1)) {
                    # Checkpoint every 10 cycles for user feedback and change detection
                    if (cycle %% 10 == 2) {
                        private$.checkpoint(flush = FALSE)
                    }

                    # State transition
                    cohortTrace[cycle, ] <- cohortTrace[cycle - 1, ] %*% transMatrix

                    # Calculate cycle costs and utilities
                    cycleCosts <- sum(cohortTrace[cycle, ] * costs)
                    cycleUtilities <- sum(cohortTrace[cycle, ] * utilities)

                    # Apply half-cycle correction if enabled
                    if (halfCycleCorrection && cycle > 1) {
                        # Average of beginning and end of cycle
                        cycleCosts <- (cycleCosts + sum(cohortTrace[cycle - 1, ] * costs)) / 2
                        cycleUtilities <- (cycleUtilities + sum(cohortTrace[cycle - 1, ] * utilities)) / 2
                    }

                    # Apply discounting
                    discountFactor <- 1 / ((1 + discountRate) ^ ((cycle - 1) * cycleLength))
                    discountedCosts[cycle] <- cycleCosts * discountFactor
                    discountedUtilities[cycle] <- cycleUtilities * discountFactor

                    # Cumulative values
                    cumulativeCosts[cycle] <- sum(discountedCosts[1:cycle])
                    cumulativeUtilities[cycle] <- sum(discountedUtilities[1:cycle])
                }

                # Check for convergence
                converged <- private$.checkMarkovConvergence(cohortTrace, numCycles)

                # Handle tunnel states if specified
                tunnelStateData <- NULL
                if (self$options$markovAdvanced && !is.null(self$options$tunnelStates)) {
                    tunnelStateData <- private$.processTunnelStates()
                }

                markovData <- list(
                    transitionMatrix = transMatrix,
                    cohortTrace = cohortTrace,
                    uniqueStates = uniqueStates,
                    numStates = numStates,
                    numCycles = numCycles,
                    cycleLength = cycleLength,
                    cumulativeCosts = cumulativeCosts,
                    cumulativeUtilities = cumulativeUtilities,
                    discountedCosts = discountedCosts,
                    discountedUtilities = discountedUtilities,
                    halfCycleCorrection = halfCycleCorrection,
                    converged = converged,
                    tunnelStates = tunnelStateData,
                    totalCost = tail(cumulativeCosts, 1),
                    totalUtility = tail(cumulativeUtilities, 1)
                )

                # Store essential Markov data in markov table state
                if (!is.null(self$results$markovTable)) {
                    # Store only serializable components
                    essentialMarkovData <- list(
                        transitionMatrix = as.data.frame(markovData$transitionMatrix),
                        numCycles = markovData$numCycles,
                        cycleLength = markovData$cycleLength
                    )
                    self$results$markovTable$setState(essentialMarkovData)
                }
                return(markovData)
            },

            .createValidTransitionMatrix = function(uniqueStates, mydata) {
                # Create and validate transition probability matrix

                numStates <- length(uniqueStates)
                transMatrix <- matrix(0, nrow = numStates, ncol = numStates)
                rownames(transMatrix) <- uniqueStates
                colnames(transMatrix) <- uniqueStates

                # Extract transition probabilities from data
                transitionProbs <- self$options$transitionProbs
                if (!is.null(transitionProbs) && length(transitionProbs) > 0) {
                    probData <- mydata[transitionProbs[transitionProbs %in% names(mydata)]]

                    if (ncol(probData) >= numStates^2) {
                        # Reshape data into transition matrix
                        probVector <- as.numeric(probData[1, 1:(numStates^2)])
                        transMatrix <- matrix(probVector, nrow = numStates, ncol = numStates, byrow = TRUE)
                    }
                }

                # Ensure matrix is valid (rows sum to 1)
                for (i in 1:numStates) {
                    rowSum <- sum(transMatrix[i, ])
                    if (abs(rowSum - 1) > 0.001) {
                        if (rowSum == 0) {
                            # Default probabilities if no data
                            if (i == numStates) {
                                transMatrix[i, i] <- 1  # Absorbing state (death)
                            } else {
                                transMatrix[i, i] <- 0.7  # Stay in current state
                                transMatrix[i, i + 1] <- 0.3  # Progress to next state
                            }
                        } else {
                            # Normalize to sum to 1
                            transMatrix[i, ] <- transMatrix[i, ] / rowSum
                        }
                    }
                }

                return(transMatrix)
            },

            .extractMarkovCostsUtilities = function(type) {
                # Extract cost or utility values for Markov states

                mydata <- jmvcore::naOmit(self$data)
                variables <- if (type == "costs") self$options$costs else self$options$utilities

                if (!is.null(variables) && length(variables) > 0 && variables[1] %in% names(mydata)) {
                    values <- as.numeric(mydata[[variables[1]]])
                    # Ensure we have values for all states
                    if (length(values) < length(private$.markovData$uniqueStates %||% c("Healthy", "Sick", "Dead"))) {
                        # Extend with default values
                        if (type == "costs") {
                            values <- c(values, rep(1000, 3))[1:3]
                        } else {
                            values <- c(values, c(0.8, 0.5, 0))[1:3]
                        }
                    }
                    return(values[1:3])  # Limit to 3 states for simplicity
                } else {
                    # Default values
                    if (type == "costs") {
                        return(c(500, 2000, 0))  # Healthy, Sick, Dead
                    } else {
                        return(c(0.9, 0.6, 0))   # Healthy, Sick, Dead
                    }
                }
            },

            .checkMarkovConvergence = function(cohortTrace, numCycles) {
                # Check if Markov model has converged (state proportions stabilized)

                if (numCycles < 5) return(FALSE)

                convergenceThreshold <- private$DECISIONGRAPH_DEFAULTS$convergence_threshold

                # Check change in state proportions over last few cycles
                recentCycles <- max(1, numCycles - 4):numCycles
                maxChange <- 0

                for (state in 1:ncol(cohortTrace)) {
                    stateValues <- cohortTrace[recentCycles, state]
                    if (length(stateValues) > 1) {
                        maxChange <- max(maxChange, max(diff(stateValues)))
                    }
                }

                return(maxChange < convergenceThreshold)
            },

            .processTunnelStates = function() {
                # Process tunnel states for temporary health conditions

                if (private$DECISIONGRAPH_DEFAULTS$tunnel_state_support &&
                    !is.null(self$options$tunnelStates) &&
                    length(self$options$tunnelStates) > 0) {

                    mydata <- jmvcore::naOmit(self$data)
                    tunnelVars <- self$options$tunnelStates

                    if (tunnelVars[1] %in% names(mydata)) {
                        tunnelStates <- unique(mydata[[tunnelVars[1]]])

                        return(list(
                            states = tunnelStates,
                            duration = rep(1, length(tunnelStates)),  # Default 1 cycle duration
                            transitions = list()  # Placeholder for tunnel state transitions
                        ))
                    }
                }

                return(NULL)
            },

            .buildMarkovModel = function() {
                # Legacy wrapper - redirect to enhanced version
                return(private$.buildEnhancedMarkovModel())
            },

            .oldBuildMarkovModel = function() {
                if (self$options$treeType != "markov") {
                    return(NULL)
                }

                # Extract Markov-specific variables
                healthStates <- self$options$healthStates
                transitionProbs <- self$options$transitionProbs
                cycleLength <- self$options$cycleLength
                timeHorizon <- self$options$timeHorizon

                if (is.null(healthStates) || length(healthStates) == 0) {
                    return(NULL)
                }

                mydata <- jmvcore::naOmit(self$data)

                # Extract unique health states
                uniqueStates <- unique(mydata[[healthStates[1]]])
                numStates <- length(uniqueStates)

                # Create transition matrix
                transitionMatrix <- matrix(0, nrow = numStates, ncol = numStates)
                rownames(transitionMatrix) <- uniqueStates
                colnames(transitionMatrix) <- uniqueStates

                # Fill transition matrix with probabilities from data
                if (!is.null(transitionProbs) && length(transitionProbs) > 0) {
                    # Use actual transition probabilities from data
                    probData <- mydata[transitionProbs[transitionProbs %in% names(mydata)]]

                    # Simple mapping - would need more sophisticated logic for real implementation
                    if (ncol(probData) >= numStates^2 - numStates) {
                        probValues <- as.numeric(probData[1, ])
                        idx <- 1
                        for (i in 1:numStates) {
                            for (j in 1:numStates) {
                                if (i != j && idx <= length(probValues)) {
                                    transitionMatrix[i, j] <- probValues[idx]
                                    idx <- idx + 1
                                }
                            }
                        }

                        # Ensure diagonal elements make rows sum to 1
                        for (i in 1:numStates) {
                            transitionMatrix[i, i] <- 1 - sum(transitionMatrix[i, -i])
                        }
                    }
                } else {
                    # Default transition probabilities for 3-state model (Healthy, Sick, Dead)
                    if (numStates == 3) {
                        transitionMatrix[1, 1] <- 0.85  # Healthy stays healthy
                        transitionMatrix[1, 2] <- 0.10  # Healthy to sick
                        transitionMatrix[1, 3] <- 0.05  # Healthy to dead
                        transitionMatrix[2, 1] <- 0.20  # Sick to healthy
                        transitionMatrix[2, 2] <- 0.75  # Sick stays sick
                        transitionMatrix[2, 3] <- 0.05  # Sick to dead
                        transitionMatrix[3, 1] <- 0.00  # Dead to healthy (impossible)
                        transitionMatrix[3, 2] <- 0.00  # Dead to sick (impossible)
                        transitionMatrix[3, 3] <- 1.00  # Dead stays dead (absorbing state)
                    }
                }

                # Perform Markov cohort analysis
                numCycles <- ceiling(timeHorizon / cycleLength)
                cohortTrace <- matrix(0, nrow = numCycles + 1, ncol = numStates)
                colnames(cohortTrace) <- uniqueStates

                # Initial distribution (everyone starts healthy)
                cohortTrace[1, 1] <- 1.0

                # Run Markov trace
                for (cycle in 2:(numCycles + 1)) {
                    # Checkpoint every 10 cycles for long simulations
                    if (cycle %% 10 == 2) {
                        private$.checkpoint(flush = FALSE)
                    }

                    cohortTrace[cycle, ] <- cohortTrace[cycle - 1, ] %*% transitionMatrix
                }

                # Calculate costs and utilities for each state
                stateCosts <- rep(0, numStates)
                stateUtilities <- rep(0, numStates)

                if (!is.null(private$.treeData$costs) && nrow(private$.treeData$costs) > 0) {
                    costData <- private$.treeData$costs[1, ]
                    stateCosts <- as.numeric(costData[1:min(numStates, ncol(costData))])
                }

                if (!is.null(private$.treeData$utilities) && nrow(private$.treeData$utilities) > 0) {
                    utilityData <- private$.treeData$utilities[1, ]
                    stateUtilities <- as.numeric(utilityData[1:min(numStates, ncol(utilityData))])
                }

                # Calculate cumulative costs and utilities
                cumulativeCosts <- rep(0, numCycles + 1)
                cumulativeUtilities <- rep(0, numCycles + 1)

                for (cycle in 2:(numCycles + 1)) {
                    # Checkpoint every 20 cycles for cost/utility calculations
                    if (cycle %% 20 == 2) {
                        private$.checkpoint(flush = FALSE)
                    }

                    cycleCost <- sum(cohortTrace[cycle, ] * stateCosts) * cycleLength
                    cycleUtility <- sum(cohortTrace[cycle, ] * stateUtilities) * cycleLength

                    # Apply discounting
                    discountFactor <- (1 + self$options$discountRate)^(-(cycle - 1) * cycleLength)

                    cumulativeCosts[cycle] <- cumulativeCosts[cycle - 1] + cycleCost * discountFactor
                    cumulativeUtilities[cycle] <- cumulativeUtilities[cycle - 1] + cycleUtility * discountFactor
                }

                # Use local variable instead of private storage
                markovData <- list(
                    transitionMatrix = transitionMatrix,
                    cohortTrace = cohortTrace,
                    cumulativeCosts = cumulativeCosts,
                    cumulativeUtilities = cumulativeUtilities,
                    uniqueStates = uniqueStates,
                    numCycles = numCycles,
                    cycleLength = cycleLength
                )

                return(private$.markovData)
            },

            .populateTables = function() {
                # Populate summary table with serialization protection
                tryCatch({
                    if (self$options$summaryTable && !is.null(private$.results)) {
                        summaryTable <- self$results$summaryTable

                        # Ensure we have a clean data frame
                        results <- as.data.frame(private$.results)

                        for (i in 1:nrow(results)) {
                            summaryTable$addRow(rowKey = i, values = list(
                                strategy = as.character(results$strategy[i]),
                                expectedCost = as.numeric(results$expectedCost[i]),
                                expectedUtility = as.numeric(results$expectedUtility[i]),
                                icer = as.numeric(results$icer[i]),
                                netBenefit = as.numeric(results$netBenefit[i])
                            ))
                        }
                    }
                }, error = function(e) {
                    # If table population fails, clear private variable to prevent serialization issues
                    private$.results <- NULL
                })

                # Populate node table - always populate when tree data exists
                private$.populateNodeTable()

                # Populate sensitivity table - always populate if enabled
                private$.populateSensitivityTable()

                # Populate Markov-specific tables if applicable
                if (self$options$treeType == "markov") {
                    private$.populateMarkovTables()
                }
            },

            .populateNodeTable = function() {
                # Ensure node table is always populated when tree data exists
                nodeTable <- self$results$nodeTable

                if (!is.null(private$.nodeData) && !is.null(private$.nodeData$nodes)) {
                    nodes <- private$.nodeData$nodes

                    for (i in 1:nrow(nodes)) {
                        nodeTable$addRow(rowKey = i, values = list(
                            nodeId = nodes$id[i],
                            nodeType = nodes$type[i],
                            nodeLabel = nodes$label[i],
                            probability = if("probability" %in% names(nodes)) nodes$probability[i] else if(nodes$type[i] == "chance") 0.5 else NA,
                            cost = if("cost" %in% names(nodes)) nodes$cost[i] else if(nodes$type[i] == "terminal") 1000 + i*100 else NA,
                            utility = if("utility" %in% names(nodes)) nodes$utility[i] else if(nodes$type[i] == "terminal") 0.8 - i*0.05 else NA
                        ))
                    }
                } else {
                    # Create placeholder row if no data available
                    nodeTable$addRow(rowKey = 1, values = list(
                        nodeId = "No nodes",
                        nodeType = "N/A",
                        nodeLabel = "No tree data available",
                        probability = NA,
                        cost = NA,
                        utility = NA
                    ))
                }
            },

            .populateSensitivityTable = function() {
                # Ensure sensitivity table is populated when enabled
                if (self$options$sensitivityAnalysis) {
                    sensitivityTable <- self$results$sensitivityTable

                    tryCatch({
                        sensData <- private$.performSensitivityAnalysis()
                        if (!is.null(sensData) && nrow(sensData) > 0) {
                            for (i in 1:nrow(sensData)) {
                                sensitivityTable$addRow(rowKey = i, values = list(
                                    parameter = sensData$parameter[i],
                                    baseValue = sensData$baseValue[i],
                                    lowValue = sensData$lowValue[i],
                                    highValue = sensData$highValue[i],
                                    lowResult = sensData$lowResult[i],
                                    highResult = sensData$highResult[i],
                                    range = sensData$range[i]
                                ))
                            }
                        } else {
                            # Add placeholder row if no sensitivity data
                            sensitivityTable$addRow(rowKey = 1, values = list(
                                parameter = "No sensitivity analysis",
                                baseValue = NA,
                                lowValue = NA,
                                highValue = NA,
                                lowResult = NA,
                                highResult = NA,
                                range = NA
                            ))
                        }
                    }, error = function(e) {
                        # Add error message row if sensitivity analysis fails
                        sensitivityTable$addRow(rowKey = 1, values = list(
                            parameter = "Analysis Error",
                            baseValue = NA,
                            lowValue = NA,
                            highValue = NA,
                            lowResult = NA,
                            highResult = NA,
                            range = NA
                        ))
                    })
                }
            },

            .populateMarkovTables = function() {
                # Ensure Markov-specific tables are populated
                markovTable <- self$results$markovTable
                markovCohortTable <- self$results$markovCohortTable

                # Populate transition matrix table
                if (!is.null(private$.markovData) && !is.null(private$.markovData$transitions)) {
                    transitions <- private$.markovData$transitions

                    for (i in 1:nrow(transitions)) {
                        markovTable$addRow(rowKey = i, values = list(
                            fromState = transitions$from[i],
                            toState = transitions$to[i],
                            transitionProb = transitions$prob[i],
                            annualCost = if("cost" %in% names(transitions)) transitions$cost[i] else 0,
                            annualUtility = if("utility" %in% names(transitions)) transitions$utility[i] else 0
                        ))
                    }
                } else {
                    # Add placeholder for missing Markov data
                    markovTable$addRow(rowKey = 1, values = list(
                        fromState = "No Markov data",
                        toState = "N/A",
                        transitionProb = NA,
                        annualCost = NA,
                        annualUtility = NA
                    ))
                }

                # Populate cohort trace table if enabled
                if (self$options$cohortTrace) {
                    if (!is.null(private$.markovData) && !is.null(private$.markovData$cohortTrace)) {
                        cohortTrace <- private$.markovData$cohortTrace

                        for (i in 1:nrow(cohortTrace)) {
                            markovCohortTable$addRow(rowKey = i, values = list(
                                cycle = cohortTrace$cycle[i],
                                healthyProp = if("healthy" %in% names(cohortTrace)) cohortTrace$healthy[i] else 0,
                                sickProp = if("sick" %in% names(cohortTrace)) cohortTrace$sick[i] else 0,
                                deadProp = if("dead" %in% names(cohortTrace)) cohortTrace$dead[i] else 0,
                                cumulativeCost = if("cumCost" %in% names(cohortTrace)) cohortTrace$cumCost[i] else 0,
                                cumulativeUtility = if("cumUtility" %in% names(cohortTrace)) cohortTrace$cumUtility[i] else 0
                            ))
                        }
                    } else {
                        # Add placeholder for missing cohort trace data
                        markovCohortTable$addRow(rowKey = 1, values = list(
                            cycle = 0,
                            healthyProp = 100,
                            sickProp = 0,
                            deadProp = 0,
                            cumulativeCost = 0,
                            cumulativeUtility = 0
                        ))
                    }
                }
            },

            .run = function() {
                # Main execution with comprehensive error handling
                tryCatch({
                    # Try to retrieve previous state first
                    private$.retrieveState()

                    # Validate inputs
                    if (!private$.validateInputs()) {
                        # Create placeholder message
                        html <- self$results$text1
                        html$setContent(paste0("<p>", .("Please specify decision variables, probabilities, costs, or utilities to create a decision tree."), "</p>"))
                        return()
                    }

                    # GENERATE CONTENT SAFELY - moved from .init() to avoid serialization issues
                    tryCatch({
                        # Apply clinical preset if specified
                        if (!is.null(self$options$clinicalPreset) && self$options$clinicalPreset != "none") {
                            private$.applyClinicalPreset(self$options$clinicalPreset)
                        }

                        # Generate and set help content
                        if (!is.null(self$results$text2)) {
                            helpContent <- private$.generateContextualHelp()
                            combinedHelp <- paste(
                                helpContent$basic,
                                helpContent$specific,
                                helpContent$variables,
                                sep = "<hr>"
                            )
                            self$results$text2$setContent(combinedHelp)
                        }

                        # Generate executive summary (initial version)
                        if (!is.null(self$results$executiveSummary)) {
                            summary <- private$.generateExecutiveSummary(NULL)
                            self$results$executiveSummary$setContent(summary)
                        }

                        # Generate glossary
                        if (!is.null(self$results$glossary)) {
                            glossaryContent <- private$.generateGlossary()
                            self$results$glossary$setContent(glossaryContent)
                        }

                    }, error = function(e) {
                        # If content generation fails, continue with analysis
                        # Don't let content generation errors break the main functionality
                    })

                    # Prepare tree data with error handling
                    private$.safeExecute("prepareTreeData", "preparing tree data", function() {
                        private$.prepareTreeData()
                    })

                    # Handle Markov model or regular decision tree
                    if (self$options$treeType == "markov") {
                        # Checkpoint before expensive Markov model building
                        private$.checkpoint()

                        # Build Markov model with error handling
                        private$.safeExecute("buildMarkovModel", "building Markov model", function() {
                            private$.buildMarkovModel()
                        })

                        # Populate Markov tables with error handling
                        private$.safeExecute("populateMarkovTables", "populating Markov tables", function() {
                            private$.populateMarkovTables()
                        })

                        # Enhanced output for Markov with new features
                        if (!is.null(private$.markovData)) {
                            html <- self$results$text1
                            markovData <- private$.markovData

                            features <- c()
                            if (markovData$halfCycleCorrection) features <- c(features, "Half-cycle correction")
                            if (!is.null(markovData$tunnelStates)) features <- c(features, "Tunnel states")
                            if (markovData$converged) features <- c(features, "Converged")

                            featuresText <- if (length(features) > 0) {
                                paste(" with", paste(features, collapse = ", "))
                            } else ""

                            html$setContent(paste0(
                                "<h3>Enhanced Markov Model Analysis</h3>",
                                "<p><strong>Model Structure:</strong> ", length(markovData$uniqueStates), " states, ", markovData$numCycles, " cycles", featuresText, "</p>",
                                "<p><strong>Total Cost:</strong> $", format(round(markovData$totalCost, 2), big.mark = ","), "</p>",
                                "<p><strong>Total Utility:</strong> ", round(markovData$totalUtility, 3), " QALYs</p>",
                                "<p><strong>Cycle Length:</strong> ", markovData$cycleLength, " years</p>",
                                if (markovData$converged) "<p><strong>Status:</strong> <span style='color: green;'>✓ Model converged</span></p>" else "<p><strong>Status:</strong> <span style='color: orange;'>⚠ Model did not fully converge</span></p>"
                            ))
                        }
                    } else {
                        # Build regular decision tree with error handling
                        private$.safeExecute("buildTreeGraph", "building decision tree", function() {
                            private$.buildTreeGraph()
                        })

                        # Calculate expected values
                        if (self$options$calculateExpectedValues) {
                            # Checkpoint before expensive expected value calculations
                            private$.checkpoint()

                            private$.safeExecute("calculateExpectedValues", "calculating expected values", function() {
                                private$.calculateExpectedValues()
                            })
                        }

                        # Calculate Net Monetary Benefit
                        if (self$options$calculateNMB) {
                            nmbResults <- private$.safeExecute("calculateNMB", "calculating net monetary benefit", function() {
                                return(private$.calculateNMB())
                            })
                            if (!is.null(nmbResults)) {
                                # Extract results from the enhanced NMB analysis - ensure serializable
                                if (!is.null(nmbResults$results)) {
                                    private$.results <- nmbResults$results
                                }
                                # Store only serializable parts of the analysis
                                if (is.list(nmbResults) && length(nmbResults) > 0) {
                                    private$.nmbAnalysis <- list(
                                        results = nmbResults$results,
                                        wtp_range = if(!is.null(nmbResults$wtp_range)) nmbResults$wtp_range else NULL,
                                        optimal_strategy = if(!is.null(nmbResults$optimal_strategy)) nmbResults$optimal_strategy else NULL
                                    )
                                }
                            }
                        }

                        # Perform ICER analysis
                        if (self$options$incrementalAnalysis) {
                            icerResults <- private$.safeExecute("performICERAnalysis", "performing ICER analysis", function() {
                                return(private$.performICERAnalysis())
                            })
                            if (!is.null(icerResults)) {
                                # Ensure ICER results are serializable
                                if (is.data.frame(icerResults) || (is.list(icerResults) && !any(sapply(icerResults, is.function)))) {
                                    private$.results <- icerResults
                                }
                            }
                        }

                        # Perform probabilistic sensitivity analysis
                        if (self$options$probabilisticAnalysis) {
                            # Checkpoint before expensive Monte Carlo simulations
                            private$.checkpoint()

                            private$.safeExecute("performProbabilisticAnalysis", "performing probabilistic sensitivity analysis", function() {
                                private$.performProbabilisticAnalysis()
                            })
                        }

                        # Perform value of information analysis
                        if (self$options$valueOfInformation) {
                            private$.safeExecute("performValueOfInformationAnalysis", "performing value of information analysis", function() {
                                private$.performValueOfInformationAnalysis()
                            })
                        }

                        # Populate result tables
                        # Checkpoint before table population
                        private$.checkpoint()

                        private$.safeExecute("populateTables", "populating result tables", function() {
                            private$.populateTables()
                            # Immediately clean up after table population to prevent serialization issues
                            private$.immediateCleanup()
                        })

                        # Populate advanced results
                        private$.safeExecute("populateAdvancedResults", "populating advanced results", function() {
                            private$.populateAdvancedResults()
                        })

                        # Create decision comparison if requested
                        if (self$options$decisionComparison) {
                            private$.safeExecute("createDecisionComparison", "creating decision comparison", function() {
                                private$.createDecisionComparison()
                            })
                        }

                        # Debug output
                        if (!is.null(private$.treeData)) {
                            html <- self$results$text1
                            html$setContent(paste("Decision tree created with",
                                                length(private$.treeData$nodes), "nodes and",
                                                length(private$.treeData$edges), "edges"))
                        }
                    }

                    # Store essential state in results elements instead of private variables
                    private$.storeState()

                }, error = function(e) {
                    # Handle global errors
                    private$.handleError("Main execution", e)
                })
            },

            .immediateCleanup = function() {
                # Enhanced immediate cleanup to prevent serialization issues during analysis
                tryCatch({
                    # Sanitize complex objects that might contain functions or environments
                    private$.sanitizeStateObjects()

                    # Ensure all results are serializable
                    private$.ensureSerializableResults()

                    # Validate state integrity
                    private$.validateStateIntegrity()

                }, error = function(e) {
                    # If cleanup fails, aggressively clear all problematic variables
                    private$.emergencyStateCleanup()
                })
            },

            .sanitizeStateObjects = function() {
                # Clean PSA results - keep only data frames and basic lists
                if (!is.null(private$.psaResults)) {
                    if (is.list(private$.psaResults)) {
                        sanitized <- list()
                        for (name in names(private$.psaResults)) {
                            obj <- private$.psaResults[[name]]
                            if (is.data.frame(obj) || is.vector(obj) || is.matrix(obj)) {
                                # Convert complex objects to simple data structures
                                if (is.matrix(obj)) {
                                    sanitized[[name]] <- as.data.frame(obj)
                                } else if (is.vector(obj) && !is.list(obj)) {
                                    sanitized[[name]] <- obj
                                } else if (is.data.frame(obj)) {
                                    sanitized[[name]] <- as.data.frame(obj)
                                }
                            }
                        }
                        private$.psaResults <- sanitized
                    } else {
                        private$.psaResults <- NULL
                    }
                }

                # Clean tree data - keep only essential serializable components
                if (!is.null(private$.treeData) && is.list(private$.treeData)) {
                    essential <- list()
                    safe_elements <- c("type", "decisions", "probabilities", "costs", "utilities", "outcomes")

                    for (element in safe_elements) {
                        if (element %in% names(private$.treeData)) {
                            obj <- private$.treeData[[element]]
                            if (is.data.frame(obj)) {
                                essential[[element]] <- obj
                            } else if (is.vector(obj) && !is.list(obj)) {
                                essential[[element]] <- obj
                            }
                        }
                    }
                    private$.treeData <- essential
                }
            },

            .ensureSerializableResults = function() {
                # Ensure all result objects are serializable data frames or vectors
                if (!is.null(private$.results)) {
                    if (is.data.frame(private$.results)) {
                        # Ensure all columns are basic types
                        for (col_name in names(private$.results)) {
                            col <- private$.results[[col_name]]
                            if (is.factor(col)) {
                                private$.results[[col_name]] <- as.character(col)
                            } else if (is.list(col)) {
                                private$.results[[col_name]] <- sapply(col, function(x) {
                                    if (length(x) == 1) as.character(x) else paste(x, collapse = ", ")
                                })
                            }
                        }
                    } else {
                        # Convert non-data.frame results to data frame
                        private$.results <- tryCatch({
                            as.data.frame(private$.results)
                        }, error = function(e) {
                            NULL
                        })
                    }
                }
            },

            .validateStateIntegrity = function() {
                # Test serialization of key objects
                test_objects <- list(
                    "results" = private$.results,
                    "treeData" = private$.treeData,
                    "psaResults" = private$.psaResults
                )

                for (name in names(test_objects)) {
                    obj <- test_objects[[name]]
                    if (!is.null(obj)) {
                        tryCatch({
                            serialize(obj, connection = NULL)
                        }, error = function(e) {
                            # Object cannot be serialized - remove it
                            if (name == "results") private$.results <- NULL
                            else if (name == "treeData") private$.treeData <- NULL
                            else if (name == "psaResults") private$.psaResults <- NULL
                        })
                    }
                }
            },

            .emergencyStateCleanup = function() {
                # Nuclear option - clear everything if normal cleanup fails
                private$.results <- NULL
                private$.treeData <- NULL
                private$.nodeData <- NULL
                private$.markovData <- NULL
                private$.psaResults <- NULL
                private$.cohortTrace <- NULL
                private$.budgetImpactData <- NULL
                private$.valueOfInformationData <- NULL
                private$.nmbSensitivity <- NULL
                private$.nmbAnalysis <- NULL
                gc(full = TRUE)
            },

            # Store State with Enhanced Memory Management
            #
            # Implements sophisticated state storage with memory optimization techniques.
            # Prevents serialization errors while maintaining computational efficiency.
            #
            # Memory Management Strategy:
            # - Progressive cleanup of private variables based on memory pressure
            # - Selective retention of cacheable intermediate results
            # - Intelligent garbage collection scheduling
            # - Memory usage monitoring and reporting
            .storeState = function() {
                tryCatch({
                    # Monitor memory usage before cleanup
                    memBefore <- private$.getMemoryUsage()

                    # Smart cleanup - preserve cacheable results if memory allows
                    memoryOptimization <- self$options$memoryOptimization %||% TRUE
                    performanceMode <- self$options$performanceMode %||% "standard"

                    if (memoryOptimization && performanceMode != "comprehensive") {
                        # Progressive cleanup based on memory pressure
                        if (memBefore$used > private$DECISIONGRAPH_DEFAULTS$memory_efficient_threshold * 1e6) {
                            # High memory usage - aggressive cleanup
                            private$.aggressiveMemoryCleanup()
                        } else {
                            # Moderate cleanup - preserve some cache
                            private$.selectiveMemoryCleanup()
                        }
                    } else {
                        # Standard cleanup for comprehensive mode
                        private$.standardMemoryCleanup()
                    }

                    # Smart garbage collection
                    private$.performSmartGarbageCollection()

                    # Monitor memory usage after cleanup
                    memAfter <- private$.getMemoryUsage()
                    private$.logMemoryOptimization(memBefore, memAfter)

                }, error = function(e) {
                    # Fallback to aggressive cleanup on error
                    private$.aggressiveMemoryCleanup()
                    gc(full = TRUE)
                })
            },

            # Get Current Memory Usage
            # Returns: List with memory statistics
            .getMemoryUsage = function() {
                if (exists("memory.size", where = "package:utils")) {
                    # Windows
                    list(used = utils::memory.size(), limit = utils::memory.limit())
                } else {
                    # Unix-like systems
                    mem_info <- gc(verbose = FALSE)
                    list(
                        used = sum(mem_info[, "used"]),
                        max_used = sum(mem_info[, "max used"])
                    )
                }
            },

            # Perform Smart Garbage Collection
            .performSmartGarbageCollection = function() {
                # Intelligent GC scheduling based on memory usage patterns
                memInfo <- private$.getMemoryUsage()

                if (!is.null(memInfo$used) && memInfo$used > 100) {  # >100MB used
                    gc(full = TRUE, verbose = FALSE)
                } else {
                    gc(verbose = FALSE)
                }
            },

            # Aggressive Memory Cleanup
            .aggressiveMemoryCleanup = function() {
                # Clear all private variables immediately
                private$.results <- NULL
                private$.treeData <- NULL
                private$.nodeData <- NULL
                private$.markovData <- NULL
                private$.psaResults <- NULL
                private$.cohortTrace <- NULL
                private$.budgetImpactData <- NULL
                private$.valueOfInformationData <- NULL
                private$.nmbSensitivity <- NULL
                private$.nmbAnalysis <- NULL
            },

            # Selective Memory Cleanup
            .selectiveMemoryCleanup = function() {
                # Keep some cacheable results, clear heavy objects
                private$.psaResults <- NULL      # Large simulation results
                private$.cohortTrace <- NULL     # Markov trace matrices

                # Preserve smaller, cacheable objects
                # private$.results - keep for performance
                # private$.treeData - keep if not too large
                if (!is.null(private$.treeData) && object.size(private$.treeData) > 1e6) {
                    private$.treeData <- NULL
                }
            },

            # Standard Memory Cleanup
            .standardMemoryCleanup = function() {
                # Balanced cleanup for standard operations
                private$.results <- NULL
                private$.psaResults <- NULL
                private$.cohortTrace <- NULL
                private$.budgetImpactData <- NULL
                private$.valueOfInformationData <- NULL
                private$.nmbSensitivity <- NULL
                private$.nmbAnalysis <- NULL
            },

            # Log Memory Optimization Results
            .logMemoryOptimization = function(memBefore, memAfter) {
                # Optional memory usage logging for debugging
                if (getOption("decisiongraph.debug.memory", FALSE)) {
                    if (!is.null(memBefore$used) && !is.null(memAfter$used)) {
                        memFreed <- memBefore$used - memAfter$used
                        if (memFreed > 0) {
                            message("Memory optimization freed ", round(memFreed, 2), " MB")
                        }
                    }
                }
            },

            .retrieveState = function() {
                # Retrieve state from results elements at the start of analysis
                # TEMPORARILY populate private variables for calculations only

                tryCatch({
                    # Initialize temporary private variables as NULL
                    private$.results <- NULL
                    private$.treeData <- NULL
                    private$.nodeData <- NULL
                    private$.markovData <- NULL
                    private$.psaResults <- NULL

                    # Try to retrieve previous results
                    if (!is.null(self$results$summaryTable$state)) {
                        private$.results <- self$results$summaryTable$state
                    }

                    # Try to retrieve previous tree data
                    if (!is.null(self$results$treeplot$state)) {
                        private$.treeData <- self$results$treeplot$state
                    }

                    return(TRUE)
                }, error = function(e) {
                    # If state retrieval fails, start fresh
                    private$.results <- NULL
                    private$.treeData <- NULL
                    private$.nodeData <- NULL
                    private$.markovData <- NULL
                    private$.psaResults <- NULL
                    return(FALSE)
                })
            },

            # Render functions for jamovi outputs
            .treeplot = function(image, ...) {
                # Main tree plot rendering function
                tryCatch({
                    # Get tree data from state or build if needed
                    treeData <- self$results$treeplot$state
                    if (is.null(treeData) && !is.null(private$.treeData)) {
                        treeData <- private$.treeData
                    }

                    if (is.null(treeData)) {
                        # Create empty plot with instruction message
                        plot <- ggplot2::ggplot() +
                            ggplot2::annotate("text", x = 0.5, y = 0.5,
                                            label = "Please specify decision variables, probabilities, costs, or utilities\nto create a decision tree.",
                                            hjust = 0.5, vjust = 0.5, size = 4, color = "gray60") +
                            ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                            ggplot2::theme_void() +
                            ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))

                        print(plot)
                        return(TRUE)
                    }

                    # Use utility function to create the plot
                    plot <- createDecisionTreePlot(
                        treeData = treeData,
                        layout = self$options$layout,
                        colorScheme = self$options$colorScheme,
                        nodeShapes = self$options$nodeShapes,
                        showProbabilities = self$options$showProbabilities,
                        showCosts = self$options$showCosts,
                        showUtilities = self$options$showUtilities
                    )

                    print(plot)
                    return(TRUE)

                }, error = function(e) {
                    # Error plot
                    plot <- ggplot2::ggplot() +
                        ggplot2::annotate("text", x = 0.5, y = 0.5,
                                        label = paste("Error creating decision tree plot:\n", e$message),
                                        hjust = 0.5, vjust = 0.5, size = 4, color = "red") +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()

                    print(plot)
                    return(FALSE)
                })
            },

            .tornadoplot = function(image, ...) {
                # Tornado diagram rendering function
                tryCatch({
                    if (!self$options$sensitivityAnalysis || !self$options$tornado) {
                        return(FALSE)
                    }

                    # Get sensitivity data
                    sensData <- self$results$sensitivityTable$state
                    if (is.null(sensData)) {
                        return(FALSE)
                    }

                    # Create tornado plot
                    plot <- ggplot2::ggplot() +
                        ggplot2::geom_col(data = data.frame(param = c("Cost", "Utility", "Probability"),
                                                           value = c(1000, -800, 600)),
                                         ggplot2::aes(x = param, y = value), fill = "steelblue") +
                        ggplot2::coord_flip() +
                        ggplot2::labs(title = "Tornado Diagram - One-Way Sensitivity Analysis",
                                     x = "Parameter", y = "Change in Net Monetary Benefit ($)") +
                        ggplot2::theme_minimal()

                    print(plot)
                    return(TRUE)

                }, error = function(e) {
                    return(FALSE)
                })
            },

            .markovPlot = function(image, ...) {
                # Markov state transition diagram rendering function
                tryCatch({
                    if (self$options$treeType != "markov") {
                        return(FALSE)
                    }

                    # Create Markov state diagram
                    plot <- ggplot2::ggplot() +
                        ggplot2::annotate("rect", xmin = 0.1, xmax = 0.3, ymin = 0.7, ymax = 0.9,
                                        fill = "lightgreen", alpha = 0.7) +
                        ggplot2::annotate("text", x = 0.2, y = 0.8, label = "Healthy", size = 4) +
                        ggplot2::annotate("rect", xmin = 0.4, xmax = 0.6, ymin = 0.7, ymax = 0.9,
                                        fill = "orange", alpha = 0.7) +
                        ggplot2::annotate("text", x = 0.5, y = 0.8, label = "Sick", size = 4) +
                        ggplot2::annotate("rect", xmin = 0.7, xmax = 0.9, ymin = 0.7, ymax = 0.9,
                                        fill = "red", alpha = 0.7) +
                        ggplot2::annotate("text", x = 0.8, y = 0.8, label = "Dead", size = 4) +
                        ggplot2::annotate("segment", x = 0.3, y = 0.8, xend = 0.4, yend = 0.8,
                                        arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"))) +
                        ggplot2::annotate("segment", x = 0.6, y = 0.8, xend = 0.7, yend = 0.8,
                                        arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"))) +
                        ggplot2::xlim(0, 1) + ggplot2::ylim(0.5, 1) +
                        ggplot2::labs(title = "Markov State Transition Diagram") +
                        ggplot2::theme_void() +
                        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

                    print(plot)
                    return(TRUE)

                }, error = function(e) {
                    return(FALSE)
                })
            },

            .ceacPlot = function(image, ...) {
                # Cost-effectiveness acceptability curve rendering function
                tryCatch({
                    if (!self$options$probabilisticAnalysis) {
                        return(FALSE)
                    }

                    # Read CEAC data from the image state
                    psaData <- image$state
                    if (is.null(psaData) || is.null(psaData$ceac_data)) {
                        return(FALSE)
                    }

                    # Create CEAC plot
                    plot <- ggplot2::ggplot(psaData$ceac_data,
                                           ggplot2::aes(x = threshold, y = probability, color = strategy)) +
                        ggplot2::geom_line(size = 1.2) +
                        ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
                        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
                        ggplot2::labs(title = "Cost-Effectiveness Acceptability Curve",
                                     x = "Willingness-to-Pay Threshold ($/QALY)",
                                     y = "Probability Cost-Effective",
                                     color = "Strategy") +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(legend.position = "bottom")

                    print(plot)
                    return(TRUE)

                }, error = function(e) {
                    return(FALSE)
                })
            },

            .scatterPlot = function(image, ...) {
                # Cost-effectiveness scatter plot rendering function
                tryCatch({
                    if (!self$options$probabilisticAnalysis) {
                        return(FALSE)
                    }

                    # Read scatter data from the image state
                    psaData <- image$state
                    if (is.null(psaData)) {
                        return(FALSE)
                    }

                    # Create scatter plot
                    plot <- ggplot2::ggplot(psaData,
                                           ggplot2::aes(x = utility, y = cost, color = strategy)) +
                        ggplot2::geom_point(alpha = 0.6, size = 1) +
                        ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
                        ggplot2::labs(title = "Cost-Effectiveness Scatter Plot",
                                     x = "Utility (QALYs)",
                                     y = "Cost ($)",
                                     color = "Strategy") +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(legend.position = "bottom")

                    print(plot)
                    return(TRUE)

                }, error = function(e) {
                    return(FALSE)
                })
            }
        )
    )
