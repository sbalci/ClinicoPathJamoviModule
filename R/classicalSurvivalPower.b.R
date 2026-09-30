# Lachin-Foulkes power at one or more total sample sizes.
#
# gsDesign::nSurvival() computes sqrt(n) = (z_alpha * sqrt(V0) + z_beta * sqrt(V1)) / |log HR|,
# which is a straight line in z_beta = qnorm(1 - beta). Its values at beta = 0.5 and 0.25
# give the intercept and slope, so power = pnorm(z_beta) is exact for any n > 0: no root
# search, no upper limit on n. The event fraction E/N does not depend on beta, so the
# expected events at n are n * E/N. Needs z_alpha > 0 (alpha / sided < 0.5, which the UI
# guarantees). The analysis rejects invalid designs first, in .validate_options().
.classicalSurvivalPower_lf_power <- function(sample_size, design) {
    if (!is.numeric(sample_size) || length(sample_size) == 0L ||
        any(!is.finite(sample_size) | sample_size <= 0)) {
        stop("Sample size must be a positive finite number.")
    }
    at_beta <- function(beta) do.call(gsDesign::nSurvival, c(design, list(beta = beta)))
    half <- at_beta(0.5)
    slope <- (sqrt(at_beta(0.25)$n) - sqrt(half$n)) / qnorm(0.75)
    if (!is.finite(half$n) || half$n <= 0 || !is.finite(half$nEvents) ||
        !is.finite(slope) || slope <= 0) {
        stop("The Lachin-Foulkes formulas give no finite sample size for this design.")
    }
    z <- (sqrt(sample_size) - sqrt(half$n)) / slope
    list(power = pnorm(z), beta = pnorm(z, lower.tail = FALSE),
         events = sample_size * half$nEvents / half$n)
}

#' @title Survival Analysis Power & Sample Size
#' @description
#' Power analysis and sample size calculation for survival studies using Lachin-Foulkes
#' and Schoenfeld methods from the gsDesign package. This module provides comprehensive
#' power calculations for clinical trials with time-to-event endpoints.
#'
#' @details
#' This module implements the functionality requested in GitHub Issue #72,
#' providing comprehensive power analysis capabilities for survival studies.
#' Two main methods are supported:
#'
#' Lachin-Foulkes Method:
#' - Full study design approach
#' - Accounts for accrual duration, follow-up period, dropout rates
#' - Supports uniform and exponential patient entry patterns
#' - Provides sample size and number of events
#'
#' Schoenfeld Method:
#' - Events-based approximation
#' - Based on asymptotic normal distribution of log-rank statistic
#' - Simpler calculations focused on number of events
#' - Useful for quick estimates and design comparisons
#'
#' @param calculation_type Type of calculation (sample size, power, events, hazard ratio).
#' @param method Calculation method (Lachin-Foulkes or Schoenfeld).
#' @param hazard_control Event hazard rate for control group.
#' @param hazard_treatment Event hazard rate for treatment group.
#' @param study_duration Maximum study duration.
#' @param accrual_duration Patient accrual duration.
#' @param alpha Type I error rate.
#' @param beta Type II error rate.
#'
#' @return Power analysis results with sample size, number of events, and study design parameters.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom gsDesign nSurvival nEvents hrn2z hrz2n zn2hr
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_point geom_rect annotate
#' @importFrom ggplot2 labs scale_y_continuous scale_y_discrete scale_fill_manual theme_minimal
#' @importFrom ggplot2 theme element_text element_blank
#' @importFrom scales percent
classicalSurvivalPowerClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "classicalSurvivalPowerClass",
        inherit = classicalSurvivalPowerBase,
        private = list(
            # Internal storage
            .results_data = NULL,
            .init = function() {
                # Initialize instructions
                instructions_html <- paste(
                    "<div style='background-color: rgba(33, 181, 248, 0.14); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>",
                    "<h3 style='color: #0277bd; margin-top: 0;'>Survival Analysis Power & Sample Size</h3>",
                    "<div style='margin: 10px 0;'>",
                    "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>Power Analysis for Clinical Trials:</h4>",
                    "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                    "<li><strong>Sample Size Calculation:</strong> Determine required sample size for desired power</li>",
                    "<li><strong>Power Analysis:</strong> Calculate power given sample size and effect size</li>",
                    "<li><strong>Events Calculation:</strong> Determine number of events needed</li>",
                    "<li><strong>Hazard Ratio Detection:</strong> Minimum detectable effect size</li>",
                    "</ul>",
                    "</div>",
                    "<div style='margin: 10px 0;'>",
                    "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>Calculation Methods:</h4>",
                    "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                    "<li><strong>Lachin-Foulkes:</strong> Full study design with accrual, follow-up, and dropout</li>",
                    "<li><strong>Schoenfeld:</strong> Events-based approximation for quick estimates</li>",
                    "</ul>",
                    "</div>",
                    "<div style='margin: 10px 0;'>",
                    "<h4 style='color: #0288d1; margin: 10px 0 5px 0;'>Quick Start:</h4>",
                    "<ol style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                    "<li><strong>Choose Calculation Type:</strong> What you want to calculate</li>",
                    "<li><strong>Select Method:</strong> Lachin-Foulkes for full design, Schoenfeld for events</li>",
                    "<li><strong>Set Parameters:</strong> Hazard rates, study duration, error rates</li>",
                    "<li><strong>Review Results:</strong> Sample size, power, and study design summary</li>",
                    "</ol>",
                    "</div>",
                    "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 10px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                    "<p style='margin: 0; color: #f57c00;'><strong>Clinical Note:</strong> These calculations are essential for proper clinical trial design. Consider consultation with a biostatistician for complex studies.</p>",
                    "</div>",
                    "<p style='margin: 10px 0 0 0; color: #666; font-style: italic;'> This module implements GitHub Issue #72 using gsDesign package methods.</p>",
                    "</div>"
                )

                self$results$instructions$setContent(instructions_html)
            },
            .run = function() {
                # Clear instructions when parameters are set
                if (isTRUE(self$options$calculation_type != "") && isTRUE(self$options$method != "")) {
                    self$results$instructions$setContent("")
                }

                # Check for required packages
                if (!requireNamespace("gsDesign", quietly = TRUE)) {
                    jmvcore::reject("Package 'gsDesign' is required for survival power analysis.")
                }

                # Before the catch-all below: reject() raises a plain simpleError, which
                # that handler would show as red text instead of jamovi's error state.
                private$.validate_options()

                # Perform calculations
                tryCatch(
                    {
                        private$.calculate_power()

                        # Generate results
                        if (self$options$show_summary) {
                            private$.generate_summary()
                        }

                        if (self$options$show_formulas) {
                            private$.generate_formulas()
                        }

                        if (self$options$show_interpretation) {
                            private$.generate_interpretation()
                        }

                        if (self$options$export_results || self$options$export_power_curve) {
                            private$.generate_export_summary()
                        }
                    },
                    error = function(e) {
                        error_msg <- paste(
                            "Error in power calculation:",
                            htmltools::htmlEscape(conditionMessage(e))
                        )
                        self$results$power_results$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
                        # `export_summary` is declaratively visible whenever either
                        # export option is on, and .generate_export_summary() sits
                        # inside this tryCatch -- so a failed calculation used to
                        # leave a titled, completely empty "Reporting Summary" pane
                        # sitting under the error. Say why it is empty instead.
                        if (self$options$export_results || self$options$export_power_curve) {
                            self$results$export_summary$setContent(paste0(
                                "<div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; ",
                                "border-radius: 5px; margin: 10px 0; color: inherit;'>",
                                "<h4>Reporting Summary</h4>",
                                "<p>Nothing to summarise: the power calculation did not complete. ",
                                "See the message above.</p></div>"))
                        }
                    }
                )
            },
            # Map the `sided` List option ("one_sided"/"two_sided") to the numeric
            # code (1/2) expected by gsDesign. as.numeric() on the option string
            # returns NA, which silently propagates NA into all gsDesign results.
            .sided_num = function() {
                if (identical(self$options$sided, "two_sided")) 2 else 1
            },
            # One message per input problem. The option ranges in the .a.yaml already
            # keep hazards, durations, alpha, beta and the counts positive.
            .validate_options = function() {
                calc_type <- self$options$calculation_type
                if (self$options$method == "schoenfeld") {
                    if (calc_type %in% c("sample_size", "events") && self$options$hazard_ratio == 1) {
                        jmvcore::reject(.("A hazard ratio of 1 means the groups do not differ, so no number of events reaches the target power. Enter the hazard ratio you expect to detect."))
                    }
                    return(invisible(NULL))
                }

                # gsDesign::nSurvival only answers sample size / power for the
                # full Lachin-Foulkes design. 'events' and 'hazard_ratio' have no
                # branch there and used to fall through, leaving every pane blank.
                if (calc_type %in% c("events", "hazard_ratio")) {
                    calc_label <- switch(calc_type,
                        events = .("Number of Events Required"),
                        hazard_ratio = .("Hazard Ratio Detection")
                    )
                    jmvcore::reject(.fmt(
                        .("The Lachin-Foulkes method cannot perform the '{calculation}' calculation. Select the Schoenfeld (events-based) method for this calculation type, or keep Lachin-Foulkes and choose 'Sample Size from Power' or 'Power from Sample Size' instead."),
                        calculation = calc_label
                    ))
                }

                design <- private$.lf_design()
                if (design$lambda1 == design$lambda2) {
                    jmvcore::reject(.("The control and treatment hazard rates are equal (hazard ratio 1), so there is no difference to detect and no sample size reaches the target power. Enter different hazard rates for the two groups."))
                }
                if (design$Ts < design$Tr) {
                    jmvcore::reject(.fmt(
                        .("The total study duration ({duration}) is shorter than the accrual duration ({accrual}). Recruitment must end within the study, so lengthen the study or shorten accrual."),
                        duration = design$Ts, accrual = design$Tr
                    ))
                }
                if (design$entry == "expo") {
                    if (design$gamma == 0) {
                        jmvcore::reject(.("Exponential entry needs a non-zero gamma: a positive value means faster recruitment early in accrual, a negative value faster recruitment late. For evenly spread recruitment choose 'Uniform Entry'."))
                    }
                    # gsDesign's exponential-entry event probability divides by
                    # (hazard + dropout - gamma) for each group hazard and for their
                    # allocation-weighted mean, so it is 0/0 (NaN) when gamma equals one.
                    rates <- design$eta + c(design$lambda1, design$lambda2,
                        (design$lambda1 + design$ratio * design$lambda2) / (1 + design$ratio))
                    if (any(abs(rates - design$gamma) * design$Tr < 1e-8)) {
                        jmvcore::reject(.fmt(
                            .("Gamma ({gamma}) equals a hazard rate plus the dropout rate, where the exponential-entry formula is undefined (0/0). Change gamma slightly, for example by 0.001."),
                            gamma = design$gamma
                        ))
                    }
                }
            },
            .lf_design = function() {
                entry <- self$options$entry_type
                list(
                    lambda1 = self$options$hazard_control,
                    lambda2 = self$options$hazard_treatment,
                    Ts = self$options$study_duration,
                    Tr = self$options$accrual_duration,
                    eta = self$options$dropout_rate,
                    ratio = self$options$allocation_ratio,
                    alpha = self$options$alpha,
                    sided = private$.sided_num(),
                    entry = entry,
                    gamma = if (entry == "expo") self$options$gamma else NA
                )
            },
            # Schoenfeld (1981): power with D events = pnorm(sqrt(D * r) / (1 + r) * |log HR| - z_alpha),
            # r the allocation ratio (symmetric in r and 1/r). Written out because
            # gsDesign::nEvents(n = D) returned the wrong power for HR > 1 before gsDesign 3.10.0.
            .schoenfeld_power = function(events, hr) {
                ratio <- self$options$allocation_ratio
                pnorm(sqrt(events * ratio) / (1 + ratio) * abs(log(hr)) -
                    qnorm(1 - self$options$alpha / private$.sided_num()))
            },
            # Power at each sample size (Lachin-Foulkes) or event count (Schoenfeld); the
            # curve and its marker use the same functions as the main result.
            .power_at = function(amounts) {
                data <- private$.results_data
                if (data$method == "Schoenfeld") {
                    private$.schoenfeld_power(amounts, data$hazard_ratio)
                } else {
                    .classicalSurvivalPower_lf_power(amounts, private$.lf_design())$power
                }
            },
            .calculate_power = function() {
                calc_type <- self$options$calculation_type
                method <- self$options$method

                if (method == "lachin_foulkes") {
                    private$.calculate_lachin_foulkes()
                } else if (method == "schoenfeld") {
                    private$.calculate_schoenfeld()
                }
            },
            .calculate_lachin_foulkes = function() {
                # .validate_options() leaves only 'sample_size' and 'power' here.
                design <- private$.lf_design()
                beta <- self$options$beta
                solve_n <- self$options$calculation_type == "sample_size"

                if (solve_n) {
                    required <- do.call(gsDesign::nSurvival, c(design, list(beta = beta)))
                    if (!is.finite(required$n)) {
                        stop("The Lachin-Foulkes formulas give no finite sample size for this design.")
                    }
                    # Whole patients, rounded up: rounding to the nearest can miss the target power.
                    sample_size <- ceiling(required$n)
                } else {
                    sample_size <- self$options$sample_size_input
                }
                # Power and expected events at the sample size that is displayed.
                at_n <- .classicalSurvivalPower_lf_power(sample_size, design)

                private$.results_data <- list(
                    method = "Lachin-Foulkes",
                    calculation = if (solve_n) "Sample Size" else "Power",
                    sample_size = sample_size,
                    events = at_n$events,
                    power = if (solve_n) 1 - beta else at_n$power,
                    achieved_power = if (solve_n) at_n$power,
                    hazard_ratio = design$lambda2 / design$lambda1,
                    control_hazard = design$lambda1,
                    treatment_hazard = design$lambda2,
                    study_duration = design$Ts,
                    accrual_duration = design$Tr,
                    alpha = design$alpha,
                    beta = if (solve_n) beta else at_n$beta
                )
            },
            .calculate_schoenfeld = function() {
                calc_type <- self$options$calculation_type
                hr <- self$options$hazard_ratio
                alpha <- self$options$alpha
                beta <- self$options$beta
                ratio <- self$options$allocation_ratio
                sided <- private$.sided_num()

                if (calc_type == "sample_size" || calc_type == "events") {
                    # nEvents() squares log(HR), so this count is right for HR > 1 on every
                    # gsDesign version. Whole events, rounded up.
                    events <- ceiling(gsDesign::nEvents(
                        hr = hr,
                        alpha = alpha,
                        beta = beta,
                        ratio = ratio,
                        sided = sided
                    ))

                    private$.results_data <- list(
                        method = "Schoenfeld",
                        calculation = if (calc_type == "events") "Number of Events" else "Sample Size (Events-Based)",
                        events = events,
                        power = 1 - beta,
                        achieved_power = private$.schoenfeld_power(events, hr),
                        hazard_ratio = hr,
                        alpha = alpha,
                        beta = beta,
                        allocation_ratio = ratio
                    )
                } else if (calc_type == "power") {
                    events <- self$options$events_input
                    power <- private$.schoenfeld_power(events, hr)

                    private$.results_data <- list(
                        method = "Schoenfeld",
                        calculation = "Power",
                        events = events,
                        power = power,
                        hazard_ratio = hr,
                        alpha = alpha,
                        beta = 1 - power,
                        allocation_ratio = ratio
                    )
                } else if (calc_type == "hazard_ratio") {
                    # Calculate detectable hazard ratio
                    events <- self$options$events_input
                    z_value <- qnorm(1 - alpha / sided) + qnorm(1 - beta)

                    # Use gsDesign helper functions
                    detectable_hr <- gsDesign::zn2hr(z = z_value, n = events, ratio = ratio)

                    private$.results_data <- list(
                        method = "Schoenfeld",
                        calculation = "Detectable Hazard Ratio",
                        events = events,
                        power = 1 - beta,
                        hazard_ratio = detectable_hr,
                        alpha = alpha,
                        beta = beta,
                        allocation_ratio = ratio
                    )
                }
            },
            .generate_summary = function() {
                if (is.null(private$.results_data)) {
                    return()
                }

                data <- private$.results_data

                # Create HTML summary
                summary_html <- "<h4>Power Analysis Results</h4>"
                summary_html <- paste0(summary_html, "<div style='background-color: rgba(88, 88, 88, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>")

                # Method and calculation type
                summary_html <- paste0(summary_html, "<p><strong>Method:</strong> ", data$method, "</p>")
                summary_html <- paste0(summary_html, "<p><strong>Calculation:</strong> ", data$calculation, "</p>")

                # Key results
                if (!is.null(data$sample_size)) {
                    n_label <- if (identical(data$calculation, "Power")) .("Entered Sample Size") else .("Required Sample Size")
                    summary_html <- paste0(summary_html, "<p><strong>", n_label, ":</strong> ", format(data$sample_size, scientific = FALSE), " patients</p>")
                }

                if (!is.null(data$events)) {
                    # Entered counts are shown as typed, expected counts rounded, required counts rounded up.
                    entered <- data$method == "Schoenfeld" && data$calculation %in% c("Power", "Detectable Hazard Ratio")
                    expected <- data$method == "Lachin-Foulkes" && identical(data$calculation, "Power")
                    events_label <- if (entered) .("Entered Events") else if (expected) .("Expected Events") else .("Required Events")
                    events_shown <- if (entered) data$events else if (expected) round(data$events) else ceiling(data$events)
                    summary_html <- paste0(summary_html, "<p><strong>", events_label, ":</strong> ", format(events_shown, scientific = FALSE), " events</p>")
                }

                summary_html <- paste0(summary_html, "<p><strong>Statistical Power:</strong> ", round(data$power * 100, 1), "%</p>")
                if (!is.null(data$achieved_power)) {
                    # The required count is rounded up, so the power it reaches can exceed the target.
                    reached <- if (is.null(data$sample_size)) {
                        .fmt(.("Power with {n} events"), n = format(data$events, scientific = FALSE))
                    } else {
                        .fmt(.("Power with {n} patients"), n = format(data$sample_size, scientific = FALSE))
                    }
                    summary_html <- paste0(summary_html, "<p><strong>", reached, ":</strong> ", round(data$achieved_power * 100, 1), "%</p>")
                }
                summary_html <- paste0(summary_html, "<p><strong>Hazard Ratio:</strong> ", round(data$hazard_ratio, 3), "</p>")
                summary_html <- paste0(summary_html, "<p><strong>Type I Error (\u{03B1}):</strong> ", data$alpha, "</p>")
                summary_html <- paste0(summary_html, "<p><strong>Type II Error (\u{03B2}):</strong> ", round(data$beta, 3), "</p>")

                # Study design parameters (Lachin-Foulkes)
                if (data$method == "Lachin-Foulkes") {
                    summary_html <- paste0(summary_html, "<h5>Study Design Parameters</h5>")
                    if (!is.null(data$control_hazard)) {
                        summary_html <- paste0(summary_html, "<p><strong>Control Hazard Rate:</strong> ", round(data$control_hazard, 4), " events per time unit</p>")
                    }
                    if (!is.null(data$treatment_hazard)) {
                        summary_html <- paste0(summary_html, "<p><strong>Treatment Hazard Rate:</strong> ", round(data$treatment_hazard, 4), " events per time unit</p>")
                    }
                    if (!is.null(data$study_duration)) {
                        summary_html <- paste0(summary_html, "<p><strong>Study Duration:</strong> ", data$study_duration, " time units</p>")
                    }
                    if (!is.null(data$accrual_duration)) {
                        summary_html <- paste0(summary_html, "<p><strong>Accrual Duration:</strong> ", data$accrual_duration, " time units</p>")
                    }
                }

                # Allocation ratio
                if (!is.null(data$allocation_ratio)) {
                    summary_html <- paste0(summary_html, "<p><strong>Allocation Ratio (T:C):</strong> ", data$allocation_ratio, ":1</p>")
                }

                # Notes
                if (!is.null(data$note)) {
                    summary_html <- paste0(summary_html, "<p style='color: #f57c00; font-style: italic;'><strong>Note:</strong> ", data$note, "</p>")
                }

                summary_html <- paste0(summary_html, "</div>")

                self$results$power_results$setContent(summary_html)
            },
            .generate_formulas = function() {
                if (is.null(private$.results_data)) {
                    return()
                }

                data <- private$.results_data

                formulas_html <- "<h4>Mathematical Formulas</h4>"
                formulas_html <- paste0(formulas_html, "<div style='background-color: rgba(255, 232, 33, 0.27); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>")

                if (data$method == "Lachin-Foulkes") {
                    formulas_html <- paste0(formulas_html, "<h5>Lachin-Foulkes Method</h5>")
                    formulas_html <- paste0(formulas_html, "<p><strong>Sample Size Formula:</strong></p>")
                    formulas_html <- paste0(formulas_html, "<p>Based on asymptotic normal approximation for log-rank test</p>")
                    formulas_html <- paste0(formulas_html, "<p>Accounts for: accrual period, follow-up period, dropout rate, entry pattern</p>")
                    formulas_html <- paste0(formulas_html, "<p><em>Reference:</em> Lachin JM, Foulkes MA (1986). Biometrics 42:507-519</p>")
                } else if (data$method == "Schoenfeld") {
                    formulas_html <- paste0(formulas_html, "<h5>Schoenfeld Method</h5>")
                    formulas_html <- paste0(formulas_html, "<p><strong>Number of Events Formula:</strong></p>")
                    formulas_html <- paste0(formulas_html, "<p>n = [(z<sub>\u{03B1}</sub> + z<sub>\u{03B2}</sub>)<sup>2</sup> \u{00D7} (1+r)<sup>2</sup>] / [r \u{00D7} (log HR)<sup>2</sup>]</p>")
                    formulas_html <- paste0(formulas_html, "<p>Where:</p>")
                    formulas_html <- paste0(formulas_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
                    formulas_html <- paste0(formulas_html, "<li>n = number of events required</li>")
                    formulas_html <- paste0(formulas_html, "<li>z<sub>\u{03B1}</sub> = critical value for Type I error</li>")
                    formulas_html <- paste0(formulas_html, "<li>z<sub>\u{03B2}</sub> = critical value for Type II error</li>")
                    formulas_html <- paste0(formulas_html, "<li>r = allocation ratio</li>")
                    formulas_html <- paste0(formulas_html, "<li>HR = hazard ratio</li>")
                    formulas_html <- paste0(formulas_html, "</ul>")
                    formulas_html <- paste0(formulas_html, "<p><em>Reference:</em> Schoenfeld D (1981). Biometrika 68:316-319</p>")
                }

                formulas_html <- paste0(formulas_html, "</div>")

                self$results$formulas$setContent(formulas_html)
            },
            .generate_interpretation = function() {
                if (is.null(private$.results_data)) {
                    return()
                }

                data <- private$.results_data

                interp_html <- "<h4>Clinical Interpretation</h4>"
                interp_html <- paste0(interp_html, "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>")

                # Interpretation based on calculation type
                if (grepl("Sample Size", data$calculation)) {
                    interp_html <- paste0(interp_html, "<h5>Sample Size Recommendations:</h5>")
                    if (!is.null(data$sample_size)) {
                        interp_html <- paste0(interp_html, "<p>\u{2022} <strong>Total enrollment:</strong> ", round(data$sample_size), " patients")
                        if (!is.null(data$allocation_ratio) && data$allocation_ratio != 1) {
                            control_n <- round(data$sample_size / (1 + data$allocation_ratio))
                            treatment_n <- round(data$sample_size - control_n)
                            interp_html <- paste0(interp_html, " (", treatment_n, " treatment, ", control_n, " control)")
                        }
                        interp_html <- paste0(interp_html, "</p>")
                    }
                    if (!is.null(data$events)) {
                        interp_html <- paste0(interp_html, "<p>\u{2022} <strong>Events required:</strong> ", ceiling(data$events), " events for analysis</p>")
                    }
                    interp_html <- paste0(interp_html, "<p>\u{2022} <strong>Statistical power:</strong> ", round(data$power * 100, 1), "% chance of detecting the specified effect size</p>")
                } else if (grepl("Power", data$calculation)) {
                    interp_html <- paste0(interp_html, "<h5>Power Analysis Results:</h5>")
                    power_pct <- round(data$power * 100, 1)
                    if (power_pct >= 80) {
                        interp_html <- paste0(interp_html, "<p style='color: green;'>\u{2022} <strong>Adequate power:</strong> ", power_pct, "% (>=80% recommended)</p>")
                    } else {
                        interp_html <- paste0(interp_html, "<p style='color: red;'>\u{2022} <strong>Insufficient power:</strong> ", power_pct, "% (<80%, consider increasing sample size)</p>")
                    }
                } else if (grepl("Events", data$calculation)) {
                    interp_html <- paste0(interp_html, "<h5>Event Requirements:</h5>")
                    interp_html <- paste0(interp_html, "<p>\u{2022} <strong>Target events:</strong> ", round(data$events), " events needed for ", round(data$power * 100, 1), "% power</p>")
                    interp_html <- paste0(interp_html, "<p>\u{2022} <strong>Effect size:</strong> Hazard ratio of ", round(data$hazard_ratio, 3), "</p>")
                }

                # General recommendations
                interp_html <- paste0(interp_html, "<h5>Study Design Considerations:</h5>")
                interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
                interp_html <- paste0(
                    interp_html, "<li><strong>Effect size:</strong> HR = ", round(data$hazard_ratio, 3),
                    " represents a ", round((1 - data$hazard_ratio) * 100, 1), "% reduction in hazard</li>"
                )
                interp_html <- paste0(interp_html, "<li><strong>Type I error:</strong> ", data$alpha, " (", round(data$alpha * 100, 1), "% false positive rate)</li>")
                interp_html <- paste0(interp_html, "<li><strong>Type II error:</strong> ", round(data$beta, 3), " (", round(data$beta * 100, 1), "% false negative rate)</li>")

                if (data$method == "Lachin-Foulkes") {
                    interp_html <- paste0(
                        interp_html, "<li><strong>Study timeline:</strong> ", data$accrual_duration, " time units for enrollment + ",
                        (data$study_duration - data$accrual_duration), " additional follow-up</li>"
                    )
                }

                interp_html <- paste0(interp_html, "</ul>")

                interp_html <- paste0(interp_html, "<h5>Recommendations:</h5>")
                interp_html <- paste0(interp_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
                interp_html <- paste0(interp_html, "<li>Consider adding 10-20% additional patients for dropouts and lost to follow-up</li>")
                interp_html <- paste0(interp_html, "<li>Verify assumptions about hazard rates with pilot data or literature</li>")
                interp_html <- paste0(interp_html, "<li>Consider interim analyses for early stopping if appropriate</li>")
                interp_html <- paste0(interp_html, "<li>Consult with biostatistician for complex trial designs</li>")
                interp_html <- paste0(interp_html, "</ul>")

                interp_html <- paste0(interp_html, "</div>")

                self$results$interpretation$setContent(interp_html)
            },
            .plot_power_curve = function(image, ggtheme, theme, ...) {
                # TODO (forward-looking): ggplot2 (and gsDesign at line ~99) should
                # be declared in DESCRIPTION under `Imports:` (or `Depends:` for
                # core dependencies) so jamovi's package manager pulls them in at
                # module install time. The runtime requireNamespace() + reject()
                # fallback here and at line ~552 is fine as a guard but module-level
                # declaration is the canonical jamovi-module convention. Same
                # concern noted for biomarkerdiscovery and causalmediation.
                if (!requireNamespace("ggplot2", quietly = TRUE)) {
                    jmvcore::reject("Package 'ggplot2' is required for power curve plots.")
                }

                if (is.null(private$.results_data)) {
                    return()
                }

                # Generate power curve data
                power_data <- private$.generate_power_curve_data()

                if (is.null(power_data) || nrow(power_data) == 0) {
                    return()
                }
                caption <- "Dashed lines: 80% (orange) and 90% (green) power thresholds"
                if (!is.null(attr(power_data, "note"))) {
                    caption <- paste(c(caption, strwrap(attr(power_data, "note"), 100)), collapse = "\n")
                }

                # Create power curve plot
                p <- ggplot2::ggplot(power_data, ggplot2::aes(x = amount, y = power)) +
                    ggplot2::geom_line(color = "#1f77b4", linewidth = 1.2) +
                    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed", color = "#ff7f0e", alpha = 0.7) +
                    ggplot2::geom_hline(yintercept = 0.9, linetype = "dashed", color = "#2ca02c", alpha = 0.7) +
                    ggplot2::labs(
                        title = paste("Power Curve -", private$.results_data$method, "Method"),
                        subtitle = paste("Hazard Ratio:", round(private$.results_data$hazard_ratio, 3)),
                        x = if (private$.results_data$method == "Schoenfeld") "Number of Events" else "Sample Size",
                        y = "Statistical Power",
                        caption = caption
                    ) +
                    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
                        panel.grid.minor = ggplot2::element_blank()
                    )

                # Add current point if applicable
                if (private$.results_data$calculation %in% c("Sample Size", "Power", "Number of Events", "Sample Size (Events-Based)")) {
                    amount <- if (private$.results_data$method == "Schoenfeld") private$.results_data$events else private$.results_data$sample_size
                    # Power reached at the displayed (rounded-up) count, so the point sits on the curve.
                    current_point <- data.frame(amount = amount, power = private$.power_at(amount))
                    if (length(current_point$amount) == 1L && is.finite(current_point$amount)) {
                        p <- p + ggplot2::geom_point(
                            data = current_point,
                            color = "#d62728",
                            size = 4,
                            shape = 19
                        ) +
                            ggplot2::annotate(
                                "text",
                                x = current_point$amount,
                                y = current_point$power + 0.05,
                                label = paste(
                                    "Current:", round(current_point$amount),
                                    if (private$.results_data$method == "Schoenfeld") "events," else "subjects,",
                                    scales::percent(current_point$power, accuracy = 0.1), "power"
                                ),
                                hjust = 0.5,
                                color = "#d62728",
                                fontface = "bold"
                            )
                    }
                }

                print(p)
                TRUE
            },
            .plot_timeline = function(image, ggtheme, theme, ...) {
                if (!requireNamespace("ggplot2", quietly = TRUE)) {
                    jmvcore::reject("Package 'ggplot2' is required for timeline plots.")
                }

                if (is.null(private$.results_data) || private$.results_data$method != "Lachin-Foulkes") {
                    return()
                }

                # Generate timeline data
                timeline_data <- private$.generate_timeline_data()

                if (is.null(timeline_data)) {
                    return()
                }

                # Create timeline plot
                # TODO (correctness): as.numeric(phase) below relies on factor level
                # indices (1, 2, 3) for ggplot Y-position. timeline_data is built
                # internally so phase is always a plain factor without jamovi
                # `values` attribute - current code is correct. If timeline_data
                # is ever sourced from a jamovi-loaded column, jmvcore::toNumeric()
                # would return label values instead of indices, breaking
                # positioning. Same factor-coding ambiguity flagged at
                # R/betabinomialdiagnostic.b.R:200 and R/biomarkerdiscovery.b.R:391.
                p <- ggplot2::ggplot(timeline_data, ggplot2::aes(x = time, y = phase, fill = phase)) +
                    ggplot2::geom_rect(
                        ggplot2::aes(xmin = start, xmax = end, ymin = as.numeric(phase) - 0.4, ymax = as.numeric(phase) + 0.4),
                        alpha = 0.7
                    ) +
                    ggplot2::scale_fill_manual(values = c("Accrual" = "#1f77b4", "Follow-up" = "#ff7f0e", "Analysis" = "#2ca02c")) +
                    ggplot2::labs(
                        title = "Study Timeline - Lachin-Foulkes Design",
                        subtitle = paste("Total Duration:", private$.results_data$study_duration, "time units"),
                        x = "Time (months/years)",
                        y = "Study Phase"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
                        legend.position = "none",
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank()
                    ) +
                    ggplot2::scale_y_discrete(limits = c("Analysis", "Follow-up", "Accrual"))

                # Add annotations
                p <- p + ggplot2::annotate(
                    "text",
                    x = private$.results_data$accrual_duration / 2,
                    y = 3.2,
                    label = paste("Accrual Period\n", private$.results_data$accrual_duration, "time units"),
                    hjust = 0.5,
                    fontface = "bold",
                    color = "#1f77b4"
                )

                if (private$.results_data$study_duration > private$.results_data$accrual_duration) {
                    p <- p + ggplot2::annotate(
                        "text",
                        x = (private$.results_data$accrual_duration + private$.results_data$study_duration) / 2,
                        y = 2.2,
                        label = paste(
                            "Follow-up Period\n",
                            private$.results_data$study_duration - private$.results_data$accrual_duration,
                            "time units"
                        ),
                        hjust = 0.5,
                        fontface = "bold",
                        color = "#ff7f0e"
                    )
                }

                print(p)
                TRUE
            },
            .generate_power_curve_data = function() {
                if (is.null(private$.results_data)) {
                    return(NULL)
                }

                # The range denotes subjects for Lachin-Foulkes and events for Schoenfeld.
                # It always runs low to high, starts above 0 and contains the current
                # design point; a note says when the user's range had to change.
                current <- if (private$.results_data$method == "Schoenfeld") private$.results_data$events else private$.results_data$sample_size
                bounds <- current * c(0.3, 2)
                note <- NULL
                range_str <- trimws(self$options$power_plot_range)
                if (!range_str %in% c("", "auto")) {
                    given <- suppressWarnings(as.numeric(strsplit(range_str, ",", fixed = TRUE)[[1]]))
                    if (length(given) != 2L || any(!is.finite(given))) {
                        note <- .("The power plot range must be two numbers separated by a comma, such as 50,500; the automatic range is shown instead.")
                    } else {
                        adjusted <- range(c(given, current))
                        if (adjusted[1] <= 0) adjusted[1] <- min(1, current)
                        if (adjusted[2] > adjusted[1]) bounds <- adjusted
                        if (!identical(bounds, given)) {
                            note <- .fmt(
                                .("The power plot range was changed to {low} to {high}: it must run from low to high, start above 0 and include the current design point."),
                                low = format(bounds[1], scientific = FALSE), high = format(bounds[2], scientific = FALSE)
                            )
                        }
                    }
                }

                amounts <- seq(bounds[1], bounds[2], length.out = 50)
                # Put the current design point on the grid so the curve passes through it.
                amounts[which.min(abs(amounts - current))] <- current
                curve <- data.frame(amount = amounts, power = private$.power_at(amounts))
                attr(curve, "note") <- note
                curve
            },
            .generate_timeline_data = function() {
                if (is.null(private$.results_data) || private$.results_data$method != "Lachin-Foulkes") {
                    return(NULL)
                }

                data.frame(
                    phase = factor(c("Accrual", "Follow-up", "Analysis"),
                        levels = c("Analysis", "Follow-up", "Accrual")
                    ),
                    start = c(0, private$.results_data$accrual_duration, private$.results_data$study_duration),
                    end = c(
                        private$.results_data$accrual_duration,
                        private$.results_data$study_duration,
                        private$.results_data$study_duration + 1
                    ),
                    time = c(
                        private$.results_data$accrual_duration / 2,
                        (private$.results_data$accrual_duration + private$.results_data$study_duration) / 2,
                        private$.results_data$study_duration + 0.5
                    )
                )
            },
            .generate_export_summary = function() {
                # On-screen reporting summary. This analysis has no dataset and adds
                # no variables to it; nothing is written back to the data.
                summary_html <- "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>"
                summary_html <- paste0(summary_html, "<h4>Reporting Summary</h4>")

                if (self$options$export_results) {
                    if (!is.null(private$.results_data)) {
                        summary_html <- paste0(summary_html, "<p><strong>Power analysis result:</strong> computed</p>")
                        # 'Power Analysis Results' is only populated when show_summary is on.
                        detail <- if (isTRUE(self$options$show_summary)) {
                            "Covers sample size, power, hazard ratios and study parameters; see 'Power Analysis Results' above."
                        } else {
                            "Covers sample size, power, hazard ratios and study parameters; tick 'Show Study Design Summary' to display them."
                        }
                        summary_html <- paste0(summary_html, "<p style='margin-left: 20px; opacity: 0.75;'>", detail, "</p>")
                    } else {
                        summary_html <- paste0(summary_html, "<p><strong>Power analysis result:</strong> not available</p>")
                    }
                }

                if (self$options$export_power_curve) {
                    power_data <- private$.generate_power_curve_data()
                    curve_count <- if (!is.null(power_data)) nrow(power_data) else 0
                    summary_html <- paste0(summary_html, "<p><strong>Power curve:</strong> ", curve_count, " points computed</p>")
                    if (curve_count == 0) {
                        detail <- .("No power curve could be computed for these settings.")
                    } else {
                        # The 'Power Curve Analysis' pane is only shown when show_power_plot is on.
                        axis <- if (private$.results_data$method == "Schoenfeld") "Number of events" else "Sample size"
                        detail <- if (isTRUE(self$options$show_power_plot)) {
                            paste0(axis, " versus power, plotted in 'Power Curve Analysis' above.")
                        } else {
                            paste0(axis, " versus power; tick 'Show Power Curve Plot' to display the curve.")
                        }
                        detail <- paste(c(detail, attr(power_data, "note")), collapse = " ")
                    }
                    summary_html <- paste0(summary_html, "<p style='margin-left: 20px; opacity: 0.75;'>", detail, "</p>")
                }

                summary_html <- paste0(summary_html, "<h5>Using these results:</h5>")
                summary_html <- paste0(summary_html, "<ul style='margin: 5px 0; padding-left: 20px;'>")
                summary_html <- paste0(summary_html, "<li>These are on-screen results only; this analysis does not add variables to your dataset.</li>")
                summary_html <- paste0(summary_html, "<li>To save these results, open jamovi's main menu and choose Export, then pick PDF or HTML as the format.</li>")
                summary_html <- paste0(summary_html, "<li>Copy the reported figures into protocol, grant or report documents.</li>")
                summary_html <- paste0(summary_html, "</ul>")

                summary_html <- paste0(summary_html, "</div>")

                self$results$export_summary$setContent(summary_html)
            }
        )
    )
}
