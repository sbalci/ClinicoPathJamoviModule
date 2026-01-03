#' @title Interrater Reliability Analysis
#' @return Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @importFrom irr kappa2 kappam.fleiss agree
#' @importFrom dplyr select group_by count
#' @importFrom htmlTable htmlTable
#' @importFrom glue glue
#'
#' @description This function calculates interrater reliability for ordinal or categorical data.
#'
#' @details The function calculates Cohen's kappa for two raters and Fleiss' kappa for three or more raters.
#'
#'


# See
# \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}


agreementClass <- if (requireNamespace("jmvcore")) R6::R6Class("agreementClass",
    inherit = agreementBase, private = list(

        .init = function() {
            # Initialize plot render functions
            if (self$options$blandAltmanPlot) {
                self$results$blandAltman$setRenderFun(private$.blandAltman)
            }
        },

        # Helper function to escape variable names for R code generation
        # Variable names with spaces or special characters are wrapped in backticks
        .escapeVariableName = function(varName) {
            if (is.null(varName) || length(varName) == 0) {
                return(NULL)
            }

            # Check if variable name needs escaping
            # If make.names() would change it, it needs backticks
            if (!identical(make.names(varName), varName)) {
                return(paste0('`', varName, '`'))
            }

            return(varName)
        },

        .createSummary = function(result1, result2, wght, exct) {
            # Create plain-language summary of agreement results

            # Extract values with safety checks
            n_subjects <- result1[["subjects"]]
            n_raters <- result1[["raters"]]
            perc_agree <- round(result1[["value"]], 1)
            kappa_val <- round(result2[["value"]], 3)
            p_val <- result2[["p.value"]]
            method <- result2[["method"]]

            # Safety check for p_val
            if (is.null(p_val) || length(p_val) == 0 || is.na(p_val)) {
                p_val <- NA
            }

            # Interpret kappa (Landis & Koch, 1977)
            if (is.na(kappa_val) || kappa_val < 0) {
                interp <- "poor agreement (worse than chance)"
            } else if (kappa_val < 0.20) {
                interp <- "slight agreement"
            } else if (kappa_val < 0.40) {
                interp <- "fair agreement"
            } else if (kappa_val < 0.60) {
                interp <- "moderate agreement"
            } else if (kappa_val < 0.80) {
                interp <- "substantial agreement"
            } else {
                interp <- "almost perfect agreement"
            }

            # Statistical significance
            if (is.na(p_val)) {
                sig_text <- "p-value not available"
            } else if (p_val < 0.001) {
                sig_text <- "p < .001, highly statistically significant"
            } else if (p_val < 0.01) {
                sig_text <- sprintf("p = %.3f, statistically significant", p_val)
            } else if (p_val < 0.05) {
                sig_text <- sprintf("p = %.3f, statistically significant", p_val)
            } else {
                sig_text <- sprintf("p = %.3f, not statistically significant", p_val)
            }

            # Weight description
            if (wght == "equal") {
                weight_desc <- " with linear weights"
            } else if (wght == "squared") {
                weight_desc <- " with squared weights"
            } else {
                weight_desc <- ""
            }

            # Exact kappa note
            exact_note <- if (exct) " using exact calculation" else ""

            # Build summary with consistent styling
            html_output <- paste0("
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>
                <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>Agreement Analysis Summary</h3>
                <p style='margin: 0; font-size: 14px; color: #666;'>", n_subjects, " cases rated by ", n_raters, " raters</p>
                </div>

                <div style='font-size: 14px; color: #333;'>
                    <table style='width: 100%; border-collapse: collapse; margin-bottom: 15px;'>
                    <tr>
                        <td style='border: 1px solid #ccc; padding: 10px; background: #f9f9f9;'>
                        <strong>Raw Agreement</strong><br>
                        <span style='font-size: 18px;'>", perc_agree, "%</span>
                        </td>
                        <td style='border: 1px solid #ccc; padding: 10px; background: #f9f9f9;'>
                        <strong>Kappa (κ)</strong><br>
                        <span style='font-size: 18px;'>", kappa_val, "</span>
                        </td>
                    </tr>
                    </table>

                    <p style='margin: 10px 0;'><strong>Method:</strong> ", method, exact_note, weight_desc, "</p>
                    <p style='margin: 10px 0;'><strong>Statistical test:</strong> ", sig_text, "</p>
                    <p style='margin: 10px 0;'><strong>Interpretation:</strong> ", interp, "</p>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                        <p style='margin: 0 0 8px 0; font-weight: bold;'>Clinical Meaning</p>
                        <p style='margin: 0; font-size: 14px;'>",
                        if (kappa_val >= 0.60) {
                            "The raters show good consistency, suggesting reliable measurements for clinical decisions or research."
                        } else if (kappa_val >= 0.40) {
                            "The raters show moderate consistency. Consider additional training or clearer protocols."
                        } else {
                            "The raters show low consistency. Measurement reliability is questionable - review criteria and provide training."
                        }, "</p>
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                        <p style='margin: 0 0 8px 0; font-weight: bold;'>Key Assumptions</p>
                        <ul style='margin: 0; font-size: 13px; padding-left: 20px;'>
                            <li>All raters scored the same cases independently</li>
                            <li>Categories are mutually exclusive and consistent</li>",
                            if (wght != "unweighted") "
                            <li>Ordinal scale with meaningful distances between categories</li>" else "", "
                            <li>Kappa sensitive to category prevalence</li>
                        </ul>
                    </div>

                    <p style='font-size: 12px; color: #666; margin-top: 15px;'>
                    Reference: Landis & Koch (1977). Biometrics, 33, 159-174.</p>
                </div>
            </div>
            ")

            return(html_output)
        },

        .createAboutPanel = function() {
            # Create explanatory panel about the analysis

            html_output <- paste0("
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>
                <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>About This Analysis</h3>
                <p style='margin: 0; font-size: 14px; color: #666;'>Understanding interrater reliability methods and applications</p>
                </div>

                <div style='font-size: 14px; color: #333;'>
                    <p style='margin: 10px 0;'><strong>What this analysis does:</strong></p>
                    <ul style='margin: 5px 0 15px 20px;'>
                        <li>Measures consistency between multiple raters</li>
                        <li>Accounts for chance agreement</li>
                        <li>Provides statistical measures with significance tests</li>
                    </ul>

                    <p style='margin: 10px 0;'><strong>When to use:</strong></p>
                    <ul style='margin: 5px 0 15px 20px;'>
                        <li>Quality assurance (inter-pathologist agreement)</li>
                        <li>Method validation (rating scales, classifications)</li>
                        <li>Training assessment (trainee vs expert concordance)</li>
                        <li>Research (multi-rater study reliability)</li>
                    </ul>

                    <p style='margin: 10px 0;'><strong>Data requirements:</strong></p>
                    <ul style='margin: 5px 0 15px 20px;'>
                        <li>At least 2 rater variables (columns)</li>
                        <li>Same cases rated by all raters</li>
                        <li>Categorical or ordinal data</li>
                        <li>Matching category levels across raters</li>
                    </ul>

                    <table style='width: 100%; border-collapse: collapse; margin: 15px 0;'>
                    <tr>
                        <td style='border: 1px solid #ccc; padding: 10px; vertical-align: top;'>
                        <strong>Cohen's kappa</strong><br>
                        <span style='font-size: 13px; color: #666;'>For 2 raters. Supports weighted analysis for ordinal data.</span>
                        </td>
                    </tr>
                    <tr>
                        <td style='border: 1px solid #ccc; padding: 10px; vertical-align: top;'>
                        <strong>Fleiss' kappa</strong><br>
                        <span style='font-size: 13px; color: #666;'>For 3+ raters. Fixed marginal probabilities.</span>
                        </td>
                    </tr>
                    <tr>
                        <td style='border: 1px solid #ccc; padding: 10px; vertical-align: top;'>
                        <strong>Krippendorff's alpha</strong><br>
                        <span style='font-size: 13px; color: #666;'>Alternative measure. Handles missing data and various data types.</span>
                        </td>
                    </tr>
                    </table>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 10px; margin-top: 15px;'>
                        <p style='margin: 0; font-size: 13px;'><strong>Tip:</strong> For ordinal data (e.g., tumor grades), use weighted kappa to account for degree of disagreement.</p>
                    </div>
                </div>
            </div>
            ")

            return(html_output)
        },

        .createWeightedKappaGuide = function(weight_type) {
            # Create HTML guide for weighted kappa interpretation

            if (weight_type == "equal") {
                weight_name <- "Linear (Equal) Weights"
                weight_desc <- "Disagreements are weighted proportionally to their distance on the ordinal scale."
                formula <- "Weight = 1 - |i - j| / (k - 1)"
                formula_desc <- "where i and j are category positions, k is number of categories"

                example_table <- "
                <table style='margin: 15px auto; border-collapse: collapse; font-size: 12px;'>
                <tr style='background: #f0f0f0;'>
                    <th style='padding: 6px 12px; border: 1px solid #ddd;'>Distance</th>
                    <th style='padding: 6px 12px; border: 1px solid #ddd;'>Example</th>
                    <th style='padding: 6px 12px; border: 1px solid #ddd;'>Weight</th>
                    <th style='padding: 6px 12px; border: 1px solid #ddd;'>Penalty</th>
                </tr>
                <tr>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>0 (agreement)</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 vs 1</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>1.00</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>None</td>
                </tr>
                <tr>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 category</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 vs 2</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>0.75</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>Small</td>
                </tr>
                <tr>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>2 categories</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 vs 3</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>0.50</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>Moderate</td>
                </tr>
                <tr>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>Maximum</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 vs 5</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>0.00</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>Full</td>
                </tr>
                </table>"

                use_case <- "Use linear weights when each step on your ordinal scale represents equal incremental difference (e.g., Likert scales with equal intervals)."

                interpretation <- "
                <ul style='font-size: 13px; line-height: 1.8; margin: 10px 0;'>
                <li>Adjacent disagreements (e.g., 'Agree' vs 'Strongly Agree') receive <b>partial credit</b></li>
                <li>Distant disagreements (e.g., 'Strongly Disagree' vs 'Strongly Agree') receive <b>no credit</b></li>
                <li>Penalty increases <b>linearly</b> with distance</li>
                <li>Weighted kappa will be <b>higher</b> than unweighted kappa when most disagreements are adjacent</li>
                </ul>"

            } else if (weight_type == "squared") {
                weight_name <- "Squared (Quadratic) Weights"
                weight_desc <- "Disagreements are weighted by the squared distance, penalizing larger disagreements more heavily."
                formula <- "Weight = 1 - [(i - j) / (k - 1)]²"
                formula_desc <- "where i and j are category positions, k is number of categories"

                example_table <- "
                <table style='margin: 15px auto; border-collapse: collapse; font-size: 12px;'>
                <tr style='background: #f0f0f0;'>
                    <th style='padding: 6px 12px; border: 1px solid #ddd;'>Distance</th>
                    <th style='padding: 6px 12px; border: 1px solid #ddd;'>Example</th>
                    <th style='padding: 6px 12px; border: 1px solid #ddd;'>Weight</th>
                    <th style='padding: 6px 12px; border: 1px solid #ddd;'>Penalty</th>
                </tr>
                <tr>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>0 (agreement)</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 vs 1</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>1.00</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>None</td>
                </tr>
                <tr>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 category</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 vs 2</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>0.94</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>Very small</td>
                </tr>
                <tr>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>2 categories</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 vs 3</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>0.75</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>Moderate</td>
                </tr>
                <tr>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>Maximum</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd;'>1 vs 5</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>0.00</td>
                    <td style='padding: 6px 12px; border: 1px solid #ddd; text-align: center;'>Full</td>
                </tr>
                </table>"

                use_case <- "Use squared weights when larger disagreements are disproportionately more serious than small ones (e.g., clinical severity scales, diagnostic accuracy)."

                interpretation <- "
                <ul style='font-size: 13px; line-height: 1.8; margin: 10px 0;'>
                <li>Adjacent disagreements receive <b>much more credit</b> than linear (0.94 vs 0.75)</li>
                <li>Distant disagreements are <b>heavily penalized</b></li>
                <li>Penalty increases <b>exponentially</b> with distance</li>
                <li>Weighted kappa will be <b>substantially higher</b> than unweighted when disagreements cluster near the diagonal</li>
                <li><b>Special property:</b> Squared weights equal the intraclass correlation coefficient under certain conditions</li>
                </ul>"
            }

            html_output <- paste0("
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>
                <h3 style='margin: 0 0 5px 0; font-size: 16px; color: #333;'>", weight_name, "</h3>
                <p style='margin: 0; font-size: 14px; color: #666;'>", weight_desc, "</p>
                </div>

                <div style='font-size: 14px; color: #333;'>
                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                        <p style='margin: 0 0 8px 0; font-weight: bold;'>Formula</p>
                        <p style='font-family: monospace; font-size: 13px; margin: 5px 0; background: white; padding: 8px; border: 1px solid #ccc;'>",
                        formula, "</p>
                        <p style='font-size: 13px; margin: 5px 0; color: #666;'>", formula_desc, "</p>
                    </div>

                    <p style='margin: 15px 0; font-weight: bold;'>Example (5-point scale):</p>",
                    example_table, "

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                        <p style='margin: 0 0 8px 0; font-weight: bold;'>When to Use</p>
                        <p style='margin: 0; font-size: 14px;'>", use_case, "</p>
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                        <p style='margin: 0 0 8px 0; font-weight: bold;'>Key Points</p>",
                        interpretation, "
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                        <p style='margin: 0 0 8px 0; font-weight: bold;'>Important Note</p>
                        <p style='margin: 0; font-size: 14px;'>Weighted kappa values cannot be directly compared to unweighted kappa. They measure different aspects of agreement.</p>
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 12px; margin: 15px 0;'>
                        <p style='margin: 0 0 8px 0; font-weight: bold;'>Interpretation Guidelines (Landis & Koch, 1977)</p>
                        <table style='width: 100%; font-size: 13px;'>
                        <tr><td style='padding: 2px 0;'>&lt; 0.00</td><td style='padding: 2px 0;'>Poor agreement</td></tr>
                        <tr><td style='padding: 2px 0;'>0.00 - 0.20</td><td style='padding: 2px 0;'>Slight agreement</td></tr>
                        <tr><td style='padding: 2px 0;'>0.21 - 0.40</td><td style='padding: 2px 0;'>Fair agreement</td></tr>
                        <tr><td style='padding: 2px 0;'>0.41 - 0.60</td><td style='padding: 2px 0;'>Moderate agreement</td></tr>
                        <tr><td style='padding: 2px 0;'>0.61 - 0.80</td><td style='padding: 2px 0;'>Substantial agreement</td></tr>
                        <tr><td style='padding: 2px 0;'>0.81 - 1.00</td><td style='padding: 2px 0;'>Almost perfect agreement</td></tr>
                        </table>
                    </div>
                </div>
            </div>
            ")

            return(html_output)
        },

        .blandAltman = function(image, ...) {
            # Render Bland-Altman plot from stored state

            plotState <- image$state

            if (is.null(plotState)) {
                return(FALSE)
            }

            # Extract data from state
            diff <- plotState$diff
            avg <- plotState$avg
            mean_diff <- plotState$mean_diff
            lower_loa <- plotState$lower_loa
            upper_loa <- plotState$upper_loa
            conf_level <- plotState$conf_level
            prop_bias <- plotState$prop_bias
            rater_names <- plotState$rater_names

            # Create plot
            plot_data <- data.frame(
                avg = avg,
                diff = diff
            )

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = avg, y = diff)) +
                ggplot2::geom_point(alpha = 0.6, size = 2) +
                ggplot2::geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue", size = 1) +
                ggplot2::geom_hline(yintercept = lower_loa, linetype = "dashed", color = "red", size = 0.8) +
                ggplot2::geom_hline(yintercept = upper_loa, linetype = "dashed", color = "red", size = 0.8) +
                ggplot2::labs(
                    title = "Bland-Altman Plot",
                    subtitle = sprintf("Mean Difference and %g%% Limits of Agreement", conf_level * 100),
                    x = sprintf("Mean of %s and %s", rater_names[1], rater_names[2]),
                    y = sprintf("Difference (%s - %s)", rater_names[1], rater_names[2])
                ) +
                ggplot2::annotate("text", x = max(avg) * 0.95, y = mean_diff,
                                label = sprintf("Mean: %.2f", mean_diff),
                                hjust = 1, vjust = -0.5, color = "blue") +
                ggplot2::annotate("text", x = max(avg) * 0.95, y = lower_loa,
                                label = sprintf("Lower LoA: %.2f", lower_loa),
                                hjust = 1, vjust = 1.2, color = "red") +
                ggplot2::annotate("text", x = max(avg) * 0.95, y = upper_loa,
                                label = sprintf("Upper LoA: %.2f", upper_loa),
                                hjust = 1, vjust = -0.2, color = "red") +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5)
                )

            # Add proportional bias trend line if requested
            if (prop_bias) {
                p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, color = "darkgreen", size = 0.8)
            }

            print(p)

            return(TRUE)
        },

        .populateBlandAltman = function(ratings) {
            # Generate Bland-Altman plot for continuous agreement analysis
            # Only applicable for 2 raters with numeric/continuous data

            # Validate inputs
            if (ncol(ratings) != 2) {
                self$results$blandAltman$setVisible(FALSE)
                self$results$blandAltmanStats$setVisible(FALSE)
                self$results$blandAltmanStats$setNote(
                    "error",
                    "Bland-Altman analysis requires exactly 2 raters. Please select only 2 variables."
                )
                return()
            }

            # Check if data is numeric/continuous
            if (!is.numeric(ratings[[1]]) || !is.numeric(ratings[[2]])) {
                self$results$blandAltman$setVisible(FALSE)
                self$results$blandAltmanStats$setVisible(FALSE)
                self$results$blandAltmanStats$setNote(
                    "error",
                    "Bland-Altman analysis requires continuous (numeric) data. Your data appears to be categorical."
                )
                return()
            }

            # Calculate Bland-Altman statistics
            rater1 <- ratings[[1]]
            rater2 <- ratings[[2]]

            # Remove missing values pairwise
            complete_idx <- complete.cases(ratings)
            rater1 <- rater1[complete_idx]
            rater2 <- rater2[complete_idx]

            # Calculate difference and mean
            diff <- rater1 - rater2
            avg <- (rater1 + rater2) / 2

            # Statistics
            mean_diff <- mean(diff, na.rm = TRUE)
            sd_diff <- sd(diff, na.rm = TRUE)

            # Limits of Agreement (LoA)
            conf_level <- self$options$baConfidenceLevel
            z_value <- qnorm((1 + conf_level) / 2)
            lower_loa <- mean_diff - z_value * sd_diff
            upper_loa <- mean_diff + z_value * sd_diff

            # Test for proportional bias (if requested)
            prop_bias_p <- NA
            if (self$options$proportionalBias) {
                tryCatch({
                    lm_result <- lm(diff ~ avg)
                    prop_bias_p <- summary(lm_result)$coefficients[2, 4]  # p-value for slope
                }, error = function(e) {
                    prop_bias_p <- NA
                })
            }

            # Populate statistics table
            self$results$blandAltmanStats$setRow(rowNo = 1, values = list(
                meanDiff = mean_diff,
                sdDiff = sd_diff,
                lowerLoA = lower_loa,
                upperLoA = upper_loa,
                propBiasP = prop_bias_p
            ))

            # Generate plot
            plot <- self$results$blandAltman
            plot$setState(list(
                diff = diff,
                avg = avg,
                mean_diff = mean_diff,
                lower_loa = lower_loa,
                upper_loa = upper_loa,
                conf_level = conf_level,
                prop_bias = self$options$proportionalBias,
                rater_names = names(ratings)
            ))
        },

        .calculateAgreementStatus = function(ratings) {
            # Calculate agreement status for each case
            # Categorizes as: All Agreed, Majority Agreed, No Agreement

            threshold <- self$options$agreementThreshold
            n_raters <- ncol(ratings)
            n_cases <- nrow(ratings)

            # Initialize results storage
            agreement_results <- data.frame(
                case_id = 1:n_cases,
                agreement_status = character(n_cases),
                agreement_percent = numeric(n_cases),
                modal_category = character(n_cases),
                n_agreeing = integer(n_cases),
                stringsAsFactors = FALSE
            )

            # Calculate for each case
            for (i in 1:n_cases) {
                row_ratings <- unlist(ratings[i, ])
                row_ratings <- row_ratings[!is.na(row_ratings)]

                if (length(row_ratings) == 0) {
                    agreement_results$agreement_status[i] <- "Missing Data"
                    agreement_results$agreement_percent[i] <- NA
                    agreement_results$modal_category[i] <- NA
                    agreement_results$n_agreeing[i] <- 0
                    next
                }

                # Find mode (most common rating)
                freq_table <- table(row_ratings)
                max_freq <- max(freq_table)
                modal_value <- names(freq_table)[which.max(freq_table)]

                # Calculate agreement percentage
                agree_pct <- (max_freq / length(row_ratings)) * 100

                # Categorize agreement status
                if (max_freq == length(row_ratings)) {
                    status <- "All Agreed"
                } else if (agree_pct >= threshold) {
                    status <- "Majority Agreed"
                } else {
                    status <- "No Agreement"
                }

                # Store results
                agreement_results$agreement_status[i] <- status
                agreement_results$agreement_percent[i] <- agree_pct / 100  # Convert to proportion for format: pc
                agreement_results$modal_category[i] <- modal_value
                agreement_results$n_agreeing[i] <- max_freq
            }

            # Populate case-level detail table
            detail_table <- self$results$agreementStatusDetail
            for (i in 1:n_cases) {
                detail_table$addRow(rowKey = i, values = list(
                    case_id = agreement_results$case_id[i],
                    agreement_status = agreement_results$agreement_status[i],
                    agreement_percent = agreement_results$agreement_percent[i],
                    modal_category = agreement_results$modal_category[i],
                    n_agreeing = agreement_results$n_agreeing[i]
                ))
            }

            # Calculate distribution summary (if requested)
            if (self$options$showAgreementTable) {
                status_counts <- table(agreement_results$agreement_status)
                status_table <- self$results$agreementStatusTable

                status_order <- c("All Agreed", "Majority Agreed", "No Agreement", "Missing Data")
                for (status in status_order) {
                    if (status %in% names(status_counts)) {
                        count <- as.integer(status_counts[status])
                        pct <- count / n_cases

                        # Interpretation text
                        interpretation <- switch(status,
                            "All Agreed" = "Complete consensus across all raters",
                            "Majority Agreed" = sprintf("≥%.0f%% of raters agree", threshold),
                            "No Agreement" = sprintf("<%.0f%% agreement - review recommended", threshold),
                            "Missing Data" = "Insufficient data for classification"
                        )

                        status_table$addRow(rowKey = status, values = list(
                            status = status,
                            count = count,
                            percentage = pct,
                            interpretation = interpretation
                        ))
                    }
                }
            }

            # Add agreement status variable to dataset (if requested)
            if (self$options$addAgreementStatus) {
                self$results$addAgreementStatus$setRowNums(1:n_cases)
                self$results$addAgreementStatus$setValues(agreement_results$agreement_status)
            }

            # Update computed variables info
            if (self$options$consensusVar || self$options$addAgreementStatus) {
                private$.updateComputedVariablesInfo()
            }
        },

        .createConsensusVariable = function(ratings) {
            # Create consensus (modal) rating variable based on selected rule

            consensus_rule <- self$options$consensusRule
            tie_breaker <- self$options$tieBreaker
            n_cases <- nrow(ratings)
            n_raters <- ncol(ratings)

            # Initialize consensus vector
            consensus <- character(n_cases)

            # Calculate threshold based on rule
            threshold <- switch(consensus_rule,
                "majority" = 0.5,        # >50%
                "supermajority" = 0.75,  # ≥75%
                "unanimous" = 1.0        # 100%
            )

            # Calculate for each case
            for (i in 1:n_cases) {
                row_ratings <- unlist(ratings[i, ])
                row_ratings <- row_ratings[!is.na(row_ratings)]

                if (length(row_ratings) == 0) {
                    consensus[i] <- NA
                    next
                }

                # Find mode and frequency
                freq_table <- table(row_ratings)
                max_freq <- max(freq_table)
                modes <- names(freq_table)[freq_table == max_freq]

                # Calculate agreement proportion
                agree_prop <- max_freq / length(row_ratings)

                # Check if consensus meets threshold
                if (agree_prop >= threshold) {
                    if (length(modes) == 1) {
                        # Single mode - use it
                        consensus[i] <- modes[1]
                    } else {
                        # Tie - apply tie breaker
                        consensus[i] <- switch(tie_breaker,
                            "exclude" = NA,
                            "first" = modes[1],
                            "lowest" = {
                                # Convert to numeric if possible for comparison
                                numeric_modes <- suppressWarnings(as.numeric(modes))
                                if (all(!is.na(numeric_modes))) {
                                    modes[which.min(numeric_modes)]
                                } else {
                                    # For character data, use alphabetical order
                                    modes[order(modes)[1]]
                                }
                            },
                            "highest" = {
                                # Convert to numeric if possible for comparison
                                numeric_modes <- suppressWarnings(as.numeric(modes))
                                if (all(!is.na(numeric_modes))) {
                                    modes[which.max(numeric_modes)]
                                } else {
                                    # For character data, use alphabetical order
                                    modes[order(modes, decreasing = TRUE)[1]]
                                }
                            },
                            NA  # Default: exclude
                        )
                    }
                } else {
                    # Does not meet threshold
                    consensus[i] <- NA
                }
            }

            # Populate consensus summary table
            consensus_no_na <- consensus[!is.na(consensus)]
            if (length(consensus_no_na) > 0) {
                freq_table <- table(consensus_no_na)
                consensus_table <- self$results$consensusTable

                # Calculate average agreement for each consensus category
                for (category in names(freq_table)) {
                    # Find cases with this consensus category
                    matching_cases <- which(consensus == category)

                    # Calculate average agreement percentage for these cases
                    avg_agreement <- mean(sapply(matching_cases, function(case_idx) {
                        row_ratings <- unlist(ratings[case_idx, ])
                        row_ratings <- row_ratings[!is.na(row_ratings)]
                        if (length(row_ratings) == 0) return(NA)
                        max_freq <- max(table(row_ratings))
                        return(max_freq / length(row_ratings))
                    }), na.rm = TRUE)

                    consensus_table$addRow(rowKey = category, values = list(
                        category = category,
                        consensus_count = as.integer(freq_table[category]),
                        percentage = as.integer(freq_table[category]) / n_cases,
                        avg_agreement = avg_agreement
                    ))
                }
            }

            # Add consensus variable to dataset (if requested)
            if (self$options$consensusVar) {
                self$results$consensusVar$setRowNums(1:n_cases)
                self$results$consensusVar$setValues(consensus)
            }

            # Update computed variables info
            if (self$options$consensusVar || self$options$addAgreementStatus) {
                private$.updateComputedVariablesInfo()
            }
        },

        .updateComputedVariablesInfo = function() {
            # Generate HTML info about computed variables added to dataset

            info_html <- "<div style='font-family: Arial, sans-serif; padding: 10px;'>"
            info_html <- paste0(info_html, "<h4 style='margin-top: 0;'>Computed Variables Added:</h4><ul>")

            if (self$options$consensusVar) {
                var_name <- self$options$consensusName
                rule_text <- switch(self$options$consensusRule,
                    "majority" = "Simple Majority (>50%)",
                    "supermajority" = "Supermajority (≥75%)",
                    "unanimous" = "Unanimous (100%)"
                )
                info_html <- paste0(info_html,
                    "<li><strong>", var_name, "</strong>: Consensus rating (", rule_text, ")</li>")
            }

            if (self$options$addAgreementStatus) {
                var_name <- self$options$agreementStatusName
                threshold <- self$options$agreementThreshold
                info_html <- paste0(info_html,
                    "<li><strong>", var_name, "</strong>: Agreement status (threshold: ", threshold, "%)</li>")
            }

            info_html <- paste0(info_html, "</ul>")
            info_html <- paste0(info_html,
                "<p style='color: #666; font-size: 12px;'>",
                "These variables are now available in your dataset for further analysis.",
                "</p></div>")

            self$results$computedVariablesInfo$setContent(info_html)
        },

        .run = function() {

        # Validate input ----
        if (is.null(self$options$vars) || length(self$options$vars) < 2) {
            self$results$welcome$setVisible(TRUE)
            self$results$welcome$setContent(
                "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 20px; margin-bottom: 20px;'>
                <h2 style='margin: 0 0 10px 0; font-size: 18px; color: #333;'>Interrater Reliability Analysis</h2>
                <p style='margin: 0; font-size: 14px; color: #666;'>Measure agreement between multiple raters scoring the same cases</p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                <h3 style='margin: 0 0 10px 0; color: #333; font-size: 16px;'>Setup Progress</h3>
                <div style='margin-bottom: 10px; font-size: 14px;'>
                [ ] Raters: 0/2 minimum - Select at least 2 rater variables to begin
                </div>
                </div>

                <table style='width: 100%; border-collapse: collapse; margin-bottom: 20px;'>
                <tr>
                <td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>
                <h4 style='margin: 0 0 10px 0; font-size: 15px;'>Quick Start</h4>
                <ol style='margin: 0; padding-left: 20px; font-size: 14px;'>
                <li>Select 2+ rater variables</li>
                <li>For ordinal data, choose weighted kappa</li>
                <li>Enable optional outputs as needed</li>
                </ol>
                </td>
                <td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>
                <h4 style='margin: 0 0 10px 0; font-size: 15px;'>Available Methods</h4>
                <ul style='margin: 0; padding-left: 20px; font-size: 14px;'>
                <li><strong>Cohen's kappa:</strong> 2 raters</li>
                <li><strong>Fleiss' kappa:</strong> 3+ raters</li>
                <li><strong>Krippendorff's alpha:</strong> Alternative measure</li>
                </ul>
                </td>
                </tr>
                </table>
                </div>"
            )
            return()
        } else {
            self$results$welcome$setVisible(FALSE)
            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            # Data preparation ----
            exct <- self$options$exct
            wght <- self$options$wght
            mydata <- self$data

            # Safe variable selection - ensure proper data frame structure
            # Note: Variable names with spaces (e.g., "Rater 1") are handled correctly
            # by R's bracket notation. No special escaping needed for data access.
            ratings <- mydata[, self$options$vars, drop = FALSE]

            # Ensure it's a proper data frame
            if (!is.data.frame(ratings)) {
                ratings <- as.data.frame(ratings)
            }

            # Preserve jamovi attributes if they exist
            if (!is.null(attr(mydata, "jmv-desc"))) {
                # Only copy attributes for selected columns
                desc_attr <- attr(mydata, "jmv-desc")
                if (length(desc_attr) > 0) {
                    selected_desc <- desc_attr[names(desc_attr) %in% self$options$vars]
                    if (length(selected_desc) > 0) {
                        attr(ratings, "jmv-desc") <- selected_desc
                    }
                }
            }

            # Check for missing values and notify user ----
            if (any(is.na(ratings))) {
                n_total <- nrow(ratings)
                n_complete <- sum(complete.cases(ratings))
                n_missing <- n_total - n_complete
                pct_missing <- round(100 * n_missing / n_total, 1)

                self$results$irrtable$setNote(
                    "missing",
                    sprintf("Note: %d of %d cases excluded due to missing values (%.1f%%). Analysis based on %d complete cases.",
                            n_missing, n_total, pct_missing, n_complete)
                )
            }

            # 2 raters: Cohen's kappa ----
            if (length(self$options$vars) == 2) {
                xorder <- unlist(lapply(ratings, is.ordered))

                if (wght %in% c("equal", "squared") && !all(xorder == TRUE)) {
                    stop("Weighted kappa requires ordinal variables. Please select ordinal data or choose 'Unweighted'.")
                }

                if (exct == TRUE) {
                    stop("Exact kappa requires at least 3 raters. Please add more rater variables or disable 'Exact Kappa'.")
                }


                # irr::kappa2 ----
                result2 <- irr::kappa2(ratings = ratings, weight = wght)

            # >=3 raters: Fleiss kappa ----
            } else if (length(self$options$vars) >= 3) {
                result2 <- irr::kappam.fleiss(ratings = ratings, exact = exct, detail = TRUE)
            }


            # Percentage agreement ----
            result1 <- irr::agree(ratings)
            if (result1[["value"]] > 100) {
                result1[["value"]] <- "Please check the data. It seems that observers do not agree on any cases"
            }

            # Populate main results table ----
            table2 <- self$results$irrtable

            # Note: Exact kappa (Conger's) does not provide z-statistic or p-value
            # Only Fleiss' formulation allows testing H0: Kappa=0
            z_stat <- if (!is.null(result2[["statistic"]])) result2[["statistic"]] else NA
            p_val <- if (!is.null(result2[["p.value"]])) result2[["p.value"]] else NA

            table2$setRow(rowNo = 1, values = list(
                method = result2[["method"]],
                subjects = result1[["subjects"]],
                raters = result1[["raters"]],
                peragree = result1[["value"]],
                kappa = result2[["value"]],
                z = z_stat,
                p = p_val
            ))

            # Add note if exact kappa was used (no statistical test)
            if (exct && length(self$options$vars) >= 3) {
                table2$setNote(
                    "exact_note",
                    "Note: Exact Kappa (Conger, 1980) does not provide statistical test. Use Fleiss' Kappa for hypothesis testing (H0: Kappa=0)."
                )
            }

            # Frequency tables (if requested) ----
            # Control visibility based on number of raters and sft option
            num_raters <- length(self$options$vars)

            if (self$options$sft) {
                # Show contingency table only for 2 raters
                self$results$contingencyTable$setVisible(num_raters == 2)

                # Show rating combinations table only for 3+ raters
                self$results$ratingCombinationsTable$setVisible(num_raters >= 3)

                # For 2 raters, create a 2x2 contingency table
                if (num_raters == 2) {
                    # Create contingency table
                    cont_table <- table(ratings[[1]], ratings[[2]])

                    # Get row and column names
                    row_names <- rownames(cont_table)
                    col_names <- colnames(cont_table)

                    # Create jamovi table with dynamic columns
                    contTable <- self$results$contingencyTable

                    # Add row name column
                    # Note: names(ratings)[1] preserves original variable name with spaces
                    contTable$addColumn(
                        name = 'rater1',
                        title = names(ratings)[1],  # Original name (e.g., "Rater 1")
                        type = 'text'
                    )

                    # Add columns for each category of rater 2
                    # Note: use make.names() for safe column IDs, display original labels
                    for (col_name in col_names) {
                        col_id <- make.names(col_name)
                        contTable$addColumn(
                            name = col_id,
                            title = as.character(col_name),  # Original category label
                            type = 'integer'
                        )
                    }

                    # Add row total column
                    contTable$addColumn(
                        name = 'row_total',
                        title = 'Total',
                        type = 'integer'
                    )

                    # Populate data rows
                    for (i in seq_along(row_names)) {
                        row_data <- list(rater1 = as.character(row_names[i]))

                        for (j in seq_along(col_names)) {
                            col_id <- make.names(col_names[j])
                            row_data[[col_id]] <- as.integer(cont_table[i, j])
                        }

                        row_data$row_total <- sum(cont_table[i, ])
                        contTable$addRow(rowKey = i, values = row_data)
                    }

                    # Add total row
                    total_row <- list(rater1 = 'Total')
                    for (j in seq_along(col_names)) {
                        col_id <- make.names(col_names[j])
                        total_row[[col_id]] <- sum(cont_table[, j])
                    }
                    total_row$row_total <- sum(cont_table)
                    contTable$addRow(rowKey = 'total', values = total_row)

                    # Add note about percentages
                    contTable$setNote(
                        'interpretation',
                        sprintf('N = %d cases. Cell counts show frequency of agreement/disagreement patterns.',
                                sum(cont_table))
                    )

                } else {
                    # For 3+ raters, show combination counts
                    freq_table <- ratings %>%
                        dplyr::group_by_all() %>%
                        dplyr::count() %>%
                        as.data.frame()

                    # Create jamovi table with dynamic columns
                    combTable <- self$results$ratingCombinationsTable

                    # Add columns for each rater
                    # Note: use make.names() for jamovi column IDs, but keep original
                    # variable names (with spaces) as titles for display
                    for (var_name in self$options$vars) {
                        col_id <- make.names(var_name)
                        combTable$addColumn(
                            name = col_id,
                            title = var_name,  # Original name with spaces preserved
                            type = 'text'
                        )
                    }

                    # Add count column
                    combTable$addColumn(
                        name = 'count',
                        title = 'Count',
                        type = 'integer'
                    )

                    # Populate rows (limit to 100 most frequent combinations)
                    max_display <- min(100, nrow(freq_table))

                    for (i in 1:max_display) {
                        row_data <- list()

                        # Add rater values
                        # Note: freq_table column names preserve spaces from original variables
                        for (var_name in self$options$vars) {
                            col_id <- make.names(var_name)
                            value <- freq_table[i, var_name, drop = TRUE]  # Explicit drop for clarity
                            row_data[[col_id]] <- as.character(value)
                        }

                        # Add count
                        row_data$count <- as.integer(freq_table$n[i])

                        combTable$addRow(rowKey = i, values = row_data)
                    }

                    # Add note
                    if (nrow(freq_table) > max_display) {
                        combTable$setNote(
                            'truncated',
                            sprintf('Showing %d of %d unique rating combinations. Total: %d ratings.',
                                    max_display, nrow(freq_table), sum(freq_table$n))
                        )
                    } else {
                        combTable$setNote(
                            'complete',
                            sprintf('%d unique rating combinations. Total: %d ratings.',
                                    nrow(freq_table), sum(freq_table$n))
                        )
                    }
                }
            } else {
                # Hide both frequency tables when sft is disabled
                self$results$contingencyTable$setVisible(FALSE)
                self$results$ratingCombinationsTable$setVisible(FALSE)
            }

            # Weighted Kappa Guide (if using weights) ----
            if (wght != "unweighted") {
                weight_guide <- private$.createWeightedKappaGuide(wght)
                self$results$weightedKappaGuide$setContent(weight_guide)
            }

            # Natural-language summary (if requested) ----
            if (self$options$showSummary) {
                summary_text <- private$.createSummary(result1, result2, wght, exct)
                self$results$summary$setContent(summary_text)
            }

            # About panel (if requested) ----
            if (self$options$showAbout) {
                about_text <- private$.createAboutPanel()
                self$results$about$setContent(about_text)
            }
        }



        # Krippendorff's Alpha (if requested) ----
        if (self$options$kripp) {
            # Convert ratings data frame to matrix
            ratings_matrix <- as.matrix(ratings)

            # Ensure numeric conversion if needed
            if (!is.numeric(ratings_matrix)) {
                # If categorical/factor data, convert to numeric codes
                ratings_matrix <- matrix(
                    as.numeric(factor(ratings_matrix)),
                    nrow = nrow(ratings_matrix),
                    ncol = ncol(ratings_matrix)
                )
            }

            # Add error handling
            tryCatch({
                # Calculate Krippendorff's alpha
                kripp_result <- irr::kripp.alpha(
                    ratings_matrix,
                    method = self$options$krippMethod
                )

                # Initialize values list for table
                values_list <- list(
                    method = paste0("Krippendorff's Alpha (", self$options$krippMethod, ")"),
                    subjects = nrow(ratings_matrix),
                    raters = ncol(ratings_matrix),
                    alpha = kripp_result$value
                )

                # Calculate bootstrap CI if requested
                if (self$options$bootstrap) {
                    set.seed(123) # for reproducibility
                    n_boot <- 1000
                    alpha_boots <- numeric(n_boot)

                    for(i in 1:n_boot) {
                        boot_indices <- sample(1:nrow(ratings_matrix), replace = TRUE)
                        boot_data <- ratings_matrix[boot_indices,]

                        boot_alpha <- try(irr::kripp.alpha(boot_data,
                                                           method = self$options$krippMethod)$value,
                                          silent = TRUE)

                        if(!inherits(boot_alpha, "try-error")) {
                            alpha_boots[i] <- boot_alpha
                        }
                    }

                    # Calculate 95% confidence intervals
                    ci <- quantile(alpha_boots, c(0.025, 0.975), na.rm = TRUE)

                    # Add CI values to list
                    values_list$ci_lower <- ci[1]
                    values_list$ci_upper <- ci[2]
                }

                # Populate results table
                krippTable <- self$results$krippTable
                krippTable$setRow(rowNo = 1, values = values_list)

            }, error = function(e) {
                # Handle any errors that occur during calculation
                errorMessage <- paste("Error calculating Krippendorff's alpha:", e$message)
                warning(errorMessage)

                # Initialize values list for error case
                values_list <- list(
                    method = paste0("Krippendorff's Alpha (", self$options$krippMethod, ")"),
                    subjects = nrow(ratings_matrix),
                    raters = ncol(ratings_matrix),
                    alpha = NA
                )

                if (self$options$bootstrap) {
                    values_list$ci_lower <- NA
                    values_list$ci_upper <- NA
                }

                # Populate table with NA values
                krippTable <- self$results$krippTable
                krippTable$setRow(rowNo = 1, values = values_list)

                # Add error message as footnote
                krippTable$addFootnote(rowNo = 1, col = "alpha", paste0("Error calculating Krippendorff's alpha: ", e$message))
            })
        }

        # Agreement Status Calculation (if requested) ----
        if (self$options$agreementStatus) {
            private$.calculateAgreementStatus(ratings)
        }

        # Consensus Variable Calculation (if requested) ----
        if (self$options$consensusVar) {
            private$.createConsensusVariable(ratings)
        }

        # Bland-Altman analysis (if requested) ----
        if (self$options$blandAltmanPlot) {
            private$.populateBlandAltman(ratings)
        }

        }  # End of .run function

    ),  # End of private list

    public = list(
        #' @description
        #' Generate R source code for Interrater Reliability analysis
        #' @return Character string with R syntax for reproducible analysis outside jamovi
        asSource = function() {
            vars <- self$options$vars

            # Return empty string if insufficient variables
            if (is.null(vars) || length(vars) < 2) {
                return('')
            }

            # Escape variable names that need backticks
            vars_escaped <- sapply(vars, function(v) {
                private$.escapeVariableName(v)
            })

            # Build vars argument for function call
            # Each variable name is quoted, with backticks if needed
            vars_arg <- paste0('vars = c(',
                             paste(sapply(vars_escaped, function(v) {
                                 # If already has backticks, preserve them in quotes
                                 if (grepl("^`.*`$", v)) {
                                     paste0('"', v, '"')
                                 } else {
                                     paste0('"', v, '"')
                                 }
                             }), collapse = ', '),
                             ')')

            # Build other arguments
            args_list <- c(vars_arg)

            # Add weighted kappa option if not default
            if (self$options$wght != "unweighted") {
                wght_arg <- paste0('wght = "', self$options$wght, '"')
                args_list <- c(args_list, wght_arg)
            }

            # Add exact kappa option if enabled
            if (self$options$exct) {
                args_list <- c(args_list, 'exct = TRUE')
            }

            # Add Krippendorff's alpha options if enabled
            if (self$options$kripp) {
                args_list <- c(args_list, 'kripp = TRUE')

                if (self$options$krippMethod != "nominal") {
                    args_list <- c(args_list, paste0('krippMethod = "', self$options$krippMethod, '"'))
                }

                if (self$options$bootstrap) {
                    args_list <- c(args_list, 'bootstrap = TRUE')
                }
            }

            # Add display options if enabled
            if (self$options$sft) {
                args_list <- c(args_list, 'sft = TRUE')
            }

            if (self$options$showSummary) {
                args_list <- c(args_list, 'showSummary = TRUE')
            }

            # Build final R code
            code <- paste0(
                '# Interrater Reliability Analysis\n',
                '# Generated by ClinicoPath jamovi module\n\n',
                'library(ClinicoPath)\n\n',
                '# Load your data\n',
                '# data <- read.csv("your_data.csv")\n\n',
                'agreement(\n',
                '    data = data,\n',
                '    ', paste(args_list, collapse = ',\n    '),
                '\n)'
            )

            return(code)
        }
    )  # End of public list
    )
