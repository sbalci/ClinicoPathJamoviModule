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
            # Pre-initialize tables with column formatting and notes

            # Set confidence level in table titles where relevant
            conf_pct <- round(self$options$confLevel * 100, 0)

            # Update ICC table CI column titles
            if (!is.null(self$results$iccTable)) {
                iccTable <- self$results$iccTable
                iccTable$getColumn("ci_lower")$setSuperTitle(paste0(conf_pct, "% CI"))
                iccTable$getColumn("ci_upper")$setSuperTitle(paste0(conf_pct, "% CI"))
            }

            # Update CCC table CI column titles
            if (!is.null(self$results$linCCCTable)) {
                cccTable <- self$results$linCCCTable
                cccTable$getColumn("ci_lower")$setSuperTitle(paste0(conf_pct, "% CI"))
                cccTable$getColumn("ci_upper")$setSuperTitle(paste0(conf_pct, "% CI"))
            }

            # Update bootstrap CI table titles
            if (!is.null(self$results$bootstrapCITable)) {
                bootTable <- self$results$bootstrapCITable
                bootTable$getColumn("ci_lower")$setSuperTitle(paste0(conf_pct, "% CI"))
                bootTable$getColumn("ci_upper")$setSuperTitle(paste0(conf_pct, "% CI"))
            }

            # Update Gwet CI column titles
            if (!is.null(self$results$gwetTable)) {
                gwetTable <- self$results$gwetTable
                gwetTable$getColumn("ci_lower")$setTitle(paste0(conf_pct, "% CI Lower"))
                gwetTable$getColumn("ci_upper")$setTitle(paste0(conf_pct, "% CI Upper"))
            }
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

        .blandAltman = function(image, ggtheme, theme, ...) {
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
                ggplot2::geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue", linewidth = 1) +
                ggplot2::geom_hline(yintercept = lower_loa, linetype = "dashed", color = "red", linewidth = 0.8) +
                ggplot2::geom_hline(yintercept = upper_loa, linetype = "dashed", color = "red", linewidth = 0.8) +
                ggplot2::labs(
                    title = "Bland-Altman Plot",
                    subtitle = sprintf("Mean Difference and %g%% Limits of Agreement", conf_level * 100),
                    x = sprintf("Mean of %s and %s", rater_names[1], rater_names[2]),
                    y = sprintf("Difference (%s - %s)", rater_names[1], rater_names[2])
                ) +
                ggplot2::annotate("text", x = min(avg) + 0.9 * (max(avg) - min(avg)), y = mean_diff,
                                label = sprintf("Mean: %.2f", mean_diff),
                                hjust = 1, vjust = -0.5, color = "blue") +
                ggplot2::annotate("text", x = min(avg) + 0.9 * (max(avg) - min(avg)), y = lower_loa,
                                label = sprintf("Lower LoA: %.2f", lower_loa),
                                hjust = 1, vjust = 1.2, color = "red") +
                ggplot2::annotate("text", x = min(avg) + 0.9 * (max(avg) - min(avg)), y = upper_loa,
                                label = sprintf("Upper LoA: %.2f", upper_loa),
                                hjust = 1, vjust = -0.2, color = "red") +
                ggtheme +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5)
                )

            # Add proportional bias trend line if requested
            if (prop_bias) {
                p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linewidth = 0.8)
            }

            print(p)

            return(TRUE)
        },

        .agreementHeatmap = function(image, ggtheme, theme, ...) {
            # Render Agreement Heatmap from stored state

            plotState <- image$state

            if (is.null(plotState)) {
                return(FALSE)
            }

            # Extract data from state
            ratings_list <- plotState$ratings_list
            rater_names <- plotState$rater_names
            color_scheme <- plotState$color_scheme
            show_pct <- plotState$show_pct
            show_count <- plotState$show_count
            annot_size <- plotState$annot_size

            # Define color palette
            if (color_scheme == "bluered") {
                colors <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100)
            } else if (color_scheme == "traffic") {
                colors <- grDevices::colorRampPalette(c("#D7191C", "#FDAE61", "#A6D96A", "#1A9641"))(100)
            } else if (color_scheme == "viridis") {
                colors <- grDevices::hcl.colors(100, "viridis")
            } else {  # grayscale
                colors <- grDevices::gray.colors(100, start = 0.9, end = 0.1)
            }

            n_raters <- length(rater_names)
            pair_count <- 0
            max_pairs <- 6

            # Calculate how many pairs we'll plot
            total_pairs <- choose(n_raters, 2)
            pairs_to_plot <- min(total_pairs, max_pairs)

            # Set up plot layout
            if (pairs_to_plot == 1) {
                graphics::par(mfrow = c(1, 1), mar = c(5, 5, 3, 2))
            } else if (pairs_to_plot <= 2) {
                graphics::par(mfrow = c(1, 2), mar = c(5, 5, 3, 2))
            } else if (pairs_to_plot <= 4) {
                graphics::par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))
            } else {
                graphics::par(mfrow = c(2, 3), mar = c(5, 5, 3, 2))
            }

            # Plot each rater pair
            for (i in 1:(n_raters - 1)) {
                for (j in (i + 1):n_raters) {
                    if (pair_count >= max_pairs) break

                    rater1 <- ratings_list[[i]]
                    rater2 <- ratings_list[[j]]

                    # Create confusion matrix
                    confusion <- table(rater1, rater2)

                    # Convert to proportions
                    confusion_pct <- prop.table(confusion) * 100

                    # Create cell annotations
                    if (show_count && show_pct) {
                        cell_labels <- matrix(
                            sprintf("%d\n(%.1f%%)", confusion, confusion_pct),
                            nrow = nrow(confusion)
                        )
                    } else if (show_count) {
                        cell_labels <- matrix(sprintf("%d", confusion), nrow = nrow(confusion))
                    } else if (show_pct) {
                        cell_labels <- matrix(sprintf("%.1f%%", confusion_pct), nrow = nrow(confusion))
                    } else {
                        cell_labels <- NULL
                    }

                    # Plot heatmap
                    image(1:ncol(confusion), 1:nrow(confusion),
                         t(confusion_pct),
                         col = colors,
                         xlab = paste(rater_names[j], "(Rater 2)"),
                         ylab = paste(rater_names[i], "(Rater 1)"),
                         main = paste(rater_names[i], "vs", rater_names[j]),
                         axes = FALSE)

                    # Add axes
                    axis(1, at = 1:ncol(confusion), labels = colnames(confusion), las = 2)
                    axis(2, at = 1:nrow(confusion), labels = rownames(confusion), las = 1)

                    # Add grid
                    abline(h = (1:nrow(confusion)) + 0.5, col = "gray80", lwd = 0.5)
                    abline(v = (1:ncol(confusion)) + 0.5, col = "gray80", lwd = 0.5)

                    # Add cell annotations
                    if (!is.null(cell_labels)) {
                        for (r in 1:nrow(confusion)) {
                            for (c in 1:ncol(confusion)) {
                                text(c, r, cell_labels[r, c], cex = annot_size / 3.5)
                            }
                        }
                    }

                    # Highlight diagonal
                    for (d in 1:min(nrow(confusion), ncol(confusion))) {
                        if (rownames(confusion)[d] == colnames(confusion)[d]) {
                            rect(d - 0.5, d - 0.5, d + 0.5, d + 0.5, lwd = 2, border = "black")
                        }
                    }

                    pair_count <- pair_count + 1
                }
                if (pair_count >= max_pairs) break
            }

            return(TRUE)
        },

        .populateGwetExplanation = function() {
            # Provide educational content about Gwet's AC coefficient

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Gwet's AC Coefficient?</h3>
                    <p style='margin: 0; color: #333;'>
                        Gwet's AC coefficient solves the <strong>kappa paradox</strong> where Cohen's/Fleiss' kappa
                        can be artificially low despite high observed agreement.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Gwet's AC</h4>
                    <p style='margin: 0 0 10px 0;'><strong>This method is particularly valuable when you have:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Rare tumor subtypes</strong> – Unbalanced categories where some diagnoses are very uncommon</li>
                        <li><strong>High agreement rates</strong> – Easy-to-diagnose cases where raters agree >90% of the time</li>
                        <li><strong>Skewed marginal distributions</strong> – When one category dominates the data</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>AC1 vs AC2</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Method</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Use Case</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>AC1 (unweighted)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Nominal categories with no inherent order</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>AC2 (linear weights)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Ordinal data where adjacent disagreements are less severe</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>AC2 (quadratic weights)</strong></td>
                            <td style='padding: 8px;'>Ordinal data where larger disagreements should be heavily penalized</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Example</h4>
                    <p style='margin: 0; font-style: italic;'>
                        When diagnosing a rare tumor subtype that appears in only 2% of cases, kappa may be low (e.g., 0.40)
                        even when pathologists agree 98% of the time. Gwet's AC provides a more accurate measure
                        (e.g., 0.92) that reflects the true level of agreement.
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Gwet, K. L. (2008). Computing inter-rater reliability and its variance
                        in the presence of high agreement. <em>British Journal of Mathematical and Statistical Psychology</em>, 61(1), 29-48.
                    </p>
                </div>
            </div>
            "

            self$results$gwetExplanation$setContent(html_content)
        },

        .populateClinicalUseCases = function() {
            # Provide comprehensive clinical use cases and method selection guide

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 900px; line-height: 1.6;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 20px; margin-bottom: 20px;'>
                    <h2 style='margin: 0 0 10px 0;'>Clinical Use Cases & Method Selection Guide</h2>
                    <p style='margin: 0; opacity: 0.9;'>Choose the right agreement measure for your pathology research</p>
                </div>

                <!-- Categorical Data Methods -->
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 20px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 15px 0; color: #333;'> Categorical/Ordinal Data</h3>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px; margin-bottom: 15px;'>
                        <h4 style='margin: 0 0 10px 0; color: #333;'>Cohen's/Fleiss' Kappa (Standard Method)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use for:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Tumor grading</strong> - G1, G2, G3 classification</li>
                            <li><strong>Histologic type</strong> - Adenocarcinoma, squamous cell carcinoma, etc.</li>
                            <li><strong>Margin status</strong> - Positive, negative, close</li>
                            <li><strong>TNM staging</strong> - T1, T2, T3, T4 categories</li>
                            <li><strong>Biomarker scoring</strong> - Negative (0), 1+, 2+, 3+</li>
                        </ul>
                        <p style='margin: 0; font-size: 13px; color: #666;'><em>Note: May be problematic with high agreement or rare categories</em></p>
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px; margin-bottom: 15px;'>
                        <h4 style='margin: 0 0 10px 0; color: #333;'>Weighted Kappa (Ordinal Data)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use for ordered categories where partial credit matters:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Dysplasia grading</strong> - None, low-grade, high-grade</li>
                            <li><strong>Mitotic count categories</strong> - Low (1-9), intermediate (10-19), high (≥20)</li>
                            <li><strong>Inflammation severity</strong> - Absent, mild, moderate, severe</li>
                            <li><strong>Fibrosis stage</strong> - F0, F1, F2, F3, F4</li>
                        </ul>
                        <p style='margin: 0; padding: 10px; background: #f9f9f9; border-left: 3px solid #333; font-size: 13px;'>
                            <strong> Important:</strong> Use \"Show Level Ordering Information\" to verify proper ordering (e.g., F0 → F1 → F2 → F3 → F4)
                        </p>
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px; margin-bottom: 15px;'>
                        <h4 style='margin: 0 0 10px 0; color: #333;'>Gwet's AC1/AC2 (Kappa Paradox Solution)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Choose when you have:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Rare tumor subtypes</strong> - Neuroendocrine carcinoma in colorectal specimens (2% prevalence)</li>
                            <li><strong>High agreement rates</strong> - Benign vs malignant with 95%+ agreement</li>
                            <li><strong>Unbalanced categories</strong> - Metastasis (5% positive) vs primary (95%)</li>
                            <li><strong>Quality control studies</strong> - Most cases straightforward, few difficult</li>
                        </ul>
                        <p style='margin: 0; padding: 10px; background: #f9f9f9; border-left: 3px solid #333; font-size: 13px;'>
                            <strong> Tip:</strong> Run both Kappa and Gwet's AC - if they differ substantially, Gwet's AC is more reliable
                        </p>
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px; margin-bottom: 15px;'>
                        <h4 style='margin: 0 0 10px 0; color: #333;'>Krippendorff's Alpha (Missing Data/Flexible)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use when:</strong></p>
                        <ul style='margin: 0; padding-left: 20px;'>
                            <li><strong>Incomplete ratings</strong> - Not all pathologists rated all cases</li>
                            <li><strong>Different measurement levels</strong> - Comparing nominal, ordinal, interval data</li>
                            <li><strong>Large panels</strong> - Many raters (>5) with varying participation</li>
                        </ul>
                    </div>
                </div>

                <!-- Continuous Data Methods -->
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 20px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 15px 0; color: #333;'> Continuous/Numeric Data</h3>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px; margin-bottom: 15px;'>
                        <h4 style='margin: 0 0 10px 0; color: #333;'>ICC (Intraclass Correlation Coefficient)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use for:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Tumor size</strong> - Diameter measurements (mm or cm)</li>
                            <li><strong>Biomarker quantification</strong> - Ki-67 proliferation index (%)</li>
                            <li><strong>Cell counts</strong> - Mitotic figures per 10 HPF</li>
                            <li><strong>Morphometric analysis</strong> - Nuclear area (μm²), gland perimeter</li>
                            <li><strong>Digital pathology</strong> - Automated vs manual measurements</li>
                            <li><strong>Continuous scores</strong> - Visual analog scales (0-100)</li>
                        </ul>
                        <p style='margin: 0; padding: 10px; background: #f9f9f9; border-left: 3px solid #333; font-size: 13px;'>
                            <strong> Model selection:</strong> Use ICC(2,1) or ICC(3,1) for most pathology studies where all cases are rated by the same pathologists
                        </p>
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px; margin-bottom: 15px;'>
                        <h4 style='margin: 0 0 10px 0; color: #333;'>Bland-Altman Plot (Visual Assessment)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use alongside ICC to:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Detect systematic bias</strong> - Does one rater consistently measure higher/lower?</li>
                            <li><strong>Identify outliers</strong> - Cases with unusually large disagreement</li>
                            <li><strong>Check proportional bias</strong> - Does disagreement increase with measurement size?</li>
                            <li><strong>Define acceptable limits</strong> - What range of disagreement is clinically acceptable?</li>
                        </ul>
                        <p style='margin: 0; padding: 10px; background: #f9f9f9; border-left: 3px solid #333; font-size: 13px;'>
                            <strong> Best practice:</strong> Always use ICC + Bland-Altman together for continuous data
                        </p>
                    </div>
                </div>

                <!-- Quick Decision Guide -->
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 20px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 15px 0;'> Quick Decision Guide</h3>
                    <table style='width: 100%; background: white; color: #333; border-radius: 4px; overflow: hidden;'>
                        <tr style='background: #f8f9fa;'>
                            <th style='padding: 12px; text-align: left; border-bottom: 2px solid #dee2e6;'>Your Data Type</th>
                            <th style='padding: 12px; text-align: left; border-bottom: 2px solid #dee2e6;'>Recommended Method</th>
                        </tr>
                        <tr>
                            <td style='padding: 10px; border-bottom: 1px solid #dee2e6;'>Nominal categories (no order)</td>
                            <td style='padding: 10px; border-bottom: 1px solid #dee2e6;'><strong>Kappa</strong> or <strong>Gwet's AC1</strong></td>
                        </tr>
                        <tr>
                            <td style='padding: 10px; border-bottom: 1px solid #dee2e6;'>Ordinal categories (ordered)</td>
                            <td style='padding: 10px; border-bottom: 1px solid #dee2e6;'><strong>Weighted Kappa</strong> or <strong>Gwet's AC2</strong></td>
                        </tr>
                        <tr>
                            <td style='padding: 10px; border-bottom: 1px solid #dee2e6;'>Continuous measurements</td>
                            <td style='padding: 10px; border-bottom: 1px solid #dee2e6;'><strong>ICC + Bland-Altman</strong></td>
                        </tr>
                        <tr>
                            <td style='padding: 10px; border-bottom: 1px solid #dee2e6;'>Rare categories (< 10%)</td>
                            <td style='padding: 10px; border-bottom: 1px solid #dee2e6;'><strong>Gwet's AC</strong> (not Kappa)</td>
                        </tr>
                        <tr>
                            <td style='padding: 10px;'>Missing data/incomplete ratings</td>
                            <td style='padding: 10px;'><strong>Krippendorff's Alpha</strong></td>
                        </tr>
                    </table>
                </div>

                <!-- Sample Size Recommendations -->
                <div style='margin-top: 20px; padding: 15px; background: #f9f9f9; border-left: 4px solid #333;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'> Sample Size Recommendations</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 14px;'>
                        <li><strong>Minimum:</strong> 30 cases for stable kappa/ICC estimates</li>
                        <li><strong>Recommended:</strong> 50-100 cases for categorical data, 30-50 for continuous</li>
                        <li><strong>Rare categories:</strong> Include ≥20 positive cases if possible</li>
                        <li><strong>Pilot studies:</strong> 20 cases minimum, but interpret cautiously</li>
                    </ul>
                </div>
            </div>
            "

            self$results$clinicalUseCases$setContent(html_content)
        },

        .populateLightKappaExplanation = function() {
            # Provide educational content about Light's Kappa

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Light's Kappa?</h3>
                    <p style='margin: 0; color: #333;'>
                        Light's kappa is an alternative agreement measure for <strong>3 or more raters</strong>.
                        It calculates the <strong>average of all pairwise kappas</strong> between raters.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Light's Kappa</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Choose Light's kappa instead of Fleiss' kappa when:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Raters have different marginal distributions</strong> - Some pathologists may be more conservative/liberal than others</li>
                        <li><strong>Unequal rater characteristics</strong> - Mix of senior and junior pathologists</li>
                        <li><strong>Assumptions questionable</strong> - When Fleiss' kappa assumptions don't hold</li>
                        <li><strong>Pairwise comparisons matter</strong> - Want to understand individual rater-to-rater agreement</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Light's Kappa vs Fleiss' Kappa</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Feature</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Light's Kappa</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Fleiss' Kappa</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Method</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Average of pairwise kappas</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Overall agreement across all raters</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Assumptions</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Fewer, more flexible</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Assumes similar marginal distributions</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Use when</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Raters differ systematically</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Raters are similar/interchangeable</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Typical value</strong></td>
                            <td style='padding: 8px;'>Often slightly lower</td>
                            <td style='padding: 8px;'>Often slightly higher</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Example</h4>
                    <p style='margin: 0; font-style: italic;'>
                        In a study with 3 pathologists grading dysplasia (none, low-grade, high-grade):
                        Pathologist A is conservative (more often grades as \"none\"), Pathologist B is moderate,
                        and Pathologist C is more aggressive (more often grades as \"high-grade\").
                        Light's kappa would be more appropriate than Fleiss' kappa because the raters have
                        systematically different rating patterns.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpretation</h4>
                    <p style='margin: 0 0 5px 0;'>Use the same interpretation guidelines as Cohen's kappa:</p>
                    <table style='width: 100%; border-collapse: collapse; font-size: 14px;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>< 0.20</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Slight agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.20 – 0.40</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Fair agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.40 – 0.60</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Moderate agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.60 – 0.80</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Substantial agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>0.80 – 1.00</td>
                            <td style='padding: 5px;'>Almost perfect agreement</td>
                        </tr>
                    </table>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Light, R. J. (1971). Measures of response agreement for qualitative data: Some generalizations and alternatives.
                        <em>Psychological Bulletin</em>, 76(5), 365-377.
                    </p>
                </div>
            </div>
            "

            self$results$lightKappaExplanation$setContent(html_content)
        },

        .calculateLightKappa = function(ratings) {
            # Calculate Light's kappa for 3+ raters

            # Validate minimum number of raters
            if (ncol(ratings) < 3) {
                self$results$lightKappaTable$setNote(
                    "error",
                    "Light's kappa requires at least 3 raters. You have selected only 2 raters. Use Cohen's kappa instead."
                )
                return()
            }

            # Validate data is categorical
            if (any(sapply(ratings, is.numeric)) && !any(sapply(ratings, is.factor))) {
                self$results$lightKappaTable$setNote(
                    "error",
                    "Light's kappa requires categorical data. Your data appears to be continuous. Use ICC instead."
                )
                return()
            }

            # Prepare data - remove rows with all missing values
            complete_idx <- rowSums(!is.na(ratings)) > 0
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 2) {
                self$results$lightKappaTable$setNote(
                    "error",
                    "Insufficient complete cases for Light's kappa calculation. At least 2 cases required."
                )
                return()
            }

            # Calculate Light's kappa
            tryCatch({
                light_result <- irr::kappam.light(ratings_clean)

                # Populate table
                self$results$lightKappaTable$setRow(rowNo = 1, values = list(
                    method = "Light's Kappa (average of pairwise kappas)",
                    subjects = light_result$subjects,
                    raters = light_result$raters,
                    kappa = light_result$value,
                    p = light_result$p.value
                ))

                # Add interpretation note
                kappa_val <- light_result$value
                if (kappa_val < 0.20) {
                    interp <- "Slight agreement"
                } else if (kappa_val < 0.40) {
                    interp <- "Fair agreement"
                } else if (kappa_val < 0.60) {
                    interp <- "Moderate agreement"
                } else if (kappa_val < 0.80) {
                    interp <- "Substantial agreement"
                } else {
                    interp <- "Almost perfect agreement"
                }

                self$results$lightKappaTable$setNote(
                    "interpretation",
                    sprintf("Interpretation: %s (Landis & Koch, 1977). Light's kappa averages %d pairwise kappas between raters.",
                            interp, choose(light_result$raters, 2))
                )

            }, error = function(e) {
                self$results$lightKappaTable$setNote(
                    "error",
                    sprintf("Error calculating Light's kappa: %s", e$message)
                )
            })
        },

        .populateFinnExplanation = function() {
            # Provide educational content about Finn Coefficient

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is the Finn Coefficient?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Finn coefficient is a <strong>variance-based agreement measure</strong> for categorical data.
                        Unlike kappa-based measures that focus on exact agreement, Finn's approach uses analysis of variance
                        to quantify interrater reliability. It is <strong>especially useful when variance between raters
                        is low</strong> (i.e., when agreement is already high).
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Finn Coefficient</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Particularly useful for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>High agreement scenarios</strong> - When raters already agree well, kappa may be paradoxically low</li>
                        <li><strong>Ordered categorical data</strong> - Tumor grades (G1/G2/G3), severity scales, ordinal ratings</li>
                        <li><strong>Alternative to kappa</strong> - When kappa assumptions are violated or kappa values are unstable</li>
                        <li><strong>Low variance situations</strong> - When between-rater variance is minimal</li>
                        <li><strong>Educational/research settings</strong> - Comparing trained raters with similar rating patterns</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Finn Coefficient vs Kappa</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Feature</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Finn Coefficient</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Cohen's/Fleiss' Kappa</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Approach</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Variance-based (ANOVA)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Chance-corrected agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Best when</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Low between-rater variance</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Moderate agreement levels</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Paradox issue</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Less susceptible</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Can be paradoxically low</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Models</strong></td>
                            <td style='padding: 8px;'>One-way or two-way random effects</td>
                            <td style='padding: 8px;'>Depends on kappa variant</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Pathology Use Cases</h4>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>1. Dysplasia Grading in Barrett's Esophagus (High Agreement Scenario):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Four specialized GI pathologists grade dysplasia (none, low-grade, high-grade)</li>
                        <li><strong>Challenge:</strong> Pathologists agree >90% of the time - skewed distribution</li>
                        <li><strong>Kappa problem:</strong> Cohen's kappa paradoxically low despite excellent clinical agreement</li>
                        <li><strong>Why Finn:</strong> Variance-based approach provides stable reliability estimate in high-agreement scenarios</li>
                        <li><strong>Clinical Impact:</strong> Validates expert panel for surveillance program quality assurance</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>2. Tumor Grade Harmonization (Post-Training Assessment):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Six pathologists grade neuroendocrine tumors (G1/G2/G3) after standardization workshop</li>
                        <li><strong>Context:</strong> Training aimed to reduce inter-observer variation</li>
                        <li><strong>Expected outcome:</strong> High agreement with low between-rater variance</li>
                        <li><strong>Why Finn:</strong> ANOVA-based approach better captures improved consistency than kappa</li>
                        <li><strong>Application:</strong> Demonstrates effectiveness of training intervention</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>3. Inflammatory Activity Scoring (IBD Biopsies):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Three GI pathologists score inflammatory activity (quiescent, mild, moderate, severe)</li>
                        <li><strong>Data characteristics:</strong> Most cases are quiescent or mild (skewed distribution)</li>
                        <li><strong>Kappa limitation:</strong> Prevalence effect causes kappa to underestimate actual agreement</li>
                        <li><strong>Why Finn:</strong> Less susceptible to prevalence-related paradoxes</li>
                        <li><strong>Clinical Use:</strong> Reliable scoring for clinical trials and treatment response monitoring</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>4. Mitotic Figure Detection Training (AI vs. Pathologists):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Comparing AI algorithm with expert pathologists on mitotic count categories (low/intermediate/high)</li>
                        <li><strong>Context:</strong> AI trained on expert consensus, expecting high agreement</li>
                        <li><strong>Challenge:</strong> Most tumors are low mitotic rate (skewed toward one category)</li>
                        <li><strong>Why Finn:</strong> Provides meaningful agreement metric even with category imbalance</li>
                        <li><strong>Validation goal:</strong> Demonstrate AI reliability for breast cancer grading automation</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>5. Fibrosis Staging (Liver Biopsies - Metavir Score):</p>
                    <ul style='margin: 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Five hepatopathologists stage fibrosis (F0-F4) in chronic hepatitis C</li>
                        <li><strong>Rating pattern:</strong> Most cases F0-F2 (early stages) with excellent inter-rater consistency</li>
                        <li><strong>Statistical issue:</strong> Low variance between raters makes kappa unstable</li>
                        <li><strong>Why Finn:</strong> Variance-based model designed for low between-rater variance situations</li>
                        <li><strong>Clinical importance:</strong> Reliable staging determines antiviral treatment eligibility</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Model Types</h4>
                    <p style='margin: 0 0 10px 0;'><strong>One-way model (subjects random):</strong></p>
                    <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                        <li>Each subject may be rated by different raters</li>
                        <li>Raters are considered fixed effects</li>
                        <li>Use when rater assignment is not random</li>
                    </ul>
                    <p style='margin: 0 0 10px 0;'><strong>Two-way model (subjects and raters random):</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Subjects and raters both randomly sampled from populations</li>
                        <li>All subjects rated by all raters</li>
                        <li>Generalizes to broader population of raters</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpretation</h4>
                    <p style='margin: 0 0 5px 0;'>The Finn coefficient ranges from 0 to 1:</p>
                    <table style='width: 100%; border-collapse: collapse; font-size: 14px;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>< 0.40</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Poor agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.40 – 0.60</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Fair agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.60 – 0.75</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Good agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.75 – 0.90</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Excellent agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>0.90 – 1.00</td>
                            <td style='padding: 5px;'>Outstanding agreement</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Statistical test:</strong> F-test evaluates if agreement is significantly better than chance.
                        Significant p-value (< 0.05) indicates reliable agreement beyond random variation.
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Finn, R. H. (1970). A note on estimating the reliability of categorical data.
                        <em>Educational and Psychological Measurement</em>, 30, 71-76.
                    </p>
                </div>
            </div>
            "

            self$results$finnExplanation$setContent(html_content)
        },

        .calculateFinn = function(ratings) {
            # Calculate Finn coefficient for categorical agreement

            # Get number of rating categories from user option
            n_levels <- self$options$finnLevels

            # Get model type
            model_type <- self$options$finnModel

            # Validate minimum number of raters (need at least 2)
            if (ncol(ratings) < 2) {
                self$results$finnTable$setNote(
                    "error",
                    "Finn coefficient requires at least 2 raters."
                )
                return()
            }

            # Validate data is categorical or can be treated as categorical
            # Convert factors to numeric for finn() function
            ratings_numeric <- as.data.frame(lapply(ratings, function(x) {
                if (is.factor(x)) {
                    as.numeric(x)
                } else if (is.numeric(x)) {
                    x
                } else {
                    as.numeric(as.factor(x))
                }
            }))

            # Remove rows with missing data
            complete_idx <- complete.cases(ratings_numeric)
            ratings_clean <- ratings_numeric[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 2) {
                self$results$finnTable$setNote(
                    "error",
                    "Insufficient complete cases for Finn coefficient calculation. At least 2 cases required."
                )
                return()
            }

            # Check if specified levels match data range
            actual_max <- max(ratings_clean, na.rm = TRUE)
            actual_min <- min(ratings_clean, na.rm = TRUE)

            if (actual_max > n_levels || actual_min < 1) {
                self$results$finnTable$setNote(
                    "warning",
                    sprintf("Data range (%d to %d) may not match specified number of categories (%d).
                            Ensure your rating categories are coded as 1 to %d.",
                            round(actual_min), round(actual_max), n_levels, n_levels)
                )
            }

            # Calculate Finn coefficient
            tryCatch({
                finn_result <- irr::finn(
                    ratings = ratings_clean,
                    s.levels = n_levels,
                    model = model_type
                )

                # Get model description
                model_desc <- if (model_type == "oneway") {
                    "One-way (subjects random)"
                } else {
                    "Two-way (subjects and raters random)"
                }

                # Interpret the coefficient value
                finn_val <- finn_result$value
                if (is.na(finn_val)) {
                    interp <- "Cannot interpret"
                } else if (finn_val < 0.40) {
                    interp <- "Poor agreement"
                } else if (finn_val < 0.60) {
                    interp <- "Fair agreement"
                } else if (finn_val < 0.75) {
                    interp <- "Good agreement"
                } else if (finn_val < 0.90) {
                    interp <- "Excellent agreement"
                } else {
                    interp <- "Outstanding agreement"
                }

                # Populate table
                self$results$finnTable$setRow(rowNo = 1, values = list(
                    method = sprintf("Finn Coefficient (%s)", model_desc),
                    subjects = finn_result$subjects,
                    raters = finn_result$raters,
                    levels = n_levels,
                    finn_value = finn_result$value,
                    f_statistic = finn_result$statistic,
                    df = finn_result$stat.name,
                    p = finn_result$p.value,
                    interpretation = interp
                ))

                # Add interpretation note
                sig_text <- if (finn_result$p.value < 0.05) {
                    "statistically significant (p < 0.05)"
                } else {
                    "not statistically significant (p ≥ 0.05)"
                }

                self$results$finnTable$setNote(
                    "interpretation",
                    sprintf("%s. Agreement is %s. The variance-based approach is especially useful when rater variance is low.",
                            interp, sig_text)
                )

            }, error = function(e) {
                self$results$finnTable$setNote(
                    "error",
                    sprintf("Error calculating Finn coefficient: %s. Ensure data are categorical and properly coded (1 to %d).",
                            e$message, n_levels)
                )
            })
        },

        .populateKendallWExplanation = function() {
            # Provide educational content about Kendall's W

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Kendall's W?</h3>
                    <p style='margin: 0; color: #333;'>
                        Kendall's coefficient of concordance (W) measures the <strong>agreement among multiple raters</strong>
                        when ranking or rating ordinal data. W ranges from <strong>0 (no agreement)</strong> to
                        <strong>1 (perfect agreement)</strong>.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Kendall's W</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Particularly useful for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Rankings</strong> - When raters rank cases from best to worst (e.g., ranking diagnostic difficulty)</li>
                        <li><strong>Ordinal scales</strong> - Tumor grade (G1/G2/G3), severity scores, stage classifications</li>
                        <li><strong>Agreement on order</strong> - When you care whether raters rank cases in similar order</li>
                        <li><strong>Multiple raters (≥3)</strong> - Works best with 3 or more independent raters</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpreting Kendall's W</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>W = 0.00 - 0.20</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Very weak agreement (essentially random)</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>W = 0.21 - 0.40</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Weak agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>W = 0.41 - 0.60</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Moderate agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>W = 0.61 - 0.80</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Strong agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>W = 0.81 - 1.00</td>
                            <td style='padding: 5px;'>Very strong agreement (nearly unanimous)</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Kendall's W vs Other Methods</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 13px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Method</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Best For</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Kendall's W</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Ranking agreement, ordinal scales, consensus on ordering</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Weighted Kappa</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Exact category matching with partial credit for near-misses</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Continuous measurements (tumor size, biomarker levels)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Fleiss'/Light's Kappa</strong></td>
                            <td style='padding: 8px;'>Categorical agreement (nominal or ordinal categories)</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Pathology Use Cases</h4>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>1. Diagnostic Difficulty Ranking (Educational Assessment):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Four pathologists rank 20 melanoma cases by diagnostic difficulty (1 = easiest, 20 = most difficult)</li>
                        <li><strong>Purpose:</strong> Identify cases suitable for training vs. requiring expert consultation</li>
                        <li><strong>W = 0.75:</strong> Strong agreement - pathologists generally concur on which cases are easy vs. challenging</li>
                        <li><strong>Application:</strong> High-concordance difficult cases become core training material; low-concordance cases may need expert consensus review</li>
                        <li><strong>Educational value:</strong> Validates curriculum development for residency training programs</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>2. Tumor Grade Severity Ordering (Prognostic Validation):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Five oncologic pathologists rank 30 sarcoma cases by perceived aggressiveness (1 = least aggressive, 30 = most aggressive)</li>
                        <li><strong>Context:</strong> Validating whether pathologists intuitively rank cases consistent with formal grading systems (FNCLCC)</li>
                        <li><strong>Ordinal data:</strong> Mitotic count, differentiation score, necrosis percentage inform rankings</li>
                        <li><strong>W = 0.82:</strong> Very strong agreement - pathologists rank aggressiveness similarly</li>
                        <li><strong>Clinical validation:</strong> High W confirms grading system aligns with expert prognostic intuition</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>3. Specimen Adequacy Ranking (Quality Control):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Three cytopathologists rank 25 thyroid FNA specimens from most to least adequate for diagnosis</li>
                        <li><strong>Criteria:</strong> Cellularity, preservation quality, colloid amount, macrophage presence</li>
                        <li><strong>W = 0.68:</strong> Strong agreement on adequacy rankings</li>
                        <li><strong>Application:</strong> Top-ranked specimens used as reference standards for cytotechnologist training</li>
                        <li><strong>QC improvement:</strong> Bottom-ranked cases analyzed for pre-analytical factors (aspiration technique, fixation timing)</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>4. Inflammation Severity Ordering (Disease Activity Assessment):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Four GI pathologists rank 40 ulcerative colitis biopsies by inflammation severity</li>
                        <li><strong>Ordinal features:</strong> Crypt distortion, neutrophil infiltration, erosions, crypt abscesses</li>
                        <li><strong>Gold standard:</strong> Compare rankings to validated Nancy Histological Index scores</li>
                        <li><strong>W = 0.71:</strong> Strong concordance indicates pathologists assess severity consistently</li>
                        <li><strong>Clinical relevance:</strong> Validates ability to stratify patients for escalation of biologic therapy</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>5. Treatment Response Priority Ranking (Clinical Trial Design):</p>
                    <ul style='margin: 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Six pathologists rank 35 RECIST-assessed lung cancer cases by degree of treatment response (complete response to progressive disease)</li>
                        <li><strong>Assessment:</strong> Pre- and post-treatment tumor burden, necrosis, fibrosis, residual viable tumor</li>
                        <li><strong>Purpose:</strong> Determine if pathologic assessment aligns with radiologic RECIST criteria</li>
                        <li><strong>W = 0.79:</strong> Strong agreement on response ranking</li>
                        <li><strong>Trial application:</strong> High W supports pathologic response as trial endpoint; discordant cases identify RECIST limitations</li>
                        <li><strong>Biomarker development:</strong> Agreement validates pathologic response for predictive biomarker studies</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Statistical Significance</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        The chi-square test evaluates whether the observed agreement (W) is statistically different from
                        random chance. A significant p-value (< 0.05) indicates that raters agree more than expected by chance.
                        However, even statistically significant agreement may not be clinically meaningful if W is low.
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Kendall, M. G., & Babington Smith, B. (1939). The problem of m rankings.
                        <em>The Annals of Mathematical Statistics</em>, 10(3), 275-287.
                    </p>
                </div>
            </div>
            "

            self$results$kendallWExplanation$setContent(html_content)
        },

        .calculateKendallW = function(ratings) {
            # Calculate Kendall's coefficient of concordance (W) for ordinal/ranking data

            # Validate data is numeric or can be converted to ranks
            # Kendall's W requires either numeric data or ordinal factors
            if (!all(sapply(ratings, function(x) is.numeric(x) || is.ordered(x) || is.factor(x)))) {
                self$results$kendallWTable$setNote(
                    "error",
                    "Kendall's W requires numeric, ordinal, or factor data that can be ranked."
                )
                return()
            }

            # Prepare data - remove rows with all missing values
            complete_idx <- rowSums(!is.na(ratings)) > 0
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 2) {
                self$results$kendallWTable$setNote(
                    "error",
                    "Insufficient complete cases for Kendall's W calculation. At least 2 cases required."
                )
                return()
            }

            # Convert factors to numeric if needed (preserving ordinal structure)
            ratings_matrix <- as.matrix(ratings_clean)
            for (i in 1:ncol(ratings_matrix)) {
                if (is.factor(ratings_clean[[i]]) || is.ordered(ratings_clean[[i]])) {
                    ratings_matrix[, i] <- as.numeric(ratings_clean[[i]])
                }
            }

            # Calculate Kendall's W
            tryCatch({
                # irr::kendall returns W coefficient, Chisq, p.value
                kendall_result <- irr::kendall(ratings_matrix, correct = TRUE)

                # Populate table
                self$results$kendallWTable$setRow(rowNo = 1, values = list(
                    method = "Kendall's W (coefficient of concordance)",
                    subjects = kendall_result$subjects,
                    raters = kendall_result$raters,
                    w = kendall_result$value,
                    chisq = kendall_result$statistic,
                    df = nrow(ratings_matrix) - 1,
                    p = kendall_result$p.value
                ))

                # Add interpretation note
                w_val <- kendall_result$value
                if (w_val <= 0.20) {
                    interp <- "Very weak agreement (essentially random)"
                } else if (w_val <= 0.40) {
                    interp <- "Weak agreement"
                } else if (w_val <= 0.60) {
                    interp <- "Moderate agreement"
                } else if (w_val <= 0.80) {
                    interp <- "Strong agreement"
                } else {
                    interp <- "Very strong agreement"
                }

                # Format p-value for note
                if (kendall_result$p.value < 0.001) {
                    p_text <- "p < .001"
                } else {
                    p_text <- sprintf("p = %.3f", kendall_result$p.value)
                }

                self$results$kendallWTable$setNote(
                    "interpretation",
                    sprintf("Interpretation: %s. Chi-square test: %s, indicating agreement %s significantly different from chance.",
                            interp, p_text,
                            if(kendall_result$p.value < 0.05) "is" else "is not")
                )

            }, error = function(e) {
                self$results$kendallWTable$setNote(
                    "error",
                    sprintf("Error calculating Kendall's W: %s", e$message)
                )
            })
        },

        .populateRobinsonAExplanation = function() {
            # Provide educational content about Robinson's A

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Robinson's A?</h3>
                    <p style='margin: 0; color: #333;'>
                        Robinson's A is an <strong>ordinal agreement coefficient</strong> based on the proportion of
                        <strong>concordant pairs</strong>. It measures how often raters agree on the relative ordering
                        of cases. A ranges from <strong>-1 (complete reversal)</strong> through <strong>0 (chance agreement)</strong>
                        to <strong>+1 (perfect agreement)</strong>.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Robinson's A</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Particularly useful for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Ordinal categories with meaningful order</strong> - Tumor grades (G1/G2/G3), disease severity (mild/moderate/severe)</li>
                        <li><strong>Alternative to weighted kappa</strong> - Less affected by marginal distribution imbalances</li>
                        <li><strong>Focus on ordinal agreement</strong> - Emphasizes whether raters agree on relative ranking rather than exact categories</li>
                        <li><strong>Pairwise comparisons</strong> - Best for 2 raters; extends to multiple raters via averaging</li>
                        <li><strong>Skewed distributions</strong> - More robust than kappa when category distributions are unbalanced</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Robinson's A vs Other Ordinal Measures</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Measure</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Focus</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Sensitivity</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Robinson's A</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Concordant pairs (ordinal association)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Low to marginal distributions</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Weighted Kappa</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Exact + partial credit for near-misses</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>High to marginal distributions</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Kendall's tau-b</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Correlation (association, not agreement)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Measures correlation not agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Kendall's W</strong></td>
                            <td style='padding: 8px;'>Overall concordance (3+ raters)</td>
                            <td style='padding: 8px;'>For rankings, not categories</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Pathology Use Cases</h4>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>1. Tumor Grade Agreement (Ordinal Classification):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Two pathologists grade 80 breast carcinomas (G1, G2, G3)</li>
                        <li><strong>Challenge:</strong> Most cases are G2 (skewed distribution)</li>
                        <li><strong>Why Robinson's A:</strong> Less affected by G2 prevalence than weighted kappa</li>
                        <li><strong>A = 0.78:</strong> Strong ordinal agreement - pathologists consistently agree on relative aggressiveness</li>
                        <li><strong>Clinical value:</strong> Validates grading reliability despite category imbalance</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>2. Dysplasia Severity Assessment (Barrett's Esophagus):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Expert and community pathologist assess 100 Barrett's biopsies (none, LGD, HGD)</li>
                        <li><strong>Distribution:</strong> 70% non-dysplastic, 20% LGD, 10% HGD (highly skewed)</li>
                        <li><strong>Weighted kappa issue:</strong> Paradoxically low despite good clinical agreement</li>
                        <li><strong>Robinson's A = 0.72:</strong> Confirms strong ordinal agreement unaffected by skew</li>
                        <li><strong>Application:</strong> Validates community pathologist for surveillance program</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>3. Fibrosis Stage Concordance (Chronic Liver Disease):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Two hepatopathologists stage fibrosis using Metavir (F0-F4)</li>
                        <li><strong>Ordinal progression:</strong> F0 → F1 → F2 → F3 → F4 represents increasing severity</li>
                        <li><strong>Focus:</strong> Agreement on progression severity more important than exact stage</li>
                        <li><strong>A = 0.81:</strong> Excellent ordinal agreement on disease severity trajectory</li>
                        <li><strong>Clinical decision:</strong> Reliable staging for antiviral treatment timing</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>4. Inflammation Activity Scoring (Inflammatory Bowel Disease):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Trainee and attending score 60 IBD biopsies (quiescent, mild, moderate, severe)</li>
                        <li><strong>Learning objective:</strong> Assess if trainee recognizes severity progression</li>
                        <li><strong>Robinson's A = 0.68:</strong> Good ordinal agreement on activity trajectory</li>
                        <li><strong>Educational feedback:</strong> Trainee reliably orders cases by severity despite some category mismatches</li>
                        <li><strong>Training focus:</strong> High A validates understanding of inflammatory progression; category-specific training addresses exact scoring</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>5. Digital Pathology Validation (Gleason Grading):</p>
                    <ul style='margin: 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Compare AI algorithm vs. expert on prostate Gleason scores (6, 7, 8, 9, 10)</li>
                        <li><strong>Challenge:</strong> Most cases Gleason 6-7 (unbalanced distribution)</li>
                        <li><strong>Why Robinson's A:</strong> Emphasizes agreement on prognostic ordering (low vs. intermediate vs. high risk)</li>
                        <li><strong>A = 0.75:</strong> Strong ordinal agreement on risk stratification</li>
                        <li><strong>Validation outcome:</strong> AI reliably captures prognostic progression despite some grade discordance</li>
                        <li><strong>Regulatory value:</strong> Ordinal agreement supports AI as clinical decision support tool</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpretation Guidelines</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 14px;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>A &lt; 0.20</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Poor ordinal agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.20 ≤ A &lt; 0.40</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Fair ordinal agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.40 ≤ A &lt; 0.60</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Moderate ordinal agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.60 ≤ A &lt; 0.80</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Good ordinal agreement</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>A ≥ 0.80</td>
                            <td style='padding: 5px;'>Excellent ordinal agreement</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Statistical test:</strong> Z-test evaluates whether observed agreement is significantly
                        different from chance (A = 0). Significant p-value (< 0.05) indicates ordinal agreement exceeds random ordering.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Key Advantages</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Robust to skewed distributions:</strong> Less affected by category imbalance than kappa</li>
                        <li><strong>Ordinal focus:</strong> Emphasizes agreement on ordering rather than exact matching</li>
                        <li><strong>Intuitive interpretation:</strong> Based on concordant/discordant pairs (similar to Kendall's tau)</li>
                        <li><strong>Clinical relevance:</strong> Often more meaningful than exact agreement for prognostic/severity scales</li>
                    </ul>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Robinson, W. S. (1957). The statistical measurement of agreement.
                        <em>American Sociological Review</em>, 22(1), 17-25.
                    </p>
                </div>
            </div>
            "

            self$results$robinsonAExplanation$setContent(html_content)
        },

        .calculateRobinsonA = function(ratings) {
            # Calculate Robinson's A coefficient for ordinal agreement
            # Based on proportion of concordant pairs

            # Validate data is ordinal or numeric
            if (!all(sapply(ratings, function(x) is.numeric(x) || is.ordered(x) || is.factor(x)))) {
                self$results$robinsonATable$setNote(
                    "error",
                    "Robinson's A requires numeric, ordinal, or factor data with meaningful order."
                )
                return()
            }

            # Prepare data - remove rows with any missing values (pairwise complete)
            complete_idx <- complete.cases(ratings)
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 2) {
                self$results$robinsonATable$setNote(
                    "error",
                    "Insufficient complete cases for Robinson's A calculation. At least 2 cases required."
                )
                return()
            }

            # Convert factors to numeric (preserving ordinal structure)
            ratings_matrix <- as.matrix(ratings_clean)
            for (i in 1:ncol(ratings_matrix)) {
                if (is.factor(ratings_clean[[i]]) || is.ordered(ratings_clean[[i]])) {
                    ratings_matrix[, i] <- as.numeric(ratings_clean[[i]])
                }
            }

            n_raters <- ncol(ratings_matrix)
            n_subjects <- nrow(ratings_matrix)

            # Calculate Robinson's A
            tryCatch({
                if (n_raters == 2) {
                    # For 2 raters, calculate directly
                    robinson_result <- private$.robinsonAPairwise(ratings_matrix[, 1], ratings_matrix[, 2])
                    a_value <- robinson_result$A
                    se_value <- robinson_result$SE
                    z_value <- robinson_result$z
                    p_value <- robinson_result$p
                } else {
                    # For >2 raters, average all pairwise Robinson's A values
                    n_pairs <- n_raters * (n_raters - 1) / 2
                    pairwise_A <- numeric(n_pairs)
                    idx <- 1L
                    for (i in 1:(n_raters - 1)) {
                        for (j in (i + 1):n_raters) {
                            pair_result <- private$.robinsonAPairwise(ratings_matrix[, i], ratings_matrix[, j])
                            pairwise_A[idx] <- pair_result$A
                            idx <- idx + 1L
                        }
                    }
                    a_value <- mean(pairwise_A)
                    # Approximate SE and z-test for averaged A
                    se_value <- sd(pairwise_A) / sqrt(length(pairwise_A))
                    if (is.na(se_value) || se_value < .Machine$double.eps) {
                        z_value <- NA
                        p_value <- NA
                    } else {
                        z_value <- a_value / se_value
                        p_value <- 2 * pnorm(-abs(z_value))
                    }
                }

                # Interpret A value
                if (a_value < 0.20) {
                    interp <- "Poor ordinal agreement"
                } else if (a_value < 0.40) {
                    interp <- "Fair ordinal agreement"
                } else if (a_value < 0.60) {
                    interp <- "Moderate ordinal agreement"
                } else if (a_value < 0.80) {
                    interp <- "Good ordinal agreement"
                } else {
                    interp <- "Excellent ordinal agreement"
                }

                # Populate table
                method_text <- if (n_raters == 2) {
                    "Robinson's A (ordinal agreement)"
                } else {
                    sprintf("Robinson's A (average of %d pairwise comparisons)", choose(n_raters, 2))
                }

                self$results$robinsonATable$setRow(rowNo = 1, values = list(
                    method = method_text,
                    subjects = n_subjects,
                    raters = n_raters,
                    robinsonA = a_value,
                    se = se_value,
                    z = z_value,
                    p = p_value,
                    interpretation = interp
                ))

                # Add interpretation note
                if (p_value < 0.001) {
                    p_text <- "p < .001"
                } else {
                    p_text <- sprintf("p = %.3f", p_value)
                }

                self$results$robinsonATable$setNote(
                    "interpretation",
                    sprintf("Interpretation: %s (A = %.3f, %s). Robinson's A measures concordance of ordinal rankings. Raters agree on relative ordering %s significantly better than chance.",
                            interp, a_value, p_text,
                            if(p_value < 0.05) "" else "not")
                )

            }, error = function(e) {
                self$results$robinsonATable$setNote(
                    "error",
                    sprintf("Error calculating Robinson's A: %s", e$message)
                )
            })
        },

        .robinsonAPairwise = function(x, y) {
            # Calculate Robinson's A for a pair of raters
            # A = (C - D) / (C + D)
            # where C = concordant pairs, D = discordant pairs

            n <- length(x)

            # Count concordant and discordant pairs
            C <- 0  # Concordant
            D <- 0  # Discordant
            T <- 0  # Tied

            for (i in 1:(n - 1)) {
                for (j in (i + 1):n) {
                    diff_x <- x[i] - x[j]
                    diff_y <- y[i] - y[j]

                    if (sign(diff_x) == sign(diff_y) && diff_x != 0 && diff_y != 0) {
                        C <- C + 1
                    } else if (sign(diff_x) != sign(diff_y) && diff_x != 0 && diff_y != 0) {
                        D <- D + 1
                    } else {
                        T <- T + 1
                    }
                }
            }

            # Calculate Robinson's A
            total_pairs <- C + D + T
            if ((C + D) == 0) {
                # All pairs are tied - cannot compute meaningful A
                A <- 0
                SE <- NA
                z <- NA
                p <- 1
            } else {
                A <- (C - D) / (C + D)

                # Approximate standard error
                # SE(A) ≈ sqrt((1 - A^2) / (C + D))
                SE <- sqrt((1 - A^2) / (C + D))

                # Z-test for A significantly different from 0
                if (SE < .Machine$double.eps) {
                    z <- NA
                    p <- if (abs(A) >= 1.0 - .Machine$double.eps) 0 else 1
                } else {
                    z <- A / SE
                    p <- 2 * pnorm(-abs(z))
                }
            }

            return(list(A = A, SE = SE, z = z, p = p, C = C, D = D, T = T))
        },

        .populateMeanSpearmanExplanation = function() {
            # Provide educational content about Mean Spearman Rho

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Mean Spearman Rho?</h3>
                    <p style='margin: 0; color: #333;'>
                        Mean Spearman Rho (ρ) is the <strong>average rank correlation coefficient</strong> across all
                        pairs of raters. Spearman's rho measures the <strong>monotonic association</strong> between two
                        ordinal variables. It ranges from <strong>-1 (perfect negative correlation)</strong> through
                        <strong>0 (no correlation)</strong> to <strong>+1 (perfect positive correlation)</strong>.
                        For interrater agreement, high positive values indicate raters rank cases similarly.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Mean Spearman Rho</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Particularly useful for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Ordinal data</strong> - Tumor grades, severity scores, disease stages with rank order</li>
                        <li><strong>Ranked data</strong> - When raters rank cases from best to worst, or by priority</li>
                        <li><strong>Monotonic relationships</strong> - Tests if higher ratings by one rater correspond to higher ratings by another</li>
                        <li><strong>Robust to outliers</strong> - Uses ranks instead of raw values, less affected by extreme scores</li>
                        <li><strong>Non-normal distributions</strong> - Does not assume linear relationship or normal distribution</li>
                        <li><strong>Correlation focus</strong> - Emphasizes rank-order association rather than exact agreement</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Spearman Rho vs Other Ordinal Measures</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Measure</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>What It Measures</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Best Use</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Spearman Rho</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Monotonic rank correlation</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Ordinal data, robust to outliers</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Kendall's tau</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Concordant pairs (similar to Spearman)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Small samples, ties in data</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Robinson's A</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Ordinal agreement (concordance)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Agreement focus, skewed distributions</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Weighted Kappa</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Chance-corrected ordinal agreement</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Exact + partial credit matching</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Kendall's W</strong></td>
                            <td style='padding: 8px;'>Overall concordance (3+ raters)</td>
                            <td style='padding: 8px;'>Multiple raters, ranking agreement</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Key difference:</strong> Spearman rho measures <em>correlation</em> (association) while
                        weighted kappa and Robinson's A measure <em>agreement</em> (concordance). High correlation indicates
                        raters rank similarly but may use different scales.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Pathology Use Cases</h4>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>1. Tumor Grade Severity Correlation (Multi-Rater Consistency):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Four pathologists grade 50 neuroendocrine tumors (G1, G2, G3) based on Ki-67 and mitotic count</li>
                        <li><strong>Purpose:</strong> Assess if pathologists rank tumors by aggressiveness consistently</li>
                        <li><strong>Mean ρ = 0.85:</strong> Strong rank correlation - pathologists agree on relative tumor aggressiveness</li>
                        <li><strong>Clinical value:</strong> High correlation validates grading system reliability for prognostic stratification</li>
                        <li><strong>Advantage:</strong> Less sensitive to exact grade disagreement (G1 vs. G2) than kappa; focuses on ordinal progression</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>2. Inflammation Severity Ranking (Cross-Specialty Agreement):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> GI pathologist and general pathologist rank 60 IBD biopsies by inflammatory activity (quiescent, mild, moderate, severe)</li>
                        <li><strong>Challenge:</strong> Specialists may use different thresholds but recognize same severity progression</li>
                        <li><strong>Mean ρ = 0.78:</strong> Good rank correlation despite potential systematic differences in grading</li>
                        <li><strong>Interpretation:</strong> Both pathologists recognize severity trajectory; category-specific calibration may still be needed</li>
                        <li><strong>Application:</strong> Validates general pathologist competency for IBD assessment with specialist oversight</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>3. Fibrosis Progression Assessment (Longitudinal Consistency):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Three hepatopathologists rank 40 liver biopsies by fibrosis severity (Ishak 0-6 or Metavir F0-F4)</li>
                        <li><strong>Ordinal nature:</strong> Fibrosis stages represent clear progression from none to cirrhosis</li>
                        <li><strong>Mean ρ = 0.89:</strong> Excellent rank correlation on disease progression</li>
                        <li><strong>Clinical decision:</strong> Consistent severity ranking supports reliable treatment timing recommendations</li>
                        <li><strong>Robust to staging system:</strong> Correlation reflects biological progression regardless of exact staging scheme</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>4. Diagnostic Difficulty Ranking (Educational Calibration):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Five dermatopathologists rank 30 melanocytic lesions by diagnostic difficulty (1 = easiest, 30 = most difficult)</li>
                        <li><strong>Educational goal:</strong> Identify cases with consensus difficulty for training curriculum</li>
                        <li><strong>Mean ρ = 0.72:</strong> Good correlation on difficulty progression</li>
                        <li><strong>Case selection:</strong> High-correlation difficult cases become core teaching material</li>
                        <li><strong>Low-correlation cases:</strong> Reveal pathologist-specific knowledge gaps requiring targeted education</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>5. AI Algorithm Validation (Prognostic Ordering):</p>
                    <ul style='margin: 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Compare AI algorithm vs. three expert pathologists on Gleason grading (6-10) for 100 prostate cores</li>
                        <li><strong>Validation focus:</strong> AI should rank cases by prognostic severity similar to experts</li>
                        <li><strong>Mean ρ = 0.81:</strong> Strong rank correlation between AI and expert consensus</li>
                        <li><strong>Regulatory value:</strong> High correlation demonstrates AI captures prognostic progression</li>
                        <li><strong>Advantage:</strong> Robust measure unaffected by systematic grade shifts (AI consistently one grade higher/lower)</li>
                        <li><strong>Clinical deployment:</strong> Supports AI as decision support for risk stratification</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpretation Guidelines</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 14px;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>ρ &lt; 0.30</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Negligible correlation</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.30 ≤ ρ &lt; 0.50</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Low correlation</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.50 ≤ ρ &lt; 0.70</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Moderate correlation</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.70 ≤ ρ &lt; 0.90</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>High correlation</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>ρ ≥ 0.90</td>
                            <td style='padding: 5px;'>Very high correlation (nearly identical rankings)</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Note:</strong> For interrater agreement, Mean ρ represents the average correlation
                        across all rater pairs. Min/Max values show range of pairwise correlations, indicating
                        heterogeneity among rater pairs.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Key Advantages</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Nonparametric:</strong> No assumptions about distribution or linearity</li>
                        <li><strong>Robust to outliers:</strong> Uses ranks, not raw values</li>
                        <li><strong>Monotonic focus:</strong> Detects any consistent increasing/decreasing trend</li>
                        <li><strong>Scale-independent:</strong> Works when raters use different thresholds but agree on ordering</li>
                        <li><strong>Interpretable:</strong> Familiar correlation coefficient interpretation</li>
                    </ul>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Spearman, C. (1904). The proof and measurement of association between two things.
                        <em>American Journal of Psychology</em>, 15(1), 72-101.
                    </p>
                </div>
            </div>
            "

            self$results$meanSpearmanExplanation$setContent(html_content)
        },

        .calculateMeanSpearman = function(ratings) {
            # Calculate mean Spearman rho across all rater pairs

            # Validate data is numeric or ordinal
            if (!all(sapply(ratings, function(x) is.numeric(x) || is.ordered(x) || is.factor(x)))) {
                self$results$meanSpearmanTable$setNote(
                    "error",
                    "Mean Spearman Rho requires numeric, ordinal, or factor data that can be ranked."
                )
                return()
            }

            # Prepare data - remove rows with any missing values
            complete_idx <- complete.cases(ratings)
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 3) {
                self$results$meanSpearmanTable$setNote(
                    "error",
                    "Insufficient complete cases for Spearman correlation. At least 3 cases required."
                )
                return()
            }

            # Convert factors to numeric (preserving ordinal structure)
            ratings_matrix <- as.matrix(ratings_clean)
            for (i in 1:ncol(ratings_matrix)) {
                if (is.factor(ratings_clean[[i]]) || is.ordered(ratings_clean[[i]])) {
                    ratings_matrix[, i] <- as.numeric(ratings_clean[[i]])
                }
            }

            n_raters <- ncol(ratings_matrix)
            n_subjects <- nrow(ratings_matrix)

            if (n_raters < 2) {
                self$results$meanSpearmanTable$setNote(
                    "error",
                    "At least 2 raters required for correlation analysis."
                )
                return()
            }

            # Calculate Spearman correlation for all rater pairs
            tryCatch({
                pairwise_rho <- c()

                for (i in 1:(n_raters - 1)) {
                    for (j in (i + 1):n_raters) {
                        # Use cor.test for Spearman correlation
                        spearman_test <- cor.test(ratings_matrix[, i], ratings_matrix[, j],
                                                   method = "spearman", exact = FALSE)
                        pairwise_rho <- c(pairwise_rho, spearman_test$estimate)
                    }
                }

                # Calculate statistics
                mean_rho <- mean(pairwise_rho)
                min_rho <- min(pairwise_rho)
                max_rho <- max(pairwise_rho)

                # Interpret mean rho
                if (mean_rho < 0.30) {
                    interp <- "Negligible rank correlation"
                } else if (mean_rho < 0.50) {
                    interp <- "Low rank correlation"
                } else if (mean_rho < 0.70) {
                    interp <- "Moderate rank correlation"
                } else if (mean_rho < 0.90) {
                    interp <- "High rank correlation"
                } else {
                    interp <- "Very high rank correlation"
                }

                # Populate table
                n_pairs <- choose(n_raters, 2)
                method_text <- if (n_raters == 2) {
                    "Spearman's rho (rank correlation)"
                } else {
                    sprintf("Mean Spearman's rho (average of %d pairwise correlations)", n_pairs)
                }

                self$results$meanSpearmanTable$setRow(rowNo = 1, values = list(
                    method = method_text,
                    subjects = n_subjects,
                    raters = n_raters,
                    meanRho = mean_rho,
                    minRho = min_rho,
                    maxRho = max_rho,
                    interpretation = interp
                ))

                # Add interpretation note
                range_text <- sprintf("[%.3f, %.3f]", min_rho, max_rho)
                self$results$meanSpearmanTable$setNote(
                    "interpretation",
                    sprintf("Interpretation: %s (Mean ρ = %.3f). Pairwise correlations range from %s, indicating %s among rater pairs. High positive correlations indicate raters rank cases similarly.",
                            interp, mean_rho, range_text,
                            if(max_rho - min_rho < 0.20) "consistent agreement" else "variability in agreement")
                )

            }, error = function(e) {
                self$results$meanSpearmanTable$setNote(
                    "error",
                    sprintf("Error calculating Mean Spearman Rho: %s", e$message)
                )
            })
        },

        .populateMeanPearsonExplanation = function() {
            # Provide educational content about Mean Pearson Correlation

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Mean Pearson Correlation?</h3>
                    <p style='margin: 0; color: #333;'>
                        Mean Pearson Correlation (r) is the <strong>average linear correlation coefficient</strong> across all
                        pairs of raters for continuous measurements. Pearson's r measures the <strong>strength and direction
                        of linear association</strong> between two variables. It ranges from <strong>-1 (perfect negative)</strong>
                        through <strong>0 (no linear relationship)</strong> to <strong>+1 (perfect positive correlation)</strong>.
                        For interrater agreement, high positive values indicate raters' measurements vary together linearly.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Mean Pearson Correlation</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Particularly useful for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Continuous measurements</strong> - Tumor size (mm), biomarker concentrations (ng/mL), quantitative scores</li>
                        <li><strong>Linear relationships</strong> - When measurements should vary proportionally between raters</li>
                        <li><strong>Normally distributed data</strong> - Optimal when data approximates normal distribution</li>
                        <li><strong>Correlation focus</strong> - Assesses if raters' measurements covary, not absolute agreement</li>
                        <li><strong>Simple interpretation</strong> - Familiar correlation coefficient widely understood in research</li>
                        <li><strong>Complements ICC</strong> - ICC measures absolute agreement, Pearson measures relative association</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Pearson r vs Other Continuous Measures</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Measure</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>What It Measures</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Best Use</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Pearson r</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Linear correlation (association)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Continuous data, linear relationships</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Absolute agreement (concordance)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>When raters should give identical values</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Spearman Rho</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Monotonic rank correlation</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Ordinal data, non-linear relationships, outliers</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Bland-Altman</strong></td>
                            <td style='padding: 8px;'>Limits of agreement (bias and precision)</td>
                            <td style='padding: 8px;'>Method comparison, detecting systematic bias</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Key difference:</strong> Pearson r measures <em>correlation</em> (how measurements covary) while
                        ICC measures <em>agreement</em> (how close measurements are). Two raters can have perfect correlation (r = 1.0)
                        but poor agreement if one systematically measures higher/lower than the other.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Pathology Use Cases</h4>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>1. Tumor Measurement Agreement (Longest Diameter):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Three pathologists measure tumor size (mm) on 60 H&E slides using calibrated microscopes</li>
                        <li><strong>Continuous scale:</strong> Tumor diameter ranges from 5-85 mm with normal distribution</li>
                        <li><strong>Mean r = 0.94:</strong> Very high linear correlation - measurements vary together proportionally</li>
                        <li><strong>Clinical interpretation:</strong> Pathologists agree on relative tumor sizes despite potential measurement variations</li>
                        <li><strong>Advantage:</strong> Simple correlation reveals if raters rank tumors similarly by size</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>2. Biomarker Quantification (Ki-67 Percentage):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Four pathologists estimate Ki-67 proliferation index (0-100%) on 50 breast cancers</li>
                        <li><strong>Challenge:</strong> Visual estimation varies between observers but should correlate</li>
                        <li><strong>Mean r = 0.88:</strong> High correlation indicates pathologists identify high vs. low proliferation consistently</li>
                        <li><strong>ICC comparison:</strong> ICC may be lower due to systematic differences in thresholds, but correlation remains high</li>
                        <li><strong>Clinical value:</strong> Validates Ki-67 for risk stratification despite inter-observer scaling differences</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>3. Digital Image Analysis (Nuclear Area Measurement):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Compare three image analysis algorithms measuring nuclear area (μm²) on 100 lymphoma cells</li>
                        <li><strong>Continuous output:</strong> Algorithms provide precise continuous measurements</li>
                        <li><strong>Mean r = 0.96:</strong> Excellent correlation - algorithms rank nuclear sizes identically</li>
                        <li><strong>Systematic bias:</strong> One algorithm may consistently measure 5% larger, but correlation remains perfect</li>
                        <li><strong>Application:</strong> Algorithms capture same biological variation despite calibration differences</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>4. Mitotic Count Concordance (Per 10 HPF):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Five pathologists count mitotic figures per 10 HPF in 40 sarcoma cases (range: 0-35)</li>
                        <li><strong>Count data:</strong> Discrete counts treated as continuous for correlation</li>
                        <li><strong>Mean r = 0.82:</strong> High correlation - pathologists identify mitotically active vs. inactive tumors</li>
                        <li><strong>Prognostic value:</strong> High correlation validates mitotic count for grading despite count variability</li>
                        <li><strong>Training assessment:</strong> Lower correlations identify pathologists needing mitosis recognition training</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>5. Automated Quantification Validation (Collagen Area Fraction):</p>
                    <ul style='margin: 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Validate AI algorithm vs. manual quantification of fibrosis (collagen %) on 70 liver biopsies</li>
                        <li><strong>Measurement:</strong> Continuous percentage (0-100%) from Sirius Red staining</li>
                        <li><strong>Comparison:</strong> AI algorithm vs. three expert hepatopathologists</li>
                        <li><strong>Mean r = 0.91:</strong> Excellent correlation between AI and human quantification</li>
                        <li><strong>Regulatory significance:</strong> High correlation supports AI for automated fibrosis quantification</li>
                        <li><strong>Efficiency gain:</strong> AI provides objective, reproducible measurements with human-level correlation</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpretation Guidelines</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 14px;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>r &lt; 0.30</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Negligible correlation</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.30 ≤ r &lt; 0.50</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Low correlation</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.50 ≤ r &lt; 0.70</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Moderate correlation</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.70 ≤ r &lt; 0.90</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>High correlation</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>r ≥ 0.90</td>
                            <td style='padding: 5px;'>Very high correlation (nearly perfect linear relationship)</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Note:</strong> For interrater agreement, Mean r represents the average correlation
                        across all rater pairs. Min/Max values reveal heterogeneity among pairs. High correlation
                        indicates measurements covary linearly but does NOT guarantee absolute agreement.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Correlation vs. Agreement</h4>
                    <p style='margin: 0 0 10px 0; font-size: 13px;'>
                        <strong>Important distinction:</strong> Pearson correlation measures how measurements <em>vary together</em>,
                        not how <em>close</em> they are. Example:
                    </p>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px; line-height: 1.6;'>
                        <li><strong>Perfect correlation (r = 1.0):</strong> Rater A measures 10, 20, 30 mm; Rater B measures 20, 40, 60 mm
                            (doubled values - perfect linear relationship but poor agreement)</li>
                        <li><strong>Good agreement (ICC = 0.95):</strong> Rater A: 10, 20, 30 mm; Rater B: 11, 19, 31 mm
                            (nearly identical values - may have slightly lower correlation if measurement error is random)</li>
                    </ul>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Recommendation:</strong> Use Pearson r when assessing if raters <em>rank</em> measurements similarly.
                        Use ICC when raters should produce <em>identical</em> measurements. Often report both for comprehensive assessment.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Key Advantages</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Simple and familiar:</strong> Widely understood correlation coefficient</li>
                        <li><strong>Sensitive to linear trends:</strong> Detects proportional relationships between raters</li>
                        <li><strong>Complements ICC:</strong> Provides different perspective (correlation vs. agreement)</li>
                        <li><strong>Scale-independent:</strong> Unaffected by systematic shifts (all measurements +10 mm)</li>
                        <li><strong>Hypothesis testing:</strong> Built-in significance test for correlation ≠ 0</li>
                    </ul>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Pearson, K. (1895). Note on regression and inheritance in the case of two parents.
                        <em>Proceedings of the Royal Society of London</em>, 58, 240-242.
                    </p>
                </div>
            </div>
            "

            self$results$meanPearsonExplanation$setContent(html_content)
        },

        .calculateMeanPearson = function(ratings) {
            # Calculate mean Pearson correlation across all rater pairs

            # Validate data is numeric
            if (!all(sapply(ratings, function(x) is.numeric(x)))) {
                self$results$meanPearsonTable$setNote(
                    "error",
                    "Mean Pearson Correlation requires numeric (continuous) data."
                )
                return()
            }

            # Prepare data - remove rows with any missing values
            complete_idx <- complete.cases(ratings)
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 3) {
                self$results$meanPearsonTable$setNote(
                    "error",
                    "Insufficient complete cases for Pearson correlation. At least 3 cases required."
                )
                return()
            }

            n_raters <- ncol(ratings_clean)
            n_subjects <- nrow(ratings_clean)

            if (n_raters < 2) {
                self$results$meanPearsonTable$setNote(
                    "error",
                    "At least 2 raters required for correlation analysis."
                )
                return()
            }

            # Calculate Pearson correlation for all rater pairs
            tryCatch({
                pairwise_r <- c()

                for (i in 1:(n_raters - 1)) {
                    for (j in (i + 1):n_raters) {
                        # Use cor.test for Pearson correlation
                        pearson_test <- cor.test(ratings_clean[[i]], ratings_clean[[j]],
                                                  method = "pearson")
                        pairwise_r <- c(pairwise_r, pearson_test$estimate)
                    }
                }

                # Calculate statistics
                mean_r <- mean(pairwise_r)
                min_r <- min(pairwise_r)
                max_r <- max(pairwise_r)

                # Interpret mean r
                if (mean_r < 0.30) {
                    interp <- "Negligible linear correlation"
                } else if (mean_r < 0.50) {
                    interp <- "Low linear correlation"
                } else if (mean_r < 0.70) {
                    interp <- "Moderate linear correlation"
                } else if (mean_r < 0.90) {
                    interp <- "High linear correlation"
                } else {
                    interp <- "Very high linear correlation"
                }

                # Populate table
                n_pairs <- choose(n_raters, 2)
                method_text <- if (n_raters == 2) {
                    "Pearson's r (linear correlation)"
                } else {
                    sprintf("Mean Pearson's r (average of %d pairwise correlations)", n_pairs)
                }

                self$results$meanPearsonTable$setRow(rowNo = 1, values = list(
                    method = method_text,
                    subjects = n_subjects,
                    raters = n_raters,
                    meanR = mean_r,
                    minR = min_r,
                    maxR = max_r,
                    interpretation = interp
                ))

                # Add interpretation note
                range_text <- sprintf("[%.3f, %.3f]", min_r, max_r)
                self$results$meanPearsonTable$setNote(
                    "interpretation",
                    sprintf("Interpretation: %s (Mean r = %.3f). Pairwise correlations range from %s, indicating %s among rater pairs. High correlations indicate measurements vary together linearly. Note: Correlation measures association, not absolute agreement.",
                            interp, mean_r, range_text,
                            if(max_r - min_r < 0.20) "consistent correlation" else "variability in correlation")
                )

            }, error = function(e) {
                self$results$meanPearsonTable$setNote(
                    "error",
                    sprintf("Error calculating Mean Pearson Correlation: %s", e$message)
                )
            })
        },

        .populateLinCCCExplanation = function() {
            # Provide educational content about Lin's Concordance Correlation Coefficient

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Lin's Concordance Correlation Coefficient (CCC)?</h3>
                    <p style='margin: 0; color: #333;'>
                        Lin's CCC evaluates <strong>agreement</strong> (not just association) by measuring both:
                    </p>
                    <ul style='margin: 10px 0 0 0; padding-left: 20px;'>
                        <li><strong>Precision</strong> - How closely measurements correlate (Pearson's r)</li>
                        <li><strong>Accuracy</strong> - How close measurements are to perfect agreement (bias correction factor C<sub>b</sub>)</li>
                    </ul>
                    <p style='margin: 10px 0 0 0; color: #333;'>
                        <strong>Formula:</strong> CCC = r × C<sub>b</sub>, where C<sub>b</sub> = 2 / (v + 1/v + u²)
                    </p>
                    <p style='margin: 5px 0 0 0; color: #666; font-size: 0.9em;'>
                        v = σ<sub>x</sub> / σ<sub>y</sub> (scale shift), u = (μ<sub>x</sub> - μ<sub>y</sub>) / √(σ<sub>x</sub>σ<sub>y</sub>) (location shift)
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Lin's CCC</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for method comparison and validation:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Method comparison</strong> - Comparing manual vs. automated measurements</li>
                        <li><strong>Instrument validation</strong> - Assessing new equipment against gold standard</li>
                        <li><strong>Observer agreement</strong> - Evaluating measurement consistency between raters</li>
                        <li><strong>Quality control</strong> - Monitoring measurement system performance over time</li>
                        <li><strong>Biomarker validation</strong> - Comparing different assay platforms</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpreting CCC Values</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>CCC Value</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Agreement Strength</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Interpretation</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>&lt; 0.40</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Poor</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Substantial disagreement - methods not interchangeable</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.40 - 0.70</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Fair</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Consider method calibration before use</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.70 - 0.90</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Moderate</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Acceptable for some applications with caution</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.90 - 0.95</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Good</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Methods reasonably interchangeable</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.95 - 0.99</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Substantial</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Methods highly interchangeable</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>&gt; 0.99</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Almost perfect</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Excellent interchangeability</td>
                        </tr>
                    </table>
                    <p style='margin: 15px 0 0 0; font-style: italic; color: #555;'>
                        Note: Thresholds are context-dependent. Clinical applications may require CCC > 0.95 for safety-critical measurements.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>CCC vs. Pearson's r: Key Differences</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Characteristic</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Pearson's r</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Lin's CCC</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Measures</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Linear association only</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Agreement (precision + accuracy)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Systematic bias</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Ignored (r=1 even with bias)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Penalized (CCC < 1 with bias)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Scale differences</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Ignored</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Penalized</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Best use</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Association studies</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Method comparison, agreement</td>
                        </tr>
                    </table>
                    <p style='margin: 15px 0 0 0;'>
                        <strong>Example:</strong> Two methods with r = 1.0 but Method B = 2 × Method A have perfect correlation
                        but poor agreement. CCC would be substantially < 1.0, correctly identifying the systematic difference.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Use Cases in Pathology</h4>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>1</span>
                            Manual vs. Digital Ki-67 Quantification
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Validate digital image analysis software against manual Ki-67 counting
                            by expert pathologists on 100 breast cancer cases (0-100% proliferation index).
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Pearson's r = 0.96 (excellent correlation)<br>
                            • CCC = 0.88 (moderate agreement)<br>
                            • Bias correction factor C<sub>b</sub> = 0.92 (8% accuracy loss)<br>
                            • Digital consistently 3-5% lower than manual
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> High correlation (r=0.96) masks systematic underestimation
                            by digital method. CCC (0.88) correctly identifies poor agreement requiring calibration before
                            clinical use.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Apply +4% calibration correction to digital scores to achieve
                            CCC > 0.95 for clinical implementation.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>2</span>
                            Whole Slide Imaging vs. Glass Slide Mitotic Count
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Four pathologists count mitoses per 10 HPF on 80 breast cancer cases
                            using both glass slides and whole slide images (WSI).
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results (averaged across pathologists):</strong><br>
                            • Pearson's r = 0.94<br>
                            • CCC = 0.93<br>
                            • Bias correction factor C<sub>b</sub> = 0.99 (minimal bias)<br>
                            • Mean difference: WSI 0.3 mitoses/10HPF higher (not significant)
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> Excellent agreement (CCC=0.93) with minimal systematic
                            bias (C<sub>b</sub>=0.99). High r and high C<sub>b</sub> both contribute to strong CCC.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> WSI validated for mitotic count assessment; can replace glass slides
                            for this application without calibration.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>3</span>
                            PD-L1 TPS: Two IHC Antibody Clones Comparison
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Compare PD-L1 tumor proportion scores (0-100%) between 22C3 and SP263
                            antibody clones on 150 NSCLC cases.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Pearson's r = 0.89<br>
                            • CCC = 0.75<br>
                            • Bias correction factor C<sub>b</sub> = 0.84 (16% accuracy loss)<br>
                            • SP263 scores average 8% higher than 22C3
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> Poor agreement (CCC=0.75) despite good correlation
                            (r=0.89). Low C<sub>b</sub> (0.84) indicates substantial systematic bias between clones.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Clones are NOT interchangeable for therapy selection cutoffs
                            (1%, 50%). Use clone-specific thresholds or apply correction factors.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>4</span>
                            Tumor Size: Radiology vs. Pathology Measurement
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Compare preoperative imaging (CT/MRI) vs. pathology gross measurement
                            for tumor size (mm) in 200 breast cancer cases.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Pearson's r = 0.82<br>
                            • CCC = 0.67<br>
                            • Bias correction factor C<sub>b</sub> = 0.82<br>
                            • Imaging overestimates by 3.2 mm on average (includes surrounding edema)
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> Poor agreement (CCC=0.67) due to both imperfect
                            correlation (r=0.82) and systematic bias (C<sub>b</sub>=0.82). Both precision and accuracy
                            contribute to disagreement.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Pathology remains gold standard for tumor staging. Radiologic
                            measurements require -3mm correction factor for surgical planning but still have high
                            individual case variability.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>5</span>
                            HER2/neu Copy Number: FISH vs. Chromogenic ISH
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Compare HER2 gene copy number (continuous ratio HER2/CEP17) between
                            FISH and chromogenic ISH (CISH) on 120 breast cancer cases.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Pearson's r = 0.97<br>
                            • CCC = 0.96<br>
                            • Bias correction factor C<sub>b</sub> = 0.99 (minimal bias)<br>
                            • Mean difference: CISH 0.05 ratio units lower (not clinically significant)
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> Excellent agreement (CCC=0.96) with both high precision
                            (r=0.97) and high accuracy (C<sub>b</sub>=0.99). Methods are essentially equivalent.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> CISH validated as acceptable alternative to FISH for HER2 testing.
                            Methods are interchangeable at standard cutoffs (ratio ≥2.0).
                        </p>
                    </div>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'> Important Considerations</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Sample size:</strong> Minimum 30 paired observations; 100+ preferred for stable estimates</li>
                        <li><strong>Outliers:</strong> CCC is sensitive to outliers; investigate extreme values before analysis</li>
                        <li><strong>Range restriction:</strong> Limited measurement range can artificially inflate CCC</li>
                        <li><strong>Missing data:</strong> Only complete pairs are analyzed; ensure balanced missingness</li>
                        <li><strong>Clinical context:</strong> CCC thresholds vary by application; safety-critical uses require higher values</li>
                        <li><strong>Complementary analysis:</strong> Use with Bland-Altman plots to visualize bias patterns</li>
                        <li><strong>Multiple comparisons:</strong> For 3+ raters, all pairwise CCCs calculated; consider adjustment</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Statistical Notes</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>CCC formula:</strong> ρ<sub>c</sub> = 2ρσ<sub>x</sub>σ<sub>y</sub> / (σ<sub>x</sub>² + σ<sub>y</sub>² + (μ<sub>x</sub> - μ<sub>y</sub>)²)</li>
                        <li><strong>Components:</strong> CCC = r × C<sub>b</sub>, where C<sub>b</sub> is bias correction factor</li>
                        <li><strong>Confidence intervals:</strong> Calculated using Fisher's Z-transformation</li>
                        <li><strong>Null hypothesis:</strong> CCC = 0 (no concordance); test using asymptotic variance</li>
                        <li><strong>Assumptions:</strong> Bivariate normal distribution preferred but not strictly required</li>
                        <li><strong>Relationship to ICC:</strong> CCC for 2 raters ≈ ICC(2,1) under certain conditions</li>
                    </ul>
                </div>
            </div>
            "

            self$results$linCCCExplanation$setContent(html_content)
        },

        .calculateLinCCC = function(ratings) {
            # Calculate Lin's Concordance Correlation Coefficient for all pairwise comparisons

            tryCatch({
                # Remove rows with missing values
                complete_rows <- complete.cases(ratings)
                ratings_complete <- ratings[complete_rows, , drop = FALSE]

                n_cases <- nrow(ratings_complete)
                n_raters <- ncol(ratings_complete)

                if (n_cases < 10) {
                    self$results$linCCCTable$setNote(
                        "error",
                        sprintf("Lin's CCC requires at least 10 complete paired observations. Found %d.", n_cases)
                    )
                    return()
                }

                if (n_raters < 2) {
                    self$results$linCCCTable$setNote(
                        "error",
                        "Lin's CCC requires at least 2 raters/methods for comparison."
                    )
                    return()
                }

                # Check that all data is numeric
                is_all_numeric <- all(sapply(ratings_complete, function(x) is.numeric(x) && !is.factor(x)))

                if (!is_all_numeric) {
                    self$results$linCCCTable$setNote(
                        "error",
                        "Lin's CCC requires continuous numeric data. Categorical data found."
                    )
                    return()
                }

                # Calculate CCC for all pairwise combinations
                cccTable <- self$results$linCCCTable
                rater_names <- colnames(ratings_complete)

                for (i in 1:(n_raters - 1)) {
                    for (j in (i + 1):n_raters) {
                        x <- ratings_complete[[i]]
                        y <- ratings_complete[[j]]

                        comparison_name <- paste(rater_names[i], "vs", rater_names[j])

                        # Calculate CCC components
                        mean_x <- mean(x)
                        mean_y <- mean(y)
                        sd_x <- sd(x)
                        sd_y <- sd(y)

                        # Guard against zero variance
                        if (sd_x < .Machine$double.eps || sd_y < .Machine$double.eps) {
                            cccTable$addRow(rowKey = comparison_name, list(
                                comparison = comparison_name,
                                subjects = n_cases,
                                ccc = NA_real_,
                                ci_lower = NA_real_,
                                ci_upper = NA_real_,
                                pearson_r = NA_real_,
                                bias_factor = NA_real_,
                                interpretation = "Cannot compute: one rater has zero variance (all values identical)"
                            ))
                            next
                        }

                        # Pearson correlation (precision)
                        pearson_r <- cor(x, y, method = "pearson")

                        # Scale shift
                        v <- sd_x / sd_y

                        # Location shift (standardized mean difference)
                        u <- (mean_x - mean_y) / sqrt(sd_x * sd_y)

                        # Bias correction factor (accuracy)
                        Cb <- 2 / (v + 1/v + u^2)

                        # Lin's CCC
                        ccc <- pearson_r * Cb

                        # Prefer validated CI implementation when available
                        ci_lower <- NA_real_
                        ci_upper <- NA_real_
                        if (requireNamespace("DescTools", quietly = TRUE)) {
                            ccc_result <- tryCatch(
                                DescTools::CCC(x, y, ci = "z-transform"),
                                error = function(e) NULL
                            )
                            if (!is.null(ccc_result) && !is.null(ccc_result$rho.c)) {
                                ccc <- ccc_result$rho.c$est[1]
                                ci_lower <- ccc_result$rho.c$lwr.ci[1]
                                ci_upper <- ccc_result$rho.c$upr.ci[1]
                                Cb <- ccc_result$C.b
                            }
                        }

                        # Fallback CI approximation if DescTools is unavailable
                        if (is.na(ci_lower) || is.na(ci_upper)) {
                            n <- n_cases
                            conf <- self$options$confLevel
                            if (is.finite(pearson_r) && abs(pearson_r) > .Machine$double.eps && is.finite(ccc)) {
                                var_ccc <- ((1 - pearson_r^2) * ccc^2 * (1 - ccc^2)) / (pearson_r^2 * n)
                                if (is.finite(var_ccc) && var_ccc >= 0) {
                                    se_ccc <- sqrt(var_ccc)
                                    z_crit <- qnorm((1 + conf) / 2)
                                    ci_lower <- ccc - z_crit * se_ccc
                                    ci_upper <- ccc + z_crit * se_ccc
                                }
                            }
                            ci_lower <- if (is.finite(ci_lower)) max(-1, ci_lower) else NA_real_
                            ci_upper <- if (is.finite(ci_upper)) min(1, ci_upper) else NA_real_
                        }

                        # Interpretation (McBride 2005, adjusted for clinical research)
                        if (ccc < 0.40) {
                            interp <- "Poor agreement - methods not interchangeable"
                        } else if (ccc < 0.70) {
                            interp <- "Fair agreement - consider method calibration"
                        } else if (ccc < 0.90) {
                            interp <- "Moderate agreement - use with caution"
                        } else if (ccc < 0.95) {
                            interp <- "Good agreement - methods reasonably interchangeable"
                        } else if (ccc < 0.99) {
                            interp <- "Substantial agreement - methods highly interchangeable"
                        } else {
                            interp <- "Almost perfect agreement - excellent interchangeability"
                        }

                        cccTable$addRow(rowKey = comparison_name, list(
                            comparison = comparison_name,
                            subjects = n_cases,
                            ccc = ccc,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            pearson_r = pearson_r,
                            bias_factor = Cb,
                            interpretation = interp
                        ))
                    }
                }

            }, error = function(e) {
                self$results$linCCCTable$setNote(
                    "error",
                    sprintf("Error calculating Lin's CCC: %s", e$message)
                )
            })
        },

        .populateTDIExplanation = function() {
            # Provide educational content about Total Deviation Index

            html <- '<div class="agreement-explanation">'

            # What is TDI?
            html <- paste0(html, '<h3>What is Total Deviation Index (TDI)?</h3>')
            html <- paste0(html, '<p>The <strong>Total Deviation Index (TDI)</strong> quantifies the limits within which ',
                          'a specified proportion (e.g., 90%) of differences between two measurement methods will fall. ',
                          'It provides a single numerical index for assessing whether two methods achieve acceptable agreement ',
                          'based on predefined clinically relevant criteria.</p>')

            # Key Features
            html <- paste0(html, '<h3>Key Features</h3>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Coverage Probability:</strong> Specifies the percentage of measurements that must fall ',
                          'within acceptable limits (typically 90% or 95%)</li>')
            html <- paste0(html, '<li><strong>Acceptable Limit:</strong> User-defined threshold for maximum allowable difference ',
                          'in original measurement units</li>')
            html <- paste0(html, '<li><strong>Regulatory Compliance:</strong> Widely used in medical device validation and method ',
                          'comparison studies required by FDA and other agencies</li>')
            html <- paste0(html, '<li><strong>Interpretability:</strong> Direct comparison to clinical acceptability criteria</li>')
            html <- paste0(html, '</ul>')

            # Interpretation
            html <- paste0(html, '<h3>Interpretation</h3>')
            html <- paste0(html, '<p><strong>Decision Rule:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>If <strong>TDI ≤ Acceptable Limit</strong>: Methods are <span style="color: green;">',
                          '<strong>equivalent/acceptable</strong></span> - the specified proportion of differences ',
                          'falls within acceptable bounds</li>')
            html <- paste0(html, '<li>If <strong>TDI > Acceptable Limit</strong>: Methods are <span style="color: red;">',
                          '<strong>not equivalent</strong></span> - too many differences exceed acceptable bounds</li>')
            html <- paste0(html, '</ul>')

            # When to Use
            html <- paste0(html, '<h3>When to Use TDI</h3>')
            html <- paste0(html, '<p><strong>Essential for:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Medical device validation (manual vs. automated instruments)</li>')
            html <- paste0(html, '<li>Laboratory method comparison (old vs. new assay platforms)</li>')
            html <- paste0(html, '<li>Biomarker assay validation with regulatory requirements</li>')
            html <- paste0(html, '<li>Imaging modality comparison (different scanners, readers, software)</li>')
            html <- paste0(html, '<li>Quality control when acceptable limits are predefined</li>')
            html <- paste0(html, '</ul>')

            # Clinical Examples
            html <- paste0(html, '<h3>Pathology Examples</h3>')
            html <- paste0(html, '<div class="clinical-examples">')

            html <- paste0(html, '<p><strong>Example 1: Tumor Size Measurement</strong></p>')
            html <- paste0(html, '<p><em>Context:</em> Comparing caliper-measured tumor size vs. digital pathology measurements.</p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Acceptable Limit:</strong> 5 mm (clinical decision threshold)</li>')
            html <- paste0(html, '<li><strong>Coverage:</strong> 90% of differences should be ≤5 mm</li>')
            html <- paste0(html, '<li><strong>Result:</strong> If TDI = 3.8 mm → <span style="color: green;">Methods equivalent</span></li>')
            html <- paste0(html, '<li><strong>Result:</strong> If TDI = 6.2 mm → <span style="color: red;">Methods not equivalent</span></li>')
            html <- paste0(html, '<li><strong>Action:</strong> Acceptable agreement allows replacement of manual with digital measurements</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Example 2: Ki-67 Proliferation Index</strong></p>')
            html <- paste0(html, '<p><em>Context:</em> Manual counting vs. automated image analysis for Ki-67% in breast cancer.</p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Acceptable Limit:</strong> 10% absolute difference (clinical cutoff significance)</li>')
            html <- paste0(html, '<li><strong>Coverage:</strong> 95% of differences should be ≤10%</li>')
            html <- paste0(html, '<li><strong>Result:</strong> If TDI = 7.5% → Automated method acceptable for clinical use</li>')
            html <- paste0(html, '<li><strong>Clinical Impact:</strong> Supports FDA 510(k) submission for digital pathology system</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Example 3: Mitotic Count Comparison</strong></p>')
            html <- paste0(html, '<p><em>Context:</em> Two pathologists counting mitotic figures in 10 HPF.</p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Acceptable Limit:</strong> 5 mitoses (affects tumor grade)</li>')
            html <- paste0(html, '<li><strong>Coverage:</strong> 90% of differences should be ≤5</li>')
            html <- paste0(html, '<li><strong>Result:</strong> If TDI = 4.2 → Inter-observer agreement acceptable</li>')
            html <- paste0(html, '<li><strong>Quality Control:</strong> Can be used to certify pathologist performance</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '</div>')

            # Advantages Over Alternatives
            html <- paste0(html, '<h3>Advantages Over Alternative Methods</h3>')
            html <- paste0(html, '<table style="width: 100%; border-collapse: collapse;">')
            html <- paste0(html, '<tr style="background-color: #f0f0f0;">')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">Method</th>')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">Limitation</th>')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">TDI Advantage</th>')
            html <- paste0(html, '</tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">Bland-Altman</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">No direct comparison to acceptable limits</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Single index vs. predefined criterion</td></tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">ICC/CCC</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Unitless, not linked to clinical thresholds</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">In original units, clinically interpretable</td></tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">Correlation (r)</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Measures association, not agreement</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Quantifies actual differences</td></tr>')
            html <- paste0(html, '</table>')

            # Statistical Details
            html <- paste0(html, '<h3>Statistical Details</h3>')
            html <- paste0(html, '<p><strong>Calculation:</strong> TDI represents the radius of a tolerance interval that captures ',
                          'the specified proportion of absolute differences between methods.</p>')
            html <- paste0(html, '<p><strong>Requirements:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Continuous numeric measurements from 2 raters/methods</li>')
            html <- paste0(html, '<li>At least 30 paired observations recommended for stable estimates</li>')
            html <- paste0(html, '<li>Predefined acceptable limit based on clinical significance</li>')
            html <- paste0(html, '</ul>')

            # References
            html <- paste0(html, '<h3>References</h3>')
            html <- paste0(html, '<p style="font-size: 0.9em;">')
            html <- paste0(html, 'Lin L, Hedayat AS, Sinha B, Yang M. Statistical methods in assessing agreement: ',
                          'Models, issues, and tools. <em>J Am Stat Assoc.</em> 2002;97(457):257-270.<br>')
            html <- paste0(html, 'Barnhart HX, Haber M, Song J. Overall concordance correlation coefficient for evaluating ',
                          'agreement among multiple observers. <em>Biometrics.</em> 2002;58(4):1020-1027.<br>')
            html <- paste0(html, 'Carstensen B. Comparing and predicting between several methods of measurement. ',
                          '<em>Biostatistics.</em> 2004;5(3):399-413.')
            html <- paste0(html, '</p>')

            html <- paste0(html, '</div>')

            self$results$tdiExplanation$setContent(html)
        },

        .calculateTDI = function(ratings) {
            # Calculate Total Deviation Index for all pairwise comparisons

            tryCatch({
                # Get user-specified parameters
                coverage <- self$options$tdiCoverage / 100  # Convert to proportion
                acceptable_limit <- self$options$tdiLimit

                # Remove rows with missing values
                complete_rows <- complete.cases(ratings)
                ratings_complete <- ratings[complete_rows, , drop = FALSE]

                n_cases <- nrow(ratings_complete)
                n_raters <- ncol(ratings_complete)

                if (n_cases < 10) {
                    self$results$tdiTable$setNote(
                        "error",
                        sprintf("TDI requires at least 10 complete paired observations. Found %d. Recommend ≥30 for stable estimates.", n_cases)
                    )
                    return()
                }

                if (n_cases < 30) {
                    self$results$tdiTable$setNote(
                        "warning",
                        sprintf("TDI calculated with %d observations. Recommend ≥30 for stable estimates and reliable confidence intervals.", n_cases)
                    )
                }

                if (n_raters < 2) {
                    self$results$tdiTable$setNote(
                        "error",
                        "TDI requires at least 2 raters/methods for comparison."
                    )
                    return()
                }

                # Check that all data is numeric
                is_all_numeric <- all(sapply(ratings_complete, function(x) is.numeric(x) && !is.factor(x)))

                if (!is_all_numeric) {
                    self$results$tdiTable$setNote(
                        "error",
                        "TDI requires continuous numeric data. Categorical data found."
                    )
                    return()
                }

                # Calculate TDI for all pairwise combinations
                tdiTable <- self$results$tdiTable
                rater_names <- colnames(ratings_complete)

                for (i in 1:(n_raters - 1)) {
                    for (j in (i + 1):n_raters) {
                        x <- ratings_complete[[i]]
                        y <- ratings_complete[[j]]

                        comparison_name <- paste(rater_names[i], "vs", rater_names[j])

                        # Calculate differences
                        differences <- x - y

                        # Mean and SD of differences
                        mean_diff <- mean(differences)
                        sd_diff <- sd(differences)

                        # TDI calculation using quantile-based method
                        # TDI is the value such that coverage% of |differences| fall within [-TDI, TDI]

                        # Method 1: Direct quantile of absolute differences
                        abs_diff <- abs(differences)
                        tdi_estimate <- quantile(abs_diff, probs = coverage, names = FALSE)

                        # Method 2: Normal approximation (alternative)
                        # Assumes differences ~ Normal(mean_diff, sd_diff^2)
                        # For two-sided interval: P(-TDI < d < TDI) = coverage
                        # This gives: TDI = z_p * sqrt(sd_diff^2 + mean_diff^2)
                        # where z_p is the quantile for (1 + coverage)/2
                        z_coverage <- qnorm((1 + coverage) / 2)
                        tdi_normal <- z_coverage * sqrt(sd_diff^2 + mean_diff^2)

                        # Use empirical quantile as primary estimate (more robust)
                        # but provide normal-based CI

                        # Bootstrap confidence interval for TDI
                        n_boot <- self$options$nBoot
                        boot_tdi <- numeric(n_boot)

                        for (b in 1:n_boot) {
                            boot_idx <- sample(1:n_cases, replace = TRUE)
                            boot_diff <- differences[boot_idx]
                            boot_abs <- abs(boot_diff)
                            boot_tdi[b] <- quantile(boot_abs, probs = coverage, names = FALSE)
                        }

                        ci_lower <- quantile(boot_tdi, probs = 0.025, names = FALSE)
                        ci_upper <- quantile(boot_tdi, probs = 0.975, names = FALSE)

                        # Determine if methods meet acceptability criteria
                        meets_criteria <- tdi_estimate <= acceptable_limit
                        acceptable_text <- ifelse(meets_criteria,
                                                 " Yes (Acceptable)",
                                                 " No (Not Acceptable)")

                        # Interpretation
                        if (meets_criteria) {
                            if (ci_upper <= acceptable_limit) {
                                interp <- sprintf("Methods are equivalent - %.0f%% of differences ≤%.1f (limit: %.1f). Upper CI also acceptable.",
                                                 coverage * 100, tdi_estimate, acceptable_limit)
                            } else {
                                interp <- sprintf("Methods likely equivalent - TDI=%.1f ≤ limit (%.1f), but upper CI (%.1f) exceeds limit. Consider more data.",
                                                 tdi_estimate, acceptable_limit, ci_upper)
                            }
                        } else {
                            margin <- tdi_estimate - acceptable_limit
                            pct_over <- if (acceptable_limit > 0) (margin / acceptable_limit) * 100 else NA
                            if (!is.na(pct_over)) {
                                interp <- sprintf("Methods NOT equivalent - TDI=%.1f exceeds acceptable limit (%.1f) by %.1f (%.0f%%). Unacceptable disagreement.",
                                                 tdi_estimate, acceptable_limit, margin, pct_over)
                            } else {
                                interp <- sprintf("Methods NOT equivalent - TDI=%.1f exceeds acceptable limit (%.1f) by %.1f. Unacceptable disagreement.",
                                                 tdi_estimate, acceptable_limit, margin)
                            }
                        }

                        tdiTable$addRow(rowKey = comparison_name, list(
                            comparison = comparison_name,
                            subjects = n_cases,
                            tdi_estimate = tdi_estimate,
                            coverage_prob = coverage,
                            acceptable_limit = acceptable_limit,
                            meets_criteria = acceptable_text,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            interpretation = interp
                        ))
                    }
                }

            }, error = function(e) {
                self$results$tdiTable$setNote(
                    "error",
                    sprintf("Error calculating TDI: %s", e$message)
                )
            })
        },

        .populateSpecificAgreementExplanation = function() {
            # Provide educational content about Specific Agreement Indices

            html <- '<div class="agreement-explanation">'

            # What are Specific Agreement Indices?
            html <- paste0(html, '<h3>What are Specific Agreement Indices?</h3>')
            html <- paste0(html, '<p><strong>Specific Agreement Indices</strong> measure agreement for individual categories ',
                          'rather than overall agreement across all categories. For binary data, this includes:</p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Positive Specific Agreement (PSA):</strong> Agreement on positive cases - ',
                          'proportion of cases rated positive by one rater that are also rated positive by another</li>')
            html <- paste0(html, '<li><strong>Negative Specific Agreement (NSA):</strong> Agreement on negative cases - ',
                          'proportion of cases rated negative by one rater that are also rated negative by another</li>')
            html <- paste0(html, '</ul>')
            html <- paste0(html, '<p>For multi-category data, calculates agreement index for each category separately.</p>')

            # Why Use Specific Agreement?
            html <- paste0(html, '<h3>Why Use Specific Agreement?</h3>')
            html <- paste0(html, '<p><strong>Limitations of Overall Kappa:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Treats all disagreements equally (benign→malignant = malignant→benign)</li>')
            html <- paste0(html, '<li>Cannot identify which specific categories have poor agreement</li>')
            html <- paste0(html, '<li>May mask problems with critical categories (e.g., cancer diagnosis)</li>')
            html <- paste0(html, '<li>Affected by category prevalence (low prevalence categories contribute less)</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Advantages of Specific Agreement:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Focus on clinically critical categories (cancer, adverse events)</li>')
            html <- paste0(html, '<li>Identify asymmetric errors (e.g., over-diagnosis vs. under-diagnosis)</li>')
            html <- paste0(html, '<li>Guide targeted training for problematic categories</li>')
            html <- paste0(html, '<li>Essential for diagnostic test validation</li>')
            html <- paste0(html, '</ul>')

            # Interpretation
            html <- paste0(html, '<h3>Interpretation</h3>')
            html <- paste0(html, '<p><strong>Specific Agreement Values:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>>0.90:</strong> <span style="color: green;">Excellent</span> - ',
                          'category has strong agreement, reliable for clinical use</li>')
            html <- paste0(html, '<li><strong>0.75-0.90:</strong> <span style="color: orange;">Good</span> - ',
                          'acceptable agreement, monitor for improvement</li>')
            html <- paste0(html, '<li><strong>0.60-0.75:</strong> <span style="color: orange;">Fair</span> - ',
                          'category needs attention, consider additional training</li>')
            html <- paste0(html, '<li><strong><0.60:</strong> <span style="color: red;">Poor</span> - ',
                          'unreliable for this category, requires intervention</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Asymmetric Agreement:</strong> When PSA ≠ NSA, suggests systematic bias:</p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>PSA > NSA: Better agreement on positive cases (over-diagnosis tendency)</li>')
            html <- paste0(html, '<li>NSA > PSA: Better agreement on negative cases (under-diagnosis tendency)</li>')
            html <- paste0(html, '</ul>')

            # Clinical Examples
            html <- paste0(html, '<h3>Pathology Examples</h3>')
            html <- paste0(html, '<div class="clinical-examples">')

            html <- paste0(html, '<p><strong>Example 1: Cancer Diagnosis</strong></p>')
            html <- paste0(html, '<p><em>Context:</em> Two pathologists diagnosing breast biopsies (Benign vs. Malignant).</p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Overall Kappa:</strong> 0.82 (substantial agreement)</li>')
            html <- paste0(html, '<li><strong>PSA (Malignant):</strong> 0.95 (excellent agreement on cancer cases)</li>')
            html <- paste0(html, '<li><strong>NSA (Benign):</strong> 0.78 (good but lower agreement on benign cases)</li>')
            html <- paste0(html, '<li><strong>Interpretation:</strong> Pathologists are very reliable at identifying cancer but ',
                          'have some inconsistency in benign diagnoses. May benefit from training on benign mimics.</li>')
            html <- paste0(html, '<li><strong>Clinical Impact:</strong> Low PSA would be concerning (missed cancers), but here PSA is excellent.</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Example 2: Tumor Grade Classification</strong></p>')
            html <- paste0(html, '<p><em>Context:</em> Inter-rater reliability for tumor grade (G1, G2, G3).</p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Overall Weighted Kappa:</strong> 0.68 (moderate)</li>')
            html <- paste0(html, '<li><strong>G1 Specific Agreement:</strong> 0.92 (excellent)</li>')
            html <- paste0(html, '<li><strong>G2 Specific Agreement:</strong> 0.52 (poor)</li>')
            html <- paste0(html, '<li><strong>G3 Specific Agreement:</strong> 0.88 (good)</li>')
            html <- paste0(html, '<li><strong>Action:</strong> The low G2 agreement indicates intermediate grade is ambiguous. ',
                          'Consider collapsing into G1+G2 vs. G3, or developing clearer G2 criteria.</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Example 3: Immunostaining Interpretation</strong></p>')
            html <- paste0(html, '<p><em>Context:</em> Ki-67 scoring (Negative, Low, High).</p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Negative Specific Agreement:</strong> 0.95 (excellent)</li>')
            html <- paste0(html, '<li><strong>Low Specific Agreement:</strong> 0.58 (poor)</li>')
            html <- paste0(html, '<li><strong>High Specific Agreement:</strong> 0.89 (good)</li>')
            html <- paste0(html, '<li><strong>Problem:</strong> "Low" category has poor agreement, often confused with Negative or High</li>')
            html <- paste0(html, '<li><strong>Solution:</strong> Consider binary classification (Low/Negative vs. High) or use quantitative cutoffs</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '</div>')

            # When to Use
            html <- paste0(html, '<h3>When to Use Specific Agreement Indices</h3>')
            html <- paste0(html, '<p><strong>Essential for:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Diagnostic test validation where false positives and false negatives have different implications</li>')
            html <- paste0(html, '<li>Cancer diagnosis studies (critical to have high PSA)</li>')
            html <- paste0(html, '<li>Quality control focusing on specific problematic categories</li>')
            html <- paste0(html, '<li>Identifying need for category clarification or combination</li>')
            html <- paste0(html, '<li>Training assessment showing which categories need improvement</li>')
            html <- paste0(html, '<li>Regulatory submissions requiring category-specific reliability</li>')
            html <- paste0(html, '</ul>')

            # Calculation Method
            html <- paste0(html, '<h3>Calculation Method</h3>')
            html <- paste0(html, '<p><strong>For Binary Data (Category = "Positive"):</strong></p>')
            html <- paste0(html, '<p>PSA = 2 × (Both Positive) / (Rater1 Positive + Rater2 Positive)</p>')
            html <- paste0(html, '<p>This represents: Of all cases either rater called positive, what proportion did both raters agree were positive?</p>')

            html <- paste0(html, '<p><strong>For Multi-Category Data:</strong></p>')
            html <- paste0(html, '<p>For each category C:</p>')
            html <- paste0(html, '<p>Specific Agreement(C) = 2 × (Both rated C) / (Rater1 rated C + Rater2 rated C)</p>')

            html <- paste0(html, '<p><strong>Confidence Intervals:</strong> Wilson score method (recommended for proportions)</p>')

            # Comparison Table
            html <- paste0(html, '<h3>Comparison with Overall Metrics</h3>')
            html <- paste0(html, '<table style="width: 100%; border-collapse: collapse;">')
            html <- paste0(html, '<tr style="background-color: #f0f0f0;">')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">Metric</th>')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">What It Measures</th>')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">Best Use</th>')
            html <- paste0(html, '</tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">Overall Kappa</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Agreement across all categories</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">General reliability assessment</td></tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">Specific Agreement</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Agreement within each category</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Identify problematic categories</td></tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">PSA/NSA</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Separate positive/negative agreement</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Detect diagnostic bias</td></tr>')
            html <- paste0(html, '</table>')

            # References
            html <- paste0(html, '<h3>References</h3>')
            html <- paste0(html, '<p style="font-size: 0.9em;">')
            html <- paste0(html, 'Cicchetti DV, Feinstein AR. High agreement but low kappa: II. Resolving the paradoxes. ',
                          '<em>J Clin Epidemiol.</em> 1990;43(6):551-558.<br>')
            html <- paste0(html, 'de Vet HC, Mokkink LB, Terwee CB, Hoekstra OS, Knol DL. Clinicians are right not to like ',
                          'Cohen\'s κ. <em>BMJ.</em> 2013;346:f2125.<br>')
            html <- paste0(html, 'Graham P, Jackson R. The analysis of ordinal agreement data: beyond weighted kappa. ',
                          '<em>J Clin Epidemiol.</em> 1993;46(9):1055-1062.')
            html <- paste0(html, '</p>')

            html <- paste0(html, '</div>')

            self$results$specificAgreementExplanation$setContent(html)
        },

        .calculateSpecificAgreement = function(ratings) {
            # Calculate category-specific agreement indices

            tryCatch({
                # Get user options
                positive_category <- self$options$specificPositiveCategory
                all_categories <- self$options$specificAllCategories
                include_ci <- self$options$specificConfidenceIntervals

                # Remove rows with missing values
                complete_rows <- complete.cases(ratings)
                ratings_complete <- ratings[complete_rows, , drop = FALSE]

                n_cases <- nrow(ratings_complete)
                n_raters <- ncol(ratings_complete)

                if (n_cases < 10) {
                    self$results$specificAgreementTable$setNote(
                        "error",
                        sprintf("Specific agreement requires at least 10 complete observations. Found %d.", n_cases)
                    )
                    return()
                }

                if (n_raters < 2) {
                    self$results$specificAgreementTable$setNote(
                        "error",
                        "Specific agreement requires at least 2 raters for comparison."
                    )
                    return()
                }

                # Check if data is categorical
                is_categorical <- all(sapply(ratings_complete, function(x) is.factor(x) || is.character(x)))

                if (!is_categorical) {
                    self$results$specificAgreementTable$setNote(
                        "error",
                        "Specific agreement requires categorical data. Use TDI or Lin's CCC for continuous data."
                    )
                    return()
                }

                # Get all unique categories across all raters
                all_levels <- unique(unlist(lapply(ratings_complete, function(x) {
                    if (is.factor(x)) levels(x) else unique(as.character(x))
                })))

                # Determine which categories to analyze
                if (all_categories) {
                    categories_to_analyze <- all_levels
                } else if (nzchar(positive_category)) {
                    if (!(positive_category %in% all_levels)) {
                        self$results$specificAgreementTable$setNote(
                            "error",
                            sprintf("Specified positive category '%s' not found in data. Available: %s",
                                   positive_category, paste(all_levels, collapse=", "))
                        )
                        return()
                    }
                    categories_to_analyze <- c(positive_category)
                } else {
                    # Default: analyze all categories
                    categories_to_analyze <- all_levels
                }

                # Calculate for all pairwise combinations
                saTable <- self$results$specificAgreementTable
                rater_names <- colnames(ratings_complete)

                for (i in 1:(n_raters - 1)) {
                    for (j in (i + 1):n_raters) {
                        rater1 <- as.character(ratings_complete[[i]])
                        rater2 <- as.character(ratings_complete[[j]])

                        pair_name <- paste(rater_names[i], "vs", rater_names[j])

                        # Calculate for each category
                        for (category in categories_to_analyze) {
                            # Count cases where both raters chose this category
                            both_positive <- sum(rater1 == category & rater2 == category)

                            # Count total cases where at least one rater chose this category
                            rater1_positive <- sum(rater1 == category)
                            rater2_positive <- sum(rater2 == category)
                            total_positive <- rater1_positive + rater2_positive

                            if (total_positive == 0) {
                                # Category not used by either rater
                                next
                            }

                            # Specific Agreement = 2 * (both positive) / (rater1 positive + rater2 positive)
                            specific_agreement <- (2 * both_positive) / total_positive

                            # Approximate CI with Wilson interval on specific agreement scale
                            # Use number of subjects with at least one positive rating as N
                            if (include_ci) {
                                n_eff <- sum(rater1 == category | rater2 == category)
                                if (n_eff < 1) {
                                    ci_lower <- NA
                                    ci_upper <- NA
                                } else {
                                    p_hat <- specific_agreement
                                    z <- qnorm(0.975)  # 95% CI
                                    denom <- 1 + z^2 / n_eff
                                    center <- (p_hat + z^2 / (2 * n_eff)) / denom
                                    margin <- z * sqrt(p_hat * (1 - p_hat) / n_eff + z^2 / (4 * n_eff^2)) / denom
                                    ci_lower <- max(0, center - margin)
                                    ci_upper <- min(1, center + margin)
                                }
                            } else {
                                ci_lower <- NA
                                ci_upper <- NA
                            }

                            # Interpretation
                            if (specific_agreement >= 0.90) {
                                interp <- "Excellent agreement for this category"
                            } else if (specific_agreement >= 0.75) {
                                interp <- "Good agreement for this category"
                            } else if (specific_agreement >= 0.60) {
                                interp <- "Fair agreement - category needs attention"
                            } else {
                                interp <- "Poor agreement - unreliable for this category"
                            }

                            # Add note for binary PSA/NSA interpretation
                            if (nzchar(positive_category) && category == positive_category) {
                                type_label <- sprintf("%s (Positive)", category)
                                if (specific_agreement < 0.80) {
                                    interp <- paste(interp, "- Critical: Low PSA indicates inconsistent positive diagnosis")
                                }
                            } else if (nzchar(positive_category) && category != positive_category) {
                                type_label <- sprintf("%s (Negative)", category)
                            } else {
                                type_label <- category
                            }

                            saTable$addRow(rowKey = paste(pair_name, category, sep="_"), list(
                                category = type_label,
                                rater_pair = pair_name,
                                n_both_positive = both_positive,
                                n_total_positive = total_positive,
                                specific_agreement = specific_agreement,
                                ci_lower = ci_lower,
                                ci_upper = ci_upper,
                                interpretation = interp
                            ))
                        }
                    }
                }

                # Add summary note if PSA/NSA both calculated
                if (nzchar(positive_category) && length(categories_to_analyze) >= 2) {
                    self$results$specificAgreementTable$setNote(
                        "info",
                        sprintf("PSA (Positive Specific Agreement) for '%s' and NSA (Negative Specific Agreement) for other categories. Large PSA-NSA differences suggest systematic diagnostic bias.",
                               positive_category)
                    )
                }

            }, error = function(e) {
                self$results$specificAgreementTable$setNote(
                    "error",
                    sprintf("Error calculating specific agreement: %s", e$message)
                )
            })
        },

        .populateAgreementHeatmapExplanation = function() {
            # Provide educational content about Agreement Heatmap

            html <- '<div class="agreement-explanation">'

            # What is Agreement Heatmap?
            html <- paste0(html, '<h3>What is an Agreement Heatmap?</h3>')
            html <- paste0(html, '<p>An <strong>Agreement Heatmap</strong> is a visual confusion matrix that displays how two raters\' ',
                          'classifications correspond using color-coded cells. The diagonal cells show perfect agreement ',
                          '(both raters assigned the same category), while off-diagonal cells reveal disagreements and show ',
                          'which specific category confusions occur most frequently.</p>')

            # Key Features
            html <- paste0(html, '<h3>Key Features</h3>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Diagonal Cells:</strong> Perfect agreement - both raters chose the same category</li>')
            html <- paste0(html, '<li><strong>Off-Diagonal Cells:</strong> Disagreements - shows which categories are confused</li>')
            html <- paste0(html, '<li><strong>Color Intensity:</strong> Darker colors indicate higher frequency/percentage</li>')
            html <- paste0(html, '<li><strong>Cell Annotations:</strong> Shows counts and/or percentages for each cell</li>')
            html <- paste0(html, '<li><strong>Row/Column Labels:</strong> Rater 1 (rows) vs. Rater 2 (columns) categories</li>')
            html <- paste0(html, '</ul>')

            # How to Read the Heatmap
            html <- paste0(html, '<h3>How to Read the Heatmap</h3>')
            html <- paste0(html, '<p><strong>Perfect Agreement Scenario:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>All/most cases on the diagonal (same category by both raters)</li>')
            html <- paste0(html, '<li>Off-diagonal cells nearly empty or very light colored</li>')
            html <- paste0(html, '<li>Interpretation: Strong agreement, reliable classification</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Systematic Bias Patterns:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li><strong>Upper Triangle Dark:</strong> Rater 2 consistently rates higher categories than Rater 1</li>')
            html <- paste0(html, '<li><strong>Lower Triangle Dark:</strong> Rater 1 consistently rates higher categories than Rater 2</li>')
            html <- paste0(html, '<li><strong>Specific Cell Dark:</strong> Common confusion between two specific categories</li>')
            html <- paste0(html, '<li><strong>Scattered Pattern:</strong> Random/inconsistent disagreement, needs training</li>')
            html <- paste0(html, '</ul>')

            # Clinical Examples
            html <- paste0(html, '<h3>Pathology Examples</h3>')
            html <- paste0(html, '<div class="clinical-examples">')

            html <- paste0(html, '<p><strong>Example 1: Tumor Grade (G1/G2/G3)</strong></p>')
            html <- paste0(html, '<p><em>Scenario:</em> Two pathologists grading invasive breast carcinoma.</p>')
            html <- paste0(html, '<p><strong>Heatmap Pattern Observed:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>G1 diagonal: Dark (strong agreement on well-differentiated)</li>')
            html <- paste0(html, '<li>G3 diagonal: Dark (strong agreement on poorly-differentiated)</li>')
            html <- paste0(html, '<li>G2 diagonal: Light (poor agreement on moderately-differentiated)</li>')
            html <- paste0(html, '<li>G2G1 and G2G3 cells: Moderately dark (G2 confused with both)</li>')
            html <- paste0(html, '</ul>')
            html <- paste0(html, '<p><strong>Interpretation:</strong> Raters agree on extreme grades but have difficulty ',
                          'with intermediate grade 2. The G2 category is ambiguous and problematic.</p>')
            html <- paste0(html, '<p><strong>Action:</strong> Consider collapsing G1+G2 vs. G3, or develop clearer G2 criteria.</p>')

            html <- paste0(html, '<p><strong>Example 2: Dysplasia Grading (Negative/Low/High)</strong></p>')
            html <- paste0(html, '<p><em>Scenario:</em> Assessing agreement on Barrett\'s esophagus dysplasia.</p>')
            html <- paste0(html, '<p><strong>Heatmap Pattern Observed:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Upper triangle darker than lower triangle</li>')
            html <- paste0(html, '<li>Specifically: Negative→Low cell dark (Rater 2 upgrades many cases)</li>')
            html <- paste0(html, '<li>Low→High cell: Minimal</li>')
            html <- paste0(html, '</ul>')
            html <- paste0(html, '<p><strong>Interpretation:</strong> Rater 2 is systematically more aggressive, ',
                          'calling low-grade dysplasia when Rater 1 calls negative. Classic over-diagnosis pattern.</p>')
            html <- paste0(html, '<p><strong>Action:</strong> Review criteria for negative vs. low-grade with Rater 2, ',
                          'focus training on borderline cases.</p>')

            html <- paste0(html, '<p><strong>Example 3: Immunostaining (Negative/1+/2+/3+)</strong></p>')
            html <- paste0(html, '<p><em>Scenario:</em> HER2 scoring in breast cancer (4-tier system).</p>')
            html <- paste0(html, '<p><strong>Heatmap Pattern Observed:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>0 diagonal: Very dark (excellent agreement on negative cases)</li>')
            html <- paste0(html, '<li>3+ diagonal: Very dark (excellent agreement on strong positive)</li>')
            html <- paste0(html, '<li>1+ and 2+ diagonal: Light (poor agreement on intermediate categories)</li>')
            html <- paste0(html, '<li>2+1+ and 2+3+ cells: Both dark (2+ confused with neighbors)</li>')
            html <- paste0(html, '</ul>')
            html <- paste0(html, '<p><strong>Interpretation:</strong> Excellent agreement on clinically critical categories ',
                          '(0 = no treatment, 3+ = treatment), but poor agreement on equivocal 2+ which requires FISH testing.</p>')
            html <- paste0(html, '<p><strong>Clinical Impact:</strong> This is acceptable since 2+ cases undergo reflex FISH anyway. ',
                          'The important reliability is distinguishing clear negative (0) from clear positive (3+).</p>')

            html <- paste0(html, '</div>')

            # When to Use
            html <- paste0(html, '<h3>When to Use Agreement Heatmap</h3>')
            html <- paste0(html, '<p><strong>Essential for:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Identifying which specific categories have poor agreement</li>')
            html <- paste0(html, '<li>Detecting systematic bias (one rater consistently over/under-grades)</li>')
            html <- paste0(html, '<li>Discovering common category confusions for targeted training</li>')
            html <- paste0(html, '<li>Multi-category classifications (3+ categories) where patterns matter</li>')
            html <- paste0(html, '<li>Quality control reports showing visual evidence of agreement issues</li>')
            html <- paste0(html, '<li>Presentations and publications requiring intuitive visualization</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Advantages over numeric metrics:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Immediately shows which specific disagreements are most common</li>')
            html <- paste0(html, '<li>Reveals patterns that overall kappa values hide</li>')
            html <- paste0(html, '<li>Intuitive for non-statisticians to understand</li>')
            html <- paste0(html, '<li>Guides targeted interventions for problematic categories</li>')
            html <- paste0(html, '</ul>')

            # Color Scheme Guide
            html <- paste0(html, '<h3>Choosing Color Schemes</h3>')
            html <- paste0(html, '<table style="width: 100%; border-collapse: collapse;">')
            html <- paste0(html, '<tr style="background-color: #f0f0f0;">')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">Color Scheme</th>')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">Best Use</th>')
            html <- paste0(html, '<th style="border: 1px solid #ddd; padding: 8px;">Considerations</th>')
            html <- paste0(html, '</tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">Blue-Red Diverging</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Default, emphasizes agreement (diagonal)</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Strong visual contrast, easy pattern recognition</td></tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">Traffic Light</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Presentations, intuitive interpretation</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Green=agree, Yellow=moderate, Red=disagree</td></tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">Viridis</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Colorblind-safe, publications</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Perceptually uniform, accessible</td></tr>')
            html <- paste0(html, '<tr><td style="border: 1px solid #ddd; padding: 8px;">Grayscale</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">Black-and-white printing</td>')
            html <- paste0(html, '<td style="border: 1px solid #ddd; padding: 8px;">No color required, cost-effective</td></tr>')
            html <- paste0(html, '</table>')

            # Technical Details
            html <- paste0(html, '<h3>Technical Details</h3>')
            html <- paste0(html, '<p><strong>Matrix Construction:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Rows represent Rater 1\'s classifications</li>')
            html <- paste0(html, '<li>Columns represent Rater 2\'s classifications</li>')
            html <- paste0(html, '<li>Cell (i,j) = count/percentage of cases where Rater 1 chose category i AND Rater 2 chose category j</li>')
            html <- paste0(html, '<li>Multiple rater pairs create multiple heatmaps for comparison</li>')
            html <- paste0(html, '</ul>')

            html <- paste0(html, '<p><strong>Interpretation Tips:</strong></p>')
            html <- paste0(html, '<ul>')
            html <- paste0(html, '<li>Focus on diagonal vs. off-diagonal intensity ratio</li>')
            html <- paste0(html, '<li>Look for asymmetry between upper and lower triangles</li>')
            html <- paste0(html, '<li>Identify specific "hot spots" of frequent disagreement</li>')
            html <- paste0(html, '<li>Compare patterns across multiple rater pairs if available</li>')
            html <- paste0(html, '</ul>')

            # References
            html <- paste0(html, '<h3>References</h3>')
            html <- paste0(html, '<p style="font-size: 0.9em;">')
            html <- paste0(html, 'Friendly M. Visualizing Categorical Data. <em>SAS Institute.</em> 2000.<br>')
            html <- paste0(html, 'Warrens MJ. Understanding the Cohen-Fleiss paradox in the context of ',
                          'disagreement. <em>J Classif.</em> 2020;37:648-661.<br>')
            html <- paste0(html, 'Wilkinson L, Friendly M. The history of the cluster heat map. ',
                          '<em>Am Stat.</em> 2009;63(2):179-184.')
            html <- paste0(html, '</p>')

            html <- paste0(html, '</div>')

            self$results$agreementHeatmapExplanation$setContent(html)
        },

        .populateAgreementHeatmap = function(ratings) {
            # Prepare data for agreement heatmap visualization

            tryCatch({
                # Remove rows with missing values
                complete_rows <- complete.cases(ratings)
                ratings_complete <- ratings[complete_rows, , drop = FALSE]

                n_cases <- nrow(ratings_complete)
                n_raters <- ncol(ratings_complete)

                if (n_cases < 10) {
                    self$results$agreementHeatmapPlot$setError("Agreement heatmap requires at least 10 complete observations.")
                    return()
                }

                if (n_raters < 2) {
                    self$results$agreementHeatmapPlot$setError("Agreement heatmap requires at least 2 raters.")
                    return()
                }

                # Check if data is categorical
                is_categorical <- all(sapply(ratings_complete, function(x) is.factor(x) || is.character(x)))

                if (!is_categorical) {
                    self$results$agreementHeatmapPlot$setError("Agreement heatmap requires categorical data. Use Bland-Altman plot for continuous data.")
                    return()
                }

                # Convert to character for consistency and prepare as list
                ratings_list <- lapply(ratings_complete, as.character)
                rater_names <- names(ratings_complete)

                # Get user options
                color_scheme <- self$options$heatmapColorScheme
                show_pct <- self$options$heatmapShowPercentages
                show_count <- self$options$heatmapShowCounts
                annot_size <- self$options$heatmapAnnotationSize

                # Store state for rendering
                plot <- self$results$agreementHeatmapPlot
                plot$setState(list(
                    ratings_list = ratings_list,
                    rater_names = rater_names,
                    color_scheme = color_scheme,
                    show_pct = show_pct,
                    show_count = show_count,
                    annot_size = annot_size
                ))

            }, error = function(e) {
                self$results$agreementHeatmapPlot$setError(sprintf("Error generating heatmap: %s", e$message))
            })
        },

        # ============================================================
        # RATER PROFILE PLOTS
        # ============================================================

        .populateRaterProfileExplanation = function() {
            # Provide educational content about Rater Profile Plots

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What are Rater Profile Plots?</h3>
                    <p style='margin: 0; color: #333;'>
                        Rater Profile Plots visualize the <strong>distribution of ratings</strong> for each individual rater.
                        Unlike agreement statistics that focus on concordance between raters, profile plots reveal
                        <strong>rating patterns and systematic differences</strong> in how raters use the rating scale.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Rater Profile Plots</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential when:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Agreement is low</strong> – Determine if disagreement stems from systematic rating differences vs. random variation</li>
                        <li><strong>Training evaluation</strong> – Identify raters with restricted range use or systematic over/under-grading</li>
                        <li><strong>Quality control</strong> – Detect raters who need recalibration or additional training</li>
                        <li><strong>Method validation</strong> – Ensure all raters are using the full scale appropriately</li>
                        <li><strong>Identifying outlier raters</strong> – Find raters whose distributions differ markedly from the group</li>
                    </ul>
                </div>

                <h4 style='color: #333;'>Plot Types and When to Use Them</h4>
                <table style='width: 100%; border-collapse: collapse; margin-bottom: 20px; border: 1px solid #ccc;'>
                    <tr style='background: #f5f5f5;'>
                        <th style='padding: 8px; border: 1px solid #ccc; text-align: left;'>Plot Type</th>
                        <th style='padding: 8px; border: 1px solid #ccc; text-align: left;'>Best For</th>
                        <th style='padding: 8px; border: 1px solid #ccc; text-align: left;'>Shows</th>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ccc;'><strong>Box Plot</strong></td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Continuous data, comparing medians</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Median, IQR, outliers, range</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ccc;'><strong>Violin Plot</strong></td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Continuous data, full distribution shape</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Distribution density, bimodality, skewness</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ccc;'><strong>Bar Plot</strong></td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Categorical/ordinal data</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Category frequencies, scale use patterns</td>
                    </tr>
                </table>

                <h4 style='color: #333;'>Clinical Use Cases in Pathology</h4>

                <div style='background: #f9f9f9; padding: 15px; margin-bottom: 15px; border-left: 3px solid #333;'>
                    <h5 style='margin: 0 0 10px 0; color: #333;'>1. Tumor Grade Assessment (Ordinal Data)</h5>
                    <p style='margin: 0 0 10px 0; color: #666;'>
                        <strong>Scenario:</strong> Five pathologists grade 100 breast tumors as G1/G2/G3.
                    </p>
                    <p style='margin: 0 0 10px 0; color: #666;'>
                        <strong>Bar Plot Reveals:</strong>
                    </p>
                    <ul style='margin: 0; padding-left: 20px; color: #666;'>
                        <li>Path A: 40% G1, 50% G2, 10% G3 (under-grades G3)</li>
                        <li>Path B: 15% G1, 40% G2, 45% G3 (over-grades G3)</li>
                        <li>Path C: 25% G1, 50% G2, 25% G3 (balanced distribution)</li>
                        <li>Path D: 10% G1, 80% G2, 10% G3 (restricted range - G2 overused)</li>
                        <li>Path E: 30% G1, 40% G2, 30% G3 (balanced)</li>
                    </ul>
                    <p style='margin: 10px 0 0 0; color: #666;'>
                        <strong>Interpretation:</strong> Path A and B show opposite systematic biases.
                        Path D shows central tendency bias (avoiding extreme grades). Targeted training
                        can address these specific patterns.
                    </p>
                </div>

                <div style='background: #f9f9f9; padding: 15px; margin-bottom: 15px; border-left: 3px solid #333;'>
                    <h5 style='margin: 0 0 10px 0; color: #333;'>2. Ki-67 Proliferation Index (Continuous Data)</h5>
                    <p style='margin: 0 0 10px 0; color: #666;'>
                        <strong>Scenario:</strong> Four pathologists score Ki-67 (0-100%) on 80 breast cancers.
                    </p>
                    <p style='margin: 0 0 10px 0; color: #666;'>
                        <strong>Box Plot Reveals:</strong>
                    </p>
                    <ul style='margin: 0; padding-left: 20px; color: #666;'>
                        <li>Scorer 1: Median 25%, IQR 15-40%, range 5-75%</li>
                        <li>Scorer 2: Median 35%, IQR 25-50%, range 10-85% (systematically higher by ~10%)</li>
                        <li>Scorer 3: Median 30%, IQR 20-45%, range 15-60% (restricted range, no extremes)</li>
                        <li>Scorer 4: Median 28%, IQR 18-42%, range 2-95% (full range use)</li>
                    </ul>
                    <p style='margin: 10px 0 0 0; color: #666;'>
                        <strong>Interpretation:</strong> Scorer 2 systematically over-estimates by ~10% (needs recalibration).
                        Scorer 3 avoids extreme values (central tendency bias). Violin plots would additionally show
                        if distributions are unimodal or have multiple peaks.
                    </p>
                </div>

                <div style='background: #f9f9f9; padding: 15px; margin-bottom: 15px; border-left: 3px solid #333;'>
                    <h5 style='margin: 0 0 10px 0; color: #333;'>3. Mitotic Count (Continuous with Outliers)</h5>
                    <p style='margin: 0 0 10px 0; color: #666;'>
                        <strong>Scenario:</strong> Six pathologists count mitoses per 10 HPF in 60 sarcomas.
                    </p>
                    <p style='margin: 0 0 10px 0; color: #666;'>
                        <strong>Violin Plot Reveals:</strong>
                    </p>
                    <ul style='margin: 0; padding-left: 20px; color: #666;'>
                        <li>Path A-E: Unimodal distributions, median ~8 mitoses, similar spread</li>
                        <li>Path F: <strong>Bimodal distribution</strong> with peaks at 5 and 15 mitoses</li>
                    </ul>
                    <p style='margin: 10px 0 0 0; color: #666;'>
                        <strong>Interpretation:</strong> Path F's bimodal distribution suggests inconsistent counting
                        methodology (perhaps using different field sizes or magnifications for different cases).
                        Requires investigation and retraining.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'> Important Interpretation Notes</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Distribution differences ≠ poor agreement:</strong> Raters can have different distributions
                            but still agree well on relative rankings</li>
                        <li><strong>Context matters:</strong> Some distribution differences may reflect genuine expertise
                            (expert vs. trainee) rather than error</li>
                        <li><strong>Sample size:</strong> Need ≥30 cases per rater for reliable distribution estimates</li>
                        <li><strong>Outliers:</strong> Investigate outliers individually - may represent genuine biological extremes
                            or measurement errors</li>
                        <li><strong>Central tendency bias:</strong> Restricted range (avoiding extremes) is common in trainees
                            and reduces diagnostic discrimination</li>
                    </ul>
                </div>

                <h4 style='color: #333;'>What to Do Based on Profile Patterns</h4>
                <table style='width: 100%; border-collapse: collapse; margin-bottom: 20px; border: 1px solid #ccc;'>
                    <tr style='background: #f5f5f5;'>
                        <th style='padding: 8px; border: 1px solid #ccc; text-align: left;'>Pattern</th>
                        <th style='padding: 8px; border: 1px solid #ccc; text-align: left;'>Interpretation</th>
                        <th style='padding: 8px; border: 1px solid #ccc; text-align: left;'>Action</th>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Systematic shift (higher/lower median)</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Over/under-grading bias</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Recalibration with reference standards</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Restricted range (narrow IQR)</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Central tendency bias, avoiding extremes</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Training on diagnostic thresholds</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Bimodal distribution</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Inconsistent methodology</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Standardize measurement protocol</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Excessive outliers</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Measurement errors or misclassifications</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Review outlier cases individually</td>
                    </tr>
                    <tr>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Similar distributions across all raters</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>Good scale use calibration</td>
                        <td style='padding: 8px; border: 1px solid #ccc;'>No action needed (assess agreement separately)</td>
                    </tr>
                </table>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'> Best Practices</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Use alongside agreement statistics (kappa/ICC) - not a replacement</li>
                        <li>Violin plots better than box plots for detecting bimodality and distribution shape</li>
                        <li>Show individual points for small datasets (N < 50 per rater)</li>
                        <li>Compare profile plots before and after training to assess improvement</li>
                        <li>For categorical data, look for overuse of middle categories (central tendency)</li>
                    </ul>
                </div>
            </div>
            "

            html <- jmvcore::format(html_content)
            self$results$raterProfileExplanation$setContent(html)
        },

        .raterProfilePlot = function(image, ggtheme, theme, ...) {
            # Render rater profile plot from stored state

            plotState <- image$state

            if (is.null(plotState)) {
                return(FALSE)
            }

            # Extract data from state
            plot_data <- plotState$plot_data
            plot_type <- plotState$plot_type
            is_categorical <- plotState$is_categorical
            show_points <- plotState$show_points

            # Create plot based on type
            if (is_categorical || plot_type == "barplot") {
                # Bar plot for categorical data
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Rater, fill = Category)) +
                    ggplot2::geom_bar(position = "fill", stat = "count") +
                    ggplot2::scale_y_continuous(labels = scales::percent) +
                    ggplot2::labs(
                        title = "Rater Profile: Category Distribution",
                        subtitle = "Proportion of each category assigned by each rater",
                        x = "Rater",
                        y = "Percentage",
                        fill = "Category"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5),
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                    )
            } else if (plot_type == "violin") {
                # Violin plot for continuous data
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Rater, y = Rating)) +
                    ggplot2::geom_violin(fill = "#2196F3", alpha = 0.6) +
                    ggplot2::geom_boxplot(width = 0.1, outlier.shape = NA) +
                    ggplot2::labs(
                        title = "Rater Profile: Rating Distribution",
                        subtitle = "Violin plots showing full distribution shape for each rater",
                        x = "Rater",
                        y = "Rating"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5),
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                    )

                if (show_points) {
                    p <- p + ggplot2::geom_jitter(alpha = 0.3, width = 0.2, size = 0.5)
                }
            } else {
                # Box plot for continuous data (default)
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Rater, y = Rating)) +
                    ggplot2::geom_boxplot(fill = "#2196F3", alpha = 0.6) +
                    ggplot2::labs(
                        title = "Rater Profile: Rating Distribution",
                        subtitle = "Box plots showing median, IQR, and outliers for each rater",
                        x = "Rater",
                        y = "Rating"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5),
                        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                    )

                if (show_points) {
                    p <- p + ggplot2::geom_jitter(alpha = 0.3, width = 0.2, size = 0.5)
                }
            }

            print(p)
            return(TRUE)
        },

        .populateRaterProfiles = function(ratings) {
            # Prepare data for rater profile plots

            tryCatch({
                # Get user options
                plot_type <- self$options$raterProfileType
                show_points <- self$options$raterProfileShowPoints

                # Check if data is categorical
                is_categorical <- all(sapply(ratings, function(x) is.factor(x) || is.character(x)))

                # Prepare data in long format
                rater_names <- names(ratings)
                n_raters <- length(rater_names)
                n_cases <- nrow(ratings)

                if (is_categorical) {
                    # For categorical data, create long format with counts
                    plot_data <- data.frame()
                    for (i in 1:n_raters) {
                        rater_data <- data.frame(
                            Rater = rater_names[i],
                            Category = as.character(ratings[[i]]),
                            stringsAsFactors = FALSE
                        )
                        plot_data <- rbind(plot_data, rater_data)
                    }
                    plot_data$Rater <- factor(plot_data$Rater, levels = rater_names)
                } else {
                    # For continuous data, create long format
                    plot_data <- data.frame()
                    for (i in 1:n_raters) {
                        rater_data <- data.frame(
                            Rater = rater_names[i],
                            Rating = as.numeric(ratings[[i]]),
                            stringsAsFactors = FALSE
                        )
                        plot_data <- rbind(plot_data, rater_data)
                    }
                    plot_data$Rater <- factor(plot_data$Rater, levels = rater_names)
                    plot_data <- plot_data[!is.na(plot_data$Rating), ]
                }

                # Store state for rendering
                plot <- self$results$raterProfilePlot
                plot$setState(list(
                    plot_data = plot_data,
                    plot_type = plot_type,
                    is_categorical = is_categorical,
                    show_points = show_points
                ))

            }, error = function(e) {
                self$results$raterProfilePlot$setError(sprintf("Error generating rater profile plot: %s", e$message))
            })
        },

        # ============================================================
        # AGREEMENT BY SUBGROUP
        # ============================================================

        .populateSubgroupExplanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>Agreement by Subgroup (Stratified Analysis)</h3>
                    <p style='margin: 0; color: #333;'>Calculate agreement separately for each subgroup to identify which case types show reliable agreement.</p>
                </div>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Use Cases</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Compare agreement across tumor types (benign vs. malignant)</li>
                        <li>Assess agreement by disease stage or grade</li>
                        <li>Identify case difficulty (easy vs. hard cases)</li>
                        <li>Evaluate agreement across anatomical sites</li>
                    </ul>
                </div></div>"
            self$results$subgroupExplanation$setContent(jmvcore::format(html))
        },

        .subgroupForestPlot = function(image, ggtheme, theme, ...) {
            plotState <- image$state
            if (is.null(plotState)) return(FALSE)

            forest_data <- plotState$forest_data

            # Create forest plot
            p <- ggplot2::ggplot(forest_data, ggplot2::aes(x = agreement, y = subgroup)) +
                ggplot2::geom_point(size = 3) +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
                ggplot2::labs(title = "Agreement by Subgroup (Forest Plot)",
                             subtitle = "Point estimates with 95% confidence intervals",
                             x = "Agreement Statistic",
                             y = "Subgroup") +
                ggtheme +
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                              plot.subtitle = ggplot2::element_text(hjust = 0.5))
            print(p)
            return(TRUE)
        },

        .calculateAgreementBySubgroup = function(ratings) {
            tryCatch({
                subgroup_var <- self$options$subgroupVariable
                min_cases <- self$options$subgroupMinCases

                if (is.null(subgroup_var) || subgroup_var == "") {
                    return()
                }

                # Get subgroup variable from data
                subgroup <- self$data[[subgroup_var]]
                if (is.null(subgroup)) {
                    self$results$subgroupAgreementTable$setNote("error", sprintf("Subgroup variable '%s' not found", subgroup_var))
                    return()
                }

                # Determine if data is categorical
                is_categorical <- all(sapply(ratings, function(x) is.factor(x) || is.character(x)))

                # Calculate agreement for each subgroup
                subgroup_levels <- unique(subgroup[!is.na(subgroup)])
                result_rows <- list()
                forest_data <- data.frame()

                for (level in subgroup_levels) {
                    # Subset data for this subgroup
                    idx <- which(subgroup == level & complete.cases(ratings))

                    if (length(idx) < min_cases) {
                        next  # Skip subgroups with insufficient cases
                    }

                    sub_ratings <- ratings[idx, , drop = FALSE]
                    n_cases <- nrow(sub_ratings)
                    n_raters <- ncol(sub_ratings)
                    agreement_stat <- NA_real_
                    stat_type <- "Not estimable"
                    ci_lower <- NA_real_
                    ci_upper <- NA_real_

                    # Calculate appropriate agreement statistic
                    if (is_categorical) {
                        # Use Cohen's kappa for 2 raters; Fleiss' kappa for 3+ raters
                        if (requireNamespace("irr", quietly = TRUE)) {
                            kappa_result <- tryCatch({
                                if (n_raters == 2) {
                                    irr::kappa2(sub_ratings)
                                } else {
                                    irr::kappam.fleiss(sub_ratings)
                                }
                            }, error = function(e) NULL)
                            if (!is.null(kappa_result)) {
                                agreement_stat <- kappa_result$value
                                stat_type <- if (n_raters == 2) "Cohen's Kappa" else "Fleiss' Kappa"

                                # Approximate CI via z statistic when available
                                z_stat <- kappa_result$statistic
                                if (!is.null(z_stat) && is.finite(z_stat) && abs(z_stat) > 1e-6 && is.finite(agreement_stat)) {
                                    se <- abs(agreement_stat / z_stat)
                                    # Guard: SE should be small relative to kappa range [-1, 1]
                                    if (se < 2) {
                                        ci_lower <- max(-1, agreement_stat - 1.96 * se)
                                        ci_upper <- min(1, agreement_stat + 1.96 * se)
                                    }
                                }
                            } else {
                                agreement_stat <- NA
                                stat_type <- "Kappa (error)"
                                ci_lower <- NA
                                ci_upper <- NA
                            }
                        }
                    } else {
                        # Use ICC for continuous data
                        if (requireNamespace("irr", quietly = TRUE)) {
                            icc_result <- tryCatch(irr::icc(sub_ratings, model = "twoway", type = "agreement"), error = function(e) NULL)
                            if (!is.null(icc_result)) {
                                agreement_stat <- icc_result$value
                                stat_type <- "ICC(2,1)"
                                ci_lower <- icc_result$lbound
                                ci_upper <- icc_result$ubound
                            } else {
                                agreement_stat <- NA
                                stat_type <- "ICC (error)"
                                ci_lower <- NA
                                ci_upper <- NA
                            }
                        }
                    }

                    # Interpret agreement
                    interpretation <- if (is.na(agreement_stat)) {
                        "Error calculating agreement"
                    } else if (agreement_stat < 0.40) {
                        "Poor"
                    } else if (agreement_stat < 0.60) {
                        "Fair"
                    } else if (agreement_stat < 0.75) {
                        "Good"
                    } else if (agreement_stat < 0.90) {
                        "Excellent"
                    } else {
                        "Outstanding"
                    }

                    # Add row to table
                    result_rows[[length(result_rows) + 1]] <- list(
                        subgroup = as.character(level),
                        n_cases = n_cases,
                        n_raters = n_raters,
                        agreement_stat = agreement_stat,
                        stat_type = stat_type,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper,
                        interpretation = interpretation
                    )

                    # Add to forest plot data
                    if (!is.na(agreement_stat)) {
                        forest_data <- rbind(forest_data, data.frame(
                            subgroup = as.character(level),
                            agreement = agreement_stat,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            stringsAsFactors = FALSE
                        ))
                    }
                }

                # Populate table
                table <- self$results$subgroupAgreementTable
                for (row in result_rows) {
                    table$addRow(rowKey = row$subgroup, values = row)
                }

                # Store forest plot state
                if (self$options$subgroupForestPlot && nrow(forest_data) > 0) {
                    forest_data$subgroup <- factor(forest_data$subgroup, levels = rev(forest_data$subgroup))
                    plot <- self$results$subgroupForestPlotImage
                    plot$setState(list(forest_data = forest_data))
                }

            }, error = function(e) {
                self$results$subgroupAgreementTable$setNote("error", sprintf("Error in subgroup analysis: %s", e$message))
            })
        },

        # ============================================================
        # RATER CLUSTERING
        # ============================================================

        .populateRaterClusterExplanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>Rater Clustering</h3>
                    <p style='margin: 0; color: #333;'>Identifies groups of raters with similar rating patterns using hierarchical or k-means clustering.</p>
                </div>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Use Cases</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Identify subgroups of similarly-trained raters</li>
                        <li>Detect outlier raters with unique rating patterns</li>
                        <li>Understand sources of disagreement (within vs. between groups)</li>
                        <li>Optimize panel composition for maximum diversity or homogeneity</li>
                        <li>Target training to specific rater clusters</li>
                    </ul>
                    <h4 style='margin: 10px 0 10px 0; color: #333;'>Interpretation</h4>
                    <p style='margin: 0;'><strong>Dendrogram height:</strong> Lower = more similar. High first split = distinct rater groups.</p>
                    <p style='margin: 5px 0 0 0;'><strong>Cluster heatmap:</strong> Dark diagonal blocks = tight clusters. Off-diagonal = between-cluster differences.</p>
                </div></div>"
            self$results$raterClusterExplanation$setContent(jmvcore::format(html))
        },

        .raterDendrogram = function(image, ggtheme, theme, ...) {
            plotState <- image$state
            if (is.null(plotState)) return(FALSE)

            # Reconstruct hclust object from serialized components
            hc <- structure(list(
                merge = plotState$merge[[1]],
                height = plotState$height[[1]],
                order = plotState$order[[1]],
                labels = plotState$labels[[1]],
                method = as.character(plotState$method),
                dist.method = "euclidean"
            ), class = "hclust")
            cluster_labels <- plotState$cluster_labels[[1]]

            # Set up plot
            par(mar = c(7, 4, 4, 2))

            # Plot dendrogram
            plot(hc, main = "Rater Clustering Dendrogram",
                 xlab = "", ylab = "Distance", sub = "", hang = -1)

            # Add cluster labels if available
            if (!is.null(cluster_labels)) {
                # Color branches by cluster
                rect.hclust(hc, k = length(unique(cluster_labels)), border = 2:6)
            }

            return(TRUE)
        },

        .raterClusterHeatmap = function(image, ggtheme, theme, ...) {
            plotState <- image$state
            if (is.null(plotState)) return(FALSE)

            sim_matrix <- plotState$similarity_matrix
            cluster_assign <- plotState$cluster_assignments
            rater_names <- plotState$rater_names

            # Reorder by cluster
            if (!is.null(cluster_assign)) {
                ord <- order(cluster_assign)
                sim_matrix <- sim_matrix[ord, ord]
                rater_names <- rater_names[ord]
                cluster_assign <- cluster_assign[ord]
            }

            # Create heatmap using base graphics
            n_raters <- nrow(sim_matrix)
            colors <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100)

            par(mar = c(8, 8, 4, 2))
            image(1:n_raters, 1:n_raters, sim_matrix,
                  col = colors,
                  xlab = "", ylab = "",
                  main = "Rater Similarity Matrix (Clustered)",
                  axes = FALSE)

            axis(1, at = 1:n_raters, labels = rater_names, las = 2, cex.axis = 0.8)
            axis(2, at = 1:n_raters, labels = rater_names, las = 1, cex.axis = 0.8)

            # Add cluster boundaries
            if (!is.null(cluster_assign)) {
                cluster_bounds <- which(diff(cluster_assign) != 0)
                for (b in cluster_bounds) {
                    abline(h = b + 0.5, col = "black", lwd = 2)
                    abline(v = b + 0.5, col = "black", lwd = 2)
                }
            }

            return(TRUE)
        },

        .performRaterClustering = function(ratings) {
            tryCatch({
                method <- self$options$clusterMethod
                distance_metric <- self$options$clusterDistance
                linkage <- self$options$clusterLinkage
                k <- self$options$nClusters

                # Check if data is categorical
                is_categorical <- all(sapply(ratings, function(x) is.factor(x) || is.character(x)))

                n_raters <- ncol(ratings)
                rater_names <- names(ratings)

                if (n_raters < 3) {
                    self$results$raterClusterTable$setNote("error", "Clustering requires at least 3 raters")
                    return()
                }

                # Compute distance matrix
                if (is_categorical || distance_metric == "agreement") {
                    # For categorical: use agreement-based distance
                    dist_matrix <- matrix(0, n_raters, n_raters)
                    for (i in 1:(n_raters-1)) {
                        for (j in (i+1):n_raters) {
                            # Calculate pairwise agreement
                            pairs <- complete.cases(ratings[, c(i, j)])
                            if (sum(pairs) > 0) {
                                agreement <- sum(ratings[pairs, i] == ratings[pairs, j]) / sum(pairs)
                                dist_matrix[i, j] <- dist_matrix[j, i] <- 1 - agreement
                            } else {
                                dist_matrix[i, j] <- dist_matrix[j, i] <- 1
                            }
                        }
                    }
                    dist_obj <- as.dist(dist_matrix)
                } else {
                    # For continuous: use correlation or Euclidean
                    if (distance_metric == "correlation") {
                        cor_matrix <- cor(ratings, use = "pairwise.complete.obs")
                        dist_matrix <- 1 - cor_matrix
                        dist_obj <- as.dist(dist_matrix)
                    } else if (distance_metric == "euclidean") {
                        dist_obj <- dist(t(ratings), method = "euclidean")
                        dist_matrix <- as.matrix(dist_obj)
                    } else {  # manhattan
                        dist_obj <- dist(t(ratings), method = "manhattan")
                        dist_matrix <- as.matrix(dist_obj)
                    }
                }

                # Perform clustering
                if (method == "hierarchical") {
                    # Hierarchical clustering
                    linkage_method <- switch(linkage,
                                           "average" = "average",
                                           "complete" = "complete",
                                           "single" = "single",
                                           "ward" = "ward.D2")
                    hc <- hclust(dist_obj, method = linkage_method)

                    # Cut tree to get cluster assignments
                    cluster_assign <- cutree(hc, k = k)

                    # Store dendrogram state (serializable components only)
                    if (self$options$showDendrogram) {
                        dendro <- self$results$raterDendrogram
                        dendro$setState(as.data.frame(list(
                            merge = I(list(hc$merge)),
                            height = I(list(hc$height)),
                            order = I(list(hc$order)),
                            labels = I(list(hc$labels)),
                            method = hc$method,
                            cluster_labels = I(list(cluster_assign))
                        )))
                    }
                } else {
                    # K-means clustering (for continuous data only)
                    if (is_categorical) {
                        self$results$raterClusterTable$setNote("error", "K-means clustering requires continuous data")
                        return()
                    }

                    # Transpose: raters as rows, cases as columns
                    km <- kmeans(t(ratings), centers = k, nstart = 25)
                    cluster_assign <- km$cluster
                }

                # Calculate within-cluster similarities
                similarity_matrix <- 1 - dist_matrix
                cluster_similarities <- numeric(n_raters)

                for (i in 1:n_raters) {
                    # Mean similarity to other members of same cluster
                    same_cluster <- which(cluster_assign == cluster_assign[i] & 1:n_raters != i)
                    if (length(same_cluster) > 0) {
                        cluster_similarities[i] <- mean(similarity_matrix[i, same_cluster])
                    } else {
                        cluster_similarities[i] <- NA
                    }
                }

                # Populate table
                table <- self$results$raterClusterTable
                cluster_sizes <- table(cluster_assign)

                for (i in 1:n_raters) {
                    table$addRow(rowKey = rater_names[i], values = list(
                        rater = rater_names[i],
                        cluster = cluster_assign[i],
                        cluster_size = cluster_sizes[cluster_assign[i]],
                        avg_similarity = cluster_similarities[i]
                    ))
                }

                # Store heatmap state
                if (self$options$showClusterHeatmap) {
                    heatmap <- self$results$raterClusterHeatmap
                    heatmap$setState(list(
                        similarity_matrix = similarity_matrix,
                        cluster_assignments = cluster_assign,
                        rater_names = rater_names
                    ))
                }

            }, error = function(e) {
                self$results$raterClusterTable$setNote("error", sprintf("Error in rater clustering: %s", e$message))
            })
        },

        .populateCaseClusterExplanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>Case Clustering</h3>
                    <p style='margin: 0; color: #333;'>Identifies groups of cases with similar rating patterns across raters using hierarchical or k-means clustering.</p>
                </div>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Use Cases</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Identify subgroups of cases with consistent rating patterns</li>
                        <li>Detect controversial or difficult-to-classify cases</li>
                        <li>Understand sources of disagreement (easy vs. hard cases)</li>
                        <li>Stratify analysis by case difficulty</li>
                        <li>Identify cases requiring expert review or consensus discussion</li>
                    </ul>
                    <h4 style='margin: 10px 0 10px 0; color: #333;'>Clinical Examples</h4>
                    <p style='margin: 0;'><strong>Tumor grading:</strong> Cluster cases by grade agreement. Low-agreement clusters may contain borderline cases needing expert review.</p>
                    <p style='margin: 5px 0 0 0;'><strong>Diagnostic categorization:</strong> Identify groups of cases with consistent vs. variable diagnoses across pathologists.</p>
                    <h4 style='margin: 10px 0 10px 0; color: #333;'>Interpretation</h4>
                    <p style='margin: 0;'><strong>Dendrogram height:</strong> Lower = more similar rating patterns. High first split = distinct case groups.</p>
                    <p style='margin: 5px 0 0 0;'><strong>Cluster heatmap:</strong> Dark diagonal blocks = tight clusters (similar cases). Off-diagonal = between-cluster differences.</p>
                    <p style='margin: 5px 0 0 0;'><strong>Low within-cluster similarity:</strong> Cases in cluster are difficult/controversial - low agreement among raters.</p>
                </div></div>"
            self$results$caseClusterExplanation$setContent(jmvcore::format(html))
        },

        .caseDendrogram = function(image, ggtheme, theme, ...) {
            plotState <- image$state
            if (is.null(plotState)) return(FALSE)

            # Reconstruct hclust object from serialized components
            hc <- structure(list(
                merge = plotState$merge[[1]],
                height = plotState$height[[1]],
                order = plotState$order[[1]],
                labels = plotState$labels[[1]],
                method = as.character(plotState$method),
                dist.method = "euclidean"
            ), class = "hclust")
            cluster_labels <- plotState$cluster_labels[[1]]

            # Set up plot
            par(mar = c(7, 4, 4, 2))

            # Plot dendrogram
            plot(hc, main = "Case Clustering Dendrogram",
                 xlab = "", ylab = "Distance", sub = "", hang = -1)

            # Add cluster labels if available
            if (!is.null(cluster_labels)) {
                # Color branches by cluster
                rect.hclust(hc, k = length(unique(cluster_labels)), border = 2:6)
            }

            return(TRUE)
        },

        .caseClusterHeatmap = function(image, ggtheme, theme, ...) {
            plotState <- image$state
            if (is.null(plotState)) return(FALSE)

            sim_matrix <- plotState$similarity_matrix
            cluster_assign <- plotState$cluster_assignments
            case_ids <- plotState$case_ids

            # Reorder by cluster
            if (!is.null(cluster_assign)) {
                ord <- order(cluster_assign)
                sim_matrix <- sim_matrix[ord, ord]
                case_ids <- case_ids[ord]
                cluster_assign <- cluster_assign[ord]
            }

            # Create heatmap using base graphics
            n_cases <- nrow(sim_matrix)
            colors <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100)

            par(mar = c(8, 8, 4, 2))

            # For large datasets, show only a subset of labels
            show_labels <- n_cases <= 50

            image(1:n_cases, 1:n_cases, sim_matrix,
                  col = colors,
                  xlab = "", ylab = "",
                  main = "Case Similarity Matrix (Clustered)",
                  axes = FALSE)

            if (show_labels) {
                axis(1, at = 1:n_cases, labels = case_ids, las = 2, cex.axis = 0.6)
                axis(2, at = 1:n_cases, labels = case_ids, las = 1, cex.axis = 0.6)
            } else {
                # Show only cluster boundaries for large datasets
                axis(1, at = seq(1, n_cases, by = max(1, floor(n_cases/20))),
                     labels = seq(1, n_cases, by = max(1, floor(n_cases/20))), las = 2)
                axis(2, at = seq(1, n_cases, by = max(1, floor(n_cases/20))),
                     labels = seq(1, n_cases, by = max(1, floor(n_cases/20))), las = 1)
            }

            # Add cluster boundaries
            if (!is.null(cluster_assign)) {
                cluster_bounds <- which(diff(cluster_assign) != 0)
                for (b in cluster_bounds) {
                    abline(h = b + 0.5, col = "black", lwd = 2)
                    abline(v = b + 0.5, col = "black", lwd = 2)
                }
            }

            return(TRUE)
        },

        .performCaseClustering = function(ratings) {
            tryCatch({
                method <- self$options$caseClusterMethod
                distance_metric <- self$options$caseClusterDistance
                linkage <- self$options$caseClusterLinkage
                k <- self$options$nCaseClusters

                # Check if data is categorical
                is_categorical <- all(sapply(ratings, function(x) is.factor(x) || is.character(x)))

                n_cases <- nrow(ratings)
                n_raters <- ncol(ratings)
                case_ids <- rownames(ratings)
                if (is.null(case_ids)) {
                    case_ids <- paste0("Case_", 1:n_cases)
                }

                if (n_cases < 3 || n_raters < 2) {
                    self$results$caseClusterTable$setNote("error", "Clustering requires at least 3 cases and 2 raters")
                    return()
                }

                # Compute distance matrix between cases
                if (is_categorical || distance_metric == "agreement") {
                    # For categorical: use agreement-based distance
                    dist_matrix <- matrix(0, n_cases, n_cases)
                    for (i in 1:(n_cases-1)) {
                        for (j in (i+1):n_cases) {
                            # Calculate proportion of raters that agree on this case pair
                            # This measures similarity of rating patterns
                            case_i <- as.character(unlist(ratings[i, ]))
                            case_j <- as.character(unlist(ratings[j, ]))
                            valid_pairs <- !is.na(case_i) & !is.na(case_j)

                            if (sum(valid_pairs) > 0) {
                                # Agreement: proportion of raters giving same rating to both cases
                                agreement <- sum(case_i[valid_pairs] == case_j[valid_pairs]) / sum(valid_pairs)
                                dist_matrix[i, j] <- dist_matrix[j, i] <- 1 - agreement
                            } else {
                                dist_matrix[i, j] <- dist_matrix[j, i] <- 1
                            }
                        }
                    }
                    dist_obj <- as.dist(dist_matrix)
                } else {
                    # For continuous: use correlation or Euclidean
                    if (distance_metric == "correlation") {
                        # Correlation between rating vectors across raters
                        cor_matrix <- tryCatch({
                            stats::cor(t(as.matrix(ratings)), use = "pairwise.complete.obs")
                        }, error = function(e) {
                            matrix(0, n_cases, n_cases)
                        })
                        cor_matrix[is.na(cor_matrix)] <- 0
                        dist_matrix <- 1 - cor_matrix
                        dist_obj <- as.dist(dist_matrix)
                    } else if (distance_metric == "euclidean") {
                        dist_obj <- dist(ratings, method = "euclidean")
                        dist_matrix <- as.matrix(dist_obj)
                    } else {  # manhattan
                        dist_obj <- dist(ratings, method = "manhattan")
                        dist_matrix <- as.matrix(dist_obj)
                    }
                }

                # Perform clustering
                if (method == "hierarchical") {
                    # Hierarchical clustering
                    linkage_method <- switch(linkage,
                                           "average" = "average",
                                           "complete" = "complete",
                                           "single" = "single",
                                           "ward" = "ward.D2")
                    hc <- hclust(dist_obj, method = linkage_method)

                    # Cut tree to get cluster assignments
                    cluster_assign <- cutree(hc, k = k)

                    if (any(is.na(cluster_assign))) {
                        self$results$caseClusterTable$setNote("error", "Error: Missing values in cluster assignments. Try fewer clusters or check data.")
                        return()
                    }

                    # Store dendrogram state (serializable components only)
                    if (self$options$showCaseDendrogram) {
                        dendro <- self$results$caseDendrogram
                        dendro$setState(as.data.frame(list(
                            merge = I(list(hc$merge)),
                            height = I(list(hc$height)),
                            order = I(list(hc$order)),
                            labels = I(list(hc$labels)),
                            method = hc$method,
                            cluster_labels = I(list(cluster_assign))
                        )))
                    }
                } else {
                    # K-means clustering (for continuous data only)
                    if (is_categorical) {
                        self$results$caseClusterTable$setNote("error", "K-means clustering requires continuous data")
                        return()
                    }

                    # Cases as rows, raters as columns (already in correct orientation)
                    km <- kmeans(ratings, centers = k, nstart = 25)
                    cluster_assign <- km$cluster
                }

                # Calculate within-cluster similarities
                similarity_matrix <- 1 - dist_matrix
                cluster_similarities <- numeric(n_cases)

                for (i in 1:n_cases) {
                    # Mean similarity to other members of same cluster
                    same_cluster <- which(cluster_assign == cluster_assign[i] & 1:n_cases != i)
                    if (length(same_cluster) > 0) {
                        cluster_similarities[i] <- mean(similarity_matrix[i, same_cluster])
                    } else {
                        cluster_similarities[i] <- NA
                    }
                }

                # Populate table (limit to first 1000 rows for performance)
                table <- self$results$caseClusterTable
                cluster_sizes <- table(cluster_assign)

                max_rows <- min(n_cases, 1000)
                if (n_cases > 1000) {
                    table$setNote("info", sprintf("Showing first 1000 of %d cases", n_cases))
                }

                for (i in 1:max_rows) {
                    table$addRow(rowKey = i, values = list(
                        case_id = case_ids[i],
                        cluster = cluster_assign[i],
                        avg_similarity = cluster_similarities[i]
                    ))
                }

                # Store heatmap state
                if (self$options$showCaseClusterHeatmap) {
                    heatmap <- self$results$caseClusterHeatmap

                    # For large datasets, subsample for heatmap
                    if (n_cases > 200) {
                        # Sample up to 200 cases, stratified by cluster
                        sample_idx <- c()
                        for (cl in unique(cluster_assign)) {
                            cl_idx <- which(cluster_assign == cl)
                            n_sample <- min(length(cl_idx), ceiling(200 * length(cl_idx) / n_cases))
                            sample_idx <- c(sample_idx, cl_idx[sample.int(length(cl_idx), n_sample)])
                        }

                        heatmap$setState(list(
                            similarity_matrix = similarity_matrix[sample_idx, sample_idx],
                            cluster_assignments = cluster_assign[sample_idx],
                            case_ids = case_ids[sample_idx]
                        ))
                        heatmap$setNote("info", sprintf("Showing %d of %d cases (stratified sample)",
                                                       length(sample_idx), n_cases))
                    } else {
                        heatmap$setState(list(
                            similarity_matrix = similarity_matrix,
                            cluster_assignments = cluster_assign,
                            case_ids = case_ids
                        ))
                    }
                }

            }, error = function(e) {
                self$results$caseClusterTable$setNote("error", sprintf("Error in case clustering: %s", e$message))
            })
        },

        .populateMaxwellREExplanation = function() {
            # Provide educational content about Maxwell's Random Error Index

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Maxwell's Random Error (RE) Index?</h3>
                    <p style='margin: 0; color: #333;'>
                        Maxwell's RE index <strong>decomposes total measurement variance</strong> into two components:
                    </p>
                    <ul style='margin: 10px 0 0 0; padding-left: 20px;'>
                        <li><strong>Systematic Error</strong> - Consistent differences between raters or methods
                            (e.g., one rater consistently rates higher than another)</li>
                        <li><strong>Random Error</strong> - Inconsistent, unpredictable measurement variation
                            (e.g., reading errors, environmental noise, observer fatigue)</li>
                    </ul>
                    <p style='margin: 10px 0 0 0; color: #333;'>
                        <strong>RE = Random Variance / Total Variance</strong>, ranging from 0 (all error is systematic)
                        to 1 (all error is random).
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Maxwell's RE</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for understanding error sources in:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Method comparison studies</strong> - Comparing new vs. established diagnostic methods</li>
                        <li><strong>Rater training</strong> - Identifying whether low agreement is due to systematic bias or random inconsistency</li>
                        <li><strong>Measurement system analysis</strong> - Evaluating measurement process capability</li>
                        <li><strong>Quality improvement</strong> - Targeting interventions (calibration for systematic error, training for random error)</li>
                        <li><strong>Diagnostic test validation</strong> - Understanding measurement reliability characteristics</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpreting RE Values</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>RE Value</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Error Type</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Intervention Strategy</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>RE ≈ 0 - 0.30</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Predominantly systematic error</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Calibration, standardization, bias correction</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>RE ≈ 0.30 - 0.70</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Mixed error sources</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Combined approach: calibration + training</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>RE ≈ 0.70 - 1.00</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Predominantly random error</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Rater training, standardized protocols, quality control</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Use Cases in Pathology</h4>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>1</span>
                            Ki-67 Proliferation Index - Manual vs. Digital Scoring
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Four pathologists score Ki-67 proliferation index (0-100%) on 60 breast cancer
                            cases using both manual counting and digital image analysis software.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Total variance: 145.2<br>
                            • Systematic variance: 112.4 (77.4%) - digital consistently scores 5-8% lower<br>
                            • Random variance: 32.8 (22.6%)<br>
                            • RE = 0.23
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> Low RE (0.23) indicates disagreement is primarily systematic
                            rather than random. Digital method consistently underestimates compared to manual counting.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Apply calibration factor to digital scores rather than extensive rater
                            retraining. Systematic bias can be corrected; random error would require protocol revision.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>2</span>
                            Mitotic Count Agreement in Melanoma
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Three dermatopathologists count mitoses per mm² in 80 melanocytic lesions.
                            Goal: Understand if disagreement stems from systematic differences in counting approach or random errors.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Total variance: 8.7 mitoses²/mm⁴<br>
                            • Systematic variance: 1.9 (21.8%) - minimal systematic differences<br>
                            • Random variance: 6.8 (78.2%)<br>
                            • RE = 0.78
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> High RE (0.78) indicates disagreement is predominantly random
                            rather than systematic. Pathologists don't consistently differ in their counts - variability is
                            unpredictable case-by-case.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Focus on standardizing counting protocols, defining mitotic figures more
                            clearly, and increasing training with challenging examples. Calibration would not help since there's
                            minimal systematic bias.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>3</span>
                            Tumor Size Measurement - Caliper vs. Digital Planimetry
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Two pathologists measure tumor size (mm) on 100 breast specimens using
                            both traditional calipers and digital planimetry software. Each tumor measured twice by each method.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Total variance: 42.5 mm²<br>
                            • Systematic variance: 28.3 (66.6%) - calipers average 2.1 mm larger than digital<br>
                            • Random variance: 14.2 (33.4%)<br>
                            • RE = 0.33
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> Moderate-low RE (0.33) indicates mixed error sources with
                            systematic error predominating. Calipers consistently overestimate due to including surrounding tissue,
                            but there's also substantial random variation within each method.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Standardize measurement protocol (especially tumor margins) AND apply
                            correction factor for caliper measurements. Dual intervention addresses both error sources.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>4</span>
                            Gleason Grading - Expert vs. Community Pathologists
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Five community pathologists and three expert genitourinary pathologists
                            assign Gleason scores (converted to numeric scale) on 120 prostate biopsies.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Total variance: 1.84 (Gleason scale units²)<br>
                            • Systematic variance: 1.12 (60.9%) - community pathologists average 0.7 points higher<br>
                            • Random variance: 0.72 (39.1%)<br>
                            • RE = 0.39
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> Mixed error pattern (RE = 0.39) with systematic error
                            slightly predominating. Community pathologists consistently over-grade compared to experts, but
                            there's also considerable random disagreement within each group.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Implement comprehensive intervention: (1) recalibration sessions to
                            address systematic over-grading, AND (2) standardized criteria training to reduce random
                            disagreement. Monitor RE over time - successful training should not change RE much (addresses both
                            sources).
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>5</span>
                            HER2 IHC Scoring - Pre vs. Post-Training Assessment
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Six pathologists score HER2 IHC (0/1+/2+/3+ converted to 0-3 numeric scale)
                            on 80 breast cancer cases before and after standardized training workshop.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results - Pre-Training:</strong><br>
                            • Total variance: 0.89<br>
                            • Systematic variance: 0.15 (16.9%)<br>
                            • Random variance: 0.74 (83.1%)<br>
                            • RE = 0.83
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results - Post-Training:</strong><br>
                            • Total variance: 0.42 (53% reduction)<br>
                            • Systematic variance: 0.09 (21.4%)<br>
                            • Random variance: 0.33 (78.6%)<br>
                            • RE = 0.79
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> High RE both pre (0.83) and post (0.79) training indicates
                            random error is the primary problem. Training successfully reduced total variance by 53%, with both
                            systematic and random components decreasing proportionally. RE remained stable, confirming the
                            intervention appropriately targeted the predominant error source.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Demonstrates training effectiveness. Stable high RE validates that
                            training (not calibration) was the correct intervention. Continued monitoring with RE analysis
                            ensures sustained quality improvement.
                        </p>
                    </div>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Understanding the Decomposition</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Variance components:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Total Variance</strong> = Systematic Variance + Random Variance</li>
                        <li><strong>Systematic Variance</strong> = Variance of rater/method means (consistent differences)</li>
                        <li><strong>Random Variance</strong> = Average within-rater/method variance (inconsistent variation)</li>
                        <li><strong>RE Index</strong> = Random Variance / Total Variance</li>
                    </ul>
                    <p style='margin: 10px 0 0 0;'>
                        <em>Example:</em> If two methods differ by a constant offset but each is internally consistent, RE ≈ 0
                        (systematic). If two methods have identical means but high variability, RE ≈ 1 (random).
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'> Important Considerations</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Data requirements:</strong> Requires 2+ raters/methods with repeated or paired measurements</li>
                        <li><strong>Interpretation context:</strong> RE alone doesn't indicate good/bad reliability - must consider
                            total variance magnitude</li>
                        <li><strong>Complementary use:</strong> Use with ICC/kappa for complete reliability assessment</li>
                        <li><strong>Intervention planning:</strong> Low RE → calibration/standardization; High RE → training/protocols</li>
                        <li><strong>Monitoring over time:</strong> Track RE changes after interventions to assess effectiveness</li>
                        <li><strong>Data type:</strong> Best suited for continuous or ordinal data with meaningful numeric scale</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Statistical Notes</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Calculation based on one-way ANOVA variance decomposition</li>
                        <li>Between-group (rater/method) variance = systematic component</li>
                        <li>Within-group (residual) variance = random component</li>
                        <li>Assumptions: independence of measurements, homogeneity of variance across groups</li>
                        <li>Related to ICC: When RE is low (systematic error dominates), ICC tends to be lower</li>
                    </ul>
                </div>
            </div>
            "

            self$results$maxwellREExplanation$setContent(html_content)
        },

        .calculateMaxwellRE = function(ratings) {
            # Calculate Maxwell's Random Error Index
            # RE = Random Variance / Total Variance

            tryCatch({
                # Remove rows with any missing values
                complete_rows <- complete.cases(ratings)
                ratings_complete <- ratings[complete_rows, , drop = FALSE]

                n_cases <- nrow(ratings_complete)
                n_raters <- ncol(ratings_complete)

                if (n_cases < 5) {
                    self$results$maxwellRETable$setRow(rowNo = 1, list(
                        method = "Maxwell's RE",
                        subjects = n_cases,
                        raters = n_raters,
                        re_value = NA,
                        systematic_prop = NA,
                        random_prop = NA,
                        total_variance = NA,
                        systematic_var = NA,
                        random_var = NA,
                        interpretation = "Insufficient data (minimum 5 cases required)"
                    ))
                    return()
                }

                if (n_raters < 2) {
                    self$results$maxwellRETable$setRow(rowNo = 1, list(
                        method = "Maxwell's RE",
                        subjects = n_cases,
                        raters = n_raters,
                        re_value = NA,
                        systematic_prop = NA,
                        random_prop = NA,
                        total_variance = NA,
                        systematic_var = NA,
                        random_var = NA,
                        interpretation = "Requires at least 2 raters/methods"
                    ))
                    return()
                }

                # Check that data is numeric
                is_all_numeric <- all(sapply(ratings_complete, function(x) is.numeric(x) && !is.factor(x)))

                if (!is_all_numeric) {
                    self$results$maxwellRETable$setRow(rowNo = 1, list(
                        method = "Maxwell's RE",
                        subjects = n_cases,
                        raters = n_raters,
                        re_value = NA,
                        systematic_prop = NA,
                        random_prop = NA,
                        total_variance = NA,
                        systematic_var = NA,
                        random_var = NA,
                        interpretation = "Maxwell's RE requires continuous or ordinal numeric data"
                    ))
                    return()
                }

                # Convert to matrix for calculations
                ratings_matrix <- as.matrix(ratings_complete)

                # Calculate variance components using one-way ANOVA approach
                # Total variance
                grand_mean <- mean(ratings_matrix)
                total_variance <- var(as.vector(ratings_matrix))

                # Between-rater variance (systematic component)
                rater_means <- colMeans(ratings_matrix)
                between_variance <- var(rater_means)

                # Within-rater variance (random component)
                # Calculate variance within each rater and average
                within_variances <- apply(ratings_matrix, 2, var)
                within_variance <- mean(within_variances)

                # Alternative calculation using sum of squares
                # This is more robust for unbalanced or small samples
                SS_total <- sum((ratings_matrix - grand_mean)^2)
                SS_between <- n_cases * sum((rater_means - grand_mean)^2)
                SS_within <- sum(sapply(1:n_raters, function(j) {
                    sum((ratings_matrix[, j] - rater_means[j])^2)
                }))

                # Variance components from sum of squares
                MS_between <- SS_between / (n_raters - 1)
                MS_within <- SS_within / (n_cases * n_raters - n_raters)

                # Systematic variance component (adjusted for sample size)
                systematic_var <- max(0, (MS_between - MS_within) / n_cases)

                # Random variance component
                random_var <- MS_within

                # Total variance (recalculated from components)
                total_var_components <- systematic_var + random_var

                # Guard: zero total variance (all raters identical on all cases)
                if (total_var_components < .Machine$double.eps) {
                    self$results$maxwellRETable$setRow(rowNo = 1, list(
                        method = "Maxwell's Random Error Index",
                        subjects = n_cases,
                        raters = n_raters,
                        re_value = NA,
                        systematic_prop = NA,
                        random_prop = NA,
                        total_variance = 0,
                        systematic_var = 0,
                        random_var = 0,
                        interpretation = "Cannot compute: total variance is zero (all raters identical on all cases)"
                    ))
                    return()
                }

                # Maxwell's RE index
                re_value <- random_var / total_var_components

                # Proportions
                systematic_prop <- systematic_var / total_var_components
                random_prop <- random_var / total_var_components

                # Interpretation
                if (re_value < 0.30) {
                    interp <- "Predominantly systematic error - consider calibration/standardization"
                } else if (re_value < 0.50) {
                    interp <- "Mixed error (systematic > random) - combined intervention needed"
                } else if (re_value < 0.70) {
                    interp <- "Mixed error (random > systematic) - focus on training and protocols"
                } else {
                    interp <- "Predominantly random error - improve consistency through training"
                }

                self$results$maxwellRETable$setRow(rowNo = 1, list(
                    method = "Maxwell's Random Error Index",
                    subjects = n_cases,
                    raters = n_raters,
                    re_value = re_value,
                    systematic_prop = systematic_prop,
                    random_prop = random_prop,
                    total_variance = total_var_components,
                    systematic_var = systematic_var,
                    random_var = random_var,
                    interpretation = interp
                ))

            }, error = function(e) {
                self$results$maxwellRETable$setNote(
                    "error",
                    sprintf("Error calculating Maxwell's RE: %s", e$message)
                )
            })
        },

        .populateInterIntraRaterExplanation = function() {
            # Provide educational content about Inter/Intra-Rater Reliability

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Inter/Intra-Rater Reliability?</h3>
                    <p style='margin: 0; color: #333;'>
                        This analysis simultaneously assesses two critical aspects of measurement reliability:
                    </p>
                    <ul style='margin: 10px 0 0 0; padding-left: 20px;'>
                        <li><strong>Intra-Rater Reliability (Test-Retest)</strong> - Consistency of each individual rater's
                            assessments across different time points (within-rater consistency)</li>
                        <li><strong>Inter-Rater Reliability</strong> - Agreement between different raters' assessments
                            (between-rater agreement)</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use This Analysis</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for test-retest reliability studies where:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Training evaluation</strong> - Assess whether trainees maintain consistent ratings over time</li>
                        <li><strong>Fatigue studies</strong> - Detect changes in rating consistency during long reading sessions</li>
                        <li><strong>Quality control</strong> - Monitor both individual consistency and team agreement</li>
                        <li><strong>Method validation</strong> - Establish both reproducibility (intra-rater) and comparability (inter-rater)</li>
                        <li><strong>Digital pathology validation</strong> - Compare glass slide vs. digital slide ratings by same pathologists</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Data Structure Requirements</h4>
                    <p style='margin: 0 0 10px 0;'>
                        Column names must follow a pattern to identify rater-timepoint pairs:
                    </p>
                    <div style='background: #fff; padding: 10px; border-radius: 5px; font-family: monospace;'>
                        <strong>Pattern:</strong> RaterID<span style='color: #333;'>[separator]</span>TimePoint<br>
                        <strong>Example with underscore (_):</strong><br>
                        • Rater1_Time1, Rater1_Time2, Rater1_Time3<br>
                        • Rater2_Time1, Rater2_Time2, Rater2_Time3<br>
                        • PathA_T1, PathA_T2 (works with any separator)<br>
                        <strong>Example with dot (.):</strong><br>
                        • Observer1.Pre, Observer1.Post<br>
                        • Observer2.Pre, Observer2.Post
                    </div>
                    <p style='margin: 10px 0 0 0;'><em>The separator character (default: underscore) is specified in the analysis options.</em></p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Statistical Methods Used</h4>
                    <p style='margin: 0 0 10px 0;'>The analysis automatically selects appropriate statistics based on data type:</p>
                    <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                        <li><strong>Categorical/Ordinal Data:</strong> Cohen's Kappa or Weighted Kappa for each rater's test-retest pairs</li>
                        <li><strong>Continuous Data:</strong> Intraclass Correlation Coefficient (ICC) for each rater</li>
                        <li><strong>Inter-Rater:</strong> Overall agreement metric across all raters and time points</li>
                    </ul>
                    <table style='width: 100%; border-collapse: collapse; margin-top: 15px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Reliability Value</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Interpretation</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>&lt; 0.40</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Poor reliability - additional training needed</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.40 - 0.60</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Fair reliability - caution in interpretation</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.60 - 0.75</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Good reliability - acceptable for most purposes</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.75 - 0.90</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Excellent reliability - high quality</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>&gt; 0.90</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Outstanding reliability</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Use Cases in Pathology</h4>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>1</span>
                            Pathology Trainee Evaluation
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Three pathology residents grade 50 breast biopsies for atypia (Absent/Mild/Moderate/Severe)
                            at the beginning of rotation (Week 1) and end of rotation (Week 12).
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Data Structure:</strong> Resident1_Week1, Resident1_Week12, Resident2_Week1, Resident2_Week12, etc.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Intra-rater: Resident1 κ=0.72, Resident2 κ=0.58, Resident3 κ=0.81<br>
                            • Inter-rater: Overall κ=0.64
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> Resident3 shows excellent individual consistency (κ=0.81),
                            Resident2 shows fair consistency (κ=0.58) and may need additional training. Overall inter-rater
                            agreement is good (κ=0.64), indicating residents are learning similar grading criteria.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Identifies which residents need additional mentoring and whether
                            training is producing consistent grading standards across the cohort.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>2</span>
                            Digital Pathology Validation Study
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Four pathologists score 100 melanocytic lesions for mitotic count (continuous)
                            on both glass slides and whole slide images (WSI) separated by 2 weeks.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Data Structure:</strong> PathA_Glass, PathA_WSI, PathB_Glass, PathB_WSI, etc.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Intra-rater: PathA ICC=0.89, PathB ICC=0.85, PathC ICC=0.78, PathD ICC=0.91<br>
                            • Inter-rater: Overall ICC=0.82
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> All pathologists show excellent individual consistency
                            (ICC 0.78-0.91) between glass and digital platforms. Strong inter-rater agreement (ICC=0.82)
                            indicates the digital platform does not introduce systematic bias.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Provides evidence for digital pathology implementation, showing
                            individual pathologists are reliable across platforms and maintain group concordance.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>3</span>
                            Fatigue Study in Frozen Section Diagnosis
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Five pathologists review 40 frozen section cases for adequacy (Adequate/Inadequate)
                            at start of shift (Hour 0) and end of 8-hour shift (Hour 8).
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Data Structure:</strong> Path1_H0, Path1_H8, Path2_H0, Path2_H8, etc.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Intra-rater: Path1 κ=0.91, Path2 κ=0.88, Path3 κ=0.76, Path4 κ=0.85, Path5 κ=0.82<br>
                            • Inter-rater: Hour 0 κ=0.84, Hour 8 κ=0.79
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> All pathologists maintain good-to-excellent individual
                            consistency despite fatigue. Slight decrease in inter-rater agreement (0.84→0.79) suggests
                            mild group-level fatigue effect but still within acceptable range.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Demonstrates that frozen section adequacy assessment remains
                            reliable throughout standard shifts, supporting current work scheduling practices.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>4</span>
                            Multi-Center Clinical Trial Quality Control
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Central pathology review for Phase III trial. Six expert GI pathologists
                            score 80 colonic biopsies for dysplasia grade (None/Low/High) at baseline and 6-month recalibration.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Data Structure:</strong> Expert1_Base, Expert1_M6, Expert2_Base, Expert2_M6, etc.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Intra-rater: Range κ=0.68-0.84 (all experts maintain good consistency)<br>
                            • Inter-rater: Baseline κ=0.72, Month 6 κ=0.76
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> All experts demonstrate good individual test-retest
                            reliability (κ≥0.68). Inter-rater agreement actually improved after recalibration session (0.72→0.76),
                            indicating successful standardization.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Validates central review process for clinical trial, showing both
                            individual pathologist reliability and improved group concordance with periodic recalibration.
                        </p>
                    </div>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #333; margin: 10px 0 5px 0;'>
                            <span style='background: #C2185B; color: white; padding: 2px 8px; border-radius: 3px; margin-right: 8px;'>5</span>
                            Immunohistochemistry Scoring Reproducibility
                        </h5>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Scenario:</strong> Four pathologists score PD-L1 tumor proportion score (TPS, 0-100%, continuous)
                            on 60 lung cancer cases twice, separated by 4 weeks (blind to previous scores).
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Data Structure:</strong> Scorer1_Read1, Scorer1_Read2, Scorer2_Read1, Scorer2_Read2, etc.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Results:</strong><br>
                            • Intra-rater: Scorer1 ICC=0.88, Scorer2 ICC=0.92, Scorer3 ICC=0.79, Scorer4 ICC=0.86<br>
                            • Inter-rater: Overall ICC=0.81
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Clinical Interpretation:</strong> All scorers show good-to-excellent individual reproducibility
                            (ICC 0.79-0.92). Scorer3 with lower ICC (0.79) is still acceptable but might benefit from additional
                            training. Strong inter-rater ICC (0.81) supports use for therapeutic decision-making.
                        </p>
                        <p style='margin: 5px 0; padding-left: 30px;'>
                            <strong>Application:</strong> Establishes that PD-L1 scoring is sufficiently reproducible both
                            within individual scorers and across the scoring team, critical for biomarker-driven therapy selection.
                        </p>
                    </div>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'> Important Considerations</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Blinding:</strong> Raters should be blinded to their previous assessments to measure true reproducibility</li>
                        <li><strong>Time interval:</strong> Balance between too short (memory effects) and too long (true biological changes)</li>
                        <li><strong>Practice effects:</strong> Learning between sessions can inflate intra-rater reliability</li>
                        <li><strong>Sample size:</strong> Minimum 30 cases recommended; 50+ preferred for stable estimates</li>
                        <li><strong>Missing data:</strong> Cases are excluded if any rater-timepoint pair is missing</li>
                        <li><strong>Interpretation:</strong> Low intra-rater reliability suggests measurement unreliability; low inter-rater
                            reliability suggests raters disagree on criteria</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Statistical Notes</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Confidence intervals estimated using Fisher's Z transformation (continuous) or asymptotic standard errors (categorical)</li>
                        <li>P-values test whether reliability significantly differs from zero</li>
                        <li>For ordinal data with 3+ categories, weighted kappa automatically applied</li>
                        <li>Missing values handled by complete case analysis within each rater-timepoint pair</li>
                    </ul>
                </div>
            </div>
            "

            self$results$interIntraRaterExplanation$setContent(html_content)
        },

        .calculateInterIntraRater = function(ratings) {
            # Calculate both intra-rater (test-retest) and inter-rater reliability
            # from column names using separator pattern

            tryCatch({
                separator <- self$options$interIntraSeparator
                var_names <- colnames(ratings)

                # Parse column names to extract rater ID and timepoint
                parsed <- lapply(var_names, function(vname) {
                    parts <- strsplit(vname, separator, fixed = TRUE)[[1]]
                    if (length(parts) >= 2) {
                        list(
                            original = vname,
                            rater = parts[1],
                            timepoint = paste(parts[-1], collapse = separator)
                        )
                    } else {
                        NULL
                    }
                })

                # Remove NULL entries (columns without separator)
                parsed <- Filter(Negate(is.null), parsed)

                if (length(parsed) < 2) {
                    self$results$interIntraRaterIntraTable$setNote(
                        "error",
                        sprintf("Need at least 2 columns with separator '%s' in their names. Found %d. Example: Rater1%sTime1, Rater1%sTime2",
                                separator, length(parsed), separator, separator)
                    )
                    return()
                }

                # Organize by rater
                rater_data <- split(parsed, sapply(parsed, function(x) x$rater))

                # Check that each rater has multiple timepoints
                rater_timepoints <- sapply(rater_data, length)
                valid_raters <- names(rater_timepoints[rater_timepoints >= 2])

                if (length(valid_raters) == 0) {
                    self$results$interIntraRaterIntraTable$setNote(
                        "error",
                        "No rater has multiple time points. Each rater needs at least 2 time points for test-retest analysis."
                    )
                    return()
                }

                # Calculate intra-rater reliability (test-retest for each rater)
                intraTable <- self$results$interIntraRaterIntraTable

                for (rater in valid_raters) {
                    rater_cols <- sapply(rater_data[[rater]], function(x) x$original)

                    # Get all pairwise combinations of timepoints for this rater
                    n_timepoints <- length(rater_cols)

                    if (n_timepoints < 2) next

                    # For simplicity, calculate reliability between first and second timepoint
                    # (could extend to all pairwise combinations and average)
                    col1 <- rater_cols[1]
                    col2 <- rater_cols[2]

                    data1 <- ratings[[col1]]
                    data2 <- ratings[[col2]]

                    # Remove missing pairs
                    complete_idx <- complete.cases(data1, data2)
                    data1 <- data1[complete_idx]
                    data2 <- data2[complete_idx]

                    n_cases <- length(data1)

                    if (n_cases < 5) {
                        intraTable$addRow(rowKey = rater, list(
                            rater = rater,
                            n_cases = n_cases,
                            n_timepoints = n_timepoints,
                            statistic_name = "N/A",
                            value = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            p = NA,
                            interpretation = "Insufficient data"
                        ))
                        next
                    }

                    # Determine data type and calculate appropriate statistic
                    is_numeric1 <- is.numeric(data1) && !is.factor(data1)
                    is_numeric2 <- is.numeric(data2) && !is.factor(data2)

                    if (is_numeric1 && is_numeric2) {
                        # Continuous data - use ICC(2,1)
                        data_matrix <- cbind(data1, data2)
                        if (requireNamespace("irr", quietly = TRUE)) {
                            icc_result <- tryCatch(
                                irr::icc(data_matrix, model = "twoway", type = "agreement", unit = "single"),
                                error = function(e) NULL
                            )

                            if (!is.null(icc_result)) {
                                icc_value <- icc_result$value
                                ci_lower <- icc_result$lbound
                                ci_upper <- icc_result$ubound
                                p_value <- icc_result$p.value

                                if (icc_value < 0.40) {
                                    interp <- "Poor"
                                } else if (icc_value < 0.60) {
                                    interp <- "Fair"
                                } else if (icc_value < 0.75) {
                                    interp <- "Good"
                                } else if (icc_value < 0.90) {
                                    interp <- "Excellent"
                                } else {
                                    interp <- "Outstanding"
                                }

                                intraTable$addRow(rowKey = rater, list(
                                    rater = rater,
                                    n_cases = n_cases,
                                    n_timepoints = n_timepoints,
                                    statistic_name = "ICC(2,1)",
                                    value = icc_value,
                                    ci_lower = ci_lower,
                                    ci_upper = ci_upper,
                                    p = p_value,
                                    interpretation = interp
                                ))
                            } else {
                                intraTable$addRow(rowKey = rater, list(
                                    rater = rater,
                                    n_cases = n_cases,
                                    n_timepoints = n_timepoints,
                                    statistic_name = "ICC(2,1)",
                                    value = NA,
                                    ci_lower = NA,
                                    ci_upper = NA,
                                    p = NA,
                                    interpretation = "ICC calculation failed"
                                ))
                            }
                        } else {
                            intraTable$addRow(rowKey = rater, list(
                                rater = rater,
                                n_cases = n_cases,
                                n_timepoints = n_timepoints,
                                statistic_name = "N/A",
                                value = NA,
                                ci_lower = NA,
                                ci_upper = NA,
                                p = NA,
                                interpretation = "irr package required"
                            ))
                        }

                    } else {
                        # Categorical/ordinal data - use kappa
                        data1 <- as.factor(data1)
                        data2 <- as.factor(data2)

                        # Ensure same levels
                        all_levels <- sort(unique(c(levels(data1), levels(data2))))
                        data1 <- factor(data1, levels = all_levels)
                        data2 <- factor(data2, levels = all_levels)

                        # Check if ordinal (3+ categories) for weighted kappa
                        is_ordinal <- length(all_levels) >= 3

                        if (requireNamespace("irr", quietly = TRUE)) {
                            if (is_ordinal) {
                                # Weighted kappa for ordinal
                                kappa_result <- irr::kappa2(cbind(data1, data2), weight = "squared")
                                stat_name <- "Weighted κ"
                            } else {
                                # Unweighted kappa for nominal
                                kappa_result <- irr::kappa2(cbind(data1, data2), weight = "unweighted")
                                stat_name <- "Cohen's κ"
                            }

                            kappa_value <- kappa_result$value
                            p_value <- kappa_result$p.value

                            # Confidence interval (approximate from z statistic, when available)
                            ci_lower <- NA_real_
                            ci_upper <- NA_real_
                            z_stat <- kappa_result$statistic
                            if (!is.null(z_stat) && is.finite(z_stat) && abs(z_stat) > 1e-6 && is.finite(kappa_value)) {
                                se_kappa <- abs(kappa_value / z_stat)
                                if (se_kappa < 2) {
                                    ci_lower <- max(-1, kappa_value - 1.96 * se_kappa)
                                    ci_upper <- min(1, kappa_value + 1.96 * se_kappa)
                                }
                            }

                            # Interpretation
                            if (kappa_value < 0.40) {
                                interp <- "Poor"
                            } else if (kappa_value < 0.60) {
                                interp <- "Fair"
                            } else if (kappa_value < 0.75) {
                                interp <- "Good"
                            } else if (kappa_value < 0.90) {
                                interp <- "Excellent"
                            } else {
                                interp <- "Outstanding"
                            }

                            intraTable$addRow(rowKey = rater, list(
                                rater = rater,
                                n_cases = n_cases,
                                n_timepoints = n_timepoints,
                                statistic_name = stat_name,
                                value = kappa_value,
                                ci_lower = ci_lower,
                                ci_upper = ci_upper,
                                p = p_value,
                                interpretation = interp
                            ))
                        } else {
                            intraTable$addRow(rowKey = rater, list(
                                rater = rater,
                                n_cases = n_cases,
                                n_timepoints = n_timepoints,
                                statistic_name = "N/A",
                                value = NA,
                                ci_lower = NA,
                                ci_upper = NA,
                                p = NA,
                                interpretation = "irr package required"
                            ))
                        }
                    }
                }

                # Calculate inter-rater reliability across all columns
                # Use the same approach as overall agreement calculation
                all_cols <- sapply(parsed, function(x) x$original)
                inter_ratings <- ratings[, all_cols, drop = FALSE]

                # Remove rows with any missing values
                complete_rows <- complete.cases(inter_ratings)
                inter_ratings <- inter_ratings[complete_rows, , drop = FALSE]

                n_cases <- nrow(inter_ratings)
                n_raters <- ncol(inter_ratings)

                if (n_cases < 5) {
                    self$results$interIntraRaterInterTable$setRow(rowNo = 1, list(
                        method = "Inter-Rater Reliability",
                        n_cases = n_cases,
                        n_raters = n_raters,
                        statistic_name = "N/A",
                        value = NA,
                        ci_lower = NA,
                        ci_upper = NA,
                        p = NA,
                        interpretation = "Insufficient data"
                    ))
                    return()
                }

                # Check data type
                is_all_numeric <- all(sapply(inter_ratings, function(x) is.numeric(x) && !is.factor(x)))

                if (is_all_numeric) {
                    # Use ICC for continuous data
                    if (requireNamespace("irr", quietly = TRUE)) {
                        icc_result <- irr::icc(inter_ratings, model = "twoway", type = "agreement", unit = "single")

                        # Interpretation
                        icc_value <- icc_result$value
                        if (icc_value < 0.40) {
                            interp <- "Poor"
                        } else if (icc_value < 0.60) {
                            interp <- "Fair"
                        } else if (icc_value < 0.75) {
                            interp <- "Good"
                        } else if (icc_value < 0.90) {
                            interp <- "Excellent"
                        } else {
                            interp <- "Outstanding"
                        }

                        self$results$interIntraRaterInterTable$setRow(rowNo = 1, list(
                            method = "Inter-Rater Reliability (All Raters × Time Points)",
                            n_cases = n_cases,
                            n_raters = n_raters,
                            statistic_name = "ICC(2,1)",
                            value = icc_result$value,
                            ci_lower = icc_result$lbound,
                            ci_upper = icc_result$ubound,
                            p = icc_result$p.value,
                            interpretation = interp
                        ))
                    }
                } else {
                    # Use Fleiss' Kappa or Light's Kappa for categorical data
                    # Convert all to factors with same levels
                    all_data <- unlist(inter_ratings)
                    all_levels <- sort(unique(all_data[!is.na(all_data)]))

                    for (i in 1:ncol(inter_ratings)) {
                        inter_ratings[[i]] <- factor(inter_ratings[[i]], levels = all_levels)
                    }

                    if (requireNamespace("irr", quietly = TRUE)) {
                        # Try Fleiss' kappa for multiple raters
                        kappa_result <- irr::kappam.fleiss(inter_ratings)

                        kappa_value <- kappa_result$value
                        p_value <- kappa_result$p.value

                        # Interpretation
                        if (kappa_value < 0.40) {
                            interp <- "Poor"
                        } else if (kappa_value < 0.60) {
                            interp <- "Fair"
                        } else if (kappa_value < 0.75) {
                            interp <- "Good"
                        } else if (kappa_value < 0.90) {
                            interp <- "Excellent"
                        } else {
                            interp <- "Outstanding"
                        }

                        # Approximate CI from z statistic when available
                        ci_lower <- NA_real_
                        ci_upper <- NA_real_
                        z_stat <- kappa_result$statistic
                        if (!is.null(z_stat) && is.finite(z_stat) && abs(z_stat) > 1e-6 && is.finite(kappa_value)) {
                            se_kappa <- abs(kappa_value / z_stat)
                            if (se_kappa < 2) {
                                ci_lower <- max(-1, kappa_value - 1.96 * se_kappa)
                                ci_upper <- min(1, kappa_value + 1.96 * se_kappa)
                            }
                        }

                        self$results$interIntraRaterInterTable$setRow(rowNo = 1, list(
                            method = "Inter-Rater Reliability (All Raters × Time Points)",
                            n_cases = n_cases,
                            n_raters = n_raters,
                            statistic_name = "Fleiss' κ",
                            value = kappa_value,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper,
                            p = p_value,
                            interpretation = interp
                        ))
                    }
                }

            }, error = function(e) {
                self$results$interIntraRaterIntraTable$setNote(
                    "error",
                    sprintf("Error calculating Inter/Intra-Rater Reliability: %s", e$message)
                )
            })
        },

        .populateRaterBiasExplanation = function() {
            # Provide educational content about Rater Bias Test

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is the Rater Bias Test?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Rater Bias Test uses a <strong>chi-square test</strong> to detect whether raters have
                        <strong>systematically different rating patterns</strong>. It tests the null hypothesis that
                        all raters use the rating categories with equal frequency.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use This Test</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for quality control when:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Rater training</strong> - Identifying trainees who are systematically too lenient or strict</li>
                        <li><strong>Performance monitoring</strong> - Detecting drift in rater behavior over time</li>
                        <li><strong>Multi-center studies</strong> - Ensuring consistent grading across sites</li>
                        <li><strong>Certification</strong> - Verifying raters use categories appropriately before certification</li>
                        <li><strong>Detecting systematic errors</strong> - Finding raters who consistently over-diagnose or under-diagnose</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpreting Results</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Result</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Interpretation</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>p ≥ 0.05</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>No significant bias detected - raters use categories similarly</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>p < 0.05</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Significant bias detected - raters have systematically different patterns</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>p < 0.001</strong></td>
                            <td style='padding: 8px;'>Strong evidence of bias - investigate individual rater frequencies</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Common Bias Patterns in Pathology</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 13px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Pattern</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Clinical Impact</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Lenient rater</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Systematically assigns lower grades (potential under-diagnosis)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Strict rater</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Systematically assigns higher grades (potential over-diagnosis)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Central tendency bias</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Avoids extreme categories, clusters ratings in middle</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Extreme response bias</strong></td>
                            <td style='padding: 8px;'>Overuses extreme categories, avoids middle grades</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Example</h4>
                    <p style='margin: 0; padding: 10px; background: white; border-radius: 4px; font-size: 13px;'>
                        <strong>Scenario:</strong> Three pathologists grade 50 tumor samples as G1/G2/G3.<br><br>
                        <strong>Pathologist A:</strong> 45% G1, 40% G2, 15% G3<br>
                        <strong>Pathologist B:</strong> 20% G1, 60% G2, 20% G3<br>
                        <strong>Pathologist C:</strong> 15% G1, 40% G2, 45% G3<br><br>
                        <strong>Result: p < 0.001</strong> - Strong evidence of systematic bias. Pathologist A is lenient
                        (favors low grades), Pathologist C is strict (favors high grades). This requires investigation
                        and potentially retraining before they can reliably grade cases.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Important Considerations</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Sample size matters:</strong> Test has limited power with small samples (< 30 cases)</li>
                        <li><strong>Bias ≠ Poor agreement:</strong> Raters can be biased but still agree (all systematically lenient)</li>
                        <li><strong>Clinical context:</strong> Some bias may be acceptable (e.g., erring on side of caution)</li>
                        <li><strong>Training intervention:</strong> Significant bias often correctable with feedback and training</li>
                        <li><strong>Follow-up:</strong> After detecting bias, examine individual rater frequency distributions</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Relationship to Agreement Measures</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        Rater bias is <strong>independent from agreement</strong>. You can have:<br><br>
                         <strong>High agreement + No bias:</strong> Ideal situation<br>
                         <strong>High agreement + Bias:</strong> All raters systematically lenient/strict together<br>
                         <strong>Low agreement + No bias:</strong> Random disagreement but no systematic patterns<br>
                         <strong>Low agreement + Bias:</strong> Both random and systematic errors present
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Stuart, A. A. (1955). A test for homogeneity of the marginal distributions
                        in a two-way classification. <em>Biometrika</em>, 42(3/4), 412-416.
                    </p>
                </div>
            </div>
            "

            self$results$raterBiasExplanation$setContent(html_content)
        },

        .calculateRaterBias = function(ratings) {
            # Test for systematic rater bias using chi-square test
            # Detects if raters have different marginal distributions

            # Validate data is categorical
            if (any(sapply(ratings, is.numeric)) && !any(sapply(ratings, is.factor))) {
                self$results$raterBiasTable$setNote(
                    "error",
                    "Rater Bias Test requires categorical data. Your data appears to be continuous."
                )
                return()
            }

            # irr::rater.bias only works with exactly 2 raters
            if (ncol(ratings) != 2) {
                self$results$raterBiasTable$setNote(
                    "error",
                    sprintf("Rater Bias Test requires exactly 2 raters. You have %d. For 3+ raters, use Rater Profile Plots or marginal tests.",
                            ncol(ratings))
                )
                return()
            }

            # Prepare data - remove rows with all missing values
            complete_idx <- rowSums(!is.na(ratings)) > 0
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 2) {
                self$results$raterBiasTable$setNote(
                    "error",
                    "Insufficient complete cases for Rater Bias Test. At least 2 cases required."
                )
                return()
            }

            # Calculate Rater Bias Test
            tryCatch({
                # irr::rater.bias returns chi-square statistic, df, p-value
                bias_result <- irr::rater.bias(ratings_clean)

                # Populate table
                self$results$raterBiasTable$setRow(rowNo = 1, values = list(
                    method = "Chi-square test for marginal homogeneity",
                    chisq = bias_result$statistic,
                    df = bias_result$parameter,
                    p = bias_result$p.value
                ))

                # Add interpretation note
                if (bias_result$p.value >= 0.05) {
                    interp <- "No significant bias detected"
                    detail <- "Raters use rating categories with similar frequencies. No evidence of systematic lenient/strict patterns."
                } else if (bias_result$p.value >= 0.01) {
                    interp <- "Significant bias detected"
                    detail <- "Raters have systematically different rating patterns (p < .05). Examine individual rater frequency distributions to identify which raters are lenient/strict."
                } else {
                    interp <- "Strong evidence of bias"
                    detail <- "Raters have markedly different rating patterns (p < .01). This indicates some raters systematically over-diagnose or under-diagnose. Recommend retraining or excluding biased raters."
                }

                self$results$raterBiasTable$setNote(
                    "interpretation",
                    sprintf("%s: %s", interp, detail)
                )

                # Add sample size note if small
                if (nrow(ratings_clean) < 30) {
                    self$results$raterBiasTable$setNote(
                        "power",
                        sprintf("Note: Small sample size (n=%d). Test may have limited power to detect bias. Results should be interpreted cautiously.",
                                nrow(ratings_clean))
                    )
                }

            }, error = function(e) {
                self$results$raterBiasTable$setNote(
                    "error",
                    sprintf("Error calculating Rater Bias Test: %s", e$message)
                )
            })
        },

        .populateBhapkarExplanation = function() {
            # Provide educational content about Bhapkar Test

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is the Bhapkar Test?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Bhapkar test is a <strong>chi-square test for marginal homogeneity</strong> between
                        <strong>two raters</strong> with <strong>multiple categories</strong>. It tests whether two
                        raters use rating categories with equal frequency. This is the extension of <strong>McNemar's test</strong>
                        (which is limited to 2×2 tables) to larger contingency tables.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Bhapkar Test</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Ideal for paired comparisons with &gt;2 categories:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Pre-post training</strong> - Compare pathologist before and after training on same cases</li>
                        <li><strong>Novice vs. Expert</strong> - Compare trainee ratings with senior pathologist</li>
                        <li><strong>AI vs. Human</strong> - Compare algorithm predictions with pathologist diagnoses</li>
                        <li><strong>Method comparison</strong> - Compare two grading systems on same specimens</li>
                        <li><strong>Rater drift detection</strong> - Compare same rater at two time points</li>
                        <li><strong>Quality control</strong> - Test if systematic bias exists between paired raters</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Bhapkar vs. Related Tests</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Test</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Raters</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Categories</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Purpose</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>McNemar</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>2 (paired)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>2</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Binary marginal homogeneity</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Bhapkar</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>2 (paired)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>&gt;2</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Multi-category marginal homogeneity</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Stuart-Maxwell</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>2 (paired)</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>&gt;2</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Less powerful alternative</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Rater Bias</strong></td>
                            <td style='padding: 8px;'>2+</td>
                            <td style='padding: 8px;'>2+</td>
                            <td style='padding: 8px;'>Test all raters simultaneously</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Key difference:</strong> Bhapkar is more powerful than Stuart-Maxwell for large samples.
                        Both tests are asymptotically equivalent.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Pathology Use Cases</h4>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>1. AI Algorithm Validation (Dysplasia Detection):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Compare AI algorithm vs. expert pathologist on 100 cervical dysplasia cases</li>
                        <li><strong>Categories:</strong> Negative, LSIL, HSIL, carcinoma</li>
                        <li><strong>Agreement:</strong> 60% perfect agreement on diagonal</li>
                        <li><strong>Marginal frequencies:</strong> AI: 20% negative, 35% LSIL, 30% HSIL, 15% CA; Pathologist: 25%, 30%, 30%, 15%</li>
                        <li><strong>Bhapkar p = 0.03:</strong> AI systematically assigns fewer &quot;negative&quot; and more &quot;LSIL&quot; diagnoses</li>
                        <li><strong>Action:</strong> Algorithm requires recalibration to match expert diagnostic thresholds</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>2. Pre-Post Training Evaluation (Inflammatory Bowel Disease):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> GI pathologist re-evaluates 80 IBD biopsies after attending Robarts Histopathology Index workshop</li>
                        <li><strong>Categories:</strong> Quiescent, mild, moderate, severe activity</li>
                        <li><strong>Hypothesis:</strong> Training should standardize scoring, reducing tendency to overgrade mild cases</li>
                        <li><strong>Bhapkar test:</strong> Tests if marginal distributions changed after training</li>
                        <li><strong>p &lt; 0.05:</strong> Significant shift - pathologist now uses &quot;quiescent&quot; more and &quot;mild&quot; less frequently</li>
                        <li><strong>Interpretation:</strong> Training successfully recalibrated diagnostic thresholds</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>3. Trainee vs. Expert Comparison (Glomerulonephritis):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Resident and attending nephrologist both score 60 kidney biopsies</li>
                        <li><strong>Categories:</strong> Normal, minimal change, focal segmental, diffuse proliferative, advanced sclerosis</li>
                        <li><strong>Concern:</strong> Trainee may systematically underdiagnose severe disease</li>
                        <li><strong>Bhapkar test:</strong> Detects if resident's marginal distribution differs from expert</li>
                        <li><strong>p = 0.002:</strong> Resident assigns significantly more &quot;minimal change&quot; and fewer &quot;advanced sclerosis&quot; diagnoses</li>
                        <li><strong>Educational intervention:</strong> Focused training on chronic injury recognition</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>4. Immunostain Method Comparison (PD-L1 Scoring):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Compare two PD-L1 antibody clones (22C3 vs. SP263) on same lung cancer cases</li>
                        <li><strong>Categories:</strong> TPS <1%, 1-49%, 50-74%, ≥75%</li>
                        <li><strong>Same pathologist:</strong> Evaluates paired sections stained with different antibodies</li>
                        <li><strong>Bhapkar test:</strong> Tests if antibodies yield different category distributions</li>
                        <li><strong>p = 0.12:</strong> No significant difference - antibodies produce comparable scoring distributions</li>
                        <li><strong>Conclusion:</strong> Methods can be used interchangeably for clinical decision-making</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>5. Rater Drift Detection (Longitudinal Quality Control):</p>
                    <ul style='margin: 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Senior dermatopathologist re-scores 50 melanoma cases after 2-year interval</li>
                        <li><strong>Categories:</strong> Clark level I, II, III, IV, V</li>
                        <li><strong>Quality assurance:</strong> Detect if scoring patterns have drifted over time</li>
                        <li><strong>Bhapkar test:</strong> Compares marginal distributions at Time 1 vs. Time 2</li>
                        <li><strong>p = 0.04:</strong> Pathologist now assigns more Level III and fewer Level II diagnoses</li>
                        <li><strong>Action:</strong> Review diagnostic criteria, consider reference set recalibration</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpretation</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 14px;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>p ≥ 0.05</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>No significant difference in marginal distributions</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>p &lt; 0.05</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Significant marginal heterogeneity detected</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>p &lt; 0.01</td>
                            <td style='padding: 5px;'>Strong evidence - raters use categories differently</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Important:</strong> Significant Bhapkar test indicates <em>systematic bias</em> in category
                        usage but does not mean <em>poor agreement</em>. Use in conjunction with Cohen's kappa to assess
                        both agreement and bias.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Requirements and Assumptions</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Exactly 2 raters:</strong> Test designed for paired comparisons only</li>
                        <li><strong>Same subjects rated twice:</strong> Data must be paired (same cases)</li>
                        <li><strong>Multiple categories (&gt;2):</strong> For 2×2 tables, use McNemar's test instead</li>
                        <li><strong>Large sample:</strong> More reliable with n &gt; 30 cases</li>
                        <li><strong>Categorical data:</strong> Ordered or unordered categories</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Complementary Analyses</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        Use Bhapkar test <strong>together with</strong>:<br><br>

                         <strong>Cohen's Kappa:</strong> Measures agreement (Bhapkar tests marginal homogeneity)<br>
                         <strong>Confusion matrix:</strong> Shows specific disagreement patterns<br>
                         <strong>Marginal frequency tables:</strong> Identify which categories differ most<br><br>

                        <strong>Ideal scenario:</strong> High kappa (good agreement) + Non-significant Bhapkar (no bias)
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Bhapkar, V. P. (1966). A note on the equivalence of two test criteria
                        for hypotheses in categorical data. <em>Journal of the American Statistical Association</em>, 61, 228-235.
                    </p>
                </div>
            </div>
            "

            self$results$bhapkarExplanation$setContent(html_content)
        },

        .calculateBhapkar = function(ratings) {
            # Calculate Bhapkar test for marginal homogeneity (2 raters only)

            # Validate exactly 2 raters
            if (ncol(ratings) != 2) {
                self$results$bhapkarTable$setNote(
                    "error",
                    sprintf("Bhapkar test requires exactly 2 raters. You have selected %d raters. For multiple raters, use Rater Bias Test instead.",
                            ncol(ratings))
                )
                return()
            }

            # Validate data is categorical
            if (any(sapply(ratings, is.numeric)) && !any(sapply(ratings, is.factor))) {
                self$results$bhapkarTable$setNote(
                    "error",
                    "Bhapkar test requires categorical data. Your data appears to be continuous."
                )
                return()
            }

            # Remove rows with missing data
            complete_idx <- complete.cases(ratings)
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 5) {
                self$results$bhapkarTable$setNote(
                    "error",
                    "Insufficient complete cases for Bhapkar test. At least 5 cases required."
                )
                return()
            }

            # Check if more than 2 categories exist
            n_categories <- length(unique(c(ratings_clean[[1]], ratings_clean[[2]])))

            if (n_categories == 2) {
                self$results$bhapkarTable$setNote(
                    "warning",
                    "Only 2 categories detected. For 2×2 tables, McNemar's test is more appropriate. Bhapkar test is designed for >2 categories."
                )
            }

            # Calculate Bhapkar test
            tryCatch({
                bhapkar_result <- irr::bhapkar(ratings_clean)

                # Interpret p-value
                if (is.na(bhapkar_result$p.value)) {
                    interp <- "Cannot interpret"
                } else if (bhapkar_result$p.value >= 0.05) {
                    interp <- "No significant difference in marginal distributions"
                } else if (bhapkar_result$p.value >= 0.01) {
                    interp <- "Significant marginal heterogeneity (p < .05)"
                } else {
                    interp <- "Strong evidence of different category usage (p < .01)"
                }

                # Populate table
                df_value <- as.integer(gsub(".*\\((\\d+)\\).*", "\\1", bhapkar_result$stat.name))
                self$results$bhapkarTable$setRow(rowNo = 1, values = list(
                    method = "Bhapkar test for marginal homogeneity",
                    subjects = bhapkar_result$subjects,
                    raters = bhapkar_result$raters,
                    chisq = bhapkar_result$statistic,
                    df = df_value,
                    p = bhapkar_result$p.value,
                    interpretation = interp
                ))

                # Add detailed interpretation note
                if (bhapkar_result$p.value < 0.05) {
                    detail <- sprintf("The two raters use rating categories with significantly different frequencies (p = %.4f).
                                     This indicates systematic bias - one rater may be more lenient or strict than the other.
                                     Examine the marginal frequency tables to identify which categories show the largest discrepancies.
                                     Consider this alongside Cohen's kappa to distinguish bias from poor agreement.",
                                     bhapkar_result$p.value)
                } else {
                    detail <- sprintf("No significant difference in marginal distributions (p = %.3f).
                                     Both raters use rating categories with similar frequencies, suggesting no systematic bias.",
                                     bhapkar_result$p.value)
                }

                self$results$bhapkarTable$setNote(
                    "interpretation",
                    detail
                )

                # Add sample size warning if needed
                if (nrow(ratings_clean) < 30) {
                    self$results$bhapkarTable$setNote(
                        "power",
                        sprintf("Note: Small sample size (n=%d). Test may have limited power. Results should be interpreted cautiously.
                                Recommend n ≥ 30 for reliable chi-square approximation.",
                                nrow(ratings_clean))
                    )
                }

            }, error = function(e) {
                self$results$bhapkarTable$setNote(
                    "error",
                    sprintf("Error calculating Bhapkar test: %s", e$message)
                )
            })
        },

        .populateStuartMaxwellExplanation = function() {
            # Provide educational content about Stuart-Maxwell Test

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is the Stuart-Maxwell Test?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Stuart-Maxwell test is a <strong>classic chi-square test for marginal homogeneity</strong>
                        between <strong>two raters</strong> with <strong>multiple categories</strong>. It tests whether
                        two raters use rating categories with equal frequency in <strong>matched/paired data</strong>.
                        This is the traditional extension of <strong>McNemar's test</strong> (which is for 2×2 tables) to
                        larger contingency tables.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Stuart-Maxwell Test</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Ideal for matched/paired comparisons:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Matched data analysis</strong> - Same subjects rated twice under different conditions</li>
                        <li><strong>Pre-post comparisons</strong> - Before and after intervention on same cases</li>
                        <li><strong>Repeated measurements</strong> - Same rater assessing cases at two time points</li>
                        <li><strong>Method comparison</strong> - Two different diagnostic methods on same specimens</li>
                        <li><strong>Observer agreement</strong> - Two observers rating same cases independently</li>
                        <li><strong>Traditional choice</strong> - Historical standard for marginal homogeneity testing</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Stuart-Maxwell vs. Related Tests</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Test</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Categories</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Characteristics</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>McNemar</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>2</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Classic test for binary paired data</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Stuart-Maxwell</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>&gt;2</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Traditional choice, well-established</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Bhapkar</strong></td>
                            <td style='padding: 8px;'>&gt;2</td>
                            <td style='padding: 8px;'>More powerful for large samples (n &gt; 50)</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Note:</strong> Stuart-Maxwell and Bhapkar are asymptotically equivalent and give similar
                        results with large samples. Bhapkar is recommended for n &gt; 50 due to better statistical properties.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Pathology Use Cases</h4>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>1. Post-Training Diagnostic Recalibration (Breast Pathology):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Pathologist re-evaluates 80 breast cases after HER2 scoring workshop</li>
                        <li><strong>Categories:</strong> Benign, atypical, DCIS, invasive carcinoma</li>
                        <li><strong>Original grading:</strong> 35% benign, 25% atypical, 20% DCIS, 20% invasive</li>
                        <li><strong>Post-training:</strong> 30% benign, 20% atypical, 25% DCIS, 25% invasive</li>
                        <li><strong>Stuart-Maxwell p = 0.04:</strong> Significant shift toward more aggressive diagnoses</li>
                        <li><strong>Interpretation:</strong> Training recalibrated diagnostic thresholds - investigate if improvement or overcorrection</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>2. Digital vs. Glass Slide Diagnosis (Method Comparison):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Same pathologist diagnoses 100 skin biopsies using whole slide imaging vs. traditional microscopy</li>
                        <li><strong>Categories:</strong> Normal, dysplastic nevus, melanoma in-situ, invasive melanoma</li>
                        <li><strong>Concern:</strong> Digital platform may alter diagnostic behavior</li>
                        <li><strong>Stuart-Maxwell test:</strong> Detects if marginal distributions differ between modalities</li>
                        <li><strong>p = 0.68:</strong> No significant difference in category usage between digital and glass</li>
                        <li><strong>Validation outcome:</strong> Digital pathology yields equivalent diagnostic distributions - suitable for clinical use</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>3. Temporal Stability Assessment (Intra-Rater Reliability):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Gynecologic pathologist re-scores 60 endometrial biopsies 6 months later (blinded)</li>
                        <li><strong>Categories:</strong> Proliferative, hyperplasia without atypia, atypical hyperplasia, carcinoma</li>
                        <li><strong>Quality control:</strong> Assess consistency of diagnostic criteria over time</li>
                        <li><strong>Stuart-Maxwell test:</strong> Tests if marginal distributions are stable</li>
                        <li><strong>p = 0.89:</strong> No significant drift - pathologist maintains consistent diagnostic thresholds</li>
                        <li><strong>QA implication:</strong> Demonstrates long-term diagnostic reliability</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>4. Stain Protocol Optimization (Immunohistochemistry):</p>
                    <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> Compare standard vs. optimized Ki-67 staining protocol on same cases</li>
                        <li><strong>Categories:</strong> Low (<10%), intermediate (10-30%), high (30-50%), very high (>50%)</li>
                        <li><strong>Same observer:</strong> Scores 70 breast cancer cases with both protocols</li>
                        <li><strong>Stuart-Maxwell test:</strong> Detects if new protocol shifts scoring distribution</li>
                        <li><strong>p = 0.02:</strong> Optimized protocol yields more &quot;high&quot; and &quot;very high&quot; scores</li>
                        <li><strong>Technical decision:</strong> Enhanced sensitivity requires recalibration of clinical cutoffs</li>
                    </ul>

                    <p style='margin: 0 0 10px 0; font-weight: bold;'>5. Diagnostic Criteria Update Impact (Prostate Pathology):</p>
                    <ul style='margin: 0; padding-left: 20px; line-height: 1.6;'>
                        <li><strong>Scenario:</strong> GU pathologist applies 2014 vs. 2019 WHO criteria to 90 prostate biopsies</li>
                        <li><strong>Categories:</strong> Benign, HGPIN, Gleason 6, Gleason 7, Gleason 8-10</li>
                        <li><strong>Context:</strong> 2019 criteria refined cribriform pattern and intraductal carcinoma definitions</li>
                        <li><strong>Stuart-Maxwell test:</strong> Quantifies impact of criteria change on diagnostic distribution</li>
                        <li><strong>p = 0.001:</strong> 2019 criteria yield significantly more Gleason 7 and fewer Gleason 6 diagnoses</li>
                        <li><strong>Clinical impact:</strong> Demonstrates systematic shift in treatment thresholds with updated guidelines</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpretation</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 14px;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>p ≥ 0.05</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>No significant difference - marginal homogeneity holds</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>p &lt; 0.05</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Significant marginal heterogeneity detected</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>p &lt; 0.01</td>
                            <td style='padding: 5px;'>Strong evidence - systematic shift in category usage</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 13px;'>
                        <strong>Important:</strong> Significant test indicates <em>systematic change</em> in category usage
                        but does not necessarily mean <em>poor agreement</em>. Use alongside Cohen's kappa or correlation
                        to assess overall consistency.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Assumptions and Requirements</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Paired/matched data:</strong> Same subjects rated twice or same cases by two raters</li>
                        <li><strong>Two raters only:</strong> Designed for comparing exactly 2 sets of ratings</li>
                        <li><strong>Multiple categories (&gt;2):</strong> Use McNemar for 2×2 tables</li>
                        <li><strong>Categorical data:</strong> Works with nominal or ordinal categories</li>
                        <li><strong>Sample size:</strong> More reliable with n &gt; 30; consider Bhapkar for n &gt; 50</li>
                        <li><strong>Complete pairs:</strong> Cases with missing data are excluded</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Use Together With</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        <strong>Complementary analyses:</strong><br><br>

                         <strong>Cohen's Kappa:</strong> Measures agreement (Stuart-Maxwell tests marginal distributions)<br>
                         <strong>Contingency table:</strong> Shows exact disagreement patterns cell-by-cell<br>
                         <strong>McNemar-Bowker:</strong> Provides cell-by-cell symmetry test<br>
                         <strong>Marginal frequency comparison:</strong> Identifies which categories differ most<br><br>

                        <strong>Ideal outcome:</strong> High kappa (good agreement) + Non-significant Stuart-Maxwell
                        (no systematic bias in category usage)
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>References:</strong><br>
                        Stuart, A. A. (1955). A test for homogeneity of the marginal distributions in a two-way
                        classification. <em>Biometrika</em>, 42, 412-416.<br>
                        Maxwell, A. E. (1970). Comparing the classification of subjects by two independent judges.
                        <em>British Journal of Psychiatry</em>, 116, 651-655.
                    </p>
                </div>
            </div>
            "

            self$results$stuartMaxwellExplanation$setContent(html_content)
        },

        .calculateStuartMaxwell = function(ratings) {
            # Calculate Stuart-Maxwell test for marginal homogeneity (2 raters only)

            # Validate exactly 2 raters
            if (ncol(ratings) != 2) {
                self$results$stuartMaxwellTable$setNote(
                    "error",
                    sprintf("Stuart-Maxwell test requires exactly 2 raters. You have selected %d raters. For multiple raters, use Rater Bias Test instead.",
                            ncol(ratings))
                )
                return()
            }

            # Validate data is categorical
            if (any(sapply(ratings, is.numeric)) && !any(sapply(ratings, is.factor))) {
                self$results$stuartMaxwellTable$setNote(
                    "error",
                    "Stuart-Maxwell test requires categorical data. Your data appears to be continuous."
                )
                return()
            }

            # Remove rows with missing data
            complete_idx <- complete.cases(ratings)
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 5) {
                self$results$stuartMaxwellTable$setNote(
                    "error",
                    "Insufficient complete cases for Stuart-Maxwell test. At least 5 cases required."
                )
                return()
            }

            # Check if more than 2 categories exist
            n_categories <- length(unique(c(ratings_clean[[1]], ratings_clean[[2]])))

            if (n_categories == 2) {
                self$results$stuartMaxwellTable$setNote(
                    "warning",
                    "Only 2 categories detected. For 2×2 tables, McNemar's test is the standard choice. Stuart-Maxwell is designed for >2 categories."
                )
            }

            # Calculate Stuart-Maxwell test
            tryCatch({
                # irr::stuart.maxwell.mh expects a matrix
                ratings_matrix <- as.matrix(ratings_clean)

                stuart_result <- irr::stuart.maxwell.mh(ratings_matrix)

                # Interpret p-value
                if (is.na(stuart_result$p.value)) {
                    interp <- "Cannot interpret"
                } else if (stuart_result$p.value >= 0.05) {
                    interp <- "No significant difference - marginal homogeneity holds"
                } else if (stuart_result$p.value >= 0.01) {
                    interp <- "Significant marginal heterogeneity (p < .05)"
                } else {
                    interp <- "Strong evidence of systematic shift (p < .01)"
                }

                # Populate table
                df_value <- as.integer(gsub(".*\\((\\d+)\\).*", "\\1", stuart_result$stat.name))
                self$results$stuartMaxwellTable$setRow(rowNo = 1, values = list(
                    method = "Stuart-Maxwell test for marginal homogeneity",
                    subjects = stuart_result$subjects,
                    raters = stuart_result$raters,
                    chisq = stuart_result$statistic,
                    df = df_value,
                    p = stuart_result$p.value,
                    interpretation = interp
                ))

                # Add detailed interpretation note
                if (stuart_result$p.value < 0.05) {
                    detail <- sprintf("The two raters use rating categories with significantly different frequencies (p = %.4f).
                                     This indicates systematic change or bias in category usage between the two conditions/raters.
                                     Examine marginal frequency tables to identify which categories show the largest shifts.
                                     Consider this alongside Cohen's kappa to distinguish systematic bias from poor agreement.",
                                     stuart_result$p.value)
                } else {
                    detail <- sprintf("No significant difference in marginal distributions (p = %.3f).
                                     Marginal homogeneity holds - both raters/conditions use categories with similar frequencies.",
                                     stuart_result$p.value)
                }

                self$results$stuartMaxwellTable$setNote(
                    "interpretation",
                    detail
                )

                # Add sample size note
                if (nrow(ratings_clean) < 30) {
                    self$results$stuartMaxwellTable$setNote(
                        "power",
                        sprintf("Note: Small sample size (n=%d). Test may have limited power. Results should be interpreted cautiously.
                                For n > 50, consider using Bhapkar test which has better statistical properties for large samples.",
                                nrow(ratings_clean))
                    )
                } else if (nrow(ratings_clean) >= 50) {
                    self$results$stuartMaxwellTable$setNote(
                        "note",
                        sprintf("Sample size n=%d. For samples this large, Bhapkar test may be more powerful as it has better asymptotic properties.",
                                nrow(ratings_clean))
                    )
                }

            }, error = function(e) {
                self$results$stuartMaxwellTable$setNote(
                    "error",
                    sprintf("Error calculating Stuart-Maxwell test: %s", e$message)
                )
            })
        },

        .populatePairwiseKappaExplanation = function() {
            # Provide educational content about Pairwise Kappa Analysis

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Pairwise Kappa Analysis?</h3>
                    <p style='margin: 0; color: #333;'>
                        Pairwise Kappa Analysis compares <strong>each rater individually</strong> against a
                        <strong>reference rater</strong> (gold standard, consensus, or senior expert).
                        Each comparison produces a separate Cohen's kappa measuring agreement between that
                        rater and the reference.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Pairwise Analysis</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Training assessment</strong> - Compare trainees vs expert to measure learning progress</li>
                        <li><strong>Rater certification</strong> - Verify raters meet minimum kappa threshold vs gold standard</li>
                        <li><strong>Performance monitoring</strong> - Track individual rater agreement with reference over time</li>
                        <li><strong>Quality control</strong> - Identify specific raters who need retraining</li>
                        <li><strong>Competency evaluation</strong> - Rank raters by performance for advancement decisions</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpreting Kappa Values</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Kappa</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Agreement Level</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Training Status</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>κ < 0.40</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Poor to fair</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'> Needs significant training</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.40-0.60</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Moderate</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'> Additional training recommended</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.60-0.75</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Substantial</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'> Acceptable performance</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>κ > 0.75</strong></td>
                            <td style='padding: 8px;'>Excellent</td>
                            <td style='padding: 8px;'> Certified/Expert level</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Choosing the Reference Rater</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 13px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Reference Type</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Use Case</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Gold Standard</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Verified diagnosis (biopsy, surgery, outcome data)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Consensus Score</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Modal rating from expert panel (create using Consensus Variable)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Senior Expert</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Most experienced pathologist/clinician</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>External Reviewer</strong></td>
                            <td style='padding: 8px;'>Independent expert from another institution</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Example: Trainee Certification</h4>
                    <p style='margin: 0; padding: 10px; background: white; border-radius: 4px; font-size: 13px;'>
                        <strong>Scenario:</strong> Five pathology residents (Raters 1-5) grade 100 tumor samples.
                        A senior pathologist provides reference diagnoses.<br><br>
                        <strong>Results:</strong><br>
                        • Resident 1: κ = 0.82 (Excellent)  Ready for certification<br>
                        • Resident 2: κ = 0.68 (Substantial)  Acceptable, continue monitoring<br>
                        • Resident 3: κ = 0.52 (Moderate)  Additional training needed<br>
                        • Resident 4: κ = 0.45 (Moderate)  Review difficult cases with expert<br>
                        • Resident 5: κ = 0.28 (Poor)  Requires intensive retraining<br><br>
                        <strong>Action:</strong> Residents 1-2 certified. Residents 3-5 receive targeted training
                        based on specific error patterns, then retest after 3 months.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Ranking Raters</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        When <strong>Rank Raters by Performance</strong> is enabled, raters are sorted from highest
                        to lowest kappa. This identifies:<br><br>
                         <strong>Top performers:</strong> Candidates for senior roles, teaching positions, or gold standard rating<br>
                         <strong>Middle performers:</strong> Acceptable but could benefit from continued training<br>
                         <strong>Low performers:</strong> Require immediate intervention or exclusion from study
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Pairwise vs Overall Agreement</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        <strong>Pairwise Kappa (vs Reference):</strong> Measures each rater's agreement with gold standard<br>
                        → <em>Focus: Individual performance assessment</em><br><br>
                        <strong>Fleiss'/Light's Kappa:</strong> Measures overall agreement among all raters<br>
                        → <em>Focus: General reliability of rating system</em><br><br>
                        <strong>Recommendation:</strong> Use both! Overall kappa shows if your rating system is reliable.
                        Pairwise kappa identifies which specific raters need training.
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Cohen, J. (1960). A coefficient of agreement for nominal scales.
                        <em>Educational and Psychological Measurement</em>, 20(1), 37-46.
                    </p>
                </div>
            </div>
            "

            self$results$pairwiseKappaExplanation$setContent(html_content)
        },

        .calculatePairwiseKappa = function(ratings, reference_ratings) {
            # Calculate Cohen's kappa for each rater vs reference

            # Validate reference rater is provided
            if (is.null(reference_ratings) || length(reference_ratings) == 0) {
                self$results$pairwiseKappaTable$setNote(
                    "error",
                    "Please select a reference rater variable."
                )
                return()
            }

            # Ensure reference_ratings is a vector
            if (is.data.frame(reference_ratings)) {
                reference_ratings <- reference_ratings[[1]]
            }

            # Validate data is categorical
            if (any(sapply(ratings, is.numeric)) && !any(sapply(ratings, is.factor))) {
                self$results$pairwiseKappaTable$setNote(
                    "warning",
                    "Pairwise kappa is designed for categorical data. Your data appears to be continuous."
                )
            }

            # Get rater names
            rater_names <- names(ratings)
            n_raters <- length(rater_names)

            # Store results for ranking
            kappa_results <- list()

            # Calculate kappa for each rater vs reference
            for (i in 1:n_raters) {
                rater_data <- ratings[[i]]
                rater_name <- rater_names[i]

                # Create pairwise data frame
                pairwise_data <- data.frame(
                    rater = rater_data,
                    reference = reference_ratings
                )

                # Remove rows with any missing values
                complete_idx <- complete.cases(pairwise_data)
                pairwise_clean <- pairwise_data[complete_idx, ]

                if (nrow(pairwise_clean) < 2) {
                    # Insufficient data for this rater
                    kappa_results[[rater_name]] <- list(
                        rater = rater_name,
                        subjects = 0,
                        kappa = NA,
                        z = NA,
                        p = NA,
                        error = "Insufficient complete cases"
                    )
                    next
                }

                # Calculate Cohen's kappa
                tryCatch({
                    kappa_result <- irr::kappa2(pairwise_clean)

                    kappa_results[[rater_name]] <- list(
                        rater = rater_name,
                        subjects = kappa_result$subjects,
                        kappa = kappa_result$value,
                        z = kappa_result$statistic,
                        p = kappa_result$p.value,
                        error = NULL
                    )

                }, error = function(e) {
                    kappa_results[[rater_name]] <- list(
                        rater = rater_name,
                        subjects = nrow(pairwise_clean),
                        kappa = NA,
                        z = NA,
                        p = NA,
                        error = e$message
                    )
                })
            }

            # Rank raters if requested
            if (self$options$rankRaters) {
                # Sort by kappa (descending), handling NAs
                valid_kappas <- sapply(kappa_results, function(x) if(is.null(x$error)) x$kappa else NA)
                rank_order <- order(valid_kappas, decreasing = TRUE, na.last = TRUE)
                kappa_results <- kappa_results[rank_order]

                # Assign ranks
                rank <- 1
                for (i in seq_along(kappa_results)) {
                    if (!is.na(kappa_results[[i]]$kappa)) {
                        kappa_results[[i]]$rank <- rank
                        rank <- rank + 1
                    } else {
                        kappa_results[[i]]$rank <- NA
                    }
                }
            }

            # Populate table
            pairwise_table <- self$results$pairwiseKappaTable

            for (result in kappa_results) {
                row_values <- list(
                    rater = result$rater,
                    subjects = result$subjects,
                    kappa = result$kappa,
                    z = result$z,
                    p = result$p
                )

                if (self$options$rankRaters) {
                    row_values$rank <- result$rank
                }

                pairwise_table$addRow(rowKey = result$rater, values = row_values)

                # Add interpretation or error note
                if (!is.null(result$error)) {
                    pairwise_table$addFootnote(rowKey = result$rater, col = "kappa",
                                               sprintf("Error: %s", result$error))
                } else if (!is.na(result$kappa)) {
                    # Add interpretation
                    kappa_val <- result$kappa
                    if (kappa_val < 0.40) {
                        interp <- "Poor to fair - needs training"
                    } else if (kappa_val < 0.60) {
                        interp <- "Moderate - additional training recommended"
                    } else if (kappa_val < 0.75) {
                        interp <- "Substantial - acceptable"
                    } else {
                        interp <- "Excellent - certified/expert level"
                    }

                    pairwise_table$addFootnote(rowKey = result$rater, col = "kappa", interp)
                }
            }

            # Add overall summary note
            valid_kappas <- sapply(kappa_results, function(x) if(is.null(x$error)) x$kappa else NA)
            mean_kappa <- mean(valid_kappas, na.rm = TRUE)
            n_valid <- sum(!is.na(valid_kappas))

            if (n_valid > 0) {
                pairwise_table$setNote(
                    "summary",
                    sprintf("Average kappa across %d raters: %.3f. Raters are compared individually against the reference rater using Cohen's kappa.",
                            n_valid, mean_kappa)
                )
            }
        },

        .populateICCExplanation = function() {
            # Provide educational content about ICC

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is ICC (Intraclass Correlation Coefficient)?</h3>
                    <p style='margin: 0; color: #333;'>
                        ICC measures the reliability and agreement of <strong>continuous measurements</strong>
                        between raters. It's the gold standard for assessing inter-rater reliability with numeric data.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use ICC</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Use ICC for continuous measurements:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Tumor size</strong> – Diameter measurements in mm or cm</li>
                        <li><strong>Biomarker quantification</strong> – Protein expression levels, cell counts</li>
                        <li><strong>Morphometric analysis</strong> – Nuclear size, gland area measurements</li>
                        <li><strong>Scoring systems</strong> – Continuous scores (e.g., 0-100 scale)</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>ICC Model Selection Guide</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 13px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Model</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Design</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Use Case</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(1,1)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>One-way random</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Each case rated by different raters</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(2,1)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Two-way random</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>All cases rated by same raters (random sample)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(3,1)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Two-way mixed</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>All cases rated by same raters (fixed panel)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(1,k)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>One-way, average</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Reliability of mean of k different raters</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(2,k)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Two-way random, average</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Reliability of mean rating (random raters)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>ICC(3,k)</strong></td>
                            <td style='padding: 8px;'>Two-way mixed, average</td>
                            <td style='padding: 8px;'>Reliability of mean rating (fixed panel)</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 12px; color: #555;'>
                        <strong>Tip:</strong> Use ICC(2,1) or ICC(3,1) for most pathology studies where all cases are rated by the same pathologists.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Technical Reference: irr::icc() Parameters</h4>
                    <p style='margin: 0 0 10px 0; font-size: 13px; color: #555;'>
                        This table shows the exact parameters passed to the <code>irr::icc()</code> function for each model.
                        Useful for understanding implementation details and replicating results in R.
                    </p>
                    <table style='width: 100%; border-collapse: collapse; font-size: 12px; font-family: monospace;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Model</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>irr::icc Parameters</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Use Case</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(1,1)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>model=&quot;oneway&quot;, type=&quot;consistency&quot;, unit=&quot;single&quot;</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Each case rated by different raters</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(2,1)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>model=&quot;twoway&quot;, type=&quot;agreement&quot;, unit=&quot;single&quot;</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>All cases by same raters (random sample)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(3,1)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>model=&quot;twoway&quot;, type=&quot;consistency&quot;, unit=&quot;single&quot;</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>All cases by same raters (fixed panel)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(1,k)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>model=&quot;oneway&quot;, type=&quot;consistency&quot;, unit=&quot;average&quot;</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Average of k different raters</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC(2,k)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>model=&quot;twoway&quot;, type=&quot;agreement&quot;, unit=&quot;average&quot;</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Average rating (random raters)</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>ICC(3,k)</strong></td>
                            <td style='padding: 8px;'>model=&quot;twoway&quot;, type=&quot;consistency&quot;, unit=&quot;average&quot;</td>
                            <td style='padding: 8px;'>Average rating (fixed panel)</td>
                        </tr>
                    </table>
                    <p style='margin: 10px 0 0 0; font-size: 11px; color: #666;'>
                        <strong>Note:</strong> <em>model</em> = &quot;oneway&quot; or &quot;twoway&quot; |
                        <em>type</em> = &quot;consistency&quot; or &quot;agreement&quot; |
                        <em>unit</em> = &quot;single&quot; or &quot;average&quot;
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpreting ICC Values</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>ICC < 0.50</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Poor reliability</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.50 – 0.75</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Moderate reliability</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold; border-bottom: 1px solid #ddd;'>0.75 – 0.90</td>
                            <td style='padding: 5px; border-bottom: 1px solid #ddd;'>Good reliability</td>
                        </tr>
                        <tr>
                            <td style='padding: 5px; font-weight: bold;'>ICC > 0.90</td>
                            <td style='padding: 5px;'>Excellent reliability</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>ICC vs Bland-Altman</h4>
                    <p style='margin: 0;'>
                        <strong>ICC</strong> quantifies reliability with a single coefficient (0-1).<br>
                        <strong>Bland-Altman</strong> visualizes agreement and detects systematic bias.<br>
                        <em>Best practice: Use both methods together for comprehensive assessment.</em>
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in assessing rater reliability.
                        <em>Psychological Bulletin</em>, 86(2), 420-428.
                    </p>
                </div>
            </div>
            "

            self$results$iccExplanation$setContent(html_content)
        },

        .calculateICC = function(ratings) {
            # Calculate Intraclass Correlation Coefficient for continuous data

            # Validate that data is numeric
            if (!all(sapply(ratings, is.numeric))) {
                self$results$iccTable$setNote(
                    "error",
                    "ICC requires continuous (numeric) data. Your data appears to be categorical. Use kappa instead."
                )
                return()
            }

            # Get ICC type
            icc_type <- self$options$iccType

            # Map user-friendly names to irr package format
            # irr::icc(ratings, model, type, unit, conf.level)
            # model: "oneway" or "twoway"
            # type: "consistency" or "agreement"
            # unit: "single" or "average"

            model_mapping <- list(
                icc11 = "oneway",
                icc21 = "twoway",
                icc31 = "twoway",
                icc1k = "oneway",
                icc2k = "twoway",
                icc3k = "twoway"
            )

            type_mapping <- list(
                icc11 = "consistency",
                icc21 = "agreement",
                icc31 = "consistency",
                icc1k = "consistency",
                icc2k = "agreement",
                icc3k = "consistency"
            )

            unit_mapping <- list(
                icc11 = "single",
                icc21 = "single",
                icc31 = "single",
                icc1k = "average",
                icc2k = "average",
                icc3k = "average"
            )

            # Get ICC parameters
            model_param <- model_mapping[[icc_type]]
            type_param <- type_mapping[[icc_type]]
            unit_param <- unit_mapping[[icc_type]]

            # Prepare data - remove rows with any missing values
            complete_idx <- complete.cases(ratings)
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) < 2) {
                self$results$iccTable$setNote(
                    "error",
                    "Insufficient complete cases for ICC calculation. At least 2 complete cases required."
                )
                return()
            }

            # Calculate ICC
            tryCatch({
                icc_result <- irr::icc(
                    ratings_clean,
                    model = model_param,
                    type = type_param,
                    unit = unit_param,
                    conf.level = self$options$confLevel
                )

                # Determine model name for display
                model_names <- list(
                    icc11 = "ICC(1,1) - One-way random, single",
                    icc21 = "ICC(2,1) - Two-way random, single",
                    icc31 = "ICC(3,1) - Two-way mixed, single",
                    icc1k = "ICC(1,k) - One-way random, average",
                    icc2k = "ICC(2,k) - Two-way random, average",
                    icc3k = "ICC(3,k) - Two-way mixed, average"
                )

                # Populate table
                self$results$iccTable$setRow(rowNo = 1, values = list(
                    model = model_names[[icc_type]],
                    subjects = nrow(ratings_clean),
                    raters = ncol(ratings_clean),
                    icc_value = icc_result$value,
                    ci_lower = icc_result$lbound,
                    ci_upper = icc_result$ubound,
                    f_value = icc_result$Fvalue,
                    df1 = icc_result$df1,
                    df2 = icc_result$df2,
                    p = icc_result$p.value
                ))

                # Add interpretation note
                icc_val <- icc_result$value
                if (icc_val < 0.50) {
                    interp <- "Poor reliability"
                } else if (icc_val < 0.75) {
                    interp <- "Moderate reliability"
                } else if (icc_val < 0.90) {
                    interp <- "Good reliability"
                } else {
                    interp <- "Excellent reliability"
                }

                self$results$iccTable$setNote(
                    "interpretation",
                    sprintf("Interpretation: %s (Koo & Li, 2016). Consider using Bland-Altman plot for visual assessment of agreement.", interp)
                )

            }, error = function(e) {
                self$results$iccTable$setNote(
                    "error",
                    sprintf("Error calculating ICC: %s", e$message)
                )
            })
        },

        .calculateIota = function(ratings) {
            # Calculate Iota coefficient for multivariate interrater agreement
            # Iota measures agreement across multiple variables/dimensions simultaneously

            n_vars <- ncol(ratings)
            n_cases <- nrow(ratings)

            # Iota requires at least 2 variables (dimensions) to be meaningful
            if (n_vars < 2) {
                self$results$iotaTable$setNote(
                    "error",
                    "Iota coefficient requires multiple variables (raters) for multivariate analysis. Select at least 2 rater columns. For single-variable agreement, use ICC or Kappa instead."
                )
                return()
            }

            # Prepare data for irr::iota
            # irr::iota expects a list of matrices, one per variable
            # In our simplified implementation, we treat each column as one dimension

            # Check data type
            has_numeric <- any(sapply(ratings, is.numeric))
            has_factor <- any(sapply(ratings, is.factor)) || any(sapply(ratings, is.character))

            # Determine scale data type
            if (all(sapply(ratings, is.numeric))) {
                scale_data <- "quantitative"
                data_desc <- "quantitative (continuous)"
            } else if (all(sapply(ratings, is.factor)) || all(sapply(ratings, function(x) is.factor(x) || is.character(x)))) {
                scale_data <- "nominal"
                data_desc <- "nominal (categorical)"
                # Convert character columns to factors
                for (i in seq_along(ratings)) {
                    if (is.character(ratings[[i]])) {
                        ratings[[i]] <- as.factor(ratings[[i]])
                    }
                }
            } else {
                self$results$iotaTable$setNote(
                    "error",
                    "Iota requires all variables to be the same type (all numeric OR all categorical). Your data contains mixed types."
                )
                return()
            }

            # For Iota, create list of matrices - simplified implementation
            ratings_list <- list(as.matrix(ratings))

            # Calculate Iota
            tryCatch({
                iota_result <- irr::iota(
                    ratings_list,
                    scaledata = scale_data,
                    standardize = self$options$iotaStandardize && scale_data == "quantitative"
                )

                # Populate table
                method_text <- sprintf("Iota (%s data)", data_desc)
                if (self$options$iotaStandardize && scale_data == "quantitative") {
                    method_text <- paste0(method_text, " - standardized")
                }

                # Interpret Iota value
                iota_val <- iota_result$value
                if (iota_val < 0.40) {
                    interp <- "Poor agreement"
                } else if (iota_val < 0.60) {
                    interp <- "Fair agreement"
                } else if (iota_val < 0.75) {
                    interp <- "Good agreement"
                } else if (iota_val < 0.90) {
                    interp <- "Very good agreement"
                } else {
                    interp <- "Excellent agreement"
                }

                self$results$iotaTable$setRow(rowNo = 1, values = list(
                    method = method_text,
                    subjects = iota_result$subjects,
                    raters = iota_result$raters,
                    variables = length(ratings_list),
                    iota_value = iota_val,
                    interpretation = interp
                ))

                # Add informational note
                note_text <- sprintf(
                    "Iota coefficient: %.3f (%s). Iota is chance-corrected and ranges from 0 (no agreement) to 1 (perfect agreement).",
                    iota_val, interp
                )

                if (scale_data == "quantitative" && self$options$iotaStandardize) {
                    note_text <- paste0(note_text, " Variables were z-standardized before analysis.")
                }

                self$results$iotaTable$setNote("info", note_text)

            }, error = function(e) {
                self$results$iotaTable$setNote(
                    "error",
                    sprintf("Error calculating Iota: %s", e$message)
                )
            })
        },

        .populateIotaExplanation = function() {
            # Generate comprehensive explanation of Iota coefficient

            html_content <- "
                <div style='font-family: Arial, sans-serif; max-width: 900px; padding: 20px;'>
                    <h3 style='color: #333; border-bottom: 2px solid #333; padding-bottom: 10px;'>
                        Iota Coefficient for Multivariate Interrater Agreement
                    </h3>

                    <div style='background: #E8F5E9; padding: 15px; border-left: 4px solid #4CAF50; margin: 20px 0;'>
                        <h4 style='margin: 0 0 10px 0; color: #333;'>What is Iota?</h4>
                        <p style='margin: 0; line-height: 1.6;'>
                            Iota (ι) is a <strong>chance-corrected agreement index</strong> for multivariate observations. Unlike ICC which
                            analyzes one variable at a time, Iota assesses <strong>overall agreement across multiple variables simultaneously</strong>.
                        </p>
                    </div>

                    <div style='background: #FFF3E0; padding: 15px; border-left: 4px solid #FF9800; margin: 20px 0;'>
                        <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Pathology Use Cases</h4>

                        <p style='margin: 0 0 10px 0; font-weight: bold;'>1. Comprehensive Tumor Assessment (Multiple Parameters):</p>
                        <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                            <li><strong>Scenario:</strong> Two pathologists evaluate colorectal adenocarcinomas</li>
                            <li><strong>Variables:</strong> Tumor size (mm), histologic grade (1-3), mitotic count (per 10 HPF), percentage of necrosis, lymphovascular invasion score</li>
                            <li><strong>Why Iota:</strong> Single value captures overall diagnostic concordance across all tumor characteristics</li>
                            <li><strong>Advantage:</strong> More clinically relevant than separate ICC for each variable</li>
                        </ul>

                        <p style='margin: 0 0 10px 0; font-weight: bold;'>2. IHC Biomarker Panels (Breast Cancer Subtyping):</p>
                        <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                            <li><strong>Scenario:</strong> Quality control study comparing pathologists' IHC scoring</li>
                            <li><strong>Variables:</strong> ER Allred score (0-8), PR Allred score (0-8), HER2 score (0-3+), Ki-67 percentage (0-100)</li>
                            <li><strong>Why Iota:</strong> Accounts for biological correlation between biomarkers (ER and PR often concordant)</li>
                            <li><strong>Clinical Impact:</strong> Ensures reliable breast cancer subtype classification for treatment decisions</li>
                        </ul>

                        <p style='margin: 0 0 10px 0; font-weight: bold;'>3. Digital Pathology Validation (Multi-Feature Assessment):</p>
                        <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                            <li><strong>Scenario:</strong> Validating AI algorithm vs. expert pathologists on prostate biopsies</li>
                            <li><strong>Variables:</strong> Gleason pattern 3 percentage, pattern 4 percentage, pattern 5 percentage, cribriform pattern presence, perineural invasion extent</li>
                            <li><strong>Why Iota:</strong> Multivariate assessment matches clinical workflow where multiple features inform Gleason score</li>
                            <li><strong>Application:</strong> Validates AI systems for clinical deployment</li>
                        </ul>

                        <p style='margin: 0 0 10px 0; font-weight: bold;'>4. Multi-Organ Biopsy Agreement (Transplant Pathology):</p>
                        <ul style='margin: 0 0 15px 0; padding-left: 20px; line-height: 1.6;'>
                            <li><strong>Scenario:</strong> Two pathologists assess kidney transplant biopsies (Banff criteria)</li>
                            <li><strong>Variables:</strong> Interstitial inflammation (i score 0-3), tubulitis (t score 0-3), arteritis (v score 0-3), glomerulitis (g score 0-3), peritubular capillaritis (ptc score 0-3)</li>
                            <li><strong>Why Iota:</strong> Banff classification requires integration of multiple morphologic features</li>
                            <li><strong>Clinical Relevance:</strong> Accurate rejection grading determines immunosuppression management</li>
                        </ul>

                        <p style='margin: 0 0 10px 0; font-weight: bold;'>5. Cytology Adequacy Assessment (Thyroid FNA):</p>
                        <ul style='margin: 0; padding-left: 20px; line-height: 1.6;'>
                            <li><strong>Scenario:</strong> Comparing cytotechnologist and cytopathologist assessments</li>
                            <li><strong>Variables:</strong> Number of follicular cell groups, cellularity score (1-4), colloid amount (0-3), macrophage count category</li>
                            <li><strong>Why Iota:</strong> Adequacy depends on multiple interdependent morphologic criteria</li>
                            <li><strong>Benefit:</strong> Single reliability metric for training and quality assurance programs</li>
                        </ul>
                    </div>

                    <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px;'>
                        <h4 style='margin: 0 0 10px 0;'>Interpretation</h4>
                        <p><strong>< 0.40:</strong> Poor | <strong>0.40-0.60:</strong> Fair | <strong>0.60-0.75:</strong> Good | <strong>0.75-0.90:</strong> Very good | <strong>> 0.90:</strong> Excellent</p>
                    </div>
                </div>
            "

            self$results$iotaExplanation$setContent(html_content)
        },

        .calculatePABAK = function(ratings) {
            # Calculate PABAK, Prevalence Index, and Bias Index (Byrt et al. 1993)
            # Requires exactly 2 raters with categorical data
            tryCatch({
                n_raters <- ncol(ratings)
                if (n_raters != 2) {
                    self$results$pabakTable$setNote(
                        "error",
                        "PABAK requires exactly 2 raters. Please select only 2 rater variables."
                    )
                    return()
                }

                # Get complete cases
                complete_idx <- complete.cases(ratings)
                r1 <- as.character(ratings[complete_idx, 1])
                r2 <- as.character(ratings[complete_idx, 2])
                n <- length(r1)

                if (n < 2) {
                    self$results$pabakTable$setNote(
                        "error",
                        "Insufficient complete cases for PABAK calculation."
                    )
                    return()
                }

                # Build confusion matrix
                categories <- sort(unique(c(r1, r2)))
                n_cat <- length(categories)

                # Calculate observed agreement
                agree <- sum(r1 == r2)
                po <- agree / n

                # Cohen's kappa
                confusion <- table(factor(r1, levels = categories), factor(r2, levels = categories))
                row_margins <- rowSums(confusion)
                col_margins <- colSums(confusion)
                pe <- sum(row_margins * col_margins) / (n^2)

                kappa <- if (abs(1 - pe) < .Machine$double.eps) NA_real_ else (po - pe) / (1 - pe)

                # PABAK = 2*Po - 1
                pabak <- 2 * po - 1

                # For 2x2 tables: compute prevalence and bias indices
                if (n_cat == 2) {
                    a <- confusion[1, 1]  # both say category 1
                    d <- confusion[2, 2]  # both say category 2
                    b <- confusion[1, 2]  # rater1=cat1, rater2=cat2
                    cc <- confusion[2, 1] # rater1=cat2, rater2=cat1
                    prevalence_index <- abs(a - d) / n
                    bias_index <- abs(b - cc) / n
                } else {
                    # Generalized for multi-category (Byrt 1993 extension)
                    prevalence_index <- NA_real_
                    bias_index <- NA_real_
                    self$results$pabakTable$setNote(
                        "multicategory",
                        "Prevalence and bias indices are defined for 2x2 tables. For multi-category data, only PABAK is reported."
                    )
                }

                # Interpretation
                if (is.na(kappa)) {
                    interp <- "Kappa undefined (perfect marginal agreement)"
                } else if (!is.na(prevalence_index) && prevalence_index > 0.5 && kappa < 0.4) {
                    interp <- "Kappa paradox: low kappa likely due to high prevalence imbalance"
                } else if (!is.na(bias_index) && bias_index > 0.3) {
                    interp <- "Substantial rater bias detected; PABAK may be more appropriate"
                } else if (abs(pabak - ifelse(is.na(kappa), 0, kappa)) > 0.2) {
                    interp <- "Notable difference between kappa and PABAK; check prevalence/bias effects"
                } else {
                    interp <- "Kappa and PABAK are consistent"
                }

                self$results$pabakTable$setRow(rowNo = 1, values = list(
                    subjects = n,
                    observedAgreement = po,
                    kappa = kappa,
                    pabak = pabak,
                    prevalenceIndex = prevalence_index,
                    biasIndex = bias_index,
                    interpretation = interp
                ))

            }, error = function(e) {
                self$results$pabakTable$setNote(
                    "error",
                    paste("Error calculating PABAK:", e$message)
                )
            })
        },

        .calculateGwetAC = function(ratings) {
            # Calculate Gwet's AC1 (unweighted) or AC2 (weighted) coefficient
            # More stable than kappa for high agreement or rare categories

            # Get weights option
            weights <- self$options$gwetWeights

            # Prepare data - irrCAC expects subjects in rows, raters in columns
            # Convert to matrix format, handling missing data
            n_subjects <- nrow(ratings)
            n_raters <- ncol(ratings)

            # Remove rows with all missing values
            complete_idx <- rowSums(!is.na(ratings)) > 0
            ratings_clean <- ratings[complete_idx, , drop = FALSE]

            if (nrow(ratings_clean) == 0) {
                self$results$gwetTable$setNote(
                    "error",
                    "No complete cases available for Gwet's AC calculation."
                )
                return()
            }

            # Convert to format expected by irrCAC
            # For categorical data, need to convert factors to character/numeric
            ratings_matrix <- as.matrix(ratings_clean)

            # Determine method name
            if (weights == "unweighted") {
                method_name <- "Gwet's AC1 (unweighted)"
                weight_param <- "unweighted"
            } else if (weights == "linear") {
                method_name <- "Gwet's AC2 (linear weights)"
                weight_param <- "linear"
            } else {
                method_name <- "Gwet's AC2 (quadratic weights)"
                weight_param <- "quadratic"
            }

            # Calculate Gwet's AC
            tryCatch({
                # Use gwet.ac1.raw for raw agreement data
                result <- irrCAC::gwet.ac1.raw(
                    ratings = ratings_matrix,
                    weights = weight_param,
                    conflev = self$options$confLevel,
                    N = Inf
                )

                # Extract results (irrCAC >= 1.0 field names)
                coef <- result$est$coeff.val
                se <- result$est$coeff.se
                p_value <- result$est$p.value

                # Parse conf.int string like "(-0.751,1)"
                ci_str <- gsub("[\\(\\)]", "", result$est$conf.int)
                ci_parts <- as.numeric(strsplit(ci_str, ",")[[1]])
                ci_lower <- ci_parts[1]
                ci_upper <- ci_parts[2]

                # Populate table
                self$results$gwetTable$setRow(rowNo = 1, values = list(
                    method = method_name,
                    subjects = nrow(ratings_clean),
                    raters = n_raters,
                    coefficient = coef,
                    se = se,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    p = p_value
                ))

                # Add interpretation note
                if (coef < 0) {
                    interp <- "Poor agreement (less than chance)"
                } else if (coef < 0.20) {
                    interp <- "Slight agreement"
                } else if (coef < 0.40) {
                    interp <- "Fair agreement"
                } else if (coef < 0.60) {
                    interp <- "Moderate agreement"
                } else if (coef < 0.80) {
                    interp <- "Substantial agreement"
                } else {
                    interp <- "Almost perfect agreement"
                }

                self$results$gwetTable$setNote(
                    "interpretation",
                    sprintf("Interpretation: %s. Gwet's AC is more robust than kappa when dealing with high agreement rates or unbalanced categories.", interp)
                )

            }, error = function(e) {
                self$results$gwetTable$setNote(
                    "error",
                    sprintf("Error calculating Gwet's AC: %s", e$message)
                )
            })
        },

        .populateLevelInfo = function(ratings) {
            # Display level ordering information for categorical variables
            # Essential for weighted kappa to verify ordinal levels are properly ordered

            table <- self$results$levelInfoTable

            # Process each rater variable
            for (i in seq_along(self$options$vars)) {
                var_name <- self$options$vars[i]
                var_data <- ratings[[i]]

                # Determine variable type and levels
                is_factor <- is.factor(var_data)
                is_ordered <- is.ordered(var_data)

                if (is_factor) {
                    levels_list <- levels(var_data)
                    n_levels <- length(levels_list)
                    levels_str <- paste(levels_list, collapse = " → ")

                    if (is_ordered) {
                        data_type <- "Ordinal (ordered factor)"
                        note <- " Suitable for weighted kappa"
                    } else {
                        data_type <- "Nominal (unordered factor)"
                        note <- "Use unweighted kappa or convert to ordinal"
                    }
                } else {
                    # Numeric or other types
                    unique_vals <- sort(unique(var_data[!is.na(var_data)]))
                    n_levels <- length(unique_vals)

                    if (n_levels <= 20) {
                        levels_str <- paste(unique_vals, collapse = " → ")
                    } else {
                        levels_str <- paste("Range:", min(unique_vals), "to", max(unique_vals))
                    }

                    if (is.numeric(var_data)) {
                        data_type <- "Numeric (continuous)"
                        note <- "Consider Bland-Altman instead of kappa"
                    } else {
                        data_type <- "Character/Other"
                        note <- "Convert to factor for kappa analysis"
                    }
                }

                # Add row to table
                table$addRow(rowKey = i, values = list(
                    variable = var_name,
                    levels = levels_str,
                    n_levels = n_levels,
                    data_type = data_type,
                    note = note
                ))
            }

            # Add informational note about weighted kappa
            if (any(sapply(ratings, is.ordered))) {
                table$setNote(
                    "weighted_info",
                    "For weighted kappa, verify that ordinal levels are in the correct order (e.g., G1 → G2 → G3 for tumor grades). Use jamovi's Data → Setup tab to reorder levels if needed."
                )
            }
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
                    prop_bias_p <<- NA
                })
            }

            # Normality test on differences (Shapiro-Wilk)
            normality_w <- NA
            normality_p <- NA
            tryCatch({
                n_diff <- length(diff)
                if (n_diff >= 3 && n_diff <= 5000) {
                    sw <- shapiro.test(diff)
                    normality_w <- sw$statistic
                    normality_p <- sw$p.value
                }
            }, error = function(e) {
                normality_w <<- NA
                normality_p <<- NA
            })

            # Populate statistics table
            ba_values <- list(
                meanDiff = mean_diff,
                sdDiff = sd_diff,
                lowerLoA = lower_loa,
                upperLoA = upper_loa,
                propBiasP = prop_bias_p,
                normalityW = normality_w,
                normalityP = normality_p
            )
            self$results$blandAltmanStats$setRow(rowNo = 1, values = ba_values)

            # Warn if non-normal
            if (!is.na(normality_p) && normality_p < 0.05) {
                self$results$blandAltmanStats$setNote(
                    "normality",
                    "Differences are not normally distributed (Shapiro-Wilk p < 0.05). Limits of Agreement may be unreliable. Consider a non-parametric approach or data transformation."
                )
            }

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

        # REMOVED: .calculateAgreementStatus() - consolidated into .calculateLevelOfAgreement()
        # This method was replaced by adding "simple" mode to Level of Agreement feature
        # The simple mode provides the same 3-category classification (All Agreed, Majority Agreed, No Agreement)
        # that this method used to provide, but within the more flexible LoA framework

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
            if (self$options$consensusVar) {
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

            if (self$options$loaVariable) {
                var_name <- self$options$loaVariableName
                detail_mode <- self$options$detailLevel

                if (detail_mode == "simple") {
                    threshold <- self$options$simpleThreshold
                    info_html <- paste0(info_html,
                        "<li><strong>", var_name, "</strong>: Case Agreement (Simple mode - threshold: ", threshold, "%)</li>")
                } else {
                    method_text <- switch(self$options$loaThresholds,
                        "custom" = paste0("Custom (High: ", self$options$loaHighThreshold, "%, Low: ", self$options$loaLowThreshold, "%)"),
                        "quartiles" = "Quartile-based (data-driven)",
                        "tertiles" = "Tertile-based (data-driven)"
                    )
                    info_html <- paste0(info_html,
                        "<li><strong>", var_name, "</strong>: Case Agreement (Detailed mode - ", method_text, ")</li>")
                }
            }

            info_html <- paste0(info_html, "</ul>")
            info_html <- paste0(info_html,
                "<p style='color: #666; font-size: 12px;'>",
                "These variables are now available in your dataset for further analysis.",
                "</p></div>")

            self$results$computedVariablesInfo$setContent(info_html)
        },

        .calculateLevelOfAgreement = function(ratings) {
            # Calculate case agreement categorization
            # Supports two modes:
            #   - Simple: 3 categories (All Agreed, Majority Agreed, No Agreement)
            #   - Detailed: 5 categories (Absolute, High, Moderate, Low, Poor)

            n_cases <- nrow(ratings)
            n_raters <- ncol(ratings)
            detail_mode <- self$options$detailLevel

            # Initialize result vectors
            loa_categories <- character(n_cases)
            agreement_pcts <- numeric(n_cases)
            modal_ratings <- character(n_cases)
            n_agreeing_vec <- integer(n_cases)

            # Calculate agreement proportion for each case
            for (i in 1:n_cases) {
                row_ratings <- unlist(ratings[i, ])
                row_ratings <- row_ratings[!is.na(row_ratings)]

                if (length(row_ratings) == 0) {
                    loa_categories[i] <- NA
                    agreement_pcts[i] <- NA
                    modal_ratings[i] <- NA
                    n_agreeing_vec[i] <- NA
                    next
                }

                # Find mode and frequency
                freq_table <- table(row_ratings)
                max_freq <- max(freq_table)
                modes <- names(freq_table)[freq_table == max_freq]

                # Calculate agreement proportion
                agree_pct <- max_freq / length(row_ratings)
                agreement_pcts[i] <- agree_pct
                n_agreeing_vec[i] <- max_freq
                modal_ratings[i] <- modes[1]  # Use first mode if tie
            }

            # Categorize based on detail mode
            if (detail_mode == "simple") {
                # Simple mode: 3 categories (replaces old Agreement Status feature)
                threshold <- self$options$simpleThreshold / 100

                for (i in 1:n_cases) {
                    if (is.na(agreement_pcts[i])) {
                        loa_categories[i] <- NA
                    } else if (agreement_pcts[i] == 1.0) {
                        loa_categories[i] <- "All Agreed"
                    } else if (agreement_pcts[i] >= threshold) {
                        loa_categories[i] <- "Majority Agreed"
                    } else {
                        loa_categories[i] <- "No Agreement"
                    }
                }

                # Populate distribution table with simple categories
                if (self$options$showLoaTable) {
                    loa_table <- self$results$loaTable
                    category_counts <- table(loa_categories, useNA = "no")
                    category_order <- c("All Agreed", "Majority Agreed", "No Agreement")

                    for (category in category_order) {
                        if (category %in% names(category_counts)) {
                            count <- as.integer(category_counts[category])
                            pct <- count / n_cases

                            # Interpretation text for simple mode
                            interpretation <- switch(category,
                                "All Agreed" = "Complete consensus (100%) across all raters",
                                "Majority Agreed" = sprintf("≥%.0f%% of raters agree on the same rating", self$options$simpleThreshold),
                                "No Agreement" = sprintf("<%.0f%% agreement - review recommended", self$options$simpleThreshold)
                            )

                            loa_table$addRow(rowKey = category, values = list(
                                loa_category = category,
                                count = count,
                                percentage = pct,
                                interpretation = interpretation
                            ))
                        }
                    }
                }

            } else {
                # Detailed mode: 5 categories (original LoA implementation)
                method <- self$options$loaThresholds

                # Determine thresholds based on method
                if (method == "custom") {
                    high_threshold <- self$options$loaHighThreshold / 100
                    low_threshold <- self$options$loaLowThreshold / 100
                    moderate_threshold <- (high_threshold + low_threshold) / 2
                } else if (method == "quartiles") {
                    # Data-driven quartile approach
                    valid_pcts <- agreement_pcts[!is.na(agreement_pcts)]
                    quartiles <- quantile(valid_pcts, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
                    low_threshold <- quartiles[1]
                    moderate_threshold <- quartiles[2]
                    high_threshold <- quartiles[3]
                } else if (method == "tertiles") {
                    # Data-driven tertile approach
                    valid_pcts <- agreement_pcts[!is.na(agreement_pcts)]
                    tertiles <- quantile(valid_pcts, probs = c(1/3, 2/3), na.rm = TRUE)
                    low_threshold <- tertiles[1]
                    moderate_threshold <- tertiles[1]  # Same as low for tertiles
                    high_threshold <- tertiles[2]
                }

                # Categorize each case with detailed thresholds
                for (i in 1:n_cases) {
                    if (is.na(agreement_pcts[i])) {
                        loa_categories[i] <- NA
                    } else if (agreement_pcts[i] == 1.0) {
                        loa_categories[i] <- "Absolute"
                    } else if (agreement_pcts[i] >= high_threshold) {
                        loa_categories[i] <- "High"
                    } else if (agreement_pcts[i] >= moderate_threshold) {
                        loa_categories[i] <- "Moderate"
                    } else if (agreement_pcts[i] >= low_threshold) {
                        loa_categories[i] <- "Low"
                    } else {
                        loa_categories[i] <- "Poor"
                    }
                }

                # Populate distribution table with detailed categories
                if (self$options$showLoaTable) {
                    loa_table <- self$results$loaTable
                    category_counts <- table(loa_categories, useNA = "no")
                    category_order <- c("Absolute", "High", "Moderate", "Low", "Poor")

                    for (category in category_order) {
                        if (category %in% names(category_counts)) {
                            count <- as.integer(category_counts[category])
                            pct <- count / n_cases

                            # Interpretation text for detailed mode
                            interpretation <- switch(category,
                                "Absolute" = "Perfect agreement (100%) - all raters concur",
                                "High" = "Strong agreement - most raters concur, minimal disagreement",
                                "Moderate" = "Moderate agreement - majority concur, some disagreement",
                                "Low" = "Weak agreement - considerable disagreement present",
                                "Poor" = "Poor agreement - substantial disagreement, difficult case"
                            )

                            loa_table$addRow(rowKey = category, values = list(
                                loa_category = category,
                                count = count,
                                percentage = pct,
                                interpretation = interpretation
                            ))
                        }
                    }
                }
            }

            # Populate case-level detail table (same for both modes)
            detail_table <- self$results$loaDetailTable
            for (i in 1:n_cases) {
                detail_table$addRow(rowKey = i, values = list(
                    case_id = i,
                    loa_category = if (!is.na(loa_categories[i])) loa_categories[i] else "N/A",
                    agreement_pct = agreement_pcts[i],
                    modal_rating = if (!is.na(modal_ratings[i])) modal_ratings[i] else "N/A",
                    n_agreeing = n_agreeing_vec[i]
                ))
            }

            # Add LoA variable to dataset
            if (self$options$loaVariable) {
                self$results$loaOutput$setRowNums(1:n_cases)
                self$results$loaOutput$setValues(loa_categories)
            }

            # Update computed variables info
            if (self$options$consensusVar || self$options$loaVariable) {
                private$.updateComputedVariablesInfo()
            }
        },

        .calculateHierarchicalKappa = function(ratings, cluster_data) {
            # Hierarchical/Multilevel Kappa Analysis
            # Uses lme4 mixed-effects models for variance decomposition

            # --- Validate cluster variable ---
            if (is.null(cluster_data) || ncol(cluster_data) == 0) {
                self$results$hierarchicalOverallTable$setNote(
                    "error",
                    "Please select a cluster/institution variable to perform hierarchical analysis."
                )
                return()
            }

            cluster_vec <- cluster_data[[1]]
            n_cases <- nrow(ratings)
            rater_names <- colnames(ratings)
            n_raters <- length(rater_names)

            # Check cluster has >1 level
            cluster_levels <- unique(na.omit(cluster_vec))
            n_clusters <- length(cluster_levels)

            if (n_clusters < 2) {
                self$results$hierarchicalOverallTable$setNote(
                    "error",
                    "Cluster variable must have at least 2 distinct levels for hierarchical analysis."
                )
                return()
            }

            # --- Reshape to long format ---
            long_df <- data.frame(
                case_id = rep(seq_len(n_cases), times = n_raters),
                rater   = rep(rater_names, each = n_cases),
                cluster = rep(cluster_vec, times = n_raters),
                score   = unlist(ratings, use.names = FALSE),
                stringsAsFactors = FALSE
            )
            long_df <- long_df[complete.cases(long_df), ]
            long_df$case_id <- factor(long_df$case_id)
            long_df$rater   <- factor(long_df$rater)
            long_df$cluster <- factor(long_df$cluster)

            if (nrow(long_df) < 10) {
                self$results$hierarchicalOverallTable$setNote(
                    "error",
                    "Insufficient complete observations for hierarchical analysis after removing missing data."
                )
                return()
            }

            # Mixed-effects decomposition below is valid for continuous numeric scores
            if (!all(sapply(ratings, is.numeric))) {
                self$results$hierarchicalOverallTable$setNote(
                    "error",
                    "Hierarchical mixed-effects decomposition currently requires continuous numeric ratings. For categorical ratings, use standard/cluster-specific kappa analyses."
                )
                return()
            }

            # --- Fit mixed-effects model ---
            model <- NULL
            model_fit_ok <- FALSE

            tryCatch({
                model <- lme4::lmer(
                    score ~ 1 + (1 | case_id) + (1 | rater) + (1 | cluster),
                    data = long_df,
                    control = lme4::lmerControl(
                        optimizer = "bobyqa",
                        calc.derivs = FALSE
                    )
                )
                model_fit_ok <- TRUE
            }, error = function(e) {
                # Try simpler model without rater effect
                tryCatch({
                    model <<- lme4::lmer(
                        score ~ 1 + (1 | case_id) + (1 | cluster),
                        data = long_df,
                        control = lme4::lmerControl(
                            optimizer = "bobyqa",
                            calc.derivs = FALSE
                        )
                    )
                    model_fit_ok <<- TRUE
                    self$results$hierarchicalOverallTable$setNote(
                        "model_note",
                        "Rater random effect was singular; a reduced model (case + cluster) was fit."
                    )
                }, error = function(e2) {
                    self$results$hierarchicalOverallTable$setNote(
                        "error",
                        paste0("Mixed model fitting failed: ", e2$message,
                               ". Data may have insufficient variability.")
                    )
                })
            })

            if (!model_fit_ok || is.null(model)) return()

            # --- Extract variance components ---
            vc <- as.data.frame(lme4::VarCorr(model))
            sigma2_case <- if ("case_id" %in% vc$grp) vc[vc$grp == "case_id", "vcov"] else 0
            sigma2_rater <- if ("rater" %in% vc$grp) vc[vc$grp == "rater", "vcov"] else 0
            sigma2_cluster <- if ("cluster" %in% vc$grp) vc[vc$grp == "cluster", "vcov"] else 0
            sigma2_resid <- sigma(model)^2
            sigma2_total <- sigma2_case + sigma2_rater + sigma2_cluster + sigma2_resid

            if (sigma2_total < 1e-12) {
                self$results$hierarchicalOverallTable$setNote(
                    "error",
                    "Total variance is effectively zero; all scores appear identical."
                )
                return()
            }

            # --- Step 1: Overall hierarchical agreement table ---
            icc1 <- sigma2_case / sigma2_total

            # CI for this variance-ratio estimate is not directly available from intercept CIs
            ci_lower <- NA
            ci_upper <- NA

            self$results$hierarchicalOverallTable$setRow(rowNo = 1, values = list(
                method = "Mixed-Effects ICC (hierarchical agreement proxy)",
                cases = n_cases,
                raters = n_raters,
                clusters = n_clusters,
                overall_kappa = icc1,
                ci_lower = ci_lower,
                ci_upper = ci_upper
            ))

            self$results$hierarchicalOverallTable$setNote(
                "model",
                paste0("Model: score ~ 1 + (1|case) + (1|rater) + (1|cluster); ",
                       nrow(long_df), " observations; ",
                       "ICC(1) = case variance / total variance. ",
                       "95% CI is not shown because intercept CIs are not valid for this variance ratio.")
            )

            # Warn about small clusters
            cluster_sizes <- table(cluster_vec[!is.na(cluster_vec)])
            small_clusters <- names(cluster_sizes[cluster_sizes < 3])
            if (length(small_clusters) > 0) {
                self$results$hierarchicalOverallTable$setNote(
                    "small_clusters",
                    paste0("Warning: ", length(small_clusters),
                           " cluster(s) have fewer than 3 cases. Estimates may be unstable: ",
                           paste(small_clusters, collapse = ", "))
                )
            }

            # --- Step 2: Variance decomposition table ---
            if (self$options$varianceDecomposition) {
                var_table <- self$results$varianceDecompositionTable

                interpret_component <- function(name, proportion) {
                    pct <- round(proportion * 100, 1)
                    if (name == "Case (Subject)") {
                        if (pct > 50) return("Scores driven by true case differences (desirable)")
                        if (pct > 20) return("Moderate case-level variation")
                        return("Low case-level variation; other sources dominate")
                    } else if (name == "Rater") {
                        if (pct > 30) return("Substantial rater bias; calibration needed")
                        if (pct > 10) return("Moderate rater effects")
                        return("Minimal rater bias")
                    } else if (name == "Cluster (Institution)") {
                        if (pct > 30) return("Large institutional differences; protocol harmonization needed")
                        if (pct > 10) return("Moderate cluster effects")
                        return("Minimal institutional variation")
                    } else {
                        if (pct > 50) return("High unexplained variability; consider additional factors")
                        if (pct > 30) return("Moderate residual noise")
                        return("Low residual noise (good)")
                    }
                }

                components <- list(
                    list(name = "Case (Subject)", var = sigma2_case),
                    list(name = "Rater", var = sigma2_rater),
                    list(name = "Cluster (Institution)", var = sigma2_cluster),
                    list(name = "Residual", var = sigma2_resid)
                )

                for (i in seq_along(components)) {
                    comp <- components[[i]]
                    prop <- comp$var / sigma2_total
                    var_table$addRow(rowKey = i, values = list(
                        component = comp$name,
                        variance = comp$var,
                        sd = sqrt(comp$var),
                        proportion = prop,
                        interpretation = interpret_component(comp$name, prop)
                    ))
                }

                # Total row
                var_table$addRow(rowKey = length(components) + 1, values = list(
                    component = "Total",
                    variance = sigma2_total,
                    sd = sqrt(sigma2_total),
                    proportion = 1.0,
                    interpretation = ""
                ))
            }

            # --- Step 3: Hierarchical ICC decomposition ---
            if (self$options$iccHierarchical) {
                icc_table <- self$results$hierarchicalICCTable

                # ICC(1): Single rating reliability
                icc1_val <- sigma2_case / sigma2_total

                # ICC(2): Reliability of mean ratings (averaged over k raters), including cluster variance
                icc2_val <- sigma2_case / (sigma2_case + sigma2_cluster / n_clusters + sigma2_rater / n_raters + sigma2_resid / n_raters)

                # G-coefficient: Universe score variance / expected observed variance
                g_coeff <- sigma2_case / (sigma2_case + sigma2_rater / n_raters +
                    sigma2_cluster / n_clusters + sigma2_resid / (n_raters * n_clusters))

                interpret_icc <- function(val) {
                    if (is.na(val) || !is.finite(val)) return("Not estimable")
                    if (val >= 0.9) return("Excellent reliability")
                    if (val >= 0.75) return("Good reliability")
                    if (val >= 0.5) return("Moderate reliability")
                    if (val >= 0.0) return("Poor reliability")
                    return("Negative (problematic)")
                }

                icc_rows <- list(
                    list(type = "ICC(1) - Single Rating",
                         val = icc1_val),
                    list(type = "ICC(2) - Mean of k Ratings",
                         val = icc2_val),
                    list(type = "G-coefficient",
                         val = g_coeff)
                )

                for (i in seq_along(icc_rows)) {
                    row <- icc_rows[[i]]
                    icc_table$addRow(rowKey = i, values = list(
                        icc_type = row$type,
                        icc_value = row$val,
                        ci_lower = NA,
                        ci_upper = NA,
                        interpretation = interpret_icc(row$val)
                    ))
                }

                icc_table$setNote(
                    "info",
                    paste0("ICC(1): reliability of a single rating; ",
                           "ICC(2): reliability of the mean of ", n_raters, " raters; ",
                           "G-coefficient: generalizability across raters and clusters (",
                           n_raters, " raters, ", n_clusters, " clusters).")
                )
            }

            # --- Step 4: Cluster-specific kappa estimates ---
            if (self$options$clusterSpecificKappa) {
                cluster_table <- self$results$clusterSpecificTable

                cluster_kappas <- list()
                cluster_labels <- levels(long_df$cluster)

                for (cl in cluster_labels) {
                    cl_mask <- cluster_vec == cl & !is.na(cluster_vec)
                    cl_ratings <- ratings[cl_mask, , drop = FALSE]
                    cl_ratings <- cl_ratings[complete.cases(cl_ratings), , drop = FALSE]
                    n_cl <- nrow(cl_ratings)

                    if (n_cl < 2) {
                        cluster_kappas[[cl]] <- list(
                            kappa = NA, ci_lower = NA, ci_upper = NA, n = n_cl
                        )
                        next
                    }

                    kappa_val <- NA
                    kappa_ci <- c(NA, NA)

                    tryCatch({
                        icc_res <- irr::icc(cl_ratings, model = "twoway", type = "agreement", unit = "single")
                        kappa_val <- icc_res$value
                        kappa_ci <- c(icc_res$lbound, icc_res$ubound)
                    }, error = function(e) {
                        # Leave as NA when cluster-specific ICC is not estimable
                    })

                    cluster_kappas[[cl]] <- list(
                        kappa = kappa_val, ci_lower = kappa_ci[1],
                        ci_upper = kappa_ci[2], n = n_cl
                    )
                }

                # Compute shrinkage estimates from random effects (BLUPs)
                shrinkage_values <- rep(NA, n_clusters)
                if (self$options$shrinkageEstimates && "cluster" %in% names(lme4::ranef(model))) {
                    re_cluster <- lme4::ranef(model)$cluster
                    for (j in seq_along(cluster_labels)) {
                        cl <- cluster_labels[j]
                        if (cl %in% rownames(re_cluster)) {
                            raw_k <- cluster_kappas[[cl]]$kappa
                            if (!is.na(raw_k)) {
                                cl_n <- cluster_kappas[[cl]]$n
                                shrinkage_factor <- cl_n / (cl_n + n_raters)
                                shrinkage_values[j] <- icc1 * (1 - shrinkage_factor) + raw_k * shrinkage_factor
                            }
                        }
                    }
                }

                # Rank clusters by kappa (if requested)
                kappa_vals <- sapply(cluster_kappas, function(x) x$kappa)
                ranks <- if (self$options$clusterRankings) {
                    rank(-kappa_vals, na.last = "keep", ties.method = "min")
                } else {
                    rep(NA, length(kappa_vals))
                }

                for (j in seq_along(cluster_labels)) {
                    cl <- cluster_labels[j]
                    ck <- cluster_kappas[[cl]]
                    cluster_table$addRow(rowKey = j, values = list(
                        cluster = cl,
                        n_cases = ck$n,
                        n_raters = n_raters,
                        kappa = ck$kappa,
                        ci_lower = ck$ci_lower,
                        ci_upper = ck$ci_upper,
                        shrinkage_kappa = shrinkage_values[j],
                        rank = if (self$options$clusterRankings && !is.na(ranks[j])) as.integer(ranks[j]) else NA
                    ))
                }
            }

            # --- Step 5: Homogeneity test ---
            if (self$options$testClusterHomogeneity) {
                homo_table <- self$results$homogeneityTestTable

                tryCatch({
                    # Likelihood ratio test: full model vs model without cluster
                    model_reduced <- lme4::lmer(
                        score ~ 1 + (1 | case_id) + (1 | rater),
                        data = long_df,
                        REML = FALSE,
                        control = lme4::lmerControl(
                            optimizer = "bobyqa",
                            calc.derivs = FALSE
                        )
                    )
                    model_full_ml <- stats::update(model, REML = FALSE)

                    lr_test <- stats::anova(model_reduced, model_full_ml)

                    chi_sq <- lr_test[["Chisq"]][2]
                    df_val <- lr_test[["Df"]][2]
                    p_val <- lr_test[["Pr(>Chisq)"]][2]

                    if (is.null(chi_sq) || is.na(chi_sq)) {
                        chi_sq <- as.numeric(lr_test[2, "Chisq"])
                        df_val <- as.numeric(lr_test[2, "Df"])
                        p_val <- as.numeric(lr_test[2, "Pr(>Chisq)"])
                    }

                    conclusion <- if (!is.na(p_val) && p_val < 0.05) {
                        "Significant heterogeneity across clusters (p < 0.05); agreement differs by institution."
                    } else if (!is.na(p_val)) {
                        "No significant heterogeneity detected; agreement is similar across clusters."
                    } else {
                        "Test result inconclusive."
                    }

                    homo_table$setRow(rowNo = 1, values = list(
                        test_name = "Likelihood Ratio Test (cluster effect)",
                        statistic = if (!is.na(chi_sq)) chi_sq else NA,
                        df = if (!is.na(df_val)) as.integer(df_val) else NA,
                        p_value = if (!is.na(p_val)) p_val else NA,
                        conclusion = conclusion
                    ))

                    homo_table$setNote(
                        "info",
                        "Compares full model (with cluster random effect) to reduced model (without). Significant p-value indicates heterogeneous agreement across clusters."
                    )
                }, error = function(e) {
                    homo_table$setRow(rowNo = 1, values = list(
                        test_name = "Likelihood Ratio Test",
                        statistic = NA,
                        df = NA,
                        p_value = NA,
                        conclusion = paste0("Test could not be computed: ", e$message)
                    ))
                })
            }
        },

        .populateHierarchicalExplanation = function() {
            # Generate comprehensive HTML explanation for hierarchical kappa

            html <- "<div style='font-family: Arial, sans-serif; padding: 15px; line-height: 1.6;'>"

            html <- paste0(html, "
                <h3 style='color: #333; margin-top: 0;'>Hierarchical/Multilevel Kappa Analysis</h3>

                <div style='background-color: #f9f9f9; padding: 12px; border-left: 4px solid #333; margin-bottom: 15px;'>
                    <strong>What is it?</strong><br/>
                    Hierarchical kappa extends standard kappa to account for nested data structures where raters
                    are grouped within clusters (institutions, centers, scanners). It decomposes agreement into
                    between-cluster and within-cluster components, providing more accurate estimates when clustering exists.
                </div>

                <h4 style='color: #333; margin-top: 20px;'>When to Use</h4>
                <ul>
                    <li><strong>Multi-center trials</strong>: Pathologists nested within hospitals</li>
                    <li><strong>Multi-scanner studies</strong>: Radiologists nested within imaging centers</li>
                    <li><strong>Training programs</strong>: Residents nested within training sites</li>
                    <li><strong>Quality control</strong>: Identifying institutions with poor agreement</li>
                </ul>

                <h4 style='color: #333; margin-top: 20px;'>Key Components</h4>
                <ul>
                    <li><strong>Overall Hierarchical Kappa</strong>: Population-level agreement accounting for clustering</li>
                    <li><strong>Cluster-Specific Estimates</strong>: Kappa for each institution/center</li>
                    <li><strong>Variance Decomposition</strong>: Between-cluster vs within-cluster variance</li>
                    <li><strong>Homogeneity Testing</strong>: Are all clusters performing equally?</li>
                    <li><strong>Shrinkage Estimates</strong>: Stabilized estimates for small clusters</li>
                </ul>

                <h4 style='color: #333; margin-top: 20px;'>Interpreting Variance Components</h4>
                <ul>
                    <li><strong>High between-cluster variance</strong>: Institutional differences (protocols, training)</li>
                    <li><strong>High within-cluster variance</strong>: Local rater disagreement</li>
                    <li><strong>Shrinkage</strong>: Pulls extreme cluster estimates toward overall mean</li>
                </ul>

                <div style='background-color: #f9f9f9; padding: 12px; border-left: 4px solid #333; margin-top: 15px;'>
                    <strong>Note:</strong> Full hierarchical analysis is computationally intensive and requires sufficient
                    data within each cluster (typically ≥10 cases per cluster recommended).
                </div>
            </div>")

            self$results$hierarchicalExplanation$setContent(html)
        },

        .calculateMixedEffectsComparison = function(ratings, condition_data, mydata) {
            # Mixed-Effects Condition Comparison
            # Model: score ~ condition + (1|rater_id) + (1|case_id)

            if (is.null(condition_data) || ncol(condition_data) == 0) {
                self$results$mixedEffectsTable$setNote(
                    "error",
                    "Please select a condition variable (e.g., AI vs Manual) for mixed-effects comparison."
                )
                return()
            }

            condition_vec <- condition_data[[1]]
            condition_levels <- unique(na.omit(condition_vec))
            n_conditions <- length(condition_levels)

            if (n_conditions < 2) {
                self$results$mixedEffectsTable$setNote(
                    "error",
                    "Condition variable must have at least 2 levels (e.g., 'AI' and 'Manual')."
                )
                return()
            }

            n_cases <- nrow(ratings)
            rater_names <- colnames(ratings)
            n_raters <- length(rater_names)

            long_df <- data.frame(
                case_id   = rep(seq_len(n_cases), times = n_raters),
                rater     = rep(rater_names, each = n_cases),
                condition = rep(condition_vec, times = n_raters),
                score     = unlist(ratings, use.names = FALSE),
                stringsAsFactors = FALSE
            )
            long_df <- long_df[complete.cases(long_df), ]
            long_df$case_id   <- factor(long_df$case_id)
            long_df$rater     <- factor(long_df$rater)
            long_df$condition <- factor(long_df$condition)

            if (nrow(long_df) < 10) {
                self$results$mixedEffectsTable$setNote(
                    "error",
                    "Insufficient complete observations for mixed-effects analysis."
                )
                return()
            }

            if (!is.numeric(long_df$score)) {
                long_df$score <- as.numeric(factor(long_df$score))
            }

            model <- NULL
            model_ok <- FALSE

            tryCatch({
                model <- lme4::lmer(
                    score ~ condition + (1 | case_id) + (1 | rater),
                    data = long_df,
                    control = lme4::lmerControl(
                        optimizer = "bobyqa",
                        calc.derivs = FALSE
                    )
                )
                model_ok <- TRUE
            }, error = function(e) {
                tryCatch({
                    model <<- lme4::lmer(
                        score ~ condition + (1 | case_id),
                        data = long_df,
                        control = lme4::lmerControl(
                            optimizer = "bobyqa",
                            calc.derivs = FALSE
                        )
                    )
                    model_ok <<- TRUE
                    self$results$mixedEffectsTable$setNote(
                        "model_note",
                        "Rater random effect was singular; reduced model (case only) was fit."
                    )
                }, error = function(e2) {
                    self$results$mixedEffectsTable$setNote(
                        "error",
                        paste0("Model fitting failed: ", e2$message)
                    )
                })
            })

            if (!model_ok || is.null(model)) return()

            me_table <- self$results$mixedEffectsTable
            coefs <- summary(model)$coefficients

            ci_vals <- tryCatch(
                confint(model, parm = "beta_", method = "profile"),
                error = function(e) {
                    tryCatch(
                        confint(model, parm = "beta_", method = "Wald"),
                        error = function(e2) NULL
                    )
                }
            )

            # Get lmerTest model once for efficiency
            lmerTest_model <- NULL
            lmerTest_coefs <- NULL
            tryCatch({
                if (requireNamespace("lmerTest", quietly = TRUE)) {
                    lmerTest_model <- lmerTest::as_lmerModLmerTest(model)
                    lmerTest_coefs <- summary(lmerTest_model)$coefficients
                }
            }, error = function(e) {
                # lmerTest conversion may fail; safe to ignore — fallback uses lm p-values
            })

            term_names <- rownames(coefs)
            raw_p <- numeric(nrow(coefs))

            for (i in seq_len(nrow(coefs))) {
                term <- term_names[i]
                display_term <- gsub("^condition", "Condition: ", term)
                if (term == "(Intercept)") display_term <- paste0("Intercept (ref: ", levels(long_df$condition)[1], ")")

                ci_lo <- if (!is.null(ci_vals) && i <= nrow(ci_vals)) ci_vals[i, 1] else NA
                ci_hi <- if (!is.null(ci_vals) && i <= nrow(ci_vals)) ci_vals[i, 2] else NA

                df_val <- if (!is.null(lmerTest_coefs)) lmerTest_coefs[i, "df"] else NA
                p_val <- if (!is.null(lmerTest_coefs)) {
                    lmerTest_coefs[i, "Pr(>|t|)"]
                } else {
                    2 * pnorm(abs(coefs[i, "t value"]), lower.tail = FALSE)
                }
                raw_p[i] <- p_val

                me_table$addRow(rowKey = i, values = list(
                    term = display_term,
                    estimate = coefs[i, "Estimate"],
                    se = coefs[i, "Std. Error"],
                    ci_lower = ci_lo,
                    ci_upper = ci_hi,
                    t_value = coefs[i, "t value"],
                    df = if (!is.na(df_val)) df_val else NA,
                    p_value = p_val
                ))
            }

            # Apply multiplicity correction if requested
            if (self$options$multipleTestCorrection != "none" && nrow(coefs) > 1) {
                adj_p <- p.adjust(raw_p, method = self$options$multipleTestCorrection)
                # Update table with adjusted p-values
                for (i in seq_len(nrow(coefs))) {
                    me_table$setCell(rowKey = i, col = "p_value", value = adj_p[i])
                }
                me_table$setNote(
                    "correction",
                    paste0("P-values adjusted using ", self$options$multipleTestCorrection,
                           " correction for ", length(adj_p), " comparisons.")
                )
            }

            # Variance components
            var_table <- self$results$mixedEffectsVarianceTable
            vc <- as.data.frame(lme4::VarCorr(model))
            sigma2_case <- if ("case_id" %in% vc$grp) vc[vc$grp == "case_id", "vcov"] else 0
            sigma2_rater <- if ("rater" %in% vc$grp) vc[vc$grp == "rater", "vcov"] else 0
            sigma2_resid <- sigma(model)^2
            sigma2_total <- sigma2_case + sigma2_rater + sigma2_resid

            if (sigma2_total > 1e-12) {
                interpret_var <- function(name, prop) {
                    pct <- round(prop * 100, 1)
                    if (name == "Case (Subject)") {
                        if (pct > 50) return("Scores driven by true case differences (desirable)")
                        if (pct > 20) return("Moderate case-level variation")
                        return("Low case-level variation")
                    } else if (name == "Rater") {
                        if (pct > 30) return("Substantial rater bias; calibration needed")
                        if (pct > 10) return("Moderate rater effects")
                        return("Minimal rater bias")
                    } else {
                        if (pct > 50) return("High unexplained variability")
                        if (pct > 30) return("Moderate residual noise")
                        return("Low residual noise (good)")
                    }
                }

                components <- list(
                    list(name = "Case (Subject)", var = sigma2_case),
                    list(name = "Rater", var = sigma2_rater),
                    list(name = "Residual", var = sigma2_resid)
                )

                for (i in seq_along(components)) {
                    comp <- components[[i]]
                    prop <- comp$var / sigma2_total
                    var_table$addRow(rowKey = i, values = list(
                        component = comp$name,
                        variance = comp$var,
                        sd = sqrt(comp$var),
                        proportion = prop,
                        interpretation = interpret_var(comp$name, prop)
                    ))
                }
            }

            ref_level <- levels(long_df$condition)[1]
            me_table$setNote(
                "model",
                paste0("Model: score ~ condition + (1|case) + (1|rater); ",
                       nrow(long_df), " observations; ",
                       n_raters, " raters; ",
                       n_cases, " cases; ",
                       n_conditions, " conditions. ",
                       "Reference level: '", ref_level, "'.")
            )

        },

        .populateMixedEffectsExplanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; padding: 15px; line-height: 1.6;'>
                <h3 style='color: #333; margin-top: 0;'>Mixed-Effects Condition Comparison</h3>
                <div style='background-color: #f9f9f9; padding: 12px; border-left: 4px solid #333; margin-bottom: 15px;'>
                    <strong>What is it?</strong><br/>
                    A linear mixed-effects model that compares measurement conditions (e.g., AI-assisted vs.
                    conventional scoring) while properly accounting for the crossed structure of rater x case data.
                </div>
                <h4 style='color: #333;'>Model</h4>
                <p><code>score ~ condition + (1|rater) + (1|case)</code></p>
                <ul>
                    <li><strong>Fixed effect (condition)</strong>: Systematic difference between conditions</li>
                    <li><strong>Random effect (rater)</strong>: Accounts for systematic rater differences</li>
                    <li><strong>Random effect (case)</strong>: Accounts for case difficulty</li>
                </ul>
                <h4 style='color: #333;'>When to Use</h4>
                <ul>
                    <li>Comparing AI-assisted vs. conventional scoring</li>
                    <li>Pre-training vs. post-training assessment</li>
                    <li>Any crossed rater x case x condition design</li>
                </ul>
            </div>"
            self$results$mixedEffectsExplanation$setContent(html)
        },

        .calculateConfusionMatrix = function(ratings) {
            tryCatch({
                if (ncol(ratings) != 2) {
                    self$results$confusionMatrixTable$setNote(
                        "error",
                        .("Confusion matrix requires exactly 2 raters. Only the first 2 selected variables are used.")
                    )
                }
                r1 <- ratings[[1]]
                r2 <- ratings[[2]]
                complete <- complete.cases(r1, r2)
                r1 <- r1[complete]
                r2 <- r2[complete]

                if (length(r1) < 2) {
                    self$results$confusionMatrixTable$setNote("insufficient", "Insufficient complete cases for confusion matrix.")
                    return()
                }

                all_levels <- sort(unique(c(as.character(r1), as.character(r2))))
                r1_f <- factor(r1, levels = all_levels)
                r2_f <- factor(r2, levels = all_levels)
                cm <- table(Reference = r1_f, Predicted = r2_f)

                norm_type <- self$options$confusionNormalize

                table <- self$results$confusionMatrixTable
                for (i in seq_along(all_levels)) {
                    for (j in seq_along(all_levels)) {
                        count_val <- as.integer(cm[i, j])
                        prop_val <- if (norm_type == "row" && sum(cm[i, ]) > 0) {
                            cm[i, j] / sum(cm[i, ])
                        } else if (norm_type == "column" && sum(cm[, j]) > 0) {
                            cm[i, j] / sum(cm[, j])
                        } else {
                            cm[i, j] / sum(cm)
                        }
                        table$addRow(rowKey = paste0(i, "_", j), values = list(
                            reference_class = all_levels[i],
                            predicted_class = all_levels[j],
                            count = count_val,
                            proportion = prop_val
                        ))
                    }
                }

                rater_names <- colnames(ratings)
                table$setNote("raters", paste0("Reference: ", rater_names[1], ", Predicted: ", rater_names[2], ". N = ", length(r1), " complete cases."))
                if (norm_type == "row") {
                    table$setNote("norm", "Proportions are row-normalized (recall per reference class).")
                } else if (norm_type == "column") {
                    table$setNote("norm", "Proportions are column-normalized (precision per predicted class).")
                }

                # Per-class metrics
                metrics_table <- self$results$perClassMetricsTable
                for (cls in all_levels) {
                    tp <- cm[cls, cls]
                    fp <- sum(cm[, cls]) - tp
                    fn <- sum(cm[cls, ]) - tp
                    support <- as.integer(sum(cm[cls, ]))
                    prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
                    rec  <- if ((tp + fn) > 0) tp / (tp + fn) else 0
                    f1   <- if ((prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0
                    interp <- if (f1 >= 0.9) "Excellent"
                              else if (f1 >= 0.8) "Good"
                              else if (f1 >= 0.7) "Moderate"
                              else if (f1 >= 0.5) "Fair"
                              else "Poor"
                    metrics_table$addRow(rowKey = cls, values = list(
                        class_label = cls, n = as.integer(tp), precision = prec,
                        recall = rec, f1 = f1, support = support, interpretation = interp
                    ))
                }

                # Macro-average
                all_prec <- sapply(all_levels, function(cls) { tp <- cm[cls,cls]; fp <- sum(cm[,cls])-tp; if ((tp+fp)>0) tp/(tp+fp) else 0 })
                all_rec <- sapply(all_levels, function(cls) { tp <- cm[cls,cls]; fn <- sum(cm[cls,])-tp; if ((tp+fn)>0) tp/(tp+fn) else 0 })
                all_f1 <- sapply(seq_along(all_levels), function(k) { p <- all_prec[k]; r <- all_rec[k]; if ((p+r)>0) 2*p*r/(p+r) else 0 })
                metrics_table$addRow(rowKey = "macro_avg", values = list(
                    class_label = "Macro Average", n = as.integer(sum(diag(cm))),
                    precision = mean(all_prec), recall = mean(all_rec), f1 = mean(all_f1),
                    support = as.integer(sum(cm)),
                    interpretation = paste0("Overall accuracy: ", round(sum(diag(cm))/sum(cm)*100, 1), "%")
                ))
            }, error = function(e) {
                self$results$confusionMatrixTable$setNote("error", paste("Confusion matrix error:", e$message))
            })
        },

        .populateConfusionMatrixExplanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>
                <h3>Multi-Class Confusion Matrix</h3>
                <p>Cross-tabulation of ratings from two raters (Reference vs. Predicted).</p>
                <h4>Per-Class Metrics</h4>
                <ul>
                    <li><strong>Precision (PPV)</strong>: TP / (TP + FP)</li>
                    <li><strong>Recall (Sensitivity)</strong>: TP / (TP + FN)</li>
                    <li><strong>F1 Score</strong>: Harmonic mean of precision and recall</li>
                </ul>
                <div style='background-color: #f9f9f9; padding: 12px; border-left: 4px solid #333; margin-top: 15px;'>
                    <strong>Clinical Use:</strong> In ordinal scoring (e.g., HER2 0/1+/2+/3+), the confusion
                    matrix reveals systematic misclassification patterns.
                </div>
            </div>"
            self$results$confusionMatrixExplanation$setContent(html)
        },

        .calculateBootstrapCI = function(ratings) {
            tryCatch({
                n_boot <- self$options$nBoot
                n_cases <- nrow(ratings)
                n_raters <- ncol(ratings)

                if (n_cases < 10) {
                    self$results$bootstrapCITable$setNote("insufficient", "At least 10 cases required for bootstrap CIs.")
                    return()
                }

                # Helper: compute agreement metrics
                compute_metrics <- function(boot_ratings) {
                    result <- list()
                    n_agree <- sum(apply(boot_ratings, 1, function(x) { x <- x[!is.na(x)]; length(unique(x)) == 1 }))
                    result$pct_agreement <- n_agree / nrow(boot_ratings)
                    is_categorical <- all(sapply(boot_ratings, function(x) is.factor(x) || is.character(x) || length(unique(na.omit(x))) <= 20))
                    if (is_categorical) {
                        char_ratings <- as.data.frame(lapply(boot_ratings, as.character), stringsAsFactors = FALSE)
                        if (n_raters == 2) {
                            tryCatch({ result$kappa <- irr::kappa2(char_ratings, weight = "unweighted")$value }, error = function(e) { result$kappa <<- NA })
                        } else {
                            tryCatch({ result$kappa <- irr::kappam.fleiss(char_ratings)$value }, error = function(e) { result$kappa <<- NA })
                        }
                        tryCatch({
                            ka_result <- irr::kripp.alpha(t(as.matrix(sapply(char_ratings, function(x) as.numeric(factor(x))))))
                            result$kripp_alpha <- ka_result$value
                        }, error = function(e) { result$kripp_alpha <<- NA })
                    } else {
                        tryCatch({
                            icc_result <- irr::icc(boot_ratings, model = "twoway", type = "agreement", unit = "single")
                            result$icc <- icc_result$value
                        }, error = function(e) { result$icc <<- NA })
                        result$kappa <- NA
                        result$kripp_alpha <- NA
                    }
                    return(result)
                }

                obs <- compute_metrics(ratings)

                set.seed(42)
                boot_results <- lapply(seq_len(n_boot), function(b) {
                    idx <- sample(n_cases, replace = TRUE)
                    compute_metrics(ratings[idx, , drop = FALSE])
                })

                boot_pct <- sapply(boot_results, function(x) x$pct_agreement)
                boot_kappa <- sapply(boot_results, function(x) x$kappa)
                boot_kripp <- sapply(boot_results, function(x) x$kripp_alpha)
                boot_icc <- if (!is.null(boot_results[[1]]$icc)) sapply(boot_results, function(x) x$icc) else NULL

                # BCa CI helper with explicit metric_name parameter
                conf <- self$options$confLevel
                bca_ci <- function(obs_val, boot_dist, metric_name, alpha = 1 - conf) {
                    boot_dist <- boot_dist[!is.na(boot_dist)]
                    if (length(boot_dist) < 50) return(list(lower = NA, upper = NA, se = NA, bias = NA, method = "N/A"))
                    boot_se <- sd(boot_dist)
                    boot_bias <- mean(boot_dist) - obs_val
                    z0 <- qnorm(mean(boot_dist < obs_val))
                    if (is.infinite(z0)) z0 <- 0

                    # Jackknife acceleration
                    jack_vals <- sapply(seq_len(min(n_cases, 200)), function(i) {
                        tryCatch({
                            jack_res <- compute_metrics(ratings[-i, , drop = FALSE])
                            val <- jack_res[[metric_name]]
                            if (is.null(val)) NA else val
                        }, error = function(e) obs_val)
                    })
                    jack_mean <- mean(jack_vals, na.rm = TRUE)
                    jack_diff <- jack_mean - jack_vals
                    denom_a <- 6 * sum(jack_diff^2, na.rm = TRUE)^1.5
                    a_hat <- if (abs(denom_a) < .Machine$double.eps) 0 else sum(jack_diff^3, na.rm = TRUE) / denom_a
                    if (is.na(a_hat) || is.infinite(a_hat)) a_hat <- 0

                    z_alpha <- qnorm(c(alpha/2, 1 - alpha/2))
                    denom_lower <- 1 - a_hat * (z0 + z_alpha[1])
                    denom_upper <- 1 - a_hat * (z0 + z_alpha[2])
                    if (abs(denom_lower) < 1e-10 || abs(denom_upper) < 1e-10) {
                        return(pct_ci(obs_val, boot_dist))
                    }
                    adj_lower <- pnorm(z0 + (z0 + z_alpha[1]) / denom_lower)
                    adj_upper <- pnorm(z0 + (z0 + z_alpha[2]) / denom_upper)
                    adj_lower <- max(adj_lower, 0.001)
                    adj_upper <- min(adj_upper, 0.999)
                    ci <- quantile(boot_dist, probs = c(adj_lower, adj_upper), na.rm = TRUE)
                    list(lower = ci[1], upper = ci[2], se = boot_se, bias = boot_bias, method = "BCa")
                }

                pct_ci <- function(obs_val, boot_dist) {
                    boot_dist <- boot_dist[!is.na(boot_dist)]
                    if (length(boot_dist) < 50) return(list(lower = NA, upper = NA, se = NA, bias = NA, method = "N/A"))
                    alpha <- 1 - conf
                    ci <- quantile(boot_dist, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
                    list(lower = ci[1], upper = ci[2], se = sd(boot_dist), bias = mean(boot_dist) - obs_val, method = "Percentile")
                }

                table <- self$results$bootstrapCITable

                ci_pct <- tryCatch(bca_ci(obs$pct_agreement, boot_pct, "pct_agreement"), error = function(e) pct_ci(obs$pct_agreement, boot_pct))
                table$addRow(rowKey = "pct_agreement", values = list(
                    metric = "Percent Agreement", estimate = obs$pct_agreement,
                    boot_se = ci_pct$se, ci_lower = ci_pct$lower, ci_upper = ci_pct$upper,
                    boot_bias = ci_pct$bias, ci_method = ci_pct$method
                ))

                if (!is.na(obs$kappa)) {
                    ci_kappa <- tryCatch(bca_ci(obs$kappa, boot_kappa, "kappa"), error = function(e) pct_ci(obs$kappa, boot_kappa))
                    kappa_label <- if (n_raters == 2) "Cohen's Kappa" else "Fleiss' Kappa"
                    table$addRow(rowKey = "kappa", values = list(
                        metric = kappa_label, estimate = obs$kappa,
                        boot_se = ci_kappa$se, ci_lower = ci_kappa$lower, ci_upper = ci_kappa$upper,
                        boot_bias = ci_kappa$bias, ci_method = ci_kappa$method
                    ))
                }

                if (!is.na(obs$kripp_alpha)) {
                    ci_kripp <- tryCatch(bca_ci(obs$kripp_alpha, boot_kripp, "kripp_alpha"), error = function(e) pct_ci(obs$kripp_alpha, boot_kripp))
                    table$addRow(rowKey = "kripp_alpha", values = list(
                        metric = "Krippendorff's Alpha", estimate = obs$kripp_alpha,
                        boot_se = ci_kripp$se, ci_lower = ci_kripp$lower, ci_upper = ci_kripp$upper,
                        boot_bias = ci_kripp$bias, ci_method = ci_kripp$method
                    ))
                }

                if (!is.null(boot_icc)) {
                    ci_icc <- tryCatch(bca_ci(obs$icc, boot_icc, "icc"), error = function(e) pct_ci(obs$icc, boot_icc))
                    table$addRow(rowKey = "icc", values = list(
                        metric = "ICC (two-way, agreement)", estimate = obs$icc,
                        boot_se = ci_icc$se, ci_lower = ci_icc$lower, ci_upper = ci_icc$upper,
                        boot_bias = ci_icc$bias, ci_method = ci_icc$method
                    ))
                }

                table$setNote("boot", paste0("Based on ", n_boot, " bootstrap resamples (case resampling). Seed: 42 for reproducibility."))
            }, error = function(e) {
                self$results$bootstrapCITable$setNote("error", paste("Bootstrap CI error:", e$message))
            })
        },

        .populateBootstrapCIExplanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>
                <h3>Bootstrap Confidence Intervals</h3>
                <p>Bootstrap CIs are computed by resampling cases with replacement and recomputing
                each agreement metric. The BCa (Bias-Corrected and Accelerated) method adjusts for
                both bias and skewness in the bootstrap distribution.</p>
                <h4>Interpreting Results</h4>
                <ul>
                    <li><strong>Boot SE</strong>: Standard deviation of bootstrap distribution</li>
                    <li><strong>95% CI</strong>: Range containing the true metric with 95% confidence</li>
                    <li><strong>Bias</strong>: Mean bootstrap estimate minus observed; large bias suggests instability</li>
                </ul>
                <div style='background-color: #f9f9f9; padding: 12px; border-left: 4px solid #333; margin-top: 15px;'>
                    <strong>Recommendation:</strong> Use bootstrap CIs when reporting agreement metrics in publications.
                    They are distribution-free and provide more robust coverage than analytical CIs.
                </div>
            </div>"
            self$results$bootstrapCIExplanation$setContent(html)
        },

        .calculateConcordanceF1 = function(ratings) {
            tryCatch({
                n_cases <- nrow(ratings)
                n_raters <- ncol(ratings)

                if (n_raters < 3) {
                    self$results$concordanceF1Table$setNote("insufficient", "At least 3 raters required for multi-annotator concordance.")
                    return()
                }

                pred_col <- self$options$predictionColumn
                if (pred_col > n_raters) {
                    self$results$concordanceF1Table$setNote(
                        "error",
                        sprintf(.("Prediction column %d exceeds number of raters (%d). Using last rater column instead."), pred_col, n_raters)
                    )
                    pred_col <- n_raters
                }
                ref_cols <- setdiff(seq_len(n_raters), pred_col)

                predictions <- ratings[[pred_col]]
                annotators <- ratings[, ref_cols, drop = FALSE]

                valid <- !is.na(predictions)
                predictions <- predictions[valid]
                annotators <- annotators[valid, , drop = FALSE]
                n_valid <- length(predictions)

                if (n_valid < 5) {
                    self$results$concordanceF1Table$setNote("insufficient", "Fewer than 5 valid cases for concordance analysis.")
                    return()
                }

                all_classes <- sort(unique(c(as.character(predictions), as.character(unlist(annotators)))))
                all_classes <- all_classes[!is.na(all_classes)]

                # Concordance: prediction matches ANY annotator
                concordance_match <- sapply(seq_len(n_valid), function(i) {
                    pred <- as.character(predictions[i])
                    refs <- as.character(unlist(annotators[i, ]))
                    refs <- refs[!is.na(refs)]
                    pred %in% refs
                })

                # Strict: prediction matches majority consensus
                consensus <- sapply(seq_len(n_valid), function(i) {
                    refs <- as.character(unlist(annotators[i, ]))
                    refs <- refs[!is.na(refs)]
                    if (length(refs) == 0) return(NA)
                    tab <- sort(table(refs), decreasing = TRUE)
                    names(tab)[1]
                })

                strict_match <- as.character(predictions) == consensus
                concordance_accuracy <- mean(concordance_match, na.rm = TRUE)
                strict_accuracy <- mean(strict_match, na.rm = TRUE)

                table <- self$results$concordanceF1Table
                table$addRow(rowKey = "conc_acc", values = list(
                    metric = "Concordance Accuracy", value = concordance_accuracy,
                    comparison = paste0("vs Strict: ", round(strict_accuracy, 4)),
                    interpretation = paste0(round((concordance_accuracy - strict_accuracy) * 100, 1), "% improvement over strict consensus")
                ))
                table$addRow(rowKey = "strict_acc", values = list(
                    metric = "Strict Accuracy (vs Consensus)", value = strict_accuracy,
                    comparison = "Majority consensus",
                    interpretation = if (strict_accuracy >= 0.9) "Excellent" else if (strict_accuracy >= 0.8) "Good" else if (strict_accuracy >= 0.7) "Moderate" else "Needs improvement"
                ))

                # Mean annotator agreement
                pairwise_agreements <- c()
                for (j in seq_along(ref_cols)) {
                    for (k in seq_along(ref_cols)) {
                        if (j < k) {
                            r_j <- as.character(annotators[[j]])
                            r_k <- as.character(annotators[[k]])
                            valid_pair <- !is.na(r_j) & !is.na(r_k)
                            if (sum(valid_pair) > 0) {
                                pairwise_agreements <- c(pairwise_agreements, mean(r_j[valid_pair] == r_k[valid_pair]))
                            }
                        }
                    }
                }
                if (length(pairwise_agreements) > 0) {
                    table$addRow(rowKey = "annotator_agree", values = list(
                        metric = "Mean Annotator Agreement", value = mean(pairwise_agreements),
                        comparison = paste0("Range: [", round(min(pairwise_agreements), 3), ", ", round(max(pairwise_agreements), 3), "]"),
                        interpretation = "Baseline inter-annotator agreement among references"
                    ))
                }
                table$addRow(rowKey = "n_info", values = list(
                    metric = "N Annotators (reference)", value = length(ref_cols),
                    comparison = paste0("Prediction: rater ", pred_col),
                    interpretation = paste0(n_valid, " cases evaluated")
                ))

                # Per-class concordance F1
                per_class_table <- self$results$concordanceF1PerClassTable
                for (cls in all_classes) {
                    conc_tp <- sum(as.character(predictions) == cls & concordance_match)
                    conc_fp <- sum(as.character(predictions) == cls & !concordance_match)
                    any_annotator_cls <- sapply(seq_len(n_valid), function(i) {
                        refs <- as.character(unlist(annotators[i, ]))
                        cls %in% refs[!is.na(refs)]
                    })
                    conc_fn <- sum(any_annotator_cls & as.character(predictions) != cls)
                    conc_prec <- if ((conc_tp + conc_fp) > 0) conc_tp / (conc_tp + conc_fp) else 0
                    conc_rec <- if ((conc_tp + conc_fn) > 0) conc_tp / (conc_tp + conc_fn) else 0
                    conc_f1 <- if ((conc_prec + conc_rec) > 0) 2 * conc_prec * conc_rec / (conc_prec + conc_rec) else 0

                    strict_tp <- sum(as.character(predictions) == cls & consensus == cls, na.rm = TRUE)
                    strict_fp <- sum(as.character(predictions) == cls & consensus != cls, na.rm = TRUE)
                    strict_fn <- sum(as.character(predictions) != cls & consensus == cls, na.rm = TRUE)
                    strict_prec <- if ((strict_tp + strict_fp) > 0) strict_tp / (strict_tp + strict_fp) else 0
                    strict_rec <- if ((strict_tp + strict_fn) > 0) strict_tp / (strict_tp + strict_fn) else 0
                    strict_f1 <- if ((strict_prec + strict_rec) > 0) 2 * strict_prec * strict_rec / (strict_prec + strict_rec) else 0
                    improvement <- if (strict_f1 > 0) (conc_f1 - strict_f1) / strict_f1 else 0

                    per_class_table$addRow(rowKey = cls, values = list(
                        class_label = cls, concordance_f1 = conc_f1, strict_f1 = strict_f1,
                        improvement = improvement, n_cases = as.integer(sum(any_annotator_cls))
                    ))
                }

                rater_names <- colnames(ratings)
                table$setNote("method", paste0("Prediction column: ", rater_names[pred_col],
                    ". Reference annotators: ", paste(rater_names[ref_cols], collapse = ", "), "."))
            }, error = function(e) {
                self$results$concordanceF1Table$setNote("error", paste("Concordance F1 error:", e$message))
            })
        },

        .populateConcordanceF1Explanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>
                <h3>Multi-Annotator Concordance</h3>
                <p>Evaluates predictions against all annotators simultaneously. A prediction is correct
                if it matches ANY reference annotator's label (concordance) or only the majority
                consensus (strict).</p>
                <div style='background-color: #f9f9f9; padding: 12px; border-left: 4px solid #333; margin-top: 15px;'>
                    <strong>Reference:</strong> Ottl et al. (2025) used concordance F1 for HER2 scoring
                    evaluation with multiple annotators.
                </div>
            </div>"
            self$results$concordanceF1Explanation$setContent(html)
        },

        .calculatePairedAgreementComparison = function(ratings_A) {
            tryCatch({
                condB_vars <- self$options$conditionBVars
                if (is.null(condB_vars) || length(condB_vars) < 2) {
                    self$results$pairedAgreementTable$setNote("error", "Select at least 2 rater variables for Condition B.")
                    return()
                }

                ratings_B <- data.frame(
                    lapply(condB_vars, function(v) as.character(self$data[[v]])),
                    stringsAsFactors = FALSE
                )
                colnames(ratings_B) <- condB_vars

                # Ensure type consistency: convert A to character as well
                ratings_A_char <- data.frame(
                    lapply(ratings_A, as.character),
                    stringsAsFactors = FALSE
                )
                colnames(ratings_A_char) <- colnames(ratings_A)

                complete <- complete.cases(ratings_A_char) & complete.cases(ratings_B)
                ratings_A <- ratings_A_char[complete, , drop = FALSE]
                ratings_B <- ratings_B[complete, , drop = FALSE]
                n_cases <- nrow(ratings_A)

                if (n_cases < 10) {
                    self$results$pairedAgreementTable$setNote("error", paste0("Too few complete cases (", n_cases, "). Need at least 10."))
                    return()
                }

                compute_pct_agree <- function(mat) {
                    n_raters <- ncol(mat)
                    if (n_raters < 2) return(NA)
                    pairs <- combn(n_raters, 2)
                    mean(apply(pairs, 2, function(p) mean(mat[, p[1]] == mat[, p[2]], na.rm = TRUE)))
                }

                compute_kappa <- function(mat) {
                    tryCatch({
                        if (ncol(mat) == 2) irr::kappa2(mat)$value else irr::kappam.fleiss(mat)$value
                    }, error = function(e) NA)
                }

                pct_A <- compute_pct_agree(ratings_A)
                pct_B <- compute_pct_agree(ratings_B)
                kappa_A <- compute_kappa(ratings_A)
                kappa_B <- compute_kappa(ratings_B)

                n_boot <- self$options$pairedBootN
                set.seed(42)
                boot_pct_diff <- numeric(n_boot)
                boot_kappa_diff <- numeric(n_boot)

                for (b in seq_len(n_boot)) {
                    idx <- sample(n_cases, replace = TRUE)
                    boot_A <- ratings_A[idx, , drop = FALSE]
                    boot_B <- ratings_B[idx, , drop = FALSE]
                    boot_pct_diff[b] <- compute_pct_agree(boot_B) - compute_pct_agree(boot_A)
                    boot_kappa_diff[b] <- compute_kappa(boot_B) - compute_kappa(boot_A)
                }

                table <- self$results$pairedAgreementTable

                pct_diff <- pct_B - pct_A
                pct_ci <- quantile(boot_pct_diff, c(0.025, 0.975), na.rm = TRUE)
                pct_p <- min(1, 2 * min(mean(boot_pct_diff <= 0, na.rm = TRUE), mean(boot_pct_diff >= 0, na.rm = TRUE)))
                pct_interp <- if (pct_p < 0.05) { if (pct_diff > 0) "Significant improvement" else "Significant decrease" } else "No significant difference"

                table$addRow(rowKey = "pct_agree", values = list(
                    metric = "Percent Agreement", condition_a = pct_A, condition_b = pct_B,
                    difference = pct_diff, ci_lower = pct_ci[1], ci_upper = pct_ci[2],
                    p_value = pct_p, interpretation = pct_interp
                ))

                if (!is.na(kappa_A) && !is.na(kappa_B)) {
                    kappa_diff <- kappa_B - kappa_A
                    kappa_ci <- quantile(boot_kappa_diff, c(0.025, 0.975), na.rm = TRUE)
                    kappa_p <- min(1, 2 * min(mean(boot_kappa_diff <= 0, na.rm = TRUE), mean(boot_kappa_diff >= 0, na.rm = TRUE)))
                    kappa_interp <- if (kappa_p < 0.05) { if (kappa_diff > 0) "Significant improvement" else "Significant decrease" } else "No significant difference"
                    kappa_label <- if (ncol(ratings_A) == 2) "Cohen's Kappa" else "Fleiss' Kappa"
                    table$addRow(rowKey = "kappa", values = list(
                        metric = kappa_label, condition_a = kappa_A, condition_b = kappa_B,
                        difference = kappa_diff, ci_lower = kappa_ci[1], ci_upper = kappa_ci[2],
                        p_value = kappa_p, interpretation = kappa_interp
                    ))
                }

                table$setNote("info", paste0("Bootstrap test with ", n_boot, " replications (seed = 42). N = ", n_cases, " cases."))
            }, error = function(e) {
                self$results$pairedAgreementTable$setNote("error", paste("Paired agreement error:", e$message))
            })
        },

        .populatePairedAgreementExplanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>
                <h3>Paired Agreement Comparison</h3>
                <p>Compares interobserver agreement between two conditions applied to the same cases
                (e.g., manual vs AI-assisted scoring). Uses case-bootstrap test.</p>
                <div style='background-color: #f9f9f9; padding: 12px; border-left: 4px solid #333; margin-top: 15px;'>
                    <strong>Study design note:</strong> Assumes a paired (crossover) design where the same
                    cases are scored under both conditions.
                </div>
            </div>"
            self$results$pairedAgreementExplanation$setContent(html)
        },

        .calculateAgreementSampleSize = function() {
            tryCatch({
                metric <- self$options$ssMetric
                k0 <- self$options$ssKappaNull
                k1 <- self$options$ssKappaAlt
                alpha <- self$options$ssAlpha
                power <- self$options$ssPower
                n_raters <- self$options$ssNRaters
                n_cat <- self$options$ssNCategories

                if (k1 <= k0) {
                    self$results$agreementSampleSizeTable$setNote("error", "Expected kappa (H1) must be greater than null kappa (H0).")
                    return()
                }

                z_alpha <- qnorm(1 - alpha / 2)
                z_beta <- qnorm(power)

                table <- self$results$agreementSampleSizeTable

                if (metric == "kappa" || metric == "fleiss") {
                    if (!requireNamespace("kappaSize", quietly = TRUE)) {
                        self$results$agreementSampleSizeTable$setNote(
                            "error",
                            "kappaSize package is required for validated kappa sample size calculations."
                        )
                        return()
                    }

                    if (metric == "kappa" && n_raters != 2) {
                        self$results$agreementSampleSizeTable$setNote(
                            "error",
                            "Cohen's kappa sample size requires exactly 2 raters. Use Fleiss' kappa for 3+ raters."
                        )
                        return()
                    }

                    if (metric == "fleiss" && n_raters < 3) {
                        self$results$agreementSampleSizeTable$setNote(
                            "error",
                            "Fleiss' kappa sample size requires at least 3 raters."
                        )
                        return()
                    }

                    if (n_cat < 2 || n_cat > 5) {
                        self$results$agreementSampleSizeTable$setNote(
                            "error",
                            "Validated kappa sample size formulas are currently available for 2 to 5 categories."
                        )
                        return()
                    }

                    props <- rep(1 / n_cat, n_cat)
                    n_est <- switch(
                        as.character(n_cat),
                        "2" = kappaSize::PowerBinary(kappa0 = k0, kappa1 = k1, props = props,
                                                     raters = n_raters, alpha = alpha, power = power)$N[1],
                        "3" = kappaSize::Power3Cats(kappa0 = k0, kappa1 = k1, props = props,
                                                    raters = n_raters, alpha = alpha, power = power)$N[1],
                        "4" = kappaSize::Power4Cats(kappa0 = k0, kappa1 = k1, props = props,
                                                    raters = n_raters, alpha = alpha, power = power)$N[1],
                        "5" = kappaSize::Power5Cats(kappa0 = k0, kappa1 = k1, props = props,
                                                    raters = n_raters, alpha = alpha, power = power)$N[1]
                    )

                    n_required <- max(ceiling(n_est), 2 * n_raters)
                    metric_label <- if (metric == "kappa") "Cohen's Kappa" else "Fleiss' Kappa"
                } else {
                    # ICC: Walter, Eliasziw & Donner (1998)
                    rho0 <- k0
                    rho1 <- k1
                    k <- n_raters
                    if (abs(rho1 - rho0) < 1e-6) {
                        self$results$agreementSampleSizeTable$setNote(
                            "error",
                            .("H0 and H1 ICC values are too close together. Increase the difference to get a valid sample size.")
                        )
                        return()
                    }
                    numerator <- 2 * (z_alpha + z_beta)^2 * (1 - rho1)^2 * (1 + (k - 1) * rho1)^2
                    denominator <- k * (k - 1) * (rho1 - rho0)^2
                    n_required <- ceiling(1 + numerator / denominator)
                    n_required <- max(n_required, k + 1)
                    metric_label <- "ICC"
                }

                table$addRow(rowKey = "metric", values = list(parameter = "Agreement Metric", value = metric_label))
                table$addRow(rowKey = "n_required", values = list(parameter = "Required Sample Size (subjects)", value = as.character(n_required)))
                table$addRow(rowKey = "n_raters", values = list(parameter = "Number of Raters", value = as.character(n_raters)))
                if (metric != "icc") {
                    table$addRow(rowKey = "n_cat", values = list(parameter = "Number of Categories", value = as.character(n_cat)))
                }
                table$addRow(rowKey = "kappa_null", values = list(parameter = paste0(metric_label, " under H0"), value = format(k0, digits = 3)))
                table$addRow(rowKey = "kappa_alt", values = list(parameter = paste0(metric_label, " under H1"), value = format(k1, digits = 3)))
                table$addRow(rowKey = "alpha", values = list(parameter = "Significance Level (alpha)", value = format(alpha, digits = 3)))
                table$addRow(rowKey = "power", values = list(parameter = "Target Power (1 - beta)", value = format(power, digits = 3)))
                table$addRow(rowKey = "total_reads", values = list(parameter = "Total Reads Required", value = as.character(n_required * n_raters)))

                table$setNote("info", paste0(
                    "Formula: ",
                    if (metric == "icc") "Walter, Eliasziw & Donner (1998)"
                    else "kappaSize implementation of Sim & Wright/Donner methods",
                    ". Assumes ",
                    if (metric != "icc") "equal marginal proportions across categories. "
                    else paste0(n_raters, " raters. "),
                    "Two-sided test."
                ))
            }, error = function(e) {
                self$results$agreementSampleSizeTable$setNote("error", paste("Sample size error:", e$message))
            })
        },

        .populateAgreementSampleSizeExplanation = function() {
            html <- "<div style='font-family: Arial, sans-serif; max-width: 700px; line-height: 1.5;'>
                <h3>Sample Size for Agreement Studies</h3>
                <p>Calculates required number of subjects for a prospective interrater agreement study.</p>
                <h4>Formulas</h4>
                <ul>
                    <li><strong>Kappa (2-5 categories)</strong>: kappaSize implementation of Sim & Wright/Donner methods</li>
                    <li><strong>ICC</strong>: Based on Walter, Eliasziw & Donner (1998)</li>
                </ul>
                <div style='background-color: #f9f9f9; padding: 12px; border-left: 4px solid #333; margin-top: 15px;'>
                    <strong>Note:</strong> Kappa sample sizes assume equal marginal proportions across categories.
                    Highly imbalanced categories generally require larger sample sizes.
                </div>
            </div>"
            self$results$agreementSampleSizeExplanation$setContent(html)
        },

        .populateContingencyTableExplanation = function() {
            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What are Contingency Tables &amp; Rating Combinations?</h3>
                    <p style='margin: 0; color: #333;'>
                        These tables show the <strong>raw frequency distribution</strong> of ratings across raters.
                        For 2 raters, a cross-tabulation (contingency table) is displayed. For 3+ raters,
                        all unique rating combinations and their frequencies are shown.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>How to Read the Table</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Diagonal cells</strong> (where Row = Column) represent <strong>agreement</strong> &mdash; both raters assigned the same category</li>
                        <li><strong>Off-diagonal cells</strong> represent <strong>disagreement</strong> &mdash; raters assigned different categories</li>
                        <li>A well-agreeing pair of raters will have most counts concentrated on the diagonal</li>
                        <li>Systematic patterns in off-diagonal cells may indicate <strong>rater bias</strong> (e.g., one rater consistently upgrading)</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Before computing agreement statistics</strong> &mdash; inspect the raw data to identify unexpected patterns</li>
                        <li><strong>When kappa is low</strong> &mdash; the contingency table reveals <em>where</em> disagreements occur</li>
                        <li><strong>To detect specific confusion pairs</strong> &mdash; which categories are most often confused?</li>
                        <li><strong>To check data completeness</strong> &mdash; are all expected categories represented?</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Example</h4>
                    <p style='margin: 0; font-style: italic;'>
                        In a breast cancer grading study, the contingency table might reveal that Grade 1 vs Grade 2
                        is the most common disagreement (off-diagonal counts concentrated there), while Grade 1 vs Grade 3
                        disagreements are rare. This directs targeted training efforts.
                    </p>
                </div>
            </div>
            "
            self$results$contingencyTableExplanation$setContent(html_content)
        },

        .populateBlandAltmanExplanation = function() {
            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Bland-Altman Analysis?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Bland-Altman method assesses <strong>agreement between two continuous measurements</strong>
                        by plotting the difference between methods against their mean. It is the gold standard for
                        method comparison studies in clinical research.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Bland-Altman</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Comparing two measurement methods</strong> &mdash; e.g., automated vs manual Ki-67 scoring</li>
                        <li><strong>Validating a new instrument</strong> against an established reference method</li>
                        <li><strong>Continuous data only</strong> &mdash; for categorical data, use kappa-based methods instead</li>
                        <li><strong>Paired measurements</strong> &mdash; both methods must measure the same cases</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpreting the Results</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Metric</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Interpretation</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Mean Difference (Bias)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Systematic difference between methods; close to 0 = no systematic bias</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Limits of Agreement (LoA)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Mean &plusmn; 1.96 SD; 95% of differences fall within these limits</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Proportional Bias (p)</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>If significant, the difference between methods depends on the measurement magnitude</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Normality test</strong></td>
                            <td style='padding: 8px;'>Differences should be approximately normally distributed for valid LoA</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Decision Rule</h4>
                    <p style='margin: 0; font-style: italic;'>
                        The key question is: &ldquo;Are the limits of agreement clinically acceptable?&rdquo; For example,
                        if two Ki-67 scoring methods have LoA of &plusmn;5%, and the clinical threshold is 20%,
                        a &plusmn;5% discrepancy could change treatment decisions for patients near the cutoff.
                        Clinical context &mdash; not statistical significance &mdash; determines acceptability.
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Bland, J. M., &amp; Altman, D. G. (1986). Statistical methods for assessing
                        agreement between two methods of clinical measurement. <em>The Lancet</em>, 327(8476), 307-310.
                    </p>
                </div>
            </div>
            "
            self$results$blandAltmanExplanation$setContent(html_content)
        },

        .populateKrippExplanation = function() {
            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is Krippendorff's Alpha?</h3>
                    <p style='margin: 0; color: #333;'>
                        Krippendorff's alpha (&alpha;) is a versatile reliability coefficient that works with
                        <strong>any number of raters</strong>, <strong>any measurement scale</strong>
                        (nominal, ordinal, interval, ratio), and <strong>handles missing data</strong> gracefully.
                        It is one of the most widely used agreement measures in content analysis and clinical research.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use Krippendorff's Alpha</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Missing data</strong> &mdash; Unlike kappa, it naturally handles incomplete designs where not all raters rate all cases</li>
                        <li><strong>Multiple raters (3+)</strong> &mdash; Generalizes seamlessly beyond 2 raters without modification</li>
                        <li><strong>Mixed scale types</strong> &mdash; Choose nominal, ordinal, interval, or ratio depending on your data</li>
                        <li><strong>Content analysis studies</strong> &mdash; Standard in communication and social science research</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Interpretation Thresholds (Krippendorff 2004)</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Alpha Range</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Interpretation</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Action</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>&alpha; &ge; 0.80</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Reliable agreement</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Data can be used with confidence</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.667 &le; &alpha; &lt; 0.80</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Tentative conclusions</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Use cautiously; consider additional rater training</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>&alpha; &lt; 0.667</strong></td>
                            <td style='padding: 8px;'>Insufficient agreement</td>
                            <td style='padding: 8px;'>Variable should be discarded or criteria revised</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #e3f2fd; border-left: 4px solid #1565C0; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Krippendorff's Alpha vs Other Measures</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Feature</th>
                            <th style='padding: 8px; text-align: center; border-bottom: 2px solid #333;'>Krippendorff</th>
                            <th style='padding: 8px; text-align: center; border-bottom: 2px solid #333;'>Fleiss' Kappa</th>
                            <th style='padding: 8px; text-align: center; border-bottom: 2px solid #333;'>ICC</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Handles missing data</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>&check;</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>&cross;</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>&cross;</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Any number of raters</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>&check;</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>&check;</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>&check;</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Nominal/ordinal/interval</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>&check;</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>Nominal only</td>
                            <td style='padding: 8px; text-align: center; border-bottom: 1px solid #ddd;'>Continuous only</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'>Kappa paradox resistant</td>
                            <td style='padding: 8px; text-align: center;'>Partially</td>
                            <td style='padding: 8px; text-align: center;'>&cross;</td>
                            <td style='padding: 8px; text-align: center;'>N/A</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Example</h4>
                    <p style='margin: 0; font-style: italic;'>
                        In a multi-site tumor grading study where 5 pathologists each reviewed a subset of cases
                        (not all pathologists reviewed every slide), Krippendorff's alpha is the ideal choice because
                        it handles the incomplete design naturally. If &alpha; = 0.72 for histologic grade,
                        conclusions should be considered tentative and additional consensus training is recommended.
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Krippendorff, K. (2004). <em>Content Analysis: An Introduction to Its Methodology</em>
                        (2nd ed.). Sage Publications. Chapter 11.
                    </p>
                </div>
            </div>
            "
            self$results$krippExplanation$setContent(html_content)
        },

        .populatePABAKExplanation = function() {
            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #333;'>What is PABAK?</h3>
                    <p style='margin: 0; color: #333;'>
                        PABAK (Prevalence-Adjusted Bias-Adjusted Kappa) corrects Cohen's kappa for the effects of
                        <strong>unequal category prevalence</strong> and <strong>systematic rater bias</strong>.
                        It directly addresses the <strong>kappa paradox</strong>, where kappa can be misleadingly low
                        despite high observed agreement.
                    </p>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>When to Use PABAK</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Rare diagnoses</strong> &mdash; When one category is much more common than others (e.g., benign vs malignant in screening)</li>
                        <li><strong>Low kappa despite high agreement</strong> &mdash; The classic kappa paradox situation</li>
                        <li><strong>Two raters only</strong> &mdash; PABAK is defined for 2&times;2 and multi-category tables with exactly 2 raters</li>
                        <li><strong>Alongside kappa</strong> &mdash; Report both to show whether prevalence/bias effects are distorting kappa</li>
                    </ul>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Key Indices Explained</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Index</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Formula</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #333;'>Interpretation</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>PABAK</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>2P<sub>o</sub> &minus; 1</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Agreement adjusted for both prevalence and bias; ranges from &minus;1 to 1</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Prevalence Index</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>|a &minus; d| / n</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>High values (&gt;0.5) indicate unbalanced categories; kappa is deflated</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>Bias Index</strong></td>
                            <td style='padding: 8px;'>|b &minus; c| / n</td>
                            <td style='padding: 8px;'>High values (&gt;0.3) indicate systematic difference between raters</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #f9f9f9; border-left: 4px solid #333; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #333;'>Clinical Example: The Kappa Paradox</h4>
                    <p style='margin: 0; font-style: italic;'>
                        Two pathologists classify 100 biopsies as benign/malignant. Results: 90 both say benign,
                        5 both say malignant, 3 and 2 disagree. Observed agreement = 95%, but kappa = 0.47
                        (&ldquo;moderate&rdquo;). This is misleading! The Prevalence Index = 0.85 (highly
                        imbalanced categories) is deflating kappa. PABAK = 0.90 (&ldquo;almost perfect&rdquo;)
                        better reflects the true agreement level.
                    </p>
                </div>

                <div style='margin-top: 15px; padding: 10px; background: #f5f5f5; border-radius: 4px;'>
                    <p style='margin: 0; font-size: 12px; color: #666;'>
                        <strong>Reference:</strong> Byrt, T., Bishop, J., &amp; Carlin, J. B. (1993). Bias, prevalence and kappa.
                        <em>Journal of Clinical Epidemiology</em>, 46(5), 423-429.
                    </p>
                </div>
            </div>
            "
            self$results$pabakExplanation$setContent(html_content)
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
            self$results$irrtableHeading$setVisible(FALSE)

            # Populate guide explanations even before data is selected
            # so users can read about methods while setting up analysis
            if (self$options$showKrippGuide) private$.populateKrippExplanation()
            if (self$options$showLightKappaGuide) private$.populateLightKappaExplanation()
            if (self$options$showFinnGuide) private$.populateFinnExplanation()
            if (self$options$showKendallWGuide) private$.populateKendallWExplanation()
            if (self$options$showRobinsonAGuide) private$.populateRobinsonAExplanation()
            if (self$options$showMeanSpearmanGuide) private$.populateMeanSpearmanExplanation()
            if (self$options$showICCGuide) private$.populateICCExplanation()
            if (self$options$showMeanPearsonGuide) private$.populateMeanPearsonExplanation()
            if (self$options$showLinCCCGuide) private$.populateLinCCCExplanation()
            if (self$options$showTDIGuide) private$.populateTDIExplanation()
            if (self$options$showSpecificAgreementGuide) private$.populateSpecificAgreementExplanation()
            if (self$options$showAgreementHeatmapGuide) private$.populateAgreementHeatmapExplanation()
            if (self$options$showRaterProfileGuide) private$.populateRaterProfileExplanation()
            if (self$options$showSubgroupGuide) private$.populateSubgroupExplanation()
            if (self$options$showRaterClusterGuide) private$.populateRaterClusterExplanation()
            if (self$options$showCaseClusterGuide) private$.populateCaseClusterExplanation()
            if (self$options$showMaxwellREGuide) private$.populateMaxwellREExplanation()
            if (self$options$showInterIntraRaterGuide) private$.populateInterIntraRaterExplanation()
            if (self$options$showIotaGuide) private$.populateIotaExplanation()
            if (self$options$showHierarchicalGuide) private$.populateHierarchicalExplanation()
            if (self$options$showBlandAltmanGuide) private$.populateBlandAltmanExplanation()
            if (self$options$showMixedEffectsGuide) private$.populateMixedEffectsExplanation()
            if (self$options$showConfusionMatrixGuide) private$.populateConfusionMatrixExplanation()
            if (self$options$showBootstrapCIGuide) private$.populateBootstrapCIExplanation()
            if (self$options$showConcordanceF1Guide) private$.populateConcordanceF1Explanation()
            if (self$options$showPairedAgreementGuide) private$.populatePairedAgreementExplanation()
            if (self$options$showSampleSizeGuide) private$.populateAgreementSampleSizeExplanation()
            if (self$options$showRaterBiasGuide) private$.populateRaterBiasExplanation()
            if (self$options$showBhapkarGuide) private$.populateBhapkarExplanation()
            if (self$options$showStuartMaxwellGuide) private$.populateStuartMaxwellExplanation()
            if (self$options$showPairwiseKappaGuide) private$.populatePairwiseKappaExplanation()
            if (self$options$showGwetGuide) private$.populateGwetExplanation()
            if (self$options$showPABAKGuide) private$.populatePABAKExplanation()

            return()
        } else {
            self$results$welcome$setVisible(FALSE)
            if (nrow(self$data) == 0) {
                self$results$irrtableHeading$setVisible(FALSE)
                self$results$irrtable$setNote("error", "Data contains no (complete) rows. Please check your dataset.")
                return()
            }

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

            # Level Ordering Information (if requested) ----
            if (self$options$showLevelInfo) {
                private$.populateLevelInfo(ratings)
            }

            # Detect whether data is continuous (numeric, non-factor) ----
            is_continuous <- all(sapply(ratings, function(x) is.numeric(x) && !is.factor(x) && !is.ordered(x)))
            n_unique_vals <- max(sapply(ratings, function(x) length(unique(na.omit(x)))))

            # Skip kappa for continuous data — kappa is only meaningful for categorical/ordinal data
            if (is_continuous && n_unique_vals > 20) {
                self$results$irrtableHeading$setVisible(FALSE)
                self$results$irrtable$setVisible(FALSE)

                # Check if any continuous-appropriate analyses are enabled
                has_continuous_analysis <- self$options$icc || self$options$meanPearson ||
                    self$options$linCCC || self$options$tdi || self$options$blandAltmanPlot ||
                    self$options$kripp

                if (!has_continuous_analysis) {
                    # Show guidance when no appropriate analysis is enabled
                    self$results$welcome$setVisible(TRUE)
                    self$results$welcome$setContent(
                        "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                        <div style='background: #fff3cd; border: 2px solid #856404; padding: 20px; margin-bottom: 20px;'>
                        <h2 style='margin: 0 0 10px 0; font-size: 18px; color: #856404;'>Continuous Data Detected</h2>
                        <p style='margin: 0; font-size: 14px; color: #664d03;'>Your variables have many unique values (>20), indicating continuous measurements. Cohen's/Fleiss' Kappa requires categorical data and has been skipped.</p>
                        </div>

                        <div style='background: #f9f9f9; border-left: 4px solid #0d6efd; padding: 15px; margin-bottom: 20px;'>
                        <h3 style='margin: 0 0 10px 0; color: #333; font-size: 16px;'>Recommended Analyses for Continuous Data</h3>
                        <table style='width: 100%; border-collapse: collapse;'>
                        <tr><td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>ICC</strong></td><td style='padding: 8px; border-bottom: 1px solid #ddd;'>Intraclass Correlation Coefficient - standard measure for continuous rater agreement</td></tr>
                        <tr><td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Lin's CCC</strong></td><td style='padding: 8px; border-bottom: 1px solid #ddd;'>Concordance Correlation Coefficient - assesses both precision and accuracy (2 raters)</td></tr>
                        <tr><td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>Bland-Altman</strong></td><td style='padding: 8px; border-bottom: 1px solid #ddd;'>Limits of Agreement plot - visualizes systematic bias and agreement range (2 raters)</td></tr>
                        <tr><td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>TDI</strong></td><td style='padding: 8px; border-bottom: 1px solid #ddd;'>Total Deviation Index - boundary within which a proportion of differences fall</td></tr>
                        <tr><td style='padding: 8px;'><strong>Krippendorff's Alpha</strong></td><td style='padding: 8px;'>Works with any data type including continuous (interval/ratio level)</td></tr>
                        </table>
                        </div>

                        <p style='font-size: 13px; color: #666;'>Enable these options from the panels on the left to analyze your continuous measurements.</p>
                        </div>"
                    )
                }
            } else {

            self$results$irrtableHeading$setVisible(TRUE)

            # 2 raters: Cohen's kappa ----
            if (length(self$options$vars) == 2) {
                xorder <- unlist(lapply(ratings, is.ordered))

                if (wght %in% c("equal", "squared") && !all(xorder == TRUE)) {
                    self$results$irrtable$setNote("error", "Weighted kappa requires ordinal variables. Please select ordinal data or choose 'Unweighted'.")
                    result2 <- list(value = NA, method = "Weighted Cohen's Kappa (not computed - requires ordinal data)",
                                    stat.name = "Kappa", statistic = NA, p.value = NA)
                } else if (exct == TRUE) {
                    self$results$irrtable$setNote("error", "Exact kappa requires at least 3 raters. Please add more rater variables or disable 'Exact Kappa'.")
                    result2 <- list(value = NA, method = "Cohen's Kappa (not computed - exact requires 3+ raters)",
                                    stat.name = "Kappa", statistic = NA, p.value = NA)
                } else if (nrow(na.omit(ratings)) > 0) {
                    # irr::kappa2 ----
                    result2 <- irr::kappa2(ratings = ratings, weight = wght)
                } else {
                    result2 <- list(value = NA, stat.name = "Kappa", statistic = NA, p.value = NA)
                }

            # >=3 raters: Fleiss kappa ----
            } else if (length(self$options$vars) >= 3) {
                if (nrow(na.omit(ratings)) > 0) {
                    result2 <- irr::kappam.fleiss(ratings = ratings, exact = exct, detail = TRUE)
                } else {
                    result2 <- list(value = NA, stat.name = "Kappa", statistic = NA, p.value = NA)
                }
            }


            # Percentage agreement ----
            result1 <- irr::agree(ratings)
            if (!is.na(result1[["value"]]) && result1[["value"]] > 100) {
                result1[["value"]] <- NA
                self$results$irrtable$setNote("data_error",
                    "Unexpected agreement value > 100%. Please check the data.")
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

            }  # end of categorical kappa block

            # Frequency tables (if requested) ----
            # Control visibility based on number of raters and sft option
            num_raters <- length(self$options$vars)

            if (self$options$sft) {
                self$results$contingencyTableHeading$setVisible(TRUE)
                if (self$options$showAbout) { private$.populateContingencyTableExplanation() }
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
                        dplyr::count(dplyr::across(dplyr::everything())) %>%
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
            if (wght != "unweighted" && !(is_continuous && n_unique_vals > 20)) {
                weight_guide <- private$.createWeightedKappaGuide(wght)
                self$results$weightedKappaGuide$setContent(weight_guide)
            }

            # Natural-language summary (if requested) ----
            if (self$options$showSummary && exists("result1") && exists("result2")) {
                summary_text <- private$.createSummary(result1, result2, wght, exct)
                self$results$summary$setContent(summary_text)
            }

            # About panel (if requested) ----
            if (self$options$showAbout) {
                about_text <- private$.createAboutPanel()
                self$results$about$setContent(about_text)

                # Clinical use cases guide
                private$.populateClinicalUseCases()
            }
        }



        private$.checkpoint()

        # Krippendorff's Alpha (if requested) ----
        if (self$options$kripp || self$options$showKrippGuide) {
            self$results$krippTableHeading$setVisible(TRUE)
        }
        if (self$options$showKrippGuide) {
            private$.populateKrippExplanation()
        }
        if (self$options$kripp) {
            # Convert ratings data frame to matrix
            ratings_matrix <- as.matrix(ratings)

            # Ensure numeric conversion if needed
            if (!is.numeric(ratings_matrix)) {
                # Preserve original factor level ordering (critical for ordinal method)
                all_levels <- NULL
                for (col_idx in seq_len(ncol(ratings))) {
                    col_data <- ratings[[col_idx]]
                    if (is.factor(col_data) || is.ordered(col_data)) {
                        if (is.null(all_levels)) {
                            all_levels <- levels(col_data)
                        } else {
                            new_levels <- setdiff(levels(col_data), all_levels)
                            all_levels <- c(all_levels, new_levels)
                        }
                    }
                }

                if (!is.null(all_levels)) {
                    ratings_matrix <- matrix(
                        as.numeric(factor(ratings_matrix, levels = all_levels)),
                        nrow = nrow(ratings_matrix),
                        ncol = ncol(ratings_matrix)
                    )
                } else {
                    # Fallback for character data: alphabetical
                    ratings_matrix <- matrix(
                        as.numeric(factor(ratings_matrix)),
                        nrow = nrow(ratings_matrix),
                        ncol = ncol(ratings_matrix)
                    )
                }
            }

            # Add error handling
            tryCatch({
                # Calculate Krippendorff's alpha
                kripp_result <- irr::kripp.alpha(
                    t(ratings_matrix),
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
                    set.seed(42) # consistent with other bootstraps in this module
                    n_boot <- self$options$nBoot
                    alpha_boots <- rep(NA_real_, n_boot)

                    for(i in 1:n_boot) {
                        boot_indices <- sample(1:nrow(ratings_matrix), replace = TRUE)
                        boot_data <- ratings_matrix[boot_indices, , drop = FALSE]

                        boot_alpha <- try(irr::kripp.alpha(t(boot_data),
                                                           method = self$options$krippMethod)$value,
                                          silent = TRUE)

                        if(!inherits(boot_alpha, "try-error")) {
                            alpha_boots[i] <- boot_alpha
                        }
                    }

                    # Calculate confidence intervals using confLevel
                    kripp_alpha_level <- 1 - self$options$confLevel
                    ci <- quantile(alpha_boots, c(kripp_alpha_level/2, 1 - kripp_alpha_level/2), na.rm = TRUE)

                    # Add CI values to list
                    values_list$ci_lower <- ci[1]
                    values_list$ci_upper <- ci[2]
                }

                # Interpretation (Krippendorff 2004 thresholds)
                alpha_val <- kripp_result$value
                if (is.na(alpha_val) || !is.finite(alpha_val)) {
                    values_list$interpretation <- "Cannot interpret"
                } else if (alpha_val < 0) {
                    values_list$interpretation <- "Below chance agreement"
                } else if (alpha_val < 0.667) {
                    values_list$interpretation <- "Insufficient agreement - discard variable"
                } else if (alpha_val < 0.80) {
                    values_list$interpretation <- "Tentative conclusions only"
                } else {
                    values_list$interpretation <- "Reliable agreement"
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
                    alpha = NA,
                    interpretation = "Error in calculation"
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

        private$.checkpoint()

        # Light's Kappa (if requested) ----
        if (self$options$lightKappa || self$options$showLightKappaGuide || self$options$finn || self$options$showFinnGuide || self$options$kendallW || self$options$showKendallWGuide || self$options$robinsonA || self$options$showRobinsonAGuide) {
            self$results$lightKappaTableHeading$setVisible(TRUE)
        }
        if (self$options$showLightKappaGuide) {
            private$.populateLightKappaExplanation()
        }
        if (self$options$lightKappa) {
            if (is_continuous && n_unique_vals > 20) {
                self$results$lightKappaTable$setNote("type_error",
                    "Light's Kappa requires categorical data. Your data appears to be continuous. Consider using ICC or Lin's CCC instead.")
            } else {
                private$.calculateLightKappa(ratings)
            }
        }

        private$.checkpoint()

        # Finn Coefficient (if requested) ----
        if (self$options$showFinnGuide) {
            private$.populateFinnExplanation()
        }
        if (self$options$finn) {
            if (is_continuous && n_unique_vals > 20) {
                self$results$finnTable$setNote("type_error",
                    "Finn Coefficient is designed for categorical/ordinal data. Your data appears to be continuous. Results may not be meaningful.")
            }
            private$.calculateFinn(ratings)
        }

        private$.checkpoint()

        # Kendall's W (if requested) ----
        if (self$options$showKendallWGuide) {
            private$.populateKendallWExplanation()
        }
        if (self$options$kendallW) {
            private$.calculateKendallW(ratings)
        }

        private$.checkpoint()

        # Robinson's A (if requested) ----
        if (self$options$showRobinsonAGuide) {
            private$.populateRobinsonAExplanation()
        }
        if (self$options$robinsonA) {
            private$.calculateRobinsonA(ratings)
        }

        private$.checkpoint()

        # Mean Spearman Rho (if requested) ----
        if (self$options$showMeanSpearmanGuide) {
            private$.populateMeanSpearmanExplanation()
        }
        if (self$options$meanSpearman) {
            private$.calculateMeanSpearman(ratings)
        }

        private$.checkpoint()

        # Rater Bias Test (if requested) ----
        if (self$options$raterBias || self$options$showRaterBiasGuide || self$options$bhapkar || self$options$showBhapkarGuide || self$options$stuartMaxwell || self$options$showStuartMaxwellGuide) {
            if (is_continuous && n_unique_vals > 20) {
                if (self$options$raterBias) self$results$raterBiasTable$setNote("type_error",
                    "Rater Bias test requires categorical data. Your data appears to be continuous.")
                if (self$options$bhapkar) self$results$bhapkarTable$setNote("type_error",
                    "Bhapkar test requires categorical data. Your data appears to be continuous.")
                if (self$options$stuartMaxwell) self$results$stuartMaxwellTable$setNote("type_error",
                    "Stuart-Maxwell test requires categorical data. Your data appears to be continuous.")
            } else {
                self$results$raterBiasHeading$setVisible(TRUE)
            }
        }
        if (self$options$showRaterBiasGuide && !(is_continuous && n_unique_vals > 20)) {
            private$.populateRaterBiasExplanation()
        }
        if (self$options$raterBias && !(is_continuous && n_unique_vals > 20)) {
            private$.calculateRaterBias(ratings)
        }

        private$.checkpoint()

        # Bhapkar Test (if requested) ----
        if (self$options$showBhapkarGuide && !(is_continuous && n_unique_vals > 20)) {
            private$.populateBhapkarExplanation()
        }
        if (self$options$bhapkar && !(is_continuous && n_unique_vals > 20)) {
            private$.calculateBhapkar(ratings)
        }

        private$.checkpoint()

        # Stuart-Maxwell Test (if requested) ----
        if (self$options$showStuartMaxwellGuide && !(is_continuous && n_unique_vals > 20)) {
            private$.populateStuartMaxwellExplanation()
        }
        if (self$options$stuartMaxwell && !(is_continuous && n_unique_vals > 20)) {
            private$.calculateStuartMaxwell(ratings)
        }

        private$.checkpoint()

        # Pairwise Kappa Analysis (if requested) ----
        if (self$options$showPairwiseKappaGuide && !(is_continuous && n_unique_vals > 20)) {
            private$.populatePairwiseKappaExplanation()
        }
        if (self$options$pairwiseKappa) {
            if (is_continuous && n_unique_vals > 20) {
                self$results$pairwiseKappaTable$setNote("type_error",
                    "Pairwise Kappa requires categorical data. Your data appears to be continuous. Consider using ICC or Lin's CCC instead.")
            } else {
                # Get reference rater data
                if (!is.null(self$options$referenceRater)) {
                    reference_ratings <- jmvcore::select(mydata, self$options$referenceRater)
                    private$.calculatePairwiseKappa(ratings, reference_ratings)
                } else {
                    self$results$pairwiseKappaTable$setNote(
                        "error",
                        "Please select a reference rater variable to perform pairwise kappa analysis."
                    )
                }
            }
        }

        private$.checkpoint()

        # Gwet's AC1/AC2 (if requested) ----
        if (self$options$gwet || self$options$showGwetGuide || self$options$pabak || self$options$showPABAKGuide) {
            if (is_continuous && n_unique_vals > 20) {
                if (self$options$gwet) self$results$gwetTable$setNote("type_error",
                    "Gwet's AC requires categorical data. Your data appears to be continuous. Consider using ICC or Lin's CCC instead.")
                if (self$options$pabak) self$results$pabakTable$setNote("type_error",
                    "PABAK requires categorical data. Your data appears to be continuous.")
            } else {
                self$results$gwetHeading$setVisible(TRUE)
            }
        }
        if (self$options$showGwetGuide && !(is_continuous && n_unique_vals > 20)) {
            private$.populateGwetExplanation()
        }
        if (self$options$gwet && !(is_continuous && n_unique_vals > 20)) {
            private$.calculateGwetAC(ratings)
        }

        private$.checkpoint()

        # PABAK & Prevalence/Bias Indices (if requested) ----
        if (self$options$showPABAKGuide && !(is_continuous && n_unique_vals > 20)) {
            private$.populatePABAKExplanation()
        }
        if (self$options$pabak && !(is_continuous && n_unique_vals > 20)) {
            private$.calculatePABAK(ratings)
        }

        private$.checkpoint()

        # ICC (if requested) ----
        if (self$options$icc || self$options$showICCGuide || self$options$meanPearson || self$options$linCCC || self$options$tdi) {
            self$results$iccHeading$setVisible(TRUE)
        }
        if (self$options$showICCGuide) {
            private$.populateICCExplanation()
        }
        if (self$options$icc) {
            private$.calculateICC(ratings)
        }

        private$.checkpoint()

        # Mean Pearson Correlation (if requested) ----
        if (self$options$showMeanPearsonGuide) {
            private$.populateMeanPearsonExplanation()
        }
        if (self$options$meanPearson) {
            private$.calculateMeanPearson(ratings)
        }

        private$.checkpoint()

        # Lin's Concordance Correlation Coefficient (if requested) ----
        if (self$options$showLinCCCGuide) {
            private$.populateLinCCCExplanation()
        }
        if (self$options$linCCC) {
            private$.calculateLinCCC(ratings)
        }

        private$.checkpoint()

        # Total Deviation Index (if requested) ----
        if (self$options$showTDIGuide) {
            private$.populateTDIExplanation()
        }
        if (self$options$tdi) {
            private$.calculateTDI(ratings)
        }

        private$.checkpoint()

        # Specific Agreement Indices (if requested) ----
        if (self$options$specificAgreement || self$options$showSpecificAgreementGuide) {
            self$results$specificAgreementHeading$setVisible(TRUE)
        }
        if (self$options$showSpecificAgreementGuide) {
            private$.populateSpecificAgreementExplanation()
        }
        if (self$options$specificAgreement) {
            private$.calculateSpecificAgreement(ratings)
        }

        private$.checkpoint()

        # Agreement Heatmap (if requested) ----
        if (self$options$showAgreementHeatmapGuide) {
            private$.populateAgreementHeatmapExplanation()
        }
        if (self$options$agreementHeatmap) {
            private$.populateAgreementHeatmap(ratings)
        }

        private$.checkpoint()

        # Rater Profile Plots (if requested) ----
        if (self$options$showRaterProfileGuide) {
            private$.populateRaterProfileExplanation()
        }
        if (self$options$raterProfiles) {
            private$.populateRaterProfiles(ratings)
        }

        private$.checkpoint()

        # Agreement by Subgroup (if requested) ----
        if (self$options$showSubgroupGuide) {
            private$.populateSubgroupExplanation()
        }
        if (self$options$agreementBySubgroup) {
            private$.calculateAgreementBySubgroup(ratings)
        }

        private$.checkpoint()

        # Rater Clustering (if requested) ----
        if (self$options$raterClustering || self$options$showRaterClusterGuide || self$options$caseClustering || self$options$showCaseClusterGuide) {
            self$results$raterClusterHeading$setVisible(TRUE)
        }
        if (self$options$showRaterClusterGuide) {
            private$.populateRaterClusterExplanation()
        }
        if (self$options$raterClustering) {
            private$.performRaterClustering(ratings)
        }

        private$.checkpoint()

        # Case Clustering (if requested) ----
        if (self$options$showCaseClusterGuide) {
            private$.populateCaseClusterExplanation()
        }
        if (self$options$caseClustering) {
            private$.performCaseClustering(ratings)
        }

        private$.checkpoint()

        # Maxwell's Random Error Index (if requested) ----
        if (self$options$maxwellRE || self$options$showMaxwellREGuide || self$options$interIntraRater || self$options$showInterIntraRaterGuide || self$options$iota || self$options$showIotaGuide) {
            self$results$maxwellREHeading$setVisible(TRUE)
        }
        if (self$options$showMaxwellREGuide) {
            private$.populateMaxwellREExplanation()
        }
        if (self$options$maxwellRE) {
            private$.calculateMaxwellRE(ratings)
        }

        private$.checkpoint()

        # Inter/Intra-Rater Reliability (if requested) ----
        if (self$options$showInterIntraRaterGuide) {
            private$.populateInterIntraRaterExplanation()
        }
        if (self$options$interIntraRater) {
            private$.calculateInterIntraRater(ratings)
        }

        private$.checkpoint()

        # Iota Coefficient (if requested) ----
        if (self$options$showIotaGuide) {
            private$.populateIotaExplanation()
        }
        if (self$options$iota) {
            private$.calculateIota(ratings)
        }

        private$.checkpoint()

        # Consensus Variable Calculation (if requested) ----
        if (self$options$consensusVar || self$options$loaVariable) {
            self$results$computedVariablesHeading$setVisible(TRUE)
        }
        if (self$options$consensusVar) {
            private$.createConsensusVariable(ratings)
        }

        private$.checkpoint()

        # Level of Agreement Variable (if requested) ----
        if (self$options$loaVariable) {
            private$.calculateLevelOfAgreement(ratings)
        }

        private$.checkpoint()

        # Hierarchical/Multilevel Kappa (if requested) ----
        if (self$options$hierarchicalKappa || self$options$showHierarchicalGuide) {
            self$results$hierarchicalHeading$setVisible(TRUE)
        }
        if (self$options$showHierarchicalGuide) {
            private$.populateHierarchicalExplanation()
        }
        if (self$options$hierarchicalKappa) {
            # Get cluster variable data
            cluster_data <- NULL
            if (!is.null(self$options$clusterVariable)) {
                cluster_data <- jmvcore::select(mydata, self$options$clusterVariable)
            }

            private$.calculateHierarchicalKappa(ratings, cluster_data)
        }

        private$.checkpoint()

        # Bland-Altman analysis (if requested) ----
        if (self$options$blandAltmanPlot || self$options$showBlandAltmanGuide) {
            self$results$blandAltmanHeading$setVisible(TRUE)
        }
        if (self$options$showBlandAltmanGuide) {
            private$.populateBlandAltmanExplanation()
        }
        if (self$options$blandAltmanPlot) {
            private$.populateBlandAltman(ratings)
        }

        private$.checkpoint()

        # Mixed-Effects Condition Comparison (if requested) ----
        if (self$options$mixedEffectsComparison || self$options$showMixedEffectsGuide || self$options$confusionMatrix || self$options$showConfusionMatrixGuide || self$options$bootstrapCI || self$options$showBootstrapCIGuide || self$options$multiAnnotatorConcordance || self$options$showConcordanceF1Guide) {
            self$results$advancedHeading$setVisible(TRUE)
        }
        if (self$options$showMixedEffectsGuide) {
            private$.populateMixedEffectsExplanation()
        }
        if (self$options$mixedEffectsComparison) {
            condition_data <- NULL
            if (!is.null(self$options$conditionVariable)) {
                condition_data <- jmvcore::select(mydata, self$options$conditionVariable)
            }
            private$.calculateMixedEffectsComparison(ratings, condition_data, mydata)
        }

        private$.checkpoint()

        # Multi-Class Confusion Matrix (if requested) ----
        if (self$options$showConfusionMatrixGuide) {
            private$.populateConfusionMatrixExplanation()
        }
        if (self$options$confusionMatrix) {
            private$.calculateConfusionMatrix(ratings)
        }

        private$.checkpoint()

        # Bootstrap Confidence Intervals (if requested) ----
        if (self$options$showBootstrapCIGuide) {
            private$.populateBootstrapCIExplanation()
        }
        if (self$options$bootstrapCI) {
            private$.calculateBootstrapCI(ratings)
        }

        private$.checkpoint()

        # Multi-Annotator Concordance F1 (if requested) ----
        if (self$options$showConcordanceF1Guide) {
            private$.populateConcordanceF1Explanation()
        }
        if (self$options$multiAnnotatorConcordance) {
            private$.calculateConcordanceF1(ratings)
        }

        private$.checkpoint()

        # Paired Agreement Comparison (if requested) ----
        if (self$options$pairedAgreementTest || self$options$showPairedAgreementGuide || self$options$agreementSampleSize || self$options$showSampleSizeGuide) {
            self$results$pairedAgreementHeading$setVisible(TRUE)
        }
        if (self$options$showPairedAgreementGuide) {
            private$.populatePairedAgreementExplanation()
        }
        if (self$options$pairedAgreementTest) {
            private$.calculatePairedAgreementComparison(ratings)
        }

        private$.checkpoint()

        # Sample Size for Agreement Studies (if requested) ----
        if (self$options$showSampleSizeGuide) {
            private$.populateAgreementSampleSizeExplanation()
        }
        if (self$options$agreementSampleSize) {
            private$.calculateAgreementSampleSize()
        }

        }  # End of .run function

    ),  # End of private list

    public = list()

    )
