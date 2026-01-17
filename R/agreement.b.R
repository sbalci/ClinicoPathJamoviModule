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
            # Initialization logic
            # Plot render functions are specified in .r.yaml via renderFun parameter
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

        .agreementHeatmap = function(image, ...) {
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Gwet's AC Coefficient?</h3>
                    <p style='margin: 0; color: #333;'>
                        Gwet's AC coefficient solves the <strong>kappa paradox</strong> where Cohen's/Fleiss' kappa
                        can be artificially low despite high observed agreement.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Gwet's AC</h4>
                    <p style='margin: 0 0 10px 0;'><strong>This method is particularly valuable when you have:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Rare tumor subtypes</strong> – Unbalanced categories where some diagnoses are very uncommon</li>
                        <li><strong>High agreement rates</strong> – Easy-to-diagnose cases where raters agree >90% of the time</li>
                        <li><strong>Skewed marginal distributions</strong> – When one category dominates the data</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>AC1 vs AC2</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Method</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Use Case</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Clinical Example</h4>
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
                <div style='background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; margin-bottom: 20px; border-radius: 8px;'>
                    <h2 style='margin: 0 0 10px 0;'>Clinical Use Cases & Method Selection Guide</h2>
                    <p style='margin: 0; opacity: 0.9;'>Choose the right agreement measure for your pathology research</p>
                </div>

                <!-- Categorical Data Methods -->
                <div style='background: #f8f9fa; border-left: 5px solid #6c757d; padding: 20px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 15px 0; color: #343a40;'>📊 Categorical/Ordinal Data</h3>

                    <div style='background: white; padding: 15px; margin-bottom: 15px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>
                        <h4 style='margin: 0 0 10px 0; color: #495057;'>Cohen's/Fleiss' Kappa (Standard Method)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use for:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Tumor grading</strong> - G1, G2, G3 classification</li>
                            <li><strong>Histologic type</strong> - Adenocarcinoma, squamous cell carcinoma, etc.</li>
                            <li><strong>Margin status</strong> - Positive, negative, close</li>
                            <li><strong>TNM staging</strong> - T1, T2, T3, T4 categories</li>
                            <li><strong>Biomarker scoring</strong> - Negative (0), 1+, 2+, 3+</li>
                        </ul>
                        <p style='margin: 0; font-size: 13px; color: #6c757d;'><em>Note: May be problematic with high agreement or rare categories</em></p>
                    </div>

                    <div style='background: white; padding: 15px; margin-bottom: 15px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>
                        <h4 style='margin: 0 0 10px 0; color: #495057;'>Weighted Kappa (Ordinal Data)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use for ordered categories where partial credit matters:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Dysplasia grading</strong> - None, low-grade, high-grade</li>
                            <li><strong>Mitotic count categories</strong> - Low (1-9), intermediate (10-19), high (≥20)</li>
                            <li><strong>Inflammation severity</strong> - Absent, mild, moderate, severe</li>
                            <li><strong>Fibrosis stage</strong> - F0, F1, F2, F3, F4</li>
                        </ul>
                        <p style='margin: 0; padding: 10px; background: #fff3cd; border-left: 3px solid #ffc107; font-size: 13px;'>
                            <strong>⚠️ Important:</strong> Use \"Show Level Ordering Information\" to verify proper ordering (e.g., F0 → F1 → F2 → F3 → F4)
                        </p>
                    </div>

                    <div style='background: white; padding: 15px; margin-bottom: 15px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>
                        <h4 style='margin: 0 0 10px 0; color: #495057;'>Gwet's AC1/AC2 (Kappa Paradox Solution)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Choose when you have:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Rare tumor subtypes</strong> - Neuroendocrine carcinoma in colorectal specimens (2% prevalence)</li>
                            <li><strong>High agreement rates</strong> - Benign vs malignant with 95%+ agreement</li>
                            <li><strong>Unbalanced categories</strong> - Metastasis (5% positive) vs primary (95%)</li>
                            <li><strong>Quality control studies</strong> - Most cases straightforward, few difficult</li>
                        </ul>
                        <p style='margin: 0; padding: 10px; background: #d1ecf1; border-left: 3px solid #17a2b8; font-size: 13px;'>
                            <strong>💡 Tip:</strong> Run both Kappa and Gwet's AC - if they differ substantially, Gwet's AC is more reliable
                        </p>
                    </div>

                    <div style='background: white; padding: 15px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>
                        <h4 style='margin: 0 0 10px 0; color: #495057;'>Krippendorff's Alpha (Missing Data/Flexible)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use when:</strong></p>
                        <ul style='margin: 0; padding-left: 20px;'>
                            <li><strong>Incomplete ratings</strong> - Not all pathologists rated all cases</li>
                            <li><strong>Different measurement levels</strong> - Comparing nominal, ordinal, interval data</li>
                            <li><strong>Large panels</strong> - Many raters (>5) with varying participation</li>
                        </ul>
                    </div>
                </div>

                <!-- Continuous Data Methods -->
                <div style='background: #f8f9fa; border-left: 5px solid #007bff; padding: 20px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 15px 0; color: #343a40;'>📏 Continuous/Numeric Data</h3>

                    <div style='background: white; padding: 15px; margin-bottom: 15px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>
                        <h4 style='margin: 0 0 10px 0; color: #495057;'>ICC (Intraclass Correlation Coefficient)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use for:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Tumor size</strong> - Diameter measurements (mm or cm)</li>
                            <li><strong>Biomarker quantification</strong> - Ki-67 proliferation index (%)</li>
                            <li><strong>Cell counts</strong> - Mitotic figures per 10 HPF</li>
                            <li><strong>Morphometric analysis</strong> - Nuclear area (μm²), gland perimeter</li>
                            <li><strong>Digital pathology</strong> - Automated vs manual measurements</li>
                            <li><strong>Continuous scores</strong> - Visual analog scales (0-100)</li>
                        </ul>
                        <p style='margin: 0; padding: 10px; background: #e7f3ff; border-left: 3px solid #007bff; font-size: 13px;'>
                            <strong>📋 Model selection:</strong> Use ICC(2,1) or ICC(3,1) for most pathology studies where all cases are rated by the same pathologists
                        </p>
                    </div>

                    <div style='background: white; padding: 15px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>
                        <h4 style='margin: 0 0 10px 0; color: #495057;'>Bland-Altman Plot (Visual Assessment)</h4>
                        <p style='margin: 0 0 10px 0;'><strong>Use alongside ICC to:</strong></p>
                        <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                            <li><strong>Detect systematic bias</strong> - Does one rater consistently measure higher/lower?</li>
                            <li><strong>Identify outliers</strong> - Cases with unusually large disagreement</li>
                            <li><strong>Check proportional bias</strong> - Does disagreement increase with measurement size?</li>
                            <li><strong>Define acceptable limits</strong> - What range of disagreement is clinically acceptable?</li>
                        </ul>
                        <p style='margin: 0; padding: 10px; background: #d4edda; border-left: 3px solid #28a745; font-size: 13px;'>
                            <strong>✅ Best practice:</strong> Always use ICC + Bland-Altman together for continuous data
                        </p>
                    </div>
                </div>

                <!-- Quick Decision Guide -->
                <div style='background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 20px; border-radius: 8px;'>
                    <h3 style='margin: 0 0 15px 0;'>🎯 Quick Decision Guide</h3>
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
                <div style='margin-top: 20px; padding: 15px; background: #fff3e0; border-left: 4px solid #ff9800; border-radius: 4px;'>
                    <h4 style='margin: 0 0 10px 0; color: #e65100;'>📊 Sample Size Recommendations</h4>
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Light's Kappa?</h3>
                    <p style='margin: 0; color: #333;'>
                        Light's kappa is an alternative agreement measure for <strong>3 or more raters</strong>.
                        It calculates the <strong>average of all pairwise kappas</strong> between raters.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Light's Kappa</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Choose Light's kappa instead of Fleiss' kappa when:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Raters have different marginal distributions</strong> - Some pathologists may be more conservative/liberal than others</li>
                        <li><strong>Unequal rater characteristics</strong> - Mix of senior and junior pathologists</li>
                        <li><strong>Assumptions questionable</strong> - When Fleiss' kappa assumptions don't hold</li>
                        <li><strong>Pairwise comparisons matter</strong> - Want to understand individual rater-to-rater agreement</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Light's Kappa vs Fleiss' Kappa</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Feature</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Light's Kappa</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Fleiss' Kappa</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Clinical Example</h4>
                    <p style='margin: 0; font-style: italic;'>
                        In a study with 3 pathologists grading dysplasia (none, low-grade, high-grade):
                        Pathologist A is conservative (more often grades as \"none\"), Pathologist B is moderate,
                        and Pathologist C is more aggressive (more often grades as \"high-grade\").
                        Light's kappa would be more appropriate than Fleiss' kappa because the raters have
                        systematically different rating patterns.
                    </p>
                </div>

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Interpretation</h4>
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is the Finn Coefficient?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Finn coefficient is a <strong>variance-based agreement measure</strong> for categorical data.
                        Unlike kappa-based measures that focus on exact agreement, Finn's approach uses analysis of variance
                        to quantify interrater reliability. It is <strong>especially useful when variance between raters
                        is low</strong> (i.e., when agreement is already high).
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Finn Coefficient</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Particularly useful for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>High agreement scenarios</strong> - When raters already agree well, kappa may be paradoxically low</li>
                        <li><strong>Ordered categorical data</strong> - Tumor grades (G1/G2/G3), severity scales, ordinal ratings</li>
                        <li><strong>Alternative to kappa</strong> - When kappa assumptions are violated or kappa values are unstable</li>
                        <li><strong>Low variance situations</strong> - When between-rater variance is minimal</li>
                        <li><strong>Educational/research settings</strong> - Comparing trained raters with similar rating patterns</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Finn Coefficient vs Kappa</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Feature</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Finn Coefficient</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Cohen's/Fleiss' Kappa</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Clinical Pathology Use Cases</h4>

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

                <div style='background: #fff8e1; border-left: 4px solid #FFA000; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>Model Types</h4>
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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Interpretation</h4>
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Kendall's W?</h3>
                    <p style='margin: 0; color: #333;'>
                        Kendall's coefficient of concordance (W) measures the <strong>agreement among multiple raters</strong>
                        when ranking or rating ordinal data. W ranges from <strong>0 (no agreement)</strong> to
                        <strong>1 (perfect agreement)</strong>.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Kendall's W</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Particularly useful for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Rankings</strong> - When raters rank cases from best to worst (e.g., ranking diagnostic difficulty)</li>
                        <li><strong>Ordinal scales</strong> - Tumor grade (G1/G2/G3), severity scores, stage classifications</li>
                        <li><strong>Agreement on order</strong> - When you care whether raters rank cases in similar order</li>
                        <li><strong>Multiple raters (≥3)</strong> - Works best with 3 or more independent raters</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Interpreting Kendall's W</h4>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Kendall's W vs Other Methods</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 13px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #C2185B;'>Method</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #C2185B;'>Best For</th>
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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Clinical Pathology Use Cases</h4>

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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>Statistical Significance</h4>
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Robinson's A?</h3>
                    <p style='margin: 0; color: #333;'>
                        Robinson's A is an <strong>ordinal agreement coefficient</strong> based on the proportion of
                        <strong>concordant pairs</strong>. It measures how often raters agree on the relative ordering
                        of cases. A ranges from <strong>-1 (complete reversal)</strong> through <strong>0 (chance agreement)</strong>
                        to <strong>+1 (perfect agreement)</strong>.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Robinson's A</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Particularly useful for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Ordinal categories with meaningful order</strong> - Tumor grades (G1/G2/G3), disease severity (mild/moderate/severe)</li>
                        <li><strong>Alternative to weighted kappa</strong> - Less affected by marginal distribution imbalances</li>
                        <li><strong>Focus on ordinal agreement</strong> - Emphasizes whether raters agree on relative ranking rather than exact categories</li>
                        <li><strong>Pairwise comparisons</strong> - Best for 2 raters; extends to multiple raters via averaging</li>
                        <li><strong>Skewed distributions</strong> - More robust than kappa when category distributions are unbalanced</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Robinson's A vs Other Ordinal Measures</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Measure</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Focus</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Sensitivity</th>
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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Clinical Pathology Use Cases</h4>

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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Interpretation Guidelines</h4>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>Key Advantages</h4>
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
                    pairwise_A <- c()
                    for (i in 1:(n_raters - 1)) {
                        for (j in (i + 1):n_raters) {
                            pair_result <- private$.robinsonAPairwise(ratings_matrix[, i], ratings_matrix[, j])
                            pairwise_A <- c(pairwise_A, pair_result$A)
                        }
                    }
                    a_value <- mean(pairwise_A)
                    # Approximate SE and z-test for averaged A
                    se_value <- sd(pairwise_A) / sqrt(length(pairwise_A))
                    z_value <- a_value / se_value
                    p_value <- 2 * pnorm(-abs(z_value))
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
                z <- A / SE
                p <- 2 * pnorm(-abs(z))
            }

            return(list(A = A, SE = SE, z = z, p = p, C = C, D = D, T = T))
        },

        .populateMeanSpearmanExplanation = function() {
            # Provide educational content about Mean Spearman Rho

            html_content <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.6;'>
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Mean Spearman Rho?</h3>
                    <p style='margin: 0; color: #333;'>
                        Mean Spearman Rho (ρ) is the <strong>average rank correlation coefficient</strong> across all
                        pairs of raters. Spearman's rho measures the <strong>monotonic association</strong> between two
                        ordinal variables. It ranges from <strong>-1 (perfect negative correlation)</strong> through
                        <strong>0 (no correlation)</strong> to <strong>+1 (perfect positive correlation)</strong>.
                        For interrater agreement, high positive values indicate raters rank cases similarly.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Mean Spearman Rho</h4>
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

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Spearman Rho vs Other Ordinal Measures</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Measure</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>What It Measures</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Best Use</th>
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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Clinical Pathology Use Cases</h4>

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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Interpretation Guidelines</h4>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>Key Advantages</h4>
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Mean Pearson Correlation?</h3>
                    <p style='margin: 0; color: #333;'>
                        Mean Pearson Correlation (r) is the <strong>average linear correlation coefficient</strong> across all
                        pairs of raters for continuous measurements. Pearson's r measures the <strong>strength and direction
                        of linear association</strong> between two variables. It ranges from <strong>-1 (perfect negative)</strong>
                        through <strong>0 (no linear relationship)</strong> to <strong>+1 (perfect positive correlation)</strong>.
                        For interrater agreement, high positive values indicate raters' measurements vary together linearly.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Mean Pearson Correlation</h4>
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

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Pearson r vs Other Continuous Measures</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Measure</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>What It Measures</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Best Use</th>
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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Clinical Pathology Use Cases</h4>

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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Interpretation Guidelines</h4>
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

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>Correlation vs. Agreement</h4>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>Key Advantages</h4>
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Lin's Concordance Correlation Coefficient (CCC)?</h3>
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

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Lin's CCC</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for method comparison and validation:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Method comparison</strong> - Comparing manual vs. automated measurements</li>
                        <li><strong>Instrument validation</strong> - Assessing new equipment against gold standard</li>
                        <li><strong>Observer agreement</strong> - Evaluating measurement consistency between raters</li>
                        <li><strong>Quality control</strong> - Monitoring measurement system performance over time</li>
                        <li><strong>Biomarker validation</strong> - Comparing different assay platforms</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Interpreting CCC Values</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>CCC Value</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Agreement Strength</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Interpretation</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>&lt; 0.90</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Poor</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Substantial disagreement - methods not interchangeable</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.90 - 0.95</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Moderate</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Acceptable for some applications with caution</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.95 - 0.99</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Substantial</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Good agreement - methods reasonably interchangeable</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>&gt; 0.99</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Almost perfect</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Excellent agreement - methods highly interchangeable</td>
                        </tr>
                    </table>
                    <p style='margin: 15px 0 0 0; font-style: italic; color: #555;'>
                        Note: Thresholds are context-dependent. Clinical applications may require CCC > 0.95 for safety-critical measurements.
                    </p>
                </div>

                <div style='background: #e1f5fe; border-left: 4px solid #03A9F4; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #0277BD;'>CCC vs. Pearson's r: Key Differences</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #03A9F4;'>Characteristic</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #03A9F4;'>Pearson's r</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #03A9F4;'>Lin's CCC</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Clinical Use Cases in Pathology</h4>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>⚠️ Important Considerations</h4>
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

                <div style='background: #f3e5f5; border-left: 4px solid #9C27B0; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #7B1FA2;'>Statistical Notes</h4>
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

                        # Alternative direct formula (should give same result)
                        # ccc_alt <- 2 * pearson_r * sd_x * sd_y / (sd_x^2 + sd_y^2 + (mean_x - mean_y)^2)

                        # Confidence interval using Fisher's Z-transformation
                        # Asymptotic variance of CCC
                        n <- n_cases
                        var_ccc <- ((1 - pearson_r^2) * ccc^2 * (1 - ccc^2)) / (pearson_r^2 * n)
                        se_ccc <- sqrt(var_ccc)

                        # 95% CI using normal approximation
                        z_crit <- qnorm(0.975)
                        ci_lower <- ccc - z_crit * se_ccc
                        ci_upper <- ccc + z_crit * se_ccc

                        # Ensure CI bounds within [-1, 1]
                        ci_lower <- max(-1, ci_lower)
                        ci_upper <- min(1, ci_upper)

                        # Interpretation
                        if (ccc < 0.90) {
                            interp <- "Poor agreement - methods not interchangeable"
                        } else if (ccc < 0.95) {
                            interp <- "Moderate agreement - use with caution"
                        } else if (ccc < 0.99) {
                            interp <- "Substantial agreement - methods reasonably interchangeable"
                        } else {
                            interp <- "Almost perfect agreement - methods highly interchangeable"
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
                        n_boot <- 1000
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
                                                 "✓ Yes (Acceptable)",
                                                 "✗ No (Not Acceptable)")

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
                            pct_over <- (margin / acceptable_limit) * 100
                            interp <- sprintf("Methods NOT equivalent - TDI=%.1f exceeds acceptable limit (%.1f) by %.1f (%.0f%%). Unacceptable disagreement.",
                                             tdi_estimate, acceptable_limit, margin, pct_over)
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

                            # Calculate Wilson score confidence interval
                            if (include_ci && both_positive > 0) {
                                # Use Wilson score for the proportion both_positive / (average of rater counts)
                                # This is more stable than exact binomial for specific agreement
                                n_eff <- (rater1_positive + rater2_positive) / 2
                                p_hat <- both_positive / n_eff

                                z <- qnorm(0.975)  # 95% CI
                                denom <- 1 + z^2 / n_eff
                                center <- (p_hat + z^2 / (2 * n_eff)) / denom
                                margin <- z * sqrt(p_hat * (1 - p_hat) / n_eff + z^2 / (4 * n_eff^2)) / denom

                                # Convert back to specific agreement scale (2p)
                                ci_lower <- max(0, 2 * (center - margin))
                                ci_upper <- min(1, 2 * (center + margin))
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
            html <- paste0(html, '<li>G2↔G1 and G2↔G3 cells: Moderately dark (G2 confused with both)</li>')
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
            html <- paste0(html, '<li>2+↔1+ and 2+↔3+ cells: Both dark (2+ confused with neighbors)</li>')
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What are Rater Profile Plots?</h3>
                    <p style='margin: 0; color: #333;'>
                        Rater Profile Plots visualize the <strong>distribution of ratings</strong> for each individual rater.
                        Unlike agreement statistics that focus on concordance between raters, profile plots reveal
                        <strong>rating patterns and systematic differences</strong> in how raters use the rating scale.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Rater Profile Plots</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential when:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Agreement is low</strong> – Determine if disagreement stems from systematic rating differences vs. random variation</li>
                        <li><strong>Training evaluation</strong> – Identify raters with restricted range use or systematic over/under-grading</li>
                        <li><strong>Quality control</strong> – Detect raters who need recalibration or additional training</li>
                        <li><strong>Method validation</strong> – Ensure all raters are using the full scale appropriately</li>
                        <li><strong>Identifying outlier raters</strong> – Find raters whose distributions differ markedly from the group</li>
                    </ul>
                </div>

                <h4 style='color: #1976D2;'>Plot Types and When to Use Them</h4>
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

                <h4 style='color: #1976D2;'>Clinical Use Cases in Pathology</h4>

                <div style='background: #f9f9f9; padding: 15px; margin-bottom: 15px; border-left: 3px solid #2196F3;'>
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

                <div style='background: #f9f9f9; padding: 15px; margin-bottom: 15px; border-left: 3px solid #2196F3;'>
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

                <div style='background: #f9f9f9; padding: 15px; margin-bottom: 15px; border-left: 3px solid #2196F3;'>
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

                <div style='background: #ffe5e5; border-left: 4px solid #d32f2f; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #c62828;'>⚠️ Important Interpretation Notes</h4>
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

                <h4 style='color: #1976D2;'>What to Do Based on Profile Patterns</h4>
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

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #2E7D32;'>✓ Best Practices</h4>
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

        .raterProfilePlot = function(image, ...) {
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
                    ggplot2::theme_minimal() +
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
                    ggplot2::theme_minimal() +
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
                    ggplot2::theme_minimal() +
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>Agreement by Subgroup (Stratified Analysis)</h3>
                    <p style='margin: 0; color: #333;'>Calculate agreement separately for each subgroup to identify which case types show reliable agreement.</p>
                </div>
                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>Use Cases</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Compare agreement across tumor types (benign vs. malignant)</li>
                        <li>Assess agreement by disease stage or grade</li>
                        <li>Identify case difficulty (easy vs. hard cases)</li>
                        <li>Evaluate agreement across anatomical sites</li>
                    </ul>
                </div></div>"
            self$results$subgroupExplanation$setContent(jmvcore::format(html))
        },

        .subgroupForestPlot = function(image, ...) {
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
                ggplot2::theme_minimal() +
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

                    # Calculate appropriate agreement statistic
                    if (is_categorical) {
                        # Use Fleiss' kappa for categorical data
                        if (requireNamespace("irr", quietly = TRUE)) {
                            kappa_result <- tryCatch(irr::kappam.fleiss(sub_ratings), error = function(e) NULL)
                            if (!is.null(kappa_result)) {
                                agreement_stat <- kappa_result$value
                                stat_type <- "Fleiss' Kappa"
                                # Approximate CI using standard error
                                se <- sqrt(kappa_result$statistic / n_cases)
                                ci_lower <- agreement_stat - 1.96 * se
                                ci_upper <- agreement_stat + 1.96 * se
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>Rater Clustering</h3>
                    <p style='margin: 0; color: #333;'>Identifies groups of raters with similar rating patterns using hierarchical or k-means clustering.</p>
                </div>
                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>Use Cases</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Identify subgroups of similarly-trained raters</li>
                        <li>Detect outlier raters with unique rating patterns</li>
                        <li>Understand sources of disagreement (within vs. between groups)</li>
                        <li>Optimize panel composition for maximum diversity or homogeneity</li>
                        <li>Target training to specific rater clusters</li>
                    </ul>
                    <h4 style='margin: 10px 0 10px 0; color: #F57C00;'>Interpretation</h4>
                    <p style='margin: 0;'><strong>Dendrogram height:</strong> Lower = more similar. High first split = distinct rater groups.</p>
                    <p style='margin: 5px 0 0 0;'><strong>Cluster heatmap:</strong> Dark diagonal blocks = tight clusters. Off-diagonal = between-cluster differences.</p>
                </div></div>"
            self$results$raterClusterExplanation$setContent(jmvcore::format(html))
        },

        .raterDendrogram = function(image, ...) {
            plotState <- image$state
            if (is.null(plotState)) return(FALSE)

            hc <- plotState$hclust_obj
            cluster_labels <- plotState$cluster_labels

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

        .raterClusterHeatmap = function(image, ...) {
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

                    # Store dendrogram state
                    if (self$options$showDendrogram) {
                        dendro <- self$results$raterDendrogram
                        dendro$setState(list(
                            hclust_obj = hc,
                            cluster_labels = cluster_assign
                        ))
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>Case Clustering</h3>
                    <p style='margin: 0; color: #333;'>Identifies groups of cases with similar rating patterns across raters using hierarchical or k-means clustering.</p>
                </div>
                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>Use Cases</h4>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li>Identify subgroups of cases with consistent rating patterns</li>
                        <li>Detect controversial or difficult-to-classify cases</li>
                        <li>Understand sources of disagreement (easy vs. hard cases)</li>
                        <li>Stratify analysis by case difficulty</li>
                        <li>Identify cases requiring expert review or consensus discussion</li>
                    </ul>
                    <h4 style='margin: 10px 0 10px 0; color: #F57C00;'>Clinical Examples</h4>
                    <p style='margin: 0;'><strong>Tumor grading:</strong> Cluster cases by grade agreement. Low-agreement clusters may contain borderline cases needing expert review.</p>
                    <p style='margin: 5px 0 0 0;'><strong>Diagnostic categorization:</strong> Identify groups of cases with consistent vs. variable diagnoses across pathologists.</p>
                    <h4 style='margin: 10px 0 10px 0; color: #F57C00;'>Interpretation</h4>
                    <p style='margin: 0;'><strong>Dendrogram height:</strong> Lower = more similar rating patterns. High first split = distinct case groups.</p>
                    <p style='margin: 5px 0 0 0;'><strong>Cluster heatmap:</strong> Dark diagonal blocks = tight clusters (similar cases). Off-diagonal = between-cluster differences.</p>
                    <p style='margin: 5px 0 0 0;'><strong>Low within-cluster similarity:</strong> Cases in cluster are difficult/controversial - low agreement among raters.</p>
                </div></div>"
            self$results$caseClusterExplanation$setContent(jmvcore::format(html))
        },

        .caseDendrogram = function(image, ...) {
            plotState <- image$state
            if (is.null(plotState)) return(FALSE)

            hc <- plotState$hclust_obj
            cluster_labels <- plotState$cluster_labels

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

        .caseClusterHeatmap = function(image, ...) {
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

                    # Store dendrogram state
                    if (self$options$showCaseDendrogram) {
                        dendro <- self$results$caseDendrogram
                        dendro$setState(list(
                            hclust_obj = hc,
                            cluster_labels = cluster_assign
                        ))
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Maxwell's Random Error (RE) Index?</h3>
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

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Maxwell's RE</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for understanding error sources in:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Method comparison studies</strong> - Comparing new vs. established diagnostic methods</li>
                        <li><strong>Rater training</strong> - Identifying whether low agreement is due to systematic bias or random inconsistency</li>
                        <li><strong>Measurement system analysis</strong> - Evaluating measurement process capability</li>
                        <li><strong>Quality improvement</strong> - Targeting interventions (calibration for systematic error, training for random error)</li>
                        <li><strong>Diagnostic test validation</strong> - Understanding measurement reliability characteristics</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Interpreting RE Values</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>RE Value</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Error Type</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Intervention Strategy</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Clinical Use Cases in Pathology</h4>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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

                <div style='background: #e1f5fe; border-left: 4px solid #03A9F4; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #0277BD;'>Understanding the Decomposition</h4>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>⚠️ Important Considerations</h4>
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

                <div style='background: #f3e5f5; border-left: 4px solid #9C27B0; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #7B1FA2;'>Statistical Notes</h4>
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Inter/Intra-Rater Reliability?</h3>
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

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use This Analysis</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for test-retest reliability studies where:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Training evaluation</strong> - Assess whether trainees maintain consistent ratings over time</li>
                        <li><strong>Fatigue studies</strong> - Detect changes in rating consistency during long reading sessions</li>
                        <li><strong>Quality control</strong> - Monitor both individual consistency and team agreement</li>
                        <li><strong>Method validation</strong> - Establish both reproducibility (intra-rater) and comparability (inter-rater)</li>
                        <li><strong>Digital pathology validation</strong> - Compare glass slide vs. digital slide ratings by same pathologists</li>
                    </ul>
                </div>

                <div style='background: #e1f5fe; border-left: 4px solid #03A9F4; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #0277BD;'>Data Structure Requirements</h4>
                    <p style='margin: 0 0 10px 0;'>
                        Column names must follow a pattern to identify rater-timepoint pairs:
                    </p>
                    <div style='background: #fff; padding: 10px; border-radius: 5px; font-family: monospace;'>
                        <strong>Pattern:</strong> RaterID<span style='color: #d32f2f;'>[separator]</span>TimePoint<br>
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

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Statistical Methods Used</h4>
                    <p style='margin: 0 0 10px 0;'>The analysis automatically selects appropriate statistics based on data type:</p>
                    <ul style='margin: 0 0 10px 0; padding-left: 20px;'>
                        <li><strong>Categorical/Ordinal Data:</strong> Cohen's Kappa or Weighted Kappa for each rater's test-retest pairs</li>
                        <li><strong>Continuous Data:</strong> Intraclass Correlation Coefficient (ICC) for each rater</li>
                        <li><strong>Inter-Rater:</strong> Overall agreement metric across all raters and time points</li>
                    </ul>
                    <table style='width: 100%; border-collapse: collapse; margin-top: 15px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Reliability Value</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Interpretation</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Clinical Use Cases in Pathology</h4>

                    <div style='margin-bottom: 20px;'>
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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
                        <h5 style='color: #C2185B; margin: 10px 0 5px 0;'>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>⚠️ Important Considerations</h4>
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

                <div style='background: #f3e5f5; border-left: 4px solid #9C27B0; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #7B1FA2;'>Statistical Notes</h4>
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
                        # Continuous data - use ICC or correlation
                        # Calculate ICC(2,1) - two-way random effects, single rater
                        data_matrix <- cbind(data1, data2)

                        # Simple ICC calculation
                        n <- nrow(data_matrix)
                        k <- 2  # number of timepoints being compared

                        # Between-subject variance
                        row_means <- rowMeans(data_matrix)
                        grand_mean <- mean(data_matrix)
                        MSB <- sum((row_means - grand_mean)^2) * k / (n - 1)

                        # Within-subject variance
                        residuals <- data_matrix - row_means
                        MSW <- sum(residuals^2) / (n * (k - 1))

                        # ICC(2,1) calculation
                        icc_value <- (MSB - MSW) / (MSB + (k - 1) * MSW)

                        # Confidence interval using F-distribution
                        df1 <- n - 1
                        df2 <- n * (k - 1)
                        F_stat <- MSB / MSW
                        F_lower <- F_stat / qf(0.975, df1, df2)
                        F_upper <- F_stat / qf(0.025, df1, df2)

                        ci_lower <- (F_lower - 1) / (F_lower + k - 1)
                        ci_upper <- (F_upper - 1) / (F_upper + k - 1)

                        # P-value
                        p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)

                        # Interpretation
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

                            # Confidence interval (approximate)
                            se_kappa <- sqrt(kappa_result$var.kappa)
                            ci_lower <- kappa_value - 1.96 * se_kappa
                            ci_upper <- kappa_value + 1.96 * se_kappa

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

                        # Approximate CI
                        se_kappa <- sqrt(kappa_result$var)
                        ci_lower <- kappa_value - 1.96 * se_kappa
                        ci_upper <- kappa_value + 1.96 * se_kappa

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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is the Rater Bias Test?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Rater Bias Test uses a <strong>chi-square test</strong> to detect whether raters have
                        <strong>systematically different rating patterns</strong>. It tests the null hypothesis that
                        all raters use the rating categories with equal frequency.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use This Test</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for quality control when:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Rater training</strong> - Identifying trainees who are systematically too lenient or strict</li>
                        <li><strong>Performance monitoring</strong> - Detecting drift in rater behavior over time</li>
                        <li><strong>Multi-center studies</strong> - Ensuring consistent grading across sites</li>
                        <li><strong>Certification</strong> - Verifying raters use categories appropriately before certification</li>
                        <li><strong>Detecting systematic errors</strong> - Finding raters who consistently over-diagnose or under-diagnose</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Interpreting Results</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Result</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Interpretation</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Common Bias Patterns in Pathology</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 13px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #C2185B;'>Pattern</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #C2185B;'>Clinical Impact</th>
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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Clinical Example</h4>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>Important Considerations</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Sample size matters:</strong> Test has limited power with small samples (< 30 cases)</li>
                        <li><strong>Bias ≠ Poor agreement:</strong> Raters can be biased but still agree (all systematically lenient)</li>
                        <li><strong>Clinical context:</strong> Some bias may be acceptable (e.g., erring on side of caution)</li>
                        <li><strong>Training intervention:</strong> Significant bias often correctable with feedback and training</li>
                        <li><strong>Follow-up:</strong> After detecting bias, examine individual rater frequency distributions</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Relationship to Agreement Measures</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        Rater bias is <strong>independent from agreement</strong>. You can have:<br><br>
                        ✅ <strong>High agreement + No bias:</strong> Ideal situation<br>
                        ⚠️ <strong>High agreement + Bias:</strong> All raters systematically lenient/strict together<br>
                        ⚠️ <strong>Low agreement + No bias:</strong> Random disagreement but no systematic patterns<br>
                        ❌ <strong>Low agreement + Bias:</strong> Both random and systematic errors present
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is the Bhapkar Test?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Bhapkar test is a <strong>chi-square test for marginal homogeneity</strong> between
                        <strong>two raters</strong> with <strong>multiple categories</strong>. It tests whether two
                        raters use rating categories with equal frequency. This is the extension of <strong>McNemar's test</strong>
                        (which is limited to 2×2 tables) to larger contingency tables.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Bhapkar Test</h4>
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

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Bhapkar vs. Related Tests</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Test</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Raters</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Categories</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Purpose</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Clinical Pathology Use Cases</h4>

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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Interpretation</h4>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>Requirements and Assumptions</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Exactly 2 raters:</strong> Test designed for paired comparisons only</li>
                        <li><strong>Same subjects rated twice:</strong> Data must be paired (same cases)</li>
                        <li><strong>Multiple categories (&gt;2):</strong> For 2×2 tables, use McNemar's test instead</li>
                        <li><strong>Large sample:</strong> More reliable with n &gt; 30 cases</li>
                        <li><strong>Categorical data:</strong> Ordered or unordered categories</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Complementary Analyses</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        Use Bhapkar test <strong>together with</strong>:<br><br>

                        ✅ <strong>Cohen's Kappa:</strong> Measures agreement (Bhapkar tests marginal homogeneity)<br>
                        ✅ <strong>Confusion matrix:</strong> Shows specific disagreement patterns<br>
                        ✅ <strong>Marginal frequency tables:</strong> Identify which categories differ most<br><br>

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
                self$results$bhapkarTable$setRow(rowNo = 1, values = list(
                    method = "Bhapkar test for marginal homogeneity",
                    subjects = bhapkar_result$subjects,
                    raters = bhapkar_result$raters,
                    chisq = bhapkar_result$statistic,
                    df = bhapkar_result$stat.name,
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is the Stuart-Maxwell Test?</h3>
                    <p style='margin: 0; color: #333;'>
                        The Stuart-Maxwell test is a <strong>classic chi-square test for marginal homogeneity</strong>
                        between <strong>two raters</strong> with <strong>multiple categories</strong>. It tests whether
                        two raters use rating categories with equal frequency in <strong>matched/paired data</strong>.
                        This is the traditional extension of <strong>McNemar's test</strong> (which is for 2×2 tables) to
                        larger contingency tables.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Stuart-Maxwell Test</h4>
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

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Stuart-Maxwell vs. Related Tests</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Test</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Categories</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Characteristics</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Clinical Pathology Use Cases</h4>

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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Interpretation</h4>
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

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>Assumptions and Requirements</h4>
                    <ul style='margin: 0; padding-left: 20px; font-size: 13px;'>
                        <li><strong>Paired/matched data:</strong> Same subjects rated twice or same cases by two raters</li>
                        <li><strong>Two raters only:</strong> Designed for comparing exactly 2 sets of ratings</li>
                        <li><strong>Multiple categories (&gt;2):</strong> Use McNemar for 2×2 tables</li>
                        <li><strong>Categorical data:</strong> Works with nominal or ordinal categories</li>
                        <li><strong>Sample size:</strong> More reliable with n &gt; 30; consider Bhapkar for n &gt; 50</li>
                        <li><strong>Complete pairs:</strong> Cases with missing data are excluded</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Use Together With</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        <strong>Complementary analyses:</strong><br><br>

                        ✅ <strong>Cohen's Kappa:</strong> Measures agreement (Stuart-Maxwell tests marginal distributions)<br>
                        ✅ <strong>Contingency table:</strong> Shows exact disagreement patterns cell-by-cell<br>
                        ✅ <strong>McNemar-Bowker:</strong> Provides cell-by-cell symmetry test<br>
                        ✅ <strong>Marginal frequency comparison:</strong> Identifies which categories differ most<br><br>

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
                self$results$stuartMaxwellTable$setRow(rowNo = 1, values = list(
                    method = "Stuart-Maxwell test for marginal homogeneity",
                    subjects = stuart_result$subjects,
                    raters = stuart_result$raters,
                    chisq = stuart_result$statistic,
                    df = stuart_result$stat.name,
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is Pairwise Kappa Analysis?</h3>
                    <p style='margin: 0; color: #333;'>
                        Pairwise Kappa Analysis compares <strong>each rater individually</strong> against a
                        <strong>reference rater</strong> (gold standard, consensus, or senior expert).
                        Each comparison produces a separate Cohen's kappa measuring agreement between that
                        rater and the reference.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use Pairwise Analysis</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Essential for:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Training assessment</strong> - Compare trainees vs expert to measure learning progress</li>
                        <li><strong>Rater certification</strong> - Verify raters meet minimum kappa threshold vs gold standard</li>
                        <li><strong>Performance monitoring</strong> - Track individual rater agreement with reference over time</li>
                        <li><strong>Quality control</strong> - Identify specific raters who need retraining</li>
                        <li><strong>Competency evaluation</strong> - Rank raters by performance for advancement decisions</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Interpreting Kappa Values</h4>
                    <table style='width: 100%; border-collapse: collapse;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Kappa</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Agreement Level</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Training Status</th>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>κ < 0.40</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Poor to fair</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>⚠️ Needs significant training</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.40-0.60</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Moderate</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>⚠️ Additional training recommended</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'><strong>0.60-0.75</strong></td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>Substantial</td>
                            <td style='padding: 8px; border-bottom: 1px solid #ddd;'>✅ Acceptable performance</td>
                        </tr>
                        <tr>
                            <td style='padding: 8px;'><strong>κ > 0.75</strong></td>
                            <td style='padding: 8px;'>Excellent</td>
                            <td style='padding: 8px;'>✅ Certified/Expert level</td>
                        </tr>
                    </table>
                </div>

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Choosing the Reference Rater</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 13px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #C2185B;'>Reference Type</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #C2185B;'>Use Case</th>
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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>Clinical Example: Trainee Certification</h4>
                    <p style='margin: 0; padding: 10px; background: white; border-radius: 4px; font-size: 13px;'>
                        <strong>Scenario:</strong> Five pathology residents (Raters 1-5) grade 100 tumor samples.
                        A senior pathologist provides reference diagnoses.<br><br>
                        <strong>Results:</strong><br>
                        • Resident 1: κ = 0.82 (Excellent) ✅ Ready for certification<br>
                        • Resident 2: κ = 0.68 (Substantial) ✅ Acceptable, continue monitoring<br>
                        • Resident 3: κ = 0.52 (Moderate) ⚠️ Additional training needed<br>
                        • Resident 4: κ = 0.45 (Moderate) ⚠️ Review difficult cases with expert<br>
                        • Resident 5: κ = 0.28 (Poor) ❌ Requires intensive retraining<br><br>
                        <strong>Action:</strong> Residents 1-2 certified. Residents 3-5 receive targeted training
                        based on specific error patterns, then retest after 3 months.
                    </p>
                </div>

                <div style='background: #fff9c4; border-left: 4px solid #FBC02D; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57F17;'>Ranking Raters</h4>
                    <p style='margin: 0; font-size: 13px;'>
                        When <strong>Rank Raters by Performance</strong> is enabled, raters are sorted from highest
                        to lowest kappa. This identifies:<br><br>
                        ✅ <strong>Top performers:</strong> Candidates for senior roles, teaching positions, or gold standard rating<br>
                        ⚠️ <strong>Middle performers:</strong> Acceptable but could benefit from continued training<br>
                        ❌ <strong>Low performers:</strong> Require immediate intervention or exclusion from study
                    </p>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>Pairwise vs Overall Agreement</h4>
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
                <div style='background: #f0f8ff; border-left: 4px solid #2196F3; padding: 15px; margin-bottom: 20px;'>
                    <h3 style='margin: 0 0 10px 0; color: #1976D2;'>What is ICC (Intraclass Correlation Coefficient)?</h3>
                    <p style='margin: 0; color: #333;'>
                        ICC measures the reliability and agreement of <strong>continuous measurements</strong>
                        between raters. It's the gold standard for assessing inter-rater reliability with numeric data.
                    </p>
                </div>

                <div style='background: #fff3e0; border-left: 4px solid #FF9800; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>When to Use ICC</h4>
                    <p style='margin: 0 0 10px 0;'><strong>Use ICC for continuous measurements:</strong></p>
                    <ul style='margin: 0; padding-left: 20px;'>
                        <li><strong>Tumor size</strong> – Diameter measurements in mm or cm</li>
                        <li><strong>Biomarker quantification</strong> – Protein expression levels, cell counts</li>
                        <li><strong>Morphometric analysis</strong> – Nuclear size, gland area measurements</li>
                        <li><strong>Scoring systems</strong> – Continuous scores (e.g., 0-100 scale)</li>
                    </ul>
                </div>

                <div style='background: #e8f5e9; border-left: 4px solid #4CAF50; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #388E3C;'>ICC Model Selection Guide</h4>
                    <table style='width: 100%; border-collapse: collapse; font-size: 13px;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Model</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Design</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #388E3C;'>Use Case</th>
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

                <div style='background: #fff8e1; border-left: 4px solid #FFA000; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #F57C00;'>Technical Reference: irr::icc() Parameters</h4>
                    <p style='margin: 0 0 10px 0; font-size: 13px; color: #555;'>
                        This table shows the exact parameters passed to the <code>irr::icc()</code> function for each model.
                        Useful for understanding implementation details and replicating results in R.
                    </p>
                    <table style='width: 100%; border-collapse: collapse; font-size: 12px; font-family: monospace;'>
                        <tr style='background: #f5f5f5;'>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #FFA000;'>Model</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #FFA000;'>irr::icc Parameters</th>
                            <th style='padding: 8px; text-align: left; border-bottom: 2px solid #FFA000;'>Use Case</th>
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

                <div style='background: #fce4ec; border-left: 4px solid #E91E63; padding: 15px; margin-bottom: 20px;'>
                    <h4 style='margin: 0 0 10px 0; color: #C2185B;'>Interpreting ICC Values</h4>
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

                <div style='background: #e3f2fd; border-left: 4px solid #1976D2; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; color: #1565C0;'>ICC vs Bland-Altman</h4>
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
                    conf.level = 0.95
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
                    <h3 style='color: #2E7D32; border-bottom: 2px solid #4CAF50; padding-bottom: 10px;'>
                        Iota Coefficient for Multivariate Interrater Agreement
                    </h3>

                    <div style='background: #E8F5E9; padding: 15px; border-left: 4px solid #4CAF50; margin: 20px 0;'>
                        <h4 style='margin: 0 0 10px 0; color: #2E7D32;'>What is Iota?</h4>
                        <p style='margin: 0; line-height: 1.6;'>
                            Iota (ι) is a <strong>chance-corrected agreement index</strong> for multivariate observations. Unlike ICC which
                            analyzes one variable at a time, Iota assesses <strong>overall agreement across multiple variables simultaneously</strong>.
                        </p>
                    </div>

                    <div style='background: #FFF3E0; padding: 15px; border-left: 4px solid #FF9800; margin: 20px 0;'>
                        <h4 style='margin: 0 0 10px 0; color: #E65100;'>Clinical Pathology Use Cases</h4>

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

                    <div style='background: white; padding: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>
                        <h4 style='margin: 0 0 10px 0;'>Interpretation</h4>
                        <p><strong>< 0.40:</strong> Poor | <strong>0.40-0.60:</strong> Fair | <strong>0.60-0.75:</strong> Good | <strong>0.75-0.90:</strong> Very good | <strong>> 0.90:</strong> Excellent</p>
                    </div>
                </div>
            "

            self$results$iotaExplanation$setContent(html_content)
        },

        .calculateGwetAC = function(ratings) {
            # Calculate Gwet's AC1 (unweighted) or AC2 (weighted) coefficient
            # More stable than kappa for high agreement or rare categories

            # Check if irrCAC package is available
            if (!requireNamespace("irrCAC", quietly = TRUE)) {
                self$results$gwetTable$setNote(
                    "error",
                    "The 'irrCAC' package is required for Gwet's AC calculation but is not installed. Please install it with: install.packages('irrCAC')"
                )
                return()
            }

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
                # Use gwet.ac.raw for raw agreement data
                result <- irrCAC::gwet.ac.raw(
                    ratings = ratings_matrix,
                    weights = weight_param,
                    conflev = 0.95,
                    N = Inf,  # Assume infinite population
                    print = FALSE
                )

                # Extract results
                coef <- result$est$coefficient
                se <- result$est$se
                ci_lower <- result$est$conf.int[1]
                ci_upper <- result$est$conf.int[2]

                # Calculate p-value (two-tailed test against H0: AC = 0)
                z_stat <- coef / se
                p_value <- 2 * (1 - pnorm(abs(z_stat)))

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
                        note <- "✓ Suitable for weighted kappa"
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
            # Hierarchical/Multilevel Kappa Analysis for nested data structures
            # TODO: Full implementation requires multilevel modeling (lme4/nlme packages)

            # Validate cluster variable
            if (is.null(cluster_data) || length(cluster_data) == 0) {
                self$results$hierarchicalOverallTable$setNote(
                    "error",
                    "Please select a cluster/institution variable to perform hierarchical analysis."
                )
                return()
            }

            # For now, provide informative placeholder
            self$results$hierarchicalOverallTable$setNote(
                "info",
                "Hierarchical/Multilevel Kappa analysis is currently under development. This advanced feature will provide: (1) Overall kappa accounting for clustering, (2) Cluster-specific estimates with shrinkage, (3) Variance decomposition (between/within clusters), (4) ICC decomposition, (5) Homogeneity testing across sites. Full implementation coming in next release."
            )

            # TODO: Implement the following components when complete:
            # - Overall hierarchical kappa using multilevel model
            # - Cluster-specific kappa estimates
            # - Variance component decomposition (between-cluster, within-cluster)
            # - Hierarchical ICC (ICC1, ICC2, ICC3)
            # - Homogeneity test (Breslow-Day or Q-statistic)
            # - Empirical Bayes shrinkage estimates
            # - Cluster rankings with confidence intervals
        },

        .populateHierarchicalExplanation = function() {
            # Generate comprehensive HTML explanation for hierarchical kappa

            html <- "<div style='font-family: Arial, sans-serif; padding: 15px; line-height: 1.6;'>"

            html <- paste0(html, "
                <h3 style='color: #2E5090; margin-top: 0;'>Hierarchical/Multilevel Kappa Analysis</h3>

                <div style='background-color: #F0F7FF; padding: 12px; border-left: 4px solid #2E5090; margin-bottom: 15px;'>
                    <strong>What is it?</strong><br/>
                    Hierarchical kappa extends standard kappa to account for nested data structures where raters
                    are grouped within clusters (institutions, centers, scanners). It decomposes agreement into
                    between-cluster and within-cluster components, providing more accurate estimates when clustering exists.
                </div>

                <h4 style='color: #2E5090; margin-top: 20px;'>When to Use</h4>
                <ul>
                    <li><strong>Multi-center trials</strong>: Pathologists nested within hospitals</li>
                    <li><strong>Multi-scanner studies</strong>: Radiologists nested within imaging centers</li>
                    <li><strong>Training programs</strong>: Residents nested within training sites</li>
                    <li><strong>Quality control</strong>: Identifying institutions with poor agreement</li>
                </ul>

                <h4 style='color: #2E5090; margin-top: 20px;'>Key Components</h4>
                <ul>
                    <li><strong>Overall Hierarchical Kappa</strong>: Population-level agreement accounting for clustering</li>
                    <li><strong>Cluster-Specific Estimates</strong>: Kappa for each institution/center</li>
                    <li><strong>Variance Decomposition</strong>: Between-cluster vs within-cluster variance</li>
                    <li><strong>Homogeneity Testing</strong>: Are all clusters performing equally?</li>
                    <li><strong>Shrinkage Estimates</strong>: Stabilized estimates for small clusters</li>
                </ul>

                <h4 style='color: #2E5090; margin-top: 20px;'>Interpreting Variance Components</h4>
                <ul>
                    <li><strong>High between-cluster variance</strong>: Institutional differences (protocols, training)</li>
                    <li><strong>High within-cluster variance</strong>: Local rater disagreement</li>
                    <li><strong>Shrinkage</strong>: Pulls extreme cluster estimates toward overall mean</li>
                </ul>

                <div style='background-color: #FFF9E6; padding: 12px; border-left: 4px solid #FFA500; margin-top: 15px;'>
                    <strong>Note:</strong> Full hierarchical analysis is computationally intensive and requires sufficient
                    data within each cluster (typically ≥10 cases per cluster recommended).
                </div>
            </div>")

            self$results$hierarchicalExplanation$setContent(html)
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

            # Level Ordering Information (if requested) ----
            if (self$options$showLevelInfo) {
                private$.populateLevelInfo(ratings)
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
                if (nrow(na.omit(ratings)) > 0) {
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

                # Clinical use cases guide
                private$.populateClinicalUseCases()
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

        # Light's Kappa (if requested) ----
        if (self$options$lightKappa) {
            private$.populateLightKappaExplanation()
            private$.calculateLightKappa(ratings)
        }

        # Finn Coefficient (if requested) ----
        if (self$options$finn) {
            private$.populateFinnExplanation()
            private$.calculateFinn(ratings)
        }

        # Kendall's W (if requested) ----
        if (self$options$kendallW) {
            private$.populateKendallWExplanation()
            private$.calculateKendallW(ratings)
        }

        # Robinson's A (if requested) ----
        if (self$options$robinsonA || self$options$showRobinsonAGuide) {
            private$.populateRobinsonAExplanation()
        }
        if (self$options$robinsonA) {
            private$.calculateRobinsonA(ratings)
        }

        # Mean Spearman Rho (if requested) ----
        if (self$options$meanSpearman || self$options$showMeanSpearmanGuide) {
            private$.populateMeanSpearmanExplanation()
        }
        if (self$options$meanSpearman) {
            private$.calculateMeanSpearman(ratings)
        }

        # Rater Bias Test (if requested) ----
        if (self$options$raterBias) {
            private$.populateRaterBiasExplanation()
            private$.calculateRaterBias(ratings)
        }

        # Bhapkar Test (if requested) ----
        if (self$options$bhapkar) {
            private$.populateBhapkarExplanation()
            private$.calculateBhapkar(ratings)
        }

        # Stuart-Maxwell Test (if requested) ----
        if (self$options$stuartMaxwell) {
            private$.populateStuartMaxwellExplanation()
            private$.calculateStuartMaxwell(ratings)
        }

        # Pairwise Kappa Analysis (if requested) ----
        if (self$options$pairwiseKappa) {
            private$.populatePairwiseKappaExplanation()

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

        # Gwet's AC1/AC2 (if requested) ----
        if (self$options$gwet) {
            private$.populateGwetExplanation()
            private$.calculateGwetAC(ratings)
        }

        # ICC (if requested) ----
        if (self$options$icc) {
            private$.populateICCExplanation()
            private$.calculateICC(ratings)
        }

        # Mean Pearson Correlation (if requested) ----
        if (self$options$meanPearson || self$options$showMeanPearsonGuide) {
            private$.populateMeanPearsonExplanation()
        }
        if (self$options$meanPearson) {
            private$.calculateMeanPearson(ratings)
        }

        # Lin's Concordance Correlation Coefficient (if requested) ----
        if (self$options$linCCC || self$options$showLinCCCGuide) {
            private$.populateLinCCCExplanation()
        }
        if (self$options$linCCC) {
            private$.calculateLinCCC(ratings)
        }

        # Total Deviation Index (if requested) ----
        if (self$options$tdi || self$options$showTDIGuide) {
            private$.populateTDIExplanation()
        }
        if (self$options$tdi) {
            private$.calculateTDI(ratings)
        }

        # Specific Agreement Indices (if requested) ----
        if (self$options$specificAgreement || self$options$showSpecificAgreementGuide) {
            private$.populateSpecificAgreementExplanation()
        }
        if (self$options$specificAgreement) {
            private$.calculateSpecificAgreement(ratings)
        }

        # Agreement Heatmap (if requested) ----
        if (self$options$agreementHeatmap || self$options$showAgreementHeatmapGuide) {
            private$.populateAgreementHeatmapExplanation()
        }
        if (self$options$agreementHeatmap) {
            private$.populateAgreementHeatmap(ratings)
        }

        # Rater Profile Plots (if requested) ----
        if (self$options$raterProfiles || self$options$showRaterProfileGuide) {
            private$.populateRaterProfileExplanation()
        }
        if (self$options$raterProfiles) {
            private$.populateRaterProfiles(ratings)
        }

        # Agreement by Subgroup (if requested) ----
        if (self$options$agreementBySubgroup || self$options$showSubgroupGuide) {
            private$.populateSubgroupExplanation()
        }
        if (self$options$agreementBySubgroup) {
            private$.calculateAgreementBySubgroup(ratings)
        }

        # Rater Clustering (if requested) ----
        if (self$options$raterClustering || self$options$showRaterClusterGuide) {
            private$.populateRaterClusterExplanation()
        }
        if (self$options$raterClustering) {
            private$.performRaterClustering(ratings)
        }

        # Case Clustering (if requested) ----
        if (self$options$caseClustering || self$options$showCaseClusterGuide) {
            private$.populateCaseClusterExplanation()
        }
        if (self$options$caseClustering) {
            private$.performCaseClustering(ratings)
        }

        # Maxwell's Random Error Index (if requested) ----
        if (self$options$maxwellRE || self$options$showMaxwellREGuide) {
            private$.populateMaxwellREExplanation()
        }
        if (self$options$maxwellRE) {
            private$.calculateMaxwellRE(ratings)
        }

        # Inter/Intra-Rater Reliability (if requested) ----
        if (self$options$interIntraRater || self$options$showInterIntraRaterGuide) {
            private$.populateInterIntraRaterExplanation()
        }
        if (self$options$interIntraRater) {
            private$.calculateInterIntraRater(ratings)
        }

        # Iota Coefficient (if requested) ----
        if (self$options$iota) {
            private$.populateIotaExplanation()
            private$.calculateIota(ratings)
        }

        # Consensus Variable Calculation (if requested) ----
        if (self$options$consensusVar) {
            private$.createConsensusVariable(ratings)
        }

        # Level of Agreement Variable (if requested) ----
        if (self$options$loaVariable) {
            private$.calculateLevelOfAgreement(ratings)
        }

        # Hierarchical/Multilevel Kappa (if requested) ----
        if (self$options$hierarchicalKappa) {
            # Populate explanation if requested
            if (self$options$showAbout) {
                private$.populateHierarchicalExplanation()
            }

            # Get cluster variable data
            cluster_data <- NULL
            if (!is.null(self$options$clusterVariable)) {
                cluster_data <- jmvcore::select(mydata, self$options$clusterVariable)
            }

            private$.calculateHierarchicalKappa(ratings, cluster_data)
        }

        # Bland-Altman analysis (if requested) ----
        if (self$options$blandAltmanPlot) {
            private$.populateBlandAltman(ratings)
        }

        }  # End of .run function

    ),  # End of private list

    public = list(
        # #' @description
        # #' Generate R source code for Interrater Reliability analysis
        # #' @return Character string with R syntax for reproducible analysis outside jamovi
        # TEMPORARILY COMMENTED OUT TO PREVENT ERRORS
        # asSource = function() {
        #     vars <- self$options$vars
        #
        #     # Return empty string if insufficient variables
        #     if (is.null(vars) || length(vars) < 2) {
        #         return('')
        #     }

        #             # Escape variable names that need backticks
        #             vars_escaped <- sapply(vars, function(v) {
        #                 private$.escapeVariableName(v)
        #             })
        # 
        #             # Build vars argument for function call
        #             # Each variable name is quoted, with backticks if needed
        #             vars_arg <- paste0('vars = c(',
        #                              paste(sapply(vars_escaped, function(v) {
        #                                  # If already has backticks, preserve them in quotes
        #                                  if (grepl("^`.*`$", v)) {
        #                                      paste0('"', v, '"')
        #                                  } else {
        #                                      paste0('"', v, '"')
        #                                  }
        #                              }), collapse = ', '),
        #                              ')')
        # 
        #             # Build other arguments
        #             args_list <- c(vars_arg)
        # 
        #             # Add weighted kappa option if not default
        #             if (self$options$wght != "unweighted") {
        #                 wght_arg <- paste0('wght = "', self$options$wght, '"')
        #                 args_list <- c(args_list, wght_arg)
        #             }
        # 
        #             # Add exact kappa option if enabled
        #             if (self$options$exct) {
        #                 args_list <- c(args_list, 'exct = TRUE')
        #             }
        # 
        #             # Add Krippendorff's alpha options if enabled
        #             if (self$options$kripp) {
        #                 args_list <- c(args_list, 'kripp = TRUE')
        # 
        #                 if (self$options$krippMethod != "nominal") {
        #                     args_list <- c(args_list, paste0('krippMethod = "', self$options$krippMethod, '"'))
        #                 }
        # 
        #                 if (self$options$bootstrap) {
        #                     args_list <- c(args_list, 'bootstrap = TRUE')
        #                 }
        #             }
        # 
        #             # Add display options if enabled
        #             if (self$options$sft) {
        #                 args_list <- c(args_list, 'sft = TRUE')
        #             }
        # 
        #             if (self$options$showSummary) {
        #                 args_list <- c(args_list, 'showSummary = TRUE')
        #             }
        # 
        #             # Build final R code
        #             code <- paste0(
        #                 '# Interrater Reliability Analysis\n',
        #                 '# Generated by ClinicoPath jamovi module\n\n',
        #                 'library(ClinicoPath)\n\n',
        #                 '# Load your data\n',
        #                 '# data <- read.csv("your_data.csv")\n\n',
        #                 'agreement(\n',
        #                 '    data = data,\n',
        #                 '    ', paste(args_list, collapse = ',\n    '),
        #                 '\n)'
        #             )
        #
        #     return(code)
        # }
    )  # End of public list
    )
