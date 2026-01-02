#' @title Interrater Reliability Analysis
#' @description Calculate interrater reliability for multiple raters using kappa statistics,
#'   Krippendorff's alpha, and Gwet's AC. Supports weighted kappa for ordinal data,
#'   hierarchical/multilevel analysis, and consensus score derivation.
#'
#' @param data The dataset containing ratings
#' @param vars Character vector of rater variable names (minimum 2 required)
#' @param wght Weighting scheme for kappa: "unweighted" (default), "equal", or "squared".
#'   Use weighted kappa for ordinal data to account for degree of disagreement.
#' @param exct Logical; use exact p-values for 3+ raters (default: FALSE)
#' @param kripp Logical; calculate Krippendorff's alpha (default: FALSE)
#' @param krippMethod Data type for Krippendorff's alpha: "nominal", "ordinal", "interval", "ratio"
#' @param bootstrap Logical; calculate bootstrap confidence intervals for Krippendorff's alpha
#' @param gwet Logical; calculate Gwet's AC1/AC2 coefficient (default: FALSE)
#' @param gwetWeights Weights for Gwet's AC: "unweighted", "linear", "quadratic"
#' @param showLevelInfo Logical; display variable level ordering information
#' @param hierarchicalKappa Logical; enable hierarchical/multilevel kappa analysis
#' @param clusterVariable Variable defining clusters/institutions for hierarchical analysis
#' @param baConfidenceLevel Confidence level for Bland-Altman limits of agreement (default: 0.95)
#' @param proportionalBias Logical; test for proportional bias in Bland-Altman plot
#' @param blandAltmanPlot Logical; generate Bland-Altman plot for continuous agreement
#' @param sft Logical; display frequency tables
#' @param showSummary Logical; display plain-language interpretation summary
#' @param showAbout Logical; display analysis explanation panel
#' @param consensusVar Logical; create consensus variable from multiple raters
#' @param consensusRule Consensus rule: "majority", "supermajority", "unanimous"
#' @param tieBreaker Tie handling: "exclude", "first", "lowest", "highest"
#' @param consensusName Name for the computed consensus variable
#' @param referenceRater Reference rater for pairwise comparisons
#' @param rankRaters Logical; rank raters by kappa relative to reference
#' @param loaVariable Logical; create level of agreement categorical variable
#' @param loaThresholds LoA thresholds: "custom", "quartiles", "tertiles"
#' @param loaHighThreshold High LoA threshold percentage (default: 75)
#' @param loaLowThreshold Low LoA threshold percentage (default: 56)
#'
#' @return jamovi results object containing kappa statistics, tables, and optional outputs
#'
#' @details
#' The function calculates Cohen's kappa for two raters and Fleiss' kappa for three or more raters.
#' Weighted kappa accounts for degree of disagreement in ordinal data. Krippendorff's alpha
#' handles missing data. Gwet's AC corrects for paradoxical kappa behavior with high agreement rates.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @importFrom irr kappa2 kappam.fleiss agree
#' @importFrom dplyr select group_by count
#' @importFrom htmlTable htmlTable
#' @importFrom glue glue
#'
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal scales.
#' \emph{Educational and Psychological Measurement}, 20(1), 37-46.
#'
#' Cohen, J. (1968). Weighted kappa: Nominal scale agreement with provision
#' for scaled disagreement or partial credit. \emph{Psychological Bulletin}, 70(4), 213-220.
#'
#' Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters.
#' \emph{Psychological Bulletin}, 76(5), 378-382.
#'
#' Landis, J. R., & Koch, G. G. (1977). The measurement of observer agreement for
#' categorical data. \emph{Biometrics}, 33(1), 159-174.
#'
#' Conger, A. J. (1980). Integration and generalization of kappas for multiple raters.
#' \emph{Psychological Bulletin}, 88(2), 322-328.
#'
#'


# See
# \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}


agreementClass <- if (requireNamespace("jmvcore")) R6::R6Class("agreementClass",
    inherit = agreementBase, private = list(

        .escapeVar = function(x) {
            # Escape variable names with spaces/special characters
            # Mimic jmvcore::composeTerm behavior
            if (is.null(x) || length(x) == 0) return(character(0))

            # If already backtick-quoted, return as-is
            if (grepl("^`.*`$", x)) return(x)

            # If contains spaces or special chars, backtick-quote
            if (grepl("[^A-Za-z0-9_.]", x)) {
                return(paste0("`", x, "`"))
            }

            return(x)
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

        .populateBlandAltmanPlot = function(image, ...) {
            # Only for 2 raters with continuous data
            if (length(self$options$vars) != 2) {
                return(FALSE)
            }

            mydata <- self$data
            ratings <- mydata[, self$options$vars, drop = FALSE]

            # Check if data is continuous (numeric)
            if (!all(sapply(ratings, is.numeric))) {
                return(FALSE)
            }

            # Remove missing values
            ratings <- na.omit(ratings)
            if (nrow(ratings) == 0) {
                return(FALSE)
            }

            # Calculate mean and difference
            mean_val <- rowMeans(ratings)
            diff_val <- ratings[[1]] - ratings[[2]]

            # Calculate limits of agreement
            conf_level <- self$options$baConfidenceLevel
            z_crit <- qnorm((1 + conf_level) / 2)

            mean_diff <- mean(diff_val)
            sd_diff <- sd(diff_val)

            loa_upper <- mean_diff + z_crit * sd_diff
            loa_lower <- mean_diff - z_crit * sd_diff

            # Create plot data
            plotData <- data.frame(
                mean = mean_val,
                diff = diff_val
            )

            # Test for proportional bias (if requested)
            prop_bias_text <- ""
            if (self$options$proportionalBias) {
                lm_result <- lm(diff ~ mean, data = plotData)
                p_val <- summary(lm_result)$coefficients[2, 4]

                if (p_val < 0.05) {
                    prop_bias_text <- sprintf("\nProportional bias detected (p = %.4f)", p_val)
                } else {
                    prop_bias_text <- sprintf("\nNo proportional bias (p = %.4f)", p_val)
                }
            }

            # Create plot
            plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = mean, y = diff)) +
                ggplot2::geom_point(alpha = 0.6, size = 2) +
                ggplot2::geom_hline(yintercept = mean_diff, linetype = "solid", color = "blue", size = 1) +
                ggplot2::geom_hline(yintercept = loa_upper, linetype = "dashed", color = "red", size = 0.8) +
                ggplot2::geom_hline(yintercept = loa_lower, linetype = "dashed", color = "red", size = 0.8) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "gray50", size = 0.5) +
                ggplot2::labs(
                    title = "Bland-Altman Plot",
                    subtitle = paste0(
                        "Mean difference = ", round(mean_diff, 2),
                        "\nLimits of agreement: [", round(loa_lower, 2), ", ", round(loa_upper, 2), "]",
                        prop_bias_text
                    ),
                    x = paste0("Mean of ", names(ratings)[1], " and ", names(ratings)[2]),
                    y = paste0("Difference (", names(ratings)[1], " - ", names(ratings)[2], ")")
                ) +
                ggplot2::theme_minimal(base_size = 12) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 10)
                )

            # Set plot state
            image$setState(list(
                data = plotData,
                mean_diff = mean_diff,
                loa_upper = loa_upper,
                loa_lower = loa_lower
            ))

            print(plot)

            return(TRUE)
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
                    if (self$options$sft) {
                        # For 2 raters, create a 2x2 contingency table
                        if (length(self$options$vars) == 2) {
                            # Create contingency table
                            cont_table <- table(ratings[[1]], ratings[[2]])

                            # Format as text output (like janitor::tabyl)
                            text_output <- paste0(
                                "\nContingency Table: ", names(ratings)[1], " vs ", names(ratings)[2], "\n",
                                paste(rep("-", 60), collapse = ""), "\n\n"
                            )

                            # Create formatted table
                            capture_output <- capture.output({
                                print(addmargins(cont_table))
                                cat("\n")
                                cat("Row Percentages:\n")
                                print(round(prop.table(cont_table, 1) * 100, 1))
                                cat("\n")
                                cat("Column Percentages:\n")
                                print(round(prop.table(cont_table, 2) * 100, 1))
                            })

                            self$results$text$setContent(paste(capture_output, collapse = "\n"))

                            # Compact HTML version
                            n_row <- nrow(cont_table)
                            n_col <- ncol(cont_table)

                            html_table <- paste0(
                                "<div style='font-family: monospace; font-size: 12px; overflow-x: auto;'>",
                                "<p style='font-weight: bold; margin-bottom: 8px;'>",
                                names(ratings)[1], " vs ", names(ratings)[2], "</p>",
                                "<table style='border-collapse: collapse;'>",
                                "<tr><th style='border: 1px solid #999; padding: 6px; background: #f5f5f5;'></th>"
                            )

                            # Column headers
                            for (j in 1:n_col) {
                                html_table <- paste0(html_table,
                                    "<th style='border: 1px solid #999; padding: 6px; background: #f5f5f5; text-align: center;'>",
                                    colnames(cont_table)[j], "</th>")
                            }
                            html_table <- paste0(html_table, "<th style='border: 1px solid #999; padding: 6px; background: #e8e8e8;'>Total</th></tr>")

                            # Data rows
                            for (i in 1:n_row) {
                                html_table <- paste0(html_table,
                                    "<tr><th style='border: 1px solid #999; padding: 6px; background: #f5f5f5;'>",
                                    rownames(cont_table)[i], "</th>")

                                row_total <- sum(cont_table[i, ])
                                for (j in 1:n_col) {
                                    pct <- round(cont_table[i, j] / row_total * 100, 1)
                                    html_table <- paste0(html_table,
                                        "<td style='border: 1px solid #999; padding: 6px; text-align: center;'>",
                                        cont_table[i, j], "<br><span style='font-size: 11px; color: #666;'>(",
                                        pct, "%)</span></td>")
                                }
                                html_table <- paste0(html_table,
                                    "<td style='border: 1px solid #999; padding: 6px; background: #f9f9f9; text-align: center;'>",
                                    row_total, "</td></tr>")
                            }

                            # Total row
                            html_table <- paste0(html_table,
                                "<tr><th style='border: 1px solid #999; padding: 6px; background: #e8e8e8;'>Total</th>")
                            grand_total <- sum(cont_table)
                            for (j in 1:n_col) {
                                col_total <- sum(cont_table[, j])
                                html_table <- paste0(html_table,
                                    "<td style='border: 1px solid #999; padding: 6px; background: #f9f9f9; text-align: center;'>",
                                    col_total, "</td>")
                            }
                            html_table <- paste0(html_table,
                                "<td style='border: 1px solid #999; padding: 6px; background: #e8e8e8; text-align: center; font-weight: bold;'>",
                                grand_total, "</td></tr>",
                                "</table></div>")

                            self$results$text2$setContent(html_table)

                        } else {
                            # For 3+ raters, show combination counts
                            freq_table <- ratings %>%
                                dplyr::group_by_all() %>%
                                dplyr::count() %>%
                                as.data.frame()

                            # Text output
                            self$results$text$setContent(capture.output(print(freq_table)))

                            # Compact HTML for multiple raters
                            max_display <- min(50, nrow(freq_table))  # Limit to 50 rows

                            html_table <- paste0(
                                "<div style='font-family: monospace; font-size: 11px; max-height: 400px; overflow-y: auto;'>",
                                "<p style='font-weight: bold;'>Rating Combinations (showing ", max_display, " of ", nrow(freq_table), ")</p>",
                                "<table style='width: auto; border-collapse: collapse;'>",
                                "<tr style='background: #f5f5f5;'>"
                            )

                            # Headers
                            for (col_name in names(freq_table)) {
                                display_name <- if (col_name == "n") "Count" else col_name
                                html_table <- paste0(html_table,
                                    "<th style='border: 1px solid #ccc; padding: 4px 8px; font-size: 11px;'>",
                                    display_name, "</th>")
                            }
                            html_table <- paste0(html_table, "</tr>")

                            # Data rows (limited)
                            for (i in 1:max_display) {
                                html_table <- paste0(html_table, "<tr>")
                                for (j in seq_len(ncol(freq_table))) {
                                    html_table <- paste0(html_table,
                                        "<td style='border: 1px solid #ccc; padding: 4px 8px;'>",
                                        freq_table[i, j], "</td>")
                                }
                                html_table <- paste0(html_table, "</tr>")
                            }

                            html_table <- paste0(html_table,
                                "</table>",
                                "<p style='margin-top: 8px; font-size: 11px;'>Total: ", sum(freq_table$n), " ratings</p>",
                                "</div>")

                            self$results$text2$setContent(html_table)
                        }
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


                    # Gwet's AC (if requested) ----
                    if (self$options$gwet) {
                        # Determine weight type
                        gwet_weights <- switch(self$options$gwetWeights,
                            "unweighted" = "identity",
                            "linear" = "linear",
                            "quadratic" = "quadratic"
                        )

                        tryCatch({
                            # Calculate Gwet's AC using irrCAC package
                            if (requireNamespace("irrCAC", quietly = TRUE)) {
                                gwet_result <- irrCAC::gwet.ac.raw(
                                    ratings,
                                    weights = gwet_weights
                                )

                                gwetTable <- self$results$gwetTable
                                gwetTable$setRow(rowNo = 1, values = list(
                                    method = paste0("Gwet's AC (", self$options$gwetWeights, " weights)"),
                                    subjects = nrow(ratings),
                                    raters = ncol(ratings),
                                    coefficient = gwet_result$est$coefficient,
                                    se = gwet_result$est$se,
                                    ci_lower = gwet_result$est$conf.int[1],
                                    ci_upper = gwet_result$est$conf.int[2],
                                    pvalue = gwet_result$est$p.value
                                ))
                            } else {
                                gwetTable <- self$results$gwetTable
                                gwetTable$setRow(rowNo = 1, values = list(
                                    method = paste0("Gwet's AC (", self$options$gwetWeights, " weights)"),
                                    subjects = nrow(ratings),
                                    raters = ncol(ratings),
                                    coefficient = NA,
                                    se = NA,
                                    ci_lower = NA,
                                    ci_upper = NA,
                                    pvalue = NA
                                ))
                                gwetTable$addFootnote(
                                    rowNo = 1,
                                    col = "coefficient",
                                    "Package 'irrCAC' required for Gwet's AC calculation. Install with: install.packages('irrCAC')"
                                )
                            }

                        }, error = function(e) {
                            gwetTable <- self$results$gwetTable
                            gwetTable$setRow(rowNo = 1, values = list(
                                method = paste0("Gwet's AC (", self$options$gwetWeights, " weights)"),
                                subjects = nrow(ratings),
                                raters = ncol(ratings),
                                coefficient = NA,
                                se = NA,
                                ci_lower = NA,
                                ci_upper = NA,
                                pvalue = NA
                            ))
                            gwetTable$addFootnote(
                                rowNo = 1,
                                col = "coefficient",
                                paste0("Error calculating Gwet's AC: ", e$message)
                            )
                        })
                    }


                    # Show Level Ordering Information (if requested) ----
                    if (self$options$showLevelInfo) {
                        level_info_html <- "<div style='font-family: Arial, sans-serif; max-width: 800px;'>"
                        level_info_html <- paste0(level_info_html,
                            "<div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>",
                            "<h3 style='margin: 0 0 5px 0; font-size: 16px;'>Variable Level Ordering</h3>",
                            "<p style='margin: 0; font-size: 14px; color: #666;'>Current ordering of categorical levels in selected variables</p>",
                            "</div>")

                        for (var_name in self$options$vars) {
                            var_data <- self$data[[var_name]]

                            if (is.factor(var_data)) {
                                var_levels <- levels(var_data)
                                is_ordered <- is.ordered(var_data)

                                level_info_html <- paste0(level_info_html,
                                    "<div style='border: 1px solid #ccc; padding: 12px; margin-bottom: 10px;'>",
                                    "<p style='margin: 0 0 8px 0; font-weight: bold;'>", var_name, "</p>",
                                    "<p style='margin: 0 0 5px 0; font-size: 13px;'>Type: ",
                                    ifelse(is_ordered, "<span style='color: green;'>Ordered</span>", "<span style='color: orange;'>Nominal</span>"),
                                    "</p>",
                                    "<p style='margin: 0; font-size: 13px;'>Levels: ",
                                    paste(var_levels, collapse = " → "),
                                    "</p>",
                                    "</div>")
                            } else {
                                level_info_html <- paste0(level_info_html,
                                    "<div style='border: 1px solid #ccc; padding: 12px; margin-bottom: 10px;'>",
                                    "<p style='margin: 0 0 8px 0; font-weight: bold;'>", var_name, "</p>",
                                    "<p style='margin: 0; font-size: 13px; color: #999;'>Not a factor variable</p>",
                                    "</div>")
                            }
                        }

                        level_info_html <- paste0(level_info_html, "</div>")
                        self$results$levelInfo$setContent(level_info_html)
                    }


                    # Consensus variable (if requested) ----
                    if (self$options$consensusVar && length(self$options$vars) >= 2) {
                        tryCatch({
                            # Calculate consensus for each row
                            consensus_scores <- apply(ratings, 1, function(row) {
                                # Get frequency table
                                freq_table <- table(row)
                                if (length(freq_table) == 0) return(NA)

                                max_count <- max(freq_table)
                                n_raters <- length(row[!is.na(row)])

                                if (n_raters == 0) return(NA)

                                # Check consensus rule
                                pct_agree <- max_count / n_raters * 100

                                consensus_threshold <- switch(self$options$consensusRule,
                                    "majority" = 50,
                                    "supermajority" = 75,
                                    "unanimous" = 100
                                )

                                if (pct_agree < consensus_threshold) {
                                    return(NA)  # No consensus
                                }

                                # Get modal value(s)
                                modes <- names(freq_table)[freq_table == max_count]

                                # Handle ties
                                if (length(modes) > 1) {
                                    consensus_val <- switch(self$options$tieBreaker,
                                        "exclude" = NA,
                                        "first" = modes[1],
                                        "lowest" = modes[which.min(modes)],
                                        "highest" = modes[which.max(modes)]
                                    )
                                } else {
                                    consensus_val <- modes[1]
                                }

                                return(consensus_val)
                            })

                            # Calculate summary statistics
                            n_consensus <- sum(!is.na(consensus_scores))
                            n_ties <- sum(is.na(consensus_scores))
                            pct_consensus <- round(n_consensus / nrow(ratings) * 100, 1)

                            # Populate summary table
                            self$results$consensusTable$setRow(rowNo = 1, values = list(
                                consensus_var = self$options$consensusName,
                                n_consensus = n_consensus,
                                n_ties = n_ties,
                                pct_consensus = pct_consensus
                            ))

                            # Create output variable
                            self$results$consensusVar$setValues(consensus_scores)
                            self$results$consensusVar$setName(self$options$consensusName)

                        }, error = function(e) {
                            warning("Error creating consensus variable: ", e$message)
                        })
                    }


                    # Pairwise kappa (if reference rater specified) ----
                    if (!is.null(self$options$referenceRater) && length(self$options$vars) >= 2) {
                        ref_rater <- self$data[[self$options$referenceRater]]

                        pairwise_results <- list()

                        for (var_name in self$options$vars) {
                            if (var_name == self$options$referenceRater) next  # Skip self-comparison

                            test_rater <- self$data[[var_name]]

                            # Create 2-rater data frame
                            pair_data <- data.frame(
                                ref = ref_rater,
                                test = test_rater
                            )
                            pair_data <- na.omit(pair_data)

                            if (nrow(pair_data) > 0) {
                                tryCatch({
                                    # Calculate kappa
                                    kappa_result <- irr::kappa2(pair_data, weight = self$options$wght)

                                    # Calculate confidence interval (assuming normal distribution)
                                    kappa_val <- kappa_result$value
                                    n <- nrow(pair_data)
                                    se_kappa <- sqrt((kappa_val * (1 - kappa_val)) / n)  # Simplified SE

                                    ci_lower <- kappa_val - 1.96 * se_kappa
                                    ci_upper <- kappa_val + 1.96 * se_kappa

                                    # Interpret kappa (Landis & Koch)
                                    if (is.na(kappa_val) || kappa_val < 0) {
                                        interp <- "Poor"
                                    } else if (kappa_val < 0.20) {
                                        interp <- "Slight"
                                    } else if (kappa_val < 0.40) {
                                        interp <- "Fair"
                                    } else if (kappa_val < 0.60) {
                                        interp <- "Moderate"
                                    } else if (kappa_val < 0.80) {
                                        interp <- "Substantial"
                                    } else {
                                        interp <- "Almost Perfect"
                                    }

                                    pairwise_results[[var_name]] <- list(
                                        rater = var_name,
                                        n_cases = nrow(pair_data),
                                        kappa = kappa_val,
                                        ci_lower = ci_lower,
                                        ci_upper = ci_upper,
                                        interpretation = interp
                                    )
                                }, error = function(e) {
                                    warning("Error calculating pairwise kappa for ", var_name, ": ", e$message)
                                })
                            }
                        }

                        # Sort by kappa if ranking requested
                        if (self$options$rankRaters && length(pairwise_results) > 0) {
                            pairwise_results <- pairwise_results[order(
                                sapply(pairwise_results, function(x) x$kappa),
                                decreasing = TRUE
                            )]
                        }

                        # Populate table
                        if (length(pairwise_results) > 0) {
                            pairwise_table <- self$results$pairwiseKappaTable
                            for (i in seq_along(pairwise_results)) {
                                row_data <- pairwise_results[[i]]
                                if (self$options$rankRaters) {
                                    row_data$rank <- i
                                }
                                pairwise_table$setRow(rowNo = i, values = row_data)
                            }
                        }
                    }


                    # Level of Agreement variable (if requested) ----
                    if (self$options$loaVariable && length(self$options$vars) >= 2) {
                        tryCatch({
                            # Calculate level of agreement for each case
                            loa_scores <- apply(ratings, 1, function(row) {
                                # Remove missing values
                                row_clean <- row[!is.na(row)]
                                if (length(row_clean) == 0) return(NA)

                                # Calculate agreement percentage
                                freq_table <- table(row_clean)
                                max_count <- max(freq_table)
                                pct_agree <- (max_count / length(row_clean)) * 100

                                # Categorize based on thresholds
                                if (self$options$loaThresholds == "custom") {
                                    high_thresh <- self$options$loaHighThreshold
                                    low_thresh <- self$options$loaLowThreshold

                                    if (pct_agree == 100) {
                                        return("Absolute")
                                    } else if (pct_agree >= high_thresh) {
                                        return("High")
                                    } else if (pct_agree >= low_thresh) {
                                        return("Moderate")
                                    } else {
                                        return("Low")
                                    }
                                } else if (self$options$loaThresholds == "quartiles") {
                                    # Will be calculated after all scores
                                    return(pct_agree)
                                } else if (self$options$loaThresholds == "tertiles") {
                                    # Will be calculated after all scores
                                    return(pct_agree)
                                }
                            })

                            # If using data-driven thresholds, calculate them
                            if (self$options$loaThresholds %in% c("quartiles", "tertiles")) {
                                numeric_scores <- as.numeric(loa_scores)
                                numeric_scores <- numeric_scores[!is.na(numeric_scores)]

                                if (self$options$loaThresholds == "quartiles") {
                                    breaks <- quantile(numeric_scores, probs = c(0, 0.25, 0.5, 0.75, 1))
                                    loa_scores <- cut(as.numeric(loa_scores),
                                                    breaks = breaks,
                                                    labels = c("Low", "Moderate", "High", "Very High"),
                                                    include.lowest = TRUE)
                                } else {
                                    breaks <- quantile(numeric_scores, probs = c(0, 0.33, 0.67, 1))
                                    loa_scores <- cut(as.numeric(loa_scores),
                                                    breaks = breaks,
                                                    labels = c("Low", "Moderate", "High"),
                                                    include.lowest = TRUE)
                                }

                                loa_scores <- as.character(loa_scores)
                            }

                            # Convert to factor with proper ordering
                            loa_levels <- c("Low", "Moderate", "High", "Very High", "Absolute")
                            loa_scores_factor <- factor(loa_scores, levels = loa_levels, ordered = TRUE)

                            # Create distribution table
                            loa_table_data <- as.data.frame(table(loa_scores_factor))
                            names(loa_table_data) <- c("category", "n")
                            loa_table_data$percent <- round(loa_table_data$n / sum(loa_table_data$n) * 100, 1)

                            # Populate table
                            loaTable <- self$results$loaTable
                            for (i in 1:nrow(loa_table_data)) {
                                loaTable$setRow(rowNo = i, values = list(
                                    category = as.character(loa_table_data$category[i]),
                                    n = loa_table_data$n[i],
                                    percent = loa_table_data$percent[i]
                                ))
                            }

                            # Create output variable
                            self$results$loaVar$setValues(loa_scores_factor)
                            self$results$loaVar$setName("level_of_agreement")

                        }, error = function(e) {
                            warning("Error creating level of agreement variable: ", e$message)
                        })
                    }


                    # Publication-ready text (if showSummary is enabled) ----
                    if (self$options$showSummary && exists("result2")) {
                        # Generate APA-style reporting text
                        pub_text <- paste0(
                            "<div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>",
                            "<div style='background: #f5f5f5; border: 2px solid #333; padding: 15px; margin-bottom: 15px;'>",
                            "<h3 style='margin: 0 0 5px 0; font-size: 16px;'>Publication-Ready Text</h3>",
                            "<p style='margin: 0; font-size: 14px; color: #666;'>Copy and adapt for your manuscript</p>",
                            "</div>",
                            "<div style='background: #fff; border: 1px solid #ccc; padding: 15px; font-size: 14px;'>",
                            "<p style='margin: 0 0 10px 0;'>",
                            "Interrater reliability was assessed using ", result2[["method"]], ". ",
                            "Agreement was calculated for ", result1[["subjects"]], " cases rated by ",
                            result1[["raters"]], " raters. "
                        )

                        if (wght != "unweighted") {
                            pub_text <- paste0(pub_text,
                                "Weighted kappa (", wght, " weights) was used to account for the ordinal nature of the data. "
                            )
                        }

                        pub_text <- paste0(pub_text,
                            "The kappa coefficient was ", sprintf("%.3f", result2[["value"]]),
                            " (", sprintf("%.1f%%", result1[["value"]]), " raw agreement)"
                        )

                        if (!is.null(result2[["p.value"]]) && !is.na(result2[["p.value"]])) {
                            if (result2[["p.value"]] < 0.001) {
                                pub_text <- paste0(pub_text, ", <em>p</em> < .001")
                            } else {
                                pub_text <- paste0(pub_text, ", <em>p</em> = ", sprintf("%.3f", result2[["p.value"]]))
                            }
                        }

                        # Add interpretation
                        kappa_val <- result2[["value"]]
                        if (!is.na(kappa_val)) {
                            if (kappa_val < 0) {
                                interp <- "poor agreement"
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

                            pub_text <- paste0(pub_text,
                                ", indicating ", interp, " (Landis & Koch, 1977)."
                            )
                        } else {
                            pub_text <- paste0(pub_text, ".")
                        }

                        pub_text <- paste0(pub_text,
                            "</p>",
                            "</div>",
                            "</div>"
                        )

                        self$results$reportText$setContent(pub_text)
                    }
                }  # Close else block from line 559






        }
    
    ))
