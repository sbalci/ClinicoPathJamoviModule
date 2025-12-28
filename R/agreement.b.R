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


# See also:
# \url{http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/#ordinal-data-weighted-kappa}


agreementClass <- if (requireNamespace("jmvcore")) R6::R6Class("agreementClass",
    inherit = agreementBase, private = list(

        # Store harmonized levels and ordering information for validation
        .harmonizedLevels = NULL,
        .isOrdered = FALSE,

        # Variable name escaping utility for special characters
        .escapeVar = function(x) {
            if (is.null(x) || length(x) == 0) return(character(0))
            # Handle spaces, special characters, and ensure valid R names
            vapply(x, function(v) {
                # First make it a valid R name
                v <- make.names(v)
                # Replace any remaining non-alphanumeric characters with underscore
                gsub("[^A-Za-z0-9_]+", "_", v)
            }, character(1), USE.NAMES = FALSE)
        },

        .prepareRatingColumn = function(column, numericLevels = NULL) {
            if (is.data.frame(column)) {
                stop("Internal error: expected a vector when preparing ratings")
            }

            # Convert character columns to factors
            if (is.character(column))
                column <- factor(column)

            # Convert numeric columns to ordered factors
            if (is.numeric(column)) {
                levels_sorted <- numericLevels
                if (is.null(levels_sorted))
                    levels_sorted <- sort(unique(column))
                return(factor(column, levels = levels_sorted, ordered = TRUE))
            }

            # Return factor columns as-is (preserving jamovi's ordering)
            column
        },

        .buildLevelInfo = function() {
            # Build HTML output showing current level ordering for all variables
            html <- "<div style='background-color: #fff9c4; padding: 15px; border-radius: 8px; margin: 10px 0;'>"
            html <- paste0(html, "<h4 style='margin-top: 0; color: #f57c00;'>📋 Current Level Ordering</h4>")
            html <- paste0(html, "<p style='margin: 10px 0;'>For weighted kappa, ordinal levels must be properly ordered. ",
                          "Check your level ordering below:</p>")

            vars <- self$options$vars
            if (length(vars) == 0) {
                html <- paste0(html, "<p style='color: #666; font-style: italic;'>No variables selected yet.</p>")
                html <- paste0(html, "</div>")
                return(html)
            }

            for (var in vars) {
                col <- self$data[[var]]
                if (is.factor(col)) {
                    lvls <- levels(col)
                    is_ord <- is.ordered(col)

                    # Color code based on ordered status
                    border_color <- if (is_ord) "#4CAF50" else "#FF9800"
                    status_icon <- if (is_ord) "✓" else "⚠️"
                    status_text <- if (is_ord) "Ordered" else "Unordered"

                    html <- paste0(html, "<div style='margin: 10px 0; padding: 10px; background-color: white; ",
                                  "border-left: 4px solid ", border_color, "; border-radius: 4px;'>")
                    html <- paste0(html, "<strong>", var, "</strong>: ")
                    html <- paste0(html, "<span style='color: ", border_color, ";'>",
                                  status_icon, " ", status_text, "</span><br>")
                    html <- paste0(html, "<span style='font-family: monospace; font-size: 13px; color: #333;'>",
                                  paste(lvls, collapse = " → "), "</span>")

                    # Show warning if weighted kappa is requested but variable is unordered
                    if (!is_ord && self$options$wght != "unweighted") {
                        html <- paste0(html, "<br><span style='color: #d32f2f; font-size: 12px; margin-top: 5px;'>",
                                      "⚠️ Weighted kappa requires ordered factors. ",
                                      "Set variable to Ordinal in Data tab.</span>")
                    }
                    html <- paste0(html, "</div>")
                }
            }

            html <- paste0(html, "<div style='margin-top: 15px; padding: 10px; background-color: #e3f2fd; ",
                          "border-radius: 4px; font-size: 12px;'>")
            html <- paste0(html, "<strong>💡 To fix ordering:</strong><br>")
            html <- paste0(html, "1. Go to Data tab → Select variable<br>")
            html <- paste0(html, "2. Set as Ordinal (if appropriate)<br>")
            html <- paste0(html, "3. Click Edit Levels → Drag levels to reorder<br>")
            html <- paste0(html, "4. First level = lowest, last level = highest")
            html <- paste0(html, "</div>")
            html <- paste0(html, "</div>")

            return(html)
        },

        .interpretKappa = function(kappa) {
            if (is.null(kappa) || length(kappa) == 0 || is.na(kappa))
                return("Unable to evaluate")

            if (kappa < 0)
                return("Poor agreement (worse than chance)")
            if (kappa < 0.20)
                return("Slight agreement")
            if (kappa < 0.40)
                return("Fair agreement")
            if (kappa < 0.60)
                return("Moderate agreement")
            if (kappa < 0.80)
                return("Substantial agreement")

            "Almost perfect agreement"
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
            # Generate Bland-Altman plot
            data <- self$data
            vars <- self$options$vars

            if (length(vars) < 2) return()

            # For now, plot the first pair
            method1_name <- vars[1]
            method2_name <- vars[2]

            x <- as.numeric(data[[method1_name]])
            y <- as.numeric(data[[method2_name]])

            # Remove missing values
            complete_cases <- complete.cases(x, y)
            x <- x[complete_cases]
            y <- y[complete_cases]

            if (length(x) < 3) return()

            # Calculate statistics
            differences <- x - y
            averages <- (x + y) / 2
            mean_diff <- mean(differences)
            sd_diff <- sd(differences)

            conf_level <- self$options$baConfidenceLevel
            z_value <- qnorm(1 - (1 - conf_level) / 2)

            loa_lower <- mean_diff - z_value * sd_diff
            loa_upper <- mean_diff + z_value * sd_diff

            # Create plot data
            plot_data <- data.frame(
                average = averages,
                difference = differences
            )

            # Create ggplot
            if (requireNamespace("ggplot2", quietly = TRUE)) {
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = average, y = difference)) +
                    ggplot2::geom_point(alpha = 0.6, size = 2) +
                    ggplot2::geom_hline(yintercept = mean_diff, color = "blue", linetype = "solid", linewidth = 1) +
                    ggplot2::geom_hline(yintercept = loa_lower, color = "red", linetype = "dashed", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = loa_upper, color = "red", linetype = "dashed", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = 0, color = "gray50", linetype = "dotted", linewidth = 0.5) +
                    ggplot2::labs(
                        title = paste("Bland-Altman Plot:", method1_name, "vs", method2_name),
                        x = paste("Average of", method1_name, "and", method2_name),
                        y = paste("Difference (", method1_name, "-", method2_name, ")", sep = "")
                    ) +
                    ggplot2::annotate("text", x = max(averages) * 0.95, y = mean_diff,
                                     label = sprintf("Mean = %.2f", mean_diff),
                                     hjust = 1, vjust = -0.5, color = "blue") +
                    ggplot2::annotate("text", x = max(averages) * 0.95, y = loa_upper,
                                     label = sprintf("+%.1f SD = %.2f", z_value, loa_upper),
                                     hjust = 1, vjust = -0.5, color = "red") +
                    ggplot2::annotate("text", x = max(averages) * 0.95, y = loa_lower,
                                     label = sprintf("-%.1f SD = %.2f", z_value, loa_lower),
                                     hjust = 1, vjust = 1.5, color = "red") +
                    ggplot2::theme_classic() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        axis.title = ggplot2::element_text(size = 11),
                        axis.text = ggplot2::element_text(size = 10)
                    )

                # Add proportional bias line if requested
                if (self$options$proportionalBias) {
                    prop_bias_model <- lm(differences ~ averages)
                    slope_p <- summary(prop_bias_model)$coefficients[2, 4]

                    if (slope_p < 0.05) {
                        p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE,
                                                      color = "purple", linetype = "dotted", linewidth = 0.8)
                    }
                }

                print(p)
                TRUE
            }
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

            # Enhanced validation ----
            if (is.null(self$data) || nrow(self$data) == 0) {
                # error_notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = "emptyDataset",
                #     type = jmvcore::NoticeType$ERROR
                # )
                # error_notice$setContent(
                #     "⛔ <b>Empty Dataset:</b> The dataset contains no rows. Please load data before running analysis."
                # )
                # self$results$insert(999, error_notice)
                return()
            }

            if (nrow(self$data) < 3) {
                # error_notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = "insufficientObservations",
                #     type = jmvcore::NoticeType$ERROR
                # )
                # error_notice$setContent(paste0(
                #     "⛔ <b>Insufficient Observations:</b> At least 3 observations are required for reliability analysis. ",
                #     "Currently ", nrow(self$data), " observation(s) available. ",
                #     "Please use a dataset with more cases."
                # ))
                # self$results$insert(999, error_notice)
                return()
            }

            # Validate variable types
            for (v in self$options$vars) {
                if (!v %in% names(self$data)) {
                    # error_notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = "variableNotFound",
                    #     type = jmvcore::NoticeType$ERROR
                    # )
                    # error_notice$setContent(paste0(
                    #     "⛔ <b>Variable Not Found:</b> Variable '", v, "' does not exist in the dataset. ",
                    #     "Available variables: ", paste(names(self$data), collapse = ", "), ". ",
                    #     "Please select valid rater variables."
                    # ))
                    # self$results$insert(999, error_notice)
                    return()
                }
                var_data <- self$data[[v]]
                if (!is.factor(var_data) && !is.numeric(var_data) && !is.ordered(var_data)) {
                    # error_notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = "invalidVariableType",
                    #     type = jmvcore::NoticeType$ERROR
                    # )
                    # error_notice$setContent(paste0(
                    #     "⛔ <b>Invalid Variable Type:</b> Variable '", v, "' must be categorical (factor), ordinal, or numeric. ",
                    #     "Current type: ", class(var_data)[1], ". ",
                    #     "Please convert to appropriate type in jamovi Data tab (Setup → Data Type)."
                    # ))
                    # self$results$insert(999, error_notice)
                    return()
                }
            }

            # Display level ordering information if requested ----
            if (self$options$showLevelInfo) {
                levelInfoHtml <- private$.buildLevelInfo()
                self$results$levelInfo$setContent(levelInfoHtml)
            }

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

            # Extract numeric levels for variables (used to preserve ordering when converting to factors)
            numericLevels <- NULL
            numeric_values <- unique(unlist(lapply(ratings, function(col) {
                if (is.numeric(col)) return(as.numeric(stats::na.omit(col)))
                NULL
            })))
            if (length(numeric_values) > 0)
                numericLevels <- sort(numeric_values)

            # Prepare all rating columns as categorical factors
            ratings[] <- lapply(ratings, function(col)
                private$.prepareRatingColumn(col, numericLevels))

            # CRITICAL FIX #1: Level Harmonization
            # Ensure ALL raters have identical factor levels in identical order
            # This is essential for valid kappa calculations, especially weighted kappa
            tryCatch({
                # Extract all unique levels across ALL raters
                all_levels_list <- lapply(ratings, function(col) {
                    if (is.factor(col)) return(levels(col))
                    return(NULL)
                })
                all_levels_list <- all_levels_list[!sapply(all_levels_list, is.null)]

                if (length(all_levels_list) > 0) {
                    # Get union of all levels while preserving order from first rater
                    all_levels <- all_levels_list[[1]]
                    for (i in seq_along(all_levels_list)[-1]) {
                        new_levels <- setdiff(all_levels_list[[i]], all_levels)
                        if (length(new_levels) > 0) {
                            all_levels <- c(all_levels, new_levels)
                        }
                    }

                    # Check if any rater has ordered factor
                    any_ordered <- any(sapply(ratings, is.ordered))

                    # Harmonize: ensure ALL raters have same levels in same order
                    ratings[] <- lapply(ratings, function(col) {
                        if (is.factor(col)) {
                            # Re-factor with common levels, preserving ordered status
                            factor(as.character(col), levels = all_levels, ordered = any_ordered)
                        } else {
                            col  # Keep as-is if not factor
                        }
                    })

                    # Validation: Check all raters now have identical levels
                    harmonized_levels <- lapply(ratings, levels)
                    unique_level_sets <- unique(lapply(harmonized_levels, function(x) paste(x, collapse = "|")))

                    if (length(unique_level_sets) > 1) {
                        # warning_notice <- jmvcore::Notice$new(
                        #     options = self$options,
                        #     name = "levelHarmonizationWarning",
                        #     type = jmvcore::NoticeType$STRONG_WARNING
                        # )
                        # warning_notice$setContent(paste0(
                        #     "⚠️ <b>Level Harmonization Issue:</b> Different raters had different category levels. ",
                        #     "All raters have been harmonized to use the union of all levels: {",
                        #     paste(all_levels, collapse = ", "), "}. ",
                        #     "This ensures statistically valid kappa calculations. ",
                        #     "Verify this matches your intended category structure."
                        # ))
                        # self$results$insert(999, warning_notice)
                    }

                    # Store harmonized levels for later use (Krippendorff, cluster analyses)
                    private$.harmonizedLevels <- all_levels
                    private$.isOrdered <- any_ordered
                } else {
                    # No factors found (all numeric), use numericLevels
                    private$.harmonizedLevels <- numericLevels
                    private$.isOrdered <- TRUE  # Numeric data treated as ordered
                }
            }, error = function(e) {
                warning(paste("Level harmonization failed:", e$message, "- Results may be unreliable"))
                private$.harmonizedLevels <- NULL
                private$.isOrdered <- FALSE
            })

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
                if (wght %in% c("equal", "squared")) {
                    for (col_name in names(ratings)) {
                        column <- ratings[[col_name]]

                        if (!is.ordered(column)) {
                            # error_notice <- jmvcore::Notice$new(
                            #     options = self$options,
                            #     name = "weightedKappaRequiresOrdered",
                            #     type = jmvcore::NoticeType$ERROR
                            # )
                            # error_notice$setContent(paste0(
                            #     "⛔ <b>Weighted Kappa Requirement:</b> Weighted kappa requires ordinal (ordered factor) or numeric variables. ",
                            #     "Variable '", col_name, "' is nominal (unordered factor). ",
                            #     "Please either convert to ordinal in jamovi Data tab (Setup → Data Type → Ordinal) ",
                            #     "or change weighting to 'Unweighted' for nominal categories."
                            # ))
                            # self$results$insert(999, error_notice)
                            return()
                        }
                    }
                }

                if (exct == TRUE) {
                    # error_notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = "exactKappaRequiresThreeRaters",
                    #     type = jmvcore::NoticeType$ERROR
                    # )
                    # error_notice$setContent(paste0(
                    #     "⛔ <b>Exact Kappa Limitation:</b> Exact p-value calculation requires 3 or more raters. ",
                    #     "Currently 2 raters selected. ",
                    #     "Either select additional rater variables, or disable 'Exact Kappa' option to use normal approximation."
                    # ))
                    # self$results$insert(999, error_notice)
                    return()
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
        }


        # Consensus Score Derivation (if requested) ----
        if (self$options$consensusVar) {
            tryCatch({
                ratings <- self$data[, self$options$vars, drop = FALSE]
                ratings[] <- lapply(ratings, function(col) private$.prepareRatingColumn(col, numericLevels))
                n_raters <- length(self$options$vars)

                # Calculate modal category for each row
                consensus <- apply(ratings, 1, function(row) {
                    row <- row[!is.na(row)]  # Remove NA
                    if (length(row) == 0) return(NA)

                    # Get frequency table
                    freq_table <- table(row)
                    max_freq <- max(freq_table)
                    modes <- names(freq_table)[freq_table == max_freq]

                    # Apply consensus rule
                    rule <- self$options$consensusRule
                    threshold <- switch(rule,
                                      "majority" = 0.50,
                                      "supermajority" = 0.75,
                                      "unanimous" = 1.00,
                                      0.50)  # default

                    if (max_freq / length(row) < threshold) {
                        return(NA)  # Threshold not met
                    }

                    # Handle ties
                    if (length(modes) > 1) {
                        tieBreaker <- self$options$tieBreaker
                        if (tieBreaker == "exclude") return(NA)
                        if (tieBreaker == "first") return(modes[1])
                        if (tieBreaker == "lowest") {
                            numeric_modes <- as.numeric(modes)
                            if (all(!is.na(numeric_modes))) {
                                return(as.character(min(numeric_modes)))
                            } else {
                                return(modes[1])
                            }
                        }
                        if (tieBreaker == "highest") {
                            numeric_modes <- as.numeric(modes)
                            if (all(!is.na(numeric_modes))) {
                                return(as.character(max(numeric_modes)))
                            } else {
                                return(modes[length(modes)])
                            }
                        }
                    }

                    return(modes[1])
                })

                # Add consensus variable to dataset (as computed column)
                consensus_name <- self$options$consensusName

                # Convert to factor with same levels as original raters
                if (length(self$options$vars) > 0 && is.factor(self$data[[self$options$vars[1]]])) {
                    consensus <- factor(consensus, levels = levels(self$data[[self$options$vars[1]]]))
                }

                # Update summary table
                self$results$consensusTable$setRow(rowNo = 1, values = list(
                    consensus_var = consensus_name,
                    n_consensus = sum(!is.na(consensus)),
                    n_ties = sum(is.na(consensus) & rowSums(!is.na(ratings)) > 0),
                    pct_consensus = round(100 * sum(!is.na(consensus)) / nrow(ratings), 1)
                ))

                # Create the actual computed column in the dataset
                self$results$consensusVar$setValues(consensus)

            }, error = function(e) {
                # Handle errors gracefully
                self$results$consensusTable$setRow(rowNo = 1, values = list(
                    consensus_var = self$options$consensusName,
                    n_consensus = 0,
                    n_ties = 0,
                    pct_consensus = 0
                ))
                warning(paste("Error creating consensus variable:", e$message))
            })
        }

        # Pairwise Kappa (each rater vs reference) ----
        if (!is.null(self$options$referenceRater)) {
            tryCatch({
                reference_var <- self$options$referenceRater
                reference_data <- private$.prepareRatingColumn(self$data[[reference_var]], numericLevels)

                rater_results <- list()

                for (rater_var in self$options$vars) {
                    if (rater_var == reference_var) next  # Skip self-comparison

                    private$.checkpoint()  # Allow UI responsiveness

                    rater_data <- private$.prepareRatingColumn(self$data[[rater_var]], numericLevels)

                    # Create pairwise data frame
                    pair_data <- data.frame(reference_data, rater_data)
                    pair_data <- pair_data[complete.cases(pair_data), ]

                    n_cases <- nrow(pair_data)

                    if (n_cases < 3) {
                        kappa_val <- NA
                        ci_lower <- NA
                        ci_upper <- NA
                    } else {
                        # Calculate Cohen's kappa with specified weights
                        if (self$options$wght %in% c("equal", "squared")) {
                            for (col_idx in seq_along(pair_data)) {
                                column <- pair_data[[col_idx]]

                                if (is.numeric(column)) {
                                    pair_data[[col_idx]] <- factor(column,
                                                                  levels = sort(unique(column)),
                                                                  ordered = TRUE)
                                } else if (!is.ordered(column)) {
                                    stop("Weighted kappa requires ordinal (ordered factor) or numeric variables. Adjust the reference or rater column selection or choose 'Unweighted'.")
                                }
                            }
                        }

                        kappa_result <- irr::kappa2(pair_data, weight = self$options$wght)
                        kappa_val <- kappa_result$value

                        # Compute CI from reported variance when available
                        if (!is.null(kappa_result$var) && !is.na(kappa_result$var) && kappa_result$var >= 0) {
                            se_kappa <- sqrt(kappa_result$var)
                        } else if (!is.null(kappa_result$statistic) &&
                                   !is.na(kappa_result$statistic) &&
                                   kappa_result$statistic != 0) {
                            se_kappa <- abs(kappa_val / kappa_result$statistic)
                        } else {
                            se_kappa <- NA
                        }

                        if (!is.na(se_kappa)) {
                            ci_lower <- max(-1, kappa_val - 1.96 * se_kappa)
                            ci_upper <- min(1, kappa_val + 1.96 * se_kappa)
                        } else {
                            ci_lower <- NA
                            ci_upper <- NA
                        }
                    }

                    rater_results[[rater_var]] <- list(
                        rater = rater_var,
                        n_cases = n_cases,
                        kappa = kappa_val,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper,
                        interpretation = private$.interpretKappa(kappa_val)
                    )
                }

                # Sort by kappa if requested
                if (self$options$rankRaters && length(rater_results) > 0) {
                    kappa_values <- sapply(rater_results, function(x) {
                        if (is.na(x$kappa)) return(-Inf) else return(x$kappa)
                    })
                    rater_results <- rater_results[order(kappa_values, decreasing = TRUE)]
                }

                # Populate table
                pairwiseTable <- self$results$pairwiseKappaTable

                for (i in seq_along(rater_results)) {
                    res <- rater_results[[i]]
                    pairwiseTable$addRow(
                        rowKey = res$rater,
                        values = list(
                            rank = if (self$options$rankRaters) i else NA,
                            rater = res$rater,
                            n_cases = res$n_cases,
                            kappa = res$kappa,
                            ci_lower = res$ci_lower,
                            ci_upper = res$ci_upper,
                            interpretation = res$interpretation
                        )
                    )
                }

            }, error = function(e) {
                # Handle errors gracefully
                warning(paste("Error in pairwise kappa analysis:", e$message))
            })
        }

        # Level of Agreement (LoA) Categorization ----
        if (self$options$loaVariable) {
            tryCatch({
                ratings <- self$data[, self$options$vars, drop = FALSE]
                n_raters <- length(self$options$vars)

                ratings[] <- lapply(ratings, function(col) private$.prepareRatingColumn(col, numericLevels))

                # Calculate agreement count and available raters for each row
                agreement_stats <- t(apply(ratings, 1, function(row) {
                    row <- row[!is.na(row)]
                    if (length(row) == 0) return(c(count = 0, total = 0))

                    freq_table <- table(row)
                    max_count <- max(freq_table)
                    c(count = max_count, total = length(row))
                }))

                if (is.null(dim(agreement_stats))) {
                    agreement_stats <- matrix(agreement_stats, ncol = 2, byrow = TRUE)
                }

                agreement_stats <- as.data.frame(agreement_stats)
                if (!all(c("count", "total") %in% names(agreement_stats))) {
                    names(agreement_stats) <- c("count", "total")
                }

                agreement_counts <- agreement_stats$count
                available_raters <- agreement_stats$total

                pct_agreement <- ifelse(available_raters > 0,
                                        (agreement_counts / available_raters) * 100,
                                        NA)

                # Categorize based on thresholds
                if (self$options$loaThresholds == "custom") {
                    high_thresh <- self$options$loaHighThreshold
                    low_thresh <- self$options$loaLowThreshold

                    loa_category <- ifelse(available_raters == n_raters & agreement_counts == available_raters, "Absolute",
                                    ifelse(pct_agreement >= high_thresh, "High",
                                    ifelse(pct_agreement >= low_thresh, "Low", "Poor")))
                } else if (self$options$loaThresholds == "quartiles") {
                    quartiles <- quantile(pct_agreement, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
                    loa_category <- ifelse(available_raters == n_raters & agreement_counts == available_raters, "Absolute",
                                    ifelse(pct_agreement >= quartiles[3], "High",
                                    ifelse(pct_agreement >= quartiles[2], "Moderate",
                                    ifelse(pct_agreement >= quartiles[1], "Low", "Poor"))))
                } else if (self$options$loaThresholds == "tertiles") {
                    tertiles <- quantile(pct_agreement, probs = c(0.33, 0.67), na.rm = TRUE)
                    loa_category <- ifelse(available_raters == n_raters & agreement_counts == available_raters, "Absolute",
                                    ifelse(pct_agreement >= tertiles[2], "High",
                                    ifelse(pct_agreement >= tertiles[1], "Moderate", "Low")))
                } else {
                    # Default to custom
                    loa_category <- ifelse(agreement_counts == n_raters, "Absolute", "Moderate")
                }

                # Populate summary table
                loa_table <- table(loa_category, useNA = "no")
                loaTable <- self$results$loaTable

                total_cases <- sum(loa_table)

                for (i in seq_along(loa_table)) {
                    loaTable$addRow(
                        rowKey = names(loa_table)[i],
                        values = list(
                            category = names(loa_table)[i],
                            n = as.numeric(loa_table[i]),
                            percent = if (total_cases > 0) round(100 * loa_table[i] / total_cases, 1) else NA
                        )
                    )
                }

                # Create the actual computed column in the dataset
                self$results$loaVar$setValues(loa_category)

            }, error = function(e) {
                # Handle errors gracefully
                warning(paste("Error in LoA categorization:", e$message))
            })
        }


        # Krippendorff's Alpha (if requested) ----
        if (self$options$kripp) {
            # Convert ratings data frame to matrix
            ratings_matrix <- as.matrix(ratings)

            # CRITICAL FIX #2: Preserve ordinal spacing when converting to numeric
            # Use harmonized levels from earlier processing to ensure consistent coding
            if (!is.numeric(ratings_matrix)) {
                # Check if we have harmonized factor levels from earlier processing
                if (!is.null(private$.harmonizedLevels) && all(sapply(as.data.frame(ratings), is.factor))) {
                    # Convert each column using the SAME harmonized level set
                    # This preserves: 1) ordering, 2) consistent codes across raters
                    ratings_matrix_list <- lapply(as.data.frame(ratings_matrix), function(col) {
                        # Use harmonized levels to get consistent numeric codes
                        factor_col <- factor(as.character(col), levels = private$.harmonizedLevels,
                                           ordered = private$.isOrdered)
                        as.numeric(factor_col)
                    })
                    ratings_matrix <- do.call(cbind, ratings_matrix_list)

                    # Add warning for ordinal/interval methods with categorical data
                    if (self$options$krippMethod %in% c("ordinal", "interval")) {
                        kripp_notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = "krippCategoricalWarning",
                            type = jmvcore::NoticeType$WARNING
                        )
                        kripp_notice$setContent(
                            paste0("Ordinal/interval Krippendorff's alpha computed on categorical data recoded as numeric (1,2,3,...). ",
                                  "Results assume EQUAL SPACING between categories. ",
                                  "If categories have unequal clinical importance or spacing, use 'nominal' method or provide numeric ratings directly.")
                        )
                        self$results$insert(999, kripp_notice)
                    }
                } else {
                    # Fallback: convert to numeric (less ideal, but better than crashing)
                    # This pools all values - not recommended but maintains backward compatibility
                    ratings_matrix <- matrix(
                        as.numeric(factor(ratings_matrix)),
                        nrow = nrow(ratings_matrix),
                        ncol = ncol(ratings_matrix)
                    )

                    # Always warn when using fallback method
                    # kripp_warning_notice <- jmvcore::Notice$new(
                    #     options = self$options,
                    #     name = "krippEncodingWarning",
                    #     type = jmvcore::NoticeType$WARNING
                    # )
                    # kripp_warning_notice$setContent(
                    #     "Krippendorff's alpha: Categorical data converted to numeric codes. Results may not be interpretable if original categories had specific meaning or spacing."
                    # )
                    # self$results$insert(999, kripp_warning_notice)
                }
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

        # Gwet's AC1/AC2 (if requested) ----
        if (self$options$gwet) {
            tryCatch({
                # Use the same ratings matrix as Krippendorff
                gwet_matrix <- as.matrix(ratings)

                # Ensure numeric conversion if needed
                if (!is.numeric(gwet_matrix)) {
                    # If categorical/factor data, convert to numeric codes
                    gwet_matrix <- matrix(
                        as.numeric(factor(gwet_matrix)),
                        nrow = nrow(gwet_matrix),
                        ncol = ncol(gwet_matrix)
                    )
                }

                # Remove rows with all NA
                complete_rows <- apply(gwet_matrix, 1, function(x) !all(is.na(x)))
                gwet_matrix <- gwet_matrix[complete_rows, , drop = FALSE]

                n_raters <- ncol(gwet_matrix)

                # Determine weights
                weights <- switch(self$options$gwetWeights,
                    "unweighted" = "unweighted",
                    "linear" = "linear",
                    "quadratic" = "quadratic"
                )

                # Calculate Gwet's AC using irrCAC package
                if (requireNamespace("irrCAC", quietly = TRUE)) {
                    gwet_result <- irrCAC::gwet.ac1.raw(gwet_matrix, weights = weights)

                    # Prepare method name
                    method_name <- if (weights == "unweighted") {
                        "Gwet's AC1 (unweighted)"
                    } else {
                        paste0("Gwet's AC2 (", weights, " weights)")
                    }

                    # Extract results - gwet.ac1.raw returns a data frame
                    # The structure is: gwet_result$est with columns coefficient.value, std.error, etc.
                    est_df <- gwet_result$est

                    values_list <- list(
                        method = method_name,
                        subjects = sum(complete_rows),
                        raters = n_raters,
                        coefficient = as.numeric(est_df$coefficient.value[1]),
                        se = as.numeric(est_df$std.error[1]),
                        ci_lower = as.numeric(est_df$conf.interval[1]),
                        ci_upper = as.numeric(est_df$conf.interval[2]),
                        pvalue = as.numeric(est_df$p.value[1])
                    )

                    # Set results in table
                    gwetTable <- self$results$gwetTable
                    gwetTable$setRow(rowNo = 1, values = values_list)

                } else {
                    # Package not available
                    errorMessage <- "Package 'irrCAC' is required for Gwet's AC calculation. Please install it using: install.packages('irrCAC')"

                    values_list <- list(
                        method = "Gwet's AC (Error)",
                        subjects = sum(complete_rows),
                        raters = n_raters,
                        coefficient = NaN,
                        se = NaN,
                        ci_lower = NaN,
                        ci_upper = NaN,
                        pvalue = NaN
                    )

                    gwetTable <- self$results$gwetTable
                    gwetTable$setRow(rowNo = 1, values = values_list)
                    gwetTable$addFootnote(rowNo = 1, col = "coefficient", errorMessage)
                }

            }, error = function(e) {
                # Handle errors
                errorMessage <- paste("Error calculating Gwet's AC:", e$message)

                values_list <- list(
                    method = paste0("Gwet's AC (", self$options$gwetWeights, ")"),
                    subjects = 0,
                    raters = length(self$options$vars),
                    coefficient = NaN,
                    se = NaN,
                    ci_lower = NaN,
                    ci_upper = NaN,
                    pvalue = NaN
                )

                gwetTable <- self$results$gwetTable
                gwetTable$setRow(rowNo = 1, values = values_list)
                gwetTable$addFootnote(rowNo = 1, col = "coefficient", paste0("Error calculating Gwet's AC: ", e$message))
            })
        }

        # Hierarchical/Multilevel Kappa Analysis ----
        if (self$options$hierarchicalKappa) {
            tryCatch({
                # Check for cluster variable
                if (is.null(self$options$clusterVariable)) {
                    stop("Hierarchical kappa requires a cluster/institution variable")
                }

                # Populate hierarchical ICC components
                if (self$options$iccHierarchical) {
                    private$.populateHierarchicalICC()
                }

                # Populate variance components
                if (self$options$betweenClusterVariance || self$options$withinClusterVariance) {
                    private$.populateVarianceComponents()
                }

                # Populate cluster-specific kappa estimates
                if (self$options$clusterSpecificKappa) {
                    private$.populateClusterSpecificKappa()
                }

                # Test cluster homogeneity
                if (self$options$testClusterHomogeneity) {
                    private$.testClusterHomogeneity()
                }

                # Populate cluster rankings
                if (self$options$clusterRankings) {
                    private$.populateClusterRankings()
                }

            }, error = function(e) {
                # Handle hierarchical kappa errors
                # For now, error is silently caught
                # Could add error messaging to a result item if needed
            })
        }
        },

        # Hierarchical/Multilevel Kappa Methods
        .populateHierarchicalICC = function() {
            # Calculate hierarchical ICC using mixed effects model
            # ICC(1): proportion of variance between clusters (reliability of single rating)
            # ICC(2): reliability of cluster means
            # ICC(3): average reliability of raters within clusters

            table <- self$results$hierarchicalICCTable
            if (is.null(table)) return()

            # CRITICAL FIX #3: Block ICC for categorical data
            # ICC from linear mixed models on categorical data is statistically invalid
            data <- self$data
            vars <- self$options$vars

            # Check if data is categorical (factors or limited unique values)
            is_categorical <- FALSE
            unique_value_counts <- c()

            for (var in vars) {
                col <- data[[var]]
                if (is.factor(col)) {
                    is_categorical <- TRUE
                    break
                }
                if (is.numeric(col)) {
                    n_unique <- length(unique(col[!is.na(col)]))
                    unique_value_counts <- c(unique_value_counts, n_unique)
                    if (n_unique < 10) {
                        is_categorical <- TRUE
                    }
                }
            }

            if (is_categorical) {
                # Block ICC analysis and show error notice
                # icc_error_notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = "iccCategoricalError",
                #     type = jmvcore::NoticeType$ERROR
                # )
                
                # error_msg <- paste0(
                #     "⛔ Hierarchical ICC CANNOT be computed for categorical/ordinal data. ",
                #     "ICC from linear mixed models on categorical data (factors or numeric codes 1,2,3,...) is STATISTICALLY INVALID and produces uninterpretable results. ",
                #     "\n\nFor categorical agreement across clusters, use:\n",
                #     "• Stratified kappa (compute kappa within each cluster)\n",
                #     "• Cluster-specific kappa table (see results below)\n",
                #     "• Multilevel categorical models (not implemented in this module)\n\n",
                #     "ICC is ONLY valid for true continuous interval/ratio scale data (e.g., blood pressure, temperature, tumor size in mm)."
                # )

                # icc_error_notice$setContent(error_msg)
                # self$results$insert(999, icc_error_notice)

                # Populate table with error message
                table$setRow(rowNo=1, values=list(
                    icc_type = "ICC Analysis Blocked",
                    icc_value = NA,
                    icc_ci_lower = NA,
                    icc_ci_upper = NA,
                    interpretation = "ERROR: Cannot compute ICC for categorical data"
                ))

                table$setNote("categorical_error",
                    "⛔ Hierarchical ICC requires continuous numeric data. Use kappa-based methods for categorical ratings.")

                return()  # Exit without computing ICC
            }

            # Check if required packages are available
            if (!requireNamespace("lme4", quietly = TRUE) &&
                !requireNamespace("nlme", quietly = TRUE)) {
                stop("Hierarchical ICC requires either 'lme4' or 'nlme' package")
            }

            cluster_var <- self$options$clusterVariable

            if (length(vars) < 2) {
                stop("Hierarchical ICC requires at least 2 raters")
            }

            # Reshape data to long format for mixed model
            # Each row: subject, cluster, rater, rating
            # Optimized: pre-allocate list and vectorize
            data_list <- lapply(seq_along(vars), function(rater_idx) {
                rater_name <- vars[rater_idx]
                data.frame(
                    subject = 1:nrow(data),
                    cluster = data[[cluster_var]],
                    rater = rater_name,
                    rating = as.numeric(data[[rater_name]]),
                    stringsAsFactors = FALSE
                )
            })
            long_data <- do.call(rbind, data_list)

            # Remove missing values
            long_data <- long_data[complete.cases(long_data), ]

            if (nrow(long_data) < 10) {
                stop("Insufficient data for hierarchical ICC calculation")
            }

            # Convert to factors
            long_data$subject <- as.factor(long_data$subject)
            long_data$cluster <- as.factor(long_data$cluster)
            long_data$rater <- as.factor(long_data$rater)

            # Fit mixed effects model
            # rating ~ 1 + (1|cluster) + (1|subject) + (1|rater)
            tryCatch({
                if (requireNamespace("lme4", quietly = TRUE)) {
                    # Use lme4 (preferred)
                    model <- lme4::lmer(rating ~ 1 + (1|cluster) + (1|subject) + (1|rater),
                                       data = long_data,
                                       REML = TRUE)

                    # Extract variance components
                    vc <- as.data.frame(lme4::VarCorr(model))
                    var_cluster <- vc$vcov[vc$grp == "cluster"]
                    var_subject <- vc$vcov[vc$grp == "subject"]
                    var_rater <- vc$vcov[vc$grp == "rater"]
                    var_residual <- vc$vcov[vc$grp == "Residual"]

                } else if (requireNamespace("nlme", quietly = TRUE)) {
                    # Use nlme as fallback
                    model <- nlme::lme(rating ~ 1,
                                      random = list(cluster = ~1, subject = ~1, rater = ~1),
                                      data = long_data,
                                      method = "REML")

                    vc <- nlme::VarCorr(model)
                    var_cluster <- as.numeric(vc["cluster", "Variance"])
                    var_subject <- as.numeric(vc["subject", "Variance"])
                    var_rater <- as.numeric(vc["rater", "Variance"])
                    var_residual <- as.numeric(vc["Residual", "Variance"])
                }

                # Calculate ICCs
                total_var <- var_cluster + var_subject + var_rater + var_residual

                # ICC(1): Between-cluster reliability (single measurement)
                icc1 <- var_cluster / total_var

                # ICC(2): Reliability of cluster means
                k <- length(unique(long_data$rater))  # number of raters
                icc2 <- var_cluster / (var_cluster + (var_subject + var_rater + var_residual) / k)

                # ICC(3): Within-cluster agreement (average of raters)
                icc3 <- (var_cluster + var_subject) / total_var

                # Calculate approximate 95% CI using Fisher's Z transformation
                .icc_ci <- function(icc, n) {
                    if (is.na(icc) || icc <= 0 || icc >= 1) {
                        return(c(NA, NA))
                    }
                    z <- 0.5 * log((1 + icc) / (1 - icc))
                    se_z <- 1 / sqrt(n - 3)
                    z_lower <- z - 1.96 * se_z
                    z_upper <- z + 1.96 * se_z
                    ci_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
                    ci_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
                    return(c(max(0, ci_lower), min(1, ci_upper)))
                }

                n_obs <- nrow(long_data)
                icc1_ci <- .icc_ci(icc1, n_obs)
                icc2_ci <- .icc_ci(icc2, n_obs)
                icc3_ci <- .icc_ci(icc3, n_obs)

                # Interpretation function
                .interpret_icc <- function(icc) {
                    if (is.na(icc)) return("Unable to calculate")
                    if (icc < 0.40) return("Poor reliability")
                    if (icc < 0.60) return("Fair reliability")
                    if (icc < 0.75) return("Good reliability")
                    return("Excellent reliability")
                }

                # Populate table
                table$setRow(rowNo=1, values=list(
                    icc_type = "ICC(1): Between-Cluster",
                    icc_value = icc1,
                    icc_ci_lower = icc1_ci[1],
                    icc_ci_upper = icc1_ci[2],
                    interpretation = .interpret_icc(icc1)
                ))

                table$setRow(rowNo=2, values=list(
                    icc_type = "ICC(2): Cluster Means Reliability",
                    icc_value = icc2,
                    icc_ci_lower = icc2_ci[1],
                    icc_ci_upper = icc2_ci[2],
                    interpretation = .interpret_icc(icc2)
                ))

                table$setRow(rowNo=3, values=list(
                    icc_type = "ICC(3): Within-Cluster Agreement",
                    icc_value = icc3,
                    icc_ci_lower = icc3_ci[1],
                    icc_ci_upper = icc3_ci[2],
                    interpretation = .interpret_icc(icc3)
                ))

            }, error = function(e) {
                # Handle errors gracefully
                table$setRow(rowNo=1, values=list(
                    icc_type = "ICC(1): Between-Cluster",
                    icc_value = NA,
                    icc_ci_lower = NA,
                    icc_ci_upper = NA,
                    interpretation = paste0("Error: ", e$message)
                ))
            })
        },

        .populateVarianceComponents = function() {
            # Decompose total variance into hierarchical components
            # Shows how much variance is attributable to clusters vs. raters vs. subjects

            table <- self$results$varianceComponents
            if (is.null(table)) return()

            # CRITICAL FIX #3: Block variance components for categorical data
            # Variance decomposition on categorical data is statistically invalid
            data <- self$data
            vars <- self$options$vars

            # Check if data is categorical
            is_categorical <- FALSE
            for (var in vars) {
                col <- data[[var]]
                if (is.factor(col)) {
                    is_categorical <- TRUE
                    break
                }
                if (is.numeric(col)) {
                    n_unique <- length(unique(col[!is.na(col)]))
                    if (n_unique < 10) {
                        is_categorical <- TRUE
                        break
                    }
                }
            }

            if (is_categorical) {
                # Block variance components and show error notice
                # vc_error_notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = "varianceComponentsCategoricalError",
                #     type = jmvcore::NoticeType$ERROR
                # )

                # vc_error_notice$setContent(
                #     paste0("⛔ Variance components CANNOT be computed for categorical/ordinal data. ",
                #           "Variance decomposition requires continuous numeric measurements. ",
                #           "For categorical data, use kappa-based agreement measures instead.")
                # )
                # self$results$insert(999, vc_error_notice)

                # Populate table with error message
                table$setRow(rowNo=1, values=list(
                    component = "Analysis Blocked",
                    variance = NA,
                    pct_total = NA,
                    interpretation = "ERROR: Categorical data"
                ))

                table$setNote("categorical_error",
                    "⛔ Variance components require continuous numeric data.")

                return()  # Exit without computing variance components
            }

            # Check if required packages are available
            if (!requireNamespace("lme4", quietly = TRUE) &&
                !requireNamespace("nlme", quietly = TRUE)) {
                stop("Variance components require either 'lme4' or 'nlme' package")
            }

            cluster_var <- self$options$clusterVariable

            if (length(vars) < 2) {
                stop("Variance components require at least 2 raters")
            }

            # Reshape data to long format
            # Optimized: pre-allocate list and vectorize
            data_list <- lapply(seq_along(vars), function(rater_idx) {
                rater_name <- vars[rater_idx]
                data.frame(
                    subject = 1:nrow(data),
                    cluster = data[[cluster_var]],
                    rater = rater_name,
                    rating = as.numeric(data[[rater_name]]),
                    stringsAsFactors = FALSE
                )
            })
            long_data <- do.call(rbind, data_list)

            # Remove missing values
            long_data <- long_data[complete.cases(long_data), ]

            if (nrow(long_data) < 10) {
                stop("Insufficient data for variance component estimation")
            }

            # Convert to factors
            long_data$subject <- as.factor(long_data$subject)
            long_data$cluster <- as.factor(long_data$cluster)
            long_data$rater <- as.factor(long_data$rater)

            # Fit mixed effects model
            tryCatch({
                if (requireNamespace("lme4", quietly = TRUE)) {
                    # Use lme4 (preferred)
                    model <- lme4::lmer(rating ~ 1 + (1|cluster) + (1|subject) + (1|rater),
                                       data = long_data,
                                       REML = TRUE)

                    # Extract variance components
                    vc <- as.data.frame(lme4::VarCorr(model))
                    var_cluster <- vc$vcov[vc$grp == "cluster"]
                    var_subject <- vc$vcov[vc$grp == "subject"]
                    var_rater <- vc$vcov[vc$grp == "rater"]
                    var_residual <- vc$vcov[vc$grp == "Residual"]

                } else if (requireNamespace("nlme", quietly = TRUE)) {
                    # Use nlme as fallback
                    model <- nlme::lme(rating ~ 1,
                                      random = list(cluster = ~1, subject = ~1, rater = ~1),
                                      data = long_data,
                                      method = "REML")

                    vc <- nlme::VarCorr(model)
                    var_cluster <- as.numeric(vc["cluster", "Variance"])
                    var_subject <- as.numeric(vc["subject", "Variance"])
                    var_rater <- as.numeric(vc["rater", "Variance"])
                    var_residual <- as.numeric(vc["Residual", "Variance"])
                }

                # Calculate total variance
                total_var <- var_cluster + var_subject + var_rater + var_residual

                # Calculate percentages
                pct_cluster <- (var_cluster / total_var) * 100
                pct_subject <- (var_subject / total_var) * 100
                pct_rater <- (var_rater / total_var) * 100
                pct_residual <- (var_residual / total_var) * 100

                # Interpretation function
                .interpret_variance <- function(pct, component_type) {
                    if (is.na(pct)) return("Unable to calculate")

                    if (component_type == "cluster") {
                        if (pct < 5) return("Minimal between-cluster variation")
                        if (pct < 15) return("Low between-cluster variation")
                        if (pct < 30) return("Moderate between-cluster variation")
                        return("High between-cluster variation - investigate differences")
                    } else if (component_type == "subject") {
                        if (pct < 20) return("Low between-subject variation")
                        if (pct < 50) return("Moderate between-subject variation")
                        return("High between-subject variation (expected)")
                    } else if (component_type == "rater") {
                        if (pct < 5) return("Minimal rater bias")
                        if (pct < 15) return("Low rater bias")
                        if (pct < 30) return("Moderate rater bias - consider training")
                        return("High rater bias - training needed")
                    } else {  # residual
                        if (pct < 20) return("Low measurement error")
                        if (pct < 40) return("Moderate measurement error")
                        return("High measurement error - review protocol")
                    }
                }

                # Add rows based on user selection
                row_key <- 1

                if (self$options$betweenClusterVariance) {
                    table$addRow(rowKey=row_key, values=list(
                        component = "Between-Cluster Variance",
                        variance = var_cluster,
                        sd = sqrt(var_cluster),
                        percent_total = pct_cluster,
                        interpretation = .interpret_variance(pct_cluster, "cluster")
                    ))
                    row_key <- row_key + 1
                }

                if (self$options$withinClusterVariance) {
                    # Within-cluster includes subject, rater, and residual
                    table$addRow(rowKey=row_key, values=list(
                        component = "Between-Subject Variance",
                        variance = var_subject,
                        sd = sqrt(var_subject),
                        percent_total = pct_subject,
                        interpretation = .interpret_variance(pct_subject, "subject")
                    ))
                    row_key <- row_key + 1

                    table$addRow(rowKey=row_key, values=list(
                        component = "Between-Rater Variance",
                        variance = var_rater,
                        sd = sqrt(var_rater),
                        percent_total = pct_rater,
                        interpretation = .interpret_variance(pct_rater, "rater")
                    ))
                    row_key <- row_key + 1

                    table$addRow(rowKey=row_key, values=list(
                        component = "Residual Variance (Error)",
                        variance = var_residual,
                        sd = sqrt(var_residual),
                        percent_total = pct_residual,
                        interpretation = .interpret_variance(pct_residual, "residual")
                    ))
                }

            }, error = function(e) {
                # Handle errors gracefully
                table$addRow(rowKey=1, values=list(
                    component = "Error",
                    variance = NA,
                    sd = NA,
                    percent_total = NA,
                    interpretation = paste0("Error: ", e$message)
                ))
            })
        },

        .populateClusterSpecificKappa = function() {
            # Calculate kappa separately for each cluster/institution
            # Identifies which sites have poor agreement for quality control

            table <- self$results$clusterSpecificKappaTable
            if (is.null(table)) return()

            # CRITICAL FIX #6: Notify users shrinkage not implemented
            if (self$options$shrinkageEstimates) {
                # shrinkage_notice <- jmvcore::Notice$new(
                #     options = self$options,
                #     name = "shrinkageNotImplemented",
                #     type = jmvcore::NoticeType$WARNING
                # )
                # shrinkage_notice$setContent(
                #     paste0("Shrinkage estimates are not yet implemented. The shrinkage_kappa column will show NA values. ",
                #           "Empirical Bayes shrinkage toward overall kappa requires pooled estimation across all clusters, ",
                #           "which is planned for a future release. Use raw cluster-specific kappa values for now.")
                # )
                # self$results$insert(999, shrinkage_notice)
            }

            # Get data
            data <- self$data
            vars <- self$options$vars
            cluster_var <- self$options$clusterVariable

            if (length(vars) < 2) {
                stop("Cluster-specific kappa requires at least 2 raters")
            }

            # Get unique clusters
            clusters <- unique(data[[cluster_var]])
            clusters <- clusters[!is.na(clusters)]

            if (length(clusters) < 2) {
                stop("Cluster-specific kappa requires at least 2 clusters")
            }

            # Calculate kappa for each cluster
            for (cluster_name in clusters) {
                tryCatch({
                    private$.checkpoint()  # Allow UI to remain responsive

                    # Subset data for this cluster
                    cluster_data <- data[data[[cluster_var]] == cluster_name, ]

                    # Extract ratings for this cluster
                    ratings_matrix <- as.matrix(cluster_data[, vars, drop = FALSE])

                    # CRITICAL FIX #5: Use harmonized levels for cluster-specific kappa
                    # Ensure all clusters use same level set even if some clusters don't have all categories
                    if (!is.null(private$.harmonizedLevels) && all(sapply(as.data.frame(ratings_matrix), is.factor))) {
                        # Re-harmonize using the global level set
                        ratings_df <- as.data.frame(ratings_matrix)
                        ratings_df[] <- lapply(ratings_df, function(col) {
                            factor(as.character(col), levels = private$.harmonizedLevels,
                                  ordered = private$.isOrdered)
                        })
                        ratings_matrix <- as.matrix(ratings_df)
                    }

                    # Remove rows with all NA
                    complete_rows <- rowSums(!is.na(ratings_matrix)) > 0
                    ratings_matrix <- ratings_matrix[complete_rows, , drop = FALSE]

                    n_cases <- nrow(ratings_matrix)
                    n_raters <- length(vars)

                    if (n_cases < 3) {
                        # Not enough data for this cluster
                        table$addRow(rowKey=as.character(cluster_name), values=list(
                            cluster = as.character(cluster_name),
                            n_cases = n_cases,
                            n_raters = n_raters,
                            kappa = NA,
                            kappa_ci_lower = NA,
                            kappa_ci_upper = NA,
                            shrinkage_kappa = NA
                        ))
                        next
                    }

                    # Calculate kappa based on number of raters
                    if (n_raters == 2) {
                        # Cohen's kappa for 2 raters
                        if (requireNamespace("irr", quietly = TRUE)) {
                            kappa_result <- irr::kappa2(ratings_matrix, weight = "unweighted")
                            kappa_val <- kappa_result$value

                            # Approximate CI from standard error if available
                            if (!is.null(kappa_result$var) && !is.na(kappa_result$var) && kappa_result$var >= 0) {
                                se <- sqrt(kappa_result$var)
                            } else if (!is.null(kappa_result$statistic) &&
                                       !is.na(kappa_result$statistic) &&
                                       kappa_result$statistic != 0) {
                                se <- abs(kappa_val / kappa_result$statistic)
                            } else {
                                se <- NA
                            }

                            if (!is.na(se)) {
                                margin <- 1.96 * se
                                ci_lower <- max(-1, kappa_val - margin)
                                ci_upper <- min(1, kappa_val + margin)
                            } else {
                                ci_lower <- NA
                                ci_upper <- NA
                            }
                        } else {
                            kappa_val <- NA
                            ci_lower <- NA
                            ci_upper <- NA
                        }
                    } else {
                        # Fleiss' kappa for 3+ raters
                        if (requireNamespace("irr", quietly = TRUE)) {
                            kappa_result <- irr::kappam.fleiss(ratings_matrix)
                            kappa_val <- kappa_result$value

                            # Approximate CI
                            if (!is.null(kappa_result$var) && !is.na(kappa_result$var) && kappa_result$var >= 0) {
                                se <- sqrt(kappa_result$var)
                            } else if (!is.null(kappa_result$statistic) &&
                                       !is.na(kappa_result$statistic) &&
                                       kappa_result$statistic != 0) {
                                se <- abs(kappa_val / kappa_result$statistic)
                            } else {
                                se <- NA
                            }

                            if (!is.na(se)) {
                                margin <- 1.96 * se
                                ci_lower <- max(-1, kappa_val - margin)
                                ci_upper <- min(1, kappa_val + margin)
                            } else {
                                ci_lower <- NA
                                ci_upper <- NA
                            }
                        } else {
                            kappa_val <- NA
                            ci_lower <- NA
                            ci_upper <- NA
                        }
                    }

                    # Calculate shrinkage estimate if requested
                    shrinkage_kappa <- NA
                    if (self$options$shrinkageEstimates && !is.na(kappa_val)) {
                        # Simple empirical Bayes shrinkage toward grand mean
                        # This would require calculating the overall kappa across all clusters
                        # For now, leave as NA - full implementation would need grand mean
                        shrinkage_kappa <- NA
                    }

                    # Add row to table
                    table$addRow(rowKey=as.character(cluster_name), values=list(
                        cluster = as.character(cluster_name),
                        n_cases = n_cases,
                        n_raters = n_raters,
                        kappa = kappa_val,
                        kappa_ci_lower = ci_lower,
                        kappa_ci_upper = ci_upper,
                        shrinkage_kappa = shrinkage_kappa
                    ))

                }, error = function(e) {
                    # Handle errors for this cluster
                    table$addRow(rowKey=as.character(cluster_name), values=list(
                        cluster = as.character(cluster_name),
                        n_cases = 0,
                        n_raters = 0,
                        kappa = NA,
                        kappa_ci_lower = NA,
                        kappa_ci_upper = NA,
                        shrinkage_kappa = NA
                    ))
                })
            }
        },

        .testClusterHomogeneity = function() {
            # Test whether kappa is homogeneous across clusters
            # Null hypothesis: all clusters have equal kappa

            table <- self$results$clusterHomogeneityTest
            if (is.null(table)) return()

            # Get data
            data <- self$data
            vars <- self$options$vars
            cluster_var <- self$options$clusterVariable

            if (length(vars) < 2) {
                stop("Cluster homogeneity test requires at least 2 raters")
            }

            # Get unique clusters
            clusters <- unique(data[[cluster_var]])
            clusters <- clusters[!is.na(clusters)]

            if (length(clusters) < 2) {
                stop("Cluster homogeneity test requires at least 2 clusters")
            }

            tryCatch({
                # Calculate kappa for each cluster and collect values
                cluster_kappas <- c()
                cluster_ns <- c()
                cluster_names <- c()

                for (cluster_name in clusters) {
                    # Subset data for this cluster
                    cluster_data <- data[data[[cluster_var]] == cluster_name, ]

                    # Extract ratings for this cluster
                    ratings_matrix <- as.matrix(cluster_data[, vars, drop = FALSE])

                    # Remove rows with all NA
                    complete_rows <- rowSums(!is.na(ratings_matrix)) > 0
                    ratings_matrix <- ratings_matrix[complete_rows, , drop = FALSE]

                    n_cases <- nrow(ratings_matrix)
                    n_raters <- length(vars)

                    if (n_cases < 3) next  # Skip clusters with insufficient data

                    # Calculate kappa
                    if (n_raters == 2) {
                        if (requireNamespace("irr", quietly = TRUE)) {
                            kappa_result <- irr::kappa2(ratings_matrix, weight = "unweighted")
                            kappa_val <- kappa_result$value
                        } else {
                            next
                        }
                    } else {
                        if (requireNamespace("irr", quietly = TRUE)) {
                            kappa_result <- irr::kappam.fleiss(ratings_matrix)
                            kappa_val <- kappa_result$value
                        } else {
                            next
                        }
                    }

                    if (!is.na(kappa_val)) {
                        cluster_kappas <- c(cluster_kappas, kappa_val)
                        cluster_ns <- c(cluster_ns, n_cases)
                        cluster_names <- c(cluster_names, as.character(cluster_name))
                    }
                }

                if (length(cluster_kappas) < 2) {
                    table$setRow(rowNo=1, values=list(
                        test_statistic = NA,
                        df = NA,
                        p_value = NA,
                        conclusion = "Insufficient data for homogeneity test"
                    ))
                    return()
                }

                # Perform Cochran's Q-like test for kappa heterogeneity
                # Using weighted variance approach

                # Calculate overall weighted kappa
                total_n <- sum(cluster_ns)
                weights <- cluster_ns / total_n
                weighted_kappa <- sum(weights * cluster_kappas)

                # Calculate Q statistic (weighted sum of squared deviations)
                Q <- sum(cluster_ns * (cluster_kappas - weighted_kappa)^2)

                # Degrees of freedom
                df <- length(cluster_kappas) - 1

                # P-value from chi-square distribution
                p_value <- 1 - pchisq(Q, df)

                # Interpretation with EXPLORATORY disclaimer
                # CRITICAL FIX #4: Label test as exploratory/unvalidated
                if (p_value < 0.001) {
                    conclusion <- "Strong evidence of heterogeneity across clusters (p < 0.001) [EXPLORATORY]"
                } else if (p_value < 0.01) {
                    conclusion <- "Significant heterogeneity across clusters (p < 0.01) [EXPLORATORY]"
                } else if (p_value < 0.05) {
                    conclusion <- "Moderate heterogeneity across clusters (p < 0.05) [EXPLORATORY]"
                } else {
                    conclusion <- "No significant heterogeneity detected (homogeneous) [EXPLORATORY]"
                }

                # Populate table with disclaimer note
                table$setRow(rowNo=1, values=list(
                    test_statistic = Q,
                    df = df,
                    p_value = p_value,
                    conclusion = conclusion
                ))

                # Add critical warning note
                table$setNote("exploratory_warning",
                    paste0("⚠️ EXPLORATORY TEST: This heterogeneity test is not formally validated for kappa statistics. ",
                          "The Q statistic treats weighted kappa deviations as chi-square distributed, which lacks theoretical justification. ",
                          "P-values should be interpreted with caution. Bootstrap or permutation methods recommended for formal testing. ",
                          "See Fleiss & Davies (1982) and Donner & Eliasziw (1992) for validated approaches.")
                )

            }, error = function(e) {
                # Handle errors gracefully
                table$setRow(rowNo=1, values=list(
                    test_statistic = NA,
                    df = NA,
                    p_value = NA,
                    conclusion = paste0("Error: ", e$message)
                ))
            })
        },

        .populateClusterRankings = function() {
            # Rank clusters/institutions by agreement performance
            # Identifies best and worst performing sites

            table <- self$results$clusterRankingsTable
            if (is.null(table)) return()

            # Get data
            data <- self$data
            vars <- self$options$vars
            cluster_var <- self$options$clusterVariable

            if (length(vars) < 2) {
                stop("Cluster rankings require at least 2 raters")
            }

            # Get unique clusters
            clusters <- unique(data[[cluster_var]])
            clusters <- clusters[!is.na(clusters)]

            if (length(clusters) < 2) {
                stop("Cluster rankings require at least 2 clusters")
            }

            # Calculate kappa for each cluster
            cluster_results <- list()

            for (cluster_name in clusters) {
                tryCatch({
                    # Subset data for this cluster
                    cluster_data <- data[data[[cluster_var]] == cluster_name, ]

                    # Extract ratings for this cluster
                    ratings_matrix <- as.matrix(cluster_data[, vars, drop = FALSE])

                    # CRITICAL FIX #5: Use harmonized levels for cluster ranking kappa
                    if (!is.null(private$.harmonizedLevels) && all(sapply(as.data.frame(ratings_matrix), is.factor))) {
                        ratings_df <- as.data.frame(ratings_matrix)
                        ratings_df[] <- lapply(ratings_df, function(col) {
                            factor(as.character(col), levels = private$.harmonizedLevels,
                                  ordered = private$.isOrdered)
                        })
                        ratings_matrix <- as.matrix(ratings_df)
                    }

                    # Remove rows with all NA
                    complete_rows <- rowSums(!is.na(ratings_matrix)) > 0
                    ratings_matrix <- ratings_matrix[complete_rows, , drop = FALSE]

                    n_cases <- nrow(ratings_matrix)
                    n_raters <- length(vars)

                    if (n_cases < 3) next  # Skip clusters with insufficient data

                    # Calculate kappa
                    if (n_raters == 2) {
                        if (requireNamespace("irr", quietly = TRUE)) {
                            kappa_result <- irr::kappa2(ratings_matrix, weight = "unweighted")
                            kappa_val <- kappa_result$value
                        } else {
                            next
                        }
                    } else {
                        if (requireNamespace("irr", quietly = TRUE)) {
                            kappa_result <- irr::kappam.fleiss(ratings_matrix)
                            kappa_val <- kappa_result$value
                        } else {
                            next
                        }
                    }

                    if (!is.na(kappa_val)) {
                        cluster_results[[as.character(cluster_name)]] <- list(
                            cluster = as.character(cluster_name),
                            kappa = kappa_val,
                            n_cases = n_cases
                        )
                    }
                }, error = function(e) {
                    # Skip clusters with errors
                })
            }

            if (length(cluster_results) < 2) {
                return()  # Not enough clusters to rank
            }

            # Sort clusters by kappa (descending)
            cluster_df <- do.call(rbind, lapply(cluster_results, function(x) {
                data.frame(
                    cluster = x$cluster,
                    kappa = x$kappa,
                    n_cases = x$n_cases,
                    stringsAsFactors = FALSE
                )
            }))

            cluster_df <- cluster_df[order(-cluster_df$kappa), ]

            # Add ranks
            cluster_df$rank <- 1:nrow(cluster_df)

            # Performance rating based on kappa
            .rate_performance <- function(kappa) {
                if (is.na(kappa)) return("Unable to assess")
                if (kappa < 0) return("Poor (worse than chance)")
                if (kappa < 0.20) return("Slight agreement")
                if (kappa < 0.40) return("Fair agreement")
                if (kappa < 0.60) return("Moderate agreement")
                if (kappa < 0.80) return("Substantial agreement")
                return("Almost perfect agreement")
            }

            cluster_df$rating <- sapply(cluster_df$kappa, .rate_performance)

            # Add rows to table
            for (i in 1:nrow(cluster_df)) {
                table$addRow(rowKey=as.character(i), values=list(
                    rank = cluster_df$rank[i],
                    cluster = cluster_df$cluster[i],
                    kappa = cluster_df$kappa[i],
                    rating = cluster_df$rating[i]
                ))
            }
        }

    ))  # Close private list and R6Class
