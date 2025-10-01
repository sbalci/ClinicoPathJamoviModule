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








    }))
