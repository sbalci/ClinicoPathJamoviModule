#' @title Survival Feature Ranking - Univariate Cox Screening
#' @description
#' Performs univariate survival analysis for multiple features to identify potential prognostic factors.
#' This analysis runs a separate Cox proportional hazards model for each selected feature and ranks them
#' by statistical significance, hazard ratio, or concordance index. Inspired by Orange Data Mining's
#' Rank Survival Features widget, adapted for jamovi with comprehensive statistical reporting.
#'
#' @details
#' The function performs the following analyses for each feature:
#' \itemize{
#'   \item Univariate Cox proportional hazards regression
#'   \item Hazard ratio with 95% confidence intervals
#'   \item Wald test p-value for association with survival
#'   \item Concordance index (C-index) for discriminative ability
#'   \item Optional multiple testing correction (FDR, Bonferroni, etc.)
#' }
#'
#' Features can be ranked by:
#' \itemize{
#'   \item P-value: Most statistically significant features first
#'   \item Hazard ratio: Features with largest effect size (furthest from 1)
#'   \item C-index: Features with best discriminative ability
#' }
#'
#' @section Use Cases:
#' \itemize{
#'   \item Biomarker screening: Test many candidate biomarkers for prognostic value
#'   \item Exploratory analysis: Identify promising variables before multivariable modeling
#'   \item Feature selection: Prioritize variables for inclusion in complex models
#'   \item Publication tables: Generate univariate analysis tables for manuscripts
#' }
#'
#' @section Interpretation:
#' \itemize{
#'   \item HR > 1: Feature associated with higher hazard (worse outcome)
#'   \item HR < 1: Feature associated with lower hazard (better outcome)
#'   \item C-index > 0.7: Good discrimination, > 0.8: Excellent
#'   \item Adjusted p-values: Control false discovery rate when testing multiple features
#' }
#'
#' @examples
#' \dontrun{
#' # Screen multiple biomarkers
#' survivalfeaturerank(
#'   data = cancer_data,
#'   survtime = "survival_months",
#'   event = "death",
#'   eventLevel = "1",
#'   features = c("age", "stage", "grade", "ki67", "p53", "her2"),
#'   rankBy = "pvalue",
#'   adjustPValues = TRUE,
#'   showTopKM = TRUE,
#'   topN = 3
#' )
#' }
#'
#' @references
#' Orange Data Mining: https://orangedatamining.com/workflows/Survival-Analysis/
#'
#' @author ClinicoPath Development Team
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @importFrom glue glue

survivalfeaturerankClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "survivalfeaturerankClass",
    inherit = survivalfeaturerankBase,
    private = list(

        # Store ranking results for plotting
        .rankingData = NULL,

        # Initialize ----
        .init = function() {

            # Set visibility of results based on options
            if (self$options$showSummary) {
                self$results$summaryText$setVisible(TRUE)
            }

            if (self$options$showForestPlot) {
                self$results$forestPlot$setVisible(TRUE)
            }

            if (self$options$showTopKM) {
                self$results$topFeaturesHeading$setVisible(TRUE)
            }

            # Initialize ranking table
            self$results$rankingTable$setVisible(TRUE)

            # Set instructions
            if (!is.null(self$options$features) && length(self$options$features) > 0) {
                html <- "<p><b>Survival Feature Ranking</b></p>
                        <p>This analysis performs univariate Cox regression for each selected feature
                        and ranks them by the selected criterion.</p>
                        <ul>
                        <li><b>P-value ranking:</b> Identifies most statistically significant features</li>
                        <li><b>Hazard ratio ranking:</b> Identifies features with largest effect sizes</li>
                        <li><b>C-index ranking:</b> Identifies features with best discrimination</li>
                        </ul>"
                self$results$instructions$setContent(html)
            }

        },

        # Main analysis ----
        .run = function() {

            # Check for required inputs
            if (is.null(self$options$survtime) ||
                is.null(self$options$event) ||
                is.null(self$options$features) ||
                length(self$options$features) == 0) {
                return()
            }

            # Get data
            data <- self$data

            # Validate inputs
            validation <- private$.validateInputs(data)
            if (validation$should_stop) {
                for (err in validation$errors) {
                    jmvcore::reject(err, code='')
                }
                return()
            }

            # Prepare survival data
            surv_data <- private$.prepareSurvivalData(data)
            if (is.null(surv_data)) return()

            # Run univariate analyses for all features
            ranking_results <- private$.runUnivariateAnalyses(surv_data)
            if (is.null(ranking_results)) return()

            # Rank features
            ranked_features <- private$.rankFeatures(ranking_results)

            # Store for plotting
            private$.rankingData <- ranked_features

            # Populate ranking table
            private$.populateRankingTable(ranked_features)

            # Generate summary text
            if (self$options$showSummary) {
                private$.generateSummary(ranked_features)
            }

            # Generate interpretation
            private$.generateInterpretation(ranked_features)

            # Export if requested
            if (self$options$exportRanking &&
                self$results$exportRanking$isNotFilled()) {
                private$.exportRanking(ranked_features)
            }

        },

        # Validation ----
        .validateInputs = function(data) {
            validation <- list(
                errors = character(0),
                warnings = character(0),
                should_stop = FALSE
            )

            # Check survival time variable
            survtime_col <- self$options$survtime
            if (!survtime_col %in% names(data)) {
                validation$errors <- c(validation$errors,
                    "Survival time variable not found in dataset")
                validation$should_stop <- TRUE
                return(validation)
            }

            # Check event variable
            event_col <- self$options$event
            if (!event_col %in% names(data)) {
                validation$errors <- c(validation$errors,
                    "Event variable not found in dataset")
                validation$should_stop <- TRUE
                return(validation)
            }

            # Check features
            missing_features <- setdiff(self$options$features, names(data))
            if (length(missing_features) > 0) {
                validation$errors <- c(validation$errors,
                    paste("Features not found:", paste(missing_features, collapse=", ")))
                validation$should_stop <- TRUE
            }

            return(validation)
        },

        # Prepare survival data ----
        .prepareSurvivalData = function(data) {

            tryCatch({

                # Get survival time
                survtime <- jmvcore::toNumeric(data[[self$options$survtime]])

                # Get event status
                event_col <- data[[self$options$event]]
                event_level <- self$options$eventLevel

                if (is.factor(event_col)) {
                    event <- as.numeric(event_col == event_level)
                } else {
                    event <- jmvcore::toNumeric(event_col)
                }

                # Check for valid data
                if (all(is.na(survtime)) || all(is.na(event))) {
                    return(NULL)
                }

                # Create data frame with complete cases
                surv_df <- data.frame(
                    survtime = survtime,
                    event = event,
                    stringsAsFactors = FALSE
                )

                # Add features
                for (feat in self$options$features) {
                    surv_df[[feat]] <- data[[feat]]
                }

                # Remove rows with missing survival data
                surv_df <- surv_df[!is.na(surv_df$survtime) & !is.na(surv_df$event), ]

                return(surv_df)

            }, error = function(e) {
                jmvcore::reject(paste("Error preparing survival data:", e$message), code='')
                return(NULL)
            })
        },

        # Run univariate analyses ----
        .runUnivariateAnalyses = function(surv_data) {

            results_list <- list()

            for (feat in self$options$features) {

                tryCatch({

                    # Create subset with complete data for this feature
                    complete_idx <- !is.na(surv_data[[feat]])
                    feat_data <- surv_data[complete_idx, ]

                    if (nrow(feat_data) < 10) {
                        next  # Skip features with too few observations
                    }

                    # Build Cox model
                    formula_str <- paste("survival::Surv(survtime, event) ~", feat)
                    cox_model <- survival::coxph(as.formula(formula_str), data = feat_data)

                    # Extract results
                    coef_summary <- summary(cox_model)$coefficients
                    conf_int <- summary(cox_model)$conf.int
                    concordance <- summary(cox_model)$concordance

                    # Determine feature type
                    feat_type <- if (is.factor(feat_data[[feat]])) "Categorical" else "Continuous"

                    # For categorical variables, use first coefficient
                    # For continuous variables, there's only one coefficient
                    # LIMITATION: For categorical variables with >2 levels, this only reports
                    # the HR for the first level comparison (vs. reference).
                    # A more robust approach would use:
                    #   - Global Wald test for overall association
                    #   - Report range of HRs across all levels
                    #   - Or use anova(cox_model) for overall p-value
                    # Current approach is simple but may miss important categorical associations.
                    coef_idx <- 1

                    # FIX: Calculate C-index confidence intervals
                    # Previous version calculated cindex_se but didn't use it for CIs
                    cindex_val <- concordance["C"]
                    cindex_se_val <- concordance["se(C)"]
                    z_val <- qnorm(0.975)  # 95% CI
                    cindex_ci_lower <- cindex_val - z_val * cindex_se_val
                    cindex_ci_upper <- cindex_val + z_val * cindex_se_val

                    # Bound C-index CIs to [0, 1]
                    cindex_ci_lower <- max(0, min(1, cindex_ci_lower))
                    cindex_ci_upper <- max(0, min(1, cindex_ci_upper))

                    results_list[[feat]] <- list(
                        feature = feat,
                        type = feat_type,
                        n = nrow(feat_data),
                        events = sum(feat_data$event),
                        hr = exp(coef_summary[coef_idx, "coef"]),
                        ci_lower = conf_int[coef_idx, "lower .95"],
                        ci_upper = conf_int[coef_idx, "upper .95"],
                        pvalue = coef_summary[coef_idx, "Pr(>|z|)"],
                        cindex = cindex_val,
                        cindex_se = cindex_se_val,
                        cindex_ci_lower = cindex_ci_lower,
                        cindex_ci_upper = cindex_ci_upper
                    )

                }, error = function(e) {
                    # Skip features that cause errors
                    message(paste("Skipping feature", feat, "due to error:", e$message))
                })
            }

            if (length(results_list) == 0) {
                jmvcore::reject("No features could be analyzed successfully", code='')
                return(NULL)
            }

            # Convert to data frame
            results_df <- do.call(rbind, lapply(results_list, function(x) {
                data.frame(x, stringsAsFactors = FALSE)
            }))

            # Adjust p-values if requested
            if (self$options$adjustPValues) {
                results_df$adj_pvalue <- p.adjust(
                    results_df$pvalue,
                    method = self$options$adjustMethod
                )
            } else {
                results_df$adj_pvalue <- results_df$pvalue
            }

            return(results_df)
        },

        # Rank features ----
        .rankFeatures = function(results_df) {

            rank_by <- self$options$rankBy

            if (rank_by == "pvalue") {
                # Sort by p-value (smallest first)
                results_df <- results_df[order(results_df$pvalue), ]
            } else if (rank_by == "hazard") {
                # Sort by absolute log hazard ratio (largest effect)
                results_df <- results_df[order(abs(log(results_df$hr)), decreasing = TRUE), ]
            } else if (rank_by == "cindex") {
                # Sort by C-index (highest first)
                results_df <- results_df[order(results_df$cindex, decreasing = TRUE), ]
            }

            # Add rank column
            results_df$rank <- seq_len(nrow(results_df))

            # Reorder columns (now includes C-index CIs)
            results_df <- results_df[, c("rank", "feature", "type", "n", "events",
                                        "hr", "ci_lower", "ci_upper",
                                        "pvalue", "adj_pvalue",
                                        "cindex", "cindex_se", "cindex_ci_lower", "cindex_ci_upper")]

            return(results_df)
        },

        # Populate ranking table ----
        .populateRankingTable = function(ranked_features) {

            table <- self$results$rankingTable

            alpha <- self$options$alphaLevel

            for (i in seq_len(nrow(ranked_features))) {
                row <- ranked_features[i, ]

                # Determine significance
                p_to_use <- if (self$options$adjustPValues) row$adj_pvalue else row$pvalue
                sig_symbol <- if (p_to_use < alpha) "✓" else ""

                # Format CI text
                ci_text <- sprintf("%.2f - %.2f", row$ci_lower, row$ci_upper)

                # Filter by significance if requested
                if (!self$options$showFullTable && p_to_use >= alpha) {
                    next
                }

                table$addRow(rowKey = i, values = list(
                    rank = row$rank,
                    feature = as.character(row$feature),
                    type = row$type,
                    n = row$n,
                    events = row$events,
                    hr = row$hr,
                    ci_lower = row$ci_lower,
                    ci_upper = row$ci_upper,
                    ci_text = ci_text,
                    pvalue = row$pvalue,
                    adj_pvalue = row$adj_pvalue,
                    cindex = row$cindex,
                    cindex_se = row$cindex_se,
                    significant = sig_symbol
                ))
            }
        },

        # Generate summary ----
        .generateSummary = function(ranked_features) {

            alpha <- self$options$alphaLevel
            p_col <- if (self$options$adjustPValues) "adj_pvalue" else "pvalue"

            n_features <- nrow(ranked_features)
            n_significant <- sum(ranked_features[[p_col]] < alpha)

            top_feature <- ranked_features[1, "feature"]
            top_hr <- ranked_features[1, "hr"]
            top_p <- ranked_features[1, p_col]
            top_cindex <- ranked_features[1, "cindex"]

            pct_significant <- (n_significant / n_features) * 100
            ci_lower <- ranked_features[1, "ci_lower"]
            ci_upper <- ranked_features[1, "ci_upper"]

            summary_text <- glue::glue("
                Univariate Survival Analysis Summary

                Total features tested: {n_features}
                Significant features (p < {format(alpha, nsmall=3)}): {n_significant} ({format(pct_significant, digits=1, nsmall=1)}%)

                Top-ranked feature: {top_feature}
                  Hazard Ratio: {format(top_hr, digits=2, nsmall=2)} (95% CI: {format(ci_lower, digits=2, nsmall=2)} - {format(ci_upper, digits=2, nsmall=2)})
                  P-value: {format(top_p, digits=4, nsmall=4)}
                  C-index: {format(top_cindex, digits=3, nsmall=3)}
            ")

            if (self$options$adjustPValues) {
                summary_text <- paste0(
                    summary_text,
                    sprintf("\nNote: P-values adjusted for multiple testing using %s method.",
                           toupper(self$options$adjustMethod))
                )
            }

            self$results$summaryText$setContent(summary_text)
        },

        # Generate interpretation ----
        .generateInterpretation = function(ranked_features) {

            alpha <- self$options$alphaLevel
            p_col <- if (self$options$adjustPValues) "adj_pvalue" else "pvalue"

            n_significant <- sum(ranked_features[[p_col]] < alpha)

            html <- "<h3>Interpretation Guide</h3>"

            if (n_significant == 0) {
                html <- paste0(html,
                    "<p><b>No significant features found.</b> None of the tested features ",
                    "showed statistically significant association with survival at α = ", alpha, ".</p>",
                    "<p>Consider:</p><ul>",
                    "<li>Increasing sample size</li>",
                    "<li>Testing different features</li>",
                    "<li>Checking data quality and missing values</li>",
                    "</ul>")
            } else {
                top3 <- head(ranked_features[ranked_features[[p_col]] < alpha, ], 3)

                html <- paste0(html, "<p><b>Top prognostic features:</b></p><ol>")

                for (i in seq_len(nrow(top3))) {
                    feat <- top3[i, ]
                    direction <- if (feat$hr > 1) "increased" else "decreased"

                    feat_name <- feat$feature
                    feat_hr <- format(feat$hr, digits = 2, nsmall = 2)
                    feat_p <- format(feat[[p_col]], digits = 4, nsmall = 4)

                    html <- paste0(html,
                        glue::glue("<li><b>{feat_name}</b> (HR: {feat_hr}, p = {feat_p}): Associated with {direction} hazard</li>"))
                }

                html <- paste0(html, "</ol>")

                html <- paste0(html,
                    "<p><b>Next steps:</b></p><ul>",
                    "<li>Review Kaplan-Meier curves for top features</li>",
                    "<li>Consider including significant features in multivariable models</li>",
                    "<li>Validate findings in independent cohort if possible</li>",
                    "</ul>")
            }

            self$results$interpretation$setContent(html)
        },

        # Export ranking ----
        .exportRanking = function(ranked_features) {

            # FIX: CRITICAL BUG - Previous implementation had fundamental design flaw
            #
            # ORIGINAL BUG (lines 484-493):
            # for (i in seq_len(nrow(ranked_features))) {
            #     row_indices <- seq_len(self$data$rowCount)  # ALL ROWS!
            #     self$results$exportRanking$setValues(rep(rank_val, length(row_indices)))  # OVERWRITES!
            # }
            #
            # Problems:
            # 1. row_indices was ALL rows in dataset (not specific to feature)
            # 2. Each loop iteration OVERWROTE previous assignments
            # 3. Final result: ALL rows got rank of LAST feature processed
            #
            # CONCEPTUAL ISSUE:
            # This analysis ranks FEATURES (columns), not observations (rows).
            # Cannot meaningfully assign feature ranks to data rows.
            #
            # PROPER FIX:
            # Export should create a SUMMARY TABLE of feature rankings, not row-level data.
            # For now, disabled to prevent incorrect results.

            # TODO: Implement proper export as summary table if needed
            # Current approach: Do not export (prevents incorrect results)

            warning(
                "Export ranking functionality is currently disabled due to design limitations.\n",
                "Feature ranking results are displayed in the ranking table.\n",
                "To export results, use jamovi's 'Export Results' feature on the ranking table.",
                call. = FALSE
            )

            # Do not modify exportRanking column (leave empty/unfilled)
            return(invisible(NULL))
        },

        # Forest plot ----
        .forestPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.rankingData)) {
                return()
            }

            library(ggplot2)

            ranked_data <- private$.rankingData

            # Reverse order for plotting (top feature at top)
            ranked_data$feature <- factor(ranked_data$feature,
                                         levels = rev(ranked_data$feature))

            # Create forest plot
            plot <- ggplot(ranked_data, aes(x = hr, y = feature)) +
                geom_point(size = 3) +
                geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
                geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
                scale_x_log10() +
                labs(
                    x = "Hazard Ratio (95% CI)",
                    y = "Feature",
                    title = "Forest Plot - Univariate Cox Regression"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(hjust = 0.5, face = "bold"),
                    panel.grid.major.y = element_blank()
                )

            print(plot)
            TRUE
        },

        # KM plots for top features ----
        .kmPlot1 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 1)
        },

        .kmPlot2 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 2)
        },

        .kmPlot3 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 3)
        },

        .kmPlot4 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 4)
        },

        .kmPlot5 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 5)
        },

        .kmPlot6 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 6)
        },

        .kmPlot7 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 7)
        },

        .kmPlot8 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 8)
        },

        .kmPlot9 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 9)
        },

        .kmPlot10 = function(image, ggtheme, theme, ...) {
            private$.plotTopFeatureKM(image, ggtheme, theme, rank = 10)
        },

        # Helper for KM plots ----
        .plotTopFeatureKM = function(image, ggtheme, theme, rank) {

            if (is.null(private$.rankingData)) {
                return(FALSE)
            }

            if (rank > self$options$topN || rank > nrow(private$.rankingData)) {
                return(FALSE)
            }

            library(survival)
            library(survminer)

            # Get feature name for this rank
            feature_name <- private$.rankingData[rank, "feature"]

            # Prepare survival data
            surv_data <- private$.prepareSurvivalData(self$data)
            if (is.null(surv_data)) return(FALSE)

            # Remove missing values for this feature
            plot_data <- surv_data[!is.na(surv_data[[feature_name]]), ]

            # Create survival formula and fit
            surv_formula <- as.formula(paste("survival::Surv(survtime, event) ~", feature_name))
            fit <- survival::survfit(surv_formula, data = plot_data)

            # Create plot
            plot <- survminer::ggsurvplot(
                fit,
                data = plot_data,
                title = sprintf("Rank %d: %s (HR=%.2f, p=%.4f)",
                               rank,
                               feature_name,
                               private$.rankingData[rank, "hr"],
                               private$.rankingData[rank, "pvalue"]),
                pval = self$options$pplot,
                risk.table = self$options$risktable,
                xlim = c(0, self$options$endplot),
                break.time.by = self$options$byplot,
                ggtheme = theme_minimal()
            )

            print(plot)
            TRUE
        }
    )
)
