#' @title Diagnostic Test Meta-Analysis for Pathology
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom mada reitsma phm
#' @importFrom metafor rma
#' @importFrom htmltools tagList
#' @importFrom stats qnorm pnorm qt pt
#' @export
#' @return An \code{R6} class generator object for the \code{diagnosticmetaClass} backend; used internally by the jamovi analysis wrapper and not called directly.


diagnosticmetaClass <- R6::R6Class(
    "diagnosticmetaClass",
    inherit = diagnosticmetaBase,
    private = list(
        # Cache variables for performance optimization
        .mada_data = NULL,
        .analysis_data = NULL,
        .data_cache_valid = FALSE,
        .continuity_correction = FALSE,
        .correction_method_used = "none",
        .corrected_study_names = character(0),
        .pooled_sensitivity = NULL,
        .pooled_specificity = NULL,
        # Confidence intervals for the pooled estimates, so the interpretation
        # text can qualify a claim the interval does not support.
        .pooled_sens_ci = NULL,
        .pooled_spec_ci = NULL,
        .pooled_sens_pi = NULL,
        .pooled_spec_pi = NULL,
        .n_studies = 0,
        .biv_model = NULL,

        # Helper for null-safe operations
        `%||%` = function(x, y) {
            if (is.null(x)) y else x
        },

        .metaforMethod = function(method) {
            switch(
                tolower(method %||% "reml"),
                "fixed" = "FE",
                "ml" = "ML",
                "reml" = "REML",
                "mm" = "DL",
                "vc" = "HE",
                "REML"
            )
        },

        .metaforLevel = function() {
            level <- self$options$confidence_level %||% 95
            max(50, min(99, as.numeric(level)))
        },

        .renderSymbols = function(text) {
            replacements <- c(
                "[[SUP2]]" = intToUtf8(0x00B2),
                "[[GE]]" = intToUtf8(0x2265),
                "[[APPROX]]" = intToUtf8(0x2248),
                "[[TIMES]]" = intToUtf8(0x00D7)
            )
            for (token in names(replacements)) {
                text <- gsub(token, replacements[[token]], text, fixed = TRUE)
            }
            text
        },

        # Helper function to get color palette for accessibility
        .getColorPalette = function() {
            palette_option <- self$options$color_palette %||% "standard"

            switch(palette_option,
                "standard" = list(
                    primary = "darkblue",
                    secondary = "darkgreen",
                    tertiary = "darkred",
                    study_points = "gray"
                ),
                "colorblind_safe" = list(
                    primary = "#0173B2",     # Blue
                    secondary = "#029E73",   # Green
                    tertiary = "#CC78BC",    # Pink
                    study_points = "#56B4E9" # Light blue
                ),
                "high_contrast" = list(
                    primary = "#000000",     # Black
                    secondary = "#FFFFFF",   # White
                    tertiary = "#808080",    # Gray
                    study_points = "#404040" # Dark gray
                ),
                "viridis" = list(
                    primary = "#440154",     # Dark purple
                    secondary = "#21908C",   # Teal
                    tertiary = "#FDE725",    # Yellow
                    study_points = "#35B779"  # Green
                ),
                "plasma" = list(
                    primary = "#0D0887",     # Dark blue
                    secondary = "#CC4678",   # Pink
                    tertiary = "#F0F921",    # Yellow
                    study_points = "#7E03A8"  # Purple
                ),
                # Default fallback
                list(
                    primary = "darkblue",
                    secondary = "darkgreen",
                    tertiary = "darkred",
                    study_points = "gray"
                )
            )
        },

        .init = function() {

            # Initialize content for all Html outputs
            private$.populateInstructions()
            private$.populateAboutPanel()
            private$.populateInterpretation()

            # Set initial visibility based on data state
            self$results$instructions$setVisible(TRUE)
            self$results$summary$setVisible(FALSE)
            self$results$about$setVisible(self$options$show_methodology)
            self$results$interpretation$setVisible(FALSE)

            # Fixed row structure for the bivariate results table: the same five
            # parameters with the same labels on every run. Only the estimates
            # depend on the fitted model, so .run() fills them with setRow().
            biv_parameters <- list(
                sensitivity = "Pooled Sensitivity",
                specificity = "Pooled Specificity",
                plr         = "Positive Likelihood Ratio",
                nlr         = "Negative Likelihood Ratio",
                dor         = "Diagnostic Odds Ratio"
            )
            for (biv_key in names(biv_parameters))
                self$results$bivariateresults$addRow(
                    rowKey = biv_key,
                    values = list(parameter = biv_parameters[[biv_key]]))

        },
        
        .run = function() {

            # TODO (jamovify): i18n - this function emits substantial English
            # text (HTML banners in .populateWelcome/.populateAbout, table
            # notes, sprintf-formatted summaries in .generateSummary /
            # .generateBasicSummary, all interpretation/explanation panels).
            # Currently zero `.()` calls wrap the literals, so the strings are
            # not extractable for translation. Run `/prepare-translation
            # diagnosticmeta` to wrap them and produce the .po file.

            # TODO (jamovify): notices migration - error reporting uses ad-hoc
            # patterns: (a) the 5 `setNote("error", sprintf(... e$message))`
            # tryCatch handlers at lines 222/241/253/265/277, (b) HTML alert
            # banners via `.appendInstructionMessage()`, (c) plain HTML
            # written to `self$results$instructions$setContent(...)` at 151
            # and 186. Migrate to the structured notices pattern documented
            # in docs/NOTICE_TO_HTML_CONVERSION_GUIDE.md and demonstrated in
            # R/waterfall.b.R (`.addNotice` / `.renderNotices`). Adds proper
            # severity tiers (ERROR/STRONG_WARNING/WARNING/INFO) and a
            # consistent UI location instead of the mix of `instructions`,
            # table-footer notes, and welcome-panel overlays today.

            # Invalidate cache when data changes
            private$.data_cache_valid <- FALSE
            private$.mada_data <- NULL
            private$.analysis_data <- NULL
            private$.continuity_correction <- FALSE
            private$.pooled_sensitivity <- NULL
            private$.pooled_specificity <- NULL
            private$.biv_model <- NULL

            # Check if data is ready
            if (is.null(self$data) || nrow(self$data) == 0) {
                # Show instructions when no data
                self$results$instructions$setVisible(TRUE)
                return()
            }
            
            # Get variables with safe escaping for special characters
            # Use raw option values as column-name lookup keys. The previous
            # .escapeVar() helper mangled names containing spaces/punctuation
            # (e.g. "Study Name (2020)" became "Study_Name_2020_") and then
            # used the mangled string as a self$data[[...]] key, which silently
            # returned NULL and broke the analysis. The variable names never
            # flow into a formula in this function, so no escaping is needed.
            study_var <- self$options$study
            tp_var <- self$options$true_positives
            fp_var <- self$options$false_positives
            fn_var <- self$options$false_negatives
            tn_var <- self$options$true_negatives

            # Check if all required variables are provided
            all_provided <- !is.null(study_var) && !is.null(tp_var) &&
                          !is.null(fp_var) && !is.null(fn_var) && !is.null(tn_var)

            if (!all_provided) {
                # Show instructions if any variables are missing
                self$results$instructions$setVisible(TRUE)
                return()
            }

            # All variables provided - hide instructions and proceed with analysis
            self$results$instructions$setVisible(FALSE)
            
            # Extract data
            data <- self$data
            
            # Create diagnostic test data structure
            meta_data <- data.frame(
                row_id = seq_len(nrow(data)),
                study = as.character(data[[study_var]]),
                tp = as.numeric(data[[tp_var]]),
                fp = as.numeric(data[[fp_var]]),
                fn = as.numeric(data[[fn_var]]),
                tn = as.numeric(data[[tn_var]]),
                stringsAsFactors = FALSE
            )

            # Store original row count for validation
            original_n <- nrow(meta_data)

            # Exclude unusable studies and SAY SO.
            #
            # A single missing cell anywhere previously aborted the entire
            # analysis, and negative counts were dropped silently while the
            # original_n passed to .validateStudyData was never used - so the
            # reported number of studies was the post-exclusion count with no
            # indication that anything had been removed.
            incomplete <- is.na(meta_data$tp) | is.na(meta_data$fp) |
                          is.na(meta_data$fn) | is.na(meta_data$tn)
            negative   <- !incomplete &
                          (meta_data$tp < 0 | meta_data$fp < 0 |
                           meta_data$fn < 0 | meta_data$tn < 0)
            # A study with no diseased or no non-diseased participants yields no
            # sensitivity or no specificity and cannot enter a bivariate model.
            empty_arm  <- !incomplete & !negative &
                          ((meta_data$tp + meta_data$fn) == 0 |
                           (meta_data$fp + meta_data$tn) == 0)

            drop <- incomplete | negative | empty_arm
            meta_data <- meta_data[!drop, , drop = FALSE]

            if (any(drop)) {
                reasons <- character(0)
                if (sum(incomplete) > 0)
                    reasons <- c(reasons, sprintf("%d with missing counts", sum(incomplete)))
                if (sum(negative) > 0)
                    reasons <- c(reasons, sprintf("%d with negative counts", sum(negative)))
                if (sum(empty_arm) > 0)
                    reasons <- c(reasons, sprintf("%d with no diseased or no non-diseased participants", sum(empty_arm)))
                private$.appendInstructionMessage(sprintf(
                    paste0("<div class='alert alert-warning'><h4>Studies excluded</h4><p>",
                           "%d of %d studies were excluded before analysis (%s). ",
                           "All results below are based on the remaining %d studies.</p></div>"),
                    sum(drop), original_n, paste(reasons, collapse = "; "), nrow(meta_data)))
            }

            if (nrow(meta_data) < 3) {
                # Fatal: reject() so jamovi greys the results and reports an
                # analysis-level error. Writing this into the instructions panel
                # left the pane looking healthy, so a hard stop read as advice.
                jmvcore::reject(
                    jmvcore::format(
                        .("At least 3 studies with complete diagnostic test data are required ({n} found)."),
                        n = nrow(meta_data)),
                    code = "insufficient_studies")
            }

            # Enhanced validation with user-friendly warnings. Its own
            # <3-studies branch is unreachable now that the reject above fires
            # first; it is kept as a guard for any other caller.
            validation_result <- private$.validateStudyData(meta_data, original_n)
            if (!validation_result) {
                return()
            }

            # Store number of studies for summary
            private$.n_studies <- nrow(meta_data)


            prepared_data <- private$.prepareAnalysisData(meta_data)
            analysis_data <- prepared_data$analysis_data
            mada_data <- prepared_data$mada_data

            # Prepare data for analysis - debug info removed for clean output

            # Perform bivariate meta-analysis when enabled
            if (isTRUE(self$options$bivariate_analysis)) {
                tryCatch({
                    private$.performBivariateMetaAnalysis(meta_data = meta_data,
                                                          analysis_data = analysis_data,
                                                          mada_data = mada_data)
                    private$.generateSummary(meta_data)
                }, error = function(e) {
                    private$.pooled_sensitivity <- NULL
                    private$.pooled_specificity <- NULL
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Instead, use table note
                    self$results$bivariateresults$setNote("error", sprintf('Bivariate analysis error: %s', htmltools::htmlEscape(e$message)))
                    private$.generateSummary(meta_data)
                })
            } else {
                private$.pooled_sensitivity <- NULL
                private$.pooled_specificity <- NULL
                self$results$bivariateresults$setNote("disabled", "Bivariate analysis disabled by user option")
                private$.generateBasicSummary(meta_data)
            }
            self$results$summary$setVisible(self$options$show_analysis_summary)

            # Perform the Holling proportional-hazards SROC analysis if requested
            if (isTRUE(self$options$hsroc_analysis)) {
                tryCatch({
                    private$.performPHMSROCAnalysis(
                        meta_data = meta_data,
                        mada_data = mada_data
                    )
                }, error = function(e) {
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Use table note instead
                    self$results$hsrocresults$setNote(
                        "error",
                        sprintf(
                            "Proportional-hazards SROC analysis error: %s",
                            htmltools::htmlEscape(e$message)
                        )
                    )
                })
            }

            # Perform heterogeneity analysis
            if (isTRUE(self$options$heterogeneity_analysis)) {
                tryCatch({
                    private$.performHeterogeneityAnalysis(meta_data = meta_data,
                                                          analysis_data = analysis_data)
                }, error = function(e) {
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Use table note instead
                    self$results$heterogeneity$setNote("error", sprintf('Heterogeneity analysis error: %s', htmltools::htmlEscape(e$message)))
                })
            }

            # Perform meta-regression if covariate specified
            if (isTRUE(self$options$meta_regression)) {
                tryCatch({
                    private$.performMetaRegression(meta_data = meta_data,
                                                   analysis_data = analysis_data)
                }, error = function(e) {
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Use table note instead
                    self$results$metaregression$setNote("error", sprintf('Meta-regression error: %s', htmltools::htmlEscape(e$message)))
                })
            }

            # Perform publication bias assessment
            if (isTRUE(self$options$publication_bias)) {
                tryCatch({
                    private$.performPublicationBiasAssessment(meta_data = meta_data,
                                                              analysis_data = analysis_data)
                }, error = function(e) {
                    # SERIALIZATION FIX: Don't insert Notice objects (causes serialization errors)
                    # Use table note instead
                    self$results$publicationbias$setNote("error", sprintf('Publication bias analysis error: %s', htmltools::htmlEscape(e$message)))
                })
            }
            
            # Generate plots
            if (self$options$forest_plot) {
                private$.populateForestPlot(analysis_data)
            }

            if (self$options$sroc_plot) {
                private$.populateSROCPlot(meta_data)
            }

            if (self$options$funnel_plot && self$options$publication_bias) {
                private$.populateFunnelPlot(analysis_data)
            }

            # Generate plot explanations if requested
            if (self$options$show_plot_explanations) {
                if (self$options$forest_plot) {
                    private$.populateForestPlotExplanation()
                }
                if (self$options$sroc_plot) {
                    private$.populateSROCPlotExplanation()
                }
                if (self$options$funnel_plot && self$options$publication_bias) {
                    private$.populateFunnelPlotExplanation()
                }
            }

            # Populate individual studies table
            if (self$options$show_individual_studies) {
                private$.populateIndividualStudies(meta_data)
            }

            # Set visibility for interpretation and about based on options and data completeness
            if (!is.null(study_var) && !is.null(tp_var) && !is.null(fp_var) &&
                !is.null(fn_var) && !is.null(tn_var)) {
                self$results$interpretation$setVisible(self$options$show_interpretation)
                self$results$about$setVisible(self$options$show_methodology)
            }

            # SERIALIZATION FIX: Removed completion notice (causes serialization errors)
            # Analysis completion info is shown in summary table instead
        },
        
        .performBivariateMetaAnalysis = function(meta_data, analysis_data, mada_data) {

            # Check data availability
            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                self$results$bivariateresults$setNote("data_error", "Analysis data is missing or empty")
                return()
            }

            if (!requireNamespace("mada", quietly = TRUE)) {
                self$results$bivariateresults$setNote("package_error", "mada package is not available")
                return()
            }

            if (is.null(mada_data) || nrow(mada_data) < 3) {
                n_rows <- if (is.null(mada_data)) 0 else nrow(mada_data)
                self$results$bivariateresults$setNote("insufficient_data", paste("Insufficient data for meta-analysis. Found", n_rows, "studies, need at least 3"))
                return()
            }

            method_option <- self$options$method %||% "reml"

            # Ensure we only use methods supported by mada::reitsma
            valid_methods <- c("fixed", "ml", "reml", "mm", "vc")
            if (!(method_option %in% valid_methods)) {
                method_used <- "reml"
                # SERIALIZATION FIX: Use table note instead of inserting Notice
                self$results$bivariateresults$setNote("method_warning", sprintf(
                    "Method '%s' is not supported for bivariate diagnostic meta-analysis. Using REML instead.",
                    method_option
                ))
            } else {
                method_used <- method_option
            }

            conf_level <- (self$options$confidence_level %||% 95) / 100
            conf_level <- max(min(conf_level, 0.999), 0.5)

            # Be explicit about the continuity correction rather than inheriting
            # mada's defaults (correction = 0.5, correction.control = "all").
            #
            # Under "all", a single zero cell anywhere causes mada to add 0.5 to
            # ALL FOUR cells of EVERY study. The option offering that is labelled
            # "None (Model-Based)" and is the default, so the setting a user picks
            # to AVOID a correction applied a heavier one than either option that
            # advertises a correction - and the disclosure block was gated on
            # correction_method != "none", making it the only setting that said
            # nothing. "single" corrects only the affected studies.
            correction_method <- self$options$zero_cell_correction %||% "none"
            zero_present <- any(mada_data[, c("TP", "FP", "FN", "TN")] == 0, na.rm = TRUE)

            biv_model <- mada::reitsma(
                mada_data, method = method_used,
                correction = 0.5,
                correction.control = if (identical(correction_method, "none")) "single" else "all"
            )

            if (zero_present) {
                n_zero_studies <- sum(rowSums(mada_data[, c("TP", "FP", "FN", "TN")] == 0) > 0)
                bivariate_table_early <- self$results$bivariateresults
                bivariate_table_early$setNote("zero_cell", sprintf(
                    paste("%d study/studies contain a zero cell. A continuity correction of 0.5 was applied to",
                          "%s so the model could be fitted. Continuity corrections shift the pooled estimate;",
                          "compare the alternatives under Zero Cell Correction to see how sensitive your result is."),
                    n_zero_studies,
                    if (identical(correction_method, "none")) "those studies only"
                    else "all studies"))
            }
            private$.biv_model <- biv_model
            summary_results <- tryCatch(
                summary(biv_model, level = conf_level),
                error = function(e) NULL
            )
            coefficients <- if (!is.null(summary_results)) {
                summary_results[["coefficients"]]
            } else {
                NULL
            }

            # mada 0.5.12 fits method = "fixed" correctly, but its
            # summary.reitsma() assumes a random-effects covariance matrix and
            # errors. Build the same four summary rows directly from the fitted
            # fixed-effect coefficients and covariance matrix.
            if (is.null(coefficients) && method_used == "fixed") {
                fixed_coef <- biv_model$coefficients
                fixed_vcov <- biv_model$vcov
                if (is.matrix(fixed_coef) && nrow(fixed_coef) >= 1 &&
                    all(c("tsens", "tfpr") %in% colnames(fixed_coef)) &&
                    is.matrix(fixed_vcov) && nrow(fixed_vcov) >= 2) {
                    logit_estimate <- c(
                        tsens = fixed_coef[1, "tsens"],
                        tfpr = fixed_coef[1, "tfpr"]
                    )
                    standard_error <- sqrt(diag(fixed_vcov)[1:2])
                    z_value <- logit_estimate / standard_error
                    p_value <- 2 * stats::pnorm(-abs(z_value))
                    z_critical <- stats::qnorm(1 - (1 - conf_level) / 2)
                    logit_lower <- logit_estimate - z_critical * standard_error
                    logit_upper <- logit_estimate + z_critical * standard_error
                    ci_prefix <- paste0(100 * conf_level, "%ci.")

                    coefficients <- rbind(
                        "tsens.(Intercept)" = c(
                            logit_estimate["tsens"], standard_error[1],
                            z_value["tsens"], p_value["tsens"],
                            logit_lower["tsens"], logit_upper["tsens"]
                        ),
                        "tfpr.(Intercept)" = c(
                            logit_estimate["tfpr"], standard_error[2],
                            z_value["tfpr"], p_value["tfpr"],
                            logit_lower["tfpr"], logit_upper["tfpr"]
                        ),
                        "sensitivity" = c(
                            stats::plogis(logit_estimate["tsens"]),
                            NA_real_, NA_real_, NA_real_,
                            stats::plogis(logit_lower["tsens"]),
                            stats::plogis(logit_upper["tsens"])
                        ),
                        "false pos. rate" = c(
                            stats::plogis(logit_estimate["tfpr"]),
                            NA_real_, NA_real_, NA_real_,
                            stats::plogis(logit_lower["tfpr"]),
                            stats::plogis(logit_upper["tfpr"])
                        )
                    )
                    colnames(coefficients) <- c(
                        "Estimate", "Std. Error", "z", "Pr(>|z|)",
                        paste0(ci_prefix, "lb"), paste0(ci_prefix, "ub")
                    )
                    self$results$bivariateresults$setNote(
                        "fixed_summary",
                        paste(
                            "Fixed-effect confidence intervals were computed",
                            "from the fitted Reitsma covariance matrix."
                        )
                    )
                }
            }

            if (is.null(coefficients) || !is.matrix(coefficients)) {
                self$results$bivariateresults$setNote("model_error", "Reitsma model failed - coefficient matrix missing")
                private$.appendInstructionMessage(
                    "<div class='alert alert-warning'><h4> Bivariate Output Missing</h4><p>The Reitsma model did not return coefficient estimates, so pooled sensitivity and specificity are unavailable.</p></div>"
                )
                return()
            }

            coef_rows <- rownames(coefficients)
            get_row <- function(target) {
                if (is.null(coef_rows)) {
                    return(NULL)
                }
                idx <- which(coef_rows == target)
                if (length(idx) == 0) {
                    idx <- which(tolower(coef_rows) == tolower(target))
                }
                if (length(idx) == 0) {
                    return(NULL)
                }
                coefficients[idx[1], , drop = FALSE]
            }

            sens_prob_row <- get_row("sensitivity")
            fpr_prob_row <- get_row("false pos. rate")
            sens_logit_row <- get_row("tsens.(Intercept)")
            fpr_logit_row <- get_row("tfpr.(Intercept)")

            if (is.null(sens_prob_row) || is.null(fpr_prob_row) ||
                is.null(sens_logit_row) || is.null(fpr_logit_row)) {
                self$results$bivariateresults$setNote(
                    "model_error",
                    "Reitsma model returned unexpected coefficient structure"
                )
                private$.appendInstructionMessage(
                    "<div class='alert alert-warning'><h4> Bivariate Output Missing</h4><p>The Reitsma model did not return the expected coefficient estimates, so pooled sensitivity and specificity are unavailable.</p></div>"
                )
                return()
            }

            safe_lr <- function(numer, denom) {
                if (!is.finite(numer) || !is.finite(denom) || denom <= 0) {
                    return(NA_real_)
                }
                numer / denom
            }

            z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

            bivariate_table <- self$results$bivariateresults

            ci_lower_col <- grep("ci\\.lb$", colnames(coefficients), value = TRUE)
            ci_upper_col <- grep("ci\\.ub$", colnames(coefficients), value = TRUE)
            ci_lower_col <- ci_lower_col[1]
            ci_upper_col <- ci_upper_col[1]

            pooled_sens <- sens_prob_row[1, "Estimate"]
            sens_ci <- c(NA_real_, NA_real_)
            if (!is.null(ci_lower_col) && !is.null(ci_upper_col)) {
                sens_ci <- c(sens_prob_row[1, ci_lower_col], sens_prob_row[1, ci_upper_col])
            }
            private$.pooled_sensitivity <- pooled_sens

            fpr_estimate <- fpr_prob_row[1, "Estimate"]
            pooled_spec <- if (is.finite(fpr_estimate)) 1 - fpr_estimate else NA_real_
            spec_ci <- c(NA_real_, NA_real_)
            if (!is.null(ci_lower_col) && !is.null(ci_upper_col)) {
                fpr_ci_lower <- fpr_prob_row[1, ci_lower_col]
                fpr_ci_upper <- fpr_prob_row[1, ci_upper_col]
                if (is.finite(fpr_ci_lower) && is.finite(fpr_ci_upper)) {
                    spec_ci <- c(1 - fpr_ci_upper, 1 - fpr_ci_lower)
                }
            }
            private$.pooled_specificity <- pooled_spec

            # Heterogeneity assessment: Use Q-statistic from bivariate model
            # Note: Univariate I^2 values ignore bivariate correlation and overstate precision
            # Proper assessment requires examining the bivariate covariance structure
            sens_i2 <- NA_real_
            spec_i2 <- NA_real_

            # SERIALIZATION FIX: Use table note instead of inserting Notice
            self$results$bivariateresults$setNote("heterogeneity_info",
                "I\u{00b2} is not reported here: a univariate I\u{00b2} ignores the within-study correlation between sensitivity and specificity and does not describe the bivariate model (Zwinderman & Bossuyt 2008). The Heterogeneity Assessment table reports Q, tau-squared and a univariate I\u{00b2} computed SEPARATELY for sensitivity and for specificity - read those as descriptive summaries of each margin, not as the heterogeneity of the bivariate model. For a model-consistent statement of how much studies differ, use the prediction interval above and the prediction region on the SROC plot."
            )

            # The p-values on the sensitivity/specificity rows are the Wald tests
            # of the logit intercepts, i.e. H0: sensitivity = 50% and
            # H0: false-positive rate = 50%. That is almost never the hypothesis a
            # reader assumes from an unlabelled "p" column, and it is trivially
            # significant for any usable test, so name it.
            bivariate_table$setNote("pvalue_meaning", paste(
                "P-values on the sensitivity and specificity rows test the null hypothesis that the",
                "parameter equals 50% (no better than chance), not that the test meets any clinical",
                "threshold. They are significant for essentially any usable assay and should not be",
                "read as evidence of adequate accuracy - use the confidence and prediction intervals",
                "for that. Sensitivity and specificity rows are percentages; likelihood ratios and the",
                "diagnostic odds ratio are ratios."))

            private$.pooled_sens_ci <- sens_ci * 100
            private$.pooled_spec_ci <- spec_ci * 100

            # Prediction interval for a FUTURE study, and a heterogeneity warning.
            #
            # The pooled point plus its confidence interval describes how well the
            # AVERAGE is known; it says nothing about whether the assay performs
            # consistently. With substantial between-study variance the analysis
            # would previously report a tight pooled estimate with no indication
            # that a new laboratory could see something quite different. Built
            # from vcov + Psi, the same quantity mada uses for its prediction
            # region, so the note and the SROC plot agree.
            tryCatch({
                Sig_m <- stats::vcov(biv_model)
                Psi_m <- biv_model$Psi
                if (is.matrix(Sig_m) && is.matrix(Psi_m) &&
                    all(dim(Sig_m) >= 2) && all(dim(Psi_m) >= 2)) {
                    tot <- Sig_m[1:2, 1:2] + Psi_m[1:2, 1:2]
                    mu_l <- as.numeric(stats::coef(biv_model))
                    if (all(is.finite(diag(tot))) && all(diag(tot) >= 0) &&
                        length(mu_l) >= 2 && all(is.finite(mu_l[1:2]))) {
                        z <- z_crit
                        sens_pi <- stats::plogis(mu_l[1] + c(-1, 1) * z * sqrt(tot[1, 1])) * 100
                        # tfpr -> specificity: spec = 1 - plogis(tfpr)
                        spec_pi <- sort((1 - stats::plogis(mu_l[2] + c(-1, 1) * z * sqrt(tot[2, 2]))) * 100)

                        pct <- self$options$confidence_level %||% 95
                        private$.pooled_sens_pi <- sens_pi
                        private$.pooled_spec_pi <- spec_pi

                        bivariate_table$setNote("prediction", sprintf(
                            paste("Prediction interval for a future study (%d%%): sensitivity %.1f%%-%.1f%%,",
                                  "specificity %.1f%%-%.1f%%. This is where a NEW study or laboratory is expected",
                                  "to fall and includes between-study heterogeneity; the confidence intervals",
                                  "above describe only how precisely the pooled average is estimated."),
                            pct, sens_pi[1], sens_pi[2], spec_pi[1], spec_pi[2]))

                        # Flag when heterogeneity, not sampling error, dominates.
                        if (is.finite(sens_pi[2] - sens_pi[1]) &&
                            ((sens_pi[2] - sens_pi[1]) > 30 || (spec_pi[2] - spec_pi[1]) > 30)) {
                            private$.appendInstructionMessage(paste0(
                                "<div class='alert alert-warning'><h4>Substantial between-study heterogeneity</h4>",
                                "<p>The prediction interval spans a wide range of accuracy (sensitivity ",
                                sprintf("%.0f%%-%.0f%%", sens_pi[1], sens_pi[2]), ", specificity ",
                                sprintf("%.0f%%-%.0f%%", spec_pi[1], spec_pi[2]), "). ",
                                "A single pooled sensitivity and specificity may not usefully describe this ",
                                "body of evidence: studies differ more than sampling error explains, commonly ",
                                "because of differing positivity thresholds, patient spectrum or reference ",
                                "standards. Prefer the SROC curve and the prediction region over the pooled ",
                                "point, and investigate the source of heterogeneity before applying these ",
                                "figures to your own practice.</p></div>"))
                        }
                    }
                }
            }, error = function(e) NULL)

            bivariate_table$setRow(rowKey = "sensitivity", values = list(
                estimate = pooled_sens * 100,  # Convert to percentage
                ci_lower = sens_ci[1] * 100,   # Convert to percentage
                ci_upper = sens_ci[2] * 100,   # Convert to percentage
                i_squared = sens_i2,
                p_value = sens_logit_row[1, "Pr(>|z|)"]
            ))

            bivariate_table$setRow(rowKey = "specificity", values = list(
                estimate = pooled_spec * 100,  # Convert to percentage
                ci_lower = spec_ci[1] * 100,   # Convert to percentage
                ci_upper = spec_ci[2] * 100,   # Convert to percentage
                i_squared = spec_i2,
                p_value = fpr_logit_row[1, "Pr(>|z|)"]
            ))

            pooled_plr <- safe_lr(pooled_sens, 1 - pooled_spec)
            pooled_nlr <- safe_lr(1 - pooled_sens, pooled_spec)
            pooled_dor <- safe_lr(pooled_plr, pooled_nlr)

            lr_ci <- list(plr = c(NA_real_, NA_real_),
                          nlr = c(NA_real_, NA_real_),
                          dor = c(NA_real_, NA_real_))

            vcov_matrix <- biv_model$vcov
            if (is.matrix(vcov_matrix) && all(dim(vcov_matrix) >= 2) &&
                all(is.finite(vcov_matrix[1:2, 1:2])) &&
                is.finite(pooled_sens) && is.finite(pooled_spec)) {

                var_logit_sens <- vcov_matrix[1, 1]
                var_logit_spec <- vcov_matrix[2, 2]
                # mada's Reitsma model is parameterized in (tsens, tfpr) =
                # (logit sensitivity, logit FALSE-positive rate). Because
                # logit(spec) = logit(1 - FPR) = -tfpr, the covariance between
                # logit(sens) and logit(spec) is the NEGATIVE of vcov[1, 2].
                # The three delta-method variance formulas below add
                # 2 * grad_s * grad_p * cov_sens_spec, so using the correctly
                # signed covariance yields v1 + v2 - 2c for DOR, etc.
                cov_sens_spec <- -vcov_matrix[1, 2]

                if (var_logit_sens >= 0 && var_logit_spec >= 0) {
                    if (is.finite(pooled_plr) && pooled_plr > 0) {
                        var_log_plr <- ((1 - pooled_sens)^2 * var_logit_sens) +
                                       (pooled_spec^2 * var_logit_spec) +
                                       (2 * (1 - pooled_sens) * pooled_spec * cov_sens_spec)
                        if (is.finite(var_log_plr) && var_log_plr >= 0) {
                            se_log_plr <- sqrt(var_log_plr)
                            lr_ci$plr <- exp(log(pooled_plr) + c(-1, 1) * z_crit * se_log_plr)
                        }
                    }

                    if (is.finite(pooled_nlr) && pooled_nlr > 0) {
                        var_log_nlr <- (pooled_sens^2 * var_logit_sens) +
                                       ((1 - pooled_spec)^2 * var_logit_spec) +
                                       (2 * pooled_sens * (1 - pooled_spec) * cov_sens_spec)
                        if (is.finite(var_log_nlr) && var_log_nlr >= 0) {
                            se_log_nlr <- sqrt(var_log_nlr)
                            lr_ci$nlr <- exp(log(pooled_nlr) + c(-1, 1) * z_crit * se_log_nlr)
                        }
                    }

                    if (is.finite(pooled_dor) && pooled_dor > 0) {
                        var_log_dor <- var_logit_sens + var_logit_spec + 2 * cov_sens_spec
                        if (is.finite(var_log_dor) && var_log_dor >= 0) {
                            se_log_dor <- sqrt(var_log_dor)
                            lr_ci$dor <- exp(log(pooled_dor) + c(-1, 1) * z_crit * se_log_dor)
                        }
                    }
                }
            }

            bivariate_table$setRow(rowKey = "plr", values = list(
                estimate = pooled_plr,
                ci_lower = lr_ci$plr[1],
                ci_upper = lr_ci$plr[2],
                i_squared = NA_real_,
                p_value = NA_real_
            ))

            bivariate_table$setRow(rowKey = "nlr", values = list(
                estimate = pooled_nlr,
                ci_lower = lr_ci$nlr[1],
                ci_upper = lr_ci$nlr[2],
                i_squared = NA_real_,
                p_value = NA_real_
            ))

            bivariate_table$setRow(rowKey = "dor", values = list(
                estimate = pooled_dor,
                ci_lower = lr_ci$dor[1],
                ci_upper = lr_ci$dor[2],
                i_squared = NA_real_,
                p_value = NA_real_
            ))

            # Analysis completed successfully - table should be populated
            bivariate_table$setNote("success", "Analysis completed successfully - table populated")
            bivariate_table$setNote("method", paste("Reitsma model estimated via", method_used))
        },
        
        .performPHMSROCAnalysis = function(meta_data, mada_data) {

            if (requireNamespace("mada", quietly = TRUE)) {

                hsroc_table <- self$results$hsrocresults
                hsroc_table$deleteRows()

                # Validate input data
                if (is.null(meta_data) || nrow(meta_data) == 0) {
                    self$results$hsrocresults$setNote(
                        "insufficient",
                        "Insufficient data for proportional-hazards SROC analysis"
                    )
                    return()
                }

                # Check for required columns
                required_cols <- c("tp", "fp", "fn", "tn")
                if (!all(required_cols %in% names(meta_data))) {
                    self$results$hsrocresults$setNote(
                        "missing",
                        "Missing required columns for proportional-hazards SROC analysis"
                    )
                    return()
                }

                if (is.null(mada_data) || nrow(mada_data) == 0) {
                    self$results$hsrocresults$setNote(
                        "invalid",
                        "Processed data unavailable for proportional-hazards SROC analysis"
                    )
                    return()
                }

                # Check if we have enough data
                if (nrow(mada_data) < 3) {
                    self$results$hsrocresults$setNote(
                        "toofew",
                        "Proportional-hazards SROC analysis requires at least 3 studies"
                    )
                    return()
                }

                # mada::phm() applies its documented continuity correction when
                # zero cells are present. Surface that behavior to the user.
                zero_cells <- any(meta_data$tp == 0 | meta_data$fp == 0 |
                                  meta_data$fn == 0 | meta_data$tn == 0)
                if (zero_cells) {
                    self$results$hsrocresults$setNote(
                        "zerocells",
                        paste(
                            "Zero cells detected; mada::phm applies a continuity",
                            "correction, so results should be interpreted cautiously."
                        )
                    )
                }

                # Fit the Holling proportional-hazards SROC model.
                hsroc_model <- tryCatch({
                    result <- mada::phm(mada_data)
                    if (is.null(result)) {
                        stop("Proportional-hazards SROC model fitting returned NULL")
                    }
                    # Model fitted successfully
                    result
                }, warning = function(w) {
                    # Try with warnings suppressed
                    tryCatch({
                        result <- suppressWarnings(mada::phm(mada_data))
                        if (is.null(result)) {
                            stop("Proportional-hazards SROC model fitting returned NULL after warning")
                        }
                        result
                    }, error = function(e2) {
                        self$results$hsrocresults$setNote(
                            "error",
                            paste("Proportional-hazards SROC fitting failed:", e2$message)
                        )
                        return(NULL)
                    })
                }, error = function(e) {
                    self$results$hsrocresults$setNote(
                        "error",
                        paste("Proportional-hazards SROC fitting error:", e$message)
                    )
                    return(NULL)
                })

                # Validate model object
                if (is.null(hsroc_model)) {
                    self$results$hsrocresults$setNote(
                        "failed",
                        "Proportional-hazards SROC fitting failed; check data quality"
                    )
                    return()
                }

                # Get summary with error handling
                hsroc_summary <- tryCatch({
                    result <- summary(
                        hsroc_model,
                        level = private$.metaforLevel() / 100
                    )
                    if (is.null(result)) {
                        stop("Proportional-hazards SROC summary is NULL")
                    }
                    result
                }, error = function(e) {
                    self$results$hsrocresults$setNote(
                        "summary_error",
                        paste("Proportional-hazards SROC summary error:", e$message)
                    )
                    return(NULL)
                })

                if (is.null(hsroc_summary)) {
                    return()
                }

                # Extract model parameters from the mada summary object.
                coefficients <- NULL
                tryCatch({
                    if (!is.null(hsroc_summary$object) && "coefficients" %in% names(hsroc_summary$object)) {
                        coefficients <- hsroc_summary$object$coefficients
                    } else if (!is.null(hsroc_summary) && "coefficients" %in% names(hsroc_summary)) {
                        coefficients <- hsroc_summary$coefficients
                    } else if (!is.null(hsroc_summary) && "coef" %in% names(hsroc_summary)) {
                        coefficients <- hsroc_summary$coef
                    }
                }, error = function(e) {
                    self$results$hsrocresults$setNote("coef_access_error", paste("Cannot access coefficients:", e$message))
                })

                if (is.null(coefficients)) {
                    self$results$hsrocresults$setNote(
                        "no_coefficients",
                        "Proportional-hazards SROC summary contains no coefficients"
                    )
                    return()
                }

                # Validate coefficient structure
                if (length(coefficients) == 0) {
                    self$results$hsrocresults$setNote(
                        "empty_coefficients",
                        "Proportional-hazards SROC coefficients are empty"
                    )
                    return()
                }

                # Handle coefficients as named vector (typical for mada phm)
                if (is.vector(coefficients) && !is.null(names(coefficients))) {
                    # Define parameter labels
                    param_labels <- list(
                        "theta" = "Diagnostic accuracy parameter (theta)",
                        "taus_sq" = "Between-study variance (tau^2)"
                    )

                    # Note: hsroc_table$deleteRows() was already called at the
                    # top of .performPHMSROCAnalysis(); no need to clear again.

                    # Process each coefficient
                    for (param_name in names(coefficients)) {
                        display_name <- param_labels[[param_name]] %||% param_name
                        estimate <- coefficients[param_name]

                        # Get variance/SE from vcov matrix if available
                        std_error <- NA_real_
                        z_value <- NA_real_
                        p_value <- NA_real_

                        if (!is.null(hsroc_summary$object$vcov)) {
                            vcov_matrix <- hsroc_summary$object$vcov
                            vcov_names <- rownames(vcov_matrix)

                            # Find the index of the parameter in the vcov matrix
                            param_idx <- which(names(coefficients) == param_name)

                            if (length(param_idx) > 0 && param_idx <= nrow(vcov_matrix)) {
                                variance <- vcov_matrix[param_idx, param_idx]
                                if (is.finite(variance) && variance > 0) {
                                    std_error <- sqrt(variance)
                                    if (is.finite(std_error) && std_error > 0 && is.finite(estimate)) {
                                        z_value <- estimate / std_error
                                        p_value <- 2 * (1 - stats::pnorm(abs(z_value)))
                                    }
                                }
                            }
                        }

                        hsroc_table$addRow(rowKey = param_name, values = list(
                            parameter = display_name,
                            estimate = estimate,
                            std_error = std_error,
                            z_value = z_value,
                            p_value = p_value
                        ))
                    }

                    hsroc_table$setNote(
                        "method",
                        paste(
                            "Holling proportional-hazards SROC model fitted",
                            "with adjusted profile maximum likelihood."
                        )
                    )
                } else {
                    self$results$hsrocresults$setNote(
                        "unsupported_format",
                        "Proportional-hazards SROC coefficient format is not supported"
                    )
                }
            }
        },
        
        .performHeterogeneityAnalysis = function(meta_data, analysis_data) {

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                return()
            }

            if (requireNamespace("metafor", quietly = TRUE)) {

                analysis_data$sens <- analysis_data$tp / (analysis_data$tp + analysis_data$fn)
                analysis_data$spec <- analysis_data$tn / (analysis_data$tn + analysis_data$fp)

                analysis_data$logit_sens <- stats::qlogis(analysis_data$sens)
                analysis_data$logit_spec <- stats::qlogis(analysis_data$spec)

                analysis_data$var_logit_sens <- 1 / analysis_data$tp + 1 / analysis_data$fn
                analysis_data$var_logit_spec <- 1 / analysis_data$tn + 1 / analysis_data$fp

                het_table <- self$results$heterogeneity
                het_table$deleteRows()

                sens_valid <- is.finite(analysis_data$logit_sens) &
                    is.finite(analysis_data$var_logit_sens) &
                    analysis_data$var_logit_sens > 0
                spec_valid <- is.finite(analysis_data$logit_spec) &
                    is.finite(analysis_data$var_logit_spec) &
                    analysis_data$var_logit_spec > 0

                dropped_sens <- sum(!sens_valid)
                dropped_spec <- sum(!spec_valid)
                if (dropped_sens > 0 || dropped_spec > 0) {
                    het_table$setNote(
                        "nonfinite_rows",
                        sprintf(
                            paste(
                                "Excluded non-finite study rows from univariate",
                                "heterogeneity models (sensitivity: %d; specificity: %d).",
                                "Choose a zero-cell correction to retain zero-cell studies."
                            ),
                            dropped_sens,
                            dropped_spec
                        )
                    )
                }

                rma_method <- private$.metaforMethod(self$options$method)
                rma_level <- private$.metaforLevel()

                fit_heterogeneity <- function(data, measure) {
                    if (nrow(data) < 2) {
                        het_table$setNote(
                            paste0("insufficient_", tolower(measure)),
                            paste("At least two finite studies are required for", measure)
                        )
                        return(NULL)
                    }

                    tryCatch(
                        metafor::rma(
                            yi = data$effect,
                            vi = data$variance,
                            method = rma_method,
                            level = rma_level
                        ),
                        error = function(e) {
                            het_table$setNote(
                                paste0("error_", tolower(measure)),
                                paste(measure, "heterogeneity model failed:", e$message)
                            )
                            NULL
                        }
                    )
                }

                sens_meta <- fit_heterogeneity(
                    data.frame(
                        effect = analysis_data$logit_sens[sens_valid],
                        variance = analysis_data$var_logit_sens[sens_valid]
                    ),
                    "Sensitivity"
                )
                spec_meta <- fit_heterogeneity(
                    data.frame(
                        effect = analysis_data$logit_spec[spec_valid],
                        variance = analysis_data$var_logit_spec[spec_valid]
                    ),
                    "Specificity"
                )

                add_heterogeneity_row <- function(row_key, measure, model) {
                    if (is.null(model)) {
                        return()
                    }
                    i_squared <- if (is.finite(model$QE) && model$QE > 0) {
                        max(0, (model$QE - (model$k - 1)) / model$QE * 100)
                    } else {
                        0
                    }
                    het_table$addRow(rowKey = row_key, values = list(
                        measure = measure,
                        q_statistic = model$QE,
                        df = model$k - 1,
                        p_value = model$QEp,
                        i_squared = i_squared,
                        tau_squared = model$tau2
                    ))

                    # Name the estimator. Each row is a SEPARATE univariate
                    # random-effects model on logit sensitivity or logit
                    # specificity; the bivariate table's note used to send readers
                    # here for "proper evaluation", which was contradictory since
                    # this is exactly the univariate I-squared that note disowns.
                    het_table$setNote("i2_meaning", paste(
                        "Each row is a separate univariate random-effects model on the logit of that",
                        "measure. The I-squared is therefore the proportion of variance in THAT margin",
                        "alone that is not attributable to sampling error; it does not describe the",
                        "bivariate model and should not be read as an overall heterogeneity figure",
                        "(Zwinderman & Bossuyt 2008). In diagnostic accuracy meta-analysis a high",
                        "I-squared is expected whenever studies used different positivity thresholds -",
                        "it signals a threshold effect to be modelled by the SROC curve, not",
                        "necessarily a defect. Use the prediction interval and the SROC prediction",
                        "region for a model-consistent statement of between-study variability."))
                }

                add_heterogeneity_row("sensitivity", "Sensitivity", sens_meta)
                add_heterogeneity_row("specificity", "Specificity", spec_meta)
                het_table$setNote(
                    "method",
                    paste("Univariate auxiliary models used", rma_method, "estimation.")
                )
            }
        },
        
        .performMetaRegression = function(meta_data, analysis_data) {

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                return()
            }

            covariate_var <- self$options$covariate
            if (is.null(covariate_var)) {
                # Add message when meta-regression is enabled but no covariate is selected
                if (isTRUE(self$options$meta_regression)) {
                    private$.appendInstructionMessage(
                        "<div class='alert alert-info'><h4> Meta-Regression Requires a Covariate</h4><p>To perform meta-regression analysis, please select a covariate variable (e.g., study year, population type, method) that may explain heterogeneity between studies. The covariate should be a study-level characteristic that varies across included studies.</p></div>"
                    )
                }
                return()
            }

            covariate_values <- self$data[[covariate_var]]
            if (is.null(covariate_values)) return()

            if (!"row_id" %in% names(meta_data)) {
                private$.appendInstructionMessage(
                    "<div class='alert alert-warning'><h4> Meta-Regression Skipped</h4><p>Row identifiers were not preserved during preprocessing, so the covariate could not be aligned with the filtered studies.</p></div>"
                )
                return()
            }

            analysis_data$covariate <- covariate_values[meta_data$row_id]
            analysis_data <- analysis_data[!is.na(analysis_data$covariate), , drop = FALSE]

            if (nrow(analysis_data) < 3) {
                # SERIALIZATION FIX: Use table note instead of inserting Notice
                self$results$metaregression$setNote("insufficient_data",
                    'Meta-regression not run: fewer than three studies remain after removing missing covariate values.')
                return()
            }

            # Guard on residual DEGREES OF FREEDOM, not just the study count.
            #
            # A categorical covariate contributes (levels - 1) parameters, so a
            # 4-level covariate on 5 studies leaves no residual df: the model is
            # saturated, fits perfectly, and reports confidence intervals that
            # mean nothing. metafor silently drops redundant predictors in that
            # situation ("Redundant predictors dropped from the model"), so the
            # table would appear normal while describing a different model from
            # the one requested.
            cov_vals <- analysis_data$covariate
            n_param <- if (is.factor(cov_vals) || is.character(cov_vals)) {
                max(length(unique(stats::na.omit(as.character(cov_vals)))) - 1, 1)
            } else 1
            n_studies_mr <- nrow(analysis_data)
            resid_df <- n_studies_mr - n_param - 1

            if (n_param >= 1 && length(unique(stats::na.omit(as.character(cov_vals)))) < 2) {
                self$results$metaregression$setNote("constant_covariate",
                    'Meta-regression not run: the covariate takes the same value in every study, so it cannot explain any between-study variation.')
                return()
            }

            if (resid_df < 1) {
                self$results$metaregression$setNote("overparameterised", sprintf(
                    paste('Meta-regression not run: %d studies cannot support a covariate contributing %d',
                          'model parameter(s) - no residual degrees of freedom remain, so the model would fit',
                          'perfectly and its confidence intervals would be meaningless. At least %d studies',
                          'are needed for this covariate, and 10 per covariate is the usual recommendation.'),
                    n_studies_mr, n_param, n_param + 2))
                return()
            }

            # Stability warning for small sample sizes
            if (n_studies_mr < 10 * n_param) {
                self$results$metaregression$setNote("small_sample_warning", sprintf(
                    paste('Meta-regression on %d studies with a covariate contributing %d model parameter(s)',
                          '(%d residual degrees of freedom). The usual recommendation is at least 10 studies',
                          'per covariate, so estimates may be unstable and confidence intervals unreliable.',
                          'Report this as exploratory only.'),
                    n_studies_mr, n_param, resid_df))
            }

            if (requireNamespace("metafor", quietly = TRUE)) {

                analysis_data$sens <- analysis_data$tp / (analysis_data$tp + analysis_data$fn)
                analysis_data$spec <- analysis_data$tn / (analysis_data$tn + analysis_data$fp)

                analysis_data$logit_sens <- stats::qlogis(analysis_data$sens)
                analysis_data$logit_spec <- stats::qlogis(analysis_data$spec)

                analysis_data$var_logit_sens <- 1 / analysis_data$tp + 1 / analysis_data$fn
                analysis_data$var_logit_spec <- 1 / analysis_data$tn + 1 / analysis_data$fp

                metareg_table <- self$results$metaregression
                metareg_table$deleteRows()

                sens_valid <- is.finite(analysis_data$logit_sens) &
                    is.finite(analysis_data$var_logit_sens) &
                    analysis_data$var_logit_sens > 0
                spec_valid <- is.finite(analysis_data$logit_spec) &
                    is.finite(analysis_data$var_logit_spec) &
                    analysis_data$var_logit_spec > 0
                sens_data <- analysis_data[sens_valid, , drop = FALSE]
                spec_data <- analysis_data[spec_valid, , drop = FALSE]

                dropped_sens <- sum(!sens_valid)
                dropped_spec <- sum(!spec_valid)
                if (dropped_sens > 0 || dropped_spec > 0) {
                    metareg_table$setNote(
                        "nonfinite_rows",
                        sprintf(
                            paste(
                                "Excluded non-finite study rows from meta-regression",
                                "(sensitivity: %d; specificity: %d).",
                                "Choose a zero-cell correction to retain zero-cell studies."
                            ),
                            dropped_sens,
                            dropped_spec
                        )
                    )
                }

                rma_method <- private$.metaforMethod(self$options$method)
                rma_level <- private$.metaforLevel()

                fit_meta_regression <- function(data, effect, variance, measure) {
                    if (nrow(data) < 3) {
                        metareg_table$setNote(
                            paste0("insufficient_", tolower(measure)),
                            paste(
                                "At least three finite studies are required for",
                                paste0(measure, " meta-regression.")
                            )
                        )
                        return(NULL)
                    }

                    tryCatch(
                        metafor::rma(
                            yi = data[[effect]],
                            vi = data[[variance]],
                            mods = ~ covariate,
                            data = data,
                            method = rma_method,
                            level = rma_level
                        ),
                        error = function(e) {
                            private$.appendInstructionMessage(
                                paste0(
                                    "<div class='alert alert-warning'><h4> ",
                                    measure,
                                    " Meta-Regression Failed</h4><p>",
                                    htmltools::htmlEscape(e$message),
                                    "</p></div>"
                                )
                            )
                            NULL
                        }
                    )
                }

                sens_metareg <- fit_meta_regression(
                    sens_data,
                    "logit_sens",
                    "var_logit_sens",
                    "Sensitivity"
                )
                spec_metareg <- fit_meta_regression(
                    spec_data,
                    "logit_spec",
                    "var_logit_spec",
                    "Specificity"
                )
                metareg_table$setNote(
                    "method",
                    paste("Meta-regression used", rma_method, "estimation.")
                )

                # Defense-in-depth: even though jamovi tables render cells as
                # plain text, the covariate parameter name originates from a
                # user-supplied column name. Render it through htmlEscape so
                # the cell stays safe if a future renderer ever interprets HTML.
                safe_covariate_label <- htmltools::htmlEscape(covariate_var)

                # Emit one row per non-intercept coefficient. A categorical
                # covariate with k > 2 levels produces k - 1 contrasts, so
                # iterate over 2:length(beta) instead of reporting only beta[2].
                # Label each row with the metafor coefficient name
                # (rownames(model$beta)); fall back to the covariate name.
                add_metareg_rows <- function(model, measure, key_prefix) {
                    if (is.null(model)) {
                        return()
                    }
                    metareg_table$addRow(
                        rowKey = paste0(key_prefix, "_intercept"),
                        values = list(
                            measure = measure,
                            parameter = "Intercept",
                            estimate = model$beta[1],
                            std_error = model$se[1],
                            z_value = model$zval[1],
                            p_value = model$pval[1]
                        )
                    )

                    n_coef <- length(model$beta)
                    if (n_coef < 2) {
                        return()
                    }

                    beta_names <- rownames(model$beta)
                    for (j in 2:n_coef) {
                        # Show the user's variable name. The model is fitted on an
                        # internal column literally named `covariate`, so
                        # rownames(model$beta) are "covariate", "covariateB", ...
                        # and the table said "covariate" instead of the variable
                        # the user actually chose.
                        coef_label <- if (!is.null(beta_names) &&
                            length(beta_names) >= j &&
                            !is.na(beta_names[j]) &&
                            nzchar(beta_names[j])) {
                            htmltools::htmlEscape(
                                sub("^covariate", paste0(covariate_var, " "), beta_names[j]))
                        } else {
                            safe_covariate_label
                        }
                        coef_label <- trimws(coef_label)
                        metareg_table$addRow(
                            rowKey = paste0(key_prefix, "_covariate_", j),
                            values = list(
                                measure = measure,
                                parameter = coef_label,
                                estimate = model$beta[j],
                                std_error = model$se[j],
                                z_value = model$zval[j],
                                p_value = model$pval[j]
                            )
                        )
                    }
                }

                add_metareg_rows(sens_metareg, "Sensitivity", "sens")
                add_metareg_rows(spec_metareg, "Specificity", "spec")
            }
        },
        
        .performPublicationBiasAssessment = function(meta_data, analysis_data) {

            if (is.null(analysis_data) || nrow(analysis_data) == 0) {
                return()
            }

            if (requireNamespace("metafor", quietly = TRUE)) {

                if (nrow(analysis_data) < 10) {
                    private$.appendInstructionMessage(
                        "<div class='alert alert-info'><h4> Publication Bias Caution</h4><p>Deeks' test is unreliable with fewer than 10 studies; interpret asymmetry results cautiously.</p></div>"
                    )
                }

                # Effective sample size for Deeks' test (Deeks, Macaskill & Irwig
                # 2005, J Clin Epidemiol 58:882-93):
                #
                #     ESS = 4 * n1 * n0 / (n1 + n0)
                #
                # where n1 = TP + FN (diseased) and n0 = FP + TN (non-diseased) -
                # i.e. twice the harmonic mean of the two GROUP sizes.
                #
                # This previously used 4 / (1/TP + 1/FN + 1/FP + 1/TN), the
                # harmonic mean of the four CELL counts, which is a different and
                # non-monotone function of the table: two studies with identical
                # ESS under Deeks' definition can get different values here, and
                # the ranking of studies by ESS changes (Spearman ~0.3 on a
                # realistic set). Since 1/sqrt(ESS) is the regression predictor,
                # the asymmetry test statistic is materially wrong - on a 10-study
                # example the correct test gives z = -8.25, p < 0.0001 while this
                # gave z = +1.58, p = 0.11, i.e. "No significant asymmetry"
                # reported for strongly asymmetric data, with the sign reversed.
                n_diseased <- analysis_data$tp + analysis_data$fn
                n_healthy  <- analysis_data$fp + analysis_data$tn
                analysis_data$ess <- 4 * n_diseased * n_healthy / (n_diseased + n_healthy)

                # Deeks' test needs finite log DOR, so a zero cell must be handled
                # here regardless of the user's model-level choice. With the
                # default ("none") a single zero made log_dor and se_log_dor
                # infinite and the whole test returned NaN with no explanation.
                cells <- c("tp", "fp", "fn", "tn")
                zero_rows <- rowSums(analysis_data[, cells] == 0) > 0
                n_zero <- sum(zero_rows)
                if (n_zero > 0) {
                    analysis_data[zero_rows, cells] <- analysis_data[zero_rows, cells] + 0.5
                }

                analysis_data$log_dor <- log((analysis_data$tp * analysis_data$tn) /
                                              (analysis_data$fp * analysis_data$fn))
                analysis_data$inv_root_ess <- 1 / sqrt(analysis_data$ess)

                # Deeks, Macaskill & Irwig (2005, J Clin Epidemiol 58:882-93):
                # regress log DOR on 1/sqrt(ESS), WEIGHTED BY ESS, and refer the
                # slope to a t distribution on k - 2 df.
                #
                # This previously used metafor::rma(vi = se_log_dor^2, method =
                # "FE") and reported a z. Inverse-variance weighting on
                # var(log DOR) = 1/TP+1/FP+1/FN+1/TN reintroduces exactly the
                # log-DOR/SE correlation Deeks' method exists to avoid - the same
                # defect as using Egger's, moved from the predictor into the
                # weights - and a normal reference is anticonservative at the
                # small k typical of DTA meta-analyses. On a 4-study example the
                # old form reported p = 0.0014 "Significant asymmetry" where
                # Deeks' own specification gives p = 0.26.
                ok_rows <- is.finite(analysis_data$log_dor) &
                           is.finite(analysis_data$inv_root_ess) &
                           is.finite(analysis_data$ess) & analysis_data$ess > 0
                fit_data <- analysis_data[ok_rows, , drop = FALSE]

                bias_table <- self$results$publicationbias
                bias_table$deleteRows()

                if (nrow(fit_data) < 3) {
                    bias_table$addRow(rowKey = "deeks_test", values = list(
                        test = "Deeks' Funnel Plot Asymmetry Test",
                        statistic = NA_real_, p_value = NA_real_,
                        interpretation = "Not estimable: at least 3 studies with finite odds ratios are required"
                    ))
                } else {
                    deeks_fit <- stats::lm(log_dor ~ inv_root_ess,
                                           data = fit_data, weights = fit_data$ess)
                    cf <- summary(deeks_fit)$coefficients
                    deeks_t <- cf[2, 3]
                    deeks_p <- cf[2, 4]
                    deeks_df <- stats::df.residual(deeks_fit)

                    bias_table$addRow(rowKey = "deeks_test", values = list(
                        test = "Deeks' Funnel Plot Asymmetry Test",
                        statistic = deeks_t,
                        p_value = deeks_p,
                        interpretation = ifelse(deeks_p < 0.05,
                                              "Significant asymmetry detected",
                                              "No significant asymmetry")
                    ))

                    note <- sprintf(paste(
                        "Deeks' test: log diagnostic odds ratio regressed on 1/sqrt(effective sample size),",
                        "weighted by effective sample size; slope referred to t on %d df (Deeks, Macaskill &",
                        "Irwig 2005). Asymmetry is not by itself evidence of publication bias - it can also",
                        "arise from between-study heterogeneity or a threshold effect."), deeks_df)
                    if (n_zero > 0) {
                        note <- paste(note, sprintf(
                            "A continuity correction of 0.5 was applied to %d study/studies with a zero cell so the odds ratio was finite.",
                            n_zero))
                    }
                    bias_table$setNote("deeks_method", note)
                }
            }
        },
        
        .populateForestPlot = function(meta_data) {

            image <- self$results$forestplot
            # SERIALIZATION FIX: Ensure it's a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }

            # RELOAD FIX: serialize the pooled sensitivity/specificity point and
            # their confidence intervals into the image state (mirroring
            # .populateSROCPlot) so the pooled diamond survives an .omv reload.
            # Reading private$.pooled_* / private$.biv_model at render time
            # returned NULL after a saved file was reopened without re-running.
            pooled_sens <- private$.pooled_sensitivity
            pooled_spec <- private$.pooled_specificity
            pooled_sens_ci <- c(NA_real_, NA_real_)
            pooled_spec_ci <- c(NA_real_, NA_real_)

            conf_level <- self$options$confidence_level
            if (is.null(conf_level) || length(conf_level) == 0 || !is.finite(conf_level)) {
                conf_level <- 95
            }
            conf_level <- min(max(conf_level, 50), 99) / 100

            if (!is.null(private$.biv_model)) {
                tryCatch({
                    summary_results <- summary(private$.biv_model, level = conf_level)
                    coefficients <- summary_results$coefficients

                    # CI FIX: build the Wald interval on the LOGIT scale using the
                    # tsens/tfpr intercept rows (estimate + SE both on link scale)
                    # and transform with plogis() exactly ONCE. The probability-
                    # scale rows ('sensitivity'/'false pos. rate') are already in
                    # [0, 1] and often carry NA SE, so applying plogis() to them
                    # double-squashed the interval.
                    get_logit_ci <- function(param_name) {
                        if (is.null(rownames(coefficients))) return(c(NA_real_, NA_real_))
                        idx <- which(rownames(coefficients) == param_name)
                        if (length(idx) == 0) return(c(NA_real_, NA_real_))
                        estimate <- coefficients[idx[1], 1]
                        se <- coefficients[idx[1], 2]
                        if (!is.finite(estimate) || !is.finite(se)) return(c(NA_real_, NA_real_))
                        z <- stats::qnorm(1 - (1 - conf_level) / 2)
                        c(estimate - z * se, estimate + z * se)
                    }

                    sens_ci_logit <- get_logit_ci("tsens.(Intercept)")
                    if (!any(is.na(sens_ci_logit))) {
                        pooled_sens_ci <- stats::plogis(sens_ci_logit)
                    }

                    # logit(spec) = -tfpr, so specificity CI is the reversed
                    # complement of the FPR interval to keep lower < upper.
                    fpr_ci_logit <- get_logit_ci("tfpr.(Intercept)")
                    if (!any(is.na(fpr_ci_logit))) {
                        pooled_spec_ci <- rev(1 - stats::plogis(fpr_ci_logit))
                    }
                }, error = function(e) {
                    # Silently fall back to NA CIs if extraction fails
                })
            }

            plot_state <- list(
                data = meta_data,
                pooled_sens = if (!is.null(pooled_sens)) as.numeric(pooled_sens) else NA_real_,
                pooled_spec = if (!is.null(pooled_spec)) as.numeric(pooled_spec) else NA_real_,
                pooled_sens_ci = pooled_sens_ci,
                pooled_spec_ci = pooled_spec_ci
            )

            image$setState(plot_state)
        },
        
        .forestplot = function(image, ggtheme, theme, ...) {

            state <- image$state

            if (is.null(state))
                return(FALSE)

            # Unpack state. New state is a list carrying the study data plus the
            # serialized pooled point and CIs. Legacy states (older saved .omv
            # files) stored only the data frame - fall back to private fields,
            # which are populated whenever .run() has just executed.
            pooled_sens <- NULL
            pooled_spec <- NULL
            pooled_sens_ci <- c(NA_real_, NA_real_)
            pooled_spec_ci <- c(NA_real_, NA_real_)

            if (is.data.frame(state)) {
                meta_data <- state
                pooled_sens <- private$.pooled_sensitivity
                pooled_spec <- private$.pooled_specificity
            } else if (is.list(state)) {
                meta_data <- state$data
                pooled_sens <- state$pooled_sens
                pooled_spec <- state$pooled_spec
                if (!is.null(state$pooled_sens_ci)) pooled_sens_ci <- state$pooled_sens_ci
                if (!is.null(state$pooled_spec_ci)) pooled_spec_ci <- state$pooled_spec_ci
            } else {
                return(FALSE)
            }

            # Validate meta_data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Ensure meta_data is a proper data frame
                meta_data <- as.data.frame(meta_data)
                
                # Calculate sens and spec with CIs
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
                
                conf_level <- self$options$confidence_level
                if (is.null(conf_level) || length(conf_level) == 0 || !is.finite(conf_level)) {
                    conf_level <- 95
                }
                conf_level <- min(max(conf_level, 50), 99) / 100
                alpha <- 1 - conf_level
                z_crit <- stats::qnorm(1 - alpha / 2)

                # Wilson score interval (proper CI for proportions, not Wald)
                # More accurate for extreme proportions near 0 or 1
                wilson_ci <- function(x, n, z) {
                    if (n == 0 || !is.finite(x) || !is.finite(n)) {
                        return(c(0, 1))
                    }
                    p <- x / n
                    denominator <- 1 + z^2 / n
                    center <- (p + z^2 / (2 * n)) / denominator
                    margin <- z * sqrt((p * (1 - p) / n + z^2 / (4 * n^2))) / denominator

                    lower <- pmax(0, center - margin)
                    upper <- pmin(1, center + margin)
                    c(lower, upper)
                }

                # Calculate Wilson CIs for sensitivity
                meta_data$sens_ci_lower <- NA_real_
                meta_data$sens_ci_upper <- NA_real_
                for (i in seq_len(nrow(meta_data))) {
                    ci <- wilson_ci(meta_data$tp[i], meta_data$tp[i] + meta_data$fn[i], z_crit)
                    meta_data$sens_ci_lower[i] <- ci[1]
                    meta_data$sens_ci_upper[i] <- ci[2]
                }

                # Calculate Wilson CIs for specificity
                meta_data$spec_ci_lower <- NA_real_
                meta_data$spec_ci_upper <- NA_real_
                for (i in seq_len(nrow(meta_data))) {
                    ci <- wilson_ci(meta_data$tn[i], meta_data$tn[i] + meta_data$fp[i], z_crit)
                    meta_data$spec_ci_lower[i] <- ci[1]
                    meta_data$spec_ci_upper[i] <- ci[2]
                }

                # Reshape data to long format for faceting
                sens_data <- meta_data[, c("study", "sens", "sens_ci_lower", "sens_ci_upper")]
                sens_data$metric <- "Sensitivity"
                colnames(sens_data) <- c("study", "estimate", "ci_lower", "ci_upper", "metric")

                spec_data <- meta_data[, c("study", "spec", "spec_ci_lower", "spec_ci_upper")]
                spec_data$metric <- "Specificity"
                colnames(spec_data) <- c("study", "estimate", "ci_lower", "ci_upper", "metric")

                plot_data <- rbind(sens_data, spec_data)

                # Get color palette for accessibility
                colors <- private$.getColorPalette()

                # Add pooled estimates as diamond (standard meta-analysis
                # convention). Pooled point + CIs come from the serialized state
                # (see .populateForestPlot), so they persist across .omv reloads.
                pooled_data <- NULL
                if (!is.null(pooled_sens) && !is.null(pooled_spec) &&
                    is.finite(pooled_sens) && is.finite(pooled_spec)) {

                    # Create pooled estimate rows
                    pooled_sens_row <- data.frame(
                        study = "POOLED ESTIMATE",
                        estimate = pooled_sens,
                        ci_lower = pooled_sens_ci[1],
                        ci_upper = pooled_sens_ci[2],
                        metric = "Sensitivity",
                        stringsAsFactors = FALSE
                    )

                    pooled_spec_row <- data.frame(
                        study = "POOLED ESTIMATE",
                        estimate = pooled_spec,
                        ci_lower = pooled_spec_ci[1],
                        ci_upper = pooled_spec_ci[2],
                        metric = "Specificity",
                        stringsAsFactors = FALSE
                    )

                    pooled_data <- rbind(pooled_sens_row, pooled_spec_row)
                }

                # Create forest plot
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = estimate, y = reorder(study, estimate))) +
                    ggplot2::geom_point(size = 3, color = colors$primary) +
                    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                                          height = 0.2, color = colors$primary) +
                    ggplot2::facet_wrap(~ metric) +
                    ggplot2::labs(
                        title = "Forest Plot: Sensitivity and Specificity by Study",
                        x = "Estimate",
                        y = "Study"
                    ) +
                    ggplot2::xlim(0, 1) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10),
                        panel.spacing = ggplot2::unit(2, "lines")
                    )

                # Add pooled estimate diamond (standard meta-analysis convention)
                if (!is.null(pooled_data)) {
                    # Create diamond shape data for each metric
                    # Draw the pooled confidence interval as a horizontal bar,
                    # matching how the individual studies are drawn.
                    #
                    # This was a geom_polygon "diamond" whose four vertices were
                    # all given the SAME discrete y ("POOLED ESTIMATE"); the
                    # y_numeric half-height column was computed but never mapped
                    # in aes(). The polygon therefore had zero area and the pooled
                    # estimate appeared with no visible confidence interval at all
                    # - the one value on the plot a reader most needs the
                    # uncertainty for.
                    p <- p +
                        ggplot2::geom_errorbarh(
                            data = pooled_data,
                            ggplot2::aes(xmin = ci_lower, xmax = ci_upper, y = "POOLED ESTIMATE"),
                            height = 0.35,
                            linewidth = 1.1,
                            color = colors$secondary,
                            inherit.aes = FALSE
                        ) +
                        ggplot2::geom_point(
                            data = pooled_data,
                            ggplot2::aes(x = estimate, y = "POOLED ESTIMATE"),
                            size = 4,
                            color = colors$secondary,
                            shape = 18,  # Diamond shape
                            inherit.aes = FALSE
                        )
                }
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateSROCPlot = function(meta_data) {

            image <- self$results$srocplot

            # Check if bivariate model is available. The SROC pooled summary
            # point is derived from the bivariate (Reitsma) model, so without it
            # the plot would render blank with no explanation. Carry a message in
            # the state so .srocplot can draw an informative panel instead.
            if (is.null(private$.biv_model)) {
                image$setState(list(
                    message = paste(
                        "The Summary ROC plot requires the bivariate",
                        "random-effects (Reitsma) model. Enable",
                        "'Bivariate Analysis' to display the pooled summary",
                        "point and study estimates."
                    )
                ))
                return()
            }

            # CRITICAL FIX: Extract only serializable data from model
            # Do NOT store the model object itself (contains non-serializable functions)
            biv_model <- private$.biv_model
            summary_results <- summary(biv_model)
            coefficients <- summary_results$coefficients

            # Helper to safely get coefficient by name
            get_coef <- function(target) {
                if (is.null(rownames(coefficients))) return(NULL)
                idx <- which(rownames(coefficients) == target)
                if (length(idx) == 0) idx <- which(tolower(rownames(coefficients)) == tolower(target))
                if (length(idx) > 0) coefficients[idx[1], 1] else NULL
            }

            sum_sens <- get_coef("sensitivity")
            sum_fpr <- get_coef("false pos. rate")

            # Fallback to intercept transformation if named rows missing
            if (is.null(sum_sens)) {
                tsens <- get_coef("tsens.(Intercept)")
                if (!is.null(tsens)) sum_sens <- stats::plogis(tsens)
            }

            if (is.null(sum_fpr)) {
                tfpr <- get_coef("tfpr.(Intercept)")
                if (!is.null(tfpr)) sum_fpr <- stats::plogis(tfpr)
            }

            # Store only serializable data (no model object!)
            # SERIALIZATION FIX: Ensure meta_data is a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }

            # Pre-compute the SROC regression curve and the confidence region
            # (ellipse) here, where the Reitsma model object is available, and
            # store only the resulting (fpr, sens) coordinates so the plot state
            # stays serializable. The ellipse is the confidence region for the
            # pooled (logit-sensitivity, logit-fpr) mean derived from the bivariate
            # vcov, back-transformed to ROC space.
            sroc_curve <- NULL
            conf_ellipse <- NULL
            pred_ellipse <- NULL
            tryCatch({
                crv <- as.data.frame(mada::sroc(biv_model))
                if (ncol(crv) >= 2) {
                    names(crv)[1:2] <- c("fpr", "sens")
                    crv <- crv[stats::complete.cases(crv[, c("fpr", "sens")]), c("fpr", "sens"), drop = FALSE]
                    if (nrow(crv) > 1) sroc_curve <- crv
                }

                mu  <- as.numeric(stats::coef(biv_model))   # (tsens, tfpr), logit scale
                Sig <- stats::vcov(biv_model)
                if (length(mu) >= 2 && all(is.finite(mu[1:2])) &&
                    is.matrix(Sig) && all(dim(Sig) >= 2) && all(is.finite(Sig[1:2, 1:2]))) {
                    theta <- seq(0, 2 * pi, length.out = 200)
                    L     <- t(chol(Sig[1:2, 1:2]))
                    # Follow the user's confidence_level. This was hard-coded at
                    # 0.95, so setting 99% gave 99% CIs in every table beside a
                    # 95% ellipse on the SROC plot, with nothing saying so.
                    ell_level <- (self$options$confidence_level %||% 95) / 100
                    ell_level <- max(min(ell_level, 0.999), 0.5)
                    rad   <- sqrt(stats::qchisq(ell_level, df = 2))
                    pts   <- L %*% (rad * rbind(cos(theta), sin(theta))) + mu[1:2]
                    conf_ellipse <- data.frame(
                        fpr  = stats::plogis(pts[2, ]),
                        sens = stats::plogis(pts[1, ])
                    )

                    # PREDICTION region: where the accuracy of a FUTURE study is
                    # expected to lie. Built from vcov + Psi (uncertainty in the
                    # pooled mean PLUS between-study heterogeneity), which is
                    # exactly what mada's own plot.reitsma(predict = TRUE) does:
                    #   Sigma <- x$Psi + vcov(x)
                    # The confidence region above describes only the precision of
                    # the summary point and is always the smaller of the two; with
                    # substantial heterogeneity they differ enormously, and showing
                    # only the confidence region invites reading a tight ellipse as
                    # "this assay performs consistently".
                    Psi <- biv_model$Psi
                    if (is.matrix(Psi) && all(dim(Psi) >= 2) && all(is.finite(Psi[1:2, 1:2]))) {
                        Sig_pred <- Sig[1:2, 1:2] + Psi[1:2, 1:2]
                        Lp <- tryCatch(t(chol(Sig_pred)), error = function(e) NULL)
                        if (!is.null(Lp)) {
                            pts_p <- Lp %*% (rad * rbind(cos(theta), sin(theta))) + mu[1:2]
                            pred_ellipse <- data.frame(
                                fpr  = stats::plogis(pts_p[2, ]),
                                sens = stats::plogis(pts_p[1, ])
                            )
                        }
                    }
                }
            }, error = function(e) NULL)

            plot_state <- list(
                data = meta_data,
                pooled_sens = as.numeric(sum_sens),
                pooled_fpr = as.numeric(sum_fpr),
                sroc_curve = sroc_curve,
                conf_ellipse = conf_ellipse,
                pred_ellipse = pred_ellipse
            )

            image$setState(plot_state)
        },
        
        .srocplot = function(image, ggtheme, theme, ...) {

            # The SROC plot shows individual study points, the pooled summary
            # point, the SROC regression curve, and the confidence region
            # (ellipse). The curve and ellipse coordinates are pre-computed in
            # .populateSROCPlot from the Reitsma bivariate model and carried in
            # the (serializable) plot state.

            state <- image$state
            if (is.null(state)) {
                return(FALSE)
            }

            # Bivariate model unavailable: draw a centered explanatory panel
            # instead of a blank canvas.
            if (!is.null(state$message)) {
                if (requireNamespace("ggplot2", quietly = TRUE)) {
                    p <- ggplot2::ggplot() +
                        ggplot2::annotate(
                            "text", x = 0.5, y = 0.5,
                            label = state$message,
                            hjust = 0.5, vjust = 0.5, size = 5
                        ) +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggplot2::theme_void()
                    print(p)
                    return(TRUE)
                }
                return(FALSE)
            }

            # Extract pre-computed values from state (no model object)
            meta_data <- state$data
            sum_sens <- state$pooled_sens
            sum_fpr <- state$pooled_fpr

            # Validate data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            # Check if we have summary point values
            has_summary <- !is.null(sum_sens) && !is.null(sum_fpr)

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Individual study points
                meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
                meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
                meta_data$fpr <- 1 - meta_data$spec
                meta_data$n <- meta_data$tp + meta_data$fn + meta_data$fp + meta_data$tn

                # Get color palette for accessibility
                colors <- private$.getColorPalette()

                # Base plot: individual study points sized by sample size
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = fpr, y = sens)) +
                    ggplot2::geom_point(ggplot2::aes(size = n), color = colors$study_points, alpha = 0.7)

                # SROC regression curve (from the Reitsma bivariate model)
                if (!is.null(state$sroc_curve) && is.data.frame(state$sroc_curve) &&
                    nrow(state$sroc_curve) > 1) {
                    p <- p + ggplot2::geom_path(
                        data = state$sroc_curve,
                        ggplot2::aes(x = fpr, y = sens),
                        color = colors$primary, linewidth = 0.8, inherit.aes = FALSE
                    )
                }

                # Confidence region (ellipse) around the pooled estimate
                if (!is.null(state$conf_ellipse) && is.data.frame(state$conf_ellipse) &&
                    nrow(state$conf_ellipse) > 2) {
                    p <- p + ggplot2::geom_path(
                        data = state$conf_ellipse,
                        ggplot2::aes(x = fpr, y = sens),
                        color = colors$primary, linewidth = 0.6, linetype = "dashed",
                        inherit.aes = FALSE
                    )
                }

                # Prediction region (dotted) - drawn before the summary point
                if (!is.null(state$pred_ellipse) && is.data.frame(state$pred_ellipse) &&
                    nrow(state$pred_ellipse) > 2) {
                    p <- p + ggplot2::geom_path(
                        data = state$pred_ellipse,
                        ggplot2::aes(x = fpr, y = sens),
                        color = colors$primary, linewidth = 0.5, linetype = "dotted",
                        inherit.aes = FALSE
                    )
                }

                # Pooled summary point on top
                if (has_summary) {
                    p <- p + ggplot2::geom_point(
                        data = data.frame(fpr = sum_fpr, sens = sum_sens),
                        ggplot2::aes(x = fpr, y = sens),
                        color = colors$primary, size = 5, shape = 17, inherit.aes = FALSE
                    )
                }

                have_curve   <- !is.null(state$sroc_curve)
                have_ellipse <- !is.null(state$conf_ellipse)
                have_pred    <- !is.null(state$pred_ellipse)
                # Name the actual level - this said "95%" unconditionally, which
                # became wrong as soon as the ellipse started honouring
                # confidence_level. It is a CONFIDENCE region for the summary
                # point, not a prediction region for a future study; say which.
                ell_pct <- self$options$confidence_level %||% 95
                subtitle_txt <- paste0(
                    "Studies (circles), pooled estimate (triangle)",
                    if (have_curve) ", SROC curve" else "",
                    if (have_ellipse) paste0(", ", ell_pct, "% confidence region (dashed)") else "",
                    if (have_pred) paste0(", ", ell_pct, "% prediction region (dotted)") else ""
                )

                p <- p +
                    ggplot2::scale_x_continuous(limits = c(0, 1), name = "False Positive Rate (1 - Specificity)") +
                    ggplot2::scale_y_continuous(limits = c(0, 1), name = "Sensitivity") +
                    ggplot2::labs(
                        title = "Summary ROC Plot",
                        subtitle = subtitle_txt,
                        size = "Sample Size"
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )

                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        .populateFunnelPlot = function(meta_data) {

            image <- self$results$funnelplot
            # SERIALIZATION FIX: Ensure it's a plain data frame
            if (!is.null(meta_data) && is.data.frame(meta_data)) {
                meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)
                # Remove any attributes that might contain functions
                attributes(meta_data) <- attributes(meta_data)[c("names", "row.names", "class")]
            }
            image$setState(meta_data)
        },
        
        .funnelplot = function(image, ggtheme, theme, ...) {

            meta_data <- image$state

            # Validate meta_data
            if (is.null(meta_data) || !is.data.frame(meta_data) || nrow(meta_data) == 0) {
                return(FALSE)
            }

            if (requireNamespace("ggplot2", quietly = TRUE)) {

                # Ensure meta_data is a proper data frame
                meta_data <- as.data.frame(meta_data)

                # Deeks' funnel plot: log DOR against 1/sqrt(ESS).
                #
                # This previously plotted precision = 1/SE(log DOR) on the y axis,
                # the conventional Egger-style funnel. Deeks, Macaskill & Irwig
                # (2005) showed that is misleading for diagnostic accuracy data
                # precisely because log DOR and its standard error are
                # intrinsically correlated, which induces asymmetry even with no
                # publication bias - that is why the accompanying test regresses
                # on 1/sqrt(ESS). Plot and test must use the same x/y, otherwise a
                # visibly symmetric funnel sits beside a significant asymmetry
                # p-value (or the reverse).
                meta_data$log_dor <- log((meta_data$tp * meta_data$tn) / (meta_data$fp * meta_data$fn))
                n_diseased <- meta_data$tp + meta_data$fn
                n_healthy  <- meta_data$fp + meta_data$tn
                meta_data$ess <- 4 * n_diseased * n_healthy / (n_diseased + n_healthy)
                meta_data$inv_root_ess <- 1 / sqrt(meta_data$ess)

                # Get color palette for accessibility
                colors <- private$.getColorPalette()

                # Create funnel plot
                p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = log_dor, y = inv_root_ess)) +
                    ggplot2::geom_point(size = 3, alpha = 0.7, color = colors$secondary) +
                    ggplot2::scale_y_reverse() +
                    ggplot2::labs(
                        title = "Deeks' Funnel Plot: Publication Bias Assessment",
                        x = "Log Diagnostic Odds Ratio",
                        y = expression(1/sqrt("effective sample size"))
                    ) +
                    ggtheme +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 14, face = "bold"),
                        axis.title = ggplot2::element_text(size = 12),
                        axis.text = ggplot2::element_text(size = 10)
                    )
                
                print(p)
                TRUE
            } else {
                FALSE
            }
        },
        
        # TODO (UX): `self$results$instructions` is overloaded as: (a) onboarding
        # message when no data, (b) data-validation error sink (lines 151, 186),
        # (c) meta-regression-missing-covariate notice (.appendInstructionMessage),
        # (d) SROC unavailable / sparse-data notices. Mixing onboarding with
        # error states makes the UI confusing - first-time users see error
        # styling when they have not yet selected variables. Splitting into a
        # dedicated `notices` Html output (notice-pattern from waterfall.b.R)
        # would clarify the UX and align with the notices-migration TODO at
        # the top of `.run()`.
        .populateInstructions = function() {
            
            html <- "
            <h2>Diagnostic Test Meta-Analysis for Pathology</h2>
            
            <h3>Purpose</h3>
            <p>This module performs comprehensive meta-analysis of diagnostic test accuracy studies, specifically designed for pathology research including AI algorithm validation and biomarker diagnostic accuracy synthesis.</p>
            
            <h3>Required Data Structure</h3>
            <p><strong>Essential Variables (Required):</strong></p>
            <ul>
                <li><strong>Study identifier:</strong> Unique name or ID for each study (e.g., 'Smith_2020', 'Study_1')</li>
                <li><strong>True positives (TP):</strong> Number correctly identified as positive</li>
                <li><strong>False positives (FP):</strong> Number incorrectly identified as positive</li>
                <li><strong>False negatives (FN):</strong> Number incorrectly identified as negative</li>
                <li><strong>True negatives (TN):</strong> Number correctly identified as negative</li>
            </ul>
            
            <p><strong>Optional Variables for Meta-Regression:</strong></p>
            <ul>
                <li><strong>Patient population:</strong> Disease stage, demographics (e.g., 'early_stage', 'advanced', 'mixed')</li>
                <li><strong>Technical method:</strong> Staining protocol (e.g., 'automated', 'manual')</li>
                <li><strong>Geographic region:</strong> Study location for population analysis</li>
                <li><strong>Publication year:</strong> For temporal trend investigation</li>
            </ul>
            
            <h3>Data Preparation Checklist</h3>
            <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #007bff; margin: 10px 0; color: inherit;'>
                <p><strong>Before running analysis, verify:</strong></p>
                <ul>
                    <li> No missing values in TP, FP, FN, TN columns</li>
                    <li> All values are non-negative integers</li>
                    <li> At least 2 studies with complete data</li>
                    <li> Study identifiers are unique</li>
                    <li> Sample sizes are realistic (TP+FP+FN+TN = total cases per study)</li>
                </ul>
            </div>
            
            <h3>Example Data Format</h3>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: rgba(33, 33, 33, 0.06); color: inherit;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>study_name</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>true_positives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>false_positives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>false_negatives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>true_negatives</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>population</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Smith_2020</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>47</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>101</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>9</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>738</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>mixed</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Johnson_2021</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>126</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>272</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>51</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>1543</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>early_stage</td>
                </tr>
            </table>
            
            <h3>Analysis Methods</h3>
            <ul>
                <li><strong>Bivariate Random-Effects Model (Recommended):</strong> Jointly analyzes sensitivity and specificity accounting for correlation</li>
                <li><strong>Proportional-Hazards SROC Analysis:</strong> Holling model estimated by adjusted profile maximum likelihood</li>
                <li><strong>Meta-Regression:</strong> Investigates sources of heterogeneity using study-level covariates</li>
                <li><strong>Publication Bias Assessment:</strong> Deeks' funnel plot asymmetry test</li>
            </ul>

            <h3>Statistical Method Selection Guide</h3>
            <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-left: 4px solid #28a745; margin: 10px 0; color: inherit;'>
                <p><strong>Choose the appropriate estimation method for your meta-analysis:</strong></p>
                <ul>
                    <li><strong>REML (Recommended):</strong> Default choice for most diagnostic meta-analyses. Most robust for random effects modeling with good performance across different scenarios.</li>
                    <li><strong>Maximum Likelihood:</strong> Alternative estimation approach when maximum likelihood estimation is specifically preferred or required by study protocol.</li>
                    <li><strong>Fixed Effects:</strong> Use when between-study heterogeneity is minimal or you want to assume all studies estimate the same underlying effect size.</li>
                    <li><strong>Method of Moments:</strong> Classical moment-based estimation method, useful for comparison with older meta-analyses or when computational resources are limited.</li>
                    <li><strong>Variance Components:</strong> Specialized approach for variance component estimation, typically used in advanced methodological research.</li>
                    <li><strong>DerSimonian-Laird:</strong> Popular classical method familiar to many researchers (automatically optimized to use REML for better performance).</li>
                </ul>
                <p><strong> Recommendation:</strong> Start with REML unless you have specific methodological requirements. It provides the best balance of statistical properties and computational stability for diagnostic test meta-analysis.</p>
            </div>
            
            <h3>Clinical Applications</h3>
            <ul>
                <li>IHC marker validation across multiple pathology studies</li>
                <li>AI algorithm performance meta-analysis for clinical implementation</li>
                <li>Biomarker diagnostic accuracy synthesis for guideline development</li>
                <li>Cross-population comparison of diagnostic test performance</li>
                <li>Assessment of test performance heterogeneity and variation sources</li>
            </ul>
            "
            
            self$results$instructions$setContent(html)
        },
        
        .populateIndividualStudies = function(meta_data) {

            table <- self$results$individualstudies

            table$deleteRows()

            # Calculate sensitivity and specificity for each study
            meta_data$sens <- meta_data$tp / (meta_data$tp + meta_data$fn)
            meta_data$spec <- meta_data$tn / (meta_data$tn + meta_data$fp)
            meta_data$sample_size <- meta_data$tp + meta_data$fp + meta_data$fn + meta_data$tn

            for (i in seq_len(nrow(meta_data))) {
                table$addRow(rowKey = i, values = list(
                    study = as.character(meta_data$study[i]),
                    # Percent, matching the pooled table. These were proportions
                    # (0.82) while the bivariate table held percentages (81.59),
                    # so the same quantity appeared on two scales on one screen.
                    sensitivity = meta_data$sens[i] * 100,
                    specificity = meta_data$spec[i] * 100,
                    tp = meta_data$tp[i],
                    fp = meta_data$fp[i],
                    fn = meta_data$fn[i],
                    tn = meta_data$tn[i],
                    sample_size = meta_data$sample_size[i]
                ))
            }
        },

        .populateInterpretation = function() {
            
            html <- "
            <h2>Clinical Interpretation Guidelines</h2>
            
            <h3> Primary Results Interpretation</h3>
            
            <h4>Pooled Sensitivity and Specificity</h4>
            <ul>
                <li><strong>Pooled Sensitivity:</strong> Proportion of diseased cases correctly identified
                    <ul>
                        <li>Excellent sensitivity ([[GE]]90%): few diseased cases missed</li>
                        <li>Good sensitivity (80-89%): acceptable miss rate for screening</li>
                        <li>Moderate sensitivity (70-79%): appreciable number of cases missed</li>
                        <li>Limited sensitivity (&lt;70%): many cases missed</li>
                    </ul>
                </li>
                <li><strong>Pooled Specificity:</strong> Proportion of non-diseased cases correctly identified
                    <ul>
                        <li>Excellent specificity ([[GE]]90%): few false alarms</li>
                        <li>Good specificity (80-89%): some false positives</li>
                        <li>Moderate specificity (70-79%): appreciable false-positive rate</li>
                        <li>Limited specificity (&lt;70%): many false alarms</li>
                    </ul>
                </li>
            </ul>
            
            <h4>Likelihood Ratios for Clinical Decision-Making</h4>
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: rgba(33, 33, 33, 0.06); color: inherit;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Likelihood Ratio</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Value Range</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Clinical Interpretation</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>>10</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Strong evidence FOR disease when test positive</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>5-10</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate evidence for disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Positive LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>2-5</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Weak evidence for disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'><0.1</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Strong evidence AGAINST disease when test negative</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.1-0.2</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Moderate evidence against disease</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Negative LR</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>0.2-0.5</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Weak evidence against disease</td>
                </tr>
            </table>
            
            <h4>Diagnostic Odds Ratio (DOR)</h4>
            <ul>
                <li><strong>DOR > 25:</strong> Excellent overall discriminative ability</li>
                <li><strong>DOR 10-25:</strong> Good discriminative ability</li>
                <li><strong>DOR 5-10:</strong> Moderate discriminative ability</li>
                <li><strong>DOR < 5:</strong> Limited discriminative ability</li>
            </ul>
            
            <h3> Heterogeneity Assessment</h3>
            
            <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0; color: inherit;'>
                <h4>I[[SUP2]] Statistic Interpretation:</h4>
                <p><em>The bands below are Higgins' conventional cut-points from intervention
                meta-analysis. They are not established thresholds for diagnostic accuracy, where a
                high I[[SUP2]] commonly reflects studies using different positivity thresholds - the
                situation the SROC curve exists to model - rather than a reason to abandon the
                analysis. Treat them as rough orientation, and prefer the prediction region.</em></p>
                <ul>
                    <li><strong>I[[SUP2]] < 25%:</strong> Low heterogeneity - results can be reliably pooled</li>
                    <li><strong>I[[SUP2]] 25-50%:</strong> Moderate heterogeneity - investigate potential sources</li>
                    <li><strong>I[[SUP2]] 50-75%:</strong> Substantial heterogeneity - pooling questionable</li>
                    <li><strong>I[[SUP2]] &gt; 75%:</strong> Considerable heterogeneity - a single pooled point is unlikely to describe the evidence; investigate thresholds, spectrum and reference standards, and report the SROC curve and prediction region</li>
                </ul>
            </div>
            
            <h4>Common Sources of Heterogeneity:</h4>
            <ul>
                <li><strong>Patient Population:</strong> Disease stage, severity, demographics</li>
                <li><strong>Technical Factors:</strong> Staining protocols, antibody sources, automation</li>
                <li><strong>Methodological:</strong> Reference standards, blinding, cut-off thresholds</li>
                <li><strong>Geographic/Temporal:</strong> Population differences, technology evolution</li>
            </ul>
            
            <h3> Publication Bias Assessment</h3>
            
            <h4>Deeks' Funnel Plot Test:</h4>
            <ul>
                <li><strong>p [[GE]] 0.05:</strong> No significant asymmetry - low risk of publication bias</li>
                <li><strong>p < 0.05:</strong> Significant asymmetry - potential publication bias detected</li>
            </ul>
            
            <div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-left: 4px solid #dc3545; margin: 10px 0; color: inherit;'>
                <p><strong> When Publication Bias is Detected:</strong></p>
                <ul>
                    <li>Pooled estimates may be overoptimistic</li>
                    <li>Search for unpublished studies or negative results</li>
                    <li>Consider contacting study authors for additional data</li>
                    <li>Report limitations and interpret results cautiously</li>
                </ul>
            </div>
            
            <h3> Clinical Application Guidance</h3>
            
            <h4>IHC Marker Validation:</h4>
            <ul>
                <li><strong>Screening Applications:</strong> Prioritize high sensitivity ([[GE]]90%)</li>
                <li><strong>Confirmatory Testing:</strong> Prioritize high specificity ([[GE]]90%)</li>
                <li><strong>Balanced Performance:</strong> Consider clinical costs of false positives vs false negatives</li>
            </ul>
            
            <h4>AI Algorithm Implementation:</h4>
            <ul>
                <li><strong>Consistent Performance:</strong> Low heterogeneity supports broad implementation</li>
                <li><strong>Variable Performance:</strong> High heterogeneity suggests population-specific validation needed</li>
                <li><strong>External Validation:</strong> Meta-analysis provides evidence for regulatory approval</li>
            </ul>
            
            <h4>Predictive Values in Clinical Practice:</h4>
            <p><strong>Important:</strong> Sensitivity and specificity are test characteristics, but clinicians need predictive values that depend on disease prevalence in their population.</p>
            
            <table style='border-collapse: collapse; width: 100%; margin: 10px 0;'>
                <tr style='background-color: rgba(33, 33, 33, 0.06); color: inherit;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Disease Prevalence</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>PPV (Sen=90%, Spe=80%)</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>NPV (Sen=90%, Spe=80%)</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>5%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>19%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>99%</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>20%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>53%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>97%</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>50%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>82%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>89%</td>
                </tr>
            </table>
            
            <h3> Reporting Recommendations</h3>
            
            <p>When reporting your meta-analysis results, include:</p>
            <ul>
                <li> <strong>Study Selection:</strong> Number of studies included and excluded</li>
                <li> <strong>Pooled Estimates:</strong> Sensitivity and specificity with 95% confidence intervals</li>
                <li> <strong>Likelihood Ratios:</strong> For clinical decision-making context</li>
                <li> <strong>Heterogeneity:</strong> I[[SUP2]] values and potential sources investigated</li>
                <li> <strong>Publication Bias:</strong> Deeks' test results and visual assessment</li>
                <li> <strong>Clinical Implications:</strong> Population-specific predictive values</li>
                <li> <strong>Limitations:</strong> Study quality, missing data, generalizability</li>
            </ul>
            
            <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-left: 4px solid #17a2b8; margin: 10px 0; color: inherit;'>
                <p><strong> Pro Tip:</strong> Always interpret meta-analysis results in the context of your specific clinical population and intended use. A test excellent for one application may be inappropriate for another.</p>
            </div>
            "
            
            self$results$interpretation$setContent(private$.renderSymbols(html))
        },

        .appendInstructionMessage = function(message) {

            if (is.null(message) || !nzchar(message)) {
                return()
            }

            current <- self$results$instructions$content
            if (!is.null(current) && nzchar(current)) {
                self$results$instructions$setContent(paste(current, message))
            } else {
                self$results$instructions$setContent(message)
            }
            # .run() hides this element as soon as all five variables are chosen,
            # and every warning in the analysis is routed here. Without this the
            # messages were written into a hidden panel and never seen - and a
            # missing cell produced a completely blank analysis with no
            # explanation anywhere.
            self$results$instructions$setVisible(TRUE)
        },

        # Enhanced data validation with user-friendly warnings
        .validateStudyData = function(meta_data, original_n) {

            # Check minimum study requirement first (critical error)
            if (nrow(meta_data) < 3) {
                # SERIALIZATION FIX: Use table note instead of inserting Notice
                error_msg <- sprintf(
                    'Meta-analysis requires at least 3 studies with complete data. Current studies: %d. Please add more studies or use individual study analysis instead.',
                    nrow(meta_data)
                )
                self$results$bivariateresults$setNote("insufficient_data", error_msg)
                self$results$hsrocresults$setNote("insufficient_data", error_msg)
                self$results$heterogeneity$setNote("insufficient_data", error_msg)
                return(FALSE)
            }

            # Check for zero cells (store flag for continuity correction)
            zero_cells <- any(meta_data$tp == 0 | meta_data$fp == 0 |
                            meta_data$fn == 0 | meta_data$tn == 0)
            if (zero_cells) {
                private$.continuity_correction <- TRUE
            }

            return(TRUE)
        },

        # Generate natural language summary
        .generateSummary = function(meta_data) {

            # If pooled estimates are not available, provide basic summary
            if (is.null(private$.pooled_sensitivity) || is.null(private$.pooled_specificity)) {
                private$.generateBasicSummary(meta_data)
                return()
            }

            # Calculate confidence intervals (these should be set by bivariate analysis)
            sens_pct <- round(private$.pooled_sensitivity * 100, 1)
            spec_pct <- round(private$.pooled_specificity * 100, 1)

            # Calculate positive and negative likelihood ratios
            lr_pos <- NA_real_
            lr_neg <- NA_real_

            if (is.finite(private$.pooled_sensitivity) && is.finite(private$.pooled_specificity)) {
                denom_plr <- 1 - private$.pooled_specificity
                denom_nlr <- private$.pooled_specificity

                if (is.finite(denom_plr) && denom_plr > 0) {
                    lr_pos <- private$.pooled_sensitivity / denom_plr
                }

                if (is.finite(denom_nlr) && denom_nlr > 0) {
                    lr_neg <- (1 - private$.pooled_sensitivity) / denom_nlr
                }
            }

            inv_lr_neg <- if (is.finite(lr_neg) && lr_neg > 0) 1 / lr_neg else NA_real_

            plr_text <- if (is.finite(lr_pos)) {
                sprintf("<p><strong>Positive Likelihood Ratio:</strong> %.2f - A positive test is %.1fx more likely in disease than healthy</p>",
                        lr_pos, lr_pos)
            } else {
                private$.renderSymbols("<p><strong>Positive Likelihood Ratio:</strong> Not estimable with the current data (specificity [[APPROX]] 100% or model unstable).</p>")
            }

            nlr_text <- if (is.finite(lr_neg)) {
                if (is.finite(inv_lr_neg)) {
                    sprintf("<p><strong>Negative Likelihood Ratio:</strong> %.2f - A negative test is %.1fx more likely in healthy than disease</p>",
                            lr_neg, inv_lr_neg)
                } else {
                    private$.renderSymbols(sprintf(
                        "<p><strong>Negative Likelihood Ratio:</strong> %.2f - Interpretation unstable (sensitivity [[APPROX]] 100%%).</p>",
                        lr_neg
                    ))
                }
            } else {
                private$.renderSymbols("<p><strong>Negative Likelihood Ratio:</strong> Not estimable with the current data (sensitivity [[APPROX]] 100% or model unstable).</p>")
            }

            copy_text <- sprintf(
                "Meta-analysis of %d diagnostic accuracy studies shows pooled sensitivity of %.1f%% and specificity of %.1f%%.",
                private$.n_studies, sens_pct, spec_pct
            )

            if (is.finite(lr_pos) && is.finite(lr_neg)) {
                copy_text <- sprintf(
                    "%s Positive LR %.2f and negative LR %.2f.",
                    copy_text, lr_pos, lr_neg
                )
            }

            # Build zero-cell correction disclosure
            correction_disclosure <- ""
            if (!is.null(private$.correction_method_used) && private$.correction_method_used != "none") {
                n_corrected <- length(private$.corrected_study_names)
                if (n_corrected > 0) {
                    method_label <- switch(private$.correction_method_used,
                                         constant = "constant +0.5 to all cells",
                                         treatment_arm = "treatment-arm (+0.5 to zero cells only)",
                                         empirical = "empirical (1/N) correction",
                                         "unknown method")

                    # Escape user-supplied study names before HTML / note interpolation
                    safe_studies_note <- paste(
                        htmltools::htmlEscape(head(private$.corrected_study_names, 3)),
                        collapse = ", "
                    )
                    safe_studies_html <- paste(
                        htmltools::htmlEscape(head(private$.corrected_study_names, 5)),
                        collapse = ", "
                    )

                    # SERIALIZATION FIX: Use table note instead of inserting Notice
                    warning_msg <- sprintf(
                        'Zero-cell correction applied (%s) to %d of %d studies (%s). Results should be interpreted with caution as corrections can introduce bias, especially in large studies.',
                        method_label, n_corrected, private$.n_studies,
                        safe_studies_note
                    )
                    self$results$bivariateresults$setNote("zero_cell_warning", warning_msg)

                    correction_disclosure <- sprintf(
                        "<div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                            <h5> Zero-Cell Correction Applied</h5>
                            <p><strong>Method:</strong> %s</p>
                            <p><strong>Studies corrected:</strong> %d of %d (%s)</p>
                            <p><em>Note:</em> Zero-cell corrections can introduce bias, especially in large studies. Results should be interpreted with caution.</p>
                        </div>",
                        method_label,
                        n_corrected,
                        private$.n_studies,
                        safe_studies_html
                    )
                }
            }

            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: rgba(33, 149, 188, 0.1); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4>Meta-Analysis Summary</h4>
                <p><strong>Analysis Type:</strong> Diagnostic test accuracy meta-analysis of %d studies</p>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Pooled Test Performance</h5>
                    <p><strong>Sensitivity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 patients with disease</p>
                    <p><strong>Specificity:</strong> %.1f%% - The test correctly identifies %.0f out of 100 healthy individuals</p>
                </div>

                %s

                <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Clinical Decision Metrics</h5>
                    %s
                    %s
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Interpretation Guide</h5>
                    <p>%s</p>
                </div>

                <div style='margin-top: 15px;'>
                    <button onclick='navigator.clipboard.writeText(this.getAttribute(\"data-text\"))'
                            data-text='%s'
                            style='background-color: #007bff; color: #ffffff; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer;'>
                        Copy Summary to Clipboard
                    </button>
                </div>
            </div>
            ",
            private$.n_studies,
            sens_pct, sens_pct,
            spec_pct, spec_pct,
            correction_disclosure,
            plr_text,
            nlr_text,
            private$.getInterpretationText(sens_pct, spec_pct, lr_pos, lr_neg,
                                          private$.pooled_sens_ci, private$.pooled_spec_ci),
            copy_text
            )

            self$results$summary$setContent(summary_html)
        },

        # Generate basic summary when pooled estimates are not available
        .generateBasicSummary = function(meta_data) {

            if (is.null(meta_data) || nrow(meta_data) == 0) {
                return()
            }

            # Calculate individual study statistics
            meta_data$sensitivity <- meta_data$tp / (meta_data$tp + meta_data$fn)
            meta_data$specificity <- meta_data$tn / (meta_data$tn + meta_data$fp)
            meta_data$sample_size <- meta_data$tp + meta_data$fp + meta_data$fn + meta_data$tn

            # Calculate basic descriptive statistics
            n_studies <- nrow(meta_data)
            total_sample <- sum(meta_data$sample_size, na.rm = TRUE)

            sens_mean <- mean(meta_data$sensitivity, na.rm = TRUE) * 100
            sens_range <- range(meta_data$sensitivity, na.rm = TRUE) * 100
            spec_mean <- mean(meta_data$specificity, na.rm = TRUE) * 100
            spec_range <- range(meta_data$specificity, na.rm = TRUE) * 100

            # Determine why pooled estimates failed
            reason <- ""
            if (!isTRUE(self$options$bivariate_analysis)) {
                reason <- "Bivariate analysis was not enabled. Enable bivariate analysis for pooled estimates."
            } else if (n_studies < 3) {
                reason <- sprintf("Only %d studies available. At least 3 studies are required for meta-analysis.", n_studies)
            } else {
                reason <- "Bivariate meta-analysis encountered an error. Check individual study results and data quality."
            }

            summary_html <- sprintf("
            <div class='analysis-summary' style='background-color: rgba(33, 149, 188, 0.1); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4>Meta-Analysis Summary</h4>
                <p><strong>Analysis Status:</strong> %s</p>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>Study Overview</h5>
                    <p><strong>Number of Studies:</strong> %d</p>
                    <p><strong>Total Sample Size:</strong> %d participants</p>
                    <p><strong>Sample Size Range:</strong> %d - %d per study</p>
                </div>

                <div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Individual Study Performance (Descriptive)</h5>
                    <p><strong>Sensitivity:</strong> Mean %.1f%% (Range: %.1f%% - %.1f%%)</p>
                    <p><strong>Specificity:</strong> Mean %.1f%% (Range: %.1f%% - %.1f%%)</p>
                    <p><em>Note: These are simple averages, not meta-analytic pooled estimates.</em></p>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Recommendation</h5>
                    <p>%s</p>
                    <p>Individual study results are available in the table below for detailed examination.</p>
                </div>
            </div>
            ",
            reason,
            n_studies,
            total_sample,
            min(meta_data$sample_size, na.rm = TRUE),
            max(meta_data$sample_size, na.rm = TRUE),
            sens_mean, sens_range[1], sens_range[2],
            spec_mean, spec_range[1], spec_range[2],
            reason
            )

            self$results$summary$setContent(summary_html)
        },

        # Helper function for dynamic interpretation text with actual values
        .getInterpretationText = function(sens, spec, lr_pos, lr_neg,
                                         sens_ci = NULL, spec_ci = NULL) {
            # Classify against the standard bands, then check whether the
            # confidence interval actually supports the label. Every claim below
            # was previously made from the point estimate alone, so a pooled
            # sensitivity of 90.4% with a 95% CI of 71-97% was reported as
            # "excellent ... will detect 90 out of 100 patients", which the
            # interval does not support.
            band <- function(x) if (x >= 90) "excellent" else if (x >= 80) "good"
                                else if (x >= 70) "moderate" else "limited"

            sens_class <- band(sens)
            spec_class <- band(spec)

            # TRUE when the interval spans more than one performance band, i.e.
            # the data cannot distinguish "excellent" from something worse.
            ci_spans_bands <- function(ci) {
                if (is.null(ci) || length(ci) < 2 || any(!is.finite(ci))) return(FALSE)
                band(min(ci)) != band(max(ci))
            }
            sens_uncertain <- ci_spans_bands(sens_ci)
            spec_uncertain <- ci_spans_bands(spec_ci)

            ci_txt <- function(ci) {
                if (is.null(ci) || length(ci) < 2 || any(!is.finite(ci))) return("")
                sprintf(" (%.1f%%-%.1f%%)", min(ci), max(ci))
            }

            # Classify positive LR
            plr_class <- if (is.finite(lr_pos)) {
                if (lr_pos > 10) "strong"
                else if (lr_pos >= 5) "moderate"
                else if (lr_pos >= 2) "weak"
                else "minimal"
            } else {
                "not estimable"
            }

            # Classify negative LR
            nlr_class <- if (is.finite(lr_neg)) {
                if (lr_neg < 0.1) "strong"
                else if (lr_neg <= 0.2) "moderate"
                else if (lr_neg <= 0.5) "weak"
                else "minimal"
            } else {
                "not estimable"
            }

            # Build dynamic interpretation with actual values
            interpretation <- sprintf(
                "<strong>Your pooled sensitivity of %.1f%%%s</strong> is classified as <em>%s</em> for screening purposes. ",
                sens, ci_txt(sens_ci), sens_class
            )
            if (sens_uncertain) {
                interpretation <- paste0(interpretation,
                    "<strong>Note:</strong> the confidence interval spans more than one performance category, ",
                    "so this classification is not firmly established by the pooled data. ")
            }

            # Add sensitivity-specific guidance
            if (sens >= 90) {
                interpretation <- paste0(interpretation,
                    if (sens_uncertain) {
                        sprintf("On the pooled estimate this test would detect about %.0f of 100 patients with disease, but the interval%s admits materially worse performance. ",
                                sens, ci_txt(sens_ci))
                    } else {
                        sprintf("With %.1f%% sensitivity, this test will detect %.0f out of 100 patients with disease, missing only %.0f. ",
                                sens, sens, 100 - sens)
                    },
                    "<strong>Clinical implication:</strong> Excellent for ruling OUT disease when test is negative (SnNout principle). "
                )
            } else if (sens >= 80) {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% sensitivity, approximately %.0f out of 100 diseased patients will be correctly identified. ",
                            sens, sens),
                    "<strong>Clinical implication:</strong> Acceptable for screening, but negative results should be interpreted with caution. "
                )
            } else {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% sensitivity, up to %.0f out of 100 diseased patients may be missed. ",
                            sens, 100 - sens),
                    "<strong>Clinical implication:</strong> Limited screening utility - negative results do NOT effectively rule out disease. "
                )
            }

            # Add specificity interpretation
            interpretation <- paste0(interpretation,
                sprintf("<br><br><strong>Your pooled specificity of %.1f%%</strong> is classified as <em>%s</em> for confirmatory testing. ",
                        spec, spec_class)
            )

            if (spec >= 90) {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% specificity, only %.0f out of 100 healthy individuals will test positive (false alarms). ",
                            spec, 100 - spec),
                    "<strong>Clinical implication:</strong> Excellent for ruling IN disease when test is positive (SpPin principle). "
                )
            } else if (spec >= 80) {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% specificity, approximately %.0f out of 100 healthy individuals will be correctly classified. ",
                            spec, spec),
                    "<strong>Clinical implication:</strong> Acceptable for confirmation, but positive results may include false positives. "
                )
            } else {
                interpretation <- paste0(interpretation,
                    sprintf("With %.1f%% specificity, up to %.0f out of 100 healthy individuals may test positive. ",
                            spec, 100 - spec),
                    "<strong>Clinical implication:</strong> Limited confirmatory value - positive results do NOT strongly confirm disease. "
                )
            }

            # Add likelihood ratio interpretation if available
            if (plr_class != "not estimable") {
                interpretation <- paste0(interpretation,
                    sprintf("<br><br><strong>Your positive LR of %.2f</strong> provides <em>%s</em> evidence FOR disease when test is positive. ",
                            lr_pos, plr_class)
                )

                if (lr_pos > 10) {
                    interpretation <- paste0(interpretation,
                        sprintf("A positive result increases disease probability substantially (multiplies pre-test odds by %.1fx). ",
                                lr_pos)
                    )
                } else if (lr_pos >= 5) {
                    interpretation <- paste0(interpretation,
                        "A positive result moderately increases disease probability. "
                    )
                } else {
                    interpretation <- paste0(interpretation,
                        "A positive result provides only weak evidence - clinical context is essential. "
                    )
                }
            }

            if (nlr_class != "not estimable") {
                interpretation <- paste0(interpretation,
                    sprintf("<strong>Your negative LR of %.2f</strong> provides <em>%s</em> evidence AGAINST disease when test is negative. ",
                            lr_neg, nlr_class)
                )

                if (lr_neg < 0.1) {
                    interpretation <- paste0(interpretation,
                        sprintf("A negative result substantially decreases disease probability (divides pre-test odds by %.1fx). ",
                                1/lr_neg)
                    )
                } else if (lr_neg <= 0.2) {
                    interpretation <- paste0(interpretation,
                        "A negative result moderately decreases disease probability. "
                    )
                } else {
                    interpretation <- paste0(interpretation,
                        "A negative result provides only weak evidence - clinical context is essential. "
                    )
                }
            }

            # Overall recommendation
            if (sens >= 90 && spec >= 90) {
                interpretation <- paste0(interpretation,
                    "<br><br><strong>Overall Assessment:</strong> This test demonstrates <em>excellent</em> diagnostic accuracy suitable for both screening and confirmatory use in appropriate clinical populations."
                )
            } else if (sens >= 80 && spec >= 80) {
                interpretation <- paste0(interpretation,
                    "<br><br><strong>Overall Assessment:</strong> This test demonstrates <em>good</em> diagnostic accuracy and can be useful for clinical decision-making when combined with other clinical information."
                )
            } else if (sens >= 90 || spec >= 90) {
                interpretation <- paste0(interpretation,
                    "<br><br><strong>Overall Assessment:</strong> This test has <em>asymmetric</em> performance - excellent for one purpose (rule-in OR rule-out) but limited for the other. Use strategically based on clinical goals."
                )
            } else {
                interpretation <- paste0(interpretation,
                    "<br><br><strong>Overall Assessment:</strong> This test demonstrates <em>moderate</em> diagnostic accuracy. Consider using in combination with other tests or as part of a broader diagnostic algorithm rather than as a standalone test."
                )
            }

            return(interpretation)
        },

        # Populate About This Analysis panel
        .populateAboutPanel = function() {

            html <- "
            <div class='about-panel' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4> About Diagnostic Test Meta-Analysis</h4>

                <div style='margin: 15px 0;'>
                    <h5>What This Analysis Does</h5>
                    <p>Combines results from multiple diagnostic accuracy studies to estimate overall test performance through:</p>
                    <ul>
                        <li> <strong>Bivariate modeling</strong> - Jointly analyzes sensitivity and specificity</li>
                        <li> <strong>Proportional-hazards SROC modeling</strong> - Models the trade-off between sensitivity and false-positive rate</li>
                        <li> <strong>Heterogeneity assessment</strong> - Evaluates consistency across studies</li>
                        <li> <strong>Publication bias</strong> - Checks for selective reporting</li>
                    </ul>
                </div>

                <div style='margin: 15px 0; background-color: rgba(33, 152, 239, 0.13); padding: 15px; border-radius: 5px; border-left: 4px solid #2196F3; color: inherit;'>
                    <h5> Understanding Bivariate and Proportional-Hazards SROC Models</h5>
                    <p><strong>These models answer related questions using different parameterizations:</strong></p>

                    <p><strong>Bivariate Random-Effects Model (Recommended Primary Approach):</strong></p>
                    <ul>
                        <li> Provides <em>intuitive</em> pooled sensitivity and specificity estimates</li>
                        <li> Accounts for correlation between sensitivity and specificity</li>
                        <li> Directly interpretable for clinical decision-making</li>
                        <li> Preferred for meta-analyses with <em>homogeneous thresholds</em></li>
                        <li> <strong>Use this when:</strong> Studies use the same diagnostic threshold</li>
                    </ul>

                    <p><strong>Holling Proportional-Hazards SROC Model:</strong></p>
                    <ul>
                        <li> Relates sensitivity (<em>p</em>) and false-positive rate (<em>u</em>) through <em>u</em><sup>theta</sup> = <em>p</em></li>
                        <li> Reports theta as the diagnostic accuracy parameter</li>
                        <li> Reports tau<sup>2</sup> as between-study variation in diagnostic accuracy</li>
                        <li> Uses adjusted profile maximum likelihood and is suitable for smaller study sets</li>
                        <li> Is distinct from the Rutter-Gatsonis HSROC model</li>
                    </ul>

                    <p><strong> Clinical Insight:</strong> The plotted SROC curve is derived from the <em>bivariate model</em>. The proportional-hazards SROC table is a separate compact model of diagnostic accuracy and between-study variation; it should not be interpreted as a Rutter-Gatsonis HSROC threshold/accuracy table.</p>
                </div>

                <div style='margin: 15px 0;'>
                    <h5>When to Use This Analysis</h5>
                    <ul>
                        <li> Evaluating AI algorithms for pathology diagnosis</li>
                        <li> Assessing biomarker diagnostic accuracy</li>
                        <li> Comparing imaging modalities</li>
                        <li> Synthesizing evidence for clinical guidelines</li>
                    </ul>
                </div>

                <div style='margin: 15px 0; background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; color: inherit;'>
                    <h5> Key Requirements & Assumptions</h5>
                    <ul>
                        <li>Minimum 3 studies with 2[[TIMES]]2 diagnostic data</li>
                        <li>Studies should evaluate the same test and target condition</li>
                        <li>Reference standard should be consistent across studies</li>
                        <li>Patient spectrum should be clinically relevant</li>
                    </ul>
                </div>

                <div style='margin: 15px 0;'>
                    <h5> Quick Start Guide</h5>
                    <ol>
                        <li><strong>Step 1:</strong> Select your study identifier variable</li>
                        <li><strong>Step 2:</strong> Assign TP, FP, FN, TN count variables</li>
                        <li><strong>Step 3:</strong> Choose analysis options (bivariate recommended)</li>
                        <li><strong>Step 4:</strong> Review pooled estimates and heterogeneity</li>
                        <li><strong>Step 5:</strong> Interpret in your clinical context</li>
                    </ol>
                </div>

                <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-radius: 5px; margin: 15px 0; color: inherit;'>
                    <p><strong> Tip:</strong> Start with the bivariate model and forest plot to understand overall performance, then explore heterogeneity sources with meta-regression if needed.</p>
                </div>
            </div>
            "

            self$results$about$setContent(private$.renderSymbols(html))
        },

        # Optimized data preparation with caching
        .prepareAnalysisData = function(meta_data) {

            if (private$.data_cache_valid &&
                !is.null(private$.analysis_data) &&
                !is.null(private$.mada_data)) {
                return(list(
                    analysis_data = private$.analysis_data,
                    mada_data = private$.mada_data,
                    continuity_correction = private$.continuity_correction
                ))
            }

            if (is.null(meta_data) || nrow(meta_data) == 0) {
                private$.analysis_data <- meta_data
                private$.mada_data <- data.frame()
                private$.continuity_correction <- FALSE
                private$.data_cache_valid <- TRUE
                return(list(
                    analysis_data = private$.analysis_data,
                    mada_data = private$.mada_data,
                    continuity_correction = private$.continuity_correction
                ))
            }

            analysis_data <- meta_data
            numeric_cols <- c("tp", "fp", "fn", "tn")

            for (col in numeric_cols) {
                analysis_data[[col]] <- as.numeric(analysis_data[[col]])
            }

            # Apply zero-cell correction based on user choice
            correction_method <- self$options$zero_cell_correction %||% "none"
            correction_flags <- rep(FALSE, nrow(analysis_data))
            corrected_studies <- character(0)

            for (i in seq_len(nrow(analysis_data))) {
                row_counts <- as.numeric(analysis_data[i, numeric_cols])
                if (any(!is.finite(row_counts))) {
                    next
                }
                if (any(row_counts < 0)) {
                    next
                }

                has_zero <- any(row_counts == 0)

                if (has_zero && correction_method != "none") {
                    if (correction_method == "constant") {
                        # Global +0.5 to all cells (biased for large studies)
                        analysis_data[i, numeric_cols] <- row_counts + 0.5
                        correction_flags[i] <- TRUE
                        corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))

                    } else if (correction_method == "treatment_arm") {
                        # Add 0.5 only to zero cells (treatment-arm correction)
                        corrected_row <- row_counts
                        corrected_row[row_counts == 0] <- 0.5
                        analysis_data[i, numeric_cols] <- corrected_row
                        correction_flags[i] <- TRUE
                        corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))

                    } else if (correction_method == "empirical") {
                        # Empirical correction: use 1/N where N is total sample size
                        total_n <- sum(row_counts, na.rm = TRUE)
                        if (total_n > 0) {
                            correction <- 1 / total_n
                            analysis_data[i, numeric_cols] <- row_counts + correction
                            correction_flags[i] <- TRUE
                            corrected_studies <- c(corrected_studies, as.character(analysis_data[i, "study"]))
                        }
                    }
                }
            }

            # Store correction info for reporting
            private$.correction_method_used <- correction_method
            private$.corrected_study_names <- corrected_studies

            private$.analysis_data <- analysis_data
            private$.mada_data <- data.frame(
                TP = analysis_data$tp,
                FP = analysis_data$fp,
                FN = analysis_data$fn,
                TN = analysis_data$tn
            )
            private$.continuity_correction <- any(correction_flags)
            private$.data_cache_valid <- TRUE

            list(
                analysis_data = private$.analysis_data,
                mada_data = private$.mada_data,
                continuity_correction = private$.continuity_correction,
                corrected_rows = which(correction_flags)
            )
        },

        # Plot explanation functions
        .populateForestPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4> Forest Plot Interpretation Guide</h4>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>Forest Plot:</strong> Displays individual study results for sensitivity and specificity with confidence intervals. Each study is represented by a point (estimate) with horizontal lines (confidence intervals).</p>

                    <ul>
                        <li><strong>Left Panel (Sensitivity):</strong> Proportion of diseased cases correctly identified</li>
                        <li><strong>Right Panel (Specificity):</strong> Proportion of healthy cases correctly identified</li>
                        <li><strong>Horizontal Lines:</strong> 95% confidence intervals showing precision of estimates</li>
                        <li><strong>Point Position:</strong> Higher on Y-axis = higher study estimate</li>
                    </ul>
                </div>

                <div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Clinical Interpretation</h5>
                    <ul>
                        <li><strong>Consistent Results:</strong> Points clustered together = low heterogeneity</li>
                        <li><strong>Wide Spread:</strong> Points scattered = high heterogeneity (investigate sources)</li>
                        <li><strong>Narrow CIs:</strong> Large studies with precise estimates</li>
                        <li><strong>Wide CIs:</strong> Small studies with less precise estimates</li>
                    </ul>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Quick Assessment Tips</h5>
                    <ul>
                        <li>Look for outlier studies (points far from others)</li>
                        <li>Check if confidence intervals overlap substantially</li>
                        <li>Consider whether variation reflects true differences or chance</li>
                        <li>Use this plot to identify studies for sensitivity analysis</li>
                    </ul>
                </div>
            </div>
            "

            self$results$forestplot_explanation$setContent(html)
        },

        .populateSROCPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4> Summary ROC Plot Interpretation Guide</h4>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>SROC Plot:</strong> Summary Receiver Operating Characteristic curve showing the trade-off between sensitivity and specificity across all studies.</p>

                    <ul>
                        <li><strong>X-axis:</strong> False Positive Rate (1 - Specificity) - lower is better</li>
                        <li><strong>Y-axis:</strong> True Positive Rate (Sensitivity) - higher is better</li>
                        <li><strong>Individual Studies:</strong> Circles sized by sample size</li>
                        <li><strong>Pooled Estimate:</strong> Large triangle showing meta-analytic summary</li>
                        <li><strong>Confidence region (dashed):</strong> how precisely the POOLED point is estimated. It shrinks as more studies are added.</li>
                        <li><strong>Prediction region (dotted):</strong> where the accuracy of a FUTURE study in a new setting is expected to fall. It includes between-study heterogeneity and does NOT shrink with more studies.</li>
                    </ul>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Confidence region vs prediction region</h5>
                    <p>These answer different questions and are routinely confused. A tight
                    <strong>confidence</strong> region means the pooled estimate is well determined; it says
                    nothing about whether the assay will perform consistently. A wide
                    <strong>prediction</strong> region means that even though the average is known precisely,
                    the next study - or your laboratory - could see materially different sensitivity and
                    specificity. <strong>For deciding whether to adopt an assay, the prediction region is the
                    relevant one.</strong> When the two differ greatly, between-study heterogeneity dominates
                    and the pooled point alone should not drive the decision.</p>
                </div>

                <div style='background-color: rgba(33, 159, 33, 0.1); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5>Clinical Interpretation</h5>
                    <ul>
                        <li><strong>Upper Left Corner:</strong> Ideal performance (high sensitivity, low false positive rate)</li>
                        <li><strong>Diagonal Line:</strong> Represents random chance (no discriminative ability)</li>
                        <li><strong>Above Diagonal:</strong> Better than chance performance</li>
                        <li><strong>Point Scatter:</strong> Studies clustered tightly = consistent test performance</li>
                    </ul>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Quick Assessment Tips</h5>
                    <ul>
                        <li>Closer to upper-left corner = better overall diagnostic accuracy</li>
                        <li>Wide scatter of points = substantial between-study heterogeneity</li>
                        <li>Triangle position shows where your pooled test performance lies</li>
                        <li>Compare triangle position to individual studies for consistency</li>
                    </ul>
                </div>

                <div style='background-color: rgba(33, 163, 188, 0.21); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Clinical Decision Making</h5>
                    <p><strong>Use this plot to:</strong> Visualize test performance trade-offs, identify optimal operating points, and assess consistency across different study populations and settings.</p>
                </div>
            </div>
            "

            self$results$srocplot_explanation$setContent(html)
        },

        .populateFunnelPlotExplanation = function() {
            html <- "
            <div class='plot-explanation' style='background-color: rgba(138, 155, 172, 0.06); padding: 20px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                <h4> Funnel Plot Interpretation Guide</h4>

                <div style='background-color: white; padding: 15px; border-radius: 5px; margin: 10px 0;'>
                    <h5>What This Plot Shows</h5>
                    <p><strong>Funnel Plot:</strong> Assesses publication bias by plotting study precision against effect size (log diagnostic odds ratio).</p>

                    <ul>
                        <li><strong>X-axis:</strong> Log Diagnostic Odds Ratio (effect size)</li>
                        <li><strong>Y-axis:</strong> Precision (1/Standard Error) - higher = more precise</li>
                        <li><strong>Each Point:</strong> One study in your meta-analysis</li>
                        <li><strong>Expected Pattern:</strong> Inverted funnel shape if no bias present</li>
                    </ul>
                </div>

                <div style='background-color: rgba(216, 33, 50, 0.18); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Publication Bias Indicators</h5>
                    <ul>
                        <li><strong>Asymmetric Funnel:</strong> Missing studies on one side (usually left = negative results)</li>
                        <li><strong>Gap in Lower Region:</strong> Small studies with negative/null results missing</li>
                        <li><strong>Deeks' Test p < 0.05:</strong> Statistical evidence of funnel plot asymmetry</li>
                    </ul>
                </div>

                <div style='background-color: rgba(33, 162, 64, 0.19); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> No Bias Indicators</h5>
                    <ul>
                        <li><strong>Symmetric Funnel:</strong> Studies distributed evenly on both sides</li>
                        <li><strong>Deeks' Test p [[GE]] 0.05:</strong> No statistical evidence of asymmetry</li>
                        <li><strong>Small Studies Present:</strong> Range of precision levels represented</li>
                    </ul>
                </div>

                <div style='background-color: rgba(255, 202, 33, 0.23); padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>
                    <h5> Interpretation Caveats</h5>
                    <ul>
                        <li><strong>Small Sample:</strong> Funnel plot unreliable with <10 studies</li>
                        <li><strong>Heterogeneity:</strong> Clinical differences can mimic publication bias</li>
                        <li><strong>Other Causes:</strong> Language bias, database bias, or chance</li>
                        <li><strong>Action Needed:</strong> If bias detected, search for unpublished studies</li>
                    </ul>
                </div>
            </div>
            "

            self$results$funnelplot_explanation$setContent(
                private$.renderSymbols(html)
            )
        }

        # TODO (forward-looking): no `.asSource()` method - the jamovi syntax
        # pane therefore cannot render the equivalent R call for the user's
        # configured analysis (users cannot copy-paste the analysis as
        # reproducible R code). Adding one requires emitting a call shape like:
        #   diagnosticmeta(
        #       data           = data,
        #       study          = <varname>,
        #       true_positives = <varname>, ...
        #       bivariate_analysis   = <bool>,
        #       confidence_level     = <int>,
        #       method               = <enum>,
        #       zero_cell_correction = <enum>,
        #       ...
        #   )
        # Use `jmvcore::sourcifyOption()` per option and `jmvcore::sourcifyName()`
        # for variable references (NOT manual paste0 quoting - see project
        # MEMORY.md `feedback_sourcify_quoting_correct_helper`). The
        # `/add-R-code diagnosticmeta` skill scaffolds this with the
        # `showRCode` option + `rCode` Html output.
    )
)
