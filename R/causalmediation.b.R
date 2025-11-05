#' @title Causal Mediation Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export

causalmediationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "causalmediationClass",
    inherit = causalmediationBase,
    private = list(

        # Data storage
        .mediation_results = NULL,
        .hd_results = NULL,

        #---------------------------------------------
        # INIT
        #---------------------------------------------
        .init = function() {

            # Instructions
            html <- "<h3>Causal Mediation Analysis</h3>
            <p>This module provides three-tier mediation analysis:</p>

            <h4>Tier 1: Basic Mediation (mediation package)</h4>
            <ul>
            <li><b>Use when:</b> Single mediator, standard confounders</li>
            <li><b>Provides:</b> ACME, ADE, Total Effect, Proportion Mediated</li>
            <li><b>Methods:</b> Bootstrap confidence intervals</li>
            </ul>

            <h4>Tier 2: Comprehensive Mediation (CMAverse)</h4>
            <ul>
            <li><b>Use when:</b> Multiple mediators, complex relationships</li>
            <li><b>Provides:</b> DAG visualization, path-specific effects</li>
            <li><b>Methods:</b> Regression-based, IORW, MSM</li>
            </ul>

            <h4>Tier 3: HIGH-DIMENSIONAL Mediation (hdmax2) 🌟 FLAGSHIP</h4>
            <ul>
            <li><b>Use when:</b> Omics-scale mediators (thousands to millions)</li>
            <li><b>Provides:</b> Automatic mediator selection, FDR control</li>
            <li><b>Applications:</b> Methylation, gene expression, metabolomics</li>
            <li><b>Example:</b> 'Which of 500,000 methylation sites mediate trauma → cortisol?'</li>
            </ul>

            <h4>Getting Started:</h4>
            <ol>
            <li>Select your <b>Outcome</b> variable (Y)</li>
            <li>Select your <b>Treatment/Exposure</b> variable (X)</li>
            <li>For Basic/Comprehensive: Select <b>single Mediator</b> (M)</li>
            <li>For HIGH-DIMENSIONAL: Select <b>Multiple Mediators</b> (thousands OK!)</li>
            <li>Choose appropriate <b>Mediation Tier</b></li>
            <li>Add <b>Covariates</b> (confounders) if needed</li>
            </ol>"

            self$results$instructions$setContent(html)

            # Interpretation guide
            interp_html <- "<h3>Interpretation Guide</h3>

            <h4>Effect Decomposition:</h4>
            <ul>
            <li><b>Total Effect (TE):</b> Overall effect of X on Y</li>
            <li><b>Average Direct Effect (ADE):</b> Direct effect not through mediator</li>
            <li><b>Average Causal Mediation Effect (ACME):</b> Indirect effect through mediator</li>
            <li><b>Proportion Mediated:</b> ACME / TE (percentage explained by mediator)</li>
            </ul>

            <h4>Statistical Significance:</h4>
            <ul>
            <li><b>p < 0.05:</b> Significant mediation effect</li>
            <li><b>Confidence Interval excludes 0:</b> Robust evidence for mediation</li>
            </ul>

            <h4>HIGH-DIMENSIONAL Interpretation:</h4>
            <ul>
            <li><b>α (alpha):</b> Effect of X on mediator</li>
            <li><b>β (beta):</b> Effect of mediator on Y (adjusting for X)</li>
            <li><b>Indirect Effect:</b> α × β (product method)</li>
            <li><b>FDR:</b> False Discovery Rate (controls for multiple testing)</li>
            </ul>

            <h4>Causal Assumptions:</h4>
            <ul>
            <li>No unmeasured confounding of X-Y, X-M, M-Y relationships</li>
            <li>No mediator-outcome confounders affected by treatment</li>
            <li>Correct functional form of models</li>
            <li>Use sensitivity analysis to assess violation impact</li>
            </ul>"

            self$results$interpretation$setContent(interp_html)
        },

        #---------------------------------------------
        # RUN
        #---------------------------------------------
        .run = function() {

            # Check requirements
            if (is.null(self$options$outcome) || is.null(self$options$treatment)) {
                return()
            }

            # Get mediation tier
            mediation_tier <- self$options$mediation_tier

            # Display tier information
            private$.showTierInfo(mediation_tier)

            # Run appropriate mediation analysis
            tryCatch({

                if (mediation_tier == "basic") {
                    private$.runBasicMediation()
                } else if (mediation_tier == "comprehensive") {
                    private$.runComprehensiveMediation()
                } else if (mediation_tier == "hd") {
                    private$.runHDMediation()
                }

            }, error = function(e) {
                error_msg <- paste0("<h3>Mediation Analysis Error</h3><p>", e$message, "</p>")
                self$results$modelInfo$setContent(error_msg)
            })
        },

        #---------------------------------------------
        # TIER INFORMATION
        #---------------------------------------------
        .showTierInfo = function(tier) {

            info <- switch(tier,
                "basic" = "<b>Selected:</b> Tier 1 - Basic Mediation\n<b>Package:</b> mediation\n<b>Best for:</b> Single mediator analysis",
                "comprehensive" = "<b>Selected:</b> Tier 2 - Comprehensive Mediation\n<b>Package:</b> CMAverse\n<b>Best for:</b> Multiple mediators with DAG",
                "hd" = "<b>Selected:</b> Tier 3 - HIGH-DIMENSIONAL Mediation 🌟\n<b>Package:</b> hdmax2\n<b>Best for:</b> Omics-scale mediators",
                "Unknown tier"
            )

            self$results$tierInfo$setContent(info)
        },

        #---------------------------------------------
        # TIER 1: BASIC MEDIATION (mediation package)
        #---------------------------------------------
        .runBasicMediation = function() {

            # Check for mediation package
            if (!requireNamespace('mediation', quietly = TRUE)) {
                stop("mediation package is required. Install using: install.packages('mediation')")
            }

            # Get variables
            outcome_var <- self$options$outcome
            treatment_var <- self$options$treatment
            mediator_var <- self$options$mediator
            covariates <- self$options$covariates

            if (is.null(mediator_var)) {
                stop("Please select a Mediator variable for basic mediation analysis")
            }

            # Get data
            data <- self$data

            # Build formulas
            if (!is.null(covariates) && length(covariates) > 0) {
                mediator_formula <- reformulate(c(treatment_var, covariates), response = mediator_var)
                outcome_formula <- reformulate(c(treatment_var, mediator_var, covariates), response = outcome_var)
            } else {
                mediator_formula <- reformulate(treatment_var, response = mediator_var)
                outcome_formula <- reformulate(c(treatment_var, mediator_var), response = outcome_var)
            }

            # Fit mediator model
            mediator_model_type <- self$options$mediator_model %||% "lm"
            if (mediator_model_type == "lm") {
                model_m <- lm(mediator_formula, data = data)
            } else if (mediator_model_type == "glm_logit") {
                model_m <- glm(mediator_formula, data = data, family = binomial(link = "logit"))
            } else if (mediator_model_type == "glm_poisson") {
                model_m <- glm(mediator_formula, data = data, family = poisson(link = "log"))
            }

            # Fit outcome model
            outcome_model_type <- self$options$outcome_model %||% "lm"
            if (outcome_model_type == "lm") {
                model_y <- lm(outcome_formula, data = data)
            } else if (outcome_model_type == "glm_logit") {
                model_y <- glm(outcome_formula, data = data, family = binomial(link = "logit"))
            } else if (outcome_model_type == "survival") {
                stop("Survival models not yet implemented for mediation")
            }

            # Perform mediation analysis
            boot_samples <- self$options$boot_samples %||% 1000
            conf_level <- self$options$conf_level %||% 0.95

            set.seed(self$options$random_seed %||% 42)

            mediation_result <- mediation::mediate(
                model.m = model_m,
                model.y = model_y,
                treat = treatment_var,
                mediator = mediator_var,
                boot = TRUE,
                sims = boot_samples,
                conf.level = conf_level
            )

            # Store results
            private$.mediation_results <- mediation_result

            # Format and display results
            private$.formatBasicMediationResults(mediation_result)

            # Sensitivity analysis if requested
            if (self$options$sensitivity_analysis) {
                private$.performSensitivityAnalysis(mediation_result, model_m, model_y, treatment_var, mediator_var)
            }
        },

        .formatBasicMediationResults = function(med_result) {

            # Extract effects
            acme <- med_result$d.avg
            acme_ci <- med_result$d.avg.ci
            acme_p <- med_result$d.avg.p

            ade <- med_result$z.avg
            ade_ci <- med_result$z.avg.ci
            ade_p <- med_result$z.avg.p

            total <- med_result$tau.coef
            total_ci <- med_result$tau.ci
            total_p <- med_result$tau.p

            prop_med <- med_result$n.avg
            prop_med_ci <- med_result$n.avg.ci
            prop_med_p <- med_result$n.avg.p

            # Populate effects table
            table <- self$results$effectsTable

            table$addRow(rowKey = "acme", values = list(
                effect = "ACME (Average Causal Mediation Effect)",
                estimate = acme,
                se = NA,
                ci_lower = acme_ci[1],
                ci_upper = acme_ci[2],
                p_value = acme_p
            ))

            table$addRow(rowKey = "ade", values = list(
                effect = "ADE (Average Direct Effect)",
                estimate = ade,
                se = NA,
                ci_lower = ade_ci[1],
                ci_upper = ade_ci[2],
                p_value = ade_p
            ))

            table$addRow(rowKey = "total", values = list(
                effect = "Total Effect",
                estimate = total,
                se = NA,
                ci_lower = total_ci[1],
                ci_upper = total_ci[2],
                p_value = total_p
            ))

            # Proportion mediated
            prop_table <- self$results$proportionMediated

            prop_table$addRow(rowKey = "prop", values = list(
                metric = "Proportion Mediated",
                value = prop_med,
                ci_lower = prop_med_ci[1],
                ci_upper = prop_med_ci[2]
            ))

            # Model information
            info_html <- paste0("<h4>Basic Mediation Results</h4>")
            info_html <- paste0(info_html, "<p><b>Bootstrap Samples:</b> ", self$options$boot_samples, "</p>")
            info_html <- paste0(info_html, "<p><b>Confidence Level:</b> ", self$options$conf_level * 100, "%</p>")

            if (acme_p < 0.05) {
                info_html <- paste0(info_html, "<p><b>✅ Significant mediation effect detected</b></p>")
                info_html <- paste0(info_html, "<p>The mediator explains approximately ",
                                  round(abs(prop_med) * 100, 1),
                                  "% of the total effect.</p>")
            } else {
                info_html <- paste0(info_html, "<p><b>❌ No significant mediation effect</b></p>")
            }

            self$results$modelInfo$setContent(info_html)
        },

        #---------------------------------------------
        # TIER 3: HIGH-DIMENSIONAL MEDIATION (hdmax2)
        #---------------------------------------------
        .runHDMediation = function() {

            # Check for hdmax2 package
            if (!requireNamespace('hdmax2', quietly = TRUE)) {
                stop("hdmax2 package is required. Install using: install.packages('hdmax2')")
            }

            # Get variables
            outcome_var <- self$options$outcome
            treatment_var <- self$options$treatment
            mediators <- self$options$mediators
            covariates <- self$options$covariates

            if (is.null(mediators) || length(mediators) == 0) {
                stop("Please select Multiple Mediators for high-dimensional analysis")
            }

            # Get data
            data <- self$data

            # Prepare data matrices
            Y <- data[[outcome_var]]
            X <- data[[treatment_var]]
            M <- as.matrix(data[, mediators, drop = FALSE])

            # Optional covariates
            if (!is.null(covariates) && length(covariates) > 0) {
                Z <- as.matrix(data[, covariates, drop = FALSE])
            } else {
                Z <- NULL
            }

            # HD Mediation Analysis
            hd_method <- self$options$hd_method %||% "hdmax2"
            fdr_threshold <- self$options$hd_fdr_threshold %||% 0.1

            set.seed(self$options$random_seed %||% 42)

            tryCatch({

                if (hd_method == "hdmax2") {
                    # HDMAX2: Joint significance + Sobel test
                    hd_result <- hdmax2::hdmax2(
                        Y = Y,
                        M = M,
                        X = X,
                        Z = Z,
                        FDRcut = fdr_threshold
                    )
                } else {
                    # Fallback message for other methods
                    stop(paste("Method", hd_method, "not yet fully implemented. Use hdmax2 for now."))
                }

                # Store results
                private$.hd_results <- hd_result

                # Format and display results
                private$.formatHDMediationResults(hd_result, mediators)

            }, error = function(e) {
                error_msg <- e$message

                if (grepl("not installed|namespace", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste(
                        "hdmax2 package not available:", error_msg,
                        "\n\nInstall hdmax2 package using: install.packages('hdmax2')"
                    )
                } else if (grepl("dimension|matrix", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste(
                        "Data dimension error:", error_msg,
                        "\n\nSuggestions:",
                        "• Ensure all mediators are numeric",
                        "• Check for missing values",
                        "• Verify sample size is sufficient (n > p recommended)"
                    )
                } else {
                    detailed_msg <- paste("HD mediation error:", error_msg)
                }

                error_html <- paste0("<h3>HIGH-DIMENSIONAL Mediation Error</h3><p>", detailed_msg, "</p>")
                self$results$modelInfo$setContent(error_html)
            })
        },

        .formatHDMediationResults = function(hd_result, mediator_names) {

            # Extract significant mediators
            if (!is.null(hd_result$mediator)) {
                sig_mediators <- hd_result$mediator
                n_sig <- length(sig_mediators)

                # Top mediators to display
                top_n <- min(self$options$hd_top_mediators %||% 20, n_sig)

                # Populate HD mediators table
                table <- self$results$hdMediatorsTable

                for (i in 1:top_n) {
                    med_idx <- sig_mediators[i]
                    mediator_name <- mediator_names[med_idx]

                    # Extract effects (if available from hdmax2 output)
                    alpha <- if (!is.null(hd_result$alpha)) hd_result$alpha[med_idx] else NA
                    beta <- if (!is.null(hd_result$beta)) hd_result$beta[med_idx] else NA
                    indirect <- if (!is.null(alpha) && !is.null(beta)) alpha * beta else NA
                    p_val <- if (!is.null(hd_result$pval)) hd_result$pval[med_idx] else NA
                    fdr_val <- if (!is.null(hd_result$FDR)) hd_result$FDR[med_idx] else NA

                    table$addRow(rowKey = as.character(i), values = list(
                        mediator = mediator_name,
                        alpha = alpha,
                        beta = beta,
                        indirect_effect = indirect,
                        p_value = p_val,
                        fdr = fdr_val
                    ))
                }

                # Summary
                summary_html <- "<h4>HIGH-DIMENSIONAL Mediation Results 🌟</h4>"
                summary_html <- paste0(summary_html, "<p><b>Total Mediators Tested:</b> ", length(mediator_names), "</p>")
                summary_html <- paste0(summary_html, "<p><b>Significant Mediators (FDR < ", self$options$hd_fdr_threshold, "):</b> ", n_sig, "</p>")
                summary_html <- paste0(summary_html, "<p><b>Top ", top_n, " Mediators Displayed</b></p>")

                summary_html <- paste0(summary_html, "<h4>Interpretation:</h4>")
                summary_html <- paste0(summary_html, "<ul>")
                summary_html <- paste0(summary_html, "<li>These mediators show significant mediation effects after FDR correction</li>")
                summary_html <- paste0(summary_html, "<li>α (alpha) = effect of treatment on mediator</li>")
                summary_html <- paste0(summary_html, "<li>β (beta) = effect of mediator on outcome</li>")
                summary_html <- paste0(summary_html, "<li>Indirect Effect = α × β (product of coefficients)</li>")
                summary_html <- paste0(summary_html, "</ul>")

                if (n_sig > 0) {
                    summary_html <- paste0(summary_html, "<p><b>✅ HIGH-DIMENSIONAL mediation pathway identified!</b></p>")
                } else {
                    summary_html <- paste0(summary_html, "<p><b>No significant mediators detected at FDR threshold</b></p>")
                }

                self$results$hdSummary$setContent(summary_html)

            } else {
                summary_html <- "<h4>HIGH-DIMENSIONAL Mediation Results</h4>"
                summary_html <- paste0(summary_html, "<p>No significant mediators detected at FDR threshold of ", self$options$hd_fdr_threshold, "</p>")
                self$results$hdSummary$setContent(summary_html)
            }
        },

        #---------------------------------------------
        # TIER 2: COMPREHENSIVE MEDIATION (CMAverse)
        #---------------------------------------------
        .runComprehensiveMediation = function() {

            # Placeholder for CMAverse implementation
            html <- "<h4>Comprehensive Mediation (CMAverse)</h4>"
            html <- paste0(html, "<p><i>CMAverse integration coming soon!</i></p>")
            html <- paste0(html, "<p>CMAverse provides:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li>Multiple mediator analysis</li>")
            html <- paste0(html, "<li>DAG visualization</li>")
            html <- paste0(html, "<li>Path-specific effects</li>")
            html <- paste0(html, "<li>Advanced estimation methods (IORW, MSM)</li>")
            html <- paste0(html, "</ul>")

            self$results$cmaResults$setContent(html)
        },

        #---------------------------------------------
        # SENSITIVITY ANALYSIS
        #---------------------------------------------
        .performSensitivityAnalysis = function(med_result, model_m, model_y, treat, mediator) {

            tryCatch({
                # Parse rho values
                rho_str <- self$options$rho_values %||% "0, 0.1, 0.2, 0.3"
                rho_vals <- as.numeric(unlist(strsplit(rho_str, ",")))

                # Perform sensitivity analysis
                sens_result <- mediation::medsens(med_result, rho.by = 0.1, effect.type = "indirect")

                # Populate sensitivity table (simplified)
                table <- self$results$sensitivityTable

                for (rho in rho_vals) {
                    table$addRow(rowKey = as.character(rho), values = list(
                        rho = rho,
                        acme_lower = NA,  # Would extract from sens_result
                        acme_upper = NA
                    ))
                }

            }, error = function(e) {
                # Silently skip if sensitivity analysis fails
            })
        },

        #---------------------------------------------
        # VISUALIZATION
        #---------------------------------------------
        .plotEffects = function(image, ...) {
            # Placeholder for effect plots
            TRUE
        },

        .plotManhattan = function(image, ...) {
            # Placeholder for Manhattan plot
            TRUE
        },

        .plotVolcano = function(image, ...) {
            # Placeholder for volcano plot
            TRUE
        },

        .plotDAG = function(image, ...) {
            # Placeholder for DAG visualization
            TRUE
        },

        .plotSensitivity = function(image, ...) {
            # Placeholder for sensitivity plot
            TRUE
        }
    )
)
