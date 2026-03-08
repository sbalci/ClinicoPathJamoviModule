ncvregcoxClass <- R6::R6Class(
    "ncvregcoxClass",
    inherit = ncvregcoxBase,
    private = list(
        .analysis_data = NULL,
        .covariates = NULL,
        .cv_fit = NULL,
        .final_fit = NULL,
        .lambda_opt = NULL,
        .X = NULL,
        .Y = NULL,

        .init = function() {
            private$.update_instructions()
        },
        
        .run = function() {
            if (is.null(self$options$time) || is.null(self$options$event) ||
                length(self$options$covariates) == 0) {
                return()
            }
            
            private$.prepare_data()

            # Assess data suitability if requested
            if (self$options$suitabilityCheck) {
                private$.assessSuitability()
            }

            private$.fit_ncvreg_cox()
            private$.populate_results()
            private$.create_plots()
        },
        
        .update_instructions = function() {
            html_content <- private$.generate_instructions_html()
            self$results$instructions$setContent(html_content)
        },
        
        .generate_instructions_html = function() {
            penalty <- self$options$penalty
            
            html <- paste0(
                "<h3>SCAD Cox Regression Analysis</h3>",
                "<p>Smoothly Clipped Absolute Deviation (SCAD) Cox regression for high-dimensional survival data analysis.</p>",
                "<h4>Current Configuration:</h4>",
                "<ul>",
                "<li><strong>Penalty Function:</strong> ", private$.format_penalty(penalty), "</li>",
                "<li><strong>Cross-Validation:</strong> ", self$options$cv_folds, "-fold CV</li>",
                "<li><strong>Lambda Selection:</strong> ", private$.format_lambda_type(self$options$lambda_type), "</li>",
                "</ul>",
                "<h4>Key Features:</h4>",
                "<ul>",
                "<li>Oracle properties for variable selection</li>",
                "<li>Avoids over-penalization of large coefficients</li>",
                "<li>Maintains sparsity for irrelevant variables</li>",
                "<li>Cross-validation for optimal penalty selection</li>",
                "<li>Variable importance and stability analysis</li>",
                "</ul>",
                "<p><strong>Note:</strong> SCAD penalty provides superior variable selection properties ",
                "compared to LASSO, particularly for scenarios with large true effects.</p>"
            )
            
            return(html)
        },
        
        .format_penalty = function(penalty) {
            switch(penalty,
                "SCAD" = "Smoothly Clipped Absolute Deviation",
                "MCP" = "Minimax Concave Penalty",
                "lasso" = "LASSO (L1 Penalty)",
                penalty
            )
        },
        
        .format_lambda_type = function(type) {
            switch(type,
                "min" = "Minimum CV Error",
                "1se" = "One Standard Error Rule",
                type
            )
        },
        
        .prepare_data = function() {
            # Get data
            data <- self$data
            
            # Extract variables
            time_var <- self$options$time
            event_var <- self$options$event
            covariates <- self$options$covariates
            
            if (is.null(time_var) || is.null(event_var) || length(covariates) == 0) {
                return()
            }
            
            # Create analysis dataset
            event_col <- data[[event_var]]
            # Convert factor event to numeric (0/1) if needed
            if (is.factor(event_col)) {
                event_col <- as.numeric(event_col) - 1
            }
            analysis_data <- data.frame(
                time = as.numeric(data[[time_var]]),
                event = as.numeric(event_col)
            )
            
            # Add covariates
            for (cov in covariates) {
                analysis_data[[cov]] <- data[[cov]]
            }
            
            # Remove missing values
            analysis_data <- na.omit(analysis_data)
            
            if (nrow(analysis_data) == 0) {
                stop("No complete cases available for analysis")
            }
            
            private$.analysis_data <- analysis_data
            private$.covariates <- covariates
        },
        
        .fit_ncvreg_cox = function() {
            if (!requireNamespace("ncvreg", quietly = TRUE)) {
                stop("Package 'ncvreg' is required for SCAD Cox regression")
            }
            
            if (!requireNamespace("survival", quietly = TRUE)) {
                stop("Package 'survival' is required for Cox regression")
            }
            
            tryCatch({
                data <- private$.analysis_data
                covariates <- private$.covariates
                
                # Create design matrix (handle factors via model.matrix)
                cov_data <- data[, covariates, drop = FALSE]
                has_factors <- any(sapply(cov_data, is.factor))
                if (has_factors) {
                    mm_formula <- as.formula(paste("~", paste(covariates, collapse = " + ")))
                    X <- model.matrix(mm_formula, data = cov_data)[, -1, drop = FALSE]
                } else {
                    X <- as.matrix(cov_data)
                }
                
                # Create survival object
                Y <- survival::Surv(data$time, data$event)
                
                # Handle different penalty types
                penalty_type <- self$options$penalty
                if (penalty_type == "lasso") {
                    penalty_type <- "lasso"
                } else if (penalty_type == "MCP") {
                    penalty_type <- "MCP"
                } else {
                    penalty_type <- "SCAD"
                }
                
                # Fit ncvreg Cox model with cross-validation
                cv_fit <- ncvreg::cv.ncvsurv(
                    X = X,
                    y = Y,
                    penalty = penalty_type,
                    gamma = self$options$gamma,
                    alpha = self$options$alpha,
                    nfolds = self$options$cv_folds,
                    standardize = self$options$standardize,
                    returnY = FALSE
                )
                
                # Get optimal lambda (fall back to lambda.min if 1se not available)
                lambda_type <- self$options$lambda_type
                if (lambda_type == "1se" && !is.null(cv_fit$lambda.1se) && length(cv_fit$lambda.1se) == 1) {
                    lambda_opt <- cv_fit$lambda.1se
                } else {
                    lambda_opt <- cv_fit$lambda.min
                }
                
                # Use the fit from cv.ncvsurv (already has the full path)
                final_fit <- cv_fit$fit

                private$.cv_fit <- cv_fit
                private$.final_fit <- final_fit
                private$.lambda_opt <- lambda_opt
                private$.X <- X
                private$.Y <- Y
                
            }, error = function(e) {
                stop(paste("Error in SCAD Cox regression:", e$message))
            })
        },
        
        .populate_results = function() {
            if (is.null(private$.final_fit)) {
                return()
            }
            
            private$.populate_model_summary()
            private$.populate_selected_variables()
            private$.populate_cross_validation_results()
            private$.populate_model_comparison()
            private$.populate_convergence_info()
            private$.create_model_interpretation()
            
            if (self$options$variable_importance) {
                private$.populate_variable_importance()
            }
        },
        
        .populate_model_summary = function() {
            table <- self$results$model_summary
            cv_fit <- private$.cv_fit
            final_fit <- private$.final_fit
            lambda_opt <- private$.lambda_opt

            # Calculate metrics
            penalty_name <- private$.format_penalty(self$options$penalty)
            cv_error <- min(cv_fit$cve, na.rm = TRUE)

            # Get selected variables
            coeffs <- coef(final_fit, lambda = lambda_opt)
            n_selected <- as.integer(sum(coeffs != 0))

            # Computer proper C-index from linear predictor
            cindex <- NA_real_
            if (n_selected > 0) {
                cindex <- tryCatch({
                    lp <- as.numeric(private$.X %*% coeffs)
                    # Higher LP = higher hazard = worse prognosis, so reverse=TRUE
                    ci <- survival::concordance(private$.Y ~ lp, reverse = TRUE)
                    as.numeric(ci$concordance)
                }, error = function(e) NA_real_)
            }

            table$addRow(rowKey = 1, values = list(
                penalty = penalty_name,
                lambda_selected = as.numeric(lambda_opt),
                cv_error = as.numeric(cv_error),
                n_selected = n_selected,
                deviance_explained = as.numeric(if (!is.na(cindex)) cindex else NA_real_)
            ))
        },
        
        .populate_selected_variables = function() {
            table <- self$results$selected_variables
            final_fit <- private$.final_fit
            lambda_opt <- private$.lambda_opt

            # Get penalized coefficients
            coeffs <- coef(final_fit, lambda = lambda_opt)
            var_names <- colnames(private$.X)
            if (is.null(var_names)) var_names <- paste0("V", seq_along(coeffs))
            selected_vars <- which(coeffs != 0)

            if (length(selected_vars) == 0) {
                return()
            }

            for (i in seq_along(selected_vars)) {
                var_idx <- selected_vars[i]
                var_name <- var_names[var_idx]
                # Use penalized coefficient (from SCAD/MCP)
                coeff <- as.numeric(coeffs[var_idx])
                hr <- exp(coeff)

                table$addRow(rowKey = i, values = list(
                    variable = var_name,
                    coefficient = coeff,
                    hazard_ratio = hr
                ))
            }
        },
        
        .populate_cross_validation_results = function() {
            table <- self$results$cross_validation_results
            cv_fit <- private$.cv_fit
            
            # Get CV results
            lambdas <- cv_fit$lambda
            cv_errors <- cv_fit$cve
            cv_ses <- cv_fit$cvse
            
            # Get number of variables for each lambda
            n_vars <- apply(cv_fit$fit$beta, 2, function(x) sum(x != 0))
            
            # Add results (show key lambda values)
            key_indices <- c(1, which.min(cv_errors), length(lambdas))
            key_indices <- unique(key_indices)
            
            for (i in seq_along(key_indices)) {
                idx <- key_indices[i]
                table$addRow(rowKey = i, values = list(
                    lambda = lambdas[idx],
                    cv_error = cv_errors[idx],
                    cv_se = cv_ses[idx],
                    n_vars = n_vars[idx]
                ))
            }
        },
        
        .populate_model_comparison = function() {
            table <- self$results$model_comparison
            cv_fit <- private$.cv_fit

            # Compare different lambda selections
            lambda_min <- cv_fit$lambda.min
            lambda_1se <- cv_fit$lambda.1se

            # Build model list (only include valid lambdas)
            models <- list()
            if (!is.null(lambda_min) && length(lambda_min) == 1) {
                min_idx <- which.min(abs(cv_fit$lambda - lambda_min))
                models[[length(models) + 1]] <- list(name = "Lambda Min", lambda = lambda_min, idx = min_idx)
            }
            if (!is.null(lambda_1se) && length(lambda_1se) == 1) {
                se_idx <- which.min(abs(cv_fit$lambda - lambda_1se))
                models[[length(models) + 1]] <- list(name = "Lambda 1SE", lambda = lambda_1se, idx = se_idx)
            }

            if (length(models) == 0) return()

            for (i in seq_along(models)) {
                model <- models[[i]]
                idx <- model$idx

                cv_error <- as.numeric(cv_fit$cve[idx])
                beta_at_lambda <- cv_fit$fit$beta[, idx]
                n_vars <- as.integer(sum(beta_at_lambda != 0))

                # Compute proper C-index from the linear predictor
                c_index <- NA_real_
                if (n_vars > 0) {
                    c_index <- tryCatch({
                        lp <- as.numeric(private$.X %*% beta_at_lambda)
                        ci <- survival::concordance(private$.Y ~ lp, reverse = TRUE)
                        as.numeric(ci$concordance)
                    }, error = function(e) NA_real_)
                }

                # Compute proper AIC by refitting Cox with selected variables
                aic <- tryCatch({
                    sel <- which(beta_at_lambda != 0)
                    if (length(sel) > 0) {
                        sel_X <- private$.X[, sel, drop = FALSE]
                        refit <- survival::coxph(private$.Y ~ sel_X)
                        AIC(refit)
                    } else {
                        NA_real_
                    }
                }, error = function(e) NA_real_)

                table$addRow(rowKey = i, values = list(
                    model = model$name,
                    lambda = as.numeric(model$lambda),
                    cv_error = cv_error,
                    n_vars = n_vars,
                    c_index = as.numeric(c_index),
                    aic = as.numeric(aic)
                ))
            }
        },
        
        .populate_convergence_info = function() {
            table <- self$results$convergence_info
            final_fit <- private$.final_fit

            # Extract real convergence info from the ncvreg fit object
            n_iter <- tryCatch(
                as.integer(final_fit$iter),
                error = function(e) NA_integer_
            )
            # ncvreg uses eps for convergence tolerance (default 1e-4)
            tol <- tryCatch(
                as.numeric(final_fit$eps),
                error = function(e) NA_real_
            )
            # ncvreg does not expose a "converged" flag directly;
            # if iter < max.iter the algorithm converged
            max_iter <- tryCatch(
                as.integer(final_fit$max.iter),
                error = function(e) NA_integer_
            )
            converged_flag <- if (!is.na(n_iter) && !is.na(max_iter)) {
                if (n_iter < max_iter) "Yes" else "No (max iterations reached)"
            } else {
                "Unknown"
            }

            table$addRow(rowKey = 1, values = list(
                converged = converged_flag,
                iterations = if (!is.na(n_iter)) n_iter else NA_integer_,
                tolerance = if (!is.na(tol)) tol else NA_real_,
                algorithm = paste("Coordinate Descent with", self$options$penalty, "penalty")
            ))
        },
        
        .populate_variable_importance = function() {
            table <- self$results$variable_importance
            final_fit <- private$.final_fit
            lambda_opt <- private$.lambda_opt

            # Calculate importance based on absolute coefficients
            coeffs <- coef(final_fit, lambda = lambda_opt)
            var_names <- colnames(private$.X)
            if (is.null(var_names)) var_names <- paste0("V", seq_along(coeffs))
            importance <- abs(as.numeric(coeffs))

            # Rank variables
            ranks <- rank(-importance, ties.method = "first")
            max_imp <- max(importance, na.rm = TRUE)
            relative_importance <- if (max_imp > 0) (importance / max_imp) * 100 else rep(0, length(importance))

            # Add variables with non-zero importance
            for (i in seq_along(coeffs)) {
                if (importance[i] > 0) {
                    table$addRow(rowKey = i, values = list(
                        variable = var_names[i],
                        importance = importance[i],
                        rank = as.integer(ranks[i]),
                        relative_importance = relative_importance[i]
                    ))
                }
            }
        },
        
        .create_plots = function() {
            # Store plot data in image states for render functions
            if (!is.null(private$.cv_fit)) {
                self$results$regularization_path$setState(list(ready = TRUE))
                self$results$cv_error_plot$setState(list(ready = TRUE))
            }
            if (!is.null(private$.final_fit) && self$options$variable_importance) {
                self$results$variable_selection_plot$setState(list(ready = TRUE))
            }
        },

        .plot_regularization_path = function(image, ggtheme, theme, ...) {
            if (is.null(private$.final_fit)) return(FALSE)
            plot(private$.final_fit)
            title("Regularization Path", line = 2.5)
            return(TRUE)
        },

        .plot_cv_error = function(image, ggtheme, theme, ...) {
            if (is.null(private$.cv_fit)) return(FALSE)
            plot(private$.cv_fit)
            title("Cross-Validation Error", line = 2.5)
            return(TRUE)
        },

        .plot_variable_selection = function(image, ggtheme, theme, ...) {
            if (is.null(private$.final_fit)) return(FALSE)
            coeffs <- coef(private$.final_fit, lambda = private$.lambda_opt)
            importance <- abs(coeffs)
            importance <- importance[importance > 0]
            if (length(importance) == 0) {
                plot.new()
                text(0.5, 0.5, "No variables selected", cex = 1.2)
                return(TRUE)
            }
            importance <- sort(importance, decreasing = TRUE)
            barplot(importance, las = 2, main = "Variable Importance",
                    ylab = "Absolute Coefficient", col = "steelblue",
                    cex.names = 0.8)
            return(TRUE)
        },
        
        .create_model_interpretation = function() {
            if (is.null(private$.final_fit)) {
                return()
            }
            
            coeffs <- coef(private$.final_fit, lambda = private$.lambda_opt)
            n_selected <- sum(coeffs != 0)
            penalty <- self$options$penalty
            
            html <- paste0(
                "<h4>Model Interpretation</h4>",
                "<p><strong>Variable Selection Results:</strong> ",
                "The ", private$.format_penalty(penalty), " penalty selected ", n_selected, 
                " variables out of ", length(private$.covariates), " total variables.</p>",
                
                "<h5>Clinical Implications:</h5>",
                "<ul>",
                "<li><strong>Sparsity:</strong> The model identified a parsimonious set of predictive variables</li>",
                "<li><strong>Interpretability:</strong> Selected variables represent key predictors of survival</li>",
                "<li><strong>Regularization:</strong> ", penalty, " penalty prevents overfitting in high-dimensional data</li>",
                "</ul>",
                
                "<h5>Methodological Notes:</h5>",
                "<ul>",
                "<li>Oracle properties ensure consistent variable selection</li>",
                "<li>Cross-validation provides unbiased performance estimates</li>",
                "<li>Hazard ratios represent multiplicative effects on hazard</li>",
                "<li>Standard errors and p-values are not reported for penalized regression. Traditional inference is invalid after variable selection (post-selection inference).</li>",
                "</ul>"
            )
            
            self$results$model_interpretation$setContent(html)
        },

        # Data suitability assessment
        .assessSuitability = function() {
            checks <- list()
            data <- private$.analysis_data
            
            n <- nrow(data)
            n_events <- sum(data$event == 1)
            p <- length(private$.covariates)
            event_rate <- n_events / n

            # -- Check 1: Events-Per-Variable (EPV) --
            epv <- n_events / p
            if (epv >= 10) {
                checks$epv <- list(
                    color = "green", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "High EPV. Regularization will perform robustly."
                )
            } else if (epv >= 1) {
                checks$epv <- list(
                    color = "yellow", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "Adequate for SCAD/MCP penalized regression, which handles low EPV better than standard Cox."
                )
            } else {
                checks$epv <- list(
                    color = "yellow", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.3f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "Ultra-low EPV. Standard Cox would fail. Penalized regression is strictly required."
                )
            }

            # -- Check 2: Regularization Need --
            if (p >= n / 3) {
                checks$regularization <- list(
                    color = "green", label = "Regularization Need",
                    value = sprintf("p=%d, n=%d (ratio=%.2f)", p, n, p / n),
                    detail = "High-dimensional setting. Penalized regularization is strongly indicated."
                )
            } else {
                checks$regularization <- list(
                    color = "yellow", label = "Regularization Need",
                    value = sprintf("p=%d, EPV=%.0f", p, epv),
                    detail = "Moderate/low dimensionality. Standard Cox may also suffice."
                )
            }

            # -- Check 3: Sample Size --
            if (n >= 100) {
                checks$sample_size <- list(
                    color = "green", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Adequate sample size for penalized regression."
                )
            } else if (n >= 30) {
                checks$sample_size <- list(
                    color = "yellow", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Small sample. CV folds may be somewhat unstable."
                )
            } else {
                checks$sample_size <- list(
                    color = "red", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Very small sample. Results will be highly variable."
                )
            }

            # -- Check 4: Event Rate --
            if (event_rate >= 0.20 && event_rate <= 0.80) {
                checks$event_rate <- list(
                    color = "green", label = "Event Rate",
                    value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
                    detail = "Balanced event rate. Good for model estimation."
                )
            } else {
                checks$event_rate <- list(
                    color = "yellow", label = "Event Rate",
                    value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
                    detail = "Imbalanced event rate. Model calibration may be affected."
                )
            }

            # -- Check 5: Multicollinearity --
            tryCatch({
                if (p <= 2000 && p >= 2) {
                    cov_data <- data[, private$.covariates, drop = FALSE]
                    # Only calculate correlation on numeric columns to avoid failures
                    num_data <- cov_data[, sapply(cov_data, is.numeric), drop = FALSE]
                    if (ncol(num_data) >= 2) {
                        cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
                        diag(cor_matrix) <- 0
                        max_cor <- max(abs(cor_matrix), na.rm = TRUE)
                        
                        if (max_cor < 0.7) {
                            checks$collinearity <- list(
                                color = "green", label = "Multicollinearity",
                                value = sprintf("Max |r| = %.2f", max_cor),
                                detail = "No concerning collinearity detected."
                            )
                        } else if (max_cor < 0.9) {
                            checks$collinearity <- list(
                                color = "yellow", label = "Multicollinearity",
                                value = sprintf("Max |r| = %.2f", max_cor),
                                detail = "Moderate collinearity. SCAD/MCP works well under moderate collinearity."
                            )
                        } else {
                            checks$collinearity <- list(
                                color = "red", label = "Multicollinearity",
                                value = sprintf("Max |r| = %.2f", max_cor),
                                detail = "High collinearity. SCAD/MCP can be highly unstable under extreme collinearity. Consider using Elastic Net (via Penalized Cox option) instead."
                            )
                        }
                    }
                }
            }, error = function(e) {
                NULL
            })

            # -- Check 6: Data Quality --
            original_data <- self$data
            n_total <- nrow(original_data)
            n_missing <- n_total - n
            pct_missing <- 100 * n_missing / n_total

            if (n_missing == 0) {
                checks$data_quality <- list(
                    color = "green", label = "Data Quality",
                    value = "No missing data",
                    detail = "Complete dataset."
                )
            } else {
                checks$data_quality <- list(
                    color = if (pct_missing > 20) "red" else "yellow",
                    label = "Data Quality",
                    value = sprintf("%.1f%% missing", pct_missing),
                    detail = sprintf("%.1f%% missing data (%d rows excluded).", pct_missing, n_missing)
                )
            }

            # -- Overall Verdict --
            colors <- sapply(checks, function(x) x$color)
            if (any(colors == "red")) {
                overall <- "red"
                overall_text <- "Some issues require attention before relying on these results."
            } else if (any(colors == "yellow")) {
                overall <- "yellow"
                overall_text <- "Data is usable but review the flagged items."
            } else {
                overall <- "green"
                overall_text <- "Data is well-suited for SCAD/MCP Cox regression."
            }

            private$.generateSuitabilityHtml(checks, overall, overall_text)
        },

        .generateSuitabilityHtml = function(checks, overall, overall_text) {
            bg_colors <- list(
                green  = "background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb;",
                yellow = "background-color: #fff3cd; color: #856404; border: 1px solid #ffeeba;",
                red    = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb;"
            )
            dot_colors <- list(green = "#28a745", yellow = "#ffc107", red = "#dc3545")

            html <- paste0(
                "<div style='", bg_colors[[overall]], " padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
                "<strong>Overall: ", overall_text, "</strong></div>"
            )

            html <- paste0(html,
                "<table style='width: 100%; border-collapse: collapse; font-size: 13px;'>",
                "<thead><tr style='border-bottom: 2px solid #dee2e6;'>",
                "<th style='padding: 6px; text-align: left;'>Status</th>",
                "<th style='padding: 6px; text-align: left;'>Check</th>",
                "<th style='padding: 6px; text-align: left;'>Value</th>",
                "<th style='padding: 6px; text-align: left;'>Detail</th>",
                "</tr></thead><tbody>"
            )

            for (chk in checks) {
                if (is.null(chk)) next
                dot <- paste0("<span style='color: ", dot_colors[[chk$color]], "; font-size: 18px;'>&#9679;</span>")
                html <- paste0(html,
                    "<tr style='border-bottom: 1px solid #dee2e6;'>",
                    "<td style='padding: 6px;'>", dot, "</td>",
                    "<td style='padding: 6px;'><strong>", chk$label, "</strong></td>",
                    "<td style='padding: 6px;'>", chk$value, "</td>",
                    "<td style='padding: 6px;'>", chk$detail, "</td>",
                    "</tr>"
                )
            }

            html <- paste0(html, "</tbody></table>")

            self$results$suitabilityReport$setContent(html)
        }
    ),
    
    public = list(
        initialize = function(...) {
            super$initialize(...)
        }
    )
)