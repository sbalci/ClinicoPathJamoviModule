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
        .warnings_collected = NULL,
        .lambda_1se_fallback = FALSE,
        .cindex_cached = NA_real_,

        .init = function() {
            private$.update_instructions()
        },

        .run = function() {
            if (is.null(self$options$time) || is.null(self$options$event) ||
                length(self$options$covariates) == 0) {
                return()
            }

            # Collect warnings from the entire analysis pipeline
            private$.warnings_collected <- character(0)
            analysis_ok <- TRUE

            tryCatch(
                withCallingHandlers({
                    private$.prepare_data()

                    if (self$options$suitabilityCheck) {
                        private$.assessSuitability()
                    }

                    private$.fit_ncvreg_cox()
                    private$.populate_results()
                    private$.create_plots()
                }, warning = function(w) {
                    private$.warnings_collected <- c(
                        private$.warnings_collected, conditionMessage(w)
                    )
                    invokeRestart("muffleWarning")
                }),
                error = function(e) {
                    analysis_ok <<- FALSE
                    private$.insertNotice(
                        'analysisError', jmvcore::NoticeType$ERROR,
                        e$message, position = 1
                    )
                }
            )

            if (!analysis_ok) return()

            # Post-analysis notices
            private$.add_post_analysis_notices()

            # Append collected warnings to model interpretation (Html fallback)
            if (length(private$.warnings_collected) > 0) {
                private$.append_warnings_html()
                # Also add a Notice banner for convergence warnings
                n_warn <- length(unique(private$.warnings_collected))
                private$.insertNotice(
                    'convergenceWarnings', jmvcore::NoticeType$WARNING,
                    sprintf('%d warning(s) during model fitting -- see Model Interpretation section for details.', n_warn),
                    position = 999
                )
            }

            # Completion info notice
            if (!is.null(private$.final_fit)) {
                n_obs <- nrow(private$.analysis_data)
                n_events <- sum(private$.analysis_data$event == 1)
                coeffs <- coef(private$.final_fit, lambda = private$.lambda_opt)
                n_sel <- sum(coeffs != 0)
                private$.insertNotice(
                    'analysisComplete', jmvcore::NoticeType$INFO,
                    sprintf('Analysis completed: n=%d, events=%d, %s penalty, %d/%d variables selected.',
                            n_obs, n_events, self$options$penalty, n_sel, ncol(private$.X)),
                    position = 999
                )
            }
        },

        .update_instructions = function() {
            html_content <- private$.generate_instructions_html()
            self$results$instructions$setContent(html_content)
        },

        .generate_instructions_html = function() {
            penalty <- self$options$penalty

            html <- paste0(
                "<h3>SCAD/MCP Cox Regression Analysis</h3>",
                "<p>Non-convex penalized Cox regression for high-dimensional survival data analysis.</p>",
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
                "<p><strong>Note:</strong> ", penalty, " penalty provides superior variable selection properties ",
                "compared to LASSO, particularly for scenarios with large true effects.</p>"
            )

            return(html)
        },

        .format_penalty = function(penalty) {
            switch(penalty,
                "SCAD" = "Smoothly Clipped Absolute Deviation (SCAD)",
                "MCP" = "Minimax Concave Penalty (MCP)",
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
            data <- self$data

            time_var <- self$options$time
            event_var <- self$options$event
            covariates <- self$options$covariates

            if (is.null(time_var) || is.null(event_var) || length(covariates) == 0) {
                return()
            }

            # Two-level outcome encoding
            event_col <- data[[event_var]]
            event_chr <- as.character(event_col)

            outcome_level_opt <- self$options$outcomeLevel
            censor_level_opt <- self$options$censorLevel

            observed_levels <- sort(unique(event_chr[!is.na(event_chr)]))
            if (length(observed_levels) < 2) {
                stop("Event variable must have at least 2 observed levels.")
            }

            # Resolve event level
            if (is.null(outcome_level_opt) || !nzchar(as.character(outcome_level_opt))) {
                event_level <- observed_levels[2]
            } else {
                event_level <- as.character(outcome_level_opt)
                if (!(event_level %in% observed_levels)) {
                    stop(paste0("Selected event level ('", event_level,
                                "') is not present in observed data. Available levels: ",
                                paste(observed_levels, collapse = ", ")))
                }
            }

            # Resolve censor level
            if (is.null(censor_level_opt) || !nzchar(as.character(censor_level_opt))) {
                remaining <- setdiff(observed_levels, event_level)
                censor_level <- remaining[1]
            } else {
                censor_level <- as.character(censor_level_opt)
                if (!(censor_level %in% observed_levels)) {
                    stop(paste0("Selected censored level ('", censor_level,
                                "') is not present in observed data. Available levels: ",
                                paste(observed_levels, collapse = ", ")))
                }
            }

            if (event_level == censor_level) {
                stop("Event level and censored level must be different.")
            }

            # Strict two-level encoding
            event_numeric <- rep(NA_real_, length(event_chr))
            event_numeric[event_chr == event_level] <- 1
            event_numeric[event_chr == censor_level] <- 0

            analysis_data <- data.frame(
                time = as.numeric(data[[time_var]]),
                event = event_numeric
            )

            for (cov in covariates) {
                analysis_data[[cov]] <- data[[cov]]
            }

            # Complete-case filter BEFORE model.matrix (critical for factors)
            analysis_data <- na.omit(analysis_data)

            if (nrow(analysis_data) == 0) {
                stop("No complete cases available for analysis")
            }

            private$.analysis_data <- analysis_data
            private$.covariates <- covariates
        },

        .fit_ncvreg_cox = function() {
            if (!requireNamespace("ncvreg", quietly = TRUE)) {
                stop("Package 'ncvreg' is required for SCAD/MCP Cox regression")
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
                    # Backtick-escape variable names for safe formula construction
                    escaped_covs <- paste0("`", covariates, "`")
                    mm_formula <- as.formula(paste("~", paste(escaped_covs, collapse = " + ")))
                    X <- model.matrix(mm_formula, data = cov_data)[, -1, drop = FALSE]
                } else {
                    X <- as.matrix(cov_data)
                }

                # Create survival object
                Y <- survival::Surv(data$time, data$event)

                # Map penalty option to ncvreg argument
                penalty_type <- self$options$penalty

                # Use appropriate gamma default per upstream ncvreg:
                # SCAD default gamma = 3.7, MCP default gamma = 3.0
                # If user left gamma at the SCAD default (3.7) but switched to MCP,
                # use the MCP-recommended default instead
                gamma_val <- self$options$gamma
                if (penalty_type == "MCP" && gamma_val == 3.7) {
                    gamma_val <- 3.0
                }

                # Fit ncvreg Cox model with cross-validation
                cv_fit <- ncvreg::cv.ncvsurv(
                    X = X,
                    y = Y,
                    penalty = penalty_type,
                    gamma = gamma_val,
                    alpha = self$options$alpha,
                    nfolds = self$options$cv_folds,
                    standardize = self$options$standardize,
                    returnY = FALSE
                )

                # Get optimal lambda (fall back to lambda.min if 1se not available)
                lambda_type <- self$options$lambda_type
                private$.lambda_1se_fallback <- FALSE
                if (lambda_type == "1se" && !is.null(cv_fit$lambda.1se) && length(cv_fit$lambda.1se) == 1) {
                    lambda_opt <- cv_fit$lambda.1se
                } else {
                    lambda_opt <- cv_fit$lambda.min
                    if (lambda_type == "1se") {
                        private$.lambda_1se_fallback <- TRUE
                    }
                }

                final_fit <- cv_fit$fit

                private$.cv_fit <- cv_fit
                private$.final_fit <- final_fit
                private$.lambda_opt <- lambda_opt
                private$.X <- X
                private$.Y <- Y

            }, error = function(e) {
                stop(paste("Error in", self$options$penalty, "Cox regression:", e$message))
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

            penalty_name <- private$.format_penalty(self$options$penalty)
            cv_error <- min(cv_fit$cve, na.rm = TRUE)

            coeffs <- coef(final_fit, lambda = lambda_opt)
            n_selected <- as.integer(sum(coeffs != 0))

            # Compute C-index from linear predictor (cached for reuse)
            cindex <- NA_real_
            if (n_selected > 0) {
                cindex <- tryCatch({
                    lp <- as.numeric(private$.X %*% coeffs)
                    # Higher LP = higher hazard = worse prognosis, so reverse=TRUE
                    ci <- survival::concordance(private$.Y ~ lp, reverse = TRUE)
                    as.numeric(ci$concordance)
                }, error = function(e) NA_real_)
            }
            private$.cindex_cached <- cindex

            table$addRow(rowKey = 1, values = list(
                penalty = penalty_name,
                lambda_selected = as.numeric(lambda_opt),
                cv_error = as.numeric(cv_error),
                n_selected = n_selected,
                c_index = as.numeric(if (!is.na(cindex)) cindex else NA_real_)
            ))
        },

        .populate_selected_variables = function() {
            table <- self$results$selected_variables
            final_fit <- private$.final_fit
            lambda_opt <- private$.lambda_opt

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

            lambdas <- cv_fit$lambda
            cv_errors <- cv_fit$cve
            cv_ses <- cv_fit$cvse

            n_vars <- apply(cv_fit$fit$beta, 2, function(x) sum(x != 0))

            # Show key lambda values: first, min, 1se (if available), evenly spaced, last
            n_total <- length(lambdas)
            min_idx <- which.min(cv_errors)

            key_indices <- c(1, min_idx)

            # Add 1se lambda index if available
            if (!is.null(cv_fit$lambda.1se) && length(cv_fit$lambda.1se) == 1) {
                se_idx <- which.min(abs(lambdas - cv_fit$lambda.1se))
                key_indices <- c(key_indices, se_idx)
            }

            # Add evenly spaced indices for context (up to ~8 total rows)
            if (n_total > 6) {
                spacing <- max(1, floor(n_total / 6))
                evenly <- seq(1, n_total, by = spacing)
                key_indices <- c(key_indices, evenly)
            }

            key_indices <- c(key_indices, n_total)
            key_indices <- sort(unique(key_indices))

            # Cap at 10 rows for readability
            if (length(key_indices) > 10) {
                key_indices <- key_indices[round(seq(1, length(key_indices), length.out = 10))]
            }

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

            lambda_min <- cv_fit$lambda.min
            lambda_1se <- cv_fit$lambda.1se

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

                c_index <- NA_real_
                if (n_vars > 0) {
                    c_index <- tryCatch({
                        lp <- as.numeric(private$.X %*% beta_at_lambda)
                        ci <- survival::concordance(private$.Y ~ lp, reverse = TRUE)
                        as.numeric(ci$concordance)
                    }, error = function(e) NA_real_)
                }

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

            table$setNote('aic_note',
                'AIC is computed from an unpenalized Cox refit on the selected variables. Values are approximate and intended for relative comparison only.')
        },

        .populate_convergence_info = function() {
            table <- self$results$convergence_info
            final_fit <- private$.final_fit

            # ncvreg stores per-lambda iterations as a vector; extract at selected lambda
            n_iter <- tryCatch({
                iter_vec <- final_fit$iter
                if (length(iter_vec) > 1) {
                    lambda_idx <- which.min(abs(final_fit$lambda - private$.lambda_opt))
                    as.integer(iter_vec[lambda_idx])
                } else {
                    as.integer(iter_vec)
                }
            }, error = function(e) NA_integer_)
            tol <- tryCatch(
                as.numeric(final_fit$eps),
                error = function(e) NA_real_
            )
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

            coeffs <- coef(final_fit, lambda = lambda_opt)
            var_names <- colnames(private$.X)
            if (is.null(var_names)) var_names <- paste0("V", seq_along(coeffs))
            importance <- abs(as.numeric(coeffs))

            ranks <- rank(-importance, ties.method = "first")
            max_imp <- max(importance, na.rm = TRUE)
            relative_importance <- if (max_imp > 0) (importance / max_imp) * 100 else rep(0, length(importance))

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

            if (!self$options$standardize) {
                table$setNote('scale_note',
                    'Standardization is off. Importance scores reflect raw coefficient magnitude and may not be comparable across variables with different scales.')
            }
        },

        # ── Protobuf-safe plot state management ──────────────────────
        # Extract plain numeric vectors into state so plots persist
        # when re-opening saved .omv files. Never store model objects.

        .create_plots = function() {
            if (!is.null(private$.cv_fit) && !is.null(private$.final_fit)) {
                # Regularization path state
                tryCatch({
                    beta_matrix <- as.matrix(private$.final_fit$beta)
                    lambda_vec <- as.numeric(private$.final_fit$lambda)
                    var_names <- as.character(rownames(beta_matrix))
                    if (is.null(var_names)) var_names <- paste0("V", seq_len(nrow(beta_matrix)))

                    self$results$regularization_path$setState(list(
                        beta = as.data.frame(beta_matrix),
                        lambda = lambda_vec,
                        var_names = var_names,
                        lambda_opt = as.numeric(private$.lambda_opt),
                        penalty = as.character(self$options$penalty)
                    ))
                }, error = function(e) NULL)

                # CV error plot state
                tryCatch({
                    lambda_min <- as.numeric(private$.cv_fit$lambda.min)
                    lambda_1se <- if (!is.null(private$.cv_fit$lambda.1se) &&
                                      length(private$.cv_fit$lambda.1se) == 1) {
                        as.numeric(private$.cv_fit$lambda.1se)
                    } else {
                        NA_real_
                    }

                    self$results$cv_error_plot$setState(list(
                        lambda = as.numeric(private$.cv_fit$lambda),
                        cve = as.numeric(private$.cv_fit$cve),
                        cvse = as.numeric(private$.cv_fit$cvse),
                        lambda_min = lambda_min,
                        lambda_1se = lambda_1se,
                        lambda_opt = as.numeric(private$.lambda_opt)
                    ))
                }, error = function(e) NULL)
            }

            # Variable importance plot state
            if (!is.null(private$.final_fit) && self$options$variable_importance) {
                tryCatch({
                    coeffs <- as.numeric(coef(private$.final_fit, lambda = private$.lambda_opt))
                    var_names <- as.character(colnames(private$.X))
                    if (is.null(var_names)) var_names <- paste0("V", seq_along(coeffs))
                    importance <- abs(coeffs)

                    # Only keep non-zero variables
                    nz <- which(importance > 0)
                    if (length(nz) > 0) {
                        self$results$variable_selection_plot$setState(list(
                            var_names = var_names[nz],
                            importance = as.numeric(importance[nz])
                        ))
                    } else {
                        self$results$variable_selection_plot$setState(list(
                            var_names = character(0),
                            importance = numeric(0)
                        ))
                    }
                }, error = function(e) NULL)
            }
        },

        # ── ggplot2 render functions ─────────────────────────────────

        .plot_regularization_path = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$beta)) return(FALSE)

            beta_df <- state$beta
            lambda_vec <- state$lambda
            var_names <- state$var_names
            lambda_opt <- state$lambda_opt
            penalty <- state$penalty

            # Build long-format data for ggplot
            n_vars <- nrow(beta_df)
            n_lambda <- ncol(beta_df)
            log_lambda <- log(lambda_vec)

            plot_data <- data.frame(
                log_lambda = rep(log_lambda, each = n_vars),
                coefficient = as.numeric(unlist(beta_df)),
                variable = rep(var_names, times = n_lambda),
                stringsAsFactors = FALSE
            )

            p <- ggplot2::ggplot(plot_data,
                    ggplot2::aes(x = log_lambda, y = coefficient,
                                 group = variable, color = variable)) +
                ggplot2::geom_line(alpha = 0.7, show.legend = FALSE) +
                ggplot2::geom_vline(xintercept = log(lambda_opt),
                                    linetype = "dashed", color = "red", linewidth = 0.8) +
                ggplot2::labs(
                    title = paste(penalty, "Regularization Path"),
                    x = expression(log(lambda)),
                    y = "Coefficient"
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        .plot_cv_error = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$lambda)) return(FALSE)

            cv_data <- data.frame(
                log_lambda = log(state$lambda),
                cve = state$cve,
                upper = state$cve + state$cvse,
                lower = state$cve - state$cvse
            )

            p <- ggplot2::ggplot(cv_data, ggplot2::aes(x = log_lambda, y = cve)) +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                     alpha = 0.2, fill = "steelblue") +
                ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
                ggplot2::geom_point(color = "steelblue", size = 1)

            # Add reference lines
            if (!is.na(state$lambda_min)) {
                p <- p + ggplot2::geom_vline(xintercept = log(state$lambda_min),
                                             linetype = "dashed", color = "red", linewidth = 0.7)
            }
            if (!is.na(state$lambda_1se)) {
                p <- p + ggplot2::geom_vline(xintercept = log(state$lambda_1se),
                                             linetype = "dotted", color = "darkgreen", linewidth = 0.7)
            }

            # Mark selected lambda
            p <- p + ggplot2::geom_vline(xintercept = log(state$lambda_opt),
                                         linetype = "solid", color = "black", linewidth = 0.5)

            p <- p +
                ggplot2::labs(
                    title = "Cross-Validation Error",
                    x = expression(log(lambda)),
                    y = "Cross-Validation Error"
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        .plot_variable_selection = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$var_names)) return(FALSE)

            var_names <- state$var_names
            importance <- state$importance

            if (length(var_names) == 0 || length(importance) == 0) {
                plot.new()
                text(0.5, 0.5, "No variables selected", cex = 1.2)
                return(TRUE)
            }

            # Sort by importance and limit to top 30 for readability
            ord <- order(importance, decreasing = TRUE)
            n_show <- min(length(ord), 30)
            ord <- ord[seq_len(n_show)]

            plot_data <- data.frame(
                variable = factor(var_names[ord], levels = rev(var_names[ord])),
                importance = importance[ord],
                stringsAsFactors = FALSE
            )

            p <- ggplot2::ggplot(plot_data,
                    ggplot2::aes(x = variable, y = importance)) +
                ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = "Variable Importance",
                    x = NULL,
                    y = "Absolute Coefficient"
                ) +
                ggtheme

            print(p)
            return(TRUE)
        },

        # ── jmvcore::Notice helpers ───────────────────────────────────

        .insertNotice = function(name, type, content, position = 999) {
            tryCatch({
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = name,
                    type = type
                )
                notice$setContent(content)
                self$results$insert(position, notice)
            }, error = function(e) {
                # Silently skip if Notice serialization fails
                NULL
            })
        },

        .add_post_analysis_notices = function() {
            if (is.null(private$.final_fit) || is.null(private$.analysis_data)) return()

            data <- private$.analysis_data
            n <- nrow(data)
            n_events <- sum(data$event == 1)
            p <- ncol(private$.X)

            # LOW EVENTS: clinical threshold checks
            if (n_events < 10) {
                private$.insertNotice(
                    'lowEvents', jmvcore::NoticeType$STRONG_WARNING,
                    sprintf('Very few events (%d). Model estimates are highly unstable; interpret with extreme caution.', n_events),
                    position = 1
                )
            } else if (n_events < 20) {
                private$.insertNotice(
                    'moderateEvents', jmvcore::NoticeType$WARNING,
                    sprintf('Only %d events with %d predictors (EPV=%.1f). Consider reducing predictors or increasing sample size.', n_events, p, n_events / p),
                    position = 1
                )
            }

            # NO VARIABLES SELECTED
            coeffs <- coef(private$.final_fit, lambda = private$.lambda_opt)
            n_selected <- sum(coeffs != 0)
            if (n_selected == 0) {
                private$.insertNotice(
                    'noSelection', jmvcore::NoticeType$WARNING,
                    'No variables selected at the chosen lambda. Try lambda_type="min" or increase alpha toward 1.',
                    position = 1
                )
            }

            # C-INDEX BELOW 0.5 (worse than random) — use cached value
            cindex <- private$.cindex_cached

            if (!is.na(cindex) && cindex < 0.5) {
                private$.insertNotice(
                    'poorDiscrimination', jmvcore::NoticeType$STRONG_WARNING,
                    sprintf('C-index = %.3f (below 0.5). Model discrimination is worse than random; do not use for clinical prediction.', cindex),
                    position = 1
                )
            }

            # LAMBDA 1SE FALLBACK
            if (isTRUE(private$.lambda_1se_fallback)) {
                private$.insertNotice(
                    'lambda1seFallback', jmvcore::NoticeType$WARNING,
                    'Lambda 1-SE not available for this fit; using lambda.min instead. This typically selects more variables.',
                    position = 999
                )
            }

            # MISSING DATA EXCLUDED
            n_total <- nrow(self$data)
            n_excluded <- n_total - n
            if (n_excluded > 0) {
                pct <- round(100 * n_excluded / n_total, 1)
                private$.insertNotice(
                    'missingData', jmvcore::NoticeType$WARNING,
                    sprintf('%d rows (%.1f%%) excluded due to missing data. Analysis uses %d complete cases.', n_excluded, pct, n),
                    position = 999
                )
            }
        },

        # ── Model interpretation HTML ────────────────────────────────

        .create_model_interpretation = function() {
            if (is.null(private$.final_fit)) {
                return()
            }

            coeffs <- coef(private$.final_fit, lambda = private$.lambda_opt)
            var_names <- colnames(private$.X)
            if (is.null(var_names)) var_names <- paste0("V", seq_along(coeffs))
            n_selected <- sum(coeffs != 0)
            penalty <- self$options$penalty
            n_total <- length(private$.covariates)
            n_obs <- nrow(private$.analysis_data)
            n_events <- sum(private$.analysis_data$event == 1)

            # Use cached C-index from model summary
            cindex <- private$.cindex_cached

            html <- paste0(
                "<h4>Model Interpretation</h4>",
                "<p><strong>Variable Selection Results:</strong> ",
                "The ", private$.format_penalty(penalty), " penalty selected ", n_selected,
                " variables out of ", n_total, " total variables.</p>"
            )

            # Per-variable clinical interpretation for selected variables
            selected_idx <- which(coeffs != 0)
            if (length(selected_idx) > 0) {
                html <- paste0(html, "<h5>Selected Variable Interpretations:</h5><ul>")
                for (idx in selected_idx) {
                    coeff_val <- as.numeric(coeffs[idx])
                    hr_val <- exp(coeff_val)
                    direction <- if (coeff_val > 0) "increased" else "decreased"
                    pct_change <- abs(round((hr_val - 1) * 100, 1))
                    html <- paste0(html,
                        "<li><strong>", htmltools::htmlEscape(var_names[idx]),
                        ":</strong> HR = ", sprintf("%.3f", hr_val),
                        " — a one-unit increase is associated with ",
                        direction, " hazard by ", pct_change, "%.</li>"
                    )
                }
                html <- paste0(html, "</ul>")
            }

            html <- paste0(html,
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
                "<li>Standard errors and p-values are not reported for penalized regression. ",
                "Traditional inference is invalid after variable selection (post-selection inference).</li>",
                "</ul>"
            )

            # Report sentence (copy-ready)
            report_sentence <- private$.generate_report_sentence(
                penalty, n_selected, n_total, n_obs, n_events, cindex,
                coeffs, var_names, selected_idx
            )
            html <- paste0(html,
                "<h5>Report Sentence (copy-ready):</h5>",
                "<div style='background-color: #f8f9fa; border: 1px solid #dee2e6; ",
                "border-radius: 4px; padding: 10px; font-style: italic; margin: 8px 0;'>",
                report_sentence,
                "</div>"
            )

            self$results$model_interpretation$setContent(html)
        },

        .generate_report_sentence = function(penalty, n_selected, n_total,
                                              n_obs, n_events, cindex,
                                              coeffs, var_names, selected_idx) {
            penalty_full <- private$.format_penalty(penalty)

            if (n_selected == 0) {
                return(sprintf(
                    "%s Cox regression (n=%d, %d events) selected no variables at the chosen regularization level (lambda=%.4f).",
                    penalty_full, n_obs, n_events, as.numeric(private$.lambda_opt)
                ))
            }

            # Identify strongest predictor
            abs_coeffs <- abs(as.numeric(coeffs[selected_idx]))
            top_idx <- selected_idx[which.max(abs_coeffs)]
            top_name <- var_names[top_idx]
            top_hr <- exp(as.numeric(coeffs[top_idx]))

            cindex_str <- if (!is.na(cindex)) sprintf(", C-index=%.2f", cindex) else ""

            sprintf(
                "%s Cox regression (n=%d, %d events) selected %d of %d variables (lambda=%.4f%s). The strongest predictor was %s (HR=%.2f).",
                penalty_full, n_obs, n_events, n_selected, n_total,
                as.numeric(private$.lambda_opt), cindex_str,
                top_name, top_hr
            )
        },

        .append_warnings_html = function() {
            warnings <- private$.warnings_collected
            if (length(warnings) == 0) return()

            # Deduplicate
            warnings <- unique(warnings)

            warning_html <- paste0(
                "<h5 style='color: #856404;'>Analysis Warnings</h5>",
                "<ul style='background-color: #fff3cd; padding: 10px 10px 10px 30px; ",
                "border: 1px solid #ffeeba; border-radius: 4px; font-size: 12px;'>"
            )
            for (w in warnings) {
                warning_html <- paste0(warning_html,
                    "<li>", htmltools::htmlEscape(w), "</li>"
                )
            }
            warning_html <- paste0(warning_html, "</ul>")

            # Append to existing model interpretation content
            existing <- self$results$model_interpretation$content
            if (!is.null(existing) && nzchar(existing)) {
                self$results$model_interpretation$setContent(
                    paste0(existing, warning_html)
                )
            } else {
                self$results$model_interpretation$setContent(warning_html)
            }
        },

        # ── Data suitability assessment ──────────────────────────────

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
                                detail = "High collinearity. SCAD/MCP can be highly unstable under extreme collinearity. Consider using Elastic Net instead."
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
