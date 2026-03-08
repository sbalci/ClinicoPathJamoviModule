adaptivelassoClass <- R6::R6Class(
    "adaptivelassoClass",
    inherit = adaptivelassoBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$time) || is.null(self$options$event)) {
                private$.showMessage(
                    "<html>
                    <head>
                    <style>
                        h2 {color: #e74c3c;}
                        body {font-family: Arial, sans-serif; margin: 20px;}
                        .highlight {background-color: #f39c12; padding: 2px 4px; border-radius: 3px;}
                        .step {margin: 10px 0; padding: 8px; background-color: #ecf0f1; border-radius: 5px;}
                    </style>
                    </head>
                    <body>
                    <h2>Adaptive LASSO for Cox Models</h2>
                    <p><strong>Advanced penalized regression with data-driven variable selection</strong></p>
                    <div class='step'>
                    <strong>Required Data:</strong>
                    <ul>
                        <li><span class='highlight'>Time Variable</span>: Time to event or censoring</li>
                        <li><span class='highlight'>Event Indicator</span>: 0 = censored, 1 = event</li>
                        <li><span class='highlight'>Predictor Variables</span>: Candidate variables for selection</li>
                    </ul>
                    </div>
                    <p><em>Tip: If the event variable has multiple levels, choose the event of interest using Event Level.</em></p>
                    </body>
                    </html>"
                )
                return()
            }

            private$.initResults()
        },

        .run = function() {
            if (is.null(self$data) || is.null(self$options$time) || is.null(self$options$event) ||
                length(self$options$predictors) == 0) {
                return()
            }

            private$.hideMessage()

            # Prepare data and validate inputs
            cox_data <- private$.prepareData()
            if (is.null(cox_data)) {
                return()
            }

            # Check if required packages are available
            required_packages <- c("glmnet", "survival")
            missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
            if (length(missing_packages) > 0) {
                private$.showMessage(
                    paste0("<html><body><h3>Missing Required Packages</h3>",
                           "<p>Please install the following packages:</p>",
                           "<ul>", paste0("<li>", missing_packages, "</li>", collapse = ""), "</ul>",
                           "<p><code>install.packages(c('", paste(missing_packages, collapse = "', '"), "'))</code></p>",
                           "</body></html>")
                )
                return()
            }
            
            # Assess data suitability if requested
            if (self$options$suitabilityCheck) {
                private$.assessSuitability()
            }

            # Fit adaptive LASSO model
            tryCatch({
                adaptive_results <- private$.fitAdaptiveLasso(cox_data)
                if (!is.null(adaptive_results)) {
                    private$.populateResults(adaptive_results, cox_data)
                }
            }, error = function(e) {
                private$.showMessage(
                    paste0("<html><body><h3>Analysis Error</h3><p>",
                           "Error in adaptive LASSO fitting: ", e$message,
                           "</p><p>Try reducing the number of predictors or adjusting penalty settings.</p></body></html>")
                )
            })
        },

        .showMessage = function(message_html) {
            self$results$instructions$setVisible(TRUE)
            self$results$instructions$setContent(message_html)
        },

        .hideMessage = function() {
            self$results$instructions$setVisible(FALSE)
            self$results$instructions$setContent("")
        },

        .assessSuitability = function() {
            data <- self$data
            time_var <- self$options$time
            status_var <- self$options$event
            predictors <- self$options$predictors

            html <- self$results$suitabilityReport
            
            if (is.null(html)) return()
            
            # 1. Sample Size & Events
            total_n <- nrow(data)
            events <- 0
            if (!is.null(status_var) && status_var %in% names(data)) {
                # Try to count events roughly
                event_data <- data[[status_var]]
                if (is.numeric(event_data) || is.logical(event_data)) {
                    # If multiple unique non-zero values, this might be inaccurate without knowing event_level,
                    # but it gives a rough heuristic before encoding.
                    unique_vals <- sort(unique(event_data[!is.na(event_data)]))
                    if (length(unique_vals) == 2) {
                        events <- sum(event_data == max(unique_vals), na.rm = TRUE)
                    } else if (all(unique_vals %in% c(0,1))) {
                        events <- sum(event_data == 1, na.rm = TRUE)
                    } else if (!is.null(self$options$event_level)) {
                        events <- sum(event_data == suppressWarnings(as.numeric(self$options$event_level)), na.rm = TRUE)
                    } else {
                        events <- sum(event_data != 0, na.rm = TRUE) 
                    }
                } else {
                    event_chr <- as.character(event_data)
                    event_levels <- if(is.factor(event_data)) levels(event_data) else sort(unique(event_chr))
                    if (!is.null(self$options$event_level)) {
                        events <- sum(event_chr == self$options$event_level, na.rm = TRUE)
                    } else if (length(event_levels) == 2) {
                        events <- sum(event_chr == event_levels[2], na.rm = TRUE)
                    } else {
                        events <- NA
                    }
                }
            }
            n_vars <- length(predictors)
            epv <- if (!is.na(events) && n_vars > 0) events / n_vars else 0

            # Define warning colors
            epv_color <- ifelse(epv < 5, "red", ifelse(epv < 10, "orange", "green"))
            
            # 2. Missing Data
            missing_text <- ""
            complete_cases <- sum(complete.cases(data[, c(time_var, status_var, predictors), drop=FALSE]))
            missing_pct <- 100 * (1 - complete_cases/total_n)
            
            if (missing_pct > 10) {
                missing_text <- paste0("<div style='color: orange; margin-top: 5px;'>",
                                     "⚠️ <b>Note:</b> ", round(missing_pct, 1), "% of cases have missing data and will be excluded via listwise deletion.",
                                     "</div>")
            }

            # Construct HTML
            content <- paste0(
                "<html>
                <style>
                    .suit-box { border: 1px solid #ccc; padding: 10px; border-radius: 5px; margin-bottom: 10px; }
                    .suit-title { font-weight: bold; border-bottom: 1px solid #ddd; padding-bottom: 5px; margin-bottom: 5px; }
                    .metric { display: flex; justify-content: space-between; margin: 3px 0; }
                </style>
                <div class='suit-box'>
                    <div class='suit-title'>Dataset Assessment for Adaptive LASSO Cox</div>
                    
                    <div class='metric'>
                        <span><b>Total Screened:</b> ", total_n, "</span>
                        <span><b>Approx. Events:</b> ", if(is.na(events)) "Unknown" else events, "</span>
                    </div>
                    
                    <div class='metric'>
                        <span><b>Predictors (P):</b> ", n_vars, "</span>
                        <span><b style='color:", epv_color, "'>Events Per Variable (EPV): ", if(is.na(events)) "Unknown" else round(epv, 2), "</b></span>
                    </div>
                    
                    <div style='margin-top: 10px;'>
                        <b>Assessment:</b><br>",
                        if(is.na(events)) "<span style='color: orange;'>⚠️ <b>EPV Cannot Be Calculated:</b> Multiple event levels detected and no Event Level specified.</span>"
                        else if (epv < 5) 
                               "<span style='color: red;'>⚠️ <b>High Dimensionality:</b> EPV is very low. Regularization via Adaptive LASSO is strictly necessary to prevent severe overfitting.</span>"
                        else if (epv < 10) 
                               "<span style='color: orange;'>⚠️ <b>Moderate Dimensionality:</b> Adaptive LASSO will effectively shrink weak predictors and select the most relevant ones.</span>"
                        else 
                               "<span style='color: green;'>✓ <b>Adequate Events:</b> Sample size is sufficient for this number of predictors without regularization, but Adaptive LASSO may still improve parsimony.</span>",
                    "</div>",
                    missing_text,
                "</div></html>"
            )
            
            html$setContent(content)
        },

        .encodeEventIndicator = function(event_raw) {
            event_level <- self$options$event_level
            if (!is.null(event_level)) {
                event_level <- as.character(event_level)
                if (!nzchar(event_level)) {
                    event_level <- NULL
                }
            }

            if (is.factor(event_raw) || is.character(event_raw)) {
                event_chr <- as.character(event_raw)
                event_levels <- if (is.factor(event_raw)) levels(event_raw) else sort(unique(event_chr))

                if (!is.null(event_level)) {
                    if (!(event_level %in% event_levels)) {
                        return(list(
                            ok = FALSE,
                            message = paste0(
                                "<html><body><h3>Invalid Event Level</h3><p>Selected event level ('",
                                event_level,
                                "') is not present in the event variable.</p></body></html>"
                            )
                        ))
                    }
                    event_values <- as.numeric(event_chr == event_level)
                    return(list(ok = TRUE, values = event_values, event_level = event_level))
                }

                if (length(event_levels) == 2) {
                    default_level <- event_levels[2]
                    event_values <- as.numeric(event_chr == default_level)
                    return(list(ok = TRUE, values = event_values, event_level = default_level))
                }

                return(list(
                    ok = FALSE,
                    message = paste0(
                        "<html><body><h3>Event Level Required</h3>",
                        "<p>Event variable has ",
                        length(event_levels),
                        " levels. Please choose an Event Level to define the event of interest.</p></body></html>"
                    )
                ))
            }

            # Numeric/logical event variable
            event_num <- jmvcore::toNumeric(event_raw)
            unique_vals <- sort(unique(event_num[!is.na(event_num)]))
            if (length(unique_vals) == 0) {
                return(list(
                    ok = FALSE,
                    message = "<html><body><h3>Invalid Event Variable</h3><p>No non-missing values found.</p></body></html>"
                ))
            }

            if (!is.null(event_level)) {
                event_level_num <- suppressWarnings(as.numeric(event_level))
                if (is.na(event_level_num)) {
                    return(list(
                        ok = FALSE,
                        message = "<html><body><h3>Invalid Event Level</h3><p>Event Level must be numeric for numeric event variables.</p></body></html>"
                    ))
                }
                event_values <- as.numeric(event_num == event_level_num)
                return(list(ok = TRUE, values = event_values, event_level = as.character(event_level_num)))
            }

            if (all(unique_vals %in% c(0, 1))) {
                return(list(ok = TRUE, values = as.numeric(event_num), event_level = "1"))
            }

            if (length(unique_vals) == 2) {
                default_level_num <- max(unique_vals)
                event_values <- as.numeric(event_num == default_level_num)
                return(list(ok = TRUE, values = event_values, event_level = as.character(default_level_num)))
            }

            return(list(
                ok = FALSE,
                message = paste0(
                    "<html><body><h3>Event Level Required</h3><p>Numeric event variable has non-binary values (",
                    paste(unique_vals, collapse = ", "),
                    "). Please set Event Level to define the event of interest.</p></body></html>"
                )
            ))
        },

        .buildLambdaSequence = function() {
            mode <- self$options$lambda_sequence

            if (mode == "auto") {
                return(NULL)
            }

            if (mode == "single") {
                lambda_single <- as.numeric(self$options$lambda_single)
                if (is.na(lambda_single) || lambda_single <= 0) {
                    stop("Single lambda value must be > 0.")
                }
                return(lambda_single)
            }

            if (mode == "custom") {
                lambda_max <- as.numeric(self$options$lambda_custom_max)
                lambda_min <- as.numeric(self$options$lambda_custom_min)
                n_lambda <- as.integer(self$options$n_lambda)

                if (is.na(lambda_max) || is.na(lambda_min) || lambda_max <= lambda_min || lambda_min <= 0) {
                    stop("For custom lambda, require: lambda_max > lambda_min > 0.")
                }
                return(exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda)))
            }

            NULL
        },

        .assignRiskGroups = function(lp, requested_groups) {
            requested_groups <- max(2, as.integer(requested_groups))
            probs <- seq(0, 1, length.out = requested_groups + 1)
            breaks <- as.numeric(stats::quantile(lp, probs = probs, na.rm = TRUE, type = 8))
            breaks <- unique(breaks)

            if (length(breaks) < 2) {
                breaks <- c(min(lp, na.rm = TRUE) - 1e-8, max(lp, na.rm = TRUE) + 1e-8)
            }
            if (length(breaks) == 2 && diff(breaks) <= 0) {
                breaks <- c(breaks[1] - 1e-8, breaks[2] + 1e-8)
            }

            breaks[1] <- breaks[1] - 1e-8
            breaks[length(breaks)] <- breaks[length(breaks)] + 1e-8

            n_groups <- length(breaks) - 1
            labels <- paste0("Risk Group ", seq_len(n_groups))
            groups <- cut(lp, breaks = breaks, labels = labels, include.lowest = TRUE)

            list(groups = groups, labels = labels, n_groups = n_groups)
        },

        .prepareData = function() {
            data <- self$data

            # Get variable names
            time_var <- self$options$time
            event_var <- self$options$event
            pred_vars <- self$options$predictors
            strata_var <- self$options$strata
            if (is.null(strata_var) || !nzchar(strata_var)) {
                strata_var <- NULL
            }

            if (is.null(time_var) || is.null(event_var) || length(pred_vars) == 0) {
                return(NULL)
            }

            # Create analysis dataset
            vars_needed <- c(time_var, event_var, pred_vars)
            if (!is.null(strata_var)) {
                vars_needed <- c(vars_needed, strata_var)
            }

            analysis_data <- data[, vars_needed, drop = FALSE]
            analysis_data <- na.omit(analysis_data)

            # Prepare survival object
            time_values <- jmvcore::toNumeric(analysis_data[[time_var]])
            if (any(!is.finite(time_values), na.rm = TRUE) || any(time_values <= 0, na.rm = TRUE)) {
                private$.showMessage(
                    "<html><body><h3>Invalid Time Values</h3>
                    <p>All time values must be finite and positive for survival analysis.</p></body></html>"
                )
                return(NULL)
            }

            event_parse <- private$.encodeEventIndicator(analysis_data[[event_var]])
            if (!isTRUE(event_parse$ok)) {
                private$.showMessage(event_parse$message)
                return(NULL)
            }
            event_values <- event_parse$values

            n_events <- sum(event_values == 1, na.rm = TRUE)
            if (n_events < 3) {
                private$.showMessage(
                    "<html><body><h3>Insufficient Events</h3>
                    <p>Need at least 3 events for survival modeling.</p></body></html>"
                )
                return(NULL)
            }

            # Prepare predictor matrix
            pred_data <- analysis_data[, pred_vars, drop = FALSE]
            x_matrix <- model.matrix(~ . - 1, data = pred_data)

            # Remove constant columns (zero variance) before standardization
            col_vars <- apply(x_matrix, 2, var, na.rm = TRUE)
            constant_cols <- which(col_vars == 0 | is.na(col_vars))
            if (length(constant_cols) > 0) {
                x_matrix <- x_matrix[, -constant_cols, drop = FALSE]
            }

            if (ncol(x_matrix) == 0) {
                private$.showMessage(
                    "<html><body><h3>No Valid Predictors</h3>
                    <p>All predictors are constant after processing. Check variable coding.</p></body></html>"
                )
                return(NULL)
            }

            min_required <- ncol(x_matrix) + 10
            if (nrow(analysis_data) < min_required) {
                private$.showMessage(
                    paste0(
                        "<html><body><h3>Insufficient Data</h3>",
                        "<p>Need at least p + 10 complete observations for reliable adaptive LASSO after factor expansion.</p>",
                        "<p>Current complete observations: ", nrow(analysis_data),
                        "; required minimum: ", min_required, ".</p></body></html>"
                    )
                )
                return(NULL)
            }

            if (self$options$standardize) {
                x_matrix <- scale(x_matrix)
                x_matrix[is.nan(x_matrix)] <- 0
            }

            requireNamespace("survival", quietly = TRUE)
            surv_obj <- survival::Surv(time_values, event_values)

            strata_values <- NULL
            if (!is.null(strata_var)) {
                strata_values <- as.factor(analysis_data[[strata_var]])
            }

            return(list(
                data = analysis_data,
                x = x_matrix,
                y = surv_obj,
                time = time_values,
                event = event_values,
                n_obs = nrow(analysis_data),
                n_events = n_events,
                pred_names = colnames(x_matrix),
                original_pred_names = pred_vars,
                strata = strata_values,
                event_level_used = event_parse$event_level
            ))
        },

        .fitAdaptiveLasso = function(cox_data) {
            requireNamespace("glmnet", quietly = TRUE)
            requireNamespace("survival", quietly = TRUE)

            # Stratified Cox support in glmnet
            y_glmnet <- cox_data$y
            if (!is.null(cox_data$strata)) {
                y_glmnet <- glmnet::stratifySurv(y_glmnet, strata = cox_data$strata)
            }

            parallel_enabled <- FALSE
            cluster_obj <- NULL
            if (isTRUE(self$options$parallel_computing) &&
                requireNamespace("doParallel", quietly = TRUE) &&
                requireNamespace("foreach", quietly = TRUE)) {
                available_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
                if (is.na(available_cores) || available_cores < 1) {
                    available_cores <- 1
                }
                n_cores <- max(1, min(as.integer(self$options$n_cores), available_cores))
                if (n_cores > 1) {
                    cluster_obj <- parallel::makePSOCKcluster(n_cores)
                    doParallel::registerDoParallel(cluster_obj)
                    parallel_enabled <- TRUE
                }
            }
            on.exit({
                if (!is.null(cluster_obj)) {
                    parallel::stopCluster(cluster_obj)
                    foreach::registerDoSEQ()
                }
            }, add = TRUE)

            # Calculate adaptive weights
            adaptive_weights <- private$.calculateAdaptiveWeights(cox_data)

            # Validate cv_measure: glmnet only supports "deviance" and "C" for Cox
            cv_measure <- self$options$cv_measure
            if (!(cv_measure %in% c("deviance", "C"))) {
                cv_measure <- "deviance"  # Safe fallback
            }

            # Set up lambda sequence using user options
            lambda_seq <- private$.buildLambdaSequence()

            # Fit adaptive LASSO with cross-validation
            set.seed(self$options$random_seed)
            nfolds <- min(as.integer(self$options$cv_folds), max(3, cox_data$n_obs - 1))

            cv_args <- list(
                x = cox_data$x,
                y = y_glmnet,
                family = "cox",
                alpha = self$options$alpha,
                penalty.factor = adaptive_weights,
                nfolds = nfolds,
                lambda = lambda_seq,
                type.measure = cv_measure,
                thresh = self$options$convergence_threshold,
                maxit = self$options$max_iterations,
                standardize = FALSE,  # Already standardized if requested
                parallel = parallel_enabled
            )
            if (is.null(lambda_seq)) {
                cv_args$nlambda <- self$options$n_lambda
                cv_args$lambda.min.ratio <- self$options$lambda_min_ratio
            }
            single_lambda_mode <- !is.null(lambda_seq) && length(lambda_seq) == 1

            if (single_lambda_mode) {
                full_fit <- glmnet::glmnet(
                    x = cox_data$x,
                    y = y_glmnet,
                    family = "cox",
                    alpha = self$options$alpha,
                    penalty.factor = adaptive_weights,
                    lambda = lambda_seq,
                    thresh = self$options$convergence_threshold,
                    maxit = self$options$max_iterations,
                    standardize = FALSE
                )

                cv_fit <- list(
                    lambda = as.numeric(lambda_seq),
                    lambda.min = as.numeric(lambda_seq),
                    lambda.1se = as.numeric(lambda_seq),
                    cvm = rep(NA_real_, 1),
                    cvsd = rep(NA_real_, 1),
                    cvup = rep(NA_real_, 1),
                    cvlo = rep(NA_real_, 1),
                    glmnet.fit = full_fit
                )
            } else {
                cv_fit <- do.call(glmnet::cv.glmnet, cv_args)

                # Fit full model for path
                full_fit <- glmnet::glmnet(
                    x = cox_data$x,
                    y = y_glmnet,
                    family = "cox",
                    alpha = self$options$alpha,
                    penalty.factor = adaptive_weights,
                    lambda = cv_fit$lambda,
                    thresh = self$options$convergence_threshold,
                    maxit = self$options$max_iterations,
                    standardize = FALSE
                )
            }

            # Extract coefficients at optimal lambda
            lambda_min <- cv_fit$lambda.min
            lambda_1se <- cv_fit$lambda.1se

            coef_min <- as.matrix(coef(full_fit, s = lambda_min))
            coef_1se <- as.matrix(coef(full_fit, s = lambda_1se))

            # Model diagnostics (pass tie_method for refitted Cox)
            diagnostics <- private$.calculateDiagnostics(cox_data, coef_min, lambda_min)

            # Stability selection if requested
            stability_results <- NULL
            if (self$options$stability_selection) {
                stability_results <- private$.stabilitySelection(
                    cox_data = cox_data,
                    y_glmnet = y_glmnet,
                    adaptive_weights = adaptive_weights,
                    parallel_enabled = parallel_enabled
                )
            }

            return(list(
                cv_fit = cv_fit,
                full_fit = full_fit,
                lambda_min = lambda_min,
                lambda_1se = lambda_1se,
                coef_min = coef_min,
                coef_1se = coef_1se,
                adaptive_weights = adaptive_weights,
                diagnostics = diagnostics,
                stability = stability_results
            ))
        },

        .calculateAdaptiveWeights = function(cox_data) {
            method <- self$options$weight_method
            gamma <- self$options$gamma
            n_vars <- ncol(cox_data$x)

            if (method == "equal") {
                return(rep(1, n_vars))
            }

            initial_coefs <- rep(0, n_vars)
            y_glmnet <- cox_data$y
            if (!is.null(cox_data$strata)) {
                y_glmnet <- glmnet::stratifySurv(y_glmnet, strata = cox_data$strata)
            }

            if (method == "ridge") {
                # Ridge regression for initial estimates
                ridge_fit <- glmnet::glmnet(
                    x = cox_data$x,
                    y = y_glmnet,
                    family = "cox",
                    alpha = 0,  # Pure ridge
                    standardize = FALSE
                )

                # Use lambda that gives reasonable shrinkage
                lambda_ridge <- ridge_fit$lambda[length(ridge_fit$lambda) %/% 4]
                initial_coefs <- as.vector(coef(ridge_fit, s = lambda_ridge))

            } else if (method == "univariate") {
                # Univariate Cox regressions
                for (i in 1:n_vars) {
                    tryCatch({
                        if (is.null(cox_data$strata)) {
                            uni_fit <- survival::coxph(cox_data$y ~ cox_data$x[, i])
                        } else {
                            uni_fit <- survival::coxph(
                                cox_data$y ~ cox_data$x[, i] + survival::strata(cox_data$strata)
                            )
                        }
                        initial_coefs[i] <- coef(uni_fit)[1]
                    }, error = function(e) {
                        initial_coefs[i] <<- 0
                    })
                }

            } else if (method == "cox") {
                # Full Cox model (if feasible)
                if (ncol(cox_data$x) < nrow(cox_data$x) / 3) {
                    tryCatch({
                        if (is.null(cox_data$strata)) {
                            cox_fit <- survival::coxph(cox_data$y ~ cox_data$x)
                        } else {
                            cox_fit <- survival::coxph(
                                cox_data$y ~ cox_data$x + survival::strata(cox_data$strata)
                            )
                        }
                        initial_coefs <- coef(cox_fit)
                    }, error = function(e) {
                        # Fallback to ridge if Cox fails
                        ridge_fit <- glmnet::glmnet(
                            cox_data$x, y_glmnet,
                            family = "cox",
                            alpha = 0
                        )
                        lambda_ridge <- ridge_fit$lambda[length(ridge_fit$lambda) %/% 4]
                        initial_coefs <<- as.vector(coef(ridge_fit, s = lambda_ridge))
                    })
                } else {
                    # Too many variables, use ridge instead
                    ridge_fit <- glmnet::glmnet(
                        cox_data$x, y_glmnet,
                        family = "cox",
                        alpha = 0
                    )
                    lambda_ridge <- ridge_fit$lambda[length(ridge_fit$lambda) %/% 4]
                    initial_coefs <- as.vector(coef(ridge_fit, s = lambda_ridge))
                }

            } else if (method == "correlation") {
                # Marginal correlations: use univariate Cox z-statistics
                for (i in 1:n_vars) {
                    tryCatch({
                        if (is.null(cox_data$strata)) {
                            uni_fit <- survival::coxph(cox_data$y ~ cox_data$x[, i])
                        } else {
                            uni_fit <- survival::coxph(
                                cox_data$y ~ cox_data$x[, i] + survival::strata(cox_data$strata)
                            )
                        }
                        z_stat <- coef(uni_fit) / sqrt(diag(vcov(uni_fit)))
                        initial_coefs[i] <- as.numeric(z_stat[1])
                    }, error = function(e) {
                        initial_coefs[i] <<- 0
                    })
                }
            }

            # Calculate adaptive weights
            initial_coefs[is.na(initial_coefs)] <- 0
            weights <- 1 / (abs(initial_coefs) + 1e-8)^gamma

            # Normalize weights to prevent numerical issues
            max_w <- max(weights, na.rm = TRUE)
            if (!is.finite(max_w) || max_w <= 0) {
                weights <- rep(1, n_vars)
            } else {
                weights <- weights / max_w
            }

            return(weights)
        },

        .calculateDiagnostics = function(cox_data, coefficients, lambda) {
            diagnostics <- list()

            # Selected variables
            selected_vars <- which(abs(coefficients) > 1e-8)
            diagnostics$n_selected <- length(selected_vars)
            diagnostics$selected_vars <- cox_data$pred_names[selected_vars]
            diagnostics$selected_idx <- selected_vars

            # Model fit statistics via refitted unpenalized Cox
            if (length(selected_vars) > 0) {
                tryCatch({
                    selected_x <- cox_data$x[, selected_vars, drop = FALSE]
                    # Pass tie_method to coxph for consistency
                    tie_method <- self$options$tie_method
                    if (is.null(cox_data$strata)) {
                        cox_fit <- survival::coxph(
                            cox_data$y ~ selected_x,
                            ties = tie_method
                        )
                    } else {
                        cox_fit <- survival::coxph(
                            cox_data$y ~ selected_x + survival::strata(cox_data$strata),
                            ties = tie_method
                        )
                    }

                    diagnostics$concordance <- cox_fit$concordance["concordance"]
                    diagnostics$loglik <- cox_fit$loglik[2]
                    diagnostics$aic <- -2 * cox_fit$loglik[2] + 2 * length(selected_vars)

                    # Store refitted Cox for SE/CI extraction
                    diagnostics$cox_fit <- cox_fit
                    diagnostics$cox_coefs <- coef(cox_fit)
                    diagnostics$cox_se <- sqrt(diag(vcov(cox_fit)))

                    # Proportional hazards test if requested
                    if (self$options$proportional_hazards && length(selected_vars) >= 1) {
                        ph_test <- survival::cox.zph(cox_fit)
                        diagnostics$ph_test <- ph_test
                        if ("GLOBAL" %in% rownames(ph_test$table)) {
                            diagnostics$ph_global_p <- ph_test$table["GLOBAL", "p"]
                        } else {
                            diagnostics$ph_global_p <- ph_test$table[nrow(ph_test$table), "p"]
                        }
                    }

                    if (self$options$influence_diagnostics) {
                        dfbeta_resid <- residuals(cox_fit, type = "dfbeta")
                        if (is.matrix(dfbeta_resid)) {
                            max_abs_dfbeta <- apply(abs(dfbeta_resid), 1, max, na.rm = TRUE)
                        } else {
                            max_abs_dfbeta <- abs(as.numeric(dfbeta_resid))
                        }
                        infl_cutoff <- 2 / sqrt(cox_data$n_obs)
                        diagnostics$influence_max <- max(max_abs_dfbeta, na.rm = TRUE)
                        diagnostics$influence_n <- sum(max_abs_dfbeta > infl_cutoff, na.rm = TRUE)
                        diagnostics$influence_cutoff <- infl_cutoff
                    }

                }, error = function(e) {
                    diagnostics$concordance <<- NA
                    diagnostics$loglik <<- NA
                    diagnostics$aic <<- NA
                })
            }

            return(diagnostics)
        },

        .stabilitySelection = function(cox_data, y_glmnet, adaptive_weights, parallel_enabled = FALSE) {
            n_boot <- self$options$bootstrap_samples
            subsample_ratio <- self$options$subsampling_ratio
            n_vars <- ncol(cox_data$x)

            selection_matrix <- matrix(0, nrow = n_boot, ncol = n_vars)

            set.seed(self$options$random_seed)

            for (b in 1:n_boot) {
                # Bootstrap/subsample data
                n_sub <- max(3, floor(nrow(cox_data$x) * subsample_ratio))
                boot_idx <- sample(nrow(cox_data$x), n_sub, replace = FALSE)

                boot_x <- cox_data$x[boot_idx, , drop = FALSE]
                boot_y <- y_glmnet[boot_idx, ]

                tryCatch({
                    # Fit adaptive LASSO on bootstrap sample
                    boot_fit <- glmnet::glmnet(
                        x = boot_x,
                        y = boot_y,
                        family = "cox",
                        alpha = self$options$alpha,
                        penalty.factor = adaptive_weights,
                        standardize = FALSE
                    )

                    # Use cross-validation to select lambda
                    boot_nfold <- min(5, self$options$cv_folds, max(3, n_sub - 1))
                    boot_cv <- glmnet::cv.glmnet(
                        x = boot_x,
                        y = boot_y,
                        family = "cox",
                        alpha = self$options$alpha,
                        penalty.factor = adaptive_weights,
                        nfolds = boot_nfold,
                        standardize = FALSE,
                        parallel = parallel_enabled
                    )

                    # Extract selected variables
                    boot_coef <- as.vector(coef(boot_fit, s = boot_cv$lambda.1se))
                    selection_matrix[b, ] <- as.numeric(abs(boot_coef) > 1e-8)

                }, error = function(e) {
                    # Skip this bootstrap sample if it fails
                })
            }

            # Calculate selection frequencies
            selection_freq <- colMeans(selection_matrix)

            # Stable selection based on threshold
            stable_selection <- selection_freq >= self$options$stability_threshold

            return(list(
                selection_frequencies = selection_freq,
                stable_variables = cox_data$pred_names[stable_selection],
                selection_matrix = selection_matrix
            ))
        },

        .populateResults = function(adaptive_results, cox_data) {
            # Coefficients table
            if (self$options$show_coefficients) {
                private$.populateCoefficients(adaptive_results, cox_data)
            }

            # Selection path
            if (self$options$show_selection_path) {
                private$.populateSelectionPath(adaptive_results)
            }

            # Cross-validation results
            if (self$options$show_cv_results) {
                private$.populateCVResults(adaptive_results)
            }

            # Stability selection results
            if (self$options$stability_selection && !is.null(adaptive_results$stability)) {
                private$.populateStabilityResults(adaptive_results)
            }

            # Model diagnostics
            if (self$options$show_diagnostics) {
                private$.populateDiagnostics(adaptive_results)
            }

            # Performance metrics
            if (self$options$goodness_of_fit) {
                private$.populatePerformance(adaptive_results, cox_data)
            }

            # Risk group analysis
            private$.populateRiskGroups(adaptive_results, cox_data)

            # Predictions table
            if (self$options$baseline_survival) {
                private$.populatePredictions(adaptive_results, cox_data)
            } else {
                self$results$predictions$deleteRows()
            }

            # Plots -- set state data for all enabled plots
            if (self$options$plot_selection_path) {
                private$.plotSelectionPath(adaptive_results)
            }

            if (self$options$plot_cv_curve) {
                private$.plotCVCurve(adaptive_results)
            }

            if (self$options$plot_stability && !is.null(adaptive_results$stability)) {
                private$.plotStability(adaptive_results)
            }

            if (self$options$plot_survival_curves) {
                private$.plotSurvivalCurves(adaptive_results, cox_data)
            }

            if (self$options$plot_baseline_hazard && self$options$baseline_survival) {
                private$.plotBaselineHazard(adaptive_results, cox_data)
            } else {
                self$results$baselineHazardPlot$setState(NULL)
            }

            if (self$options$plot_diagnostics) {
                private$.plotDiagnosticsData(adaptive_results, cox_data)
            }
        },

        .populateCoefficients = function(adaptive_results, cox_data) {
            table <- self$results$coefficients
            table$deleteRows()

            coef_vec <- as.vector(adaptive_results$coef_min)
            selected_idx <- which(abs(coef_vec) > 1e-8)

            if (length(selected_idx) == 0) {
                table$addRow(rowKey = 1, values = list(
                    variable = "No variables selected",
                    coefficient = NA,
                    exp_coefficient = NA,
                    std_error = NA,
                    lower_ci = NA,
                    upper_ci = NA,
                    adaptive_weight = NA
                ))
                return()
            }

            # Get SEs and CIs from refitted unpenalized Cox model
            diag <- adaptive_results$diagnostics
            has_refit <- !is.null(diag$cox_coefs) &&
                !is.null(diag$cox_se) &&
                length(diag$cox_coefs) == length(selected_idx) &&
                length(diag$cox_se) == length(selected_idx)

            for (j in seq_along(selected_idx)) {
                idx <- selected_idx[j]
                penalized_coef <- coef_vec[idx]
                coef_val <- if (has_refit) as.numeric(diag$cox_coefs[j]) else penalized_coef

                if (has_refit) {
                    se_val <- diag$cox_se[j]
                    lower <- coef_val - 1.96 * se_val
                    upper <- coef_val + 1.96 * se_val
                } else {
                    se_val <- NA
                    lower <- NA
                    upper <- NA
                }

                table$addRow(rowKey = j, values = list(
                    variable = cox_data$pred_names[idx],
                    coefficient = coef_val,
                    exp_coefficient = exp(coef_val),
                    std_error = se_val,
                    lower_ci = lower,
                    upper_ci = upper,
                    adaptive_weight = adaptive_results$adaptive_weights[idx]
                ))
            }

            table$setNote(
                "se_note",
                "Displayed coefficients/SE/CI are from unpenalized Cox refit on selected variables. Adaptive weights come from penalized fitting."
            )
        },

        .populateSelectionPath = function(adaptive_results) {
            table <- self$results$selectionPath
            table$deleteRows()

            cv_fit <- adaptive_results$cv_fit
            full_fit <- adaptive_results$full_fit

            # Create path summary
            n_lambda <- length(cv_fit$lambda)
            n_steps <- min(20, n_lambda)
            step_indices <- round(seq(1, n_lambda, length.out = n_steps))

            for (s in seq_along(step_indices)) {
                i <- step_indices[s]
                coefs <- as.vector(coef(full_fit, s = cv_fit$lambda[i]))
                n_sel <- sum(abs(coefs) > 1e-8)

                table$addRow(rowKey = s, values = list(
                    step = s,
                    lambda = cv_fit$lambda[i],
                    n_selected = n_sel,
                    deviance = cv_fit$cvm[i],
                    cv_error = cv_fit$cvsd[i]
                ))
            }

            metric_note <- if (self$options$cv_measure == "C") {
                "CV Metric column reports Harrell's C-index (higher is better)."
            } else {
                "CV Metric column reports partial likelihood deviance (lower is better)."
            }
            table$setNote("metric_note", metric_note)
        },

        .populateCVResults = function(adaptive_results) {
            table <- self$results$cvResults
            table$deleteRows()

            cv_fit <- adaptive_results$cv_fit
            min_idx <- which(cv_fit$lambda == cv_fit$lambda.min)
            se_idx <- which(cv_fit$lambda == cv_fit$lambda.1se)

            no_cv <- length(cv_fit$lambda) == 1 && all(is.na(cv_fit$cvm))
            criterion_label <- if (no_cv) {
                "Single Lambda (No CV)"
            } else if (self$options$cv_measure == "C") {
                "Cross-Validation (C-index)"
            } else {
                "Cross-Validation (Deviance)"
            }

            table$addRow(rowKey = 1, values = list(
                criterion = criterion_label,
                lambda_min = cv_fit$lambda.min,
                lambda_1se = cv_fit$lambda.1se,
                cv_error_min = if (length(min_idx) > 0) cv_fit$cvm[min_idx[1]] else NA,
                cv_error_1se = if (length(se_idx) > 0) cv_fit$cvm[se_idx[1]] else NA,
                n_variables_min = as.integer(sum(abs(adaptive_results$coef_min) > 1e-8)),
                n_variables_1se = as.integer(sum(abs(adaptive_results$coef_1se) > 1e-8))
            ))
        },

        .populateStabilityResults = function(adaptive_results) {
            table <- self$results$stabilityResults
            table$deleteRows()

            stability <- adaptive_results$stability
            var_names <- rownames(adaptive_results$cv_fit$glmnet.fit$beta)
            threshold <- self$options$stability_threshold
            p <- length(stability$selection_frequencies)
            if (length(var_names) != p) {
                var_names <- paste0("V", seq_len(p))
            }

            # Compute per-family error rate bound (Meinshausen & Buhlmann 2010)
            # PFER <= q^2 / ((2*threshold - 1) * p) where q = E[selected vars]
            q_hat <- sum(stability$selection_frequencies)
            pfer_bound <- if (threshold > 0.5) {
                q_hat^2 / ((2 * threshold - 1) * p)
            } else {
                NA
            }

            for (i in seq_len(p)) {
                table$addRow(rowKey = i, values = list(
                    variable = var_names[i],
                    selection_frequency = stability$selection_frequencies[i],
                    stability_score = stability$selection_frequencies[i],
                    stable_selection = if (stability$selection_frequencies[i] >= threshold) "Stable" else "Unstable",
                    error_bound = pfer_bound
                ))
            }

            table$setNote("pfer_note",
                paste0("Error bound: per-family error rate (PFER) from Meinshausen & Buhlmann (2010). ",
                       "PFER = ", round(pfer_bound, 3), " with threshold = ", threshold, "."))
        },

        .populateDiagnostics = function(adaptive_results) {
            table <- self$results$modelDiagnostics
            table$deleteRows()

            diag <- adaptive_results$diagnostics

            table$addRow(rowKey = 1, values = list(
                diagnostic = "Variables Selected",
                statistic = diag$n_selected,
                p_value = NA,
                interpretation = paste(diag$n_selected, "of", length(adaptive_results$adaptive_weights), "variables retained")
            ))

            conc_val <- if (!is.null(diag$concordance)) diag$concordance else NA
            conc_interp <- if (!is.na(conc_val)) {
                if (conc_val >= 0.8) "Excellent discrimination"
                else if (conc_val >= 0.7) "Good discrimination"
                else if (conc_val >= 0.6) "Moderate discrimination"
                else "Poor discrimination"
            } else "Not available"

            table$addRow(rowKey = 2, values = list(
                diagnostic = "Concordance Index",
                statistic = conc_val,
                p_value = NA,
                interpretation = conc_interp
            ))

            table$addRow(rowKey = 3, values = list(
                diagnostic = "Log-Likelihood",
                statistic = if (!is.null(diag$loglik)) diag$loglik else NA,
                p_value = NA,
                interpretation = "Model fit measure (higher is better)"
            ))

            table$addRow(rowKey = 4, values = list(
                diagnostic = "AIC",
                statistic = if (!is.null(diag$aic)) diag$aic else NA,
                p_value = NA,
                interpretation = "Model complexity-penalized fit (lower is better)"
            ))

            next_row <- 5

            # Proportional hazards test if available
            if (!is.null(diag$ph_global_p)) {
                ph_interp <- if (diag$ph_global_p < 0.05) {
                    "PH assumption may be violated (p < 0.05)"
                } else {
                    "PH assumption appears satisfied"
                }
                table$addRow(rowKey = next_row, values = list(
                    diagnostic = "Proportional Hazards (Global)",
                    statistic = NA,
                    p_value = diag$ph_global_p,
                    interpretation = ph_interp
                ))
                next_row <- next_row + 1
            }

            if (!is.null(diag$influence_max)) {
                table$addRow(rowKey = next_row, values = list(
                    diagnostic = "Max |dfbeta|",
                    statistic = diag$influence_max,
                    p_value = NA,
                    interpretation = "Largest absolute dfbeta across observations"
                ))
                next_row <- next_row + 1
            }

            if (!is.null(diag$influence_n)) {
                table$addRow(rowKey = next_row, values = list(
                    diagnostic = "Potentially Influential Cases",
                    statistic = diag$influence_n,
                    p_value = NA,
                    interpretation = paste0("Count with |dfbeta| > ", round(diag$influence_cutoff, 3))
                ))
            }
        },

        .populatePerformance = function(adaptive_results, cox_data) {
            table <- self$results$performanceMetrics
            table$deleteRows()

            diag <- adaptive_results$diagnostics

            # C-index with CI if available from refitted Cox
            conc_val <- if (!is.null(diag$concordance)) diag$concordance else NA
            conc_ci <- ""
            if (!is.null(diag$cox_fit)) {
                tryCatch({
                    se <- as.numeric(diag$cox_fit$concordance["std"])
                    if (!is.na(se) && se > 0) {
                        lower <- max(0, conc_val - 1.96 * se)
                        upper <- min(1, conc_val + 1.96 * se)
                        conc_ci <- paste0("[", round(lower, 3), ", ", round(upper, 3), "]")
                    }
                }, error = function(e) {})
            }

            table$addRow(rowKey = 1, values = list(
                metric = "C-index",
                value = conc_val,
                confidence_interval = conc_ci,
                description = "Concordance index (discrimination ability)"
            ))

            table$addRow(rowKey = 2, values = list(
                metric = "Variables Selected",
                value = diag$n_selected,
                confidence_interval = "",
                description = paste0("Out of ", length(adaptive_results$adaptive_weights), " candidates")
            ))

            table$addRow(rowKey = 3, values = list(
                metric = "Log-Likelihood",
                value = if (!is.null(diag$loglik)) diag$loglik else NA,
                confidence_interval = "",
                description = "Partial log-likelihood of refitted Cox model"
            ))

            table$addRow(rowKey = 4, values = list(
                metric = "AIC",
                value = if (!is.null(diag$aic)) diag$aic else NA,
                confidence_interval = "",
                description = "Akaike Information Criterion (lower = better)"
            ))

            # Number of observations and events
            table$addRow(rowKey = 5, values = list(
                metric = "N (events)",
                value = cox_data$n_events,
                confidence_interval = paste0("of ", cox_data$n_obs, " total"),
                description = "Number of events in analysis"
            ))
            
            table$setNote("cindex_note", "* Note: The C-index evaluates performance on the same data used for variable selection, which may overestimate true out-of-sample performance due to optimism.")
        },

        .populateRiskGroups = function(adaptive_results, cox_data) {
            table <- self$results$riskGroups
            table$deleteRows()

            diag <- adaptive_results$diagnostics
            selected_idx <- diag$selected_idx
            if (is.null(selected_idx) || length(selected_idx) == 0) return()

            requested_groups <- self$options$risk_groups
            coef_vec <- as.vector(adaptive_results$coef_min)

            # Calculate linear predictor for risk stratification
            lp <- as.numeric(cox_data$x %*% coef_vec)
            group_info <- private$.assignRiskGroups(lp, requested_groups)
            risk_group <- group_info$groups
            group_labels <- group_info$labels

            if (group_info$n_groups < requested_groups) {
                table$setNote(
                    "risk_groups_note",
                    paste0(
                        "Requested ", requested_groups, " groups, but only ",
                        group_info$n_groups,
                        " non-empty risk strata could be formed from available linear predictor values."
                    )
                )
            }

            for (g in seq_len(group_info$n_groups)) {
                g_mask <- risk_group == group_labels[g]
                g_time <- cox_data$time[g_mask]
                g_event <- cox_data$event[g_mask]
                n_subj <- sum(g_mask)
                n_ev <- sum(g_event == 1)

                # Median survival via Kaplan-Meier
                med_surv <- NA
                tryCatch({
                    km <- survival::survfit(survival::Surv(g_time, g_event) ~ 1)
                    med_surv <- summary(km)$table["median"]
                }, error = function(e) {})

                surv_range <- if (n_subj > 0) {
                    paste0("[", round(min(g_time), 1), ", ", round(max(g_time), 1), "]")
                } else {
                    "[NA, NA]"
                }

                # Hazard ratio relative to group 1 (reference)
                hr_val <- NA
                if (g > 1 && !is.null(diag$cox_fit)) {
                    tryCatch({
                        ref_mask <- risk_group == group_labels[1]
                        combined <- c(which(ref_mask), which(g_mask))
                        group_var <- factor(c(rep("ref", sum(ref_mask)), rep("test", sum(g_mask))),
                                         levels = c("ref", "test"))
                        if (is.null(cox_data$strata)) {
                            cox_hr <- survival::coxph(
                                survival::Surv(cox_data$time[combined], cox_data$event[combined]) ~ group_var
                            )
                        } else {
                            strata_sub <- cox_data$strata[combined]
                            cox_hr <- survival::coxph(
                                survival::Surv(cox_data$time[combined], cox_data$event[combined]) ~
                                    group_var + survival::strata(strata_sub)
                            )
                        }
                        hr_val <- exp(coef(cox_hr))
                    }, error = function(e) {})
                } else if (g == 1) {
                    hr_val <- 1.0  # Reference group
                }

                table$addRow(rowKey = g, values = list(
                    risk_group = group_labels[g],
                    n_subjects = as.integer(n_subj),
                    n_events = as.integer(n_ev),
                    median_survival = med_surv,
                    survival_range = surv_range,
                    hazard_ratio = hr_val
                ))
            }
        },

        .populatePredictions = function(adaptive_results, cox_data) {
            table <- self$results$predictions
            table$deleteRows()

            if (!isTRUE(self$options$baseline_survival)) {
                return()
            }

            tp_string <- trimws(self$options$time_points)
            if (nchar(tp_string) == 0) return()

            diag <- adaptive_results$diagnostics
            if (is.null(diag$cox_fit)) return()

            # Parse time points from comma-separated string
            time_points <- tryCatch({
                as.numeric(trimws(unlist(strsplit(tp_string, ","))))
            }, warning = function(w) numeric(0),
               error = function(e) numeric(0))
            time_points <- sort(unique(time_points[!is.na(time_points) & time_points > 0]))
            if (length(time_points) == 0) return()

            # Get baseline survival from refitted Cox model
            tryCatch({
                base_surv <- survival::survfit(diag$cox_fit, se.fit = TRUE)
                surv_times <- base_surv$time
                surv_est <- if (is.matrix(base_surv$surv)) rowMeans(base_surv$surv) else base_surv$surv
                surv_lower <- if (is.matrix(base_surv$lower)) rowMeans(base_surv$lower) else base_surv$lower
                surv_upper <- if (is.matrix(base_surv$upper)) rowMeans(base_surv$upper) else base_surv$upper

                for (k in seq_along(time_points)) {
                    tp <- time_points[k]
                    # Find the survival estimate at or just before this time point
                    idx <- max(which(surv_times <= tp), 0)
                    if (idx > 0) {
                        table$addRow(rowKey = k, values = list(
                            time_point = tp,
                            baseline_survival = surv_est[idx],
                            survival_lower = surv_lower[idx],
                            survival_upper = surv_upper[idx]
                        ))
                    } else {
                        table$addRow(rowKey = k, values = list(
                            time_point = tp,
                            baseline_survival = 1.0,
                            survival_lower = NA,
                            survival_upper = NA
                        ))
                    }
                }
            }, error = function(e) {
                # If baseline survival fails, populate with NAs
                for (k in seq_along(time_points)) {
                    table$addRow(rowKey = k, values = list(
                        time_point = time_points[k],
                        baseline_survival = NA,
                        survival_lower = NA,
                        survival_upper = NA
                    ))
                }
            })
        },

        .plotSurvivalCurves = function(adaptive_results, cox_data) {
            image <- self$results$survivalPlot
            coef_vec <- as.vector(adaptive_results$coef_min)
            lp <- as.numeric(cox_data$x %*% coef_vec)
            group_info <- private$.assignRiskGroups(lp, self$options$risk_groups)
            risk_group <- group_info$groups

            plot_data <- list(
                time = as.numeric(cox_data$time),
                event = as.numeric(cox_data$event),
                risk_group = as.character(risk_group),
                n_groups = as.integer(group_info$n_groups)
            )
            image$setState(as.data.frame(plot_data))
        },

        .plotBaselineHazard = function(adaptive_results, cox_data) {
            image <- self$results$baselineHazardPlot
            diag <- adaptive_results$diagnostics
            if (is.null(diag$cox_fit)) {
                image$setState(NULL)
                return()
            }
            tryCatch({
                base_haz <- survival::basehaz(diag$cox_fit, centered = FALSE)
                plot_data <- list(
                    time = as.numeric(base_haz$time),
                    hazard = as.numeric(base_haz$hazard)
                )
                image$setState(as.data.frame(plot_data))
            }, error = function(e) {
                image$setState(NULL)
            })
        },

        .plotDiagnosticsData = function(adaptive_results, cox_data) {
            image <- self$results$diagnosticsPlot
            diag <- adaptive_results$diagnostics
            if (is.null(diag$cox_fit)) {
                image$setState(NULL)
                return()
            }
            tryCatch({
                # Deviance residuals for diagnostic plot
                dev_resid <- residuals(diag$cox_fit, type = "deviance")
                lp <- predict(diag$cox_fit, type = "lp")
                plot_data <- list(
                    linear_predictor = as.numeric(lp),
                    deviance_residuals = as.numeric(dev_resid)
                )
                if (isTRUE(self$options$influence_diagnostics)) {
                    dfbeta_resid <- residuals(diag$cox_fit, type = "dfbeta")
                    if (is.matrix(dfbeta_resid)) {
                        plot_data$max_abs_dfbeta <- as.numeric(apply(abs(dfbeta_resid), 1, max, na.rm = TRUE))
                    } else {
                        plot_data$max_abs_dfbeta <- as.numeric(abs(dfbeta_resid))
                    }
                }
                image$setState(as.data.frame(plot_data))
            }, error = function(e) {
                image$setState(NULL)
            })
        },

        .plotSelectionPath = function(adaptive_results) {
            image <- self$results$pathPlot
            full_fit <- adaptive_results$full_fit
            beta_mat <- as.matrix(full_fit$beta)
            if (nrow(beta_mat) == 0 || ncol(beta_mat) == 0) {
                image$setState(NULL)
                return()
            }

            max_abs <- apply(abs(beta_mat), 1, max, na.rm = TRUE)
            keep_idx <- order(max_abs, decreasing = TRUE)[seq_len(min(20, length(max_abs)))]
            beta_top <- beta_mat[keep_idx, , drop = FALSE]

            state_df <- as.data.frame(t(beta_top))
            colnames(state_df) <- make.names(rownames(beta_top), unique = TRUE)
            state_df$lambda <- as.numeric(full_fit$lambda)
            state_df$lambda_min <- rep(as.numeric(adaptive_results$lambda_min), nrow(state_df))
            state_df$lambda_1se <- rep(as.numeric(adaptive_results$lambda_1se), nrow(state_df))
            image$setState(state_df)
        },

        .plotCVCurve = function(adaptive_results) {
            image <- self$results$cvPlot
            cv_fit <- adaptive_results$cv_fit
            plot_data <- list(
                lambda = as.numeric(cv_fit$lambda),
                cvm = as.numeric(cv_fit$cvm),
                cvsd = as.numeric(cv_fit$cvsd),
                cvup = as.numeric(cv_fit$cvup),
                cvlo = as.numeric(cv_fit$cvlo),
                lambda_min = as.numeric(cv_fit$lambda.min),
                lambda_1se = as.numeric(cv_fit$lambda.1se),
                measure_label = if (self$options$cv_measure == "C") "C-index" else "Partial Likelihood Deviance"
            )
            image$setState(plot_data)
        },

        .plotStability = function(adaptive_results) {
            image <- self$results$stabilityPlot
            if (!is.null(adaptive_results$stability)) {
                var_names <- as.character(rownames(adaptive_results$cv_fit$glmnet.fit$beta))
                if (length(var_names) != length(adaptive_results$stability$selection_frequencies)) {
                    var_names <- paste0("V", seq_along(adaptive_results$stability$selection_frequencies))
                }
                plot_data <- list(
                    selection_frequencies = as.numeric(adaptive_results$stability$selection_frequencies),
                    var_names = var_names,
                    threshold = as.numeric(self$options$stability_threshold)
                )
                image$setState(plot_data)
            } else {
                image$setState(NULL)
            }
        },

        .renderPathPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || nrow(state) == 0) return(FALSE)
            vars <- setdiff(colnames(state), c("lambda", "lambda_min", "lambda_1se"))
            if (length(vars) == 0) return(FALSE)

            x <- log(state$lambda)
            y <- as.matrix(state[, vars, drop = FALSE])
            matplot(x, y, type = "l", lty = 1,
                    xlab = "Log(Lambda)", ylab = "Coefficient",
                    main = "Coefficient Paths")

            lambda_min <- state$lambda_min[1]
            lambda_1se <- state$lambda_1se[1]
            abline(v = log(lambda_min), lty = 2, col = "red")
            if (!is.na(lambda_1se)) {
                abline(v = log(lambda_1se), lty = 3, col = "blue")
            }

            legend_vars <- head(vars, 8)
            legend(
                "topright",
                legend = c(legend_vars, "Lambda Min", "Lambda 1SE"),
                col = c(seq_along(legend_vars), "red", "blue"),
                lty = c(rep(1, length(legend_vars)), 2, 3),
                cex = 0.7
            )
            return(TRUE)
        },

        .renderCVPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            if (all(is.na(state$cvm))) {
                plot.new()
                title(main = "Cross-Validation Curve")
                text(0.5, 0.5, "Single lambda mode:\nCV curve not available")
                return(TRUE)
            }
            log_lambda <- log(state$lambda)
            y_label <- if (!is.null(state$measure_label)) state$measure_label else "CV Metric"
            plot(log_lambda, state$cvm, type = "n",
                 xlab = "Log(Lambda)", ylab = y_label,
                 main = "Cross-Validation Curve",
                 ylim = range(c(state$cvlo, state$cvup), na.rm = TRUE))
            polygon(c(log_lambda, rev(log_lambda)),
                    c(state$cvup, rev(state$cvlo)),
                    col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)
            lines(log_lambda, state$cvm, col = "red", lwd = 2)
            abline(v = log(state$lambda_min), lty = 2, col = "blue")
            if (!is.null(state$lambda_1se))
                abline(v = log(state$lambda_1se), lty = 3, col = "blue")
            return(TRUE)
        },

        .renderStabilityPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            freq <- state$selection_frequencies
            names(freq) <- state$var_names
            freq <- sort(freq, decreasing = TRUE)
            freq <- head(freq, 20)
            barplot(freq, las = 2, main = "Stability Selection",
                    ylab = "Selection Frequency", col = "steelblue",
                    cex.names = 0.7)
            abline(h = state$threshold, lty = 2, col = "red")
            return(TRUE)
        },

        .renderSurvivalPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || nrow(state) == 0) return(FALSE)

            surv_obj <- survival::Surv(state$time, state$event)
            risk_group <- factor(state$risk_group)
            km_fit <- survival::survfit(surv_obj ~ risk_group)

            # Use base R plot for reliability
            plot(km_fit, col = seq_len(nlevels(risk_group)),
                 lwd = 2, xlab = "Time", ylab = "Survival Probability",
                 main = "Risk Group Survival Curves")
            legend("topright", levels(risk_group),
                   col = seq_len(nlevels(risk_group)), lwd = 2, cex = 0.7)

            # Add log-rank p-value
            tryCatch({
                lr <- survival::survdiff(surv_obj ~ risk_group)
                p_val <- 1 - pchisq(lr$chisq, df = nlevels(risk_group) - 1)
                mtext(paste0("Log-rank p = ", format.pval(p_val, digits = 3)),
                      side = 3, line = 0, cex = 0.8)
            }, error = function(e) {})

            return(TRUE)
        },

        .renderBaselineHazardPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || nrow(state) == 0) return(FALSE)

            par(mfrow = c(1, 2))

            # Cumulative baseline hazard
            plot(state$time, state$hazard, type = "s", lwd = 2, col = "darkblue",
                 xlab = "Time", ylab = "Cumulative Hazard",
                 main = "Cumulative Baseline Hazard")

            # Instantaneous baseline hazard (smoothed)
            n <- nrow(state)
            if (n > 1) {
                dt <- diff(state$time)
                dh <- diff(state$hazard)
                inst_haz <- dh / pmax(dt, 1e-8)
                mid_time <- (state$time[-n] + state$time[-1]) / 2
                plot(mid_time, inst_haz, type = "l", lwd = 2, col = "darkred",
                     xlab = "Time", ylab = "Hazard Rate",
                     main = "Baseline Hazard Rate")
            }

            par(mfrow = c(1, 1))
            return(TRUE)
        },

        .renderDiagnosticsPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || nrow(state) == 0) return(FALSE)

            par(mfrow = c(1, 2))

            # Deviance residuals vs linear predictor
            plot(state$linear_predictor, state$deviance_residuals,
                 xlab = "Linear Predictor", ylab = "Deviance Residuals",
                 main = "Residuals vs Linear Predictor",
                 pch = 16, col = rgb(0, 0, 0, 0.4), cex = 0.8)
            abline(h = 0, lty = 2, col = "red")
            lines(lowess(state$linear_predictor, state$deviance_residuals),
                  col = "blue", lwd = 2)

            if ("max_abs_dfbeta" %in% names(state)) {
                plot(seq_along(state$max_abs_dfbeta), state$max_abs_dfbeta,
                     xlab = "Observation Index", ylab = "Max |dfbeta|",
                     main = "Influence Diagnostics",
                     pch = 16, col = rgb(0.8, 0.2, 0.2, 0.5), cex = 0.7)
                abline(h = 2 / sqrt(nrow(state)), lty = 2, col = "red")
            } else {
                # QQ plot of deviance residuals
                qqnorm(state$deviance_residuals, main = "QQ Plot of Deviance Residuals",
                       pch = 16, col = rgb(0, 0, 0, 0.4), cex = 0.8)
                qqline(state$deviance_residuals, col = "red", lwd = 2)
            }

            par(mfrow = c(1, 1))
            return(TRUE)
        },

        .initResults = function() {
            # Initialize result tables with proper visibility
            private$.hideMessage()
            self$results$coefficients$setVisible(self$options$show_coefficients)
            self$results$selectionPath$setVisible(self$options$show_selection_path)
            self$results$cvResults$setVisible(self$options$show_cv_results)
            self$results$stabilityResults$setVisible(self$options$stability_selection)
            self$results$modelDiagnostics$setVisible(self$options$show_diagnostics)
            self$results$performanceMetrics$setVisible(self$options$goodness_of_fit)
        }
    )
)
