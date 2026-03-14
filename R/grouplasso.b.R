grouplassoClass <- R6::R6Class(
    "grouplassoClass",
    inherit = grouplassoBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$time) || is.null(self$options$event)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        h2 {color: #2ecc71;}
                        body {font-family: Arial, sans-serif; margin: 20px;}
                        .highlight {background-color: #f39c12; padding: 2px 4px; border-radius: 3px;}
                        .step {margin: 10px 0; padding: 8px; background-color: #ecf0f1; border-radius: 5px;}
                    </style>
                    </head>
                    <body>
                    <h2> Group LASSO for Survival Analysis</h2>
                    <p><strong>True group-level penalized Cox regression via grpreg (L1/L2 mixed norm)</strong></p>

                    <div class='step'>
                    <strong> Required Data:</strong>
                    <ul>
                        <li><span class='highlight'>Time Variable</span>: Time to event or censoring</li>
                        <li><span class='highlight'>Event Indicator</span>: 0 = censored, 1 = event</li>
                        <li><span class='highlight'>Predictor Variables</span>: Variables organized into meaningful groups</li>
                    </ul>
                    </div>

                    <div class='step'>
                    <strong> Group Definition Methods:</strong>
                    <ul>
                        <li><strong>Automatic:</strong> Each original variable (and its dummies) forms a group</li>
                        <li><strong>Factor-Based:</strong> Dummy variables from the same factor grouped together</li>
                        <li><strong>Custom:</strong> User-defined groups (e.g., clinical domains, gene pathways)</li>
                    </ul>
                    </div>

                    <div class='step'>
                    <strong> Penalty Types (via grpreg):</strong>
                    <ul>
                        <li><strong>Group LASSO:</strong> L1/L2 mixed norm selects entire groups simultaneously</li>
                        <li><strong>Group MCP:</strong> Non-convex penalty with less bias for large coefficients</li>
                        <li><strong>Group SCAD:</strong> Smoothly clipped penalty for oracle properties</li>
                        <li><strong>Adaptive Group LASSO:</strong> Data-driven weights improve selection consistency</li>
                    </ul>
                    </div>

                    <div class='step'>
                    <strong> Key Features:</strong>
                    <ul>
                        <li> True group selection (entire groups selected or excluded together)</li>
                        <li> Cross-validation for optimal penalty selection</li>
                        <li> Stability selection for robust group identification</li>
                        <li> Nested CV for unbiased performance assessment</li>
                        <li> Permutation testing for statistical significance</li>
                    </ul>
                    </div>

                    <p><em> Tip: Use factor-based grouping for categorical variables and custom grouping for domain-specific applications like gene pathways.</em></p>
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

            # Check if grpreg is available
            if (!requireNamespace("grpreg", quietly = TRUE)) {
                private$.insertNotice(
                    'missingGrpreg', jmvcore::NoticeType$ERROR,
                    "Package 'grpreg' is required for Group LASSO Cox regression. Install with: install.packages('grpreg')"
                )
                return()
            }

            if (!requireNamespace("survival", quietly = TRUE)) {
                private$.insertNotice(
                    'missingSurvival', jmvcore::NoticeType$ERROR,
                    "Package 'survival' is required. Install with: install.packages('survival')"
                )
                return()
            }

            # Prepare data and validate inputs
            group_data <- private$.prepareData()
            if (is.null(group_data)) return()

            # Assess data suitability if requested
            if (self$options$suitabilityCheck) {
                pred_mat <- group_data$x
                if (ncol(pred_mat) > 0) {
                    private$.assessSuitability(pred_mat, group_data$time, group_data$event)
                }
            }

            # Clinical notices for low events
            n_events <- group_data$n_events
            n_predictors <- ncol(group_data$x)
            if (n_events < 10) {
                private$.insertNotice(
                    'veryLowEvents', jmvcore::NoticeType$STRONG_WARNING,
                    sprintf("Only %d events detected. Results are highly unreliable with fewer than 10 events. Consider collecting more data.", n_events),
                    position = 1
                )
            } else if (n_events < 20) {
                private$.insertNotice(
                    'lowEvents', jmvcore::NoticeType$WARNING,
                    sprintf("Only %d events detected. Consider that penalized models need adequate events for stable selection.", n_events)
                )
            }

            # Fit group LASSO model
            tryCatch({
                group_results <- private$.fitGroupLasso(group_data)
                if (!is.null(group_results)) {
                    private$.populateResults(group_results, group_data)

                    # Post-analysis notices
                    n_selected <- sum(abs(group_results$coef_min) > self$options$selection_threshold)
                    if (n_selected == 0) {
                        private$.insertNotice(
                            'noSelection', jmvcore::NoticeType$WARNING,
                            "No variables were selected at lambda.min. All coefficients are zero. Consider relaxing the penalty or checking data quality."
                        )
                    }

                    # Completion notice
                    penalty_label <- switch(self$options$penalty_type,
                        group_lasso = "Group LASSO",
                        group_mcp = "Group MCP",
                        group_scad = "Group SCAD",
                        adaptive_group = "Adaptive Group LASSO",
                        "Group LASSO"
                    )
                    n_groups <- max(group_data$groups)
                    n_groups_sel <- if (n_selected > 0) {
                        length(unique(group_data$groups[abs(group_results$coef_min) > self$options$selection_threshold]))
                    } else { 0 }

                    private$.insertNotice(
                        'analysisComplete', jmvcore::NoticeType$INFO,
                        sprintf("%s analysis completed: %d/%d groups selected, %d/%d variables selected, using %d-fold CV with %d observations (%d events).",
                                penalty_label, n_groups_sel, n_groups, n_selected, n_predictors,
                                self$options$cv_folds, group_data$n_obs, n_events)
                    )
                }
            }, error = function(e) {
                private$.insertNotice(
                    'analysisError', jmvcore::NoticeType$ERROR,
                    sprintf("Error in Group LASSO fitting: %s. Check group definitions and variable structure.", e$message),
                    position = 1
                )
            })
        },

        # ══════════════════════════════════════════════════════════════
        # Notice Helper
        # ══════════════════════════════════════════════════════════════
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
                # Fallback to todo Html if Notice insertion fails
                tryCatch({
                    existing <- self$results$todo$content
                    if (is.null(existing) || nchar(existing) == 0) existing <- ""
                    label <- switch(as.character(type),
                        "1" = "ERROR", "2" = "WARNING", "3" = "WARNING", "4" = "INFO", "NOTE")
                    self$results$todo$setContent(paste0(
                        existing, "<p><strong>", label, ":</strong> ", content, "</p>"
                    ))
                }, error = function(e2) NULL)
            })
        },

        # ══════════════════════════════════════════════════════════════
        # Data Preparation
        # ══════════════════════════════════════════════════════════════
        .prepareData = function() {
            data <- self$data

            time_var <- self$options$time
            event_var <- self$options$event
            pred_vars <- self$options$predictors

            if (is.null(time_var) || is.null(event_var) || length(pred_vars) == 0) {
                return(NULL)
            }

            # Create analysis dataset
            vars_needed <- c(time_var, event_var, pred_vars)
            analysis_data <- data[, vars_needed, drop = FALSE]
            analysis_data <- na.omit(analysis_data)

            if (nrow(analysis_data) < length(pred_vars) + 10) {
                private$.insertNotice(
                    'insufficientData', jmvcore::NoticeType$ERROR,
                    sprintf("Need at least p + 10 = %d complete observations for reliable Group LASSO. Only %d available.", length(pred_vars) + 10, nrow(analysis_data)),
                    position = 1
                )
                return(NULL)
            }

            # Prepare survival object
            time_values <- as.numeric(analysis_data[[time_var]])

            # Two-level outcome encoding using outcomeLevel / censorLevel
            event_data <- analysis_data[[event_var]]
            event_level <- as.character(self$options$outcomeLevel)
            censor_level <- as.character(self$options$censorLevel)

            event_values <- rep(NA_real_, length(event_data))

            if (is.factor(event_data)) {
                event_chr <- as.character(event_data)
                event_values[event_chr == event_level] <- 1
                event_values[event_chr == censor_level] <- 0
            } else {
                event_level_num <- suppressWarnings(as.numeric(event_level))
                censor_level_num <- suppressWarnings(as.numeric(censor_level))
                if (!is.na(event_level_num)) event_values[event_data == event_level_num] <- 1
                if (!is.na(censor_level_num)) event_values[event_data == censor_level_num] <- 0
            }

            n_excluded_outcome <- sum(is.na(event_values) & !is.na(event_data))
            if (n_excluded_outcome > 0) {
                private$.insertNotice(
                    'excludedRows', jmvcore::NoticeType$WARNING,
                    sprintf("%d row(s) excluded because outcome value matched neither event level ('%s') nor censored level ('%s').",
                            n_excluded_outcome, event_level, censor_level)
                )
            }

            # Remove rows where event encoding produced NA
            keep_rows <- !is.na(event_values) & !is.na(time_values)
            analysis_data <- analysis_data[keep_rows, , drop = FALSE]
            time_values <- time_values[keep_rows]
            event_values <- event_values[keep_rows]

            if (nrow(analysis_data) < length(pred_vars) + 10) {
                private$.insertNotice(
                    'insufficientDataAfterEncoding', jmvcore::NoticeType$ERROR,
                    sprintf("Need at least p + 10 = %d complete observations after event encoding. Only %d remain.", length(pred_vars) + 10, nrow(analysis_data)),
                    position = 1
                )
                return(NULL)
            }

            if (any(time_values <= 0, na.rm = TRUE)) {
                private$.insertNotice(
                    'invalidTime', jmvcore::NoticeType$ERROR,
                    "All time values must be positive for survival analysis.",
                    position = 1
                )
                return(NULL)
            }

            # Prepare predictor matrix and groups
            pred_data <- analysis_data[, pred_vars, drop = FALSE]
            design_result <- private$.createDesignMatrix(pred_data, pred_vars)

            if (is.null(design_result)) return(NULL)

            # Create survival object
            surv_obj <- survival::Surv(time_values, event_values)

            return(list(
                data = analysis_data,
                x = design_result$x,
                y = surv_obj,
                time = time_values,
                event = event_values,
                groups = design_result$groups,
                group_info = design_result$group_info,
                n_obs = nrow(analysis_data),
                n_events = sum(event_values),
                var_names = colnames(design_result$x),
                original_pred_names = pred_vars
            ))
        },

        .createDesignMatrix = function(pred_data, pred_vars) {
            # Convert factors to dummy variables and create design matrix
            x_matrix <- model.matrix(~ . - 1, data = pred_data)

            # Standardize if requested (grpreg does internal standardization,
            # but explicit pre-scaling ensures consistent behavior)
            if (self$options$standardize) {
                x_scaled <- scale(x_matrix)
                # Replace NaN columns (zero-variance) with zeros
                x_scaled[is.nan(x_scaled)] <- 0
                x_matrix <- x_scaled
            }

            # Track which columns came from which original variable
            # using model.matrix's assign attribute
            assign_vec <- attr(x_matrix, "assign")
            if (is.null(assign_vec)) {
                # Fallback: map columns to original variables by name matching
                assign_vec <- rep(0L, ncol(x_matrix))
                for (i in seq_along(pred_vars)) {
                    var <- pred_vars[i]
                    col_indices <- which(startsWith(colnames(x_matrix), var))
                    assign_vec[col_indices] <- i
                }
            }

            # Define groups based on method
            group_method <- self$options$group_definition
            groups <- private$.defineGroups(x_matrix, pred_data, pred_vars, group_method, assign_vec)

            if (is.null(groups)) {
                private$.insertNotice(
                    'groupDefError', jmvcore::NoticeType$ERROR,
                    "Failed to define variable groups. Check group structure specification.",
                    position = 1
                )
                return(NULL)
            }

            # Ensure groups are consecutive integers starting at 1
            unique_groups <- sort(unique(groups$group_vector))
            group_map <- setNames(seq_along(unique_groups), unique_groups)
            groups$group_vector <- as.integer(group_map[as.character(groups$group_vector)])
            groups$group_info$group_id <- as.integer(group_map[as.character(groups$group_info$group_id)])

            return(list(
                x = x_matrix,
                groups = groups$group_vector,
                group_info = groups$group_info
            ))
        },

        .defineGroups = function(x_matrix, pred_data, pred_vars, method, assign_vec) {
            var_names <- colnames(x_matrix)

            if (method == "automatic") {
                groups <- private$.automaticGrouping(pred_data, pred_vars, var_names, assign_vec)
            } else if (method == "factor_based") {
                groups <- private$.factorBasedGrouping(x_matrix, pred_data, pred_vars, assign_vec)
            } else if (method == "custom") {
                groups <- private$.customGrouping(var_names)
            } else {
                groups <- private$.automaticGrouping(pred_data, pred_vars, var_names, assign_vec)
            }

            if (is.null(groups)) return(NULL)
            return(groups)
        },

        .automaticGrouping = function(pred_data, pred_vars, var_names, assign_vec) {
            # If factor_grouping is enabled, group dummies from same factor together
            # Otherwise, each column is its own group
            use_factor_grouping <- self$options$factor_grouping

            if (use_factor_grouping) {
                # Use assign_vec to map columns to original variables
                group_vector <- assign_vec
                # Ensure we handle any unmapped columns (assign = 0)
                if (any(group_vector == 0)) {
                    max_g <- max(group_vector)
                    zeros <- which(group_vector == 0)
                    for (j in seq_along(zeros)) {
                        group_vector[zeros[j]] <- max_g + j
                    }
                }
            } else {
                # Each column is its own group
                group_vector <- seq_len(length(var_names))
            }

            # Build group_info
            unique_groups <- sort(unique(group_vector))
            group_info <- data.frame(
                group_id = unique_groups,
                group_name = sapply(unique_groups, function(g) {
                    g_idx <- which(group_vector == g)
                    if (length(g_idx) == 1) {
                        var_names[g_idx]
                    } else {
                        # Find the common prefix (original variable name)
                        g_names <- var_names[g_idx]
                        # Try to find matching pred_var
                        matched <- FALSE
                        for (pv in pred_vars) {
                            if (all(startsWith(g_names, pv))) {
                                matched <- TRUE
                                return(pv)
                            }
                        }
                        if (!matched) paste(g_names, collapse = "+")
                    }
                }),
                group_size = as.integer(table(group_vector)[as.character(unique_groups)]),
                stringsAsFactors = FALSE
            )

            return(list(group_vector = group_vector, group_info = group_info))
        },

        .factorBasedGrouping = function(x_matrix, pred_data, pred_vars, assign_vec) {
            # Same as automatic with factor_grouping = TRUE
            var_names <- colnames(x_matrix)
            group_vector <- assign_vec

            if (any(group_vector == 0)) {
                max_g <- max(group_vector)
                zeros <- which(group_vector == 0)
                for (j in seq_along(zeros)) {
                    group_vector[zeros[j]] <- max_g + j
                }
            }

            unique_groups <- sort(unique(group_vector))
            group_info <- data.frame(
                group_id = unique_groups,
                group_name = sapply(unique_groups, function(g) {
                    g_idx <- which(group_vector == g)
                    if (length(g_idx) == 1) {
                        var_names[g_idx]
                    } else {
                        g_names <- var_names[g_idx]
                        for (pv in pred_vars) {
                            if (all(startsWith(g_names, pv))) return(pv)
                        }
                        paste(g_names, collapse = "+")
                    }
                }),
                group_size = as.integer(table(group_vector)[as.character(unique_groups)]),
                stringsAsFactors = FALSE
            )

            return(list(group_vector = group_vector, group_info = group_info))
        },

        .customGrouping = function(var_names) {
            group_str <- trimws(self$options$group_structure)
            if (nchar(group_str) == 0) {
                # Default to individual groups
                group_vector <- seq_len(length(var_names))
                group_info <- data.frame(
                    group_id = seq_len(length(var_names)),
                    group_name = var_names,
                    group_size = rep(1L, length(var_names)),
                    stringsAsFactors = FALSE
                )
                return(list(group_vector = group_vector, group_info = group_info))
            }

            # Parse custom group structure: "var1:1, var2:1, var3:2"
            group_assignments <- unlist(strsplit(group_str, ","))
            group_vector <- rep(NA_integer_, length(var_names))

            for (assignment in group_assignments) {
                parts <- trimws(unlist(strsplit(assignment, ":")))
                if (length(parts) == 2) {
                    var_name <- parts[1]
                    group_id <- suppressWarnings(as.integer(parts[2]))
                    # Match using startsWith to handle factor dummies
                    var_idx <- which(startsWith(var_names, var_name))
                    if (length(var_idx) > 0 && !is.na(group_id)) {
                        group_vector[var_idx] <- group_id
                    }
                }
            }

            # Assign unmatched variables to their own groups
            unmatched <- which(is.na(group_vector))
            if (length(unmatched) > 0) {
                max_g <- max(group_vector, na.rm = TRUE)
                if (is.na(max_g) || is.infinite(max_g)) max_g <- 0L
                for (j in seq_along(unmatched)) {
                    group_vector[unmatched[j]] <- max_g + j
                }
            }

            unique_groups <- sort(unique(group_vector))
            group_info <- data.frame(
                group_id = unique_groups,
                group_name = paste("Group", unique_groups),
                group_size = as.integer(table(group_vector)[as.character(unique_groups)]),
                stringsAsFactors = FALSE
            )

            return(list(group_vector = group_vector, group_info = group_info))
        },

        # ══════════════════════════════════════════════════════════════
        # Group Weight Computation (centralized)
        # ══════════════════════════════════════════════════════════════
        .buildGroupMultiplier = function(group_data) {
            method <- self$options$group_weights
            group_info <- group_data$group_info
            n_groups <- nrow(group_info)

            if (method == "equal") {
                weights <- rep(1, n_groups)
            } else if (method == "sqrt_size") {
                weights <- sqrt(group_info$group_size)
            } else if (method == "group_size") {
                weights <- as.numeric(group_info$group_size)
            } else if (method == "custom") {
                weight_str <- trimws(self$options$custom_weights)
                if (nchar(weight_str) > 0) {
                    weights <- as.numeric(unlist(strsplit(weight_str, ",")))
                    if (length(weights) != n_groups) {
                        private$.insertNotice(
                            'weightMismatch', jmvcore::NoticeType$WARNING,
                            sprintf("Custom weights length (%d) does not match number of groups (%d). Using equal weights.", length(weights), n_groups)
                        )
                        weights <- rep(1, n_groups)
                    }
                } else {
                    weights <- rep(1, n_groups)
                }
            } else {
                weights <- rep(1, n_groups)
            }

            return(weights)
        },

        # ══════════════════════════════════════════════════════════════
        # Model Fitting (via grpreg)
        # ══════════════════════════════════════════════════════════════
        .fitGroupLasso = function(group_data) {
            penalty_type <- self$options$penalty_type

            if (penalty_type == "group_lasso") {
                results <- private$.fitGrpregCox(group_data, penalty = "grLasso")
            } else if (penalty_type == "group_mcp") {
                results <- private$.fitGrpregCox(group_data, penalty = "grMCP")
            } else if (penalty_type == "group_scad") {
                results <- private$.fitGrpregCox(group_data, penalty = "grSCAD")
            } else if (penalty_type == "adaptive_group") {
                results <- private$.fitAdaptiveGroupLasso(group_data)
            } else {
                results <- private$.fitGrpregCox(group_data, penalty = "grLasso")
            }

            # Add stability selection if requested
            if (self$options$stability_selection) {
                stability_results <- private$.stabilitySelection(group_data)
                results$stability <- stability_results
            }

            # Add nested CV if requested
            if (self$options$nested_cv) {
                nested_results <- private$.nestedCrossValidation(group_data)
                results$nested_cv <- nested_results
            }

            return(results)
        },

        .fitGrpregCox = function(group_data, penalty = "grLasso", group_multiplier = NULL) {
            if (is.null(group_multiplier)) {
                group_multiplier <- private$.buildGroupMultiplier(group_data)
            }

            set.seed(self$options$random_seed)

            # Fit with cross-validation via grpreg
            cv_result <- grpreg::cv.grpsurv(
                X = group_data$x,
                y = group_data$y,
                group = group_data$groups,
                penalty = penalty,
                group.multiplier = group_multiplier,
                nfolds = self$options$cv_folds,
                nlambda = self$options$n_lambda,
                lambda.min = self$options$lambda_min_ratio,
                eps = self$options$tolerance,
                max.iter = self$options$max_iterations,
                seed = self$options$random_seed
            )

            full_fit <- cv_result$fit
            lambda_min <- cv_result$lambda.min

            # Compute lambda.1se (grpreg does not provide this natively)
            lambda_1se <- private$.computeLambda1se(cv_result)

            # Get coefficients at optimal lambdas
            coef_min <- as.numeric(coef(full_fit, lambda = lambda_min))
            coef_1se <- as.numeric(coef(full_fit, lambda = lambda_1se))

            # grpreg Cox coef() may include intercept as first element (= 0)
            # Remove it if present
            if (length(coef_min) == ncol(group_data$x) + 1) {
                coef_min <- coef_min[-1]
                coef_1se <- coef_1se[-1]
            }

            return(list(
                cv_fit = full_fit,
                cv_result = cv_result,
                lambda_min = lambda_min,
                lambda_1se = lambda_1se,
                coef_min = coef_min,
                coef_1se = coef_1se,
                group_vector = group_data$groups,
                group_multiplier = group_multiplier
            ))
        },

        .fitAdaptiveGroupLasso = function(group_data) {
            # Step 1: Obtain initial coefficient estimates for adaptive weights
            method <- self$options$adaptive_weights_method
            initial_coefs <- private$.getInitialEstimates(group_data, method)

            # Step 2: Compute adaptive group weights from initial estimates
            gamma_val <- 1  # Standard adaptive weight exponent
            group_info <- group_data$group_info
            group_vector <- group_data$groups
            n_groups <- nrow(group_info)
            adaptive_multiplier <- numeric(n_groups)

            for (i in seq_len(n_groups)) {
                gid <- group_info$group_id[i]
                g_idx <- which(group_vector == gid)
                group_norm <- sqrt(sum(initial_coefs[g_idx]^2))
                # Inverse of group norm; large norm = small penalty (keep important groups)
                adaptive_multiplier[i] <- if (group_norm > 1e-10) {
                    (1 / group_norm)^gamma_val
                } else {
                    1e4  # Heavy penalty for groups with near-zero initial estimates
                }
            }

            # Normalize to have mean = 1
            adaptive_multiplier <- adaptive_multiplier / mean(adaptive_multiplier)

            # Step 3: Fit with adaptive weights
            results <- private$.fitGrpregCox(group_data, penalty = "grLasso",
                                             group_multiplier = adaptive_multiplier)
            return(results)
        },

        .getInitialEstimates = function(group_data, method) {
            n_vars <- ncol(group_data$x)

            if (method == "ridge") {
                # Ridge Cox via glmnet (alpha = 0)
                tryCatch({
                    if (requireNamespace("glmnet", quietly = TRUE)) {
                        ridge_fit <- glmnet::glmnet(
                            x = group_data$x,
                            y = group_data$y,
                            family = "cox",
                            alpha = 0,
                            standardize = FALSE
                        )
                        lambda_ridge <- ridge_fit$lambda[max(1, length(ridge_fit$lambda) %/% 4)]
                        coefs <- as.numeric(coef(ridge_fit, s = lambda_ridge))
                        return(coefs)
                    }
                }, error = function(e) NULL)
            }

            if (method == "univariate") {
                # Univariate Cox for each group
                coefs <- numeric(n_vars)
                group_info <- group_data$group_info
                group_vector <- group_data$groups

                for (i in seq_len(nrow(group_info))) {
                    gid <- group_info$group_id[i]
                    g_idx <- which(group_vector == gid)
                    tryCatch({
                        x_group <- group_data$x[, g_idx, drop = FALSE]
                        fit_g <- survival::coxph(group_data$y ~ x_group)
                        coefs[g_idx] <- coef(fit_g)
                    }, error = function(e) {
                        coefs[g_idx] <<- 0
                    })
                }
                return(coefs)
            }

            # Fallback: ridge via grpreg
            tryCatch({
                ridge_fit <- grpreg::grpsurv(
                    X = group_data$x,
                    y = group_data$y,
                    group = group_data$groups,
                    penalty = "grLasso",
                    nlambda = 20
                )
                # Use coefficients at a moderate lambda
                mid_idx <- max(1, ncol(ridge_fit$beta) %/% 4)
                coefs <- as.numeric(ridge_fit$beta[, mid_idx])
                return(coefs)
            }, error = function(e) {
                return(rep(0.01, n_vars))
            })
        },

        .computeLambda1se = function(cv_result) {
            cve <- cv_result$cve
            cvse <- cv_result$cvse
            lambda <- cv_result$lambda

            if (is.null(cvse) || all(is.na(cvse))) {
                return(cv_result$lambda.min)
            }

            idx_min <- which.min(cve)
            threshold <- cve[idx_min] + cvse[idx_min]
            # Find largest lambda (most regularization) with error within 1SE
            candidates <- which(cve <= threshold)
            if (length(candidates) > 0) {
                return(lambda[min(candidates)])
            }
            return(cv_result$lambda.min)
        },

        # ══════════════════════════════════════════════════════════════
        # Stability Selection
        # ══════════════════════════════════════════════════════════════
        .stabilitySelection = function(group_data) {
            n_boot <- self$options$bootstrap_samples
            subsample_ratio <- 0.8
            n_groups <- max(group_data$groups)
            group_vector <- group_data$groups
            group_multiplier <- private$.buildGroupMultiplier(group_data)

            selection_matrix <- matrix(0, nrow = n_boot, ncol = n_groups)

            set.seed(self$options$random_seed)

            for (b in seq_len(n_boot)) {
                n_sub <- floor(nrow(group_data$x) * subsample_ratio)
                boot_idx <- sample(nrow(group_data$x), n_sub, replace = FALSE)

                boot_x <- group_data$x[boot_idx, , drop = FALSE]
                boot_y <- group_data$y[boot_idx]

                tryCatch({
                    boot_cv <- grpreg::cv.grpsurv(
                        X = boot_x,
                        y = boot_y,
                        group = group_vector,
                        penalty = "grLasso",
                        group.multiplier = group_multiplier,
                        nfolds = min(5, self$options$cv_folds),
                        seed = self$options$random_seed + b
                    )

                    boot_coef <- as.numeric(coef(boot_cv$fit, lambda = boot_cv$lambda.min))
                    if (length(boot_coef) == ncol(group_data$x) + 1) {
                        boot_coef <- boot_coef[-1]
                    }

                    for (g in seq_len(n_groups)) {
                        group_vars <- which(group_vector == g)
                        if (length(group_vars) > 0) {
                            group_selected <- any(abs(boot_coef[group_vars]) > 1e-8)
                            selection_matrix[b, g] <- as.numeric(group_selected)
                        }
                    }
                }, error = function(e) {
                    # Skip this subsample on error
                })
            }

            selection_freq <- colMeans(selection_matrix)
            stable_groups <- which(selection_freq >= self$options$stability_threshold)

            return(list(
                selection_frequencies = selection_freq,
                stable_groups = stable_groups,
                selection_matrix = selection_matrix
            ))
        },

        # ══════════════════════════════════════════════════════════════
        # Nested Cross-Validation
        # ══════════════════════════════════════════════════════════════
        .nestedCrossValidation = function(group_data) {
            outer_folds <- self$options$cv_folds
            inner_folds <- self$options$inner_cv_folds
            n <- nrow(group_data$x)
            group_vector <- group_data$groups
            group_multiplier <- private$.buildGroupMultiplier(group_data)

            set.seed(self$options$random_seed)
            fold_ids <- sample(rep(seq_len(outer_folds), length.out = n))

            outer_performance <- numeric(outer_folds)
            optimal_lambdas <- numeric(outer_folds)

            for (k in seq_len(outer_folds)) {
                test_idx <- which(fold_ids == k)
                train_idx <- which(fold_ids != k)

                train_x <- group_data$x[train_idx, , drop = FALSE]
                train_y <- group_data$y[train_idx]
                test_x <- group_data$x[test_idx, , drop = FALSE]
                test_y <- group_data$y[test_idx]

                tryCatch({
                    inner_cv <- grpreg::cv.grpsurv(
                        X = train_x,
                        y = train_y,
                        group = group_vector,
                        penalty = "grLasso",
                        group.multiplier = group_multiplier,
                        nfolds = inner_folds,
                        seed = self$options$random_seed + k
                    )

                    optimal_lambdas[k] <- inner_cv$lambda.min

                    # Evaluate on held-out fold using concordance (C-index)
                    test_coef <- as.numeric(coef(inner_cv$fit, lambda = inner_cv$lambda.min))
                    if (length(test_coef) == ncol(group_data$x) + 1) {
                        test_coef <- test_coef[-1]
                    }
                    lp_test <- as.numeric(test_x %*% test_coef)

                    conc <- survival::concordance(test_y ~ lp_test, reverse = TRUE)
                    outer_performance[k] <- conc$concordance
                }, error = function(e) {
                    outer_performance[k] <<- NA
                    optimal_lambdas[k] <<- NA
                })
            }

            return(list(
                outer_performance = outer_performance,
                optimal_lambdas = optimal_lambdas
            ))
        },

        # ══════════════════════════════════════════════════════════════
        # Permutation Test
        # ══════════════════════════════════════════════════════════════
        .runPermutationTest = function(group_data) {
            n_perm <- self$options$n_permutations
            group_vector <- group_data$groups
            group_multiplier <- private$.buildGroupMultiplier(group_data)

            # Observed model
            set.seed(self$options$random_seed)
            obs_cv <- tryCatch({
                grpreg::cv.grpsurv(
                    X = group_data$x, y = group_data$y,
                    group = group_vector,
                    penalty = "grLasso",
                    group.multiplier = group_multiplier,
                    nfolds = min(5, self$options$cv_folds),
                    seed = self$options$random_seed
                )
            }, error = function(e) NULL)

            if (is.null(obs_cv)) return(NULL)

            obs_coef <- as.numeric(coef(obs_cv$fit, lambda = obs_cv$lambda.min))
            if (length(obs_coef) == ncol(group_data$x) + 1) obs_coef <- obs_coef[-1]
            obs_n_groups <- length(unique(group_vector[abs(obs_coef) > 1e-8]))
            obs_deviance <- min(obs_cv$cve, na.rm = TRUE)

            obs_lp <- as.numeric(group_data$x %*% obs_coef)
            obs_c <- tryCatch({
                conc <- survival::concordance(group_data$y ~ obs_lp, reverse = TRUE)
                conc$concordance
            }, error = function(e) NA)

            # Permutation loop
            perm_n_groups <- numeric(n_perm)
            perm_deviance <- numeric(n_perm)
            perm_c_index <- numeric(n_perm)

            for (b in seq_len(n_perm)) {
                perm_idx <- sample(nrow(group_data$x))
                perm_y <- group_data$y[perm_idx]

                tryCatch({
                    perm_cv <- grpreg::cv.grpsurv(
                        X = group_data$x, y = perm_y,
                        group = group_vector,
                        penalty = "grLasso",
                        group.multiplier = group_multiplier,
                        nfolds = min(5, self$options$cv_folds),
                        seed = self$options$random_seed + b
                    )
                    perm_coef <- as.numeric(coef(perm_cv$fit, lambda = perm_cv$lambda.min))
                    if (length(perm_coef) == ncol(group_data$x) + 1) perm_coef <- perm_coef[-1]
                    perm_n_groups[b] <- length(unique(group_vector[abs(perm_coef) > 1e-8]))
                    perm_deviance[b] <- min(perm_cv$cve, na.rm = TRUE)

                    perm_lp <- as.numeric(group_data$x %*% perm_coef)
                    conc <- survival::concordance(perm_y ~ perm_lp, reverse = TRUE)
                    perm_c_index[b] <- conc$concordance
                }, error = function(e) {
                    perm_n_groups[b] <<- 0
                    perm_deviance[b] <<- NA
                    perm_c_index[b] <<- 0.5
                })
            }

            list(
                obs_n_groups = obs_n_groups,
                obs_deviance = obs_deviance,
                obs_c_index = obs_c,
                perm_n_groups = perm_n_groups,
                perm_deviance = perm_deviance,
                perm_c_index = perm_c_index
            )
        },

        # ══════════════════════════════════════════════════════════════
        # Results Population
        # ══════════════════════════════════════════════════════════════
        .populateResults = function(group_results, group_data) {
            if (self$options$show_group_summary) {
                private$.populateGroupSummary(group_results, group_data)
            }

            if (self$options$show_coefficients) {
                private$.populateCoefficients(group_results, group_data)
            }

            if (self$options$show_path_summary) {
                private$.populatePathSummary(group_results)
            }

            if (self$options$show_cv_results) {
                private$.populateCVResults(group_results)
            }

            if (self$options$stability_selection && !is.null(group_results$stability)) {
                private$.populateStabilityResults(group_results, group_data)
            }

            if (self$options$nested_cv && !is.null(group_results$nested_cv)) {
                private$.populateNestedCVResults(group_results, group_data)
            }

            if (self$options$permutation_test) {
                perm_results <- private$.runPermutationTest(group_data)
                if (!is.null(perm_results)) {
                    private$.populatePermutationResults(perm_results, group_results, group_data)
                }
            }

            # Performance metrics
            private$.populatePerformance(group_results, group_data)

            # Plots
            if (self$options$plot_regularization_path) {
                private$.setPathPlotState(group_results, group_data)
            }
            if (self$options$plot_cv_curve) {
                private$.setCVPlotState(group_results)
            }
            if (self$options$plot_group_importance) {
                private$.setImportancePlotState(group_results, group_data)
            }
            if (self$options$plot_stability && self$options$stability_selection &&
                !is.null(group_results$stability)) {
                private$.setStabilityPlotState(group_results, group_data)
            }
            if (self$options$plot_group_structure) {
                private$.setGroupStructurePlotState(group_data)
            }
        },

        .populateGroupSummary = function(group_results, group_data) {
            group_info <- group_data$group_info
            coef_vec <- group_results$coef_min
            group_vector <- group_data$groups
            group_multiplier <- group_results$group_multiplier

            selected_groups <- character(nrow(group_info))
            group_norms <- numeric(nrow(group_info))
            for (i in seq_len(nrow(group_info))) {
                g_idx <- which(group_vector == group_info$group_id[i])
                gnorm <- sqrt(sum(coef_vec[g_idx]^2))
                group_norms[i] <- gnorm
                selected_groups[i] <- if (gnorm > self$options$selection_threshold) "Selected" else "Not Selected"
            }

            entry_order <- rank(-group_norms, ties.method = "min")

            table <- self$results$groupSummary
            for (i in seq_len(nrow(group_info))) {
                g_idx <- which(group_vector == group_info$group_id[i])
                table$addRow(rowKey = i, values = list(
                    group_id = group_info$group_id[i],
                    group_name = group_info$group_name[i],
                    variables = paste(group_data$var_names[g_idx], collapse = ", "),
                    group_size = group_info$group_size[i],
                    group_weight = group_multiplier[i],
                    selected = selected_groups[i],
                    selection_order = entry_order[i]
                ))
            }
        },

        .populateCoefficients = function(group_results, group_data) {
            coef_vec <- group_results$coef_min
            group_vector <- group_data$groups
            var_names <- group_data$var_names
            group_info <- group_data$group_info

            # Compute per-variable group norm
            group_norms <- numeric(length(coef_vec))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                g_idx <- which(group_vector == gid)
                gnorm <- sqrt(sum(coef_vec[g_idx]^2))
                group_norms[g_idx] <- gnorm
            }

            max_abs <- max(abs(coef_vec), na.rm = TRUE)
            rel_importance <- if (max_abs > 0) abs(coef_vec) / max_abs else rep(0, length(coef_vec))

            coef_data <- data.frame(
                group_id = group_vector,
                variable = var_names,
                coefficient = coef_vec,
                exp_coefficient = exp(coef_vec),
                group_norm = group_norms,
                relative_importance = rel_importance,
                selected = ifelse(abs(coef_vec) > self$options$selection_threshold, "Yes", "No"),
                stringsAsFactors = FALSE
            )

            # Show selected variables if any; otherwise show all
            selected_vars <- coef_data[coef_data$selected == "Yes", ]
            display_data <- if (nrow(selected_vars) > 0) selected_vars else coef_data

            table <- self$results$coefficients
            for (i in seq_len(nrow(display_data))) {
                table$addRow(rowKey = i, values = list(
                    group_id = display_data$group_id[i],
                    variable = display_data$variable[i],
                    coefficient = display_data$coefficient[i],
                    exp_coefficient = display_data$exp_coefficient[i],
                    group_norm = display_data$group_norm[i],
                    relative_importance = display_data$relative_importance[i],
                    selected = display_data$selected[i]
                ))
            }
        },

        .populatePathSummary = function(group_results) {
            full_fit <- group_results$cv_fit
            if (is.null(full_fit)) return()

            lambda_seq <- full_fit$lambda
            beta_matrix <- full_fit$beta  # p x nlambda matrix (grpreg)
            n_steps <- min(20, length(lambda_seq))
            step_indices <- round(seq(1, length(lambda_seq), length.out = n_steps))
            group_vector <- group_results$group_vector

            table <- self$results$pathSummary
            for (j in seq_along(step_indices)) {
                idx <- step_indices[j]
                coefs_at_lambda <- beta_matrix[, idx]
                selected <- abs(coefs_at_lambda) > 1e-10
                n_vars_sel <- sum(selected)
                n_groups_sel <- if (any(selected)) length(unique(group_vector[selected])) else 0L

                # grpreg uses $loss for deviance and may not have $df
                dev_val <- if (!is.null(full_fit$loss)) full_fit$loss[idx] else NA
                df_val <- n_vars_sel  # approximate df as number of selected variables

                table$addRow(rowKey = j, values = list(
                    step = as.integer(j),
                    lambda = lambda_seq[idx],
                    n_groups_selected = as.integer(n_groups_sel),
                    n_variables_selected = as.integer(n_vars_sel),
                    deviance = dev_val,
                    df = df_val
                ))
            }
        },

        .populateCVResults = function(group_results) {
            cv_res <- group_results$cv_result
            if (is.null(cv_res)) return()

            # CV error at lambda.min
            idx_min <- which(cv_res$lambda == cv_res$lambda.min)
            cv_error_min <- if (length(idx_min) > 0) cv_res$cve[idx_min[1]] else min(cv_res$cve, na.rm = TRUE)

            # CV error at lambda.1se
            lambda_1se <- group_results$lambda_1se
            idx_1se <- which.min(abs(cv_res$lambda - lambda_1se))
            cv_error_1se <- if (length(idx_1se) > 0) cv_res$cve[idx_1se[1]] else NA

            # Count groups at each lambda
            group_vector <- group_results$group_vector
            coef_min_vec <- group_results$coef_min
            coef_1se_vec <- group_results$coef_1se

            count_groups <- function(cv) {
                sel <- abs(cv) > 1e-10
                if (any(sel)) length(unique(group_vector[sel])) else 0L
            }

            table <- self$results$cvResults
            table$addRow(rowKey = 1, values = list(
                criterion = "Cross-Validation (Partial Likelihood Deviance)",
                lambda_min = group_results$lambda_min,
                lambda_1se = lambda_1se,
                cv_error_min = cv_error_min,
                cv_error_1se = cv_error_1se,
                groups_min = count_groups(coef_min_vec),
                groups_1se = count_groups(coef_1se_vec)
            ))
        },

        .populateStabilityResults = function(group_results, group_data) {
            stability <- group_results$stability
            group_info <- group_data$group_info

            table <- self$results$stabilityResults
            for (i in seq_len(nrow(group_info))) {
                table$addRow(rowKey = i, values = list(
                    group_id = group_info$group_id[i],
                    group_name = group_info$group_name[i],
                    selection_frequency = stability$selection_frequencies[i],
                    stability_score = stability$selection_frequencies[i],
                    stable_selection = if (stability$selection_frequencies[i] >= self$options$stability_threshold) "Stable" else "Unstable",
                    first_selected = NA
                ))
            }
        },

        .populateNestedCVResults = function(group_results, group_data) {
            nested <- group_results$nested_cv
            outer_folds <- length(nested$outer_performance)

            table <- self$results$nestedCVResults
            for (i in seq_len(outer_folds)) {
                table$addRow(rowKey = i, values = list(
                    outer_fold = as.integer(i),
                    optimal_lambda = nested$optimal_lambdas[i],
                    n_groups_selected = NA_integer_,
                    performance = nested$outer_performance[i],
                    training_error = NA_real_
                ))
            }
        },

        .populatePermutationResults = function(perm_results, group_results, group_data) {
            table <- self$results$permutationResults

            p_groups <- mean(perm_results$perm_n_groups >= perm_results$obs_n_groups, na.rm = TRUE)
            table$addRow(rowKey = 1, values = list(
                test_statistic = "N Groups Selected",
                observed_value = perm_results$obs_n_groups,
                permutation_mean = mean(perm_results$perm_n_groups, na.rm = TRUE),
                permutation_sd = sd(perm_results$perm_n_groups, na.rm = TRUE),
                p_value = p_groups
            ))

            valid_dev <- !is.na(perm_results$perm_deviance)
            p_dev <- if (any(valid_dev)) {
                mean(perm_results$perm_deviance[valid_dev] <= perm_results$obs_deviance)
            } else NA
            table$addRow(rowKey = 2, values = list(
                test_statistic = "CV Partial Likelihood Deviance",
                observed_value = perm_results$obs_deviance,
                permutation_mean = mean(perm_results$perm_deviance, na.rm = TRUE),
                permutation_sd = sd(perm_results$perm_deviance, na.rm = TRUE),
                p_value = p_dev
            ))

            valid_c <- !is.na(perm_results$perm_c_index)
            p_c <- if (any(valid_c)) {
                mean(perm_results$perm_c_index[valid_c] >= perm_results$obs_c_index)
            } else NA
            table$addRow(rowKey = 3, values = list(
                test_statistic = "Concordance Index",
                observed_value = perm_results$obs_c_index,
                permutation_mean = mean(perm_results$perm_c_index, na.rm = TRUE),
                permutation_sd = sd(perm_results$perm_c_index, na.rm = TRUE),
                p_value = p_c
            ))
        },

        .populatePerformance = function(group_results, group_data) {
            coef_vec <- group_results$coef_min
            group_vector <- group_data$groups
            selected <- abs(coef_vec) > self$options$selection_threshold
            n_vars_selected <- sum(selected)
            n_groups_selected <- if (any(selected)) length(unique(group_vector[selected])) else 0

            cv_res <- group_results$cv_result
            cv_error <- if (!is.null(cv_res)) {
                idx <- which(cv_res$lambda == cv_res$lambda.min)
                if (length(idx) > 0) cv_res$cve[idx[1]] else min(cv_res$cve, na.rm = TRUE)
            } else { NA }

            # Compute C-index on training data
            c_index <- NA
            if (n_vars_selected > 0) {
                tryCatch({
                    lp <- as.numeric(group_data$x %*% coef_vec)
                    conc <- survival::concordance(group_data$y ~ lp, reverse = TRUE)
                    c_index <- conc$concordance
                }, error = function(e) {})
            } else {
                c_index <- 0.500
            }

            # C-index clinical notice
            if (!is.na(c_index) && c_index < 0.5 && n_vars_selected > 0) {
                private$.insertNotice(
                    'poorDiscrimination', jmvcore::NoticeType$STRONG_WARNING,
                    sprintf("Training C-index = %.3f is below 0.5, indicating the model predicts worse than chance. Review variable selection and data quality.", c_index),
                    position = 1
                )
            }

            metrics <- c("Number of Groups Selected", "Number of Variables Selected",
                         "CV Partial Likelihood Deviance", "Training Concordance Index")
            values <- c(n_groups_selected, n_vars_selected, cv_error, c_index)
            ci_texts <- c("", "", "", "")
            descriptions <- c(
                "Groups with at least one non-zero coefficient",
                "Individual variables with non-zero coefficients",
                "Cross-validation error at optimal lambda",
                "Concordance index on training data (optimistic)"
            )

            table <- self$results$modelPerformance
            for (i in seq_along(metrics)) {
                table$addRow(rowKey = i, values = list(
                    metric = metrics[i],
                    value = values[i],
                    confidence_interval = ci_texts[i],
                    description = descriptions[i]
                ))
            }

            note <- "<p><i>Note: The Training Concordance Index overestimates true out-of-sample performance, especially for high-dimensional models. Use Cross-Validation or Nested CV for a realistic assessment.</i></p>"
            self$results$modelPerformanceNote$setContent(note)
        },

        .initResults = function() {
            self$results$groupSummary$setVisible(self$options$show_group_summary)
            self$results$coefficients$setVisible(self$options$show_coefficients)
            self$results$pathSummary$setVisible(self$options$show_path_summary)
            self$results$cvResults$setVisible(self$options$show_cv_results)
            self$results$stabilityResults$setVisible(self$options$stability_selection)
            self$results$nestedCVResults$setVisible(self$options$nested_cv)
            self$results$permutationResults$setVisible(self$options$permutation_test)
        },

        # ══════════════════════════════════════════════════════════════
        # Data Suitability Assessment
        # ══════════════════════════════════════════════════════════════
        .assessSuitability = function(pred_matrix, time_var, status_var) {
            checks <- list()

            n <- nrow(pred_matrix)
            n_events <- sum(status_var == 1)
            p <- ncol(pred_matrix)
            event_rate <- n_events / n

            # -- Check 1: Events-Per-Variable (EPV) --
            epv <- n_events / p
            if (epv >= 10) {
                checks$epv <- list(
                    color = "green", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "High EPV. Model estimation will be robust."
                )
            } else if (epv >= 1) {
                checks$epv <- list(
                    color = "yellow", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "Low EPV. Standard Cox would struggle, but Group LASSO handles this well."
                )
            } else {
                checks$epv <- list(
                    color = "red", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.3f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "Ultra-low EPV (< 1). Group LASSO regularization is absolutely necessary."
                )
            }

            # -- Check 2: Regularization/Reduction Need --
            if (p >= n / 3) {
                checks$regularization <- list(
                    color = "green", label = "Reduction Need",
                    value = sprintf("p=%d, n=%d (ratio=%.2f)", p, n, p / n),
                    detail = "High-dimensional setting. Group LASSO is strongly indicated."
                )
            } else {
                checks$regularization <- list(
                    color = "yellow", label = "Reduction Need",
                    value = sprintf("p=%d, EPV=%.0f", p, epv),
                    detail = "Moderate/low dimensionality. Group LASSO is still valid for group selection."
                )
            }

            # -- Check 3: Sample Size --
            if (n >= 100) {
                checks$sample_size <- list(
                    color = "green", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Adequate sample size for cross-validation."
                )
            } else if (n >= 30) {
                checks$sample_size <- list(
                    color = "yellow", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Small sample. Consider reducing the number of CV folds (e.g., 5-fold instead of 10)."
                )
            } else {
                checks$sample_size <- list(
                    color = "red", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Very small sample. Results will be highly variable and CV may not converge."
                )
            }

            # -- Check 4: Multicollinearity --
            tryCatch({
                if (p <= 2000 && p >= 2) {
                    # Convert any non-numeric columns to numeric
                    numeric_mat <- matrix(NA, nrow = n, ncol = p)
                    for (j in seq_len(p)) {
                        numeric_mat[, j] <- as.numeric(pred_matrix[, j])
                    }
                    cor_matrix <- cor(numeric_mat, use = "pairwise.complete.obs")
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
                            detail = "Moderate collinearity. Group LASSO handles structure better than standard LASSO."
                        )
                    } else {
                        checks$collinearity <- list(
                            color = "yellow", label = "Multicollinearity",
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = "High collinearity. Combining highly correlated variables into the same group is recommended."
                        )
                    }
                }
            }, error = function(e) {
                NULL
            })

            # -- Check 5: Data Quality --
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
                overall_text <- "Data is well-suited for Group LASSO modeling."
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
        },

        # ══════════════════════════════════════════════════════════════
        # Plot setState Methods
        # ══════════════════════════════════════════════════════════════
        .setPathPlotState = function(group_results, group_data) {
            image <- self$results$pathPlot
            full_fit <- group_results$cv_fit
            beta_matrix <- full_fit$beta  # p x nlambda (grpreg)
            plot_data <- list(
                lambda = as.numeric(full_fit$lambda),
                beta = as.data.frame(t(beta_matrix)),
                var_names = as.character(rownames(beta_matrix)),
                lambda_min = as.numeric(group_results$lambda_min),
                lambda_1se = as.numeric(group_results$lambda_1se),
                group_vector = as.integer(group_data$groups),
                group_names = as.character(group_data$group_info$group_name)
            )
            image$setState(plot_data)
        },

        .setCVPlotState = function(group_results) {
            image <- self$results$cvPlot
            cv_res <- group_results$cv_result
            plot_data <- list(
                lambda = as.numeric(cv_res$lambda),
                cvm = as.numeric(cv_res$cve),
                cvsd = as.numeric(cv_res$cvse),
                cvup = as.numeric(cv_res$cve + cv_res$cvse),
                cvlo = as.numeric(cv_res$cve - cv_res$cvse),
                lambda_min = as.numeric(cv_res$lambda.min),
                lambda_1se = as.numeric(group_results$lambda_1se)
            )
            image$setState(plot_data)
        },

        .setImportancePlotState = function(group_results, group_data) {
            image <- self$results$importancePlot
            coef_vec <- as.numeric(group_results$coef_min)
            var_names <- group_data$var_names
            group_vector <- group_data$groups
            group_info <- group_data$group_info

            group_importance <- numeric(nrow(group_info))
            for (i in seq_len(nrow(group_info))) {
                gid <- group_info$group_id[i]
                g_idx <- which(group_vector == gid)
                group_importance[i] <- sqrt(sum(coef_vec[g_idx]^2))
            }

            plot_data <- list(
                var_names = as.character(var_names),
                var_importance = as.numeric(abs(coef_vec)),
                coef_values = coef_vec,
                group_vector = as.integer(group_vector),
                group_names = as.character(group_info$group_name),
                group_importance = as.numeric(group_importance)
            )
            image$setState(plot_data)
        },

        .setStabilityPlotState = function(group_results, group_data) {
            stability <- group_results$stability
            group_info <- group_data$group_info

            plot_data <- list(
                group_names = as.character(group_info$group_name),
                selection_freq = as.numeric(stability$selection_frequencies),
                threshold = as.numeric(self$options$stability_threshold)
            )
            self$results$stabilityPlot$setState(plot_data)
        },

        .setGroupStructurePlotState = function(group_data) {
            group_info <- group_data$group_info
            group_vector <- group_data$groups
            var_names <- group_data$var_names

            plot_data <- list(
                var_names = as.character(var_names),
                group_vector = as.integer(group_vector),
                group_names = as.character(group_info$group_name),
                group_sizes = as.integer(group_info$group_size)
            )
            self$results$groupStructurePlot$setState(plot_data)
        },

        # ══════════════════════════════════════════════════════════════
        # Plot Render Functions
        # ══════════════════════════════════════════════════════════════
        .renderPathPlot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            if (is.null(plotData)) return(FALSE)

            lambda_vals <- plotData$lambda
            beta_df <- plotData$beta
            var_names <- plotData$var_names
            group_vector <- plotData$group_vector

            if (is.null(lambda_vals) || length(lambda_vals) == 0) return(FALSE)

            n_lambda <- length(lambda_vals)
            n_vars <- length(var_names)
            plot_df <- data.frame(
                log_lambda = rep(log(lambda_vals), each = n_vars),
                coefficient = unlist(lapply(seq_len(n_lambda), function(i) {
                    as.numeric(beta_df[i, ])
                })),
                variable = rep(var_names, times = n_lambda),
                group = rep(as.character(group_vector), times = n_lambda),
                stringsAsFactors = FALSE
            )

            plot <- ggplot2::ggplot(plot_df,
                ggplot2::aes(x = log_lambda, y = coefficient,
                             color = group, group = variable)) +
                ggplot2::geom_line(alpha = 0.7) +
                ggplot2::geom_vline(xintercept = log(plotData$lambda_min),
                    linetype = "dashed", color = "red") +
                ggplot2::geom_vline(xintercept = log(plotData$lambda_1se),
                    linetype = "dotted", color = "blue") +
                ggplot2::labs(
                    x = expression(log(lambda)),
                    y = "Coefficient",
                    title = "Group Regularization Path",
                    color = "Group"
                ) +
                ggtheme

            print(plot)
            TRUE
        },

        .renderCVPlot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            if (is.null(plotData)) return(FALSE)

            cv_df <- data.frame(
                log_lambda = log(plotData$lambda),
                cvm = plotData$cvm,
                cvup = plotData$cvup,
                cvlo = plotData$cvlo
            )

            plot <- ggplot2::ggplot(cv_df,
                ggplot2::aes(x = log_lambda, y = cvm)) +
                ggplot2::geom_point(color = "red", size = 1.5) +
                ggplot2::geom_errorbar(
                    ggplot2::aes(ymin = cvlo, ymax = cvup),
                    width = 0.02, alpha = 0.4) +
                ggplot2::geom_vline(xintercept = log(plotData$lambda_min),
                    linetype = "dashed", color = "red") +
                ggplot2::geom_vline(xintercept = log(plotData$lambda_1se),
                    linetype = "dotted", color = "blue") +
                ggplot2::labs(
                    x = expression(log(lambda)),
                    y = "Cross-Validation Error",
                    title = "Cross-Validation Curve"
                ) +
                ggtheme

            print(plot)
            TRUE
        },

        .renderImportancePlot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            if (is.null(plotData)) return(FALSE)

            group_names <- plotData$group_names
            group_importance <- plotData$group_importance

            if (all(group_importance == 0)) return(FALSE)

            imp_df <- data.frame(
                group = factor(group_names, levels = group_names[order(group_importance)]),
                importance = group_importance,
                stringsAsFactors = FALSE
            )
            imp_df <- imp_df[imp_df$importance > 0, , drop = FALSE]
            if (nrow(imp_df) == 0) return(FALSE)

            plot <- ggplot2::ggplot(imp_df,
                ggplot2::aes(x = group, y = importance)) +
                ggplot2::geom_col(fill = "#3498db", alpha = 0.8) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    x = "",
                    y = "Group Importance (L2 Norm)",
                    title = "Group Importance"
                ) +
                ggtheme

            print(plot)
            TRUE
        },

        .renderStabilityPlot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            if (is.null(plotData)) return(FALSE)

            stab_df <- data.frame(
                group = factor(plotData$group_names,
                    levels = plotData$group_names[order(plotData$selection_freq)]),
                freq = plotData$selection_freq,
                stringsAsFactors = FALSE
            )

            plot <- ggplot2::ggplot(stab_df,
                ggplot2::aes(x = group, y = freq)) +
                ggplot2::geom_col(fill = "#2ecc71", alpha = 0.8) +
                ggplot2::geom_hline(yintercept = plotData$threshold,
                    linetype = "dashed", color = "red") +
                ggplot2::coord_flip() +
                ggplot2::scale_y_continuous(labels = scales::percent_format(),
                    limits = c(0, 1)) +
                ggplot2::labs(
                    x = "",
                    y = "Selection Frequency",
                    title = "Stability Selection"
                ) +
                ggplot2::annotate("text",
                    x = 1, y = plotData$threshold + 0.03,
                    label = paste0("Threshold = ", plotData$threshold),
                    color = "red", hjust = 0, size = 3) +
                ggtheme

            print(plot)
            TRUE
        },

        .renderGroupStructurePlot = function(image, ggtheme, theme, ...) {
            plotData <- image$state
            if (is.null(plotData)) return(FALSE)

            var_names <- plotData$var_names
            group_vector <- plotData$group_vector
            group_names <- plotData$group_names

            struct_df <- data.frame(
                variable = factor(var_names, levels = rev(var_names)),
                group = factor(
                    group_names[group_vector],
                    levels = unique(group_names)
                ),
                stringsAsFactors = FALSE
            )

            plot <- ggplot2::ggplot(struct_df,
                ggplot2::aes(x = group, y = variable, fill = group)) +
                ggplot2::geom_tile(color = "white", linewidth = 0.5) +
                ggplot2::labs(
                    x = "Group",
                    y = "Variable",
                    title = "Group Structure",
                    fill = "Group"
                ) +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                ) +
                ggtheme

            print(plot)
            TRUE
        }
    )
)
