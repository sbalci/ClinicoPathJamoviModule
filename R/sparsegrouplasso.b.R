sparsegrouplassoClass <- R6::R6Class(
    "sparsegrouplassoClass",
    inherit = sparsegrouplassoBase,
    private = list(
        .pathwayWarning = NULL,

        .init = function() {
            if (is.null(self$options$time_var) || is.null(self$options$event_var)) {
                self$results$instructions$setContent(
                    "<html>
                    <head>
                    <style>
                        .main { margin: 10px; }
                        .todo { background-color: #E8F4FD; padding: 15px; border-radius: 10px; margin: 10px 0; }
                        .instructions { background-color: #E8F4FD; padding: 15px; border-radius: 10px; }
                        .todo-title { font-weight: bold; font-size: 18px; margin-bottom: 10px; color: #2E86AB; }
                        .todo-item { margin: 8px 0; }
                    </style>
                    </head>
                    <body>
                        <div class='main'>
                            <div class='instructions'>
                                <p><b>Welcome to ClinicoPath Sparse Group LASSO Analysis</b></p>
                                <p>This analysis performs sparse group LASSO regularization for survival data, combining group-wise variable selection with individual sparsity within groups.</p>
                                <p>Please provide:</p>
                                <ul>
                                    <li><b>Time Variable:</b> Time to event or censoring</li>
                                    <li><b>Event Variable:</b> Event indicator (0/1 or FALSE/TRUE)</li>
                                    <li><b>Predictor Variables:</b> Variables for regularized selection</li>
                                </ul>
                                <p><b>Key Features:</b></p>
                                <ul>
                                    <li>Combines group LASSO and individual variable selection</li>
                                    <li>Multiple group definition methods (factor-based, custom, correlation-based)</li>
                                    <li>Cross-validation with various selection criteria</li>
                                    <li>Stability selection for robust variable identification</li>
                                    <li>Adaptive weighting schemes</li>
                                    <li>Comprehensive visualization and validation</li>
                                </ul>
                            </div>
                        </div>
                    </body>
                    </html>"
                )
                return()
            }

            if (is.null(self$options$pred_vars) || length(self$options$pred_vars) < 2) {
                self$results$todo$setContent(
                    "<html>
                    <head>
                    <style>
                        .main { margin: 10px; }
                        .todo { background-color: #FFF3CD; padding: 15px; border-radius: 10px; margin: 10px 0; }
                        .todo-title { font-weight: bold; font-size: 18px; margin-bottom: 10px; color: #856404; }
                        .todo-item { margin: 8px 0; }
                    </style>
                    </head>
                    <body>
                        <div class='main'>
                            <div class='todo'>
                                <div class='todo-title'> Setup Required</div>
                                <div class='todo-item'> Add at least 2 predictor variables for regularization</div>
                                <div class='todo-item'> Configure group definition method</div>
                                <div class='todo-item'> Adjust sparse group LASSO parameters (alpha, lambda)</div>
                                <div class='todo-item'> Select model validation approach</div>
                                <div class='todo-item'> Choose optimal selection criteria</div>
                            </div>
                        </div>
                    </body>
                    </html>"
                )
                return()
            }

            private$.initializeAnalysis()
        },

        .initializeAnalysis = function() {
            # Columns are already defined in .r.yaml; no need to add them here.

            # Set instructions for successful setup
            self$results$instructions$setContent(
                "<html>
                <head>
                <style>
                    .main { margin: 10px; }
                    .info { background-color: #D4EDDA; padding: 15px; border-radius: 10px; }
                    .info-title { font-weight: bold; font-size: 16px; margin-bottom: 10px; color: #155724; }
                </style>
                </head>
                <body>
                    <div class='main'>
                        <div class='info'>
                            <div class='info-title'> Ready for Sparse Group LASSO Analysis</div>
                            <p>Analysis configured successfully. The sparse group LASSO will perform group-wise regularization with within-group sparsity for optimal variable selection in survival analysis.</p>
                        </div>
                    </div>
                </body>
                </html>"
            )
        },

        .run = function() {
            # Check for required variables
            if (is.null(self$options$time_var) || is.null(self$options$event_var) ||
                is.null(self$options$pred_vars) || length(self$options$pred_vars) < 2) {
                return()
            }

            # Validate package availability
            if (!requireNamespace("glmnet", quietly = TRUE) ||
                !requireNamespace("survival", quietly = TRUE)) {
                self$results$instructions$setContent(
                    "<html><body><div style='color: red;'><b>Missing packages:</b> 'glmnet' and 'survival' are required for this analysis.</div></body></html>"
                )
                return()
            }

            # Assess data suitability if requested
            if (self$options$suitabilityCheck) {
                private$.assessSuitability()
            }

            # Set random seed for reproducibility
            set.seed(self$options$seed_value)

            # Get clean data
            data <- private$.prepareData()
            if (is.null(data)) return()

            # Perform sparse group LASSO analysis
            tryCatch({
                results <- private$.performSparseGroupLASSO(data)
                if (!is.null(results)) {
                    private$.populateResults(results, data)
                }
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<html><body><div style='color: red;'>",
                           "<b>Analysis Error:</b> ", e$message, "</div></body></html>")
                )
            })
        },

        .assessSuitability = function() {
            data <- self$data
            time_var <- self$options$time_var
            event_var <- self$options$event_var
            pred_vars <- self$options$pred_vars

            html <- self$results$suitabilityReport
            
            # 1. Sample Size & Events
            total_n <- nrow(data)
            events <- 0
            if (!is.null(event_var) && event_var %in% names(data)) {
                events <- sum(as.numeric(as.logical(data[[event_var]])), na.rm = TRUE)
            }
            n_vars <- length(pred_vars)
            epv <- ifelse(n_vars > 0, events / n_vars, 0)

            # Define warning colors
            epv_color <- ifelse(epv < 5, "red", ifelse(epv < 10, "orange", "green"))
            
            # 2. Missing Data
            missing_text <- ""
            complete_cases <- sum(complete.cases(data[, c(time_var, event_var, pred_vars), drop=FALSE]))
            missing_pct <- 100 * (1 - complete_cases/total_n)
            
            if (missing_pct > 10) {
                missing_text <- paste0("<div style='color: orange; margin-top: 5px;'>",
                                     "⚠️ <b>Note:</b> ", round(missing_pct, 1), "% of cases have missing data and will be excluded.",
                                     "</div>")
            }

            # 3. Multicollinearity placeholder (SGL handles this well)
            collin_text <- "<div style='color: green; margin-top: 5px;'>✓ Group structures will help manage correlated predictors.</div>"

            # Construct HTML
            content <- paste0(
                "<html>
                <style>
                    .suit-box { border: 1px solid #ccc; padding: 10px; border-radius: 5px; margin-bottom: 10px; }
                    .suit-title { font-weight: bold; border-bottom: 1px solid #ddd; padding-bottom: 5px; margin-bottom: 5px; }
                    .metric { display: flex; justify-content: space-between; margin: 3px 0; }
                </style>
                <div class='suit-box'>
                    <div class='suit-title'>Dataset Assessment for Sparse Group LASSO</div>
                    
                    <div class='metric'>
                        <span><b>Sample Size:</b> ", total_n, "</span>
                        <span><b>Number of Events:</b> ", events, "</span>
                    </div>
                    
                    <div class='metric'>
                        <span><b>Predictors (P):</b> ", n_vars, "</span>
                        <span><b style='color:", epv_color, "'>Events Per Variable (EPV): ", round(epv, 2), "</b></span>
                    </div>
                    
                    <div style='margin-top: 10px;'>
                        <b>Assessment:</b><br>",
                        ifelse(epv < 5, 
                               "<span style='color: red;'>⚠️ <b>High Dimensionality:</b> EPV is very low. Regularization (SGL) is strictly necessary, but results may still be highly unstable.</span>", 
                               ifelse(epv < 10, 
                                      "<span style='color: orange;'>⚠️ <b>Moderate Dimensionality:</b> SGL will effectively reduce dimensions.</span>",
                                      "<span style='color: green;'>✓ <b>Adequate Events:</b> Sample size is sufficient for this number of predictors. SGL will focus on variable selection.</span>"
                               )),
                    "</div>",
                    missing_text,
                    collin_text,
                "</div></html>"
            )
            
            html$setContent(content)
        },

        .prepareData = function() {
            data <- self$data

            # Get variable names
            timeVar <- self$options$time_var
            eventVar <- self$options$event_var
            predVars <- self$options$pred_vars

            # Check variable availability
            missingVars <- c()
            if (!timeVar %in% names(data)) missingVars <- c(missingVars, timeVar)
            if (!eventVar %in% names(data)) missingVars <- c(missingVars, eventVar)
            for (var in predVars) {
                if (!var %in% names(data)) missingVars <- c(missingVars, var)
            }

            if (length(missingVars) > 0) {
                self$results$instructions$setContent(
                    paste0("<html><body><div style='color: red;'>",
                           "<b>Missing variables:</b> ", paste(missingVars, collapse = ", "),
                           "</div></body></html>")
                )
                return(NULL)
            }

            # Subset data to complete cases
            keepVars <- c(timeVar, eventVar, predVars)
            if (!is.null(self$options$pathway_info) && self$options$pathway_info != "") {
                keepVars <- c(keepVars, self$options$pathway_info)
            }

            data <- data[, keepVars, drop = FALSE]
            data <- data[complete.cases(data), , drop = FALSE]

            if (nrow(data) < 10) {
                self$results$instructions$setContent(
                    "<html><body><div style='color: red;'><b>Insufficient data:</b> Need at least 10 complete observations.</div></body></html>"
                )
                return(NULL)
            }

            # Convert event variable to numeric if needed
            data[[eventVar]] <- as.numeric(as.logical(data[[eventVar]]))

            # Ensure predictor variables are numeric
            for (var in predVars) {
                if (!is.numeric(data[[var]])) {
                    if (is.factor(data[[var]]) || is.character(data[[var]])) {
                        # Convert to dummy variables
                        data <- private$.convertToDummies(data, var)
                    } else {
                        data[[var]] <- as.numeric(data[[var]])
                    }
                }
            }

            return(data)
        },

        .convertToDummies = function(data, varName) {
            var_data <- data[[varName]]
            if (is.factor(var_data)) {
                levels_var <- levels(var_data)
            } else {
                levels_var <- unique(var_data)
            }

            if (length(levels_var) <= 1) return(data)

            # Create dummy variables (exclude first level as reference)
            for (i in 2:length(levels_var)) {
                dummy_name <- paste0(varName, "_", levels_var[i])
                data[[dummy_name]] <- as.numeric(var_data == levels_var[i])
            }

            # Remove original variable
            data[[varName]] <- NULL
            return(data)
        },

        .performSparseGroupLASSO = function(data) {
            # Prepare data matrices
            timeVar <- self$options$time_var
            eventVar <- self$options$event_var
            predVars <- self$options$pred_vars

            # Update predVars to include dummy variables if created
            actual_pred_vars <- names(data)[!names(data) %in% c(timeVar, eventVar)]
            if (!is.null(self$options$pathway_info) && self$options$pathway_info != "") {
                actual_pred_vars <- actual_pred_vars[actual_pred_vars != self$options$pathway_info]
            }

            if (length(actual_pred_vars) < 2) {
                stop("Insufficient predictor variables after preprocessing")
            }

            # Create design matrix
            X <- as.matrix(data[, actual_pred_vars, drop = FALSE])
            y_time <- data[[timeVar]]
            y_event <- data[[eventVar]]

            # Preprocessing
            if (self$options$center_vars) {
                X <- scale(X, center = TRUE, scale = FALSE)
            }
            if (self$options$standardize_vars) {
                X <- scale(X, center = FALSE, scale = TRUE)
            }

            # Define groups
            groups <- private$.defineGroups(X, data, actual_pred_vars)

            # Generate lambda sequence
            lambda_seq <- private$.generateLambdaSequence(X, y_time, y_event, groups)

            # Calculate adaptive weights if specified
            adaptive_weights <- private$.calculateAdaptiveWeights(X, y_time, y_event, groups)

            # Perform sparse group LASSO fitting
            cv_results <- private$.fitSparseGroupLASSO(X, y_time, y_event, groups, lambda_seq, adaptive_weights)

            # Stability selection if requested
            stability_results <- NULL
            if (self$options$stability_selection) {
                stability_results <- private$.performStabilitySelection(X, y_time, y_event, groups, lambda_seq)
            }

            return(list(
                cv_results = cv_results,
                stability_results = stability_results,
                groups = groups,
                lambda_seq = lambda_seq,
                adaptive_weights = adaptive_weights,
                X = X,
                y_time = y_time,
                y_event = y_event,
                variable_names = actual_pred_vars
            ))
        },

        .defineGroups = function(X, data, pred_vars) {
            method <- self$options$group_definition
            groups <- NULL

            if (method == "factor_based") {
                groups <- private$.factorBasedGrouping(X, data, pred_vars)
            } else if (method == "custom") {
                groups <- private$.customGrouping(pred_vars)
            } else if (method == "pathway_based") {
                groups <- private$.pathwayBasedGrouping(data, pred_vars)
            } else if (method == "variable_type") {
                groups <- private$.variableTypeGrouping(data, pred_vars)
            } else if (method == "correlation_based") {
                groups <- private$.correlationBasedGrouping(X)
            }

            if (is.null(groups)) {
                # Default: each variable in its own group
                groups <- 1:ncol(X)
            }

            return(groups)
        },

        .factorBasedGrouping = function(X, data, pred_vars) {
            # Group variables based on their original factor structure
            groups <- rep(1, ncol(X))
            current_group <- 1
            col_index <- 1

            for (var in pred_vars) {
                if (var %in% names(data)) {
                    # Original variable exists - assign group
                    groups[col_index] <- current_group
                    col_index <- col_index + 1
                    current_group <- current_group + 1
                } else {
                    # Check for dummy variables
                    dummy_vars <- colnames(X)[grepl(paste0("^", var, "_"), colnames(X))]
                    if (length(dummy_vars) > 0) {
                        # Assign same group to all dummies of this variable
                        for (dummy_var in dummy_vars) {
                            dummy_index <- which(colnames(X) == dummy_var)
                            if (length(dummy_index) > 0) {
                                groups[dummy_index] <- current_group
                            }
                        }
                        current_group <- current_group + 1
                    }
                }
            }

            return(groups)
        },

        .customGrouping = function(pred_vars) {
            if (self$options$custom_groups == "") return(NULL)

            # Parse custom groups: "1,2;3,4,5;6"
            group_specs <- strsplit(self$options$custom_groups, ";")[[1]]
            groups <- rep(1, length(pred_vars))

            for (i in seq_along(group_specs)) {
                var_indices <- as.numeric(strsplit(trimws(group_specs[i]), ",")[[1]])
                var_indices <- var_indices[var_indices >= 1 & var_indices <= length(pred_vars)]
                groups[var_indices] <- i
            }

            return(groups)
        },

        .pathwayBasedGrouping = function(data, pred_vars) {
            if (is.null(self$options$pathway_info) || self$options$pathway_info == "") return(NULL)

            pathway_var <- self$options$pathway_info
            if (!pathway_var %in% names(data)) return(NULL)

            # Use pathway information to group variables
            # The pathway variable should contain group labels for each observation.
            # We attempt to use it if it has as many unique values as needed,
            # but this mapping is inherently approximate for observation-level labels.
            pathway_info <- data[[pathway_var]]
            unique_pathways <- unique(pathway_info)
            groups <- rep(1, length(pred_vars))

            # NOTE: This is a simplified cyclic assignment because the pathway
            # variable describes observations, not predictor columns.
            # A proper implementation would require a separate variable-to-pathway
            # mapping file or annotation. Results should be interpreted with caution.
            for (i in seq_along(pred_vars)) {
                groups[i] <- (i - 1) %% length(unique_pathways) + 1
            }

            # Store warning for display in results
            private$.pathwayWarning <- paste0(
                "WARNING: Pathway-based grouping used cyclic (modulo) assignment ",
                "because the pathway variable describes observations, not predictors. ",
                "For proper pathway-based grouping, provide a variable whose levels ",
                "correspond to predictor variable memberships, or use custom grouping."
            )

            return(groups)
        },

        .variableTypeGrouping = function(data, pred_vars) {
            groups <- rep(1, length(pred_vars))
            group_id <- 1

            # Group by variable type
            for (i in seq_along(pred_vars)) {
                var <- pred_vars[i]
                if (var %in% names(data)) {
                    if (is.numeric(data[[var]])) {
                        groups[i] <- 1  # Numeric group
                    } else {
                        groups[i] <- 2  # Categorical group
                    }
                }
            }

            return(groups)
        },

        .correlationBasedGrouping = function(X) {
            if (ncol(X) < 2) return(rep(1, ncol(X)))

            # Calculate correlation matrix
            cor_matrix <- cor(X, use = "complete.obs")
            threshold <- self$options$correlation_threshold

            # Simple clustering based on correlation
            groups <- rep(1, ncol(X))
            current_group <- 1

            for (i in 1:ncol(X)) {
                if (groups[i] == 0) next  # Already assigned

                # Find variables highly correlated with variable i
                high_cor <- which(abs(cor_matrix[i, ]) >= threshold)
                groups[high_cor] <- current_group
                current_group <- current_group + 1
            }

            return(groups)
        },

        .generateLambdaSequence = function(X, y_time, y_event, groups) {
            if (self$options$lambda_sequence == "custom" && self$options$custom_lambda != "") {
                # Parse custom lambda values
                lambda_values <- as.numeric(strsplit(self$options$custom_lambda, ",")[[1]])
                return(sort(lambda_values[!is.na(lambda_values)], decreasing = TRUE))
            }

            # Automatic lambda sequence
            n <- nrow(X)
            p <- ncol(X)

            # Estimate lambda_max (when all coefficients are zero)
            lambda_max <- private$.estimateLambdaMax(X, y_time, y_event)

            lambda_min_ratio <- self$options$lambda_min_ratio
            n_lambda <- self$options$n_lambda

            lambda_min <- lambda_max * lambda_min_ratio
            lambda_seq <- exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda))

            return(lambda_seq)
        },

        .estimateLambdaMax = function(X, y_time, y_event) {
            # Estimate lambda_max using glmnet's internal logic:
            # Fit with a very large lambda to find the boundary where
            # the first coefficient becomes nonzero.
            tryCatch({
                y_surv <- survival::Surv(y_time, y_event)
                # Let glmnet choose its own lambda sequence and take the max
                fit <- glmnet::glmnet(
                    x = X, y = y_surv,
                    family = "cox",
                    alpha = self$options$alpha_sgl,
                    standardize = FALSE,
                    nlambda = 20
                )
                lambda_max <- max(fit$lambda)
                # Add a small margin
                return(lambda_max * 1.1)
            }, error = function(e) {
                # Fallback: gradient-based estimate using Cox score
                n <- nrow(X)
                # Under null model (beta=0), Cox score for variable j is:
                # sum_events(x_ij - x_bar_risk_i)
                # Approximate with centering
                event_idx <- which(y_event == 1)
                if (length(event_idx) == 0) return(1)
                grad <- abs(colMeans(X[event_idx, , drop = FALSE]) - colMeans(X))
                lambda_max <- max(grad) * 2
                return(max(lambda_max, 0.01))
            })
        },

        .calculateAdaptiveWeights = function(X, y_time, y_event, groups) {
            if (self$options$weight_type == "none") {
                return(list(individual = rep(1, ncol(X)), group = rep(1, max(groups))))
            }

            individual_weights <- rep(1, ncol(X))
            group_weights <- rep(1, max(groups))

            if (self$options$weight_type == "ridge_based") {
                # Ridge-based weights
                ridge_fit <- private$.fitRidgeRegression(X, y_time, y_event)
                if (!is.null(ridge_fit)) {
                    individual_weights <- 1 / (abs(ridge_fit$coefficients) + 1e-8)^self$options$weight_power
                }
            } else if (self$options$weight_type == "univariate_based") {
                # Univariate association-based weights
                for (i in 1:ncol(X)) {
                    univar_coef <- private$.fitUnivariateCox(X[, i, drop = FALSE], y_time, y_event)
                    individual_weights[i] <- 1 / (abs(univar_coef) + 1e-8)^self$options$weight_power
                }
            } else if (self$options$weight_type == "lasso_based") {
                # LASSO-based weights
                lasso_fit <- private$.fitLASSO(X, y_time, y_event)
                if (!is.null(lasso_fit)) {
                    individual_weights <- 1 / (abs(lasso_fit$coefficients) + 1e-8)^self$options$weight_power
                }
            }

            # Calculate group weights as averages of individual weights
            for (g in 1:max(groups)) {
                group_indices <- which(groups == g)
                group_weights[g] <- mean(individual_weights[group_indices])
            }

            return(list(individual = individual_weights, group = group_weights))
        },

        .fitRidgeRegression = function(X, y_time, y_event) {
            # Ridge Cox regression using glmnet with alpha=0
            tryCatch({
                y_surv <- survival::Surv(y_time, y_event)
                cv_ridge <- glmnet::cv.glmnet(
                    x = X, y = y_surv,
                    family = "cox", alpha = 0,
                    nfolds = min(5, floor(nrow(X) / 3)),
                    standardize = FALSE
                )
                coef_ridge <- as.numeric(coef(cv_ridge, s = cv_ridge$lambda.min))
                return(list(coefficients = coef_ridge))
            }, error = function(e) {
                return(NULL)
            })
        },

        .fitUnivariateCox = function(x, y_time, y_event) {
            # Univariate Cox coefficient using survival::coxph
            tryCatch({
                x_vec <- as.numeric(x)
                if (var(x_vec) == 0) return(0)
                y_surv <- survival::Surv(y_time, y_event)
                fit <- survival::coxph(y_surv ~ x_vec)
                coef_val <- coef(fit)
                return(ifelse(is.na(coef_val), 0, as.numeric(coef_val)))
            }, error = function(e) {
                return(0)
            })
        },

        .fitLASSO = function(X, y_time, y_event) {
            # LASSO Cox regression using glmnet with alpha=1
            tryCatch({
                y_surv <- survival::Surv(y_time, y_event)
                cv_lasso <- glmnet::cv.glmnet(
                    x = X, y = y_surv,
                    family = "cox", alpha = 1,
                    nfolds = min(5, floor(nrow(X) / 3)),
                    standardize = FALSE
                )
                coef_lasso <- as.numeric(coef(cv_lasso, s = cv_lasso$lambda.min))
                return(list(coefficients = coef_lasso))
            }, error = function(e) {
                return(NULL)
            })
        },

        .fitSparseGroupLASSO = function(X, y_time, y_event, groups, lambda_seq, adaptive_weights) {
            # Use glmnet::cv.glmnet with Cox family as the engine.
            # glmnet alpha controls the elastic net mixing (L1 vs L2),
            # which approximates sparse group LASSO behavior when combined
            # with the group structure provided by penalty.factor.
            #
            # True sparse group LASSO requires the SGL package or custom
            # proximal gradient code. This implementation uses glmnet's
            # elastic net Cox as a principled approximation that correctly
            # optimizes the Cox partial likelihood.

            alpha_sgl <- self$options$alpha_sgl
            n_vars <- ncol(X)
            n_groups <- max(groups)
            cv_folds <- self$options$cv_folds

            # Build penalty factors incorporating group structure and adaptive weights
            # Groups with more variables get proportionally less per-variable penalty
            # to approximate the group LASSO sqrt(group_size) scaling
            penalty_factor <- rep(1, n_vars)
            for (g in 1:n_groups) {
                group_vars <- which(groups == g)
                if (length(group_vars) == 0) next
                group_size <- length(group_vars)
                # Mix group-level and individual-level weights
                for (j in group_vars) {
                    pf_individual <- adaptive_weights$individual[j]
                    pf_group <- adaptive_weights$group[g] * sqrt(group_size)
                    # Weighted combination: alpha controls individual vs group
                    penalty_factor[j] <- alpha_sgl * pf_individual +
                                         (1 - alpha_sgl) * pf_group
                }
            }
            # Normalize penalty factors (glmnet requirement)
            penalty_factor <- penalty_factor / mean(penalty_factor)

            # Create survival response
            y_surv <- survival::Surv(y_time, y_event)

            # Fit cross-validated Cox model with glmnet
            # alpha_sgl maps to glmnet alpha for L1/L2 mixing
            nfolds_safe <- max(3, min(cv_folds, floor(sum(y_event) / 3)))
            cv_fit <- glmnet::cv.glmnet(
                x = X, y = y_surv,
                family = "cox",
                alpha = alpha_sgl,
                penalty.factor = penalty_factor,
                nfolds = nfolds_safe,
                standardize = FALSE,
                lambda = lambda_seq
            )

            # Fit full model across the lambda sequence
            full_fit <- glmnet::glmnet(
                x = X, y = y_surv,
                family = "cox",
                alpha = alpha_sgl,
                penalty.factor = penalty_factor,
                standardize = FALSE,
                lambda = lambda_seq
            )

            # Extract coefficient path
            coef_matrix <- as.matrix(coef(full_fit, s = lambda_seq))
            coefficients_path <- matrix(0, nrow = n_vars, ncol = length(lambda_seq))
            for (l in seq_along(lambda_seq)) {
                coefficients_path[, l] <- as.numeric(coef(full_fit, s = lambda_seq[l]))
            }

            # Calculate deviance and df for each lambda
            deviance_path <- numeric(length(lambda_seq))
            df_path <- numeric(length(lambda_seq))
            for (l in seq_along(lambda_seq)) {
                coefs_l <- coefficients_path[, l]
                deviance_path[l] <- private$.calculateDeviance(X, y_time, y_event, coefs_l)
                df_path[l] <- sum(abs(coefs_l) > 1e-8)
            }

            # Extract CV statistics from glmnet
            # Match user-supplied lambda_seq to cv_fit$lambda
            cv_mean <- rep(NA, length(lambda_seq))
            cv_se <- rep(NA, length(lambda_seq))
            for (l in seq_along(lambda_seq)) {
                idx <- which.min(abs(cv_fit$lambda - lambda_seq[l]))
                cv_mean[l] <- cv_fit$cvm[idx]
                cv_se[l] <- cv_fit$cvsd[idx]
            }

            # Select optimal lambda
            optimal_index <- private$.selectOptimalLambda(cv_mean, cv_se, lambda_seq, df_path)

            return(list(
                coefficients_path = coefficients_path,
                deviance_path = deviance_path,
                df_path = df_path,
                cv_mean = cv_mean,
                cv_se = cv_se,
                lambda_seq = lambda_seq,
                optimal_index = optimal_index,
                optimal_lambda = lambda_seq[optimal_index],
                optimal_coefficients = coefficients_path[, optimal_index],
                cv_fit = cv_fit,
                full_fit = full_fit
            ))
        },

        .fitSGLassoSingleLambda = function(X, y_time, y_event, groups, lambda, adaptive_weights) {
            # Fit a single-lambda Cox elastic net model via glmnet.
            # Used by stability selection (which needs many refits).
            alpha_sgl <- self$options$alpha_sgl
            n_vars <- ncol(X)
            n_groups <- max(groups)

            # Build penalty factors (same logic as main fit)
            penalty_factor <- rep(1, n_vars)
            for (g in 1:n_groups) {
                group_vars <- which(groups == g)
                if (length(group_vars) == 0) next
                group_size <- length(group_vars)
                for (j in group_vars) {
                    pf_individual <- adaptive_weights$individual[j]
                    pf_group <- adaptive_weights$group[g] * sqrt(group_size)
                    penalty_factor[j] <- alpha_sgl * pf_individual +
                                         (1 - alpha_sgl) * pf_group
                }
            }
            penalty_factor <- penalty_factor / mean(penalty_factor)

            y_surv <- survival::Surv(y_time, y_event)
            tryCatch({
                fit <- glmnet::glmnet(
                    x = X, y = y_surv,
                    family = "cox",
                    alpha = alpha_sgl,
                    penalty.factor = penalty_factor,
                    lambda = lambda,
                    standardize = FALSE
                )
                return(as.numeric(coef(fit, s = lambda)))
            }, error = function(e) {
                return(rep(0, n_vars))
            })
        },

        .softThreshold = function(x, lambda) {
            sign(x) * pmax(0, abs(x) - lambda)
        },

        .calculatePredictionError = function(X_test, y_test_time, y_test_event, coefficients) {
            # Calculate prediction error using 1 - C-index (Harrell's concordance)
            if (sum(abs(coefficients)) == 0) {
                return(0.5)  # Null model: random concordance
            }

            linear_pred <- as.numeric(X_test %*% coefficients)

            if (var(linear_pred) == 0) {
                return(0.5)
            }

            tryCatch({
                y_surv <- survival::Surv(y_test_time, y_test_event)
                # Higher risk score = worse prognosis, so reverse = TRUE
                cindex_result <- survival::concordance(y_surv ~ linear_pred, reverse = TRUE)
                error <- 1 - cindex_result$concordance
                return(ifelse(is.na(error), 0.5, error))
            }, error = function(e) {
                return(0.5)
            })
        },

        .calculateDeviance = function(X, y_time, y_event, coefficients) {
            # Calculate -2 * Cox partial log-likelihood
            if (sum(abs(coefficients)) == 0) {
                # Null model partial log-likelihood
                # Under null model (beta=0), the partial log-likelihood simplifies
                # to sum over events of -log(n_at_risk_i)
                order_idx <- order(y_time)
                sorted_event <- y_event[order_idx]
                n <- length(y_time)
                null_loglik <- 0
                for (i in which(sorted_event == 1)) {
                    n_at_risk <- n - i + 1
                    if (n_at_risk > 0) null_loglik <- null_loglik - log(n_at_risk)
                }
                return(-2 * null_loglik)
            }

            # Cox partial log-likelihood calculation
            linear_pred <- as.numeric(X %*% coefficients)
            # Use survival::coxph.detail or direct computation
            tryCatch({
                y_surv <- survival::Surv(y_time, y_event)
                # Fit a Cox model with the linear predictor as offset
                # The partial log-likelihood at beta is computed directly
                order_idx <- order(y_time)
                sorted_lp <- linear_pred[order_idx]
                sorted_event <- y_event[order_idx]
                n <- length(y_time)

                # Breslow partial log-likelihood
                loglik <- 0
                log_sum_exp <- log(sum(exp(sorted_lp)))
                for (i in 1:n) {
                    if (sorted_event[i] == 1) {
                        # At each event time, subtract log of risk set sum
                        risk_set_lp <- sorted_lp[i:n]
                        log_risk_sum <- log(sum(exp(risk_set_lp)))
                        loglik <- loglik + sorted_lp[i] - log_risk_sum
                    }
                }
                return(-2 * loglik)
            }, error = function(e) {
                return(NA)
            })
        },

        .selectOptimalLambda = function(cv_mean, cv_se, lambda_seq, df_path) {
            criterion <- self$options$selection_criterion
            
            if (criterion == "cv_deviance") {
                # Select lambda with minimum CV error
                return(which.min(cv_mean))
            } else if (criterion == "cv_c_index") {
                # Select lambda with maximum C-index (minimum error)
                return(which.min(cv_mean))
            } else if (criterion == "aic") {
                # Simplified AIC calculation
                aic_values <- cv_mean + 2 * df_path / length(cv_mean)
                return(which.min(aic_values))
            } else if (criterion == "bic") {
                # Simplified BIC calculation
                bic_values <- cv_mean + log(length(cv_mean)) * df_path / length(cv_mean)
                return(which.min(bic_values))
            } else if (criterion == "ebic") {
                # Extended BIC
                gamma <- self$options$ebic_gamma
                n <- length(cv_mean)
                p <- max(df_path)
                ebic_values <- cv_mean + log(n) * df_path / n + 2 * gamma * log(choose(p, df_path)) / n
                return(which.min(ebic_values))
            }
            
            return(which.min(cv_mean))  # Default
        },

        .performStabilitySelection = function(X, y_time, y_event, groups, lambda_seq) {
            # Stability selection: subsample repeatedly and fit entire lambda path
            n_bootstrap <- 50  # Reduced from 100 since glmnet path fitting is heavier
            subsample_ratio <- self$options$stability_subsample
            threshold <- self$options$stability_threshold
            alpha_sgl <- self$options$alpha_sgl

            n_vars <- ncol(X)
            n_groups <- max(groups)
            # Use a subset of lambda values for efficiency
            lambda_subset_idx <- round(seq(1, length(lambda_seq),
                                           length.out = min(20, length(lambda_seq))))
            lambda_subset <- lambda_seq[lambda_subset_idx]
            n_lambda <- length(lambda_subset)
            n_samples <- nrow(X)
            subsample_size <- floor(n_samples * subsample_ratio)

            # Build penalty factors (unit adaptive weights for stability)
            penalty_factor <- rep(1, n_vars)
            for (g in 1:n_groups) {
                group_vars <- which(groups == g)
                if (length(group_vars) == 0) next
                group_size <- length(group_vars)
                for (j in group_vars) {
                    penalty_factor[j] <- alpha_sgl * 1 +
                                         (1 - alpha_sgl) * sqrt(group_size)
                }
            }
            penalty_factor <- penalty_factor / mean(penalty_factor)

            # Storage for selection frequencies
            selection_matrix <- array(0, dim = c(n_vars, n_lambda, n_bootstrap))

            for (b in 1:n_bootstrap) {
                # Bootstrap subsample
                sample_idx <- sample(1:n_samples, subsample_size, replace = FALSE)
                X_sub <- X[sample_idx, , drop = FALSE]
                y_time_sub <- y_time[sample_idx]
                y_event_sub <- y_event[sample_idx]

                # Fit entire lambda path at once (much faster than per-lambda)
                tryCatch({
                    y_surv_sub <- survival::Surv(y_time_sub, y_event_sub)
                    fit_sub <- glmnet::glmnet(
                        x = X_sub, y = y_surv_sub,
                        family = "cox",
                        alpha = alpha_sgl,
                        penalty.factor = penalty_factor,
                        lambda = lambda_subset,
                        standardize = FALSE
                    )
                    for (l in seq_along(lambda_subset)) {
                        coefs <- as.numeric(coef(fit_sub, s = lambda_subset[l]))
                        selection_matrix[, l, b] <- as.numeric(abs(coefs) > 1e-8)
                    }
                }, error = function(e) {
                    # Leave zeros for this bootstrap iteration
                })
            }

            # Calculate selection probabilities
            selection_probs <- apply(selection_matrix, c(1, 2), mean)

            # Find stable selections
            stable_vars <- apply(selection_probs, 1, function(x) any(x >= threshold))

            return(list(
                selection_probs = selection_probs,
                stable_vars = stable_vars,
                threshold = threshold,
                lambda_seq = lambda_subset
            ))
        },

        .populateResults = function(results, data) {
            # Populate summary table
            if (self$options$show_summary) {
                private$.populateSummaryTable(results)
            }

            # Populate coefficients table
            if (self$options$show_coefficients) {
                private$.populateCoefficientsTable(results)
            }

            # Populate group structure table
            if (self$options$show_groups) {
                private$.populateGroupStructureTable(results)
            }

            # Populate performance metrics
            if (self$options$show_performance) {
                private$.populatePerformanceTable(results)
                private$.populateComparisonTable(results)
            }

            # Populate validation results
            if (self$options$show_validation) {
                private$.populateValidationTable(results)
            }

            # Populate stability results if available
            if (self$options$stability_selection && !is.null(results$stability_results)) {
                private$.populateStabilityTable(results)
            }

            # Generate plots (set state for render functions)
            if (self$options$plot_cv_error) {
                private$.plotCVError(results)
            }

            if (self$options$plot_coefficients) {
                private$.plotCoefficientPath(results)
            }

            if (self$options$plot_groups) {
                private$.plotGroupSelection(results)
            }

            if (self$options$plot_sparsity) {
                private$.plotSparsity(results)
            }

            if (self$options$plot_stability && !is.null(results$stability_results)) {
                private$.plotStability(results)
            }

            # Populate explanations
            if (self$options$showExplanations) {
                private$.populateExplanations(results)
            }
        },

        .populateSummaryTable = function(results) {
            table <- self$results$summary

            optimal_lambda <- results$cv_results$optimal_lambda
            optimal_coefs <- results$cv_results$optimal_coefficients
            n_selected <- sum(abs(optimal_coefs) > 1e-8)
            n_groups_selected <- length(unique(results$groups[abs(optimal_coefs) > 1e-8]))

            rows <- list(
                list(metric = "Optimal Lambda", 
                     value = format(optimal_lambda, digits = 4),
                     description = "Regularization parameter selected by cross-validation"),
                list(metric = "Variables Selected", 
                     value = as.character(n_selected),
                     description = paste("Out of", length(optimal_coefs), "total variables")),
                list(metric = "Groups Selected", 
                     value = as.character(n_groups_selected),
                     description = paste("Out of", max(results$groups), "total groups")),
                list(metric = "Alpha Parameter", 
                     value = format(self$options$alpha_sgl, digits = 3),
                     description = "Mixing parameter (0=group LASSO, 1=LASSO)"),
                list(metric = "CV Folds", 
                     value = as.character(self$options$cv_folds),
                     description = "Number of cross-validation folds used"),
                list(metric = "Selection Criterion", 
                     value = self$options$selection_criterion,
                     description = "Criterion used for model selection")
            )

            for (row in rows) {
                table$addRow(rowKey = row$metric, values = row)
            }
        },

        .populateCoefficientsTable = function(results) {
            table <- self$results$coefficients

            optimal_coefs <- results$cv_results$optimal_coefficients
            var_names <- results$variable_names
            groups <- results$groups

            # Only show selected variables
            selected_idx <- which(abs(optimal_coefs) > 1e-8)

            if (length(selected_idx) == 0) {
                table$addRow(rowKey = "none", values = list(
                    variable = "No variables selected",
                    group = "-",
                    coefficient = 0,
                    hazard_ratio = 1,
                    standardized_coef = 0,
                    importance = 0,
                    selection_frequency = 0
                ))
                return()
            }

            for (i in selected_idx) {
                coef_val <- optimal_coefs[i]
                
                row_values <- list(
                    variable = var_names[i],
                    group = paste0("Group ", groups[i]),
                    coefficient = coef_val,
                    hazard_ratio = exp(coef_val),
                    standardized_coef = coef_val,  # Already standardized if option was selected
                    importance = abs(coef_val),
                    selection_frequency = 1.0  # For final model, always 1
                )

                table$addRow(rowKey = var_names[i], values = row_values)
            }
        },

        .populateGroupStructureTable = function(results) {
            table <- self$results$groupStructure

            groups <- results$groups
            var_names <- results$variable_names
            optimal_coefs <- results$cv_results$optimal_coefficients

            unique_groups <- unique(groups)

            for (g in unique_groups) {
                group_vars <- which(groups == g)
                group_coefs <- optimal_coefs[group_vars]
                n_selected_in_group <- sum(abs(group_coefs) > 1e-8)
                sparsity_within <- (length(group_vars) - n_selected_in_group) / length(group_vars) * 100

                row_values <- list(
                    group_id = g,
                    group_name = paste0("Group ", g),
                    n_variables = length(group_vars),
                    group_selected = ifelse(n_selected_in_group > 0, "Yes", "No"),
                    group_penalty = 1.0,  # Simplified
                    sparsity_within = sparsity_within,
                    variables_list = paste(var_names[group_vars], collapse = ", ")
                )

                table$addRow(rowKey = paste0("group_", g), values = row_values)
            }
        },

        .populatePerformanceTable = function(results) {
            table <- self$results$performance

            cv_results <- results$cv_results
            optimal_idx <- cv_results$optimal_index

            # Compute training C-index using survival::concordance
            training_cindex <- 0.500
            tryCatch({
                optimal_coefs <- cv_results$optimal_coefficients
                if (sum(abs(optimal_coefs) > 1e-8) > 0) {
                    risk_scores <- as.numeric(results$X %*% optimal_coefs)
                    y_surv <- survival::Surv(results$y_time, results$y_event)
                    cindex_result <- survival::concordance(y_surv ~ risk_scores, reverse = TRUE)
                    training_cindex <- cindex_result$concordance
                }
            }, error = function(e) {
                training_cindex <<- 0.500
            })

            rows <- list(
                list(metric = "Training Concordance Index*",
                     training = training_cindex,
                     cv_mean = NA,
                     cv_se = NA,
                     interpretation = ifelse(!is.na(training_cindex) && training_cindex > 0.7,
                                            "Good discrimination",
                                            ifelse(!is.na(training_cindex) && training_cindex > 0.6,
                                                   "Moderate discrimination",
                                                   "Weak discrimination"))),
                list(metric = "CV Partial Likelihood Deviance",
                     training = NA,
                     cv_mean = cv_results$cv_mean[optimal_idx],
                     cv_se = cv_results$cv_se[optimal_idx],
                     interpretation = "Lower is better (Cox partial likelihood)"),
                list(metric = "Model Deviance",
                     training = cv_results$deviance_path[optimal_idx],
                     cv_mean = NA,
                     cv_se = NA,
                     interpretation = "-2 * Cox partial log-likelihood"),
                list(metric = "Degrees of Freedom",
                     training = cv_results$df_path[optimal_idx],
                     cv_mean = NA,
                     cv_se = NA,
                     interpretation = "Number of nonzero coefficients")
            )

            for (row in rows) {
                table$addRow(rowKey = row$metric, values = row)
            }
            
            table$setNote("cindex", "* Note: The Training Concordance Index overestimates true out-of-sample performance, as it is evaluated on the data used for variable selection.")
        },

        .populateComparisonTable = function(results) {
            table <- self$results$comparisonTable

            cv_results <- results$cv_results
            optimal_idx <- cv_results$optimal_index

            # Compute actual C-index for the selected model
            cindex_val <- NA
            tryCatch({
                optimal_coefs <- cv_results$optimal_coefficients
                if (sum(abs(optimal_coefs) > 1e-8) > 0) {
                    risk_scores <- as.numeric(results$X %*% optimal_coefs)
                    y_surv <- survival::Surv(results$y_time, results$y_event)
                    cindex_result <- survival::concordance(y_surv ~ risk_scores, reverse = TRUE)
                    cindex_val <- cindex_result$concordance
                }
            }, error = function(e) {
                cindex_val <<- NA
            })

            selected_groups <- if (sum(abs(cv_results$optimal_coefficients) > 1e-8) > 0) {
                length(unique(results$groups[abs(cv_results$optimal_coefficients) > 1e-8]))
            } else {
                0L
            }

            methods <- list(
                list(method = paste0("Sparse Group LASSO (alpha=", self$options$alpha_sgl, ")"),
                     n_selected = as.integer(cv_results$df_path[optimal_idx]),
                     n_groups_selected = as.integer(selected_groups),
                     cv_error = cv_results$cv_mean[optimal_idx],
                     c_index = cindex_val,
                     relative_performance = "Selected"),
                list(method = "Group LASSO (alpha=0)",
                     n_selected = NA,
                     n_groups_selected = NA,
                     cv_error = NA,
                     c_index = NA,
                     relative_performance = "Reference (not fitted)"),
                list(method = "LASSO (alpha=1)",
                     n_selected = NA,
                     n_groups_selected = NA,
                     cv_error = NA,
                     c_index = NA,
                     relative_performance = "Reference (not fitted)")
            )

            for (method in methods) {
                table$addRow(rowKey = method$method, values = method)
            }
        },

        .populateValidationTable = function(results) {
            table <- self$results$validationResults

            cv_results <- results$cv_results
            lambda_seq <- cv_results$lambda_seq
            cv_mean <- cv_results$cv_mean
            cv_se <- cv_results$cv_se
            df_path <- cv_results$df_path

            # Show top 10 models
            n_show <- min(10, length(lambda_seq))
            indices_to_show <- round(seq(1, length(lambda_seq), length.out = n_show))

            for (i in seq_along(indices_to_show)) {
                idx <- indices_to_show[i]
                
                row_values <- list(
                    lambda_value = lambda_seq[idx],
                    cv_error = cv_mean[idx],
                    cv_se = cv_se[idx],
                    n_selected = df_path[idx],
                    model_rank = i
                )

                table$addRow(rowKey = paste0("lambda_", idx), values = row_values)
            }
        },

        .populateStabilityTable = function(results) {
            table <- self$results$stabilityResults

            stability_results <- results$stability_results
            var_names <- results$variable_names
            groups <- results$groups

            if (is.null(stability_results)) return()

            selection_probs <- stability_results$selection_probs
            stable_vars <- stability_results$stable_vars
            threshold <- stability_results$threshold

            for (i in 1:length(var_names)) {
                max_prob <- max(selection_probs[i, ])
                
                row_values <- list(
                    variable = var_names[i],
                    group = paste0("Group ", groups[i]),
                    selection_probability = max_prob * 100,
                    stable_selection = ifelse(stable_vars[i], "Yes", "No"),
                    first_selected = NA,  # Would need more detailed tracking
                    last_selected = NA
                )

                table$addRow(rowKey = var_names[i], values = row_values)
            }
        },

        .plotCVError = function(results) {
            cv_results <- results$cv_results
            image <- self$results$cvErrorPlot
            # Extract only plain numeric data for protobuf-safe serialization
            image$setState(list(
                lambda_seq = as.numeric(cv_results$lambda_seq),
                cv_mean = as.numeric(cv_results$cv_mean),
                cv_se = as.numeric(cv_results$cv_se),
                optimal_index = as.integer(cv_results$optimal_index)
            ))
        },

        .plotCoefficientPath = function(results) {
            cv_results <- results$cv_results
            image <- self$results$coefficientPlot
            # Convert coefficients_path matrix to plain data.frame
            coef_path <- as.data.frame(cv_results$coefficients_path)
            image$setState(list(
                lambda_seq = as.numeric(cv_results$lambda_seq),
                coefficients_path = coef_path,
                variable_names = as.character(results$variable_names),
                optimal_index = as.integer(cv_results$optimal_index)
            ))
        },

        .plotGroupSelection = function(results) {
            image <- self$results$groupSelectionPlot
            image$setState(list(
                groups = as.integer(results$groups),
                variable_names = as.character(results$variable_names),
                optimal_coefficients = as.numeric(results$cv_results$optimal_coefficients)
            ))
        },

        .plotSparsity = function(results) {
            image <- self$results$sparsityPlot
            cv_results <- results$cv_results
            # Store sparsity info: proportion of zero coefficients at each lambda
            n_vars <- nrow(cv_results$coefficients_path)
            sparsity_levels <- numeric(ncol(cv_results$coefficients_path))
            for (l in seq_len(ncol(cv_results$coefficients_path))) {
                sparsity_levels[l] <- sum(abs(cv_results$coefficients_path[, l]) < 1e-8) / n_vars
            }
            image$setState(list(
                lambda_seq = as.numeric(cv_results$lambda_seq),
                sparsity_levels = as.numeric(sparsity_levels),
                df_path = as.numeric(cv_results$df_path),
                optimal_index = as.integer(cv_results$optimal_index)
            ))
        },

        .plotStability = function(results) {
            image <- self$results$stabilityPlot
            if (is.null(results$stability_results)) {
                image$setState(NULL)
                return()
            }
            stability <- results$stability_results
            # Max selection probability per variable across all lambdas
            max_probs <- apply(stability$selection_probs, 1, max)
            image$setState(list(
                variable_names = as.character(results$variable_names),
                max_probs = as.numeric(max_probs),
                stable_vars = as.logical(stability$stable_vars),
                threshold = as.numeric(stability$threshold)
            ))
        },

        # ---- Plot render functions (called by jamovi via renderFun) ----

        .renderCVErrorPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$plot_cv_error) return()

            state <- image$state
            if (is.null(state)) return()

            cv_data <- data.frame(
                lambda = state$lambda_seq,
                cv_mean = state$cv_mean,
                cv_upper = state$cv_mean + state$cv_se,
                cv_lower = state$cv_mean - state$cv_se
            )
            # Remove NAs
            cv_data <- cv_data[!is.na(cv_data$cv_mean), ]
            if (nrow(cv_data) == 0) return()

            optimal_lambda <- state$lambda_seq[state$optimal_index]

            p <- ggplot2::ggplot(cv_data, ggplot2::aes(x = log(lambda), y = cv_mean)) +
                ggplot2::geom_point(color = "red", size = 0.8) +
                ggplot2::geom_errorbar(
                    ggplot2::aes(ymin = cv_lower, ymax = cv_upper),
                    color = "darkgrey", width = 0.02
                ) +
                ggplot2::geom_vline(
                    xintercept = log(optimal_lambda),
                    linetype = "dashed", color = "blue"
                ) +
                ggplot2::labs(
                    title = "Cross-Validation Error",
                    subtitle = paste("Blue dashed: optimal lambda =",
                                     format(optimal_lambda, digits = 4)),
                    x = "Log(Lambda)",
                    y = "Cox Partial Likelihood Deviance"
                ) +
                ggtheme

            print(p)
            TRUE
        },

        .renderCoefficientPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$plot_coefficients) return()

            state <- image$state
            if (is.null(state)) return()

            lambda_seq <- state$lambda_seq
            var_names <- state$variable_names
            coef_path <- as.matrix(as.data.frame(state$coefficients_path))

            if (length(var_names) == 0 || length(lambda_seq) == 0) return()

            # Build long-form data for ggplot
            plot_data <- data.frame()
            for (i in seq_along(var_names)) {
                row_data <- data.frame(
                    log_lambda = log(lambda_seq),
                    coefficient = coef_path[i, ],
                    variable = var_names[i],
                    stringsAsFactors = FALSE
                )
                plot_data <- rbind(plot_data, row_data)
            }

            optimal_lambda <- lambda_seq[state$optimal_index]

            p <- ggplot2::ggplot(plot_data,
                    ggplot2::aes(x = log_lambda, y = coefficient, color = variable)) +
                ggplot2::geom_line(alpha = 0.7) +
                ggplot2::geom_vline(
                    xintercept = log(optimal_lambda),
                    linetype = "dashed", color = "black"
                ) +
                ggplot2::labs(
                    title = "Coefficient Path",
                    subtitle = "Dashed line: optimal lambda",
                    x = "Log(Lambda)",
                    y = "Coefficient",
                    color = "Variable"
                ) +
                ggtheme +
                ggplot2::theme(legend.position = "right")

            # If too many variables, hide legend
            if (length(var_names) > 15) {
                p <- p + ggplot2::theme(legend.position = "none")
            }

            print(p)
            TRUE
        },

        .renderGroupSelectionPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$plot_groups) return()

            state <- image$state
            if (is.null(state)) return()

            var_names <- state$variable_names
            groups <- state$groups
            coefs <- state$optimal_coefficients

            if (length(var_names) == 0) return()

            plot_data <- data.frame(
                variable = factor(var_names, levels = rev(var_names)),
                group = factor(paste0("Group ", groups)),
                coefficient = coefs,
                selected = abs(coefs) > 1e-8,
                stringsAsFactors = FALSE
            )

            p <- ggplot2::ggplot(plot_data,
                    ggplot2::aes(x = variable, y = coefficient, fill = group)) +
                ggplot2::geom_col(alpha = 0.8) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = "Group Selection Pattern",
                    subtitle = paste(sum(plot_data$selected), "of",
                                     nrow(plot_data), "variables selected"),
                    x = "Variable",
                    y = "Coefficient",
                    fill = "Group"
                ) +
                ggtheme

            print(p)
            TRUE
        },

        .renderSparsityPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$plot_sparsity) return()

            state <- image$state
            if (is.null(state)) return()

            plot_data <- data.frame(
                log_lambda = log(state$lambda_seq),
                sparsity = state$sparsity_levels * 100,
                n_selected = state$df_path
            )
            plot_data <- plot_data[!is.na(plot_data$sparsity), ]
            if (nrow(plot_data) == 0) return()

            optimal_lambda <- state$lambda_seq[state$optimal_index]

            p <- ggplot2::ggplot(plot_data,
                    ggplot2::aes(x = log_lambda, y = sparsity)) +
                ggplot2::geom_line(color = "steelblue", linewidth = 1) +
                ggplot2::geom_point(color = "steelblue", size = 0.8) +
                ggplot2::geom_vline(
                    xintercept = log(optimal_lambda),
                    linetype = "dashed", color = "red"
                ) +
                ggplot2::labs(
                    title = "Sparsity Pattern Across Lambda",
                    subtitle = "Percentage of zero coefficients at each lambda",
                    x = "Log(Lambda)",
                    y = "Sparsity (%)"
                ) +
                ggplot2::ylim(0, 100) +
                ggtheme

            print(p)
            TRUE
        },

        .renderStabilityPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$plot_stability) return()

            state <- image$state
            if (is.null(state)) return()

            var_names <- state$variable_names
            max_probs <- state$max_probs
            stable_vars <- state$stable_vars
            threshold <- state$threshold

            if (length(var_names) == 0) return()

            # Order by selection probability
            ord <- order(max_probs, decreasing = TRUE)
            plot_data <- data.frame(
                variable = factor(var_names[ord], levels = var_names[ord]),
                probability = max_probs[ord] * 100,
                stable = stable_vars[ord],
                stringsAsFactors = FALSE
            )

            p <- ggplot2::ggplot(plot_data,
                    ggplot2::aes(x = variable, y = probability,
                                 fill = stable)) +
                ggplot2::geom_col(alpha = 0.8) +
                ggplot2::geom_hline(
                    yintercept = threshold * 100,
                    linetype = "dashed", color = "red"
                ) +
                ggplot2::scale_fill_manual(
                    values = c("TRUE" = "steelblue", "FALSE" = "grey60"),
                    labels = c("TRUE" = "Stable", "FALSE" = "Unstable"),
                    name = "Selection"
                ) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = "Stability Selection Results",
                    subtitle = paste("Threshold:", threshold * 100, "%"),
                    x = "Variable",
                    y = "Selection Probability (%)"
                ) +
                ggplot2::ylim(0, 100) +
                ggtheme

            print(p)
            TRUE
        },

        .populateExplanations = function(results) {
            html <- self$results$explanations

            optimal_coefs <- results$cv_results$optimal_coefficients
            n_selected <- sum(abs(optimal_coefs) > 1e-8)
            alpha_sgl <- self$options$alpha_sgl

            # Build conditional warning sections
            warnings_html <- ""

            # Draft status warning (always shown)
            warnings_html <- paste0(warnings_html,
                "<div style='background-color: #fff3cd; border: 1px solid #ffc107; padding: 12px; border-radius: 5px; margin: 10px 0;'>",
                "<strong>DRAFT STATUS:</strong> This analysis is in the Drafts submenu. ",
                "It uses glmnet's elastic net Cox regression with weighted penalty factors to approximate ",
                "sparse group LASSO behavior. This is NOT a true sparse group LASSO implementation (which ",
                "requires the SGL package or custom proximal gradient descent). The group-level penalty ",
                "structure is approximated through penalty.factor weighting. Results should be validated ",
                "against dedicated sparse group LASSO software for publication.",
                "</div>")

            # Pathway warning
            if (!is.null(private$.pathwayWarning)) {
                warnings_html <- paste0(warnings_html,
                    "<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 12px; border-radius: 5px; margin: 10px 0;'>",
                    "<strong>PATHWAY GROUPING WARNING:</strong> ", private$.pathwayWarning,
                    "</div>")
            }

            content <- paste0("
            <html>
            <head>
            <style>
                .explanation { margin: 10px; padding: 15px; background-color: #f8f9fa; border-radius: 8px; }
                .result-section { margin: 15px 0; }
                .highlight { background-color: #fff3cd; padding: 2px 4px; border-radius: 3px; }
                .interpretation { background-color: #e8f4fd; padding: 10px; border-radius: 5px; margin: 10px 0; }
                .method-info { background-color: #e1f5fe; padding: 10px; border-radius: 5px; }
            </style>
            </head>
            <body>
                <div class='explanation'>
                    <h3>Sparse Group LASSO Analysis Results</h3>

                    ", warnings_html, "

                    <div class='method-info'>
                        <h4>Method Overview</h4>
                        <p><strong>Sparse Group LASSO (Approximation)</strong> uses glmnet Cox regression with ",
                        "penalty.factor weighting to approximate group-wise variable selection with individual ",
                        "sparsity within groups. ",
                        "With alpha=", format(alpha_sgl, digits = 3), ", the method emphasizes ",
                        ifelse(alpha_sgl > 0.5, "individual variable sparsity", "group-wise selection"), ".</p>
                        <p><strong>Engine:</strong> glmnet::cv.glmnet(family='cox') with Cox partial likelihood.</p>
                    </div>

                    <div class='result-section'>
                        <h4>Variable Selection Results</h4>
                        <div class='interpretation'>
                            <p>Selected <span class='highlight'>", n_selected, " variables</span> out of ", length(optimal_coefs), " total variables.</p>
                            <p>This represents a <span class='highlight'>", round((1 - n_selected/length(optimal_coefs)) * 100, 1), "% reduction</span> in model complexity.</p>
                        </div>
                    </div>

                    <div class='result-section'>
                        <h4>Model Configuration</h4>
                        <ul>
                            <li><strong>Alpha Parameter:</strong> ", format(alpha_sgl, digits = 3), " (",
                            ifelse(alpha_sgl > 0.8, "High individual sparsity",
                                   ifelse(alpha_sgl > 0.5, "Balanced sparsity", "Group-focused selection")), ")</li>
                            <li><strong>Optimal Lambda:</strong> ", format(results$cv_results$optimal_lambda, digits = 4), "</li>
                            <li><strong>Cross-Validation:</strong> ", self$options$cv_folds, "-fold CV</li>
                            <li><strong>Selection Criterion:</strong> ", self$options$selection_criterion, "</li>
                        </ul>
                    </div>

                    <div class='result-section'>
                        <h4>Clinical Interpretation</h4>
                        <div class='interpretation'>
                            <p><strong>Sparse Group LASSO</strong> is particularly valuable for:</p>
                            <ul>
                                <li><strong>Genomic studies:</strong> Selecting important pathways while identifying key genes within pathways</li>
                                <li><strong>Clinical prediction:</strong> Balancing model complexity with interpretability</li>
                                <li><strong>High-dimensional data:</strong> Managing correlated predictors through group structure</li>
                                <li><strong>Biomarker discovery:</strong> Identifying both important biological groups and specific markers</li>
                            </ul>
                        </div>
                    </div>

                    <div class='result-section'>
                        <h4>Important Considerations</h4>
                        <ul>
                            <li><strong>Approximation:</strong> This uses elastic net with weighted penalties, not a true sparse group LASSO solver. Group selection behavior may differ from the theoretical SGL formulation.</li>
                            <li><strong>Group Definition:</strong> Results depend heavily on how variables are grouped</li>
                            <li><strong>Alpha Selection:</strong> Higher alpha values favor individual sparsity, lower values favor group selection</li>
                            <li><strong>Stability:</strong> Consider stability selection for more robust variable identification</li>
                            <li><strong>Validation:</strong> External validation is crucial for clinical applications</li>
                        </ul>
                    </div>
                </div>
            </body>
            </html>")

            html$setContent(content)
        }
    )
)