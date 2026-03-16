sparsegrouplassoClass <- R6::R6Class(
    "sparsegrouplassoClass",
    inherit = sparsegrouplassoBase,
    private = list(
        .pathwayWarning = NULL,

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
                tryCatch({
                    existing <- self$results$todo$content
                    if (is.null(existing) || nchar(existing) == 0) existing <- ""
                    label <- switch(as.character(type),
                        "1" = "ERROR", "2" = "WARNING", "3" = "WARNING", "4" = "INFO", "NOTE")
                    self$results$todo$setContent(paste0(
                        existing, "<p><strong>", label, ":</strong> ", content, "</p>"
                    ))
                    self$results$todo$setVisible(TRUE)
                }, error = function(e2) NULL)
            })
        },

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
            # Set visibility for adaptive weights table (no negation support in .r.yaml)
            self$results$adaptiveWeights$setVisible(self$options$weight_type != "none")

            # Check for required variables
            if (is.null(self$options$time_var) || is.null(self$options$event_var) ||
                is.null(self$options$pred_vars) || length(self$options$pred_vars) < 2) {
                return()
            }

            # Validate package availability
            if (!requireNamespace("glmnet", quietly = TRUE)) {
                private$.insertNotice(
                    'missingGlmnet', jmvcore::NoticeType$ERROR,
                    "Package 'glmnet' is required for Sparse Group LASSO Cox regression. Install with: install.packages('glmnet')",
                    position = 1
                )
                return()
            }
            if (!requireNamespace("survival", quietly = TRUE)) {
                private$.insertNotice(
                    'missingSurvival', jmvcore::NoticeType$ERROR,
                    "Package 'survival' is required. Install with: install.packages('survival')",
                    position = 1
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

                    # Clinical notices based on results
                    n_events <- sum(results$y_event == 1)
                    n_vars <- ncol(results$X)
                    n_selected <- sum(abs(results$cv_results$optimal_coefficients) > 1e-8)

                    if (n_events < 10) {
                        private$.insertNotice(
                            'veryLowEvents', jmvcore::NoticeType$STRONG_WARNING,
                            sprintf("Only %d events detected. Results are highly unreliable with fewer than 10 events. Consider collecting more data.", n_events),
                            position = 1
                        )
                    } else if (n_events < 20) {
                        private$.insertNotice(
                            'lowEvents', jmvcore::NoticeType$WARNING,
                            sprintf("Only %d events detected. Penalized models need adequate events for stable variable selection.", n_events)
                        )
                    }

                    if (n_selected == 0) {
                        private$.insertNotice(
                            'noSelection', jmvcore::NoticeType$WARNING,
                            "No variables were selected at the optimal lambda. All coefficients are zero. Consider relaxing the penalty (lower alpha) or checking data quality."
                        )
                    }

                    # C-index warning
                    if (!is.null(results$cv_results$optimal_coefficients) && n_selected > 0) {
                        tryCatch({
                            lp <- as.numeric(results$X %*% results$cv_results$optimal_coefficients)
                            conc <- survival::concordance(
                                survival::Surv(results$y_time, results$y_event) ~ lp, reverse = TRUE
                            )
                            if (!is.na(conc$concordance) && conc$concordance < 0.5) {
                                private$.insertNotice(
                                    'poorDiscrimination', jmvcore::NoticeType$STRONG_WARNING,
                                    sprintf("Training C-index = %.3f is below 0.5, indicating worse-than-chance discrimination. Review variable selection and data quality.", conc$concordance),
                                    position = 1
                                )
                            }
                        }, error = function(e) NULL)
                    }

                    # Success notice
                    penalty_label <- sprintf("Sparse Group LASSO (alpha=%.2f)", self$options$alpha_sgl)
                    n_groups <- max(results$groups)
                    n_groups_sel <- if (n_selected > 0) {
                        length(unique(results$groups[abs(results$cv_results$optimal_coefficients) > 1e-8]))
                    } else 0L

                    private$.insertNotice(
                        'analysisComplete', jmvcore::NoticeType$INFO,
                        sprintf("%s completed: %d/%d variables selected across %d/%d groups, using %d-fold CV with %d observations (%d events).",
                                penalty_label, n_selected, n_vars, n_groups_sel, n_groups,
                                self$options$cv_folds, nrow(results$X), n_events)
                    )
                }
            }, error = function(e) {
                private$.insertNotice(
                    'analysisError', jmvcore::NoticeType$ERROR,
                    sprintf("Error in Sparse Group LASSO fitting: %s", e$message),
                    position = 1
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
                event_level <- as.character(self$options$outcomeLevel)
                event_data_raw <- data[[event_var]]
                if (is.factor(event_data_raw)) {
                    events <- sum(as.character(event_data_raw) == event_level, na.rm = TRUE)
                } else {
                    event_level_num <- suppressWarnings(as.numeric(event_level))
                    if (!is.na(event_level_num)) {
                        events <- sum(event_data_raw == event_level_num, na.rm = TRUE)
                    } else {
                        events <- sum(as.character(event_data_raw) == event_level, na.rm = TRUE)
                    }
                }
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

            # 3. Multicollinearity assessment
            collin_text <- ""
            tryCatch({
                num_vars <- pred_vars[sapply(pred_vars, function(v) is.numeric(data[[v]]))]
                if (length(num_vars) >= 2) {
                    cor_mat <- cor(data[, num_vars, drop = FALSE], use = "pairwise.complete.obs")
                    diag(cor_mat) <- 0
                    max_cor <- max(abs(cor_mat), na.rm = TRUE)
                    if (max_cor >= 0.9) {
                        collin_text <- sprintf("<div style='color: orange; margin-top: 5px;'>Note: High pairwise correlation detected (max |r| = %.2f). Group structure will help manage correlated predictors.</div>", max_cor)
                    } else if (max_cor >= 0.7) {
                        collin_text <- sprintf("<div style='color: green; margin-top: 5px;'>Moderate pairwise correlations detected (max |r| = %.2f). Group structure will help manage correlated predictors.</div>", max_cor)
                    } else {
                        collin_text <- sprintf("<div style='color: green; margin-top: 5px;'>No concerning multicollinearity (max |r| = %.2f).</div>", max_cor)
                    }
                }
            }, error = function(e) {
                collin_text <<- ""
            })

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
                private$.insertNotice(
                    'missingVars', jmvcore::NoticeType$ERROR,
                    sprintf("Missing variables in data: %s", paste(missingVars, collapse = ", ")),
                    position = 1
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
                private$.insertNotice(
                    'insufficientData', jmvcore::NoticeType$ERROR,
                    sprintf("Need at least 10 complete observations. Only %d available after removing missing data.", nrow(data)),
                    position = 1
                )
                return(NULL)
            }

            # Two-level outcome encoding using outcomeLevel / censorLevel
            event_data <- data[[eventVar]]
            event_level <- as.character(self$options$outcomeLevel)
            censor_level <- as.character(self$options$censorLevel)

            event_numeric <- rep(NA_real_, length(event_data))

            if (is.factor(event_data)) {
                event_chr <- as.character(event_data)
                event_numeric[event_chr == event_level] <- 1
                event_numeric[event_chr == censor_level] <- 0
            } else {
                event_level_num <- suppressWarnings(as.numeric(event_level))
                censor_level_num <- suppressWarnings(as.numeric(censor_level))
                if (!is.na(event_level_num)) event_numeric[event_data == event_level_num] <- 1
                if (!is.na(censor_level_num)) event_numeric[event_data == censor_level_num] <- 0
            }

            n_excluded_outcome <- sum(is.na(event_numeric) & !is.na(event_data))
            if (n_excluded_outcome > 0) {
                private$.insertNotice(
                    'excludedRows', jmvcore::NoticeType$WARNING,
                    sprintf("%d row(s) excluded because outcome value matched neither event level ('%s') nor censored level ('%s').",
                            n_excluded_outcome, event_level, censor_level)
                )
            }

            # Remove rows where event encoding produced NA
            keep_rows <- !is.na(event_numeric)
            data <- data[keep_rows, , drop = FALSE]
            data[[eventVar]] <- event_numeric[keep_rows]

            if (nrow(data) < 10) {
                private$.insertNotice(
                    'insufficientDataEncoding', jmvcore::NoticeType$ERROR,
                    sprintf("Need at least 10 complete observations after event encoding. Only %d remain.", nrow(data)),
                    position = 1
                )
                return(NULL)
            }

            # Validate time values
            time_vals <- as.numeric(data[[timeVar]])
            if (any(time_vals <= 0, na.rm = TRUE)) {
                n_nonpos <- sum(time_vals <= 0, na.rm = TRUE)
                private$.insertNotice(
                    'nonPositiveTime', jmvcore::NoticeType$ERROR,
                    sprintf("All survival times must be positive. Found %d non-positive value(s). Check the time variable.", n_nonpos),
                    position = 1
                )
                return(NULL)
            }

            if (sum(data[[eventVar]] == 1) == 0) {
                private$.insertNotice(
                    'noEvents', jmvcore::NoticeType$ERROR,
                    "No events detected after encoding. All observations are censored. The model cannot be fitted.",
                    position = 1
                )
                return(NULL)
            }

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

            # Define groups (pass both original and actual pred_vars)
            groups <- private$.defineGroups(X, data, actual_pred_vars, predVars)

            # Build descriptive group names from member variables
            group_names <- private$.buildGroupNames(groups, actual_pred_vars, predVars)

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

            # Bootstrap confidence intervals if requested
            bootstrap_ci <- NULL
            if (self$options$confidence_intervals) {
                bootstrap_ci <- private$.bootstrapConfidenceIntervals(
                    X, y_time, y_event, groups, cv_results$optimal_lambda, adaptive_weights
                )
            }

            return(list(
                cv_results = cv_results,
                stability_results = stability_results,
                bootstrap_ci = bootstrap_ci,
                groups = groups,
                group_names = group_names,
                lambda_seq = lambda_seq,
                adaptive_weights = adaptive_weights,
                X = X,
                y_time = y_time,
                y_event = y_event,
                variable_names = actual_pred_vars
            ))
        },

        .buildGroupNames = function(groups, col_names, original_pred_vars) {
            # Build descriptive group names from their member variables
            unique_groups <- sort(unique(groups))
            names_map <- character(max(groups))

            for (g in unique_groups) {
                g_idx <- which(groups == g)
                g_cols <- col_names[g_idx]

                if (length(g_cols) == 1) {
                    # Single variable: use its name directly
                    names_map[g] <- g_cols
                } else {
                    # Multiple variables: find common original predictor name
                    matched_orig <- NULL
                    for (pv in original_pred_vars) {
                        if (all(g_cols == pv | startsWith(g_cols, paste0(pv, "_")))) {
                            matched_orig <- pv
                            break
                        }
                    }
                    if (!is.null(matched_orig)) {
                        names_map[g] <- matched_orig
                    } else {
                        # Truncate long lists
                        if (length(g_cols) <= 3) {
                            names_map[g] <- paste(g_cols, collapse = ", ")
                        } else {
                            names_map[g] <- paste0(paste(g_cols[1:2], collapse = ", "), " +", length(g_cols) - 2, " more")
                        }
                    }
                }
            }
            return(names_map)
        },

        .defineGroups = function(X, data, pred_vars, original_pred_vars = NULL) {
            method <- self$options$group_definition
            groups <- NULL
            orig_pv <- if (!is.null(original_pred_vars)) original_pred_vars else self$options$pred_vars

            if (method == "factor_based") {
                groups <- private$.factorBasedGrouping(X, data, pred_vars, orig_pv)
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

        .factorBasedGrouping = function(X, data, pred_vars, original_pred_vars = NULL) {
            # Group X columns by their originating predictor variable.
            # After dummy encoding, "ecog" (factor with levels 0,1,2,3)
            # becomes columns "ecog_1", "ecog_2", "ecog_3" in X.
            # We map each X column back to its original predictor name
            # and assign all dummies from the same predictor to one group.
            x_cols <- colnames(X)
            groups <- rep(0L, ncol(X))
            current_group <- 1L

            # Use original predictor names (before dummy expansion) for matching
            orig_vars <- if (!is.null(original_pred_vars)) original_pred_vars else pred_vars
            # Sort by descending length to avoid prefix collisions
            sorted_idx <- order(nchar(orig_vars), decreasing = TRUE)

            for (idx in sorted_idx) {
                var <- orig_vars[idx]
                # Match exact name or name followed by "_" (dummy suffix)
                matched <- which(x_cols == var | startsWith(x_cols, paste0(var, "_")))
                matched <- matched[groups[matched] == 0L]
                if (length(matched) > 0) {
                    groups[matched] <- current_group
                    current_group <- current_group + 1L
                }
            }

            # Assign remaining unmatched columns to their own groups
            unmatched <- which(groups == 0L)
            for (j in unmatched) {
                groups[j] <- current_group
                current_group <- current_group + 1L
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

            # The pathway variable must be a factor with exactly as many levels
            # as there are predictor variables. Each level names the pathway/group
            # for the corresponding predictor (in order).
            # E.g., with pred_vars = c("EGFR","BRAF","TP53","MDM2"),
            # pathway_info might have first 4 unique values: "RTK","RAS","p53","p53"
            # mapping each predictor to its pathway.
            pathway_data <- data[[pathway_var]]
            unique_pathways <- if (is.factor(pathway_data)) levels(pathway_data) else unique(pathway_data)

            if (length(unique_pathways) < 2) {
                private$.pathwayWarning <- "Pathway variable has fewer than 2 unique levels. Falling back to individual groups."
                return(NULL)
            }

            # Strategy: if # unique pathway levels == # predictors, assume 1:1 mapping
            # (each observation's pathway value tells which group that predictor belongs to).
            # Otherwise, try name-matching between pathway levels and predictor variable names.
            groups <- rep(1L, length(pred_vars))

            # Try prefix matching: check if any pathway level appears as prefix in a predictor name
            # This handles cases like pathway levels "EGFR_pathway", "PI3K_pathway" matching
            # predictor names "EGFR_score", "EGFR_expression", "PI3K_activity"
            matched <- FALSE
            for (i in seq_along(pred_vars)) {
                for (j in seq_along(unique_pathways)) {
                    pw <- as.character(unique_pathways[j])
                    pv <- pred_vars[i]
                    # Match if predictor starts with pathway name (with separator)
                    # or pathway name contains predictor name
                    if (startsWith(pv, paste0(pw, "_")) || startsWith(pv, paste0(pw, "."))) {
                        groups[i] <- j
                        matched <- TRUE
                        break
                    }
                }
            }

            if (!matched) {
                # Fallback: use custom_groups syntax or sequential assignment
                # Use the first n values from the pathway variable as group assignments
                pw_values <- as.character(pathway_data[1:min(length(pred_vars), length(pathway_data))])
                pw_mapping <- match(pw_values, unique_pathways)
                pw_mapping[is.na(pw_mapping)] <- 1L
                groups <- pw_mapping[seq_along(pred_vars)]

                private$.pathwayWarning <- paste0(
                    "Pathway grouping used the first ", length(pred_vars),
                    " values from the pathway variable as group assignments. ",
                    "For precise control, use Custom Groups with explicit index-based specification."
                )
            }

            return(groups)
        },

        .variableTypeGrouping = function(data, pred_vars) {
            # After dummy encoding, pred_vars may no longer exist in data.
            # We need to classify based on the ORIGINAL data before dummies.
            # Use self$data (original) to determine types, then map to X columns.
            original_data <- self$data
            x_cols <- colnames(data)
            x_cols <- x_cols[!x_cols %in% c(self$options$time_var, self$options$event_var)]
            if (!is.null(self$options$pathway_info) && self$options$pathway_info != "") {
                x_cols <- x_cols[x_cols != self$options$pathway_info]
            }

            groups <- rep(1L, length(x_cols))
            # Group 1 = continuous, Group 2 = categorical-derived

            for (i in seq_along(x_cols)) {
                col <- x_cols[i]
                # Check if this column is a dummy from a factor
                is_dummy <- FALSE
                for (pv in pred_vars) {
                    if (col != pv && startsWith(col, paste0(pv, "_"))) {
                        is_dummy <- TRUE
                        break
                    }
                }
                if (is_dummy) {
                    groups[i] <- 2L
                } else if (col %in% names(original_data)) {
                    if (is.factor(original_data[[col]]) || is.character(original_data[[col]])) {
                        groups[i] <- 2L
                    }
                }
            }

            return(groups)
        },

        .correlationBasedGrouping = function(X) {
            if (ncol(X) < 2) return(rep(1, ncol(X)))

            cor_matrix <- cor(X, use = "complete.obs")
            threshold <- self$options$correlation_threshold

            # Greedy clustering: unassigned variables (0) get grouped
            groups <- rep(0L, ncol(X))
            current_group <- 1L

            for (i in 1:ncol(X)) {
                if (groups[i] != 0L) next  # Already assigned

                # Find unassigned variables highly correlated with variable i
                high_cor <- which(abs(cor_matrix[i, ]) >= threshold & groups == 0L)
                groups[high_cor] <- current_group
                current_group <- current_group + 1L
            }

            return(groups)
        },

        .generateLambdaSequence = function(X, y_time, y_event, groups) {
            if (self$options$lambda_sequence == "custom" && self$options$custom_lambda != "") {
                lambda_values <- as.numeric(strsplit(self$options$custom_lambda, ",")[[1]])
                return(sort(lambda_values[!is.na(lambda_values)], decreasing = TRUE))
            }

            # Estimate lambda_max (when all coefficients are zero)
            lambda_max <- private$.estimateLambdaMax(X, y_time, y_event)

            lambda_min_ratio <- self$options$lambda_min_ratio
            n_lambda <- self$options$n_lambda

            if (self$options$lambda_sequence == "adaptive") {
                # Adaptive: use glmnet's own lambda sequence (data-driven spacing)
                tryCatch({
                    y_surv <- survival::Surv(y_time, y_event)
                    auto_fit <- glmnet::glmnet(
                        x = X, y = y_surv, family = "cox",
                        alpha = self$options$alpha_sgl,
                        standardize = FALSE,
                        nlambda = n_lambda
                    )
                    return(auto_fit$lambda)
                }, error = function(e) NULL)
            }

            # Default auto: log-spaced sequence
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
            nfolds_safe <- max(3, min(cv_folds, floor(sum(y_event) / 3)))
            cv_repeats <- self$options$cv_repeats

            if (cv_repeats > 1) {
                # Repeated CV: average CV errors across repeats
                cvm_accum <- NULL
                cvsd_accum <- NULL
                for (r in seq_len(cv_repeats)) {
                    set.seed(self$options$seed_value + r - 1)
                    cv_r <- glmnet::cv.glmnet(
                        x = X, y = y_surv,
                        family = "cox",
                        alpha = alpha_sgl,
                        penalty.factor = penalty_factor,
                        nfolds = nfolds_safe,
                        standardize = FALSE,
                        lambda = lambda_seq
                    )
                    if (is.null(cvm_accum)) {
                        cvm_accum <- cv_r$cvm
                        cvsd_accum <- cv_r$cvsd^2
                    } else {
                        cvm_accum <- cvm_accum + cv_r$cvm
                        cvsd_accum <- cvsd_accum + cv_r$cvsd^2
                    }
                }
                # Use last cv_r as template, overwrite with averaged values
                cv_fit <- cv_r
                cv_fit$cvm <- cvm_accum / cv_repeats
                cv_fit$cvsd <- sqrt(cvsd_accum / cv_repeats)
                cv_fit$cvup <- cv_fit$cvm + cv_fit$cvsd
                cv_fit$cvlo <- cv_fit$cvm - cv_fit$cvsd
                cv_fit$lambda.min <- cv_fit$lambda[which.min(cv_fit$cvm)]
                idx_1se <- which(cv_fit$cvm <= min(cv_fit$cvm) + cv_fit$cvsd[which.min(cv_fit$cvm)])
                cv_fit$lambda.1se <- cv_fit$lambda[min(idx_1se)]
            } else {
                cv_fit <- glmnet::cv.glmnet(
                    x = X, y = y_surv,
                    family = "cox",
                    alpha = alpha_sgl,
                    penalty.factor = penalty_factor,
                    nfolds = nfolds_safe,
                    standardize = FALSE,
                    lambda = lambda_seq
                )
            }

            # Fit full model across the lambda sequence
            full_fit <- glmnet::glmnet(
                x = X, y = y_surv,
                family = "cox",
                alpha = alpha_sgl,
                penalty.factor = penalty_factor,
                standardize = FALSE,
                lambda = lambda_seq
            )

            # Extract coefficient path (single call, no per-lambda loop)
            coefficients_path <- as.matrix(coef(full_fit, s = lambda_seq))

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

            # Select optimal lambda (pass n_obs for AIC/BIC/EBIC)
            optimal_index <- private$.selectOptimalLambda(cv_mean, cv_se, lambda_seq, df_path, n_obs = nrow(X))

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

        .logSumExp = function(x) {
            # Numerically stable log(sum(exp(x)))
            m <- max(x)
            m + log(sum(exp(x - m)))
        },

        .calculateDeviance = function(X, y_time, y_event, coefficients) {
            # Calculate -2 * Cox partial log-likelihood (Breslow)
            if (sum(abs(coefficients)) == 0) {
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

            linear_pred <- as.numeric(X %*% coefficients)
            tryCatch({
                order_idx <- order(y_time)
                sorted_lp <- linear_pred[order_idx]
                sorted_event <- y_event[order_idx]
                n <- length(y_time)

                loglik <- 0
                for (i in 1:n) {
                    if (sorted_event[i] == 1) {
                        risk_set_lp <- sorted_lp[i:n]
                        log_risk_sum <- private$.logSumExp(risk_set_lp)
                        loglik <- loglik + sorted_lp[i] - log_risk_sum
                    }
                }
                return(-2 * loglik)
            }, error = function(e) {
                return(NA)
            })
        },

        .selectOptimalLambda = function(cv_mean, cv_se, lambda_seq, df_path, n_obs = NULL) {
            criterion <- self$options$selection_criterion

            if (criterion == "cv_deviance" || criterion == "cv_c_index") {
                # Both use minimum CV error (glmnet cvm is deviance-based)
                return(which.min(cv_mean))
            }

            # For information criteria, n = number of observations (not lambdas)
            n <- if (!is.null(n_obs)) n_obs else length(cv_mean)
            p_total <- max(df_path, na.rm = TRUE)

            if (criterion == "aic") {
                # AIC = deviance + 2 * df
                aic_values <- cv_mean * n + 2 * df_path
                return(which.min(aic_values))
            } else if (criterion == "bic") {
                # BIC = deviance + log(n) * df
                bic_values <- cv_mean * n + log(n) * df_path
                return(which.min(bic_values))
            } else if (criterion == "ebic") {
                # Extended BIC = BIC + 2 * gamma * log(choose(p, df))
                gamma <- self$options$ebic_gamma
                log_choose <- numeric(length(df_path))
                for (k in seq_along(df_path)) {
                    dk <- max(0, min(df_path[k], p_total))
                    log_choose[k] <- if (dk > 0 && dk < p_total) lchoose(p_total, dk) else 0
                }
                ebic_values <- cv_mean * n + log(n) * df_path + 2 * gamma * log_choose
                return(which.min(ebic_values))
            }

            return(which.min(cv_mean))
        },

        .performStabilitySelection = function(X, y_time, y_event, groups, lambda_seq) {
            # Stability selection: subsample repeatedly and fit entire lambda path
            n_bootstrap <- min(self$options$bootstrap_samples, 200)
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

            # Populate solution path
            if (self$options$show_path) {
                private$.populateSolutionPath(results)
            }

            # Populate stability results if available
            if (self$options$stability_selection && !is.null(results$stability_results)) {
                private$.populateStabilityTable(results)
            }

            # Populate adaptive weights table
            if (self$options$weight_type != "none" && !is.null(results$adaptive_weights)) {
                private$.populateAdaptiveWeightsTable(results)
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
            has_ci <- !is.null(results$bootstrap_ci)

            # Only show selected variables
            selected_idx <- which(abs(optimal_coefs) > 1e-8)

            if (length(selected_idx) == 0) {
                table$addRow(rowKey = "none", values = list(
                    variable = "No variables selected",
                    group = "-",
                    coefficient = 0,
                    hazard_ratio = 1,
                    standardized_coef = 0,
                    ci_lower = NA,
                    ci_upper = NA,
                    importance = 0,
                    selection_frequency = 0
                ))
                return()
            }

            # Compute selection frequency: stability > bootstrap > default (1.0)
            sel_freq <- rep(1.0, length(optimal_coefs))
            if (!is.null(results$stability_results)) {
                probs <- results$stability_results$selection_probs
                sel_freq <- apply(probs, 1, max)
            } else if (has_ci) {
                # Use bootstrap inclusion frequency as proxy
                boot_coefs <- results$bootstrap_ci$boot_coefs
                sel_freq <- colMeans(abs(boot_coefs) > 1e-8)
            }

            for (i in selected_idx) {
                coef_val <- optimal_coefs[i]

                ci_lo <- if (has_ci) results$bootstrap_ci$ci_lower[i] else NA
                ci_hi <- if (has_ci) results$bootstrap_ci$ci_upper[i] else NA

                row_values <- list(
                    variable = var_names[i],
                    group = results$group_names[groups[i]],
                    coefficient = coef_val,
                    hazard_ratio = exp(coef_val),
                    standardized_coef = coef_val,
                    ci_lower = ci_lo,
                    ci_upper = ci_hi,
                    importance = abs(coef_val),
                    selection_frequency = sel_freq[i]
                )

                table$addRow(rowKey = var_names[i], values = row_values)
            }

            if (has_ci) {
                alpha <- self$options$alpha_level
                table$setNote("ci", paste0(
                    "CI: ", round((1 - alpha) * 100), "% bootstrap percentile interval (",
                    self$options$bootstrap_samples, " resamples)."
                ))
            }
        },

        .populateGroupStructureTable = function(results) {
            table <- self$results$groupStructure

            groups <- results$groups
            var_names <- results$variable_names
            optimal_coefs <- results$cv_results$optimal_coefficients
            alpha_sgl <- self$options$alpha_sgl
            adaptive_weights <- results$adaptive_weights

            unique_groups <- unique(groups)

            for (g in unique_groups) {
                group_vars <- which(groups == g)
                group_coefs <- optimal_coefs[group_vars]
                n_selected_in_group <- sum(abs(group_coefs) > 1e-8)
                sparsity_within <- (length(group_vars) - n_selected_in_group) / length(group_vars) * 100

                # Compute the actual group penalty (same formula as .fitSparseGroupLASSO)
                group_size <- length(group_vars)
                avg_pf <- mean(sapply(group_vars, function(j) {
                    pf_ind <- adaptive_weights$individual[j]
                    pf_grp <- adaptive_weights$group[g] * sqrt(group_size)
                    alpha_sgl * pf_ind + (1 - alpha_sgl) * pf_grp
                }))

                row_values <- list(
                    group_id = g,
                    group_name = results$group_names[g],
                    n_variables = group_size,
                    group_selected = ifelse(n_selected_in_group > 0, "Yes", "No"),
                    group_penalty = avg_pf,
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
            y_surv <- survival::Surv(results$y_time, results$y_event)

            # Helper: fit a reference model at a given alpha and extract metrics
            .fitReference <- function(alpha_val, label) {
                tryCatch({
                    nfolds_safe <- max(3, min(self$options$cv_folds,
                                              floor(sum(results$y_event) / 3)))
                    ref_cv <- glmnet::cv.glmnet(
                        x = results$X, y = y_surv,
                        family = "cox", alpha = alpha_val,
                        nfolds = nfolds_safe, standardize = FALSE
                    )
                    ref_coefs <- as.numeric(coef(ref_cv, s = ref_cv$lambda.min))
                    n_sel <- sum(abs(ref_coefs) > 1e-8)
                    n_grp_sel <- if (n_sel > 0) length(unique(results$groups[abs(ref_coefs) > 1e-8])) else 0L
                    cv_err <- min(ref_cv$cvm, na.rm = TRUE)
                    ci <- NA
                    if (n_sel > 0) {
                        lp <- as.numeric(results$X %*% ref_coefs)
                        ci <- survival::concordance(y_surv ~ lp, reverse = TRUE)$concordance
                    }
                    list(method = label, n_selected = as.integer(n_sel),
                         n_groups_selected = as.integer(n_grp_sel),
                         cv_error = cv_err, c_index = ci, relative_performance = "Fitted")
                }, error = function(e) {
                    list(method = label, n_selected = NA_integer_,
                         n_groups_selected = NA_integer_,
                         cv_error = NA, c_index = NA,
                         relative_performance = paste0("Error: ", e$message))
                })
            }

            # SGL model (already fitted)
            sgl_coefs <- cv_results$optimal_coefficients
            sgl_n <- sum(abs(sgl_coefs) > 1e-8)
            sgl_grp <- if (sgl_n > 0) length(unique(results$groups[abs(sgl_coefs) > 1e-8])) else 0L
            sgl_ci <- NA
            tryCatch({
                if (sgl_n > 0) {
                    lp <- as.numeric(results$X %*% sgl_coefs)
                    sgl_ci <- survival::concordance(y_surv ~ lp, reverse = TRUE)$concordance
                }
            }, error = function(e) NULL)

            sgl_row <- list(
                method = sprintf("Sparse Group LASSO (alpha=%.2f)", self$options$alpha_sgl),
                n_selected = as.integer(sgl_n),
                n_groups_selected = as.integer(sgl_grp),
                cv_error = cv_results$cv_mean[optimal_idx],
                c_index = sgl_ci,
                relative_performance = "Selected"
            )

            # Fit Group LASSO (alpha near 0) and LASSO (alpha=1)
            grp_row <- .fitReference(0.05, "Group LASSO (alpha~0)")
            lasso_row <- .fitReference(1.0, "LASSO (alpha=1)")

            for (row in list(sgl_row, grp_row, lasso_row)) {
                table$addRow(rowKey = row$method, values = row)
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
            lambda_subset <- stability_results$lambda_seq

            for (i in seq_along(var_names)) {
                max_prob <- max(selection_probs[i, ])

                # Find first and last lambda where variable was selected (prob > 0)
                selected_lambdas <- which(selection_probs[i, ] > 0)
                first_sel <- if (length(selected_lambdas) > 0) lambda_subset[min(selected_lambdas)] else NA
                last_sel <- if (length(selected_lambdas) > 0) lambda_subset[max(selected_lambdas)] else NA

                row_values <- list(
                    variable = var_names[i],
                    group = results$group_names[groups[i]],
                    selection_probability = max_prob * 100,
                    stable_selection = ifelse(stable_vars[i], "Yes", "No"),
                    first_selected = first_sel,
                    last_selected = last_sel
                )

                table$addRow(rowKey = var_names[i], values = row_values)
            }
        },

        .populateSolutionPath = function(results) {
            table <- self$results$solutionPath
            cv_results <- results$cv_results
            lambda_seq <- cv_results$lambda_seq
            n_vars <- nrow(cv_results$coefficients_path)
            groups <- results$groups

            n_show <- min(20, length(lambda_seq))
            indices <- round(seq(1, length(lambda_seq), length.out = n_show))

            for (j in seq_along(indices)) {
                idx <- indices[j]
                coefs_l <- cv_results$coefficients_path[, idx]
                selected <- abs(coefs_l) > 1e-8
                n_sel <- sum(selected)
                n_groups_sel <- if (any(selected)) length(unique(groups[selected])) else 0L
                sparsity <- (n_vars - n_sel) / n_vars * 100

                table$addRow(rowKey = j, values = list(
                    lambda_index = as.integer(idx),
                    lambda_value = lambda_seq[idx],
                    n_selected_vars = as.integer(n_sel),
                    n_selected_groups = as.integer(n_groups_sel),
                    deviance = cv_results$deviance_path[idx],
                    df = cv_results$df_path[idx],
                    sparsity_level = sparsity
                ))
            }
        },

        .populateAdaptiveWeightsTable = function(results) {
            table <- self$results$adaptiveWeights
            var_names <- results$variable_names
            groups <- results$groups
            weights <- results$adaptive_weights

            for (i in seq_along(var_names)) {
                g <- groups[i]
                ind_w <- weights$individual[i]
                grp_w <- weights$group[g]
                combined <- ind_w * grp_w

                rationale <- if (ind_w < 1) "Strong initial effect → less penalty" else "Weak initial effect → more penalty"

                table$addRow(rowKey = var_names[i], values = list(
                    variable = var_names[i],
                    group = results$group_names[g],
                    individual_weight = ind_w,
                    group_weight = grp_w,
                    combined_weight = combined,
                    weight_rationale = rationale
                ))
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

        .bootstrapConfidenceIntervals = function(X, y_time, y_event, groups, optimal_lambda, adaptive_weights) {
            n_boot <- self$options$bootstrap_samples
            alpha <- self$options$alpha_level
            n <- nrow(X)
            n_vars <- ncol(X)

            boot_coefs <- matrix(0, nrow = n_boot, ncol = n_vars)

            for (b in seq_len(n_boot)) {
                boot_idx <- sample(n, n, replace = TRUE)
                boot_X <- X[boot_idx, , drop = FALSE]
                boot_time <- y_time[boot_idx]
                boot_event <- y_event[boot_idx]

                tryCatch({
                    coefs_b <- private$.fitSGLassoSingleLambda(
                        boot_X, boot_time, boot_event,
                        groups, optimal_lambda, adaptive_weights
                    )
                    boot_coefs[b, ] <- coefs_b
                }, error = function(e) {
                    # Leave zeros for failed bootstrap
                })
            }

            # Compute percentile CIs
            ci_lower <- apply(boot_coefs, 2, quantile, probs = alpha / 2, na.rm = TRUE)
            ci_upper <- apply(boot_coefs, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)

            return(list(
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                boot_coefs = boot_coefs
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
                group_names = as.character(results$group_names),
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

            grp_labels <- if (!is.null(state$group_names)) state$group_names[groups] else paste0("Group ", groups)
            plot_data <- data.frame(
                variable = factor(var_names, levels = rev(var_names)),
                group = factor(grp_labels),
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