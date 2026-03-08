plscoxClass <- R6::R6Class(
    "plscoxClass",
    inherit = plscoxBase,
    private = list(
        .init = function() {
            # Show welcome only when no variables are selected
            if (is.null(self$options$time) ||
                is.null(self$options$status) ||
                length(self$options$predictors) == 0) {

                todo <- glue::glue(
                "
                <br>Welcome to ClinicoPath
                <br><br>
                <b>Partial Least Squares Cox Models</b>
                <br><br>
                This analysis performs PLS-based Cox regression for high-dimensional survival data:

                <br><br>
                <b>Analysis Steps:</b>
                <ul>
                <li>1. Select your <b>Time Variable</b> (follow-up time)</li>
                <li>2. Select your <b>Status Variable</b> (event indicator: 1=event, 0=censored)</li>
                <li>3. Select your <b>High-dimensional Predictors</b> (genes, proteins, etc.)</li>
                <li>4. Configure PLS parameters (number of components, algorithm)</li>
                <li>5. Choose cross-validation method for component selection</li>
                <li>6. Select validation and visualization options</li>
                </ul>

                <br>
                <b>Key Features:</b>
                <ul>
                <li><b>Dimensionality Reduction:</b> PLS extracts latent components from high-dimensional data</li>
                <li><b>Supervised Learning:</b> Components are optimized for survival prediction</li>
                <li><b>Cross-Validation:</b> Automatic selection of optimal number of components</li>
                <li><b>Variable Importance:</b> Identification of influential predictors</li>
                <li><b>Risk Stratification:</b> Patient grouping based on PLS risk scores</li>
                <li><b>Bootstrap Validation:</b> Assessment of model stability and performance</li>
                </ul>

                <br>
                <b>When to Use PLS-Cox:</b>
                <ul>
                <li>High-dimensional data (p >> n): genomics, proteomics, metabolomics</li>
                <li>Multicollinearity among predictors</li>
                <li>Need for interpretable components</li>
                <li>Supervised dimensionality reduction for survival outcomes</li>
                </ul>

                <br>
                <b>Interpretation:</b>
                <ul>
                <li><b>PLS Components:</b> Linear combinations of original variables optimized for survival prediction</li>
                <li><b>Variable Loadings:</b> Contribution of each variable to PLS components</li>
                <li><b>Component Scores:</b> Patient-specific values for each PLS component</li>
                <li><b>Hazard Ratios:</b> Effect of each PLS component on hazard of event</li>
                </ul>

                <br>
                <b>Required Packages:</b> survival, plsRcox
                <br>
                <b>Recommended minimum:</b> At least 10-20 events for reliable results
                <br><br>
                "
                )

                self$results$todo$setContent(todo)
            } else {
                self$results$todo$setVisible(visible = FALSE)
            }
        },

        .run = function() {

            # Get options
            time <- self$options$time
            status <- self$options$status
            predictors <- self$options$predictors

            # Check if variables are selected
            if (is.null(time) || is.null(status) || length(predictors) == 0) {
                return()
            }

            # Hide welcome when variables are present
            self$results$todo$setVisible(visible = FALSE)

            # Check required packages
            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$modelSummary$setContent("<h3>Error: Package Not Found</h3><p>The <b>survival</b> package is required but not installed. Please install it using:<br><code>install.packages('survival')</code></p>")
                return()
            }

            if (!requireNamespace("plsRcox", quietly = TRUE)) {
                self$results$modelSummary$setContent("<h3>Error: Package Not Found</h3><p>The <b>plsRcox</b> package is required but not installed. Please install it using:<br><code>install.packages('plsRcox')</code></p>")
                return()
            }

            # Get data
            data <- self$data

            if (nrow(data) == 0) return()

            # Prepare variables
            time_var <- data[[time]]
            status_var <- data[[status]]

            # Handle status variable (ensure 0/1 coding)
            if (is.factor(status_var)) {
                status_levels <- levels(status_var)
                if (length(status_levels) == 2) {
                    status_var <- as.numeric(status_var) - 1
                } else {
                    self$results$modelSummary$setContent("<h3>Error: Status Variable</h3><p>Status variable must be binary (0/1 or factor with 2 levels)</p>")
                    return()
                }
            }

            # Create predictor matrix with proper factor handling
            pred_data <- data[, predictors, drop = FALSE]

            # Dummy-encode factor columns for PLS (which requires numeric matrix)
            has_factors <- any(sapply(pred_data, is.factor))
            if (has_factors) {
                # Build model matrix (dummy encoding), removing intercept
                mm_formula <- as.formula(paste("~", paste(
                    sapply(predictors, function(v) {
                        if (is.factor(pred_data[[v]])) paste0("as.factor(`", v, "`)") else paste0("`", v, "`")
                    }),
                    collapse = " + "
                )))
                pred_matrix <- model.matrix(mm_formula, data = pred_data)[, -1, drop = FALSE]
            } else {
                pred_matrix <- as.matrix(pred_data)
            }

            # Check for constant columns (zero variance)
            col_vars <- apply(pred_matrix, 2, var, na.rm = TRUE)
            if (any(col_vars == 0, na.rm = TRUE)) {
                constant_cols <- names(which(col_vars == 0))
                pred_matrix <- pred_matrix[, col_vars > 0, drop = FALSE]
                if (ncol(pred_matrix) == 0) {
                    self$results$modelSummary$setContent("<h3>Error</h3><p>All predictor variables have zero variance after encoding.</p>")
                    return()
                }
            }

            # Check for missing values
            complete_cases <- complete.cases(cbind(time_var, status_var, pred_matrix))
            missing_note <- ""
            if (sum(complete_cases) < nrow(data)) {
                missing_count <- nrow(data) - sum(complete_cases)
                missing_note <- glue::glue("<p><i>Note: {missing_count} observations with missing values were excluded from analysis.</i></p>")
            }

            # Filter complete cases
            time_var <- time_var[complete_cases]
            status_var <- status_var[complete_cases]
            pred_matrix <- pred_matrix[complete_cases, , drop = FALSE]

            # Check minimum sample size
            n_events <- sum(status_var)
            if (n_events < 10) {
                self$results$modelSummary$setContent("<h3>Warning: Insufficient Events</h3><p>Less than 10 events observed. Results may be unreliable. Consider collecting more data.</p>")
                return()
            }

            # Variable scaling (plsRcox has its own scaleX parameter, but we
            # apply user-selected scaling for transparency and for non-standard methods)
            scaling_method <- self$options$scaling_method
            use_plsRcox_scaling <- FALSE
            if (scaling_method == "standardize") {
                # Let plsRcox handle standard scaling via scaleX=TRUE
                use_plsRcox_scaling <- TRUE
            } else if (scaling_method == "unit_variance") {
                pred_matrix <- scale(pred_matrix, center = FALSE, scale = TRUE)
            } else if (scaling_method == "minmax") {
                pred_matrix <- apply(pred_matrix, 2, function(x) {
                    rng <- max(x) - min(x)
                    if (rng == 0) return(rep(0.5, length(x)))
                    (x - min(x)) / rng
                })
            }
            # else: scaling_method == "none", no scaling

            # Read all user options
            pls_algorithm <- self$options$pls_algorithm
            component_selection <- self$options$component_selection
            max_iterations_opt <- self$options$max_iterations
            tolerance_opt <- self$options$tolerance
            show_ci <- self$options$confidence_intervals

            # Assess data suitability if requested
            if (self$options$suitabilityCheck) {
                private$.assessSuitability(pred_matrix, time_var, status_var)
            }

            # Perform PLS Cox regression using plsRcox
            tryCatch({

                # Cross-validation setup
                cv_method <- self$options$cross_validation
                if (cv_method == "loo") {
                    cv_folds <- nrow(pred_matrix)
                } else if (cv_method == "k10") {
                    cv_folds <- 10
                } else if (cv_method == "k5") {
                    cv_folds <- 5
                } else {
                    cv_folds <- 0 # sentinel for "none"
                }

                # Maximum components (bounded by data dimensions)
                max_components <- min(
                    self$options$pls_components,
                    ncol(pred_matrix) - 1,
                    nrow(pred_matrix) - 2
                )
                max_components <- max(max_components, 1)

                # Determine optimal number of components
                cv_results <- NULL
                if (component_selection == "manual") {
                    # Manual: use user-specified number directly
                    optimal_nt <- min(self$options$pls_components, max_components)

                } else if (cv_method != "none" && component_selection %in% c("cv_loglik", "cv_cindex")) {
                    # Cross-validated PLS Cox using plsRcox::cv.plsRcox
                    # API: data = list(x=, time=, status=), nfold=, nt=
                    cv_data_list <- list(
                        x = pred_matrix,
                        time = time_var,
                        status = status_var
                    )

                    pls_cv <- plsRcox::cv.plsRcox(
                        data = cv_data_list,
                        nt = max_components,
                        nfold = cv_folds,
                        scaleX = use_plsRcox_scaling,
                        plot.it = FALSE,
                        verbose = FALSE,
                        allCVcrit = (component_selection == "cv_cindex")
                    )

                    # Extract optimal component count
                    # cv.plsRcox stores results as lambda.min{K} and cv.error{K}
                    cv_error_name <- paste0("cv.error", cv_folds)
                    cv_se_name <- paste0("cv.se", cv_folds)
                    lambda_min_name <- paste0("lambda.min", cv_folds)

                    if (!is.null(pls_cv[[lambda_min_name]])) {
                        optimal_nt <- pls_cv[[lambda_min_name]]
                    } else {
                        # Fallback: use the nt value from the result
                        optimal_nt <- max_components
                    }

                    # Ensure optimal_nt is at least 1
                    optimal_nt <- max(optimal_nt, 1)

                    # Build CV results table for display
                    cv_errors <- pls_cv[[cv_error_name]]
                    cv_ses <- pls_cv[[cv_se_name]]
                    if (!is.null(cv_errors)) {
                        cv_results <- data.frame(
                            n_components = seq_along(cv_errors),
                            cv_score = cv_errors,
                            se_cv_score = if (!is.null(cv_ses)) cv_ses else rep(NA, length(cv_errors)),
                            c_index = NA,
                            selected = ifelse(seq_along(cv_errors) == optimal_nt, "Yes", "No")
                        )
                    }

                } else if (component_selection %in% c("bic", "aic")) {
                    # Fit models with 1..max_components and pick by AIC/BIC
                    ic_values <- numeric(max_components)
                    for (k in seq_len(max_components)) {
                        tryCatch({
                            tmp_model <- plsRcox::plsRcox(
                                Xplan = pred_matrix,
                                time = time_var,
                                event = status_var,
                                nt = k,
                                scaleX = use_plsRcox_scaling,
                                verbose = FALSE
                            )
                            tmp_scores <- tmp_model$tt
                            colnames(tmp_scores) <- paste0("PLS_", seq_len(ncol(tmp_scores)))
                            tmp_cox_data <- data.frame(time = time_var, status = status_var, tmp_scores)
                            tmp_formula <- as.formula(paste(
                                "survival::Surv(time, status) ~",
                                paste(colnames(tmp_scores), collapse = " + ")
                            ))
                            tmp_cox <- survival::coxph(tmp_formula, data = tmp_cox_data)
                            if (component_selection == "aic") {
                                ic_values[k] <- extractAIC(tmp_cox)[2]
                            } else {
                                aic_val <- extractAIC(tmp_cox)[2]
                                df_val <- extractAIC(tmp_cox)[1]
                                ic_values[k] <- aic_val + (df_val * (log(nrow(tmp_cox_data)) - 2))
                            }
                        }, error = function(e) {
                            ic_values[k] <<- Inf
                        })
                    }
                    optimal_nt <- which.min(ic_values)
                    if (length(optimal_nt) == 0) optimal_nt <- 1

                    cv_results <- data.frame(
                        n_components = seq_len(max_components),
                        cv_score = ic_values,
                        se_cv_score = NA,
                        c_index = NA,
                        selected = ifelse(seq_len(max_components) == optimal_nt, "Yes", "No")
                    )
                } else {
                    # No cross-validation, no manual: use max_components
                    optimal_nt <- max_components
                }

                # Populate component selection table
                if (!is.null(cv_results)) {
                    for (row_i in seq_len(nrow(cv_results))) {
                        self$results$componentSelection$addRow(
                            rowKey = row_i,
                            values = as.list(cv_results[row_i, ])
                        )
                    }
                }

                # Fit final PLS Cox model with optimal components
                pls_model <- plsRcox::plsRcox(
                    Xplan = pred_matrix,
                    time = time_var,
                    event = status_var,
                    nt = optimal_nt,
                    scaleX = use_plsRcox_scaling,
                    tol_Xi = tolerance_opt,
                    verbose = FALSE
                )

                # Extract PLS components
                pls_scores <- pls_model$tt
                if (is.null(pls_scores)) {
                    self$results$modelSummary$setContent("<h3>Error</h3><p>PLS model did not produce component scores. Check your data and settings.</p>")
                    return()
                }
                colnames(pls_scores) <- paste0("PLS_", seq_len(ncol(pls_scores)))

                # Fit Cox model on PLS components
                cox_data <- data.frame(
                    time = time_var,
                    status = status_var,
                    pls_scores
                )

                cox_formula <- as.formula(paste("survival::Surv(time, status) ~",
                                               paste(colnames(pls_scores), collapse = " + ")))

                cox_model <- survival::coxph(cox_formula, data = cox_data)

                # Model summary
                algo_label <- switch(pls_algorithm,
                    nipals = "NIPALS",
                    kernel = "Kernel PLS",
                    widekernelpls = "Wide Kernel PLS",
                    pls_algorithm
                )
                comp_sel_label <- switch(component_selection,
                    cv_loglik = "Cross-Validated Log-Likelihood",
                    cv_cindex = "Cross-Validated C-Index",
                    bic = "BIC",
                    aic = "AIC",
                    manual = "Manual",
                    component_selection
                )

                summary_text <- glue::glue("
                <h3>PLS Cox Model Results</h3>
                {missing_note}
                <p><b>Analysis Summary:</b></p>
                <ul>
                <li>Sample size: {nrow(pred_matrix)} subjects</li>
                <li>Number of events: {n_events}</li>
                <li>Number of predictors: {ncol(pred_matrix)}</li>
                <li>PLS components used: {optimal_nt}</li>
                <li>PLS algorithm: {algo_label}</li>
                <li>Component selection: {comp_sel_label}</li>
                <li>Scaling method: {scaling_method}</li>
                <li>Cross-validation: {cv_method} ({cv_folds} folds)</li>
                </ul>

                <p><b>Model Performance:</b></p>
                <ul>
                <li>Training Concordance Index: {round(summary(cox_model)$concordance[1], 3)} (SE: {round(summary(cox_model)$concordance[2], 3)})</li>
                <li>Likelihood ratio test: {round(summary(cox_model)$logtest[1], 2)} (p = {format.pval(summary(cox_model)$logtest[3], digits = 4)})</li>
                <li>Wald test: {round(summary(cox_model)$waldtest[1], 2)} (p = {format.pval(summary(cox_model)$waldtest[3], digits = 4)})</li>
                </ul>
                <p><i>Note: The Training Concordance Index overestimates true out-of-sample performance, especially for high-dimensional data. Use Bootstrap or Permutation tests for rigorous validation.</i></p>
                ")

                self$results$modelSummary$setContent(summary_text)

                # Model coefficients table
                cox_summary <- summary(cox_model)
                n_comp_actual <- ncol(pls_scores)
                coef_mat <- cox_summary$coefficients
                ci_mat <- cox_summary$conf.int

                for (k in seq_len(n_comp_actual)) {
                    row_vals <- list(
                        component = paste("PLS Component", k),
                        coefficient = coef_mat[k, "coef"],
                        hr = coef_mat[k, "exp(coef)"],
                        hr_lower = if (show_ci) ci_mat[k, "lower .95"] else NA,
                        hr_upper = if (show_ci) ci_mat[k, "upper .95"] else NA,
                        se = coef_mat[k, "se(coef)"],
                        z_value = coef_mat[k, "z"],
                        p_value = coef_mat[k, "Pr(>|z|)"]
                    )
                    self$results$modelCoefficients$addRow(rowKey = k, values = row_vals)
                }

                # Variable loadings and importance
                if (self$options$feature_importance) {
                    loadings_matrix <- pls_model$wwetoile

                    if (!is.null(loadings_matrix) && nrow(loadings_matrix) > 0) {
                        # Calculate variable importance scores
                        importance_scores <- apply(abs(loadings_matrix), 1, function(x) sqrt(sum(x^2)))

                        # Sort by importance
                        sorted_idx <- order(-importance_scores)

                        for (vi in sorted_idx) {
                            row_vals <- list(
                                variable = rownames(loadings_matrix)[vi],
                                component_1 = if (ncol(loadings_matrix) >= 1) loadings_matrix[vi, 1] else NA,
                                component_2 = if (ncol(loadings_matrix) >= 2) loadings_matrix[vi, 2] else NA,
                                component_3 = if (ncol(loadings_matrix) >= 3) loadings_matrix[vi, 3] else NA,
                                importance_score = importance_scores[vi]
                            )
                            self$results$variableLoadings$addRow(rowKey = vi, values = row_vals)
                        }
                    }
                }

                # Model performance metrics
                if (self$options$prediction_accuracy) {

                    # C-index with confidence interval
                    c_index <- summary(cox_model)$concordance[1]
                    c_index_se <- summary(cox_model)$concordance[2]

                    aic_val <- extractAIC(cox_model)[2]
                    df_val <- extractAIC(cox_model)[1]
                    bic_val <- aic_val + (df_val * (log(nrow(cox_data)) - 2))

                    perf_rows <- list(
                        list(metric = "Training Concordance Index", value = c_index,
                             se = c_index_se,
                             lower_ci = c_index - 1.96 * c_index_se,
                             upper_ci = c_index + 1.96 * c_index_se),
                        list(metric = "R-squared (Nagelkerke)", value = summary(cox_model)$rsq["rsq"],
                             se = NA, lower_ci = NA, upper_ci = NA),
                        list(metric = "AIC", value = aic_val,
                             se = NA, lower_ci = NA, upper_ci = NA),
                        list(metric = "BIC", value = bic_val,
                             se = NA, lower_ci = NA, upper_ci = NA)
                    )

                    for (pi in seq_along(perf_rows)) {
                        self$results$modelPerformance$addRow(rowKey = pi, values = perf_rows[[pi]])
                    }
                }

                # Risk stratification
                risk_groups <- self$options$risk_groups
                linear_pred <- predict(cox_model, type = "lp")
                risk_quantiles <- quantile(linear_pred, probs = seq(0, 1, length.out = risk_groups + 1))
                # Ensure unique breaks
                if (length(unique(risk_quantiles)) < length(risk_quantiles)) {
                    risk_quantiles <- unique(risk_quantiles)
                    risk_groups <- length(risk_quantiles) - 1
                }
                risk_categories <- cut(linear_pred, breaks = risk_quantiles,
                                     labels = paste("Risk Group", seq_len(risk_groups)),
                                     include.lowest = TRUE)

                # Risk group analysis
                risk_data <- data.frame(
                    time = time_var,
                    status = status_var,
                    risk_group = risk_categories
                )

                # Survival analysis by risk group
                risk_fit <- survival::survfit(survival::Surv(time, status) ~ risk_group, data = risk_data)
                fit_summary <- summary(risk_fit)$table

                for (gi in seq_len(risk_groups)) {
                    rg_name <- paste("Risk Group", gi)
                    rg_subset <- risk_data[risk_data$risk_group == rg_name, ]
                    n_subj <- nrow(rg_subset)
                    n_ev <- sum(rg_subset$status)

                    # Median survival from survfit table
                    med_surv <- if (nrow(fit_summary) >= gi) fit_summary[gi, "median"] else NA
                    se_med <- if (nrow(fit_summary) >= gi) fit_summary[gi, "se(median)"] else NA

                    hr_val <- NA
                    hr_p <- NA
                    if (gi == 1) {
                        hr_val <- 1.0
                    } else if (gi > 1) {
                        tryCatch({
                            subset_data <- risk_data[risk_data$risk_group %in% paste("Risk Group", c(1, gi)), ]
                            subset_data$risk_group <- droplevels(subset_data$risk_group)
                            if (nrow(subset_data) > 0 && length(unique(subset_data$risk_group)) == 2) {
                                subset_cox <- survival::coxph(
                                    survival::Surv(time, status) ~ risk_group,
                                    data = subset_data
                                )
                                hr_val <- exp(coef(subset_cox))
                                hr_p <- summary(subset_cox)$coefficients[, "Pr(>|z|)"]
                            }
                        }, error = function(e) {})
                    }

                    self$results$riskStratification$addRow(
                        rowKey = gi,
                        values = list(
                            risk_group = rg_name,
                            n_subjects = n_subj,
                            n_events = n_ev,
                            median_survival = med_surv,
                            survival_se = se_med,
                            hr_vs_low = hr_val,
                            hr_p_value = hr_p
                        )
                    )
                }

                # Store plot data as serializable data frames (avoid protobuf issues)
                private$plsResults <- list(
                    pls_scores = as.data.frame(pls_scores),
                    loadings_matrix = if (!is.null(pls_model$wwetoile)) as.data.frame(pls_model$wwetoile) else NULL,
                    inf_crit = pls_model$InfCrit,
                    time_var = time_var,
                    status_var = status_var,
                    risk_categories = risk_categories,
                    optimal_nt = optimal_nt,
                    cv_results = cv_results,
                    cox_concordance = summary(cox_model)$concordance[1],
                    cox_concordance_se = summary(cox_model)$concordance[2],
                    linear_pred = linear_pred
                )

                # Clinical guidance
                guidance_text <- "
                <h3>Clinical Interpretation Guide</h3>

                <h4>PLS Components</h4>
                <p>PLS components represent linear combinations of your original predictors that are optimally related to survival outcomes. Each component captures a different aspect of the biological variation in your data.</p>

                <h4>Variable Loadings</h4>
                <p>Loadings indicate how much each original variable contributes to each PLS component. Variables with higher absolute loadings have stronger influence on that component.</p>

                <h4>Hazard Ratios</h4>
                <p><b>Note: The Hazard Ratios and p-values correspond to the abstract PLS components, not your original variables.</b> Each PLS component's hazard ratio indicates the relative risk associated with a one-unit increase in that component score. HR > 1 indicates increased risk, HR &lt; 1 indicates decreased risk.</p>

                <h4>Risk Stratification</h4>
                <p>Patients are grouped based on their overall PLS risk score. Higher risk groups should show shorter survival times and more events.</p>

                <h4>Model Validation</h4>
                <p>Cross-validation helps select the optimal number of components. Bootstrap validation can assess model stability and provide confidence intervals for performance metrics.</p>

                <h4>Clinical Application</h4>
                <p>This model can be used for:</p>
                <ul>
                <li>Risk stratification of patients</li>
                <li>Identification of prognostic biomarker signatures</li>
                <li>Treatment decision support</li>
                <li>Clinical trial stratification</li>
                </ul>
                "

                self$results$clinicalGuidance$setContent(guidance_text)

                # Technical notes
                technical_text <- "
                <h3>Technical Notes and Assumptions</h3>

                <h4>PLS Cox Methodology</h4>
                <p>This analysis combines Partial Least Squares (PLS) dimensionality reduction with Cox proportional hazards regression. PLS finds components that maximize covariance between predictors and the survival outcome.</p>

                <h4>Model Assumptions</h4>
                <ul>
                <li><b>Proportional Hazards:</b> The hazard ratio for each component is constant over time</li>
                <li><b>Linear Relationships:</b> Log-hazard is linear in the PLS components</li>
                <li><b>Independence:</b> Observations are independent</li>
                <li><b>Non-informative Censoring:</b> Censoring is independent of the event process</li>
                </ul>

                <h4>Component Selection</h4>
                <p>Cross-validation is used to select the optimal number of PLS components to avoid overfitting while maintaining predictive performance.</p>

                <h4>Variable Scaling</h4>
                <p>Predictor variables should be scaled when they have different units or vastly different ranges to ensure fair contribution to PLS components.</p>

                <h4>Sample Size Considerations</h4>
                <p>For reliable results, aim for at least 10-15 events per PLS component included in the model. With high-dimensional data, cross-validation becomes crucial.</p>

                <h4>Interpretation Cautions</h4>
                <ul>
                <li>PLS components are linear combinations - biological interpretation may be complex</li>
                <li>Variable importance should be interpreted in context of component loadings</li>
                <li>External validation is recommended before clinical application</li>
                </ul>
                "

                self$results$technicalNotes$setContent(technical_text)

                # Bootstrap validation
                if (self$options$bootstrap_validation) {
                    n_bootstrap <- self$options$n_bootstrap

                    bootstrap_c_indices <- numeric(n_bootstrap)
                    valid_bootstraps <- 0

                    for (i in seq_len(n_bootstrap)) {
                        tryCatch({
                            # Resample
                            boot_idx <- sample(nrow(pred_matrix), replace = TRUE)
                            boot_X <- pred_matrix[boot_idx, , drop = FALSE]
                            boot_time <- time_var[boot_idx]
                            boot_status <- status_var[boot_idx]

                            if (sum(boot_status) > 5) {
                                boot_pls <- plsRcox::plsRcox(
                                    Xplan = boot_X,
                                    time = boot_time,
                                    event = boot_status,
                                    nt = optimal_nt,
                                    scaleX = use_plsRcox_scaling,
                                    tol_Xi = tolerance_opt,
                                    verbose = FALSE
                                )

                                boot_scores <- boot_pls$tt
                                if (!is.null(boot_scores)) {
                                    boot_cox_data <- data.frame(
                                        time = boot_time,
                                        status = boot_status,
                                        boot_scores
                                    )
                                    colnames(boot_cox_data)[3:ncol(boot_cox_data)] <- paste0("Comp", seq_len(ncol(boot_scores)))

                                    boot_formula <- as.formula(paste("survival::Surv(time, status) ~",
                                                                   paste(colnames(boot_cox_data)[3:ncol(boot_cox_data)], collapse = " + ")))

                                    boot_c <- survival::concordance(boot_formula, data = boot_cox_data, reverse = TRUE)$concordance
                                    valid_bootstraps <- valid_bootstraps + 1
                                    bootstrap_c_indices[valid_bootstraps] <- boot_c
                                }
                            }
                        }, error = function(e) {
                            # Skip failed bootstraps
                        })
                    }

                    if (valid_bootstraps > 0) {
                        bootstrap_c_indices <- bootstrap_c_indices[seq_len(valid_bootstraps)]
                        mean_boot_c <- mean(bootstrap_c_indices)
                        sd_boot_c <- sd(bootstrap_c_indices)
                        ci_lower <- quantile(bootstrap_c_indices, 0.025)
                        ci_upper <- quantile(bootstrap_c_indices, 0.975)

                        bootstrap_text <- glue::glue("
                        <h3>Bootstrap Validation Results</h3>
                        <p><b>Bootstrap validation with {valid_bootstraps} successful replications</b> (Target: {n_bootstrap})</p>
                        <ul>
                        <li><b>Mean Bootstrap C-Index:</b> {round(mean_boot_c, 3)}</li>
                        <li><b>Standard Error:</b> {round(sd_boot_c, 3)}</li>
                        <li><b>95% Confidence Interval:</b> {round(ci_lower, 3)} - {round(ci_upper, 3)}</li>
                        </ul>
                        <p><i>Note:</i> Confidence intervals reflect the variability of the model performance on resampled data (internal validity).</p>
                        ")
                    } else {
                        bootstrap_text <- "<h3>Bootstrap Validation Failed</h3><p>Could not generate valid bootstrap models (possibly due to small sample size or convergence issues).</p>"
                    }

                    self$results$bootstrapResults$setContent(bootstrap_text)
                }

                # Permutation test
                if (self$options$permutation_test) {
                    n_permutations <- self$options$n_permutations

                    # Original C-index
                    original_c <- summary(cox_model)$concordance[1]

                    perm_c_indices <- numeric(n_permutations)
                    valid_perms <- 0

                    for (i in seq_len(n_permutations)) {
                        tryCatch({
                            # Permute outcome
                            perm_idx <- sample(nrow(pred_matrix))
                            perm_time <- time_var[perm_idx]
                            perm_status <- status_var[perm_idx]

                            # Fit PLS on permuted data
                            perm_pls <- plsRcox::plsRcox(
                                Xplan = pred_matrix,
                                time = perm_time,
                                event = perm_status,
                                nt = optimal_nt,
                                scaleX = use_plsRcox_scaling,
                                tol_Xi = tolerance_opt,
                                verbose = FALSE
                            )

                            perm_scores <- perm_pls$tt
                            if (!is.null(perm_scores)) {
                                perm_cox_data <- data.frame(
                                    time = perm_time,
                                    status = perm_status,
                                    perm_scores
                                )
                                colnames(perm_cox_data)[3:ncol(perm_cox_data)] <- paste0("Comp", seq_len(ncol(perm_scores)))

                                perm_formula <- as.formula(paste("survival::Surv(time, status) ~",
                                                               paste(colnames(perm_cox_data)[3:ncol(perm_cox_data)], collapse = " + ")))

                                perm_c <- survival::concordance(perm_formula, data = perm_cox_data)$concordance
                                valid_perms <- valid_perms + 1
                                perm_c_indices[valid_perms] <- perm_c
                            }
                        }, error = function(e) {
                            # Skip failed permutations
                        })
                    }

                    if (valid_perms > 0) {
                        perm_c_indices <- perm_c_indices[seq_len(valid_perms)]
                        n_better <- sum(perm_c_indices >= original_c)
                        p_val <- (n_better + 1) / (valid_perms + 1)

                        permutation_text <- glue::glue("
                        <h3>Permutation Test Results</h3>
                        <p><b>Permutation testing with {valid_perms} successful permutations</b> (Target: {n_permutations})</p>
                        <ul>
                        <li><b>Original C-Index:</b> {round(original_c, 3)}</li>
                        <li><b>Mean Permuted C-Index:</b> {round(mean(perm_c_indices), 3)} (expected ~0.5)</li>
                        <li><b>P-value:</b> {format.pval(p_val, digits = 4)}</li>
                        </ul>
                        <p><i>Interpretation:</i> The p-value represents the probability of observing a C-index as high as {round(original_c, 3)} by chance alone (if predictors had no relationship to survival).</p>
                        ")
                    } else {
                        permutation_text <- "<h3>Permutation Test Failed</h3><p>Could not generate valid permutation models.</p>"
                    }

                    self$results$permutationResults$setContent(permutation_text)
                }

            }, error = function(e) {
                error_msg <- glue::glue("<h3>Error in PLS Cox Analysis</h3><p>An error occurred during analysis: {e$message}</p><p>Please check your data and parameter settings.</p>")
                self$results$modelSummary$setContent(error_msg)
                return()
            })
        },

        # Plotting functions
        .plotComponents = function(image, ...) {
            if (is.null(private$plsResults)) return(FALSE)

            # Use InfCrit from plsRcox model for component information
            inf_crit <- private$plsResults$inf_crit
            optimal_nt <- private$plsResults$optimal_nt

            if (is.null(inf_crit)) {
                # Fallback: plot component index vs Cox concordance contribution
                plot_data <- data.frame(
                    Component = seq_len(optimal_nt),
                    Value = seq_len(optimal_nt)
                )
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Component, y = Value)) +
                    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
                    ggplot2::labs(
                        title = "PLS Components Used",
                        x = "PLS Component",
                        y = "Component Index"
                    ) +
                    ggplot2::theme_minimal()
                print(p)
                return(TRUE)
            }

            # InfCrit is a matrix with rows = components, columns = criteria
            if (is.matrix(inf_crit) || is.data.frame(inf_crit)) {
                n_comp <- nrow(inf_crit)
                # Try to extract AIC or loglikelihood columns
                if ("AIC" %in% colnames(inf_crit)) {
                    plot_data <- data.frame(
                        Component = seq_len(n_comp),
                        AIC = inf_crit[, "AIC"]
                    )
                    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Component, y = AIC)) +
                        ggplot2::geom_line(color = "steelblue", linewidth = 1) +
                        ggplot2::geom_point(color = "steelblue", size = 3) +
                        ggplot2::geom_vline(xintercept = optimal_nt, linetype = "dashed", color = "red", alpha = 0.7) +
                        ggplot2::labs(
                            title = "PLS Component Selection",
                            subtitle = paste("Optimal:", optimal_nt, "components"),
                            x = "Number of PLS Components",
                            y = "AIC"
                        ) +
                        ggplot2::theme_minimal() +
                        ggplot2::theme(
                            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                            plot.subtitle = ggplot2::element_text(hjust = 0.5)
                        )
                    print(p)
                }
            }

            return(TRUE)
        },

        .plotLoadings = function(image, ...) {
            if (is.null(private$plsResults)) return(FALSE)
            if (!self$options$feature_importance) return(FALSE)

            loadings_df <- private$plsResults$loadings_matrix
            if (is.null(loadings_df)) return(FALSE)

            loadings_matrix <- as.matrix(loadings_df)

            # Plot loadings for first two components
            if (ncol(loadings_matrix) >= 2) {
                plot_data <- data.frame(
                    Variable = rownames(loadings_matrix),
                    PC1 = loadings_matrix[, 1],
                    PC2 = loadings_matrix[, 2]
                )

                # Color by importance
                importance_scores <- sqrt(plot_data$PC1^2 + plot_data$PC2^2)
                plot_data$Importance <- importance_scores

                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC1, y = PC2, color = Importance)) +
                    ggplot2::geom_point(alpha = 0.7, size = 2) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
                    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
                    ggplot2::scale_color_gradient(low = "lightblue", high = "darkred") +
                    ggplot2::labs(
                        title = "PLS Variable Loadings",
                        subtitle = "Variable contributions to first two PLS components",
                        x = "PLS Component 1",
                        y = "PLS Component 2"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5)
                    )

                # Add variable labels for top contributors
                if (requireNamespace("ggrepel", quietly = TRUE)) {
                    top_vars <- plot_data[order(-plot_data$Importance)[1:min(10, nrow(plot_data))], ]
                    p <- p + ggrepel::geom_text_repel(
                        data = top_vars,
                        ggplot2::aes(label = Variable),
                        size = 3,
                        alpha = 0.8
                    )
                }

                print(p)
            } else if (ncol(loadings_matrix) == 1) {
                plot_data <- data.frame(
                    Variable = rownames(loadings_matrix),
                    Loading = loadings_matrix[, 1]
                )
                plot_data <- plot_data[order(-abs(plot_data$Loading)), ]
                # Show top 20
                if (nrow(plot_data) > 20) plot_data <- plot_data[1:20, ]
                plot_data$Variable <- factor(plot_data$Variable, levels = rev(plot_data$Variable))

                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Variable, y = Loading)) +
                    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
                    ggplot2::coord_flip() +
                    ggplot2::labs(
                        title = "PLS Component 1 Variable Loadings",
                        subtitle = "Top contributing variables",
                        x = "",
                        y = "Loading"
                    ) +
                    ggplot2::theme_minimal()
                print(p)
            }

            return(TRUE)
        },

        .plotScores = function(image, ...) {
            if (is.null(private$plsResults)) return(FALSE)

            pls_scores <- as.matrix(private$plsResults$pls_scores)
            time_var <- private$plsResults$time_var
            status_var <- private$plsResults$status_var

            # Plot first component scores vs survival
            if (ncol(pls_scores) >= 1) {
                plot_data <- data.frame(
                    PLS_Score = pls_scores[, 1],
                    Time = time_var,
                    Status = factor(status_var, levels = c(0, 1), labels = c("Censored", "Event"))
                )

                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PLS_Score, y = Time, color = Status)) +
                    ggplot2::geom_point(alpha = 0.6, size = 2) +
                    ggplot2::scale_color_manual(values = c("Censored" = "blue", "Event" = "red")) +
                    ggplot2::labs(
                        title = "PLS Component 1 Scores vs Survival Time",
                        subtitle = "Relationship between first PLS component and survival outcomes",
                        x = "PLS Component 1 Score",
                        y = "Survival Time"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                        plot.subtitle = ggplot2::element_text(hjust = 0.5)
                    )

                print(p)
            }

            return(TRUE)
        },

        .plotValidation = function(image, ...) {
            if (is.null(private$plsResults)) return(FALSE)
            if (is.null(private$plsResults$cv_results)) return(FALSE)

            cv_results <- private$plsResults$cv_results

            p <- ggplot2::ggplot(cv_results, ggplot2::aes(x = n_components, y = cv_score)) +
                ggplot2::geom_line(color = "blue", linewidth = 1) +
                ggplot2::geom_point(color = "blue", size = 2) +
                ggplot2::geom_vline(
                    xintercept = which(cv_results$selected == "Yes"),
                    linetype = "dashed", color = "red", alpha = 0.7
                ) +
                ggplot2::labs(
                    title = "Cross-Validation Results",
                    subtitle = "Selection of optimal number of PLS components",
                    x = "Number of PLS Components",
                    y = "Cross-Validation Score"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5)
                )

            # Add error bars if SE is available
            if (!all(is.na(cv_results$se_cv_score))) {
                p <- p + ggplot2::geom_errorbar(
                    ggplot2::aes(
                        ymin = cv_score - se_cv_score,
                        ymax = cv_score + se_cv_score
                    ),
                    width = 0.1, alpha = 0.5
                )
            }

            print(p)
            return(TRUE)
        },

        .plotSurvival = function(image, ...) {
            if (is.null(private$plsResults)) return(FALSE)

            time_var <- private$plsResults$time_var
            status_var <- private$plsResults$status_var
            risk_categories <- private$plsResults$risk_categories

            # Create survival data
            risk_data <- data.frame(
                time = time_var,
                status = status_var,
                risk_group = risk_categories
            )

            # Fit survival curves by risk group
            risk_fit <- survival::survfit(
                survival::Surv(time, status) ~ risk_group,
                data = risk_data
            )

            # Use survminer if available, otherwise base plot
            if (requireNamespace("survminer", quietly = TRUE)) {
                p <- survminer::ggsurvplot(
                    risk_fit,
                    data = risk_data,
                    risk.table = FALSE,
                    pval = TRUE,
                    conf.int = self$options$confidence_intervals,
                    title = "Risk-Stratified Survival Curves",
                    subtitle = "Survival curves by PLS-based risk groups",
                    xlab = "Time",
                    ylab = "Survival Probability",
                    legend.title = "Risk Group"
                )
                print(p$plot)
            } else {
                n_groups <- length(unique(risk_categories))
                plot(risk_fit, col = seq_len(n_groups),
                     main = "Risk-Stratified Survival Curves",
                     xlab = "Time", ylab = "Survival Probability")
                legend("topright",
                       legend = paste("Risk Group", seq_len(n_groups)),
                       col = seq_len(n_groups), lty = 1)
            }

            return(TRUE)
        },

        # Data suitability assessment
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
                    detail = "Low EPV for standard modeling, but PLS handles this well through dimensionality reduction."
                )
            } else {
                checks$epv <- list(
                    color = "yellow", label = "Events-Per-Variable (Overall)",
                    value = sprintf("%.3f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = "Ultra-low EPV. Standard Cox would fail. PLS is an excellent approach here as it massively reduces dimensionality."
                )
            }

            # -- Check 2: Regularization/Reduction Need --
            if (p >= n / 3) {
                checks$regularization <- list(
                    color = "green", label = "Reduction Need",
                    value = sprintf("p=%d, n=%d (ratio=%.2f)", p, n, p / n),
                    detail = "High-dimensional setting. Dimensionality reduction via PLS is strongly indicated."
                )
            } else {
                checks$regularization <- list(
                    color = "yellow", label = "Reduction Need",
                    value = sprintf("p=%d, EPV=%.0f", p, epv),
                    detail = "Moderate/low dimensionality. Standard Cox might suffice, but PLS is still valid."
                )
            }

            # -- Check 3: Sample Size --
            if (n >= 100) {
                checks$sample_size <- list(
                    color = "green", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Adequate sample size for PLS regression cross-validation."
                )
            } else if (n >= 30) {
                checks$sample_size <- list(
                    color = "yellow", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Small sample. Consider LOO (Leave-One-Out) cross-validation instead of k-fold."
                )
            } else {
                checks$sample_size <- list(
                    color = "red", label = "Sample Size",
                    value = sprintf("n=%d", n),
                    detail = "Very small sample. Results will be highly variable and CV may not converge."
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
                    # Only calculate correlation on numeric columns to avoid failures
                    cor_matrix <- cor(pred_matrix, use = "pairwise.complete.obs")
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
                            color = "green", label = "Multicollinearity",
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = "Moderate collinearity. PLS effectively orthogonalizes these correlated predictors."
                        )
                    } else {
                        checks$collinearity <- list(
                            color = "green", label = "Multicollinearity",
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = "High collinearity. PLS is the ideal method for perfectly handling highly correlated predictors."
                        )
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
                overall_text <- "Data is well-suited for PLS Cox regression."
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

        plsResults = NULL
    )
)