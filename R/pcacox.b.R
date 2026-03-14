#' @title Principal Component Cox Models
#' @importFrom R6 R6Class
#' @import jmvcore
#'

pcacoxClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "pcacoxClass",
    inherit = pcacoxBase,
    private = list(
        
        .init = function() {
            # Package checks are handled via tryCatch in the methods that use them.
            # The required 'survival' package is listed in DESCRIPTION Imports.
        },
        
        .run = function() {
            
            # Check if variables are selected
            if (is.null(self$options$time) || is.null(self$options$status) || 
                is.null(self$options$predictors) || length(self$options$predictors) < 2) {
                
                self$results$todo$setContent(
                    "<h3>Welcome to Principal Component Cox Models</h3>
                    <p>PCA-based Cox regression addresses the challenge of high-dimensional data 
                    where the number of predictors approaches or exceeds the sample size.</p>
                    
                    <h4>When to Use PC-Cox Models:</h4>
                    <ul>
                    <li>Genomics data (gene expression, SNPs, methylation)</li>
                    <li>Proteomics and metabolomics studies</li>
                    <li>High-dimensional imaging features</li>
                    <li>Large sets of clinical variables with multicollinearity</li>
                    <li>Situations where p >> n (predictors >> samples)</li>
                    </ul>
                    
                    <h4>Method Advantages:</h4>
                    <ul>
                    <li><b>Dimensionality Reduction:</b> Transform many correlated predictors into fewer components</li>
                    <li><b>Multicollinearity Handling:</b> Principal components are orthogonal</li>
                    <li><b>Noise Reduction:</b> Focus on components explaining most variance</li>
                    <li><b>Interpretability:</b> Identify key feature patterns</li>
                    </ul>
                    
                    <h4>PCA Approaches Available:</h4>
                    <ul>
                    <li><b>Supervised PCA:</b> Uses survival information to guide component selection</li>
                    <li><b>Standard PCA:</b> Unsupervised dimensionality reduction</li>
                    <li><b>Sparse PCA:</b> Produces interpretable components with few non-zero loadings</li>
                    <li><b>Kernel PCA:</b> Captures non-linear relationships</li>
                    </ul>
                    
                    <p>Please select survival time, event status, and high-dimensional predictors (minimum 2 variables) to begin analysis.</p>"
                )
                return()
            }
            
            # Get data and variables
            data <- self$data
            time_var <- self$options$time
            status_var <- self$options$status
            predictors <- self$options$predictors
            clinical_vars <- self$options$clinical_vars
            
            # Clean data
            analysis_vars <- c(time_var, status_var, predictors, clinical_vars)
            analysis_vars <- analysis_vars[!is.null(analysis_vars)]
            clean_data <- data[complete.cases(data[, analysis_vars]), analysis_vars]

            # Two-level outcome encoding: event level -> 1, censored level -> 0, else -> NA (excluded)
            event_data <- clean_data[[status_var]]
            event_level <- as.character(self$options$outcomeLevel)
            censor_level <- as.character(self$options$censorLevel)

            event_numeric <- rep(NA_real_, length(event_data))
            event_chr <- if (is.factor(event_data)) as.character(event_data) else as.character(event_data)
            event_numeric[event_chr == event_level] <- 1
            event_numeric[event_chr == censor_level] <- 0

            n_excluded_outcome <- sum(is.na(event_numeric) & !is.na(event_data))
            if (n_excluded_outcome > 0) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-warning'><strong>Note:</strong> ",
                    n_excluded_outcome, " row(s) excluded because outcome value matched neither ",
                    "the event level ('", event_level, "') nor the censored level ('", censor_level, "').</div>"
                ))
            }

            # Replace the status column with numeric 0/1 and drop unmatched rows
            clean_data[[status_var]] <- event_numeric
            clean_data <- clean_data[!is.na(clean_data[[status_var]]), ]

            if (nrow(clean_data) < 20) {
                stop("Insufficient data for PCA-Cox analysis (minimum 20 complete cases required)")
            }

            if (length(predictors) > nrow(clean_data)) {
                message("High-dimensional setting detected: p (", length(predictors), ") > n (", nrow(clean_data), ")")
            }

            # Store for use in other methods
            private$clean_data <- clean_data
            private$time_var <- time_var
            private$status_var <- status_var
            private$predictors <- predictors
            private$clinical_vars <- clinical_vars
            
            # Assess data suitability if requested
            if (self$options$suitabilityCheck) {
                private$.assessSuitability()
            }
            
            # Perform PCA analysis
            private$.performPCA()
            
            # Additional analyses if model is available
            if (!is.null(private$cox_model)) {
                
                # Generate summary
                private$.generateSummary()
                
                # Calculate additional analyses if requested
                if (self$options$feature_importance) {
                    private$.calculateFeatureImportance()
                }
                
                if (self$options$risk_score) {
                    private$.calculateRiskScore()
                }
                
                # Prepare plots
                if (self$options$plot_scree) {
                    private$.prepareScreePlot()
                }
                
                if (self$options$plot_loadings) {
                    private$.prepareLoadingsPlot()
                }
                
                if (self$options$plot_biplot) {
                    private$.prepareBiplot()
                }
                
                if (self$options$plot_survival) {
                    # Survival plot requires risk scores; compute them if not already done
                    if (is.null(private$risk_scores)) {
                        private$.calculateRiskScore()
                    }
                    private$.prepareSurvivalPlot()
                }

                # Validation analyses
                if (self$options$bootstrap_validation) {
                    private$.performBootstrapValidation()
                }

                if (self$options$permutation_test) {
                    private$.performPermutationTest()
                }
                
                if (self$options$show_model_comparison) {
                    private$.populateModelComparison()
                }
            }

            # Pathway analysis (not yet implemented)
            if (isTRUE(self$options$pathway_analysis)) {
                self$results$pathwayAnalysis$setContent(
                    "<h4>Pathway Enrichment Analysis</h4>
                    <p><b>Status:</b> This feature is not yet implemented in the current version.</p>
                    <p>Pathway analysis requires external gene set databases (e.g., MSigDB, KEGG, Reactome)
                    and is planned for a future release. In the meantime, you can export the feature importance
                    rankings from this analysis and use dedicated pathway analysis tools such as:</p>
                    <ul>
                    <li><b>GSEA</b> (Gene Set Enrichment Analysis)</li>
                    <li><b>clusterProfiler</b> R package</li>
                    <li><b>Enrichr</b> web tool</li>
                    </ul>"
                )
            }
        },
        
        .performPCA = function() {
            
            tryCatch({
                
                # Extract predictor matrix using model.matrix to handle factors
                formula_str <- paste("~ -1 +", paste(private$predictors, collapse = " + "))
                X <- model.matrix(as.formula(formula_str), data = private$clean_data)
                
                # Handle missing values
                if (any(is.na(X))) {
                    complete_idx <- complete.cases(X)
                    X <- X[complete_idx, , drop = FALSE]
                    private$clean_data <- private$clean_data[complete_idx, , drop = FALSE]
                }
                
                # Scale and center if requested
                if (self$options$scaling || self$options$centering) {
                    X <- scale(X, 
                              center = self$options$centering %||% TRUE,
                              scale = self$options$scaling %||% TRUE)
                }
                
                # Store processed matrix
                private$X_matrix <- X
                
                # Choose PCA method
                pca_method <- self$options$pca_method %||% "supervised"
                
                if (pca_method == "supervised") {
                    private$.performSupervisedPCA()
                } else if (pca_method == "sparse") {
                    private$.performSparsePCA()
                } else if (pca_method == "kernel") {
                    private$.performKernelPCA()
                } else {
                    private$.performStandardPCA()
                }
                
                # Select optimal number of components
                private$.selectComponents()
                
                # Populate PCA summary table
                private$.populatePCASummary()
                
                # Fit Cox model on principal components
                private$.fitPCCoxModel()
                
            }, error = function(e) {
                self$results$todo$setContent(
                    paste0("<html><body><div style='color: red;'>",
                           "<b>PCA analysis failed:</b> ", htmltools::htmlEscape(e$message), 
                           "</div></body></html>")
                )
            })
        },
        
        .assessSuitability = function() {
            data <- self$data
            time_var <- self$options$time
            status_var <- self$options$status
            predictors <- self$options$predictors

            html <- self$results$suitabilityReport
            
            if (is.null(html)) return()
            
            # 1. Sample Size & Events
            total_n <- nrow(data)
            events <- 0
            if (!is.null(status_var) && status_var %in% names(data)) {
                # Use two-level encoding consistent with .run()
                event_level <- as.character(self$options$outcomeLevel)
                raw_status <- if (is.factor(data[[status_var]])) as.character(data[[status_var]]) else as.character(data[[status_var]])
                events <- sum(raw_status == event_level, na.rm = TRUE)
            }
            n_vars <- length(predictors)
            epv <- ifelse(n_vars > 0, events / n_vars, 0)

            # Define warning colors
            epv_color <- ifelse(epv < 5, "red", ifelse(epv < 10, "orange", "green"))
            
            # 2. Missing Data
            missing_text <- ""
            complete_cases <- sum(complete.cases(data[, c(time_var, status_var, predictors), drop=FALSE]))
            missing_pct <- 100 * (1 - complete_cases/total_n)
            
            if (missing_pct > 10) {
                missing_text <- paste0("<div style='color: orange; margin-top: 5px;'>",
                                     "⚠️ <b>Note:</b> ", round(missing_pct, 1), "% of cases have missing data and will be excluded.",
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
                    <div class='suit-title'>Dataset Assessment for PCA Cox</div>
                    
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
                               "<span style='color: red;'>⚠️ <b>High Dimensionality:</b> EPV is very low. Regularization via dimensionality reduction (PCA) is strictly necessary, but results may still be highly unstable.</span>", 
                               ifelse(epv < 10, 
                                      "<span style='color: orange;'>⚠️ <b>Moderate Dimensionality:</b> PCA will effectively perform dimension reduction and manage multicollinearity.</span>",
                                      "<span style='color: green;'>✓ <b>Adequate Events:</b> Sample size is sufficient for this number of predictors. PCA will focus on identifying feature patterns.</span>"
                               )),
                    "</div>",
                    missing_text,
                "</div></html>"
            )
            
            html$setContent(content)
        },
        
        .performStandardPCA = function() {
            
            # Standard PCA using prcomp
            pca_result <- prcomp(private$X_matrix, center = FALSE, scale. = FALSE)
            
            # Store results
            private$pca_result <- pca_result
            private$pc_scores <- pca_result$x
            private$loadings <- pca_result$rotation
            private$eigenvalues <- pca_result$sdev^2
            private$prop_variance <- private$eigenvalues / sum(private$eigenvalues)
            private$cumul_variance <- cumsum(private$prop_variance)
        },
        
        .performSupervisedPCA = function() {
            
            tryCatch({
                
                # Check if superpc is available
                if (!requireNamespace('superpc', quietly = TRUE)) {
                    message("superpc not available, falling back to standard PCA")
                    private$.performStandardPCA()
                    return()
                }
                
                # Prepare data for superpc
                y <- private$clean_data[[private$status_var]]
                censoring.status <- private$clean_data[[private$status_var]]
                
                # Create superpc data structure
                data_superpc <- list(
                    x = t(private$X_matrix),  # superpc expects genes x samples
                    y = private$clean_data[[private$time_var]],
                    censoring.status = censoring.status,
                    featurenames = colnames(private$X_matrix)
                )
                
                # Train supervised PC
                train_result <- superpc::superpc.train(
                    data = data_superpc,
                    type = "survival"
                )
                
                # Get predictions for different numbers of components
                n_components <- min(self$options$n_components %||% 5, ncol(private$X_matrix), nrow(private$X_matrix))
                
                # Extract supervised PC scores
                pred_result <- superpc::superpc.predict(
                    train_result,
                    data_superpc,
                    data_superpc,
                    threshold = 0.1,
                    n.components = n_components
                )
                
                # Store results (simplified for demonstration)
                private$pca_result <- train_result
                private$pc_scores <- matrix(pred_result$v.pred, ncol = n_components)
                colnames(private$pc_scores) <- paste0("PC", 1:n_components)
                
                # Approximate loadings and variance (superpc doesn't directly provide these)
                private$.approximatePCAMetrics()
                
            }, error = function(e) {
                message("Supervised PCA failed, using standard PCA: ", e$message)
                private$.performStandardPCA()
            })
        },
        
        .performSparsePCA = function() {
            
            if (requireNamespace("sparsepca", quietly = TRUE)) {
                tryCatch({
                    # Determine k (number of components)
                    n_components <- self$options$n_components %||% 5
                    n_components <- min(n_components, ncol(private$X_matrix), nrow(private$X_matrix))
                    
                    # Run Sparse PCA
                    # Note: sparsepca expects centered data usually, X_matrix is already scaled/centered if requested
                    sparse_param <- self$options$sparse_parameter %||% 0.1
                    spca_res <- sparsepca::spca(private$X_matrix, k = n_components, alpha = sparse_param, beta = sparse_param)
                    
                    private$pca_result <- spca_res
                    
                    # Store scores
                    private$pc_scores <- spca_res$transform
                    colnames(private$pc_scores) <- paste0("PC", 1:ncol(private$pc_scores))
                    
                    # Store loadings
                    private$loadings <- spca_res$loadings
                    rownames(private$loadings) <- colnames(private$X_matrix)
                    colnames(private$loadings) <- paste0("PC", 1:ncol(private$loadings))
                    
                    # Store eigenvalues
                    private$eigenvalues <- spca_res$eigenvalues
                    
                    # Calculate variance
                    total_var <- sum(private$eigenvalues) # Approximation for sparse
                    if (total_var > 0) {
                        private$prop_variance <- private$eigenvalues / total_var
                        private$cumul_variance <- cumsum(private$prop_variance)
                    } else {
                        private$.approximatePCAMetrics()
                    }
                    
                }, error = function(e) {
                    message("Sparse PCA failed: ", e$message, ". Falling back to standard PCA.")
                    private$.performStandardPCA()
                })
            } else {
                message("sparsepca package not available, using standard PCA")
                private$.performStandardPCA()
            }
        },
        
        .performKernelPCA = function() {
            
            if (requireNamespace("kernlab", quietly = TRUE)) {
                tryCatch({
                    # Determine features
                    n_components <- self$options$n_components %||% 5
                    n_components <- min(n_components, nrow(private$X_matrix)) # kpca features limit
                    
                    # Run Kernel PCA (Radial Basis Function kernel default)
                    kpca_res <- kernlab::kpca(private$X_matrix, kernel = "rbfdot", features = n_components)
                    
                    private$pca_result <- kpca_res
                    
                    # Store scores (rotated data)
                    private$pc_scores <- kernlab::rotated(kpca_res)
                    colnames(private$pc_scores) <- paste0("PC", 1:ncol(private$pc_scores))
                    
                    # Store eigenvalues
                    private$eigenvalues <- kernlab::eig(kpca_res)
                    
                    # Calculate variance (approximate for Kernel PCA)
                    total_var <- sum(private$eigenvalues)
                    if (total_var > 0) {
                        private$prop_variance <- private$eigenvalues / total_var
                        private$cumul_variance <- cumsum(private$prop_variance)
                    }
                    
                    # Loadings are not directly available in Kernel PCA (feature space).
                    # Use correlation loadings approximation
                    private$.approximatePCAMetrics()
                    
                }, error = function(e) {
                    message("Kernel PCA failed: ", e$message, ". Falling back to standard PCA.")
                    private$.performStandardPCA()
                })
            } else {
                message("kernlab package not available, using standard PCA")
                private$.performStandardPCA()
            }
        },
        
        .approximatePCAMetrics = function() {
            
            # Calculate actual variance of the components
            if (!is.null(private$pc_scores)) {
                # Eigenvalues approx as variance of scores
                component_variances <- apply(private$pc_scores, 2, var, na.rm = TRUE)
                private$eigenvalues <- component_variances
                
                # Calculate variance explained
                # Total variance of standardized data is number of variables (if scaled)
                total_variance <- if (self$options$scaling) ncol(private$X_matrix) else sum(apply(private$X_matrix, 2, var, na.rm = TRUE))
                
                private$prop_variance <- private$eigenvalues / total_variance
                private$cumul_variance <- cumsum(private$prop_variance)
            }
            
            # Calculate loadings as correlation between variables and components
            if (is.null(private$loadings) && !is.null(private$X_matrix) && !is.null(private$pc_scores)) {
                # Compute correlations (structure matrix)
                # This represents the correlation between original vars and PCs
                private$loadings <- cor(private$X_matrix, private$pc_scores, use = "pairwise.complete.obs")
                
                # If correlations fail (e.g. constant variables), replace NAs with 0
                private$loadings[is.na(private$loadings)] <- 0
            }
        },
        
        .selectComponents = function() {
            
            selection_method <- self$options$component_selection %||% "fixed"
            n_components_requested <- self$options$n_components %||% 5
            
            if (selection_method == "fixed") {
                private$selected_components <- min(n_components_requested, 
                                                 ncol(private$pc_scores), 
                                                 nrow(private$clean_data) - 10)
                
            } else if (selection_method == "variance") {
                variance_threshold <- self$options$variance_threshold %||% 0.8
                private$selected_components <- which(private$cumul_variance >= variance_threshold)[1]
                private$selected_components <- max(1, min(private$selected_components, n_components_requested))
                
            } else if (selection_method == "cv") {
                # Cross-validation selection (simplified)
                private$selected_components <- private$.performCVSelection()
                
            } else {
                private$selected_components <- n_components_requested
            }
            
            # Ensure reasonable bounds
            private$selected_components <- max(1, min(private$selected_components, 
                                                    ncol(private$pc_scores),
                                                    nrow(private$clean_data) - 5))
        },
        
        .performCVSelection = function() {
            
            tryCatch({
                
                cv_folds <- self$options$cv_folds %||% 10
                max_components <- min(10, ncol(private$pc_scores), nrow(private$clean_data) - 10)
                
                cv_scores <- numeric(max_components)
                
                # Simple CV implementation
                for (k in 1:max_components) {
                    
                    # Create folds
                    n <- nrow(private$clean_data)
                    fold_id <- sample(rep(1:cv_folds, length.out = n))
                    
                    fold_scores <- numeric(cv_folds)
                    
                    for (fold in 1:cv_folds) {
                        
                        train_idx <- which(fold_id != fold)
                        test_idx <- which(fold_id == fold)
                        
                        if (length(test_idx) < 5) next
                        
                        # Fit Cox model on training data
                        train_data <- private$clean_data[train_idx, ]
                        train_pcs <- private$pc_scores[train_idx, 1:k, drop = FALSE]
                        
                        cox_data <- data.frame(
                            time = train_data[[private$time_var]],
                            status = train_data[[private$status_var]],
                            train_pcs
                        )
                        
                        cox_formula <- as.formula(paste("Surv(time, status) ~", 
                                                      paste(colnames(train_pcs), collapse = " + ")))
                        
                        cox_model <- survival::coxph(cox_formula, data = cox_data)
                        
                        # Predict on test data
                        test_pcs <- private$pc_scores[test_idx, 1:k, drop = FALSE]
                        test_pred <- predict(cox_model, newdata = as.data.frame(test_pcs))
                        
                        # Calculate C-index using correct Surv ~ predictor formula and handling potential zero-variance predicting scores in small folds.
                        test_time <- private$clean_data[[private$time_var]][test_idx]
                        test_status <- private$clean_data[[private$status_var]][test_idx]

                        tryCatch({
                            c_obj <- survival::concordance(
                                survival::Surv(test_time, test_status) ~ test_pred, 
                                reverse = TRUE
                            )
                            fold_scores[fold] <- c_obj$concordance
                        }, error = function(e) {
                            fold_scores[fold] <- 0.500
                        })
                    }
                    
                    cv_scores[k] <- mean(fold_scores, na.rm = TRUE)
                }
                
                # Select number of components with highest CV score
                optimal_k <- which.max(cv_scores)
                
                # Store CV results for reporting
                private$cv_scores <- cv_scores
                
                return(optimal_k)
                
            }, error = function(e) {
                message("CV selection failed, using fixed number: ", e$message)
                return(min(5, ncol(private$pc_scores)))
            })
        },
        
        .populatePCASummary = function() {
            
            table <- self$results$pcaSummary
            
            n_show <- min(private$selected_components + 2, length(private$eigenvalues))
            
            for (i in 1:n_show) {
                
                is_selected <- if (i <= private$selected_components) "Yes" else "No"
                
                table$addRow(rowKey = i, values = list(
                    component = paste0("PC", i),
                    eigenvalue = round(private$eigenvalues[i], 3),
                    prop_variance = round(private$prop_variance[i], 3),
                    cumul_variance = round(private$cumul_variance[i], 3),
                    selected = is_selected
                ))
            }
        },
        
        .fitPCCoxModel = function() {
            
            tryCatch({
                
                # Prepare data for Cox regression
                selected_pcs <- private$pc_scores[, 1:private$selected_components, drop = FALSE]
                
                cox_data <- data.frame(
                    time = private$clean_data[[private$time_var]],
                    status = private$clean_data[[private$status_var]],
                    selected_pcs
                )
                
                # Add clinical variables if provided
                if (length(private$clinical_vars) > 0) {
                    clinical_data <- private$clean_data[, private$clinical_vars, drop = FALSE]
                    cox_data <- cbind(cox_data, clinical_data)
                }
                
                # Create formula
                pc_vars <- colnames(selected_pcs)
                all_vars <- c(pc_vars, private$clinical_vars)
                
                cox_formula <- as.formula(paste("Surv(time, status) ~", 
                                              paste(all_vars, collapse = " + ")))
                
                # Fit Cox model
                cox_model <- survival::coxph(cox_formula, data = cox_data)
                
                # Store model and data
                private$cox_model <- cox_model
                private$cox_data <- cox_data
                
                # Populate results table
                private$.formatCoxResults()
                
                # Calculate model performance
                private$.calculateModelPerformance()
                
            }, error = function(e) {
                self$results$errors$setContent(
                    paste("Cox model fitting failed:", e$message)
                )
            })
        },
        
        .formatCoxResults = function() {
            
            if (is.null(private$cox_model)) return()
            
            table <- self$results$coxResults
            coef_summary <- summary(private$cox_model)$coefficients
            conf_level <- self$options$confidence_level %||% 0.95
            confint_result <- confint(private$cox_model, level = conf_level)
            
            for (i in 1:nrow(coef_summary)) {
                
                var_name <- rownames(coef_summary)[i]
                coef_val <- coef_summary[i, "coef"]
                se_val <- coef_summary[i, "se(coef)"]
                z_val <- coef_summary[i, "z"]
                p_val <- coef_summary[i, "Pr(>|z|)"]
                hr_val <- exp(coef_val)
                
                # Get confidence intervals
                ci_lower <- exp(confint_result[i, 1])
                ci_upper <- exp(confint_result[i, 2])
                
                table$addRow(rowKey = i, values = list(
                    variable = var_name,
                    coefficient = round(coef_val, 4),
                    se = round(se_val, 4),
                    z_value = round(z_val, 3),
                    p_value = round(p_val, 4),
                    hr = round(hr_val, 3),
                    hr_lower = round(ci_lower, 3),
                    hr_upper = round(ci_upper, 3)
                ))
            }
        },
        
        .calculateModelPerformance = function() {
            
            if (is.null(private$cox_model)) return()
            
            tryCatch({
                
                table <- self$results$modelPerformance
                
                # Calculate C-index
                c_index <- survival::concordance(private$cox_model)
                c_stat <- c_index$concordance
                c_se <- sqrt(c_index$var)
                
                # R-squared (Nagelkerke)
                loglik <- private$cox_model$loglik
                r_squared <- 1 - exp((loglik[1] - loglik[2]) * 2 / nrow(private$cox_data))
                
                # AIC
                aic_val <- AIC(private$cox_model)
                
                # Confidence Multiplier
                conf_level <- self$options$confidence_level %||% 0.95
                z_mult <- qnorm(1 - (1 - conf_level)/2)
                
                # Add metrics to table
                metrics <- list(
                    list("Training Concordance Index*", c_stat, c_se, c_stat - z_mult*c_se, c_stat + z_mult*c_se),
                    list("R-squared", r_squared, NA, NA, NA),
                    list("AIC", aic_val, NA, NA, NA),
                    list("Log-likelihood", loglik[2], NA, NA, NA)
                )
                
                for (i in seq_along(metrics)) {
                    table$addRow(rowKey = i, values = list(
                        metric = metrics[[i]][[1]],
                        value = round(metrics[[i]][[2]], 4),
                        se = ifelse(is.na(metrics[[i]][[3]]), "", round(metrics[[i]][[3]], 4)),
                        ci_lower = ifelse(is.na(metrics[[i]][[4]]), "", round(metrics[[i]][[4]], 4)),
                        ci_upper = ifelse(is.na(metrics[[i]][[5]]), "", round(metrics[[i]][[5]], 4))
                    ))
                }
                
                table$setNote("cindex", "* Note: The Training Concordance Index overestimates true out-of-sample performance, as it is evaluated on the data used for variable selection. See Bootstrap Validation for pessimism-corrected estimates.")
                
            }, error = function(e) {
                message("Performance calculation failed: ", e$message)
            })
        },
        
        .calculateFeatureImportance = function() {
            
            tryCatch({
                
                if (is.null(private$loadings)) return()
                
                table <- self$results$featureImportance
                
                # Calculate importance scores based on loadings and component coefficients
                cox_coef <- coef(private$cox_model)
                pc_coef <- cox_coef[grepl("PC", names(cox_coef))]
                
                # Calculate weighted loadings
                importance_scores <- numeric(length(private$predictors))
                names(importance_scores) <- private$predictors
                
                for (i in 1:private$selected_components) {
                    pc_name <- paste0("PC", i)
                    if (pc_name %in% names(pc_coef)) {
                        loadings_i <- private$loadings[, i]
                        importance_scores <- importance_scores + abs(pc_coef[pc_name] * loadings_i)
                    }
                }
                
                # Rank features
                feature_ranks <- rank(-importance_scores)
                
                # Show top features
                top_features <- order(importance_scores, decreasing = TRUE)[1:min(20, length(importance_scores))]
                
                for (idx in top_features) {
                    feature_name <- private$predictors[idx]
                    importance <- importance_scores[idx]
                    rank_val <- feature_ranks[idx]
                    
                    # Find main contributing PCs
                    pc_contributions <- abs(private$loadings[idx, 1:private$selected_components])
                    main_pcs <- paste(paste0("PC", which(pc_contributions > 0.1)), collapse = ", ")
                    
                    table$addRow(rowKey = idx, values = list(
                        feature = feature_name,
                        importance_score = round(importance, 4),
                        rank = rank_val,
                        contributing_pcs = main_pcs
                    ))
                }
                
            }, error = function(e) {
                message("Feature importance calculation failed: ", e$message)
            })
        },
        
        .calculateRiskScore = function() {
            
            tryCatch({
                
                if (is.null(private$cox_model)) return()
                
                # Calculate risk scores
                risk_scores <- predict(private$cox_model, type = "lp")
                
                # Divide into risk groups (tertiles)
                tertiles <- quantile(risk_scores, c(1/3, 2/3), na.rm = TRUE)
                # Ensure cut breaks are strictly unique
                if (tertiles[1] == tertiles[2] || min(risk_scores, na.rm=T) == tertiles[1] || max(risk_scores, na.rm=T) == tertiles[2]) {
                    # Fallback if there are flat distributions preventing pure tertiles
                    unique_vals <- unique(risk_scores)
                    if(length(unique_vals) <= 3) {
                         risk_groups <- factor(risk_scores, labels = paste("Level", seq_along(unique_vals)))
                    } else {
                        tertiles <- attr(suppressWarnings(Hmisc::wtd.quantile(risk_scores, probs=c(1/3, 2/3), type="quantile", na.rm=TRUE)), "names")
                        tertiles <- quantile(jitter(risk_scores), c(1/3, 2/3), na.rm = TRUE)
                        risk_groups <- cut(risk_scores, 
                                         breaks = c(-Inf, tertiles, Inf),
                                         labels = c("Low", "Medium", "High"))
                    }
                } else {
                    risk_groups <- cut(risk_scores, 
                                     breaks = c(-Inf, tertiles, Inf),
                                     labels = c("Low", "Medium", "High"))
                }
                
                # Calculate group statistics
                table <- self$results$riskScore
                
                for (group in c("Low", "Medium", "High")) {

                    group_idx <- which(risk_groups == group)
                    n_patients <- length(group_idx)

                    group_time <- private$clean_data[[private$time_var]][group_idx]
                    group_status <- private$clean_data[[private$status_var]][group_idx]

                    n_events <- sum(group_status)
                    median_survival <- median(group_time[group_status == 1], na.rm = TRUE)

                    # Calculate HR vs low risk group
                    if (group == "Low") {
                        hr_vs_low <- 1.0
                        p_value <- NA
                    } else {
                        tryCatch({
                            # Subset to Low + current group for pairwise HR
                            compare_idx <- which(risk_groups %in% c("Low", group))
                            compare_factor <- factor(
                                as.character(risk_groups[compare_idx]),
                                levels = c("Low", group)
                            )
                            cox_temp <- survival::coxph(
                                survival::Surv(
                                    private$clean_data[[private$time_var]][compare_idx],
                                    private$clean_data[[private$status_var]][compare_idx]
                                ) ~ compare_factor
                            )
                            hr_vs_low <- exp(coef(cox_temp)[1])
                            p_value <- summary(cox_temp)$coefficients[1, "Pr(>|z|)"]
                        }, error = function(e) {
                            hr_vs_low <<- NA
                            p_value <<- NA
                        })
                    }

                    table$addRow(rowKey = group, values = list(
                        risk_group = group,
                        n_patients = n_patients,
                        n_events = as.integer(n_events),
                        median_survival = ifelse(is.na(median_survival), "", round(median_survival, 2)),
                        hr_vs_low = ifelse(is.na(hr_vs_low), "", round(hr_vs_low, 3)),
                        p_value = ifelse(is.na(p_value), "", round(p_value, 4))
                    ))
                }
                
                # Store risk scores for plotting
                private$risk_scores <- risk_scores
                private$risk_groups <- risk_groups
                
            }, error = function(e) {
                message("Risk score calculation failed: ", e$message)
            })
        },
        
        .generateSummary = function() {
            
            n_subjects <- nrow(private$clean_data)
            n_predictors <- length(private$predictors)
            n_events <- sum(private$clean_data[[private$status_var]])
            
            pca_method <- self$options$pca_method %||% "supervised"
            n_components <- private$selected_components
            
            summary_html <- paste0(
                "<h3>Principal Component Cox Model Summary</h3>",
                "<p><b>Data Characteristics:</b></p>",
                "<ul>",
                "<li><b>Sample Size:</b> ", n_subjects, " subjects</li>",
                "<li><b>Events:</b> ", n_events, " (", round(n_events/n_subjects*100, 1), "%)</li>",
                "<li><b>Predictors:</b> ", n_predictors, " variables</li>",
                "<li><b>Dimensionality:</b> ", ifelse(n_predictors > n_subjects, "High (p > n)", "Standard"), "</li>",
                "</ul>",
                
                "<p><b>PCA Configuration:</b></p>",
                "<ul>",
                "<li><b>Method:</b> ", stringr::str_to_title(gsub("_", " ", pca_method)), "</li>",
                "<li><b>Components Selected:</b> ", n_components, "</li>",
                "<li><b>Variance Explained:</b> ", round(private$cumul_variance[n_components] * 100, 1), "%</li>",
                "<li><b>Selection Method:</b> ", stringr::str_to_title(gsub("_", " ", self$options$component_selection)), "</li>",
                "</ul>"
            )
            
            if (!is.null(private$cox_model)) {
                c_index <- survival::concordance(private$cox_model)$concordance
                summary_html <- paste0(summary_html,
                    "<p><b>Model Performance:</b></p>",
                    "<ul>",
                    "<li><b>C-Index:</b> ", round(c_index, 3), "</li>",
                    "<li><b>AIC:</b> ", round(AIC(private$cox_model), 2), "</li>",
                    "</ul>",
                    
                    "<p><b>Clinical Applications:</b></p>",
                    "<ul>",
                    "<li>Genomic risk stratification</li>",
                    "<li>Biomarker signature development</li>",
                    "<li>Personalized survival prediction</li>",
                    "<li>Feature selection for further analysis</li>",
                    "</ul>"
                )
            }
            
            self$results$summary$setContent(summary_html)
        },
        
        .prepareScreePlot = function() {
            image <- self$results$screePlot
            # Extract only plain numeric data for protobuf-safe serialization
            image$setState(list(
                eigenvalues = as.numeric(private$eigenvalues),
                selected = as.integer(private$selected_components)
            ))
        },

        .prepareLoadingsPlot = function() {
            image <- self$results$loadingsPlot
            image$setState(list(
                loadings = as.data.frame(private$loadings),
                selected = as.integer(private$selected_components)
            ))
        },

        .prepareBiplot = function() {
            image <- self$results$biplot
            image$setState(list(
                pc_scores = as.data.frame(private$pc_scores),
                loadings = as.data.frame(private$loadings)
            ))
        },

        .prepareSurvivalPlot = function() {
            if (is.null(private$risk_groups) || is.null(private$clean_data)) return()

            image <- self$results$survivalPlot
            # Store actual data columns (not variable name strings) for protobuf-safe serialization
            image$setState(as.data.frame(list(
                risk_scores = as.numeric(private$risk_scores),
                risk_groups = as.character(private$risk_groups),
                time = as.numeric(private$clean_data[[private$time_var]]),
                status = as.integer(private$clean_data[[private$status_var]])
            )))
        },
        
        .performBootstrapValidation = function() {

            tryCatch({

                if (is.null(private$cox_model) || is.null(private$cox_data)) {
                    self$results$bootstrapValidation$setContent(
                        "<h4>Bootstrap Validation</h4><p>No fitted model available for validation.</p>"
                    )
                    return()
                }

                n_bootstrap <- self$options$n_bootstrap %||% 100
                n <- nrow(private$cox_data)

                # Harrell's optimism-corrected bootstrap
                # Apparent C-index from the original model
                apparent_c <- survival::concordance(private$cox_model)$concordance

                optimism_vals <- numeric(n_bootstrap)
                cal_slopes <- numeric(n_bootstrap)

                for (b in seq_len(n_bootstrap)) {
                    tryCatch({
                        # Draw bootstrap sample
                        boot_idx <- sample(n, n, replace = TRUE)
                        boot_data <- private$cox_data[boot_idx, ]

                        # Fit model on bootstrap sample
                        boot_model <- survival::coxph(
                            formula(private$cox_model),
                            data = boot_data
                        )

                        # C-index on bootstrap sample (training performance)
                        c_boot <- survival::concordance(boot_model)$concordance

                        # Predict on original data using bootstrap model
                        lp_orig <- predict(boot_model, newdata = private$cox_data, type = "lp")
                        c_orig <- survival::concordance(
                            survival::Surv(private$cox_data$time, private$cox_data$status) ~ lp_orig
                        )$concordance

                        # Optimism = boot performance - original data performance
                        optimism_vals[b] <- c_boot - c_orig

                        # Calibration slope: regress original outcome on bootstrap LP
                        cal_model <- survival::coxph(
                            survival::Surv(time, status) ~ lp_orig,
                            data = private$cox_data
                        )
                        cal_slopes[b] <- coef(cal_model)

                    }, error = function(e) {
                        optimism_vals[b] <<- NA
                        cal_slopes[b] <<- NA
                    })
                }

                # Compute summaries
                mean_optimism <- mean(optimism_vals, na.rm = TRUE)
                corrected_c <- apparent_c - mean_optimism
                mean_cal_slope <- mean(cal_slopes, na.rm = TRUE)
                n_successful <- sum(!is.na(optimism_vals))

                bootstrap_html <- paste0(
                    "<h4>Bootstrap Validation Results</h4>",
                    "<p>Harrell's optimism-corrected bootstrap validation (", n_successful,
                    " of ", n_bootstrap, " samples completed):</p>",
                    "<table border='1' cellpadding='4' cellspacing='0' style='border-collapse:collapse;'>",
                    "<tr><th>Metric</th><th>Value</th></tr>",
                    "<tr><td>Apparent C-index</td><td>", round(apparent_c, 4), "</td></tr>",
                    "<tr><td>Mean optimism</td><td>", round(mean_optimism, 4), "</td></tr>",
                    "<tr><td><b>Optimism-corrected C-index</b></td><td><b>", round(corrected_c, 4), "</b></td></tr>",
                    "<tr><td>Mean calibration slope</td><td>", round(mean_cal_slope, 4), "</td></tr>",
                    "</table>",
                    "<p><b>Interpretation:</b></p>",
                    "<ul>",
                    "<li>Optimism-corrected C-index removes overfitting bias. Values above 0.6 suggest useful discrimination.</li>",
                    "<li>Calibration slope near 1.0 indicates good calibration; values less than 1.0 suggest overfitting.</li>",
                    "</ul>"
                )

                self$results$bootstrapValidation$setContent(bootstrap_html)

            }, error = function(e) {
                self$results$bootstrapValidation$setContent(
                    paste0("<h4>Bootstrap Validation</h4>",
                           "<p>Bootstrap validation could not be completed: ", htmltools::htmlEscape(e$message), "</p>")
                )
            })
        },
        
        .performPermutationTest = function() {

            tryCatch({

                if (is.null(private$cox_model) || is.null(private$cox_data)) {
                    self$results$permutationTest$setContent(
                        "<h4>Permutation Test</h4><p>No fitted model available for permutation testing.</p>"
                    )
                    return()
                }

                n_permutations <- self$options$n_permutations %||% 100

                # Observed model log-likelihood ratio statistic
                observed_llr <- 2 * diff(private$cox_model$loglik)

                # Per-component observed z-statistics
                cox_summary <- summary(private$cox_model)$coefficients
                pc_rows <- grep("^PC", rownames(cox_summary))
                observed_z <- abs(cox_summary[pc_rows, "z"])
                pc_names <- rownames(cox_summary)[pc_rows]

                # Permutation distribution: shuffle survival outcomes
                perm_llr <- numeric(n_permutations)
                perm_z_matrix <- matrix(NA, nrow = n_permutations, ncol = length(pc_rows))

                for (b in seq_len(n_permutations)) {
                    tryCatch({
                        perm_data <- private$cox_data
                        perm_idx <- sample(nrow(perm_data))
                        perm_data$time <- private$cox_data$time[perm_idx]
                        perm_data$status <- private$cox_data$status[perm_idx]

                        perm_model <- survival::coxph(
                            formula(private$cox_model),
                            data = perm_data
                        )

                        perm_llr[b] <- 2 * diff(perm_model$loglik)

                        perm_coef <- summary(perm_model)$coefficients
                        perm_pc_rows <- grep("^PC", rownames(perm_coef))
                        if (length(perm_pc_rows) == length(pc_rows)) {
                            perm_z_matrix[b, ] <- abs(perm_coef[perm_pc_rows, "z"])
                        }
                    }, error = function(e) {
                        perm_llr[b] <<- NA
                    })
                }

                # Calculate p-values
                n_valid <- sum(!is.na(perm_llr))
                overall_p <- mean(perm_llr >= observed_llr, na.rm = TRUE)

                # Per-component p-values
                pc_p_values <- sapply(seq_along(pc_rows), function(j) {
                    perm_z_col <- perm_z_matrix[, j]
                    mean(perm_z_col >= observed_z[j], na.rm = TRUE)
                })

                # Build results table
                pc_table_rows <- paste0(
                    sapply(seq_along(pc_names), function(j) {
                        paste0("<tr><td>", pc_names[j], "</td>",
                               "<td>", round(observed_z[j], 3), "</td>",
                               "<td>", round(pc_p_values[j], 4), "</td>",
                               "<td>", ifelse(pc_p_values[j] < 0.05, "Significant", "Not significant"), "</td></tr>")
                    }),
                    collapse = ""
                )

                permutation_html <- paste0(
                    "<h4>Permutation Test Results</h4>",
                    "<p>Significance testing with ", n_valid, " of ", n_permutations,
                    " permutations completed:</p>",
                    "<p><b>Overall model p-value (permutation):</b> ", round(overall_p, 4),
                    ifelse(overall_p < 0.05,
                           " (significant: model captures genuine survival signal)",
                           " (not significant: model may reflect noise)"),
                    "</p>",
                    "<table border='1' cellpadding='4' cellspacing='0' style='border-collapse:collapse;'>",
                    "<tr><th>Component</th><th>Observed |z|</th><th>Perm. p-value</th><th>Conclusion</th></tr>",
                    pc_table_rows,
                    "</table>",
                    "<p><b>Interpretation:</b> Permutation tests validate that the PCA-derived components ",
                    "contain genuine survival-relevant information. Components with p &lt; 0.05 show ",
                    "associations unlikely to arise by chance.</p>"
                )

                self$results$permutationTest$setContent(permutation_html)

            }, error = function(e) {
                self$results$permutationTest$setContent(
                    paste0("<h4>Permutation Test</h4>",
                           "<p>Permutation test could not be completed: ", htmltools::htmlEscape(e$message), "</p>")
                )
            })
        },
        
        .plotScree = function(image, ...) {
            
            if (is.null(private$eigenvalues)) return()
            
            eigenvalues <- private$eigenvalues
            n_show <- min(20, length(eigenvalues))
            
            plot(1:n_show, eigenvalues[1:n_show], 
                 type = "b", pch = 19, col = "blue",
                 main = "Scree Plot - Principal Component Eigenvalues",
                 xlab = "Principal Component", 
                 ylab = "Eigenvalue",
                 lwd = 2)
            
            # Add variance explained
            prop_var <- private$prop_variance[1:n_show] * 100
            text(1:n_show, eigenvalues[1:n_show], 
                 labels = paste0(round(prop_var, 1), "%"), 
                 pos = 3, cex = 0.8)
            
            # Highlight selected components
            selected <- private$selected_components
            if (selected <= n_show) {
                abline(v = selected, col = "red", lty = 2, lwd = 2)
                text(selected, max(eigenvalues[1:n_show]), 
                     paste("Selected:", selected), 
                     pos = 4, col = "red", cex = 1)
            }
            
            grid(col = "lightgray")
            
            TRUE
        },
        
        .plotLoadings = function(image, ...) {
            
            if (is.null(private$loadings)) return()
            
            # Plot loadings for first few components
            n_components <- min(private$selected_components, 4)
            
            par(mfrow = c(2, 2))
            
            for (i in 1:n_components) {
                loadings_i <- private$loadings[, i]
                
                # Show top loadings
                top_loadings <- sort(abs(loadings_i), decreasing = TRUE)[1:min(20, length(loadings_i))]
                top_idx <- match(top_loadings, abs(loadings_i))
                
                barplot(loadings_i[top_idx], 
                       main = paste("PC", i, "Loadings (Top Features)"),
                       las = 2, cex.names = 0.6,
                       col = ifelse(loadings_i[top_idx] > 0, "blue", "red"))
            }
            
            par(mfrow = c(1, 1))
            
            TRUE
        },
        
        .plotBiplot = function(image, ...) {
            
            if (is.null(private$pc_scores)) return()
            
            # Create PCA biplot
            pc1 <- private$pc_scores[, 1]
            pc2 <- private$pc_scores[, 2]
            
            # Color by survival status
            colors <- ifelse(private$clean_data[[private$status_var]] == 1, "red", "blue")
            
            plot(pc1, pc2, 
                 col = colors, pch = 19, alpha = 0.6,
                 main = "PCA Biplot (PC1 vs PC2)",
                 xlab = paste("PC1 (", round(private$prop_variance[1] * 100, 1), "%)"),
                 ylab = paste("PC2 (", round(private$prop_variance[2] * 100, 1), "%)"))
            
            # Add legend
            legend("topright", 
                   legend = c("Event", "Censored"),
                   col = c("red", "blue"), 
                   pch = 19)
            
            # Add loading vectors for top features (simplified)
            if (!is.null(private$loadings)) {
                # Show only top 5 loadings to avoid clutter
                pc1_loadings <- private$loadings[, 1]
                pc2_loadings <- private$loadings[, 2]
                
                combined_loadings <- sqrt(pc1_loadings^2 + pc2_loadings^2)
                top_features <- order(combined_loadings, decreasing = TRUE)[1:5]
                
                scale_factor <- min(max(abs(c(pc1, pc2)))) / max(abs(c(pc1_loadings, pc2_loadings))) * 0.7
                
                for (i in top_features) {
                    arrows(0, 0, 
                           pc1_loadings[i] * scale_factor, 
                           pc2_loadings[i] * scale_factor,
                           col = "darkgreen", lwd = 2)
                    
                    text(pc1_loadings[i] * scale_factor * 1.1, 
                         pc2_loadings[i] * scale_factor * 1.1,
                         private$predictors[i], 
                         cex = 0.7, col = "darkgreen")
                }
            }
            
            TRUE
        },
        
        .plotSurvival = function(image, ...) {
            
            if (is.null(private$risk_groups) || is.null(private$clean_data)) return()
            
            # Create survival curves by risk group
            surv_data <- data.frame(
                time = private$clean_data[[private$time_var]],
                status = private$clean_data[[private$status_var]],
                risk_group = private$risk_groups
            )
            
            # Fit survival curves
            surv_fit <- survival::survfit(Surv(time, status) ~ risk_group, data = surv_data)
            
            # Plot survival curves
            plot(surv_fit, 
                 col = c("green", "orange", "red"),
                 lwd = 2,
                 main = "Survival Curves by PC-Based Risk Groups",
                 xlab = "Time", 
                 ylab = "Survival Probability")
            
            legend("topright", 
                   legend = c("Low Risk", "Medium Risk", "High Risk"),
                   col = c("green", "orange", "red"), 
                   lwd = 2)
            
            # Add risk table
            times <- seq(0, max(surv_data$time, na.rm = TRUE), length.out = 5)
            risk_table <- summary(surv_fit, times = times)
            
            TRUE
        }
    )
)