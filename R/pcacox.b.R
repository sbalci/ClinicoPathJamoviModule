#' @title Principal Component Cox Models
#' @importFrom R6 R6Class
#' @import jmvcore
#'

pcacoxClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "pcacoxClass",
    inherit = pcacoxBase,
    private = list(

        # Private fields (initialized to NULL)
        clean_data = NULL,
        time_var = NULL,
        status_var = NULL,
        predictors = NULL,
        clinical_vars = NULL,
        X_matrix = NULL,
        pca_result = NULL,
        pc_scores = NULL,
        loadings = NULL,
        eigenvalues = NULL,
        prop_variance = NULL,
        cumul_variance = NULL,
        selected_components = NULL,
        cox_model = NULL,
        cox_data = NULL,
        risk_scores = NULL,
        risk_groups = NULL,
        cv_scores = NULL,

        # Notice collection helpers. A single Preformatted (plain-text) output item:
        # avoids BOTH the jmvcore::Notice serialization error from
        # self$results$insert(999, Notice) AND any HTML in notices (project convention:
        # notice content must be plain text).
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
            # Render immediately so early-return validation aborts still display the notice
            private$.renderNotices()
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }

            # Plain text only — notices avoid HTML by project convention; the Preformatted
            # output item renders this literally (no markup, no injection surface).
            blocks <- vapply(private$.noticeList, function(notice) {
                prefix <- switch(notice$type,
                    ERROR          = "ERROR: ",
                    STRONG_WARNING = "WARNING: ",
                    WARNING        = "WARNING: ",
                    "")
                paste0(prefix, notice$title, "\n", notice$content)
            }, character(1))

            self$results$notices$setContent(paste(blocks, collapse = "\n\n"))
        },

        .init = function() {
            # Package checks are handled via tryCatch in the methods that use them.
        },
        
        .run = function() {

            private$.noticeList <- list()

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
                private$.addNotice('WARNING', 'Rows Excluded',
                    sprintf("%d row(s) excluded because outcome value matched neither event level ('%s') nor censored level ('%s').",
                            n_excluded_outcome, event_level, censor_level)
                )
            }

            # Replace the status column with numeric 0/1 and drop unmatched rows
            clean_data[[status_var]] <- event_numeric
            clean_data <- clean_data[!is.na(clean_data[[status_var]]), ]

            # Validate time values
            time_vals <- as.numeric(clean_data[[time_var]])
            if (any(time_vals <= 0, na.rm = TRUE)) {
                n_nonpos <- sum(time_vals <= 0, na.rm = TRUE)
                private$.addNotice('ERROR', 'Non-Positive Survival Time',
                    sprintf("All survival times must be positive. Found %d non-positive value(s).", n_nonpos)
                )
                return()
            }

            # Check for zero events
            if (sum(clean_data[[status_var]] == 1) == 0) {
                private$.addNotice('ERROR', 'No Events Detected',
                    "No events detected after encoding. All observations are censored. PCA-Cox cannot be fitted."
                )
                return()
            }

            if (nrow(clean_data) < 20) {
                private$.addNotice('ERROR', 'Insufficient Data',
                    sprintf("Insufficient data for PCA-Cox analysis. Need at least 20 complete cases, found %d.", nrow(clean_data))
                )
                return()
            }

            if (length(predictors) > nrow(clean_data)) {
                private$.addNotice('WARNING', 'High-Dimensional Setting',
                    sprintf("High-dimensional setting: p (%d) > n (%d). PCA dimensionality reduction is essential.",
                            length(predictors), nrow(clean_data))
                )
            }

            # TODO (data hygiene): 17 private fields declared at L11-29 (clean_data, time_var,
            # status_var, predictors, clinical_vars, X_matrix, pca_result, pc_scores, loadings,
            # eigenvalues, prop_variance, cumul_variance, selected_components, cox_model,
            # cox_data, risk_scores, risk_groups, cv_scores, ...) persist across .run() calls.
            # If .run() early-returns at L143 (nonPositiveTime), L150 (noEvents), or L162
            # (insufficientData) BEFORE reaching these assignments, prior run's data lingers.
            # Defense-in-depth: reset all 17 fields to NULL at the very top of .run() (mirrors
            # outbreakanalysis.b.R:84-100 10-field-reset pattern). No security exploit path
            # (all downstream consumers re-check state or write to type:text Tables).
            # TODO (correctness/reproducibility): no set.seed anywhere in this file even though
            # the analysis uses cv_folds, bootstrap_validation, and permutation_test (all consume
            # RNG via sample()). Results are non-deterministic across runs of same data. Add
            # a user-configurable `seed` option to .a.yaml and wrap with on.exit({restore})
            # save-restore pattern from optimalcutpoint.b.R:765-772.

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

                # Populate CV results if CV was used
                if (!is.null(private$cv_scores)) {
                    private$.populateCrossValidation()
                }

                # Populate technical details and clinical interpretation
                private$.populateTechnicalDetails()
                private$.populateClinicalInterpretation()

                # Clinical notices
                n_events <- sum(private$clean_data[[private$status_var]])
                if (n_events < 10) {
                    private$.addNotice('STRONG_WARNING', 'Very Low Event Count',
                        sprintf("Only %d events detected. PCA-Cox results are highly unreliable with fewer than 10 events.", n_events)
                    )
                } else if (n_events < 20) {
                    private$.addNotice('WARNING', 'Low Event Count',
                        sprintf("Only %d events detected. Consider reducing the number of components to improve stability.", n_events)
                    )
                }

                c_idx <- tryCatch(survival::concordance(private$cox_model)$concordance, error = function(e) NA)
                if (!is.na(c_idx) && c_idx < 0.5) {
                    private$.addNotice('STRONG_WARNING', 'Poor Discrimination',
                        sprintf("Training C-index = %.3f is below 0.5, indicating worse-than-chance discrimination.", c_idx)
                    )
                }

                # Success notice
                private$.addNotice('INFO', 'Analysis Complete',
                    sprintf("PCA-Cox completed: %d components selected from %d predictors, %d observations (%d events), training C-index = %.3f.",
                            private$selected_components, length(private$predictors),
                            nrow(private$clean_data), n_events,
                            ifelse(is.na(c_idx), 0.5, c_idx))
                )
            }

            # Loading-based feature clustering analysis
            if (isTRUE(self$options$pathway_analysis)) {
                private$.performLoadingClusterAnalysis()
            }
        },
        
        .performPCA = function() {
            
            tryCatch({
                
                # Extract predictor matrix using model.matrix to handle factors
                # composeTerms backtick-escapes user-supplied predictor names (Defense 1);
                # jmvcore::asFormula allow-list validates the RHS (Defense 2 — closes C1 RCE).
                formula_str <- paste("~ -1 +", paste(jmvcore::composeTerms(as.list(private$predictors)), collapse = " + "))
                X <- model.matrix(jmvcore::asFormula(formula_str), data = private$clean_data)
                
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
                private$.addNotice('ERROR', 'PCA Analysis Failed',
                    sprintf("PCA analysis failed: %s", conditionMessage(e))
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

            # If survival_weighting is FALSE, use standard PCA instead
            if (!isTRUE(self$options$survival_weighting)) {
                private$.performStandardPCA()
                return()
            }

            tryCatch({

                if (!requireNamespace('superpc', quietly = TRUE)) {
                    private$.addNotice('WARNING', 'Package superpc Not Available',
                        "Package 'superpc' not available. Falling back to standard PCA. Install with: install.packages('superpc')"
                    )
                    private$.performStandardPCA()
                    return()
                }

                # Prepare data for superpc
                censoring_status <- private$clean_data[[private$status_var]]

                data_superpc <- list(
                    x = t(private$X_matrix),
                    y = private$clean_data[[private$time_var]],
                    censoring.status = censoring_status,
                    featurenames = colnames(private$X_matrix)
                )

                # Train supervised PC
                train_result <- superpc::superpc.train(
                    data = data_superpc,
                    type = "survival"
                )

                n_components <- min(self$options$n_components %||% 5,
                                    ncol(private$X_matrix),
                                    nrow(private$X_matrix))

                # Select threshold via cross-validation (not hardcoded)
                cv_folds <- min(self$options$cv_folds %||% 10, floor(nrow(private$X_matrix) / 5))
                optimal_threshold <- tryCatch({
                    cv_result <- superpc::superpc.cv(
                        train_result,
                        data_superpc,
                        n.fold = max(3, cv_folds),
                        n.components = min(3, n_components),
                        n.threshold = 10
                    )
                    # Pick threshold with best CV likelihood ratio
                    best_idx <- which.max(cv_result$scor[, 1])
                    cv_result$thresholds[best_idx]
                }, error = function(e) {
                    # Fallback: use median absolute score as threshold
                    median(abs(train_result$feature.scores))
                })

                # Extract supervised PC scores with CV-selected threshold
                pred_result <- superpc::superpc.predict(
                    train_result,
                    data_superpc,
                    data_superpc,
                    threshold = optimal_threshold,
                    n.components = n_components
                )

                private$pca_result <- train_result
                private$pc_scores <- matrix(pred_result$v.pred, ncol = n_components)
                colnames(private$pc_scores) <- paste0("PC", 1:n_components)

                private$.approximatePCAMetrics()

            }, error = function(e) {
                private$.addNotice('WARNING', 'Supervised PCA Failed',
                    sprintf("Supervised PCA failed: %s. Using standard PCA.", conditionMessage(e))
                )
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
                    private$.addNotice('WARNING', 'Sparse PCA Failed',
                        sprintf("Sparse PCA failed: %s. Using standard PCA instead.", conditionMessage(e))
                    )
                    private$.performStandardPCA()
                })
            } else {
                private$.addNotice('WARNING', 'Package sparsepca Not Available',
                    "Package 'sparsepca' not available. Using standard PCA. Install with: install.packages('sparsepca')"
                )
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
                    private$.addNotice('WARNING', 'Kernel PCA Failed',
                        sprintf("Kernel PCA failed: %s. Using standard PCA instead.", conditionMessage(e))
                    )
                    private$.performStandardPCA()
                })
            } else {
                private$.addNotice('WARNING', 'Package kernlab Not Available',
                    "Package 'kernlab' not available. Using standard PCA. Install with: install.packages('kernlab')"
                )
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
                # Cross-validation selection
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
                        
                        # PC names are internal ("PC1"/"PC2"/...); no composeTerms needed.
                        # .asSurvivalFormula provides Defense 2 (Surv allow-list for jamovi 2.7.27+).
                        cox_formula <- .asSurvivalFormula(paste("Surv(time, status) ~",
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
                private$.addNotice('WARNING', 'CV Component Selection Failed',
                    sprintf("CV component selection failed: %s. Using default %d components.", conditionMessage(e), min(5, ncol(private$pc_scores)))
                )
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
                # PC names ("PC1"/"PC2"/...) are internal — no escape needed.
                # composeTerms backtick-escapes user-supplied clinical_vars (Defense 1).
                # .asSurvivalFormula provides Defense 2 (Surv allow-list).
                pc_vars <- colnames(selected_pcs)
                all_vars_escaped <- c(pc_vars, jmvcore::composeTerms(as.list(private$clinical_vars)))

                cox_formula <- .asSurvivalFormula(paste("Surv(time, status) ~",
                                              paste(all_vars_escaped, collapse = " + ")))
                
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
                private$.addNotice('ERROR', 'Cox Model Fitting Failed',
                    sprintf("Cox model fitting failed: %s", conditionMessage(e))
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
                n_obs <- nrow(private$cox_data)
                cox_snell <- 1 - exp((loglik[1] - loglik[2]) * 2 / n_obs)
                r_max <- 1 - exp(2 * loglik[1] / n_obs)
                r_squared <- cox_snell / r_max  # Nagelkerke correction
                
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
                private$.addNotice('WARNING', 'Performance Calculation Failed',
                    sprintf("Performance calculation failed: %s", conditionMessage(e))
                )
            })
        },
        
        .calculateFeatureImportance = function() {

            tryCatch({

                if (is.null(private$loadings) || is.null(private$cox_model)) return()

                table <- self$results$featureImportance

                cox_coef <- coef(private$cox_model)
                pc_coef <- cox_coef[grepl("PC", names(cox_coef))]

                # Use loading row names (= design matrix columns) for feature names
                # This handles the case where model.matrix expanded factors into dummies
                feat_names <- rownames(private$loadings)
                if (is.null(feat_names)) feat_names <- colnames(private$X_matrix)
                if (is.null(feat_names)) feat_names <- private$predictors
                n_feats <- length(feat_names)

                # Calculate weighted loadings across selected components
                importance_scores <- numeric(n_feats)
                names(importance_scores) <- feat_names

                n_comp <- min(private$selected_components, ncol(private$loadings))
                for (i in seq_len(n_comp)) {
                    pc_name <- paste0("PC", i)
                    if (pc_name %in% names(pc_coef)) {
                        loadings_i <- private$loadings[, i]
                        importance_scores <- importance_scores + abs(pc_coef[pc_name] * loadings_i[seq_len(n_feats)])
                    }
                }

                feature_ranks <- rank(-importance_scores)
                top_features <- order(importance_scores, decreasing = TRUE)[1:min(20, n_feats)]

                for (idx in top_features) {
                    feature_name <- feat_names[idx]
                    importance <- importance_scores[idx]
                    rank_val <- feature_ranks[idx]

                    pc_contributions <- abs(private$loadings[idx, seq_len(n_comp)])
                    main_pcs <- paste(paste0("PC", which(pc_contributions > 0.1)), collapse = ", ")
                    if (nchar(main_pcs) == 0) main_pcs <- paste0("PC", which.max(pc_contributions))

                    table$addRow(rowKey = idx, values = list(
                        feature = feature_name,
                        importance_score = round(importance, 4),
                        rank = rank_val,
                        contributing_pcs = main_pcs
                    ))
                }

            }, error = function(e) {
                private$.addNotice('WARNING', 'Feature Importance Failed',
                    sprintf("Feature importance calculation failed: %s", conditionMessage(e))
                )
            })
        },
        
        .calculateRiskScore = function() {
            
            tryCatch({
                
                if (is.null(private$cox_model)) return()
                
                # Calculate risk scores
                risk_scores <- predict(private$cox_model, type = "lp")
                
                # Divide into risk groups (tertiles)
                # Use jitter to break ties when quantiles collapse
                tertiles <- quantile(risk_scores, c(1/3, 2/3), na.rm = TRUE)
                if (tertiles[1] >= tertiles[2]) {
                    tertiles <- quantile(jitter(risk_scores, amount = diff(range(risk_scores, na.rm = TRUE)) * 0.001),
                                         c(1/3, 2/3), na.rm = TRUE)
                }

                unique_breaks <- unique(c(-Inf, tertiles, Inf))
                if (length(unique_breaks) < 4) {
                    # Fallback: median split into 2 groups
                    med <- median(risk_scores, na.rm = TRUE)
                    risk_groups <- factor(
                        ifelse(risk_scores <= med, "Low", "High"),
                        levels = c("Low", "High")
                    )
                } else {
                    risk_groups <- cut(risk_scores,
                                     breaks = c(-Inf, tertiles, Inf),
                                     labels = c("Low", "Medium", "High"))
                }
                
                # Calculate group statistics
                table <- self$results$riskScore
                group_levels <- levels(risk_groups)

                for (group in group_levels) {

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
                private$.addNotice('WARNING', 'Risk Score Calculation Failed',
                    sprintf("Risk score calculation failed: %s", conditionMessage(e))
                )
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
                "<li><b>Method:</b> ", tools::toTitleCase(gsub("_", " ", pca_method)), "</li>",
                "<li><b>Components Selected:</b> ", n_components, "</li>",
                "<li><b>Variance Explained:</b> ", round(private$cumul_variance[n_components] * 100, 1), "%</li>",
                "<li><b>Selection Method:</b> ", tools::toTitleCase(gsub("_", " ", self$options$component_selection)), "</li>",
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
        
        .populateModelComparison = function() {
            if (is.null(private$cox_model) || is.null(private$pc_scores)) return()

            table <- self$results$modelComparison
            max_k <- min(private$selected_components + 2, ncol(private$pc_scores), nrow(private$clean_data) - 5)

            for (k in 1:max_k) {
                tryCatch({
                    pcs_k <- private$pc_scores[, 1:k, drop = FALSE]
                    cox_data_k <- data.frame(
                        time = private$clean_data[[private$time_var]],
                        status = private$clean_data[[private$status_var]],
                        pcs_k
                    )
                    pc_names_k <- colnames(pcs_k)
                    # PC names internal; composeTerms on user clinical_vars (Defense 1).
                    # .asSurvivalFormula provides Defense 2 (Surv allow-list).
                    all_vars_k_escaped <- c(pc_names_k, jmvcore::composeTerms(as.list(private$clinical_vars)))
                    fml_k <- .asSurvivalFormula(paste("Surv(time, status) ~", paste(all_vars_k_escaped, collapse = " + ")))

                    if (length(private$clinical_vars) > 0) {
                        clin_data <- private$clean_data[, private$clinical_vars, drop = FALSE]
                        cox_data_k <- cbind(cox_data_k, clin_data)
                    }

                    model_k <- survival::coxph(fml_k, data = cox_data_k)
                    c_idx <- survival::concordance(model_k)$concordance

                    table$addRow(rowKey = k, values = list(
                        model = paste0(k, " PC", ifelse(k > 1, "s", ""),
                                       if (length(private$clinical_vars) > 0) paste0(" + ", length(private$clinical_vars), " clinical") else ""),
                        n_components = as.integer(k),
                        c_index = round(c_idx, 4),
                        aic = round(AIC(model_k), 2),
                        bic = round(BIC(model_k), 2),
                        log_likelihood = round(logLik(model_k), 2)
                    ))
                }, error = function(e) NULL)
            }
        },

        .populateCrossValidation = function() {
            if (is.null(private$cv_scores)) return()

            cv_html <- paste0(
                "<h4>Cross-Validation Component Selection</h4>",
                "<p>CV C-index for 1 to ", length(private$cv_scores), " components:</p>",
                "<table border='1' cellpadding='4' cellspacing='0' style='border-collapse:collapse;'>",
                "<tr><th>Components</th><th>CV C-Index</th><th>Selected</th></tr>"
            )
            for (k in seq_along(private$cv_scores)) {
                is_sel <- if (k == private$selected_components) "<b>Yes</b>" else ""
                cv_html <- paste0(cv_html,
                    "<tr><td>", k, "</td><td>", round(private$cv_scores[k], 4),
                    "</td><td>", is_sel, "</td></tr>")
            }
            cv_html <- paste0(cv_html,
                "</table>",
                "<p>Optimal: ", private$selected_components,
                " components (max CV C-index = ", round(max(private$cv_scores, na.rm = TRUE), 4), ")</p>")

            self$results$crossValidation$setContent(cv_html)
        },

        .populateTechnicalDetails = function() {
            pca_method <- self$options$pca_method %||% "supervised"
            sel_method <- self$options$component_selection %||% "fixed"
            n <- nrow(private$clean_data)
            p <- length(private$predictors)

            html <- paste0(
                "<h4>Technical Details</h4>",
                "<p><b>PCA Method:</b> ", tools::toTitleCase(gsub("_", " ", pca_method)), "</p>",
                "<ul>",
                if (pca_method == "supervised") "<li>Supervised PCA via superpc: uses survival information to weight features before extracting components</li>"
                else if (pca_method == "sparse") paste0("<li>Sparse PCA via sparsepca: sparsity parameter = ", self$options$sparse_parameter, "</li>")
                else if (pca_method == "kernel") "<li>Kernel PCA via kernlab: Radial Basis Function kernel for nonlinear relationships</li>"
                else "<li>Standard PCA via prcomp: unsupervised dimensionality reduction</li>",
                "</ul>",
                "<p><b>Component Selection:</b> ", tools::toTitleCase(gsub("_", " ", sel_method)), "</p>",
                "<p><b>Preprocessing:</b> ",
                if (self$options$centering) "Centered" else "Not centered", " + ",
                if (self$options$scaling) "Scaled" else "Not scaled", "</p>",
                "<p><b>Design Matrix:</b> ", n, " observations x ", ncol(private$X_matrix), " columns",
                if (ncol(private$X_matrix) != p) paste0(" (expanded from ", p, " predictors via indicator variable encoding)") else "",
                "</p>",
                "<p><b>Cox Model:</b> Surv(time, status) ~ PC1 + ... + PC", private$selected_components,
                if (length(private$clinical_vars) > 0) paste0(" + ", paste(private$clinical_vars, collapse = " + ")) else "",
                "</p>"
            )
            self$results$technicalDetails$setContent(html)
        },

        .populateClinicalInterpretation = function() {
            if (is.null(private$cox_model)) return()

            c_idx <- survival::concordance(private$cox_model)$concordance
            n_sel <- private$selected_components
            n_events <- sum(private$clean_data[[private$status_var]])

            disc_label <- if (c_idx >= 0.7) "good" else if (c_idx >= 0.6) "moderate" else "weak"

            html <- paste0(
                "<h4>Clinical Interpretation</h4>",
                "<p>A PCA-Cox model using <b>", n_sel, " principal component",
                ifelse(n_sel > 1, "s", ""), "</b> was fitted to ",
                nrow(private$clean_data), " patients (", n_events, " events).</p>",
                "<p>The model showed <b>", disc_label, " discrimination</b> (training C-index = ",
                round(c_idx, 3), "). ",
                if (c_idx >= 0.7) "This suggests the PC-based risk score may be clinically useful for patient stratification."
                else if (c_idx >= 0.6) "Performance is moderate; consider bootstrap validation to assess overfitting."
                else "Performance is weak; the PCA components may not capture sufficient survival-relevant variation.",
                "</p>",
                "<p><b>Important caveats:</b></p>",
                "<ul>",
                "<li>Training C-index overestimates true performance. Use bootstrap validation for realistic estimates.</li>",
                "<li>PCA components are linear combinations of original features and may lack direct biological interpretability.</li>",
                "<li>External validation on independent data is essential before clinical deployment.</li>",
                "</ul>"
            )
            self$results$clinicalInterpretation$setContent(html)
        },

        .performLoadingClusterAnalysis = function() {
            if (is.null(private$loadings) || is.null(private$cox_model)) return()

            tryCatch({
                cox_coef <- coef(private$cox_model)
                pc_coef <- cox_coef[grepl("PC", names(cox_coef))]
                n_comp <- private$selected_components
                feat_names <- rownames(private$loadings)
                if (is.null(feat_names)) feat_names <- private$predictors

                # For each component, identify top contributing features
                # and compute weighted importance per feature
                importance <- numeric(length(feat_names))
                names(importance) <- feat_names
                for (k in seq_len(n_comp)) {
                    pc_name <- paste0("PC", k)
                    if (pc_name %in% names(pc_coef)) {
                        importance <- importance + abs(pc_coef[pc_name] * private$loadings[, k])
                    }
                }

                # Assign each feature to its dominant component
                dominant_pc <- apply(abs(private$loadings[, 1:n_comp, drop = FALSE]), 1, which.max)

                # Build component-wise feature clusters
                html <- "<h4>Feature Cluster Analysis by Principal Component</h4>"
                html <- paste0(html,
                    "<p>Features are grouped by their dominant principal component (highest absolute loading). ",
                    "Within each cluster, features are ranked by survival-weighted importance.</p>")

                for (k in seq_len(n_comp)) {
                    pc_name <- paste0("PC", k)
                    cluster_idx <- which(dominant_pc == k)
                    if (length(cluster_idx) == 0) next

                    # Sort by importance within cluster
                    cluster_imp <- importance[cluster_idx]
                    cluster_order <- cluster_idx[order(cluster_imp, decreasing = TRUE)]

                    # Cox coefficient for this PC
                    pc_beta <- if (pc_name %in% names(pc_coef)) pc_coef[pc_name] else NA
                    hr_text <- if (!is.na(pc_beta)) paste0(" (HR = ", round(exp(pc_beta), 3), ")") else ""
                    var_pct <- round(private$prop_variance[k] * 100, 1)

                    html <- paste0(html,
                        "<h5>", pc_name, " Cluster: ", var_pct, "% variance", hr_text, "</h5>",
                        "<table border='1' cellpadding='3' cellspacing='0' style='border-collapse:collapse; font-size:13px;'>",
                        "<tr><th>Feature</th><th>Loading</th><th>Importance</th></tr>")

                    n_show <- min(10, length(cluster_order))
                    for (j in seq_len(n_show)) {
                        idx <- cluster_order[j]
                        html <- paste0(html,
                            "<tr><td>", feat_names[idx], "</td>",
                            "<td>", round(private$loadings[idx, k], 4), "</td>",
                            "<td>", round(importance[idx], 4), "</td></tr>")
                    }
                    if (length(cluster_order) > n_show) {
                        html <- paste0(html, "<tr><td colspan='3'><i>... and ",
                                       length(cluster_order) - n_show, " more features</i></td></tr>")
                    }
                    html <- paste0(html, "</table>")
                }

                # Summary statistics
                n_features <- length(feat_names)
                cluster_sizes <- table(dominant_pc)
                html <- paste0(html,
                    "<p><b>Summary:</b> ", n_features, " features distributed across ",
                    n_comp, " components. ",
                    "Largest cluster: PC", names(which.max(cluster_sizes)),
                    " (", max(cluster_sizes), " features). ",
                    "Smallest cluster: PC", names(which.min(cluster_sizes)),
                    " (", min(cluster_sizes), " features).</p>",
                    "<p><i>Note: For formal pathway enrichment analysis (GSEA, KEGG, Reactome), ",
                    "export feature importance rankings and use dedicated tools like clusterProfiler or Enrichr.</i></p>")

                self$results$pathwayAnalysis$setContent(html)

            }, error = function(e) {
                private$.addNotice('WARNING', 'Feature Cluster Analysis Failed',
                    sprintf("Feature cluster analysis could not be completed: %s", conditionMessage(e))
                )
            })
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
        
        .plotScree = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            eigenvalues <- state$eigenvalues
            selected <- state$selected
            n_show <- min(20, length(eigenvalues))

            prop_var <- eigenvalues[1:n_show] / sum(eigenvalues) * 100

            df <- data.frame(
                pc = 1:n_show,
                eigenvalue = eigenvalues[1:n_show],
                pct = prop_var,
                sel = factor(ifelse(1:n_show <= selected, "Selected", "Not selected"))
            )

            p <- ggplot2::ggplot(df, ggplot2::aes(x = pc, y = eigenvalue)) +
                ggplot2::geom_line(color = "steelblue", linewidth = 1) +
                ggplot2::geom_point(ggplot2::aes(color = sel), size = 3) +
                ggplot2::scale_color_manual(values = c("Selected" = "#2ecc71", "Not selected" = "grey60"),
                                           name = "") +
                ggplot2::geom_vline(xintercept = selected + 0.5, linetype = "dashed", color = "red") +
                ggplot2::geom_text(ggplot2::aes(label = paste0(round(pct, 1), "%")),
                                   vjust = -1, size = 3) +
                ggplot2::labs(
                    title = "Scree Plot",
                    subtitle = paste(selected, "components selected"),
                    x = "Principal Component",
                    y = "Eigenvalue"
                ) +
                ggtheme

            print(p)
            TRUE
        },
        
        .plotLoadings = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            loadings_df <- as.data.frame(state$loadings)
            selected <- state$selected
            n_comp <- min(selected, 2, ncol(loadings_df))

            # Build long-form data for top loadings of first n_comp PCs
            plot_data <- data.frame()
            for (i in 1:n_comp) {
                vals <- loadings_df[, i]
                names(vals) <- rownames(loadings_df)
                top_idx <- order(abs(vals), decreasing = TRUE)[1:min(15, length(vals))]
                df_i <- data.frame(
                    feature = factor(names(vals)[top_idx], levels = rev(names(vals)[top_idx])),
                    loading = vals[top_idx],
                    component = paste0("PC", i),
                    direction = ifelse(vals[top_idx] > 0, "Positive", "Negative"),
                    stringsAsFactors = FALSE
                )
                plot_data <- rbind(plot_data, df_i)
            }

            p <- ggplot2::ggplot(plot_data,
                    ggplot2::aes(x = feature, y = loading, fill = direction)) +
                ggplot2::geom_col(alpha = 0.8) +
                ggplot2::scale_fill_manual(values = c("Positive" = "#3498db", "Negative" = "#e74c3c"),
                                          name = "Direction") +
                ggplot2::coord_flip() +
                ggplot2::facet_wrap(~ component, scales = "free_y") +
                ggplot2::labs(title = "Component Loadings (Top Features)",
                             x = "", y = "Loading") +
                ggtheme

            print(p)
            TRUE
        },
        
        .plotBiplot = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            pc_df <- as.data.frame(state$pc_scores)
            loadings_df <- as.data.frame(state$loadings)

            if (ncol(pc_df) < 2 || ncol(loadings_df) < 2) return(FALSE)

            pc1 <- pc_df[, 1]
            pc2 <- pc_df[, 2]
            event_status <- factor(
                ifelse(private$clean_data[[private$status_var]] == 1, "Event", "Censored"),
                levels = c("Event", "Censored")
            )

            scores_df <- data.frame(PC1 = pc1, PC2 = pc2, Status = event_status)

            # Variance labels
            pv1 <- round(private$prop_variance[1] * 100, 1)
            pv2 <- round(private$prop_variance[2] * 100, 1)

            p <- ggplot2::ggplot(scores_df, ggplot2::aes(x = PC1, y = PC2, color = Status)) +
                ggplot2::geom_point(alpha = 0.6, size = 2) +
                ggplot2::scale_color_manual(values = c("Event" = "#e74c3c", "Censored" = "#3498db")) +
                ggplot2::labs(
                    title = "PCA Biplot",
                    x = paste0("PC1 (", pv1, "%)"),
                    y = paste0("PC2 (", pv2, "%)")
                ) +
                ggtheme

            # Add top 5 loading vectors
            l1 <- loadings_df[, 1]
            l2 <- loadings_df[, 2]
            combined <- sqrt(l1^2 + l2^2)
            top5 <- order(combined, decreasing = TRUE)[1:min(5, length(combined))]
            scale_f <- min(max(abs(pc1)), max(abs(pc2))) / max(abs(c(l1, l2))) * 0.7

            arrow_df <- data.frame(
                x = 0, y = 0,
                xend = l1[top5] * scale_f,
                yend = l2[top5] * scale_f,
                label = rownames(loadings_df)[top5]
            )
            p <- p +
                ggplot2::geom_segment(data = arrow_df,
                    ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                    color = "darkgreen", linewidth = 0.8, inherit.aes = FALSE) +
                ggplot2::geom_text(data = arrow_df,
                    ggplot2::aes(x = xend * 1.1, y = yend * 1.1, label = label),
                    color = "darkgreen", size = 3, inherit.aes = FALSE)

            print(p)
            TRUE
        },
        
        .plotSurvival = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            surv_data <- as.data.frame(state)
            surv_data$risk_groups <- factor(surv_data$risk_groups)

            surv_fit <- survival::survfit(
                survival::Surv(time, status) ~ risk_groups, data = surv_data
            )

            # Build KM data manually for ggplot
            n_groups <- length(levels(surv_data$risk_groups))
            group_colors <- c("#2ecc71", "#f39c12", "#e74c3c")[1:n_groups]

            plot_data <- data.frame()
            for (i in seq_along(surv_fit$strata)) {
                strata_name <- names(surv_fit$strata)[i]
                grp_label <- sub("risk_groups=", "", strata_name)
                n_times <- surv_fit$strata[i]
                start_idx <- if (i == 1) 1 else sum(surv_fit$strata[1:(i-1)]) + 1
                end_idx <- sum(surv_fit$strata[1:i])

                df_i <- data.frame(
                    time = surv_fit$time[start_idx:end_idx],
                    surv = surv_fit$surv[start_idx:end_idx],
                    group = grp_label,
                    stringsAsFactors = FALSE
                )
                # Add origin point
                df_i <- rbind(data.frame(time = 0, surv = 1, group = grp_label), df_i)
                plot_data <- rbind(plot_data, df_i)
            }

            p <- ggplot2::ggplot(plot_data,
                    ggplot2::aes(x = time, y = surv, color = group)) +
                ggplot2::geom_step(linewidth = 1) +
                ggplot2::scale_color_manual(values = group_colors, name = "Risk Group") +
                ggplot2::labs(
                    title = "Survival by PC-Based Risk Groups",
                    x = "Time",
                    y = "Survival Probability"
                ) +
                ggplot2::ylim(0, 1) +
                ggtheme

            print(p)
            TRUE
        }
    )
)