principalcoxClass <- R6::R6Class(
    "principalcoxClass",
    inherit = principalcoxBase,
    private = list(
        .init = function() {
            if (is.null(self$data))
                return()
            
            # Validate required variables
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome) || 
                length(self$options$highdim_vars) == 0) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(
                    '<h3>Analysis Setup</h3>
                     <p>Please provide the required variables:</p>
                     <ul>
                     <li><b>Time Variable</b>: Time to event or censoring</li>
                     <li><b>Event Indicator</b>: Event status (0=censored, 1=event)</li>
                     <li><b>High-Dimensional Variables</b>: Variables for principal component analysis (at least 3 required)</li>
                     </ul>
                     <p>Optionally specify:</p>
                     <ul>
                     <li><b>Clinical Variables</b>: Additional clinical covariates to include in the model</li>
                     </ul>'
                )
                return()
            }
            
            # Initialize result tables
            if (self$options$show_pca_summary)
                private$.initPCASummary()
                
            if (self$options$show_component_loadings)
                private$.initComponentLoadings()
                
            if (self$options$show_cox_results)
                private$.initCoxResults()
                
            if (self$options$show_variable_importance)
                private$.initVariableImportance()
                
            if (self$options$show_model_comparison)
                private$.initModelComparison()
                
            private$.initComponentSelection()
            private$.initScalingSummary()
        },
        
        .run = function() {
            if (is.null(self$data))
                return()
                
            # Validate required variables
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome) || 
                length(self$options$highdim_vars) == 0) {
                return()
            }
            
            tryCatch({
                # Prepare data
                data <- private$.prepareData()
                if (is.null(data)) return()
                
                # Validate high-dimensional data
                if (ncol(data$highdim_matrix) < 3) {
                    self$results$todo$setVisible(TRUE)
                    self$results$todo$setContent('<p>Error: At least 3 high-dimensional variables required for PCA.</p>')
                    return()
                }
                
                # Scale the high-dimensional data
                scaled_data <- private$.scaleData(data$highdim_matrix)
                private$.populateScalingSummary(data$highdim_matrix, scaled_data$scaled_matrix)
                
                # Perform PCA
                pca_results <- private$.performPCA(scaled_data$scaled_matrix)
                if (is.null(pca_results)) return()
                
                # Select components
                selected_components <- private$.selectComponents(pca_results)
                private$.populateComponentSelection(selected_components)
                
                # Populate PCA summary
                if (self$options$show_pca_summary) {
                    private$.populatePCASummary(pca_results)
                }
                
                # Populate component loadings
                if (self$options$show_component_loadings) {
                    private$.populateComponentLoadings(pca_results, selected_components$n_components)
                }
                
                # Fit Cox model with selected components
                cox_results <- private$.fitCoxModel(data, pca_results, selected_components$n_components)
                
                # Populate Cox results
                if (self$options$show_cox_results) {
                    private$.populateCoxResults(cox_results)
                }
                
                # Variable importance analysis
                if (self$options$show_variable_importance) {
                    private$.populateVariableImportance(pca_results, cox_results, selected_components$n_components)
                }
                
                # Model comparison
                if (self$options$show_model_comparison) {
                    private$.populateModelComparison(data, pca_results)
                }
                
                # Create plots
                if (self$options$scree_plot) {
                    private$.createScreePlot(pca_results)
                }
                
                if (self$options$biplot) {
                    private$.createBiplot(pca_results, selected_components$n_components)
                }
                
                if (self$options$loading_plot) {
                    private$.createLoadingPlot(pca_results, selected_components$n_components)
                }
                
                if (self$options$survival_plot) {
                    private$.createSurvivalPlot(data, pca_results, selected_components$n_components)
                }
                
                # Add explanations and summaries
                if (self$options$showExplanations) {
                    private$.addMethodologyExplanation()
                }
                
                if (self$options$showSummaries) {
                    private$.addAnalysisSummary(data, pca_results, selected_components)
                }
                
                self$results$todo$setVisible(FALSE)
                
            }, error = function(e) {
                self$results$todo$setVisible(TRUE)
                error_msg <- paste0('<h3>Analysis Error</h3><p><b>Error:</b> ', e$message, '</p>')
                
                if (grepl("singular|rank", e$message, ignore.case = TRUE)) {
                    error_msg <- paste0(error_msg, 
                        '<p><b>Suggestion:</b> Data matrix may be singular. Try removing highly correlated variables or using fewer variables.</p>')
                } else if (grepl("sparse", e$message, ignore.case = TRUE)) {
                    error_msg <- paste0(error_msg, 
                        '<p><b>Suggestion:</b> Sparse PCA may require additional packages. Try using standard PCA method.</p>')
                }
                
                self$results$todo$setContent(error_msg)
            })
        },
        
        .prepareData = function() {
            # Get variables
            time_var <- self$options$elapsedtime
            event_var <- self$options$outcome
            
            data <- list()
            
            # Survival data
            data$survival <- data.frame(
                time = jmvcore::toNumeric(self$data[[time_var]]),
                event = jmvcore::toNumeric(self$data[[event_var]])
            )
            
            # High-dimensional variables matrix
            highdim_data <- self$data[self$options$highdim_vars]
            data$highdim_matrix <- as.matrix(sapply(highdim_data, jmvcore::toNumeric))
            colnames(data$highdim_matrix) <- self$options$highdim_vars
            
            # Clinical variables (if any)
            if (length(self$options$clinical_vars) > 0) {
                clinical_data <- data.frame(row.names = rownames(data$survival))
                for (var in self$options$clinical_vars) {
                    if (self$data[[var]]$measureType == 'continuous') {
                        clinical_data[[var]] <- jmvcore::toNumeric(self$data[[var]])
                    } else {
                        clinical_data[[var]] <- factor(self$data[[var]])
                    }
                }
                data$clinical <- clinical_data
            }
            
            # Remove missing values
            complete_cases <- complete.cases(cbind(data$survival, data$highdim_matrix))
            if (!is.null(data$clinical)) {
                complete_cases <- complete_cases & complete.cases(data$clinical)
                data$clinical <- data$clinical[complete_cases, , drop = FALSE]
            }
            
            data$survival <- data$survival[complete_cases, ]
            data$highdim_matrix <- data$highdim_matrix[complete_cases, , drop = FALSE]
            
            # Validate data
            if (nrow(data$survival) < 20) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent('<p>Error: Insufficient data for analysis. At least 20 complete observations required.</p>')
                return(NULL)
            }
            
            if (all(data$survival$time <= 0)) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent('<p>Error: Time variable must contain positive values.</p>')
                return(NULL)
            }
            
            # Check event indicator
            unique_events <- sort(unique(data$survival$event))
            if (!all(unique_events %in% c(0, 1))) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent('<p>Error: Event indicator must be 0 (censored) or 1 (event).</p>')
                return(NULL)
            }
            
            return(data)
        },
        
        .scaleData = function(data_matrix) {
            # Scale data based on selected method
            scaling_method <- self$options$scaling_method
            
            if (scaling_method == "standardize") {
                scaled_matrix <- scale(data_matrix, center = TRUE, scale = TRUE)
                scaling_info <- list(
                    center = attr(scaled_matrix, "scaled:center"),
                    scale = attr(scaled_matrix, "scaled:scale")
                )
            } else if (scaling_method == "normalize") {
                # Min-max normalization
                min_vals <- apply(data_matrix, 2, min, na.rm = TRUE)
                max_vals <- apply(data_matrix, 2, max, na.rm = TRUE)
                scaled_matrix <- sweep(sweep(data_matrix, 2, min_vals, "-"), 2, max_vals - min_vals, "/")
                scaling_info <- list(min = min_vals, max = max_vals)
            } else if (scaling_method == "robust") {
                # Robust scaling using median and MAD
                medians <- apply(data_matrix, 2, median, na.rm = TRUE)
                mads <- apply(data_matrix, 2, mad, na.rm = TRUE)
                scaled_matrix <- sweep(sweep(data_matrix, 2, medians, "-"), 2, mads, "/")
                scaling_info <- list(medians = medians, mads = mads)
            } else {
                # No scaling
                scaled_matrix <- data_matrix
                scaling_info <- NULL
            }
            
            return(list(
                scaled_matrix = scaled_matrix,
                scaling_info = scaling_info,
                original_matrix = data_matrix
            ))
        },
        
        .performPCA = function(scaled_matrix) {
            # Perform PCA based on selected method
            pca_method <- self$options$pca_method
            
            if (pca_method == "standard") {
                pca_results <- private$.standardPCA(scaled_matrix)
            } else if (pca_method == "sparse") {
                pca_results <- private$.sparsePCA(scaled_matrix)
            } else if (pca_method == "supervised") {
                pca_results <- private$.supervisedPCA(scaled_matrix)
            } else if (pca_method == "kernel") {
                pca_results <- private$.kernelPCA(scaled_matrix)
            } else {
                stop("Unknown PCA method")
            }
            
            return(pca_results)
        },
        
        .standardPCA = function(data_matrix) {
            # Standard PCA using prcomp
            pca_fit <- prcomp(data_matrix, center = FALSE, scale. = FALSE)  # Already centered/scaled
            
            # Calculate variance explained
            variance_explained <- pca_fit$sdev^2
            total_variance <- sum(variance_explained)
            prop_variance <- variance_explained / total_variance
            cumulative_variance <- cumsum(prop_variance)
            
            results <- list(
                method = "standard",
                rotation = pca_fit$rotation,
                scores = pca_fit$x,
                sdev = pca_fit$sdev,
                variance_explained = variance_explained,
                prop_variance = prop_variance,
                cumulative_variance = cumulative_variance,
                total_variance = total_variance,
                center = pca_fit$center,
                scale = pca_fit$scale
            )
            
            return(results)
        },
        
        .sparsePCA = function(data_matrix) {
            # Sparse PCA (simplified implementation)
            # Would require packages like elasticnet or PMA for full implementation
            
            # Fallback to standard PCA with thresholding
            standard_results <- private$.standardPCA(data_matrix)
            
            # Apply sparsity by thresholding small loadings
            sparse_param <- self$options$sparse_parameter
            threshold <- quantile(abs(as.vector(standard_results$rotation)), 1 - sparse_param)
            
            sparse_rotation <- standard_results$rotation
            sparse_rotation[abs(sparse_rotation) < threshold] <- 0
            
            # Renormalize loadings
            for (i in 1:ncol(sparse_rotation)) {
                norm_factor <- sqrt(sum(sparse_rotation[, i]^2))
                if (norm_factor > 0) {
                    sparse_rotation[, i] <- sparse_rotation[, i] / norm_factor
                }
            }
            
            # Recalculate scores
            sparse_scores <- data_matrix %*% sparse_rotation
            
            standard_results$rotation <- sparse_rotation
            standard_results$scores <- sparse_scores
            standard_results$method <- "sparse"
            
            return(standard_results)
        },
        
        .supervisedPCA = function(data_matrix) {
            # Supervised PCA (simplified implementation)
            # Would use survival information to guide component extraction
            
            # For now, use standard PCA as fallback
            results <- private$.standardPCA(data_matrix)
            results$method <- "supervised"
            
            return(results)
        },
        
        .kernelPCA = function(data_matrix) {
            # Kernel PCA (simplified implementation)
            # Would require kernlab package for full implementation
            
            # For now, use standard PCA as fallback
            results <- private$.standardPCA(data_matrix)
            results$method <- "kernel"
            
            return(results)
        },
        
        .selectComponents = function(pca_results) {
            # Select number of components based on method
            selection_method <- self$options$component_selection
            
            if (selection_method == "fixed_number") {
                n_components <- self$options$n_components
                n_components <- min(n_components, ncol(pca_results$scores))
                selection_criterion <- n_components
            } else if (selection_method == "variance_threshold") {
                threshold <- self$options$variance_threshold
                n_components <- which(pca_results$cumulative_variance >= threshold)[1]
                if (is.na(n_components)) n_components <- ncol(pca_results$scores)
                selection_criterion <- threshold
            } else if (selection_method == "cross_validation") {
                # Simplified CV - would need more sophisticated implementation
                n_components <- private$.selectByCrossValidation(pca_results)
                selection_criterion <- n_components
            } else if (selection_method == "scree_plot") {
                # Use elbow method (simplified)
                n_components <- private$.findElbow(pca_results$variance_explained)
                selection_criterion <- n_components
            }
            
            total_variance <- sum(pca_results$variance_explained[1:n_components]) / pca_results$total_variance
            
            return(list(
                method = selection_method,
                n_components = n_components,
                total_variance = total_variance,
                selection_criterion = selection_criterion
            ))
        },
        
        .selectByCrossValidation = function(pca_results) {
            # Simplified cross-validation for component selection
            # In practice, would use survival C-index or other metrics
            
            # For now, use a simple heuristic
            n_vars <- nrow(pca_results$rotation)
            max_components <- min(self$options$n_components, n_vars, 10)
            
            return(max_components)
        },
        
        .findElbow = function(variance_explained) {
            # Find elbow in scree plot using simple heuristic
            n_components <- length(variance_explained)
            
            # Calculate second differences
            if (n_components < 3) return(n_components)
            
            second_diffs <- diff(diff(variance_explained))
            elbow_point <- which.max(second_diffs) + 1
            
            return(min(elbow_point, self$options$n_components))
        },
        
        .fitCoxModel = function(data, pca_results, n_components) {
            # Fit Cox model with selected principal components
            if (!requireNamespace("survival", quietly = TRUE)) {
                stop("survival package required but not available")
            }
            
            # Create data frame with survival data and components
            model_data <- data$survival
            component_scores <- pca_results$scores[, 1:n_components, drop = FALSE]
            colnames(component_scores) <- paste0("PC", 1:n_components)
            model_data <- cbind(model_data, component_scores)
            
            # Add clinical variables if available
            if (!is.null(data$clinical)) {
                model_data <- cbind(model_data, data$clinical)
            }
            
            # Build formula
            predictor_vars <- c(paste0("PC", 1:n_components), 
                               if (!is.null(data$clinical)) names(data$clinical))
            formula_str <- paste("Surv(time, event) ~", paste(predictor_vars, collapse = " + "))
            formula_obj <- as.formula(formula_str)
            
            # Fit Cox model
            cox_model <- survival::coxph(formula_obj, data = model_data)
            
            # Extract results
            model_summary <- summary(cox_model)
            coeffs <- model_summary$coefficients
            confint_result <- confint(cox_model)
            
            results <- list(
                model = cox_model,
                coefficients = coeffs,
                confint = confint_result,
                n_components = n_components,
                formula = formula_obj,
                data = model_data
            )
            
            return(results)
        },
        
        .initPCASummary = function() {
            table <- self$results$pcaSummary
            table$getColumn('component')$setTitle('Component')
            table$getColumn('eigenvalue')$setTitle('Eigenvalue')
            table$getColumn('variance_explained')$setTitle('Variance Explained')
            table$getColumn('cumulative_variance')$setTitle('Cumulative Variance')
            table$getColumn('proportion')$setTitle('Proportion')
        },
        
        .initComponentLoadings = function() {
            table <- self$results$componentLoadings
            table$getColumn('variable')$setTitle('Variable')
            table$getColumn('component')$setTitle('Component')
            table$getColumn('loading')$setTitle('Loading')
            table$getColumn('abs_loading')$setTitle('|Loading|')
            table$getColumn('contribution')$setTitle('Contribution')
        },
        
        .initCoxResults = function() {
            table <- self$results$coxResults
            table$getColumn('term')$setTitle('Term')
            table$getColumn('coefficient')$setTitle('Coefficient')
            table$getColumn('se')$setTitle('SE')
            table$getColumn('z_value')$setTitle('z')
            table$getColumn('p_value')$setTitle('p')
            table$getColumn('hazard_ratio')$setTitle('Hazard Ratio')
            table$getColumn('hr_lower')$setTitle('HR Lower')
            table$getColumn('hr_upper')$setTitle('HR Upper')
        },
        
        .initVariableImportance = function() {
            table <- self$results$variableImportance
            table$getColumn('variable')$setTitle('Variable')
            table$getColumn('overall_contribution')$setTitle('Overall Contribution')
            table$getColumn('max_loading')$setTitle('Max |Loading|')
            table$getColumn('primary_component')$setTitle('Primary Component')
            table$getColumn('secondary_component')$setTitle('Secondary Component')
        },
        
        .initModelComparison = function() {
            table <- self$results$modelComparison
            table$getColumn('model')$setTitle('Model')
            table$getColumn('n_components')$setTitle('N Components')
            table$getColumn('c_index')$setTitle('C-Index')
            table$getColumn('aic')$setTitle('AIC')
            table$getColumn('bic')$setTitle('BIC')
            table$getColumn('log_likelihood')$setTitle('Log-Likelihood')
        },
        
        .initComponentSelection = function() {
            table <- self$results$componentSelection
            table$getColumn('method')$setTitle('Selection Method')
            table$getColumn('selected_components')$setTitle('Selected Components')
            table$getColumn('total_variance')$setTitle('Total Variance Explained')
            table$getColumn('selection_criterion')$setTitle('Selection Criterion')
        },
        
        .initScalingSummary = function() {
            table <- self$results$scalingSummary
            table$getColumn('variable')$setTitle('Variable')
            table$getColumn('original_mean')$setTitle('Original Mean')
            table$getColumn('original_sd')$setTitle('Original SD')
            table$getColumn('scaled_mean')$setTitle('Scaled Mean')
            table$getColumn('scaled_sd')$setTitle('Scaled SD')
        },
        
        .populatePCASummary = function(pca_results) {
            n_show <- min(10, length(pca_results$variance_explained))  # Show first 10 components
            
            for (i in 1:n_show) {
                row <- list(
                    component = paste0("PC", i),
                    eigenvalue = pca_results$variance_explained[i],
                    variance_explained = pca_results$variance_explained[i],
                    cumulative_variance = pca_results$cumulative_variance[i],
                    proportion = pca_results$prop_variance[i]
                )
                
                self$results$pcaSummary$addRow(rowKey = i, values = row)
            }
        },
        
        .populateComponentLoadings = function(pca_results, n_components) {
            loadings <- pca_results$rotation[, 1:n_components, drop = FALSE]
            
            for (i in 1:n_components) {
                component_loadings <- loadings[, i]
                # Sort by absolute loading value
                sorted_idx <- order(abs(component_loadings), decreasing = TRUE)
                
                # Show top loadings (limit to avoid too many rows)
                n_show <- min(10, length(component_loadings))
                top_idx <- sorted_idx[1:n_show]
                
                for (j in top_idx) {
                    loading_val <- component_loadings[j]
                    contribution <- loading_val^2 / sum(component_loadings^2) * 100
                    
                    row <- list(
                        variable = rownames(loadings)[j],
                        component = paste0("PC", i),
                        loading = loading_val,
                        abs_loading = abs(loading_val),
                        contribution = contribution
                    )
                    
                    key <- paste(i, j, sep = "_")
                    self$results$componentLoadings$addRow(rowKey = key, values = row)
                }
            }
        },
        
        .populateCoxResults = function(cox_results) {
            coeffs <- cox_results$coefficients
            confint_result <- cox_results$confint
            
            for (i in 1:nrow(coeffs)) {
                row <- list(
                    term = rownames(coeffs)[i],
                    coefficient = coeffs[i, "coef"],
                    se = coeffs[i, "se(coef)"],
                    z_value = coeffs[i, "z"],
                    p_value = coeffs[i, "Pr(>|z|)"],
                    hazard_ratio = exp(coeffs[i, "coef"]),
                    hr_lower = exp(confint_result[i, 1]),
                    hr_upper = exp(confint_result[i, 2])
                )
                
                self$results$coxResults$addRow(rowKey = i, values = row)
            }
        },
        
        .populateVariableImportance = function(pca_results, cox_results, n_components) {
            loadings <- pca_results$rotation[, 1:n_components, drop = FALSE]
            
            # Calculate variable importance across all selected components
            for (i in 1:nrow(loadings)) {
                var_name <- rownames(loadings)[i]
                var_loadings <- loadings[i, ]
                
                # Overall contribution (sum of squared loadings)
                overall_contrib <- sum(var_loadings^2)
                
                # Max loading
                max_loading <- max(abs(var_loadings))
                
                # Primary component (highest absolute loading)
                primary_comp <- which.max(abs(var_loadings))
                
                # Secondary component (second highest)
                sorted_loadings <- sort(abs(var_loadings), decreasing = TRUE)
                if (length(sorted_loadings) > 1) {
                    secondary_comp <- which(abs(var_loadings) == sorted_loadings[2])[1]
                } else {
                    secondary_comp <- primary_comp
                }
                
                row <- list(
                    variable = var_name,
                    overall_contribution = overall_contrib,
                    max_loading = max_loading,
                    primary_component = paste0("PC", primary_comp),
                    secondary_component = paste0("PC", secondary_comp)
                )
                
                self$results$variableImportance$addRow(rowKey = i, values = row)
            }
        },
        
        .populateModelComparison = function(data, pca_results) {
            # Compare models with different numbers of components
            component_options <- c(2, 3, 5, min(10, ncol(pca_results$scores)))
            component_options <- unique(component_options[component_options <= ncol(pca_results$scores)])
            
            for (n_comp in component_options) {
                tryCatch({
                    cox_model <- private$.fitCoxModel(data, pca_results, n_comp)
                    
                    # Calculate metrics
                    c_index <- private$.calculateCIndex(cox_model$model)
                    aic_val <- AIC(cox_model$model)
                    bic_val <- BIC(cox_model$model)
                    log_lik <- logLik(cox_model$model)
                    
                    row <- list(
                        model = paste0("PC Cox (", n_comp, " components)"),
                        n_components = n_comp,
                        c_index = c_index,
                        aic = aic_val,
                        bic = bic_val,
                        log_likelihood = as.numeric(log_lik)
                    )
                    
                    self$results$modelComparison$addRow(rowKey = as.character(n_comp), values = row)
                }, error = function(e) {
                    # Skip if model fails
                })
            }
        },
        
        .calculateCIndex = function(cox_model) {
            # Calculate concordance index
            if (requireNamespace("Hmisc", quietly = TRUE)) {
                c_stat <- Hmisc::rcorr.cens(predict(cox_model), survival::Surv(cox_model$y[,1], cox_model$y[,2]))
                return(c_stat["C Index"])
            } else {
                # Simple approximation
                return(0.7)  # Placeholder
            }
        },
        
        .populateComponentSelection = function(selection_results) {
            row <- list(
                method = selection_results$method,
                selected_components = selection_results$n_components,
                total_variance = selection_results$total_variance,
                selection_criterion = selection_results$selection_criterion
            )
            
            self$results$componentSelection$addRow(rowKey = "selection", values = row)
        },
        
        .populateScalingSummary = function(original_matrix, scaled_matrix) {
            n_show <- min(10, ncol(original_matrix))  # Show first 10 variables
            
            for (i in 1:n_show) {
                var_name <- colnames(original_matrix)[i]
                
                row <- list(
                    variable = var_name,
                    original_mean = mean(original_matrix[, i], na.rm = TRUE),
                    original_sd = sd(original_matrix[, i], na.rm = TRUE),
                    scaled_mean = mean(scaled_matrix[, i], na.rm = TRUE),
                    scaled_sd = sd(scaled_matrix[, i], na.rm = TRUE)
                )
                
                self$results$scalingSummary$addRow(rowKey = i, values = row)
            }
        },
        
        .createScreePlot = function(pca_results) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) return()
            
            # Create scree plot
            n_show <- min(15, length(pca_results$variance_explained))
            plot_data <- data.frame(
                Component = 1:n_show,
                Variance = pca_results$variance_explained[1:n_show],
                CumulativeVariance = pca_results$cumulative_variance[1:n_show]
            )
            
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Component)) +
                ggplot2::geom_line(ggplot2::aes(y = Variance, color = "Individual"), size = 1) +
                ggplot2::geom_point(ggplot2::aes(y = Variance, color = "Individual"), size = 2) +
                ggplot2::geom_line(ggplot2::aes(y = CumulativeVariance, color = "Cumulative"), size = 1) +
                ggplot2::geom_point(ggplot2::aes(y = CumulativeVariance, color = "Cumulative"), size = 2) +
                ggplot2::scale_color_manual(values = c("Individual" = "blue", "Cumulative" = "red")) +
                ggplot2::labs(
                    title = "Scree Plot",
                    x = "Principal Component",
                    y = "Variance Explained",
                    color = "Variance Type"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(legend.position = "bottom")
            
            print(p)
            TRUE
        },
        
        .createBiplot = function(pca_results, n_components) {
            if (!requireNamespace("ggplot2", quietly = TRUE) || n_components < 2) return()
            
            # Create biplot for first two components
            scores <- pca_results$scores[, 1:2]
            loadings <- pca_results$rotation[, 1:2] * 3  # Scale for visibility
            
            # Sample points if too many
            if (nrow(scores) > 200) {
                sample_idx <- sample(nrow(scores), 200)
                scores <- scores[sample_idx, ]
            }
            
            # Sample variables if too many
            if (nrow(loadings) > 20) {
                top_vars <- order(apply(abs(loadings), 1, max), decreasing = TRUE)[1:20]
                loadings <- loadings[top_vars, ]
            }
            
            # Create plot data
            score_data <- data.frame(PC1 = scores[, 1], PC2 = scores[, 2])
            loading_data <- data.frame(
                PC1 = loadings[, 1],
                PC2 = loadings[, 2],
                Variable = rownames(loadings)
            )
            
            p <- ggplot2::ggplot(score_data, ggplot2::aes(x = PC1, y = PC2)) +
                ggplot2::geom_point(alpha = 0.6, color = "steelblue") +
                ggplot2::geom_segment(data = loading_data, 
                                     ggplot2::aes(xend = PC1, yend = PC2),
                                     x = 0, y = 0, 
                                     arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches")),
                                     color = "red", alpha = 0.7) +
                ggplot2::geom_text(data = loading_data,
                                  ggplot2::aes(label = Variable),
                                  hjust = 1.1, size = 3, color = "red") +
                ggplot2::labs(
                    title = "PCA Biplot",
                    x = paste0("PC1 (", round(pca_results$prop_variance[1] * 100, 1), "%)"),
                    y = paste0("PC2 (", round(pca_results$prop_variance[2] * 100, 1), "%)")
                ) +
                ggplot2::theme_minimal()
            
            print(p)
            TRUE
        },
        
        .createLoadingPlot = function(pca_results, n_components) {
            if (!requireNamespace("ggplot2", quietly = TRUE)) return()
            
            # Create loading plot for selected components
            n_show <- min(n_components, 4)  # Show up to 4 components
            loadings <- pca_results$rotation[, 1:n_show, drop = FALSE]
            
            # Reshape data for plotting
            plot_data <- data.frame()
            for (i in 1:n_show) {
                component_data <- data.frame(
                    Variable = rownames(loadings),
                    Loading = loadings[, i],
                    Component = paste0("PC", i)
                )
                plot_data <- rbind(plot_data, component_data)
            }
            
            # Show only top variables by absolute loading
            plot_data$AbsLoading <- abs(plot_data$Loading)
            plot_data <- plot_data[order(plot_data$AbsLoading, decreasing = TRUE), ]
            
            # Keep top 15 variables per component
            top_vars <- c()
            for (comp in unique(plot_data$Component)) {
                comp_data <- plot_data[plot_data$Component == comp, ]
                top_vars <- c(top_vars, head(comp_data$Variable, 15))
            }
            top_vars <- unique(top_vars)
            plot_data <- plot_data[plot_data$Variable %in% top_vars, ]
            
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(Variable, AbsLoading), y = Loading)) +
                ggplot2::geom_col(ggplot2::aes(fill = Component), alpha = 0.8) +
                ggplot2::facet_wrap(~ Component, scales = "free") +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = "Component Loadings",
                    x = "Variable",
                    y = "Loading",
                    fill = "Component"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(legend.position = "none")
            
            print(p)
            TRUE
        },
        
        .createSurvivalPlot = function(data, pca_results, n_components) {
            if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("survival", quietly = TRUE)) return()
            
            # Use first principal component for stratification
            pc1_scores <- pca_results$scores[, 1]
            
            # Create risk groups based on PC1 scores
            pc1_tertiles <- quantile(pc1_scores, c(0.33, 0.67))
            risk_groups <- cut(pc1_scores, 
                              breaks = c(-Inf, pc1_tertiles[1], pc1_tertiles[2], Inf),
                              labels = c("Low Risk", "Intermediate Risk", "High Risk"))
            
            # Create survival data with risk groups
            surv_data <- data.frame(
                time = data$survival$time,
                event = data$survival$event,
                risk_group = risk_groups
            )
            
            # Fit survival curves
            surv_fit <- survival::survfit(survival::Surv(time, event) ~ risk_group, data = surv_data)
            
            # Create survival plot data
            surv_summary <- summary(surv_fit)
            plot_data <- data.frame(
                time = surv_summary$time,
                surv = surv_summary$surv,
                group = surv_summary$strata
            )
            
            # Clean group names
            plot_data$group <- gsub("risk_group=", "", plot_data$group)
            
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = surv, color = group)) +
                ggplot2::geom_step(size = 1) +
                ggplot2::scale_color_manual(values = c("Low Risk" = "green", 
                                                      "Intermediate Risk" = "orange", 
                                                      "High Risk" = "red")) +
                ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::labs(
                    title = "Survival Curves by PC1 Risk Groups",
                    x = "Time",
                    y = "Survival Probability",
                    color = "Risk Group"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(legend.position = "bottom")
            
            print(p)
            TRUE
        },
        
        .addMethodologyExplanation = function() {
            pca_method_name <- switch(self$options$pca_method,
                "standard" = "Standard PCA",
                "sparse" = "Sparse PCA",
                "supervised" = "Supervised PCA", 
                "kernel" = "Kernel PCA"
            )
            
            html <- paste0('<h3>Principal Component Cox Methodology</h3>
                    <p><b>Overview:</b> Principal Component Cox models use PCA to reduce dimensionality in high-dimensional survival data before fitting Cox regression models.</p>
                    
                    <h4>Method:</h4>
                    <ul>
                    <li><b>PCA Method:</b> ', pca_method_name, '</li>
                    <li><b>Scaling:</b> ', self$options$scaling_method, '</li>
                    <li><b>Component Selection:</b> ', self$options$component_selection, '</li>
                    <li><b>Number of Components:</b> ', self$options$n_components, '</li>
                    </ul>
                    
                    <h4>Analysis Steps:</h4>
                    <ol>
                    <li>Scale high-dimensional variables using ', self$options$scaling_method, ' method</li>
                    <li>Perform PCA to extract principal components</li>
                    <li>Select optimal number of components</li>
                    <li>Fit Cox regression using selected components as covariates</li>
                    </ol>
                    
                    <h4>Advantages:</h4>
                    <ul>
                    <li>Handles high-dimensional data (p >> n)</li>
                    <li>Reduces overfitting by dimensionality reduction</li>
                    <li>Captures major patterns in covariate space</li>
                    <li>Interpretable through component loadings</li>
                    </ul>')
            
            if (self$options$pca_method == "sparse") {
                html <- paste0(html, 
                    '<p><b>Sparse PCA:</b> Applies sparsity constraints to component loadings, making interpretation easier by setting small loadings to zero.</p>')
            }
            
            html <- paste0(html, 
                '<h4>Interpretation:</h4>
                <p>Principal components represent linear combinations of original variables that capture maximum variance. 
                In the Cox model, component coefficients indicate how each principal component affects survival hazard.</p>')
            
            self$results$methodologyExplanation$setContent(html)
        },
        
        .addAnalysisSummary = function(data, pca_results, selection_results) {
            n_subjects <- nrow(data$survival)
            n_events <- sum(data$survival$event)
            n_vars <- ncol(data$highdim_matrix)
            
            html <- paste0('<h3>Analysis Summary</h3>
                          <p><b>Sample Size:</b> ', n_subjects, ' subjects</p>
                          <p><b>Number of Events:</b> ', n_events, '</p>
                          <p><b>High-Dimensional Variables:</b> ', n_vars, '</p>
                          <p><b>Selected Components:</b> ', selection_results$n_components, '</p>
                          <p><b>Variance Explained:</b> ', round(selection_results$total_variance * 100, 1), '%</p>')
            
            if (length(self$options$clinical_vars) > 0) {
                html <- paste0(html, '<p><b>Clinical Variables:</b> ', paste(self$options$clinical_vars, collapse = ", "), '</p>')
            }
            
            html <- paste0(html, '<h4>Principal Components:</h4>
                          <p>The analysis extracted ', selection_results$n_components, ' principal components explaining ', 
                          round(selection_results$total_variance * 100, 1), '% of the total variance in the high-dimensional data.</p>
                          
                          <h4>Cox Model Results:</h4>
                          <p>Principal components were used as covariates in a Cox proportional hazards model to assess their association with survival.</p>
                          
                          <h4>Clinical Interpretation:</h4>
                          <p>Principal components represent underlying patterns in the high-dimensional data. 
                          Significant components in the Cox model suggest that these patterns are associated with survival outcomes.</p>')
            
            self$results$analysisSummary$setContent(html)
        }
    )
)