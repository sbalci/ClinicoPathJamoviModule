
# This file is a generated template, your changes will not be overwritten

# Social Science Statistical Visualization using sjPlot package
# Following jamovi naming convention with j-prefix to avoid namespace conflicts

jsjplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jsjplotClass",
    inherit = jsjplotBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .cached_model = NULL,
        .cached_plot = NULL,
        .cached_table = NULL,
        .cached_statistics = NULL,
        .cached_summary = NULL,
        
        .init = function() {
            # Initialize with appropriate plot dimensions
            self$results$plot$setSize(800, 600)
            self$results$model_table$setVisible(TRUE)
            self$results$statistics$setVisible(TRUE)
            self$results$summary$setVisible(TRUE)
        },
        
        # Performance optimization methods
        .calculateDataHash = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(NULL)
            }
            
            # Create hash based on relevant data columns and analysis type
            analysis_type <- self$options$analysis_type
            
            relevant_vars <- c(self$options$dependent_var, 
                             self$options$independent_vars,
                             self$options$grouping_var,
                             self$options$interaction_vars)
            relevant_vars <- relevant_vars[!sapply(relevant_vars, is.null)]
            
            if (length(relevant_vars) == 0 && !analysis_type %in% c("frequency_table", "correlation_matrix", "pca_plot")) {
                return(NULL)
            }
            
            # For frequency/correlation/PCA, use all variables
            if (analysis_type %in% c("frequency_table", "correlation_matrix", "pca_plot")) {
                relevant_vars <- names(self$data)
            }
            
            # Create a simple hash string
            data_summary <- paste(
                analysis_type,
                nrow(self$data),
                ncol(self$data),
                paste(relevant_vars, collapse = "_"),
                if (length(relevant_vars) > 0) {
                    paste(sapply(relevant_vars, function(var) {
                        if (var %in% names(self$data)) {
                            if (is.numeric(self$data[[var]])) {
                                paste(range(self$data[[var]], na.rm = TRUE), collapse = "_")
                            } else {
                                paste(length(unique(self$data[[var]])), "levels")
                            }
                        } else {
                            "missing"
                        }
                    }), collapse = "_")
                } else {
                    "no_vars"
                },
                sep = "_"
            )
            
            return(data_summary)
        },
        
        .calculateOptionsHash = function() {
            # Create hash of all relevant options
            options_list <- list(
                analysis_type = self$options$analysis_type,
                model_type = self$options$model_type,
                family = self$options$family,
                plot_type = self$options$plot_type,
                confidence_level = self$options$confidence_level,
                standardized = self$options$standardized,
                show_values = self$options$show_values,
                show_p_values = self$options$show_p_values,
                sort_estimates = self$options$sort_estimates,
                remove_intercept = self$options$remove_intercept,
                grid_breaks = self$options$grid_breaks,
                dot_size = self$options$dot_size,
                line_size = self$options$line_size,
                colors = self$options$colors,
                theme_style = self$options$theme_style,
                title = self$options$title,
                axis_labels = self$options$axis_labels,
                transform_axis = self$options$transform_axis,
                show_data = self$options$show_data,
                show_statistics = self$options$show_statistics,
                show_summary = self$options$show_summary,
                html_output = self$options$html_output
            )
            
            return(paste(options_list, collapse = "_"))
        },
        
        .canUseCache = function() {
            current_data_hash <- private$.calculateDataHash()
            current_options_hash <- private$.calculateOptionsHash()
            
            return(!is.null(private$.data_hash) &&
                   !is.null(private$.options_hash) &&
                   !is.null(current_data_hash) &&
                   !is.null(current_options_hash) &&
                   current_data_hash == private$.data_hash &&
                   current_options_hash == private$.options_hash)
        },
        
        .prepareData = function() {
            current_hash <- private$.calculateDataHash()
            
            if (is.null(private$.data_hash) || private$.data_hash != current_hash) {
                # Data has changed, prepare new data
                data <- self$data
                
                # Basic data validation
                if (is.null(data) || nrow(data) == 0) {
                    private$.prepared_data <- NULL
                    private$.data_hash <- current_hash
                    return(NULL)
                }
                
                # Store cleaned data
                private$.prepared_data <- data
                private$.data_hash <- current_hash
            }
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            current_hash <- private$.calculateOptionsHash()
            
            if (is.null(private$.options_hash) || private$.options_hash != current_hash) {
                # Options have changed, update cache
                private$.prepared_options <- self$options
                private$.options_hash <- current_hash
                
                # Clear model cache when options change
                private$.cached_model <- NULL
            }
            
            return(private$.prepared_options)
        },
        
        .canUseModelCache = function() {
            # Check if we can reuse the fitted model
            return(!is.null(private$.cached_model) && 
                   private$.canUseCache())
        },
        
        .run = function() {
            
            # Check if sjPlot package is available
            if (!requireNamespace('sjPlot', quietly = TRUE)) {
                stop('The sjPlot package is required but not installed. Please install it using install.packages("sjPlot")')
            }
            
            # Performance optimization: check if we can use cached results
            if (private$.canUseCache()) {
                # Use cached results
                if (!is.null(private$.cached_plot)) {
                    self$results$plot$setState(private$.cached_plot)
                }
                if (!is.null(private$.cached_table)) {
                    self$results$model_table$setContent(private$.cached_table)
                }
                if (!is.null(private$.cached_statistics)) {
                    self$results$statistics$setContent(private$.cached_statistics)
                }
                if (!is.null(private$.cached_summary)) {
                    self$results$summary$setContent(private$.cached_summary)
                }
                return()
            }
            
            # Prepare data and options with caching
            data <- private$.prepareData()
            options <- private$.prepareOptions()
            
            # Check for minimum required data
            if (is.null(data) || nrow(data) == 0) {
                return()
            }
            
            # Get analysis type
            analysis_type <- options$analysis_type
            
            # Execute analysis based on type with caching
            if (analysis_type == 'regression_table') {
                self$.runRegressionTable(data)
            } else if (analysis_type == 'coefficient_plot') {
                self$.runCoefficientPlot(data)
            } else if (analysis_type == 'interaction_plot') {
                self$.runInteractionPlot(data)
            } else if (analysis_type == 'marginal_effects') {
                self$.runMarginalEffects(data)
            } else if (analysis_type == 'frequency_table') {
                self$.runFrequencyTable(data)
            } else if (analysis_type == 'correlation_matrix') {
                self$.runCorrelationMatrix(data)
            } else if (analysis_type == 'pca_plot') {
                self$.runPCAPlot(data)
            }
            
            # Cache the results for future use
            private$.cached_plot <- self$results$plot$state
            private$.cached_table <- if (self$results$model_table$visible) self$results$model_table$content else NULL
            private$.cached_statistics <- if (self$results$statistics$visible) self$results$statistics$content else NULL
            private$.cached_summary <- if (self$results$summary$visible) self$results$summary$content else NULL
        },
        
        .runRegressionTable = function(data) {
            
            options <- self$options
            
            # Check required variables
            if (is.null(options$dependent_var) || length(options$independent_vars) == 0) {
                return()
            }
            
            tryCatch({
                
                # Build and fit model
                model <- self$.buildModel(data)
                
                if (is.null(model)) {
                    return()
                }
                
                # Generate regression table using sjPlot
                table_output <- self$.jtab_model(model)
                
                # Set table output
                self$results$model_table$setContent(table_output)
                
                # Generate model statistics
                stats_output <- self$.jmodel_statistics(model)
                self$results$statistics$setContent(stats_output)
                
                # Generate summary
                summary_output <- self$.jmodel_summary(model, data)
                self$results$summary$setContent(summary_output)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in regression table analysis:', e$message))
            })
        },
        
        .runCoefficientPlot = function(data) {
            
            options <- self$options
            
            # Check required variables
            if (is.null(options$dependent_var) || length(options$independent_vars) == 0) {
                return()
            }
            
            tryCatch({
                
                # Build and fit model
                model <- self$.buildModel(data)
                
                if (is.null(model)) {
                    return()
                }
                
                # Generate coefficient plot using sjPlot
                plot_obj <- self$.jplot_model(model, type = "est")
                
                # Set plot
                self$results$plot$setState(plot_obj)
                
                # Generate model statistics
                stats_output <- self$.jmodel_statistics(model)
                self$results$statistics$setContent(stats_output)
                
                # Generate summary
                summary_output <- self$.jmodel_summary(model, data)
                self$results$summary$setContent(summary_output)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in coefficient plot analysis:', e$message))
            })
        },
        
        .runInteractionPlot = function(data) {
            
            options <- self$options
            
            # Check required variables
            if (is.null(options$dependent_var) || length(options$interaction_vars) < 2) {
                return()
            }
            
            tryCatch({
                
                # Build interaction model
                model <- self$.buildInteractionModel(data)
                
                if (is.null(model)) {
                    return()
                }
                
                # Generate interaction plot
                plot_obj <- self$.jplot_model(model, type = "int")
                
                # Set plot
                self$results$plot$setState(plot_obj)
                
                # Generate model statistics
                stats_output <- self$.jmodel_statistics(model)
                self$results$statistics$setContent(stats_output)
                
                # Generate summary
                summary_output <- self$.jmodel_summary(model, data)
                self$results$summary$setContent(summary_output)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in interaction plot analysis:', e$message))
            })
        },
        
        .runMarginalEffects = function(data) {
            
            options <- self$options
            
            # Check required variables
            if (is.null(options$dependent_var) || length(options$independent_vars) == 0) {
                return()
            }
            
            tryCatch({
                
                # Build model
                model <- self$.buildModel(data)
                
                if (is.null(model)) {
                    return()
                }
                
                # Generate marginal effects plot
                plot_obj <- self$.jplot_model(model, type = "eff")
                
                # Set plot
                self$results$plot$setState(plot_obj)
                
                # Generate model statistics
                stats_output <- self$.jmodel_statistics(model)
                self$results$statistics$setContent(stats_output)
                
                # Generate summary
                summary_output <- self$.jmodel_summary(model, data)
                self$results$summary$setContent(summary_output)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in marginal effects analysis:', e$message))
            })
        },
        
        .runFrequencyTable = function(data) {
            
            options <- self$options
            
            # Check for categorical variables
            categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
            
            if (length(categorical_vars) == 0) {
                jmvcore::reject('No categorical variables found for frequency analysis')
                return()
            }
            
            tryCatch({
                
                # Generate frequency tables
                freq_output <- self$.jfreq_tables(data, categorical_vars)
                
                # Set table output
                self$results$model_table$setContent(freq_output)
                
                # Simple summary for frequency tables
                summary_text <- paste(
                    "<h3>Frequency Analysis Summary</h3>",
                    "<p>Analysis includes", length(categorical_vars), "categorical variables.</p>",
                    "<p>Total observations:", nrow(data), "</p>"
                )
                
                self$results$summary$setContent(summary_text)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in frequency table analysis:', e$message))
            })
        },
        
        .runCorrelationMatrix = function(data) {
            
            # Get numeric variables
            numeric_vars <- names(data)[sapply(data, is.numeric)]
            
            if (length(numeric_vars) < 2) {
                jmvcore::reject('At least 2 numeric variables required for correlation analysis')
                return()
            }
            
            tryCatch({
                
                # Generate correlation matrix plot
                plot_obj <- self$.jcorrelation_matrix(data[numeric_vars])
                
                # Set plot
                self$results$plot$setState(plot_obj)
                
                # Generate correlation table
                cor_table <- self$.jcorrelation_table(data[numeric_vars])
                self$results$model_table$setContent(cor_table)
                
                # Generate summary
                summary_text <- paste(
                    "<h3>Correlation Analysis Summary</h3>",
                    "<p>Correlation matrix for", length(numeric_vars), "numeric variables.</p>",
                    "<p>Sample size:", nrow(data), "observations.</p>"
                )
                
                self$results$summary$setContent(summary_text)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in correlation analysis:', e$message))
            })
        },
        
        .runPCAPlot = function(data) {
            
            # Get numeric variables
            numeric_vars <- names(data)[sapply(data, is.numeric)]
            
            if (length(numeric_vars) < 3) {
                jmvcore::reject('At least 3 numeric variables required for PCA analysis')
                return()
            }
            
            tryCatch({
                
                # Perform PCA
                pca_result <- self$.jperform_pca(data[numeric_vars])
                
                # Generate PCA plot
                plot_obj <- self$.jpca_plot(pca_result)
                
                # Set plot
                self$results$plot$setState(plot_obj)
                
                # Generate PCA summary table
                pca_table <- self$.jpca_summary_table(pca_result)
                self$results$model_table$setContent(pca_table)
                
                # Generate summary
                summary_text <- self$.jpca_interpretation(pca_result, length(numeric_vars))
                self$results$summary$setContent(summary_text)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in PCA analysis:', e$message))
            })
        },
        
        # sjPlot wrapper functions with j-prefix to avoid namespace conflicts
        .buildModel = function(data) {
            
            # Performance optimization: check if we can reuse cached model
            if (private$.canUseModelCache()) {
                return(private$.cached_model)
            }
            
            options <- self$options
            dep_var <- options$dependent_var
            indep_vars <- options$independent_vars
            model_type <- options$model_type
            
            # Validate variables exist
            if (is.null(dep_var) || length(indep_vars) == 0) {
                return(NULL)
            }
            
            all_vars <- c(dep_var, indep_vars)
            missing_vars <- setdiff(all_vars, names(data))
            if (length(missing_vars) > 0) {
                jmvcore::reject(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
                return(NULL)
            }
            
            # Clean data
            model_data <- data[all_vars]
            model_data <- model_data[complete.cases(model_data), ]
            
            if (nrow(model_data) == 0) {
                jmvcore::reject("No complete cases available for model fitting after removing missing values")
                return(NULL)
            }
            
            if (nrow(model_data) < length(indep_vars) + 2) {
                jmvcore::reject("Insufficient data points for model fitting. Need more observations than predictors.")
                return(NULL)
            }
            
            tryCatch({
                # Build formula
                formula_str <- paste(dep_var, "~", paste(indep_vars, collapse = " + "))
                model_formula <- as.formula(formula_str)
                
                # Fit model based on type
                if (model_type == "lm") {
                    model <- lm(model_formula, data = model_data)
                } else if (model_type == "glm") {
                    family_name <- options$family
                    family_obj <- switch(family_name,
                        "gaussian" = gaussian(),
                        "binomial" = binomial(),
                        "poisson" = poisson(),
                        "gamma" = Gamma(),
                        gaussian()
                    )
                    model <- glm(model_formula, data = model_data, family = family_obj)
                } else if (model_type == "logistic") {
                    model <- glm(model_formula, data = model_data, family = binomial())
                } else if (model_type == "poisson") {
                    model <- glm(model_formula, data = model_data, family = poisson())
                } else if (model_type == "lmer") {
                    # Check if lme4 is available for mixed models
                    if (!requireNamespace('lme4', quietly = TRUE)) {
                        jmvcore::reject('lme4 package required for mixed effects models. Please install it using install.packages("lme4")')
                        return(NULL)
                    }
                    # For now, fallback to lm - proper mixed model implementation would need grouping structure
                    model <- lm(model_formula, data = model_data)
                } else {
                    # Default to linear model
                    model <- lm(model_formula, data = model_data)
                }
                
                # Cache the model
                private$.cached_model <- model
                
                return(model)
                
            }, error = function(e) {
                jmvcore::reject(paste("Error fitting model:", e$message))
                return(NULL)
            })
        },
        
        .buildInteractionModel = function(data) {
            
            options <- self$options
            dep_var <- options$dependent_var
            int_vars <- options$interaction_vars[1:2]  # Take first two
            other_vars <- setdiff(options$independent_vars, int_vars)
            
            # Build interaction formula
            int_term <- paste(int_vars[1], "*", int_vars[2])
            other_terms <- if (length(other_vars) > 0) paste(other_vars, collapse = " + ") else ""
            
            if (other_terms != "") {
                formula_str <- paste(dep_var, "~", int_term, "+", other_terms)
            } else {
                formula_str <- paste(dep_var, "~", int_term)
            }
            
            model_formula <- as.formula(formula_str)
            
            # Clean data
            all_vars <- c(dep_var, int_vars, other_vars)
            model_data <- data[all_vars]
            model_data <- model_data[complete.cases(model_data), ]
            
            if (nrow(model_data) == 0) {
                return(NULL)
            }
            
            # Fit model
            model <- lm(model_formula, data = model_data)
            
            return(model)
        },
        
        .jtab_model = function(model) {
            
            options <- self$options
            
            # Generate model table using sjPlot::tab_model with j-prefix wrapper
            if (options$html_output) {
                table_output <- sjPlot::tab_model(
                    model,
                    show.p = options$show_p_values,
                    show.std = options$standardized,
                    show.ci = TRUE,
                    ci.lvl = options$confidence_level,
                    file = NULL
                )
                
                # Convert to HTML string
                return(as.character(table_output))
            } else {
                # Generate simple table
                model_summary <- summary(model)
                coefs <- model_summary$coefficients
                
                # Create HTML table
                html_table <- "<table style='border-collapse: collapse; width: 100%;'>"
                html_table <- paste0(html_table, "<tr style='background-color: #f2f2f2;'>")
                html_table <- paste0(html_table, "<th style='border: 1px solid #ddd; padding: 8px;'>Term</th>")
                html_table <- paste0(html_table, "<th style='border: 1px solid #ddd; padding: 8px;'>Estimate</th>")
                html_table <- paste0(html_table, "<th style='border: 1px solid #ddd; padding: 8px;'>Std. Error</th>")
                html_table <- paste0(html_table, "<th style='border: 1px solid #ddd; padding: 8px;'>t value</th>")
                html_table <- paste0(html_table, "<th style='border: 1px solid #ddd; padding: 8px;'>p value</th>")
                html_table <- paste0(html_table, "</tr>")
                
                for (i in 1:nrow(coefs)) {
                    html_table <- paste0(html_table, "<tr>")
                    html_table <- paste0(html_table, "<td style='border: 1px solid #ddd; padding: 8px;'>", rownames(coefs)[i], "</td>")
                    html_table <- paste0(html_table, "<td style='border: 1px solid #ddd; padding: 8px;'>", round(coefs[i, 1], 4), "</td>")
                    html_table <- paste0(html_table, "<td style='border: 1px solid #ddd; padding: 8px;'>", round(coefs[i, 2], 4), "</td>")
                    html_table <- paste0(html_table, "<td style='border: 1px solid #ddd; padding: 8px;'>", round(coefs[i, 3], 3), "</td>")
                    html_table <- paste0(html_table, "<td style='border: 1px solid #ddd; padding: 8px;'>", 
                                        if (coefs[i, 4] < 0.001) "< 0.001" else round(coefs[i, 4], 3), "</td>")
                    html_table <- paste0(html_table, "</tr>")
                }
                
                html_table <- paste0(html_table, "</table>")
                
                return(html_table)
            }
        },
        
        .jplot_model = function(model, type = "est") {
            
            options <- self$options
            
            # Generate plot using sjPlot::plot_model with j-prefix wrapper
            plot_obj <- sjPlot::plot_model(
                model,
                type = type,
                show.values = options$show_values,
                show.p = options$show_p_values,
                sort.est = options$sort_estimates,
                rm.terms = if (options$remove_intercept) "(Intercept)" else NULL,
                grid.breaks = options$grid_breaks,
                dot.size = options$dot_size,
                line.size = options$line_size,
                colors = options$colors,
                title = if (options$title != "") options$title else NULL,
                axis.labels = if (options$axis_labels != "") strsplit(options$axis_labels, ",")[[1]] else NULL,
                transform = options$transform_axis
            )
            
            # Apply theme
            plot_obj <- plot_obj + self$.japply_sjplot_theme(options$theme_style)
            
            return(plot_obj)
        },
        
        .jmodel_statistics = function(model) {
            
            # Extract model statistics
            model_summary <- summary(model)
            
            if (inherits(model, "lm")) {
                r_squared <- model_summary$r.squared
                adj_r_squared <- model_summary$adj.r.squared
                f_stat <- model_summary$fstatistic
                n_obs <- nobs(model)
                
                stats_html <- paste(
                    "<h3>Model Fit Statistics</h3>",
                    "<ul>",
                    paste0("<li>R-squared: ", round(r_squared, 4), "</li>"),
                    paste0("<li>Adjusted R-squared: ", round(adj_r_squared, 4), "</li>"),
                    paste0("<li>F-statistic: ", round(f_stat[1], 3), " on ", f_stat[2], " and ", f_stat[3], " DF</li>"),
                    paste0("<li>Number of observations: ", n_obs, "</li>"),
                    "</ul>"
                )
            } else if (inherits(model, "glm")) {
                aic <- AIC(model)
                bic <- BIC(model)
                deviance <- deviance(model)
                n_obs <- nobs(model)
                
                stats_html <- paste(
                    "<h3>Model Fit Statistics</h3>",
                    "<ul>",
                    paste0("<li>AIC: ", round(aic, 2), "</li>"),
                    paste0("<li>BIC: ", round(bic, 2), "</li>"),
                    paste0("<li>Deviance: ", round(deviance, 2), "</li>"),
                    paste0("<li>Number of observations: ", n_obs, "</li>"),
                    "</ul>"
                )
            } else {
                stats_html <- "<h3>Model Fit Statistics</h3><p>Statistics not available for this model type.</p>"
            }
            
            return(stats_html)
        },
        
        .jmodel_summary = function(model, data) {
            
            # Generate comprehensive model summary
            options <- self$options
            model_summary <- summary(model)
            
            # Basic information
            summary_html <- paste(
                "<h3>Model Summary</h3>",
                "<p><strong>Model Type:</strong>", class(model)[1], "</p>",
                "<p><strong>Formula:</strong>", deparse(formula(model)), "</p>",
                "<p><strong>Sample Size:</strong>", nobs(model), "observations</p>"
            )
            
            # Add interpretation based on model type
            if (inherits(model, "lm")) {
                r_sq <- summary(model)$r.squared
                interpretation <- if (r_sq > 0.7) "Strong" else if (r_sq > 0.3) "Moderate" else "Weak"
                
                summary_html <- paste(summary_html,
                    "<p><strong>Model Interpretation:</strong>", interpretation, 
                    "relationship detected (R² =", round(r_sq, 3), ")</p>"
                )
            }
            
            # Add variable information
            n_predictors <- length(coefficients(model)) - 1  # Exclude intercept
            summary_html <- paste(summary_html,
                "<p><strong>Number of Predictors:</strong>", n_predictors, "</p>"
            )
            
            return(summary_html)
        },
        
        .jfreq_tables = function(data, categorical_vars) {
            
            # Generate frequency tables for categorical variables
            freq_html <- "<h3>Frequency Tables</h3>"
            
            for (var in categorical_vars[1:min(5, length(categorical_vars))]) {  # Limit to first 5
                freq_table <- table(data[[var]], useNA = "ifany")
                prop_table <- prop.table(freq_table) * 100
                
                freq_html <- paste0(freq_html, "<h4>", var, "</h4>")
                freq_html <- paste0(freq_html, "<table style='border-collapse: collapse; margin-bottom: 20px;'>")
                freq_html <- paste0(freq_html, "<tr style='background-color: #f2f2f2;'>")
                freq_html <- paste0(freq_html, "<th style='border: 1px solid #ddd; padding: 8px;'>Level</th>")
                freq_html <- paste0(freq_html, "<th style='border: 1px solid #ddd; padding: 8px;'>Frequency</th>")
                freq_html <- paste0(freq_html, "<th style='border: 1px solid #ddd; padding: 8px;'>Percentage</th>")
                freq_html <- paste0(freq_html, "</tr>")
                
                for (level in names(freq_table)) {
                    freq_html <- paste0(freq_html, "<tr>")
                    freq_html <- paste0(freq_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", level, "</td>")
                    freq_html <- paste0(freq_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", freq_table[level], "</td>")
                    freq_html <- paste0(freq_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", round(prop_table[level], 1), "%</td>")
                    freq_html <- paste0(freq_html, "</tr>")
                }
                
                freq_html <- paste0(freq_html, "</table>")
            }
            
            return(freq_html)
        },
        
        .jcorrelation_matrix = function(data) {
            
            if (!requireNamespace('ggplot2', quietly = TRUE)) {
                return(NULL)
            }
            
            # Compute correlation matrix
            cor_matrix <- cor(data, use = "complete.obs")
            
            # Create correlation plot using ggplot2
            cor_data <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
            cor_data$value <- as.vector(cor_matrix)
            
            p <- ggplot2::ggplot(cor_data, ggplot2::aes(Var1, Var2, fill = value)) +
                ggplot2::geom_tile() +
                ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
                ggplot2::theme_minimal() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggplot2::labs(title = "Correlation Matrix", x = "", y = "", fill = "Correlation")
            
            return(p)
        },
        
        .jcorrelation_table = function(data) {
            
            # Generate correlation table
            cor_matrix <- cor(data, use = "complete.obs")
            
            # Create HTML table
            cor_html <- "<h3>Correlation Matrix</h3>"
            cor_html <- paste0(cor_html, "<table style='border-collapse: collapse;'>")
            
            # Header row
            cor_html <- paste0(cor_html, "<tr style='background-color: #f2f2f2;'>")
            cor_html <- paste0(cor_html, "<th style='border: 1px solid #ddd; padding: 8px;'>Variable</th>")
            for (var in colnames(cor_matrix)) {
                cor_html <- paste0(cor_html, "<th style='border: 1px solid #ddd; padding: 8px;'>", var, "</th>")
            }
            cor_html <- paste0(cor_html, "</tr>")
            
            # Data rows
            for (i in 1:nrow(cor_matrix)) {
                cor_html <- paste0(cor_html, "<tr>")
                cor_html <- paste0(cor_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", rownames(cor_matrix)[i], "</td>")
                for (j in 1:ncol(cor_matrix)) {
                    cor_val <- round(cor_matrix[i, j], 3)
                    cor_html <- paste0(cor_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", cor_val, "</td>")
                }
                cor_html <- paste0(cor_html, "</tr>")
            }
            
            cor_html <- paste0(cor_html, "</table>")
            
            return(cor_html)
        },
        
        .jperform_pca = function(data) {
            
            # Perform PCA
            pca_result <- prcomp(data, scale. = TRUE, center = TRUE)
            
            return(pca_result)
        },
        
        .jpca_plot = function(pca_result) {
            
            if (!requireNamespace('ggplot2', quietly = TRUE)) {
                return(NULL)
            }
            
            # Create PCA biplot
            pca_data <- data.frame(
                PC1 = pca_result$x[, 1],
                PC2 = pca_result$x[, 2]
            )
            
            p <- ggplot2::ggplot(pca_data, ggplot2::aes(PC1, PC2)) +
                ggplot2::geom_point(alpha = 0.6) +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                    title = "PCA Biplot",
                    x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "% variance)"),
                    y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "% variance)")
                )
            
            return(p)
        },
        
        .jpca_summary_table = function(pca_result) {
            
            # Generate PCA summary table
            importance <- summary(pca_result)$importance
            
            pca_html <- "<h3>PCA Summary</h3>"
            pca_html <- paste0(pca_html, "<table style='border-collapse: collapse;'>")
            pca_html <- paste0(pca_html, "<tr style='background-color: #f2f2f2;'>")
            pca_html <- paste0(pca_html, "<th style='border: 1px solid #ddd; padding: 8px;'>Component</th>")
            pca_html <- paste0(pca_html, "<th style='border: 1px solid #ddd; padding: 8px;'>Standard Deviation</th>")
            pca_html <- paste0(pca_html, "<th style='border: 1px solid #ddd; padding: 8px;'>Proportion of Variance</th>")
            pca_html <- paste0(pca_html, "<th style='border: 1px solid #ddd; padding: 8px;'>Cumulative Proportion</th>")
            pca_html <- paste0(pca_html, "</tr>")
            
            n_components <- min(5, ncol(importance))  # Show first 5 components
            for (i in 1:n_components) {
                pca_html <- paste0(pca_html, "<tr>")
                pca_html <- paste0(pca_html, "<td style='border: 1px solid #ddd; padding: 8px;'>PC", i, "</td>")
                pca_html <- paste0(pca_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", round(importance[1, i], 3), "</td>")
                pca_html <- paste0(pca_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", round(importance[2, i], 3), "</td>")
                pca_html <- paste0(pca_html, "<td style='border: 1px solid #ddd; padding: 8px;'>", round(importance[3, i], 3), "</td>")
                pca_html <- paste0(pca_html, "</tr>")
            }
            
            pca_html <- paste0(pca_html, "</table>")
            
            return(pca_html)
        },
        
        .jpca_interpretation = function(pca_result, n_vars) {
            
            importance <- summary(pca_result)$importance
            pc1_var <- importance[2, 1] * 100
            pc2_var <- importance[2, 2] * 100
            total_var_pc12 <- importance[3, 2] * 100
            
            interpretation <- paste(
                "<h3>PCA Interpretation</h3>",
                "<p><strong>Dimensionality Reduction:</strong> Reduced", n_vars, "variables to principal components.</p>",
                "<p><strong>First Component (PC1):</strong> Explains", round(pc1_var, 1), "% of total variance.</p>",
                "<p><strong>Second Component (PC2):</strong> Explains", round(pc2_var, 1), "% of total variance.</p>",
                "<p><strong>Combined Explanation:</strong> First two components explain", round(total_var_pc12, 1), "% of total variance.</p>"
            )
            
            if (total_var_pc12 > 70) {
                interpretation <- paste(interpretation,
                    "<p style='color: #5cb85c;'><strong>Good dimensionality reduction:</strong> Most variance captured in first two components.</p>"
                )
            } else if (total_var_pc12 > 50) {
                interpretation <- paste(interpretation,
                    "<p style='color: #f0ad4e;'><strong>Moderate dimensionality reduction:</strong> Reasonable variance captured.</p>"
                )
            } else {
                interpretation <- paste(interpretation,
                    "<p style='color: #d9534f;'><strong>Limited dimensionality reduction:</strong> Consider more components or original variables.</p>"
                )
            }
            
            return(interpretation)
        },
        
        .japply_sjplot_theme = function(theme_style) {
            
            if (!requireNamespace('ggplot2', quietly = TRUE)) {
                return(ggplot2::theme_gray())
            }
            
            switch(theme_style,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "apa" = ggplot2::theme_minimal() + ggplot2::theme(
                    panel.grid = ggplot2::element_blank(),
                    axis.line = ggplot2::element_line(color = "black")
                ),
                "bw" = ggplot2::theme_bw(),
                ggplot2::theme_gray() # sjplot default
            )
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            
            # Render the plot stored in state
            plot_obj <- image$state
            
            if (!is.null(plot_obj)) {
                print(plot_obj)
                TRUE
            } else {
                FALSE
            }
        }
    )
)
