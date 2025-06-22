
# This file is a generated template, your changes will not be overwritten

jggstatsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jggstatsClass",
    inherit = jggstatsBase,
    private = list(
        .init = function() {
            if (is.null(self$data) || is.null(self$options$dependent_var)) {
                self$results$plot$setVisible(FALSE)
                return()
            }
        },
        
        .run = function() {
            # Early return if essential variables not specified
            if (is.null(self$options$dependent_var) && 
                !self$options$analysis_type %in% c("gglikert", "ggcascade", "stat_prop")) {
                return()
            }
            
            # Prepare data
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Clean variable names
            if (requireNamespace('janitor', quietly = TRUE)) {
                data <- janitor::clean_names(data)
            }
            
            # Generate plot based on analysis type
            plot <- self$.create_ggstats_plot(data)
            
            # Set plot
            self$results$plot$setState(plot)
            
            # Generate model table if requested
            if (self$options$output_format %in% c("model_table", "both")) {
                self$.populate_model_table(data)
            }
            
            # Generate model summary if requested
            if (self$options$show_model_summary) {
                self$.populate_model_summary(data)
            }
            
            # Generate interpretation if requested
            if (self$options$show_interpretation) {
                self$.generate_interpretation(data)
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            plot <- image$state
            print(plot)
            
            TRUE
        },
        
        .create_ggstats_plot = function(data) {
            # Load required packages
            packages <- c('ggplot2', 'ggstats', 'dplyr', 'broom')
            for (pkg in packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    stop(paste0("Package '", pkg, "' is required but not installed"))
                }
            }
            
            # Create plot based on analysis type
            plot <- switch(self$options$analysis_type,
                "ggcoef_model" = self$.create_ggcoef_model(data),
                "ggcoef_compare" = self$.create_ggcoef_compare(data),
                "gglikert" = self$.create_gglikert(data),
                "ggsurvey" = self$.create_ggsurvey(data),
                "stat_prop" = self$.create_stat_prop(data),
                "stat_cross" = self$.create_stat_cross(data),
                "stat_weighted_mean" = self$.create_weighted_mean(data),
                "ggcascade" = self$.create_ggcascade(data),
                self$.create_ggcoef_model(data)
            )
            
            # Apply customizations
            plot <- self$.apply_plot_customizations(plot)
            
            return(plot)
        },
        
        .create_ggcoef_model = function(data) {
            # Build formula
            formula <- self$.build_formula(data)
            
            # Fit model
            model <- self$.fit_model(data, formula)
            
            # Create coefficient plot using ggstats
            plot <- ggstats::ggcoef_model(
                model,
                intercept = self$options$show_intercept,
                sort = if(self$options$sort_coefficients) "ascending" else FALSE
            )
            
            return(plot)
        },
        
        .create_ggcoef_compare = function(data) {
            # Build multiple models for comparison
            models <- self$.build_comparison_models(data)
            
            # Create comparison plot
            plot <- ggstats::ggcoef_compare(
                models,
                intercept = self$options$show_intercept,
                sort = if(self$options$sort_coefficients) "ascending" else FALSE
            )
            
            return(plot)
        },
        
        .create_gglikert = function(data) {
            # Prepare Likert data
            likert_data <- self$.prepare_likert_data(data)
            
            # Create Likert plot
            plot <- ggstats::gglikert(
                likert_data,
                levels = self$options$likert_levels
            )
            
            return(plot)
        },
        
        .create_ggsurvey = function(data) {
            # Prepare survey data with weights
            survey_data <- self$.prepare_survey_data(data)
            
            # Create survey visualization
            plot <- ggplot2::ggplot(survey_data, ggplot2::aes_string(
                x = self$options$dependent_var,
                weight = self$options$weight_var
            )) +
            ggstats::stat_prop() +
            ggplot2::geom_bar()
            
            return(plot)
        },
        
        .create_stat_prop = function(data) {
            # Create proportion analysis plot
            plot <- ggplot2::ggplot(data, ggplot2::aes_string(
                x = self$options$dependent_var
            )) +
            ggstats::stat_prop() +
            ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(prop)))
            
            if (!is.null(self$options$grouping_var)) {
                plot <- plot + ggplot2::aes_string(fill = self$options$grouping_var)
            }
            
            return(plot)
        },
        
        .create_stat_cross = function(data) {
            # Create cross-tabulation plot
            if (is.null(self$options$grouping_var)) {
                stop("Grouping variable required for cross-tabulation")
            }
            
            plot <- ggplot2::ggplot(data, ggplot2::aes_string(
                x = self$options$dependent_var,
                fill = self$options$grouping_var
            )) +
            ggstats::stat_cross() +
            ggplot2::geom_bar(position = "fill")
            
            return(plot)
        },
        
        .create_weighted_mean = function(data) {
            # Create weighted mean plot
            if (is.null(self$options$weight_var)) {
                stop("Weight variable required for weighted means")
            }
            
            plot <- ggplot2::ggplot(data, ggplot2::aes_string(
                x = self$options$grouping_var,
                y = self$options$dependent_var,
                weight = self$options$weight_var
            )) +
            ggstats::stat_weighted_mean() +
            ggplot2::geom_point()
            
            return(plot)
        },
        
        .create_ggcascade = function(data) {
            # Create cascade plot for data filtering visualization
            cascade_data <- self$.prepare_cascade_data(data)
            
            plot <- ggstats::ggcascade(cascade_data)
            
            return(plot)
        },
        
        .build_formula = function(data) {
            if (nchar(self$options$model_formula) > 0) {
                return(as.formula(self$options$model_formula))
            }
            
            # Auto-build formula
            dependent <- self$options$dependent_var
            
            if (!is.null(self$options$independent_vars) && length(self$options$independent_vars) > 0) {
                independent <- paste(self$options$independent_vars, collapse = " + ")
                formula_str <- paste(dependent, "~", independent)
            } else {
                formula_str <- paste(dependent, "~ 1")
            }
            
            return(as.formula(formula_str))
        },
        
        .fit_model = function(data, formula) {
            # Fit model based on type
            model_type <- self$options$model_type
            family_type <- self$options$family
            
            model <- switch(model_type,
                "lm" = stats::lm(formula, data = data),
                "glm" = stats::glm(formula, data = data, family = family_type),
                "coxph" = {
                    if (requireNamespace('survival', quietly = TRUE)) {
                        survival::coxph(formula, data = data)
                    } else {
                        stop("survival package required for Cox regression")
                    }
                },
                "lmer" = {
                    if (requireNamespace('lme4', quietly = TRUE)) {
                        lme4::lmer(formula, data = data)
                    } else {
                        stop("lme4 package required for mixed effects models")
                    }
                },
                "glmer" = {
                    if (requireNamespace('lme4', quietly = TRUE)) {
                        lme4::glmer(formula, data = data, family = family_type)
                    } else {
                        stop("lme4 package required for generalized mixed effects models")
                    }
                },
                stats::lm(formula, data = data)
            )
            
            return(model)
        },
        
        .build_comparison_models = function(data) {
            # Build multiple models for comparison
            # This would need to be customized based on specific comparison needs
            formula <- self$.build_formula(data)
            
            # For demonstration, create two models
            model1 <- self$.fit_model(data, formula)
            
            # Second model with interaction if grouping variable exists
            if (!is.null(self$options$grouping_var)) {
                formula_str <- paste(as.character(formula)[2], "~", 
                                   as.character(formula)[3], "*", self$options$grouping_var)
                formula2 <- as.formula(formula_str)
                model2 <- self$.fit_model(data, formula2)
                return(list("Model 1" = model1, "Model 2" = model2))
            }
            
            return(list("Model" = model1))
        },
        
        .prepare_likert_data = function(data) {
            # Prepare data for Likert plot
            # This function would need customization based on data structure
            return(data)
        },
        
        .prepare_survey_data = function(data) {
            # Prepare survey data with proper weights
            return(data)
        },
        
        .prepare_cascade_data = function(data) {
            # Prepare data for cascade plot
            # This would show filtering steps
            return(data)
        },
        
        .apply_plot_customizations = function(plot) {
            # Apply theme
            theme_func <- switch(self$options$theme_style,
                "minimal" = ggplot2::theme_minimal(),
                "classic" = ggplot2::theme_classic(),
                "light" = ggplot2::theme_light(),
                "dark" = ggplot2::theme_dark(),
                ggplot2::theme_gray()
            )
            
            plot <- plot + theme_func
            
            # Apply color palette
            if (self$options$color_palette != "default") {
                colors <- switch(self$options$color_palette,
                    "viridis" = ggplot2::scale_color_viridis_d(),
                    "set1" = ggplot2::scale_color_brewer(type = "qual", palette = "Set1"),
                    "dark2" = ggplot2::scale_color_brewer(type = "qual", palette = "Dark2"),
                    "paired" = ggplot2::scale_color_brewer(type = "qual", palette = "Paired"),
                    ggplot2::scale_color_viridis_d()
                )
                plot <- plot + colors
            }
            
            # Add titles
            if (nchar(self$options$plot_title) > 0) {
                plot <- plot + ggplot2::ggtitle(self$options$plot_title)
            }
            
            if (nchar(self$options$plot_subtitle) > 0) {
                plot <- plot + ggplot2::labs(subtitle = self$options$plot_subtitle)
            }
            
            # Add axis labels
            if (nchar(self$options$x_label) > 0) {
                plot <- plot + ggplot2::xlab(self$options$x_label)
            }
            
            if (nchar(self$options$y_label) > 0) {
                plot <- plot + ggplot2::ylab(self$options$y_label)
            }
            
            # Add faceting if specified
            if (!is.null(self$options$facet_var)) {
                if (self$options$facet_type == "wrap") {
                    plot <- plot + ggplot2::facet_wrap(~ get(self$options$facet_var))
                } else {
                    plot <- plot + ggplot2::facet_grid(~ get(self$options$facet_var))
                }
            }
            
            return(plot)
        },
        
        .populate_model_table = function(data) {
            # Create and fit model
            formula <- self$.build_formula(data)
            model <- self$.fit_model(data, formula)
            
            # Extract model results
            if (requireNamespace('broom', quietly = TRUE)) {
                model_results <- broom::tidy(model, conf.int = TRUE, conf.level = self$options$confidence_level)
                
                # Populate results table
                for (i in seq_len(nrow(model_results))) {
                    self$results$modeltab$addRow(
                        rowKey = i,
                        values = list(
                            term = model_results$term[i],
                            estimate = model_results$estimate[i],
                            std_error = model_results$std.error[i],
                            statistic = model_results$statistic[i],
                            p_value = model_results$p.value[i],
                            conf_low = model_results$conf.low[i],
                            conf_high = model_results$conf.high[i]
                        )
                    )
                }
            }
        },
        
        .populate_model_summary = function(data) {
            # Create and fit model
            formula <- self$.build_formula(data)
            model <- self$.fit_model(data, formula)
            
            # Extract model summary statistics
            if (requireNamespace('broom', quietly = TRUE)) {
                model_glance <- broom::glance(model)
                
                # Create summary statistics
                summary_stats <- list()
                
                if ("r.squared" %in% names(model_glance)) {
                    summary_stats <- append(summary_stats, list(
                        list(metric = "R-squared", value = round(model_glance$r.squared, 4))
                    ))
                }
                
                if ("adj.r.squared" %in% names(model_glance)) {
                    summary_stats <- append(summary_stats, list(
                        list(metric = "Adjusted R-squared", value = round(model_glance$adj.r.squared, 4))
                    ))
                }
                
                if ("AIC" %in% names(model_glance)) {
                    summary_stats <- append(summary_stats, list(
                        list(metric = "AIC", value = round(model_glance$AIC, 2))
                    ))
                }
                
                if ("BIC" %in% names(model_glance)) {
                    summary_stats <- append(summary_stats, list(
                        list(metric = "BIC", value = round(model_glance$BIC, 2))
                    ))
                }
                
                summary_stats <- append(summary_stats, list(
                    list(metric = "Observations", value = nrow(data))
                ))
                
                # Populate results table
                for (i in seq_along(summary_stats)) {
                    self$results$summary$addRow(
                        rowKey = i,
                        values = summary_stats[[i]]
                    )
                }
            }
        },
        
        .generate_interpretation = function(data) {
            # Create and fit model
            formula <- self$.build_formula(data)
            model <- self$.fit_model(data, formula)
            
            analysis_name <- switch(self$options$analysis_type,
                "ggcoef_model" = "model coefficient visualization",
                "ggcoef_compare" = "model comparison analysis",
                "gglikert" = "Likert scale analysis",
                "ggsurvey" = "survey data analysis",
                "stat_prop" = "proportion analysis",
                "stat_cross" = "cross-tabulation analysis",
                "stat_weighted_mean" = "weighted mean analysis",
                "ggcascade" = "cascade analysis",
                "statistical analysis"
            )
            
            # Generate basic interpretation
            interpretation <- paste0(
                "<h3>Statistical Analysis Summary</h3>",
                "<p>This ", analysis_name, " was performed using the ggstats package.</p>"
            )
            
            # Add model-specific interpretation
            if (self$options$analysis_type %in% c("ggcoef_model", "ggcoef_compare")) {
                if (requireNamespace('broom', quietly = TRUE)) {
                    model_results <- broom::tidy(model)
                    n_significant <- sum(model_results$p.value < 0.05, na.rm = TRUE)
                    
                    interpretation <- paste0(interpretation,
                        "<p>The model includes ", nrow(model_results), " terms, of which ",
                        n_significant, " are statistically significant at α = 0.05.</p>"
                    )
                    
                    if (requireNamespace('broom', quietly = TRUE)) {
                        model_glance <- broom::glance(model)
                        if ("r.squared" %in% names(model_glance)) {
                            interpretation <- paste0(interpretation,
                                "<p>The model explains approximately ", 
                                round(model_glance$r.squared * 100, 1), 
                                "% of the variance in the outcome variable.</p>"
                            )
                        }
                    }
                }
            }
            
            # Add methodology note
            interpretation <- paste0(interpretation,
                "<p>The ggstats package provides enhanced statistical visualization capabilities ",
                "for ggplot2, including advanced coefficient plotting, survey analysis, and ",
                "specialized statistical graphics.</p>"
            )
            
            self$results$interpretation$setContent(interpretation)
        })
)
