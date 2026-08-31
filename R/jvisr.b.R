
# This file is a generated template, your changes will not be overwritten

# Clinical Research Visualization with visR
# Following jamovi naming convention with j-prefix to avoid namespace conflicts

jvisrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jvisrClass",
    inherit = jvisrBase,
    private = list(
        # Performance optimization: cache variables
        .prepared_data = NULL,
        .prepared_options = NULL,
        .data_hash = NULL,
        .options_hash = NULL,
        .cached_plot = NULL,
        .cached_summary = NULL,
        .cached_interpretation = NULL,
        
        .init = function() {
            # Initialize with appropriate plot dimensions
            self$results$plot$setSize(800, 600)
            self$results$summary$setVisible(TRUE)
            self$results$interpretation$setVisible(TRUE)
            
            # Initialize results visibility based on required variables
            if (is.null(self$options$time_var) || is.null(self$options$event_var)) {
                if (!self$options$cdisc_format || is.null(self$options$aval_var) || is.null(self$options$cnsr_var)) {
                    self$results$plot$setVisible(FALSE)
                    self$results$summary$setVisible(FALSE)
                    self$results$interpretation$setVisible(FALSE)
                }
            }
        },
        
        # Performance optimization methods
        .calculateDataHash = function() {
            if (is.null(self$data) || nrow(self$data) == 0) {
                return(NULL)
            }
            
            # Determine relevant variables based on CDISC vs standard format
            relevant_vars <- c()
            
            if (self$options$cdisc_format) {
                if (!is.null(self$options$aval_var)) relevant_vars <- c(relevant_vars, self$options$aval_var)
                if (!is.null(self$options$cnsr_var)) relevant_vars <- c(relevant_vars, self$options$cnsr_var)
            } else {
                if (!is.null(self$options$time_var)) relevant_vars <- c(relevant_vars, self$options$time_var)
                if (!is.null(self$options$event_var)) relevant_vars <- c(relevant_vars, self$options$event_var)
            }
            
            if (!is.null(self$options$strata_var)) relevant_vars <- c(relevant_vars, self$options$strata_var)
            
            # Remove NULLs and ensure variables exist
            relevant_vars <- relevant_vars[!sapply(relevant_vars, is.null)]
            relevant_vars <- relevant_vars[relevant_vars %in% names(self$data)]
            
            if (length(relevant_vars) == 0) {
                return(NULL)
            }
            
            # Create hash string including data summary
            data_summary <- paste(
                nrow(self$data),
                ncol(self$data),
                paste(relevant_vars, collapse = "_"),
                paste(sapply(relevant_vars, function(var) {
                    if (is.numeric(self$data[[var]])) {
                        paste(range(self$data[[var]], na.rm = TRUE), collapse = "_")
                    } else {
                        paste(length(unique(self$data[[var]])), "levels")
                    }
                }), collapse = "_"),
                self$options$analysis_type,
                self$options$cdisc_format,
                sep = "_"
            )
            
            return(data_summary)
        },
        
        .calculateOptionsHash = function() {
            # Create hash of all relevant options for clinical visualization
            options_list <- list(
                analysis_type = self$options$analysis_type,
                time_var = self$options$time_var,
                event_var = self$options$event_var,
                strata_var = self$options$strata_var,
                cdisc_format = self$options$cdisc_format,
                aval_var = self$options$aval_var,
                cnsr_var = self$options$cnsr_var,
                fun_type = self$options$fun_type,
                confidence_interval = self$options$confidence_interval,
                risk_table = self$options$risk_table,
                quantiles = self$options$quantiles,
                p_value = self$options$p_value,
                legend_position = self$options$legend_position,
                time_label = self$options$time_label,
                time_units = self$options$time_units,
                survival_label = self$options$survival_label,
                title = self$options$title,
                theme_style = self$options$theme_style,
                color_palette = self$options$color_palette,
                show_summary = self$options$show_summary,
                show_interpretation = self$options$show_interpretation
            )
            
            return(paste(options_list, collapse = "_"))
        },
        
        .canUseCache = function() {
            current_data_hash <- private$.calculateDataHash()
            current_options_hash <- private$.calculateOptionsHash()
            
            return(!is.null(private$.cached_plot) &&
                   !is.null(private$.data_hash) &&
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
                mydata <- self$data
                
                if (is.null(mydata) || nrow(mydata) == 0) {
                    private$.prepared_data <- NULL
                    private$.data_hash <- current_hash
                    return(NULL)
                }
                
                # Clean data and prepare variables
                if (self$options$cdisc_format) {
                    # CDISC format processing
                    if (!is.null(self$options$aval_var) && !is.null(self$options$cnsr_var)) {
                        time_var <- self$options$aval_var
                        event_var <- self$options$cnsr_var
                        # Convert CNSR to event (CNSR: 1=censored, 0=event -> EVENT: 1=event, 0=censored)
                        # toNumeric handles factor CNSR (permitted: [factor, numeric] in .a.yaml) - honors `values` attribute
                        mydata[[paste0(event_var, '_event')]] <- 1 - jmvcore::toNumeric(mydata[[event_var]])
                        mydata$time_variable <- mydata[[time_var]]
                        mydata$event_variable <- mydata[[paste0(event_var, '_event')]]
                    }
                } else {
                    # Standard format processing
                    if (!is.null(self$options$time_var) && !is.null(self$options$event_var)) {
                        mydata$time_variable <- mydata[[self$options$time_var]]
                        mydata$event_variable <- mydata[[self$options$event_var]]
                    }
                }
                
                # Clean missing data
                if (!is.null(mydata$time_variable) && !is.null(mydata$event_variable)) {
                    mydata <- mydata[!is.na(mydata$time_variable) & !is.na(mydata$event_variable), ]
                }
                
                private$.prepared_data <- mydata
                private$.data_hash <- current_hash
            }
            
            return(private$.prepared_data)
        },
        
        .prepareOptions = function() {
            current_hash <- private$.calculateOptionsHash()
            
            if (is.null(private$.options_hash) || private$.options_hash != current_hash) {
                private$.prepared_options <- self$options
                private$.options_hash <- current_hash
                
                # Clear cached results when options change
                private$.cached_plot <- NULL
                private$.cached_summary <- NULL
                private$.cached_interpretation <- NULL
            }
            
            return(private$.prepared_options)
        },
        
        .run = function() {
            
            # Check required variables first
            required_vars_missing <- FALSE
            
            if (self$options$cdisc_format) {
                if (is.null(self$options$aval_var) || is.null(self$options$cnsr_var)) {
                    required_vars_missing <- TRUE
                }
            } else {
                if (is.null(self$options$time_var) || is.null(self$options$event_var)) {
                    required_vars_missing <- TRUE
                }
            }
            
            if (required_vars_missing) {
                todo <- paste(
                    "<br>Welcome to Clinical Research Visualization with visR",
                    "<br><br>",
                    "This tool provides fit-for-purpose clinical visualizations using the visR package.",
                    "<br><br>",
                    "<b>Required Variables:</b>",
                    if (self$options$cdisc_format) {
                        "<br>\u{2022} AVAL Variable (Analysis Value - time to event)<br>\u{2022} CNSR Variable (Censor indicator)"
                    } else {
                        "<br>\u{2022} Time Variable (time to event or censoring)<br>\u{2022} Event Variable (event indicator)"
                    },
                    "<br><br>",
                    "<b>Analysis Types Available:</b>",
                    "<br>\u{2022} Kaplan-Meier Survival Curves",
                    "<br>\u{2022} Cumulative Incidence Plots",
                    "<br>\u{2022} Table One Summaries",
                    "<br>\u{2022} Attrition Flowcharts",
                    "<br>\u{2022} Risk Tables",
                    "<br><hr>"
                )
                
                self$results$summary$setContent(todo)
                return()
            }
            
            # Check if visR package is available (only when needed)
            if (!requireNamespace('visR', quietly = TRUE)) {
                jmvcore::reject('The visR package is required but not installed. Please install it using install.packages("visR")')
                return()
            }
            
            # Performance optimization: prepare data and options with caching
            mydata <- private$.prepareData()
            options <- private$.prepareOptions()
            
            if (is.null(mydata) || nrow(mydata) == 0) {
                jmvcore::reject("Data contains no (complete) rows")
                return()
            }
            
            # Check if we can use cached results
            if (private$.canUseCache()) {
                if (!is.null(private$.cached_plot)) {
                    self$results$plot$setState(private$.cached_plot)
                }
                if (!is.null(private$.cached_summary) && options$show_summary) {
                    self$results$summary$setContent(private$.cached_summary)
                }
                if (!is.null(private$.cached_interpretation) && options$show_interpretation) {
                    self$results$interpretation$setContent(private$.cached_interpretation)
                }
                return()
            }
            
            # Execute analysis based on type
            analysis_type <- options$analysis_type
            
            if (analysis_type == 'kaplan_meier') {
                private$.runKaplanMeier(mydata)
            } else if (analysis_type == 'cuminc') {
                private$.runCumInc(mydata)
            } else if (analysis_type == 'tableone') {
                private$.runTableOne(mydata)
            } else if (analysis_type == 'attrition') {
                private$.runAttrition(mydata)
            } else if (analysis_type == 'risktable') {
                private$.runRiskTable(mydata)
            }
        },
        
        .runKaplanMeier = function(data) {
            
            options <- self$options
            strata_var <- options$strata_var
            
            # Create survival formula using prepared time and event variables
            if (!is.null(strata_var) && strata_var %in% names(data)) {
                formula <- jmvcore::asFormula(paste('Surv(time_variable, event_variable) ~', jmvcore::composeTerm(strata_var)))
            } else {
                formula <- jmvcore::asFormula('Surv(time_variable, event_variable) ~ 1')
            }

            # Estimate Kaplan-Meier curves using enhanced visR integration
            tryCatch({
                # Use enhanced KM estimation with better visR integration
                km_fit <- private$.jestimate_KM_enhanced(data, formula)
                
                # Generate enhanced visR plot
                plot_obj <- private$.jvisr_plot_enhanced(km_fit, data)
                
                # Cache and set plot
                private$.cached_plot <- plot_obj
                self$results$plot$setState(plot_obj)
                
                # Generate summary with caching
                if (options$show_summary) {
                    summary_table <- private$.jget_summary(km_fit)
                    private$.cached_summary <- summary_table
                    self$results$summary$setContent(summary_table)
                } else {
                    self$results$summary$setContent("")
                }
                
                # Generate interpretation with caching
                if (options$show_interpretation) {
                    interpretation <- private$.jget_interpretation(km_fit, data, strata_var)
                    private$.cached_interpretation <- interpretation
                    self$results$interpretation$setContent(interpretation)
                } else {
                    self$results$interpretation$setContent("")
                }
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in Kaplan-Meier analysis:', e$message))
            })
        },
        
        .runCumInc = function(data) {
            
            options <- self$options
            strata_var <- options$strata_var
            
            # For cumulative incidence, we need competing risks
            tryCatch({
                
                # Create cumulative incidence plot
                plot_obj <- private$.jcumulative_incidence(data, "time_variable", "event_variable", strata_var)
                
                # Cache and set plot
                private$.cached_plot <- plot_obj
                self$results$plot$setState(plot_obj)
                
                # Summary for cumulative incidence with caching
                if (options$show_summary) {
                    summary_text <- paste(
                        "<h3>Cumulative Incidence Analysis</h3>",
                        "<p>Analysis includes", nrow(data), "observations.</p>",
                        if (!is.null(strata_var)) paste("<p>Stratified by:", htmltools::htmlEscape(strata_var), "</p>") else ""
                    )
                    private$.cached_summary <- summary_text
                    self$results$summary$setContent(summary_text)
                } else {
                    self$results$summary$setContent("")
                }
                
                # Interpretation with caching
                if (options$show_interpretation) {
                    interpretation <- paste(
                        "<h3>Cumulative Incidence Interpretation</h3>",
                        "<p>This plot shows the cumulative incidence of events over time.</p>"
                    )
                    private$.cached_interpretation <- interpretation
                    self$results$interpretation$setContent(interpretation)
                } else {
                    self$results$interpretation$setContent("")
                }
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in cumulative incidence analysis:', e$message))
            })
        },
        
        .runTableOne = function(data) {
            
            options <- self$options
            
            tryCatch({
                
                # Generate Table One using existing method
                table_one <- private$.jtable_one(data)

                # Convert to HTML for display (escape=TRUE - table cells include user column names and factor levels)
                html_table <- knitr::kable(table_one, format = 'html', escape = TRUE)
                
                # Cache and set results
                private$.cached_summary <- html_table
                self$results$summary$setContent(html_table)
                
                # Simple interpretation
                if (options$show_interpretation) {
                    interpretation <- paste(
                        "<h3>Table One Summary</h3>",
                        "<p>Descriptive statistics for", nrow(data), "observations across", ncol(data), "variables.</p>"
                    )
                    private$.cached_interpretation <- interpretation
                    self$results$interpretation$setContent(interpretation)
                } else {
                    self$results$interpretation$setContent("")
                }
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in Table One analysis:', e$message))
            })
        },
        
        .runAttrition = function(data) {
            
            options <- self$options
            
            tryCatch({
                
                # Create attrition flowchart
                plot_obj <- private$.jattrition_chart(data)
                
                # Cache and set plot
                private$.cached_plot <- plot_obj
                self$results$plot$setState(plot_obj)
                
                if (options$show_summary) {
                    # Summary
                    summary_text <- paste(
                        "<h3>Attrition Flowchart</h3>",
                        "<p>Patient flow diagram showing data attrition.</p>"
                    )
                    private$.cached_summary <- summary_text
                    self$results$summary$setContent(summary_text)
                } else {
                    self$results$summary$setContent("")
                }
                
                if (options$show_interpretation) {
                    # Interpretation
                    interpretation <- paste(
                        "<h3>Attrition Analysis</h3>",
                        "<p>This chart shows the flow of patients through the study.</p>"
                    )
                    private$.cached_interpretation <- interpretation
                    self$results$interpretation$setContent(interpretation)
                } else {
                    self$results$interpretation$setContent("")
                }
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in attrition analysis:', e$message))
            })
        },
        
        .runRiskTable = function(data) {
            
            options <- self$options
            strata_var <- options$strata_var
            
            tryCatch({
                
                # Create survival object for risk table using prepared variables
                if (!is.null(strata_var) && strata_var %in% names(data)) {
                    formula <- jmvcore::asFormula(paste('Surv(time_variable, event_variable) ~', jmvcore::composeTerm(strata_var)))
                } else {
                    formula <- jmvcore::asFormula('Surv(time_variable, event_variable) ~ 1')
                }

                km_fit <- survival::survfit(formula, data = data)
                
                # Generate risk table
                risk_table <- private$.jget_risktable(km_fit)
                
                # Convert to HTML
                html_table <- knitr::kable(risk_table, format = 'html', escape = FALSE)
                
                # Cache and set results
                private$.cached_summary <- html_table
                self$results$summary$setContent(html_table)
                
                if (options$show_interpretation) {
                    # Interpretation
                    interpretation <- paste(
                        "<h3>Risk Table</h3>",
                        "<p>Numbers at risk over time for survival analysis.</p>"
                    )
                    private$.cached_interpretation <- interpretation
                    self$results$interpretation$setContent(interpretation)
                } else {
                    self$results$interpretation$setContent("")
                }
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in risk table analysis:', e$message))
            })
        },
        
        # Enhanced visR wrapper functions with j-prefix to avoid namespace conflicts
        .jestimate_KM_enhanced = function(data, formula) {
            
            # visR::estimate_KM() is deliberately NOT used here. Called without formula= it
            # falls back to visR's CDISC ADaM column contract and always stop()s on this
            # module's prepared frame ("Following columns are missing from `data`: CNSR
            # AVAL"), so the branch only ever cost a thrown-and-caught error per run.
            # (It is also deprecated as of visR 0.4.0.)
            km_fit <- survival::survfit(formula, data = data)
            # survminer re-parses fit$call$formula; passing the formula through a
            # variable leaves a bare symbol there and ggsurvplot dies with
            # "object of type 'symbol' is not subsettable".
            km_fit$call$formula <- formula
            return(km_fit)
        },
        
        .jvisr_plot_enhanced = function(km_fit, data) {
            
            options <- self$options
            
            # Enhanced plotting with proper visR integration
            if (requireNamespace('visR', quietly = TRUE)) {
                # Use visR's plotting capabilities
                tryCatch({
                    p <- visR::visr(km_fit)
                    
                    # Apply customizations
                    if (options$confidence_interval) {
                        p <- p + ggplot2::geom_ribbon(alpha = 0.2)
                    }
                    
                    # Apply theme based on options
                    if (options$theme_style == "visr") {
                        p <- p + visR::define_theme()
                    } else {
                        p <- p + private$.apply_theme(options$theme_style)
                    }
                    
                    # Apply color palette
                    p <- p + private$.apply_color_palette(options$color_palette)
                    
                    # Apply labels
                    p <- p + ggplot2::labs(
                        x = if (options$time_label != "") {
                            paste(options$time_label, if (options$time_units != "") paste0(" (", options$time_units, ")") else "")
                        } else {
                            "Time"
                        },
                        y = if (options$survival_label != "") options$survival_label else "Survival Probability",
                        title = if (options$title != "") options$title else "Kaplan-Meier Survival Curves"
                    )
                    
                    # Apply legend position
                    p <- p + ggplot2::theme(legend.position = options$legend_position)
                    
                    return(p)
                    
                }, error = function(e) {
                    # Fallback to survminer if visR fails
                    return(private$.jvisr_plot_fallback(km_fit, data))
                })
            } else {
                # Fallback to survminer
                return(private$.jvisr_plot_fallback(km_fit, data))
            }
        },
        
        .apply_theme = function(theme_style) {
            # Return a ggplot2 theme object for the requested style
            switch(theme_style,
                "classic" = ggplot2::theme_classic(),
                "minimal" = ggplot2::theme_minimal(),
                "clean"   = ggplot2::theme_bw(),
                ggplot2::theme_minimal()
            )
        },

        .apply_color_palette = function(color_palette) {
            # Return a ggplot2 brewer colour scale, or NULL (no-op) for the default palette
            if (is.null(color_palette) || color_palette == "default") {
                return(NULL)
            }
            ggplot2::scale_color_brewer(palette = color_palette)
        },

        .jvisr_plot_fallback = function(km_fit, data) {
            
            options <- self$options
            
            # Fallback plotting using survminer
            if (requireNamespace('survminer', quietly = TRUE)) {
                p <- survminer::ggsurvplot(
                    km_fit,
                    data = data,
                    conf.int = options$confidence_interval,
                    risk.table = options$risk_table,
                    pval = options$p_value,
                    legend.labs = NULL,
                    legend = options$legend_position,
                    xlab = if (options$time_label != "") {
                        paste(options$time_label, if (options$time_units != "") paste0(" (", options$time_units, ")") else "")
                    } else {
                        "Time"
                    },
                    ylab = if (options$survival_label != "") options$survival_label else "Survival Probability",
                    title = if (options$title != "") options$title else "Kaplan-Meier Survival Curves",
                    fun = options$fun_type,
                    palette = options$color_palette
                )
                
                # Return the plot component
                return(p$plot)
            } else {
                # Final fallback to base plotting
                return(private$.jvisr_plot_base(km_fit))
            }
        },
        
        .jvisr_plot_base = function(km_fit) {
            options <- self$options
            # Create a simple ggplot2 version as final fallback
            survdata <- data.frame(
                time = km_fit$time,
                surv = km_fit$surv,
                upper = km_fit$upper,
                lower = km_fit$lower
            )
            
            p <- ggplot2::ggplot(survdata, ggplot2::aes(x = time, y = surv)) +
                ggplot2::geom_step() +
                ggplot2::labs(
                    x = if (options$time_label != "") options$time_label else "Time",
                    y = if (options$survival_label != "") options$survival_label else "Survival Probability",
                    title = if (options$title != "") options$title else "Survival Curves"
                ) +
                ggplot2::theme_minimal()
            
            if (options$confidence_interval) {
                p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2)
            }
            
            return(p)
        },
        
        .jget_summary = function(km_fit) {
            
            # Generate summary statistics
            summary_stats <- summary(km_fit)
            
            # Create HTML table
            summary_table <- data.frame(
                Time = summary_stats$time,
                n.risk = summary_stats$n.risk,
                n.event = summary_stats$n.event,
                survival = round(summary_stats$surv, 3),
                std.err = round(summary_stats$std.err, 3),
                lower.95 = round(summary_stats$lower, 3),
                upper.95 = round(summary_stats$upper, 3)
            )
            
            # Convert to HTML
            html_table <- knitr::kable(summary_table, format = 'html', escape = FALSE,
                                     caption = "Survival Summary Statistics")
            
            return(html_table)
        },
        
        .jget_interpretation = function(km_fit, data, strata_var = NULL) {
            
            # Generate clinical interpretation
            median_surv <- summary(km_fit)$table[,'median']
            
            interpretation <- paste(
                "<h3>Clinical Interpretation</h3>",
                "<p><strong>Sample Size:</strong>", nrow(data), "patients</p>",
                "<p><strong>Median Survival:</strong>", 
                if (!is.na(median_surv[1])) paste(round(median_surv[1], 2), "time units") else "Not reached",
                "</p>"
            )
            
            if (!is.null(strata_var)) {
                interpretation <- paste(interpretation,
                    "<p><strong>Stratification:</strong> Analysis stratified by", htmltools::htmlEscape(strata_var), "</p>"
                )
            }
            
            # Add clinical relevance
            interpretation <- paste(interpretation,
                "<p><strong>Clinical Relevance:</strong> These survival curves show the probability of survival over time. ",
                "Confidence intervals indicate the uncertainty in survival estimates. ",
                "Use this information to understand patient prognosis and treatment effectiveness.</p>"
            )
            
            return(interpretation)
        },
        
        .jcumulative_incidence = function(data, time_var, event_var, strata_var = NULL) {
            
            # Create cumulative incidence plot
            data$.j_cum_event <- cumsum(data[[event_var]]) / nrow(data)
            p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[time_var]])) +
                ggplot2::geom_step(ggplot2::aes(y = .data$.j_cum_event)) +
                ggplot2::labs(x = "Time", y = "Cumulative Incidence", title = "Cumulative Incidence Curve") +
                ggplot2::theme_minimal()

            if (!is.null(strata_var)) {
                p <- p + ggplot2::aes(color = .data[[strata_var]])
            }
            
            return(p)
        },
        
        .jtable_one = function(data) {
            
            # Generate Table One summary
            # Simple descriptive statistics
            numeric_vars <- sapply(data, is.numeric)
            factor_vars <- sapply(data, is.factor) | sapply(data, is.character)
            
            results <- data.frame(
                Variable = character(),
                N = numeric(),
                Summary = character(),
                stringsAsFactors = FALSE
            )
            
            # Numeric variables
            for (var in names(data)[numeric_vars]) {
                var_data <- data[[var]][!is.na(data[[var]])]
                if (length(var_data) > 0) {
                    summary_text <- paste0(
                        round(mean(var_data), 2), " \u{00B1} ", round(sd(var_data), 2),
                        " (median: ", round(median(var_data), 2), ")"
                    )
                    results <- rbind(results, data.frame(
                        Variable = var,
                        N = length(var_data),
                        Summary = summary_text
                    ))
                }
            }
            
            # Categorical variables
            for (var in names(data)[factor_vars]) {
                var_data <- data[[var]][!is.na(data[[var]])]
                if (length(var_data) > 0) {
                    freq_table <- table(var_data)
                    summary_text <- paste(
                        paste0(names(freq_table), ": ", freq_table, " (", 
                              round(freq_table/sum(freq_table)*100, 1), "%)"),
                        collapse = "; "
                    )
                    results <- rbind(results, data.frame(
                        Variable = var,
                        N = length(var_data),
                        Summary = summary_text
                    ))
                }
            }
            
            return(results)
        },
        
        .jattrition_chart = function(data) {
            
            # Simple attrition chart
            n_total <- nrow(data)
            n_complete <- sum(complete.cases(data))
            n_missing <- n_total - n_complete
            
            # Create a simple bar chart showing attrition
            attrition_data <- data.frame(
                Stage = c("Total", "Complete Cases", "Missing Data"),
                Count = c(n_total, n_complete, n_missing),
                Percentage = c(100, n_complete/n_total*100, n_missing/n_total*100)
            )
            
            p <- ggplot2::ggplot(attrition_data, ggplot2::aes(x = Stage, y = Count)) +
                ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
                ggplot2::geom_text(ggplot2::aes(label = paste0(Count, " (", round(Percentage, 1), "%)"),
                                               vjust = -0.5)) +
                ggplot2::labs(title = "Data Attrition Chart", x = "Stage", y = "Count") +
                ggplot2::theme_minimal()
            
            return(p)
        },
        
        .jget_risktable = function(km_fit) {
            
            # Extract risk table information
            times <- seq(0, max(km_fit$time), length.out = 6)
            
            risk_data <- data.frame(
                Time = times,
                AtRisk = sapply(times, function(t) {
                    idx <- max(which(km_fit$time <= t))
                    if (length(idx) > 0 && idx > 0) {
                        km_fit$n.risk[idx]
                    } else {
                        km_fit$n.risk[1]
                    }
                })
            )
            
            return(risk_data)
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
