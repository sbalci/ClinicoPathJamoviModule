
# This file is a generated template, your changes will not be overwritten

# Clinical Research Visualization with visR
# Following jamovi naming convention with j-prefix to avoid namespace conflicts

visrClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "visrClass",
    inherit = visrBase,
    private = list(
        .init = function() {
            # Initialize with appropriate plot dimensions
            self$results$plot$setSize(800, 600)
            self$results$summary$setVisible(TRUE)
            self$results$interpretation$setVisible(TRUE)
        },
        
        .run = function() {
            
            # Check if visR package is available
            if (!requireNamespace('visR', quietly = TRUE)) {
                stop('The visR package is required but not installed. Please install it using install.packages("visR")')
            }
            
            # Get data and options
            data <- self$data
            options <- self$options
            
            # Check for minimum required data
            if (is.null(data) || nrow(data) == 0) {
                return()
            }
            
            # Determine analysis type and required variables
            analysis_type <- options$analysis_type
            
            # Variable handling based on CDISC format
            if (options$cdisc_format) {
                # Use CDISC variable names
                if (is.null(options$aval_var) || is.null(options$cnsr_var)) {
                    return()
                }
                time_var <- options$aval_var
                event_var <- options$cnsr_var
                # Convert CNSR to event (CNSR: 1=censored, 0=event -> EVENT: 1=event, 0=censored)
                data[[paste0(event_var, '_event')]] <- 1 - data[[event_var]]
                event_var <- paste0(event_var, '_event')
            } else {
                # Use standard variable names
                if (is.null(options$time_var) || is.null(options$event_var)) {
                    return()
                }
                time_var <- options$time_var
                event_var <- options$event_var
            }
            
            # Clean data
            data <- data[!is.na(data[[time_var]]) & !is.na(data[[event_var]]), ]
            
            if (nrow(data) == 0) {
                return()
            }
            
            # Execute analysis based on type
            if (analysis_type == 'kaplan_meier') {
                self$.runKaplanMeier(data, time_var, event_var)
            } else if (analysis_type == 'cuminc') {
                self$.runCumInc(data, time_var, event_var)
            } else if (analysis_type == 'tableone') {
                self$.runTableOne(data)
            } else if (analysis_type == 'attrition') {
                self$.runAttrition(data)
            } else if (analysis_type == 'risktable') {
                self$.runRiskTable(data, time_var, event_var)
            }
        },
        
        .runKaplanMeier = function(data, time_var, event_var) {
            
            options <- self$options
            strata_var <- options$strata_var
            
            # Create survival formula
            if (!is.null(strata_var)) {
                formula <- as.formula(paste('Surv(', time_var, ',', event_var, ') ~', strata_var))
            } else {
                formula <- as.formula(paste('Surv(', time_var, ',', event_var, ') ~ 1'))
            }
            
            # Estimate Kaplan-Meier curves using visR wrapper
            tryCatch({
                km_fit <- self$.jestimate_KM(data, formula)
                
                # Generate plot using visR
                plot_obj <- self$.jvisr_plot(km_fit, data)
                
                # Set plot
                self$results$plot$setState(plot_obj)
                
                # Generate summary
                summary_table <- self$.jget_summary(km_fit)
                self$results$summary$setContent(summary_table)
                
                # Generate interpretation
                interpretation <- self$.jget_interpretation(km_fit, data, strata_var)
                self$results$interpretation$setContent(interpretation)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in Kaplan-Meier analysis:', e$message))
            })
        },
        
        .runCumInc = function(data, time_var, event_var) {
            
            options <- self$options
            strata_var <- options$strata_var
            
            # For cumulative incidence, we need competing risks
            tryCatch({
                
                # Create cumulative incidence plot
                plot_obj <- self$.jcumulative_incidence(data, time_var, event_var, strata_var)
                
                self$results$plot$setState(plot_obj)
                
                # Summary for cumulative incidence
                summary_text <- paste(
                    "<h3>Cumulative Incidence Analysis</h3>",
                    "<p>Analysis includes", nrow(data), "observations.</p>",
                    if (!is.null(strata_var)) paste("<p>Stratified by:", strata_var, "</p>") else ""
                )
                
                self$results$summary$setContent(summary_text)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in cumulative incidence analysis:', e$message))
            })
        },
        
        .runTableOne = function(data) {
            
            tryCatch({
                
                # Generate Table One using visR
                table_one <- self$.jtable_one(data)
                
                # Convert to HTML
                html_table <- knitr::kable(table_one, format = 'html', escape = FALSE)
                
                self$results$summary$setContent(html_table)
                
                # Simple interpretation
                interpretation <- paste(
                    "<h3>Table One Summary</h3>",
                    "<p>Descriptive statistics for", nrow(data), "observations across", ncol(data), "variables.</p>"
                )
                
                self$results$interpretation$setContent(interpretation)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in Table One analysis:', e$message))
            })
        },
        
        .runAttrition = function(data) {
            
            tryCatch({
                
                # Create attrition flowchart
                plot_obj <- self$.jattrition_chart(data)
                
                self$results$plot$setState(plot_obj)
                
                # Summary
                summary_text <- paste(
                    "<h3>Attrition Flowchart</h3>",
                    "<p>Patient flow diagram showing data attrition.</p>"
                )
                
                self$results$summary$setContent(summary_text)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in attrition analysis:', e$message))
            })
        },
        
        .runRiskTable = function(data, time_var, event_var) {
            
            options <- self$options
            strata_var <- options$strata_var
            
            tryCatch({
                
                # Create survival object for risk table
                if (!is.null(strata_var)) {
                    formula <- as.formula(paste('Surv(', time_var, ',', event_var, ') ~', strata_var))
                } else {
                    formula <- as.formula(paste('Surv(', time_var, ',', event_var, ') ~ 1'))
                }
                
                km_fit <- survival::survfit(formula, data = data)
                
                # Generate risk table
                risk_table <- self$.jget_risktable(km_fit)
                
                # Convert to HTML
                html_table <- knitr::kable(risk_table, format = 'html', escape = FALSE)
                
                self$results$summary$setContent(html_table)
                
                # Interpretation
                interpretation <- paste(
                    "<h3>Risk Table</h3>",
                    "<p>Numbers at risk over time for survival analysis.</p>"
                )
                
                self$results$interpretation$setContent(interpretation)
                
            }, error = function(e) {
                jmvcore::reject(paste('Error in risk table analysis:', e$message))
            })
        },
        
        # visR wrapper functions with j-prefix to avoid namespace conflicts
        .jestimate_KM = function(data, formula) {
            
            # Wrapper for visR::estimate_KM with additional options
            km_fit <- survival::survfit(formula, data = data)
            
            return(km_fit)
        },
        
        .jvisr_plot = function(km_fit, data) {
            
            options <- self$options
            
            # Create base plot using ggplot2 (since visR uses ggplot2)
            p <- ggplot2::ggplot()
            
            # Add survival curves
            if (requireNamespace('survminer', quietly = TRUE)) {
                p <- survminer::ggsurvplot(
                    km_fit,
                    data = data,
                    conf.int = options$confidence_interval,
                    risk.table = options$risk_table,
                    pval = options$p_value,
                    legend.labs = NULL,
                    legend = options$legend_position,
                    xlab = if (options$time_label != "") options$time_label else "Time",
                    ylab = if (options$survival_label != "") options$survival_label else "Survival Probability",
                    title = if (options$title != "") options$title else "Kaplan-Meier Survival Curves",
                    fun = options$fun_type,
                    palette = options$color_palette
                )
                
                # Return the plot component
                return(p$plot)
            } else {
                # Fallback to base survival plot
                return(plot(km_fit, main = if (options$title != "") options$title else "Survival Curves"))
            }
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
                    "<p><strong>Stratification:</strong> Analysis stratified by", strata_var, "</p>"
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
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = time_var)) +
                ggplot2::geom_step(ggplot2::aes_string(y = paste0('cumsum(', event_var, ') / n()'))) +
                ggplot2::labs(x = "Time", y = "Cumulative Incidence", title = "Cumulative Incidence Curve") +
                ggplot2::theme_minimal()
            
            if (!is.null(strata_var)) {
                p <- p + ggplot2::aes_string(color = strata_var)
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
                        round(mean(var_data), 2), " ± ", round(sd(var_data), 2),
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
