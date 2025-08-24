rmstregressionClass <- R6::R6Class(
    "rmstregressionClass",
    inherit = rmstregressionBase,
    private = list(
        .init = function() {
            if (is.null(self$data))
                return()
            
            # Validate required variables
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent(
                    '<h3>Analysis Setup</h3>
                     <p>Please provide the required variables:</p>
                     <ul>
                     <li><b>Time Variable</b>: Time to event or censoring</li>
                     <li><b>Event Indicator</b>: Event status (0=censored, 1=event)</li>
                     </ul>
                     <p>Optionally specify:</p>
                     <ul>
                     <li><b>Grouping Variable</b>: For group comparisons</li>
                     <li><b>Explanatory Variables</b>: For regression analysis</li>
                     </ul>'
                )
                return()
            }
            
            # Initialize result tables
            private$.initTauSelection()
            
            if (self$options$show_rmst_table)
                private$.initRMSTSummary()
                
            if (self$options$show_difference_table)
                private$.initRMSTDifferences()
                
            if (self$options$show_regression_table)
                private$.initRegressionResults()
                
            if (self$options$show_model_diagnostics)
                private$.initModelDiagnostics()
                
            if (self$options$bootstrap_se)
                private$.initBootstrapResults()
        },
        
        .run = function() {
            if (is.null(self$data))
                return()
                
            # Validate required variables
            if (is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }
            
            tryCatch({
                # Prepare data
                data <- private$.prepareData()
                if (is.null(data)) return()
                
                # Select tau
                tau_info <- private$.selectTau(data)
                private$.populateTauSelection(tau_info)
                
                # Perform RMST analysis based on analysis type
                if (self$options$analysis_type %in% c("comparison", "combined")) {
                    private$.performRMSTComparison(data, tau_info$tau)
                }
                
                if (self$options$analysis_type %in% c("regression", "combined")) {
                    private$.performRMSTRegression(data, tau_info$tau)
                }
                
                # Create plots
                if (self$options$rmst_plot) {
                    private$.createRMSTPlot(data, tau_info$tau)
                }
                
                if (self$options$difference_plot) {
                    private$.createDifferencePlot(data, tau_info$tau)
                }
                
                if (self$options$residual_plot && self$options$analysis_type %in% c("regression", "combined")) {
                    private$.createResidualPlot(data, tau_info$tau)
                }
                
                # Add explanations and summaries
                if (self$options$showExplanations) {
                    private$.addMethodologyExplanation(tau_info$tau)
                }
                
                if (self$options$showSummaries) {
                    private$.addAnalysisSummary(data, tau_info$tau)
                }
                
                self$results$todo$setVisible(FALSE)
                
            }, error = function(e) {
                self$results$todo$setVisible(TRUE)
                error_msg <- paste0('<h3>Analysis Error</h3><p><b>Error:</b> ', e$message, '</p>')
                
                if (grepl("tau|restriction", e$message, ignore.case = TRUE)) {
                    error_msg <- paste0(error_msg, 
                        '<p><b>Suggestion:</b> Try adjusting the restriction time (tau) or tau selection method.</p>')
                } else if (grepl("survival|rmst", e$message, ignore.case = TRUE)) {
                    error_msg <- paste0(error_msg, 
                        '<p><b>Suggestion:</b> Verify that time and event variables are correctly specified and that tau is appropriate for your data.</p>')
                }
                
                self$results$todo$setContent(error_msg)
            })
        },
        
        .prepareData = function() {
            # Get variables
            time_var <- self$options$elapsedtime
            event_var <- self$options$outcome
            
            data <- data.frame(
                time = jmvcore::toNumeric(self$data[[time_var]]),
                event = jmvcore::toNumeric(self$data[[event_var]])
            )
            
            # Add grouping variable
            if (!is.null(self$options$group_var)) {
                data$group <- factor(self$data[[self$options$group_var]])
            } else {
                data$group <- factor(rep("All", nrow(data)))
            }
            
            # Add explanatory variables
            if (length(self$options$explanatory) > 0) {
                for (var in self$options$explanatory) {
                    if (self$data[[var]]$measureType == 'continuous') {
                        data[[var]] <- jmvcore::toNumeric(self$data[[var]])
                    } else {
                        data[[var]] <- factor(self$data[[var]])
                    }
                }
            }
            
            # Remove missing values
            complete_cases <- complete.cases(data)
            if (sum(complete_cases) < nrow(data)) {
                n_missing <- sum(!complete_cases)
                message(sprintf("Removed %d observations with missing values", n_missing))
            }
            
            data <- data[complete_cases, ]
            
            # Validate data
            if (nrow(data) < 10) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent('<p>Error: Insufficient data for analysis. At least 10 complete observations required.</p>')
                return(NULL)
            }
            
            if (all(data$time <= 0)) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent('<p>Error: Time variable must contain positive values.</p>')
                return(NULL)
            }
            
            # Check event indicator
            unique_events <- sort(unique(data$event))
            if (!all(unique_events %in% c(0, 1))) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent('<p>Error: Event indicator must be 0 (censored) or 1 (event).</p>')
                return(NULL)
            }
            
            return(data)
        },
        
        .selectTau = function(data) {
            if (self$options$tau_method == "fixed") {
                tau <- self$options$tau
                rationale <- "User-specified fixed value"
            } else if (self$options$tau_method == "percentile") {
                percentile <- self$options$tau_percentile
                tau <- quantile(data$time, percentile, na.rm = TRUE)
                rationale <- paste0("Based on ", percentile * 100, "th percentile of follow-up time")
            } else if (self$options$tau_method == "adaptive") {
                # Adaptive selection based on data characteristics
                max_time <- max(data$time)
                event_times <- data$time[data$event == 1]
                
                if (length(event_times) > 0) {
                    # Use 80th percentile of event times, but not more than 90% of max follow-up
                    tau_event <- quantile(event_times, 0.8, na.rm = TRUE)
                    tau_followup <- 0.9 * max_time
                    tau <- min(tau_event, tau_followup)
                } else {
                    tau <- 0.8 * max_time
                }
                rationale <- "Adaptively selected based on event distribution and follow-up"
            }
            
            # Calculate follow-up percentage and events by tau
            followup_pct <- mean(data$time >= tau) * 100
            events_by_tau <- sum(data$event == 1 & data$time <= tau)
            
            return(list(
                tau = tau,
                method = self$options$tau_method,
                rationale = rationale,
                followup_pct = followup_pct,
                events_by_tau = events_by_tau
            ))
        },
        
        .performRMSTComparison = function(data, tau) {
            # Check for required packages
            if (!requireNamespace("survival", quietly = TRUE)) {
                self$results$todo$setVisible(TRUE)
                self$results$todo$setContent('<p>Error: Required package "survival" is not installed.</p>')
                return()
            }
            
            # Calculate RMST for each group
            groups <- levels(data$group)
            rmst_results <- list()
            
            for (grp in groups) {
                grp_data <- data[data$group == grp, ]
                rmst_info <- private$.calculateRMST(grp_data, tau)
                rmst_results[[grp]] <- rmst_info
                
                # Populate RMST summary table
                if (self$options$show_rmst_table) {
                    row <- list(
                        group = grp,
                        n = rmst_info$n,
                        events = rmst_info$events,
                        rmst = rmst_info$rmst,
                        se = rmst_info$se,
                        ci_lower = rmst_info$ci_lower,
                        ci_upper = rmst_info$ci_upper,
                        tau_reached = ifelse(rmst_info$tau_reached, "Yes", "No")
                    )
                    self$results$rmstSummary$addRow(rowKey = grp, values = row)
                }
            }
            
            # Calculate pairwise differences if multiple groups
            if (length(groups) > 1 && self$options$show_difference_table) {
                private$.calculateRMSTDifferences(rmst_results, groups)
            }
        },
        
        .calculateRMST = function(data, tau) {
            # Create survival object
            surv_obj <- survival::Surv(data$time, data$event)
            km_fit <- survival::survfit(surv_obj ~ 1)
            
            # Calculate RMST using integration of survival function
            times <- km_fit$time
            surv <- km_fit$surv
            
            # Restrict to tau
            times <- c(0, times[times <= tau], tau)
            surv_extended <- c(1, surv[km_fit$time <= tau])
            if (max(km_fit$time) < tau) {
                surv_extended <- c(surv_extended, tail(surv, 1))
            } else {
                # Interpolate survival at tau if needed
                if (tau %in% km_fit$time) {
                    surv_at_tau <- surv[km_fit$time == tau]
                } else {
                    # Use last available survival estimate
                    surv_at_tau <- tail(surv[km_fit$time <= tau], 1)
                    if (length(surv_at_tau) == 0) surv_at_tau <- 1
                }
                surv_extended <- c(surv_extended, surv_at_tau)
            }
            
            times <- unique(sort(times))
            surv_extended <- surv_extended[1:length(times)]
            
            # Calculate RMST as area under curve
            rmst <- 0
            for (i in 2:length(times)) {
                width <- times[i] - times[i-1]
                height <- surv_extended[i-1]
                rmst <- rmst + width * height
            }
            
            # Calculate standard error using Greenwood formula
            if (requireNamespace("survRM2", quietly = TRUE)) {
                # Use survRM2 package if available for more accurate SE calculation
                tryCatch({
                    rmst_obj <- survRM2::rmst2(data$time, data$event, arm = rep(1, nrow(data)), tau = tau)
                    rmst_se <- rmst_obj$RMST.arm1$se
                }, error = function(e) {
                    rmst_se <- private$.calculateRMSTSE(km_fit, tau, rmst)
                })
            } else {
                rmst_se <- private$.calculateRMSTSE(km_fit, tau, rmst)
            }
            
            # Calculate confidence intervals
            ci_level <- self$options$confidence_level
            alpha <- 1 - ci_level
            z_score <- qnorm(1 - alpha/2)
            
            ci_lower <- max(0, rmst - z_score * rmst_se)
            ci_upper <- rmst + z_score * rmst_se
            
            return(list(
                rmst = rmst,
                se = rmst_se,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                n = nrow(data),
                events = sum(data$event),
                tau_reached = max(data$time) >= tau
            ))
        },
        
        .calculateRMSTSE = function(km_fit, tau, rmst) {
            # Approximate standard error calculation using Greenwood formula
            times <- km_fit$time[km_fit$time <= tau]
            n_risk <- km_fit$n.risk[km_fit$time <= tau]
            n_event <- km_fit$n.event[km_fit$time <= tau]
            surv <- km_fit$surv[km_fit$time <= tau]
            
            if (length(times) == 0) {
                return(sqrt(tau / km_fit$n))  # Approximate for early tau
            }
            
            # Greenwood variance for survival function
            greenwood_var <- cumsum(n_event / (n_risk * (n_risk - n_event)))
            surv_var <- surv^2 * greenwood_var
            
            # Approximate RMST variance using integral
            times_ext <- c(0, times, tau)
            surv_ext <- c(1, surv, tail(surv, 1))
            surv_var_ext <- c(0, surv_var, tail(surv_var, 1))
            
            rmst_var <- 0
            for (i in 2:length(times_ext)) {
                width <- times_ext[i] - times_ext[i-1]
                rmst_var <- rmst_var + width^2 * surv_var_ext[i-1]
            }
            
            return(sqrt(rmst_var))
        },
        
        .calculateRMSTDifferences = function(rmst_results, groups) {
            # Calculate all pairwise differences
            differences <- list()
            
            for (i in 1:(length(groups)-1)) {
                for (j in (i+1):length(groups)) {
                    grp1 <- groups[i]
                    grp2 <- groups[j]
                    
                    rmst1 <- rmst_results[[grp1]]
                    rmst2 <- rmst_results[[grp2]]
                    
                    # Calculate difference
                    diff <- rmst1$rmst - rmst2$rmst
                    se_diff <- sqrt(rmst1$se^2 + rmst2$se^2)
                    
                    # Statistical test
                    z_stat <- diff / se_diff
                    p_value <- 2 * (1 - pnorm(abs(z_stat)))
                    
                    # Confidence interval
                    ci_level <- self$options$confidence_level
                    alpha <- 1 - ci_level
                    z_score <- qnorm(1 - alpha/2)
                    ci_lower <- diff - z_score * se_diff
                    ci_upper <- diff + z_score * se_diff
                    
                    differences[[paste(grp1, "vs", grp2)]] <- list(
                        comparison = paste(grp1, "vs", grp2),
                        difference = diff,
                        se = se_diff,
                        statistic = z_stat,
                        p = p_value,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper
                    )
                }
            }
            
            # Apply multiple comparison adjustment
            p_values <- sapply(differences, function(x) x$p)
            if (self$options$adjustment_method != "none") {
                adj_p_values <- p.adjust(p_values, method = switch(
                    self$options$adjustment_method,
                    "bonferroni" = "bonferroni",
                    "holm" = "holm",
                    "benjamini_hochberg" = "BH"
                ))
            } else {
                adj_p_values <- p_values
            }
            
            # Populate differences table
            for (i in seq_along(differences)) {
                diff_info <- differences[[i]]
                diff_info$adj_p <- adj_p_values[i]
                
                self$results$rmstDifferences$addRow(
                    rowKey = diff_info$comparison, 
                    values = diff_info
                )
            }
        },
        
        .performRMSTRegression = function(data, tau) {
            if (length(self$options$explanatory) == 0) {
                return()
            }
            
            if (self$options$regression_method == "pseudo_observation") {
                private$.performPseudoObservationRegression(data, tau)
            } else if (self$options$regression_method == "direct_modeling") {
                private$.performDirectModeling(data, tau)
            } else if (self$options$regression_method == "wei_lin_ying") {
                private$.performWeiLinYingRegression(data, tau)
            }
        },
        
        .performPseudoObservationRegression = function(data, tau) {
            # Calculate pseudo-observations for RMST
            n <- nrow(data)
            pseudo_values <- numeric(n)
            
            # Original RMST
            rmst_original <- private$.calculateRMST(data, tau)$rmst
            
            # Jackknife pseudo-observations
            for (i in 1:n) {
                data_jack <- data[-i, ]
                rmst_jack <- private$.calculateRMST(data_jack, tau)$rmst
                pseudo_values[i] <- n * rmst_original - (n - 1) * rmst_jack
            }
            
            # Add pseudo-observations to data
            reg_data <- data
            reg_data$pseudo_rmst <- pseudo_values
            
            # Build regression formula
            explanatory_vars <- self$options$explanatory
            formula_str <- paste("pseudo_rmst ~", paste(explanatory_vars, collapse = " + "))
            formula_obj <- as.formula(formula_str)
            
            # Fit linear regression
            model <- lm(formula_obj, data = reg_data)
            
            # Extract results
            model_summary <- summary(model)
            coeffs <- model_summary$coefficients
            
            # Calculate confidence intervals
            ci_level <- self$options$confidence_level
            ci <- confint(model, level = ci_level)
            
            # Populate results table
            for (i in 1:nrow(coeffs)) {
                row <- list(
                    term = rownames(coeffs)[i],
                    estimate = coeffs[i, "Estimate"],
                    se = coeffs[i, "Std. Error"],
                    statistic = coeffs[i, "t value"],
                    p = coeffs[i, "Pr(>|t|)"],
                    ci_lower = ci[i, 1],
                    ci_upper = ci[i, 2]
                )
                
                self$results$regressionResults$addRow(rowKey = rownames(coeffs)[i], values = row)
            }
            
            # Model diagnostics
            if (self$options$show_model_diagnostics) {
                private$.addRegressionDiagnostics(model)
            }
        },
        
        .performDirectModeling = function(data, tau) {
            # Direct modeling approach using survival regression
            if (!requireNamespace("survival", quietly = TRUE)) {
                return()
            }
            
            # Create modified data for direct RMST modeling
            # This is a simplified approach - full implementation would be more complex
            data$time_restricted <- pmin(data$time, tau)
            data$event_restricted <- ifelse(data$time <= tau, data$event, 1)
            
            # Fit parametric survival model
            explanatory_vars <- self$options$explanatory
            formula_str <- paste("Surv(time_restricted, event_restricted) ~", paste(explanatory_vars, collapse = " + "))
            formula_obj <- as.formula(formula_str)
            
            # Try different distributions
            model <- tryCatch({
                survival::survreg(formula_obj, data = data, dist = "weibull")
            }, error = function(e) {
                survival::survreg(formula_obj, data = data, dist = "exponential")
            })
            
            # Extract coefficients (transformed for interpretability)
            coeffs <- summary(model)$table
            
            # Populate results table
            for (i in 1:nrow(coeffs)) {
                row <- list(
                    term = rownames(coeffs)[i],
                    estimate = coeffs[i, "Value"],
                    se = coeffs[i, "Std. Error"],
                    statistic = coeffs[i, "z"],
                    p = coeffs[i, "p"],
                    ci_lower = coeffs[i, "Value"] - 1.96 * coeffs[i, "Std. Error"],
                    ci_upper = coeffs[i, "Value"] + 1.96 * coeffs[i, "Std. Error"]
                )
                
                self$results$regressionResults$addRow(rowKey = rownames(coeffs)[i], values = row)
            }
        },
        
        .performWeiLinYingRegression = function(data, tau) {
            # Wei, Lin & Weissfeld approach for RMST regression
            # This is a placeholder for the full implementation
            # which would require specialized packages or custom implementation
            
            private$.performPseudoObservationRegression(data, tau)  # Fallback
        },
        
        .addRegressionDiagnostics = function(model) {
            # Add model diagnostic information
            r_sq <- summary(model)$r.squared
            adj_r_sq <- summary(model)$adj.r.squared
            aic_val <- AIC(model)
            bic_val <- BIC(model)
            
            diagnostics <- list(
                list(metric = "R-squared", value = r_sq, interpretation = "Proportion of variance explained"),
                list(metric = "Adjusted R-squared", value = adj_r_sq, interpretation = "Adjusted for degrees of freedom"),
                list(metric = "AIC", value = aic_val, interpretation = "Akaike Information Criterion"),
                list(metric = "BIC", value = bic_val, interpretation = "Bayesian Information Criterion")
            )
            
            # Test for normality of residuals
            if (length(residuals(model)) >= 3) {
                norm_test <- shapiro.test(residuals(model))
                diagnostics <- append(diagnostics, list(
                    list(metric = "Residual Normality (p)", value = norm_test$p.value, 
                         interpretation = ifelse(norm_test$p.value > 0.05, "Normal residuals", "Non-normal residuals"))
                ))
            }
            
            for (i in seq_along(diagnostics)) {
                self$results$modelDiagnostics$addRow(rowKey = as.character(i), values = diagnostics[[i]])
            }
        },
        
        .createRMSTPlot = function(data, tau) {
            if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("survival", quietly = TRUE)) {
                return()
            }
            
            # Create survival curves by group
            groups <- levels(data$group)
            plot_data <- data.frame()
            
            time_seq <- seq(0, tau, length.out = 100)
            
            for (grp in groups) {
                grp_data <- data[data$group == grp, ]
                surv_obj <- survival::Surv(grp_data$time, grp_data$event)
                km_fit <- survival::survfit(surv_obj ~ 1)
                
                # Get survival estimates at time sequence
                surv_est <- summary(km_fit, times = time_seq, extend = TRUE)
                
                # Calculate cumulative RMST (area under curve up to each time point)
                cumulative_rmst <- numeric(length(time_seq))
                for (i in 1:length(time_seq)) {
                    t_current <- time_seq[i]
                    if (i == 1) {
                        cumulative_rmst[i] <- 0
                    } else {
                        width <- time_seq[i] - time_seq[i-1]
                        height <- surv_est$surv[i-1]
                        if (is.na(height)) height <- tail(surv_est$surv[!is.na(surv_est$surv)], 1)
                        cumulative_rmst[i] <- cumulative_rmst[i-1] + width * height
                    }
                }
                
                temp_data <- data.frame(
                    time = time_seq,
                    survival = ifelse(is.na(surv_est$surv), tail(surv_est$surv[!is.na(surv_est$surv)], 1), surv_est$surv),
                    cumulative_rmst = cumulative_rmst,
                    group = grp
                )
                
                plot_data <- rbind(plot_data, temp_data)
            }
            
            # Create the plot
            if (length(groups) > 1) {
                p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = survival, color = group)) +
                    ggplot2::geom_line(size = 1) +
                    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = survival, fill = group), alpha = 0.3) +
                    ggplot2::geom_vline(xintercept = tau, linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = paste("Survival Curves and RMST Areas (τ =", tau, ")"),
                        x = "Time",
                        y = "Survival Probability",
                        color = "Group",
                        fill = "Group"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::scale_y_continuous(limits = c(0, 1))
                
                p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = cumulative_rmst, color = group)) +
                    ggplot2::geom_line(size = 1) +
                    ggplot2::geom_vline(xintercept = tau, linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = "Cumulative RMST Over Time",
                        x = "Time",
                        y = "Cumulative RMST",
                        color = "Group"
                    ) +
                    ggplot2::theme_minimal()
                
                # Combine plots
                if (requireNamespace("gridExtra", quietly = TRUE)) {
                    combined_plot <- gridExtra::grid.arrange(p1, p2, ncol = 1)
                    print(combined_plot)
                } else {
                    print(p1)
                }
            } else {
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = survival)) +
                    ggplot2::geom_line(size = 1, color = "blue") +
                    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = survival), alpha = 0.3, fill = "blue") +
                    ggplot2::geom_vline(xintercept = tau, linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = paste("Survival Curve and RMST Area (τ =", tau, ")"),
                        x = "Time",
                        y = "Survival Probability"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::scale_y_continuous(limits = c(0, 1))
                
                print(p)
            }
            
            TRUE
        },
        
        .createDifferencePlot = function(data, tau) {
            if (!requireNamespace("ggplot2", quietly = TRUE) || !is.null(self$options$group_var) == FALSE) {
                return()
            }
            
            # This would create a plot showing RMST differences over time
            # Implementation would calculate RMST differences at multiple tau values
            # and plot the difference trajectory
            
            groups <- levels(data$group)
            if (length(groups) != 2) return()  # Only for two groups
            
            tau_seq <- seq(max(5, min(data$time)), min(tau * 1.5, max(data$time)), length.out = 20)
            diff_data <- data.frame()
            
            for (t in tau_seq) {
                grp1_data <- data[data$group == groups[1], ]
                grp2_data <- data[data$group == groups[2], ]
                
                rmst1 <- private$.calculateRMST(grp1_data, t)
                rmst2 <- private$.calculateRMST(grp2_data, t)
                
                diff <- rmst1$rmst - rmst2$rmst
                se_diff <- sqrt(rmst1$se^2 + rmst2$se^2)
                
                diff_data <- rbind(diff_data, data.frame(
                    tau = t,
                    difference = diff,
                    se = se_diff,
                    ci_lower = diff - 1.96 * se_diff,
                    ci_upper = diff + 1.96 * se_diff
                ))
            }
            
            p <- ggplot2::ggplot(diff_data, ggplot2::aes(x = tau, y = difference)) +
                ggplot2::geom_line(size = 1, color = "blue") +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                ggplot2::geom_vline(xintercept = tau, linetype = "dotted", color = "green") +
                ggplot2::labs(
                    title = paste("RMST Difference Over Time:", groups[1], "vs", groups[2]),
                    x = "Restriction Time (τ)",
                    y = "RMST Difference",
                    subtitle = "Green line shows selected τ"
                ) +
                ggplot2::theme_minimal()
            
            print(p)
            TRUE
        },
        
        .createResidualPlot = function(data, tau) {
            # This would create residual plots for regression diagnostics
            # Placeholder for full implementation
            return()
        },
        
        .initTauSelection = function() {
            table <- self$results$tauSelection
            table$getColumn('method')$setTitle('Method')
            table$getColumn('tau_selected')$setTitle('Selected τ')
            table$getColumn('rationale')$setTitle('Rationale')
            table$getColumn('followup_pct')$setTitle('Follow-up %')
            table$getColumn('events_by_tau')$setTitle('Events by τ')
        },
        
        .initRMSTSummary = function() {
            table <- self$results$rmstSummary
            table$getColumn('group')$setTitle('Group')
            table$getColumn('n')$setTitle('N')
            table$getColumn('events')$setTitle('Events')
            table$getColumn('rmst')$setTitle('RMST')
            table$getColumn('se')$setTitle('SE')
            table$getColumn('ci_lower')$setTitle('Lower CI')
            table$getColumn('ci_upper')$setTitle('Upper CI')
            table$getColumn('tau_reached')$setTitle('τ Reached')
        },
        
        .initRMSTDifferences = function() {
            table <- self$results$rmstDifferences
            table$getColumn('comparison')$setTitle('Comparison')
            table$getColumn('difference')$setTitle('Difference')
            table$getColumn('se')$setTitle('SE')
            table$getColumn('statistic')$setTitle('z')
            table$getColumn('p')$setTitle('p')
            table$getColumn('ci_lower')$setTitle('Lower CI')
            table$getColumn('ci_upper')$setTitle('Upper CI')
            table$getColumn('adj_p')$setTitle('Adj. p')
        },
        
        .initRegressionResults = function() {
            table <- self$results$regressionResults
            table$getColumn('term')$setTitle('Term')
            table$getColumn('estimate')$setTitle('Estimate')
            table$getColumn('se')$setTitle('SE')
            table$getColumn('statistic')$setTitle('t/z')
            table$getColumn('p')$setTitle('p')
            table$getColumn('ci_lower')$setTitle('Lower CI')
            table$getColumn('ci_upper')$setTitle('Upper CI')
        },
        
        .initModelDiagnostics = function() {
            table <- self$results$modelDiagnostics
            table$getColumn('metric')$setTitle('Metric')
            table$getColumn('value')$setTitle('Value')
            table$getColumn('interpretation')$setTitle('Interpretation')
        },
        
        .initBootstrapResults = function() {
            table <- self$results$bootstrapResults
            table$getColumn('parameter')$setTitle('Parameter')
            table$getColumn('original')$setTitle('Original')
            table$getColumn('bootstrap_mean')$setTitle('Bootstrap Mean')
            table$getColumn('bootstrap_se')$setTitle('Bootstrap SE')
            table$getColumn('bias')$setTitle('Bias')
            table$getColumn('ci_type')$setTitle('CI Type')
        },
        
        .populateTauSelection = function(tau_info) {
            row <- list(
                method = tau_info$method,
                tau_selected = tau_info$tau,
                rationale = tau_info$rationale,
                followup_pct = tau_info$followup_pct,
                events_by_tau = tau_info$events_by_tau
            )
            
            self$results$tauSelection$addRow(rowKey = "tau_selection", values = row)
        },
        
        .addMethodologyExplanation = function(tau) {
            html <- paste0('<h3>Restricted Mean Survival Time (RMST) Analysis</h3>
                    <p><b>Overview:</b> RMST provides a clinically interpretable summary measure that represents the mean survival time up to a specified time point τ = ', tau, '.</p>
                    
                    <h4>Method:</h4>
                    <ul>
                    <li><b>Analysis Type:</b> ', self$options$analysis_type, '</li>
                    <li><b>Regression Method:</b> ', self$options$regression_method, '</li>
                    <li><b>Tau Selection:</b> ', self$options$tau_method, '</li>
                    </ul>
                    
                    <h4>Interpretation:</h4>
                    <p><b>RMST (τ = ', tau, '):</b> The expected survival time from 0 to ', tau, ' time units, calculated as the area under the survival curve.</p>
                    
                    <h4>Advantages of RMST:</h4>
                    <ul>
                    <li>Clinically interpretable (mean survival time)</li>
                    <li>No proportional hazards assumption required</li>
                    <li>Robust to late differences in survival</li>
                    <li>Suitable when curves cross</li>
                    <li>Accounts for entire follow-up period up to τ</li>
                    </ul>
                    
                    <h4>Regression Coefficients:</h4>
                    <p>When using regression analysis, coefficients represent the change in RMST (in time units) per unit change in the covariate.</p>')
            
            if (self$options$regression_method == "pseudo_observation") {
                html <- paste0(html, 
                    '<p><b>Pseudo-observation Method:</b> Uses jackknife pseudo-observations to transform the RMST estimation into a standard regression framework.</p>')
            }
            
            self$results$methodologyExplanation$setContent(html)
        },
        
        .addAnalysisSummary = function(data, tau) {
            groups <- levels(data$group)
            
            html <- paste0('<h3>Analysis Summary</h3>
                          <p><b>Sample Size:</b> ', nrow(data), ' observations</p>
                          <p><b>Number of Events:</b> ', sum(data$event), '</p>
                          <p><b>Restriction Time (τ):</b> ', tau, '</p>')
            
            if (length(groups) > 1) {
                html <- paste0(html, '<p><b>Number of Groups:</b> ', length(groups), '</p>
                              <p><b>Groups:</b> ', paste(groups, collapse = ", "), '</p>')
            }
            
            if (length(self$options$explanatory) > 0) {
                html <- paste0(html, '<p><b>Explanatory Variables:</b> ', paste(self$options$explanatory, collapse = ", "), '</p>')
            }
            
            # Add interpretation guidance
            html <- paste0(html, '<h4>Clinical Interpretation:</h4>
                          <p>RMST represents the expected survival time within the first ', tau, ' time units. 
                          A difference in RMST between groups indicates how many additional time units, on average, 
                          one group survives compared to another within this period.</p>')
            
            if (self$options$analysis_type %in% c("regression", "combined")) {
                html <- paste0(html, '<p><b>Regression Analysis:</b> Coefficient estimates represent the change in RMST 
                              (in time units) associated with a one-unit change in the covariate, holding other variables constant.</p>')
            }
            
            self$results$analysisSummary$setContent(html)
        }
    )
)