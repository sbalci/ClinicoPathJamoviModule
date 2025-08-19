# Restricted Mean Survival Time Tests
# This file is part of ClinicoPath

rmstClass <- R6::R6Class(
    "rmstClass",
    inherit = rmstBase,
    private = list(
        .init = function() {
            
            private$.initTodo()
        },
        
        .initTodo = function() {
            todo <- glue::glue(
                "<b>Restricted Mean Survival Time Tests</b>
                <br><br>
                Select variables:
                <br>• Time Variable: {ifelse(is.null(self$options$elapsedtime), '❌ Not selected', '✅ Selected')}
                <br>• Event Variable: {ifelse(is.null(self$options$outcome), '❌ Not selected', '✅ Selected')}  
                <br>• Group Variable: {ifelse(is.null(self$options$explanatory), '❌ Not selected', '✅ Selected')}
                <br><br>
                <b>About RMST Analysis:</b>
                <br>• Compares average survival time within a specified period (tau)
                <br>• More intuitive than hazard ratios for clinical interpretation
                <br>• Provides absolute difference in restricted mean survival time
                <br>• Robust alternative when proportional hazards assumption is violated
                "
            )
            
            self$results$todo$setContent(todo)
        },
        
        .run = function() {
            
            # Early return if variables not selected
            if (is.null(self$options$elapsedtime) || 
                is.null(self$options$outcome) || 
                is.null(self$options$explanatory)) {
                return()
            }
            
            # Prepare data
            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()
            
            # Determine tau value
            tau <- private$.determineTau(prepared_data)
            if (is.null(tau)) return()
            
            # Calculate RMST estimates
            rmst_results <- private$.calculateRMST(prepared_data, tau)
            if (is.null(rmst_results)) return()
            
            # Populate results tables
            private$.populateRMSTTable(rmst_results)
            
            # Perform group comparisons
            if (self$options$show_difference_test || self$options$show_ratio_test) {
                comparison_results <- private$.performComparisons(prepared_data, tau)
                if (!is.null(comparison_results)) {
                    private$.populateComparisonTable(comparison_results)
                }
            }
            
            # Create plots
            if (self$options$show_rmst_plot) {
                private$.createRMSTPlot(prepared_data, tau, rmst_results)
            }
            
            if (self$options$show_tau_analysis) {
                private$.createTauAnalysis(prepared_data)
            }
            
            # Generate summaries
            if (self$options$showSummaries) {
                private$.generateSummary(rmst_results, tau)
            }
            
            if (self$options$showExplanations) {
                private$.generateExplanation()
            }
        },
        
        .prepareData = function() {
            
            # Get data
            time_var <- self$options$elapsedtime
            event_var <- self$options$outcome 
            group_var <- self$options$explanatory
            
            if (is.null(time_var) || is.null(event_var) || is.null(group_var)) {
                return(NULL)
            }
            
            mydata <- self$data
            
            # Extract variables
            time <- jmvcore::toNumeric(mydata[[time_var]])
            event <- mydata[[event_var]]
            group <- mydata[[group_var]]
            
            # Handle outcome level
            outcome_level <- self$options$outcomeLevel
            if (is.factor(event)) {
                event_binary <- ifelse(event == outcome_level, 1, 0)
            } else {
                event_binary <- ifelse(event == as.numeric(outcome_level), 1, 0)
            }
            
            # Remove missing data
            complete_cases <- complete.cases(time, event_binary, group)
            if (sum(complete_cases) == 0) {
                jmvcore::reject("No complete cases available for analysis")
                return(NULL)
            }
            
            # Create clean dataset
            clean_data <- data.frame(
                time = time[complete_cases],
                event = event_binary[complete_cases],
                group = group[complete_cases]
            )
            
            # Ensure group is factor
            clean_data$group <- as.factor(clean_data$group)
            
            # Validate data
            if (length(levels(clean_data$group)) < 2) {
                jmvcore::reject("At least 2 groups are required for comparison")
                return(NULL)
            }
            
            if (sum(clean_data$event) == 0) {
                jmvcore::reject("No events observed in the data")
                return(NULL)
            }
            
            # Create survival object
            clean_data$surv <- survival::Surv(clean_data$time, clean_data$event)
            
            return(clean_data)
        },
        
        .determineTau = function(data) {
            
            tau_method <- self$options$tau_method
            
            if (tau_method == "manual") {
                tau <- self$options$tau_value
            } else if (tau_method == "percentile") {
                max_time <- max(data$time)
                percentile <- self$options$tau_percentile / 100
                tau <- max_time * percentile
            } else { # auto
                # Use minimum of maximum observed times per group
                max_times_by_group <- tapply(data$time, data$group, max)
                tau <- min(max_times_by_group)
            }
            
            # Validate tau
            min_time <- min(data$time)
            max_time <- max(data$time)
            
            if (tau <= min_time) {
                jmvcore::reject(glue::glue("Tau ({tau}) must be greater than minimum time ({min_time})"))
                return(NULL)
            }
            
            if (tau > max_time) {
                jmvcore::reject(glue::glue("Tau ({tau}) cannot exceed maximum follow-up time ({max_time})"))
                return(NULL)
            }
            
            return(tau)
        },
        
        .calculateRMST = function(data, tau) {
            
            results_list <- list()
            groups <- levels(data$group)
            
            for (group in groups) {
                group_data <- data[data$group == group, ]
                
                # Fit Kaplan-Meier
                km_fit <- survival::survfit(surv ~ 1, data = group_data)
                
                # Calculate RMST using integration
                rmst_result <- private$.calculateRMSTIntegral(km_fit, tau)
                
                results_list[[group]] <- list(
                    group = group,
                    n = nrow(group_data),
                    events = sum(group_data$event),
                    rmst = rmst_result$rmst,
                    rmst_se = rmst_result$se,
                    rmst_var = rmst_result$var,
                    km_fit = km_fit
                )
            }
            
            # Add confidence intervals
            conf_level <- self$options$confidence_level
            z_alpha <- qnorm(1 - (1 - conf_level) / 2)
            
            for (group in names(results_list)) {
                result <- results_list[[group]]
                margin <- z_alpha * result$rmst_se
                results_list[[group]]$rmst_lower <- result$rmst - margin
                results_list[[group]]$rmst_upper <- result$rmst + margin
            }
            
            return(results_list)
        },
        
        .calculateRMSTIntegral = function(km_fit, tau) {
            
            # Extract survival function
            times <- c(0, km_fit$time)
            survival <- c(1, km_fit$surv)
            
            # Truncate at tau
            tau_index <- which(times <= tau)
            times_trunc <- times[tau_index]
            survival_trunc <- survival[tau_index]
            
            # Add tau point if needed
            if (max(times_trunc) < tau) {
                # Linear interpolation to get survival at tau
                surv_at_tau <- approx(times, survival, xout = tau, rule = 2)$y
                times_trunc <- c(times_trunc, tau)
                survival_trunc <- c(survival_trunc, surv_at_tau)
            }
            
            # Calculate RMST using trapezoidal rule
            if (length(times_trunc) > 1) {
                dt <- diff(times_trunc)
                avg_surv <- (survival_trunc[-length(survival_trunc)] + survival_trunc[-1]) / 2
                rmst <- sum(dt * avg_surv)
            } else {
                rmst <- 0
            }
            
            # Calculate variance using Greenwood's formula
            # This is an approximation for RMST variance
            n_risk <- km_fit$n.risk
            n_events <- km_fit$n.event
            
            # Greenwood variance for survival function
            greenwood_var <- cumsum(n_events / (n_risk * (n_risk - n_events)))
            
            # Approximate RMST variance (simplified)
            # In practice, this would need more sophisticated variance calculation
            rmst_var <- ifelse(length(greenwood_var) > 0, 
                              mean(greenwood_var[times <= tau]) * (tau^2 / 4), 
                              0)
            rmst_se <- sqrt(rmst_var)
            
            return(list(rmst = rmst, se = rmst_se, var = rmst_var))
        },
        
        .populateRMSTTable = function(rmst_results) {
            
            table <- self$results$rmstTable
            
            for (group_name in names(rmst_results)) {
                result <- rmst_results[[group_name]]
                
                table$addRow(rowKey = group_name, values = list(
                    group = result$group,
                    n = result$n,
                    events = result$events,
                    rmst = result$rmst,
                    rmst_se = result$rmst_se,
                    rmst_lower = result$rmst_lower,
                    rmst_upper = result$rmst_upper
                ))
            }
        },
        
        .performComparisons = function(data, tau) {
            
            groups <- levels(data$group)
            if (length(groups) != 2) {
                # For now, only support 2-group comparisons
                return(NULL)
            }
            
            # Calculate RMST for each group
            rmst_results <- private$.calculateRMST(data, tau)
            
            group1 <- names(rmst_results)[1]
            group2 <- names(rmst_results)[2]
            
            rmst1 <- rmst_results[[group1]]
            rmst2 <- rmst_results[[group2]]
            
            results <- list()
            
            # Difference test
            if (self$options$show_difference_test) {
                diff <- rmst1$rmst - rmst2$rmst
                se_diff <- sqrt(rmst1$rmst_var + rmst2$rmst_var)
                z_score <- diff / se_diff
                p_value <- 2 * (1 - pnorm(abs(z_score)))
                
                conf_level <- self$options$confidence_level
                z_alpha <- qnorm(1 - (1 - conf_level) / 2)
                margin <- z_alpha * se_diff
                
                results$difference <- list(
                    comparison = glue::glue("{group1} - {group2}"),
                    test_type = "Difference",
                    estimate = diff,
                    se = se_diff,
                    z_score = z_score,
                    p_value = p_value,
                    ci_lower = diff - margin,
                    ci_upper = diff + margin
                )
            }
            
            # Ratio test
            if (self$options$show_ratio_test) {
                ratio <- rmst1$rmst / rmst2$rmst
                
                # Delta method for ratio variance
                var_ratio <- ratio^2 * (rmst1$rmst_var / rmst1$rmst^2 + rmst2$rmst_var / rmst2$rmst^2)
                se_ratio <- sqrt(var_ratio)
                
                # Use log transformation for CI
                log_ratio <- log(ratio)
                se_log_ratio <- se_ratio / ratio
                z_score <- log_ratio / se_log_ratio
                p_value <- 2 * (1 - pnorm(abs(z_score)))
                
                conf_level <- self$options$confidence_level
                z_alpha <- qnorm(1 - (1 - conf_level) / 2)
                margin_log <- z_alpha * se_log_ratio
                
                results$ratio <- list(
                    comparison = glue::glue("{group1} / {group2}"),
                    test_type = "Ratio",
                    estimate = ratio,
                    se = se_ratio,
                    z_score = z_score,
                    p_value = p_value,
                    ci_lower = exp(log_ratio - margin_log),
                    ci_upper = exp(log_ratio + margin_log)
                )
            }
            
            return(results)
        },
        
        .populateComparisonTable = function(comparison_results) {
            
            table <- self$results$comparisonTable
            
            for (test_name in names(comparison_results)) {
                result <- comparison_results[[test_name]]
                
                table$addRow(rowKey = test_name, values = list(
                    comparison = result$comparison,
                    test_type = result$test_type,
                    estimate = result$estimate,
                    se = result$se,
                    z_score = result$z_score,
                    p_value = result$p_value,
                    ci_lower = result$ci_lower,
                    ci_upper = result$ci_upper
                ))
            }
        },
        
        .createRMSTPlot = function(data, tau, rmst_results) {
            
            if (!requireNamespace("ggplot2", quietly = TRUE) ||
                !requireNamespace("survminer", quietly = TRUE)) {
                return()
            }
            
            # Create survival plot with RMST areas
            fit <- survival::survfit(surv ~ group, data = data)
            
            plot <- survminer::ggsurvplot(
                fit,
                data = data,
                title = glue::glue("Survival Curves with RMST (tau = {round(tau, 2)})"),
                xlab = "Time",
                ylab = "Survival Probability",
                legend.title = "Group",
                conf.int = TRUE,
                xlim = c(0, tau * 1.1),
                break.x.by = tau / 5,
                ggtheme = ggplot2::theme_minimal()
            )
            
            # Add vertical line at tau
            plot$plot <- plot$plot + 
                ggplot2::geom_vline(xintercept = tau, linetype = "dashed", color = "red", alpha = 0.7) +
                ggplot2::annotate("text", x = tau, y = 0.1, label = glue::glue("tau = {round(tau, 2)}"), 
                                 color = "red", hjust = -0.1)
            
            # Create and save plot
            image <- self$results$rmstPlot
            image$setState(plot)
        },
        
        .createTauAnalysis = function(data) {
            
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return()
            }
            
            # Create range of tau values for sensitivity analysis
            max_time <- max(data$time)
            tau_values <- seq(from = max_time * 0.2, to = max_time * 0.9, length.out = 20)
            
            sensitivity_results <- data.frame(
                tau = numeric(0),
                rmst_diff = numeric(0),
                p_value = numeric(0),
                ci_lower = numeric(0),
                ci_upper = numeric(0)
            )
            
            for (tau in tau_values) {
                tryCatch({
                    comparison <- private$.performComparisons(data, tau)
                    if (!is.null(comparison$difference)) {
                        sensitivity_results <- rbind(sensitivity_results, data.frame(
                            tau = tau,
                            rmst_diff = comparison$difference$estimate,
                            p_value = comparison$difference$p_value,
                            ci_lower = comparison$difference$ci_lower,
                            ci_upper = comparison$difference$ci_upper
                        ))
                    }
                }, error = function(e) {
                    # Skip this tau value if calculation fails
                })
            }
            
            # Populate sensitivity table
            if (nrow(sensitivity_results) > 0) {
                table <- self$results$tauAnalysisTable
                
                for (i in 1:nrow(sensitivity_results)) {
                    table$addRow(rowKey = i, values = list(
                        tau = sensitivity_results$tau[i],
                        rmst_diff = sensitivity_results$rmst_diff[i],
                        p_value = sensitivity_results$p_value[i],
                        ci_lower = sensitivity_results$ci_lower[i],
                        ci_upper = sensitivity_results$ci_upper[i]
                    ))
                }
                
                # Create sensitivity plot
                plot <- ggplot2::ggplot(sensitivity_results, ggplot2::aes(x = tau, y = rmst_diff)) +
                    ggplot2::geom_line(color = "blue", size = 1) +
                    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), 
                                        alpha = 0.2, fill = "blue") +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                    ggplot2::labs(
                        title = "RMST Difference Sensitivity Analysis",
                        x = "Tau (Restriction Time)",
                        y = "RMST Difference"
                    ) +
                    ggplot2::theme_minimal()
                
                image <- self$results$tauAnalysisPlot
                image$setState(plot)
            }
        },
        
        .generateSummary = function(rmst_results, tau) {
            
            groups <- names(rmst_results)
            
            summary_text <- glue::glue(
                "<b>Restricted Mean Survival Time Analysis Summary</b>
                <br><br>
                <b>Restriction Time (tau):</b> {round(tau, 2)}
                <br><br>
                <b>Group Results:</b>
                <br>"
            )
            
            for (group in groups) {
                result <- rmst_results[[group]]
                summary_text <- paste0(summary_text, glue::glue(
                    "• {group}: RMST = {round(result$rmst, 2)} 
                    (95% CI: {round(result$rmst_lower, 2)} - {round(result$rmst_upper, 2)}),
                    N = {result$n}, Events = {result$events}<br>"
                ))
            }
            
            if (length(groups) == 2) {
                diff <- rmst_results[[groups[1]]]$rmst - rmst_results[[groups[2]]]$rmst
                summary_text <- paste0(summary_text, glue::glue(
                    "<br><b>Group Comparison:</b>
                    <br>• Difference in RMST: {round(diff, 2)} time units
                    <br>• {groups[1]} has {ifelse(diff > 0, 'longer', 'shorter')} average survival time within tau
                    "
                ))
            }
            
            self$results$analysisSummary$setContent(summary_text)
        },
        
        .generateExplanation = function() {
            
            explanation <- glue::glue(
                "<b>Restricted Mean Survival Time (RMST) Analysis</b>
                <br><br>
                <b>Method Overview:</b>
                <br>RMST measures the area under the survival curve up to a specified time point (tau), 
                representing the average survival time within the restriction period. It provides a 
                clinically meaningful alternative to hazard ratios.
                <br><br>
                <b>Key Advantages:</b>
                <br>• Direct clinical interpretation as average survival time
                <br>• Robust when proportional hazards assumption is violated
                <br>• Provides absolute rather than relative treatment effects
                <br>• Less sensitive to long-term tail behavior of survival curves
                <br><br>
                <b>Tau Selection:</b>
                <br>• Should be chosen based on clinical relevance
                <br>• Commonly set to a clinically meaningful follow-up period
                <br>• Can be automatic (minimum of group maxima) or manual
                <br><br>
                <b>Interpretation:</b>
                <br>• RMST difference: Absolute difference in average survival time
                <br>• RMST ratio: Relative difference in average survival time
                <br>• Confidence intervals provide uncertainty quantification
                "
            )
            
            self$results$methodExplanation$setContent(explanation)
        }
    )
)
