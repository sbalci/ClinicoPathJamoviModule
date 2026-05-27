#' @title Median Survival Comparisons
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import survminer
#' @import ggplot2
#'

mediansurvivalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mediansurvivalClass",
    inherit = mediansurvivalBase,
    private = list(
        .init = function() {
            private$.initResults()
        },
        
        .run = function() {
            # Get variables
            elapsedtime <- self$options$elapsedtime
            outcome <- self$options$outcome  
            explanatory <- self$options$explanatory
            
            if (is.null(elapsedtime) || is.null(outcome)) {
                self$results$todo$setContent("<p>Please specify both Time Variable and Event Indicator to proceed with the analysis.</p>")
                return()
            }
            
            # Prepare data
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            # Fit survival models
            if (!is.null(explanatory)) {
                survival_results <- private$.performGroupComparison(data)
            } else {
                survival_results <- private$.performSingleGroup(data)
            }
            
            # Populate results
            private$.populateMedianTable(survival_results)
            private$.populateComparisonTable(survival_results)
            private$.populatePlots(survival_results, data)
            private$.populateSummary(survival_results)
            private$.populateMethodology()
        },
        
        .initResults = function() {
            # Initialize todo content
            self$results$todo$setContent("<p>Configure the analysis options and run to see median survival comparison results.</p>")
        },
        
        .prepareData = function() {
            # Extract variables from data
            data <- self$data
            
            elapsedtime_name <- self$options$elapsedtime
            outcome_name <- self$options$outcome
            explanatory_name <- self$options$explanatory
            
            if (is.null(elapsedtime_name) || is.null(outcome_name)) return(NULL)
            
            # Create analysis dataset
            analysis_data <- data.frame(
                time = data[[elapsedtime_name]],
                event = data[[outcome_name]]
            )
            
            # Add grouping variable if specified
            if (!is.null(explanatory_name)) {
                analysis_data$group <- data[[explanatory_name]]
            }
            
            # Handle outcome coding — honor jamovi values attribute on labelled factors
            outcome_level <- self$options$outcomeLevel
            if (is.factor(analysis_data$event)) {
                event_numeric <- jmvcore::toNumeric(analysis_data$event)
                level_value <- suppressWarnings(as.numeric(outcome_level))
                if (!is.na(level_value)) {
                    analysis_data$event <- as.integer(event_numeric == level_value)
                } else {
                    analysis_data$event <- as.integer(as.character(analysis_data$event) == outcome_level)
                }
            } else if (is.character(analysis_data$event)) {
                analysis_data$event <- as.integer(analysis_data$event == outcome_level)
            } else {
                level_value <- suppressWarnings(as.numeric(outcome_level))
                analysis_data$event <- as.integer(analysis_data$event == level_value)
            }
            
            # Remove missing values
            analysis_data <- analysis_data[complete.cases(analysis_data), ]
            
            if (nrow(analysis_data) == 0) {
                self$results$todo$setContent("<p>No complete cases available for analysis after removing missing values.</p>")
                return(NULL)
            }
            
            return(analysis_data)
        },
        
        .performSingleGroup = function(data) {
            # Fit single group survival model
            surv_obj <- Surv(data$time, data$event)
            km_fit <- survfit(surv_obj ~ 1)
            
            # Extract median with confidence intervals
            median_surv <- private$.extractMedianSurvival(km_fit)
            
            return(list(
                km_fit = km_fit,
                median_results = median_surv,
                comparison_tests = NULL,
                groups = "Overall"
            ))
        },
        
        .performGroupComparison = function(data) {
            # Fit grouped survival model
            surv_obj <- Surv(data$time, data$event)
            km_fit <- survfit(surv_obj ~ group, data = data)
            
            # Extract median survival for each group
            median_surv <- private$.extractGroupedMedianSurvival(km_fit, data)
            
            # Perform comparison tests
            comparison_tests <- private$.performComparisonTests(data)
            
            return(list(
                km_fit = km_fit,
                median_results = median_surv,
                comparison_tests = comparison_tests,
                groups = unique(data$group)
            ))
        },
        
        .extractMedianSurvival = function(km_fit) {
            # Extract median survival with confidence intervals
            quantiles <- quantile(km_fit, probs = 0.5)
            conf_method <- self$options$confidence_method
            conf_level <- self$options$confidence_level
            
            median_val <- quantiles$quantile[1]
            lower_ci <- quantiles$lower[1]
            upper_ci <- quantiles$upper[1]
            
            # Handle different confidence interval methods
            if (conf_method != "brookmeyer_crowley") {
                # TODO (correctness): the refit below uses `km_fit$time` and `km_fit$n.event > 0`
                # from the *fitted* survfit object as if they were original data. That's not
                # equivalent to refitting on the original data with a different conf.type — it
                # collapses ties/censoring into unique event timepoints with synthetic 0/1 events
                # and recomputes CIs on a different dataset. Pass the original `data$time`/`data$event`
                # (or build survfit with the desired `conf.type` in `.performSingleGroup` directly).
                km_fit_method <- survfit(Surv(km_fit$time, km_fit$n.event > 0) ~ 1,
                                       conf.type = private$.mapConfMethod(conf_method),
                                       conf.int = conf_level)
                quantiles_method <- quantile(km_fit_method, probs = 0.5)
                lower_ci <- quantiles_method$lower[1]
                upper_ci <- quantiles_method$upper[1]
            }
            
            return(data.frame(
                group = "Overall",
                median = median_val,
                lower_ci = lower_ci,
                upper_ci = upper_ci,
                n_events = sum(km_fit$n.event),
                n_total = km_fit$n[1],
                stringsAsFactors = FALSE
            ))
        },
        
        .extractGroupedMedianSurvival = function(km_fit, data) {
            # Extract median survival for each group
            quantiles <- quantile(km_fit, probs = 0.5)
            
            group_names <- names(km_fit$strata)
            # TODO (correctness): `gsub("group=", "", x)` is fragile — a factor level that itself
            # contains "group=" or "=" (e.g. "sex=M") is silently mangled. Prefer `sub("^[^=]+=", "", x)`
            # or use `survminer::surv_summary()` / the structured strata names from `km_fit$strata`.
            group_names <- gsub("group=", "", group_names)
            
            results <- data.frame(
                group = group_names,
                median = quantiles$quantile,
                lower_ci = quantiles$lower,
                upper_ci = quantiles$upper,
                stringsAsFactors = FALSE
            )
            
            # Add sample sizes and event counts
            for (i in seq_along(group_names)) {
                group_data <- data[data$group == group_names[i], ]
                results$n_events[i] <- sum(group_data$event)
                results$n_total[i] <- nrow(group_data)
            }
            
            return(results)
        },
        
        .performComparisonTests = function(data) {
            test_method <- self$options$test_method
            
            # Create survival object
            surv_obj <- Surv(data$time, data$event)
            
            results <- list()
            
            if (test_method == "logrank" || test_method == "all") {
                logrank_test <- survdiff(surv_obj ~ group, data = data)
                results$logrank <- private$.extractTestResults(logrank_test, "Log-rank")
            }
            
            # TODO (correctness): the `wilcoxon` and `petopeto` branches below both call
            # `survdiff(... rho = 1)` — that's the *same* test (Gehan-Breslow weights) under two
            # different UI labels. With `test_method = "all"` the user sees identical χ² and p
            # in two rows. Either (a) implement true Peto-Peto via `coin::logrank_test(... type = "Peto-Peto")`
            # or `survminer::pairwise_survdiff(..., p.adjust.method = ...)` with the appropriate
            # weight family, or (b) collapse the UI to one option.
            if (test_method == "wilcoxon" || test_method == "all") {
                wilcoxon_test <- survdiff(surv_obj ~ group, data = data, rho = 1)
                results$wilcoxon <- private$.extractTestResults(wilcoxon_test, "Wilcoxon")
            }

            if (test_method == "petopeto" || test_method == "all") {
                petopeto_test <- survdiff(surv_obj ~ group, data = data, rho = 1)
                results$petopeto <- private$.extractTestResults(petopeto_test, "Peto-Peto")
            }
            
            # Perform pairwise comparisons if more than 2 groups
            groups <- unique(data$group)
            if (length(groups) > 2) {
                results$pairwise <- private$.performPairwiseComparisons(data, groups)
            }
            
            return(results)
        },
        
        .extractTestResults = function(test_result, method_name) {
            chi_square <- test_result$chisq
            df <- length(test_result$n) - 1
            p_value <- 1 - pchisq(chi_square, df)
            
            return(data.frame(
                comparison = "Overall",
                test_statistic = chi_square,
                p_value = p_value,
                method = method_name,
                stringsAsFactors = FALSE
            ))
        },
        
        .performPairwiseComparisons = function(data, groups) {
            # Perform all pairwise comparisons
            n_groups <- length(groups)
            comparisons <- combn(groups, 2, simplify = FALSE)
            
            results <- data.frame(
                comparison = character(),
                test_statistic = numeric(),
                p_value = numeric(),
                method = character(),
                stringsAsFactors = FALSE
            )
            
            for (comp in comparisons) {
                subset_data <- data[data$group %in% comp, ]
                surv_obj <- Surv(subset_data$time, subset_data$event)
                
                test_result <- survdiff(surv_obj ~ group, data = subset_data)
                
                comparison_name <- paste(comp[1], "vs", comp[2])
                chi_square <- test_result$chisq
                p_value <- 1 - pchisq(chi_square, 1)
                
                results <- rbind(results, data.frame(
                    comparison = comparison_name,
                    test_statistic = chi_square,
                    p_value = p_value,
                    method = self$options$test_method,
                    stringsAsFactors = FALSE
                ))
            }
            
            # Apply multiple comparison correction
            multiple_method <- self$options$multiple_comparison
            if (multiple_method != "none") {
                results$p_adjusted <- p.adjust(results$p_value, method = multiple_method)
            } else {
                results$p_adjusted <- results$p_value
            }
            
            return(results)
        },
        
        .mapConfMethod = function(method) {
            switch(method,
                   "brookmeyer_crowley" = "log",
                   "log_transform" = "log", 
                   "loglog_transform" = "log-log",
                   "plain" = "plain",
                   "log")
        },
        
        .populateMedianTable = function(results) {
            table <- self$results$medianTable
            
            median_data <- results$median_results
            
            for (i in seq_len(nrow(median_data))) {
                row <- median_data[i, ]
                table$addRow(rowKey = i, values = list(
                    group = row$group,
                    median = row$median,
                    lower_ci = row$lower_ci,
                    upper_ci = row$upper_ci,
                    n_events = row$n_events,
                    n_total = row$n_total
                ))
            }
        },
        
        .populateComparisonTable = function(results) {
            if (is.null(results$comparison_tests)) return()
            
            table <- self$results$comparisonTable
            
            # Add overall test results
            for (test_name in names(results$comparison_tests)) {
                if (test_name != "pairwise") {
                    test_data <- results$comparison_tests[[test_name]]
                    table$addRow(rowKey = test_name, values = list(
                        comparison = test_data$comparison,
                        test_statistic = test_data$test_statistic,
                        p_value = test_data$p_value,
                        p_adjusted = test_data$p_value,
                        method = test_data$method
                    ))
                }
            }
            
            # Add pairwise comparisons if available
            if ("pairwise" %in% names(results$comparison_tests)) {
                pairwise_data <- results$comparison_tests$pairwise
                for (i in seq_len(nrow(pairwise_data))) {
                    row <- pairwise_data[i, ]
                    table$addRow(rowKey = paste0("pairwise_", i), values = list(
                        comparison = row$comparison,
                        test_statistic = row$test_statistic,
                        p_value = row$p_value,
                        p_adjusted = row$p_adjusted,
                        method = row$method
                    ))
                }
            }
        },
        
        .populatePlots = function(results, data) {
            # Survival plot with median indicators
            if (self$options$show_survival_plot) {
                survival_plot <- private$.createSurvivalPlot(results, data)
                self$results$survivalPlot$setState(survival_plot)
            }
            
            # Median comparison plot
            median_plot <- private$.createMedianComparisonPlot(results)
            self$results$medianComparisonPlot$setState(median_plot)
        },
        
        .createSurvivalPlot = function(results, data) {
            km_fit <- results$km_fit
            
            # Create base plot
            p <- ggsurvplot(
                km_fit, 
                data = data,
                conf.int = self$options$show_confidence_bands,
                risk.table = self$options$show_risk_table,
                palette = "jco"
            )
            
            # Add median lines if requested
            if (self$options$show_median_lines) {
                median_data <- results$median_results
                for (i in seq_len(nrow(median_data))) {
                    if (!is.na(median_data$median[i])) {
                        p$plot <- p$plot + 
                            geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.7) +
                            geom_vline(xintercept = median_data$median[i], linetype = "dashed", alpha = 0.7)
                    }
                }
            }
            
            return(p$plot)
        },
        
        .createMedianComparisonPlot = function(results) {
            median_data <- results$median_results
            
            p <- ggplot(median_data, aes(x = group, y = median)) +
                geom_point(size = 3) +
                geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
                labs(
                    title = "Median Survival Comparison",
                    x = "Group",
                    y = "Median Survival Time"
                ) +
                theme_minimal()
            
            return(p)
        },
        
        .populateSummary = function(results) {
            if (!self$options$showSummaries) return()
            
            median_data <- results$median_results
            
            summary_html <- "<h3>Analysis Summary</h3><ul>"
            
            for (i in seq_len(nrow(median_data))) {
                row <- median_data[i, ]
                summary_html <- paste0(summary_html,
                    "<li>Group '", htmltools::htmlEscape(as.character(row$group)), "': Median survival = ",
                    round(row$median, 2),
                    " (95% CI: ", round(row$lower_ci, 2), " - ", round(row$upper_ci, 2), ")",
                    " with ", row$n_events, " events out of ", row$n_total, " subjects</li>")
            }
            
            summary_html <- paste0(summary_html, "</ul>")
            
            self$results$analysisSummary$setContent(summary_html)
        },
        
        .populateMethodology = function() {
            if (!self$options$showExplanations) return()
            
            methodology_html <- "
            <h3>Median Survival Analysis Methodology</h3>
            <p><strong>Median Survival Time:</strong> The time at which 50% of subjects experience the event of interest.</p>
            <p><strong>Confidence Intervals:</strong> Calculated using the selected method to provide uncertainty estimates around the median.</p>
            <p><strong>Statistical Tests:</strong> Compare median survival times between groups using the selected test method.</p>
            <p><strong>Multiple Comparisons:</strong> Adjustments applied when comparing multiple groups to control family-wise error rate.</p>"
            
            self$results$methodExplanation$setContent(methodology_html)
        }
    )
)