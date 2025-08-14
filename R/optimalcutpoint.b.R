# This file is a generated template, your changes will not be overwritten

#' @import jmvcore
#' @import R6
#' @import survival
#' @import survminer
#' @importFrom maxstat maxstat.test
#' @importFrom cutpointr cutpointr roc_curve add_cutpointr_metrics
#' @importFrom pROC roc coords
#' @importFrom timeROC timeROC
#' @importFrom stats quantile median
#' @importFrom utils capture.output
#' @export


optimalcutpointClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "optimalcutpointClass",
    inherit = optimalcutpointBase,
    private = list(
        .init = function() {
            
            # Initialize results tables with proper structure
            private$.initCutpointTable()
            private$.initValidationTable()
            private$.initPerformanceTable()
            private$.initSurvivalTable()
            
            # Set up instructions
            private$.populateInstructionsTable()
            
        },
        
        .run = function() {
            
            # Early exit if no data
            if (is.null(self$data) || nrow(self$data) == 0)
                return()
            
            # Get variables
            biomarker <- self$options$biomarker
            outcome <- self$options$outcome
            time_var <- self$options$time_var
            status_var <- self$options$status_var
            
            # Validate inputs
            if (is.null(biomarker) || length(biomarker) == 0) {
                self$results$instructions$setContent("Please specify a biomarker variable to begin cutpoint analysis.")
                return()
            }
            
            if (is.null(outcome) && (is.null(time_var) || is.null(status_var))) {
                self$results$instructions$setContent("Please specify either an outcome variable or time/status variables for survival analysis.")
                return()
            }
            
            # Build the analysis
            private$.buildAnalysis()
            
        },
        
        .buildAnalysis = function() {
            
            # Prepare data for analysis
            analysisData <- private$.prepareData()
            
            if (is.null(analysisData)) {
                return()
            }
            
            # Determine analysis type
            analysis_type <- self$options$analysis_type
            
            if (analysis_type == "binary") {
                private$.performBinaryCutpointAnalysis(analysisData)
            } else if (analysis_type == "survival") {
                private$.performSurvivalCutpointAnalysis(analysisData)
            } else if (analysis_type == "continuous") {
                private$.performContinuousCutpointAnalysis(analysisData)
            }
            
            # Generate interpretation
            private$.generateInterpretation()
            
        },
        
        .prepareData = function() {
            
            # Get variables
            biomarker <- self$options$biomarker
            outcome <- self$options$outcome
            time_var <- self$options$time_var
            status_var <- self$options$status_var
            
            # Get data
            data <- self$data
            
            # Create variable list based on analysis type
            vars_to_check <- c(biomarker)
            
            if (self$options$analysis_type == "binary" && !is.null(outcome)) {
                vars_to_check <- c(vars_to_check, outcome)
            } else if (self$options$analysis_type == "survival") {
                if (!is.null(time_var) && !is.null(status_var)) {
                    vars_to_check <- c(vars_to_check, time_var, status_var)
                }
            } else if (self$options$analysis_type == "continuous" && !is.null(outcome)) {
                vars_to_check <- c(vars_to_check, outcome)
            }
            
            if (length(vars_to_check) < 2) {
                self$results$instructions$setContent("Insufficient variables specified for analysis.")
                return(NULL)
            }
            
            # Create analysis dataset
            analysisData <- data[, vars_to_check, drop = FALSE]
            analysisData <- na.omit(analysisData)
            
            if (nrow(analysisData) < 20) {
                self$results$instructions$setContent("Insufficient data for cutpoint analysis. Need at least 20 complete observations.")
                return(NULL)
            }
            
            # Ensure outcome is properly formatted for binary analysis
            if (self$options$analysis_type == "binary" && !is.null(outcome)) {
                # Convert to factor and then to numeric (0/1)
                if (!is.numeric(analysisData[[outcome]])) {
                    analysisData[[outcome]] <- as.factor(analysisData[[outcome]])
                    analysisData[[outcome]] <- as.numeric(analysisData[[outcome]]) - 1
                }
                
                # Ensure binary
                unique_vals <- unique(analysisData[[outcome]])
                if (length(unique_vals) != 2) {
                    self$results$instructions$setContent("Outcome variable must be binary (2 unique values) for binary analysis.")
                    return(NULL)
                }
            }
            
            return(analysisData)
        },
        
        .performBinaryCutpointAnalysis = function(data) {
            
            biomarker <- self$options$biomarker
            outcome <- self$options$outcome
            method <- self$options$cutpoint_method
            
            tryCatch({
                
                # Use cutpointr package for comprehensive analysis
                if (method == "youden") {
                    metric <- "youden"
                } else if (method == "closest_topleft") {
                    metric <- "closest_topleft"
                } else if (method == "roc01") {
                    metric <- "roc01"
                } else {
                    metric <- "youden"  # Default
                }
                
                # Perform cutpoint analysis
                cutpoint_result <- cutpointr::cutpointr(
                    data = data,
                    x = !!rlang::sym(biomarker),
                    class = !!rlang::sym(outcome),
                    method = cutpointr::maximize_metric,
                    metric = get(metric, envir = asNamespace("cutpointr")),
                    boot_runs = if(self$options$bootstrap_validation) 1000 else 0
                )
                
                # Store results
                private$.populateBinaryCutpointTable(cutpoint_result, data, biomarker, outcome)
                
                # Performance metrics
                private$.populateBinaryPerformanceTable(cutpoint_result)
                
                # Bootstrap validation if requested
                if (self$options$bootstrap_validation) {
                    private$.populateValidationTable(cutpoint_result)
                }
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Binary cutpoint analysis failed:", e$message))
            })
        },
        
        .performSurvivalCutpointAnalysis = function(data) {
            
            biomarker <- self$options$biomarker
            time_var <- self$options$time_var
            status_var <- self$options$status_var
            method <- self$options$cutpoint_method
            
            tryCatch({
                
                if (method == "maxstat") {
                    # Maximally selected rank statistics
                    formula_str <- paste("Surv(", time_var, ",", status_var, ") ~", biomarker)
                    
                    maxstat_result <- maxstat::maxstat.test(
                        formula = as.formula(formula_str),
                        data = data,
                        smethod = "LogRank",
                        pmethod = "HL"
                    )
                    
                    optimal_cutpoint <- maxstat_result$estimate
                    p_value <- maxstat_result$p.value
                    
                    private$.populateSurvivalCutpointTable(
                        cutpoint = optimal_cutpoint,
                        p_value = p_value,
                        method = "Maximally Selected Rank Statistics",
                        data = data,
                        biomarker = biomarker,
                        time_var = time_var,
                        status_var = status_var
                    )
                    
                } else {
                    # Quantile-based cutpoints
                    if (method == "median") {
                        cutpoint <- median(data[[biomarker]], na.rm = TRUE)
                    } else if (method == "tertiles") {
                        cutpoints <- quantile(data[[biomarker]], probs = c(1/3, 2/3), na.rm = TRUE)
                        # For now, use first tertile as cutpoint
                        cutpoint <- cutpoints[1]
                    } else if (method == "quartiles") {
                        cutpoints <- quantile(data[[biomarker]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
                        # Use median as cutpoint
                        cutpoint <- cutpoints[2]
                    }
                    
                    # Test significance with log-rank test
                    data$biomarker_group <- ifelse(data[[biomarker]] <= cutpoint, "Low", "High")
                    
                    formula_str <- paste("Surv(", time_var, ",", status_var, ") ~ biomarker_group")
                    survdiff_result <- survival::survdiff(as.formula(formula_str), data = data)
                    p_value <- 1 - pchisq(survdiff_result$chisq, df = 1)
                    
                    private$.populateSurvivalCutpointTable(
                        cutpoint = cutpoint,
                        p_value = p_value,
                        method = method,
                        data = data,
                        biomarker = biomarker,
                        time_var = time_var,
                        status_var = status_var
                    )
                }
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Survival cutpoint analysis failed:", e$message))
            })
        },
        
        .performContinuousCutpointAnalysis = function(data) {
            
            biomarker <- self$options$biomarker
            outcome <- self$options$outcome
            method <- self$options$cutpoint_method
            
            tryCatch({
                
                # For continuous outcomes, find cutpoint that maximizes correlation or minimizes MSE
                biomarker_values <- sort(unique(data[[biomarker]]))
                n_cutpoints <- min(length(biomarker_values) - 1, 100)  # Limit search space
                
                if (n_cutpoints < 10) {
                    self$results$instructions$setContent("Too few unique biomarker values for continuous cutpoint analysis.")
                    return()
                }
                
                # Sample cutpoints if too many
                if (length(biomarker_values) > 100) {
                    cutpoint_indices <- seq(1, length(biomarker_values), length.out = 100)
                    test_cutpoints <- biomarker_values[cutpoint_indices]
                } else {
                    test_cutpoints <- biomarker_values[-length(biomarker_values)]  # Exclude maximum
                }
                
                best_cutpoint <- NULL
                best_metric <- -Inf
                cutpoint_results <- list()
                
                for (cutpoint in test_cutpoints) {
                    # Create binary groups
                    group <- ifelse(data[[biomarker]] <= cutpoint, 0, 1)
                    
                    # Calculate metric based on method
                    if (method == "correlation") {
                        metric_value <- abs(cor(group, data[[outcome]], use = "complete.obs"))
                    } else {
                        # Default: minimize MSE or maximize R-squared
                        model <- lm(data[[outcome]] ~ group)
                        metric_value <- summary(model)$r.squared
                    }
                    
                    cutpoint_results[[as.character(cutpoint)]] <- metric_value
                    
                    if (metric_value > best_metric) {
                        best_metric <- metric_value
                        best_cutpoint <- cutpoint
                    }
                }
                
                # Populate results
                private$.populateContinuousCutpointTable(
                    cutpoint = best_cutpoint,
                    metric_value = best_metric,
                    method = method,
                    data = data,
                    biomarker = biomarker,
                    outcome = outcome
                )
                
            }, error = function(e) {
                self$results$instructions$setContent(paste("Continuous cutpoint analysis failed:", e$message))
            })
        },
        
        .populateBinaryCutpointTable = function(cutpoint_result, data, biomarker, outcome) {
            
            table <- self$results$cutpointresults
            
            # Extract results
            optimal_cutpoint <- cutpoint_result$optimal_cutpoint
            auc <- cutpoint_result$AUC
            sensitivity <- cutpoint_result$sensitivity
            specificity <- cutpoint_result$specificity
            
            # Calculate additional metrics
            n_total <- nrow(data)
            n_positive <- sum(data[[outcome]] == 1, na.rm = TRUE)
            n_negative <- sum(data[[outcome]] == 0, na.rm = TRUE)
            
            # Group counts
            n_above <- sum(data[[biomarker]] > optimal_cutpoint, na.rm = TRUE)
            n_below <- sum(data[[biomarker]] <= optimal_cutpoint, na.rm = TRUE)
            
            row <- list(
                method = paste("Binary -", self$options$cutpoint_method),
                cutpoint = round(optimal_cutpoint, 4),
                auc = round(auc, 4),
                sensitivity = round(sensitivity, 4),
                specificity = round(specificity, 4),
                n_total = n_total,
                n_above = n_above,
                n_below = n_below,
                p_value = NA
            )
            
            table$addRow(rowKey = "binary", values = row)
        },
        
        .populateBinaryPerformanceTable = function(cutpoint_result) {
            
            table <- self$results$performance
            
            # Calculate performance metrics
            ppv <- cutpoint_result$pos_pred_value
            npv <- cutpoint_result$neg_pred_value
            accuracy <- cutpoint_result$accuracy
            
            # Likelihood ratios
            lr_pos <- cutpoint_result$sensitivity / (1 - cutpoint_result$specificity)
            lr_neg <- (1 - cutpoint_result$sensitivity) / cutpoint_result$specificity
            
            # Youden index
            youden <- cutpoint_result$sensitivity + cutpoint_result$specificity - 1
            
            row <- list(
                metric = "Positive Predictive Value",
                value = round(ppv, 4),
                interpretation = private$.interpretPPV(ppv)
            )
            table$addRow(rowKey = "ppv", values = row)
            
            row <- list(
                metric = "Negative Predictive Value", 
                value = round(npv, 4),
                interpretation = private$.interpretNPV(npv)
            )
            table$addRow(rowKey = "npv", values = row)
            
            row <- list(
                metric = "Accuracy",
                value = round(accuracy, 4),
                interpretation = private$.interpretAccuracy(accuracy)
            )
            table$addRow(rowKey = "accuracy", values = row)
            
            row <- list(
                metric = "Positive Likelihood Ratio",
                value = round(lr_pos, 4),
                interpretation = private$.interpretLR(lr_pos, "positive")
            )
            table$addRow(rowKey = "lr_pos", values = row)
            
            row <- list(
                metric = "Negative Likelihood Ratio",
                value = round(lr_neg, 4),
                interpretation = private$.interpretLR(lr_neg, "negative")
            )
            table$addRow(rowKey = "lr_neg", values = row)
            
            row <- list(
                metric = "Youden Index",
                value = round(youden, 4),
                interpretation = private$.interpretYouden(youden)
            )
            table$addRow(rowKey = "youden", values = row)
        },
        
        .populateSurvivalCutpointTable = function(cutpoint, p_value, method, data, biomarker, time_var, status_var) {
            
            table <- self$results$cutpointresults
            
            # Calculate group statistics
            n_total <- nrow(data)
            n_above <- sum(data[[biomarker]] > cutpoint, na.rm = TRUE)
            n_below <- sum(data[[biomarker]] <= cutpoint, na.rm = TRUE)
            
            # Calculate survival statistics
            data$group <- ifelse(data[[biomarker]] <= cutpoint, "Low", "High")
            
            # Fit survival curves
            formula_str <- paste("Surv(", time_var, ",", status_var, ") ~ group")
            survfit_result <- survival::survfit(as.formula(formula_str), data = data)
            
            # Calculate median survival times
            median_surv <- summary(survfit_result)$table[, "median"]
            if (length(median_surv) == 2) {
                median_low <- median_surv[1]
                median_high <- median_surv[2]
            } else {
                median_low <- NA
                median_high <- NA
            }
            
            # Calculate hazard ratio
            formula_str_cox <- paste("Surv(", time_var, ",", status_var, ") ~ group")
            cox_result <- survival::coxph(as.formula(formula_str_cox), data = data)
            hr <- exp(coef(cox_result))
            hr_ci <- exp(confint(cox_result))
            
            row <- list(
                method = paste("Survival -", method),
                cutpoint = round(cutpoint, 4),
                auc = NA,
                sensitivity = NA,
                specificity = NA,
                n_total = n_total,
                n_above = n_above,
                n_below = n_below,
                p_value = round(p_value, 4)
            )
            
            table$addRow(rowKey = "survival", values = row)
            
            # Add survival-specific results
            survival_table <- self$results$survival
            
            row <- list(
                group = "Low (≤ cutpoint)",
                n = n_below,
                events = sum(data[[status_var]][data$group == "Low"], na.rm = TRUE),
                median_survival = if(is.na(median_low)) "Not reached" else round(median_low, 2),
                hr = "Reference",
                hr_ci = "Reference"
            )
            survival_table$addRow(rowKey = "low", values = row)
            
            row <- list(
                group = "High (> cutpoint)",
                n = n_above,
                events = sum(data[[status_var]][data$group == "High"], na.rm = TRUE),
                median_survival = if(is.na(median_high)) "Not reached" else round(median_high, 2),
                hr = round(hr, 4),
                hr_ci = paste0(round(hr_ci[1], 2), " - ", round(hr_ci[2], 2))
            )
            survival_table$addRow(rowKey = "high", values = row)
        },
        
        .populateContinuousCutpointTable = function(cutpoint, metric_value, method, data, biomarker, outcome) {
            
            table <- self$results$cutpointresults
            
            # Calculate group statistics
            n_total <- nrow(data)
            n_above <- sum(data[[biomarker]] > cutpoint, na.rm = TRUE)
            n_below <- sum(data[[biomarker]] <= cutpoint, na.rm = TRUE)
            
            # Calculate group means
            mean_below <- mean(data[[outcome]][data[[biomarker]] <= cutpoint], na.rm = TRUE)
            mean_above <- mean(data[[outcome]][data[[biomarker]] > cutpoint], na.rm = TRUE)
            
            row <- list(
                method = paste("Continuous -", method),
                cutpoint = round(cutpoint, 4),
                auc = round(metric_value, 4),  # Use AUC column for metric value
                sensitivity = round(mean_below, 4),  # Use for mean below
                specificity = round(mean_above, 4),  # Use for mean above
                n_total = n_total,
                n_above = n_above,
                n_below = n_below,
                p_value = NA
            )
            
            table$addRow(rowKey = "continuous", values = row)
        },
        
        .populateValidationTable = function(cutpoint_result) {
            
            if (!self$options$bootstrap_validation) return()
            
            table <- self$results$validation
            
            # Extract bootstrap results if available
            if ("boot" %in% names(cutpoint_result)) {
                boot_results <- cutpoint_result$boot
                
                # Calculate bootstrap confidence intervals
                cutpoint_ci <- quantile(boot_results$optimal_cutpoint, c(0.025, 0.975), na.rm = TRUE)
                auc_ci <- quantile(boot_results$AUC, c(0.025, 0.975), na.rm = TRUE)
                sens_ci <- quantile(boot_results$sensitivity, c(0.025, 0.975), na.rm = TRUE)
                spec_ci <- quantile(boot_results$specificity, c(0.025, 0.975), na.rm = TRUE)
                
                # Add rows for each metric
                row <- list(
                    metric = "Optimal Cutpoint",
                    original = round(cutpoint_result$optimal_cutpoint, 4),
                    bootstrap_mean = round(mean(boot_results$optimal_cutpoint, na.rm = TRUE), 4),
                    ci_lower = round(cutpoint_ci[1], 4),
                    ci_upper = round(cutpoint_ci[2], 4),
                    bias = round(mean(boot_results$optimal_cutpoint, na.rm = TRUE) - cutpoint_result$optimal_cutpoint, 4)
                )
                table$addRow(rowKey = "cutpoint", values = row)
                
                row <- list(
                    metric = "AUC",
                    original = round(cutpoint_result$AUC, 4),
                    bootstrap_mean = round(mean(boot_results$AUC, na.rm = TRUE), 4),
                    ci_lower = round(auc_ci[1], 4),
                    ci_upper = round(auc_ci[2], 4),
                    bias = round(mean(boot_results$AUC, na.rm = TRUE) - cutpoint_result$AUC, 4)
                )
                table$addRow(rowKey = "auc", values = row)
                
                row <- list(
                    metric = "Sensitivity",
                    original = round(cutpoint_result$sensitivity, 4),
                    bootstrap_mean = round(mean(boot_results$sensitivity, na.rm = TRUE), 4),
                    ci_lower = round(sens_ci[1], 4),
                    ci_upper = round(sens_ci[2], 4),
                    bias = round(mean(boot_results$sensitivity, na.rm = TRUE) - cutpoint_result$sensitivity, 4)
                )
                table$addRow(rowKey = "sensitivity", values = row)
                
                row <- list(
                    metric = "Specificity",
                    original = round(cutpoint_result$specificity, 4),
                    bootstrap_mean = round(mean(boot_results$specificity, na.rm = TRUE), 4),
                    ci_lower = round(spec_ci[1], 4),
                    ci_upper = round(spec_ci[2], 4),
                    bias = round(mean(boot_results$specificity, na.rm = TRUE) - cutpoint_result$specificity, 4)
                )
                table$addRow(rowKey = "specificity", values = row)
            }
        },
        
        .interpretPPV = function(ppv) {
            if (is.na(ppv)) return("Cannot calculate")
            if (ppv < 0.5) return("Poor positive prediction")
            if (ppv < 0.7) return("Moderate positive prediction")
            if (ppv < 0.9) return("Good positive prediction")
            return("Excellent positive prediction")
        },
        
        .interpretNPV = function(npv) {
            if (is.na(npv)) return("Cannot calculate")
            if (npv < 0.5) return("Poor negative prediction")
            if (npv < 0.7) return("Moderate negative prediction")
            if (npv < 0.9) return("Good negative prediction")
            return("Excellent negative prediction")
        },
        
        .interpretAccuracy = function(accuracy) {
            if (is.na(accuracy)) return("Cannot calculate")
            if (accuracy < 0.6) return("Poor accuracy")
            if (accuracy < 0.7) return("Fair accuracy")
            if (accuracy < 0.8) return("Good accuracy")
            if (accuracy < 0.9) return("Very good accuracy")
            return("Excellent accuracy")
        },
        
        .interpretLR = function(lr, type) {
            if (is.na(lr)) return("Cannot calculate")
            
            if (type == "positive") {
                if (lr < 2) return("Poor rule-in value")
                if (lr < 5) return("Fair rule-in value")
                if (lr < 10) return("Good rule-in value")
                return("Excellent rule-in value")
            } else {
                if (lr > 0.5) return("Poor rule-out value")
                if (lr > 0.2) return("Fair rule-out value")
                if (lr > 0.1) return("Good rule-out value")
                return("Excellent rule-out value")
            }
        },
        
        .interpretYouden = function(youden) {
            if (is.na(youden)) return("Cannot calculate")
            if (youden < 0.2) return("Poor discrimination")
            if (youden < 0.5) return("Fair discrimination")
            if (youden < 0.7) return("Good discrimination")
            return("Excellent discrimination")
        },
        
        .generateInterpretation = function() {
            
            # Generate clinical interpretation
            biomarker <- self$options$biomarker
            analysis_type <- self$options$analysis_type
            method <- self$options$cutpoint_method
            
            html <- ""
            
            html <- paste0(html, "<h3>Optimal Cutpoint Analysis Interpretation</h3>")
            
            html <- paste0(html, "<h4>Analysis Overview</h4>")
            html <- paste0(html, "<p>This analysis determines the optimal cutpoint for <strong>", biomarker, "</strong> using ")
            
            if (analysis_type == "binary") {
                html <- paste0(html, "binary outcome optimization with the <strong>", method, "</strong> method.")
            } else if (analysis_type == "survival") {
                html <- paste0(html, "survival analysis optimization with the <strong>", method, "</strong> method.")
            } else {
                html <- paste0(html, "continuous outcome optimization with the <strong>", method, "</strong> method.")
            }
            
            html <- paste0(html, "</p>")
            
            html <- paste0(html, "<h4>Clinical Significance</h4>")
            html <- paste0(html, "<p>Optimal cutpoint determination is crucial for:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>Clinical Decision Making:</strong> Converting continuous biomarkers to actionable thresholds</li>")
            html <- paste0(html, "<li><strong>Diagnostic Accuracy:</strong> Maximizing sensitivity and specificity for clinical use</li>")
            html <- paste0(html, "<li><strong>Risk Stratification:</strong> Identifying high-risk patients for targeted interventions</li>")
            html <- paste0(html, "<li><strong>Standardization:</strong> Establishing consistent thresholds across institutions</li>")
            html <- paste0(html, "</ul>")
            
            html <- paste0(html, "<h4>Method-Specific Information</h4>")
            
            if (method == "youden") {
                html <- paste0(html, "<p><strong>Youden Index:</strong> Maximizes (Sensitivity + Specificity - 1). Best for balanced importance of sensitivity and specificity.</p>")
            } else if (method == "closest_topleft") {
                html <- paste0(html, "<p><strong>Closest to Top-Left:</strong> Minimizes distance to perfect classification point (0,1). Good for general diagnostic purposes.</p>")
            } else if (method == "maxstat") {
                html <- paste0(html, "<p><strong>Maximally Selected Rank Statistics:</strong> Finds cutpoint with maximum log-rank test statistic. Optimal for survival discrimination.</p>")
            } else if (method == "median") {
                html <- paste0(html, "<p><strong>Median Split:</strong> Uses median value as cutpoint. Simple but may not be optimal for discrimination.</p>")
            }
            
            html <- paste0(html, "<h4>Validation Considerations</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>External Validation:</strong> Test cutpoint performance in independent datasets</li>")
            html <- paste0(html, "<li><strong>Clinical Context:</strong> Consider clinical feasibility and interpretability</li>")
            html <- paste0(html, "<li><strong>Sample Size:</strong> Ensure adequate power for reliable cutpoint estimation</li>")
            html <- paste0(html, "<li><strong>Multiple Testing:</strong> Account for searching across multiple cutpoints</li>")
            html <- paste0(html, "</ul>")
            
            html <- paste0(html, "<h4>Digital Pathology Applications</h4>")
            html <- paste0(html, "<p>Optimal cutpoints are essential for:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>H-score Thresholds:</strong> Converting continuous IHC scores to clinically meaningful categories</li>")
            html <- paste0(html, "<li><strong>Ki-67 Cutpoints:</strong> Determining proliferation index thresholds for prognosis</li>")
            html <- paste0(html, "<li><strong>Tumor Cell Density:</strong> Establishing cell count thresholds for diagnosis</li>")
            html <- paste0(html, "<li><strong>Nuclear Features:</strong> Optimizing morphometric thresholds for grading</li>")
            html <- paste0(html, "</ul>")
            
            self$results$interpretation$setContent(html)
        },
        
        .initCutpointTable = function() {
            table <- self$results$cutpointresults
            
            table$addColumn(name = 'method', title = 'Method', type = 'text')
            table$addColumn(name = 'cutpoint', title = 'Cutpoint', type = 'number')
            table$addColumn(name = 'auc', title = 'AUC/Metric', type = 'number')
            table$addColumn(name = 'sensitivity', title = 'Sensitivity', type = 'number')
            table$addColumn(name = 'specificity', title = 'Specificity', type = 'number')
            table$addColumn(name = 'n_total', title = 'N Total', type = 'integer')
            table$addColumn(name = 'n_above', title = 'N Above', type = 'integer')
            table$addColumn(name = 'n_below', title = 'N Below', type = 'integer')
            table$addColumn(name = 'p_value', title = 'p-value', type = 'number', format = 'zto,pvalue')
        },
        
        .initValidationTable = function() {
            table <- self$results$validation
            
            table$addColumn(name = 'metric', title = 'Metric', type = 'text')
            table$addColumn(name = 'original', title = 'Original', type = 'number')
            table$addColumn(name = 'bootstrap_mean', title = 'Bootstrap Mean', type = 'number')
            table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number')
            table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number')
            table$addColumn(name = 'bias', title = 'Bias', type = 'number')
        },
        
        .initPerformanceTable = function() {
            table <- self$results$performance
            
            table$addColumn(name = 'metric', title = 'Metric', type = 'text')
            table$addColumn(name = 'value', title = 'Value', type = 'number')
            table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
        },
        
        .initSurvivalTable = function() {
            table <- self$results$survival
            
            table$addColumn(name = 'group', title = 'Group', type = 'text')
            table$addColumn(name = 'n', title = 'N', type = 'integer')
            table$addColumn(name = 'events', title = 'Events', type = 'integer')
            table$addColumn(name = 'median_survival', title = 'Median Survival', type = 'text')
            table$addColumn(name = 'hr', title = 'Hazard Ratio', type = 'text')
            table$addColumn(name = 'hr_ci', title = '95% CI', type = 'text')
        },
        
        .populateInstructionsTable = function() {
            html <- paste0(
                "<h2>Optimal Cutpoint Determination</h2>",
                "<p>This module finds optimal cutpoints for continuous biomarkers to maximize diagnostic or prognostic performance.</p>",
                "<h3>Getting Started</h3>",
                "<ol>",
                "<li><strong>Biomarker Variable:</strong> Select your continuous biomarker (e.g., H-score, Ki-67, cell count)</li>",
                "<li><strong>Analysis Type:</strong> Choose based on your outcome:</li>",
                "<ul>",
                "<li><strong>Binary:</strong> For diagnostic accuracy (disease vs. no disease)</li>",
                "<li><strong>Survival:</strong> For prognostic cutpoints (time-to-event outcomes)</li>",
                "<li><strong>Continuous:</strong> For continuous outcomes (e.g., expression levels)</li>",
                "</ul>",
                "<li><strong>Outcome Variable:</strong> Specify the outcome for optimization</li>",
                "<li><strong>Method:</strong> Select the optimization criterion</li>",
                "</ol>",
                "<h3>Cutpoint Methods</h3>",
                "<ul>",
                "<li><strong>Youden Index:</strong> Maximizes sensitivity + specificity - 1</li>",
                "<li><strong>Closest to Top-Left:</strong> Minimizes distance to perfect classifier</li>",
                "<li><strong>ROC01:</strong> Minimizes misclassification cost</li>",
                "<li><strong>Maxstat:</strong> Maximally selected rank statistics for survival</li>",
                "<li><strong>Median/Quantiles:</strong> Simple splits for comparison</li>",
                "</ul>",
                "<h3>Validation Options</h3>",
                "<p><strong>Bootstrap Validation:</strong> Provides confidence intervals and bias correction for cutpoint stability</p>",
                "<h3>Clinical Applications</h3>",
                "<ul>",
                "<li><strong>IHC Scoring:</strong> Convert H-scores to positive/negative calls</li>",
                "<li><strong>Proliferation Index:</strong> Optimize Ki-67 thresholds for prognosis</li>",
                "<li><strong>Cell Density:</strong> Establish diagnostic thresholds for cell counts</li>",
                "<li><strong>Biomarker Development:</strong> Find optimal thresholds for clinical trials</li>",
                "</ul>"
            )
            
            self$results$instructions$setContent(html)
        }
    )
)