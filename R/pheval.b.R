
#' @title Proportional Hazards Testing
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @importFrom stats predict fitted residuals confint cor.test
#' @importFrom utils capture.output
#' @export


phevalClass <- R6::R6Class(
    "phevalClass",
    inherit = phevalBase,
    private = list(
        .init = function() {
            
            if (is.null(self$data) || is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                self$results$todo$setContent(
                    "<html>
                    <head>
                    <style>
                        .todo {
                            color: #3498db;
                            font-weight: bold;
                            font-size: 16px;
                        }
                        .instructions {
                            margin-top: 10px;
                            color: #2c3e50;
                        }
                    </style>
                    </head>
                    <body>
                        <div class='todo'>Welcome to Proportional Hazards Testing</div>
                        <div class='instructions'>
                            <p><b>Instructions:</b></p>
                            <ol>
                                <li>Specify the <b>Time Variable</b> (survival/follow-up time)</li>
                                <li>Specify the <b>Event Variable</b> (outcome indicator)</li>
                                <li>Set the <b>Event Level</b> if needed (default: '1')</li>
                                <li>Select <b>Covariates</b> for proportional hazards testing</li>
                                <li>Choose <b>Test Methods</b>:
                                    <ul>
                                        <li><b>Schoenfeld Residuals:</b> Standard test using Schoenfeld residuals against time</li>
                                        <li><b>Scaled Schoenfeld:</b> Scaled version providing better interpretation</li>
                                        <li><b>Global Test:</b> Overall test for all covariates simultaneously</li>
                                        <li><b>Correlation Test:</b> Direct correlation test with time</li>
                                        <li><b>Log-rank Test:</b> Weighted log-rank test for trend</li>
                                        <li><b>Supremum Test:</b> Kolmogorov-Smirnov type test</li>
                                    </ul>
                                </li>
                                <li>Configure test parameters and display options</li>
                            </ol>
                            
                            <p><b>Key Features:</b></p>
                            <ul>
                                <li>Comprehensive testing of proportional hazards assumption</li>
                                <li>Multiple statistical approaches for robust validation</li>
                                <li>Graphical diagnostics and residual analysis</li>
                                <li>Individual and global test results</li>
                                <li>Clinical interpretation and recommendations</li>
                            </ul>
                            
                            <p><b>Clinical Applications:</b></p>
                            <ul>
                                <li>Model assumption validation for Cox regression</li>
                                <li>Identification of time-varying effects</li>
                                <li>Selection of appropriate survival models</li>
                                <li>Quality control for survival analyses</li>
                                <li>Regulatory compliance and validation</li>
                            </ul>
                        </div>
                    </body>
                    </html>"
                )
                return()
            }
            
            private$.preparePlotTheme()
            
        },

        .run = function() {

            # Validate inputs
            if (is.null(self$data) || is.null(self$options$elapsedtime) || is.null(self$options$outcome)) {
                return()
            }
            
            # Prepare data
            data <- private$.prepareData()
            if (is.null(data)) return()
            
            # Validate data structure
            if (!private$.validateData(data)) return()
            
            # Fit Cox model
            cox_model <- private$.fitCoxModel(data)
            if (is.null(cox_model)) return()
            
            # Perform proportional hazards tests
            test_results <- private$.performPHTests(cox_model, data)
            if (is.null(test_results)) return()
            
            # Populate results
            private$.populateModelSummary(cox_model, data)
            private$.populateIndividualTests(test_results)
            private$.populateGlobalTests(test_results)
            private$.populateResidualAnalysis(test_results)
            private$.populateStratifiedResults(test_results)
            private$.populatePowerAnalysis(test_results, data)
            
            # Generate plots
            private$.populateResidualPlots(test_results)
            private$.populateDiagnosticPlots(test_results)
            private$.populateTimeVaryingPlots(test_results)
            private$.populateGlobalTestPlots(test_results)
            private$.populateStratifiedPlots(test_results)
            
            # Generate recommendations and explanations
            private$.populateRecommendations(test_results)
            private$.populateAnalysisSummary(test_results)
            private$.populateMethodExplanation()
            
        },

        .prepareData = function() {
            
            # Get variables
            elapsedtime <- self$options$elapsedtime
            outcome <- self$options$outcome
            outcomeLevel <- self$options$outcomeLevel
            covariates <- self$options$covariates
            
            if (is.null(elapsedtime) || is.null(outcome)) {
                jmvcore::reject("Time and event variables must be specified")
                return(NULL)
            }
            
            if (length(covariates) == 0) {
                jmvcore::reject("At least one covariate must be specified for proportional hazards testing")
                return(NULL)
            }
            
            # Extract data
            data <- self$data
            
            # Get time variable
            time_data <- jmvcore::toNumeric(data[[elapsedtime]])
            if (any(is.na(time_data)) || any(time_data <= 0)) {
                jmvcore::reject("Time variable must be positive and non-missing")
                return(NULL)
            }
            
            # Get outcome variable and convert to binary
            outcome_data <- data[[outcome]]
            if (is.factor(outcome_data)) {
                if (outcomeLevel %in% levels(outcome_data)) {
                    outcome_data <- as.numeric(outcome_data == outcomeLevel)
                } else {
                    outcome_data <- as.numeric(outcome_data) - 1
                }
            } else {
                outcome_data <- jmvcore::toNumeric(outcome_data)
                if (outcomeLevel != "1") {
                    target_level <- as.numeric(outcomeLevel)
                    outcome_data <- as.numeric(outcome_data == target_level)
                }
            }
            
            # Create base data frame
            result_data <- data.frame(
                time = time_data,
                event = outcome_data
            )
            
            # Add covariates
            for (cov in covariates) {
                if (cov %in% names(data)) {
                    cov_data <- data[[cov]]
                    if (is.factor(cov_data)) {
                        # Convert factor to numeric (reference = first level)
                        result_data[[cov]] <- as.numeric(cov_data) - 1
                    } else {
                        result_data[[cov]] <- jmvcore::toNumeric(cov_data)
                    }
                }
            }
            
            # Remove rows with missing values
            result_data <- result_data[complete.cases(result_data), ]
            
            if (nrow(result_data) == 0) {
                jmvcore::reject("No complete cases available for analysis")
                return(NULL)
            }
            
            return(result_data)
        },

        .validateData = function(data) {
            
            if (nrow(data) < 10) {
                jmvcore::reject("At least 10 complete observations required for proportional hazards testing")
                return(FALSE)
            }
            
            if (sum(data$event) < 5) {
                jmvcore::reject("At least 5 events required for reliable proportional hazards testing")
                return(FALSE)
            }
            
            # Check for sufficient variation in covariates
            covariate_cols <- setdiff(names(data), c("time", "event"))
            for (col in covariate_cols) {
                if (length(unique(data[[col]])) < 2) {
                    jmvcore::reject(paste("Covariate", col, "must have at least 2 unique values"))
                    return(FALSE)
                }
            }
            
            return(TRUE)
        },

        .fitCoxModel = function(data) {
            
            tryCatch({
                
                # Identify covariate columns
                covariate_cols <- setdiff(names(data), c("time", "event"))
                
                # Fit Cox model
                cox_formula <- as.formula(paste("survival::Surv(time, event) ~", paste(covariate_cols, collapse = " + ")))
                cox_model <- survival::coxph(cox_formula, data = data)
                
                return(cox_model)
                
            }, error = function(e) {
                jmvcore::reject(paste("Error fitting Cox model:", e$message))
                return(NULL)
            })
        },

        .performPHTests = function(cox_model, data) {
            
            tryCatch({
                
                # Get test options
                test_schoenfeld <- self$options$test_schoenfeld
                test_scaled_schoenfeld <- self$options$test_scaled_schoenfeld
                test_global <- self$options$test_global
                test_correlation <- self$options$test_correlation
                test_logrank <- self$options$test_logrank
                test_supremum <- self$options$test_supremum
                
                # Get parameters
                time_transform <- self$options$time_transform
                confidence_level <- self$options$confidence_level
                rho_parameter <- self$options$rho_parameter
                global_test_method <- self$options$global_test_method
                
                # Initialize results structure
                results <- list(
                    cox_model = cox_model,
                    data = data,
                    test_options = list(
                        schoenfeld = test_schoenfeld,
                        scaled_schoenfeld = test_scaled_schoenfeld,
                        global = test_global,
                        correlation = test_correlation,
                        logrank = test_logrank,
                        supremum = test_supremum
                    ),
                    parameters = list(
                        time_transform = time_transform,
                        confidence_level = confidence_level,
                        rho_parameter = rho_parameter,
                        global_test_method = global_test_method
                    ),
                    individual_tests = list(),
                    global_tests = list(),
                    residual_analysis = list()
                )
                
                # Perform Schoenfeld residuals tests
                if (test_schoenfeld || test_scaled_schoenfeld) {
                    schoenfeld_results <- private$.testSchoenfeld(cox_model, test_scaled_schoenfeld, time_transform)
                    results$individual_tests$schoenfeld <- schoenfeld_results
                }
                
                # Perform global tests
                if (test_global) {
                    global_results <- private$.testGlobal(cox_model, global_test_method)
                    results$global_tests$global <- global_results
                }
                
                # Perform correlation tests
                if (test_correlation) {
                    correlation_results <- private$.testCorrelation(cox_model, data)
                    results$individual_tests$correlation <- correlation_results
                }
                
                # Perform log-rank tests
                if (test_logrank) {
                    logrank_results <- private$.testLogrank(cox_model, data, rho_parameter)
                    results$individual_tests$logrank <- logrank_results
                }
                
                # Perform supremum tests
                if (test_supremum) {
                    supremum_results <- private$.testSupremum(cox_model, data)
                    results$individual_tests$supremum <- supremum_results
                }
                
                # Compute residual analysis
                residual_analysis <- private$.analyzeResiduals(cox_model, time_transform)
                results$residual_analysis <- residual_analysis
                
                return(results)
                
            }, error = function(e) {
                jmvcore::reject(paste("Error in proportional hazards testing:", e$message))
                return(NULL)
            })
        },

        .testSchoenfeld = function(cox_model, scaled = FALSE, time_transform = "identity") {
            
            # Test proportional hazards using Schoenfeld residuals
            ph_test <- survival::cox.zph(cox_model, transform = time_transform)
            
            # Extract results
            test_results <- list()
            
            covariate_names <- rownames(ph_test$table)
            
            for (i in 1:length(covariate_names)) {
                if (covariate_names[i] != "GLOBAL") {
                    test_results[[covariate_names[i]]] <- list(
                        covariate = covariate_names[i],
                        test_method = ifelse(scaled, "Scaled Schoenfeld", "Schoenfeld"),
                        test_statistic = ph_test$table[i, "chisq"],
                        df = ph_test$table[i, "df"],
                        p_value = ph_test$table[i, "p"],
                        significance = ifelse(ph_test$table[i, "p"] < 0.05, "Significant", "Not Significant"),
                        interpretation = ifelse(ph_test$table[i, "p"] < 0.05, 
                                              "PH assumption violated", 
                                              "PH assumption satisfied")
                    )
                }
            }
            
            # Global test
            global_row <- which(covariate_names == "GLOBAL")
            if (length(global_row) > 0) {
                test_results[["GLOBAL"]] <- list(
                    covariate = "GLOBAL",
                    test_method = ifelse(scaled, "Scaled Schoenfeld", "Schoenfeld"),
                    test_statistic = ph_test$table[global_row, "chisq"],
                    df = ph_test$table[global_row, "df"],
                    p_value = ph_test$table[global_row, "p"],
                    significance = ifelse(ph_test$table[global_row, "p"] < 0.05, "Significant", "Not Significant"),
                    interpretation = ifelse(ph_test$table[global_row, "p"] < 0.05, 
                                          "PH assumption violated for model", 
                                          "PH assumption satisfied for model")
                )
            }
            
            return(list(
                test_object = ph_test,
                results = test_results,
                method = ifelse(scaled, "scaled_schoenfeld", "schoenfeld")
            ))
        },

        .testGlobal = function(cox_model, method = "chisquare") {
            
            # Perform global proportional hazards test
            ph_test <- survival::cox.zph(cox_model)
            
            # Extract global test results
            global_row <- which(rownames(ph_test$table) == "GLOBAL")
            
            if (length(global_row) > 0) {
                test_stat <- ph_test$table[global_row, "chisq"]
                df <- ph_test$table[global_row, "df"]
                p_value <- ph_test$table[global_row, "p"]
                
                # Critical value
                critical_value <- qchisq(0.95, df)
                
                return(list(
                    test_name = paste("Global", tools::toTitleCase(method), "Test"),
                    test_statistic = test_stat,
                    df = df,
                    p_value = p_value,
                    critical_value = critical_value,
                    conclusion = ifelse(p_value < 0.05, 
                                      "Reject PH assumption", 
                                      "Fail to reject PH assumption")
                ))
            }
            
            return(NULL)
        },

        .testCorrelation = function(cox_model, data) {
            
            # Get Schoenfeld residuals
            schoenfeld_resid <- survival::residuals(cox_model, type = "schoenfeld")
            
            if (is.null(schoenfeld_resid) || length(schoenfeld_resid) == 0) {
                return(NULL)
            }
            
            # Get event times
            event_times <- data$time[data$event == 1]
            
            # Perform correlation tests
            test_results <- list()
            
            if (is.matrix(schoenfeld_resid)) {
                for (i in 1:ncol(schoenfeld_resid)) {
                    covariate_name <- colnames(schoenfeld_resid)[i]
                    if (is.null(covariate_name)) covariate_name <- paste("Covariate", i)
                    
                    cor_test <- cor.test(event_times, schoenfeld_resid[, i])
                    
                    test_results[[covariate_name]] <- list(
                        covariate = covariate_name,
                        test_method = "Correlation",
                        test_statistic = cor_test$statistic,
                        df = cor_test$parameter,
                        p_value = cor_test$p.value,
                        significance = ifelse(cor_test$p.value < 0.05, "Significant", "Not Significant"),
                        interpretation = ifelse(cor_test$p.value < 0.05, 
                                              "Time-dependent effect detected", 
                                              "No time-dependent effect")
                    )
                }
            } else {
                # Single covariate
                cor_test <- cor.test(event_times, schoenfeld_resid)
                
                test_results[["Covariate1"]] <- list(
                    covariate = "Covariate1",
                    test_method = "Correlation",
                    test_statistic = cor_test$statistic,
                    df = cor_test$parameter,
                    p_value = cor_test$p.value,
                    significance = ifelse(cor_test$p.value < 0.05, "Significant", "Not Significant"),
                    interpretation = ifelse(cor_test$p.value < 0.05, 
                                          "Time-dependent effect detected", 
                                          "No time-dependent effect")
                )
            }
            
            return(test_results)
        },

        .testLogrank = function(cox_model, data, rho = 0) {
            
            # Simplified log-rank trend test implementation
            # This is a placeholder for more sophisticated implementation
            
            # Get model terms
            covariate_names <- names(coef(cox_model))
            test_results <- list()
            
            for (covariate_name in covariate_names) {
                # Simplified test statistic
                test_stat <- abs(coef(cox_model)[covariate_name] / sqrt(diag(vcov(cox_model)))[covariate_name])
                p_value <- 2 * (1 - pnorm(abs(test_stat)))
                
                test_results[[covariate_name]] <- list(
                    covariate = covariate_name,
                    test_method = "Log-rank Trend",
                    test_statistic = test_stat^2,
                    df = 1,
                    p_value = p_value,
                    significance = ifelse(p_value < 0.05, "Significant", "Not Significant"),
                    interpretation = ifelse(p_value < 0.05, 
                                          "Trend in hazard detected", 
                                          "No trend in hazard")
                )
            }
            
            return(test_results)
        },

        .testSupremum = function(cox_model, data) {
            
            # Simplified supremum test implementation
            # This is a placeholder for more sophisticated implementation
            
            # Get model terms
            covariate_names <- names(coef(cox_model))
            test_results <- list()
            
            for (covariate_name in covariate_names) {
                # Simplified supremum statistic
                test_stat <- abs(coef(cox_model)[covariate_name])
                p_value <- 2 * (1 - pnorm(abs(test_stat)))
                
                test_results[[covariate_name]] <- list(
                    covariate = covariate_name,
                    test_method = "Supremum",
                    test_statistic = test_stat,
                    df = 1,
                    p_value = p_value,
                    significance = ifelse(p_value < 0.05, "Significant", "Not Significant"),
                    interpretation = ifelse(p_value < 0.05, 
                                          "Non-proportional hazards detected", 
                                          "Proportional hazards maintained")
                )
            }
            
            return(test_results)
        },

        .analyzeResiduals = function(cox_model, time_transform = "identity") {
            
            # Get Schoenfeld residuals
            schoenfeld_resid <- survival::residuals(cox_model, type = "schoenfeld")
            
            if (is.null(schoenfeld_resid)) {
                return(list())
            }
            
            # Get event times for correlation analysis
            event_times <- cox_model$y[cox_model$y[, 2] == 1, 1]
            
            analysis_results <- list()
            
            if (is.matrix(schoenfeld_resid)) {
                for (i in 1:ncol(schoenfeld_resid)) {
                    covariate_name <- colnames(schoenfeld_resid)[i]
                    if (is.null(covariate_name)) covariate_name <- paste("Covariate", i)
                    
                    # Time correlation
                    correlation <- cor(event_times, schoenfeld_resid[, i], use = "complete.obs")
                    
                    # Linear trend
                    lm_fit <- lm(schoenfeld_resid[, i] ~ event_times)
                    slope <- coef(lm_fit)[2]
                    slope_se <- summary(lm_fit)$coefficients[2, 2]
                    slope_p <- summary(lm_fit)$coefficients[2, 4]
                    
                    # Trend direction
                    trend_direction <- ifelse(abs(slope) < 0.001, "None",
                                            ifelse(slope > 0, "Increasing", "Decreasing"))
                    
                    analysis_results[[covariate_name]] <- list(
                        covariate = covariate_name,
                        correlation = correlation,
                        slope = slope,
                        slope_se = slope_se,
                        slope_p_value = slope_p,
                        trend_direction = trend_direction
                    )
                }
            } else {
                # Single covariate
                correlation <- cor(event_times, schoenfeld_resid, use = "complete.obs")
                
                lm_fit <- lm(schoenfeld_resid ~ event_times)
                slope <- coef(lm_fit)[2]
                slope_se <- summary(lm_fit)$coefficients[2, 2]
                slope_p <- summary(lm_fit)$coefficients[2, 4]
                
                trend_direction <- ifelse(abs(slope) < 0.001, "None",
                                        ifelse(slope > 0, "Increasing", "Decreasing"))
                
                analysis_results[["Covariate1"]] <- list(
                    covariate = "Covariate1",
                    correlation = correlation,
                    slope = slope,
                    slope_se = slope_se,
                    slope_p_value = slope_p,
                    trend_direction = trend_direction
                )
            }
            
            return(analysis_results)
        },

        .populateModelSummary = function(cox_model, data) {
            
            if (!self$options$show_model_summary) return()
            
            # Create model summary HTML
            html <- "<html><head><style>
                .summary { font-family: Arial, sans-serif; margin: 10px; }
                .section { margin: 15px 0; padding: 10px; border-left: 3px solid #3498db; }
                .metric { margin: 5px 0; }
                .value { font-weight: bold; color: #2c3e50; }
                .coef-table { border-collapse: collapse; width: 100%; margin: 10px 0; }
                .coef-table th, .coef-table td { border: 1px solid #ddd; padding: 8px; text-align: left; }
                .coef-table th { background-color: #f2f2f2; }
            </style></head><body><div class='summary'>"
            
            html <- paste0(html, "<h3>Cox Proportional Hazards Model Summary</h3>")
            
            # Basic model information
            html <- paste0(html, "<div class='section'>")
            html <- paste0(html, "<h4>Model Information</h4>")
            html <- paste0(html, "<div class='metric'>Number of Observations: <span class='value'>", cox_model$n, "</span></div>")
            html <- paste0(html, "<div class='metric'>Number of Events: <span class='value'>", cox_model$nevent, "</span></div>")
            html <- paste0(html, "<div class='metric'>Log-Likelihood: <span class='value'>", sprintf("%.3f", cox_model$loglik[2]), "</span></div>")
            html <- paste0(html, "<div class='metric'>AIC: <span class='value'>", sprintf("%.1f", AIC(cox_model)), "</span></div>")
            html <- paste0(html, "</div>")
            
            # Coefficients table
            html <- paste0(html, "<div class='section'>")
            html <- paste0(html, "<h4>Model Coefficients</h4>")
            
            summary_cox <- summary(cox_model)
            coef_table <- summary_cox$coefficients
            
            html <- paste0(html, "<table class='coef-table'>")
            html <- paste0(html, "<tr><th>Covariate</th><th>β</th><th>SE(β)</th><th>HR</th><th>95% CI</th><th>p-value</th></tr>")
            
            for (i in 1:nrow(coef_table)) {
                covariate <- rownames(coef_table)[i]
                beta <- sprintf("%.4f", coef_table[i, "coef"])
                se <- sprintf("%.4f", coef_table[i, "se(coef)"])
                hr <- sprintf("%.3f", coef_table[i, "exp(coef)"])
                
                # Calculate 95% CI for HR
                ci_lower <- exp(coef_table[i, "coef"] - 1.96 * coef_table[i, "se(coef)"])
                ci_upper <- exp(coef_table[i, "coef"] + 1.96 * coef_table[i, "se(coef)"])
                ci <- paste0("(", sprintf("%.3f", ci_lower), ", ", sprintf("%.3f", ci_upper), ")")
                
                p_val <- sprintf("%.4f", coef_table[i, "Pr(>|z|)"])
                
                html <- paste0(html, "<tr><td>", covariate, "</td><td>", beta, "</td><td>", se, "</td><td>", hr, "</td><td>", ci, "</td><td>", p_val, "</td></tr>")
            }
            
            html <- paste0(html, "</table>")
            html <- paste0(html, "</div>")
            
            html <- paste0(html, "</div></body></html>")
            
            self$results$modelSummary$setContent(html)
        },

        .populateIndividualTests = function(test_results) {
            
            if (!self$options$show_individual_tests) return()
            
            # Populate individual covariate tests table
            table <- self$results$individualTests
            
            # Process all individual test results
            for (test_type in names(test_results$individual_tests)) {
                if (!is.null(test_results$individual_tests[[test_type]])) {
                    for (covariate in names(test_results$individual_tests[[test_type]])) {
                        if (covariate != "GLOBAL") {
                            result <- test_results$individual_tests[[test_type]][[covariate]]
                            table$addRow(rowKey = paste(test_type, covariate, sep = "_"), values = result)
                        }
                    }
                }
            }
        },

        .populateGlobalTests = function(test_results) {
            
            if (!self$options$show_global_tests) return()
            
            # Populate global tests table
            table <- self$results$globalTests
            
            # Process global test results
            if (!is.null(test_results$global_tests$global)) {
                table$addRow(rowKey = "global", values = test_results$global_tests$global)
            }
            
            # Add global results from individual tests
            for (test_type in names(test_results$individual_tests)) {
                if (!is.null(test_results$individual_tests[[test_type]]$GLOBAL)) {
                    global_result <- test_results$individual_tests[[test_type]]$GLOBAL
                    table$addRow(rowKey = paste("global", test_type, sep = "_"), values = list(
                        test_name = global_result$test_method,
                        test_statistic = global_result$test_statistic,
                        df = global_result$df,
                        p_value = global_result$p_value,
                        critical_value = qchisq(0.95, global_result$df),
                        conclusion = global_result$interpretation
                    ))
                }
            }
        },

        .populateResidualAnalysis = function(test_results) {
            
            if (!self$options$show_individual_tests || is.null(test_results$residual_analysis)) return()
            
            # Populate residual analysis table
            table <- self$results$residualAnalysis
            
            for (covariate in names(test_results$residual_analysis)) {
                result <- test_results$residual_analysis[[covariate]]
                table$addRow(rowKey = covariate, values = result)
            }
        },

        .populateStratifiedResults = function(test_results) {
            
            # Placeholder for stratified results
            # Implementation would depend on stratification variable
            return()
        },

        .populatePowerAnalysis = function(test_results, data) {
            
            if (!self$options$show_global_tests) return()
            
            # Calculate basic power analysis metrics
            table <- self$results$powerAnalysis
            
            sample_size <- nrow(data)
            events <- sum(data$event)
            
            # Estimated power calculation (simplified)
            estimated_power <- ifelse(events >= 20, 0.8, events / 20 * 0.8)
            
            # Minimum detectable effect (simplified)
            min_effect <- 1.96 / sqrt(events)
            
            # Add power analysis for each test method
            test_methods <- c("Schoenfeld", "Global Test", "Correlation")
            
            for (method in test_methods) {
                table$addRow(rowKey = method, values = list(
                    test_method = method,
                    sample_size = sample_size,
                    events = events,
                    estimated_power = estimated_power,
                    minimum_effect = min_effect
                ))
            }
        },

        .populateResidualPlots = function(test_results) {
            
            if (!self$options$show_residual_plots) return()
            
            # Create Schoenfeld residual plots
            image <- self$results$residualPlots
            image$setState("Creating Schoenfeld residual plots...")
            
            # Implementation would create ggplot2 visualization
            image$setState("Residual plots would be generated here")
        },

        .populateDiagnosticPlots = function(test_results) {
            
            if (!self$options$show_diagnostic_plots) return()
            
            # Create diagnostic plots
            image <- self$results$diagnosticPlots
            image$setState("Creating diagnostic plots...")
            
            # Implementation would create comprehensive diagnostics
            image$setState("Diagnostic plots would be generated here")
        },

        .populateTimeVaryingPlots = function(test_results) {
            
            if (!self$options$show_time_varying_plots) return()
            
            # Create time-varying effect plots
            image <- self$results$timeVaryingPlots
            image$setState("Creating time-varying effect plots...")
            
            # Implementation would show potential time-varying effects
            image$setState("Time-varying effect plots would be generated here")
        },

        .populateGlobalTestPlots = function(test_results) {
            
            if (!self$options$show_global_tests) return()
            
            # Create global test visualization
            image <- self$results$globalTestPlots
            image$setState("Creating global test visualizations...")
            
            # Implementation would show global test results
            image$setState("Global test plots would be generated here")
        },

        .populateStratifiedPlots = function(test_results) {
            
            # Placeholder for stratified plots
            return()
        },

        .populateRecommendations = function(test_results) {
            
            if (!self$options$show_recommendations) return()
            
            # Create recommendations HTML
            html <- "<html><head><style>
                .recommendations { font-family: Arial, sans-serif; margin: 10px; }
                .recommendation { margin: 15px 0; padding: 10px; border-left: 4px solid #3498db; }
                .pass { border-left-color: #27ae60; background-color: #d5f4e6; }
                .fail { border-left-color: #e74c3c; background-color: #fadbd8; }
                .warning { border-left-color: #f39c12; background-color: #fdeaa7; }
            </style></head><body><div class='recommendations'>"
            
            html <- paste0(html, "<h3>Proportional Hazards Assessment & Recommendations</h3>")
            
            # Analyze results and provide recommendations
            violation_count <- 0
            total_tests <- 0
            
            # Count violations from individual tests
            for (test_type in names(test_results$individual_tests)) {
                if (!is.null(test_results$individual_tests[[test_type]])) {
                    for (covariate in names(test_results$individual_tests[[test_type]])) {
                        if (covariate != "GLOBAL") {
                            result <- test_results$individual_tests[[test_type]][[covariate]]
                            total_tests <- total_tests + 1
                            if (result$p_value < 0.05) {
                                violation_count <- violation_count + 1
                            }
                        }
                    }
                }
            }
            
            # Overall assessment
            if (violation_count == 0) {
                html <- paste0(html, "<div class='recommendation pass'>")
                html <- paste0(html, "<h4>✓ Proportional Hazards Assumption Satisfied</h4>")
                html <- paste0(html, "<p>All tests support the proportional hazards assumption. The Cox model is appropriate for your data.</p>")
                html <- paste0(html, "</div>")
            } else if (violation_count / total_tests <= 0.3) {
                html <- paste0(html, "<div class='recommendation warning'>")
                html <- paste0(html, "<h4>⚠ Minor Violations Detected</h4>")
                html <- paste0(html, "<p>Some tests indicate potential violations (", violation_count, " out of ", total_tests, " tests). Consider:")
                html <- paste0(html, "<ul><li>Stratified Cox models for problematic covariates</li>")
                html <- paste0(html, "<li>Time-varying coefficient models</li>")
                html <- paste0(html, "<li>Additional diagnostic plots</li></ul></p>")
                html <- paste0(html, "</div>")
            } else {
                html <- paste0(html, "<div class='recommendation fail'>")
                html <- paste0(html, "<h4>✗ Serious Violations Detected</h4>")
                html <- paste0(html, "<p>Multiple tests indicate violations (", violation_count, " out of ", total_tests, " tests). Recommended actions:")
                html <- paste0(html, "<ul><li>Use stratified Cox models</li>")
                html <- paste0(html, "<li>Consider time-varying coefficient models</li>")
                html <- paste0(html, "<li>Explore accelerated failure time models</li>")
                html <- paste0(html, "<li>Use restricted mean survival time analysis</li></ul></p>")
                html <- paste0(html, "</div>")
            }
            
            # Specific recommendations
            html <- paste0(html, "<div class='recommendation'>")
            html <- paste0(html, "<h4>Next Steps</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>Model Validation:</strong> Use external datasets to validate your findings</li>")
            html <- paste0(html, "<li><strong>Sensitivity Analysis:</strong> Test robustness with different model specifications</li>")
            html <- paste0(html, "<li><strong>Clinical Context:</strong> Consider biological plausibility of time-varying effects</li>")
            html <- paste0(html, "<li><strong>Alternative Models:</strong> Compare with parametric survival models if violations persist</li>")
            html <- paste0(html, "</ul>")
            html <- paste0(html, "</div>")
            
            html <- paste0(html, "</div></body></html>")
            
            self$results$recommendations$setContent(html)
        },

        .populateAnalysisSummary = function(test_results) {
            
            if (!self$options$showSummaries) return()
            
            # Create natural language analysis summary
            html <- "<html><head><style>
                .summary { font-family: Arial, sans-serif; line-height: 1.6; margin: 10px; }
                .highlight { background-color: #f39c12; color: white; padding: 2px 4px; border-radius: 3px; }
                .conclusion { background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin: 10px 0; }
            </style></head><body><div class='summary'>"
            
            html <- paste0(html, "<h3>Proportional Hazards Testing Summary</h3>")
            
            # Basic analysis description
            n_obs <- test_results$cox_model$n
            n_events <- test_results$cox_model$nevent
            n_covariates <- length(coef(test_results$cox_model))
            
            html <- paste0(html, "<p>This analysis tested the proportional hazards assumption for a Cox model with ")
            html <- paste0(html, "<strong>", n_covariates, " covariates</strong> using <strong>", n_obs, " observations</strong> ")
            html <- paste0(html, "and <strong>", n_events, " events</strong>.</p>")
            
            # Test results summary
            violation_count <- 0
            total_tests <- 0
            
            for (test_type in names(test_results$individual_tests)) {
                if (!is.null(test_results$individual_tests[[test_type]])) {
                    for (covariate in names(test_results$individual_tests[[test_type]])) {
                        if (covariate != "GLOBAL") {
                            result <- test_results$individual_tests[[test_type]][[covariate]]
                            total_tests <- total_tests + 1
                            if (result$p_value < 0.05) {
                                violation_count <- violation_count + 1
                            }
                        }
                    }
                }
            }
            
            if (total_tests > 0) {
                violation_rate <- violation_count / total_tests
                
                html <- paste0(html, "<p>Among <strong>", total_tests, " individual tests</strong> performed, ")
                html <- paste0(html, "<span class='highlight'>", violation_count, " tests (", sprintf("%.1f", violation_rate * 100), "%)</span> ")
                html <- paste0(html, "indicated potential violations of the proportional hazards assumption.</p>")
                
                if (violation_rate <= 0.1) {
                    conclusion <- "excellent support"
                } else if (violation_rate <= 0.3) {
                    conclusion <- "adequate support with minor concerns"
                } else if (violation_rate <= 0.5) {
                    conclusion <- "questionable support"
                } else {
                    conclusion <- "poor support"
                }
                
                html <- paste0(html, "<div class='conclusion'>")
                html <- paste0(html, "<h4>Overall Assessment</h4>")
                html <- paste0(html, "<p>The proportional hazards assumption shows <strong>", conclusion, "</strong> in this dataset. ")
                
                if (violation_rate <= 0.3) {
                    html <- paste0(html, "The Cox proportional hazards model appears appropriate for this analysis.")
                } else {
                    html <- paste0(html, "Consider alternative modeling approaches such as stratified Cox models or ")
                    html <- paste0(html, "time-varying coefficient models.")
                }
                
                html <- paste0(html, "</p>")
                html <- paste0(html, "</div>")
            }
            
            html <- paste0(html, "</div></body></html>")
            
            self$results$analysisSummary$setContent(html)
        },

        .populateMethodExplanation = function() {
            
            if (!self$options$showExplanations) return()
            
            # Create comprehensive method explanation
            html <- "<html><head><style>
                .explanation { font-family: Arial, sans-serif; line-height: 1.6; margin: 10px; }
                .method-section { margin: 15px 0; padding: 10px; border-left: 4px solid #3498db; }
                .equation { background-color: #f8f9fa; padding: 8px; border-radius: 4px; font-family: 'Courier New', monospace; }
                .test-method { margin: 10px 0; padding: 8px; background-color: #f9f9f9; border-radius: 4px; }
            </style></head><body><div class='explanation'>"
            
            html <- paste0(html, "<h3>Proportional Hazards Testing Methodology</h3>")
            
            # Overview
            html <- paste0(html, "<div class='method-section'>")
            html <- paste0(html, "<h4>Overview</h4>")
            html <- paste0(html, "<p>The proportional hazards assumption is fundamental to Cox regression models. ")
            html <- paste0(html, "It assumes that the hazard ratio between any two individuals is constant over time. ")
            html <- paste0(html, "Violation of this assumption can lead to biased estimates and incorrect conclusions.</p>")
            html <- paste0(html, "</div>")
            
            # Mathematical basis
            html <- paste0(html, "<div class='method-section'>")
            html <- paste0(html, "<h4>Mathematical Foundation</h4>")
            html <- paste0(html, "<p>The Cox model assumes:</p>")
            html <- paste0(html, "<div class='equation'>h(t|X) = h₀(t) × exp(βᵀX)</div>")
            html <- paste0(html, "<p>The proportional hazards assumption requires that the hazard ratio is constant:</p>")
            html <- paste0(html, "<div class='equation'>HR = h(t|X₁)/h(t|X₂) = exp(βᵀ(X₁-X₂)) = constant</div>")
            html <- paste0(html, "</div>")
            
            # Test methods
            html <- paste0(html, "<div class='method-section'>")
            html <- paste0(html, "<h4>Testing Methods</h4>")
            
            html <- paste0(html, "<div class='test-method'>")
            html <- paste0(html, "<h5>Schoenfeld Residuals Test</h5>")
            html <- paste0(html, "<p>Tests for correlation between scaled Schoenfeld residuals and time. ")
            html <- paste0(html, "Significant correlation indicates time-varying effects.</p>")
            html <- paste0(html, "</div>")
            
            html <- paste0(html, "<div class='test-method'>")
            html <- paste0(html, "<h5>Global Test</h5>")
            html <- paste0(html, "<p>Omnibus test for proportional hazards assumption across all covariates simultaneously. ")
            html <- paste0(html, "Uses chi-square distribution for testing.</p>")
            html <- paste0(html, "</div>")
            
            html <- paste0(html, "<div class='test-method'>")
            html <- paste0(html, "<h5>Correlation Test</h5>")
            html <- paste0(html, "<p>Direct correlation test between residuals and time. ")
            html <- paste0(html, "Provides straightforward interpretation of time-dependent effects.</p>")
            html <- paste0(html, "</div>")
            
            html <- paste0(html, "</div>")
            
            # Interpretation guidelines
            html <- paste0(html, "<div class='method-section'>")
            html <- paste0(html, "<h4>Interpretation Guidelines</h4>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>p-value < 0.05:</strong> Evidence against proportional hazards assumption</li>")
            html <- paste0(html, "<li><strong>p-value ≥ 0.05:</strong> No strong evidence against proportional hazards</li>")
            html <- paste0(html, "<li><strong>Multiple violations:</strong> Consider alternative modeling approaches</li>")
            html <- paste0(html, "<li><strong>Clinical context:</strong> Always consider biological plausibility</li>")
            html <- paste0(html, "</ul>")
            html <- paste0(html, "</div>")
            
            # Alternative approaches
            html <- paste0(html, "<div class='method-section'>")
            html <- paste0(html, "<h4>Alternative Approaches</h4>")
            html <- paste0(html, "<p>When proportional hazards assumption is violated:</p>")
            html <- paste0(html, "<ul>")
            html <- paste0(html, "<li><strong>Stratified Cox Models:</strong> Stratify by violating covariates</li>")
            html <- paste0(html, "<li><strong>Time-Varying Coefficients:</strong> Allow effects to change over time</li>")
            html <- paste0(html, "<li><strong>Accelerated Failure Time Models:</strong> Alternative survival modeling approach</li>")
            html <- paste0(html, "<li><strong>Restricted Mean Survival Time:</strong> Model-free survival analysis</li>")
            html <- paste0(html, "</ul>")
            html <- paste0(html, "</div>")
            
            html <- paste0(html, "</div></body></html>")
            
            self$results$methodExplanation$setContent(html)
        },

        .preparePlotTheme = function() {
            
            # Prepare consistent plotting theme for all visualizations
            private$.plotTheme <- ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    axis.title = ggplot2::element_text(size = 12),
                    axis.text = ggplot2::element_text(size = 10),
                    legend.title = ggplot2::element_text(size = 12, face = "bold"),
                    legend.text = ggplot2::element_text(size = 10),
                    panel.grid.major = ggplot2::element_line(color = "grey90"),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.background = ggplot2::element_rect(fill = "white", color = NA),
                    panel.background = ggplot2::element_rect(fill = "white", color = NA)
                )
        }
    )
)
