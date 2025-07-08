#' @title Lasso-Cox Regression for Variable Selection in Survival Analysis
#' @description
#' Performs Lasso-penalized Cox proportional hazards regression for variable selection
#' in survival analysis. This function uses penalized likelihood to identify the most
#' important predictors while preventing overfitting, making it ideal for high-dimensional
#' survival data where the number of potential predictors may approach or exceed the sample size.
#' 
#' @details
#' The Lasso-Cox regression combines the Cox proportional hazards model with L1 regularization
#' (Lasso penalty) to perform automatic variable selection. The method minimizes the partial
#' likelihood penalized by the L1 norm of the coefficient vector, effectively shrinking
#' less important coefficients toward zero and setting some exactly to zero.
#' 
#' Key features:
#' - Automatic variable selection through L1 regularization
#' - Cross-validation for optimal tuning parameter selection
#' - Risk score calculation and stratification
#' - Comprehensive model performance evaluation
#' - Survival curve visualization by risk groups
#' 
#' The function uses the glmnet package for efficient computation and supports both
#' lambda.min (minimum cross-validation error) and lambda.1se (1 standard error rule)
#' for tuning parameter selection.
#' 
#' @examples
#' \dontrun{
#' # Basic Lasso-Cox regression
#' result <- lassocox(
#'   data = survival_data,
#'   elapsedtime = "time",
#'   outcome = "status", 
#'   outcomeLevel = "1",
#'   explanatory = c("age", "gender", "stage", "grade"),
#'   lambda = "lambda.1se",
#'   nfolds = 10
#' )
#' 
#' # High-dimensional scenario
#' result <- lassocox(
#'   data = genomic_data,
#'   elapsedtime = "survival_time",
#'   outcome = "event",
#'   outcomeLevel = "death",
#'   explanatory = gene_variables,
#'   lambda = "lambda.min",
#'   nfolds = 5,
#'   standardize = TRUE
#' )
#' }
#' 
#' @importFrom R6 R6Class
#' @import jmvcore

lassocoxClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lassocoxClass",
    inherit = lassocoxBase,
    private = list(
        
        # Initialize results and validate dependencies
        .init = function() {
            # Check for required packages
            missing_packages <- c()
            if (!requireNamespace("glmnet", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "glmnet")
            }
            if (!requireNamespace("survival", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "survival")
            }
            if (!requireNamespace("survminer", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "survminer")
            }
            
            if (length(missing_packages) > 0) {
                error_msg <- paste0(
                    "The following required packages are not installed: ",
                    paste(missing_packages, collapse = ", "),
                    "\n\nPlease install them using:\n",
                    "install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))"
                )
                
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'>",
                    "<h4>Missing Dependencies</h4>",
                    "<p>", gsub("\n", "<br>", error_msg), "</p>",
                    "</div>"
                ))
                return()
            }
            
            # Initialize with welcome message if no variables selected
            if (is.null(self$options$elapsedtime) ||
                is.null(self$options$outcome) ||
                is.null(self$options$explanatory)) {

                welcome_msg <- "
                <div class='alert alert-info'>
                <h4>Welcome to Lasso-Cox Regression</h4>
                <p>This analysis performs variable selection in survival analysis using the Lasso penalty.</p>
                
                <h5>Required inputs:</h5>
                <ul>
                <li><strong>Time Elapsed</strong>: Survival/follow-up time (continuous, positive values)</li>
                <li><strong>Outcome</strong>: Binary outcome variable (event/censored status)</li>
                <li><strong>Explanatory Variables</strong>: Potential predictors for selection (≥2 variables)</li>
                </ul>
                
                <h5>The analysis will provide:</h5>
                <ul>
                <li>Variable selection with Lasso regularization</li>
                <li>Cross-validation for optimal tuning parameter</li>
                <li>Model performance metrics (C-index, etc.)</li>
                <li>Risk score calculation and stratification</li>
                <li>Survival curves by risk groups</li>
                <li>Comprehensive visualizations</li>
                </ul>
                
                <h5>Key Features:</h5>
                <ul>
                <li>Handles high-dimensional data (p ≥ n scenarios)</li>
                <li>Automatic variable selection</li>
                <li>Prevents overfitting through regularization</li>
                <li>Clinical risk stratification</li>
                </ul>
                </div>
                "
                
                self$results$todo$setContent(welcome_msg)

                # Hide results until data is provided
                self$results$modelSummary$setVisible(FALSE)
                self$results$coefficients$setVisible(FALSE)
                self$results$performance$setVisible(FALSE)
                self$results$cv_plot$setVisible(FALSE)
                self$results$coef_plot$setVisible(FALSE)
                self$results$survival_plot$setVisible(FALSE)
            }
        },

        .run = function() {
            # Early exits for missing data or variables
            if (is.null(self$data) || nrow(self$data) == 0) {
                return()
            }

            if (is.null(self$options$elapsedtime) ||
                is.null(self$options$outcome) ||
                is.null(self$options$explanatory)) {
                return()
            }

            # Hide welcome message and show results
            self$results$todo$setVisible(FALSE)
            self$results$modelSummary$setVisible(TRUE)
            self$results$coefficients$setVisible(TRUE)
            self$results$performance$setVisible(TRUE)

            # Main analysis pipeline with comprehensive error handling
            tryCatch({
                # Prepare and validate data
                data <- private$.cleanData()
                if (is.null(data)) return()
                
                # Fit Lasso-Cox model
                results <- private$.fitModel(data)
                if (is.null(results)) return()
                
                # Populate result tables
                private$.populateModelSummary(results)
                private$.populateCoefficients(results)
                private$.populatePerformance(results)
                
                # Save plot data for rendering
                private$.savePlotData(results)
                
            }, error = function(e) {
                error_msg <- paste0(
                    "<div class='alert alert-danger'>",
                    "<h4>Analysis Error</h4>",
                    "<p><strong>Error:</strong> ", e$message, "</p>",
                    "<p>Please check your data and variable selections.</p>",
                    "</div>"
                )
                self$results$todo$setContent(error_msg)
                self$results$todo$setVisible(TRUE)
            })
        },

        # Comprehensive data cleaning and validation
        .cleanData = function() {
            data <- self$data
            
            # Validate and clean time variable
            time_var <- self$options$elapsedtime
            time <- jmvcore::toNumeric(data[[time_var]])
            
            if (any(is.na(time))) {
                stop("Time variable contains missing values. Please remove or impute missing observations.")
            }
            if (any(time < 0, na.rm = TRUE)) {
                stop("Time variable contains negative values. Survival times must be non-negative.")
            }
            if (any(time == 0, na.rm = TRUE)) {
                warning("Time variable contains zero values. Consider adding a small constant or reviewing data.")
            }
            
            # Validate and clean outcome variable
            outcome <- data[[self$options$outcome]]
            unique_levels <- unique(na.omit(outcome))
            
            if (length(unique_levels) != 2) {
                stop(paste0("Outcome variable must be binary (exactly 2 levels). Found ", 
                           length(unique_levels), " levels: ", 
                           paste(unique_levels, collapse = ", ")))
            }
            
            if (is.null(self$options$outcomeLevel) || !(self$options$outcomeLevel %in% unique_levels)) {
                stop("Please specify a valid outcome level that represents the event of interest.")
            }
            
            # Create binary status variable (1 = event, 0 = censored)
            status <- as.numeric(outcome == self$options$outcomeLevel)
            
            # Validate predictors
            explanatory_vars <- self$options$explanatory
            if (length(explanatory_vars) < 2) {
                stop("At least 2 explanatory variables are required for Lasso regression.")
            }
            
            predictors <- data[explanatory_vars]
            
            # Check for constant variables
            constant_vars <- sapply(predictors, function(x) {
                if (is.numeric(x)) {
                    var(x, na.rm = TRUE) == 0
                } else {
                    length(unique(na.omit(x))) <= 1
                }
            })
            
            if (any(constant_vars)) {
                constant_var_names <- names(predictors)[constant_vars]
                stop(paste0("The following variables are constant and will be removed: ",
                           paste(constant_var_names, collapse = ", ")))
            }
            
            # Check for complete cases
            complete <- complete.cases(time, status, predictors)
            n_complete <- sum(complete)
            n_events <- sum(status[complete])
            
            if (n_complete < 10) {
                stop(paste0("Too few complete cases for analysis (", n_complete, 
                           "). Need at least 10 complete observations."))
            }
            
            if (n_events < 5) {
                stop(paste0("Too few events for analysis (", n_events, 
                           "). Need at least 5 events for reliable estimation."))
            }
            
            # Validate events per variable ratio
            n_predictors <- ncol(predictors)
            if (n_events < n_predictors && n_events < 10) {
                warning(paste0("Low events per variable ratio (", n_events, " events, ", 
                              n_predictors, " variables). Consider reducing variables or using stronger regularization."))
            }
            
            # Create design matrix with proper handling of factors
            tryCatch({
                # Handle factor variables properly
                factor_vars <- sapply(predictors[complete,], is.factor)
                if (any(factor_vars)) {
                    # Ensure factors have sufficient levels in complete cases
                    for (var_name in names(factor_vars)[factor_vars]) {
                        factor_levels <- length(unique(predictors[complete, var_name]))
                        if (factor_levels < 2) {
                            stop(paste0("Factor variable '", var_name, 
                                       "' has insufficient variation in complete cases."))
                        }
                    }
                }
                
                # Create model matrix
                X <- model.matrix(~ ., data = predictors[complete,])[, -1]
                
                # Check for rank deficiency
                if (ncol(X) == 0) {
                    stop("No valid predictors remaining after processing. Check variable coding.")
                }
                
                # Remove any columns with all zeros or NAs
                valid_cols <- apply(X, 2, function(col) {
                    !all(is.na(col)) && var(col, na.rm = TRUE) > 0
                })
                
                if (!any(valid_cols)) {
                    stop("No valid predictors after removing constant/invalid columns.")
                }
                
                X <- X[, valid_cols, drop = FALSE]
                
            }, error = function(e) {
                stop(paste0("Error creating design matrix: ", e$message, 
                           ". Check factor variable coding and missing values."))
            })
            
            # Standardize variables if requested
            if (self$options$standardize) {
                # Store scaling parameters for later use
                X_scaled <- scale(X)
                scaling_info <- list(
                    center = attr(X_scaled, "scaled:center"),
                    scale = attr(X_scaled, "scaled:scale")
                )
                X <- as.matrix(X_scaled)
            } else {
                scaling_info <- NULL
            }
            
            # Final validation
            if (any(is.infinite(X))) {
                stop("Design matrix contains infinite values. Check for extreme outliers.")
            }
            
            # Return cleaned and validated data
            return(list(
                time = time[complete],
                status = status[complete],
                X = X,
                n = n_complete,
                n_events = n_events,
                n_censored = n_complete - n_events,
                variable_names = colnames(X),
                original_variable_names = explanatory_vars,
                scaling_info = scaling_info,
                complete_cases = which(complete)
            ))
        },

        # Enhanced model fitting with comprehensive error handling
        .fitModel = function(data) {
            # Validate package availability
            if (!requireNamespace("glmnet", quietly = TRUE) ||
                !requireNamespace("survival", quietly = TRUE)) {
                stop("Required packages 'glmnet' and 'survival' not available")
            }
            
            # Create survival object
            y <- survival::Surv(data$time, data$status)
            
            # Validate survival object
            if (any(is.na(y))) {
                stop("Invalid survival object. Check time and status variables.")
            }
            
            # Set up cross-validation parameters
            nfolds <- max(3, min(self$options$nfolds, data$n %/% 3))
            if (nfolds != self$options$nfolds) {
                warning(paste0("Reduced number of CV folds to ", nfolds, 
                              " due to small sample size."))
            }
            
            # Fit cross-validated Lasso-Cox model
            tryCatch({
                set.seed(123456)  # For reproducibility
                cv_fit <- glmnet::cv.glmnet(
                    x = data$X,
                    y = y,
                    family = "cox",
                    alpha = 1,  # Lasso (L1) penalty
                    nfolds = nfolds,
                    standardize = FALSE,  # Already standardized if requested
                    parallel = FALSE  # Avoid parallel processing issues
                )
                
                # Check if cross-validation succeeded
                if (is.null(cv_fit$lambda.min) || is.na(cv_fit$lambda.min)) {
                    stop("Cross-validation failed. Check data quality and sample size.")
                }
                
            }, error = function(e) {
                stop(paste0("Error in cross-validation: ", e$message))
            })
            
            # Get optimal lambda based on user selection
            lambda_optimal <- switch(self$options$lambda,
                "lambda.min" = cv_fit$lambda.min,
                "lambda.1se" = cv_fit$lambda.1se,
                cv_fit$lambda.1se  # Default fallback
            )
            
            # Fit final model with optimal lambda
            tryCatch({
                final_model <- glmnet::glmnet(
                    x = data$X,
                    y = y,
                    family = "cox",
                    alpha = 1,
                    lambda = lambda_optimal,
                    standardize = FALSE
                )
                
            }, error = function(e) {
                stop(paste0("Error fitting final model: ", e$message))
            })
            
            # Extract coefficients and selected variables
            coef_matrix <- as.matrix(coef(final_model, s = lambda_optimal))
            selected_vars <- which(coef_matrix != 0)
            
            if (length(selected_vars) == 0) {
                warning("No variables selected by Lasso. Consider using lambda.min or less regularization.")
                # Use lambda.min as fallback
                lambda_optimal <- cv_fit$lambda.min
                final_model <- glmnet::glmnet(
                    x = data$X, y = y, family = "cox",
                    alpha = 1, lambda = lambda_optimal, standardize = FALSE
                )
                coef_matrix <- as.matrix(coef(final_model, s = lambda_optimal))
                selected_vars <- which(coef_matrix != 0)
            }
            
            # Calculate risk scores
            risk_scores <- as.numeric(predict(final_model, newx = data$X, s = lambda_optimal, type = "link"))
            
            # Calculate comprehensive performance metrics
            performance_metrics <- private$.calculatePerformanceMetrics(y, risk_scores, data)
            
            # Calculate variable importance (absolute coefficient values)
            if (length(selected_vars) > 0) {
                var_importance <- abs(coef_matrix[selected_vars, 1])
                names(var_importance) <- data$variable_names[selected_vars]
                var_importance <- sort(var_importance, decreasing = TRUE)
            } else {
                var_importance <- numeric(0)
            }
            
            return(list(
                cv_fit = cv_fit,
                final_model = final_model,
                coef_matrix = coef_matrix,
                selected_vars = selected_vars,
                lambda_optimal = lambda_optimal,
                risk_scores = risk_scores,
                performance_metrics = performance_metrics,
                var_importance = var_importance,
                data = data
            ))
        },
        
        # Calculate comprehensive performance metrics
        .calculatePerformanceMetrics = function(y, risk_scores, data) {
            metrics <- list()
            
            # C-index (concordance index)
            tryCatch({
                cindex_result <- survival::concordance(y ~ risk_scores)
                metrics$cindex <- cindex_result$concordance
                metrics$cindex_se <- sqrt(cindex_result$var)
            }, error = function(e) {
                metrics$cindex <- NA
                metrics$cindex_se <- NA
            })
            
            # AUC at different time points (if applicable)
            if (requireNamespace("survcomp", quietly = TRUE)) {
                tryCatch({
                    # Calculate AUC at median follow-up time
                    median_time <- median(data$time[data$status == 1])
                    if (!is.na(median_time) && median_time > 0) {
                        # This would require survcomp package
                        # metrics$auc_median <- survcomp::concordance.index(risk_scores, y, time = median_time)$c.index
                    }
                }, error = function(e) {
                    # Silently fail if survcomp not available or calculation fails
                })
            }
            
            # Risk group analysis
            tryCatch({
                risk_groups <- cut(risk_scores, 
                                 breaks = quantile(risk_scores, c(0, 0.5, 1)),
                                 labels = c("Low Risk", "High Risk"),
                                 include.lowest = TRUE)
                
                # Log-rank test
                fit <- survival::survdiff(y ~ risk_groups)
                metrics$logrank_p <- 1 - pchisq(fit$chisq, df = length(fit$n) - 1)
                
                # Hazard ratio between risk groups
                cox_fit <- survival::coxph(y ~ risk_groups)
                metrics$hazard_ratio <- exp(coef(cox_fit))
                metrics$hr_ci_lower <- exp(confint(cox_fit)[1])
                metrics$hr_ci_upper <- exp(confint(cox_fit)[2])
                
            }, error = function(e) {
                metrics$logrank_p <- NA
                metrics$hazard_ratio <- NA
                metrics$hr_ci_lower <- NA
                metrics$hr_ci_upper <- NA
            })
            
            return(metrics)
        },

        # Enhanced table population methods
        .populateModelSummary = function(results) {
            table <- self$results$modelSummary
            
            # Clear existing rows
            table$deleteRows()
            
            # Add model summary statistics
            table$addRow(rowKey = 1, values = list(
                statistic = "Total Variables",
                value = ncol(results$data$X)
            ))
            
            table$addRow(rowKey = 2, values = list(
                statistic = "Selected Variables",
                value = length(results$selected_vars)
            ))
            
            table$addRow(rowKey = 3, values = list(
                statistic = "Selection Proportion",
                value = paste0(round(100 * length(results$selected_vars) / ncol(results$data$X), 1), "%")
            ))
            
            table$addRow(rowKey = 4, values = list(
                statistic = "Optimal Lambda",
                value = format(results$lambda_optimal, scientific = TRUE, digits = 3)
            ))
            
            table$addRow(rowKey = 5, values = list(
                statistic = "Sample Size",
                value = results$data$n
            ))
            
            table$addRow(rowKey = 6, values = list(
                statistic = "Number of Events",
                value = results$data$n_events
            ))
            
            table$addRow(rowKey = 7, values = list(
                statistic = "Censoring Rate",
                value = paste0(round(100 * results$data$n_censored / results$data$n, 1), "%")
            ))
        },

        .populateCoefficients = function(results) {
            table <- self$results$coefficients
            
            # Clear existing rows
            table$deleteRows()
            
            if (length(results$selected_vars) == 0) {
                table$addRow(rowKey = 1, values = list(
                    variable = "No variables selected",
                    coefficient = "",
                    hazardRatio = "",
                    importance = ""
                ))
                return()
            }
            
            # Add coefficient rows for selected variables
            for (i in seq_along(results$selected_vars)) {
                var_idx <- results$selected_vars[i]
                var_name <- results$data$variable_names[var_idx]
                coef_val <- results$coef_matrix[var_idx, 1]
                hr_val <- exp(coef_val)
                importance <- results$var_importance[var_name]
                
                table$addRow(rowKey = i, values = list(
                    variable = var_name,
                    coefficient = round(coef_val, 4),
                    hazardRatio = round(hr_val, 4),
                    importance = round(importance, 4)
                ))
            }
        },

        .populatePerformance = function(results) {
            table <- self$results$performance
            metrics <- results$performance_metrics
            
            # Clear existing rows
            table$deleteRows()
            
            # C-index
            if (!is.na(metrics$cindex)) {
                cindex_text <- if (!is.na(metrics$cindex_se)) {
                    paste0(round(metrics$cindex, 3), " (SE: ", round(metrics$cindex_se, 3), ")")
                } else {
                    round(metrics$cindex, 3)
                }
                
                table$addRow(rowKey = 1, values = list(
                    metric = "C-index",
                    value = cindex_text,
                    interpretation = private$.interpretCindex(metrics$cindex)
                ))
            }
            
            # Log-rank test p-value
            if (!is.na(metrics$logrank_p)) {
                table$addRow(rowKey = 2, values = list(
                    metric = "Log-rank p-value",
                    value = if (metrics$logrank_p < 0.001) "< 0.001" else round(metrics$logrank_p, 3),
                    interpretation = if (metrics$logrank_p < 0.05) "Significant risk stratification" else "Non-significant stratification"
                ))
            }
            
            # Hazard ratio between risk groups
            if (!is.na(metrics$hazard_ratio)) {
                hr_text <- if (!is.na(metrics$hr_ci_lower) && !is.na(metrics$hr_ci_upper)) {
                    paste0(round(metrics$hazard_ratio, 2), " (95% CI: ", 
                          round(metrics$hr_ci_lower, 2), "-", round(metrics$hr_ci_upper, 2), ")")
                } else {
                    round(metrics$hazard_ratio, 2)
                }
                
                table$addRow(rowKey = 3, values = list(
                    metric = "Hazard Ratio (High vs Low Risk)",
                    value = hr_text,
                    interpretation = private$.interpretHazardRatio(metrics$hazard_ratio)
                ))
            }
        },
        
        # Helper functions for interpretation
        .interpretCindex = function(cindex) {
            if (is.na(cindex)) return("Not available")
            if (cindex < 0.6) return("Poor discrimination")
            if (cindex < 0.7) return("Fair discrimination") 
            if (cindex < 0.8) return("Good discrimination")
            return("Excellent discrimination")
        },
        
        .interpretHazardRatio = function(hr) {
            if (is.na(hr)) return("Not available")
            if (hr < 1.5) return("Weak risk stratification")
            if (hr < 2.5) return("Moderate risk stratification")
            return("Strong risk stratification")
        },

        # Enhanced plotting functions
        .cvPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$cv_plot) return()
            
            results <- image$state
            if (is.null(results)) return()
            
            # Create ggplot version of CV plot
            cv_data <- data.frame(
                lambda = results$cv_fit$lambda,
                cvm = results$cv_fit$cvm,
                cvsd = results$cv_fit$cvsd,
                cvup = results$cv_fit$cvup,
                cvlo = results$cv_fit$cvlo
            )
            
            # Add optimal lambda points
            lambda_min_idx <- which.min(abs(cv_data$lambda - results$cv_fit$lambda.min))
            lambda_1se_idx <- which.min(abs(cv_data$lambda - results$cv_fit$lambda.1se))
            
            p <- ggplot2::ggplot(cv_data, ggplot2::aes(x = log(lambda), y = cvm)) +
                ggplot2::geom_point(color = "red", size = 0.8) +
                ggplot2::geom_errorbar(ggplot2::aes(ymin = cvlo, ymax = cvup), 
                                      color = "darkgrey", width = 0.02) +
                ggplot2::geom_vline(xintercept = log(results$cv_fit$lambda.min), 
                                   linetype = "dashed", color = "blue") +
                ggplot2::geom_vline(xintercept = log(results$cv_fit$lambda.1se), 
                                   linetype = "dashed", color = "green") +
                ggplot2::labs(
                    title = "Cross-Validation Plot",
                    subtitle = "Blue: lambda.min, Green: lambda.1se",
                    x = "Log Lambda",
                    y = "Partial Likelihood Deviance"
                ) +
                ggtheme
            
            print(p)
            TRUE
        },

        .coefPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$coef_plot) return()
            
            results <- image$state
            if (is.null(results) || length(results$selected_vars) == 0) return()
            
            # Create coefficient plot
            coef_data <- data.frame(
                variable = factor(names(results$var_importance), 
                                levels = names(results$var_importance)),
                coefficient = as.numeric(results$coef_matrix[results$selected_vars, 1]),
                importance = results$var_importance
            )
            
            p <- ggplot2::ggplot(coef_data, ggplot2::aes(x = variable, y = coefficient)) +
                ggplot2::geom_col(ggplot2::aes(fill = coefficient > 0), alpha = 0.7) +
                ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                                          labels = c("Protective", "Risk Factor"),
                                          name = "Effect") +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = "Selected Variables and Coefficients",
                    x = "Variables",
                    y = "Coefficient"
                ) +
                ggtheme
            
            print(p)
            TRUE
        },

        .survivalPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$survival_plot) return()
            
            results <- image$state
            if (is.null(results)) return()
            
            # Create risk groups based on median risk score
            risk_groups <- cut(results$risk_scores,
                             breaks = c(-Inf, median(results$risk_scores), Inf),
                             labels = c("Low Risk", "High Risk"))
            
            # Create survival object
            surv_obj <- survival::Surv(results$data$time, results$data$status)
            
            # Fit survival curves
            fit <- survival::survfit(surv_obj ~ risk_groups)
            
            # Create enhanced survival plot
            if (requireNamespace("survminer", quietly = TRUE)) {
                p <- survminer::ggsurvplot(
                    fit,
                    data = data.frame(risk_groups = risk_groups),
                    risk.table = TRUE,
                    pval = TRUE,
                    conf.int = TRUE,
                    ggtheme = ggtheme,
                    title = "Survival Curves by Risk Groups",
                    xlab = "Time",
                    ylab = "Survival Probability",
                    legend.title = "Risk Group",
                    palette = c("blue", "red")
                )
                print(p)
            } else {
                # Fallback to base plot
                plot(fit, col = c("blue", "red"), lwd = 2,
                     xlab = "Time", ylab = "Survival Probability",
                     main = "Survival Curves by Risk Groups")
                legend("topright", legend = c("Low Risk", "High Risk"),
                       col = c("blue", "red"), lwd = 2)
            }
            
            TRUE
        },

        .savePlotData = function(results) {
            # Save data for plot rendering
            if (self$options$cv_plot) {
                self$results$cv_plot$setState(results)
            }
            
            if (self$options$coef_plot && length(results$selected_vars) > 0) {
                self$results$coef_plot$setState(results)
            }
            
            if (self$options$survival_plot) {
                self$results$survival_plot$setState(results)
            }
            
            # Add risk scores to dataset if requested
            if (!is.null(self$results$riskScore)) {
                # Create full-length vector with NAs for missing cases
                full_risk_scores <- rep(NA, nrow(self$data))
                full_risk_scores[results$data$complete_cases] <- results$risk_scores
                self$results$riskScore$setValues(full_risk_scores)
            }
        }
    )
)