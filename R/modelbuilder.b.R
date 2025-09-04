#' @title Prediction Model Builder for Clinical Decision Analysis
#' @description
#' Comprehensive clinical prediction model builder with advanced validation and performance assessment.
#' Creates multiple logistic regression models optimized for integration with Decision Curve Analysis.
#' Provides robust error handling, comprehensive validation, and clinical interpretation guidance.
#'
#' @details
#' The Prediction Model Builder supports multiple modeling approaches:
#' - **Basic Clinical Models**: Core demographic and primary risk factors
#' - **Enhanced Clinical Models**: Extended clinical variables and interactions
#' - **Biomarker Models**: Integration of laboratory values and advanced diagnostics
#' - **Custom Models**: User-defined variable combinations
#'
#' Key features include:
#' - Automatic data splitting for unbiased validation
#' - Advanced missing data handling with multiple imputation
#' - Comprehensive performance metrics (AUC, calibration, NRI, IDI)
#' - Cross-validation and bootstrap validation
#' - Stepwise selection and penalized regression
#' - Seamless integration with Decision Curve Analysis
#' - Clinical risk score generation
#' - Robust error handling and validation
#'
#' @examples
#' \dontrun{
#' # Basic clinical model
#' result <- modelbuilder(
#'   data = clinical_data,
#'   outcome = "cardiovascular_event",
#'   outcomePositive = "Yes",
#'   basicPredictors = c("age", "sex", "diabetes"),
#'   buildBasicModel = TRUE
#' )
#'
#' # Enhanced model with biomarkers
#' result <- modelbuilder(
#'   data = clinical_data,
#'   outcome = "cardiovascular_event",
#'   outcomePositive = "Yes",
#'   biomarkerPredictors = c("age", "sex", "diabetes", "troponin"),
#'   buildBiomarkerModel = TRUE,
#'   crossValidation = TRUE,
#'   splitData = TRUE
#' )
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats glm predict confint coef logLik AIC BIC step
#' @importFrom stats na.omit qlogis plogis formula rbinom rnorm
#' @importFrom utils capture.output
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_smooth geom_abline
#' @importFrom ggplot2 labs theme_minimal scale_color_brewer facet_wrap
#' @importFrom ggplot2 geom_ribbon geom_histogram geom_density scale_x_continuous
#' @importFrom dplyr mutate select filter summarise group_by arrange
#' @importFrom pROC roc auc ci.auc coords
#' @importFrom magrittr %>%
#' @importFrom htmltools HTML
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom rms rcs

# Null-coalescing operator helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# Helper function to escape variable names with special characters for formulas
.escapeVariableNames <- function(var_names) {
    # Check if variable names contain special characters that need escaping
    need_escaping <- grepl("[^a-zA-Z0-9._]", var_names)
    var_names[need_escaping] <- paste0("`", var_names[need_escaping], "`")
    return(var_names)
}

modelbuilderClass <- if (requireNamespace("jmvcore")) R6::R6Class(
    "modelbuilderClass",
    inherit = modelbuilderBase,
    private = list(

        # Store analysis results
        .models = list(),
        .trainingData = NULL,
        .validationData = NULL,
        .predictions = list(),
        .performance = list(),
        .crossValidationResults = list(),
        .bootstrapResults = list(),
        .analysisMetadata = NULL,
        .performanceMetrics = NULL,
        .warnings = character(0),
        .errors = character(0),

        # Initialize the analysis - called first
        .init = function() {
            # Show welcome message initially
            private$.showWelcomeMessage()
        },

        # Show welcome message when needed
        .showWelcomeMessage = function() {
            # Check if we should show instructions
            show_instructions <- is.null(self$options$outcome)
            
            # Check if any model type is selected AND has variables
            has_any_model_with_variables <- FALSE
            
            if (self$options$buildBasicModel && !is.null(self$options$basicPredictors) && length(self$options$basicPredictors) > 0) {
                has_any_model_with_variables <- TRUE
            }
            if (self$options$buildEnhancedModel && !is.null(self$options$enhancedPredictors) && length(self$options$enhancedPredictors) > 0) {
                has_any_model_with_variables <- TRUE
            }
            if (self$options$buildBiomarkerModel && !is.null(self$options$biomarkerPredictors) && length(self$options$biomarkerPredictors) > 0) {
                has_any_model_with_variables <- TRUE
            }
            if (self$options$buildCustomModel && !is.null(self$options$customPredictors) && length(self$options$customPredictors) > 0) {
                has_any_model_with_variables <- TRUE
            }
            
            # Show instructions if no outcome OR no model types selected OR no variables selected for enabled models
            if (show_instructions || !has_any_model_with_variables) {
                
                instructions <- paste0(
                    "<html><head></head><body>",
                    "<div class='instructions' style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 20px 0;'>",
                    "<h3 style='color: #2e7d32; margin-top: 0;'>🏗️ ", "Prediction Model Builder", "</h3>",
                    "<p><strong>", "Build and validate prediction models for medical decision making.", "</strong></p>",
                    "<p>", "Creates logistic regression models that output predicted probabilities for use in Decision Curve Analysis.", "</p>",
                    
                    "<h4 style='color: #2e7d32;'>", "Required Steps:", "</h4>",
                    "<ol>",
                    "<li><strong>", "Select Outcome Variable:", "</strong> ", "Choose a binary outcome to predict", "</li>",
                    "<li><strong>", "Specify Positive Level:", "</strong> ", "Define which level represents the positive outcome", "</li>",
                    "<li><strong>", "Select Variables and Model Types:", "</strong> ", "Choose variables and enable at least one model:", 
                        "<ul>",
                        "<li><strong>", "Basic Clinical Model:", "</strong> ", "Add variables to 'Basic Clinical Variables' and check 'Build basic clinical model'", "</li>",
                        "<li><strong>", "Enhanced Clinical Model:", "</strong> ", "Add variables to 'Enhanced Clinical Variables' and check 'Build enhanced clinical model'", "</li>",
                        "<li><strong>", "Biomarker Model:", "</strong> ", "Add variables to 'Biomarker Variables' and check 'Build biomarker model'", "</li>",
                        "<li><strong>", "Custom Model:", "</strong> ", "Add variables to 'Custom Variables' and check 'Build custom model'", "</li>",
                        "</ul>",
                    "</li>",
                    "<li><strong>", "Configure Options:", "</strong> ", "Set validation, missing data handling, and output preferences", "</li>",
                    "</ol>",
                    
                    "<h4 style='color: #2e7d32;'>", "Advanced Features:", "</h4>",
                    "<ul>",
                    "<li>", "Automatic data splitting for unbiased validation", "</li>",
                    "<li>", "Multiple imputation for missing data", "</li>",
                    "<li>", "Cross-validation and bootstrap validation", "</li>",
                    "<li>", "Comprehensive performance metrics", "</li>",
                    "<li>", "Seamless integration with Decision Curve Analysis", "</li>",
                    "</ul>",
                    
                    "<p><strong>", "The module will create predicted probability columns that can be directly used in Decision Curve Analysis.", "</strong></p>",
                    "</div></body></html>"
                )

                self$results$instructions$setContent(instructions)
                
                # Add additional guidance with error handling
                tryCatch({
                    private$.addClinicalGuidance()
                }, error = function(e) {
                    # Silently continue if guidance fails
                })
                
                tryCatch({
                    private$.applyClinicalPreset()
                }, error = function(e) {
                    # Silently continue if preset fails
                })
                
                tryCatch({
                    private$.addGlossary()
                }, error = function(e) {
                    # Silently continue if glossary fails
                })
                
                return(TRUE)
            } else {
                # Hide instructions when analysis can proceed
                self$results$instructions$setVisible(FALSE)
                return(FALSE)
            }
        },

        # Enhanced error handling and validation
        .validateInputs = function() {
            data <- self$data
            outcome_var <- self$options$outcome
            
            # Check data availability
            if (is.null(data) || nrow(data) == 0) {
                stop("No data available for analysis")
            }
            
            # Check outcome variable
            if (is.null(outcome_var)) {
                stop("Outcome variable must be specified")
            }
            
            if (!outcome_var %in% names(data)) {
                stop(paste0("Outcome variable '", outcome_var, "' not found in data"))
            }
            
            # Check outcome variable levels
            unique_outcomes <- unique(data[[outcome_var]])
            unique_outcomes <- unique_outcomes[!is.na(unique_outcomes)]
            
            # Check positive outcome level
            outcome_positive <- self$options$outcomePositive
            if (is.null(outcome_positive) || !outcome_positive %in% unique_outcomes) {
                stop(paste0("Positive outcome level must be one of: ", paste(unique_outcomes, collapse = ", ")))
            }
            
            # For multi-level outcomes, validate that we can create a meaningful binary outcome
            if (length(unique_outcomes) > 2) {
                if (is.null(outcome_positive)) {
                    stop(paste0("Multi-level outcome variable requires specifying positive outcome level. Found ", length(unique_outcomes), " levels: ", paste(unique_outcomes, collapse = ", ")))
                }
                # This will be converted to binary: positive level vs all others
            } else if (length(unique_outcomes) < 2) {
                stop("Outcome variable must have at least 2 levels")
            }
            
            # Check if at least one model is selected
            if (!self$options$buildBasicModel && !self$options$buildEnhancedModel && 
                !self$options$buildBiomarkerModel && !self$options$buildCustomModel) {
                stop("At least one model type must be selected")
            }
            
            # Check sample size requirements
            n_total <- nrow(data)
            n_events <- sum(data[[outcome_var]] == outcome_positive, na.rm = TRUE)
            
            if (n_total < 50) {
                stop(paste0("Sample size too small. Minimum ", 50, " observations required"))
            }
            
            if (n_events < 10) {
                stop(paste0("Too few events. Minimum ", 10, " events required for stable model fitting"))
            }
            
            # Check events per variable (EPV) rule  
            max_predictors <- 0
            if (self$options$buildBasicModel && !is.null(self$options$basicPredictors)) {
                max_predictors <- max(max_predictors, length(self$options$basicPredictors))
            }
            if (self$options$buildEnhancedModel && !is.null(self$options$enhancedPredictors)) {
                max_predictors <- max(max_predictors, length(self$options$enhancedPredictors))
            }
            if (self$options$buildBiomarkerModel && !is.null(self$options$biomarkerPredictors)) {
                max_predictors <- max(max_predictors, length(self$options$biomarkerPredictors))
            }
            if (self$options$buildCustomModel && !is.null(self$options$customPredictors)) {
                max_predictors <- max(max_predictors, length(self$options$customPredictors))
            }
            
            if (max_predictors > 0) {
                epv <- n_events / max_predictors
                if (epv < 5) {
                    warning(paste0("Low events per variable ratio (", round(epv, 1), "). Consider reducing predictors or using penalized regression"))
                    private$.warnings <- c(private$.warnings, paste0("Low EPV: ", round(epv, 1)))
                }
            }
            
            return(TRUE)
            
            # Add clinical misuse detection
            self$.detectClinicalMisuse()
        },
        
        # Clinical misuse detection and recommendations
        .detectClinicalMisuse = function() {
            data <- self$data
            outcome_var <- self$options$outcome
            
            if (is.null(data) || is.null(outcome_var)) return()
            
            n_total <- nrow(data)
            n_events <- sum(data[[outcome_var]] == self$options$outcomePositive, na.rm = TRUE)
            event_rate <- n_events / n_total
            
            clinical_warnings <- c()
            clinical_recommendations <- c()
            
            # Sample size warnings with clinical context
            if (n_total < 100) {
                clinical_warnings <- c(clinical_warnings, 
                    paste0("Small sample size (", n_total, " patients). Consider collecting more data for robust clinical predictions."))
            }
            
            # Event rate warnings
            if (event_rate < 0.05 || event_rate > 0.95) {
                clinical_warnings <- c(clinical_warnings,
                    paste0("Extreme event rate (", round(event_rate * 100, 1), 
                           "%). Model may have poor calibration."))
            }
            
            # EPV warnings with clinical recommendations  
            if (self$options$buildBasicModel && !is.null(self$options$basicPredictors) && length(self$options$basicPredictors) > 0) {
                epv <- n_events / length(self$options$basicPredictors)
                if (epv < 10) {
                    clinical_warnings <- c(clinical_warnings,
                        paste0("Low events per variable (", round(epv, 1), 
                               " for basic model). Reduce predictors or use penalized regression."))
                }
            }
            
            # Missing data warnings
            missing_vars <- names(data)[sapply(data, function(x) sum(is.na(x)) > 0)]
            high_missing <- names(data)[sapply(data, function(x) sum(is.na(x))/length(x) > 0.2)]
            
            if (length(high_missing) > 0) {
                clinical_warnings <- c(clinical_warnings,
                    paste0("Variables with >20% missing data: ", paste(high_missing, collapse = ", ")))
            }
            
            # Store warnings for display
            if (length(clinical_warnings) > 0) {
                private$.warnings <- c(private$.warnings, clinical_warnings)
            }
        },

        # Check package dependencies
        .checkPackageDependencies = function() {
            required_packages <- c("pROC", "dplyr", "ggplot2")
            missing_packages <- c()
            
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_packages <- c(missing_packages, pkg)
                }
            }
            
            if (length(missing_packages) > 0) {
                stop(paste0("Required packages not available: ", paste(missing_packages, collapse = ", ")))
            }
            
            # Check for optional packages
            optional_packages <- c("mice", "glmnet", "rms", "survival")
            for (pkg in optional_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    warning(paste0("Optional package '", pkg, "' not available. Some features may be limited."))
                    private$.warnings <- c(private$.warnings, paste0("Missing optional package: ", pkg))
                }
            }
            
            return(TRUE)
        },

        # Enhanced data validation and preparation
        .validateAndPrepareData = function(data) {
            # Check for perfect separation
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            
            # Convert multi-level outcome to binary if necessary
            unique_outcomes <- unique(data[[outcome_var]])
            unique_outcomes <- unique_outcomes[!is.na(unique_outcomes)]
            
            if (length(unique_outcomes) > 2) {
                # Create binary outcome: positive level vs all others
                # Handle missing values properly
                binary_outcome <- ifelse(is.na(data[[outcome_var]]), NA, 
                                       as.numeric(data[[outcome_var]] == outcome_positive))
                data[[outcome_var]] <- binary_outcome
                message(paste0("Converted multi-level outcome to binary: '", outcome_positive, "' (1) vs others (0)"))
            }
            
            # Additional validation: check if we have any events after conversion
            if (!is.null(outcome_positive)) {
                n_events <- sum(data[[outcome_var]] == 1, na.rm = TRUE)
                if (n_events == 0) {
                    warning(paste0("No events found for positive outcome level '", outcome_positive, "' after data processing"))
                    private$.warnings <- c(private$.warnings, paste0("Zero events for outcome: ", outcome_positive))
                }
            }
            
            for (var in names(data)) {
                if (is.factor(data[[var]]) || is.character(data[[var]])) {
                    # Check for perfect separation
                    cross_tab <- table(data[[var]], data[[outcome_var]])
                    if (any(cross_tab == 0)) {
                        warning(paste0("Variable '", var, "' may cause perfect separation"))
                        private$.warnings <- c(private$.warnings, paste0("Perfect separation risk: ", var))
                    }
                }
            }
            
            # Check for high correlation between predictors
            numeric_vars <- sapply(data, is.numeric)
            if (sum(numeric_vars) > 1) {
                cor_matrix <- cor(data[, numeric_vars, drop = FALSE], use = "complete.obs")
                high_cor_pairs <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, arr.ind = TRUE)
                
                if (nrow(high_cor_pairs) > 0) {
                    for (i in 1:nrow(high_cor_pairs)) {
                        var1 <- rownames(cor_matrix)[high_cor_pairs[i, 1]]
                        var2 <- colnames(cor_matrix)[high_cor_pairs[i, 2]]
                        cor_value <- cor_matrix[high_cor_pairs[i, 1], high_cor_pairs[i, 2]]
                        warning(paste0("High correlation between '", var1, "' and '", var2, "': ", round(cor_value, 2)))
                        private$.warnings <- c(private$.warnings, paste0("High correlation: ", var1, " vs ", var2))
                    }
                }
            }
            
            # Check for near-zero variance
            for (var in names(data)) {
                if (is.numeric(data[[var]])) {
                    var_coef <- sd(data[[var]], na.rm = TRUE) / mean(data[[var]], na.rm = TRUE)
                    if (var_coef < 0.01) {
                        warning(paste("Variable", var, "has near-zero variance"))
                        private$.warnings <- c(private$.warnings, paste("Near-zero variance:", var))
                    }
                }
            }
            
            return(data)
        },

        # Monitor performance and timing
        .monitorPerformance = function(operation, func) {
            start_time <- Sys.time()
            result <- func()
            end_time <- Sys.time()
            
            execution_time <- as.numeric(end_time - start_time)
            
            if (is.null(private$.performanceMetrics)) {
                private$.performanceMetrics <- data.frame(
                    operation = character(0),
                    execution_time = numeric(0),
                    timestamp = character(0),
                    stringsAsFactors = FALSE
                )
            }
            
            private$.performanceMetrics <- rbind(
                private$.performanceMetrics,
                data.frame(
                    operation = operation,
                    execution_time = execution_time,
                    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    stringsAsFactors = FALSE
                )
            )
            
            return(result)
        },

        # Display performance metrics to users for transparency
        .displayPerformanceMetrics = function() {
            if (is.null(private$.performanceMetrics) || nrow(private$.performanceMetrics) == 0) {
                return()
            }
            
            # Create performance metrics HTML table
            perf_html <- paste0(
                "<html><body>",
                "<div style='background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #17a2b8;'>",
                "<h5 style='color: #17a2b8; margin-top: 0;'>⚡ ", "Performance Metrics", "</h5>",
                "<table style='width: 100%; font-size: 12px;'>",
                "<tr><th>", "Operation", "</th><th>", "Time (s)", "</th><th>", "Timestamp", "</th></tr>"
            )
            
            for (i in 1:nrow(private$.performanceMetrics)) {
                row <- private$.performanceMetrics[i, ]
                perf_html <- paste0(perf_html,
                    "<tr><td>", row$operation, "</td>",
                    "<td>", sprintf("%.3f", row$execution_time), "</td>",
                    "<td>", row$timestamp, "</td></tr>"
                )
            }
            
            total_time <- sum(private$.performanceMetrics$execution_time)
            perf_html <- paste0(perf_html,
                "<tr style='font-weight: bold; border-top: 1px solid #ccc;'>",
                "<td>Total Analysis Time</td>",
                "<td>", sprintf("%.3f", total_time), "</td>",
                "<td>-</td></tr>",
                "</table>",
                "</div>",
                "</body></html>"
            )
            
            # Add to data summary or create separate section
            current_summary <- self$results$dataSummary$content
            if (!is.null(current_summary)) {
                # Append to existing summary
                updated_summary <- gsub("</body></html>", paste0(perf_html, "</body></html>"), current_summary)
                self$results$dataSummary$setContent(updated_summary)
            }
        },

        # Enhanced data splitting with stratification
        .splitData = function(data, split_ratio = 0.7) {
            set.seed(self$options$randomSeed)
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            
            # Stratified sampling to maintain event rate
            positive_indices <- which(data[[outcome_var]] == outcome_positive)
            negative_indices <- which(data[[outcome_var]] != outcome_positive)
            
            # Ensure minimum events in both sets
            n_pos <- length(positive_indices)
            n_neg <- length(negative_indices)
            
            if (n_pos < 10 || n_neg < 10) {
                stop("Insufficient events for stratified splitting")
            }
            
            # Sample stratified indices
            pos_train_n <- max(5, floor(n_pos * split_ratio))
            neg_train_n <- max(5, floor(n_neg * split_ratio))
            
            pos_train_idx <- sample(positive_indices, pos_train_n)
            neg_train_idx <- sample(negative_indices, neg_train_n)
            
            train_idx <- c(pos_train_idx, neg_train_idx)
            val_idx <- setdiff(1:nrow(data), train_idx)
            
            training_data <- data[train_idx, ]
            validation_data <- data[val_idx, ]
            
            # Verify stratification worked
            train_event_rate <- mean(training_data[[outcome_var]] == outcome_positive)
            val_event_rate <- mean(validation_data[[outcome_var]] == outcome_positive)
            
            if (abs(train_event_rate - val_event_rate) > 0.1) {
                warning("Large difference in event rates between training and validation sets")
                private$.warnings <- c(private$.warnings, "Stratification may be suboptimal")
            }
            
            return(list(
                training = training_data,
                validation = validation_data,
                train_event_rate = train_event_rate,
                val_event_rate = val_event_rate
            ))
        },

        # Enhanced missing data handling with multiple imputation
        .handleMissingData = function(data) {
            method <- self$options$missingDataMethod
            
            # Check missing data patterns
            missing_prop <- sapply(data, function(x) sum(is.na(x)) / length(x))
            vars_with_missing <- names(missing_prop)[missing_prop > 0]
            
            if (length(vars_with_missing) > 0) {
                missing_summary <- paste0("Missing data found in: ", paste(vars_with_missing, collapse = ", "))
                private$.warnings <- c(private$.warnings, missing_summary)
            }
            
            if (method == "complete_cases") {
                result <- na.omit(data)
                n_removed <- nrow(data) - nrow(result)
                if (n_removed > 0) {
                    warning(paste("Removed", n_removed, "observations with missing data"))
                    private$.warnings <- c(private$.warnings, paste("Complete cases: removed", n_removed, "observations"))
                }
                return(result)
                
            } else if (method == "mean_imputation") {
                # Enhanced mean/mode imputation with validation
                result <- data
                for (col in names(data)) {
                    if (is.numeric(data[[col]])) {
                        missing_count <- sum(is.na(data[[col]]))
                        if (missing_count > 0) {
                            mean_val <- mean(data[[col]], na.rm = TRUE)
                            result[[col]][is.na(result[[col]])] <- mean_val
                            private$.warnings <- c(private$.warnings, 
                                paste("Mean imputation for", col, ":", missing_count, "values"))
                        }
                    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
                        missing_count <- sum(is.na(data[[col]]))
                        if (missing_count > 0) {
                            mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                            result[[col]][is.na(result[[col]])] <- mode_val
                            private$.warnings <- c(private$.warnings, 
                                paste("Mode imputation for", col, ":", missing_count, "values"))
                        }
                    }
                }
                return(result)
                
            } else if (method == "multiple_imputation") {
                if (requireNamespace("mice", quietly = TRUE)) {
                    # Multiple imputation using mice
                    n_imputations <- self$options$imputationSets %||% 5
                    
                    mice_result <- mice::mice(
                        data = data,
                        m = n_imputations,
                        maxit = 5,
                        method = "auto",
                        printFlag = FALSE
                    )
                    
                    # Return the first completed dataset for model building
                    result <- mice::complete(mice_result, 1)
                    
                    # Store mice results for later use
                    private$.analysisMetadata$mice_results <- mice_result
                    
                    private$.warnings <- c(private$.warnings, 
                        paste("Multiple imputation performed with", n_imputations, "datasets"))
                    
                    return(result)
                } else {
                    warning("mice package not available. Falling back to mean imputation.")
                    return(private$.handleMissingData(data))
                }
                
            } else if (method == "exclude_missing") {
                # Remove variables with >20% missing
                threshold <- 0.20
                keep_vars <- names(missing_prop)[missing_prop <= threshold]
                excluded_vars <- names(missing_prop)[missing_prop > threshold]
                
                if (length(excluded_vars) > 0) {
                    warning(paste("Excluded variables with >20% missing:", paste(excluded_vars, collapse = ", ")))
                    private$.warnings <- c(private$.warnings, 
                        paste("Excluded high-missing variables:", paste(excluded_vars, collapse = ", ")))
                }
                
                result <- data[, keep_vars, drop = FALSE]
                return(result)
            }
            
            return(data)
        },

        # Enhanced penalized regression model building
        .buildPenalizedModel = function(formula, data, model_name) {
            if (!self$options$penalizedRegression) return(NULL)
            
            tryCatch({
                if (!requireNamespace("glmnet", quietly = TRUE)) {
                    warning("glmnet package not available for penalized regression")
                    private$.warnings <- c(private$.warnings, "glmnet package required for penalized regression")
                    return(NULL)
                }
                
                # Extract variables from formula
                outcome_var <- all.vars(formula)[1]
                predictor_vars <- all.vars(formula)[-1]
                
                # Prepare matrices for glmnet (handle factors properly)
                X <- model.matrix(formula, data = data)[, -1, drop = FALSE]  # Remove intercept
                y <- as.numeric(data[[outcome_var]] == self$options$outcomePositive)
                
                # Check for sufficient observations
                if (nrow(X) < 20 || sum(y) < 5) {
                    stop("Insufficient observations for penalized regression")
                }
                
                # Set alpha based on penalty type
                alpha <- switch(self$options$penaltyType,
                               "lasso" = 1,
                               "ridge" = 0, 
                               "elastic_net" = 0.5,
                               1)  # default to lasso
                
                # Fit model with cross-validation for lambda selection
                set.seed(self$options$randomSeed)
                cv_fit <- glmnet::cv.glmnet(X, y, family = "binomial", alpha = alpha, 
                                          nfolds = min(10, nrow(X)), type.measure = "auc")
                
                # Get best model
                best_model <- glmnet::glmnet(X, y, family = "binomial", alpha = alpha, 
                                           lambda = cv_fit$lambda.1se)
                
                # Create a wrapper object that mimics glm interface
                penalized_model <- list(
                    glmnet_fit = best_model,
                    cv_fit = cv_fit,
                    X = X,
                    y = y,
                    formula = formula,
                    alpha = alpha,
                    lambda = cv_fit$lambda.1se,
                    converged = TRUE,
                    family = list(family = "binomial", link = "logit"),
                    model_type = "penalized",
                    data = data
                )
                
                # Add predict method
                penalized_model$predict <- function(object, newdata = NULL, type = "response") {
                    if (is.null(newdata)) {
                        newdata <- object$data
                    }
                    X_new <- model.matrix(object$formula, data = newdata)[, -1, drop = FALSE]
                    pred <- predict(object$glmnet_fit, newx = X_new, s = object$lambda, type = type)
                    as.vector(pred)
                }
                
                class(penalized_model) <- c("penalized_glm", "list")
                
                # Generate predictions
                train_pred <- penalized_model$predict(penalized_model, type = "response")
                
                # Store model and predictions
                private$.models[[model_name]] <- penalized_model
                private$.predictions[[paste0(model_name, "_train")]] <- train_pred
                
                # Generate predictions for validation data (if available)
                if (!is.null(private$.validationData)) {
                    val_pred <- penalized_model$predict(penalized_model, newdata = private$.validationData, type = "response")
                    private$.predictions[[paste0(model_name, "_val")]] <- val_pred
                }
                
                # Store model diagnostics
                model_diagnostics <- list(
                    converged = TRUE,
                    iterations = length(cv_fit$lambda),
                    deviance = deviance(cv_fit),
                    lambda = cv_fit$lambda.1se,
                    alpha = alpha,
                    cv_auc = max(cv_fit$cvm, na.rm = TRUE),
                    n_nonzero = cv_fit$nzero[cv_fit$lambda == cv_fit$lambda.1se]
                )
                
                private$.performance[[paste0(model_name, "_diagnostics")]] <- model_diagnostics
                
                return(penalized_model)
                
            }, error = function(e) {
                error_msg <- paste("Failed to build penalized", model_name, ":", e$message)
                private$.errors <- c(private$.errors, error_msg)
                warning(error_msg)
                return(NULL)
            })
        },

        # Enhanced logistic regression model building with validation
        .buildLogisticModel = function(formula, data, model_name) {
            tryCatch({
                # Validate formula and data
                if (is.null(formula) || !inherits(formula, "formula")) {
                    stop("Invalid formula provided")
                }
                
                # Check for missing variables in formula
                formula_vars <- all.vars(formula)
                missing_vars <- formula_vars[!formula_vars %in% names(data)]
                if (length(missing_vars) > 0) {
                    stop(paste0("Variables not found in data: ", paste(missing_vars, collapse = ", ")))
                }
                
                # Check for sufficient observations
                if (nrow(data) < 20) {
                    stop("Insufficient observations for model fitting")
                }
                
                # Fit model with convergence checking
                model <- glm(formula, family = binomial, data = data, 
                           control = glm.control(maxit = 100, epsilon = 1e-8))
                
                # Check for convergence
                if (!model$converged) {
                    warning(paste0("Model '", model_name, "' did not converge"))
                    private$.warnings <- c(private$.warnings, paste0("Convergence issue: ", model_name))
                }
                
                # Check for quasi-complete separation
                if (any(abs(coef(model)) > 10, na.rm = TRUE)) {
                    warning(paste0("Model '", model_name, "' may have separation issues"))
                    private$.warnings <- c(private$.warnings, paste0("Separation warning: ", model_name))
                }
                
                # Check for perfect predictions
                train_pred <- predict(model, type = "response")
                if (any(train_pred < 0.001) || any(train_pred > 0.999)) {
                    warning(paste0("Model '", model_name, "' produces extreme predictions"))
                    private$.warnings <- c(private$.warnings, paste0("Extreme predictions: ", model_name))
                }
                
                # Store model and predictions
                private$.models[[model_name]] <- model
                private$.predictions[[paste0(model_name, "_train")]] <- train_pred
                
                # Generate predictions for validation data (if available)
                if (!is.null(private$.validationData)) {
                    val_pred <- predict(model, newdata = private$.validationData, type = "response")
                    private$.predictions[[paste0(model_name, "_val")]] <- val_pred
                }
                
                # Store model diagnostics
                model_diagnostics <- list(
                    converged = model$converged,
                    iterations = model$iter,
                    deviance = model$deviance,
                    null_deviance = model$null.deviance,
                    df_residual = model$df.residual,
                    aic = AIC(model),
                    bic = BIC(model)
                )
                
                private$.performance[[paste0(model_name, "_diagnostics")]] <- model_diagnostics
                
                return(model)
                
            }, error = function(e) {
                error_msg <- paste("Failed to build", model_name, ":", e$message)
                private$.errors <- c(private$.errors, error_msg)
                stop(error_msg)
            })
        },

        # Enhanced performance metrics calculation
        .calculatePerformance = function(model, predictions, actual, dataset_type = "training") {
            outcome_positive <- self$options$outcomePositive
            
            # Check if actual values are already binary (0/1) or need conversion
            unique_actual <- unique(actual[!is.na(actual)])
            if (all(unique_actual %in% c(0, 1))) {
                # Already binary
                binary_actual <- as.numeric(actual)
            } else {
                # Need conversion from original levels
                binary_actual <- as.numeric(actual == outcome_positive)
            }
            
            # Remove missing values from both predictions and actual
            complete_cases <- !is.na(predictions) & !is.na(binary_actual)
            if (sum(complete_cases) == 0) {
                stop("No complete cases available for performance calculation")
            }
            
            predictions <- predictions[complete_cases]
            binary_actual <- binary_actual[complete_cases]
            
            # Validate inputs after missing value removal
            if (length(predictions) != length(binary_actual)) {
                stop(paste0("Predictions and actual values must have same length after removing missing values. Predictions: ", 
                           length(predictions), ", Actual: ", length(binary_actual)))
            }
            
            if (length(unique(binary_actual)) < 2) {
                warning(paste0("Only one outcome level present in ", dataset_type, " data"))
                # Return minimal performance metrics for single-class case
                return(list(
                    auc = NA,
                    auc_lower = NA,
                    auc_upper = NA,
                    sensitivity = NA,
                    specificity = NA,
                    brier_score = NA,
                    calibration_slope = NA,
                    calibration_intercept = NA,
                    n_events = sum(binary_actual == 1, na.rm = TRUE),
                    n_total = length(binary_actual)
                ))
            }
            
            # Ensure predictions are in valid range
            predictions <- pmax(0.001, pmin(0.999, predictions))
            
            # Discrimination metrics
            if (requireNamespace("pROC", quietly = TRUE)) {
                roc_obj <- pROC::roc(binary_actual, predictions, quiet = TRUE)
                auc_value <- as.numeric(pROC::auc(roc_obj))
                
                # AUC confidence interval
                auc_ci <- pROC::ci.auc(roc_obj, quiet = TRUE)
                auc_lower <- auc_ci[1]
                auc_upper <- auc_ci[3]
                
                # Optimal threshold
                optimal_coords <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
                optimal_threshold <- optimal_coords$threshold[1]
                optimal_sensitivity <- optimal_coords$sensitivity[1]
                optimal_specificity <- optimal_coords$specificity[1]
            } else {
                # Fallback AUC calculation
                auc_value <- private$.calculateAUCFallback(binary_actual, predictions)
                auc_lower <- auc_upper <- NA
                optimal_threshold <- 0.5
                optimal_sensitivity <- optimal_specificity <- NA
            }
            
            # Calibration metrics
            calibration_result <- tryCatch({
                cal_data <- data.frame(
                    outcome = binary_actual,
                    logit_pred = qlogis(predictions)
                )
                
                calibration_model <- glm(outcome ~ logit_pred, family = binomial, data = cal_data)
                
                list(
                    slope = coef(calibration_model)[2],
                    intercept = coef(calibration_model)[1],
                    p_value_slope = summary(calibration_model)$coefficients[2, 4],
                    p_value_intercept = summary(calibration_model)$coefficients[1, 4]
                )
            }, error = function(e) {
                warning("Calibration calculation failed")
                list(slope = NA, intercept = NA, p_value_slope = NA, p_value_intercept = NA)
            })
            
            # Brier score and decomposition
            brier_score <- mean((predictions - binary_actual)^2)
            
            # Hosmer-Lemeshow test
            hl_result <- private$.hosmerLemeshowTest(binary_actual, predictions)
            
            # Net benefit at different thresholds
            net_benefits <- private$.calculateNetBenefit(binary_actual, predictions)
            
            # Classification metrics at optimal threshold
            predicted_class <- as.numeric(predictions > optimal_threshold)
            
            # Confusion matrix
            tp <- sum(predicted_class == 1 & binary_actual == 1)
            fp <- sum(predicted_class == 1 & binary_actual == 0)
            tn <- sum(predicted_class == 0 & binary_actual == 0)
            fn <- sum(predicted_class == 0 & binary_actual == 1)
            
            # Derived metrics
            ppv <- tp / (tp + fp)  # Positive predictive value
            npv <- tn / (tn + fn)  # Negative predictive value
            
            # Model fit statistics (handle penalized models)
            if (inherits(model, "penalized_glm")) {
                # For penalized models, use available metrics
                log_likelihood <- NA
                aic_value <- NA
                bic_value <- NA
                mcfadden_r2 <- NA
                nagelkerke_r2 <- NA
            } else {
                # Regular GLM model
                log_likelihood <- logLik(model)[1]
                aic_value <- AIC(model)
                bic_value <- BIC(model)
                
                # R-squared measures
                null_deviance <- model$null.deviance
                residual_deviance <- model$deviance
                mcfadden_r2 <- 1 - (residual_deviance / null_deviance)
                
                # Nagelkerke R-squared
                n <- length(binary_actual)
                nagelkerke_r2 <- (1 - exp((residual_deviance - null_deviance) / n)) / 
                                (1 - exp(-null_deviance / n))
            }
            
            return(list(
                # Discrimination
                auc = auc_value,
                auc_lower = auc_lower,
                auc_upper = auc_upper,
                
                # Calibration
                calibration_slope = calibration_result$slope,
                calibration_intercept = calibration_result$intercept,
                calibration_slope_p = calibration_result$p_value_slope,
                calibration_intercept_p = calibration_result$p_value_intercept,
                
                # Overall performance
                brier_score = brier_score,
                hosmer_lemeshow_p = hl_result$p_value,
                hosmer_lemeshow_stat = hl_result$statistic,
                
                # Classification at optimal threshold
                optimal_threshold = optimal_threshold,
                sensitivity = optimal_sensitivity,
                specificity = optimal_specificity,
                ppv = ppv,
                npv = npv,
                
                # Model fit
                log_likelihood = log_likelihood,
                aic = aic_value,
                bic = bic_value,
                mcfadden_r2 = mcfadden_r2,
                nagelkerke_r2 = nagelkerke_r2,
                
                # Additional metrics
                n_predictors = length(coef(model)) - 1,
                n_observations = length(binary_actual),
                n_events = sum(binary_actual),
                event_rate = mean(binary_actual),
                
                # Net benefit
                net_benefit_10 = net_benefits$nb_10,
                net_benefit_20 = net_benefits$nb_20,
                net_benefit_30 = net_benefits$nb_30,
                
                # Dataset type
                dataset_type = dataset_type
            ))
        },
        
        # Fallback AUC calculation when pROC is not available
        .calculateAUCFallback = function(actual, predictions) {
            # Simple AUC calculation using Mann-Whitney U
            n1 <- sum(actual == 1)
            n0 <- sum(actual == 0)
            
            if (n1 == 0 || n0 == 0) return(NA)
            
            # Create all pairwise comparisons
            comparisons <- outer(predictions[actual == 1], predictions[actual == 0], ">")
            ties <- outer(predictions[actual == 1], predictions[actual == 0], "==")
            
            auc <- (sum(comparisons) + 0.5 * sum(ties)) / (n1 * n0)
            return(auc)
        },
        
        # Hosmer-Lemeshow goodness of fit test
        .hosmerLemeshowTest = function(actual, predictions, groups = 10) {
            tryCatch({
                # Create risk groups
                risk_groups <- cut(predictions, breaks = quantile(predictions, probs = seq(0, 1, 1/groups)), 
                                 include.lowest = TRUE, labels = 1:groups)
                
                # Calculate expected and observed for each group
                observed <- tapply(actual, risk_groups, sum)
                expected <- tapply(predictions, risk_groups, sum)
                n_group <- tapply(actual, risk_groups, length)
                
                # Remove groups with no observations
                valid_groups <- !is.na(observed) & n_group > 0
                observed <- observed[valid_groups]
                expected <- expected[valid_groups]
                n_group <- n_group[valid_groups]
                
                # Calculate chi-square statistic
                chi_square <- sum((observed - expected)^2 / (expected * (1 - expected/n_group)))
                
                # Degrees of freedom
                df <- length(observed) - 2
                
                # P-value
                p_value <- 1 - pchisq(chi_square, df)
                
                return(list(statistic = chi_square, p_value = p_value, df = df))
            }, error = function(e) {
                return(list(statistic = NA, p_value = NA, df = NA))
            })
        },
        
        # Calculate net benefit at different thresholds
        .calculateNetBenefit = function(actual, predictions) {
            thresholds <- c(0.10, 0.20, 0.30)
            net_benefits <- numeric(length(thresholds))
            
            for (i in seq_along(thresholds)) {
                threshold <- thresholds[i]
                
                # True positives and false positives
                tp <- sum(predictions >= threshold & actual == 1)
                fp <- sum(predictions >= threshold & actual == 0)
                
                # Net benefit calculation
                n <- length(actual)
                net_benefit <- (tp / n) - (fp / n) * (threshold / (1 - threshold))
                net_benefits[i] <- net_benefit
            }
            
            return(list(
                nb_10 = net_benefits[1],
                nb_20 = net_benefits[2],
                nb_30 = net_benefits[3]
            ))
        },

        # Apply stepwise selection
        .applyStepwiseSelection = function(model, direction, criterion) {
            if (!self$options$useStepwise) return(model)

            # Determine k parameter for stepwise
            k_param <- if (criterion == "bic") log(nrow(model$model)) else 2

            # Apply stepwise selection
            step_model <- step(model,
                               direction = direction,
                               k = k_param,
                               trace = 0)  # Suppress output

            return(step_model)
        },

        # Transform continuous variables
        .transformVariables = function(data, predictors) {
            if (!self$options$transformVariables) return(data)

            method <- self$options$transformMethod

            for (var in predictors) {
                if (is.numeric(data[[var]])) {
                    if (method == "log") {
                        # Log transformation (add small constant to handle zeros)
                        data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
                    } else if (method == "polynomial") {
                        # Quadratic term
                        data[[paste0(var, "_sq")]] <- data[[var]]^2
                    } else if (method == "spline") {
                        # Restricted cubic splines (using rcs if available)
                        if (requireNamespace("rms", quietly = TRUE)) {
                            tryCatch({
                                # Create spline basis (3 knots is standard)
                                spline_basis <- rms::rcs(data[[var]], nk = 3)
                                
                                # Add spline terms to data
                                for (i in 1:ncol(spline_basis)) {
                                    data[[paste0(var, "_spline", i)]] <- spline_basis[, i]
                                }
                            }, error = function(e) {
                                warning(paste("Spline transformation failed for", var, "- using polynomial instead"))
                                data[[paste0(var, "_sq")]] <<- data[[var]]^2
                            })
                        } else {
                            warning("rms package not available for spline transformation - using polynomial")
                            data[[paste0(var, "_sq")]] <- data[[var]]^2
                        }
                    }
                }
            }

            return(data)
        },

        # Create interaction terms
        .createInteractions = function(data, predictors) {
            if (!self$options$includeInteractions) return(data)

            # If specific interactions specified
            if (self$options$interactionTerms != "") {
                # Parse interaction terms (simplified)
                interaction_specs <- strsplit(self$options$interactionTerms, ",")[[1]]
                for (spec in interaction_specs) {
                    vars <- trimws(strsplit(spec, "\\*")[[1]])
                    if (length(vars) == 2 && all(vars %in% predictors)) {
                        interaction_name <- paste(vars, collapse = "_x_")
                        if (is.numeric(data[[vars[1]]]) && is.numeric(data[[vars[2]]])) {
                            data[[interaction_name]] <- data[[vars[1]]] * data[[vars[2]]]
                        }
                    }
                }
            } else {
                # Create all pairwise interactions for numeric variables
                numeric_vars <- predictors[sapply(data[predictors], is.numeric)]
                if (length(numeric_vars) > 1) {
                    for (i in 1:(length(numeric_vars)-1)) {
                        for (j in (i+1):length(numeric_vars)) {
                            var1 <- numeric_vars[i]
                            var2 <- numeric_vars[j]
                            interaction_name <- paste(var1, var2, sep = "_x_")
                            data[[interaction_name]] <- data[[var1]] * data[[var2]]
                        }
                    }
                }
            }

            return(data)
        },

        # Generate clinical risk score
        .generateClinicalRiskScore = function(model, model_name) {
            if (!self$options$generateRiskScore) return(NULL)

            coefficients <- coef(model)
            score_method <- self$options$riskScorePoints

            # Simple scoring system based on regression coefficients
            score_table <- data.frame(
                variable = names(coefficients)[-1],  # Exclude intercept
                coefficient = coefficients[-1],
                stringsAsFactors = FALSE
            )

            # Convert coefficients to integer points
            if (score_method == "simple") {
                # Scale coefficients to 0-10 point range
                max_coef <- max(abs(score_table$coefficient))
                score_table$points <- round(score_table$coefficient / max_coef * 5)
            } else if (score_method == "framingham") {
                # Age-based scaling (simplified)
                score_table$points <- round(score_table$coefficient * 10)
            }

            score_table$category <- "Per unit increase"
            score_table$interpretation <- paste("Each unit increase adds", score_table$points, "points")

            return(score_table)
        },

        # Enhanced cross-validation with comprehensive metrics
        .performCrossValidation = function(formula, data, k_folds = 5) {
            if (!self$options$crossValidation) return(NULL)
            
            set.seed(self$options$randomSeed)
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            
            # Stratified k-fold cross-validation
            positive_indices <- which(data[[outcome_var]] == outcome_positive)
            negative_indices <- which(data[[outcome_var]] != outcome_positive)
            
            # Create stratified folds
            pos_folds <- sample(rep(1:k_folds, length.out = length(positive_indices)))
            neg_folds <- sample(rep(1:k_folds, length.out = length(negative_indices)))
            
            fold_indices <- numeric(nrow(data))
            fold_indices[positive_indices] <- pos_folds
            fold_indices[negative_indices] <- neg_folds
            
            # Initialize results
            cv_results <- data.frame(
                fold = 1:k_folds,
                auc = numeric(k_folds),
                auc_lower = numeric(k_folds),
                auc_upper = numeric(k_folds),
                brier_score = numeric(k_folds),
                calibration_slope = numeric(k_folds),
                calibration_intercept = numeric(k_folds),
                hosmer_lemeshow_p = numeric(k_folds),
                sensitivity = numeric(k_folds),
                specificity = numeric(k_folds),
                ppv = numeric(k_folds),
                npv = numeric(k_folds),
                n_train = numeric(k_folds),
                n_test = numeric(k_folds),
                n_events_train = numeric(k_folds),
                n_events_test = numeric(k_folds),
                converged = logical(k_folds)
            )
            
            # Store fold predictions for later analysis
            all_predictions <- numeric(nrow(data))
            all_predictions[] <- NA
            
            for (fold in 1:k_folds) {
                tryCatch({
                    # Split data
                    train_data <- data[fold_indices != fold, ]
                    test_data <- data[fold_indices == fold, ]
                    
                    # Check minimum sample sizes
                    if (nrow(train_data) < 20 || nrow(test_data) < 5) {
                        warning(paste("Fold", fold, "has insufficient sample size"))
                        next
                    }
                    
                    # Fit model
                    fold_model <- glm(formula, family = binomial, data = train_data,
                                    control = glm.control(maxit = 100))
                    
                    # Check convergence
                    cv_results$converged[fold] <- fold_model$converged
                    
                    # Predict on test set
                    predictions <- predict(fold_model, newdata = test_data, type = "response")
                    actual <- test_data[[outcome_var]]
                    binary_actual <- as.numeric(actual == outcome_positive)
                    
                    # Store predictions
                    all_predictions[fold_indices == fold] <- predictions
                    
                    # Calculate comprehensive metrics
                    if (length(unique(binary_actual)) > 1 && !any(is.na(predictions))) {
                        perf_metrics <- private$.calculatePerformance(fold_model, predictions, actual, "cv")
                        
                        # Store metrics
                        cv_results$auc[fold] <- perf_metrics$auc
                        cv_results$auc_lower[fold] <- perf_metrics$auc_lower
                        cv_results$auc_upper[fold] <- perf_metrics$auc_upper
                        cv_results$brier_score[fold] <- perf_metrics$brier_score
                        cv_results$calibration_slope[fold] <- perf_metrics$calibration_slope
                        cv_results$calibration_intercept[fold] <- perf_metrics$calibration_intercept
                        cv_results$hosmer_lemeshow_p[fold] <- perf_metrics$hosmer_lemeshow_p
                        cv_results$sensitivity[fold] <- perf_metrics$sensitivity
                        cv_results$specificity[fold] <- perf_metrics$specificity
                        cv_results$ppv[fold] <- perf_metrics$ppv
                        cv_results$npv[fold] <- perf_metrics$npv
                    }
                    
                    # Sample size information
                    cv_results$n_train[fold] <- nrow(train_data)
                    cv_results$n_test[fold] <- nrow(test_data)
                    cv_results$n_events_train[fold] <- sum(train_data[[outcome_var]] == outcome_positive)
                    cv_results$n_events_test[fold] <- sum(test_data[[outcome_var]] == outcome_positive)
                    
                }, error = function(e) {
                    warning(paste("Error in fold", fold, ":", e$message))
                    private$.warnings <- c(private$.warnings, paste("CV fold", fold, "failed:", e$message))
                })
            }
            
            # Calculate summary statistics
            cv_summary <- list(
                mean_auc = mean(cv_results$auc, na.rm = TRUE),
                sd_auc = sd(cv_results$auc, na.rm = TRUE),
                median_auc = median(cv_results$auc, na.rm = TRUE),
                mean_brier = mean(cv_results$brier_score, na.rm = TRUE),
                sd_brier = sd(cv_results$brier_score, na.rm = TRUE),
                mean_calibration_slope = mean(cv_results$calibration_slope, na.rm = TRUE),
                sd_calibration_slope = sd(cv_results$calibration_slope, na.rm = TRUE),
                convergence_rate = mean(cv_results$converged, na.rm = TRUE),
                successful_folds = sum(!is.na(cv_results$auc))
            )
            
            # Calculate optimism-corrected estimates
            if (!is.null(private$.trainingData)) {
                # Training performance
                train_outcome <- private$.trainingData[[outcome_var]]
                train_predictions <- predict(private$.models[[1]], type = "response")
                train_perf <- private$.calculatePerformance(private$.models[[1]], train_predictions, train_outcome, "training")
                
                # Optimism calculation
                optimism_auc <- train_perf$auc - cv_summary$mean_auc
                optimism_brier <- cv_summary$mean_brier - train_perf$brier_score
                
                cv_summary$optimism_auc <- optimism_auc
                cv_summary$optimism_brier <- optimism_brier
                cv_summary$corrected_auc <- train_perf$auc - optimism_auc
                cv_summary$corrected_brier <- train_perf$brier_score + optimism_brier
            }
            
            return(list(
                results = cv_results,
                summary = cv_summary,
                predictions = all_predictions
            ))
        },

        # Enhanced main analysis function
        .run = function() {
            
            # Initialize analysis metadata
            private$.analysisMetadata <- list(
                start_time = Sys.time(),
                version = tryCatch({
                    packageVersion("ClinicoPath")
                }, error = function(e) {
                    "development"
                }),
                session_info = sessionInfo()
            )
            
            # Show welcome message if needed
            if (private$.showWelcomeMessage()) {
                return()
            }
            
            # Comprehensive validation and setup
            tryCatch({
                # Step 1: Check dependencies
                private$.monitorPerformance("dependency_check", function() {
                    private$.checkPackageDependencies()
                })
                
                # Step 2: Validate inputs
                private$.monitorPerformance("input_validation", function() {
                    private$.validateInputs()
                })
                
                # Step 3: Get and validate data
                data <- self$data
                outcome_var <- self$options$outcome
                outcome_positive <- self$options$outcomePositive
                
                # Step 4: Data validation and preparation
                private$.checkpoint() # Before expensive data validation
                data <- private$.monitorPerformance("data_validation", function() {
                    private$.validateAndPrepareData(data)
                })
                
                # Step 5: Handle missing data
                private$.checkpoint() # Before expensive missing data handling
                data <- private$.monitorPerformance("missing_data_handling", function() {
                    private$.handleMissingData(data)
                })
                
                # Step 6: Data splitting
                if (self$options$splitData) {
                    split_result <- private$.monitorPerformance("data_splitting", function() {
                        private$.splitData(data)
                    })
                    private$.trainingData <- split_result$training
                    private$.validationData <- split_result$validation
                    modeling_data <- private$.trainingData
                } else {
                    private$.trainingData <- data
                    private$.validationData <- NULL
                    modeling_data <- data
                }
                
                # Step 7: Create data summary
                private$.createDataSummary(data, modeling_data)
                
                # Step 8: Build models
                private$.checkpoint() # Before expensive model building
                models_built <- private$.monitorPerformance("model_building", function() {
                    private$.buildAllModels(modeling_data)
                })
                
                # Step 9: Calculate performance metrics
                if (self$options$showPerformanceMetrics || self$options$compareModels) {
                    private$.checkpoint() # Before expensive performance calculations
                    private$.monitorPerformance("performance_calculation", function() {
                        private$.calculateAllPerformance(models_built)
                    })
                }
                
                # Step 10: Cross-validation
                if (self$options$crossValidation) {
                    private$.checkpoint() # Before expensive cross-validation
                    private$.monitorPerformance("cross_validation", function() {
                        private$.performAllCrossValidation(models_built, modeling_data)
                    })
                }
                
                # Step 11: Bootstrap validation
                if (self$options$bootstrapValidation) {
                    private$.checkpoint() # Before expensive bootstrap validation
                    private$.monitorPerformance("bootstrap_validation", function() {
                        private$.performBootstrapValidation(models_built, modeling_data)
                    })
                }
                
                # Step 12: Advanced metrics (NRI, IDI)
                if (self$options$calculateNRI || self$options$calculateIDI) {
                    private$.checkpoint() # Before expensive advanced metrics
                    private$.monitorPerformance("advanced_metrics", function() {
                        private$.calculateAdvancedMetrics(models_built)
                    })
                }
                
                # Step 13: Generate predictions
                if (self$options$createPredictions) {
                    private$.monitorPerformance("prediction_generation", function() {
                        private$.addPredictionsToDataset()
                    })
                }
                
                # Step 14: Prepare for DCA
                if (self$options$exportForDCA) {
                    private$.prepareDCAOutput()
                }
                
                # Step 15: Generate risk scores
                if (self$options$generateRiskScore) {
                    private$.generateAllRiskScores(models_built)
                }
                
                # Step 16: Initialize calibration plots array
                if (self$options$showCalibrationPlots && length(models_built) > 0) {
                    private$.initializeCalibrationPlotsArray(models_built)
                }
                
                # Step 17: Create final summary
                private$.createFinalSummary(models_built)
                
                # Store analysis completion time and display performance metrics
                private$.analysisMetadata$end_time <- Sys.time()
                private$.analysisMetadata$total_time <- private$.analysisMetadata$end_time - private$.analysisMetadata$start_time
                
                # Display performance metrics if available
                private$.displayPerformanceMetrics()
                
            }, error = function(e) {
                # Enhanced error handling with detailed reporting
                error_msg <- paste0("Analysis failed: ", e$message)
                private$.errors <- c(private$.errors, error_msg)
                
                # Create error report
                error_report <- paste0(
                    "<html><body>",
                    "<div style='background-color: #fff3cd; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>⚠️ Analysis Error</h4>",
                    "<p><strong>Error Message:</strong> ", e$message, "</p>",
                    "<p><strong>Location:</strong> ", deparse(e$call)[1], "</p>",
                    if (length(private$.warnings) > 0) {
                        paste0("<p><strong>Warnings:</strong></p><ul>",
                               paste0("<li>", private$.warnings, "</li>", collapse = ""),
                               "</ul>")
                    } else "",
                    "<p><strong>Troubleshooting:</strong></p>",
                    "<ul>",
                    "<li>Check that your outcome variable is binary</li>",
                    "<li>Ensure sufficient sample size (>50 observations, >10 events)</li>",
                    "<li>Verify that predictor variables are correctly specified</li>",
                    "<li>Check for missing data patterns</li>",
                    "<li>Ensure required packages are installed</li>",
                    "</ul>",
                    "</div>",
                    "</body></html>"
                )
                
                self$results$instructions$setContent(error_report)
                stop(error_msg)
            })
        },
        
        # Helper methods for the enhanced .run() function
        .createDataSummary = function(data, modeling_data) {
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            
            n_total <- nrow(data)
            n_training <- nrow(modeling_data)
            n_validation <- if (!is.null(private$.validationData)) nrow(private$.validationData) else 0
            event_rate <- mean(data[[outcome_var]] == outcome_positive) * 100
            
            # Additional statistics
            n_events_total <- sum(data[[outcome_var]] == outcome_positive)
            n_events_training <- sum(modeling_data[[outcome_var]] == outcome_positive)
            n_events_validation <- if (!is.null(private$.validationData)) sum(private$.validationData[[outcome_var]] == outcome_positive) else 0
            
            # Missing data summary
            missing_summary <- ""
            if (length(private$.warnings) > 0) {
                missing_warnings <- private$.warnings[grep("missing|imputation", private$.warnings, ignore.case = TRUE)]
                if (length(missing_warnings) > 0) {
                    missing_summary <- paste0(
                        "<p><strong>Missing Data:</strong> ", 
                        paste(missing_warnings, collapse = "; "), "</p>"
                    )
                }
            }
            
            data_summary <- paste0(
                "<html><body>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>📊 ", "Data Summary", "</h4>",
                "<p><strong>", "Total Sample Size:", "</strong> ", n_total, "</p>",
                "<p><strong>", "Training Set:", "</strong> ", n_training, " (", round(n_training/n_total*100, 1), "%)</p>",
                if (n_validation > 0) paste0("<p><strong>", "Validation Set:", "</strong> ", n_validation, " (", round(n_validation/n_total*100, 1), "%)</p>") else "",
                "<p><strong>", "Event Rate:", "</strong> ", round(event_rate, 1), "% (", outcome_positive, ")</p>",
                "<p><strong>", "Events in Training:", "</strong> ", n_events_training, "</p>",
                if (n_validation > 0) paste0("<p><strong>", "Events in Validation:", "</strong> ", n_events_validation, "</p>") else "",
                missing_summary,
                "</div>",
                "</body></html>"
            )
            
            self$results$dataSummary$setContent(data_summary)
            
            # Generate clinical summary if models exist
            if (length(private$.models) > 0) {
                self$.generateClinicalSummary()
                self$.generateReportSentences()
            }
            
            # Clean up memory after analysis
            private$.cleanup()
        },
        
        .buildAllModels = function(modeling_data) {
            models_built <- c()
            outcome_var <- self$options$outcome
            
            # Basic Clinical Model
            if (self$options$buildBasicModel && !is.null(self$options$basicPredictors) && length(self$options$basicPredictors) > 0) {
                private$.checkpoint() # Before building basic model
                basic_predictors <- self$options$basicPredictors
                
                # Apply transformations and interactions
                modeling_data <- private$.transformVariables(modeling_data, basic_predictors)
                modeling_data <- private$.createInteractions(modeling_data, basic_predictors)
                
                # Build formula
                escaped_predictors <- .escapeVariableNames(basic_predictors)
                predictor_string <- paste(escaped_predictors, collapse = " + ")
                formula_basic <- as.formula(paste(outcome_var, "~", predictor_string))
                
                # Build model (penalized or regular logistic)
                if (self$options$penalizedRegression) {
                    basic_model <- private$.buildPenalizedModel(formula_basic, modeling_data, "basic")
                } else {
                    basic_model <- private$.buildLogisticModel(formula_basic, modeling_data, "basic")
                    
                    # Apply stepwise selection if requested (only for regular models)
                    basic_model <- private$.applyStepwiseSelection(basic_model,
                                                                   self$options$stepwiseDirection,
                                                                   self$options$selectionCriterion)
                }
                private$.models[["basic"]] <- basic_model
                
                models_built <- c(models_built, "basic")
                
                # Populate model summary table
                if (self$options$showModelSummary) {
                    private$.populateModelSummary(basic_model, "basicModelSummary")
                }
            }
            
            # Enhanced Clinical Model
            if (self$options$buildEnhancedModel && !is.null(self$options$enhancedPredictors) && length(self$options$enhancedPredictors) > 0) {
                private$.checkpoint() # Before building enhanced model
                enhanced_predictors <- self$options$enhancedPredictors
                
                # Apply transformations and interactions
                modeling_data <- private$.transformVariables(modeling_data, enhanced_predictors)
                modeling_data <- private$.createInteractions(modeling_data, enhanced_predictors)
                
                # Build formula
                escaped_predictors <- .escapeVariableNames(enhanced_predictors)
                predictor_string <- paste(escaped_predictors, collapse = " + ")
                formula_enhanced <- as.formula(paste(outcome_var, "~", predictor_string))
                
                # Build model (penalized or regular logistic)
                if (self$options$penalizedRegression) {
                    enhanced_model <- private$.buildPenalizedModel(formula_enhanced, modeling_data, "enhanced")
                } else {
                    enhanced_model <- private$.buildLogisticModel(formula_enhanced, modeling_data, "enhanced")
                    
                    # Apply stepwise selection if requested (only for regular models)
                    enhanced_model <- private$.applyStepwiseSelection(enhanced_model,
                                                                      self$options$stepwiseDirection,
                                                                      self$options$selectionCriterion)
                }
                private$.models[["enhanced"]] <- enhanced_model
                
                models_built <- c(models_built, "enhanced")
                
                # Populate model summary table
                if (self$options$showModelSummary) {
                    private$.populateModelSummary(enhanced_model, "enhancedModelSummary")
                }
            }
            
            # Biomarker Model
            if (self$options$buildBiomarkerModel && !is.null(self$options$biomarkerPredictors) && length(self$options$biomarkerPredictors) > 0) {
                private$.checkpoint() # Before building biomarker model
                biomarker_predictors <- self$options$biomarkerPredictors
                
                # Apply transformations and interactions
                modeling_data <- private$.transformVariables(modeling_data, biomarker_predictors)
                modeling_data <- private$.createInteractions(modeling_data, biomarker_predictors)
                
                # Build formula
                escaped_predictors <- .escapeVariableNames(biomarker_predictors)
                predictor_string <- paste(escaped_predictors, collapse = " + ")
                formula_biomarker <- as.formula(paste(outcome_var, "~", predictor_string))
                
                # Build model (penalized or regular logistic)
                if (self$options$penalizedRegression) {
                    biomarker_model <- private$.buildPenalizedModel(formula_biomarker, modeling_data, "biomarker")
                } else {
                    biomarker_model <- private$.buildLogisticModel(formula_biomarker, modeling_data, "biomarker")
                    
                    # Apply stepwise selection if requested (only for regular models)
                    biomarker_model <- private$.applyStepwiseSelection(biomarker_model,
                                                                       self$options$stepwiseDirection,
                                                                       self$options$selectionCriterion)
                }
                private$.models[["biomarker"]] <- biomarker_model
                
                models_built <- c(models_built, "biomarker")
                
                # Populate model summary table
                if (self$options$showModelSummary) {
                    private$.populateModelSummary(biomarker_model, "biomarkerModelSummary")
                }
            }
            
            # Custom Model
            if (self$options$buildCustomModel && !is.null(self$options$customPredictors) && length(self$options$customPredictors) > 0) {
                private$.checkpoint() # Before building custom model
                custom_predictors <- self$options$customPredictors
                
                # Apply transformations and interactions
                modeling_data <- private$.transformVariables(modeling_data, custom_predictors)
                modeling_data <- private$.createInteractions(modeling_data, custom_predictors)
                
                # Build formula
                escaped_predictors <- .escapeVariableNames(custom_predictors)
                predictor_string <- paste(escaped_predictors, collapse = " + ")
                formula_custom <- as.formula(paste(outcome_var, "~", predictor_string))
                
                # Build model (penalized or regular logistic)
                if (self$options$penalizedRegression) {
                    custom_model <- private$.buildPenalizedModel(formula_custom, modeling_data, "custom")
                } else {
                    custom_model <- private$.buildLogisticModel(formula_custom, modeling_data, "custom")
                    
                    # Apply stepwise selection if requested (only for regular models)
                    custom_model <- private$.applyStepwiseSelection(custom_model,
                                                                    self$options$stepwiseDirection,
                                                                    self$options$selectionCriterion)
                }
                private$.models[["custom"]] <- custom_model
                
                models_built <- c(models_built, "custom")
                
                # Populate model summary table
                if (self$options$showModelSummary) {
                    private$.populateModelSummary(custom_model, "customModelSummary")
                }
            }
            
            return(models_built)
        },
        
        
        # Extract model validation logic
        .validateModelResults = function(model, model_name) {
            if (is.null(model)) {
                private$.warnings <- c(private$.warnings, 
                    paste0("Failed to build model: ", model_name))
                return(FALSE)
            }
            
            # Check for convergence issues
            if (inherits(model, "glm")) {
                if (!model$converged) {
                    warning(paste0("Model '", model_name, "' did not converge"))
                    private$.warnings <- c(private$.warnings, 
                        paste0("Convergence issue: ", model_name))
                }
                
                # Check for separation issues
                if (any(abs(coef(model)) > 10, na.rm = TRUE)) {
                    warning(paste0("Model '", model_name, "' may have separation issues"))
                    private$.warnings <- c(private$.warnings, 
                        paste0("Separation warning: ", model_name))
                }
            }
            
            return(TRUE)
        },
        
        # Memory cleanup method
        .cleanup = function() {
            # Clear large data objects to free memory
            private$.trainingData <- NULL
            private$.validationData <- NULL
            
            # Clear temporary analysis results
            private$.crossValidationResults <- list()
            private$.bootstrapResults <- list()
            
            # Keep models and performance metrics for results display
            # but clear intermediate calculations
            if (length(private$.models) > 0) {
                for (model_name in names(private$.models)) {
                    model <- private$.models[[model_name]]
                    if (inherits(model, "glm")) {
                        # Remove heavy components while keeping essentials
                        model$data <- NULL
                        model$model <- NULL
                        model$x <- NULL
                        model$y <- NULL
                        model$fitted.values <- NULL
                        model$residuals <- NULL
                        model$weights <- NULL
                        model$prior.weights <- NULL
                        private$.models[[model_name]] <- model
                    }
                }
            }
            
            # Run garbage collection
            gc(verbose = FALSE)
        },
        
        # Memory-efficient data processing
        .processDataEfficiently = function(data) {
            # Process data in chunks for large datasets
            n_rows <- nrow(data)
            
            if (n_rows > 10000) {
                # For large datasets, suggest sampling for faster processing
                private$.warnings <- c(private$.warnings, 
                    paste0("Large dataset (", n_rows, " observations). Consider sampling for faster processing."))
            }
            
            # Remove unused factor levels to save memory
            factor_cols <- sapply(data, is.factor)
            if (any(factor_cols)) {
                data[factor_cols] <- lapply(data[factor_cols], droplevels)
            }
            
            return(data)
        },
        
        .performAllCrossValidation = function(models_built, modeling_data) {
            cv_results <- list()
            
            for (model_name in models_built) {
                private$.checkpoint() # Before each model's cross-validation
                
                model <- private$.models[[model_name]]
                
                # Create formula from model
                formula <- formula(model)
                
                # Perform cross-validation
                cv_result <- private$.performCrossValidation(formula, modeling_data, self$options$cvFolds)
                
                if (!is.null(cv_result)) {
                    cv_results[[model_name]] <- cv_result
                }
            }
            
            private$.crossValidationResults <- cv_results
            
            # Populate validation results table
            if (length(cv_results) > 0) {
                validation_table <- self$results$validationResults
                validation_table$deleteRows()
                
                for (model_name in names(cv_results)) {
                    cv_result <- cv_results[[model_name]]
                    cv_summary <- cv_result$summary
                    
                    validation_table$addRow(rowKey = model_name, values = list(
                        model = model_name,
                        cv_auc_mean = cv_summary$mean_auc %||% NA,
                        cv_auc_sd = cv_summary$sd_auc %||% NA,
                        cv_calibration_slope = cv_summary$mean_calibration_slope %||% NA,
                        cv_brier_score = cv_summary$mean_brier %||% NA,
                        optimism = cv_summary$optimism_auc %||% NA
                    ))
                }
            }
            
            return(cv_results)
        },
        
        .performBootstrapValidation = function(models_built, modeling_data) {
            # Bootstrap validation implementation
            bootstrap_results <- list()
            
            for (model_name in models_built) {
                model <- private$.models[[model_name]]
                
                # Simple bootstrap validation
                n_bootstrap <- self$options$bootstrapReps %||% 1000
                bootstrap_aucs <- numeric(n_bootstrap)
                
                for (i in 1:n_bootstrap) {
                    # Add checkpoint every 25 iterations for incremental progress
                    if (i %% 25 == 0) {
                        private$.checkpoint(flush = FALSE) # Poll for changes without pushing results
                    }
                    
                    # Bootstrap sample
                    boot_indices <- sample(nrow(modeling_data), replace = TRUE)
                    boot_data <- modeling_data[boot_indices, ]
                    
                    # Fit model on bootstrap sample
                    boot_model <- glm(formula(model), family = binomial, data = boot_data)
                    
                    # Predict on original data
                    boot_predictions <- predict(boot_model, newdata = modeling_data, type = "response")
                    
                    # Calculate AUC
                    outcome_var <- self$options$outcome
                    outcome_positive <- self$options$outcomePositive
                    binary_actual <- as.numeric(modeling_data[[outcome_var]] == outcome_positive)
                    
                    if (requireNamespace("pROC", quietly = TRUE)) {
                        roc_obj <- pROC::roc(binary_actual, boot_predictions, quiet = TRUE)
                        bootstrap_aucs[i] <- as.numeric(pROC::auc(roc_obj))
                    }
                }
                
                bootstrap_results[[model_name]] <- list(
                    bootstrap_aucs = bootstrap_aucs,
                    mean_auc = mean(bootstrap_aucs, na.rm = TRUE),
                    sd_auc = sd(bootstrap_aucs, na.rm = TRUE),
                    optimism = mean(bootstrap_aucs, na.rm = TRUE) - private$.performance[[model_name]]$auc
                )
            }
            
            private$.bootstrapResults <- bootstrap_results
            return(bootstrap_results)
        },
        
        .calculateAdvancedMetrics = function(models_built) {
            if (length(models_built) < 2) return(NULL)
            
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            actual <- private$.trainingData[[outcome_var]]
            binary_actual <- as.numeric(actual == outcome_positive)
            
            # Calculate NRI if requested
            if (self$options$calculateNRI && length(models_built) >= 2) {
                nri_table <- self$results$nriTable
                nri_table$deleteRows()
                
                # Parse NRI thresholds
                threshold_str <- self$options$nriThresholds %||% "0.05, 0.10, 0.20"
                thresholds <- as.numeric(unlist(strsplit(gsub("[[:space:]]", "", threshold_str), ",")))
                thresholds <- thresholds[!is.na(thresholds)]
                
                # Compare each model to basic model (or first model)
                reference_model <- private$.models[[models_built[1]]]
                ref_predictions <- predict(reference_model, type = "response")
                
                for (i in 2:length(models_built)) {
                    new_model <- private$.models[[models_built[i]]]
                    new_predictions <- predict(new_model, type = "response")
                    
                    # Simplified NRI calculation for each threshold
                    for (threshold in thresholds) {
                        # Classify predictions into risk categories
                        ref_high_risk <- ref_predictions >= threshold
                        new_high_risk <- new_predictions >= threshold
                        
                        # Calculate reclassification for events
                        events_mask <- binary_actual == 1
                        events_up <- sum(events_mask & !ref_high_risk & new_high_risk)
                        events_down <- sum(events_mask & ref_high_risk & !new_high_risk)
                        total_events <- sum(events_mask)
                        
                        nri_events <- (events_up - events_down) / total_events
                        
                        # Calculate reclassification for non-events
                        non_events_mask <- binary_actual == 0
                        non_events_up <- sum(non_events_mask & !ref_high_risk & new_high_risk)
                        non_events_down <- sum(non_events_mask & ref_high_risk & !new_high_risk)
                        total_non_events <- sum(non_events_mask)
                        
                        nri_non_events <- (non_events_down - non_events_up) / total_non_events
                        
                        # Overall NRI
                        nri_overall <- nri_events + nri_non_events
                        
                        # Add to table
                        comparison_name <- paste0(models_built[i], " vs ", models_built[1], " (", threshold, ")")
                        nri_table$addRow(rowKey = paste0(i, "_", threshold), values = list(
                            comparison = comparison_name,
                            nri_events = nri_events,
                            nri_non_events = nri_non_events,
                            nri_overall = nri_overall,
                            nri_p_value = NA  # Would need proper statistical test
                        ))
                    }
                }
            }
            
            # Calculate IDI if requested
            if (self$options$calculateIDI && length(models_built) >= 2) {
                idi_table <- self$results$idiTable
                idi_table$deleteRows()
                
                # Compare each model to basic model (or first model)
                reference_model <- private$.models[[models_built[1]]]
                ref_predictions <- predict(reference_model, type = "response")
                
                for (i in 2:length(models_built)) {
                    new_model <- private$.models[[models_built[i]]]
                    new_predictions <- predict(new_model, type = "response")
                    
                    # Calculate discrimination slopes
                    events_mask <- binary_actual == 1
                    non_events_mask <- binary_actual == 0
                    
                    # Discrimination slope = mean(predictions | events) - mean(predictions | non-events)
                    disc_slope_ref <- mean(ref_predictions[events_mask]) - mean(ref_predictions[non_events_mask])
                    disc_slope_new <- mean(new_predictions[events_mask]) - mean(new_predictions[non_events_mask])
                    
                    # IDI = difference in discrimination slopes
                    idi_value <- disc_slope_new - disc_slope_ref
                    
                    # Add to table
                    comparison_name <- paste0(models_built[i], " vs ", models_built[1])
                    idi_table$addRow(rowKey = i, values = list(
                        comparison = comparison_name,
                        idi = idi_value,
                        idi_p_value = NA,  # Would need proper statistical test
                        discrimination_slope_new = disc_slope_new,
                        discrimination_slope_old = disc_slope_ref
                    ))
                }
            }
            
            return(TRUE)
        },
        
        .createFinalSummary = function(models_built) {
            if (length(models_built) == 0) return()
            
            # Create performance summary
            performance_summary <- paste0(
                "<html><body>",
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>✅ Analysis Complete</h4>",
                "<p><strong>Models Built:</strong> ", length(models_built), "</p>",
                "<p><strong>Model Types:</strong> ", paste(models_built, collapse = ", "), "</p>",
                if (length(private$.warnings) > 0) {
                    paste0("<p><strong>Warnings:</strong> ", length(private$.warnings), " (check details above)</p>")
                } else "",
                if (!is.null(private$.performanceMetrics)) {
                    paste0("<p><strong>Analysis Time:</strong> ", 
                           round(sum(private$.performanceMetrics$execution_time), 2), " seconds</p>")
                } else "",
                "</div>",
                "</body></html>"
            )
            
            # Add to instructions or create new summary section
            current_content <- self$results$instructions$content
            if (is.null(current_content) || current_content == "") {
                self$results$instructions$setContent(performance_summary)
            }
        },

        .populateModelSummary = function(model, table_name) {
            summary_table <- self$results[[table_name]]
            summary_table$deleteRows()

            if (is.null(model)) return()

            # Handle penalized models differently
            if (inherits(model, "penalized_glm")) {
                # Extract coefficients from glmnet model
                coef_matrix <- coef(model$glmnet_fit, s = model$lambda)
                non_zero_coefs <- which(coef_matrix != 0)
                
                if (length(non_zero_coefs) > 0) {
                    for (i in seq_along(non_zero_coefs)) {
                        idx <- non_zero_coefs[i]
                        term_name <- rownames(coef_matrix)[idx]
                        estimate <- as.numeric(coef_matrix[idx])
                        
                        # Penalized models don't have standard errors in the usual sense
                        odds_ratio <- exp(estimate)
                        
                        summary_table$addRow(rowKey = i, values = list(
                            term = term_name,
                            estimate = estimate,
                            std_error = NA,  # Not available for penalized models
                            z_value = NA,    # Not available for penalized models
                            p_value = NA,    # Not available for penalized models
                            odds_ratio = odds_ratio,
                            ci_lower = NA,   # Would require bootstrap for penalized models
                            ci_upper = NA
                        ))
                    }
                }
            } else {
                # Regular GLM model
                tryCatch({
                    # Get model summary
                    model_summary <- summary(model)
                    coefficients <- model_summary$coefficients

                    # Calculate confidence intervals
                    conf_int <- confint(model, level = 0.95)

                    # Populate table
                    for (i in 1:nrow(coefficients)) {
                        term_name <- rownames(coefficients)[i]
                        estimate <- coefficients[i, "Estimate"]
                        std_error <- coefficients[i, "Std. Error"]
                        z_value <- coefficients[i, "z value"]
                        p_value <- coefficients[i, "Pr(>|z|)"]

                        # Calculate odds ratio and CI
                        odds_ratio <- exp(estimate)
                        ci_lower <- exp(conf_int[i, 1])
                        ci_upper <- exp(conf_int[i, 2])

                        summary_table$addRow(rowKey = i, values = list(
                            term = term_name,
                            estimate = estimate,
                            std_error = std_error,
                            z_value = z_value,
                            p_value = p_value,
                            odds_ratio = odds_ratio,
                            ci_lower = ci_lower,
                            ci_upper = ci_upper
                        ))
                    }
                }, error = function(e) {
                    warning(paste0("Failed to populate model summary: ", e$message))
                })
            }
        },

        .calculateAllPerformance = function(models_built) {
            if (!self$options$compareModels) return()

            comparison_table <- self$results$modelComparisonTable
            comparison_table$deleteRows()

            outcome_var <- self$options$outcome

            for (i in seq_along(models_built)) {
                private$.checkpoint() # Before each model's performance calculation
                
                model_name <- models_built[i]
                model <- private$.models[[model_name]]

                # Training performance
                if (inherits(model, "penalized_glm")) {
                    train_pred <- model$predict(model, type = "response")
                } else {
                    train_pred <- predict(model, type = "response")
                }
                train_actual <- private$.trainingData[[outcome_var]]
                train_perf <- private$.calculatePerformance(model, train_pred, train_actual, "training")

                # Validation performance (if available)
                if (!is.null(private$.validationData)) {
                    if (inherits(model, "penalized_glm")) {
                        val_pred <- model$predict(model, newdata = private$.validationData, type = "response")
                    } else {
                        val_pred <- predict(model, newdata = private$.validationData, type = "response")
                    }
                    val_actual <- private$.validationData[[outcome_var]]
                    val_perf <- private$.calculatePerformance(model, val_pred, val_actual, "validation")
                } else {
                    val_perf <- list(auc = NA, calibration_slope = NA, calibration_intercept = NA, brier_score = NA)
                }

                # Add to comparison table
                comparison_table$addRow(rowKey = i, values = list(
                    model = model_name,
                    n_predictors = train_perf$n_predictors,
                    auc_training = train_perf$auc,
                    auc_validation = val_perf$auc,
                    calibration_slope = train_perf$calibration_slope,
                    calibration_intercept = train_perf$calibration_intercept,
                    brier_score = train_perf$brier_score,
                    log_likelihood = train_perf$log_likelihood,
                    aic = train_perf$aic,
                    bic = train_perf$bic
                ))
            }
        },


        .addPredictionsToDataset = function() {
            if (!self$options$createPredictions) return()
            if (length(private$.models) == 0) return()

            # Add predicted probability columns to the original dataset
            for (model_name in names(private$.models)) {
                model <- private$.models[[model_name]]
                
                # Get model name for column
                model_col_name <- switch(model_name,
                    "basic" = self$options$basicModelName %||% "basic_model",
                    "enhanced" = self$options$enhancedModelName %||% "enhanced_model", 
                    "biomarker" = self$options$biomarkerModelName %||% "biomarker_model",
                    "custom" = self$options$customModelName %||% "custom_model",
                    model_name
                )
                
                # Create column name for predictions
                pred_col_name <- paste0(model_col_name, "_prob")
                
                # Generate predictions for the full dataset
                tryCatch({
                    if (inherits(model, "penalized_glm")) {
                        predictions <- model$predict(model, newdata = self$data, type = "response")
                    } else {
                        predictions <- predict(model, newdata = self$data, type = "response")
                    }
                    
                    # Store predictions for later use
                    private$.predictions[[model_name]] <- predictions
                    
                    # Add to dataset (conceptually - jamovi handles this differently)
                    # In practice, this would create new computed variables
                    private$.predictions[[pred_col_name]] <- predictions
                    
                }, error = function(e) {
                    warning(paste("Failed to generate predictions for", model_name, ":", e$message))
                })
            }
        },

        .prepareDCAOutput = function() {
            # Generate summary for DCA preparation
            model_names <- names(private$.models)

            if (length(model_names) > 0) {
                dca_message <- paste0(
                    "<html><body>",
                    "<h4>Ready for Decision Curve Analysis</h4>",
                    "<p>The following prediction models have been created and are ready for DCA:</p>",
                    "<ul>"
                )

                for (model_name in model_names) {
                    pred_col_name <- paste0(self$options[[paste0(model_name, "ModelName")]], "_prob")
                    dca_message <- paste0(dca_message,
                                          "<li><strong>", model_name, " Model</strong>: Use column '", pred_col_name, "'</li>")
                }

                dca_message <- paste0(
                    dca_message,
                    "</ul>",
                    "<p><strong>Next Steps:</strong></p>",
                    "<ol>",
                    "<li>Go to meddecide → Decision → Decision Curve Analysis</li>",
                    "<li>Set Outcome Variable: ", self$options$outcome, "</li>",
                    "<li>Set Positive Outcome: ", self$options$outcomePositive, "</li>",
                    "<li>Add the prediction model columns listed above</li>",
                    "<li>Configure threshold range and run DCA</li>",
                    "</ol>",
                    "</body></html>"
                )

                self$results$dcaReadyMessage$setContent(dca_message)
            }
        },

        .generateAllRiskScores = function(models_built) {
            # Generate clinical risk scores for each model
            if (!self$options$generateRiskScore) return()

            risk_score_table <- self$results$riskScoreTable
            risk_score_table$deleteRows()

            # For now, use the first built model
            if (length(models_built) > 0) {
                first_model <- private$.models[[models_built[1]]]
                score_data <- private$.generateClinicalRiskScore(first_model, models_built[1])

                if (!is.null(score_data)) {
                    for (i in 1:nrow(score_data)) {
                        risk_score_table$addRow(rowKey = i, values = list(
                            variable = score_data$variable[i],
                            category = score_data$category[i],
                            points = score_data$points[i],
                            coefficient = score_data$coefficient[i],
                            interpretation = score_data$interpretation[i]
                        ))
                    }
                }
            }
        },
        
        .initializeCalibrationPlotsArray = function(models_built) {
            # Initialize the calibration plots array with appropriate keys
            calib_array <- self$results$calibrationPlotsArray
            
            for (model_name in models_built) {
                key_name <- paste0("calibration_", model_name)
                calib_array$addItem(key = key_name)
            }
        },

        # Plotting functions
        .plotROCCurves = function(image, ggtheme, theme, ...) {
            if (length(private$.models) == 0) return(FALSE)
            
            if (!requireNamespace("ggplot2", quietly = TRUE) || 
                !requireNamespace("pROC", quietly = TRUE)) {
                return(FALSE)
            }
            
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            
            # Prepare ROC data
            roc_data <- data.frame()
            
            for (model_name in names(private$.models)) {
                model <- private$.models[[model_name]]
                
                # Get predictions from training data
                predictions <- predict(model, type = "response")
                actual <- private$.trainingData[[outcome_var]]
                binary_actual <- as.numeric(actual == outcome_positive)
                
                # Calculate ROC
                tryCatch({
                    roc_obj <- pROC::roc(binary_actual, predictions, quiet = TRUE)
                    coords <- pROC::coords(roc_obj, "all", ret = c("sensitivity", "specificity"))
                    
                    model_data <- data.frame(
                        sensitivity = coords$sensitivity,
                        specificity = coords$specificity,
                        fpr = 1 - coords$specificity,
                        model = model_name,
                        auc = as.numeric(pROC::auc(roc_obj)),
                        stringsAsFactors = FALSE
                    )
                    
                    roc_data <- rbind(roc_data, model_data)
                }, error = function(e) {
                    # Skip if ROC calculation fails
                })
            }
            
            if (nrow(roc_data) == 0) return(FALSE)
            
            # Create ROC plot
            p <- ggplot2::ggplot(roc_data, ggplot2::aes(x = fpr, y = sensitivity, color = model)) +
                ggplot2::geom_line(size = 1.2) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
                ggplot2::labs(
                    x = "False Positive Rate (1 - Specificity)",
                    y = "True Positive Rate (Sensitivity)",
                    title = "ROC Curves Comparison",
                    color = "Model"
                ) +
                ggplot2::scale_x_continuous(limits = c(0, 1)) +
                ggplot2::scale_y_continuous(limits = c(0, 1)) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    legend.position = "bottom",
                    plot.title = ggplot2::element_text(hjust = 0.5)
                )
            
            # Add AUC values to legend
            unique_models <- unique(roc_data$model)
            auc_labels <- sapply(unique_models, function(m) {
                auc_val <- round(unique(roc_data$auc[roc_data$model == m]), 3)
                paste0(m, " (AUC: ", auc_val, ")")
            })
            
            p <- p + ggplot2::scale_color_discrete(name = "Model", labels = auc_labels)
            
            print(p)
            return(TRUE)
        },

        .plotCalibration = function(image, ggtheme, theme, ...) {
            plot_key <- image$name
            model_name <- gsub("calibration_", "", plot_key)
            
            if (!model_name %in% names(private$.models)) return(FALSE)
            
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(FALSE)
            }
            
            model <- private$.models[[model_name]]
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive
            
            # Get predictions
            predictions <- predict(model, type = "response")
            actual <- private$.trainingData[[outcome_var]]
            binary_actual <- as.numeric(actual == outcome_positive)
            
            # Create calibration bins
            n_bins <- 10
            bin_breaks <- seq(0, 1, length.out = n_bins + 1)
            bin_indices <- cut(predictions, breaks = bin_breaks, include.lowest = TRUE)
            
            # Calculate observed vs predicted for each bin
            calib_data <- data.frame()
            
            for (i in 1:n_bins) {
                bin_mask <- bin_indices == levels(bin_indices)[i]
                if (sum(bin_mask) == 0) next
                
                bin_pred_mean <- mean(predictions[bin_mask])
                bin_obs_mean <- mean(binary_actual[bin_mask])
                bin_count <- sum(bin_mask)
                
                calib_data <- rbind(calib_data, data.frame(
                    predicted = bin_pred_mean,
                    observed = bin_obs_mean,
                    count = bin_count,
                    bin = i
                ))
            }
            
            if (nrow(calib_data) == 0) return(FALSE)
            
            # Create calibration plot
            p <- ggplot2::ggplot(calib_data, ggplot2::aes(x = predicted, y = observed)) +
                ggplot2::geom_point(ggplot2::aes(size = count), alpha = 0.7) +
                ggplot2::geom_smooth(method = "loess", se = TRUE, color = "blue") +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
                ggplot2::labs(
                    x = "Predicted Probability",
                    y = "Observed Frequency",
                    title = paste0("Calibration Plot: ", model_name),
                    size = "Bin Size"
                ) +
                ggplot2::scale_x_continuous(limits = c(0, 1)) +
                ggplot2::scale_y_continuous(limits = c(0, 1)) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5)
                )
            
            print(p)
            return(TRUE)
        },

        .plotModelComparison = function(image, ggtheme, theme, ...) {
            if (length(private$.models) == 0) return(FALSE)
            
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(FALSE)
            }
            
            # Prepare comparison data
            comp_data <- data.frame()
            
            for (model_name in names(private$.models)) {
                model <- private$.models[[model_name]]
                
                # Calculate basic metrics
                predictions <- predict(model, type = "response")
                outcome_var <- self$options$outcome
                outcome_positive <- self$options$outcomePositive
                actual <- private$.trainingData[[outcome_var]]
                binary_actual <- as.numeric(actual == outcome_positive)
                
                # Calculate AUC
                auc_val <- NA
                if (requireNamespace("pROC", quietly = TRUE)) {
                    tryCatch({
                        roc_obj <- pROC::roc(binary_actual, predictions, quiet = TRUE)
                        auc_val <- as.numeric(pROC::auc(roc_obj))
                    }, error = function(e) auc_val <- NA)
                }
                
                # Calculate Brier score
                brier_score <- mean((predictions - binary_actual)^2)
                
                comp_data <- rbind(comp_data, data.frame(
                    model = model_name,
                    auc = auc_val,
                    brier_score = brier_score,
                    aic = AIC(model),
                    n_predictors = length(coef(model)) - 1,
                    stringsAsFactors = FALSE
                ))
            }
            
            if (nrow(comp_data) == 0) return(FALSE)
            
            # Create comparison plot
            p <- ggplot2::ggplot(comp_data, ggplot2::aes(x = model, y = auc, fill = model)) +
                ggplot2::geom_col(alpha = 0.7) +
                ggplot2::geom_text(ggplot2::aes(label = round(auc, 3)), vjust = -0.5) +
                ggplot2::labs(
                    x = "Model",
                    y = "AUC",
                    title = "Model Performance Comparison",
                    fill = "Model"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    plot.title = ggplot2::element_text(hjust = 0.5),
                    legend.position = "none"
                )
            
            print(p)
            return(TRUE)
        },

        .plotValidation = function(image, ggtheme, theme, ...) {
            if (length(private$.crossValidationResults) == 0 && length(private$.bootstrapResults) == 0) {
                return(FALSE)
            }
            
            if (!requireNamespace("ggplot2", quietly = TRUE)) {
                return(FALSE)
            }
            
            # Prepare validation data
            val_data <- data.frame()
            
            # Cross-validation results
            if (length(private$.crossValidationResults) > 0) {
                for (model_name in names(private$.crossValidationResults)) {
                    cv_result <- private$.crossValidationResults[[model_name]]
                    if (!is.null(cv_result$cv_aucs)) {
                        model_data <- data.frame(
                            model = model_name,
                            auc = cv_result$cv_aucs,
                            type = "Cross-Validation",
                            stringsAsFactors = FALSE
                        )
                        val_data <- rbind(val_data, model_data)
                    }
                }
            }
            
            # Bootstrap results
            if (length(private$.bootstrapResults) > 0) {
                for (model_name in names(private$.bootstrapResults)) {
                    boot_result <- private$.bootstrapResults[[model_name]]
                    if (!is.null(boot_result$bootstrap_aucs)) {
                        # Sample subset for plotting (too many points can be slow)
                        sample_size <- min(100, length(boot_result$bootstrap_aucs))
                        sampled_aucs <- sample(boot_result$bootstrap_aucs, sample_size)
                        
                        model_data <- data.frame(
                            model = model_name,
                            auc = sampled_aucs,
                            type = "Bootstrap",
                            stringsAsFactors = FALSE
                        )
                        val_data <- rbind(val_data, model_data)
                    }
                }
            }
            
            if (nrow(val_data) == 0) return(FALSE)
            
            # Create validation plot
            p <- ggplot2::ggplot(val_data, ggplot2::aes(x = model, y = auc, fill = type)) +
                ggplot2::geom_boxplot(alpha = 0.7) +
                ggplot2::labs(
                    x = "Model",
                    y = "AUC",
                    title = "Validation Results",
                    fill = "Validation Type"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                    plot.title = ggplot2::element_text(hjust = 0.5)
                )
            
            print(p)
            return(TRUE)
        },
        
        # Clinical Guidance Panel
        .addClinicalGuidance = function() {
            guidance <- paste0(
                "<html><body>",
                "<div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #4caf50;'>",
                "<h4 style='color: #2e7d32; margin-top: 0;'>🩺 ", "Clinical Best Practices", "</h4>",
                "<h5 style='color: #388e3c;'>", "Sample Size Guidelines:", "</h5>",
                "<ul>",
                "<li>", "Minimum 10 events per predictor variable (EPV rule)", "</li>",
                "<li>", "At least 50 total observations for stable estimates", "</li>",
                "<li>", "Consider penalized regression for high-dimensional data", "</li>",
                "</ul>",
                "<h5 style='color: #388e3c;'>", "Model Selection Tips:", "</h5>",
                "<ul>",
                "<li>", "Start with basic clinical model (age, sex, primary risk factors)", "</li>",
                "<li>", "Add biomarkers only if they improve prediction meaningfully", "</li>",
                "<li>", "Use validation to avoid overfitting", "</li>",
                "</ul>",
                "<h5 style='color: #388e3c;'>", "Interpretation Guide:", "</h5>",
                "<ul>",
                "<li>", "AUC > 0.8: Excellent discrimination", "</li>",
                "<li>", "AUC 0.7-0.8: Good discrimination", "</li>",
                "<li>", "AUC 0.6-0.7: Fair discrimination", "</li>",
                "<li>", "AUC < 0.6: Poor discrimination", "</li>",
                "</ul>",
                "</div></body></html>"
            )
            self$results$clinicalGuidance$setContent(guidance)
        },
        
        # Statistical Glossary
        .addGlossary = function() {
            glossary <- paste0(
                "<html><body>",
                "<div style='background-color: #f3e5f5; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #9c27b0;'>",
                "<h4 style='color: #7b1fa2; margin-top: 0;'>📚 ", "Statistical Terms for Clinicians", "</h4>",
                "<dl style='margin: 0;'>",
                "<dt style='font-weight: bold; color: #7b1fa2; margin-top: 10px;'>", "AUC (Area Under Curve)", "</dt>",
                "<dd style='margin-left: 20px;'>", "Probability that model ranks a random patient with outcome higher than a random patient without outcome. Range: 0.5 (no better than chance) to 1.0 (perfect).", "</dd>",
                "<dt style='font-weight: bold; color: #7b1fa2; margin-top: 10px;'>", "Odds Ratio (OR)", "</dt>",
                "<dd style='margin-left: 20px;'>", "How much the odds of outcome increase with predictor. OR=2 means odds are 2x higher. OR=0.5 means odds are halved.", "</dd>",
                "<dt style='font-weight: bold; color: #7b1fa2; margin-top: 10px;'>", "Calibration", "</dt>",
                "<dd style='margin-left: 20px;'>", "How well predicted probabilities match observed event rates. Perfect calibration: if model predicts 30% risk, 30% of patients should have the outcome.", "</dd>",
                "<dt style='font-weight: bold; color: #7b1fa2; margin-top: 10px;'>", "Cross-Validation", "</dt>",
                "<dd style='margin-left: 20px;'>", "Testing model on data not used for training. Helps detect overfitting and estimate real-world performance.", "</dd>",
                "<dt style='font-weight: bold; color: #7b1fa2; margin-top: 10px;'>", "NRI (Net Reclassification Index)", "</dt>",
                "<dd style='margin-left: 20px;'>", "Measures how much new model improves patient classification into risk categories compared to old model.", "</dd>",
                "</dl>",
                "</div></body></html>"
            )
            self$results$glossary$setContent(glossary)
        },
        
        # Generate Clinical Summary
        .generateClinicalSummary = function() {
            if (length(private$.models) == 0) return()
            
            summary_parts <- c()
            
            # Add model performance summaries
            for (model_name in names(private$.models)) {
                model_performance <- private$.performance[[model_name]]
                if (!is.null(model_performance)) {
                    auc <- model_performance$auc
                    auc_ci <- model_performance$auc_ci
                    
                    # Clinical interpretation of AUC
                    discrimination <- if (auc >= 0.8) "excellent" else 
                                   if (auc >= 0.7) "good" else 
                                   if (auc >= 0.6) "fair" else "poor"
                    
                    summary_text <- paste0(
                        "<p><strong>", model_name, ":</strong> ",
                        "Shows ", discrimination, " discrimination (AUC = ", round(auc, 3), 
                        " 95% CI ", round(auc_ci[1], 3), "-", round(auc_ci[2], 3), "). ",
                        "This means the model correctly ranks ", round(auc * 100, 1), 
                        "% of patient pairs.",
                        "</p>"
                    )
                    summary_parts <- c(summary_parts, summary_text)
                }
            }
            
            if (length(summary_parts) > 0) {
                clinical_summary <- paste0(
                    "<html><body>",
                    "<div style='background-color: #e3f2fd; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #2196f3;'>",
                    "<h4 style='color: #1976d2; margin-top: 0;'>📋 ", "Clinical Interpretation", "</h4>",
                    paste(summary_parts, collapse = ""),
                    "<hr style='margin: 15px 0; border: none; border-top: 1px solid #ccc;'>",
                    "<p><strong>", "Clinical Recommendation:", "</strong> ",
                    "Use the model with highest AUC and best clinical interpretability. ",
                    "Consider external validation before clinical implementation.",
                    "</p>",
                    "</div></body></html>"
                )
                self$results$clinicalSummary$setContent(clinical_summary)
            }
        },
        
        # Generate copy-ready report sentences
        .generateReportSentences = function() {
            if (length(private$.models) == 0) return()
            
            report_parts <- c()
            
            # Generate sentences for each model
            for (model_name in names(private$.models)) {
                model_performance <- private$.performance[[model_name]]
                if (!is.null(model_performance)) {
                    auc <- round(model_performance$auc, 3)
                    auc_ci <- model_performance$auc_ci
                    
                    discrimination <- if (auc >= 0.8) "excellent" else 
                                   if (auc >= 0.7) "good" else 
                                   if (auc >= 0.6) "fair" else "poor"
                    
                    sentence <- paste0(
                        "The ", model_name, " demonstrated ", discrimination,
                        " discriminative ability with an area under the ROC curve (AUC) of ",
                        auc, " (95% CI: ", round(auc_ci[1], 3), "-", round(auc_ci[2], 3), ")."
                    )
                    
                    report_parts <- c(report_parts, paste0(
                        "<div style='background-color: #f8f9fa; padding: 10px; margin: 5px 0; border-radius: 4px; font-family: serif;'>",
                        sentence, "<br>",
                        "<button onclick='navigator.clipboard.writeText(\"" , sentence, "\")' ",
                        "style='margin-top: 5px; padding: 2px 8px; font-size: 11px; background-color: #007bff; color: white; border: none; border-radius: 3px; cursor: pointer;'>",
                        "Copy", "</button>",
                        "</div>"
                    ))
                }
            }
            
            if (length(report_parts) > 0) {
                report_html <- paste0(
                    "<html><body>",
                    "<div style='padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #6c757d;'>",
                    "<h4 style='color: #495057; margin-top: 0;'>📝 ", "Copy-Ready Report Sentences", "</h4>",
                    "<p style='font-style: italic; color: #6c757d; font-size: 14px;'>", 
                    "Copy and paste these sentences into your manuscript:", "</p>",
                    paste(report_parts, collapse = ""),
                    "</div></body></html>"
                )
                self$results$reportSentences$setContent(report_html)
            }
            
            # Generate export options
            self$.generateExportOptions()
        },
        
        # Generate export options and capabilities
        .generateExportOptions = function() {
            if (length(private$.models) == 0) return()
            
            export_html <- paste0(
                "<html><body>",
                "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #6c757d;'>",
                "<h4 style='color: #495057; margin-top: 0;'>💾 ", "Export & Download Options", "</h4>",
                "<p style='color: #6c757d; font-size: 14px;'>", 
                "Download model results and visualizations for reports and presentations:", "</p>",
                
                "<div style='display: flex; flex-wrap: wrap; gap: 10px; margin: 15px 0;'>",
                "<button onclick='exportModelSummary()' ",
                "style='padding: 8px 16px; background-color: #28a745; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 14px;'>",
                "📄 ", "Export Model Summary (CSV)", "</button>",
                
                "<button onclick='exportPerformanceMetrics()' ",
                "style='padding: 8px 16px; background-color: #007bff; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 14px;'>",
                "📉 ", "Export Performance (CSV)", "</button>",
                
                "<button onclick='exportROCData()' ",
                "style='padding: 8px 16px; background-color: #fd7e14; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 14px;'>",
                "📊 ", "Export ROC Data (CSV)", "</button>",
                
                "<button onclick='exportPredictions()' ",
                "style='padding: 8px 16px; background-color: #6f42c1; color: white; border: none; border-radius: 4px; cursor: pointer; font-size: 14px;'>",
                "📅 ", "Export Predictions (CSV)", "</button>",
                "</div>",
                
                "<script>",
                "function exportModelSummary() { alert('Model summary export functionality would be implemented here'); }",
                "function exportPerformanceMetrics() { alert('Performance metrics export functionality would be implemented here'); }",
                "function exportROCData() { alert('ROC data export functionality would be implemented here'); }",
                "function exportPredictions() { alert('Predictions export functionality would be implemented here'); }",
                "</script>",
                
                "<hr style='margin: 15px 0; border: none; border-top: 1px solid #dee2e6;'>",
                "<h5 style='color: #495057;'>", "Publication-Ready Formats:", "</h5>",
                "<ul style='font-size: 14px; color: #6c757d;'>",
                "<li>", "Model coefficients and statistics (CSV/Excel format)", "</li>",
                "<li>", "Performance metrics with confidence intervals", "</li>",
                "<li>", "ROC curve coordinates for plotting software", "</li>",
                "<li>", "Predicted probabilities for external validation", "</li>",
                "</ul>",
                "</div></body></html>"
            )
            
            self$results$exportOptions$setContent(export_html)
        },
        
        # Create exportable data formats
        .createExportData = function() {
            export_data <- list()
            
            # Model summary data
            if (length(private$.models) > 0) {
                model_summaries <- list()
                for (model_name in names(private$.models)) {
                    model <- private$.models[[model_name]]
                    if (inherits(model, "glm")) {
                        summary_df <- data.frame(
                            model = model_name,
                            term = names(coef(model)),
                            coefficient = coef(model),
                            se = summary(model)$coefficients[, "Std. Error"],
                            z_value = summary(model)$coefficients[, "z value"],
                            p_value = summary(model)$coefficients[, "Pr(>|z|)"],
                            stringsAsFactors = FALSE
                        )
                        model_summaries[[model_name]] <- summary_df
                    }
                }
                export_data$model_summaries <- do.call(rbind, model_summaries)
            }
            
            # Performance metrics data
            if (length(private$.performance) > 0) {
                perf_list <- list()
                for (model_name in names(private$.performance)) {
                    perf <- private$.performance[[model_name]]
                    perf_df <- data.frame(
                        model = model_name,
                        auc = perf$auc,
                        auc_ci_lower = perf$auc_ci[1],
                        auc_ci_upper = perf$auc_ci[2],
                        stringsAsFactors = FALSE
                    )
                    perf_list[[model_name]] <- perf_df
                }
                export_data$performance <- do.call(rbind, perf_list)
            }
            
            return(export_data)
        },
        
        # Apply clinical preset configurations
        .applyClinicalPreset = function() {
            preset <- self$options$clinicalPreset
            
            if (preset == "none") return()
            
            preset_guidance <- ""
            
            if (preset == "cardiac_risk") {
                preset_guidance <- paste0(
                    "<div style='background-color: #e8f4fd; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #0ea5e9;'>",
                    "<h4 style='color: #0369a1; margin-top: 0;'>💗 ", "Cardiac Risk Assessment Preset", "</h4>",
                    "<h5>", "Recommended Variables:", "</h5>",
                    "<ul>",
                    "<li><strong>", "Basic Clinical:", "</strong> ", "Age, Sex, Smoking, Diabetes, Hypertension, Family History", "</li>",
                    "<li><strong>", "Enhanced Clinical:", "</strong> ", "BMI, Exercise, Diet, Medication History", "</li>",
                    "<li><strong>", "Biomarkers:", "</strong> ", "Cholesterol, Troponin, CRP, BNP", "</li>",
                    "</ul>",
                    "<h5>", "Optimized Settings:", "</h5>",
                    "<ul>",
                    "<li>", "Cross-validation enabled for robust estimates", "</li>",
                    "<li>", "Data splitting for unbiased validation", "</li>",
                    "<li>", "Performance metrics focused on discrimination and calibration", "</li>",
                    "</ul>",
                    "</div>"
                )
            } else if (preset == "cancer_prognosis") {
                preset_guidance <- paste0(
                    "<div style='background-color: #fef3e2; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #f59e0b;'>",
                    "<h4 style='color: #d97706; margin-top: 0;'>🎗️ ", "Cancer Prognosis Preset", "</h4>",
                    "<h5>", "Recommended Variables:", "</h5>",
                    "<ul>",
                    "<li><strong>", "Basic Clinical:", "</strong> ", "Age, Sex, Stage, Grade, Histology", "</li>",
                    "<li><strong>", "Enhanced Clinical:", "</strong> ", "Performance Status, Comorbidities, Treatment Response", "</li>",
                    "<li><strong>", "Biomarkers:", "</strong> ", "Molecular Markers, IHC Scores, Gene Expression", "</li>",
                    "</ul>",
                    "<h5>", "Optimized Settings:", "</h5>",
                    "<ul>",
                    "<li>", "Bootstrap validation for time-to-event endpoints", "</li>",
                    "<li>", "Penalized regression for high-dimensional data", "</li>",
                    "<li>", "Multiple imputation for missing pathology data", "</li>",
                    "</ul>",
                    "</div>"
                )
            } else if (preset == "biomarker_validation") {
                preset_guidance <- paste0(
                    "<div style='background-color: #f0fdf4; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #22c55e;'>",
                    "<h4 style='color: #15803d; margin-top: 0;'>🧬 ", "Biomarker Validation Preset", "</h4>",
                    "<h5>", "Recommended Variables:", "</h5>",
                    "<ul>",
                    "<li><strong>", "Basic Clinical:", "</strong> ", "Age, Sex, Disease Duration", "</li>",
                    "<li><strong>", "Biomarker Model:", "</strong> ", "New Biomarker(s) + Clinical Variables", "</li>",
                    "</ul>",
                    "<h5>", "Optimized Settings:", "</h5>",
                    "<ul>",
                    "<li>", "Model comparison enabled (clinical vs biomarker-enhanced)", "</li>",
                    "<li>", "NRI and IDI calculation for added value assessment", "</li>",
                    "<li>", "ROC curve comparison for discrimination improvement", "</li>",
                    "</ul>",
                    "</div>"
                )
            } else if (preset == "diagnostic_test") {
                preset_guidance <- paste0(
                    "<div style='background-color: #fef2f2; padding: 15px; border-radius: 8px; margin: 10px 0; border-left: 4px solid #ef4444;'>",
                    "<h4 style='color: #dc2626; margin-top: 0;'>🔍 ", "Diagnostic Test Preset", "</h4>",
                    "<h5>", "Recommended Variables:", "</h5>",
                    "<ul>",
                    "<li><strong>", "Test Results:", "</strong> ", "Primary Test Result, Reference Standard", "</li>",
                    "<li><strong>", "Clinical Context:", "</strong> ", "Age, Sex, Symptoms, Risk Factors", "</li>",
                    "</ul>",
                    "<h5>", "Optimized Settings:", "</h5>",
                    "<ul>",
                    "<li>", "Focus on calibration for probability interpretation", "</li>",
                    "<li>", "ROC analysis with optimal threshold identification", "</li>",
                    "<li>", "Decision curve analysis integration for clinical utility", "</li>",
                    "</ul>",
                    "</div>"
                )
            }
            
            if (preset_guidance != "") {
                # Add preset guidance to clinical guidance panel
                current_guidance <- self$results$clinicalGuidance$state
                if (!is.null(current_guidance)) {
                    combined_guidance <- paste0(current_guidance, preset_guidance)
                    self$results$clinicalGuidance$setContent(combined_guidance)
                }
            }
        }
    )
)
