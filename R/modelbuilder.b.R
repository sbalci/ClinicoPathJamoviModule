#' @title Prediction Model Builder
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats glm predict confint coef logLik AIC BIC step
#' @importFrom stats na.omit qlogis plogis formula rbinom
#' @importFrom utils capture.output
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_smooth geom_abline
#' @importFrom ggplot2 labs theme_minimal scale_color_brewer facet_wrap
#' @importFrom dplyr mutate select filter summarise group_by arrange
#' @importFrom pROC roc auc ci.auc coords

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

        # Split data into training and validation sets
        .splitData = function(data, split_ratio = 0.7) {
            set.seed(self$options$randomSeed)
            n <- nrow(data)
            train_idx <- sample(n, floor(n * split_ratio))

            training_data <- data[train_idx, ]
            validation_data <- data[-train_idx, ]

            return(list(
                training = training_data,
                validation = validation_data
            ))
        },

        # Handle missing data
        .handleMissingData = function(data) {
            method <- self$options$missingDataMethod

            if (method == "complete_cases") {
                return(na.omit(data))
            } else if (method == "mean_imputation") {
                # Simple mean/mode imputation
                for (col in names(data)) {
                    if (is.numeric(data[[col]])) {
                        data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
                    } else if (is.factor(data[[col]])) {
                        mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                        data[[col]][is.na(data[[col]])] <- mode_val
                    }
                }
                return(data)
            } else if (method == "exclude_missing") {
                # Remove variables with >20% missing
                missing_prop <- sapply(data, function(x) sum(is.na(x)) / length(x))
                keep_vars <- names(missing_prop)[missing_prop <= 0.20]
                return(data[, keep_vars, drop = FALSE])
            }

            return(data)
        },

        # Build a single logistic regression model
        .buildLogisticModel = function(formula, data, model_name) {
            tryCatch({
                # Fit model
                model <- glm(formula, family = binomial, data = data)

                # Store model
                private$.models[[model_name]] <- model

                # Generate predictions for training data
                train_pred <- predict(model, type = "response")
                private$.predictions[[paste0(model_name, "_train")]] <- train_pred

                # Generate predictions for validation data (if available)
                if (!is.null(private$.validationData)) {
                    val_pred <- predict(model, newdata = private$.validationData, type = "response")
                    private$.predictions[[paste0(model_name, "_val")]] <- val_pred
                }

                return(model)

            }, error = function(e) {
                stop(paste("Failed to build", model_name, ":", e$message))
            })
        },

        # Calculate model performance metrics
        .calculatePerformance = function(model, predictions, actual, dataset_type = "training") {
            outcome_positive <- self$options$outcomePositive
            binary_actual <- as.numeric(actual == outcome_positive)

            # AUC
            roc_obj <- pROC::roc(binary_actual, predictions, quiet = TRUE)
            auc_value <- as.numeric(pROC::auc(roc_obj))

            # Calibration metrics
            calibration_model <- glm(binary_actual ~ qlogis(pmax(0.001, pmin(0.999, predictions))),
                                     family = binomial)
            cal_slope <- coef(calibration_model)[2]
            cal_intercept <- coef(calibration_model)[1]

            # Brier score
            brier_score <- mean((predictions - binary_actual)^2)

            # Model fit statistics
            log_likelihood <- logLik(model)[1]
            aic_value <- AIC(model)
            bic_value <- BIC(model)

            return(list(
                auc = auc_value,
                calibration_slope = cal_slope,
                calibration_intercept = cal_intercept,
                brier_score = brier_score,
                log_likelihood = log_likelihood,
                aic = aic_value,
                bic = bic_value,
                n_predictors = length(coef(model)) - 1  # Excluding intercept
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

        # Perform cross-validation
        .performCrossValidation = function(formula, data, k_folds = 5) {
            if (!self$options$crossValidation) return(NULL)

            set.seed(self$options$randomSeed)
            n <- nrow(data)
            fold_indices <- sample(rep(1:k_folds, length.out = n))

            cv_results <- data.frame(
                fold = 1:k_folds,
                auc = numeric(k_folds),
                brier_score = numeric(k_folds),
                calibration_slope = numeric(k_folds)
            )

            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive

            for (fold in 1:k_folds) {
                # Split data
                train_data <- data[fold_indices != fold, ]
                test_data <- data[fold_indices == fold, ]

                # Fit model
                fold_model <- glm(formula, family = binomial, data = train_data)

                # Predict on test set
                predictions <- predict(fold_model, newdata = test_data, type = "response")
                actual <- test_data[[outcome_var]]
                binary_actual <- as.numeric(actual == outcome_positive)

                # Calculate metrics
                if (length(unique(binary_actual)) > 1) {
                    roc_obj <- pROC::roc(binary_actual, predictions, quiet = TRUE)
                    cv_results$auc[fold] <- as.numeric(pROC::auc(roc_obj))
                    cv_results$brier_score[fold] <- mean((predictions - binary_actual)^2)

                    # Calibration slope
                    cal_model <- tryCatch({
                        glm(binary_actual ~ qlogis(pmax(0.001, pmin(0.999, predictions))), family = binomial)
                    }, error = function(e) NULL)

                    if (!is.null(cal_model)) {
                        cv_results$calibration_slope[fold] <- coef(cal_model)[2]
                    }
                }
            }

            return(cv_results)
        },

        # Main analysis function
        .run = function() {

            # Show instructions if needed
            if (is.null(self$options$outcome) ||
                (!self$options$buildBasicModel && !self$options$buildEnhancedModel &&
                 !self$options$buildBiomarkerModel && !self$options$buildCustomModel)) {

                instructions <- "
                <html>
                <head></head>
                <body>
                <div class='instructions'>
                <p><b>Prediction Model Builder</b></p>
                <p>Build and validate prediction models for medical decision making. This module creates logistic regression models that output predicted probabilities for use in Decision Curve Analysis.</p>
                <p>To get started:</p>
                <ol>
                <li>Select a binary <b>Outcome Variable</b> to predict</li>
                <li>Specify which level represents the positive outcome</li>
                <li>Choose at least one model to build:</li>
                <ul>
                <li><b>Basic Clinical Model</b>: Core demographic and risk factors</li>
                <li><b>Enhanced Clinical Model</b>: Additional clinical variables</li>
                <li><b>Biomarker Model</b>: Laboratory values and biomarkers</li>
                <li><b>Custom Model</b>: User-defined variable set</li>
                </ul>
                <li>Configure validation and output options</li>
                </ol>
                <p>The module will create predicted probability columns that can be directly used in Decision Curve Analysis.</p>
                </div>
                </body>
                </html>
                "

                self$results$instructions$setContent(instructions)
                return()
            }

            # Hide instructions when analysis can proceed
            self$results$instructions$setVisible(FALSE)

            # Get data and basic setup
            data <- self$data
            outcome_var <- self$options$outcome
            outcome_positive <- self$options$outcomePositive

            # Validate outcome variable
            if (length(unique(data[[outcome_var]])) != 2) {
                stop("Outcome variable must be binary (exactly 2 levels)")
            }

            # Handle missing data
            data <- private$.handleMissingData(data)

            # Split data if requested
            if (self$options$splitData) {
                split_result <- private$.splitData(data)
                private$.trainingData <- split_result$training
                private$.validationData <- split_result$validation
                modeling_data <- private$.trainingData
            } else {
                private$.trainingData <- data
                private$.validationData <- NULL
                modeling_data <- data
            }

            # Create data summary
            n_total <- nrow(data)
            n_training <- nrow(modeling_data)
            n_validation <- if (!is.null(private$.validationData)) nrow(private$.validationData) else 0
            event_rate <- mean(data[[outcome_var]] == outcome_positive) * 100

            data_summary <- paste0(
                "<html><body>",
                "<h4>Data Summary</h4>",
                "<p><strong>Total Sample Size:</strong> ", n_total, "</p>",
                "<p><strong>Training Set:</strong> ", n_training, " (", round(n_training/n_total*100, 1), "%)</p>",
                if (n_validation > 0) paste0("<p><strong>Validation Set:</strong> ", n_validation, " (", round(n_validation/n_total*100, 1), "%)</p>") else "",
                "<p><strong>Event Rate:</strong> ", round(event_rate, 1), "% (", outcome_positive, ")</p>",
                "<p><strong>Events in Training:</strong> ", sum(modeling_data[[outcome_var]] == outcome_positive), "</p>",
                "</body></html>"
            )

            self$results$dataSummary$setContent(data_summary)

            # Build models
            models_built <- c()

            # Basic Clinical Model
            if (self$options$buildBasicModel && length(self$options$basicPredictors) > 0) {
                basic_predictors <- self$options$basicPredictors

                # Apply transformations and interactions
                modeling_data <- private$.transformVariables(modeling_data, basic_predictors)
                modeling_data <- private$.createInteractions(modeling_data, basic_predictors)

                # Build formula
                predictor_string <- paste(basic_predictors, collapse = " + ")
                formula_basic <- as.formula(paste(outcome_var, "~", predictor_string))

                # Build model
                basic_model <- private$.buildLogisticModel(formula_basic, modeling_data, "basic")

                # Apply stepwise selection if requested
                basic_model <- private$.applyStepwiseSelection(basic_model,
                                                               self$options$stepwiseDirection,
                                                               self$options$selectionCriterion)
                private$.models[["basic"]] <- basic_model

                models_built <- c(models_built, "basic")

                # Populate model summary table
                if (self$options$showModelSummary) {
                    private$.populateModelSummary(basic_model, "basicModelSummary")
                }
            }

            # Enhanced Clinical Model
            if (self$options$buildEnhancedModel && length(self$options$enhancedPredictors) > 0) {
                enhanced_predictors <- self$options$enhancedPredictors

                # Apply transformations and interactions
                modeling_data <- private$.transformVariables(modeling_data, enhanced_predictors)
                modeling_data <- private$.createInteractions(modeling_data, enhanced_predictors)

                # Build formula
                predictor_string <- paste(enhanced_predictors, collapse = " + ")
                formula_enhanced <- as.formula(paste(outcome_var, "~", predictor_string))

                # Build model
                enhanced_model <- private$.buildLogisticModel(formula_enhanced, modeling_data, "enhanced")

                # Apply stepwise selection if requested
                enhanced_model <- private$.applyStepwiseSelection(enhanced_model,
                                                                  self$options$stepwiseDirection,
                                                                  self$options$selectionCriterion)
                private$.models[["enhanced"]] <- enhanced_model

                models_built <- c(models_built, "enhanced")

                # Populate model summary table
                if (self$options$showModelSummary) {
                    private$.populateModelSummary(enhanced_model, "enhancedModelSummary")
                }
            }

            # Biomarker Model
            if (self$options$buildBiomarkerModel && length(self$options$biomarkerPredictors) > 0) {
                biomarker_predictors <- self$options$biomarkerPredictors

                # Apply transformations and interactions
                modeling_data <- private$.transformVariables(modeling_data, biomarker_predictors)
                modeling_data <- private$.createInteractions(modeling_data, biomarker_predictors)

                # Build formula
                predictor_string <- paste(biomarker_predictors, collapse = " + ")
                formula_biomarker <- as.formula(paste(outcome_var, "~", predictor_string))

                # Build model
                biomarker_model <- private$.buildLogisticModel(formula_biomarker, modeling_data, "biomarker")

                # Apply stepwise selection if requested
                biomarker_model <- private$.applyStepwiseSelection(biomarker_model,
                                                                   self$options$stepwiseDirection,
                                                                   self$options$selectionCriterion)
                private$.models[["biomarker"]] <- biomarker_model

                models_built <- c(models_built, "biomarker")

                # Populate model summary table
                if (self$options$showModelSummary) {
                    private$.populateModelSummary(biomarker_model, "biomarkerModelSummary")
                }
            }

            # Custom Model
            if (self$options$buildCustomModel && length(self$options$customPredictors) > 0) {
                custom_predictors <- self$options$customPredictors

                # Apply transformations and interactions
                modeling_data <- private$.transformVariables(modeling_data, custom_predictors)
                modeling_data <- private$.createInteractions(modeling_data, custom_predictors)

                # Build formula
                predictor_string <- paste(custom_predictors, collapse = " + ")
                formula_custom <- as.formula(paste(outcome_var, "~", predictor_string))

                # Build model
                custom_model <- private$.buildLogisticModel(formula_custom, modeling_data, "custom")

                # Apply stepwise selection if requested
                custom_model <- private$.applyStepwiseSelection(custom_model,
                                                                self$options$stepwiseDirection,
                                                                self$options$selectionCriterion)
                private$.models[["custom"]] <- custom_model

                models_built <- c(models_built, "custom")

                # Populate model summary table
                if (self$options$showModelSummary) {
                    private$.populateModelSummary(custom_model, "customModelSummary")
                }
            }

            # Calculate performance metrics for all models
            if (self$options$showPerformanceMetrics || self$options$compareModels) {
                private$.calculateAllPerformance(models_built)
            }

            # Generate cross-validation results
            if (self$options$crossValidation) {
                private$.performAllCrossValidation(models_built, modeling_data)
            }

            # Add predictions to dataset
            if (self$options$createPredictions) {
                private$.addPredictionsToDataset()
            }

            # Prepare for DCA
            if (self$options$exportForDCA) {
                private$.prepareDCAOutput()
            }

            # Generate risk scores
            if (self$options$generateRiskScore) {
                private$.generateAllRiskScores(models_built)
            }
        },

        .populateModelSummary = function(model, table_name) {
            summary_table <- self$results[[table_name]]
            summary_table$deleteRows()

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
        },

        .calculateAllPerformance = function(models_built) {
            if (!self$options$compareModels) return()

            comparison_table <- self$results$modelComparisonTable
            comparison_table$deleteRows()

            outcome_var <- self$options$outcome

            for (i in seq_along(models_built)) {
                model_name <- models_built[i]
                model <- private$.models[[model_name]]

                # Training performance
                train_pred <- predict(model, type = "response")
                train_actual <- private$.trainingData[[outcome_var]]
                train_perf <- private$.calculatePerformance(model, train_pred, train_actual, "training")

                # Validation performance (if available)
                if (!is.null(private$.validationData)) {
                    val_pred <- predict(model, newdata = private$.validationData, type = "response")
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

        .performAllCrossValidation = function(models_built, data) {
            if (!self$options$crossValidation) return()

            # Implementation placeholder for cross-validation
            # This would iterate through all models and perform CV
        },

        .addPredictionsToDataset = function() {
            if (!self$options$createPredictions) return()

            # Add predicted probability columns to the original dataset
            # This would modify self$data to include prediction columns
            # Implementation placeholder
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

        # Plotting functions
        .plotROCCurves = function(image, ggtheme, theme, ...) {
            # Implementation for ROC curves comparison plot
            return(FALSE)  # Placeholder
        },

        .plotCalibration = function(image, ggtheme, theme, ...) {
            # Implementation for calibration plots
            return(FALSE)  # Placeholder
        },

        .plotModelComparison = function(image, ggtheme, theme, ...) {
            # Implementation for model comparison plot
            return(FALSE)  # Placeholder
        },

        .plotValidation = function(image, ggtheme, theme, ...) {
            # Implementation for validation results plot
            return(FALSE)  # Placeholder
        }
    )
)
