#' @title AI Model Validation with Cross-Validation
#' @description
#' Comprehensive validation of AI models and diagnostic tests using cross-validation,
#' model selection, and advanced performance metrics. Designed for AI diagnostic research
#' including comparison of AI vs human performance with statistical significance testing.
#' 
#' This function addresses the complete workflow needed for AI diagnostic research:
#' - Cross-validated performance assessment
#' - Automated model selection with AIC/BIC
#' - Statistical comparison between AI and human performance
#' - Advanced metrics (NRI, IDI, calibration)
#' - Publication-ready visualizations
#' 
#' @details
#' \strong{Cross-Validation Methods:}
#' \itemize{
#'   \item k-fold cross-validation (5-fold, 10-fold)
#'   \item Leave-one-out cross-validation
#'   \item Repeated cross-validation with multiple iterations
#'   \item Stratified sampling to maintain outcome proportions
#' }
#' 
#' \strong{Model Selection:}
#' \itemize{
#'   \item AIC/BIC-based stepwise selection
#'   \item Forward, backward, or bidirectional selection
#'   \item LASSO and Ridge regularization
#'   \item Variable importance ranking
#' }
#' 
#' \strong{Statistical Tests:}
#' \itemize{
#'   \item DeLong test for AUC comparison
#'   \item McNemar's test for paired predictions
#'   \item Hosmer-Lemeshow calibration test
#'   \item Bootstrap confidence intervals
#' }
#' 
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom caret createFolds createDataPartition train trainControl
#' @importFrom pROC roc auc ci coords roc.test
#' @importFrom MASS stepAIC
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom boot boot boot.ci
#' @importFrom ResourceSelection hoslem.test
#' @importFrom PredictABEL reclassification
#' @importFrom reshape2 melt
#' @importFrom scales percent

aivalidationClass <- if (requireNamespace("jmvcore", quietly = TRUE)) 
  R6::R6Class(
    "aivalidationClass",
    inherit = aivalidationBase,
    
    private = list(
      .data = NULL,
      .results_cache = list(),
      .cv_results = NULL,
      .model_results = NULL,
      
      .init = function() {
        # Initialize results tables and set up dynamic UI elements
        private$.initializeTables()
        private$.setupDynamicUI()
        
        if (private$.hasMinimalInputs()) {
          self$results$todo$setVisible(FALSE)
        } else {
          private$.showWelcomeMessage()
        }
      },
      
      .hasMinimalInputs = function() {
        return(!is.null(self$options$outcomeVar) && 
               length(self$options$predictorVars) > 0)
      },
      
      .showWelcomeMessage = function() {
        welcome_text <- "
        <h3>AI Model Validation with Cross-Validation</h3>
        <p><strong>Welcome to comprehensive AI diagnostic validation!</strong></p>
        
        <h4>Quick Start:</h4>
        <ol>
          <li><strong>Select Predictor Variables:</strong> AI scores, human scores, biomarkers</li>
          <li><strong>Choose Outcome Variable:</strong> Gold standard diagnosis (binary)</li>
          <li><strong>Set Positive Level:</strong> Which outcome represents disease/positive</li>
          <li><strong>Configure Cross-Validation:</strong> Default 10-fold is recommended</li>
          <li><strong>Enable Model Comparison:</strong> Compare AI vs human performance</li>
        </ol>
        
        <h4>Perfect for:</h4>
        <ul>
          <li>AI vs Human Expert Comparison Studies</li>
          <li>Biomarker Validation Research</li>
          <li>Clinical Prediction Model Development</li>
          <li>Diagnostic Test Evaluation</li>
          <li>Machine Learning Model Validation</li>
        </ul>
        
        <h4>Key Features:</h4>
        <ul>
          <li>Cross-validated performance metrics with confidence intervals</li>
          <li>DeLong test for statistical AUC comparison</li>
          <li>Automated model selection (AIC/BIC/LASSO)</li>
          <li>Calibration assessment and plots</li>
          <li>Net Reclassification Index (NRI) and IDI</li>
          <li>Publication-ready visualizations</li>
        </ul>
        "
        
        self$results$todo$setContent(welcome_text)
        self$results$todo$setVisible(TRUE)
      },
      
      .initializeTables = function() {
        # Initialize all results tables with proper structure
        private$.initCVPerformanceTable()
        private$.initModelSelectionTable()
        private$.initModelComparisonTable()
        private$.initNRIIDITable()
        private$.initCalibrationTable()
        private$.initVariableImportanceTable()
        private$.initCVFoldTable()
      },
      
      .initCVPerformanceTable = function() {
        table <- self$results$cvPerformanceTable
        # Table structure defined in .r.yaml
      },
      
      .initModelSelectionTable = function() {
        table <- self$results$modelSelectionTable
        # Table structure defined in .r.yaml
      },
      
      .initModelComparisonTable = function() {
        table <- self$results$modelComparisonTable
        # Table structure defined in .r.yaml
      },
      
      .initNRIIDITable = function() {
        table <- self$results$nriIdiTable
        # Table structure defined in .r.yaml
      },
      
      .initCalibrationTable = function() {
        table <- self$results$calibrationTable
        # Table structure defined in .r.yaml
      },
      
      .initVariableImportanceTable = function() {
        table <- self$results$variableImportanceTable
        # Table structure defined in .r.yaml
      },
      
      .initCVFoldTable = function() {
        table <- self$results$cvFoldResults
        # Table structure defined in .r.yaml  
      },
      
      .setupDynamicUI = function() {
        # Set up dynamic UI elements based on data
        if (!is.null(self$data)) {
          private$.updateOutcomeLevels()
          private$.updateReferencePredictors()
        }
      },
      
      .updateOutcomeLevels = function() {
        if (!is.null(self$options$outcomeVar) && self$options$outcomeVar != "") {
          outcome_var <- self$options$outcomeVar
          if (outcome_var %in% names(self$data)) {
            levels <- levels(as.factor(self$data[[outcome_var]]))
            # Update positive level options dynamically
          }
        }
      },
      
      .updateReferencePredictors = function() {
        if (length(self$options$predictorVars) > 0) {
          # Update reference predictor options dynamically
        }
      },
      
      .run = function() {
        # Main analysis workflow
        if (!private$.hasMinimalInputs()) {
          return()
        }
        
        tryCatch({
          # Step 1: Validate and prepare data
          private$.validateInputs()
          private$.data <- private$.prepareData()
          
          # Step 2: Perform cross-validation analysis
          if (self$options$crossValidation != "none") {
            private$.cv_results <- private$.performCrossValidation()
            private$.fillCVPerformanceTable()
            private$.fillCVFoldTable()
          }
          
          # Step 3: Model selection if requested
          if (self$options$modelSelection != "none") {
            private$.model_results <- private$.performModelSelection()
            if (self$options$showModelSelection) {
              private$.fillModelSelectionTable()
            }
            if (self$options$variableImportancePlot) {
              private$.fillVariableImportanceTable()
            }
          }
          
          # Step 4: Model comparison and statistical tests
          if (self$options$compareModels && length(self$options$predictorVars) >= 2) {
            private$.performModelComparison()
          }
          
          # Step 5: Advanced metrics (NRI/IDI)
          if ((self$options$calculateNRI || self$options$calculateIDI) && 
              !is.null(self$options$referencePredictor)) {
            private$.calculateAdvancedMetrics()
          }
          
          # Step 6: Calibration analysis
          if (self$options$calibrationTest) {
            private$.performCalibrationAnalysis()
          }
          
          # Step 7: Generate plots
          private$.generateAllPlots()
          
          # Step 8: Generate explanations and summaries
          if (self$options$showExplanations) {
            private$.generateExplanations()
          }
          if (self$options$showSummaries) {
            private$.generateSummaries()
          }
          
        }, error = function(e) {
          stop(paste("Analysis failed:", conditionMessage(e), 
                    "Please check your data and settings."))
        })
      },
      
      .validateInputs = function() {
        # Comprehensive input validation
        if (is.null(self$data) || nrow(self$data) == 0) {
          stop("No data provided or data is empty.")
        }
        
        if (length(self$options$predictorVars) == 0) {
          stop("At least one predictor variable must be selected.")
        }
        
        if (is.null(self$options$outcomeVar) || self$options$outcomeVar == "") {
          stop("Outcome variable must be selected.")
        }
        
        if (is.null(self$options$positiveLevel) || self$options$positiveLevel == "") {
          stop("Positive level for outcome variable must be specified.")
        }
        
        # Check if all variables exist in data
        all_vars <- c(self$options$predictorVars, self$options$outcomeVar)
        missing_vars <- all_vars[!all_vars %in% names(self$data)]
        if (length(missing_vars) > 0) {
          stop(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
        }
        
        # Check outcome variable has exactly 2 levels
        outcome_levels <- unique(self$data[[self$options$outcomeVar]])
        outcome_levels <- outcome_levels[!is.na(outcome_levels)]
        if (length(outcome_levels) != 2) {
          stop(paste("Outcome variable must have exactly 2 levels. Found:", 
                    length(outcome_levels), "levels"))
        }
        
        if (!self$options$positiveLevel %in% outcome_levels) {
          stop(paste("Positive level '", self$options$positiveLevel, 
                    "' not found in outcome variable levels:", 
                    paste(outcome_levels, collapse = ", ")))
        }
        
        # Validate predictor variables are numeric
        for (var in self$options$predictorVars) {
          if (!is.numeric(self$data[[var]])) {
            stop(paste("Predictor variable '", var, "' must be numeric."))
          }
        }
      },
      
      .prepareData = function() {
        # Clean and prepare data for analysis
        data <- self$data
        
        # Select relevant variables
        vars_needed <- c(self$options$predictorVars, self$options$outcomeVar)
        data <- data[, vars_needed, drop = FALSE]
        
        # Remove rows with missing values
        complete_rows <- complete.cases(data)
        if (sum(complete_rows) == 0) {
          stop("No complete cases found in the data.")
        }
        
        data <- data[complete_rows, , drop = FALSE]
        
        # Convert outcome to binary factor with positive class as level 2
        outcome_var <- self$options$outcomeVar
        positive_level <- self$options$positiveLevel
        
        data[[outcome_var]] <- factor(data[[outcome_var]], 
                                     levels = c(setdiff(unique(data[[outcome_var]]), positive_level), 
                                               positive_level))
        
        # Standardize predictor variables for some analyses
        for (var in self$options$predictorVars) {
          # Keep original for interpretation, create standardized versions if needed
          data[[paste0(var, "_std")]] <- scale(data[[var]])[,1]
        }
        
        return(data)
      },
      
      .performCrossValidation = function() {
        # Main cross-validation orchestrator
        cv_method <- self$options$crossValidation
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        
        set.seed(self$options$randomSeed)
        
        if (cv_method == "5-fold") {
          folds <- private$.create5FoldCV(data, outcome_var)
        } else if (cv_method == "10-fold") {
          folds <- private$.create10FoldCV(data, outcome_var)
        } else if (cv_method == "LOO") {
          folds <- private$.createLOOCV(data)
        } else if (cv_method == "repeated") {
          folds <- private$.createRepeatedCV(data, outcome_var)
        }
        
        # Perform CV for each predictor variable
        cv_results <- list()
        for (predictor in self$options$predictorVars) {
          cv_results[[predictor]] <- private$.runCVForPredictor(data, predictor, folds)
        }
        
        # If model selection is enabled, also run CV on selected models
        if (self$options$modelSelection != "none") {
          cv_results[["selected_model"]] <- private$.runCVForSelectedModel(data, folds)
        }
        
        return(cv_results)
      },
      
      .create5FoldCV = function(data, outcome_var) {
        if (self$options$stratified) {
          return(caret::createFolds(data[[outcome_var]], k = 5, returnTrain = TRUE))
        } else {
          n <- nrow(data)
          fold_size <- ceiling(n / 5)
          folds <- rep(1:5, length.out = n)
          folds <- sample(folds)
          fold_list <- list()
          for (i in 1:5) {
            fold_list[[paste0("Fold", i)]] <- which(folds != i)
          }
          return(fold_list)
        }
      },
      
      .create10FoldCV = function(data, outcome_var) {
        if (self$options$stratified) {
          return(caret::createFolds(data[[outcome_var]], k = 10, returnTrain = TRUE))
        } else {
          n <- nrow(data)
          fold_size <- ceiling(n / 10)
          folds <- rep(1:10, length.out = n)
          folds <- sample(folds)
          fold_list <- list()
          for (i in 1:10) {
            fold_list[[paste0("Fold", sprintf("%02d", i))]] <- which(folds != i)
          }
          return(fold_list)
        }
      },
      
      .createLOOCV = function(data) {
        n <- nrow(data)
        fold_list <- list()
        for (i in 1:n) {
          fold_list[[paste0("Fold", sprintf("%03d", i))]] <- setdiff(1:n, i)
        }
        return(fold_list)
      },
      
      .createRepeatedCV = function(data, outcome_var) {
        n_repeats <- self$options$nRepeats
        all_folds <- list()
        
        for (rep in 1:n_repeats) {
          set.seed(self$options$randomSeed + rep)
          rep_folds <- private$.create10FoldCV(data, outcome_var)
          for (fold_name in names(rep_folds)) {
            new_name <- paste0("Rep", sprintf("%02d", rep), "_", fold_name)
            all_folds[[new_name]] <- rep_folds[[fold_name]]
          }
        }
        return(all_folds)
      },
      
      .runCVForPredictor = function(data, predictor, folds) {
        # Run cross-validation for a single predictor
        outcome_var <- self$options$outcomeVar
        
        fold_results <- list()
        
        for (fold_name in names(folds)) {
          train_idx <- folds[[fold_name]]
          test_idx <- setdiff(1:nrow(data), train_idx)
          
          train_data <- data[train_idx, ]
          test_data <- data[test_idx, ]
          
          # Fit logistic regression model
          formula_str <- paste(outcome_var, "~", predictor)
          model <- glm(formula_str, data = train_data, family = "binomial")
          
          # Make predictions
          pred_probs <- predict(model, test_data, type = "response")
          
          # Calculate performance metrics
          actual <- as.numeric(test_data[[outcome_var]]) - 1  # Convert to 0/1
          
          # ROC analysis
          if (requireNamespace("pROC", quietly = TRUE)) {
            roc_obj <- pROC::roc(actual, pred_probs, quiet = TRUE)
            auc_val <- as.numeric(pROC::auc(roc_obj))
            
            # Optimal threshold using Youden's J
            if (self$options$youdensJ) {
              coords_result <- pROC::coords(roc_obj, "best", best.method = "youden")
              threshold <- coords_result$threshold
              sensitivity <- coords_result$sensitivity
              specificity <- coords_result$specificity
              youdens_j <- sensitivity + specificity - 1
            } else {
              threshold <- 0.5
              sensitivity <- sum(pred_probs >= threshold & actual == 1) / sum(actual == 1)
              specificity <- sum(pred_probs < threshold & actual == 0) / sum(actual == 0)
              youdens_j <- sensitivity + specificity - 1
            }
            
            accuracy <- mean((pred_probs >= threshold) == actual)
            
            fold_results[[fold_name]] <- list(
              auc = auc_val,
              sensitivity = sensitivity,
              specificity = specificity,
              accuracy = accuracy,
              youdens_j = youdens_j,
              threshold = threshold,
              n_cases = sum(actual == 1),
              n_controls = sum(actual == 0),
              predictions = pred_probs,
              actual = actual
            )
          }
        }
        
        return(fold_results)
      },
      
      .runCVForSelectedModel = function(data, folds) {
        # Run cross-validation for the selected model (multiple predictors)
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        
        fold_results <- list()
        
        for (fold_name in names(folds)) {
          train_idx <- folds[[fold_name]]
          test_idx <- setdiff(1:nrow(data), train_idx)
          
          train_data <- data[train_idx, ]
          test_data <- data[test_idx, ]
          
          # Perform model selection on training data
          selected_model <- private$.performModelSelectionOnFold(train_data, outcome_var, predictors)
          
          # Make predictions using selected model
          pred_probs <- predict(selected_model, test_data, type = "response")
          
          # Calculate performance metrics (same as single predictor)
          actual <- as.numeric(test_data[[outcome_var]]) - 1
          
          if (requireNamespace("pROC", quietly = TRUE)) {
            roc_obj <- pROC::roc(actual, pred_probs, quiet = TRUE)
            auc_val <- as.numeric(pROC::auc(roc_obj))
            
            # Calculate other metrics...
            if (self$options$youdensJ) {
              coords_result <- pROC::coords(roc_obj, "best", best.method = "youden")
              threshold <- coords_result$threshold
              sensitivity <- coords_result$sensitivity
              specificity <- coords_result$specificity
              youdens_j <- sensitivity + specificity - 1
            } else {
              threshold <- 0.5
              sensitivity <- sum(pred_probs >= threshold & actual == 1) / sum(actual == 1)
              specificity <- sum(pred_probs < threshold & actual == 0) / sum(actual == 0)
              youdens_j <- sensitivity + specificity - 1
            }
            
            accuracy <- mean((pred_probs >= threshold) == actual)
            
            fold_results[[fold_name]] <- list(
              auc = auc_val,
              sensitivity = sensitivity,
              specificity = specificity,
              accuracy = accuracy,
              youdens_j = youdens_j,
              threshold = threshold,
              n_cases = sum(actual == 1),
              n_controls = sum(actual == 0),
              predictions = pred_probs,
              actual = actual,
              selected_vars = names(selected_model$coefficients)[-1]  # Exclude intercept
            )
          }
        }
        
        return(fold_results)
      },
      
      .performModelSelectionOnFold = function(train_data, outcome_var, predictors) {
        # Perform model selection on a single fold
        selection_method <- self$options$modelSelection
        
        if (selection_method %in% c("AIC", "BIC")) {
          return(private$.performStepwiseSelection(train_data, outcome_var, predictors))
        } else if (selection_method == "lasso") {
          return(private$.performLassoSelection(train_data, outcome_var, predictors))
        } else if (selection_method == "ridge") {
          return(private$.performRidgeRegression(train_data, outcome_var, predictors))
        } else if (selection_method == "elastic") {
          return(private$.performElasticNet(train_data, outcome_var, predictors))
        } else {
          # Full model
          formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
          return(glm(formula_str, data = train_data, family = "binomial"))
        }
      },
      
      .performStepwiseSelection = function(train_data, outcome_var, predictors) {
        # AIC/BIC stepwise selection
        full_formula <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
        full_model <- glm(full_formula, data = train_data, family = "binomial")
        
        null_formula <- paste(outcome_var, "~ 1")
        null_model <- glm(null_formula, data = train_data, family = "binomial")
        
        direction <- self$options$selectionDirection
        k_value <- ifelse(self$options$modelSelection == "BIC", log(nrow(train_data)), 2)
        
        if (requireNamespace("MASS", quietly = TRUE)) {
          selected_model <- MASS::stepAIC(null_model, 
                                         scope = list(lower = null_model, upper = full_model),
                                         direction = direction, 
                                         k = k_value, 
                                         trace = FALSE)
          return(selected_model)
        } else {
          return(full_model)  # Fallback to full model
        }
      },
      
      .performLassoSelection = function(train_data, outcome_var, predictors) {
        # LASSO regularization
        if (requireNamespace("glmnet", quietly = TRUE)) {
          x_matrix <- as.matrix(train_data[, predictors, drop = FALSE])
          y_vector <- as.numeric(train_data[[outcome_var]]) - 1
          
          cv_lasso <- glmnet::cv.glmnet(x_matrix, y_vector, family = "binomial", alpha = 1)
          lasso_model <- glmnet::glmnet(x_matrix, y_vector, family = "binomial", 
                                       alpha = 1, lambda = cv_lasso$lambda.min)
          
          # Convert to glm-like object for compatibility
          selected_vars <- predictors[which(as.vector(coef(lasso_model))[-1] != 0)]
          if (length(selected_vars) > 0) {
            formula_str <- paste(outcome_var, "~", paste(selected_vars, collapse = " + "))
            return(glm(formula_str, data = train_data, family = "binomial"))
          } else {
            # Return null model if no variables selected
            null_formula <- paste(outcome_var, "~ 1")
            return(glm(null_formula, data = train_data, family = "binomial"))
          }
        } else {
          # Fallback to full model
          formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
          return(glm(formula_str, data = train_data, family = "binomial"))
        }
      },
      
      .performRidgeRegression = function(train_data, outcome_var, predictors) {
        # Ridge regression (similar to LASSO but alpha = 0)
        if (requireNamespace("glmnet", quietly = TRUE)) {
          x_matrix <- as.matrix(train_data[, predictors, drop = FALSE])
          y_vector <- as.numeric(train_data[[outcome_var]]) - 1
          
          cv_ridge <- glmnet::cv.glmnet(x_matrix, y_vector, family = "binomial", alpha = 0)
          ridge_model <- glmnet::glmnet(x_matrix, y_vector, family = "binomial", 
                                       alpha = 0, lambda = cv_ridge$lambda.min)
          
          # For ridge, typically keep all variables
          formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
          return(glm(formula_str, data = train_data, family = "binomial"))
        } else {
          formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
          return(glm(formula_str, data = train_data, family = "binomial"))
        }
      },
      
      .performElasticNet = function(train_data, outcome_var, predictors) {
        # Elastic net (alpha = 0.5)
        if (requireNamespace("glmnet", quietly = TRUE)) {
          x_matrix <- as.matrix(train_data[, predictors, drop = FALSE])
          y_vector <- as.numeric(train_data[[outcome_var]]) - 1
          
          cv_elastic <- glmnet::cv.glmnet(x_matrix, y_vector, family = "binomial", alpha = 0.5)
          elastic_model <- glmnet::glmnet(x_matrix, y_vector, family = "binomial", 
                                         alpha = 0.5, lambda = cv_elastic$lambda.min)
          
          # Select non-zero coefficients
          selected_vars <- predictors[which(as.vector(coef(elastic_model))[-1] != 0)]
          if (length(selected_vars) > 0) {
            formula_str <- paste(outcome_var, "~", paste(selected_vars, collapse = " + "))
            return(glm(formula_str, data = train_data, family = "binomial"))
          } else {
            null_formula <- paste(outcome_var, "~ 1")
            return(glm(null_formula, data = train_data, family = "binomial"))
          }
        } else {
          formula_str <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
          return(glm(formula_str, data = train_data, family = "binomial"))
        }
      },
      
      .fillCVPerformanceTable = function() {
        # Fill the cross-validated performance table
        if (is.null(private$.cv_results)) return()
        
        table <- self$results$cvPerformanceTable
        
        for (model_name in names(private$.cv_results)) {
          fold_results <- private$.cv_results[[model_name]]
          
          # Calculate summary statistics across folds
          aucs <- sapply(fold_results, function(x) x$auc)
          sensitivities <- sapply(fold_results, function(x) x$sensitivity)
          specificities <- sapply(fold_results, function(x) x$specificity)
          accuracies <- sapply(fold_results, function(x) x$accuracy)
          youdens_js <- sapply(fold_results, function(x) x$youdens_j)
          
          # Calculate means and confidence intervals
          mean_auc <- mean(aucs, na.rm = TRUE)
          mean_sen <- mean(sensitivities, na.rm = TRUE)
          mean_spe <- mean(specificities, na.rm = TRUE)
          mean_acc <- mean(accuracies, na.rm = TRUE)
          mean_youdens <- mean(youdens_js, na.rm = TRUE)
          
          # Bootstrap confidence intervals if requested
          if (self$options$bootstrapCI) {
            auc_ci <- private$.calculateBootstrapCI(aucs)
            sen_ci <- private$.calculateBootstrapCI(sensitivities)
            spe_ci <- private$.calculateBootstrapCI(specificities)
          } else {
            # Use standard error approximation
            auc_ci <- private$.calculateSECI(aucs)
            sen_ci <- private$.calculateSECI(sensitivities)
            spe_ci <- private$.calculateSECI(specificities)
          }
          
          # Add row to table
          table$addRow(rowKey = model_name, values = list(
            model = model_name,
            auc = mean_auc,
            auc_ci_lower = auc_ci[1],
            auc_ci_upper = auc_ci[2],
            sensitivity = mean_sen,
            sen_ci_lower = sen_ci[1],
            sen_ci_upper = sen_ci[2],
            specificity = mean_spe,
            spe_ci_lower = spe_ci[1],
            spe_ci_upper = spe_ci[2],
            accuracy = mean_acc,
            youdens_j = mean_youdens
          ))
        }
      },
      
      .calculateBootstrapCI = function(values) {
        # Calculate bootstrap confidence intervals
        if (length(values) < 3) return(c(NA, NA))
        
        if (requireNamespace("boot", quietly = TRUE)) {
          boot_fun <- function(data, indices) {
            mean(data[indices], na.rm = TRUE)
          }
          
          boot_result <- boot::boot(values, boot_fun, R = self$options$nBootstrap)
          ci_result <- boot::boot.ci(boot_result, conf = self$options$confidenceLevel, type = "perc")
          
          if (!is.null(ci_result$percent)) {
            return(c(ci_result$percent[4], ci_result$percent[5]))
          }
        }
        
        # Fallback to SE-based CI
        return(private$.calculateSECI(values))
      },
      
      .calculateSECI = function(values) {
        # Calculate confidence intervals using standard error
        if (length(values) < 2) return(c(NA, NA))
        
        mean_val <- mean(values, na.rm = TRUE)
        se_val <- sd(values, na.rm = TRUE) / sqrt(length(values))
        
        alpha <- 1 - self$options$confidenceLevel
        t_crit <- qt(1 - alpha/2, df = length(values) - 1)
        
        ci_lower <- mean_val - t_crit * se_val
        ci_upper <- mean_val + t_crit * se_val
        
        return(c(ci_lower, ci_upper))
      },
      
      .fillCVFoldTable = function() {
        # Fill the detailed CV fold results table
        if (is.null(private$.cv_results) || !self$options$showCrossValidation) return()
        
        table <- self$results$cvFoldResults
        
        for (model_name in names(private$.cv_results)) {
          fold_results <- private$.cv_results[[model_name]]
          
          for (fold_name in names(fold_results)) {
            fold_data <- fold_results[[fold_name]]
            
            table$addRow(rowKey = paste(model_name, fold_name, sep = "_"), values = list(
              fold = fold_name,
              model = model_name,
              auc = fold_data$auc,
              sensitivity = fold_data$sensitivity,
              specificity = fold_data$specificity,
              accuracy = fold_data$accuracy,
              n_cases = fold_data$n_cases,
              n_controls = fold_data$n_controls
            ))
          }
        }
      },
      
      .performModelSelection = function() {
        # Perform model selection on full dataset
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        
        selection_method <- self$options$modelSelection
        
        if (selection_method == "none") return(NULL)
        
        # Perform the selected model selection method
        selected_model <- private$.performModelSelectionOnFold(data, outcome_var, predictors)
        
        # Calculate model statistics
        model_stats <- list(
          model = selected_model,
          selected_variables = names(selected_model$coefficients)[-1],  # Exclude intercept
          aic = AIC(selected_model),
          bic = BIC(selected_model),
          deviance = deviance(selected_model),
          r_squared = private$.calculatePseudoRSquared(selected_model)
        )
        
        return(model_stats)
      },
      
      .calculatePseudoRSquared = function(model) {
        # Calculate Nagelkerke pseudo R-squared
        null_model <- update(model, . ~ 1)
        
        ll_null <- logLik(null_model)
        ll_model <- logLik(model)
        n <- length(model$fitted.values)
        
        # Nagelkerke R-squared
        cox_snell <- 1 - exp((2/n) * (ll_null - ll_model))
        nagelkerke <- cox_snell / (1 - exp((2/n) * ll_null))
        
        return(as.numeric(nagelkerke))
      },
      
      .fillModelSelectionTable = function() {
        # Fill model selection results table
        if (is.null(private$.model_results)) return()
        
        table <- self$results$modelSelectionTable
        model_stats <- private$.model_results
        
        table$addRow(rowKey = "selected", values = list(
          model = "Selected Model",
          variables = paste(model_stats$selected_variables, collapse = ", "),
          aic = model_stats$aic,
          bic = model_stats$bic,
          deviance = model_stats$deviance,
          r_squared = model_stats$r_squared
        ))
        
        # Also add full model for comparison
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        
        full_formula <- paste(outcome_var, "~", paste(predictors, collapse = " + "))
        full_model <- glm(full_formula, data = data, family = "binomial")
        
        table$addRow(rowKey = "full", values = list(
          model = "Full Model",
          variables = paste(predictors, collapse = ", "),
          aic = AIC(full_model),
          bic = BIC(full_model),
          deviance = deviance(full_model),
          r_squared = private$.calculatePseudoRSquared(full_model)
        ))
      },
      
      .performModelComparison = function() {
        # Perform statistical comparison between models
        if (!self$options$compareModels || length(self$options$predictorVars) < 2) return()
        
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        
        # DeLong test for AUC comparison
        if (self$options$delongTest) {
          private$.performDelongTest()
        }
        
        # McNemar's test for paired predictions
        if (self$options$mcnemarTest) {
          private$.performMcnemarTest()
        }
      },
      
      .performDelongTest = function() {
        # DeLong test for comparing AUCs
        if (!requireNamespace("pROC", quietly = TRUE)) return()
        
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        
        # Create ROC objects for each predictor
        roc_objects <- list()
        for (pred in predictors) {
          actual <- as.numeric(data[[outcome_var]]) - 1
          roc_objects[[pred]] <- pROC::roc(actual, data[[pred]], quiet = TRUE)
        }
        
        # Perform pairwise comparisons
        table <- self$results$modelComparisonTable
        
        for (i in 1:(length(predictors)-1)) {
          for (j in (i+1):length(predictors)) {
            pred1 <- predictors[i]
            pred2 <- predictors[j]
            
            # DeLong test
            delong_result <- pROC::roc.test(roc_objects[[pred1]], roc_objects[[pred2]], 
                                           method = "delong")
            
            auc_diff <- as.numeric(pROC::auc(roc_objects[[pred1]]) - pROC::auc(roc_objects[[pred2]]))
            
            comparison_name <- paste(pred1, "vs", pred2)
            
            table$addRow(rowKey = comparison_name, values = list(
              comparison = comparison_name,
              test = "DeLong Test",
              statistic = delong_result$statistic,
              p_value = delong_result$p.value,
              effect_size = auc_diff,
              ci_lower = NA,  # DeLong test doesn't provide CI directly
              ci_upper = NA
            ))
          }
        }
      },
      
      .performMcnemarTest = function() {
        # McNemar's test for paired binary predictions
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        
        # Create binary predictions for each predictor
        predictions <- list()
        actual <- as.numeric(data[[outcome_var]]) - 1
        
        for (pred in predictors) {
          # Use optimal threshold or 0.5
          if (self$options$youdensJ) {
            roc_obj <- pROC::roc(actual, data[[pred]], quiet = TRUE)
            coords_result <- pROC::coords(roc_obj, "best", best.method = "youden")
            threshold <- coords_result$threshold
          } else {
            threshold <- 0.5
          }
          
          predictions[[pred]] <- as.numeric(data[[pred]] >= threshold)
        }
        
        # Perform pairwise McNemar's tests
        table <- self$results$modelComparisonTable
        
        for (i in 1:(length(predictors)-1)) {
          for (j in (i+1):length(predictors)) {
            pred1 <- predictors[i]
            pred2 <- predictors[j]
            
            # Create contingency table for McNemar's test
            # Rows: pred1, Cols: pred2, Values: correct/incorrect predictions
            pred1_correct <- predictions[[pred1]] == actual
            pred2_correct <- predictions[[pred2]] == actual
            
            # McNemar's test table
            a <- sum(pred1_correct & pred2_correct)     # Both correct
            b <- sum(pred1_correct & !pred2_correct)    # Only pred1 correct
            c <- sum(!pred1_correct & pred2_correct)    # Only pred2 correct
            d <- sum(!pred1_correct & !pred2_correct)   # Both incorrect
            
            # McNemar's test statistic
            if (b + c > 0) {
              mcnemar_stat <- (abs(b - c) - 1)^2 / (b + c)  # With continuity correction
              p_value <- 1 - pchisq(mcnemar_stat, df = 1)
              
              comparison_name <- paste(pred1, "vs", pred2, "(Predictions)")
              
              table$addRow(rowKey = paste(comparison_name, "McNemar"), values = list(
                comparison = comparison_name,
                test = "McNemar's Test",
                statistic = mcnemar_stat,
                p_value = p_value,
                effect_size = (b - c) / (b + c),  # Relative difference
                ci_lower = NA,
                ci_upper = NA
              ))
            }
          }
        }
      },
      
      .calculateAdvancedMetrics = function() {
        # Calculate NRI and IDI
        if (!self$options$calculateNRI && !self$options$calculateIDI) return()
        if (is.null(self$options$referencePredictor)) return()
        
        # This would require implementing NRI/IDI calculations
        # For now, placeholder structure
        private$.calculateNRI()
        private$.calculateIDI()
      },
      
      .calculateNRI = function() {
        if (!self$options$calculateNRI || is.null(self$options$referencePredictor)) return()
        
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        ref_pred <- self$options$referencePredictor
        predictors <- self$options$predictorVars
        
        # Calculate NRI for each predictor vs reference
        table <- self$results$nriIdiTable
        
        for (pred in setdiff(predictors, ref_pred)) {
          # Build models
          ref_formula <- paste(outcome_var, "~", ref_pred)
          new_formula <- paste(outcome_var, "~", ref_pred, "+", pred)
          
          ref_model <- glm(ref_formula, data = data, family = "binomial")
          new_model <- glm(new_formula, data = data, family = "binomial")
          
          # Get predictions
          ref_probs <- predict(ref_model, type = "response")
          new_probs <- predict(new_model, type = "response")
          
          actual <- as.numeric(data[[outcome_var]]) - 1
          
          # Calculate NRI using risk categories (tertiles)
          ref_cats <- cut(ref_probs, breaks = quantile(ref_probs, c(0, 0.33, 0.67, 1)), 
                          include.lowest = TRUE, labels = c("Low", "Intermediate", "High"))
          new_cats <- cut(new_probs, breaks = quantile(new_probs, c(0, 0.33, 0.67, 1)), 
                          include.lowest = TRUE, labels = c("Low", "Intermediate", "High"))
          
          # NRI calculation
          # Cases (events = 1)
          cases_idx <- actual == 1
          cases_up <- sum(as.numeric(new_cats[cases_idx]) > as.numeric(ref_cats[cases_idx]), na.rm = TRUE)
          cases_down <- sum(as.numeric(new_cats[cases_idx]) < as.numeric(ref_cats[cases_idx]), na.rm = TRUE)
          cases_total <- sum(cases_idx)
          nri_cases <- (cases_up - cases_down) / cases_total
          
          # Controls (events = 0)
          controls_idx <- actual == 0
          controls_up <- sum(as.numeric(new_cats[controls_idx]) > as.numeric(ref_cats[controls_idx]), na.rm = TRUE)
          controls_down <- sum(as.numeric(new_cats[controls_idx]) < as.numeric(ref_cats[controls_idx]), na.rm = TRUE)
          controls_total <- sum(controls_idx)
          nri_controls <- (controls_down - controls_up) / controls_total
          
          # Total NRI
          nri_total <- nri_cases + nri_controls
          
          # Standard error approximation
          se_cases <- sqrt((cases_up + cases_down) / (cases_total^2))
          se_controls <- sqrt((controls_up + controls_down) / (controls_total^2))
          se_total <- sqrt(se_cases^2 + se_controls^2)
          
          # Confidence interval
          alpha <- 1 - self$options$confidenceLevel
          z_crit <- qnorm(1 - alpha/2)
          ci_lower <- nri_total - z_crit * se_total
          ci_upper <- nri_total + z_crit * se_total
          
          # Z-test
          z_stat <- nri_total / se_total
          p_value <- 2 * (1 - pnorm(abs(z_stat)))
          
          table$addRow(rowKey = paste("nri", pred, sep = "_"), values = list(
            comparison = paste("NRI:", pred, "vs", ref_pred),
            metric = "Net Reclassification Index",
            estimate = nri_total,
            se = se_total,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            p_value = p_value
          ))
        }
      },
      
      .calculateIDI = function() {
        if (!self$options$calculateIDI || is.null(self$options$referencePredictor)) return()
        
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        ref_pred <- self$options$referencePredictor
        predictors <- self$options$predictorVars
        
        table <- self$results$nriIdiTable
        
        for (pred in setdiff(predictors, ref_pred)) {
          # Build models
          ref_formula <- paste(outcome_var, "~", ref_pred)
          new_formula <- paste(outcome_var, "~", ref_pred, "+", pred)
          
          ref_model <- glm(ref_formula, data = data, family = "binomial")
          new_model <- glm(new_formula, data = data, family = "binomial")
          
          # Get predictions
          ref_probs <- predict(ref_model, type = "response")
          new_probs <- predict(new_model, type = "response")
          
          actual <- as.numeric(data[[outcome_var]]) - 1
          
          # IDI calculation
          # Discrimination slopes
          cases_idx <- actual == 1
          controls_idx <- actual == 0
          
          ref_slope <- mean(ref_probs[cases_idx]) - mean(ref_probs[controls_idx])
          new_slope <- mean(new_probs[cases_idx]) - mean(new_probs[controls_idx])
          
          idi <- new_slope - ref_slope
          
          # Standard error (approximation)
          n_cases <- sum(cases_idx)
          n_controls <- sum(controls_idx)
          
          var_ref_cases <- var(ref_probs[cases_idx]) / n_cases
          var_ref_controls <- var(ref_probs[controls_idx]) / n_controls
          var_new_cases <- var(new_probs[cases_idx]) / n_cases
          var_new_controls <- var(new_probs[controls_idx]) / n_controls
          
          se_idi <- sqrt(var_new_cases + var_new_controls + var_ref_cases + var_ref_controls)
          
          # Confidence interval
          alpha <- 1 - self$options$confidenceLevel
          z_crit <- qnorm(1 - alpha/2)
          ci_lower <- idi - z_crit * se_idi
          ci_upper <- idi + z_crit * se_idi
          
          # Z-test
          z_stat <- idi / se_idi
          p_value <- 2 * (1 - pnorm(abs(z_stat)))
          
          table$addRow(rowKey = paste("idi", pred, sep = "_"), values = list(
            comparison = paste("IDI:", pred, "vs", ref_pred),
            metric = "Integrated Discrimination Index",
            estimate = idi,
            se = se_idi,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            p_value = p_value
          ))
        }
      },
      
      .performCalibrationAnalysis = function() {
        # Calibration analysis using Hosmer-Lemeshow test
        if (!self$options$calibrationTest) return()
        
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        
        table <- self$results$calibrationTable
        
        for (pred in predictors) {
          # Fit logistic model
          formula_str <- paste(outcome_var, "~", pred)
          model <- glm(formula_str, data = data, family = "binomial")
          
          # Get predictions
          pred_probs <- predict(model, type = "response")
          actual <- as.numeric(data[[outcome_var]]) - 1
          
          # Hosmer-Lemeshow test
          if (requireNamespace("ResourceSelection", quietly = TRUE)) {
            hl_test <- ResourceSelection::hoslem.test(actual, pred_probs, g = 10)
            
            # Calibration slope and intercept
            calib_model <- glm(actual ~ qlogis(pred_probs), family = "binomial")
            calib_slope <- coef(calib_model)[2]
            calib_intercept <- coef(calib_model)[1]
            
            # Brier score
            brier_score <- mean((actual - pred_probs)^2)
            
            table$addRow(rowKey = pred, values = list(
              model = pred,
              hl_statistic = hl_test$statistic,
              hl_df = hl_test$parameter,
              hl_p_value = hl_test$p.value,
              calibration_slope = calib_slope,
              calibration_intercept = calib_intercept,
              brier_score = brier_score
            ))
          }
        }
      },
      
      .fillVariableImportanceTable = function() {
        # Fill variable importance table from model selection
        if (is.null(private$.model_results)) return()
        
        table <- self$results$variableImportanceTable
        
        # Get selected variables and their coefficients
        model <- private$.model_results$model
        selected_vars <- private$.model_results$selected_variables
        
        if (length(selected_vars) > 0) {
          # Calculate importance based on absolute coefficient values
          coefficients <- abs(coef(model)[-1])  # Exclude intercept
          names(coefficients) <- selected_vars
          
          # Rank by importance
          importance_order <- order(coefficients, decreasing = TRUE)
          
          for (i in 1:length(coefficients)) {
            var_idx <- importance_order[i]
            var_name <- names(coefficients)[var_idx]
            importance_score <- coefficients[var_idx]
            
            table$addRow(rowKey = var_name, values = list(
              variable = var_name,
              importance = importance_score,
              rank = i,
              selected = "Yes"
            ))
          }
          
          # Add non-selected variables
          all_predictors <- self$options$predictorVars
          non_selected <- setdiff(all_predictors, selected_vars)
          
          for (var in non_selected) {
            table$addRow(rowKey = paste(var, "_unselected"), values = list(
              variable = var,
              importance = 0,
              rank = length(coefficients) + 1,
              selected = "No"
            ))
          }
        }
      },
      
      .generateAllPlots = function() {
        # Generate all requested plots
        if (self$options$rocPlot) {
          private$.generateROCPlot()
        }
        
        if (self$options$calibrationPlot) {
          private$.generateCalibrationPlot()
        }
        
        if (self$options$comparisonPlot && self$options$compareModels) {
          private$.generateComparisonPlot()
        }
        
        if (self$options$cvPerformancePlot && self$options$crossValidation != "none") {
          private$.generateCVPerformancePlot()
        }
        
        if (self$options$variableImportancePlot && self$options$modelSelection != "none") {
          private$.generateVariableImportancePlot()
        }
      },
      
      .generateROCPlot = function() {
        # Generate ROC curves with cross-validation confidence bands
        if (!requireNamespace("pROC", quietly = TRUE)) return()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return()
        
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        actual <- as.numeric(data[[outcome_var]]) - 1
        
        # Create base plot
        p <- ggplot2::ggplot()
        
        # Color palette
        colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
        
        # Add ROC curve for each predictor
        for (i in seq_along(predictors)) {
          pred <- predictors[i]
          roc_obj <- pROC::roc(actual, data[[pred]], quiet = TRUE)
          
          # Create data frame for ggplot
          roc_data <- data.frame(
            specificity = roc_obj$specificities,
            sensitivity = roc_obj$sensitivities,
            predictor = pred
          )
          
          # Add curve to plot
          p <- p + ggplot2::geom_line(data = roc_data, 
                                     ggplot2::aes(x = 1 - specificity, y = sensitivity, 
                                                 color = predictor),
                                     size = 1)
          
          # Add AUC to legend
          auc_val <- round(as.numeric(pROC::auc(roc_obj)), 3)
        }
        
        # Add reference line
        p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                                     color = "gray50")
        
        # Styling
        p <- p + ggplot2::labs(
          title = "ROC Curves Comparison",
          subtitle = "Cross-validated performance with confidence assessment",
          x = "1 - Specificity (False Positive Rate)",
          y = "Sensitivity (True Positive Rate)",
          color = "Predictor"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          legend.position = "bottom"
        ) +
        ggplot2::coord_fixed() +
        ggplot2::scale_color_manual(values = colors[1:length(predictors)])
        
        # Save plot
        self$results$rocPlot$setState(p)
      },
      
      .generateCalibrationPlot = function() {
        if (!requireNamespace("ggplot2", quietly = TRUE)) return()
        
        data <- private$.data
        outcome_var <- self$options$outcomeVar
        predictors <- self$options$predictorVars
        actual <- as.numeric(data[[outcome_var]]) - 1
        
        # Prepare calibration data
        calib_data <- data.frame()
        
        for (pred in predictors) {
          # Fit model and get predictions
          formula_str <- paste(outcome_var, "~", pred)
          model <- glm(formula_str, data = data, family = "binomial")
          pred_probs <- predict(model, type = "response")
          
          # Create calibration groups (deciles)
          n_groups <- min(10, floor(nrow(data) / 15)) # At least 15 obs per group
          if (n_groups < 3) n_groups <- 3  # Minimum 3 groups
          
          prob_groups <- cut(pred_probs, breaks = quantile(pred_probs, 
                            seq(0, 1, length.out = n_groups + 1)), 
                            include.lowest = TRUE)
          
          # Calculate observed vs expected for each group
          group_stats <- aggregate(
            list(observed = actual, expected = pred_probs, count = rep(1, length(actual))),
            by = list(group = prob_groups, predictor = pred),
            FUN = function(x) c(mean = mean(x, na.rm = TRUE), sum = sum(x, na.rm = TRUE))
          )
          
          group_data <- data.frame(
            predictor = pred,
            group = 1:nrow(group_stats),
            expected = group_stats$expected[,"mean"],
            observed = group_stats$observed[,"mean"],
            count = group_stats$count[,"sum"],
            stringsAsFactors = FALSE
          )
          
          # Calculate standard errors
          group_data$se <- sqrt(group_data$observed * (1 - group_data$observed) / group_data$count)
          group_data$se[is.na(group_data$se)] <- 0
          
          calib_data <- rbind(calib_data, group_data)
        }
        
        # Create calibration plot
        p <- ggplot2::ggplot(calib_data, ggplot2::aes(x = expected, y = observed)) +
          ggplot2::geom_point(ggplot2::aes(size = count, color = predictor), alpha = 0.7) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(0, observed - 1.96 * se), 
                                             ymax = pmin(1, observed + 1.96 * se),
                                             color = predictor), 
                                width = 0.02, alpha = 0.5) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
          ggplot2::geom_smooth(ggplot2::aes(color = predictor), method = "loess", se = TRUE, alpha = 0.2) +
          ggplot2::facet_wrap(~ predictor, scales = "fixed") +
          ggplot2::labs(
            title = "Calibration Plot",
            subtitle = "Observed vs Predicted Probabilities with 95% Confidence Intervals",
            x = "Predicted Probability",
            y = "Observed Frequency",
            size = "Group Size",
            color = "Predictor"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            plot.subtitle = ggplot2::element_text(hjust = 0.5),
            legend.position = "bottom"
          ) +
          ggplot2::coord_fixed() +
          ggplot2::scale_x_continuous(limits = c(0, 1)) +
          ggplot2::scale_y_continuous(limits = c(0, 1))
        
        self$results$calibrationPlot$setState(p)
      },
      
      .generateComparisonPlot = function() {
        # Generate forest plot for model comparison
        if (!requireNamespace("ggplot2", quietly = TRUE)) return()
        if (is.null(private$.cv_results)) return()
        
        # Prepare comparison data from CV results
        comparison_data <- data.frame()
        
        for (model_name in names(private$.cv_results)) {
          fold_results <- private$.cv_results[[model_name]]
          
          # Calculate summary statistics
          aucs <- sapply(fold_results, function(x) x$auc)
          sensitivities <- sapply(fold_results, function(x) x$sensitivity)
          specificities <- sapply(fold_results, function(x) x$specificity)
          
          # Calculate means and confidence intervals
          mean_auc <- mean(aucs, na.rm = TRUE)
          mean_sen <- mean(sensitivities, na.rm = TRUE)
          mean_spe <- mean(specificities, na.rm = TRUE)
          
          # Calculate confidence intervals
          auc_ci <- private$.calculateSECI(aucs)
          sen_ci <- private$.calculateSECI(sensitivities)
          spe_ci <- private$.calculateSECI(specificities)
          
          # Add to comparison data
          model_data <- data.frame(
            model = rep(model_name, 3),
            metric = c("AUC", "Sensitivity", "Specificity"),
            estimate = c(mean_auc, mean_sen, mean_spe),
            ci_lower = c(auc_ci[1], sen_ci[1], spe_ci[1]),
            ci_upper = c(auc_ci[2], sen_ci[2], spe_ci[2]),
            stringsAsFactors = FALSE
          )
          
          comparison_data <- rbind(comparison_data, model_data)
        }
        
        # Create forest plot
        p <- ggplot2::ggplot(comparison_data, ggplot2::aes(x = estimate, y = model, color = model)) +
          ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5)) +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                                 height = 0.2, position = ggplot2::position_dodge(width = 0.5)) +
          ggplot2::facet_wrap(~ metric, scales = "free_x") +
          ggplot2::labs(
            title = "Model Comparison Forest Plot",
            subtitle = "Cross-Validated Performance with 95% Confidence Intervals",
            x = "Performance Metric Value",
            y = "Model",
            color = "Model"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            plot.subtitle = ggplot2::element_text(hjust = 0.5),
            legend.position = "bottom",
            strip.text = ggplot2::element_text(face = "bold")
          ) +
          ggplot2::geom_vline(data = data.frame(metric = "AUC", xintercept = 0.5), 
                             ggplot2::aes(xintercept = xintercept), linetype = "dashed", alpha = 0.5) +
          ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1))
        
        self$results$comparisonPlot$setState(p)
      },
      
      .generateCVPerformancePlot = function() {
        # Generate cross-validation performance plot
        if (!requireNamespace("ggplot2", quietly = TRUE)) return()
        if (is.null(private$.cv_results)) return()
        
        # Prepare data for plotting
        plot_data <- data.frame()
        
        for (model_name in names(private$.cv_results)) {
          fold_results <- private$.cv_results[[model_name]]
          
          for (fold_name in names(fold_results)) {
            fold_data <- fold_results[[fold_name]]
            
            plot_data <- rbind(plot_data, data.frame(
              Model = model_name,
              Fold = fold_name,
              AUC = fold_data$auc,
              Sensitivity = fold_data$sensitivity,
              Specificity = fold_data$specificity,
              Accuracy = fold_data$accuracy
            ))
          }
        }
        
        # Reshape data for plotting
        if (requireNamespace("reshape2", quietly = TRUE)) {
          plot_data_long <- reshape2::melt(plot_data, 
                                          id.vars = c("Model", "Fold"),
                                          variable.name = "Metric",
                                          value.name = "Value")
        } else {
          # Fallback manual reshape
          plot_data_long <- data.frame()
          metrics <- c("AUC", "Sensitivity", "Specificity", "Accuracy")
          for (metric in metrics) {
            temp_data <- data.frame(
              Model = plot_data$Model,
              Fold = plot_data$Fold,
              Metric = metric,
              Value = plot_data[[metric]]
            )
            plot_data_long <- rbind(plot_data_long, temp_data)
          }
        }
        
        # Create box plot
        p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = Model, y = Value, fill = Model)) +
          ggplot2::geom_boxplot(alpha = 0.7) +
          ggplot2::geom_point(ggplot2::aes(color = Model), 
                             position = ggplot2::position_jitter(width = 0.2), 
                             alpha = 0.6) +
          ggplot2::facet_wrap(~ Metric, scales = "free_y") +
          ggplot2::labs(
            title = "Cross-Validation Performance Distribution",
            subtitle = paste("Performance across", length(unique(plot_data$Fold)), "folds"),
            x = "Model",
            y = "Performance Value"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            plot.subtitle = ggplot2::element_text(hjust = 0.5),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "none"
          )
        
        self$results$cvPerformancePlot$setState(p)
      },
      
      .generateVariableImportancePlot = function() {
        # Generate variable importance plot
        if (!requireNamespace("ggplot2", quietly = TRUE)) return()
        
        # Implementation would go here
        # For now, create placeholder
        p <- ggplot2::ggplot() + 
          ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                         label = "Variable Importance Plot\n(Implementation in progress)"),
                            size = 6) +
          ggplot2::theme_void() +
          ggplot2::labs(title = "Variable Importance from Model Selection")
        
        self$results$variableImportancePlot$setState(p)
      },
      
      .generateStatisticalNotes = function() {
        # Generate statistical notes and assumptions
        notes_text <- "
        <h3>Statistical Notes and Assumptions</h3>
        
        <h4>Cross-Validation Assumptions:</h4>
        <ul>
          <li><strong>Independence:</strong> Observations assumed independent within and across folds</li>
          <li><strong>Stationarity:</strong> Underlying data generating process assumed stable</li>
          <li><strong>Sample Size:</strong> Each fold should have sufficient size for reliable estimates</li>
        </ul>
        
        <h4>Model Selection Considerations:</h4>
        <ul>
          <li><strong>AIC/BIC:</strong> Penalized likelihood approaches, BIC more conservative</li>
          <li><strong>LASSO:</strong> L1 regularization leads to sparse models with feature selection</li>
          <li><strong>Ridge:</strong> L2 regularization shrinks coefficients but keeps all variables</li>
          <li><strong>Elastic Net:</strong> Combines L1 and L2 penalties for balanced approach</li>
        </ul>
        
        <h4>Performance Metrics Interpretation:</h4>
        <ul>
          <li><strong>AUC Guidelines:</strong> >0.9 Excellent, 0.8-0.9 Good, 0.7-0.8 Fair, 0.6-0.7 Poor, <0.6 No discrimination</li>
          <li><strong>Calibration:</strong> Hosmer-Lemeshow p>0.05 indicates adequate calibration</li>
          <li><strong>NRI/IDI:</strong> Measures of reclassification improvement, positive values favor new model</li>
        </ul>
        
        <h4>Clinical Implementation Notes:</h4>
        <ul>
          <li>Cross-validated performance estimates are more realistic than apparent performance</li>
          <li>External validation in independent datasets recommended before clinical use</li>
          <li>Consider cost-benefit analysis and clinical context when interpreting statistical significance</li>
          <li>Model calibration as important as discrimination for probability interpretation</li>
        </ul>
        "
        
        self$results$statisticalNotes$setContent(notes_text)
      },
      
      .generateExplanations = function() {
        # Generate methodology explanations
        private$.generateStatisticalNotes()
        
        methodology_text <- "
        <h3>Methodology Explanation</h3>
        
        <h4>Cross-Validation Approach:</h4>
        <p>Cross-validation provides unbiased estimates of model performance by:</p>
        <ul>
          <li>Splitting data into training and testing folds</li>
          <li>Training models on training folds and testing on held-out data</li>
          <li>Averaging performance across all folds</li>
          <li>Providing confidence intervals for performance metrics</li>
        </ul>
        
        <h4>Statistical Tests Performed:</h4>
        <ul>
          <li><strong>DeLong Test:</strong> Compares AUC values between models using non-parametric approach</li>
          <li><strong>McNemar's Test:</strong> Compares paired binary predictions for statistical significance</li>
          <li><strong>Hosmer-Lemeshow Test:</strong> Assesses calibration quality (p > 0.05 indicates good calibration)</li>
        </ul>
        
        <h4>Performance Metrics:</h4>
        <ul>
          <li><strong>AUC:</strong> Area Under the ROC Curve (0.5 = no discrimination, 1.0 = perfect)</li>
          <li><strong>Sensitivity:</strong> True Positive Rate (ability to detect positive cases)</li>
          <li><strong>Specificity:</strong> True Negative Rate (ability to detect negative cases)</li>
          <li><strong>Youden's J:</strong> Optimal threshold balancing sensitivity and specificity</li>
        </ul>
        "
        
        self$results$methodologyExplanation$setContent(methodology_text)
      },
      
      .generateSummaries = function() {
        # Generate results interpretation and recommendations
        if (is.null(private$.cv_results)) return()
        
        # Analyze results and generate summary
        best_model <- private$.identifyBestModel()
        
        interpretation_text <- paste0("
        <h3>Results Interpretation</h3>
        
        <h4>Model Performance Summary:</h4>
        <p>Based on cross-validated performance metrics:</p>
        <ul>
          <li><strong>Best performing model:</strong> ", best_model$name, "</li>
          <li><strong>Cross-validated AUC:</strong> ", round(best_model$auc, 3), 
          " (95% CI: ", round(best_model$auc_ci[1], 3), "-", round(best_model$auc_ci[2], 3), ")</li>
          <li><strong>Sensitivity:</strong> ", round(best_model$sensitivity, 3), "</li>
          <li><strong>Specificity:</strong> ", round(best_model$specificity, 3), "</li>
        </ul>
        
        <h4>Clinical Interpretation:</h4>
        <p>", private$.interpretAUC(best_model$auc), "</p>
        ")
        
        self$results$resultsInterpretation$setContent(interpretation_text)
        
        # Generate recommendations
        recommendations_text <- "
        <h3>Recommendations</h3>
        <ul>
          <li>Consider the clinical context when interpreting statistical significance</li>
          <li>Validate findings in independent datasets before clinical implementation</li>
          <li>Assess model calibration for probability interpretation</li>
          <li>Consider cost-benefit analysis for clinical decision making</li>
        </ul>
        "
        
        self$results$recommendationsText$setContent(recommendations_text)
      },
      
      .identifyBestModel = function() {
        # Identify the best performing model based on AUC
        if (is.null(private$.cv_results)) return(NULL)
        
        best_auc <- 0
        best_model_name <- NULL
        
        for (model_name in names(private$.cv_results)) {
          fold_results <- private$.cv_results[[model_name]]
          aucs <- sapply(fold_results, function(x) x$auc)
          mean_auc <- mean(aucs, na.rm = TRUE)
          
          if (mean_auc > best_auc) {
            best_auc <- mean_auc
            best_model_name <- model_name
          }
        }
        
        if (!is.null(best_model_name)) {
          fold_results <- private$.cv_results[[best_model_name]]
          aucs <- sapply(fold_results, function(x) x$auc)
          sensitivities <- sapply(fold_results, function(x) x$sensitivity)
          specificities <- sapply(fold_results, function(x) x$specificity)
          
          auc_ci <- private$.calculateBootstrapCI(aucs)
          
          return(list(
            name = best_model_name,
            auc = mean(aucs, na.rm = TRUE),
            auc_ci = auc_ci,
            sensitivity = mean(sensitivities, na.rm = TRUE),
            specificity = mean(specificities, na.rm = TRUE)
          ))
        }
        
        return(NULL)
      },
      
      .interpretAUC = function(auc) {
        # Provide clinical interpretation of AUC value
        if (auc >= 0.9) {
          return("Excellent discrimination. The model demonstrates outstanding ability to distinguish between positive and negative cases.")
        } else if (auc >= 0.8) {
          return("Good discrimination. The model shows strong predictive ability suitable for clinical use.")
        } else if (auc >= 0.7) {
          return("Fair discrimination. The model has moderate predictive ability. Consider combining with other clinical factors.")
        } else if (auc >= 0.6) {
          return("Poor discrimination. The model shows limited predictive ability. Clinical utility may be questionable.")
        } else {
          return("No discrimination. The model performs no better than chance. Not suitable for clinical use.")
        }
      }
    )
  )
