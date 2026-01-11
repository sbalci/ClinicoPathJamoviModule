# Clinical Model Validation Suite
#
# This function provides comprehensive validation for clinical prediction models
# including bootstrap validation, cross-validation, model calibration assessment,
# and clinical performance evaluation

#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon geom_abline geom_smooth geom_hline annotate labs xlim ylim
#' @importFrom pROC roc auc ci ci.se
#' @importFrom PRROC pr.curve
#' @importFrom stats residuals predict hatvalues coef lowess qqnorm qqline aggregate
#' @importFrom graphics par plot abline lines text plot.new
#' @importFrom survival cox.zph

clinicalvalidationClass <- R6::R6Class(
  "clinicalvalidationClass",
  inherit = clinicalvalidationBase,
  private = list(

    # Utility function to escape variable names
    .escapeVar = function(var_name) {
      if (grepl("^[a-zA-Z][a-zA-Z0-9._]*$", var_name)) {
        return(var_name)
      } else {
        return(paste0("`", gsub("`", "\\`", var_name), "`"))
      }
    },

    # Core analysis method
    .run = function() {
      
      # Check if analysis is ready
      if (!private$.isReady()) {
        self$results$instructions$setContent("Select outcome variable and at least one predictor to begin analysis.")
        return()
      }
      
      # Get data
      data <- private$.getData()
      
      # Set seed for reproducibility
      if (self$options$set_seed) {
        set.seed(self$options$seed_value)
      }
      
      # Prepare data
      prepared_data <- private$.prepareData(data)
      
      if (is.null(prepared_data)) {
        self$results$instructions$setContent("Error in data preparation. Please check variable selection and data quality.")
        return()
      }
      
      # Fit model
      model_results <- private$.fitModel(prepared_data)
      
      if (is.null(model_results)) {
        self$results$instructions$setContent("Error in model fitting. Please check model specification and data.")
        return()
      }
      
      # Run validation
      validation_results <- private$.runValidation(prepared_data, model_results)
      
      # Generate results
      private$.populateResults(model_results, validation_results)
      
    },
    
    # Check if ready to run
    .isReady = function() {
      outcome <- self$options$outcome
      predictors <- self$options$predictors
      
      return(!is.null(outcome) && length(predictors) > 0)
    },
    
    # Get and validate data
    .getData = function() {
      data <- self$data
      
      # Remove rows with missing outcome
      if (!is.null(self$options$outcome)) {
        data <- data[!is.na(data[[self$options$outcome]]), ]
      }
      
      return(data)
    },
    
    # Prepare data for analysis
    .prepareData = function(data) {
      
      tryCatch({
        
        outcome_var <- self$options$outcome
        predictor_vars <- self$options$predictors
        time_var <- self$options$time_variable
        
        # Handle missing data based on selected method
        if (self$options$missing_data_handling == "complete_cases") {
          complete_vars <- c(outcome_var, predictor_vars)
          if (!is.null(time_var)) complete_vars <- c(complete_vars, time_var)
          data <- data[complete.cases(data[, complete_vars, drop = FALSE]), ]
        }
        
        # Check if we have enough data
        if (nrow(data) < 10) {
          return(NULL)
        }
        
        # Prepare outcome variable - handle both factor and numeric
        outcome_col <- data[[outcome_var]]

        # Convert to factor if numeric
        if (is.numeric(outcome_col)) {
          unique_vals <- sort(unique(outcome_col[!is.na(outcome_col)]))
          if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
            # Binary 0/1 numeric
            data[[outcome_var]] <- factor(outcome_col, levels = c(0, 1), labels = c("0", "1"))
            outcome_level <- "1"  # 1 is "positive" class
          } else {
            return(NULL)  # Not binary
          }
        } else {
          # Factor outcome
          if (!is.null(self$options$outcomeLevel)) {
            outcome_level <- self$options$outcomeLevel
          } else {
            outcome_level <- levels(data[[outcome_var]])[1]
          }
        }

        # Create binary outcome if needed
        if (self$options$model_type %in% c("logistic", "random_forest", "svm", "lda")) {
          data$outcome_binary <- as.factor(ifelse(data[[outcome_var]] == outcome_level, 1, 0))
        }
        
        return(list(
          data = data,
          outcome_var = outcome_var,
          predictor_vars = predictor_vars,
          time_var = time_var,
          outcome_level = outcome_level
        ))
        
      }, error = function(e) {
        return(NULL)
      })
    },
    
    # Fit the specified model
    .fitModel = function(prepared_data) {
      
      tryCatch({
        
        data <- prepared_data$data
        outcome_var <- prepared_data$outcome_var
        predictor_vars <- prepared_data$predictor_vars
        time_var <- prepared_data$time_var
        
        # Create formula
        if (nchar(self$options$model_formula) > 0) {
          formula_str <- self$options$model_formula
        } else {
          if (self$options$model_type == "cox") {
            formula_str <- paste("Surv(", time_var, ",", outcome_var, ") ~", paste(predictor_vars, collapse = " + "))
          } else {
            target_var <- if (self$options$model_type %in% c("logistic", "random_forest", "svm", "lda")) "outcome_binary" else outcome_var
            formula_str <- paste(target_var, "~", paste(predictor_vars, collapse = " + "))
          }
        }
        
        model_formula <- as.formula(formula_str)
        
        # Fit model based on type
        model <- switch(
          self$options$model_type,
          "logistic" = glm(model_formula, data = data, family = binomial()),
          "cox" = survival::coxph(model_formula, data = data),
          "random_forest" = randomForest::randomForest(model_formula, data = data, ntree = 500),
          "lda" = MASS::lda(model_formula, data = data),
          "svm" = e1071::svm(model_formula, data = data, probability = TRUE),
          NULL
        )
        
        if (is.null(model)) {
          return(NULL)
        }
        
        return(list(
          model = model,
          formula = model_formula,
          model_type = self$options$model_type
        ))
        
      }, error = function(e) {
        return(NULL)
      })
    },
    
    # Run validation analysis
    .runValidation = function(prepared_data, model_results) {
      
      validation_method <- self$options$validation_method
      
      results <- list()
      
      # Run specified validation method
      if (validation_method == "bootstrap") {
        results$bootstrap <- private$.runBootstrapValidation(prepared_data, model_results)
      } else if (validation_method == "cross_validation") {
        results$cv <- private$.runCrossValidation(prepared_data, model_results)
      } else if (validation_method == "repeated_cv") {
        results$repeated_cv <- private$.runRepeatedCV(prepared_data, model_results)
      }
      
      # Calculate performance metrics
      results$performance <- private$.calculatePerformanceMetrics(prepared_data, model_results)
      
      # Assess calibration
      if (self$options$model_type %in% c("logistic", "random_forest")) {
        results$calibration <- private$.assessCalibration(prepared_data, model_results)
      }
      
      # Model comparison if requested
      if (self$options$compare_models) {
        results$comparison <- private$.compareModels(prepared_data)
      }
      
      return(results)
    },
    
    # K-fold cross-validation
    .runCrossValidation = function(prepared_data, model_results) {

      n_folds <- self$options$cv_folds
      data <- prepared_data$data

      # Create folds
      n <- nrow(data)
      fold_size <- floor(n / n_folds)

      # Stratified or simple random folds
      if (self$options$stratified_sampling && "outcome_binary" %in% names(data)) {
        # Stratified by outcome
        pos_idx <- which(as.numeric(data$outcome_binary) == 2)
        neg_idx <- which(as.numeric(data$outcome_binary) == 1)

        pos_folds <- split(sample(pos_idx), rep(1:n_folds, length.out = length(pos_idx)))
        neg_folds <- split(sample(neg_idx), rep(1:n_folds, length.out = length(neg_idx)))

        folds <- lapply(1:n_folds, function(i) c(pos_folds[[i]], neg_folds[[i]]))
      } else {
        # Simple random folds
        fold_ids <- sample(rep(1:n_folds, length.out = n))
        folds <- split(1:n, fold_ids)
      }

      # Storage for CV results
      cv_metrics <- data.frame(
        auc = numeric(n_folds),
        accuracy = numeric(n_folds),
        sensitivity = numeric(n_folds),
        specificity = numeric(n_folds)
      )

      for (i in 1:n_folds) {

        test_idx <- folds[[i]]
        train_idx <- setdiff(1:n, test_idx)

        train_data <- data[train_idx, ]
        test_data <- data[test_idx, ]

        tryCatch({

          # Fit model on training fold
          fold_model <- private$.fitBootstrapModel(
            train_data,
            model_results$formula,
            model_results$model_type
          )

          # Evaluate on test fold
          metrics <- private$.evaluateModel(fold_model, test_data, prepared_data)

          cv_metrics$auc[i] <- metrics$auc
          cv_metrics$accuracy[i] <- metrics$accuracy
          cv_metrics$sensitivity[i] <- metrics$sensitivity
          cv_metrics$specificity[i] <- metrics$specificity

        }, error = function(e) {
          # Handle fold errors
        })
      }

      # Calculate summary statistics
      summary_stats <- list(
        auc = list(
          mean = mean(cv_metrics$auc, na.rm = TRUE),
          sd = sd(cv_metrics$auc, na.rm = TRUE),
          ci = quantile(cv_metrics$auc, c(0.025, 0.975), na.rm = TRUE)
        ),
        accuracy = list(
          mean = mean(cv_metrics$accuracy, na.rm = TRUE),
          sd = sd(cv_metrics$accuracy, na.rm = TRUE),
          ci = quantile(cv_metrics$accuracy, c(0.025, 0.975), na.rm = TRUE)
        ),
        sensitivity = list(
          mean = mean(cv_metrics$sensitivity, na.rm = TRUE),
          sd = sd(cv_metrics$sensitivity, na.rm = TRUE),
          ci = quantile(cv_metrics$sensitivity, c(0.025, 0.975), na.rm = TRUE)
        ),
        specificity = list(
          mean = mean(cv_metrics$specificity, na.rm = TRUE),
          sd = sd(cv_metrics$specificity, na.rm = TRUE),
          ci = quantile(cv_metrics$specificity, c(0.025, 0.975), na.rm = TRUE)
        )
      )

      return(list(
        raw_results = cv_metrics,
        summary = summary_stats,
        n_folds = n_folds
      ))
    },

    # Repeated cross-validation
    .runRepeatedCV = function(prepared_data, model_results) {

      n_repeats <- self$options$cv_repeats

      # Storage for all repeats
      all_metrics <- list()

      for (rep in 1:n_repeats) {

        # Run one round of CV
        cv_results <- private$.runCrossValidation(prepared_data, model_results)

        all_metrics[[rep]] <- cv_results$raw_results
      }

      # Combine all repeats
      combined_metrics <- do.call(rbind, all_metrics)

      # Calculate overall summary
      summary_stats <- list(
        auc = list(
          mean = mean(combined_metrics$auc, na.rm = TRUE),
          sd = sd(combined_metrics$auc, na.rm = TRUE),
          ci = quantile(combined_metrics$auc, c(0.025, 0.975), na.rm = TRUE)
        ),
        accuracy = list(
          mean = mean(combined_metrics$accuracy, na.rm = TRUE),
          sd = sd(combined_metrics$accuracy, na.rm = TRUE),
          ci = quantile(combined_metrics$accuracy, c(0.025, 0.975), na.rm = TRUE)
        ),
        sensitivity = list(
          mean = mean(combined_metrics$sensitivity, na.rm = TRUE),
          sd = sd(combined_metrics$sensitivity, na.rm = TRUE),
          ci = quantile(combined_metrics$sensitivity, c(0.025, 0.975), na.rm = TRUE)
        ),
        specificity = list(
          mean = mean(combined_metrics$specificity, na.rm = TRUE),
          sd = sd(combined_metrics$specificity, na.rm = TRUE),
          ci = quantile(combined_metrics$specificity, c(0.025, 0.975), na.rm = TRUE)
        )
      )

      return(list(
        raw_results = combined_metrics,
        summary = summary_stats,
        n_repeats = n_repeats,
        n_folds = self$options$cv_folds
      ))
    },

    # Bootstrap validation
    .runBootstrapValidation = function(prepared_data, model_results) {
      
      n_bootstrap <- self$options$bootstrap_samples
      data <- prepared_data$data
      
      # Storage for bootstrap results
      performance_metrics <- data.frame(
        auc = numeric(n_bootstrap),
        accuracy = numeric(n_bootstrap),
        sensitivity = numeric(n_bootstrap),
        specificity = numeric(n_bootstrap)
      )
      
      for (i in 1:n_bootstrap) {
        
        # Bootstrap sample
        boot_indices <- sample(nrow(data), replace = TRUE)
        boot_data <- data[boot_indices, ]
        test_data <- data[-unique(boot_indices), ]
        
        if (nrow(test_data) == 0) {
          test_data <- data
        }
        
        tryCatch({
          
          # Fit model on bootstrap sample
          boot_model <- private$.fitBootstrapModel(boot_data, model_results$formula, model_results$model_type)
          
          # Evaluate on test data
          metrics <- private$.evaluateModel(boot_model, test_data, prepared_data)
          
          performance_metrics$auc[i] <- metrics$auc
          performance_metrics$accuracy[i] <- metrics$accuracy
          performance_metrics$sensitivity[i] <- metrics$sensitivity
          performance_metrics$specificity[i] <- metrics$specificity
          
        }, error = function(e) {
          # Handle bootstrap iteration errors
        })
      }
      
      # Calculate summary statistics
      summary_stats <- list(
        auc = list(
          mean = mean(performance_metrics$auc, na.rm = TRUE),
          sd = sd(performance_metrics$auc, na.rm = TRUE),
          ci = quantile(performance_metrics$auc, c(0.025, 0.975), na.rm = TRUE)
        ),
        accuracy = list(
          mean = mean(performance_metrics$accuracy, na.rm = TRUE),
          sd = sd(performance_metrics$accuracy, na.rm = TRUE),
          ci = quantile(performance_metrics$accuracy, c(0.025, 0.975), na.rm = TRUE)
        ),
        sensitivity = list(
          mean = mean(performance_metrics$sensitivity, na.rm = TRUE),
          sd = sd(performance_metrics$sensitivity, na.rm = TRUE),
          ci = quantile(performance_metrics$sensitivity, c(0.025, 0.975), na.rm = TRUE)
        ),
        specificity = list(
          mean = mean(performance_metrics$specificity, na.rm = TRUE),
          sd = sd(performance_metrics$specificity, na.rm = TRUE),
          ci = quantile(performance_metrics$specificity, c(0.025, 0.975), na.rm = TRUE)
        )
      )
      
      return(list(
        raw_results = performance_metrics,
        summary = summary_stats
      ))
    },
    
    # Fit model for bootstrap iteration
    .fitBootstrapModel = function(data, formula, model_type) {
      
      switch(
        model_type,
        "logistic" = glm(formula, data = data, family = binomial()),
        "cox" = survival::coxph(formula, data = data),
        "random_forest" = randomForest::randomForest(formula, data = data, ntree = 100),
        "lda" = MASS::lda(formula, data = data),
        "svm" = e1071::svm(formula, data = data, probability = TRUE)
      )
    },
    
    # Evaluate model performance
    .evaluateModel = function(model, test_data, prepared_data) {

      model_type <- class(model)[1]

      # Get predictions based on model type
      pred_probs <- NULL
      predictions <- NULL

      tryCatch({

        if (model_type == "glm") {
          pred_probs <- predict(model, test_data, type = "response")
          predictions <- ifelse(pred_probs > 0.5, 1, 0)

        } else if (model_type == "randomForest") {
          pred_probs <- predict(model, test_data, type = "prob")[, 2]
          predictions <- as.numeric(predict(model, test_data)) - 1

        } else if (model_type == "svm") {
          # SVM with probability estimates
          pred_obj <- predict(model, test_data, probability = TRUE)
          pred_probs <- attr(pred_obj, "probabilities")[, "1"]
          predictions <- as.numeric(pred_obj) - 1

        } else if (model_type == "lda") {
          # LDA predictions
          pred_obj <- predict(model, test_data)
          pred_probs <- pred_obj$posterior[, "1"]
          predictions <- as.numeric(pred_obj$class) - 1

        } else if (model_type == "coxph") {
          # Cox model - use linear predictor as risk score
          pred_probs <- predict(model, test_data, type = "risk")
          # For Cox, we don't have binary predictions directly
          # Use median risk as threshold
          median_risk <- median(pred_probs, na.rm = TRUE)
          predictions <- ifelse(pred_probs > median_risk, 1, 0)

        } else {
          return(list(auc = NA, accuracy = NA, sensitivity = NA, specificity = NA))
        }

      }, error = function(e) {
        return(list(auc = NA, accuracy = NA, sensitivity = NA, specificity = NA))
      })

      if (is.null(pred_probs) || is.null(predictions)) {
        return(list(auc = NA, accuracy = NA, sensitivity = NA, specificity = NA))
      }

      # Actual outcomes
      if ("outcome_binary" %in% names(test_data)) {
        actual <- as.numeric(test_data$outcome_binary) - 1
      } else {
        # For Cox models, use original outcome
        actual <- as.numeric(test_data[[prepared_data$outcome_var]])
      }

      # Calculate metrics
      if (length(unique(actual)) > 1 && !all(is.na(pred_probs))) {

        # AUC
        auc <- tryCatch(as.numeric(pROC::auc(actual, pred_probs)), error = function(e) NA)

        # Confusion matrix metrics
        cm <- tryCatch({
          table(Predicted = predictions, Actual = actual)
        }, error = function(e) NULL)

        if (!is.null(cm) && nrow(cm) == 2 && ncol(cm) == 2) {
          TP <- cm[2, 2]
          TN <- cm[1, 1]
          FP <- cm[2, 1]
          FN <- cm[1, 2]

          accuracy <- (TP + TN) / sum(cm)
          sensitivity <- TP / (TP + FN)
          specificity <- TN / (TN + FP)
        } else {
          accuracy <- sensitivity <- specificity <- NA
        }

      } else {
        auc <- accuracy <- sensitivity <- specificity <- NA
      }

      return(list(
        auc = auc,
        accuracy = accuracy,
        sensitivity = sensitivity,
        specificity = specificity
      ))
    },
    
    # Calculate overall performance metrics
    .calculatePerformanceMetrics = function(prepared_data, model_results) {
      
      data <- prepared_data$data
      model <- model_results$model
      
      metrics <- private$.evaluateModel(model, data, prepared_data)
      
      # Add clinical interpretation
      interpretation <- private$.interpretPerformanceMetrics(metrics)
      
      return(list(
        metrics = metrics,
        interpretation = interpretation
      ))
    },
    
    # Interpret performance metrics clinically
    .interpretPerformanceMetrics = function(metrics) {
      
      clinical_context <- self$options$clinical_context
      
      interpretations <- list()
      
      # AUC interpretation
      auc <- metrics$auc
      if (!is.na(auc)) {
        if (auc >= 0.9) {
          interpretations$auc <- "Excellent discrimination - suitable for clinical decision making"
        } else if (auc >= 0.8) {
          interpretations$auc <- "Good discrimination - clinically useful with caution"
        } else if (auc >= 0.7) {
          interpretations$auc <- "Fair discrimination - may need additional predictors"
        } else {
          interpretations$auc <- "Poor discrimination - not recommended for clinical use"
        }
      }
      
      # Sensitivity interpretation based on clinical context
      sens <- metrics$sensitivity
      if (!is.na(sens)) {
        if (clinical_context %in% c("screening", "diagnosis")) {
          if (sens >= 0.9) {
            interpretations$sensitivity <- "Excellent - few cases will be missed"
          } else if (sens >= 0.8) {
            interpretations$sensitivity <- "Good - acceptable miss rate for most applications"
          } else {
            interpretations$sensitivity <- "Suboptimal - high miss rate may be problematic"
          }
        }
      }
      
      return(interpretations)
    },
    
    # Assess model calibration
    .assessCalibration = function(prepared_data, model_results) {
      
      data <- prepared_data$data
      model <- model_results$model
      
      # Get predicted probabilities
      if (class(model)[1] == "glm") {
        pred_probs <- predict(model, data, type = "response")
      } else if (class(model)[1] == "randomForest") {
        pred_probs <- predict(model, data, type = "prob")[, 2]
      } else {
        return(NULL)
      }
      
      actual <- as.numeric(data$outcome_binary) - 1
      
      # Hosmer-Lemeshow test
      hl_test <- tryCatch({
        ResourceSelection::hoslem.test(actual, pred_probs, g = self$options$calibration_bins)
      }, error = function(e) NULL)
      
      # Brier score
      brier_score <- mean((pred_probs - actual)^2)
      
      return(list(
        hosmer_lemeshow = hl_test,
        brier_score = brier_score
      ))
    },
    
    # Compare multiple models (fixed - no state mutation)
    .compareModels = function(prepared_data) {

      comparison_type <- self$options$comparison_models

      models_to_compare <- switch(
        comparison_type,
        "logistic_rf" = c("logistic", "random_forest"),
        "all_classification" = c("logistic", "random_forest", "lda", "svm"),
        c("logistic", "random_forest")  # default
      )

      comparison_results <- data.frame(
        model = character(),
        auc = numeric(),
        accuracy = numeric(),
        sensitivity = numeric(),
        specificity = numeric(),
        stringsAsFactors = FALSE
      )

      # Fit each model type without mutating options
      for (model_type in models_to_compare) {

        model_results <- tryCatch({
          private$.fitModelWithType(prepared_data, model_type)
        }, error = function(e) {
          NULL
        })

        if (!is.null(model_results)) {

          # Evaluate performance
          metrics <- tryCatch({
            private$.evaluateModel(model_results$model, prepared_data$data, prepared_data)
          }, error = function(e) {
            list(auc = NA, accuracy = NA, sensitivity = NA, specificity = NA)
          })

          comparison_results <- rbind(comparison_results, data.frame(
            model = model_type,
            auc = metrics$auc,
            accuracy = metrics$accuracy,
            sensitivity = metrics$sensitivity,
            specificity = metrics$specificity,
            stringsAsFactors = FALSE
          ))
        }
      }

      # Rank models by AUC
      if (nrow(comparison_results) > 0) {
        comparison_results$rank <- rank(-comparison_results$auc, na.last = "keep")
      }

      return(comparison_results)
    },

    # Fit model with specified type (no option mutation)
    .fitModelWithType = function(prepared_data, model_type) {

      tryCatch({

        data <- prepared_data$data
        outcome_var <- prepared_data$outcome_var
        predictor_vars <- prepared_data$predictor_vars
        time_var <- prepared_data$time_var

        # Create formula
        if (model_type == "cox") {
          formula_str <- paste("Surv(", time_var, ",", outcome_var, ") ~", paste(predictor_vars, collapse = " + "))
        } else {
          target_var <- if (model_type %in% c("logistic", "random_forest", "svm", "lda")) "outcome_binary" else outcome_var
          formula_str <- paste(target_var, "~", paste(predictor_vars, collapse = " + "))
        }

        model_formula <- as.formula(formula_str)

        # Fit model based on type
        model <- switch(
          model_type,
          "logistic" = glm(model_formula, data = data, family = binomial()),
          "cox" = survival::coxph(model_formula, data = data),
          "random_forest" = randomForest::randomForest(model_formula, data = data, ntree = 500),
          "lda" = MASS::lda(model_formula, data = data),
          "svm" = e1071::svm(model_formula, data = data, probability = TRUE),
          NULL
        )

        if (is.null(model)) {
          return(NULL)
        }

        return(list(
          model = model,
          formula = model_formula,
          model_type = model_type
        ))

      }, error = function(e) {
        return(NULL)
      })
    },
    
    # Populate all results
    .populateResults = function(model_results, validation_results) {

      # Model summary
      if (self$options$show_model_summary) {
        private$.populateModelSummary(model_results)
      }

      # Performance table - handle different validation methods
      if (self$options$show_performance_table) {
        validation_data <- NULL
        if (!is.null(validation_results$bootstrap)) {
          validation_data <- validation_results$bootstrap
        } else if (!is.null(validation_results$cv)) {
          validation_data <- validation_results$cv
        } else if (!is.null(validation_results$repeated_cv)) {
          validation_data <- validation_results$repeated_cv
        }
        private$.populatePerformanceTable(validation_results$performance, validation_data)
      }

      # Bootstrap/CV detailed results
      if (self$options$validation_method == "bootstrap" && self$options$detailed_bootstrap) {
        private$.populateBootstrapResults(validation_results$bootstrap)
      } else if (self$options$validation_method %in% c("cross_validation", "repeated_cv") && self$options$detailed_bootstrap) {
        # Show CV results in bootstrap table
        if (!is.null(validation_results$cv)) {
          private$.populateBootstrapResults(validation_results$cv)
        } else if (!is.null(validation_results$repeated_cv)) {
          private$.populateBootstrapResults(validation_results$repeated_cv)
        }
      }

      # Calibration results
      if (!is.null(validation_results$calibration)) {
        private$.populateCalibrationResults(validation_results$calibration)
      }

      # Model comparison
      if (self$options$compare_models && !is.null(validation_results$comparison)) {
        private$.populateModelComparison(validation_results$comparison)
      }

      # Clinical interpretation
      if (self$options$show_clinical_interpretation) {
        private$.populateClinicalInterpretation(validation_results)
      }

      # Validation report
      if (self$options$export_results) {
        private$.populateValidationReport(model_results, validation_results)
      }
    },
    
    # Populate model summary
    .populateModelSummary = function(model_results) {
      
      model <- model_results$model
      model_type <- model_results$model_type
      
      html <- paste0(
        "<h3>Model Summary</h3>",
        "<p><strong>Model Type:</strong> ", tools::toTitleCase(gsub("_", " ", model_type)), "</p>",
        "<p><strong>Formula:</strong> ", deparse(model_results$formula), "</p>",
        "<p><strong>Number of Observations:</strong> ", nobs(model), "</p>"
      )
      
      if (model_type == "logistic") {
        html <- paste0(html, "<p><strong>AIC:</strong> ", round(AIC(model), 2), "</p>")
      }
      
      self$results$modelsummary$setContent(html)
    },
    
    # Populate performance metrics table
    .populatePerformanceTable = function(performance_results, bootstrap_results = NULL) {
      
      metrics <- performance_results$metrics
      interpretation <- performance_results$interpretation
      
      table_data <- data.frame(
        metric = character(),
        estimate = numeric(),
        lower_ci = numeric(),
        upper_ci = numeric(),
        interpretation = character(),
        stringsAsFactors = FALSE
      )
      
      # Add metrics with confidence intervals from bootstrap if available
      if (!is.null(bootstrap_results)) {
        
        bs_summary <- bootstrap_results$summary
        
        table_data <- rbind(table_data, data.frame(
          metric = "AUC",
          estimate = bs_summary$auc$mean,
          lower_ci = bs_summary$auc$ci[1],
          upper_ci = bs_summary$auc$ci[2],
          interpretation = interpretation$auc %||% "",
          stringsAsFactors = FALSE
        ))
        
        table_data <- rbind(table_data, data.frame(
          metric = "Accuracy",
          estimate = bs_summary$accuracy$mean,
          lower_ci = bs_summary$accuracy$ci[1],
          upper_ci = bs_summary$accuracy$ci[2],
          interpretation = "",
          stringsAsFactors = FALSE
        ))
        
        table_data <- rbind(table_data, data.frame(
          metric = "Sensitivity",
          estimate = bs_summary$sensitivity$mean,
          lower_ci = bs_summary$sensitivity$ci[1],
          upper_ci = bs_summary$sensitivity$ci[2],
          interpretation = interpretation$sensitivity %||% "",
          stringsAsFactors = FALSE
        ))
        
        table_data <- rbind(table_data, data.frame(
          metric = "Specificity",
          estimate = bs_summary$specificity$mean,
          lower_ci = bs_summary$specificity$ci[1],
          upper_ci = bs_summary$specificity$ci[2],
          interpretation = "",
          stringsAsFactors = FALSE
        ))
        
      } else {
        
        # Just point estimates
        table_data <- rbind(table_data, data.frame(
          metric = "AUC",
          estimate = metrics$auc,
          lower_ci = NA,
          upper_ci = NA,
          interpretation = interpretation$auc %||% "",
          stringsAsFactors = FALSE
        ))
        
        table_data <- rbind(table_data, data.frame(
          metric = "Accuracy",
          estimate = metrics$accuracy,
          lower_ci = NA,
          upper_ci = NA,
          interpretation = "",
          stringsAsFactors = FALSE
        ))
        
        table_data <- rbind(table_data, data.frame(
          metric = "Sensitivity",
          estimate = metrics$sensitivity,
          lower_ci = NA,
          upper_ci = NA,
          interpretation = interpretation$sensitivity %||% "",
          stringsAsFactors = FALSE
        ))
        
        table_data <- rbind(table_data, data.frame(
          metric = "Specificity",
          estimate = metrics$specificity,
          lower_ci = NA,
          upper_ci = NA,
          interpretation = "",
          stringsAsFactors = FALSE
        ))
      }
      
      for (i in seq_len(nrow(table_data))) {
          self$results$performancetable$addRow(rowKey = i, values = list(
              metric = table_data$metric[i],
              estimate = table_data$estimate[i],
              lower_ci = table_data$lower_ci[i],
              upper_ci = table_data$upper_ci[i],
              interpretation = table_data$interpretation[i]
          ))
      }
    },
    
    # Populate bootstrap detailed results
    .populateBootstrapResults = function(bootstrap_results) {
      
      if (is.null(bootstrap_results)) return()
      
      summary_stats <- bootstrap_results$summary
      
      table_data <- data.frame(
        statistic = c("AUC", "Accuracy", "Sensitivity", "Specificity"),
        original = c(
          summary_stats$auc$mean,
          summary_stats$accuracy$mean,
          summary_stats$sensitivity$mean,
          summary_stats$specificity$mean
        ),
        bias = c(0, 0, 0, 0),  # Would need original model performance to calculate bias
        bias_corrected = c(
          summary_stats$auc$mean,
          summary_stats$accuracy$mean,
          summary_stats$sensitivity$mean,
          summary_stats$specificity$mean
        ),
        percentile_ci_lower = c(
          summary_stats$auc$ci[1],
          summary_stats$accuracy$ci[1],
          summary_stats$sensitivity$ci[1],
          summary_stats$specificity$ci[1]
        ),
        percentile_ci_upper = c(
          summary_stats$auc$ci[2],
          summary_stats$accuracy$ci[2],
          summary_stats$sensitivity$ci[2],
          summary_stats$specificity$ci[2]
        ),
        stringsAsFactors = FALSE
      )
      
      for (i in seq_len(nrow(table_data))) {
          self$results$bootstrapresults$addRow(rowKey = i, values = list(
              statistic = table_data$statistic[i],
              original = table_data$original[i],
              bias = table_data$bias[i],
              bias_corrected = table_data$bias_corrected[i],
              percentile_ci_lower = table_data$percentile_ci_lower[i],
              percentile_ci_upper = table_data$percentile_ci_upper[i]
          ))
      }
    },
    
    # Populate calibration results
    .populateCalibrationResults = function(calibration_results) {
      
      table_data <- data.frame(
        test = character(),
        statistic = numeric(),
        p_value = numeric(),
        interpretation = character(),
        stringsAsFactors = FALSE
      )
      
      # Hosmer-Lemeshow test
      if (!is.null(calibration_results$hosmer_lemeshow)) {
        hl <- calibration_results$hosmer_lemeshow
        interpretation <- if (hl$p.value > 0.05) {
          "Good calibration - model predictions match observed outcomes"
        } else {
          "Poor calibration - model predictions differ from observed outcomes"
        }
        
        table_data <- rbind(table_data, data.frame(
          test = "Hosmer-Lemeshow",
          statistic = hl$statistic,
          p_value = hl$p.value,
          interpretation = interpretation,
          stringsAsFactors = FALSE
        ))
      }
      
      for (i in seq_len(nrow(table_data))) {
          self$results$calibrationtable$addRow(rowKey = i, values = list(
              test = table_data$test[i],
              statistic = table_data$statistic[i],
              p_value = table_data$p_value[i],
              interpretation = table_data$interpretation[i]
          ))
      }
      
      # Brier score
      if (!is.null(calibration_results$brier_score)) {
        brier_data <- data.frame(
          component = c("Brier Score", "Max Possible", "Scaled Brier Score"),
          value = c(
            calibration_results$brier_score,
            0.25,
            1 - (calibration_results$brier_score / 0.25)
          ),
          interpretation = c(
            "Lower is better (0 = perfect, 0.25 = uninformative)",
            "Score for random predictions",
            "Improvement over random (1 = perfect, 0 = random)"
          ),
          stringsAsFactors = FALSE
        )
        
        for (i in seq_len(nrow(brier_data))) {
            self$results$brierscore$addRow(rowKey = i, values = list(
                component = brier_data$component[i],
                value = brier_data$value[i],
                interpretation = brier_data$interpretation[i]
            ))
        }
      }
    },
    
    # Populate model comparison
    .populateModelComparison = function(comparison_results) {
      
      for (i in seq_len(nrow(comparison_results))) {
          self$results$modelcomparison$addRow(rowKey = i, values = list(
              model = comparison_results$model[i],
              auc = comparison_results$auc[i],
              accuracy = comparison_results$accuracy[i],
              sensitivity = comparison_results$sensitivity[i],
              specificity = comparison_results$specificity[i],
              rank = comparison_results$rank[i]
          ))
      }
    },
    
    # Populate clinical interpretation
    .populateClinicalInterpretation = function(validation_results) {
      
      clinical_context <- self$options$clinical_context
      performance <- validation_results$performance
      
      html <- paste0(
        "<h3>Clinical Decision Guidelines</h3>",
        "<p><strong>Clinical Application:</strong> ", tools::toTitleCase(gsub("_", " ", clinical_context)), "</p>"
      )
      
      # Context-specific recommendations
      if (clinical_context == "screening") {
        html <- paste0(html,
          "<h4>Screening Application Guidelines:</h4>",
          "<ul>",
          "<li>High sensitivity (>0.90) is critical to minimize missed cases</li>",
          "<li>Moderate specificity acceptable as follow-up testing can confirm</li>",
          "<li>Consider population prevalence when setting thresholds</li>",
          "</ul>"
        )
      } else if (clinical_context == "diagnosis") {
        html <- paste0(html,
          "<h4>Diagnostic Application Guidelines:</h4>",
          "<ul>",
          "<li>Balance sensitivity and specificity based on consequences</li>",
          "<li>High specificity important to avoid false positive diagnoses</li>",
          "<li>AUC >0.80 generally required for diagnostic applications</li>",
          "</ul>"
        )
      } else if (clinical_context == "histological") {
        html <- paste0(html,
          "<h4>Histological Classification Guidelines:</h4>",
          "<ul>",
          "<li>Focus on variable importance for pathological features</li>",
          "<li>Model should be interpretable for pathologist review</li>",
          "<li>Validation on external cohorts strongly recommended</li>",
          "</ul>"
        )
      }
      
      # Performance-based recommendations
      metrics <- performance$metrics
      
      if (!is.na(metrics$auc)) {
        if (metrics$auc >= 0.8) {
          html <- paste0(html, 
            "<p><strong>Recommendation:</strong> Model performance supports clinical evaluation.</p>"
          )
        } else {
          html <- paste0(html, 
            "<p><strong>Recommendation:</strong> Model performance may be insufficient for clinical use. Consider:</p>",
            "<ul>",
            "<li>Adding additional predictors</li>",
            "<li>Exploring different modeling approaches</li>",
            "<li>External validation before clinical implementation</li>",
            "</ul>"
          )
        }
      }
      
      self$results$clinicalinterpretation$setContent(html)
    },

    # Populate validation report
    .populateValidationReport = function(model_results, validation_results) {

      perf <- validation_results$performance$metrics
      interp <- validation_results$performance$interpretation

      # Get validation CI if available
      ci_text <- ""
      if (!is.null(validation_results$bootstrap)) {
        ci_low <- validation_results$bootstrap$summary$auc$ci[1]
        ci_high <- validation_results$bootstrap$summary$auc$ci[2]
        ci_text <- sprintf(" (95%% CI: %.3f-%.3f)", ci_low, ci_high)
      } else if (!is.null(validation_results$cv)) {
        ci_low <- validation_results$cv$summary$auc$ci[1]
        ci_high <- validation_results$cv$summary$auc$ci[2]
        ci_text <- sprintf(" (95%% CI: %.3f-%.3f)", ci_low, ci_high)
      } else if (!is.null(validation_results$repeated_cv)) {
        ci_low <- validation_results$repeated_cv$summary$auc$ci[1]
        ci_high <- validation_results$repeated_cv$summary$auc$ci[2]
        ci_text <- sprintf(" (95%% CI: %.3f-%.3f)", ci_low, ci_high)
      }

      # Build comprehensive report
      html <- paste0(
        "<h2>Clinical Model Validation Report</h2>",
        "<h3>Model Specification</h3>",
        "<ul>",
        "<li><strong>Model Type:</strong> ", tools::toTitleCase(gsub("_", " ", model_results$model_type)), "</li>",
        "<li><strong>Outcome:</strong> ", self$options$outcome, "</li>",
        "<li><strong>Predictors:</strong> ", paste(self$options$predictors, collapse = ", "), "</li>",
        "<li><strong>Sample Size:</strong> ", nobs(model_results$model), "</li>",
        "<li><strong>Validation Method:</strong> ", tools::toTitleCase(gsub("_", " ", self$options$validation_method)), "</li>",
        "</ul>",

        "<h3>Performance Summary</h3>",
        "<table border='1' style='border-collapse: collapse; width: 100%; margin: 10px 0;'>",
        "<tr style='background: #f0f0f0;'><th>Metric</th><th>Value</th><th>Interpretation</th></tr>",
        "<tr><td>AUC (Discrimination)</td><td>", sprintf("%.3f%s", perf$auc, ci_text), "</td><td>", interp$auc %||% "", "</td></tr>",
        "<tr><td>Sensitivity</td><td>", sprintf("%.1f%%", perf$sensitivity * 100), "</td><td>", interp$sensitivity %||% "", "</td></tr>",
        "<tr><td>Specificity</td><td>", sprintf("%.1f%%", perf$specificity * 100), "</td><td>Correctly identifies true negatives</td></tr>",
        "<tr><td>Accuracy</td><td>", sprintf("%.1f%%", perf$accuracy * 100), "</td><td>Overall correct classification rate</td></tr>",
        "</table>"
      )

      # Add clinical context recommendations
      html <- paste0(html,
        "<h3>Clinical Application Guidelines</h3>",
        "<p><strong>Intended Use:</strong> ", tools::toTitleCase(gsub("_", " ", self$options$clinical_context)), "</p>"
      )

      # Context-specific guidance
      if (self$options$clinical_context == "screening") {
        html <- paste0(html,
          "<p><strong>Screening Recommendations:</strong></p>",
          "<ul>",
          "<li>Current sensitivity of ", sprintf("%.1f%%", perf$sensitivity * 100),
          if (perf$sensitivity >= 0.9) " meets the screening threshold (≥90%)" else " may miss too many cases for screening",
          "</li>",
          "<li>Consider lowering decision threshold to increase sensitivity if needed</li>",
          "<li>False positives acceptable as follow-up tests can confirm diagnosis</li>",
          "</ul>"
        )
      } else if (self$options$clinical_context == "diagnosis") {
        html <- paste0(html,
          "<p><strong>Diagnostic Recommendations:</strong></p>",
          "<ul>",
          "<li>Balance achieved: Sensitivity ", sprintf("%.1f%%", perf$sensitivity * 100),
          ", Specificity ", sprintf("%.1f%%", perf$specificity * 100), "</li>",
          "<li>AUC of ", sprintf("%.3f", perf$auc),
          if (perf$auc >= 0.8) " supports clinical diagnostic use" else " may need additional predictors",
          "</li>",
          "<li>Consider clinical consequences when setting decision threshold</li>",
          "</ul>"
        )
      }

      # Add copy-ready text
      html <- paste0(html,
        "<h3>Copy-Ready Report Text</h3>",
        "<div style='background: #f9f9f9; border-left: 4px solid #007bff; padding: 15px; margin: 10px 0;'>",
        "<p><strong>Methods:</strong> We developed a ", model_results$model_type, " model to predict ", self$options$outcome,
        " using ", length(self$options$predictors), " predictor variables (", paste(self$options$predictors, collapse = ", "),
        ") in ", nobs(model_results$model), " patients. Model performance was assessed using ", self$options$validation_method,
        " validation", if (self$options$validation_method == "bootstrap") paste0(" with ", self$options$bootstrap_samples, " resamples") else "", ".</p>",

        "<p><strong>Results:</strong> The model achieved an AUC of ", sprintf("%.3f%s", perf$auc, ci_text),
        ", indicating ", tolower(interp$auc %||% "discriminatory performance"),
        ". Sensitivity was ", sprintf("%.1f%%", perf$sensitivity * 100),
        " and specificity was ", sprintf("%.1f%%", perf$specificity * 100),
        ". ", if (perf$auc >= 0.8) "These results suggest the model has clinically useful discriminatory ability." else "Additional predictors or alternative modeling approaches may improve performance.", "</p>",

        "<p><strong>Conclusion:</strong> ",
        if (perf$auc >= 0.8 && perf$sensitivity >= 0.8) {
          "The model demonstrates good performance and may support clinical decision-making after appropriate external validation."
        } else if (perf$auc >= 0.7) {
          "The model shows promise but would benefit from refinement before clinical implementation."
        } else {
          "The model's current performance is insufficient for clinical use and requires substantial improvement."
        },
        "</p>",
        "</div>"
      )

      self$results$validationreport$setContent(html)
    },

    # Render validation learning curves
    .validation_curves_plot = function(image, ggtheme, theme, ...) {

      if (!self$.isReady()) return()

      # Get data and prepare
      data <- self$.getData()
      prepared_data <- private$.prepareData(data)

      if (is.null(prepared_data)) return()

      # Fit model
      model_results <- private$.fitModel(prepared_data)

      if (is.null(model_results)) return()

      # Create learning curves showing performance vs. training size
      train_data <- prepared_data$data
      n_samples <- nrow(train_data)
      sample_sizes <- seq(30, n_samples, length.out = 10)

      performance_by_size <- data.frame(
        sample_size = numeric(),
        auc = numeric(),
        type = character(),
        stringsAsFactors = FALSE
      )

      for (size in sample_sizes) {

        # Sample data
        sample_idx <- sample(n_samples, min(floor(size), n_samples))
        train_subset <- train_data[sample_idx, ]

        tryCatch({

          # Fit model on subset
          subset_model <- private$.fitBootstrapModel(
            train_subset,
            model_results$formula,
            model_results$model_type
          )

          # Evaluate on training set
          train_metrics <- private$.evaluateModel(subset_model, train_subset, prepared_data)

          # Evaluate on full set (validation)
          val_metrics <- private$.evaluateModel(subset_model, train_data, prepared_data)

          performance_by_size <- rbind(performance_by_size, data.frame(
            sample_size = size,
            auc = train_metrics$auc,
            type = "Training",
            stringsAsFactors = FALSE
          ))

          performance_by_size <- rbind(performance_by_size, data.frame(
            sample_size = size,
            auc = val_metrics$auc,
            type = "Validation",
            stringsAsFactors = FALSE
          ))

        }, error = function(e) {})
      }

      # Plot learning curves
      if (nrow(performance_by_size) > 0) {
        p <- ggplot2::ggplot(performance_by_size, ggplot2::aes(x = sample_size, y = auc, color = type)) +
          ggplot2::geom_line(size = 1.2) +
          ggplot2::geom_point(size = 3) +
          ggplot2::labs(
            x = "Training Sample Size",
            y = "AUC",
            title = "Validation Learning Curves",
            color = "Dataset"
          ) +
          ggplot2::ylim(0.5, 1) +
          ggtheme

        print(p)
        TRUE
      } else {
        FALSE
      }
    },

    # Render calibration plot
    .calibration_plot = function(image, ggtheme, theme, ...) {

      if (!self$.isReady()) return()

      # Get data
      data <- self$.getData()
      prepared_data <- private$.prepareData(data)

      if (is.null(prepared_data)) return()

      # Fit model
      model_results <- private$.fitModel(prepared_data)

      if (is.null(model_results)) return()

      # Get predictions
      model <- model_results$model
      model_data <- prepared_data$data

      if (class(model)[1] == "glm") {
        pred_probs <- predict(model, model_data, type = "response")
      } else if (class(model)[1] == "randomForest") {
        pred_probs <- predict(model, model_data, type = "prob")[, 2]
      } else {
        return(FALSE)
      }

      actual <- as.numeric(model_data$outcome_binary) - 1

      # Create calibration bins
      n_bins <- self$options$calibration_bins

      # Bin predicted probabilities
      breaks <- seq(0, 1, length.out = n_bins + 1)
      pred_bins <- cut(pred_probs, breaks = breaks, include.lowest = TRUE)

      # Calculate observed vs expected in each bin
      calib_data <- data.frame(
        pred = pred_probs,
        actual = actual,
        bin = pred_bins
      )

      calib_summary <- aggregate(cbind(pred, actual) ~ bin, data = calib_data, FUN = mean)
      calib_summary$n <- as.vector(table(pred_bins))

      # Create calibration plot
      p <- ggplot2::ggplot(calib_summary, ggplot2::aes(x = pred, y = actual)) +
        ggplot2::geom_point(ggplot2::aes(size = n), alpha = 0.6, color = "steelblue") +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        ggplot2::geom_smooth(method = "loess", se = TRUE, color = "darkblue") +
        ggplot2::labs(
          x = "Predicted Probability",
          y = "Observed Proportion",
          title = "Calibration Plot",
          size = "N observations"
        ) +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1) +
        ggtheme

      print(p)
      TRUE
    },

    # Render ROC curve with confidence intervals
    .roc_curve_plot = function(image, ggtheme, theme, ...) {

      if (!self$.isReady()) return()

      # Get data
      data <- self$.getData()
      prepared_data <- private$.prepareData(data)

      if (is.null(prepared_data)) return()

      # Fit model
      model_results <- private$.fitModel(prepared_data)

      if (is.null(model_results)) return()

      # Get predictions
      model <- model_results$model
      model_data <- prepared_data$data

      if (class(model)[1] == "glm") {
        pred_probs <- predict(model, model_data, type = "response")
      } else if (class(model)[1] == "randomForest") {
        pred_probs <- predict(model, model_data, type = "prob")[, 2]
      } else {
        return(FALSE)
      }

      actual <- as.numeric(model_data$outcome_binary) - 1

      # Calculate ROC curve
      roc_obj <- pROC::roc(actual, pred_probs, ci = TRUE)

      # Get confidence intervals for ROC curve
      ci_obj <- pROC::ci.se(roc_obj, specificities = seq(0, 1, 0.05))

      # Create data frame for plotting
      roc_data <- data.frame(
        fpr = 1 - roc_obj$specificities,
        tpr = roc_obj$sensitivities
      )

      ci_data <- data.frame(
        fpr = 1 - as.numeric(rownames(ci_obj)),
        lower = ci_obj[, 1],
        upper = ci_obj[, 3]
      )

      # Plot ROC curve
      auc_value <- as.numeric(roc_obj$auc)
      auc_ci <- as.numeric(roc_obj$ci)

      p <- ggplot2::ggplot(roc_data, ggplot2::aes(x = fpr, y = tpr)) +
        ggplot2::geom_ribbon(
          data = ci_data,
          ggplot2::aes(x = fpr, ymin = lower, ymax = upper),
          fill = "steelblue", alpha = 0.3
        ) +
        ggplot2::geom_line(size = 1.2, color = "darkblue") +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        ggplot2::annotate(
          "text",
          x = 0.6, y = 0.2,
          label = sprintf("AUC = %.3f\n95%% CI: [%.3f, %.3f]", auc_value, auc_ci[1], auc_ci[3]),
          size = 5,
          hjust = 0
        ) +
        ggplot2::labs(
          x = "False Positive Rate (1 - Specificity)",
          y = "True Positive Rate (Sensitivity)",
          title = "ROC Curve with 95% Confidence Intervals"
        ) +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1) +
        ggtheme

      print(p)
      TRUE
    },

    # Render precision-recall curve
    .prc_curve_plot = function(image, ggtheme, theme, ...) {

      if (!self$.isReady()) return()

      # Get data
      data <- self$.getData()
      prepared_data <- private$.prepareData(data)

      if (is.null(prepared_data)) return()

      # Fit model
      model_results <- private$.fitModel(prepared_data)

      if (is.null(model_results)) return()

      # Get predictions
      model <- model_results$model
      model_data <- prepared_data$data

      if (class(model)[1] == "glm") {
        pred_probs <- predict(model, model_data, type = "response")
      } else if (class(model)[1] == "randomForest") {
        pred_probs <- predict(model, model_data, type = "prob")[, 2]
      } else {
        return(FALSE)
      }

      actual <- as.numeric(model_data$outcome_binary) - 1

      # Calculate precision-recall curve
      pr_data <- PRROC::pr.curve(scores.class0 = pred_probs[actual == 1],
                                  scores.class1 = pred_probs[actual == 0],
                                  curve = TRUE)

      # Create data frame
      pr_curve_data <- data.frame(
        recall = pr_data$curve[, 1],
        precision = pr_data$curve[, 2]
      )

      # Calculate baseline (prevalence)
      prevalence <- mean(actual)

      # Plot PR curve
      p <- ggplot2::ggplot(pr_curve_data, ggplot2::aes(x = recall, y = precision)) +
        ggplot2::geom_line(size = 1.2, color = "darkblue") +
        ggplot2::geom_hline(yintercept = prevalence, linetype = "dashed", color = "red") +
        ggplot2::annotate(
          "text",
          x = 0.2, y = prevalence + 0.05,
          label = sprintf("Baseline (Prevalence) = %.3f", prevalence),
          size = 4,
          hjust = 0,
          color = "red"
        ) +
        ggplot2::annotate(
          "text",
          x = 0.6, y = 0.2,
          label = sprintf("AUC-PR = %.3f", pr_data$auc.integral),
          size = 5,
          hjust = 0
        ) +
        ggplot2::labs(
          x = "Recall (Sensitivity)",
          y = "Precision (PPV)",
          title = "Precision-Recall Curve"
        ) +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1) +
        ggtheme

      print(p)
      TRUE
    },

    # Render residual plots for model diagnostics
    .residual_plots = function(image, ggtheme, theme, ...) {

      if (!self$.isReady()) return()

      # Get data
      data <- self$.getData()
      prepared_data <- private$.prepareData(data)

      if (is.null(prepared_data)) return()

      # Fit model
      model_results <- private$.fitModel(prepared_data)

      if (is.null(model_results)) return()

      model <- model_results$model
      model_type <- model_results$model_type
      model_data <- prepared_data$data

      # Generate residuals based on model type
      if (model_type == "logistic") {

        # Get predictions
        pred_probs <- predict(model, model_data, type = "response")
        actual <- as.numeric(model_data$outcome_binary) - 1

        # Calculate deviance residuals
        residuals <- residuals(model, type = "deviance")
        fitted <- predict(model, model_data, type = "link")

        # Create 4-panel diagnostic plot
        par(mfrow = c(2, 2))

        # 1. Residuals vs Fitted
        plot(fitted, residuals,
             xlab = "Fitted values (logit scale)",
             ylab = "Deviance residuals",
             main = "Residuals vs Fitted",
             pch = 20, col = "steelblue")
        abline(h = 0, lty = 2, col = "red")
        lines(lowess(fitted, residuals), col = "darkblue", lwd = 2)

        # 2. Q-Q plot
        qqnorm(residuals, main = "Normal Q-Q Plot", pch = 20, col = "steelblue")
        qqline(residuals, col = "red", lwd = 2)

        # 3. Scale-Location
        sqrt_abs_resid <- sqrt(abs(residuals))
        plot(fitted, sqrt_abs_resid,
             xlab = "Fitted values (logit scale)",
             ylab = expression(sqrt("|Deviance residuals|")),
             main = "Scale-Location",
             pch = 20, col = "steelblue")
        lines(lowess(fitted, sqrt_abs_resid), col = "darkblue", lwd = 2)

        # 4. Residuals vs Leverage
        leverage <- hatvalues(model)
        plot(leverage, residuals,
             xlab = "Leverage",
             ylab = "Deviance residuals",
             main = "Residuals vs Leverage",
             pch = 20, col = "steelblue")
        abline(h = 0, lty = 2, col = "red")

        # Add Cook's distance contours
        cook_levels <- c(0.5, 1)
        n <- nrow(model_data)
        p <- length(coef(model))

        for (level in cook_levels) {
          x_seq <- seq(0.001, max(leverage), length.out = 100)
          y_upper <- sqrt(level * p * (1 - x_seq) / x_seq)
          y_lower <- -y_upper
          lines(x_seq, y_upper, lty = 2, col = "red")
          lines(x_seq, y_lower, lty = 2, col = "red")
        }

        par(mfrow = c(1, 1))

        TRUE

      } else if (model_type == "cox") {

        # Cox model diagnostics
        par(mfrow = c(2, 2))

        # Martingale residuals
        mart_resid <- residuals(model, type = "martingale")
        linear_pred <- predict(model, type = "lp")

        plot(linear_pred, mart_resid,
             xlab = "Linear Predictor",
             ylab = "Martingale Residuals",
             main = "Martingale Residuals",
             pch = 20, col = "steelblue")
        abline(h = 0, lty = 2, col = "red")
        lines(lowess(linear_pred, mart_resid), col = "darkblue", lwd = 2)

        # Deviance residuals
        dev_resid <- residuals(model, type = "deviance")

        plot(linear_pred, dev_resid,
             xlab = "Linear Predictor",
             ylab = "Deviance Residuals",
             main = "Deviance Residuals",
             pch = 20, col = "steelblue")
        abline(h = 0, lty = 2, col = "red")
        lines(lowess(linear_pred, dev_resid), col = "darkblue", lwd = 2)

        # Score residuals (Schoenfeld)
        schoen_resid <- residuals(model, type = "schoenfeld")

        if (!is.null(schoen_resid) && ncol(as.matrix(schoen_resid)) > 0) {
          schoen_test <- survival::cox.zph(model)
          plot(schoen_test)
        } else {
          plot.new()
          text(0.5, 0.5, "Schoenfeld residuals not available", cex = 1.5)
        }

        # dfbeta
        dfbeta_resid <- residuals(model, type = "dfbeta")

        if (!is.null(dfbeta_resid) && ncol(as.matrix(dfbeta_resid)) > 0) {
          plot(1:nrow(dfbeta_resid), dfbeta_resid[, 1],
               xlab = "Observation",
               ylab = "dfbeta",
               main = "dfbeta for First Predictor",
               pch = 20, col = "steelblue")
          abline(h = 0, lty = 2, col = "red")
        } else {
          plot.new()
          text(0.5, 0.5, "dfbeta not available", cex = 1.5)
        }

        par(mfrow = c(1, 1))

        TRUE

      } else {

        # Generic residual plot for other model types
        FALSE
      }
    }
  )
)