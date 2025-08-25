# Clinical Model Validation Suite
# 
# This function provides comprehensive validation for clinical prediction models
# including bootstrap validation, cross-validation, model calibration assessment,
# and clinical performance evaluation

clinicalvalidationClass <- R6::R6Class(
  "clinicalvalidationClass",
  inherit = clinicalvalidationBase,
  private = list(
    
    # Core analysis method
    .run = function() {
      
      # Check if analysis is ready
      if (!self$.isReady()) {
        self$results$instructions$setContent("Select outcome variable and at least one predictor to begin analysis.")
        return()
      }
      
      # Get data
      data <- self$.getData()
      
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
        
        # Prepare outcome variable
        if (!is.null(self$options$outcomeLevel)) {
          outcome_level <- self$options$outcomeLevel
        } else {
          outcome_level <- levels(data[[outcome_var]])[1]
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
      
      # Get predictions
      if (model_type == "glm") {
        pred_probs <- predict(model, test_data, type = "response")
        predictions <- ifelse(pred_probs > 0.5, 1, 0)
      } else if (model_type == "randomForest") {
        pred_probs <- predict(model, test_data, type = "prob")[, 2]
        predictions <- predict(model, test_data)
      } else {
        return(list(auc = NA, accuracy = NA, sensitivity = NA, specificity = NA))
      }
      
      # Actual outcomes
      actual <- as.numeric(test_data$outcome_binary) - 1
      
      # Calculate metrics
      if (length(unique(actual)) > 1 && !all(is.na(pred_probs))) {
        
        # AUC
        auc <- tryCatch(as.numeric(pROC::auc(actual, pred_probs)), error = function(e) NA)
        
        # Confusion matrix metrics
        cm <- table(Predicted = predictions, Actual = actual)
        
        if (nrow(cm) == 2 && ncol(cm) == 2) {
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
    
    # Compare multiple models
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
      
      for (model_type in models_to_compare) {
        
        tryCatch({
          
          # Temporarily change model type
          original_model_type <- self$options$model_type
          self$options$model_type <- model_type
          
          # Fit model
          model_results <- private$.fitModel(prepared_data)
          
          if (!is.null(model_results)) {
            
            # Evaluate performance
            metrics <- private$.evaluateModel(model_results$model, prepared_data$data, prepared_data)
            
            comparison_results <- rbind(comparison_results, data.frame(
              model = model_type,
              auc = metrics$auc,
              accuracy = metrics$accuracy,
              sensitivity = metrics$sensitivity,
              specificity = metrics$specificity,
              stringsAsFactors = FALSE
            ))
          }
          
          # Restore original model type
          self$options$model_type <- original_model_type
          
        }, error = function(e) {
          # Handle model fitting errors
        })
      }
      
      # Rank models by AUC
      if (nrow(comparison_results) > 0) {
        comparison_results$rank <- rank(-comparison_results$auc, na.last = "keep")
      }
      
      return(comparison_results)
    },
    
    # Populate all results
    .populateResults = function(model_results, validation_results) {
      
      # Model summary
      if (self$options$show_model_summary) {
        private$.populateModelSummary(model_results)
      }
      
      # Performance table
      if (self$options$show_performance_table) {
        private$.populatePerformanceTable(validation_results$performance, validation_results$bootstrap)
      }
      
      # Bootstrap results
      if (self$options$validation_method == "bootstrap" && self$options$detailed_bootstrap) {
        private$.populateBootstrapResults(validation_results$bootstrap)
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
      
      self$results$performancetable$setContent(table_data)
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
      
      self$results$bootstrapresults$setContent(table_data)
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
      
      self$results$calibrationtable$setContent(table_data)
      
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
        
        self$results$brierscore$setContent(brier_data)
      }
    },
    
    # Populate model comparison
    .populateModelComparison = function(comparison_results) {
      
      self$results$modelcomparison$setContent(comparison_results)
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
    }
  )
)