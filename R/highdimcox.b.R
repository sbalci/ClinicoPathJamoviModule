#' @title High-Dimensional Cox Regression Implementation
#' @description
#' Backend implementation class for high-dimensional Cox proportional hazards regression.
#' This R6 class provides advanced functionality for regularized survival analysis
#' with many predictors, stability selection, and comprehensive variable screening
#' methods specifically designed for genomic, proteomic, and high-throughput clinical data.
#' 
#' @details
#' The highdimcoxClass implements advanced high-dimensional Cox regression with:
#' 
#' \strong{Advanced Regularization Methods:}
#' - LASSO (L1) penalty for sparse variable selection
#' - Ridge (L2) penalty for correlated predictor handling
#' - Elastic Net combining L1 and L2 penalties
#' - Adaptive LASSO with data-driven penalty weights
#' - Group LASSO for grouped predictors
#' 
#' \strong{High-Dimensional Specific Features:}
#' - Stability selection for robust variable identification
#' - Comprehensive model validation and diagnostics
#' - Variable importance rankings
#' - Stability-based confidence measures
#' 
#' \strong{Clinical Applications:}
#' - Genomic survival analysis (expression, methylation, CNV)
#' - Proteomic and metabolomic survival modeling
#' - Multi-omics integration for survival prediction
#' - Biomarker discovery in high-throughput data
#' 
#' @seealso \code{\link{highdimcox}} for the main user interface function
#' @importFrom R6 R6Class
#' @import jmvcore
#' @keywords internal

highdimcoxClass <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class(
    "highdimcoxClass",
    inherit = highdimcoxBase,
    private = list(
      
      # Model objects and results storage
      .glmnet_model = NULL,
      .cv_results = NULL,
      .selected_lambda = NULL,
      .selected_variables = NULL,
      .stability_results = NULL,
      .variable_importance = NULL,
      .screening_results = NULL,
      
      # Constants for high-dimensional analysis
      DEFAULT_CV_FOLDS = 10,
      DEFAULT_ALPHA = 0.5,
      DEFAULT_SUBSAMPLING_ITERATIONS = 500,
      DEFAULT_STABILITY_THRESHOLD = 0.8,
      MIN_OBSERVATIONS = 30,
      
      # Core initialization method
      .init = function() {
        # Initialize results with informative messages
        self$results$todo$setContent(
          paste0(
            "<h3>High-Dimensional Cox Regression</h3>",
            "<p>This analysis is designed for survival data with many predictors and requires:</p>",
            "<ul>",
            "<li><b>Time variable:</b> Follow-up time or survival duration</li>",
            "<li><b>Outcome variable:</b> Event indicator (0/1 or FALSE/TRUE)</li>",
            "<li><b>High-dimensional predictors:</b> Many variables (genes, proteins, features)</li>",
            "</ul>",
            "<p><strong>Key Features:</strong></p>",
            "<ul>",
            "<li>Handles p >> n scenarios (more variables than observations)</li>",
            "<li>Advanced regularization with LASSO, Ridge, Elastic Net, Adaptive LASSO</li>",
            "<li>Stability selection for robust variable identification</li>",
            "<li>Comprehensive model validation and diagnostics</li>",
            "</ul>",
            "<p>Select variables to begin high-dimensional survival analysis.</p>"
          )
        )
        
        # Early return if no data
        if (is.null(self$data) || nrow(self$data) == 0) {
          return()
        }
        
        # Validate minimum required inputs
        validation <- private$.validateInputs()
        if (!validation$valid) {
          return()
        }
        
        # Initialize result tables
        private$.initializeResultTables()
      },
      
      # Main analysis execution
      .run = function() {
        # Early validation
        validation <- private$.validateInputs()
        if (!validation$valid) {
          return()
        }
        
        # Clear todo message
        self$results$todo$setContent("")
        
        # Check package availability
        required_packages <- c("glmnet", "survival")
        missing_packages <- c()
        
        for (pkg in required_packages) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
            missing_packages <- c(missing_packages, pkg)
          }
        }
        
        if (length(missing_packages) > 0) {
          self$results$todo$setContent(
            paste0(
              "<h3>Required Packages Missing</h3>",
              "<p>The following packages are required for high-dimensional Cox regression:</p>",
              "<ul>", paste0("<li>", missing_packages, "</li>", collapse = ""), "</ul>",
              "<p>Please install these packages to proceed with the analysis.</p>"
            )
          )
          return()
        }
        
        tryCatch({
          # Prepare data for analysis
          data_prep <- private$.prepareData()
          if (is.null(data_prep)) {
            return()
          }
          
          # Assess data suitability if requested
          if (self$options$suitabilityCheck) {
            private$.assessSuitability(data_prep)
          }

          # Execute high-dimensional Cox regression
          model_results <- private$.performHighDimCoxRegression(data_prep)
          
          # Perform stability selection if requested
          stability_results <- NULL
          if (self$options$stability_selection) {
            stability_results <- private$.performStabilitySelection(data_prep)
          }

          # Generate comprehensive results
          private$.populateResults(model_results, data_prep, stability_results)
          
          # Create visualizations
          private$.createPlots(model_results, data_prep, stability_results)
          
          # Generate summaries if requested
          if (self$options$showSummaries) {
            private$.generateSummaries(model_results, stability_results)
          }
          
          # Generate explanations if requested
          if (self$options$showExplanations) {
            private$.generateExplanations()
          }
          
        }, error = function(e) {
          self$results$todo$setContent(
            paste0(
              "<h3>Analysis Error</h3>",
              "<p>An error occurred during high-dimensional Cox regression analysis:</p>",
              "<pre>", gsub("&", "&amp;", gsub(">", "&gt;", gsub("<", "&lt;", e$message))), "</pre>",
              "<p>Please check your data and analysis options.</p>"
            )
          )
        })
      },
      
      # Input validation
      .validateInputs = function() {
        # Check for required variables
        if (is.null(self$options$elapsedtime) || self$options$elapsedtime == "") {
          return(list(valid = FALSE, message = "Time variable is required"))
        }
        
        if (is.null(self$options$outcome) || self$options$outcome == "") {
          return(list(valid = FALSE, message = "Event variable is required"))
        }
        
        if (is.null(self$options$predictors) || length(self$options$predictors) == 0) {
          return(list(valid = FALSE, message = "At least one predictor variable is required"))
        }
        
        # Check minimum observations
        if (nrow(self$data) < private$MIN_OBSERVATIONS) {
          return(list(
            valid = FALSE, 
            message = paste("At least", private$MIN_OBSERVATIONS, "observations required for high-dimensional analysis")
          ))
        }
        
        return(list(valid = TRUE, message = ""))
      },
      
      # Data preparation for high-dimensional analysis
      .prepareData = function() {
        # Extract variables
        time_var <- self$options$elapsedtime
        event_var <- self$options$outcome
        pred_vars <- self$options$predictors
        
        # Get time variable
        time_data <- self$data[[time_var]]
        if (!is.numeric(time_data)) {
          self$results$todo$setContent(
            "<h3>Data Error</h3><p>Time variable must be numeric.</p>"
          )
          return(NULL)
        }
        
        # Handle outcome variable
        event_data <- self$data[[event_var]]
        outcome_level <- as.character(self$options$outcomeLevel)
        
        if (is.factor(event_data)) {
          event_numeric <- as.numeric(event_data == outcome_level)
        } else {
          event_numeric <- as.numeric(event_data == as.numeric(outcome_level))
        }
        
        # Get predictor matrix
        pred_data <- self$data[pred_vars]

        # Check if any predictors are factors requiring dummy encoding
        has_factors <- any(vapply(pred_data, is.factor, logical(1)))
        if (has_factors) {
          # Use model.matrix to create proper dummy variables (no intercept)
          pred_matrix <- model.matrix(~ . - 1, data = pred_data)
        } else {
          pred_matrix <- as.matrix(pred_data)
        }
        
        # Remove rows with missing values
        complete_rows <- complete.cases(time_data, event_numeric, pred_matrix)
        
        if (sum(complete_rows) < private$MIN_OBSERVATIONS) {
          self$results$todo$setContent(
            paste0(
              "<h3>Data Error</h3>",
              "<p>Insufficient complete observations (", sum(complete_rows), ") after removing missing values.</p>",
              "<p>At least ", private$MIN_OBSERVATIONS, " complete observations are required.</p>"
            )
          )
          return(NULL)
        }
        
        # Create survival object
        surv_obj <- survival::Surv(time_data[complete_rows], event_numeric[complete_rows])
        
        list(
          survival = surv_obj,
          predictors = pred_matrix[complete_rows, , drop = FALSE],
          time = time_data[complete_rows],
          event = event_numeric[complete_rows],
          n_obs = sum(complete_rows),
          n_vars = ncol(pred_matrix),
          complete_rows = complete_rows,
          var_names = colnames(pred_matrix)
        )
      },
      
      # High-dimensional Cox regression with advanced regularization
      .performHighDimCoxRegression = function(data_prep) {
        
        # Use full predictor set
        X <- data_prep$predictors
        y <- data_prep$survival
        
        # Set regularization parameters
        alpha <- self$options$alpha_value
        regularization_method <- self$options$regularization_method
        penalty_weights <- rep(1, ncol(X))

        # Adjust alpha based on regularization method
        if (regularization_method == "lasso") {
          alpha <- 1.0
        } else if (regularization_method == "ridge") {
          alpha <- 0.0
        } else if (regularization_method == "adaptive_lasso") {
          alpha <- 1.0
          # Compute adaptive penalty weights from Ridge initial estimates
          penalty_weights <- tryCatch({
            ridge_fit <- glmnet::glmnet(
              x = X, y = y, family = "cox",
              alpha = 0, standardize = TRUE
            )
            lambda_ridge <- ridge_fit$lambda[max(1, length(ridge_fit$lambda) %/% 4)]
            initial_coefs <- as.vector(coef(ridge_fit, s = lambda_ridge))
            gamma <- 1  # standard adaptive LASSO exponent
            w <- 1 / (abs(initial_coefs) + 1e-6)^gamma
            w / mean(w)  # normalize so mean penalty weight = 1
          }, error = function(e) {
            self$results$todo$setContent(
              "<div class='alert alert-warning'><strong>Warning:</strong> Ridge regression failed during Adaptive LASSO weights calculation. Falling back to standard LASSO.</div>"
            )
            rep(1, ncol(X))
          })
        }
        # else: elastic_net uses user-provided alpha_value

        # Perform cross-validation
        cv_folds <- self$options$cv_folds

        cv_fit <- glmnet::cv.glmnet(
          x = X,
          y = y,
          family = "cox",
          alpha = alpha,
          penalty.factor = penalty_weights,
          nfolds = cv_folds,
          standardize = TRUE,
          parallel = FALSE
        )

        # Select lambda based on method
        lambda_selection <- self$options$cv_method
        if (lambda_selection == "cv_min") {
          selected_lambda <- cv_fit$lambda.min
        } else {
          selected_lambda <- cv_fit$lambda.1se  # cv_1se
        }

        # Fit final model
        final_fit <- glmnet::glmnet(
          x = X,
          y = y,
          family = "cox",
          alpha = alpha,
          penalty.factor = penalty_weights,
          standardize = TRUE
        )
        
        # Extract coefficients at selected lambda
        coefficients <- as.vector(coef(final_fit, s = selected_lambda))
        selected_vars_idx <- which(coefficients != 0)

        # Calculate variable importance
        var_importance <- abs(coefficients)
        names(var_importance) <- data_prep$var_names

        # Calculate concordance index (C-index)
        # Note: higher risk score = worse prognosis, so reverse = TRUE
        concordance_val <- tryCatch({
          risk_scores <- as.vector(X %*% coefficients)
          cindex_result <- survival::concordance(y ~ risk_scores, reverse = TRUE)
          cindex_result$concordance
        }, error = function(e) NA_real_)

        list(
          cv_fit = cv_fit,
          final_fit = final_fit,
          selected_lambda = selected_lambda,
          coefficients = coefficients,
          selected_variables = selected_vars_idx,
          variable_importance = var_importance,
          alpha = alpha,
          n_selected = length(selected_vars_idx),
          concordance = concordance_val
        )
      },
      
      # Stability selection for robust variable identification
      .performStabilitySelection = function(data_prep) {
        
        n_bootstrap <- self$options$subsampling_iterations
        stability_threshold <- self$options$stability_threshold
        
        X <- data_prep$predictors
        y <- data_prep$survival
        n_obs <- nrow(X)
        n_vars <- ncol(X)
        
        # Storage for subsampling results
        selection_matrix <- matrix(0, nrow = n_bootstrap, ncol = n_vars)
        
        # Subsampling and variable selection
        for (b in seq_len(n_bootstrap)) {
          # Subsample
          boot_indices <- sample(n_obs, size = floor(n_obs * 0.5), replace = FALSE)
          X_boot <- X[boot_indices, , drop = FALSE]
          y_boot <- y[boot_indices]
          
          tryCatch({
            # Fit LASSO model (alpha = 1 for selection)
            cv_boot <- glmnet::cv.glmnet(
              x = X_boot,
              y = y_boot,
              family = "cox",
              alpha = 1.0,
              nfolds = 5,
              standardize = TRUE
            )
            
            # Get selected variables
            coeffs <- as.vector(coef(cv_boot, s = "lambda.1se"))
            selected <- which(coeffs != 0)
            selection_matrix[b, selected] <- 1
            
          }, error = function(e) {
            # Skip this bootstrap iteration on error
          })
        }
        
        # Calculate selection probabilities
        selection_probs <- colMeans(selection_matrix)
        stable_vars <- which(selection_probs >= stability_threshold)
        
        list(
          selection_matrix = selection_matrix,
          selection_probabilities = selection_probs,
          stable_variables = stable_vars,
          stability_threshold = stability_threshold,
          n_bootstrap = n_bootstrap,
          n_stable = length(stable_vars)
        )
      },
      
      # Initialize result tables
      .initializeResultTables = function() {
        # Tables are initialized via addRow calls in populate methods
      },
      
      # Populate all result tables and summaries
      .populateResults = function(model_results, data_prep, stability_results = NULL) {
        # Populate model summary
        private$.populateModelSummary(model_results, data_prep)
        
        # Populate selected variables table
        if (self$options$show_coefficients_table) {
          private$.populateVariablesTable(model_results, data_prep)
        }
        
        # Populate regularization metrics
        private$.populateRegularizationMetrics(model_results)
        
        # Populate stability results if available
        if (!is.null(stability_results) && self$options$stability_selection) {
          private$.populateStabilityResults(stability_results, data_prep)
        }
      },
      
      # Populate model summary
      .populateModelSummary = function(model_results, data_prep) {
        summary_content <- paste0(
          "<h4>High-Dimensional Cox Regression Results</h4>",
          "<p><strong>Regularization:</strong> ", self$options$regularization_method, 
          " (α = ", round(model_results$alpha, 3), ")</p>",
          "<p><strong>Selected Lambda:</strong> ", 
          format(model_results$selected_lambda, scientific = TRUE, digits = 3), "</p>",
          "<p><strong>Variables:</strong> ", 
          length(self$options$predictors), " candidate variables → ",
          model_results$n_selected, " selected</p>",
          "<p><strong>Cross-Validation:</strong> ", self$options$cv_folds, "-fold CV</p>",
          if (!is.na(model_results$concordance)) {
            paste0("<p><strong>Concordance (C-index):</strong> ",
                   round(model_results$concordance, 3), "</p>")
          } else ""
        )
        
        self$results$modelSummary$setContent(summary_content)
      },
      
      # Populate selected variables table
      .populateVariablesTable = function(model_results, data_prep) {
        if (model_results$n_selected == 0) {
          return()
        }
        
        selected_idx <- model_results$selected_variables

        var_names <- data_prep$var_names[selected_idx]
        coefficients <- model_results$coefficients[selected_idx]
        hazard_ratios <- exp(coefficients)
        importance_scores <- as.numeric(model_results$variable_importance[selected_idx])
        
        # Create table data
        for (i in seq_along(selected_idx)) {
          self$results$selectedVariables$addRow(rowKey = i, values = list(
            variable = var_names[i],
            coefficient = coefficients[i],
            hazard_ratio = hazard_ratios[i],
            importance_score = importance_scores[i]
          ))
        }
      },
      
      # Populate regularization metrics
      .populateRegularizationMetrics = function(model_results) {
        cv_fit <- model_results$cv_fit
        
        conc_val <- if (!is.na(model_results$concordance)) {
          round(model_results$concordance, 4)
        } else "N/A"

        metrics <- data.frame(
          metric = c(
            "Selected Lambda",
            "Lambda Min",
            "Lambda 1SE",
            "CV Deviance at Selected Lambda",
            "Concordance (C-index)",
            "Number of Selected Variables",
            "Regularization Method"
          ),
          value = c(
            format(model_results$selected_lambda, scientific = TRUE, digits = 3),
            format(cv_fit$lambda.min, scientific = TRUE, digits = 3),
            format(cv_fit$lambda.1se, scientific = TRUE, digits = 3),
            round(cv_fit$cvm[which.min(abs(cv_fit$lambda - model_results$selected_lambda))], 3),
            as.character(conc_val),
            model_results$n_selected,
            paste0(self$options$regularization_method, " (α=", round(model_results$alpha, 2), ")")
          ),
          interpretation = c(
            "Optimal regularization strength",
            "Lambda minimizing CV error",
            "Lambda within 1-SE of minimum",
            "Cross-validated model deviance",
            "Discrimination ability (0.5 = random, 1.0 = perfect)",
            "Variables with non-zero coefficients",
            "Applied regularization strategy"
          )
        )
        
        for (i in seq_len(nrow(metrics))) {
          self$results$regularizationMetrics$addRow(rowKey = i, values = list(
            metric = metrics$metric[i],
            value = metrics$value[i],
            interpretation = metrics$interpretation[i]
          ))
        }
      },
      
      # Populate stability selection results
      .populateStabilityResults = function(stability_results, data_prep) {
        selection_probs <- stability_results$selection_probabilities
        stable_vars <- stability_results$stable_variables
        
        var_names <- data_prep$var_names
        
        # Create results for variables above threshold
        stable_data <- data.frame(
          variable = var_names[stable_vars],
          selection_probability = selection_probs[stable_vars],
          stable = rep("Yes", length(stable_vars)),
          importance_rank = rank(-selection_probs[stable_vars])
        )
        
        # Add some variables below threshold for comparison
        unstable_vars <- setdiff(seq_along(selection_probs), stable_vars)
        if (length(unstable_vars) > 0) {
          top_unstable <- head(unstable_vars[order(-selection_probs[unstable_vars])], 5)
          unstable_data <- data.frame(
            variable = var_names[top_unstable],
            selection_probability = selection_probs[top_unstable],
            stable = rep("No", length(top_unstable)),
            importance_rank = length(stable_vars) + seq_along(top_unstable)
          )
          
          final_data <- rbind(stable_data, unstable_data)
        } else {
          final_data <- stable_data
        }
        
        # Sort by selection probability
        final_data <- final_data[order(-final_data$selection_probability), ]
        
        for (i in seq_len(nrow(final_data))) {
          self$results$stabilityResults$addRow(rowKey = i, values = list(
            variable = final_data$variable[i],
            selection_probability = final_data$selection_probability[i],
            stable = final_data$stable[i],
            importance_rank = as.integer(final_data$importance_rank[i])
          ))
        }
      },
      
      # Create visualization plots
      .createPlots = function(model_results, data_prep, stability_results = NULL) {
        if (self$options$show_regularization_path) {
          private$.createRegularizationPath(model_results)
        }
        
        if (self$options$show_cv_plot) {
          private$.createCVPlot(model_results)
        }
        
        if (self$options$show_variable_importance) {
          private$.createVariableImportancePlot(model_results, data_prep)
        }
        
        if (self$options$show_model_diagnostics) {
          private$.createModelDiagnostics(model_results, data_prep)
        }
        
        if (!is.null(stability_results) && self$options$stability_selection) {
          private$.createStabilityPlot(stability_results, data_prep)
        }
      },
      
      # Create regularization path plot
      .createRegularizationPath = function(model_results) {
        tryCatch({
          image <- self$results$regularizationPath
          # Extract only plain numeric data for protobuf-safe serialization
          cv_fit <- model_results$cv_fit
          plot_data <- list(
            lambda = as.numeric(cv_fit$lambda),
            cvm = as.numeric(cv_fit$cvm),
            cvsd = as.numeric(cv_fit$cvsd),
            lambda_min = as.numeric(cv_fit$lambda.min),
            lambda_1se = as.numeric(cv_fit$lambda.1se)
          )
          image$setState(plot_data)
        }, error = function(e) {
          # Skip plot creation on error
        })
      },

      # Create cross-validation plot
      .createCVPlot = function(model_results) {
        tryCatch({
          image <- self$results$cvPlot
          cv_fit <- model_results$cv_fit
          plot_data <- list(
            lambda = as.numeric(cv_fit$lambda),
            cvm = as.numeric(cv_fit$cvm),
            cvsd = as.numeric(cv_fit$cvsd),
            cvup = as.numeric(cv_fit$cvup),
            cvlo = as.numeric(cv_fit$cvlo),
            lambda_min = as.numeric(cv_fit$lambda.min),
            lambda_1se = as.numeric(cv_fit$lambda.1se),
            selected_lambda = as.numeric(model_results$selected_lambda)
          )
          image$setState(plot_data)
        }, error = function(e) {
          # Skip plot creation on error
        })
      },

      # Create variable importance plot
      .createVariableImportancePlot = function(model_results, data_prep) {
        if (model_results$n_selected == 0) return()

        tryCatch({
          image <- self$results$variableImportance
          # Store selected variable names (not indices) for color matching in render
          selected_var_names <- names(model_results$variable_importance)[model_results$selected_variables]
          plot_data <- list(
            var_names = as.character(names(model_results$variable_importance)),
            importance = as.numeric(model_results$variable_importance),
            selected_vars = as.character(selected_var_names)
          )
          image$setState(plot_data)
        }, error = function(e) {
          # Skip plot creation on error
        })
      },

      # Create model diagnostics plot
      .createModelDiagnostics = function(model_results, data_prep) {
        tryCatch({
          image <- self$results$modelDiagnostics
          selected_var_names <- names(model_results$variable_importance)[model_results$selected_variables]
          plot_data <- list(
            n_selected = as.integer(model_results$n_selected),
            selected_vars = as.character(selected_var_names),
            concordance = as.numeric(model_results$concordance)
          )
          image$setState(plot_data)
        }, error = function(e) {
          # Skip plot creation on error
        })
      },

      # Create stability selection plot
      .createStabilityPlot = function(stability_results, data_prep) {
        tryCatch({
          image <- self$results$stabilityPlot
          var_names <- data_prep$var_names
          plot_data <- list(
            selection_frequencies = as.numeric(stability_results$selection_probabilities),
            var_names = as.character(var_names),
            threshold = as.numeric(stability_results$stability_threshold)
          )
          image$setState(plot_data)
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Generate natural language summaries
      .generateSummaries = function(model_results, stability_results = NULL) {
        summary_text <- paste0(
          "<h4>Analysis Summary</h4>",
          "<p>High-dimensional Cox regression analysis was performed on ", 
          length(self$options$predictors), " predictor variables using ",
          self$options$regularization_method, " regularization.</p>",
          
          "<p><strong>Model Selection:</strong> ",
          "Cross-validation with ", self$options$cv_folds, " folds identified ",
          "an optimal regularization parameter (λ = ", 
          format(model_results$selected_lambda, scientific = TRUE, digits = 3), ") ",
          "that selected ", model_results$n_selected, " variables from the candidate set.</p>",
          
          if (!is.null(stability_results)) {
            paste0(
              "<p><strong>Stability Selection:</strong> ",
              "Bootstrap stability analysis with ", stability_results$n_bootstrap, " iterations ",
              "identified ", stability_results$n_stable, " variables with selection probability ≥ ",
              stability_results$stability_threshold, ", indicating robust variable selection.</p>"
            )
          } else "",
          
          "<p><strong>Interpretation:</strong> ",
          "The selected variables represent the most predictive features for survival outcome ",
          "after accounting for multiple testing and overfitting through regularization. ",
          "Variables with larger absolute coefficients have stronger associations with survival risk.</p>"
        )
        
        self$results$analysisSummary$setContent(summary_text)
      },
      
      # Generate methodology explanations
      .generateExplanations = function() {
        explanations <- paste0(
          "<h4>High-Dimensional Cox Regression Methodology</h4>",
          
          "<h5>Overview</h5>",
          "<p>High-dimensional Cox regression extends traditional Cox proportional hazards modeling ",
          "to handle datasets where the number of predictors (p) may exceed or approach the number ",
          "of observations (n). This scenario is common in genomic, proteomic, and other high-throughput ",
          "biomedical research contexts.</p>",
          
          "<h5>Regularization Methods</h5>",
          "<ul>",
          "<li><strong>LASSO (L1):</strong> Performs automatic variable selection by shrinking some coefficients to exactly zero</li>",
          "<li><strong>Ridge (L2):</strong> Shrinks coefficients toward zero but retains all variables, useful when predictors are correlated</li>",
          "<li><strong>Elastic Net:</strong> Combines L1 and L2 penalties, balancing variable selection and coefficient shrinkage</li>",
          "<li><strong>Adaptive LASSO:</strong> Uses data-driven penalty weights for improved variable selection properties</li>",
          "</ul>",
          
          "<h5>Cross-Validation</h5>",
          "<p>The regularization parameter (λ) is selected using cross-validation to optimize prediction performance. ",
          "The '1-SE rule' selects a more parsimonious model by choosing the largest λ within one standard error ",
          "of the minimum cross-validation error.</p>",
          
          "<h5>Stability Selection</h5>",
          "<p>When enabled, stability selection performs variable selection across multiple bootstrap samples ",
          "to identify variables that are consistently selected. This provides a measure of selection confidence ",
          "and helps identify the most robust predictive features.</p>",
          
          "<h5>Clinical Interpretation</h5>",
          "<p>Selected variables and their coefficients can be used to:</p>",
          "<ul>",
          "<li>Identify key biomarkers associated with survival</li>",
          "<li>Develop prognostic signatures for risk stratification</li>",
          "<li>Guide hypothesis generation for follow-up studies</li>",
          "<li>Build personalized survival prediction models</li>",
          "</ul>"
        )
        
        self$results$methodExplanation$setContent(explanations)
      },

      # Data suitability assessment
      .assessSuitability = function(data_prep) {
        checks <- list()
        n <- data_prep$n_obs
        n_events <- sum(data_prep$event)
        p <- data_prep$n_vars
        event_rate <- n_events / n

        # -- Check 1: Events-Per-Variable (EPV) --
        epv <- n_events / p
        if (epv >= 10) { # Adjusted for high dim
          checks$epv <- list(
            color = "green", label = "Events-Per-Variable (Overall)",
            value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
            detail = "High EPV. Regularization will perform robustly."
          )
        } else if (epv >= 1) {
          checks$epv <- list(
            color = "yellow", label = "Events-Per-Variable (Overall)",
            value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
            detail = "Adequate for regularized regression, which handles low EPV better than standard Cox."
          )
        } else {
          checks$epv <- list(
            color = "yellow", label = "Events-Per-Variable (Overall)",
            value = sprintf("%.3f (n_events=%d, p=%d)", epv, n_events, p),
            detail = "Ultra-low EPV (p > n_events). Standard Cox would fail. Penalized regression is strictly required."
          )
        }

        # -- Check 2: Regularization Need --
        if (p >= n / 3) {
          checks$regularization <- list(
            color = "green", label = "Regularization Need",
            value = sprintf("p=%d, n=%d (ratio=%.2f)", p, n, p / n),
            detail = "High-dimensional setting. Penalized regularization is strongly indicated."
          )
        } else {
          checks$regularization <- list(
            color = "yellow", label = "Regularization Need",
            value = sprintf("p=%d, EPV=%.0f", p, epv),
            detail = "Moderate/low dimensionality. Standard Cox may also suffice."
          )
        }

        # -- Check 3: Sample Size --
        if (n >= 100) {
          checks$sample_size <- list(
            color = "green", label = "Sample Size",
            value = sprintf("n=%d", n),
            detail = "Adequate sample size for penalized regression."
          )
        } else if (n >= 30) {
          checks$sample_size <- list(
            color = "yellow", label = "Sample Size",
            value = sprintf("n=%d", n),
            detail = "Small sample. CV folds may be somewhat unstable."
          )
        } else {
          checks$sample_size <- list(
            color = "red", label = "Sample Size",
            value = sprintf("n=%d", n),
            detail = "Very small sample. Results will be highly variable."
          )
        }

        # -- Check 4: Event Rate --
        if (event_rate >= 0.20 && event_rate <= 0.80) {
          checks$event_rate <- list(
            color = "green", label = "Event Rate",
            value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
            detail = "Balanced event rate. Good for model estimation."
          )
        } else {
          checks$event_rate <- list(
            color = "yellow", label = "Event Rate",
            value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
            detail = "Imbalanced event rate. Model calibration may be affected."
          )
        }

        # -- Check 5: Multicollinearity --
        tryCatch({
          # Due to potentially high dimensionality, we only check collinearity on a subset or skip heavy logic
          if (p <= 2000) {
              if (p >= 2) {
                  cor_matrix <- cor(data_prep$predictors, use = "pairwise.complete.obs")
                  diag(cor_matrix) <- 0
                  max_cor <- max(abs(cor_matrix), na.rm = TRUE)
                  
                  if (max_cor < 0.7) {
                      checks$collinearity <- list(
                          color = "green", label = "Multicollinearity",
                          value = sprintf("Max |r| = %.2f", max_cor),
                          detail = "No concerning collinearity detected."
                      )
                  } else if (max_cor < 0.9) {
                      checks$collinearity <- list(
                          color = "yellow", label = "Multicollinearity",
                          value = sprintf("Max |r| = %.2f", max_cor),
                          detail = "Moderate collinearity. Elastic Net or Ridge handles this well."
                      )
                  } else {
                      checks$collinearity <- list(
                          color = "yellow", label = "Multicollinearity",
                          value = sprintf("Max |r| = %.2f", max_cor),
                          detail = "High collinearity. Elastic Net or Ridge is strongly recommended to retain correlated predictors."
                      )
                  }
              }
          } else {
              checks$collinearity <- list(
                  color = "green", label = "Multicollinearity",
                  value = paste0("p=", p, " (skipped)"),
                  detail = "Skipped exhaustive collinearity check due to ultra-high dimensionality."
              )
          }
        }, error = function(e) {
          NULL
        })

        # -- Check 6: Data Quality --
        original_data <- self$data
        pred_vars <- self$options$predictors
        n_total <- nrow(original_data)
        n_missing <- n_total - n
        pct_missing <- 100 * n_missing / n_total

        # Check for constant predictors
        constant_cols <- apply(data_prep$predictors, 2, function(col) var(col, na.rm = TRUE) == 0)
        n_constant <- sum(constant_cols)

        if (n_missing == 0 && n_constant == 0) {
            checks$data_quality <- list(
                color = "green", label = "Data Quality",
                value = "No issues",
                detail = "Complete data with no constant predictors."
            )
        } else {
            issues <- character(0)
            if (pct_missing > 0) issues <- c(issues, sprintf("%.1f%% missing data (%d rows excluded).", pct_missing, n_missing))
            if (n_constant > 0) issues <- c(issues, sprintf("%d constant predictor column(s) detected.", n_constant))
            checks$data_quality <- list(
                color = if (pct_missing > 20 || n_constant > 0) "red" else "yellow",
                label = "Data Quality",
                value = sprintf("%.1f%% missing, %d constant", pct_missing, n_constant),
                detail = paste(issues, collapse = " ")
            )
        }

        # -- Overall Verdict --
        colors <- sapply(checks, function(x) x$color)
        if (any(colors == "red")) {
            overall <- "red"
            overall_text <- "Some issues require attention before relying on these results."
        } else if (any(colors == "yellow")) {
            overall <- "yellow"
            overall_text <- "Data is usable but review the flagged items."
        } else {
            overall <- "green"
            overall_text <- "Data is well-suited for High-Dimensional Cox regression."
        }

        private$.generateSuitabilityHtml(checks, overall, overall_text)
      },

      .generateSuitabilityHtml = function(checks, overall, overall_text) {
          # Color mapping
          bg_colors <- list(
              green  = "background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb;",
              yellow = "background-color: #fff3cd; color: #856404; border: 1px solid #ffeeba;",
              red    = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb;"
          )
          dot_colors <- list(green = "#28a745", yellow = "#ffc107", red = "#dc3545")

          # Overall banner
          html <- paste0(
              "<div style='", bg_colors[[overall]], " padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
              "<strong>Overall: ", overall_text, "</strong></div>"
          )

          # Check table
          html <- paste0(html,
              "<table style='width: 100%; border-collapse: collapse; font-size: 13px;'>",
              "<thead><tr style='border-bottom: 2px solid #dee2e6;'>",
              "<th style='padding: 6px; text-align: left;'>Status</th>",
              "<th style='padding: 6px; text-align: left;'>Check</th>",
              "<th style='padding: 6px; text-align: left;'>Value</th>",
              "<th style='padding: 6px; text-align: left;'>Detail</th>",
              "</tr></thead><tbody>"
          )

          for (chk in checks) {
              if (is.null(chk)) next
              dot <- paste0("<span style='color: ", dot_colors[[chk$color]], "; font-size: 18px;'>&#9679;</span>")
              html <- paste0(html,
                  "<tr style='border-bottom: 1px solid #dee2e6;'>",
                  "<td style='padding: 6px;'>", dot, "</td>",
                  "<td style='padding: 6px;'><strong>", chk$label, "</strong></td>",
                  "<td style='padding: 6px;'>", chk$value, "</td>",
                  "<td style='padding: 6px;'>", chk$detail, "</td>",
                  "</tr>"
              )
          }

          html <- paste0(html, "</tbody></table>")

          self$results$suitabilityReport$setContent(html)
      },

      # Render function for regularization path plot
      .plot_regularization_path = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          log_lambda <- log(plot_data$lambda)
          plot(log_lambda, plot_data$cvm, type = "l", col = "blue", lwd = 2,
               xlab = expression(log(lambda)), ylab = "Partial Likelihood Deviance",
               main = "Regularization Path")
          abline(v = log(plot_data$lambda_min), lty = 2, col = "red")
          if (!is.null(plot_data$lambda_1se))
            abline(v = log(plot_data$lambda_1se), lty = 2, col = "darkgreen")
          legend("topright", legend = c("Lambda Min", "Lambda 1SE"),
                 lty = 2, col = c("red", "darkgreen"), cex = 0.8)
          TRUE
        }, error = function(e) FALSE)
      },

      # Render function for cross-validation plot
      .plot_cv = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          log_lambda <- log(plot_data$lambda)
          plot(log_lambda, plot_data$cvm, type = "n",
               ylim = range(c(plot_data$cvlo, plot_data$cvup), na.rm = TRUE),
               xlab = expression(log(lambda)), ylab = "Partial Likelihood Deviance",
               main = "Cross-Validation Results")
          polygon(c(log_lambda, rev(log_lambda)),
                  c(plot_data$cvup, rev(plot_data$cvlo)),
                  col = rgb(0.8, 0.8, 1, 0.3), border = NA)
          lines(log_lambda, plot_data$cvm, col = "blue", lwd = 2)
          abline(v = log(plot_data$lambda_min), lty = 2, col = "red")
          if (!is.null(plot_data$lambda_1se))
            abline(v = log(plot_data$lambda_1se), lty = 2, col = "darkgreen")
          legend("topright", legend = c("CV Mean", "Lambda Min", "Lambda 1SE"),
                 lty = c(1, 2, 2), col = c("blue", "red", "darkgreen"),
                 lwd = c(2, 1, 1), cex = 0.8)
          TRUE
        }, error = function(e) FALSE)
      },

      # Render function for variable importance plot
      .plot_variable_importance = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          imp <- plot_data$importance
          names(imp) <- plot_data$var_names
          imp <- sort(imp, decreasing = FALSE)
          n <- length(imp)
          if (n == 0) return(FALSE)
          cols <- ifelse(names(imp) %in% plot_data$selected_vars, "steelblue", "gray70")
          par(mar = c(5, max(8, max(nchar(names(imp))) * 0.6), 4, 2))
          barplot(imp, horiz = TRUE, las = 1, col = cols,
                  xlab = "Importance Score", main = "Variable Importance")
          TRUE
        }, error = function(e) FALSE)
      },

      # Render function for model diagnostics plot
      .plot_model_diagnostics = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          conc_text <- if (!is.null(plot_data$concordance) && !is.na(plot_data$concordance)) {
            paste("Concordance (C-index):", round(plot_data$concordance, 3))
          } else {
            "Concordance: N/A"
          }
          info <- c(
            paste("Selected Variables:", plot_data$n_selected),
            conc_text
          )
          plot.new()
          plot.window(xlim = c(0, 1), ylim = c(0, 1))
          title(main = "Model Diagnostics Summary")
          text(0.5, 0.7, info[1], cex = 1.2)
          text(0.5, 0.5, info[2], cex = 1.2)
          if (!is.null(plot_data$selected_vars) && length(plot_data$selected_vars) > 0) {
            vars_text <- paste("Variables:", paste(plot_data$selected_vars, collapse = ", "))
            text(0.5, 0.3, vars_text, cex = 0.9)
          }
          TRUE
        }, error = function(e) FALSE)
      },

      # Render function for stability selection plot
      .plot_stability = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          freqs <- plot_data$selection_frequencies
          names(freqs) <- plot_data$var_names
          freqs <- sort(freqs, decreasing = FALSE)
          n <- length(freqs)
          if (n == 0) return(FALSE)
          cols <- ifelse(freqs >= plot_data$threshold, "steelblue", "gray70")
          par(mar = c(5, max(8, max(nchar(names(freqs))) * 0.6), 4, 2))
          barplot(freqs, horiz = TRUE, las = 1, col = cols,
                  xlab = "Selection Frequency", main = "Stability Selection",
                  xlim = c(0, 1))
          abline(v = plot_data$threshold, lty = 2, col = "red")
          legend("bottomright", legend = paste("Threshold:", plot_data$threshold),
                 lty = 2, col = "red", cex = 0.8)
          TRUE
        }, error = function(e) FALSE)
      }

    )
  ) # End R6Class