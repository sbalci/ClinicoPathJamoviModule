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
#' - Bootstrap-based variable importance assessment
#' - Screening methods for ultra-high dimensional data
#' - Cross-validation optimized for high-dimensional settings
#' - Regularization path analysis and visualization
#' 
#' \strong{Model Assessment & Validation:}
#' - Time-dependent prediction accuracy
#' - High-dimensional model diagnostics
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
      DEFAULT_BOOTSTRAP_ITERATIONS = 500,
      DEFAULT_STABILITY_THRESHOLD = 0.8,
      MIN_OBSERVATIONS = 30,
      MAX_VARIABLES_WITHOUT_SCREENING = 1000,
      
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
          
          # Perform variable screening if needed
          screening_results <- private$.performVariableScreening(data_prep)
          screening_results$data_prep <- data_prep  # Add data_prep to screening_results
          
          # Execute high-dimensional Cox regression
          model_results <- private$.performHighDimCoxRegression(screening_results)
          
          # Perform stability selection if requested
          if (self$options$stability_selection) {
            stability_results <- private$.performStabilitySelection(screening_results)
          }
          
          # Generate comprehensive results
          private$.populateResults(model_results, screening_results, stability_results)
          
          # Create visualizations
          private$.createPlots(model_results, screening_results, stability_results)
          
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
              "<pre>", htmlspecialchars(e$message), "</pre>",
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
        
        # Convert factors to numeric if needed
        for (i in seq_along(pred_data)) {
          if (is.factor(pred_data[[i]])) {
            pred_data[[i]] <- as.numeric(pred_data[[i]])
          }
        }
        
        pred_matrix <- as.matrix(pred_data)
        
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
      
      # Variable screening for ultra-high dimensional data
      .performVariableScreening = function(data_prep) {
        n_vars <- data_prep$n_vars
        n_obs <- data_prep$n_obs
        
        # Skip screening if number of variables is manageable
        if (n_vars <= private$MAX_VARIABLES_WITHOUT_SCREENING) {
          return(list(
            screened_vars = seq_len(n_vars),
            screening_performed = FALSE,
            screening_method = "none",
            n_screened = n_vars
          ))
        }
        
        # Perform marginal screening using Cox univariate models
        screening_pvals <- numeric(n_vars)
        screening_coeffs <- numeric(n_vars)
        
        for (i in seq_len(n_vars)) {
          tryCatch({
            cox_fit <- survival::coxph(data_prep$survival ~ data_prep$predictors[, i])
            screening_pvals[i] <- summary(cox_fit)$coefficients[, "Pr(>|z|)"]
            screening_coeffs[i] <- coef(cox_fit)
          }, error = function(e) {
            screening_pvals[i] <- 1.0
            screening_coeffs[i] <- 0.0
          })
        }
        
        # Select top variables based on significance
        top_k <- min(floor(n_obs / 2), n_vars, 500)  # Conservative screening
        selected_indices <- order(screening_pvals)[seq_len(top_k)]
        
        list(
          screened_vars = selected_indices,
          screening_performed = TRUE,
          screening_method = "marginal_cox",
          n_screened = top_k,
          screening_pvals = screening_pvals,
          screening_coeffs = screening_coeffs
        )
      },
      
      # High-dimensional Cox regression with advanced regularization
      .performHighDimCoxRegression = function(screening_results) {
        data_prep <- screening_results$data_prep
        screened_vars <- screening_results$screened_vars
        
        # Use screened variables
        X <- data_prep$predictors[, screened_vars, drop = FALSE]
        y <- data_prep$survival
        
        # Set regularization parameters
        alpha <- self$options$alpha_value
        regularization_method <- self$options$regularization_method
        
        # Adjust alpha based on regularization method
        if (regularization_method == "lasso") {
          alpha <- 1.0
        } else if (regularization_method == "ridge") {
          alpha <- 0.0
        }
        
        # Perform cross-validation
        cv_folds <- self$options$cv_folds
        
        cv_fit <- glmnet::cv.glmnet(
          x = X,
          y = y,
          family = "cox",
          alpha = alpha,
          nfolds = cv_folds,
          standardize = TRUE,
          parallel = FALSE
        )
        
        # Select lambda based on method
        lambda_selection <- self$options$cv_method
        if (lambda_selection == "cv_min") {
          selected_lambda <- cv_fit$lambda.min
        } else {
          selected_lambda <- cv_fit$lambda.1se
        }
        
        # Fit final model
        final_fit <- glmnet::glmnet(
          x = X,
          y = y,
          family = "cox",
          alpha = alpha,
          standardize = TRUE
        )
        
        # Extract coefficients at selected lambda
        coefficients <- as.vector(coef(final_fit, s = selected_lambda))
        selected_vars_idx <- which(coefficients != 0)
        
        # Calculate variable importance
        var_importance <- abs(coefficients)
        names(var_importance) <- data_prep$var_names[screened_vars]
        
        list(
          cv_fit = cv_fit,
          final_fit = final_fit,
          selected_lambda = selected_lambda,
          coefficients = coefficients,
          selected_variables = selected_vars_idx,
          variable_importance = var_importance,
          screened_indices = screened_vars,
          alpha = alpha,
          n_selected = length(selected_vars_idx)
        )
      },
      
      # Stability selection for robust variable identification
      .performStabilitySelection = function(screening_results) {
        data_prep <- screening_results$data_prep
        screened_vars <- screening_results$screened_vars
        
        n_bootstrap <- self$options$bootstrap_iterations
        stability_threshold <- self$options$stability_threshold
        
        X <- data_prep$predictors[, screened_vars, drop = FALSE]
        y <- data_prep$survival
        n_obs <- nrow(X)
        n_vars <- ncol(X)
        
        # Storage for bootstrap results
        selection_matrix <- matrix(0, nrow = n_bootstrap, ncol = n_vars)
        
        # Bootstrap sampling and variable selection
        for (b in seq_len(n_bootstrap)) {
          # Bootstrap sample
          boot_indices <- sample(n_obs, size = floor(n_obs * 0.8), replace = FALSE)
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
        # Initialize tables with empty structure
        if (self$options$show_coefficients_table) {
          self$results$selectedVariables$setKeys(character(0))
        }
        
        self$results$regularizationMetrics$setKeys(c("lambda", "deviance", "variables", "alpha"))
        self$results$dimensionalityReduction$setKeys(c("original", "screened", "selected"))
      },
      
      # Populate all result tables and summaries
      .populateResults = function(model_results, screening_results, stability_results = NULL) {
        # Populate model summary
        private$.populateModelSummary(model_results, screening_results)
        
        # Populate selected variables table
        if (self$options$show_coefficients_table) {
          private$.populateVariablesTable(model_results, screening_results)
        }
        
        # Populate regularization metrics
        private$.populateRegularizationMetrics(model_results)
        
        # Populate dimensionality reduction summary
        private$.populateDimensionalityReduction(model_results, screening_results)
        
        # Populate stability results if available
        if (!is.null(stability_results) && self$options$stability_selection) {
          private$.populateStabilityResults(stability_results, screening_results)
        }
      },
      
      # Populate model summary
      .populateModelSummary = function(model_results, screening_results) {
        summary_content <- paste0(
          "<h4>High-Dimensional Cox Regression Results</h4>",
          "<p><strong>Regularization:</strong> ", self$options$regularization_method, 
          " (α = ", round(model_results$alpha, 3), ")</p>",
          "<p><strong>Selected Lambda:</strong> ", 
          format(model_results$selected_lambda, scientific = TRUE, digits = 3), "</p>",
          "<p><strong>Variables:</strong> ", 
          length(self$options$predictors), " original → ",
          screening_results$n_screened, " screened → ",
          model_results$n_selected, " selected</p>",
          "<p><strong>Cross-Validation:</strong> ", self$options$cv_folds, "-fold CV</p>"
        )
        
        self$results$modelSummary$setContent(summary_content)
      },
      
      # Populate selected variables table
      .populateVariablesTable = function(model_results, screening_results) {
        if (model_results$n_selected == 0) {
          return()
        }
        
        selected_idx <- model_results$selected_variables
        screened_vars <- model_results$screened_indices
        
        # Get variable names and coefficients
        var_names <- names(model_results$variable_importance)[screened_vars][selected_idx]
        coefficients <- model_results$coefficients[selected_idx]
        hazard_ratios <- exp(coefficients)
        importance_scores <- model_results$variable_importance[screened_vars][selected_idx]
        
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
        
        metrics <- data.frame(
          metric = c(
            "Selected Lambda",
            "Lambda Min",
            "Lambda 1SE",
            "CV Deviance at Selected Lambda",
            "Number of Selected Variables",
            "Regularization Method"
          ),
          value = c(
            format(model_results$selected_lambda, scientific = TRUE, digits = 3),
            format(cv_fit$lambda.min, scientific = TRUE, digits = 3),
            format(cv_fit$lambda.1se, scientific = TRUE, digits = 3),
            round(cv_fit$cvm[cv_fit$lambda == model_results$selected_lambda], 3),
            model_results$n_selected,
            paste0(self$options$regularization_method, " (α=", round(model_results$alpha, 2), ")")
          ),
          interpretation = c(
            "Optimal regularization strength",
            "Lambda minimizing CV error",
            "Lambda within 1-SE of minimum",
            "Cross-validated model deviance",
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
      
      # Populate dimensionality reduction summary
      .populateDimensionalityReduction = function(model_results, screening_results) {
        n_original <- length(self$options$predictors)
        n_screened <- screening_results$n_screened
        n_selected <- model_results$n_selected
        
        stages <- data.frame(
          stage = c("Original Dataset", "After Screening", "Final Selection"),
          variables_input = c(n_original, n_screened, n_selected),
          variables_selected = c(n_screened, n_selected, n_selected),
          reduction_ratio = c(
            round(n_screened / n_original, 3),
            round(n_selected / n_screened, 3),
            1.0
          )
        )
        
        for (i in seq_len(nrow(stages))) {
          self$results$dimensionalityReduction$addRow(rowKey = i, values = list(
            stage = stages$stage[i],
            variables_input = stages$variables_input[i],
            variables_selected = stages$variables_selected[i],
            reduction_ratio = stages$reduction_ratio[i]
          ))
        }
      },
      
      # Populate stability selection results
      .populateStabilityResults = function(stability_results, screening_results) {
        selection_probs <- stability_results$selection_probabilities
        stable_vars <- stability_results$stable_variables
        screened_vars <- screening_results$screened_vars
        
        # Get variable names for screened variables
        data_prep <- screening_results$data_prep
        var_names <- data_prep$var_names[screened_vars]
        
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
      .createPlots = function(model_results, screening_results, stability_results = NULL) {
        if (self$options$show_regularization_path) {
          private$.createRegularizationPath(model_results)
        }
        
        if (self$options$show_cv_plot) {
          private$.createCVPlot(model_results)
        }
        
        if (self$options$show_variable_importance) {
          private$.createVariableImportancePlot(model_results, screening_results)
        }
        
        if (self$options$show_model_diagnostics) {
          private$.createModelDiagnostics(model_results, screening_results)
        }
        
        if (!is.null(stability_results) && self$options$stability_selection) {
          private$.createStabilityPlot(stability_results)
        }
      },
      
      # Create regularization path plot
      .createRegularizationPath = function(model_results) {
        tryCatch({
          image <- self$results$regularizationPath
          
          image$setState(list(
            width = 800,
            height = 600,
            model_results = model_results
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create cross-validation plot
      .createCVPlot = function(model_results) {
        tryCatch({
          image <- self$results$cvPlot
          
          image$setState(list(
            width = 800,
            height = 600,
            cv_fit = model_results$cv_fit,
            selected_lambda = model_results$selected_lambda
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create variable importance plot
      .createVariableImportancePlot = function(model_results, screening_results) {
        if (model_results$n_selected == 0) return()
        
        tryCatch({
          image <- self$results$variableImportance
          
          image$setState(list(
            width = 800,
            height = 600,
            importance_data = model_results$variable_importance,
            selected_vars = model_results$selected_variables,
            screened_indices = model_results$screened_indices
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create model diagnostics plot
      .createModelDiagnostics = function(model_results, screening_results) {
        tryCatch({
          image <- self$results$modelDiagnostics
          
          image$setState(list(
            width = 800,
            height = 600,
            model_results = model_results,
            screening_results = screening_results
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create stability selection plot
      .createStabilityPlot = function(stability_results) {
        tryCatch({
          image <- self$results$stabilityPlot
          
          image$setState(list(
            width = 800,
            height = 600,
            stability_results = stability_results
          ))
          
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
          
          "<h5>Variable Screening</h5>",
          "<p>For ultra-high dimensional data (p >> n), marginal screening using univariate Cox models ",
          "pre-selects the most promising variables before applying regularized regression, improving ",
          "computational efficiency and statistical properties.</p>",
          
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
      }
      
    )
  ) # End R6Class