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
      
      # Variable name display mapping (model.matrix names → original names)
      .var_display_names = NULL,

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
        # Early validation — display via todo HTML (no Notice objects)
        validation <- private$.validateInputs()
        if (!validation$valid) {
          self$results$todo$setContent(paste0(
            "<div class='alert alert-danger'>",
            "<h4>Validation Error</h4>",
            "<p>", validation$message, "</p></div>"))
          return()
        }

        # Check package availability
        required_packages <- c("glmnet", "survival")
        missing_packages <- c()
        for (pkg in required_packages) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
            missing_packages <- c(missing_packages, pkg)
          }
        }
        if (length(missing_packages) > 0) {
          self$results$todo$setContent(paste0(
            "<div class='alert alert-danger'>",
            "<h4>Missing Packages</h4>",
            "<p>Required packages missing: ",
            paste(missing_packages, collapse = ", "),
            ". Please install them to proceed.</p></div>"))
          return()
        }

        # Clear todo and begin analysis
        self$results$todo$setContent("")
        self$results$todo$setVisible(FALSE)

        # Collect warnings during the pipeline (avoids Notice serialization)
        collected_warnings <- character(0)

        withCallingHandlers(
          tryCatch({
            # Prepare data for analysis
            data_prep <- private$.prepareData()
            if (is.null(data_prep)) return()

            # Data quality warnings
            if (data_prep$n_excluded_outcome > 0) {
              warning(sprintf(
                '%d row(s) excluded: outcome matched neither event level nor censored level.',
                data_prep$n_excluded_outcome))
            }
            if (data_prep$n_constant_removed > 0) {
              warning(sprintf(
                '%d constant predictor(s) removed (zero variance).',
                data_prep$n_constant_removed))
            }

            # Assess data suitability if requested
            if (self$options$suitabilityCheck) {
              private$.assessSuitability(data_prep)
            }

            # Warn on low events
            n_events <- sum(data_prep$event)
            if (n_events < 10) {
              warning(sprintf(
                'Only %d events detected. Results may be highly unstable; consider increasing sample size or reducing predictors.',
                n_events))
            } else if (n_events < 20) {
              warning(sprintf(
                '%d events detected. Regularized models handle low events better than standard Cox, but interpret results cautiously.',
                n_events))
            }

            # Execute high-dimensional Cox regression
            model_results <- private$.performHighDimCoxRegression(data_prep)

            # Perform stability selection if requested
            stability_results <- NULL
            if (self$options$stability_selection) {
              stability_results <- private$.performStabilitySelection(data_prep)
              if (stability_results$n_successful < stability_results$n_bootstrap * 0.5) {
                warning(sprintf(
                  'Only %d/%d stability iterations succeeded. Stability probabilities are unreliable; consider increasing sample size or reducing predictors.',
                  stability_results$n_successful, stability_results$n_bootstrap))
              }
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

            # Display collected warnings + success message in todo HTML
            success_msg <- sprintf(
              'Analysis completed: %d observations, %d events, %d predictors, %d selected via %s (C-index=%.3f).',
              data_prep$n_obs, n_events, data_prep$n_vars,
              model_results$n_selected, self$options$regularization_method,
              ifelse(is.na(model_results$concordance), 0, model_results$concordance))

            if (length(collected_warnings) > 0) {
              warn_items <- paste0("<li>", collected_warnings, "</li>", collapse = "")
              todo_html <- paste0(
                "<div class='alert alert-warning'>",
                "<h4>Analysis Notes</h4>",
                "<ul>", warn_items, "</ul></div>",
                "<div class='alert alert-success'><p>", success_msg, "</p></div>")
            } else {
              todo_html <- paste0(
                "<div class='alert alert-success'><p>", success_msg, "</p></div>")
            }
            self$results$todo$setContent(todo_html)
            self$results$todo$setVisible(TRUE)

          }, error = function(e) {
            error_msg <- paste0(
              "<div class='alert alert-danger'>",
              "<h4>Analysis Error</h4>",
              "<p><strong>Error:</strong> ", e$message, "</p>",
              "<p>Please check your data and variable selections.</p></div>")
            private$.clearAnalysisOutputs()
            self$results$todo$setContent(error_msg)
            self$results$todo$setVisible(TRUE)
          }),
          warning = function(w) {
            collected_warnings <<- c(collected_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
      },
      
      # Input validation
      .validateInputs = function() {
        # Check for required variables
        if (is.null(self$options$elapsedtime) || self$options$elapsedtime == "") {
          return(list(valid = FALSE, message = "Time variable is required."))
        }

        if (is.null(self$options$outcome) || self$options$outcome == "") {
          return(list(valid = FALSE, message = "Event variable is required."))
        }

        if (is.null(self$options$predictors) || length(self$options$predictors) == 0) {
          return(list(valid = FALSE, message = "At least one predictor variable is required."))
        }

        # Check minimum observations
        if (nrow(self$data) < private$MIN_OBSERVATIONS) {
          return(list(
            valid = FALSE,
            message = sprintf("At least %d observations required for high-dimensional analysis (found %d).",
                              private$MIN_OBSERVATIONS, nrow(self$data))
          ))
        }

        # Validate outcome levels are selected and distinct
        event_level <- self$options$outcomeLevel
        censor_level <- self$options$censorLevel
        if (is.null(event_level) || is.null(censor_level) ||
            event_level == "" || censor_level == "") {
          return(list(valid = FALSE, message = "Both event level and censored level must be selected."))
        }
        if (as.character(event_level) == as.character(censor_level)) {
          return(list(valid = FALSE,
                      message = sprintf("Event level and censored level must be different (both set to '%s').",
                                        as.character(event_level))))
        }

        # Validate that at least some rows match the declared levels
        event_data <- self$data[[self$options$outcome]]
        event_chr <- as.character(event_data)
        n_event <- sum(event_chr == as.character(event_level), na.rm = TRUE)
        n_censor <- sum(event_chr == as.character(censor_level), na.rm = TRUE)
        if (n_event == 0) {
          return(list(valid = FALSE,
                      message = sprintf("No rows match event level '%s' in the outcome variable.", event_level)))
        }
        if (n_censor == 0) {
          return(list(valid = FALSE,
                      message = sprintf("No rows match censored level '%s' in the outcome variable.", censor_level)))
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
        
        # Handle outcome variable — two-level encoding
        # Rows matching event_level → 1, censor_level → 0, anything else → NA (excluded)
        event_data <- self$data[[event_var]]
        event_level <- as.character(self$options$outcomeLevel)
        censor_level <- as.character(self$options$censorLevel)

        event_numeric <- rep(NA_real_, length(event_data))

        if (is.factor(event_data)) {
          event_chr <- as.character(event_data)
          event_numeric[event_chr == event_level] <- 1
          event_numeric[event_chr == censor_level] <- 0
        } else {
          event_level_num <- suppressWarnings(as.numeric(event_level))
          censor_level_num <- suppressWarnings(as.numeric(censor_level))
          if (!is.na(event_level_num)) event_numeric[event_data == event_level_num] <- 1
          if (!is.na(censor_level_num)) event_numeric[event_data == censor_level_num] <- 0
        }

        n_excluded_outcome <- sum(is.na(event_numeric) & !is.na(event_data))
        n_na_outcome <- sum(is.na(event_data))
        # Warnings will be emitted as Notices in .run(); store counts in returned list
        
        # Get predictor data
        pred_data <- self$data[pred_vars]

        # Complete-case filter BEFORE model.matrix (model.matrix drops NA
        # rows silently for factor columns, causing length mismatches)
        complete_rows <- complete.cases(time_data, event_numeric, pred_data)

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

        time_data <- time_data[complete_rows]
        event_numeric <- event_numeric[complete_rows]
        pred_data <- pred_data[complete_rows, , drop = FALSE]

        # Check if any predictors are factors requiring dummy encoding
        has_factors <- any(vapply(pred_data, is.factor, logical(1)))
        if (has_factors) {
          # Use model.matrix to create proper dummy variables (no intercept)
          pred_matrix <- model.matrix(~ . - 1, data = pred_data)
        } else {
          pred_matrix <- as.matrix(pred_data)
        }

        # Remove constant predictors (zero variance) — glmnet cannot handle them
        col_vars <- apply(pred_matrix, 2, function(col) var(col, na.rm = TRUE))
        constant_cols <- which(col_vars == 0 | is.na(col_vars))
        if (length(constant_cols) > 0) {
          pred_matrix <- pred_matrix[, -constant_cols, drop = FALSE]
          if (ncol(pred_matrix) == 0) {
            self$results$todo$setContent(
              "<h3>Data Error</h3><p>All predictors are constant (zero variance). Cannot fit a model.</p>"
            )
            return(NULL)
          }
        }

        # Create survival object (data already filtered to complete cases)
        surv_obj <- survival::Surv(time_data, event_numeric)
        
        # Build display name mapping: model.matrix names → user-friendly names
        mm_names <- colnames(pred_matrix)
        display_names <- mm_names
        for (pv in pred_vars) {
          # Match columns that start with make.names(pv) or pv itself
          safe_pv <- make.names(pv)
          for (j in seq_along(mm_names)) {
            if (mm_names[j] == safe_pv || mm_names[j] == pv) {
              display_names[j] <- pv
            } else if (startsWith(mm_names[j], safe_pv)) {
              # Factor dummy: e.g. "GradeLow" → "Grade: Low"
              level_part <- substring(mm_names[j], nchar(safe_pv) + 1)
              display_names[j] <- paste0(pv, ": ", level_part)
            }
          }
        }
        private$.var_display_names <- display_names

        list(
          survival = surv_obj,
          predictors = pred_matrix,
          time = time_data,
          event = event_numeric,
          n_obs = length(time_data),
          n_vars = ncol(pred_matrix),
          complete_rows = complete_rows,
          var_names = display_names,
          n_excluded_outcome = n_excluded_outcome,
          n_na_outcome = n_na_outcome,
          n_constant_removed = length(constant_cols)
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
        cv_folds <- self$options$cv_folds

        # Adjust alpha based on regularization method
        if (regularization_method == "lasso") {
          alpha <- 1.0
        } else if (regularization_method == "ridge") {
          alpha <- 0.0
        } else if (regularization_method == "adaptive_lasso") {
          alpha <- 1.0
          # Compute adaptive penalty weights from CV-selected Ridge initial estimates
          penalty_weights <- tryCatch({
            cv_ridge <- glmnet::cv.glmnet(
              x = X, y = y, family = "cox",
              alpha = 0, standardize = TRUE,
              nfolds = min(cv_folds, nrow(X))
            )
            initial_coefs <- as.vector(coef(cv_ridge, s = cv_ridge$lambda.min))
            gamma <- 1  # standard adaptive LASSO exponent
            w <- 1 / (abs(initial_coefs) + 1e-6)^gamma
            w / mean(w)  # normalize so mean penalty weight = 1
          }, error = function(e) {
            rep(1, ncol(X))
          })
        }
        # else: elastic_net uses user-provided alpha_value

        # Perform cross-validation
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

        # Calculate variable importance (use display names)
        var_importance <- abs(coefficients)
        names(var_importance) <- data_prep$var_names

        # Calculate concordance index (C-index) — training set (optimistic)
        # Note: higher risk score = worse prognosis, so reverse = TRUE
        # This is a re-substitution estimate; external validation is needed
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

        # Determine alpha from user's regularization method
        # Stability selection requires sparsity, so ridge (alpha=0) gets bumped to elastic net
        reg_method <- self$options$regularization_method
        stab_alpha <- switch(reg_method,
          lasso = 1.0,
          ridge = 0.5,  # ridge has no selection; use elastic net instead
          elastic_net = self$options$alpha_value,
          adaptive_lasso = 1.0,
          1.0  # fallback
        )

        # Per Meinshausen & Buhlmann (2010): use a FIXED lambda across all subsamples
        # to maintain theoretical FDR/FWER control. Compute lambda from full-data CV.
        fixed_lambda <- tryCatch({
          cv_full <- glmnet::cv.glmnet(
            x = X, y = y, family = "cox",
            alpha = stab_alpha,
            nfolds = min(self$options$cv_folds, n_obs),
            standardize = TRUE
          )
          cv_full$lambda.1se
        }, error = function(e) NULL)

        if (is.null(fixed_lambda)) {
          return(list(
            selection_probabilities = rep(0, n_vars),
            stable_variables = integer(0),
            stability_threshold = stability_threshold,
            n_bootstrap = n_bootstrap,
            n_successful = 0L,
            n_stable = 0L,
            alpha_used = stab_alpha
          ))
        }

        # Storage for subsampling results
        selection_matrix <- matrix(0, nrow = n_bootstrap, ncol = n_vars)
        iter_succeeded <- logical(n_bootstrap)

        # Subsampling and variable selection
        subsample_ratio <- self$options$subsampling_ratio
        subsample_size <- floor(n_obs * subsample_ratio)

        for (b in seq_len(n_bootstrap)) {
          boot_indices <- sample(n_obs, size = subsample_size, replace = FALSE)
          X_boot <- X[boot_indices, , drop = FALSE]
          y_boot <- y[boot_indices]

          tryCatch({
            # Fit glmnet path (NOT cv.glmnet) — much faster per iteration
            fit_boot <- glmnet::glmnet(
              x = X_boot, y = y_boot, family = "cox",
              alpha = stab_alpha, standardize = TRUE
            )

            # Extract coefficients at the fixed lambda
            coeffs <- as.vector(coef(fit_boot, s = fixed_lambda))
            selected <- which(coeffs != 0)
            selection_matrix[b, selected] <- 1
            iter_succeeded[b] <- TRUE

          }, error = function(e) {
            # Skip this bootstrap iteration on error
          })
        }

        # Calculate selection probabilities using only successful iterations
        n_successful <- sum(iter_succeeded)
        if (n_successful > 0) {
          selection_probs <- colMeans(selection_matrix[iter_succeeded, , drop = FALSE])
        } else {
          selection_probs <- rep(0, n_vars)
        }
        stable_vars <- which(selection_probs >= stability_threshold)

        list(
          selection_probabilities = selection_probs,
          stable_variables = stable_vars,
          stability_threshold = stability_threshold,
          n_bootstrap = n_bootstrap,
          n_successful = n_successful,
          n_stable = length(stable_vars),
          alpha_used = stab_alpha
        )
      },
      
      # Clear analysis outputs on error to prevent stale results
      .clearAnalysisOutputs = function() {
        self$results$selectedVariables$deleteRows()
        self$results$regularizationMetrics$deleteRows()
        self$results$stabilityResults$deleteRows()
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
            paste0("<p><strong>Training C-index (optimistic):</strong> ",
                   round(model_results$concordance, 3), "</p>")
          } else ""
        )
        
        self$results$modelSummary$setContent(summary_content)
      },
      
      # Populate selected variables table
      .populateVariablesTable = function(model_results, data_prep) {
        if (model_results$n_selected == 0) {
          self$results$selectedVariables$setNote(
            "empty",
            "No variables were selected at the chosen regularization level. Consider using a less restrictive lambda (minimum CV) or a different regularization method."
          )
          return()
        }
        # Ridge retains all variables — add clarifying note
        if (self$options$regularization_method == "ridge" &&
            model_results$n_selected == data_prep$n_vars) {
          self$results$selectedVariables$setNote(
            "ridge",
            "Ridge regression shrinks coefficients but does not set them to zero. All variables are retained. Use LASSO or Elastic Net for variable selection."
          )
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
            "Training C-index (optimistic)",
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
            "Training-set estimate; likely overestimates true discrimination. Validate externally.",
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
          final_fit <- model_results$final_fit

          # Extract coefficient matrix across lambda path (actual coefficient trajectories)
          coef_matrix <- as.matrix(coef(final_fit))  # p x n_lambda
          lambdas <- final_fit$lambda

          # Build long-format data.frame for ggplot2
          rows <- list()
          var_names <- rownames(coef_matrix)
          for (j in seq_len(ncol(coef_matrix))) {
            nonzero <- which(coef_matrix[, j] != 0)
            if (length(nonzero) > 0) {
              rows[[length(rows) + 1]] <- data.frame(
                log_lambda = rep(log(lambdas[j]), length(nonzero)),
                coefficient = coef_matrix[nonzero, j],
                variable = var_names[nonzero],
                stringsAsFactors = FALSE
              )
            }
          }

          if (length(rows) > 0) {
            plot_data <- do.call(rbind, rows)
          } else {
            plot_data <- data.frame(
              log_lambda = numeric(0), coefficient = numeric(0),
              variable = character(0), stringsAsFactors = FALSE
            )
          }
          attr(plot_data, "lambda_min") <- as.numeric(model_results$cv_fit$lambda.min)
          attr(plot_data, "lambda_1se") <- as.numeric(model_results$cv_fit$lambda.1se)
          attr(plot_data, "selected_lambda") <- as.numeric(model_results$selected_lambda)
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
          plot_data <- as.data.frame(list(
            lambda = as.numeric(cv_fit$lambda),
            cvm = as.numeric(cv_fit$cvm),
            cvsd = as.numeric(cv_fit$cvsd),
            cvup = as.numeric(cv_fit$cvup),
            cvlo = as.numeric(cv_fit$cvlo)
          ))
          attr(plot_data, "lambda_min") <- as.numeric(cv_fit$lambda.min)
          attr(plot_data, "lambda_1se") <- as.numeric(cv_fit$lambda.1se)
          attr(plot_data, "selected_lambda") <- as.numeric(model_results$selected_lambda)
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
          selected_var_names <- names(model_results$variable_importance)[model_results$selected_variables]
          plot_data <- data.frame(
            var_names = as.character(names(model_results$variable_importance)),
            importance = as.numeric(model_results$variable_importance),
            stringsAsFactors = FALSE
          )
          attr(plot_data, "selected_vars") <- as.character(selected_var_names)
          image$setState(plot_data)
        }, error = function(e) {
          # Skip plot creation on error
        })
      },

      # Create model diagnostics plot
      .createModelDiagnostics = function(model_results, data_prep) {
        tryCatch({
          image <- self$results$modelDiagnostics
          selected_idx <- model_results$selected_variables
          selected_var_names <- names(model_results$variable_importance)[selected_idx]
          selected_coefs <- model_results$coefficients[selected_idx]
          selected_hr <- exp(selected_coefs)

          plot_data <- data.frame(
            n_selected = as.integer(model_results$n_selected),
            concordance = as.numeric(model_results$concordance),
            stringsAsFactors = FALSE
          )
          attr(plot_data, "selected_vars") <- as.character(selected_var_names)
          attr(plot_data, "coefficients") <- as.numeric(selected_coefs)
          attr(plot_data, "hazard_ratios") <- as.numeric(selected_hr)
          image$setState(plot_data)
        }, error = function(e) {
          # Skip plot creation on error
        })
      },

      # Create stability selection plot
      .createStabilityPlot = function(stability_results, data_prep) {
        tryCatch({
          image <- self$results$stabilityPlot
          plot_data <- data.frame(
            var_names = as.character(data_prep$var_names),
            selection_frequencies = as.numeric(stability_results$selection_probabilities),
            stringsAsFactors = FALSE
          )
          attr(plot_data, "threshold") <- as.numeric(stability_results$stability_threshold)
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
          "an optimal regularization parameter (&lambda; = ",
          format(model_results$selected_lambda, scientific = TRUE, digits = 3), ") ",
          "that selected ", model_results$n_selected, " variables from the candidate set.</p>",
          if (!is.na(model_results$concordance)) {
            paste0("<p><strong>Training C-index (optimistic):</strong> ",
                   round(model_results$concordance, 3), "</p>")
          } else "",

          if (!is.null(stability_results)) {
            paste0(
              "<p><strong>Stability Selection:</strong> ",
              "Bootstrap stability analysis with ", stability_results$n_successful, "/",
              stability_results$n_bootstrap, " successful iterations ",
              "(&alpha; = ", round(stability_results$alpha_used, 2), ") ",
              "identified ", stability_results$n_stable, " variables with selection probability &ge; ",
              stability_results$stability_threshold, ".</p>"
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
            color = "red", label = "Events-Per-Variable (Overall)",
            value = sprintf("%.3f (n_events=%d, p=%d)", epv, n_events, p),
            detail = "Ultra-low EPV (p > n_events). Results are exploratory only. Validate externally before clinical use."
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

        # Use pre-removal count (constant predictors already removed by .prepareData)
        n_constant <- data_prep$n_constant_removed

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

      # Render function for regularization path plot (ggplot2)
      .plot_regularization_path = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data) || nrow(plot_data) == 0) return(FALSE)

        tryCatch({
          if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)

          lambda_min <- attr(plot_data, "lambda_min")
          lambda_1se <- attr(plot_data, "lambda_1se")
          sel_lambda <- attr(plot_data, "selected_lambda")

          p <- ggplot2::ggplot(plot_data,
                               ggplot2::aes(x = log_lambda, y = coefficient,
                                            color = variable, group = variable)) +
            ggplot2::geom_line(alpha = 0.7, linewidth = 0.6) +
            ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey50", linewidth = 0.3) +
            ggplot2::geom_vline(xintercept = log(lambda_min), linetype = "dashed", color = "red") +
            ggplot2::geom_vline(xintercept = log(lambda_1se), linetype = "dashed", color = "darkgreen") +
            ggplot2::annotate("text", x = log(lambda_min), y = max(plot_data$coefficient, na.rm = TRUE),
                              label = "min", color = "red", hjust = -0.2, size = 3) +
            ggplot2::annotate("text", x = log(lambda_1se), y = max(plot_data$coefficient, na.rm = TRUE),
                              label = "1se", color = "darkgreen", hjust = -0.2, size = 3) +
            ggplot2::labs(
              x = expression(log(lambda)),
              y = "Coefficient",
              title = "Coefficient Path") +
            ggtheme +
            ggplot2::theme(legend.position = "none")

          print(p)
          TRUE
        }, error = function(e) FALSE)
      },

      # Render function for cross-validation plot (ggplot2)
      .plot_cv = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)

          df <- data.frame(
            log_lambda = log(plot_data$lambda),
            cvm = plot_data$cvm,
            cvup = plot_data$cvup,
            cvlo = plot_data$cvlo
          )
          lambda_min <- attr(plot_data, "lambda_min")
          lambda_1se <- attr(plot_data, "lambda_1se")
          selected_lambda <- attr(plot_data, "selected_lambda")

          p <- ggplot2::ggplot(df, ggplot2::aes(x = log_lambda, y = cvm)) +
            ggplot2::geom_ribbon(
              ggplot2::aes(ymin = cvlo, ymax = cvup),
              alpha = 0.2, fill = "steelblue") +
            ggplot2::geom_line(color = "steelblue", linewidth = 1) +
            ggplot2::geom_point(color = "steelblue", size = 1) +
            ggplot2::geom_vline(xintercept = log(lambda_min), linetype = "dashed", color = "red") +
            ggplot2::geom_vline(xintercept = log(lambda_1se), linetype = "dashed", color = "darkgreen") +
            ggplot2::annotate("text", x = log(lambda_min), y = max(df$cvup, na.rm = TRUE),
                              label = "min", color = "red", hjust = -0.2, size = 3) +
            ggplot2::annotate("text", x = log(lambda_1se), y = max(df$cvup, na.rm = TRUE),
                              label = "1se", color = "darkgreen", hjust = -0.2, size = 3) +
            ggplot2::labs(
              x = expression(log(lambda)),
              y = "Partial Likelihood Deviance",
              title = "Cross-Validation Results") +
            ggtheme

          print(p)
          TRUE
        }, error = function(e) FALSE)
      },

      # Render function for variable importance plot (ggplot2, top 25 max)
      .plot_variable_importance = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)

          df <- data.frame(
            variable = plot_data$var_names,
            importance = plot_data$importance,
            stringsAsFactors = FALSE
          )
          selected_vars <- attr(plot_data, "selected_vars")
          df$selected <- ifelse(df$variable %in% selected_vars, "Selected", "Not selected")

          # Keep only top 25 by importance for readability
          df <- df[order(-df$importance), ]
          if (nrow(df) > 25) df <- df[1:25, ]

          # Order factor for horizontal bar chart
          df$variable <- factor(df$variable, levels = rev(df$variable))

          p <- ggplot2::ggplot(df, ggplot2::aes(x = importance, y = variable, fill = selected)) +
            ggplot2::geom_col() +
            ggplot2::scale_fill_manual(
              values = c("Selected" = "steelblue", "Not selected" = "grey70"),
              name = NULL) +
            ggplot2::labs(
              x = "Importance Score (|coefficient|)",
              y = NULL,
              title = "Variable Importance (Top 25)") +
            ggtheme +
            ggplot2::theme(legend.position = "bottom")

          print(p)
          TRUE
        }, error = function(e) FALSE)
      },

      # Render function for model diagnostics — coefficient forest plot (ggplot2)
      .plot_model_diagnostics = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)

          selected_vars <- attr(plot_data, "selected_vars")
          coefs <- attr(plot_data, "coefficients")
          hr_vals <- attr(plot_data, "hazard_ratios")

          # If we have coefficient data, draw a forest-style plot
          if (!is.null(coefs) && !is.null(selected_vars) && length(coefs) > 0) {
            df <- data.frame(
              variable = selected_vars,
              hr = hr_vals,
              coef = coefs,
              stringsAsFactors = FALSE
            )
            df <- df[order(df$hr), ]
            df$variable <- factor(df$variable, levels = df$variable)

            p <- ggplot2::ggplot(df, ggplot2::aes(x = hr, y = variable)) +
              ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
              ggplot2::geom_point(color = "steelblue", size = 3) +
              ggplot2::geom_segment(ggplot2::aes(x = 1, xend = hr, y = variable, yend = variable),
                                    color = "steelblue", linewidth = 0.5) +
              ggplot2::labs(
                x = "Hazard Ratio",
                y = NULL,
                title = sprintf("Selected Variables (C-index = %.3f)",
                                ifelse(is.na(plot_data$concordance[1]), 0, plot_data$concordance[1]))) +
              ggtheme

            print(p)
          } else {
            # Fallback: text summary
            plot.new()
            plot.window(xlim = c(0, 1), ylim = c(0, 1))
            title(main = "Model Diagnostics Summary")
            conc_val <- plot_data$concordance[1]
            text(0.5, 0.6, paste("Selected Variables:", plot_data$n_selected[1]), cex = 1.2)
            text(0.5, 0.4,
                 paste("C-index:", ifelse(is.na(conc_val), "N/A", round(conc_val, 3))),
                 cex = 1.2)
          }
          TRUE
        }, error = function(e) FALSE)
      },

      # Render function for stability selection plot (ggplot2, top 25 max)
      .plot_stability = function(image, ggtheme, theme, ...) {
        plot_data <- image$state
        if (is.null(plot_data)) return(FALSE)

        tryCatch({
          if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)

          df <- data.frame(
            variable = plot_data$var_names,
            frequency = plot_data$selection_frequencies,
            stringsAsFactors = FALSE
          )
          threshold <- attr(plot_data, "threshold")
          df$stable <- ifelse(df$frequency >= threshold, "Stable", "Below threshold")

          # Keep only top 25 by frequency for readability
          df <- df[order(-df$frequency), ]
          if (nrow(df) > 25) df <- df[1:25, ]
          df$variable <- factor(df$variable, levels = rev(df$variable))

          p <- ggplot2::ggplot(df, ggplot2::aes(x = frequency, y = variable, fill = stable)) +
            ggplot2::geom_col() +
            ggplot2::geom_vline(xintercept = threshold, linetype = "dashed", color = "red") +
            ggplot2::scale_fill_manual(
              values = c("Stable" = "steelblue", "Below threshold" = "grey70"),
              name = NULL) +
            ggplot2::scale_x_continuous(limits = c(0, 1)) +
            ggplot2::labs(
              x = "Selection Frequency",
              y = NULL,
              title = sprintf("Stability Selection (threshold = %.2f)", threshold)) +
            ggtheme +
            ggplot2::theme(legend.position = "bottom")

          print(p)
          TRUE
        }, error = function(e) FALSE)
      }

    )
  ) # End R6Class