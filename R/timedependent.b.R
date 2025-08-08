#' @title Time-Dependent Covariates & ROC Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import timeROC
#' @import timereg
#' @import riskRegression
#' @import pROC
#' @import ggplot2
#' @import dplyr

timedependentClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "timedependentClass",
    inherit = timedependentBase,
    private = list(
      
      .fitted_model = NULL,
      .roc_results = NULL,
      .landmark_results = NULL,
      .validation_results = NULL,
      
      # init ----
      .init = function() {
        if (is.null(self$data) || nrow(self$data) == 0)
          return()
          
        # Set plot sizes based on options
        self$results$time_varying_plot$setSize(600, 400)
        self$results$roc_curves_plot$setSize(600, 400)
        self$results$auc_trajectory_plot$setSize(600, 400)
        self$results$cutpoint_stability_plot$setSize(600, 400)
        self$results$landmark_predictions_plot$setSize(600, 400)
        self$results$schoenfeld_plot$setSize(600, 400)
      },
      
      # run ----
      .run = function() {
        
        # Validation and setup
        if (!self$options$id || !self$options$stop_time || !self$options$event) {
          self$results$todo$setContent(self$.create_welcome_message())
          return()
        }
        
        # Check required packages
        required_packages <- c("survival", "timeROC", "timereg", "riskRegression", "pROC")
        missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
        
        if (length(missing_packages) > 0) {
          error_msg <- paste0(
            "<div style='color: red; background-color: #ffebee; padding: 20px; border-radius: 8px;'>",
            "<h4>Missing Required Packages</h4>",
            "<p>The following packages are required for time-dependent analysis:</p>",
            "<ul>", paste0("<li>", missing_packages, "</li>", collapse = ""), "</ul>",
            "<p>Please install them using: <code>install.packages(c('", paste(missing_packages, collapse = "', '"), "'))</code></p>",
            "</div>"
          )
          self$results$todo$setContent(error_msg)
          return()
        }
        
        tryCatch({
          # Prepare data
          data_prepared <- self$.prepare_data()
          if (is.null(data_prepared)) {
            return()
          }
          
          # Fit models
          model_results <- self$.fit_models(data_prepared)
          if (is.null(model_results)) {
            return()
          }
          
          private$.fitted_model <- model_results
          
          # Generate results
          self$.populate_cox_results(model_results)
          
          if (self$options$test_proportional_hazards) {
            self$.test_time_varying_effects(model_results)
          }
          
          if (self$options$perform_landmark) {
            self$.perform_landmark_analysis(data_prepared)
          }
          
          if (self$options$time_dependent_roc) {
            self$.perform_roc_analysis(data_prepared, model_results)
          }
          
          if (self$options$compare_models) {
            self$.compare_models(data_prepared)
          }
          
          if (self$options$internal_validation) {
            self$.perform_validation(data_prepared)
          }
          
          # Generate summary and interpretation
          self$.generate_model_summary(model_results)
          self$.generate_interpretation(model_results)
          self$.generate_recommendations()
          
        }, error = function(e) {
          error_msg <- paste0(
            "<div style='color: red; background-color: #ffebee; padding: 15px; border-radius: 8px;'>",
            "<h4>Analysis Error</h4>",
            "<p><strong>Error:</strong> ", e$message, "</p>",
            "</div>"
          )
          self$results$todo$setContent(error_msg)
        })
      },
      
      # Data preparation ----
      .prepare_data = function() {
        
        tryCatch({
          data <- self$data
          
          # Extract variable names
          id_var <- self$options$id
          start_var <- self$options$start_time
          stop_var <- self$options$stop_time
          event_var <- self$options$event
          td_vars <- self$options$time_dependent_vars
          baseline_vars <- self$options$baseline_vars
          
          # Check essential variables
          if (!id_var %in% names(data)) {
            stop("Patient ID variable not found in data")
          }
          if (!stop_var %in% names(data)) {
            stop("Stop time variable not found in data")
          }
          if (!event_var %in% names(data)) {
            stop("Event variable not found in data")
          }
          
          # Create start time if not provided
          if (is.null(start_var) || start_var == "" || !start_var %in% names(data)) {
            data$start_time <- 0
            start_var <- "start_time"
          }
          
          # Prepare survival data
          survival_data <- data %>%
            dplyr::select(
              id = !!sym(id_var),
              start = !!sym(start_var),
              stop = !!sym(stop_var),
              event = !!sym(event_var),
              all_of(c(td_vars, baseline_vars))
            ) %>%
            dplyr::filter(!is.na(stop), !is.na(event)) %>%
            dplyr::arrange(id, start)
          
          # Convert event to numeric if needed
          if (is.factor(survival_data$event)) {
            survival_data$event <- as.numeric(survival_data$event) - 1
          }
          
          # Validate data
          if (nrow(survival_data) == 0) {
            stop("No valid survival data after preprocessing")
          }
          
          # Check time ordering
          invalid_times <- survival_data$start >= survival_data$stop
          if (any(invalid_times, na.rm = TRUE)) {
            stop("Start time must be less than stop time for all observations")
          }
          
          return(survival_data)
          
        }, error = function(e) {
          error_msg <- paste0(
            "<div style='color: red; background-color: #ffebee; padding: 15px; border-radius: 8px;'>",
            "<h4>Data Preparation Error</h4>",
            "<p><strong>Error:</strong> ", e$message, "</p>",
            "<p>Please check your data format and variable selections.</p>",
            "</div>"
          )
          self$results$todo$setContent(error_msg)
          return(NULL)
        })
      },
      
      # Model fitting ----
      .fit_models = function(data) {
        
        private$.checkpoint()
        
        tryCatch({
          td_vars <- self$options$time_dependent_vars
          baseline_vars <- self$options$baseline_vars
          all_vars <- c(td_vars, baseline_vars)
          
          if (length(all_vars) == 0) {
            stop("At least one covariate must be specified")
          }
          
          # Create formula
          formula_str <- paste("Surv(start, stop, event) ~", paste(all_vars, collapse = " + "))
          model_formula <- as.formula(formula_str)
          
          # Fit Cox model based on type
          if (self$options$model_type == "extended_cox") {
            # Time-varying coefficients model using timereg
            tryCatch({
              # Try timereg for time-varying effects
              timereg_formula <- as.formula(paste("Event(start, stop, event) ~", paste(all_vars, collapse = " + ")))
              model <- timereg::timecox(timereg_formula, data = data)
              
              # Store both timereg model and equivalent coxph for compatibility
              results$timereg_model <- model
              results$algorithm <- "timereg_cox"
              
              # Also fit regular Cox for comparison
              model <- survival::coxph(model_formula, data = data)
              
            }, error = function(e) {
              # Fallback to standard Cox with tt functions
              tt_functions <- self$.create_time_transform_functions(all_vars)
              if (length(tt_functions) > 0) {
                model <- survival::coxph(model_formula, data = data, tt = tt_functions)
              } else {
                model <- survival::coxph(model_formula, data = data)
              }
            })
          } else if (self$options$model_type == "cox") {
            # Standard Cox model
            model <- survival::coxph(model_formula, data = data)
          } else if (self$options$model_type == "landmark_cox") {
            # Will be handled in landmark analysis
            model <- survival::coxph(model_formula, data = data)
          }
          
          # Store model details
          results <- list(
            model = model,
            formula = model_formula,
            data = data,
            variables = all_vars,
            td_vars = td_vars,
            baseline_vars = baseline_vars
          )
          
          return(results)
          
        }, error = function(e) {
          stop("Model fitting failed: ", e$message)
        })
      },
      
      # Create time transform functions ----
      .create_time_transform_functions = function(vars) {
        
        if (self$options$time_transform == "none") {
          return(list())
        }
        
        tt_functions <- list()
        
        for (var in vars) {
          tt_function_name <- paste0("tt_", var)
          
          if (self$options$time_transform == "log") {
            tt_functions[[tt_function_name]] <- function(x, t, ...) {
              return(x * log(t + 1))
            }
          } else if (self$options$time_transform == "sqrt") {
            tt_functions[[tt_function_name]] <- function(x, t, ...) {
              return(x * sqrt(t))
            }
          } else if (self$options$time_transform == "poly") {
            tt_functions[[tt_function_name]] <- function(x, t, ...) {
              return(x * poly(t, 2))
            }
          }
        }
        
        return(tt_functions)
      },
      
      # Populate Cox results table ----
      .populate_cox_results = function(model_results) {
        
        model <- model_results$model
        conf_level <- self$options$confidence_level
        
        # Extract coefficient summary
        coef_summary <- summary(model)$coefficients
        conf_int <- confint(model, level = conf_level)
        
        if (is.null(coef_summary) || nrow(coef_summary) == 0) {
          return()
        }
        
        table <- self$results$cox_results
        
        for (i in 1:nrow(coef_summary)) {
          term <- rownames(coef_summary)[i]
          estimate <- coef_summary[i, "coef"]
          se <- coef_summary[i, "se(coef)"]
          z_stat <- coef_summary[i, "z"]
          p_value <- coef_summary[i, "Pr(>|z|)"]
          hr <- exp(estimate)
          conf_low <- exp(conf_int[i, 1])
          conf_high <- exp(conf_int[i, 2])
          
          table$addRow(rowKey = term, values = list(
            term = term,
            estimate = estimate,
            se = se,
            statistic = z_stat,
            p = p_value,
            conf_low = conf_low,
            conf_high = conf_high,
            hr = hr
          ))
        }
      },
      
      # Test time-varying effects ----
      .test_time_varying_effects = function(model_results) {
        
        private$.checkpoint()
        
        tryCatch({
          model <- model_results$model
          
          # Test proportional hazards assumption
          ph_test <- survival::cox.zph(model, transform = self$options$schoenfeld_transform)
          
          table <- self$results$time_varying_effects
          
          # Individual tests
          for (i in 1:nrow(ph_test$table)) {
            var_name <- rownames(ph_test$table)[i]
            if (var_name == "GLOBAL") next
            
            chisq <- ph_test$table[i, "chisq"]
            df <- ph_test$table[i, "df"]
            p_val <- ph_test$table[i, "p"]
            
            interpretation <- ifelse(p_val < 0.05, 
              "Time-varying effect detected", 
              "Proportional hazards assumption met")
            
            table$addRow(rowKey = var_name, values = list(
              variable = var_name,
              chisq = chisq,
              df = df,
              p_value = p_val,
              interpretation = interpretation
            ))
          }
          
          # Global test
          global_idx <- which(rownames(ph_test$table) == "GLOBAL")
          if (length(global_idx) > 0) {
            chisq <- ph_test$table[global_idx, "chisq"]
            df <- ph_test$table[global_idx, "df"]
            p_val <- ph_test$table[global_idx, "p"]
            
            interpretation <- ifelse(p_val < 0.05, 
              "Overall non-proportional hazards", 
              "Overall proportional hazards assumption met")
            
            table$addRow(rowKey = "GLOBAL", values = list(
              variable = "GLOBAL TEST",
              chisq = chisq,
              df = df,
              p_value = p_val,
              interpretation = interpretation
            ))
          }
          
        }, error = function(e) {
          # Silently handle errors in proportional hazards testing
        })
      },
      
      # Landmark analysis ----
      .perform_landmark_analysis = function(data) {
        
        private$.checkpoint()
        
        tryCatch({
          landmark_times_str <- self$options$landmark_times
          landmark_times <- as.numeric(unlist(strsplit(landmark_times_str, ",")))
          prediction_window <- self$options$prediction_window
          
          table <- self$results$landmark_results
          
          for (landmark_time in landmark_times) {
            # Filter data for landmark analysis
            landmark_data <- data %>%
              dplyr::filter(stop > landmark_time) %>%
              dplyr::mutate(
                start = pmax(start, landmark_time),
                stop = pmin(stop, landmark_time + prediction_window),
                event = ifelse(stop == landmark_time + prediction_window & 
                              event == 1 & stop < max(data$stop), 0, event)
              )
            
            n_at_risk <- nrow(landmark_data)
            
            if (n_at_risk < 10) next  # Skip if too few subjects
            
            # Fit landmark model
            all_vars <- c(self$options$time_dependent_vars, self$options$baseline_vars)
            formula_str <- paste("Surv(start, stop, event) ~", paste(all_vars, collapse = " + "))
            landmark_formula <- as.formula(formula_str)
            
            landmark_model <- survival::coxph(landmark_formula, data = landmark_data)
            
            # Extract results
            coef_summary <- summary(landmark_model)$coefficients
            conf_int <- confint(landmark_model, level = self$options$confidence_level)
            
            for (i in 1:nrow(coef_summary)) {
              term <- rownames(coef_summary)[i]
              estimate <- coef_summary[i, "coef"]
              hr <- exp(estimate)
              conf_low <- exp(conf_int[i, 1])
              conf_high <- exp(conf_int[i, 2])
              p_value <- coef_summary[i, "Pr(>|z|)"]
              
              row_key <- paste(landmark_time, term, sep = "_")
              
              table$addRow(rowKey = row_key, values = list(
                landmark_time = landmark_time,
                n_at_risk = n_at_risk,
                variable = term,
                estimate = estimate,
                hr = hr,
                conf_low = conf_low,
                conf_high = conf_high,
                p_value = p_value
              ))
            }
          }
          
        }, error = function(e) {
          # Handle landmark analysis errors
        })
      },
      
      # ROC analysis ----
      .perform_roc_analysis = function(data, model_results) {
        
        private$.checkpoint()
        
        tryCatch({
          roc_times_str <- self$options$roc_times
          roc_times <- as.numeric(unlist(strsplit(roc_times_str, ",")))
          
          # Get predictions from model
          linear_predictors <- predict(model_results$model, type = "lp")
          
          # Prepare data for timeROC
          roc_data <- data.frame(
            time = data$stop,
            event = data$event,
            marker = linear_predictors
          )
          
          # Remove rows with missing values
          roc_data <- roc_data[complete.cases(roc_data), ]
          
          if (nrow(roc_data) < 20) {
            return()  # Need sufficient data for ROC analysis
          }
          
          table <- self$results$roc_results
          
          for (time_point in roc_times) {
            if (time_point >= max(roc_data$time)) next
            
            # Calculate time-dependent ROC
            roc_result <- timeROC::timeROC(
              T = roc_data$time,
              delta = roc_data$event,
              marker = roc_data$marker,
              cause = 1,
              times = time_point,
              iid = TRUE
            )
            
            auc <- roc_result$AUC[2]  # AUC at specified time
            auc_se <- sqrt(roc_result$inference$vect_sd_1[2]^2)
            auc_lower <- auc - 1.96 * auc_se
            auc_upper <- auc + 1.96 * auc_se
            
            # Find optimal cutpoint if requested
            optimal_cutpoint <- NA
            sensitivity <- NA
            specificity <- NA
            
            if (self$options$optimal_cutpoint) {
              cutpoint_result <- self$.find_optimal_cutpoint(roc_data, time_point)
              if (!is.null(cutpoint_result)) {
                optimal_cutpoint <- cutpoint_result$cutpoint
                sensitivity <- cutpoint_result$sensitivity
                specificity <- cutpoint_result$specificity
              }
            }
            
            table$addRow(rowKey = as.character(time_point), values = list(
              time_point = time_point,
              variable = "Linear Predictor",
              auc = auc,
              auc_lower = auc_lower,
              auc_upper = auc_upper,
              optimal_cutpoint = optimal_cutpoint,
              sensitivity = sensitivity,
              specificity = specificity
            ))
          }
          
          # Store results for plotting
          private$.roc_results <- list(
            times = roc_times,
            data = roc_data,
            model = model_results$model
          )
          
        }, error = function(e) {
          # Handle ROC analysis errors
        })
      },
      
      # Find optimal cutpoint ----
      .find_optimal_cutpoint = function(data, time_point) {
        
        tryCatch({
          # Create binary outcome at time point
          at_risk <- data$time >= time_point
          event_by_time <- data$event == 1 & data$time <= time_point
          
          if (sum(event_by_time) < 5 || sum(at_risk & !event_by_time) < 5) {
            return(NULL)
          }
          
          # Calculate ROC for cutpoint optimization
          roc_obj <- pROC::roc(
            response = event_by_time[at_risk],
            predictor = data$marker[at_risk],
            quiet = TRUE
          )
          
          # Find optimal cutpoint based on method
          method <- self$options$cutpoint_method
          
          if (method == "youden") {
            coords_result <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
          } else if (method == "closest") {
            coords_result <- pROC::coords(roc_obj, "best", best.method = "closest.topleft", 
                                        ret = c("threshold", "sensitivity", "specificity"))
          } else {
            coords_result <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
          }
          
          return(list(
            cutpoint = coords_result$threshold,
            sensitivity = coords_result$sensitivity,
            specificity = coords_result$specificity
          ))
          
        }, error = function(e) {
          return(NULL)
        })
      },
      
      # Model comparison ----
      .compare_models = function(data) {
        
        private$.checkpoint()
        
        tryCatch({
          # Fit time-fixed model
          all_vars <- c(self$options$time_dependent_vars, self$options$baseline_vars)
          formula_str <- paste("Surv(start, stop, event) ~", paste(all_vars, collapse = " + "))
          fixed_model <- survival::coxph(as.formula(formula_str), data = data)
          
          # Current time-varying model
          varying_model <- private$.fitted_model$model
          
          # Calculate comparison metrics
          metric <- self$options$comparison_metric
          table <- self$results$model_comparison
          
          if (metric == "cindex") {
            # Calculate C-index for both models
            fixed_cindex <- survival::concordance(fixed_model)$concordance
            varying_cindex <- survival::concordance(varying_model)$concordance
            
            improvement <- varying_cindex - fixed_cindex
            
            table$addRow(rowKey = "fixed", values = list(
              model = "Time-Fixed Model",
              metric_value = fixed_cindex,
              se = NA,
              improvement = 0,
              p_value = NA
            ))
            
            table$addRow(rowKey = "varying", values = list(
              model = "Time-Varying Model",
              metric_value = varying_cindex,
              se = NA,
              improvement = improvement,
              p_value = NA
            ))
          }
          
          # Likelihood ratio test
          lrt <- anova(fixed_model, varying_model, test = "Chisq")
          if (nrow(lrt) > 1) {
            p_value <- lrt$`Pr(>Chi)`[2]
            
            # Update p-value in varying model row
            table$setRow(rowKey = "varying", values = list(p_value = p_value))
          }
          
        }, error = function(e) {
          # Handle model comparison errors
        })
      },
      
      # Validation ----
      .perform_validation = function(data) {
        
        private$.checkpoint()
        
        tryCatch({
          roc_times_str <- self$options$roc_times
          roc_times <- as.numeric(unlist(strsplit(roc_times_str, ",")))
          
          cv_folds <- self$options$cv_folds
          n_obs <- nrow(data)
          
          # Create folds
          fold_ids <- sample(rep(1:cv_folds, length.out = n_obs))
          
          table <- self$results$validation_results
          
          for (time_point in roc_times) {
            cv_aucs <- numeric(cv_folds)
            
            # Cross-validation
            for (fold in 1:cv_folds) {
              private$.checkpoint(flush = FALSE)
              
              train_data <- data[fold_ids != fold, ]
              test_data <- data[fold_ids == fold, ]
              
              if (nrow(train_data) < 20 || nrow(test_data) < 10) next
              
              tryCatch({
                # Fit model on training data
                all_vars <- c(self$options$time_dependent_vars, self$options$baseline_vars)
                formula_str <- paste("Surv(start, stop, event) ~", paste(all_vars, collapse = " + "))
                fold_model <- survival::coxph(as.formula(formula_str), data = train_data)
                
                # Predict on test data
                test_lp <- predict(fold_model, newdata = test_data, type = "lp")
                
                # Calculate ROC on test data
                test_roc_data <- data.frame(
                  time = test_data$stop,
                  event = test_data$event,
                  marker = test_lp
                )
                
                test_roc_data <- test_roc_data[complete.cases(test_roc_data), ]
                
                if (nrow(test_roc_data) >= 10) {
                  roc_result <- timeROC::timeROC(
                    T = test_roc_data$time,
                    delta = test_roc_data$event,
                    marker = test_roc_data$marker,
                    cause = 1,
                    times = time_point,
                    iid = FALSE
                  )
                  
                  cv_aucs[fold] <- roc_result$AUC[2]
                }
              }, error = function(e) {
                cv_aucs[fold] <<- NA
              })
            }
            
            # Calculate validation metrics
            cv_aucs <- cv_aucs[!is.na(cv_aucs)]
            if (length(cv_aucs) > 0) {
              cv_auc <- mean(cv_aucs)
              cv_se <- sd(cv_aucs) / sqrt(length(cv_aucs))
              
              # Apparent performance
              apparent_auc <- self$.calculate_apparent_auc(data, time_point)
              optimism <- apparent_auc - cv_auc
              corrected_auc <- apparent_auc - optimism
              
              table$addRow(rowKey = as.character(time_point), values = list(
                time_point = time_point,
                apparent_auc = apparent_auc,
                optimism = optimism,
                corrected_auc = corrected_auc,
                cv_auc = cv_auc,
                cv_se = cv_se
              ))
            }
          }
          
        }, error = function(e) {
          # Handle validation errors
        })
      },
      
      # Calculate apparent AUC ----
      .calculate_apparent_auc = function(data, time_point) {
        
        tryCatch({
          all_vars <- c(self$options$time_dependent_vars, self$options$baseline_vars)
          formula_str <- paste("Surv(start, stop, event) ~", paste(all_vars, collapse = " + "))
          model <- survival::coxph(as.formula(formula_str), data = data)
          
          # Try using riskRegression for enhanced AUC calculation
          if (requireNamespace("riskRegression", quietly = TRUE)) {
            auc_result <- riskRegression::Score(
              list("cox" = model),
              formula = as.formula(paste("Hist(stop, event) ~", "1")),
              data = data,
              times = time_point,
              metrics = "auc",
              summary = "risks"
            )
            
            if (!is.null(auc_result$AUC) && nrow(auc_result$AUC$score) > 0) {
              return(auc_result$AUC$score$AUC[1])
            }
          }
          
          # Fallback to timeROC
          lp <- predict(model, type = "lp")
          
          roc_data <- data.frame(
            time = data$stop,
            event = data$event,
            marker = lp
          )
          
          roc_data <- roc_data[complete.cases(roc_data), ]
          
          roc_result <- timeROC::timeROC(
            T = roc_data$time,
            delta = roc_data$event,
            marker = roc_data$marker,
            cause = 1,
            times = time_point,
            iid = FALSE
          )
          
          return(roc_result$AUC[2])
          
        }, error = function(e) {
          return(NA)
        })
      },
      
      # Generate model summary ----
      .generate_model_summary = function(model_results) {
        
        model <- model_results$model
        n_obs <- model$n
        n_events <- model$nevent
        
        # Model fit statistics
        concordance <- survival::concordance(model)
        loglik <- model$loglik
        aic <- -2 * loglik[2] + 2 * length(model$coefficients)
        
        summary_html <- paste0(
          "<div style='background-color: #e3f2fd; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
          "<h3 style='color: #1976d2; margin-top: 0;'>Time-Dependent Survival Analysis Summary</h3>",
          
          "<h4 style='color: #2c5aa0;'>Model Information:</h4>",
          "<ul>",
          "<li><strong>Model Type:</strong> ", 
          switch(self$options$model_type,
                 "cox" = "Cox Proportional Hazards",
                 "extended_cox" = "Extended Cox (Time-Varying Coefficients)",
                 "landmark_cox" = "Landmark Cox Model",
                 "joint" = "Joint Longitudinal-Survival Model"),
          "</li>",
          "<li><strong>Sample Size:</strong> ", n_obs, " observations</li>",
          "<li><strong>Events:</strong> ", n_events, " (", round(100 * n_events / n_obs, 1), "%)</li>",
          "<li><strong>Concordance:</strong> ", round(concordance$concordance, 3), 
          " (95% CI: ", round(concordance$concordance - 1.96 * sqrt(concordance$var), 3), "-",
          round(concordance$concordance + 1.96 * sqrt(concordance$var), 3), ")</li>",
          "<li><strong>AIC:</strong> ", round(aic, 1), "</li>",
          "</ul>",
          
          "<h4 style='color: #2c5aa0;'>Analysis Components:</h4>",
          "<ul>"
        )
        
        if (self$options$test_proportional_hazards) {
          summary_html <- paste0(summary_html, "<li>✓ Proportional hazards testing</li>")
        }
        
        if (self$options$perform_landmark) {
          summary_html <- paste0(summary_html, "<li>✓ Landmark analysis</li>")
        }
        
        if (self$options$time_dependent_roc) {
          summary_html <- paste0(summary_html, "<li>✓ Time-dependent ROC analysis</li>")
        }
        
        if (self$options$internal_validation) {
          summary_html <- paste0(summary_html, "<li>✓ Cross-validation</li>")
        }
        
        summary_html <- paste0(summary_html, "</ul></div>")
        
        self$results$model_summary$setContent(summary_html)
      },
      
      # Generate interpretation ----
      .generate_interpretation = function(model_results) {
        
        interpretation_html <- paste0(
          "<div style='background-color: #f3e5f5; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
          "<h3 style='color: #7b1fa2; margin-top: 0;'>Clinical Interpretation</h3>",
          
          "<h4 style='color: #8e24aa;'>Time-Dependent Effects:</h4>",
          "<p>This analysis evaluates how covariate effects change over time. ",
          "Time-dependent covariates allow for more realistic modeling of clinical scenarios ",
          "where treatment effects, biomarker relationships, or risk factors vary with follow-up duration.</p>",
          
          "<h4 style='color: #8e24aa;'>Key Findings:</h4>",
          "<ul>",
          "<li><strong>Model Performance:</strong> The concordance index indicates the model's discriminative ability.</li>",
          "<li><strong>Time-Varying Effects:</strong> Schoenfeld residual tests identify variables with non-proportional hazards.</li>",
          "<li><strong>Landmark Analysis:</strong> Provides risk assessments at specific time points.</li>",
          "<li><strong>Dynamic ROC:</strong> Shows how prediction accuracy changes over time.</li>",
          "</ul>",
          
          "<h4 style='color: #8e24aa;'>Clinical Applications:</h4>",
          "<ul>",
          "<li>Dynamic risk stratification for patient management</li>",
          "<li>Optimal timing for screening or intervention programs</li>",
          "<li>Personalized treatment selection based on time-varying biomarkers</li>",
          "<li>Adaptive clinical decision making throughout follow-up</li>",
          "</ul>",
          "</div>"
        )
        
        self$results$interpretation$setContent(interpretation_html)
      },
      
      # Generate recommendations ----
      .generate_recommendations = function() {
        
        recommendations_html <- paste0(
          "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>",
          "<h3 style='color: #2e7d32; margin-top: 0;'>Clinical Recommendations</h3>",
          
          "<h4 style='color: #388e3c;'>Model Validation:</h4>",
          "<ul>",
          "<li>Validate time-dependent predictions in external cohorts</li>",
          "<li>Assess calibration of risk predictions over time</li>",
          "<li>Consider bootstrap validation for optimism correction</li>",
          "</ul>",
          
          "<h4 style='color: #388e3c;'>Implementation:</h4>",
          "<ul>",
          "<li>Use landmark analysis for clinical decision points</li>",
          "<li>Update predictions as new covariate measurements become available</li>",
          "<li>Consider optimal cutpoints for risk stratification</li>",
          "</ul>",
          
          "<h4 style='color: #388e3c;'>Further Analysis:</h4>",
          "<ul>",
          "<li>Investigate significant time-varying effects clinically</li>",
          "<li>Consider interaction terms for treatment-time relationships</li>",
          "<li>Validate findings in independent patient cohorts</li>",
          "</ul>",
          "</div>"
        )
        
        self$results$recommendations$setContent(recommendations_html)
      },
      
      # Create welcome message ----
      .create_welcome_message = function() {
        
        paste0(
          "<div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 20px 0;'>",
          "<h3 style='color: #2e7d32; margin-top: 0;'>Welcome to Time-Dependent Covariates & ROC Analysis!</h3>",
          "<p><strong>Advanced survival analysis with time-varying effects and dynamic predictions</strong></p>",
          
          "<h4 style='color: #2e7d32;'>Required Variables:</h4>",
          "<ol>",
          "<li><strong>Patient ID:</strong> Unique identifier for each patient</li>",
          "<li><strong>Stop Time:</strong> Event or censoring time</li>",
          "<li><strong>Event Status:</strong> Event indicator (0=censored, 1=event)</li>",
          "<li><strong>Covariates:</strong> At least one time-dependent or baseline variable</li>",
          "</ol>",
          
          "<h4 style='color: #2e7d32;'>Key Features:</h4>",
          "<ul>",
          "<li><strong>Time-Varying Coefficients:</strong> Test for non-proportional hazards</li>",
          "<li><strong>Dynamic ROC:</strong> AUC curves over time with optimal cutpoints</li>",
          "<li><strong>Landmark Analysis:</strong> Risk assessment at specific time points</li>",
          "<li><strong>Cross-Validation:</strong> Internal validation of time-dependent predictions</li>",
          "</ul>",
          
          "<h4 style='color: #2e7d32;'>Clinical Applications:</h4>",
          "<ul>",
          "<li>Dynamic biomarker utilization for precision medicine</li>",
          "<li>Screening program optimization with time-dependent performance</li>",
          "<li>Treatment effect monitoring over time</li>",
          "<li>Adaptive clinical decision making</li>",
          "</ul>",
          
          "<p style='font-size: 12px; color: #555; margin-top: 20px;'>",
          "💡 <em>Start by selecting your patient ID, time variables, and covariates from the variable list.</em>",
          "</p>",
          "</div>"
        )
      },
      
      # Plotting functions ----
      .plot_time_varying = function(image, ggtheme, theme, ...) {
        
        if (is.null(private$.fitted_model) || !self$options$test_proportional_hazards) {
          return()
        }
        
        tryCatch({
          model <- private$.fitted_model$model
          ph_test <- survival::cox.zph(model)
          
          # Create Schoenfeld residual plots
          plot_data <- list()
          var_names <- names(model$coefficients)
          
          if (length(var_names) == 0) return()
          
          # Get residuals and time points
          resid_df <- data.frame(
            time = ph_test$time,
            ph_test$residuals
          )
          
          # Create plots for each variable
          plots <- list()
          for (i in seq_along(var_names)) {
            var_name <- var_names[i]
            resid_col <- paste0("X", i)
            
            if (resid_col %in% names(resid_df)) {
              plot_df <- data.frame(
                time = resid_df$time,
                residuals = resid_df[[resid_col]]
              )
              
              p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time, y = residuals)) +
                ggplot2::geom_point(alpha = 0.6, color = "#1976d2") +
                ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#d32f2f") +
                ggplot2::labs(
                  title = paste("Time-Varying Effect:", var_name),
                  x = "Time",
                  y = "Scaled Schoenfeld Residuals"
                ) +
                ggtheme
              
              plots[[var_name]] <- p
            }
          }
          
          if (length(plots) > 0) {
            # Show first plot or combine multiple plots
            final_plot <- plots[[1]]
            print(final_plot)
          }
          
          TRUE
          
        }, error = function(e) {
          FALSE
        })
      },
      
      .plot_roc_curves = function(image, ggtheme, theme, ...) {
        
        if (is.null(private$.roc_results)) {
          return()
        }
        
        tryCatch({
          roc_data <- private$.roc_results$data
          times <- private$.roc_results$times
          
          # Calculate ROC curves for plotting
          roc_curves <- list()
          
          for (time_point in times) {
            if (time_point >= max(roc_data$time)) next
            
            roc_result <- timeROC::timeROC(
              T = roc_data$time,
              delta = roc_data$event,
              marker = roc_data$marker,
              cause = 1,
              times = time_point,
              iid = FALSE
            )
            
            roc_df <- data.frame(
              FPR = roc_result$FP[, 2],
              TPR = roc_result$TP[, 2],
              time_point = paste("Time =", time_point),
              AUC = round(roc_result$AUC[2], 3)
            )
            
            roc_curves[[as.character(time_point)]] <- roc_df
          }
          
          if (length(roc_curves) == 0) return()
          
          # Combine all ROC curves
          combined_roc <- do.call(rbind, roc_curves)
          combined_roc$label <- paste0(combined_roc$time_point, " (AUC=", combined_roc$AUC, ")")
          
          # Create plot
          p <- ggplot2::ggplot(combined_roc, ggplot2::aes(x = FPR, y = TPR, color = label)) +
            ggplot2::geom_line(size = 1.2) +
            ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
            ggplot2::labs(
              title = "Time-Dependent ROC Curves",
              x = "False Positive Rate",
              y = "True Positive Rate",
              color = "Time Point"
            ) +
            ggplot2::xlim(0, 1) +
            ggplot2::ylim(0, 1) +
            ggplot2::theme(legend.position = "bottom") +
            ggtheme
          
          print(p)
          
          TRUE
          
        }, error = function(e) {
          FALSE
        })
      },
      
      .plot_auc_trajectory = function(image, ggtheme, theme, ...) {
        
        # Extract AUC values from results table
        tryCatch({
          roc_table <- self$results$roc_results
          
          if (roc_table$rowCount == 0) return()
          
          # Extract data from table
          auc_data <- data.frame(
            time_point = numeric(),
            auc = numeric(),
            auc_lower = numeric(),
            auc_upper = numeric()
          )
          
          for (i in 1:roc_table$rowCount) {
            row_data <- roc_table$get(i)
            auc_data <- rbind(auc_data, data.frame(
              time_point = row_data$time_point,
              auc = row_data$auc,
              auc_lower = row_data$auc_lower,
              auc_upper = row_data$auc_upper
            ))
          }
          
          if (nrow(auc_data) == 0) return()
          
          # Create AUC trajectory plot
          p <- ggplot2::ggplot(auc_data, ggplot2::aes(x = time_point, y = auc)) +
            ggplot2::geom_line(color = "#1976d2", size = 1.2) +
            ggplot2::geom_point(color = "#1976d2", size = 3) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = auc_lower, ymax = auc_upper), 
                               alpha = 0.3, fill = "#1976d2") +
            ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
            ggplot2::labs(
              title = "AUC Over Time",
              x = "Time Point",
              y = "Area Under the ROC Curve (AUC)",
              subtitle = "95% Confidence Intervals shown as shaded area"
            ) +
            ggplot2::ylim(0.3, 1.0) +
            ggtheme
          
          print(p)
          
          True
          
        }, error = function(e) {
          FALSE
        })
      },
      
      .plot_cutpoint_stability = function(image, ggtheme, theme, ...) {
        
        # Plot optimal cutpoints over time
        tryCatch({
          roc_table <- self$results$roc_results
          
          if (roc_table$rowCount == 0 || !self$options$optimal_cutpoint) return()
          
          # Extract cutpoint data
          cutpoint_data <- data.frame(
            time_point = numeric(),
            cutpoint = numeric(),
            sensitivity = numeric(),
            specificity = numeric()
          )
          
          for (i in 1:roc_table$rowCount) {
            row_data <- roc_table$get(i)
            if (!is.na(row_data$optimal_cutpoint)) {
              cutpoint_data <- rbind(cutpoint_data, data.frame(
                time_point = row_data$time_point,
                cutpoint = row_data$optimal_cutpoint,
                sensitivity = row_data$sensitivity,
                specificity = row_data$specificity
              ))
            }
          }
          
          if (nrow(cutpoint_data) == 0) return()
          
          # Create cutpoint stability plot
          p <- ggplot2::ggplot(cutpoint_data, ggplot2::aes(x = time_point, y = cutpoint)) +
            ggplot2::geom_line(color = "#d32f2f", size = 1.2) +
            ggplot2::geom_point(color = "#d32f2f", size = 3) +
            ggplot2::labs(
              title = "Optimal Cutpoint Stability Over Time",
              x = "Time Point",
              y = "Optimal Cutpoint Value",
              subtitle = paste("Cutpoint method:", self$options$cutpoint_method)
            ) +
            ggtheme
          
          print(p)
          
          TRUE
          
        }, error = function(e) {
          FALSE
        })
      },
      
      .plot_landmark_predictions = function(image, ggtheme, theme, ...) {
        
        # Create landmark survival curves
        tryCatch({
          if (is.null(private$.landmark_results) || !self$options$perform_landmark) {
            return()
          }
          
          # This would require more complex implementation
          # For now, return placeholder
          return()
          
        }, error = function(e) {
          FALSE
        })
      },
      
      .plot_schoenfeld_residuals = function(image, ggtheme, theme, ...) {
        
        # Plot Schoenfeld residuals for proportional hazards testing
        tryCatch({
          if (is.null(private$.fitted_model) || !self$options$test_proportional_hazards) {
            return()
          }
          
          model <- private$.fitted_model$model
          ph_test <- survival::cox.zph(model)
          
          # Plot residuals for first variable
          var_names <- names(model$coefficients)
          if (length(var_names) == 0) return()
          
          resid_data <- data.frame(
            time = ph_test$time,
            residuals = ph_test$residuals[, 1]
          )
          
          p <- ggplot2::ggplot(resid_data, ggplot2::aes(x = time, y = residuals)) +
            ggplot2::geom_point(alpha = 0.6, color = "#1976d2") +
            ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#d32f2f") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
            ggplot2::labs(
              title = paste("Schoenfeld Residuals:", var_names[1]),
              x = "Time",
              y = "Scaled Schoenfeld Residuals",
              subtitle = "Tests proportional hazards assumption"
            ) +
            ggtheme
          
          print(p)
          
          TRUE
          
        }, error = function(e) {
          FALSE
        })
      }
    )
  )