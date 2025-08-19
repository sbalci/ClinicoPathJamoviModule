#' @title Royston-Parmar Flexible Parametric Models Implementation
#' @description
#' Backend implementation class for Royston-Parmar flexible parametric survival models.
#' This R6 class provides comprehensive functionality for flexible parametric modeling
#' using restricted cubic splines to model the baseline log cumulative hazard function,
#' allowing for non-monotonic hazard functions while maintaining parametric advantages.
#' 
#' @details
#' The flexrstpm2Class implements Royston-Parmar flexible parametric models with:
#' 
#' \strong{Model Scales:}
#' - Proportional hazards scale (log cumulative hazard)
#' - Proportional odds scale (log cumulative odds)
#' - Probit scale (normal distribution transformation)
#' 
#' \strong{Flexible Modeling Features:}
#' - Restricted cubic splines for baseline function modeling
#' - Time-varying covariate effects through spline interactions
#' - Customizable knot placement and degrees of freedom
#' - Cure fraction modeling for long-term survivors
#' 
#' \strong{Advanced Capabilities:}
#' - Background hazard incorporation for relative survival
#' - Multiple link functions and transformation scales
#' - Time-dependent effects visualization
#' - Comprehensive model diagnostics and validation
#' 
#' \strong{Clinical Applications:}
#' - Cancer survival modeling with flexible hazard patterns
#' - Long-term follow-up studies with cure fractions
#' - Population-based survival analysis with background mortality
#' - Treatment effect evaluation with time-varying impacts
#' 
#' @seealso \code{\link{flexrstpm2}} for the main user interface function
#' @importFrom R6 R6Class
#' @import jmvcore
#' @keywords internal

flexrstpm2Class <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class(
    "flexrstpm2Class",
    inherit = flexrstpm2Base,
    private = list(
      
      # Model objects and results storage
      .rstpm2_model = NULL,
      .survival_curves = NULL,
      .hazard_curves = NULL,
      .time_varying_effects = NULL,
      .model_diagnostics = NULL,
      
      # Constants for analysis
      DEFAULT_DF = 4,
      DEFAULT_TVC_DF = 3,
      MIN_OBSERVATIONS = 30,
      MIN_EVENTS = 15,
      
      # Core initialization method
      .init = function() {
        # Initialize results with informative messages
        self$results$todo$setContent(
          paste0(
            "<h3>Royston-Parmar Flexible Parametric Models</h3>",
            "<p>This analysis implements flexible parametric survival models using restricted cubic splines and requires:</p>",
            "<ul>",
            "<li><b>Time variable:</b> Survival time or follow-up duration</li>",
            "<li><b>Event variable:</b> Event indicator (0/1, FALSE/TRUE, or factor)</li>",
            "<li><b>Covariates:</b> Variables for flexible parametric modeling</li>",
            "</ul>",
            "<p><strong>Key Features:</strong></p>",
            "<ul>",
            "<li>Flexible baseline hazard through restricted cubic splines</li>",
            "<li>Time-varying covariate effects with spline interactions</li>",
            "<li>Multiple scales: proportional hazards, odds, and probit</li>",
            "<li>Smooth survival and hazard function estimation</li>",
            "<li>Direct parametric modeling advantages for extrapolation</li>",
            "</ul>",
            "<p>Configure spline degrees of freedom and select variables to begin flexible parametric analysis.</p>"
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
        required_packages <- c("rstpm2", "survival", "splines")
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
              "<p>The following packages are required for Royston-Parmar flexible parametric models:</p>",
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
          
          # Fit Royston-Parmar flexible parametric model
          model_results <- private$.fitFlexibleParametricModel(data_prep)
          
          # Extract time-varying effects if specified
          time_varying_results <- private$.extractTimeVaryingEffects(model_results, data_prep)
          
          # Generate comprehensive results
          private$.populateResults(model_results, time_varying_results, data_prep)
          
          # Create visualizations
          private$.createPlots(model_results, time_varying_results, data_prep)
          
          # Generate summaries if requested
          if (self$options$showSummaries) {
            private$.generateSummaries(model_results, time_varying_results)
          }
          
          # Generate explanations if requested
          if (self$options$showExplanations) {
            private$.generateExplanations()
          }
          
        }, error = function(e) {
          self$results$todo$setContent(
            paste0(
              "<h3>Analysis Error</h3>",
              "<p>An error occurred during flexible parametric analysis:</p>",
              "<pre>", htmlspecialchars(e$message), "</pre>",
              "<p>Please check your data and model specifications.</p>"
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
        
        if (is.null(self$options$covariates) || length(self$options$covariates) == 0) {
          return(list(valid = FALSE, message = "At least one covariate is required"))
        }
        
        # Check minimum observations
        if (nrow(self$data) < private$MIN_OBSERVATIONS) {
          return(list(
            valid = FALSE, 
            message = paste("At least", private$MIN_OBSERVATIONS, "observations required for flexible parametric analysis")
          ))
        }
        
        # Validate degrees of freedom
        df <- self$options$df
        if (df < 1 || df > 10) {
          return(list(valid = FALSE, message = "Degrees of freedom must be between 1 and 10"))
        }
        
        return(list(valid = TRUE, message = ""))
      },
      
      # Data preparation for flexible parametric analysis
      .prepareData = function() {
        # Extract variables
        time_var <- self$options$elapsedtime
        event_var <- self$options$outcome
        covariate_vars <- self$options$covariates
        tvc_vars <- self$options$time_varying_covariates
        bhazard_var <- self$options$bhazard
        
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
        
        # Check minimum events
        n_events <- sum(event_numeric, na.rm = TRUE)
        if (n_events < private$MIN_EVENTS) {
          self$results$todo$setContent(
            paste0(
              "<h3>Data Error</h3>",
              "<p>Insufficient events (", n_events, ") for reliable flexible parametric analysis.</p>",
              "<p>At least ", private$MIN_EVENTS, " events are required.</p>"
            )
          )
          return(NULL)
        }
        
        # Get covariate data
        covariate_data <- self$data[covariate_vars]
        
        # Handle factors properly
        for (i in seq_along(covariate_data)) {
          if (is.character(covariate_data[[i]])) {
            covariate_data[[i]] <- as.factor(covariate_data[[i]])
          }
        }
        
        # Handle time-varying covariates
        tvc_data <- NULL
        if (!is.null(tvc_vars) && length(tvc_vars) > 0) {
          tvc_data <- self$data[tvc_vars]
          for (i in seq_along(tvc_data)) {
            if (is.character(tvc_data[[i]])) {
              tvc_data[[i]] <- as.factor(tvc_data[[i]])
            }
          }
        }
        
        # Handle background hazard
        bhazard_data <- NULL
        if (!is.null(bhazard_var) && bhazard_var != "") {
          bhazard_data <- self$data[[bhazard_var]]
          if (!is.numeric(bhazard_data)) {
            self$results$todo$setContent(
              "<h3>Data Error</h3><p>Background hazard variable must be numeric.</p>"
            )
            return(NULL)
          }
        }
        
        # Create complete dataset
        complete_data <- data.frame(
          time = time_data,
          event = event_numeric,
          covariate_data
        )
        
        if (!is.null(tvc_data)) {
          complete_data <- cbind(complete_data, tvc_data)
        }
        
        if (!is.null(bhazard_data)) {
          complete_data$bhazard <- bhazard_data
        }
        
        complete_rows <- complete.cases(complete_data)
        
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
          complete_data = complete_data[complete_rows, ],
          time = time_data[complete_rows],
          event = event_numeric[complete_rows],
          n_obs = sum(complete_rows),
          n_events = sum(event_numeric[complete_rows]),
          complete_rows = complete_rows,
          covariate_names = covariate_vars,
          tvc_names = tvc_vars,
          has_bhazard = !is.null(bhazard_data),
          bhazard_name = bhazard_var
        )
      },
      
      # Fit Royston-Parmar flexible parametric model
      .fitFlexibleParametricModel = function(data_prep) {
        # Get model specifications
        scale <- self$options$scale
        df <- self$options$df
        tvc_df <- self$options$tvc_df
        cure_fraction <- self$options$cure_fraction
        link_function <- self$options$link_function
        robust_se <- self$options$robust_se
        
        # Parse knots if specified
        knots <- NULL
        if (!is.null(self$options$knots) && self$options$knots != "") {
          knots <- as.numeric(strsplit(self$options$knots, ",")[[1]])
        }
        
        boundary_knots <- NULL
        if (!is.null(self$options$boundary_knots) && self$options$boundary_knots != "") {
          boundary_knots <- as.numeric(strsplit(self$options$boundary_knots, ",")[[1]])
        }
        
        # Build formula
        covar_names <- data_prep$covariate_names
        tvc_names <- data_prep$tvc_names
        
        # Base formula
        formula_parts <- c("survival", "~")
        
        # Add covariates
        if (length(covar_names) > 0) {
          formula_parts <- c(formula_parts, paste(covar_names, collapse = " + "))
        }
        
        # Add time-varying effects if specified
        if (!is.null(tvc_names) && length(tvc_names) > 0) {
          if (length(covar_names) > 0) {
            formula_parts <- c(formula_parts, " + ")
          }
          tvc_terms <- paste0("tt(", tvc_names, ")")
          formula_parts <- c(formula_parts, paste(tvc_terms, collapse = " + "))
        }
        
        model_formula <- as.formula(paste(formula_parts, collapse = ""))
        
        # Fit the model using rstpm2
        rstpm2_fit <- rstpm2::stpm2(
          formula = model_formula,
          data = data_prep$complete_data,
          df = df,
          scale = scale,
          link.type = link_function,
          knots = knots,
          boundary.knots = boundary_knots,
          cure = cure_fraction,
          bhazard = if (data_prep$has_bhazard) data_prep$bhazard_name else NULL,
          tvc = if (!is.null(tvc_names)) list(tvc_names = tvc_df) else NULL,
          robust = robust_se
        )
        
        # Extract model summary
        model_summary <- summary(rstpm2_fit)
        
        list(
          model = rstpm2_fit,
          summary = model_summary,
          formula = model_formula,
          scale = scale,
          df = df,
          tvc_df = tvc_df,
          knots = knots,
          boundary_knots = boundary_knots,
          cure_fraction = cure_fraction,
          link_function = link_function,
          robust_se = robust_se
        )
      },
      
      # Extract time-varying effects
      .extractTimeVaryingEffects = function(model_results, data_prep) {
        if (is.null(data_prep$tvc_names) || length(data_prep$tvc_names) == 0) {
          return(NULL)
        }
        
        rstpm2_model <- model_results$model
        tvc_names <- data_prep$tvc_names
        
        # Define time points for evaluation
        max_time <- max(data_prep$time)
        time_points <- seq(0.1, max_time, length.out = 50)
        
        tvc_results <- list()
        
        tryCatch({
          for (var_name in tvc_names) {
            # Extract time-varying effects for this variable
            newdata <- data.frame(time_points)
            colnames(newdata) <- "time"
            
            # Add other covariates at their mean/mode values
            for (cov in data_prep$covariate_names) {
              if (is.numeric(data_prep$complete_data[[cov]])) {
                newdata[[cov]] <- mean(data_prep$complete_data[[cov]], na.rm = TRUE)
              } else {
                # Use most frequent factor level
                newdata[[cov]] <- names(sort(table(data_prep$complete_data[[cov]]), decreasing = TRUE))[1]
              }
            }
            
            # Set the time-varying covariate of interest
            if (var_name %in% data_prep$tvc_names) {
              if (is.numeric(data_prep$complete_data[[var_name]])) {
                newdata[[var_name]] <- 1  # Unit change
              } else {
                newdata[[var_name]] <- levels(data_prep$complete_data[[var_name]])[2]  # Second level
              }
            }
            
            # Predict time-varying effects
            tvc_effects <- predict(rstpm2_model, newdata = newdata, type = "hr", se.fit = TRUE)
            
            tvc_results[[var_name]] <- data.frame(
              time = time_points,
              effect = tvc_effects$fit,
              se = tvc_effects$se.fit,
              lower = tvc_effects$fit - 1.96 * tvc_effects$se.fit,
              upper = tvc_effects$fit + 1.96 * tvc_effects$se.fit
            )
          }
        }, error = function(e) {
          # Return NULL if time-varying effects cannot be calculated
          return(NULL)
        })
        
        tvc_results
      },
      
      # Initialize result tables
      .initializeResultTables = function() {
        # Initialize tables with empty structure
        if (self$options$show_coefficients_table) {
          self$results$coefficientsTable$setKeys(character(0))
        }
        
        if (!is.null(self$options$time_varying_covariates) && 
            length(self$options$time_varying_covariates) > 0 && 
            self$options$show_coefficients_table) {
          self$results$timeVaryingTable$setKeys(character(0))
        }
        
        self$results$modelFit$setKeys(c("loglik", "aic", "bic", "df"))
        self$results$splineInfo$setKeys(c("baseline", "tvc"))
      },
      
      # Populate all result tables and summaries
      .populateResults = function(model_results, time_varying_results, data_prep) {
        # Populate model summary
        private$.populateModelSummary(model_results, data_prep)
        
        # Populate coefficients table
        if (self$options$show_coefficients_table) {
          private$.populateCoefficientsTable(model_results)
        }
        
        # Populate time-varying effects table
        if (!is.null(time_varying_results) && self$options$show_coefficients_table) {
          private$.populateTimeVaryingTable(time_varying_results)
        }
        
        # Populate model fit statistics
        private$.populateModelFit(model_results)
        
        # Populate spline information
        private$.populateSplineInfo(model_results)
      },
      
      # Populate model summary
      .populateModelSummary = function(model_results, data_prep) {
        scale_text <- switch(model_results$scale,
          "hazard" = "Proportional Hazards",
          "odds" = "Proportional Odds",
          "normal" = "Probit (Normal)"
        )
        
        summary_content <- paste0(
          "<h4>Royston-Parmar Flexible Parametric Model Results</h4>",
          "<p><strong>Model Scale:</strong> ", scale_text, "</p>",
          "<p><strong>Link Function:</strong> ", model_results$link_function, "</p>",
          "<p><strong>Observations:</strong> ", data_prep$n_obs, 
          " (", data_prep$n_events, " events)</p>",
          "<p><strong>Baseline Spline DF:</strong> ", model_results$df, "</p>",
          if (!is.null(data_prep$tvc_names) && length(data_prep$tvc_names) > 0) {
            paste0(
              "<p><strong>Time-Varying Covariates:</strong> ", 
              paste(data_prep$tvc_names, collapse = ", "), 
              " (DF: ", model_results$tvc_df, ")</p>"
            )
          } else "",
          if (!is.null(model_results$knots)) {
            paste0("<p><strong>Knots:</strong> ", paste(model_results$knots, collapse = ", "), "</p>")
          } else "",
          if (model_results$cure_fraction) "<p><strong>Cure Fraction:</strong> Included</p>" else "",
          if (data_prep$has_bhazard) "<p><strong>Background Hazard:</strong> Included</p>" else "",
          "<p><strong>Robust SE:</strong> ", ifelse(model_results$robust_se, "Yes", "No"), "</p>"
        )
        
        self$results$modelSummary$setContent(summary_content)
      },
      
      # Populate coefficients table
      .populateCoefficientsTable = function(model_results) {
        tryCatch({
          model_summary <- model_results$summary
          
          # Extract coefficients table
          if (!is.null(model_summary$coef)) {
            coef_table <- model_summary$coef
            
            for (i in seq_len(nrow(coef_table))) {
              term_name <- rownames(coef_table)[i]
              
              self$results$coefficientsTable$addRow(rowKey = i, values = list(
                term = term_name,
                coefficient = coef_table[i, "coef"],
                se = coef_table[i, "se(coef)"],
                z_value = coef_table[i, "z"],
                p_value = coef_table[i, "Pr(>|z|)"],
                lower_ci = coef_table[i, "lower .95"],
                upper_ci = coef_table[i, "upper .95"]
              ))
            }
          }
        }, error = function(e) {
          # Skip coefficient table on error
        })
      },
      
      # Populate time-varying effects table
      .populateTimeVaryingTable = function(time_varying_results) {
        if (is.null(time_varying_results)) {
          return()
        }
        
        row_index <- 1
        for (var_name in names(time_varying_results)) {
          tvc_data <- time_varying_results[[var_name]]
          
          # Sample key time points
          n_points <- min(10, nrow(tvc_data))
          sampled_indices <- round(seq(1, nrow(tvc_data), length.out = n_points))
          
          for (idx in sampled_indices) {
            self$results$timeVaryingTable$addRow(rowKey = row_index, values = list(
              covariate = var_name,
              time_point = tvc_data$time[idx],
              effect = tvc_data$effect[idx],
              se = tvc_data$se[idx],
              lower_ci = tvc_data$lower[idx],
              upper_ci = tvc_data$upper[idx]
            ))
            row_index <- row_index + 1
          }
        }
      },
      
      # Populate model fit statistics
      .populateModelFit = function(model_results) {
        model_summary <- model_results$summary
        rstpm2_model <- model_results$model
        
        statistics <- list(
          "Log-likelihood" = list(
            value = if (!is.null(rstpm2_model$loglik)) rstpm2_model$loglik else NA,
            interpretation = "Higher values indicate better fit"
          ),
          "AIC" = list(
            value = if (!is.null(rstpm2_model$aic)) rstpm2_model$aic else NA,
            interpretation = "Lower values indicate better model selection"
          ),
          "BIC" = list(
            value = if (!is.null(rstpm2_model$bic)) rstpm2_model$bic else NA,
            interpretation = "Lower values favor more parsimonious models"
          ),
          "Model DF" = list(
            value = if (!is.null(rstpm2_model$df)) rstpm2_model$df else NA,
            interpretation = "Total model degrees of freedom"
          )
        )
        
        for (i in seq_along(statistics)) {
          stat_name <- names(statistics)[i]
          stat_info <- statistics[[i]]
          
          if (!is.na(stat_info$value)) {
            self$results$modelFit$addRow(rowKey = i, values = list(
              statistic = stat_name,
              value = stat_info$value,
              interpretation = stat_info$interpretation
            ))
          }
        }
      },
      
      # Populate spline information
      .populateSplineInfo = function(model_results) {
        # Baseline spline info
        self$results$splineInfo$addRow(rowKey = 1, values = list(
          component = "Baseline Function",
          df = as.integer(model_results$df),
          knots = if (!is.null(model_results$knots)) paste(model_results$knots, collapse = ", ") else "Default",
          boundary_knots = if (!is.null(model_results$boundary_knots)) paste(model_results$boundary_knots, collapse = ", ") else "Default"
        ))
        
        # Time-varying covariate spline info
        if (!is.null(self$options$time_varying_covariates) && 
            length(self$options$time_varying_covariates) > 0) {
          self$results$splineInfo$addRow(rowKey = 2, values = list(
            component = "Time-Varying Effects",
            df = as.integer(model_results$tvc_df),
            knots = "Default",
            boundary_knots = "Default"
          ))
        }
      },
      
      # Create visualization plots
      .createPlots = function(model_results, time_varying_results, data_prep) {
        if (self$options$show_survival_curves) {
          private$.createSurvivalCurves(model_results, data_prep)
        }
        
        if (self$options$show_hazard_curves) {
          private$.createHazardCurves(model_results, data_prep)
        }
        
        if (!is.null(time_varying_results) && self$options$show_time_varying_plots) {
          private$.createTimeVaryingPlots(time_varying_results)
        }
        
        if (self$options$show_model_diagnostics) {
          private$.createModelDiagnostics(model_results)
        }
        
        if (self$options$show_residuals) {
          private$.createResidualPlots(model_results)
        }
      },
      
      # Create survival curves
      .createSurvivalCurves = function(model_results, data_prep) {
        tryCatch({
          image <- self$results$survivalCurves
          
          image$setState(list(
            width = 800,
            height = 600,
            model_results = model_results,
            data_prep = data_prep
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create hazard curves
      .createHazardCurves = function(model_results, data_prep) {
        tryCatch({
          image <- self$results$hazardCurves
          
          image$setState(list(
            width = 800,
            height = 600,
            model_results = model_results,
            data_prep = data_prep
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create time-varying effect plots
      .createTimeVaryingPlots = function(time_varying_results) {
        tryCatch({
          image <- self$results$timeVaryingPlots
          
          image$setState(list(
            width = 800,
            height = 600,
            time_varying_results = time_varying_results
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create model diagnostics
      .createModelDiagnostics = function(model_results) {
        tryCatch({
          image <- self$results$modelDiagnostics
          
          image$setState(list(
            width = 800,
            height = 600,
            model_results = model_results
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create residual plots
      .createResidualPlots = function(model_results) {
        tryCatch({
          image <- self$results$residualPlots
          
          image$setState(list(
            width = 800,
            height = 600,
            model_results = model_results
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Generate natural language summaries
      .generateSummaries = function(model_results, time_varying_results) {
        scale_text <- switch(model_results$scale,
          "hazard" = "proportional hazards",
          "odds" = "proportional odds",
          "normal" = "probit"
        )
        
        summary_text <- paste0(
          "<h4>Analysis Summary</h4>",
          "<p>Royston-Parmar flexible parametric survival analysis was performed using the ",
          scale_text, " scale with restricted cubic splines to model the baseline function. ",
          "This approach provides flexible modeling of the baseline hazard while maintaining ",
          "parametric advantages for extrapolation and smooth curve estimation.</p>",
          
          "<p><strong>Model Specification:</strong> ",
          "The baseline function was modeled using ", model_results$df, " degrees of freedom ",
          "with ", model_results$link_function, " link function. ",
          if (!is.null(time_varying_results)) {
            paste0("Time-varying effects were included for ", length(time_varying_results), " covariates ",
                   "using ", model_results$tvc_df, " degrees of freedom for spline interactions. ")
          } else "All covariate effects were assumed to be time-constant. ",
          if (model_results$cure_fraction) "A cure fraction was incorporated to model long-term survivors. " else "",
          "</p>",
          
          if (!is.null(time_varying_results)) {
            paste0(
              "<p><strong>Time-Varying Effects:</strong> ",
              "Spline-based interactions were used to model time-varying covariate effects. ",
              "These effects show how the impact of covariates changes over the follow-up period, ",
              "providing insights into early vs late effects of risk factors or treatments.</p>"
            )
          } else "",
          
          "<p><strong>Clinical Interpretation:</strong> ",
          "The flexible parametric approach allows for smooth estimation of survival and hazard functions ",
          "while accommodating complex hazard patterns that may not fit standard parametric distributions. ",
          "This is particularly valuable for long-term follow-up studies where hazard patterns may be ",
          "non-monotonic or require extrapolation beyond the observed data.</p>"
        )
        
        self$results$analysisSummary$setContent(summary_text)
      },
      
      # Generate methodology explanations
      .generateExplanations = function() {
        explanations <- paste0(
          "<h4>Royston-Parmar Flexible Parametric Model Methodology</h4>",
          
          "<h5>Overview</h5>",
          "<p>Royston-Parmar flexible parametric models extend traditional parametric survival models ",
          "by using restricted cubic splines to model the baseline function. This provides flexibility ",
          "in the shape of the baseline hazard while maintaining the advantages of parametric modeling:</p>",
          "<ul>",
          "<li>Smooth survival and hazard function estimates</li>",
          "<li>Direct modeling of survival times</li>",
          "<li>Extrapolation beyond observed follow-up</li>",
          "<li>Efficient parameter estimation</li>",
          "</ul>",
          
          "<h5>Model Scales</h5>",
          "<ul>",
          "<li><strong>Proportional Hazards Scale:</strong> Models log cumulative hazard function, similar to Cox models</li>",
          "<li><strong>Proportional Odds Scale:</strong> Models log cumulative odds of survival</li>",
          "<li><strong>Probit Scale:</strong> Uses normal distribution transformation</li>",
          "</ul>",
          
          "<h5>Spline Methodology</h5>",
          "<p>Restricted cubic splines are used to model the transformation of the baseline function:</p>",
          "<ul>",
          "<li><strong>Degrees of Freedom:</strong> Control flexibility vs smoothness trade-off</li>",
          "<li><strong>Knot Placement:</strong> Can be automatic (quantile-based) or user-specified</li>",
          "<li><strong>Boundary Knots:</strong> Define the range over which splines are fitted</li>",
          "<li><strong>Smoothness Constraints:</strong> Ensure smooth transitions between spline segments</li>",
          "</ul>",
          
          "<h5>Time-Varying Effects</h5>",
          "<p>When specified, time-varying covariate effects are modeled using spline interactions:</p>",
          "<p><strong>log h(t|x) = s₀(log t) + Σᵢ βᵢxᵢ + Σⱼ sⱼ(log t)xⱼ</strong></p>",
          "<p>where s₀(log t) is the baseline spline and sⱼ(log t) are time-varying effect splines.</p>",
          
          "<h5>Advanced Features</h5>",
          "<ul>",
          "<li><strong>Cure Fractions:</strong> Model long-term survivors who will never experience the event</li>",
          "<li><strong>Background Hazard:</strong> Incorporate population mortality rates for relative survival</li>",
          "<li><strong>Multiple Link Functions:</strong> Choose appropriate transformation for the data</li>",
          "<li><strong>Robust Standard Errors:</strong> Account for model uncertainty and potential misspecification</li>",
          "</ul>",
          
          "<h5>Model Selection</h5>",
          "<ul>",
          "<li><strong>Degrees of Freedom:</strong> Balance model complexity with goodness of fit</li>",
          "<li><strong>Information Criteria:</strong> Use AIC/BIC for model comparison</li>",
          "<li><strong>Residual Analysis:</strong> Check model assumptions and adequacy</li>",
          "<li><strong>Cross-validation:</strong> Assess predictive performance</li>",
          "</ul>",
          
          "<h5>Clinical Applications</h5>",
          "<ul>",
          "<li>Cancer survival modeling with complex hazard patterns</li>",
          "<li>Long-term follow-up studies requiring extrapolation</li>",
          "<li>Population-based survival analysis with background mortality</li>",
          "<li>Treatment effect evaluation with time-varying impacts</li>",
          "<li>Health economic modeling requiring parametric survival functions</li>",
          "</ul>"
        )
        
        self$results$methodExplanation$setContent(explanations)
      }
      
    )
  ) # End R6Class