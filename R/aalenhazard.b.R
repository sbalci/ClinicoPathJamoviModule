#' @title Aalen's Additive Hazard Models Implementation
#' @description
#' Backend implementation class for Aalen's additive hazard regression models.
#' This R6 class provides comprehensive functionality for non-proportional hazards
#' modeling using additive covariate effects, allowing for time-varying coefficients
#' and detailed analysis of cumulative regression functions over time.
#' 
#' @details
#' The aalenhazardClass implements Aalen's additive hazard regression with:
#' 
#' \strong{Model Types:}
#' - Additive hazard model with time-varying coefficients
#' - Semi-parametric Aalen model with mixed constant/time-varying effects
#' - Non-parametric Aalen model for exploratory analysis
#' 
#' \strong{Key Features:}
#' - Non-proportional hazards analysis without PH assumption
#' - Time-varying covariate effects estimation
#' - Cumulative regression function plots
#' - Tests for constant effects over time
#' - Robust standard error estimation
#' 
#' \strong{Statistical Methods:}
#' - Cumulative regression coefficient estimation
#' - Kolmogorov-Smirnov tests for constant effects
#' - Model goodness-of-fit assessment
#' - Residual analysis and diagnostics
#' 
#' \strong{Clinical Applications:}
#' - Analysis when proportional hazards assumption fails
#' - Investigation of time-varying treatment effects
#' - Exploration of covariate effect patterns over time
#' - Non-parametric survival regression modeling
#' 
#' @seealso \code{\link{aalenhazard}} for the main user interface function
#' @importFrom R6 R6Class
#' @import jmvcore
#' @keywords internal

aalenhazardClass <- if (requireNamespace('jmvcore', quietly=TRUE))
  R6::R6Class(
    "aalenhazardClass",
    inherit = aalenhazardBase,
    private = list(
      
      # Model objects and results storage
      .aalen_model = NULL,
      .test_results = NULL,
      .cumulative_functions = NULL,
      .residuals = NULL,
      .model_comparison = NULL,
      
      # Constants for analysis
      DEFAULT_BANDWIDTH = 1.0,
      MIN_OBSERVATIONS = 20,
      MIN_EVENTS = 10,
      
      # Core initialization method
      .init = function() {
        # Initialize results with informative messages
        self$results$todo$setContent(
          paste0(
            "<h3>Aalen's Additive Hazard Models</h3>",
            "<p>This analysis implements non-proportional hazards regression with additive effects and requires:</p>",
            "<ul>",
            "<li><b>Time variable:</b> Survival time or follow-up duration</li>",
            "<li><b>Event variable:</b> Event indicator (0/1, FALSE/TRUE, or factor)</li>",
            "<li><b>Covariates:</b> Variables for additive hazard modeling</li>",
            "</ul>",
            "<p><strong>Key Advantages:</strong></p>",
            "<ul>",
            "<li>No proportional hazards assumption required</li>",
            "<li>Time-varying covariate effects allowed</li>",
            "<li>Cumulative regression function estimation</li>",
            "<li>Robust to model misspecification</li>",
            "</ul>",
            "<p>Select variables to begin Aalen additive hazard analysis.</p>"
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
        required_packages <- c("timereg", "survival")
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
              "<p>The following packages are required for Aalen's additive hazard models:</p>",
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
          
          # Fit Aalen additive hazard model
          model_results <- private$.fitAalenModel(data_prep)
          
          # Perform statistical tests
          test_results <- private$.performTests(model_results, data_prep)
          
          # Generate comprehensive results
          private$.populateResults(model_results, test_results, data_prep)
          
          # Create visualizations
          private$.createPlots(model_results, test_results, data_prep)
          
          # Generate summaries if requested
          if (self$options$showSummaries) {
            private$.generateSummaries(model_results, test_results)
          }
          
          # Generate explanations if requested
          if (self$options$showExplanations) {
            private$.generateExplanations()
          }
          
        }, error = function(e) {
          self$results$todo$setContent(
            paste0(
              "<h3>Analysis Error</h3>",
              "<p>An error occurred during Aalen's additive hazard analysis:</p>",
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
        
        if (is.null(self$options$covariates) || length(self$options$covariates) == 0) {
          return(list(valid = FALSE, message = "At least one covariate is required"))
        }
        
        # Check minimum observations
        if (nrow(self$data) < private$MIN_OBSERVATIONS) {
          return(list(
            valid = FALSE, 
            message = paste("At least", private$MIN_OBSERVATIONS, "observations required for Aalen analysis")
          ))
        }
        
        return(list(valid = TRUE, message = ""))
      },
      
      # Data preparation for Aalen analysis
      .prepareData = function() {
        # Extract variables
        time_var <- self$options$elapsedtime
        event_var <- self$options$outcome
        covariate_vars <- self$options$covariates
        constant_vars <- self$options$constant_effects
        
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
              "<p>Insufficient events (", n_events, ") for reliable Aalen analysis.</p>",
              "<p>At least ", private$MIN_EVENTS, " events are required.</p>"
            )
          )
          return(NULL)
        }
        
        # Get covariate data
        covariate_data <- self$data[covariate_vars]
        
        # Convert factors to numeric if needed (preserve factor structure for timereg)
        covariate_matrix <- as.data.frame(covariate_data)
        for (i in seq_along(covariate_matrix)) {
          if (is.character(covariate_matrix[[i]])) {
            covariate_matrix[[i]] <- as.factor(covariate_matrix[[i]])
          }
        }
        
        # Handle constant effects variables
        constant_data <- NULL
        if (!is.null(constant_vars) && length(constant_vars) > 0) {
          constant_data <- self$data[constant_vars]
          for (i in seq_along(constant_data)) {
            if (is.character(constant_data[[i]])) {
              constant_data[[i]] <- as.factor(constant_data[[i]])
            }
          }
        }
        
        # Remove rows with missing values
        complete_data <- data.frame(
          time = time_data,
          event = event_numeric,
          covariate_matrix
        )
        
        if (!is.null(constant_data)) {
          complete_data <- cbind(complete_data, constant_data)
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
          covariates = covariate_matrix[complete_rows, , drop = FALSE],
          constant_effects = if (!is.null(constant_data)) constant_data[complete_rows, , drop = FALSE] else NULL,
          time = time_data[complete_rows],
          event = event_numeric[complete_rows],
          complete_data = complete_data[complete_rows, ],
          n_obs = sum(complete_rows),
          n_events = sum(event_numeric[complete_rows]),
          complete_rows = complete_rows,
          covariate_names = covariate_vars,
          constant_names = constant_vars
        )
      },
      
      # Fit Aalen additive hazard model
      .fitAalenModel = function(data_prep) {
        model_type <- self$options$model_type
        robust_se <- self$options$robust_se
        bandwidth <- self$options$bandwidth
        
        # Prepare formula components
        surv_obj <- data_prep$survival
        covariates <- data_prep$covariates
        constant_effects <- data_prep$constant_effects
        complete_data <- data_prep$complete_data
        
        # Build formula based on model type and constant effects
        covar_names <- data_prep$covariate_names
        const_names <- data_prep$constant_names
        
        if (model_type == "additive") {
          # Pure additive model - all effects time-varying
          formula_str <- paste0("survival ~ ", paste(covar_names, collapse = " + "))
          
        } else if (model_type == "semiparametric" && !is.null(const_names)) {
          # Semi-parametric - some constant, some time-varying
          varying_names <- setdiff(covar_names, const_names)
          
          if (length(const_names) > 0 && length(varying_names) > 0) {
            formula_str <- paste0("survival ~ const(", paste(const_names, collapse = ", "), ") + ", 
                                paste(varying_names, collapse = " + "))
          } else if (length(const_names) > 0) {
            formula_str <- paste0("survival ~ const(", paste(const_names, collapse = ", "), ")")
          } else {
            formula_str <- paste0("survival ~ ", paste(covar_names, collapse = " + "))
          }
          
        } else {
          # Non-parametric or fallback to additive
          formula_str <- paste0("survival ~ ", paste(covar_names, collapse = " + "))
        }
        
        # Create formula and fit model
        model_formula <- as.formula(formula_str)
        
        aalen_fit <- timereg::aalen(
          formula = model_formula,
          data = complete_data,
          robust = if (robust_se) 1 else 0,
          bandwidth = bandwidth,
          n.sim = 0  # Disable simulation for faster computation
        )
        
        # Extract model results
        summary_fit <- summary(aalen_fit)
        
        list(
          model = aalen_fit,
          summary = summary_fit,
          formula = model_formula,
          model_type = model_type,
          robust_se = robust_se,
          bandwidth = bandwidth
        )
      },
      
      # Perform statistical tests
      .performTests = function(model_results, data_prep) {
        test_constant <- self$options$test_constant_effects
        aalen_model <- model_results$model
        
        test_results <- list()
        
        # Test for constant effects if requested
        if (test_constant) {
          tryCatch({
            # Kolmogorov-Smirnov test for constant effects
            const_test <- timereg::const(aalen_model)
            test_results$constant_effects <- const_test
          }, error = function(e) {
            test_results$constant_effects <- NULL
          })
        }
        
        # Model goodness of fit tests
        tryCatch({
          # Extract test statistics from model summary
          model_summary <- model_results$summary
          
          # Create goodness of fit summary
          test_results$goodness_of_fit <- list(
            loglik = if (!is.null(aalen_model$loglik)) aalen_model$loglik else NA,
            aic = if (!is.null(aalen_model$aic)) aalen_model$aic else NA,
            n_obs = data_prep$n_obs,
            n_events = data_prep$n_events
          )
          
        }, error = function(e) {
          test_results$goodness_of_fit <- NULL
        })
        
        test_results
      },
      
      # Initialize result tables
      .initializeResultTables = function() {
        # Initialize tables with empty structure
        if (self$options$show_coefficients_table) {
          self$results$coefficientsTable$setKeys(character(0))
        }
        
        if (self$options$test_constant_effects && self$options$show_test_results) {
          self$results$constantEffectsTest$setKeys(character(0))
        }
        
        self$results$goodnessOfFit$setKeys(c("loglik", "aic", "n_events", "n_obs"))
        self$results$modelComparison$setKeys(c("model", "loglik", "aic", "n_events", "n_obs"))
      },
      
      # Populate all result tables and summaries
      .populateResults = function(model_results, test_results, data_prep) {
        # Populate model summary
        private$.populateModelSummary(model_results, data_prep)
        
        # Populate coefficients table
        if (self$options$show_coefficients_table) {
          private$.populateCoefficientsTable(model_results)
        }
        
        # Populate test results
        if (self$options$show_test_results) {
          private$.populateTestResults(test_results)
        }
        
        # Populate goodness of fit
        private$.populateGoodnessOfFit(test_results, data_prep)
        
        # Populate model comparison
        if (self$options$show_model_summary) {
          private$.populateModelComparison(model_results, data_prep)
        }
      },
      
      # Populate model summary
      .populateModelSummary = function(model_results, data_prep) {
        model_type <- switch(model_results$model_type,
          "additive" = "Additive Hazard Model",
          "semiparametric" = "Semi-parametric Aalen Model",
          "nonparametric" = "Non-parametric Aalen Model"
        )
        
        summary_content <- paste0(
          "<h4>Aalen's Additive Hazard Model Results</h4>",
          "<p><strong>Model Type:</strong> ", model_type, "</p>",
          "<p><strong>Observations:</strong> ", data_prep$n_obs, 
          " (", data_prep$n_events, " events)</p>",
          "<p><strong>Covariates:</strong> ", length(data_prep$covariate_names), " variables</p>",
          if (!is.null(data_prep$constant_names) && length(data_prep$constant_names) > 0) {
            paste0("<p><strong>Constant Effects:</strong> ", 
                   paste(data_prep$constant_names, collapse = ", "), "</p>")
          } else "",
          "<p><strong>Bandwidth:</strong> ", model_results$bandwidth, "</p>",
          "<p><strong>Robust SE:</strong> ", ifelse(model_results$robust_se, "Yes", "No"), "</p>"
        )
        
        self$results$modelSummary$setContent(summary_content)
      },
      
      # Populate coefficients table
      .populateCoefficientsTable = function(model_results) {
        tryCatch({
          model_summary <- model_results$summary
          aalen_model <- model_results$model
          
          # Extract coefficient information
          if (!is.null(model_summary$coef)) {
            coef_table <- model_summary$coef
            
            for (i in seq_len(nrow(coef_table))) {
              var_name <- rownames(coef_table)[i]
              
              self$results$coefficientsTable$addRow(rowKey = i, values = list(
                variable = var_name,
                cumulative_coeff = coef_table[i, "coef"],
                se = coef_table[i, "se(coef)"],
                z_value = coef_table[i, "z"],
                p_value = coef_table[i, "p"]
              ))
            }
          }
        }, error = function(e) {
          # Skip coefficient table on error
        })
      },
      
      # Populate test results
      .populateTestResults = function(test_results) {
        # Populate constant effects test
        if (!is.null(test_results$constant_effects) && 
            self$options$test_constant_effects && 
            self$options$show_test_results) {
          
          tryCatch({
            const_test <- test_results$constant_effects
            
            if (!is.null(const_test$test.table)) {
              test_table <- const_test$test.table
              
              for (i in seq_len(nrow(test_table))) {
                var_name <- rownames(test_table)[i]
                p_val <- test_table[i, "p-value"]
                
                interpretation <- if (p_val < 0.05) "Time-varying" else "Constant"
                
                self$results$constantEffectsTest$addRow(rowKey = i, values = list(
                  variable = var_name,
                  test_statistic = test_table[i, "supremum"],
                  df = as.integer(test_table[i, "df"]),
                  p_value = p_val,
                  interpretation = interpretation
                ))
              }
            }
          }, error = function(e) {
            # Skip test results on error
          })
        }
      },
      
      # Populate goodness of fit
      .populateGoodnessOfFit = function(test_results, data_prep) {
        if (!is.null(test_results$goodness_of_fit)) {
          gof <- test_results$goodness_of_fit
          
          tests <- c("Log-likelihood", "AIC", "Observations", "Events")
          values <- c(
            if (!is.na(gof$loglik)) gof$loglik else NA,
            if (!is.na(gof$aic)) gof$aic else NA,
            gof$n_obs,
            gof$n_events
          )
          
          for (i in seq_along(tests)) {
            if (!is.na(values[i])) {
              self$results$goodnessOfFit$addRow(rowKey = i, values = list(
                test = tests[i],
                statistic = if (i <= 2) values[i] else NA,
                df = if (i <= 2) NA else as.integer(values[i]),
                p_value = NA
              ))
            }
          }
        }
      },
      
      # Populate model comparison
      .populateModelComparison = function(model_results, data_prep) {
        model_type <- switch(model_results$model_type,
          "additive" = "Additive",
          "semiparametric" = "Semi-parametric",
          "nonparametric" = "Non-parametric"
        )
        
        aalen_model <- model_results$model
        
        self$results$modelComparison$addRow(rowKey = 1, values = list(
          model = paste("Aalen", model_type),
          loglik = if (!is.null(aalen_model$loglik)) aalen_model$loglik else NA,
          aic = if (!is.null(aalen_model$aic)) aalen_model$aic else NA,
          n_events = as.integer(data_prep$n_events),
          n_obs = as.integer(data_prep$n_obs)
        ))
      },
      
      # Create visualization plots
      .createPlots = function(model_results, test_results, data_prep) {
        if (self$options$show_cumulative_plots) {
          private$.createCumulativePlots(model_results)
        }
        
        if (self$options$show_hazard_plots) {
          private$.createHazardPlots(model_results, data_prep)
        }
        
        if (self$options$show_diagnostics) {
          private$.createDiagnosticPlots(model_results)
          private$.createResidualPlots(model_results)
        }
      },
      
      # Create cumulative regression function plots
      .createCumulativePlots = function(model_results) {
        tryCatch({
          image <- self$results$cumulativePlots
          
          image$setState(list(
            width = 800,
            height = 600,
            model_results = model_results
          ))
          
        }, error = function(e) {
          # Skip plot creation on error
        })
      },
      
      # Create hazard function plots
      .createHazardPlots = function(model_results, data_prep) {
        tryCatch({
          image <- self$results$hazardPlots
          
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
      
      # Create diagnostic plots
      .createDiagnosticPlots = function(model_results) {
        tryCatch({
          image <- self$results$diagnosticPlots
          
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
      .generateSummaries = function(model_results, test_results) {
        model_type_text <- switch(model_results$model_type,
          "additive" = "additive hazard",
          "semiparametric" = "semi-parametric Aalen",
          "nonparametric" = "non-parametric Aalen"
        )
        
        summary_text <- paste0(
          "<h4>Analysis Summary</h4>",
          "<p>Aalen's ", model_type_text, " regression analysis was performed to model ",
          "survival data without assuming proportional hazards. This approach allows ",
          "covariate effects to vary over time through additive rather than multiplicative effects.</p>",
          
          "<p><strong>Model Specification:</strong> ",
          "The analysis included ", length(self$options$covariates), " covariates ",
          if (!is.null(self$options$constant_effects) && length(self$options$constant_effects) > 0) {
            paste0("with ", length(self$options$constant_effects), " variables constrained to have constant effects. ")
          } else "with all effects allowed to vary over time. ",
          "Robust standard errors were ", ifelse(model_results$robust_se, "used", "not used"), 
          " for inference.</p>",
          
          if (!is.null(test_results$constant_effects) && self$options$test_constant_effects) {
            paste0(
              "<p><strong>Time-varying Effects:</strong> ",
              "Statistical tests for constant covariate effects over time were performed. ",
              "Variables showing significant time-variation (p < 0.05) may have effects that ",
              "change substantially during the follow-up period, violating proportional hazards assumptions.</p>"
            )
          } else "",
          
          "<p><strong>Clinical Interpretation:</strong> ",
          "The cumulative regression functions show how covariate effects accumulate over time. ",
          "Steep slopes indicate periods of strong covariate influence, while flat regions suggest ",
          "minimal impact. This analysis is particularly valuable when standard Cox models ",
          "fail proportional hazards assumptions.</p>"
        )
        
        self$results$analysisSummary$setContent(summary_text)
      },
      
      # Generate methodology explanations
      .generateExplanations = function() {
        explanations <- paste0(
          "<h4>Aalen's Additive Hazard Model Methodology</h4>",
          
          "<h5>Overview</h5>",
          "<p>Aalen's additive hazard model provides an alternative to Cox regression when the ",
          "proportional hazards assumption is violated. Instead of multiplicative covariate effects, ",
          "this model assumes additive effects on the hazard function:</p>",
          "<p><strong>λ(t|X) = λ₀(t) + β₁(t)X₁ + β₂(t)X₂ + ... + βₚ(t)Xₚ</strong></p>",
          "<p>where λ₀(t) is the baseline hazard and β(t) are time-varying regression coefficients.</p>",
          
          "<h5>Model Types</h5>",
          "<ul>",
          "<li><strong>Additive Model:</strong> All covariate effects are time-varying</li>",
          "<li><strong>Semi-parametric:</strong> Some effects are constant, others time-varying</li>",
          "<li><strong>Non-parametric:</strong> Purely exploratory without parametric assumptions</li>",
          "</ul>",
          
          "<h5>Key Features</h5>",
          "<ul>",
          "<li><strong>No Proportional Hazards Assumption:</strong> Covariate effects can change over time</li>",
          "<li><strong>Cumulative Regression Functions:</strong> β̂(t) shows how effects accumulate</li>",
          "<li><strong>Additive Risk Interpretation:</strong> Effects add directly to hazard rate</li>",
          "<li><strong>Time-varying Effect Detection:</strong> Statistical tests identify changing effects</li>",
          "</ul>",
          
          "<h5>Statistical Tests</h5>",
          "<ul>",
          "<li><strong>Kolmogorov-Smirnov Test:</strong> Tests whether covariate effects are constant over time</li>",
          "<li><strong>Supremum Test:</strong> Maximum deviation from constancy across follow-up time</li>",
          "<li><strong>Goodness of Fit:</strong> Overall model adequacy assessment</li>",
          "</ul>",
          
          "<h5>Interpretation Guidelines</h5>",
          "<ul>",
          "<li><strong>Cumulative Coefficients:</strong> Final values represent total effect over study period</li>",
          "<li><strong>Slope Changes:</strong> Steep regions indicate periods of strong influence</li>",
          "<li><strong>Crossing Zero:</strong> Indicates sign changes in covariate effects over time</li>",
          "<li><strong>Confidence Bands:</strong> Show uncertainty in time-varying effects</li>",
          "</ul>",
          
          "<h5>When to Use</h5>",
          "<ul>",
          "<li>Cox model proportional hazards assumption is violated</li>",
          "<li>Interest in time-varying covariate effects</li>",
          "<li>Exploratory survival analysis without strong model assumptions</li>",
          "<li>Treatment effects that may change over time</li>",
          "</ul>"
        )
        
        self$results$methodExplanation$setContent(explanations)
      }
      
    )
  ) # End R6Class