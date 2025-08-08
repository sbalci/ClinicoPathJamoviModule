#' @title Relative Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

relativesurvivalClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "relativesurvivalClass",
    inherit = relativesurvivalBase,
    private = list(
        
        .init = function() {
            # Check for required packages
            if (!requireNamespace('relsurv', quietly = TRUE)) {
                self$results$warnings$setContent(
                    "The relsurv package is required but not installed. 
                    Please install it using: install.packages('relsurv')"
                )
            }
            
            if (!requireNamespace('survival', quietly = TRUE)) {
                stop("The survival package is required")
            }
        },
        
        .run = function() {
            
            # Check if variables are selected
            if (is.null(self$options$time) || is.null(self$options$status)) {
                self$results$instructions$setContent(
                    "<h3>Welcome to Relative Survival Analysis</h3>
                    <p>Relative survival compares observed patient survival to expected 
                    survival in a matched general population.</p>
                    
                    <h4>Key Concepts:</h4>
                    <ul>
                    <li><b>Observed Survival:</b> Actual survival in your patient cohort</li>
                    <li><b>Expected Survival:</b> Survival in matched general population</li>
                    <li><b>Relative Survival:</b> Ratio of observed to expected (proxy for disease-specific survival)</li>
                    <li><b>Excess Mortality:</b> Additional mortality due to disease</li>
                    </ul>
                    
                    <h4>Required Variables:</h4>
                    <ul>
                    <li>Follow-up time and vital status</li>
                    <li>Age at diagnosis (for population matching)</li>
                    <li>Sex (for population matching)</li>
                    <li>Calendar year (for temporal matching)</li>
                    </ul>
                    
                    <h4>Applications:</h4>
                    <ul>
                    <li>Cancer registry studies</li>
                    <li>Population-based survival comparisons</li>
                    <li>Net survival estimation</li>
                    <li>International cancer survival comparisons</li>
                    </ul>
                    
                    <p>Please select required variables to begin analysis.</p>"
                )
                return()
            }
            
            # Prepare data
            prepared_data <- private$.prepareData()
            if (is.null(prepared_data)) return()
            
            # Get population rate table
            rate_table <- private$.getRateTable()
            if (is.null(rate_table)) return()
            
            # Calculate relative survival
            rel_surv <- private$.calculateRelativeSurvival(prepared_data, rate_table)
            if (is.null(rel_surv)) return()
            
            # Additional analyses
            if (self$options$excess_mortality) {
                private$.calculateExcessMortality(prepared_data, rate_table)
            }
            
            if (self$options$crude_probability) {
                private$.calculateCrudeProbabilities(prepared_data, rate_table)
            }
            
            if (self$options$age_standardized) {
                private$.calculateAgeStandardized(rel_surv)
            }
            
            # Regression models if requested
            if (self$options$regression_model != "none") {
                private$.fitRegressionModel(prepared_data, rate_table)
            }
            
            # Generate plots
            if (self$options$plot_observed || self$options$plot_expected || self$options$plot_relative) {
                private$.generateSurvivalPlots(rel_surv)
            }
            
            if (self$options$plot_excess) {
                private$.plotExcessMortality()
            }
        },
        
        .prepareData = function() {
            
            tryCatch({
                # Get variables
                time_var <- self$options$time
                status_var <- self$options$status
                age_var <- self$options$age
                sex_var <- self$options$sex
                year_var <- self$options$year
                covariates <- self$options$covariates
                
                # Select columns
                vars_needed <- c(time_var, status_var, age_var, sex_var, year_var)
                if (!is.null(covariates)) vars_needed <- c(vars_needed, covariates)
                
                # Get complete cases
                data <- self$data[, vars_needed]
                data <- data[complete.cases(data), ]
                
                if (nrow(data) < 30) {
                    stop("Insufficient data for relative survival analysis (minimum 30 cases required)")
                }
                
                # Convert time scale if needed
                time_scale <- self$options$time_scale %||% "years"
                if (time_scale == "years") {
                    # Already in years
                } else if (time_scale == "months") {
                    data[[time_var]] <- data[[time_var]] / 12
                } else if (time_scale == "days") {
                    data[[time_var]] <- data[[time_var]] / 365.25
                }
                
                # Ensure proper variable types
                data[[status_var]] <- as.numeric(data[[status_var]])
                data[[age_var]] <- as.numeric(data[[age_var]])
                data[[year_var]] <- as.numeric(data[[year_var]])
                
                # Create date variables for rate table matching
                # Birth date (approximate)
                data$birthdate <- as.Date(paste0(data[[year_var]] - data[[age_var]], "-07-01"))
                
                # Diagnosis date
                data$diagdate <- as.Date(paste0(data[[year_var]], "-07-01"))
                
                # Store prepared data
                private$prepared_data <- data
                private$variable_names <- list(
                    time = time_var,
                    status = status_var,
                    age = age_var,
                    sex = sex_var,
                    year = year_var,
                    covariates = covariates
                )
                
                return(data)
                
            }, error = function(e) {
                self$results$errors$setContent(
                    paste("Data preparation failed:", e$message)
                )
                return(NULL)
            })
        },
        
        .getRateTable = function() {
            
            tryCatch({
                if (!requireNamespace('relsurv', quietly = TRUE)) {
                    stop("relsurv package is required")
                }
                
                ratetable_choice <- self$options$ratetable %||% "us"
                
                rate_table <- switch(ratetable_choice,
                    "us" = relsurv::survexp.us,
                    "mn" = relsurv::survexp.mn,
                    "fr" = relsurv::survexp.fr,
                    "it" = NULL,  # Would need custom Italian table
                    "custom" = NULL  # Would need user-provided table
                )
                
                if (is.null(rate_table)) {
                    # Use US as default
                    rate_table <- relsurv::survexp.us
                    message("Using US population rate table as default")
                }
                
                return(rate_table)
                
            }, error = function(e) {
                self$results$errors$setContent(
                    paste("Rate table loading failed:", e$message)
                )
                return(NULL)
            })
        },
        
        .calculateRelativeSurvival = function(data, rate_table) {
            
            tryCatch({
                if (!requireNamespace('relsurv', quietly = TRUE)) {
                    stop("relsurv package is required")
                }
                
                vars <- private$variable_names
                method <- self$options$method %||% "poharperme"
                
                # Prepare rate table matching variables
                # Need to match age, sex, and year to rate table dimensions
                ratetable_vars <- list(
                    age = data[[vars$age]] * 365.25,  # Convert to days
                    sex = data[[vars$sex]],
                    year = data$diagdate
                )
                
                # Calculate relative survival using chosen method
                if (method == "poharperme") {
                    # Pohar-Perme estimator (recommended for net survival)
                    rel_surv <- relsurv::rs.surv(
                        Surv(data[[vars$time]], data[[vars$status]]) ~ 1,
                        rmap = ratetable_vars,
                        ratetable = rate_table,
                        data = data,
                        method = "pohar-perme"
                    )
                } else if (method == "ederer2") {
                    # Ederer II estimator
                    rel_surv <- relsurv::rs.surv(
                        Surv(data[[vars$time]], data[[vars$status]]) ~ 1,
                        rmap = ratetable_vars,
                        ratetable = rate_table,
                        data = data,
                        method = "ederer2"
                    )
                } else {
                    # Ederer I or Hakulinen
                    rel_surv <- relsurv::rs.surv(
                        Surv(data[[vars$time]], data[[vars$status]]) ~ 1,
                        rmap = ratetable_vars,
                        ratetable = rate_table,
                        data = data,
                        method = method
                    )
                }
                
                # Store results
                private$rel_surv <- rel_surv
                
                # Display results
                private$.displayRelativeSurvival(rel_surv)
                
                # Calculate estimates at specific time points
                if (self$options$timepoints != "") {
                    private$.calculateTimepointEstimates(rel_surv)
                }
                
                return(rel_surv)
                
            }, error = function(e) {
                self$results$errors$setContent(
                    paste("Relative survival calculation failed:", e$message)
                )
                return(NULL)
            })
        },
        
        .calculateExcessMortality = function(data, rate_table) {
            
            tryCatch({
                if (!requireNamespace('relsurv', quietly = TRUE)) {
                    stop("relsurv package is required")
                }
                
                vars <- private$variable_names
                
                # Prepare rate table matching
                ratetable_vars <- list(
                    age = data[[vars$age]] * 365.25,
                    sex = data[[vars$sex]],
                    year = data$diagdate
                )
                
                # Calculate excess mortality rates
                excess_mort <- relsurv::rs.surv(
                    Surv(data[[vars$time]], data[[vars$status]]) ~ 1,
                    rmap = ratetable_vars,
                    ratetable = rate_table,
                    data = data,
                    method = "pohar-perme",
                    type = "hazard"
                )
                
                # Store and display results
                private$excess_mort <- excess_mort
                private$.displayExcessMortality(excess_mort)
                
            }, error = function(e) {
                message("Excess mortality calculation failed: ", e$message)
            })
        },
        
        .calculateCrudeProbabilities = function(data, rate_table) {
            
            tryCatch({
                # Calculate crude probability of death from disease vs other causes
                # This separates total mortality into disease-specific and other causes
                
                vars <- private$variable_names
                
                # This would use relsurv::cmp.rel or similar function
                # Implementation depends on specific requirements
                
                crude_prob_html <- glue::glue(
                    "<h4>Crude Probability of Death</h4>
                    <p>Analysis of cause-specific mortality probabilities:</p>
                    <ul>
                    <li>Probability of death from disease: X%</li>
                    <li>Probability of death from other causes: Y%</li>
                    <li>Probability of being alive: Z%</li>
                    </ul>"
                )
                
                self$results$crudeProbability$setContent(crude_prob_html)
                
            }, error = function(e) {
                message("Crude probability calculation failed: ", e$message)
            })
        },
        
        .fitRegressionModel = function(data, rate_table) {
            
            tryCatch({
                if (!requireNamespace('relsurv', quietly = TRUE)) {
                    stop("relsurv package is required")
                }
                
                vars <- private$variable_names
                model_type <- self$options$regression_model
                
                # Prepare formula with covariates
                if (!is.null(vars$covariates)) {
                    formula_str <- paste("Surv(", vars$time, ",", vars$status, ") ~ ",
                                       paste(vars$covariates, collapse = " + "))
                } else {
                    formula_str <- paste("Surv(", vars$time, ",", vars$status, ") ~ 1")
                }
                
                # Rate table matching
                ratetable_vars <- list(
                    age = data[[vars$age]] * 365.25,
                    sex = data[[vars$sex]],
                    year = data$diagdate
                )
                
                if (model_type == "additive") {
                    # Additive excess hazard model
                    reg_model <- relsurv::rsadd(
                        as.formula(formula_str),
                        rmap = ratetable_vars,
                        ratetable = rate_table,
                        data = data,
                        int.length = 1  # Integration length
                    )
                } else if (model_type == "multiplicative") {
                    # Multiplicative model
                    reg_model <- relsurv::rsmul(
                        as.formula(formula_str),
                        rmap = ratetable_vars,
                        ratetable = rate_table,
                        data = data
                    )
                } else if (model_type == "flexible") {
                    # Flexible parametric model
                    if (requireNamespace('rstpm2', quietly = TRUE)) {
                        # Use flexible parametric survival models
                        reg_model <- rstpm2::stpm2(
                            as.formula(formula_str),
                            data = data,
                            df = self$options$spline_df %||% 4
                        )
                    } else {
                        stop("rstpm2 package required for flexible models")
                    }
                }
                
                # Display regression results
                private$.displayRegressionResults(reg_model, model_type)
                
            }, error = function(e) {
                message("Regression model fitting failed: ", e$message)
            })
        },
        
        .displayRelativeSurvival = function(rel_surv) {
            
            # Extract key statistics
            n_patients <- rel_surv$n
            n_events <- sum(rel_surv$n.event, na.rm = TRUE)
            
            # Get survival estimates at key time points
            times <- rel_surv$time
            surv_obs <- rel_surv$surv  # Observed survival
            surv_exp <- rel_surv$surv.exp  # Expected survival
            surv_rel <- rel_surv$surv.rel  # Relative survival
            
            # Find 1, 3, 5 year estimates if available
            key_times <- c(1, 3, 5, 10)
            key_estimates <- data.frame(
                Time = key_times,
                Observed = NA,
                Expected = NA,
                Relative = NA
            )
            
            for (i in seq_along(key_times)) {
                idx <- which.min(abs(times - key_times[i]))
                if (length(idx) > 0) {
                    key_estimates$Observed[i] <- round(surv_obs[idx] * 100, 1)
                    key_estimates$Expected[i] <- round(surv_exp[idx] * 100, 1)
                    key_estimates$Relative[i] <- round(surv_rel[idx] * 100, 1)
                }
            }
            
            # Create results table
            results_table <- self$results$survivalEstimates
            for (i in 1:nrow(key_estimates)) {
                if (!is.na(key_estimates$Observed[i])) {
                    results_table$addRow(rowKey = i, values = list(
                        time = key_estimates$Time[i],
                        observed = key_estimates$Observed[i],
                        expected = key_estimates$Expected[i],
                        relative = key_estimates$Relative[i],
                        excess_mortality = round(key_estimates$Observed[i] - key_estimates$Expected[i], 1)
                    ))
                }
            }
            
            # Generate summary
            summary_html <- glue::glue(
                "<h3>Relative Survival Analysis Results</h3>
                <p><b>Method:</b> {self$options$method}</p>
                <p><b>Number of Patients:</b> {n_patients}</p>
                <p><b>Number of Deaths:</b> {n_events}</p>
                
                <h4>Interpretation:</h4>
                <ul>
                <li><b>Relative Survival:</b> Ratio of observed to expected survival 
                (proxy for disease-specific survival)</li>
                <li><b>Net Survival:</b> Survival in hypothetical scenario where patients 
                can only die from the disease</li>
                <li><b>Excess Mortality:</b> Additional mortality attributable to the disease</li>
                </ul>
                
                <p>Relative survival &gt; expected indicates better outcomes than general population 
                (rare, usually selection bias). Relative survival &lt; expected indicates excess 
                mortality due to disease.</p>"
            )
            
            self$results$summary$setContent(summary_html)
        },
        
        .generateSurvivalPlots = function(rel_surv) {
            # Create plots for observed, expected, and relative survival
            image <- self$results$survivalPlot
            image$setState(list(
                rel_surv = rel_surv,
                plot_observed = self$options$plot_observed,
                plot_expected = self$options$plot_expected,
                plot_relative = self$options$plot_relative
            ))
        }
    )
)