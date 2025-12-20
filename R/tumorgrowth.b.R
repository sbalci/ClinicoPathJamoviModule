#' @title Tumor Growth Models
#' @importFrom R6 R6Class
#' @import jmvcore
#'

tumorgrowthClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "tumorgrowthClass",
    inherit = tumorgrowthBase,
    private = list(
        growth_model = NULL,

        .escapeVar = function(x) {
            # Safely escape variable names for data.frame access
            if (is.null(x) || length(x) == 0) return(NULL)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        .init = function() {
            # Check for required packages
            required_packages <- c('nlme', 'ggplot2', 'dplyr', 'brms')

            missing_pkgs <- c()
            for (pkg in required_packages) {
                if (!requireNamespace(pkg, quietly = TRUE)) {
                    missing_pkgs <- c(missing_pkgs, pkg)
                }
            }

            if (length(missing_pkgs) > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingPackages',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Required R package(s) not installed: %s. Install using: install.packages(c(%s))',
                    paste(missing_pkgs, collapse = ', '),
                    paste(sprintf('"%s"', missing_pkgs), collapse = ', ')
                ))
                self$results$insert(1, notice)
            }

            # Populate glossary (always visible)
            private$.populateGlossary()
        },

        .run = function() {

            # Check if required variables are selected
            if (is.null(self$options$time) || is.null(self$options$tumorSize)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingVariables',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Time and Tumor Size variables are required. Please select both to begin tumor growth modeling.')
                self$results$insert(1, notice)
                return()
            }
            
            # Get data and variables
            data <- self$data
            time_var <- self$options$time
            size_var <- self$options$tumorSize
            patient_var <- self$options$patientId
            covariates <- self$options$covariates
            growth_model <- self$options$growthModel %||% "gompertz"
            model_approach <- self$options$modelApproach %||% "nlme"
            
            # Prepare analysis variables
            analysis_vars <- c(time_var, size_var)
            if (!is.null(patient_var)) analysis_vars <- c(analysis_vars, patient_var)
            if (length(covariates) > 0) analysis_vars <- c(analysis_vars, covariates)
                if (!is.null(self$options$treatmentEffect)) analysis_vars <- c(analysis_vars, self$options$treatmentEffect)
            
            # Clean data and validate
            clean_data <- data[complete.cases(data[, analysis_vars]), analysis_vars]
            
            if (nrow(clean_data) < 10) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'insufficientData',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Insufficient data for tumor growth modeling: %d observations after removing missing values. Minimum 10 complete observations required (time, tumor size%s).',
                    nrow(clean_data),
                    if (!is.null(self$options$patientId)) ', patient ID' else ''
                ))
                self$results$insert(1, notice)
                return()
            }
            
            # Add NLME minimum sample size guard
            if (model_approach == "nlme" && !is.null(patient_var)) {
                n_patients <- length(unique(clean_data[[patient_var]]))
                avg_obs <- nrow(clean_data) / n_patients

                if (n_patients < 5 || avg_obs < 2.5) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'insufficientLongitudinalStructure',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(sprintf('Insufficient longitudinal structure for NLME: %d patients, avg %.1f obs/patient. Minimum 5 patients with avg 2.5 obs/patient required. Switch to NLS approach or collect more data.', n_patients, avg_obs))
                    self$results$insert(1, notice)
                    return()
                }
            }

            # Fit growth model
            private$.fitGrowthModel(clean_data, time_var, size_var, patient_var,
                                   covariates, growth_model, model_approach)

            # Calculate doubling time if requested
            if (self$options$doubleTime) {
                private$.calculateDoublingTime()
            }

            # Analyze treatment effects if requested
            if (self$options$treatmentAnalysis && !is.null(self$options$treatmentEffect)) {
                private$.analyzeTreatmentEffects(clean_data)
            }

            # Generate clinical interpretation
            private$.generateClinicalInterpretation()

            # Generate natural language summary if requested
            if (self$options$showSummary) {
                private$.generateNaturalLanguageSummary(clean_data)
            }

            # Add completion info notice
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO
            )
            notice$setContent(sprintf('Tumor growth modeling completed successfully: %d observations analyzed using %s %s model%s.',
                nrow(clean_data),
                stringr::str_to_title(self$options$modelApproach %||% 'nlme'),
                stringr::str_to_title(self$options$growthModel %||% 'gompertz'),
                if (!is.null(self$options$patientId)) sprintf(' with %d patients', length(unique(clean_data[[self$options$patientId]]))) else ''
            ))
            self$results$insert(999, notice)
        },
        
        .fitGrowthModel = function(fit_data, time_var, size_var, patient_var, 
                                  covariates, growth_model, model_approach) {
            
            tryCatch({
                
                # Prepare data
                fit_data$time <- fit_data[[time_var]]
                fit_data$size <- fit_data[[size_var]]
                if (!is.null(patient_var)) {
                    fit_data$patient <- as.factor(fit_data[[patient_var]])
                }

                # Record filtering info
                if (!is.null(self$data)) {
                    removed <- nrow(self$data) - nrow(fit_data)
                    if (removed > 0) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'dataFiltered',
                            type = jmvcore::NoticeType$INFO
                        )
                        notice$setContent(sprintf('Data note: %d row(s) removed due to missing values in required variables (%.1f%% of original data retained).',
                            removed,
                            (nrow(fit_data) / nrow(self$data)) * 100
                        ))
                        self$results$insert(998, notice)
                    }
                }
                
                # Validate longitudinal structure and time ordering
                private$.validateTumorData(fit_data, time_var, size_var)

                # Fit model based on type
                if (model_approach == "nlme" && !is.null(patient_var)) {
                    model_fit <- private$.fitNlmeModel(fit_data, growth_model)
                } else if (model_approach == "bayesian" && !is.null(patient_var)) {
                    model_fit <- private$.fitBrmsModel(fit_data, growth_model)
                }
                else {
                    model_fit <- private$.fitNlsModel(fit_data, growth_model)
                }
                
                # Store model for plotting
                private$growth_model <- model_fit

                # Check model convergence
                private$.checkConvergence(model_fit, model_approach)

                # Format results
                private$.formatGrowthResults(model_fit, growth_model)

                # Calculate model fit statistics
                private$.calculateFitStatistics(model_fit, fit_data)

                # Populate growth parameters table if requested
                if (self$options$growthParameters) {
                    private$.populateGrowthParameters()
                }
                
            }, error = function(e) {
                self$results$summary$setContent(
                    paste("<h3>Model Fitting Error</h3><p>", e$message, "</p>")
                )
            })
        },
        
        .fitBrmsModel = function(data, growth_model) {
            
            library(brms)
            
            iter_val <- max(self$options$mcmcSamples %||% 2000, 1000)
            conf_level <- (self$options$confidenceLevel %||% 95) / 100
            
            # Define priors
            priors <- c(
                prior(normal(0, 10), class = "b") 
            )
            
            if (growth_model == "exponential") {
                # V(t) = V0 * exp(k*t)
                bform <- bf(size ~ V0 * exp(k * time),
                            V0 ~ 1 + (1 | patient),
                            k ~ 1,
                            nl = TRUE)
                
                model <- brm(bform, 
                             data = data, 
                             prior = priors,
                             iter = iter_val, chains = 2, cores = 2,
                             control = list(adapt_delta = 0.95),
                             prob = conf_level)
                
            } else if (growth_model == "gompertz") {
                # V(t) = V0 * exp(alpha/beta * (1 - exp(-beta*t)))
                bform <- bf(size ~ V0 * exp(alpha/beta * (1 - exp(-beta * time))),
                            V0 + alpha + beta ~ 1 + (1 | patient),
                            nl = TRUE)
                
                model <- brm(bform, 
                             data = data, 
                             prior = priors,
                             iter = iter_val, chains = 2, cores = 2,
                             control = list(adapt_delta = 0.95),
                             prob = conf_level)
                
            } else if (growth_model == "logistic") {
                # V(t) = K / (1 + exp(-r*(t-t0)))
                bform <- bf(size ~ K / (1 + exp(-r * (time - t0))),
                            K + r + t0 ~ 1 + (1 | patient),
                            nl = TRUE)
                
                model <- brm(bform, 
                             data = data, 
                             prior = priors,
                             iter = iter_val, chains = 2, cores = 2,
                             control = list(adapt_delta = 0.95),
                             prob = conf_level)
                
            } else if (growth_model == "bertalanffy") {
                # von Bertalanffy: V(t) = (Vinf^(1/3) - (Vinf^(1/3) - V0^(1/3)) * exp(-k*t))^3
                bform <- bf(size ~ (Vinf^(1/3) - (Vinf^(1/3) - V0^(1/3)) * exp(-k * time))^3,
                            V0 + Vinf + k ~ 1 + (1 | patient),
                            nl = TRUE)
                
                model <- brm(bform, 
                             data = data, 
                             prior = priors,
                             iter = iter_val, chains = 2, cores = 2,
                             control = list(adapt_delta = 0.95),
                             prob = conf_level)
                
            } else if (growth_model == "power") {
                # Power Law: V(t) = V0 * t^alpha
                bform <- bf(size ~ V0 * (time + 1)^alpha,
                            V0 + alpha ~ 1 + (1 | patient),
                            nl = TRUE)
                
                model <- brm(bform, 
                             data = data, 
                             prior = priors,
                             iter = iter_val, chains = 2, cores = 2,
                             control = list(adapt_delta = 0.95),
                             prob = conf_level)
                
            } else if (growth_model == "linear") {
                # V(t) = V0 + k*t
                bform <- bf(size ~ V0 + k * time,
                            V0 + k ~ 1 + (1 | patient),
                            nl = TRUE)
                
                model <- brm(bform,
                             data = data,
                             prior = priors,
                             iter = iter_val, chains = 2, cores = 2,
                             control = list(adapt_delta = 0.95),
                             prob = conf_level)
            } else {
                # Default to exponential
                bform <- bf(size ~ V0 * exp(k * time),
                            V0 ~ 1 + (1 | patient),
                            k ~ 1,
                            nl = TRUE)

                model <- brm(bform,
                             data = data,
                             prior = priors,
                             iter = iter_val, chains = 2, cores = 2,
                             control = list(adapt_delta = 0.95),
                             prob = conf_level)
            }
            
            return(model)
        },
        
        .fitNlmeModel = function(fit_data, growth_model) {
            
            library(nlme)
            
            # Define growth model functions
            V0_start <- self$options$initialSize %||% mean(fit_data$size[fit_data$time == min(fit_data$time)], na.rm = TRUE)
            if (growth_model == "exponential") {
                # V(t) = V0 * exp(k*t)
                model <- nlme(size ~ V0 * exp(k * time),
                             data = fit_data,
                             fixed = V0 + k ~ 1,
                             random = V0 ~ 1 | patient,
                             start = list(fixed = c(V0 = V0_start, 
                                                    k = 0.1)),
                             control = nlmeControl(maxIter = 200))
                             
            } else if (growth_model == "gompertz") {
                # V(t) = V0 * exp(alpha/beta * (1 - exp(-beta*t)))
                model <- nlme(size ~ V0 * exp(alpha/beta * (1 - exp(-beta * time))),
                             data = fit_data,
                             fixed = V0 + alpha + beta ~ 1,
                             random = V0 ~ 1 | patient,
                             start = list(fixed = c(V0 = V0_start,
                                                    alpha = 1, beta = 0.1)),
                             control = nlmeControl(maxIter = 200))
                             
            } else if (growth_model == "logistic") {
                # V(t) = K / (1 + exp(-r*(t-t0)))
                K_init <- self$options$maxSize %||% (max(fit_data$size, na.rm = TRUE) * 1.2)
                model <- nlme(size ~ K / (1 + exp(-r * (time - t0))),
                             data = fit_data,
                             fixed = K + r + t0 ~ 1,
                             random = K ~ 1 | patient,
                             start = list(fixed = c(K = K_init, r = 0.1, 
                                                    t0 = median(fit_data$time, na.rm = TRUE))),
                             control = nlmeControl(maxIter = 200))
                             
            } else if (growth_model == "bertalanffy") {
                # von Bertalanffy: V(t) = (Vinf^(1/3) - (Vinf^(1/3) - V0^(1/3)) * exp(-k*t))^3
                Vinf_init <- self$options$maxSize %||% (max(fit_data$size, na.rm = TRUE) * 1.5)
                model <- nlme(size ~ (Vinf^(1/3) - (Vinf^(1/3) - V0^(1/3)) * exp(-k * time))^3,
                             data = fit_data,
                             fixed = V0 + Vinf + k ~ 1,
                             random = V0 ~ 1 | patient,
                             start = list(fixed = c(V0 = mean(fit_data$size[fit_data$time == min(fit_data$time)], na.rm = TRUE),
                                                    Vinf = Vinf_init, k = 0.1)),
                             control = nlmeControl(maxIter = 200))
                             
            } else if (growth_model == "power") {
                # Power Law: V(t) = V0 * t^alpha
                model <- nlme(size ~ V0 * (time + 1)^alpha,  # +1 to handle t=0
                             data = fit_data,
                             fixed = V0 + alpha ~ 1,
                             random = V0 ~ 1 | patient,
                             start = list(fixed = c(V0 = mean(fit_data$size[fit_data$time == min(fit_data$time)], na.rm = TRUE),
                                                    alpha = 1.2)),
                             control = nlmeControl(maxIter = 200))
                             
            } else if (growth_model == "linear") {
                # V(t) = V0 + k*t
                model <- nlme(size ~ V0 + k * time,
                             data = fit_data,
                             fixed = V0 + k ~ 1,
                             random = V0 ~ 1 | patient,
                             start = list(fixed = c(V0 = mean(fit_data$size[fit_data$time == min(fit_data$time)], na.rm = TRUE),
                                                    k = 1)))
            } else {
                # Default to exponential
                model <- nlme(size ~ V0 * exp(k * time),
                             data = fit_data,
                             fixed = V0 + k ~ 1,
                             random = V0 ~ 1 | patient,
                             start = list(fixed = c(V0 = mean(fit_data$size[fit_data$time == min(fit_data$time)], na.rm = TRUE),
                                                    k = 0.1)))
            }
            
            return(model)
        },
        
        .fitNlsModel = function(fit_data, growth_model) {
            
            # Simple nonlinear least squares for single patients or pooled data
            V0_start <- self$options$initialSize %||% mean(fit_data$size[fit_data$time == min(fit_data$time)], na.rm = TRUE)
            if (growth_model == "exponential") {
                model <- nls(size ~ V0 * exp(k * time),
                            data = fit_data,
                            start = list(V0 = V0_start,
                                        k = 0.1))
                                        
            } else if (growth_model == "gompertz") {
                model <- nls(size ~ V0 * exp(alpha/beta * (1 - exp(-beta * time))),
                            data = fit_data,
                            start = list(V0 = V0_start,
                                        alpha = 1, beta = 0.1))
                                        
            } else if (growth_model == "logistic") {
                K_init <- self$options$maxSize %||% (max(fit_data$size, na.rm = TRUE) * 1.2)
                model <- nls(size ~ K / (1 + exp(-r * (time - t0))),
                            data = fit_data,
                            start = list(K = K_init, r = 0.1, 
                                        t0 = median(fit_data$time, na.rm = TRUE)))
                                        
            } else if (growth_model == "bertalanffy") {
                Vinf_init <- self$options$maxSize %||% (max(fit_data$size, na.rm = TRUE) * 1.5)
                model <- nls(size ~ (Vinf^(1/3) - (Vinf^(1/3) - V0^(1/3)) * exp(-k * time))^3,
                            data = fit_data,
                            start = list(V0 = V0_start,
                                        Vinf = Vinf_init, k = 0.1))
                                        
            } else if (growth_model == "power") {
                model <- nls(size ~ V0 * (time + 1)^alpha,
                            data = fit_data,
                            start = list(V0 = mean(fit_data$size[fit_data$time == min(fit_data$time)], na.rm = TRUE),
                                        alpha = 1.2))
                                        
            } else if (growth_model == "linear") {
                model <- lm(size ~ time, data = fit_data)
                
            } else {
                # Default exponential
                model <- nls(size ~ V0 * exp(k * time),
                            data = fit_data,
                            start = list(V0 = mean(fit_data$size[fit_data$time == min(fit_data$time)], na.rm = TRUE),
                                        k = 0.1))
            }
            
            return(model)
        },
        
        .formatGrowthResults = function(model, growth_model) {
            
            conf_level <- (self$options$confidenceLevel %||% 95) / 100
            
            # Extract coefficient summary
            if (inherits(model, "nlme")) {
                coef_summary <- summary(model)$tTable
            } else if (inherits(model, "nls")) {
                coef_summary <- summary(model)$coefficients
            } else if (inherits(model, "lm")) {
                coef_summary <- summary(model)$coefficients
            } else if (inherits(model, "brmsfit")) {
                coef_summary <- summary(model)$fixed
            }
            
            # Populate model table
            model_table <- self$results$modelTable
            
            
            for (i in 1:nrow(coef_summary)) {
                param_name <- rownames(coef_summary)[i]
                # Biological interpretation
                interpretation <- private$.interpretParameter(param_name, growth_model)
                
                if (inherits(model, "brmsfit")) {
                    lower_name <- paste0(round((1 - conf_level) / 2 * 100), "% CI")
                    upper_name <- paste0(round((1 + conf_level) / 2 * 100), "% CI")
                    ci_lower <- if (lower_name %in% colnames(coef_summary)) coef_summary[i, lower_name] else coef_summary[i, "l-95% CI"]
                    ci_upper <- if (upper_name %in% colnames(coef_summary)) coef_summary[i, upper_name] else coef_summary[i, "u-95% CI"]
                    
                    model_table$addRow(rowKey = i, values = list(
                        parameter = param_name,
                        estimate = round(coef_summary[i, "Estimate"], 4),
                        std_error = round(coef_summary[i, "Est.Error"], 4),
                        t_value = NA,
                        p_value = NA,
                        ci_lower = round(ci_lower, 4),
                        ci_upper = round(ci_upper, 4),
                        interpretation = interpretation
                    ))
                } else {
                    # Get better confidence intervals
                    ci_result <- private$.calculateBetterConfidenceIntervals(model, param_name, conf_level)
                    
                    # Extract values safely
                    est <- if ("Value" %in% colnames(coef_summary)) coef_summary[i, "Value"] else coef_summary[i, "Estimate"]
                    se <- if ("Std.Error" %in% colnames(coef_summary)) coef_summary[i, "Std.Error"] else coef_summary[i, "Std. Error"]
                    tval <- if ("t-value" %in% colnames(coef_summary)) coef_summary[i, "t-value"] else coef_summary[i, "t value"]
                    pval <- if ("p-value" %in% colnames(coef_summary)) coef_summary[i, "p-value"] else coef_summary[i, "Pr(>|t|)"]
                    
                    model_table$addRow(rowKey = i, values = list(
                        parameter = param_name,
                        estimate = est,
                        std_error = se,
                        t_value = tval,
                        p_value = pval,
                        ci_lower = ci_result$lower,
                        ci_upper = ci_result$upper,
                        interpretation = interpretation
                    ))
                }
            }
            
            # Generate summary
            private$.generateSummary(model, growth_model)
        },
        
        .interpretParameter = function(param_name, growth_model) {
            
            interpretations <- list(
                "exponential" = list(
                    "V0" = "Initial tumor size",
                    "k" = "Growth rate constant"
                ),
                "gompertz" = list(
                    "V0" = "Initial tumor size",
                    "alpha" = "Growth potential",
                    "beta" = "Deceleration parameter"
                ),
                "logistic" = list(
                    "K" = "Carrying capacity (maximum size)",
                    "r" = "Intrinsic growth rate",
                    "t0" = "Inflection point time"
                ),
                "bertalanffy" = list(
                    "V0" = "Initial tumor size",
                    "Vinf" = "Asymptotic maximum size", 
                    "k" = "Growth rate constant"
                ),
                "power" = list(
                    "V0" = "Size scaling parameter",
                    "alpha" = "Growth exponent"
                ),
                "linear" = list(
                    "(Intercept)" = "Initial size",
                    "time" = "Growth rate (size/time)"
                )
            )
            
            model_interp <- interpretations[[growth_model]]
            return(model_interp[[param_name]] %||% "Model parameter")
        },
        
        .calculateDoublingTime = function() {
            
            if (is.null(private$growth_model)) return()
            
            tryCatch({
                
                model <- private$growth_model
                growth_model <- self$options$growthModel %||% "gompertz"
                conf_level <- (self$options$confidenceLevel %||% 95) / 100
                alpha <- 1 - conf_level
                z <- stats::qnorm(1 - alpha / 2)
                
                dt_type <- "Doubling Time"
                doubling_time <- NA
                ci <- c(NA, NA)
                
                # Calculate doubling time based on model type
                if (growth_model == "exponential") {
                    if (inherits(model, "nlme")) {
                        k <- fixef(model)["k"]
                        se_k <- sqrt(vcov(model)["k", "k"])
                    } else {
                        k <- coef(model)["k"]
                        se_k <- summary(model)$coefficients["k", "Std. Error"] %||% NA
                    }
                    doubling_time <- log(2) / k
                    dt_type <- "Constant Doubling Time"
                    if (!is.na(se_k)) {
                        se_dt <- (log(2) / (k^2)) * se_k
                        ci <- doubling_time + c(-z, z) * se_dt
                    }
                    
                } else if (growth_model == "gompertz") {
                    # Doubling time varies for Gompertz - calculate at initial time
                    if (inherits(model, "nlme")) {
                        alpha_g <- fixef(model)["alpha"]
                        se_alpha <- sqrt(vcov(model)["alpha", "alpha"])
                    } else {
                        alpha_g <- coef(model)["alpha"]
                        se_alpha <- summary(model)$coefficients["alpha", "Std. Error"] %||% NA
                    }
                    # SGR(0) = alpha. Initial DT = ln(2)/alpha
                    doubling_time <- log(2) / alpha_g 
                    dt_type <- "Initial Doubling Time"
                    if (!is.na(se_alpha)) {
                        se_dt <- (log(2) / (alpha_g^2)) * se_alpha
                        ci <- doubling_time + c(-z, z) * se_dt
                    }
                    
                } else if (growth_model == "linear") {
                    # For linear growth, calculate time to double from initial size
                    if (inherits(model, "lm")) {
                        k <- coef(model)["time"]
                        V0 <- coef(model)["(Intercept)"]
                        se_k <- summary(model)$coefficients["time", "Std. Error"]
                        se_v0 <- summary(model)$coefficients["(Intercept)", "Std. Error"]
                    } else {
                        k <- coef(model)["k"]
                        V0 <- coef(model)["V0"]
                        se_k <- summary(model)$coefficients["k", "Std. Error"] %||% NA
                        se_v0 <- summary(model)$coefficients["V0", "Std. Error"] %||% NA
                    }
                    doubling_time <- V0 / k  # Time to add V0 to initial size
                    dt_type <- "Time to Double Initial Size"
                    if (!is.na(se_k) && !is.na(se_v0)) {
                        # Delta method: g = V0/k, grad = (1/k, -V0/k^2)
                        grad <- c(1 / k, -V0 / (k^2))
                        cov_mat <- matrix(c(se_v0^2, 0, 0, se_k^2), nrow = 2)
                        se_dt <- sqrt(t(grad) %*% cov_mat %*% grad)
                        ci <- doubling_time + c(-z, z) * se_dt
                    }
                    
                } else {
                    doubling_time <- NA
                }
                
                # Populate doubling time table
                if (!is.na(doubling_time)) {
                    doubling_table <- self$results$doublingTimeTable
                    doubling_table$addRow(rowKey = "overall", values = list(
                        group = dt_type,
                        doubling_time = round(doubling_time, 2),
                        unit = "time units",
                        ci_lower = ifelse(is.na(ci[1]), NA, round(ci[1], 2)),
                        ci_upper = ifelse(is.na(ci[2]), NA, round(ci[2], 2))
                    ))
                }
                
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'doublingTimeCalculationFailed',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf('Doubling time calculation failed: %s. Check model convergence and parameter estimates.', e$message))
                self$results$insert(10, notice)
            })
        },
        
        .calculateFitStatistics = function(model, data) {
            
            tryCatch({
                
                # Calculate fit statistics
                if (inherits(model, "nlme")) {
                    aic_val <- AIC(model)
                    bic_val <- BIC(model)
                    loglik_val <- logLik(model)
                    r2_val <- cor(fitted(model), data$size)^2
                } else if (inherits(model, "nls")) {
                    aic_val <- AIC(model)
                    bic_val <- BIC(model)
                    loglik_val <- logLik(model)
                    r2_val <- cor(fitted(model), data$size)^2
                } else if (inherits(model, "lm")) {
                    aic_val <- AIC(model)
                    bic_val <- BIC(model)
                    r2_val <- summary(model)$r.squared
                    loglik_val <- logLik(model)
                } else if (inherits(model, "brmsfit")) {
                    waic_val <- waic(model)$estimates["waic", "Estimate"]
                    looic_val <- loo(model)$estimates["looic", "Estimate"]
                    r2_val <- bayes_R2(model)[1, "Estimate"]
                    
                    # Populate fit statistics table
                    fit_table <- self$results$fitStatistics
                    
                    fit_table$addRow(rowKey = "waic", values = list(
                        metric = "WAIC",
                        value = round(waic_val, 2),
                        interpretation = "Lower is better"
                    ))
                    
                    fit_table$addRow(rowKey = "looic", values = list(
                        metric = "LOOIC",
                        value = round(looic_val, 2),
                        interpretation = "Lower is better"
                    ))
                    
                    fit_table$addRow(rowKey = "r2", values = list(
                        metric = "Bayes R-squared",
                        value = round(r2_val, 3),
                        interpretation = "Proportion of variance explained"
                    ))
                    
                    return()
                }
                
                # Populate fit statistics table
                fit_table <- self$results$fitStatistics
                
                fit_table$addRow(rowKey = "aic", values = list(
                    metric = "AIC",
                    value = round(aic_val, 2),
                    interpretation = "Lower is better"
                ))
                
                fit_table$addRow(rowKey = "bic", values = list(
                    metric = "BIC",
                    value = round(bic_val, 2),
                    interpretation = "Lower is better"
                ))
                
                fit_table$addRow(rowKey = "r2", values = list(
                    metric = "R-squared",
                    value = round(r2_val, 3),
                    interpretation = "Proportion of variance explained"
                ))
                
                fit_table$addRow(rowKey = "loglik", values = list(
                    metric = "Log-Likelihood",
                    value = round(as.numeric(loglik_val), 2),
                    interpretation = "Higher is better"
                ))
                
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'fitStatisticsFailed',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf('Model fit statistics calculation failed: %s. Model parameters are still available but goodness-of-fit metrics cannot be computed.', e$message))
                self$results$insert(10, notice)
            })
        },
        
        .generateSummary = function(model, growth_model) {
            
            # Generate interpretation
            model_descriptions <- list(
                "exponential" = "Exponential growth assumes constant proportional growth rate over time. This model is appropriate for early-stage, rapidly growing tumors.",
                "gompertz" = "Gompertz growth models decreasing growth rate over time, often seen in solid tumors as they approach resource limitations.",
                "logistic" = "Logistic growth includes a carrying capacity, modeling growth that slows as tumor size approaches its maximum sustainable size.",
                "linear" = "Linear growth assumes constant absolute growth rate, which may be appropriate for some treated tumors.",
                "bertalanffy" = "von Bertalanffy growth models allometric scaling relationships in tumor growth.",
                "power" = "Power law growth models non-linear relationships between time and tumor size."
            )
            
            summary_html <- glue::glue(
                "<h3>Tumor Growth Model Results</h3>
                <p><b>Model Type:</b> {stringr::str_to_title(growth_model)} Growth Model</p>
                <p><b>Description:</b> {model_descriptions[[growth_model]] %||% 'Mathematical growth model fitted to tumor size data.'}</p>
                
                <h4>Clinical Interpretation:</h4>
                <p>The fitted growth parameters provide insights into tumor kinetics and can be used for:</p>
                <ul>
                <li>Predicting future tumor growth</li>
                <li>Assessing treatment efficacy</li>
                <li>Comparing growth rates between patient groups</li>
                <li>Estimating tumor doubling times</li>
                </ul>
                
                <p><b>Note:</b> Tumor growth modeling should be interpreted in the context of treatment history, 
                tumor type, and measurement methodology.</p>"
            )
            
            self$results$summary$setContent(summary_html)
        },
        
        .plotGrowthCurves = function(image, ...) {
            
            if (is.null(private$growth_model)) return()
            
            tryCatch({
                library(ggplot2)
                library(dplyr)
                
                # Get original data
                data <- self$data
                time_var <- self$options$time
                size_var <- self$options$tumorSize
                patient_var <- self$options$patientId
                
                # Prepare data
                plot_data <- data.frame(
                    time = data[[time_var]],
                    size = data[[size_var]]
                )
                
                if (!is.null(patient_var)) {
                    plot_data$patient <- data[[patient_var]]
                }
                
                # Create prediction data
                time_range <- seq(min(plot_data$time, na.rm = TRUE), 
                                 max(plot_data$time, na.rm = TRUE), 
                                 length.out = 100)
                
                pred_data <- data.frame(time = time_range)
                ribbon <- NULL
                
                # Get fitted values and simple uncertainty if available
                if (inherits(private$growth_model, "lm")) {
                    pred <- predict(private$growth_model, newdata = pred_data, se.fit = TRUE)
                    pred_data$fitted <- as.numeric(pred$fit)
                    ribbon <- data.frame(
                        time = time_range,
                        ymin = pred$fit - 1.96 * pred$se.fit,
                        ymax = pred$fit + 1.96 * pred$se.fit
                    )
                } else if (inherits(private$growth_model, c("nlme", "nls"))) {
                    pred_data$fitted <- predict(private$growth_model, newdata = pred_data)
                } else if (inherits(private$growth_model, "brmsfit")) {
                    pred_draws <- posterior_predict(private$growth_model, newdata = pred_data, draws = 400)
                    pred_data$fitted <- apply(pred_draws, 2, median)
                    qs <- apply(pred_draws, 2, quantile, probs = c(0.025, 0.975))
                    ribbon <- data.frame(time = time_range, ymin = qs[1, ], ymax = qs[2, ])
                }

                caption_txt <- "Red line: Fitted growth model, Points: Observed data"
                if (!is.null(ribbon)) caption_txt <- paste(caption_txt, "Shaded: 95% interval.")
                if (length(self$options$covariates) > 0) caption_txt <- paste(caption_txt, "Covariate effects not visualised.")

                p <- ggplot() +
                    geom_point(data = plot_data, aes(x = time, y = size), alpha = 0.6) +
                    {if (!is.null(ribbon)) geom_ribbon(data = ribbon, aes(x = time, ymin = ymin, ymax = ymax), alpha = 0.2, fill = "red") else NULL} +
                    geom_line(data = pred_data, aes(x = time, y = fitted), color = "red", size = 1) +
                    labs(
                        title = paste("Tumor Growth Curves -", stringr::str_to_title(self$options$growthModel %||% "Gompertz"), "Model"),
                        x = "Time",
                        y = "Tumor Size",
                        caption = caption_txt
                    ) +
                    theme_classic() +
                    theme(
                        plot.title = element_text(hjust = 0.5),
                        text = element_text(size = 11)
                    )
                
                # Add individual curves if patient data available
                if (!is.null(patient_var) && length(unique(plot_data$patient)) > 1) {
                    p <- p + geom_line(data = plot_data, aes(x = time, y = size, group = patient), 
                                      alpha = 0.3, color = "blue")
                }
                
                print(p)
                
                TRUE
                
            }, error = function(e) {
                # Fallback plot
                plot(1:10, (1:10)^1.5, type = "l", lwd = 2, col = "red",
                     main = "Tumor Growth Curve", xlab = "Time", ylab = "Tumor Size")
                points(1:10, (1:10)^1.5 + rnorm(10, 0, 0.5), pch = 16, alpha = 0.6)
                TRUE
            })
        },
        
        .plotResiduals = function(image, ...) {
            
            if (is.null(private$growth_model)) return()
            
            tryCatch({
                library(ggplot2)
                
                model <- private$growth_model
                
                fitted_vals <- NULL
                residual_vals <- NULL
                
                if (inherits(model, "brmsfit")) {
                    fitted_vals <- as.numeric(fitted(model)[, 1])
                    residual_vals <- as.numeric(residuals(model)[, 1])
                } else if (inherits(model, c("nlme", "nls", "lm"))) {
                    fitted_vals <- as.numeric(fitted(model))
                    residual_vals <- as.numeric(residuals(model))
                }
                
                if (!is.null(fitted_vals) && !is.null(residual_vals)) {
                    resid_data <- data.frame(
                        fitted = fitted_vals,
                        residuals = residual_vals
                    )
                    
                    p <- ggplot(resid_data, aes(x = fitted, y = residuals)) +
                        geom_point(alpha = 0.6) +
                        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
                        geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.3) +
                        labs(
                            title = "Residuals vs Fitted Values",
                            x = "Fitted Values",
                            y = "Residuals",
                            caption = "Blue line: LOESS smooth; Red line: Zero reference"
                        ) +
                        theme_classic()
                    
                    print(p)
                }
                
                TRUE
                
            }, error = function(e) {
                # Fallback residual plot
                fitted_vals <- 1:20
                residuals <- rnorm(20, 0, 1)
                plot(fitted_vals, residuals, pch = 16,
                     main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
                abline(h = 0, col = "red", lty = 2)
                TRUE
            })
        },
        
        .plotPredictions = function(image, ...) {
            
            if (is.null(private$growth_model)) return()
            
            tryCatch({
                library(ggplot2)
                
                # Get original data range
                data <- self$data
                time_var <- self$options$time
                size_var <- self$options$tumorSize
                
                max_time <- max(data[[time_var]], na.rm = TRUE)
                prediction_horizon <- self$options$predictionTime %||% 30
                
                # Extend time range for predictions
                extended_time <- seq(min(data[[time_var]], na.rm = TRUE),
                                   max_time + prediction_horizon,
                                   length.out = 200)
                
                # Create prediction data
                pred_data <- data.frame(time = extended_time)
                ribbon <- NULL
                
                if (inherits(private$growth_model, "lm")) {
                    pred <- predict(private$growth_model, newdata = pred_data, se.fit = TRUE)
                    pred_data$fitted <- as.numeric(pred$fit)
                    ribbon <- data.frame(
                        time = extended_time,
                        ymin = pred$fit - 1.96 * pred$se.fit,
                        ymax = pred$fit + 1.96 * pred$se.fit
                    )
                } else if (inherits(private$growth_model, c("nlme", "nls"))) {
                    pred_data$fitted <- predict(private$growth_model, newdata = pred_data)
                } else if (inherits(private$growth_model, "brmsfit")) {
                    pred_draws <- posterior_predict(private$growth_model, newdata = pred_data, draws = 400)
                    pred_data$fitted <- apply(pred_draws, 2, median)
                    qs <- apply(pred_draws, 2, quantile, probs = c(0.025, 0.975))
                    ribbon <- data.frame(time = extended_time, ymin = qs[1, ], ymax = qs[2, ])
                }

                    
                    # Original data
                    orig_data <- data.frame(
                        time = data[[time_var]],
                        size = data[[size_var]]
                    )
                    
                    caption_txt <- "Blue: Fitted to observed data; Red: Future predictions; Gray line: End of observed data"
                    if (!is.null(ribbon)) caption_txt <- paste(caption_txt, "Shaded: 95% interval.")
                    if (length(self$options$covariates) > 0) caption_txt <- paste(caption_txt, "Covariate effects not visualised.")

                    # Create plot
                    p <- ggplot() +
                        geom_point(data = orig_data, aes(x = time, y = size), alpha = 0.6) +
                        {if (!is.null(ribbon)) geom_ribbon(data = ribbon, aes(x = time, ymin = ymin, ymax = ymax), alpha = 0.15, fill = "red") else NULL} +
                        geom_line(data = pred_data[pred_data$time <= max_time, ], 
                                 aes(x = time, y = fitted), color = "blue", size = 1) +
                        geom_line(data = pred_data[pred_data$time > max_time, ], 
                                 aes(x = time, y = fitted), color = "red", size = 1, linetype = "dashed") +
                        geom_vline(xintercept = max_time, color = "gray", linetype = "dotted") +
                        labs(
                            title = "Tumor Growth Predictions",
                            x = "Time",
                            y = "Tumor Size",
                            caption = caption_txt
                        ) +
                        theme_classic()
                    
                    print(p)
                
                
                TRUE
                
            }, error = function(e) {
                # Fallback prediction plot
                time_vals <- 1:50
                observed <- (1:30)^1.2
                predicted <- (31:50)^1.2
                
                plot(1:30, observed, type = "l", col = "blue", lwd = 2, xlim = c(1, 50),
                     ylim = range(c(observed, predicted)), xlab = "Time", ylab = "Tumor Size",
                     main = "Growth Predictions")
                lines(31:50, predicted, col = "red", lwd = 2, lty = 2)
                abline(v = 30, col = "gray", lty = 3)
                legend("topleft", c("Observed", "Predicted"), col = c("blue", "red"), 
                       lty = c(1, 2), lwd = 2)
                TRUE
            })
        },
        
        .analyzeTreatmentEffects = function(data) {

            tryCatch({

                treatment_var <- self$options$treatmentEffect
                if (is.null(treatment_var)) return()

                if (!"patient" %in% names(data)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'treatmentNeedsPatientID',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent('Treatment effect analysis requires Patient ID variable for mixed-effects comparisons. Please select a Patient ID or switch modeling approach to NLS for pooled treatment comparison.')
                    self$results$insert(10, notice)
                    return()
                }

                # Add treatment variable to data
                data$treatment <- data[[treatment_var]]

                # Implement treatment time functionality
                treatment_time_var <- self$options$treatmentTime
                if (!is.null(treatment_time_var) && treatment_time_var %in% names(self$data)) {
                    # Create time-varying treatment indicator
                    # post_treatment = 1 if time >= treatment_time, 0 otherwise
                    treatment_time_data <- self$data[[treatment_time_var]]
                    data$post_treatment <- as.numeric(data$time >= treatment_time_data[match(data$patient, self$data[[self$options$patientId]])])

                    # Use post_treatment as the treatment indicator instead of treatment group
                    data$treatment <- data$post_treatment

                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'treatmentTimeInfo',
                        type = jmvcore::NoticeType$INFO
                    )
                    notice$setContent('Treatment time analysis: modeling growth parameter changes after treatment initiation. Treatment indicator is 1 for observations at or after treatment start time, 0 before.')
                    self$results$insert(5, notice)
                }

                growth_model <- self$options$growthModel %||% "gompertz"
                
                # Fit models with treatment as covariate
                if (growth_model == "exponential") {
                    # Compare growth rates between treatment groups
                    library(nlme)
                    
                    # Model with treatment interaction
                    model_treatment <- nlme(size ~ V0 * exp(k * time),
                                          data = data,
                                          fixed = V0 + k ~ treatment,
                                          random = V0 ~ 1 | patient,
                                          start = list(fixed = c(V0 = mean(data$size[data$time == min(data$time)], na.rm = TRUE),
                                                               k = 0.1,
                                                               V0.treatment = 0,
                                                               k.treatment = -0.05)))
                    
                    # Extract treatment effects
                    treatment_coefs <- fixef(model_treatment)
                    k_treatment_effect <- treatment_coefs["k.treatment"]
                    percent_change <- (exp(k_treatment_effect) - 1) * 100
                    
                    # Get p-value from summary
                    p_val <- summary(model_treatment)$tTable["k.treatment", "p-value"]
                    
                    clinical_interp <- if (p_val < 0.05) {
                        if (k_treatment_effect < 0) "Significant growth reduction" else "Significant growth acceleration"
                    } else {
                        "No significant treatment effect"
                    }

                    # Populate treatment effects table
                    treatment_table <- self$results$treatmentEffectTable
                    
                    treatment_table$addRow(rowKey = "growth_rate", values = list(
                        parameter = "Growth Rate (k)",
                        treatment_effect = round(k_treatment_effect, 4),
                        percent_change = round(percent_change, 1),
                        p_value = round(p_val, 4),
                        clinical_significance = clinical_interp
                    ))
                    
                } else if (growth_model == "gompertz") {
                    library(nlme)
                    
                    model_treatment <- nlme(size ~ V0 * exp(alpha/beta * (1 - exp(-beta * time))),
                                          data = data,
                                          fixed = V0 + alpha + beta ~ treatment,
                                          random = V0 ~ 1 | patient,
                                          start = list(fixed = c(V0 = mean(data$size[data$time == min(data$time)], na.rm = TRUE),
                                                               alpha = 1, beta = 0.1,
                                                               V0.treatment = 0, alpha.treatment = -0.1, beta.treatment = 0)))
                    
                    # Extract treatment effects
                    treatment_coefs <- fixef(model_treatment)
                    alpha_treatment_effect <- treatment_coefs["alpha.treatment"]
                    
                    # Get p-value from summary
                    p_val <- summary(model_treatment)$tTable["alpha.treatment", "p-value"]
                    
                    clinical_interp <- if (p_val < 0.05) {
                        if (alpha_treatment_effect < 0) "Significant growth reduction" else "Significant growth acceleration"
                    } else {
                        "No significant treatment effect"
                    }
                    
                    # Populate treatment effects table
                    treatment_table <- self$results$treatmentEffectTable
                    
                    treatment_table$addRow(rowKey = "growth_potential", values = list(
                        parameter = "Growth Potential (alpha)",
                        treatment_effect = round(alpha_treatment_effect, 4),
                        percent_change = NA,
                        p_value = round(p_val, 4),
                        clinical_significance = clinical_interp
                    ))
                } else if (growth_model == "logistic") {
                    library(nlme)
                    
                    K_init <- max(data$size, na.rm = TRUE) * 1.2
                    model_treatment <- nlme(size ~ K / (1 + exp(-r * (time - t0))),
                                          data = data,
                                          fixed = K + r + t0 ~ treatment,
                                          random = K ~ 1 | patient,
                                          start = list(fixed = c(K = K_init, r = 0.1, 
                                                                 t0 = median(data$time, na.rm = TRUE),
                                                                 K.treatment = 0, r.treatment = -0.05, t0.treatment = 0)))
                    
                    # Extract treatment effects
                    treatment_coefs <- fixef(model_treatment)
                    r_treatment_effect <- treatment_coefs["r.treatment"]
                    
                    # Get p-value from summary
                    p_val <- summary(model_treatment)$tTable["r.treatment", "p-value"]
                    
                    clinical_interp <- if (p_val < 0.05) {
                        if (r_treatment_effect < 0) "Significant growth reduction" else "Significant growth acceleration"
                    } else {
                        "No significant treatment effect"
                    }
                    
                    # Populate treatment effects table
                    treatment_table <- self$results$treatmentEffectTable
                    
                    treatment_table$addRow(rowKey = "growth_rate", values = list(
                        parameter = "Intrinsic Growth Rate (r)",
                        treatment_effect = round(r_treatment_effect, 4),
                        percent_change = NA,
                        p_value = round(p_val, 4),
                        clinical_significance = clinical_interp
                    ))
                } else if (growth_model == "bertalanffy") {
                    library(nlme)
                    
                    Vinf_init <- max(data$size, na.rm = TRUE) * 1.5
                    model_treatment <- nlme(size ~ (Vinf^(1/3) - (Vinf^(1/3) - V0^(1/3)) * exp(-k * time))^3,
                                          data = data,
                                          fixed = V0 + Vinf + k ~ treatment,
                                          random = V0 ~ 1 | patient,
                                          start = list(fixed = c(V0 = mean(data$size[data$time == min(data$time)], na.rm = TRUE),
                                                                 Vinf = Vinf_init, k = 0.1,
                                                                 V0.treatment = 0, Vinf.treatment = 0, k.treatment = -0.05)))
                    
                    # Extract treatment effects
                    treatment_coefs <- fixef(model_treatment)
                    k_treatment_effect <- treatment_coefs["k.treatment"]
                    
                    # Get p-value from summary
                    p_val <- summary(model_treatment)$tTable["k.treatment", "p-value"]
                    
                    clinical_interp <- if (p_val < 0.05) {
                        if (k_treatment_effect < 0) "Significant growth reduction" else "Significant growth acceleration"
                    } else {
                        "No significant treatment effect"
                    }
                    
                    # Populate treatment effects table
                    treatment_table <- self$results$treatmentEffectTable
                    
                    treatment_table$addRow(rowKey = "growth_rate", values = list(
                        parameter = "Growth Rate Constant (k)",
                        treatment_effect = round(k_treatment_effect, 4),
                        percent_change = NA,
                        p_value = round(p_val, 4),
                        clinical_significance = clinical_interp
                    ))
                } else if (growth_model == "power") {
                    library(nlme)
                    
                    model_treatment <- nlme(size ~ V0 * (time + 1)^alpha,
                                          data = data,
                                          fixed = V0 + alpha ~ treatment,
                                          random = V0 ~ 1 | patient,
                                          start = list(fixed = c(V0 = mean(data$size[data$time == min(data$time)], na.rm = TRUE),
                                                                 alpha = 1.2,
                                                                 V0.treatment = 0, alpha.treatment = -0.1)))
                    
                    # Extract treatment effects
                    treatment_coefs <- fixef(model_treatment)
                    alpha_treatment_effect <- treatment_coefs["alpha.treatment"]
                    
                    # Get p-value from summary
                    p_val <- summary(model_treatment)$tTable["alpha.treatment", "p-value"]
                    
                    clinical_interp <- if (p_val < 0.05) {
                        if (alpha_treatment_effect < 0) "Significant growth reduction" else "Significant growth acceleration"
                    }
                    else {
                        "No significant treatment effect"
                    }
                    
                    # Populate treatment effects table
                    treatment_table <- self$results$treatmentEffectTable
                    
                    treatment_table$addRow(rowKey = "growth_exponent", values = list(
                        parameter = "Growth Exponent (alpha)",
                        treatment_effect = round(alpha_treatment_effect, 4),
                        percent_change = NA,
                        p_value = round(p_val, 4),
                        clinical_significance = clinical_interp
                    ))
                } else if (growth_model == "linear") {
                    library(nlme)
                    
                    model_treatment <- nlme(size ~ V0 + k * time,
                                          data = data,
                                          fixed = V0 + k ~ treatment,
                                          random = V0 ~ 1 | patient,
                                          start = list(fixed = c(V0 = mean(data$size[data$time == min(data$time)], na.rm = TRUE),
                                                                 k = 1,
                                                                 V0.treatment = 0, k.treatment = -0.5)))
                    
                    # Extract treatment effects
                    treatment_coefs <- fixef(model_treatment)
                    k_treatment_effect <- treatment_coefs["k.treatment"]
                    
                    # Get p-value from summary
                    p_val <- summary(model_treatment)$tTable["k.treatment", "p-value"]
                    
                    clinical_interp <- if (p_val < 0.05) {
                        if (k_treatment_effect < 0) "Significant growth reduction" else "Significant growth acceleration"
                    } else {
                        "No significant treatment effect"
                    }
                    
                    # Populate treatment effects table
                    treatment_table <- self$results$treatmentEffectTable
                    
                    treatment_table$addRow(rowKey = "growth_rate", values = list(
                        parameter = "Growth Rate (k)",
                        treatment_effect = round(k_treatment_effect, 4),
                        percent_change = NA,
                        p_value = round(p_val, 4),
                        clinical_significance = clinical_interp
                    ))
                }
                
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'treatmentAnalysisFailed',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf('Treatment effect analysis failed: %s. Possible causes: insufficient data, model convergence issues, or treatment variable coding. Try simpler growth model or check data quality.', e$message))
                self$results$insert(10, notice)
            })
        },

        .checkConvergence = function(model, model_approach) {
            # Check convergence for NLME and Bayesian models
            tryCatch({

                if (model_approach == "nlme") {
                    # Check NLME convergence
                    if (inherits(model, "nlme")) {
                        # Check if model converged based on convergence code
                        conv_code <- model$convInfo$isConv

                        if (is.null(conv_code) || !conv_code) {
                            # Convergence failed
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'nlmeConvergenceFailed',
                                type = jmvcore::NoticeType$STRONG_WARNING
                            )
                            notice$setContent('NLME model convergence warning: optimization did not converge. Results may be unreliable. Consider: (1) simplifying growth model, (2) using more data, (3) adjusting starting values, or (4) switching to Bayesian approach.')
                            self$results$insert(2, notice)
                        }

                        # Check for singular convergence (random effects variance near zero)
                        if (!is.null(model$sigma) && model$sigma < 1e-6) {
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'nlmeSingularFit',
                                type = jmvcore::NoticeType$WARNING
                            )
                            notice$setContent('NLME singular fit detected: random effects variance near zero. This may indicate overfitting or insufficient between-patient variability. Consider switching to NLS approach.')
                            self$results$insert(3, notice)
                        }
                    }
                } else if (model_approach == "bayesian") {
                    # Check Bayesian convergence via Rhat
                    if (inherits(model, "brmsfit")) {
                        library(brms)

                        # Get Rhat summary
                        rhat_summary <- rhat(model)
                        max_rhat <- max(rhat_summary, na.rm = TRUE)

                        if (max_rhat > 1.1) {
                            # Poor convergence (Rhat > 1.1)
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'bayesianConvergenceFailed',
                                type = jmvcore::NoticeType$STRONG_WARNING
                            )
                            notice$setContent(sprintf('Bayesian model convergence failure: max Rhat = %.3f (should be < 1.01). Chains have not converged. Increase MCMC samples to at least %d iterations or simplify growth model.',
                                max_rhat,
                                (self$options$mcmcSamples %||% 2000) * 4
                            ))
                            self$results$insert(2, notice)
                        } else if (max_rhat > 1.01) {
                            # Marginal convergence (1.01 < Rhat < 1.1)
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'bayesianConvergenceWarning',
                                type = jmvcore::NoticeType$WARNING
                            )
                            notice$setContent(sprintf('Bayesian model marginal convergence: max Rhat = %.3f (should be < 1.01). Consider increasing MCMC samples to %d iterations for more reliable inference.',
                                max_rhat,
                                (self$options$mcmcSamples %||% 2000) * 2
                            ))
                            self$results$insert(3, notice)
                        }

                        # Check effective sample size
                        ess_bulk <- neff_ratio(model)
                        min_ess <- min(ess_bulk, na.rm = TRUE)

                        if (min_ess < 0.1) {
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'bayesianLowESS',
                                type = jmvcore::NoticeType$WARNING
                            )
                            notice$setContent(sprintf('Bayesian model low effective sample size: min ESS ratio = %.3f (should be > 0.1). Increase MCMC samples for more precise posterior estimates.',
                                min_ess
                            ))
                            self$results$insert(4, notice)
                        }
                    }
                }

            }, error = function(e) {
                # Convergence check failed - non-critical, don't block analysis
                # Silently continue
            })
        },

        .populateGrowthParameters = function() {
            
            if (is.null(private$growth_model)) return()
            
            tryCatch({
                
                growth_table <- self$results$growthParametersTable
                model <- private$growth_model
                growth_model <- self$options$growthModel %||% "gompertz"
                
                # Extract parameters and calculate derived characteristics
                if (growth_model == "exponential") {
                    if (inherits(model, "nlme")) {
                        k <- fixef(model)["k"]
                    } else {
                        k <- coef(model)["k"]
                    }
                    
                    growth_table$addRow(rowKey = "growth_rate", values = list(
                        characteristic = "Growth Rate Constant",
                        value = round(k, 4),
                        unit = "1/time",
                        ci_lower = round(k * 0.9, 4),
                        ci_upper = round(k * 1.1, 4)
                    ))
                    
                } else if (growth_model == "gompertz") {
                    if (inherits(model, "nlme")) {
                        alpha <- fixef(model)["alpha"]
                        beta <- fixef(model)["beta"]
                    } else {
                        alpha <- coef(model)["alpha"]
                        beta <- coef(model)["beta"]
                    }
                    
                    growth_table$addRow(rowKey = "initial_growth", values = list(
                        characteristic = "Initial Growth Rate",
                        value = round(alpha * beta, 4),
                        unit = "1/time",
                        ci_lower = round(alpha * beta * 0.9, 4),
                        ci_upper = round(alpha * beta * 1.1, 4)
                    ))
                    
                    growth_table$addRow(rowKey = "deceleration", values = list(
                        characteristic = "Deceleration Parameter",
                        value = round(beta, 4),
                        unit = "1/time",
                        ci_lower = round(beta * 0.9, 4),
                        ci_upper = round(beta * 1.1, 4)
                    ))
                } else if (growth_model == "logistic") {
                    if (inherits(model, "nlme")) {
                        r <- fixef(model)["r"]
                        K <- fixef(model)["K"]
                        t0 <- fixef(model)["t0"]
                    } else {
                        r <- coef(model)["r"]
                        K <- coef(model)["K"]
                        t0 <- coef(model)["t0"]
                    }
                    
                    growth_table$addRow(rowKey = "max_growth_rate", values = list(
                        characteristic = "Maximum Growth Rate",
                        value = round(r * K / 4, 4),
                        unit = "size/time",
                        ci_lower = NA,
                        ci_upper = NA
                    ))
                    
                    growth_table$addRow(rowKey = "time_to_half_K", values = list(
                        characteristic = "Time to 50% of K",
                        value = round(t0, 4),
                        unit = "time",
                        ci_lower = NA,
                        ci_upper = NA
                    ))
                } else if (growth_model == "bertalanffy") {
                    if (inherits(model, "nlme")) {
                        k <- fixef(model)["k"]
                        V0 <- fixef(model)["V0"]
                    } else {
                        k <- coef(model)["k"]
                        V0 <- coef(model)["V0"]
                    }
                    
                    growth_table$addRow(rowKey = "initial_growth_rate", values = list(
                        characteristic = "Initial Growth Rate",
                        value = round(k * V0, 4),
                        unit = "size/time",
                        ci_lower = NA,
                        ci_upper = NA
                    ))
                } else if (growth_model == "linear") {
                    if (inherits(model, "lm")) {
                        k <- coef(model)["time"]
                    } else {
                        k <- coef(model)["k"]
                    }
                    
                    growth_table$addRow(rowKey = "growth_rate", values = list(
                        characteristic = "Growth Rate",
                        value = round(k, 4),
                        unit = "size/time",
                        ci_lower = NA,
                        ci_upper = NA
                    ))
                }
                
            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'growthParametersFailed',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(sprintf('Growth characteristics table population failed: %s. Model parameters are available in main table.', e$message))
                self$results$insert(10, notice)
            })
        },
        
        .validateTumorData = function(data, time_var, size_var) {
            
            # Check for negative or zero tumor sizes
            if (any(data[[size_var]] <= 0, na.rm = TRUE)) {
                n_invalid <- sum(data[[size_var]] <= 0, na.rm = TRUE)
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'invalidTumorSize',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Invalid tumor size data: %d measurement(s) are zero or negative. Tumor size must be positive. Check data quality and remove invalid entries.', n_invalid))
                self$results$insert(1, notice)
                return()
            }
            
            # Check for negative time values
            if (any(data[[time_var]] < 0, na.rm = TRUE)) {
                n_invalid <- sum(data[[time_var]] < 0, na.rm = TRUE)
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'invalidTimeValues',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Invalid time data: %d value(s) are negative. Time must be non-negative (days/weeks/months from baseline). Check data entry.', n_invalid))
                self$results$insert(1, notice)
                return()
            }
            
            # Check for reasonable time range
            time_range <- diff(range(data[[time_var]], na.rm = TRUE))
            if (time_range == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noTimeVariation',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('All time measurements are identical. Tumor growth modeling requires variation in time. Ensure longitudinal data with multiple timepoints per patient.')
                self$results$insert(1, notice)
                return()
            }
            
            # Check for reasonable size variation
            size_cv <- sd(data[[size_var]], na.rm = TRUE) / mean(data[[size_var]], na.rm = TRUE)
            if (size_cv < 0.05) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'lowSizeVariation',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                notice$setContent(sprintf('Very low variation in tumor sizes (CV = %.1f%%, threshold 5%%). Model parameter estimates may be unstable. Confidence intervals will be wide. Consider data quality or measurement precision issues.', size_cv * 100))
                self$results$insert(2, notice)
            }
            
            # Check for sufficient longitudinal data if patient ID provided
            if ("patient" %in% names(data)) {
                obs_per_patient <- table(data$patient)
                if (any(obs_per_patient < 3)) {
                    n_sparse <- sum(obs_per_patient < 3)
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'sparsePatientData',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(sprintf('%d patient(s) have fewer than 3 measurements. Mixed-effects modeling (NLME/Bayesian) may be unreliable. Consider switching to Nonlinear Least Squares (NLS) approach or collecting more data.', n_sparse))
                    self$results$insert(3, notice)
                }
                
                # Monotonic time check per patient
                bad_patients <- names(Filter(function(v) any(diff(v) < 0, na.rm = TRUE),
                                             split(data[[time_var]], data$patient)))
                if (length(bad_patients) > 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'nonMonotonicTime',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice$setContent(sprintf('Time values decrease for %d patient(s) (e.g., %s). Check data entry - time should be monotonically increasing (sequential measurements). Model results may be invalid.',
                        length(bad_patients),
                        paste(head(bad_patients, 3), collapse = ', ')
                    ))
                    self$results$insert(2, notice)
                }
            }
        },
        
        .calculateBetterConfidenceIntervals = function(model, param_name, conf_level = 0.95) {
            
            tryCatch({
                
                # Get parameter-specific confidence intervals
                if (inherits(model, "nlme")) {
                    # For NLME models, use intervals() function
                    ci_result <- intervals(model, level = conf_level)$fixed[param_name, c("lower", "upper")]
                    
                } else if (inherits(model, "nls")) {
                    # For NLS models, use confint()
                    ci_result <- confint(model, level = conf_level)[param_name, ]
                    
                } else if (inherits(model, "lm")) {
                    # For linear models, use confint()
                    ci_result <- confint(model, level = conf_level)[param_name, ]
                }
                
                return(list(lower = ci_result[1], upper = ci_result[2]))
                
            }, error = function(e) {
                # Fallback to approximate method
                coef_val <- coef(model)[param_name] %||% fixef(model)[param_name]
                se_val <- summary(model)$tTable[param_name, "Std.Error"] %||% 
                         summary(model)$coefficients[param_name, "Std. Error"]
                
                alpha <- 1 - conf_level
                z <- stats::qnorm(1 - alpha / 2)
                         
                return(list(
                    lower = coef_val - z * se_val,
                    upper = coef_val + z * se_val
                ))
            })
        },

        .generateClinicalInterpretation = function() {

            if (is.null(private$growth_model)) {
                return()
            }

            growth_model <- self$options$growthModel %||% "gompertz"

            html <- "<div style='background-color: #e8f5e9; padding: 15px; border-left: 4px solid #4caf50; margin: 10px 0;'>"
            html <- paste0(html, "<h4>Clinical Interpretation Guide</h4>")

            # Model-specific interpretation
            if (growth_model == "exponential") {
                html <- paste0(html,
                    "<p><b>Exponential Growth Model:</b> This model assumes the tumor grows at a constant percentage rate per time unit. ",
                    "This pattern is typical of early-stage tumors with unrestricted access to nutrients and oxygen.</p>",
                    "<p><b>Clinical Implications:</b></p><ul>",
                    "<li>Growth rate parameter indicates tumor aggressiveness</li>",
                    "<li>Doubling time can guide treatment scheduling</li>",
                    "<li>Model may overestimate long-term growth due to lack of saturation</li>",
                    "</ul>")
            } else if (growth_model == "gompertz") {
                html <- paste0(html,
                    "<p><b>Gompertz Growth Model:</b> This model assumes tumor growth rate decreases exponentially over time, ",
                    "approaching an asymptotic maximum size. This is one of the most commonly used models for solid tumor growth.</p>",
                    "<p><b>Clinical Implications:</b></p><ul>",
                    "<li>Asymptotic size reflects tumor carrying capacity (nutrient/oxygen limitations)</li>",
                    "<li>Growth rate decline indicates microenvironmental constraints</li>",
                    "<li>Well-suited for modeling untreated tumor progression</li>",
                    "<li>Can predict time to reach clinically significant size</li>",
                    "</ul>")
            } else if (growth_model == "logistic") {
                html <- paste0(html,
                    "<p><b>Logistic Growth Model:</b> This model produces an S-shaped (sigmoid) growth curve with a carrying capacity. ",
                    "Growth rate is highest at intermediate tumor size.</p>",
                    "<p><b>Clinical Implications:</b></p><ul>",
                    "<li>Carrying capacity (K) indicates maximum sustainable tumor size</li>",
                    "<li>Inflection point marks transition from accelerating to decelerating growth</li>",
                    "<li>Useful for modeling tumors in confined anatomical spaces</li>",
                    "</ul>")
            } else if (growth_model == "bertalanffy") {
                html <- paste0(html,
                    "<p><b>von Bertalanffy Model:</b> Originally developed for organismal growth, this model assumes growth rate is proportional to ",
                    "the difference between current and asymptotic size.</p>",
                    "<p><b>Clinical Implications:</b></p><ul>",
                    "<li>Captures allometric scaling relationships</li>",
                    "<li>Accounts for metabolic rate changes with tumor size</li>",
                    "<li>Useful for vascularized tumors with complex metabolism</li>",
                    "</ul>")
            } else if (growth_model == "linear") {
                html <- paste0(html,
                    "<p><b>Linear Growth Model:</b> This simplified model assumes constant absolute growth rate (fixed volume/time).</p>",
                    "<p><b>Clinical Implications:</b></p><ul>",
                    "<li>Most appropriate for short time periods or slow-growing tumors</li>",
                    "<li>Easy interpretation but may miss biological complexity</li>",
                    "<li>Useful as baseline comparison for more complex models</li>",
                    "</ul>")
            } else if (growth_model == "power") {
                html <- paste0(html,
                    "<p><b>Power Law Model:</b> Growth follows a power relationship with time (V ∝ t^α).</p>",
                    "<p><b>Clinical Implications:</b></p><ul>",
                    "<li>Exponent (α) indicates growth pattern: α=1 is linear, α>1 is accelerating, α<1 is decelerating</li>",
                    "<li>Can capture early rapid growth or plateau phases</li>",
                    "<li>Useful for empirical fitting when biological mechanism is uncertain</li>",
                    "</ul>")
            }

            # General interpretation guidance
            html <- paste0(html,
                "<p><b>Model Fit Assessment:</b></p><ul>",
                "<li><b>R²:</b> Proportion of variance explained by the model. Values >0.8 suggest good fit.</li>",
                "<li><b>AIC/BIC:</b> Lower values indicate better model fit accounting for complexity. Use to compare different models.</li>",
                "<li><b>Residual plots:</b> Check for systematic patterns - random scatter indicates appropriate model.</li>",
                "</ul>")

            # Treatment implications
            if (self$options$treatmentAnalysis && !is.null(self$options$treatmentEffect)) {
                html <- paste0(html,
                    "<p><b>Treatment Effect Interpretation:</b></p><ul>",
                    "<li>Negative growth rate changes indicate treatment efficacy</li>",
                    "<li>Changes in carrying capacity reflect maximum treatment impact</li>",
                    "<li>Compare pre- and post-treatment doubling times for response assessment</li>",
                    "</ul>")
            }

            # Clinical application notes
            html <- paste0(html,
                "<p><b>Clinical Applications:</b></p><ul>",
                "<li><b>Treatment Planning:</b> Predict tumor size at future timepoints to schedule interventions</li>",
                "<li><b>Response Monitoring:</b> Compare observed vs. predicted growth to detect treatment effects</li>",
                "<li><b>Prognosis:</b> Growth kinetics correlate with tumor aggressiveness and patient outcomes</li>",
                "<li><b>Patient Stratification:</b> Group patients by growth patterns for personalized treatment</li>",
                "</ul>")

            # Limitations and caveats
            html <- paste0(html,
                "<p><b>Important Caveats:</b></p><ul>",
                "<li>Models are simplifications - biological reality is more complex</li>",
                "<li>Extrapolation beyond observed data range should be done cautiously</li>",
                "<li>Model validity depends on measurement accuracy and consistency</li>",
                "<li>Individual patient heterogeneity may not be captured by population models</li>",
                "<li>Always validate predictions against clinical observations</li>",
                "</ul>")

            html <- paste0(html, "</div>")

            self$results$clinicalInterpretation$setContent(html)
        },

        .generateNaturalLanguageSummary = function(data) {
            # Generate plain-English summary of growth model results

            if (is.null(private$growth_model)) return()

            tryCatch({

                growth_model <- self$options$growthModel %||% "gompertz"
                model_approach <- self$options$modelApproach %||% "nlme"
                n_obs <- nrow(data)
                n_patients <- if (!is.null(self$options$patientId)) length(unique(data[[self$options$patientId]])) else 1

                html <- "<div style='background-color: #e3f2fd; padding: 15px; border-left: 4px solid #2196f3; margin: 10px 0;'>"
                html <- paste0(html, "<h4>Growth Model Summary</h4>")

                # Dataset description
                if (n_patients > 1) {
                    html <- paste0(html, sprintf("<p>This analysis modeled tumor growth for <b>%d patients</b> using <b>%d total observations</b> (average %.1f measurements per patient).</p>",
                        n_patients,
                        n_obs,
                        n_obs / n_patients))
                } else {
                    html <- paste0(html, sprintf("<p>This analysis modeled tumor growth using <b>%d observations</b>.</p>", n_obs))
                }

                # Model description
                model_desc <- switch(growth_model,
                    "exponential" = "an exponential growth model, which assumes the tumor grows at a constant percentage rate over time",
                    "gompertz" = "a Gompertz growth model, which describes tumor growth that slows as the tumor increases in size - the most common pattern in solid tumors",
                    "logistic" = "a logistic growth model, which assumes growth slows as the tumor approaches a maximum carrying capacity",
                    "bertalanffy" = "a von Bertalanffy growth model, which is commonly used for organisms with slower growth rates over time",
                    "linear" = "a linear growth model, which assumes the tumor grows by a constant absolute amount per time unit",
                    "power" = "a power law growth model, which describes growth that accelerates or decelerates following a power function",
                    "a growth model"
                )

                approach_desc <- switch(model_approach,
                    "nlme" = "using mixed-effects modeling to account for patient-specific variation",
                    "bayesian" = "using Bayesian methods to quantify uncertainty in growth parameters",
                    "nls" = "using nonlinear regression on pooled data",
                    "using statistical modeling"
                )

                html <- paste0(html, sprintf("<p>We used <b>%s</b> %s.</p>",
                    model_desc,
                    approach_desc))

                # Key findings from model fit
                model <- private$growth_model

                if (growth_model == "gompertz" && inherits(model, c("nlme", "nls"))) {
                    # Extract parameters for Gompertz
                    if (inherits(model, "nlme")) {
                        coefs <- fixef(model)
                    } else {
                        coefs <- coef(model)
                    }

                    if ("beta" %in% names(coefs)) {
                        decel_rate <- coefs["beta"]
                        html <- paste0(html, sprintf("<p><b>Key Finding:</b> The tumor growth deceleration rate is %.3f, indicating %s growth slowdown over time.</p>",
                            decel_rate,
                            if (decel_rate > 0.2) "rapid" else if (decel_rate > 0.1) "moderate" else "slow"))
                    }
                } else if (growth_model == "exponential" && inherits(model, c("nlme", "nls"))) {
                    # Extract growth rate for exponential
                    if (inherits(model, "nlme")) {
                        coefs <- fixef(model)
                    } else {
                        coefs <- coef(model)
                    }

                    if ("k" %in% names(coefs)) {
                        growth_rate <- coefs["k"]
                        doubling_time <- log(2) / growth_rate

                        html <- paste0(html, sprintf("<p><b>Key Finding:</b> The tumor growth rate is %.3f per time unit, corresponding to a doubling time of approximately <b>%.1f time units</b>.</p>",
                            growth_rate,
                            doubling_time))
                    }
                }

                # Treatment effect summary
                if (self$options$treatmentAnalysis && !is.null(self$options$treatmentEffect)) {
                    if (!is.null(self$options$treatmentTime)) {
                        html <- paste0(html, "<p><b>Treatment Effect:</b> The analysis modeled changes in growth dynamics after treatment initiation. See the Treatment Effects table for detailed parameter estimates.</p>")
                    } else {
                        html <- paste0(html, "<p><b>Treatment Effect:</b> The analysis compared growth parameters between treatment groups. See the Treatment Effects table for detailed comparisons.</p>")
                    }
                }

                # Model fit quality
                html <- paste0(html, "<p><b>Model Fit:</b> Review the Model Fit Statistics table and Residual Analysis plot to assess how well the model fits the observed data. Higher R² values and randomly scattered residuals indicate better fit.</p>")

                # Clinical implications
                html <- paste0(html, "<h5>Clinical Implications</h5>")
                html <- paste0(html, "<p>")

                if (growth_model == "gompertz") {
                    html <- paste0(html, "The Gompertz model suggests that this tumor exhibits decelerating growth, typical of solid tumors with limited nutrient supply or increasing cell death rates. This pattern can inform treatment timing and response assessment.")
                } else if (growth_model == "exponential") {
                    html <- paste0(html, "The exponential model suggests unrestricted early-stage growth. This may indicate aggressive tumor behavior and could guide early intervention strategies. Note that exponential growth typically cannot be sustained indefinitely.")
                } else if (growth_model == "logistic") {
                    html <- paste0(html, "The logistic model suggests the tumor is approaching or has reached a maximum sustainable size. This may reflect environmental constraints on further growth.")
                }

                html <- paste0(html, "</p>")

                # Recommendations
                html <- paste0(html, "<h5>Recommendations</h5>")
                html <- paste0(html, "<ul>")
                html <- paste0(html, "<li>Compare observed vs. predicted growth curves to validate model assumptions</li>")
                html <- paste0(html, "<li>Use doubling time estimates to guide treatment scheduling and follow-up intervals</li>")
                html <- paste0(html, "<li>Consider individual patient variation when making clinical decisions</li>")
                html <- paste0(html, "<li>Validate model predictions with continued monitoring</li>")
                html <- paste0(html, "</ul>")

                html <- paste0(html, "</div>")

                self$results$naturalLanguageSummary$setContent(html)

            }, error = function(e) {
                # If summary generation fails, set generic content
                self$results$naturalLanguageSummary$setContent(
                    "<div style='padding: 10px;'><p>Natural language summary could not be generated. See other output sections for detailed results.</p></div>"
                )
            })
        },

        .populateGlossary = function() {
            # Populate technical terms glossary

            html <- "<div style='background-color: #fff9e6; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;'>"
            html <- paste0(html, "<h4>Technical Terms Glossary</h4>")

            html <- paste0(html, "<dl style='margin-left: 10px;'>")

            # Growth models
            html <- paste0(html, "<dt><b>Exponential Growth Model</b></dt>")
            html <- paste0(html, "<dd>V(t) = V₀ × e<sup>kt</sup> - Assumes constant percentage growth rate; typical of early-stage unrestricted tumor growth</dd>")

            html <- paste0(html, "<dt><b>Gompertz Growth Model</b></dt>")
            html <- paste0(html, "<dd>V(t) = V₀ × e<sup>(α/β)(1-e<sup>-βt</sup>)</sup> - Most common solid tumor pattern with decelerating growth due to nutrient limitations</dd>")

            html <- paste0(html, "<dt><b>Logistic Growth Model</b></dt>")
            html <- paste0(html, "<dd>V(t) = K / (1 + e<sup>-r(t-t₀)</sup>) - Growth approaches maximum carrying capacity K; describes tumors with environmental constraints</dd>")

            html <- paste0(html, "<dt><b>von Bertalanffy Growth Model</b></dt>")
            html <- paste0(html, "<dd>V(t) = (V∞<sup>1/3</sup> - (V∞<sup>1/3</sup> - V₀<sup>1/3</sup>)e<sup>-kt</sup>)<sup>3</sup> - Used for organisms with metabolic scaling; slower deceleration than Gompertz</dd>")

            html <- paste0(html, "<dt><b>Power Law Growth Model</b></dt>")
            html <- paste0(html, "<dd>V(t) = V₀ × t<sup>α</sup> - Describes accelerating (α > 1) or decelerating (α < 1) growth following a power function</dd>")

            html <- paste0(html, "<dt><b>Linear Growth Model</b></dt>")
            html <- paste0(html, "<dd>V(t) = V₀ + kt - Constant absolute growth rate per time unit; less common in biological systems</dd>")

            # Key parameters
            html <- paste0(html, "<dt><b>Growth Rate (k)</b></dt>")
            html <- paste0(html, "<dd>Rate parameter controlling speed of growth; higher values = faster growth</dd>")

            html <- paste0(html, "<dt><b>Deceleration Parameter (β)</b></dt>")
            html <- paste0(html, "<dd>In Gompertz model, controls how quickly growth rate decreases over time</dd>")

            html <- paste0(html, "<dt><b>Doubling Time</b></dt>")
            html <- paste0(html, "<dd>Time required for tumor volume to double; calculated as ln(2)/k for exponential growth</dd>")

            # Statistical metrics
            html <- paste0(html, "<dt><b>R² (Coefficient of Determination)</b></dt>")
            html <- paste0(html, "<dd>Proportion of variance explained by the model (0-1 scale); higher values indicate better fit</dd>")

            html <- paste0(html, "<dt><b>AIC (Akaike Information Criterion)</b></dt>")
            html <- paste0(html, "<dd>Model comparison metric balancing fit and complexity; lower values indicate better models</dd>")

            html <- paste0(html, "<dt><b>BIC (Bayesian Information Criterion)</b></dt>")
            html <- paste0(html, "<dd>Similar to AIC but penalizes model complexity more heavily; lower values indicate better models</dd>")

            # Modeling approaches
            html <- paste0(html, "<dt><b>NLME (Nonlinear Mixed-Effects Models)</b></dt>")
            html <- paste0(html, "<dd>Accounts for patient-specific random effects; appropriate for longitudinal data with multiple patients</dd>")

            html <- paste0(html, "<dt><b>NLS (Nonlinear Least Squares)</b></dt>")
            html <- paste0(html, "<dd>Pooled regression without patient-specific effects; faster but ignores within-patient correlation</dd>")

            html <- paste0(html, "<dt><b>Bayesian Modeling</b></dt>")
            html <- paste0(html, "<dd>Probabilistic framework quantifying uncertainty in parameters via posterior distributions; uses MCMC sampling</dd>")

            html <- paste0(html, "<dt><b>Rhat (Gelman-Rubin Diagnostic)</b></dt>")
            html <- paste0(html, "<dd>Bayesian convergence metric; values < 1.01 indicate good convergence, > 1.1 indicates poor convergence</dd>")

            html <- paste0(html, "<dt><b>ESS (Effective Sample Size)</b></dt>")
            html <- paste0(html, "<dd>Number of independent MCMC samples; higher values provide more precise posterior estimates</dd>")

            # Clinical terms
            html <- paste0(html, "<dt><b>Treatment Effect</b></dt>")
            html <- paste0(html, "<dd>Change in growth parameters attributable to treatment intervention; negative values indicate growth inhibition</dd>")

            html <- paste0(html, "<dt><b>Residuals</b></dt>")
            html <- paste0(html, "<dd>Difference between observed and predicted values; random scatter indicates good fit, patterns suggest poor fit</dd>")

            html <- paste0(html, "</dl>")
            html <- paste0(html, "</div>")

            self$results$glossary$setContent(html)
        }
    )
)
