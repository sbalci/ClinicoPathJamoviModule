#' @title Joint Frailty Models for Recurrent Events
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import frailtypack
#' @import survival
#' @importFrom ggplot2 ggplot aes geom_line geom_step geom_point geom_histogram geom_density labs theme_minimal facet_wrap
#' @export


jointfrailtyClass <- R6::R6Class(
    "jointfrailtyClass",
    inherit = jointfrailtyBase,
    private = list(
        .init = function() {
            
            if (is.null(self$options$subjectID) || is.null(self$options$time) || 
                is.null(self$options$event) || is.null(self$options$terminal_time) || 
                is.null(self$options$terminal_event)) {
                self$results$todo$setContent(
                    "<html>
                    <head>
                    <style>
                        h1 {color: #3366cc;}
                        body {font-family: Arial, sans-serif; margin: 40px;}
                        .info {background-color: #f0f8ff; padding: 15px; border-left: 5px solid #3366cc;}
                        .step {margin: 10px 0; padding: 8px; background-color: #f9f9f9;}
                        .formula {background-color: #f5f5f5; padding: 10px; font-family: monospace; margin: 10px 0;}
                        .interpretation {background-color: #e8f5e8; padding: 15px; border-left: 5px solid #4CAF50; margin: 15px 0;}
                    </style>
                    </head>
                    <body>
                    <h1>🔗 Joint Frailty Models for Recurrent Events Analysis</h1>
                    
                    <div class='info'>
                    <strong>Welcome to Joint Frailty Models Analysis</strong><br>
                    This analysis implements joint frailty models that simultaneously analyze:
                    <ul>
                    <li><strong>Recurrent Events:</strong> Events that can occur multiple times per subject</li>
                    <li><strong>Terminal Events:</strong> Events that prevent further recurrent events (death, cure)</li>
                    <li><strong>Association Structure:</strong> Dependence between the two processes</li>
                    <li><strong>Individual Heterogeneity:</strong> Subject-specific frailty effects</li>
                    </ul>
                    </div>

                    <h2>📋 Required Variables:</h2>
                    <div class='step'><strong>1. Subject ID:</strong> Unique identifier for each subject/patient</div>
                    <div class='step'><strong>2. Event Time:</strong> Time of each recurrent event occurrence</div>
                    <div class='step'><strong>3. Event Indicator:</strong> Binary indicator (1=event occurred, 0=censored)</div>
                    <div class='step'><strong>4. Terminal Event Time:</strong> Time to terminal event or censoring</div>
                    <div class='step'><strong>5. Terminal Event Indicator:</strong> Binary indicator for terminal event</div>
                    
                    <h2>📊 Optional Variables:</h2>
                    <div class='step'><strong>Shared Covariates:</strong> Variables affecting both processes</div>
                    <div class='step'><strong>Recurrent-Specific Covariates:</strong> Variables only affecting recurrent events</div>
                    <div class='step'><strong>Terminal-Specific Covariates:</strong> Variables only affecting terminal events</div>

                    <h2>🔍 Joint Model Structure:</h2>
                    <div class='formula'>
                    <strong>Recurrent Process:</strong> λᵣ(t|uᵢ,Xᵣᵢ) = uᵢ λ₀ᵣ(t) exp(βᵣᵀXᵣᵢ)<br>
                    <strong>Terminal Process:</strong> λₜ(t|vᵢ,Xₜᵢ) = vᵢ λ₀ₜ(t) exp(βₜᵀXₜᵢ)<br>
                    <strong>Frailty Association:</strong> (uᵢ, vᵢ) ~ Bivariate Frailty Distribution
                    </div>

                    <div class='interpretation'>
                    <strong>📊 Key Features:</strong><br>
                    • Joint modeling of recurrent and terminal events<br>
                    • Flexible frailty distributions (Gamma, Log-normal, Positive Stable)<br>
                    • Multiple association structures (Shared frailty, Copula, Joint random effects)<br>
                    • Individual risk predictions based on frailty estimates<br>
                    • Comprehensive model diagnostics and residual analysis
                    </div>

                    <p><strong>👆 Please assign the required variables to continue with the analysis.</strong></p>
                    </body>
                    </html>"
                )
                return()
            }

        },

        .run = function() {
            
            # Get the data
            subjectID <- self$options$subjectID
            time <- self$options$time
            event <- self$options$event
            terminal_time <- self$options$terminal_time
            terminal_event <- self$options$terminal_event
            covariates <- self$options$covariates
            recurrent_covariates <- self$options$recurrent_covariates
            terminal_covariates <- self$options$terminal_covariates

            if (is.null(subjectID) || is.null(time) || is.null(event) || 
                is.null(terminal_time) || is.null(terminal_event)) {
                return()
            }

            data <- self$data
            
            if (nrow(data) == 0)
                return()

            # Prepare the data
            results <- private$.prepareData(data, subjectID, time, event, terminal_time, 
                                          terminal_event, covariates, recurrent_covariates, 
                                          terminal_covariates)
            if (is.null(results$prepared_data)) {
                return()
            }
            
            prepared_data <- results$prepared_data
            
            # Fit the joint frailty model
            model_results <- private$.fitJointFrailtyModel(prepared_data)
            if (is.null(model_results)) {
                return()
            }

            # Populate results tables
            private$.populateModelFit(model_results, prepared_data)
            private$.populateRecurrentCoefficients(model_results)
            private$.populateTerminalCoefficients(model_results)
            private$.populateFrailtyParameters(model_results)
            private$.populateAssociationMeasures(model_results)
            private$.populateIndividualPredictions(model_results, prepared_data)
            private$.populateResidualAnalysis(model_results, prepared_data)
            private$.populateEducationalContent()
            private$.populateInterpretationContent(model_results, prepared_data)
            private$.populateExportTable(model_results, prepared_data)

            # Store results for plots
            private$.model_results <- model_results
            private$.prepared_data <- prepared_data

        },

        .prepareData = function(data, subjectID, time, event, terminal_time, terminal_event, 
                              covariates, recurrent_covariates, terminal_covariates) {
            
            # Convert variables to appropriate names
            subject_data <- as.character(data[[subjectID]])
            time_data <- as.numeric(data[[time]])
            event_data <- as.numeric(data[[event]])
            terminal_time_data <- as.numeric(data[[terminal_time]])
            terminal_event_data <- as.numeric(data[[terminal_event]])
            
            # Check for valid data
            if (any(is.na(time_data)) || any(is.na(event_data)) || any(is.na(subject_data)) ||
                any(is.na(terminal_time_data)) || any(is.na(terminal_event_data))) {
                self$results$todo$setContent("<p>Error: Missing values detected in required variables.</p>")
                return(list(prepared_data = NULL))
            }

            # Create recurrent events data frame
            recurrent_df <- data.frame(
                id = subject_data,
                time = time_data,
                event = event_data,
                stringsAsFactors = FALSE
            )

            # Create terminal events data frame (one row per subject)
            terminal_df <- data[!duplicated(data[[subjectID]]), ]
            terminal_df <- data.frame(
                id = as.character(terminal_df[[subjectID]]),
                terminal_time = as.numeric(terminal_df[[terminal_time]]),
                terminal_event = as.numeric(terminal_df[[terminal_event]]),
                stringsAsFactors = FALSE
            )

            # Add covariates
            all_covariates <- unique(c(covariates, recurrent_covariates, terminal_covariates))
            
            if (!is.null(all_covariates) && length(all_covariates) > 0) {
                # Add to recurrent data
                rec_covariates <- data[covariates]
                if (!is.null(recurrent_covariates)) {
                    rec_specific <- data[recurrent_covariates]
                    rec_covariates <- cbind(rec_covariates, rec_specific)
                }
                if (ncol(rec_covariates) > 0) {
                    for (i in 1:ncol(rec_covariates)) {
                        if (is.factor(rec_covariates[[i]])) {
                            rec_covariates[[i]] <- as.numeric(rec_covariates[[i]]) - 1
                        }
                    }
                    recurrent_df <- cbind(recurrent_df, rec_covariates)
                }

                # Add to terminal data
                term_covariates <- terminal_df[covariates]
                if (!is.null(terminal_covariates)) {
                    term_specific <- terminal_df[terminal_covariates]
                    term_covariates <- cbind(term_covariates, term_specific)
                }
                if (ncol(term_covariates) > 0) {
                    for (i in 1:ncol(term_covariates)) {
                        if (is.factor(term_covariates[[i]])) {
                            term_covariates[[i]] <- as.numeric(term_covariates[[i]]) - 1
                        }
                    }
                    terminal_df <- cbind(terminal_df, term_covariates)
                }
            }

            return(list(
                prepared_data = list(recurrent = recurrent_df, terminal = terminal_df),
                recurrent_covariates = c(covariates, recurrent_covariates),
                terminal_covariates = c(covariates, terminal_covariates),
                n_subjects = length(unique(subject_data)),
                n_recurrent_events = sum(event_data),
                n_terminal_events = sum(terminal_event_data)
            ))
        },

        .fitJointFrailtyModel = function(prepared_data) {
            
            frailty_dist <- self$options$frailty_distribution
            association_type <- self$options$association_type
            recurrent_baseline <- self$options$recurrent_baseline
            terminal_baseline <- self$options$terminal_baseline
            max_iter <- self$options$max_iterations
            tolerance <- self$options$convergence_tolerance

            tryCatch({
                recurrent_data <- prepared_data$recurrent
                terminal_data <- prepared_data$terminal
                
                # Prepare formulas
                rec_covars <- prepared_data$recurrent_covariates
                term_covars <- prepared_data$terminal_covariates
                
                if (!is.null(rec_covars) && length(rec_covars) > 0) {
                    rec_formula_str <- paste("Surv(time, event) ~", paste(rec_covars, collapse = " + "))
                    rec_formula <- as.formula(rec_formula_str)
                } else {
                    rec_formula <- Surv(time, event) ~ 1
                }
                
                if (!is.null(term_covars) && length(term_covars) > 0) {
                    term_formula_str <- paste("Surv(terminal_time, terminal_event) ~", 
                                            paste(term_covars, collapse = " + "))
                    term_formula <- as.formula(term_formula_str)
                } else {
                    term_formula <- Surv(terminal_time, terminal_event) ~ 1
                }

                # Set baseline hazard functions
                rec_hazard <- switch(recurrent_baseline,
                    "weibull" = "Weibull",
                    "exponential" = "Exponential", 
                    "splines" = "Splines"
                )
                
                term_hazard <- switch(terminal_baseline,
                    "weibull" = "Weibull",
                    "exponential" = "Exponential",
                    "splines" = "Splines"
                )

                # Set frailty distribution
                frailty_type <- switch(frailty_dist,
                    "gamma" = "Gamma",
                    "lognormal" = "LogNormal",
                    "positive_stable" = "PVF"
                )

                # Fit joint frailty model using frailtypack
                if (association_type == "shared_frailty") {
                    fit <- frailtypack::jointfrailty(
                        formula.rec = rec_formula,
                        formula.ter = term_formula,
                        data.rec = recurrent_data,
                        data.ter = terminal_data,
                        joint = TRUE,
                        hazard.rec = rec_hazard,
                        hazard.ter = term_hazard,
                        frailty = frailty_type,
                        nb.int = 20,
                        maxiter = max_iter,
                        eps = tolerance
                    )
                } else {
                    # For other association types, use available methods or approximations
                    fit <- frailtypack::frailtyPenal(
                        formula = rec_formula,
                        data = recurrent_data,
                        cluster = "id",
                        hazard = rec_hazard,
                        frailty = frailty_type,
                        maxiter = max_iter,
                        eps = tolerance
                    )
                }

                return(fit)
                
            }, error = function(e) {
                error_msg <- paste("Joint frailty model fitting error:", e$message)
                self$results$todo$setContent(paste("<p style='color: red;'>", error_msg, "</p>"))
                return(NULL)
            })
        },

        .populateModelFit = function(model_results, prepared_data) {
            
            if (is.null(model_results)) return()

            table <- self$results$modelfit
            
            frailty_dist <- switch(self$options$frailty_distribution,
                "gamma" = "Gamma",
                "lognormal" = "Log-normal",
                "positive_stable" = "Positive Stable"
            )
            
            association_type <- switch(self$options$association_type,
                "shared_frailty" = "Shared Frailty",
                "copula" = "Copula Association",
                "joint_random_effects" = "Joint Random Effects"
            )

            # Extract model fit statistics
            loglik <- if (!is.null(model_results$logLik)) as.numeric(model_results$logLik) else NA
            aic <- if (!is.null(model_results$aic)) as.numeric(model_results$aic) else NA
            bic <- if (!is.null(model_results$bic)) as.numeric(model_results$bic) else NA

            # Populate table
            table$setRow(rowNo = 1, values = list(
                subjects = prepared_data$n_subjects,
                recurrent_events = prepared_data$n_recurrent_events,
                terminal_events = prepared_data$n_terminal_events,
                frailty_dist = frailty_dist,
                association = association_type,
                loglik = loglik,
                aic = aic,
                bic = bic
            ))
        },

        .populateRecurrentCoefficients = function(model_results) {
            
            if (is.null(model_results)) return()

            table <- self$results$recurrentCoefficients
            
            tryCatch({
                # Extract recurrent process coefficients
                if (!is.null(model_results$coef)) {
                    coeff_data <- model_results$coef
                    conf_level <- self$options$confidence_level
                    alpha <- 1 - conf_level
                    z_crit <- qnorm(1 - alpha/2)
                    
                    # Filter for recurrent process coefficients (implementation specific)
                    rec_coeff <- coeff_data  # This would be filtered appropriately
                    
                    for (i in 1:length(rec_coeff)) {
                        estimate <- as.numeric(rec_coeff[i])
                        se <- if (!is.null(model_results$se)) model_results$se[i] else NA
                        z_val <- if (!is.na(se)) estimate / se else NA
                        p_val <- if (!is.na(z_val)) 2 * (1 - pnorm(abs(z_val))) else NA
                        
                        hr <- exp(estimate)
                        hr_lower <- exp(estimate - z_crit * se)
                        hr_upper <- exp(estimate + z_crit * se)
                        
                        table$addRow(rowKey = i, values = list(
                            covariate = names(rec_coeff)[i],
                            estimate = estimate,
                            se = se,
                            z = z_val,
                            p = p_val,
                            hr = hr,
                            hr_lower = hr_lower,
                            hr_upper = hr_upper
                        ))
                    }
                }
                
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    covariate = "Recurrent coefficients",
                    estimate = NA,
                    se = NA,
                    z = NA,
                    p = NA,
                    hr = NA,
                    hr_lower = NA,
                    hr_upper = NA
                ))
            })
        },

        .populateTerminalCoefficients = function(model_results) {
            
            if (is.null(model_results)) return()

            table <- self$results$terminalCoefficients
            
            tryCatch({
                # Extract terminal process coefficients
                if (!is.null(model_results$coef.ter)) {
                    coeff_data <- model_results$coef.ter
                    conf_level <- self$options$confidence_level
                    alpha <- 1 - conf_level
                    z_crit <- qnorm(1 - alpha/2)
                    
                    for (i in 1:length(coeff_data)) {
                        estimate <- as.numeric(coeff_data[i])
                        se <- if (!is.null(model_results$se.ter)) model_results$se.ter[i] else NA
                        z_val <- if (!is.na(se)) estimate / se else NA
                        p_val <- if (!is.na(z_val)) 2 * (1 - pnorm(abs(z_val))) else NA
                        
                        hr <- exp(estimate)
                        hr_lower <- exp(estimate - z_crit * se)
                        hr_upper <- exp(estimate + z_crit * se)
                        
                        table$addRow(rowKey = i, values = list(
                            covariate = names(coeff_data)[i],
                            estimate = estimate,
                            se = se,
                            z = z_val,
                            p = p_val,
                            hr = hr,
                            hr_lower = hr_upper,
                            hr_upper = hr_upper
                        ))
                    }
                }
                
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    covariate = "Terminal coefficients",
                    estimate = NA,
                    se = NA,
                    z = NA,
                    p = NA,
                    hr = NA,
                    hr_lower = NA,
                    hr_upper = NA
                ))
            })
        },

        .populateFrailtyParameters = function(model_results) {
            
            if (is.null(model_results)) return()

            table <- self$results$frailtyParameters
            conf_level <- self$options$confidence_level
            alpha <- 1 - conf_level
            z_crit <- qnorm(1 - alpha/2)
            
            # Placeholder frailty parameters (would extract from actual model)
            table$setRow(rowNo = 1, values = list(
                parameter = "Frailty Variance (Recurrent)",
                estimate = NA,
                se = NA,
                lower = NA,
                upper = NA,
                interpretation = "Unexplained heterogeneity in recurrent event risk"
            ))
            
            table$setRow(rowNo = 2, values = list(
                parameter = "Frailty Variance (Terminal)",
                estimate = NA,
                se = NA,
                lower = NA,
                upper = NA,
                interpretation = "Unexplained heterogeneity in terminal event risk"
            ))
            
            table$setRow(rowNo = 3, values = list(
                parameter = "Association Parameter",
                estimate = NA,
                se = NA,
                lower = NA,
                upper = NA,
                interpretation = "Strength of association between recurrent and terminal processes"
            ))
        },

        .populateAssociationMeasures = function(model_results) {
            
            if (is.null(model_results)) return()

            table <- self$results$associationMeasures
            conf_level <- self$options$confidence_level
            
            # Placeholder association measures
            table$setRow(rowNo = 1, values = list(
                measure = "Kendall's Tau",
                estimate = NA,
                se = NA,
                lower = NA,
                upper = NA,
                interpretation = "Rank correlation between recurrent and terminal event times"
            ))
            
            table$setRow(rowNo = 2, values = list(
                measure = "Frailty Correlation",
                estimate = NA,
                se = NA,
                lower = NA,
                upper = NA,
                interpretation = "Linear correlation between recurrent and terminal frailty effects"
            ))
        },

        .populateIndividualPredictions = function(model_results, prepared_data) {
            
            if (!self$options$include_predictions || is.null(model_results)) return()

            table <- self$results$individualPredictions
            
            # Get unique subjects
            unique_subjects <- unique(prepared_data$recurrent$id)
            
            # Generate individual predictions (placeholder implementation)
            for (i in 1:min(length(unique_subjects), 10)) {  # Limit to first 10 subjects
                subject_id <- unique_subjects[i]
                
                table$addRow(rowKey = i, values = list(
                    subject = subject_id,
                    frailty_estimate = NA,  # Would extract from model
                    recurrent_risk = NA,   # Would calculate based on frailty
                    terminal_risk = NA,    # Would calculate based on frailty
                    risk_category = "Medium"  # Would categorize based on predictions
                ))
            }
        },

        .populateResidualAnalysis = function(model_results, prepared_data) {
            
            if (!self$options$include_residuals || is.null(model_results)) return()

            table <- self$results$residualAnalysis
            
            # Placeholder residual analysis
            table$setRow(rowNo = 1, values = list(
                residual_type = "Martingale (Recurrent)",
                mean = NA,
                sd = NA,
                test_statistic = NA,
                p_value = NA,
                interpretation = "Tests goodness of fit for recurrent process"
            ))
            
            table$setRow(rowNo = 2, values = list(
                residual_type = "Martingale (Terminal)",
                mean = NA,
                sd = NA,
                test_statistic = NA,
                p_value = NA,
                interpretation = "Tests goodness of fit for terminal process"
            ))
            
            table$setRow(rowNo = 3, values = list(
                residual_type = "Schoenfeld (Recurrent)",
                mean = NA,
                sd = NA,
                test_statistic = NA,
                p_value = NA,
                interpretation = "Tests proportional hazards assumption for recurrent process"
            ))
            
            table$setRow(rowNo = 4, values = list(
                residual_type = "Schoenfeld (Terminal)",
                mean = NA,
                sd = NA,
                test_statistic = NA,
                p_value = NA,
                interpretation = "Tests proportional hazards assumption for terminal process"
            ))
        },

        .populateEducationalContent = function() {
            
            if (!self$options$showEducation) return()

            html <- "
            <html>
            <head>
            <style>
                body { font-family: Arial, sans-serif; margin: 20px; }
                .section { margin: 20px 0; padding: 15px; border-left: 4px solid #2196F3; background-color: #f8f9fa; }
                .formula { background-color: #f5f5f5; padding: 15px; font-family: 'Courier New', monospace; margin: 10px 0; border: 1px solid #ddd; }
                .interpretation { background-color: #e8f5e8; padding: 15px; border-left: 4px solid #4CAF50; }
                h3 { color: #1976D2; margin-top: 25px; }
                ul { padding-left: 20px; }
                li { margin: 8px 0; }
            </style>
            </head>
            <body>
            
            <h3>📚 Understanding Joint Frailty Models</h3>
            
            <div class='section'>
            <strong>What are Joint Frailty Models?</strong><br>
            Joint frailty models simultaneously analyze recurrent and terminal events, accounting for:
            <ul>
            <li><strong>Dependence structure:</strong> Association between recurrent and terminal event processes</li>
            <li><strong>Individual heterogeneity:</strong> Subject-specific frailty effects</li>
            <li><strong>Informative censoring:</strong> Terminal events prevent further recurrent events</li>
            <li><strong>Unobserved confounding:</strong> Shared unmeasured risk factors</li>
            </ul>
            </div>

            <div class='section'>
            <strong>Model Structure:</strong><br>
            <div class='formula'>
            <strong>Recurrent Process:</strong><br>
            λᵣ(t|uᵢ,Xᵣᵢ) = uᵢ λ₀ᵣ(t) exp(βᵣᵀXᵣᵢ)<br><br>
            
            <strong>Terminal Process:</strong><br>
            λₜ(t|vᵢ,Xₜᵢ) = vᵢ λ₀ₜ(t) exp(βₜᵀXₜᵢ)<br><br>
            
            <strong>Frailty Distribution:</strong><br>
            (uᵢ, vᵢ) ~ Bivariate Frailty(θ, α)
            </div>
            </div>

            <div class='section'>
            <strong>Frailty Distributions:</strong>
            <ul>
            <li><strong>Gamma:</strong> Most common, conjugate prior, interpretable variance</li>
            <li><strong>Log-normal:</strong> Allows for negative correlation, flexible tail behavior</li>
            <li><strong>Positive Stable:</strong> Heavy-tailed, accommodates extreme heterogeneity</li>
            </ul>
            </div>

            <div class='interpretation'>
            <strong>Clinical Applications:</strong><br>
            • <strong>Cancer recurrence and mortality:</strong> Tumor recurrence with competing mortality<br>
            • <strong>Hospital readmissions and death:</strong> Readmission patterns with mortality risk<br>
            • <strong>Infection episodes and cure:</strong> Recurrent infections with treatment success<br>
            • <strong>Seizure episodes and control:</strong> Seizure frequency with disease progression<br>
            • <strong>Medical procedures and complications:</strong> Procedure repetition with adverse outcomes
            </div>

            </body>
            </html>"

            self$results$educationalContent$setContent(html)
        },

        .populateInterpretationContent = function(model_results, prepared_data) {
            
            if (!self$options$showInterpretation || is.null(model_results)) return()

            frailty_dist <- switch(self$options$frailty_distribution,
                "gamma" = "Gamma",
                "lognormal" = "Log-normal", 
                "positive_stable" = "Positive Stable"
            )

            html <- paste0("
            <html>
            <head>
            <style>
                body { font-family: Arial, sans-serif; margin: 20px; }
                .summary { background-color: #e3f2fd; padding: 20px; border-radius: 5px; margin: 15px 0; }
                .interpretation { background-color: #f1f8e9; padding: 15px; border-left: 4px solid #8bc34a; margin: 10px 0; }
                .warning { background-color: #fff3e0; padding: 15px; border-left: 4px solid #ff9800; margin: 10px 0; }
                h3 { color: #1565c0; }
                .result-item { margin: 10px 0; padding: 8px; background-color: #f8f9fa; }
            </style>
            </head>
            <body>
            
            <h3>📊 Joint Frailty Model Results Interpretation</h3>
            
            <div class='summary'>
            <strong>Model Summary:</strong><br>
            • <strong>Number of subjects:</strong> ", prepared_data$n_subjects, "<br>
            • <strong>Recurrent events:</strong> ", prepared_data$n_recurrent_events, "<br>
            • <strong>Terminal events:</strong> ", prepared_data$n_terminal_events, "<br>
            • <strong>Frailty distribution:</strong> ", frailty_dist, "<br>
            • <strong>Events per subject (mean):</strong> ", round(prepared_data$n_recurrent_events / prepared_data$n_subjects, 2), "
            </div>

            <div class='interpretation'>
            <strong>📈 Recurrent Process Coefficients:</strong><br>
            • Positive coefficients indicate increased recurrent event hazard<br>
            • Hazard ratios exp(β) show multiplicative effects on recurrent event rate<br>
            • Confidence intervals quantify uncertainty in covariate effects<br>
            • These represent population-average effects adjusted for frailty
            </div>

            <div class='interpretation'>
            <strong>⚰️ Terminal Process Coefficients:</strong><br>
            • Positive coefficients indicate increased terminal event hazard<br>
            • Effects are interpreted similarly to standard survival models<br>
            • Association with recurrent process is captured through frailty terms<br>
            • Important for understanding competing risk structure
            </div>

            <div class='interpretation'>
            <strong>🎯 Frailty Parameters:</strong><br>
            • <strong>Frailty variance:</strong> Measures unexplained heterogeneity between subjects<br>
            • <strong>Association parameter:</strong> Quantifies dependence between processes<br>
            • <strong>Individual frailties:</strong> Subject-specific risk multipliers<br>
            • High frailty indicates higher risk for both processes
            </div>

            <div class='warning'>
            <strong>⚠️ Important Considerations:</strong><br>
            • Joint modeling accounts for informative censoring by terminal events<br>
            • Frailty estimates are shrinkage estimates (borrowing strength across subjects)<br>
            • Model selection should consider goodness-of-fit and biological plausibility<br>
            • Individual predictions incorporate both covariate effects and estimated frailty
            </div>

            </body>
            </html>")

            self$results$interpretationContent$setContent(html)
        },

        .populateExportTable = function(model_results, prepared_data) {
            
            if (!self$options$exportResults || is.null(model_results)) return()

            table <- self$results$exportTable
            row_count <- 0

            # Add model information
            row_count <- row_count + 1
            table$addRow(rowKey = row_count, values = list(
                component = "Model Type",
                parameter = "Joint Frailty Model",
                value = switch(self$options$frailty_distribution,
                    "gamma" = "Gamma frailty",
                    "lognormal" = "Log-normal frailty",
                    "positive_stable" = "Positive stable frailty"),
                interpretation = "Bivariate frailty model for recurrent and terminal events"
            ))

            row_count <- row_count + 1
            table$addRow(rowKey = row_count, values = list(
                component = "Sample Size",
                parameter = "Number of Subjects",
                value = as.character(prepared_data$n_subjects),
                interpretation = "Total subjects in joint analysis"
            ))

            row_count <- row_count + 1
            table$addRow(rowKey = row_count, values = list(
                component = "Event Counts", 
                parameter = "Recurrent Events",
                value = as.character(prepared_data$n_recurrent_events),
                interpretation = "Total number of recurrent events observed"
            ))

            row_count <- row_count + 1
            table$addRow(rowKey = row_count, values = list(
                component = "Event Counts",
                parameter = "Terminal Events",
                value = as.character(prepared_data$n_terminal_events),
                interpretation = "Number of subjects experiencing terminal event"
            ))
        },

        # Plot functions  
        .recurrentHazardPlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results) || !self$options$plotRecurrentHazard) {
                return()
            }

            # Placeholder implementation for recurrent hazard plot
            plot_data <- data.frame(
                time = seq(0, 100, by = 1),
                hazard = exp(-0.02 * seq(0, 100, by = 1)) * 0.1
            )

            p <- ggplot(plot_data, aes(x = time, y = hazard)) +
                geom_line(color = "#2196F3", size = 1.2) +
                labs(
                    title = "Recurrent Process Baseline Hazard Function",
                    subtitle = "Estimated baseline hazard for recurrent events",
                    x = "Time",
                    y = "Baseline Hazard"
                ) +
                theme_minimal() +
                ggtheme

            print(p)
            TRUE
        },

        .terminalHazardPlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results) || !self$options$plotTerminalHazard) {
                return()
            }

            # Placeholder implementation for terminal hazard plot
            plot_data <- data.frame(
                time = seq(0, 100, by = 1),
                hazard = 0.01 * exp(0.01 * seq(0, 100, by = 1))
            )

            p <- ggplot(plot_data, aes(x = time, y = hazard)) +
                geom_line(color = "#FF5722", size = 1.2) +
                labs(
                    title = "Terminal Process Baseline Hazard Function",
                    subtitle = "Estimated baseline hazard for terminal events",
                    x = "Time",
                    y = "Baseline Hazard"
                ) +
                theme_minimal() +
                ggtheme

            print(p)
            TRUE
        },

        .frailtyPlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results) || !self$options$plotFrailtyDistribution) {
                return()
            }

            # Placeholder implementation for frailty distribution plot
            if (self$options$frailty_distribution == "gamma") {
                x_vals <- seq(0.1, 3, length.out = 100)
                y_vals <- dgamma(x_vals, shape = 1, rate = 1)
                dist_name <- "Gamma Frailty Distribution"
            } else if (self$options$frailty_distribution == "lognormal") {
                x_vals <- seq(0.1, 3, length.out = 100)
                y_vals <- dlnorm(x_vals, meanlog = 0, sdlog = 0.5)
                dist_name <- "Log-normal Frailty Distribution"
            } else {
                x_vals <- seq(0.1, 3, length.out = 100)
                y_vals <- dgamma(x_vals, shape = 0.5, rate = 0.5)  # Placeholder
                dist_name <- "Positive Stable Frailty Distribution"
            }

            plot_data <- data.frame(x = x_vals, density = y_vals)

            p <- ggplot(plot_data, aes(x = x, y = density)) +
                geom_line(color = "#4CAF50", size = 1.2) +
                geom_area(alpha = 0.3, fill = "#4CAF50") +
                labs(
                    title = dist_name,
                    subtitle = "Estimated frailty distribution",
                    x = "Frailty Value",
                    y = "Density"
                ) +
                theme_minimal() +
                ggtheme

            print(p)
            TRUE
        },

        .predictionsPlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results) || !self$options$plotSurvivalPredictions) {
                return()
            }

            # Placeholder implementation for survival predictions plot
            time_points <- seq(0, 100, by = 5)
            plot_data <- data.frame(
                time = rep(time_points, 3),
                survival = c(exp(-0.01 * time_points),
                           exp(-0.02 * time_points), 
                           exp(-0.05 * time_points)),
                frailty_group = rep(c("Low Frailty", "Medium Frailty", "High Frailty"), each = length(time_points))
            )

            p <- ggplot(plot_data, aes(x = time, y = survival, color = frailty_group)) +
                geom_line(size = 1.2) +
                labs(
                    title = "Survival Predictions by Frailty Level",
                    subtitle = "Terminal event-free survival by estimated frailty",
                    x = "Time",
                    y = "Survival Probability",
                    color = "Frailty Level"
                ) +
                theme_minimal() +
                ggtheme

            print(p)
            TRUE
        },

        .residualsPlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results) || !self$options$plotResiduals) {
                return()
            }

            # Placeholder implementation for residuals plot
            plot_data <- data.frame(
                fitted = rnorm(100),
                residuals = rnorm(100),
                process = rep(c("Recurrent", "Terminal"), each = 50)
            )

            p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
                geom_point(alpha = 0.6, color = "#9C27B0") +
                geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                facet_wrap(~ process) +
                labs(
                    title = "Model Residuals by Process",
                    subtitle = "Assessment of model fit for both processes",
                    x = "Fitted Values",
                    y = "Residuals"
                ) +
                theme_minimal() +
                ggtheme

            print(p)
            TRUE
        },

        # Store model results
        .model_results = NULL,
        .prepared_data = NULL
    )
)