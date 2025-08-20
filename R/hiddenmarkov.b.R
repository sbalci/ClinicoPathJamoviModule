hiddenmarkovClass <- R6::R6Class(
    "hiddenmarkovClass",
    inherit = hiddenmarkovBase,
    private = list(
        .model = NULL,
        .data = NULL,
        
        .init = function() {
            if (is.null(self$options$subject) || 
                is.null(self$options$state) || 
                is.null(self$options$time)) {
                self$results$modelSummary$setNote(
                    "note", 
                    "Subject ID, Observed State, and Time variables are required"
                )
                return()
            }
        },
        
        .run = function() {
            # Get data
            if (!private$.getData())
                return()
            
            # Check if msm package is available
            if (!requireNamespace("msm", quietly = TRUE)) {
                self$results$modelSummary$setNote(
                    "note",
                    "The 'msm' package is required but not installed. Please install it using: install.packages('msm')"
                )
                return()
            }
            
            # Fit model
            if (!private$.fitModel())
                return()
            
            # Populate results
            private$.populateModelSummary()
            private$.populateTransitionMatrix()
            
            if (self$options$showTransitions)
                private$.populateTransitionProbabilities()
            
            if (self$options$showPrevalence)
                private$.populateStatePrevalence()
            
            if (self$options$showResiduals)
                private$.populateGoodnessOfFit()
            
            if (self$options$showViterbi)
                private$.populateViterbiStates()
                
            # Educational content
            if (self$options$showEducational)
                private$.populateEducational()
        },
        
        .getData = function() {
            data <- self$data
            
            if (nrow(data) == 0) {
                self$results$modelSummary$setNote("note", "Data contains no observations")
                return(FALSE)
            }
            
            # Get variables
            subject <- jmvcore::toNumeric(data[[self$options$subject]])
            state <- jmvcore::toNumeric(data[[self$options$state]])
            time <- jmvcore::toNumeric(data[[self$options$time]])
            
            # Remove missing values
            complete_cases <- complete.cases(subject, state, time)
            if (sum(complete_cases) == 0) {
                self$results$modelSummary$setNote("note", "No complete cases available")
                return(FALSE)
            }
            
            private$.data <- list(
                subject = subject[complete_cases],
                state = state[complete_cases], 
                time = time[complete_cases]
            )
            
            # Add covariates if specified
            if (!is.null(self$options$covs) && length(self$options$covs) > 0) {
                cov_data <- data[self$options$covs]
                private$.data$covs <- cov_data[complete_cases, , drop = FALSE]
            }
            
            # Add hidden state covariates if specified
            if (!is.null(self$options$hiddenCovs) && length(self$options$hiddenCovs) > 0) {
                hidden_cov_data <- data[self$options$hiddenCovs]
                private$.data$hiddenCovs <- hidden_cov_data[complete_cases, , drop = FALSE]
            }
            
            # Add censoring if specified
            if (!is.null(self$options$censor)) {
                censor <- jmvcore::toNumeric(data[[self$options$censor]])
                private$.data$censor <- censor[complete_cases]
            }
            
            return(TRUE)
        },
        
        .fitModel = function() {
            tryCatch({
                # Prepare data for msm
                data_for_msm <- data.frame(
                    subject = private$.data$subject,
                    state = private$.data$state,
                    time = private$.data$time
                )
                
                # Add covariates to data frame
                if (!is.null(private$.data$covs)) {
                    data_for_msm <- cbind(data_for_msm, private$.data$covs)
                }
                
                # Build Q matrix structure
                nstates <- self$options$nstates
                qmatrix <- private$.buildQMatrix(nstates)
                
                # Build covariate formula
                cov_formula <- NULL
                if (!is.null(private$.data$covs)) {
                    cov_names <- names(private$.data$covs)
                    cov_formula <- as.formula(paste("~", paste(cov_names, collapse = " + ")))
                }
                
                # Build emission matrix if misclassification is specified
                ematrix <- NULL
                if (self$options$ematrix != "identity") {
                    ematrix <- private$.buildEMatrix(nstates)
                }
                
                # Handle observation type
                obstype <- switch(self$options$obstype,
                    "exact" = 1,
                    "panel" = 2,  
                    "mixed" = 3
                )
                
                # Parse initial probabilities if provided
                initprobs <- NULL
                if (self$options$initprobs != "") {
                    initprobs <- as.numeric(strsplit(self$options$initprobs, ",")[[1]])
                }
                
                # Parse fixed parameters if provided
                fixedpar <- NULL
                if (self$options$fixedpar != "") {
                    fixedpar <- as.numeric(strsplit(self$options$fixedpar, ",")[[1]])
                }
                
                # Fit the Hidden Markov model
                private$.model <- msm::msm(
                    formula = state ~ time,
                    subject = subject,
                    data = data_for_msm,
                    qmatrix = qmatrix,
                    ematrix = ematrix,
                    covariates = cov_formula,
                    obstype = obstype,
                    method = self$options$method,
                    fixedpar = fixedpar,
                    initprobs = initprobs,
                    control = list(fnscale = 1000, maxit = 10000)
                )
                
                return(TRUE)
                
            }, error = function(e) {
                self$results$modelSummary$setNote("note", 
                    paste("Error fitting model:", e$message))
                return(FALSE)
            })
        },
        
        .buildQMatrix = function(nstates) {
            # Build Q matrix structure based on user selection
            qmatrix <- matrix(0, nstates, nstates)
            
            if (self$options$qmatrix == "irreversible") {
                # Unidirectional transitions (illness progression)
                for (i in 1:(nstates-1)) {
                    for (j in (i+1):nstates) {
                        qmatrix[i, j] <- 1  # Allow transitions to higher states
                    }
                }
            } else if (self$options$qmatrix == "reversible") {
                # Bidirectional transitions
                for (i in 1:nstates) {
                    for (j in 1:nstates) {
                        if (i != j) {
                            qmatrix[i, j] <- 1
                        }
                    }
                }
            } else {
                # Custom structure - for now, same as irreversible
                # In a full implementation, this would allow user specification
                for (i in 1:(nstates-1)) {
                    for (j in (i+1):nstates) {
                        qmatrix[i, j] <- 1
                    }
                }
            }
            
            return(qmatrix)
        },
        
        .buildEMatrix = function(nstates) {
            # Build emission matrix for misclassification
            ematrix <- matrix(0.1, nstates, nstates)
            diag(ematrix) <- 0.9  # High probability of correct classification
            
            if (self$options$ematrix == "symmetric") {
                # Symmetric misclassification
                for (i in 1:nstates) {
                    for (j in 1:nstates) {
                        if (i != j) {
                            ematrix[i, j] <- 0.1 / (nstates - 1)
                        }
                    }
                }
            }
            
            return(ematrix)
        },
        
        .populateModelSummary = function() {
            if (is.null(private$.model))
                return()
            
            summary_model <- summary(private$.model)
            
            # Get parameter estimates with confidence intervals
            estimates <- summary_model$parameters
            
            table <- self$results$modelSummary
            table$deleteRows()
            
            for (i in 1:nrow(estimates)) {
                row <- list(
                    parameter = rownames(estimates)[i],
                    estimate = estimates[i, "estimate"],
                    se = estimates[i, "se"],
                    lower = estimates[i, "L95%"],
                    upper = estimates[i, "U95%"],
                    pvalue = estimates[i, "p"]
                )
                table$addRow(rowKey = i, values = row)
            }
        },
        
        .populateTransitionMatrix = function() {
            if (is.null(private$.model))
                return()
            
            # Get transition intensity matrix
            qmatrix <- msm::qmatrix.msm(private$.model, ci = "normal")
            
            table <- self$results$transitionMatrix
            table$deleteRows()
            
            nstates <- self$options$nstates
            row_idx <- 1
            
            for (i in 1:nstates) {
                for (j in 1:nstates) {
                    if (i != j && qmatrix[i, j] > 0) {
                        row <- list(
                            from_state = paste("State", i),
                            to_state = paste("State", j),
                            intensity = qmatrix[i, j],
                            se = attr(qmatrix, "se")[i, j],
                            lower = attr(qmatrix, "L")[i, j],
                            upper = attr(qmatrix, "U")[i, j]
                        )
                        table$addRow(rowKey = row_idx, values = row)
                        row_idx <- row_idx + 1
                    }
                }
            }
        },
        
        .populateTransitionProbabilities = function() {
            if (is.null(private$.model))
                return()
            
            # Parse prediction times
            times_str <- trimws(self$options$predTimes)
            if (times_str == "")
                return()
            
            times <- as.numeric(unlist(strsplit(times_str, ",")))
            times <- times[!is.na(times)]
            
            if (length(times) == 0)
                return()
            
            table <- self$results$transitionProbs
            table$deleteRows()
            
            nstates <- self$options$nstates
            row_idx <- 1
            
            for (time in times) {
                tryCatch({
                    pmatrix <- msm::pmatrix.msm(private$.model, t = time, ci = "normal")
                    
                    for (i in 1:nstates) {
                        for (j in 1:nstates) {
                            row <- list(
                                time = time,
                                from_state = paste("State", i),
                                to_state = paste("State", j),
                                probability = pmatrix[i, j],
                                lower = attr(pmatrix, "L")[i, j],
                                upper = attr(pmatrix, "U")[i, j]
                            )
                            table$addRow(rowKey = row_idx, values = row)
                            row_idx <- row_idx + 1
                        }
                    }
                }, error = function(e) {
                    # Skip problematic time points
                })
            }
        },
        
        .populateStatePrevalence = function() {
            if (is.null(private$.model))
                return()
            
            # Parse prediction times
            times_str <- trimws(self$options$predTimes)
            if (times_str == "")
                return()
            
            times <- as.numeric(unlist(strsplit(times_str, ",")))
            times <- times[!is.na(times)]
            
            if (length(times) == 0)
                return()
            
            table <- self$results$statePrevalence
            table$deleteRows()
            
            nstates <- self$options$nstates
            row_idx <- 1
            
            for (time in times) {
                tryCatch({
                    prev <- msm::prevalence.msm(private$.model, times = time, ci = "normal")
                    
                    for (i in 1:nstates) {
                        row <- list(
                            time = time,
                            state = paste("State", i),
                            prevalence = prev[1, i],
                            se = attr(prev, "se")[1, i],
                            lower = attr(prev, "L")[1, i],
                            upper = attr(prev, "U")[1, i]
                        )
                        table$addRow(rowKey = row_idx, values = row)
                        row_idx <- row_idx + 1
                    }
                }, error = function(e) {
                    # Skip problematic time points
                })
            }
        },
        
        .populateGoodnessOfFit = function() {
            if (is.null(private$.model))
                return()
            
            table <- self$results$goodnessOfFit
            table$deleteRows()
            
            # Log-likelihood
            loglik <- private$.model$minus2loglik / -2
            aic <- private$.model$AIC
            bic <- private$.model$BIC
            n_params <- length(private$.model$estimates)
            
            rows <- list(
                list(statistic = "Log-likelihood", value = loglik, df = "", pvalue = ""),
                list(statistic = "AIC", value = aic, df = "", pvalue = ""),
                list(statistic = "BIC", value = bic, df = "", pvalue = ""),
                list(statistic = "Parameters", value = n_params, df = "", pvalue = "")
            )
            
            for (i in seq_along(rows)) {
                table$addRow(rowKey = i, values = rows[[i]])
            }
        },
        
        .populateViterbiStates = function() {
            if (is.null(private$.model))
                return()
            
            tryCatch({
                # Get Viterbi states (most likely hidden state sequence)
                viterbi <- msm::viterbi.msm(private$.model)
                
                table <- self$results$viterbiStates
                table$deleteRows()
                
                data_df <- data.frame(
                    subject = private$.data$subject,
                    time = private$.data$time,
                    observed_state = private$.data$state,
                    viterbi_state = viterbi$viterbi
                )
                
                # Show only first 100 rows to avoid overwhelming output
                max_rows <- min(100, nrow(data_df))
                
                for (i in 1:max_rows) {
                    row <- list(
                        subject = as.character(data_df$subject[i]),
                        time = data_df$time[i],
                        observed_state = paste("State", data_df$observed_state[i]),
                        viterbi_state = paste("State", data_df$viterbi_state[i]),
                        probability = 1.0  # Viterbi gives most likely sequence
                    )
                    table$addRow(rowKey = i, values = row)
                }
                
                if (nrow(data_df) > 100) {
                    table$setNote("note", 
                        paste("Showing first 100 rows of", nrow(data_df), "total observations"))
                }
                
            }, error = function(e) {
                table$setNote("note", paste("Error computing Viterbi states:", e$message))
            })
        },
        
        .populateEducational = function() {
            html <- self$results$educationalText
            
            str <- paste0(
                '<div style="font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto;">',
                
                '<h2 style="color: #2E86C1; border-bottom: 2px solid #2E86C1; padding-bottom: 10px;">',
                'Hidden Markov Models for Survival Analysis</h2>',
                
                '<div style="background-color: #F8F9FA; padding: 15px; border-left: 4px solid #2E86C1; margin: 20px 0;">',
                '<h3 style="color: #1B4F72; margin-top: 0;">Overview</h3>',
                '<p><strong>Hidden Markov Models (HMMs)</strong> are particularly useful when the true disease states ',
                'are not directly observable but must be inferred from noisy or imperfect measurements. Unlike standard ',
                'multi-state models, HMMs account for <em>misclassification</em> in state observations.</p>',
                '</div>',
                
                '<h3 style="color: #1B4F72;">Key Features</h3>',
                '<ul style="line-height: 1.8;">',
                '<li><strong>Hidden States:</strong> True underlying disease progression states</li>',
                '<li><strong>Observed States:</strong> Noisy measurements or clinical assessments</li>',
                '<li><strong>Emission Matrix:</strong> Probabilities of observing each state given the true state</li>',
                '<li><strong>Transition Intensities:</strong> Rates of moving between hidden states</li>',
                '</ul>',
                
                '<h3 style="color: #1B4F72;">Model Components</h3>',
                '<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 20px 0;">',
                '<div style="background-color: #EBF5FB; padding: 15px; border-radius: 5px;">',
                '<h4 style="color: #2E86C1; margin-top: 0;">Transition Model</h4>',
                '<p>Models how patients move between hidden disease states over time using continuous-time Markov processes.</p>',
                '</div>',
                '<div style="background-color: #EAFAF1; padding: 15px; border-radius: 5px;">',
                '<h4 style="color: #27AE60; margin-top: 0;">Observation Model</h4>',
                '<p>Models the relationship between true hidden states and observed measurements or clinical assessments.</p>',
                '</div>',
                '</div>',
                
                '<h3 style="color: #1B4F72;">Clinical Applications</h3>',
                '<ul style="line-height: 1.8;">',
                '<li><strong>Disease Monitoring:</strong> When biomarker measurements are noisy</li>',
                '<li><strong>Cognitive Decline:</strong> True cognitive state vs. test performance</li>',
                '<li><strong>Cancer Progression:</strong> True tumor burden vs. imaging assessments</li>',
                '<li><strong>Psychiatric Disorders:</strong> True symptom severity vs. clinical ratings</li>',
                '</ul>',
                
                '<h3 style="color: #1B4F72;">Interpretation Guide</h3>',
                '<div style="background-color: #FEF9E7; padding: 15px; border-left: 4px solid #F39C12; margin: 20px 0;">',
                '<h4 style="color: #D68910; margin-top: 0;">Transition Intensities</h4>',
                '<p>Higher values indicate faster transitions between states. Units are in events per time unit.</p>',
                '</div>',
                
                '<div style="background-color: #FDEDEC; padding: 15px; border-left: 4px solid #E74C3C; margin: 20px 0;">',
                '<h4 style="color: #C0392B; margin-top: 0;">Misclassification Probabilities</h4>',
                '<p>Diagonal elements should be high (accurate classification). Off-diagonal elements represent ',
                'misclassification rates.</p>',
                '</div>',
                
                '<div style="background-color: #F4ECF7; padding: 15px; border-left: 4px solid #8E44AD; margin: 20px 0;">',
                '<h4 style="color: #7D3C98; margin-top: 0;">State Prevalence</h4>',
                '<p>Expected proportion of patients in each hidden state at specified time points.</p>',
                '</div>',
                
                '<h3 style="color: #1B4F72;">Statistical Considerations</h3>',
                '<ul style="line-height: 1.8;">',
                '<li><strong>Identifiability:</strong> Ensure sufficient data to distinguish between states</li>',
                '<li><strong>Model Selection:</strong> Use AIC/BIC to choose optimal number of states</li>',
                '<li><strong>Convergence:</strong> Check that optimization converged successfully</li>',
                '<li><strong>Residuals:</strong> Examine model fit using standardized residuals</li>',
                '</ul>',
                
                '<div style="background-color: #E8F8F5; padding: 15px; border-left: 4px solid #17A2B8; margin: 20px 0;">',
                '<h4 style="color: #138496; margin-top: 0;">R Package Reference</h4>',
                '<p>This analysis uses the <code>msm</code> package. For more details, see: ',
                'Jackson, C.H. (2011). Multi-State Models for Panel Data: The msm Package for R. ',
                'Journal of Statistical Software, 38(8), 1-29.</p>',
                '</div>',
                
                '</div>'
            )
            
            html$setContent(str)
        },
        
        .transitionPlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.model))
                return()
            
            if (!self$options$plotTransitions)
                return()
            
            # Create transition intensity plot over time
            tryCatch({
                # Get time range from data
                time_range <- range(private$.data$time)
                time_seq <- seq(time_range[1], time_range[2], length.out = 50)
                
                # Extract transition intensities
                qmatrix <- msm::qmatrix.msm(private$.model)
                nstates <- self$options$nstates
                
                plot_data <- data.frame()
                for (i in 1:nstates) {
                    for (j in 1:nstates) {
                        if (i != j && qmatrix[i, j] > 0) {
                            plot_data <- rbind(plot_data, data.frame(
                                time = time_seq,
                                intensity = rep(qmatrix[i, j], length(time_seq)),
                                transition = paste("State", i, "->", "State", j)
                            ))
                        }
                    }
                }
                
                if (nrow(plot_data) == 0) {
                    return()
                }
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = intensity, color = transition)) +
                    ggplot2::geom_line(size = 1.2) +
                    ggplot2::labs(
                        title = "Transition Intensities Over Time",
                        x = "Time",
                        y = "Transition Intensity",
                        color = "Transition"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )
                
                print(p)
                
            }, error = function(e) {
                # Silent failure for plotting
            })
        },
        
        .prevalencePlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.model))
                return()
            
            if (!self$options$plotPrevalence)
                return()
            
            tryCatch({
                # Get time range from data
                time_range <- range(private$.data$time)
                time_seq <- seq(time_range[1], time_range[2], length.out = 50)
                
                # Calculate prevalence at each time point
                plot_data <- data.frame()
                nstates <- self$options$nstates
                
                for (t in time_seq) {
                    prev <- msm::prevalence.msm(private$.model, times = t)
                    for (s in 1:nstates) {
                        plot_data <- rbind(plot_data, data.frame(
                            time = t,
                            prevalence = prev[1, s],
                            state = paste("State", s)
                        ))
                    }
                }
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = prevalence, color = state)) +
                    ggplot2::geom_line(size = 1.2) +
                    ggplot2::labs(
                        title = "Expected State Prevalence Over Time", 
                        x = "Time",
                        y = "Prevalence",
                        color = "Hidden State"
                    ) +
                    ggplot2::scale_y_continuous(limits = c(0, 1)) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )
                
                print(p)
                
            }, error = function(e) {
                # Silent failure for plotting
            })
        },
        
        .residualPlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.model))
                return()
            
            if (!self$options$plotResiduals)
                return()
            
            tryCatch({
                # Plot Pearson residuals
                residuals <- msm::pearson.msm(private$.model)
                
                plot_data <- data.frame(
                    time = private$.data$time,
                    residuals = residuals,
                    state = paste("State", private$.data$state)
                )
                
                p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = residuals)) +
                    ggplot2::geom_point(ggplot2::aes(color = state), alpha = 0.7) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                    ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
                    ggplot2::labs(
                        title = "Model Residuals",
                        x = "Time", 
                        y = "Pearson Residuals",
                        color = "Observed State"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
                        legend.position = "bottom"
                    )
                
                print(p)
                
            }, error = function(e) {
                # Silent failure for plotting  
            })
        }
    )
)