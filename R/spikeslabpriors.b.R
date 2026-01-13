
# This file is a generated template, your changes will not be overwritten

spikeslabpriorsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "spikeslabpriorsClass",
    inherit = spikeslabpriorsBase,
    private = list(
        .run = function() {
            # 1. Variable validation
            outcome <- self$options$outcome
            predictors <- self$options$predictors

            if (is.null(outcome) || length(predictors) == 0) {
                return()
            }

            # 2. Package check
            if (!requireNamespace("BoomSpikeSlab", quietly = TRUE)) {
                self$results$variableSelection$setNote("pkg_missing", "The 'BoomSpikeSlab' package is required for this analysis. Please install it using install.packages('BoomSpikeSlab').")
                return()
            }

            # 3. Data preparation
            data <- self$data
            
            # Filter variables and handle missing values
            vars_to_keep <- c(outcome, predictors)
            if (self$options$model_type == "cox") {
                if (is.null(self$options$time_variable) || is.null(self$options$status_variable)) {
                    self$results$variableSelection$setNote("missing_surv", "Time and Status variables are required for Cox regression.")
                    return()
                }
                vars_to_keep <- c(vars_to_keep, self$options$time_variable, self$options$status_variable)
            }
            
            data <- jmvcore::naOmit(data[, vars_to_keep])
            
            if (nrow(data) == 0) {
                jmvcore::reject("No data left after removing missing values.")
            }

            # 4. Model fitting
            tryCatch({
                res <- private$.fitModel(data)
                
                if (!is.null(res)) {
                    # 5. Populate Results Tables
                    private$.populateResults(res)
                    
                    # 6. Store result for plots
                    private$.model_res <- res
                }
            }, error = function(e) {
                jmvcore::reject(paste("Analysis error: ", e$message))
            })
        },

        .model_res = NULL,

        .fitModel = function(data) {
            model_type <- self$options$model_type
            outcome <- self$options$outcome
            predictors <- self$options$predictors
            niter <- self$options$mcmc_samples
            burnin <- self$options$mcmc_burnin
            
            # Prepare Formula
            formula_str <- paste0("`", outcome, "` ~ ", paste0("`", predictors, "`", collapse = " + "))
            formula <- as.formula(formula_str)
            
            res <- NULL
            
            # Determine Prior Inclusion Probabilities
            # BoomSpikeSlab uses a complex prior object, usually constructed via SpikeSlabPrior
            # For simplicity, we can pass expected.model.size if supported by the function
            
            if (model_type == "regression") {
                res <- BoomSpikeSlab::lm.spike(
                    formula,
                    data = data,
                    niter = niter,
                    ping = 0
                )
            } else if (model_type == "logistic") {
                res <- BoomSpikeSlab::logit.spike(
                    formula,
                    data = data,
                    niter = niter,
                    ping = 0
                )
            } else if (model_type == "poisson") {
                res <- BoomSpikeSlab::poisson.spike(
                    formula,
                    data = data,
                    niter = niter,
                    ping = 0
                )
            } else if (model_type == "probit") {
                res <- BoomSpikeSlab::probit.spike(
                    formula,
                    data = data,
                    niter = niter,
                    ping = 0
                )
            } else if (model_type == "cox") {
                # BoomSpikeSlab does not have Cox. Fallback to penalized or suggest alternative.
                stop("Cox proportional hazards is currently not supported by BoomSpikeSlab in this module. Support via spBayesSurv is planned.")
            }
            
            return(res)
        },

        .populateResults = function(res) {
            # Extract basic info
            # BoomSpikeSlab objects usually have:
            # - beta: MCMCsamples x nvars matrix
            # - inclusion.probabilities: vector length nvars
            
            # 1. Variable Selection Table
            table <- self$results$variableSelection
            pip <- BoomSpikeSlab::InclusionProbabilities(res)
            
            # Coefficients (posterior means)
            # res$beta usually includes intercept as first column
            beta <- res$beta
            post_means <- colMeans(beta)
            
            # Remove intercept from predictors list for display
            predictor_names <- names(pip)
            
            for (i in seq_along(predictor_names)) {
                var_name <- predictor_names[i]
                prob <- pip[i]
                
                # Selection based on threshold
                threshold <- self$options$inclusion_threshold
                selected <- if (prob >= threshold) "Yes" else "No"
                
                table$addRow(rowKey = var_name, values = list(
                    variable = var_name,
                    inclusion_probability = prob,
                    bayes_factor = prob / (1 - prob), # Simple approximation
                    selected = selected,
                    rank = rank(-pip)[i]
                ))
            }
            
            # 2. Inclusion Probabilities Table
            table_pip <- self$results$inclusionProbabilities
            for (i in seq_along(predictor_names)) {
                var_name <- predictor_names[i]
                prob <- pip[i]
                
                table_pip$addRow(rowKey = var_name, values = list(
                    variable = var_name,
                    posterior_pip = prob,
                    prior_pip = self$options$prior_inclusion_prob,
                    bf_inclusion = (prob / (1 - prob)) / (self$options$prior_inclusion_prob / (1 - self$options$prior_inclusion_prob))
                ))
            }
            
            # 3. Coefficient Estimates Table
            table_coef <- self$results$coefficientEstimates
            conf_level <- self$options$confidence_level
            lower_q <- (1 - conf_level) / 2
            upper_q <- 1 - lower_q
            
            # BoomSpikeSlab res$beta has intercept potentially
            # We need to align with names(pip)
            
            for (i in seq_along(predictor_names)) {
                var_name <- predictor_names[i]
                # Match var_name in beta columns
                # BoomSpikeSlab names might be clean or messed up
                col_idx <- which(colnames(beta) == var_name)
                
                if (length(col_idx) > 0) {
                    samples <- beta[, col_idx]
                    table_coef$addRow(rowKey = var_name, values = list(
                        variable = var_name,
                        posterior_mean = mean(samples),
                        posterior_sd = sd(samples),
                        credible_interval_lower = quantile(samples, lower_q),
                        credible_interval_upper = quantile(samples, upper_q)
                    ))
                }
            }
            
            # Additional tables can be populated here...
        },

        .plotInclusionProbabilities = function(image, ...) {
            if (is.null(private$.model_res)) return(FALSE)
            
            res <- private$.model_res
            pip <- BoomSpikeSlab::InclusionProbabilities(res)
            df <- data.frame(
                Variable = names(pip),
                PIP = as.numeric(pip)
            )
            df <- df[order(df$PIP, decreasing = TRUE), ]
            df$Variable <- factor(df$Variable, levels = df$Variable)
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = Variable, y = PIP)) +
                ggplot2::geom_bar(stat = "identity", fill = "#E64B35FF") +
                ggplot2::geom_hline(yintercept = self$options$inclusion_threshold, linetype = "dashed", color = "gray") +
                ggplot2::ylim(0, 1) +
                ggplot2::labs(title = "Posterior Inclusion Probabilities",
                             x = "Variables", y = "Probability") +
                ggplot2::theme_minimal() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            
            print(p)
            TRUE
        },
        
        .plotCoefficients = function(image, ...) {
            if (is.null(private$.model_res)) return(FALSE)
            
            res <- private$.model_res
            beta <- res$beta
            
            # Exclude Intercept
            beta_no_int <- beta[, colnames(beta) != "(Intercept)", drop = FALSE]
            
            df_list <- lapply(colnames(beta_no_int), function(name) {
                samples <- beta_no_int[, name]
                data.frame(
                    Variable = name,
                    Mean = mean(samples),
                    Lower = quantile(samples, 0.025),
                    Upper = quantile(samples, 0.975)
                )
            })
            df <- do.call(rbind, df_list)
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(Variable, Mean), y = Mean)) +
                ggplot2::geom_pointrange(ggplot2::aes(ymin = Lower, ymax = Upper), color = "#3C5488FF") +
                ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
                ggplot2::coord_flip() +
                ggplot2::labs(title = "Coefficient Estimates (95% Credible Interval)",
                             x = "Variables", y = "Estimate") +
                ggplot2::theme_minimal()
            
            print(p)
            TRUE
        }
    )
)
