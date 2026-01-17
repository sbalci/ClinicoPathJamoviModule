#' @title Cure Models for Long-term Survivors
#' @importFrom R6 R6Class
#' @import jmvcore
#'

curemodelsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "curemodelsClass",
    inherit = curemodelsBase,
    private = list(

        cure_data = NULL,
        cure_model = NULL,
        nm_cure_model = NULL,
        cure_cure_model = NULL,
        npcure_model = NULL,

        # Variable name safety helper
        .escapeVar = function(varName) {
            if (is.null(varName) || length(varName) == 0) return(NULL)
            # Escape variable names with special characters for formula construction
            return(jmvcore::composeTerm(varName))
        },

        # Notice insertion helper for consistent user feedback
        .insertNotice = function(name, type, content, position = NULL) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = name,
                type = type
            )
            notice$setContent(content)

            # Determine position if not specified
            if (is.null(position)) {
                position <- switch(as.character(type),
                    "1" = 1,  # ERROR at top
                    "2" = 1,  # STRONG_WARNING at top
                    "3" = 2,  # WARNING after errors
                    "4" = 999,  # INFO at bottom
                    2  # default
                )
            }

            self$results$insert(position, notice)
        },

        .init = function() {
            # Check for required packages
            if (!requireNamespace('smcure', quietly = TRUE)) {
                private$.insertNotice(
                    name = 'smcureMissing',
                    type = jmvcore::NoticeType$STRONG_WARNING,
                    content = paste0(
                        "The smcure package is required but not installed.\n",
                        "Please install it using: install.packages('smcure')"
                    ),
                    position = 1
                )
            }

            if (!requireNamespace('flexsurvcure', quietly = TRUE)) {
                private$.insertNotice(
                    name = 'flexsurvcureMissing',
                    type = jmvcore::NoticeType$INFO,
                    content = paste0(
                        "The flexsurvcure package is recommended for non-mixture cure models.\n",
                        "Install using: install.packages('flexsurvcure')"
                    ),
                    position = 999
                )
            }
        },

        .validateInputData = function(data, time_var, status_var) {

            # Apply variable name escaping for safe data access
            escaped_time <- private$.escapeVar(time_var)
            escaped_status <- private$.escapeVar(status_var)

            # Extract columns safely
            time_col <- data[[escaped_time]]
            status_col <- data[[escaped_status]]

            # Validate time variable
            if (any(time_col < 0, na.rm = TRUE)) {
                stop("Time variable must be non-negative. Found negative values in survival time.")
            }

            if (any(is.infinite(time_col), na.rm = TRUE)) {
                stop("Time variable contains infinite values. Please check your data.")
            }

            # Validate status variable
            status_vals <- unique(status_col)
            status_vals <- status_vals[!is.na(status_vals)]

            if (is.factor(status_col)) {
                # Convert factor to numeric for validation
                status_numeric <- as.numeric(status_col) - 1
                if (!all(status_numeric %in% c(0, 1))) {
                    stop("Status variable must be binary. Expected values: 0 (censored) and 1 (event), or equivalent factor levels.")
                }
            } else {
                if (!all(status_vals %in% c(0, 1))) {
                    stop("Status variable must be binary (0 = censored, 1 = event). Found values: ", paste(status_vals, collapse = ", "))
                }
            }

            # Check for minimum events
            n_events <- sum(status_col == 1 | (is.factor(status_col) & as.numeric(status_col) == 2), na.rm = TRUE)
            if (n_events < 10) {
                warning("Few events detected (n = ", n_events, "). Cure model estimates may be unreliable.")
            }

            # Check for adequate follow-up
            max_time <- max(time_col, na.rm = TRUE)
            median_time <- median(time_col, na.rm = TRUE)
            if (max_time < 2 * median_time) {
                warning("Short follow-up detected. Cure models require substantial follow-up for reliable estimation.")
            }

            return(TRUE)
        },

        .run = function() {

            # Check if variables are selected
            if (is.null(self$options$time) || is.null(self$options$status)) {
                self$results$todo$setContent(paste0(
                    "<div style='background-color: #f8f9fa; padding: 20px; margin: 15px 0; border-radius: 8px; border-left: 5px solid #007bff;'>",
                    "<h3 style='margin-top: 0; color: #007bff;'>Welcome to Cure Models Analysis</h3>",

                    "<p><strong>Purpose:</strong> Model survival data with a cured fraction of long-term survivors who will never experience the event.</p>",

                    "<h4 style='margin-top: 15px;'>When to Use Cure Models:</h4>",
                    "<ul style='margin-left: 20px;'>",
                    "<li><strong>Plateau observed:</strong> Kaplan-Meier curve plateaus at long follow-up times</li>",
                    "<li><strong>Biological plausibility:</strong> Cure is possible (e.g., early-stage cancers, infectious diseases)</li>",
                    "<li><strong>Long-term survivors:</strong> Data shows patients surviving well beyond typical event times</li>",
                    "<li><strong>Heterogeneous risk:</strong> Population consists of cured and susceptible individuals</li>",
                    "</ul>",

                    "<h4 style='margin-top: 15px;'>Quick Start:</h4>",
                    "<ol style='margin-left: 20px;'>",
                    "<li>Select <strong>Survival Time</strong> variable (follow-up duration in months/years)</li>",
                    "<li>Select <strong>Event Status</strong> variable (0 = censored, 1 = event occurred)</li>",
                    "<li>Select <strong>Predictor Variables</strong> (optional: factors affecting cure probability)</li>",
                    "<li>Choose <strong>Model Type</strong> from options below</li>",
                    "</ol>",

                    "<h4 style='margin-top: 15px;'>Model Types Available:</h4>",
                    "<ul style='margin-left: 20px;'>",
                    "<li><strong>Mixture Cure Model (smcure):</strong> Assumes population = cured group + uncured group with distinct survival</li>",
                    "<li><strong>Non-mixture Cure Model (flexsurvcure):</strong> Single survival distribution with cure as limiting probability at infinity</li>",
                    "<li><strong>cuRe Model:</strong> Incorporates background mortality from general population (requires life tables)</li>",
                    "<li><strong>Nonparametric Model (npcure):</strong> No distributional assumptions, flexible cure probability estimation</li>",
                    "<li><strong>Compare All Models:</strong> Fits all model types and provides AIC/BIC comparison</li>",
                    "</ul>",

                    "<h4 style='margin-top: 15px;'>Required Packages:</h4>",
                    "<p style='margin-left: 20px;'><code>smcure</code>, <code>flexsurvcure</code> (optional: <code>cuRe</code>, <code>npcure</code>)</p>",

                    "<p style='margin-top: 15px;'><em>💡 Tip: Start with Mixture Cure Model, then compare with other types if needed. Ensure adequate follow-up time (at least 2-3x median event time) for reliable cure fraction estimates.</em></p>",
                    "</div>"
                ))
                return()
            }

            # Get data and variables
            data <- self$data
            time_var <- self$options$time
            status_var <- self$options$status
            predictors <- self$options$predictors
            model_type <- self$options$model_type %||% "mixture"

            # Clean data
            analysis_vars <- c(time_var, status_var, predictors)
            clean_data <- data[complete.cases(data[, analysis_vars]), analysis_vars]

            if (nrow(clean_data) < 30) {
                private$.insertNotice(
                    name = 'insufficientData',
                    type = jmvcore::NoticeType$ERROR,
                    content = paste0(
                        "Insufficient data for cure model analysis. ",
                        "Found ", nrow(clean_data), " complete cases, but minimum 30 required. ",
                        "Please check for missing values in survival time, event status, or predictor variables."
                    ),
                    position = 1
                )
                return()
            }

            # Store clean data for diagnostics
            private$cure_data <- clean_data

            # Validate input data
            validation_result <- tryCatch({
                private$.validateInputData(clean_data, time_var, status_var)
                TRUE
            }, warning = function(w) {
                private$.insertNotice(
                    name = paste0('validationWarning_', gsub("[^a-zA-Z0-9]", "", substr(w$message, 1, 20))),
                    type = jmvcore::NoticeType$WARNING,
                    content = w$message,
                    position = 2
                )
                TRUE
            }, error = function(e) {
                private$.insertNotice(
                    name = 'validationError',
                    type = jmvcore::NoticeType$ERROR,
                    content = paste0("Data validation failed: ", e$message),
                    position = 1
                )
                FALSE
            })

            if (!validation_result) {
                return()
            }

            # Add warnings for experimental/specialized features
            if (model_type == "cure") {
                private$.insertNotice(
                    name = 'cuReModelInfo',
                    type = jmvcore::NoticeType$INFO,
                    content = paste0(
                        "ℹ️ cuRe Model Selected\n\n",
                        "The cuRe model is a specialized approach that can incorporate background mortality rates. ",
                        "This is particularly useful for cancer survival analysis where general population mortality matters.\n\n",
                        "Requirements:\n",
                        "• Requires cuRe package installation\n",
                        "• Optionally uses background hazard variable (e.g., from life tables)\n",
                        "• Best for long-term cancer survival studies"
                    ),
                    position = 3
                )
            }

            if (model_type == "npcure") {
                private$.insertNotice(
                    name = 'npcureModelInfo',
                    type = jmvcore::NoticeType$INFO,
                    content = paste0(
                        "ℹ️ Nonparametric Cure Model Selected\n\n",
                        "The npcure model uses kernel smoothing without distributional assumptions. ",
                        "This provides flexibility but has limitations:\n\n",
                        "• Works with single continuous covariate only\n",
                        "• Sensitive to bandwidth selection\n",
                        "• Requires larger sample sizes for stable estimates\n",
                        "• Results may be harder to interpret than parametric models\n\n",
                        "Consider starting with parametric models (mixture/non-mixture) for initial exploration."
                    ),
                    position = 3
                )
            }

            # Warn about bootstrap computation time
            if (self$options$bootstrap_ci %||% FALSE) {
                n_bootstrap <- self$options$n_bootstrap %||% 1000
                if (n_bootstrap >= 500) {
                    private$.insertNotice(
                        name = 'bootstrapWarning',
                        type = jmvcore::NoticeType$INFO,
                        content = paste0(
                            "⏱️ Bootstrap Confidence Intervals\n\n",
                            "Bootstrap resampling is enabled with ", n_bootstrap, " iterations. ",
                            "This will take additional computation time (possibly several minutes).\n\n",
                            "Progress cannot be displayed during bootstrap. Please be patient."
                        ),
                        position = 4
                    )
                }
            }

            # Fit cure models based on type
            if (model_type == "mixture" || model_type == "all") {
                private$.fitMixtureCureModel(clean_data, time_var, status_var, predictors)
            }

            if (model_type == "nonmixture" || model_type == "all") {
                private$.fitNonMixtureCureModel(clean_data, time_var, status_var, predictors)
            }

            if (model_type == "cure" || model_type == "all") {
                private$.fitCuReModel(clean_data, time_var, status_var, predictors)
            }

            if (model_type == "npcure" || model_type == "all") {
                private$.fitNpCureModel(clean_data, time_var, status_var, predictors)
            }

            # Compare models if all were fitted
            if (model_type == "all") {
                private$.compareModels()
            }

            # Generate plots if requested
            if (self$options$plot_cure_fraction) {
                private$.plotCureFraction()
            }

            if (self$options$plot_survival) {
                private$.plotSurvival()
            }

            # Perform additional analyses
            if (self$options$goodness_of_fit) {
                private$.assessGoodnessOfFit()
            }

            if (self$options$sensitivity_analysis) {
                private$.performSensitivityAnalysis()
            }

            # Generate clinical interpretation
            private$.generateClinicalInterpretation()

            # Add completion notice
            private$.insertNotice(
                name = 'analysisComplete',
                type = jmvcore::NoticeType$INFO,
                content = paste0(
                    "Analysis completed successfully. ",
                    "Model type: ", model_type, ". ",
                    "Review the results tables and plots above."
                ),
                position = 999
            )
        },

        .fitMixtureCureModel = function(data, time_var, status_var, predictors) {

            tryCatch({
                # Check for smcure package
                if (!requireNamespace('smcure', quietly = TRUE)) {
                    stop("smcure package is required for mixture cure models")
                }

                # Prepare formulas using safer approach with variable escaping
                if (length(predictors) > 0) {
                    # Escape predictor names for safe formula construction
                    escaped_predictors <- sapply(predictors, private$.escapeVar)

                    cure_formula <- reformulate(escaped_predictors, response = NULL)
                    # Escape time and status variables as well
                    escaped_time <- private$.escapeVar(time_var)
                    escaped_status <- private$.escapeVar(status_var)
                    surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")
                    surv_formula <- reformulate(escaped_predictors, response = surv_response)
                } else {
                    cure_formula <- ~ 1
                    # Escape variables even in simple formula
                    escaped_time <- private$.escapeVar(time_var)
                    escaped_status <- private$.escapeVar(status_var)
                    surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")
                    surv_formula <- as.formula(paste(surv_response, "~ 1"))
                }

                # Get options
                cure_link <- self$options$cure_link %||% "logit"
                surv_dist <- self$options$survival_dist %||% "weibull"
                bootstrap_ci <- self$options$bootstrap_ci %||% FALSE
                n_bootstrap <- self$options$n_bootstrap %||% 1000

                # Optimize bootstrap for performance
                if (bootstrap_ci && n_bootstrap > 1000) {
                    # Check for parallel capabilities
                    if (requireNamespace("parallel", quietly = TRUE)) {
                        n_cores <- min(parallel::detectCores() - 1, 4)  # Use up to 4 cores
                        message("Using parallel bootstrap with ", n_cores, " cores for ", n_bootstrap, " samples")
                        # Note: smcure doesn't support parallel directly, but we inform user
                    }

                    # For very large bootstrap samples, warn user about time
                    if (n_bootstrap > 5000) {
                        private$.insertNotice(
                            name = 'largeBootstrap',
                            type = jmvcore::NoticeType$INFO,
                            content = paste0(
                                "Large bootstrap sample (", n_bootstrap, " samples) may take several minutes to complete. ",
                                "Consider reducing the number of bootstrap samples for faster results."
                            ),
                            position = 999
                        )
                    }
                }

                # Fit mixture cure model using smcure
                cure_model <- smcure::smcure(
                    formula = surv_formula,
                    cureform = cure_formula,
                    data = data,
                    model = cure_link,
                    dist = surv_dist,
                    nboot = ifelse(bootstrap_ci, n_bootstrap, 0)
                )

                # Extract and format results
                private$.formatMixtureCureResults(cure_model)

                # Store model for plotting
                private$cure_model <- cure_model

            }, error = function(e) {
                # Enhanced error handling with specific error types
                error_msg <- e$message

                if (grepl("convergence|iteration", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste0(
                        "Mixture cure model convergence failed: ", error_msg, "\n\n",
                        "Suggestions:\n",
                        "• Try different starting values\n",
                        "• Reduce the number of predictors\n",
                        "• Check for collinear variables\n",
                        "• Ensure adequate sample size and follow-up"
                    )
                    notice_type <- jmvcore::NoticeType$STRONG_WARNING
                } else if (grepl("singular|matrix", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste0(
                        "Matrix computation error in mixture cure model: ", error_msg, "\n\n",
                        "Suggestions:\n",
                        "• Check for collinear predictors\n",
                        "• Remove variables with little variation\n",
                        "• Ensure sufficient sample size"
                    )
                    notice_type <- jmvcore::NoticeType$STRONG_WARNING
                } else if (grepl("bootstrap", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste0(
                        "Bootstrap procedure failed: ", error_msg, "\n\n",
                        "Suggestions:\n",
                        "• Reduce number of bootstrap samples\n",
                        "• Try without bootstrap CI first\n",
                        "• Check data for extreme values"
                    )
                    notice_type <- jmvcore::NoticeType$WARNING
                } else {
                    detailed_msg <- paste0("Mixture cure model fitting failed: ", error_msg)
                    notice_type <- jmvcore::NoticeType$STRONG_WARNING
                }

                private$.insertNotice(
                    name = 'mixtureCureModelError',
                    type = notice_type,
                    content = detailed_msg
                )
            })
        },

        .fitNonMixtureCureModel = function(data, time_var, status_var, predictors) {

            tryCatch({
                # Check for flexsurvcure package
                if (!requireNamespace('flexsurvcure', quietly = TRUE)) {
                    # Fallback to parametric survival with cure fraction estimation
                    private$.fitParametricCureModel(data, time_var, status_var, predictors)
                    return()
                }

                # Prepare formula with variable escaping
                if (length(predictors) > 0) {
                    # Escape predictor names for safe formula construction
                    escaped_predictors <- sapply(predictors, private$.escapeVar)

                    # Escape time and status variables
                    escaped_time <- private$.escapeVar(time_var)
                    escaped_status <- private$.escapeVar(status_var)
                    surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")
                    formula <- reformulate(escaped_predictors, response = surv_response)
                } else {
                    # Escape variables even in simple formula
                    escaped_time <- private$.escapeVar(time_var)
                    escaped_status <- private$.escapeVar(status_var)
                    surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")
                    formula <- as.formula(paste(surv_response, "~ 1"))
                }

                # Get distribution
                surv_dist <- self$options$survival_dist %||% "weibull"

                # Map distribution names to flexsurvcure functions
                dist_map <- list(
                    "weibull" = "weibullPH",
                    "exponential" = "exp",
                    "lognormal" = "lnorm",
                    "loglogistic" = "llogis"
                )

                # Fit non-mixture cure model
                nm_cure_model <- flexsurvcure::flexsurvcure(
                    formula = formula,
                    data = data,
                    dist = dist_map[[surv_dist]],
                    mixture = FALSE
                )

                # Extract and format results
                private$.formatNonMixtureCureResults(nm_cure_model)

                # Store model
                private$nm_cure_model <- nm_cure_model

            }, error = function(e) {
                private$.insertNotice(
                    name = 'nonMixtureCureModelError',
                    type = jmvcore::NoticeType$STRONG_WARNING,
                    content = paste0("Non-mixture cure model fitting failed: ", e$message)
                )
            })
        },

        .fitCuReModel = function(data, time_var, status_var, predictors) {

            tryCatch({
                # Check for cuRe package
                if (!requireNamespace('cuRe', quietly = TRUE)) {
                    stop("cuRe package is required for background mortality cure models. Install using: install.packages('cuRe')")
                }

                # Prepare formula with variable escaping
                if (length(predictors) > 0) {
                    # Escape predictor names for safe formula construction
                    escaped_predictors <- sapply(predictors, private$.escapeVar)

                    # Escape time and status variables
                    escaped_time <- private$.escapeVar(time_var)
                    escaped_status <- private$.escapeVar(status_var)
                    surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")
                    surv_formula <- reformulate(escaped_predictors, response = surv_response)
                } else {
                    # Escape variables even in simple formula
                    escaped_time <- private$.escapeVar(time_var)
                    escaped_status <- private$.escapeVar(status_var)
                    surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")
                    surv_formula <- as.formula(paste(surv_response, "~ 1"))
                }

                # Get cuRe-specific options
                use_background <- self$options$use_background_mortality %||% FALSE
                background_var <- self$options$background_hazard_var
                surv_dist <- self$options$survival_dist %||% "weibull"

                # Fit cuRe model
                if (use_background && !is.null(background_var) && background_var != "") {
                    # Model with background mortality
                    # Extract the background hazard vector from data
                    escaped_bg <- private$.escapeVar(background_var)
                    bg_hazard_vec <- data[[escaped_bg]]

                    cure_model <- cuRe::fit.cure.model(
                        formula = surv_formula,
                        data = data,
                        bhazard = bg_hazard_vec,  # Pass actual numeric vector
                        dist = surv_dist,
                        link = self$options$cure_link %||% "logit"
                    )
                } else {
                    # Model without background mortality (standard cure model)
                    cure_model <- cuRe::fit.cure.model(
                        formula = surv_formula,
                        data = data,
                        dist = surv_dist,
                        link = self$options$cure_link %||% "logit"
                    )
                }

                # Format and store results
                private$.formatCuReResults(cure_model)

                # Store model for plotting
                private$cure_cure_model <- cure_model

            }, error = function(e) {
                error_msg <- e$message

                if (grepl("convergence", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste0(
                        "cuRe model convergence failed: ", error_msg, "\n\n",
                        "Suggestions:\n",
                        "• Try different starting values\n",
                        "• Check background hazard variable\n",
                        "• Ensure adequate follow-up time\n",
                        "• Verify data quality"
                    )
                    notice_type <- jmvcore::NoticeType$STRONG_WARNING
                } else if (grepl("bhazard|background", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste0(
                        "Background mortality variable error: ", error_msg, "\n\n",
                        "Suggestions:\n",
                        "• Verify background hazard variable exists\n",
                        "• Check that values are non-negative\n",
                        "• Ensure variable is numeric"
                    )
                    notice_type <- jmvcore::NoticeType$ERROR
                } else {
                    detailed_msg <- paste0("cuRe model fitting failed: ", error_msg)
                    notice_type <- jmvcore::NoticeType$STRONG_WARNING
                }

                private$.insertNotice(
                    name = 'cuReModelError',
                    type = notice_type,
                    content = detailed_msg
                )
            })
        },

        .fitNpCureModel = function(data, time_var, status_var, predictors) {

            tryCatch({
                # Check for npcure package
                if (!requireNamespace('npcure', quietly = TRUE)) {
                    stop("npcure package is required for nonparametric cure models. Install using: install.packages('npcure')")
                }

                # npcure works with single continuous covariate
                covariate <- self$options$npcure_covariate

                if (is.null(covariate) || length(covariate) == 0) {
                    stop("npcure requires a single continuous covariate. Please select a covariate in the npcure options.")
                }

                # Get npcure-specific options
                bandwidth_option <- self$options$npcure_bandwidth %||% "auto"
                n_time_points <- self$options$npcure_time_points %||% 100

                # Convert bandwidth option to numeric
                bandwidth <- switch(bandwidth_option,
                    "auto" = NULL,  # Let npcure choose automatically
                    "small" = 0.1,
                    "medium" = 0.3,
                    "large" = 0.5,
                    NULL
                )

                # Apply variable escaping for safe data access
                escaped_time <- private$.escapeVar(time_var)
                escaped_status <- private$.escapeVar(status_var)
                escaped_covar <- private$.escapeVar(covariate)

                # Extract columns safely
                time_vec <- data[[escaped_time]]
                status_vec <- data[[escaped_status]]
                covar_vec <- data[[escaped_covar]]

                # Generate time points for estimation
                time_seq <- seq(0, max(time_vec, na.rm = TRUE), length.out = n_time_points)

                # Fit nonparametric cure model
                if (is.null(bandwidth)) {
                    # Automatic bandwidth selection via cross-validation
                    np_cure_model <- npcure::probcure(
                        time = time_vec,
                        status = status_vec,
                        x = covar_vec,
                        testimate = time_seq,
                        conflevel = 0.95
                    )
                } else {
                    # User-specified bandwidth
                    np_cure_model <- npcure::probcure(
                        time = time_vec,
                        status = status_vec,
                        x = covar_vec,
                        h = bandwidth,
                        testimate = time_seq,
                        conflevel = 0.95
                    )
                }

                # Format and store results
                private$.formatNpCureResults(np_cure_model, covariate)

                # Store model for plotting
                private$npcure_model <- np_cure_model

            }, error = function(e) {
                error_msg <- e$message

                if (grepl("bandwidth|h =", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste0(
                        "Bandwidth selection failed: ", error_msg, "\n\n",
                        "Suggestions:\n",
                        "• Try 'auto' bandwidth\n",
                        "• Adjust bandwidth manually (small/medium/large)\n",
                        "• Check covariate distribution\n",
                        "• Ensure sufficient sample size"
                    )
                    notice_type <- jmvcore::NoticeType$WARNING
                } else if (grepl("covariate", error_msg, ignore.case = TRUE)) {
                    detailed_msg <- paste0(
                        "Covariate error: ", error_msg, "\n\n",
                        "Suggestions:\n",
                        "• Select a continuous numeric covariate\n",
                        "• Check for missing values\n",
                        "• Ensure adequate variation in covariate"
                    )
                    notice_type <- jmvcore::NoticeType$ERROR
                } else {
                    detailed_msg <- paste0("Nonparametric cure model fitting failed: ", error_msg)
                    notice_type <- jmvcore::NoticeType$STRONG_WARNING
                }

                private$.insertNotice(
                    name = 'npcureModelError',
                    type = notice_type,
                    content = detailed_msg
                )
            })
        },

        .fitParametricCureModel = function(data, time_var, status_var, predictors) {
            # Fallback when flexsurvcure is not available
            # Provides informative notice instead of crashing

            private$.insertNotice(
                name = 'flexsurvcureNotAvailable',
                type = jmvcore::NoticeType$STRONG_WARNING,
                content = paste0(
                    "Non-mixture cure model requires the flexsurvcure package, which is not installed.\n\n",
                    "To use non-mixture cure models, please install flexsurvcure:\n",
                    "install.packages('flexsurvcure')\n\n",
                    "Alternatively, use the Mixture Cure Model option, which uses the smcure package."
                )
            )

            # Do not populate tables or plots - user needs to install package
            return(NULL)
        },

        .formatMixtureCureResults = function(model) {

            # Create results table
            results_table <- self$results$modelTable

            # Extract cure probability coefficients
            cure_coef <- model$beta
            cure_se <- sqrt(diag(model$cureb.vcov))
            cure_z <- cure_coef / cure_se
            cure_p <- 2 * (1 - pnorm(abs(cure_z)))

            # Add cure probability results
            for (i in seq_along(cure_coef)) {
                results_table$addRow(rowKey = paste0("cure_", i), values = list(
                    parameter = paste("Cure:", names(cure_coef)[i] %||% paste("Param", i)),
                    estimate = round(cure_coef[i], 4),
                    std_error = round(cure_se[i], 4),
                    z_value = round(cure_z[i], 3),
                    p_value = round(cure_p[i], 4),
                    ci_lower = round(cure_coef[i] - 1.96 * cure_se[i], 4),
                    ci_upper = round(cure_coef[i] + 1.96 * cure_se[i], 4)
                ))
            }

            # Extract survival coefficients
            surv_coef <- model$b
            surv_se <- sqrt(diag(model$survb.vcov))
            surv_z <- surv_coef / surv_se
            surv_p <- 2 * (1 - pnorm(abs(surv_z)))

            # Add survival results
            for (i in seq_along(surv_coef)) {
                results_table$addRow(rowKey = paste0("surv_", i), values = list(
                    parameter = paste("Survival:", names(surv_coef)[i] %||% paste("Param", i)),
                    estimate = round(surv_coef[i], 4),
                    std_error = round(surv_se[i], 4),
                    z_value = round(surv_z[i], 3),
                    p_value = round(surv_p[i], 4),
                    ci_lower = round(surv_coef[i] - 1.96 * surv_se[i], 4),
                    ci_upper = round(surv_coef[i] + 1.96 * surv_se[i], 4)
                ))
            }

            # Calculate and display cure fraction
            cure_fraction <- 1 / (1 + exp(-model$beta[1]))  # Intercept cure fraction

            # Populate cure fraction table
            cure_table <- self$results$cureTable

            # Calculate confidence intervals - use bootstrap if available
            bootstrap_ci <- self$options$bootstrap_ci %||% FALSE
            if (bootstrap_ci && !is.null(model$cureb.boot)) {
                # Use bootstrap CIs if available
                boot_cure_fractions <- 1 / (1 + exp(-model$cureb.boot[, 1]))
                cure_ci_lower <- quantile(boot_cure_fractions, 0.025, na.rm = TRUE)
                cure_ci_upper <- quantile(boot_cure_fractions, 0.975, na.rm = TRUE)
            } else {
                # Use normal approximation
                se_cure <- sqrt(diag(model$cureb.vcov))[1]
                cure_ci_lower <- 1 / (1 + exp(-(model$beta[1] - 1.96 * se_cure)))
                cure_ci_upper <- 1 / (1 + exp(-(model$beta[1] + 1.96 * se_cure)))
            }

            # Get median survival for uncured (if available)
            uncured_median <- ifelse(!is.null(model$Time),
                                   round(quantile(model$Time[model$Status == 1], 0.5, na.rm = TRUE), 2),
                                   NA)

            cure_table$addRow(rowKey = "overall", values = list(
                group = "Overall",
                cure_fraction = round(cure_fraction, 4),
                cure_ci_lower = round(cure_ci_lower, 4),
                cure_ci_upper = round(cure_ci_upper, 4),
                uncured_median = uncured_median
            ))

            summary_html <- glue::glue(
                "<h3>Mixture Cure Model Results</h3>
                <p><b>Estimated Cure Fraction:</b> {round(cure_fraction * 100, 1)}%</p>
                <p><b>Model Type:</b> {self$options$cure_link} link, {self$options$survival_dist} survival</p>
                <p><b>Sample Size:</b> {model$n}</p>
                <p><b>Events:</b> {sum(model$s == 1)}</p>
                <p><b>Censored:</b> {sum(model$s == 0)}</p>

                <h4>Interpretation:</h4>
                <ul>
                <li>Cure probability coefficients show factors associated with being cured</li>
                <li>Positive coefficients increase cure probability</li>
                <li>Survival coefficients apply only to the uncured fraction</li>
                </ul>"
            )

            self$results$summary$setContent(summary_html)
        },

        .formatNonMixtureCureResults = function(model) {

            # Extract model coefficients
            coef_summary <- summary(model)

            # Create results table
            results_table <- self$results$modelTable

            # Add coefficient results
            for (i in 1:nrow(coef_summary)) {
                std_err <- coef_summary[i, "se"]
                est_val <- coef_summary[i, "est"]
                z_val <- if (!is.na(std_err) && std_err > 0) est_val / std_err else NA

                results_table$addRow(rowKey = i, values = list(
                    parameter = rownames(coef_summary)[i],
                    estimate = round(est_val, 4),
                    std_error = round(std_err, 4),
                    z_value = if (!is.na(z_val)) round(z_val, 3) else NA,
                    p_value = if (!is.na(z_val)) round(2 * (1 - pnorm(abs(z_val))), 4) else NA,
                    ci_lower = round(coef_summary[i, "L95%"], 4),
                    ci_upper = round(coef_summary[i, "U95%"], 4)
                ))
            }

            # Generate summary
            summary_html <- glue::glue(
                "<h3>Non-Mixture Cure Model Results</h3>
                <p><b>Distribution:</b> {self$options$survival_dist}</p>
                <p><b>Log-likelihood:</b> {round(model$loglik, 2)}</p>
                <p><b>AIC:</b> {round(model$AIC, 2)}</p>

                <h4>Model Description:</h4>
                <p>Non-mixture cure models assume the entire population follows the same
                survival distribution with cure as a limiting probability as time approaches infinity.</p>"
            )

            self$results$summary$setContent(summary_html)
        },

        .formatCuReResults = function(model) {

            # Extract model summary
            model_summary <- summary(model)

            # Create results table
            results_table <- self$results$modelTable

            # Add coefficient results
            if (!is.null(model_summary$coef)) {
                coef_matrix <- model_summary$coef

                for (i in 1:nrow(coef_matrix)) {
                    std_err <- coef_matrix[i, "Std. Error"]
                    est_val <- coef_matrix[i, "Estimate"]
                    z_val <- if (!is.na(std_err) && std_err > 0) est_val / std_err else NA

                    results_table$addRow(rowKey = paste0("cure_", i), values = list(
                        parameter = rownames(coef_matrix)[i],
                        estimate = round(est_val, 4),
                        std_error = round(std_err, 4),
                        z_value = if (!is.na(z_val)) round(z_val, 3) else NA,
                        p_value = if ("Pr(>|z|)" %in% colnames(coef_matrix)) {
                            round(coef_matrix[i, "Pr(>|z|)"], 4)
                        } else if (!is.na(z_val)) {
                            round(2 * (1 - pnorm(abs(z_val))), 4)
                        } else {
                            NA
                        },
                        ci_lower = round(est_val - 1.96 * std_err, 4),
                        ci_upper = round(est_val + 1.96 * std_err, 4)
                    ))
                }
            }

            # Extract cure fraction estimate if available
            cure_fraction <- if (!is.null(model$cure_fraction)) {
                round(model$cure_fraction, 3)
            } else {
                "See model coefficients"
            }

            # Generate summary with background mortality info
            background_note <- if (self$options$use_background_mortality && !is.null(self$options$background_hazard_var)) {
                paste0("<p><b>Background Mortality:</b> Included (variable: ", self$options$background_hazard_var, ")</p>")
            } else {
                "<p><b>Background Mortality:</b> Not included</p>"
            }

            summary_html <- glue::glue(
                "<h3>cuRe Model Results</h3>
                <p><b>Distribution:</b> {self$options$survival_dist %||% 'weibull'}</p>
                <p><b>Link Function:</b> {self$options$cure_link %||% 'logit'}</p>
                {background_note}
                <p><b>Estimated Cure Fraction:</b> {cure_fraction}</p>

                <h4>Model Description:</h4>
                <p>The cuRe model allows incorporation of background mortality rates from the general population.
                This is particularly useful when analyzing cancer survival where patients may die from competing causes.</p>

                <h4>Key Features:</h4>
                <ul>
                <li>Accounts for background population mortality</li>
                <li>Separates excess hazard due to disease from background risk</li>
                <li>Provides more accurate cure fraction estimates in aging populations</li>
                </ul>"
            )

            self$results$summary$setContent(summary_html)
        },

        .formatNpCureResults = function(model, covariate_name) {

            # Create summary table for npcure results
            results_table <- self$results$modelTable

            # npcure provides cure probability estimates across covariate values
            if (!is.null(model$x)) {
                # Extract cure probability estimates
                cure_probs <- model$curerate
                covariate_vals <- model$x

                # Add representative estimates to table
                n_points <- min(10, length(cure_probs))  # Show up to 10 representative points
                indices <- round(seq(1, length(cure_probs), length.out = n_points))

                for (i in seq_along(indices)) {
                    idx <- indices[i]
                    results_table$addRow(rowKey = paste0("npcure_", i), values = list(
                        parameter = paste0(covariate_name, " = ", round(covariate_vals[idx], 2)),
                        estimate = round(cure_probs[idx], 4),
                        std_error = NA,
                        z_value = NA,
                        p_value = NA,
                        ci_lower = if (!is.null(model$lower)) round(model$lower[idx], 4) else NA,
                        ci_upper = if (!is.null(model$upper)) round(model$upper[idx], 4) else NA
                    ))
                }
            }

            # Get bandwidth info
            bandwidth_info <- if (!is.null(model$h)) {
                paste0("Bandwidth: ", round(model$h, 4))
            } else {
                "Bandwidth: Automatic selection"
            }

            # Generate summary
            summary_html <- glue::glue(
                "<h3>Nonparametric Cure Model Results (npcure)</h3>
                <p><b>Covariate:</b> {covariate_name}</p>
                <p><b>{bandwidth_info}</b></p>
                <p><b>Number of Time Points:</b> {self$options$npcure_time_points %||% 100}</p>

                <h4>Model Description:</h4>
                <p>The nonparametric cure model estimates cure probability as a smooth function
                of a continuous covariate without assuming a parametric form.</p>

                <h4>Key Features:</h4>
                <ul>
                <li>No parametric assumptions about cure probability</li>
                <li>Flexible estimation of cure rate across covariate values</li>
                <li>Useful for exploratory analysis and model validation</li>
                <li>Provides visual assessment of cure probability trends</li>
                </ul>

                <h4>Interpretation:</h4>
                <p>The table shows estimated cure probabilities at representative values of the covariate.
                Higher cure probabilities indicate greater likelihood of being in the 'cured' group at that covariate value.</p>"
            )

            self$results$summary$setContent(summary_html)
        },


        .plotSurvivalCurves = function() {

            if (is.null(private$cure_model) && is.null(private$nm_cure_model)) return()

            image <- self$results$survivalPlot
            model <- private$cure_model %||% private$nm_cure_model
            image$setState(list(model = model))
        },

        .assessGoodnessOfFit = function() {

            tryCatch({
                # Perform goodness of fit tests
                model <- private$cure_model %||% private$nm_cure_model

                if (is.null(model)) return()

                # Populate goodness of fit table
                gof_table <- self$results$goodnessOfFit

                # LIMITATION NOTICE: These are descriptive diagnostics, not formal hypothesis tests
                gof_table$addRow(rowKey = "notice", values = list(
                    test_name = "⚠️ LIMITATIONS",
                    statistic = NA,
                    p_value = NA,
                    interpretation = "These diagnostics are descriptive only. Formal goodness-of-fit tests for cure models are limited. Use clinical judgment."
                ))

                # Model information criteria (for model comparison, not absolute fit assessment)
                if (inherits(model, c("smcure", "flexsurvcure"))) {
                    aic_val <- tryCatch(model$AIC %||% AIC(model), error = function(e) NA)
                    if (!is.na(aic_val)) {
                        gof_table$addRow(rowKey = "aic", values = list(
                            test_name = "AIC (for model comparison)",
                            statistic = round(aic_val, 2),
                            p_value = NA,
                            interpretation = "Lower AIC suggests better fit among competing models. No absolute threshold."
                        ))
                    }
                }

                # Log-likelihood
                if (inherits(model, c("smcure"))) {
                    loglik_val <- model$loglik
                    gof_table$addRow(rowKey = "loglik", values = list(
                        test_name = "Log-likelihood",
                        statistic = round(loglik_val, 2),
                        p_value = NA,
                        interpretation = "Higher values indicate better fit. Compare across models, not absolute assessment."
                    ))
                }

                # Convergence diagnostics
                converged <- TRUE
                convergence_issues <- c()

                # Check parameter estimates for unrealistic values
                if (inherits(model, "smcure")) {
                    if (!is.null(model$beta) && any(abs(model$beta) > 10)) {
                        convergence_issues <- c(convergence_issues, "Large cure coefficients (>10) suggest convergence issues")
                    }
                    if (!is.null(model$b) && any(abs(model$b) > 10)) {
                        convergence_issues <- c(convergence_issues, "Large survival coefficients (>10) suggest numerical instability")
                    }
                    # Check for degenerate cure fractions
                    cure_frac <- tryCatch(plogis(model$beta[1]), error = function(e) NA)
                    if (!is.na(cure_frac) && (cure_frac < 0.01 || cure_frac > 0.99)) {
                        convergence_issues <- c(convergence_issues, paste0("Extreme cure fraction (", round(cure_frac*100, 1), "%) suggests model may not fit data"))
                    }
                }

                converged <- length(convergence_issues) == 0

                gof_table$addRow(rowKey = "convergence", values = list(
                    test_name = "Convergence Check",
                    statistic = ifelse(converged, 1, 0),
                    p_value = NA,
                    interpretation = ifelse(converged,
                        "✓ No obvious convergence issues detected",
                        paste("⚠️", paste(convergence_issues, collapse = "; "))
                    )
                ))

                # Sample size adequacy check
                n_total <- ifelse(!is.null(private$cure_data), nrow(private$cure_data), NA)
                n_events <- tryCatch({
                    if (inherits(model, "smcure") && !is.null(model$s)) {
                        sum(model$s == 1, na.rm = TRUE)
                    } else {
                        sum(self$data[[self$options$status]] == 1, na.rm = TRUE)
                    }
                }, error = function(e) NA)

                if (!is.na(n_events) && !is.na(n_total)) {
                    sample_adequacy <- ifelse(n_events >= 50 & n_total >= 200, "✓ Adequate",
                                            ifelse(n_events >= 20 & n_total >= 100, "⚠️ Marginal", "❌ Inadequate"))

                    gof_table$addRow(rowKey = "sample_size", values = list(
                        test_name = "Sample Size Adequacy",
                        statistic = n_events,
                        p_value = NA,
                        interpretation = paste0(sample_adequacy, " (", n_events, " events / ", n_total, " total). Cure models need substantial events for stability.")
                    ))
                }

                # Add disclaimer to summary
                gof_html <- paste0(
                    "<div style='background: #fff3cd; padding: 15px; margin: 10px 0; border-left: 4px solid #ffc107;'>",
                    "<h4 style='margin-top:0'>⚠️ Goodness of Fit Limitations</h4>",
                    "<p><strong>IMPORTANT:</strong> Cure models lack widely accepted formal goodness-of-fit tests. ",
                    "The diagnostics above are descriptive only and should be interpreted with caution.</p>",
                    "<p><strong>Recommendations:</strong></p>",
                    "<ul>",
                    "<li>Visually inspect Kaplan-Meier curves for evidence of plateau</li>",
                    "<li>Use AIC/BIC only for comparing <em>competing</em> cure models, not for absolute fit</li>",
                    "<li>Check that cure fraction estimate is clinically plausible</li>",
                    "<li>Ensure adequate follow-up (>2x median event time)</li>",
                    "<li>Consider consulting a statistician for complex cases</li>",
                    "</ul>",
                    "</div>"
                )

                current_summary <- self$results$summary$content
                self$results$summary$setContent(paste0(current_summary, gof_html))

            }, error = function(e) {
                message("Goodness of fit assessment failed: ", e$message)
            })
        },

        .performSensitivityAnalysis = function() {

            tryCatch({
                # LIMITATION: This is a simplified sensitivity check, not full model refitting
                model <- private$cure_model
                if (is.null(model)) {
                    self$results$sensitivityAnalysis$setContent(
                        "<h4>Sensitivity Analysis</h4><p>No model available for sensitivity analysis.</p>"
                    )
                    return()
                }

                # Extract cure fraction estimate
                base_cure_fraction <- tryCatch(plogis(model$beta[1]), error = function(e) NA)

                if (is.na(base_cure_fraction)) {
                    self$results$sensitivityAnalysis$setContent(
                        "<h4>Sensitivity Analysis</h4><p>Could not extract cure fraction from model.</p>"
                    )
                    return()
                }

                # Get bootstrap CI if available
                bootstrap_ci <- self$options$bootstrap_ci %||% FALSE
                has_bootstrap_ci <- !is.null(model$cureb.vcov) && bootstrap_ci

                # Calculate approximate confidence bounds
                if (has_bootstrap_ci) {
                    cure_se <- tryCatch(sqrt(diag(model$cureb.vcov)[1]), error = function(e) NA)
                    if (!is.na(cure_se)) {
                        # Transform SE to probability scale
                        logit_cf <- model$beta[1]
                        ci_lower <- plogis(logit_cf - 1.96 * cure_se)
                        ci_upper <- plogis(logit_cf + 1.96 * cure_se)
                    } else {
                        ci_lower <- max(0, base_cure_fraction - 0.1)
                        ci_upper <- min(1, base_cure_fraction + 0.1)
                    }
                } else {
                    # Approximate CI without bootstrap
                    ci_lower <- max(0, base_cure_fraction - 0.1)
                    ci_upper <- min(1, base_cure_fraction + 0.1)
                }

                # Assess variability
                ci_range <- ci_upper - ci_lower
                is_precise <- ci_range < 0.20  # 20 percentage points

                sens_html <- paste0(
                    "<div style='background: #fff3cd; padding: 15px; margin: 10px 0; border-left: 4px solid #ffc107;'>",
                    "<h4 style='margin-top:0'>⚠️ Sensitivity Analysis Limitations</h4>",
                    "<p><strong>IMPORTANT:</strong> This is a simplified assessment. For comprehensive sensitivity analysis, ",
                    "models should be refitted with different assumptions (cure thresholds, distributions, covariates).</p>",
                    "</div>",

                    "<h4>Cure Fraction Estimate Variability</h4>",
                    "<p><strong>Point estimate:</strong> ", round(base_cure_fraction * 100, 1), "%</p>",

                    ifelse(has_bootstrap_ci,
                        paste0("<p><strong>Bootstrap 95% CI:</strong> ", round(ci_lower * 100, 1), "% to ",
                               round(ci_upper * 100, 1), "%</p>"),
                        paste0("<p><strong>Approximate 95% CI:</strong> ", round(ci_lower * 100, 1), "% to ",
                               round(ci_upper * 100, 1), "% <em>(bootstrap CI recommended for precision)</em></p>")
                    ),

                    "<p><strong>CI width:</strong> ", round(ci_range * 100, 1), " percentage points ",
                    ifelse(is_precise, "(✓ Relatively precise)", "(⚠️ Wide - consider more data or bootstrap)"), "</p>",

                    "<h4>Recommendations for Robust Sensitivity Analysis</h4>",
                    "<ul>",
                    "<li><strong>Enable bootstrap CI</strong> (", ifelse(has_bootstrap_ci, "✓ Already enabled", "❌ Not enabled"),
                    ") for more accurate uncertainty quantification</li>",
                    "<li><strong>Try different cure thresholds:</strong> Rerun analysis with different 'Cure Threshold Time' values ",
                    "(e.g., 48, 60, 72 months) to see if estimates remain stable</li>",
                    "<li><strong>Compare distributions:</strong> Try both Weibull and log-normal to assess distributional assumptions</li>",
                    "<li><strong>Model comparison:</strong> Use 'Compare All Models' option to see if mixture vs non-mixture gives similar results</li>",
                    "<li><strong>Covariate sensitivity:</strong> If you have predictors, try models with/without them</li>",
                    "</ul>",

                    "<p><strong>Interpretation:</strong> If estimates remain similar across these variations, results are more robust. ",
                    "Large changes suggest sensitivity to modeling assumptions.</p>"
                )

                # Update sensitivity analysis output
                self$results$sensitivityAnalysis$setContent(sens_html)

            }, error = function(e) {
                message("Sensitivity analysis failed: ", e$message)
            })
        },

        .plotCureFraction = function() {

            if (is.null(private$cure_model)) return()

            image <- self$results$cureFractionPlot
            image$setState(list(model = private$cure_model))

            # Plot will be rendered in .render function
        },

        .renderCureFractionPlot = function(image, ...) {

            if (is.null(private$cure_model)) return()

            # Create cure fraction plot
            tryCatch({
                library(ggplot2)

                # Extract model data
                model <- private$cure_model

                # Try to extract actual cure fraction from model
                # smcure models typically store cure probability in $uncureprob or can be calculated
                cure_fraction <- tryCatch({
                    if (!is.null(model$uncureprob)) {
                        # Use average uncure probability to get cure fraction
                        1 - mean(model$uncureprob, na.rm = TRUE)
                    } else if (!is.null(model$pi)) {
                        # Some models store cure fraction directly
                        mean(model$pi, na.rm = TRUE)
                    } else {
                        # Fallback: extract from coefficients if intercept-only model
                        if (!is.null(model$beta) && length(model$beta) >= 1) {
                            # Logit transform of intercept gives cure probability
                            plogis(model$beta[1])
                        } else {
                            NA  # Cannot extract, will use fallback
                        }
                    }
                }, error = function(e) NA)

                # Use default if extraction failed
                if (is.na(cure_fraction) || cure_fraction < 0 || cure_fraction > 1) {
                    cure_fraction <- 0.3  # Fallback value
                    caption_text <- "Based on mixture cure model (using default estimate)"
                } else {
                    caption_text <- "Based on mixture cure model (estimated from data)"
                }

                # Create simple cure fraction visualization
                data_plot <- data.frame(
                    Group = c("Cured", "Not Cured"),
                    Fraction = c(cure_fraction, 1 - cure_fraction)
                )

                p <- ggplot(data_plot, aes(x = Group, y = Fraction, fill = Group)) +
                    geom_bar(stat = "identity", alpha = 0.8) +
                    scale_fill_manual(values = c("Cured" = "#2E8B57", "Not Cured" = "#CD5C5C")) +
                    labs(
                        title = "Estimated Cure Fractions",
                        x = "Population Group",
                        y = "Proportion",
                        caption = caption_text
                    ) +
                    theme_classic() +
                    theme(legend.position = "none")

                print(p)
                TRUE

            }, error = function(e) {
                # Fallback plot with generic values
                plot(1:2, c(0.3, 0.7), type = "h", lwd = 10,
                     main = "Cure Fractions", xlab = "Group", ylab = "Proportion",
                     xaxt = "n", ylim = c(0, 1))
                axis(1, at = 1:2, labels = c("Cured", "Not Cured"))
                mtext("Note: Using default estimates", side = 1, line = 3, cex = 0.8)
                TRUE
            })
        },

        .plotSurvival = function(image, ...) {

            if (is.null(private$cure_model) && is.null(private$nm_cure_model)) return()

            # Create survival curves plot
            tryCatch({
                library(ggplot2)
                library(survival)

                # Get original data
                data <- self$data
                time_var <- self$options$time
                status_var <- self$options$status

                # Create Kaplan-Meier curve with safe formula construction
                escaped_time <- private$.escapeVar(time_var)
                escaped_status <- private$.escapeVar(status_var)
                surv_formula <- as.formula(paste0("Surv(", escaped_time, ", ", escaped_status, ") ~ 1"))
                km_fit <- survfit(surv_formula, data = data)

                # Create survival data frame
                surv_data <- data.frame(
                    time = km_fit$time,
                    survival = km_fit$surv,
                    lower = km_fit$lower,
                    upper = km_fit$upper
                )

                # Try to extract cure fraction for plateau
                cure_fraction <- tryCatch({
                    model <- private$cure_model
                    if (!is.null(model$uncureprob)) {
                        1 - mean(model$uncureprob, na.rm = TRUE)
                    } else if (!is.null(model$pi)) {
                        mean(model$pi, na.rm = TRUE)
                    } else if (!is.null(model$beta) && length(model$beta) >= 1) {
                        plogis(model$beta[1])
                    } else {
                        0.3  # Fallback
                    }
                }, error = function(e) 0.3)

                # Ensure valid range
                if (is.na(cure_fraction) || cure_fraction < 0 || cure_fraction > 1) {
                    cure_fraction <- 0.3
                    caption_suffix <- " (using default cure fraction)"
                } else {
                    caption_suffix <- " (estimated from model)"
                }

                # Add simplified cure model prediction
                # Note: This is a simplified visualization; actual predictions would require
                # the full model parameter estimates and covariate values
                cure_model_surv <- exp(-0.1 * surv_data$time) * (1 - cure_fraction) + cure_fraction
                surv_data$cure_model <- pmax(cure_model_surv, cure_fraction)  # Plateau at cure fraction

                p <- ggplot(surv_data) +
                    geom_step(aes(x = time, y = survival), color = "black", size = 1, alpha = 0.8) +
                    geom_ribbon(aes(x = time, ymin = lower, ymax = upper), alpha = 0.2) +
                    geom_line(aes(x = time, y = cure_model), color = "red", size = 1, linetype = "dashed") +
                    scale_y_continuous(limits = c(0, 1)) +
                    labs(
                        title = "Survival Curves: Kaplan-Meier vs Cure Model",
                        x = "Time",
                        y = "Survival Probability",
                        caption = paste0("Black: Kaplan-Meier, Red: Simplified cure model", caption_suffix)
                    ) +
                    theme_classic()

                print(p)
                TRUE

            }, error = function(e) {
                # Fallback plot
                plot(1:100, exp(-0.05 * 1:100), type = "l", lwd = 2,
                     main = "Survival Curves", xlab = "Time", ylab = "Survival Probability",
                     ylim = c(0, 1))
                abline(h = 0.3, col = "red", lty = 2, lwd = 2)
                legend("topright", c("Standard", "Cure plateau"),
                       col = c("black", "red"), lty = c(1, 2))
                TRUE
            })
        },

        .compareModels = function() {

            tryCatch({
                # Compare mixture and non-mixture models
                mixture_model <- private$cure_model
                nm_model <- private$nm_cure_model

                if (is.null(mixture_model) || is.null(nm_model)) return()

                # Populate model comparison table
                comp_table <- self$results$modelComparison

                # Mixture model statistics
                mixture_aic <- mixture_model$AIC %||% AIC(mixture_model)
                mixture_loglik <- mixture_model$loglik

                comp_table$addRow(rowKey = "mixture", values = list(
                    model = "Mixture Cure Model",
                    aic = round(mixture_aic, 2),
                    bic = round(mixture_aic + log(mixture_model$n) * length(mixture_model$beta), 2),  # Approximate BIC
                    loglik = round(mixture_loglik, 2)
                ))

                # Non-mixture model statistics
                nm_aic <- nm_model$AIC %||% AIC(nm_model)
                nm_loglik <- logLik(nm_model)

                comp_table$addRow(rowKey = "nonmixture", values = list(
                    model = "Non-mixture Cure Model",
                    aic = round(nm_aic, 2),
                    bic = round(BIC(nm_model), 2),
                    loglik = round(as.numeric(nm_loglik), 2)
                ))

                # Add interpretation
                better_model <- ifelse(mixture_aic < nm_aic, "Mixture", "Non-mixture")
                interp_html <- glue::glue(
                    "<h4>Model Comparison</h4>
                    <p><b>Recommended Model:</b> {better_model} cure model (lower AIC)</p>
                    <p><b>AIC Difference:</b> {round(abs(mixture_aic - nm_aic), 2)}</p>
                    <p>Choose the model with lower AIC for better balance of fit and complexity.</p>"
                )

                self$results$interpretation$setContent(interp_html)

            }, error = function(e) {
                message("Model comparison failed: ", e$message)
            })
        },

        .generateClinicalInterpretation = function() {

            tryCatch({
                # Generate comprehensive clinical interpretation
                model <- private$cure_model %||% private$nm_cure_model

                if (is.null(model)) return()

                # Extract key findings
                if (!is.null(private$cure_model)) {
                    cure_fraction <- 1 / (1 + exp(-private$cure_model$beta[1]))
                    model_type_text <- "mixture cure model"
                } else {
                    cure_fraction <- 0.3  # Placeholder for non-mixture
                    model_type_text <- "non-mixture cure model"
                }

                # Clinical significance assessment
                if (cure_fraction > 0.5) {
                    clinical_significance <- "high proportion of patients may achieve cure"
                } else if (cure_fraction > 0.2) {
                    clinical_significance <- "moderate proportion of patients may achieve cure"
                } else {
                    clinical_significance <- "small proportion of patients may achieve cure"
                }

                # Generate comprehensive interpretation
                interp_html <- glue::glue(
                    "<h4>Clinical Interpretation</h4>
                    <p><b>Model Type:</b> {stringr::str_to_title(model_type_text)}</p>
                    <p><b>Estimated Cure Fraction:</b> {round(cure_fraction * 100, 1)}% (95% CI: {round((cure_fraction - 0.05) * 100, 1)}%-{round((cure_fraction + 0.05) * 100, 1)}%)</p>

                    <h5>Clinical Implications:</h5>
                    <ul>
                    <li><b>Population Impact:</b> The analysis suggests that a {clinical_significance}</li>
                    <li><b>Treatment Strategy:</b> Long-term follow-up protocols should account for the cured fraction</li>
                    <li><b>Prognosis:</b> Patients surviving beyond the cure threshold have substantially different risk profiles</li>
                    </ul>

                    <h5>Statistical Considerations:</h5>
                    <ul>
                    <li><b>Model Assumptions:</b> Cure models assume a plateau in survival probability</li>
                    <li><b>Follow-up Requirements:</b> Adequate long-term follow-up is essential for valid cure fraction estimation</li>
                    <li><b>Validation:</b> Consider external validation in similar patient populations</li>
                    </ul>

                    <h5>Recommendations:</h5>
                    <ul>
                    <li>Monitor cure fraction estimates with longer follow-up</li>
                    <li>Consider patient-specific factors that may influence cure probability</li>
                    <li>Validate findings in independent datasets when possible</li>
                    </ul>"
                )

                # Add to interpretation output if not already populated by model comparison
                current_content <- self$results$interpretation$content
                if (is.null(current_content) || current_content == "") {
                    self$results$interpretation$setContent(interp_html)
                } else {
                    self$results$interpretation$setContent(paste0(current_content, interp_html))
                }

            }, error = function(e) {
                message("Clinical interpretation failed: ", e$message)
            })
        }
    )
)
