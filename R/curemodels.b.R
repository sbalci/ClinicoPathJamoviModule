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

        # Cure fraction + CI storage (populated by fitting functions)
        .cure_fraction = NULL,
        .cure_ci_lower = NULL,
        .cure_ci_upper = NULL,
        .cure_ci_method = NULL,
        .interpretation_html = "",  # accumulates interpretation content

        # HTML notice system (replaces insert(999, Notice) pattern)
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                return()
            }

            typeStyles <- list(
                ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5"),
                STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74"),
                WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047"),
                INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd")
            )

            # Separate errors from warnings/info
            error_html <- ""
            warn_html <- ""

            for (notice in private$.noticeList) {
                style <- typeStyles[[notice$type]] %||% typeStyles$INFO

                block <- paste0(
                    "<div style='background-color: ", style$bgcolor, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: ", style$color, ";'>",
                    notice$title, "</strong><br>",
                    "<span style='color: #374151;'>", notice$content, "</span>",
                    "</div>"
                )

                if (notice$type == "ERROR") {
                    error_html <- paste0(error_html, block)
                } else {
                    warn_html <- paste0(warn_html, block)
                }
            }

            if (nchar(error_html) > 0) {
                self$results$errors$setContent(paste0("<div>", error_html, "</div>"))
            }
            if (nchar(warn_html) > 0) {
                self$results$warnings$setContent(paste0("<div>", warn_html, "</div>"))
            }
        },

        .init = function() {
            # No package checks here - they cause serialization issues with Notice objects.
            # Package availability is checked in each fitting function with proper HTML notices.
        },

        .validateInputData = function(data, time_var, status_var) {

            # Use raw variable names for data[[ ]] access (NOT escaped/backticked)
            time_col <- data[[time_var]]
            status_col <- data[[status_var]]

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

            # Reset state for each run
            private$.noticeList <- list()
            private$.interpretation_html <- ""

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

                    "<p style='margin-top: 15px;'><em>Tip: Start with Mixture Cure Model, then compare with other types if needed. Ensure adequate follow-up time (at least 2-3x median event time) for reliable cure fraction estimates.</em></p>",
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

            # Clean data - use raw variable names for data access
            analysis_vars <- c(time_var, status_var, predictors)
            clean_data <- data[complete.cases(data[, analysis_vars]), analysis_vars]

            if (nrow(clean_data) < 30) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Insufficient Data",
                    content = paste0(
                        "Found ", nrow(clean_data), " complete cases, but minimum 30 required. ",
                        "Please check for missing values in survival time, event status, or predictor variables."
                    )
                )
                private$.renderNotices()
                return()
            }

            # Store clean data for diagnostics
            private$cure_data <- clean_data

            # Validate input data
            validation_result <- tryCatch({
                private$.validateInputData(clean_data, time_var, status_var)
                TRUE
            }, warning = function(w) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Data Validation Warning",
                    content = w$message
                )
                TRUE
            }, error = function(e) {
                private$.addNotice(
                    type = "ERROR",
                    title = "Data Validation Error",
                    content = e$message
                )
                FALSE
            })

            if (!validation_result) {
                private$.renderNotices()
                return()
            }

            # Clinical data quality checks
            status_col <- clean_data[[status_var]]
            n_events <- if (is.factor(status_col)) {
                sum(as.numeric(status_col) == 2, na.rm = TRUE)
            } else {
                sum(status_col == 1, na.rm = TRUE)
            }
            event_rate <- n_events / nrow(clean_data)

            # EPV (events per variable) check
            if (length(predictors) > 0 && n_events / length(predictors) < 10) {
                private$.addNotice(
                    type = "STRONG_WARNING",
                    title = "Low Events Per Variable",
                    content = sprintf(
                        "Only %.1f events per predictor (%d events / %d predictors). Recommend >= 10 EPV for stable cure model estimates. Consider reducing the number of predictors.",
                        n_events / length(predictors), n_events, length(predictors))
                )
            }

            # Prevalence extreme check
            if (event_rate < 0.05) {
                private$.addNotice(
                    type = "STRONG_WARNING",
                    title = "Very Low Event Rate",
                    content = sprintf(
                        "Event rate is only %.1f%% (%d/%d). Cure model estimates may be unstable with very few events. Ensure adequate follow-up and consider whether a cure model is appropriate.",
                        event_rate * 100, n_events, nrow(clean_data))
                )
            } else if (event_rate > 0.95) {
                private$.addNotice(
                    type = "STRONG_WARNING",
                    title = "Very High Event Rate",
                    content = sprintf(
                        "Event rate is %.1f%% (%d/%d). Nearly all patients experienced the event, suggesting cure may not be plausible. Consider standard survival analysis instead.",
                        event_rate * 100, n_events, nrow(clean_data))
                )
            }

            # Add informational notices for specialized model types
            if (model_type == "cure") {
                private$.addNotice(
                    type = "INFO",
                    title = "cuRe Model Selected",
                    content = paste0(
                        "The cuRe model incorporates background mortality rates. ",
                        "Requires the cuRe package. Optionally uses a background hazard variable (e.g., from life tables). ",
                        "Best suited for long-term cancer survival studies."
                    )
                )
            }

            if (model_type == "npcure") {
                private$.addNotice(
                    type = "INFO",
                    title = "Nonparametric Cure Model Selected",
                    content = paste0(
                        "The npcure model uses kernel smoothing without distributional assumptions. ",
                        "Works with a single continuous covariate only. Sensitive to bandwidth selection. ",
                        "Requires larger sample sizes for stable estimates."
                    )
                )
            }

            if (self$options$bootstrap_ci %||% FALSE) {
                n_bootstrap <- self$options$n_bootstrap %||% 1000
                if (n_bootstrap >= 500) {
                    private$.addNotice(
                        type = "INFO",
                        title = "Bootstrap Confidence Intervals",
                        content = paste0(
                            "Bootstrap resampling enabled with ", n_bootstrap, " iterations. ",
                            "This may take several minutes. Please be patient."
                        )
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

            # Set plot state (render functions are called by jamovi when visible)
            if (self$options$plot_cure_fraction) {
                private$.plotCureFraction()
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

            # Render all accumulated notices as HTML
            private$.renderNotices()
        },

        # ============================================================
        # MIXTURE CURE MODEL (smcure)
        # ============================================================
        .fitMixtureCureModel = function(data, time_var, status_var, predictors) {

            tryCatch({
                if (!requireNamespace('smcure', quietly = TRUE)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Package Missing: smcure",
                        content = "The smcure package is required for mixture cure models. Install using: install.packages('smcure')"
                    )
                    return()
                }

                # Build formulas using jmvcore::composeTerm for formula safety
                if (length(predictors) > 0) {
                    escaped_predictors <- sapply(predictors, jmvcore::composeTerm)
                    escaped_time <- jmvcore::composeTerm(time_var)
                    escaped_status <- jmvcore::composeTerm(status_var)

                    surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")
                    surv_formula <- as.formula(paste(surv_response, "~", paste(escaped_predictors, collapse = " + ")))
                    cure_formula <- as.formula(paste("~", paste(escaped_predictors, collapse = " + ")))
                } else {
                    escaped_time <- jmvcore::composeTerm(time_var)
                    escaped_status <- jmvcore::composeTerm(status_var)

                    surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")
                    surv_formula <- as.formula(paste(surv_response, "~ 1"))
                    cure_formula <- ~ 1
                }

                # Get options
                cure_link <- self$options$cure_link %||% "logit"
                smcure_model_type <- self$options$smcure_model_type %||% "ph"
                bootstrap_ci <- self$options$bootstrap_ci %||% FALSE
                n_bootstrap <- self$options$n_bootstrap %||% 1000

                # FIX C1: Correct smcure::smcure() call
                # model = "ph" or "aft" (survival model type)
                # link = "logit"/"probit"/"cloglog" (cure probability link)
                # Var = TRUE/FALSE (compute variance via bootstrap)
                # nboot = number of bootstrap samples (only used when Var=TRUE)
                # smcure does NOT have a 'dist' parameter
                cure_model <- smcure::smcure(
                    formula = surv_formula,
                    cureform = cure_formula,
                    data = data,
                    model = smcure_model_type,
                    link = cure_link,
                    Var = bootstrap_ci,
                    nboot = if (bootstrap_ci) n_bootstrap else 0
                )

                # Extract and format results
                private$.formatMixtureCureResults(cure_model)

                # Store model for plotting
                private$cure_model <- cure_model

            }, error = function(e) {
                error_msg <- e$message

                if (grepl("convergence|iteration", error_msg, ignore.case = TRUE)) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Mixture Cure Model Convergence Failed",
                        content = paste0(
                            error_msg,
                            " -- Try: (1) different link function, (2) fewer predictors, ",
                            "(3) check for collinear variables, (4) ensure adequate sample size and follow-up."
                        )
                    )
                } else if (grepl("singular|matrix", error_msg, ignore.case = TRUE)) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Matrix Computation Error in Mixture Cure Model",
                        content = paste0(
                            error_msg,
                            " -- Try: (1) check for collinear predictors, ",
                            "(2) remove variables with little variation, (3) ensure sufficient sample size."
                        )
                    )
                } else {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Mixture Cure Model Error",
                        content = paste0("Fitting failed: ", error_msg)
                    )
                }
            })
        },

        # ============================================================
        # NON-MIXTURE CURE MODEL (flexsurvcure)
        # ============================================================
        .fitNonMixtureCureModel = function(data, time_var, status_var, predictors) {

            tryCatch({
                if (!requireNamespace('flexsurvcure', quietly = TRUE)) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Package Missing: flexsurvcure",
                        content = paste0(
                            "The flexsurvcure package is required for non-mixture cure models. ",
                            "Install using: install.packages('flexsurvcure'). ",
                            "Alternatively, use the Mixture Cure Model option."
                        )
                    )
                    return()
                }

                # Build formula
                escaped_time <- jmvcore::composeTerm(time_var)
                escaped_status <- jmvcore::composeTerm(status_var)
                surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")

                if (length(predictors) > 0) {
                    escaped_predictors <- sapply(predictors, jmvcore::composeTerm)
                    formula <- as.formula(paste(surv_response, "~", paste(escaped_predictors, collapse = " + ")))
                } else {
                    formula <- as.formula(paste(surv_response, "~ 1"))
                }

                # Map distribution names to flexsurvcure-compatible names
                surv_dist <- self$options$survival_dist %||% "weibull"
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

                # FIX C4: Extract results correctly from flexsurvcure
                private$.formatNonMixtureCureResults(nm_cure_model)

                # Store model
                private$nm_cure_model <- nm_cure_model

            }, error = function(e) {
                private$.addNotice(
                    type = "STRONG_WARNING",
                    title = "Non-mixture Cure Model Error",
                    content = paste0("Fitting failed: ", e$message)
                )
            })
        },

        # ============================================================
        # cuRe MODEL (with background mortality)
        # ============================================================
        .fitCuReModel = function(data, time_var, status_var, predictors) {

            tryCatch({
                if (!requireNamespace('cuRe', quietly = TRUE)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Package Missing: cuRe",
                        content = "The cuRe package is required. Install using: install.packages('cuRe')"
                    )
                    return()
                }

                # Build formula
                escaped_time <- jmvcore::composeTerm(time_var)
                escaped_status <- jmvcore::composeTerm(status_var)
                surv_response <- paste0("Surv(", escaped_time, ", ", escaped_status, ")")

                if (length(predictors) > 0) {
                    escaped_predictors <- sapply(predictors, jmvcore::composeTerm)
                    surv_formula <- as.formula(paste(surv_response, "~", paste(escaped_predictors, collapse = " + ")))
                } else {
                    surv_formula <- as.formula(paste(surv_response, "~ 1"))
                }

                # Get cuRe-specific options
                use_background <- self$options$use_background_mortality %||% FALSE
                background_var <- self$options$background_hazard_var
                surv_dist <- self$options$survival_dist %||% "weibull"
                cure_link <- self$options$cure_link %||% "logit"

                if (use_background && !is.null(background_var) && background_var != "") {
                    bg_hazard_vec <- data[[background_var]]

                    cure_model <- cuRe::fit.cure.model(
                        formula = surv_formula,
                        data = data,
                        bhazard = bg_hazard_vec,
                        dist = surv_dist,
                        link = cure_link
                    )
                } else {
                    cure_model <- cuRe::fit.cure.model(
                        formula = surv_formula,
                        data = data,
                        dist = surv_dist,
                        link = cure_link
                    )
                }

                private$.formatCuReResults(cure_model)
                private$cure_cure_model <- cure_model

            }, error = function(e) {
                error_msg <- e$message

                if (grepl("convergence", error_msg, ignore.case = TRUE)) {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "cuRe Model Convergence Failed",
                        content = paste0(
                            error_msg,
                            " -- Try: (1) different starting values, (2) check background hazard variable, ",
                            "(3) ensure adequate follow-up time."
                        )
                    )
                } else if (grepl("bhazard|background", error_msg, ignore.case = TRUE)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Background Mortality Variable Error",
                        content = paste0(
                            error_msg,
                            " -- Verify the background hazard variable exists, contains non-negative numeric values."
                        )
                    )
                } else {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "cuRe Model Error",
                        content = paste0("Fitting failed: ", error_msg)
                    )
                }
            })
        },

        # ============================================================
        # NONPARAMETRIC CURE MODEL (npcure)
        # ============================================================
        .fitNpCureModel = function(data, time_var, status_var, predictors) {

            tryCatch({
                if (!requireNamespace('npcure', quietly = TRUE)) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Package Missing: npcure",
                        content = "The npcure package is required. Install using: install.packages('npcure')"
                    )
                    return()
                }

                # npcure works with single continuous covariate
                covariate <- self$options$npcure_covariate

                if (is.null(covariate) || length(covariate) == 0) {
                    private$.addNotice(
                        type = "ERROR",
                        title = "Missing Covariate",
                        content = "npcure requires a single continuous covariate. Please select a covariate in the npcure options."
                    )
                    return()
                }

                # Get npcure-specific options
                bandwidth_option <- self$options$npcure_bandwidth %||% "auto"

                bandwidth <- switch(bandwidth_option,
                    "auto" = NULL,
                    "small" = 0.1,
                    "medium" = 0.3,
                    "large" = 0.5,
                    NULL
                )

                # Use raw variable names for data[[ ]] access
                time_vec <- data[[time_var]]
                status_vec <- data[[status_var]]
                covar_vec <- data[[covariate]]

                # Convert factor status to numeric if needed
                if (is.factor(status_vec)) {
                    status_vec <- as.numeric(status_vec) - 1
                }

                # FIX C3: Correct npcure::probcure() parameter names
                # x = covariate vector
                # t = time vector
                # d = event indicator vector
                # x0 = covariate values at which to estimate cure probability
                # (NOT: time=, status=, testimate=)

                # Generate covariate values for estimation
                covar_range <- range(covar_vec, na.rm = TRUE)
                n_points_opt <- self$options$npcure_time_points %||% 100
                n_points <- min(n_points_opt, length(unique(covar_vec)))
                x0_values <- seq(covar_range[1], covar_range[2], length.out = n_points)

                if (is.null(bandwidth)) {
                    np_cure_model <- npcure::probcure(
                        x = covar_vec,
                        t = time_vec,
                        d = status_vec,
                        x0 = x0_values,
                        conflevel = 0.95
                    )
                } else {
                    np_cure_model <- npcure::probcure(
                        x = covar_vec,
                        t = time_vec,
                        d = status_vec,
                        x0 = x0_values,
                        h = bandwidth,
                        conflevel = 0.95
                    )
                }

                private$.formatNpCureResults(np_cure_model, covariate)
                private$npcure_model <- np_cure_model

            }, error = function(e) {
                error_msg <- e$message

                if (grepl("bandwidth|h =", error_msg, ignore.case = TRUE)) {
                    private$.addNotice(
                        type = "WARNING",
                        title = "npcure Bandwidth Issue",
                        content = paste0(
                            error_msg,
                            " -- Try: (1) 'auto' bandwidth, (2) different manual bandwidth, ",
                            "(3) check covariate distribution."
                        )
                    )
                } else {
                    private$.addNotice(
                        type = "STRONG_WARNING",
                        title = "Nonparametric Cure Model Error",
                        content = paste0("Fitting failed: ", error_msg)
                    )
                }
            })
        },

        # ============================================================
        # FORMAT MIXTURE CURE RESULTS
        # ============================================================
        .formatMixtureCureResults = function(model) {

            results_table <- self$results$modelTable

            # FIX C2: In smcure return objects:
            #   $b     = cure probability (incidence/logistic) coefficients
            #   $beta  = latency (survival) coefficients
            #   $b_sd  = SE for cure coefficients (when Var=TRUE)
            #   $beta_sd = SE for survival coefficients (when Var=TRUE)

            cure_coef <- model$b        # Incidence (cure) coefficients
            surv_coef <- model$beta     # Latency (survival) coefficients

            bootstrap_ci <- self$options$bootstrap_ci %||% FALSE

            # --- Cure probability coefficients ---
            if (!is.null(cure_coef)) {
                cure_names <- names(cure_coef)
                if (is.null(cure_names)) {
                    cure_names <- if (length(cure_coef) == 1) "(Intercept)" else paste0("Param_", seq_along(cure_coef))
                }

                # SE: use b_sd from bootstrap if available
                if (bootstrap_ci && !is.null(model$b_sd)) {
                    cure_se <- model$b_sd
                } else {
                    cure_se <- rep(NA_real_, length(cure_coef))
                }

                for (i in seq_along(cure_coef)) {
                    se_i <- if (!is.na(cure_se[i]) && cure_se[i] > 0) cure_se[i] else NA_real_
                    z_i <- if (!is.na(se_i)) cure_coef[i] / se_i else NA_real_
                    p_i <- if (!is.na(z_i)) 2 * (1 - pnorm(abs(z_i))) else NA_real_
                    ci_lo <- if (!is.na(se_i)) cure_coef[i] - 1.96 * se_i else NA_real_
                    ci_hi <- if (!is.na(se_i)) cure_coef[i] + 1.96 * se_i else NA_real_

                    results_table$addRow(rowKey = paste0("cure_", i), values = list(
                        parameter = paste("Cure:", cure_names[i]),
                        estimate = cure_coef[i],
                        std_error = se_i,
                        z_value = z_i,
                        p_value = p_i,
                        ci_lower = ci_lo,
                        ci_upper = ci_hi
                    ))
                }
            }

            # --- Survival (latency) coefficients ---
            if (!is.null(surv_coef)) {
                surv_names <- names(surv_coef)
                if (is.null(surv_names)) {
                    surv_names <- paste0("Param_", seq_along(surv_coef))
                }

                if (bootstrap_ci && !is.null(model$beta_sd)) {
                    surv_se <- model$beta_sd
                } else {
                    surv_se <- rep(NA_real_, length(surv_coef))
                }

                for (i in seq_along(surv_coef)) {
                    se_i <- if (!is.na(surv_se[i]) && surv_se[i] > 0) surv_se[i] else NA_real_
                    z_i <- if (!is.na(se_i)) surv_coef[i] / se_i else NA_real_
                    p_i <- if (!is.na(z_i)) 2 * (1 - pnorm(abs(z_i))) else NA_real_
                    ci_lo <- if (!is.na(se_i)) surv_coef[i] - 1.96 * se_i else NA_real_
                    ci_hi <- if (!is.na(se_i)) surv_coef[i] + 1.96 * se_i else NA_real_

                    results_table$addRow(rowKey = paste0("surv_", i), values = list(
                        parameter = paste("Survival:", surv_names[i]),
                        estimate = surv_coef[i],
                        std_error = se_i,
                        z_value = z_i,
                        p_value = p_i,
                        ci_lower = ci_lo,
                        ci_upper = ci_hi
                    ))
                }
            }

            # --- Cure fraction from intercept ---
            # Cure fraction = inverse-logit of b[1] (incidence intercept)
            cure_fraction <- plogis(model$b[1])

            # Compute CI for cure fraction using delta method on logit scale
            if (bootstrap_ci && !is.null(model$b_sd) && length(model$b_sd) >= 1 && model$b_sd[1] > 0) {
                cure_se_logit <- model$b_sd[1]
                logit_cf <- model$b[1]
                cure_ci_lower <- plogis(logit_cf - 1.96 * cure_se_logit)
                cure_ci_upper <- plogis(logit_cf + 1.96 * cure_se_logit)
                ci_method <- "Bootstrap delta method (logit scale)"
            } else {
                # No bootstrap available - report NA rather than fabricating CI
                cure_ci_lower <- NA_real_
                cure_ci_upper <- NA_real_
                ci_method <- "Not available (enable bootstrap CI)"
            }

            # Store for use in other functions
            private$.cure_fraction <- cure_fraction
            private$.cure_ci_lower <- cure_ci_lower
            private$.cure_ci_upper <- cure_ci_upper
            private$.cure_ci_method <- ci_method

            # Populate cure fraction table
            cure_table <- self$results$cureTable

            # Get median survival for uncured
            uncured_median <- tryCatch({
                if (!is.null(model$Time) && !is.null(model$Status)) {
                    event_times <- model$Time[model$Status == 1]
                    if (length(event_times) > 0) {
                        round(median(event_times, na.rm = TRUE), 2)
                    } else {
                        NA_real_
                    }
                } else {
                    NA_real_
                }
            }, error = function(e) NA_real_)

            cure_table$addRow(rowKey = "overall", values = list(
                group = "Overall",
                cure_fraction = cure_fraction,
                cure_ci_lower = cure_ci_lower,
                cure_ci_upper = cure_ci_upper,
                uncured_median = uncured_median
            ))

            # Count events
            n_events <- tryCatch({
                if (!is.null(model$s)) sum(model$s == 1, na.rm = TRUE)
                else if (!is.null(model$Status)) sum(model$Status == 1, na.rm = TRUE)
                else NA
            }, error = function(e) NA)

            n_censored <- tryCatch({
                if (!is.null(model$s)) sum(model$s == 0, na.rm = TRUE)
                else if (!is.null(model$Status)) sum(model$Status == 0, na.rm = TRUE)
                else NA
            }, error = function(e) NA)

            n_total <- tryCatch({
                if (!is.null(model$n)) model$n
                else if (!is.null(model$s)) length(model$s)
                else nrow(private$cure_data)
            }, error = function(e) nrow(private$cure_data))

            smcure_type <- self$options$smcure_model_type %||% "ph"
            smcure_type_label <- if (smcure_type == "ph") "Proportional Hazards" else "Accelerated Failure Time"

            # Build CI text
            ci_text <- if (!is.na(cure_ci_lower) && !is.na(cure_ci_upper)) {
                paste0(" (95% CI: ", round(cure_ci_lower * 100, 1), "%-", round(cure_ci_upper * 100, 1), "%)")
            } else {
                " (CI not available; enable bootstrap for confidence intervals)"
            }

            summary_html <- paste0(
                "<h3>Mixture Cure Model Results</h3>",
                "<p><b>Estimated Cure Fraction:</b> ", round(cure_fraction * 100, 1), "%", ci_text, "</p>",
                "<p><b>Survival Model Type:</b> ", smcure_type_label, "</p>",
                "<p><b>Link Function:</b> ", self$options$cure_link %||% "logit", "</p>",
                "<p><b>Sample Size:</b> ", n_total, "</p>",
                "<p><b>Events:</b> ", n_events, "</p>",
                "<p><b>Censored:</b> ", n_censored, "</p>",
                "<h4>Interpretation:</h4>",
                "<ul>",
                "<li>Cure probability coefficients ($b) show factors associated with being cured</li>",
                "<li>Positive cure coefficients increase cure probability (on ", self$options$cure_link %||% "logit", " scale)</li>",
                "<li>Survival coefficients ($beta) apply only to the uncured fraction</li>",
                "</ul>"
            )

            self$results$summary$setContent(summary_html)
        },

        # ============================================================
        # FORMAT NON-MIXTURE CURE RESULTS (flexsurvcure)
        # ============================================================
        .formatNonMixtureCureResults = function(model) {

            results_table <- self$results$modelTable

            # FIX C4: Use model$res.t for parameter estimates
            # model$res.t is a matrix with columns: est, L95%, U95%
            # (and optionally se)
            res_matrix <- model$res.t

            if (!is.null(res_matrix) && nrow(res_matrix) > 0) {
                param_names <- rownames(res_matrix)

                for (i in seq_len(nrow(res_matrix))) {
                    est_val <- res_matrix[i, "est"]
                    ci_lo <- res_matrix[i, "L95%"]
                    ci_hi <- res_matrix[i, "U95%"]
                    # SE may or may not be present
                    se_val <- if ("se" %in% colnames(res_matrix)) res_matrix[i, "se"] else NA_real_
                    z_val <- if (!is.na(se_val) && se_val > 0) est_val / se_val else NA_real_
                    p_val <- if (!is.na(z_val)) 2 * (1 - pnorm(abs(z_val))) else NA_real_

                    results_table$addRow(rowKey = paste0("nm_", i), values = list(
                        parameter = param_names[i],
                        estimate = est_val,
                        std_error = se_val,
                        z_value = z_val,
                        p_value = p_val,
                        ci_lower = ci_lo,
                        ci_upper = ci_hi
                    ))
                }
            }

            # Extract cure fraction from theta parameter
            # In flexsurvcure, theta is the cure probability parameter
            # For non-mixture (promotion time) models, theta is on the probability scale
            cure_fraction <- NA_real_
            cure_ci_lower <- NA_real_
            cure_ci_upper <- NA_real_

            if ("theta" %in% rownames(res_matrix)) {
                theta_est <- res_matrix["theta", "est"]
                theta_lo <- res_matrix["theta", "L95%"]
                theta_hi <- res_matrix["theta", "U95%"]

                # theta is on probability scale for flexsurvcure
                cure_fraction <- theta_est
                cure_ci_lower <- theta_lo
                cure_ci_upper <- theta_hi
            }

            # Store for clinical interpretation
            private$.cure_fraction <- cure_fraction
            private$.cure_ci_lower <- cure_ci_lower
            private$.cure_ci_upper <- cure_ci_upper
            private$.cure_ci_method <- "Profile likelihood (flexsurvcure)"

            # Populate cure table if we have a cure fraction
            if (!is.na(cure_fraction)) {
                cure_table <- self$results$cureTable
                cure_table$addRow(rowKey = "overall", values = list(
                    group = "Overall",
                    cure_fraction = cure_fraction,
                    cure_ci_lower = cure_ci_lower,
                    cure_ci_upper = cure_ci_upper,
                    uncured_median = NA_real_
                ))
            }

            # Build summary
            loglik_val <- tryCatch(model$loglik, error = function(e) NA)
            aic_val <- tryCatch(model$AIC, error = function(e) NA)

            cure_pct_text <- if (!is.na(cure_fraction)) {
                paste0(round(cure_fraction * 100, 1), "%")
            } else {
                "See theta parameter in results table"
            }

            summary_html <- paste0(
                "<h3>Non-Mixture Cure Model Results</h3>",
                "<p><b>Distribution:</b> ", self$options$survival_dist %||% "weibull", "</p>",
                if (!is.na(loglik_val)) paste0("<p><b>Log-likelihood:</b> ", round(loglik_val, 2), "</p>") else "",
                if (!is.na(aic_val)) paste0("<p><b>AIC:</b> ", round(aic_val, 2), "</p>") else "",
                "<p><b>Estimated Cure Fraction (theta):</b> ", cure_pct_text, "</p>",
                "<h4>Model Description:</h4>",
                "<p>Non-mixture (promotion time) cure models assume the entire population follows the same ",
                "survival distribution with cure as a limiting probability as time approaches infinity. ",
                "The theta parameter directly represents the cure probability.</p>"
            )

            self$results$summary$setContent(summary_html)
        },

        # ============================================================
        # FORMAT cuRe RESULTS
        # ============================================================
        .formatCuReResults = function(model) {

            model_summary <- tryCatch(summary(model), error = function(e) NULL)
            results_table <- self$results$modelTable

            if (!is.null(model_summary) && !is.null(model_summary$coef)) {
                coef_matrix <- model_summary$coef

                for (i in seq_len(nrow(coef_matrix))) {
                    est_val <- coef_matrix[i, "Estimate"]
                    std_err <- if ("Std. Error" %in% colnames(coef_matrix)) coef_matrix[i, "Std. Error"] else NA_real_
                    z_val <- if (!is.na(std_err) && std_err > 0) est_val / std_err else NA_real_

                    p_val <- if ("Pr(>|z|)" %in% colnames(coef_matrix)) {
                        coef_matrix[i, "Pr(>|z|)"]
                    } else if (!is.na(z_val)) {
                        2 * (1 - pnorm(abs(z_val)))
                    } else {
                        NA_real_
                    }

                    ci_lo <- if (!is.na(std_err)) est_val - 1.96 * std_err else NA_real_
                    ci_hi <- if (!is.na(std_err)) est_val + 1.96 * std_err else NA_real_

                    results_table$addRow(rowKey = paste0("cure_", i), values = list(
                        parameter = rownames(coef_matrix)[i],
                        estimate = est_val,
                        std_error = std_err,
                        z_value = z_val,
                        p_value = p_val,
                        ci_lower = ci_lo,
                        ci_upper = ci_hi
                    ))
                }
            }

            # Try to extract cure fraction from cuRe model
            cure_fraction <- tryCatch({
                if (!is.null(model$cure_fraction)) {
                    model$cure_fraction
                } else {
                    NA_real_
                }
            }, error = function(e) NA_real_)

            if (!is.na(cure_fraction)) {
                private$.cure_fraction <- cure_fraction
                private$.cure_ci_lower <- NA_real_
                private$.cure_ci_upper <- NA_real_
                private$.cure_ci_method <- "cuRe model estimate"

                cure_table <- self$results$cureTable
                cure_table$addRow(rowKey = "overall", values = list(
                    group = "Overall",
                    cure_fraction = cure_fraction,
                    cure_ci_lower = NA_real_,
                    cure_ci_upper = NA_real_,
                    uncured_median = NA_real_
                ))
            }

            background_note <- if ((self$options$use_background_mortality %||% FALSE) && !is.null(self$options$background_hazard_var)) {
                paste0("<p><b>Background Mortality:</b> Included (variable: ", self$options$background_hazard_var, ")</p>")
            } else {
                "<p><b>Background Mortality:</b> Not included</p>"
            }

            cure_fraction_text <- if (!is.na(cure_fraction)) {
                round(cure_fraction, 3)
            } else {
                "See model coefficients"
            }

            summary_html <- paste0(
                "<h3>cuRe Model Results</h3>",
                "<p><b>Distribution:</b> ", self$options$survival_dist %||% "weibull", "</p>",
                "<p><b>Link Function:</b> ", self$options$cure_link %||% "logit", "</p>",
                background_note,
                "<p><b>Estimated Cure Fraction:</b> ", cure_fraction_text, "</p>",
                "<h4>Model Description:</h4>",
                "<p>The cuRe model allows incorporation of background mortality rates from the general population. ",
                "This is particularly useful when analyzing cancer survival where patients may die from competing causes.</p>",
                "<h4>Key Features:</h4>",
                "<ul>",
                "<li>Accounts for background population mortality</li>",
                "<li>Separates excess hazard due to disease from background risk</li>",
                "<li>Provides more accurate cure fraction estimates in aging populations</li>",
                "</ul>"
            )

            self$results$summary$setContent(summary_html)
        },

        # ============================================================
        # FORMAT NONPARAMETRIC CURE RESULTS (npcure)
        # ============================================================
        .formatNpCureResults = function(model, covariate_name) {

            results_table <- self$results$modelTable

            # npcure::probcure returns:
            #   $q = cure probability estimates at x0 values
            #   $x0 = covariate values where estimated
            #   $h = bandwidth used
            #   $conf = list with $lower and $upper confidence limits

            if (!is.null(model$q) && !is.null(model$x0)) {
                cure_probs <- model$q
                covariate_vals <- model$x0

                # Show up to 10 representative points
                n_points <- min(10, length(cure_probs))
                indices <- round(seq(1, length(cure_probs), length.out = n_points))

                for (i in seq_along(indices)) {
                    idx <- indices[i]
                    ci_lo <- if (!is.null(model$conf) && !is.null(model$conf$lower)) model$conf$lower[idx] else NA_real_
                    ci_hi <- if (!is.null(model$conf) && !is.null(model$conf$upper)) model$conf$upper[idx] else NA_real_

                    results_table$addRow(rowKey = paste0("npcure_", i), values = list(
                        parameter = paste0(covariate_name, " = ", round(covariate_vals[idx], 2)),
                        estimate = cure_probs[idx],
                        std_error = NA_real_,
                        z_value = NA_real_,
                        p_value = NA_real_,
                        ci_lower = ci_lo,
                        ci_upper = ci_hi
                    ))
                }

                # Store overall cure fraction as mean across covariate values
                private$.cure_fraction <- mean(cure_probs, na.rm = TRUE)
                private$.cure_ci_lower <- NA_real_
                private$.cure_ci_upper <- NA_real_
                private$.cure_ci_method <- "Nonparametric kernel smoothing"
            }

            bandwidth_info <- if (!is.null(model$h)) {
                paste0("Bandwidth: ", round(model$h, 4))
            } else {
                "Bandwidth: Automatic selection"
            }

            summary_html <- paste0(
                "<h3>Nonparametric Cure Model Results (npcure)</h3>",
                "<p><b>Covariate:</b> ", covariate_name, "</p>",
                "<p><b>", bandwidth_info, "</b></p>",
                "<h4>Model Description:</h4>",
                "<p>The nonparametric cure model estimates cure probability as a smooth function ",
                "of a continuous covariate without assuming a parametric form.</p>",
                "<h4>Interpretation:</h4>",
                "<p>The table shows estimated cure probabilities at representative values of the covariate. ",
                "Higher cure probabilities indicate greater likelihood of being in the 'cured' group at that covariate value.</p>"
            )

            self$results$summary$setContent(summary_html)
        },

        # ============================================================
        # PLOT: Cure Fraction
        # ============================================================
        .plotCureFraction = function() {
            if (is.null(private$cure_model) && is.null(private$nm_cure_model) &&
                is.null(private$cure_cure_model) && is.null(private$npcure_model)) return()

            image <- self$results$cureFractionPlot

            # Store state as plain data.frame to avoid protobuf serialization issues
            cure_frac <- private$.cure_fraction
            if (is.null(cure_frac) || is.na(cure_frac)) cure_frac <- NA_real_

            image$setState(as.data.frame(list(
                cure_fraction = cure_frac,
                ci_lower = private$.cure_ci_lower %||% NA_real_,
                ci_upper = private$.cure_ci_upper %||% NA_real_
            )))
        },

        .renderCureFractionPlot = function(image, ggtheme, theme, ...) {

            state <- image$state
            if (is.null(state)) return(FALSE)

            cure_fraction <- state$cure_fraction[1]
            if (is.na(cure_fraction) || cure_fraction < 0 || cure_fraction > 1) return(FALSE)

            tryCatch({
                data_plot <- data.frame(
                    Group = factor(c("Cured", "Not Cured"), levels = c("Cured", "Not Cured")),
                    Fraction = c(cure_fraction, 1 - cure_fraction)
                )

                p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = Group, y = Fraction, fill = Group)) +
                    ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
                    ggplot2::scale_fill_manual(values = c("Cured" = "#2563eb", "Not Cured" = "#ea580c")) +
                    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                    ggplot2::labs(
                        title = "Estimated Cure Fractions",
                        subtitle = paste0("Cure: ", round(cure_fraction * 100, 1), "%"),
                        x = "Population Group",
                        y = "Proportion"
                    ) +
                    ggtheme +
                    ggplot2::theme(legend.position = "none")

                # Add CI error bars if available
                ci_lo <- state$ci_lower[1]
                ci_hi <- state$ci_upper[1]
                if (!is.na(ci_lo) && !is.na(ci_hi)) {
                    error_data <- data.frame(
                        Group = factor("Cured", levels = c("Cured", "Not Cured")),
                        Fraction = cure_fraction,
                        ci_lower = ci_lo,
                        ci_upper = ci_hi
                    )
                    p <- p + ggplot2::geom_errorbar(data = error_data,
                        ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                        width = 0.2, linewidth = 0.8)
                }

                print(p)
                TRUE

            }, error = function(e) FALSE)
        },

        # ============================================================
        # PLOT: Survival Curves
        # ============================================================
        .plotSurvival = function(image, ggtheme, theme, ...) {

            if (is.null(private$cure_data)) return(FALSE)

            tryCatch({
                data <- private$cure_data
                time_var <- self$options$time
                status_var <- self$options$status

                escaped_time <- jmvcore::composeTerm(time_var)
                escaped_status <- jmvcore::composeTerm(status_var)
                surv_formula <- as.formula(paste0("survival::Surv(", escaped_time, ", ", escaped_status, ") ~ 1"))
                km_fit <- survival::survfit(surv_formula, data = data)

                surv_data <- data.frame(
                    time = km_fit$time,
                    survival = km_fit$surv,
                    lower = km_fit$lower,
                    upper = km_fit$upper
                )

                cure_fraction <- private$.cure_fraction
                if (is.null(cure_fraction) || is.na(cure_fraction)) {
                    cure_fraction <- NULL
                }

                p <- ggplot2::ggplot(surv_data) +
                    ggplot2::geom_step(ggplot2::aes(x = time, y = survival), color = "black", linewidth = 1, alpha = 0.8) +
                    ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = lower, ymax = upper), alpha = 0.2) +
                    ggplot2::scale_y_continuous(limits = c(0, 1)) +
                    ggplot2::labs(
                        title = "Survival Curve with Cure Model Plateau",
                        x = "Time",
                        y = "Survival Probability"
                    ) +
                    ggtheme

                # Add cure fraction plateau line if available
                if (!is.null(cure_fraction) && cure_fraction > 0 && cure_fraction < 1) {
                    p <- p +
                        ggplot2::geom_hline(yintercept = cure_fraction, color = "red",
                                   linetype = "dashed", linewidth = 1) +
                        ggplot2::annotate("text", x = max(surv_data$time) * 0.7, y = cure_fraction + 0.04,
                                 label = paste0("Cure fraction: ", round(cure_fraction * 100, 1), "%"),
                                 color = "red", size = 3.5)
                }

                print(p)
                TRUE

            }, error = function(e) FALSE)
        },

        # ============================================================
        # GOODNESS OF FIT
        # ============================================================
        .assessGoodnessOfFit = function() {

            tryCatch({
                model <- private$cure_model %||% private$nm_cure_model %||% private$cure_cure_model

                if (is.null(model)) return()

                gof_table <- self$results$goodnessOfFit

                gof_table$addRow(rowKey = "notice", values = list(
                    test_name = "LIMITATIONS",
                    statistic = NA,
                    p_value = NA,
                    interpretation = "These diagnostics are descriptive only. Formal goodness-of-fit tests for cure models are limited."
                ))

                # AIC: only available from flexsurvcure, NOT from smcure
                if (inherits(model, "flexsurvcure")) {
                    aic_val <- tryCatch(model$AIC, error = function(e) NA)
                    if (!is.na(aic_val)) {
                        gof_table$addRow(rowKey = "aic", values = list(
                            test_name = "AIC (for model comparison)",
                            statistic = round(aic_val, 2),
                            p_value = NA,
                            interpretation = "Lower AIC suggests better fit among competing models."
                        ))
                    }

                    loglik_val <- tryCatch(model$loglik, error = function(e) NA)
                    if (!is.na(loglik_val)) {
                        gof_table$addRow(rowKey = "loglik", values = list(
                            test_name = "Log-likelihood",
                            statistic = round(loglik_val, 2),
                            p_value = NA,
                            interpretation = "Higher values indicate better fit. Compare across models."
                        ))
                    }
                }

                # smcure does NOT provide loglik or AIC - note this clearly
                if (inherits(model, "smcure")) {
                    gof_table$addRow(rowKey = "smcure_note", values = list(
                        test_name = "Note: smcure model",
                        statistic = NA,
                        p_value = NA,
                        interpretation = "smcure does not provide AIC/BIC or log-likelihood. Use non-mixture (flexsurvcure) for information criteria."
                    ))
                }

                # Convergence diagnostics for smcure
                if (inherits(model, "smcure")) {
                    convergence_issues <- c()

                    # FIX C2: $b = cure, $beta = survival
                    if (!is.null(model$b) && any(abs(model$b) > 10)) {
                        convergence_issues <- c(convergence_issues, "Large cure coefficients (>10) suggest convergence issues")
                    }
                    if (!is.null(model$beta) && any(abs(model$beta) > 10)) {
                        convergence_issues <- c(convergence_issues, "Large survival coefficients (>10) suggest numerical instability")
                    }

                    cure_frac <- tryCatch(plogis(model$b[1]), error = function(e) NA)
                    if (!is.na(cure_frac) && (cure_frac < 0.01 || cure_frac > 0.99)) {
                        convergence_issues <- c(convergence_issues,
                            paste0("Extreme cure fraction (", round(cure_frac * 100, 1), "%) suggests model may not fit well"))
                    }

                    converged <- length(convergence_issues) == 0

                    gof_table$addRow(rowKey = "convergence", values = list(
                        test_name = "Convergence Check",
                        statistic = if (converged) 1 else 0,
                        p_value = NA,
                        interpretation = if (converged) "No obvious convergence issues detected"
                                        else paste(convergence_issues, collapse = "; ")
                    ))
                }

                # Sample size adequacy
                n_total <- if (!is.null(private$cure_data)) nrow(private$cure_data) else NA
                n_events <- tryCatch({
                    status_col <- private$cure_data[[self$options$status]]
                    if (is.factor(status_col)) {
                        sum(as.numeric(status_col) == 2, na.rm = TRUE)
                    } else {
                        sum(status_col == 1, na.rm = TRUE)
                    }
                }, error = function(e) NA)

                if (!is.na(n_events) && !is.na(n_total)) {
                    adequacy <- if (n_events >= 50 && n_total >= 200) "Adequate"
                               else if (n_events >= 20 && n_total >= 100) "Marginal"
                               else "Inadequate"

                    gof_table$addRow(rowKey = "sample_size", values = list(
                        test_name = "Sample Size Adequacy",
                        statistic = n_events,
                        p_value = NA,
                        interpretation = paste0(adequacy, " (", n_events, " events / ", n_total, " total)")
                    ))
                }

            }, error = function(e) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Goodness of Fit",
                    content = paste0("Assessment failed: ", e$message)
                )
            })
        },

        # ============================================================
        # SENSITIVITY ANALYSIS
        # ============================================================
        .performSensitivityAnalysis = function() {

            tryCatch({
                cure_fraction <- private$.cure_fraction

                if (is.null(cure_fraction) || is.na(cure_fraction)) {
                    self$results$sensitivityAnalysis$setContent(
                        "<h4>Sensitivity Analysis</h4><p>No cure fraction estimate available for sensitivity analysis.</p>"
                    )
                    return()
                }

                ci_lower <- private$.cure_ci_lower
                ci_upper <- private$.cure_ci_upper
                ci_method <- private$.cure_ci_method %||% "Not specified"
                has_ci <- !is.null(ci_lower) && !is.na(ci_lower) && !is.null(ci_upper) && !is.na(ci_upper)

                if (has_ci) {
                    ci_range <- ci_upper - ci_lower
                    is_precise <- ci_range < 0.20
                } else {
                    ci_range <- NA
                    is_precise <- NA
                }

                # Use cure_threshold option for context
                cure_threshold <- self$options$cure_threshold %||% 60

                # Count patients surviving beyond threshold
                data <- private$cure_data
                time_var <- self$options$time
                status_var <- self$options$status
                n_beyond <- sum(data[[time_var]] >= cure_threshold, na.rm = TRUE)
                n_events_beyond <- sum(data[[time_var]] >= cure_threshold &
                    (if (is.factor(data[[status_var]])) as.numeric(data[[status_var]]) == 2
                     else data[[status_var]] == 1), na.rm = TRUE)

                sens_html <- paste0(
                    "<div style='background: #fff3cd; padding: 15px; margin: 10px 0; border-left: 4px solid #ffc107;'>",
                    "<h4 style='margin-top:0'>Sensitivity Analysis</h4>",
                    "<p><strong>Note:</strong> For comprehensive sensitivity analysis, refit models ",
                    "with different cure thresholds, distributions, and covariates.</p>",
                    "</div>",

                    "<h4>Cure Threshold Assessment</h4>",
                    "<p><strong>Cure threshold time:</strong> ", cure_threshold, " (user-specified)</p>",
                    "<p><strong>Patients surviving beyond threshold:</strong> ", n_beyond,
                    " (", round(100 * n_beyond / nrow(data), 1), "% of total)</p>",
                    "<p><strong>Events beyond threshold:</strong> ", n_events_beyond,
                    if (n_events_beyond == 0) " (supports cure assumption: no events after threshold)"
                    else paste0(" (events after threshold suggest cure threshold may be too early)"),
                    "</p>",

                    "<h4>Cure Fraction Estimate</h4>",
                    "<p><strong>Point estimate:</strong> ", round(cure_fraction * 100, 1), "%</p>",
                    "<p><strong>CI method:</strong> ", ci_method, "</p>"
                )

                if (has_ci) {
                    sens_html <- paste0(sens_html,
                        "<p><strong>95% CI:</strong> ", round(ci_lower * 100, 1), "% to ",
                        round(ci_upper * 100, 1), "%</p>",
                        "<p><strong>CI width:</strong> ", round(ci_range * 100, 1), " percentage points ",
                        if (is_precise) "(Relatively precise)" else "(Wide -- consider more data or bootstrap)", "</p>"
                    )
                } else {
                    sens_html <- paste0(sens_html,
                        "<p><strong>95% CI:</strong> Not available. Enable bootstrap confidence intervals for uncertainty quantification.</p>"
                    )
                }

                sens_html <- paste0(sens_html,
                    "<h4>Recommendations for Robust Sensitivity Analysis</h4>",
                    "<ul>",
                    "<li><strong>Enable bootstrap CI</strong> for accurate uncertainty quantification</li>",
                    "<li><strong>Try different cure thresholds:</strong> Rerun with different 'Cure Threshold Time' values (e.g., 48, 60, 72 months)</li>",
                    "<li><strong>Compare distributions:</strong> Try Weibull and log-normal to assess distributional assumptions</li>",
                    "<li><strong>Model comparison:</strong> Use 'Compare All Models' to compare mixture vs non-mixture results</li>",
                    "<li><strong>Survival model type:</strong> For smcure, compare PH vs AFT model types</li>",
                    "</ul>"
                )

                self$results$sensitivityAnalysis$setContent(sens_html)

            }, error = function(e) {
                self$results$sensitivityAnalysis$setContent(
                    paste0("<h4>Sensitivity Analysis</h4><p>Failed: ", e$message, "</p>")
                )
            })
        },

        # ============================================================
        # MODEL COMPARISON
        # ============================================================
        .compareModels = function() {

            tryCatch({
                mixture_model <- private$cure_model
                nm_model <- private$nm_cure_model

                comp_table <- self$results$modelComparison

                # flexsurvcure provides AIC/BIC/loglik
                if (!is.null(nm_model) && inherits(nm_model, "flexsurvcure")) {
                    nm_aic <- tryCatch(nm_model$AIC, error = function(e) NA)
                    nm_loglik <- tryCatch(as.numeric(logLik(nm_model)), error = function(e) NA)
                    nm_bic <- tryCatch(BIC(nm_model), error = function(e) NA)

                    comp_table$addRow(rowKey = "nonmixture", values = list(
                        model = "Non-mixture Cure Model (flexsurvcure)",
                        aic = if (!is.na(nm_aic)) round(nm_aic, 2) else NA,
                        bic = if (!is.na(nm_bic)) round(nm_bic, 2) else NA,
                        loglik = if (!is.na(nm_loglik)) round(nm_loglik, 2) else NA
                    ))
                }

                # smcure does NOT provide AIC/BIC -- be honest about this
                if (!is.null(mixture_model) && inherits(mixture_model, "smcure")) {
                    comp_table$addRow(rowKey = "mixture", values = list(
                        model = "Mixture Cure Model (smcure)",
                        aic = NA,
                        bic = NA,
                        loglik = NA
                    ))
                }

                # cuRe model
                cure_model <- private$cure_cure_model
                if (!is.null(cure_model)) {
                    cure_aic <- tryCatch(AIC(cure_model), error = function(e) NA)
                    cure_loglik <- tryCatch(as.numeric(logLik(cure_model)), error = function(e) NA)
                    cure_bic <- tryCatch(BIC(cure_model), error = function(e) NA)

                    comp_table$addRow(rowKey = "cure", values = list(
                        model = "cuRe Model",
                        aic = if (!is.na(cure_aic)) round(cure_aic, 2) else NA,
                        bic = if (!is.na(cure_bic)) round(cure_bic, 2) else NA,
                        loglik = if (!is.na(cure_loglik)) round(cure_loglik, 2) else NA
                    ))
                }

                # npcure doesn't provide information criteria
                if (!is.null(private$npcure_model)) {
                    comp_table$addRow(rowKey = "npcure", values = list(
                        model = "Nonparametric (npcure)",
                        aic = NA,
                        bic = NA,
                        loglik = NA
                    ))
                }

                # Store comparison notes for later combination with clinical interpretation
                private$.interpretation_html <- paste0(
                    private$.interpretation_html,
                    "<h4>Model Comparison Notes</h4>",
                    "<p><strong>Note:</strong> smcure and npcure do not provide information criteria (AIC/BIC). ",
                    "Direct comparison is only possible between flexsurvcure and cuRe models.</p>",
                    "<p>For comparing mixture (smcure) vs non-mixture (flexsurvcure), consider ",
                    "clinical plausibility and visual assessment of survival curves in addition to statistical criteria.</p>"
                )

            }, error = function(e) {
                private$.addNotice(
                    type = "WARNING",
                    title = "Model Comparison",
                    content = paste0("Comparison failed: ", e$message)
                )
            })
        },

        # ============================================================
        # CLINICAL INTERPRETATION
        # ============================================================
        .generateClinicalInterpretation = function() {

            tryCatch({
                cure_fraction <- private$.cure_fraction

                if (is.null(cure_fraction) || is.na(cure_fraction)) return()

                # Determine model type text
                model_type <- self$options$model_type %||% "mixture"
                model_type_text <- switch(model_type,
                    "mixture" = "mixture cure model (smcure)",
                    "nonmixture" = "non-mixture cure model (flexsurvcure)",
                    "cure" = "cuRe model (with background mortality)",
                    "npcure" = "nonparametric cure model (npcure)",
                    "all" = "multiple cure models",
                    model_type
                )

                # Clinical significance
                clinical_significance <- if (cure_fraction > 0.5) {
                    "high proportion of patients may achieve cure"
                } else if (cure_fraction > 0.2) {
                    "moderate proportion of patients may achieve cure"
                } else {
                    "small proportion of patients may achieve cure"
                }

                # Build CI text from actual computed values
                ci_lower <- private$.cure_ci_lower
                ci_upper <- private$.cure_ci_upper
                ci_method <- private$.cure_ci_method

                if (!is.null(ci_lower) && !is.na(ci_lower) && !is.null(ci_upper) && !is.na(ci_upper)) {
                    ci_text <- paste0(round(ci_lower * 100, 1), "%-", round(ci_upper * 100, 1), "%")
                    ci_note <- paste0(" (95% CI: ", ci_text, "; method: ", ci_method %||% "unspecified", ")")
                } else {
                    ci_note <- " (95% CI not available; enable bootstrap for confidence intervals)"
                }

                interp_html <- paste0(
                    "<h4>Clinical Interpretation</h4>",
                    "<p><b>Model Type:</b> ", model_type_text, "</p>",
                    "<p><b>Estimated Cure Fraction:</b> ", round(cure_fraction * 100, 1), "%", ci_note, "</p>",

                    "<h5>Clinical Implications:</h5>",
                    "<ul>",
                    "<li><b>Population Impact:</b> The analysis suggests that a ", clinical_significance, "</li>",
                    "<li><b>Treatment Strategy:</b> Long-term follow-up protocols should account for the cured fraction</li>",
                    "<li><b>Prognosis:</b> Patients surviving beyond the cure threshold have substantially different risk profiles</li>",
                    "</ul>",

                    "<h5>Statistical Considerations:</h5>",
                    "<ul>",
                    "<li><b>Model Assumptions:</b> Cure models assume a plateau in survival probability</li>",
                    "<li><b>Follow-up Requirements:</b> Adequate long-term follow-up is essential for valid cure fraction estimation</li>",
                    "<li><b>Validation:</b> Consider external validation in similar patient populations</li>",
                    "</ul>",

                    "<h5>Recommendations:</h5>",
                    "<ul>",
                    "<li>Monitor cure fraction estimates with longer follow-up</li>",
                    "<li>Consider patient-specific factors that may influence cure probability</li>",
                    "<li>Validate findings in independent datasets when possible</li>",
                    "</ul>"
                )

                # Build copy-ready report sentence
                n_total <- nrow(private$cure_data)
                median_fu <- round(median(private$cure_data[[self$options$time]], na.rm = TRUE), 1)

                report_sentence <- sprintf(
                    "Using a %s, the estimated cure fraction was %.1f%%%s, based on %d patients with a median follow-up of %.1f time units.",
                    model_type_text, cure_fraction * 100, ci_note, n_total, median_fu
                )

                interp_html <- paste0(interp_html,
                    "<h5>Report Sentence (copy-ready):</h5>",
                    "<div style='background-color:#f8f9fa; padding:10px; border-left:3px solid #007bff; margin:8px 0; font-style:italic;'>",
                    report_sentence,
                    "</div>",
                    "<p style='font-size:0.85em; color:#666;'>Tip: Copy the text above directly into your manuscript results section.</p>"
                )

                # Append clinical interpretation and flush all accumulated content
                private$.interpretation_html <- paste0(private$.interpretation_html, interp_html)
                self$results$interpretation$setContent(private$.interpretation_html)

            }, error = function(e) {
                # Silently fail - clinical interpretation is supplementary
            })
        }
    )
)
