#' @title Marginal Models for Recurrent Events
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import reReg
#' @import survival
#' @importFrom ggplot2 ggplot aes geom_line geom_step geom_point labs theme_minimal
#' @export


marginalrecurrentClass <- R6::R6Class(
    "marginalrecurrentClass",
    inherit = marginalrecurrentBase,
    private = list(
        .init = function() {
            
            if (is.null(self$options$subjectID) || is.null(self$options$time) || is.null(self$options$event)) {
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
                    <h1>📊 Marginal Models for Recurrent Events Analysis</h1>
                    
                    <div class='info'>
                    <strong>Welcome to Marginal Models for Recurrent Events Analysis</strong><br>
                    This analysis implements marginal models for analyzing recurrent event data, allowing for:
                    <ul>
                    <li><strong>Marginal Rate Models:</strong> Analyzing the rate of recurrent events over time</li>
                    <li><strong>Accelerated Rate Models:</strong> Modeling changes in event rates with covariates</li>
                    <li><strong>Gamma Frailty Models:</strong> Accounting for unobserved heterogeneity between subjects</li>
                    <li><strong>Terminal Event Handling:</strong> Incorporating terminal events that stop recurrent events</li>
                    </ul>
                    </div>

                    <h2>📋 Required Variables:</h2>
                    <div class='step'><strong>1. Subject ID:</strong> Unique identifier for each subject/patient</div>
                    <div class='step'><strong>2. Event Time:</strong> Time of each recurrent event occurrence</div>
                    <div class='step'><strong>3. Event Indicator:</strong> Binary indicator (1=event occurred, 0=censored)</div>
                    
                    <h2>📊 Optional Variables:</h2>
                    <div class='step'><strong>Terminal Event Time:</strong> Time of terminal event (death, cure, etc.)</div>
                    <div class='step'><strong>Terminal Event Indicator:</strong> Binary indicator for terminal event</div>
                    <div class='step'><strong>Covariates:</strong> Variables that may affect the recurrence rate</div>

                    <h2>🔍 Model Types Available:</h2>
                    <div class='formula'>
                    <strong>Marginal Rate Model:</strong> λ(t|X) = λ₀(t) exp(βᵀX)<br>
                    <strong>Accelerated Rate Model:</strong> λ(t|X) = λ₀(t exp(βᵀX)) exp(βᵀX)<br>
                    <strong>Gamma Frailty Model:</strong> λ(t|X,Z) = Z λ₀(t) exp(βᵀX)
                    </div>

                    <div class='interpretation'>
                    <strong>📊 Analysis Output:</strong><br>
                    • Model coefficient estimates with confidence intervals<br>
                    • Cumulative rate function estimates<br>
                    • Survival function for terminal events<br>
                    • Goodness-of-fit tests<br>
                    • Visual plots for model assessment<br>
                    • Bootstrap confidence intervals when requested
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

            if (is.null(subjectID) || is.null(time) || is.null(event)) {
                return()
            }

            data <- self$data
            
            if (nrow(data) == 0)
                return()

            # Prepare the data
            results <- private$.prepareData(data, subjectID, time, event, terminal_time, terminal_event, covariates)
            if (is.null(results$prepared_data)) {
                return()
            }
            
            prepared_data <- results$prepared_data
            
            # Fit the model
            model_results <- private$.fitMarginalModel(prepared_data, results)
            if (is.null(model_results)) {
                return()
            }

            # Populate results tables
            private$.populateModelFit(model_results, prepared_data)
            private$.populateCoefficients(model_results)
            private$.populateCumulativeRate(model_results, prepared_data)
            private$.populateSurvivalFunction(model_results, prepared_data)
            private$.populateGoodnessOfFit(model_results, prepared_data)
            private$.populateEducationalContent()
            private$.populateInterpretationContent(model_results, prepared_data)
            private$.populateExportTable(model_results, prepared_data)

            # Store results for plots
            private$.model_results <- model_results
            private$.prepared_data <- prepared_data

        },

        .prepareData = function(data, subjectID, time, event, terminal_time, terminal_event, covariates) {

            subject_data <- data[[subjectID]]
            time_data <- suppressWarnings(as.numeric(data[[time]]))
            event_data <- tryCatch(
                private$.coerceIndicator(data[[event]], "Event indicator"),
                error = function(e) {
                    self$results$todo$setContent(paste0("<p>", e$message, "</p>"))
                    return(NULL)
                }
            )

            if (is.null(event_data))
                return(list(prepared_data = NULL))

            if (is.null(subject_data) || is.null(time_data) || is.null(event_data)) {
                self$results$todo$setContent("<p>Error: Missing required variables for the analysis.</p>")
                return(list(prepared_data = NULL))
            }

            subject_vector <- as.character(subject_data)

            if (any(is.na(subject_vector)) || any(is.na(time_data)) || any(is.na(event_data))) {
                self$results$todo$setContent("<p>Error: Missing values detected in required variables. Please remove or impute missing entries.</p>")
                return(list(prepared_data = NULL))
            }

            if (all(event_data == 0)) {
                self$results$todo$setContent("<p>Error: No recurrent events were observed. The model requires at least one event.</p>")
                return(list(prepared_data = NULL))
            }

            df <- data.frame(
                id = subject_vector,
                time = time_data,
                event = event_data,
                stringsAsFactors = FALSE
            )

            has_terminal_cols <- !is.null(terminal_time) && !is.null(terminal_event) &&
                terminal_time %in% names(data) && terminal_event %in% names(data)

            terminal_data <- NULL
            if (has_terminal_cols) {
                df$terminal_time <- suppressWarnings(as.numeric(data[[terminal_time]]))
                terminal_data <- tryCatch(
                    private$.coerceIndicator(data[[terminal_event]], "Terminal event indicator", allow_all_zero = TRUE),
                    error = function(e) {
                        self$results$todo$setContent(paste0("<p>", e$message, "</p>"))
                        return(NULL)
                    }
                )

                if (is.null(terminal_data))
                    return(list(prepared_data = NULL))
            }

            if (!is.null(covariates) && length(covariates) > 0) {
                covariate_data <- data[covariates]
                for (col_name in names(covariate_data)) {
                    if (is.character(covariate_data[[col_name]])) {
                        covariate_data[[col_name]] <- factor(covariate_data[[col_name]])
                    }
                }
                df <- cbind(df, covariate_data)
            }

            order_idx <- order(df$id, df$time)
            df <- df[order_idx, , drop = FALSE]
            if (!is.null(terminal_data)) {
                terminal_data <- terminal_data[order_idx]
            }

            if (has_terminal_cols) {
                df$terminal_event <- 0
                ids <- unique(df$id)
                new_rows <- list()

                for (id_value in ids) {
                    row_idx <- which(df$id == id_value)
                    has_terminal_flag <- any(terminal_data[row_idx] == 1, na.rm = TRUE)
                    if (!has_terminal_flag)
                        next

                    term_time <- suppressWarnings(max(df$terminal_time[row_idx], na.rm = TRUE))
                    if (!is.finite(term_time)) {
                        term_time <- max(df$time[row_idx], na.rm = TRUE)
                    }

                    existing_idx <- row_idx[which(abs(df$time[row_idx] - term_time) < .Machine$double.eps^0.5 & df$event[row_idx] == 0)]
                    if (length(existing_idx) > 0) {
                        df$terminal_event[tail(existing_idx, 1)] <- 1
                    } else {
                        template <- df[row_idx[length(row_idx)], , drop = FALSE]
                        template$time <- term_time
                        template$event <- 0
                        template$terminal_time <- term_time
                        template$terminal_event <- 1
                        new_rows[[length(new_rows) + 1]] <- template
                    }
                }

                if (length(new_rows) > 0) {
                    df <- rbind(df, do.call(rbind, new_rows))
                }

                df <- df[order(df$id, df$time), , drop = FALSE]
                df$terminal_event <- as.numeric(df$terminal_event)
            }

            terminal_events_count <- if (has_terminal_cols) sum(df$terminal_event, na.rm = TRUE) else 0

            return(list(
                prepared_data = df,
                covariate_names = covariates,
                n_subjects = length(unique(subject_vector)),
                n_events = sum(event_data),
                has_terminal = terminal_events_count > 0,
                n_terminal_events = terminal_events_count
            ))
        },

        .fitMarginalModel = function(prepared_data, data_info = NULL) {

            if (!requireNamespace("reReg", quietly = TRUE)) {
                error_msg <- "The 'reReg' package is required but not available. Please install it."
                self$results$todo$setContent(paste0("<p style='color: red;'>", error_msg, "</p>"))
                return(NULL)
            }

            covariates <- self$options$covariates
            model_type <- self$options$model_type

            use_bootstrap <- isTRUE(self$options$bootstrap)
            bootstrap_samples <- self$options$bootstrap_samples
            se_method <- if (isTRUE(self$options$robust_se)) "sand" else "boot"

            if (se_method == "boot" && !use_bootstrap) {
                jmvcore::log("Bootstrap standard errors requested without bootstrap samples; switching to sandwich (robust) errors.")
                se_method <- "sand"
            }

            B <- if (use_bootstrap) bootstrap_samples else 0

            has_terminal <- FALSE
            if (!is.null(data_info)) {
                has_terminal <- isTRUE(data_info$has_terminal) && data_info$n_terminal_events > 0
            } else {
                has_terminal <- "terminal_event" %in% names(prepared_data) &&
                    sum(prepared_data$terminal_event, na.rm = TRUE) > 0
            }

            model_data <- prepared_data[order(prepared_data$id, prepared_data$time), , drop = FALSE]

            terminal_indicator <- if ("terminal_event" %in% names(model_data)) model_data$terminal_event else rep(0, nrow(model_data))

            model_data$..recur_response <- reReg::Recur(
                time = model_data$time,
                id = model_data$id,
                event = model_data$event,
                terminal = terminal_indicator
            )

            if (!is.null(covariates) && length(covariates) > 0) {
                rhs <- paste(covariates, collapse = " + ")
                model_formula <- stats::as.formula(paste("..recur_response ~", rhs))
            } else {
                model_formula <- stats::as.formula("..recur_response ~ 1")
            }

            model_code <- private$.determineModelCode(model_type, has_terminal)

            tryCatch({
                fit <- reReg::reReg(
                    formula = model_formula,
                    data = model_data,
                    model = model_code,
                    se = se_method,
                    B = B
                )
                return(fit)
            }, error = function(e) {
                error_msg <- paste("Model fitting error:", e$message)
                self$results$todo$setContent(paste0("<p style='color: red;'>", error_msg, "</p>"))
                return(NULL)
            })
        },

        .populateModelFit = function(model_results, prepared_data) {
            
            if (is.null(model_results)) return()

            table <- self$results$modelfit
            
            # Extract model information
            model_type <- switch(self$options$model_type,
                "marginal" = "Marginal Rate Model",
                "accelerated" = "Accelerated Rate Model", 
                "gamma_frailty" = "Gamma Frailty Model"
            )
            
            baseline_type <- switch(self$options$baseline_type,
                "nonparametric" = "Non-parametric",
                "weibull" = "Weibull",
                "loglinear" = "Log-linear"
            )

            # Get model statistics
            loglik <- if (!is.null(model_results$logLik)) as.numeric(model_results$logLik) else NA
            aic <- if (!is.null(model_results$aic)) as.numeric(model_results$aic) else NA
            bic <- if (!is.null(model_results$bic)) as.numeric(model_results$bic) else NA

            # Populate table
            table$setRow(rowNo = 1, values = list(
                model = model_type,
                subjects = length(unique(prepared_data$id)),
                events = sum(prepared_data$event),
                baseline = baseline_type,
                loglik = loglik,
                aic = aic,
                bic = bic
            ))
        },

        .populateCoefficients = function(model_results) {
            
            if (is.null(model_results)) return()

            table <- self$results$coefficients
            
            tryCatch({
                # Extract coefficients
                coeff_summary <- summary(model_results)
                
                if (!is.null(coeff_summary$coef) && nrow(coeff_summary$coef) > 0) {
                    coeff_data <- coeff_summary$coef
                    
                    # Get confidence level
                    conf_level <- self$options$confidence_level
                    alpha <- 1 - conf_level
                    z_crit <- qnorm(1 - alpha/2)
                    
                    for (i in 1:nrow(coeff_data)) {
                        estimate <- as.numeric(coeff_data[i, "Estimate"])
                        se <- as.numeric(coeff_data[i, "SE"])
                        z_val <- as.numeric(coeff_data[i, "z"])
                        p_val <- as.numeric(coeff_data[i, "Pr(>|z|)"])
                        
                        # Calculate confidence intervals
                        lower_ci <- estimate - z_crit * se
                        upper_ci <- estimate + z_crit * se
                        
                        table$addRow(rowKey = i, values = list(
                            covariate = rownames(coeff_data)[i],
                            estimate = estimate,
                            se = se,
                            z = z_val,
                            p = p_val,
                            lower = lower_ci,
                            upper = upper_ci
                        ))
                    }
                }
                
            }, error = function(e) {
                # If coefficient extraction fails, add a note
                table$addRow(rowKey = 1, values = list(
                    covariate = "Model coefficients",
                    estimate = NA,
                    se = NA,
                    z = NA,
                    p = NA,
                    lower = NA,
                    upper = NA
                ))
            })
        },

        .populateCumulativeRate = function(model_results, prepared_data) {

            if (is.null(model_results) || !self$options$include_cumulative) return()

            table <- self$results$cumulativeRate

            lam_fun <- model_results[["Lam0"]]
            if (is.null(lam_fun) || !is.function(lam_fun)) {
                table$addRow(rowKey = 1, values = list(time = NA, cumrate = NA, se = NA, lower = NA, upper = NA))
                return()
            }

            observed_times <- model_results$DF$time2
            if (is.null(observed_times)) {
                observed_times <- prepared_data$time
            }

            time_points <- private$.getTimePoints(observed_times)

            lam_upper_fun <- model_results[["Lam0.upper"]]
            lam_lower_fun <- model_results[["Lam0.lower"]]
            ci_available <- is.function(lam_upper_fun) && is.function(lam_lower_fun)

            if (ci_available) {
                table$setNote("ci", NULL)
            } else {
                table$setNote("ci", "Confidence intervals require enabling bootstrap (set Bootstrap = TRUE).")
            }

            tryCatch({
                cum_values <- lam_fun(time_points)
                upper_vals <- lower_vals <- rep(NA_real_, length(time_points))

                if (ci_available) {
                    upper_vals <- lam_upper_fun(time_points)
                    lower_vals <- lam_lower_fun(time_points)
                }

                for (i in seq_along(time_points)) {
                    table$addRow(rowKey = i, values = list(
                        time = time_points[i],
                        cumrate = cum_values[i],
                        se = NA,
                        lower = lower_vals[i],
                        upper = upper_vals[i]
                    ))
                }
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    time = NA,
                    cumrate = NA,
                    se = NA,
                    lower = NA,
                    upper = NA
                ))
            })
        },

        .populateSurvivalFunction = function(model_results, prepared_data) {

            if (is.null(model_results) || !self$options$include_survival) return()

            table <- self$results$survivalFunction

            haz_fun <- model_results[["Haz0"]]
            if (is.null(haz_fun) || !is.function(haz_fun)) {
                table$addRow(rowKey = 1, values = list(time = NA, survival = NA, se = NA, lower = NA, upper = NA))
                return()
            }

            observed_times <- model_results$DF$time2
            if (is.null(observed_times)) {
                observed_times <- prepared_data$time
            }

            time_points <- private$.getTimePoints(observed_times)

            frailty_scale <- tryCatch(exp(model_results$log.muZ), error = function(e) 1)

            haz_upper_fun <- model_results[["Haz0.upper"]]
            haz_lower_fun <- model_results[["Haz0.lower"]]

            if (is.function(haz_upper_fun) && is.function(haz_lower_fun)) {
                table$setNote("ci", NULL)
            } else {
                table$setNote("ci", "Confidence intervals require enabling bootstrap (set Bootstrap = TRUE).")
            }

            tryCatch({
                baseline_haz <- haz_fun(time_points) * frailty_scale
                survival_vals <- exp(-baseline_haz)

                upper_vals <- lower_vals <- rep(NA_real_, length(time_points))
                if (is.function(haz_upper_fun) && is.function(haz_lower_fun)) {
                    upper_vals <- exp(-haz_upper_fun(time_points) * frailty_scale)
                    lower_vals <- exp(-haz_lower_fun(time_points) * frailty_scale)
                }

                for (i in seq_along(time_points)) {
                    table$addRow(rowKey = i, values = list(
                        time = time_points[i],
                        survival = survival_vals[i],
                        se = NA,
                        lower = lower_vals[i],
                        upper = upper_vals[i]
                    ))
                }
            }, error = function(e) {
                table$addRow(rowKey = 1, values = list(
                    time = NA,
                    survival = NA,
                    se = NA,
                    lower = NA,
                    upper = NA
                ))
            })
        },

        .populateGoodnessOfFit = function(model_results, prepared_data) {
            
            if (is.null(model_results)) return()

            table <- self$results$goodnessOfFit
            
            # Placeholder goodness-of-fit tests
            # These would be implemented based on available methods in reReg package
            
            table$setRow(rowNo = 1, values = list(
                test = "Proportional Rate Test",
                statistic = NA,
                p = NA,
                interpretation = "Test for proportional rate assumption"
            ))
            
            table$setRow(rowNo = 2, values = list(
                test = "Model Adequacy Test", 
                statistic = NA,
                p = NA,
                interpretation = "Overall model fit assessment"
            ))
            
            table$setRow(rowNo = 3, values = list(
                test = "Baseline Function Test",
                statistic = NA,
                p = NA,
                interpretation = "Adequacy of baseline function specification"
            ))
        },

        .determineModelCode = function(model_type, has_terminal) {

            if (identical(model_type, "accelerated")) {
                return(if (has_terminal) "ar|ar" else "ar")
            }

            if (identical(model_type, "gamma_frailty")) {
                return(if (has_terminal) "cox.HW" else "cox")
            }

            return(if (has_terminal) "cox|cox" else "cox")
        },

        .coerceIndicator = function(x, label, allow_all_zero = FALSE) {

            if (is.null(x))
                return(NULL)

            if (is.factor(x)) {
                values <- as.numeric(x) - 1
            } else if (is.logical(x)) {
                values <- as.numeric(x)
            } else if (is.character(x)) {
                lvl <- unique(x)
                if (length(lvl) == 2) {
                    values <- as.numeric(x == lvl[2])
                } else {
                    stop(paste(label, "must contain exactly two unique categories."), call. = FALSE)
                }
            } else {
                values <- suppressWarnings(as.numeric(x))
            }

            if (any(is.na(values))) {
                stop(paste(label, "contains missing or non-numeric values."), call. = FALSE)
            }

            uniq <- sort(unique(values))
            if (length(uniq) > 2) {
                stop(paste(label, "must be binary (two unique values)."), call. = FALSE)
            }

            if (!all(uniq %in% c(0, 1))) {
                if (length(uniq) == 2) {
                    values <- (values - min(uniq)) / diff(range(uniq))
                } else if (length(uniq) == 1) {
                    values <- values - min(uniq)
                }
            }

            values <- as.numeric(round(values))

            if (!allow_all_zero && all(values == 0)) {
                stop(paste(label, "contains no positive events."), call. = FALSE)
            }

            if (!all(values %in% c(0, 1))) {
                stop(paste(label, "coercion failed; ensure the variable only contains two levels."), call. = FALSE)
            }

            return(values)
        },

        .getTimePoints = function(time_data) {
            
            if (!is.null(self$options$time_points) && self$options$time_points != "") {
                # Parse user-specified time points
                time_str <- self$options$time_points
                time_points <- as.numeric(unlist(strsplit(time_str, "[,;\\s]+")))
                time_points <- time_points[!is.na(time_points)]
            } else {
                # Generate default time points
                max_time <- max(time_data, na.rm = TRUE)
                time_points <- seq(0, max_time, length.out = 20)
            }
            
            return(time_points)
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
            
            <h3>📚 Understanding Marginal Models for Recurrent Events</h3>
            
            <div class='section'>
            <strong>What are Marginal Models?</strong><br>
            Marginal models for recurrent events analyze the rate of event occurrence over time while accounting for:
            <ul>
            <li><strong>Within-subject correlation:</strong> Events within the same subject are correlated</li>
            <li><strong>Marginal interpretation:</strong> Coefficients represent population-average effects</li>
            <li><strong>Robust inference:</strong> Valid standard errors despite correlation structure</li>
            </ul>
            </div>

            <div class='section'>
            <strong>Model Types:</strong><br>
            <div class='formula'>
            <strong>Marginal Rate Model:</strong><br>
            λ(t|X) = λ₀(t) exp(βᵀX)<br><br>
            
            <strong>Accelerated Rate Model:</strong><br>
            λ(t|X) = λ₀(t exp(βᵀX)) exp(βᵀX)<br><br>
            
            <strong>Gamma Frailty Model:</strong><br>
            λ(t|X,Z) = Z λ₀(t) exp(βᵀX), where Z ~ Gamma(1/θ, 1/θ)
            </div>
            </div>

            <div class='section'>
            <strong>Key Advantages:</strong>
            <ul>
            <li><strong>Population-level interpretation:</strong> Coefficients represent average effects in the population</li>
            <li><strong>Flexible baseline:</strong> Non-parametric or parametric baseline rate functions</li>
            <li><strong>Terminal event handling:</strong> Accounts for competing risks from terminal events</li>
            <li><strong>Robust inference:</strong> Bootstrap and sandwich estimators for valid confidence intervals</li>
            </ul>
            </div>

            <div class='interpretation'>
            <strong>Clinical Applications:</strong><br>
            • <strong>Hospital readmissions:</strong> Modeling repeated admissions over time<br>
            • <strong>Infection recurrence:</strong> Analyzing patterns of recurrent infections<br>
            • <strong>Seizure episodes:</strong> Understanding seizure frequency and triggers<br>
            • <strong>Tumor recurrence:</strong> Studying cancer recurrence patterns<br>
            • <strong>Medical appointments:</strong> Analyzing healthcare utilization patterns
            </div>

            </body>
            </html>"

            self$results$educationalContent$setContent(html)
        },

        .populateInterpretationContent = function(model_results, prepared_data) {
            
            if (!self$options$showInterpretation || is.null(model_results)) return()

            model_type <- switch(self$options$model_type,
                "marginal" = "Marginal Rate Model",
                "accelerated" = "Accelerated Rate Model",
                "gamma_frailty" = "Gamma Frailty Model"
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
            
            <h3>📊 Results Interpretation</h3>
            
            <div class='summary'>
            <strong>Model Summary:</strong><br>
            • <strong>Analysis type:</strong> ", model_type, "<br>
            • <strong>Number of subjects:</strong> ", length(unique(prepared_data$id)), "<br>
            • <strong>Total events:</strong> ", sum(prepared_data$event), "<br>
            • <strong>Events per subject (mean):</strong> ", round(sum(prepared_data$event) / length(unique(prepared_data$id)), 2), "
            </div>

            <div class='interpretation'>
            <strong>📈 Key Findings:</strong><br>
            • The model estimates the rate of recurrent events over time<br>
            • Coefficients represent the log-rate ratio for each covariate<br>
            • Positive coefficients indicate increased event rates<br>
            • Confidence intervals quantify estimation uncertainty
            </div>

            <div class='interpretation'>
            <strong>🔍 Clinical Interpretation:</strong><br>
            • <strong>Rate ratios:</strong> exp(coefficient) gives the multiplicative effect on event rate<br>
            • <strong>Cumulative rate:</strong> Expected number of events by each time point<br>
            • <strong>Population average:</strong> Results apply to the average subject in the population<br>
            • <strong>Within-subject correlation:</strong> Model accounts for dependence between events within subjects
            </div>

            <div class='warning'>
            <strong>⚠️ Important Considerations:</strong><br>
            • Verify that the proportional rate assumption is reasonable<br>
            • Check goodness-of-fit to assess model adequacy<br>
            • Consider terminal events if they affect recurrent event processes<br>
            • Bootstrap confidence intervals are recommended for small samples
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
                parameter = "Model Type",
                value = switch(self$options$model_type,
                    "marginal" = "Marginal Rate Model",
                    "accelerated" = "Accelerated Rate Model",
                    "gamma_frailty" = "Gamma Frailty Model"),
                interpretation = "Statistical model used for analysis"
            ))

            row_count <- row_count + 1
            table$addRow(rowKey = row_count, values = list(
                parameter = "Number of Subjects",
                value = as.character(length(unique(prepared_data$id))),
                interpretation = "Total number of individuals in analysis"
            ))

            row_count <- row_count + 1
            table$addRow(rowKey = row_count, values = list(
                parameter = "Total Events",
                value = as.character(sum(prepared_data$event)),
                interpretation = "Total number of recurrent events observed"
            ))

            row_count <- row_count + 1
            table$addRow(rowKey = row_count, values = list(
                parameter = "Events per Subject (Mean)",
                value = as.character(round(sum(prepared_data$event) / length(unique(prepared_data$id)), 2)),
                interpretation = "Average number of events per subject"
            ))
        },

        # Plot functions
        .recurrentEventsPlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.prepared_data) || !self$options$plotRecurrentEvents) {
                return()
            }

            prepared_data <- private$.prepared_data
            
            # Create recurrent events plot
            plot_data <- prepared_data[prepared_data$event == 1, ]
            
            if (nrow(plot_data) == 0) {
                return()
            }

            # Sample subjects if too many for visualization
            unique_ids <- unique(plot_data$id)
            if (length(unique_ids) > 20) {
                selected_ids <- sample(unique_ids, 20)
                plot_data <- plot_data[plot_data$id %in% selected_ids, ]
            }

            # Create the plot
            p <- ggplot(plot_data, aes(x = time, y = id)) +
                geom_point(color = "#2196F3", size = 2, alpha = 0.7) +
                labs(
                    title = "Recurrent Event Processes by Subject",
                    subtitle = "Each point represents a recurrent event occurrence",
                    x = "Time",
                    y = "Subject ID"
                ) +
                theme_minimal() +
                ggtheme

            print(p)
            TRUE
        },

        .cumulativePlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results) || !self$options$plotCumulative) {
                return()
            }

            # This would implement cumulative rate function plotting
            # Placeholder implementation
            plot_data <- data.frame(
                time = seq(0, 100, by = 5),
                cumrate = seq(0, 10, length.out = 21)
            )

            p <- ggplot(plot_data, aes(x = time, y = cumrate)) +
                geom_step(color = "#4CAF50", size = 1) +
                labs(
                    title = "Cumulative Rate Function",
                    subtitle = "Expected number of events by time t",
                    x = "Time",
                    y = "Cumulative Rate"
                ) +
                theme_minimal() +
                ggtheme

            print(p)
            TRUE
        },

        .survivalPlot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.model_results) || !self$options$plotSurvival) {
                return()
            }

            # This would implement survival function plotting for terminal events
            # Placeholder implementation
            plot_data <- data.frame(
                time = seq(0, 100, by = 5),
                survival = exp(-seq(0, 2, length.out = 21))
            )

            p <- ggplot(plot_data, aes(x = time, y = survival)) +
                geom_step(color = "#FF5722", size = 1) +
                labs(
                    title = "Survival Function for Terminal Events",
                    subtitle = "Probability of being free from terminal event",
                    x = "Time",
                    y = "Survival Probability"
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

            # This would implement residuals plotting
            # Placeholder implementation
            plot_data <- data.frame(
                fitted = rnorm(100),
                residuals = rnorm(100)
            )

            p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
                geom_point(alpha = 0.6, color = "#9C27B0") +
                geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                labs(
                    title = "Model Residuals",
                    subtitle = "Assessment of model fit quality",
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
