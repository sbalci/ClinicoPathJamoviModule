#' @title Lasso-Cox Regression for Variable Selection in Survival Analysis
#' @description
#' Performs Lasso-penalized Cox proportional hazards regression for variable selection
#' in survival analysis. This function uses penalized likelihood to identify the most
#' important predictors while preventing overfitting, making it ideal for high-dimensional
#' survival data where the number of potential predictors may approach or exceed the sample size.
#' 
#' @details
#' The Lasso-Cox regression combines the Cox proportional hazards model with L1 regularization
#' (Lasso penalty) to perform automatic variable selection. The method minimizes the partial
#' likelihood penalized by the L1 norm of the coefficient vector, effectively shrinking
#' less important coefficients toward zero and setting some exactly to zero.
#' 
#' Key features:
#' - Automatic variable selection through L1 regularization
#' - Cross-validation for optimal tuning parameter selection
#' - Risk score calculation and stratification
#' - Comprehensive model performance evaluation
#' - Survival curve visualization by risk groups
#' 
#' The function uses the glmnet package for efficient computation and supports both
#' lambda.min (minimum cross-validation error) and lambda.1se (1 standard error rule)
#' for tuning parameter selection.
#' 
#' @examples
#' \dontrun{
#' # Basic Lasso-Cox regression
#' result <- lassocox(
#'   data = survival_data,
#'   elapsedtime = "time",
#'   outcome = "status", 
#'   outcomeLevel = "1",
#'   explanatory = c("age", "gender", "stage", "grade"),
#'   lambda = "lambda.1se",
#'   nfolds = 10
#' )
#' 
#' # High-dimensional scenario
#' result <- lassocox(
#'   data = genomic_data,
#'   elapsedtime = "survival_time",
#'   outcome = "event",
#'   outcomeLevel = "death",
#'   explanatory = gene_variables,
#'   lambda = "lambda.min",
#'   nfolds = 5,
#'   standardize = TRUE
#' )
#' }
#' 
#' @importFrom R6 R6Class
#' @import jmvcore

lassocoxClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "lassocoxClass",
    inherit = lassocoxBase,
    private = list(

        # Initialize results and validate dependencies
        .init = function() {
            # Check for required packages
            missing_packages <- c()
            if (!requireNamespace("glmnet", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "glmnet")
            }
            if (!requireNamespace("survival", quietly = TRUE)) {
                missing_packages <- c(missing_packages, "survival")
            }
            
            if (length(missing_packages) > 0) {
                pkg_list <- paste(missing_packages, collapse = ", ")
                install_cmd <- paste0("install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))")
                error_msg <- jmvcore::format(
                    .("The following required packages are not installed: {pkgs}\n\nPlease install them using:\n{cmd}"),
                    list(pkgs = pkg_list, cmd = install_cmd)
                )

                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'>",
                    "<h4>", .("Missing Dependencies"), "</h4>",
                    "<p>", gsub("\n", "<br>", error_msg), "</p>",
                    "</div>"
                ))
                return()
            }
            
            # Initialize with welcome message if no variables selected
            if (is.null(self$options$elapsedtime) ||
                is.null(self$options$outcome) ||
                is.null(self$options$explanatory) ||
                length(self$options$explanatory) == 0) {

                welcome_msg <- paste0(
                "<div class='alert alert-info'>",
                "<h4>", .("Welcome to Lasso-Cox Regression"), "</h4>",
                "<p>", .("This analysis performs variable selection in survival analysis using the Lasso penalty."), "</p>",
                "<h5>", .("Required inputs:"), "</h5>",
                "<ul>",
                "<li><strong>", .("Time Elapsed"), "</strong>: ", .("Survival/follow-up time (continuous, positive values)"), "</li>",
                "<li><strong>", .("Outcome"), "</strong>: ", .("Binary outcome variable (event/censored status)"), "</li>",
                "<li><strong>", .("Explanatory Variables"), "</strong>: ", .("Potential predictors for selection (2 or more variables)"), "</li>",
                "</ul>",
                "<h5>", .("The analysis will provide:"), "</h5>",
                "<ul>",
                "<li>", .("Variable selection with Lasso regularization"), "</li>",
                "<li>", .("Cross-validation for optimal tuning parameter"), "</li>",
                "<li>", .("Model performance metrics (C-index, etc.)"), "</li>",
                "<li>", .("Risk score calculation and stratification"), "</li>",
                "<li>", .("Survival curves by risk groups"), "</li>",
                "<li>", .("Comprehensive visualizations"), "</li>",
                "</ul>",
                "<h5>", .("Key Features:"), "</h5>",
                "<ul>",
                "<li>", .("Handles high-dimensional data (p >= n scenarios)"), "</li>",
                "<li>", .("Automatic variable selection"), "</li>",
                "<li>", .("Prevents overfitting through regularization"), "</li>",
                "<li>", .("Clinical risk stratification"), "</li>",
                "</ul>",
                "</div>"
                )
                
                self$results$todo$setContent(welcome_msg)

                # Hide results until data is provided
                self$results$modelSummary$setVisible(FALSE)
                self$results$coefficients$setVisible(FALSE)
                self$results$performance$setVisible(FALSE)
                self$results$cv_plot$setVisible(FALSE)
                self$results$coef_plot$setVisible(FALSE)
                self$results$survival_plot$setVisible(FALSE)
            }

            # Informational note: survminer is optional because a base-R fallback is provided
            if (!requireNamespace("survminer", quietly = TRUE) && isTRUE(self$options$survival_plot)) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-warning'>",
                    "<h4>", .("Optional Package Not Installed"), "</h4>",
                    "<p>", .("<code>survminer</code> is not installed. Survival curves will use a base-R fallback."), "</p>",
                    "</div>"
                ))
                self$results$todo$setVisible(TRUE)
            }
            
            # Initialize explanatory content
            private$.initializeExplanations()
        },

        .run = function() {
            # Early exits for missing data or variables
            if (is.null(self$data) || nrow(self$data) == 0) {
                return()
            }

            if (is.null(self$options$elapsedtime) ||
                is.null(self$options$outcome) ||
                is.null(self$options$explanatory) ||
                length(self$options$explanatory) == 0) {
                return()
            }

            # Hide welcome message and show results
            self$results$todo$setVisible(FALSE)
            self$results$modelSummary$setVisible(TRUE)
            self$results$coefficients$setVisible(TRUE)
            self$results$performance$setVisible(TRUE)

            # Collect warnings during the pipeline so they appear in jamovi GUI
            collected_warnings <- character(0)

            # Main analysis pipeline with comprehensive error handling
            withCallingHandlers(
                tryCatch({
                    # Prepare and validate data
                    data <- private$.cleanData()
                    if (is.null(data)) return()

                    # Run suitability assessment (advisory only, always proceeds)
                    if (self$options$suitabilityCheck) {
                        private$.assessSuitability(data)
                    }

                    # Fit Lasso-Cox model
                    results <- private$.fitModel(data)
                    if (is.null(results)) return()

                    # Populate result tables
                    private$.populateModelSummary(results)
                    private$.populateCoefficients(results)
                    private$.populatePerformance(results)

                    # Save plot data for rendering
                    private$.savePlotData(results)

                    # Display collected warnings in results panel
                    if (length(collected_warnings) > 0) {
                        warn_items <- paste0("<li>", collected_warnings, "</li>",
                                             collapse = "")
                        warn_html <- paste0(
                            "<div class='alert alert-warning'>",
                            "<h4>", .("Analysis Notes"), "</h4>",
                            "<ul>", warn_items, "</ul>",
                            "</div>"
                        )
                        self$results$todo$setContent(warn_html)
                        self$results$todo$setVisible(TRUE)
                    }

                }, error = function(e) {
                    error_msg <- paste0(
                        "<div class='alert alert-danger'>",
                        "<h4>", .("Analysis Error"), "</h4>",
                        "<p><strong>", .("Error:"), "</strong> ", e$message, "</p>",
                        "<p>", .("Please check your data and variable selections."), "</p>",
                        "</div>"
                    )
                    private$.clearAnalysisOutputs()
                    self$results$todo$setContent(error_msg)
                    self$results$todo$setVisible(TRUE)
                }),
                warning = function(w) {
                    collected_warnings <<- c(collected_warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }
            )
        },

        .clearAnalysisOutputs = function() {
            # Clear tabular outputs
            self$results$modelSummary$deleteRows()
            self$results$coefficients$deleteRows()
            self$results$performance$deleteRows()
            self$results$variableImportance$deleteRows()
            self$results$modelComparison$deleteRows()

            # Clear plot states
            self$results$cv_plot$setState(NULL)
            self$results$coef_plot$setState(NULL)
            self$results$survival_plot$setState(NULL)

            # Clear saved output variable values
            if (!is.null(self$results$riskScore) && !is.null(self$data) && nrow(self$data) > 0) {
                self$results$riskScore$setValues(rep(NA_real_, nrow(self$data)))
            }
        },

        # Comprehensive data cleaning and validation
        .cleanData = function() {
            data <- self$data
            time_var <- self$options$elapsedtime
            outcome_var <- self$options$outcome
            explanatory_vars <- self$options$explanatory

            # Validate predictors
            if (length(explanatory_vars) < 2) {
                stop(.("At least 2 explanatory variables are required for Lasso regression."))
            }

            # Extract core variables
            time <- jmvcore::toNumeric(data[[time_var]])
            outcome_raw <- data[[outcome_var]]
            predictors <- data[explanatory_vars]

            # Determine event coding robustly for factor/character/numeric outcomes
            # Uses strict two-level encoding: event_level -> 1, censor_level -> 0,
            # anything else (including NA) -> NA (excluded by complete.cases)
            event_level_used <- NULL
            censor_level_used <- NULL

            if (is.factor(outcome_raw) || is.character(outcome_raw)) {
                outcome_chr <- as.character(outcome_raw)
                observed_levels <- sort(unique(outcome_chr[!is.na(outcome_chr)]))
                if (length(observed_levels) < 2) {
                    stop(jmvcore::format(.('Outcome variable must have at least 2 observed values. Found {n} level(s): {levels}'),
                        list(n = length(observed_levels), levels = paste(observed_levels, collapse = ", "))))
                }

                # Resolve event level
                outcome_level_opt <- self$options$outcomeLevel
                if (is.null(outcome_level_opt) || !nzchar(as.character(outcome_level_opt))) {
                    event_level_used <- observed_levels[2]
                } else {
                    event_level_used <- as.character(outcome_level_opt)
                    if (!(event_level_used %in% observed_levels)) {
                        stop(jmvcore::format(.("Selected event level ('{level}') is not present in observed outcome data."),
                            list(level = event_level_used)))
                    }
                }

                # Resolve censor level
                censor_level_opt <- self$options$censorLevel
                if (is.null(censor_level_opt) || !nzchar(as.character(censor_level_opt))) {
                    # Default: first observed level that is not the event level
                    remaining <- setdiff(observed_levels, event_level_used)
                    if (length(remaining) == 0) {
                        stop(.("Cannot determine censored level: all observed levels equal the event level."))
                    }
                    censor_level_used <- remaining[1]
                } else {
                    censor_level_used <- as.character(censor_level_opt)
                    if (!(censor_level_used %in% observed_levels)) {
                        stop(jmvcore::format(.("Selected censored level ('{level}') is not present in observed outcome data."),
                            list(level = censor_level_used)))
                    }
                }

                if (event_level_used == censor_level_used) {
                    stop(.("Event level and censored level must be different."))
                }

                # Strict two-level encoding: only recognized levels get 0/1
                status <- rep(NA_real_, length(outcome_chr))
                status[outcome_chr == event_level_used] <- 1
                status[outcome_chr == censor_level_used] <- 0
                # Rows with NA outcome_chr or unrecognized values stay NA

                n_unrecognized <- sum(!is.na(outcome_chr) &
                                      outcome_chr != event_level_used &
                                      outcome_chr != censor_level_used)
                if (n_unrecognized > 0) {
                    warning(jmvcore::format(
                        .('Outcome contains {n} row(s) with values other than the event ("{event}") or censored ("{censor}") levels. These rows will be excluded.'),
                        list(n = n_unrecognized, event = event_level_used, censor = censor_level_used)))
                }
            } else {
                outcome_num <- jmvcore::toNumeric(outcome_raw)
                observed_levels <- sort(unique(outcome_num[!is.na(outcome_num)]))
                if (length(observed_levels) < 2) {
                    stop(jmvcore::format(.('Numeric outcome must have at least 2 observed values. Found: {values}'),
                        list(values = paste(observed_levels, collapse = ", "))))
                }

                # Resolve event level
                outcome_level_opt <- self$options$outcomeLevel
                if (!is.null(outcome_level_opt) && nzchar(as.character(outcome_level_opt))) {
                    event_level_num <- suppressWarnings(as.numeric(outcome_level_opt))
                    if (is.na(event_level_num) || !(event_level_num %in% observed_levels)) {
                        stop(.("For numeric outcomes, Event Level must be one of the observed outcome values."))
                    }
                } else if (all(observed_levels %in% c(0, 1))) {
                    event_level_num <- 1
                } else {
                    event_level_num <- max(observed_levels)
                }

                # Resolve censor level
                censor_level_opt <- self$options$censorLevel
                if (!is.null(censor_level_opt) && nzchar(as.character(censor_level_opt))) {
                    censor_level_num <- suppressWarnings(as.numeric(censor_level_opt))
                    if (is.na(censor_level_num) || !(censor_level_num %in% observed_levels)) {
                        stop(.("For numeric outcomes, Censored Level must be one of the observed outcome values."))
                    }
                } else if (all(observed_levels %in% c(0, 1))) {
                    censor_level_num <- 0
                } else {
                    censor_level_num <- min(observed_levels)
                }

                if (event_level_num == censor_level_num) {
                    stop(.("Event level and censored level must be different."))
                }

                event_level_used <- as.character(event_level_num)
                censor_level_used <- as.character(censor_level_num)

                # Strict two-level encoding
                status <- rep(NA_real_, length(outcome_num))
                status[outcome_num == event_level_num] <- 1
                status[outcome_num == censor_level_num] <- 0

                n_unrecognized <- sum(!is.na(outcome_num) &
                                      outcome_num != event_level_num &
                                      outcome_num != censor_level_num)
                if (n_unrecognized > 0) {
                    warning(jmvcore::format(
                        .('Outcome contains {n} row(s) with values other than the event ({event}) or censored ({censor}) levels. These rows will be excluded.'),
                        list(n = n_unrecognized, event = event_level_used, censor = censor_level_used)))
                }
            }

            # Remove constant explanatory variables instead of failing hard
            constant_vars <- sapply(predictors, function(x) {
                if (is.numeric(x)) {
                    v <- var(x, na.rm = TRUE)
                    is.na(v) || v == 0
                } else {
                    length(unique(na.omit(x))) <= 1
                }
            })
            if (any(constant_vars)) {
                constant_var_names <- names(predictors)[constant_vars]
                predictors <- predictors[, !constant_vars, drop = FALSE]
                explanatory_vars <- names(predictors)
                warning(jmvcore::format(.('Removed constant explanatory variables: {vars}'),
                    list(vars = paste(constant_var_names, collapse = ", "))))
            }
            if (ncol(predictors) == 0) {
                stop(.("No valid explanatory variables remain after removing constant predictors."))
            }
            if (ncol(predictors) == 1) {
                warning(.("Only one non-constant explanatory variable remains; LASSO selection is limited."))
            }

            # Complete-case filtering across all analysis inputs
            complete <- complete.cases(time, status, predictors)
            n_complete <- sum(complete)
            n_excluded <- length(complete) - n_complete

            if (n_complete < 10) {
                stop(jmvcore::format(.('Too few complete cases for analysis ({n}). Need at least 10 complete observations.'),
                    list(n = n_complete)))
            }
            if (n_excluded > 0) {
                warning(jmvcore::format(.('Excluded {n} row(s) with missing values in time/outcome/predictors (complete-case analysis).'),
                    list(n = n_excluded)))
            }

            time_cc <- time[complete]
            status_cc <- status[complete]

            if (any(!is.finite(time_cc))) {
                stop(.("Time variable contains non-finite values after filtering. Please correct the input."))
            }
            if (any(time_cc < 0, na.rm = TRUE)) {
                stop(.("Time variable contains negative values. Survival times must be non-negative."))
            }
            if (any(time_cc == 0, na.rm = TRUE)) {
                warning(.("Time variable contains zero values. Consider adding a small constant if convergence issues occur."))
            }

            # Outcome must remain binary after complete-case filtering
            if (!(length(unique(status_cc)) == 2 && all(unique(status_cc) %in% c(0, 1)))) {
                stop(.("Outcome is not binary after complete-case filtering. Check event level and missing-data pattern."))
            }

            n_events <- sum(status_cc == 1)
            if (n_events < 5) {
                stop(jmvcore::format(.('Too few events for analysis ({n}). Need at least 5 events for reliable estimation.'),
                    list(n = n_events)))
            }

            # Validate events-per-predictor ratio
            n_predictors <- ncol(predictors)
            if (n_events < n_predictors && n_events < 10) {
                warning(jmvcore::format(.('Low events-per-predictor ratio ({n_events} events, {n_pred} predictors). Consider stronger regularization or fewer predictors.'),
                    list(n_events = n_events, n_pred = n_predictors)))
            }

            # Create design matrix with robust factor handling
            tryCatch({
                factor_vars <- sapply(predictors[complete, , drop = FALSE], is.factor)
                if (any(factor_vars)) {
                    for (var_name in names(factor_vars)[factor_vars]) {
                        factor_levels <- length(unique(predictors[complete, var_name]))
                        if (factor_levels < 2) {
                            stop(jmvcore::format(.("Factor variable '{var}' has insufficient variation in complete cases."),
                                list(var = var_name)))
                        }
                    }
                }

                X <- model.matrix(~ ., data = predictors[complete, , drop = FALSE])[, -1, drop = FALSE]
                if (ncol(X) == 0) {
                    stop(.("No valid predictors remaining after model-matrix encoding."))
                }

                valid_cols <- apply(X, 2, function(col) {
                    !all(is.na(col)) && is.finite(var(col, na.rm = TRUE)) && var(col, na.rm = TRUE) > 0
                })
                if (!any(valid_cols)) {
                    stop(.("No valid predictors after removing degenerate design-matrix columns."))
                }
                X <- X[, valid_cols, drop = FALSE]
            }, error = function(e) {
                stop(jmvcore::format(.('Error creating design matrix: {msg}. Check factor coding and missing values.'),
                    list(msg = e$message)))
            })

            # Standardize variables if requested
            if (self$options$standardize) {
                X_scaled <- scale(X)
                scale_vals <- attr(X_scaled, "scaled:scale")
                scale_vals[is.na(scale_vals) | scale_vals == 0] <- 1
                attr(X_scaled, "scaled:scale") <- scale_vals
                X <- as.matrix(X_scaled)
                X[is.na(X)] <- 0
                scaling_info <- list(
                    center = attr(X_scaled, "scaled:center"),
                    scale = scale_vals
                )
            } else {
                scaling_info <- NULL
            }

            if (any(is.infinite(X))) {
                stop(.("Design matrix contains infinite values. Check for extreme outliers."))
            }

            return(list(
                time = time_cc,
                status = status_cc,
                X = X,
                n = n_complete,
                n_events = n_events,
                n_censored = n_complete - n_events,
                variable_names = colnames(X),
                original_variable_names = explanatory_vars,
                scaling_info = scaling_info,
                complete_cases = which(complete),
                event_level_used = event_level_used,
                excluded_rows = n_excluded
            ))
        },

        .makeStratifiedFoldId = function(status, nfolds, seed_value = NULL) {
            if (length(status) < nfolds) {
                return(NULL)
            }

            event_idx <- which(status == 1)
            cens_idx <- which(status == 0)
            if (length(event_idx) < nfolds || length(cens_idx) < nfolds) {
                return(NULL)
            }

            # Save and restore RNG state to avoid side effects on user's session
            old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
            on.exit({
                if (!is.null(old_seed)) {
                    assign(".Random.seed", old_seed, envir = .GlobalEnv)
                } else if (exists(".Random.seed", envir = .GlobalEnv)) {
                    rm(".Random.seed", envir = .GlobalEnv)
                }
            })

            if (!is.null(seed_value) && !is.na(seed_value)) {
                set.seed(seed_value)
            }

            foldid <- integer(length(status))
            foldid[event_idx] <- sample(rep(seq_len(nfolds), length.out = length(event_idx)))
            foldid[cens_idx] <- sample(rep(seq_len(nfolds), length.out = length(cens_idx)))

            if (length(unique(foldid)) < nfolds) {
                return(NULL)
            }
            foldid
        },

        .makeBinaryRiskGroups = function(risk_scores) {
            q <- as.numeric(stats::quantile(risk_scores, probs = c(0, 0.5, 1), na.rm = TRUE, type = 8))
            q <- unique(q)

            if (length(q) >= 3) {
                return(cut(
                    risk_scores,
                    breaks = q,
                    labels = c(.("Low Risk"), .("High Risk")),
                    include.lowest = TRUE
                ))
            }

            med <- stats::median(risk_scores, na.rm = TRUE)
            groups <- ifelse(risk_scores <= med, .("Low Risk"), .("High Risk"))
            groups <- factor(groups, levels = c(.("Low Risk"), .("High Risk")))
            if (length(unique(groups)) < 2) {
                return(NULL)
            }
            groups
        },

        # Enhanced model fitting with comprehensive error handling
        .fitModel = function(data) {
            # Validate package availability
            if (!requireNamespace("glmnet", quietly = TRUE) ||
                !requireNamespace("survival", quietly = TRUE)) {
                stop(.("Required packages 'glmnet' and 'survival' not available"))
            }
            
            # Create survival object
            y <- survival::Surv(data$time, data$status)
            
            # Validate survival object
            if (any(is.na(y))) {
                stop(.("Invalid survival object. Check time and status variables."))
            }

            # Set up cross-validation parameters
            nfolds_requested <- as.integer(self$options$nfolds)
            nfolds <- max(3, min(nfolds_requested, data$n - 1))
            if (nfolds != nfolds_requested) {
                warning(jmvcore::format(.('Reduced number of CV folds to {n} due to sample size constraints.'),
                    list(n = nfolds)))
            }

            seed_value <- tryCatch(as.integer(self$options$random_seed), error = function(e) NA_integer_)
            if (is.na(seed_value)) {
                seed_value <- 123456
            }

            foldid <- private$.makeStratifiedFoldId(data$status, nfolds, seed_value)
            if (is.null(foldid)) {
                warning(.("Could not create stratified CV folds; using default fold assignment."))
            }

            # Fit cross-validated Lasso-Cox model
            tryCatch({
                # Save and restore RNG state to avoid side effects on user's session
                old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
                on.exit({
                    if (!is.null(old_seed)) {
                        assign(".Random.seed", old_seed, envir = .GlobalEnv)
                    } else if (exists(".Random.seed", envir = .GlobalEnv)) {
                        rm(".Random.seed", envir = .GlobalEnv)
                    }
                })
                set.seed(seed_value)
                cv_args <- list(
                    x = data$X,
                    y = y,
                    family = "cox",
                    alpha = 1,  # Lasso (L1) penalty
                    standardize = FALSE,  # Already standardized if requested
                    parallel = FALSE  # Avoid parallel processing issues
                )
                if (is.null(foldid)) {
                    cv_args$nfolds <- nfolds
                } else {
                    cv_args$foldid <- foldid
                }
                private$.checkpoint()
                cv_fit <- do.call(glmnet::cv.glmnet, cv_args)
                
                # Check if cross-validation succeeded
                if (is.null(cv_fit$lambda.min) || is.na(cv_fit$lambda.min)) {
                    stop(.("Cross-validation failed. Check data quality and sample size."))
                }
                
            }, error = function(e) {
                stop(jmvcore::format(.('Error in cross-validation: {msg}'), list(msg = e$message)))
            })
            
            # Get optimal lambda based on user selection
            lambda_optimal <- switch(self$options$lambda,
                "lambda.min" = cv_fit$lambda.min,
                "lambda.1se" = cv_fit$lambda.1se,
                cv_fit$lambda.1se  # Default fallback
            )
            
            # Fit final model with optimal lambda
            tryCatch({
                final_model <- glmnet::glmnet(
                    x = data$X,
                    y = y,
                    family = "cox",
                    alpha = 1,
                    lambda = lambda_optimal,
                    standardize = FALSE
                )
                
            }, error = function(e) {
                stop(jmvcore::format(.('Error fitting final model: {msg}'), list(msg = e$message)))
            })
            
            # Extract coefficients and selected variables
            coef_matrix <- as.matrix(coef(final_model, s = lambda_optimal))
            selected_vars <- which(abs(coef_matrix) > 1e-8)
            
            if (length(selected_vars) == 0) {
                warning(.("No variables selected by Lasso. Consider using lambda.min or less regularization."))
                # Use lambda.min as fallback
                lambda_optimal <- cv_fit$lambda.min
                final_model <- glmnet::glmnet(
                    x = data$X, y = y, family = "cox",
                    alpha = 1, lambda = lambda_optimal, standardize = FALSE
                )
                coef_matrix <- as.matrix(coef(final_model, s = lambda_optimal))
                selected_vars <- which(abs(coef_matrix) > 1e-8)
            }
            
            # Calculate risk scores
            risk_scores <- as.numeric(predict(final_model, newx = data$X, s = lambda_optimal, type = "link"))
            
            # Calculate comprehensive performance metrics
            performance_metrics <- private$.calculatePerformanceMetrics(y, risk_scores, data)
            
            # Calculate variable importance (absolute coefficient values)
            if (length(selected_vars) > 0) {
                var_importance <- abs(coef_matrix[selected_vars, 1])
                names(var_importance) <- data$variable_names[selected_vars]
                var_importance <- sort(var_importance, decreasing = TRUE)
            } else {
                var_importance <- numeric(0)
            }
            
            return(list(
                cv_fit = cv_fit,
                final_model = final_model,
                coef_matrix = coef_matrix,
                selected_vars = selected_vars,
                lambda_optimal = lambda_optimal,
                risk_scores = risk_scores,
                performance_metrics = performance_metrics,
                var_importance = var_importance,
                data = data
            ))
        },
        
        # Calculate comprehensive performance metrics
        .calculatePerformanceMetrics = function(y, risk_scores, data) {
            metrics <- list()
            
            # C-index (concordance index)
            # Note: higher risk_scores = worse prognosis, so we need reverse=TRUE
            # because survival::concordance() by default treats higher values as better
            tryCatch({
                cindex_result <- survival::concordance(y ~ risk_scores, reverse = TRUE)
                metrics$cindex <- cindex_result$concordance
                metrics$cindex_se <- sqrt(max(0, cindex_result$var))
            }, error = function(e) {
                metrics$cindex <<- NA
                metrics$cindex_se <<- NA
            })
            
            # Risk group analysis
            tryCatch({
                risk_groups <- private$.makeBinaryRiskGroups(risk_scores)
                if (is.null(risk_groups)) {
                    stop(.("Unable to form two risk groups from risk scores."))
                }
                
                # Log-rank test
                fit <- survival::survdiff(y ~ risk_groups)
                metrics$logrank_p <- 1 - pchisq(fit$chisq, df = length(fit$n) - 1)
                
                # Hazard ratio between risk groups
                cox_fit <- survival::coxph(y ~ risk_groups)
                hr <- exp(as.numeric(coef(cox_fit)[1]))
                ci <- exp(stats::confint(cox_fit))
                metrics$hazard_ratio <- hr
                metrics$hr_ci_lower <- as.numeric(ci[1, 1])
                metrics$hr_ci_upper <- as.numeric(ci[1, 2])
                
            }, error = function(e) {
                metrics$logrank_p <<- NA
                metrics$hazard_ratio <<- NA
                metrics$hr_ci_lower <<- NA
                metrics$hr_ci_upper <<- NA
            })
            
            return(metrics)
        },

        # Enhanced table population methods
        .populateModelSummary = function(results) {
            table <- self$results$modelSummary
            
            # Clear existing rows
            table$deleteRows()
            
            # Add model summary statistics
            table$addRow(rowKey = 1, values = list(
                statistic = .("Total Variables"),
                value = ncol(results$data$X)
            ))

            table$addRow(rowKey = 2, values = list(
                statistic = .("Selected Variables"),
                value = length(results$selected_vars)
            ))

            table$addRow(rowKey = 3, values = list(
                statistic = .("Selection Proportion"),
                value = paste0(round(100 * length(results$selected_vars) / ncol(results$data$X), 1), "%")
            ))

            table$addRow(rowKey = 4, values = list(
                statistic = .("Optimal Lambda"),
                value = format(results$lambda_optimal, scientific = TRUE, digits = 3)
            ))

            table$addRow(rowKey = 5, values = list(
                statistic = .("Sample Size"),
                value = results$data$n
            ))

            table$addRow(rowKey = 6, values = list(
                statistic = .("Number of Events"),
                value = results$data$n_events
            ))

            table$addRow(rowKey = 7, values = list(
                statistic = .("Censoring Rate"),
                value = paste0(round(100 * results$data$n_censored / results$data$n, 1), "%")
            ))

            table$addRow(rowKey = 8, values = list(
                statistic = .("Event Level Used"),
                value = results$data$event_level_used
            ))

            if (!is.null(results$data$excluded_rows) && results$data$excluded_rows > 0) {
                table$addRow(rowKey = 9, values = list(
                    statistic = .("Rows Excluded (Missing Data)"),
                    value = results$data$excluded_rows
                ))
            }
        },

        .populateCoefficients = function(results) {
            table <- self$results$coefficients
            
            # Clear existing rows
            table$deleteRows()
            
            if (length(results$selected_vars) == 0) {
                table$addRow(rowKey = 1, values = list(
                    variable = .("No variables selected"),
                    coefficient = NA,
                    hazardRatio = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    p_value = NA,
                    importance = NA
                ))
                return()
            }
            
            # Post-selection unpenalized Cox refit for CIs and p-values
            refit_results <- NULL
            tryCatch({
                selected_X <- results$data$X[, results$selected_vars, drop = FALSE]
                y <- survival::Surv(results$data$time, results$data$status)
                refit_df <- as.data.frame(selected_X)
                refit_df$.time <- results$data$time
                refit_df$.status <- results$data$status
                # Only refit if p < n (standard Cox requires this)
                if (ncol(selected_X) < nrow(selected_X)) {
                    refit_formula <- as.formula(paste0(
                        "survival::Surv(.time, .status) ~ ",
                        paste0("`", colnames(selected_X), "`", collapse = " + ")
                    ))
                    refit_cox <- survival::coxph(refit_formula, data = refit_df)
                    refit_summary <- summary(refit_cox)
                    refit_results <- list(
                        coefs = refit_summary$coefficients,
                        ci = refit_summary$conf.int
                    )
                }
            }, error = function(e) {
                # Silently fall back to LASSO coefficients without CIs
                refit_results <<- NULL
            })

            # Add coefficient rows for selected variables
            for (i in seq_along(results$selected_vars)) {
                var_idx <- results$selected_vars[i]
                var_name <- results$data$variable_names[var_idx]
                coef_val <- results$coef_matrix[var_idx, 1]
                hr_val <- exp(coef_val)
                importance <- results$var_importance[var_name]

                # Try to get CI and p-value from post-selection Cox refit
                ci_lo <- NA
                ci_up <- NA
                p_val <- NA
                if (!is.null(refit_results)) {
                    # Match variable name in refit results
                    refit_row <- match(var_name, rownames(refit_results$coefs))
                    if (!is.na(refit_row)) {
                        ci_lo <- refit_results$ci[refit_row, 3]  # lower .95
                        ci_up <- refit_results$ci[refit_row, 4]  # upper .95
                        p_val <- refit_results$coefs[refit_row, 5]  # Pr(>|z|)
                        # Use refit HR/coef for consistency with CIs
                        hr_val <- refit_results$ci[refit_row, 1]  # exp(coef)
                        coef_val <- refit_results$coefs[refit_row, 1]  # coef
                    }
                }

                table$addRow(rowKey = i, values = list(
                    variable = var_name,
                    coefficient = coef_val,
                    hazardRatio = hr_val,
                    ci_lower = ci_lo,
                    ci_upper = ci_up,
                    p_value = p_val,
                    importance = round(importance, 4)
                ))
            }

            # Notes about methodology
            if (!is.null(refit_results)) {
                table$setNote("refit",
                    .("CIs and p-values are from an unpenalized Cox model refitted with LASSO-selected variables (post-selection inference). These do not account for the variable selection step and may be anti-conservative."))
            } else {
                table$setNote("noci",
                    .("CIs and p-values could not be computed (post-selection Cox refit failed or p >= n)."))
            }
            if (!is.null(results$data$scaling_info)) {
                table$setNote("scale",
                    .("Coefficients are on the standardized scale (per 1-SD change in the predictor)."))
            }
        },

        .populatePerformance = function(results) {
            table <- self$results$performance
            metrics <- results$performance_metrics
            
            # Clear existing rows
            table$deleteRows()
            
            # C-index
            if (!is.na(metrics$cindex)) {
                cindex_text <- if (!is.na(metrics$cindex_se)) {
                    paste0(round(metrics$cindex, 3), " (SE: ", round(metrics$cindex_se, 3), ")")
                } else {
                    round(metrics$cindex, 3)
                }
                
                table$addRow(rowKey = 1, values = list(
                    metric = .("C-index"),
                    value = cindex_text,
                    interpretation = private$.interpretCindex(metrics$cindex)
                ))
                table$setNote("cindex",
                    .("All performance metrics are apparent (training) values computed on the same data used for model fitting. They may be optimistic. External validation is recommended."))
            }

            # Log-rank test p-value
            if (!is.null(metrics$logrank_p) && !is.na(metrics$logrank_p)) {
                table$addRow(rowKey = 2, values = list(
                    metric = .("Log-rank p-value"),
                    value = if (metrics$logrank_p < 0.001) "< 0.001" else round(metrics$logrank_p, 3),
                    interpretation = if (metrics$logrank_p < 0.05) .("Significant risk stratification") else .("Non-significant stratification")
                ))
            }

            # Hazard ratio between risk groups
            if (!is.null(metrics$hazard_ratio) && !is.na(metrics$hazard_ratio)) {
                hr_text <- if (!is.null(metrics$hr_ci_lower) && !is.na(metrics$hr_ci_lower) &&
                               !is.null(metrics$hr_ci_upper) && !is.na(metrics$hr_ci_upper)) {
                    paste0(round(metrics$hazard_ratio, 2), " (95% CI: ",
                          round(metrics$hr_ci_lower, 2), "-", round(metrics$hr_ci_upper, 2), ")")
                } else {
                    round(metrics$hazard_ratio, 2)
                }

                table$addRow(rowKey = 3, values = list(
                    metric = .("Hazard Ratio (High vs Low Risk)"),
                    value = hr_text,
                    interpretation = private$.interpretHazardRatio(metrics$hazard_ratio)
                ))
            }
        },
        
        # Helper functions for interpretation
        .interpretCindex = function(cindex) {
            if (is.na(cindex)) return(.("Not available"))
            if (cindex < 0.6) return(.("Poor discrimination"))
            if (cindex < 0.7) return(.("Fair discrimination"))
            if (cindex < 0.8) return(.("Good discrimination"))
            return(.("Excellent discrimination"))
        },

        .interpretHazardRatio = function(hr) {
            if (is.na(hr)) return(.("Not available"))
            if (hr < 1.5) return(.("Weak risk stratification"))
            if (hr < 2.5) return(.("Moderate risk stratification"))
            return(.("Strong risk stratification"))
        },

        # Enhanced plotting functions
        .cvPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$cv_plot) return()

            state <- image$state
            if (is.null(state)) return()

            # Build data frame from plain numeric state
            cv_data <- data.frame(
                lambda = state$lambda,
                cvm    = state$cvm,
                cvsd   = state$cvsd,
                cvup   = state$cvup,
                cvlo   = state$cvlo
            )

            p <- ggplot2::ggplot(cv_data, ggplot2::aes(x = log(lambda), y = cvm)) +
                ggplot2::geom_point(color = "red", size = 0.8) +
                ggplot2::geom_errorbar(ggplot2::aes(ymin = cvlo, ymax = cvup),
                                      color = "darkgrey", width = 0.02) +
                ggplot2::geom_vline(xintercept = log(state$lambda_min),
                                   linetype = "dashed", color = "blue") +
                ggplot2::geom_vline(xintercept = log(state$lambda_1se),
                                   linetype = "dashed", color = "green") +
                ggplot2::labs(
                    title = .("Cross-Validation Plot"),
                    subtitle = .("Blue: lambda.min, Green: lambda.1se"),
                    x = .("Log Lambda"),
                    y = .("Partial Likelihood Deviance")
                ) +
                ggtheme

            print(p)
            TRUE
        },

        .coefPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$coef_plot) return()

            state <- image$state
            if (is.null(state) || length(state$var_names) == 0) return()

            # Build data frame from plain state
            coef_data <- data.frame(
                variable   = factor(state$var_names, levels = state$var_names),
                coefficient = state$coef_values,
                importance  = state$var_importance
            )

            p <- ggplot2::ggplot(coef_data, ggplot2::aes(x = variable, y = coefficient)) +
                ggplot2::geom_col(ggplot2::aes(fill = coefficient > 0), alpha = 0.7) +
                ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                                          labels = c(.("Protective"), .("Risk Factor")),
                                          name = .("Effect")) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = .("Selected Variables and Coefficients"),
                    subtitle = if (!is.null(self$options$standardize) && self$options$standardize) .("Coefficients reflect standardized effect sizes.") else NULL,
                    x = .("Variables"),
                    y = .("Coefficient")
                ) +
                ggtheme

            print(p)
            TRUE
        },

        .survivalPlot = function(image, ggtheme, theme, ...) {
            if (!self$options$survival_plot) return()

            state <- image$state
            if (is.null(state)) return()

            # Check if risk scores are available and valid
            if (is.null(state$risk_scores) || length(state$risk_scores) == 0) {
                text_warning <- .("No risk scores available.\nLASSO selected no variables.\n\nConsider using:\n\u2022 lambda.min instead of lambda.1se\n\u2022 Less regularization (lower lambda)\n\u2022 More explanatory variables")
                
                # Create a new page with proper formatting
                grid::grid.newpage()
                # Create a viewport with margins for better readability
                vp <- grid::viewport(
                  width = 0.9,    # Wider viewport for left-aligned text
                  height = 0.9,   # Keep reasonable margins
                  x = 0.5,        # Center the viewport
                  y = 0.5         # Center the viewport
                )
                grid::pushViewport(vp)
                # Add the text with left alignment
                grid::grid.text(
                  text_warning,
                  x = 0.05,           # Move text to the left (5% margin)
                  y = 0.95,           # Start from top (5% margin)
                  just = c("left", "top"),  # Left align and top justify
                  gp = grid::gpar(
                    fontsize = 11,        # Maintain readable size
                    fontface = "plain",   # Regular font
                    lineheight = 1.3,     # Slightly increased line spacing for readability
                    col = "red"           # Red color for warning
                  )
                )
                # Reset viewport
                grid::popViewport()
                return(TRUE)
            }
            
            # Check if all risk scores are the same (no discrimination)
            if (length(unique(state$risk_scores)) <= 1) {
                text_warning <- .("Risk scores are uniform.\nNo discrimination possible.\n\nThis can happen when:\n\u2022 All selected variables have very small coefficients\n\u2022 The model overfits to noise\n\u2022 Lambda value is inappropriate")
                
                grid::grid.newpage()
                vp <- grid::viewport(width = 0.9, height = 0.9, x = 0.5, y = 0.5)
                grid::pushViewport(vp)
                grid::grid.text(
                  text_warning,
                  x = 0.05, y = 0.95,
                  just = c("left", "top"),
                  gp = grid::gpar(fontsize = 11, fontface = "plain", lineheight = 1.3, col = "orange")
                )
                grid::popViewport()
                return(TRUE)
            }
            
            # Create risk groups based on median risk score
            tryCatch({
                risk_groups <- private$.makeBinaryRiskGroups(state$risk_scores)
                if (is.null(risk_groups)) {
                    stop(.("Unable to create two risk groups from risk scores."))
                }

                # Check if we have valid data
                if (is.null(state$time) || is.null(state$status)) {
                    text_warning <- .("Survival data not available.\n\nPlease check that:\n\u2022 Time and outcome variables are properly selected\n\u2022 Data contains valid survival information")
                    
                    grid::grid.newpage()
                    vp <- grid::viewport(width = 0.9, height = 0.9, x = 0.5, y = 0.5)
                    grid::pushViewport(vp)
                    grid::grid.text(
                      text_warning,
                      x = 0.05, y = 0.95,
                      just = c("left", "top"),
                      gp = grid::gpar(fontsize = 11, fontface = "plain", lineheight = 1.3, col = "red")
                    )
                    grid::popViewport()
                    return(TRUE)
                }
                
                # Create complete data frame for survminer
                plot_data <- data.frame(
                    time = state$time,
                    status = state$status,
                    risk_groups = risk_groups
                )
                
                # Remove any rows with missing data
                plot_data <- plot_data[complete.cases(plot_data), ]
                
                if (nrow(plot_data) == 0) {
                    text_warning <- .("No complete survival data available.\n\nThis can occur when:\n\u2022 There are missing values in time or outcome variables\n\u2022 Risk score calculation failed\n\u2022 Data filtering removed all observations")
                    
                    grid::grid.newpage()
                    vp <- grid::viewport(width = 0.9, height = 0.9, x = 0.5, y = 0.5)
                    grid::pushViewport(vp)
                    grid::grid.text(
                      text_warning,
                      x = 0.05, y = 0.95,
                      just = c("left", "top"),
                      gp = grid::gpar(fontsize = 11, fontface = "plain", lineheight = 1.3, col = "red")
                    )
                    grid::popViewport()
                    return(TRUE)
                }
                
                # Fit survival curves using column names in formula
                fit <- survival::survfit(survival::Surv(time, status) ~ risk_groups, data = plot_data)
                
                # Create enhanced survival plot
                if (requireNamespace("survminer", quietly = TRUE)) {
                    p <- survminer::ggsurvplot(
                        fit,
                        data = plot_data,
                        risk.table = TRUE,
                        risk.table.y.text = FALSE,
                        pval = TRUE,
                        conf.int = TRUE,
                        ggtheme = ggtheme,
                        title = .("Survival Curves by Risk Groups"),
                        xlab = .("Time"),
                        ylab = .("Survival Probability"),
                        legend.title = .("Risk Group"),
                        legend.labs = c(.("Low Risk"), .("High Risk")),
                        palette = c("#2166AC", "#B2182B")
                    )
                    print(p)
                } else {
                    # Fallback to base plot
                    plot(fit, col = c("blue", "red"), lwd = 2,
                         xlab = .("Time"), ylab = .("Survival Probability"),
                         main = .("Survival Curves by Risk Groups"))
                    legend("topright", legend = c(.("Low Risk"), .("High Risk")),
                           col = c("blue", "red"), lwd = 2)
                }

            }, error = function(e) {
                # Handle any errors gracefully using grid graphics
                text_warning <- jmvcore::format(.("Error creating survival plot:\n{msg}\n\nPlease check your data and model parameters."), list(msg = e$message))
                
                grid::grid.newpage()
                vp <- grid::viewport(width = 0.9, height = 0.9, x = 0.5, y = 0.5)
                grid::pushViewport(vp)
                grid::grid.text(
                  text_warning,
                  x = 0.05, y = 0.95,
                  just = c("left", "top"),
                  gp = grid::gpar(fontsize = 11, fontface = "plain", lineheight = 1.3, col = "red")
                )
                grid::popViewport()
            })
            
            TRUE
        },

        .savePlotData = function(results) {
            # Save PLAIN data for plot rendering (no glmnet/cv.glmnet objects)
            # to avoid protobuf serialization errors with function references
            if (self$options$cv_plot) {
                cv_plot_data <- list(
                    lambda = as.numeric(results$cv_fit$lambda),
                    cvm = as.numeric(results$cv_fit$cvm),
                    cvsd = as.numeric(results$cv_fit$cvsd),
                    cvup = as.numeric(results$cv_fit$cvup),
                    cvlo = as.numeric(results$cv_fit$cvlo),
                    lambda_min = as.numeric(results$cv_fit$lambda.min),
                    lambda_1se = as.numeric(results$cv_fit$lambda.1se)
                )
                self$results$cv_plot$setState(cv_plot_data)
                # Add CV plot explanation
                if (self$options$showExplanations) {
                    private$.populateCrossValidationExplanation()
                }
            }

            if (self$options$coef_plot && length(results$selected_vars) > 0) {
                # Align coef_values with var_importance ordering (sorted by |coef|)
                all_coefs <- results$coef_matrix[, 1]
                names(all_coefs) <- results$data$variable_names
                aligned_coefs <- all_coefs[names(results$var_importance)]
                coef_plot_data <- list(
                    var_names = names(results$var_importance),
                    var_importance = as.numeric(results$var_importance),
                    coef_values = as.numeric(aligned_coefs)
                )
                self$results$coef_plot$setState(coef_plot_data)
                # Add regularization path explanation
                if (self$options$showExplanations) {
                    private$.populateRegularizationPathExplanation()
                }
            } else if (self$options$coef_plot) {
                self$results$coef_plot$setState(NULL)
            }

            if (self$options$survival_plot) {
                survival_plot_data <- as.data.frame(list(
                    time = as.numeric(results$data$time),
                    status = as.integer(results$data$status),
                    risk_scores = as.numeric(results$risk_scores)
                ))
                self$results$survival_plot$setState(survival_plot_data)
                # Add risk score explanation
                if (self$options$showExplanations) {
                    private$.populateRiskScoreExplanation()
                }
            }
            
            # Populate additional analysis tables
            if (self$options$showVariableImportance) {
                private$.populateVariableImportance(results)
            }
            
            if (self$options$showModelComparison) {
                private$.populateModelComparison(results)
            }
            
            # Populate natural-language summary
            if (self$options$showSummary) {
                private$.populateSummary(results)
            }

            # Add risk scores to dataset if requested
            if (!is.null(self$results$riskScore)) {
                # Create full-length vector with NAs for missing cases
                full_risk_scores <- rep(NA, nrow(self$data))
                full_risk_scores[results$data$complete_cases] <- results$risk_scores
                self$results$riskScore$setValues(full_risk_scores)
            }
        },

        # Explanatory Functions
        .initializeExplanations = function() {
            # Main LASSO Cox explanation
            if (self$options$showExplanations) {
                private$.populateLassoExplanation()
            }
            
            # Methodology notes
            if (self$options$showMethodologyNotes) {
                private$.populateMethodologyNotes()
            }
            
            # Clinical guidance
            if (self$options$includeClinicalGuidance) {
                private$.populateClinicalGuidance()
            }
        },

        .populateLassoExplanation = function() {
            html_content <- "
            <h3>Understanding LASSO Cox Regression</h3>
            
            <div class='alert alert-info'>
                <h4> What is LASSO Cox Regression?</h4>
                <p>LASSO (Least Absolute Shrinkage and Selection Operator) Cox regression combines the Cox proportional hazards model with LASSO regularization for automatic variable selection in survival analysis.</p>
            </div>
            
            <h4> Key Concepts:</h4>
            <ul>
                <li><strong>Regularization (λ):</strong> A penalty parameter that controls the strength of variable selection
                    <ul>
                        <li>Higher λ → More variables excluded (simpler model)</li>
                        <li>Lower λ → More variables included (complex model)</li>
                        <li>λ = 0 → Standard Cox regression (no penalty)</li>
                    </ul>
                </li>
                
                <li><strong>Variable Selection:</strong> LASSO automatically sets coefficients of unimportant variables to exactly zero</li>
                
                <li><strong>Cross-Validation:</strong> K-fold CV determines optimal λ that minimizes prediction error</li>
                
                <li><strong>Shrinkage:</strong> Coefficients of selected variables are shrunk toward zero, reducing overfitting</li>
            </ul>
            
            <h4> How to Interpret Results:</h4>
            <ul>
                <li><strong>Selected Variables:</strong> Variables with non-zero coefficients at optimal λ</li>
                <li><strong>Coefficients:</strong> Log hazard ratios (positive = increased risk, negative = decreased risk)</li>
                <li><strong>Risk Scores:</strong> Linear combination of selected variables weighted by their coefficients</li>
                <li><strong>C-index:</strong> Discrimination ability (0.5 = no discrimination, 1.0 = perfect discrimination)</li>
            </ul>
            
            <div class='alert alert-success'>
                <h4> Advantages of LASSO Cox:</h4>
                <ul>
                    <li>Automatic variable selection eliminates manual stepwise procedures</li>
                    <li>Reduces overfitting in high-dimensional data</li>
                    <li>Handles multicollinearity by selecting one variable from correlated groups</li>
                    <li>Provides parsimonious models suitable for clinical prediction</li>
                    <li>Objective, reproducible variable selection process</li>
                </ul>
            </div>
            
            <div class='alert alert-warning'>
                <h4> Important Considerations:</h4>
                <ul>
                    <li>Variable selection depends on the specific dataset and may vary with new data</li>
                    <li>Standardization is typically required for fair penalization across variables</li>
                    <li>Results should be validated in independent datasets</li>
                    <li>Clinical expertise should guide final model interpretation</li>
                </ul>
            </div>
            "
            
            self$results$lassoExplanation$setContent(html_content)
        },

        .populateMethodologyNotes = function() {
            html_content <- "
            <h3>LASSO Cox Methodology Notes</h3>
            
            <h4> Technical Details:</h4>
            
            <div class='alert alert-primary'>
                <h5>Mathematical Foundation</h5>
                <p>LASSO Cox regression minimizes the negative partial log-likelihood with an L1 penalty:</p>
                <p><strong>Objective Function:</strong> -ℓ(β) + λ Σ|βⱼ|</p>
                <ul>
                    <li>ℓ(β): Partial log-likelihood from Cox model</li>
                    <li>λ: Regularization parameter</li>
                    <li>Σ|βⱼ|: L1 penalty (sum of absolute coefficients)</li>
                </ul>
            </div>
            
            <h4> Algorithm Steps:</h4>
            <ol>
                <li><strong>Data Preprocessing:</strong>
                    <ul>
                        <li>Handle missing values (complete case analysis or imputation)</li>
                        <li>Standardize continuous variables (mean=0, SD=1)</li>
                        <li>Create dummy variables for categorical predictors</li>
                    </ul>
                </li>
                
                <li><strong>Cross-Validation:</strong>
                    <ul>
                        <li>Divide data into K folds (typically K=10)</li>
                        <li>For each λ value, train on K-1 folds and validate on remaining fold</li>
                        <li>Calculate cross-validated partial likelihood deviance</li>
                        <li>Select λ that minimizes CV error</li>
                    </ul>
                </li>
                
                <li><strong>Final Model:</strong>
                    <ul>
                        <li>Fit LASSO Cox model on full data using optimal λ</li>
                        <li>Extract non-zero coefficients (selected variables)</li>
                        <li>Calculate risk scores: Σ(βⱼ × xⱼ)</li>
                    </ul>
                </li>
                
                <li><strong>Performance Assessment:</strong>
                    <ul>
                        <li>Calculate C-index (concordance probability)</li>
                        <li>Perform log-rank test between risk groups</li>
                        <li>Generate survival curves for risk stratification</li>
                    </ul>
                </li>
            </ol>
            
            <h4> Hyperparameter Selection:</h4>
            <ul>
                <li><strong>λ.min:</strong> Lambda that minimizes CV error</li>
                <li><strong>λ.1se:</strong> Largest lambda within 1 SE of minimum (more parsimonious)</li>
                <li><strong>Choice:</strong> λ.1se often preferred for better generalizability</li>
            </ul>
            
            <h4> Variable Importance Metrics:</h4>
            <ul>
                <li><strong>Coefficient Magnitude:</strong> Larger |β| indicates stronger effect</li>
                <li><strong>Path Inclusion Proportion:</strong> Fraction of lambda values in the regularization path where the variable has a non-zero coefficient</li>
                <li><strong>Stability:</strong> Consistency of coefficient estimates across folds</li>
                <li><strong>Path Entry:</strong> Lambda value at which variable first enters the model</li>
            </ul>
            
            <div class='alert alert-info'>
                <h5> Implementation Notes</h5>
                <ul>
                    <li>Uses coordinate descent algorithm for optimization</li>
                    <li>Handles ties in survival times using Breslow approximation (glmnet default)</li>
                    <li>Standardization (if enabled) is applied to predictors but not survival times</li>
                    <li>When standardization is enabled, coefficients are on the standardized scale</li>
                </ul>
            </div>
            "
            
            self$results$methodologyNotes$setContent(html_content)
        },

        .populateClinicalGuidance = function() {
            html_content <- "
            <h3>Clinical Interpretation Guidance</h3>
            
            <div class='alert alert-success'>
                <h4> Clinical Applications</h4>
                <ul>
                    <li><strong>Prognostic Models:</strong> Identify key factors affecting patient survival</li>
                    <li><strong>Risk Stratification:</strong> Classify patients into risk groups for treatment planning</li>
                    <li><strong>Biomarker Discovery:</strong> Screen large sets of potential biomarkers</li>
                    <li><strong>Clinical Decision Support:</strong> Develop tools for personalized medicine</li>
                </ul>
            </div>
            
            <h4> Interpreting Model Results:</h4>
            
            <div class='row'>
                <div class='col-md-6'>
                    <h5> Coefficients & Hazard Ratios</h5>
                    <ul>
                        <li><strong>Positive coefficient:</strong> Higher values increase hazard (worse prognosis)</li>
                        <li><strong>Negative coefficient:</strong> Higher values decrease hazard (better prognosis)</li>
                        <li><strong>Hazard Ratio = exp(coefficient)</strong></li>
                        <li><strong>Example:</strong> Coefficient = 0.693 → HR = 2.0 (doubled risk)</li>
                    </ul>
                </div>
                
                <div class='col-md-6'>
                    <h5> Risk Scores</h5>
                    <ul>
                        <li><strong>Higher risk score:</strong> Worse expected survival</li>
                        <li><strong>Lower risk score:</strong> Better expected survival</li>
                        <li><strong>Threshold:</strong> Often use median to split low/high risk</li>
                        <li><strong>Validation:</strong> Test in independent cohorts</li>
                    </ul>
                </div>
            </div>
            
            <h4> Clinical Decision Making:</h4>
            <ul>
                <li><strong>Treatment Selection:</strong> High-risk patients may benefit from aggressive therapy</li>
                <li><strong>Follow-up Intensity:</strong> Adjust monitoring frequency based on risk</li>
                <li><strong>Patient Counseling:</strong> Inform patients about prognosis and options</li>
                <li><strong>Clinical Trial Stratification:</strong> Balance risk factors between treatment arms</li>
            </ul>
            
            <h4> Model Performance Assessment:</h4>
            <table class='table table-sm'>
                <thead>
                    <tr><th>C-index Range</th><th>Discrimination</th><th>Clinical Utility</th></tr>
                </thead>
                <tbody>
                    <tr><td>0.50 - 0.60</td><td>Poor</td><td>Limited clinical value</td></tr>
                    <tr><td>0.60 - 0.70</td><td>Acceptable</td><td>May inform decisions</td></tr>
                    <tr><td>0.70 - 0.80</td><td>Good</td><td>Useful for risk stratification</td></tr>
                    <tr><td>0.80 - 0.90</td><td>Excellent</td><td>Strong clinical utility</td></tr>
                    <tr><td>> 0.90</td><td>Outstanding</td><td>May indicate overfitting</td></tr>
                </tbody>
            </table>
            
            <div class='alert alert-warning'>
                <h4> Clinical Validation Requirements:</h4>
                <ul>
                    <li><strong>External Validation:</strong> Test model in different populations</li>
                    <li><strong>Temporal Validation:</strong> Verify performance over time</li>
                    <li><strong>Clinical Impact:</strong> Demonstrate improved patient outcomes</li>
                    <li><strong>Implementation:</strong> Consider practical feasibility in clinical workflow</li>
                </ul>
            </div>
            
            <div class='alert alert-info'>
                <h4> Reporting Recommendations:</h4>
                <ul>
                    <li>Report both λ.min and λ.1se results</li>
                    <li>Describe variable selection process and stability</li>
                    <li>Provide confidence intervals for C-index</li>
                    <li>Include calibration assessment when possible</li>
                    <li>Discuss clinical context and limitations</li>
                    <li>Share code and data for reproducibility</li>
                </ul>
            </div>
            
            <h4> Model Updating and Maintenance:</h4>
            <ul>
                <li><strong>Regular Validation:</strong> Monitor performance with new data</li>
                <li><strong>Model Recalibration:</strong> Update when performance degrades</li>
                <li><strong>Variable Drift:</strong> Check for changes in predictor distributions</li>
                <li><strong>Outcome Definition:</strong> Ensure consistent endpoint definitions</li>
            </ul>
            "
            
            self$results$clinicalGuidance$setContent(html_content)
        },

        .populateCrossValidationExplanation = function() {
            html_content <- "
            <h4> Understanding the Cross-Validation Plot</h4>
            
            <div class='alert alert-info'>
                <p>The cross-validation plot shows how model performance varies with different levels of regularization (λ values).</p>
            </div>
            
            <ul>
                <li><strong>X-axis:</strong> Log(λ) - Regularization strength (left = weak, right = strong)</li>
                <li><strong>Y-axis:</strong> Partial likelihood deviance (lower = better fit)</li>
                <li><strong>Error bars:</strong> Standard errors across CV folds</li>
                <li><strong>Vertical lines:</strong> 
                    <ul>
                        <li>Left line: λ.min (minimum CV error)</li>
                        <li>Right line: λ.1se (most regularization within 1 SE)</li>
                    </ul>
                </li>
                <li><strong>Numbers at top:</strong> Number of non-zero variables at each λ</li>
            </ul>
            
            <p><strong>Interpretation:</strong> Choose λ.1se (right line) for more robust, generalizable models with fewer variables.</p>
            "
            
            self$results$crossValidationExplanation$setContent(html_content)
        },

        .populateRegularizationPathExplanation = function() {
            html_content <- "
            <h4> Understanding the Coefficient Summary Plot</h4>
            
            <div class='alert alert-info'>
                <p>This plot summarizes the selected variables and their coefficient values at the chosen lambda.</p>
            </div>
            
            <ul>
                <li><strong>X-axis:</strong> Selected variables</li>
                <li><strong>Y-axis:</strong> Coefficient values at the selected lambda</li>
                <li><strong>Color coding:</strong> Direction of effect (higher vs lower hazard)</li>
                <li><strong>Magnitude:</strong> Larger absolute coefficients indicate stronger contribution to risk score</li>
            </ul>
            
            <p><strong>Variable Selection:</strong> Variables with non-zero coefficients at the chosen λ are included in the final model.</p>
            "
            
            self$results$regularizationPathExplanation$setContent(html_content)
        },

        .populateRiskScoreExplanation = function() {
            html_content <- "
            <h4> Understanding Risk Scores and Survival Curves</h4>
            
            <div class='alert alert-info'>
                <p>Risk scores combine selected variables to predict individual patient risk. Survival curves show outcomes for different risk groups.</p>
            </div>
            
            <h5>Risk Score Calculation:</h5>
            <ul>
                <li><strong>Formula:</strong> Risk Score = β₁×X₁ + β₂×X₂ + ... + βₖ×Xₖ</li>
                <li><strong>Higher scores:</strong> Increased risk (worse prognosis)</li>
                <li><strong>Lower scores:</strong> Decreased risk (better prognosis)</li>
                <li><strong>Risk groups:</strong> Often split at median (low vs high risk)</li>
            </ul>
            
            <h5>Survival Curve Interpretation:</h5>
            <ul>
                <li><strong>Y-axis:</strong> Survival probability (1.0 = 100% survival)</li>
                <li><strong>X-axis:</strong> Time (same units as input data)</li>
                <li><strong>Curve separation:</strong> Better separation indicates stronger risk prediction</li>
                <li><strong>P-value:</strong> Statistical significance of risk group differences</li>
                <li><strong>Risk table:</strong> Number of patients at risk at each time point</li>
            </ul>
            "
            
            self$results$riskScoreExplanation$setContent(html_content)
        },

        .populateVariableImportance = function(results) {
            table <- self$results$variableImportance
            table$deleteRows()
            
            if (length(results$selected_vars) == 0) {
                table$addRow(rowKey = 1, values = list(
                    variable = .("No variables selected"),
                    importance_score = NA,
                    selection_frequency = NA,
                    stability_rank = NA
                ))
                return()
            }
            
            # Pre-compute selection frequencies from the regularization path
            all_coefs <- tryCatch(
                as.matrix(coef(results$cv_fit$glmnet.fit)),
                error = function(e) NULL
            )
            n_lambdas <- if (!is.null(all_coefs)) ncol(all_coefs) else 0

            # Compute importance-based ranks
            abs_coefs <- abs(results$coef_matrix[results$selected_vars, 1])
            importance_order <- order(abs_coefs, decreasing = TRUE)
            importance_ranks <- integer(length(results$selected_vars))
            importance_ranks[importance_order] <- seq_along(importance_order)

            for (i in seq_along(results$selected_vars)) {
                var_idx <- results$selected_vars[i]
                var_name <- results$data$variable_names[var_idx]
                coef_val <- abs(results$coef_matrix[var_idx, 1])

                importance <- coef_val

                # Selection frequency: fraction of lambda path where variable is non-zero
                freq <- if (!is.null(all_coefs) && n_lambdas > 0) {
                    sum(all_coefs[var_idx, ] != 0) / n_lambdas
                } else {
                    NA
                }

                table$addRow(rowKey = i, values = list(
                    variable = var_name,
                    importance_score = importance,
                    selection_frequency = freq,
                    stability_rank = importance_ranks[i]
                ))
            }
        },

        # ── Data Suitability Assessment ─────────────────────────────
        .assessSuitability = function(data) {
            checks <- list()
            n <- data$n
            n_events <- data$n_events
            p <- ncol(data$X)
            event_rate <- n_events / n

            # -- Check 1: Events-Per-Variable (EPV) --
            epv <- n_events / p
            if (epv >= 20) {
                checks$epv <- list(
                    color = "green", label = .("Events-Per-Variable"),
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = .("Excellent EPV ratio. Reliable coefficient estimates expected.")
                )
            } else if (epv >= 2) {
                checks$epv <- list(
                    color = "yellow", label = .("Events-Per-Variable"),
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = .("Adequate for LASSO (which handles low EPV better than standard Cox), but interpret with caution.")
                )
            } else {
                checks$epv <- list(
                    color = "red", label = .("Events-Per-Variable"),
                    value = sprintf("%.1f (n_events=%d, p=%d)", epv, n_events, p),
                    detail = .("Very low EPV. Results may be unreliable even with LASSO regularization. Consider reducing variables or collecting more data.")
                )
            }

            # -- Check 2: Regularization Need --
            if (p >= n / 3) {
                checks$regularization <- list(
                    color = "green", label = .("Regularization Need"),
                    value = sprintf("p=%d, n=%d (ratio=%.2f)", p, n, p / n),
                    detail = .("High-dimensional setting. LASSO regularization is strongly indicated.")
                )
            } else if (p <= 5 && epv >= 30) {
                checks$regularization <- list(
                    color = "green", label = .("Regularization Need"),
                    value = sprintf("p=%d, EPV=%.0f", p, epv),
                    detail = .("Low-dimensional with ample events. LASSO is valid but standard Cox regression may suffice.")
                )
            } else if (p <= 10 && epv >= 20) {
                checks$regularization <- list(
                    color = "yellow", label = .("Regularization Need"),
                    value = sprintf("p=%d, EPV=%.0f", p, epv),
                    detail = .("Moderate dimensionality. Consider standard Cox regression (Multivariable Survival in ClinicoPath) as an alternative.")
                )
            } else {
                checks$regularization <- list(
                    color = "green", label = .("Regularization Need"),
                    value = sprintf("p=%d, n=%d", p, n),
                    detail = .("LASSO regularization is appropriate for this data dimension.")
                )
            }

            # -- Check 3: Sample Size --
            if (n >= 100) {
                checks$sample_size <- list(
                    color = "green", label = .("Sample Size"),
                    value = sprintf("n=%d", n),
                    detail = .("Adequate sample size for penalized regression.")
                )
            } else if (n >= 20) {
                checks$sample_size <- list(
                    color = "yellow", label = .("Sample Size"),
                    value = sprintf("n=%d", n),
                    detail = .("Small sample. Cross-validation folds may be unstable. Consider reducing the number of CV folds.")
                )
            } else {
                checks$sample_size <- list(
                    color = "red", label = .("Sample Size"),
                    value = sprintf("n=%d", n),
                    detail = .("Very small sample. Penalized regression results will be highly variable. Consider simpler analyses.")
                )
            }

            # -- Check 4: Event Rate --
            if (event_rate >= 0.20 && event_rate <= 0.80) {
                checks$event_rate <- list(
                    color = "green", label = .("Event Rate"),
                    value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
                    detail = .("Balanced event rate. Good for model estimation.")
                )
            } else if ((event_rate >= 0.10 && event_rate < 0.20) ||
                       (event_rate > 0.80 && event_rate <= 0.90)) {
                checks$event_rate <- list(
                    color = "yellow", label = .("Event Rate"),
                    value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
                    detail = .("Imbalanced event rate. Model calibration may be affected.")
                )
            } else {
                checks$event_rate <- list(
                    color = "red", label = .("Event Rate"),
                    value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
                    detail = .("Extreme event rate. Consider whether survival analysis is appropriate or collect longer follow-up.")
                )
            }

            # -- Check 5: Multicollinearity --
            tryCatch({
                # Identify which design-matrix columns came from the same original variable.
                # Sort by decreasing name length so that longer variable names (e.g. "age_group")
                # are matched before shorter prefixes (e.g. "age"), preventing mis-assignment.
                orig_vars <- data$original_variable_names
                col_names <- colnames(data$X)
                col_origin <- rep(NA_character_, length(col_names))
                sorted_vars <- orig_vars[order(nchar(orig_vars), decreasing = TRUE)]
                for (v in sorted_vars) {
                    v_safe <- make.names(v)
                    unassigned <- is.na(col_origin)
                    col_origin[unassigned & (startsWith(col_names, v) | startsWith(col_names, v_safe))] <- v
                }
                # Fallback for unmatched columns
                col_origin[is.na(col_origin)] <- col_names[is.na(col_origin)]

                if (ncol(data$X) >= 2) {
                    cor_matrix <- cor(data$X, use = "pairwise.complete.obs")
                    diag(cor_matrix) <- 0
                    # Zero out within-factor dummy correlations
                    for (i in seq_len(ncol(cor_matrix))) {
                        for (j in seq_len(ncol(cor_matrix))) {
                            if (!is.na(col_origin[i]) && !is.na(col_origin[j]) &&
                                col_origin[i] == col_origin[j]) {
                                cor_matrix[i, j] <- 0
                            }
                        }
                    }
                    max_cor <- max(abs(cor_matrix), na.rm = TRUE)

                    # Find top correlated pairs
                    top_pairs <- character(0)
                    if (max_cor > 0.5) {
                        cor_vals <- sort(abs(cor_matrix[upper.tri(cor_matrix)]), decreasing = TRUE)
                        idx <- which(abs(cor_matrix) >= cor_vals[min(3, length(cor_vals))] & upper.tri(cor_matrix), arr.ind = TRUE)
                        for (k in seq_len(min(3, nrow(idx)))) {
                            top_pairs <- c(top_pairs,
                                sprintf("%s & %s (r=%.2f)", col_names[idx[k, 1]], col_names[idx[k, 2]],
                                        cor_matrix[idx[k, 1], idx[k, 2]]))
                        }
                    }
                    pair_text <- if (length(top_pairs) > 0) paste0(" ", .("Top pairs:"), " ", paste(top_pairs, collapse = "; "), ".") else ""

                    if (max_cor < 0.7) {
                        checks$collinearity <- list(
                            color = "green", label = .("Multicollinearity"),
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = paste0(.("No concerning collinearity detected."), pair_text)
                        )
                    } else if (max_cor < 0.9) {
                        checks$collinearity <- list(
                            color = "yellow", label = .("Multicollinearity"),
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = paste0(.("Moderate collinearity. LASSO handles this by selecting one from correlated groups. Consider Elastic Net (Adaptive LASSO or NCVReg Cox in ClinicoPath) if you want to retain correlated predictors."), pair_text)
                        )
                    } else if (max_cor < 0.99) {
                        checks$collinearity <- list(
                            color = "yellow", label = .("Multicollinearity"),
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = paste0(.("High collinearity. Strongly recommend Elastic Net or Ridge regression."), pair_text)
                        )
                    } else {
                        checks$collinearity <- list(
                            color = "red", label = .("Multicollinearity"),
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = paste0(.("Near-perfect collinearity detected. Remove redundant variables before analysis."), pair_text)
                        )
                    }
                } else {
                    checks$collinearity <- list(
                        color = "green", label = .("Multicollinearity"),
                        value = .("N/A (single column)"),
                        detail = .("Only one predictor column; collinearity not applicable.")
                    )
                }
            }, error = function(e) {
                checks$collinearity <<- list(
                    color = "green", label = .("Multicollinearity"),
                    value = .("Could not compute"),
                    detail = .("Correlation matrix could not be computed (e.g., all categorical predictors). Proceeding without this check.")
                )
            })

            # -- Check 6: Data Quality --
            original_data <- self$data
            n_total <- nrow(original_data)
            n_missing <- n_total - n
            pct_missing <- 100 * n_missing / n_total

            # Check for constant predictors (already caught in cleanData, but summarize)
            constant_cols <- apply(data$X, 2, function(col) var(col, na.rm = TRUE) == 0)
            n_constant <- sum(constant_cols)

            if (n_missing == 0 && n_constant == 0) {
                checks$data_quality <- list(
                    color = "green", label = .("Data Quality"),
                    value = .("No issues"),
                    detail = .("Complete data with no constant predictors.")
                )
            } else if (pct_missing <= 5 && n_constant == 0) {
                checks$data_quality <- list(
                    color = "yellow", label = .("Data Quality"),
                    value = sprintf(.("%.1f%% missing (%d rows excluded)"), pct_missing, n_missing),
                    detail = .("Minor missingness. Complete-case analysis used. Results should be reliable if data is missing at random.")
                )
            } else {
                issues <- character(0)
                if (pct_missing > 5) issues <- c(issues, sprintf(.("%.1f%% missing data (%d rows excluded). Consider multiple imputation."), pct_missing, n_missing))
                if (n_constant > 0) issues <- c(issues, sprintf(.("%d constant predictor column(s) detected."), n_constant))
                checks$data_quality <- list(
                    color = if (pct_missing > 20) "red" else "yellow",
                    label = .("Data Quality"),
                    value = sprintf(.("%.1f%% missing, %d constant"), pct_missing, n_constant),
                    detail = paste(issues, collapse = " ")
                )
            }

            # -- Check 7: Proportional Hazards Assumption (advisory) --
            tryCatch({
                # Fit a simple Cox model with top predictors to check PH
                # Use at most 5 columns to keep computation fast
                n_check <- min(5, ncol(data$X))
                top_cols <- order(apply(data$X, 2, function(col) {
                    tryCatch(abs(cor(col, data$status, use = "complete.obs")), error = function(e) 0)
                }), decreasing = TRUE)[seq_len(n_check)]
                check_df <- as.data.frame(data$X[, top_cols, drop = FALSE])
                check_df$.time <- data$time
                check_df$.status <- data$status
                check_formula <- as.formula(paste0(
                    "survival::Surv(.time, .status) ~ ",
                    paste0("`", colnames(data$X)[top_cols], "`", collapse = " + ")
                ))
                check_cox <- survival::coxph(check_formula, data = check_df)
                ph_test <- survival::cox.zph(check_cox)
                global_p <- ph_test$table["GLOBAL", "p"]

                if (global_p >= 0.05) {
                    checks$ph_assumption <- list(
                        color = "green", label = .("Proportional Hazards"),
                        value = sprintf(.("Global test p = %.3f"), global_p),
                        detail = .("No evidence of PH violation in top predictors (Schoenfeld residuals test).")
                    )
                } else if (global_p >= 0.01) {
                    checks$ph_assumption <- list(
                        color = "yellow", label = .("Proportional Hazards"),
                        value = sprintf(.("Global test p = %.3f"), global_p),
                        detail = .("Possible PH violation. LASSO variable selection is still valid, but hazard ratios may vary over time. Consider time-dependent effects.")
                    )
                } else {
                    checks$ph_assumption <- list(
                        color = "yellow", label = .("Proportional Hazards"),
                        value = sprintf(.("Global test p = %.3f"), global_p),
                        detail = .("Evidence of PH violation. Variable selection is still meaningful, but interpret hazard ratios with caution. Consider stratified models or time-varying coefficients in downstream analysis.")
                    )
                }
            }, error = function(e) {
                checks$ph_assumption <<- list(
                    color = "green", label = .("Proportional Hazards"),
                    value = .("Could not test"),
                    detail = .("PH assumption test could not be performed (e.g., insufficient data or all categorical predictors). Proceeding without this check.")
                )
            })

            # -- Overall Verdict --
            colors <- sapply(checks, function(x) x$color)
            if (any(colors == "red")) {
                overall <- "red"
                overall_text <- .("Some issues require attention before relying on these results.")
            } else if (any(colors == "yellow")) {
                # Special case: if only yellow is regularization-not-needed and rest green
                yellow_checks <- names(checks)[colors == "yellow"]
                if (length(yellow_checks) == 1 && yellow_checks == "regularization") {
                    overall <- "green"
                    overall_text <- .("Data is suitable for LASSO Cox regression (though standard Cox may also be appropriate).")
                } else {
                    overall <- "yellow"
                    overall_text <- .("Data is usable but review the flagged items.")
                }
            } else {
                overall <- "green"
                overall_text <- .("Data is well-suited for LASSO Cox regression.")
            }

            private$.generateSuitabilityHtml(checks, overall, overall_text)
        },

        .generateSuitabilityHtml = function(checks, overall, overall_text) {
            # Color mapping
            bg_colors <- list(
                green  = "background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb;",
                yellow = "background-color: #fff3cd; color: #856404; border: 1px solid #ffeeba;",
                red    = "background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb;"
            )
            dot_colors <- list(green = "#28a745", yellow = "#ffc107", red = "#dc3545")

            # Overall banner
            html <- paste0(
                "<div style='", bg_colors[[overall]], " padding: 12px; border-radius: 6px; margin-bottom: 12px;'>",
                "<strong>", .("Overall:"), " ", overall_text, "</strong></div>"
            )

            # Check table
            html <- paste0(html,
                "<table style='width: 100%; border-collapse: collapse; font-size: 13px;'>",
                "<thead><tr style='border-bottom: 2px solid #dee2e6;'>",
                "<th style='padding: 6px; text-align: left;'>", .("Status"), "</th>",
                "<th style='padding: 6px; text-align: left;'>", .("Check"), "</th>",
                "<th style='padding: 6px; text-align: left;'>", .("Value"), "</th>",
                "<th style='padding: 6px; text-align: left;'>", .("Detail"), "</th>",
                "</tr></thead><tbody>"
            )

            for (chk in checks) {
                dot <- paste0("<span style='color: ", dot_colors[[chk$color]], "; font-size: 18px;'>&#9679;</span>")
                html <- paste0(html,
                    "<tr style='border-bottom: 1px solid #dee2e6;'>",
                    "<td style='padding: 6px;'>", dot, "</td>",
                    "<td style='padding: 6px;'><strong>", chk$label, "</strong></td>",
                    "<td style='padding: 6px;'>", chk$value, "</td>",
                    "<td style='padding: 6px;'>", chk$detail, "</td>",
                    "</tr>"
                )
            }

            html <- paste0(html, "</tbody></table>")

            # Recommendations (if any yellow/red)
            colors <- sapply(checks, function(x) x$color)
            if (any(colors %in% c("yellow", "red"))) {
                html <- paste0(html,
                    "<div style='margin-top: 12px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;'>",
                    "<strong>", .("Recommendations:"), "</strong><ul style='margin: 6px 0;'>"
                )
                if (!is.null(checks$epv) && checks$epv$color == "red") {
                    html <- paste0(html, "<li>", .("Consider Kaplan-Meier curves with log-rank test for very low EPV scenarios."), "</li>")
                }
                if (!is.null(checks$regularization) && checks$regularization$color == "yellow") {
                    html <- paste0(html, "<li>", .("Use <strong>Multivariable Survival</strong> in ClinicoPath for standard Cox regression with fewer predictors."), "</li>")
                }
                if (!is.null(checks$collinearity) && checks$collinearity$color %in% c("yellow", "red")) {
                    html <- paste0(html, "<li>", .("Use <strong>Adaptive LASSO Cox</strong> or <strong>NCVReg Cox</strong> in ClinicoPath with Elastic Net regularization to retain correlated predictors."), "</li>")
                }
                if (!is.null(checks$sample_size) && checks$sample_size$color == "red") {
                    html <- paste0(html, "<li>", .("With very small samples, consider univariable analyses or use <strong>Survival Analysis</strong> in ClinicoPath."), "</li>")
                }
                if (!is.null(checks$data_quality) && checks$data_quality$color %in% c("yellow", "red")) {
                    html <- paste0(html, "<li>", .("Consider multiple imputation before LASSO analysis for substantial missing data."), "</li>")
                }
                if (!is.null(checks$ph_assumption) && checks$ph_assumption$color == "yellow") {
                    html <- paste0(html, "<li>", .("Proportional hazards assumption may not hold. Consider time-dependent coefficients or stratified analysis in downstream modeling."), "</li>")
                }
                html <- paste0(html, "</ul></div>")
            }

            # Interpretation guidance (always shown)
            html <- paste0(html,
                "<div style='margin-top: 10px; font-size: 12px; color: #6c757d;'>",
                "<em>", .("This assessment is advisory. The analysis proceeds regardless of the verdict. Green = no concerns, Yellow = proceed with caution, Red = results may be unreliable."), "</em></div>"
            )

            self$results$suitabilityReport$setContent(html)
        },

        .populateModelComparison = function(results) {
            table <- self$results$modelComparison
            table$deleteRows()

            # LASSO model metrics
            lasso_cindex <- NA
            lasso_aic <- NA
            lasso_loglik <- NA
            tryCatch({
                # Refit Cox model with only selected variables for AIC/loglik
                if (length(results$selected_vars) > 0) {
                    selected_X <- as.data.frame(results$data$X[, results$selected_vars, drop = FALSE])
                    y <- survival::Surv(results$data$time, results$data$status)
                    lasso_cox <- survival::coxph(y ~ ., data = selected_X)
                    lasso_cindex_result <- survival::concordance(lasso_cox, reverse = TRUE)
                    lasso_cindex <- lasso_cindex_result$concordance
                    lasso_aic <- AIC(lasso_cox)
                    lasso_loglik <- as.numeric(logLik(lasso_cox))
                }
            }, error = function(e) {
                # silently use NA
            })

            table$addRow(rowKey = 1, values = list(
                model_type = .("Post-LASSO Standard Cox (Selected Variables)"),
                n_variables = length(results$selected_vars),
                cindex = lasso_cindex,
                aic = lasso_aic,
                log_likelihood = lasso_loglik
            ))

            # Standard Cox with all variables
            std_cindex <- NA
            std_aic <- NA
            std_loglik <- NA
            tryCatch({
                y <- survival::Surv(results$data$time, results$data$status)
                selected_data <- as.data.frame(results$data$X)
                std_cox <- survival::coxph(y ~ ., data = selected_data)
                std_cindex_result <- survival::concordance(std_cox, reverse = TRUE)
                std_cindex <- std_cindex_result$concordance
                std_aic <- AIC(std_cox)
                std_loglik <- as.numeric(logLik(std_cox))
            }, error = function(e) {
                # Standard Cox may fail with too many variables (p > n)
            })

            table$addRow(rowKey = 2, values = list(
                model_type = .("Standard Cox (all variables)"),
                n_variables = ncol(results$data$X),
                cindex = std_cindex,
                aic = std_aic,
                log_likelihood = std_loglik
            ))

            table$setNote(
                "comparison_note",
                .("C-index values in this table are from the corresponding unpenalized Cox models for comparability. Apparent penalized-model C-index is reported in Model Performance.")
            )
        },

        # Natural-language summary for copy-ready clinical reporting
        .populateSummary = function(results) {
            n_total <- ncol(results$data$X)
            n_selected <- length(results$selected_vars)
            n_obs <- results$data$n
            n_events <- results$data$n_events
            lambda_method <- self$options$lambda
            lambda_val <- results$lambda_optimal
            metrics <- results$performance_metrics

            # Build selected variable list
            if (n_selected > 0) {
                var_names <- results$data$variable_names[results$selected_vars]
                var_list <- paste(var_names, collapse = ", ")
            } else {
                var_list <- .("none")
            }

            # C-index text
            cindex_text <- if (!is.na(metrics$cindex)) {
                ci_lo <- metrics$cindex - 1.96 * metrics$cindex_se
                ci_hi <- metrics$cindex + 1.96 * metrics$cindex_se
                if (!is.na(metrics$cindex_se)) {
                    sprintf("%.3f (95%% CI: %.3f-%.3f)", metrics$cindex, max(0, ci_lo), min(1, ci_hi))
                } else {
                    sprintf("%.3f", metrics$cindex)
                }
            } else {
                .("not available")
            }

            # HR text
            hr_text <- ""
            if (!is.null(metrics$hazard_ratio) && !is.na(metrics$hazard_ratio)) {
                if (!is.na(metrics$hr_ci_lower) && !is.na(metrics$hr_ci_upper)) {
                    hr_text <- sprintf(
                        .("The hazard ratio for high- versus low-risk group was %.2f (95%% CI: %.2f-%.2f, log-rank p %s)."),
                        metrics$hazard_ratio, metrics$hr_ci_lower, metrics$hr_ci_upper,
                        if (!is.null(metrics$logrank_p) && !is.na(metrics$logrank_p)) {
                            if (metrics$logrank_p < 0.001) "< 0.001" else sprintf("= %.3f", metrics$logrank_p)
                        } else { .("not available") }
                    )
                }
            }

            # Standardization note
            scale_note <- if (self$options$standardize) {
                .("Coefficients are on the standardized scale (per 1-SD change).")
            } else {
                .("Coefficients are on the original variable scale.")
            }

            summary_text <- paste0(
                "<div style='background-color: #f0f7ff; border: 1px solid #b8d4f0; border-radius: 6px; padding: 14px; margin-bottom: 12px;'>",
                "<h4 style='margin-top: 0;'>", .("Results Summary"), "</h4>",
                "<p>", jmvcore::format(
                    .("LASSO Cox regression was performed on {n_obs} observations ({n_events} events) with {n_total} candidate predictors using {lambda_method} for lambda selection."),
                    list(n_obs = n_obs, n_events = n_events, n_total = n_total, lambda_method = lambda_method)
                ), " ",
                jmvcore::format(
                    .("The model selected {n_selected} of {n_total} variables: {var_list}."),
                    list(n_selected = n_selected, n_total = n_total, var_list = var_list)
                ), "</p>",
                "<p>", jmvcore::format(
                    .("The apparent C-index was {cindex}, indicating {interp}."),
                    list(cindex = cindex_text, interp = tolower(private$.interpretCindex(metrics$cindex)))
                ), " ",
                if (nzchar(hr_text)) hr_text else "", "</p>",
                "<p><em>", scale_note, " ",
                .("All performance metrics are apparent (training) values; external validation is recommended before clinical use."), "</em></p>",
                "</div>"
            )

            self$results$summaryText$setContent(summary_text)
        }
    )
)
