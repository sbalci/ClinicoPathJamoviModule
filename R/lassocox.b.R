#' @title Lasso-Cox Regression for Variable Selection in Survival Analysis
#' @description
#' Performs Lasso-penalized Cox proportional hazards regression for variable selection
#' in survival analysis. This function uses penalized likelihood to shrink coefficients and
#' produce a sparse candidate model. Regularization can reduce, but does not eliminate,
#' overfitting; development results require internal and external validation before clinical use.
#' 
#' @details
#' The Lasso-Cox regression combines the Cox proportional hazards model with L1 regularization
#' (Lasso penalty) to estimate a sparse coefficient vector. The method minimizes the partial
#' likelihood penalized by the L1 norm of the coefficient vector, shrinking coefficients
#' toward zero and setting some exactly to zero. A zero coefficient is a model-selection
#' result in these data and does not establish that a predictor is clinically irrelevant.
#' 
#' Key features:
#' - Automatic variable selection through L1 regularization
#' - Cross-validation for optimal tuning parameter selection
#' - Risk score calculation and stratification
#' - Apparent development-sample discrimination
#' - Exploratory survival-curve visualization by a development-sample median split
#' 
#' The function uses the glmnet package for efficient computation and supports both
#' lambda.min (minimum cross-validation error) and lambda.1se (1 standard error rule)
#' for tuning parameter selection.
#' 
#' @examples
#' \dontrun{
#' # Both outcomeLevel and censorLevel are `type: Level` options. jamovi forbids a
#' # default on a Level, so both are REQUIRED arguments of this function even though
#' # the GUI fills them in for you - pass them explicitly when calling from R.
#' # Basic Lasso-Cox regression
#' result <- lassocox(
#'   data = survival_data,
#'   elapsedtime = "time",
#'   outcome = "status", 
#'   outcomeLevel = "1",
#'   censorLevel = "0",
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
#'   censorLevel = "alive",
#'   explanatory = gene_variables,
#'   lambda = "lambda.min",
#'   nfolds = 5,
#'   standardize = TRUE
#' )
#' }
#' 
#' @importFrom R6 R6Class
#' @import jmvcore
#' @return An \code{R6} class generator object for the \code{lassocoxClass} backend; used internally by the jamovi analysis wrapper and not called directly.

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
            
            # NOTE on jmvcore::format placeholders - two constraints, both easy to
            # violate and neither of which errors:
            #   1. Pass substitutions as named dots, NOT wrapped in list(). format()
            #      reads `...` and checks `name %in% names(args)`; a single unnamed
            #      list matches nothing, so every token falls through to the "..."
            #      default. jmvcore::reject is worse - its second positional argument
            #      is `code`, so a list lands there and is never seen as a value.
            #   2. Placeholder names must be camelCase. The token regex is
            #      `\\{ *[A-Za-z][A-Za-z0-9]* *\\}` (jmvcore 2.7.38) - it does NOT
            #      accept an underscore, so `{n_obs}` stays literal even when the
            #      matching named dot is supplied correctly.
            # Verified 2026-08-20:
            #   jmvcore::format("n = {n_obs}", list(n_obs = 242))  -> "n = {n_obs}"
            #   jmvcore::format("n = {n_obs}", n_obs = 242)        -> "n = {n_obs}"
            #   jmvcore::format("n = {nObs}",  nObs  = 242)        -> "n = 242"
            # Both were wrong throughout this file (24 list() sites, 9 underscored
            # tokens) and are now fixed. Keep new strings camelCase.
            if (length(missing_packages) > 0) {
                pkg_list <- paste(missing_packages, collapse = ", ")
                install_cmd <- paste0("install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))")
                error_msg <- jmvcore::format(
                    .("The following required packages are not installed: {pkgs}\n\nPlease install them using:\n{cmd}"),
                    pkgs = pkg_list, cmd = install_cmd)

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
                "<li>", .("Apparent development-sample discrimination (C-index)"), "</li>",
                "<li>", .("Risk score calculation for model development"), "</li>",
                "<li>", .("Exploratory survival curves from a development-sample median split"), "</li>",
                "<li>", .("Comprehensive visualizations"), "</li>",
                "</ul>",
                "<h5>", .("Key Features:"), "</h5>",
                "<ul>",
                "<li>", .("Handles high-dimensional data (p >= n scenarios)"), "</li>",
                "<li>", .("Sparse coefficient estimation"), "</li>",
                "<li>", .("Can reduce overfitting through regularization"), "</li>",
                "<li>", .("Supports model development; it does not establish clinical utility"), "</li>",
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

                    # Fit Lasso-Cox model
                    results <- private$.fitModel(data)
                    if (is.null(results)) return()

                    # Suitability includes diagnostics for the selected model, so it
                    # must run after fitting rather than screening unrelated variables.
                    if (self$options$suitabilityCheck) {
                        private$.assessSuitability(results)
                    }

                    # Populate result tables
                    private$.populateModelSummary(results)
                    private$.populateCoefficients(results)
                    private$.populatePerformance(results)

                    # Save plot data for rendering
                    private$.savePlotData(results)

                    # Display collected warnings in results panel
                    if (length(collected_warnings) > 0) {
                        warn_items <- paste0("<li>", htmltools::htmlEscape(collected_warnings), "</li>",
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
                        "<p><strong>", .("Error:"), "</strong> ", htmltools::htmlEscape(e$message), "</p>",
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
                jmvcore::reject(.("At least 2 explanatory variables are required for Lasso regression."))
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
                if (length(observed_levels) != 2) {
                    jmvcore::reject(jmvcore::format(.('Outcome variable must have exactly 2 observed values. Found {n} level(s): {levels}. For competing events, construct an explicit binary cause-specific status in which the event of interest is 1 and every non-event observation is 0; do not omit other event types.'),
                        n = length(observed_levels), levels = paste(observed_levels, collapse = ", ")))
                }

                # Resolve event level
                outcome_level_opt <- self$options$outcomeLevel
                if (is.null(outcome_level_opt) || !nzchar(as.character(outcome_level_opt))) {
                    event_level_used <- observed_levels[2]
                } else {
                    event_level_used <- as.character(outcome_level_opt)
                    if (!(event_level_used %in% observed_levels)) {
                        jmvcore::reject(jmvcore::format(.("Selected event level ('{level}') is not present in observed outcome data."),
                            level = event_level_used))
                    }
                }

                # Resolve censor level
                censor_level_opt <- self$options$censorLevel
                if (is.null(censor_level_opt) || !nzchar(as.character(censor_level_opt))) {
                    # Default: first observed level that is not the event level
                    remaining <- setdiff(observed_levels, event_level_used)
                    if (length(remaining) == 0) {
                        jmvcore::reject(.("Cannot determine censored level: all observed levels equal the event level."))
                    }
                    censor_level_used <- remaining[1]
                } else {
                    censor_level_used <- as.character(censor_level_opt)
                    if (!(censor_level_used %in% observed_levels)) {
                        jmvcore::reject(jmvcore::format(.("Selected censored level ('{level}') is not present in observed outcome data."),
                            level = censor_level_used))
                    }
                }

                if (event_level_used == censor_level_used) {
                    jmvcore::reject(.("Event level and censored level must be different."))
                }

                # Strict two-level encoding: only recognized levels get 0/1.
                status <- rep(NA_real_, length(outcome_chr))
                status[outcome_chr == event_level_used] <- 1
                status[outcome_chr == censor_level_used] <- 0
                # Rows with missing outcome remain missing and are handled below.
            } else {
                outcome_num <- jmvcore::toNumeric(outcome_raw)
                observed_levels <- sort(unique(outcome_num[!is.na(outcome_num)]))
                if (length(observed_levels) != 2) {
                    jmvcore::reject(jmvcore::format(.('Numeric outcome must have exactly 2 observed values. Found {n} value(s): {values}. For competing events, construct an explicit binary cause-specific status in which the event of interest is 1 and every non-event observation is 0; do not omit other event types.'),
                        n = length(observed_levels), values = paste(observed_levels, collapse = ", ")))
                }

                # Resolve event level
                outcome_level_opt <- self$options$outcomeLevel
                if (!is.null(outcome_level_opt) && nzchar(as.character(outcome_level_opt))) {
                    event_level_num <- suppressWarnings(as.numeric(outcome_level_opt))
                    if (is.na(event_level_num) || !(event_level_num %in% observed_levels)) {
                        jmvcore::reject(.("For numeric outcomes, Event Level must be one of the observed outcome values."))
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
                        jmvcore::reject(.("For numeric outcomes, Censored Level must be one of the observed outcome values."))
                    }
                } else if (all(observed_levels %in% c(0, 1))) {
                    censor_level_num <- 0
                } else {
                    censor_level_num <- min(observed_levels)
                }

                if (event_level_num == censor_level_num) {
                    jmvcore::reject(.("Event level and censored level must be different."))
                }

                event_level_used <- as.character(event_level_num)
                censor_level_used <- as.character(censor_level_num)

                # Strict two-level encoding
                status <- rep(NA_real_, length(outcome_num))
                status[outcome_num == event_level_num] <- 1
                status[outcome_num == censor_level_num] <- 0
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
                    vars = paste(constant_var_names, collapse = ", ")))
            }
            if (ncol(predictors) == 0) {
                jmvcore::reject(.("No valid explanatory variables remain after removing constant predictors."))
            }
            if (ncol(predictors) == 1) {
                warning(.("Only one non-constant explanatory variable remains; LASSO selection is limited."))
            }

            # Complete-case filtering across all analysis inputs
            complete <- complete.cases(time, status, predictors)
            n_complete <- sum(complete)
            n_excluded <- length(complete) - n_complete

            if (n_complete < 10) {
                jmvcore::reject(jmvcore::format(.('Too few complete cases for analysis ({n}). Need at least 10 complete observations.'),
                    n = n_complete))
            }
            if (n_excluded > 0) {
                warning(jmvcore::format(.('Excluded {n} row(s) with missing values in time/outcome/predictors (complete-case analysis).'),
                    n = n_excluded))
            }

            time_cc <- time[complete]
            status_cc <- status[complete]

            if (any(!is.finite(time_cc))) {
                jmvcore::reject(.("Time variable contains non-finite values after filtering. Please correct the input."))
            }
            if (any(time_cc < 0, na.rm = TRUE)) {
                jmvcore::reject(.("Time variable contains negative values. Survival times must be non-negative."))
            }
            if (any(time_cc == 0, na.rm = TRUE)) {
                warning(.("Time variable contains zero values. Consider adding a small constant if convergence issues occur."))
            }

            # Outcome must remain binary after complete-case filtering
            if (!(length(unique(status_cc)) == 2 && all(unique(status_cc) %in% c(0, 1)))) {
                jmvcore::reject(.("Outcome is not binary after complete-case filtering. Check event level and missing-data pattern."))
            }

            n_events <- sum(status_cc == 1)
            n_censored <- sum(status_cc == 0)
            if (n_events < 3 || n_censored < 3) {
                jmvcore::reject(jmvcore::format(
                    .('Stratified cross-validation requires at least 3 events and 3 censored observations. Found {nEvents} events and {nCensored} censored observations. This is a computational minimum, not evidence that the sample is adequate for prediction modeling.'),
                    nEvents = n_events, nCensored = n_censored))
            }

            if (n_events < 10 || n_censored < 10) {
                warning(jmvcore::format(
                    .('Only {nEvents} events and {nCensored} censored observations are available. Cross-validation and selected coefficients may be highly unstable; no fixed event count guarantees reliable prediction modeling.'),
                    nEvents = n_events, nCensored = n_censored))
            }

            # Create design matrix with robust factor handling
            factor_predictors <- character(0)
            tryCatch({
                factor_vars <- sapply(predictors[complete, , drop = FALSE], is.factor)
                if (any(factor_vars)) {
                    factor_predictors <- names(factor_vars)[factor_vars]
                    for (var_name in names(factor_vars)[factor_vars]) {
                        factor_levels <- length(unique(predictors[complete, var_name]))
                        if (factor_levels < 2) {
                            jmvcore::reject(jmvcore::format(.("Factor variable '{var}' has insufficient variation in complete cases."),
                                var = var_name))
                        }
                    }
                }

                X <- .stripBackticks(model.matrix(~ ., data = predictors[complete, , drop = FALSE])[, -1, drop = FALSE])
                if (ncol(X) == 0) {
                    jmvcore::reject(.("No valid predictors remaining after model-matrix encoding."))
                }

                valid_cols <- apply(X, 2, function(col) {
                    !all(is.na(col)) && is.finite(var(col, na.rm = TRUE)) && var(col, na.rm = TRUE) > 0
                })
                if (!any(valid_cols)) {
                    jmvcore::reject(.("No valid predictors after removing degenerate design-matrix columns."))
                }
                X <- X[, valid_cols, drop = FALSE]
            }, error = function(e) {
                jmvcore::reject(jmvcore::format(.('Error creating design matrix: {msg}. Check factor coding and missing values.'),
                    msg = e$message))
            })

            if (length(factor_predictors) > 0) {
                warning(jmvcore::format(
                    .('Categorical predictors are represented by indicator columns and LASSO selects those columns individually rather than selecting each factor as a group. Factor(s): {vars}.'),
                    vars = paste(factor_predictors, collapse = ", ")))
            }

            if (n_events < ncol(X)) {
                warning(jmvcore::format(
                    .('There are {nEvents} events for {nColumns} encoded predictor columns. Regularization permits fitting in this setting but does not guarantee stable selection or performance; use resampling that repeats the entire modeling process.'),
                    nEvents = n_events, nColumns = ncol(X)))
            }

            # Keep the design matrix on its original scale. glmnet receives the
            # standardize option below, so each cross-validation training fit performs
            # its own internal scaling rather than using means/SDs calculated from the
            # held-out rows. glmnet back-transforms returned coefficients to the original
            # predictor scale; the values retained here are descriptive only.
            if (self$options$standardize) {
                scale_vals <- apply(X, 2, stats::sd)
                scale_vals[is.na(scale_vals) | scale_vals == 0] <- 1
                scaling_info <- list(
                    center = colMeans(X),
                    scale = scale_vals
                )
            } else {
                scaling_info <- NULL
            }

            if (any(is.infinite(X))) {
                jmvcore::reject(.("Design matrix contains infinite values. Check for extreme outliers."))
            }

            return(list(
                time = time_cc,
                status = status_cc,
                X = X,
                n = n_complete,
                n_events = n_events,
                n_censored = n_censored,
                variable_names = colnames(X),
                original_variable_names = explanatory_vars,
                factor_predictors = factor_predictors,
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
                jmvcore::reject(.("Required packages 'glmnet' and 'survival' not available"))
            }
            
            # Create survival object
            y <- survival::Surv(data$time, data$status)
            
            # Validate survival object
            if (any(is.na(y))) {
                jmvcore::reject(.("Invalid survival object. Check time and status variables."))
            }

            # Set up cross-validation parameters
            nfolds_requested <- as.integer(self$options$nfolds)
            nfolds <- min(nfolds_requested, data$n_events, data$n_censored)
            if (nfolds < 3) {
                jmvcore::reject(jmvcore::format(
                    .('Cannot form stratified cross-validation folds: at least 3 folds require at least 3 events and 3 censored observations. Found {nEvents} events and {nCensored} censored observations.'),
                    nEvents = data$n_events, nCensored = data$n_censored))
            }
            if (nfolds != nfolds_requested) {
                warning(jmvcore::format(
                    .('Reduced the number of CV folds from {requested} to {used} so every fold contains event and censored observations.'),
                    requested = nfolds_requested, used = nfolds))
            }

            seed_value <- tryCatch(as.integer(self$options$random_seed), error = function(e) NA_integer_)
            if (is.na(seed_value)) {
                seed_value <- 123456
            }

            foldid <- private$.makeStratifiedFoldId(data$status, nfolds, seed_value)
            if (is.null(foldid)) {
                jmvcore::reject(.("Could not create event/censor-stratified cross-validation folds. Reduce the requested folds or provide more outcome observations."))
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
                    family = "cox", cox.ties = "breslow",
                    alpha = 1,  # Lasso (L1) penalty
                    standardize = isTRUE(self$options$standardize),
                    parallel = FALSE  # Avoid parallel processing issues
                )
                cv_args$foldid <- foldid
                private$.checkpoint()
                cv_fit <- .quietly(do.call(glmnet::cv.glmnet, cv_args))
                
                # Check if cross-validation succeeded
                if (is.null(cv_fit$lambda.min) || is.na(cv_fit$lambda.min)) {
                    jmvcore::reject(.("Cross-validation failed. Check data quality and sample size."))
                }
                
            }, error = function(e) {
                jmvcore::reject(jmvcore::format(.('Error in cross-validation: {msg}'), msg = e$message))
            })
            
            # Respect the selected rule exactly. In particular, an empty model at
            # lambda.1se is a valid cross-validation result and must not be replaced
            # data-dependently by lambda.min merely to force predictors into the table.
            lambda_optimal <- switch(self$options$lambda,
                "lambda.min" = cv_fit$lambda.min,
                "lambda.1se" = cv_fit$lambda.1se,
                cv_fit$lambda.1se  # Default fallback
            )
            lambda_rule_used <- self$options$lambda
            if (length(lambda_optimal) != 1 || !is.finite(lambda_optimal) || lambda_optimal <= 0) {
                jmvcore::reject(.("The selected cross-validation rule did not produce a valid positive lambda."))
            }
            
            # Fit final model with optimal lambda
            tryCatch({
                final_model <- .quietly(glmnet::glmnet(
                    x = data$X,
                    y = y,
                    family = "cox", cox.ties = "breslow",
                    alpha = 1,
                    lambda = lambda_optimal,
                    standardize = isTRUE(self$options$standardize)
                ))
                
            }, error = function(e) {
                jmvcore::reject(jmvcore::format(.('Error fitting final model: {msg}'), msg = e$message))
            })
            
            # Extract coefficients and selected variables. Use glmnet's fitted degrees
            # of freedom to distinguish a genuinely empty fit from very small coefficients
            # on large-unit predictors. A fixed cutoff on the original coefficient scale
            # is not scale invariant and can erase a clinically meaningful linear predictor.
            coef_matrix <- as.matrix(coef(final_model, s = lambda_optimal))
            fitted_df <- as.integer(final_model$df[length(final_model$df)])
            if (length(fitted_df) != 1 || is.na(fitted_df) || fitted_df < 0) {
                fitted_df <- sum(coef_matrix[, 1] != 0)
            }
            if (fitted_df == 0) {
                coef_matrix[,] <- 0
                selected_vars <- integer(0)
            } else {
                selected_vars <- which(coef_matrix[, 1] != 0)
            }

            if (length(selected_vars) == 0) {
                warning(jmvcore::format(
                    .('The {rule} rule retained no predictor columns. The valid empty model has been preserved; choose a different lambda rule only as a prespecified modeling decision, not because the empty result is inconvenient.'),
                    rule = lambda_rule_used))
            }
            
            # Calculate scores from the same coefficient vector that is reported in the
            # table. For df = 0 this explicitly removes machine-epsilon residue so an empty
            # model cannot imply discrimination. A Cox glmnet model has no intercept, so
            # X %*% beta is the link.
            risk_scores <- if (length(selected_vars) == 0) {
                rep(0, nrow(data$X))
            } else {
                as.numeric(data$X %*% coef_matrix[, 1])
            }
            
            # Calculate comprehensive performance metrics
            performance_metrics <- private$.calculatePerformanceMetrics(y, risk_scores, data)
            
            # Scale-adjusted coefficient magnitude. Raw glmnet coefficients are on
            # original predictor units, so |beta| alone is not comparable across columns
            # measured in different units. |beta| * SD(X) is the absolute change in the
            # linear predictor for a one-SD change in that encoded design column.
            if (length(selected_vars) > 0) {
                column_sd <- apply(data$X, 2, stats::sd)
                column_sd[!is.finite(column_sd) | column_sd <= 0] <- NA_real_
                var_importance <- abs(
                    coef_matrix[selected_vars, 1] * column_sd[selected_vars]
                )
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
                lambda_rule_used = lambda_rule_used,
                nfolds_used = nfolds,
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

            # NAMESPACE has a blanket import(jmvcore), and jmvcore exports its own
            # format() -- a {}-placeholder string templater, not base::format. A bare
            # format(x, scientific = TRUE, digits = 3) therefore resolved to jmvcore's,
            # which stringifies x and silently ignores both arguments. Keep base:: here.
            table$addRow(rowKey = 4, values = list(
                statistic = .("Optimal Lambda"),
                value = base::format(results$lambda_optimal, scientific = TRUE, digits = 3)
            ))

            table$addRow(rowKey = 5, values = list(
                statistic = .("Penalty Selected By"),
                value = results$lambda_rule_used
            ))

            table$addRow(rowKey = 6, values = list(
                statistic = .("Stratified CV Folds Used"),
                value = results$nfolds_used
            ))

            table$addRow(rowKey = 7, values = list(
                statistic = .("Sample Size"),
                value = results$data$n
            ))

            table$addRow(rowKey = 8, values = list(
                statistic = .("Number of Events"),
                value = results$data$n_events
            ))

            table$addRow(rowKey = 9, values = list(
                statistic = .("Censoring Rate"),
                value = paste0(round(100 * results$data$n_censored / results$data$n, 1), "%")
            ))

            table$addRow(rowKey = 10, values = list(
                statistic = .("Event Level Used"),
                value = results$data$event_level_used
            ))

            if (!is.null(results$data$excluded_rows) && results$data$excluded_rows > 0) {
                table$addRow(rowKey = 11, values = list(
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
                    importance = NA
                ))
                table$setNote("empty",
                    .("The selected cross-validation rule produced a valid empty model. No less-penalized model was substituted."))
                return()
            }

            # Add coefficient rows for selected variables
            for (i in seq_along(results$selected_vars)) {
                var_idx <- results$selected_vars[i]
                var_name <- results$data$variable_names[var_idx]
                coef_val <- results$coef_matrix[var_idx, 1]
                hr_val <- exp(coef_val)
                importance <- results$var_importance[var_name]

                table$addRow(rowKey = i, values = list(
                    variable = var_name,
                    coefficient = coef_val,
                    hazardRatio = hr_val,
                    importance = round(importance, 4)
                ))
            }

            table$setNote("penalized",
                .("Coefficient and Hazard Ratio are from the penalized LASSO Cox fit at the selected lambda. Conventional p-values and confidence intervals are intentionally not reported because same-data post-selection inference would not account for variable and penalty selection."))
            table$setNote("importance",
                .("Scale-adjusted magnitude is |coefficient| multiplied by the complete-case SD of that encoded design column. It describes a one-SD change in the fitted linear predictor, not causal importance or selection stability; for a factor indicator it is not the category-versus-reference hazard ratio."))
            if (!is.null(results$data$scaling_info)) {
                table$setNote("scale",
                    .("Predictors were standardized internally within glmnet fitting for the penalty calculation. glmnet back-transforms the displayed coefficients to the original design-column scale; indicator-column hazard ratios therefore compare that category with its reference category. Indicators are still penalized and selected separately rather than as a grouped factor."))
            } else if (length(results$data$factor_predictors) > 0) {
                table$setNote("factor",
                    .("Categorical predictors are expanded into indicator columns and those columns are selected separately; this is not grouped selection of the whole factor."))
            }
        },

        .populatePerformance = function(results) {
            table <- self$results$performance
            metrics <- results$performance_metrics
            
            # Clear existing rows
            table$deleteRows()
            
            # C-index
            if (!is.na(metrics$cindex)) {
                cindex_text <- round(metrics$cindex, 3)
                
                table$addRow(rowKey = 1, values = list(
                    metric = .("Apparent C-index"),
                    value = cindex_text,
                    interpretation = .("Development data only; optimism not corrected")
                ))
            }

            table$setNote("apparent",
                .("This C-index is apparent (training) performance from the same patients used for preprocessing, penalty selection, and model fitting. Its uncertainty does not include the modeling process. Use bootstrap optimism correction or nested cross-validation that repeats all preprocessing and tuning, followed by external validation before clinical use."))
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
                # Both vectors must be NAME-matched. `values` already was, but `labels`
                # was a bare positional vector, so when every selected coefficient shared a
                # sign only one level existed and the first label landed on it: an
                # all-positive (risk-increasing) model drew red bars labelled "Protective".
                # Mixed signs happened to render correctly, which is why it survived.
                ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                                          labels = c("FALSE" = .("Protective"),
                                                     "TRUE"  = .("Risk Factor")),
                                          breaks = c("FALSE", "TRUE"),
                                          limits = c("FALSE", "TRUE"),
                                          drop = FALSE,
                                          name = .("Effect")) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = .("Selected Variables and Coefficients"),
                    subtitle = paste(
                        .("Penalized LASSO coefficients at the selected lambda."),
                        if (!is.null(self$options$standardize) && self$options$standardize)
                            .("Internal standardization was used for fitting; displayed coefficients are back-transformed to the original design-column scale.") else ""
                    ),
                    x = .("Variables"),
                    y = .("Penalized coefficient")
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
                text_warning <- .("No variable-based risk scores are available because the selected rule retained no predictors.\n\nThe empty model is a valid result. Choose another lambda rule only as a prespecified modeling decision and report that choice transparently.")
                
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
                text_warning <- .("Risk scores are uniform, so no risk-group curve can be formed. This is expected for an empty model or when all fitted linear predictors are identical.")
                
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
            
            # Development-sample median split for descriptive visualization only
            tryCatch({
                risk_groups <- private$.makeBinaryRiskGroups(state$risk_scores)
                if (is.null(risk_groups)) {
                    jmvcore::reject(.("Unable to create two risk groups from risk scores."))
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
                    p <- .quietly(survminer::ggsurvplot(
                        fit,
                        data = plot_data,
                        risk.table = TRUE,
                        risk.table.y.text = FALSE,
                        tables.theme = survminer::theme_cleantable(),  # jamovi's 16-pt ggtheme otherwise swallows the 25% table strip
                        pval = FALSE,
                        conf.int = TRUE,
                        ggtheme = ggtheme,
                        title = .("Exploratory Development-Sample Risk Groups"),
                        xlab = .("Time"),
                        ylab = .("Survival Probability"),
                        legend.title = .("Risk Group"),
                        legend.labs = c(.("Low Risk"), .("High Risk")),
                        palette = c("#2166AC", "#B2182B")
                    ))
                    .quietly(print(p))
                } else {
                    # Fallback to base plot
                    plot(fit, col = c("blue", "red"), lwd = 2,
                         xlab = .("Time"), ylab = .("Survival Probability"),
                         main = .("Exploratory Development-Sample Risk Groups"))
                    legend("topright", legend = c(.("Low Risk"), .("High Risk")),
                           col = c("blue", "red"), lwd = 2)
                }

            }, error = function(e) {
                # Handle any errors gracefully using grid graphics
                text_warning <- jmvcore::format(.("Error creating survival plot:\n{msg}\n\nPlease check your data and model parameters."), msg = e$message)
                
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
                # Align original-unit coefficients with the scale-adjusted-magnitude
                # ordering used by the companion importance column.
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
                <p>LASSO (Least Absolute Shrinkage and Selection Operator) Cox regression combines the Cox proportional hazards model with an L1 penalty to estimate a sparse candidate prediction model.</p>
            </div>
            
            <h4> Key Concepts:</h4>
            <ul>
                <li><strong>Regularization (\u{03BB}):</strong> A penalty parameter that controls the strength of variable selection
                    <ul>
                        <li>Higher \u{03BB} \u{2192} More variables excluded (simpler model)</li>
                        <li>Lower \u{03BB} \u{2192} More variables included (complex model)</li>
                        <li>\u{03BB} = 0 \u{2192} Standard Cox regression (no penalty)</li>
                    </ul>
                </li>
                
                <li><strong>Sparsity:</strong> LASSO sets some fitted coefficients to zero; this does not prove that those predictors are clinically irrelevant</li>
                
                <li><strong>Cross-Validation:</strong> Event/censor-stratified K-fold CV estimates partial-likelihood deviance and applies the selected lambda rule</li>
                
                <li><strong>Shrinkage:</strong> Coefficients of selected variables are shrunk toward zero, reducing overfitting</li>
            </ul>
            
            <h4> How to Interpret Results:</h4>
            <ul>
                <li><strong>Selected Variables:</strong> Variables with non-zero coefficients at optimal \u{03BB}</li>
                <li><strong>Coefficients:</strong> Log hazard ratios (positive = increased risk, negative = decreased risk)</li>
                <li><strong>Risk Scores:</strong> Linear combination of selected variables weighted by their coefficients</li>
                <li><strong>C-index:</strong> Discrimination ability (0.5 = no discrimination, 1.0 = perfect discrimination)</li>
            </ul>
            
            <div class='alert alert-success'>
                <h4> Advantages of LASSO Cox:</h4>
                <ul>
                    <li>Produces sparse candidate models without stepwise significance testing</li>
                    <li>Can reduce overfitting, but does not prevent it</li>
                    <li>Can select one among correlated predictors; the choice may be unstable</li>
                    <li>Provides a reproducible fitting procedure when preprocessing and folds are fixed</li>
                </ul>
            </div>
            
            <div class='alert alert-warning'>
                <h4> Important Considerations:</h4>
                <ul>
                    <li>Variable selection depends on the specific dataset and may vary with new data</li>
                    <li>Standardization is typically required for fair penalization across variables</li>
                    <li>Categorical predictors are expanded into indicator columns and selected column by column, not as whole factors</li>
                    <li>All preprocessing and tuning must be repeated inside bootstrap or nested cross-validation; external validation is also required before clinical use</li>
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
                <p><strong>Objective Function:</strong> -\u{2113}(\u{03B2}) + \u{03BB} \u{03A3}|\u{03B2}\u{2C7C}|</p>
                <ul>
                    <li>\u{2113}(\u{03B2}): Partial log-likelihood from Cox model</li>
                    <li>\u{03BB}: Regularization parameter</li>
                    <li>\u{03A3}|\u{03B2}\u{2C7C}|: L1 penalty (sum of absolute coefficients)</li>
                </ul>
            </div>
            
            <h4> Algorithm Steps:</h4>
            <ol>
                <li><strong>Data Preprocessing:</strong>
                    <ul>
                        <li>This analysis uses complete cases. If imputation is used in a separate workflow, estimate it inside each resampling training split to avoid information leakage</li>
                        <li>Standardize continuous variables (mean=0, SD=1)</li>
                        <li>Create dummy variables for categorical predictors</li>
                    </ul>
                </li>
                
                <li><strong>Cross-Validation:</strong>
                    <ul>
                        <li>Divide data into K folds (typically K=10)</li>
                        <li>For each \u{03BB} value, train on K-1 folds and validate on remaining fold</li>
                        <li>Calculate cross-validated partial likelihood deviance</li>
                        <li>Select \u{03BB} that minimizes CV error</li>
                    </ul>
                </li>
                
                <li><strong>Final Model:</strong>
                    <ul>
                        <li>Fit LASSO Cox model on full data using optimal \u{03BB}</li>
                        <li>Extract non-zero coefficients (selected variables)</li>
                        <li>Calculate risk scores: \u{03A3}(\u{03B2}\u{2C7C} \u{00D7} x\u{2C7C})</li>
                    </ul>
                </li>
                
                <li><strong>Performance Assessment:</strong>
                    <ul>
                        <li>Calculate C-index (concordance probability)</li>
                        <li>Report the apparent development-sample C-index as descriptive only</li>
                        <li>Generate exploratory curves using a development-sample median split without inferential testing</li>
                    </ul>
                </li>
            </ol>
            
            <h4> Hyperparameter Selection:</h4>
            <ul>
                <li><strong>\u{03BB}.min:</strong> Lambda that minimizes CV error</li>
                <li><strong>\u{03BB}.1se:</strong> Largest lambda within 1 SE of minimum (more parsimonious)</li>
                <li><strong>Choice:</strong> \u{03BB}.1se is more regularized, but neither rule guarantees generalizability; choose and report the rule transparently</li>
            </ul>
            
            <h4> Variable Importance Metrics:</h4>
            <ul>
                <li><strong>Scale-adjusted magnitude:</strong> |\u{03B2}| \u{00D7} SD(X) is the absolute fitted linear-predictor change for a one-SD change in an encoded design column</li>
                <li><strong>Interpretation:</strong> This descriptive magnitude is not causal importance or selection stability; for a factor indicator it is not the category-versus-reference hazard ratio</li>
                <li><strong>Path Inclusion Proportion:</strong> Fraction of lambda values in the regularization path where the variable has a non-zero coefficient</li>
                <li><strong>Limitation:</strong> Path inclusion is not a bootstrap selection frequency and does not measure selection stability</li>
            </ul>
            
            <div class='alert alert-info'>
                <h5> Implementation Notes</h5>
                <ul>
                    <li>Uses coordinate descent algorithm for optimization</li>
                    <li>Handles ties in survival times using Breslow approximation (glmnet default)</li>
                    <li>Standardization (if enabled) is performed internally within each glmnet model fit, not on survival times</li>
                    <li>glmnet back-transforms displayed coefficients to the original design-column scale; an indicator-column hazard ratio is therefore its category-versus-reference contrast, although indicators are penalized and selected separately</li>
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
                    <li><strong>Prognostic Model Development:</strong> Build a sparse candidate predictor model for later validation</li>
                    <li><strong>Biomarker Screening:</strong> Explore large candidate sets while recognizing selection instability</li>
                    <li><strong>Not Clinical Decision Support:</strong> This development analysis alone does not establish calibration, clinical utility, or a treatment threshold</li>
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
                        <li><strong>Example:</strong> Coefficient = 0.693 \u{2192} HR = 2.0 (doubled risk)</li>
                    </ul>
                </div>
                
                <div class='col-md-6'>
                    <h5> Risk Scores</h5>
                    <ul>
                        <li><strong>Higher risk score:</strong> Worse expected survival</li>
                        <li><strong>Lower risk score:</strong> Better expected survival</li>
                        <li><strong>Displayed groups:</strong> The median split is descriptive and data-dependent; it is not a validated clinical cutoff</li>
                        <li><strong>Validation:</strong> Repeat all preprocessing and tuning during internal validation, then test the locked model in independent cohorts</li>
                    </ul>
                </div>
            </div>
            
            <h4> Clinical-use boundary:</h4>
            <ul>
                <li>Do not use the selected variables, apparent C-index, or median-split curves to choose treatment or follow-up.</li>
                <li>Before clinical use, specify the prediction time horizon, estimate baseline survival, assess calibration and discrimination with uncertainty, and evaluate clinical utility in the target setting.</li>
                <li>Any cutoff must be prespecified or developed and validated in separate data; the sample median is not a clinical threshold.</li>
            </ul>
            
            <h4> Model Performance Assessment:</h4>
            <p>The C-index measures ranking discrimination only. No universal C-index threshold establishes calibration, clinical utility, transportability, or readiness for patient care.</p>
            
            <div class='alert alert-warning'>
                <h4> Clinical Validation Requirements:</h4>
                <ul>
                    <li><strong>Internal Validation:</strong> Use bootstrap optimism correction or nested cross-validation that repeats preprocessing and lambda selection</li>
                    <li><strong>External Validation:</strong> Test a locked model in representative independent populations</li>
                    <li><strong>Clinical Impact:</strong> Demonstrate improved patient outcomes</li>
                    <li><strong>Implementation:</strong> Consider practical feasibility in clinical workflow</li>
                </ul>
            </div>
            
            <div class='alert alert-info'>
                <h4> Reporting Recommendations:</h4>
                <ul>
                    <li>Report both \u{03BB}.min and \u{03BB}.1se results</li>
                    <li>Describe variable selection process and stability</li>
                    <li>Report optimism-corrected discrimination and calibration with uncertainty</li>
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
                <p>The cross-validation plot shows how model performance varies with different levels of regularization (\u{03BB} values).</p>
            </div>
            
            <ul>
                <li><strong>X-axis:</strong> Log(\u{03BB}) - Regularization strength (left = weak, right = strong)</li>
                <li><strong>Y-axis:</strong> Partial likelihood deviance (lower = better fit)</li>
                <li><strong>Error bars:</strong> Standard errors across CV folds</li>
                <li><strong>Vertical lines:</strong> 
                    <ul>
                        <li>Left line: \u{03BB}.min (minimum CV error)</li>
                        <li>Right line: \u{03BB}.1se (most regularization within 1 SE)</li>
                    </ul>
                </li>
                <li><strong>Numbers at top:</strong> Number of non-zero variables at each \u{03BB}</li>
            </ul>
            
            <p><strong>Interpretation:</strong> \u{03BB}.1se is the more regularized rule and may select an empty model. It must not be replaced automatically by \u{03BB}.min; neither rule guarantees generalizability.</p>
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
                <li><strong>Ordering:</strong> Variables are ordered by |\u{03B2}| \u{00D7} SD(X); raw coefficient heights are on original predictor units and should not be compared across differently scaled variables</li>
            </ul>
            
            <p><strong>Variable Selection:</strong> Variables with non-zero coefficients at the chosen \u{03BB} are included in the final model.</p>
            "
            
            self$results$regularizationPathExplanation$setContent(html_content)
        },

        .populateRiskScoreExplanation = function() {
            html_content <- "
            <h4> Understanding Risk Scores and Survival Curves</h4>
            
            <div class='alert alert-info'>
                <p>The linear predictor combines selected design columns. The displayed survival curves split the development data at its own median score and are exploratory only.</p>
            </div>
            
            <h5>Risk Score Calculation:</h5>
            <ul>
                <li><strong>Formula:</strong> Risk Score = \u{03B2}\u{2081}\u{00D7}X\u{2081} + \u{03B2}\u{2082}\u{00D7}X\u{2082} + ... + \u{03B2}\u{2096}\u{00D7}X\u{2096}</li>
                <li><strong>Higher scores:</strong> Increased risk (worse prognosis)</li>
                <li><strong>Lower scores:</strong> Decreased risk (better prognosis)</li>
                <li><strong>Displayed groups:</strong> Split at the development-sample median; this is not a validated clinical cutoff</li>
            </ul>
            
            <h5>Survival Curve Interpretation:</h5>
            <ul>
                <li><strong>Y-axis:</strong> Survival probability (1.0 = 100% survival)</li>
                <li><strong>X-axis:</strong> Time (same units as input data)</li>
                <li><strong>Curve separation:</strong> Apparent separation is optimistically biased because the same data selected and fitted the model</li>
                <li><strong>Inference:</strong> No log-rank p-value or group hazard ratio is reported because a same-data, data-dependent split would give anti-conservative inference</li>
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

            # Compute ranks from the same scale-adjusted magnitudes displayed elsewhere.
            selected_names <- results$data$variable_names[results$selected_vars]
            selected_importance <- results$var_importance[selected_names]
            importance_order <- order(selected_importance, decreasing = TRUE,
                                      na.last = TRUE)
            importance_ranks <- integer(length(results$selected_vars))
            importance_ranks[importance_order] <- seq_along(importance_order)

            for (i in seq_along(results$selected_vars)) {
                var_idx <- results$selected_vars[i]
                var_name <- results$data$variable_names[var_idx]
                importance <- selected_importance[i]

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

            table$setNote(
                "importance",
                .("Scale-adjusted magnitude is |coefficient| multiplied by the complete-case SD of each encoded design column. It is descriptive, not causal importance or selection stability. Path inclusion proportion is the fraction of the fitted lambda path with a nonzero coefficient, not a bootstrap selection frequency.")
            )
        },

        # ── Data Suitability Assessment ─────────────────────────────
        .assessSuitability = function(results) {
            data <- results$data
            checks <- list()
            n <- data$n
            n_events <- data$n_events
            p <- ncol(data$X)
            event_rate <- n_events / n

            # - Check 1: Events-Per-Variable (EPV) --
            epv <- n_events / p
            if (epv >= 20) {
                checks$epv <- list(
                    color = "green", label = .("Events-Per-Variable"),
                    value = sprintf("%.1f (nEvents=%d, p=%d)", epv, n_events, p),
                    detail = .("Higher event information per encoded candidate column. This descriptive ratio is not a sample-size guarantee and does not establish stable selection.")
                )
            } else if (epv >= 5) {
                checks$epv <- list(
                    color = "yellow", label = .("Events-Per-Variable"),
                    value = sprintf("%.1f (nEvents=%d, p=%d)", epv, n_events, p),
                    detail = .("Limited event information per encoded candidate column. Penalization enables fitting but does not make selection or performance reliable.")
                )
            } else {
                checks$epv <- list(
                    color = "red", label = .("Events-Per-Variable"),
                    value = sprintf("%.1f (nEvents=%d, p=%d)", epv, n_events, p),
                    detail = .("Very low event information per encoded candidate column. Expect unstable selection and optimistic apparent performance; consider reducing prespecified candidates or collecting more outcome information.")
                )
            }

            # - Check 2: Regularization Need --
            if (p >= n / 3) {
                checks$regularization <- list(
                    color = "green", label = .("Regularization Need"),
                    value = sprintf("p=%d, n=%d (ratio=%.2f)", p, n, p / n),
                    detail = .("High-dimensional setting where an unpenalized full Cox model is unlikely to be stable. Penalization is appropriate, but rigorous internal validation remains necessary.")
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
                    detail = .("Penalized regression is a reasonable candidate approach for this data dimension; this check does not establish sample-size adequacy.")
                )
            }

            # - Check 3: Sample Size --
            checks$sample_size <- list(
                color = if (n < 20 || n_events < 10) "red" else "yellow",
                label = .("Sample Size"),
                value = sprintf("n=%d, events=%d, p=%d", n, n_events, p),
                detail = .("Sample-size adequacy cannot be determined from total n or a universal EPV cutoff. Use a prediction-model sample-size calculation based on outcome frequency, candidate parameters, anticipated model fit, and target optimism.")
            )

            # - Check 4: Event Rate --
            if (event_rate >= 0.20 && event_rate <= 0.80) {
                checks$event_rate <- list(
                    color = "green", label = .("Event Rate"),
                    value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
                    detail = .("Event and censoring observations are both represented; adequacy still depends on absolute counts and model complexity.")
                )
            } else if ((event_rate >= 0.10 && event_rate < 0.20) ||
                       (event_rate > 0.80 && event_rate <= 0.90)) {
                checks$event_rate <- list(
                    color = "yellow", label = .("Event Rate"),
                    value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
                    detail = .("Outcome imbalance may make folds and performance estimates unstable; inspect absolute event and censoring counts.")
                )
            } else {
                checks$event_rate <- list(
                    color = "red", label = .("Event Rate"),
                    value = sprintf("%.1f%% (%d/%d)", event_rate * 100, n_events, n),
                    detail = .("Extreme outcome imbalance provides limited information for one outcome state and may destabilize cross-validation.")
                )
            }

            # - Check 5: Multicollinearity --
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
                    col_origin[unassigned & (base::startsWith(col_names, v) | base::startsWith(col_names, v_safe))] <- v
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
                                sprintf("%s & %s (r=%.2f)",
                                        htmltools::htmlEscape(col_names[idx[k, 1]]),
                                        htmltools::htmlEscape(col_names[idx[k, 2]]),
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
                            detail = paste0(.("Moderate collinearity. LASSO may select one of several correlated columns, and that choice can be unstable. Consider Elastic Net if retaining correlated predictors is important."), pair_text)
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
                    color = "yellow", label = .("Multicollinearity"),
                    value = .("Could not compute"),
                    detail = .("The correlation summary could not be computed. This is an unavailable diagnostic, not evidence that collinearity is absent.")
                )
            })

            # - Check 6: Data Quality --
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
                    detail = .("Complete-case analysis was used. Even a small excluded fraction can bias results when missingness is informative.")
                )
            } else {
                issues <- character(0)
                if (pct_missing > 5) issues <- c(issues, sprintf(.("%.1f%% missing data (%d rows excluded). If multiple imputation is used, estimate imputation and preprocessing separately within each resampling training split."), pct_missing, n_missing))
                if (n_constant > 0) issues <- c(issues, sprintf(.("%d constant predictor column(s) detected."), n_constant))
                checks$data_quality <- list(
                    color = if (pct_missing > 20) "red" else "yellow",
                    label = .("Data Quality"),
                    value = sprintf(.("%.1f%% missing, %d constant"), pct_missing, n_constant),
                    detail = paste(issues, collapse = " ")
                )
            }

            # - Check 7: Proportional Hazards Assumption (advisory) --
            # cox.zph() does not accept a glmnet fit. Assess the same selected encoded
            # columns in an unpenalized Cox refit and label the result honestly: this is
            # an exploratory diagnostic, not valid post-selection inference on the
            # penalized estimator.
            if (length(results$selected_vars) == 0) {
                checks$ph_assumption <- list(
                    color = "yellow", label = .("Proportional Hazards"),
                    value = .("Not assessable (empty model)"),
                    detail = .("The selected lambda retained no predictor columns, so there is no fitted predictor effect for a Schoenfeld-residual diagnostic.")
                )
            } else {
                tryCatch({
                    ph_df <- as.data.frame(
                        data$X[, results$selected_vars, drop = FALSE]
                    )
                    names(ph_df) <- paste0(".v", seq_len(ncol(ph_df)))
                    ph_df$.time <- data$time
                    ph_df$.status <- data$status

                    ph_fit <- survival::coxph(
                        survival::Surv(.time, .status) ~ .,
                        data = ph_df,
                        ties = "breslow",
                        x = TRUE
                    )
                    ph_test <- survival::cox.zph(ph_fit)
                    p_rows <- ph_test$table[, "p"]
                    global_p <- if ("GLOBAL" %in% names(p_rows)) {
                        unname(p_rows["GLOBAL"])
                    } else {
                        unname(p_rows[1])
                    }
                    if (length(global_p) != 1 || !is.finite(global_p)) {
                        stop("non-finite Schoenfeld-residual test result")
                    }

                    if (global_p >= 0.05) {
                        checks$ph_assumption <- list(
                            color = "green", label = .("Proportional Hazards"),
                            value = sprintf(.("Selected-refit test p = %.3f"), global_p),
                            detail = .("No evidence of non-proportional hazards in an exploratory unpenalized refit of the selected encoded columns. Limited power and post-selection use mean that this does not prove the assumption.")
                        )
                    } else {
                        checks$ph_assumption <- list(
                            color = if (global_p < 0.01) "red" else "yellow",
                            label = .("Proportional Hazards"),
                            value = sprintf(.("Selected-refit test p = %.3f"), global_p),
                            detail = .("The exploratory selected-variable refit shows evidence of non-proportional hazards. The Cox effect estimates and selection may be misspecified; investigate time-varying effects or a prespecified alternative model.")
                        )
                    }
                }, error = function(e) {
                    checks$ph_assumption <<- list(
                        color = "yellow", label = .("Proportional Hazards"),
                        value = .("Could not assess"),
                        detail = .("The selected-variable Schoenfeld-residual diagnostic could not be computed. This is an unavailable diagnostic, not evidence that proportional hazards holds.")
                    )
                })
            }

            # - Overall Verdict --
            colors <- sapply(checks, function(x) x$color)
            if (any(colors == "red")) {
                overall <- "red"
                overall_text <- .("Some issues require attention before relying on these results.")
            } else if (any(colors == "yellow")) {
                # Special case: if only yellow is regularization-not-needed and rest green
                yellow_checks <- names(checks)[colors == "yellow"]
                if (length(yellow_checks) == 1 && yellow_checks == "regularization") {
                    overall <- "green"
                    overall_text <- .("No major concern was identified by these limited checks; standard Cox may also be appropriate.")
                } else {
                    overall <- "yellow"
                    overall_text <- .("Data is usable but review the flagged items.")
                }
            } else {
                overall <- "green"
                overall_text <- .("No concern was identified by these limited checks; formal sample-size planning and validation are still required.")
            }

            private$.generateSuitabilityHtml(checks, overall, overall_text)
        },

        .generateSuitabilityHtml = function(checks, overall, overall_text) {
            # Color mapping
            bg_colors <- list(
                green  = "background-color: rgba(33, 162, 64, 0.19); color: inherit; border: 1px solid #c3e6cb;",
                yellow = "background-color: rgba(255, 202, 33, 0.23); color: inherit; border: 1px solid #ffeeba;",
                red    = "background-color: rgba(216, 33, 50, 0.18); color: inherit; border: 1px solid #f5c6cb;"
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
                dot <- paste0("<span style='color: ", dot_colors[[chk$color]], "; font-size: 18px;'>\u{25CF}</span>")
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
                    "<div style='margin-top: 12px; padding: 10px; background-color: rgba(138, 155, 172, 0.06); border-radius: 4px; color: inherit;'>",
                    "<strong>", .("Recommendations:"), "</strong><ul style='margin: 6px 0;'>"
                )
                if (!is.null(checks$epv) && checks$epv$color == "red") {
                    html <- paste0(html, "<li>", .("Reduce candidate parameters using subject-matter knowledge or collect more outcome information; penalization alone cannot repair very limited information."), "</li>")
                }
                if (!is.null(checks$regularization) && checks$regularization$color == "yellow") {
                    html <- paste0(html, "<li>", .("Use <strong>Multivariable Survival</strong> in ClinicoPath for standard Cox regression with fewer predictors."), "</li>")
                }
                if (!is.null(checks$collinearity) && checks$collinearity$color %in% c("yellow", "red")) {
                    html <- paste0(html, "<li>", .("Consider an analysis that supports grouped or elastic-net penalties when correlated predictors should be retained together; ordinary LASSO may select only one member of a correlated set."), "</li>")
                }
                if (!is.null(checks$sample_size) && checks$sample_size$color == "red") {
                    html <- paste0(html, "<li>", .("With very small samples, consider univariable analyses or use <strong>Survival Analysis</strong> in ClinicoPath."), "</li>")
                }
                if (!is.null(checks$data_quality) && checks$data_quality$color %in% c("yellow", "red")) {
                    html <- paste0(html, "<li>", .("If using multiple imputation, repeat imputation and all preprocessing inside each resampling training split; imputing once before validation leaks information."), "</li>")
                }
                if (!is.null(checks$ph_assumption) && checks$ph_assumption$color %in% c("yellow", "red")) {
                    html <- paste0(html, "<li>", .("Review the proportional-hazards diagnostic. When violation is suspected, investigate time-dependent effects or a prespecified alternative survival model; when unavailable, do not assume PH holds."), "</li>")
                }
                html <- paste0(html, "</ul></div>")
            }

            # Interpretation guidance (always shown)
            html <- paste0(html,
                "<div style='margin-top: 10px; font-size: 12px; color: #6c757d;'>",
                "<em>", .("This assessment is advisory and cannot establish model adequacy. The analysis proceeds regardless of the verdict. Green = no issue detected by this limited check, Yellow = proceed with caution, Red = results may be highly unstable or misspecified."), "</em></div>"
            )

            self$results$suitabilityReport$setContent(html)
        },

        .populateModelComparison = function(results) {
            table <- self$results$modelComparison
            table$deleteRows()

            # Exploratory unpenalized refit after LASSO selection
            selected_refit_cindex <- NA
            selected_refit_aic <- NA
            selected_refit_loglik <- NA
            failures <- character(0)
            tryCatch({
                # Refit Cox model with only selected variables for AIC/loglik
                if (length(results$selected_vars) > 0) {
                    selected_X <- as.data.frame(results$data$X[, results$selected_vars, drop = FALSE])
                    y <- survival::Surv(results$data$time, results$data$status)
                    selected_refit <- survival::coxph(y ~ ., data = selected_X, ties = "breslow")
                    # No reverse= here. survival::concordance() rejects that argument on a
                    # coxph object ("reverse argument is not an appropriate fit object")
                    # because concordance.coxph already applies the Cox sign convention.
                    # It used to be passed, so this call errored on EVERY dataset; being the
                    # first statement in the block it also took AIC() and logLik() with it,
                    # and the empty handler below turned all six cells of this table into a
                    # permanent NA under a note describing numbers that were never computed.
                    selected_refit_cindex_result <- survival::concordance(selected_refit)
                    selected_refit_cindex <- selected_refit_cindex_result$concordance
                    selected_refit_aic <- stats::AIC(selected_refit)
                    selected_refit_loglik <- as.numeric(stats::logLik(selected_refit))
                }
            }, error = function(e) {
                failures <<- c(failures, sprintf("post-LASSO Cox refit: %s", conditionMessage(e)))
            })

            table$addRow(rowKey = 1, values = list(
                model_type = .("Unpenalized refit after LASSO selection"),
                n_variables = length(results$selected_vars),
                cindex = selected_refit_cindex,
                aic = selected_refit_aic,
                log_likelihood = selected_refit_loglik
            ))

            # Standard Cox with all variables
            std_cindex <- NA
            std_aic <- NA
            std_loglik <- NA
            tryCatch({
                y <- survival::Surv(results$data$time, results$data$status)
                selected_data <- as.data.frame(results$data$X)
                std_cox <- survival::coxph(y ~ ., data = selected_data, ties = "breslow")
                std_cindex_result <- survival::concordance(std_cox)
                std_cindex <- std_cindex_result$concordance
                std_aic <- stats::AIC(std_cox)
                std_loglik <- as.numeric(stats::logLik(std_cox))
            }, error = function(e) {
                # Standard Cox legitimately fails when p is large relative to n - say so
                # rather than presenting an empty row as though it were a result.
                failures <<- c(failures, sprintf("standard Cox on all variables: %s", conditionMessage(e)))
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
                .("Both rows are unpenalized Cox fits on the same development data. The first is fitted only after those columns were selected by LASSO; it is not the penalized model. C-index, log-likelihood, and AIC are apparent and selection-biased. In particular, the AIC values must not be used as a formal model-selection comparison because the candidate model was chosen from these same data.")
            )

            if (length(failures) > 0) {
                table$setNote(
                    "failed",
                    jmvcore::format(
                        .("Some rows could not be computed ({detail}). Empty cells mean the model could not be fitted, not that it performed poorly."),
                        detail = paste(failures, collapse = "; ")
                    )
                )
            }
        },

        # Natural-language summary for copy-ready clinical reporting
        .populateSummary = function(results) {
            n_total <- ncol(results$data$X)
            n_selected <- length(results$selected_vars)
            n_obs <- results$data$n
            n_events <- results$data$n_events
            lambda_method <- if (!is.null(results$lambda_rule_used)) results$lambda_rule_used
                             else self$options$lambda
            empty_model_note <- if (n_selected == 0)
                .("The selected rule retained no predictor columns; this valid empty model was preserved.")
            else ""

            lambda_val <- results$lambda_optimal
            metrics <- results$performance_metrics

            # Build selected variable list
            if (n_selected > 0) {
                var_names <- results$data$variable_names[results$selected_vars]
                var_list <- paste(htmltools::htmlEscape(var_names), collapse = ", ")
            } else {
                var_list <- .("none")
            }

            # Apparent C-index point estimate only. Its usual model-conditional SE
            # does not include preprocessing, tuning, or variable selection.
            cindex_text <- if (!is.na(metrics$cindex)) {
                sprintf("%.3f", metrics$cindex)
            } else {
                .("not available")
            }

            # Standardization note
            scale_note <- if (self$options$standardize) {
                .("Internal standardization was used for penalized fitting; glmnet back-transforms displayed coefficients to the original design-column scale.")
            } else {
                .("Coefficients are on the original variable scale.")
            }

            summary_text <- paste0(
                "<div style='background-color: rgba(33, 137, 255, 0.07); border: 1px solid #b8d4f0; border-radius: 6px; padding: 14px; margin-bottom: 12px; color: inherit;'>",
                "<h4 style='margin-top: 0;'>", .("Results Summary"), "</h4>",
                "<p>", jmvcore::format(
                    .("LASSO Cox regression was performed on {nObs} observations ({nEvents} events) with {nTotal} candidate predictors using {lambdaMethod} for lambda selection."),
                    nObs = n_obs, nEvents = n_events, nTotal = n_total, lambdaMethod = lambda_method), " ",
                jmvcore::format(
                    .("The model selected {nSelected} of {nTotal} variables: {varList}."),
                    nSelected = n_selected, nTotal = n_total, varList = var_list), "</p>",
                "<p>", jmvcore::format(
                    .("The apparent development-sample C-index was {cindex}."),
                    cindex = cindex_text), " ",
                if (nzchar(empty_model_note)) paste0(" ", empty_model_note) else "", "</p>",
                "<p><em>", scale_note, " ",
                .("This output is for model development only. Use bootstrap optimism correction or nested cross-validation that repeats preprocessing and tuning, followed by external validation, before considering clinical use."), "</em></p>",
                "</div>"
            )

            self$results$summaryText$setContent(summary_text)
        }
    )
)
