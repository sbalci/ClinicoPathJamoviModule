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
#' - Optional coefficient paths, encoding tables, fold provenance, and upstream R code
#'
#' Nominal and ordered factors use explicit treatment indicators, with the first
#' observed level in the factor order as reference. Global or custom contrasts are
#' not used. Ordered factors are not assumed to follow polynomial trends. Original
#' predictors and encoded columns are counted separately. Nonpositive follow-up
#' times, infinite predictor values, and predictors containing only missing values
#' are rejected. Time and outcome variables cannot also be predictors.
#'
#' The optional R code refits the same development data using upstream glmnet;
#' it is not external validation or an absolute-risk calculator. Pairwise correlation
#' diagnostics are skipped above 500 encoded columns, without changing the model.
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

        .inputsReady = function() {
            !is.null(self$options$elapsedtime) && !is.null(self$options$outcome) &&
                length(self$options$explanatory) > 0
        },

        .hasPackage = function(package) {
            requireNamespace(package, quietly = TRUE)
        },

        .dependencyError = function() {
            packages <- c("glmnet", "survival")
            missing <- packages[!vapply(packages, private$.hasPackage, logical(1))]
            if (!length(missing)) return(FALSE)
            self$results$todo$setContent(paste0(
                "<div class='alert alert-danger'><h4>", .("Missing Dependencies"), "</h4><p>",
                htmltools::htmlEscape(.fmt(
                    .("Required packages are unavailable: {packages}. Reinstall or update the module before running this analysis."),
                    packages = paste(missing, collapse = ", "))), "</p></div>"))
            TRUE
        },

        .init = function() {
            private$.clearAnalysisOutputs()
            for (name in c("lassoExplanation", "methodologyNotes", "clinicalGuidance",
                "regularizationPathExplanation", "crossValidationExplanation", "riskScoreExplanation")) {
                self$results[[name]]$setContent("")
            }
            if (private$.dependencyError()) return()
            if (!private$.inputsReady()) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-info'><h4>", .("Welcome to Lasso-Cox Regression"),
                    "</h4><p>", .("Select strictly positive follow-up time, a binary outcome with event and censored levels, and predictors that yield at least two usable encoded columns."),
                    "</p><p>", .("This analysis fits a sparse development model. Its apparent performance and median-split survival curves require validation before clinical use."),
                    "</p></div>"))
            } else {
                private$.initFixedTables()
            }
            private$.initializeExplanations()
        },

        .initFixedTables = function() {
            labels <- c(.("Encoded Predictor Columns"), .("Selected Predictor Columns"),
                .("Column Selection Proportion"), .("Optimal Lambda"), .("Penalty Selected By"),
                .("Stratified CV Folds Used"), .("Sample Size"), .("Number of Events"),
                .("Censoring Rate"), .("Event Level Used"), .("Rows Excluded (Missing Data)"),
                .("Original Candidate Predictors"), .("Selected Original Predictors"),
                .("Censored Level Used"))
            for (i in seq_along(labels)) self$results$modelSummary$addRow(rowKey = i,
                values = list(statistic = labels[i], value = NA_character_))
            self$results$performance$addRow(rowKey = 1,
                values = list(metric = .("Apparent C-index"), value = NA_character_,
                    interpretation = .("Development data only; optimism not corrected")))
            if (self$options$showModelComparison) {
                for (i in 1:2) self$results$modelComparison$addRow(rowKey = i,
                    values = list(model_type = c(.("Unpenalized refit after LASSO selection"),
                        .("Standard Cox (all variables)"))[i]))
            }
        },

        .run = function() {
            private$.clearAnalysisOutputs(keep_rows = TRUE, clear_todo = FALSE)
            if (private$.dependencyError()) return()
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

            self$results$todo$setContent("")
            if (!private$.hasPackage("survminer") && self$options$survival_plot) {
                self$results$todo$setContent(paste0("<div class='alert alert-info'><p>",
                    .("survminer is unavailable. Survival curves use the base-R fallback without a risk table."),
                    "</p></div>"))
            }

            # Collect warnings during the pipeline so they appear in jamovi GUI
            collected_warnings <- character(0)
            stability_warnings <- character(0)

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
                    private$.populateReproducibility(results)
                    if (self$options$showRCode) private$.generateRCode(results)

                    # Save plot data for rendering
                    private$.savePlotData(results)

                    if (length(stability_warnings)) {
                        self$results$todo$setContent(paste0(self$results$todo$content,
                            "<div class='alert alert-danger'><h4>", .("Model stability warnings"),
                            "</h4><ul>", paste0("<li>", htmltools::htmlEscape(stability_warnings),
                                "</li>", collapse = ""), "</ul></div>"))
                    }
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
                        self$results$todo$setContent(paste0(self$results$todo$content, warn_html))

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

                }),
                warning = function(w) {
                    if (inherits(w, "lassocox_instability")) {
                        stability_warnings <<- c(stability_warnings, conditionMessage(w))
                    } else collected_warnings <<- c(collected_warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }
            )
        },

        .warnInstability = function(message) {
            warning(structure(list(message = message, call = NULL),
                class = c("lassocox_instability", "warning", "condition")))
        },

        .clearAnalysisOutputs = function(keep_rows = FALSE, clear_todo = TRUE) {
            fixed_columns <- list(modelSummary = "value",
                performance = "value",
                modelComparison = c("n_variables", "cindex", "aic", "log_likelihood"))
            for (name in c(names(fixed_columns), "coefficients", "variableImportance",
                "encoding", "reproducibility")) {
                table <- self$results[[name]]
                for (key in names(table$notes)) table$setNote(key, NULL, init = FALSE)
                if (keep_rows && name %in% names(fixed_columns)) {
                    for (i in seq_len(table$rowCount)) table$setRow(rowNo = i,
                        values = stats::setNames(rep(list(NA), length(fixed_columns[[name]])),
                            fixed_columns[[name]]))
                } else table$deleteRows()
            }
            for (name in c("summaryText", "suitabilityReport", "rCode")) {
                self$results[[name]]$setContent("")
            }
            if (clear_todo) self$results$todo$setContent("")
            for (name in c("cv_plot", "coef_plot", "path_plot", "survival_plot")) {
                self$results[[name]]$setState(NULL)
            }
            if (!is.null(self$results$riskScore) && !is.null(self$data)) {
                self$results$riskScore$setValues(rep(NA_real_, nrow(self$data)))
            }
        },

        # Comprehensive data cleaning and validation
        .cleanData = function() {
            data <- self$data
            time_var <- self$options$elapsedtime
            outcome_var <- self$options$outcome
            explanatory_vars <- self$options$explanatory

            if (identical(time_var, outcome_var)) {
                jmvcore::reject(.("Time and outcome must be different variables."))
            }
            overlap <- intersect(explanatory_vars, c(time_var, outcome_var))
            if (length(overlap) > 0) {
                jmvcore::reject(.fmt(
                    .("Time and outcome cannot also be predictors: {vars}. Use predictors available at the intended prediction time."),
                    vars = paste(overlap, collapse = ", ")))
            }
            candidate_variables <- explanatory_vars

            # Extract core variables
            time <- jmvcore::toNumeric(data[[time_var]])
            outcome_raw <- data[[outcome_var]]
            predictors <- data[explanatory_vars]
            infinite_vars <- names(predictors)[vapply(predictors, function(x) {
                is.numeric(x) && any(is.infinite(x))
            }, logical(1))]
            if (length(infinite_vars) > 0) {
                jmvcore::reject(.fmt(
                    .("Predictors contain infinite values: {vars}. Correct these values before fitting; these predictors have not been removed as constant."),
                    vars = paste(infinite_vars, collapse = ", ")))
            }
            empty_vars <- names(predictors)[vapply(predictors, function(x) {
                all(is.na(x))
            }, logical(1))]
            if (length(empty_vars) > 0) {
                jmvcore::reject(.fmt(
                    .("Predictors contain only missing values: {vars}. Supply data or explicitly remove these predictors."),
                    vars = paste(empty_vars, collapse = ", ")))
            }

            # Determine event coding robustly for factor/character/numeric outcomes
            # Uses strict two-level encoding: event_level -> 1, censor_level -> 0,
            # anything else (including NA) -> NA (excluded by complete.cases)
            event_level_used <- NULL
            censor_level_used <- NULL

            if (is.factor(outcome_raw) || is.character(outcome_raw)) {
                outcome_chr <- as.character(outcome_raw)
                observed_levels <- sort(unique(outcome_chr[!is.na(outcome_chr)]))
                if (length(observed_levels) != 2) {
                    jmvcore::reject(.fmt(.('Outcome variable must have exactly 2 observed values. Found {n} level(s): {levels}. For competing events, construct an explicit binary cause-specific status in which the event of interest is 1 and every non-event observation is 0; do not omit other event types.'),
                        n = length(observed_levels), levels = paste(observed_levels, collapse = ", ")))
                }

                # Resolve event level
                outcome_level_opt <- self$options$outcomeLevel
                if (is.null(outcome_level_opt) || !nzchar(as.character(outcome_level_opt))) {
                    event_level_used <- observed_levels[2]
                } else {
                    event_level_used <- as.character(outcome_level_opt)
                    if (!(event_level_used %in% observed_levels)) {
                        jmvcore::reject(.fmt(.("Selected event level ('{level}') is not present in observed outcome data."),
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
                        jmvcore::reject(.fmt(.("Selected censored level ('{level}') is not present in observed outcome data."),
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
                    jmvcore::reject(.fmt(.('Numeric outcome must have exactly 2 observed values. Found {n} value(s): {values}. For competing events, construct an explicit binary cause-specific status in which the event of interest is 1 and every non-event observation is 0; do not omit other event types.'),
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
                length(unique(x[!is.na(x)])) <= 1
            })
            constant_var_names <- names(predictors)[constant_vars]
            if (any(constant_vars)) {
                constant_var_names <- names(predictors)[constant_vars]
                predictors <- predictors[, !constant_vars, drop = FALSE]
                explanatory_vars <- names(predictors)
                warning(.fmt(.('Removed constant explanatory variables: {vars}'),
                    vars = paste(constant_var_names, collapse = ", ")))
            }
            if (ncol(predictors) == 0) {
                jmvcore::reject(.("No valid explanatory variables remain after removing constant predictors."))
            }
            # Complete-case filtering across all analysis inputs
            complete <- complete.cases(time, status, predictors)
            n_complete <- sum(complete)
            n_excluded <- length(complete) - n_complete

            if (n_complete < 10) {
                jmvcore::reject(.fmt(.('Too few complete cases for analysis ({n}). Need at least 10 complete observations.'),
                    n = n_complete))
            }
            if (n_excluded > 0) {
                warning(.fmt(.('Excluded {n} row(s) with missing values in time/outcome/predictors (complete-case analysis).'),
                    n = n_excluded))
            }

            time_cc <- time[complete]
            status_cc <- status[complete]

            if (any(!is.finite(time_cc))) {
                jmvcore::reject(.("Time variable contains non-finite values after filtering. Please correct the input."))
            }
            if (any(time_cc < 0, na.rm = TRUE)) {
                jmvcore::reject(.("Time variable contains negative values. This Cox engine requires strictly positive follow-up times."))
            }
            if (any(time_cc == 0, na.rm = TRUE)) {
                jmvcore::reject(.("Time variable contains zero values. This Cox engine requires strictly positive follow-up times. Check the time origin and measurement resolution; times have not been automatically adjusted."))
            }

            # Outcome must remain binary after complete-case filtering
            if (!(length(unique(status_cc)) == 2 && all(unique(status_cc) %in% c(0, 1)))) {
                jmvcore::reject(.("Outcome is not binary after complete-case filtering. Check event level and missing-data pattern."))
            }

            n_events <- sum(status_cc == 1)
            n_censored <- sum(status_cc == 0)
            if (n_events < 3 || n_censored < 3) {
                jmvcore::reject(.fmt(
                    .('Stratified cross-validation requires at least 3 events and 3 censored observations. Found {nEvents} events and {nCensored} censored observations. This is a computational minimum, not evidence that the sample is adequate for prediction modeling.'),
                    nEvents = n_events, nCensored = n_censored))
            }

            if (n_events < 10 || n_censored < 10) {
                private$.warnInstability(.fmt(
                    .('Only {nEvents} events and {nCensored} censored observations are available. Cross-validation and selected coefficients may be highly unstable; no fixed event count guarantees reliable prediction modeling.'),
                    nEvents = n_events, nCensored = n_censored))
            }

            # Explicit treatment contrasts avoid dependence on global contrast options,
            # ordered-factor polynomial defaults, or custom factor contrast attributes.
            predictors_cc <- predictors[complete, , drop = FALSE]
            factor_predictors <- names(predictors_cc)[vapply(predictors_cc, function(x) {
                is.factor(x) || is.character(x)
            }, logical(1))]
            ordered_predictors <- factor_predictors[vapply(
                predictors_cc[factor_predictors], is.ordered, logical(1))]
            factor_levels <- list()
            contrasts_list <- list()
            encoding <- NULL
            removed_design_columns <- character()
            tryCatch({
                for (var_name in factor_predictors) {
                    x <- predictors_cc[[var_name]]
                    lev <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(x))
                    if (length(lev) < 2) {
                        jmvcore::reject(.fmt(.("Factor variable '{var}' has insufficient variation in complete cases."),
                            var = var_name))
                    }
                    factor_levels[[var_name]] <- lev
                    predictors_cc[[var_name]] <- factor(as.character(x), levels = lev)
                    contrasts_list[[var_name]] <- stats::contr.treatment(lev, base = 1)
                }

                design <- stats::model.matrix(~ ., data = predictors_cc,
                    contrasts.arg = if (length(contrasts_list)) contrasts_list else NULL)
                assignments <- attr(design, "assign")[-1]
                X <- .stripBackticks(design[, -1, drop = FALSE])
                origins <- names(predictors_cc)[assignments]
                encoding <- data.frame(
                    column = colnames(X), variable = origins,
                    coding = rep("numeric", ncol(X)),
                    reference = rep("", ncol(X)), level = rep("", ncol(X)),
                    stringsAsFactors = FALSE)
                for (var_name in factor_predictors) {
                    idx <- which(origins == var_name)
                    encoding$coding[idx] <- if (var_name %in% ordered_predictors)
                        "ordered_treatment" else "treatment"
                    encoding$reference[idx] <- factor_levels[[var_name]][1]
                    encoding$level[idx] <- factor_levels[[var_name]][-1]
                }
                if (any(!is.finite(X))) {
                    jmvcore::reject(.("Encoded predictors contain non-finite values."))
                }
                column_variance <- apply(X, 2, stats::var)
                if (any(!is.finite(column_variance))) {
                    jmvcore::reject(.("Predictor variance cannot be represented numerically. Rescale extreme numeric values before fitting."))
                }
                valid_cols <- column_variance > 0
                removed_design_columns <- colnames(X)[!valid_cols]
                if (any(!valid_cols)) warning(.fmt(
                    .("Removed design columns constant in complete cases: {vars}."),
                    vars = paste(colnames(X)[!valid_cols], collapse = ", ")))
                X <- X[, valid_cols, drop = FALSE]
                encoding <- encoding[valid_cols, , drop = FALSE]
                if (ncol(X) < 2) {
                    jmvcore::reject(.("At least two non-constant encoded predictor columns are required by this LASSO engine."))
                }
            }, error = function(e) {
                jmvcore::reject(.fmt(.('Error creating design matrix: {msg}. Check factor coding and missing values.'),
                    msg = e$message))
            })

            if (length(factor_predictors) > 0) {
                warning(.fmt(
                    .('Categorical predictors are represented by indicator columns and LASSO selects those columns individually rather than selecting each factor as a group. Factor(s): {vars}.'),
                    vars = paste(factor_predictors, collapse = ", ")))
            }
            if (length(ordered_predictors) > 0) {
                warning(.fmt(
                    .("Ordered factors use categorical treatment coding, not polynomial trends: {vars}. The first observed level in the factor order is the reference."),
                    vars = paste(ordered_predictors, collapse = ", ")))
            }

            if (n_events < ncol(X)) {
                private$.warnInstability(.fmt(
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
                candidate_variables = candidate_variables,
                removed_constants = constant_var_names,
                removed_design_columns = removed_design_columns,
                encoding = encoding,
                factor_levels = factor_levels,
                factor_predictors = factor_predictors,
                scaling_info = scaling_info,
                complete_cases = which(complete),
                event_level_used = event_level_used,
                censor_level_used = censor_level_used,
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
            if (!private$.hasPackage("glmnet") ||
                !private$.hasPackage("survival")) {
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
                jmvcore::reject(.fmt(
                    .('Cannot form stratified cross-validation folds: at least 3 folds require at least 3 events and 3 censored observations. Found {nEvents} events and {nCensored} censored observations.'),
                    nEvents = data$n_events, nCensored = data$n_censored))
            }
            if (nfolds != nfolds_requested) {
                warning(.fmt(
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
                jmvcore::reject(.fmt(.('Error in cross-validation: {msg}'), msg = e$message))
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
            
            # Use the full-data path already fitted by CV. Tables, scores, and
            # coefficient paths then describe exactly the same fitted model.
            final_model <- cv_fit$glmnet.fit
            
            # Extract coefficients and selected variables. Use glmnet's fitted degrees
            # of freedom to distinguish a genuinely empty fit from very small coefficients
            # on large-unit predictors. A fixed cutoff on the original coefficient scale
            # is not scale invariant and can erase a clinically meaningful linear predictor.
            coef_matrix <- as.matrix(coef(final_model, s = lambda_optimal))
            fitted_df <- as.integer(final_model$df[match(lambda_optimal, final_model$lambda)])
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
                warning(.fmt(
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
                foldid = foldid,
                seed_used = seed_value,
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

        .populateModelSummary = function(results) {
            values <- list(ncol(results$data$X), length(results$selected_vars),
                paste0(round(100 * length(results$selected_vars) / ncol(results$data$X), 1), "%"),
                base::format(results$lambda_optimal, scientific = TRUE, digits = 3),
                results$lambda_rule_used, results$nfolds_used, results$data$n,
                results$data$n_events,
                paste0(round(100 * results$data$n_censored / results$data$n, 1), "%"),
                results$data$event_level_used, results$data$excluded_rows,
                length(results$data$candidate_variables),
                length(unique(results$data$encoding$variable[results$selected_vars])),
                results$data$censor_level_used)
            for (i in seq_along(values)) self$results$modelSummary$setRow(rowKey = i,
                values = list(value = as.character(values[[i]])))
        },

        .populateReproducibility = function(results) {
            self$results$encoding$deleteRows()
            self$results$reproducibility$deleteRows()
            if (self$options$showEncoding) {
                for (i in seq_len(nrow(results$data$encoding))) {
                    row <- as.list(results$data$encoding[i, , drop = FALSE])
                    row$coding <- switch(row$coding,
                        numeric = .("Numeric: one-unit increase"),
                        ordered_treatment = .("Ordered factor: treatment indicators"),
                        .("Treatment indicators"))
                    row$selected <- if (i %in% results$selected_vars) .("Yes") else .("No")
                    self$results$encoding$addRow(rowKey = i, values = row)
                }
                self$results$encoding$setNote("coding",
                    .("The first observed level in the supplied factor order is the reference; character levels are sorted. Global and custom contrasts are overridden. Indicators are penalized and selected separately, not as whole factors."), init = FALSE)
            }
            if (self$options$showReproducibility) {
                details <- stats::setNames(list(
                    base::format(results$lambda_optimal, digits = 17),
                    base::format(results$cv_fit$lambda.min, digits = 17),
                    base::format(results$cv_fit$lambda.1se, digits = 17),
                    results$lambda_rule_used,
                    as.character(results$seed_used),
                    "Breslow",
                    if (self$options$standardize) .("Yes") else .("No"),
                    if (length(results$data$removed_constants))
                        paste(results$data$removed_constants, collapse = ", ") else .("None"),
                    if (length(results$data$removed_design_columns))
                        paste(results$data$removed_design_columns, collapse = ", ") else .("None"),
                    as.character(getRversion()),
                    as.character(utils::packageVersion("glmnet")),
                    as.character(utils::packageVersion("survival"))),
                    c(.("Selected lambda"), "lambda.min", "lambda.1se", .("Lambda rule"),
                      .("Random seed"), .("Tie handling"), .("Internal standardization"),
                      .("Removed constant predictors"), .("Removed constant design columns"), .("R version"),
                      .("glmnet version"), .("survival version")))
                for (k in seq_len(results$nfolds_used)) {
                    idx <- results$foldid == k
                    details[[.fmt(.("Fold {fold}: events / censored"), fold = k)]] <-
                        paste(sum(results$data$status[idx] == 1),
                              sum(results$data$status[idx] == 0), sep = " / ")
                }
                for (i in seq_along(details)) {
                    self$results$reproducibility$addRow(rowKey = i,
                        values = list(item = names(details)[i], value = details[[i]]))
                }
                self$results$reproducibility$setNote("scope",
                    .("Folds depend on complete-case row order and event coding. Reproducing a fit requires the same data, coding, row order, options, and compatible software versions. Lambda tuning is not full-process model validation."), init = FALSE)
            }
        },

        .buildRCode = function(results) {
            # niceNames can emit invalid backticked list names containing backticks.
            # Explicit attributes keep all user-provided names inside quoted strings.
            literal <- function(x) paste(utils::capture.output(dput(x,
                control = c("keepNA", "keepInteger", "showAttributes"))), collapse = "\n")
            lines <- c(
                "# Refit this development analysis using upstream packages.",
                "# Supply the SAME data frame named data, including factor levels and row order.",
                "# This code does not validate the model or calculate absolute event probabilities.",
                paste0("# glmnet ", utils::packageVersion("glmnet"),
                    "; survival ", utils::packageVersion("survival")),
                "lassocox_fit <- local({",
                paste0("  variables <- ", literal(results$data$original_variable_names)),
                paste0("  time_name <- ", literal(self$options$elapsedtime)),
                paste0("  outcome_name <- ", literal(self$options$outcome)),
                paste0("  event_level <- ", literal(results$data$event_level_used)),
                paste0("  censor_level <- ", literal(results$data$censor_level_used)),
                paste0("  factor_levels <- ", literal(results$data$factor_levels)),
                paste0("  columns <- ", literal(results$data$variable_names)),
                "  stopifnot(all(c(variables, time_name, outcome_name) %in% names(data)))",
                "  predictors <- data[, variables, drop = FALSE]",
                "  time <- as.numeric(data[[time_name]])",
                "  outcome <- as.character(data[[outcome_name]])",
                "  stopifnot(all(is.na(outcome) | outcome %in% c(event_level, censor_level)))",
                "  status <- ifelse(is.na(outcome), NA_integer_, as.integer(outcome == event_level))",
                "  complete <- complete.cases(time, status, predictors)",
                "  predictors <- predictors[complete, , drop = FALSE]",
                "  time <- time[complete]",
                "  status <- status[complete]",
                "  stopifnot(all(is.finite(time)), all(time > 0))",
                "  contrasts_list <- list()",
                "  for (v in names(factor_levels)) {",
                "    lev <- factor_levels[[v]]",
                "    stopifnot(all(as.character(predictors[[v]]) %in% lev))",
                "    predictors[[v]] <- factor(as.character(predictors[[v]]), levels = lev)",
                "    contrasts_list[[v]] <- stats::contr.treatment(lev, base = 1)",
                "  }",
                "  X <- stats::model.matrix(~ ., data = predictors,",
                "    contrasts.arg = if (length(contrasts_list)) contrasts_list else NULL)[, -1, drop = FALSE]",
                "  colnames(X) <- make.unique(gsub('`', '', colnames(X), fixed = TRUE), sep = '_')",
                "  stopifnot(all(columns %in% colnames(X)))",
                "  X <- X[, columns, drop = FALSE]",
                "  stopifnot(all(is.finite(X)))",
                paste0("  nfolds <- ", results$nfolds_used, "L"),
                "  event_idx <- which(status == 1)",
                "  cens_idx <- which(status == 0)",
                "  stopifnot(length(event_idx) >= nfolds, length(cens_idx) >= nfolds)",
                "  had_seed <- exists('.Random.seed', envir = .GlobalEnv, inherits = FALSE)",
                "  old_seed <- if (had_seed) get('.Random.seed', envir = .GlobalEnv) else NULL",
                "  on.exit({",
                "    if (had_seed) assign('.Random.seed', old_seed, envir = .GlobalEnv)",
                "    else if (exists('.Random.seed', envir = .GlobalEnv, inherits = FALSE))",
                "      rm('.Random.seed', envir = .GlobalEnv)",
                "  }, add = TRUE)",
                paste0("  set.seed(", results$seed_used, ")"),
                "  foldid <- integer(length(status))",
                "  foldid[event_idx] <- sample(rep(seq_len(nfolds), length.out = length(event_idx)))",
                "  foldid[cens_idx] <- sample(rep(seq_len(nfolds), length.out = length(cens_idx)))",
                paste0("  set.seed(", results$seed_used, ")"),
                "  y <- survival::Surv(time, status)",
                "  cv_fit <- glmnet::cv.glmnet(X, y, family = 'cox', cox.ties = 'breslow',",
                paste0("    alpha = 1, standardize = ", literal(isTRUE(self$options$standardize)),
                    ", foldid = foldid, parallel = FALSE)"),
                paste0("  lambda_selected <- cv_fit[[", literal(results$lambda_rule_used), "]]"),
                "  beta <- as.matrix(stats::coef(cv_fit, s = lambda_selected))",
                "  if (cv_fit$glmnet.fit$df[match(lambda_selected, cv_fit$glmnet.fit$lambda)] == 0)",
                "    beta[,] <- 0",
                "  score <- as.numeric(X %*% beta[, 1])",
                "  risk_scores <- rep(NA_real_, nrow(data))",
                "  risk_scores[complete] <- score",
                "  apparent_c <- survival::concordance(y ~ score, reverse = TRUE)$concordance",
                "  list(cv_fit = cv_fit, coefficients = beta, lambda = lambda_selected,",
                "    X = X, foldid = foldid, complete_rows = which(complete),",
                "    risk_scores = risk_scores, apparent_c = apparent_c)",
                "})",
                "# Optional: plot(lassocox_fit$cv_fit)",
                "# Optional: plot(lassocox_fit$cv_fit$glmnet.fit, xvar = 'lambda')")
            paste(lines, collapse = "\n")
        },

        .generateRCode = function(results) {
            code <- private$.buildRCode(results)
            self$results$rCode$setContent(paste0(
                "<pre style='white-space: pre-wrap; color: inherit;'><code>",
                htmltools::htmlEscape(code), "</code></pre>"))
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
                    .("The selected cross-validation rule produced a valid empty model. No less-penalized model was substituted."), init = FALSE)
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
                .("Coefficient and Hazard Ratio are from the penalized LASSO Cox fit at the selected lambda. Conventional p-values and confidence intervals are intentionally not reported because same-data post-selection inference would not account for variable and penalty selection."), init = FALSE)
            table$setNote("importance",
                .("Scale-adjusted magnitude is |coefficient| multiplied by the complete-case SD of that encoded design column. It describes a one-SD change in the fitted linear predictor, not causal importance or selection stability; for a factor indicator it is not the category-versus-reference hazard ratio."), init = FALSE)
            if (!is.null(results$data$scaling_info)) {
                table$setNote("scale",
                    .("Predictors were standardized internally within glmnet fitting for the penalty calculation. glmnet back-transforms the displayed coefficients to the original design-column scale; indicator-column hazard ratios therefore compare that category with its reference category. Indicators are still penalized and selected separately rather than as a grouped factor."), init = FALSE)
            } else if (length(results$data$factor_predictors) > 0) {
                table$setNote("factor",
                    .("Categorical predictors are expanded into indicator columns and those columns are selected separately; this is not grouped selection of the whole factor."), init = FALSE)
            }
        },

        .populatePerformance = function(results) {
            value <- results$performance_metrics$cindex
            self$results$performance$setRow(rowKey = 1, values = list(
                value = if (is.finite(value)) as.character(round(value, 3)) else .("Not available")))
            self$results$performance$setNote("apparent",
                .("This C-index is apparent (training) performance from the same patients used for preprocessing, penalty selection, and model fitting. Its uncertainty does not include the modeling process. Use bootstrap optimism correction or nested cross-validation that repeats all preprocessing and tuning, followed by external validation before clinical use."), init = FALSE)
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

        .pathPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            tick_idx <- unique(round(seq(1, length(state$lambda), length.out = 6)))
            p <- ggplot2::ggplot(state$paths,
                ggplot2::aes(x = log_lambda, y = coefficient, group = column, color = column)) +
                ggtheme +
                ggplot2::geom_line(linewidth = 0.6) +
                ggplot2::geom_vline(xintercept = log(state$lambda_min),
                    color = "#0072B2", linetype = "dashed") +
                ggplot2::geom_vline(xintercept = log(state$lambda_1se),
                    color = "#D55E00", linetype = "dotted") +
                ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(
                    breaks = log(state$lambda[tick_idx]), labels = state$nzero[tick_idx],
                    name = .("Nonzero columns in the full model"))) +
                ggplot2::labs(x = .("Log lambda"), y = .("Penalized coefficient"),
                    color = NULL,
                    subtitle = .("Dashed blue: lambda.min; dotted orange: lambda.1se"),
                    caption = paste(strwrap(.fmt(
                        .("Showing {shown} of {total} paths on the original scale, ranked by summed absolute coefficients (not clinical importance). Long legend labels are abbreviated."),
                        shown = state$shown, total = state$total), width = 90), collapse = "\n")) +
                ggplot2::scale_color_discrete(labels = function(x) {
                    if (any(nchar(x) > 18)) abbreviate(x, minlength = 12, strict = TRUE) else x
                }) +
                ggplot2::theme(legend.position = "bottom",
                    legend.text = ggplot2::element_text(size = 7),
                    legend.key.height = grid::unit(9, "pt"),
                    legend.key.width = grid::unit(10, "pt"),
                    legend.spacing.y = grid::unit(0, "pt"),
                    legend.margin = ggplot2::margin(0, 0, 0, 0),
                    plot.subtitle = ggplot2::element_text(size = 10),
                    plot.caption = ggplot2::element_text(size = 8),
                    axis.title = ggplot2::element_text(size = 11),
                    axis.text = ggplot2::element_text(size = 9)) +
                ggplot2::guides(color = ggplot2::guide_legend(ncol = 6, byrow = TRUE))
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
                ggtheme +
                ggplot2::geom_col(ggplot2::aes(fill = coefficient > 0), alpha = 0.7) +
                # Both vectors must be NAME-matched. `values` already was, but `labels`
                # was a bare positional vector, so when every selected coefficient shared a
                # sign only one level existed and the first label landed on it: an
                # all-positive (risk-increasing) model drew red bars labelled "Protective".
                # Mixed signs happened to render correctly, which is why it survived.
                ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                                          labels = c("FALSE" = .("Lower fitted hazard"),
                                                     "TRUE"  = .("Higher fitted hazard")),
                                          breaks = c("FALSE", "TRUE"),
                                          limits = c("FALSE", "TRUE"),
                                          drop = FALSE,
                                          name = .("Effect")) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = .("Selected Variables and Coefficients"),
                    subtitle = paste(strwrap(
                        .("Penalized coefficients at the selected lambda, on the original predictor scale."),
                        width = 60), collapse = "\n"),
                    x = .("Variables"),
                    y = .("Penalized coefficient")
                ) +
                ggplot2::theme(legend.position = "bottom",
                    legend.title = ggplot2::element_blank(),
                    legend.text = ggplot2::element_text(size = 9),
                    plot.title = ggplot2::element_text(size = 12),
                    plot.subtitle = ggplot2::element_text(size = 9),
                    axis.title = ggplot2::element_text(size = 11),
                    axis.text = ggplot2::element_text(size = 9))

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
                  paste(vapply(strsplit(text_warning, "\n", fixed = TRUE)[[1]],
                    function(line) paste(strwrap(line, width = 62), collapse = "\n"),
                    character(1)), collapse = "\n"),
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
                  paste(vapply(strsplit(text_warning, "\n", fixed = TRUE)[[1]],
                    function(line) paste(strwrap(line, width = 62), collapse = "\n"),
                    character(1)), collapse = "\n"),
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
                      paste(vapply(strsplit(text_warning, "\n", fixed = TRUE)[[1]],
                    function(line) paste(strwrap(line, width = 62), collapse = "\n"),
                    character(1)), collapse = "\n"),
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
                      paste(vapply(strsplit(text_warning, "\n", fixed = TRUE)[[1]],
                    function(line) paste(strwrap(line, width = 62), collapse = "\n"),
                    character(1)), collapse = "\n"),
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
                if (private$.hasPackage("survminer")) {
                    p <- .quietly(survminer::ggsurvplot(
                        fit,
                        data = plot_data,
                        risk.table = TRUE,
                        risk.table.y.text = FALSE,
                        risk.table.height = 0.32,
                        risk.table.fontsize = 3,
                        risk.table.title = .("Number at risk"),
                        tables.theme = survminer::theme_cleantable(base_size = 10),
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
                    p$plot <- p$plot + ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 12),
                        legend.title = ggplot2::element_text(size = 10),
                        legend.text = ggplot2::element_text(size = 10),
                        axis.title = ggplot2::element_text(size = 11),
                        axis.text = ggplot2::element_text(size = 9))
                    p$table <- p$table + ggplot2::theme(
                        plot.title = ggplot2::element_text(size = 10),
                        axis.text = ggplot2::element_text(size = 9))
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
                text_warning <- .fmt(.("Error creating survival plot:\n{msg}\n\nPlease check your data and model parameters."), msg = e$message)
                
                grid::grid.newpage()
                vp <- grid::viewport(width = 0.9, height = 0.9, x = 0.5, y = 0.5)
                grid::pushViewport(vp)
                grid::grid.text(
                  paste(vapply(strsplit(text_warning, "\n", fixed = TRUE)[[1]],
                    function(line) paste(strwrap(line, width = 62), collapse = "\n"),
                    character(1)), collapse = "\n"),
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
            if (self$options$path_plot) {
                beta <- results$cv_fit$glmnet.fit$beta
                path_order <- order(Matrix::rowSums(abs(beta)), decreasing = TRUE)
                shown <- head(path_order, 30L)
                path_beta <- as.matrix(beta[shown, , drop = FALSE])
                path_beta[, results$cv_fit$glmnet.fit$df == 0] <- 0
                path_lambda <- as.numeric(results$cv_fit$glmnet.fit$lambda)
                self$results$path_plot$setState(list(
                    paths = data.frame(
                        log_lambda = rep(log(path_lambda), times = length(shown)),
                        coefficient = as.numeric(t(path_beta)),
                        column = rep(rownames(path_beta), each = length(path_lambda)),
                        stringsAsFactors = FALSE),
                    lambda = path_lambda,
                    nzero = as.integer(results$cv_fit$glmnet.fit$df),
                    lambda_min = as.numeric(results$cv_fit$lambda.min),
                    lambda_1se = as.numeric(results$cv_fit$lambda.1se),
                    shown = length(shown), total = nrow(beta)))
            } else {
                self$results$path_plot$setState(NULL)
            }
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

        .explanation = function(title, paragraphs) {
            paste0("<h4>", htmltools::htmlEscape(title), "</h4><ul>",
                paste0("<li>", htmltools::htmlEscape(paragraphs), "</li>", collapse = ""), "</ul>")
        },

        .initializeExplanations = function() {
            if (self$options$showExplanations) {
                private$.populateLassoExplanation()
                private$.populateCrossValidationExplanation()
                private$.populateRegularizationPathExplanation()
                private$.populateRiskScoreExplanation()
            }
            if (self$options$showMethodologyNotes) private$.populateMethodologyNotes()
            if (self$options$includeClinicalGuidance) private$.populateClinicalGuidance()
        },

        .populateLassoExplanation = function() {
            self$results$lassoExplanation$setContent(private$.explanation(
                .("Understanding LASSO Cox regression"), c(
                    .("LASSO Cox regression combines the proportional-hazards model with an L1 penalty to estimate a sparse development model."),
                    .("Increasing lambda shrinks coefficients toward zero. A coefficient of zero is a selection result in these data, not proof that a predictor is clinically irrelevant."),
                    .("Nominal and ordered factors use treatment indicators. The first observed factor level is the reference, and indicators are selected separately rather than as a group."),
                    .("The minimum-error and 1-SE rules are tuning choices. An empty model selected by the requested rule is preserved."),
                    .("Penalized hazard ratios describe fitted relative hazards, not event probabilities or causal effects. Ordinary post-selection p-values and confidence intervals are not reported."),
                    .("The displayed C-index is apparent development performance. Validate the entire modeling process before considering clinical use."))))
        },

        .populateMethodologyNotes = function() {
            self$results$methodologyNotes$setContent(private$.explanation(
                .("LASSO Cox methodology"), c(
                    .("Rows missing time, outcome or a retained predictor are excluded. Infinite values and entirely missing predictors are rejected; constant predictors are reported and removed."),
                    .("Cross-validation folds are stratified by event status and use the recorded random seed. Fold counts may be reduced to retain both outcome states in each fold."),
                    .("glmnet standardizes within each training fit when requested. Displayed coefficients are returned on the original predictor scale."),
                    .("The selected lambda and the full-data coefficients come from the same fitted path, with Breslow handling of tied event times."),
                    .("Scale-adjusted magnitude is |coefficient| multiplied by the encoded-column standard deviation. Path inclusion is not bootstrap selection frequency or evidence of stability."),
                    .("The PH diagnostic uses an exploratory unpenalized refit of selected columns. It is not a test of post-selection inference for the penalized estimator."),
                    .("Both optional comparison rows are unpenalized development-data fits. Their AIC values are not a formal selection-adjusted model comparison."))))
        },

        .populateClinicalGuidance = function() {
            self$results$clinicalGuidance$setContent(private$.explanation(
                .("Clinical interpretation and validation"), c(
                    .("Do not use selected variables, apparent discrimination or development-sample risk groups to choose treatment or surveillance intervals."),
                    .("No universal C-index or events-per-variable threshold establishes calibration, transportability or clinical utility."),
                    .("Internal validation must repeat preprocessing, lambda selection and fitting within each resample. Independently evaluate a frozen model in external data."),
                    .("Absolute event probabilities require a baseline survival estimate and evaluation at clinically relevant time horizons; the exported linear predictor alone is insufficient."),
                    .("If imputation or cutoffs are developed, learn them only in the development training splits and apply the frozen definitions to validation patients."),
                    .("Report the endpoint, candidate parameters, exclusions, coding, penalty rule, folds, software versions, and validation limitations."))))
        },

        .populateCrossValidationExplanation = function() {
            self$results$crossValidationExplanation$setContent(private$.explanation(
                .("Understanding the cross-validation plot"), c(
                    .("The horizontal axis is log lambda; the vertical axis is cross-validated partial-likelihood deviance. Lower deviance is better for this tuning criterion."),
                    .("Error bars show cross-validation standard errors. The blue line marks lambda.min and the green line marks lambda.1se."),
                    .("The 1-SE rule selects the most regularized value within one standard error of the minimum and may select no predictors."),
                    .("Nonzero-column counts appear above the separate coefficient-path plot, not above this CV plot. Lambda tuning does not provide an independent performance estimate."))))
        },

        .populateRegularizationPathExplanation = function() {
            self$results$regularizationPathExplanation$setContent(private$.explanation(
                .("Understanding coefficients and paths"), c(
                    .("The coefficient summary is a horizontal bar plot: the horizontal axis shows penalized coefficients and the vertical axis shows selected encoded columns."),
                    .("Bar colors indicate higher or lower fitted hazard. Ordering uses |coefficient| times the column standard deviation; raw bar lengths use original units."),
                    .("The separate coefficient-path plot shows coefficients against log lambda, with full-model nonzero counts on its upper axis."),
                    .("At most 30 paths are shown, ranked by summed absolute coefficients across the path. All retained columns remain in fitting; trace rank is not clinical importance."),
                    .("The path legend uses a compact layout and abbreviates long names. Predictor encoding lists the complete design-column names."))))
        },

        .populateRiskScoreExplanation = function() {
            self$results$riskScoreExplanation$setContent(private$.explanation(
                .("Understanding risk scores and survival curves"), c(
                    .("The score is the sum of encoded predictor values multiplied by their penalized coefficients. Higher scores indicate higher fitted hazard."),
                    .("Saved scores align with the original rows; excluded observations receive missing values. A valid empty model produces zero scores."),
                    .("Survival curves split the development sample at its median score. This data-dependent split is descriptive and is not a validated clinical cutoff."),
                    .("No log-rank p-value or group hazard ratio is reported for this same-data split. Uniform scores cannot form two risk groups."),
                    .("These curves do not estimate an individual patient's absolute risk. External use requires a frozen prediction model and independent validation."))))
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
                .("Scale-adjusted magnitude is |coefficient| multiplied by the complete-case SD of each encoded design column. It is descriptive, not causal importance or selection stability. Path inclusion proportion is the fraction of the fitted lambda path with a nonzero coefficient, not a bootstrap selection frequency."),
                init = FALSE)
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
                color = if (n < 20 || n_events < 10) "red" else "neutral",
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
                # Term assignments are captured before subsetting the model matrix.
                col_names <- colnames(data$X)
                col_origin <- data$encoding$variable

                if (ncol(data$X) > 500) {
                    checks$collinearity <- list(
                        color = "yellow", label = .("Pairwise Correlations"),
                        value = .("Not computed"),
                        detail = .("Pairwise correlation diagnostics are limited to 500 encoded columns to bound memory and computation. The fitted model still uses every retained column."))
                } else if (ncol(data$X) >= 2) {
                    cor_matrix <- cor(data$X, use = "pairwise.complete.obs")
                    diag(cor_matrix) <- 0
                    # Within-factor dummy correlations describe the coding constraint,
                    # rather than redundancy between independently supplied predictors.
                    origin_equal <- outer(col_origin, col_origin, "==")
                    cor_matrix[!is.na(origin_equal) & origin_equal] <- 0
                    max_cor <- max(abs(cor_matrix), na.rm = TRUE)

                    # Find top correlated pairs
                    top_pairs <- character(0)
                    if (max_cor > 0.5) {
                        cor_vals <- sort(abs(cor_matrix[upper.tri(cor_matrix)]), decreasing = TRUE)
                        idx <- which(abs(cor_matrix) >= cor_vals[min(3, length(cor_vals))] & upper.tri(cor_matrix), arr.ind = TRUE)
                        for (k in seq_len(min(3, nrow(idx)))) {
                            top_pairs <- c(top_pairs,
                                sprintf("%s & %s (r=%.2f)",
                                        col_names[idx[k, 1]],
                                        col_names[idx[k, 2]],
                                        cor_matrix[idx[k, 1], idx[k, 2]]))
                        }
                    }
                    pair_text <- if (length(top_pairs) > 0) {
                        paste0(" ", .fmt(
                            .("Top correlated encoded-column pairs: {pairs}."),
                            pairs = htmltools::htmlEscape(paste(top_pairs, collapse = "; "))))
                    } else ""

                    if (max_cor < 0.7) {
                        checks$collinearity <- list(
                            color = "green", label = .("Multicollinearity"),
                            value = sprintf("Max |r| = %.2f", max_cor),
                            detail = paste0(.("No pairwise correlation reaches the warning threshold; this does not rule out multivariable linear dependencies."), pair_text)
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

            n_constant <- length(data$removed_constants) + length(data$removed_design_columns)
            issues <- character()
            if (n_missing > 0) issues <- c(issues, .fmt(
                .("Complete-case analysis excluded {n} rows ({pct}%). Even a small excluded fraction can bias results when missingness is informative."),
                n = n_missing, pct = sprintf("%.1f", pct_missing)))
            if (length(data$removed_constants)) issues <- c(issues, .fmt(
                .("Removed constant candidate predictors: {vars}."),
                vars = paste(data$removed_constants, collapse = ", ")))
            if (length(data$removed_design_columns)) issues <- c(issues, .fmt(
                .("Removed encoded columns constant in complete cases: {vars}."),
                vars = paste(data$removed_design_columns, collapse = ", ")))
            checks$data_quality <- list(
                color = if (pct_missing > 20) "red" else if (length(issues)) "yellow" else "green",
                label = .("Data Quality"),
                value = if (length(issues)) .fmt(
                    .("{n} rows excluded; {constants} constant predictors or columns removed"),
                    n = n_missing, constants = n_constant) else .("No issues"),
                detail = if (length(issues)) htmltools::htmlEscape(paste(issues, collapse = " "))
                    else .("Complete data with no constant predictors."))

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
                    ph_fit <- private$.coxRefit(data, results$selected_vars)
                    ph_warnings <- character(0)
                    ph_test <- withCallingHandlers(
                        survival::cox.zph(ph_fit),
                        warning = function(w) {
                            ph_warnings <<- c(ph_warnings, conditionMessage(w))
                            invokeRestart("muffleWarning")
                        }
                    )
                    if (length(ph_warnings) > 0) {
                        stop(.fmt(
                            .("The proportional-hazards diagnostic did not complete cleanly: {message}"),
                            message = paste(unique(ph_warnings), collapse = "; ")
                        ), call. = FALSE)
                    }
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
            dot_colors <- list(green = "#28a745", yellow = "#ffc107",
                red = "#dc3545", neutral = "#6c757d")

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
                "<em>", .("This assessment is advisory and cannot establish model adequacy. The analysis proceeds regardless of the verdict. Green = no issue detected by this limited check, Gray = adequacy not determined by this diagnostic, Yellow = proceed with caution, Red = results may be highly unstable or misspecified."), "</em></div>"
            )

            self$results$suitabilityReport$setContent(html)
        },

        .coxRefit = function(data, columns) {
            refit_data <- as.data.frame(data$X[, columns, drop = FALSE])
            names(refit_data) <- paste0(".v", seq_len(ncol(refit_data)))
            refit_data$.time <- data$time
            refit_data$.status <- data$status
            refit_warnings <- character(0)
            fit <- withCallingHandlers(
                survival::coxph(survival::Surv(.time, .status) ~ .,
                    data = refit_data, ties = "breslow", x = TRUE),
                warning = function(w) {
                    refit_warnings <<- c(refit_warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }
            )
            if (length(refit_warnings) > 0) {
                stop(.fmt(
                    .("Cox refit did not converge cleanly: {message}"),
                    message = paste(unique(refit_warnings), collapse = "; ")
                ), call. = FALSE)
            }
            coefficients <- stats::coef(fit)
            if (length(coefficients) == 0 || any(!is.finite(coefficients))) {
                stop(.("Cox refit returned non-finite coefficient estimates."), call. = FALSE)
            }
            refit_loglik <- as.numeric(stats::logLik(fit))
            if (length(refit_loglik) == 0 || any(!is.finite(refit_loglik))) {
                stop(.("Cox refit returned a non-finite log-likelihood."), call. = FALSE)
            }
            fit
        },

        .populateModelComparison = function(results) {
            table <- self$results$modelComparison

            # Exploratory unpenalized refit after LASSO selection
            selected_refit_cindex <- NA
            selected_refit_aic <- NA
            selected_refit_loglik <- NA
            failures <- character(0)
            tryCatch({
                # Refit Cox model with only selected variables for AIC/loglik
                if (length(results$selected_vars) > 0) {
                    selected_refit <- private$.coxRefit(results$data, results$selected_vars)
                    # concordance.coxph already applies the Cox score orientation.
                    selected_refit_cindex_result <- survival::concordance(selected_refit)
                    selected_refit_cindex <- selected_refit_cindex_result$concordance
                    selected_refit_aic <- stats::AIC(selected_refit)
                    selected_refit_loglik <- as.numeric(stats::logLik(selected_refit))
                }
            }, error = function(e) {
                failures <<- c(failures, .fmt(.("Post-LASSO Cox refit: {message}"), message = conditionMessage(e)))
            })

            table$setRow(rowNo = 1, values = list(
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
                std_cox <- private$.coxRefit(results$data, seq_len(ncol(results$data$X)))
                std_cindex_result <- survival::concordance(std_cox)
                std_cindex <- std_cindex_result$concordance
                std_aic <- stats::AIC(std_cox)
                std_loglik <- as.numeric(stats::logLik(std_cox))
            }, error = function(e) {
                # Standard Cox legitimately fails when p is large relative to n - say so
                # rather than presenting an empty row as though it were a result.
                failures <<- c(failures, .fmt(.("Standard Cox on all variables: {message}"), message = conditionMessage(e)))
            })

            table$setRow(rowNo = 2, values = list(
                model_type = .("Standard Cox (all variables)"),
                n_variables = ncol(results$data$X),
                cindex = std_cindex,
                aic = std_aic,
                log_likelihood = std_loglik
            ))

            table$setNote(
                "comparison_note",
                .("Both rows are unpenalized Cox fits on the same development data. The first is fitted only after those columns were selected by LASSO; it is not the penalized model. C-index, log-likelihood, and AIC are apparent and selection-biased. In particular, the AIC values must not be used as a formal model-selection comparison because the candidate model was chosen from these same data."),
                init = FALSE)

            if (length(failures) > 0) {
                table$setNote(
                    "failed",
                    .fmt(
                        .("Some rows could not be computed ({detail}). Empty cells mean the model could not be fitted, not that it performed poorly."),
                        detail = paste(failures, collapse = "; ")
                    ),
                    init = FALSE)
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
                "<p>", .fmt(
                    .("LASSO Cox regression was performed on {nObs} observations ({nEvents} events) with {nTotal} encoded predictor columns using {lambdaMethod} for lambda selection."),
                    nObs = n_obs, nEvents = n_events, nTotal = n_total, lambdaMethod = lambda_method), " ",
                .fmt(
                    .("The model selected {nSelected} of {nTotal} encoded columns: {varList}."),
                    nSelected = n_selected, nTotal = n_total, varList = var_list), "</p>",
                "<p>", .fmt(
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
