#' @title Coefficient Forest Plot (ggcoefstats)
#' @importFrom R6 R6Class
#' @import jmvcore
#'

jjcoefstatsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jjcoefstatsClass",
    inherit = jjcoefstatsBase,
    private = list(

        .modelFit = NULL,
        .tidyCoefs = NULL,

        .escapeVar = function(var) {
            # Safely escape variable names for formula construction
            if (is.null(var)) return(NULL)
            # Use backticks for variables with spaces/special chars
            if (grepl("[^A-Za-z0-9_\\.]", var)) {
                return(paste0("`", var, "`"))
            }
            return(var)
        },

        .cleanVarName = function(var) {
            # Clean variable name without copying entire dataset
            if (is.null(var)) return(NULL)
            janitor::make_clean_names(var)
        },

        .init = function() {
            # Show initial instructions
            instructions <- glue::glue("
            <h3>Coefficient Forest Plot (ggcoefstats)</h3>
            <hr>
            <p><b>Purpose:</b> Create publication-ready forest plots for regression coefficients
            and meta-analysis results.</p>

            <p><b>Input Modes:</b></p>
            <ul>
            <li><b>Pre-computed Coefficients:</b> Use existing model results with term names,
                estimates, standard errors, and confidence intervals</li>
            <li><b>Fit Model from Data:</b> Automatically fit a regression model (linear,
                logistic, Cox, or mixed effects) and plot the coefficients</li>
            </ul>

            <p><b>Supported Models:</b></p>
            <ul>
            <li>Linear regression (lm)</li>
            <li>Logistic regression (glm binomial)</li>
            <li>Cox proportional hazards (survival::coxph)</li>
            <li>Mixed effects models (lmer/glmer)</li>
            </ul>

            <p><b>Plot Features:</b></p>
            <ul>
            <li>Sort coefficients by magnitude</li>
            <li>Exponentiate for odds ratios or hazard ratios</li>
            <li>Multiple color schemes and themes</li>
            <li>P-value symbols or numeric display</li>
            </ul>

            <p><b>Packages Used:</b> ggstatsplot, broom, survival, lme4</p>
            <hr>
            ")

            self$results$instructions$setContent(instructions)

            # Set dynamic plot dimensions
            if (!is.null(self$options$plotWidth) && !is.null(self$options$plotHeight)) {
                width <- self$options$plotWidth
                height <- self$options$plotHeight
                self$results$coefPlot$setSize(width, height)
            }
        },


.run = function() {
    # Always generate About content
    private$.generateAboutContent()

    # Generate summary if requested
    if (isTRUE(self$options$showexplanations)) {
        private$.generateSummary()
    }

    # Check required packages
    if (!requireNamespace('ggstatsplot', quietly = TRUE)) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingGgstatsplot',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent('The ggstatsplot package is required but not installed. Please install it with install.packages("ggstatsplot") and try again.')
        self$results$insert(1, notice)
        return()
    }
    if (!requireNamespace('broom', quietly = TRUE)) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingBroom',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent('The broom package is required but not installed. Please install it with install.packages("broom") and try again.')
        self$results$insert(1, notice)
        return()
    }

    # Process based on input mode
    if (self$options$inputMode == "precomputed") {
        private$.runPrecomputed()
    } else {
        private$.runFitModel()
    }

    # Generate model summary
    private$.generateModelSummary()

    # Generate plain-language summary if explanations requested
    if (isTRUE(self$options$showexplanations)) {
        private$.generatePlainSummary()
    }

    # Add success notice if analysis completed
    if (!is.null(private$.tidyCoefs)) {
        n_obs <- if (!is.null(private$.modelFit)) {
            tryCatch(nobs(private$.modelFit), error = function(e) nrow(self$data))
        } else {
            nrow(self$data)
        }

        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'analysisComplete',
            type = jmvcore::NoticeType$INFO
        )

        dist_method <- if (self$options$inputMode == "precomputed" &&
                          !is.null(self$options$degreesOfFreedom) &&
                          self$options$degreesOfFreedom > 0) {
            sprintf('t-distribution (df = %d)', self$options$degreesOfFreedom)
        } else {
            'normal approximation'
        }

        notice$setContent(sprintf('Analysis completed successfully. %d coefficients plotted from N = %d observations. Confidence intervals and p-values calculated using %s.', nrow(private$.tidyCoefs), n_obs, dist_method))
        self$results$insert(999, notice)
    }
},

.generateAboutContent = function() {
    about_content <- glue::glue("
    <h3>About Coefficient Forest Plots</h3>
    <hr>
    <p><b>Purpose:</b> This analysis creates a forest plot to visualize the
    coefficients (estimates) from a regression model. It helps in understanding
    the direction, magnitude, and uncertainty of the effect of each predictor
    variable on the outcome.</p>

    <p><b>When to Use:</b></p>
    <ul>
        <li><b>Model Interpretation:</b> To quickly assess the results of a
        linear, logistic, or Cox proportional hazards model.</li>
        <li><b>Meta-Analysis:</b> To visualize and compare effect sizes from
        multiple studies.</li>
        <li><b>Publication:</b> To create a publication-ready summary of your
        model's findings.</li>
    </ul>

    <p><b>Key Features:</b></p>
    <ul>
        <li>Supports both pre-computed coefficients and fitting a new model
        from your data.</li>
        <li>Can exponentiate coefficients to show odds ratios (logistic) or
        hazard ratios (Cox).</li>
        <li>Provides options for sorting, theming, and customizing the plot.</li>
    </ul>
    <hr>
    ")
    self$results$about$setContent(about_content)
},

.generateSummary = function() {
    if (is.null(private$.tidyCoefs)) return()

    n_coefs <- nrow(private$.tidyCoefs)
    sig_coefs <- sum(private$.tidyCoefs$p.value < 0.05, na.rm = TRUE)

    summary_text <- glue::glue("
    <h4>Analysis Summary</h4>
    <p><b>Number of coefficients:</b> {n_coefs}</p>
    <p><b>Significant coefficients (p < 0.05):</b> {sig_coefs}</p>

    <p><b>Settings:</b></p>
    <ul>
    <li>Confidence level: {self$options$ciLevel * 100}%</li>
    <li>Exponentiated: {ifelse(self$options$exponentiate, 'Yes (OR/HR)', 'No')}</li>
    <li>Sorted: {ifelse(self$options$sortCoefs, 'Yes', 'No')}</li>
    <li>Reference value: {self$options$referenceValue}</li>
    </ul>
    ")

    self$results$modelSummary$setContent(summary_text)
},

.runPrecomputed = function() {
    # Check required variables
    if (is.null(self$options$term) || is.null(self$options$estimate) ||
        is.null(self$options$stdError)) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingRequiredVars',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent('Pre-computed mode requires three variables: Term names, Estimates, and Standard Errors. Please select all three and re-run.')
        self$results$insert(1, notice)
        return()
    }

    # Get data
    mydata <- self$data %>% janitor::clean_names()

    # Get variable names
    term_var <- janitor::make_clean_names(self$options$term)
    estimate_var <- janitor::make_clean_names(self$options$estimate)
    stderr_var <- janitor::make_clean_names(self$options$stdError)

    # Create tidy coefficients data frame
    tidy_coefs <- data.frame(
        term = as.character(mydata[[term_var]]),
        estimate = mydata[[estimate_var]],
        std.error = mydata[[stderr_var]],
        stringsAsFactors = FALSE
    )

            # Determine whether to use t-distribution or normal approximation
            use_t_dist <- !is.null(self$options$degreesOfFreedom) && self$options$degreesOfFreedom > 0
            df <- if (use_t_dist) self$options$degreesOfFreedom else NULL

    # Add CI bounds if provided
    if (!is.null(self$options$confLow)) {
        conflow_var <- janitor::make_clean_names(self$options$confLow)
        tidy_coefs$conf.low <- mydata[[conflow_var]]
    } else {
        # Calculate CI from SE if not provided
        if (use_t_dist) {
            # Use t-distribution
            t_val <- qt((1 + self$options$ciLevel) / 2, df = df)
            tidy_coefs$conf.low <- tidy_coefs$estimate - t_val * tidy_coefs$std.error
        } else {
            # Use normal approximation
            z_val <- qnorm((1 + self$options$ciLevel) / 2)
            tidy_coefs$conf.low <- tidy_coefs$estimate - z_val * tidy_coefs$std.error
        }
    }

    if (!is.null(self$options$confHigh)) {
        confhigh_var <- janitor::make_clean_names(self$options$confHigh)
        tidy_coefs$conf.high <- mydata[[confhigh_var]]
    } else {
        # Calculate CI from SE if not provided
        if (use_t_dist) {
            # Use t-distribution
            t_val <- qt((1 + self$options$ciLevel) / 2, df = df)
            tidy_coefs$conf.high <- tidy_coefs$estimate + t_val * tidy_coefs$std.error
        } else {
            # Use normal approximation
            z_val <- qnorm((1 + self$options$ciLevel) / 2)
            tidy_coefs$conf.high <- tidy_coefs$estimate + z_val * tidy_coefs$std.error
        }
    }

    # Add p-values if provided
    if (!is.null(self$options$pValue)) {
        pval_var <- janitor::make_clean_names(self$options$pValue)
        tidy_coefs$p.value <- mydata[[pval_var]]
    } else {
        # Calculate p-value from SE
        tidy_coefs$statistic <- tidy_coefs$estimate / tidy_coefs$std.error

        if (use_t_dist) {
            # Use t-distribution
            tidy_coefs$p.value <- 2 * (1 - pt(abs(tidy_coefs$statistic), df = df))
        } else {
            # Use normal approximation
            tidy_coefs$p.value <- 2 * (1 - pnorm(abs(tidy_coefs$statistic)))
        }
    }

    # Filter out intercept if requested (robust pattern)
    if (self$options$excludeIntercept) {
        # More robust pattern: match whole word only, anchor to start/end
        intercept_pattern <- "^(\\()?intercept(\\))?$|^const(ant)?$"
        tidy_coefs <- tidy_coefs[!grepl(intercept_pattern, tolower(tidy_coefs$term)), ]
    }

    # Exponentiate if requested
    if (self$options$exponentiate) {
        if ("std.error" %in% names(tidy_coefs)) {
            tidy_coefs$std.error <- tidy_coefs$std.error * exp(tidy_coefs$estimate)
        }

        tidy_coefs$estimate <- exp(tidy_coefs$estimate)
        tidy_coefs$conf.low <- exp(tidy_coefs$conf.low)
        tidy_coefs$conf.high <- exp(tidy_coefs$conf.high)

        if ("statistic" %in% names(tidy_coefs)) {
            tidy_coefs$statistic <- NULL
        }
    }

    # Sort if requested
    if (self$options$sortCoefs) {
        tidy_coefs <- tidy_coefs[order(abs(tidy_coefs$estimate), decreasing = TRUE), ]
    }

    private$.tidyCoefs <- tidy_coefs

    # Populate table
    private$.populateCoefTable(tidy_coefs)
},
        .runFitModel = function() {
            # Check required variables
            if (self$options$modelType != "cox" && is.null(self$options$outcome)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingOutcome',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Please select an outcome variable for model fitting.')
                self$results$insert(1, notice)
                return()
            }
            if (self$options$modelType == "cox" && (is.null(self$options$survivalTime) || is.null(self$options$eventStatus))) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingSurvivalVars',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Cox proportional hazards model requires both Survival Time and Event Status variables. Please select both and re-run.')
                self$results$insert(1, notice)
                return()
            }
            if (is.null(self$options$predictors) || length(self$options$predictors) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingPredictors',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('Please select at least one predictor variable for the model.')
                self$results$insert(1, notice)
                return()
            }

            # Get data
            mydata <- self$data %>% janitor::clean_names()

            # Get variable names
            outcome_var <- if (!is.null(self$options$outcome)) janitor::make_clean_names(self$options$outcome) else NULL
            predictor_vars <- janitor::make_clean_names(self$options$predictors)

            # Create formula (only if outcome is present) with proper escaping
            if (!is.null(outcome_var)) {
                outcome_safe <- private$.escapeVar(outcome_var)
                predictors_safe <- sapply(predictor_vars, private$.escapeVar)
                formula_str <- paste(outcome_safe, "~", paste(predictors_safe, collapse = " + "))
                formula <- as.formula(formula_str)
            }

            # Fit model based on type
            model_type <- self$options$modelType

            tryCatch({
                if (model_type == "lm") {
                    # Linear regression
                    model <- lm(formula, data = mydata)

                } else if (model_type == "glm") {
                    # Logistic regression - validate binary outcome
                    outcome_unique <- unique(mydata[[outcome_var]])
                    outcome_unique <- outcome_unique[!is.na(outcome_unique)]

                    if (length(outcome_unique) != 2) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'nonBinaryOutcome',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(sprintf('Logistic regression requires a binary outcome (exactly 2 unique values). Found %d unique values in %s. Expected 0/1 or TRUE/FALSE.', length(outcome_unique), self$options$outcome))
                        self$results$insert(1, notice)
                        return()
                    }

                    # Force to 0/1 if logical
                    if (is.logical(mydata[[outcome_var]])) {
                        mydata[[outcome_var]] <- as.numeric(mydata[[outcome_var]])
                        outcome_unique <- c(0, 1)
                    }

                    if (!all(outcome_unique %in% c(0, 1))) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'wrongOutcomeCoding',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(sprintf('Outcome variable must be coded as 0/1 for logistic regression. Found values: %s', paste(outcome_unique, collapse = ', ')))
                        self$results$insert(1, notice)
                        return()
                    }

                    model <- glm(formula, data = mydata, family = binomial(link = "logit"))

                } else if (model_type == "cox") {
                    # Cox proportional hazards
                    if (is.null(self$options$survivalTime) || is.null(self$options$eventStatus)) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'missingSurvivalVarsInCox',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent('Survival time and event status are required for Cox model. Please select both variables.')
                        self$results$insert(1, notice)
                        return()
                    }

                    if (!requireNamespace('survival', quietly = TRUE)) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'missingSurvivalPackage',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent('The survival package is required for Cox models. Please install it with install.packages("survival") and try again.')
                        self$results$insert(1, notice)
                        return()
                    }

                    time_var <- janitor::make_clean_names(self$options$survivalTime)
                    event_var <- janitor::make_clean_names(self$options$eventStatus)

                    # Validate event status coding
                    event_unique <- unique(mydata[[event_var]])
                    event_unique <- event_unique[!is.na(event_unique)]

                    if (is.logical(mydata[[event_var]])) {
                        mydata[[event_var]] <- as.numeric(mydata[[event_var]])
                        event_unique <- unique(mydata[[event_var]])
                    }

                    if (!all(event_unique %in% c(0, 1))) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'wrongEventCoding',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(sprintf('Event status for Cox model must be coded as 0 (censored) or 1 (event). Found values: %s', paste(event_unique, collapse = ', ')))
                        self$results$insert(1, notice)
                        return()
                    }

                    # Validate survival time is positive; warn and drop non-positive
                    if (any(mydata[[time_var]] <= 0, na.rm = TRUE)) {
                        n_removed <- sum(mydata[[time_var]] <= 0, na.rm = TRUE)
                        n_before <- nrow(mydata)
                        mydata <- mydata[mydata[[time_var]] > 0, ]
                        n_after <- nrow(mydata)

                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'nonPositiveTimes',
                            type = jmvcore::NoticeType$STRONG_WARNING
                        )
                        notice$setContent(sprintf('Removed %d observations with non-positive survival times. Cox models require time > 0. Final sample: N = %d (from original N = %d).', n_removed, n_after, n_before))
                        self$results$insert(1, notice)
                    }

                    # Create Surv object and formula with escaped predictors
                    predictors_safe <- sapply(predictor_vars, private$.escapeVar)
                    surv_formula <- as.formula(paste0("survival::Surv(", private$.escapeVar(time_var), ", ", private$.escapeVar(event_var), ") ~ ",
                                                     paste(predictors_safe, collapse = " + ")))
                    model <- survival::coxph(surv_formula, data = mydata)

                } else if (model_type == "mixed") {
                    # Mixed effects models
                    if (is.null(self$options$randomEffects)) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'missingRandomEffects',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent('Mixed effects models require a grouping variable for random effects (e.g., subject ID, cluster ID). Please select a grouping variable.')
                        self$results$insert(1, notice)
                        return()
                    }

                    if (!requireNamespace('lme4', quietly = TRUE)) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'missingLme4',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent('The lme4 package is required for mixed effects models. Please install it with install.packages("lme4") and try again.')
                        self$results$insert(1, notice)
                        return()
                    }

                    random_var <- janitor::make_clean_names(self$options$randomEffects)

                    # Construct formula with random intercept: outcome ~ predictors + (1|group)
                    predictors_safe <- sapply(predictor_vars, private$.escapeVar)
                    mixed_formula <- as.formula(paste0(private$.escapeVar(outcome_var), " ~ ",
                                                      paste(predictors_safe, collapse = " + "),
                                                      " + (1|", private$.escapeVar(random_var), ")"))

                    # Determine whether to use lmer (continuous) or glmer (binary)
                    outcome_unique <- sort(unique(mydata[[outcome_var]][!is.na(mydata[[outcome_var]])]))

                    if (length(outcome_unique) == 2) {
                        # Binary outcome - use glmer
                        if (!all(outcome_unique %in% c(0, 1))) {
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'wrongGlmerOutcome',
                                type = jmvcore::NoticeType$ERROR
                            )
                            notice$setContent('For mixed-effects logistic regression (glmer), outcome must be coded as 0 and 1.')
                            self$results$insert(1, notice)
                            return()
                        }
                        model <- lme4::glmer(mixed_formula, data = mydata, family = binomial(link = "logit"))
                    } else {
                        # Continuous outcome - use lmer
                        model <- lme4::lmer(mixed_formula, data = mydata)
                    }

                } else {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'unknownModelType',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(sprintf('Unknown or unsupported model type: %s', model_type))
                    self$results$insert(1, notice)
                    return()
                }

                private$.modelFit <- model

                # Extract tidy coefficients using broom
                if (inherits(model, "lmerMod")) {
                    if (!requireNamespace("broom.mixed", quietly = TRUE)) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'missingBroomMixed',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent('The broom.mixed package is required for mixed-effects models. Please install it with install.packages("broom.mixed") and try again.')
                        self$results$insert(1, notice)
                        return()
                    }
                    tidy_coefs <- broom.mixed::tidy(model, conf.int = TRUE, conf.level = self$options$ciLevel)
                } else {
                    tidy_coefs <- broom::tidy(model, conf.int = TRUE, conf.level = self$options$ciLevel)
                }

                # Filter out intercept if requested (robust pattern)
                if (self$options$excludeIntercept) {
                    # More robust pattern: match whole word only, anchor to start/end
                    intercept_pattern <- "^(\\()?intercept(\\))?$|^const(ant)?$"
                    tidy_coefs <- tidy_coefs[!grepl(intercept_pattern, tolower(tidy_coefs$term)), ]
                }

                # Exponentiate if requested
                if (self$options$exponentiate) {
                    # Use delta method to transform std.error: SE_exp = SE * exp(estimate)
                    if ("std.error" %in% names(tidy_coefs)) {
                        tidy_coefs$std.error <- tidy_coefs$std.error * exp(tidy_coefs$estimate)
                    }

                    # Exponentiate estimates and CI bounds
                    tidy_coefs$estimate <- exp(tidy_coefs$estimate)
                    if ("conf.low" %in% names(tidy_coefs)) {
                        tidy_coefs$conf.low <- exp(tidy_coefs$conf.low)
                    }
                    if ("conf.high" %in% names(tidy_coefs)) {
                        tidy_coefs$conf.high <- exp(tidy_coefs$conf.high)
                    }

                    # Remove statistic as it's not meaningful on exponentiated scale
                    if ("statistic" %in% names(tidy_coefs)) {
                        tidy_coefs$statistic <- NULL
                    }
                }

                # Sort if requested
                if (self$options$sortCoefs) {
                    tidy_coefs <- tidy_coefs[order(abs(tidy_coefs$estimate), decreasing = TRUE), ]
                }

                private$.tidyCoefs <- tidy_coefs

                # Check for small sample size / low events-per-predictor and add warnings
                private$.checkSampleSize(model, mydata, outcome_var, event_var, predictor_vars)

                # Populate table
                private$.populateCoefTable(tidy_coefs)

                # Populate model metrics
                private$.populateModelMetrics(model)

            }, error = function(e) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'modelFittingError',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(sprintf('Model fitting failed: %s', e$message))
                self$results$insert(1, notice)
                return()
            })
        },

        .checkSampleSize = function(model, mydata, outcome_var, event_var, predictor_vars) {
            # Check for small sample size warnings for binary/event outcomes
            tryCatch({
                if (inherits(model, c("glm", "glmerMod"))) {
                    # Logistic regression - check events-per-variable (EPV)
                    n_events <- sum(mydata[[outcome_var]] == 1, na.rm = TRUE)
                    n_predictors <- length(predictor_vars)
                    epv <- n_events / n_predictors

                    if (epv < 10) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'lowEventsPerPredictor',
                            type = jmvcore::NoticeType$STRONG_WARNING
                        )
                        notice$setContent(sprintf('Low events-per-variable ratio (EPV = %.1f, with %d events and %d predictors). Models with EPV < 10 may have unstable estimates and inflated Type I error rates. Consider reducing predictors or collecting more data.', epv, n_events, n_predictors))
                        self$results$insert(1, notice)
                    }
                } else if (inherits(model, "coxph")) {
                    # Cox model - check events-per-variable
                    n_events <- sum(mydata[[event_var]] == 1, na.rm = TRUE)
                    n_predictors <- length(predictor_vars)
                    epv <- n_events / n_predictors

                    if (n_events < 10) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'veryLowEvents',
                            type = jmvcore::NoticeType$ERROR
                        )
                        notice$setContent(sprintf('Very low number of events (N = %d). Cox models require at least 10 events for reliable estimation. Results may be highly unstable.', n_events))
                        self$results$insert(1, notice)
                    } else if (epv < 10) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'lowEventsPerPredictorCox',
                            type = jmvcore::NoticeType$STRONG_WARNING
                        )
                        notice$setContent(sprintf('Low events-per-variable ratio (EPV = %.1f, with %d events and %d predictors). Cox models with EPV < 10 may produce biased estimates. Consider reducing predictors or collecting more data.', epv, n_events, n_predictors))
                        self$results$insert(1, notice)
                    } else if (epv < 20) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'moderateEventsPerPredictorCox',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(sprintf('Moderate events-per-variable ratio (EPV = %.1f). EPV between 10-20 is acceptable but EPV > 20 is preferred for optimal performance.', epv))
                        self$results$insert(1, notice)
                    }
                } else if (inherits(model, "lm")) {
                    # Linear regression - check sample size
                    n_obs <- nobs(model)
                    n_predictors <- length(predictor_vars)
                    ratio <- n_obs / n_predictors

                    if (n_obs < 30) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'smallSampleSize',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(sprintf('Small sample size (N = %d). Linear models typically require N > 30 for reliable inference. Consider interpreting results cautiously.', n_obs))
                        self$results$insert(1, notice)
                    } else if (ratio < 10) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'lowObsPerPredictor',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(sprintf('Low observations-per-predictor ratio (%.1f with N = %d and %d predictors). A ratio of at least 10-15 observations per predictor is recommended.', ratio, n_obs, n_predictors))
                        self$results$insert(1, notice)
                    }
                }
            }, error = function(e) {
                # Silently skip sample size checks if they fail
            })
        },

        .populateCoefTable = function(tidy_coefs) {
            table <- self$results$coefficientTable

            # Adjust table titles if exponentiating
            if (self$options$exponentiate) {
                estTitle <- if (self$options$inputMode == "precomputed") {
                    if (self$options$expScaleLabel == "or") {
                        "Odds Ratio"
                    } else if (self$options$expScaleLabel == "hr") {
                        "Hazard Ratio"
                    } else {
                        "Exp(Estimate)"
                    }
                } else if (self$options$modelType == "glm") {
                    "Odds Ratio"
                } else if (self$options$modelType == "cox") {
                    "Hazard Ratio"
                } else {
                    "Exp(Estimate)"
                }
                table$getColumn("estimate")$setTitle(estTitle)
                if (!is.null(table$getColumn("statistic"))) {
                    table$getColumn("statistic")$setVisible(FALSE)
                }
            }

            for (i in 1:nrow(tidy_coefs)) {
                # Optional p-value symbols
                p_out <- NA
                if ("p.value" %in% names(tidy_coefs)) {
                    p_val <- tidy_coefs$p.value[i]
                    if (self$options$showPValues) {
                        if (self$options$pValueDisplay == "symbols") {
                            if (is.na(p_val)) {
                                p_out <- NA
                            } else if (p_val < 0.001) {
                                p_out <- "***"
                            } else if (p_val < 0.01) {
                                p_out <- "**"
                            } else if (p_val < 0.05) {
                                p_out <- "*"
                            } else {
                                p_out <- ""
                            }
                        } else if (self$options$pValueDisplay == "numeric") {
                            p_out <- p_val
                        } else {
                            # "none" option
                            p_out <- NA
                        }
                    }
                }

                table$addRow(rowKey = i, values = list(
                    term = tidy_coefs$term[i],
                    estimate = round(tidy_coefs$estimate[i], 3),
                    std_error = if ("std.error" %in% names(tidy_coefs)) round(tidy_coefs$std.error[i], 3) else NA,
                    statistic = if ("statistic" %in% names(tidy_coefs)) round(tidy_coefs$statistic[i], 3) else NA,
                    p_value = p_out,
                    conf_low = round(tidy_coefs$conf.low[i], 3),
                    conf_high = round(tidy_coefs$conf.high[i], 3)
                ))
            }
        },

        .populateModelMetrics = function(model) {
            table <- self$results$modelMetrics

            # Extract model metrics based on type
            tryCatch({
                if (inherits(model, "lm")) {
                    # R-squared for linear models
                    r_sq <- summary(model)$r.squared
                    adj_r_sq <- summary(model)$adj.r.squared
                    aic <- AIC(model)

                    table$addRow(rowKey = 1, values = list(metric = "R-squared", value = round(r_sq, 3)))
                    table$addRow(rowKey = 2, values = list(metric = "Adjusted R-squared", value = round(adj_r_sq, 3)))
                    table$addRow(rowKey = 3, values = list(metric = "AIC", value = round(aic, 1)))

                } else if (inherits(model, "glm")) {
                    # AIC and deviance for GLM
                    aic <- AIC(model)
                    deviance <- deviance(model)
                    null_dev <- model$null.deviance
                    pseudo_r2 <- 1 - (deviance / null_dev)

                    table$addRow(rowKey = 1, values = list(metric = "AIC", value = round(aic, 1)))
                    table$addRow(rowKey = 2, values = list(metric = "Pseudo R-squared", value = round(pseudo_r2, 3)))

                } else if (inherits(model, "coxph")) {
                    # Concordance for Cox models
                    concordance <- model$concordance[1]
                    aic <- AIC(model)

                    table$addRow(rowKey = 1, values = list(metric = "Concordance", value = round(concordance, 3)))
                    table$addRow(rowKey = 2, values = list(metric = "AIC", value = round(aic, 1)))

                } else if (inherits(model, c("lmerMod", "glmerMod"))) {
                    # AIC and BIC for mixed models
                    aic <- AIC(model)
                    bic <- BIC(model)

                    table$addRow(rowKey = 1, values = list(metric = "AIC", value = round(aic, 1)))
                    table$addRow(rowKey = 2, values = list(metric = "BIC", value = round(bic, 1)))
                }
            }, error = function(e) {
                # If metrics can't be extracted, add a note
                table$addRow(rowKey = 1, values = list(metric = "Note", value = "Metrics unavailable"))
            })
        },

        .generateModelSummary = function() {
            if (is.null(private$.tidyCoefs)) return()

            n_coefs <- nrow(private$.tidyCoefs)
            
            if ("p.value" %in% names(private$.tidyCoefs)) {
                sig_coefs <- sum(private$.tidyCoefs$p.value < 0.05, na.rm = TRUE)
            } else {
                sig_coefs <- "N/A"
            }


            summary_text <- glue::glue("
            <h4>Analysis Summary</h4>
            <p><b>Number of coefficients:</b> {n_coefs}</p>
            <p><b>Significant coefficients (p < 0.05):</b> {sig_coefs}</p>

            <p><b>Settings:</b></p>
            <ul>
            <li>Confidence level: {self$options$ciLevel * 100}%</li>
            <li>Exponentiated: {ifelse(self$options$exponentiate, 'Yes (OR/HR)', 'No')}</li>
            <li>Sorted: {ifelse(self$options$sortCoefs, 'Yes', 'No')}</li>
            <li>Reference value: {self$options$referenceValue}</li>
            </ul>
            ")

            # Control visibility based on showexplanations option
            if (isTRUE(self$options$showexplanations)) {
                self$results$modelSummary$setVisible(TRUE)
                self$results$modelSummary$setContent(summary_text)
            } else {
                self$results$modelSummary$setVisible(FALSE)
            }
        },

        .generatePlainSummary = function() {
            if (!isTRUE(self$options$showexplanations)) return()
            if (is.null(private$.tidyCoefs)) return()

            # Determine model type label
            model_type_label <- if (self$options$inputMode == "precomputed") {
                "pre-computed coefficients"
            } else {
                switch(self$options$modelType,
                    "lm" = "linear regression",
                    "glm" = "logistic regression",
                    "cox" = "Cox proportional hazards regression",
                    "mixed" = "mixed-effects model",
                    "regression model"
                )
            }

            # Scale label
            scale_label <- if (self$options$exponentiate) {
                if (self$options$inputMode == "precomputed") {
                    switch(self$options$expScaleLabel,
                        "or" = "odds ratios",
                        "hr" = "hazard ratios",
                        "exp" = "exponentiated coefficients"
                    )
                } else {
                    switch(self$options$modelType,
                        "glm" = "odds ratios",
                        "cox" = "hazard ratios",
                        "exponentiated coefficients"
                    )
                }
            } else {
                "regression coefficients"
            }

            n_coefs <- nrow(private$.tidyCoefs)

            # Get sample size
            n_obs <- if (!is.null(private$.modelFit)) {
                tryCatch(nobs(private$.modelFit), error = function(e) nrow(self$data))
            } else {
                nrow(self$data)
            }

            # Find most significant predictor for example
            example_text <- ""
            if ("p.value" %in% names(private$.tidyCoefs) && nrow(private$.tidyCoefs) > 0) {
                sig_idx <- which.min(private$.tidyCoefs$p.value)
                if (length(sig_idx) > 0) {
                    example_term <- private$.tidyCoefs$term[sig_idx]
                    example_est <- private$.tidyCoefs$estimate[sig_idx]
                    example_ci_low <- private$.tidyCoefs$conf.low[sig_idx]
                    example_ci_high <- private$.tidyCoefs$conf.high[sig_idx]
                    example_p <- private$.tidyCoefs$p.value[sig_idx]

                    # Format p-value
                    p_text <- if (example_p < 0.001) "p < 0.001" else sprintf("p = %.3f", example_p)

                    # Determine scale label for example
                    scale_abbrev <- if (self$options$exponentiate) {
                        if (self$options$inputMode == "precomputed") {
                            switch(self$options$expScaleLabel,
                                "or" = "OR",
                                "hr" = "HR",
                                "exp(β)"
                            )
                        } else {
                            switch(self$options$modelType,
                                "glm" = "OR",
                                "cox" = "HR",
                                "exp(β)"
                            )
                        }
                    } else {
                        "β"
                    }

                    example_text <- sprintf(
                        "The strongest effect was for <b>%s</b>, with %s = %.2f (95%% CI: %.2f to %.2f, %s).",
                        example_term,
                        scale_abbrev,
                        example_est, example_ci_low, example_ci_high, p_text
                    )
                }
            }

            summary_html <- glue::glue("
            <h4>Plain-Language Summary</h4>
            <div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>
            <p>This analysis examined <b>{n_coefs} predictors</b> using <b>{model_type_label}</b> (N = {n_obs} observations).
            Results are presented as <b>{scale_label}</b> with <b>{self$options$ciLevel * 100}% confidence intervals</b>.</p>
            {if (example_text != '') paste0('<p>', example_text, '</p>') else ''}
            <p><i>💡 Copy this summary to your report and customize as needed.</i></p>
            </div>
            ")

            self$results$plainSummary$setVisible(TRUE)
            self$results$plainSummary$setContent(summary_html)
        },

        .coefPlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.tidyCoefs)) return()

            tidy_coefs <- private$.tidyCoefs

            # Determine reference value
            ref_val <- self$options$referenceValue
            if (self$options$exponentiate && ref_val == 0) {
                ref_val <- 1  # For ratios, reference is 1
            }

                # Determine axis labels
                xlab <- if (self$options$exponentiate) {
                    if (self$options$inputMode == "precomputed") {
                        if (self$options$expScaleLabel == "or") {
                            "Odds Ratio"
                        } else if (self$options$expScaleLabel == "hr") {
                            "Hazard Ratio"
                        } else {
                            "Exponentiated Coefficient"
                        }
                    } else {
                        switch(self$options$modelType,
                            "glm" = "Odds Ratio",
                            "cox" = "Hazard Ratio",
                            "Exponentiated Coefficient"
                        )
                    }
                } else {
                    "Coefficient Estimate"
                }


            tryCatch({
                # Determine stats label args based on p-value options
                stats_label_args <- if (self$options$showPValues && self$options$pValueDisplay != "none") {
                    if (self$options$pValueDisplay == "symbols") {
                        list(p.value = TRUE, p.value.label = "symbol")
                    } else {
                        list(p.value = TRUE)
                    }
                } else {
                    list(p.value = FALSE)
                }

                # Determine color palette based on scheme
                palette_args <- switch(self$options$colorScheme,
                    "colorblind" = list(package = "ggthemes", palette = "Colorblind"),
                    "viridis" = list(package = "viridis", palette = "viridis"),
                    "grayscale" = list(package = "gray", palette = "gray"),
                    list(package = "RColorBrewer", palette = "Set2")  # default
                )

                # Always use the tidy data frame with ggcoefstats
                plot <- ggstatsplot::ggcoefstats(
                    x = tidy_coefs,
                    conf.level = self$options$ciLevel,
                    exclude.intercept = self$options$excludeIntercept,
                    sort = if (self$options$sortCoefs) "ascending" else "none",
                    stats.label.color = c("#0072B2", "#D55E00"),
                    stats.label.args = stats_label_args,
                    package = palette_args$package,
                    palette = palette_args$palette,
                    vline.args = list(
                        xintercept = ref_val,
                        linetype = "dashed",
                        color = "red"
                    ),
                    ggtheme = ggtheme
                ) +
                ggplot2::labs(x = xlab)


                # Apply theme customization
                if (self$options$plotTheme != "default") {
                    plot <- plot + switch(self$options$plotTheme,
                        "classic" = ggplot2::theme_classic(),
                        "minimal" = ggplot2::theme_minimal(),
                        "light" = ggplot2::theme_light(),
                        ggplot2::theme_gray()
                    )
                }

                print(plot)

            }, error = function(e) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                    label = paste("Error creating plot:\n", e$message),
                                    size = 5) +
                    ggplot2::theme_void()
                print(plot)
            })

            TRUE
        }
    )
)
