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
        stop('The ggstatsplot package is required but not installed')
    }
    if (!requireNamespace('broom', quietly = TRUE)) {
        stop('The broom package is required but not installed')
    }

    # Process based on input mode
    if (self$options$inputMode == "precomputed") {
        private$.runPrecomputed()
    } else {
        private$.runFitModel()
    }

    # Generate model summary
    private$.generateModelSummary()
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

    # Filter out intercept if requested (broader match)
    if (self$options$excludeIntercept) {
        tidy_coefs <- tidy_coefs[!grepl("intercept|\\(intercept\\)|const", tolower(tidy_coefs$term)), ]
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
            # Check required variables
            if (self$options$modelType != "cox" && is.null(self$options$outcome)) {
                return()
            }
            if (self$options$modelType == "cox" && (is.null(self$options$survivalTime) || is.null(self$options$eventStatus))) {
                return()
            }
            if (is.null(self$options$predictors) || length(self$options$predictors) == 0) {
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
                        stop(paste0("Logistic regression requires a binary outcome variable. ",
                                   "The outcome '", outcome_var, "' has ", length(outcome_unique),
                                   " unique values. Expected exactly 2 (e.g., 0/1, TRUE/FALSE)."))
                    }

                    # Force to 0/1 if logical
                    if (is.logical(mydata[[outcome_var]])) {
                        mydata[[outcome_var]] <- as.numeric(mydata[[outcome_var]])
                    }

                    if (!all(outcome_unique %in% c(0, 1))) {
                        stop(paste0("Outcome variable '", outcome_var, "' must be coded as 0/1 for logistic regression. Values found: ",
                                   paste(outcome_unique, collapse = ", ")))
                    }

                    model <- glm(formula, data = mydata, family = binomial(link = "logit"))

                } else if (model_type == "cox") {
                    # Cox proportional hazards
                    if (is.null(self$options$survivalTime) || is.null(self$options$eventStatus)) {
                        stop("Survival time and event status are required for Cox model")
                    }

                    if (!requireNamespace('survival', quietly = TRUE)) {
                        stop('The survival package is required for Cox models')
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
                        stop(paste0("Event status for Cox model must be coded as 0/1 or TRUE/FALSE. Found values: ",
                                   paste(event_unique, collapse = ", "),
                                   ". Use 0 for censored and 1 for events."))
                    }

                    # Validate survival time is positive; warn and drop non-positive
                    if (any(mydata[[time_var]] <= 0, na.rm = TRUE)) {
                        warning("Survival time contains non-positive values; these rows will be removed.")
                        mydata <- mydata[mydata[[time_var]] > 0, ]
                    }

                    # Create Surv object and formula with escaped predictors
                    predictors_safe <- sapply(predictor_vars, private$.escapeVar)
                    surv_formula <- as.formula(paste0("survival::Surv(", private$.escapeVar(time_var), ", ", private$.escapeVar(event_var), ") ~ ",
                                                     paste(predictors_safe, collapse = " + ")))
                    model <- survival::coxph(surv_formula, data = mydata)

                } else if (model_type == "mixed") {
                    # Mixed effects models
                    if (is.null(self$options$randomEffects)) {
                        stop("Random effects grouping variable is required for mixed effects models. Please specify a grouping variable (e.g., subject ID).")
                    }

                    if (!requireNamespace('lme4', quietly = TRUE)) {
                        stop('The lme4 package is required for mixed effects models')
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
                            stop("For mixed-effects logistic regression (glmer), the outcome variable must be coded as 0 and 1.")
                        }
                        model <- lme4::glmer(mixed_formula, data = mydata, family = binomial(link = "logit"))
                    } else {
                        # Continuous outcome - use lmer
                        model <- lme4::lmer(mixed_formula, data = mydata)
                    }

                } else {
                    stop(paste("Unknown or unsupported model type:", model_type))
                }

                private$.modelFit <- model

                # Extract tidy coefficients using broom
                if (inherits(model, "lmerMod")) {
                    if (!requireNamespace("broom.mixed", quietly = TRUE)) {
                        stop("The broom.mixed package is required for mixed-effects models. Please install it.")
                    }
                    tidy_coefs <- broom.mixed::tidy(model, conf.int = TRUE, conf.level = self$options$ciLevel)
                } else {
                    tidy_coefs <- broom::tidy(model, conf.int = TRUE, conf.level = self$options$ciLevel)
                }

                # Filter out intercept if requested
                if (self$options$excludeIntercept) {
                    tidy_coefs <- tidy_coefs[!grepl("intercept", tolower(tidy_coefs$term)), ]
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

                # Populate table
                private$.populateCoefTable(tidy_coefs)

                # Populate model metrics
                private$.populateModelMetrics(model)

            }, error = function(e) {
                stop(paste("Model fitting failed:", e$message))
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

            self$results$modelSummary$setContent(summary_text)
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
