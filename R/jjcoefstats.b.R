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
    if (self$options$showexplanations) {
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

    self$results$summary$setContent(summary_text)
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

    # Filter out intercept if requested
    if (self$options$excludeIntercept) {
        tidy_coefs <- tidy_coefs[!grepl("intercept", tolower(tidy_coefs$term)), ]
    }

    # Exponentiate if requested
    if (self$options$exponentiate) {
        # Use delta method to transform std.error: SE_exp = SE * exp(estimate)
        tidy_coefs$std.error <- tidy_coefs$std.error * exp(tidy_coefs$estimate)

        # Exponentiate estimates and CI bounds
        tidy_coefs$estimate <- exp(tidy_coefs$estimate)
        tidy_coefs$conf.low <- exp(tidy_coefs$conf.low)
        tidy_coefs$conf.high <- exp(tidy_coefs$conf.high)

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
},
        .runFitModel = function() {
            # Check required variables
            if (is.null(self$options$outcome) || is.null(self$options$predictors) ||
                length(self$options$predictors) == 0) {
                return()
            }

            # Get data
            mydata <- self$data %>% janitor::clean_names()

            # Get variable names
            outcome_var <- janitor::make_clean_names(self$options$outcome)
            predictor_vars <- janitor::make_clean_names(self$options$predictors)

            # Create formula
            formula_str <- paste(outcome_var, "~", paste(predictor_vars, collapse = " + "))
            formula <- as.formula(formula_str)

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

                    if (!all(outcome_unique %in% c(0, 1, TRUE, FALSE))) {
                        warning(paste0("Outcome variable '", outcome_var, "' will be treated as binary ",
                                      "but values are not standard (0/1 or TRUE/FALSE). Values found: ",
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

                    if (!all(event_unique %in% c(0, 1, TRUE, FALSE))) {
                        stop(paste0("Event status for Cox model must be coded as 0/1 or TRUE/FALSE. ",
                                   "Found values: ", paste(event_unique, collapse = ", "),
                                   ". Use 0 for censored and 1 for events."))
                    }

                    # Validate survival time is positive
                    if (any(mydata[[time_var]] <= 0, na.rm = TRUE)) {
                        warning("Survival time contains non-positive values. These will cause errors in Cox model.")
                    }

                    # Create Surv object and formula
                    surv_formula <- as.formula(paste0("survival::Surv(", time_var, ", ", event_var, ") ~ ",
                                                     paste(predictor_vars, collapse = " + ")))
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
                    mixed_formula <- as.formula(paste0(outcome_var, " ~ ",
                                                      paste(predictor_vars, collapse = " + "),
                                                      " + (1|", random_var, ")"))

                    # Determine whether to use lmer (continuous) or glmer (binary)
                    # Check if outcome is binary
                    outcome_unique <- unique(mydata[[outcome_var]])
                    outcome_unique <- outcome_unique[!is.na(outcome_unique)]

                    if (length(outcome_unique) == 2 && all(outcome_unique %in% c(0, 1))) {
                        # Binary outcome - use glmer
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
                tidy_coefs <- broom::tidy(model, conf.int = TRUE, conf.level = self$options$ciLevel)

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

            for (i in 1:nrow(tidy_coefs)) {
                table$addRow(rowKey = i, values = list(
                    term = tidy_coefs$term[i],
                    estimate = round(tidy_coefs$estimate[i], 3),
                    std_error = round(tidy_coefs$std.error[i], 3),
                    statistic = if (!is.null(tidy_coefs$statistic)) round(tidy_coefs$statistic[i], 3) else NA,
                    p_value = if (!is.null(tidy_coefs$p.value)) tidy_coefs$p.value[i] else NA,
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

        .coefPlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.tidyCoefs)) return()

            tidy_coefs <- private$.tidyCoefs

            # Determine reference value
            ref_val <- self$options$referenceValue
            if (self$options$exponentiate && ref_val == 0) {
                ref_val <- 1  # For ratios, reference is 1
            }

            # Determine axis labels
            if (self$options$exponentiate) {
                if (self$options$modelType == "glm") {
                    xlab <- "Odds Ratio"
                } else if (self$options$modelType == "cox") {
                    xlab <- "Hazard Ratio"
                } else {
                    xlab <- "Exponentiated Coefficient"
                }
            } else {
                xlab <- "Coefficient Estimate"
            }

            # Create basic data frame for ggcoefstats
            plot_data <- data.frame(
                term = tidy_coefs$term,
                estimate = tidy_coefs$estimate,
                conf.low = tidy_coefs$conf.low,
                conf.high = tidy_coefs$conf.high,
                p.value = if (!is.null(tidy_coefs$p.value)) tidy_coefs$p.value else NA,
                stringsAsFactors = FALSE
            )

            tryCatch({
                # Determine if we should use a model object or tidy data
                if (!is.null(private$.modelFit)) {
                    # Use model object directly
                    model_obj <- private$.modelFit
                    use_model <- TRUE
                } else {
                    # Use tidy data frame
                    use_model <- FALSE
                }

                # Create plot using ggcoefstats
                if (use_model) {
                    plot <- ggstatsplot::ggcoefstats(
                        x = private$.modelFit,
                        conf.level = self$options$ciLevel,
                        exclude.intercept = self$options$excludeIntercept,
                        sort = if (self$options$sortCoefs) "ascending" else "none",
                        stats.label.color = c("#0072B2", "#D55E00"),
                        vline.args = list(
                            xintercept = ref_val,
                            linetype = "dashed",
                            color = "red"
                        ),
                        ggtheme = ggtheme
                    ) +
                    ggplot2::labs(x = xlab)
                } else {
                    # Manual plotting when using pre-computed data
                    # Create a simple ggplot since ggcoefstats needs a model object
                    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = estimate, y = term)) +
                        ggplot2::geom_point(size = 3, color = "#0072B2") +
                        ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high),
                                               height = 0.2, color = "#0072B2") +
                        ggplot2::geom_vline(xintercept = ref_val, linetype = "dashed", color = "red") +
                        ggplot2::labs(
                            x = xlab,
                            y = "Term",
                            title = "Coefficient Forest Plot"
                        ) +
                        ggtheme

                    # Add p-values if requested
                    if (self$options$showPValues && !is.null(plot_data$p.value)) {
                        if (self$options$pSymbols) {
                            # Use symbols
                            plot_data$sig_symbol <- ifelse(plot_data$p.value < 0.001, "***",
                                                          ifelse(plot_data$p.value < 0.01, "**",
                                                                ifelse(plot_data$p.value < 0.05, "*", "ns")))
                            plot <- plot +
                                ggplot2::geom_text(data = plot_data,
                                                  ggplot2::aes(label = sig_symbol, x = conf.high),
                                                  hjust = -0.5, size = 4)
                        } else {
                            # Use numeric p-values
                            plot_data$p_label <- sprintf("p=%.3f", plot_data$p.value)
                            plot <- plot +
                                ggplot2::geom_text(data = plot_data,
                                                  ggplot2::aes(label = p_label, x = conf.high),
                                                  hjust = -0.1, size = 3)
                        }
                    }
                }

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
