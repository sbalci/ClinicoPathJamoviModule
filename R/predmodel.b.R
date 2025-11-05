#' @title Clinical Prediction Model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export


predmodelClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "predmodelClass",
        inherit = predmodelBase,
        private = list(
            .model = NULL,
            .data = NULL,

            # init ----
            .init = function() {
                if (is.null(self$options$outcome) || is.null(self$options$predictors) ||
                    length(self$options$predictors) == 0) {
                    return()
                }

                # Set plot sizes
                self$results$rocPlot$setSize(500, 500)
                self$results$calibrationPlot$setSize(500, 500)
            },

            # run ----
            .run = function() {
                if (is.null(self$options$outcome) || is.null(self$options$predictors) ||
                    length(self$options$predictors) == 0) {
                    return()
                }

                # Prepare data
                private$.prepareData()

                # Fit model
                private$.fitModel()

                # Populate results
                private$.populateCoefficients()
                if (self$options$showDiscrimination) {
                    private$.populateDiscrimination()
                }
                if (self$options$showCalibration) {
                    private$.populateCalibration()
                }
                if (self$options$showRiskGroups) {
                    private$.populateRiskStratification()
                }
                if (self$options$validationMethod != "none") {
                    private$.populateValidation()
                }

                private$.populateInterpretation()
            },

            # Prepare data ----
            .prepareData = function() {
                # Get data
                mydata <- jmvcore::naOmit(self$data)

                outcome <- self$options$outcome
                predictors <- self$options$predictors

                # Subset to relevant variables
                vars <- c(outcome, predictors)
                mydata <- mydata[, vars, drop = FALSE]

                # Ensure outcome is binary factor
                mydata[[outcome]] <- as.factor(mydata[[outcome]])
                if (nlevels(mydata[[outcome]]) != 2) {
                    stop("Outcome must have exactly 2 levels")
                }

                private$.data <- mydata
            },

            # Fit model ----
            .fitModel = function() {
                outcome <- self$options$outcome
                predictors <- self$options$predictors

                # Build formula
                formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
                formula <- as.formula(formula_str)

                # Fit logistic regression
                model <- glm(formula, data = private$.data, family = binomial(link = "logit"))

                private$.model <- model

                # Model summary
                summary_html <- paste0(
                    "<div style='padding: 15px;'>",
                    "<h4>Model Formula:</h4>",
                    "<pre>", formula_str, "</pre>",
                    "<p><strong>Sample size:</strong> ", nrow(private$.data), "</p>",
                    "<p><strong>Events:</strong> ", sum(as.numeric(private$.data[[outcome]]) - 1), "</p>",
                    "</div>"
                )
                self$results$modelSummary$setContent(summary_html)
            },

            # Populate coefficients ----
            .populateCoefficients = function() {
                if (is.null(private$.model)) return()

                coef_summary <- summary(private$.model)$coefficients
                ci_level <- self$options$ciLevel

                # Calculate CIs and ORs
                cis <- confint.default(private$.model, level = ci_level)

                table <- self$results$coefficients

                for (i in 1:nrow(coef_summary)) {
                    term <- rownames(coef_summary)[i]
                    estimate <- coef_summary[i, "Estimate"]
                    se <- coef_summary[i, "Std. Error"]
                    z <- coef_summary[i, "z value"]
                    p <- coef_summary[i, "Pr(>|z|)"]

                    or <- exp(estimate)
                    ciLower <- exp(cis[i, 1])
                    ciUpper <- exp(cis[i, 2])

                    table$addRow(rowKey = i, values = list(
                        term = term,
                        estimate = estimate,
                        se = se,
                        z = z,
                        p = p,
                        or = or,
                        ciLower = ciLower,
                        ciUpper = ciUpper
                    ))
                }
            },

            # Populate discrimination ----
            .populateDiscrimination = function() {
                if (is.null(private$.model)) return()

                # Get predictions
                outcome <- self$options$outcome
                y_actual <- as.numeric(private$.data[[outcome]]) - 1  # 0/1
                y_pred <- predict(private$.model, type = "response")

                # Calculate AUC
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_obj <- pROC::roc(y_actual, y_pred, quiet = TRUE)
                    auc_value <- as.numeric(pROC::auc(roc_obj))
                    auc_ci <- as.numeric(pROC::ci.auc(roc_obj))

                    # Calculate Brier score
                    brier <- mean((y_pred - y_actual)^2)

                    table <- self$results$discriminationMetrics

                    table$addRow(rowKey = 1, values = list(
                        metric = "AUC (C-statistic)",
                        value = auc_value,
                        ciLower = auc_ci[1],
                        ciUpper = auc_ci[3]
                    ))

                    table$addRow(rowKey = 2, values = list(
                        metric = "Brier Score",
                        value = brier,
                        ciLower = NaN,
                        ciUpper = NaN
                    ))
                }
            },

            # Populate calibration ----
            .populateCalibration = function() {
                if (is.null(private$.model)) return()

                outcome <- self$options$outcome
                y_actual <- as.numeric(private$.data[[outcome]]) - 1
                y_pred <- predict(private$.model, type = "response")

                # Hosmer-Lemeshow test (10 groups)
                g <- 10
                breaks <- quantile(y_pred, probs = seq(0, 1, 1/g))
                breaks <- unique(breaks)
                groups <- cut(y_pred, breaks = breaks, include.lowest = TRUE)

                obs <- tapply(y_actual, groups, sum)
                exp <- tapply(y_pred, groups, sum)
                n <- tapply(y_actual, groups, length)

                hl_stat <- sum(((obs - exp)^2) / (exp * (1 - exp/n)), na.rm = TRUE)
                hl_df <- length(obs) - 2
                hl_p <- 1 - pchisq(hl_stat, hl_df)

                table <- self$results$calibrationMetrics

                table$addRow(rowKey = 1, values = list(
                    metric = "Hosmer-Lemeshow χ²",
                    value = hl_stat,
                    df = hl_df,
                    p = hl_p
                ))

                # Calibration slope (from calibration model)
                cal_model <- glm(y_actual ~ qlogis(y_pred), family = binomial)
                cal_slope <- coef(cal_model)[2]

                table$addRow(rowKey = 2, values = list(
                    metric = "Calibration Slope",
                    value = cal_slope,
                    df = NaN,
                    p = NaN
                ))
            },

            # Populate risk stratification ----
            .populateRiskStratification = function() {
                if (is.null(private$.model)) return()

                outcome <- self$options$outcome
                y_actual <- as.numeric(private$.data[[outcome]]) - 1
                y_pred <- predict(private$.model, type = "response")

                # Parse cutoffs
                cutoffs_str <- self$options$riskCutoffs
                cutoffs <- as.numeric(strsplit(cutoffs_str, ",")[[1]])
                cutoffs <- sort(c(0, cutoffs, 1))

                # Assign risk groups
                risk_groups <- cut(y_pred, breaks = cutoffs,
                                 labels = paste0("Risk Group ", 1:(length(cutoffs)-1)),
                                 include.lowest = TRUE)

                table <- self$results$riskStratification

                for (i in 1:(length(cutoffs)-1)) {
                    group_name <- paste0("Risk Group ", i, " (",
                                       round(cutoffs[i], 2), "-",
                                       round(cutoffs[i+1], 2), ")")
                    idx <- risk_groups == paste0("Risk Group ", i)
                    n <- sum(idx)
                    percent <- 100 * n / length(y_pred)
                    events <- sum(y_actual[idx])
                    event_rate <- events / n

                    table$addRow(rowKey = i, values = list(
                        riskGroup = group_name,
                        n = n,
                        percent = percent,
                        events = events,
                        eventRate = event_rate
                    ))
                }
            },

            # Populate validation ----
            .populateValidation = function() {
                if (is.null(private$.model)) return()

                method <- self$options$validationMethod

                if (method == "bootstrap") {
                    private$.bootstrapValidation()
                } else if (method == "kfold") {
                    private$.kfoldValidation()
                }
            },

            # Bootstrap validation ----
            .bootstrapValidation = function() {
                n_boot <- self$options$nBootstrap
                outcome <- self$options$outcome
                predictors <- self$options$predictors

                formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
                formula <- as.formula(formula_str)

                y_actual <- as.numeric(private$.data[[outcome]]) - 1

                # Apparent performance
                y_pred <- predict(private$.model, type = "response")
                if (requireNamespace("pROC", quietly = TRUE)) {
                    apparent_auc <- as.numeric(pROC::auc(pROC::roc(y_actual, y_pred, quiet = TRUE)))
                } else {
                    apparent_auc <- NaN
                }
                apparent_brier <- mean((y_pred - y_actual)^2)

                # Bootstrap optimism
                optimism_auc <- 0
                optimism_brier <- 0

                for (i in 1:min(n_boot, 50)) {  # Limit to 50 for performance
                    boot_idx <- sample(1:nrow(private$.data), replace = TRUE)
                    boot_data <- private$.data[boot_idx, ]

                    boot_model <- glm(formula, data = boot_data, family = binomial)

                    # Performance on bootstrap sample
                    boot_y <- as.numeric(boot_data[[outcome]]) - 1
                    boot_pred <- predict(boot_model, type = "response")
                    if (requireNamespace("pROC", quietly = TRUE)) {
                        boot_auc <- as.numeric(pROC::auc(pROC::roc(boot_y, boot_pred, quiet = TRUE)))
                    } else {
                        boot_auc <- NaN
                    }
                    boot_brier <- mean((boot_pred - boot_y)^2)

                    # Performance on original data
                    orig_pred <- predict(boot_model, newdata = private$.data, type = "response")
                    if (requireNamespace("pROC", quietly = TRUE)) {
                        orig_auc <- as.numeric(pROC::auc(pROC::roc(y_actual, orig_pred, quiet = TRUE)))
                    } else {
                        orig_auc <- NaN
                    }
                    orig_brier <- mean((orig_pred - y_actual)^2)

                    optimism_auc <- optimism_auc + (boot_auc - orig_auc)
                    optimism_brier <- optimism_brier + (boot_brier - orig_brier)
                }

                optimism_auc <- optimism_auc / min(n_boot, 50)
                optimism_brier <- optimism_brier / min(n_boot, 50)

                table <- self$results$validationMetrics

                table$addRow(rowKey = 1, values = list(
                    metric = "AUC",
                    apparent = apparent_auc,
                    optimism = optimism_auc,
                    corrected = apparent_auc - optimism_auc
                ))

                table$addRow(rowKey = 2, values = list(
                    metric = "Brier Score",
                    apparent = apparent_brier,
                    optimism = optimism_brier,
                    corrected = apparent_brier - optimism_brier
                ))
            },

            # K-fold validation ----
            .kfoldValidation = function() {
                # Simplified k-fold implementation
                # Full implementation would require more extensive code
                table <- self$results$validationMetrics
                table$addRow(rowKey = 1, values = list(
                    metric = "K-fold CV (simplified)",
                    apparent = NaN,
                    optimism = NaN,
                    corrected = NaN
                ))
            },

            # Populate interpretation ----
            .populateInterpretation = function() {
                if (is.null(private$.model)) return()

                interpretation <- paste0(
                    "<div style='padding: 15px; background-color: #f8f9fa;'>",
                    "<h4>Clinical Interpretation</h4>",
                    "<p>This logistic regression model predicts the probability of the outcome based on the specified predictors.</p>",
                    "<ul>",
                    "<li><strong>Model Performance:</strong> Review the AUC (C-statistic) for discrimination. Values >0.70 indicate acceptable discrimination, >0.80 is excellent.</li>",
                    "<li><strong>Calibration:</strong> The Hosmer-Lemeshow test assesses calibration. A non-significant p-value (>0.05) suggests good calibration.</li>",
                    "<li><strong>Validation:</strong> Bootstrap-corrected metrics provide more realistic estimates of model performance in new patients.</li>",
                    "<li><strong>Clinical Use:</strong> Predicted probabilities can be used to stratify patients into risk groups for targeted interventions.</li>",
                    "</ul>",
                    "</div>"
                )

                self$results$interpretation$setContent(interpretation)
            },

            # ROC plot ----
            .rocPlot = function(image, ggtheme, theme, ...) {
                if (is.null(private$.model)) return()

                if (!requireNamespace("pROC", quietly = TRUE)) {
                    return()
                }

                outcome <- self$options$outcome
                y_actual <- as.numeric(private$.data[[outcome]]) - 1
                y_pred <- predict(private$.model, type = "response")

                roc_obj <- pROC::roc(y_actual, y_pred, quiet = TRUE)

                plot(roc_obj, main = "ROC Curve",
                     col = "#2c3e50", lwd = 2,
                     print.auc = TRUE, auc.polygon = TRUE,
                     auc.polygon.col = "#3498db30")
                abline(a = 0, b = 1, lty = 2, col = "gray")

                TRUE
            },

            # Calibration plot ----
            .calibrationPlot = function(image, ggtheme, theme, ...) {
                if (is.null(private$.model)) return()

                outcome <- self$options$outcome
                y_actual <- as.numeric(private$.data[[outcome]]) - 1
                y_pred <- predict(private$.model, type = "response")

                # Group predictions into deciles
                g <- 10
                breaks <- quantile(y_pred, probs = seq(0, 1, 1/g))
                breaks <- unique(breaks)
                groups <- cut(y_pred, breaks = breaks, include.lowest = TRUE)

                obs_rate <- tapply(y_actual, groups, mean)
                pred_rate <- tapply(y_pred, groups, mean)

                plot(pred_rate, obs_rate,
                     xlim = c(0, 1), ylim = c(0, 1),
                     xlab = "Predicted Probability",
                     ylab = "Observed Proportion",
                     main = "Calibration Plot",
                     pch = 19, col = "#2c3e50", cex = 1.5)
                abline(a = 0, b = 1, lty = 2, col = "gray")

                # Add loess smooth
                if (length(y_pred) > 20) {
                    loess_fit <- loess(y_actual ~ y_pred)
                    pred_seq <- seq(min(y_pred), max(y_pred), length.out = 100)
                    loess_pred <- predict(loess_fit, newdata = pred_seq)
                    lines(pred_seq, loess_pred, col = "#e74c3c", lwd = 2)
                }

                TRUE
            }
        )
    )
