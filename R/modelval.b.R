#' @title Model Validation Dashboard
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export


modelvalClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "modelvalClass",
        inherit = modelvalBase,
        private = list(
            .data = NULL,
            .outcome_binary = NULL,
            .predicted = NULL,

            # init ----
            .init = function() {
                if (is.null(self$options$outcome) || is.null(self$options$predicted)) {
                    return()
                }

                # Set plot sizes
                self$results$calibrationPlot$setSize(600, 600)
                self$results$rocPlot$setSize(500, 500)
                self$results$dcaPlot$setSize(600, 500)
                self$results$subgroupCalibrationPlot$setSize(700, 600)
            },

            # run ----
            .run = function() {
                if (is.null(self$options$outcome) || is.null(self$options$predicted)) {
                    return()
                }

                # Prepare data
                private$.prepareData()

                # Populate summary
                private$.populateSummary()

                # Calibration analysis
                private$.populateCalibration()

                # Discrimination
                if (self$options$showDiscrimination) {
                    private$.populateDiscrimination()
                }

                # Net benefit
                if (self$options$showNetBenefit) {
                    private$.populateNetBenefit()
                }

                # Subgroup analysis
                if (self$options$showSubgroupAnalysis && !is.null(self$options$subgroup)) {
                    private$.populateSubgroupAnalysis()
                }

                # Recommendations
                private$.populateRecommendations()
            },

            # Prepare data ----
            .prepareData = function() {
                mydata <- jmvcore::naOmit(self$data)

                outcome <- self$options$outcome
                predicted <- self$options$predicted

                # Get variables
                vars <- c(outcome, predicted)
                if (!is.null(self$options$subgroup)) {
                    vars <- c(vars, self$options$subgroup)
                }

                mydata <- mydata[, vars, drop = FALSE]

                # Convert outcome to binary 0/1
                mydata[[outcome]] <- as.factor(mydata[[outcome]])
                if (nlevels(mydata[[outcome]]) != 2) {
                    stop("Outcome must have exactly 2 levels")
                }

                outcome_binary <- as.numeric(mydata[[outcome]]) - 1

                # Validate predicted probabilities
                pred_values <- mydata[[predicted]]
                if (any(pred_values < 0 | pred_values > 1, na.rm = TRUE)) {
                    stop("Predicted probabilities must be between 0 and 1")
                }

                private$.data <- mydata
                private$.outcome_binary <- outcome_binary
                private$.predicted <- pred_values
            },

            # Populate summary ----
            .populateSummary = function() {
                validation_type <- switch(self$options$validationType,
                    "external" = "External Validation",
                    "temporal" = "Temporal Validation",
                    "geographic" = "Geographic Validation",
                    "general" = "General Model Validation"
                )

                n <- length(private$.outcome_binary)
                n_events <- sum(private$.outcome_binary)
                event_rate <- n_events / n

                summary_html <- paste0(
                    "<div style='padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>",
                    "<h4>", validation_type, "</h4>",
                    "<table style='width: 100%; margin-top: 10px;'>",
                    "<tr><td><strong>Sample size:</strong></td><td>", n, "</td></tr>",
                    "<tr><td><strong>Events:</strong></td><td>", n_events, " (", round(100*event_rate, 1), "%)</td></tr>",
                    "<tr><td><strong>Model:</strong></td><td>External prediction model</td></tr>",
                    "</table>",
                    "</div>"
                )

                self$results$validationSummary$setContent(summary_html)
            },

            # Populate calibration ----
            .populateCalibration = function() {
                y <- private$.outcome_binary
                p <- private$.predicted

                ci_level <- self$options$ciLevel
                z <- qnorm(1 - (1 - ci_level) / 2)

                table <- self$results$calibrationMetrics

                # Calibration-in-the-large
                if (self$options$showCalibrationLarge) {
                    mean_pred <- mean(p)
                    mean_obs <- mean(y)
                    cal_large <- mean_obs - mean_pred

                    se_cal_large <- sqrt(mean_obs * (1 - mean_obs) / length(y))

                    interpretation <- if (abs(cal_large) < 0.05) {
                        "Good (near 0)"
                    } else if (cal_large > 0) {
                        "Model underestimates risk"
                    } else {
                        "Model overestimates risk"
                    }

                    table$addRow(rowKey = 1, values = list(
                        metric = "Calibration-in-the-Large",
                        estimate = cal_large,
                        se = se_cal_large,
                        ciLower = cal_large - z * se_cal_large,
                        ciUpper = cal_large + z * se_cal_large,
                        interpretation = interpretation
                    ))
                }

                # Calibration slope
                if (self$options$showCalibrationSlope) {
                    # Fit logistic calibration model
                    logit_p <- log(p / (1 - p))
                    cal_model <- glm(y ~ logit_p, family = binomial)
                    cal_slope <- coef(cal_model)[2]
                    se_slope <- summary(cal_model)$coefficients[2, 2]

                    interpretation <- if (abs(cal_slope - 1) < 0.1) {
                        "Good (near 1.0)"
                    } else if (cal_slope < 1) {
                        "Model predictions too extreme"
                    } else {
                        "Model predictions too moderate"
                    }

                    table$addRow(rowKey = 2, values = list(
                        metric = "Calibration Slope",
                        estimate = cal_slope,
                        se = se_slope,
                        ciLower = cal_slope - z * se_slope,
                        ciUpper = cal_slope + z * se_slope,
                        interpretation = interpretation
                    ))

                    # Calibration intercept
                    cal_intercept <- coef(cal_model)[1]
                    se_intercept <- summary(cal_model)$coefficients[1, 2]

                    table$addRow(rowKey = 3, values = list(
                        metric = "Calibration Intercept",
                        estimate = cal_intercept,
                        se = se_intercept,
                        ciLower = cal_intercept - z * se_intercept,
                        ciUpper = cal_intercept + z * se_intercept,
                        interpretation = "Should be near 0"
                    ))
                }
            },

            # Populate discrimination ----
            .populateDiscrimination = function() {
                if (!requireNamespace("pROC", quietly = TRUE)) {
                    return()
                }

                y <- private$.outcome_binary
                p <- private$.predicted

                # AUC
                roc_obj <- pROC::roc(y, p, quiet = TRUE)
                auc_value <- as.numeric(pROC::auc(roc_obj))
                auc_ci <- as.numeric(pROC::ci.auc(roc_obj, conf.level = self$options$ciLevel))

                # Brier score
                brier <- mean((p - y)^2)

                # Scaled Brier score
                brier_max <- mean(y) * (1 - mean(y))
                scaled_brier <- 1 - (brier / brier_max)

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

                table$addRow(rowKey = 3, values = list(
                    metric = "Scaled Brier Score",
                    value = scaled_brier,
                    ciLower = NaN,
                    ciUpper = NaN
                ))
            },

            # Populate net benefit ----
            .populateNetBenefit = function() {
                y <- private$.outcome_binary
                p <- private$.predicted

                # Parse threshold range
                threshold_str <- self$options$thresholdRange
                threshold_range <- as.numeric(strsplit(threshold_str, ",")[[1]])
                thresholds <- seq(threshold_range[1], threshold_range[2], by = 0.05)

                # Calculate net benefit at key thresholds
                key_thresholds <- c(0.05, 0.10, 0.20, 0.30, 0.50)
                table <- self$results$netBenefitSummary

                for (i in seq_along(key_thresholds)) {
                    pt <- key_thresholds[i]

                    # Net benefit of model
                    tp <- sum(y == 1 & p >= pt)
                    fp <- sum(y == 0 & p >= pt)
                    n <- length(y)

                    nb_model <- (tp / n) - (fp / n) * (pt / (1 - pt))

                    # Net benefit of treat all
                    nb_all <- (sum(y) / n) - ((n - sum(y)) / n) * (pt / (1 - pt))

                    # Net benefit of treat none
                    nb_none <- 0

                    table$addRow(rowKey = i, values = list(
                        threshold = pt,
                        netBenefit = nb_model,
                        treatAll = nb_all,
                        treatNone = nb_none
                    ))
                }
            },

            # Populate subgroup analysis ----
            .populateSubgroupAnalysis = function() {
                outcome <- self$options$outcome
                predicted <- self$options$predicted
                subgroup <- self$options$subgroup

                subgroup_levels <- levels(as.factor(private$.data[[subgroup]]))

                table <- self$results$subgroupPerformance

                for (i in seq_along(subgroup_levels)) {
                    level <- subgroup_levels[i]
                    idx <- private$.data[[subgroup]] == level

                    y_sub <- private$.outcome_binary[idx]
                    p_sub <- private$.predicted[idx]

                    n_sub <- sum(idx)
                    events_sub <- sum(y_sub)

                    # AUC
                    if (requireNamespace("pROC", quietly = TRUE)) {
                        auc_sub <- as.numeric(pROC::auc(pROC::roc(y_sub, p_sub, quiet = TRUE)))
                    } else {
                        auc_sub <- NaN
                    }

                    # Brier
                    brier_sub <- mean((p_sub - y_sub)^2)

                    # Calibration slope
                    logit_p <- log(p_sub / (1 - p_sub))
                    cal_model <- glm(y_sub ~ logit_p, family = binomial)
                    cal_slope_sub <- coef(cal_model)[2]

                    table$addRow(rowKey = i, values = list(
                        subgroup = level,
                        n = n_sub,
                        events = events_sub,
                        auc = auc_sub,
                        brier = brier_sub,
                        calSlope = cal_slope_sub
                    ))
                }
            },

            # Populate recommendations ----
            .populateRecommendations = function() {
                # Get calibration slope from results
                cal_table <- self$results$calibrationMetrics
                cal_slope <- NULL
                if (cal_table$rowCount > 0) {
                    for (i in 1:cal_table$rowCount) {
                        row <- cal_table$get(rowNo = i)
                        if (!is.null(row$metric) && row$metric == "Calibration Slope") {
                            cal_slope <- row$estimate
                            break
                        }
                    }
                }

                recommendations <- paste0(
                    "<div style='padding: 15px; background-color: #e8f5e9; border-left: 4px solid #4caf50;'>",
                    "<h4>Evidence-Based Recommendations</h4>",
                    "<ol style='line-height: 1.8;'>"
                )

                # Recommendation based on calibration slope
                if (!is.null(cal_slope)) {
                    if (abs(cal_slope - 1) < 0.1) {
                        recommendations <- paste0(recommendations,
                            "<li><strong>Calibration:</strong> Good calibration slope (", round(cal_slope, 2), "). ",
                            "Model predictions are well-calibrated and can be used directly.</li>")
                    } else if (cal_slope < 1) {
                        recommendations <- paste0(recommendations,
                            "<li><strong>Calibration:</strong> Calibration slope < 1 (", round(cal_slope, 2), "). ",
                            "Consider recalibration by fitting a logistic model with offset or apply formula: ",
                            "<code>recalibrated = 1 / (1 + exp(-intercept - slope * logit(predicted)))</code></li>")
                    } else {
                        recommendations <- paste0(recommendations,
                            "<li><strong>Calibration:</strong> Calibration slope > 1 (", round(cal_slope, 2), "). ",
                            "Model may need recalibration or updating with local data.</li>")
                    }
                }

                recommendations <- paste0(recommendations,
                    "<li><strong>Clinical Use:</strong> ",
                    "If AUC > 0.70 and calibration is acceptable, model may be suitable for risk stratification.</li>",
                    "<li><strong>Next Steps:</strong> ",
                    "Consider impact analysis (decision curves) and implementation feasibility.</li>",
                    "<li><strong>Reporting:</strong> ",
                    "Report all validation metrics and consider TRIPOD guidelines for transparent reporting.</li>",
                    "</ol>",
                    "</div>"
                )

                self$results$recommendations$setContent(recommendations)
            },

            # Calibration plot ----
            .calibrationPlot = function(image, ggtheme, theme, ...) {
                y <- private$.outcome_binary
                p <- private$.predicted

                n_groups <- self$options$calibrationGroups

                # Group predictions
                breaks <- quantile(p, probs = seq(0, 1, 1/n_groups))
                breaks <- unique(breaks)
                groups <- cut(p, breaks = breaks, include.lowest = TRUE)

                obs_rate <- tapply(y, groups, mean)
                pred_rate <- tapply(p, groups, mean)

                # Main plot
                plot(pred_rate, obs_rate,
                     xlim = c(0, 1), ylim = c(0, 1),
                     xlab = "Predicted Probability",
                     ylab = "Observed Proportion",
                     main = "Calibration Plot",
                     pch = 19, col = "#2c3e50", cex = 1.5)

                # Perfect calibration line
                abline(a = 0, b = 1, lty = 2, col = "gray", lwd = 2)

                # Flexible calibration curve (loess)
                if (length(p) > 20 && self$options$showFlexibleCalibration) {
                    loess_fit <- loess(y ~ p, span = 0.75)
                    pred_seq <- seq(min(p), max(p), length.out = 100)
                    loess_pred <- predict(loess_fit, newdata = pred_seq)
                    lines(pred_seq, loess_pred, col = "#e74c3c", lwd = 2.5)
                }

                # Add legend
                legend("topleft",
                       legend = c("Perfect calibration", "Observed", "Flexible curve"),
                       col = c("gray", "#2c3e50", "#e74c3c"),
                       lty = c(2, NA, 1),
                       pch = c(NA, 19, NA),
                       lwd = c(2, NA, 2.5),
                       bty = "n")

                TRUE
            },

            # ROC plot ----
            .rocPlot = function(image, ggtheme, theme, ...) {
                if (!requireNamespace("pROC", quietly = TRUE)) {
                    return()
                }

                y <- private$.outcome_binary
                p <- private$.predicted

                roc_obj <- pROC::roc(y, p, quiet = TRUE)

                plot(roc_obj, main = "ROC Curve",
                     col = "#2c3e50", lwd = 2,
                     print.auc = TRUE, auc.polygon = TRUE,
                     auc.polygon.col = "#3498db30")
                abline(a = 0, b = 1, lty = 2, col = "gray")

                TRUE
            },

            # DCA plot ----
            .dcaPlot = function(image, ggtheme, theme, ...) {
                y <- private$.outcome_binary
                p <- private$.predicted

                # Parse threshold range
                threshold_str <- self$options$thresholdRange
                threshold_range <- as.numeric(strsplit(threshold_str, ",")[[1]])
                thresholds <- seq(threshold_range[1], threshold_range[2], by = 0.01)

                # Calculate net benefit across thresholds
                nb_model <- numeric(length(thresholds))
                nb_all <- numeric(length(thresholds))

                for (i in seq_along(thresholds)) {
                    pt <- thresholds[i]

                    # Model
                    tp <- sum(y == 1 & p >= pt)
                    fp <- sum(y == 0 & p >= pt)
                    n <- length(y)
                    nb_model[i] <- (tp / n) - (fp / n) * (pt / (1 - pt))

                    # Treat all
                    nb_all[i] <- (sum(y) / n) - ((n - sum(y)) / n) * (pt / (1 - pt))
                }

                # Plot
                plot(thresholds, nb_model, type = "l", lwd = 2, col = "#2c3e50",
                     xlab = "Threshold Probability", ylab = "Net Benefit",
                     main = "Decision Curve Analysis",
                     ylim = range(c(nb_model, nb_all, 0), na.rm = TRUE))
                lines(thresholds, nb_all, lty = 2, lwd = 2, col = "#e74c3c")
                abline(h = 0, lty = 3, col = "gray")

                legend("topright",
                       legend = c("Model", "Treat all", "Treat none"),
                       col = c("#2c3e50", "#e74c3c", "gray"),
                       lty = c(1, 2, 3),
                       lwd = 2,
                       bty = "n")

                TRUE
            },

            # Subgroup calibration plot ----
            .subgroupCalibrationPlot = function(image, ggtheme, theme, ...) {
                subgroup <- self$options$subgroup
                if (is.null(subgroup)) return()

                subgroup_levels <- levels(as.factor(private$.data[[subgroup]]))
                n_subgroups <- length(subgroup_levels)

                # Set up multi-panel plot
                par(mfrow = c(ceiling(n_subgroups/2), 2))

                for (level in subgroup_levels) {
                    idx <- private$.data[[subgroup]] == level
                    y_sub <- private$.outcome_binary[idx]
                    p_sub <- private$.predicted[idx]

                    # Group predictions
                    breaks <- quantile(p_sub, probs = seq(0, 1, 0.1))
                    breaks <- unique(breaks)
                    groups <- cut(p_sub, breaks = breaks, include.lowest = TRUE)

                    obs_rate <- tapply(y_sub, groups, mean)
                    pred_rate <- tapply(p_sub, groups, mean)

                    plot(pred_rate, obs_rate,
                         xlim = c(0, 1), ylim = c(0, 1),
                         xlab = "Predicted", ylab = "Observed",
                         main = paste("Subgroup:", level),
                         pch = 19, col = "#2c3e50")
                    abline(a = 0, b = 1, lty = 2, col = "gray")

                    if (length(p_sub) > 10) {
                        loess_fit <- loess(y_sub ~ p_sub)
                        pred_seq <- seq(min(p_sub), max(p_sub), length.out = 50)
                        loess_pred <- predict(loess_fit, newdata = pred_seq)
                        lines(pred_seq, loess_pred, col = "#e74c3c", lwd = 2)
                    }
                }

                TRUE
            }
        )
    )
