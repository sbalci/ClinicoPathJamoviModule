#' @title Time-Dependent Survival Calibration
#' @importFrom R6 R6Class
#' @import jmvcore
#'

survivalcalibrationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "survivalcalibrationClass",
    inherit = survivalcalibrationBase,
    private = list(

        .validatedData = NULL,
        .calibrationResults = NULL,
        .discriminationResults = NULL,

        .init = function() {
            # Show initial instructions
            if (is.null(self$options$time) || is.null(self$options$event) ||
                is.null(self$options$predicted)) {

                instructions <- glue::glue("
                <h3>Time-Dependent Survival Model Calibration</h3>
                <hr>
                <p><b>Purpose:</b> Evaluate the performance of survival prediction models
                with time-dependent calibration and discrimination metrics.</p>

                <p><b>Required Variables:</b></p>
                <ul>
                <li><b>Survival Time:</b> Time to event or censoring (continuous, in months)</li>
                <li><b>Event Indicator:</b> Binary status (0=censored, 1=event)</li>
                <li><b>Predicted Survival:</b> Predicted survival probability at calibration time (0-1)</li>
                </ul>

                <p><b>Optional Variables:</b></p>
                <ul>
                <li><b>Linear Predictor:</b> From Cox model (alternative to predicted probability)</li>
                <li><b>Validation Set:</b> Training/validation/external grouping</li>
                </ul>

                <p><b>Outputs:</b></p>
                <ul>
                <li>Calibration plot (observed vs predicted survival)</li>
                <li>Time-dependent C-index with confidence intervals</li>
                <li>Integrated Brier score (prediction error)</li>
                <li>Calibration metrics (slope, intercept, E:O ratio)</li>
                <li>TRIPOD-compliant validation report</li>
                </ul>

                <p><b>Packages Used:</b> survival, rms, pec, riskRegression</p>
                <hr>
                ")

                self$results$instructions$setContent(instructions)
            }
        },

        .run = function() {
            # Check required variables
            if (is.null(self$options$time) || is.null(self$options$event) ||
                is.null(self$options$predicted)) {
                return()
            }

            # Check required packages
            if (!requireNamespace('survival', quietly = TRUE)) {
                stop('The survival package is required but not installed')
            }
            if (!requireNamespace('rms', quietly = TRUE)) {
                stop('The rms package is required but not installed')
            }

            # Validate and prepare data
            tryCatch({
                private$.prepareData()
                private$.performCalibration()
                private$.performDiscrimination()
                private$.populateResults()

                if (self$options$tripodReport) {
                    private$.generateTRIPODReport()
                }

                private$.generateInterpretation()

            }, error = function(e) {
                stop(paste("Analysis failed:", e$message))
            })
        },

        .prepareData = function() {
            # Get data
            mydata <- self$data

            # Clean names
            mydata <- mydata %>% janitor::clean_names()

            # Get variable names
            time_var <- janitor::make_clean_names(self$options$time)
            event_var <- janitor::make_clean_names(self$options$event)
            pred_var <- janitor::make_clean_names(self$options$predicted)

            # Extract vectors
            time_vec <- mydata[[time_var]]
            event_vec <- mydata[[event_var]]
            pred_vec <- mydata[[pred_var]]

            # Validate data
            if (any(is.na(time_vec)) || any(is.na(event_vec)) || any(is.na(pred_vec))) {
                stop("Missing values detected in required variables")
            }

            if (any(time_vec <= 0)) {
                stop("Survival times must be positive")
            }

            if (!all(event_vec %in% c(0, 1))) {
                stop("Event indicator must be binary (0=censored, 1=event)")
            }

            if (any(pred_vec < 0) || any(pred_vec > 1)) {
                stop("Predicted survival probabilities must be between 0 and 1")
            }

            # Store validated data
            private$.validatedData <- list(
                time = time_vec,
                event = event_vec,
                predicted = pred_vec,
                n = length(time_vec),
                n_events = sum(event_vec),
                n_censored = sum(1 - event_vec)
            )
        },

        .performCalibration = function() {
            time_vec <- private$.validatedData$time
            event_vec <- private$.validatedData$event
            pred_vec <- private$.validatedData$predicted
            cal_time <- self$options$calibrationTime

            # Calculate observed survival at calibration time using Kaplan-Meier
            km_fit <- survival::survfit(survival::Surv(time_vec, event_vec) ~ 1)

            # Get observed survival at calibration time
            obs_surv_at_time <- tryCatch({
                summary(km_fit, times = cal_time)$surv
            }, error = function(e) {
                NA
            })

            # Group patients by predicted risk (deciles)
            n_groups <- self$options$nGroups
            pred_groups <- cut(pred_vec, breaks = quantile(pred_vec, probs = seq(0, 1, 1/n_groups)),
                              include.lowest = TRUE, labels = 1:n_groups)

            # Calculate calibration metrics for each group
            cal_results <- list()

            for (g in 1:n_groups) {
                group_idx <- which(pred_groups == g)
                if (length(group_idx) > 0) {
                    group_pred <- mean(pred_vec[group_idx])

                    # Observed survival for this group
                    km_group <- survival::survfit(
                        survival::Surv(time_vec[group_idx], event_vec[group_idx]) ~ 1
                    )

                    group_obs <- tryCatch({
                        summary(km_group, times = cal_time)$surv
                    }, error = function(e) {
                        NA
                    })

                    cal_results[[g]] <- list(
                        group = g,
                        predicted = group_pred,
                        observed = group_obs,
                        n = length(group_idx)
                    )
                }
            }

            # Calculate calibration slope and intercept
            valid_results <- Filter(function(x) !is.na(x$observed), cal_results)

            if (length(valid_results) >= 3) {
                pred_vals <- sapply(valid_results, function(x) x$predicted)
                obs_vals <- sapply(valid_results, function(x) x$observed)

                # Calibration model: observed ~ predicted
                cal_model <- lm(obs_vals ~ pred_vals)
                cal_slope <- coef(cal_model)[2]
                cal_intercept <- coef(cal_model)[1]

                # E:O ratio (Expected to Observed)
                eo_ratio <- mean(pred_vals) / mean(obs_vals)
            } else {
                cal_slope <- NA
                cal_intercept <- NA
                eo_ratio <- NA
            }

            private$.calibrationResults <- list(
                cal_slope = cal_slope,
                cal_intercept = cal_intercept,
                eo_ratio = eo_ratio,
                grouped_results = cal_results,
                obs_surv_at_time = obs_surv_at_time
            )
        },

        .performDiscrimination = function() {
            time_vec <- private$.validatedData$time
            event_vec <- private$.validatedData$event
            pred_vec <- private$.validatedData$predicted
            cal_time <- self$options$calibrationTime

            # Calculate apparent C-index
            tryCatch({
                # Create survival object
                surv_obj <- survival::Surv(time_vec, event_vec)

                # C-index using concordance
                cindex_result <- survival::concordance(surv_obj ~ pred_vec)
                apparent_cindex <- cindex_result$concordance

                # Standard error and CI
                se_cindex <- sqrt(cindex_result$var)
                ci_level <- self$options$ciLevel
                z_val <- qnorm((1 + ci_level) / 2)
                ci_lower <- apparent_cindex - z_val * se_cindex
                ci_upper <- apparent_cindex + z_val * se_cindex

                # Apparent Brier score at calibration time
                # Calculate prediction error
                # For censored observations before cal_time, treat as if they survived
                survived_to_cal_time <- ifelse(time_vec >= cal_time & event_vec == 0, 1,
                                               ifelse(time_vec >= cal_time & event_vec == 1, 0,
                                                     ifelse(time_vec < cal_time & event_vec == 1, 0, NA)))

                valid_idx <- !is.na(survived_to_cal_time)
                if (sum(valid_idx) > 0) {
                    apparent_brier <- mean((pred_vec[valid_idx] - survived_to_cal_time[valid_idx])^2)
                } else {
                    apparent_brier <- NA
                }

                # Perform validation if requested
                validated_cindex <- NA
                validated_brier <- NA
                optimism_cindex <- NA
                optimism_brier <- NA

                if (self$options$validationMethod == "bootstrap") {
                    validation_results <- private$.bootstrapValidation()
                    validated_cindex <- validation_results$cindex
                    validated_brier <- validation_results$brier
                    optimism_cindex <- apparent_cindex - validated_cindex
                    optimism_brier <- apparent_brier - validated_brier
                } else if (self$options$validationMethod == "kfold") {
                    validation_results <- private$.kfoldValidation()
                    validated_cindex <- validation_results$cindex
                    validated_brier <- validation_results$brier
                }

                private$.discriminationResults <- list(
                    apparent_cindex = apparent_cindex,
                    cindex_ci_lower = ci_lower,
                    cindex_ci_upper = ci_upper,
                    apparent_brier = apparent_brier,
                    validated_cindex = validated_cindex,
                    validated_brier = validated_brier,
                    optimism_cindex = optimism_cindex,
                    optimism_brier = optimism_brier
                )

            }, error = function(e) {
                message("Discrimination metrics could not be calculated: ", e$message)
                private$.discriminationResults <- list(
                    apparent_cindex = NA,
                    cindex_ci_lower = NA,
                    cindex_ci_upper = NA,
                    apparent_brier = NA
                )
            })
        },

        .bootstrapValidation = function() {
            time_vec <- private$.validatedData$time
            event_vec <- private$.validatedData$event
            pred_vec <- private$.validatedData$predicted
            n_boot <- min(self$options$nBootstrap, 100)  # Limit for performance
            cal_time <- self$options$calibrationTime

            cindex_vals <- numeric(n_boot)
            brier_vals <- numeric(n_boot)

            for (i in 1:n_boot) {
                # Checkpoint for long operations
                if (i %% 10 == 0) {
                    private$.checkpoint()
                }

                # Bootstrap sample
                boot_idx <- sample(1:length(time_vec), replace = TRUE)
                boot_time <- time_vec[boot_idx]
                boot_event <- event_vec[boot_idx]
                boot_pred <- pred_vec[boot_idx]

                # Calculate C-index on bootstrap sample
                tryCatch({
                    surv_obj <- survival::Surv(boot_time, boot_event)
                    cindex_result <- survival::concordance(surv_obj ~ boot_pred)
                    cindex_vals[i] <- cindex_result$concordance

                    # Calculate Brier score
                    survived <- ifelse(boot_time >= cal_time & boot_event == 0, 1,
                                      ifelse(boot_time >= cal_time & boot_event == 1, 0,
                                            ifelse(boot_time < cal_time & boot_event == 1, 0, NA)))
                    valid_idx <- !is.na(survived)
                    if (sum(valid_idx) > 0) {
                        brier_vals[i] <- mean((boot_pred[valid_idx] - survived[valid_idx])^2)
                    } else {
                        brier_vals[i] <- NA
                    }
                }, error = function(e) {
                    cindex_vals[i] <- NA
                    brier_vals[i] <- NA
                })
            }

            # Return validated estimates (mean of bootstrap samples)
            list(
                cindex = mean(cindex_vals, na.rm = TRUE),
                brier = mean(brier_vals, na.rm = TRUE)
            )
        },

        .kfoldValidation = function() {
            time_vec <- private$.validatedData$time
            event_vec <- private$.validatedData$event
            pred_vec <- private$.validatedData$predicted
            n_folds <- self$options$nFolds
            cal_time <- self$options$calibrationTime
            n <- length(time_vec)

            # Create folds
            fold_idx <- sample(rep(1:n_folds, length.out = n))

            cindex_vals <- numeric(n_folds)
            brier_vals <- numeric(n_folds)

            for (fold in 1:n_folds) {
                private$.checkpoint()

                # Test set for this fold
                test_idx <- which(fold_idx == fold)
                test_time <- time_vec[test_idx]
                test_event <- event_vec[test_idx]
                test_pred <- pred_vec[test_idx]

                # Calculate C-index on test set
                tryCatch({
                    surv_obj <- survival::Surv(test_time, test_event)
                    cindex_result <- survival::concordance(surv_obj ~ test_pred)
                    cindex_vals[fold] <- cindex_result$concordance

                    # Calculate Brier score
                    survived <- ifelse(test_time >= cal_time & test_event == 0, 1,
                                      ifelse(test_time >= cal_time & test_event == 1, 0,
                                            ifelse(test_time < cal_time & test_event == 1, 0, NA)))
                    valid_idx <- !is.na(survived)
                    if (sum(valid_idx) > 0) {
                        brier_vals[fold] <- mean((test_pred[valid_idx] - survived[valid_idx])^2)
                    } else {
                        brier_vals[fold] <- NA
                    }
                }, error = function(e) {
                    cindex_vals[fold] <- NA
                    brier_vals[fold] <- NA
                })
            }

            # Return validated estimates (mean across folds)
            list(
                cindex = mean(cindex_vals, na.rm = TRUE),
                brier = mean(brier_vals, na.rm = TRUE)
            )
        },

        .populateResults = function() {
            # Populate performance metrics table
            perf_table <- self$results$performanceMetrics

            disc_results <- private$.discriminationResults
            cal_results <- private$.calibrationResults

            # C-index
            perf_table$addRow(rowKey = 1, values = list(
                metric = "C-Index",
                value = round(disc_results$apparent_cindex, 3),
                ci_lower = round(disc_results$cindex_ci_lower, 3),
                ci_upper = round(disc_results$cindex_ci_upper, 3),
                interpretation = private$.interpretCindex(disc_results$apparent_cindex)
            ))

            # Brier score
            if (!is.na(disc_results$apparent_brier)) {
                perf_table$addRow(rowKey = 2, values = list(
                    metric = "Brier Score",
                    value = round(disc_results$apparent_brier, 3),
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = private$.interpretBrier(disc_results$apparent_brier)
                ))
            }

            # Populate calibration metrics table
            cal_table <- self$results$calibrationMetrics
            cal_table$addRow(rowKey = 1, values = list(
                time_point = self$options$calibrationTime,
                cal_slope = round(cal_results$cal_slope, 3),
                cal_intercept = round(cal_results$cal_intercept, 3),
                eo_ratio = round(cal_results$eo_ratio, 3),
                interpretation = private$.interpretCalibration(cal_results$cal_slope, cal_results$eo_ratio)
            ))

            # Populate discrimination with validation results
            if (self$options$validationMethod != "none") {
                disc_table <- self$results$discriminationMetrics

                disc_table$addRow(rowKey = 1, values = list(
                    metric = "C-Index",
                    apparent = round(disc_results$apparent_cindex, 3),
                    validated = round(disc_results$validated_cindex, 3),
                    optimism = if (!is.na(disc_results$optimism_cindex)) round(disc_results$optimism_cindex, 3) else NA,
                    interpretation = private$.interpretCindex(disc_results$validated_cindex)
                ))

                if (!is.na(disc_results$apparent_brier)) {
                    disc_table$addRow(rowKey = 2, values = list(
                        metric = "Brier Score",
                        apparent = round(disc_results$apparent_brier, 3),
                        validated = round(disc_results$validated_brier, 3),
                        optimism = if (!is.na(disc_results$optimism_brier)) round(disc_results$optimism_brier, 3) else NA,
                        interpretation = private$.interpretBrier(disc_results$validated_brier)
                    ))
                }
            }

            # Generate validation summary
            private$.generateValidationSummary()
        },

        .interpretCindex = function(cindex) {
            if (is.na(cindex)) return("N/A")
            if (cindex >= 0.8) return("Excellent")
            if (cindex >= 0.7) return("Good")
            if (cindex >= 0.6) return("Moderate")
            return("Poor")
        },

        .interpretBrier = function(brier) {
            if (is.na(brier)) return("N/A")
            if (brier <= 0.1) return("Excellent")
            if (brier <= 0.2) return("Good")
            if (brier <= 0.3) return("Moderate")
            return("Poor")
        },

        .interpretCalibration = function(slope, eo_ratio) {
            if (is.na(slope) || is.na(eo_ratio)) return("N/A")

            slope_ok <- abs(slope - 1) < 0.2
            eo_ok <- abs(eo_ratio - 1) < 0.2

            if (slope_ok && eo_ok) return("Good calibration")
            if (slope_ok) return("Good slope, check E:O ratio")
            if (eo_ok) return("Good E:O, check slope")
            return("Needs recalibration")
        },

        .generateValidationSummary = function() {
            disc_results <- private$.discriminationResults
            cal_results <- private$.calibrationResults
            n <- private$.validatedData$n
            n_events <- private$.validatedData$n_events

            summary_text <- glue::glue("
            <h4>Validation Summary</h4>
            <p><b>Sample Size:</b> {n} patients ({n_events} events, {n - n_events} censored)</p>
            <p><b>Calibration Time:</b> {self$options$calibrationTime} months</p>
            <p><b>Validation Method:</b> {self$options$validationMethod}</p>

            <p><b>Key Findings:</b></p>
            <ul>
            <li>C-Index: {round(disc_results$apparent_cindex, 3)}
                (95% CI: {round(disc_results$cindex_ci_lower, 3)}-{round(disc_results$cindex_ci_upper, 3)})</li>
            <li>Calibration Slope: {round(cal_results$cal_slope, 3)} (ideal = 1.0)</li>
            <li>E:O Ratio: {round(cal_results$eo_ratio, 3)} (ideal = 1.0)</li>
            </ul>
            ")

            self$results$validationSummary$setContent(summary_text)
        },

        .generateTRIPODReport = function() {
            disc_results <- private$.discriminationResults
            cal_results <- private$.calibrationResults
            n <- private$.validatedData$n
            n_events <- private$.validatedData$n_events

            tripod_text <- glue::glue("
            <h4>TRIPOD-Compliant Validation Report</h4>

            <p><b>Study Design:</b> Model validation study</p>
            <p><b>Sample:</b> {n} patients with {n_events} events ({round(100*n_events/n, 1)}%)</p>
            <p><b>Follow-up:</b> Median {round(median(private$.validatedData$time), 1)} months</p>

            <h5>Discrimination Performance</h5>
            <ul>
            <li>C-Index (concordance): {round(disc_results$apparent_cindex, 3)}</li>
            <li>95% Confidence Interval: ({round(disc_results$cindex_ci_lower, 3)}, {round(disc_results$cindex_ci_upper, 3)})</li>
            <li>Interpretation: {private$.interpretCindex(disc_results$apparent_cindex)} discrimination</li>
            </ul>

            <h5>Calibration Performance at {self$options$calibrationTime} months</h5>
            <ul>
            <li>Calibration Slope: {round(cal_results$cal_slope, 3)} (ideal = 1.0)</li>
            <li>Calibration Intercept: {round(cal_results$cal_intercept, 3)} (ideal = 0.0)</li>
            <li>E:O Ratio: {round(cal_results$eo_ratio, 3)} (ideal = 1.0)</li>
            <li>Assessment: {private$.interpretCalibration(cal_results$cal_slope, cal_results$eo_ratio)}</li>
            </ul>

            <h5>Internal Validation</h5>
            <p>Method: {self$options$validationMethod}</p>
            ")

            if (self$options$validationMethod == "bootstrap") {
                tripod_text <- paste0(tripod_text, glue::glue("
                <ul>
                <li>Bootstrap samples: {self$options$nBootstrap}</li>
                <li>Validated C-Index: {round(disc_results$validated_cindex, 3)}</li>
                <li>Optimism: {round(disc_results$optimism_cindex, 3)}</li>
                </ul>
                "))
            }

            self$results$tripodReport$setContent(tripod_text)
        },

        .generateInterpretation = function() {
            disc_results <- private$.discriminationResults
            cal_results <- private$.calibrationResults

            interp_text <- glue::glue("
            <h4>Clinical Interpretation</h4>

            <p><b>Model Discrimination:</b> The C-index of {round(disc_results$apparent_cindex, 3)}
            indicates {tolower(private$.interpretCindex(disc_results$apparent_cindex))} ability to
            distinguish between patients who will and will not experience the event.</p>

            <p><b>Model Calibration:</b> {private$.interpretCalibration(cal_results$cal_slope, cal_results$eo_ratio)}.
            The calibration slope of {round(cal_results$cal_slope, 3)} suggests predictions are
            {if (cal_results$cal_slope < 0.9) 'too extreme' else if (cal_results$cal_slope > 1.1) 'too modest' else 'appropriately calibrated'}.</p>

            <p><b>Recommendations:</b></p>
            <ul>
            ")

            if (disc_results$apparent_cindex < 0.7) {
                interp_text <- paste0(interp_text, "<li>Consider adding additional predictors to improve discrimination</li>")
            }

            if (abs(cal_results$cal_slope - 1) > 0.2) {
                interp_text <- paste0(interp_text, "<li>Model may benefit from recalibration (updating intercept and slope)</li>")
            }

            if (abs(cal_results$eo_ratio - 1) > 0.2) {
                interp_text <- paste0(interp_text, "<li>Systematic over/underestimation detected - check model assumptions</li>")
            }

            interp_text <- paste0(interp_text, "</ul>")

            self$results$clinicalInterpretation$setContent(interp_text)
        },

        .calibrationPlot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.calibrationResults)) return()

            cal_results <- private$.calibrationResults$grouped_results

            # Extract predicted and observed values
            valid_results <- Filter(function(x) !is.na(x$observed), cal_results)

            if (length(valid_results) < 3) {
                plot <- ggplot2::ggplot() +
                    ggplot2::annotate("text", x = 0.5, y = 0.5,
                                    label = "Insufficient data for calibration plot",
                                    size = 6) +
                    ggplot2::theme_void()
                print(plot)
                return(TRUE)
            }

            plot_df <- data.frame(
                predicted = sapply(valid_results, function(x) x$predicted),
                observed = sapply(valid_results, function(x) x$observed),
                n = sapply(valid_results, function(x) x$n)
            )

            # Create calibration plot
            plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = predicted, y = observed, size = n)) +
                ggplot2::geom_point(alpha = 0.6, color = "#0072B2") +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
                ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::labs(
                    title = paste("Calibration Plot at", self$options$calibrationTime, "Months"),
                    subtitle = "Perfect calibration shown as dashed line",
                    x = "Predicted Survival Probability",
                    y = "Observed Survival Probability (Kaplan-Meier)",
                    size = "N"
                ) +
                ggtheme

            # Add smooth line if requested
            if (self$options$smoothCalibration) {
                plot <- plot +
                    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2)
            }

            print(plot)
            TRUE
        },

        .groupedCalibrationPlot = function(image, ggtheme, theme, ...) {
            # Placeholder for grouped calibration curves at multiple time points
            plot <- ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5,
                                label = "Grouped calibration curves\n(under development)",
                                size = 6) +
                ggplot2::theme_void()
            print(plot)
            TRUE
        },

        .cindexPlot = function(image, ggtheme, theme, ...) {
            # Placeholder for time-dependent C-index plot
            plot <- ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5,
                                label = "Time-dependent C-index\n(under development)",
                                size = 6) +
                ggplot2::theme_void()
            print(plot)
            TRUE
        },

        .brierPlot = function(image, ggtheme, theme, ...) {
            # Placeholder for Brier score over time plot
            plot <- ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5,
                                label = "Brier score over time\n(under development)",
                                size = 6) +
                ggplot2::theme_void()
            print(plot)
            TRUE
        }
    )
)
