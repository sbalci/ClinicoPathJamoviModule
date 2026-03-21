#' @title Leave-One-Center-Out Cross-Validation
#' @description
#' Internal-external validation for multi-institutional prediction models.
#' Trains on all-but-one center and evaluates on held-out center, repeating
#' for each center. Supports logistic, Cox, and linear regression with
#' optional LASSO regularization.
#'
#' @importFrom R6 R6Class
#' @import jmvcore

leaveonecenteroutClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "leaveonecenteroutClass",
    inherit = leaveonecenteroutBase,
    private = list(

        # Store results for plot reuse
        .cv_results = NULL,
        .noticeList = list(),

        .addNotice = function(type, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, content = content)
        },

        .renderNotices = function() {
            notices <- private$.noticeList
            if (length(notices) == 0) {
                self$results$notices$setContent("")
                return()
            }
            html_parts <- vapply(notices, function(n) {
                css_class <- switch(n$type,
                    "error" = "alert-danger",
                    "strong_warning" = "alert-warning",
                    "warning" = "alert-info",
                    "info" = "alert-success",
                    "alert-info")
                sprintf("<div class='alert %s' style='padding:6px 12px;margin:2px 0;font-size:0.9em;'>%s</div>",
                        css_class, n$content)
            }, character(1))
            self$results$notices$setContent(paste(html_parts, collapse = "\n"))
        },

        .init = function() {
            if (is.null(self$options$outcome) ||
                is.null(self$options$predictors) || length(self$options$predictors) == 0 ||
                is.null(self$options$centerVariable)) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-info'>",
                    "<h4>Leave-One-Center-Out Cross-Validation</h4>",
                    "<p>Internal-external validation for multi-institutional prediction models. ",
                    "Trains on all-but-one center and evaluates on the held-out center.</p>",
                    "<h5>Required inputs:</h5>",
                    "<ul>",
                    "<li><strong>Outcome</strong>: Variable to predict</li>",
                    "<li><strong>Predictors</strong>: Variables to include in the model</li>",
                    "<li><strong>Center Variable</strong>: Institution/site grouping (min 3 centers)</li>",
                    "</ul>",
                    "<p><em>Recommended by TRIPOD guidelines for multi-center studies ",
                    "(Debray et al., BMJ 2015).</em></p>",
                    "</div>"))
                return()
            }
        },

        .run = function() {
            if (is.null(self$options$outcome) ||
                is.null(self$options$predictors) || length(self$options$predictors) == 0 ||
                is.null(self$options$centerVariable)) return()

            set.seed(self$options$random_seed)

            # ── 1. Prepare data ────────────────────────────────────────────
            prepared <- tryCatch(private$.prepareData(), error = function(e) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'><h4>Data Error</h4><p>", e$message, "</p></div>"))
                return(NULL)
            })
            if (is.null(prepared)) return()

            self$results$todo$setContent("")
            private$.noticeList <- list()

            # ── Notices: data quality warnings ────────────────────────────
            if (length(prepared$small_centers) > 0) {
                private$.addNotice("warning", sprintf(
                    .('Centers with <5 cases may produce unreliable estimates: %s'),
                    paste(prepared$small_centers, collapse = ', ')))
            }

            if (prepared$model_type %in% c("logistic", "cox")) {
                n_events <- sum(prepared$y == 1, na.rm = TRUE)
                n_predictors <- length(self$options$predictors)
                epv <- n_events / max(n_predictors, 1)
                if (epv < 10) {
                    private$.addNotice("strong_warning", sprintf(
                        .('Low events per variable (EPV = %.1f, %d events / %d predictors). Minimum 10 recommended; model may be overfit.'),
                        epv, n_events, n_predictors))
                }

                # Inform which level was used as event
                if (!is.na(prepared$event_level)) {
                    private$.addNotice("info", sprintf(
                        .('Event level: "%s" (coded as 1). Reference level: "%s" (coded as 0).'),
                        prepared$event_level, prepared$ref_level))
                }
            }

            if (self$options$useLasso && prepared$model_type == "linear") {
                private$.addNotice("warning",
                    .('LASSO regularization is not available for linear regression. Standard OLS is used instead.'))
            }

            # ── 2. Design summary ─────────────────────────────────────────
            private$.populateDesignSummary(prepared)

            private$.checkpoint()

            # ── 3. Run LOOCV ───────────────────────────────────────────────
            cv_results <- private$.runLOOCV(prepared)
            private$.cv_results <- cv_results

            # ── 4. Populate per-center results ─────────────────────────────
            private$.populatePerCenterResults(cv_results)

            private$.checkpoint()

            # ── 5. Pooled performance ──────────────────────────────────────
            if (self$options$pooledPerformance) {
                private$.populatePooledPerformance(cv_results)
            }

            # ── 6. Interpretation ──────────────────────────────────────────
            private$.populateInterpretation(prepared, cv_results)

            # ── Notices: post-analysis ────────────────────────────────────
            auc_vals <- sapply(cv_results, function(r) r$auc)
            auc_valid <- auc_vals[!is.na(auc_vals)]
            if (length(auc_valid) > 0) {
                n_valid <- sapply(cv_results, function(r) r$n_test)[!is.na(auc_vals)]
                pooled_disc <- mean(auc_valid)
                sd_disc <- sd(auc_valid)

                if (prepared$model_type != "linear" && pooled_disc < 0.7) {
                    metric <- if (prepared$model_type == "cox") "C-index" else "AUC"
                    private$.addNotice("strong_warning", sprintf(
                        .('Poor external discrimination (pooled %s = %.3f < 0.70). Consider simplifying the model or adding stronger predictors.'),
                        metric, pooled_disc))
                }

                if (prepared$model_type == "linear" && pooled_disc < 0) {
                    private$.addNotice("strong_warning", sprintf(
                        .('Pooled R-squared is negative (%.3f): the model predicts worse than the mean. The model does not generalize.'),
                        pooled_disc))
                }

                if (sd_disc > 0.10) {
                    private$.addNotice("warning", sprintf(
                        .('High heterogeneity across centers (SD = %.3f > 0.10). Model performance varies substantially; consider center-level covariates or stratification.'),
                        sd_disc))
                }
            }

            n_skipped <- sum(sapply(cv_results, function(r) grepl("Skipped|Error", r$assessment)))
            private$.addNotice("info", sprintf(
                .('LOOCV completed: %d centers, %d observations, %s model%s.'),
                prepared$n_centers, prepared$n, self$options$modelType,
                if (n_skipped > 0) sprintf(' (%d center(s) skipped)', n_skipped) else ''))

            private$.renderNotices()
        },

        # ══════════════════════════════════════════════════════════════════
        # Prepare data
        # ══════════════════════════════════════════════════════════════════
        .prepareData = function() {
            data <- self$data
            outcome_var <- self$options$outcome
            predictor_vars <- self$options$predictors
            center_var <- self$options$centerVariable
            model_type <- self$options$modelType

            outcome_raw <- data[[outcome_var]]
            center_raw <- as.character(data[[center_var]])
            predictors <- data[predictor_vars]

            # Sanitize column names for formula-based fitting (handles spaces/special chars)
            colnames(predictors) <- make.names(colnames(predictors), unique = TRUE)

            # Handle outcome encoding for binary models
            if (model_type %in% c("logistic", "cox")) {
                if (is.factor(outcome_raw) || is.character(outcome_raw)) {
                    outcome_chr <- as.character(outcome_raw)
                    levels_obs <- sort(unique(outcome_chr[!is.na(outcome_chr)]))
                    event_level <- as.character(self$options$outcomeLevel)
                    if (is.null(event_level) || !nzchar(event_level)) event_level <- levels_obs[2]
                    ref_level <- setdiff(levels_obs, event_level)[1]
                    y <- rep(NA_real_, length(outcome_chr))
                    y[outcome_chr == event_level] <- 1
                    y[outcome_chr == ref_level] <- 0
                } else {
                    y <- jmvcore::toNumeric(outcome_raw)
                    event_level <- "1"
                    ref_level <- "0"
                }
            } else {
                y <- jmvcore::toNumeric(outcome_raw)
                event_level <- NA
                ref_level <- NA
            }

            # Time variable for Cox
            time_val <- NULL
            if (model_type == "cox") {
                if (is.null(self$options$elapsedtime)) {
                    stop(.("Time variable is required for Cox regression."))
                }
                time_val <- jmvcore::toNumeric(data[[self$options$elapsedtime]])
            }

            # Complete cases
            if (!is.null(time_val)) {
                complete <- complete.cases(y, predictors, center_raw, time_val)
            } else {
                complete <- complete.cases(y, predictors, center_raw)
            }
            n_complete <- sum(complete)
            if (n_complete < 20) stop(.("Too few complete cases for LOOCV."))

            y <- y[complete]
            predictors <- predictors[complete, , drop = FALSE]
            center_raw <- center_raw[complete]
            if (!is.null(time_val)) time_val <- time_val[complete]

            centers <- sort(unique(center_raw))
            n_centers <- length(centers)
            if (n_centers < 3) {
                stop(sprintf(.("At least 3 centers are required for LOOCV. Found %d center(s): %s"),
                             n_centers, paste(centers, collapse = ", ")))
            }

            # Check minimum cases per center
            center_counts <- table(center_raw)
            small_centers <- names(center_counts[center_counts < 5])

            center_sizes <- as.integer(center_counts)
            names(center_sizes) <- names(center_counts)

            list(
                y = y,
                predictors = predictors,
                centers = center_raw,
                center_levels = centers,
                n_centers = n_centers,
                n = n_complete,
                time = time_val,
                model_type = model_type,
                event_level = event_level,
                ref_level = ref_level,
                center_counts = center_sizes,
                small_centers = small_centers
            )
        },

        # ══════════════════════════════════════════════════════════════════
        # Run LOOCV
        # ══════════════════════════════════════════════════════════════════
        .runLOOCV = function(prepared) {
            results <- list()

            for (i in seq_along(prepared$center_levels)) {
                test_center <- prepared$center_levels[i]
                test_idx <- prepared$centers == test_center
                train_idx <- !test_idx

                n_train <- sum(train_idx)
                n_test <- sum(test_idx)

                y_train <- prepared$y[train_idx]
                y_test <- prepared$y[test_idx]
                X_train <- prepared$predictors[train_idx, , drop = FALSE]
                X_test <- prepared$predictors[test_idx, , drop = FALSE]
                time_train <- if (!is.null(prepared$time)) prepared$time[train_idx] else NULL
                time_test <- if (!is.null(prepared$time)) prepared$time[test_idx] else NULL

                # Skip if test set has no events or all events (for binary)
                if (prepared$model_type %in% c("logistic", "cox")) {
                    n_events_test <- sum(y_test == 1, na.rm = TRUE)
                    if (n_events_test == 0 || n_events_test == n_test) {
                        results[[i]] <- list(
                            center = test_center, n_train = n_train, n_test = n_test,
                            n_events_test = n_events_test,
                            auc = NA, auc_ci = c(NA, NA), brier = NA, accuracy = NA,
                            assessment = "Skipped (no variation in test set)")
                        next
                    }
                } else {
                    n_events_test <- NA
                }

                # Fit model
                fold_result <- tryCatch(
                    private$.fitAndEvaluate(y_train, y_test, X_train, X_test, prepared, time_train, time_test),
                    error = function(e) {
                        list(auc = NA, auc_ci = c(NA, NA), brier = NA, accuracy = NA,
                             assessment = paste0("Error: ", e$message))
                    }
                )

                results[[i]] <- list(
                    center = test_center,
                    n_train = n_train,
                    n_test = n_test,
                    n_events_test = if (is.na(n_events_test)) NA_integer_ else n_events_test,
                    auc = fold_result$auc,
                    auc_ci = fold_result$auc_ci,
                    brier = fold_result$brier,
                    accuracy = fold_result$accuracy,
                    assessment = fold_result$assessment
                )
            }

            results
        },

        .fitAndEvaluate = function(y_train, y_test, X_train, X_test, prepared,
                                    time_train = NULL, time_test = NULL) {
            model_type <- prepared$model_type

            if (model_type == "logistic") {
                if (self$options$useLasso) {
                    return(private$.fitLassoLogistic(y_train, y_test, X_train, X_test))
                } else {
                    return(private$.fitGLM(y_train, y_test, X_train, X_test))
                }
            } else if (model_type == "cox") {
                if (is.null(time_train) || is.null(time_test)) {
                    return(list(auc = NA, auc_ci = c(NA, NA), brier = NA, accuracy = NA,
                                assessment = "Error: time variable missing"))
                }
                if (self$options$useLasso) {
                    return(private$.fitLassoCox(y_train, y_test, X_train, X_test,
                                                time_train, time_test))
                } else {
                    return(private$.fitCox(y_train, y_test, X_train, X_test,
                                           time_train, time_test))
                }
            } else {
                return(private$.fitLinear(y_train, y_test, X_train, X_test))
            }
        },

        .fitGLM = function(y_train, y_test, X_train, X_test) {
            df_train <- data.frame(y = y_train, X_train)
            df_test <- data.frame(y = y_test, X_test)

            model <- glm(y ~ ., data = df_train, family = binomial)
            prob_test <- predict(model, newdata = df_test, type = "response")

            private$.evaluateBinary(y_test, prob_test)
        },

        .fitLassoLogistic = function(y_train, y_test, X_train, X_test) {
            X_train_mm <- tryCatch({
                mm <- model.matrix(~ ., data = X_train)[, -1, drop = FALSE]
                mm
            }, error = function(e) as.matrix(X_train))

            X_test_mm <- tryCatch({
                mm <- model.matrix(~ ., data = X_test)[, -1, drop = FALSE]
                mm
            }, error = function(e) as.matrix(X_test))

            # Align columns
            common_cols <- intersect(colnames(X_train_mm), colnames(X_test_mm))
            if (length(common_cols) == 0) {
                return(list(auc = NA, auc_ci = c(NA, NA), brier = NA, accuracy = NA,
                            assessment = "No common columns between train/test"))
            }
            X_train_mm <- X_train_mm[, common_cols, drop = FALSE]
            X_test_mm <- X_test_mm[, common_cols, drop = FALSE]

            # Remove zero-variance columns in training
            col_vars <- apply(X_train_mm, 2, var, na.rm = TRUE)
            good_cols <- !is.na(col_vars) & col_vars > 0
            if (sum(good_cols) < 1) {
                return(list(auc = NA, auc_ci = c(NA, NA), brier = NA, accuracy = NA,
                            assessment = "All predictor columns have zero variance"))
            }
            X_train_mm <- X_train_mm[, good_cols, drop = FALSE]
            X_test_mm <- X_test_mm[, good_cols, drop = FALSE]

            nfolds_inner <- min(10, length(y_train) - 1)
            nfolds_inner <- max(nfolds_inner, 3)

            cv_fit <- glmnet::cv.glmnet(
                x = X_train_mm, y = y_train,
                family = "binomial", alpha = 1,
                nfolds = nfolds_inner
            )

            lambda_opt <- switch(self$options$lambdaMethod,
                "lambda.min" = cv_fit$lambda.min,
                "lambda.1se" = cv_fit$lambda.1se,
                cv_fit$lambda.1se)

            prob_test <- as.numeric(predict(cv_fit, newx = X_test_mm,
                                           s = lambda_opt, type = "response"))

            private$.evaluateBinary(y_test, prob_test)
        },

        .fitCox = function(y_train, y_test, X_train, X_test, time_train, time_test) {
            df_train <- data.frame(time = time_train, status = y_train, X_train)
            df_test <- data.frame(time = time_test, status = y_test, X_test)

            model <- survival::coxph(survival::Surv(time, status) ~ ., data = df_train)
            lp_test <- predict(model, newdata = df_test, type = "lp")

            conc <- survival::concordance(
                survival::Surv(time_test, y_test) ~ lp_test,
                reverse = TRUE)
            c_index <- conc$concordance
            se <- sqrt(conc$var)

            ci_lo <- max(0, c_index - 1.96 * se)
            ci_hi <- min(1, c_index + 1.96 * se)
            assessment <- if (c_index >= 0.7) "Good" else if (c_index >= 0.6) "Acceptable" else "Poor"
            list(auc = c_index, auc_ci = c(ci_lo, ci_hi),
                 brier = NA, accuracy = NA, assessment = assessment)
        },

        .fitLassoCox = function(y_train, y_test, X_train, X_test, time_train, time_test) {
            X_train_mm <- tryCatch({
                model.matrix(~ ., data = X_train)[, -1, drop = FALSE]
            }, error = function(e) as.matrix(X_train))
            X_test_mm <- tryCatch({
                model.matrix(~ ., data = X_test)[, -1, drop = FALSE]
            }, error = function(e) as.matrix(X_test))

            common_cols <- intersect(colnames(X_train_mm), colnames(X_test_mm))
            if (length(common_cols) == 0) {
                return(list(auc = NA, auc_ci = c(NA, NA), brier = NA, accuracy = NA,
                            assessment = "No common columns between train/test"))
            }
            X_train_mm <- X_train_mm[, common_cols, drop = FALSE]
            X_test_mm <- X_test_mm[, common_cols, drop = FALSE]

            col_vars <- apply(X_train_mm, 2, var, na.rm = TRUE)
            good_cols <- !is.na(col_vars) & col_vars > 0
            if (sum(good_cols) < 1) {
                return(list(auc = NA, auc_ci = c(NA, NA), brier = NA, accuracy = NA,
                            assessment = "All predictor columns have zero variance"))
            }
            X_train_mm <- X_train_mm[, good_cols, drop = FALSE]
            X_test_mm <- X_test_mm[, good_cols, drop = FALSE]

            y_surv <- survival::Surv(time_train, y_train)
            nfolds_inner <- min(10, length(y_train) - 1)
            nfolds_inner <- max(nfolds_inner, 3)

            cv_fit <- glmnet::cv.glmnet(
                x = X_train_mm, y = y_surv,
                family = "cox", alpha = 1,
                nfolds = nfolds_inner)

            lambda_opt <- switch(self$options$lambdaMethod,
                "lambda.min" = cv_fit$lambda.min,
                "lambda.1se" = cv_fit$lambda.1se,
                cv_fit$lambda.1se)

            lp_test <- as.numeric(predict(cv_fit, newx = X_test_mm,
                                          s = lambda_opt, type = "link"))

            conc <- survival::concordance(
                survival::Surv(time_test, y_test) ~ lp_test,
                reverse = TRUE)
            c_index <- conc$concordance
            se <- sqrt(conc$var)

            ci_lo <- max(0, c_index - 1.96 * se)
            ci_hi <- min(1, c_index + 1.96 * se)
            assessment <- if (c_index >= 0.7) "Good" else if (c_index >= 0.6) "Acceptable" else "Poor"
            list(auc = c_index, auc_ci = c(ci_lo, ci_hi),
                 brier = NA, accuracy = NA, assessment = assessment)
        },

        .fitLinear = function(y_train, y_test, X_train, X_test) {
            df_train <- data.frame(y = y_train, X_train)
            df_test <- data.frame(y = y_test, X_test)

            model <- lm(y ~ ., data = df_train)
            pred_test <- predict(model, newdata = df_test)

            rmse <- sqrt(mean((y_test - pred_test)^2))
            r_squared <- 1 - sum((y_test - pred_test)^2) / sum((y_test - mean(y_test))^2)

            assessment <- if (r_squared > 0.5) "Good" else if (r_squared > 0.2) "Moderate" else if (r_squared >= 0) "Poor" else "Very Poor"

            list(auc = r_squared, auc_ci = c(NA, NA), brier = rmse,
                 accuracy = NA, assessment = assessment)
        },

        .evaluateBinary = function(y_test, prob_test) {
            auc_val <- NA
            auc_ci <- c(NA, NA)
            brier <- mean((prob_test - y_test)^2)
            predicted_class <- ifelse(prob_test >= 0.5, 1, 0)
            accuracy <- mean(predicted_class == y_test)

            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_obj <- pROC::roc(y_test, prob_test, quiet = TRUE)
                    auc_val <- as.numeric(pROC::auc(roc_obj))
                    ci_obj <- tryCatch(pROC::ci.auc(roc_obj, method = "delong"),
                                       error = function(e) NULL)
                    if (!is.null(ci_obj)) auc_ci <- c(ci_obj[1], ci_obj[3])
                }
            }, error = function(e) {
                # AUC computation failed; leave auc_val as NA
            })

            assessment <- if (!is.na(auc_val)) {
                if (auc_val >= 0.8) "Good" else if (auc_val >= 0.7) "Acceptable" else "Poor"
            } else {
                "N/A"
            }

            list(auc = auc_val, auc_ci = auc_ci, brier = brier,
                 accuracy = accuracy, assessment = assessment)
        },

        # ══════════════════════════════════════════════════════════════════
        # Populate results
        # ══════════════════════════════════════════════════════════════════
        .populateDesignSummary = function(prepared) {
            table <- self$results$designSummary
            rows <- list(
                list("Total observations", as.character(prepared$n)),
                list("Number of centers", as.character(prepared$n_centers)),
                list("Centers", paste(prepared$center_levels, collapse = ", ")),
                list("Cases per center (range)", sprintf("%d - %d",
                    min(prepared$center_counts), max(prepared$center_counts))),
                list("Cases per center", paste(
                    sprintf("%s: %d", names(prepared$center_counts), prepared$center_counts),
                    collapse = "; ")),
                list("Model type", self$options$modelType),
                list("Number of predictors", as.character(length(self$options$predictors))),
                list("LASSO regularization", if (self$options$useLasso) "Yes" else "No"),
                list("Validation type", "Internal-external (LOOCV)")
            )
            if (!is.na(prepared$event_level)) {
                n_events <- sum(prepared$y == 1, na.rm = TRUE)
                rows <- c(rows, list(list("Event class", paste0(prepared$event_level,
                    " (n=", n_events, ", ", sprintf("%.1f%%", n_events / prepared$n * 100), ")"))))
            }
            for (i in seq_along(rows)) {
                table$addRow(rowKey = i, values = list(
                    statistic = rows[[i]][[1]], value = rows[[i]][[2]]))
            }
        },

        .populatePerCenterResults = function(cv_results) {
            table <- self$results$perCenterResults
            for (i in seq_along(cv_results)) {
                r <- cv_results[[i]]
                table$addRow(rowKey = i, values = list(
                    center = r$center,
                    n_train = r$n_train,
                    n_test = r$n_test,
                    n_events_test = r$n_events_test,
                    auc = r$auc,
                    auc_ci_lower = r$auc_ci[1],
                    auc_ci_upper = r$auc_ci[2],
                    brier = r$brier,
                    accuracy = r$accuracy,
                    assessment = r$assessment
                ))
            }
        },

        .populatePooledPerformance = function(cv_results) {
            table <- self$results$pooledPerformance
            model_type <- self$options$modelType

            auc_vals <- sapply(cv_results, function(r) r$auc)
            brier_vals <- sapply(cv_results, function(r) r$brier)
            acc_vals <- sapply(cv_results, function(r) r$accuracy)
            n_tests <- sapply(cv_results, function(r) r$n_test)

            auc_valid <- auc_vals[!is.na(auc_vals)]
            brier_valid <- brier_vals[!is.na(brier_vals)]
            acc_valid <- acc_vals[!is.na(acc_vals)]

            # Simple mean across centers (consistent with SD and CI)
            if (length(auc_valid) > 0) {
                mean_auc <- mean(auc_valid)
                sd_auc <- sd(auc_valid)
                ci_auc <- if (length(auc_valid) >= 3)
                    mean_auc + c(-1, 1) * qt(0.975, length(auc_valid) - 1) * sd_auc / sqrt(length(auc_valid))
                else c(NA, NA)

                metric_label <- if (model_type == "linear") "R-squared" else if (model_type == "cox") "C-index" else "AUC"
                table$addRow(rowKey = 1, values = list(
                    metric = paste0(metric_label, " (mean)"),
                    mean_value = mean_auc, sd_value = sd_auc,
                    ci_lower = ci_auc[1], ci_upper = ci_auc[2],
                    min_value = min(auc_valid), max_value = max(auc_valid)))
            }

            if (length(brier_valid) > 0) {
                brier_label <- if (model_type == "linear") "RMSE" else "Brier Score"
                table$addRow(rowKey = 2, values = list(
                    metric = brier_label,
                    mean_value = mean(brier_valid), sd_value = sd(brier_valid),
                    ci_lower = NA, ci_upper = NA,
                    min_value = min(brier_valid), max_value = max(brier_valid)))
            }

            if (length(acc_valid) > 0 && model_type != "linear") {
                table$addRow(rowKey = 3, values = list(
                    metric = "Accuracy",
                    mean_value = mean(acc_valid), sd_value = sd(acc_valid),
                    ci_lower = NA, ci_upper = NA,
                    min_value = min(acc_valid), max_value = max(acc_valid)))
            }
        },

        .populateInterpretation = function(prepared, cv_results) {
            auc_vals <- sapply(cv_results, function(r) r$auc)
            auc_valid <- auc_vals[!is.na(auc_vals)]
            n_valid <- sapply(cv_results, function(r) r$n_test)[!is.na(auc_vals)]

            if (length(auc_valid) == 0) {
                self$results$interpretation$setContent(
                    "<p>No valid center-level results to interpret.</p>")
                return()
            }

            pooled_auc <- mean(auc_valid)
            sd_auc <- sd(auc_valid)
            range_auc <- range(auc_valid)
            heterogeneity <- if (sd_auc > 0.1) "high" else if (sd_auc > 0.05) "moderate" else "low"

            metric_label <- if (self$options$modelType == "linear") "R-squared" else if (self$options$modelType == "cox") "C-index" else "AUC"

            html <- paste0(
                "<h4>Internal-External Validation Summary</h4>",
                "<p>The model was validated using leave-one-center-out cross-validation ",
                "across ", prepared$n_centers, " centers (N=", prepared$n, " total).</p>",
                "<ul>",
                "<li><strong>Pooled ", metric_label, "</strong>: ",
                sprintf("%.3f (SD: %.3f, range: %.3f-%.3f)", pooled_auc, sd_auc, range_auc[1], range_auc[2]),
                "</li>",
                "<li><strong>Heterogeneity across centers</strong>: ", heterogeneity, "</li>",
                "</ul>",
                "<h5>Interpretation</h5>"
            )

            if (self$options$modelType == "linear") {
                if (pooled_auc >= 0.5) {
                    html <- paste0(html,
                        "<p>The model explains a <strong>substantial proportion of variance</strong> ",
                        "(pooled R-squared >= 0.50). ")
                } else if (pooled_auc >= 0.2) {
                    html <- paste0(html,
                        "<p>The model explains a <strong>moderate proportion of variance</strong> ",
                        "(pooled R-squared 0.20-0.50). ")
                } else if (pooled_auc >= 0) {
                    html <- paste0(html,
                        "<p>The model explains a <strong>small proportion of variance</strong> ",
                        "(pooled R-squared < 0.20). Consider adding stronger predictors. ")
                } else {
                    html <- paste0(html,
                        "<p>The model predicts <strong>worse than the mean</strong> ",
                        "(pooled R-squared < 0). The model does not generalize and should be reconsidered. ")
                }
            } else {
                if (pooled_auc >= 0.8) {
                    html <- paste0(html,
                        "<p>The model shows <strong>good external generalizability</strong> ",
                        "(pooled ", metric_label, " >= 0.80). ")
                } else if (pooled_auc >= 0.7) {
                    html <- paste0(html,
                        "<p>The model shows <strong>acceptable external generalizability</strong> ",
                        "(pooled ", metric_label, " 0.70-0.80). ")
                } else {
                    html <- paste0(html,
                        "<p>The model shows <strong>poor external generalizability</strong> ",
                        "(pooled ", metric_label, " < 0.70). Consider simplifying the model or collecting more data. ")
                }
            }

            if (heterogeneity == "high") {
                html <- paste0(html,
                    "There is <strong>substantial heterogeneity</strong> across centers ",
                    "(SD > 0.10), suggesting the model performs inconsistently. ",
                    "Consider stratifying by center characteristics or including center-level ",
                    "covariates.</p>")
            } else {
                html <- paste0(html,
                    "The performance is <strong>consistent across centers</strong>, ",
                    "supporting generalizability.</p>")
            }

            html <- paste0(html,
                "<h5>Comparison with Apparent Performance</h5>",
                "<p>The LOOCV pooled ", metric_label, " is typically lower than the apparent ",
                "(training) ", metric_label, ". The difference represents the <strong>optimism</strong> ",
                "— the degree to which training-set performance overestimates true performance. ",
                "LOOCV provides a more honest estimate than standard bootstrap or k-fold CV ",
                "because each test set comes from a genuinely separate institution.</p>",
                "<p><em>Reference: Debray TPA et al. A new framework to enhance the interpretation ",
                "of external validation studies. J Clin Epidemiol 2015;68:279-289.</em></p>")

            self$results$interpretation$setContent(html)
        },

        # ══════════════════════════════════════════════════════════════════
        # Forest plot
        # ══════════════════════════════════════════════════════════════════
        .forestPlot = function(image, ggtheme, theme, ...) {
            cv_results <- private$.cv_results
            if (is.null(cv_results)) {
                # Recompute if needed (ensure reproducibility)
                set.seed(self$options$random_seed)
                prepared <- tryCatch(private$.prepareData(), error = function(e) NULL)
                if (is.null(prepared)) return(FALSE)
                cv_results <- private$.runLOOCV(prepared)
            }

            auc_vals <- sapply(cv_results, function(r) r$auc)
            if (all(is.na(auc_vals))) return(FALSE)

            centers <- sapply(cv_results, function(r) r$center)
            ci_lower <- sapply(cv_results, function(r) r$auc_ci[1])
            ci_upper <- sapply(cv_results, function(r) r$auc_ci[2])
            n_tests <- sapply(cv_results, function(r) r$n_test)

            valid <- !is.na(auc_vals)
            pooled <- mean(auc_vals[valid])

            metric_label <- if (self$options$modelType == "linear") "R-squared" else if (self$options$modelType == "cox") "C-index" else "AUC"

            df <- data.frame(
                center = factor(centers[valid], levels = rev(centers[valid])),
                auc = auc_vals[valid],
                ci_lower = ci_lower[valid],
                ci_upper = ci_upper[valid],
                n = n_tests[valid],
                stringsAsFactors = FALSE
            )

            if (requireNamespace("ggplot2", quietly = TRUE)) {
                p <- ggplot2::ggplot(df, ggplot2::aes(x = auc, y = center)) +
                    ggplot2::geom_point(ggplot2::aes(size = n), color = "#2E86C1") +
                    ggplot2::geom_errorbarh(
                        ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
                        height = 0.2, color = "#2E86C1", na.rm = TRUE) +
                    ggplot2::geom_vline(xintercept = pooled, linetype = "dashed",
                                        color = "#E74C3C", linewidth = 0.8) +
                    ggplot2::annotate("text", x = pooled, y = nrow(df) + 0.4,
                                     label = sprintf("Pooled = %.3f", pooled),
                                     color = "#E74C3C", hjust = -0.1, vjust = 0, size = 3.5) +
                    ggplot2::scale_size_continuous(name = "N (test)", range = c(2, 6)) +
                    ggplot2::labs(
                        title = paste0("Leave-One-Center-Out: ", metric_label, " per Center"),
                        x = metric_label, y = "Held-Out Center") +
                x_lo <- min(c(df$ci_lower, df$auc), na.rm = TRUE) - 0.05
                if (self$options$modelType != "linear") x_lo <- max(0, x_lo)
                x_hi <- min(1, max(c(df$ci_upper, df$auc), na.rm = TRUE) + 0.05)

                p <- p +
                    ggplot2::coord_cartesian(xlim = c(x_lo, x_hi)) +
                    ggtheme
                print(p)
            } else {
                plot(df$auc, seq_along(df$auc), pch = 19, xlim = c(0, 1),
                     xlab = metric_label, ylab = "", yaxt = "n",
                     main = "Center-Specific Performance")
                axis(2, at = seq_along(df$auc), labels = df$center, las = 1)
                abline(v = pooled, lty = 2, col = "red")
            }
            TRUE
        }
    )
)
