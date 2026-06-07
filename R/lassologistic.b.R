#' @title LASSO Logistic Regression for Binary Classification
#' @description
#' Performs LASSO-penalized logistic regression for variable selection in binary
#' classification problems. Supports LASSO, Ridge, and Elastic Net penalties.
#' Includes suitability assessment, bootstrap validation, ROC analysis, and
#' automated scoring system generation.
#'
#' @importFrom R6 R6Class
#' @import jmvcore

lassologisticClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "lassologisticClass",
    inherit = lassologisticBase,
    private = list(

        # ══════════════════════════════════════════════════════════════════
        # Notice collection (HTML-based)
        # jmvcore::Notice objects inserted via self$results$insert() carry
        # function references that break jamovi's protobuf serialization
        # ("attempt to apply non-function"). Collect notices here and render
        # them into a single Html output item instead. See waterfall.b.R.
        # ══════════════════════════════════════════════════════════════════
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content)
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) return()

            typeStyles <- list(
                ERROR          = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5"),
                STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74"),
                WARNING        = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047"),
                INFO           = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd")
            )

            html <- "<div style='margin: 10px 0;'>"
            for (notice in private$.noticeList) {
                style <- typeStyles[[notice$type]] %||% typeStyles$INFO
                html <- paste0(html,
                    "<div style='background-color: ", style$bgcolor, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: ", style$color, ";'>",
                    htmltools::htmlEscape(notice$title), "</strong><br>",
                    "<span style='color: #374151;'>",
                    htmltools::htmlEscape(notice$content), "</span>",
                    "</div>")
            }
            html <- paste0(html, "</div>")
            self$results$notices$setContent(html)
        },

        .init = function() {
            if (!requireNamespace("glmnet", quietly = TRUE)) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'><h4>Missing Dependency</h4>",
                    "<p>Package 'glmnet' is required. Install with: install.packages('glmnet')</p></div>"
                ))
                return()
            }

            if (is.null(self$options$outcome) ||
                is.null(self$options$explanatory) ||
                length(self$options$explanatory) == 0) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-info'>",
                    "<h4>", .("Welcome to LASSO Logistic Regression"), "</h4>",
                    "<p>", .("Penalized logistic regression for automatic feature selection in binary classification."), "</p>",
                    "<h5>", .("Required inputs:"), "</h5>",
                    "<ul>",
                    "<li><strong>", .("Binary Outcome"), "</strong>: ", .("Categorical variable with two levels (e.g., PanNET G3 vs PanNEC)"), "</li>",
                    "<li><strong>", .("Explanatory Variables"), "</strong>: ", .("At least 2 candidate predictors"), "</li>",
                    "</ul>",
                    "<h5>", .("Features:"), "</h5>",
                    "<ul>",
                    "<li>", .("LASSO, Ridge, or Elastic Net penalty"), "</li>",
                    "<li>", .("Cross-validated lambda selection"), "</li>",
                    "<li>", .("Bootstrap optimism-corrected validation"), "</li>",
                    "<li>", .("Automated scoring system generation"), "</li>",
                    "<li>", .("ROC curve with AUC and confidence interval"), "</li>",
                    "</ul></div>"
                ))
                return()
            }

            if (!is.null(self$results$predictions) && !is.null(self$data) && nrow(self$data) > 0) {
                self$results$predictions$setValues(rep(NA_real_, nrow(self$data)))
            }
        },

        .run = function() {
            if (is.null(self$options$outcome) ||
                is.null(self$options$explanatory) ||
                length(self$options$explanatory) < 2) return()

            set.seed(self$options$random_seed)

            # ── 1. Clean data ──────────────────────────────────────────────
            data <- tryCatch(private$.cleanData(), error = function(e) {
                msg_html <- htmltools::htmlEscape(e$message)
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'><h4>Data Error</h4><p>", msg_html, "</p></div>"))
                private$.addNotice("ERROR", .("Data Error"),
                    sprintf(.('Data preparation failed: %s'), e$message))
                private$.renderNotices()
                return(NULL)
            })
            if (is.null(data)) return()

            self$results$todo$setContent("")

            # ── 2. Suitability assessment ──────────────────────────────────
            if (self$options$suitabilityCheck) {
                private$.suitabilityAssessment(data)
            }

            private$.checkpoint()

            # ── 3. Fit LASSO model ─────────────────────────────────────────
            fit_result <- tryCatch(private$.fitLasso(data), error = function(e) {
                msg_html <- htmltools::htmlEscape(e$message)
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'><h4>Model Fitting Error</h4><p>", msg_html, "</p></div>"))
                private$.addNotice("ERROR", .("Model Fitting Error"),
                    sprintf(.('LASSO model fitting failed: %s'), e$message))
                private$.renderNotices()
                return(NULL)
            })
            if (is.null(fit_result)) return()

            # ── Notice: no variables selected ──────────────────────────────
            if (length(fit_result$selected) == 0) {
                private$.addNotice("STRONG_WARNING", .("No Variables Selected"),
                    .('LASSO selected zero variables at the chosen lambda. Try lambda.min instead of lambda.1se, or add more predictors.'))
            }

            private$.checkpoint()

            # ── 4. Populate results ────────────────────────────────────────
            private$.populateModelSummary(data, fit_result)
            private$.populateCoefficients(fit_result)
            private$.populatePerformance(data, fit_result)

            # ── 5. Scoring system ──────────────────────────────────────────
            if (self$options$scoringSystem) {
                private$.populateScoringSystem(data, fit_result)
            }

            private$.checkpoint()

            # ── 6. Bootstrap validation ────────────────────────────────────
            if (self$options$bootstrapValidation) {
                private$.bootstrapValidation(data, fit_result)
            }

            # ── 7. Save plot data (plain numerics to avoid protobuf errors) ──
            private$.savePlotData(data, fit_result)

            # ── 8. Predictions output ──────────────────────────────────────
            if (!is.null(self$results$predictions)) {
                pred_full <- rep(NA_real_, nrow(self$data))
                pred_full[data$complete_idx] <- fit_result$probabilities
                self$results$predictions$setValues(pred_full)
            }

            # ── 9. Variable importance ─────────────────────────────────────
            if (self$options$showVariableImportance) {
                private$.populateVariableImportance(data, fit_result)
            }

            # ── 10. Model comparison ───────────────────────────────────────
            if (self$options$showModelComparison) {
                private$.populateModelComparison(data, fit_result)
            }

            # ── 11. Explanatory outputs ────────────────────────────────────
            if (self$options$showSummary) private$.populateSummary(data, fit_result)
            if (self$options$showExplanations) private$.populateExplanations()
            if (self$options$showMethodologyNotes) private$.populateMethodologyNotes()
            if (self$options$includeClinicalGuidance) private$.populateClinicalGuidance()

            # ── 12. Completion notice ──────────────────────────────────────
            n_sel <- length(fit_result$selected)
            private$.addNotice("INFO", .("Analysis Complete"),
                sprintf(.('LASSO logistic regression completed: %d/%d predictors selected using %s penalty with %s lambda (N=%d, %d events).'),
                    n_sel, data$p, self$options$penalty, self$options$lambda, data$n, data$n_events))

            # ── 13. Render all collected notices as HTML ───────────────────
            private$.renderNotices()
        },

        # ══════════════════════════════════════════════════════════════════
        # Data cleaning (adapted from lassocox)
        # ══════════════════════════════════════════════════════════════════
        .cleanData = function() {
            data <- self$data
            outcome_var <- self$options$outcome
            explanatory_vars <- self$options$explanatory

            if (length(explanatory_vars) < 2) {
                jmvcore::reject(.("At least 2 explanatory variables are required for LASSO regression."))
            }

            outcome_raw <- data[[outcome_var]]
            predictors <- data[explanatory_vars]

            # Determine event coding
            if (is.factor(outcome_raw) || is.character(outcome_raw)) {
                outcome_chr <- as.character(outcome_raw)
                observed_levels <- sort(unique(outcome_chr[!is.na(outcome_chr)]))
                if (length(observed_levels) < 2) {
                    jmvcore::reject(.("Outcome variable must have at least 2 observed values."))
                }
                outcome_level_opt <- self$options$outcomeLevel
                if (is.null(outcome_level_opt) || !nzchar(as.character(outcome_level_opt))) {
                    event_level <- observed_levels[2]
                } else {
                    event_level <- as.character(outcome_level_opt)
                }
                ref_level <- setdiff(observed_levels, event_level)[1]
                status <- rep(NA_real_, length(outcome_chr))
                status[outcome_chr == event_level] <- 1
                status[outcome_chr == ref_level] <- 0
            } else {
                outcome_num <- jmvcore::toNumeric(outcome_raw)
                observed_levels <- sort(unique(outcome_num[!is.na(outcome_num)]))
                if (length(observed_levels) < 2) {
                    jmvcore::reject(.("Numeric outcome must have at least 2 observed values."))
                }
                outcome_level_opt <- self$options$outcomeLevel
                if (!is.null(outcome_level_opt) && nzchar(as.character(outcome_level_opt))) {
                    event_level_num <- suppressWarnings(as.numeric(outcome_level_opt))
                } else if (all(observed_levels %in% c(0, 1))) {
                    event_level_num <- 1
                } else {
                    event_level_num <- max(observed_levels)
                }
                ref_level_num <- setdiff(observed_levels, event_level_num)[1]
                event_level <- as.character(event_level_num)
                ref_level <- as.character(ref_level_num)
                status <- rep(NA_real_, length(outcome_num))
                status[outcome_num == event_level_num] <- 1
                status[outcome_num == ref_level_num] <- 0
            }

            # Remove constant variables
            constant_vars <- sapply(predictors, function(x) {
                if (is.numeric(x)) {
                    v <- var(x, na.rm = TRUE)
                    is.na(v) || v == 0
                } else {
                    length(unique(na.omit(x))) <= 1
                }
            })
            if (any(constant_vars)) {
                predictors <- predictors[, !constant_vars, drop = FALSE]
                explanatory_vars <- names(predictors)
            }
            if (ncol(predictors) < 2) jmvcore::reject(.("Fewer than 2 non-constant predictors remain."))

            # Complete-case filtering
            complete <- complete.cases(status, predictors)
            n_complete <- sum(complete)
            if (n_complete < 10) jmvcore::reject(.("Too few complete cases for analysis."))

            status_cc <- status[complete]
            if (!(length(unique(status_cc)) == 2 && all(unique(status_cc) %in% c(0, 1)))) {
                jmvcore::reject(.("Outcome is not binary after filtering."))
            }

            n_events <- sum(status_cc == 1)
            n_nonevents <- sum(status_cc == 0)
            if (n_events < 5 || n_nonevents < 5) {
                jmvcore::reject(.("Need at least 5 cases in each outcome class."))
            }

            # Build design matrix
            pred_cc <- predictors[complete, , drop = FALSE]
            X <- tryCatch({
                mm <- model.matrix(~ ., data = pred_cc)
                mm[, -1, drop = FALSE]  # remove intercept
            }, error = function(e) {
                jmvcore::reject("Design matrix error: {}", e$message)
            })

            # Remove degenerate columns
            col_vars <- apply(X, 2, var, na.rm = TRUE)
            good_cols <- !is.na(col_vars) & col_vars > 0
            if (sum(good_cols) < 2) jmvcore::reject(.("Too few non-degenerate predictor columns."))
            X <- X[, good_cols, drop = FALSE]

            # Optional standardization
            if (self$options$standardize) {
                X <- scale(X)
            }

            list(
                X = X,
                y = status_cc,
                n = n_complete,
                n_events = n_events,
                n_nonevents = n_nonevents,
                p = ncol(X),
                complete_idx = which(complete),
                event_level = event_level,
                ref_level = ref_level,
                explanatory_vars = explanatory_vars,
                col_names = colnames(X)
            )
        },

        # ══════════════════════════════════════════════════════════════════
        # Suitability assessment
        # ══════════════════════════════════════════════════════════════════
        .suitabilityAssessment = function(data) {
            epv <- min(data$n_events, data$n_nonevents) / data$p
            checks <- list()

            # EPV check
            if (epv >= 10) {
                checks$epv <- list(status = "green", label = "Events per variable",
                    detail = sprintf("EPV = %.1f (≥10: adequate)", epv))
            } else if (epv >= 5) {
                checks$epv <- list(status = "yellow", label = "Events per variable",
                    detail = sprintf("EPV = %.1f (5-10: marginal, results may be unstable)", epv))
            } else {
                checks$epv <- list(status = "red", label = "Events per variable",
                    detail = sprintf("EPV = %.1f (<5: insufficient, high overfitting risk)", epv))
            }

            # Sample size check
            if (data$n >= 100) {
                checks$n <- list(status = "green", label = "Sample size",
                    detail = sprintf("N = %d (≥100: adequate)", data$n))
            } else if (data$n >= 50) {
                checks$n <- list(status = "yellow", label = "Sample size",
                    detail = sprintf("N = %d (50-100: marginal)", data$n))
            } else {
                checks$n <- list(status = "red", label = "Sample size",
                    detail = sprintf("N = %d (<50: small, consider fewer predictors)", data$n))
            }

            # Class balance
            minority_pct <- min(data$n_events, data$n_nonevents) / data$n * 100
            if (minority_pct >= 30) {
                checks$balance <- list(status = "green", label = "Class balance",
                    detail = sprintf("Minority class: %.1f%% (balanced)", minority_pct))
            } else if (minority_pct >= 10) {
                checks$balance <- list(status = "yellow", label = "Class balance",
                    detail = sprintf("Minority class: %.1f%% (moderate imbalance)", minority_pct))
            } else {
                checks$balance <- list(status = "red", label = "Class balance",
                    detail = sprintf("Minority class: %.1f%% (severe imbalance)", minority_pct))
            }

            # Predictor count
            if (data$p <= data$n / 5) {
                checks$p <- list(status = "green", label = "Predictor count",
                    detail = sprintf("p = %d predictors, n/p = %.1f (good ratio)", data$p, data$n / data$p))
            } else {
                checks$p <- list(status = "yellow", label = "Predictor count",
                    detail = sprintf("p = %d predictors, n/p = %.1f (regularization essential)", data$p, data$n / data$p))
            }

            # Collinearity check
            cor_matrix <- tryCatch(cor(data$X, use = "pairwise.complete.obs"), error = function(e) NULL)
            max_cor <- NA
            if (!is.null(cor_matrix)) {
                diag(cor_matrix) <- 0
                max_cor <- max(abs(cor_matrix), na.rm = TRUE)
                if (max_cor < 0.7) {
                    checks$collinearity <- list(status = "green", label = "Collinearity",
                        detail = sprintf("Max |r| = %.2f (<0.7: acceptable)", max_cor))
                } else if (max_cor < 0.9) {
                    checks$collinearity <- list(status = "yellow", label = "Collinearity",
                        detail = sprintf("Max |r| = %.2f (0.7-0.9: moderate, LASSO will handle)", max_cor))
                } else {
                    checks$collinearity <- list(status = "red", label = "Collinearity",
                        detail = sprintf("Max |r| = %.2f (≥0.9: high, consider elastic net)", max_cor))
                }
            }

            # Render HTML
            icons <- c(green = "&#x2705;", yellow = "&#x26A0;&#xFE0F;", red = "&#x274C;")
            n_green <- sum(sapply(checks, function(x) x$status == "green"))
            n_yellow <- sum(sapply(checks, function(x) x$status == "yellow"))
            n_red <- sum(sapply(checks, function(x) x$status == "red"))

            if (n_red > 0) {
                overall <- "<span style='color:red;font-weight:bold;'>Caution: Major concerns detected</span>"
            } else if (n_yellow > 0) {
                overall <- "<span style='color:#cc8800;font-weight:bold;'>Acceptable with caveats</span>"
            } else {
                overall <- "<span style='color:green;font-weight:bold;'>Data suitable for LASSO logistic</span>"
            }

            rows <- sapply(checks, function(x) {
                sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>",
                        icons[x$status], x$label, x$detail)
            })

            html <- paste0(
                "<h4>Data Suitability Assessment</h4>",
                "<p>Overall: ", overall, "</p>",
                "<table class='table table-condensed'><thead>",
                "<tr><th></th><th>Check</th><th>Result</th></tr></thead><tbody>",
                paste(rows, collapse = ""),
                "</tbody></table>"
            )
            self$results$suitabilityReport$setContent(html)

            # Surface critical suitability issues as Notices
            if (n_red > 0) {
                red_items <- paste(sapply(checks[sapply(checks, function(x) x$status == "red")],
                    function(x) x$label), collapse = "; ")
                private$.addNotice("STRONG_WARNING", .("Data Suitability"),
                    sprintf(.('Data suitability: %d major concern(s) detected (%s). Results may be unreliable; consider reducing predictors or collecting more data.'),
                            n_red, red_items))
            } else if (n_yellow > 0) {
                private$.addNotice("WARNING", .("Data Suitability"),
                    sprintf(.('Data suitability: %d minor concern(s). Enable bootstrap validation to assess overfitting risk.'),
                            n_yellow))
            }
        },

        # ══════════════════════════════════════════════════════════════════
        # Fit LASSO logistic regression
        # ══════════════════════════════════════════════════════════════════
        .fitLasso = function(data) {
            # Determine alpha
            alpha_val <- switch(self$options$penalty,
                "lasso" = 1,
                "ridge" = 0,
                "elasticnet" = self$options$alpha,
                1
            )

            # Adjust nfolds if needed
            nfolds <- min(self$options$nfolds, data$n - 1)
            nfolds <- max(nfolds, 3)

            # Stratified CV folds for balanced sampling
            foldid <- NULL
            tryCatch({
                pos_idx <- which(data$y == 1)
                neg_idx <- which(data$y == 0)
                folds_pos <- sample(rep(seq_len(nfolds), length.out = length(pos_idx)))
                folds_neg <- sample(rep(seq_len(nfolds), length.out = length(neg_idx)))
                foldid <- integer(data$n)
                foldid[pos_idx] <- folds_pos
                foldid[neg_idx] <- folds_neg
            }, error = function(e) { foldid <<- NULL })

            # Fit CV model
            cv_args <- list(
                x = data$X,
                y = data$y,
                family = "binomial",
                alpha = alpha_val,
                standardize = FALSE,  # already standardized if requested
                type.measure = "deviance"
            )
            if (!is.null(foldid)) {
                cv_args$foldid <- foldid
            } else {
                cv_args$nfolds <- nfolds
            }

            cv_fit <- do.call(glmnet::cv.glmnet, cv_args)

            # Select lambda
            lambda_optimal <- switch(self$options$lambda,
                "lambda.min" = cv_fit$lambda.min,
                "lambda.1se" = cv_fit$lambda.1se,
                cv_fit$lambda.1se
            )

            # Final model
            final_model <- glmnet::glmnet(
                x = data$X,
                y = data$y,
                family = "binomial",
                alpha = alpha_val,
                lambda = lambda_optimal,
                standardize = FALSE
            )

            # Extract coefficients
            coefs <- as.matrix(coef(final_model, s = lambda_optimal))
            intercept <- coefs[1, 1]
            beta <- coefs[-1, 1]
            selected <- names(beta)[beta != 0]
            selected_coefs <- beta[beta != 0]

            # Predicted probabilities
            probabilities <- as.numeric(predict(final_model, newx = data$X,
                                                s = lambda_optimal, type = "response"))

            list(
                cv_fit = cv_fit,
                final_model = final_model,
                lambda = lambda_optimal,
                alpha = alpha_val,
                intercept = intercept,
                beta = beta,
                selected = selected,
                selected_coefs = selected_coefs,
                probabilities = probabilities,
                nfolds = nfolds
            )
        },

        # ══════════════════════════════════════════════════════════════════
        # Populate results tables
        # ══════════════════════════════════════════════════════════════════
        .populateModelSummary = function(data, fit) {
            table <- self$results$modelSummary
            rows <- list(
                list("Total observations", as.character(data$n)),
                list("Event class (positive)", paste0(data$event_level, " (n=", data$n_events, ")")),
                list("Reference class", paste0(data$ref_level, " (n=", data$n_nonevents, ")")),
                list("Candidate predictors", as.character(data$p)),
                list("Selected predictors", as.character(length(fit$selected))),
                list("Penalty type", self$options$penalty),
                list("Alpha", sprintf("%.2f", fit$alpha)),
                list("Lambda (optimal)", sprintf("%.4f", fit$lambda)),
                list("Lambda selection", self$options$lambda),
                list("CV folds", as.character(fit$nfolds))
            )
            for (i in seq_along(rows)) {
                table$addRow(rowKey = i, values = list(statistic = rows[[i]][[1]], value = rows[[i]][[2]]))
            }
        },

        .populateCoefficients = function(fit) {
            table <- self$results$coefficients
            if (length(fit$selected) == 0) {
                table$addRow(rowKey = 1, values = list(
                    variable = "No variables selected",
                    coefficient = NA, oddsRatio = NA,
                    ci_lower = NA, ci_upper = NA, importance = NA
                ))
                return()
            }

            max_abs <- max(abs(fit$selected_coefs))
            for (i in seq_along(fit$selected)) {
                coef_val <- fit$selected_coefs[i]
                or_val <- exp(coef_val)
                importance <- abs(coef_val) / max_abs

                table$addRow(rowKey = i, values = list(
                    variable = fit$selected[i],
                    coefficient = coef_val,
                    oddsRatio = or_val,
                    ci_lower = NA,  # LASSO doesn't produce CIs directly
                    ci_upper = NA,
                    importance = importance
                ))
            }
            table$setNote("ci_note", .("Note: LASSO coefficients do not have standard CIs. Use bootstrap validation for inference."))
        },

        .populatePerformance = function(data, fit) {
            table <- self$results$performance

            # AUC
            auc_val <- NA
            auc_ci_lower <- NA
            auc_ci_upper <- NA
            roc_obj <- NULL
            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_obj <- pROC::roc(data$y, fit$probabilities, quiet = TRUE)
                    auc_val <- as.numeric(pROC::auc(roc_obj))
                    ci_obj <- pROC::ci.auc(roc_obj, method = "delong")
                    auc_ci_lower <- ci_obj[1]
                    auc_ci_upper <- ci_obj[3]
                }
            }, error = function(e) {})

            # Optimal threshold
            optimal_threshold <- 0.5
            tryCatch({
                if (!is.null(roc_obj)) {
                    coords_best <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
                    optimal_threshold <- coords_best$threshold[1]
                }
            }, error = function(e) {})

            predicted_class <- ifelse(fit$probabilities >= optimal_threshold, 1, 0)
            accuracy <- mean(predicted_class == data$y)
            sensitivity <- sum(predicted_class == 1 & data$y == 1) / sum(data$y == 1)
            specificity <- sum(predicted_class == 0 & data$y == 0) / sum(data$y == 0)

            # Brier score
            brier <- mean((fit$probabilities - data$y)^2)

            # F1
            tp <- sum(predicted_class == 1 & data$y == 1)
            fp <- sum(predicted_class == 1 & data$y == 0)
            fn <- sum(predicted_class == 0 & data$y == 1)
            precision <- if (tp + fp > 0) tp / (tp + fp) else 0
            recall <- sensitivity
            f1 <- if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0

            rows <- list(
                list("AUC (apparent)",
                     if (is.na(auc_val)) "Not available" else sprintf("%.3f (%.3f-%.3f)", auc_val, auc_ci_lower, auc_ci_upper),
                     if (is.na(auc_val)) "AUC could not be computed" else if (auc_val >= 0.9) "Excellent" else if (auc_val >= 0.8) "Good" else if (auc_val >= 0.7) "Acceptable" else "Poor"),
                list("Optimal threshold", sprintf("%.3f", optimal_threshold), "Youden index"),
                list("Accuracy", sprintf("%.3f", accuracy), ""),
                list("Sensitivity (Recall)", sprintf("%.3f", sensitivity), ""),
                list("Specificity", sprintf("%.3f", specificity), ""),
                list("Precision (PPV)", sprintf("%.3f", precision), ""),
                list("F1 Score", sprintf("%.3f", f1), ""),
                list("Brier Score", sprintf("%.4f", brier),
                     if (brier < 0.1) "Excellent calibration" else if (brier < 0.2) "Good" else "Poor")
            )

            for (i in seq_along(rows)) {
                table$addRow(rowKey = i, values = list(
                    metric = rows[[i]][[1]], value = rows[[i]][[2]], interpretation = rows[[i]][[3]]))
            }

            if (!is.na(auc_val) && auc_val > 0.95 && data$n < 100) {
                table$setNote("overfit_warning",
                    .("Warning: Very high apparent AUC with small sample size suggests possible overfitting. Enable bootstrap validation to assess optimism."))
                private$.addNotice("STRONG_WARNING", .("Possible Overfitting"),
                    sprintf(.('Apparent AUC = %.3f with N = %d: likely overfitted. Enable bootstrap validation for corrected estimate.'),
                            auc_val, data$n))
            }

            if (!is.na(auc_val) && auc_val < 0.7) {
                private$.addNotice("WARNING", .("Poor Discrimination"),
                    sprintf(.('AUC = %.3f indicates poor discrimination. Consider adding more informative predictors or using a different model.'),
                            auc_val))
            }
        },

        # ══════════════════════════════════════════════════════════════════
        # Scoring system generation — three methods
        # ══════════════════════════════════════════════════════════════════

        # ── Core: compute integer points by each method ────────────────
        .computePoints = function(coefs, method, max_points = 10) {
            abs_coefs <- abs(coefs)
            signs <- sign(coefs)

            if (method == "beta10") {
                # Zhang et al. 2017: multiply by 10, round
                raw <- coefs * max_points  # scale to max_points (default 10)
                raw <- raw / max(abs(raw)) * max_points
                pts <- round(raw)
                # Ensure minimum 1 point for non-zero coefficients
                pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]

            } else if (method == "schneeweiss") {
                # Mehta et al. 2016: divide by smallest absolute coefficient
                min_abs <- min(abs_coefs[abs_coefs > 0])
                raw <- coefs / min_abs
                pts <- round(raw)
                pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]

            } else if (method == "sullivan") {
                # Sullivan et al. 2004 (Framingham):
                # Choose reference variable W (largest |beta|)
                # For each predictor: Points = beta_i / W * max_points
                # This preserves relative risk relationships
                W <- max(abs_coefs)
                if (W == 0) return(rep(0L, length(coefs)))
                raw <- (coefs / W) * max_points
                pts <- round(raw)
                pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]

            } else {
                pts <- round(coefs * max_points / max(abs_coefs))
            }

            as.integer(pts)
        },

        # ── Compute total scores for a dataset given point assignments ──
        .computeTotalScores = function(data, variables, points) {
            total <- rep(0, data$n)
            for (i in seq_along(variables)) {
                var_col <- variables[i]
                if (var_col %in% colnames(data$X)) {
                    col_vals <- data$X[, var_col]
                    # For binary: score when present; for continuous: multiply
                    if (all(col_vals %in% c(0, 1), na.rm = TRUE)) {
                        total <- total + ifelse(col_vals > 0, points[i], 0)
                    } else {
                        # Continuous predictor: score = points * (value > median)
                        total <- total + ifelse(col_vals > median(col_vals), points[i], 0)
                    }
                }
            }
            total
        },

        # ── Evaluate a scoring system's performance ─────────────────────
        .evaluateScore = function(y, total_scores) {
            # Find optimal cutoff by Youden index
            score_vals <- sort(unique(total_scores))
            best_youden <- -Inf
            best_cutoff <- score_vals[1]
            for (cutoff in score_vals) {
                pred <- ifelse(total_scores >= cutoff, 1, 0)
                sens <- sum(pred == 1 & y == 1) / max(sum(y == 1), 1)
                spec <- sum(pred == 0 & y == 0) / max(sum(y == 0), 1)
                youden <- sens + spec - 1
                if (youden > best_youden) {
                    best_youden <- youden
                    best_cutoff <- cutoff
                }
            }

            pred <- ifelse(total_scores >= best_cutoff, 1, 0)
            tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
            fn <- sum(pred == 0 & y == 1)
            sens <- tp / max(tp + fn, 1)
            spec <- sum(pred == 0 & y == 0) / max(sum(y == 0), 1)
            acc <- mean(pred == y)
            prec <- if (tp + fp > 0) tp / (tp + fp) else 0
            f1 <- if (prec + sens > 0) 2 * prec * sens / (prec + sens) else 0

            auc_val <- NA
            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_obj <- pROC::roc(y, total_scores, quiet = TRUE)
                    auc_val <- as.numeric(pROC::auc(roc_obj))
                }
            }, error = function(e) {})

            list(auc = auc_val, cutoff = best_cutoff, accuracy = acc,
                 sensitivity = sens, specificity = spec,
                 precision = prec, f1 = f1,
                 mean_pos = mean(total_scores[y == 1]),
                 mean_neg = mean(total_scores[y == 0]),
                 range = range(total_scores))
        },

        # ── Main scoring system population ──────────────────────────────
        .populateScoringSystem = function(data, fit) {
            table <- self$results$scoringTable
            perf_table <- self$results$scoringPerformance

            if (length(fit$selected) == 0) return()

            method <- self$options$scoringMethod %||% "schneeweiss"
            max_points <- self$options$scoringMaxPoints

            coefs <- fit$selected_coefs
            vars <- fit$selected

            # Compute points by all three methods
            pts_beta10 <- private$.computePoints(coefs, "beta10", max_points)
            pts_schneeweiss <- private$.computePoints(coefs, "schneeweiss", max_points)
            pts_sullivan <- private$.computePoints(coefs, "sullivan", max_points)

            # Select primary method's points
            pts_primary <- switch(method,
                "beta10" = pts_beta10,
                "schneeweiss" = pts_schneeweiss,
                "sullivan" = pts_sullivan,
                "compare" = pts_schneeweiss,  # default to Schneeweiss for primary
                pts_schneeweiss
            )

            # Build score data
            score_data <- data.frame(
                variable = vars,
                coefficient = coefs,
                oddsRatio = exp(coefs),
                direction = ifelse(coefs > 0, .("Positive (+)"), .("Negative (-)")),
                points_beta10 = pts_beta10,
                points_schneeweiss = pts_schneeweiss,
                points_sullivan = pts_sullivan,
                points = pts_primary,
                stringsAsFactors = FALSE
            )

            # Sort by absolute points descending
            score_data <- score_data[order(-abs(score_data$points)), ]

            for (i in seq_len(nrow(score_data))) {
                table$addRow(rowKey = i, values = list(
                    variable = score_data$variable[i],
                    oddsRatio = score_data$oddsRatio[i],
                    direction = score_data$direction[i],
                    points_beta10 = score_data$points_beta10[i],
                    points_schneeweiss = score_data$points_schneeweiss[i],
                    points_sullivan = score_data$points_sullivan[i],
                    points = score_data$points[i]
                ))
            }

            # Add method reference note
            method_refs <- list(
                beta10 = .("Beta10 method (Zhang et al. Ann Transl Med 2017)"),
                schneeweiss = .("Schneeweiss method (Mehta et al. J Clin Epidemiol 2016)"),
                sullivan = .("Sullivan/D'Agostino method (Statistics in Medicine 2004)"),
                compare = .("All three methods shown for comparison")
            )
            table$setNote("method", method_refs[[method]])

            # Evaluate primary scoring system
            total_scores <- private$.computeTotalScores(data, vars, pts_primary)
            perf <- private$.evaluateScore(data$y, total_scores)

            perf_rows <- list(
                list(.("Scoring method"), switch(method,
                    "beta10" = "Beta10", "schneeweiss" = "Schneeweiss",
                    "sullivan" = "Sullivan/D'Agostino", "compare" = "Schneeweiss (primary)")),
                list(.("Score AUC"), sprintf("%.3f", perf$auc)),
                list(.("Optimal score cutoff"), as.character(perf$cutoff)),
                list(.("Accuracy"), sprintf("%.3f", perf$accuracy)),
                list(.("Sensitivity"), sprintf("%.3f", perf$sensitivity)),
                list(.("Specificity"), sprintf("%.3f", perf$specificity)),
                list(.("Precision"), sprintf("%.3f", perf$precision)),
                list(.("F1 Score"), sprintf("%.3f", perf$f1)),
                list(.("Mean score (positive class)"), sprintf("%.2f", perf$mean_pos)),
                list(.("Mean score (reference class)"), sprintf("%.2f", perf$mean_neg)),
                list(.("Score range"), sprintf("%d to %d", perf$range[1], perf$range[2]))
            )

            for (i in seq_along(perf_rows)) {
                perf_table$addRow(rowKey = i, values = list(
                    metric = perf_rows[[i]][[1]], value = perf_rows[[i]][[2]]))
            }

            perf_table$setNote("dichotomization",
                .("Continuous predictors are scored by dichotomizing at their median (1 point block above the median, 0 below). The performance shown here reflects this simplified integer point system and may differ from the continuous LASSO model in the Classification Performance table."))

            # ── Method comparison (when compare mode selected) ──────────
            if (method == "compare") {
                comp_table <- self$results$methodComparison

                # Full model AUC for reference
                full_auc <- NA
                tryCatch({
                    if (requireNamespace("pROC", quietly = TRUE)) {
                        full_auc <- as.numeric(pROC::auc(pROC::roc(data$y, fit$probabilities, quiet = TRUE)))
                    }
                }, error = function(e) {})

                methods_list <- list(
                    list("Beta10", pts_beta10, "Zhang et al. 2017"),
                    list("Schneeweiss", pts_schneeweiss, "Mehta et al. 2016"),
                    list("Sullivan/D'Agostino", pts_sullivan, "Sullivan et al. 2004")
                )

                for (j in seq_along(methods_list)) {
                    m <- methods_list[[j]]
                    scores_j <- private$.computeTotalScores(data, vars, m[[2]])
                    perf_j <- private$.evaluateScore(data$y, scores_j)

                    info_loss <- if (!is.na(full_auc) && !is.na(perf_j$auc) && full_auc > 0) {
                        (1 - perf_j$auc / full_auc) * 100
                    } else NA

                    comp_table$addRow(rowKey = j, values = list(
                        method = m[[1]],
                        auc = perf_j$auc,
                        accuracy = perf_j$accuracy,
                        info_loss = info_loss,
                        reference = m[[3]]
                    ))
                }

                # Add full model as reference row
                comp_table$addRow(rowKey = 4, values = list(
                    method = .("Full LASSO model (continuous)"),
                    auc = full_auc,
                    accuracy = NA,
                    info_loss = 0,
                    reference = .("Reference (no rounding)")
                ))
            }

            # ── Score-to-probability lookup table ───────────────────────
            if (isTRUE(self$options$scoreLookupTable)) {
                private$.populateLookupTable(data, total_scores, perf$cutoff)
            }
        },

        # ── Score-to-probability lookup table ───────────────────────────
        .populateLookupTable = function(data, total_scores, cutoff) {
            lookup_table <- self$results$lookupTable
            score_vals <- sort(unique(total_scores))

            for (s in score_vals) {
                idx <- total_scores == s
                n_cases <- sum(idx)
                n_events <- sum(data$y[idx] == 1)
                prob <- n_events / n_cases

                risk_group <- if (s >= cutoff) .("High risk") else .("Low risk")

                lookup_table$addRow(rowKey = as.character(s), values = list(
                    score = as.integer(s),
                    n_cases = as.integer(n_cases),
                    n_events = as.integer(n_events),
                    probability = prob,
                    risk_group = risk_group
                ))
            }
        },

        # ══════════════════════════════════════════════════════════════════
        # Bootstrap internal validation (Harrell method)
        # ══════════════════════════════════════════════════════════════════
        .bootstrapValidation = function(data, fit) {
            table <- self$results$validationTable
            B <- self$options$bootstrapN

            alpha_val <- fit$alpha

            # Apparent performance
            apparent_auc <- NA
            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_app <- pROC::roc(data$y, fit$probabilities, quiet = TRUE)
                    apparent_auc <- as.numeric(pROC::auc(roc_app))
                }
            }, error = function(e) {})

            apparent_brier <- mean((fit$probabilities - data$y)^2)

            # Bootstrap optimism estimation
            optimism_auc <- rep(NA_real_, B)
            optimism_brier <- rep(NA_real_, B)

            for (b in seq_len(B)) {
                tryCatch({
                    idx <- sample(data$n, replace = TRUE)
                    X_boot <- data$X[idx, , drop = FALSE]
                    y_boot <- data$y[idx]

                    # Skip if boot sample not binary
                    if (length(unique(y_boot)) < 2) next

                    # Fit on bootstrap
                    nfolds_boot <- min(fit$nfolds, length(unique(idx)) - 1)
                    nfolds_boot <- max(nfolds_boot, 3)

                    cv_boot <- glmnet::cv.glmnet(
                        x = X_boot, y = y_boot,
                        family = "binomial", alpha = alpha_val,
                        nfolds = nfolds_boot, standardize = FALSE
                    )
                    lambda_boot <- switch(self$options$lambda,
                        "lambda.min" = cv_boot$lambda.min,
                        "lambda.1se" = cv_boot$lambda.1se,
                        cv_boot$lambda.1se
                    )

                    # Predict on bootstrap sample and original
                    prob_boot_boot <- as.numeric(predict(cv_boot, newx = X_boot,
                                                         s = lambda_boot, type = "response"))
                    prob_boot_orig <- as.numeric(predict(cv_boot, newx = data$X,
                                                         s = lambda_boot, type = "response"))

                    if (requireNamespace("pROC", quietly = TRUE)) {
                        auc_boot_boot <- as.numeric(pROC::auc(pROC::roc(y_boot, prob_boot_boot, quiet = TRUE)))
                        auc_boot_orig <- as.numeric(pROC::auc(pROC::roc(data$y, prob_boot_orig, quiet = TRUE)))
                        optimism_auc[b] <- auc_boot_boot - auc_boot_orig
                    }

                    brier_boot_boot <- mean((prob_boot_boot - y_boot)^2)
                    brier_boot_orig <- mean((prob_boot_orig - data$y)^2)
                    optimism_brier[b] <- brier_boot_boot - brier_boot_orig  # negative optimism for Brier

                }, error = function(e) {})
            }

            # Compute corrected metrics (NA-based tracking avoids excluding legitimate zero-optimism samples)
            mean_optimism_auc <- mean(optimism_auc, na.rm = TRUE)
            mean_optimism_brier <- mean(optimism_brier, na.rm = TRUE)
            corrected_auc <- apparent_auc - mean_optimism_auc
            corrected_brier <- apparent_brier - mean_optimism_brier

            rows <- list(
                list("AUC", apparent_auc, mean_optimism_auc, corrected_auc),
                list("Brier Score", apparent_brier, mean_optimism_brier, corrected_brier)
            )

            for (i in seq_along(rows)) {
                table$addRow(rowKey = i, values = list(
                    metric = rows[[i]][[1]],
                    apparent = rows[[i]][[2]],
                    optimism = rows[[i]][[3]],
                    corrected = rows[[i]][[4]]
                ))
            }

            if (!is.na(mean_optimism_auc) && mean_optimism_auc > 0.05) {
                table$setNote("optimism_warning",
                    sprintf(.("Optimism = %.3f indicates overfitting. The corrected AUC (%.3f) is a more realistic estimate of future performance."),
                            mean_optimism_auc, corrected_auc))
            }
        },

        # ══════════════════════════════════════════════════════════════════
        # Variable importance
        # ══════════════════════════════════════════════════════════════════
        .populateVariableImportance = function(data, fit) {
            table <- self$results$variableImportance

            # Calculate inclusion proportion across lambda path
            lambda_path <- fit$cv_fit$glmnet.fit$lambda
            all_coefs <- as.matrix(coef(fit$cv_fit$glmnet.fit))[-1, , drop = FALSE]

            inclusion_prop <- rowMeans(all_coefs != 0)
            max_abs <- apply(abs(all_coefs), 1, max)

            imp_df <- data.frame(
                variable = rownames(all_coefs),
                importance_score = max_abs,
                selection_frequency = inclusion_prop,
                stringsAsFactors = FALSE
            )
            imp_df <- imp_df[order(-imp_df$importance_score), ]
            imp_df$stability_rank <- seq_len(nrow(imp_df))

            for (i in seq_len(min(nrow(imp_df), 20))) {
                table$addRow(rowKey = i, values = list(
                    variable = imp_df$variable[i],
                    importance_score = imp_df$importance_score[i],
                    selection_frequency = imp_df$selection_frequency[i],
                    stability_rank = imp_df$stability_rank[i]
                ))
            }
        },

        # ══════════════════════════════════════════════════════════════════
        # Model comparison
        # ══════════════════════════════════════════════════════════════════
        .populateModelComparison = function(data, fit) {
            table <- self$results$modelComparison

            # Standard logistic with selected variables
            if (length(fit$selected) > 0) {
                X_sel <- data$X[, fit$selected, drop = FALSE]
                df_sel <- data.frame(y = data$y, X_sel)
                tryCatch({
                    glm_sel <- glm(y ~ ., data = df_sel, family = binomial)
                    prob_sel <- predict(glm_sel, type = "response")
                    auc_sel <- NA
                    if (requireNamespace("pROC", quietly = TRUE)) {
                        auc_sel <- as.numeric(pROC::auc(pROC::roc(data$y, prob_sel, quiet = TRUE)))
                    }
                    brier_sel <- mean((prob_sel - data$y)^2)
                    table$addRow(rowKey = 1, values = list(
                        model_type = "LASSO (selected vars)",
                        n_variables = length(fit$selected),
                        auc = auc_sel,
                        aic = AIC(glm_sel),
                        brier = brier_sel
                    ))
                }, error = function(e) {})
            }

            # Full logistic with all variables
            tryCatch({
                df_all <- data.frame(y = data$y, data$X)
                glm_all <- glm(y ~ ., data = df_all, family = binomial)
                prob_all <- predict(glm_all, type = "response")
                auc_all <- NA
                if (requireNamespace("pROC", quietly = TRUE)) {
                    auc_all <- as.numeric(pROC::auc(pROC::roc(data$y, prob_all, quiet = TRUE)))
                }
                brier_all <- mean((prob_all - data$y)^2)
                table$addRow(rowKey = 2, values = list(
                    model_type = "Standard logistic (all vars)",
                    n_variables = ncol(data$X),
                    auc = auc_all,
                    aic = AIC(glm_all),
                    brier = brier_all
                ))
            }, error = function(e) {})
        },

        # ══════════════════════════════════════════════════════════════════
        # Save plain plot data (avoids protobuf serialization of glmnet objects)
        # ══════════════════════════════════════════════════════════════════
        .savePlotData = function(data, fit) {
            # CV plot: save plain numeric vectors
            if (self$options$cv_plot) {
                cv_state <- list(
                    lambda     = as.numeric(fit$cv_fit$lambda),
                    cvm        = as.numeric(fit$cv_fit$cvm),
                    cvsd       = as.numeric(fit$cv_fit$cvsd),
                    cvup       = as.numeric(fit$cv_fit$cvup),
                    cvlo       = as.numeric(fit$cv_fit$cvlo),
                    lambda_min = as.numeric(fit$cv_fit$lambda.min),
                    lambda_1se = as.numeric(fit$cv_fit$lambda.1se),
                    nzero      = as.integer(fit$cv_fit$nzero)
                )
                self$results$cv_plot$setState(cv_state)
            }

            # Coefficient plot: save selected variable names and coefficients
            if (self$options$coef_plot && length(fit$selected) > 0) {
                ord <- order(abs(fit$selected_coefs))
                coef_state <- list(
                    var_names    = as.character(fit$selected[ord]),
                    coef_values  = as.numeric(fit$selected_coefs[ord])
                )
                self$results$coef_plot$setState(coef_state)
            } else if (self$options$coef_plot) {
                self$results$coef_plot$setState(NULL)
            }

            # ROC plot: save response and probabilities
            if (self$options$roc_plot) {
                roc_state <- as.data.frame(list(
                    y             = as.integer(data$y),
                    probabilities = as.numeric(fit$probabilities)
                ))
                self$results$roc_plot$setState(roc_state)
            }
        },

        # ══════════════════════════════════════════════════════════════════
        # Plot render functions (read from state, no re-fitting)
        # ══════════════════════════════════════════════════════════════════
        .cvPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

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
                    title = .("Cross-Validation for LASSO Logistic Regression"),
                    subtitle = .("Blue: lambda.min, Green: lambda.1se"),
                    x = .("Log Lambda"),
                    y = .("Binomial Deviance")
                ) +
                ggtheme

            print(p)
            TRUE
        },

        .coefPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || length(state$var_names) == 0) return(FALSE)

            df <- data.frame(
                variable    = factor(state$var_names, levels = state$var_names),
                coefficient = state$coef_values,
                direction   = ifelse(state$coef_values > 0, "Positive", "Negative")
            )

            p <- ggplot2::ggplot(df, ggplot2::aes(x = variable, y = coefficient, fill = direction)) +
                ggplot2::geom_col() +
                ggplot2::coord_flip() +
                ggplot2::scale_fill_manual(values = c("Positive" = "#E74C3C", "Negative" = "#2E86C1")) +
                ggplot2::labs(title = .("LASSO Logistic Regression Coefficients"),
                             x = "", y = .("Coefficient"), fill = .("Direction")) +
                ggtheme
            print(p)
            TRUE
        },

        .rocPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)
            if (!requireNamespace("pROC", quietly = TRUE)) return(FALSE)

            roc_obj <- pROC::roc(state$y, state$probabilities, quiet = TRUE)
            auc_val <- sprintf("%.3f", as.numeric(pROC::auc(roc_obj)))
            ci_obj <- tryCatch(pROC::ci.auc(roc_obj, method = "delong"), error = function(e) NULL)

            plot(roc_obj, main = paste0("ROC Curve (AUC = ", auc_val, ")"),
                 col = "#2E86C1", lwd = 2, print.auc = FALSE)
            if (!is.null(ci_obj)) {
                legend("bottomright",
                       legend = sprintf("AUC = %s (95%% CI: %.3f-%.3f)", auc_val, ci_obj[1], ci_obj[3]),
                       col = "#2E86C1", lwd = 2, bty = "n")
            }
            abline(a = 0, b = 1, lty = 2, col = "gray50")
            TRUE
        },

        # ══════════════════════════════════════════════════════════════════
        # Explanatory text outputs
        # ══════════════════════════════════════════════════════════════════
        .populateSummary = function(data, fit) {
            n_sel <- length(fit$selected)
            penalty_name <- switch(self$options$penalty,
                "lasso" = "LASSO (L1)", "ridge" = "Ridge (L2)", "elasticnet" = "Elastic Net")

            html <- paste0(
                "<p>", penalty_name, " logistic regression was performed with ",
                data$p, " candidate predictors and ", data$n, " observations (",
                data$n_events, " events, ", data$n_nonevents, " non-events). ",
                "Using ", self$options$lambda, " lambda selection with ",
                fit$nfolds, "-fold cross-validation, ",
                n_sel, " predictor(s) were selected.</p>"
            )
            self$results$summaryText$setContent(html)
        },

        .populateExplanations = function() {
            self$results$lassoExplanation$setContent(paste0(
                "<h4>LASSO Logistic Regression</h4>",
                "<p>LASSO (Least Absolute Shrinkage and Selection Operator) adds an L1 penalty ",
                "to the logistic regression likelihood, which shrinks some coefficients exactly to zero. ",
                "This performs automatic variable selection, identifying the most important predictors ",
                "for your binary outcome.</p>",
                "<h5>Key Concepts</h5>",
                "<ul>",
                "<li><strong>Lambda</strong>: Controls regularization strength. Higher lambda = fewer variables selected.</li>",
                "<li><strong>1SE Rule</strong>: Selects the most parsimonious model within 1 SE of minimum CV error.</li>",
                "<li><strong>Odds Ratio</strong>: exp(coefficient). OR > 1 increases probability of the positive class.</li>",
                "<li><strong>Elastic Net</strong>: Combines L1 and L2 penalties; useful when predictors are correlated.</li>",
                "</ul>"
            ))
        },

        .populateMethodologyNotes = function() {
            self$results$methodologyNotes$setContent(paste0(
                "<h4>Technical Notes</h4>",
                "<ul>",
                "<li>LASSO coefficients do not have standard errors or p-values. Use bootstrap validation for inference.</li>",
                "<li>With correlated predictors, LASSO arbitrarily selects one from a group. Consider elastic net (alpha 0.5).</li>",
                "<li>Events-per-variable (EPV) should be ≥10. Below 5, results are unreliable regardless of regularization.</li>",
                "<li>The scoring system rounds coefficients to integers, which loses precision but gains clinical usability.</li>",
                "<li>Continuous predictors in the scoring system are dichotomized at their median; the score-based performance therefore reflects a simplified point model and may differ from the continuous LASSO model's AUC.</li>",
                "<li>Bootstrap optimism correction estimates how much the apparent AUC overestimates true performance.</li>",
                "</ul>"
            ))
        },

        .populateClinicalGuidance = function() {
            self$results$clinicalGuidance$setContent(paste0(
                "<h4>Clinical Interpretation</h4>",
                "<ul>",
                "<li>Selected variables are the features most useful for distinguishing the two groups.</li>",
                "<li>Variables NOT selected are not necessarily unimportant — they may be redundant with selected features.</li>",
                "<li>The scoring system assigns positive points for features favoring the event class and negative points for the reference class.</li>",
                "<li>Higher total scores indicate higher probability of the event (positive class).</li>",
                "<li>Always validate the scoring system on an independent cohort before clinical adoption.</li>",
                "<li>Inter-observer agreement should be assessed for any morphologic scoring components.</li>",
                "</ul>"
            ))
        }
    )
)
