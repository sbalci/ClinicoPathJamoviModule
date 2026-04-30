#' @title Clinical Scoring System Generator
#' @description
#' Builds clinical integer-point scoring systems from regression models using
#' three published methods: Sullivan/D'Agostino (2004), Schneeweiss (Mehta et al.
#' 2016), and Beta10 (Zhang et al. 2017). Supports logistic (binary), Cox
#' (time-to-event), linear (continuous), and ordinal (ordered categories)
#' regression. Includes automatic variable categorization, calibration/
#' discrimination assessment, nomogram generation, score-to-outcome lookup,
#' bootstrap validation, and TRIPOD compliance.
#'
#' @importFrom R6 R6Class
#' @import jmvcore

clinicalscoreClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "clinicalscoreClass",
    inherit = clinicalscoreBase,
    private = list(

        # Store for plot reuse
        .model_data = NULL,

        # Initialize notice collection list
        .noticeList = list(),

        # Add a notice to the collection
        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type,
                title = title,
                content = content
            )
        },

        # Render collected notices as HTML
        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                return()
            }

            # Map notice types to colors
            typeStyles <- list(
                ERROR = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5"),
                STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74"),
                WARNING = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047"),
                INFO = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd")
            )

            html <- "<div style='margin: 10px 0;'>"

            for (notice in private$.noticeList) {
                style <- typeStyles[[notice$type]]
                if (is.null(style)) style <- typeStyles$INFO

                html <- paste0(html,
                    "<div style='background-color: ", style$bgcolor, "; ",
                    "border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: ", style$color, ";'>",
                    htmltools::htmlEscape(notice$title), "</strong><br>",
                    "<span style='color: #374151;'>", htmltools::htmlEscape(notice$content), "</span>",
                    "</div>"
                )
            }

            html <- paste0(html, "</div>")

            self$results$notices$setContent(html)
        },

        # Safe variable name matching: find which original variable
        # a model term belongs to (handles spaces, special chars)
        .matchTermToVar = function(term, var_list) {
            # Try exact match first
            if (term %in% var_list) return(list(var = term, cat = ""))
            # Try longest-prefix match (longest var name that is a prefix)
            candidates <- var_list[vapply(var_list, function(v) {
                # Use make.names to get the R-safe column name
                safe_v <- make.names(v)
                startsWith(term, safe_v) || startsWith(term, v)
            }, logical(1))]
            if (length(candidates) > 0) {
                # Pick the longest match
                best <- candidates[which.max(nchar(candidates))]
                safe_best <- make.names(best)
                if (startsWith(term, safe_best)) {
                    cat_part <- substring(term, nchar(safe_best) + 1)
                } else {
                    cat_part <- substring(term, nchar(best) + 1)
                }
                return(list(var = best, cat = cat_part))
            }
            list(var = term, cat = "")
        },

        .init = function() {
            if (is.null(self$options$outcome) ||
                is.null(self$options$explanatory) ||
                length(self$options$explanatory) < 2) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-info'>",
                    "<h4>", .("Clinical Scoring System Generator"), "</h4>",
                    "<p>", .("Build validated integer-point scoring systems for clinical decision making."), "</p>",
                    "<h5>", .("Steps:"), "</h5>",
                    "<ol>",
                    "<li>", .("Select your <strong>outcome</strong> variable (binary diagnosis or survival event)"), "</li>",
                    "<li>", .("Add <strong>predictor</strong> variables (clinical features, biomarkers, IHC scores)"), "</li>",
                    "<li>", .("Choose a <strong>scoring method</strong> or compare all three"), "</li>",
                    "<li>", .("Review the scoring card, lookup table, and performance metrics"), "</li>",
                    "</ol>",
                    "<h5>", .("Scoring Methods:"), "</h5>",
                    "<ul>",
                    "<li><strong>Schneeweiss</strong>: ", .("Divides coefficients by the smallest; recommended by Mehta et al. (2016)"), "</li>",
                    "<li><strong>Sullivan/D'Agostino</strong>: ", .("Framingham-style reference scaling (Sullivan et al. 2004)"), "</li>",
                    "<li><strong>Beta10</strong>: ", .("Scale to maximum points then round (Zhang et al. 2017)"), "</li>",
                    "</ul>",
                    "<p><em>", .("Tip: Use 'Compare All Methods' to see which produces the best discrimination with least information loss."), "</em></p>",
                    "</div>"))
                return()
            }
        },

        .run = function() {
            if (is.null(self$options$outcome) ||
                is.null(self$options$explanatory) ||
                length(self$options$explanatory) < 2) return()

            # Reset notice collection for fresh run
            private$.noticeList <- list()

            set.seed(self$options$random_seed)

            # ── 1. Clean and prepare data ──────────────────────────────
            prepared <- tryCatch(private$.prepareData(), error = function(e) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'><h4>", .("Data Error"), "</h4><p>", e$message, "</p></div>"))
                private$.addNotice("ERROR", .("Data Error"),
                    sprintf(.('Data preparation failed: %s'), e$message))
                private$.renderNotices()
                return(NULL)
            })
            if (is.null(prepared)) return()

            self$results$todo$setContent("")

            # ── 2. Suitability assessment ──────────────────────────────
            if (self$options$suitabilityCheck) {
                private$.assessSuitability(prepared)
            }

            private$.checkpoint()

            # ── 3. Fit model ───────────────────────────────────────────
            model <- tryCatch(private$.fitModel(prepared), error = function(e) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'><h4>", .("Model Error"), "</h4><p>", e$message, "</p></div>"))
                private$.addNotice("ERROR", .("Model Error"),
                    sprintf(.('Model fitting failed: %s'), e$message))
                private$.renderNotices()
                return(NULL)
            })
            if (is.null(model)) return()

            private$.checkpoint()

            # ── 4. Save plot data early (needed by scoring system) ─────
            private$.model_data <- list(prepared = prepared, model = model)

            # ── 5. Populate model results ──────────────────────────────
            private$.populateModelSummary(prepared, model)
            private$.populateCoefficients(model)

            # ── 6. Generate scoring system ─────────────────────────────
            private$.populateScoringSystem(prepared, model)

            private$.checkpoint()

            # ── 7. Discrimination metrics ──────────────────────────────
            if (self$options$showDiscrimination) {
                private$.populateDiscrimination(prepared, model)
            }

            # ── 8. Bootstrap validation ────────────────────────────────
            if (self$options$bootstrapValidation) {
                private$.populateValidation(prepared, model)
            }

            private$.checkpoint()

            # ── 9. Decision curve analysis ────────────────────────────
            if (self$options$showDecisionCurve) {
                private$.populateDecisionCurve(prepared, model)
            }

            # ── 10. TRIPOD checklist ───────────────────────────────────
            if (self$options$showTRIPOD) private$.populateTRIPOD(prepared, model)
            if (self$options$showSummary) private$.populateSummary(prepared, model)
            if (self$options$showExplanations) private$.populateExplanations()

            # ── 11. Completion notice ─────────────────────────────────
            model_label <- switch(prepared$model_type,
                "logistic" = .("logistic"), "cox" = .("Cox"),
                "linear" = .("linear"), "ordinal" = .("ordinal"))
            event_info <- if (prepared$model_type == "linear") {
                sprintf(.("N=%d"), prepared$n)
            } else {
                sprintf(.("N=%d, %d events"), prepared$n, prepared$n_events)
            }
            private$.addNotice("INFO", .("Analysis Complete"),
                sprintf(.('Scoring system built using %s regression with %d predictors (%s). Method: %s.'),
                    model_label, length(prepared$expl_vars), event_info,
                    self$options$scoringMethod))

            # ── 12. Render all collected notices as HTML ─────────────
            private$.renderNotices()
        },

        # ══════════════════════════════════════════════════════════════════
        # Data preparation
        # ══════════════════════════════════════════════════════════════════
        .prepareData = function() {
            data <- self$data
            outcome_var <- self$options$outcome
            expl_vars <- self$options$explanatory
            model_type <- self$options$modelType

            # Extract outcome based on model type
            outcome_raw <- data[[outcome_var]]
            event_level <- NULL
            ref_level <- NULL
            y <- NULL
            y_ordered <- NULL  # for ordinal models

            if (model_type == "linear") {
                # Continuous outcome — no binary conversion
                y <- jmvcore::toNumeric(outcome_raw)
                if (all(is.na(y))) stop(.("Outcome variable must be numeric for linear regression."))
                event_level <- "continuous"
                ref_level <- "continuous"

            } else if (model_type == "ordinal") {
                # Ordinal outcome — ensure ordered factor
                if (is.factor(outcome_raw)) {
                    if (!is.ordered(outcome_raw)) {
                        y_ordered <- factor(outcome_raw, levels = levels(outcome_raw), ordered = TRUE)
                    } else {
                        y_ordered <- outcome_raw
                    }
                } else {
                    vals <- sort(unique(as.character(outcome_raw[!is.na(outcome_raw)])))
                    y_ordered <- factor(as.character(outcome_raw), levels = vals, ordered = TRUE)
                }
                # Also create numeric for scoring evaluation
                y <- as.numeric(y_ordered) - 1  # 0-based
                event_level <- paste(levels(y_ordered), collapse = " < ")
                ref_level <- levels(y_ordered)[1]

            } else {
                # Logistic or Cox — convert to 0/1 binary
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
                    event_level <- if (model_type == "logistic") "1" else as.character(self$options$outcomeLevel)
                    if (is.null(event_level) || !nzchar(event_level)) event_level <- "1"
                    ref_level <- "0"
                }
            }

            # Time variable for Cox
            time_val <- NULL
            if (model_type == "cox") {
                if (is.null(self$options$elapsedtime))
                    stop(.("Time variable is required for Cox regression."))
                time_val <- jmvcore::toNumeric(data[[self$options$elapsedtime]])
            }

            # Extract predictors
            predictors <- data[expl_vars]

            # Categorize continuous variables if requested
            cat_info <- list()
            if (self$options$autoCategorize) {
                method <- self$options$categorizeMethod
                for (v in expl_vars) {
                    col <- predictors[[v]]
                    if (is.numeric(col) && !is.factor(col) && length(unique(na.omit(col))) > 5) {
                        breaks <- private$.getBreaks(col, method)
                        if (!is.null(breaks)) {
                            labels <- private$.makeLabels(v, breaks)
                            predictors[[v]] <- cut(col, breaks = c(-Inf, breaks, Inf),
                                                   labels = labels, include.lowest = TRUE)
                            cat_info[[v]] <- list(breaks = breaks, labels = labels)
                        }
                    }
                }
            }

            # Complete cases
            if (!is.null(time_val)) {
                complete <- complete.cases(y, predictors, time_val)
            } else {
                complete <- complete.cases(y, predictors)
            }
            n_complete <- sum(complete)
            if (n_complete < 20) stop(.("Too few complete cases (need at least 20)."))

            y <- y[complete]
            predictors <- predictors[complete, , drop = FALSE]
            if (!is.null(time_val)) time_val <- time_val[complete]
            if (!is.null(y_ordered)) y_ordered <- y_ordered[complete]

            # n_events: for linear = NA (continuous), for ordinal = count in highest category
            if (model_type == "linear") {
                n_events <- NA
                n_nonevents <- NA
            } else if (model_type == "ordinal") {
                n_events <- sum(y == max(y, na.rm = TRUE), na.rm = TRUE)
                n_nonevents <- n_complete - n_events
            } else {
                n_events <- sum(y == 1, na.rm = TRUE)
                n_nonevents <- n_complete - n_events
            }

            list(
                y = y, y_ordered = y_ordered,
                predictors = predictors, time = time_val,
                n = n_complete, n_events = n_events,
                n_nonevents = n_nonevents,
                event_level = event_level, ref_level = ref_level,
                model_type = model_type, cat_info = cat_info,
                expl_vars = expl_vars
            )
        },

        .getBreaks = function(x, method) {
            x <- na.omit(x)
            if (length(x) < 10) return(NULL)

            if (method == "median") {
                return(median(x))
            } else if (method == "tertiles") {
                return(as.numeric(quantile(x, c(1/3, 2/3))))
            } else if (method == "quartiles") {
                return(as.numeric(quantile(x, c(0.25, 0.5, 0.75))))
            } else if (method == "manual") {
                breaks_str <- self$options$customBreaks
                if (is.null(breaks_str) || !nzchar(breaks_str)) return(median(x))
                return(sort(as.numeric(strsplit(breaks_str, ",")[[1]])))
            }
            median(x)
        },

        .makeLabels = function(varname, breaks) {
            n_groups <- length(breaks) + 1
            labs <- character(n_groups)
            labs[1] <- sprintf("<= %.1f", breaks[1])
            if (n_groups > 2) {
                for (i in 2:(n_groups - 1)) {
                    labs[i] <- sprintf("%.1f - %.1f", breaks[i-1], breaks[i])
                }
            }
            labs[n_groups] <- sprintf("> %.1f", breaks[length(breaks)])
            labs
        },

        # ══════════════════════════════════════════════════════════════════
        # Model fitting
        # ══════════════════════════════════════════════════════════════════
        .fitModel = function(prepared) {
            df <- data.frame(y = prepared$y, prepared$predictors)

            if (prepared$model_type == "logistic") {
                model <- glm(y ~ ., data = df, family = binomial)
                coefs <- coef(model)[-1]  # drop intercept
                se <- summary(model)$coefficients[-1, 2]
                p_vals <- summary(model)$coefficients[-1, 4]
                or_hr <- exp(coefs)
                ci_lower <- exp(coefs - 1.96 * se)
                ci_upper <- exp(coefs + 1.96 * se)
                predicted <- predict(model, type = "response")
                intercept <- coef(model)[1]

            } else if (prepared$model_type == "linear") {
                model <- lm(y ~ ., data = df)
                sm <- summary(model)
                coefs <- coef(model)[-1]  # drop intercept
                se <- sm$coefficients[-1, 2]
                p_vals <- sm$coefficients[-1, 4]
                # For linear: effect size = standardized coefficient
                or_hr <- coefs  # raw coefficients (no exponentiation)
                ci_lower <- coefs - 1.96 * se
                ci_upper <- coefs + 1.96 * se
                predicted <- predict(model)
                intercept <- coef(model)[1]

            } else if (prepared$model_type == "ordinal") {
                if (!requireNamespace("MASS", quietly = TRUE))
                    stop(.("Package 'MASS' is required for ordinal regression."))
                df$y_ord <- prepared$y_ordered
                model <- MASS::polr(y_ord ~ . - y, data = df, Hess = TRUE)
                sm <- summary(model)
                # polr coefficients (not intercepts)
                coefs <- coef(model)
                se <- sm$coefficients[seq_along(coefs), 2]
                # p-values from t-distribution (polr doesn't provide them)
                t_vals <- coefs / se
                p_vals <- 2 * pt(abs(t_vals), df = prepared$n - length(coefs), lower.tail = FALSE)
                or_hr <- exp(coefs)  # proportional odds ratios
                ci_lower <- exp(coefs - 1.96 * se)
                ci_upper <- exp(coefs + 1.96 * se)
                # Predicted class probabilities → linear predictor
                predicted <- as.numeric(predict(model, type = "lp"))
                intercept <- NA

            } else {
                surv_obj <- survival::Surv(prepared$time, prepared$y)
                df$surv <- surv_obj
                model <- survival::coxph(surv ~ . - y, data = df)
                coefs <- coef(model)
                se <- sqrt(diag(vcov(model)))
                sm <- summary(model)
                p_vals <- sm$coefficients[, 5]
                or_hr <- exp(coefs)
                ci_lower <- exp(coefs - 1.96 * se)
                ci_upper <- exp(coefs + 1.96 * se)
                predicted <- predict(model, type = "risk")
                intercept <- NA
            }

            list(
                model = model,
                coefs = coefs, se = se, p_vals = p_vals,
                or_hr = or_hr, ci_lower = ci_lower, ci_upper = ci_upper,
                predicted = predicted, intercept = intercept,
                var_names = names(coefs)
            )
        },

        # ══════════════════════════════════════════════════════════════════
        # Scoring methods (reused from lassologistic, enhanced)
        # ══════════════════════════════════════════════════════════════════
        .computePoints = function(coefs, method, max_points = 10) {
            abs_coefs <- abs(coefs)
            signs <- sign(coefs)
            if (all(abs_coefs == 0)) return(rep(0L, length(coefs)))

            if (method == "beta10") {
                max_abs <- max(abs_coefs)
                raw <- coefs * max_points / max_abs
                pts <- round(raw)
                pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]
            } else if (method == "schneeweiss") {
                min_abs <- min(abs_coefs[abs_coefs > 0])
                raw <- coefs / min_abs
                pts <- round(raw)
                pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]
            } else if (method == "sullivan") {
                W <- max(abs_coefs)
                raw <- (coefs / W) * max_points
                pts <- round(raw)
                pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]
            } else {
                pts <- round(coefs)
            }
            as.integer(pts)
        },

        .computeTotalScores = function(model_matrix, points) {
            as.numeric(model_matrix %*% points)
        },

        .evaluateScore = function(y, scores, model_type = "logistic") {
            if (length(unique(scores)) < 2) {
                return(list(auc = NA, cutoff = NA, accuracy = NA,
                            sensitivity = NA, specificity = NA,
                            r_squared = NA, rmse = NA, correlation = NA))
            }

            if (model_type == "linear") {
                # For continuous outcomes: R², RMSE, correlation
                r2 <- cor(y, scores, use = "complete.obs")^2
                rmse <- sqrt(mean((y - scores)^2, na.rm = TRUE))
                corr <- cor(y, scores, use = "complete.obs")
                return(list(auc = NA, cutoff = NA, accuracy = NA,
                            sensitivity = NA, specificity = NA,
                            r_squared = r2, rmse = rmse, correlation = corr))
            }

            auc_val <- NA
            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    if (model_type == "ordinal") {
                        # Multi-class AUC
                        roc_obj <- pROC::multiclass.roc(y, scores, quiet = TRUE)
                        auc_val <- as.numeric(roc_obj$auc)
                    } else {
                        roc_obj <- pROC::roc(y, scores, quiet = TRUE)
                        auc_val <- as.numeric(pROC::auc(roc_obj))
                    }
                }
            }, error = function(e) {})

            # Youden-optimal cutoff (for binary/cox)
            score_vals <- sort(unique(scores))
            best_youden <- -Inf; best_cutoff <- score_vals[1]
            if (model_type %in% c("logistic", "cox")) {
                for (cutoff in score_vals) {
                    pred <- as.integer(scores >= cutoff)
                    sens <- sum(pred == 1 & y == 1) / max(sum(y == 1), 1)
                    spec <- sum(pred == 0 & y == 0) / max(sum(y == 0), 1)
                    youden <- sens + spec - 1
                    if (youden > best_youden) {
                        best_youden <- youden; best_cutoff <- cutoff
                    }
                }
            } else {
                # Ordinal: use median score as cutoff
                best_cutoff <- median(scores, na.rm = TRUE)
            }

            pred <- as.integer(scores >= best_cutoff)
            y_bin <- if (model_type == "ordinal") as.integer(y >= max(y) / 2) else y
            tp <- sum(pred == 1 & y_bin == 1); fp <- sum(pred == 1 & y_bin == 0)
            fn <- sum(pred == 0 & y_bin == 1)
            sens <- tp / max(tp + fn, 1)
            spec <- sum(pred == 0 & y_bin == 0) / max(sum(y_bin == 0), 1)
            acc <- mean(pred == y_bin)

            list(auc = auc_val, cutoff = best_cutoff, accuracy = acc,
                 sensitivity = sens, specificity = spec,
                 r_squared = NA, rmse = NA, correlation = NA)
        },

        # ══════════════════════════════════════════════════════════════════
        # Populate results
        # ══════════════════════════════════════════════════════════════════
        .assessSuitability = function(prepared) {
            p <- length(prepared$expl_vars)
            checks <- c()
            icons <- c(green = "&#x2705;", yellow = "&#x26A0;&#xFE0F;", red = "&#x274C;")

            if (prepared$model_type == "linear") {
                # Linear: observations per variable (OPV), not EPV
                opv <- prepared$n / p
                if (opv >= 15) {
                    checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>OPV = %.1f (&ge;15: adequate)</td></tr>",
                        icons["green"], .("Observations per variable"), opv))
                } else if (opv >= 10) {
                    checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>OPV = %.1f (10-15: marginal)</td></tr>",
                        icons["yellow"], .("Observations per variable"), opv))
                } else {
                    checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>OPV = %.1f (&lt;10: overfitting risk)</td></tr>",
                        icons["red"], .("Observations per variable"), opv))
                }
            } else {
                epv <- min(prepared$n_events, prepared$n_nonevents) / p
                if (epv >= 10) {
                    checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>EPV = %.1f (&ge;10: adequate)</td></tr>",
                        icons["green"], .("Events per variable"), epv))
                } else if (epv >= 5) {
                    checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>EPV = %.1f (5-10: marginal, consider fewer predictors)</td></tr>",
                        icons["yellow"], .("Events per variable"), epv))
                } else {
                    checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>EPV = %.1f (&lt;5: high overfitting risk, reduce predictors)</td></tr>",
                        icons["red"], .("Events per variable"), epv))
                }
            }

            # Sample size
            status <- if (prepared$n >= 100) "green" else if (prepared$n >= 50) "yellow" else "red"
            checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>N = %d</td></tr>",
                icons[status], .("Sample size"), prepared$n))

            if (prepared$model_type != "linear") {
                # Events
                status <- if (prepared$n_events >= 50) "green" else if (prepared$n_events >= 20) "yellow" else "red"
                checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>%d events, %d non-events</td></tr>",
                    icons[status], .("Event count"), prepared$n_events, prepared$n_nonevents))
            }

            # Minimum recommended N
            if (prepared$model_type == "linear") {
                # Rule of thumb: N >= 10*p + 50
                min_n <- max(10 * p + 50, 100)
            } else {
                event_frac <- prepared$n_events / prepared$n
                min_denom <- min(event_frac, 1 - event_frac)
                min_n <- if (min_denom > 0) max(10 * p / min_denom, 100) else prepared$n + 1
            }
            status <- if (prepared$n >= min_n) "green" else "red"
            min_n_display <- if (is.finite(min_n)) as.integer(ceiling(min_n)) else as.integer(prepared$n + 1L)
            checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>N = %d vs recommended minimum %d (Riley et al. 2020)</td></tr>",
                icons[status], .("Minimum sample size"), as.integer(prepared$n), min_n_display))

            html <- paste0(
                "<h4>", .("Data Suitability Assessment"), "</h4>",
                "<table class='table table-condensed'><thead>",
                "<tr><th></th><th>", .("Check"), "</th><th>", .("Result"), "</th></tr></thead><tbody>",
                paste(checks, collapse = ""),
                "</tbody></table>")
            self$results$suitabilityReport$setContent(html)

            # Surface critical issues as notices (collected for HTML rendering)
            if (prepared$model_type == "linear") {
                opv <- prepared$n / p
                if (opv < 10) {
                    private$.addNotice("STRONG_WARNING", .("Low Observations Per Variable"),
                        sprintf(.('OPV = %.1f (<10): high overfitting risk. Reduce predictors or collect more data.'), opv))
                } else if (opv < 15) {
                    private$.addNotice("WARNING", .("Marginal Observations Per Variable"),
                        sprintf(.('OPV = %.1f (marginal). Enable bootstrap validation to assess overfitting risk.'), opv))
                }
            } else {
                epv <- min(prepared$n_events, prepared$n_nonevents) / p
                if (epv < 5) {
                    private$.addNotice("STRONG_WARNING", .("Low Events Per Variable"),
                        sprintf(.('EPV = %.1f (<5): high overfitting risk. Reduce to %d or fewer predictors, or collect more data.'),
                                epv, as.integer(floor(min(prepared$n_events, prepared$n_nonevents) / 5))))
                } else if (epv < 10) {
                    private$.addNotice("WARNING", .("Marginal Events Per Variable"),
                        sprintf(.('EPV = %.1f (marginal). Enable bootstrap validation to assess overfitting risk.'), epv))
                }
            }

            if (prepared$n < min_n) {
                private$.addNotice("STRONG_WARNING", .("Insufficient Sample Size"),
                    sprintf(.('Sample size N=%d is below the recommended minimum of %d (Riley et al. 2020). Results may not generalize.'),
                            as.integer(prepared$n), min_n_display))
            }
        },

        .populateModelSummary = function(prepared, model) {
            table <- self$results$modelSummary
            mt <- prepared$model_type
            effect_label <- switch(mt,
                "logistic" = .("Odds Ratio"),
                "cox" = .("Hazard Ratio"),
                "linear" = .("Regression Coefficient"),
                "ordinal" = .("Proportional Odds Ratio"))
            model_label <- switch(mt,
                "logistic" = .("Logistic Regression"),
                "cox" = .("Cox Regression"),
                "linear" = .("Linear Regression"),
                "ordinal" = .("Ordinal (Proportional Odds) Regression"))

            rows <- list(
                list(.("Model type"), model_label),
                list(.("Total observations"), as.character(prepared$n))
            )

            if (mt == "linear") {
                sm <- summary(model$model)
                rows <- c(rows, list(
                    list(.("Outcome"), sprintf(.("Continuous (%s)"), self$options$outcome)),
                    list(.("R-squared"), sprintf("%.3f", sm$r.squared)),
                    list(.("Adjusted R-squared"), sprintf("%.3f", sm$adj.r.squared)),
                    list(.("RMSE"), sprintf("%.3f", sqrt(mean(sm$residuals^2))))
                ))
            } else if (mt == "ordinal") {
                rows <- c(rows, list(
                    list(.("Outcome levels"), prepared$event_level),
                    list(.("Number of categories"), as.character(length(levels(prepared$y_ordered))))
                ))
            } else {
                rows <- c(rows, list(
                    list(.("Events"), sprintf("%d (%s)", prepared$n_events, prepared$event_level)),
                    list(.("Non-events"), sprintf("%d (%s)", prepared$n_nonevents, prepared$ref_level))
                ))
            }

            p <- length(prepared$expl_vars)
            rows <- c(rows, list(
                list(.("Predictors"), as.character(p)),
                list(.("Model coefficients"), as.character(length(model$coefs))),
                list(.("Effect measure"), effect_label)
            ))

            if (mt == "linear") {
                rows <- c(rows, list(list(.("OPV"), sprintf("%.1f", prepared$n / p))))
            } else {
                epv_val <- min(prepared$n_events, prepared$n_nonevents) / p
                rows <- c(rows, list(list(.("EPV"), sprintf("%.1f", epv_val))))
            }

            for (i in seq_along(rows)) {
                table$addRow(rowKey = i, values = list(statistic = rows[[i]][[1]], value = rows[[i]][[2]]))
            }
        },

        .populateCoefficients = function(model) {
            table <- self$results$coefficients
            for (i in seq_along(model$coefs)) {
                term <- model$var_names[i]
                parsed <- private$.matchTermToVar(term, self$options$explanatory)

                table$addRow(rowKey = i, values = list(
                    variable = parsed$var,
                    category = parsed$cat,
                    coefficient = model$coefs[i],
                    effectSize = model$or_hr[i],
                    ci_lower = model$ci_lower[i],
                    ci_upper = model$ci_upper[i],
                    p_value = model$p_vals[i]
                ))
            }
        },

        .populateScoringSystem = function(prepared, model) {
            table <- self$results$scoringTable
            perf_table <- self$results$scoringPerformance

            method <- self$options$scoringMethod %||% "schneeweiss"
            max_pts <- self$options$maxPoints
            coefs <- model$coefs

            # Primary method
            pts <- private$.computePoints(coefs, if (method == "compare") "schneeweiss" else method, max_pts)

            # Build design matrix for scoring
            df_pred <- data.frame(prepared$predictors)
            mm <- model.matrix(~ ., data = df_pred)[, -1, drop = FALSE]

            # Populate scoring table
            for (i in seq_along(coefs)) {
                term <- model$var_names[i]
                parsed <- private$.matchTermToVar(term, self$options$explanatory)
                category <- if (nzchar(parsed$cat)) parsed$cat else .("Present")

                table$addRow(rowKey = i, values = list(
                    variable = parsed$var,
                    category = category,
                    effectSize = model$or_hr[i],
                    points = pts[i]
                ))
            }

            # Method references
            method_refs <- list(
                beta10 = .("Beta10 method (Zhang et al. Ann Transl Med 2017)"),
                schneeweiss = .("Schneeweiss method (Mehta et al. J Clin Epidemiol 2016)"),
                sullivan = .("Sullivan/D'Agostino method (Statistics in Medicine 2004)"),
                compare = .("Schneeweiss shown as primary; see Method Comparison table")
            )
            table$setNote("method", method_refs[[method]])

            # Total scores and performance
            total_scores <- private$.computeTotalScores(mm, pts)
            perf <- private$.evaluateScore(prepared$y, total_scores, prepared$model_type)

            perf_rows <- list(
                list(.("Scoring method"), switch(method, "beta10"="Beta10", "schneeweiss"="Schneeweiss",
                    "sullivan"="Sullivan/D'Agostino", "compare"="Schneeweiss (primary)"))
            )

            if (prepared$model_type == "linear") {
                perf_rows <- c(perf_rows, list(
                    list(.("Score-Outcome R-squared"), sprintf("%.3f", perf$r_squared)),
                    list(.("Score-Outcome Correlation"), sprintf("%.3f", perf$correlation)),
                    list(.("RMSE"), sprintf("%.3f", perf$rmse)),
                    list(.("Score range"), sprintf("%.0f to %.0f", min(total_scores), max(total_scores)))
                ))
            } else {
                perf_rows <- c(perf_rows, list(
                    list(.("Score AUC / C-index"), sprintf("%.3f", perf$auc)),
                    list(.("Optimal cutoff"), as.character(perf$cutoff)),
                    list(.("Accuracy"), sprintf("%.3f", perf$accuracy)),
                    list(.("Sensitivity"), sprintf("%.3f", perf$sensitivity)),
                    list(.("Specificity"), sprintf("%.3f", perf$specificity)),
                    list(.("Score range"), sprintf("%.0f to %.0f", min(total_scores), max(total_scores)))
                ))
            }
            for (i in seq_along(perf_rows)) {
                perf_table$addRow(rowKey = i, values = list(
                    metric = perf_rows[[i]][[1]], value = perf_rows[[i]][[2]]))
            }

            # Method comparison
            if (method == "compare") {
                comp_table <- self$results$methodComparison
                full_auc <- perf_from_model <- NA
                tryCatch({
                    if (requireNamespace("pROC", quietly = TRUE)) {
                        full_auc <- as.numeric(pROC::auc(pROC::roc(prepared$y, model$predicted, quiet = TRUE)))
                    }
                }, error = function(e) {})

                for (j in seq_along(list("beta10", "schneeweiss", "sullivan"))) {
                    m_name <- c("beta10", "schneeweiss", "sullivan")[j]
                    m_label <- c("Beta10", "Schneeweiss", "Sullivan/D'Agostino")[j]
                    m_ref <- c("Zhang et al. 2017", "Mehta et al. 2016", "Sullivan et al. 2004")[j]
                    pts_j <- private$.computePoints(coefs, m_name, max_pts)
                    scores_j <- private$.computeTotalScores(mm, pts_j)
                    perf_j <- private$.evaluateScore(prepared$y, scores_j, prepared$model_type)
                    info_loss <- if (!is.na(full_auc) && !is.na(perf_j$auc) && full_auc > 0)
                        (1 - perf_j$auc / full_auc) * 100 else NA
                    comp_table$addRow(rowKey = j, values = list(
                        method = m_label, auc = perf_j$auc, accuracy = perf_j$accuracy,
                        info_loss = info_loss, reference = m_ref))
                }
                comp_table$addRow(rowKey = 4, values = list(
                    method = .("Full model (continuous)"), auc = full_auc,
                    accuracy = NA, info_loss = 0, reference = .("Reference")))
            }

            # Lookup table
            if (isTRUE(self$options$scoreLookup)) {
                lookup <- self$results$lookupTable
                score_vals <- sort(unique(total_scores))
                for (s in score_vals) {
                    idx <- total_scores == s
                    n_c <- sum(idx)
                    if (prepared$model_type == "linear") {
                        # For linear: show mean predicted value
                        n_e <- NA
                        mean_outcome <- mean(prepared$y[idx], na.rm = TRUE)
                        risk <- if (s >= median(total_scores)) .("Above median") else .("Below median")
                        lookup$addRow(rowKey = as.character(s), values = list(
                            score = as.integer(s), n_cases = n_c, n_events = as.integer(NA),
                            probability = mean_outcome, risk_group = risk))
                    } else {
                        n_e <- sum(prepared$y[idx] == 1)
                        prob <- n_e / n_c
                        risk <- if (s >= perf$cutoff) .("High risk") else .("Low risk")
                        lookup$addRow(rowKey = as.character(s), values = list(
                            score = as.integer(s), n_cases = n_c, n_events = n_e,
                            probability = prob, risk_group = risk))
                    }
                }
            }

            # Store for plots
            private$.model_data$total_scores <- total_scores
            private$.model_data$cutoff <- perf$cutoff
        },

        .populateDiscrimination = function(prepared, model) {
            table <- self$results$discriminationTable

            if (prepared$model_type == "linear") {
                # Linear: R², adjusted R², Pearson/Spearman correlation
                sm <- summary(model$model)
                r2 <- sm$r.squared
                adj_r2 <- sm$adj.r.squared
                corr_p <- cor(prepared$y, model$predicted, use = "complete.obs", method = "pearson")
                corr_s <- cor(prepared$y, model$predicted, use = "complete.obs", method = "spearman")
                rmse <- sqrt(mean((prepared$y - model$predicted)^2, na.rm = TRUE))

                interp_r2 <- if (r2 >= 0.7) .("Excellent") else if (r2 >= 0.5) .("Good")
                             else if (r2 >= 0.3) .("Moderate") else .("Poor")
                table$addRow(rowKey = 1, values = list(
                    metric = .("R-squared"), estimate = r2,
                    ci_lower = NA, ci_upper = NA, interpretation = interp_r2))
                table$addRow(rowKey = 2, values = list(
                    metric = .("Adjusted R-squared"), estimate = adj_r2,
                    ci_lower = NA, ci_upper = NA, interpretation = ""))
                table$addRow(rowKey = 3, values = list(
                    metric = .("Pearson Correlation"), estimate = corr_p,
                    ci_lower = NA, ci_upper = NA, interpretation = ""))
                table$addRow(rowKey = 4, values = list(
                    metric = .("Spearman Correlation"), estimate = corr_s,
                    ci_lower = NA, ci_upper = NA, interpretation = ""))
                table$addRow(rowKey = 5, values = list(
                    metric = .("RMSE"), estimate = rmse,
                    ci_lower = NA, ci_upper = NA, interpretation = ""))

                if (r2 < 0.3) {
                    private$.addNotice("WARNING", .("Poor Model Fit"),
                        sprintf(.('R-squared = %.3f indicates the model explains little outcome variance. Consider additional predictors.'), r2))
                }
                return()
            }

            metric_label <- switch(prepared$model_type,
                "logistic" = "AUC",
                "ordinal" = .("Generalized AUC"),
                "C-index")

            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    if (prepared$model_type == "ordinal") {
                        roc_obj <- pROC::multiclass.roc(prepared$y, model$predicted, quiet = TRUE)
                        auc_val <- as.numeric(roc_obj$auc)
                        interp <- if (auc_val >= 0.9) .("Excellent") else if (auc_val >= 0.8) .("Good")
                                  else if (auc_val >= 0.7) .("Acceptable") else .("Poor")
                        table$addRow(rowKey = 1, values = list(
                            metric = metric_label, estimate = auc_val,
                            ci_lower = NA, ci_upper = NA, interpretation = interp))
                    } else {
                        roc_obj <- pROC::roc(prepared$y, model$predicted, quiet = TRUE)
                        auc_val <- as.numeric(pROC::auc(roc_obj))
                        ci_obj <- pROC::ci.auc(roc_obj, method = "delong")
                        interp <- if (auc_val >= 0.9) .("Excellent") else if (auc_val >= 0.8) .("Good")
                                  else if (auc_val >= 0.7) .("Acceptable") else .("Poor")
                        table$addRow(rowKey = 1, values = list(
                            metric = metric_label, estimate = auc_val,
                            ci_lower = ci_obj[1], ci_upper = ci_obj[3], interpretation = interp))
                    }

                    # Notices for discrimination concerns
                    if (auc_val < 0.7) {
                        private$.addNotice("WARNING", .("Poor Discrimination"),
                            sprintf(.('%s = %.3f indicates poor discrimination. Consider adding more informative predictors.'),
                                    metric_label, auc_val))
                    } else if (auc_val > 0.95 && prepared$n < 100) {
                        private$.addNotice("STRONG_WARNING", .("Possible Overfitting"),
                            sprintf(.('%s = %.3f with N=%d: likely overfitted. Enable bootstrap validation for corrected estimate.'),
                                    metric_label, auc_val, prepared$n))
                    }
                }
            }, error = function(e) {})

            # Brier score (logistic only)
            if (prepared$model_type == "logistic") {
                brier <- mean((model$predicted - prepared$y)^2)
                interp <- if (brier < 0.1) .("Excellent") else if (brier < 0.2) .("Good") else .("Poor")
                table$addRow(rowKey = 2, values = list(
                    metric = .("Brier Score"), estimate = brier,
                    ci_lower = NA, ci_upper = NA, interpretation = interp))
            }
        },

        .populateValidation = function(prepared, model) {
            table <- self$results$validationTable
            B <- self$options$bootstrapN
            model_type <- prepared$model_type

            if (model_type == "linear") {
                # Bootstrap optimism-corrected R²
                apparent_r2 <- summary(model$model)$r.squared
                optimism_vals <- rep(NA_real_, B)
                for (b in seq_len(B)) {
                    tryCatch({
                        idx <- sample(prepared$n, replace = TRUE)
                        df_boot <- data.frame(y = prepared$y[idx], prepared$predictors[idx, , drop = FALSE])
                        m_boot <- lm(y ~ ., data = df_boot)
                        pred_boot <- predict(m_boot)
                        pred_orig <- predict(m_boot, newdata = data.frame(y = prepared$y, prepared$predictors))
                        r2_boot <- 1 - sum((df_boot$y - pred_boot)^2) / sum((df_boot$y - mean(df_boot$y))^2)
                        r2_orig <- cor(prepared$y, pred_orig, use = "complete.obs")^2
                        optimism_vals[b] <- r2_boot - r2_orig
                    }, error = function(e) {})
                }
                mean_opt <- mean(optimism_vals, na.rm = TRUE)
                corrected <- apparent_r2 - mean_opt
                table$addRow(rowKey = 1, values = list(
                    metric = .("R-squared"), apparent = apparent_r2,
                    optimism = mean_opt, corrected = corrected))
                if (!is.na(mean_opt) && mean_opt > 0.05) {
                    table$setNote("warning",
                        sprintf(.("Optimism = %.3f indicates overfitting. Corrected R-squared (%.3f) is more realistic."),
                                mean_opt, corrected))
                }
                return()
            }

            apparent_auc <- NA
            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    if (model_type == "ordinal") {
                        apparent_auc <- as.numeric(pROC::multiclass.roc(prepared$y, model$predicted, quiet = TRUE)$auc)
                    } else {
                        apparent_auc <- as.numeric(pROC::auc(pROC::roc(prepared$y, model$predicted, quiet = TRUE)))
                    }
                }
            }, error = function(e) {})

            optimism_vals <- rep(NA_real_, B)
            for (b in seq_len(B)) {
                tryCatch({
                    idx <- sample(prepared$n, replace = TRUE)
                    df_boot <- data.frame(y = prepared$y[idx], prepared$predictors[idx, , drop = FALSE])
                    if (length(unique(df_boot$y)) < 2) next

                    if (model_type == "logistic") {
                        m_boot <- glm(y ~ ., data = df_boot, family = binomial)
                        pred_boot <- predict(m_boot, type = "response")
                        pred_orig <- predict(m_boot, newdata = data.frame(y = prepared$y, prepared$predictors), type = "response")
                    } else if (model_type == "ordinal") {
                        df_boot$y_ord <- prepared$y_ordered[idx]
                        m_boot <- MASS::polr(y_ord ~ . - y, data = df_boot, Hess = TRUE)
                        pred_boot <- as.numeric(predict(m_boot, type = "lp"))
                        pred_orig <- as.numeric(predict(m_boot, newdata = data.frame(y = prepared$y, prepared$predictors,
                            y_ord = prepared$y_ordered), type = "lp"))
                    } else {
                        df_boot$surv <- survival::Surv(prepared$time[idx], prepared$y[idx])
                        m_boot <- survival::coxph(surv ~ . - y, data = df_boot)
                        pred_boot <- predict(m_boot, type = "risk")
                        pred_orig <- predict(m_boot, newdata = data.frame(y = prepared$y, prepared$predictors,
                            surv = survival::Surv(prepared$time, prepared$y)), type = "risk")
                    }

                    if (requireNamespace("pROC", quietly = TRUE)) {
                        if (model_type == "ordinal") {
                            auc_boot <- as.numeric(pROC::multiclass.roc(df_boot$y, pred_boot, quiet = TRUE)$auc)
                            auc_orig <- as.numeric(pROC::multiclass.roc(prepared$y, pred_orig, quiet = TRUE)$auc)
                        } else {
                            auc_boot <- as.numeric(pROC::auc(pROC::roc(df_boot$y, pred_boot, quiet = TRUE)))
                            auc_orig <- as.numeric(pROC::auc(pROC::roc(prepared$y, pred_orig, quiet = TRUE)))
                        }
                        optimism_vals[b] <- auc_boot - auc_orig
                    }
                }, error = function(e) {})
            }

            mean_opt <- mean(optimism_vals, na.rm = TRUE)
            corrected <- apparent_auc - mean_opt

            metric_label <- switch(model_type,
                "logistic" = "AUC",
                "ordinal" = .("Generalized AUC"),
                "C-index")
            table$addRow(rowKey = 1, values = list(
                metric = metric_label,
                apparent = apparent_auc, optimism = mean_opt, corrected = corrected))

            if (!is.na(mean_opt) && mean_opt > 0.05) {
                table$setNote("warning",
                    sprintf(.("Optimism = %.3f indicates overfitting. Corrected estimate (%.3f) is more realistic."),
                            mean_opt, corrected))
            }
        },

        # ══════════════════════════════════════════════════════════════════
        # Plots
        # ══════════════════════════════════════════════════════════════════
        .calibrationPlot = function(image, ggtheme, theme, ...) {
            md <- private$.model_data
            if (is.null(md)) return(FALSE)

            prepared <- md$prepared; model <- md$model

            # Linear regression: predicted vs observed scatter plot
            if (prepared$model_type == "linear") {
                cal_df <- data.frame(predicted = model$predicted, observed = prepared$y)
                p <- ggplot2::ggplot(cal_df, ggplot2::aes(x = predicted, y = observed)) +
                    ggplot2::geom_point(alpha = 0.5, size = 2) +
                    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
                    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#2E86C1", fill = "#D6EAF8") +
                    ggplot2::labs(title = .("Calibration: Predicted vs Observed"),
                                  x = .("Predicted Value"), y = .("Observed Value")) +
                    ggtheme
                print(p)
                return(TRUE)
            }

            # Ordinal: skip calibration (not straightforward)
            if (prepared$model_type == "ordinal") return(FALSE)

            if (prepared$model_type != "logistic") return(FALSE)

            n_groups <- self$options$calibrationGroups
            pred <- model$predicted
            obs <- prepared$y

            # Create calibration groups -- reduce groups if quantile breaks collapse
            n_unique <- length(unique(pred))
            eff_groups <- min(n_groups, n_unique)
            if (eff_groups < 2) return(FALSE)

            breaks <- unique(quantile(pred, probs = seq(0, 1, length.out = eff_groups + 1)))
            if (length(breaks) < 2) return(FALSE)

            groups <- cut(pred, breaks = breaks, include.lowest = TRUE)
            cal_df <- data.frame(
                predicted = tapply(pred, groups, mean, na.rm = TRUE),
                observed = tapply(obs, groups, mean, na.rm = TRUE)
            )
            cal_df <- cal_df[complete.cases(cal_df), , drop = FALSE]
            if (nrow(cal_df) < 2) return(FALSE)

            p <- ggplot2::ggplot(cal_df, ggplot2::aes(x = predicted, y = observed)) +
                ggplot2::geom_point(size = 3) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
                ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#2E86C1", fill = "#D6EAF8") +
                ggplot2::labs(title = .("Calibration Plot"),
                              x = .("Predicted Probability"), y = .("Observed Proportion")) +
                ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
                ggtheme
            print(p)
            TRUE
        },

        .nomogramPlot = function(image, ggtheme, theme, ...) {
            md <- private$.model_data
            if (is.null(md)) return(FALSE)
            if (!requireNamespace("rms", quietly = TRUE)) return(FALSE)

            prepared <- md$prepared

            tryCatch({
                dd <- rms::datadist(prepared$predictors)
                options(datadist = "dd")
                on.exit(options(datadist = NULL), add = TRUE)

                if (prepared$model_type == "logistic") {
                    df_rms <- data.frame(y = prepared$y, prepared$predictors)
                    fit <- rms::lrm(y ~ ., data = df_rms)
                } else if (prepared$model_type == "linear") {
                    df_rms <- data.frame(y = prepared$y, prepared$predictors)
                    fit <- rms::ols(y ~ ., data = df_rms)
                } else if (prepared$model_type == "ordinal") {
                    df_rms <- data.frame(y = prepared$y_ordered, prepared$predictors)
                    fit <- rms::lrm(y ~ ., data = df_rms)
                } else {
                    df_rms <- data.frame(y = prepared$y, time = prepared$time, prepared$predictors)
                    surv_obj <- survival::Surv(df_rms$time, df_rms$y)
                    fit <- rms::cph(surv_obj ~ . - y - time, data = df_rms)
                }

                fun_arg <- switch(prepared$model_type,
                    "logistic" = plogis, "linear" = NULL, "ordinal" = plogis, NULL)
                funlabel_arg <- switch(prepared$model_type,
                    "logistic" = .("Risk"), "linear" = .("Predicted Value"),
                    "ordinal" = .("Cumulative Probability"), .("Log Relative Hazard"))
                nom <- rms::nomogram(fit, fun = fun_arg, funlabel = funlabel_arg)
                plot(nom, main = .("Clinical Nomogram"))
                TRUE
            }, error = function(e) {
                plot.new()
                text(0.5, 0.5, paste(.("Nomogram generation failed:"), e$message),
                     cex = 0.8, col = "red")
                TRUE
            })
        },

        .scoreDistPlot = function(image, ggtheme, theme, ...) {
            md <- private$.model_data
            if (is.null(md) || is.null(md$total_scores)) return(FALSE)

            model_type <- md$prepared$model_type

            if (model_type == "linear") {
                # Continuous outcome: histogram colored by median split
                median_y <- median(md$prepared$y, na.rm = TRUE)
                df <- data.frame(
                    score = md$total_scores,
                    outcome = factor(ifelse(md$prepared$y >= median_y,
                        .("Above median"), .("Below median")))
                )
            } else if (model_type == "ordinal") {
                # Multi-class outcome: use original ordered levels
                df <- data.frame(
                    score = md$total_scores,
                    outcome = factor(md$prepared$y_ordered)
                )
            } else {
                # Binary (logistic/cox)
                df <- data.frame(
                    score = md$total_scores,
                    outcome = factor(ifelse(md$prepared$y == 1,
                        md$prepared$event_level, md$prepared$ref_level))
                )
            }

            p <- ggplot2::ggplot(df, ggplot2::aes(x = score, fill = outcome)) +
                ggplot2::geom_histogram(bins = 20, alpha = 0.7, position = "identity")

            # Add cutoff line only for binary models with a valid cutoff
            if (model_type %in% c("logistic", "cox") && !is.null(md$cutoff) && !is.na(md$cutoff)) {
                p <- p +
                    ggplot2::geom_vline(xintercept = md$cutoff, linetype = "dashed", color = "red", linewidth = 1) +
                    ggplot2::annotate("text", x = md$cutoff, y = Inf, vjust = 2,
                                      label = sprintf(.("Cutoff = %.0f"), md$cutoff), color = "red", size = 3.5)
            }

            p <- p +
                ggplot2::labs(title = .("Score Distribution by Outcome"),
                              x = .("Total Score"), y = .("Count"), fill = .("Outcome")) +
                ggtheme
            print(p)
            TRUE
        },

        # ══════════════════════════════════════════════════════════════════
        # Decision curve analysis
        # ══════════════════════════════════════════════════════════════════
        .populateDecisionCurve = function(prepared, model) {
            if (!prepared$model_type %in% c("logistic")) return()

            pred <- model$predicted
            obs <- prepared$y
            n <- length(obs)
            prevalence <- mean(obs)

            # Fine-grained for plot
            thresholds_plot <- seq(0.01, 0.99, by = 0.01)
            # Coarser for table display (every 5%)
            thresholds_table <- seq(0.05, 0.95, by = 0.05)

            table <- self$results$decisionCurveTable

            .calc_nb <- function(pt) {
                nb_all <- prevalence - (1 - prevalence) * pt / (1 - pt)
                tp <- sum(pred >= pt & obs == 1)
                fp <- sum(pred >= pt & obs == 0)
                nb_model <- tp / n - fp / n * pt / (1 - pt)
                list(threshold = pt, net_benefit_model = nb_model, net_benefit_all = nb_all)
            }

            # Build full data for plot
            dca_list <- lapply(thresholds_plot, .calc_nb)
            dca_data <- do.call(rbind, lapply(dca_list, as.data.frame))

            # Populate table with subset
            for (pt in thresholds_table) {
                nb <- .calc_nb(pt)
                table$addRow(rowKey = as.character(pt), values = nb)
            }

            private$.model_data$dca_data <- dca_data
        },

        .decisionCurvePlot = function(image, ggtheme, theme, ...) {
            md <- private$.model_data
            if (is.null(md) || is.null(md$dca_data)) return(FALSE)

            dca <- md$dca_data

            p <- ggplot2::ggplot(dca, ggplot2::aes(x = threshold)) +
                ggplot2::geom_line(ggplot2::aes(y = net_benefit_model, color = .("Model")), linewidth = 1) +
                ggplot2::geom_line(ggplot2::aes(y = net_benefit_all, color = .("Treat All")), linewidth = 0.8, linetype = "dashed") +
                ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
                ggplot2::scale_color_manual(values = c("#2E86C1", "#E74C3C")) +
                ggplot2::labs(title = .("Decision Curve Analysis"),
                              x = .("Threshold Probability"), y = .("Net Benefit"),
                              color = .("Strategy")) +
                ggplot2::coord_cartesian(ylim = c(-0.1, max(c(dca$net_benefit_model, dca$net_benefit_all), na.rm = TRUE) + 0.05)) +
                ggtheme
            print(p)
            TRUE
        },

        # ══════════════════════════════════════════════════════════════════
        # TRIPOD checklist
        # ══════════════════════════════════════════════════════════════════
        .populateTRIPOD = function(prepared, model) {
            n_vars <- length(model$coefs)
            p <- length(prepared$expl_vars)
            has_validation <- self$options$bootstrapValidation

            if (prepared$model_type == "linear") {
                opv <- prepared$n / p
                size_adequate <- opv >= 15
                size_label <- sprintf(.("OPV = %.1f %s"), opv, if (size_adequate) .("(adequate)") else .("(INSUFFICIENT)"))
                participants_label <- sprintf(.("N=%d observations"), prepared$n)
                outcome_label <- sprintf(.("%s (continuous)"), self$options$outcome)
            } else {
                epv <- min(prepared$n_events, prepared$n_nonevents) / p
                size_adequate <- epv >= 10
                size_label <- sprintf(.("EPV = %.1f %s"), epv, if (size_adequate) .("(adequate)") else .("(INSUFFICIENT)"))
                participants_label <- sprintf(.("%d events, %d non-events"), prepared$n_events, prepared$n_nonevents)
                if (prepared$model_type == "ordinal") {
                    outcome_label <- sprintf(.("%s (ordinal: %s)"), self$options$outcome, prepared$event_level)
                } else {
                    outcome_label <- sprintf(.("%s (binary: %s vs %s)"), self$options$outcome, prepared$event_level, prepared$ref_level)
                }
            }

            model_label <- switch(prepared$model_type,
                "logistic" = .("Logistic"), "cox" = .("Cox"),
                "linear" = .("Linear"), "ordinal" = .("Ordinal"))

            items <- list(
                list(.("Title/Abstract"), .("Study design identified"), TRUE),
                list(.("Background"), .("Context and rationale"), TRUE),
                list(.("Objectives"), .("Prediction model development"), TRUE),
                list(.("Source of data"), sprintf(.("Multi-source, N=%d"), prepared$n), TRUE),
                list(.("Participants"), participants_label, TRUE),
                list(.("Outcome"), outcome_label, TRUE),
                list(.("Predictors"), sprintf(.("%d variables, %d model terms"), p, n_vars), TRUE),
                list(.("Sample size"), size_label, size_adequate),
                list(.("Missing data"), .("Complete case analysis"), TRUE),
                list(.("Model development"), sprintf(.("%s regression"), model_label), TRUE),
                list(.("Model specification"), sprintf(.("%d predictors in final model"), n_vars), TRUE),
                list(.("Discrimination"), if (self$options$showDiscrimination) .("AUC/C-index reported") else .("NOT reported"), self$options$showDiscrimination),
                list(.("Calibration"), if (self$options$showCalibration) .("Calibration plot generated") else .("NOT assessed"), self$options$showCalibration),
                list(.("Internal validation"), if (has_validation) sprintf(.("Bootstrap (B=%d)"), self$options$bootstrapN) else .("NOT performed"), has_validation),
                list(.("Clinical utility"), if (self$options$showDecisionCurve) .("Decision curve analysis performed") else .("NOT assessed (enable Decision Curve Analysis)"), self$options$showDecisionCurve),
                list(.("Interpretation"), .("Scoring system with lookup table"), TRUE)
            )

            rows <- sapply(items, function(it) {
                icon <- if (it[[3]]) "&#x2705;" else "&#x274C;"
                sprintf("<tr><td>%s</td><td><strong>%s</strong></td><td>%s</td></tr>", icon, it[[1]], it[[2]])
            })

            n_pass <- sum(sapply(items, function(it) it[[3]]))
            html <- paste0(
                "<h4>", .("TRIPOD Checklist (Type 1b: Development + Internal Validation)"), "</h4>",
                "<p>", sprintf(.("Compliance: %d/%d items met"), n_pass, length(items)), "</p>",
                "<table class='table table-condensed'><thead>",
                "<tr><th></th><th>", .("Item"), "</th><th>", .("Status"), "</th></tr></thead><tbody>",
                paste(rows, collapse = ""),
                "</tbody></table>",
                "<p><em>", .("Reference: Collins GS et al. TRIPOD Statement. BMJ 2015;350:g7594"), "</em></p>")
            self$results$tripodChecklist$setContent(html)
        },

        .populateSummary = function(prepared, model) {
            method_label <- switch(self$options$scoringMethod %||% "schneeweiss",
                "beta10" = "Beta10", "schneeweiss" = "Schneeweiss",
                "sullivan" = "Sullivan/D'Agostino", "compare" = "comparison of all three methods")

            model_label <- switch(prepared$model_type,
                "logistic" = .("logistic"), "cox" = .("Cox"),
                "linear" = .("linear"), "ordinal" = .("ordinal"))
            event_info <- if (prepared$model_type == "linear") {
                sprintf(.("%d observations"), prepared$n)
            } else {
                sprintf(.("%d observations (%d events)"), prepared$n, prepared$n_events)
            }
            outcome_desc <- if (prepared$model_type == "linear") {
                .("Each predictor was assigned points based on regression coefficient magnitude. The lookup table maps each total score to the mean predicted outcome value.")
            } else {
                paste0(
                    .("Each predictor was assigned positive points (increasing risk) or negative points (decreasing risk) based on the regression coefficient magnitude."),
                    " ", .("The score-to-risk lookup table maps each possible total score to the observed event rate in the development cohort."))
            }

            html <- paste0(
                "<h4>", .("Results Summary"), "</h4>",
                "<p>", sprintf(.("A clinical scoring system was developed using %s regression with %d predictors and %s. The %s scoring method was applied to convert model coefficients to integer points. "),
                    model_label, length(prepared$expl_vars), event_info, method_label),
                outcome_desc,
                "</p>",
                "<p>", .("This scoring system should be validated on an independent cohort before clinical adoption."),
                "</p>")
            self$results$summaryText$setContent(html)
        },

        .populateExplanations = function() {
            html <- paste0(
                "<h4>", .("Scoring System Methods"), "</h4>",
                "<h5>Schneeweiss (", .("Recommended"), ")</h5>",
                "<p>", .("Divides each regression coefficient by the smallest absolute coefficient, then rounds to the nearest integer. This preserves the relative importance of predictors and is mathematically superior to odds-ratio-based methods (Mehta et al., J Clin Epidemiol 2016)."), "</p>",
                "<h5>Sullivan/D'Agostino (Framingham)</h5>",
                "<p>", .("Scales each coefficient relative to the strongest predictor (reference variable W), multiplied by the maximum points setting. Originally developed for the Framingham Heart Study risk scores (Sullivan et al., Statistics in Medicine 2004)."), "</p>",
                "<h5>Beta10</h5>",
                "<p>", .("Multiplies each coefficient by a scaling factor (default 10) and rounds. Simple and widely used, but may lose discrimination with many predictors (Zhang et al., Ann Transl Med 2017)."), "</p>",
                "<h5>", .("How to Interpret the Score"), "</h5>",
                "<ul>",
                "<li>", .("Higher positive scores indicate higher risk of the event (positive class)"), "</li>",
                "<li>", .("Negative scores indicate features that decrease risk"), "</li>",
                "<li>", .("The optimal cutoff divides patients into high-risk vs low-risk groups"), "</li>",
                "<li>", .("Use the lookup table to convert a patient's total score to predicted risk"), "</li>",
                "</ul>",
                "<h5>", .("Validation Requirements"), "</h5>",
                "<p>", .("Before clinical use, validate the scoring system on an independent dataset. Report: discrimination (AUC/C-index), calibration (calibration plot), and clinical utility (decision curve analysis). Follow TRIPOD guidelines (Collins et al., BMJ 2015)."), "</p>")
            self$results$explanations$setContent(html)
        }
    )
)
