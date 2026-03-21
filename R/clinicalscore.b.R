#' @title Clinical Scoring System Generator
#' @description
#' Builds clinical integer-point scoring systems from logistic or Cox regression
#' models using three published methods: Sullivan/D'Agostino (2004), Schneeweiss
#' (Mehta et al. 2016), and Beta10 (Zhang et al. 2017). Includes automatic variable
#' categorization, calibration/discrimination assessment, nomogram generation,
#' score-to-probability lookup, bootstrap validation, and TRIPOD compliance.
#'
#' @importFrom R6 R6Class
#' @import jmvcore

clinicalscoreClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "clinicalscoreClass",
    inherit = clinicalscoreBase,
    private = list(

        # Store for plot reuse
        .model_data = NULL,

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
                length(self$options$explanatory) == 0) {
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

            set.seed(self$options$random_seed)

            # ── 1. Clean and prepare data ──────────────────────────────
            prepared <- tryCatch(private$.prepareData(), error = function(e) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'><h4>", .("Data Error"), "</h4><p>", e$message, "</p></div>"))
                notice <- jmvcore::Notice$new(options = self$options,
                    name = 'dataError', type = jmvcore::NoticeType$ERROR)
                notice$setContent(sprintf(.('Data preparation failed: %s'), e$message))
                self$results$insert(1, notice)
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
                notice <- jmvcore::Notice$new(options = self$options,
                    name = 'modelError', type = jmvcore::NoticeType$ERROR)
                notice$setContent(sprintf(.('Model fitting failed: %s'), e$message))
                self$results$insert(1, notice)
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
            notice <- jmvcore::Notice$new(options = self$options,
                name = 'analysisComplete', type = jmvcore::NoticeType$INFO)
            notice$setContent(
                sprintf(.('Scoring system built using %s regression with %d predictors (N=%d, %d events). Method: %s.'),
                    if (prepared$model_type == "logistic") .("logistic") else .("Cox"),
                    length(prepared$expl_vars), prepared$n, prepared$n_events,
                    self$options$scoringMethod))
            self$results$insert(999, notice)
        },

        # ══════════════════════════════════════════════════════════════════
        # Data preparation
        # ══════════════════════════════════════════════════════════════════
        .prepareData = function() {
            data <- self$data
            outcome_var <- self$options$outcome
            expl_vars <- self$options$explanatory
            model_type <- self$options$modelType

            # Extract outcome
            outcome_raw <- data[[outcome_var]]
            if (model_type == "logistic") {
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
                    event_level <- "1"; ref_level <- "0"
                }
            } else {
                y <- jmvcore::toNumeric(outcome_raw)
                event_level <- as.character(self$options$outcomeLevel)
                if (is.null(event_level) || !nzchar(event_level)) event_level <- "1"
                ref_level <- "0"
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

            n_events <- if (model_type == "logistic") sum(y == 1, na.rm = TRUE) else sum(y == 1, na.rm = TRUE)

            list(
                y = y, predictors = predictors, time = time_val,
                n = n_complete, n_events = n_events,
                n_nonevents = n_complete - n_events,
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
            labs[1] <- sprintf("%s <= %.1f", varname, breaks[1])
            if (n_groups > 2) {
                for (i in 2:(n_groups - 1)) {
                    labs[i] <- sprintf("%.1f < %s <= %.1f", breaks[i-1], varname, breaks[i])
                }
            }
            labs[n_groups] <- sprintf("%s > %.1f", varname, breaks[length(breaks)])
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
                            sensitivity = NA, specificity = NA))
            }

            auc_val <- NA
            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_obj <- pROC::roc(y, scores, quiet = TRUE)
                    auc_val <- as.numeric(pROC::auc(roc_obj))
                }
            }, error = function(e) {})

            # Youden-optimal cutoff
            score_vals <- sort(unique(scores))
            best_youden <- -Inf; best_cutoff <- score_vals[1]
            for (cutoff in score_vals) {
                pred <- as.integer(scores >= cutoff)
                sens <- sum(pred == 1 & y == 1) / max(sum(y == 1), 1)
                spec <- sum(pred == 0 & y == 0) / max(sum(y == 0), 1)
                youden <- sens + spec - 1
                if (youden > best_youden) {
                    best_youden <- youden; best_cutoff <- cutoff
                }
            }

            pred <- as.integer(scores >= best_cutoff)
            tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
            fn <- sum(pred == 0 & y == 1)
            sens <- tp / max(tp + fn, 1)
            spec <- sum(pred == 0 & y == 0) / max(sum(y == 0), 1)
            acc <- mean(pred == y)

            list(auc = auc_val, cutoff = best_cutoff, accuracy = acc,
                 sensitivity = sens, specificity = spec)
        },

        # ══════════════════════════════════════════════════════════════════
        # Populate results
        # ══════════════════════════════════════════════════════════════════
        .assessSuitability = function(prepared) {
            p <- length(prepared$expl_vars)
            epv <- min(prepared$n_events, prepared$n_nonevents) / p

            checks <- c()
            icons <- c(green = "&#x2705;", yellow = "&#x26A0;&#xFE0F;", red = "&#x274C;")

            # EPV
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

            # Sample size
            status <- if (prepared$n >= 100) "green" else if (prepared$n >= 50) "yellow" else "red"
            checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>N = %d</td></tr>",
                icons[status], .("Sample size"), prepared$n))

            # Events
            status <- if (prepared$n_events >= 50) "green" else if (prepared$n_events >= 20) "yellow" else "red"
            checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>%d events, %d non-events</td></tr>",
                icons[status], .("Event count"), prepared$n_events, prepared$n_nonevents))

            # Minimum recommended N (Riley et al. 2020)
            min_n <- max(10 * p / min(prepared$n_events/prepared$n, 1 - prepared$n_events/prepared$n), 100)
            status <- if (prepared$n >= min_n) "green" else "red"
            checks <- c(checks, sprintf("<tr><td>%s</td><td>%s</td><td>N = %d vs recommended minimum %d (Riley et al. 2020)</td></tr>",
                icons[status], .("Minimum sample size"), prepared$n, ceiling(min_n)))

            html <- paste0(
                "<h4>", .("Data Suitability Assessment"), "</h4>",
                "<table class='table table-condensed'><thead>",
                "<tr><th></th><th>", .("Check"), "</th><th>", .("Result"), "</th></tr></thead><tbody>",
                paste(checks, collapse = ""),
                "</tbody></table>")
            self$results$suitabilityReport$setContent(html)

            # Surface critical issues as Notices
            if (epv < 5) {
                notice <- jmvcore::Notice$new(options = self$options,
                    name = 'epvCritical', type = jmvcore::NoticeType$STRONG_WARNING)
                notice$setContent(
                    sprintf(.('EPV = %.1f (<5): high overfitting risk. Reduce to %d or fewer predictors, or collect more data.'),
                            epv, floor(min(prepared$n_events, prepared$n_nonevents) / 5)))
                self$results$insert(1, notice)
            } else if (epv < 10) {
                notice <- jmvcore::Notice$new(options = self$options,
                    name = 'epvMarginal', type = jmvcore::NoticeType$WARNING)
                notice$setContent(
                    sprintf(.('EPV = %.1f (marginal). Enable bootstrap validation to assess overfitting risk.'), epv))
                self$results$insert(1, notice)
            }

            if (prepared$n < ceiling(min_n)) {
                notice <- jmvcore::Notice$new(options = self$options,
                    name = 'sampleSizeInsufficient', type = jmvcore::NoticeType$STRONG_WARNING)
                notice$setContent(
                    sprintf(.('Sample size N=%d is below the recommended minimum of %d (Riley et al. 2020). Results may not generalize.'),
                            prepared$n, ceiling(min_n)))
                self$results$insert(1, notice)
            }
        },

        .populateModelSummary = function(prepared, model) {
            table <- self$results$modelSummary
            effect_label <- if (prepared$model_type == "logistic") .("Odds Ratio") else .("Hazard Ratio")
            rows <- list(
                list(.("Model type"), if (prepared$model_type == "logistic") .("Logistic Regression") else .("Cox Regression")),
                list(.("Total observations"), as.character(prepared$n)),
                list(.("Events"), sprintf("%d (%s)", prepared$n_events, prepared$event_level)),
                list(.("Non-events"), sprintf("%d (%s)", prepared$n_nonevents, prepared$ref_level)),
                list(.("Predictors"), as.character(length(prepared$expl_vars))),
                list(.("Model coefficients"), as.character(length(model$coefs))),
                list(.("Effect measure"), effect_label),
                list(.("EPV"), sprintf("%.1f", min(prepared$n_events, prepared$n_nonevents) / length(prepared$expl_vars)))
            )
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
                    "sullivan"="Sullivan/D'Agostino", "compare"="Schneeweiss (primary)")),
                list(.("Score AUC / C-index"), sprintf("%.3f", perf$auc)),
                list(.("Optimal cutoff"), as.character(perf$cutoff)),
                list(.("Accuracy"), sprintf("%.3f", perf$accuracy)),
                list(.("Sensitivity"), sprintf("%.3f", perf$sensitivity)),
                list(.("Specificity"), sprintf("%.3f", perf$specificity)),
                list(.("Score range"), sprintf("%.0f to %.0f", min(total_scores), max(total_scores)))
            )
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
                    perf_j <- private$.evaluateScore(prepared$y, scores_j)
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
                    n_c <- sum(idx); n_e <- sum(prepared$y[idx] == 1)
                    prob <- n_e / n_c
                    risk <- if (s >= perf$cutoff) .("High risk") else .("Low risk")
                    lookup$addRow(rowKey = as.character(s), values = list(
                        score = as.integer(s), n_cases = n_c, n_events = n_e,
                        probability = prob, risk_group = risk))
                }
            }

            # Store for plots
            private$.model_data$total_scores <- total_scores
            private$.model_data$cutoff <- perf$cutoff
        },

        .populateDiscrimination = function(prepared, model) {
            table <- self$results$discriminationTable
            metric_label <- if (prepared$model_type == "logistic") "AUC" else "C-index"

            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    roc_obj <- pROC::roc(prepared$y, model$predicted, quiet = TRUE)
                    auc_val <- as.numeric(pROC::auc(roc_obj))
                    ci_obj <- pROC::ci.auc(roc_obj, method = "delong")
                    interp <- if (auc_val >= 0.9) .("Excellent") else if (auc_val >= 0.8) .("Good")
                              else if (auc_val >= 0.7) .("Acceptable") else .("Poor")
                    table$addRow(rowKey = 1, values = list(
                        metric = metric_label, estimate = auc_val,
                        ci_lower = ci_obj[1], ci_upper = ci_obj[3], interpretation = interp))

                    # Notice for poor discrimination
                    if (auc_val < 0.7) {
                        notice <- jmvcore::Notice$new(options = self$options,
                            name = 'poorDiscrimination', type = jmvcore::NoticeType$WARNING)
                        notice$setContent(
                            sprintf(.('%s = %.3f indicates poor discrimination. Consider adding more informative predictors.'),
                                    metric_label, auc_val))
                        self$results$insert(1, notice)
                    } else if (auc_val > 0.95 && prepared$n < 100) {
                        notice <- jmvcore::Notice$new(options = self$options,
                            name = 'overfitRisk', type = jmvcore::NoticeType$STRONG_WARNING)
                        notice$setContent(
                            sprintf(.('%s = %.3f with N=%d: likely overfitted. Enable bootstrap validation for corrected estimate.'),
                                    metric_label, auc_val, prepared$n))
                        self$results$insert(1, notice)
                    }
                }
            }, error = function(e) {})

            # Brier score
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

            apparent_auc <- NA
            tryCatch({
                if (requireNamespace("pROC", quietly = TRUE)) {
                    apparent_auc <- as.numeric(pROC::auc(pROC::roc(prepared$y, model$predicted, quiet = TRUE)))
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
                    } else {
                        df_boot$surv <- survival::Surv(prepared$time[idx], prepared$y[idx])
                        m_boot <- survival::coxph(surv ~ . - y, data = df_boot)
                        pred_boot <- predict(m_boot, type = "risk")
                        pred_orig <- predict(m_boot, newdata = data.frame(y = prepared$y, prepared$predictors,
                            surv = survival::Surv(prepared$time, prepared$y)), type = "risk")
                    }

                    if (requireNamespace("pROC", quietly = TRUE)) {
                        auc_boot <- as.numeric(pROC::auc(pROC::roc(df_boot$y, pred_boot, quiet = TRUE)))
                        auc_orig <- as.numeric(pROC::auc(pROC::roc(prepared$y, pred_orig, quiet = TRUE)))
                        optimism_vals[b] <- auc_boot - auc_orig
                    }
                }, error = function(e) {})
            }

            mean_opt <- mean(optimism_vals, na.rm = TRUE)
            corrected <- apparent_auc - mean_opt

            table$addRow(rowKey = 1, values = list(
                metric = if (model_type == "logistic") "AUC" else "C-index",
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
                } else {
                    df_rms <- data.frame(y = prepared$y, time = prepared$time, prepared$predictors)
                    surv_obj <- survival::Surv(df_rms$time, df_rms$y)
                    fit <- rms::cph(surv_obj ~ . - y - time, data = df_rms)
                }

                nom <- rms::nomogram(fit, fun = if (prepared$model_type == "logistic") plogis else NULL,
                                     funlabel = if (prepared$model_type == "logistic") .("Risk") else .("Log Relative Hazard"))
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

            df <- data.frame(
                score = md$total_scores,
                outcome = factor(ifelse(md$prepared$y == 1, md$prepared$event_level, md$prepared$ref_level))
            )

            p <- ggplot2::ggplot(df, ggplot2::aes(x = score, fill = outcome)) +
                ggplot2::geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
                ggplot2::geom_vline(xintercept = md$cutoff, linetype = "dashed", color = "red", linewidth = 1) +
                ggplot2::annotate("text", x = md$cutoff, y = Inf, vjust = 2,
                                  label = sprintf(.("Cutoff = %d"), md$cutoff), color = "red", size = 3.5) +
                ggplot2::scale_fill_manual(values = c("#2E86C1", "#E74C3C")) +
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
            if (prepared$model_type != "logistic") return()

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
            epv <- min(prepared$n_events, prepared$n_nonevents) / length(prepared$expl_vars)
            has_validation <- self$options$bootstrapValidation

            items <- list(
                list(.("Title/Abstract"), .("Study design identified"), TRUE),
                list(.("Background"), .("Context and rationale"), TRUE),
                list(.("Objectives"), .("Prediction model development"), TRUE),
                list(.("Source of data"), sprintf(.("Multi-source, N=%d"), prepared$n), TRUE),
                list(.("Participants"), sprintf(.("%d events, %d non-events"), prepared$n_events, prepared$n_nonevents), TRUE),
                list(.("Outcome"), sprintf(.("%s (binary: %s vs %s)"), self$options$outcome, prepared$event_level, prepared$ref_level), TRUE),
                list(.("Predictors"), sprintf(.("%d variables, %d model terms"), length(prepared$expl_vars), n_vars), TRUE),
                list(.("Sample size"), sprintf(.("EPV = %.1f %s"), epv, if (epv >= 10) .("(adequate)") else .("(INSUFFICIENT)")), epv >= 10),
                list(.("Missing data"), .("Complete case analysis"), TRUE),
                list(.("Model development"), sprintf(.("%s regression"), if (prepared$model_type == "logistic") .("Logistic") else .("Cox")), TRUE),
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

            html <- paste0(
                "<h4>", .("Results Summary"), "</h4>",
                "<p>", sprintf(.("A clinical scoring system was developed using %s regression with %d predictors and %d observations (%d events). The %s scoring method was applied to convert model coefficients to integer points. "),
                    if (prepared$model_type == "logistic") .("logistic") else .("Cox"),
                    length(prepared$expl_vars), prepared$n, prepared$n_events, method_label),
                .("Each predictor was assigned positive points (increasing risk) or negative points (decreasing risk) based on the regression coefficient magnitude."),
                "</p>",
                "<p>", .("The score-to-risk lookup table maps each possible total score to the observed event rate in the development cohort. "),
                .("This scoring system should be validated on an independent cohort before clinical adoption."),
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
