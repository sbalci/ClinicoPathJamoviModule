#' @title LASSO Logistic Regression for Binary Classification
#' @description
#' Performs LASSO-penalized logistic regression for variable selection in binary
#' classification problems. Supports LASSO, Ridge, and Elastic Net penalties.
#' Includes suitability assessment, bootstrap validation, ROC analysis, and
#' automated scoring system generation.
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @return An \code{R6} class generator object for the \code{lassologisticClass} backend; used internally by the jamovi analysis wrapper and not called directly.

lassologisticClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
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
                    type = type, title = title, content = content
                )
            },
            .renderNotices = function() {
                if (length(private$.noticeList) == 0) {
                    return()
                }

                typeStyles <- list(
                    ERROR          = list(color = "#dc2626", bgcolor = "#fef2f2", border = "#fca5a5"),
                    STRONG_WARNING = list(color = "#ea580c", bgcolor = "#fff7ed", border = "#fdba74"),
                    WARNING        = list(color = "#ca8a04", bgcolor = "#fefce8", border = "#fde047"),
                    INFO           = list(color = "#2563eb", bgcolor = "#eff6ff", border = "#93c5fd")
                )

                html <- "<div style='margin: 10px 0;'>"
                for (notice in private$.noticeList) {
                    style <- typeStyles[[notice$type]]
                    if (is.null(style)) style <- typeStyles$INFO
                    html <- paste0(
                        html,
                        "<div style='background-color: ", style$bgcolor, "; ",
                        "border-left: 4px solid ", style$border, "; ",
                        "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                        "<strong style='color: ", style$color, ";'>",
                        htmltools::htmlEscape(notice$title), "</strong><br>",
                        "<span style='color: #374151;'>",
                        htmltools::htmlEscape(notice$content), "</span>",
                        "</div>"
                    )
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
                # Reset accumulated notices at the top of every run: jamovi reuses
                # the R6 instance across re-runs and .renderNotices uses setContent,
                # so without this the same notice re-renders N times. Matches the
                # documented reset pattern used to fix the accumulation bug in
                # survival.b.R.
                private$.noticeList <- list()
                private$.cutFellback <- character(0)

                # Every table is rebuilt with addRow() below and jmvcore never checks for
                # duplicate row keys, so an option toggle that is not in a table's
                # clearWith (showSummary, cv_plot, ...) re-ran .run() and doubled its rows.
                # One reset here covers all ten tables.
                for (nm in c("modelSummary", "coefficients", "performance", "scoringTable",
                             "scoringPerformance", "methodComparison", "lookupTable",
                             "validationTable", "variableImportance", "modelComparison")) {
                    tbl <- tryCatch(self$results[[nm]], error = function(e) NULL)
                    if (!is.null(tbl)) tbl$deleteRows()
                }

                if (is.null(self$options$outcome) ||
                    is.null(self$options$explanatory) ||
                    length(self$options$explanatory) < 2) {
                    # .init() shows the welcome/To Do panel only when explanatory is
                    # NULL or empty, so with EXACTLY ONE predictor selected both
                    # guards stayed silent and the user got a completely blank
                    # result - no panel, no notice, no error.
                    if (!is.null(self$options$outcome) &&
                        !is.null(self$options$explanatory) &&
                        length(self$options$explanatory) == 1) {
                        private$.addNotice(
                            "WARNING",
                            .("At least two predictors are required"),
                            .("LASSO performs variable SELECTION, so it needs a set of candidate predictors to choose among; with a single predictor there is nothing to select and a penalized fit is not informative. Add at least one more predictor, or use ordinary logistic regression for a single-predictor model.")
                        )
                        private$.renderNotices()
                    }
                    return()
                }

                set.seed(self$options$random_seed)

                # ── 1. Clean data ──────────────────────────────────────────────
                data <- tryCatch(private$.cleanData(), error = function(e) {
                    msg_html <- htmltools::htmlEscape(e$message)
                    self$results$todo$setContent(paste0(
                        "<div class='alert alert-danger'><h4>Data Error</h4><p>", msg_html, "</p></div>"
                    ))
                    private$.addNotice(
                        "ERROR", .("Data Error"),
                        sprintf(.("Data preparation failed: %s"), e$message)
                    )
                    private$.renderNotices()
                    return(NULL)
                })
                if (is.null(data)) {
                    return()
                }

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
                        "<div class='alert alert-danger'><h4>Model Fitting Error</h4><p>", msg_html, "</p></div>"
                    ))
                    private$.addNotice(
                        "ERROR", .("Model Fitting Error"),
                        sprintf(.("LASSO model fitting failed: %s"), e$message)
                    )
                    private$.renderNotices()
                    return(NULL)
                })
                if (is.null(fit_result)) {
                    return()
                }

                # ── Notice: no variables selected ──────────────────────────────
                if (length(fit_result$selected) == 0) {
                    # The advice used to say "try lambda.min instead of lambda.1se"
                    # unconditionally - useless, and slightly insulting, to a user
                    # who is already on lambda.min. Offer the remedy that is left.
                    remedy <- if (identical(self$options$lambda, "lambda.1se"))
                        .("Try the Minimum CV Error lambda instead of the 1SE rule, or add more informative predictors.")
                    else
                        .("You are already using the least conservative lambda (Minimum CV Error), so the penalty is not the limiting factor: none of these predictors carries enough signal at this sample size. Add more informative predictors or collect more cases.")
                    private$.addNotice(
                        "STRONG_WARNING", .("No Variables Selected"),
                        sprintf(.("LASSO selected zero variables at the chosen lambda. %s"), remedy)
                    )
                }

                # Listwise deletion is dominated by the single worst-populated
                # predictor, so a biomarker panel with one sparsely-stained marker
                # silently becomes a different-cohort analysis. Name the actual
                # cause: rows dropped for an out-of-model outcome level are NOT a
                # missing-predictor problem and must not send the reader hunting one.
                if (!is.null(data$n_excluded) && data$n_excluded > 0) {
                    reasons <- character(0)
                    if (isTRUE(data$n_excl_pred > 0))
                        reasons <- c(reasons, sprintf(
                            .("%d had at least one predictor missing"), data$n_excl_pred))
                    if (isTRUE(data$n_excl_outcome_na > 0))
                        reasons <- c(reasons, sprintf(
                            .("%d had a missing outcome"), data$n_excl_outcome_na))
                    if (isTRUE(data$n_excl_outcome_lvl > 0))
                        reasons <- c(reasons, sprintf(
                            .("%d were in an outcome level outside the two being compared"),
                            data$n_excl_outcome_lvl))
                    breakdown <- if (length(reasons) > 0)
                        paste0(" (", paste(reasons, collapse = "; "), ")") else ""
                    # No leading space inside .(): translators must receive the
                    # sentence, not the spacing around it.
                    advice <- if (isTRUE(data$n_excl_pred > 0))
                        paste0(" ", .("LASSO uses listwise deletion, so a single sparsely-measured predictor can remove a large share of the cohort; check which predictor is driving the exclusions before interpreting these results."))
                    else ""
                    private$.addNotice(
                        "WARNING",
                        .("Cases excluded from the analysis"),
                        sprintf(
                            .("%d of %d cases (%.1f%%) were excluded%s; the analysis uses the remaining %d cases.%s"),
                            data$n_excluded, data$n_total,
                            100 * data$n_excluded / data$n_total, breakdown, data$n, advice)
                    )
                }

                # Constant predictors are dropped in .cleanData; say so, otherwise
                # "Candidate predictors" silently disagrees with what was selected.
                if (isTRUE(data$n_dropped_constant > 0)) {
                    private$.addNotice(
                        "WARNING",
                        .("Constant predictors removed"),
                        sprintf(
                            .("%d selected variable(s) had the same value in every case and were removed before fitting: %s. A constant carries no information and cannot be penalised or selected. %d variables remain and are shown as Variables analysed."),
                            data$n_dropped_constant,
                            paste(data$dropped_constant, collapse = ", "), data$n_vars)
                    )
                }

                private$.checkpoint()

                # ── 4. Populate results ────────────────────────────────────────
                private$.populateModelSummary(data, fit_result)
                private$.populateCoefficients(fit_result, data)
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
                private$.addNotice(
                    "INFO", .("Analysis Complete"),
                    sprintf(
                        .("Penalized logistic regression completed: %d/%d model terms selected using the %s penalty with the %s lambda (N=%d, %d events)."),
                        n_sel, data$p, private$.penaltyLabel(), private$.lambdaLabel(), data$n, data$n_events
                    )
                )

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
                    # Preserve the variable's declared level ordering for factors
                    # (mirrors oddsratio) so the default "second level" event class
                    # matches levels(), not alphabetical order. Character outcomes
                    # fall back to sorted unique values.
                    if (is.factor(outcome_raw)) {
                        observed_levels <- intersect(levels(outcome_raw), unique(outcome_chr[!is.na(outcome_chr)]))
                    } else {
                        observed_levels <- sort(unique(outcome_chr[!is.na(outcome_chr)]))
                    }
                    if (length(observed_levels) < 2) {
                        jmvcore::reject(.("Outcome variable must have at least 2 observed values."))
                    }
                    outcome_level_opt <- self$options$outcomeLevel
                    if (is.null(outcome_level_opt) || !nzchar(as.character(outcome_level_opt))) {
                        event_level <- observed_levels[2]
                    } else {
                        event_level <- as.character(outcome_level_opt)
                        if (!event_level %in% observed_levels) {
                            jmvcore::reject(sprintf(
                                .("Specified event level '%s' not found in the outcome variable. Observed levels: %s."),
                                event_level, paste(observed_levels, collapse = ", ")
                            ))
                        }
                    }
                    if (length(observed_levels) > 2) {
                        private$.addNotice(
                            "WARNING", .("Non-Binary Outcome"),
                            sprintf(
                                .("Outcome has %d observed levels; only '%s' (event) vs '%s' (reference) are modeled. Cases in other levels are excluded."),
                                length(observed_levels), event_level, setdiff(observed_levels, event_level)[1]
                            )
                        )
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
                        if (is.na(event_level_num) || !event_level_num %in% observed_levels) {
                            jmvcore::reject(sprintf(
                                .("Specified event level '%s' not found in the outcome variable. Observed values: %s."),
                                as.character(outcome_level_opt), paste(observed_levels, collapse = ", ")
                            ))
                        }
                    } else if (all(observed_levels %in% c(0, 1))) {
                        event_level_num <- 1
                    } else {
                        event_level_num <- max(observed_levels)
                    }
                    if (length(observed_levels) > 2) {
                        private$.addNotice(
                            "WARNING", .("Non-Binary Outcome"),
                            sprintf(
                                .("Outcome has %d distinct numeric values; only %s (event) vs %s (reference) are modeled. Other cases are excluded."),
                                length(observed_levels), as.character(event_level_num),
                                as.character(setdiff(observed_levels, event_level_num)[1])
                            )
                        )
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
                # Report what was removed. Silently shrinking the candidate set made
                # "Candidate predictors" in the Model Summary smaller than what the
                # user actually selected, with nothing on screen saying why.
                dropped_constant <- names(predictors)[constant_vars]
                n_dropped_constant <- length(dropped_constant)
                if (any(constant_vars)) {
                    predictors <- predictors[, !constant_vars, drop = FALSE]
                    explanatory_vars <- names(predictors)
                }
                if (ncol(predictors) < 2) jmvcore::reject(.("Fewer than 2 non-constant predictors remain."))

                # Complete-case filtering
                complete <- complete.cases(status, predictors)
                n_complete <- sum(complete)
                if (n_complete < 10) jmvcore::reject(.("Too few complete cases for analysis."))

                # Why each row went away. These three are a partition of !complete
                # (outcome is judged first, then predictors), because reporting the
                # total under one heading was actively misleading: with a 5-level
                # outcome the 59 rows dropped for belonging to a non-modelled level
                # were announced as "excluded because at least one selected variable
                # was missing", sending the reader off to hunt a sparsely-measured
                # predictor that does not exist.
                outcome_na <- is.na(outcome_raw)
                n_excl_outcome_na <- sum(outcome_na)
                n_excl_outcome_lvl <- sum(!outcome_na & is.na(status))
                n_excl_pred <- sum(!is.na(status) & !complete.cases(predictors))

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
                X <- tryCatch(
                    {
                        mm <- model.matrix(~., data = pred_cc)
                        mm[, -1, drop = FALSE] # remove intercept
                    },
                    error = function(e) {
                        # reject()'s signature is reject(formats, code = NULL, ...) -
                        # a bare second positional argument binds to code= and never
                        # reaches the {} placeholder, so this used to print the
                        # literal "Design matrix error: {}" to the user.
                        jmvcore::reject(.("Design matrix error: {}"), code = NULL, e$message)
                    }
                )

                # model.matrix deparses a NON-SYNTACTIC column name with backticks,
                # so "Ki-67 (%)" arrives as `Ki-67 (%)`. Those leaked verbatim into
                # the Selected Variables and Scoring System tables, and made the
                # manual cut point the user typed ("Ki-67 (%)=20") fail to match,
                # silently falling back to the sample median. Shared with the eight
                # other analyses that build a design matrix the same way; see
                # .stripBackticks in R/utils.R for the full rationale.
                X <- .stripBackticks(X)

                # Remove degenerate columns
                col_vars <- apply(X, 2, var, na.rm = TRUE)
                good_cols <- !is.na(col_vars) & col_vars > 0
                if (sum(good_cols) < 2) jmvcore::reject(.("Too few non-degenerate predictor columns."))
                X <- X[, good_cols, drop = FALSE]

                # Optional standardization.
                #
                # Keep the centre/scale so downstream output can be expressed on the
                # ORIGINAL measurement scale. glmnet's own standardize=TRUE returns
                # coefficients back-transformed to the original scale; because this
                # code scales the matrix itself and then passes standardize=FALSE,
                # no back-transformation happens and every coefficient stays on the
                # z-scale. beta_original = beta_z / sd reproduces exactly what
                # glmnet(standardize=TRUE) would have returned.
                X_center <- rep(0, ncol(X))
                X_sd <- rep(1, ncol(X))
                names(X_center) <- names(X_sd) <- colnames(X)
                if (self$options$standardize) {
                    X <- scale(X)
                    ctr <- attr(X, "scaled:center")
                    scl <- attr(X, "scaled:scale")
                    if (!is.null(ctr)) X_center[names(ctr)] <- ctr
                    if (!is.null(scl)) X_sd[names(scl)] <- scl
                    # a zero sd would make the back-transform infinite
                    X_sd[!is.finite(X_sd) | X_sd == 0] <- 1
                }

                list(
                    X = X,
                    X_center = X_center,
                    X_sd = X_sd,
                    y = status_cc,
                    n = n_complete,
                    n_total = nrow(self$data),
                    n_excluded = nrow(self$data) - n_complete,
                    n_excl_outcome_na = n_excl_outcome_na,
                    n_excl_outcome_lvl = n_excl_outcome_lvl,
                    n_excl_pred = n_excl_pred,
                    n_dropped_constant = n_dropped_constant,
                    dropped_constant = dropped_constant,
                    # ncol(X) counts design-matrix COLUMNS (a 5-level factor is 4 of
                    # them), so it cannot stand in for "variables you selected".
                    n_vars = length(explanatory_vars),
                    n_events = n_events,
                    n_nonevents = n_nonevents,
                    p = ncol(X),
                    complete_idx = which(complete),
                    event_level = event_level,
                    ref_level = ref_level,
                    explanatory_vars = explanatory_vars
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
                    checks$epv <- list(
                        status = "green", label = .("Events per variable"),
                        detail = sprintf(.("EPV = %.1f (>=10: adequate)"), epv)
                    )
                } else if (epv >= 5) {
                    checks$epv <- list(
                        status = "yellow", label = .("Events per variable"),
                        detail = sprintf(.("EPV = %.1f (5-10: marginal, results may be unstable)"), epv)
                    )
                } else {
                    checks$epv <- list(
                        status = "red", label = .("Events per variable"),
                        detail = sprintf(.("EPV = %.1f (<5: insufficient, high overfitting risk)"), epv)
                    )
                }

                # Sample size check
                if (data$n >= 100) {
                    checks$n <- list(
                        status = "green", label = .("Sample size"),
                        detail = sprintf(.("N = %d (>=100: adequate)"), data$n)
                    )
                } else if (data$n >= 50) {
                    checks$n <- list(
                        status = "yellow", label = .("Sample size"),
                        detail = sprintf(.("N = %d (50-100: marginal)"), data$n)
                    )
                } else {
                    checks$n <- list(
                        status = "red", label = .("Sample size"),
                        detail = sprintf(.("N = %d (<50: small, consider fewer predictors)"), data$n)
                    )
                }

                # Class balance
                minority_pct <- min(data$n_events, data$n_nonevents) / data$n * 100
                if (minority_pct >= 30) {
                    checks$balance <- list(
                        status = "green", label = .("Class balance"),
                        detail = sprintf(.("Minority class: %.1f%% (balanced)"), minority_pct)
                    )
                } else if (minority_pct >= 10) {
                    checks$balance <- list(
                        status = "yellow", label = .("Class balance"),
                        detail = sprintf(.("Minority class: %.1f%% (moderate imbalance)"), minority_pct)
                    )
                } else {
                    checks$balance <- list(
                        status = "red", label = .("Class balance"),
                        detail = sprintf(.("Minority class: %.1f%% (severe imbalance)"), minority_pct)
                    )
                }

                # Predictor count
                if (data$p <= data$n / 5) {
                    checks$p <- list(
                        status = "green", label = .("Predictor count"),
                        detail = sprintf(.("p = %d predictors, n/p = %.1f (good ratio)"), data$p, data$n / data$p)
                    )
                } else {
                    checks$p <- list(
                        status = "yellow", label = .("Predictor count"),
                        detail = sprintf(.("p = %d predictors, n/p = %.1f (regularization essential)"), data$p, data$n / data$p)
                    )
                }

                # Collinearity check
                cor_matrix <- tryCatch(cor(data$X, use = "pairwise.complete.obs"), error = function(e) NULL)
                max_cor <- NA
                if (!is.null(cor_matrix)) {
                    diag(cor_matrix) <- 0
                    max_cor <- max(abs(cor_matrix), na.rm = TRUE)
                    if (max_cor < 0.7) {
                        checks$collinearity <- list(
                            status = "green", label = .("Collinearity"),
                            detail = sprintf(.("Max |r| = %.2f (<0.7: acceptable)"), max_cor)
                        )
                    } else if (max_cor < 0.9) {
                        checks$collinearity <- list(
                            status = "yellow", label = .("Collinearity"),
                            detail = sprintf(.("Max |r| = %.2f (0.7-0.9: moderate, LASSO will handle)"), max_cor)
                        )
                    } else {
                        checks$collinearity <- list(
                            status = "red", label = .("Collinearity"),
                            detail = sprintf(.("Max |r| = %.2f (>=0.9: high, consider elastic net)"), max_cor)
                        )
                    }
                }

                # Render HTML
                icons <- c(green = "\u{2705}", yellow = "\u{26A0}\u{FE0F}", red = "\u{274C}")
                n_green <- sum(sapply(checks, function(x) x$status == "green"))
                n_yellow <- sum(sapply(checks, function(x) x$status == "yellow"))
                n_red <- sum(sapply(checks, function(x) x$status == "red"))

                if (n_red > 0) {
                    overall <- paste0("<span style='color:red;font-weight:bold;'>", .("Caution: Major concerns detected"), "</span>")
                } else if (n_yellow > 0) {
                    overall <- paste0("<span style='color:#cc8800;font-weight:bold;'>", .("Acceptable with caveats"), "</span>")
                } else {
                    overall <- paste0("<span style='color:green;font-weight:bold;'>", .("Data suitable for LASSO logistic"), "</span>")
                }

                rows <- sapply(checks, function(x) {
                    sprintf(
                        "<tr><td>%s</td><td>%s</td><td>%s</td></tr>",
                        icons[x$status], x$label, x$detail
                    )
                })

                html <- paste0(
                    "<h4>", .("Data Suitability Assessment"), "</h4>",
                    "<p>", .("Overall:"), " ", overall, "</p>",
                    "<table class='table table-condensed'><thead>",
                    "<tr><th></th><th>", .("Check"), "</th><th>", .("Result"), "</th></tr></thead><tbody>",
                    paste(rows, collapse = ""),
                    "</tbody></table>"
                )
                self$results$suitabilityReport$setContent(html)

                # Surface critical suitability issues as Notices
                if (n_red > 0) {
                    red_items <- paste(sapply(
                        checks[sapply(checks, function(x) x$status == "red")],
                        function(x) x$label
                    ), collapse = "; ")
                    private$.addNotice(
                        "STRONG_WARNING", .("Data Suitability"),
                        sprintf(
                            .("Data suitability: %d major concern(s) detected (%s). Results may be unreliable; consider reducing predictors or collecting more data."),
                            n_red, red_items
                        )
                    )
                } else if (n_yellow > 0) {
                    private$.addNotice(
                        "WARNING", .("Data Suitability"),
                        sprintf(
                            .("Data suitability: %d minor concern(s). Enable bootstrap validation to assess overfitting risk."),
                            n_yellow
                        )
                    )
                }
            },

            # ══════════════════════════════════════════════════════════════════
            # Fit LASSO logistic regression
            # ══════════════════════════════════════════════════════════════════
            # Stratified fold ids: events and non-events are each dealt round the folds, so
            # every fold holds events. Shared by the main fit and the bootstrap replicates so
            # the model-selection process being validated is the one actually validated.
            # NULL on failure lets cv.glmnet fall back to its own random folds.
            .stratifiedFolds = function(y, nfolds) {
                tryCatch({
                    pos_idx <- which(y == 1)
                    neg_idx <- which(y == 0)
                    foldid <- integer(length(y))
                    foldid[pos_idx] <- sample(rep(seq_len(nfolds), length.out = length(pos_idx)))
                    foldid[neg_idx] <- sample(rep(seq_len(nfolds), length.out = length(neg_idx)))
                    foldid
                }, error = function(e) NULL)
            },

            .fitLasso = function(data) {
                # Determine alpha
                alpha_val <- switch(self$options$penalty,
                    "lasso" = 1,
                    "ridge" = 0,
                    "elasticnet" = self$options$alpha,
                    1
                )

                # Adjust nfolds if needed.
                #
                # Capping at n-1 alone is not enough: the folds are STRATIFIED, so
                # asking for more folds than there are cases in the minority class
                # leaves some folds with no events at all. With the 5-event minimum
                # this module enforces and the default of 10 folds, half the folds
                # would carry zero events and their held-out deviance is not
                # estimating what the user thinks it is. Cap at the minority count.
                nfolds_requested <- self$options$nfolds
                min_class <- min(data$n_events, data$n_nonevents)
                nfolds <- min(nfolds_requested, data$n - 1, min_class)
                nfolds <- max(nfolds, 3)
                if (nfolds < nfolds_requested) {
                    private$.addNotice(
                        "WARNING", .("Cross-validation folds reduced"),
                        sprintf(
                            .("%d folds were requested but only %d could be used: stratified cross-validation cannot create more folds than there are cases in the smaller outcome class (%d). Fewer folds mean a noisier lambda; a larger or better-balanced sample is the real remedy."),
                            nfolds_requested, nfolds, min_class)
                    )
                }

                # Stratified CV folds for balanced sampling
                foldid <- private$.stratifiedFolds(data$y, nfolds)

                # Fit CV model
                cv_args <- list(
                    x = data$X,
                    y = data$y,
                    family = "binomial",
                    alpha = alpha_val,
                    standardize = FALSE, # already standardized if requested
                    type.measure = "deviance"
                )
                if (!is.null(foldid)) {
                    cv_args$foldid <- foldid
                } else {
                    cv_args$nfolds <- nfolds
                }

                cv_fit <- .quietly(do.call(glmnet::cv.glmnet, cv_args))

                # Select lambda
                lambda_optimal <- switch(self$options$lambda,
                    "lambda.min" = cv_fit$lambda.min,
                    "lambda.1se" = cv_fit$lambda.1se,
                    cv_fit$lambda.1se
                )

                # Final model: the cross-validated path itself. A separate single-lambda
                # glmnet() refit is discouraged by the glmnet authors and differed from the
                # path by up to 3e-5, so the coefficient table disagreed with the cv_plot and
                # with coef(cv_fit, s = lambda) that an R user would compute by hand.
                final_model <- cv_fit$glmnet.fit

                # Extract coefficients
                coefs <- as.matrix(coef(final_model, s = lambda_optimal))
                intercept <- coefs[1, 1]
                beta <- coefs[-1, 1]
                # glmnet does not always store an exact zero: coordinate descent can
                # leave a machine-noise value such as -1.7e-16 in the sparse beta.
                # An exact `beta != 0` test let that through, and because Importance
                # is abs(beta)/max(abs(beta)) a lone noise coefficient normalised to
                # 1.00 - the results table announced a "selected" predictor with
                # Odds Ratio 1.000 and Importance 1.000. Treat anything below
                # ZERO_TOL as the zero glmnet meant to write.
                # Test the SCALE-INVARIANT magnitude, not the raw coefficient. With
                # standardize = FALSE, beta is per raw unit, so a predictor measured
                # in the millions can carry a genuine effect at beta ~ 1e-8; a flat
                # cutoff would erase it. |beta| * sd(column) is the per-standard-
                # deviation effect, which is 1e-16 for a denormal and O(0.1) for any
                # real term regardless of the units the predictor was recorded in.
                # (Under standardize = TRUE the columns already have sd 1, so this
                # reduces to |beta|.)
                #
                # RIDGE IS EXEMPT. The tolerance exists to recognise the zero that
                # L1 soft-thresholding meant to write; ridge (alpha = 0) never sets
                # a coefficient to zero, so every predictor is "selected" by
                # definition and there is no denormal to catch. Applying the
                # tolerance there actively broke ridge: with standardize = FALSE and
                # one predictor on a large scale, glmnet's lambda is scale-dominated
                # and crushes the unit-scale coefficients to ~1e-14 per SD - genuine
                # drivers, dropped from the table, while the Variable Importance
                # panel beside it still (correctly) showed them retained at 1.0.
                # Single implementation of the rule, shared with the bootstrap.
                ZERO_TOL <- 1e-10
                fitted <- private$.probsFrom(final_model, lambda_optimal, data$X,
                                             alpha_val, zero_tol = ZERO_TOL)
                is_selected <- fitted$keep
                selected <- names(beta)[is_selected]
                selected_coefs <- beta[is_selected]

                # Predicted probabilities, from the SAME thresholded coefficients
                # the results tables report.
                #
                # predict(final_model, ...) uses glmnet's raw beta, denormals and
                # all. Those 1e-16 residues still ORDER the cases, so pROC happily
                # ranked them: a model whose coefficient table honestly said "No
                # variables selected" was reported next to an apparent AUC of 0.617
                # derived entirely from floating-point noise. Predicting from the
                # zeroed beta keeps the coefficient table, the probabilities, the
                # AUC, the ROC curve and the saved predictions describing one model.
                probabilities <- fitted$prob(data$X)

                # Apparent AUC computed once here as the single source of truth,
                # then reused by performance/bootstrap/model-comparison/method-
                # comparison/summary to avoid redundant pROC calls and possible
                # divergence. direction/levels are fixed (y is 0/1 with 1 = event,
                # probabilities increase with the event) so a genuinely reversed
                # score reports AUC < 0.5 instead of being auto-flipped to >= 0.5.
                apparent_auc <- tryCatch(
                    {
                        if (requireNamespace("pROC", quietly = TRUE)) {
                            as.numeric(pROC::auc(pROC::roc(
                                data$y, probabilities,
                                quiet = TRUE, direction = "<", levels = c(0, 1)
                            )))
                        } else {
                            NA_real_
                        }
                    },
                    error = function(e) NA_real_
                )

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
                    apparent_auc = apparent_auc,
                    nfolds = nfolds
                )
            },

            # ══════════════════════════════════════════════════════════════════
            # Populate results tables
            # ══════════════════════════════════════════════════════════════════
            .populateModelSummary = function(data, fit) {
                table <- self$results$modelSummary
                # Map internal option codes to display labels (matches .a.yaml titles
                # and the Summary panel; also makes the values translatable).
                penalty_label <- private$.penaltyLabel()
                lambda_label <- private$.lambdaLabel()
                rows <- list(
                    # "Total observations" used to hold the COMPLETE-CASE count, so
                    # it read as the full cohort while listwise deletion had silently
                    # removed rows - and .suitabilityAssessment then green-lit the
                    # reduced N. Report both.
                    list(.("Complete cases analysed"), as.character(data$n)),
                    # "Excluded (incomplete data)" was the wrong heading whenever the
                    # outcome had more than two levels - those rows are out of scope,
                    # not incomplete. Same partition as the exclusion notice.
                    list(.("Excluded from analysis"),
                         if (data$n_excluded > 0)
                             sprintf("%d of %d (%.1f%%)", data$n_excluded, data$n_total,
                                     100 * data$n_excluded / data$n_total)
                         else .("None")),
                    list(.("Event class (positive)"), paste0(data$event_level, " (n=", data$n_events, ")")),
                    list(.("Reference class"), paste0(data$ref_level, " (n=", data$n_nonevents, ")")),
                    # Name what it counts. data$p is ncol(X) - a 5-level factor is 4
                    # of them - so labelling it "Candidate predictors" contradicted
                    # the constant-predictor notice, which counts VARIABLES (3 vs 6
                    # on the same screen). data$p is the right EPV denominator; the
                    # label was the wrong part.
                    # variables -> terms -> selected terms. "Variables selected"
                    # beside "Selected predictors" would have been two near-identical
                    # labels for two different quantities.
                    list(.("Variables analysed"), as.character(data$n_vars)),
                    list(.("Model terms (after dummy coding)"), as.character(data$p)),
                    list(.("Terms selected"), as.character(length(fit$selected))),
                    list(.("Penalty type"), penalty_label),
                    list(.("Alpha"), sprintf("%.2f", fit$alpha)),
                    list(.("Lambda (optimal)"), sprintf("%.4f", fit$lambda)),
                    list(.("Lambda selection"), lambda_label),
                    list(.("CV folds"), as.character(fit$nfolds))
                )

                # Break the exclusions down by cause, but only when there are any
                # and only for the causes that actually fired - three permanent
                # "0" rows would be noise on the common clean-data path.
                if (data$n_excluded > 0) {
                    # The indent prefix stays OUTSIDE .() - it is layout, not text,
                    # and a translator should never have to preserve it.
                    indent <- "  - "
                    causes <- list(
                        list(paste0(indent, .("predictor missing")), data$n_excl_pred),
                        list(paste0(indent, .("outcome missing")), data$n_excl_outcome_na),
                        list(paste0(indent, .("outcome level not modelled")), data$n_excl_outcome_lvl)
                    )
                    # append(x, v, after = k) makes v element k+1, so `at` is the
                    # index to insert AFTER and must be advanced only once a row has
                    # actually been placed. Incrementing first pushed the whole
                    # breakdown one slot down, so it appeared under "Event class
                    # (positive)" instead of under "Excluded from analysis".
                    at <- 2L # index of the "Excluded from analysis" row
                    for (cs in causes) {
                        if (cs[[2]] > 0) {
                            rows <- append(rows, list(list(cs[[1]], as.character(cs[[2]]))), after = at)
                            at <- at + 1L
                        }
                    }
                }

                for (i in seq_along(rows)) {
                    table$addRow(rowKey = i, values = list(statistic = rows[[i]][[1]], value = rows[[i]][[2]]))
                }
            },
            .populateCoefficients = function(fit, data = NULL) {
                table <- self$results$coefficients
                # Intercept on the ORIGINAL scale. glmnet fitted logit = b0 + sum(beta_z * z)
                # with z = (x - centre) / sd, so b0_original = b0 - sum(beta_z * centre / sd).
                # Only the selected (non-zeroed) terms enter, matching the coefficients shown.
                ctr <- if (!is.null(data) && !is.null(data$X_center)) data$X_center else NULL
                sdv <- if (!is.null(data) && !is.null(data$X_sd)) data$X_sd else NULL
                b0 <- fit$intercept
                if (length(fit$selected) > 0 && !is.null(ctr) && !is.null(sdv)) {
                    sel <- fit$selected
                    b0 <- b0 - sum(fit$selected_coefs * ctr[sel] / sdv[sel])
                }
                add_intercept <- function() {
                    table$addRow(rowKey = "intercept", values = list(
                        variable = .("(Intercept)"),
                        coefficient = b0, oddsRatio = NA, importance = NA
                    ))
                    table$setNote("intercept",
                        .("The intercept is the log-odds of the event when every selected predictor is 0 (or at its reference level); with the coefficients above it reproduces the model's predicted probability as plogis(intercept + sum(coefficient x value)). No odds ratio or importance applies to it."))
                }
                if (length(fit$selected) == 0) {
                    table$addRow(rowKey = 1, values = list(
                        variable = .("No variables selected"),
                        coefficient = NA, oddsRatio = NA, importance = NA
                    ))
                    add_intercept()
                    return()
                }

                # Report on the ORIGINAL measurement scale.
                #
                # The design matrix was standardised here and glmnet was called with
                # standardize=FALSE, so selected_coefs are per 1 SD. Dividing by the
                # column SD recovers exactly what glmnet(standardize=TRUE) returns -
                # the per-unit coefficient. This matters most for a 0/1 dummy from a
                # factor: with a balanced marker sd is about 0.5, so the per-SD odds
                # ratio is roughly the SQUARE ROOT of the model's actual
                # present-vs-absent odds ratio (1.81 printed where the model implies
                # 3.25). "Per 1 SD of p53 status" is not a quantity a pathologist can
                # act on. Importance keeps the per-SD magnitude, which is the
                # comparable-across-predictors quantity.
                sds <- if (!is.null(data) && !is.null(data$X_sd)) data$X_sd else NULL
                max_abs <- max(abs(fit$selected_coefs))
                for (i in seq_along(fit$selected)) {
                    coef_sd <- fit$selected_coefs[i]
                    sd_i <- if (!is.null(sds) && fit$selected[i] %in% names(sds))
                        sds[[fit$selected[i]]] else 1
                    if (!is.finite(sd_i) || sd_i == 0) sd_i <- 1
                    coef_val <- coef_sd / sd_i
                    or_val <- exp(coef_val)
                    importance <- abs(coef_sd) / max_abs

                    table$addRow(rowKey = i, values = list(
                        variable = fit$selected[i],
                        coefficient = coef_val,
                        oddsRatio = or_val,
                        importance = importance
                    ))
                }
                add_intercept()
                # LASSO penalization yields biased, shrunken coefficients without a
                # valid closed-form sampling distribution, so standard confidence
                # intervals are not reported here (naive post-selection CIs have
                # incorrect coverage). Enable bootstrap validation for optimism-
                # corrected performance, or refit an unpenalized model on the
                # selected variables (Model Comparison) for classical inference.
                # Point at the Model Comparison table only when the user can
                # actually see it - it is hidden behind showModelComparison, which
                # is off by default, so the old wording named an output that was
                # not on screen and gave no way to get to it.
                unpen_hint <- if (isTRUE(self$options$showModelComparison))
                    .("or the Model Comparison table for unpenalized estimates on the selected variables.")
                else
                    .("or tick 'Model comparison analysis' under Explanatory Output to see unpenalized estimates on the selected variables.")
                table$setNote("ci_note", paste(
                    .("LASSO coefficients are penalized (shrunken) and have no valid standard confidence intervals; they are omitted rather than shown as blanks. Use bootstrap validation for performance inference,"),
                    unpen_hint))
                if (isTRUE(self$options$standardize)) {
                    table$setNote("scale_note", .("Predictors were standardized before fitting so that the penalty treats them comparably, but the Coefficient and Odds Ratio columns are reported on the ORIGINAL measurement scale (per 1 unit of a continuous predictor, or present vs absent for a binary one). The Importance column is the per-standard-deviation magnitude, which is what can be compared across predictors with different units. Odds ratios remain penalized (shrunk toward 1)."))
                }
            },
            .populatePerformance = function(data, fit) {
                table <- self$results$performance

                # AUC
                auc_val <- NA
                auc_ci_lower <- NA
                auc_ci_upper <- NA
                roc_obj <- NULL
                tryCatch(
                    {
                        if (requireNamespace("pROC", quietly = TRUE)) {
                            roc_obj <- pROC::roc(data$y, fit$probabilities, quiet = TRUE, direction = "<", levels = c(0, 1))
                            auc_val <- as.numeric(pROC::auc(roc_obj))
                            # suppressWarnings: pROC warns "ci.auc() of a ROC curve
                            # with AUC == 1 is always 1-1 and can be misleading".
                            # The tryCatch here only handles errors, so that text
                            # escaped raw into jamovi's Analysis Notes. We detect
                            # the same condition below and say it properly.
                            ci_obj <- suppressWarnings(pROC::ci.auc(roc_obj, method = "delong"))
                            auc_ci_lower <- ci_obj[1]
                            auc_ci_upper <- ci_obj[3]
                        }
                    },
                    error = function(e) {}
                )

                # Optimal threshold
                optimal_threshold <- 0.5
                degenerate <- FALSE
                tryCatch(
                    {
                        if (!is.null(roc_obj)) {
                            coords_best <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
                            cand <- coords_best$threshold[1]
                            # With zero selected variables every predicted probability
                            # is identical, the ROC is degenerate and pROC returns
                            # -Inf - which was printed as "Optimal threshold: -Inf"
                            # alongside Sensitivity 1.000 / Specificity 0.000, i.e. a
                            # model that calls everyone positive presented as perfectly
                            # sensitive. Fall back to 0.5 and say so.
                            if (is.finite(cand)) {
                                optimal_threshold <- cand
                            } else {
                                optimal_threshold <- 0.5
                                # tryCatch's expr is a promise forced in THIS frame,
                                # so a plain <- lands here; <<- would skip past it.
                                degenerate <- TRUE
                            }
                        }
                    },
                    error = function(e) {}
                )
                # A constant predicted probability is also degenerate even when
                # pROC happens to return a finite "best" threshold.
                if (length(unique(round(fit$probabilities, 12))) < 2) degenerate <- TRUE

                # The other degenerate end. An apparent AUC of 1.000 means the
                # predictors separate the two classes completely ON THESE ROWS; it
                # is almost never reproduced out of sample, and it is exactly the
                # result that gets a model adopted. It was being labelled
                # "Excellent" with a 1.000-1.000 confidence interval and no caveat,
                # while the AUC = 0.500 end had acquired a strong warning and a
                # note. The old overfit guard (auc > 0.95 AND n < 100) also missed
                # it at n == 100 exactly.
                # Only an interval that has actually collapsed is "not estimable".
                # A 0.9995 proximity test threw away real, informative CIs (an AUC of
                # 0.9996 on n=400 has a genuine DeLong interval whose lower bound is
                # the number that matters) and mislabelled a non-separating model as
                # perfectly separating.
                perfect <- !is.na(auc_val) &&
                    (auc_val >= 1 || (!is.na(auc_ci_lower) && auc_ci_lower >= 1))

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
                # No positive predictions: precision is undefined, not a perfect 0.
                precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
                recall <- sensitivity
                f1 <- if (!is.na(precision) && precision + recall > 0)
                    2 * precision * recall / (precision + recall) else NA_real_

                rows <- list(
                    list(
                        .("AUC (apparent)"),
                        # DeLong's interval for AUC == 1 is always exactly 1.000 to
                        # 1.000. Printing that reads as an extraordinarily precise
                        # estimate when it is really the interval collapsing.
                        if (is.na(auc_val)) .("Not available")
                        else if (perfect) sprintf(.("%.3f (CI not estimable)"), auc_val)
                        else sprintf("%.3f (%.3f-%.3f)", auc_val, auc_ci_lower, auc_ci_upper),
                        if (is.na(auc_val)) .("AUC could not be computed")
                        else if (perfect) .("Perfect in-sample separation - see note")
                        else if (auc_val >= 0.9) .("Excellent") else if (auc_val >= 0.8) .("Good")
                        else if (auc_val >= 0.7) .("Acceptable") else .("Poor")
                    ),
                    list(.("Optimal threshold"), sprintf("%.3f", optimal_threshold), .("Youden index")),
                    list(.("Accuracy"), sprintf("%.3f", accuracy), ""),
                    list(.("Sensitivity (Recall)"), sprintf("%.3f", sensitivity), ""),
                    list(.("Specificity"), sprintf("%.3f", specificity), ""),
                    list(.("Precision (PPV)"), sprintf("%.3f", precision), ""),
                    list(.("F1 Score"), sprintf("%.3f", f1), ""),
                    list(
                        .("Brier Score"), sprintf("%.4f", brier),
                        {
                            # The Brier score is an OVERALL accuracy score, not a
                            # calibration measure, and its scale is driven by outcome
                            # prevalence: a no-information model that always predicts
                            # the base rate scores p(1-p), which is already 0.09 at
                            # 10% prevalence and would have been graded "Excellent
                            # calibration". Grade against that null model instead of
                            # fixed cut-offs.
                            prev <- mean(data$y, na.rm = TRUE)
                            null_brier <- prev * (1 - prev)
                            if (!is.finite(null_brier) || null_brier <= 0) {
                                .("Not interpretable")
                            } else {
                                bss <- 1 - brier / null_brier   # Brier skill score
                                if (bss >= 0.25) sprintf(.("Good (%.0f%% better than predicting the base rate)"), 100 * bss)
                                else if (bss > 0) sprintf(.("Marginal (%.0f%% better than base rate)"), 100 * bss)
                                else .("No better than predicting the base rate")
                            }
                        }
                    )
                )

                for (i in seq_along(rows)) {
                    table$addRow(rowKey = i, values = list(
                        metric = rows[[i]][[1]], value = rows[[i]][[2]], interpretation = rows[[i]][[3]]
                    ))
                }

                table$setNote(
                    "threshold_note",
                    .("The optimal threshold maximizes the Youden index on the same data used to fit the model, so sensitivity, specificity, accuracy, precision, and F1 are apparent (in-sample) and optimistic. Enable bootstrap validation for an optimism-corrected estimate of discrimination.")
                )

                # The fallback to 0.5 was implemented but never announced. Without
                # this note a null model (every predicted probability identical,
                # every case classified positive) is presented as Sensitivity 1.000
                # with an F1 of 0.667 - numbers that read as a highly sensitive test.
                # Assert constancy only when it is true: pROC can return a non-finite
                # "best" threshold for a heavily tied but genuinely discriminating
                # score, and the note claims every case got the same probability.
                constant_probs <- length(unique(round(fit$probabilities, 12))) < 2
                if (degenerate && constant_probs) {
                    table$setNote(
                        "degenerate_note",
                        .("This model assigns every case the same predicted probability, so it does not discriminate at all and no meaningful threshold exists. A default of 0.500 was used, which classifies every case into one group: the resulting sensitivity of 1.000 (or specificity of 1.000, depending on which side of 0.500 that constant probability falls) and the F1 score are artefacts of that, not evidence of a sensitive or specific test. Only the AUC of 0.500 is informative here.")
                    )
                    # Which metric hits 1.000 depends on which side of 0.500 the
                    # constant probability falls on, so name the one that actually
                    # did rather than assuming "everyone positive".
                    artefact <- if (sensitivity >= specificity)
                        .("a sensitivity of 1.000 here means the model calls every case positive, not that it is a sensitive test")
                    else
                        .("a specificity of 1.000 here means the model calls every case negative, not that it is a specific test")
                    private$.addNotice(
                        "STRONG_WARNING", .("Model Does Not Discriminate"),
                        sprintf(.("Every case received the same predicted probability, so the classification metrics below are artefacts of a default 0.500 threshold that assigns everyone to one group: %s. Only the AUC of 0.500 is interpretable."), artefact)
                    )
                }

                if (perfect) {
                    table$setNote(
                        "perfect_note",
                        .("An apparent AUC of 1.000 means the selected predictors separate the two classes completely in this dataset. Sensitivity, specificity, accuracy, precision and F1 of 1.000 describe the rows the model was fitted to, not future patients, and the confidence interval collapses to a single point rather than indicating precision. Complete separation also makes the coefficients themselves unstable. Enable bootstrap validation for an optimism-corrected estimate, and treat external validation as mandatory before any clinical use.")
                    )
                    private$.addNotice(
                        "STRONG_WARNING", .("Perfect Apparent Separation"),
                        sprintf(
                            .("Apparent AUC = %.3f with N = %d: the model separates the two classes completely on the data it was fitted to. This is an in-sample artefact far more often than a real effect, the reported confidence interval collapses to a point rather than showing precision, and the coefficients are unstable. Enable bootstrap validation and validate externally before drawing any conclusion."),
                            auc_val, data$n)
                    )
                }

                if (!is.na(auc_val) && !perfect && auc_val > 0.95 && data$n < 100) {
                    table$setNote(
                        "overfit_warning",
                        .("Warning: Very high apparent AUC with small sample size suggests possible overfitting. Enable bootstrap validation to assess optimism.")
                    )
                    private$.addNotice(
                        "STRONG_WARNING", .("Possible Overfitting"),
                        sprintf(
                            .("Apparent AUC = %.3f with N = %d: likely overfitted. Enable bootstrap validation for corrected estimate."),
                            auc_val, data$n
                        )
                    )
                }

                if (!is.na(auc_val) && auc_val < 0.7) {
                    private$.addNotice(
                        "WARNING", .("Poor Discrimination"),
                        sprintf(
                            .("AUC = %.3f indicates poor discrimination. Consider adding more informative predictors or using a different model."),
                            auc_val
                        )
                    )
                }
            },

            # ══════════════════════════════════════════════════════════════════
            # Scoring system generation - three methods
            # ══════════════════════════════════════════════════════════════════

            # ── Core: compute integer points by each method ────────────────
            # ── Log-odds contribution of MEETING each scoring criterion ─────
            #
            # A points system represents each factor's contribution to the linear
            # predictor for the contrast the score actually applies. The points were
            # derived from the raw per-SD coefficients while .computeTotalScores
            # awards them on a MEDIAN SPLIT, so binary and continuous predictors were
            # weighted on two different contrasts: a 0/1 dummy's per-SD coefficient
            # is beta_original * sd (about half the real effect for a balanced
            # marker) while a continuous predictor's median split spans roughly 1.6
            # SD. That mis-ranked them against each other, and the Scoring System
            # table's "Odds Ratio" column disagreed with the Selected Variables table
            # for the same predictor (2.11 vs 4.46 for p53).
            #
            # Contribution on the z-scale equals the contribution on the original
            # scale, since beta_z * delta_z = (beta_orig * sd) * (delta_orig / sd).
            #   binary     : beta_z * (z_present - z_absent)   = beta_original
            #   continuous : beta_z * (mean_z above - mean_z below the median)
            .scoreContributions = function(data, variables, coefs, cuts = NULL) {
                if (is.null(cuts)) cuts <- private$.scoreCuts(data, variables)
                out <- numeric(length(variables))
                for (i in seq_along(variables)) {
                    var_col <- variables[i]
                    if (!(var_col %in% colnames(data$X))) { out[i] <- coefs[i]; next }
                    v <- data$X[, var_col]; v <- v[!is.na(v)]
                    ci <- cuts[[i]]
                    delta <- if (isTRUE(ci$binary)) {
                        uv <- unique(v); max(uv) - min(uv)
                    } else {
                        hi <- mean(v[v > ci$cut]); lo <- mean(v[v <= ci$cut])
                        if (is.finite(hi) && is.finite(lo)) hi - lo else 1
                    }
                    if (!is.finite(delta) || delta == 0) delta <- 1
                    out[i] <- coefs[i] * delta
                }
                out
            },

            .computePoints = function(coefs, method, max_points = 10) {
                abs_coefs <- abs(coefs)
                signs <- sign(coefs)

                # Guard the reference denominator against a numerically negligible
                # contribution. Ridge is deliberately exempt from the selection
                # tolerance (it selects nothing), so a coefficient of ~1e-14 can
                # reach here; Schneeweiss then divides by it, the ratio exceeds
                # .Machine$integer.max, and as.integer() returns NA -- the two REAL
                # predictors came out blank while the noise one kept the only point.
                # Anything below this fraction of the largest contribution carries
                # no information about the score and cannot be the reference.
                NEGLIGIBLE <- 1e-8
                scale_ref <- max(abs_coefs)
                usable <- abs_coefs > scale_ref * NEGLIGIBLE
                # A points system beyond this is unusable at the bedside anyway, and
                # the cap keeps every method inside integer range by construction.
                MAX_ABS_POINTS <- 1000L
                clamp <- function(pts) {
                    pts[!is.finite(pts)] <- 0
                    as.integer(pmax(pmin(round(pts), MAX_ABS_POINTS), -MAX_ABS_POINTS))
                }

                if (method == "beta10") {
                    # Zhang et al. 2017 ("Beta10"): multiply each coefficient by a
                    # FIXED factor of 10 and round. This preserves absolute
                    # coefficient magnitude and is deliberately distinct from the
                    # max-scaled method below. (A previous version
                    # renormalized the largest |coef| to max_points, which made Beta10
                    # algebraically identical to Max-scaled and broke "Compare All
                    # Methods".) max_points is intentionally NOT used here.
                    pts <- clamp(coefs * 10)
                    # Ensure every selected (non-zero) predictor contributes >= 1 point
                    pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]
                } else if (method == "schneeweiss") {
                    # Mehta et al. 2016: divide by smallest absolute coefficient
                    # (smallest MEANINGFUL one - see NEGLIGIBLE above)
                    min_abs <- if (any(usable)) min(abs_coefs[usable]) else scale_ref
                    if (!is.finite(min_abs) || min_abs <= 0) min_abs <- 1
                    pts <- clamp(coefs / min_abs)
                    pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]
                } else if (method == "maxscaled") {
                    # Max-scaled: Points = beta_i / max|beta| * max_points, so the strongest
                    # predictor scores exactly max_points and the others keep their ratios.
                    # This used to be labelled "Sullivan/D'Agostino 2004", which it is not:
                    # Sullivan's Framingham method fixes a reference risk factor's per-category
                    # distance B as one point and awards points per CATEGORY of every factor.
                    W <- max(abs_coefs)
                    if (!is.finite(W) || W == 0) {
                        return(rep(0L, length(coefs)))
                    }
                    pts <- clamp((coefs / W) * max_points)
                    pts[pts == 0 & coefs != 0] <- signs[pts == 0 & coefs != 0]
                } else {
                    W <- max(abs_coefs)
                    if (!is.finite(W) || W == 0) return(rep(0L, length(coefs)))
                    pts <- clamp(coefs * max_points / W)
                }

                as.integer(pts)
            },

            # ── Safe read of an option the compiled .h.R may not carry yet ──
            # jmvcore ERRORS (it does not return NULL) when asked for an option the
            # compiled .h.R does not carry, so any newly-added option breaks the
            # whole analysis until jmvtools::prepare() has been re-run. Read new
            # options through this so the pre-regeneration window degrades to the
            # documented default instead of failing.
            .opt = function(name, default = NULL) {
                v <- tryCatch(self$options[[name]], error = function(e) NULL)
                if (is.null(v)) default else v
            },

            # ── Probabilities from a glmnet fit, under ONE selection rule ───
            #
            # Both .fitLasso and .bootstrapValidation go through this, so the
            # apparent estimate and the optimism correcting it always describe the
            # same model. Before it existed the bootstrap predicted from glmnet's
            # un-thresholded beta while the apparent value came from a thresholded
            # one - two different estimators in one table, which could put the
            # corrected AUC below chance.
            #
            # Returns the kept-coefficient mask, the zeroed beta, the intercept, and
            # a closure that scores any matrix with that beta. Callers that need to
            # score twice (the bootstrap) reuse the closure rather than calling this
            # again, so the threshold cannot differ between the two scorings.
            .probsFrom = function(fit_obj, lambda_s, fit_X, alpha_val, zero_tol = 1e-10) {
                cf <- as.matrix(stats::coef(fit_obj, s = lambda_s))
                b <- cf[-1, 1]
                keep <- is.finite(b)
                if (alpha_val > 0) {
                    # Threshold against the matrix the model was FITTED on. Deriving
                    # it from the matrix being SCORED made the two bootstrap passes
                    # zero different coefficients of the SAME model (column SDs
                    # differ between a resample and the original), which is exactly
                    # the "two estimators in one table" defect this helper exists to
                    # prevent. Which coefficients are zero is a property of the fit.
                    sds <- apply(fit_X, 2, stats::sd)
                    sds[!is.finite(sds) | sds == 0] <- 1
                    keep <- keep & abs(b) * sds > zero_tol
                }
                b[!keep] <- 0
                list(
                    keep = keep, beta = b, intercept = cf[1, 1],
                    prob = function(newx) as.numeric(stats::plogis(cf[1, 1] + newx %*% b))
                )
            },

            # Display labels for the two List options. Single source, because the
            # Model Summary mapped them while the copy-ready Results Summary and
            # the completion notice printed the RAW option codes - so a sentence
            # meant to be pasted into a manuscript read "with lambda.1se lambda
            # selection". The suite already guards the Model Summary against
            # exactly this (test "Model summary shows display labels, not raw
            # option codes"); the mapping simply stopped at one of three sites.
            .penaltyLabel = function() {
                switch(self$options$penalty,
                    "lasso"      = .("LASSO (L1)"),
                    "ridge"      = .("Ridge (L2)"),
                    "elasticnet" = .("Elastic Net"),
                    self$options$penalty)
            },
            .lambdaLabel = function() {
                switch(self$options$lambda,
                    "lambda.min" = .("Minimum CV Error"),
                    "lambda.1se" = .("1SE Rule (parsimonious)"),
                    self$options$lambda)
            },

            # Predictors for which "manual" silently fell back to the median.
            # Reset per run alongside .noticeList; .populateScoringSystem fills it,
            # and it runs BEFORE the methodology notes, so those see the truth too.
            .cutFellback = character(0),

            # ── Human name of the resolved cut rule ─────────────────────────
            #
            # Single source for every sentence that describes how continuous
            # predictors are dichotomised. Three outputs used to hardcode "the
            # median" while scoreCutMethod also offers mean/tertile/quartile/manual,
            # so the Scoring System table said "upper quartile" while the Scoring
            # System Performance note directly under it said "their median".
            .scoreCutLabel = function() {
                method <- private$.opt("scoreCutMethod", "median")
                if (identical(method, "manual")) {
                    # Say what was ACTUALLY applied. .scoreCuts falls back to the
                    # sample median for any predictor with no entry in
                    # scoreCutPoints, so an unqualified "the cut points you
                    # supplied" was a flat contradiction of the manual_fallback
                    # note sitting on the very same table - and the fix that
                    # centralised this label broadcast it to three panels.
                    if (length(private$.cutFellback) > 0)
                        return(.("the cut points you supplied, and the sample median for the predictors you did not give one for"))
                    return(.("the cut points you supplied"))
                }
                switch(method,
                    mean     = .("the sample mean"),
                    tertile  = .("the upper tertile"),
                    quartile = .("the upper quartile"),
                    .("the sample median"))
            },

            # ── Cut point for each predictor, on the STANDARDISED scale ─────
            #
            # Single source of truth for the dichotomisation. .scoreCriteria (what
            # is printed), .scoreContributions (what the points are derived from)
            # and .computeTotalScores (what is actually awarded) must all use the
            # SAME cut, or the published rule stops matching the computed score.
            #
            # Returns a named list: cut (on the z-scale of data$X), binary flag, and
            # the cut expressed on the original measurement scale for display.
            # Manual cuts are entered on the ORIGINAL scale and converted here.
            .scoreCuts = function(data, variables) {
                method <- private$.opt("scoreCutMethod", "median")
                manual <- private$.parseCutPoints(private$.opt("scoreCutPoints", ""))
                fellback <- character(0)

                out <- lapply(variables, function(var_col) {
                    if (!(var_col %in% colnames(data$X)))
                        return(list(binary = FALSE, cut = NA_real_, cut_raw = NA_real_))
                    v <- data$X[, var_col]
                    v <- v[!is.na(v)]
                    if (length(unique(v)) == 2)
                        return(list(binary = TRUE, cut = NA_real_, cut_raw = NA_real_))

                    ctr <- if (!is.null(data$X_center) && var_col %in% names(data$X_center))
                        data$X_center[[var_col]] else 0
                    sdv <- if (!is.null(data$X_sd) && var_col %in% names(data$X_sd))
                        data$X_sd[[var_col]] else 1
                    if (!is.finite(sdv) || sdv == 0) sdv <- 1

                    cut_z <- NA_real_
                    if (identical(method, "manual") && var_col %in% names(manual)) {
                        # entered on the original scale -> convert to the z-scale
                        cut_z <- (manual[[var_col]] - ctr) / sdv
                    } else {
                        if (identical(method, "manual")) fellback <<- c(fellback, var_col)
                        cut_z <- switch(
                            method,
                            mean     = mean(v),
                            tertile  = stats::quantile(v, 2 / 3, names = FALSE),
                            quartile = stats::quantile(v, 0.75, names = FALSE),
                            stats::median(v)   # median, and the manual fallback
                        )
                    }
                    if (!is.finite(cut_z)) cut_z <- stats::median(v)
                    list(binary = FALSE, cut = cut_z, cut_raw = cut_z * sdv + ctr)
                })
                names(out) <- variables
                attr(out, "fellback") <- fellback
                out
            },

            # Parse "ki67=20, age=65" (also accepts ';' and newlines) into a named
            # numeric vector. Unparseable entries are dropped rather than guessed.
            .parseCutPoints = function(txt) {
                if (is.null(txt) || !nzchar(trimws(txt))) return(stats::setNames(numeric(0), character(0)))
                parts <- unlist(strsplit(txt, "[,;\n]+"))
                parts <- trimws(parts[nzchar(trimws(parts))])
                nm <- character(0); vals <- numeric(0)
                for (pt in parts) {
                    kv <- strsplit(pt, "=", fixed = TRUE)[[1]]
                    if (length(kv) != 2) next
                    key <- trimws(kv[1])
                    val <- suppressWarnings(as.numeric(trimws(kv[2])))
                    if (!nzchar(key) || !is.finite(val)) next
                    nm <- c(nm, key); vals <- c(vals, val)
                }
                stats::setNames(vals, nm)
            },

            # ── Human-readable scoring criterion, on the ORIGINAL scale ─────
            #
            # .computeTotalScores awards a predictor's points when a continuous
            # value exceeds its IN-SAMPLE MEDIAN, but that median never reached the
            # output: the Scoring System table published variable / OR / points and
            # nothing else, so a clinician could not apply the score to a new
            # patient - they had no idea what "high ki67" meant. Reconstruct the cut
            # on the original measurement scale (the matrix is standardised, so
            # raw = z * sd + centre) and publish it.
            .scoreCriteria = function(data, variables, cuts = NULL) {
                if (is.null(cuts)) cuts <- private$.scoreCuts(data, variables)
                vapply(seq_along(variables), function(i) {
                    ci <- cuts[[i]]
                    if (is.null(ci) || is.na(ci$binary)) return(NA_character_)
                    if (isTRUE(ci$binary)) return(.("present"))
                    sprintf(.("> %s"), base::format(round(ci$cut_raw, 3), trim = TRUE))
                }, character(1), USE.NAMES = FALSE)
            },

            .computeTotalScores = function(data, variables, points, cuts = NULL) {
                if (is.null(cuts)) cuts <- private$.scoreCuts(data, variables)
                total <- rep(0, data$n)
                for (i in seq_along(variables)) {
                    var_col <- variables[i]
                    if (!(var_col %in% colnames(data$X))) next
                    col_vals <- data$X[, var_col]
                    ci <- cuts[[i]]
                    if (isTRUE(ci$binary)) {
                        # Score the "present" (higher) level; scaling is monotonic,
                        # so max() is the original 1/present level.
                        present_val <- max(unique(col_vals[!is.na(col_vals)]))
                        total <- total + ifelse(!is.na(col_vals) & col_vals == present_val, points[i], 0)
                    } else {
                        # Continuous: award above the resolved cut (median by
                        # default; mean/tertile/quartile or a manual clinical
                        # threshold when selected). This is the SAME cut printed in
                        # the criterion column and used to derive the points.
                        total <- total + ifelse(!is.na(col_vals) & col_vals > ci$cut, points[i], 0)
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
                tp <- sum(pred == 1 & y == 1)
                fp <- sum(pred == 1 & y == 0)
                fn <- sum(pred == 0 & y == 1)
                sens <- tp / max(tp + fn, 1)
                spec <- sum(pred == 0 & y == 0) / max(sum(y == 0), 1)
                acc <- mean(pred == y)
                prec <- if (tp + fp > 0) tp / (tp + fp) else 0
                f1 <- if (prec + sens > 0) 2 * prec * sens / (prec + sens) else 0

                auc_val <- NA
                tryCatch(
                    {
                        if (requireNamespace("pROC", quietly = TRUE)) {
                            roc_obj <- pROC::roc(y, total_scores, quiet = TRUE, direction = "<", levels = c(0, 1))
                            auc_val <- as.numeric(pROC::auc(roc_obj))
                        }
                    },
                    error = function(e) {}
                )

                list(
                    auc = auc_val, cutoff = best_cutoff, accuracy = acc,
                    sensitivity = sens, specificity = spec,
                    precision = prec, f1 = f1,
                    mean_pos = mean(total_scores[y == 1]),
                    mean_neg = mean(total_scores[y == 0]),
                    range = range(total_scores)
                )
            },

            # ── Main scoring system population ──────────────────────────────
            .populateScoringSystem = function(data, fit) {
                table <- self$results$scoringTable
                perf_table <- self$results$scoringPerformance

                # Nothing to score. Returning silently left up to four VISIBLE and
                # completely EMPTY tables on screen (Scoring System, Scoring System
                # Performance, Score-to-Probability Lookup and, in compare mode,
                # Scoring Method Comparison) with no indication of why.
                if (length(fit$selected) == 0) {
                    empty_msg <- sprintf(
                        .("No scoring system could be built: the model selected zero predictors at the chosen lambda, so there are no coefficients to convert into points. %s"),
                        if (identical(self$options$lambda, "lambda.1se"))
                            .("Try the Minimum CV Error lambda instead of the 1SE rule, or add more informative predictors.")
                        else
                            .("You are already using the least conservative lambda, so add more informative predictors or collect more cases."))
                    table$setNote("no_vars", empty_msg)
                    perf_table$setNote("no_vars", empty_msg)
                    self$results$lookupTable$setNote("no_vars", empty_msg)
                    self$results$methodComparison$setNote("no_vars", empty_msg)
                    private$.addNotice(
                        "WARNING", .("Scoring System Not Generated"),
                        empty_msg
                    )
                    return()
                }

                method <- self$options$scoringMethod
                if (is.null(method)) method <- "schneeweiss"
                max_points <- self$options$scoringMaxPoints

                vars <- fit$selected
                # Resolve the cut points ONCE and pass them to every consumer, so the
                # rule printed in the criterion column, the contrast the points are
                # derived from, and the cut actually applied when scoring cannot
                # drift apart.
                cuts <- private$.scoreCuts(data, vars)
                # Record which predictors fell back so every sentence built after
                # this point (here, and in the methodology notes) can say so.
                fb <- attr(cuts, "fellback")
                private$.cutFellback <- if (is.null(fb)) character(0) else as.character(fb)

                # Points are derived from the log-odds contribution of MEETING each
                # criterion, so they are on the same contrast the score applies and
                # are comparable between binary and continuous predictors.
                coefs <- private$.scoreContributions(data, vars, fit$selected_coefs, cuts)

                # Compute points by all three methods
                pts_beta10 <- private$.computePoints(coefs, "beta10", max_points)
                pts_schneeweiss <- private$.computePoints(coefs, "schneeweiss", max_points)
                pts_maxscaled <- private$.computePoints(coefs, "maxscaled", max_points)

                # Select primary method's points
                pts_primary <- switch(method,
                    "beta10" = pts_beta10,
                    "schneeweiss" = pts_schneeweiss,
                    "maxscaled" = pts_maxscaled,
                    "compare" = pts_schneeweiss, # default to Schneeweiss for primary
                    pts_schneeweiss
                )

                # Build score data
                score_data <- data.frame(
                    variable = vars,
                    coefficient = coefs,
                    oddsRatio = exp(coefs),
                    criterion = private$.scoreCriteria(data, vars, cuts),
                    direction = ifelse(coefs > 0, .("Positive (+)"), .("Negative (-)")),
                    points_beta10 = pts_beta10,
                    points_schneeweiss = pts_schneeweiss,
                    points_maxscaled = pts_maxscaled,
                    points = pts_primary,
                    stringsAsFactors = FALSE
                )

                # Sort by absolute points descending
                score_data <- score_data[order(-abs(score_data$points)), ]

                for (i in seq_len(nrow(score_data))) {
                    table$addRow(rowKey = i, values = list(
                        variable = score_data$variable[i],
                        oddsRatio = score_data$oddsRatio[i],
                        criterion = score_data$criterion[i],
                        direction = score_data$direction[i],
                        points_beta10 = score_data$points_beta10[i],
                        points_schneeweiss = score_data$points_schneeweiss[i],
                        points_maxscaled = score_data$points_maxscaled[i],
                        points = score_data$points[i]
                    ))
                }

                # Add method reference note
                method_refs <- list(
                    beta10 = .("Beta10 method (Zhang et al. Ann Transl Med 2017)"),
                    schneeweiss = .("Schneeweiss method (Mehta et al. J Clin Epidemiol 2016)"),
                    maxscaled = .("Max-scaled method (strongest predictor = maximum points)"),
                    compare = .("All three methods shown for comparison")
                )
                table$setNote("method", method_refs[[method]])
                fellback <- attr(cuts, "fellback")
                if (identical(private$.opt("scoreCutMethod", "median"), "manual") &&
                    !is.null(fellback) && length(fellback) > 0) {
                    table$setNote("manual_fallback", sprintf(
                        .("No manual cut point was supplied for: %s. These fell back to the sample median. Enter them as 'variable=value' pairs (for example 'ki67=20, age=65') to use established clinical thresholds."),
                        paste(fellback, collapse = ", ")))
                }
                cut_label <- private$.scoreCutLabel()
                table$setNote("criterion_note", sprintf(.("Award a factor's points when the patient meets its criterion. Continuous predictors are cut at %s. The Odds Ratio column is the penalized odds ratio for MEETING that criterion (present vs absent, or above vs below the cut), which is the contrast the points represent - so points and odds ratios are on the same footing here. A cut derived from this dataset (median, mean, tertile or quartile) is not an externally established clinical threshold and will differ in another cohort; supplying manual cut points from the literature is what makes a score portable. The score has not been validated outside these data."), cut_label))

                # A points system is only useful if a clinician can add it up at the
                # bedside. Schneeweiss divides by the SMALLEST contribution, so one
                # near-zero predictor inflates every other weight - the guard in
                # .computePoints stops that overflowing to NA, but a 1000-point scale
                # is not a usable score and must not be presented as one.
                max_pt <- suppressWarnings(max(abs(pts_primary), na.rm = TRUE))
                if (is.finite(max_pt) && max_pt > 100) {
                    table$setNote("wide_scale", sprintf(
                        .("This point scale spans up to %d points per factor, which is too wide to be used as a bedside score. It happens when one selected predictor has a far smaller effect than the others and the chosen method scales relative to it. Try the Max-scaled method, which caps the strongest predictor at the Maximum Points you set, or drop the negligible predictor."),
                        as.integer(max_pt)))
                    private$.addNotice(
                        "WARNING", .("Scoring Scale Not Usable"),
                        sprintf(
                            .("The generated scoring system reaches %d points for a single factor. A usable clinical score is typically under 20 points in total; this one is dominated by the ratio between the largest and smallest selected effects. Switch to the Max-scaled method or remove the near-zero predictor before using this score."),
                            as.integer(max_pt)))
                }

                # Evaluate primary scoring system
                total_scores <- private$.computeTotalScores(data, vars, pts_primary, cuts)
                perf <- private$.evaluateScore(data$y, total_scores)

                perf_rows <- list(
                    # A fallback is required: without one an unmapped option value
                    # returns NULL, the row collapses to length 1 and the fill loop
                    # dies with "subscript out of bounds", taking the analysis down.
                    list(.("Scoring method"), switch(method,
                        "beta10" = "Beta10",
                        "schneeweiss" = "Schneeweiss",
                        "maxscaled" = .("Max-scaled"),
                        "compare" = .("Schneeweiss (primary)"),
                        as.character(method)
                    )),
                    list(.("Score AUC (apparent)"), sprintf("%.3f", perf$auc)),
                    list(.("Optimal score cutoff (chosen on this data)"), as.character(perf$cutoff)),
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
                        metric = perf_rows[[i]][[1]], value = perf_rows[[i]][[2]]
                    ))
                }

                perf_table$setNote(
                    "dichotomization",
                    sprintf(.("Continuous predictors are scored by dichotomizing at %s: the full point block is awarded above the cut and nothing below it. The performance shown here reflects this simplified integer point system and may differ from the continuous LASSO model in the Classification Performance table."), cut_label)
                )
                # The model's own table is labelled "AUC (apparent)" and carries an
                # optimism caveat; the SCORE's table said only "Score AUC" while
                # being doubly optimistic - the points come from a model fitted on
                # these data AND the cutoff is Youden-optimised on the same data.
                perf_table$setNote(
                    "apparent",
                    .("These figures are APPARENT (in-sample) and optimistic twice over: the points were derived from a model fitted to this dataset, and the score cutoff was chosen to maximise the Youden index on the same rows. They are not an estimate of how the score would perform on new patients. Enable bootstrap validation for an optimism-corrected estimate of the model, and validate any score externally before clinical use.")
                )

                # ── Method comparison (when compare mode selected) ──────────
                if (method == "compare") {
                    comp_table <- self$results$methodComparison

                    # Full model AUC for reference (single-source apparent AUC)
                    full_auc <- fit$apparent_auc

                    methods_list <- list(
                        list("Beta10", pts_beta10, "Zhang et al. 2017"),
                        list("Schneeweiss", pts_schneeweiss, "Mehta et al. 2016"),
                        list(.("Max-scaled"), pts_maxscaled, .("beta / max|beta| x maximum points"))
                    )

                    for (j in seq_along(methods_list)) {
                        m <- methods_list[[j]]
                        scores_j <- private$.computeTotalScores(data, vars, m[[2]], cuts)
                        perf_j <- private$.evaluateScore(data$y, scores_j)

                        info_loss <- if (!is.na(full_auc) && !is.na(perf_j$auc) && full_auc > 0) {
                            (1 - perf_j$auc / full_auc) * 100
                        } else {
                            NA
                        }

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
                lookup_table$setNote(
                    "smallcell_note",
                    .("Predicted probabilities are in-sample empirical event rates; scores based on few cases (small N) are unstable and can read as 0% or 100%. Validate the score-to-probability mapping on an independent cohort before clinical use.")
                )
            },

            # ── Calibration slope: coefficient of the linear predictor when the
            #    observed outcome is regressed on logit(predicted prob). 1.0 = ideal;
            #    < 1 signals over-extreme (overfitted) predictions. ────────────────
            .calibrationSlope = function(y, p) {
                lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
                if (length(unique(lp)) < 2) {
                    return(NA_real_)
                }
                tryCatch(
                    suppressWarnings(
                        as.numeric(coef(stats::glm(y ~ lp, family = stats::binomial))[2])
                    ),
                    error = function(e) NA_real_
                )
            },

            # ══════════════════════════════════════════════════════════════════
            # Bootstrap internal validation (Harrell method)
            # ══════════════════════════════════════════════════════════════════
            .bootstrapValidation = function(data, fit) {
                table <- self$results$validationTable
                B <- self$options$bootstrapN

                alpha_val <- fit$alpha

                # Apparent performance (reuse single-source apparent AUC)
                apparent_auc <- fit$apparent_auc

                apparent_brier <- mean((fit$probabilities - data$y)^2)
                apparent_slope <- private$.calibrationSlope(data$y, fit$probabilities)

                # Bootstrap optimism estimation
                optimism_auc <- rep(NA_real_, B)
                optimism_brier <- rep(NA_real_, B)
                optimism_slope <- rep(NA_real_, B)

                for (b in seq_len(B)) {
                    # Keep the UI responsive / allow cancellation during the (heavy)
                    # per-bootstrap cv.glmnet refits without flushing partial results.
                    if (b %% 10 == 0) private$.checkpoint(flush = FALSE)
                    tryCatch(
                        {
                            idx <- sample(data$n, replace = TRUE)
                            X_boot <- data$X[idx, , drop = FALSE]
                            y_boot <- data$y[idx]

                            # Skip if boot sample not binary
                            if (length(unique(y_boot)) < 2) next

                            # Fit on bootstrap with the SAME fold rule as the main fit:
                            # stratified, capped at the resample's minority class.
                            nfolds_boot <- min(fit$nfolds, length(unique(idx)) - 1,
                                               sum(y_boot == 1), sum(y_boot == 0))
                            nfolds_boot <- max(nfolds_boot, 3)
                            foldid_boot <- private$.stratifiedFolds(y_boot, nfolds_boot)

                            cv_boot_args <- list(
                                x = X_boot, y = y_boot,
                                family = "binomial", alpha = alpha_val,
                                standardize = FALSE, type.measure = "deviance"
                            )
                            if (!is.null(foldid_boot)) cv_boot_args$foldid <- foldid_boot
                            else cv_boot_args$nfolds <- nfolds_boot
                            cv_boot <- .quietly(do.call(glmnet::cv.glmnet, cv_boot_args))
                            lambda_boot <- switch(self$options$lambda,
                                "lambda.min" = cv_boot$lambda.min,
                                "lambda.1se" = cv_boot$lambda.1se,
                                cv_boot$lambda.1se
                            )

                            # Predict on bootstrap sample and original
                            # One model, two scorings - not two thresholds.
                            boot_fit <- private$.probsFrom(cv_boot, lambda_boot, X_boot, alpha_val)
                            prob_boot_boot <- boot_fit$prob(X_boot)
                            prob_boot_orig <- boot_fit$prob(data$X)

                            if (requireNamespace("pROC", quietly = TRUE)) {
                                auc_boot_boot <- as.numeric(pROC::auc(pROC::roc(y_boot, prob_boot_boot, quiet = TRUE, direction = "<", levels = c(0, 1))))
                                auc_boot_orig <- as.numeric(pROC::auc(pROC::roc(data$y, prob_boot_orig, quiet = TRUE, direction = "<", levels = c(0, 1))))
                                optimism_auc[b] <- auc_boot_boot - auc_boot_orig
                            }

                            brier_boot_boot <- mean((prob_boot_boot - y_boot)^2)
                            brier_boot_orig <- mean((prob_boot_orig - data$y)^2)
                            optimism_brier[b] <- brier_boot_boot - brier_boot_orig # negative optimism for Brier

                            slope_boot <- private$.calibrationSlope(y_boot, prob_boot_boot)
                            slope_orig <- private$.calibrationSlope(data$y, prob_boot_orig)
                            if (!is.na(slope_boot) && !is.na(slope_orig)) {
                                optimism_slope[b] <- slope_boot - slope_orig
                            }
                        },
                        error = function(e) {}
                    )
                }

                # Compute corrected metrics (NA-based tracking avoids excluding legitimate zero-optimism samples).
                # NaN-safe mean: if every bootstrap iteration failed, render blank (NA) rather than "NaN".
                safe_mean <- function(x) {
                    m <- mean(x, na.rm = TRUE)
                    if (is.nan(m)) NA_real_ else m
                }
                mean_optimism_auc <- safe_mean(optimism_auc)
                mean_optimism_brier <- safe_mean(optimism_brier)
                mean_optimism_slope <- safe_mean(optimism_slope)
                corrected_auc <- apparent_auc - mean_optimism_auc
                corrected_brier <- apparent_brier - mean_optimism_brier
                corrected_slope <- apparent_slope - mean_optimism_slope

                # Say how many replicates the correction actually rests on. Failed
                # replicates are swallowed by the tryCatch and left as NA, and
                # safe_mean() drops them with na.rm = TRUE, so a correction computed
                # from 50 survivors of 200 looked identical to one from all 200.
                # Count replicates that COMPLETED, not those that produced an AUC:
                # without pROC every successful replicate still yields Brier and
                # calibration-slope optimism, and counting AUCs reported "0 of 200
                # completed; 200 failed" while the correction was in fact valid.
                n_ok <- sum(!is.na(optimism_auc) | !is.na(optimism_brier) | !is.na(optimism_slope))
                if (n_ok < B) {
                    table$setNote("boot_n", sprintf(
                        .("%d of %d bootstrap replicates completed; %d failed (typically a resample with too few events to fit) and were excluded. The optimism correction is based on the %d successful replicates."),
                        n_ok, B, B - n_ok, n_ok))
                }
                if (n_ok < 20) {
                    table$setNote("boot_few", .("Fewer than 20 bootstrap replicates succeeded. The optimism correction is unreliable at this number - increase the sample size or reduce the number of candidate predictors."))
                }

                rows <- list(
                    list(.("AUC"), apparent_auc, mean_optimism_auc, corrected_auc),
                    list(.("Brier Score"), apparent_brier, mean_optimism_brier, corrected_brier),
                    list(.("Calibration slope"), apparent_slope, mean_optimism_slope, corrected_slope)
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
                    table$setNote(
                        "optimism_warning",
                        sprintf(
                            .("Optimism = %.3f indicates overfitting. The corrected AUC (%.3f) is a more realistic estimate of future performance."),
                            mean_optimism_auc, corrected_auc
                        )
                    )
                }

                # A corrected AUC below 0.5 is a legitimate outcome, not a glitch:
                # even when the final model selects nothing, bootstrap resamples
                # manufacture enough spurious signal that some of them DO select
                # variables, so the optimism is genuinely positive and subtracting
                # it takes the estimate below chance. Left uncorrected on the
                # screen it just looks broken, so say what it means.
                if (!is.na(corrected_auc) && corrected_auc < 0.5) {
                    table$setNote(
                        "below_chance",
                        sprintf(
                            .("The corrected AUC (%.3f) is below 0.500. This is a real result, not an error: the bootstrap replicates fit models that pick up chance associations, so the optimism (%.3f) exceeds what the model actually achieves. Read it as evidence that these predictors carry no usable signal at this sample size - the model performs no better than, and by this estimate slightly worse than, guessing."),
                            corrected_auc, mean_optimism_auc)
                    )
                }

                table$setNote(
                    "calibration_note",
                    .("Calibration slope: 1.0 = ideal. A corrected slope below 1 means predicted probabilities are too extreme (overfitted) and would benefit from shrinkage; an apparent slope above 1 is expected for penalized models on their training data.")
                )
            },

            # ══════════════════════════════════════════════════════════════════
            # Variable importance
            # ══════════════════════════════════════════════════════════════════
            .populateVariableImportance = function(data, fit) {
                table <- self$results$variableImportance

                # Calculate inclusion proportion across lambda path
                all_coefs <- as.matrix(coef(fit$cv_fit$glmnet.fit))[-1, , drop = FALSE]

                # Same per-SD zero rule as the coefficient table (see ZERO_TOL in .fitLasso):
                # an exact `!= 0` counted coordinate-descent residues of 1e-16 as inclusions.
                sds <- if (!is.null(data$X_sd)) data$X_sd[rownames(all_coefs)] else rep(1, nrow(all_coefs))
                sds[!is.finite(sds) | sds == 0] <- 1
                inclusion_prop <- rowMeans(abs(all_coefs) * sds > 1e-10)
                max_abs <- apply(abs(all_coefs), 1, max)

                imp_df <- data.frame(
                    variable = rownames(all_coefs),
                    importance_score = max_abs,
                    selection_frequency = inclusion_prop,
                    stringsAsFactors = FALSE
                )
                imp_df <- imp_df[order(-imp_df$importance_score), ]
                imp_df$stability_rank <- seq_len(nrow(imp_df))

                n_show <- min(nrow(imp_df), 20)
                for (i in seq_len(n_show)) {
                    table$addRow(rowKey = i, values = list(
                        variable = imp_df$variable[i],
                        importance_score = imp_df$importance_score[i],
                        selection_frequency = imp_df$selection_frequency[i],
                        stability_rank = imp_df$stability_rank[i]
                    ))
                }
                if (n_show < nrow(imp_df)) {
                    table$setNote("truncated", sprintf(
                        .("Showing the %d highest-ranked of %d model terms; the remaining terms have smaller maximum coefficients along the path."),
                        n_show, nrow(imp_df)))
                }

                table$setNote(
                    "apparent_note",
                    .("Path Inclusion Proportion is the fraction of the single cross-validated lambda path on which each predictor has a non-zero coefficient (apparent), NOT a bootstrap/resampling stability-selection frequency. Importance Rank orders predictors by their maximum absolute coefficient along that path. Because Ridge (L1 weight = 0) never shrinks coefficients to zero, every predictor is retained at all lambdas and the proportion is trivially 1.0.")
                )
            },

            # ══════════════════════════════════════════════════════════════════
            # Model comparison
            # ══════════════════════════════════════════════════════════════════
            .populateModelComparison = function(data, fit) {
                table <- self$results$modelComparison

                # Track complete/quasi-complete separation in the unpenalized
                # refits (rows 2-3) so we can flag their unreliable AUC/AIC.
                separation_detected <- FALSE

                # Row 1: the actual PENALIZED LASSO model (apparent performance).
                # Penalized models have no standard AIC, so it is left blank.
                # AUC reuses the single-source apparent AUC from .fitLasso.
                tryCatch(
                    {
                        auc_lasso <- fit$apparent_auc
                        brier_lasso <- mean((fit$probabilities - data$y)^2)
                        table$addRow(rowKey = 1, values = list(
                            model_type = .("LASSO (penalized)"),
                            n_variables = length(fit$selected),
                            auc = auc_lasso,
                            aic = NA,
                            brier = brier_lasso
                        ))
                    },
                    error = function(e) {}
                )

                # Row 2: UNPENALIZED logistic refit on the LASSO-selected variables.
                # (This un-shrinks the coefficients, so its AUC is typically higher
                # than the penalized model above -- it is NOT the LASSO model itself.)
                if (length(fit$selected) > 0) {
                    X_sel <- data$X[, fit$selected, drop = FALSE]
                    df_sel <- data.frame(y = data$y, X_sel)
                    tryCatch(
                        {
                            glm_sel <- suppressWarnings(glm(y ~ ., data = df_sel, family = binomial))
                            prob_sel <- predict(glm_sel, type = "response")
                            if (!isTRUE(glm_sel$converged) ||
                                any(prob_sel > 1 - 1e-8 | prob_sel < 1e-8)) {
                                separation_detected <- TRUE
                            }
                            auc_sel <- NA
                            if (requireNamespace("pROC", quietly = TRUE)) {
                                auc_sel <- as.numeric(pROC::auc(pROC::roc(data$y, prob_sel, quiet = TRUE, direction = "<", levels = c(0, 1))))
                            }
                            brier_sel <- mean((prob_sel - data$y)^2)
                            table$addRow(rowKey = 2, values = list(
                                model_type = .("Logistic (LASSO-selected vars)"),
                                n_variables = length(fit$selected),
                                auc = auc_sel,
                                aic = AIC(glm_sel),
                                brier = brier_sel
                            ))
                        },
                        error = function(e) {
                            table$setNote("refit_failed_sel", sprintf(
                                .("The unpenalized refit on the LASSO-selected variables could not be fitted (%s), so its row is omitted."),
                                conditionMessage(e)))
                        }
                    )
                }

                # Row 3: UNPENALIZED logistic on all candidate variables.
                tryCatch(
                    {
                        df_all <- data.frame(y = data$y, data$X)
                        glm_all <- suppressWarnings(glm(y ~ ., data = df_all, family = binomial))
                        prob_all <- predict(glm_all, type = "response")
                        if (!isTRUE(glm_all$converged) ||
                            any(prob_all > 1 - 1e-8 | prob_all < 1e-8)) {
                            separation_detected <- TRUE
                        }
                        auc_all <- NA
                        if (requireNamespace("pROC", quietly = TRUE)) {
                            auc_all <- as.numeric(pROC::auc(pROC::roc(data$y, prob_all, quiet = TRUE, direction = "<", levels = c(0, 1))))
                        }
                        brier_all <- mean((prob_all - data$y)^2)
                        table$addRow(rowKey = 3, values = list(
                            model_type = .("Logistic (all vars)"),
                            n_variables = ncol(data$X),
                            auc = auc_all,
                            aic = AIC(glm_all),
                            brier = brier_all
                        ))
                    },
                    error = function(e) {
                        table$setNote("refit_failed_all", sprintf(
                            .("The unpenalized refit on all candidate variables could not be fitted (%s), so its row is omitted."),
                            conditionMessage(e)))
                    }
                )

                # Refer to the rows by NAME, not by index: the "LASSO-selected vars"
                # refit is skipped entirely when nothing was selected, so a note
                # saying "rows 2-3" pointed at a row that was not on screen.
                table$setNote(
                    "refit_note",
                    .("The 'Logistic' rows are unpenalized refits (shown for AIC comparability); their apparent AUC is typically higher than the penalized LASSO row because refitting removes LASSO shrinkage. All metrics are apparent (in-sample) - use Bootstrap Internal Validation for optimism-corrected discrimination.")
                )

                if (separation_detected) {
                    table$setNote(
                        "separation_note",
                        .("Warning: an unpenalized logistic refit (a 'Logistic' row) did not converge or produced fitted probabilities at 0 or 1, indicating complete or quasi-complete separation. Its apparent AUC (often ~1.0), AIC, and Brier score are unreliable and should not be interpreted - this is exactly the instability that LASSO penalization is designed to avoid.")
                    )
                    private$.addNotice(
                        "WARNING", .("Separation in Unpenalized Refit"),
                        .("An unpenalized logistic refit in the Model Comparison table showed complete/quasi-complete separation (non-convergence or fitted probabilities at 0/1). Its AUC/AIC are unreliable; prefer the penalized LASSO row.")
                    )
                }
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

                # Coefficient plot: save selected variable names and coefficients.
                #
                # These are the PER-SD coefficients, deliberately: bars for a 0-100%
                # Ki-67 and a 0/1 immunostain are only comparable on a common scale.
                # The Selected Variables table prints the per-UNIT (original-scale)
                # coefficient instead, and both used to be labelled just
                # "Coefficient" - so the plot's tallest bar (ki67_pct, 2.32 per SD)
                # contradicted the table's largest coefficient (rb1_loss, 2.66 per
                # unit) with nothing on screen to explain it. Carry the flag so the
                # renderer can name the scale it is actually drawing.
                if (self$options$coef_plot && length(fit$selected) > 0) {
                    ord <- order(abs(fit$selected_coefs))
                    coef_state <- list(
                        var_names    = as.character(fit$selected[ord]),
                        coef_values  = as.numeric(fit$selected_coefs[ord]),
                        standardized = isTRUE(self$options$standardize)
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
                if (is.null(state)) {
                    return(FALSE)
                }

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
                        color = "darkgrey", width = 0.02
                    ) +
                    ggplot2::geom_vline(
                        xintercept = log(state$lambda_min),
                        linetype = "dashed", color = "blue"
                    ) +
                    ggplot2::geom_vline(
                        xintercept = log(state$lambda_1se),
                        linetype = "dashed", color = "green"
                    ) +
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
                if (is.null(state) || length(state$var_names) == 0) {
                    return(FALSE)
                }

                df <- data.frame(
                    variable    = factor(state$var_names, levels = state$var_names),
                    coefficient = state$coef_values,
                    direction   = ifelse(state$coef_values > 0, "Positive", "Negative")
                )

                # Name the scale being drawn. Without this the axis reads
                # "Coefficient", identical to the Selected Variables table column,
                # while holding a different number for the same predictor.
                if (isTRUE(state$standardized)) {
                    y_lab <- .("Coefficient (per 1 SD)")
                    sub_lab <- .("Comparable across units; the table reports the original scale")
                } else {
                    y_lab <- .("Coefficient (per 1 unit)")
                    sub_lab <- .("Original measurement scale, as in the Selected Variables table")
                }

                p <- ggplot2::ggplot(df, ggplot2::aes(x = variable, y = coefficient, fill = direction)) +
                    ggplot2::geom_col() +
                    ggplot2::coord_flip() +
                    ggplot2::scale_fill_manual(values = c("Positive" = "#E74C3C", "Negative" = "#2E86C1")) +
                    ggplot2::labs(
                        title = .("LASSO Logistic Regression Coefficients"),
                        subtitle = sub_lab,
                        x = "", y = y_lab, fill = .("Direction")
                    ) +
                    ggtheme
                print(p)
                TRUE
            },
            .rocPlot = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state)) {
                    return(FALSE)
                }
                if (!requireNamespace("pROC", quietly = TRUE)) {
                    return(FALSE)
                }

                roc_obj <- pROC::roc(state$y, state$probabilities, quiet = TRUE, direction = "<", levels = c(0, 1))
                auc_val <- sprintf("%.3f", as.numeric(pROC::auc(roc_obj)))
                # suppressWarnings for the same reason as .populatePerformance: pROC
                # warns when AUC == 1, and an error-only tryCatch let that text escape
                # into jamovi's Analysis Notes on EVERY render (resize, .omv reopen).
                ci_obj <- tryCatch(suppressWarnings(pROC::ci.auc(roc_obj, method = "delong")),
                                   error = function(e) NULL)
                # DeLong collapses to exactly 1-1 at AUC == 1; drawing that beside a
                # table that just said "CI not estimable" contradicts it.
                if (!is.null(ci_obj) && is.finite(ci_obj[1]) && ci_obj[1] >= 1) ci_obj <- NULL

                # pROC::plot.roc explicitly: in the umbrella package spatstat.explore also
                # registers a plot.roc S3 method and wins the dispatch, so a bare plot() on a
                # pROC object died with "Argument 'x' is not of class 'fv'".
                pROC::plot.roc(roc_obj,
                    main = sprintf(.("ROC Curve (AUC = %s)"), auc_val),
                    col = "#2E86C1", lwd = 2, print.auc = FALSE
                )
                if (!is.null(ci_obj)) {
                    legend("bottomright",
                        legend = sprintf("AUC = %s (95%% CI: %.3f-%.3f)", auc_val, ci_obj[1], ci_obj[3]),
                        col = "#2E86C1", lwd = 2, bty = "n"
                    )
                }
                abline(a = 0, b = 1, lty = 2, col = "gray50")
                TRUE
            },

            # ══════════════════════════════════════════════════════════════════
            # Explanatory text outputs
            # ══════════════════════════════════════════════════════════════════
            .populateSummary = function(data, fit) {
                n_sel <- length(fit$selected)
                # This switch had no fallback: an unmapped penalty returned NULL and
                # sprintf then silently shifted every later placeholder along.
                penalty_name <- private$.penaltyLabel()

                # Apparent discrimination for a copy-ready report sentence
                # (reuse single-source apparent AUC from .fitLasso).
                auc_val <- fit$apparent_auc

                top_vars <- if (n_sel > 0) {
                    paste(fit$selected[seq_len(min(5, n_sel))], collapse = ", ")
                } else {
                    .("none")
                }

                # Copy-ready report sentence (complete phrase; placeholders filled from results)
                report <- sprintf(
                    .("%s logistic regression with %s lambda selection and %d-fold cross-validation was applied to %d candidate variables (%d model terms after dummy coding) in %d patients (%d events, %d non-events). %d term(s) were retained: %s."),
                    penalty_name, private$.lambdaLabel(), fit$nfolds, data$n_vars, data$p, data$n,
                    data$n_events, data$n_nonevents, n_sel, top_vars
                )
                if (!is.na(auc_val)) {
                    report <- paste0(report, " ", sprintf(
                        .("Apparent (in-sample) discrimination was AUC = %.3f; enable Bootstrap Internal Validation for an optimism-corrected estimate."),
                        auc_val
                    ))
                }

                self$results$summaryText$setContent(
                    paste0("<p>", htmltools::htmlEscape(report), "</p>")
                )
            },
            .populateExplanations = function() {
                # This panel is shown for whatever penalty is selected, so it must
                # not describe L1 behaviour under Ridge: Ridge shrinks but never
                # zeroes, so "performs automatic variable selection" is simply false
                # there - and the Selected Variables table duly lists every
                # predictor, contradicting the explanation sitting beside it.
                lead <- switch(self$options$penalty,
                    "ridge" = .("<p>Ridge regression adds an L2 penalty to the logistic regression likelihood. It shrinks coefficients toward zero but never <strong>to</strong> zero, so it does <strong>not</strong> perform variable selection: every predictor you supplied is retained in the model. Ridge is the right choice when you believe most predictors carry some signal, or when they are strongly correlated and you want the penalty shared among them rather than one arbitrarily kept.</p>"),
                    "elasticnet" = sprintf(.("<p>Elastic Net blends the L1 (LASSO) and L2 (Ridge) penalties, here with a mixing weight of alpha = %.2f. The L1 part can shrink coefficients exactly to zero, so some variable selection still happens, while the L2 part keeps correlated predictors together instead of letting the model pick one of them arbitrarily.</p>"), self$options$alpha),
                    .("<p>LASSO (Least Absolute Shrinkage and Selection Operator) adds an L1 penalty to the logistic regression likelihood, which shrinks some coefficients exactly to zero. This performs automatic variable selection, identifying the most important predictors for your binary outcome.</p>"))
                heading <- switch(self$options$penalty,
                    "ridge" = .("Ridge Logistic Regression"),
                    "elasticnet" = .("Elastic Net Logistic Regression"),
                    .("LASSO Logistic Regression"))
                self$results$lassoExplanation$setContent(paste0(
                    "<h4>", heading, "</h4>",
                    lead,
                    "<h5>", .("Key Concepts"), "</h5>",
                    "<ul>",
                    if (identical(self$options$penalty, "ridge"))
                        .("<li><strong>Lambda</strong>: Controls regularization strength. Higher lambda shrinks every coefficient further toward zero, but none reaches zero, so the number of variables never changes.</li>")
                    else
                        .("<li><strong>Lambda</strong>: Controls regularization strength. Higher lambda = fewer variables selected.</li>"),
                    .("<li><strong>1SE Rule</strong>: Selects the most parsimonious model within 1 SE of minimum CV error.</li>"),
                    .("<li><strong>Odds Ratio</strong>: exp(coefficient). OR > 1 increases probability of the positive class.</li>"),
                    .("<li><strong>Elastic Net</strong>: Combines L1 and L2 penalties; useful when predictors are correlated.</li>"),
                    "</ul>"
                ))
            },
            .populateMethodologyNotes = function() {
                # Must agree with the Selected Variables table's own scale_note and
                # with the Coefficient Plot axis. This bullet used to claim the
                # reported coefficients were per-standard-deviation, which is what
                # the model is FITTED on but the opposite of what is PRINTED -
                # .populateCoefficients divides by the column SD to put the
                # Coefficient and Odds Ratio columns back on the original scale.
                standardize_note <- if (isTRUE(self$options$standardize)) {
                    .("<li><strong>Standardization (default on):</strong> predictors are centered and scaled before fitting, so the penalty treats variables with different units comparably. The Coefficient and Odds Ratio columns of the Selected Variables table are then back-transformed to the <strong>original measurement scale</strong> (per 1 unit of a continuous predictor, or present vs absent for a binary one). The Importance column and the Coefficient Plot stay on the per-standard-deviation scale, which is the quantity that is comparable across predictors with different units.</li>")
                } else {
                    .("<li>Predictors were <strong>not</strong> standardized; coefficients, odds ratios, importance, and the Coefficient Plot are all on the original measurement scale of each variable.</li>")
                }
                self$results$methodologyNotes$setContent(paste0(
                    "<h4>", .("Technical Notes"), "</h4>",
                    "<ul>",
                    .("<li>LASSO coefficients do not have standard errors or p-values. Use bootstrap validation for inference.</li>"),
                    standardize_note,
                    if (identical(self$options$penalty, "ridge"))
                        .("<li>With correlated predictors, Ridge shares the coefficient among them rather than picking one, which is one reason to prefer it when predictors are collinear.</li>")
                    else
                        .("<li>With correlated predictors, LASSO arbitrarily selects one from a group. Consider elastic net (alpha 0.5).</li>"),
                    .("<li>Events-per-variable (EPV) should be >=10. Below 5, results are unreliable regardless of regularization.</li>"),
                    .("<li>The scoring system rounds coefficients to integers, which loses precision but gains clinical usability.</li>"),
                    sprintf(.("<li>Continuous predictors in the scoring system are dichotomized at %s (set by 'Cut Point for Continuous Predictors'); the score-based performance therefore reflects a simplified point model and may differ from the continuous LASSO model's AUC.</li>"), private$.scoreCutLabel()),
                    .("<li>Bootstrap optimism correction estimates how much the apparent AUC overestimates true performance.</li>"),
                    "</ul>"
                ))
            },
            .populateClinicalGuidance = function() {
                self$results$clinicalGuidance$setContent(paste0(
                    "<h4>", .("Clinical Interpretation"), "</h4>",
                    "<ul>",
                    .("<li>Selected variables are the features most useful for distinguishing the two groups.</li>"),
                    .("<li>Variables NOT selected are not necessarily unimportant - they may be redundant with selected features.</li>"),
                    .("<li>The scoring system assigns positive points for features favoring the event class and negative points for the reference class.</li>"),
                    .("<li>Higher total scores indicate higher probability of the event (positive class).</li>"),
                    .("<li>Always validate the scoring system on an independent cohort before clinical adoption.</li>"),
                    .("<li>Inter-observer agreement should be assessed for any morphologic scoring components.</li>"),
                    "</ul>"
                ))
            }
        )
    )
}
