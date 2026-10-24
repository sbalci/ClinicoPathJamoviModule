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
            .sourcifyOption = function(option) {
                source <- .sourcifyDiagnosticOption(option)
                if (!is.null(source)) return(source)
                super$.sourcifyOption(option)
            },

            # ══════════════════════════════════════════════════════════════════
            # Notice collection (HTML-based)
            # Notices are collected here and rendered into one Html item because
            # jmvcore::Notice content renders as escaped single-line text and these
            # panels need richer text. (jmvcore::Notice itself serializes fine; the
            # old "attempt to apply non-function" came from Group$insert() past the
            # end of the results list - see CLAUDE.md, corrected 2026-09-22.)
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

                # Severity must not be carried by the border colour ALONE. The
                # rgba tints are theme-safe, but the previous restyling also dropped
                # the coloured title, leaving a pale 4px stripe as the only cue - so
                # a fatal ERROR and the routine "Analysis Complete" INFO read as two
                # identical grey boxes, and identically so for a red-green colour
                # deficient reader. The severity word is spelled out instead: it
                # survives any theme and any form of colour blindness.
                typeStyles <- list(
                    ERROR          = list(bgcolor = "rgba(220, 38, 38, 0.10)", border = "#fca5a5", label = .("Error")),
                    STRONG_WARNING = list(bgcolor = "rgba(234, 88, 12, 0.10)", border = "#fdba74", label = .("Serious warning")),
                    WARNING        = list(bgcolor = "rgba(202, 138, 4, 0.12)", border = "#fde047", label = .("Warning")),
                    INFO           = list(bgcolor = "rgba(37, 99, 235, 0.08)", border = "#93c5fd", label = .("Information"))
                )

                # Most severe first. Notices are collected in the order the checks run, so
                # the "Analysis unit" INFO raised during data cleaning sat above every
                # warning. order() is stable: one severity keeps the order it was raised in.
                severity <- c("ERROR", "STRONG_WARNING", "WARNING", "INFO")
                notices <- private$.noticeList[order(match(
                    vapply(private$.noticeList, `[[`, character(1), "type"), severity))]

                html <- "<div style='margin: 10px 0;'>"
                for (notice in notices) {
                    style <- typeStyles[[notice$type]]
                    if (is.null(style)) style <- typeStyles$INFO
                    html <- paste0(
                        html,
                        "<div style='background-color: ", style$bgcolor, "; ",
                        "border-left: 4px solid ", style$border, "; ",
                        "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                        "<strong>",
                        jmvcore::htmlEscape(style$label), ": ",
                        jmvcore::htmlEscape(notice$title), "</strong><br>",
                        "<span style='color: inherit;'>",
                        jmvcore::htmlEscape(notice$content), "</span>",
                        "</div>"
                    )
                }
                html <- paste0(html, "</div>")
                self$results$notices$setContent(html)
            },
            # library-audit 2026-09-16 meddecide [LOW] DONE (same class): performance, scoringPerformance,
            # validationTable and methodComparison have a fixed row set, so .init() scaffolds the rows and
            # .run() fills them with setRow()
            # One place for those rows: `label` is the column that names the row, `rows` maps each stable
            # row key to its label, `fill` lists the value columns .run() writes (and blanks on a re-run),
            # and `static` holds per-row text that never depends on the data.
            .fixedRowTables = function() {
                list(
                    performance = list(
                        label = "metric", fill = c("value", "interpretation"),
                        rows = c(
                            auc = .("AUC (apparent; DeLong 95% CI)"),
                            threshold = .("Optimal threshold"),
                            accuracy = .("Accuracy"),
                            sensitivity = .("Sensitivity (Recall)"),
                            specificity = .("Specificity"),
                            precision = .("Precision (PPV)"),
                            f1 = .("F1 Score"),
                            brier = .("Brier Score"))),
                    scoringPerformance = list(
                        label = "metric", fill = "value",
                        rows = c(
                            method = .("Scoring method"),
                            auc = .("Score AUC (apparent)"),
                            cutoff = .("Optimal score cutoff (chosen on this data)"),
                            accuracy = .("Accuracy"),
                            sensitivity = .("Sensitivity"),
                            specificity = .("Specificity"),
                            precision = .("Precision"),
                            f1 = .("F1 Score"),
                            mean_pos = .("Mean score (positive class)"),
                            mean_neg = .("Mean score (reference class)"),
                            range = .("Score range"))),
                    validationTable = list(
                        label = "metric", fill = c("apparent", "optimism", "corrected"),
                        rows = c(
                            auc = .("AUC"),
                            brier = .("Brier Score"),
                            slope = .("Calibration slope"))),
                    methodComparison = list(
                        label = "method", fill = c("auc", "accuracy", "info_loss"),
                        rows = c(
                            beta10 = "Beta10",
                            smallest = .("Smallest-unit"),
                            maxscaled = .("Max-scaled"),
                            # Named for the penalty actually fitted: ridge and elastic net were
                            # labelled LASSO here (LR-04, 2026-10-21).
                            full = sprintf(.("Full %s model (continuous)"), private$.penaltyLabel())),
                        static = list(reference = c(
                            beta10 = "Mehta et al. 2016",
                            smallest = .("Sullivan-type (Sullivan et al. 2004), B = smallest contribution"),
                            maxscaled = .("beta / max|beta| x maximum points"),
                            full = .("Reference (no rounding)"))))
                )
            },
            .init = function() {
                # Laid down before any guard below, because .run() fills these rows by key
                # whether or not the inputs are complete.
                fixed <- private$.fixedRowTables()
                for (nm in names(fixed)) {
                    spec <- fixed[[nm]]
                    for (key in names(spec$rows)) {
                        values <- list(unname(spec$rows[[key]]))
                        names(values) <- spec$label
                        for (col in names(spec$static))
                            values[[col]] <- unname(spec$static[[col]][[key]])
                        self$results[[nm]]$addRow(rowKey = key, values = values)
                    }
                }

                if (!requireNamespace("glmnet", quietly = TRUE)) {
                    # Was the only raw untranslated panel in an otherwise fully
                    # translated file: a non-English user hit an English-only wall.
                    self$results$todo$setContent(paste0(
                        "<div class='alert alert-danger'><h4>", .("Missing Dependency"), "</h4>",
                        "<p>", .("Package 'glmnet' is required. Install with: install.packages('glmnet')"),
                        "</p></div>"
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
                    # setRowNums BEFORE setValues. An Output column given a bare vector is
                    # written positionally -- value i lands in spreadsheet row i -- so under an
                    # active jamovi row filter self$data is a SUBSET and every prediction is
                    # saved against the wrong patient. Same pattern as decision.b.R:2304.
                    self$results$predictions$setRowNums(rownames(self$data))
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
                private$.cutAdjusted <- character(0)
                # A saved result can be restored before an incomplete selection is run.
                # Clear dependent content before every early-return path.
                for (nm in c("notices", "suitabilityReport", "summaryText", "lassoExplanation",
                             "methodologyNotes", "clinicalGuidance"))
                    self$results[[nm]]$setContent("")
                for (nm in c("cv_plot", "coef_plot", "roc_plot"))
                    self$results[[nm]]$setState(NULL)

                # Every table is rebuilt with addRow() below and jmvcore never checks for
                # duplicate row keys, so an option toggle that is not in a table's
                # clearWith (showSummary, cv_plot, ...) re-ran .run() and doubled its rows.
                # One reset here covers the six tables whose rows depend on the data.
                for (nm in c("modelSummary", "coefficients", "scoringTable",
                             "lookupTable", "variableImportance", "modelComparison")) {
                    tbl <- tryCatch(self$results[[nm]], error = function(e) NULL)
                    if (!is.null(tbl)) .deleteTableRows(tbl)
                }
                # The four fixed-row tables keep their .init() rows; only their values are
                # blanked, so a run that stops early cannot leave the previous run's numbers.
                fixed <- private$.fixedRowTables()
                for (nm in names(fixed)) {
                    blank <- rep(list(NA), length(fixed[[nm]]$fill))
                    names(blank) <- fixed[[nm]]$fill
                    for (key in names(fixed[[nm]]$rows))
                        self$results[[nm]]$setRow(rowKey = key, values = blank)
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

                # No event level chosen and none that may be assumed: an unconfigured
                # analysis, not an error. Show the levels to choose from and stop.
                coding <- .rocOutcomeCoding(self$data[[self$options$outcome]], self$options$outcomeLevel)
                if (identical(coding$status, "unset")) {
                    private$.showEventLevelPending(coding)
                    return()
                }

                withr::local_seed(self$options$random_seed)

                # ── 1. Clean data ──────────────────────────────────────────────
                data <- tryCatch(private$.cleanData(), error = function(e) {
                    # A .checkpoint() restart is CONTROL FLOW, not a failure: it
                    # arrives here as an error carrying code = "restart" and must
                    # leave before anything is rendered.
                    if (identical(e$code, "restart")) stop(e)
                    msg_html <- jmvcore::htmlEscape(e$message)
                    self$results$todo$setContent(paste0(
                        "<div class='alert alert-danger'><h4>", .("Data Error"), "</h4><p>",
                        msg_html, "</p></div>"
                    ))
                    private$.addNotice(
                        "ERROR", .("Data Error"),
                        sprintf(.("Data preparation failed: %s"), e$message)
                    )
                    private$.renderNotices()
                    # .cleanData() refuses through jmvcore::reject(), which is a plain
                    # simpleError. Swallowing it made .run() return NORMALLY, so jamovi
                    # never entered its error state and the user saw the refusal box
                    # sitting above nineteen fully-titled, completely blank result rows
                    # - it read as a partial computation rather than a refusal.
                    # Notices are rendered first (they carry the actionable detail),
                    # then the condition is re-raised untouched so its class and $code
                    # survive.
                    stop(e)
                })

                self$results$todo$setContent("")

                # ── 2. Suitability assessment ──────────────────────────────────
                # Always run. Every sample-adequacy warning this analysis produces
                # (events-per-variable, sample size, class balance, collinearity)
                # used to live behind the checkbox, so a user who unticked it to
                # shorten the output got a full fit on 8 events across 12 predictors
                # - EPV 0.67 - with not one warning anywhere on screen. The option
                # now controls only the HTML panel (see .suitabilityAssessment);
                # the notices fire unconditionally.
                private$.suitabilityAssessment(data)

                private$.checkpoint()

                # ── 3. Fit LASSO model ─────────────────────────────────────────
                fit_result <- tryCatch(private$.fitLasso(data), error = function(e) {
                    # Same two rules as the .cleanData() handler above: a checkpoint
                    # restart leaves first, and a jmvcore::reject() refusal must reach
                    # jamovi's error state instead of being downgraded to a normal
                    # return with blank tables underneath it.
                    if (identical(e$code, "restart")) stop(e)
                    msg_html <- jmvcore::htmlEscape(e$message)
                    self$results$todo$setContent(paste0(
                        "<div class='alert alert-danger'><h4>", .("Model Fitting Error"), "</h4><p>",
                        msg_html, "</p></div>"
                    ))
                    private$.addNotice(
                        "ERROR", .("Model Fitting Error"),
                        sprintf(.("Penalized model fitting failed: %s"), e$message)
                    )
                    private$.renderNotices()
                    stop(e)
                })

                # ── Notice: no variables selected ──────────────────────────────
                if (length(fit_result$selected) == 0) {
                    # The advice used to say "try lambda.min instead of lambda.1se"
                    # unconditionally - useless, and slightly insulting, to a user
                    # who is already on lambda.min. Offer the remedy that is left.
                    remedy <- if (identical(self$options$lambda, "lambda.1se"))
                        .("Try the Minimum CV Error lambda instead of the 1SE rule, or add more informative predictors.")
                    else
                        .("You are already using the least conservative lambda (Minimum CV Error), so the penalty is not the limiting factor: none of these predictors carries enough signal at this sample size. Add more informative predictors or collect more cases.")
                    # The Coefficient Plot stays on screen with nothing to draw; say why it
                    # is empty rather than leave a blank panel (LR-02, 2026-10-21).
                    if (isTRUE(self$options$coef_plot))
                        remedy <- paste(remedy, .("The Coefficient Plot is empty because no term was retained."))
                    private$.addNotice(
                        "STRONG_WARNING", .("No Variables Selected"),
                        sprintf(.("The penalized model retained no informative terms at the chosen lambda. %s"), remedy)
                    )
                }

                # Listwise deletion is dominated by the single worst-populated
                # predictor, so a biomarker panel with one sparsely-stained marker
                # silently becomes a different-cohort analysis. Name the actual
                # cause: rows dropped for a missing outcome are NOT a
                # missing-predictor problem and must not send the reader hunting one.
                if (!is.null(data$n_excluded) && data$n_excluded > 0) {
                    reasons <- character(0)
                    if (isTRUE(data$n_excl_pred > 0))
                        reasons <- c(reasons, sprintf(
                            .("%d had at least one predictor missing"), data$n_excl_pred))
                    if (isTRUE(data$n_excl_outcome_na > 0))
                        reasons <- c(reasons, sprintf(
                            .("%d had a missing outcome"), data$n_excl_outcome_na))
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
                            .("%1$d of %2$d cases (%3$.1f%%) were excluded%4$s; the analysis uses the remaining %5$d cases.%6$s"),
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
                            .("%1$d selected variable(s) had the same value in every case and were removed before fitting: %2$s. A constant carries no information and cannot be penalised or selected. %3$d variables remain and are shown as Variables analyzed."),
                            data$n_dropped_constant,
                            paste(data$dropped_constant, collapse = ", "), data$n_vars)
                    )
                }
                # Not "the same value in every case": the spreadsheet shows these varying.
                if (length(data$dropped_single) > 0) {
                    private$.addNotice(
                        "WARNING",
                        .("Predictors constant among the analyzed cases"),
                        sprintf(
                            .("Removed before fitting: %1$s. Each varies only in rows that were excluded for a missing value, so every analyzed case has the same value, and a predictor that does not vary cannot be penalised or selected. %2$d variables remain and are shown as Variables analyzed."),
                            paste(data$dropped_single, collapse = ", "), data$n_vars)
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
                    # setRowNums BEFORE setValues -- pred_full is positional over the FILTERED
                    # self$data, so without the row identities a filtered run writes each
                    # patient's predicted probability onto a different patient.
                    self$results$predictions$setRowNums(rownames(self$data))
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
                        .("Penalized logistic regression completed: %1$d/%2$d model terms selected using the %3$s penalty with the %4$s lambda (N=%5$d, %6$d events)."),
                        n_sel, data$p, private$.penaltyLabel(), private$.lambdaLabel(), data$n, data$n_events
                    )
                )

                # ── 13. Render all collected notices as HTML ───────────────────
                private$.renderNotices()
            },

            # Pending state: the outcome is chosen but its event level is not, and it is
            # not a 0/1 numeric whose 1 may be assumed. jamovi greys a continuous
            # column's LevelSelector, so a numeric 1/2 outcome needs its measure type
            # changed before a level can be picked; say so.
            .showEventLevelPending = function(coding) {
                found <- paste(sprintf("%s (n=%d)", jmvcore::htmlEscape(coding$levels), coding$counts),
                               collapse = ", ")
                how <- if (isTRUE(coding$numeric))
                    .("This numeric outcome is not coded 0/1, so neither value can be assumed to be the event. Set the outcome's measure type to Nominal, then choose the 'Event level'.")
                else
                    .("Choose the 'Event level': the outcome level that means the condition is present (for example Malignant). It is never taken from the order of the levels.")
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-info'><h4>", .("Choose the event level"), "</h4>",
                    "<p>", how, "</p><p>", .("Levels found:"), " ", found, "</p></div>"))
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
                # R API only (jamovi's variable list will not place one column twice): the
                # outcome as its own predictor separates the classes perfectly.
                if (outcome_var %in% explanatory_vars)
                    jmvcore::reject(.("The outcome variable cannot also be an explanatory variable."))

                # An explicit NA level (addNA(), R API only) is a missing value, not a
                # category: it passed complete.cases() and became a model term, and a
                # missing outcome was reported as "outcome level not modelled".
                # levels = levels(x) keeps unused levels (a bare factor() drops them).
                drop_na_level <- function(x)
                    if (is.factor(x) && anyNA(levels(x))) factor(x, levels = levels(x), exclude = NA) else x
                predictors <- data[explanatory_vars]
                predictors[] <- lapply(predictors, drop_na_level)
                # R API only (jamovi sends factors): model.matrix codes character and logical
                # columns as factors anyway, but is.factor() below did not, so a dummy such as
                # smokerTRUE printed "= 1" instead of "present". Same coefficients either way.
                predictors[] <- lapply(predictors, function(x)
                    if (is.character(x) || is.logical(x)) factor(x) else x)
                # jamovi delivers an Ordinal column as an ORDERED factor. Recorded here
                # so the coefficient table can say how it was coded (see model.matrix).
                ordinal_vars <- names(predictors)[vapply(predictors, is.ordered, logical(1))]

                # Event coding through the shared coder in R/utils.R (as enhancedROC and
                # psychopdaROC do). The event is never inferred from level order or size:
                # it used to be the SECOND level in R while jamovi's LevelSelector filled
                # in the FIRST, so the same data gave inverted odds ratios in the two.
                # Only a numeric 0/1 outcome defaults (1 = event); .run() has already
                # stopped with a pending panel when no level was chosen otherwise. The
                # coder also treats an explicit NA level and "" as missing.
                coding <- .rocOutcomeCoding(data[[outcome_var]], self$options$outcomeLevel)
                # reject() runs its message through jmvcore::format(), which turns any {name}
                # into "..." - so a level called "{grade}" would vanish from the refusal.
                no_braces <- function(x) chartr("{}", "()", x)
                if (identical(coding$status, "single"))
                    jmvcore::reject(.("Outcome variable must have at least 2 observed values."))
                if (identical(coding$status, "invalid"))
                    jmvcore::reject(sprintf(
                        .("Specified event level '%1$s' not found in the outcome variable. Observed levels: %2$s."),
                        no_braces(coding$positive), no_braces(paste(coding$levels, collapse = ", "))))
                if (identical(coding$status, "unset"))
                    jmvcore::reject(.("Choose the 'Event level' of the outcome."))
                observed_levels <- coding$levels
                # Refused, not narrowed: the reference used to be the first non-event level
                # in level order, so which two groups were compared was inferred, not chosen.
                if (length(observed_levels) > 2)
                    jmvcore::reject(sprintf(
                        .("The outcome has %1$d observed levels (%2$s), but this analysis compares exactly two. Filter the data to the two levels to compare, or recode the outcome into two groups."),
                        length(observed_levels), no_braces(paste(observed_levels, collapse = ", "))))
                event_level <- coding$positive
                ref_level <- setdiff(observed_levels, event_level)
                outcome_chr <- as.character(coding$values)
                status <- rep(NA_real_, length(outcome_chr))
                status[outcome_chr == event_level] <- 1
                status[outcome_chr == ref_level] <- 0

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
                # Listwise deletion is the usual cause of the three refusals below, and the exclusion
                # notice that names it is only raised once cleaning succeeds: a sparse marker recorded
                # in one diagnostic group left a single outcome class with no count or cause on screen.
                why <- function(cc) sprintf(
                    .("%1$d of %2$d cases have the outcome and every predictor recorded; %3$d were excluded because a predictor was missing."),
                    sum(cc), length(cc), sum(!is.na(status) & !complete.cases(predictors)))
                if (sum(complete) < 10)
                    jmvcore::reject(paste(.("Too few complete cases for analysis."), why(complete)))
                # A predictor can vary overall yet take one value in every complete case (its other
                # values occur only in rows excluded for a missing value). droplevels() below then
                # left such a factor a single level and model.matrix() aborted the whole analysis;
                # a numeric one was dropped as a degenerate column without a word. Drop it here,
                # say so, and recount without it: adding rows back cannot make another predictor
                # constant, so one recount is enough.
                single <- vapply(predictors, function(x) length(unique(x[complete])) < 2, logical(1))
                dropped_single <- names(predictors)[single]
                if (any(single)) {
                    predictors <- predictors[, !single, drop = FALSE]
                    explanatory_vars <- names(predictors)
                    if (ncol(predictors) < 2) jmvcore::reject(.("Fewer than 2 non-constant predictors remain."))
                    complete <- complete.cases(status, predictors)
                }
                n_complete <- sum(complete)

                # Why each row went away: a partition of !complete (outcome is judged
                # first, then predictors), so a missing outcome is never announced as a
                # missing predictor and sends nobody hunting a sparse marker.
                outcome_na <- is.na(coding$values)
                n_excl_outcome_na <- sum(outcome_na)
                n_excl_pred <- sum(!is.na(status) & !complete.cases(predictors))

                status_cc <- status[complete]
                id_var <- self$options$patientID
                if (!is.null(id_var)) {
                    if (id_var %in% c(outcome_var, explanatory_vars))
                        jmvcore::reject(.("The patient ID must be a separate identifier, not an outcome or predictor."))
                    ids <- data[[id_var]][complete]
                    # Test native values: formatting large numeric IDs as text can
                    # collapse distinct, exactly representable identifiers.
                    missing_id <- is.na(ids)
                    if (is.factor(ids) || is.character(ids))
                        missing_id <- missing_id | trimws(as.character(ids)) == ""
                    if (any(missing_id))
                        jmvcore::reject(.("Patient IDs are missing among the complete analysis rows. Supply an ID for every analyzed observation."))
                    if (anyDuplicated(ids))
                        jmvcore::reject(.("Repeated patient IDs were found among the analysis rows. This analysis requires independent observations and does not support repeated lesions, visits, or readings. Create a justified patient-level summary or use a model and validation method that account for clustering."))
                }
                private$.addNotice("INFO", .("Analysis unit: independent observations"),
                    if (is.null(id_var))
                        .("Each analyzed row is treated as one independent observation. Repeated lesions, visits, or readings from the same patient violate this assumption. Supply the optional patient ID to check for repeated patients; this analysis does not model clustering.")
                    else
                        .("Patient IDs are unique among the analyzed rows. Fitting, cross-validation, and bootstrap validation assume that these observations are independent; unique IDs alone cannot establish independence."))
                if (!(length(unique(status_cc)) == 2 && all(unique(status_cc) %in% c(0, 1)))) {
                    left <- if (all(status_cc == 1)) event_level else ref_level
                    jmvcore::reject(paste(.("Outcome is not binary after filtering."),
                        sprintf(.("Only cases with outcome '%s' remain."), no_braces(left)), why(complete)))
                }

                n_events <- sum(status_cc == 1)
                n_nonevents <- sum(status_cc == 0)
                if (n_events < 5 || n_nonevents < 5) {
                    jmvcore::reject(paste(.("Need at least 5 cases in each outcome class."),
                        sprintf(.("The complete cases include %1$d events and %2$d non-events."), n_events, n_nonevents),
                        why(complete)))
                }

                # Build design matrix
                # droplevels: a level seen only in excluded rows has no patients. When it was the
                # FIRST level the other dummies summed to 1 against an empty reference (and a
                # character column converted above kept such a level, which model.matrix's own
                # conversion of the complete cases never had). A contrasts attribute set on a factor
                # (R API) would override the pinned option below and the "first level" reference.
                pred_cc <- droplevels(predictors[complete, , drop = FALSE])
                for (nm in names(pred_cc)) attr(pred_cc[[nm]], "contrasts") <- NULL
                # Treatment contrasts for EVERY factor, ordered ones included. R's default
                # codes an ordered factor with orthogonal polynomials (grade.L, grade.Q):
                # odds ratios per unit of a polynomial contrast, and a scoring rule that
                # gave the grade.Q points to grades 1 AND 3 while printing "present".
                # Pinned locally so a session-wide options(contrasts) cannot change it.
                X <- tryCatch(
                    {
                        mm <- withr::with_options(
                            list(contrasts = c(unordered = "contr.treatment", ordered = "contr.treatment")),
                            model.matrix(~., data = pred_cc))
                        # Keep provenance before presentation names are made unique.
                        source_index <- attr(mm, "assign")[-1]
                        source_names <- names(pred_cc)[source_index]
                        column_source <- stats::setNames(source_names, colnames(mm)[-1])
                        column_factor <- vapply(pred_cc[source_names], is.factor, logical(1))
                        column_level <- vapply(seq_along(source_names), function(i) {
                            src <- source_names[i]
                            if (!is.factor(pred_cc[[src]])) return(NA_character_)
                            levels(pred_cc[[src]])[1 + sum(source_names[seq_len(i)] == src)]
                        }, character(1))
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
                has_aliases <- anyDuplicated(gsub("`", "", colnames(X), fixed = TRUE)) > 0
                X <- .stripBackticks(X)
                names(column_source) <- names(column_factor) <- names(column_level) <- colnames(X)

                # Remove degenerate columns
                col_vars <- apply(X, 2, var, na.rm = TRUE)
                good_cols <- !is.na(col_vars) & col_vars > 0
                if (sum(good_cols) < 2) jmvcore::reject(.("Too few non-degenerate predictor columns."))
                X <- X[, good_cols, drop = FALSE]
                column_source <- column_source[good_cols]
                column_factor <- column_factor[good_cols]
                column_level <- column_level[good_cols]

                # Keep raw measurements. glmnet learns its 1/n scaling separately
                # in every CV training fold and bootstrap fit, and returns raw-unit
                # coefficients. These identity transforms retain the helper contract.
                X_center <- rep(0, ncol(X))
                X_sd <- rep(1, ncol(X))
                names(X_center) <- names(X_sd) <- colnames(X)

                list(
                    X = X,
                    X_center = X_center,
                    X_sd = X_sd,
                    column_source = column_source,
                    column_factor = column_factor,
                    column_level = column_level,
                    has_aliases = has_aliases,
                    y = status_cc,
                    n = n_complete,
                    n_total = nrow(self$data),
                    n_excluded = nrow(self$data) - n_complete,
                    n_excl_outcome_na = n_excl_outcome_na,
                    n_excl_pred = n_excl_pred,
                    n_dropped_constant = n_dropped_constant,
                    dropped_constant = dropped_constant,
                    dropped_single = dropped_single,
                    # ncol(X) counts design-matrix COLUMNS (a 5-level factor is 4 of
                    # them), so it cannot stand in for "variables you selected".
                    n_vars = length(explanatory_vars),
                    n_events = n_events,
                    n_nonevents = n_nonevents,
                    p = ncol(X),
                    complete_idx = which(complete),
                    ordinal_vars = intersect(ordinal_vars, explanatory_vars),
                    # Treatment contrasts: every factor term is compared with the first level.
                    factor_ref = vapply(Filter(is.factor, pred_cc), function(x) levels(x)[1], character(1)),
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
                        detail = sprintf(.("EPV = %.1f (meets the EPV >= 10 convention)"), epv)
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
                        detail = sprintf(.("N = %d (meets the N >= 100 convention)"), data$n)
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
                        detail = sprintf(.("p = %1$d predictors, n/p = %2$.1f (good ratio)"), data$p, data$n / data$p)
                    )
                } else {
                    checks$p <- list(
                        status = "yellow", label = .("Predictor count"),
                        detail = sprintf(.("p = %1$d predictors, n/p = %2$.1f (regularization essential)"), data$p, data$n / data$p)
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
                    overall <- paste0("<span style='color:green;font-weight:bold;'>", .("No concerns flagged by these checks"), "</span>")
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
                    "</tbody></table>",
                    "<p>", .("The EPV threshold of 10 comes from Peduzzi et al. (1996); the other bands are conventions. Rules of thumb like these should give way to sample-size criteria for prediction models (Riley et al. 2019, 2020), and penalization does not make a small sample reliable (Riley et al. 2021)."), "</p>"
                )
                # Only the PANEL is optional; the notices below always fire.
                if (isTRUE(self$options$suitabilityCheck))
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
                            .("Data suitability: %1$d major concern(s) detected (%2$s). Results may be unreliable; consider reducing predictors or collecting more data."),
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
            # Bootstrap copies of an observation stay together: the same source
            # case must never appear in a training and held-out fold simultaneously.
            .stratifiedFolds = function(y, nfolds, groups = seq_along(y)) {
                first <- !duplicated(groups)
                group_y <- y[first]
                if (nfolds < 3 || min(sum(group_y == 1), sum(group_y == 0)) < nfolds)
                    stop("Too few independent cases per class for stratified cross-validation.")
                group_fold <- integer(length(group_y))
                for (cls in c(1, 0)) {
                    ii <- which(group_y == cls)
                    group_fold[ii] <- sample(rep(seq_len(nfolds), length.out = length(ii)))
                }
                group_fold[match(groups, groups[first])]
            },

            .cvFit = function(X, y, alpha, foldid) {
                args <- list(x = X, y = y, family = "binomial", alpha = alpha,
                             standardize = isTRUE(self$options$standardize),
                             type.measure = "deviance", foldid = foldid)
                # At an exact null optimum the automatic lambda maximum is zero,
                # and glmnet's path interpolation fails. All full-data slopes are
                # zero for every positive penalty; a finite grid permits CV and the
                # ordinary intercept-only fit without inventing a coefficient.
                centered <- sweep(X, 2, colMeans(X))
                sd_pop <- sqrt(colMeans(centered^2))
                sd_pop[sd_pop == 0] <- 1
                gradient <- colMeans(sweep(centered, 2, sd_pop, "/") * (y - mean(y)))
                null_path <- all(abs(gradient) <= 32 * .Machine$double.eps)
                if (null_path) args$lambda <- exp(seq(log(1), log(1e-6), length.out = 100))
                # glmnet warns on EVERY call when a fold is small (per-case CV error under 3 cases a
                # fold; "dangerous ground" under 8 of a class in a training set): 44 raw English lines
                # in jamovi's Analysis Notes with the bootstrap on at n = 27. Muffled here and recorded;
                # the main fit turns each into one notice.
                known <- c(ungrouped = "grouped=FALSE enforced", small_class = "dangerous ground")
                warned <- character(0)
                ans <- withCallingHandlers(
                    .quietly(do.call(glmnet::cv.glmnet, args)),
                    warning = function(w) {
                        hit <- names(known)[vapply(known, grepl, logical(1),
                                                   x = conditionMessage(w), fixed = TRUE)]
                        if (length(hit)) {
                            warned <<- union(warned, hit)
                            invokeRestart("muffleWarning")
                        }
                    })
                attr(ans, "glmnet_warned") <- warned
                attr(ans, "null_path") <- null_path
                attr(ans$glmnet.fit, "null_path") <- null_path
                if (null_path) {
                    attr(ans, "null_intercept") <- stats::qlogis(mean(y))
                    attr(ans$glmnet.fit, "null_intercept") <- stats::qlogis(mean(y))
                }
                ans
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
                            .("%1$d folds were requested but only %2$d could be used: stratified cross-validation cannot create more folds than there are cases in the smaller outcome class (%3$d). Fewer folds mean a noisier lambda; a larger or better-balanced sample is the real remedy."),
                            nfolds_requested, nfolds, min_class)
                    )
                }

                # Stratified CV folds for balanced sampling
                foldid <- private$.stratifiedFolds(data$y, nfolds)

                cv_fit <- private$.cvFit(data$X, data$y, alpha_val, foldid)
                warned <- attr(cv_fit, "glmnet_warned")
                if ("ungrouped" %in% warned)
                    private$.addNotice("INFO", .("Cross-validation error computed per case"),
                        sprintf(.("With %1$d folds and %2$d cases some folds hold fewer than 3 cases, so glmnet computed the cross-validated deviance and its standard error per case rather than per fold. The 1SE rule uses that standard error."),
                            nfolds, data$n))
                if ("small_class" %in% warned)
                    private$.addNotice("WARNING", .("Few cases of one class per fold"),
                        sprintf(.("At least one cross-validation training set held fewer than 8 cases of one outcome class (%1$d events and %2$d non-events across %3$d folds). glmnet warns that fits this sparse are unstable: the chosen lambda depends on how these few cases fall into folds, which the random seed decides."),
                            data$n_events, data$n_nonevents, nfolds))
                if (isTRUE(attr(cv_fit, "null_path")))
                    private$.addNotice("INFO", .("Intercept-only optimum"),
                        .("All predictor gradients are zero at the intercept-only fit. A fixed positive lambda grid (1 to 0.000001) was used because the automatic path degenerates at zero. The fitted model assigns the overall event rate to every observation and has no discrimination."))

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
                if (isTRUE(attr(final_model, "null_path"))) {
                    coefs[-1, 1] <- 0
                    coefs[1, 1] <- attr(final_model, "null_intercept")
                }
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
                    list(.("Complete cases analyzed"), as.character(data$n)),
                    # Same partition as the exclusion notice.
                    list(.("Excluded from analysis"),
                         if (data$n_excluded > 0)
                             sprintf("%d of %d (%.1f%%)", data$n_excluded, data$n_total,
                                     100 * data$n_excluded / data$n_total)
                         else .("None")),
                    # .rocLabel: text cells are parsed as HTML, so a level "<LOD" rendered blank.
                    list(.("Event class (positive)"), paste0(.rocLabel(data$event_level, note = FALSE), " (n=", data$n_events, ")")),
                    list(.("Reference class"), paste0(.rocLabel(data$ref_level, note = FALSE), " (n=", data$n_nonevents, ")")),
                    # Name what it counts. data$p is ncol(X) - a 5-level factor is 4
                    # of them - so labelling it "Candidate predictors" contradicted
                    # the constant-predictor notice, which counts VARIABLES (3 vs 6
                    # on the same screen). data$p is the right EPV denominator; the
                    # label was the wrong part.
                    # variables -> terms -> selected terms. "Variables selected"
                    # beside "Selected predictors" would have been two near-identical
                    # labels for two different quantities.
                    list(.("Variables analyzed"), as.character(data$n_vars)),
                    list(.("Model terms (after dummy coding)"), as.character(data$p)),
                    list(.("Terms selected"), as.character(length(fit$selected))),
                    list(.("Penalty type"), penalty_label),
                    list(.("Alpha"), sprintf("%.2f", fit$alpha)),
                    # %.4g: lambda.min on the bundled data is 0.00021, which %.4f printed as 0.0002
                    list(.("Lambda (optimal)"), sprintf("%.4g", fit$lambda)),
                    list(.("Lambda selection"), lambda_label),
                    list(.("CV folds"), as.character(fit$nfolds)),
                    # the folds are drawn at random, so the seed decides lambda and the selection
                    list(.("Random seed"), as.character(self$options$random_seed))
                )

                # Break the exclusions down by cause, but only when there are any
                # and only for the causes that actually fired - permanent
                # "0" rows would be noise on the common clean-data path.
                if (data$n_excluded > 0) {
                    # The indent prefix stays OUTSIDE .() - it is layout, not text,
                    # and a translator should never have to preserve it.
                    indent <- "  - "
                    causes <- list(
                        list(paste0(indent, .("predictor missing")), data$n_excl_pred),
                        list(paste0(indent, .("outcome missing")), data$n_excl_outcome_na)
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
                table$setNote("source_names", private$.sourceNameNote(data))
                # glmnet returns raw-unit coefficients; identity transforms are retained
                # for the shared coefficient/scoring helper contract.
                ctr <- if (!is.null(data) && !is.null(data$X_center)) data$X_center else NULL
                sdv <- if (!is.null(data) && !is.null(data$X_sd)) data$X_sd else NULL
                b0 <- fit$intercept
                ord <- if (!is.null(data)) data$ordinal_vars else character(0)
                table$setNote("ordinal_note", if (length(ord) > 0) sprintf(
                    .("Ordinal predictor(s) %s are coded with one indicator per level, each compared with that predictor's lowest level; they are not modeled as a linear trend."),
                    .rocLabel(paste(ord, collapse = ", "))) else NULL)
                # A dummy such as gradeG2 is an odds ratio against a level the table never named.
                ref_src <- if (!is.null(data$factor_ref) && length(fit$selected) > 0)
                    unique(data$column_source[fit$selected][data$column_factor[fit$selected]]) else character(0)
                table$setNote("reference_note", if (length(ref_src) > 0) sprintf(
                    .("Each factor term is compared with its predictor's reference (first) level: %s. The penalized estimates depend on that choice: with another reference level the coefficients differ, and LASSO or Elastic Net can retain a different set of terms."),
                    .rocLabel(paste(sprintf("%s = %s", ref_src, data$factor_ref[ref_src]), collapse = "; "))) else NULL)
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

                # Raw-unit coefficients and odds ratios; importance is per sample SD.
                sds <- if (!is.null(data) && !is.null(data$X_sd)) data$X_sd else NULL
                # Importance is the per-SD magnitude whatever `standardize` says: |beta| x
                # SD of the FITTED column (1 when standardized). Without standardization it
                # used to be |per-unit beta| / max, which ranked a 0/1 marker above a Ki-67
                # (%) with four times its per-SD effect purely because of the units.
                fit_sd <- if (!is.null(data)) private$.fitColSDs(data$X)[fit$selected] else 1
                per_sd <- abs(fit$selected_coefs) * fit_sd
                max_abs <- max(per_sd)
                for (i in seq_along(fit$selected)) {
                    coef_sd <- fit$selected_coefs[i]
                    sd_i <- if (!is.null(sds) && fit$selected[i] %in% names(sds))
                        sds[[fit$selected[i]]] else 1
                    if (!is.finite(sd_i) || sd_i == 0) sd_i <- 1
                    coef_val <- coef_sd / sd_i
                    or_val <- exp(coef_val)
                    importance <- if (max_abs > 0) unname(per_sd[i]) / max_abs else NA

                    table$addRow(rowKey = i, values = list(
                        variable = .rocLabel(fit$selected[i], note = FALSE),
                        coefficient = coef_val,
                        oddsRatio = or_val,
                        importance = importance
                    ))
                }
                add_intercept()
                # LASSO penalization yields biased, shrunken coefficients without a
                # valid closed-form sampling distribution, so standard confidence
                # intervals are not reported here (naive post-selection CIs have
                # incorrect coverage). Bootstrap validation gives optimism-corrected
                # PERFORMANCE, not coefficient inference; the unpenalized refit
                # (Model Comparison) is a descriptive comparison, not valid
                # post-selection inference either.
                # The old tail pointed to the Model Comparison table "for unpenalized
                # estimates on the selected variables"; that table reports AUC, AIC and
                # Brier only, no estimates.
                table$setNote("ci_note",
                    .("LASSO coefficients are penalized (shrunken) and have no valid standard confidence intervals; they are omitted rather than shown as blanks. Bootstrap validation estimates optimism-corrected performance (AUC, Brier score, calibration slope), not coefficient uncertainty."))
                table$setNote("scale_note", if (isTRUE(self$options$standardize))
                    .("Predictors were standardized before fitting so that the penalty treats them comparably, but the Coefficient and Odds Ratio columns are reported on the ORIGINAL measurement scale (per 1 unit of a continuous predictor, or present vs absent for a binary one). The Importance column is the per-standard-deviation magnitude, which is what can be compared across predictors with different units. Odds ratios remain penalized (shrunk toward 1).")
                else
                    .("Predictors were NOT standardized, so the penalty acts on each coefficient in that predictor's own units and a predictor recorded on a large scale is penalized less. Coefficient and Odds Ratio are per 1 unit; the Importance column is the per-standard-deviation magnitude, so it can still be compared across predictors. Odds ratios remain penalized (shrunk toward 1)."))
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
                # Every case has the same predicted probability (nothing selected). That
                # is degenerate even when pROC returns a finite "best" threshold, and
                # DeLong's variance is then exactly 0: the interval printed as "0.500
                # (0.500-0.500)", a zero-width interval around a model that does not
                # discriminate, while the AUC = 1 end already said "CI not estimable".
                constant_score <- length(fit$selected) == 0 ||
                    length(unique(round(fit$probabilities, 12))) < 2
                if (constant_score) degenerate <- TRUE

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
                # Grade on the AUC as DISPLAYED (3 decimals). A raw 0.79965 printed as
                # 0.800 was graded "Acceptable" while the cell beside it said 0.800.
                # Separation detection above keeps the raw value.
                auc_shown <- if (is.na(auc_val)) NA_real_ else round(auc_val, 3)

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
                f1 <- if (2 * tp + fp + fn > 0) 2 * tp / (2 * tp + fp + fn) else NA_real_

                # Keyed by the rows .init() laid down (see .fixedRowTables()).
                rows <- list(
                    auc = list(
                        # DeLong's interval for AUC == 1 is always exactly 1.000 to
                        # 1.000. Printing that reads as an extraordinarily precise
                        # estimate when it is really the interval collapsing.
                        if (is.na(auc_val)) .("Not available")
                        else if (perfect || constant_score) sprintf(.("%.3f (CI not estimable)"), auc_val)
                        else sprintf("%.3f (%.3f-%.3f)", auc_val, auc_ci_lower, auc_ci_upper),
                        if (is.na(auc_val)) .("AUC could not be computed")
                        else if (perfect) .("Perfect in-sample separation - see note")
                        # Hosmer, Lemeshow & Sturdivant (2013) tiers; footnoted as a convention.
                        else if (auc_shown >= 0.9) .("Outstanding") else if (auc_shown >= 0.8) .("Excellent")
                        else if (auc_shown >= 0.7) .("Acceptable")
                        # "Poor" graded 0.68 and 0.41 identically. An AUC below 0.5
                        # is not merely weak: the model ranks cases the wrong way
                        # round, and the reader must be told which of the two it is.
                        # 0.500 itself is the book's "no discrimination", not "poor".
                        else if (auc_shown > 0.5) .("Poor")
                        else if (auc_shown == 0.5) .("No discrimination")
                        else .("Worse than chance - ranking is inverted")
                    ),
                    # .fmtNum, not sprintf: precision and f1 are NA_real_ when the
                    # model predicts no positives, and sprintf would print "NA".
                    # Not "Youden index" when the score is constant: the 0.500 is then the
                    # default the degenerate_note describes, not a maximised Youden J (LR-03).
                    threshold = list(private$.fmtNum(optimal_threshold),
                                     if (degenerate && optimal_threshold == 0.5) .("Default (no discrimination)") else .("Youden index")),
                    accuracy = list(private$.fmtNum(accuracy), ""),
                    sensitivity = list(private$.fmtNum(sensitivity), ""),
                    specificity = list(private$.fmtNum(specificity), ""),
                    precision = list(private$.fmtNum(precision), ""),
                    f1 = list(private$.fmtNum(f1), ""),
                    brier = list(
                        private$.fmtNum(brier, 4),
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
                                # Brier skill score, graded at the precision it is printed.
                                # `bss > 0` on the raw value let a model predicting exactly
                                # the base rate (skill +1e-16) read "Marginal (0% better)".
                                # No grade word: the old Good (>= 25%)/Marginal cut had no source.
                                bss_pct <- round(100 * (1 - brier / null_brier))
                                if (bss_pct > 0) sprintf(.("%.0f%% better than predicting the base rate (Brier skill score)"), bss_pct)
                                else .("No better than predicting the base rate")
                            }
                        }
                    )
                )

                for (key in names(rows)) {
                    table$setRow(rowKey = key, values = list(
                        value = rows[[key]][[1]], interpretation = rows[[key]][[2]]
                    ))
                }

                # DeLong's interval treats these fitted probabilities as a fixed marker.
                # They were fitted, and their variables and lambda selected, on the same
                # rows: in simulation (n = 100) the interval covered the model's
                # out-of-sample AUC in 83% of datasets, and with no signal at all it
                # excluded 0.5 in 8.5% of them. Shown only when there is an interval to
                # qualify (a constant score and AUC 1 have none; both say so in the cell).
                table$setNote("auc_ci_note",
                    if (!is.na(auc_val) && !perfect && !constant_score && !is.na(auc_ci_lower))
                        .("The interval beside the apparent AUC is DeLong's 95% CI computed on these fitted probabilities as if they were a fixed, pre-specified marker. It does not account for fitting the coefficients or for selecting the variables and lambda on the same patients, so it is centered on an optimistic estimate and can under-cover the model's discrimination in new patients. Enable Bootstrap Internal Validation for an optimism-corrected AUC.")
                    else NULL)

                table$setNote("auc_band_note", if (!is.na(auc_val) && !perfect)
                    .("AUC bands follow the convention of Hosmer, Lemeshow and Sturdivant (2013, p. 177): 0.5 no discrimination, above 0.5 and below 0.7 poor, 0.7 to 0.8 acceptable, 0.8 to 0.9 excellent, 0.9 and above outstanding. They are graded on the AUC as displayed.")
                    else NULL)

                # The probabilities themselves are penalized. Shrinkage pulls every
                # prediction toward the overall event rate; in simulation (n = 150, 3 of 8
                # real predictors) the 1SE model's external calibration slope was 2.2 -
                # a 70% risk reads as about 35% - against 1.2 for Minimum CV Error. They
                # rank patients correctly but are not absolute risks until the
                # calibration slope says so, and the saved column carries no caveat.
                table$setNote("prob_note", if (!constant_score)
                    .("Predicted probabilities, including the saved Predicted Probability column, come from the penalized model: the penalty pulls them toward the overall event rate, most strongly with the 1SE rule, so high risks tend to be under-stated and low risks over-stated. They rank patients by the model, but check the corrected calibration slope (Bootstrap Internal Validation) before reading them as absolute risks.")
                    else NULL)

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
                if (degenerate && constant_score) {
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
                            .("Apparent AUC = %1$.3f with N = %2$d: the model separates the two classes completely on the data it was fitted to. This is an in-sample artefact far more often than a real effect, the reported confidence interval collapses to a point rather than showing precision, and the coefficients are unstable. Enable bootstrap validation and validate externally before drawing any conclusion."),
                            auc_val, data$n)
                    )
                }

                if (!is.na(auc_val) && !perfect && auc_shown > 0.95 && data$n < 100) {
                    table$setNote(
                        "overfit_warning",
                        .("Warning: Very high apparent AUC with small sample size suggests possible overfitting. Enable bootstrap validation to assess optimism.")
                    )
                    private$.addNotice(
                        "STRONG_WARNING", .("Possible Overfitting"),
                        sprintf(
                            .("Apparent AUC = %1$.3f with N = %2$d: likely overfitted. Enable bootstrap validation for corrected estimate."),
                            auc_val, data$n
                        )
                    )
                }

                # Discrimination below the clinically usable threshold is a
                # STRONG_WARNING, not a plain WARNING, and an AUC below chance is an
                # ERROR: a 0.41 model used to carry exactly the same yellow weight as
                # a merely weak 0.68 one, with nothing saying the ranking is inverted.
                if (!is.na(auc_val) && auc_shown < 0.5) {
                    private$.addNotice(
                        "ERROR", .("Discrimination Worse Than Chance"),
                        sprintf(
                            .("AUC = %.3f is BELOW 0.500, so the model ranks cases the wrong way round: patients it scores high are less likely to have the event than patients it scores low. Do not read the odds ratios as they stand. Check that the event level is the one you intended, then treat these predictors as carrying no usable signal at this sample size."),
                            auc_val
                        )
                    )
                } else if (!is.na(auc_val) && !constant_score && auc_shown < 0.7) {
                    # A constant score already carries "Model Does Not Discriminate".
                    private$.addNotice(
                        "STRONG_WARNING", .("Poor Discrimination"),
                        sprintf(
                            .("AUC = %.3f is in the poor band (below 0.7) of the Hosmer-Lemeshow convention. Consider adding more informative predictors or using a different model."),
                            auc_val
                        )
                    )
                }

                # Precision is NA (a blank cell), not zero, when the
                # model makes no positive predictions. Say so once rather than
                # leaving two unexplained empty cells.
                if (is.na(precision)) {
                    table$setNote(
                        "precision_undefined",
                        .("Precision (PPV) is left blank because this model predicts no positive cases at the chosen threshold. F1 is computed as 2TP/(2TP+FP+FN), which is zero when events are present but none are predicted.")
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
                    # Never reached (selected terms are columns of X); no contrast, no contribution.
                    if (!(var_col %in% colnames(data$X))) { out[i] <- 0; next }
                    v <- data$X[, var_col]; v <- v[!is.na(v)]
                    ci <- cuts[[i]]
                    delta <- if (isTRUE(ci$binary)) {
                        uv <- unique(v); max(uv) - min(uv)
                    } else {
                        hi <- mean(v[v > ci$cut]); lo <- mean(v[v <= ci$cut])
                        hi - lo
                    }
                    # .scoreCuts() guarantees cases on both sides of every cut, so this is
                    # never reached. It used to fall back to a contrast of 1 (one SD): a
                    # manual cut above every value then printed points and an odds ratio
                    # for a criterion nobody meets. No contrast means no contribution.
                    if (!is.finite(delta)) delta <- 0
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
                # reach here; Smallest-unit then divides by it, the ratio exceeds
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

                # Points that round to 0 are raised to +/-1 so every selected predictor
                # counts. Recorded (attr "promoted") so the table can disclose it.
                promoted <- rep(FALSE, length(coefs))
                if (method == "beta10") {
                    # Beta10 (the "Beta10/integer" method of Mehta et al. 2016): multiply
                    # each coefficient by a FIXED factor of 10 and round. This preserves absolute
                    # coefficient magnitude and is deliberately distinct from the
                    # max-scaled method below. (A previous version
                    # renormalized the largest |coef| to max_points, which made Beta10
                    # algebraically identical to Max-scaled and broke "Compare All
                    # Methods".) max_points is intentionally NOT used here.
                    pts <- clamp(coefs * 10)
                    # Ensure every selected (non-zero) predictor contributes >= 1 point
                    promoted <- pts == 0 & coefs != 0
                    pts[promoted] <- signs[promoted]
                } else if (method == "smallest") {
                    # One point per smallest MEANINGFUL contribution (see NEGLIGIBLE
                    # above): a Sullivan-type scale with constant B = that contribution
                    # (Sullivan et al. 2004). Labelled "Schneeweiss" until 2026-10-12,
                    # which it is not: Schneeweiss et al. 2003 add one point per 0.3
                    # increase in ln(OR).
                    min_abs <- if (any(usable)) min(abs_coefs[usable]) else scale_ref
                    if (!is.finite(min_abs) || min_abs <= 0) min_abs <- 1
                    pts <- clamp(coefs / min_abs)
                    promoted <- pts == 0 & coefs != 0
                    pts[promoted] <- signs[promoted]
                } else {
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
                    promoted <- pts == 0 & coefs != 0
                    pts[promoted] <- signs[promoted]
                }

                out <- as.integer(pts)
                attr(out, "promoted") <- promoted
                out
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

            # ── Format a metric into a `type: text` cell ────────────────────
            # sprintf("%.3f", NA_real_) returns the literal string "NA", which in a
            # text column reads as a computation failure rather than the blank cell
            # jamovi renders for a genuinely undefined quantity. Precision is
            # undefined when the model makes no positive predictions, so every
            # metric goes through this.
            .fmtNum = function(x, digits = 3) {
                if (length(x) != 1 || !is.finite(x)) return("")
                sprintf(paste0("%.", digits, "f"), x)
            },

            # Per-sample-SD magnitudes make the zero rule invariant to measurement units.
            .fitColSDs = function(fit_X) {
                sds <- apply(fit_X, 2, stats::sd)
                sds[!is.finite(sds) | sds == 0] <- 1
                sds
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
                if (isTRUE(attr(fit_obj, "null_path"))) {
                    cf[-1, 1] <- 0
                    cf[1, 1] <- attr(fit_obj, "null_intercept")
                }
                b <- cf[-1, 1]
                keep <- is.finite(b)
                if (alpha_val > 0 || isTRUE(attr(fit_obj, "null_path"))) {
                    # Threshold against the matrix the model was FITTED on. Deriving
                    # it from the matrix being SCORED made the two bootstrap passes
                    # zero different coefficients of the SAME model (column SDs
                    # differ between a resample and the original), which is exactly
                    # the "two estimators in one table" defect this helper exists to
                    # prevent. Which coefficients are zero is a property of the fit.
                    sds <- private$.fitColSDs(fit_X)
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
            .cutAdjusted = character(0),

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
                    if (!isTRUE(self$options$scoringSystem) &&
                        length(private$.parseCutPoints(private$.opt("scoreCutPoints", ""))) == 0)
                        return(.("the cut points you supply (or the sample median when none is supplied)"))
                    if (length(private$.cutFellback) > 0 && length(private$.cutAdjusted) > 0)
                        return(.("the cut points you supplied, and the sample median where none was supplied or a supplied cut left all cases on one side"))
                    if (length(private$.cutFellback) > 0)
                        return(.("the cut points you supplied, and the sample median for the predictors you did not give one for"))
                    if (length(private$.cutAdjusted) > 0)
                        return(.("the cut points you supplied, and the sample median where a supplied cut left all cases on one side"))
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
            .sourceNameNote = function(data) {
                if (is.null(data) || !isTRUE(data$has_aliases)) return(NULL)
                mapping <- vapply(names(data$column_source), function(nm) {
                    if (isTRUE(data$column_factor[[nm]]))
                        sprintf(.("%1$s denotes %2$s = %3$s"), nm,
                                data$column_source[[nm]], data$column_level[[nm]])
                    else sprintf(.("%1$s denotes numeric predictor %2$s"), nm,
                                 data$column_source[[nm]])
                }, character(1))
                sprintf(.("Model-term names overlap, so unique labels are used: %s. Manual cut points use the original predictor names."),
                        .rocLabel(paste(mapping, collapse = "; ")))
            },

            .scoreCuts = function(data, variables) {
                method <- private$.opt("scoreCutMethod", "median")
                manual <- private$.parseCutPoints(private$.opt("scoreCutPoints", ""))
                fellback <- character(0)
                adjusted <- character(0)
                rounded <- character(0)

                out <- lapply(variables, function(var_col) {
                    if (!(var_col %in% colnames(data$X)))
                        return(list(binary = FALSE, cut = NA_real_, cut_raw = NA_real_))
                    v <- data$X[, var_col]
                    v <- v[!is.na(v)]

                    ctr <- if (!is.null(data$X_center) && var_col %in% names(data$X_center))
                        data$X_center[[var_col]] else 0
                    sdv <- if (!is.null(data$X_sd) && var_col %in% names(data$X_sd))
                        data$X_sd[[var_col]] else 1
                    if (!is.finite(sdv) || sdv == 0) sdv <- 1

                    if (length(unique(v)) == 2) {
                        # Two distinct values is NOT proof of a present/absent factor.
                        # model.matrix names a factor's dummy <variable><level>, so a
                        # design-matrix column whose name IS one of the source
                        # predictors came from a numeric measurement - ki67_pct or
                        # age_years that happens to take two values in this cohort.
                        # Printing "present" for those leaves the clinician with no
                        # threshold to apply to a new patient, which is exactly what
                        # .scoreCriteria exists to prevent. Carry the scoring value on
                        # the ORIGINAL scale so the criterion can state it.
                        dummy <- isTRUE(data$column_factor[[var_col]])
                        present_raw <- max(v) * sdv + ctr
                        return(list(binary = TRUE, dummy = dummy, cut = NA_real_,
                                    cut_raw = present_raw))
                    }

                    cut_z <- NA_real_
                    source_name <- data$column_source[[var_col]]
                    if (identical(method, "manual") && source_name %in% names(manual)) {
                        # entered on the original scale -> convert to the z-scale
                        cut_z <- (manual[[source_name]] - ctr) / sdv
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
                    # A cut with no case on one side separates nobody: a manual cut
                    # outside the observed range, or a median/quartile that equals the
                    # maximum because of ties at the top. Points derived from it rest
                    # on no contrast. Move it inside the data - the median, or just
                    # below the highest value when ties make the median the maximum
                    # (>= 3 distinct values here, so that always splits) - and record it.
                    splits <- function(cz) any(v > cz) && any(v <= cz)
                    typed <- identical(method, "manual") && source_name %in% names(manual)
                    if (!splits(cut_z)) {
                        # name the cut that failed, so a unit slip (200 for 20%) is visible
                        adjusted <<- c(adjusted, sprintf("%s at %s", var_col,
                            base::format(signif(cut_z * sdv + ctr, 6), trim = TRUE)))
                        cut_z <- stats::median(v)
                        if (!splits(cut_z)) {
                            u <- sort(unique(v))
                            cut_z <- u[length(u) - 1L]
                        }
                        typed <- FALSE
                    }
                    # A cut the user typed stays as typed; a data-derived one is replaced by
                    # the shortest decimal that awards exactly the same cases (X is on the raw
                    # scale: ctr = 0, sdv = 1), so "> 61.7" is printed instead of
                    # "> 61.6654806267845" and is also the rule applied.
                    if (!typed) {
                        nice <- private$.niceCut(v, cut_z)
                        if (nice != cut_z)
                            rounded <<- c(rounded, sprintf("%s %s", var_col, private$.cutText(nice)))
                        cut_z <- nice
                    }
                    list(binary = FALSE, cut = cut_z, cut_raw = cut_z * sdv + ctr)
                })
                names(out) <- variables
                attr(out, "fellback") <- fellback
                attr(out, "adjusted") <- adjusted
                attr(out, "rounded") <- rounded
                # Typed entries that cut nothing: malformed, or naming no explanatory variable
                # (a name containing ',' or ';' is split by the parser and lands here).
                if (identical(method, "manual"))
                    attr(out, "manual_ignored") <- c(attr(manual, "malformed"),
                        setdiff(names(manual), self$options$explanatory))
                out
            },

            # The decimal with the fewest places that splits v exactly as `cut` does (v > cut is
            # awarded): any value in [largest v <= cut, smallest v > cut) awards the same cases.
            # At each number of decimals k the grid point nearest `cut` or one of its two
            # neighbours is in that interval whenever any grid point is, and the comparison is
            # made on the doubles that will be applied. No candidate by 15 decimals: keep `cut`.
            # ponytail: a cut that needs more than 15 decimals (|values| < 1e-15) keeps its exact
            # value; widen k with the magnitude if a predictor is ever recorded on such a scale.
            .niceCut = function(v, cut) {
                lo <- max(v[v <= cut])
                hi <- min(v[v > cut])
                if (!is.finite(lo) || !is.finite(hi)) return(cut)
                for (k in 0:15) {
                    cand <- (round(cut * 10^k) + (-1:1)) / 10^k
                    cand <- cand[cand >= lo & cand < hi]
                    if (length(cand)) return(cand[which.min(abs(cand - cut))])
                }
                cut
            },

            # Parse "ki67=20, age=65" (also accepts ';' and newlines) into a named
            # numeric vector. Unparseable entries are dropped rather than guessed, and
            # listed in attr "malformed" so the table can say they were not used.
            .parseCutPoints = function(txt) {
                if (is.null(txt) || !nzchar(trimws(txt))) return(stats::setNames(numeric(0), character(0)))
                parts <- unlist(strsplit(txt, "[,;\n]+"))
                parts <- trimws(parts[nzchar(trimws(parts))])
                nm <- character(0); vals <- numeric(0); bad <- character(0)
                for (pt in parts) {
                    kv <- strsplit(pt, "=", fixed = TRUE)[[1]]
                    key <- trimws(kv[1])
                    val <- if (length(kv) == 2) suppressWarnings(as.numeric(trimws(kv[2]))) else NA_real_
                    if (!nzchar(key) || !is.finite(val)) { bad <- c(bad, pt); next }
                    nm <- c(nm, key); vals <- c(vals, val)
                }
                out <- stats::setNames(vals, nm)
                if (length(bad)) attr(out, "malformed") <- bad
                out
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
                    if (isTRUE(ci$binary)) {
                        # A factor dummy is genuinely present/absent; a two-valued
                        # NUMERIC needs the value that earns the points printed,
                        # because that is the rule .computeTotalScores applies.
                        if (isTRUE(ci$dummy) || !is.finite(ci$cut_raw)) return(.("present"))
                        return(sprintf(.("= %s"), private$.cutText(ci$cut_raw)))
                    }
                    sprintf(.("> %s"), private$.cutText(ci$cut_raw))
                }, character(1), USE.NAMES = FALSE)
            },

            # The printed cut must be the applied cut, so it cannot be rounded (a value
            # between "> 0.11" and the real 0.1097 would be scored against the rule shown).
            # %.17g guaranteed that but printed a typed 19.65 as "19.649999999999999".
            # The shortest decimal that reads back as the same double does both.
            .cutText = function(x) {
                for (d in 15:16) {
                    s <- formatC(x, digits = d, format = "g")
                    if (as.numeric(s) == x) return(trimws(s))
                }
                trimws(formatC(x, digits = 17, format = "g"))
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
                # NA, not 0. Undefined is not "perfectly bad": .populatePerformance
                # already gets this right for the same two quantities, and the
                # else-0 branches printed 0.000 in the Scoring System Performance
                # table while the identical row of the Classification Performance
                # table directly above it was blank for the same condition.
                prec <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
                f1 <- if (2 * tp + fp + fn > 0) 2 * tp / (2 * tp + fp + fn) else NA_real_

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
                table$setNote("source_names", private$.sourceNameNote(data))
                perf_table <- self$results$scoringPerformance

                # Nothing to score. Returning silently left up to four VISIBLE and
                # completely EMPTY tables on screen (Scoring System, Scoring System
                # Performance, Score Lookup and, in compare mode,
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
                if (is.null(method)) method <- "maxscaled"
                max_points <- self$options$scoringMaxPoints

                vars <- fit$selected
                ord <- if (!is.null(data)) data$ordinal_vars else character(0)
                table$setNote("ordinal_note", if (length(ord) > 0) sprintf(
                    .("Ordinal predictor(s) %s are coded with one indicator per level, each compared with that predictor's lowest level; they are not modeled as a linear trend."),
                    .rocLabel(paste(ord, collapse = ", "))) else NULL)
                ref_src <- if (!is.null(data$factor_ref) && length(vars) > 0)
                    unique(data$column_source[vars][data$column_factor[vars]]) else character(0)
                table$setNote("reference_note", if (length(ref_src) > 0) sprintf(
                    .("Each factor term is compared with its predictor's reference (first) level: %s. Points for a factor level apply relative to that reference level (0 points)."),
                    .rocLabel(paste(sprintf("%s = %s", ref_src, data$factor_ref[ref_src]), collapse = "; "))) else NULL)
                # Resolve the cut points ONCE and pass them to every consumer, so the
                # rule printed in the criterion column, the contrast the points are
                # derived from, and the cut actually applied when scoring cannot
                # drift apart.
                cuts <- private$.scoreCuts(data, vars)
                # Record which predictors fell back so every sentence built after
                # this point (here, and in the methodology notes) can say so.
                fb <- attr(cuts, "fellback")
                private$.cutFellback <- if (is.null(fb)) character(0) else as.character(fb)
                adj_attr <- attr(cuts, "adjusted")
                private$.cutAdjusted <- if (is.null(adj_attr)) character(0) else as.character(adj_attr)

                # Points are derived from the log-odds contribution of MEETING each
                # criterion, so they are on the same contrast the score applies and
                # are comparable between binary and continuous predictors.
                coefs <- private$.scoreContributions(data, vars, fit$selected_coefs, cuts)

                # Compute points by all three methods
                pts_beta10 <- private$.computePoints(coefs, "beta10", max_points)
                pts_smallest <- private$.computePoints(coefs, "smallest", max_points)
                pts_maxscaled <- private$.computePoints(coefs, "maxscaled", max_points)

                # Select primary method's points. Max-scaled is the default and the primary
                # in compare mode: LASSO leaves weak terms near zero, so dividing by the
                # smallest contribution gave the bundled PanNEN example a -23 to 120 score.
                pts_primary <- switch(method,
                    "beta10" = pts_beta10,
                    "smallest" = pts_smallest,
                    pts_maxscaled
                )

                # Build score data
                score_data <- data.frame(
                    variable = vars,
                    coefficient = coefs,
                    oddsRatio = exp(coefs),
                    criterion = private$.scoreCriteria(data, vars, cuts),
                    direction = ifelse(coefs > 0, .("Positive (+)"), .("Negative (-)")),
                    points_beta10 = pts_beta10,
                    points_smallest = pts_smallest,
                    points_maxscaled = pts_maxscaled,
                    points = pts_primary,
                    stringsAsFactors = FALSE
                )

                # Sort by absolute points descending
                score_data <- score_data[order(-abs(score_data$points)), ]

                for (i in seq_len(nrow(score_data))) {
                    table$addRow(rowKey = i, values = list(
                        variable = .rocLabel(score_data$variable[i], note = FALSE),
                        oddsRatio = score_data$oddsRatio[i],
                        criterion = score_data$criterion[i],
                        direction = score_data$direction[i],
                        points_beta10 = score_data$points_beta10[i],
                        points_smallest = score_data$points_smallest[i],
                        points_maxscaled = score_data$points_maxscaled[i],
                        points = score_data$points[i]
                    ))
                }

                # Add method reference note
                method_refs <- list(
                    beta10 = .("Beta10 method: each contribution x 10, rounded (Mehta et al. J Clin Epidemiol 2016)"),
                    smallest = .("Smallest-unit method: each contribution divided by the smallest, rounded; a Sullivan-type scale with one point per smallest contribution (Sullivan et al. Stat Med 2004)"),
                    maxscaled = .("Max-scaled method (strongest predictor = maximum points)"),
                    compare = .("All three methods are shown for comparison; Points (Selected), the score performance and the lookup table use Max-scaled")
                )
                table$setNote("method", method_refs[[method]])
                # Which predictors were raised from 0 to +/-1 point, for every method shown.
                shown <- if (method == "compare") c("beta10", "smallest", "maxscaled") else method
                pts_by <- list(beta10 = pts_beta10, smallest = pts_smallest, maxscaled = pts_maxscaled)
                lbl_by <- c(beta10 = "Beta10", smallest = .("Smallest-unit"), maxscaled = .("Max-scaled"))
                promo <- unlist(lapply(shown, function(m) {
                    v <- vars[attr(pts_by[[m]], "promoted")]
                    if (length(v)) sprintf("%s (%s)", paste(v, collapse = ", "), lbl_by[[m]])
                }))
                table$setNote("promoted", if (length(promo)) sprintf(
                    .("Raised from 0 to 1 point (of the predictor's sign) so that every selected predictor counts: %s. Their weight is overstated relative to the other predictors."),
                    .rocLabel(paste(promo, collapse = "; "))) else NULL)
                fellback <- attr(cuts, "fellback")
                if (identical(private$.opt("scoreCutMethod", "median"), "manual") &&
                    !is.null(fellback) && length(fellback) > 0) {
                    # .rocLabel: setNote honours a small HTML allow-list, so a column
                    # literally named "<b>Ki-67" turned the rest of the footnote bold
                    # and dropped its own name out of it - from the one note whose
                    # whole job is to say WHICH predictor fell back. (htmlEscape is no
                    # remedy: the client shows its entities literally, so "A&B" would read as its entity.)
                    table$setNote("manual_fallback", sprintf(
                        .("No manual cut point was supplied for: %s. These fell back to the sample median. Enter them as 'variable=value' pairs (for example 'ki67=20, age=65') to use established clinical thresholds."),
                        .rocLabel(paste(fellback, collapse = ", "))))
                }
                # Without this a typed cut that was not used vanished, and the fallback note
                # above then said none was supplied for that predictor.
                ignored <- attr(cuts, "manual_ignored")
                table$setNote("manual_ignored", if (length(ignored)) sprintf(
                    .("These manual cut point entries were not used: %s. Each must be 'variable=value' using an explanatory variable's name as written; a name containing a comma, semicolon or '=' cannot be given a manual cut."),
                    .rocLabel(paste(ignored, collapse = "; "))) else NULL)
                adj <- attr(cuts, "adjusted")
                table$setNote("cut_adjusted", if (length(adj)) sprintf(
                    .("The cut point for %s left no case on one side (nobody above it, or everybody), so it could separate no one and its points would rest on no contrast. It was moved to the sample median, or to just below the highest value when ties make the median the maximum. The criterion column shows the cut actually applied."),
                    .rocLabel(paste(adj, collapse = ", "))) else NULL)
                rnd <- attr(cuts, "rounded")
                table$setNote("cut_rounded", if (length(rnd)) sprintf(
                    .("Shown and applied as the shortest number that divides these cases exactly as the computed cut does, so the same patients earn the points: %s."),
                    .rocLabel(paste(rnd, collapse = ", "))) else NULL)
                cut_label <- private$.scoreCutLabel()
                table$setNote("criterion_note", sprintf(.("Award a factor's points when the patient meets its criterion. Continuous predictors are cut at %s. The Odds Ratio column is the penalized odds ratio for MEETING that criterion (present vs absent, or above vs below the cut), which is the contrast the points represent - so points and odds ratios are on the same footing here. A cut derived from this dataset (median, mean, tertile or quartile) is not an externally established clinical threshold and will differ in another cohort; manual cut points fix the scoring thresholds but do not establish transportability. The score has not been validated outside these data."), cut_label))

                # A points system is only useful if a clinician can add it up at the
                # bedside. Smallest-unit divides by the SMALLEST contribution, so one
                # near-zero predictor inflates every other weight; Beta10 multiplies a
                # strong effect by 10. The limit is 20 points for one factor, the largest
                # Maximum Points the Max-scaled method accepts (scoringMaxPoints max: 20).
                # It was 100, while the notice said a usable score stays under 20, so the
                # bundled example's -23 to 120 default score passed without a word.
                max_pt <- suppressWarnings(max(abs(pts_primary), na.rm = TRUE))
                if (is.finite(max_pt) && max_pt > 20) {
                    table$setNote("wide_scale", sprintf(
                        .("This point scale gives a single factor up to %d points, more than the 20 the Max-scaled method allows, which makes the total hard to add up at the bedside. Smallest-unit widens the scale when one selected predictor has a far smaller effect than the others; Beta10 does when a predictor's effect is large. Max-scaled caps the strongest predictor at the 'Maximum points per feature' you set."),
                        as.integer(max_pt)))
                    private$.addNotice(
                        "WARNING", .("Wide Scoring Scale"),
                        sprintf(
                            .("The generated scoring system gives a single factor %d points, more than the 20 the Max-scaled method allows. Switch to the Max-scaled method before using this score at the bedside."),
                            as.integer(max_pt)))
                }

                # Evaluate primary scoring system
                total_scores <- private$.computeTotalScores(data, vars, pts_primary, cuts)
                perf <- private$.evaluateScore(data$y, total_scores)

                # Keyed by the rows .init() laid down (see .fixedRowTables()).
                perf_values <- list(
                    # A fallback is required: without one an unmapped option value
                    # returns NULL and the Scoring method row is left blank.
                    method = switch(method,
                        "beta10" = "Beta10",
                        "smallest" = .("Smallest-unit"),
                        "maxscaled" = .("Max-scaled"),
                        "compare" = .("Max-scaled (primary)"),
                        as.character(method)
                    ),
                    # .fmtNum, not sprintf: an NA metric (AUC when pROC is missing,
                    # precision/F1 when the score makes no positive calls) printed as
                    # the literal string "NA" instead of an empty cell.
                    auc = private$.fmtNum(perf$auc),
                    cutoff = as.character(perf$cutoff),
                    accuracy = private$.fmtNum(perf$accuracy),
                    sensitivity = private$.fmtNum(perf$sensitivity),
                    specificity = private$.fmtNum(perf$specificity),
                    precision = private$.fmtNum(perf$precision),
                    f1 = private$.fmtNum(perf$f1),
                    mean_pos = private$.fmtNum(perf$mean_pos, 2),
                    mean_neg = private$.fmtNum(perf$mean_neg, 2),
                    range = sprintf("%d to %d", perf$range[1], perf$range[2])
                )

                for (key in names(perf_values)) {
                    perf_table$setRow(rowKey = key, values = list(value = perf_values[[key]]))
                }

                perf_table$setNote(
                    "dichotomization",
                    sprintf(.("Continuous predictors are scored by dichotomizing at %s: the full point block is awarded above the cut and nothing below it. The performance shown here reflects this simplified integer point system and may differ from the continuous penalized model in the Classification Performance table."), cut_label)
                )
                # The model's own table is labelled "AUC (apparent)" and carries an
                # optimism caveat; the SCORE's table said only "Score AUC" while
                # being doubly optimistic - the points come from a model fitted on
                # these data AND the cutoff is Youden-optimised on the same data.
                perf_table$setNote(
                    "apparent",
                    .("These figures are APPARENT (in-sample) and optimistic twice over: the points were derived from a model fitted to this dataset, and the score cutoff was chosen to maximize the Youden index on the same rows. They are not an estimate of how the score would perform on new patients. Enable bootstrap validation for an optimism-corrected estimate of the model, and validate any score externally before clinical use.")
                )

                # ── Degenerate score guard ─────────────────────────────────
                # .evaluateScore searches Youden over `total_scores >= cutoff` only.
                # At the LOWEST score every case is called positive, giving Youden
                # exactly 0; for a reversed or uninformative score every other cutoff
                # scores below that, so best_cutoff stays at the minimum. The table
                # then reads "Sensitivity 1.000, Specificity 0.000" - which looks like
                # an outstandingly sensitive score - the lookup table labels EVERY row
                # "High risk" (s >= min score is always true), and the Clinical
                # Interpretation panel still asserts that higher scores mean higher
                # risk. .populatePerformance guards the identical failure for the
                # model; the score had no guard at all.
                score_min <- suppressWarnings(min(total_scores, na.rm = TRUE))
                # Youden at the lowest cutoff is identically 0, so a score with any
                # discriminating cut point always beats it; best_cutoff == min means
                # nothing did. (A constant score lands here too, which is correct.)
                cutoff_collapsed <- is.finite(score_min) && isTRUE(perf$cutoff <= score_min)
                score_inverted <- !is.na(perf$auc) && round(perf$auc, 3) < 0.5  # as displayed
                if (cutoff_collapsed || score_inverted) {
                    # The note has to describe the rule the code ACTUALLY applied,
                    # in BOTH branches. The old fixed tail asserted "sensitivity
                    # 1.000 with specificity 0.000 ... an artefact of calling every
                    # patient positive" whichever branch fired. That is true only
                    # when the cut point collapsed to the minimum. A U-shaped score
                    # reaches score_inverted WITHOUT collapsing (e.g. AUC 0.400 with
                    # the Youden cut at the top of the range), and the table then
                    # shows sensitivity 0.400 with specificity 1.000 - the exact
                    # opposite of what the note claimed. Each branch now names its
                    # own failure and quotes the sensitivity/specificity on screen.
                    sens_txt <- private$.fmtNum(perf$sensitivity)
                    spec_txt <- private$.fmtNum(perf$specificity)
                    detail <- if (cutoff_collapsed)
                        sprintf(.("No cut point separated the two groups, so the search settled on the lowest possible total score and every patient is classified positive: the sensitivity of %1$s with a specificity of %2$s shown here is an artefact of that, not evidence of a sensitive score."),
                            sens_txt, spec_txt)
                    else
                        sprintf(.("The Youden search did find a cut point (%1$s and above, sensitivity %2$s, specificity %3$s), but the score points the wrong way, so calling the high scores high risk inverts the real risk."),
                            as.character(perf$cutoff), sens_txt, spec_txt)
                    # Headline and AUC sentence are separate msgids so the inverted
                    # case is not described as "does not discriminate" - it does
                    # discriminate, backwards.
                    headline <- if (cutoff_collapsed)
                        .("This scoring system does not discriminate.")
                    else
                        .("This scoring system runs backwards.")
                    auc_txt <- if (score_inverted)
                        sprintf(.("The score's AUC is %.3f, below 0.500: higher total scores are associated with FEWER events."), perf$auc)
                    else
                        NULL
                    reason <- paste(c(headline, auc_txt, detail), collapse = " ")
                    perf_table$setNote("score_degenerate", sprintf(
                        .("%s The risk groups in the Score Lookup table are not meaningful either. Do not publish or apply this score."),
                        reason))
                    self$results$lookupTable$setNote("score_degenerate", sprintf(
                        .("%s Every row's risk group below is therefore unreliable."),
                        reason))
                    private$.addNotice(
                        "STRONG_WARNING",
                        if (cutoff_collapsed)
                            .("Scoring System Does Not Discriminate")
                        else
                            .("Scoring System Runs Backwards"),
                        sprintf(
                            .("%s The Scoring System Performance figures and the High/Low risk labels in the lookup table are not measurements of a working score."),
                            reason)
                    )
                }

                # ── Method comparison (when compare mode selected) ──────────
                if (method == "compare") {
                    comp_table <- self$results$methodComparison

                    # Full model AUC for reference (single-source apparent AUC)
                    full_auc <- fit$apparent_auc

                    # Keyed by the rows .init() laid down with their method names and
                    # references (see .fixedRowTables()); only the numbers are filled here.
                    methods_list <- list(
                        beta10 = pts_beta10,
                        smallest = pts_smallest,
                        maxscaled = pts_maxscaled
                    )

                    for (key in names(methods_list)) {
                        scores_j <- private$.computeTotalScores(data, vars, methods_list[[key]], cuts)
                        perf_j <- private$.evaluateScore(data$y, scores_j)

                        info_loss <- if (!is.na(full_auc) && !is.na(perf_j$auc) && full_auc > 0) {
                            (1 - perf_j$auc / full_auc) * 100
                        } else {
                            NA
                        }

                        comp_table$setRow(rowKey = key, values = list(
                            auc = perf_j$auc,
                            accuracy = perf_j$accuracy,
                            info_loss = info_loss
                        ))
                    }

                    # Full model as reference row
                    comp_table$setRow(rowKey = "full", values = list(
                        auc = full_auc,
                        accuracy = NA,
                        info_loss = 0
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
                    .("Observed Event Rate is the share of cases with each total score that had the event, in this dataset (events / N): an in-sample proportion, not a model prediction, so it need not rise with the score, and scores based on few cases are unstable and can read as 0% or 100%. Validate the score-to-risk mapping on an independent cohort before clinical use.")
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
                # With one covariate the logistic slope is finite only when the classes
                # overlap on it. Under complete or quasi-complete separation glm stops at
                # an arbitrary large value: 11.9 apparent and 48.6 "corrected" on the
                # bundled PanNEN data, printed as if they were calibration slopes.
                if (max(lp[y == 0]) <= min(lp[y == 1]) || max(lp[y == 1]) <= min(lp[y == 0]))
                    return(structure(NA_real_, separated = TRUE))
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
                # The B cv.glmnet refits are by far the slowest step, and every option change
                # re-runs .run(), so toggling a plot or a text panel repeated all of them (26 s
                # at n = 1500, p = 30, B = 200). The summary lives in the table's state, which
                # jamovi restores only while no clearWith option has changed. The key also
                # covers what clearWith cannot see (a changed row filter or a data edit), so a
                # stale summary is recomputed, never shown. Rows and notes are rebuilt from it
                # on every run, because jamovi does not restore notes set in .run().
                # A hash, not column sums and index-weighted sums: an edit adding (1, -2, 1) to
                # three consecutive rows of one column preserved both and reused a stale summary.
                key <- list(
                    seed = self$options$random_seed, B = self$options$bootstrapN,
                    rule = self$options$lambda, alpha = fit$alpha, nfolds = fit$nfolds,
                    standardize = isTRUE(self$options$standardize), terms = colnames(data$X),
                    data = rlang::hash(cbind(data$y, data$X)))
                boot <- table$state
                if (is.null(boot) || !identical(boot$key, key)) {
                    boot <- private$.bootstrapSummary(data, fit)
                    boot$key <- key
                    table$setState(boot)
                }
                B <- boot$B
                n_ok <- boot$n_ok
                apparent_auc <- boot$apparent_auc
                apparent_brier <- boot$apparent_brier
                apparent_slope <- boot$apparent_slope
                mean_optimism_auc <- boot$mean_optimism_auc
                mean_optimism_brier <- boot$mean_optimism_brier
                mean_optimism_slope <- boot$mean_optimism_slope
                table$setNote("seed", .fmt(.("Random seed: {seed}"), seed = self$options$random_seed))

                table$setNote("slope_na", if (isTRUE(boot$separated))
                    .("The calibration slope is not estimable: the predicted probabilities separate the two outcome classes completely on these data, so the slope is infinite rather than a number. Treat the apparent performance as an artefact of separation.")
                    else if (is.na(apparent_slope))
                    .("The calibration slope is not estimable: every case received the same predicted probability, so there is no linear predictor to calibrate.")
                    else if (is.na(mean_optimism_slope))
                    .("No bootstrap replicate produced a calibration slope (each replicate model gave every case the same probability), so the apparent slope could not be corrected.")
                    else NULL)
                # A replicate whose model separates its own resample has no finite slope and
                # drops out of the slope optimism only, so say how many the correction rests on.
                table$setNote("slope_subset", if (!is.na(mean_optimism_slope) && boot$n_slope < n_ok)
                    sprintf(.("The calibration-slope optimism rests on %1$d of the %2$d completed replicates: in the others the replicate model separated the outcome classes or gave every case the same probability, so it had no finite slope. Separation is most likely in the replicates that overfit most, so this correction may be too small."),
                        boot$n_slope, n_ok)
                    else NULL)
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
                if (n_ok < B) {
                    table$setNote("boot_n", sprintf(
                        .("%1$d of %2$d bootstrap replicates completed; %3$d failed (typically a resample with too few events to fit) and were excluded. The optimism correction is based on the %4$d successful replicates."),
                        n_ok, B, B - n_ok, n_ok))
                }
                if (n_ok < 20) {
                    table$setNote("boot_few", .("Fewer than 20 bootstrap replicates succeeded. The optimism correction is unreliable at this number - increase the sample size or reduce the number of candidate predictors."))
                }

                # Keyed by the rows .init() laid down (see .fixedRowTables()).
                rows <- list(
                    auc = list(apparent_auc, mean_optimism_auc, corrected_auc),
                    brier = list(apparent_brier, mean_optimism_brier, corrected_brier),
                    slope = list(apparent_slope, mean_optimism_slope, corrected_slope)
                )

                for (key in names(rows)) {
                    table$setRow(rowKey = key, values = list(
                        apparent = rows[[key]][[1]],
                        optimism = rows[[key]][[2]],
                        corrected = rows[[key]][[3]]
                    ))
                }

                if (!is.na(mean_optimism_auc) && mean_optimism_auc > 0.05) {
                    table$setNote(
                        "optimism_warning",
                        sprintf(
                            .("Optimism = %1$.3f indicates overfitting. The corrected AUC (%2$.3f) removes the estimated optimism."),
                            mean_optimism_auc, corrected_auc
                        )
                    )
                }

                # Always, not only below AUC 0.7: in the 2026-10-21 simulation (n = 150, 8
                # candidates, lambda.min) the corrected AUC still exceeded the external AUC by
                # 0.0215 (MCSE 0.0068), so "corrected" must not read as "realistic" (LR-01).
                table$setNote(
                    "corrected_meaning",
                    .("Corrected values are an optimism-corrected internal validation (Harrell's bootstrap) on the same patients, not an external validation. With variable and lambda selection in a modest sample the correction removes only part of the optimism, so the corrected AUC can still be too high; confirm performance on independent data.")
                )

                # A corrected AUC below 0.5 is a legitimate outcome, not a glitch:
                # even when the final model selects nothing, bootstrap resamples
                # manufacture enough spurious signal that some of them DO select
                # variables, so the optimism is genuinely positive and subtracting
                # it takes the estimate below chance. Left uncorrected on the
                # screen it just looks broken, so say what it means.
                if (!is.na(corrected_auc) && round(corrected_auc, 3) < 0.5) {  # as displayed
                    table$setNote(
                        "below_chance",
                        sprintf(
                            .("The corrected AUC (%1$.3f) is below 0.500. This is a real result, not an error: the bootstrap replicates fit models that pick up chance associations, so the optimism (%2$.3f) exceeds what the model actually achieves. Read it as evidence that these predictors carry no usable signal at this sample size - the model performs no better than, and by this estimate slightly worse than, guessing."),
                            corrected_auc, mean_optimism_auc)
                    )
                }

                # Both directions. The note used to explain only a slope below 1, and its
                # "above 1 is expected for penalized models" read as reassurance for a
                # CORRECTED slope near 2 - which is what the 1SE rule typically produces:
                # in simulation the corrected slope (2.07) tracked the external one (2.22).
                table$setNote(
                    "calibration_note",
                    .("Calibration slope: 1.0 = ideal. A corrected slope below 1 means the predicted probabilities are too extreme (overfitted). A corrected slope above 1 means they are too moderate: the penalty pulled them too far toward the overall event rate, so high risks are under-stated and low risks over-stated. This is common with the 1SE rule, and the Minimum CV Error lambda shrinks less. The apparent slope is above 1 for any penalized model and is not itself a calibration check. These readings assume the model discriminates; when the corrected AUC is close to 0.5 the slope says little about calibration.")
                )

                # In simulation with NO true predictor (n = 150, 10 candidates, Minimum CV Error),
                # 4 of 12 datasets retained chance terms: corrected AUC 0.58-0.64 against an
                # external 0.50, and corrected slopes 1.05-2.32 - read by the note above as "too
                # moderate" - against an external slope of about 0. Harrell's bootstrap estimates
                # the AVERAGE optimism of the selection procedure, not this dataset's optimism.
                table$setNote("slope_low_auc",
                    if (!is.na(corrected_auc) && round(corrected_auc, 3) < 0.7 && is.finite(corrected_slope))
                        sprintf(.("The corrected AUC (%1$.3f) is in the poor band (below 0.7), so read the corrected calibration slope (%2$.3f) with caution: with this little discrimination it is unstable and may reflect chance associations rather than how far the penalty shrank the predictions. The bootstrap estimates the average optimism of the whole selection procedure, so when chance associations were selected in this dataset it can understate this model's optimism, and the corrected AUC may also be too high."),
                                corrected_auc, corrected_slope)
                    else NULL)
            },

            # One bootstrap run: apparent performance and the mean optimism per metric.
            .bootstrapSummary = function(data, fit) {
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
                            nfolds_boot <- min(fit$nfolds,
                                length(unique(idx[y_boot == 1])), length(unique(idx[y_boot == 0])))
                            if (nfolds_boot < 3) next
                            foldid_boot <- private$.stratifiedFolds(y_boot, nfolds_boot, groups = idx)
                            cv_boot <- private$.cvFit(X_boot, y_boot, alpha_val, foldid_boot)
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
                # A constant or separated final model has no estimable apparent slope.
                # Its replicates still produced an optimism, which sat in the row beside
                # two blank cells with nothing to correct.
                if (is.na(apparent_slope)) mean_optimism_slope <- NA_real_
                list(
                    B = B,
                    n_ok = sum(!is.na(optimism_auc) | !is.na(optimism_brier) | !is.na(optimism_slope)),
                    n_slope = sum(!is.na(optimism_slope)),
                    separated = isTRUE(attr(apparent_slope, "separated")),
                    apparent_auc = apparent_auc,
                    apparent_brier = apparent_brier,
                    apparent_slope = as.numeric(apparent_slope),
                    mean_optimism_auc = mean_optimism_auc,
                    mean_optimism_brier = mean_optimism_brier,
                    mean_optimism_slope = mean_optimism_slope
                )
            },

            # Which terms are non-zero at each lambda of the path (terms x lambdas), under the
            # same per-SD zero rule as the coefficient table (see ZERO_TOL in .fitLasso): an
            # exact `!= 0` counted coordinate-descent residues of 1e-16. Shared by the Variable
            # Importance panel and the CV plot's term counts, so both agree with the table.
            .pathKept = function(fit, X) {
                path <- as.matrix(stats::coef(fit$cv_fit$glmnet.fit))[-1, , drop = FALSE]
                # Ridge never writes a zero, so every finite coefficient counts.
                if (!(isTRUE(fit$alpha > 0) || isTRUE(attr(fit$cv_fit, "null_path"))))
                    return(is.finite(path))
                sds <- private$.fitColSDs(X)[rownames(path)]
                # .fitColSDs already sanitises its own values; this second pass catches the NA
                # a rowname that is not a column of X would produce. Not redundant.
                sds[!is.finite(sds) | sds == 0] <- 1
                abs(path) * sds > 1e-10
            },

            # ══════════════════════════════════════════════════════════════════
            # Variable importance
            # ══════════════════════════════════════════════════════════════════
            .populateVariableImportance = function(data, fit) {
                table <- self$results$variableImportance

                # Calculate inclusion proportion across lambda path
                all_coefs <- as.matrix(coef(fit$cv_fit$glmnet.fit))[-1, , drop = FALSE]
                inclusion_prop <- rowMeans(private$.pathKept(fit, data$X))
                # Per-SD magnitude, as in the Selected Variables table: without
                # standardization a raw per-unit maximum ranks predictors by their units.
                path_sd <- private$.fitColSDs(data$X)[rownames(all_coefs)]
                path_sd[!is.finite(path_sd)] <- 1
                max_abs <- apply(abs(all_coefs) * path_sd, 1, max)

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
                        variable = .rocLabel(imp_df$variable[i], note = FALSE),
                        importance_score = imp_df$importance_score[i],
                        selection_frequency = imp_df$selection_frequency[i],
                        stability_rank = imp_df$stability_rank[i]
                    ))
                }
                if (n_show < nrow(imp_df)) {
                    table$setNote("truncated", sprintf(
                        .("Showing the %1$d highest-ranked of %2$d model terms; the remaining terms have smaller maximum coefficients along the path."),
                        n_show, nrow(imp_df)))
                }

                table$setNote(
                    "apparent_note",
                    .("Path Inclusion Proportion is the fraction of the single cross-validated lambda path on which each predictor has a non-zero coefficient (apparent), NOT a bootstrap/resampling stability-selection frequency. Importance Score is the largest per-standard-deviation coefficient magnitude along that path, and Importance Rank orders predictors by it. Because Ridge (L1 weight = 0) never shrinks coefficients to zero, every predictor is retained at all lambdas and the proportion is trivially 1.0.")
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
                            model_type = sprintf(.("%s (penalized)"), private$.penaltyLabel()),
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
                                model_type = .("Logistic (retained terms)"),
                                n_variables = length(fit$selected),
                                auc = auc_sel,
                                aic = AIC(glm_sel),
                                brier = brier_sel
                            ))
                        },
                        error = function(e) {
                            table$setNote("refit_failed_sel", sprintf(
                                .("The unpenalized refit on the retained terms could not be fitted (%s), so its row is omitted."),
                                .rocLabel(conditionMessage(e))))
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
                            .rocLabel(conditionMessage(e))))
                    }
                )

                # Refer to the rows by NAME, not by index: the "LASSO-selected vars"
                # refit is skipped entirely when nothing was selected, so a note
                # saying "rows 2-3" pointed at a row that was not on screen.
                table$setNote(
                    "refit_note",
                    .("The 'Logistic' rows are unpenalized refits (shown for AIC comparability); their apparent AUC is typically higher than the penalized row because refitting removes shrinkage. All metrics are apparent (in-sample) - use Bootstrap Internal Validation for optimism-corrected discrimination.")
                )

                if (separation_detected) {
                    table$setNote(
                        "separation_note",
                        .("Warning: an unpenalized logistic refit (a 'Logistic' row) did not converge or produced fitted probabilities at 0 or 1, indicating complete or quasi-complete separation. Its apparent AUC (often ~1.0), AIC, and Brier score are unreliable and should not be interpreted - this is exactly the instability that LASSO penalization is designed to avoid.")
                    )
                    private$.addNotice(
                        "WARNING", .("Separation in Unpenalized Refit"),
                        .("An unpenalized logistic refit in the Model Comparison table showed complete/quasi-complete separation (non-convergence or fitted probabilities at 0/1). Its AUC/AIC are unreliable; prefer the penalized row.")
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
                        # not glmnet's nzero, which counts 1e-16 residues as terms
                        nzero      = as.integer(colSums(private$.pathKept(fit, data$X))),
                        penalty    = private$.penaltyLabel()
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
                    values <- fit$selected_coefs
                    if (isTRUE(self$options$standardize))
                        values <- values * private$.fitColSDs(data$X)[fit$selected]
                    ord <- order(abs(values))
                    coef_state <- list(
                        var_names    = as.character(fit$selected[ord]),
                        coef_values  = as.numeric(values[ord]),
                        standardized = isTRUE(self$options$standardize),
                        penalty      = private$.penaltyLabel()
                    )
                    self$results$coef_plot$setState(coef_state)
                } else if (self$options$coef_plot) {
                    self$results$coef_plot$setState(NULL)
                }

                # Store drawing coordinates only, not row-level outcomes or predictions.
                if (self$options$roc_plot) {
                    self$results$roc_plot$setState(private$.rocState(data$y, fit$probabilities))
                }
            },

            .rocState = function(y, probabilities) {
                roc <- pROC::roc(y, probabilities, quiet = TRUE, direction = "<", levels = c(0, 1))
                ci <- tryCatch(suppressWarnings(as.numeric(pROC::ci.auc(roc, method = "delong"))),
                               error = function(e) numeric(0))
                if (length(ci) != 3 || any(!is.finite(ci)) || ci[1] >= 1 || ci[3] <= ci[1])
                    ci <- numeric(0)
                list(schema = 2L, specificity = as.numeric(roc$specificities),
                     sensitivity = as.numeric(roc$sensitivities),
                     auc = as.numeric(pROC::auc(roc)), ci = ci)
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

                # Okabe-Ito hues. The previous red points / blue line / GREEN line
                # is the classic deuteranopia confusion: ~8% of men could not tell
                # the lambda.min line from the lambda.1se line, and the subtitle
                # naming the colours was the only key. Blue and orange separate for
                # every common form of colour deficiency, and the different dash
                # patterns carry the distinction even in greyscale.
                palette <- jmvcore::colorPalette(3, theme$palette)
                col_min <- palette[1]
                col_1se <- palette[2]
                p <- ggplot2::ggplot(cv_data, ggplot2::aes(x = log(lambda), y = cvm)) +
                    ggplot2::geom_point(color = palette[3], size = 0.8) +
                    ggplot2::geom_errorbar(ggplot2::aes(ymin = cvlo, ymax = cvup),
                        color = "darkgrey", width = 0.02
                    ) +
                    ggplot2::geom_vline(
                        xintercept = log(state$lambda_min),
                        linetype = "dashed", color = col_min
                    ) +
                    ggplot2::geom_vline(
                        xintercept = log(state$lambda_1se),
                        linetype = "dotted", color = col_1se
                    ) +
                    ggplot2::labs(
                        title = sprintf(.("Cross-Validation for %s Logistic Regression"),
                            if (is.null(state$penalty)) .("Penalized") else state$penalty),
                        # The subtitle is the plot's only key, so it must name what
                        # is actually drawn - it still said "Green: lambda.1se".
                        subtitle = .("Dashed: lambda.min; dotted: lambda.1se"),
                        x = .("Log Lambda"),
                        y = .("Binomial Deviance")
                    ) +
                    ggtheme
                # Model size along the path on the top axis, as plot.cv.glmnet draws it, so the
                # two lambda lines can be read as "this many terms". After ggtheme, which
                # replaces scales added before it.
                if (length(state$nzero) == length(state$lambda)) {
                    idx <- unique(round(seq(1, length(state$lambda), length.out = min(10, length(state$lambda)))))
                    p <- p + ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(
                        name = .("Non-zero terms"), breaks = log(state$lambda[idx]),
                        labels = state$nzero[idx]))
                }

                print(p)
                TRUE
            },
            .coefPlot = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state) || length(state$var_names) == 0) {
                    return(FALSE)
                }

                # The legend TITLE was translated while its two entries were raw
                # English literals, so a Turkish session got "Yon: Positive/Negative".
                lbl_pos <- .("Positive")
                lbl_neg <- .("Negative")
                df <- data.frame(
                    variable    = factor(state$var_names, levels = state$var_names),
                    coefficient = state$coef_values,
                    direction   = ifelse(state$coef_values > 0, lbl_pos, lbl_neg),
                    stringsAsFactors = FALSE
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
                    ggplot2::labs(
                        title = sprintf(.("%s Logistic Regression Coefficients"),
                            if (is.null(state$penalty)) .("Penalized") else state$penalty),
                        subtitle = sub_lab,
                        x = "", y = y_lab, fill = .("Direction")
                    ) +
                    ggtheme +
                    # AFTER ggtheme, not before. A jamovi ggtheme is a complete theme
                    # PLUS its own discrete fill/colour scales, so anything added
                    # ahead of it is silently replaced (and ggplot2 prints "Scale for
                    # fill is already present" on every render). The bars are sorted
                    # by |coefficient| with no sign on the axis, so colour is the ONLY
                    # cue for direction - losing this scale left the legend saying
                    # Positive/Negative beside arbitrary palette colours.
                    ggplot2::scale_fill_manual(
                        values = stats::setNames(jmvcore::colorPalette(2, theme$palette),
                                                 c(lbl_pos, lbl_neg)))
                print(p)
                TRUE
            },
            .rocPlot = function(image, ggtheme, theme, ...) {
                state <- image$state
                if (is.null(state)) {
                    return(FALSE)
                }
                # Reopen pre-1.0.84 files without requiring the original dataset.
                if (is.null(state$schema) && !is.null(state$y) && !is.null(state$probabilities))
                    state <- private$.rocState(state$y, state$probabilities)
                if (!identical(as.integer(state$schema), 2L) ||
                    length(state$specificity) < 2 ||
                    length(state$specificity) != length(state$sensitivity)) return(FALSE)
                auc_val <- sprintf("%.3f", state$auc)
                ci_obj <- if (length(state$ci) == 3) state$ci else NULL

                # This is a BASE-graphics renderer, so `ggtheme` cannot reach it and
                # the theme has to be applied through par(). Without this the axes,
                # tick labels, axis titles, the main title and the legend text all
                # drew in R's default black onto the dark background jamovi sets from
                # the user's theme - an invisible scale around a visible blue curve.
                # theme is read through tryCatch so a missing promise cannot force an
                # error here (same guard as benford.b.R:1346).
                fg <- tryCatch(theme$color[[1]], error = function(e) NULL)
                if (is.null(fg) || length(fg) != 1 || is.na(fg) || !nzchar(fg))
                    fg <- par("fg")
                op <- par(fg = fg, col = fg, col.axis = fg, col.lab = fg,
                          col.main = fg, col.sub = fg, pty = "s")
                on.exit(par(op), add = TRUE)

                curve_color <- jmvcore::colorPalette(1, theme$palette)[1]
                # The caveat rides on the plot itself because an exported plot travels
                # without the table's footnote. It is the subtitle, not a legend line: that
                # line spanned the whole axis, so the bottom-right legend reached across to
                # the curve's steep start at specificity ~1.
                plot(state$specificity, state$sensitivity, type = "l",
                    xlim = c(1, 0), ylim = c(0, 1), asp = 1, xaxs = "i", yaxs = "i",
                    xlab = .("Specificity"), ylab = .("Sensitivity"),
                    main = sprintf(.("ROC Curve (AUC = %s)"), auc_val),
                    sub = .("Apparent (in-sample): does not account for model fitting or selection"),
                    col = curve_color, lwd = 2
                )
                abline(a = 1, b = -1, col = fg, lty = 2)
                # Positional %n$ markers so a translator can reorder the three conversions.
                if (!is.null(ci_obj))
                    legend("bottomright",
                        legend = sprintf(.("AUC = %1$s (in-sample DeLong 95%% CI: %2$.3f-%3$.3f)"),
                                         auc_val, ci_obj[1], ci_obj[3]),
                        col = curve_color, lwd = 2,
                        bty = "n", text.col = fg, cex = 0.8, inset = 0.02
                    )
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

                # Copy-ready report sentence (complete phrase; placeholders filled from results)
                report <- sprintf(
                    .("%1$s logistic regression with %2$s lambda selection and %3$d-fold cross-validation was applied to %4$d candidate variables (%5$d model terms after dummy coding) in %6$d independent observations (%7$d events, %8$d non-events)."),
                    penalty_name, private$.lambdaLabel(), fit$nfolds, data$n_vars, data$p, data$n,
                    data$n_events, data$n_nonevents
                )
                # Retained terms by per-SD magnitude (|beta| x SD of the fitted column, so
                # the order holds with or without standardization). This listed the first
                # five in COLUMN order under "8 term(s) were retained:", silently dropping
                # the second-strongest predictor of birthwt.
                if (n_sel > 0) {
                    mag <- abs(fit$selected_coefs) * private$.fitColSDs(data$X)[fit$selected]
                    sel_sorted <- fit$selected[order(-mag)]
                }
                retained <- if (n_sel == 0) .("No term was retained.")
                    else if (n_sel <= 5) sprintf(
                        .("%1$d term(s) were retained (largest absolute standardized coefficient first): %2$s."),
                        n_sel, paste(sel_sorted, collapse = ", "))
                    else sprintf(
                        .("%1$d terms were retained, including the five with the largest absolute standardized coefficients: %2$s (all are listed in the Selected Variables table)."),
                        n_sel, paste(sel_sorted[1:5], collapse = ", "))
                report <- paste(report, retained)
                if (!is.na(auc_val)) {
                    report <- paste0(report, " ", sprintf(
                        .("Apparent (in-sample) discrimination was AUC = %.3f; enable Bootstrap Internal Validation for an optimism-corrected estimate."),
                        auc_val
                    ))
                }

                self$results$summaryText$setContent(
                    paste0("<p>", jmvcore::htmlEscape(report), "</p>")
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
                    .("<li><strong>Standardization (default on):</strong> glmnet learns centering and scaling within each training fold and each bootstrap fit, so held-out observations do not determine those transforms. It uses the population (1/n) standard deviation. The Coefficient and Odds Ratio columns of the Selected Variables table are returned on the <strong>original measurement scale</strong> (per 1 unit of a continuous predictor, or present vs absent for a binary one). The Importance column and the Coefficient Plot use the sample-standard-deviation scale, which is the quantity that is comparable across predictors with different units.</li>")
                } else {
                    .("<li>Predictors were <strong>not</strong> standardized, so the penalty acts on each coefficient in its own units. Coefficients, odds ratios and the Coefficient Plot are on the original measurement scale; the Importance columns are per standard deviation, so they remain comparable across predictors.</li>")
                }
                self$results$methodologyNotes$setContent(paste0(
                    "<h4>", .("Technical Notes"), "</h4>",
                    "<ul>",
                    .("<li>Penalized coefficients do not have standard errors or p-values, and this analysis provides no coefficient inference. Bootstrap validation estimates optimism-corrected performance (AUC, Brier score, calibration slope), not coefficient uncertainty.</li>"),
                    standardize_note,
                    if (identical(self$options$penalty, "ridge"))
                        .("<li>With correlated predictors, Ridge shares the coefficient among them rather than picking one, which is one reason to prefer it when predictors are collinear.</li>")
                    else
                        .("<li>With correlated predictors, LASSO arbitrarily selects one from a group. Consider elastic net (alpha 0.5).</li>"),
                    .("<li>An events-per-variable (EPV) of 10 is a rule of thumb (Peduzzi et al. 1996), not a guarantee. Sample-size criteria for prediction models (Riley et al. 2019, 2020) are preferred, and penalization does not make a small sample reliable (Riley et al. 2021).</li>"),
                    .("<li>The scoring system rounds coefficients to integers, which loses precision but gains clinical usability.</li>"),
                    sprintf(.("<li>Continuous predictors in the scoring system are dichotomized at %s (set by 'Cut point for continuous predictors'); the score-based performance therefore reflects a simplified point model and may differ from the continuous penalized model's AUC.</li>"), private$.scoreCutLabel()),
                    .("<li>Bootstrap optimism correction repeats scaling and lambda selection in every resample. Copies of the same observation stay in one cross-validation fold. It estimates optimism in model performance; it does not validate the simplified score or replace external validation.</li>"),
                    "</ul>"
                ))
            },
            .populateClinicalGuidance = function() {
                self$results$clinicalGuidance$setContent(paste0(
                    "<h4>", .("Clinical Interpretation"), "</h4>",
                    "<ul>",
                    .("<li>Selected variables are those the model retained at the chosen lambda. LASSO selection is unstable: with a different sample it can retain different variables, including ones that carry no signal, so a selected variable is not thereby shown to be a marker of the event.</li>"),
                    .("<li>Variables NOT selected are not necessarily unimportant - they may be redundant with selected features.</li>"),
                    .("<li>The scoring system assigns positive points for features favoring the event class and negative points for the reference class.</li>"),
                    .("<li>Higher total scores indicate a higher event rate only when the score discriminates in the right direction (score AUC above 0.5 in the Scoring System Performance table); a warning is shown when it does not.</li>"),
                    .("<li>Always validate the scoring system on an independent cohort before clinical adoption.</li>"),
                    .("<li>Inter-observer agreement should be assessed for any morphologic scoring components.</li>"),
                    "</ul>"
                ))
            }
        )
    )
}
