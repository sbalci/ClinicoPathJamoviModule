#' @title Firth's Penalized Likelihood Regression
#' @importFrom R6 R6Class
#' @importFrom jmvcore .
#' @import survival

firthregressionClass <- R6::R6Class(
    "firthregressionClass",
    inherit = firthregressionBase,
    private = list(

        # ── Notice collection (HTML-based) ────────────────────────────
        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content
            )
        },

        .renderNotices = function() {
            notices <- private$.noticeList
            if (length(notices) == 0) return()

            style_map <- list(
                ERROR = list(bg = "#FEE2E2", border = "#EF4444", color = "#991B1B", icon = "\u2716"),
                STRONG_WARNING = list(bg = "#FFF7ED", border = "#F97316", color = "#9A3412", icon = "\u26A0\uFE0F"),
                WARNING = list(bg = "#FFFBEB", border = "#F59E0B", color = "#92400E", icon = "\u26A0"),
                INFO = list(bg = "#EFF6FF", border = "#3B82F6", color = "#1E40AF", icon = "\u2139\uFE0F")
            )

            html <- "<div style='margin: 8px 0;'>"
            for (n in notices) {
                style <- style_map[[n$type]]
                if (is.null(style)) style <- style_map[["INFO"]]
                safe_title <- private$.escapeHtml(n$title)
                safe_content <- private$.escapeHtml(n$content)
                html <- paste0(
                    html,
                    "<div style='background: ", style$bg, "; border-left: 4px solid ", style$border, "; ",
                    "padding: 12px; margin: 8px 0; border-radius: 4px;'>",
                    "<strong style='color: ", style$color, ";'>",
                    style$icon, " ", safe_title, "</strong><br>",
                    "<span style='color: #374151;'>", safe_content, "</span>",
                    "</div>"
                )
            }
            html <- paste0(html, "</div>")
            self$results$notices$setContent(html)
        },

        .escapeHtml = function(x) {
            x <- gsub("&", "&amp;", as.character(x))
            x <- gsub("<", "&lt;", x)
            x <- gsub(">", "&gt;", x)
            x <- gsub("\"", "&quot;", x)
            x
        },

        .showMessage = function(message_html) {
            self$results$instructions$setVisible(TRUE)
            self$results$instructions$setContent(message_html)
        },

        # ── Initialization ────────────────────────────────────────────
        .init = function() {

            if (is.null(self$data) || is.null(self$options$outcome) ||
                length(self$options$predictors) == 0) {
                private$.showMessage(
                    "<html>
                    <head><style>
                        h2 { color: #2563EB; }
                        body { font-family: Arial, sans-serif; margin: 20px; }
                        .highlight { background-color: #DBEAFE; padding: 2px 6px; border-radius: 3px; }
                        .step { margin: 10px 0; padding: 8px; background-color: #F3F4F6; border-radius: 5px; }
                    </style></head>
                    <body>
                    <h2>Firth's Penalized Likelihood Regression</h2>
                    <p><strong>Bias-reduced regression for small samples, rare events, and separation</strong></p>
                    <div class='step'>
                    <strong>Required Variables:</strong>
                    <ul>
                        <li><span class='highlight'>Outcome Variable</span>: Binary variable (logistic) or event indicator (Cox)</li>
                        <li><span class='highlight'>Predictor Variables</span>: One or more candidate predictors</li>
                        <li><span class='highlight'>Time Variable</span> (Cox only): Time to event or censoring</li>
                    </ul>
                    </div>
                    <div class='step'>
                    <strong>When to use Firth's method:</strong>
                    <ul>
                        <li>Small sample sizes (n &lt; 50 or events &lt; 20)</li>
                        <li>Complete or quasi-complete separation</li>
                        <li>Rare events (event rate &lt; 10%)</li>
                        <li>When standard MLE gives infinite or extreme coefficient estimates</li>
                    </ul>
                    </div>
                    <p><em>Tip: Select 'Compare with Standard Model' to see how much bias reduction Firth provides.</em></p>
                    </body></html>"
                )
                return()
            }

            private$.initResults()
        },

        .initResults = function() {
            self$results$instructions$setVisible(FALSE)

            # Set effect size column title based on analysis type
            is_cox <- self$options$analysisType == "cox"
            effect_label <- if (is_cox) "HR" else "OR"
            self$results$coefficients$getColumn("effect_size")$setSuperTitle(effect_label)

            # Set CI column titles with confidence level
            ci_pct <- round(self$options$ciLevel * 100)
            self$results$coefficients$getColumn("lower_ci")$setSuperTitle(
                paste0(ci_pct, "% CI")
            )
            self$results$coefficients$getColumn("upper_ci")$setSuperTitle(
                paste0(ci_pct, "% CI")
            )

            # Update comparison table column titles
            comp_effect_label <- if (is_cox) "HR" else "OR"
            self$results$comparisonTable$getColumn("firth_effect")$setTitle(
                paste0("Firth ", comp_effect_label)
            )
            self$results$comparisonTable$getColumn("standard_effect")$setTitle(
                paste0("Standard ", comp_effect_label)
            )
        },

        # ── Main dispatcher ───────────────────────────────────────────
        .run = function() {

            if (is.null(self$data) || is.null(self$options$outcome) ||
                length(self$options$predictors) == 0) {
                return()
            }

            private$.noticeList <- list()

            # Check for Cox-specific requirement
            if (self$options$analysisType == "cox" && is.null(self$options$time)) {
                private$.addNotice("ERROR", "Missing Time Variable",
                    "Cox regression requires a Time Variable. Please select one.")
                private$.renderNotices()
                return()
            }

            # Prepare data
            data_info <- tryCatch(
                private$.prepareData(),
                error = function(e) {
                    private$.addNotice("ERROR", "Data Preparation Error",
                        private$.escapeHtml(e$message))
                    private$.renderNotices()
                    return(NULL)
                }
            )
            if (is.null(data_info)) return()

            # Suitability assessment
            if (isTRUE(self$options$suitabilityCheck)) {
                private$.assessSuitability(data_info)
            }

            # Separation detection (logistic only)
            if (isTRUE(self$options$separationCheck)) {
                private$.runSeparationCheck(data_info)
            }

            # Dispatch to analysis type
            if (self$options$analysisType == "logistic") {
                private$.runLogistic(data_info)
            } else {
                private$.runCox(data_info)
            }

            # Explanatory output
            if (isTRUE(self$options$showExplanations)) {
                private$.showExplanations()
            }

            private$.renderNotices()
        },

        # ── Data preparation ──────────────────────────────────────────
        .prepareData = function() {
            data <- self$data
            outcome_var <- self$options$outcome
            predictors <- self$options$predictors
            is_cox <- self$options$analysisType == "cox"

            # Build analysis data frame
            vars_needed <- c(outcome_var, predictors)
            if (is_cox) {
                time_var <- self$options$time
                vars_needed <- c(time_var, vars_needed)
            }

            # Check all variables exist
            missing_vars <- setdiff(vars_needed, names(data))
            if (length(missing_vars) > 0) {
                stop(sprintf("Variables not found in data: %s",
                    paste(missing_vars, collapse = ", ")))
            }

            analysis_data <- data[, vars_needed, drop = FALSE]
            analysis_data <- na.omit(analysis_data)

            n_complete <- nrow(analysis_data)
            n_dropped <- nrow(data) - n_complete

            if (n_complete < 5) {
                stop(sprintf("Only %d complete cases available. Need at least 5.", n_complete))
            }

            if (n_dropped > 0) {
                drop_pct <- round(100 * n_dropped / nrow(data), 1)
                if (drop_pct > 10) {
                    private$.addNotice("WARNING", "Missing Data",
                        sprintf("%d cases (%.1f%%) excluded due to missing values. %d complete cases used.",
                            n_dropped, drop_pct, n_complete))
                }
            }

            # Encode outcome
            outcome_data <- analysis_data[[outcome_var]]
            outcome_level <- self$options$outcomeLevel

            if (is.factor(outcome_data)) {
                lvls <- levels(outcome_data)
                if (!is.null(outcome_level) && outcome_level != "") {
                    event_binary <- as.integer(outcome_data == outcome_level)
                } else if (length(lvls) == 2) {
                    event_binary <- as.integer(outcome_data == lvls[2])
                } else {
                    stop(sprintf("Outcome has %d levels. Please specify the Event Level.", length(lvls)))
                }
            } else {
                unique_vals <- sort(unique(outcome_data[!is.na(outcome_data)]))
                if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
                    event_binary <- as.integer(outcome_data)
                } else if (length(unique_vals) == 2) {
                    if (!is.null(outcome_level) && outcome_level != "") {
                        event_binary <- as.integer(outcome_data == as.numeric(outcome_level))
                    } else {
                        event_binary <- as.integer(outcome_data == max(unique_vals))
                    }
                } else {
                    stop("Outcome must be binary (exactly 2 unique values).")
                }
            }

            n_events <- sum(event_binary == 1)
            n_nonevents <- sum(event_binary == 0)

            if (n_events < 1 || n_nonevents < 1) {
                stop("Outcome must have at least one event and one non-event.")
            }

            # Build predictor data
            pred_data <- analysis_data[, predictors, drop = FALSE]

            # Create model matrix for checks
            x_matrix <- tryCatch(
                model.matrix(~ . - 1, data = pred_data),
                error = function(e) {
                    stop(sprintf("Could not create model matrix: %s", e$message))
                }
            )

            # Remove near-constant columns
            col_vars <- apply(x_matrix, 2, var, na.rm = TRUE)
            constant_cols <- which(col_vars < 1e-10 | is.na(col_vars))
            if (length(constant_cols) > 0) {
                removed_names <- colnames(x_matrix)[constant_cols]
                x_matrix <- x_matrix[, -constant_cols, drop = FALSE]
                private$.addNotice("WARNING", "Constant Predictors Removed",
                    sprintf("Removed %d constant or near-constant columns: %s",
                        length(constant_cols), paste(removed_names, collapse = ", ")))
            }

            if (ncol(x_matrix) == 0) {
                stop("No valid predictor columns remain after removing constants.")
            }

            # Prepare time variable for Cox
            time_values <- NULL
            if (is_cox) {
                time_values <- as.numeric(analysis_data[[self$options$time]])
                if (any(time_values <= 0, na.rm = TRUE)) {
                    stop("Time variable must contain only positive values.")
                }
            }

            list(
                data = analysis_data,
                pred_data = pred_data,
                x_matrix = x_matrix,
                event_binary = event_binary,
                time_values = time_values,
                n = n_complete,
                n_events = n_events,
                n_nonevents = n_nonevents,
                n_predictors = ncol(x_matrix),
                predictor_names = predictors,
                is_cox = is_cox
            )
        },

        # ── Suitability assessment ────────────────────────────────────
        .assessSuitability = function(data_info) {
            html_result <- self$results$suitabilityReport
            if (is.null(html_result)) return()

            n <- data_info$n
            n_events <- data_info$n_events
            n_vars <- data_info$n_predictors
            epv <- if (n_vars > 0) n_events / n_vars else Inf
            event_rate <- n_events / n * 100

            # Traffic-light assessments
            # 1. Sample size
            n_color <- if (n < 20) "red" else if (n < 50) "orange" else "green"
            n_icon <- if (n < 20) "\u2716" else if (n < 50) "\u26A0" else "\u2714"

            # 2. EPV
            epv_color <- if (epv < 5) "red" else if (epv < 10) "orange" else "green"
            epv_icon <- if (epv < 5) "\u2716" else if (epv < 10) "\u26A0" else "\u2714"

            # 3. Event rate
            rate_color <- if (event_rate < 5 || event_rate > 95) "red" else
                           if (event_rate < 10 || event_rate > 90) "orange" else "green"
            rate_icon <- if (event_rate < 5 || event_rate > 95) "\u2716" else
                          if (event_rate < 10 || event_rate > 90) "\u26A0" else "\u2714"

            # 4. Firth recommendation
            needs_firth <- epv < 10 || event_rate < 10 || event_rate > 90 || n < 50
            firth_color <- if (needs_firth) "green" else "#555"
            firth_text <- if (needs_firth) {
                "\u2714 <b>Firth's method recommended</b>: Small-sample or rare-event conditions detected where standard MLE may be biased."
            } else {
                "\u2139 <b>Standard MLE may be adequate</b>: Data conditions do not strongly indicate the need for bias correction, but Firth's method is never harmful."
            }

            type_label <- if (data_info$is_cox) "Firth Cox Regression" else "Firth Logistic Regression"

            content <- paste0(
                "<html><div style='border: 1px solid #ccc; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
                "<div style='font-weight: bold; border-bottom: 1px solid #ddd; padding-bottom: 5px; margin-bottom: 5px;'>",
                "Dataset Assessment for ", type_label, "</div>",

                "<div style='display: flex; justify-content: space-between; margin: 3px 0;'>",
                "<span><b>Complete Cases:</b> ", n, "</span>",
                "<span><b>Events:</b> ", n_events, " / Non-events: ", data_info$n_nonevents, "</span>",
                "</div>",

                "<div style='display: flex; justify-content: space-between; margin: 3px 0;'>",
                "<span><b>Predictors (model columns):</b> ", n_vars, "</span>",
                "<span style='color:", epv_color, "'><b>EPV: ", round(epv, 1), "</b></span>",
                "</div>",

                "<table style='width:100%; margin-top: 8px; border-collapse: collapse;'>",
                "<tr><td style='padding: 3px;'><span style='color:", n_color, "'>", n_icon, "</span> Sample Size</td>",
                "<td style='padding: 3px;'>N = ", n, if (n < 20) " (very small)" else if (n < 50) " (small)" else " (adequate)", "</td></tr>",

                "<tr><td style='padding: 3px;'><span style='color:", epv_color, "'>", epv_icon, "</span> Events Per Variable</td>",
                "<td style='padding: 3px;'>EPV = ", round(epv, 1),
                if (epv < 5) " (very low, high overfitting risk)" else if (epv < 10) " (low, bias likely)" else " (adequate)", "</td></tr>",

                "<tr><td style='padding: 3px;'><span style='color:", rate_color, "'>", rate_icon, "</span> Event Rate</td>",
                "<td style='padding: 3px;'>", round(event_rate, 1), "%",
                if (event_rate < 5) " (very rare events)" else if (event_rate < 10) " (rare events)" else
                if (event_rate > 95) " (very common events)" else if (event_rate > 90) " (common events)" else " (balanced)", "</td></tr>",

                "<tr><td style='padding: 3px;'><span style='color:", firth_color, "'>", if (needs_firth) "\u2714" else "\u2139", "</span> Firth Recommendation</td>",
                "<td style='padding: 3px;'>", firth_text, "</td></tr>",
                "</table>",

                "</div></html>"
            )

            html_result$setContent(content)
        },

        # ── Separation detection ──────────────────────────────────────
        .runSeparationCheck = function(data_info) {
            if (data_info$is_cox) return()  # Separation is a logistic-specific concern

            table <- self$results$separationDiagnostics
            table$deleteRows()

            pred_data <- data_info$pred_data
            event <- data_info$event_binary
            row_key <- 0

            for (v in data_info$predictor_names) {
                col <- pred_data[[v]]
                row_key <- row_key + 1

                if (is.factor(col) || is.character(col)) {
                    col <- as.factor(col)
                    cross_tab <- table(col, event)

                    # Check for zero cells
                    has_zero <- any(cross_tab == 0)
                    if (has_zero) {
                        zero_cells <- which(cross_tab == 0, arr.ind = TRUE)
                        zero_info <- paste(
                            sapply(seq_len(nrow(zero_cells)), function(i) {
                                paste0(rownames(cross_tab)[zero_cells[i, 1]], "/",
                                       colnames(cross_tab)[zero_cells[i, 2]])
                            }),
                            collapse = "; "
                        )
                        # Check if complete separation (entire row is zero)
                        row_zeros <- apply(cross_tab, 1, function(r) any(r == 0))
                        if (all(row_zeros)) {
                            status <- "Complete Separation"
                        } else {
                            status <- "Quasi-Complete Separation"
                        }
                        table$addRow(rowKey = row_key, values = list(
                            variable = v,
                            type = "Categorical",
                            separation_status = status,
                            detail = paste("Zero cells:", zero_info)
                        ))
                    } else {
                        table$addRow(rowKey = row_key, values = list(
                            variable = v,
                            type = "Categorical",
                            separation_status = "No Separation",
                            detail = "All cross-tabulation cells non-zero"
                        ))
                    }
                } else {
                    # Continuous: check overlap of distributions
                    vals_event <- col[event == 1]
                    vals_noevent <- col[event == 0]
                    range_event <- range(vals_event, na.rm = TRUE)
                    range_noevent <- range(vals_noevent, na.rm = TRUE)

                    overlap <- min(range_event[2], range_noevent[2]) - max(range_event[1], range_noevent[1])
                    total_range <- max(range_event[2], range_noevent[2]) - min(range_event[1], range_noevent[1])

                    if (overlap <= 0) {
                        table$addRow(rowKey = row_key, values = list(
                            variable = v,
                            type = "Continuous",
                            separation_status = "Complete Separation",
                            detail = sprintf("No overlap: events [%.2f, %.2f], non-events [%.2f, %.2f]",
                                range_event[1], range_event[2], range_noevent[1], range_noevent[2])
                        ))
                    } else if (overlap / total_range < 0.1) {
                        table$addRow(rowKey = row_key, values = list(
                            variable = v,
                            type = "Continuous",
                            separation_status = "Near Separation",
                            detail = sprintf("Minimal overlap (%.1f%%)", 100 * overlap / total_range)
                        ))
                    } else {
                        table$addRow(rowKey = row_key, values = list(
                            variable = v,
                            type = "Continuous",
                            separation_status = "No Separation",
                            detail = sprintf("Adequate overlap (%.1f%%)", 100 * overlap / total_range)
                        ))
                    }
                }
            }
        },

        # ── Firth logistic regression ─────────────────────────────────
        .runLogistic = function(data_info) {

            if (!requireNamespace("logistf", quietly = TRUE)) {
                private$.addNotice("ERROR", "Package Not Available",
                    "The 'logistf' package is required for Firth logistic regression. Please install it: install.packages('logistf')")
                return()
            }

            analysis_data <- data_info$data
            analysis_data$.outcome <- data_info$event_binary

            # Build formula
            pred_terms <- paste(jmvcore::composeTerms(data_info$predictor_names), collapse = " + ")
            formula_str <- paste(".outcome ~", pred_terms)
            f <- as.formula(formula_str)

            # Fit Firth model
            ci_alpha <- 1 - self$options$ciLevel
            use_profile <- self$options$ciMethod == "profile"

            firth_fit <- tryCatch({
                logistf::logistf(f, data = analysis_data, alpha = ci_alpha,
                    pl = use_profile)
            }, error = function(e) {
                private$.addNotice("ERROR", "Firth Model Failed",
                    sprintf("logistf failed: %s", private$.escapeHtml(e$message)))
                return(NULL)
            })

            if (is.null(firth_fit)) return()

            # Fit standard model for comparison
            standard_fit <- NULL
            if (isTRUE(self$options$compareStandard)) {
                standard_fit <- tryCatch({
                    glm(f, data = analysis_data, family = binomial())
                }, warning = function(w) {
                    private$.addNotice("INFO", "Standard Model Warning",
                        sprintf("Standard GLM produced warning: %s", private$.escapeHtml(w$message)))
                    suppressWarnings(glm(f, data = analysis_data, family = binomial()))
                }, error = function(e) {
                    private$.addNotice("WARNING", "Standard Model Failed",
                        sprintf("Standard GLM could not be fit: %s. This confirms Firth's method is needed.",
                            private$.escapeHtml(e$message)))
                    NULL
                })
            }

            # Populate results
            private$.populateCoefficients(firth_fit, standard_fit, "logistic")
            private$.populateModelFit(firth_fit, standard_fit, data_info, "logistic")

            if (!is.null(standard_fit) && isTRUE(self$options$compareStandard)) {
                private$.populateComparison(firth_fit, standard_fit, "logistic")
            }

            # Plots
            if (isTRUE(self$options$forestPlot)) {
                private$.prepareForestPlot(firth_fit, "logistic")
            }
            if (isTRUE(self$options$separationPlot)) {
                private$.prepareSeparationPlot(data_info)
            }

            # Summary
            if (isTRUE(self$options$showSummary)) {
                private$.showSummaryText(firth_fit, standard_fit, data_info, "logistic")
            }

            # EPV-based notices
            epv <- data_info$n_events / data_info$n_predictors
            if (epv < 5) {
                private$.addNotice("STRONG_WARNING", "Very Low EPV",
                    sprintf("EPV = %.1f. Even with Firth correction, estimates may be unreliable. Consider reducing the number of predictors.", epv))
            } else if (epv < 10) {
                private$.addNotice("INFO", "Firth Correction Applied",
                    sprintf("EPV = %.1f. Firth's bias reduction is particularly valuable at this EPV level.", epv))
            }

            private$.addNotice("INFO", "Analysis Complete",
                sprintf("Firth logistic regression completed with %d observations (%d events, %d non-events), %d predictors.",
                    data_info$n, data_info$n_events, data_info$n_nonevents, data_info$n_predictors))
        },

        # ── Firth Cox regression ──────────────────────────────────────
        .runCox = function(data_info) {

            if (!requireNamespace("coxphf", quietly = TRUE)) {
                private$.addNotice("ERROR", "Package Not Available",
                    "The 'coxphf' package is required for Firth Cox regression. Please install it: install.packages('coxphf')")
                return()
            }

            analysis_data <- data_info$data
            analysis_data$.time <- data_info$time_values
            analysis_data$.event <- data_info$event_binary

            # Build formula
            pred_terms <- paste(jmvcore::composeTerms(data_info$predictor_names), collapse = " + ")
            formula_str <- paste("survival::Surv(.time, .event) ~", pred_terms)
            f <- as.formula(formula_str)

            # Fit Firth Cox model
            ci_alpha <- 1 - self$options$ciLevel

            firth_fit <- tryCatch({
                coxphf::coxphf(f, data = analysis_data, alpha = ci_alpha)
            }, error = function(e) {
                private$.addNotice("ERROR", "Firth Cox Model Failed",
                    sprintf("coxphf failed: %s", private$.escapeHtml(e$message)))
                return(NULL)
            })

            if (is.null(firth_fit)) return()

            # Fit standard Cox for comparison
            standard_fit <- NULL
            if (isTRUE(self$options$compareStandard)) {
                standard_fit <- tryCatch({
                    survival::coxph(f, data = analysis_data)
                }, warning = function(w) {
                    private$.addNotice("INFO", "Standard Cox Warning",
                        sprintf("Standard coxph produced warning: %s", private$.escapeHtml(w$message)))
                    suppressWarnings(survival::coxph(f, data = analysis_data))
                }, error = function(e) {
                    private$.addNotice("WARNING", "Standard Cox Failed",
                        sprintf("Standard coxph could not be fit: %s. This confirms Firth's method is needed.",
                            private$.escapeHtml(e$message)))
                    NULL
                })
            }

            # Populate results
            private$.populateCoefficients(firth_fit, standard_fit, "cox")
            private$.populateModelFit(firth_fit, standard_fit, data_info, "cox")

            if (!is.null(standard_fit) && isTRUE(self$options$compareStandard)) {
                private$.populateComparison(firth_fit, standard_fit, "cox")
            }

            # Plots
            if (isTRUE(self$options$forestPlot)) {
                private$.prepareForestPlot(firth_fit, "cox")
            }

            # Summary
            if (isTRUE(self$options$showSummary)) {
                private$.showSummaryText(firth_fit, standard_fit, data_info, "cox")
            }

            # EPV notices
            epv <- data_info$n_events / data_info$n_predictors
            if (epv < 5) {
                private$.addNotice("STRONG_WARNING", "Very Low EPV",
                    sprintf("EPV = %.1f. Even with Firth correction, estimates may be unreliable. Consider reducing the number of predictors.", epv))
            } else if (epv < 10) {
                private$.addNotice("INFO", "Firth Correction Applied",
                    sprintf("EPV = %.1f. Firth's bias reduction is particularly valuable at this EPV level.", epv))
            }

            private$.addNotice("INFO", "Analysis Complete",
                sprintf("Firth Cox regression completed with %d observations (%d events), %d predictors.",
                    data_info$n, data_info$n_events, data_info$n_predictors))
        },

        # ── Populate coefficients table ───────────────────────────────
        .populateCoefficients = function(firth_fit, standard_fit, model_type) {
            table <- self$results$coefficients
            table$deleteRows()

            if (model_type == "logistic") {
                coefs <- firth_fit$coefficients
                var_names <- names(coefs)
                # Remove intercept
                intercept_idx <- which(var_names == "(Intercept)")
                if (length(intercept_idx) > 0) {
                    coefs <- coefs[-intercept_idx]
                    var_names <- var_names[-intercept_idx]
                }

                se_vals <- sqrt(diag(vcov(firth_fit)))
                if (length(intercept_idx) > 0) se_vals <- se_vals[-intercept_idx]

                p_vals <- firth_fit$prob
                if (length(intercept_idx) > 0) p_vals <- p_vals[-intercept_idx]

                # CIs
                ci <- tryCatch(confint(firth_fit), error = function(e) NULL)
                if (!is.null(ci)) {
                    if (is.vector(ci)) ci <- matrix(ci, nrow = 1)
                    if (length(intercept_idx) > 0) ci <- ci[-intercept_idx, , drop = FALSE]
                    lower_ci <- ci[, 1]
                    upper_ci <- ci[, 2]
                } else {
                    z_crit <- qnorm(1 - (1 - self$options$ciLevel) / 2)
                    lower_ci <- coefs - z_crit * se_vals
                    upper_ci <- coefs + z_crit * se_vals
                }

                # Standard model for bias comparison
                std_coefs <- NULL
                if (!is.null(standard_fit)) {
                    std_all <- coef(standard_fit)
                    if (length(intercept_idx) > 0) {
                        int_idx_std <- which(names(std_all) == "(Intercept)")
                        if (length(int_idx_std) > 0) std_all <- std_all[-int_idx_std]
                    }
                    std_coefs <- std_all
                }

                for (i in seq_along(coefs)) {
                    bias_red <- NA
                    if (!is.null(std_coefs) && i <= length(std_coefs) && abs(std_coefs[i]) > 1e-10) {
                        bias_red <- 100 * (1 - coefs[i] / std_coefs[i])
                    }

                    table$addRow(rowKey = i, values = list(
                        variable = var_names[i],
                        coefficient = coefs[i],
                        se = se_vals[i],
                        effect_size = exp(coefs[i]),
                        lower_ci = exp(lower_ci[i]),
                        upper_ci = exp(upper_ci[i]),
                        p_value = p_vals[i],
                        bias_reduction = bias_red
                    ))
                }

                ci_method <- if (self$options$ciMethod == "profile") "profile penalized likelihood" else "Wald"
                table$setNote("method", paste0(
                    "Firth penalized likelihood logistic regression (Firth, 1993). ",
                    "CIs based on ", ci_method, " method. ",
                    "P-values from penalized likelihood ratio tests."))

            } else {
                # Cox model
                coefs <- firth_fit$coefficients
                var_names <- names(coefs)
                if (is.null(var_names)) var_names <- paste0("V", seq_along(coefs))

                p_vals <- firth_fit$prob

                lower_ci <- firth_fit$ci.lower
                upper_ci <- firth_fit$ci.upper

                # SE approximation from CI width
                z_crit <- qnorm(1 - (1 - self$options$ciLevel) / 2)
                se_vals <- (upper_ci - lower_ci) / (2 * z_crit)

                # Standard model for bias comparison
                std_coefs <- NULL
                if (!is.null(standard_fit)) {
                    std_coefs <- coef(standard_fit)
                }

                for (i in seq_along(coefs)) {
                    bias_red <- NA
                    if (!is.null(std_coefs) && i <= length(std_coefs) && abs(std_coefs[i]) > 1e-10) {
                        bias_red <- 100 * (1 - coefs[i] / std_coefs[i])
                    }

                    table$addRow(rowKey = i, values = list(
                        variable = var_names[i],
                        coefficient = coefs[i],
                        se = se_vals[i],
                        effect_size = exp(coefs[i]),
                        lower_ci = exp(lower_ci[i]),
                        upper_ci = exp(upper_ci[i]),
                        p_value = p_vals[i],
                        bias_reduction = bias_red
                    ))
                }

                table$setNote("method", paste0(
                    "Firth penalized Cox regression (Heinze & Schemper, 2001). ",
                    "CIs based on profile penalized likelihood. ",
                    "P-values from penalized likelihood ratio tests."))
            }
        },

        # ── Populate model fit statistics ─────────────────────────────
        .populateModelFit = function(firth_fit, standard_fit, data_info, model_type) {
            table <- self$results$modelFit
            table$deleteRows()

            if (model_type == "logistic") {
                # Observations
                table$addRow(rowKey = 1, values = list(
                    metric = "Observations",
                    firth_value = as.character(data_info$n),
                    standard_value = if (!is.null(standard_fit)) as.character(nobs(standard_fit)) else ""
                ))

                # Events / Non-events
                table$addRow(rowKey = 2, values = list(
                    metric = "Events / Non-events",
                    firth_value = paste0(data_info$n_events, " / ", data_info$n_nonevents),
                    standard_value = ""
                ))

                # Log-likelihood
                firth_ll <- if (!is.null(firth_fit$loglik)) round(firth_fit$loglik[2], 3) else NA
                std_ll <- if (!is.null(standard_fit)) round(logLik(standard_fit), 3) else ""
                table$addRow(rowKey = 3, values = list(
                    metric = "Log-Likelihood",
                    firth_value = as.character(firth_ll),
                    standard_value = as.character(std_ll)
                ))

                # AIC
                n_params <- length(firth_fit$coefficients)
                firth_aic <- round(-2 * firth_fit$loglik[2] + 2 * n_params, 2)
                std_aic <- if (!is.null(standard_fit)) round(AIC(standard_fit), 2) else ""
                table$addRow(rowKey = 4, values = list(
                    metric = "AIC",
                    firth_value = as.character(firth_aic),
                    standard_value = as.character(std_aic)
                ))

                # Number of parameters
                table$addRow(rowKey = 5, values = list(
                    metric = "Parameters",
                    firth_value = as.character(n_params),
                    standard_value = if (!is.null(standard_fit)) as.character(length(coef(standard_fit))) else ""
                ))

            } else {
                # Cox model
                table$addRow(rowKey = 1, values = list(
                    metric = "Observations",
                    firth_value = as.character(data_info$n),
                    standard_value = if (!is.null(standard_fit)) as.character(standard_fit$n) else ""
                ))

                table$addRow(rowKey = 2, values = list(
                    metric = "Events",
                    firth_value = as.character(data_info$n_events),
                    standard_value = if (!is.null(standard_fit)) as.character(standard_fit$nevent) else ""
                ))

                # Log-likelihood
                firth_ll <- if (!is.null(firth_fit$loglik)) round(firth_fit$loglik[2], 3) else NA
                std_ll <- if (!is.null(standard_fit)) round(standard_fit$loglik[2], 3) else ""
                table$addRow(rowKey = 3, values = list(
                    metric = "Log-Likelihood (model)",
                    firth_value = as.character(firth_ll),
                    standard_value = as.character(std_ll)
                ))

                # Concordance (standard model only — coxphf doesn't directly provide it)
                if (!is.null(standard_fit)) {
                    conc <- tryCatch({
                        c_val <- summary(standard_fit)$concordance
                        sprintf("%.3f (SE %.3f)", c_val[1], c_val[2])
                    }, error = function(e) "")
                    table$addRow(rowKey = 4, values = list(
                        metric = "Concordance (standard)",
                        firth_value = "",
                        standard_value = conc
                    ))
                }

                # Number of parameters
                table$addRow(rowKey = 5, values = list(
                    metric = "Parameters",
                    firth_value = as.character(length(firth_fit$coefficients)),
                    standard_value = if (!is.null(standard_fit)) as.character(length(coef(standard_fit))) else ""
                ))
            }
        },

        # ── Populate comparison table ─────────────────────────────────
        .populateComparison = function(firth_fit, standard_fit, model_type) {
            table <- self$results$comparisonTable
            table$deleteRows()

            if (model_type == "logistic") {
                firth_coefs <- firth_fit$coefficients
                firth_names <- names(firth_coefs)
                firth_p <- firth_fit$prob

                std_coefs <- coef(standard_fit)
                std_p <- summary(standard_fit)$coefficients[, 4]

                # Remove intercept
                int_idx <- which(firth_names == "(Intercept)")
                if (length(int_idx) > 0) {
                    firth_coefs <- firth_coefs[-int_idx]
                    firth_names <- firth_names[-int_idx]
                    firth_p <- firth_p[-int_idx]

                    int_idx_std <- which(names(std_coefs) == "(Intercept)")
                    if (length(int_idx_std) > 0) {
                        std_coefs <- std_coefs[-int_idx_std]
                        std_p <- std_p[-int_idx_std]
                    }
                }

                for (i in seq_along(firth_coefs)) {
                    std_c <- if (i <= length(std_coefs)) std_coefs[i] else NA
                    std_pv <- if (i <= length(std_p)) std_p[i] else NA

                    table$addRow(rowKey = i, values = list(
                        variable = firth_names[i],
                        firth_coef = firth_coefs[i],
                        standard_coef = std_c,
                        firth_effect = exp(firth_coefs[i]),
                        standard_effect = if (!is.na(std_c)) exp(std_c) else NA,
                        firth_p = firth_p[i],
                        standard_p = std_pv
                    ))
                }

            } else {
                # Cox comparison
                firth_coefs <- firth_fit$coefficients
                firth_names <- names(firth_coefs)
                firth_p <- firth_fit$prob

                std_coefs <- coef(standard_fit)
                std_summary <- summary(standard_fit)
                std_p <- std_summary$coefficients[, "Pr(>|z|)"]

                for (i in seq_along(firth_coefs)) {
                    std_c <- if (i <= length(std_coefs)) std_coefs[i] else NA
                    std_pv <- if (i <= length(std_p)) std_p[i] else NA

                    table$addRow(rowKey = i, values = list(
                        variable = if (!is.null(firth_names)) firth_names[i] else paste0("V", i),
                        firth_coef = firth_coefs[i],
                        standard_coef = std_c,
                        firth_effect = exp(firth_coefs[i]),
                        standard_effect = if (!is.na(std_c)) exp(std_c) else NA,
                        firth_p = firth_p[i],
                        standard_p = std_pv
                    ))
                }
            }

            table$setNote("comparison", paste0(
                "Bias reduction is evident when Firth and standard estimates diverge. ",
                "Large divergence, especially with inflated standard model estimates, ",
                "confirms separation or small-sample bias."))
        },

        # ── Forest plot ───────────────────────────────────────────────
        .prepareForestPlot = function(firth_fit, model_type) {
            image <- self$results$forestPlotImage

            if (model_type == "logistic") {
                coefs <- firth_fit$coefficients
                var_names <- names(coefs)
                int_idx <- which(var_names == "(Intercept)")
                if (length(int_idx) > 0) {
                    coefs <- coefs[-int_idx]
                    var_names <- var_names[-int_idx]
                }

                ci <- tryCatch(confint(firth_fit), error = function(e) NULL)
                if (!is.null(ci)) {
                    if (is.vector(ci)) ci <- matrix(ci, nrow = 1)
                    if (length(int_idx) > 0) ci <- ci[-int_idx, , drop = FALSE]
                    lower <- exp(ci[, 1])
                    upper <- exp(ci[, 2])
                } else {
                    se_vals <- sqrt(diag(vcov(firth_fit)))
                    if (length(int_idx) > 0) se_vals <- se_vals[-int_idx]
                    z <- qnorm(1 - (1 - self$options$ciLevel) / 2)
                    lower <- exp(coefs - z * se_vals)
                    upper <- exp(coefs + z * se_vals)
                }

                plot_df <- data.frame(
                    variable = var_names,
                    effect = exp(coefs),
                    lower = lower,
                    upper = upper,
                    stringsAsFactors = FALSE
                )
            } else {
                coefs <- firth_fit$coefficients
                var_names <- names(coefs)
                if (is.null(var_names)) var_names <- paste0("V", seq_along(coefs))

                plot_df <- data.frame(
                    variable = var_names,
                    effect = exp(coefs),
                    lower = exp(firth_fit$ci.lower),
                    upper = exp(firth_fit$ci.upper),
                    stringsAsFactors = FALSE
                )
            }

            attr(plot_df, "model_type") <- model_type
            attr(plot_df, "ci_level") <- self$options$ciLevel
            image$setState(plot_df)
        },

        .renderForestPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || nrow(state) == 0) return(FALSE)

            model_type <- attr(state, "model_type")
            ci_pct <- round((attr(state, "ci_level") %||% 0.95) * 100)
            effect_label <- if (identical(model_type, "cox")) "Hazard Ratio" else "Odds Ratio"

            # Order variables by effect size
            state$variable <- factor(state$variable, levels = rev(state$variable))

            plot <- ggplot2::ggplot(state, ggplot2::aes(x = .data$effect, y = .data$variable)) +
                ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
                ggplot2::geom_pointrange(
                    ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
                    color = "#2563EB", size = 0.8, linewidth = 0.8
                ) +
                ggplot2::scale_x_log10() +
                ggplot2::labs(
                    x = paste0(effect_label, " (", ci_pct, "% CI, log scale)"),
                    y = NULL,
                    title = paste0("Firth ", effect_label, " Estimates")
                ) +
                ggtheme

            print(plot)
            return(TRUE)
        },

        # ── Separation diagnostic plot ────────────────────────────────
        .prepareSeparationPlot = function(data_info) {
            image <- self$results$separationPlotImage
            if (data_info$is_cox) {
                image$setState(NULL)
                return()
            }

            # Prepare data for plotting: show each predictor's distribution by outcome
            pred_data <- data_info$pred_data
            event <- data_info$event_binary

            # Only include first 6 predictors to keep readable
            pred_names <- data_info$predictor_names[seq_len(min(6, length(data_info$predictor_names)))]

            plot_rows <- list()
            for (v in pred_names) {
                col <- pred_data[[v]]
                if (is.numeric(col)) {
                    plot_rows[[length(plot_rows) + 1]] <- data.frame(
                        variable = v,
                        value = col,
                        outcome = factor(event, levels = c(0, 1), labels = c("Non-event", "Event")),
                        stringsAsFactors = FALSE
                    )
                }
            }

            if (length(plot_rows) > 0) {
                plot_df <- do.call(rbind, plot_rows)
                image$setState(as.data.frame(plot_df))
            } else {
                image$setState(NULL)
            }
        },

        .renderSeparationPlot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || nrow(state) == 0) return(FALSE)

            plot <- ggplot2::ggplot(state, ggplot2::aes(x = .data$outcome, y = .data$value, fill = .data$outcome)) +
                ggplot2::geom_boxplot(alpha = 0.7, outlier.size = 1) +
                ggplot2::facet_wrap(~ variable, scales = "free_y") +
                ggplot2::scale_fill_manual(values = c("Non-event" = "#93C5FD", "Event" = "#FCA5A5")) +
                ggplot2::labs(
                    x = "Outcome Group", y = "Value",
                    title = "Distribution by Outcome (Separation Diagnostic)",
                    fill = "Outcome"
                ) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom")

            print(plot)
            return(TRUE)
        },

        # ── Summary text ──────────────────────────────────────────────
        .showSummaryText = function(firth_fit, standard_fit, data_info, model_type) {
            html <- self$results$summaryText

            effect_label <- if (model_type == "cox") "hazard ratio" else "odds ratio"
            model_label <- if (model_type == "cox") "Firth penalized Cox regression" else "Firth penalized logistic regression"

            coefs <- firth_fit$coefficients
            var_names <- names(coefs)
            p_vals <- firth_fit$prob

            if (model_type == "logistic") {
                int_idx <- which(var_names == "(Intercept)")
                if (length(int_idx) > 0) {
                    coefs <- coefs[-int_idx]
                    var_names <- var_names[-int_idx]
                    p_vals <- p_vals[-int_idx]
                }
            }

            sig_vars <- var_names[p_vals < 0.05]
            n_sig <- length(sig_vars)

            sig_text <- if (n_sig == 0) {
                "No predictors reached statistical significance at the 0.05 level."
            } else {
                sig_details <- sapply(which(p_vals < 0.05), function(i) {
                    sprintf("%s (%s = %.2f, p = %.3f)", var_names[i], toupper(substring(effect_label, 1, 2)),
                        exp(coefs[i]), p_vals[i])
                })
                paste0("Significant predictors: ", paste(sig_details, collapse = "; "), ".")
            }

            bias_text <- ""
            if (!is.null(standard_fit)) {
                std_coefs <- coef(standard_fit)
                if (model_type == "logistic") {
                    int_idx_std <- which(names(std_coefs) == "(Intercept)")
                    if (length(int_idx_std) > 0) std_coefs <- std_coefs[-int_idx_std]
                }
                max_bias <- max(abs(coefs - std_coefs[seq_along(coefs)]) / pmax(abs(std_coefs[seq_along(coefs)]), 1e-10)) * 100
                bias_text <- sprintf(
                    " The maximum relative bias reduction from Firth's penalty was %.1f%%.",
                    max_bias
                )
            }

            content <- paste0(
                "<html><div style='padding: 10px; background: #F9FAFB; border-radius: 5px;'>",
                "<p>A ", model_label, " was fitted with ", data_info$n, " observations (",
                data_info$n_events, " events) and ", data_info$n_predictors,
                " predictor variables (model matrix columns).</p>",
                "<p>", sig_text, bias_text, "</p>",
                "</div></html>"
            )

            html$setContent(content)
        },

        # ── Explanations text ─────────────────────────────────────────
        .showExplanations = function() {
            html <- self$results$explanationText

            is_cox <- self$options$analysisType == "cox"
            model_type <- if (is_cox) "Cox proportional hazards" else "logistic"
            pkg_name <- if (is_cox) "coxphf" else "logistf"
            ref <- if (is_cox) "Heinze & Schemper, 2001" else "Firth, 1993; Heinze & Schemper, 2002"

            content <- paste0(
                "<html><div style='padding: 10px;'>",
                "<h3>About Firth's Penalized Likelihood</h3>",
                "<p>Firth's method (1993) adds a Jeffreys invariant prior penalty to the likelihood function, ",
                "which removes first-order bias from maximum likelihood estimates. For ", model_type,
                " models, this means:</p>",
                "<ul>",
                "<li><b>Finite estimates</b>: Unlike standard MLE, Firth always produces finite coefficient ",
                "estimates, even with complete or quasi-complete separation.</li>",
                "<li><b>Reduced small-sample bias</b>: Standard MLE systematically overestimates effect sizes ",
                "in small samples. Firth's penalty corrects this.</li>",
                "<li><b>Profile likelihood CIs</b>: More accurate than Wald intervals, especially near ",
                "boundary conditions.</li>",
                "</ul>",
                "<h4>When to use Firth's method</h4>",
                "<ul>",
                "<li>Fewer than 10 events per predictor variable (EPV &lt; 10)</li>",
                "<li>Rare events (event rate &lt; 10%) or very common events (&gt; 90%)</li>",
                "<li>Complete or quasi-complete separation (standard model gives infinite estimates)</li>",
                "<li>Small total sample size (n &lt; 50)</li>",
                "</ul>",
                "<h4>References</h4>",
                "<p>Firth D (1993). Bias reduction of maximum likelihood estimates. <i>Biometrika</i>, 80(1), 27-38.<br>",
                if (is_cox) "Heinze G, Schemper M (2001). A solution to the problem of monotone likelihood in Cox regression. <i>Biometrics</i>, 57(1), 114-119."
                else "Heinze G, Schemper M (2002). A solution to the problem of separation in logistic regression. <i>Statistics in Medicine</i>, 21(16), 2409-2419.",
                "<br>Implementation: R package <code>", pkg_name, "</code>.</p>",
                "</div></html>"
            )

            html$setContent(content)
        }
    )
)
