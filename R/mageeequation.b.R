#' @title Magee Equation - Oncotype DX Recurrence Score Estimator
#' @description
#' Implements the four Magee Equations from Klein et al. (2013, PMID: 23503643)
#' for estimating Oncotype DX Recurrence Score from standard pathology variables.
#' Supports H-score and percentage input formats with automatic conversion.
#'
#' @importFrom R6 R6Class
#' @import jmvcore

mageeequationClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "mageeequationClass",
    inherit = mageeequationBase,
    private = list(

        .noticeList = list(),

        .addNotice = function(type, title, content) {
            private$.noticeList[[length(private$.noticeList) + 1]] <- list(
                type = type, title = title, content = content
            )
        },

        .renderNotices = function() {
            if (length(private$.noticeList) == 0) return()
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
                    "</div>")
            }
            html <- paste0(html, "</div>")
            self$results$notices$setContent(html)
        },

        .init = function() {
            if (is.null(self$options$nuclearGrade) ||
                is.null(self$options$mitosis) ||
                is.null(self$options$erHscore) ||
                is.null(self$options$prHscore) ||
                is.null(self$options$her2Status)) {

                self$results$todo$setContent(paste0(
                    "<div class='alert alert-info'>",
                    "<h4>", .("Magee Equation - Oncotype DX Score Estimator"), "</h4>",
                    "<p>", .("Estimates the Oncotype DX Recurrence Score (RS) from standard pathology variables using the published Magee Equations."), "</p>",
                    "<h5>", .("Required Variables:"), "</h5>",
                    "<ul>",
                    "<li>", .("<strong>Nuclear Grade</strong> (1-3)"), "</li>",
                    "<li>", .("<strong>Mitotic Score</strong> (1-3)"), "</li>",
                    "<li>", .("<strong>ER H-Score</strong> (0-300) or Percentage (0-100)"), "</li>",
                    "<li>", .("<strong>PR H-Score</strong> (0-300) or Percentage (0-100)"), "</li>",
                    "<li>", .("<strong>HER2 Status</strong> (positive/negative)"), "</li>",
                    "</ul>",
                    "<h5>", .("Optional (for extended equations):"), "</h5>",
                    "<ul>",
                    "<li>", .("Nottingham Score (3-9) - for Equations 1 & 2"), "</li>",
                    "<li>", .("Tumor Size (cm) - for Equations 1 & 2"), "</li>",
                    "<li>", .("Ki-67 Index (%) - for Equations 1 & 3"), "</li>",
                    "</ul>",
                    "<p><em>", .("Reference: Klein ME et al. Prediction of the Oncotype DX recurrence score: use of pathology-generated equations derived by linear regression analysis. Mod Pathol. 2013;26(5):658-664. PMID: 23503643"), "</em></p>",
                    "</div>"))
                return()
            }

            # Show equation formulas
            private$.showEquationInfo()
        },

        .run = function() {
            if (is.null(self$options$nuclearGrade) ||
                is.null(self$options$mitosis) ||
                is.null(self$options$erHscore) ||
                is.null(self$options$prHscore) ||
                is.null(self$options$her2Status)) return()

            private$.noticeList <- list()
            self$results$todo$setContent("")

            # ── 1. Extract and validate data ──────────────────────────
            prepared <- tryCatch(private$.prepareData(), error = function(e) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-danger'><h4>", .("Data Error"),
                    "</h4><p>", e$message, "</p></div>"))
                return(NULL)
            })
            if (is.null(prepared)) return()

            private$.checkpoint()

            # ── 2. Compute equations ──────────────────────────────────
            results <- private$.computeEquations(prepared)

            # ── 3. Populate results ───────────────────────────────────
            private$.populateSummary(results)
            private$.populateRiskTable(results)

            private$.checkpoint()

            if (isTRUE(self$options$showIndividual)) {
                private$.populateIndividual(results)
            }

            if (isTRUE(self$options$showConcordance) && !is.null(self$options$actualRS)) {
                private$.populateConcordance(prepared, results)
            }

            private$.addNotice("INFO", .("Analysis Complete"),
                sprintf(.("Magee Equations computed for %d patients. Risk cutoffs: Low <= %d, High >= %d (TAILORx)."),
                    prepared$n, self$options$lowCutoff, self$options$highCutoff))
            private$.renderNotices()
        },

        .showEquationInfo = function() {
            html <- paste0(
                "<h4>", .("Magee Equations (Klein et al. 2013)"), "</h4>",
                "<table class='table table-condensed'>",
                "<tr><td><strong>", .("Original"), "</strong></td>",
                "<td>RS = 13.424 + 5.420*NucGrade + 5.538*Mitosis - 0.045*ER_H - 0.030*PR_H + 9.486*HER2pos</td></tr>",
                "<tr><td><strong>", .("Equation 1"), "</strong></td>",
                "<td>RS = 15.314 + 1.406*Nottingham - 0.019*ER_H - 0.029*PR_H + 0.787*TumorSize + 0.133*Ki67 + HER2_terms</td></tr>",
                "<tr><td><strong>", .("Equation 2"), "</strong></td>",
                "<td>RS = 18.804 + 2.341*Nottingham - 0.037*ER_H - 0.031*PR_H + 0.043*TumorSize + HER2_terms</td></tr>",
                "<tr><td><strong>", .("Equation 3"), "</strong></td>",
                "<td>RS = 24.308 - 0.022*ER_H - 0.029*PR_H + 0.186*Ki67 + HER2_terms</td></tr>",
                "</table>",
                "<p><em>", .("HER2_terms: additional coefficients for HER2 equivocal/positive status. ER_H and PR_H are H-scores (0-300)."), "</em></p>")
            self$results$equationInfo$setContent(html)
        },

        .prepareData = function() {
            data <- self$data

            nuc_grade <- jmvcore::toNumeric(data[[self$options$nuclearGrade]])
            mitosis <- jmvcore::toNumeric(data[[self$options$mitosis]])
            er_val <- jmvcore::toNumeric(data[[self$options$erHscore]])
            pr_val <- jmvcore::toNumeric(data[[self$options$prHscore]])

            # Convert percentage to H-score if needed (approximate: H = % * 3)
            if (self$options$erFormat == "percentage") {
                er_val <- er_val * 3
                private$.addNotice("INFO", .("ER Conversion"),
                    .("ER percentage converted to approximate H-score (H = % x 3). For best accuracy, provide actual H-score values."))
            }
            if (self$options$prFormat == "percentage") {
                pr_val <- pr_val * 3
                private$.addNotice("INFO", .("PR Conversion"),
                    .("PR percentage converted to approximate H-score (H = % x 3). For best accuracy, provide actual H-score values."))
            }

            # HER2 status
            her2_raw <- data[[self$options$her2Status]]
            her2_pos_level <- as.character(self$options$her2Positive)
            her2_pos <- as.integer(as.character(her2_raw) == her2_pos_level)

            # Optional variables
            nottingham <- NULL
            if (!is.null(self$options$nottinghamScore))
                nottingham <- jmvcore::toNumeric(data[[self$options$nottinghamScore]])

            tumor_size <- NULL
            if (!is.null(self$options$tumorSize))
                tumor_size <- jmvcore::toNumeric(data[[self$options$tumorSize]])

            ki67 <- NULL
            if (!is.null(self$options$ki67))
                ki67 <- jmvcore::toNumeric(data[[self$options$ki67]])

            actual_rs <- NULL
            if (!is.null(self$options$actualRS))
                actual_rs <- jmvcore::toNumeric(data[[self$options$actualRS]])

            # Complete cases for required variables
            complete <- complete.cases(nuc_grade, mitosis, er_val, pr_val, her2_pos)
            n <- sum(complete)
            if (n < 1) stop(.("No complete cases for the required variables."))

            # Validation
            if (any(nuc_grade[complete] < 1 | nuc_grade[complete] > 3, na.rm = TRUE))
                private$.addNotice("WARNING", .("Nuclear Grade Range"),
                    .("Nuclear grade values outside expected range (1-3) detected."))
            if (any(er_val[complete] < 0 | er_val[complete] > 300, na.rm = TRUE))
                private$.addNotice("WARNING", .("ER H-Score Range"),
                    .("ER H-score values outside expected range (0-300) detected."))

            list(
                nuc_grade = nuc_grade, mitosis = mitosis,
                er_h = er_val, pr_h = pr_val,
                her2_pos = her2_pos,
                nottingham = nottingham, tumor_size = tumor_size,
                ki67 = ki67, actual_rs = actual_rs,
                complete = complete, n = n
            )
        },

        .computeEquations = function(prepared) {
            n <- length(prepared$nuc_grade)
            equations <- self$options$equations

            # Initialize result vectors
            rs_original <- rep(NA_real_, n)
            rs_new1 <- rep(NA_real_, n)
            rs_new2 <- rep(NA_real_, n)
            rs_new3 <- rep(NA_real_, n)

            # Original Equation: RS = 13.424 + 5.420*NucGrade + 5.538*Mitosis
            #                        - 0.045*ER_H - 0.030*PR_H + 9.486*HER2pos
            if (equations %in% c("all", "original")) {
                rs_original <- 13.424 +
                    5.420 * prepared$nuc_grade +
                    5.538 * prepared$mitosis -
                    0.045 * prepared$er_h -
                    0.030 * prepared$pr_h +
                    9.486 * prepared$her2_pos
            }

            # New Equation 1: RS = 15.314 + 1.406*Nottingham - 0.019*ER_H
            #                    - 0.029*PR_H + 0.787*TumorSize + 0.133*Ki67
            #                    + HER2_terms
            if (equations %in% c("all", "new_only")) {
                if (!is.null(prepared$nottingham) &&
                    !is.null(prepared$tumor_size) &&
                    !is.null(prepared$ki67)) {
                    rs_new1 <- 15.314 +
                        1.406 * prepared$nottingham -
                        0.019 * prepared$er_h -
                        0.029 * prepared$pr_h +
                        0.787 * prepared$tumor_size +
                        0.133 * prepared$ki67 +
                        9.486 * prepared$her2_pos  # HER2 term
                } else if (equations == "new_only") {
                    private$.addNotice("WARNING", .("Equation 1 Unavailable"),
                        .("Equation 1 requires Nottingham score, tumor size, and Ki-67. Provide all three to compute."))
                }

                # New Equation 2: RS = 18.804 + 2.341*Nottingham - 0.037*ER_H
                #                    - 0.031*PR_H + 0.043*TumorSize + HER2_terms
                if (!is.null(prepared$nottingham) &&
                    !is.null(prepared$tumor_size)) {
                    rs_new2 <- 18.804 +
                        2.341 * prepared$nottingham -
                        0.037 * prepared$er_h -
                        0.031 * prepared$pr_h +
                        0.043 * prepared$tumor_size +
                        9.486 * prepared$her2_pos  # HER2 term
                } else if (equations == "new_only") {
                    private$.addNotice("WARNING", .("Equation 2 Unavailable"),
                        .("Equation 2 requires Nottingham score and tumor size."))
                }

                # New Equation 3: RS = 24.308 - 0.022*ER_H - 0.029*PR_H
                #                    + 0.186*Ki67 + HER2_terms
                if (!is.null(prepared$ki67)) {
                    rs_new3 <- 24.308 -
                        0.022 * prepared$er_h -
                        0.029 * prepared$pr_h +
                        0.186 * prepared$ki67 +
                        9.486 * prepared$her2_pos  # HER2 term
                } else if (equations == "new_only") {
                    private$.addNotice("WARNING", .("Equation 3 Unavailable"),
                        .("Equation 3 requires Ki-67 index."))
                }
            }

            list(
                rs_original = rs_original, rs_new1 = rs_new1,
                rs_new2 = rs_new2, rs_new3 = rs_new3,
                complete = prepared$complete, n = prepared$n
            )
        },

        .classifyRisk = function(rs) {
            low <- self$options$lowCutoff
            high <- self$options$highCutoff
            ifelse(is.na(rs), NA_character_,
                   ifelse(rs <= low, "Low",
                          ifelse(rs >= high, "High", "Intermediate")))
        },

        .populateSummary = function(results) {
            table <- self$results$summaryTable
            eq_names <- c("Original", "New Eq. 1", "New Eq. 2", "New Eq. 3")
            eq_data <- list(results$rs_original, results$rs_new1,
                            results$rs_new2, results$rs_new3)

            for (i in seq_along(eq_names)) {
                vals <- eq_data[[i]][results$complete]
                if (all(is.na(vals))) next
                vals <- vals[!is.na(vals)]
                table$addRow(rowKey = i, values = list(
                    equation = eq_names[i], n = length(vals),
                    mean_rs = mean(vals), median_rs = median(vals),
                    sd_rs = sd(vals), min_rs = min(vals), max_rs = max(vals)))
            }
        },

        .populateRiskTable = function(results) {
            table <- self$results$riskTable
            eq_names <- c("Original", "New Eq. 1", "New Eq. 2", "New Eq. 3")
            eq_data <- list(results$rs_original, results$rs_new1,
                            results$rs_new2, results$rs_new3)

            for (i in seq_along(eq_names)) {
                vals <- eq_data[[i]][results$complete]
                if (all(is.na(vals))) next
                risk <- private$.classifyRisk(vals)
                risk <- risk[!is.na(risk)]
                n_total <- length(risk)
                n_low <- sum(risk == "Low")
                n_int <- sum(risk == "Intermediate")
                n_high <- sum(risk == "High")

                table$addRow(rowKey = i, values = list(
                    equation = eq_names[i],
                    low_n = n_low, low_pct = 100 * n_low / n_total,
                    int_n = n_int, int_pct = 100 * n_int / n_total,
                    high_n = n_high, high_pct = 100 * n_high / n_total))
            }

            table$setNote("cutoffs",
                sprintf(.("Risk cutoffs: Low <= %d, Intermediate %d-%d, High >= %d"),
                    self$options$lowCutoff, self$options$lowCutoff + 1,
                    self$options$highCutoff - 1, self$options$highCutoff))
        },

        .populateIndividual = function(results) {
            table <- self$results$individualTable
            idx <- which(results$complete)
            # Limit to first 200 rows for performance
            show_n <- min(length(idx), 200)

            for (j in seq_len(show_n)) {
                i <- idx[j]
                risk <- private$.classifyRisk(results$rs_original[i])
                table$addRow(rowKey = j, values = list(
                    row_id = i,
                    rs_original = results$rs_original[i],
                    rs_new1 = results$rs_new1[i],
                    rs_new2 = results$rs_new2[i],
                    rs_new3 = results$rs_new3[i],
                    risk_original = if (is.na(risk)) "" else risk))
            }

            if (length(idx) > 200) {
                table$setNote("truncated",
                    sprintf(.("Showing first 200 of %d patients."), length(idx)))
            }
        },

        .populateConcordance = function(prepared, results) {
            table <- self$results$concordanceTable
            actual <- prepared$actual_rs[prepared$complete]
            eq_names <- c("Original", "New Eq. 1", "New Eq. 2", "New Eq. 3")
            eq_data <- list(results$rs_original, results$rs_new1,
                            results$rs_new2, results$rs_new3)

            actual_risk <- private$.classifyRisk(actual)

            for (i in seq_along(eq_names)) {
                estimated <- eq_data[[i]][results$complete]
                valid <- !is.na(estimated) & !is.na(actual)
                if (sum(valid) < 3) next

                est_v <- estimated[valid]; act_v <- actual[valid]
                corr_p <- cor(est_v, act_v, use = "complete.obs", method = "pearson")
                corr_s <- cor(est_v, act_v, use = "complete.obs", method = "spearman")
                rmse <- sqrt(mean((est_v - act_v)^2))
                mae <- mean(abs(est_v - act_v))

                est_risk <- private$.classifyRisk(est_v)
                act_risk <- actual_risk[valid]
                risk_agree <- mean(est_risk == act_risk, na.rm = TRUE)

                table$addRow(rowKey = i, values = list(
                    equation = eq_names[i], correlation = corr_p,
                    spearman = corr_s, rmse = rmse, mae = mae,
                    risk_concordance = risk_agree))
            }
        },

        .distributionPlot = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$nuclearGrade)) return(FALSE)

            prepared <- tryCatch(private$.prepareData(), error = function(e) NULL)
            if (is.null(prepared)) return(FALSE)
            results <- private$.computeEquations(prepared)

            # Build long-format data for plotting
            eq_names <- c("Original", "Eq. 1", "Eq. 2", "Eq. 3")
            eq_data <- list(results$rs_original, results$rs_new1,
                            results$rs_new2, results$rs_new3)

            plot_df <- do.call(rbind, lapply(seq_along(eq_names), function(i) {
                vals <- eq_data[[i]][results$complete]
                vals <- vals[!is.na(vals)]
                if (length(vals) == 0) return(NULL)
                data.frame(equation = eq_names[i], rs = vals)
            }))

            if (is.null(plot_df) || nrow(plot_df) == 0) return(FALSE)

            low <- self$options$lowCutoff
            high <- self$options$highCutoff

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = rs, fill = equation)) +
                ggplot2::geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
                ggplot2::geom_vline(xintercept = low, linetype = "dashed", color = "#27AE60", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = high, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
                ggplot2::annotate("text", x = low, y = Inf, vjust = 2,
                    label = sprintf("Low <= %d", low), color = "#27AE60", size = 3) +
                ggplot2::annotate("text", x = high, y = Inf, vjust = 2,
                    label = sprintf("High >= %d", high), color = "#E74C3C", size = 3) +
                ggplot2::facet_wrap(~ equation, scales = "free_y") +
                ggplot2::labs(title = .("Estimated Oncotype DX Recurrence Score Distribution"),
                    x = .("Estimated RS"), y = .("Count")) +
                ggplot2::guides(fill = "none") +
                ggtheme
            print(p)
            TRUE
        },

        .concordancePlot = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$actualRS) || is.null(self$options$nuclearGrade)) return(FALSE)

            prepared <- tryCatch(private$.prepareData(), error = function(e) NULL)
            if (is.null(prepared)) return(FALSE)
            results <- private$.computeEquations(prepared)

            actual <- prepared$actual_rs[prepared$complete]
            original <- results$rs_original[prepared$complete]
            valid <- !is.na(actual) & !is.na(original)
            if (sum(valid) < 3) return(FALSE)

            df <- data.frame(actual = actual[valid], estimated = original[valid])

            p <- ggplot2::ggplot(df, ggplot2::aes(x = actual, y = estimated)) +
                ggplot2::geom_point(alpha = 0.5, size = 2) +
                ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#2E86C1", fill = "#D6EAF8") +
                ggplot2::labs(title = .("Concordance: Estimated vs Actual Oncotype DX RS"),
                    x = .("Actual RS"), y = .("Estimated RS (Original Eq.)")) +
                ggtheme
            print(p)
            TRUE
        }
    )
)
