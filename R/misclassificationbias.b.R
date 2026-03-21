#' @title Misclassification Bias Sensitivity Analysis
#' @description
#' Assesses the impact of non-differential or differential misclassification
#' of an exposure/classifier on observed effect estimates (OR, RR, RD).
#' Uses matrix inversion correction and Monte Carlo simulation for CIs.
#'
#' @importFrom R6 R6Class
#' @import jmvcore

misclassificationbiasClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "misclassificationbiasClass",
    inherit = misclassificationbiasBase,
    private = list(

        .init = function() {
            if (is.null(self$options$outcome) || is.null(self$options$exposure)) {
                self$results$todo$setContent(paste0(
                    "<div class='alert alert-info'>",
                    "<h4>Misclassification Bias Sensitivity Analysis</h4>",
                    "<p>Assess how classification errors (e.g., Ki-67 visual estimation, ",
                    "morphologic grading) affect your observed effect estimates.</p>",
                    "<h5>Required inputs:</h5>",
                    "<ul>",
                    "<li><strong>Outcome</strong>: Binary outcome (e.g., recurrence, death)</li>",
                    "<li><strong>Exposure</strong>: Binary classifier subject to misclassification</li>",
                    "<li><strong>Sensitivity/Specificity</strong>: Known or estimated error rates of the classifier</li>",
                    "</ul>",
                    "<p><em>Example: If Ki-67 visual estimation has sensitivity=0.80 and specificity=0.85 ",
                    "for detecting high-grade tumors, how much does this bias the odds ratio?</em></p>",
                    "</div>"))
                return()
            }
        },

        .run = function() {
            if (is.null(self$options$outcome) || is.null(self$options$exposure)) return()

            set.seed(self$options$random_seed)

            # ── 1. Build observed 2×2 table ────────────────────────────────
            data <- private$.buildTable()
            if (is.null(data)) return()

            self$results$todo$setContent("")

            # ── 2. Populate observed table ─────────────────────────────────
            private$.populateObservedTable(data)

            # ── 3. Correct for misclassification ──────────────────────────
            corrected <- private$.correctMisclassification(data)
            if (is.null(corrected)) return()

            private$.populateCorrectedTable(corrected)

            private$.checkpoint()

            # ── 4. Compute effect estimates ────────────────────────────────
            private$.populateBiasAnalysis(data, corrected)

            # ── 5. Range analysis ──────────────────────────────────────────
            if (self$options$rangeAnalysis) {
                private$.populateRangeAnalysis(data)
            }

            # ── 6. Interpretation ──────────────────────────────────────────
            private$.populateInterpretation(data, corrected)
        },

        # ══════════════════════════════════════════════════════════════════
        # Build the observed 2×2 table
        # ══════════════════════════════════════════════════════════════════
        .buildTable = function() {
            outcome_raw <- self$data[[self$options$outcome]]
            exposure_raw <- self$data[[self$options$exposure]]

            outcome_chr <- as.character(outcome_raw)
            exposure_chr <- as.character(exposure_raw)

            # Determine levels
            outcome_levels <- sort(unique(outcome_chr[!is.na(outcome_chr)]))
            exposure_levels <- sort(unique(exposure_chr[!is.na(exposure_chr)]))

            if (length(outcome_levels) < 2 || length(exposure_levels) < 2) {
                self$results$todo$setContent(
                    "<div class='alert alert-danger'>Both outcome and exposure must have at least 2 levels.</div>")
                return(NULL)
            }

            # Resolve event levels
            outcome_event <- as.character(self$options$outcomeLevel)
            if (is.null(outcome_event) || !nzchar(outcome_event)) outcome_event <- outcome_levels[2]
            outcome_ref <- setdiff(outcome_levels, outcome_event)[1]

            exposure_pos <- as.character(self$options$exposureLevel)
            if (is.null(exposure_pos) || !nzchar(exposure_pos)) exposure_pos <- exposure_levels[2]
            exposure_neg <- setdiff(exposure_levels, exposure_pos)[1]

            # Build 2×2: rows = outcome (case/control), cols = exposure (exposed/unexposed)
            complete <- !is.na(outcome_chr) & !is.na(exposure_chr) &
                        (outcome_chr %in% c(outcome_event, outcome_ref)) &
                        (exposure_chr %in% c(exposure_pos, exposure_neg))

            o <- outcome_chr[complete]
            e <- exposure_chr[complete]

            a <- sum(o == outcome_event & e == exposure_pos)   # case & exposed
            b <- sum(o == outcome_event & e == exposure_neg)   # case & unexposed
            c <- sum(o == outcome_ref & e == exposure_pos)     # control & exposed
            d <- sum(o == outcome_ref & e == exposure_neg)     # control & unexposed

            n <- a + b + c + d
            if (n < 10) {
                self$results$todo$setContent(
                    "<div class='alert alert-danger'>Too few complete observations for analysis.</div>")
                return(NULL)
            }

            list(a = a, b = b, c = c, d = d, n = n,
                 outcome_event = outcome_event, outcome_ref = outcome_ref,
                 exposure_pos = exposure_pos, exposure_neg = exposure_neg)
        },

        # ══════════════════════════════════════════════════════════════════
        # Correct for misclassification using matrix inversion
        # ══════════════════════════════════════════════════════════════════
        .correctMisclassification = function(data) {
            if (self$options$misclassType == "nondifferential") {
                sen <- self$options$senExposure
                spec <- self$options$specExposure

                if (sen + spec <= 1) {
                    self$results$todo$setContent(
                        "<div class='alert alert-danger'>Sensitivity + Specificity must be > 1 for correction to be valid.</div>")
                    return(NULL)
                }

                # Cases: correct exposure classification
                n1 <- data$a + data$b  # total cases
                a_adj <- (data$a - n1 * (1 - spec)) / (sen + spec - 1)
                b_adj <- n1 - a_adj

                # Controls: same error rates (non-differential)
                n0 <- data$c + data$d  # total controls
                c_adj <- (data$c - n0 * (1 - spec)) / (sen + spec - 1)
                d_adj <- n0 - c_adj

            } else {
                # Differential: different error rates for cases vs controls
                sen_case <- self$options$senExposureCase
                spec_case <- self$options$specExposureCase
                sen_ctrl <- self$options$senExposureControl
                spec_ctrl <- self$options$specExposureControl

                if (sen_case + spec_case <= 1 || sen_ctrl + spec_ctrl <= 1) {
                    self$results$todo$setContent(
                        "<div class='alert alert-danger'>Sensitivity + Specificity must be > 1 in both groups.</div>")
                    return(NULL)
                }

                n1 <- data$a + data$b
                a_adj <- (data$a - n1 * (1 - spec_case)) / (sen_case + spec_case - 1)
                b_adj <- n1 - a_adj

                n0 <- data$c + data$d
                c_adj <- (data$c - n0 * (1 - spec_ctrl)) / (sen_ctrl + spec_ctrl - 1)
                d_adj <- n0 - c_adj
            }

            # Ensure non-negative (can happen with extreme correction)
            a_adj <- max(a_adj, 0.5)
            b_adj <- max(b_adj, 0.5)
            c_adj <- max(c_adj, 0.5)
            d_adj <- max(d_adj, 0.5)

            list(a = a_adj, b = b_adj, c = c_adj, d = d_adj)
        },

        # ══════════════════════════════════════════════════════════════════
        # Compute effect measures from a 2×2 table
        # ══════════════════════════════════════════════════════════════════
        .computeEffects = function(a, b, c, d) {
            or_val <- (a * d) / (b * c)
            rr_val <- (a / (a + b)) / (c / (c + d))
            rd_val <- a / (a + b) - c / (c + d)
            list(or = or_val, rr = rr_val, rd = rd_val)
        },

        # ══════════════════════════════════════════════════════════════════
        # Monte Carlo simulation for CIs
        # ══════════════════════════════════════════════════════════════════
        .simulateCIs = function(data) {
            B <- self$options$nSimulations
            effects <- numeric(B)
            measure <- self$options$effectMeasure

            for (i in seq_len(B)) {
                # Resample from multinomial
                n <- data$n
                probs <- c(data$a, data$b, data$c, data$d) / n
                counts <- as.numeric(rmultinom(1, n, probs))
                a_sim <- counts[1]; b_sim <- counts[2]
                c_sim <- counts[3]; d_sim <- counts[4]

                # Add 0.5 continuity correction
                a_sim <- a_sim + 0.5; b_sim <- b_sim + 0.5
                c_sim <- c_sim + 0.5; d_sim <- d_sim + 0.5

                # Correct for misclassification
                if (self$options$misclassType == "nondifferential") {
                    sen <- self$options$senExposure
                    spec <- self$options$specExposure
                    n1 <- a_sim + b_sim; n0 <- c_sim + d_sim
                    a_c <- max((a_sim - n1 * (1 - spec)) / (sen + spec - 1), 0.5)
                    b_c <- max(n1 - a_c, 0.5)
                    c_c <- max((c_sim - n0 * (1 - spec)) / (sen + spec - 1), 0.5)
                    d_c <- max(n0 - c_c, 0.5)
                } else {
                    sen1 <- self$options$senExposureCase; spec1 <- self$options$specExposureCase
                    sen0 <- self$options$senExposureControl; spec0 <- self$options$specExposureControl
                    n1 <- a_sim + b_sim; n0 <- c_sim + d_sim
                    a_c <- max((a_sim - n1 * (1 - spec1)) / (sen1 + spec1 - 1), 0.5)
                    b_c <- max(n1 - a_c, 0.5)
                    c_c <- max((c_sim - n0 * (1 - spec0)) / (sen0 + spec0 - 1), 0.5)
                    d_c <- max(n0 - c_c, 0.5)
                }

                eff <- private$.computeEffects(a_c, b_c, c_c, d_c)
                effects[i] <- switch(measure, "or" = eff$or, "rr" = eff$rr, "rd" = eff$rd)
            }

            # For ratio measures, work on log scale
            if (measure %in% c("or", "rr")) {
                log_effects <- log(effects[is.finite(effects) & effects > 0])
                if (length(log_effects) < 100) return(c(NA, NA))
                ci <- exp(quantile(log_effects, c(0.025, 0.975), na.rm = TRUE))
            } else {
                ci <- quantile(effects[is.finite(effects)], c(0.025, 0.975), na.rm = TRUE)
            }
            as.numeric(ci)
        },

        # ══════════════════════════════════════════════════════════════════
        # Populate results
        # ══════════════════════════════════════════════════════════════════
        .populateObservedTable = function(data) {
            table <- self$results$observedTable
            table$addRow(rowKey = 1, values = list(
                label = paste0("Cases (", data$outcome_event, ")"),
                exposed = data$a, unexposed = data$b, total = data$a + data$b))
            table$addRow(rowKey = 2, values = list(
                label = paste0("Controls (", data$outcome_ref, ")"),
                exposed = data$c, unexposed = data$d, total = data$c + data$d))
            table$addRow(rowKey = 3, values = list(
                label = "Total",
                exposed = data$a + data$c, unexposed = data$b + data$d, total = data$n))
        },

        .populateCorrectedTable = function(corrected) {
            table <- self$results$correctedTable
            table$addRow(rowKey = 1, values = list(
                label = "Cases", exposed = corrected$a, unexposed = corrected$b))
            table$addRow(rowKey = 2, values = list(
                label = "Controls", exposed = corrected$c, unexposed = corrected$d))
        },

        .populateBiasAnalysis = function(data, corrected) {
            table <- self$results$biasAnalysis
            measure <- self$options$effectMeasure

            obs <- private$.computeEffects(data$a + 0.5, data$b + 0.5, data$c + 0.5, data$d + 0.5)
            adj <- private$.computeEffects(corrected$a, corrected$b, corrected$c, corrected$d)

            obs_val <- switch(measure, "or" = obs$or, "rr" = obs$rr, "rd" = obs$rd)
            adj_val <- switch(measure, "or" = adj$or, "rr" = adj$rr, "rd" = adj$rd)

            # Bias percentage
            if (measure == "rd") {
                bias_pct <- if (obs_val != 0) ((obs_val - adj_val) / abs(adj_val)) * 100 else NA
            } else {
                bias_pct <- if (adj_val != 0) ((log(obs_val) - log(adj_val)) / abs(log(adj_val))) * 100 else NA
            }

            # Monte Carlo CIs
            ci <- private$.simulateCIs(data)

            measure_label <- switch(measure,
                "or" = "Odds Ratio", "rr" = "Risk Ratio", "rd" = "Risk Difference")

            table$addRow(rowKey = 1, values = list(
                estimate_type = measure_label,
                observed = obs_val,
                adjusted = adj_val,
                ci_lower = ci[1],
                ci_upper = ci[2],
                bias_pct = bias_pct
            ))

            # Direction of bias
            if (measure %in% c("or", "rr")) {
                if (abs(log(obs_val)) > abs(log(adj_val))) {
                    table$setNote("direction", .("Non-differential misclassification biased the effect TOWARD the null (attenuated)."))
                } else {
                    table$setNote("direction", .("Misclassification biased the effect AWAY from the null (exaggerated)."))
                }
            }
        },

        .populateRangeAnalysis = function(data) {
            table <- self$results$rangeTable

            sen_vals <- tryCatch(
                as.numeric(strsplit(self$options$senRange, ",")[[1]]),
                error = function(e) seq(0.7, 0.95, 0.05))
            spec_vals <- tryCatch(
                as.numeric(strsplit(self$options$specRange, ",")[[1]]),
                error = function(e) seq(0.7, 0.95, 0.05))

            sen_vals <- sen_vals[!is.na(sen_vals) & sen_vals > 0 & sen_vals <= 1]
            spec_vals <- spec_vals[!is.na(spec_vals) & spec_vals > 0 & spec_vals <= 1]

            measure <- self$options$effectMeasure
            obs <- private$.computeEffects(data$a + 0.5, data$b + 0.5, data$c + 0.5, data$d + 0.5)
            obs_val <- switch(measure, "or" = obs$or, "rr" = obs$rr, "rd" = obs$rd)

            row_key <- 0
            for (sen in sen_vals) {
                for (spec in spec_vals) {
                    if (sen + spec <= 1) next

                    n1 <- data$a + data$b; n0 <- data$c + data$d
                    a_c <- max((data$a - n1 * (1 - spec)) / (sen + spec - 1), 0.5)
                    b_c <- max(n1 - a_c, 0.5)
                    c_c <- max((data$c - n0 * (1 - spec)) / (sen + spec - 1), 0.5)
                    d_c <- max(n0 - c_c, 0.5)

                    eff <- private$.computeEffects(a_c, b_c, c_c, d_c)
                    adj_val <- switch(measure, "or" = eff$or, "rr" = eff$rr, "rd" = eff$rd)

                    if (measure == "rd") {
                        bias_pct <- if (adj_val != 0) ((obs_val - adj_val) / abs(adj_val)) * 100 else NA
                    } else {
                        bias_pct <- if (adj_val > 0) ((log(obs_val) - log(adj_val)) / abs(log(adj_val))) * 100 else NA
                    }

                    row_key <- row_key + 1
                    table$addRow(rowKey = row_key, values = list(
                        sensitivity = sen, specificity = spec,
                        adjusted_effect = adj_val, bias_pct = bias_pct))
                }
            }
        },

        .populateInterpretation = function(data, corrected) {
            measure <- self$options$effectMeasure
            obs <- private$.computeEffects(data$a + 0.5, data$b + 0.5, data$c + 0.5, data$d + 0.5)
            adj <- private$.computeEffects(corrected$a, corrected$b, corrected$c, corrected$d)

            measure_label <- switch(measure,
                "or" = "odds ratio", "rr" = "risk ratio", "rd" = "risk difference")
            obs_val <- switch(measure, "or" = obs$or, "rr" = obs$rr, "rd" = obs$rd)
            adj_val <- switch(measure, "or" = adj$or, "rr" = adj$rr, "rd" = adj$rd)

            sen_used <- if (self$options$misclassType == "nondifferential")
                self$options$senExposure else
                paste0(self$options$senExposureCase, "/", self$options$senExposureControl)
            spec_used <- if (self$options$misclassType == "nondifferential")
                self$options$specExposure else
                paste0(self$options$specExposureCase, "/", self$options$specExposureControl)

            html <- paste0(
                "<h4>Interpretation</h4>",
                "<p>Given a classifier with sensitivity = ", sen_used,
                " and specificity = ", spec_used, " (",
                self$options$misclassType, " misclassification):</p>",
                "<ul>",
                "<li>The <strong>observed ", measure_label, "</strong> is ",
                sprintf("%.3f", obs_val), "</li>",
                "<li>The <strong>bias-adjusted ", measure_label, "</strong> is ",
                sprintf("%.3f", adj_val), "</li>",
                "</ul>",
                "<h5>Clinical Significance</h5>"
            )

            if (measure %in% c("or", "rr")) {
                if (abs(log(adj_val)) > abs(log(obs_val))) {
                    html <- paste0(html,
                        "<p>The true effect is <strong>stronger</strong> than observed. ",
                        "Non-differential misclassification attenuated the association toward the null. ",
                        "The observed result underestimates the true relationship.</p>")
                } else {
                    html <- paste0(html,
                        "<p>The bias-adjusted effect is similar to or weaker than observed, ",
                        "suggesting the classification error has modest impact.</p>")
                }
            }

            html <- paste0(html,
                "<h5>Key Assumptions</h5>",
                "<ul>",
                "<li>The specified sensitivity and specificity are correct (or plausible ranges are tested)</li>",
                "<li>Misclassification is independent of the outcome (",
                if (self$options$misclassType == "nondifferential") "assumed" else "not assumed", ")</li>",
                "<li>No other sources of bias (confounding, selection) are present</li>",
                "</ul>",
                "<p><em>Reference: Lash TL, Fox MP, Fink AK. Applying Quantitative Bias Analysis ",
                "to Epidemiologic Data. Springer, 2009.</em></p>")

            self$results$interpretation$setContent(html)
        },

        # ══════════════════════════════════════════════════════════════════
        # Range plot
        # ══════════════════════════════════════════════════════════════════
        .rangePlot = function(image, ggtheme, theme, ...) {
            if (is.null(self$options$outcome) || is.null(self$options$exposure)) return(FALSE)

            data <- private$.buildTable()
            if (is.null(data)) return(FALSE)

            sen_vals <- tryCatch(
                as.numeric(strsplit(self$options$senRange, ",")[[1]]),
                error = function(e) seq(0.7, 0.95, 0.05))
            spec_vals <- tryCatch(
                as.numeric(strsplit(self$options$specRange, ",")[[1]]),
                error = function(e) seq(0.7, 0.95, 0.05))
            sen_vals <- sen_vals[!is.na(sen_vals) & sen_vals > 0 & sen_vals <= 1]
            spec_vals <- spec_vals[!is.na(spec_vals) & spec_vals > 0 & spec_vals <= 1]

            measure <- self$options$effectMeasure
            measure_label <- switch(measure,
                "or" = "Odds Ratio", "rr" = "Risk Ratio", "rd" = "Risk Difference")

            results <- expand.grid(sen = sen_vals, spec = spec_vals)
            results$effect <- NA

            for (i in seq_len(nrow(results))) {
                sen <- results$sen[i]; spec <- results$spec[i]
                if (sen + spec <= 1) next
                n1 <- data$a + data$b; n0 <- data$c + data$d
                a_c <- max((data$a - n1 * (1 - spec)) / (sen + spec - 1), 0.5)
                b_c <- max(n1 - a_c, 0.5)
                c_c <- max((data$c - n0 * (1 - spec)) / (sen + spec - 1), 0.5)
                d_c <- max(n0 - c_c, 0.5)
                eff <- private$.computeEffects(a_c, b_c, c_c, d_c)
                results$effect[i] <- switch(measure, "or" = eff$or, "rr" = eff$rr, "rd" = eff$rd)
            }

            results <- results[!is.na(results$effect), ]

            if (requireNamespace("ggplot2", quietly = TRUE)) {
                p <- ggplot2::ggplot(results, ggplot2::aes(x = sen, y = spec, fill = effect)) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_gradient2(
                        low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                        midpoint = if (measure == "rd") 0 else 1,
                        name = measure_label) +
                    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", effect)), size = 3) +
                    ggplot2::labs(title = paste0("Bias-Adjusted ", measure_label,
                                                " by Classifier Performance"),
                                 x = "Classifier Sensitivity", y = "Classifier Specificity") +
                    ggtheme
                print(p)
            } else {
                plot(results$sen, results$effect, type = "p",
                     xlab = "Sensitivity", ylab = measure_label,
                     main = "Sensitivity Range Analysis")
            }
            TRUE
        }
    )
)
