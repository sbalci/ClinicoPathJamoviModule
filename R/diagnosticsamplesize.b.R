diagnosticsamplesizeClass <- R6::R6Class(
    "diagnosticsamplesizeClass",
    inherit = diagnosticsamplesizeBase,
    private = list(
        .results_cache = NULL,
        .planningLabels = function() {
            mode <- self$options$referenceType
            if (is.null(mode)) mode <- "disease"
            agreement <- mode %in% c("consensus", "method")
            list(positive = if (agreement) "Positive percent agreement (PPA)" else "Sensitivity",
                 negative = if (agreement) "Negative percent agreement (NPA)" else "Specificity",
                 prevalence = if (agreement) "Reference-positive proportion" else "Disease prevalence",
                 positive_group = if (agreement) "reference-positive participants" else "diseased participants",
                 negative_group = if (agreement) "reference-negative participants" else "nondiseased participants",
                 comparator = switch(mode, consensus = "Expert consensus", method = "Comparison method",
                                     "Disease reference standard"),
                 agreement = agreement)
        },
        .init = function() {
            if (self$options$show_methodology) private$.setMethodology()
            if (self$options$show_references) private$.setReferences()
        },
        .run = function() {
            self$results$zeroMissPlan$deleteRows()
            if (self$options$zeroMissPlanning) {
                lower <- self$options$minimumSensitivityBound
                alpha <- self$options$alpha
                positives <- ceiling(log(alpha / 2) / log(lower))
                self$results$zeroMissPlan$addRow(rowKey = "plan", values = list(
                    positives = positives, lower = (alpha / 2)^(1 / positives),
                    total = ceiling(positives / self$options$prevalence)))
                self$results$zeroMissPlan$setNote("assumption", "Two-sided exact binomial lower bound conditional on zero misses among independent positive patients. Multiple slides from one patient do not count as independent positives. Recruitment is an expectation, not a guarantee of obtaining the required positives; this is not a power or assurance calculation.")
            }
            tryCatch(
                {
                    labels <- private$.planningLabels()
                    for (name in c("sample_size_summary", "prevalence_table", "method_comparison"))
                        self$results[[name]]$deleteRows()
                    self$results$sample_size_statement$setContent("")
                    if (self$options$show_methodology) private$.setMethodology()
                    # Get parameters
                    sens <- self$options$target_sensitivity
                    spec <- self$options$target_specificity
                    prev <- self$options$prevalence
                    width <- self$options$ci_width
                    alpha <- self$options$alpha
                    nonresp <- self$options$nonresponse_rate / 100
                    purpose <- self$options$study_purpose

                    # Calculate sample sizes for sensitivity
                    n_sens <- private$.calculate_n_for_ci_width(
                        p = sens,
                        width = width,
                        alpha = alpha
                    )
                    n_sens_total <- ceiling(n_sens / prev)

                    # Calculate sample sizes for specificity
                    n_spec <- private$.calculate_n_for_ci_width(
                        p = spec,
                        width = width,
                        alpha = alpha
                    )
                    n_spec_total <- ceiling(n_spec / (1 - prev))

                    # Determine required N based on study purpose
                    if (purpose == "diagnostic") {
                        # Both sens and spec need to be high - take maximum
                        n_required <- max(n_sens_total, n_spec_total)
                        limiting_factor <- if (n_sens_total > n_spec_total) labels$positive else labels$negative
                    } else if (purpose == "screening_sens") {
                        # Emphasize sensitivity
                        n_required <- n_sens_total
                        limiting_factor <- labels$positive
                    } else {
                        # Emphasize specificity
                        n_required <- n_spec_total
                        limiting_factor <- labels$negative
                    }

                    # Adjust for non-response
                    n_final <- if (nonresp > 0) {
                        ceiling(n_required / (1 - nonresp))
                    } else {
                        n_required
                    }

                    # Cache results
                    private$.results_cache <- list(
                        n_sens = n_sens,
                        n_sens_total = n_sens_total,
                        n_spec = n_spec,
                        n_spec_total = n_spec_total,
                        n_required = n_required,
                        n_final = n_final,
                        limiting_factor = limiting_factor,
                        sens = sens,
                        spec = spec,
                        prev = prev,
                        width = width,
                        alpha = alpha,
                        nonresp = nonresp,
                        purpose = purpose
                    )

                    # Populate tables
                    private$.populateTables()

                    # Generate statement
                    if (self$options$show_statement) {
                        private$.generateStatement()
                    }

                    # Generate comparison table if requested
                    if (self$options$show_comparison_table) {
                        private$.generateComparisonTable()
                    }

                    # Generate method comparison if requested
                    if (self$options$show_method_comparison) {
                        private$.generateMethodComparison()
                    }
                },
                error = function(e) {
                    self$setError(paste("Error:", e$message))
                }
            )
        },
        .calculate_n_for_ci_width = function(p, width, alpha) {
            # Binary search for minimum N such that Clopper-Pearson CI width <= target
            # Using exact binomial confidence interval (Clopper-Pearson method)

            conf_level <- 1 - alpha

            # Start with a reasonable initial range
            n_min <- 10
            n_max <- 100000

            # Binary search
            while (n_max - n_min > 1) {
                n <- ceiling((n_min + n_max) / 2)
                x <- round(n * p) # Expected number of successes

                # Calculate Clopper-Pearson CI
                ci <- private$.clopper_pearson_ci(x, n, conf_level)
                ci_width <- ci$upper - ci$lower

                if (ci_width <= width) {
                    n_max <- n
                } else {
                    n_min <- n
                }
            }

            # Verify the final result
            x_final <- round(n_max * p)
            ci_final <- private$.clopper_pearson_ci(x_final, n_max, conf_level)
            ci_width_final <- ci_final$upper - ci_final$lower

            if (ci_width_final > width) {
                # Need to search higher
                for (n in n_max:(n_max + 1000)) {
                    x <- round(n * p)
                    ci <- private$.clopper_pearson_ci(x, n, conf_level)
                    if ((ci$upper - ci$lower) <= width) {
                        return(n)
                    }
                }
            }

            return(n_max)
        },
        .clopper_pearson_ci = function(x, n, conf_level) {
            # Clopper-Pearson exact binomial confidence interval
            # Based on beta distribution quantiles

            alpha <- 1 - conf_level

            if (x == 0) {
                lower <- 0
                upper <- 1 - (alpha / 2)^(1 / n)
            } else if (x == n) {
                lower <- (alpha / 2)^(1 / n)
                upper <- 1
            } else {
                # Use beta distribution
                lower <- qbeta(alpha / 2, x, n - x + 1)
                upper <- qbeta(1 - alpha / 2, x + 1, n - x)
            }

            list(lower = lower, upper = upper)
        },
        .populateTables = function() {
            res <- private$.results_cache

            labels <- private$.planningLabels()
            summary_table <- self$results$sample_size_summary
            summary_table$setNote("population", paste("Comparator:", labels$comparator,
                ". Independent participants, one paired test/reference result per participant.",
                "This is single-proportion precision planning, not reader-study power planning."))
            self$results$prevalence_table$getColumn("prevalence")$setTitle(labels$prevalence)
            self$results$prevalence_table$getColumn("n_sens_total")$setTitle(paste("N for", labels$positive))
            self$results$prevalence_table$getColumn("n_spec_total")$setTitle(paste("N for", labels$negative))
            self$results$method_comparison$getColumn("n_sensitivity")$setTitle(paste("N for", labels$positive))
            self$results$method_comparison$getColumn("n_specificity")$setTitle(paste("N for", labels$negative))

            # Row 1: Sensitivity
            summary_table$addRow(rowKey = 1, values = list(
                parameter = labels$positive,
                target_value = res$sens,
                n_cases = res$n_sens,
                n_total = res$n_sens_total
            ))

            # Row 2: Specificity
            summary_table$addRow(rowKey = 2, values = list(
                parameter = labels$negative,
                target_value = res$spec,
                n_cases = res$n_spec,
                n_total = res$n_spec_total
            ))

            # Row 3: Final required
            summary_table$addRow(rowKey = 3, values = list(
                parameter = paste0("Required N (", res$limiting_factor, ")"),
                target_value = NA,
                n_cases = NA,
                n_total = res$n_required
            ))

            # Row 4: Adjusted if applicable
            if (res$nonresp > 0) {
                summary_table$addRow(rowKey = 4, values = list(
                    parameter = sprintf("Adjusted for %.0f%% Non-Response", res$nonresp * 100),
                    target_value = NA,
                    n_cases = NA,
                    n_total = res$n_final
                ))
            }
        },
        .generateStatement = function() {
            res <- private$.results_cache
            labels <- private$.planningLabels()
            html <- sprintf(paste0(
                "<h3>Precision-based sample size statement</h3>",
                "<p>Comparator: %s. Plan to estimate %s at %.1f%% and %s at %.1f%%, ",
                "using %.1f%% Clopper-Pearson intervals with total width at most %.3f ",
                "at the rounded expected success count.</p>",
                "<p>%s: %.1f%%. The positive-reference stratum requires %d %s ",
                "(%d total participants); the negative-reference stratum requires %d %s ",
                "(%d total participants). The selected objective requires %d participants; ",
                "after %.1f%% incomplete-result allowance, recruit %d.</p>",
                "<p>These are independent-participant calculations. Dividing stratum requirements ",
                "by the expected reference proportions does not guarantee the realized stratum sizes ",
                "or confidence-interval widths. Clustered or multireader studies require separate planning.</p>",
                "%s"), labels$comparator, labels$positive, 100 * res$sens,
                labels$negative, 100 * res$spec, 100 * (1 - res$alpha), res$width,
                labels$prevalence, 100 * res$prev, res$n_sens, labels$positive_group,
                res$n_sens_total, res$n_spec, labels$negative_group, res$n_spec_total,
                res$n_required, 100 * res$nonresp, res$n_final,
                if (labels$agreement) paste0("<p>PPA and NPA describe agreement with the declared ",
                    "comparator; they do not establish disease sensitivity or specificity.</p>") else "")
            self$results$sample_size_statement$setContent(html)
        },
        .generateComparisonTable = function() {
            # Parse prevalence values
            prev_string <- self$options$comparison_prevalences
            prev_values <- as.numeric(unlist(strsplit(prev_string, ",")))
            if (any(!is.finite(prev_values) | prev_values <= 0 | prev_values >= 1))
                stop("Comparison reference proportions must be numbers strictly between 0 and 1.")

            if (length(prev_values) == 0) {
                return()
            }

            res <- private$.results_cache
            table <- self$results$prevalence_table

            for (i in seq_along(prev_values)) {
                prev <- prev_values[i]

                # Calculate for this prevalence
                n_sens <- private$.calculate_n_for_ci_width(
                    p = res$sens,
                    width = res$width,
                    alpha = res$alpha
                )
                n_sens_total <- ceiling(n_sens / prev)

                n_spec <- private$.calculate_n_for_ci_width(
                    p = res$spec,
                    width = res$width,
                    alpha = res$alpha
                )
                n_spec_total <- ceiling(n_spec / (1 - prev))

                n_required <- max(n_sens_total, n_spec_total)

                table$addRow(rowKey = i, values = list(
                    prevalence = prev,
                    n_sens_total = n_sens_total,
                    n_spec_total = n_spec_total,
                    n_required = n_required
                ))
            }
        },
        .generateMethodComparison = function() {
            res <- private$.results_cache
            table <- self$results$method_comparison

            # List of methods to compare
            methods <- list(
                list(name = "Clopper-Pearson (Exact)", fn = "clopper_pearson"),
                list(name = "Wilson", fn = "wilson"),
                list(name = "Agresti-Coull", fn = "agresti_coull"),
                list(name = "Normal Approximation (Wald)", fn = "wald")
            )

            # Store sample sizes for comparison
            n_values <- list()

            for (i in seq_along(methods)) {
                method <- methods[[i]]

                # Calculate N for sensitivity
                n_sens <- private$.calculate_n_for_method(
                    p = res$sens,
                    width = res$width,
                    alpha = res$alpha,
                    method = method$fn
                )
                n_sens_total <- ceiling(n_sens / res$prev)

                # Calculate N for specificity
                n_spec <- private$.calculate_n_for_method(
                    p = res$spec,
                    width = res$width,
                    alpha = res$alpha,
                    method = method$fn
                )
                n_spec_total <- ceiling(n_spec / (1 - res$prev))

                # Total required
                n_total <- max(n_sens_total, n_spec_total)
                n_values[[i]] <- n_total

                # Calculate relative efficiency (compared to Clopper-Pearson)
                if (i == 1) {
                    rel_eff <- 100 # Reference method
                } else {
                    rel_eff <- (n_values[[1]] / n_total) * 100
                }

                table$addRow(rowKey = i, values = list(
                    method = method$name,
                    n_sensitivity = n_sens_total,
                    n_specificity = n_spec_total,
                    n_total = n_total,
                    relative_efficiency = rel_eff
                ))
            }
        },
        .calculate_n_for_method = function(p, width, alpha, method) {
            # Calculate minimum N for specified CI method to achieve target width
            conf_level <- 1 - alpha

            # Binary search
            n_min <- 10
            n_max <- 100000

            while (n_max - n_min > 1) {
                n <- ceiling((n_min + n_max) / 2)
                x <- round(n * p)

                # Calculate CI using specified method
                ci <- private$.calculate_ci_method(x, n, conf_level, method)
                ci_width <- ci$upper - ci$lower

                if (ci_width <= width) {
                    n_max <- n
                } else {
                    n_min <- n
                }
            }

            return(n_max)
        },
        .calculate_ci_method = function(x, n, conf_level, method) {
            if (method == "clopper_pearson") {
                return(private$.clopper_pearson_ci(x, n, conf_level))
            } else if (method == "wilson") {
                return(private$.wilson_ci(x, n, conf_level))
            } else if (method == "agresti_coull") {
                return(private$.agresti_coull_ci(x, n, conf_level))
            } else if (method == "wald") {
                return(private$.wald_ci(x, n, conf_level))
            }
        },
        .wilson_ci = function(x, n, conf_level) {
            # Wilson score confidence interval
            alpha <- 1 - conf_level
            z <- qnorm(1 - alpha / 2)
            p_hat <- x / n

            denominator <- 1 + z^2 / n
            center <- (p_hat + z^2 / (2 * n)) / denominator
            margin <- z * sqrt((p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))) / denominator

            list(
                lower = max(0, center - margin),
                upper = min(1, center + margin)
            )
        },
        .agresti_coull_ci = function(x, n, conf_level) {
            # Agresti-Coull confidence interval
            alpha <- 1 - conf_level
            z <- qnorm(1 - alpha / 2)

            # Add z^2/2 to both successes and sample size
            n_tilde <- n + z^2
            x_tilde <- x + z^2 / 2
            p_tilde <- x_tilde / n_tilde

            # Standard Wald on adjusted values
            se <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
            margin <- z * se

            list(
                lower = max(0, p_tilde - margin),
                upper = min(1, p_tilde + margin)
            )
        },
        .wald_ci = function(x, n, conf_level) {
            # Normal approximation (Wald) confidence interval
            alpha <- 1 - conf_level
            z <- qnorm(1 - alpha / 2)
            p_hat <- x / n
            se <- sqrt(p_hat * (1 - p_hat) / n)
            margin <- z * se

            list(
                lower = max(0, p_hat - margin),
                upper = min(1, p_hat + margin)
            )
        },
        .setMethodology = function() {
            labels <- private$.planningLabels()
            self$results$methodology$setContent(paste0(
                "<h3>Single-proportion precision planning</h3><p>The declared comparator is ",
                labels$comparator, ". The two estimands are ", labels$positive, " and ",
                labels$negative, ". Numerators count matching test results within the ",
                "reference-positive and reference-negative strata, respectively.</p>",
                "<p>Planning evaluates Clopper-Pearson interval width at the rounded expected ",
                "number of matching results. The confidence level is 100 &times; (1 &minus; alpha) percent. ",
                "The expected stratum proportion converts each stratum count to a total participant count. ",
                "The selected objective uses both requirements or one specified stratum.</p>",
                "<p>Participants must be independent and contribute one paired result. These calculations ",
                "condition on the reference classification and do not include comparator uncertainty, ",
                "reader variance, patient clustering, assurance of stratum recruitment, or power for a ",
                "difference between tests. Multiple readings do not replace independent participants.</p>"))
        },
        .setReferences = function() {
            html <- "
<h3>References</h3>

<h4>Primary Reference</h4>

<p><strong>Bujang MA (2023).</strong> An Elaboration on Sample Size Planning for Performing a
One-Sample Sensitivity and Specificity Analysis by Basing on Calculations on a Specified 95%
Confidence Interval Width. <em>Diagnostics</em>, 13(8):1390.
<a href='https://doi.org/10.3390/diagnostics13081390' target='_blank'>doi:10.3390/diagnostics13081390</a></p>

<h4>Methodological Foundations</h4>

<p><strong>Clopper CJ, Pearson ES (1934).</strong> The use of confidence or fiducial limits
illustrated in the case of the Binomial. <em>Biometrika</em>, 26(4):404-413.</p>

<p><strong>Buderer NM (1996).</strong> Statistical Methodology: I. Incorporating the prevalence
of disease into the sample size calculation for sensitivity and specificity.
<em>Academic Emergency Medicine</em>, 3(9):895-900.</p>

<h4>Alternative Approaches</h4>

<p><strong>Obuchowski NA, McClish DK (1997).</strong> Sample size determination for diagnostic
accuracy studies involving binormal ROC curve indices. <em>Statistics in Medicine</em>, 16(13):1529-1542.</p>

<p><strong>Flahault A, Cadilhac M, Thomas G (2005).</strong> Sample size calculation should be
performed for design accuracy in diagnostic test studies. <em>Journal of Clinical Epidemiology</em>,
58(8):859-862.</p>

<p><strong>Hajian-Tilaki K (2014).</strong> Sample size estimation in diagnostic test studies of
biomedical informatics. <em>Journal of Biomedical Informatics</em>, 48:193-204.</p>

<h4>Related Publications by Bujang</h4>

<p><strong>Bujang MA, Adnan TH (2016).</strong> Requirements for minimum sample size for sensitivity
and specificity analysis. <em>Journal of Clinical and Diagnostic Research</em>, 10(10):YE01-YE06.</p>

<p><strong>Bujang MA (2021).</strong> A step-by-step process on sample size determination for medical
research. <em>Malaysian Journal of Medical Sciences</em>, 28(2):15-27.</p>
"
            self$results$references$setContent(html)
        }
    )
)
