
diagnosticsamplesizeClass <- R6::R6Class(
    "diagnosticsamplesizeClass",
    inherit = diagnosticsamplesizeBase,
    private = list(
        .results_cache = NULL,

        .init = function() {
            if (self$options$show_methodology) private$.setMethodology()
            if (self$options$show_references) private$.setReferences()
        },

        .run = function() {
            tryCatch({
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
                    limiting_factor <- if (n_sens_total > n_spec_total) "Sensitivity" else "Specificity"
                } else if (purpose == "screening_sens") {
                    # Emphasize sensitivity
                    n_required <- n_sens_total
                    limiting_factor <- "Sensitivity (screening emphasis)"
                } else {
                    # Emphasize specificity
                    n_required <- n_spec_total
                    limiting_factor <- "Specificity (screening emphasis)"
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

            }, error = function(e) {
                self$setError(paste("Error:", e$message))
            })
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
                x <- round(n * p)  # Expected number of successes

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

            summary_table <- self$results$sample_size_summary

            # Row 1: Sensitivity
            summary_table$addRow(rowKey = 1, values = list(
                parameter = "Sensitivity",
                target_value = res$sens,
                n_cases = res$n_sens,
                n_total = res$n_sens_total
            ))

            # Row 2: Specificity
            summary_table$addRow(rowKey = 2, values = list(
                parameter = "Specificity",
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

            purpose_text <- switch(res$purpose,
                diagnostic = "diagnostic marker with high sensitivity AND specificity",
                screening_sens = "screening tool emphasizing high sensitivity",
                screening_spec = "screening tool emphasizing high specificity"
            )

            html <- sprintf(
                "<div style='padding: 15px; background-color: #f9f9f9; border-left: 4px solid #4CAF50;'>
                <h3>Sample Size Justification Statement</h3>

                <p><strong>Study Objective:</strong> This study aims to evaluate a %s.</p>

                <p><strong>Methodology:</strong> Sample size calculation based on Clopper-Pearson exact binomial
                confidence intervals for sensitivity and specificity (Bujang, 2023).</p>

                <p><strong>Parameters:</strong></p>
                <ul>
                <li>Target population disease prevalence: <strong>%.1f%%</strong></li>
                <li>Target sensitivity: <strong>%.1f%%</strong> (95%% CI width: %.2f)</li>
                <li>Target specificity: <strong>%.1f%%</strong> (95%% CI width: %.2f)</li>
                <li>Significance level (α): <strong>%.2f</strong> (two-sided)</li>
                </ul>

                <p><strong>Sample Size Calculations:</strong></p>
                <ul>
                <li>For sensitivity estimation: <strong>%d total subjects</strong> required (to obtain %d diseased cases)</li>
                <li>For specificity estimation: <strong>%d total subjects</strong> required (to obtain %d non-diseased cases)</li>
                <li><strong>Minimum required sample size: %d subjects</strong> (limited by %s)</li>
                %s
                </ul>

                <p><strong>Reference:</strong> Bujang MA (2023). An Elaboration on Sample Size Planning for Performing a
                One-Sample Sensitivity and Specificity Analysis by Basing on Calculations on a Specified 95%% Confidence
                Interval Width. <em>Diagnostics</em> 13(8):1390.
                <a href='https://doi.org/10.3390/diagnostics13081390' target='_blank'>doi:10.3390/diagnostics13081390</a></p>
                </div>",
                purpose_text,
                res$prev * 100,
                res$sens * 100, res$width,
                res$spec * 100, res$width,
                res$alpha,
                res$n_sens_total, res$n_sens,
                res$n_spec_total, res$n_spec,
                res$n_required, res$limiting_factor,
                if (res$nonresp > 0) sprintf(
                    "<li><strong>Final sample size adjusted for %.0f%% non-response: %d subjects</strong></li>",
                    res$nonresp * 100, res$n_final
                ) else ""
            )

            self$results$sample_size_statement$setContent(html)
        },

        .generateComparisonTable = function() {
            # Parse prevalence values
            prev_string <- self$options$comparison_prevalences
            prev_values <- as.numeric(unlist(strsplit(prev_string, ",")))
            prev_values <- prev_values[!is.na(prev_values)]

            if (length(prev_values) == 0) return()

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
                    rel_eff <- 100  # Reference method
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
            z <- qnorm(1 - alpha/2)
            p_hat <- x / n

            denominator <- 1 + z^2/n
            center <- (p_hat + z^2/(2*n)) / denominator
            margin <- z * sqrt((p_hat*(1-p_hat)/n + z^2/(4*n^2))) / denominator

            list(
                lower = max(0, center - margin),
                upper = min(1, center + margin)
            )
        },

        .agresti_coull_ci = function(x, n, conf_level) {
            # Agresti-Coull confidence interval
            alpha <- 1 - conf_level
            z <- qnorm(1 - alpha/2)

            # Add z^2/2 to both successes and sample size
            n_tilde <- n + z^2
            x_tilde <- x + z^2/2
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
            z <- qnorm(1 - alpha/2)
            p_hat <- x / n
            se <- sqrt(p_hat * (1 - p_hat) / n)
            margin <- z * se

            list(
                lower = max(0, p_hat - margin),
                upper = min(1, p_hat + margin)
            )
        },

        .setMethodology = function() {
            html <- "
<h3>Diagnostic Test Sample Size Methodology (Bujang 2023)</h3>

<h4>Precision-Based Sample Size Planning</h4>
<p>This calculator uses <strong>precision-based</strong> (confidence interval width) sample size
planning rather than traditional power-based approaches. The focus is on ensuring sufficient
precision in sensitivity and specificity estimates.</p>

<h4>Clopper-Pearson Exact Binomial Confidence Intervals</h4>
<p>Sample sizes are calculated using <strong>Clopper-Pearson exact binomial confidence intervals</strong>,
which are based on the cumulative probabilities of the binomial distribution. This 'exact' method
is superior to normal approximation methods, especially for extreme values of sensitivity/specificity
or small sample sizes.</p>

<h4>Study Purpose Types</h4>

<p><strong>1. Diagnostic Purpose (High Sensitivity AND Specificity)</strong></p>
<ul>
<li>Both sensitivity and specificity should be ≥0.70 (preferably ≥0.80)</li>
<li>Sample size is determined by the <strong>maximum</strong> of sensitivity and specificity requirements</li>
<li>Example: Clinical diagnostic test replacing gold standard</li>
</ul>

<p><strong>2. Screening Purpose (Emphasize Sensitivity OR Specificity)</strong></p>
<ul>
<li>High sensitivity screening: Sens ≥0.70, Spec ≥0.50 (acceptable to sacrifice specificity)</li>
<li>High specificity screening: Spec ≥0.70, Sens ≥0.50 (acceptable to sacrifice sensitivity)</li>
<li>Example: Population screening where false negatives/positives have different costs</li>
</ul>

<h4>Key Factors Affecting Sample Size</h4>

<p><strong>1. Target Sensitivity/Specificity Values</strong></p>
<ul>
<li>Lower target values → Larger sample size needed</li>
<li>Recommendation: 0.95 = excellent, 0.90 = nearly excellent, 0.80 = good, 0.70 = fair</li>
</ul>

<p><strong>2. Disease Prevalence</strong></p>
<ul>
<li>Low prevalence → Larger N for sensitivity estimation (need more diseased cases)</li>
<li>High prevalence → Larger N for specificity estimation (need more non-diseased cases)</li>
<li>Critical consideration: Prevalence should reflect target population, not general population</li>
</ul>

<p><strong>3. Desired CI Width</strong></p>
<ul>
<li>Narrower width → Larger sample size</li>
<li>0.05 = very high precision (±2.5%); 0.10 = high precision (±5%); 0.20 = moderate precision (±10%)</li>
<li>Recommendation: Use 0.10 for most diagnostic studies, 0.20 for screening studies</li>
</ul>

<h4>Sample Size Formula</h4>
<p>For <strong>sensitivity</strong>: Find minimum N such that Clopper-Pearson CI width ≤ target width,
then <code>N_total = ceiling(N_diseased / prevalence)</code></p>

<p>For <strong>specificity</strong>: Find minimum N such that Clopper-Pearson CI width ≤ target width,
then <code>N_total = ceiling(N_non-diseased / (1 - prevalence))</code></p>

<p><strong>Final N</strong>: Take maximum of N_sensitivity and N_specificity (for diagnostic purpose)
or specific N based on screening emphasis</p>

<h4>Non-Response Adjustment</h4>
<p>If non-response/dropout is expected, inflate final N:
<code>N_adjusted = ceiling(N / (1 - non_response_rate))</code></p>
<p>Example: N=100, 20% non-response → N_adjusted = ceiling(100/0.8) = 125</p>
"
            self$results$methodology$setContent(html)
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
