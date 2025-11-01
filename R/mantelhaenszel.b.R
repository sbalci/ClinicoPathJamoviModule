
mantelhaenszelClass <- R6::R6Class(
    "mantelhaenszelClass",
    inherit = mantelhaenszelBase,
    private = list(
        .results_cache = NULL,

        .init = function() {
            if (self$options$show_methodology) private$.setMethodology()
            if (self$options$show_references) private$.setReferences()
        },

        .run = function() {
            # Check if all variables are selected
            if (is.null(self$options$rows) || is.null(self$options$cols) ||
                is.null(self$options$strata)) {
                return()
            }

            tryCatch({
                # Get data
                rows_var <- self$data[[self$options$rows]]
                cols_var <- self$data[[self$options$cols]]
                strata_var <- self$data[[self$options$strata]]

                # Remove missing values
                complete_cases <- complete.cases(rows_var, cols_var, strata_var)
                rows_var <- rows_var[complete_cases]
                cols_var <- cols_var[complete_cases]
                strata_var <- strata_var[complete_cases]

                # Convert to factors
                rows_var <- as.factor(rows_var)
                cols_var <- as.factor(cols_var)
                strata_var <- as.factor(strata_var)

                # Check for binary variables
                if (nlevels(rows_var) != 2 || nlevels(cols_var) != 2) {
                    self$setError("Rows and Columns variables must be binary (2 levels)")
                    return()
                }

                # Perform analyses
                private$.performAnalysis(rows_var, cols_var, strata_var)
                private$.populateTables()
                private$.setInterpretation()

            }, error = function(e) {
                self$setError(paste("Error:", e$message))
            })
        },

        .performAnalysis = function(rows, cols, strata) {
            # Create stratified tables
            strata_levels <- levels(strata)
            n_strata <- length(strata_levels)

            # Initialize storage
            stratum_results <- list()
            a <- numeric(n_strata)  # Exposed cases
            b <- numeric(n_strata)  # Exposed non-cases
            c <- numeric(n_strata)  # Unexposed cases
            d <- numeric(n_strata)  # Unexposed non-cases
            n <- numeric(n_strata)  # Total per stratum

            # Calculate stratum-specific statistics
            for (i in 1:n_strata) {
                stratum_data <- strata == strata_levels[i]
                tab <- table(rows[stratum_data], cols[stratum_data])

                # Ensure 2x2 table
                if (nrow(tab) != 2 || ncol(tab) != 2) {
                    next
                }

                # Extract cell counts (assuming rows=exposure, cols=outcome)
                a[i] <- tab[1, 1]  # Exposed, outcome+
                b[i] <- tab[1, 2]  # Exposed, outcome-
                c[i] <- tab[2, 1]  # Unexposed, outcome+
                d[i] <- tab[2, 2]  # Unexposed, outcome-
                n[i] <- sum(tab)

                # Stratum-specific odds ratio
                or_i <- (a[i] * d[i]) / (b[i] * c[i])

                # Confidence interval using Woolf method
                se_log_or <- sqrt(1/a[i] + 1/b[i] + 1/c[i] + 1/d[i])
                log_or <- log(or_i)
                ci_lower <- exp(log_or - qnorm(1 - self$options$alpha/2) * se_log_or)
                ci_upper <- exp(log_or + qnorm(1 - self$options$alpha/2) * se_log_or)

                # Chi-square test for stratum
                chisq_i <- sum((tab - outer(rowSums(tab), colSums(tab)) / n[i])^2 /
                               outer(rowSums(tab), colSums(tab)) / n[i])
                p_value_i <- 1 - pchisq(chisq_i, 1)

                stratum_results[[i]] <- list(
                    stratum = as.character(strata_levels[i]),
                    n_total = n[i],
                    estimate = or_i,
                    lower_ci = ci_lower,
                    upper_ci = ci_upper,
                    chisq = chisq_i,
                    p_value = p_value_i
                )
            }

            # Mantel-Haenszel common odds ratio
            mh_or <- sum(a * d / n) / sum(b * c / n)

            # Confidence interval for MH OR (Robins-Breslow-Greenland method)
            R <- sum(a * d / n)
            S <- sum(b * c / n)
            P <- sum((a + d) * a * d / n^2)
            Q <- sum((a + d) * b * c / n^2)
            U <- sum((b + c) * a * d / n^2)
            V <- sum((b + c) * b * c / n^2)

            se_log_mh_or <- sqrt((P / (2 * R^2)) + ((Q + U) / (2 * R * S)) + (V / (2 * S^2)))
            mh_or_lower <- exp(log(mh_or) - qnorm(1 - self$options$alpha/2) * se_log_mh_or)
            mh_or_upper <- exp(log(mh_or) + qnorm(1 - self$options$alpha/2) * se_log_mh_or)

            # Mantel-Haenszel chi-square test
            E_a <- (a + b) * (a + c) / n  # Expected value of a under H0
            Var_a <- ((a + b) * (c + d) * (a + c) * (b + d)) / (n^2 * (n - 1))

            if (self$options$continuity_correction) {
                mh_chisq <- (abs(sum(a) - sum(E_a)) - 0.5)^2 / sum(Var_a)
            } else {
                mh_chisq <- (sum(a) - sum(E_a))^2 / sum(Var_a)
            }
            mh_p_value <- 1 - pchisq(mh_chisq, 1)

            # Crude (unadjusted) analysis
            crude_tab <- table(rows, cols)
            crude_or <- (crude_tab[1, 1] * crude_tab[2, 2]) / (crude_tab[1, 2] * crude_tab[2, 1])
            se_crude_log_or <- sqrt(sum(1 / crude_tab))
            crude_or_lower <- exp(log(crude_or) - qnorm(1 - self$options$alpha/2) * se_crude_log_or)
            crude_or_upper <- exp(log(crude_or) + qnorm(1 - self$options$alpha/2) * se_crude_log_or)

            # Assess confounding (>10% change in OR)
            pct_change <- abs((crude_or - mh_or) / crude_or) * 100
            confounding_present <- if (pct_change > 10) {
                sprintf("Yes (%.1f%% change)", pct_change)
            } else {
                sprintf("No (%.1f%% change)", pct_change)
            }

            # Woolf test for homogeneity
            weights <- 1 / sapply(stratum_results, function(x) {
                1/a[as.numeric(x$stratum)] + 1/b[as.numeric(x$stratum)] +
                1/c[as.numeric(x$stratum)] + 1/d[as.numeric(x$stratum)]
            })
            log_ors <- sapply(stratum_results, function(x) log(x$estimate))
            woolf_chisq <- sum(weights * (log_ors - log(mh_or))^2)
            woolf_p <- 1 - pchisq(woolf_chisq, n_strata - 1)
            woolf_interp <- if (woolf_p < 0.05) {
                "Heterogeneous (MH OR may not be appropriate)"
            } else {
                "Homogeneous (MH OR is appropriate)"
            }

            # Breslow-Day test for homogeneity
            # Iterative calculation of common OR under null
            bd_chisq <- 0
            for (i in 1:n_strata) {
                # Expected value under common OR
                A <- mh_or - 1
                B <- -(a[i] + d[i] + (b[i] + c[i]) * mh_or)
                C <- (a[i] + b[i]) * (a[i] + c[i])

                # Quadratic formula
                if (A != 0) {
                    a_exp <- (-B - sqrt(B^2 - 4 * A * C)) / (2 * A)
                } else {
                    a_exp <- -C / B
                }

                bd_chisq <- bd_chisq + (a[i] - a_exp)^2 /
                            (1/a_exp + 1/(n[i] - a_exp - c[i]) + 1/(a[i] + b[i] - a_exp) +
                             1/(c[i] + d[i] - n[i] + a_exp + a[i] + b[i] - a_exp))
            }
            bd_p <- 1 - pchisq(bd_chisq, n_strata - 1)
            bd_interp <- if (bd_p < 0.05) {
                "Heterogeneous (OR varies across strata)"
            } else {
                "Homogeneous (common OR across strata)"
            }

            # Store results
            private$.results_cache <- list(
                mh_or = mh_or,
                mh_or_lower = mh_or_lower,
                mh_or_upper = mh_or_upper,
                mh_chisq = mh_chisq,
                mh_p_value = mh_p_value,
                crude_or = crude_or,
                crude_or_lower = crude_or_lower,
                crude_or_upper = crude_or_upper,
                confounding = confounding_present,
                pct_change = pct_change,
                stratum_results = stratum_results,
                woolf_chisq = woolf_chisq,
                woolf_p = woolf_p,
                woolf_interp = woolf_interp,
                bd_chisq = bd_chisq,
                bd_p = bd_p,
                bd_interp = bd_interp,
                n_strata = n_strata
            )
        },

        .populateTables = function() {
            res <- private$.results_cache

            # Mantel-Haenszel summary table
            mh_table <- self$results$mh_summary
            mh_table$setRow(rowNo = 1, values = list(
                estimate = res$mh_or,
                lower_ci = res$mh_or_lower,
                upper_ci = res$mh_or_upper,
                chisq = res$mh_chisq,
                df = 1,
                p_value = res$mh_p_value
            ))

            # Crude analysis table
            if (self$options$show_crude_analysis) {
                crude_table <- self$results$crude_analysis
                crude_table$setRow(rowNo = 1, values = list(
                    crude_estimate = res$crude_or,
                    crude_lower = res$crude_or_lower,
                    crude_upper = res$crude_or_upper,
                    confounding = res$confounding
                ))
            }

            # Homogeneity tests table
            if (self$options$show_homogeneity_tests) {
                homog_table <- self$results$homogeneity_tests

                homog_table$addRow(rowKey = 1, values = list(
                    test_name = "Woolf Test",
                    statistic = res$woolf_chisq,
                    df = res$n_strata - 1,
                    p_value = res$woolf_p,
                    interpretation = res$woolf_interp
                ))

                homog_table$addRow(rowKey = 2, values = list(
                    test_name = "Breslow-Day Test",
                    statistic = res$bd_chisq,
                    df = res$n_strata - 1,
                    p_value = res$bd_p,
                    interpretation = res$bd_interp
                ))
            }

            # Stratum-specific tables
            if (self$options$show_stratum_tables) {
                stratum_table <- self$results$stratum_specific
                for (i in seq_along(res$stratum_results)) {
                    sr <- res$stratum_results[[i]]
                    stratum_table$addRow(rowKey = i, values = sr)
                }
            }
        },

        .setInterpretation = function() {
            res <- private$.results_cache

            html <- sprintf(
                "<div style='padding: 15px; background-color: #f9f9f9; border-left: 4px solid #2196F3;'>
                <h3>Interpretation</h3>

                <p><strong>Mantel-Haenszel Common Odds Ratio: %.3f</strong> (95%% CI: %.3f - %.3f)</p>
                <ul>
                <li>The odds of outcome are <strong>%.2f times</strong> %s in exposed vs. unexposed,
                    adjusting for the stratifying variable</li>
                <li>MH Chi-Square = %.3f, p %s 0.001 → Association is %s</li>
                </ul>

                <p><strong>Confounding Assessment:</strong></p>
                <ul>
                <li>Crude OR: %.3f (95%% CI: %.3f - %.3f)</li>
                <li>Adjusted OR (MH): %.3f (95%% CI: %.3f - %.3f)</li>
                <li>Percent change: %.1f%%</li>
                <li><strong>%s</strong></li>
                </ul>

                <p><strong>Homogeneity of Odds Ratios Across Strata:</strong></p>
                <ul>
                <li>Woolf test: p = %.3f → %s</li>
                <li>Breslow-Day test: p = %.3f → %s</li>
                %s
                </ul>

                <p><strong>Recommendation:</strong> %s</p>
                </div>",
                res$mh_or, res$mh_or_lower, res$mh_or_upper,
                res$mh_or,
                if (res$mh_or > 1) "higher" else "lower",
                res$mh_chisq,
                if (res$mh_p_value < 0.001) "<" else "=",
                if (res$mh_p_value < 0.05) "statistically significant" else "not statistically significant",
                res$crude_or, res$crude_or_lower, res$crude_or_upper,
                res$mh_or, res$mh_or_lower, res$mh_or_upper,
                res$pct_change,
                res$confounding,
                res$woolf_p, res$woolf_interp,
                res$bd_p, res$bd_interp,
                if (res$woolf_p < 0.05 || res$bd_p < 0.05) {
                    "<li><strong>Warning:</strong> Odds ratios appear to vary across strata.
                    Consider presenting stratum-specific estimates instead of common MH OR.</li>"
                } else {
                    ""
                },
                if (res$woolf_p < 0.05 || res$bd_p < 0.05) {
                    "Stratum-specific ORs should be reported separately due to heterogeneity.
                    Consider interaction analysis or effect modification."
                } else if (res$pct_change > 10) {
                    "The stratifying variable is a confounder. Report the Mantel-Haenszel adjusted OR."
                } else {
                    "The stratifying variable does not appear to be a strong confounder.
                    Crude and adjusted estimates are similar."
                }
            )

            self$results$interpretation$setContent(html)
        },

        .setMethodology = function() {
            html <- "
<h3>Mantel-Haenszel Stratified Analysis Methodology</h3>

<h4>Purpose</h4>
<p>The Mantel-Haenszel method provides a way to estimate a common odds ratio (or risk ratio)
across multiple strata while controlling for a confounding variable. It is widely used in
observational epidemiology for case-control and cohort studies.</p>

<h4>Mantel-Haenszel Common Odds Ratio</h4>
<p>The common odds ratio pooled across K strata is:</p>
<pre>OR_MH = Σ(a_i × d_i / n_i) / Σ(b_i × c_i / n_i)</pre>

<p>Where for each stratum i:</p>
<ul>
<li>a_i = exposed cases</li>
<li>b_i = exposed non-cases</li>
<li>c_i = unexposed cases</li>
<li>d_i = unexposed non-cases</li>
<li>n_i = total subjects in stratum i</li>
</ul>

<p><strong>Confidence Interval:</strong> Calculated using the Robins-Breslow-Greenland variance estimator.</p>

<h4>Mantel-Haenszel Chi-Square Test</h4>
<p>Tests the null hypothesis of no association between exposure and outcome,
adjusting for strata:</p>
<pre>χ²_MH = [|Σa_i - ΣE(a_i)| - 0.5]² / ΣVar(a_i)</pre>

<p>Where:</p>
<ul>
<li>E(a_i) = (a_i + b_i)(a_i + c_i) / n_i (expected value under H0)</li>
<li>Var(a_i) = [(a_i + b_i)(c_i + d_i)(a_i + c_i)(b_i + d_i)] / [n_i² × (n_i - 1)]</li>
<li>Continuity correction of 0.5 is optional</li>
</ul>

<h4>Tests of Homogeneity</h4>

<p><strong>Woolf Test:</strong> Tests whether the odds ratios are homogeneous across strata.</p>
<pre>χ²_Woolf = Σw_i[ln(OR_i) - ln(OR_MH)]²</pre>
<p>Where w_i = 1 / Var(ln(OR_i))</p>

<p><strong>Breslow-Day Test:</strong> Similar to Woolf but uses observed vs. expected cell counts
under the assumption of a common odds ratio. More robust when sample sizes are small.</p>

<p><strong>Interpretation:</strong></p>
<ul>
<li>p < 0.05 → Heterogeneous (ORs differ across strata; MH common OR may be misleading)</li>
<li>p ≥ 0.05 → Homogeneous (ORs are similar; MH common OR is appropriate)</li>
</ul>

<h4>Confounding Assessment</h4>
<p>Confounding is assessed by comparing crude (unadjusted) OR to Mantel-Haenszel adjusted OR:</p>
<ul>
<li><strong>Percent change = |(Crude OR - MH OR) / Crude OR| × 100</strong></li>
<li>>10% change → Confounding is present</li>
<li>≤10% change → Minimal confounding</li>
</ul>

<h4>Assumptions</h4>
<ul>
<li>Binary exposure and outcome variables</li>
<li>Independent observations (no clustering)</li>
<li>Adequate sample size in each stratum (expected cell counts ≥5)</li>
<li><strong>Homogeneity assumption:</strong> Odds ratios should be similar across strata</li>
</ul>

<h4>When to Use</h4>
<p><strong>Appropriate for:</strong></p>
<ul>
<li>Case-control studies (odds ratio)</li>
<li>Cohort studies (risk ratio, risk difference)</li>
<li>Controlling for a single categorical confounder</li>
<li>Moderate number of strata (typically < 10)</li>
</ul>

<p><strong>Limitations:</strong></p>
<ul>
<li>Can only adjust for one stratifying variable</li>
<li>Loses power with many strata (sparse data)</li>
<li>Assumes no interaction between exposure and strata</li>
<li>For multiple confounders, use multivariable regression instead</li>
</ul>
"
            self$results$methodology$setContent(html)
        },

        .setReferences = function() {
            html <- "
<h3>References</h3>

<h4>Primary References</h4>

<p><strong>Mantel N, Haenszel W (1959).</strong> Statistical aspects of the analysis of data from
retrospective studies of disease. <em>Journal of the National Cancer Institute</em>, 22(4):719-748.</p>

<p><strong>Robins J, Breslow N, Greenland S (1986).</strong> Estimators of the Mantel-Haenszel variance
consistent in both sparse data and large-strata limiting models. <em>Biometrics</em>, 42(2):311-323.</p>

<h4>Homogeneity Tests</h4>

<p><strong>Woolf B (1955).</strong> On estimating the relation between blood group and disease.
<em>Annals of Human Genetics</em>, 19(4):251-253.</p>

<p><strong>Breslow NE, Day NE (1980).</strong> <em>Statistical Methods in Cancer Research,
Volume 1: The Analysis of Case-Control Studies</em>. Lyon: International Agency for Research on Cancer.</p>

<h4>Textbooks</h4>

<p><strong>Rothman KJ, Greenland S, Lash TL (2008).</strong> <em>Modern Epidemiology</em> (3rd ed.).
Philadelphia: Lippincott Williams & Wilkins. Chapter 15: Introduction to Stratified Analysis.</p>

<p><strong>Agresti A (2013).</strong> <em>Categorical Data Analysis</em> (3rd ed.).
Hoboken, NJ: Wiley. Section 6.2: Mantel-Haenszel Methods.</p>

<h4>Online Resources</h4>

<p><strong>CDC (2012).</strong> Principles of Epidemiology in Public Health Practice (3rd ed.).
Lesson 3: Measures of Risk. Available at:
<a href='https://www.cdc.gov/csels/dsepd/ss1978/' target='_blank'>CDC SSPH</a></p>
"
            self$results$references$setContent(html)
        }
    )
)
