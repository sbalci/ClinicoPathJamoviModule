
samplingerrorClass <- R6::R6Class(
    "samplingerrorClass",
    inherit = samplingerrorBase,
    private = list(
        .results_cache = NULL,

        .init = function() {
            if (self$options$showMethodology) private$.setMethodology()
            if (self$options$showReferences) private$.setReferences()
        },

        .run = function() {
            tryCatch({
                private$.calculateErrors()
                private$.populateTables()
            }, error = function(e) {
                self$setError(paste("Error:", e$message))
            })
        },

        .calculateErrors = function() {
            # Extract parameters
            sens <- self$options$detectionSensitivity / 100
            cv <- self$options$biologicalVarianceCV / 100
            n <- self$options$sampleSize
            freq <- self$options$eventFrequency / 100
            ref_vol <- self$options$referenceVolume
            sample_vol <- self$options$sampleVolume

            # Calculate three error components (Kayser et al., 2009)

            # E(Ne): Error of detecting individual event
            # Represents false negative rate
            E_Ne <- (1 - sens) * 100

            # E(B(n)): Error of measuring all elements (biological variance)
            # Decreases with sample size following standard error formula
            E_Bn <- (cv / sqrt(n)) * 100

            # E(Ne/sv): Error of frequency estimation in sample
            # Based on binomial proportion standard error
            # Accounts for sampling fraction
            sampling_fraction <- (sample_vol * n) / ref_vol
            if (sampling_fraction >= 1) {
                # If we sample entire volume, only biological variance remains
                E_Nesv <- 0
            } else {
                # Standard error of proportion with finite population correction
                if (freq > 0 && freq < 1) {
                    se_prop <- sqrt((freq * (1 - freq)) / n)
                    # Finite population correction
                    fpc <- sqrt((ref_vol - sample_vol * n) / (ref_vol - 1))
                    E_Nesv <- se_prop * fpc * 100
                } else {
                    E_Nesv <- 0
                }
            }

            # Total error: E(p) = √[E²(Ne) + E²(B(n)) + E²(Ne/sv)]
            E_p <- sqrt(E_Ne^2 + E_Bn^2 + E_Nesv^2)

            # Store results
            private$.results_cache <- list(
                E_Ne = E_Ne,
                E_Bn = E_Bn,
                E_Nesv = E_Nesv,
                E_p = E_p,
                n = n
            )
        },

        .populateTables = function() {
            results <- private$.results_cache

            # Error summary table
            interpretation <- if (results$E_p < 5) {
                "Excellent - Very low sampling error"
            } else if (results$E_p < 10) {
                "Good - Acceptable sampling error"
            } else if (results$E_p < 20) {
                "Moderate - Consider more samples"
            } else {
                "Poor - Inadequate sampling, increase N"
            }

            summary_table <- self$results$errorSummary
            summary_table$setRow(rowNo = 1, values = list(
                total_error = as.numeric(results$E_p),
                n_samples = as.integer(results$n),
                interpretation = interpretation
            ))

            # Error components table
            if (self$options$showErrorComponents) {
                comp_table <- self$results$errorComponents

                total_sq <- results$E_Ne^2 + results$E_Bn^2 + results$E_Nesv^2

                comp_table$setRow(rowNo = 1, values = list(
                    component = "E(Ne)",
                    formula = "Detection Error",
                    value = as.numeric(results$E_Ne),
                    contribution = as.numeric((results$E_Ne^2 / total_sq) * 100),
                    description = "Error from false negatives/misidentification"
                ))

                comp_table$setRow(rowNo = 2, values = list(
                    component = "E(B(n))",
                    formula = "Biological Variance",
                    value = as.numeric(results$E_Bn),
                    contribution = as.numeric((results$E_Bn^2 / total_sq) * 100),
                    description = "Error from tissue heterogeneity"
                ))

                comp_table$setRow(rowNo = 3, values = list(
                    component = "E(Ne/sv)",
                    formula = "Frequency Estimation",
                    value = as.numeric(results$E_Nesv),
                    contribution = as.numeric((results$E_Nesv^2 / total_sq) * 100),
                    description = "Error from sample vs reference volume ratio"
                ))
            }

            # Optimization table
            if (self$options$showOptimization) {
                private$.calculateOptimization()
            }
        },

        .calculateOptimization = function() {
            opt_table <- self$results$optimization
            results <- private$.results_cache
            current_n <- results$n

            # Test different target errors
            target_errors <- c(5, 10, 15, 20)

            for (target in target_errors) {
                # Estimate required N to achieve target error
                # Simplified: assuming biological variance is dominant component
                cv <- self$options$biologicalVarianceCV / 100

                # From E(B(n)) = CV / sqrt(N), solve for N
                required_n <- ceiling((cv * 100 / target)^2)

                # Cap at reasonable maximum
                required_n <- min(required_n, 1000)

                improvement <- if (required_n < current_n) {
                    "Already achieved"
                } else if (required_n == current_n) {
                    "Current"
                } else {
                    sprintf("+%d samples needed", required_n - current_n)
                }

                opt_table$addRow(rowKey = target, values = list(
                    target_error = as.numeric(target),
                    required_n = as.integer(required_n),
                    improvement = improvement
                ))
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            results <- private$.results_cache

            # Create data for plotting error vs sample size
            n_range <- seq(1, min(100, results$n * 3), by = 1)
            errors <- numeric(length(n_range))

            sens <- self$options$detectionSensitivity / 100
            cv <- self$options$biologicalVarianceCV / 100
            freq <- self$options$eventFrequency / 100
            ref_vol <- self$options$referenceVolume
            sample_vol <- self$options$sampleVolume

            for (i in seq_along(n_range)) {
                n <- n_range[i]
                E_Ne <- (1 - sens) * 100
                E_Bn <- (cv / sqrt(n)) * 100
                sampling_fraction <- (sample_vol * n) / ref_vol
                if (sampling_fraction < 1 && freq > 0 && freq < 1) {
                    se_prop <- sqrt((freq * (1 - freq)) / n)
                    fpc <- sqrt((ref_vol - sample_vol * n) / max(1, ref_vol - 1))
                    E_Nesv <- se_prop * fpc * 100
                } else {
                    E_Nesv <- 0
                }
                errors[i] <- sqrt(E_Ne^2 + E_Bn^2 + E_Nesv^2)
            }

            plot_data <- data.frame(
                n = n_range,
                error = errors
            )

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = n, y = error)) +
                ggplot2::geom_line(linewidth = 1.2, color = "#4D94CC") +
                ggplot2::geom_point(data = data.frame(n = results$n, error = results$E_p),
                                  size = 4, color = "red") +
                ggplot2::geom_hline(yintercept = self$options$targetError,
                                  linetype = "dashed", color = "darkgreen", linewidth = 1) +
                ggplot2::labs(
                    x = "Sample Size (N)",
                    y = "Total Sampling Error E(p) (%)",
                    title = "Sampling Error vs Sample Size",
                    subtitle = paste("Current N =", results$n,
                                   "| E(p) =", sprintf("%.2f%%", results$E_p))
                ) +
                ggplot2::annotate("text", x = max(n_range) * 0.7, y = self$options$targetError * 1.1,
                                label = paste("Target:", self$options$targetError, "%"),
                                color = "darkgreen") +
                ggtheme

            print(p)
            TRUE
        },

        .setMethodology = function() {
            html <- "
<h3>Sampling Error & Efficiency (Kayser et al., 2009)</h3>

<h4>The Sampling Error Formula</h4>
<p><strong>E(p) = √[E²(Ne) + E²(B(n)) + E²(Ne/sv)]</strong></p>

<p>Where:</p>

<h5>1. E(Ne): Detection Error</h5>
<ul>
<li><strong>Formula:</strong> E(Ne) = 1 - sensitivity</li>
<li><strong>Meaning:</strong> Probability of missing/misidentifying an event</li>
<li><strong>Example:</strong> 95% sensitivity → E(Ne) = 5%</li>
<li><strong>Controlled by:</strong> Observer training, staining quality, imaging resolution</li>
</ul>

<h5>2. E(B(n)): Biological Variance Error</h5>
<ul>
<li><strong>Formula:</strong> E(B(n)) = CV / √N</li>
<li><strong>Meaning:</strong> Error from tissue heterogeneity</li>
<li><strong>Example:</strong> CV=15%, N=10 → E(B(n)) = 4.7%</li>
<li><strong>Controlled by:</strong> Increasing sample size N</li>
<li><strong>Note:</strong> This is the standard error of the mean</li>
</ul>

<h5>3. E(Ne/sv): Frequency Estimation Error</h5>
<ul>
<li><strong>Formula:</strong> E(Ne/sv) = SE(proportion) × FPC</li>
<li><strong>Meaning:</strong> Error from sampling fraction of reference volume</li>
<li><strong>Controlled by:</strong> Sample volume relative to reference volume</li>
<li><strong>Finite Population Correction (FPC):</strong> Accounts for sampling without replacement</li>
</ul>

<h4>Key Insights from Formula</h4>

<p><strong>1. Smallest Error Achieved When:</strong></p>
<ul>
<li>Sample size = Reference volume (complete sampling)</li>
<li>Tissue is homogeneous (low biological variance)</li>
<li>Detection sensitivity is perfect (100%)</li>
</ul>

<p><strong>2. Error Increases With:</strong></p>
<ul>
<li>Smaller sample sizes relative to event size</li>
<li>Higher tissue heterogeneity</li>
<li>Variable sample sizes (not uniform)</li>
</ul>

<p><strong>3. Sample Size Impact:</strong></p>
<ul>
<li>Doubling N reduces E(B(n)) by ~29%</li>
<li>Quadrupling N cuts E(B(n)) in half</li>
<li>Diminishing returns after ~20-30 samples for most applications</li>
</ul>

<h4>Clinical Applications</h4>

<p><strong>Lymph Node Dissection:</strong></p>
<ul>
<li>Reference: Total lymph node basin</li>
<li>Event: Positive lymph nodes</li>
<li>Question: How many nodes needed to accurately assess N stage?</li>
</ul>

<p><strong>Tumor Sampling:</strong></p>
<ul>
<li>Reference: Total tumor volume</li>
<li>Event: Specific features (VI, PNI, necrosis)</li>
<li>Question: How many blocks needed for complete assessment?</li>
</ul>

<p><strong>Stereology Studies:</strong></p>
<ul>
<li>Reference: Entire tissue section</li>
<li>Event: Vessels, nuclei, specific cells</li>
<li>Question: How many fields needed for accurate density estimation?</li>
</ul>

<h4>Interpreting Results</h4>
<ul>
<li><strong>E(p) < 5%:</strong> Excellent sampling - minimal error</li>
<li><strong>E(p) 5-10%:</strong> Good sampling - acceptable for most purposes</li>
<li><strong>E(p) 10-20%:</strong> Moderate - adequate for exploratory studies</li>
<li><strong>E(p) > 20%:</strong> Poor - inadequate sampling, increase N</li>
</ul>

<h4>Optimization Strategy</h4>
<ol>
<li><strong>Identify dominant error component</strong> (highest contribution %)</li>
<li><strong>If E(Ne) dominates:</strong> Improve detection (training, better staining)</li>
<li><strong>If E(B(n)) dominates:</strong> Increase sample size</li>
<li><strong>If E(Ne/sv) dominates:</strong> Take larger samples or sample more of reference</li>
</ol>
"
            self$results$methodology$setContent(html)
        },

        .setReferences = function() {
            html <- "
<h3>References</h3>

<p><strong>Kayser, K., Schultz, H., Goldmann, T., Görtler, J., Kayser, G., & Vollmer, E. (2009).</strong>
Theory of sampling and its application in tissue based diagnosis.
<em>Diagnostic Pathology</em>, 4:6. doi:10.1186/1746-1596-4-6</p>

<p><em>Specifically lines 527-546 describing the sampling error formula and its components.</em></p>

<h4>Related Statistical Theory</h4>

<p><strong>Cochran, W. G. (1977).</strong>
<em>Sampling Techniques</em> (3rd ed.). New York: John Wiley & Sons.</p>

<p><strong>Thompson, S. K. (2012).</strong>
<em>Sampling</em> (3rd ed.). Hoboken, NJ: John Wiley & Sons.</p>

<h4>Pathology Applications</h4>

<p><strong>Buderer, N. M. (1996).</strong>
Statistical methodology: I. Incorporating the prevalence of disease into the sample size
calculation for sensitivity and specificity.
<em>Academic Emergency Medicine</em>, 3(9):895-900.</p>

<p><strong>Gundersen, H. J., & Jensen, E. B. (1987).</strong>
The efficiency of systematic sampling in stereology and its prediction.
<em>Journal of Microscopy</em>, 147:229-263.</p>
"
            self$results$references$setContent(html)
        }
    )
)
