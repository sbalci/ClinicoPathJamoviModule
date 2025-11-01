
ihcthresholdClass <- R6::R6Class(
    "ihcthresholdClass",
    inherit = ihcthresholdBase,
    private = list(
        .data = NULL,
        .results_cache = NULL,

        .init = function() {
            private$.results_cache <- list()
            if (self$options$showMethodology) private$.setMethodology()
            if (self$options$showReferences) private$.setReferences()
        },

        .run = function() {
            if (is.null(self$options$sampleArea) || is.null(self$options$threshold) ||
                is.null(self$options$positiveCells) || is.null(self$options$negativeCells)) {
                return()
            }

            tryCatch({
                private$.prepareData()
                private$.analyzeThresholds()
                private$.populateTables()
            }, error = function(e) {
                self$setError(paste("Error:", e$message))
            })
        },

        .prepareData = function() {
            raw_data <- self$data
            data_list <- list(
                area = as.factor(raw_data[[self$options$sampleArea]]),
                threshold = raw_data[[self$options$threshold]],
                positive = as.numeric(raw_data[[self$options$positiveCells]]),
                negative = as.numeric(raw_data[[self$options$negativeCells]])
            )
            private$.data <- as.data.frame(data_list)
            private$.data$total <- private$.data$positive + private$.data$negative
            private$.data$positive_ratio <- private$.data$positive / private$.data$total
            private$.data <- private$.data[complete.cases(private$.data), ]
        },

        .analyzeThresholds = function() {
            data <- private$.data
            thresholds <- unique(data$threshold)

            threshold_stats <- lapply(thresholds, function(t) {
                subset_data <- data[data$threshold == t, ]
                ratios <- subset_data$positive_ratio
                mean_ratio <- mean(ratios, na.rm = TRUE)
                sd_ratio <- sd(ratios, na.rm = TRUE)
                cv <- (sd_ratio / mean_ratio) * 100

                list(
                    threshold = t,
                    mean_ratio = mean_ratio,
                    sd_ratio = sd_ratio,
                    cv = cv,
                    min_ratio = min(ratios, na.rm = TRUE),
                    max_ratio = max(ratios, na.rm = TRUE),
                    consistency = 1 / (1 + cv)
                )
            })

            # Select optimal threshold based on method
            optimal_idx <- if (self$options$optimizationMethod == "cv") {
                which.min(sapply(threshold_stats, function(x) x$cv))
            } else if (self$options$optimizationMethod == "variance") {
                which.min(sapply(threshold_stats, function(x) x$sd_ratio))
            } else {
                which.max(sapply(threshold_stats, function(x) x$consistency))
            }

            private$.results_cache <- list(
                threshold_stats = threshold_stats,
                optimal = threshold_stats[[optimal_idx]]
            )
        },

        .populateTables = function() {
            data <- private$.data
            results <- private$.results_cache

            # Summary table
            summary_table <- self$results$summaryTable
            summary_table$setRow(rowNo = 1, values = list(
                n_areas = as.integer(length(unique(data$area))),
                n_thresholds = as.integer(length(unique(data$threshold))),
                total_cells_mean = as.numeric(mean(data$total, na.rm = TRUE)),
                positive_range = sprintf("%.1f%% - %.1f%%",
                    min(data$positive_ratio * 100),
                    max(data$positive_ratio * 100))
            ))

            # Threshold comparison table
            comp_table <- self$results$thresholdComparison
            for (i in seq_along(results$threshold_stats)) {
                stat <- results$threshold_stats[[i]]
                comp_table$addRow(rowKey = i, values = list(
                    threshold = as.character(stat$threshold),
                    mean_positive_ratio = as.numeric(stat$mean_ratio * 100),
                    sd_positive_ratio = as.numeric(stat$sd_ratio * 100),
                    cv = as.numeric(stat$cv),
                    min_ratio = as.numeric(stat$min_ratio * 100),
                    max_ratio = as.numeric(stat$max_ratio * 100),
                    consistency_score = as.numeric(stat$consistency)
                ))
            }

            # Optimal threshold table
            optimal <- results$optimal
            opt_data <- data[data$threshold == optimal$threshold, ]
            ci <- quantile(opt_data$positive_ratio, probs = c(0.025, 0.975)) * 100

            interpretation <- if (optimal$cv < 10) {
                "Excellent consistency"
            } else if (optimal$cv < 20) {
                "Good consistency"
            } else if (optimal$cv < 30) {
                "Moderate consistency"
            } else {
                "Poor consistency - consider additional samples"
            }

            opt_table <- self$results$optimalThreshold
            opt_table$setRow(rowNo = 1, values = list(
                optimal_threshold = as.character(optimal$threshold),
                optimization_value = as.numeric(optimal$cv),
                mean_positive_pct = as.numeric(optimal$mean_ratio * 100),
                ci_lower = as.numeric(ci[1]),
                ci_upper = as.numeric(ci[2]),
                interpretation = interpretation
            ))
        },

        .plot = function(image, ggtheme, theme, ...) {
            data <- private$.data
            results <- private$.results_cache

            if (self$options$plotType == "line") {
                # Line plot with error bars
                summary_data <- data.frame(
                    threshold = sapply(results$threshold_stats, function(x) x$threshold),
                    mean = sapply(results$threshold_stats, function(x) x$mean_ratio * 100),
                    sd = sapply(results$threshold_stats, function(x) x$sd_ratio * 100)
                )

                p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = factor(threshold), y = mean, group = 1)) +
                    ggplot2::geom_line(linewidth = 1, color = "#4D94CC") +
                    ggplot2::geom_point(size = 3, color = "#4D94CC") +
                    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
                                         width = 0.2, linewidth = 0.8) +
                    ggplot2::geom_vline(xintercept = which(summary_data$threshold == results$optimal$threshold),
                                      linetype = "dashed", color = "red", linewidth = 1) +
                    ggplot2::labs(
                        x = "Threshold Level",
                        y = "Positive Cells (%)",
                        title = paste("IHC Threshold Analysis:", self$options$stainType),
                        subtitle = paste("Optimal threshold:", results$optimal$threshold)
                    ) +
                    ggtheme
            } else {
                # Box plot
                data$threshold_factor <- factor(data$threshold)
                p <- ggplot2::ggplot(data, ggplot2::aes(x = threshold_factor, y = positive_ratio * 100)) +
                    ggplot2::geom_boxplot(fill = "#4D94CC", alpha = 0.7) +
                    ggplot2::labs(
                        x = "Threshold Level",
                        y = "Positive Cells (%)"
                    ) +
                    ggtheme
            }

            print(p)
            TRUE
        },

        .setMethodology = function() {
            html <- "
<h3>Active Sampling Methodology for IHC Threshold Determination</h3>

<h4>Principle (Kayser et al., 2009)</h4>
<p><strong>Problem:</strong> Immunohistochemistry staining intensity varies across tissue sections
and between staining runs, making objective positive/negative discrimination difficult.</p>

<p><strong>Solution:</strong> Active sampling tests multiple threshold levels across multiple
randomly-selected tissue areas and identifies the threshold where the positive/negative ratio
remains constant.</p>

<h4>Algorithm</h4>
<ol>
<li><strong>Select Sample Areas:</strong> Randomly choose 5+ areas across the tissue</li>
<li><strong>Test Multiple Thresholds:</strong> Apply 3-5 different intensity thresholds</li>
<li><strong>Count Cells:</strong> At each threshold in each area, count positive and negative cells</li>
<li><strong>Calculate Ratios:</strong> positive_ratio = positive_cells / total_cells</li>
<li><strong>Find Optimal:</strong> Select threshold with most consistent ratio across areas</li>
</ol>

<h4>Optimization Methods</h4>

<p><strong>1. Coefficient of Variation (CV) - RECOMMENDED</strong></p>
<ul>
<li>Formula: CV = (SD / Mean) × 100</li>
<li>Lower CV = more consistent across areas</li>
<li>CV < 10%: Excellent, CV < 20%: Good, CV < 30%: Acceptable</li>
</ul>

<p><strong>2. Minimum Variance</strong></p>
<ul>
<li>Selects threshold with lowest standard deviation of positive ratios</li>
<li>Similar to CV but absolute rather than relative measure</li>
</ul>

<p><strong>3. Consistency Score</strong></p>
<ul>
<li>Formula: Consistency = 1 / (1 + CV)</li>
<li>Higher score = better consistency</li>
</ul>

<h4>Clinical Application</h4>

<p><strong>Ki-67 (Proliferation Index)</strong></p>
<ul>
<li>Critical for breast cancer grading</li>
<li>Cut-offs: < 10% (low), 10-20% (intermediate), > 20% (high)</li>
<li>Consistent threshold essential for reproducibility</li>
</ul>

<p><strong>HER2, ER, PR (Hormone Receptors)</strong></p>
<ul>
<li>Treatment decisions based on positive/negative status</li>
<li>Threshold determines clinical management</li>
<li>Inter-laboratory standardization critical</li>
</ul>

<p><strong>PD-L1 (Immunotherapy)</strong></p>
<ul>
<li>Multiple scoring systems (TPS, CPS, IPS)</li>
<li>Different cut-offs (1%, 5%, 10%, 50%)</li>
<li>Threshold standardization affects treatment eligibility</li>
</ul>

<h4>Quality Control</h4>
<ul>
<li><strong>Minimum 5 sample areas</strong> for reliable statistics</li>
<li><strong>Areas should be randomly selected</strong> (not cherry-picked)</li>
<li><strong>CV should be < 20%</strong> for clinical use</li>
<li><strong>If CV > 30%</strong>: Add more sample areas or check staining quality</li>
</ul>

<h4>Implementation (EAMUS™ System)</h4>
<p>This algorithm is successfully implemented in the automated EAMUS™ (Evaluation and
Measurement System) developed by Kayser et al. for standardized IHC quantification.</p>
"
            self$results$methodology$setContent(html)
        },

        .setReferences = function() {
            html <- "
<h3>References</h3>

<p><strong>Kayser, G., Radziszowski, D., Bzdyl, P., et al. (2006).</strong>
Theory and implementation of an electronic, automated measurement system for images
obtained from immunohistochemically stained slides.
<em>Anal Quant Cytol Histol</em>, 28:27-38.</p>

<p><strong>Kayser, K., Görtler, J., Goldmann, T., et al. (2008).</strong>
Image standards in tissue-based diagnosis (Diagnostic Surgical Pathology).
<em>Diagn Pathol</em>, 3:17.</p>

<p><strong>Kayser, K., Schultz, H., Goldmann, T., et al. (2009).</strong>
Theory of sampling and its application in tissue based diagnosis.
<em>Diagnostic Pathology</em>, 4:6. doi:10.1186/1746-1596-4-6</p>

<h4>Clinical Applications</h4>

<p><strong>Nielsen, T. O., Leung, S. C., Rimm, D. L., et al. (2021).</strong>
Assessment of Ki67 in breast cancer: Updated recommendations from the International
Ki67 in Breast Cancer Working Group.
<em>J Natl Cancer Inst</em>, 113(7):808-819.</p>

<p><strong>Wolff, A. C., Hammond, M. E., Allison, K. H., et al. (2018).</strong>
Human epidermal growth factor receptor 2 testing in breast cancer: ASCO/CAP guideline update.
<em>J Clin Oncol</em>, 36(20):2105-2122.</p>
"
            self$results$references$setContent(html)
        }
    )
)
