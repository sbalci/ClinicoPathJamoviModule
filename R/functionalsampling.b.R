
functionalsamplingClass <- R6::R6Class(
    "functionalsamplingClass",
    inherit = functionalsamplingBase,
    private = list(
        .data = NULL,
        .results_cache = NULL,

        .init = function() {
            if (self$options$show_methodology) private$.setMethodology()
            if (self$options$show_references) private$.setReferences()
        },

        .run = function() {
            if (is.null(self$options$x_coord) || is.null(self$options$y_coord) ||
                is.null(self$options$event_type)) {
                return()
            }

            tryCatch({
                private$.prepareData()
                private$.analyzeSpatialPattern()
                private$.populateTables()
            }, error = function(e) {
                self$setError(paste("Error:", e$message))
            })
        },

        .prepareData = function() {
            raw_data <- self$data
            data_list <- list(
                x = as.numeric(raw_data[[self$options$x_coord]]),
                y = as.numeric(raw_data[[self$options$y_coord]]),
                type = as.factor(raw_data[[self$options$event_type]])
            )
            private$.data <- as.data.frame(data_list)
            private$.data <- private$.data[complete.cases(private$.data), ]
        },

        .analyzeSpatialPattern = function() {
            data <- private$.data
            n_total <- nrow(data)

            # Identify rare events
            rare_label <- self$options$rare_event
            rare_data <- data[data$type == rare_label, ]
            n_rare <- nrow(rare_data)
            freq_rare <- (n_rare / n_total) * 100

            if (n_rare < 2) {
                self$setError("Insufficient rare events for analysis (need ≥2)")
                return()
            }

            # Calculate nearest neighbor distances (NND)
            nnd <- numeric(n_rare)
            for (i in 1:n_rare) {
                distances <- sqrt((rare_data$x[i] - data$x)^2 + (rare_data$y[i] - data$y)^2)
                distances[distances == 0] <- Inf  # Exclude self
                nnd[i] <- min(distances)
            }

            mean_nnd <- mean(nnd)
            sd_nnd <- sd(nnd)
            cv_nnd <- (sd_nnd / mean_nnd) * 100

            # Clark-Evans randomness test
            # R = observed mean NND / expected mean NND for random pattern
            # For random pattern: E(r) = 0.5 / sqrt(density)
            area <- (max(data$x) - min(data$x)) * (max(data$y) - min(data$y))
            density <- n_rare / area
            expected_r <- 0.5 / sqrt(density)
            R <- mean_nnd / expected_r

            # Z-score for Clark-Evans test
            se_r <- 0.26136 / sqrt(n_rare * density)
            z_score <- (mean_nnd - expected_r) / se_r
            p_value <- 2 * (1 - pnorm(abs(z_score)))

            # Interpret spatial pattern
            if (R < 1 && p_value < 0.05) {
                pattern <- "Clustered"
            } else if (R > 1 && p_value < 0.05) {
                pattern <- "Regular/Uniform"
            } else {
                pattern <- "Random"
            }

            # Assess "catalyst" function criteria (Kayser et al., 2009)
            # 1. Rare (< threshold)
            criterion_1 <- freq_rare < self$options$frequency_threshold

            # 2. Random distribution
            criterion_2 <- pattern == "Random"

            # 3. Regular/consistent neighborhoods (low CV of NND)
            criterion_3 <- cv_nnd < 30  # Arbitrary but reasonable threshold

            # Overall conclusion
            if (criterion_1 && criterion_2 && criterion_3) {
                conclusion <- "Evidence of functional/catalyst role"
            } else if (criterion_1 && criterion_3) {
                conclusion <- "Possible functional role (not randomly distributed)"
            } else {
                conclusion <- "No evidence of catalyst function"
            }

            private$.results_cache <- list(
                n_total = n_total,
                n_rare = n_rare,
                freq_rare = freq_rare,
                mean_nnd = mean_nnd,
                sd_nnd = sd_nnd,
                cv_nnd = cv_nnd,
                R = R,
                expected_r = expected_r,
                z_score = z_score,
                p_value = p_value,
                pattern = pattern,
                criterion_1 = criterion_1,
                criterion_2 = criterion_2,
                criterion_3 = criterion_3,
                conclusion = conclusion,
                rare_data = rare_data,
                all_data = data
            )
        },

        .populateTables = function() {
            results <- private$.results_cache

            # Summary table
            if (self$options$show_summary) {
                summary_table <- self$results$summary
                classification <- if (results$freq_rare < self$options$frequency_threshold) {
                    "Rare event"
                } else {
                    "Not rare"
                }

                summary_table$setRow(rowNo = 1, values = list(
                    total_events = as.integer(results$n_total),
                    rare_events = as.integer(results$n_rare),
                    rare_frequency = as.numeric(results$freq_rare),
                    interpretation = classification
                ))
            }

            # NND table
            if (self$options$calculate_nnd) {
                nnd_table <- self$results$nnd_table
                consistency <- if (results$cv_nnd < 20) {
                    "High (regular neighborhoods)"
                } else if (results$cv_nnd < 40) {
                    "Moderate"
                } else {
                    "Low (variable neighborhoods)"
                }

                nnd_table$setRow(rowNo = 1, values = list(
                    mean_nnd = as.numeric(results$mean_nnd),
                    sd_nnd = as.numeric(results$sd_nnd),
                    cv_nnd = as.numeric(results$cv_nnd),
                    consistency = consistency
                ))
            }

            # Randomness test
            if (self$options$test_randomness) {
                random_table <- self$results$randomness_test
                random_table$setRow(rowNo = 1, values = list(
                    R = as.numeric(results$R),
                    expected_R = as.numeric(results$expected_r),
                    z_score = as.numeric(results$z_score),
                    p_value = as.numeric(results$p_value),
                    pattern = results$pattern
                ))
            }

            # Functional assessment
            if (self$options$analyze_neighborhoods) {
                func_table <- self$results$functional_assessment
                func_table$setRow(rowNo = 1, values = list(
                    criterion_1 = if (results$criterion_1) "✓ Yes" else "✗ No",
                    criterion_2 = if (results$criterion_2) "✓ Yes" else "✗ No",
                    criterion_3 = if (results$criterion_3) "✓ Yes" else "✗ No",
                    conclusion = results$conclusion
                ))
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            results <- private$.results_cache
            data <- results$all_data
            rare_data <- results$rare_data

            p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = type)) +
                ggplot2::geom_point(size = 2, alpha = 0.6) +
                ggplot2::geom_point(data = rare_data, size = 4, shape = 17, color = "red") +
                ggplot2::labs(
                    x = "X Coordinate",
                    y = "Y Coordinate",
                    title = "Spatial Distribution of Events",
                    subtitle = paste0("Rare events (red triangles): ", results$n_rare,
                                    " (", sprintf("%.1f%%", results$freq_rare), ")")
                ) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom")

            print(p)
            TRUE
        },

        .setMethodology = function() {
            html <- "
<h3>Functional Sampling Methodology (Kayser et al., 2009)</h3>

<h4>Concept: Rare Events with \"Catalyst\" Function</h4>
<p>Some rare cells in tissues may have disproportionate functional importance,
similar to catalysts in chemistry. This analysis identifies such cells based on
their spatial distribution pattern.</p>

<h4>Three Criteria for Catalyst Function</h4>

<p><strong>1. Rarity</strong></p>
<ul>
<li>Event frequency < threshold (default 5%)</li>
<li>Ensures we're analyzing truly rare events</li>
<li>Example: Galectin-1 binding cells < 5% in Kayser et al. study</li>
</ul>

<p><strong>2. Random Distribution in Reference Space</strong></p>
<ul>
<li>Tested using Clark-Evans R statistic</li>
<li>R < 1: Clustered, R = 1: Random, R > 1: Regular/Uniform</li>
<li>Random distribution suggests functional cells dispersed throughout tissue</li>
</ul>

<p><strong>3. Regular/Consistent Neighborhoods</strong></p>
<ul>
<li>Low coefficient of variation (CV) in nearest neighbor distances</li>
<li>CV < 30% indicates consistent spacing to neighboring cells</li>
<li>Suggests functional interaction radius</li>
</ul>

<h4>Interpretation</h4>

<p><strong>All 3 Criteria Met:</strong></p>
<ul>
<li>Strong evidence of catalyst/functional role</li>
<li>Rare cells randomly distributed but with consistent neighborhoods</li>
<li>Example: Regulatory T cells, stem cell niches, certain immune cells</li>
</ul>

<p><strong>Criteria 1 + 3 Only:</strong></p>
<ul>
<li>Possible functional role despite non-random distribution</li>
<li>May indicate tissue compartmentalization</li>
</ul>

<p><strong>No Criteria Met:</strong></p>
<ul>
<li>No evidence of special functional significance</li>
<li>Rare event may be artifact or transitional state</li>
</ul>

<h4>Clark-Evans Test</h4>
<p><strong>R Statistic:</strong> R = observed mean NND / expected mean NND</p>
<ul>
<li>R = 0: Perfect clustering (all points at same location)</li>
<li>R = 1: Random (Poisson) distribution</li>
<li>R = 2.15: Perfect hexagonal packing (maximum dispersion)</li>
</ul>

<h4>Clinical Example (from Kayser et al., 2009)</h4>
<p><strong>Galectin-1 Binding Cells in Lung Carcinoma:</strong></p>
<ul>
<li>Frequency: < 5% (rare) ✓</li>
<li>Mean distance between rare cells: 245 ± 198 μm (high CV = random) ✓</li>
<li>Distance to nearest neighbors: Consistent (low CV) ✓</li>
<li><strong>Conclusion:</strong> Evidence of regulatory/catalyst function</li>
<li><strong>Biological validation:</strong> Galectin-1 has growth regulatory and immunomodulatory properties</li>
</ul>
"
            self$results$methodology$setContent(html)
        },

        .setReferences = function() {
            html <- "
<h3>References</h3>

<p><strong>Kayser, K., Schultz, H., Goldmann, T., et al. (2009).</strong>
Theory of sampling and its application in tissue based diagnosis.
<em>Diagnostic Pathology</em>, 4:6. Lines 392-479 (Functional Sampling section).</p>

<h4>Spatial Statistics</h4>

<p><strong>Clark, P. J., & Evans, F. C. (1954).</strong>
Distance to nearest neighbor as a measure of spatial relationships in populations.
<em>Ecology</em>, 35(4):445-453.</p>

<p><strong>Diggle, P. J. (2003).</strong>
<em>Statistical Analysis of Spatial Point Patterns</em> (2nd ed.).
London: Arnold.</p>

<h4>Biological Applications</h4>

<p><strong>Kayser, K., & Gabius, H. J. (1997).</strong>
Graph theory and the entropy concept in histochemistry.
<em>Progress in Histochemistry and Cytochemistry</em>, 32:1-106.</p>
"
            self$results$references$setContent(html)
        }
    )
)
