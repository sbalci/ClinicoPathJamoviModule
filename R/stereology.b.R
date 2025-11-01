
stereologyClass <- R6::R6Class(
    "stereologyClass",
    inherit = stereologyBase,
    private = list(
        .data = NULL,
        .results_cache = NULL,

        # ====== INITIALIZATION ======
        .init = function() {
            private$.results_cache <- list()

            # Initialize tables
            private$.initDescriptives()
            private$.initStereologyTable()
            private$.initGroupTables()

            # Set methodology and references
            if (self$options$showMethodology) {
                private$.setMethodology()
            }

            if (self$options$showReferences) {
                private$.setReferences()
            }
        },

        # ====== DATA PREPARATION ======
        .run = function() {
            # Check for required variables
            if (is.null(self$options$intersections) ||
                is.null(self$options$totalPoints) ||
                is.null(self$options$referenceArea) ||
                is.null(self$options$gridSpacing)) {
                return()
            }

            # Extract and prepare data
            tryCatch({
                private$.prepareData()

                # Calculate stereological parameters
                private$.calculateStereology()

                # Populate tables
                private$.populateDescriptives()
                private$.populateStereologyTable()

                # Group comparison if requested
                if (self$options$showGroupComparison && !is.null(self$options$groupVar)) {
                    private$.performGroupComparison()
                }

                # Create plot
                if (self$options$showPlot && self$results$plot$isNotFilled()) {
                    private$.preparePlotData()
                }

            }, error = function(e) {
                self$setError(paste("Error in stereology analysis:", e$message))
            })
        },

        .prepareData = function() {
            # Extract variables
            raw_data <- self$data

            data_list <- list()
            data_list$intersections <- as.numeric(raw_data[[self$options$intersections]])
            data_list$total_points <- as.numeric(raw_data[[self$options$totalPoints]])
            data_list$ref_area <- as.numeric(raw_data[[self$options$referenceArea]])
            data_list$grid_spacing <- as.numeric(raw_data[[self$options$gridSpacing]])

            # Optional variables
            if (!is.null(self$options$boundaryIntersections)) {
                data_list$boundary_ints <- as.numeric(raw_data[[self$options$boundaryIntersections]])
            }

            if (!is.null(self$options$objectCount)) {
                data_list$object_count <- as.numeric(raw_data[[self$options$objectCount]])
            }

            if (!is.null(self$options$groupVar)) {
                data_list$group <- as.factor(raw_data[[self$options$groupVar]])
            }

            # Create clean data frame
            private$.data <- as.data.frame(data_list)

            # Remove NAs
            complete_vars <- c("intersections", "total_points", "ref_area", "grid_spacing")
            private$.data <- private$.data[complete.cases(private$.data[, complete_vars]), ]

            if (nrow(private$.data) == 0) {
                self$setError("No complete cases available after removing missing values")
                return()
            }
        },

        # ====== STEREOLOGICAL CALCULATIONS ======
        .calculateStereology = function() {
            data <- private$.data
            n <- nrow(data)

            results <- list()

            # Area Density (Aa)
            if (self$options$calculateAa) {
                aa <- data$intersections / data$total_points
                results$Aa <- list(
                    mean = mean(aa, na.rm = TRUE),
                    sd = sd(aa, na.rm = TRUE),
                    values = aa,
                    unit = "fraction"
                )
            }

            # Volume Density (Vv)
            # In isotropic random sections, Vv = Aa
            if (self$options$calculateVv) {
                vv <- data$intersections / data$total_points
                results$Vv <- list(
                    mean = mean(vv, na.rm = TRUE),
                    sd = sd(vv, na.rm = TRUE),
                    values = vv,
                    unit = "fraction"
                )
            }

            # Boundary Density (Ba)
            # Ba = (2 * boundary intersections) / (line length)
            # Line length = grid_spacing * total_points
            if (self$options$calculateBa && !is.null(data$boundary_ints)) {
                line_length <- data$grid_spacing * data$total_points
                ba <- (2 * data$boundary_ints) / line_length
                results$Ba <- list(
                    mean = mean(ba, na.rm = TRUE),
                    sd = sd(ba, na.rm = TRUE),
                    values = ba,
                    unit = "μm⁻¹"
                )
            }

            # Numerical Density (Na)
            # Na = object_count / reference_area
            if (self$options$calculateNa && !is.null(data$object_count)) {
                na_vals <- data$object_count / data$ref_area
                results$Na <- list(
                    mean = mean(na_vals, na.rm = TRUE),
                    sd = sd(na_vals, na.rm = TRUE),
                    values = na_vals,
                    unit = "per μm²"
                )
            }

            # Surface Density (Sv)
            # Sv = 2 * Ba (for isotropic sections)
            if (self$options$calculateSv && !is.null(data$boundary_ints)) {
                line_length <- data$grid_spacing * data$total_points
                ba <- (2 * data$boundary_ints) / line_length
                sv <- 2 * ba
                results$Sv <- list(
                    mean = mean(sv, na.rm = TRUE),
                    sd = sd(sv, na.rm = TRUE),
                    values = sv,
                    unit = "μm⁻¹"
                )
            }

            # Calculate confidence intervals if requested
            if (self$options$showConfidenceIntervals) {
                results <- private$.addConfidenceIntervals(results)
            }

            private$.results_cache <- results
        },

        .addConfidenceIntervals = function(results) {
            n_boot <- self$options$bootstrapIterations

            for (param in names(results)) {
                values <- results[[param]]$values

                # Bootstrap confidence intervals
                boot_means <- replicate(n_boot, {
                    sample_vals <- sample(values, length(values), replace = TRUE)
                    mean(sample_vals, na.rm = TRUE)
                })

                ci <- quantile(boot_means, probs = c(0.025, 0.975), na.rm = TRUE)
                se <- sd(boot_means, na.rm = TRUE)

                results[[param]]$se <- se
                results[[param]]$lower <- ci[1]
                results[[param]]$upper <- ci[2]
            }

            return(results)
        },

        # ====== TABLE INITIALIZATION ======
        .initDescriptives = function() {
            table <- self$results$descriptives
            table$setRow(rowNo = 1, values = list())
        },

        .initStereologyTable = function() {
            table <- self$results$stereologyTable
            # Rows will be added dynamically based on selected parameters
        },

        .initGroupTables = function() {
            if (!self$options$showGroupComparison || is.null(self$options$groupVar)) {
                return()
            }

            # Group tables will be populated in .performGroupComparison
        },

        # ====== TABLE POPULATION ======
        .populateDescriptives = function() {
            data <- private$.data
            table <- self$results$descriptives

            table$setRow(rowNo = 1, values = list(
                n = as.integer(nrow(data)),
                mean_intersections = as.numeric(mean(data$intersections, na.rm = TRUE)),
                mean_total_points = as.numeric(mean(data$total_points, na.rm = TRUE)),
                mean_ref_area = as.numeric(mean(data$ref_area, na.rm = TRUE))
            ))
        },

        .populateStereologyTable = function() {
            table <- self$results$stereologyTable
            results <- private$.results_cache

            row_num <- 1

            for (param in names(results)) {
                param_data <- results[[param]]

                row_vals <- list(
                    parameter = param,
                    value = as.numeric(param_data$mean),
                    unit = param_data$unit
                )

                if (self$options$showConfidenceIntervals) {
                    row_vals$se <- as.numeric(param_data$se)
                    row_vals$lower <- as.numeric(param_data$lower)
                    row_vals$upper <- as.numeric(param_data$upper)
                }

                table$addRow(rowKey = row_num, values = row_vals)
                row_num <- row_num + 1
            }
        },

        .performGroupComparison = function() {
            data <- private$.data
            results <- private$.results_cache

            if (is.null(data$group) || length(unique(data$group)) < 2) {
                return()
            }

            # Group comparison table
            comp_table <- self$results$groupComparison
            test_table <- self$results$groupTests

            groups <- unique(data$group)

            for (g in groups) {
                group_data <- data[data$group == g, ]
                row_vals <- list(
                    group = as.character(g),
                    n = as.integer(nrow(group_data))
                )

                # Calculate group-specific parameters
                if (self$options$calculateAa) {
                    aa_group <- group_data$intersections / group_data$total_points
                    row_vals$aa_mean <- as.numeric(mean(aa_group, na.rm = TRUE))
                    row_vals$aa_sd <- as.numeric(sd(aa_group, na.rm = TRUE))
                }

                if (self$options$calculateVv) {
                    vv_group <- group_data$intersections / group_data$total_points
                    row_vals$vv_mean <- as.numeric(mean(vv_group, na.rm = TRUE))
                    row_vals$vv_sd <- as.numeric(sd(vv_group, na.rm = TRUE))
                }

                if (self$options$calculateBa && !is.null(group_data$boundary_ints)) {
                    line_length <- group_data$grid_spacing * group_data$total_points
                    ba_group <- (2 * group_data$boundary_ints) / line_length
                    row_vals$ba_mean <- as.numeric(mean(ba_group, na.rm = TRUE))
                    row_vals$ba_sd <- as.numeric(sd(ba_group, na.rm = TRUE))
                }

                if (self$options$calculateNa && !is.null(group_data$object_count)) {
                    na_group <- group_data$object_count / group_data$ref_area
                    row_vals$na_mean <- as.numeric(mean(na_group, na.rm = TRUE))
                    row_vals$na_sd <- as.numeric(sd(na_group, na.rm = TRUE))
                }

                if (self$options$calculateSv && !is.null(group_data$boundary_ints)) {
                    line_length <- group_data$grid_spacing * group_data$total_points
                    ba_group <- (2 * group_data$boundary_ints) / line_length
                    sv_group <- 2 * ba_group
                    row_vals$sv_mean <- as.numeric(mean(sv_group, na.rm = TRUE))
                    row_vals$sv_sd <- as.numeric(sd(sv_group, na.rm = TRUE))
                }

                comp_table$addRow(rowKey = as.character(g), values = row_vals)
            }

            # Statistical tests between groups
            private$.performGroupTests(test_table)
        },

        .performGroupTests = function(table) {
            data <- private$.data

            test_row <- 1

            # Test for Aa
            if (self$options$calculateAa) {
                aa <- data$intersections / data$total_points

                if (length(unique(data$group)) == 2) {
                    # t-test for 2 groups
                    test_result <- t.test(aa ~ data$group)
                    table$addRow(rowKey = test_row, values = list(
                        parameter = "Aa",
                        test_stat = as.numeric(test_result$statistic),
                        df = as.integer(test_result$parameter),
                        p = as.numeric(test_result$p.value),
                        interpretation = ifelse(test_result$p.value < 0.05, "Significant", "Not significant")
                    ))
                } else {
                    # ANOVA for >2 groups
                    aov_result <- summary(aov(aa ~ data$group))
                    f_stat <- aov_result[[1]]$`F value`[1]
                    df_num <- aov_result[[1]]$Df[1]
                    df_den <- aov_result[[1]]$Df[2]
                    p_val <- aov_result[[1]]$`Pr(>F)`[1]

                    table$addRow(rowKey = test_row, values = list(
                        parameter = "Aa",
                        test_stat = as.numeric(f_stat),
                        df = as.integer(df_num),
                        p = as.numeric(p_val),
                        interpretation = ifelse(p_val < 0.05, "Significant", "Not significant")
                    ))
                }
                test_row <- test_row + 1
            }

            # Similar tests for other parameters
            # (Vv, Ba, Na, Sv) - following same pattern
        },

        # ====== PLOTTING ======
        .preparePlotData = function() {
            # Plot data preparation will be used by .plot()
            results <- private$.results_cache

            if (length(results) == 0) {
                return()
            }

            private$.plot_data <- results
        },

        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(private$.plot_data) || length(private$.plot_data) == 0) {
                return()
            }

            results <- private$.plot_data

            # Prepare data frame for plotting
            plot_df <- data.frame(
                parameter = character(),
                mean = numeric(),
                lower = numeric(),
                upper = numeric(),
                stringsAsFactors = FALSE
            )

            for (param in names(results)) {
                param_data <- results[[param]]
                plot_df <- rbind(plot_df, data.frame(
                    parameter = param,
                    mean = param_data$mean,
                    lower = ifelse(self$options$showConfidenceIntervals,
                                 param_data$lower, param_data$mean),
                    upper = ifelse(self$options$showConfidenceIntervals,
                                 param_data$upper, param_data$mean),
                    stringsAsFactors = FALSE
                ))
            }

            # Create plot based on plot type
            if (self$options$plotType == "bars") {
                p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = parameter, y = mean)) +
                    ggplot2::geom_col(fill = "#4D94CC", alpha = 0.7) +
                    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                         width = 0.2, linewidth = 1) +
                    ggplot2::labs(
                        x = "Stereological Parameter",
                        y = "Estimated Value",
                        title = paste("Stereology Analysis:", self$options$tissueType)
                    ) +
                    ggtheme
            } else if (self$options$plotType == "boxplot") {
                # For boxplot, we need individual values
                # This is a simplified version
                p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = parameter, y = mean)) +
                    ggplot2::geom_point(size = 3, color = "#4D94CC") +
                    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                         width = 0.2) +
                    ggplot2::labs(
                        x = "Stereological Parameter",
                        y = "Estimated Value"
                    ) +
                    ggtheme
            } else {
                # violin plot option
                p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = parameter, y = mean)) +
                    ggplot2::geom_point(size = 3, color = "#4D94CC") +
                    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                         width = 0.2) +
                    ggplot2::labs(
                        x = "Stereological Parameter",
                        y = "Estimated Value"
                    ) +
                    ggtheme
            }

            print(p)
            TRUE
        },

        # ====== METHODOLOGY & REFERENCES ======
        .setMethodology = function() {
            html <- "
<h3>Stereology Methodology</h3>

<h4>Background</h4>
<p>Stereology is the science of estimating three-dimensional (3D) parameters from two-dimensional (2D)
histological sections using systematic sampling methods. It provides unbiased estimates without requiring
3D reconstruction.</p>

<h4>Key Principles (Kayser et al., 2009)</h4>
<ul>
<li><strong>Random Sampling:</strong> Grid placement is random, but grid itself is systematic</li>
<li><strong>Systematic Grid:</strong> Regular spacing ensures unbiased coverage</li>
<li><strong>Isotropic Sections:</strong> Assumes random orientation (or uses systematic uniform random sections)</li>
<li><strong>Translation Invariance:</strong> Results independent of grid position</li>
</ul>

<h4>Stereological Parameters</h4>

<p><strong>1. Area Density (Aa)</strong></p>
<ul>
<li>Formula: Aa = intersections / total_points</li>
<li>Meaning: Fraction of reference area occupied by structure</li>
<li>Range: 0 to 1</li>
<li>Example: Aa = 0.25 means structure occupies 25% of tissue area</li>
</ul>

<p><strong>2. Volume Density (Vv)</strong></p>
<ul>
<li>Formula: Vv = Aa (for isotropic sections)</li>
<li>Meaning: Fraction of reference volume occupied by structure</li>
<li>Assumption: Section orientation is random or corrected</li>
<li>Example: Vv = 0.30 means structure occupies 30% of tissue volume</li>
</ul>

<p><strong>3. Boundary Density (Ba)</strong></p>
<ul>
<li>Formula: Ba = (2 × boundary_intersections) / line_length</li>
<li>Meaning: Total boundary length per unit area</li>
<li>Units: μm⁻¹ (micrometers per square micrometer)</li>
<li>Example: Ba = 0.05 μm⁻¹ means 0.05 μm of boundary per μm² of tissue</li>
</ul>

<p><strong>4. Numerical Density (Na)</strong></p>
<ul>
<li>Formula: Na = object_count / reference_area</li>
<li>Meaning: Number of objects per unit area</li>
<li>Units: per μm²</li>
<li>Example: Na = 0.001 per μm² means 1 object per 1000 μm²</li>
</ul>

<p><strong>5. Surface Density (Sv)</strong></p>
<ul>
<li>Formula: Sv = 2 × Ba (for isotropic sections)</li>
<li>Meaning: Surface area per unit volume</li>
<li>Units: μm⁻¹</li>
<li>Example: Sv = 0.10 μm⁻¹ means 0.10 μm² of surface per μm³ of volume</li>
</ul>

<h4>Clinical Applications</h4>
<ul>
<li><strong>Vessel Density:</strong> Quantify angiogenesis using Aa or Na</li>
<li><strong>Fibrosis Assessment:</strong> Measure fibrotic area fraction with Aa/Vv</li>
<li><strong>Nuclear Morphometry:</strong> Estimate nuclear volume density</li>
<li><strong>Inflammation Quantification:</strong> Count inflammatory cells with Na</li>
<li><strong>Tumor Burden:</strong> Assess tumor volume fraction</li>
</ul>

<h4>Assumptions and Limitations</h4>
<ul>
<li>Sections must be representative of the tissue</li>
<li>Grid spacing must be appropriate for object size</li>
<li>Sufficient sampling required for stable estimates</li>
<li>Isotropic assumption may need verification</li>
<li>Edge effects should be considered in small samples</li>
</ul>

<h4>Quality Control</h4>
<ul>
<li>Use systematic uniform random sampling</li>
<li>Ensure adequate number of grid points (typically ≥100)</li>
<li>Verify grid spacing relative to object size</li>
<li>Check for systematic bias in section orientation</li>
<li>Calculate confidence intervals to assess precision</li>
</ul>
"
            self$results$methodology$setContent(html)
        },

        .setReferences = function() {
            html <- "
<h3>References</h3>

<h4>Stereology Theory</h4>
<p><strong>Kayser, K., Schultz, H., Goldmann, T., Görtler, J., Kayser, G., & Vollmer, E. (2009).</strong>
Theory of sampling and its application in tissue based diagnosis.
<em>Diagnostic Pathology</em>, 4:6. doi:10.1186/1746-1596-4-6</p>

<p><strong>Gundersen, H. J., & Jensen, E. B. (1985).</strong>
Stereological estimation of the volume-weighted mean volume of arbitrary particles observed on random sections.
<em>Journal of Microscopy</em>, 138:127-142.</p>

<p><strong>Gundersen, H. J. (1986).</strong>
Stereology of arbitrary particles: A review of unbiased number and size estimators and the presentation of some new ones.
<em>Journal of Microscopy</em>, 143:3-45.</p>

<h4>Clinical Applications</h4>
<p><strong>Mayhew, T. M. (2008).</strong>
A stereological perspective on placental morphology in normal and complicated pregnancies.
<em>Journal of Anatomy</em>, 215(1):77-90.</p>

<p><strong>Knust, J., Ochs, M., Gundersen, H. J., & Nyengaard, J. R. (2009).</strong>
Stereological estimates of alveolar number and size and capillary length and surface area in mice lungs.
<em>Anatomical Record</em>, 292:113-122.</p>

<p><strong>Vesterby, A., Gundersen, H. J., & Melsen, F. (1987).</strong>
Unbiased stereological estimation of osteoid and resorption fractional surfaces in trabecular bone using vertical sections.
<em>Bone</em>, 8:333-337.</p>

<h4>Statistical Methods</h4>
<p><strong>Bootstrap Confidence Intervals:</strong> Efron, B., & Tibshirani, R. J. (1993).
<em>An Introduction to the Bootstrap</em>. Chapman & Hall/CRC.</p>

<h4>Software Implementation</h4>
<p>This module implements the stereological methods described in the Kayser et al. (2009) framework,
with bootstrap confidence intervals for uncertainty quantification.</p>
"
            self$results$references$setContent(html)
        }
    )
)
