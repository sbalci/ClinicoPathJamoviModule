# This file is a generated template, your changes will not be overwritten

ihcheterogeneityClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ihcheterogeneityClass",
    inherit = ihcheterogeneityBase,
    private = list(
        # Clinical threshold constants
        .CLINICAL_CONSTANTS = list(
            CV_LOW_THRESHOLD = 15,          # CV threshold for low variability
            CV_MODERATE_THRESHOLD = 30,     # CV threshold for moderate variability
            CV_QUALITY_CONTROL = 30,        # CV threshold for QC flagging
            CORRELATION_GOOD = 0.80,        # Good correlation threshold
            CORRELATION_MODERATE = 0.70,    # Moderate correlation threshold
            CORRELATION_POOR = 0.60,        # Poor correlation threshold
            MIN_CASES_ICC = 3,              # Minimum cases for ICC calculation
            MIN_CASES_ANALYSIS = 5,         # Minimum cases for analysis
            # A statistically significant offset is not automatically a clinically
            # important one; require it to exceed this share of the reference mean
            # before it vetoes the adequacy verdict. 5% matches the "Minimal (<5%)"
            # band the sampling-bias table already uses for clinical impact.
            RELATIVE_BIAS_MATERIAL = 5
        ),

        .repro_stats = NULL,

        # Accumulators for data-quality warnings and sampling-strategy notes so
        # they are merged into the final interpretation instead of being
        # clobbered (Html $state is always NULL and cannot be read back).
        .warnings_html = NULL,
        .strategy_notes = NULL,

        .icc_consistency = NULL,

        # Per-case coefficient of variation - ONE definition, used everywhere.
        #
        # The reproducibility table computed this from the regional columns only
        # while .calculateInterpretationMetrics() folded the reference measurement
        # into each case. The two numbers then disagreed on the same screen: the
        # table read "Mean CV = 23.19 / High variability" while the copy-ready
        # sentence read "moderate (mean CV = 20%)" and the assessment box read
        # "ADEQUATE SAMPLING". When a reference exists it MUST be included - the
        # question is whether a region reproduces the whole section, so excluding
        # the whole section makes a systematic under-read invisible (a 30%
        # under-read showed as 1.2% variability).
        # Systematic difference as a percentage of the reference mean.
        # Paired t-test that cannot abort the analysis.
        #
        # t.test(paired=TRUE) errors with "data are essentially constant" when the
        # difference vector has zero variance - routine when scores are binned to
        # whole percentages and every case shows the same offset. The error was
        # unguarded in four places and took the WHOLE analysis down.
        .safePairedT = function(x, y) {
            d <- x - y
            d <- d[!is.na(d)]
            if (length(d) < 2)
                return(list(estimate = if (length(d)) d[1] else NA_real_, p.value = NA_real_))
            if (stats::sd(d) < .Machine$double.eps * max(1, abs(mean(d))))
                return(list(estimate = mean(d),
                            p.value = if (abs(mean(d)) > 0) 0 else NA_real_))
            tryCatch(stats::t.test(x, y, paired = TRUE),
                     error = function(e) list(estimate = mean(d), p.value = NA_real_))
        },

        .relativeBias = function(metrics) {
            if (is.null(metrics$mean_bias) || is.na(metrics$mean_bias) ||
                is.null(metrics$ref_mean) || is.na(metrics$ref_mean) ||
                abs(metrics$ref_mean) < 1e-6) return(NA_real_)
            abs(metrics$mean_bias) / abs(metrics$ref_mean) * 100
        },

        .perCaseCV = function(whole_section, biopsy_data, has_reference) {
            if (nrow(biopsy_data) == 0) return(numeric(0))
            rows <- split(as.data.frame(biopsy_data), seq_len(nrow(biopsy_data)))
            if (has_reference && !is.null(whole_section)) {
                as.numeric(mapply(function(w, r)
                    private$.calculateRobustCV(c(w, as.numeric(r))),
                    whole_section, rows))
            } else {
                as.numeric(vapply(rows, function(r)
                    private$.calculateRobustCV(as.numeric(r)), numeric(1)))
            }
        },

        .calculateRobustCV = function(values) {
            # Robust CV calculation with safeguards against division by near-zero means
            values <- values[!is.na(values)]
            if (length(values) < 2) return(NA)

            mean_val <- mean(values)
            sd_val <- sd(values)

            # Guard against division by near-zero mean
            if (abs(mean_val) < 1e-6) {
                return(NA)
            }

            cv <- (sd_val / abs(mean_val)) * 100

            # Cap extreme CVs (>500% likely indicates data issues)
            if (!is.finite(cv) || cv > 500) {
                return(NA)
            }

            return(cv)
        },

        .init = function() {
            # Populate welcome screen
            self$results$welcome$setContent("
                <div class='jmv-welcome' style='padding: 20px; background-color: rgba(138, 155, 172, 0.06); border-left: 4px solid #007bff; color: inherit;'>
                    <h3 style='margin-top: 0;'> IHC Heterogeneity Analysis</h3>
                    <p><strong>Get started:</strong></p>
                    <ol>
                        <li>Select <strong>Regional Measurement 1</strong> (required)</li>
                        <li>Optionally add reference measurement (whole slide/hotspot)</li>
                        <li>Add more regional measurements (2-4 or use Additional Measurements)</li>
                        <li>Optionally specify <strong>Spatial Region ID</strong> for compartment analysis</li>
                    </ol>
                    <p style='margin-bottom: 0;'><em>Configure thresholds and options in the left panel.</em></p>
                </div>
            ")

            # Hide the welcome screen whenever any regional measurement is
            # supplied. The plural 'biopsies' list cannot be referenced from the
            # r.yaml 'visible' expression, so drive visibility here instead.
            self$results$welcome$setVisible(
                is.null(self$options$biopsy1) &&
                (is.null(self$options$biopsies) || length(self$options$biopsies) == 0)
            )

            # Fixed row structure for the variance component table: three
            # components plus a total, every run. Two of the component labels
            # depend on whether a reference measurement was supplied, which is
            # an option, not a result - so the whole structure belongs here.
            # Placed before the no-data return so every .init() path builds it.
            has_reference <- !is.null(self$options$wholesection)
            variance_components <- c(
                if (has_reference) "Between-Case Variance" else "Between-Case Variance (Regional Means)",
                "Within-Case Variance (Sampling)",
                if (has_reference) "Method Variance" else "Regional Method Variance",
                "Total Variance"
            )
            for (r in 1:4)
                self$results$variancetable$addRow(
                    rowKey = r, values = list(component = variance_components[r]))

            if (is.null(self$data)) {
                self$results$interpretation$setContent(
                    "<h3>IHC Heterogeneity Analysis for Digital Pathology</h3>
                    <p><strong>Purpose:</strong> Quantify spatial heterogeneity in continuous immunohistochemical (IHC) biomarker
                    expression across different tissue regions, supporting both reference-based and inter-regional
                    comparison studies for continuous biomarker measurements.</p>

                    <h4>Study Design Options:</h4>
                    <ul>
                        <li><strong>Reference-Based Study:</strong> Compare regional measurements to a reference (whole section, hotspot, or overall measurement)</li>
                        <li><strong>Inter-Regional Study:</strong> Compare regional measurements among themselves without a reference</li>
                    </ul>

                    <h4>Data Requirements:</h4>
                    <ul>
                        <li><strong>Reference Measurement (Optional):</strong> Continuous biomarker value from reference region for reference-based studies</li>
                        <li><strong>Regional Measurements:</strong> At least 2 continuous measurements from different tissue regions</li>
                        <li><strong>Optional:</strong> Spatial coordinates, region identifiers, sampling methodology information</li>
                    </ul>

                    <h4>Heterogeneity Assessment Framework:</h4>
                    <ul>
                        <li><strong>Spatial Variability:</strong> Quantify measurement variance across tissue regions</li>
                        <li><strong>Inter-Regional Reproducibility:</strong> Assess consistency between different regional measurements</li>
                        <li><strong>Reference Comparison:</strong> Evaluate how well regions represent reference values (when available)</li>
                        <li><strong>Clinical Impact:</strong> Effect on biomarker interpretation and diagnostic decisions</li>
                        <li><strong>Quality Control:</strong> Identify regions with excessive heterogeneity</li>
                    </ul>

                    <h4>Statistical Analysis:</h4>
                    <ul>
                        <li><strong>Correlation Analysis:</strong> Spearman correlations between reference and regional measurements (reference-based) or among regions (inter-regional)</li>
                        <li><strong>Reliability Assessment:</strong> Intraclass correlation coefficient (ICC) for measurement consistency</li>
                        <li><strong>Variability Decomposition:</strong> Between-region variance components and coefficient of variation</li>
                        <li><strong>Bias Detection:</strong> Systematic differences in measurements (when reference available)</li>
                        <li><strong>Power Analysis:</strong> Sample size recommendations for heterogeneity studies</li>
                    </ul>

                    <h4>Clinical Applications:</h4>
                    <ul>
                        <li>Tumor heterogeneity quantification for continuous biomarkers (Ki67 %, ER/PR H-scores)</li>
                        <li>Quality assessment of IHC staining uniformity</li>
                        <li>Validation of sampling adequacy for biomarker measurements</li>
                        <li>Optimization of tissue analysis protocols</li>
                        <li>Assessment of inter-observer measurement reliability</li>
                    </ul>

                    <p><em>This analysis is designed for continuous IHC measurements and implements statistical methods
                    for heterogeneity assessment in digital pathology workflows.</em></p>"
                )
                return()
            }
            
            # Set conditional visibility based on options and analysis type
            analysis_type <- self$options$analysis_type
            show_plots <- self$options$show_variability_plots ||
                         analysis_type == "variability" ||
                         analysis_type == "comprehensive"

            self$results$biopsyplot$setVisible(show_plots)
            self$results$variabilityplot$setVisible(show_plots)

            # Set conditional plot visibility for spatial analysis
            self$results$spatialplot$setVisible(
                show_plots && !is.null(self$options$spatial_id)
            )

            # Set conditional table visibility for power analysis and variance components
            show_power <- self$options$power_analysis || analysis_type == "comprehensive"
            show_variance <- self$options$variance_components ||
                           analysis_type == "variability" ||
                           analysis_type == "comprehensive"

            # Gate the power table on a reference being supplied: power analysis
            # returns nothing for inter-regional (no-reference) studies, so it
            # would otherwise show an empty-but-visible table by default.
            self$results$poweranalysistable$setVisible(show_power && !is.null(self$options$wholesection))
            self$results$variancetable$setVisible(show_variance)
            self$results$spatialanalysistable$setVisible(!is.null(self$options$spatial_id))

            # Set visibility for summary and glossary
            self$results$summary$setVisible(self$options$showSummary)
            self$results$glossary$setVisible(self$options$showGlossary)

            # Populate statistical glossary if requested
            if (self$options$showGlossary) {
                private$.populateGlossary()
            }
        },
        
        .run = function() {
            # Unset Variable options cannot be tested reliably with a leading `!`
            # in r.yaml (the expression is silently treated as always visible).
            # Keep the welcome panel synchronized here on every option change.
            has_regional_measurement <-
                !is.null(self$options$biopsy1) ||
                (!is.null(self$options$biopsies) && length(self$options$biopsies) > 0)
            self$results$welcome$setVisible(!has_regional_measurement)

            # Check required variables - need at least 2 regional measurements for analysis
            # wholesection is now optional (for inter-regional studies)
            if (!has_regional_measurement) {
                return()
            }

            # Count available regional measurements
            regional_count <- sum(!sapply(list(self$options$biopsy1, self$options$biopsy2,
                                             self$options$biopsy3, self$options$biopsy4), is.null))
            if (!is.null(self$options$biopsies)) {
                regional_count <- regional_count + length(self$options$biopsies)
            }

            if (regional_count < 2) {
                jmvcore::reject("At least 2 regional measurements are required for heterogeneity analysis.")
            }
            
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Extract reference data (optional)
            whole_section <- if (!is.null(self$options$wholesection)) {
                data[[self$options$wholesection]]
            } else {
                NULL
            }

            # Handle multiple regional measurement columns
            regional <- private$.extractRegionalData(data)
            biopsy_data <- regional$data
            keep_rows <- regional$keep

            # Align the reference vector with the rows retained after dropping
            # all-NA regional cases, so positional indexing, cbind() and
            # complete.cases() stay row-aligned with biopsy_data.
            if (!is.null(whole_section)) {
                whole_section <- whole_section[keep_rows]
            }

            # Enhanced data quality checks with specific warnings
            min_cases_needed <- 5
            has_reference <- !is.null(whole_section)

            # Check data sufficiency based on study design
            if (has_reference && (length(whole_section) < min_cases_needed || nrow(biopsy_data) < min_cases_needed)) {
                jmvcore::reject("Insufficient data for reference-based heterogeneity analysis. At least 5 complete cases with reference and regional measurements are required.")
            } else if (!has_reference && nrow(biopsy_data) < min_cases_needed) {
                jmvcore::reject("Insufficient data for inter-regional heterogeneity analysis. At least 5 complete cases with regional measurements are required.")
            }

            # Add misuse detection warnings
            warnings <- private$.detectMisuse(whole_section, biopsy_data)
            if (length(warnings) > 0) {
                warning_html <- paste0(
                    "<div style='background-color: rgba(255, 202, 33, 0.23); border: 1px solid #ffeaa7; padding: 15px; border-radius: 5px; margin: 10px 0; color: inherit;'>",
                    "<h4 style='color: #856404; margin-top: 0;'> Data Quality Warnings</h4>",
                    "<ul style='color: #856404; margin: 5px 0; padding-left: 20px;'>",
                    paste0("<li>", warnings, "</li>", collapse = ""),
                    "</ul>",
                    "</div>"
                )
                # Store for merging into the final interpretation. Html $state is
                # always NULL, and .generateHeterogeneityInterpretation() would
                # otherwise overwrite any setContent() issued here.
                private$.warnings_html <- warning_html
            } else {
                private$.warnings_html <- NULL
            }
            
            # Get optional spatial data
            spatial_regions <- NULL
            if (!is.null(self$options$spatial_id)) {
                spatial_id_var <- self$options$spatial_id
                if (spatial_id_var %in% names(data)) {
                    # Align with the retained biopsy rows (see keep_rows above)
                    spatial_regions <- data[[spatial_id_var]][keep_rows]
                } else {
                    # Handle case where spatial_id is selected but not in data.
                    # reject() content renders as plain text (jamovi escapes it), so no htmlEscape needed.
                    jmvcore::reject(paste0("Spatial ID variable '", self$options$spatial_id, "' not found in data."))
                }
            }
            
            # Determine study design and perform appropriate analysis
            study_design <- if (has_reference) "reference_based" else "inter_regional"

            # Perform heterogeneity analysis based on study design
            private$.performHeterogeneityAnalysis(
                whole_section = whole_section,
                biopsy_data = biopsy_data,
                spatial_regions = spatial_regions,
                study_design = study_design
            )

            # Generate plots and interpretation
            private$.generateHeterogeneityPlots(whole_section, biopsy_data, spatial_regions, study_design)
            private$.generateHeterogeneityInterpretation(whole_section, biopsy_data, study_design)
        },
        
        .extractRegionalData = function(data) {
            # Extract biopsy measurements from multiple columns efficiently
            # Collect individual biopsy columns
            individual_biopsies <- list(self$options$biopsy1, self$options$biopsy2,
                                       self$options$biopsy3, self$options$biopsy4)
            individual_biopsies <- individual_biopsies[!sapply(individual_biopsies, is.null)]

            # Combine with additional biopsy columns
            additional_cols <- if (!is.null(self$options$biopsies)) self$options$biopsies else c()
            biopsy_columns <- c(unlist(individual_biopsies), additional_cols)

            # Create biopsy data matrix (jamovi retains literal column names)
            biopsy_data <- data[, biopsy_columns, drop = FALSE]
            
            # Remove rows with all missing biopsy values
            complete_rows <- rowSums(!is.na(biopsy_data)) > 0
            biopsy_data <- biopsy_data[complete_rows, , drop = FALSE]

            # Return the retained-row mask so callers can align the reference and
            # spatial vectors with the filtered biopsy rows.
            return(list(data = biopsy_data, keep = complete_rows))
        },
        
        .performHeterogeneityAnalysis = function(whole_section, biopsy_data, spatial_regions = NULL, study_design = "reference_based") {
            n_biopsies <- ncol(biopsy_data)
            n_cases <- nrow(biopsy_data)
            has_reference <- !is.null(whole_section)

            # Get user-defined thresholds
            cv_threshold <- self$options$cv_threshold
            correlation_threshold <- self$options$correlation_threshold

            # Store analysis type for later use
            analysis_type <- self$options$analysis_type

            # 1. Reproducibility Analysis (adapted for study design)
            repro_results <- if (has_reference) {
                # Reference-based analysis: compare regions to reference
                private$.analyzeReproducibility(whole_section, biopsy_data, correlation_threshold, cv_threshold, "reference_based")
            } else {
                # Inter-regional analysis: compare regions among themselves
                private$.analyzeReproducibility(NULL, biopsy_data, correlation_threshold, cv_threshold, "inter_regional")
            }
            private$.repro_stats <- repro_results
            
            # 2. Sampling Bias Analysis  
            private$.analyzeSamplingBias(whole_section, biopsy_data)
            
            # 3. Variance Component Analysis (if enabled or required by analysis type)
            if (self$options$variance_components ||
                analysis_type == "variability" ||
                analysis_type == "comprehensive") {
                private$.analyzeVarianceComponents(whole_section, biopsy_data)
            }

            # 4. Power Analysis (if enabled or required by analysis type)
            if (self$options$power_analysis ||
                analysis_type == "comprehensive") {
                private$.performPowerAnalysis(whole_section, biopsy_data)
            }
            
            # 5. Spatial Analysis (if spatial data provided)
            if (!is.null(spatial_regions)) {
                private$.analyzeSpatialHeterogeneity(whole_section, biopsy_data, spatial_regions)
            }

            # 6. Compartment Comparison (if enabled and spatial data available)
            if (self$options$compareCompartments && !is.null(spatial_regions)) {
                private$.compareCompartments(whole_section, biopsy_data, spatial_regions)
            }

            # 7. Compartment Statistical Tests (if enabled and spatial data available)
            if (self$options$compartmentTests && !is.null(spatial_regions)) {
                private$.performCompartmentTests(whole_section, biopsy_data, spatial_regions)
            }

            # Apply sampling strategy-specific adjustments
            sampling_strategy <- self$options$sampling_strategy

            # Store interpretation for modification
            interpretation_text <- ""

            # Add analysis-type-specific interpretation
            if (analysis_type == "bias") {
                interpretation_text <- paste(interpretation_text,
                    "\n\nBias Analysis Focus: This analysis emphasizes detection of systematic differences and bias patterns between sampling methods.",
                    sep="")
            } else if (analysis_type == "variability") {
                interpretation_text <- paste(interpretation_text,
                    "\n\nVariability Analysis Focus: This analysis emphasizes variance components and spatial heterogeneity assessment.",
                    sep="")
            } else if (analysis_type == "comprehensive") {
                interpretation_text <- paste(interpretation_text,
                    "\n\nComprehensive Analysis: All analysis modules (reproducibility, bias, variability, and power) have been enabled.",
                    sep="")
            }

            if (sampling_strategy == "systematic") {
                # Add systematic sampling bias warnings
                interpretation_text <- paste(interpretation_text,
                    "\n\nNote: Systematic sampling may introduce spatial bias in heterogeneity estimates.",
                    "Consider correlation with tissue architecture patterns.", sep="")

            } else if (sampling_strategy == "stratified") {
                # Account for stratified sampling in interpretation
                interpretation_text <- paste(interpretation_text,
                    "\n\nNote: Stratified sampling design has been considered in the analysis.",
                    "Results are adjusted for sampling design effects.", sep="")

            } else if (sampling_strategy == "unknown") {
                # Add uncertainty warnings
                interpretation_text <- paste(interpretation_text,
                    "\n\nWarning: Unknown sampling strategy limits interpretation reliability.",
                    "Consider documenting sampling methodology for future analyses.", sep="")
            }

            # Store sampling-strategy / analysis-type notes for inclusion in the
            # final interpretation. Html $state is always NULL and a setContent()
            # here would be clobbered by .generateHeterogeneityInterpretation().
            private$.strategy_notes <- if (nchar(interpretation_text) > 0) interpretation_text else NULL
        },
        
        .analyzeReproducibility = function(whole_section, biopsy_data, correlation_threshold = 0.80, cv_threshold = 20.0, study_design = "reference_based") {
            n_biopsies <- ncol(biopsy_data)
            n_cases <- nrow(biopsy_data)
            has_reference <- !is.null(whole_section) && study_design == "reference_based"

            # Initialize correlation variables
            correlations <- c()

            # Compute the inter-region correlation matrix once and reuse it for
            # both the inter-regional correlations and the mean inter-biopsy
            # reproducibility metric below.
            inter_biopsy_corr <- numeric(0)
            if (n_biopsies >= 2) {
                biopsy_cor_matrix <- cor(biopsy_data, use = "pairwise.complete.obs", method = "spearman")
                upper_tri_indices <- which(upper.tri(biopsy_cor_matrix), arr.ind = TRUE)
                inter_biopsy_corr <- biopsy_cor_matrix[upper_tri_indices]
                inter_biopsy_corr <- inter_biopsy_corr[!is.na(inter_biopsy_corr)]
            }

            if (has_reference) {
                # Reference-based study: correlations between reference and each regional measurement
                combined_data <- cbind(whole_section, biopsy_data)
                colnames(combined_data) <- c("reference", paste0("region_", 1:n_biopsies))

                # Calculate all correlations at once
                if (nrow(combined_data) >= 3) {
                    all_correlations <- cor(combined_data, use = "pairwise.complete.obs", method = "spearman")
                    # Extract correlations between reference and each regional measurement
                    correlations <- all_correlations[1, -1]  # First row, excluding self-correlation
                    correlations <- correlations[!is.na(correlations)]
                } else {
                    correlations <- rep(NA, n_biopsies)
                }
            } else {
                # Inter-regional study: reuse the precomputed pairwise correlations
                correlations <- if (n_biopsies >= 2) inter_biopsy_corr else NA
            }
            
            # Calculate ICC with proper validation and fallback
            icc_result <- private$.calculateICC(
                whole_section = whole_section,
                biopsy_data = biopsy_data,
                correlations = correlations,
                has_reference = has_reference,
                n_biopsies = n_biopsies
            )

            icc_value <- icc_result$value
            icc_lower <- icc_result$lower
            icc_upper <- icc_result$upper
            
            # Inter-regional reproducibility (reuse the precomputed matrix)
            mean_inter_biopsy <- if (length(inter_biopsy_corr) > 0) mean(inter_biopsy_corr) else NA
            
            # Populate reproducibility table
            repro_table <- self$results$reproducibilitytable
            
            # Row 1 is meaningful only when a reference is supplied. In the
            # inter-regional design mean(correlations) is the same mean pairwise
            # inter-region correlation already reported as row 3, so skip it here
            # to avoid a mislabelled ("Regional-Reference") duplicate.
            if (has_reference) {
                repro_table$addRow(rowKey = 1, values = list(
                    metric = "Mean Regional-Reference Correlation",
                    value = mean(correlations, na.rm = TRUE),
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = ifelse(mean(correlations, na.rm = TRUE) >= correlation_threshold,
                                           "Good representativeness", "Limited representativeness")
                ))
            }

            if (!is.na(icc_value)) {
                # Label the row for what was ACTUALLY computed. Five fallback
                # paths in .calculateICC return the mean Spearman correlation
                # rather than an ICC; that value used to be printed under the
                # "ICC(3,1)" heading and graded on ICC reliability cut-offs, so a
                # mean rank correlation was presented to the pathologist as a
                # reliability coefficient.
                is_icc <- identical(icc_result$method, "icc")
                repro_table$addRow(rowKey = 2, values = list(
                    metric = if (is_icc) "ICC(2,1) - absolute agreement"
                             else "Mean correlation (ICC not estimable)",
                    value = icc_value,
                    ci_lower = icc_lower,
                    ci_upper = icc_upper,
                    interpretation = if (!is_icc) {
                        "Not an ICC - see note"
                    } else ifelse(icc_value >= 0.75, "Good reliability",
                                 ifelse(icc_value >= 0.50, "Moderate reliability", "Poor reliability"))
                ))
                if (!is_icc) {
                    repro_table$setNote("icc_fallback", .("The intraclass correlation could not be estimated (too few complete cases or measurements, zero variance, or the 'psych' package is unavailable). The value shown is the mean Spearman correlation, which is NOT an ICC: it is blind to systematic differences between measurements. Do not interpret it as a reliability coefficient."))
                }
            }

            # Report the consistency form alongside absolute agreement. They
            # differ exactly when there is a systematic offset between the region
            # and the reference, which is the finding that matters clinically.
            if (!is.null(private$.icc_consistency) &&
                !is.na(private$.icc_consistency$value)) {
                repro_table$addRow(rowKey = 21, values = list(
                    metric = "ICC(3,1) - consistency (bias-blind)",
                    value = private$.icc_consistency$value,
                    ci_lower = private$.icc_consistency$lower,
                    ci_upper = private$.icc_consistency$upper,
                    interpretation = .("Ignores systematic offset; compare with absolute agreement above")
                ))
            }
            
            if (!is.na(mean_inter_biopsy)) {
                repro_table$addRow(rowKey = 3, values = list(
                    metric = "Mean Inter-Regional Correlation",
                    value = mean_inter_biopsy,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = ifelse(mean_inter_biopsy >= 0.80, "Highly reproducible sampling", 
                                           ifelse(mean_inter_biopsy >= 0.60, "Moderately reproducible", "Variable sampling"))
                ))
            }
            
            # Coefficient of variation (robust calculation) - shared helper so the
            # table and the narrative can never report different CVs.
            cv_values <- private$.perCaseCV(whole_section, biopsy_data, has_reference)
            mean_cv <- mean(cv_values, na.rm = TRUE)
            if (is.nan(mean_cv) || is.infinite(mean_cv)) mean_cv <- NA
            
            repro_table$addRow(rowKey = 4, values = list(
                metric = if (has_reference) "Mean Coefficient of Variation (%) - region vs reference"
                         else "Mean Coefficient of Variation (%) - between regions",
                value = mean_cv,
                ci_lower = NA,
                ci_upper = NA,
                interpretation = ifelse(mean_cv <= cv_threshold/2, "Low variability", 
                                       ifelse(mean_cv <= cv_threshold, "Moderate variability", "High variability"))
            ))
            # icc_method lets downstream prose say "ICC" only when an ICC was
            # actually estimated; mean_cv is exported so nothing recomputes it.
            return(list(icc_value = icc_value, correlations = correlations,
                        mean_inter_biopsy = mean_inter_biopsy,
                        icc_method = icc_result$method, mean_cv = mean_cv))
        },
        
        .analyzeSamplingBias = function(whole_section, biopsy_data) {
            n_biopsies <- ncol(biopsy_data)
            bias_table <- self$results$samplingbiastable
            row_key <- 1
            has_reference <- !is.null(whole_section)

            if (has_reference) {
                # Compare each biopsy to whole section
                for (i in 1:n_biopsies) {
                    biopsy_vals <- biopsy_data[, i]
                    complete_pairs <- complete.cases(whole_section, biopsy_vals)
                    
                    if (sum(complete_pairs) >= 3) {
                        ws_complete <- whole_section[complete_pairs]
                        biopsy_complete <- biopsy_vals[complete_pairs]
                        
                        # Paired t-test for systematic bias
                        bias_test <- private$.safePairedT(biopsy_complete, ws_complete)
                        mean_diff <- bias_test$estimate
                        p_value <- bias_test$p.value
                        
                        # Effect size (Hedges' g for paired data with small-sample correction)
                        diff_vals <- biopsy_complete - ws_complete
                        n_pairs <- length(diff_vals)

                        cohens_d_raw <- mean(diff_vals) / sd(diff_vals)

                        # Apply Hedges' correction for small samples (n < 50)
                        if (n_pairs < 50) {
                            correction_factor <- 1 - (3 / (4 * n_pairs - 5))
                            cohens_d <- cohens_d_raw * correction_factor
                        } else {
                            cohens_d <- cohens_d_raw
                        }
                        
                        # Clinical impact assessment (guard near-zero/negative reference mean)
                        ref_mean <- mean(ws_complete)
                        if (is.na(ref_mean) || abs(ref_mean) < 1e-6) {
                            relative_bias <- NA_real_
                            clinical_impact <- "Not assessable (reference mean near zero)"
                        } else {
                            relative_bias <- abs(mean_diff) / abs(ref_mean) * 100
                            clinical_impact <- ifelse(relative_bias <= 5, "Minimal (<5%)",
                                                    ifelse(relative_bias <= 15, "Moderate (5-15%)", "Large (>15%)"))
                        }
                        
                        bias_table$addRow(rowKey = row_key, values = list(
                            comparison = paste("Region", i, "vs Reference"),
                            mean_diff = mean_diff,
                            p_value = p_value,
                            effect_size = cohens_d,
                            clinical_impact = clinical_impact
                        ))
                        
                        row_key <- row_key + 1
                    }
                }
                
                # Overall biopsy mean vs whole section
                if (n_biopsies >= 2) {
                    biopsy_means <- rowMeans(biopsy_data, na.rm = TRUE)
                    complete_pairs <- complete.cases(whole_section, biopsy_means)
                    
                    if (sum(complete_pairs) >= 3) {
                        overall_test <- private$.safePairedT(biopsy_means[complete_pairs], whole_section[complete_pairs])
                        overall_diff <- overall_test$estimate
                        overall_p <- overall_test$p.value
                        
                        # Effect size (Hedges' g with small-sample correction)
                        diff_vals <- biopsy_means[complete_pairs] - whole_section[complete_pairs]
                        n_pairs <- length(diff_vals)

                        overall_d_raw <- mean(diff_vals) / sd(diff_vals)

                        # Apply Hedges' correction for small samples
                        if (n_pairs < 50) {
                            correction_factor <- 1 - (3 / (4 * n_pairs - 5))
                            overall_d <- overall_d_raw * correction_factor
                        } else {
                            overall_d <- overall_d_raw
                        }
                        
                        # Guard near-zero/negative reference mean before relative bias
                        ref_mean <- mean(whole_section[complete_pairs])
                        if (is.na(ref_mean) || abs(ref_mean) < 1e-6) {
                            relative_bias <- NA_real_
                            clinical_impact <- "Not assessable (reference mean near zero)"
                        } else {
                            relative_bias <- abs(overall_diff) / abs(ref_mean) * 100
                            clinical_impact <- ifelse(relative_bias <= 5, "Minimal (<5%)",
                                                    ifelse(relative_bias <= 15, "Moderate (5-15%)", "Large (>15%)"))
                        }
                        
                        bias_table$addRow(rowKey = row_key, values = list(
                            comparison = "Mean of Regions vs Reference",
                            mean_diff = overall_diff,
                            p_value = overall_p,
                            effect_size = overall_d,
                            clinical_impact = clinical_impact
                        ))
                    }
                }
                # Up to five paired t-tests are reported here (one per region plus
                # the pooled comparison) with unadjusted p-values. Say so, rather
                # than letting a reader treat each p < 0.05 as independent evidence.
                if (row_key > 2) {
                    bias_table$setNote("multiplicity", sprintf(
                        .("%d paired comparisons are reported; p-values are unadjusted. With several regions the chance of at least one p < 0.05 under no true bias exceeds 5%% - interpret individual p-values accordingly, and prefer the mean difference and its clinical impact over statistical significance alone."),
                        row_key - 1))
                }
            } else {
                # If no reference section, bias analysis is not applicable in this context
                bias_table$addRow(rowKey = 1, values = list(
                    comparison = "Bias Analysis",
                    mean_diff = NA,
                    p_value = NA,
                    effect_size = NA,
                    clinical_impact = "Bias analysis requires a reference (whole section) measurement."
                ))
            }
        },
        
        .analyzeVarianceComponents = function(whole_section, biopsy_data) {
            # Variance components from a TWO-WAY random-effects decomposition.
            #
            #     value_ij = mu + case_i + method_j + e_ij
            #
            # The previous implementation reported three quantities that were not
            # components of a common total: "between-case" was var(whole_section),
            # "within-case" the mean of per-row variances, "method" the variance of
            # column means - each divided by the variance of ALL values POOLED.
            # Those are not orthogonal, so they did not sum to the total: on a
            # 20-case example the between-case row read 102.3% of total and the
            # three percentages summed to 107.5%, under a row explicitly labelled
            # "Sum of all variance components". A variance component larger than
            # the total variance is not interpretable.
            #
            # Expected mean squares for the balanced two-way random model give
            #     sigma^2_case   = (MS_case   - MS_error) / k
            #     sigma^2_method = (MS_method - MS_error) / n
            #     sigma^2_error  =  MS_error
            # which do sum to the total variance, so the percentages sum to 100.
            has_reference <- !is.null(whole_section)
            variance_table <- self$results$variancetable

            # Wide matrix of measurements: reference (if any) + each region.
            meas <- as.matrix(biopsy_data)
            method_names <- colnames(biopsy_data)
            if (is.null(method_names)) method_names <- paste0("Region", seq_len(ncol(meas)))
            if (has_reference) {
                meas <- cbind(as.numeric(whole_section), meas)
                method_names <- c("Reference", method_names)
            }
            colnames(meas) <- method_names

            # The mean-square formulas above assume a balanced design, so use only
            # cases measured by every method, and say how many were used.
            keep <- stats::complete.cases(meas)
            meas <- meas[keep, , drop = FALSE]
            n <- nrow(meas)   # cases
            k <- ncol(meas)   # methods

            insufficient <- function(msg) {
                variance_table$setNote("vc", msg)
                for (r in 1:4) {
                    variance_table$setRow(rowKey = r, values = list(
                        variance = NA_real_, percentage = NA_real_,
                        contribution = .("Not estimable")
                    ))
                }
            }

            if (n < 2 || k < 2) {
                insufficient(.("Variance components require at least 2 complete cases measured by at least 2 methods."))
                return()
            }

            long <- data.frame(
                case   = factor(rep(seq_len(n), times = k)),
                method = factor(rep(method_names, each = n), levels = method_names),
                value  = as.numeric(meas)
            )

            fit <- tryCatch(stats::aov(value ~ case + method, data = long),
                            error = function(e) NULL)
            if (is.null(fit)) {
                insufficient(.("Variance component model could not be fitted to these data."))
                return()
            }

            tab <- summary(fit)[[1]]
            ms  <- tab[["Mean Sq"]]
            rn  <- trimws(rownames(tab))
            ms_case   <- ms[match("case",      rn)]
            ms_method <- ms[match("method",    rn)]
            ms_error  <- ms[match("Residuals", rn)]

            if (any(is.na(c(ms_case, ms_method, ms_error)))) {
                insufficient(.("Variance component model could not be fitted to these data."))
                return()
            }

            var_case   <- (ms_case   - ms_error) / k
            var_method <- (ms_method - ms_error) / n
            var_error  <- ms_error

            # A negative estimate means the model attributes no variance to that
            # source; the conventional fix is to truncate at zero and disclose it.
            truncated <- c("case", "method")[c(var_case < 0, var_method < 0)]
            var_case   <- max(var_case, 0)
            var_method <- max(var_method, 0)

            total_variance <- var_case + var_method + var_error

            notes <- sprintf(
                .("Two-way random-effects decomposition (value = case + method + error) on %d cases measured by %d methods; components sum to the total variance."),
                n, k)
            if (sum(!keep) > 0)
                notes <- paste0(notes, " ", sprintf(
                    .("%d case(s) with an incomplete set of measurements were excluded."), sum(!keep)))
            if (length(truncated) > 0)
                notes <- paste0(notes, " ", sprintf(
                    .("The %s variance estimate was negative and has been truncated to zero."),
                    paste(truncated, collapse = " and ")))
            variance_table$setNote("vc", notes)

            pct <- function(x) if (total_variance > 0) x / total_variance * 100 else NA_real_
            case_pct   <- pct(var_case)
            error_pct  <- pct(var_error)
            method_pct <- pct(var_method)

            variance_table$setRow(rowKey = 1L, values = list(
                component = if (has_reference) "Between-Case Variance" else "Between-Case Variance (Regional Means)",
                variance = var_case,
                percentage = case_pct,
                contribution = ifelse(!is.na(case_pct) && case_pct >= 60, "Major contributor",
                                     ifelse(!is.na(case_pct) && case_pct >= 30, "Moderate contributor", "Minor contributor"))
            ))

            variance_table$setRow(rowKey = 2L, values = list(
                component = "Within-Case Variance (Sampling)",
                variance = var_error,
                percentage = error_pct,
                contribution = ifelse(!is.na(error_pct) && error_pct >= 30, "High sampling variability",
                                     ifelse(!is.na(error_pct) && error_pct >= 15, "Moderate sampling variability", "Low sampling variability"))
            ))

            variance_table$setRow(rowKey = 3L, values = list(
                component = if (has_reference) "Method Variance" else "Regional Method Variance",
                variance = var_method,
                percentage = method_pct,
                contribution = ifelse(!is.na(method_pct) && method_pct >= 20, "Significant method differences",
                                     ifelse(!is.na(method_pct) && method_pct >= 10, "Minor method differences", "Negligible method differences"))
            ))

            variance_table$setRow(rowKey = 4L, values = list(
                component = "Total Variance",
                variance = total_variance,
                percentage = if (!is.na(total_variance)) 100 else NA,
                contribution = "Sum of all variance components"
            ))
        },
        
        .performPowerAnalysis = function(whole_section, biopsy_data) {
            if (is.null(whole_section))
                return()

            # Calculate observed effect sizes
            biopsy_means <- rowMeans(biopsy_data, na.rm = TRUE)
            complete_pairs <- complete.cases(whole_section, biopsy_means)

            # n for the power calculation is the number of PAIRS the correlation
            # is actually estimated from. This used to be length(whole_section),
            # which still counts rows whose reference measurement is missing, so
            # se_z = 1/sqrt(n-3) was computed from too large an n and every
            # reported power was inflated (and required_n understated).
            n_cases <- sum(complete_pairs)

            if (n_cases >= 3) {
                # Observed correlation effect size
                obs_correlation <- cor(whole_section[complete_pairs], biopsy_means[complete_pairs], method = "spearman")
                
                # Convert correlation to effect size (Cohen's convention)
                # Small: r = 0.1, Medium: r = 0.3, Large: r = 0.5
                correlation_categories <- c(0.1, 0.3, 0.5, obs_correlation)
                
                power_table <- self$results$poweranalysistable
                row_key <- 1
                
                for (idx in seq_along(correlation_categories)) {
                    effect_size <- correlation_categories[idx]
                    # Skip if effect size is zero, near-zero, or undefined. cor()
                    # returns NA when either measurement has zero variance (e.g. a
                    # marker scored identically in every case), which would other-
                    # wise abort the whole analysis at this if().
                    if (is.na(effect_size) || abs(effect_size) < 0.01) {
                        next
                    }

                    # Power for a correlation via Fisher's z transformation.
                    z_observed <- 0.5 * log((1 + effect_size) / (1 - effect_size))

                    # Standard error under H0. Fisher's z with se = 1/sqrt(n-3) is
                    # the PEARSON result; the observed effect above is Spearman,
                    # whose z-variance is inflated by about 1.06 (Fieller, Hartley
                    # & Pearson 1957). Using the Pearson SE for a Spearman r
                    # overstates power, so apply the correction to that row.
                    se_z <- if (idx == 4) sqrt(1.06 / (n_cases - 3)) else 1 / sqrt(n_cases - 3)

                    # Non-centrality parameter
                    ncp <- z_observed / se_z

                    # Two-tailed test power (alpha = 0.05)
                    z_alpha_half <- qnorm(0.975)  # Critical value for alpha/2 = 0.025

                    # Power = P(|Z| > z_alpha/2 | H1 is true) where Z ~ N(ncp, 1) under H1
                    power <- pnorm(ncp - z_alpha_half) + pnorm(-ncp - z_alpha_half)

                    # Required sample size for 80% power
                    z_beta <- qnorm(0.80)
                    z_alpha <- qnorm(0.975)

                    if (abs(z_observed) > 0.01) {
                        required_n <- ceiling(((z_alpha + z_beta) / z_observed)^2 + 3)
                    } else {
                        required_n <- NA  # Effect size too small
                    }

                    # Ensure minimum of 5 cases
                    required_n <- pmax(required_n, 5, na.rm = TRUE)

                    # Identify the row by index (correlation_categories =
                    # c(0.1, 0.3, 0.5, obs_correlation)) so a coincidental match
                    # of the observed r to a fixed effect size cannot mis-tag it.
                    scenario <- if (idx == 4) {
                        "Observed Effect Size"
                    } else if (idx == 1) {
                        "Small Effect (r=0.1)"
                    } else if (idx == 2) {
                        "Medium Effect (r=0.3)"
                    } else {
                        "Large Effect (r=0.5)"
                    }

                    # The observed-effect row is post-hoc power: a deterministic
                    # function of the observed correlation and n, so "adequate
                    # power" for the effect you just observed is circular
                    # (Hoenig & Heisey 2001). Report it, but do not let it certify
                    # the study - only the pre-specified effect sizes get a
                    # power verdict.
                    recommendation <- if (idx == 4) {
                        "Post-hoc (observed) power - not evidence of adequacy"
                    } else if (power >= 0.80) {
                        "Adequate power achieved"
                    } else if (required_n <= n_cases * 1.5) {
                        "Consider moderate sample increase"
                    } else {
                        "Substantial sample increase recommended"
                    }

                    power_table$addRow(rowKey = row_key, values = list(
                        scenario = scenario,
                        effect_size = effect_size,
                        power = power,
                        required_n = required_n,
                        recommendation = recommendation
                    ))

                    row_key <- row_key + 1
                }
            }
        },
        
        .analyzeSpatialHeterogeneity = function(whole_section, biopsy_data, spatial_regions) {
            # Analyze variability across spatial regions
            unique_regions <- unique(spatial_regions)
            unique_regions <- unique_regions[!is.na(unique_regions)]
            
            if (length(unique_regions) >= 2) {
                spatial_table <- self$results$spatialanalysistable
                has_reference <- !is.null(whole_section)
                
                for (i in seq_along(unique_regions)) {
                    region <- unique_regions[i]
                    region_mask <- spatial_regions == region & !is.na(spatial_regions)
                    
                    if (sum(region_mask) >= 2) {
                        region_whole_section <- if (has_reference) whole_section[region_mask] else numeric(0)
                        region_biopsy_data <- biopsy_data[region_mask, , drop = FALSE]
                        
                        # Calculate regional statistics.
                        #
                        # The CV must be computed PER CASE and then averaged, the
                        # same way the reproducibility and compartment tables do it.
                        # Pooling every measurement of every case into one vector
                        # and taking a single CV measures BETWEEN-PATIENT
                        # biological spread, not spatial heterogeneity within a
                        # case - so a compartment whose cases were internally
                        # consistent but spanned a wide range of expression was
                        # ranked "High heterogeneity", inverting the true ranking
                        # under a column headed "Heterogeneity Level".
                        region_values <- c(region_whole_section, as.matrix(region_biopsy_data))
                        region_mean <- mean(region_values, na.rm = TRUE)
                        region_case_cvs <- private$.perCaseCV(
                            if (has_reference) region_whole_section else NULL,
                            region_biopsy_data, has_reference)
                        region_cv <- if (length(region_case_cvs) > 0)
                            mean(region_case_cvs, na.rm = TRUE) else NA_real_
                        if (is.nan(region_cv)) region_cv <- NA_real_

                        # Categorize heterogeneity level
                        heterogeneity_level <- ifelse(region_cv <= 15, "Low",
                                                     ifelse(region_cv <= 30, "Moderate", "High"))
                        
                        spatial_table$addRow(rowKey = i, values = list(
                            region = as.character(region),
                            n_cases = sum(region_mask),
                            mean_value = region_mean,
                            cv_percent = region_cv,
                            heterogeneity_level = heterogeneity_level
                        ))
                    }
                }
            }
        },
        
        .generateHeterogeneityPlots = function(whole_section, biopsy_data, spatial_regions = NULL, study_design = "reference_based") {
            # Prepare comprehensive plot data
            plot_data <- list(
                whole_section = whole_section,
                biopsy_data = biopsy_data,
                spatial_regions = spatial_regions,
                n_cases = if (!is.null(whole_section)) length(whole_section) else nrow(biopsy_data),
                n_biopsies = ncol(biopsy_data)
            )
            
            self$results$biopsyplot$setState(plot_data)
            self$results$variabilityplot$setState(plot_data)
            
            if (!is.null(spatial_regions)) {
                self$results$spatialplot$setState(plot_data)
            }
        },
        
        .biopsyplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            if (!requireNamespace('ggplot2', quietly = TRUE)) {
                warning("ggplot2 package required for biopsy comparison plot")
                return(FALSE)
            }

            data <- image$state
            
            # Create comparison plot (adaptive for dual study design)
            # Prepare data for plotting

            # Determine methods and values based on study design
            has_reference <- !is.null(data$whole_section) && length(data$whole_section) > 0

            if (has_reference) {
                # Reference-based study
                methods <- c("Reference", paste("Region", 1:data$n_biopsies))
                values <- c(data$whole_section, as.vector(as.matrix(data$biopsy_data)))
                n_methods <- data$n_biopsies + 1
            } else {
                # Inter-regional study
                methods <- paste("Region", 1:data$n_biopsies)
                values <- as.vector(as.matrix(data$biopsy_data))
                n_methods <- data$n_biopsies
            }

            plot_df <- data.frame(
                Case = rep(1:data$n_cases, times = n_methods),
                Method = rep(methods, each = data$n_cases),
                Value = values
            )
            
            # Remove missing values
            plot_df <- plot_df[!is.na(plot_df$Value), ]
            
            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Method, y = Value, color = Method)) +
                ggplot2::geom_boxplot(alpha = 0.7) +
                ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggplot2::labs(
                    title = if (has_reference) "Regional vs Reference Measurements" else "Inter-Regional Measurements",
                    subtitle = "Distribution of IHC biomarker values across tissue regions",
                    x = "Measurement Location",
                    y = "Biomarker Value",
                    color = "Method"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .variabilityplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)

            if (!requireNamespace('ggplot2', quietly = TRUE)) {
                warning("ggplot2 package required for variability plot")
                return(FALSE)
            }

            data <- image$state
            
            # Create variability assessment plot
            # Calculate CV for each case (vectorized approach)
            calculate_case_cv_with_id <- function(i) {
                case_values <- c(data$whole_section[i], as.numeric(data$biopsy_data[i, ]))
                case_values <- case_values[!is.na(case_values)]

                if (length(case_values) >= 2) {
                    list(case_id = i, cv = private$.calculateRobustCV(case_values))
                } else {
                    NULL
                }
            }

            cv_results <- lapply(1:data$n_cases, calculate_case_cv_with_id)
            cv_results <- cv_results[!sapply(cv_results, is.null)]

            case_ids <- sapply(cv_results, `[[`, "case_id")
            cv_values <- sapply(cv_results, `[[`, "cv")
            
            variability_df <- data.frame(
                Case = case_ids,
                CV_Percent = cv_values
            )
            
            p <- ggplot2::ggplot(variability_df, ggplot2::aes(x = Case, y = CV_Percent)) +
                ggplot2::geom_point(color = "steelblue", alpha = 0.7) +
                ggplot2::geom_smooth(method = "loess", se = TRUE, color = "red", alpha = 0.3) +
                ggplot2::geom_hline(yintercept = c(15, 30), linetype = "dashed", alpha = 0.7) +
                ggplot2::annotate("text", x = max(case_ids) * 0.8, y = 15, 
                                 label = "15% CV threshold", vjust = -0.5) +
                ggplot2::annotate("text", x = max(case_ids) * 0.8, y = 30, 
                                 label = "30% CV threshold", vjust = -0.5) +
                ggplot2::labs(
                    title = "Sampling Variability Analysis",
                    subtitle = "Coefficient of variation across sampling methods per case",
                    x = "Case Number",
                    y = "Coefficient of Variation (%)"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .spatialplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state) || is.null(image$state$spatial_regions))
                return(FALSE)

            if (!requireNamespace('ggplot2', quietly = TRUE)) {
                warning("ggplot2 package required for spatial plot")
                return(FALSE)
            }

            data <- image$state
            
            # Create spatial heterogeneity visualization
            spatial_regions <- data$spatial_regions
            unique_regions <- unique(spatial_regions)
            unique_regions <- unique_regions[!is.na(unique_regions)]
            
            if (length(unique_regions) >= 2) {
                # Calculate regional means and CVs
                region_stats <- data.frame(
                    Region = character(),
                    Mean_WS = numeric(),
                    CV = numeric(),
                    stringsAsFactors = FALSE
                )
                
                for (region in unique_regions) {
                    region_mask <- spatial_regions == region & !is.na(spatial_regions)
                    if (sum(region_mask) >= 2) {
                        region_ws <- data$whole_section[region_mask]
                        region_biopsy <- data$biopsy_data[region_mask, , drop = FALSE]
                        
                        all_regional_values <- c(region_ws, as.matrix(region_biopsy))
                        region_mean <- mean(all_regional_values, na.rm = TRUE)
                        region_cv <- private$.calculateRobustCV(all_regional_values)
                        
                        region_stats <- rbind(region_stats, data.frame(
                            Region = as.character(region),
                            Mean_WS = region_mean,
                            CV = region_cv,
                            stringsAsFactors = FALSE
                        ))
                    }
                }
                
                if (nrow(region_stats) > 0) {
                    # Create categorical CV levels for better visualization using clinical constants
                    region_stats$CV_Level <- cut(region_stats$CV,
                                               breaks = c(0, private$.CLINICAL_CONSTANTS$CV_LOW_THRESHOLD,
                                                         private$.CLINICAL_CONSTANTS$CV_MODERATE_THRESHOLD, Inf),
                                               labels = c(paste0("Low (<", private$.CLINICAL_CONSTANTS$CV_LOW_THRESHOLD, "%)"),
                                                         paste0("Moderate (", private$.CLINICAL_CONSTANTS$CV_LOW_THRESHOLD, "-",
                                                               private$.CLINICAL_CONSTANTS$CV_MODERATE_THRESHOLD, "%)"),
                                                         paste0("High (>", private$.CLINICAL_CONSTANTS$CV_MODERATE_THRESHOLD, "%)")),
                                               include.lowest = TRUE)

                    p <- ggplot2::ggplot(region_stats, ggplot2::aes(x = Region, y = Mean_WS)) +
                        ggplot2::geom_col(ggplot2::aes(fill = CV_Level), alpha = 0.7) +
                        ggplot2::geom_text(ggplot2::aes(label = paste("CV:", round(CV, 1), "%")),
                                          vjust = -0.5, size = 3) +
                        ggplot2::scale_fill_manual(values = setNames(c("green", "yellow", "red"),
                                                                    c(paste0("Low (<", private$.CLINICAL_CONSTANTS$CV_LOW_THRESHOLD, "%)"),
                                                                      paste0("Moderate (", private$.CLINICAL_CONSTANTS$CV_LOW_THRESHOLD, "-",
                                                                            private$.CLINICAL_CONSTANTS$CV_MODERATE_THRESHOLD, "%)"),
                                                                      paste0("High (>", private$.CLINICAL_CONSTANTS$CV_MODERATE_THRESHOLD, "%)"))),
                                                 name = "Variability Level") +
                        ggplot2::labs(
                            title = "Spatial Heterogeneity Analysis",
                            subtitle = "Mean biomarker values and variability by spatial region",
                            x = "Spatial Region",
                            y = "Mean Biomarker Value"
                        ) +
                        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                        ggtheme
                    
                    print(p)
                    return(TRUE)
                }
            }
            
            return(FALSE)
        },
        
        .calculateInterpretationMetrics = function(whole_section, biopsy_data, repro_stats = NULL, study_design = "reference_based") {
            has_reference <- !is.null(whole_section) && study_design == "reference_based"

            n_cases <- if (has_reference) length(whole_section) else nrow(biopsy_data)
            n_biopsies <- ncol(biopsy_data)

            biopsy_means <- if (nrow(biopsy_data) > 0) rowMeans(biopsy_data, na.rm = TRUE) else numeric(0)

            if (has_reference && length(biopsy_means) > 0) {
                complete_pairs <- complete.cases(whole_section, biopsy_means)
                if (sum(complete_pairs) >= private$.CLINICAL_CONSTANTS$MIN_CASES_ICC) {
                    # Mean of the per-region correlations, matching the
                    # "Mean Regional-Reference Correlation" table row. Correlating
                    # the reference against the AVERAGE of the regions averages
                    # away sampling noise, so it ran systematically higher than the
                    # table (0.952 in prose vs a lower table value) - two different
                    # numbers described to the reader as the same thing.
                    overall_corr <- if (!is.null(repro_stats$correlations) &&
                                        any(!is.na(repro_stats$correlations))) {
                        mean(repro_stats$correlations, na.rm = TRUE)
                    } else {
                        cor(whole_section[complete_pairs], biopsy_means[complete_pairs], method = "spearman")
                    }
                    bias_test <- private$.safePairedT(biopsy_means[complete_pairs], whole_section[complete_pairs])
                    mean_bias <- bias_test$estimate
                    bias_p <- bias_test$p.value
                    ref_mean <- mean(whole_section[complete_pairs], na.rm = TRUE)
                } else {
                    overall_corr <- mean_bias <- bias_p <- ref_mean <- NA
                }
            } else {
                overall_corr <- if (!is.null(repro_stats$mean_inter_biopsy)) repro_stats$mean_inter_biopsy else NA
                mean_bias <- NA
                bias_p <- NA
                ref_mean <- NA
            }

            # Same helper the reproducibility table uses.
            cv_values <- private$.perCaseCV(whole_section, biopsy_data, has_reference)

            mean_cv <- if (length(cv_values) > 0) mean(as.numeric(cv_values), na.rm = TRUE) else NA
            if (is.nan(mean_cv) || is.infinite(mean_cv)) mean_cv <- NA

            icc_value <- if (!is.null(repro_stats$icc_value)) repro_stats$icc_value else NA
            correlations <- if (!is.null(repro_stats$correlations)) repro_stats$correlations else NA
            mean_inter_biopsy <- if (!is.null(repro_stats$mean_inter_biopsy)) repro_stats$mean_inter_biopsy else NA

            return(list(
                n_cases = n_cases,
                n_biopsies = n_biopsies,
                overall_corr = overall_corr,
                mean_bias = mean_bias,
                bias_p = bias_p,
                ref_mean = ref_mean,
                mean_cv = mean_cv,
                icc = icc_value,
                correlations = correlations,
                mean_inter_biopsy = mean_inter_biopsy,
                has_reference = has_reference
            ))
        },

        .formatClinicalAssessment = function(metrics, cv_threshold, correlation_threshold) {
            comparison_target <- if (metrics$has_reference) "whole section" else "other regions"

            correlation_item <- if (!is.na(metrics$overall_corr)) {
                paste0(
                    "<li><strong>Representativeness:</strong> Spearman correlation = ", round(metrics$overall_corr, 3),
                    " (", ifelse(metrics$overall_corr >= private$.CLINICAL_CONSTANTS$CORRELATION_GOOD, "Good",
                                 ifelse(metrics$overall_corr >= private$.CLINICAL_CONSTANTS$CORRELATION_POOR, "Moderate", "Poor")),
                    " agreement with ", comparison_target, ")</li>"
                )
            } else {
                "<li><strong>Representativeness:</strong> Correlation could not be estimated with the available data.</li>"
            }

            variability_item <- if (!is.na(metrics$mean_cv)) {
                paste0(
                    "<li><strong>Sampling Variability:</strong> Mean CV = ", round(metrics$mean_cv, 1), "% ",
                    "(", ifelse(metrics$mean_cv <= private$.CLINICAL_CONSTANTS$CV_LOW_THRESHOLD, "Low variability",
                         ifelse(metrics$mean_cv <= private$.CLINICAL_CONSTANTS$CV_MODERATE_THRESHOLD, "Moderate variability", "High variability")), ")</li>"
                )
            } else {
                "<li><strong>Sampling Variability:</strong> Not available.</li>"
            }

            bias_item <- if (!is.na(metrics$mean_bias) && !is.na(metrics$bias_p)) {
                paste0("<li><strong>Sampling Bias:</strong> Mean difference = ", round(metrics$mean_bias, 3),
                       " (", ifelse(metrics$bias_p < 0.05, "Statistically significant", "Not significant"), ")</li>")
            } else if (metrics$has_reference) {
                "<li><strong>Sampling Bias:</strong> Not enough paired observations to test for systematic bias.</li>"
            } else {
                ""
            }

            # Systematic bias is a THIRD axis and must be able to veto the verdict.
            #
            # Correlation (Spearman) is rank agreement across cases and is blind to
            # any additive or proportional offset; the CV of a case is likewise
            # small when every region is offset the same way. So a marker where
            # every biopsy under-reads the whole section by 30% - a Ki67 of 60%
            # reported as 42% - passed both thresholds and was declared "ADEQUATE
            # SAMPLING ... suitable for clinical use" while the module's own bias
            # table reported p < 1e-13 and "Large (>15%)" impact on the same screen.
            bias_is_material <- !is.na(metrics$bias_p) && metrics$bias_p < 0.05 &&
                !is.na(metrics$mean_bias) && !is.na(metrics$mean_cv) &&
                is.finite(private$.relativeBias(metrics)) &&
                private$.relativeBias(metrics) > private$.CLINICAL_CONSTANTS$RELATIVE_BIAS_MATERIAL

            bias_veto <- if (bias_is_material) {
                paste0("<p><strong> SYSTEMATIC BIAS:</strong> Regional measurements differ from the ",
                       comparison_target, " by ", round(metrics$mean_bias, 2),
                       " on average (", round(private$.relativeBias(metrics), 1), "% of the reference mean, p ",
                       ifelse(metrics$bias_p < 0.001, "< 0.001", paste0("= ", round(metrics$bias_p, 3))), "). ",
                       "<span style='color: red;'>Correlation and CV cannot detect a constant offset, so the ",
                       "agreement thresholds below do not by themselves establish that regional ",
                       "measurements may substitute for the ", comparison_target, ".</span></p>")
            } else ""

            status_text <- if (is.na(metrics$overall_corr) || is.na(metrics$mean_cv)) {
                "<p><strong> INSUFFICIENT DATA:</strong> Unable to evaluate sampling quality because correlation or variability estimates could not be computed.</p>"
            } else if (bias_is_material) {
                paste0(bias_veto,
                       "<p><strong> NOT ADEQUATE FOR SUBSTITUTION:</strong> Agreement thresholds ",
                       "(correlation \u{2265} ", correlation_threshold, ", CV \u{2264} ", cv_threshold,
                       "%) may be met, but a clinically material systematic difference is present. ",
                       "<span style='color: red;'>Calibrate the regional measurement before using it in place of the ",
                       comparison_target, ".</span></p>")
            } else if (metrics$overall_corr >= correlation_threshold && metrics$mean_cv <= cv_threshold) {
                paste0("<p><strong> AGREEMENT THRESHOLDS MET:</strong> Regional measurements agree with the ",
                       comparison_target, " in this dataset (correlation \u{2265} ", correlation_threshold, ", CV \u{2264} ", cv_threshold, "%), ",
                       if (!is.na(metrics$bias_p) && is.finite(private$.relativeBias(metrics)))
                           "and no material systematic bias was detected. "
                       else
                           "Systematic bias could not be assessed in this run. ",
                       "<span style='color: green;'>These are summary statistics from this dataset alone; they are not an external validation and they do not describe agreement at the score thresholds used to classify cases.</span></p>")
            } else if (metrics$overall_corr >= (correlation_threshold - 0.2) && metrics$mean_cv <= (cv_threshold * 1.5)) {
                paste0("<p><strong> MODERATE SAMPLING:</strong> Regional measurements show moderate agreement with ",
                       comparison_target, " (thresholds: correlation \u{2265} ", correlation_threshold, ", CV \u{2264} ", cv_threshold, "%). ",
                       "<span style='color: orange;'>Consider additional samples or sampling optimization.</span></p>")
            } else {
                paste0("<p><strong> INADEQUATE SAMPLING:</strong> Sampling does not meet quality thresholds ",
                       "(correlation \u{2265} ", correlation_threshold, ", CV \u{2264} ", cv_threshold, "%). ",
                       "<span style='color: red;'>Review sampling strategy and consider increased sampling.</span></p>")
            }

            assessment <- paste0(
                "<h4>Key Findings:</h4>",
                "<ul>",
                correlation_item,
                bias_item,
                variability_item,
                "</ul>",

                "<h4>Clinical Assessment:</h4>",
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 10px; border-left: 4px solid #007bff; color: inherit;'>",
                status_text,
                "</div>"
            )
            return(assessment)
        },

        .generateRecommendations = function(metrics, cv_threshold, bias_p) {
            recommendations <- paste0(
                "<h4>Recommendations:</h4>",
                "<ul>",
                "<li><strong>Sample Size:</strong> ",
                if (!is.na(metrics$mean_cv)) {
                    if (metrics$mean_cv <= cv_threshold/2) {
                        paste0("Current sampling appears adequate (CV = ", round(metrics$mean_cv, 1), "% \u{2264} ", cv_threshold/2, "%)")
                    } else if (metrics$mean_cv <= cv_threshold) {
                        paste0("Consider 2-3 additional samples to reduce variability (current CV = ", round(metrics$mean_cv, 1), "%, threshold = ", cv_threshold, "%)")
                    } else {
                        paste0("Significant increase in sampling recommended (current CV = ", round(metrics$mean_cv, 1), "% > ", cv_threshold, "% threshold)")
                    }
                } else {
                    "Insufficient data for sampling recommendation"
                },
                "</li>",

                if (!is.na(bias_p) && bias_p < 0.05) {
                    "<li><strong>Bias Correction:</strong> Systematic bias detected - consider calibration or bias correction</li>"
                } else { "" },

                "<li><strong>Quality Control:</strong> Monitor cases with CV > 30% for adequate sampling</li>",
                "<li><strong>Validation:</strong> Confirm findings in independent dataset</li>",
                "</ul>",

                "<h4>Statistical Interpretation:</h4>",
                "<ul>",
                "<li>Review variance components table to understand sources of variability</li>",
                "<li>High within-case variance suggests sampling heterogeneity</li>",
                "<li>High between-case variance indicates true biological differences</li>",
                "<li>Method variance reflects systematic differences between sampling approaches</li>",
                "</ul>",

                "<p><em>This analysis follows the biopsy simulation methodology from Zilenaite-Petrulaitiene et al. (2025)
                and international guidelines for diagnostic test evaluation in pathology.</em></p>"
            )
            return(recommendations)
        },

        .generateReportSentences = function(metrics, cv_threshold, correlation_threshold) {
            # Generate copy-ready sentences for clinical reports
            comparison_target <- if (metrics$has_reference) "reference measurements" else "other regional measurements"
            correlation_phrase <- if (metrics$has_reference) "with whole-section measurements" else "between regional measurements"

            correlation_sentence <- if (!is.na(metrics$overall_corr)) {
                paste0(
                    "Regional measurements showed ",
                    # Graded against the user's correlation_threshold, not fixed
                    # cut-offs: with a threshold of 0.95 an r of 0.80 was still
                    # called "excellent" here while the verdict below called the
                    # same run inadequate.
                    ifelse(metrics$overall_corr >= correlation_threshold, "excellent",
                           ifelse(metrics$overall_corr >= correlation_threshold - 0.10, "good",
                                  ifelse(metrics$overall_corr >= correlation_threshold - 0.20, "moderate", "poor"))),
                    " correlation ", correlation_phrase, " (r = ", round(metrics$overall_corr, 3), "). "
                )
            } else {
                "Correlation metrics were not estimable with the available data. "
            }

            variability_sentence <- if (!is.na(metrics$mean_cv)) {
                paste0(
                    "Sampling variability was ",
                    # Graded against the user's cv_threshold - see above.
                    ifelse(metrics$mean_cv <= cv_threshold / 2, "low",
                           ifelse(metrics$mean_cv <= cv_threshold, "moderate", "high")),
                    " (mean CV = ", round(metrics$mean_cv, 1), "%). "
                )
            } else {
                "Sampling variability could not be estimated. "
            }

            bias_sentence <- if (metrics$has_reference) {
                if (!is.na(metrics$bias_p)) {
                    if (metrics$bias_p < 0.05) {
                        paste0("Systematic bias was detected between regional and reference measurements (p = ",
                               ifelse(metrics$bias_p < 0.001, "<0.001", round(metrics$bias_p, 3)), "). ")
                    } else {
                        "No systematic bias was detected between regional and reference measurements (p \u{2265} 0.05); this does not establish that the two agree, as the test may lack power to detect a difference of relevant size. "
                    }
                } else {
                    "Bias testing could not be performed due to limited paired observations. "
                }
            } else {
                "Bias analysis was not applicable because no reference measurement was supplied. "
            }

            quality_status <- if (!is.na(metrics$overall_corr) && metrics$overall_corr >= correlation_threshold &&
                                   !is.na(metrics$mean_cv) && metrics$mean_cv <= cv_threshold) {
                "met the predefined quality criteria"
            } else if (!is.na(metrics$overall_corr) && metrics$overall_corr >= (correlation_threshold - 0.2) &&
                       !is.na(metrics$mean_cv) && metrics$mean_cv <= (cv_threshold * 1.5)) {
                "partially met the predefined quality criteria"
            } else if (is.na(metrics$overall_corr) || is.na(metrics$mean_cv)) {
                "unable to be evaluated due to insufficient data"
            } else {
                "did not meet the predefined quality criteria"
            }

            quality_sentence <- switch(
                quality_status,
                "met the predefined quality criteria" = "The biopsy sampling approach met the predefined quality criteria (correlation and CV thresholds set in the analysis options). ",
                "partially met the predefined quality criteria" = "The sampling approach met the correlation and CV criteria only after relaxing them to correlation \u{2265} (threshold - 0.20) and CV \u{2264} (1.5 \u{00D7} threshold). ",
                "did not meet the predefined quality criteria" = "The sampling approach did not meet the predefined quality criteria. ",
                "unable to be evaluated due to insufficient data" = "Data were insufficient to evaluate overall sampling quality against predefined criteria. ",
                ""
            )

            clinical_sentence <- if (!is.na(metrics$mean_cv)) {
                if (metrics$mean_cv <= cv_threshold/2) {
                    "measurement variability was well within the CV threshold set for this analysis"
                } else if (metrics$mean_cv <= cv_threshold) {
                    "measurement variability was within, but close to, the CV threshold set for this analysis"
                } else {
                    "measurement variability exceeded the CV threshold set for this analysis"
                }
            } else {
                "the available data were insufficient to quantify measurement variability"
            }

            report_sentences <- paste0(
                "<h3>Copy-Ready Report Sentences</h3>",
                "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 15px; border: 1px solid #dee2e6; border-radius: 5px; color: inherit;'>",

                "<h4>Methods Section:</h4>",
                "<p style='font-family: monospace; background: white; padding: 10px; border-left: 4px solid #007bff;'>",
                # Describe only what was actually done. This used to assert that
                # the measurements were "simulated core biopsy measurements ...
                # following the methodology of Zilenaite-Petrulaitiene et al."
                # unconditionally - a factual misstatement of the reader's study
                # design, and a citation they may never have followed, dropped
                # straight into a Methods section. Likewise it claimed an ICC was
                # computed even when .calculateICC had fallen back to a mean
                # correlation.
                "IHC heterogeneity analysis was performed on ", metrics$n_cases, " cases with ", metrics$n_biopsies,
                " regional measurements each. ",
                if (identical(private$.repro_stats$icc_method, "icc"))
                    "Agreement was assessed using the intraclass correlation coefficient (ICC(2,1), absolute agreement) and Spearman rank correlation. "
                else
                    "Agreement was assessed using Spearman rank correlation; an intraclass correlation coefficient could not be estimated from these data. ",
                "Systematic difference from the reference measurement was tested with a paired t-test. ",
                "Sampling variability was quantified using the coefficient of variation (CV). ",
                "Quality thresholds were set at correlation \u{2265}", correlation_threshold, " and CV \u{2264}", cv_threshold, "%.",
                "</p>",

                "<h4>Results Section:</h4>",
                "<p style='font-family: monospace; background: white; padding: 10px; border-left: 4px solid #28a745;'>",
                correlation_sentence,
                variability_sentence,
                bias_sentence,
                quality_sentence,
                "</p>",

                "<h4>Clinical Interpretation:</h4>",
                "<p style='font-family: monospace; background: white; padding: 10px; border-left: 4px solid #ffc107;'>",
                # No blanket endorsement. This used to close EVERY report with
                # "The results support the use of biopsy simulation methodology
                # for quality assurance in digital pathology workflows and
                # biomarker assessment protocols" - including reports whose own
                # preceding sentence said the sampling was inadequate and required
                # protocol revision.
                "In this dataset, ", clinical_sentence, ".",
                "</p>",

                "</div>",

                "<p><strong> Usage:</strong> Click and drag to select text, then copy (Ctrl+C/Cmd+C) for use in reports.</p>"
            )

            return(report_sentences)
        },

        .generateHeterogeneityInterpretation = function(whole_section, biopsy_data, study_design = "reference_based") {
            # Get user-defined thresholds
            cv_threshold <- self$options$cv_threshold
            correlation_threshold <- self$options$correlation_threshold

            # Calculate interpretation metrics
            metrics <- private$.calculateInterpretationMetrics(whole_section, biopsy_data, private$.repro_stats, study_design)

            # Generate clinical assessment
            assessment <- private$.formatClinicalAssessment(metrics, cv_threshold, correlation_threshold)

            # Generate recommendations
            recommendations <- if (self$options$generate_recommendations) {
                private$.generateRecommendations(metrics, cv_threshold, metrics$bias_p)
            } else {
                ""
            }

            # Generate report sentences
            report_sentences <- private$.generateReportSentences(metrics, cv_threshold, correlation_threshold)

            # Combine all sections into final interpretation
            interpretation_sections <- c(assessment)
            if (nzchar(recommendations)) {
                interpretation_sections <- c(interpretation_sections, recommendations)
            }

            interpretation <- paste0(
                "<h3>IHC Heterogeneity Analysis Report</h3>",
                "<p><strong>Study Design:</strong> ", metrics$n_cases, " cases analyzed with ", metrics$n_biopsies, " simulated biopsy samples each</p>",
                paste0(interpretation_sections, collapse = "")
            )

            # Generate assumptions and methodology content
            assumptions_content <- private$.generateAssumptionsContent(metrics)

            # Merge the accumulated data-quality warnings (prepended) and the
            # sampling-strategy / analysis-type notes (appended) so neither is
            # clobbered by this setContent(). See .warnings_html / .strategy_notes.
            final_interpretation <- interpretation
            if (!is.null(private$.warnings_html)) {
                final_interpretation <- paste0(private$.warnings_html, final_interpretation)
            }
            if (!is.null(private$.strategy_notes)) {
                strategy_html <- paste0(
                    "<div style='background-color: rgba(138, 155, 172, 0.06); padding: 10px; border-left: 4px solid #6c757d; margin: 10px 0; color: inherit;'>",
                    gsub("\n\n", "<br><br>", trimws(private$.strategy_notes), fixed = TRUE),
                    "</div>"
                )
                final_interpretation <- paste0(final_interpretation, strategy_html)
            }

            self$results$interpretation$setContent(final_interpretation)
            self$results$report_sentences$setContent(report_sentences)
            self$results$assumptions$setContent(assumptions_content)

            # Generate plain-language summary if requested
            private$.generatePlainLanguageSummary(metrics)
        },

        .generateAssumptionsContent = function(metrics) {
            assumptions <- paste0(
                "<h3>Methodology & Assumptions</h3>",

                "<div style='margin: 15px 0;'>",
                "<h4> Analysis Methodology</h4>",
                "<div style='background-color: rgba(33, 152, 239, 0.13); padding: 12px; border-radius: 5px; color: inherit;'>",
                "<ul>",
                "<li><strong>IHC Heterogeneity:</strong> Quantitative comparison of biomarker measurements from regional tissue areas</li>",
                "<li><strong>Statistical Framework:</strong> Reproducibility assessed using Spearman correlation",
                if (identical(private$.repro_stats$icc_method, "icc"))
                    " and the intraclass correlation coefficient (ICC(2,1), absolute agreement)</li>"
                else
                    "; an intraclass correlation coefficient could not be estimated from these data, so the Reproducibility Assessment table reports the mean Spearman correlation in its place</li>",
                "<li><strong>Variability Metrics:</strong> Coefficient of variation (CV) calculated per case and averaged across the dataset</li>",
                "<li><strong>Reference Standard:</strong> Whole-section measurements serve as the gold standard for biomarker quantification</li>",
                "</ul>",
                "</div>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4> Data Requirements & Assumptions</h4>",
                "<div style='background-color: rgba(255, 169, 33, 0.14); padding: 12px; border-radius: 5px; color: inherit;'>",
                "<ul>",
                "<li><strong>Sample Size:</strong> Minimum 5 cases required for statistical analysis (current: ", metrics$n_cases, " cases)</li>",
                "<li><strong>Measurement Scale:</strong> Continuous biomarker values (percentages, scores, or quantitative units)</li>",
                "<li><strong>Regional Independence:</strong> Regional measurements assumed to be spatially independent within each case</li>",
                "<li><strong>Normal Distribution:</strong> Bias testing assumes approximately normal distribution of differences</li>",
                "<li><strong>Linear Relationship:</strong> Correlation analysis assumes monotonic relationship between measurements</li>",
                "</ul>",
                "</div>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4> Important Limitations</h4>",
                "<div style='background-color: rgba(255, 33, 67, 0.09); padding: 12px; border-radius: 5px; color: inherit;'>",
                "<ul>",
                "<li><strong>Simulation vs Reality:</strong> Results based on computational simulation, not actual tissue sampling</li>",
                "<li><strong>Biomarker Specificity:</strong> Findings may not generalize across different biomarkers or tissue types</li>",
                "<li><strong>Technical Variables:</strong> Does not account for pre-analytical factors (fixation time, processing variations)</li>",
                "<li><strong>Observer Variability:</strong> Does not include inter-observer or intra-observer measurement variation</li>",
                "<li><strong>Tumor Heterogeneity:</strong> Assumes biomarker distribution patterns representative of clinical cases</li>",
                "</ul>",
                "</div>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4> Scope of These Estimates</h4>",
                "<div style='background-color: rgba(153, 33, 170, 0.12); padding: 12px; border-radius: 5px; color: inherit;'>",
                "<ul>",
                "<li><strong>Quality Thresholds:</strong> The default criteria applied by this analysis are correlation \u{2265}0.80 and CV \u{2264}20%; both are set in the analysis options</li>",
                "<li><strong>Biomarker-Specific Adjustment:</strong> Thresholds may require adjustment for specific biomarkers</li>",
                "<li><strong>Protocol Validation:</strong> Results should inform but not replace empirical validation studies</li>",
                "<li><strong>Continuous Monitoring:</strong> These estimates describe the cases analyzed here and say nothing about performance over time</li>",
                "<li><strong>Multi-Center Studies:</strong> Additional validation needed for multi-institutional protocols</li>",
                "</ul>",
                "</div>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4> References & Standards</h4>",
                "<div style='background-color: rgba(33, 159, 33, 0.1); padding: 12px; border-radius: 5px; color: inherit;'>",
                "<ul>",
                "<li><strong>Primary Methodology:</strong> Zilenaite-Petrulaitiene et al. (Am J Clin Pathol 2025)</li>",
                "<li><strong>ICC Guidelines:</strong> Koo & Li (J Chiropr Med 2016) - ICC interpretation standards</li>",
                "<li><strong>Biomarker Assessment:</strong> ASCO/CAP guidelines for immunohistochemical analysis</li>",
                "<li><strong>Quality Standards:</strong> Laboratory quality management requirements</li>",
                "</ul>",
                "</div>",
                "</div>"
            )

            return(assumptions)
        },

        .populateGlossary = function() {
            glossary_content <- paste0(
                "<div style='max-width: 800px; margin: 0 auto; font-family: Arial, sans-serif;'>",
                "<h3 style='color: #2c5282; border-bottom: 2px solid #4a90e2; padding-bottom: 8px;'>Statistical Terms Glossary</h3>",

                "<div style='margin: 15px 0; padding: 15px; background-color: rgba(138, 155, 255, 0.06); border-left: 4px solid #4a90e2; border-radius: 4px; color: inherit;'>",
                "<h4 style='color: #2c5282; margin-top: 0;'> Correlation Measures</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>Spearman Correlation:</strong> Measures rank-order relationship between measurements. ",
                "Range: -1 to +1. Not affected by outliers or non-normal distributions. ",
                "Clinical meaning: >0.8 = strong relationship, 0.6-0.8 = moderate, <0.6 = weak.</li>",
                "<li><strong>Pearson Correlation:</strong> Measures linear relationship. Sensitive to outliers and requires normal distribution.</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0; padding: 15px; background-color: rgba(255, 152, 33, 0.07); border-left: 4px solid #ff8c42; border-radius: 4px; color: inherit;'>",
                "<h4 style='color: #b7410e; margin-top: 0;'> Reliability Measures</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>ICC (Intraclass Correlation):</strong> Measures agreement between measurements from same subjects. ",
                "ICC > 0.90 = excellent agreement, 0.75-0.90 = good, 0.50-0.75 = moderate, <0.50 = poor. ",
                "Clinical meaning: How well different measurements agree with each other.</li>",
                "<li><strong>Test-Retest Reliability:</strong> Consistency of measurements over time.</li>",
                "<li><strong>Inter-Rater Reliability:</strong> Agreement between different observers.</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0; padding: 15px; background-color: rgba(33, 255, 92, 0.07); border-left: 4px solid #48bb78; border-radius: 4px; color: inherit;'>",
                "<h4 style='color: #276749; margin-top: 0;'> Variability Measures</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>CV (Coefficient of Variation):</strong> Standardized measure of variability = (SD/Mean) \u{00d7} 100%. ",
                "CV < 10% = low variability (excellent), 10-20% = moderate, 20-30% = high, >30% = very high. ",
                "Clinical meaning: How much measurements vary relative to their average.</li>",
                "<li><strong>Standard Deviation (SD):</strong> Average distance of measurements from the mean.</li>",
                "<li><strong>Variance:</strong> Square of standard deviation. Measures spread of data points.</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0; padding: 15px; background-color: rgba(238, 238, 238, 0.06); border-left: 4px solid #805ad5; border-radius: 4px; color: inherit;'>",
                "<h4 style='color: #553c9a; margin-top: 0;'> IHC-Specific Terms</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>Spatial Heterogeneity:</strong> Variation in biomarker expression across different tissue regions.</li>",
                "<li><strong>H-score:</strong> Immunohistochemical scoring method: (1\u{00d7}%weak) + (2\u{00d7}%moderate) + (3\u{00d7}%strong). Range: 0-300.</li>",
                "<li><strong>Proliferation Index:</strong> Percentage of cells showing positive staining (e.g., Ki67). Range: 0-100%.</li>",
                "<li><strong>Regional Sampling:</strong> Measuring biomarker expression from specific tissue areas.</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0; padding: 15px; background-color: rgba(255, 181, 33, 0.07); border-left: 4px solid #ed8936; border-radius: 4px; color: inherit;'>",
                "<h4 style='color: #9c4221; margin-top: 0;'> Clinical Interpretation Guidelines</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>Excellent Agreement (ICC > 0.90):</strong> Regional measurements highly representative of reference.</li>",
                "<li><strong>Good Agreement (ICC 0.75-0.90):</strong> Regional measurements generally reliable.</li>",
                "<li><strong>Moderate Agreement (ICC 0.50-0.75):</strong> Some variability expected, consider additional regions.</li>",
                "<li><strong>Poor Agreement (ICC < 0.50):</strong> High heterogeneity, single region may not be representative.</li>",
                "</ul>",
                "</div>",

                "</div>"
            )

            self$results$glossary$setContent(glossary_content)
        },

        .generatePlainLanguageSummary = function(metrics) {
            if (!self$options$showSummary) return()

            to_title <- function(x) {
                if (is.na(x) || x == "") return(x)
                if (requireNamespace('stringr', quietly = TRUE)) {
                    return(stringr::str_to_title(x))
                }
                paste0(toupper(substring(x, 1, 1)), substring(x, 2))
            }

            icc_value <- metrics$icc
            # .calculateICC returns the mean Spearman correlation instead of an ICC on
            # five fallback paths (psych unavailable, fewer than 2 measurements, fewer
            # than 3 complete cases, zero variance, psych error) and records that in
            # icc_method. Same guard as the Methods paragraph: never print the word
            # "ICC" next to a number that is not one.
            is_icc <- identical(private$.repro_stats$icc_method, "icc")
            metric_label <- if (is_icc) "ICC" else "mean correlation"
            mean_cv <- metrics$mean_cv
            avg_correlation <- if (!is.null(metrics$correlations) && any(!is.na(metrics$correlations))) {
                mean(metrics$correlations, na.rm = TRUE)
            } else {
                NA_real_
            }

            agreement_level <- if (!is.na(icc_value)) {
                if (icc_value > 0.90) "excellent" else if (icc_value > 0.75) "good" else if (icc_value > 0.50) "moderate" else "poor"
            } else {
                NA_character_
            }

            variability_level <- if (!is.na(mean_cv)) {
                if (mean_cv < 15) "low" else if (mean_cv < 30) "moderate" else "high"
            } else {
                NA_character_
            }

            agreement_sentence <- if (!is.na(agreement_level)) {
                descriptor <- if (icc_value > 0.75) "are highly representative" else if (icc_value > 0.50) "show moderate agreement" else "may not fully represent"
                paste0(
                    "<li><strong>Agreement Level:</strong> ", to_title(agreement_level),
                    " (", metric_label, " = ", sprintf("%.2f", icc_value), ") - Regional measurements ", descriptor,
                    if (metrics$has_reference) " of the reference region." else " of one another.",
                    if (!is_icc) " This number is a mean Spearman correlation, not an ICC, so the ICC reliability bands do not apply to it - see the note under the Reproducibility Assessment table." else "",
                    "</li>"
                )
            } else {
                "<li><strong>Agreement Level:</strong> Not available - agreement could not be estimated with the provided data.</li>"
            }

            variability_sentence <- if (!is.na(variability_level)) {
                descriptor <- if (mean_cv < 15) "consistent measurements across regions" else if (mean_cv < 30) "moderate variation between regions" else "substantial heterogeneity"
                paste0(
                    "<li><strong>Variability:</strong> ", to_title(variability_level), " (CV = ",
                    sprintf("%.1f", mean_cv), "%) - This indicates ", descriptor, ".</li>"
                )
            } else {
                "<li><strong>Variability:</strong> Not available - insufficient data to estimate variability.</li>"
            }

            correlation_sentence <- if (!is.na(avg_correlation)) {
                descriptor <- if (avg_correlation > 0.7) "strong relationships" else if (avg_correlation > 0.5) "moderate relationships" else "weak relationships"
                target <- if (metrics$has_reference) "regional and reference measurements" else "regional measurements"
                paste0(
                    "<li><strong>Correlation:</strong> Average correlation of ", sprintf("%.2f", avg_correlation),
                    " suggests ", descriptor, " between ", target, ".</li>"
                )
            } else {
                "<li><strong>Correlation:</strong> Not available - correlation metrics were not estimable.</li>"
            }

            icc_target <- if (isTRUE(metrics$has_reference)) "between the regions and the reference measurement" else "between regions"
            clinical_sentence <- if (!is.na(icc_value)) {
                # Band labels must match the branch cut-points exactly: the moderate
                # branch is the half-open interval (0.50, 0.75], so "0.50 to 0.75"
                # would claim an endpoint that falls in the branch below.
                band <- if (icc_value > 0.75) {
                    paste0("Agreement ", icc_target, " was high in this dataset (", metric_label, " above 0.75): the measurements gave similar values.")
                } else if (icc_value > 0.50) {
                    paste0("Agreement ", icc_target, " was moderate in this dataset (", metric_label, " above 0.50 and up to 0.75): appreciable variability remained.")
                } else {
                    paste0("Agreement ", icc_target, " was low in this dataset (", metric_label, " of 0.50 or below): the measurements differed substantially.")
                }
                detail <- if (is_icc) {
                    paste0(" The ICC reported here is the absolute-agreement form: it is the share of the total variation in scores that comes from genuine differences between cases rather than from which region was measured, and a consistent offset between regions counts against it. It is not the proportion of cases whose scores matched, and it depends on how spread out your cohort is - the same measurement error yields a lower ICC when the cases have a narrow range of values. The 95% CI Lower and 95% CI Upper columns of the Reproducibility Assessment table show how precisely these ", metrics$n_cases, " cases pin the figure down.")
                } else {
                    " That figure is the mean Spearman correlation rather than an ICC, because an ICC could not be fitted to these data. A correlation only asks whether the regions rank the cases in the same order, so two regions that rank identically but differ by a constant offset still score close to 1; the note under the Reproducibility Assessment table explains why the ICC was not estimable."
                }
                paste0(band, detail)
            } else if (!is.na(mean_cv)) {
                if (mean_cv < 15) {
                    "Variability between measurements was low in this dataset (mean CV below 15%)."
                } else if (mean_cv < 30) {
                    "Variability between measurements was moderate in this dataset (mean CV 15-30%)."
                } else {
                    "Variability between measurements was high in this dataset (mean CV of 30% or more)."
                }
            } else {
                "Data were insufficient to characterize sampling reliability."
            }

            summary_content <- paste0(
                "<div style='max-width: 700px; margin: 0 auto; padding: 20px; background-color: rgba(138, 155, 172, 0.06); border-radius: 8px; font-family: Arial, sans-serif; color: inherit;'>",
                "<h3 style='color: #495057; margin-bottom: 15px; text-align: center;'> Analysis Summary in Plain Language</h3>",

                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #007bff;'>",
                "<p style='margin: 0; font-size: 16px; line-height: 1.6;'>",
                "We analyzed <strong>", metrics$n_cases, " tissue samples</strong> to understand how well measurements from ",
                "<strong>", metrics$n_biopsies, " different regions</strong> represent biomarker expression.",
                "</p>",
                "</div>",

                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #28a745;'>",
                "<h4 style='color: #28a745; margin-top: 0;'> Key Findings:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                agreement_sentence,
                variability_sentence,
                correlation_sentence,
                "</ul>",
                "</div>",

                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #ffc107;'>",
                "<h4 style='color: #856404; margin-top: 0;'> Interpretation:</h4>",
                "<p style='margin: 0; line-height: 1.6;'>",
                clinical_sentence,
                "</p>",
                "</div>",

                "<div style='text-align: center; margin-top: 15px; font-size: 14px; color: #6c757d;'>",
                "<p style='margin: 0;'>This summary provides a simplified interpretation of the statistical results for clinical understanding.</p>",
                "</div>",

                "</div>"
            )

            self$results$summary$setContent(summary_content)
        },

        .calculateICC = function(whole_section, biopsy_data, correlations, has_reference, n_biopsies) {
            # Helper function for ICC calculation with proper validation and fallback
            icc_value <- NA
            icc_lower <- NA
            icc_upper <- NA
            icc_method <- "icc"   # downgraded to "correlation" on every fallback

            # Check if psych package is available
            if (!requireNamespace('psych', quietly = TRUE)) {
                mean_r <- mean(correlations, na.rm = TRUE)

                # Calculate CI using Fisher's z transformation
                ci_lower <- NA
                ci_upper <- NA

                # Prepare ICC data to get sample size
                if (has_reference) {
                    icc_data_temp <- cbind(whole_section, biopsy_data)
                } else {
                    icc_data_temp <- biopsy_data
                }
                complete_cases_temp <- complete.cases(icc_data_temp)
                n_complete <- sum(complete_cases_temp)

                if (!is.na(mean_r) && abs(mean_r) < 0.999 && n_complete >= 3) {
                    # Fisher's z transformation
                    fisher_z <- 0.5 * log((1 + mean_r) / (1 - mean_r))

                    # Standard error
                    se_z <- 1 / sqrt(n_complete - 3)

                    # 95% CI in z-space
                    z_lower <- fisher_z - 1.96 * se_z
                    z_upper <- fisher_z + 1.96 * se_z

                    # Back-transform to correlation scale
                    ci_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
                    ci_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
                }

                if (n_biopsies >= 2) {
                    note_text <- if (!is.na(mean_r) && !is.na(ci_lower)) {
                        paste0(
                            "Note: 'psych' package not available. Using correlation-based approximation ",
                            "(r = ", round(mean_r, 3), ", 95% CI [", round(ci_lower, 3), ", ", round(ci_upper, 3), "]) ",
                            "instead of ICC(3,1). Install 'psych' for exact ICC calculations."
                        )
                    } else {
                        "Note: 'psych' package not available. Install 'psych' for enhanced reliability metrics (ICC)."
                    }

                    self$results$interpretation$setNote(
                        key = "psych_missing",
                        note = note_text,
                        init = FALSE
                    )
                }

                return(list(
                    value = mean_r,
                    lower = ci_lower,
                    upper = ci_upper,
                    method = "correlation"
                ))
            }

            # Need at least 2 measurements for ICC
            if (n_biopsies < 2) {
                return(list(
                    value = mean(correlations, na.rm = TRUE),
                    lower = NA,
                    upper = NA,
                    method = "correlation"
                ))
            }

            # Prepare ICC data matrix
            if (has_reference) {
                icc_data <- cbind(whole_section, biopsy_data)
            } else {
                icc_data <- biopsy_data
            }

            # Remove incomplete cases
            complete_cases <- complete.cases(icc_data)
            icc_data <- icc_data[complete_cases, ]

            # Check minimum requirements: 3+ cases, 2+ measurements
            if (nrow(icc_data) < 3 || ncol(icc_data) < 2) {
                return(list(
                    value = mean(correlations, na.rm = TRUE),
                    lower = NA,
                    upper = NA,
                    method = "correlation"
                ))
            }

            # Check for sufficient variance
            col_vars <- apply(icc_data, 2, var, na.rm = TRUE)
            if (!all(col_vars > 1e-6)) {
                return(list(
                    value = mean(correlations, na.rm = TRUE),
                    lower = NA,
                    upper = NA,
                    method = "correlation"
                ))
            }

            # Attempt ICC calculation
            icc_err <- tryCatch({
                # complete.cases() above already removed every incomplete row, so
                # psych's lmer path (its default) buys nothing here: it only adds
                # cost and emits "boundary (singular) fit" warnings.
                icc_result <- psych::ICC(icc_data, lmer = FALSE)

                # ICC(2,1) - ABSOLUTE AGREEMENT, single measures (McGraw & Wong
                # ICC(A,1); psych row 2, `Single_random_raters`).
                #
                # psych::ICC()$results rows are ICC1, ICC2, ICC3, ICC1k, ICC2k,
                # ICC3k. This previously took row 3 = ICC(3,1), the two-way MIXED
                # CONSISTENCY form, which removes the region main effect from the
                # denominator and is therefore mathematically BLIND to a constant
                # additive or proportional offset between biopsy and whole section.
                # That is precisely the failure this analysis exists to detect: on
                # data where every region under-reads the whole section by 30% (a
                # true Ki67 of 60% scored as 42%) the consistency form returns 0.955
                # "Good reliability" while absolute agreement returns 0.669, and the
                # module's own bias table simultaneously reports p = 5e-14 and
                # "Large (>15%)" impact. Interchangeability is an absolute-agreement
                # question - see Koo & Li (2016), which this module already cites.
                icc_value <- icc_result$results$ICC[2]
                icc_lower <- icc_result$results$`lower bound`[2]
                icc_upper <- icc_result$results$`upper bound`[2]

                # Keep the consistency form for reference; it is reported as a
                # separate row so the two can never be confused again.
                private$.icc_consistency <- list(
                    value = icc_result$results$ICC[3],
                    lower = icc_result$results$`lower bound`[3],
                    upper = icc_result$results$`upper bound`[3]
                )

                # Validate ICC result
                if (is.na(icc_value) || icc_value < -1 || icc_value > 1) {
                    icc_value <- mean(correlations, na.rm = TRUE)
                    icc_lower <- icc_upper <- NA
                    icc_method <- "correlation"
                }
                NULL
            }, error = function(e) e)
            if (!is.null(icc_err)) {
                # Fallback to mean correlation (assigned here with `<-` instead
                # of `<<-` from inside the error handler).
                icc_value <- mean(correlations, na.rm = TRUE)
                icc_lower <- icc_upper <- NA
                icc_method <- "correlation" 
            }

            return(list(
                value = icc_value,
                lower = icc_lower,
                upper = icc_upper,
                method = icc_method
            ))
        },

        .detectMisuse = function(whole_section, biopsy_data) {
            warnings <- c()

            # Check sample size adequacy
            n_cases <- if (!is.null(whole_section)) length(whole_section) else nrow(biopsy_data)
            if (n_cases < 10) {
                warnings <- c(warnings, paste0("Small sample size (n=", n_cases, ") may reduce statistical power. Consider collecting more cases for reliable estimates."))
            }

            # Check for extreme values (outliers)
            combined_data <- c(whole_section, as.matrix(biopsy_data))
            combined_data <- combined_data[!is.na(combined_data)]

            if (length(combined_data) > 0) {
                q1 <- quantile(combined_data, 0.25, na.rm = TRUE)
                q3 <- quantile(combined_data, 0.75, na.rm = TRUE)
                iqr <- q3 - q1
                outliers <- combined_data < (q1 - 1.5 * iqr) | combined_data > (q3 + 1.5 * iqr)

                if (sum(outliers) > length(combined_data) * 0.1) {  # More than 10% outliers
                    warnings <- c(warnings, paste0("High number of outliers detected (", round(sum(outliers)/length(combined_data)*100, 1), "%). Consider checking for measurement errors or data entry issues."))
                }
            }

            # Check coefficient of variation
            cv_values <- c()
            for (i in seq_len(nrow(biopsy_data))) {
                row_data <- c(whole_section[i], as.numeric(biopsy_data[i, ]))
                row_data <- row_data[!is.na(row_data)]
                if (length(row_data) >= 2) {
                    cv <- sd(row_data) / mean(row_data) * 100
                    if (!is.na(cv)) cv_values <- c(cv_values, cv)
                }
            }

            if (length(cv_values) > 0) {
                high_cv_cases <- sum(cv_values > 50, na.rm = TRUE)
                if (high_cv_cases > length(cv_values) * 0.2) {  # More than 20% high CV
                    warnings <- c(warnings, paste0("Very high variability (CV > 50%) detected in ", high_cv_cases, " cases. This may indicate measurement inconsistencies or high biological heterogeneity."))
                }
            }

            # Check for constant values (no variance)
            zero_var_columns <- apply(biopsy_data, 2, function(x) {
                v <- var(x, na.rm = TRUE)
                !is.na(v) && v == 0
            })
            if (any(zero_var_columns)) {
                warnings <- c(warnings, "One or more regional measurements show no variability (constant values). Check for data entry errors.")
            }

            # Check missing data patterns
            missing_percent <- sum(is.na(biopsy_data)) / (nrow(biopsy_data) * ncol(biopsy_data)) * 100
            if (missing_percent > 20) {
                warnings <- c(warnings, paste0("High percentage of missing regional measurements (", round(missing_percent, 1), "%). This may affect reliability of heterogeneity assessment."))
            }

            # Check for inappropriate data range (negative values for biomarkers that should be positive)
            if (any(c(whole_section, as.matrix(biopsy_data)) < 0, na.rm = TRUE)) {
                warnings <- c(warnings, "Negative values detected. Most IHC biomarkers should have non-negative values (e.g., Ki67 %, H-scores). Verify data coding.")
            }

            # Check for values outside typical biomarker ranges
            max_value <- max(c(whole_section, as.matrix(biopsy_data)), na.rm = TRUE)
            if (!is.na(max_value) && max_value > 300) {
                warnings <- c(warnings, paste0("Very high biomarker values detected (max: ", round(max_value, 1), "). Verify if these are appropriate for your biomarker scale (e.g., percentages should be \u{2264}100%, H-scores \u{2264}300)."))
            }

            return(warnings)
        },

        .compareCompartments = function(whole_section, biopsy_data, spatial_regions) {
            # Compare heterogeneity metrics (ICC, CV, mean bias) across spatial compartments
            unique_regions <- unique(spatial_regions)
            unique_regions <- unique_regions[!is.na(unique_regions)]

            if (length(unique_regions) < 2) {
                # Need at least 2 compartments for comparison
                return()
            }

            comp_table <- self$results$compartmentComparison
            has_reference <- !is.null(whole_section)
            row_key <- 1

            # For each compartment, calculate key heterogeneity metrics
            compartment_stats <- list()

            for (region in unique_regions) {
                region_mask <- spatial_regions == region & !is.na(spatial_regions)

                if (sum(region_mask) < 3) {
                    # Need at least 3 cases for meaningful statistics
                    next
                }

                region_whole_section <- if (has_reference) whole_section[region_mask] else NULL
                region_biopsy_data <- biopsy_data[region_mask, , drop = FALSE]

                # Calculate ICC for this compartment
                if (has_reference && nrow(region_biopsy_data) >= 3) {
                    region_icc_data <- cbind(region_whole_section, region_biopsy_data)
                    region_complete <- complete.cases(region_icc_data)
                    region_icc_data <- region_icc_data[region_complete, , drop = FALSE]

                    if (nrow(region_icc_data) >= 3 && ncol(region_icc_data) >= 2) {
                        # Calculate ICC using existing helper
                        region_correlations <- cor(region_icc_data[, 1],
                                                   rowMeans(region_icc_data[, -1, drop = FALSE]),
                                                   method = "spearman", use = "complete.obs")

                        icc_result <- private$.calculateICC(
                            whole_section = region_whole_section,
                            biopsy_data = region_biopsy_data,
                            correlations = region_correlations,
                            has_reference = TRUE,
                            n_biopsies = ncol(region_biopsy_data)
                        )
                        region_icc <- icc_result$value
                        region_icc_lower <- icc_result$lower
                        region_icc_upper <- icc_result$upper
                        # .calculateICC falls back to the mean Spearman correlation;
                        # carry the flag so the table row is not labelled "ICC".
                        region_icc_method <- icc_result$method
                    } else {
                        region_icc <- region_icc_lower <- region_icc_upper <- NA
                        region_icc_method <- NA_character_
                    }
                } else {
                    region_icc <- region_icc_lower <- region_icc_upper <- NA
                    region_icc_method <- NA_character_
                }

                # Calculate mean CV for this compartment
                region_cv_values <- c()
                for (i in seq_len(nrow(region_biopsy_data))) {
                    if (has_reference && i <= length(region_whole_section)) {
                        case_values <- c(region_whole_section[i], as.numeric(region_biopsy_data[i, ]))
                    } else {
                        case_values <- as.numeric(region_biopsy_data[i, ])
                    }
                    case_values <- case_values[!is.na(case_values)]
                    if (length(case_values) >= 2) {
                        cv <- private$.calculateRobustCV(case_values)
                        if (!is.na(cv) && is.finite(cv)) {
                            region_cv_values <- c(region_cv_values, cv)
                        }
                    }
                }
                region_mean_cv <- if (length(region_cv_values) > 0) mean(region_cv_values) else NA

                # Calculate mean bias (if reference available)
                if (has_reference && nrow(region_biopsy_data) > 0) {
                    region_biopsy_means <- rowMeans(region_biopsy_data, na.rm = TRUE)
                    complete_pairs <- complete.cases(region_whole_section, region_biopsy_means)
                    if (sum(complete_pairs) >= 3) {
                        bias_test <- private$.safePairedT(
                            region_biopsy_means[complete_pairs],
                            region_whole_section[complete_pairs])
                        region_mean_bias <- bias_test$estimate
                    } else {
                        region_mean_bias <- NA
                    }
                } else {
                    region_mean_bias <- NA
                }

                # Store compartment statistics
                compartment_stats[[as.character(region)]] <- list(
                    icc = region_icc,
                    icc_method = region_icc_method,
                    icc_lower = region_icc_lower,
                    icc_upper = region_icc_upper,
                    cv = region_mean_cv,
                    bias = region_mean_bias,
                    n_cases = sum(region_mask)
                )
            }

            # Populate comparison table with results
            for (region in names(compartment_stats)) {
                stats <- compartment_stats[[region]]

                # ICC row
                if (!is.na(stats$icc)) {
                    # Compare to other compartments
                    other_iccs <- sapply(compartment_stats[names(compartment_stats) != region],
                                        function(x) x$icc)
                    other_iccs <- other_iccs[!is.na(other_iccs)]

                    comparison_text <- if (length(other_iccs) > 0) {
                        mean_other <- mean(other_iccs)
                        diff <- stats$icc - mean_other
                        if (abs(diff) < 0.05) {
                            "Similar to other compartments"
                        } else if (diff > 0) {
                            sprintf("Higher reliability (+%.2f)", diff)
                        } else {
                            sprintf("Lower reliability (%.2f)", diff)
                        }
                    } else {
                        "No comparison available"
                    }

                    comp_table$addRow(rowKey = row_key, values = list(
                        # .calculateICC returns ICC(2,1) absolute agreement, or the
                        # mean Spearman correlation when no ICC could be fitted.
                        metric = if (identical(stats$icc_method, "icc"))
                                     "ICC(2,1) - absolute agreement"
                                 else "Mean correlation (ICC not estimable)",
                        compartment = region,
                        value = stats$icc,
                        ci_lower = stats$icc_lower,
                        ci_upper = stats$icc_upper,
                        comparison = comparison_text
                    ))
                    row_key <- row_key + 1
                }

                # CV row
                if (!is.na(stats$cv)) {
                    other_cvs <- sapply(compartment_stats[names(compartment_stats) != region],
                                       function(x) x$cv)
                    other_cvs <- other_cvs[!is.na(other_cvs)]

                    comparison_text <- if (length(other_cvs) > 0) {
                        mean_other <- mean(other_cvs)
                        diff <- stats$cv - mean_other
                        if (abs(diff) < 5) {
                            "Similar variability"
                        } else if (diff > 0) {
                            sprintf("Higher variability (+%.1f%%)", diff)
                        } else {
                            sprintf("Lower variability (%.1f%%)", diff)
                        }
                    } else {
                        "No comparison available"
                    }

                    comp_table$addRow(rowKey = row_key, values = list(
                        metric = "Mean CV (%)",
                        compartment = region,
                        value = stats$cv,
                        ci_lower = NA,
                        ci_upper = NA,
                        comparison = comparison_text
                    ))
                    row_key <- row_key + 1
                }

                # Bias row (if reference available)
                if (!is.na(stats$bias)) {
                    other_bias <- sapply(compartment_stats[names(compartment_stats) != region],
                                        function(x) x$bias)
                    other_bias <- other_bias[!is.na(other_bias)]

                    comparison_text <- if (length(other_bias) > 0) {
                        mean_other <- mean(other_bias)
                        diff <- stats$bias - mean_other
                        if (abs(diff) < 0.05) {
                            "Similar bias"
                        } else if (diff > 0) {
                            "Higher positive bias"
                        } else {
                            "Higher negative bias"
                        }
                    } else {
                        "No comparison available"
                    }

                    comp_table$addRow(rowKey = row_key, values = list(
                        metric = "Mean Bias",
                        compartment = region,
                        value = stats$bias,
                        ci_lower = NA,
                        ci_upper = NA,
                        comparison = comparison_text
                    ))
                    row_key <- row_key + 1
                }
            }
        },

        .performCompartmentTests = function(whole_section, biopsy_data, spatial_regions) {
            # Perform formal statistical tests comparing heterogeneity across compartments
            # Uses Levene's test for variance equality and Kruskal-Wallis for distributional differences

            unique_regions <- unique(spatial_regions)
            unique_regions <- unique_regions[!is.na(unique_regions)]

            if (length(unique_regions) < 2) {
                return()
            }

            test_table <- self$results$compartmentTests
            has_reference <- !is.null(whole_section)
            row_key <- 1

            # Prepare data for testing: calculate CV values per case per compartment
            cv_by_compartment <- list()
            values_by_compartment <- list()

            for (region in unique_regions) {
                region_mask <- spatial_regions == region & !is.na(spatial_regions)

                if (sum(region_mask) < 2) {
                    next
                }

                region_whole_section <- if (has_reference) whole_section[region_mask] else NULL
                region_biopsy_data <- biopsy_data[region_mask, , drop = FALSE]

                # Calculate CV for each case in this compartment
                region_cvs <- c()
                region_values <- c()

                for (i in seq_len(nrow(region_biopsy_data))) {
                    if (has_reference && i <= length(region_whole_section)) {
                        case_values <- c(region_whole_section[i], as.numeric(region_biopsy_data[i, ]))
                    } else {
                        case_values <- as.numeric(region_biopsy_data[i, ])
                    }
                    case_values <- case_values[!is.na(case_values)]

                    if (length(case_values) >= 2) {
                        cv <- private$.calculateRobustCV(case_values)
                        if (!is.na(cv) && is.finite(cv)) {
                            region_cvs <- c(region_cvs, cv)
                        }
                    }

                    # Collect all values for distribution testing
                    region_values <- c(region_values, case_values)
                }

                if (length(region_cvs) > 0) {
                    cv_by_compartment[[as.character(region)]] <- region_cvs
                }
                if (length(region_values) > 0) {
                    values_by_compartment[[as.character(region)]] <- region_values
                }
            }

            # 1. Levene's Test for Variance Homogeneity of CV values across compartments
            if (length(cv_by_compartment) >= 2) {
                # Prepare data for Levene's test
                cv_values <- unlist(cv_by_compartment)
                cv_groups <- rep(names(cv_by_compartment),
                                sapply(cv_by_compartment, length))

                if (length(cv_values) >= 6 && length(unique(cv_groups)) >= 2) {
                    # Perform Levene's test manually (using median-based approach)
                    tryCatch({
                        # Calculate group medians
                        group_medians <- tapply(cv_values, cv_groups, median, na.rm = TRUE)

                        # Calculate absolute deviations from group medians
                        abs_dev <- abs(cv_values - group_medians[cv_groups])

                        # One-way ANOVA on the absolute deviations. Computed with
                        # aov(), not oneway.test().
                        #
                        # ROOT CAUSE (corrected 2026-08-11; the earlier note here
                        # blamed R6 and this package's namespace, which was wrong):
                        # `formula.tools` registers an `as.character.formula` S3
                        # method that returns ONE deparsed string ("y ~ g") where
                        # base R returns c("~", "y", "g"). stats::oneway.test's
                        # second guard is `length(as.character(formula)) != 3L`,
                        # so it rejects every valid formula once that package is
                        # loaded - for the whole R session, not just this module.
                        # formula.tools arrives transitively through logistf
                        # (Imports), which firthregression loads on demand, and
                        # eagerly under devtools::load_all. Only oneway.test is
                        # affected; t.test, kruskal.test and bartlett.test formula
                        # methods are unharmed. The error was swallowed by the
                        # surrounding tryCatch, so this row ALWAYS rendered
                        # "Could not compute" and Levene's test never once reported
                        # a result. aov() gives the identical F, df and p.
                        lev_df_frame <- data.frame(abs_dev = as.numeric(abs_dev),
                                                   grp = factor(cv_groups))
                        lev_tab <- summary(stats::aov(abs_dev ~ grp, data = lev_df_frame))[[1]]

                        levene_statistic <- lev_tab[["F value"]][1]
                        levene_df <- lev_tab[["Df"]][1]
                        levene_p <- lev_tab[["Pr(>F)"]][1]

                        interpretation <- if (levene_p < 0.05) {
                            "Significant difference in variability heterogeneity across compartments"
                        } else {
                            "No significant difference in variability patterns detected across compartments; this does not establish that they are the same"
                        }

                        test_table$addRow(rowKey = row_key, values = list(
                            test_type = "Levene's Test (CV Variance)",
                            statistic = levene_statistic,
                            # oneway.test()$parameter is c(num df, denom df).
                            # Passing the length-2 vector made addRow() throw
                            # "value is not atomic" inside the surrounding
                            # tryCatch, so this row ALWAYS rendered as
                            # "Could not compute" and Levene's test never once
                            # reported a result.
                            df = as.integer(levene_df[1]),
                            p_value = levene_p,
                            interpretation = interpretation
                        ))
                    }, error = function(e) {
                        # Report WHY. The old text blamed "insufficient data or
                        # computational error" for what was in fact a coding bug
                        # in this very block, so the failure looked like a
                        # property of the user's data for as long as it existed.
                        test_table$addRow(rowKey = row_key, values = list(
                            test_type = "Levene's Test (CV Variance)",
                            statistic = NA,
                            df = NA,
                            p_value = NA,
                            interpretation = paste("Could not compute:", conditionMessage(e))
                        ))
                    })
                    # Increment once after either branch (avoids `<<-` in the handler).
                    row_key <- row_key + 1
                }
            }

            # 2. Kruskal-Wallis Test for Distribution Differences across compartments
            if (length(values_by_compartment) >= 2) {
                all_values <- unlist(values_by_compartment)
                all_groups <- rep(names(values_by_compartment),
                                 sapply(values_by_compartment, length))

                if (length(all_values) >= 6 && length(unique(all_groups)) >= 2) {
                    tryCatch({
                        kw_result <- kruskal.test(all_values ~ all_groups)

                        kw_statistic <- kw_result$statistic
                        kw_df <- kw_result$parameter
                        kw_p <- kw_result$p.value

                        interpretation <- if (kw_p < 0.05) {
                            "Significant difference in biomarker distributions across compartments"
                        } else {
                            "No significant difference in biomarker distributions detected across compartments; this does not establish that the distributions are the same"
                        }

                        test_table$addRow(rowKey = row_key, values = list(
                            test_type = "Kruskal-Wallis Test (Distribution)",
                            statistic = kw_statistic,
                            df = as.integer(kw_df),
                            p_value = kw_p,
                            interpretation = interpretation
                        ))
                    }, error = function(e) {
                        test_table$addRow(rowKey = row_key, values = list(
                            test_type = "Kruskal-Wallis Test (Distribution)",
                            statistic = NA,
                            df = NA,
                            p_value = NA,
                            interpretation = "Could not compute: insufficient data or computational error"
                        ))
                    })
                    # Increment once after either branch (avoids `<<-` in the handler).
                    row_key <- row_key + 1
                }
            }
        }

    )
)
