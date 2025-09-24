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
            MIN_CASES_ANALYSIS = 5          # Minimum cases for analysis
        ),

        .repro_stats = NULL,

        # Add variable name safety utility
        .escapeVar = function(x) {
            # Handle variables with spaces and special characters
            if (is.null(x) || length(x) == 0) return(x)
            gsub("[^A-Za-z0-9_]+", "_", make.names(x))
        },

        .init = function() {
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
            
            # Initialize results tables
            private$.initializeTables()
            
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

            self$results$poweranalysistable$setVisible(show_power)
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
            # Check required variables - need at least 2 regional measurements for analysis
            # wholesection is now optional (for inter-regional studies)
            if (is.null(self$options$biopsy1) && is.null(self$options$biopsies)) {
                return()
            }

            # Count available regional measurements
            regional_count <- sum(!sapply(list(self$options$biopsy1, self$options$biopsy2,
                                             self$options$biopsy3, self$options$biopsy4), is.null))
            if (!is.null(self$options$biopsies)) {
                regional_count <- regional_count + length(self$options$biopsies)
            }

            if (regional_count < 2) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> At least 2 regional measurements are required for heterogeneity analysis.</p>"
                )
                return()
            }
            
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Extract reference data (optional)
            whole_section <- if (!is.null(self$options$wholesection)) {
                data[[private$.escapeVar(self$options$wholesection)]]
            } else {
                NULL
            }

            # Handle multiple regional measurement columns
            biopsy_data <- private$.extractRegionalData(data)
            
            # Enhanced data quality checks with specific warnings
            min_cases_needed <- 5
            has_reference <- !is.null(whole_section)

            # Check data sufficiency based on study design
            if (has_reference && (length(whole_section) < min_cases_needed || nrow(biopsy_data) < min_cases_needed)) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for reference-based heterogeneity analysis.
                    At least 5 complete cases with reference and regional measurements are required.</p>"
                )
                return()
            } else if (!has_reference && nrow(biopsy_data) < min_cases_needed) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for inter-regional heterogeneity analysis.
                    At least 5 complete cases with regional measurements are required.</p>"
                )
                return()
            }

            # Add misuse detection warnings
            warnings <- private$.detectMisuse(whole_section, biopsy_data)
            if (length(warnings) > 0) {
                warning_html <- paste0(
                    "<div style='background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                    "<h4 style='color: #856404; margin-top: 0;'>⚠️ Data Quality Warnings</h4>",
                    "<ul style='color: #856404; margin: 5px 0; padding-left: 20px;'>",
                    paste0("<li>", warnings, "</li>", collapse = ""),
                    "</ul>",
                    "</div>"
                )
                current_content <- self$results$interpretation$state
                if (!is.null(current_content)) {
                    self$results$interpretation$setContent(paste0(warning_html, current_content))
                } else {
                    self$results$interpretation$setContent(warning_html)
                }
            }
            
            # Get optional spatial data
            spatial_regions <- NULL
            if (!is.null(self$options$spatial_id)) {
                spatial_id_var <- private$.escapeVar(self$options$spatial_id)
                if (spatial_id_var %in% names(data)) {
                    spatial_regions <- data[[spatial_id_var]]
                } else {
                    # Handle case where spatial_id is selected but not in data
                    self$results$interpretation$setContent(
                        paste0("<p style='color: red;'><strong>Error:</strong> Spatial ID variable '", self$options$spatial_id, "' not found in data.</p>")
                    )
                    return()
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
        
        .initializeTables = function() {
            # Reproducibility metrics table
            repro_table <- self$results$reproducibilitytable
            repro_table$addColumn(name = 'metric', title = 'Reproducibility Metric', type = 'text')
            repro_table$addColumn(name = 'value', title = 'Value', type = 'number', format = 'zto')
            repro_table$addColumn(name = 'ci_lower', title = '95% CI Lower', type = 'number', format = 'zto')
            repro_table$addColumn(name = 'ci_upper', title = '95% CI Upper', type = 'number', format = 'zto')
            repro_table$addColumn(name = 'interpretation', title = 'Clinical Interpretation', type = 'text')
            
            # Sampling bias table
            bias_table <- self$results$samplingbiastable
            bias_table$addColumn(name = 'comparison', title = 'Comparison', type = 'text')
            bias_table$addColumn(name = 'mean_diff', title = 'Mean Difference', type = 'number', format = 'zto')
            bias_table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            bias_table$addColumn(name = 'effect_size', title = 'Effect Size (Cohen\'s d)', type = 'number', format = 'zto')
            bias_table$addColumn(name = 'clinical_impact', title = 'Clinical Impact', type = 'text')
            
            # Variance components table
            variance_table <- self$results$variancetable
            variance_table$addColumn(name = 'component', title = 'Variance Component', type = 'text')
            variance_table$addColumn(name = 'variance', title = 'Variance', type = 'number', format = 'zto')
            variance_table$addColumn(name = 'percentage', title = 'Percentage (%)', type = 'number', format = 'zto')
            variance_table$addColumn(name = 'contribution', title = 'Contribution to Total Variance', type = 'text')
            
            # Power analysis table (if enabled)
            if (self$options$power_analysis) {
                power_table <- self$results$poweranalysistable
                power_table$addColumn(name = 'scenario', title = 'Analysis Scenario', type = 'text')
                power_table$addColumn(name = 'effect_size', title = 'Expected Effect Size', type = 'number', format = 'zto')
                power_table$addColumn(name = 'power', title = 'Statistical Power', type = 'number', format = 'pc')
                power_table$addColumn(name = 'required_n', title = 'Required Sample Size', type = 'integer')
                power_table$addColumn(name = 'recommendation', title = 'Recommendation', type = 'text')
            }
            
            # Spatial analysis table (if spatial data provided)
            if (!is.null(self$options$spatial_id)) {
                spatial_table <- self$results$spatialanalysistable
                spatial_table$addColumn(name = 'region', title = 'Spatial Region', type = 'text')
                spatial_table$addColumn(name = 'n_cases', title = 'Cases', type = 'integer')
                spatial_table$addColumn(name = 'mean_value', title = 'Mean Value', type = 'number', format = 'zto')
                spatial_table$addColumn(name = 'cv_percent', title = 'CV (%)', type = 'number', format = 'zto')
                spatial_table$addColumn(name = 'heterogeneity_level', title = 'Heterogeneity Level', type = 'text')
            }
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

            # Apply variable name safety for column extraction
            biopsy_columns <- sapply(biopsy_columns, private$.escapeVar, USE.NAMES = FALSE)

            # Create biopsy data matrix
            biopsy_data <- data[, biopsy_columns, drop = FALSE]
            
            # Remove rows with all missing biopsy values
            complete_rows <- rowSums(!is.na(biopsy_data)) > 0
            biopsy_data <- biopsy_data[complete_rows, , drop = FALSE]
            
            return(biopsy_data)
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

            # Apply sampling strategy interpretation if needed
            if (nchar(interpretation_text) > 0) {
                current_interp <- self$results$interpretation$state
                if (is.null(current_interp)) {
                    self$results$interpretation$setContent(interpretation_text)
                } else {
                    self$results$interpretation$setContent(paste(current_interp, interpretation_text, sep=""))
                }
            }
        },
        
        .analyzeReproducibility = function(whole_section, biopsy_data, correlation_threshold = 0.80, cv_threshold = 20.0, study_design = "reference_based") {
            n_biopsies <- ncol(biopsy_data)
            n_cases <- nrow(biopsy_data)
            has_reference <- !is.null(whole_section) && study_design == "reference_based"

            # Initialize correlation variables
            correlations <- c()

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
                # Inter-regional study: use mean of all pairwise correlations between regions
                if (n_biopsies >= 2) {
                    cor_matrix <- cor(biopsy_data, use = "pairwise.complete.obs", method = "spearman")
                    upper_tri_indices <- which(upper.tri(cor_matrix), arr.ind = TRUE)
                    correlations <- cor_matrix[upper_tri_indices]
                    correlations <- correlations[!is.na(correlations)]
                } else {
                    correlations <- NA
                }
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
            
            # Inter-regional reproducibility (vectorized approach)
            if (n_biopsies >= 2) {
                # Use vectorized correlation matrix calculation
                cor_matrix <- cor(biopsy_data, use = "pairwise.complete.obs", method = "spearman")

                # Extract upper triangle correlations efficiently
                upper_tri_indices <- which(upper.tri(cor_matrix), arr.ind = TRUE)
                inter_biopsy_corr <- cor_matrix[upper_tri_indices]

                # Remove NAs and calculate mean
                inter_biopsy_corr <- inter_biopsy_corr[!is.na(inter_biopsy_corr)]
                mean_inter_biopsy <- if (length(inter_biopsy_corr) > 0) mean(inter_biopsy_corr) else NA
            } else {
                mean_inter_biopsy <- NA
            }
            
            # Populate reproducibility table
            repro_table <- self$results$reproducibilitytable
            
            repro_table$addRow(rowKey = 1, values = list(
                metric = "Mean Regional-Reference Correlation",
                value = mean(correlations, na.rm = TRUE),
                ci_lower = NA,
                ci_upper = NA,
                interpretation = ifelse(mean(correlations, na.rm = TRUE) >= correlation_threshold, 
                                       "Good representativeness", "Limited representativeness")
            ))
            
            if (!is.na(icc_value)) {
                repro_table$addRow(rowKey = 2, values = list(
                    metric = "ICC(3,1) - All Methods",
                    value = icc_value,
                    ci_lower = icc_lower,
                    ci_upper = icc_upper,
                    interpretation = ifelse(icc_value >= 0.75, "Good reliability", 
                                           ifelse(icc_value >= 0.50, "Moderate reliability", "Poor reliability"))
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
            
            # Coefficient of variation
            biopsy_means <- rowMeans(biopsy_data, na.rm = TRUE)
            biopsy_sds <- apply(biopsy_data, 1, sd, na.rm = TRUE)
            cv_values <- biopsy_sds / biopsy_means * 100
            mean_cv <- mean(cv_values[is.finite(cv_values)], na.rm = TRUE)
            
            repro_table$addRow(rowKey = 4, values = list(
                metric = "Mean Coefficient of Variation (%)",
                value = mean_cv,
                ci_lower = NA,
                ci_upper = NA,
                interpretation = ifelse(mean_cv <= cv_threshold/2, "Low variability", 
                                       ifelse(mean_cv <= cv_threshold, "Moderate variability", "High variability"))
            ))
            return(list(icc_value = icc_value, correlations = correlations, mean_inter_biopsy = mean_inter_biopsy))
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
                        bias_test <- t.test(biopsy_complete, ws_complete, paired = TRUE)
                        mean_diff <- bias_test$estimate
                        p_value <- bias_test$p.value
                        
                        # Effect size (Cohen's d for paired data)
                        diff_vals <- biopsy_complete - ws_complete
                        cohens_d <- mean(diff_vals) / sd(diff_vals)
                        
                        # Clinical impact assessment
                        relative_bias <- abs(mean_diff) / mean(ws_complete) * 100
                        clinical_impact <- ifelse(relative_bias <= 5, "Minimal (<5%)", 
                                                ifelse(relative_bias <= 15, "Moderate (5-15%)", "Large (>15%)"))
                        
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
                        overall_test <- t.test(biopsy_means[complete_pairs], whole_section[complete_pairs], paired = TRUE)
                        overall_diff <- overall_test$estimate
                        overall_p <- overall_test$p.value
                        
                        # Effect size
                        diff_vals <- biopsy_means[complete_pairs] - whole_section[complete_pairs]
                        overall_d <- mean(diff_vals) / sd(diff_vals)
                        
                        relative_bias <- abs(overall_diff) / mean(whole_section[complete_pairs]) * 100
                        clinical_impact <- ifelse(relative_bias <= 5, "Minimal (<5%)", 
                                                ifelse(relative_bias <= 15, "Moderate (5-15%)", "Large (>15%)"))
                        
                        bias_table$addRow(rowKey = row_key, values = list(
                            comparison = "Mean of Regions vs Reference",
                            mean_diff = overall_diff,
                            p_value = overall_p,
                            effect_size = overall_d,
                            clinical_impact = clinical_impact
                        ))
                    }
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
            # Variance component analysis to understand sources of variability
            has_reference <- !is.null(whole_section)

            biopsy_matrix <- as.matrix(biopsy_data)
            all_values <- as.numeric(biopsy_matrix)
            if (has_reference) {
                all_values <- c(whole_section, all_values)
            }
            all_values <- all_values[!is.na(all_values)]

            total_variance <- if (length(all_values) >= 2) var(all_values, na.rm = TRUE) else NA

            if (has_reference) {
                between_case_var <- if (length(whole_section) >= 2) var(whole_section, na.rm = TRUE) else NA
            } else {
                case_means <- rowMeans(biopsy_matrix, na.rm = TRUE)
                case_means <- case_means[!is.na(case_means)]
                between_case_var <- if (length(case_means) >= 2) var(case_means, na.rm = TRUE) else NA
            }

            within_case_vars <- c()
            for (i in seq_len(nrow(biopsy_matrix))) {
                row_values <- as.numeric(biopsy_matrix[i, ])
                if (has_reference && i <= length(whole_section)) {
                    row_values <- c(whole_section[i], row_values)
                }
                row_values <- row_values[!is.na(row_values)]
                if (length(row_values) >= 2) {
                    within_case_vars <- c(within_case_vars, var(row_values))
                }
            }
            mean_within_case_var <- if (length(within_case_vars) > 0) mean(within_case_vars, na.rm = TRUE) else NA

            method_means <- if (has_reference) {
                c(mean(whole_section, na.rm = TRUE), apply(biopsy_matrix, 2, mean, na.rm = TRUE))
            } else {
                apply(biopsy_matrix, 2, mean, na.rm = TRUE)
            }
            method_means <- method_means[!is.na(method_means)]
            method_variance <- if (length(method_means) >= 2) var(method_means, na.rm = TRUE) else NA

            safe_pct <- function(x) {
                if (is.na(x) || is.na(total_variance) || total_variance <= 0) {
                    return(NA_real_)
                }
                x / total_variance * 100
            }

            between_case_pct <- safe_pct(between_case_var)
            within_case_pct <- safe_pct(mean_within_case_var)
            method_pct <- safe_pct(method_variance)

            variance_table <- self$results$variancetable

            variance_table$addRow(rowKey = 1, values = list(
                component = if (has_reference) "Between-Case Variance" else "Between-Case Variance (Regional Means)",
                variance = between_case_var,
                percentage = between_case_pct,
                contribution = ifelse(!is.na(between_case_pct) && between_case_pct >= 60, "Major contributor",
                                     ifelse(!is.na(between_case_pct) && between_case_pct >= 30, "Moderate contributor", "Minor contributor"))
            ))

            variance_table$addRow(rowKey = 2, values = list(
                component = "Within-Case Variance (Sampling)",
                variance = mean_within_case_var,
                percentage = within_case_pct,
                contribution = ifelse(!is.na(within_case_pct) && within_case_pct >= 30, "High sampling variability",
                                     ifelse(!is.na(within_case_pct) && within_case_pct >= 15, "Moderate sampling variability", "Low sampling variability"))
            ))

            variance_table$addRow(rowKey = 3, values = list(
                component = if (has_reference) "Method Variance" else "Regional Method Variance",
                variance = method_variance,
                percentage = method_pct,
                contribution = ifelse(!is.na(method_pct) && method_pct >= 20, "Significant method differences",
                                     ifelse(!is.na(method_pct) && method_pct >= 10, "Minor method differences", "Negligible method differences"))
            ))

            variance_table$addRow(rowKey = 4, values = list(
                component = "Total Variance",
                variance = total_variance,
                percentage = if (!is.na(total_variance)) 100 else NA,
                contribution = "Sum of all variance components"
            ))
        },
        
        .performPowerAnalysis = function(whole_section, biopsy_data) {
            if (is.null(whole_section))
                return()

            # Perform power analysis for different scenarios
            n_cases <- length(whole_section)
            
            # Calculate observed effect sizes
            biopsy_means <- rowMeans(biopsy_data, na.rm = TRUE)
            complete_pairs <- complete.cases(whole_section, biopsy_means)
            
            if (sum(complete_pairs) >= 3) {
                # Observed correlation effect size
                obs_correlation <- cor(whole_section[complete_pairs], biopsy_means[complete_pairs])
                
                # Convert correlation to effect size (Cohen's convention)
                # Small: r = 0.1, Medium: r = 0.3, Large: r = 0.5
                correlation_categories <- c(0.1, 0.3, 0.5, obs_correlation)
                
                power_table <- self$results$poweranalysistable
                row_key <- 1
                
                for (effect_size in correlation_categories) {
                    # Power calculation for correlation (Fisher's z transformation)
                    z_effect <- 0.5 * log((1 + effect_size) / (1 - effect_size))
                    se_z <- 1 / sqrt(n_cases - 3)
                    z_score <- z_effect / se_z
                    power <- pnorm(z_score - qnorm(0.975)) + pnorm(-z_score - qnorm(0.975))
                    
                    # Required sample size for 80% power
                    z_alpha <- qnorm(0.975)  # two-tailed test
                    z_beta <- qnorm(0.80)    # 80% power
                    required_n <- ceiling(((z_alpha + z_beta) / z_effect)^2 + 3)
                    
                    scenario <- if (effect_size == obs_correlation) {
                        "Observed Effect Size"
                    } else if (effect_size == 0.1) {
                        "Small Effect (r=0.1)"
                    } else if (effect_size == 0.3) {
                        "Medium Effect (r=0.3)"
                    } else {
                        "Large Effect (r=0.5)"
                    }
                    
                    recommendation <- if (power >= 0.80) {
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
                        required_n = pmax(required_n, 5),  # Minimum of 5
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
                        
                        # Calculate regional statistics
                        region_values <- c(region_whole_section, as.matrix(region_biopsy_data))
                        region_mean <- mean(region_values, na.rm = TRUE)
                        region_cv <- sd(region_values, na.rm = TRUE) / region_mean * 100
                        
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
                    list(case_id = i, cv = sd(case_values) / mean(case_values) * 100)
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
                        region_cv <- sd(all_regional_values, na.rm = TRUE) / region_mean * 100
                        
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
                    overall_corr <- cor(whole_section[complete_pairs], biopsy_means[complete_pairs], method = "spearman")
                    bias_test <- t.test(biopsy_means[complete_pairs], whole_section[complete_pairs], paired = TRUE)
                    mean_bias <- bias_test$estimate
                    bias_p <- bias_test$p.value
                } else {
                    overall_corr <- mean_bias <- bias_p <- NA
                }
            } else {
                overall_corr <- if (!is.null(repro_stats$mean_inter_biopsy)) repro_stats$mean_inter_biopsy else NA
                mean_bias <- NA
                bias_p <- NA
            }

            if (has_reference && nrow(biopsy_data) > 0) {
                calculate_case_cv <- function(whole_val, biopsy_row) {
                    values <- c(whole_val, as.numeric(biopsy_row))
                    values <- values[!is.na(values)]
                    if (length(values) >= 2) {
                        sd(values) / mean(values) * 100
                    } else {
                        NA
                    }
                }

                cv_values <- mapply(
                    calculate_case_cv,
                    whole_section,
                    split(biopsy_data, seq_len(nrow(biopsy_data)))
                )
            } else if (nrow(biopsy_data) > 0) {
                cv_values <- apply(biopsy_data, 1, function(row) {
                    row_vals <- as.numeric(row)
                    row_vals <- row_vals[!is.na(row_vals)]
                    if (length(row_vals) >= 2) {
                        sd(row_vals) / mean(row_vals) * 100
                    } else {
                        NA
                    }
                })
            } else {
                cv_values <- numeric(0)
            }

            mean_cv <- if (length(cv_values) > 0) mean(as.numeric(cv_values), na.rm = TRUE) else NA
            if (is.nan(mean_cv)) mean_cv <- NA

            icc_value <- if (!is.null(repro_stats$icc_value)) repro_stats$icc_value else NA
            correlations <- if (!is.null(repro_stats$correlations)) repro_stats$correlations else NA
            mean_inter_biopsy <- if (!is.null(repro_stats$mean_inter_biopsy)) repro_stats$mean_inter_biopsy else NA

            return(list(
                n_cases = n_cases,
                n_biopsies = n_biopsies,
                overall_corr = overall_corr,
                mean_bias = mean_bias,
                bias_p = bias_p,
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

            status_text <- if (is.na(metrics$overall_corr) || is.na(metrics$mean_cv)) {
                "<p><strong>ℹ INSUFFICIENT DATA:</strong> Unable to evaluate sampling quality because correlation or variability estimates could not be computed.</p>"
            } else if (metrics$overall_corr >= correlation_threshold && metrics$mean_cv <= cv_threshold) {
                paste0("<p><strong>✓ ADEQUATE SAMPLING:</strong> Regional measurements provide good representation of ",
                       comparison_target, " (correlation ≥ ", correlation_threshold, ", CV ≤ ", cv_threshold, "%). ",
                       "<span style='color: green;'>Current sampling approach is suitable for clinical use.</span></p>")
            } else if (metrics$overall_corr >= (correlation_threshold - 0.2) && metrics$mean_cv <= (cv_threshold * 1.5)) {
                paste0("<p><strong>⚠ MODERATE SAMPLING:</strong> Regional measurements show moderate agreement with ",
                       comparison_target, " (thresholds: correlation ≥ ", correlation_threshold, ", CV ≤ ", cv_threshold, "%). ",
                       "<span style='color: orange;'>Consider additional samples or sampling optimization.</span></p>")
            } else {
                paste0("<p><strong>✗ INADEQUATE SAMPLING:</strong> Sampling does not meet quality thresholds ",
                       "(correlation ≥ ", correlation_threshold, ", CV ≤ ", cv_threshold, "%). ",
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
                "<div style='background-color: #f8f9fa; padding: 10px; border-left: 4px solid #007bff;'>",
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
                        paste0("Current sampling appears adequate (CV = ", round(metrics$mean_cv, 1), "% ≤ ", cv_threshold/2, "%)")
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
                    ifelse(metrics$overall_corr >= 0.80, "excellent",
                           ifelse(metrics$overall_corr >= 0.70, "good",
                                  ifelse(metrics$overall_corr >= 0.60, "moderate", "poor"))),
                    " correlation ", correlation_phrase, " (r = ", round(metrics$overall_corr, 3), "). "
                )
            } else {
                "Correlation metrics were not estimable with the available data. "
            }

            variability_sentence <- if (!is.na(metrics$mean_cv)) {
                paste0(
                    "Sampling variability was ",
                    ifelse(metrics$mean_cv <= 15, "low", ifelse(metrics$mean_cv <= 30, "moderate", "high")),
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
                        "No systematic bias was detected between regional and reference measurements. "
                    }
                } else {
                    "Bias testing could not be performed due to limited paired observations. "
                }
            } else {
                "Bias analysis was not applicable because no reference measurement was supplied. "
            }

            quality_status <- if (!is.na(metrics$overall_corr) && metrics$overall_corr >= correlation_threshold &&
                                   !is.na(metrics$mean_cv) && metrics$mean_cv <= cv_threshold) {
                "adequate for clinical use"
            } else if (!is.na(metrics$overall_corr) && metrics$overall_corr >= (correlation_threshold - 0.2) &&
                       !is.na(metrics$mean_cv) && metrics$mean_cv <= (cv_threshold * 1.5)) {
                "moderately adequate, with recommendations for optimization"
            } else if (is.na(metrics$overall_corr) || is.na(metrics$mean_cv)) {
                "unable to be evaluated due to insufficient data"
            } else {
                "inadequate and requires protocol revision"
            }

            quality_sentence <- switch(
                quality_status,
                "adequate for clinical use" = "Based on predefined quality criteria, the biopsy sampling approach was adequate for clinical use. ",
                "moderately adequate, with recommendations for optimization" = "Based on predefined quality criteria, the sampling approach was moderately adequate, with recommendations for optimization. ",
                "inadequate and requires protocol revision" = "Based on predefined quality criteria, the sampling approach was inadequate and requires protocol revision. ",
                "unable to be evaluated due to insufficient data" = "Data were insufficient to evaluate overall sampling quality against predefined criteria. ",
                ""
            )

            clinical_sentence <- if (!is.na(metrics$mean_cv)) {
                if (metrics$mean_cv <= cv_threshold/2) {
                    "current sampling protocols provide sufficient precision for routine clinical use"
                } else if (metrics$mean_cv <= cv_threshold) {
                    "sampling protocols may benefit from additional core biopsies to improve precision"
                } else {
                    "significant protocol modifications are recommended to achieve acceptable sampling precision"
                }
            } else {
                "additional data are required to assess sampling precision"
            }

            report_sentences <- paste0(
                "<h3>Copy-Ready Report Sentences</h3>",
                "<div style='background-color: #f8f9fa; padding: 15px; border: 1px solid #dee2e6; border-radius: 5px;'>",

                "<h4>Methods Section:</h4>",
                "<p style='font-family: monospace; background: white; padding: 10px; border-left: 4px solid #007bff;'>",
                "IHC heterogeneity analysis was performed on ", metrics$n_cases, " cases with ", metrics$n_biopsies,
                " simulated core biopsy measurements each, following the methodology of Zilenaite-Petrulaitiene et al. ",
                "Reproducibility was assessed using Spearman correlation and intraclass correlation coefficient (ICC). ",
                "Sampling variability was quantified using coefficient of variation (CV). ",
                "Quality thresholds were set at correlation ≥", correlation_threshold, " and CV ≤", cv_threshold, "%.",
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
                "These findings suggest that ", clinical_sentence,
                ". The results support the use of biopsy simulation methodology for quality assurance ",
                "in digital pathology workflows and biomarker assessment protocols.",
                "</p>",

                "</div>",

                "<p><strong>📋 Usage:</strong> Click and drag to select text, then copy (Ctrl+C/Cmd+C) for use in reports.</p>"
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


            self$results$interpretation$setContent(interpretation)
            self$results$report_sentences$setContent(report_sentences)
            self$results$assumptions$setContent(assumptions_content)

            # Generate plain-language summary if requested
            private$.generatePlainLanguageSummary(metrics)
        },

        .generateAssumptionsContent = function(metrics) {
            assumptions <- paste0(
                "<h3>Methodology & Assumptions</h3>",

                "<div style='margin: 15px 0;'>",
                "<h4>🔬 Analysis Methodology</h4>",
                "<div style='background-color: #e3f2fd; padding: 12px; border-radius: 5px;'>",
                "<ul>",
                "<li><strong>IHC Heterogeneity:</strong> Quantitative comparison of biomarker measurements from regional tissue areas</li>",
                "<li><strong>Statistical Framework:</strong> Reproducibility assessed using Spearman correlation and intraclass correlation coefficient (ICC)</li>",
                "<li><strong>Variability Metrics:</strong> Coefficient of variation (CV) calculated per case and averaged across the dataset</li>",
                "<li><strong>Reference Standard:</strong> Whole-section measurements serve as the gold standard for biomarker quantification</li>",
                "</ul>",
                "</div>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4>📋 Data Requirements & Assumptions</h4>",
                "<div style='background-color: #fff3e0; padding: 12px; border-radius: 5px;'>",
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
                "<h4>⚠️ Important Limitations</h4>",
                "<div style='background-color: #ffebee; padding: 12px; border-radius: 5px;'>",
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
                "<h4>🎯 Clinical Application Guidelines</h4>",
                "<div style='background-color: #f3e5f5; padding: 12px; border-radius: 5px;'>",
                "<ul>",
                "<li><strong>Quality Thresholds:</strong> Correlation ≥0.80 and CV ≤20% recommended for routine clinical use</li>",
                "<li><strong>Biomarker-Specific Adjustment:</strong> Thresholds may require adjustment for specific biomarkers</li>",
                "<li><strong>Protocol Validation:</strong> Results should inform but not replace empirical validation studies</li>",
                "<li><strong>Continuous Monitoring:</strong> Regular quality assessment recommended for clinical implementation</li>",
                "<li><strong>Multi-Center Studies:</strong> Additional validation needed for multi-institutional protocols</li>",
                "</ul>",
                "</div>",
                "</div>",

                "<div style='margin: 15px 0;'>",
                "<h4>📚 References & Standards</h4>",
                "<div style='background-color: #e8f5e8; padding: 12px; border-radius: 5px;'>",
                "<ul>",
                "<li><strong>Primary Methodology:</strong> Zilenaite-Petrulaitiene et al. (Am J Clin Pathol 2025)</li>",
                "<li><strong>ICC Guidelines:</strong> Koo & Li (J Chiropr Med 2016) - ICC interpretation standards</li>",
                "<li><strong>Biomarker Assessment:</strong> ASCO/CAP guidelines for immunohistochemical analysis</li>",
                "<li><strong>Quality Standards:</strong> ISO 15189 requirements for laboratory quality management</li>",
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

                "<div style='margin: 15px 0; padding: 15px; background-color: #f8f9ff; border-left: 4px solid #4a90e2; border-radius: 4px;'>",
                "<h4 style='color: #2c5282; margin-top: 0;'>📊 Correlation Measures</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>Spearman Correlation:</strong> Measures rank-order relationship between measurements. ",
                "Range: -1 to +1. Not affected by outliers or non-normal distributions. ",
                "Clinical meaning: >0.8 = strong relationship, 0.6-0.8 = moderate, <0.6 = weak.</li>",
                "<li><strong>Pearson Correlation:</strong> Measures linear relationship. Sensitive to outliers and requires normal distribution.</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0; padding: 15px; background-color: #fff8f0; border-left: 4px solid #ff8c42; border-radius: 4px;'>",
                "<h4 style='color: #b7410e; margin-top: 0;'>🎯 Reliability Measures</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>ICC (Intraclass Correlation):</strong> Measures agreement between measurements from same subjects. ",
                "ICC > 0.90 = excellent agreement, 0.75-0.90 = good, 0.50-0.75 = moderate, <0.50 = poor. ",
                "Clinical meaning: How well different measurements agree with each other.</li>",
                "<li><strong>Test-Retest Reliability:</strong> Consistency of measurements over time.</li>",
                "<li><strong>Inter-Rater Reliability:</strong> Agreement between different observers.</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0; padding: 15px; background-color: #f0fff4; border-left: 4px solid #48bb78; border-radius: 4px;'>",
                "<h4 style='color: #276749; margin-top: 0;'>📈 Variability Measures</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>CV (Coefficient of Variation):</strong> Standardized measure of variability = (SD/Mean) × 100%. ",
                "CV < 10% = low variability (excellent), 10-20% = moderate, 20-30% = high, >30% = very high. ",
                "Clinical meaning: How much measurements vary relative to their average.</li>",
                "<li><strong>Standard Deviation (SD):</strong> Average distance of measurements from the mean.</li>",
                "<li><strong>Variance:</strong> Square of standard deviation. Measures spread of data points.</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0; padding: 15px; background-color: #fefefe; border-left: 4px solid #805ad5; border-radius: 4px;'>",
                "<h4 style='color: #553c9a; margin-top: 0;'>⚗️ IHC-Specific Terms</h4>",
                "<ul style='margin: 10px 0; padding-left: 20px;'>",
                "<li><strong>Spatial Heterogeneity:</strong> Variation in biomarker expression across different tissue regions.</li>",
                "<li><strong>H-score:</strong> Immunohistochemical scoring method: (1×%weak) + (2×%moderate) + (3×%strong). Range: 0-300.</li>",
                "<li><strong>Proliferation Index:</strong> Percentage of cells showing positive staining (e.g., Ki67). Range: 0-100%.</li>",
                "<li><strong>Regional Sampling:</strong> Measuring biomarker expression from specific tissue areas.</li>",
                "</ul>",
                "</div>",

                "<div style='margin: 15px 0; padding: 15px; background-color: #fffaf0; border-left: 4px solid #ed8936; border-radius: 4px;'>",
                "<h4 style='color: #9c4221; margin-top: 0;'>📋 Clinical Interpretation Guidelines</h4>",
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
                    " (ICC = ", sprintf("%.2f", icc_value), ") - Regional measurements ", descriptor,
                    if (metrics$has_reference) " of the reference region." else " of one another.",
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

            clinical_sentence <- if (!is.na(icc_value)) {
                if (icc_value > 0.80) {
                    "Single regional measurements appear highly reliable for this biomarker. The low heterogeneity suggests consistent expression patterns across tissue regions."
                } else if (icc_value > 0.60) {
                    "Regional measurements show good reliability but some variability exists. Consider measuring multiple regions for critical diagnostic decisions."
                } else {
                    "Significant heterogeneity detected. Single regional measurements may not accurately represent overall biomarker status. Multiple region sampling recommended."
                }
            } else if (!is.na(mean_cv)) {
                if (mean_cv < 15) {
                    "Sampling variability is low, supporting confident interpretation of regional measurements."
                } else if (mean_cv < 30) {
                    "Moderate variability observed. Consider additional sampling for borderline clinical decisions."
                } else {
                    "High variability observed. Clinical protocols should incorporate additional regions or repeat measurements."
                }
            } else {
                "Data were insufficient to provide clinical guidance on sampling reliability."
            }

            summary_content <- paste0(
                "<div style='max-width: 700px; margin: 0 auto; padding: 20px; background-color: #f8f9fa; border-radius: 8px; font-family: Arial, sans-serif;'>",
                "<h3 style='color: #495057; margin-bottom: 15px; text-align: center;'>📊 Analysis Summary in Plain Language</h3>",

                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #007bff;'>",
                "<p style='margin: 0; font-size: 16px; line-height: 1.6;'>",
                "We analyzed <strong>", metrics$n_cases, " tissue samples</strong> to understand how well measurements from ",
                "<strong>", metrics$n_biopsies, " different regions</strong> represent biomarker expression.",
                "</p>",
                "</div>",

                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #28a745;'>",
                "<h4 style='color: #28a745; margin-top: 0;'>🎯 Key Findings:</h4>",
                "<ul style='margin: 5px 0; padding-left: 20px; line-height: 1.6;'>",
                agreement_sentence,
                variability_sentence,
                correlation_sentence,
                "</ul>",
                "</div>",

                "<div style='background-color: white; padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #ffc107;'>",
                "<h4 style='color: #856404; margin-top: 0;'>💡 Clinical Implications:</h4>",
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

            # Check if psych package is available
            if (!requireNamespace('psych', quietly = TRUE)) {
                if (n_biopsies >= 2) {
                    self$results$interpretation$setNote(
                        key = "psych_missing",
                        note = "Note: Install 'psych' package for enhanced reliability metrics (ICC).",
                        init = FALSE
                    )
                }
                return(list(
                    value = mean(correlations, na.rm = TRUE),
                    lower = NA,
                    upper = NA
                ))
            }

            # Need at least 2 measurements for ICC
            if (n_biopsies < 2) {
                return(list(
                    value = mean(correlations, na.rm = TRUE),
                    lower = NA,
                    upper = NA
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
                    upper = NA
                ))
            }

            # Check for sufficient variance
            col_vars <- apply(icc_data, 2, var, na.rm = TRUE)
            if (!all(col_vars > 1e-6)) {
                return(list(
                    value = mean(correlations, na.rm = TRUE),
                    lower = NA,
                    upper = NA
                ))
            }

            # Attempt ICC calculation
            tryCatch({
                icc_result <- psych::ICC(icc_data)
                # Use ICC(3,1) - consistency, single measurements
                icc_value <- icc_result$results$ICC[6]
                icc_lower <- icc_result$results$`lower bound`[6]
                icc_upper <- icc_result$results$`upper bound`[6]

                # Validate ICC result
                if (is.na(icc_value) || icc_value < -1 || icc_value > 1) {
                    icc_value <- mean(correlations, na.rm = TRUE)
                    icc_lower <- icc_upper <- NA
                }
            }, error = function(e) {
                # Fallback to mean correlation
                icc_value <<- mean(correlations, na.rm = TRUE)
                icc_lower <<- icc_upper <<- NA
            })

            return(list(
                value = icc_value,
                lower = icc_lower,
                upper = icc_upper
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
            for (i in 1:nrow(biopsy_data)) {
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
                warnings <- c(warnings, paste0("Very high biomarker values detected (max: ", round(max_value, 1), "). Verify if these are appropriate for your biomarker scale (e.g., percentages should be ≤100%, H-scores ≤300)."))
            }

            return(warnings)
        }

    )
)
