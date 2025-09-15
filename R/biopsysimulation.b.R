# This file is a generated template, your changes will not be overwritten

biopsysimulationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "biopsysimulationClass",
    inherit = biopsysimulationBase,
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
        .init = function() {
            if (is.null(self$data)) {
                self$results$interpretation$setContent(
                    "<h3>Biopsy Simulation Analysis for Digital Pathology</h3>
                    <p><strong>Purpose:</strong> Simulate the impact of limited tissue sampling (core biopsies) on 
                    biomarker measurements compared to whole-section analysis, following the methodology from 
                    peer-reviewed digital pathology validation studies.</p>
                    
                    <h4>Data Requirements:</h4>
                    <ul>
                        <li><strong>Whole Section Value:</strong> Biomarker measurement from entire tissue section</li>
                        <li><strong>Simulated Biopsy Values:</strong> Multiple measurements from simulated core biopsy samples</li>
                        <li><strong>Optional:</strong> Tissue coordinates, ROI identifiers, sampling parameters</li>
                    </ul>
                    
                    <h4>Simulation Framework:</h4>
                    <ul>
                        <li><strong>Sampling Variability:</strong> Quantify measurement variance due to limited sampling</li>
                        <li><strong>Representativeness:</strong> Assess how well biopsies represent whole-section values</li>
                        <li><strong>Clinical Impact:</strong> Effect on diagnostic threshold classification</li>
                        <li><strong>Optimization:</strong> Recommend minimum sampling requirements</li>
                    </ul>
                    
                    <h4>Statistical Analysis:</h4>
                    <ul>
                        <li><strong>Reproducibility:</strong> ICC and correlation analysis across samples</li>
                        <li><strong>Bias Assessment:</strong> Systematic differences between biopsy and whole-section</li>
                        <li><strong>Variability Decomposition:</strong> Within-case vs between-case variance</li>
                        <li><strong>Power Analysis:</strong> Sample size recommendations for future studies</li>
                    </ul>
                    
                    <h4>Clinical Applications:</h4>
                    <ul>
                        <li>Core needle biopsy adequacy assessment</li>
                        <li>Tumor heterogeneity quantification</li>
                        <li>Biomarker sampling optimization</li>
                        <li>Quality control for diagnostic workflows</li>
                    </ul>
                    
                    <p><em>This analysis implements the biopsy simulation methodology from 
                    Zilenaite-Petrulaitiene et al. (Am J Clin Pathol 2025) for Ki67 heterogeneity analysis.</em></p>"
                )
                return()
            }
            
            # Initialize results tables
            private$.initializeTables()
            
            # Set plot visibility based on options
            self$results$biopsyplot$setVisible(self$options$show_variability_plots)
            self$results$variabilityplot$setVisible(self$options$show_variability_plots)

            # Set conditional plot visibility for spatial analysis
            self$results$spatialplot$setVisible(
                self$options$show_variability_plots && !is.null(self$options$spatial_id)
            )

            # Set conditional table visibility for power analysis and variance components
            self$results$poweranalysistable$setVisible(self$options$power_analysis)
            self$results$variancetable$setVisible(self$options$variance_components)
            self$results$spatialanalysistable$setVisible(!is.null(self$options$spatial_id))
        },
        
        .run = function() {
            # Check required variables
            if (is.null(self$options$wholesection) || 
                (is.null(self$options$biopsy1) && is.null(self$options$biopsies))) {
                return()
            }
            
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Extract data
            whole_section <- data[[self$options$wholesection]]
            
            # Handle multiple biopsy columns
            biopsy_data <- private$.extractBiopsyData(data)
            
            if (length(whole_section) < 5 || nrow(biopsy_data) < 5) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for biopsy simulation analysis. 
                    At least 5 complete cases with whole-section and biopsy measurements are required.</p>"
                )
                return()
            }
            
            # Get optional spatial data
            spatial_regions <- if (!is.null(self$options$spatial_id)) {
                data[[self$options$spatial_id]]
            } else {
                NULL
            }
            
            # Perform comprehensive biopsy simulation analysis
            private$.performBiopsyAnalysis(whole_section, biopsy_data, spatial_regions)
            private$.generateBiopsyPlots(whole_section, biopsy_data, spatial_regions)
            private$.generateBiopsyInterpretation(whole_section, biopsy_data)
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
        
        .extractBiopsyData = function(data) {
            # Extract biopsy measurements from multiple columns efficiently
            # Collect individual biopsy columns
            individual_biopsies <- list(self$options$biopsy1, self$options$biopsy2,
                                       self$options$biopsy3, self$options$biopsy4)
            individual_biopsies <- individual_biopsies[!sapply(individual_biopsies, is.null)]

            # Combine with additional biopsy columns
            additional_cols <- if (!is.null(self$options$biopsies)) self$options$biopsies else c()
            biopsy_columns <- c(unlist(individual_biopsies), additional_cols)
            
            # Create biopsy data matrix
            biopsy_data <- data[, biopsy_columns, drop = FALSE]
            
            # Remove rows with all missing biopsy values
            complete_rows <- rowSums(!is.na(biopsy_data)) > 0
            biopsy_data <- biopsy_data[complete_rows, , drop = FALSE]
            
            return(biopsy_data)
        },
        
        .performBiopsyAnalysis = function(whole_section, biopsy_data, spatial_regions = NULL) {
            n_cases <- length(whole_section)
            n_biopsies <- ncol(biopsy_data)
            
            # Get user-defined thresholds
            cv_threshold <- self$options$cv_threshold
            correlation_threshold <- self$options$correlation_threshold
            
            # 1. Reproducibility Analysis
            private$.analyzeReproducibility(whole_section, biopsy_data, correlation_threshold, cv_threshold)
            
            # 2. Sampling Bias Analysis  
            private$.analyzeSamplingBias(whole_section, biopsy_data)
            
            # 3. Variance Component Analysis (if enabled)
            if (self$options$variance_components) {
                private$.analyzeVarianceComponents(whole_section, biopsy_data)
            }
            
            # 4. Power Analysis (if enabled)
            if (self$options$power_analysis) {
                private$.performPowerAnalysis(whole_section, biopsy_data)
            }
            
            # 5. Spatial Analysis (if spatial data provided)
            if (!is.null(spatial_regions)) {
                private$.analyzeSpatialHeterogeneity(whole_section, biopsy_data, spatial_regions)
            }
        },
        
        .analyzeReproducibility = function(whole_section, biopsy_data, correlation_threshold = 0.80, cv_threshold = 20.0) {
            n_cases <- length(whole_section)
            n_biopsies <- ncol(biopsy_data)
            
            # Calculate correlations between whole section and each biopsy
            correlations <- c()
            for (i in 1:n_biopsies) {
                biopsy_vals <- biopsy_data[, i]
                complete_pairs <- complete.cases(whole_section, biopsy_vals)
                if (sum(complete_pairs) >= 3) {
                    corr <- cor(whole_section[complete_pairs], biopsy_vals[complete_pairs], 
                               method = "spearman", use = "complete.obs")
                    correlations <- c(correlations, corr)
                }
            }
            
            # Calculate ICC for whole section vs biopsies
            # Prepare data for ICC calculation
            if (requireNamespace('psych', quietly = TRUE) && n_biopsies >= 2) {
                # Create matrix for ICC (cases x methods)
                icc_data <- cbind(whole_section, biopsy_data)
                complete_cases <- complete.cases(icc_data)
                icc_data <- icc_data[complete_cases, ]
                
                if (nrow(icc_data) >= 3) {
                    tryCatch({
                        icc_result <- psych::ICC(icc_data, type = "consistency")
                        icc_value <- icc_result$results$ICC[6]  # ICC(3,1)
                        icc_lower <- icc_result$results$`lower bound`[6]
                        icc_upper <- icc_result$results$`upper bound`[6]
                    }, error = function(e) {
                        warning("ICC calculation failed: ", e$message)
                        icc_value <<- icc_lower <<- icc_upper <<- NA
                    })
                } else {
                    icc_value <- icc_lower <- icc_upper <- NA
                }
            } else {
                icc_value <- icc_lower <- icc_upper <- NA
            }
            
            # Inter-biopsy reproducibility
            if (n_biopsies >= 2) {
                inter_biopsy_corr <- c()
                for (i in 1:(n_biopsies-1)) {
                    for (j in (i+1):n_biopsies) {
                        complete_pairs <- complete.cases(biopsy_data[, i], biopsy_data[, j])
                        if (sum(complete_pairs) >= 3) {
                            corr <- cor(biopsy_data[complete_pairs, i], biopsy_data[complete_pairs, j], 
                                       method = "spearman", use = "complete.obs")
                            inter_biopsy_corr <- c(inter_biopsy_corr, corr)
                        }
                    }
                }
                mean_inter_biopsy <- mean(inter_biopsy_corr, na.rm = TRUE)
            } else {
                mean_inter_biopsy <- NA
            }
            
            # Populate reproducibility table
            repro_table <- self$results$reproducibilitytable
            
            repro_table$addRow(rowKey = 1, values = list(
                metric = "Mean Biopsy-Whole Section Correlation",
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
                    metric = "Mean Inter-Biopsy Correlation",
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
        },
        
        .analyzeSamplingBias = function(whole_section, biopsy_data) {
            n_biopsies <- ncol(biopsy_data)
            bias_table <- self$results$samplingbiastable
            row_key <- 1
            
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
                        comparison = paste("Biopsy", i, "vs Whole Section"),
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
                        comparison = "Mean of Biopsies vs Whole Section",
                        mean_diff = overall_diff,
                        p_value = overall_p,
                        effect_size = overall_d,
                        clinical_impact = clinical_impact
                    ))
                }
            }
        },
        
        .analyzeVarianceComponents = function(whole_section, biopsy_data) {
            # Variance component analysis to understand sources of variability
            
            # Total variance across all measurements
            all_values <- c(whole_section, as.matrix(biopsy_data))
            total_variance <- var(all_values, na.rm = TRUE)
            
            # Between-case variance (using whole section values)
            between_case_var <- var(whole_section, na.rm = TRUE)
            
            # Within-case variance (sampling variability)
            within_case_vars <- c()
            for (i in 1:nrow(biopsy_data)) {
                case_values <- c(whole_section[i], as.numeric(biopsy_data[i, ]))
                case_values <- case_values[!is.na(case_values)]
                if (length(case_values) >= 2) {
                    within_case_vars <- c(within_case_vars, var(case_values))
                }
            }
            mean_within_case_var <- mean(within_case_vars, na.rm = TRUE)
            
            # Method variance (systematic differences between biopsy methods)
            method_means <- c(mean(whole_section, na.rm = TRUE), 
                             apply(biopsy_data, 2, mean, na.rm = TRUE))
            method_variance <- var(method_means, na.rm = TRUE)
            
            # Calculate percentages
            between_case_pct <- between_case_var / total_variance * 100
            within_case_pct <- mean_within_case_var / total_variance * 100
            method_pct <- method_variance / total_variance * 100
            
            # Populate variance table
            variance_table <- self$results$variancetable
            
            variance_table$addRow(rowKey = 1, values = list(
                component = "Between-Case Variance",
                variance = between_case_var,
                percentage = between_case_pct,
                contribution = ifelse(between_case_pct >= 60, "Major contributor", 
                                     ifelse(between_case_pct >= 30, "Moderate contributor", "Minor contributor"))
            ))
            
            variance_table$addRow(rowKey = 2, values = list(
                component = "Within-Case Variance (Sampling)",
                variance = mean_within_case_var,
                percentage = within_case_pct,
                contribution = ifelse(within_case_pct >= 30, "High sampling variability", 
                                     ifelse(within_case_pct >= 15, "Moderate sampling variability", "Low sampling variability"))
            ))
            
            variance_table$addRow(rowKey = 3, values = list(
                component = "Method Variance",
                variance = method_variance,
                percentage = method_pct,
                contribution = ifelse(method_pct >= 20, "Significant method differences", 
                                     ifelse(method_pct >= 10, "Minor method differences", "Negligible method differences"))
            ))
            
            variance_table$addRow(rowKey = 4, values = list(
                component = "Total Variance",
                variance = total_variance,
                percentage = 100,
                contribution = "Sum of all variance components"
            ))
        },
        
        .performPowerAnalysis = function(whole_section, biopsy_data) {
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
                
                for (i in seq_along(unique_regions)) {
                    region <- unique_regions[i]
                    region_mask <- spatial_regions == region & !is.na(spatial_regions)
                    
                    if (sum(region_mask) >= 2) {
                        region_whole_section <- whole_section[region_mask]
                        region_biopsy_data <- biopsy_data[region_mask, , drop = FALSE]
                        
                        # Calculate regional statistics
                        region_mean <- mean(region_whole_section, na.rm = TRUE)
                        region_values <- c(region_whole_section, as.matrix(region_biopsy_data))
                        region_cv <- sd(region_values, na.rm = TRUE) / mean(region_values, na.rm = TRUE) * 100
                        
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
        
        .generateBiopsyPlots = function(whole_section, biopsy_data, spatial_regions = NULL) {
            # Prepare comprehensive plot data
            plot_data <- list(
                whole_section = whole_section,
                biopsy_data = biopsy_data,
                spatial_regions = spatial_regions,
                n_cases = length(whole_section),
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
            
            # Create comparison plot (whole section vs biopsies)
            # Prepare data for plotting
            plot_df <- data.frame(
                Case = rep(1:data$n_cases, times = data$n_biopsies + 1),
                Method = rep(c("Whole Section", paste("Biopsy", 1:data$n_biopsies)), each = data$n_cases),
                Value = c(data$whole_section, as.matrix(data$biopsy_data))
            )
            
            # Remove missing values
            plot_df <- plot_df[!is.na(plot_df$Value), ]
            
            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Method, y = Value, color = Method)) +
                ggplot2::geom_boxplot(alpha = 0.7) +
                ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggplot2::labs(
                    title = "Biopsy vs Whole Section Comparison",
                    subtitle = "Distribution of biomarker values across sampling methods",
                    x = "Sampling Method",
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
        
        .calculateInterpretationMetrics = function(whole_section, biopsy_data) {
            n_cases <- length(whole_section)
            n_biopsies <- ncol(biopsy_data)

            # Calculate key metrics for interpretation
            biopsy_means <- rowMeans(biopsy_data, na.rm = TRUE)
            complete_pairs <- complete.cases(whole_section, biopsy_means)

            if (sum(complete_pairs) >= private$.CLINICAL_CONSTANTS$MIN_CASES_ICC) {
                overall_corr <- cor(whole_section[complete_pairs], biopsy_means[complete_pairs],
                                   method = "spearman")
                bias_test <- t.test(biopsy_means[complete_pairs], whole_section[complete_pairs], paired = TRUE)
                mean_bias <- bias_test$estimate
                bias_p <- bias_test$p.value
            } else {
                overall_corr <- mean_bias <- bias_p <- NA
            }

            # Calculate coefficient of variation (vectorized for better performance)
            calculate_case_cv <- function(whole_val, biopsy_row) {
                values <- c(whole_val, as.numeric(biopsy_row))
                values <- values[!is.na(values)]
                if (length(values) >= 2) {
                    sd(values) / mean(values) * 100
                } else {
                    NA
                }
            }

            cv_values <- mapply(calculate_case_cv,
                               whole_section,
                               split(biopsy_data, seq(nrow(biopsy_data))))
            mean_cv <- mean(cv_values, na.rm = TRUE)

            return(list(
                n_cases = n_cases,
                n_biopsies = n_biopsies,
                overall_corr = overall_corr,
                mean_bias = mean_bias,
                bias_p = bias_p,
                mean_cv = mean_cv
            ))
        },

        .formatClinicalAssessment = function(metrics, cv_threshold, correlation_threshold) {
            assessment <- paste0(
                "<h4>Key Findings:</h4>",
                "<ul>",
                if (!is.na(metrics$overall_corr)) {
                    paste0("<li><strong>Representativeness:</strong> Spearman correlation = ", round(metrics$overall_corr, 3),
                           " (", ifelse(metrics$overall_corr >= private$.CLINICAL_CONSTANTS$CORRELATION_GOOD, "Good",
                                       ifelse(metrics$overall_corr >= private$.CLINICAL_CONSTANTS$CORRELATION_POOR, "Moderate", "Poor")),
                           " representation of whole section)</li>")
                } else { "" },

                if (!is.na(metrics$mean_bias) && !is.na(metrics$bias_p)) {
                    paste0("<li><strong>Sampling Bias:</strong> Mean difference = ", round(metrics$mean_bias, 3),
                           " (", ifelse(metrics$bias_p < 0.05, "Statistically significant", "Not significant"), ")</li>")
                } else { "" },

                "<li><strong>Sampling Variability:</strong> Mean CV = ", round(metrics$mean_cv, 1), "% ",
                "(", ifelse(metrics$mean_cv <= private$.CLINICAL_CONSTANTS$CV_LOW_THRESHOLD, "Low variability",
                     ifelse(metrics$mean_cv <= private$.CLINICAL_CONSTANTS$CV_MODERATE_THRESHOLD, "Moderate variability", "High variability")), ")</li>",
                "</ul>",

                "<h4>Clinical Assessment:</h4>",
                "<div style='background-color: #f8f9fa; padding: 10px; border-left: 4px solid #007bff;'>",
                if (!is.na(metrics$overall_corr) && metrics$overall_corr >= correlation_threshold &&
                    !is.na(metrics$mean_cv) && metrics$mean_cv <= cv_threshold) {
                    paste0("<p><strong>✓ ADEQUATE SAMPLING:</strong> Biopsy samples provide good representation of whole-section values ",
                           "(correlation ≥ ", correlation_threshold, ", CV ≤ ", cv_threshold, "%). ",
                           "<span style='color: green;'>Current sampling approach is suitable for clinical use.</span></p>")
                } else if (!is.na(metrics$overall_corr) && metrics$overall_corr >= (correlation_threshold - 0.2) &&
                          !is.na(metrics$mean_cv) && metrics$mean_cv <= (cv_threshold * 1.5)) {
                    paste0("<p><strong>⚠ MODERATE SAMPLING:</strong> Biopsy samples show moderate agreement with whole-section values ",
                           "(thresholds: correlation ≥ ", correlation_threshold, ", CV ≤ ", cv_threshold, "%). ",
                           "<span style='color: orange;'>Consider additional samples or sampling optimization.</span></p>")
                } else {
                    paste0("<p><strong>✗ INADEQUATE SAMPLING:</strong> Sampling does not meet quality thresholds ",
                           "(correlation ≥ ", correlation_threshold, ", CV ≤ ", cv_threshold, "%). ",
                           "<span style='color: red;'>Review sampling strategy and consider increased sampling.</span></p>")
                },
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
            report_sentences <- paste0(
                "<h3>Copy-Ready Report Sentences</h3>",
                "<div style='background-color: #f8f9fa; padding: 15px; border: 1px solid #dee2e6; border-radius: 5px;'>",

                "<h4>Methods Section:</h4>",
                "<p style='font-family: monospace; background: white; padding: 10px; border-left: 4px solid #007bff;'>",
                "Biopsy simulation analysis was performed on ", metrics$n_cases, " cases with ", metrics$n_biopsies,
                " simulated core biopsy measurements each, following the methodology of Zilenaite-Petrulaitiene et al. ",
                "Reproducibility was assessed using Spearman correlation and intraclass correlation coefficient (ICC). ",
                "Sampling variability was quantified using coefficient of variation (CV). ",
                "Quality thresholds were set at correlation ≥", correlation_threshold, " and CV ≤", cv_threshold, "%.",
                "</p>",

                "<h4>Results Section:</h4>",
                "<p style='font-family: monospace; background: white; padding: 10px; border-left: 4px solid #28a745;'>",
                if (!is.na(metrics$overall_corr)) {
                    paste0("Biopsy samples showed ",
                           ifelse(metrics$overall_corr >= 0.80, "excellent",
                                  ifelse(metrics$overall_corr >= 0.70, "good",
                                         ifelse(metrics$overall_corr >= 0.60, "moderate", "poor"))),
                           " correlation with whole-section measurements (r = ", round(metrics$overall_corr, 3), "). ")
                } else { "" },

                "Sampling variability was ",
                ifelse(metrics$mean_cv <= 15, "low", ifelse(metrics$mean_cv <= 30, "moderate", "high")),
                " (mean CV = ", round(metrics$mean_cv, 1), "%). ",

                if (!is.na(metrics$bias_p)) {
                    if (metrics$bias_p < 0.05) {
                        paste0("Systematic bias was detected between biopsy and whole-section measurements (p = ",
                               ifelse(metrics$bias_p < 0.001, "<0.001", round(metrics$bias_p, 3)), "). ")
                    } else {
                        "No systematic bias was detected between biopsy and whole-section measurements. "
                    }
                } else { "" },

                "Based on predefined quality criteria, the biopsy sampling approach was ",
                if (!is.na(metrics$overall_corr) && metrics$overall_corr >= correlation_threshold &&
                    !is.na(metrics$mean_cv) && metrics$mean_cv <= cv_threshold) {
                    "adequate for clinical use"
                } else if (!is.na(metrics$overall_corr) && metrics$overall_corr >= (correlation_threshold - 0.2) &&
                          !is.na(metrics$mean_cv) && metrics$mean_cv <= (cv_threshold * 1.5)) {
                    "moderately adequate, with recommendations for optimization"
                } else {
                    "inadequate and requires protocol revision"
                }, ".",
                "</p>",

                "<h4>Clinical Interpretation:</h4>",
                "<p style='font-family: monospace; background: white; padding: 10px; border-left: 4px solid #ffc107;'>",
                "These findings suggest that ",
                if (metrics$mean_cv <= cv_threshold/2) {
                    "current sampling protocols provide sufficient precision for routine clinical use"
                } else if (metrics$mean_cv <= cv_threshold) {
                    "sampling protocols may benefit from additional core biopsies to improve precision"
                } else {
                    "significant protocol modifications are recommended to achieve acceptable sampling precision"
                },
                ". The results support the use of biopsy simulation methodology for quality assurance ",
                "in digital pathology workflows and biomarker assessment protocols.",
                "</p>",

                "</div>",

                "<p><strong>📋 Usage:</strong> Click and drag to select text, then copy (Ctrl+C/Cmd+C) for use in reports.</p>"
            )

            return(report_sentences)
        },

        .generateBiopsyInterpretation = function(whole_section, biopsy_data) {
            # Get user-defined thresholds
            cv_threshold <- self$options$cv_threshold
            correlation_threshold <- self$options$correlation_threshold

            # Calculate interpretation metrics
            metrics <- private$.calculateInterpretationMetrics(whole_section, biopsy_data)

            # Generate clinical assessment
            assessment <- private$.formatClinicalAssessment(metrics, cv_threshold, correlation_threshold)

            # Generate recommendations
            recommendations <- private$.generateRecommendations(metrics, cv_threshold, metrics$bias_p)

            # Generate report sentences
            report_sentences <- private$.generateReportSentences(metrics, cv_threshold, correlation_threshold)

            # Combine all sections into final interpretation
            interpretation <- paste0(
                "<h3>Biopsy Simulation Analysis Report</h3>",
                "<p><strong>Study Design:</strong> ", metrics$n_cases, " cases analyzed with ", metrics$n_biopsies, " simulated biopsy samples each</p>",
                assessment,
                recommendations
            )

            # Generate assumptions and methodology content
            assumptions_content <- private$.generateAssumptionsContent(metrics)


            self$results$interpretation$setContent(interpretation)
            self$results$report_sentences$setContent(report_sentences)
            self$results$assumptions$setContent(assumptions_content)
        },

        .generateAssumptionsContent = function(metrics) {
            assumptions <- paste0(
                "<h3>Methodology & Assumptions</h3>",

                "<div style='margin: 15px 0;'>",
                "<h4>🔬 Analysis Methodology</h4>",
                "<div style='background-color: #e3f2fd; padding: 12px; border-radius: 5px;'>",
                "<ul>",
                "<li><strong>Biopsy Simulation:</strong> Quantitative comparison of biomarker measurements from simulated core biopsies versus whole tissue sections</li>",
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
                "<li><strong>Sampling Independence:</strong> Biopsy samples assumed to be spatially independent within each case</li>",
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
        }

    )
)
