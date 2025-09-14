# This file is a generated template, your changes will not be overwritten

haralicktextureClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "haralicktextureClass",
    inherit = haralicktextureBase,
    private = list(
        .init = function() {
            if (is.null(self$data)) {
                # Initialize About section
                self$results$about$setContent(private$.generateAboutContent())
                
                # Initialize Summary section
                self$results$summary$setContent(
                    "<div class='summary-placeholder' style='background-color: #f8f9fa; padding: 15px; border: 1px dashed #dee2e6; border-radius: 5px;'>
                    <h4>📊 Analysis Summary</h4>
                    <p style='color: #6c757d; font-style: italic;'>Summary will appear here after running the analysis with your texture feature data.</p>
                    <p><small>This section will provide a copy-ready summary of key findings, statistical results, and clinical interpretation.</small></p>
                    </div>"
                )
                
                # Initialize interpretation section
                self$results$interpretation$setContent(
                    "<div style='background-color: #e3f2fd; padding: 15px; border-left: 4px solid #2196f3; margin: 10px 0;'>
                    <h4>🔬 Getting Started</h4>
                    <p>Select your Haralick texture features to begin the analysis. This function will provide comprehensive statistical analysis and clinical interpretation of spatial texture heterogeneity.</p>
                    <p><strong>Next steps:</strong></p>
                    <ol>
                    <li>Select texture feature columns (entropy, contrast, correlation, etc.)</li>
                    <li>Choose analysis focus and biomarker context if applicable</li>
                    <li>Add spatial coordinates for heterogeneity analysis (optional)</li>
                    <li>Run analysis to generate results and interpretation</li>
                    </ol>
                    </div>"
                )
                return()
            }
            
            # Initialize results tables
            private$.initializeTables()
        },
        
        .run = function() {
            # Check required variables
            if (is.null(self$options$texture_features) || length(self$options$texture_features) == 0) {
                self$results$interpretation$setContent(
                    "<div style='background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;'>
                    <h4>⚠️ No Texture Features Selected</h4>
                    <p>To perform Haralick texture analysis, you need to:</p>
                    <ol>
                    <li><strong>Select Texture Features</strong>: Drag texture feature columns (entropy, contrast, correlation, energy, homogeneity, etc.) from your data into the 'Haralick Texture Features' box</li>
                    <li><strong>Verify Your Data</strong>: Ensure your data contains pre-computed Haralick texture measurements</li>
                    <li><strong>Choose Analysis Focus</strong>: Select the type of analysis you want to perform</li>
                    </ol>
                    <p><strong>Need help getting texture features?</strong> Texture features must be computed from images using image analysis software before importing to jamovi.</p>
                    </div>"
                )
                return()
            }
            
            data <- self$data
            if (nrow(data) == 0) return()
            
            # Extract texture features with error handling
            texture_data <- tryCatch({
                private$.extractTextureData(data)
            }, error = function(e) {
                self$results$interpretation$setContent(
                    paste0("<div style='background-color: #f8d7da; padding: 15px; border-left: 4px solid #dc3545; margin: 10px 0;'>
                    <h4>❌ Data Validation Error</h4>
                    <p><strong>Problem:</strong> ", e$message, "</p>
                    <p><strong>Common solutions:</strong></p>
                    <ul>
                    <li>Check that your selected columns contain numeric texture values</li>
                    <li>Verify texture features are properly computed (entropy ≥0, correlation -1 to 1, etc.)</li>
                    <li>Remove any non-texture columns from the selection</li>
                    <li>Ensure sufficient data points (at least 5 cases)</li>
                    </ul>
                    </div>")
                )
                return(NULL)
            })
            
            if (is.null(texture_data)) return()
            
            if (nrow(texture_data) < 5) {
                self$results$interpretation$setContent(
                    "<p style='color: red;'><strong>Error:</strong> Insufficient data for texture analysis. 
                    At least 5 observations with complete texture measurements are required.</p>"
                )
                return()
            }
            
            # Set initial processing message
            self$results$summary$setContent(
                paste0("<div style='background-color: #d4edda; padding: 10px; border-left: 4px solid #28a745; margin: 10px 0;'>
                <p>🔄 <strong>Processing texture analysis...</strong></p>
                <p>Analyzing ", nrow(texture_data), " cases with ", length(self$options$texture_features), " texture features.</p>
                </div>")
            )
            
            # Clinical misuse detection and warnings
            warnings_and_recommendations <- private$.detectMisuse(texture_data)
            
            # Perform comprehensive texture analysis
            private$.performTextureAnalysis(texture_data)
            private$.generateTexturePlots(texture_data)
            private$.generateTextureInterpretation(texture_data)
            
            # Add clinical warnings to interpretation if needed
            if (length(warnings_and_recommendations) > 0) {
                private$.addClinicalWarnings(warnings_and_recommendations)
            }
        },
        
        .initializeTables = function() {
            # Texture feature statistics table
            desc_table <- self$results$texturetable
            desc_table$addColumn(name = 'feature', title = 'Texture Feature', type = 'text')
            desc_table$addColumn(name = 'n', title = 'N', type = 'integer')
            desc_table$addColumn(name = 'mean', title = 'Mean', type = 'number', format = 'zto')
            desc_table$addColumn(name = 'sd', title = 'SD', type = 'number', format = 'zto')
            desc_table$addColumn(name = 'median', title = 'Median', type = 'number', format = 'zto')
            desc_table$addColumn(name = 'iqr', title = 'IQR', type = 'text')
            desc_table$addColumn(name = 'range', title = 'Range', type = 'text')
            
            # Correlation matrix table
            corr_table <- self$results$correlationtable
            corr_table$addColumn(name = 'feature1', title = 'Feature 1', type = 'text')
            corr_table$addColumn(name = 'feature2', title = 'Feature 2', type = 'text')
            corr_table$addColumn(name = 'correlation', title = 'Correlation (r)', type = 'number', format = 'zto')
            corr_table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            corr_table$addColumn(name = 'interpretation', title = 'Strength', type = 'text')
            
            # Normality testing table
            dist_table <- self$results$normalitytable
            dist_table$addColumn(name = 'feature', title = 'Texture Feature', type = 'text')
            dist_table$addColumn(name = 'shapiro_w', title = 'Shapiro-Wilk W', type = 'number', format = 'zto')
            dist_table$addColumn(name = 'shapiro_p', title = 'P-value', type = 'number', format = 'zto,pvalue')
            dist_table$addColumn(name = 'skewness', title = 'Skewness', type = 'number', format = 'zto')
            dist_table$addColumn(name = 'kurtosis', title = 'Kurtosis', type = 'number', format = 'zto')
            dist_table$addColumn(name = 'distribution', title = 'Distribution Assessment', type = 'text')
            
            # Group comparison table
            group_table <- self$results$groupcomparisontable
            group_table$addColumn(name = 'feature', title = 'Texture Feature', type = 'text')
            group_table$addColumn(name = 'group1_n', title = 'Group 1 N', type = 'integer')
            group_table$addColumn(name = 'group1_mean', title = 'Group 1 Mean', type = 'number', format = 'zto')
            group_table$addColumn(name = 'group2_n', title = 'Group 2 N', type = 'integer')
            group_table$addColumn(name = 'group2_mean', title = 'Group 2 Mean', type = 'number', format = 'zto')
            group_table$addColumn(name = 'test_statistic', title = 'Test Statistic', type = 'number', format = 'zto')
            group_table$addColumn(name = 'p_value', title = 'P-value', type = 'number', format = 'zto,pvalue')
            group_table$addColumn(name = 'effect_size', title = 'Effect Size', type = 'number', format = 'zto')
            group_table$addColumn(name = 'interpretation', title = 'Interpretation', type = 'text')
            
            # Outlier detection table
            outlier_table <- self$results$outliertable
            outlier_table$addColumn(name = 'feature', title = 'Texture Feature', type = 'text')
            outlier_table$addColumn(name = 'n_outliers', title = 'N Outliers', type = 'integer')
            outlier_table$addColumn(name = 'outlier_rate', title = 'Outlier Rate (%)', type = 'number', format = 'zto')
            outlier_table$addColumn(name = 'method', title = 'Detection Method', type = 'text')
            outlier_table$addColumn(name = 'threshold', title = 'Threshold', type = 'text')
            outlier_table$addColumn(name = 'outlier_ids', title = 'Outlier Cases', type = 'text')
            
            # Missing data summary table
            missing_table <- self$results$missingdata
            missing_table$addColumn(name = 'feature', title = 'Feature', type = 'text')
            missing_table$addColumn(name = 'n_missing', title = 'N Missing', type = 'integer')
            missing_table$addColumn(name = 'missing_rate', title = 'Missing Rate (%)', type = 'number', format = 'zto')
            missing_table$addColumn(name = 'pattern', title = 'Missing Pattern', type = 'text')
            missing_table$addColumn(name = 'impact', title = 'Impact Assessment', type = 'text')
            
            # Variability assessment table
            var_table <- self$results$variability
            var_table$addColumn(name = 'feature', title = 'Feature', type = 'text')
            var_table$addColumn(name = 'cv', title = 'Coefficient of Variation', type = 'number', format = 'zto')
            var_table$addColumn(name = 'reliability', title = 'Reliability Score', type = 'number', format = 'zto')
            var_table$addColumn(name = 'assessment', title = 'Variability Assessment', type = 'text')
        },
        
        .extractTextureData = function(data) {
            # Extract selected texture features
            feature_names <- self$options$texture_features
            
            # Comprehensive validation
            validation_results <- private$.validateTextureFeatures(data, feature_names)
            
            if (length(validation_results$errors) > 0) {
                error_msg <- paste0(
                    "Texture feature validation failed:\n",
                    paste(names(validation_results$errors), ": ", validation_results$errors, collapse = "\n")
                )
                stop(error_msg)
            }
            
            if (length(validation_results$warnings) > 0) {
                warning_msg <- paste0(
                    "Texture feature warnings:\n",
                    paste(names(validation_results$warnings), ": ", validation_results$warnings, collapse = "\n")
                )
                warning(warning_msg)
            }
            
            valid_features <- validation_results$valid_features
            
            # Apply feature selection if specified
            selected_features <- private$.applyFeatureSelection(data, valid_features)
            
            # Extract texture data
            texture_data <- data[, selected_features, drop = FALSE]
            
            # Remove rows with all missing values
            complete_rows <- rowSums(!is.na(texture_data)) > 0
            texture_data <- texture_data[complete_rows, , drop = FALSE]
            
            # Add additional variables if specified
            result_data <- texture_data
            
            # Add spatial coordinates if provided
            if (!is.null(self$options$x_coord) && self$options$x_coord %in% names(data)) {
                result_data$x_coord <- data[[self$options$x_coord]][complete_rows]
            }
            if (!is.null(self$options$y_coord) && self$options$y_coord %in% names(data)) {
                result_data$y_coord <- data[[self$options$y_coord]][complete_rows]
            }
            
            # Add grouping variable if provided
            if (!is.null(self$options$group_var) && self$options$group_var %in% names(data)) {
                result_data$group <- data[[self$options$group_var]][complete_rows]
            }
            
            # Add outcome variable if provided (for prognostic analysis)
            if (!is.null(self$options$outcome_var) && self$options$outcome_var %in% names(data)) {
                result_data$outcome <- data[[self$options$outcome_var]][complete_rows]
            }
            
            return(result_data)
        },
        
        .performTextureAnalysis = function(texture_data) {
            # Get texture feature columns only
            texture_features <- self$options$texture_features
            texture_cols <- intersect(texture_features, names(texture_data))
            texture_matrix <- texture_data[, texture_cols, drop = FALSE]
            
            # Analysis focus determines which analyses to run
            analysis_focus <- self$options$analysis_focus
            
            # Core analyses that run for all focus types
            if (analysis_focus %in% c("descriptive", "comprehensive")) {
                # 1. Descriptive Statistics Analysis  
                private$.analyzeDescriptiveStats(texture_matrix, texture_cols)
                
                # 3. Distribution Analysis
                if (self$options$normality_testing) {
                    private$.analyzeDistributions(texture_matrix, texture_cols)
                }
            }
            
            if (analysis_focus %in% c("correlation", "comprehensive")) {
                # 2. Correlation Analysis
                private$.analyzeCorrelations(texture_matrix, texture_cols)
            }
            
            if (analysis_focus %in% c("spatial", "comprehensive")) {
                # Spatial heterogeneity analysis
                if (!is.null(self$options$x_coord) && !is.null(self$options$y_coord)) {
                    private$.analyzeSpatialHeterogeneity(texture_data, texture_cols)
                }
            }
            
            if (analysis_focus %in% c("prognostic", "comprehensive")) {
                # 4. Group Comparison Analysis (prognostic)
                if (!is.null(self$options$group_var) && self$options$group_var %in% names(texture_data)) {
                    texture_matrix_with_group <- texture_data[, c(texture_cols, self$options$group_var), drop = FALSE]
                    names(texture_matrix_with_group)[names(texture_matrix_with_group) == self$options$group_var] <- "group"
                    private$.analyzeGroupComparisons(texture_matrix_with_group, texture_cols)
                }
                
                # Prognostic modeling
                if (!is.null(self$options$outcome_var) && self$options$outcome_var %in% names(texture_data)) {
                    private$.analyzePrognosticValue(texture_data, texture_cols)
                }
            }
            
            # Quality control analyses (always run)
            if (analysis_focus == "comprehensive") {
                # 5. Outlier Detection
                if (self$options$outlier_detection) {
                    private$.analyzeOutliers(texture_matrix, texture_cols)
                }
                
                # 6. Missing Data and Variability Assessment
                private$.analyzeMissingData(texture_matrix, texture_cols)
                private$.analyzeVariability(texture_matrix, texture_cols)
            }
        },
        
        .generateAboutContent = function() {
            return(paste0(
                "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;'>",
                "<h3>🔬 About Haralick Texture Analysis</h3>",
                
                "<h4>What This Analysis Does:</h4>",
                "<p>Haralick texture analysis quantifies spatial patterns in digital pathology images using ",
                "<strong>Gray-Level Co-occurrence Matrices (GLCMs)</strong>. It measures how pixel intensities ",
                "are spatially distributed, providing objective metrics for tissue heterogeneity.</p>",
                
                "<h4>When to Use This Analysis:</h4>",
                "<ul style='margin-left: 20px;'>",
                "<li><strong>Biomarker Assessment:</strong> Quantify spatial heterogeneity in Ki67, HER2, PD-L1, or other biomarkers</li>",
                "<li><strong>Prognostic Studies:</strong> Correlate texture patterns with clinical outcomes</li>",
                "<li><strong>Tumor Grading:</strong> Objective assessment of tissue architecture complexity</li>",
                "<li><strong>Treatment Response:</strong> Monitor changes in tissue organization patterns</li>",
                "<li><strong>Quality Control:</strong> Validate digital pathology algorithms and measurements</li>",
                "</ul>",
                
                "<h4>Key Haralick Features Explained:</h4>",
                "<div style='margin-left: 20px;'>",
                "<p><strong>🎯 Entropy:</strong> Measures randomness/disorder. High entropy = heterogeneous tissue patterns</p>",
                "<p><strong>⚡ Contrast:</strong> Measures local intensity variations. High contrast = sharp edges/boundaries</p>",
                "<p><strong>🔗 Correlation:</strong> Measures linear dependency of intensities. High correlation = organized patterns</p>",
                "<p><strong>💡 Energy/ASM:</strong> Measures uniformity. High energy = homogeneous regions</p>",
                "<p><strong>🏠 Homogeneity:</strong> Measures closeness to diagonal. High homogeneity = similar intensities</p>",
                "</div>",
                
                "<h4>Data Requirements:</h4>",
                "<ul style='margin-left: 20px;'>",
                "<li><strong>Required:</strong> Pre-computed Haralick texture measurements (numeric columns)</li>",
                "<li><strong>Optional:</strong> Spatial coordinates (X, Y) for heterogeneity mapping</li>",
                "<li><strong>Optional:</strong> Grouping variables (tumor grade, treatment, etc.)</li>",
                "<li><strong>Optional:</strong> Clinical outcomes for prognostic analysis</li>",
                "</ul>",
                
                "<h4>How to Interpret Results:</h4>",
                "<div style='background-color: #e8f5e8; padding: 10px; border-left: 3px solid #4caf50; margin: 10px 0;'>",
                "<p><strong>✅ High Heterogeneity (CV > 1.0):</strong></p>",
                "<ul><li>Complex tissue architecture</li><li>May indicate aggressive biology</li><li>Requires comprehensive sampling</li></ul>",
                "</div>",
                
                "<div style='background-color: #fff3cd; padding: 10px; border-left: 3px solid #ffc107; margin: 10px 0;'>",
                "<p><strong>⚠️ Moderate Heterogeneity (CV 0.5-1.0):</strong></p>",
                "<ul><li>Mixed tissue patterns</li><li>Standard for most biomarkers</li><li>Good for prognostic analysis</li></ul>",
                "</div>",
                
                "<div style='background-color: #f8d7da; padding: 10px; border-left: 3px solid #dc3545; margin: 10px 0;'>",
                "<p><strong>⚪ Low Heterogeneity (CV < 0.5):</strong></p>",
                "<ul><li>Uniform tissue patterns</li><li>May need different features</li><li>Consider sampling adequacy</li></ul>",
                "</div>",
                
                "<h4>Clinical Applications by Biomarker:</h4>",
                "<p><strong>Ki67:</strong> Entropy >2.5 suggests aggressive proliferation patterns</p>",
                "<p><strong>HER2:</strong> High heterogeneity may indicate variable therapeutic response</p>",
                "<p><strong>PD-L1:</strong> Spatial patterns affect immunotherapy prediction accuracy</p>",
                "<p><strong>CD8+:</strong> Distribution patterns reveal immune landscape organization</p>",
                
                "<h4>Quality Control Checklist:</h4>",
                "<ul style='margin-left: 20px;'>",
                "<li>✓ Verify texture features are from same image processing pipeline</li>",
                "<li>✓ Check for extreme outliers that may indicate processing errors</li>",
                "<li>✓ Ensure adequate sampling (>5 regions per case recommended)</li>",
                "<li>✓ Validate against histopathologist assessment when possible</li>",
                "</ul>",
                
                "<p><em>This implementation follows validated Haralick methodology from Haralick et al. (1973) ",
                "and recent digital pathology applications (Zilenaite-Petrulaitiene et al., 2025).</em></p>",
                "</div>"
            ))
        },
        
        .validateTextureFeatures = function(data, features) {
            validation_results <- list(
                valid_features = c(),
                errors = c(),
                warnings = c()
            )
            
            if (length(features) == 0) {
                validation_results$errors["no_features"] <- "No texture features selected. Please select at least one Haralick texture feature."
                return(validation_results)
            }
            
            missing_features <- setdiff(features, names(data))
            if (length(missing_features) > 0) {
                validation_results$errors["missing_columns"] <- paste0(
                    "Missing columns: ", paste(missing_features, collapse=", "), 
                    ". Please check that your data contains these texture feature columns."
                )
            }
            
            # Check existing features
            existing_features <- intersect(features, names(data))
            
            for (feature in existing_features) {
                feature_data <- data[[feature]]
                
                # Check if numeric
                if (!is.numeric(feature_data)) {
                    validation_results$errors[feature] <- "Non-numeric data type. Texture features must be numeric values."
                    next
                }
                
                # Remove missing values for validation
                clean_data <- feature_data[!is.na(feature_data)]
                
                if (length(clean_data) == 0) {
                    validation_results$errors[feature] <- "All values are missing."
                    next
                }
                
                if (length(clean_data) < 5) {
                    validation_results$warnings[feature] <- paste0(
                        "Only ", length(clean_data), " non-missing values. Recommend at least 5 cases for reliable analysis."
                    )
                }
                
                # Texture feature specific validations
                feature_lower <- tolower(feature)
                
                # Entropy should be non-negative
                if (grepl("entropy", feature_lower) && any(clean_data < 0)) {
                    validation_results$errors[feature] <- "Entropy values must be non-negative (≥0)."
                    next
                }
                
                # Correlation features should be between -1 and 1
                if (grepl("correlation|corr", feature_lower) && (any(clean_data < -1) || any(clean_data > 1))) {
                    validation_results$errors[feature] <- "Correlation values must be between -1 and 1."
                    next
                }
                
                # Energy/ASM should be between 0 and 1
                if (grepl("energy|asm|uniformity", feature_lower) && (any(clean_data < 0) || any(clean_data > 1))) {
                    validation_results$warnings[feature] <- "Energy/ASM values typically range from 0 to 1."
                }
                
                # Contrast should be non-negative
                if (grepl("contrast", feature_lower) && any(clean_data < 0)) {
                    validation_results$errors[feature] <- "Contrast values must be non-negative (≥0)."
                    next
                }
                
                # Homogeneity should be between 0 and 1
                if (grepl("homogeneity|idm", feature_lower) && (any(clean_data < 0) || any(clean_data > 1))) {
                    validation_results$warnings[feature] <- "Homogeneity values typically range from 0 to 1."
                }
                
                # Check for suspicious constant values
                if (length(unique(clean_data)) == 1) {
                    validation_results$warnings[feature] <- "All values are identical. This feature provides no information for analysis."
                }
                
                # Check for extreme outliers (beyond 5 IQR)
                if (length(clean_data) > 4) {
                    q1 <- quantile(clean_data, 0.25)
                    q3 <- quantile(clean_data, 0.75)
                    iqr <- q3 - q1
                    extreme_outliers <- sum(clean_data < (q1 - 5*iqr) | clean_data > (q3 + 5*iqr))
                    
                    if (extreme_outliers > 0) {
                        validation_results$warnings[feature] <- paste0(
                            extreme_outliers, " extreme outlier(s) detected. Consider reviewing data quality."
                        )
                    }
                }
                
                # If all validations pass, add to valid features
                validation_results$valid_features <- c(validation_results$valid_features, feature)
            }
            
            return(validation_results)
        },
        
        .applyFeatureSelection = function(data, features) {
            feature_selection <- self$options$feature_selection
            
            if (feature_selection == "all" || length(features) <= 2) {
                return(features)
            }
            
            # Create feature matrix for analysis
            feature_matrix <- data[, features, drop = FALSE]
            feature_matrix <- feature_matrix[complete.cases(feature_matrix), , drop = FALSE]
            
            if (nrow(feature_matrix) < 5) {
                warning("Insufficient complete cases for feature selection. Using all features.")
                return(features)
            }
            
            selected_features <- features
            
            if (feature_selection == "correlation_filter") {
                # Remove highly correlated features
                cor_matrix <- cor(feature_matrix, use = "complete.obs")
                threshold <- self$options$correlation_threshold
                
                # Find highly correlated pairs
                high_cor_pairs <- which(abs(cor_matrix) > threshold & cor_matrix != 1, arr.ind = TRUE)
                
                if (nrow(high_cor_pairs) > 0) {
                    # Remove features with highest average correlation
                    features_to_remove <- c()
                    for (i in 1:nrow(high_cor_pairs)) {
                        feature1 <- features[high_cor_pairs[i, 1]]
                        feature2 <- features[high_cor_pairs[i, 2]]
                        
                        # Calculate average correlation for each feature
                        avg_cor1 <- mean(abs(cor_matrix[high_cor_pairs[i, 1], -high_cor_pairs[i, 1]]), na.rm = TRUE)
                        avg_cor2 <- mean(abs(cor_matrix[high_cor_pairs[i, 2], -high_cor_pairs[i, 2]]), na.rm = TRUE)
                        
                        # Remove the feature with higher average correlation
                        if (avg_cor1 > avg_cor2) {
                            features_to_remove <- c(features_to_remove, feature1)
                        } else {
                            features_to_remove <- c(features_to_remove, feature2)
                        }
                    }
                    
                    selected_features <- setdiff(features, unique(features_to_remove))
                    
                    if (length(selected_features) < 2) {
                        warning("Correlation filtering removed too many features. Using original selection.")
                        selected_features <- features
                    }
                }
                
            } else if (feature_selection == "variance_filter") {
                # Remove low variance features
                feature_vars <- apply(feature_matrix, 2, var, na.rm = TRUE)
                
                # Calculate variance threshold (bottom 25th percentile)
                var_threshold <- quantile(feature_vars, 0.25, na.rm = TRUE)
                
                # Keep features with variance above threshold
                high_var_features <- names(feature_vars)[feature_vars > var_threshold]
                selected_features <- intersect(features, high_var_features)
                
                if (length(selected_features) < 2) {
                    warning("Variance filtering removed too many features. Using original selection.")
                    selected_features <- features
                }
                
            } else if (feature_selection == "clinical_priority") {
                # Prioritize clinically important texture features
                clinical_priority <- c()
                
                # High priority features (most clinically relevant)
                for (feature in features) {
                    feature_lower <- tolower(feature)
                    if (grepl("entropy", feature_lower)) {
                        clinical_priority <- c(clinical_priority, feature)
                    }
                }
                
                # Medium priority features
                for (feature in features) {
                    feature_lower <- tolower(feature)
                    if (grepl("contrast|correlation|homogeneity", feature_lower)) {
                        clinical_priority <- c(clinical_priority, feature)
                    }
                }
                
                # Lower priority features
                for (feature in features) {
                    feature_lower <- tolower(feature)
                    if (grepl("energy|asm|variance|mean|dissimilarity", feature_lower)) {
                        clinical_priority <- c(clinical_priority, feature)
                    }
                }
                
                # Add any remaining features
                clinical_priority <- c(clinical_priority, setdiff(features, clinical_priority))
                
                # Take top features based on priority (limit to reasonable number)
                max_features <- min(length(features), 10)  # Limit to 10 most important
                selected_features <- clinical_priority[1:min(max_features, length(clinical_priority))]
            }
            
            # Ensure we have at least 2 features for meaningful analysis
            if (length(selected_features) < 2) {
                warning("Feature selection resulted in too few features. Using all available features.")
                selected_features <- features
            }
            
            return(selected_features)
        },
        
        .analyzeDescriptiveStats = function(texture_matrix, texture_cols) {
            desc_table <- self$results$texturetable
            
            for (i in seq_along(texture_cols)) {
                feature_name <- texture_cols[i]
                feature_data <- texture_matrix[[feature_name]]
                feature_data <- feature_data[!is.na(feature_data)]
                
                if (length(feature_data) >= 3) {
                    n <- length(feature_data)
                    mean_val <- mean(feature_data)
                    sd_val <- sd(feature_data)
                    median_val <- median(feature_data)
                    q1 <- quantile(feature_data, 0.25)
                    q3 <- quantile(feature_data, 0.75)
                    min_val <- min(feature_data)
                    max_val <- max(feature_data)
                    
                    iqr_text <- paste0(round(q1, 3), " - ", round(q3, 3))
                    range_text <- paste0(round(min_val, 3), " - ", round(max_val, 3))
                    
                    desc_table$addRow(rowKey = i, values = list(
                        feature = feature_name,
                        n = n,
                        mean = mean_val,
                        sd = sd_val,
                        median = median_val,
                        iqr = iqr_text,
                        range = range_text
                    ))
                }
            }
        },
        
        .analyzeCorrelations = function(texture_matrix, texture_cols) {
            if (length(texture_cols) < 2) return()
            
            corr_table <- self$results$correlationtable
            row_key <- 1
            
            # Pre-compute correlation matrix for efficiency
            complete_data <- texture_matrix[complete.cases(texture_matrix), , drop = FALSE]
            if (nrow(complete_data) < 3) return()
            
            # Compute correlation matrix and p-values efficiently
            cor_matrix <- cor(complete_data, use = "complete.obs", method = "pearson")
            
            # Calculate p-values for significant correlations
            n <- nrow(complete_data)
            t_stats <- cor_matrix * sqrt((n - 2) / (1 - cor_matrix^2))
            p_matrix <- 2 * pt(-abs(t_stats), df = n - 2)
            
            # Populate correlation table with upper triangular matrix
            for (i in 1:(length(texture_cols)-1)) {
                for (j in (i+1):length(texture_cols)) {
                    feature1 <- texture_cols[i]
                    feature2 <- texture_cols[j]
                    
                    r_value <- cor_matrix[i, j]
                    p_value <- p_matrix[i, j]
                    
                    # Interpret correlation strength
                    strength <- if (abs(r_value) >= 0.7) {
                        paste0("Strong ", if(r_value > 0) "positive" else "negative")
                    } else if (abs(r_value) >= 0.5) {
                        paste0("Moderate ", if(r_value > 0) "positive" else "negative") 
                    } else if (abs(r_value) >= 0.3) {
                        paste0("Weak ", if(r_value > 0) "positive" else "negative")
                    } else {
                        "Very weak"
                    }
                    
                    corr_table$addRow(rowKey = row_key, values = list(
                        feature1 = feature1,
                        feature2 = feature2,
                        correlation = r_value,
                        p_value = p_value,
                        interpretation = strength
                    ))
                    
                    row_key <- row_key + 1
                }
            }
        },
        
        .analyzeDistributions = function(texture_matrix, texture_cols) {
            dist_table <- self$results$normalitytable
            
            for (i in seq_along(texture_cols)) {
                feature_name <- texture_cols[i]
                feature_data <- texture_matrix[[feature_name]]
                feature_data <- feature_data[!is.na(feature_data)]
                
                if (length(feature_data) >= 3) {
                    # Shapiro-Wilk test for normality
                    if (length(feature_data) <= 5000) {  # Shapiro-Wilk limitation
                        shapiro_test <- shapiro.test(feature_data)
                        shapiro_w <- shapiro_test$statistic
                        shapiro_p <- shapiro_test$p.value
                    } else {
                        shapiro_w <- NA
                        shapiro_p <- NA
                    }
                    
                    # Calculate skewness and kurtosis
                    if (requireNamespace('moments', quietly = TRUE)) {
                        skew_val <- moments::skewness(feature_data)
                        kurt_val <- moments::kurtosis(feature_data)
                    } else {
                        # Simple skewness calculation
                        n <- length(feature_data)
                        mean_val <- mean(feature_data)
                        sd_val <- sd(feature_data)
                        skew_val <- sum((feature_data - mean_val)^3) / ((n-1) * sd_val^3)
                        kurt_val <- sum((feature_data - mean_val)^4) / ((n-1) * sd_val^4)
                    }
                    
                    # Distribution assessment
                    dist_assessment <- "Unknown"
                    if (!is.na(shapiro_p)) {
                        if (shapiro_p >= 0.05) {
                            if (abs(skew_val) <= 0.5 && abs(kurt_val - 3) <= 0.5) {
                                dist_assessment <- "Normal distribution"
                            } else {
                                dist_assessment <- "Non-normal (symmetric)"
                            }
                        } else {
                            if (skew_val > 1) {
                                dist_assessment <- "Right-skewed"
                            } else if (skew_val < -1) {
                                dist_assessment <- "Left-skewed"
                            } else {
                                dist_assessment <- "Non-normal"
                            }
                        }
                    }
                    
                    dist_table$addRow(rowKey = i, values = list(
                        feature = feature_name,
                        shapiro_w = shapiro_w,
                        shapiro_p = shapiro_p,
                        skewness = skew_val,
                        kurtosis = kurt_val,
                        distribution = dist_assessment
                    ))
                }
            }
        },
        
        .analyzeGroupComparisons = function(texture_matrix, texture_cols) {
            group_table <- self$results$groupcomparisontable
            group_col <- self$options$group_var
            
            if (!group_col %in% names(texture_matrix)) return()
            
            group_levels <- unique(texture_matrix[[group_col]])
            if (length(group_levels) != 2) return()  # Only handle two groups for now
            
            for (feature in texture_cols) {
                if (!feature %in% names(texture_matrix)) next
                
                group1_data <- texture_matrix[texture_matrix[[group_col]] == group_levels[1], feature]
                group2_data <- texture_matrix[texture_matrix[[group_col]] == group_levels[2], feature]
                
                group1_data <- group1_data[!is.na(group1_data)]
                group2_data <- group2_data[!is.na(group2_data)]
                
                if (length(group1_data) < 3 || length(group2_data) < 3) next
                
                # Perform t-test
                test_result <- t.test(group1_data, group2_data)
                
                # Calculate effect size (Cohen's d)
                pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                                   (length(group2_data) - 1) * var(group2_data)) / 
                                   (length(group1_data) + length(group2_data) - 2))
                cohens_d <- (mean(group1_data) - mean(group2_data)) / pooled_sd
                
                # Interpretation
                interpretation <- "No difference"
                if (test_result$p.value < 0.05) {
                    if (abs(cohens_d) > 0.8) {
                        interpretation <- "Large significant difference"
                    } else if (abs(cohens_d) > 0.5) {
                        interpretation <- "Medium significant difference"
                    } else {
                        interpretation <- "Small significant difference"
                    }
                }
                
                group_table$addRow(rowKey = feature, list(
                    feature = feature,
                    group1_n = length(group1_data),
                    group1_mean = mean(group1_data),
                    group2_n = length(group2_data),
                    group2_mean = mean(group2_data),
                    test_statistic = test_result$statistic,
                    p_value = test_result$p.value,
                    effect_size = cohens_d,
                    interpretation = interpretation
                ))
            }
        },
        
        .analyzeOutliers = function(texture_matrix, texture_cols) {
            outlier_table <- self$results$outliertable
            method <- self$options$outlier_method
            
            for (feature in texture_cols) {
                if (!feature %in% names(texture_matrix)) next
                
                data <- texture_matrix[[feature]]
                data <- data[!is.na(data)]
                if (length(data) < 5) next
                
                outliers <- c()
                threshold_text <- ""
                
                if (method == "iqr") {
                    q1 <- quantile(data, 0.25)
                    q3 <- quantile(data, 0.75)
                    iqr <- q3 - q1
                    lower <- q1 - 1.5 * iqr
                    upper <- q3 + 1.5 * iqr
                    outliers <- which(data < lower | data > upper)
                    threshold_text <- paste0("Q1-1.5×IQR to Q3+1.5×IQR (", round(lower, 3), " to ", round(upper, 3), ")")
                } else if (method == "zscore") {
                    mean_val <- mean(data)
                    sd_val <- sd(data)
                    z_scores <- abs((data - mean_val) / sd_val)
                    outliers <- which(z_scores > 3)
                    threshold_text <- "±3 standard deviations"
                }
                
                outlier_rate <- length(outliers) / length(data) * 100
                outlier_ids <- if (length(outliers) > 0) {
                    if (length(outliers) <= 10) {
                        paste(outliers, collapse = ", ")
                    } else {
                        paste0(paste(outliers[1:10], collapse = ", "), ", ...")
                    }
                } else { "None" }
                
                outlier_table$addRow(rowKey = feature, list(
                    feature = feature,
                    n_outliers = length(outliers),
                    outlier_rate = outlier_rate,
                    method = method,
                    threshold = threshold_text,
                    outlier_ids = outlier_ids
                ))
            }
        },
        
        .analyzeMissingData = function(texture_matrix, texture_cols) {
            missing_table <- self$results$missingdata
            
            for (feature in texture_cols) {
                if (!feature %in% names(texture_matrix)) {
                    missing_table$addRow(rowKey = feature, list(
                        feature = feature,
                        n_missing = nrow(texture_matrix),
                        missing_rate = 100,
                        pattern = "Complete missingness",
                        impact = "High - feature not available"
                    ))
                    next
                }
                
                data <- texture_matrix[[feature]]
                n_missing <- sum(is.na(data))
                missing_rate <- n_missing / length(data) * 100
                
                pattern <- if (missing_rate == 0) {
                    "Complete data"
                } else if (missing_rate < 5) {
                    "Minimal missing"
                } else if (missing_rate < 20) {
                    "Moderate missing"
                } else {
                    "Substantial missing"
                }
                
                impact <- if (missing_rate == 0) {
                    "None - complete data"
                } else if (missing_rate < 5) {
                    "Low - minimal impact"
                } else if (missing_rate < 20) {
                    "Medium - consider imputation"
                } else {
                    "High - may affect analysis validity"
                }
                
                missing_table$addRow(rowKey = feature, list(
                    feature = feature,
                    n_missing = n_missing,
                    missing_rate = missing_rate,
                    pattern = pattern,
                    impact = impact
                ))
            }
        },
        
        .analyzeVariability = function(texture_matrix, texture_cols) {
            var_table <- self$results$variability
            
            for (feature in texture_cols) {
                if (!feature %in% names(texture_matrix)) next
                
                data <- texture_matrix[[feature]]
                data <- data[!is.na(data)]
                if (length(data) < 3) next
                
                mean_val <- mean(data)
                sd_val <- sd(data)
                cv <- abs(sd_val / mean_val)
                
                # Simple reliability estimate (inverse CV)
                reliability <- 1 / (1 + cv)
                
                assessment <- if (cv < 0.1) {
                    "Very stable - excellent reproducibility"
                } else if (cv < 0.3) {
                    "Stable - good reproducibility"
                } else if (cv < 0.5) {
                    "Moderate variability - acceptable"
                } else if (cv < 1.0) {
                    "High variability - use with caution"
                } else {
                    "Very high variability - poor reproducibility"
                }
                
                var_table$addRow(rowKey = feature, list(
                    feature = feature,
                    cv = cv,
                    reliability = reliability,
                    assessment = assessment
                ))
            }
        },
        
        .analyzeSpatialHeterogeneity = function(texture_data, texture_cols) {
            # Spatial heterogeneity analysis using coordinates
            x_coord <- self$options$x_coord
            y_coord <- self$options$y_coord
            
            if (!x_coord %in% names(texture_data) || !y_coord %in% names(texture_data)) {
                return()
            }
            
            spatial_data <- texture_data[, c(texture_cols, x_coord, y_coord), drop = FALSE]
            spatial_data <- spatial_data[complete.cases(spatial_data), ]
            
            if (nrow(spatial_data) < 5) return()
            
            # Enhanced spatial analysis with multiple metrics
            corr_table <- self$results$correlationtable
            
            for (feature in texture_cols) {
                if (feature %in% names(spatial_data)) {
                    feature_values <- spatial_data[[feature]]
                    x_coords <- spatial_data[[x_coord]]
                    y_coords <- spatial_data[[y_coord]]
                    
                    # 1. Basic spatial correlations
                    cor_x <- cor(feature_values, x_coords, use = "complete.obs")
                    cor_y <- cor(feature_values, y_coords, use = "complete.obs")
                    
                    # 2. Spatial autocorrelation using Moran's I approximation
                    spatial_autocorr <- private$.calculateSpatialAutocorrelation(
                        feature_values, x_coords, y_coords
                    )
                    
                    # 3. Local spatial variability (coefficient of variation by region)
                    local_variability <- private$.calculateLocalVariability(
                        feature_values, x_coords, y_coords
                    )
                    
                    # 4. Distance-based spatial pattern analysis
                    distance_pattern <- private$.analyzeDistancePattern(
                        feature_values, x_coords, y_coords
                    )
                    
                    # Combine metrics for overall spatial assessment
                    spatial_metrics <- c(abs(cor_x), abs(cor_y), abs(spatial_autocorr), local_variability)
                    overall_spatial_score <- mean(spatial_metrics, na.rm = TRUE)
                    
                    # Enhanced spatial interpretation
                    spatial_interpretation <- if (overall_spatial_score > 0.7) {
                        paste0("Strong spatial heterogeneity (Score: ", round(overall_spatial_score, 3), ") - ",
                               "Significant spatial clustering and regional variation detected")
                    } else if (overall_spatial_score > 0.5) {
                        paste0("Moderate spatial heterogeneity (Score: ", round(overall_spatial_score, 3), ") - ",
                               "Some spatial structure present")
                    } else if (overall_spatial_score > 0.3) {
                        paste0("Weak spatial heterogeneity (Score: ", round(overall_spatial_score, 3), ") - ",
                               "Limited spatial organization")
                    } else {
                        paste0("Low spatial heterogeneity (Score: ", round(overall_spatial_score, 3), ") - ",
                               "Spatially uniform distribution")
                    }
                    
                    # Add p-value estimation for spatial correlation
                    n <- nrow(spatial_data)
                    spatial_p_value <- if (abs(overall_spatial_score) > 0.1 && n > 10) {
                        # Approximate p-value for spatial correlation
                        t_stat <- overall_spatial_score * sqrt((n - 2) / (1 - overall_spatial_score^2))
                        2 * pt(-abs(t_stat), df = n - 2)
                    } else { NA }
                    
                    corr_table$addRow(rowKey = paste0(feature, "_spatial"), list(
                        feature1 = feature,
                        feature2 = "Spatial Pattern",
                        correlation = overall_spatial_score,
                        p_value = spatial_p_value,
                        interpretation = spatial_interpretation
                    ))
                }
            }
        },
        
        .analyzePrognosticValue = function(texture_data, texture_cols) {
            # Prognostic analysis when outcome variable is provided
            outcome_var <- self$options$outcome_var
            
            if (!outcome_var %in% names(texture_data)) return()
            
            prognostic_data <- texture_data[, c(texture_cols, outcome_var), drop = FALSE]
            prognostic_data <- prognostic_data[complete.cases(prognostic_data), ]
            
            if (nrow(prognostic_data) < 10) return()
            
            # Check if outcome is binary, continuous, or survival time
            outcome_data <- prognostic_data[[outcome_var]]
            
            if (is.factor(outcome_data) || is.character(outcome_data)) {
                # Binary or categorical outcome - use correlation or t-test
                if (length(unique(outcome_data)) == 2) {
                    # Binary outcome - perform AUC analysis if possible
                    group_table <- self$results$groupcomparisontable
                    
                    for (feature in texture_cols) {
                        if (feature %in% names(prognostic_data)) {
                            # Simple correlation with outcome
                            if (requireNamespace('pROC', quietly = TRUE)) {
                                roc_result <- try(pROC::roc(outcome_data, prognostic_data[[feature]], quiet = TRUE))
                                if (!inherits(roc_result, "try-error")) {
                                    auc_val <- as.numeric(pROC::auc(roc_result))
                                    
                                    interpretation <- if (auc_val > 0.8) {
                                        "Excellent prognostic value"
                                    } else if (auc_val > 0.7) {
                                        "Good prognostic value"
                                    } else if (auc_val > 0.6) {
                                        "Moderate prognostic value"
                                    } else {
                                        "Poor prognostic value"
                                    }
                                    
                                    group_table$addRow(rowKey = paste0(feature, "_prog"), list(
                                        feature = paste0(feature, " (Prognostic)"),
                                        group1_n = sum(outcome_data == levels(as.factor(outcome_data))[1]),
                                        group1_mean = mean(prognostic_data[outcome_data == levels(as.factor(outcome_data))[1], feature]),
                                        group2_n = sum(outcome_data == levels(as.factor(outcome_data))[2]),
                                        group2_mean = mean(prognostic_data[outcome_data == levels(as.factor(outcome_data))[2], feature]),
                                        test_statistic = auc_val,
                                        p_value = NA,
                                        effect_size = auc_val,
                                        interpretation = interpretation
                                    ))
                                }
                            }
                        }
                    }
                }
            } else if (is.numeric(outcome_data)) {
                # Continuous outcome - use correlation
                corr_table <- self$results$correlationtable
                
                for (feature in texture_cols) {
                    if (feature %in% names(prognostic_data)) {
                        cor_result <- cor.test(prognostic_data[[feature]], outcome_data)
                        
                        interpretation <- if (abs(cor_result$estimate) > 0.7) {
                            "Strong prognostic correlation"
                        } else if (abs(cor_result$estimate) > 0.5) {
                            "Moderate prognostic correlation"
                        } else if (abs(cor_result$estimate) > 0.3) {
                            "Weak prognostic correlation"
                        } else {
                            "No prognostic correlation"
                        }
                        
                        corr_table$addRow(rowKey = paste0(feature, "_outcome"), list(
                            feature1 = feature,
                            feature2 = outcome_var,
                            correlation = cor_result$estimate,
                            p_value = cor_result$p.value,
                            interpretation = interpretation
                        ))
                    }
                }
            }
        },
        
        .generateBiomarkerInterpretation = function(feature_summaries, highest_cv, most_variable) {
            biomarker_context <- self$options$biomarker_context
            
            if (biomarker_context == "general" || biomarker_context == "custom") {
                return("")  # No specific biomarker interpretation
            }
            
            # Calculate entropy statistics if available
            entropy_features <- names(feature_summaries)[grepl("entropy", names(feature_summaries), ignore.case = TRUE)]
            has_entropy <- length(entropy_features) > 0
            
            entropy_mean <- if (has_entropy) mean(sapply(entropy_features, function(x) feature_summaries[[x]]$mean)) else NA
            entropy_cv <- if (has_entropy) mean(sapply(entropy_features, function(x) feature_summaries[[x]]$sd / feature_summaries[[x]]$mean)) else NA
            
            biomarker_interpretation <- paste0(
                "<h4>", switch(biomarker_context,
                    "ki67" = "Ki67 Proliferation Analysis",
                    "her2" = "HER2 Expression Analysis", 
                    "pdl1" = "PD-L1 Expression Analysis",
                    "cd8" = "CD8+ T-cell Analysis",
                    "Biomarker Analysis"), ":</h4>",
                "<div style='background-color: #f0f8f0; padding: 10px; border-left: 4px solid #28a745; margin: 10px 0;'>"
            )
            
            if (biomarker_context == "ki67") {
                biomarker_interpretation <- paste0(biomarker_interpretation,
                    "<p><strong>Ki67 Spatial Heterogeneity Assessment:</strong></p>",
                    if (has_entropy && entropy_mean > 2.5) {
                        paste0("<p>🔴 <strong>High Ki67 entropy</strong> (mean = ", round(entropy_mean, 2), 
                               ") suggests <strong>aggressive tumor behavior</strong> with spatially heterogeneous proliferation patterns. This may indicate:</p>",
                               "<ul>",
                               "<li>Higher grade tumor biology</li>",
                               "<li>Potential for treatment resistance</li>",
                               "<li>Need for comprehensive sampling strategies</li>",
                               "</ul>")
                    } else if (has_entropy && entropy_mean > 1.5) {
                        paste0("<p>🟡 <strong>Moderate Ki67 entropy</strong> (mean = ", round(entropy_mean, 2), 
                               ") indicates <strong>mixed proliferation patterns</strong>:</p>",
                               "<ul>",
                               "<li>Focal areas of high proliferation</li>",
                               "<li>Standard prognostic value maintained</li>",
                               "<li>Consider regional sampling variation</li>",
                               "</ul>")
                    } else if (has_entropy) {
                        paste0("<p>🟢 <strong>Low Ki67 entropy</strong> (mean = ", round(entropy_mean, 2), 
                               ") suggests <strong>uniform proliferation</strong>:</p>",
                               "<ul>",
                               "<li>Homogeneous tumor growth pattern</li>",
                               "<li>Reliable single-field assessment</li>",
                               "<li>Good prognostic consistency</li>",
                               "</ul>")
                    } else {
                        "<p>⚠️ No entropy features detected. Consider including entropy measurements for comprehensive Ki67 spatial analysis.</p>"
                    },
                    
                    if (highest_cv > 1.0) {
                        paste0("<p><strong>Clinical Significance:</strong> High spatial variability (CV = ", round(highest_cv, 2), ") may require <strong>multiple sampling regions</strong> for accurate Ki67 assessment.</p>")
                    } else {
                        paste0("<p><strong>Clinical Significance:</strong> Moderate spatial variability (CV = ", round(highest_cv, 2), ") supports standard single hot-spot assessment protocols.</p>")
                    }
                )
                
            } else if (biomarker_context == "her2") {
                biomarker_interpretation <- paste0(biomarker_interpretation,
                    "<p><strong>HER2 Expression Spatial Analysis:</strong></p>",
                    if (has_entropy && entropy_mean > 2.0) {
                        paste0("<p>🔴 <strong>High HER2 spatial heterogeneity</strong> (entropy = ", round(entropy_mean, 2), ") suggests:</p>",
                               "<ul>",
                               "<li>Potential for <strong>heterogeneous therapeutic response</strong></li>",
                               "<li>Need for comprehensive tumor sampling</li>",
                               "<li>Consider complementary molecular testing</li>",
                               "</ul>")
                    } else if (has_entropy) {
                        paste0("<p>🟢 <strong>Homogeneous HER2 pattern</strong> (entropy = ", round(entropy_mean, 2), "):</p>",
                               "<ul>",
                               "<li>Uniform expression across tumor regions</li>",
                               "<li>Reliable therapeutic target prediction</li>",
                               "<li>Standard assessment protocols sufficient</li>",
                               "</ul>")
                    } else {
                        "<p>Analysis of HER2 spatial patterns requires entropy measurements for optimal assessment.</p>"
                    }
                )
                
            } else if (biomarker_context == "pdl1") {
                biomarker_interpretation <- paste0(biomarker_interpretation,
                    "<p><strong>PD-L1 Expression Heterogeneity:</strong></p>",
                    if (has_entropy && entropy_mean > 2.2) {
                        paste0("<p>🔴 <strong>High PD-L1 spatial variability</strong> may impact immunotherapy response prediction:</p>",
                               "<ul>",
                               "<li>Regional variation in immune landscape</li>",
                               "<li>Multiple sampling recommended</li>",
                               "<li>Consider combined predictive scores (CPS/TPS)</li>",
                               "</ul>")
                    } else if (has_entropy) {
                        "<p>🟢 <strong>Consistent PD-L1 spatial pattern</strong> supports reliable therapeutic decision-making.</p>"
                    } else {
                        "<p>PD-L1 spatial assessment benefits from entropy analysis for immunotherapy stratification.</p>"
                    }
                )
                
            } else if (biomarker_context == "cd8") {
                biomarker_interpretation <- paste0(biomarker_interpretation,
                    "<p><strong>CD8+ T-cell Spatial Distribution:</strong></p>",
                    if (highest_cv > 1.5) {
                        paste0("<p>🔵 <strong>Heterogeneous immune infiltration</strong> patterns detected:</p>",
                               "<ul>",
                               "<li>Focal immune activation zones</li>",
                               "<li>Potential for immune escape mechanisms</li>",
                               "<li>Comprehensive spatial analysis recommended</li>",
                               "</ul>")
                    } else {
                        "<p>🟢 <strong>Uniform CD8+ distribution</strong> suggests consistent immune surveillance.</p>"
                    }
                )
            }
            
            biomarker_interpretation <- paste0(biomarker_interpretation, "</div>")
            
            return(biomarker_interpretation)
        },
        
        .generateTexturePlots = function(texture_data) {
            # Prepare plot data
            plot_data <- list(
                texture_features = self$options$texture_features,
                texture_data = texture_data[, self$options$texture_features, drop = FALSE],
                has_spatial = (!is.null(self$options$x_coord) && !is.null(self$options$y_coord)),
                has_group = !is.null(self$options$group_var) && "group" %in% names(texture_data),
                spatial_coords = if (!is.null(self$options$x_coord) && !is.null(self$options$y_coord)) {
                    texture_data[, c("x_coord", "y_coord")]
                } else { NULL },
                group_data = if ("group" %in% names(texture_data)) texture_data$group else NULL
            )
            
            self$results$distributionplot$setState(plot_data)
            self$results$heatmapplot$setState(plot_data)
            self$results$boxplot$setState(plot_data)
        },
        
        .distributionplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            library(ggplot2)
            
            # Create texture distribution plots
            texture_cols <- data$texture_features
            
            if (length(texture_cols) == 0) return(FALSE)
            
            # Prepare data for plotting
            plot_df <- data.frame()
            for (col in texture_cols) {
                if (col %in% names(data$texture_data)) {
                    col_data <- data$texture_data[[col]]
                    col_data <- col_data[!is.na(col_data)]
                    if (length(col_data) > 0) {
                        temp_df <- data.frame(
                            Feature = col,
                            Value = col_data
                        )
                        plot_df <- rbind(plot_df, temp_df)
                    }
                }
            }
            
            if (nrow(plot_df) == 0) return(FALSE)
            
            # Create violin/box plot
            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Feature, y = Value)) +
                ggplot2::geom_violin(alpha = 0.7, fill = "lightblue") +
                ggplot2::geom_boxplot(width = 0.1, alpha = 0.8) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggplot2::labs(
                    title = "Haralick Texture Feature Distributions",
                    subtitle = "Distribution characteristics of selected texture features",
                    x = "Texture Feature",
                    y = "Feature Value"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .heatmapplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            library(ggplot2)
            
            texture_data <- data$texture_data
            
            if (ncol(texture_data) < 2) return(FALSE)
            
            # Calculate correlation matrix
            cor_matrix <- cor(texture_data, use = "complete.obs")
            
            # Convert to long format for plotting
            n_features <- ncol(cor_matrix)
            cor_df <- expand.grid(Feature1 = names(texture_data), Feature2 = names(texture_data))
            cor_df$Correlation <- as.vector(cor_matrix)
            
            # Create correlation heatmap
            p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Feature1, y = Feature2, fill = Correlation)) +
                ggplot2::geom_tile() +
                ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                                             midpoint = 0, limit = c(-1, 1)) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                              axis.text.y = ggplot2::element_text(angle = 0)) +
                ggplot2::labs(
                    title = "Haralick Texture Feature Correlation Matrix",
                    subtitle = "Inter-feature correlations (Pearson correlation coefficients)",
                    x = "Texture Features",
                    y = "Texture Features",
                    fill = "Correlation"
                ) +
                ggtheme
                
            print(p)
            TRUE
        },
        
        .generateTextureInterpretation = function(texture_data) {
            n_cases <- nrow(texture_data)
            texture_cols <- self$options$texture_features
            n_features <- length(texture_cols)
            
            # Calculate summary statistics
            feature_summaries <- list()
            for (col in texture_cols) {
                if (col %in% names(texture_data)) {
                    col_data <- texture_data[[col]]
                    col_data <- col_data[!is.na(col_data)]
                    if (length(col_data) > 0) {
                        feature_summaries[[col]] <- list(
                            mean = mean(col_data),
                            sd = sd(col_data),
                            median = median(col_data),
                            range = max(col_data) - min(col_data)
                        )
                    }
                }
            }
            
            # Find most variable feature (highest CV)
            cv_values <- sapply(feature_summaries, function(x) x$sd / x$mean)
            most_variable <- names(cv_values)[which.max(cv_values)]
            highest_cv <- max(cv_values, na.rm = TRUE)
            
            # Assess data quality
            complete_rate <- sum(complete.cases(texture_data[, texture_cols])) / n_cases * 100
            
            # Generate biomarker-specific interpretation
            biomarker_interpretation <- private$.generateBiomarkerInterpretation(feature_summaries, highest_cv, most_variable)
            
            interpretation <- paste0(
                "<h3>Haralick Texture Analysis Summary</h3>",
                "<p><strong>Analysis Overview:</strong> ", n_cases, " observations analyzed across ", 
                n_features, " texture features</p>",
                
                "<h4>Data Quality Assessment:</h4>",
                "<ul>",
                "<li><strong>Completeness:</strong> ", round(complete_rate, 1), "% complete cases ",
                "(", ifelse(complete_rate >= 90, "Excellent", ifelse(complete_rate >= 70, "Good", "Limited")), " data quality)</li>",
                "<li><strong>Feature Coverage:</strong> ", n_features, " texture features analyzed</li>",
                "</ul>",
                
                "<h4>Texture Heterogeneity Assessment:</h4>",
                "<ul>",
                "<li><strong>Most Variable Feature:</strong> ", most_variable, " (CV = ", round(highest_cv, 3), ")",
                ifelse(highest_cv > 0.5, " - High heterogeneity detected", " - Moderate heterogeneity"), "</li>",
                
                if (length(feature_summaries) > 0) {
                    paste0("<li><strong>Feature Ranges:</strong> ",
                           paste(sapply(names(feature_summaries)[1:min(3, length(feature_summaries))], function(x) {
                               paste0(x, ": ", round(feature_summaries[[x]]$range, 3))
                           }), collapse = ", "),
                           if (length(feature_summaries) > 3) "..." else "", "</li>")
                } else { "" },
                "</ul>",
                
                "<h4>Clinical Interpretation:</h4>",
                "<div style='background-color: #f8f9fa; padding: 10px; border-left: 4px solid #007bff;'>",
                if (highest_cv > 1.0) {
                    "<p><strong>High Spatial Heterogeneity:</strong> Texture features show substantial variability, 
                    indicating significant spatial heterogeneity. <span style='color: blue;'>This may have prognostic significance 
                    and warrants further clinical correlation.</span></p>"
                } else if (highest_cv > 0.5) {
                    "<p><strong>Moderate Spatial Heterogeneity:</strong> Texture features demonstrate moderate variability. 
                    <span style='color: green;'>Suitable for biomarker development with appropriate statistical methods.</span></p>"
                } else {
                    "<p><strong>Low Spatial Heterogeneity:</strong> Texture features show limited variability. 
                    <span style='color: orange;'>Consider alternative features or analysis methods for biomarker development.</span></p>"
                },
                "</div>",
                
                # Add biomarker-specific interpretation
                biomarker_interpretation,
                
                "<h4>Statistical Recommendations:</h4>",
                "<ul>",
                "<li><strong>Distribution Assessment:</strong> Review distribution table for normality assumptions</li>",
                "<li><strong>Feature Selection:</strong> Consider correlation analysis to identify redundant features</li>",
                if (highest_cv > 0.5) {
                    "<li><strong>Prognostic Analysis:</strong> High heterogeneity suggests potential for survival analysis</li>"
                } else { "" },
                "<li><strong>Validation:</strong> Confirm findings in independent cohort</li>",
                "</ul>",
                
                "<h4>Methodological Notes:</h4>",
                "<ul>",
                "<li>Haralick features quantify spatial relationships in grayscale co-occurrence matrices</li>",
                "<li>Entropy measures randomness/disorder in texture patterns</li>",
                "<li>High entropy indicates heterogeneous, complex tissue architecture</li>",
                "<li>Low entropy suggests uniform, homogeneous tissue patterns</li>",
                "</ul>",
                
                "<h4>Quality Control Checks:</h4>",
                "<ul>",
                "<li>Verify image preprocessing consistency across samples</li>",
                "<li>Check for outliers in texture measurements</li>",
                "<li>Confirm spatial sampling adequacy</li>",
                "<li>Validate feature extraction parameters</li>",
                "</ul>",
                
                "<p><em>This analysis implements Haralick texture feature methodology validated in digital pathology 
                research (Haralick et al. 1973; Zilenaite-Petrulaitiene et al. 2025). Features are computed from 
                grayscale co-occurrence matrices and provide quantitative measures of spatial tissue heterogeneity.</em></p>"
            )
            
            self$results$interpretation$setContent(interpretation)
            
            # Generate copy-ready summary
            copy_ready_summary <- private$.generateCopyReadySummary(n_cases, n_features, highest_cv, most_variable, complete_rate)
            self$results$summary$setContent(copy_ready_summary)
        },
        
        .generateCopyReadySummary = function(n_cases, n_features, highest_cv, most_variable, complete_rate) {
            biomarker_context <- self$options$biomarker_context
            analysis_focus <- self$options$analysis_focus
            
            # Generate main summary sentence
            heterogeneity_level <- if (highest_cv > 1.0) {
                "high spatial heterogeneity"
            } else if (highest_cv > 0.5) {
                "moderate spatial heterogeneity" 
            } else {
                "low spatial heterogeneity"
            }
            
            biomarker_text <- switch(biomarker_context,
                "ki67" = " in Ki67 proliferation patterns",
                "her2" = " in HER2 expression patterns",
                "pdl1" = " in PD-L1 expression patterns", 
                "cd8" = " in CD8+ T-cell distribution patterns",
                ""
            )
            
            main_finding <- sprintf(
                "Haralick texture analysis of %d cases across %d features%s revealed %s (CV=%.3f).",
                n_cases, n_features, biomarker_text, heterogeneity_level, highest_cv
            )
            
            # Clinical interpretation sentence
            clinical_interpretation <- if (highest_cv > 1.0) {
                "This high heterogeneity suggests complex tissue architecture that may have prognostic significance and requires comprehensive sampling for accurate biomarker assessment."
            } else if (highest_cv > 0.5) {
                "This moderate heterogeneity indicates mixed tissue patterns suitable for standard biomarker analysis protocols."
            } else {
                "This low heterogeneity suggests uniform tissue patterns; consider alternative texture features or spatial analysis methods for enhanced discrimination."
            }
            
            # Data quality assessment
            quality_note <- if (complete_rate < 90) {
                sprintf(" Data completeness was %.1f%%, which may affect result reliability.", complete_rate)
            } else {
                ""
            }
            
            # Statistical significance note
            stats_note <- if (n_cases >= 30) {
                " Results are based on adequate sample size for statistical inference."
            } else if (n_cases >= 10) {
                " Results are based on limited sample size; validation in larger cohort recommended."
            } else {
                " Results are preliminary due to small sample size (<10 cases)."
            }
            
            # Complete copy-ready text
            copy_text <- paste0(main_finding, " ", clinical_interpretation, quality_note, stats_note)
            
            return(paste0(
                "<div class='copy-ready-summary' style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; border: 1px solid #dee2e6;'>",
                "<h4>📋 Copy-Ready Summary</h4>",
                "<div style='background-color: white; padding: 15px; border-radius: 5px; border: 1px solid #e9ecef; margin: 10px 0;'>",
                "<p style='margin: 0; line-height: 1.6;'>", copy_text, "</p>",
                "</div>",
                
                "<div style='margin-top: 15px;'>",
                "<button onclick='navigator.clipboard.writeText(`", gsub('"', "'", copy_text), "`)' ",
                "style='background-color: #007bff; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer; margin-right: 10px;'>",
                "📋 Copy to Clipboard</button>",
                "<small style='color: #6c757d;'>Click to copy this summary for your clinical report or manuscript</small>",
                "</div>",
                
                "<h5 style='margin-top: 20px; margin-bottom: 10px;'>📊 Key Statistics:</h5>",
                "<ul style='margin-left: 20px; line-height: 1.8;'>",
                "<li><strong>Sample Size:</strong> ", n_cases, " cases</li>",
                "<li><strong>Texture Features:</strong> ", n_features, " Haralick measurements</li>",
                "<li><strong>Most Variable Feature:</strong> ", most_variable, " (CV = ", round(highest_cv, 3), ")</li>",
                "<li><strong>Data Completeness:</strong> ", round(complete_rate, 1), "%</li>",
                if (biomarker_context != "general" && biomarker_context != "custom") {
                    paste0("<li><strong>Biomarker Context:</strong> ", 
                           switch(biomarker_context,
                               "ki67" = "Ki67 Proliferation Analysis",
                               "her2" = "HER2 Expression Analysis",
                               "pdl1" = "PD-L1 Expression Analysis", 
                               "cd8" = "CD8+ T-cell Analysis"), "</li>")
                } else { "" },
                "<li><strong>Analysis Type:</strong> ", 
                switch(analysis_focus,
                    "descriptive" = "Descriptive Statistics",
                    "correlation" = "Feature Correlation Analysis", 
                    "spatial" = "Spatial Heterogeneity Assessment",
                    "prognostic" = "Prognostic Modeling",
                    "comprehensive" = "Comprehensive Analysis"), "</li>",
                "</ul>",
                
                "<div style='margin-top: 15px; padding: 10px; background-color: #e8f4fd; border-left: 4px solid #2196f3; border-radius: 4px;'>",
                "<p style='margin: 0; font-size: 0.9em;'><strong>📚 Reference:</strong> ",
                "Haralick texture features computed following validated methodology (Haralick et al., 1973) ",
                "with clinical applications in digital pathology biomarker assessment.</p>",
                "</div>",
                "</div>"
            ))
        },
        
        .detectMisuse = function(texture_data) {
            warnings <- c()
            texture_features <- self$options$texture_features
            
            # Check sample size adequacy
            n_cases <- nrow(texture_data)
            if (n_cases < 5) {
                warnings <- c(warnings, paste0(
                    "⚠️ <strong>Small Sample Warning:</strong> Only ", n_cases, " cases detected. ",
                    "Texture analysis typically requires at least 5 cases for reliable statistical inference. ",
                    "Results should be interpreted with caution and validated in a larger cohort."
                ))
            } else if (n_cases < 30) {
                warnings <- c(warnings, paste0(
                    "⚠️ <strong>Limited Sample Size:</strong> ", n_cases, " cases may be insufficient for robust statistical analysis. ",
                    "Consider collecting additional cases or interpreting results as preliminary findings."
                ))
            }
            
            # Check feature count adequacy
            n_features <- length(texture_features)
            if (n_features > n_cases) {
                warnings <- c(warnings, paste0(
                    "⚠️ <strong>High-Dimensional Data Warning:</strong> ", n_features, " features with only ", n_cases, " cases. ",
                    "This high feature-to-sample ratio may lead to overfitting. Consider feature selection or dimensionality reduction."
                ))
            }
            
            # Check for spatial analysis without coordinates
            analysis_focus <- self$options$analysis_focus
            if (analysis_focus == "spatial" && (is.null(self$options$x_coord) || is.null(self$options$y_coord))) {
                warnings <- c(warnings, paste0(
                    "⚠️ <strong>Spatial Analysis Warning:</strong> Spatial focus selected but X/Y coordinates not provided. ",
                    "Spatial heterogeneity analysis requires coordinate data. Please add spatial coordinates or change analysis focus."
                ))
            }
            
            # Check for prognostic analysis without outcomes
            if (analysis_focus == "prognostic" && is.null(self$options$outcome_var)) {
                warnings <- c(warnings, paste0(
                    "⚠️ <strong>Prognostic Analysis Warning:</strong> Prognostic focus selected but no outcome variable provided. ",
                    "Add a clinical outcome variable or change analysis focus to descriptive or correlation analysis."
                ))
            }
            
            # Check biomarker context alignment
            biomarker_context <- self$options$biomarker_context
            if (biomarker_context != "general" && biomarker_context != "custom") {
                # Check if entropy features are available for biomarker analysis
                has_entropy <- any(grepl("entropy", texture_features, ignore.case = TRUE))
                if (!has_entropy) {
                    warnings <- c(warnings, paste0(
                        "💡 <strong>Biomarker Analysis Recommendation:</strong> ", 
                        switch(biomarker_context,
                            "ki67" = "Ki67",
                            "her2" = "HER2", 
                            "pdl1" = "PD-L1",
                            "cd8" = "CD8+"), 
                        " analysis benefits from entropy measurements. Consider including entropy features for optimal biomarker assessment."
                    ))
                }
            }
            
            # Check for excessive missing data
            complete_cases <- sum(complete.cases(texture_data[, texture_features]))
            missing_rate <- (n_cases - complete_cases) / n_cases * 100
            
            if (missing_rate > 50) {
                warnings <- c(warnings, paste0(
                    "🔴 <strong>High Missing Data Warning:</strong> ", round(missing_rate, 1), "% of cases have missing texture data. ",
                    "High missingness may severely compromise analysis validity. Review data collection and preprocessing procedures."
                ))
            } else if (missing_rate > 20) {
                warnings <- c(warnings, paste0(
                    "🟡 <strong>Moderate Missing Data:</strong> ", round(missing_rate, 1), "% of cases have incomplete texture data. ",
                    "Consider imputation methods or sensitivity analysis to assess impact on results."
                ))
            }
            
            # Check for inappropriate feature selection with small samples
            feature_selection <- self$options$feature_selection
            if (n_cases < 20 && feature_selection != "all") {
                warnings <- c(warnings, paste0(
                    "⚠️ <strong>Feature Selection Warning:</strong> Feature selection with <20 cases may be unreliable. ",
                    "Consider using all available features or collecting additional cases before applying feature selection."
                ))
            }
            
            # Check correlation threshold appropriateness
            correlation_threshold <- self$options$correlation_threshold
            if (correlation_threshold < 0.8 && feature_selection == "correlation_filter") {
                warnings <- c(warnings, paste0(
                    "💡 <strong>Correlation Threshold Recommendation:</strong> Threshold of ", correlation_threshold, 
                    " may remove moderately correlated but clinically relevant features. ",
                    "Consider using threshold ≥0.8 for texture features unless specifically investigating redundancy."
                ))
            }
            
            # Check for homogeneous data (all features have low variance)
            texture_matrix <- texture_data[, texture_features, drop = FALSE]
            texture_matrix <- texture_matrix[complete.cases(texture_matrix), , drop = FALSE]
            
            if (nrow(texture_matrix) >= 3) {
                feature_cvs <- apply(texture_matrix, 2, function(x) {
                    if (sd(x, na.rm = TRUE) == 0 || mean(x, na.rm = TRUE) == 0) return(0)
                    abs(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
                })
                
                low_variance_features <- sum(feature_cvs < 0.1)
                if (low_variance_features > length(texture_features) * 0.5) {
                    warnings <- c(warnings, paste0(
                        "⚠️ <strong>Low Variability Warning:</strong> ", low_variance_features, "/", length(texture_features), 
                        " features show very low variability (CV < 0.1). ",
                        "This may indicate processing artifacts, insufficient tissue heterogeneity, or inappropriate feature selection. ",
                        "Review image preprocessing and sampling strategies."
                    ))
                }
            }
            
            return(warnings)
        },
        
        .calculateSpatialAutocorrelation = function(values, x_coords, y_coords) {
            # Simplified Moran's I calculation for spatial autocorrelation
            n <- length(values)
            if (n < 5) return(0)
            
            # Create simple distance-based weights (inverse distance)
            weights_sum <- 0
            numerator <- 0
            
            for (i in 1:(n-1)) {
                for (j in (i+1):n) {
                    # Euclidean distance
                    distance <- sqrt((x_coords[i] - x_coords[j])^2 + (y_coords[i] - y_coords[j])^2)
                    if (distance > 0) {
                        weight <- 1 / (1 + distance)  # Inverse distance weight
                        weights_sum <- weights_sum + 2 * weight  # Symmetric weights
                        
                        # Cross-product of deviations
                        numerator <- numerator + 2 * weight * (values[i] - mean(values)) * (values[j] - mean(values))
                    }
                }
            }
            
            # Moran's I formula approximation
            if (weights_sum > 0) {
                variance <- var(values)
                if (variance > 0) {
                    morans_i <- (n / weights_sum) * (numerator / (n * variance))
                    return(pmax(-1, pmin(1, morans_i)))  # Bound between -1 and 1
                }
            }
            return(0)
        },
        
        .calculateLocalVariability = function(values, x_coords, y_coords) {
            # Calculate local coefficient of variation based on spatial neighbors
            n <- length(values)
            if (n < 5) return(0)
            
            # Define neighborhood radius as 1/4 of the spatial range
            x_range <- max(x_coords) - min(x_coords)
            y_range <- max(y_coords) - min(y_coords)
            radius <- min(x_range, y_range) * 0.25
            
            local_cvs <- c()
            
            for (i in 1:n) {
                # Find neighbors within radius
                distances <- sqrt((x_coords - x_coords[i])^2 + (y_coords - y_coords[i])^2)
                neighbors <- which(distances <= radius)
                
                if (length(neighbors) >= 3) {
                    neighbor_values <- values[neighbors]
                    local_mean <- mean(neighbor_values)
                    local_sd <- sd(neighbor_values)
                    
                    if (local_mean > 0) {
                        local_cvs <- c(local_cvs, local_sd / local_mean)
                    }
                }
            }
            
            if (length(local_cvs) > 0) {
                return(mean(local_cvs))
            } else {
                return(sd(values) / mean(values))  # Fallback to global CV
            }
        },
        
        .analyzeDistancePattern = function(values, x_coords, y_coords) {
            # Analyze how texture values change with distance
            n <- length(values)
            if (n < 5) return(0)
            
            distances <- c()
            value_diffs <- c()
            
            for (i in 1:(n-1)) {
                for (j in (i+1):n) {
                    distance <- sqrt((x_coords[i] - x_coords[j])^2 + (y_coords[i] - y_coords[j])^2)
                    value_diff <- abs(values[i] - values[j])
                    
                    distances <- c(distances, distance)
                    value_diffs <- c(value_diffs, value_diff)
                }
            }
            
            if (length(distances) > 3) {
                # Correlation between distance and value difference
                distance_correlation <- cor(distances, value_diffs, use = "complete.obs")
                return(abs(distance_correlation))  # Return absolute correlation
            }
            return(0)
        },
        
        .addClinicalWarnings = function(warnings) {
            if (length(warnings) == 0) return()
            
            warning_html <- paste0(
                "<div style='background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 8px; padding: 15px; margin: 20px 0;'>",
                "<h4 style='color: #856404; margin-top: 0;'>⚠️ Clinical Usage Warnings & Recommendations</h4>",
                "<div style='line-height: 1.6;'>",
                paste(warnings, collapse = "<br><br>"),
                "</div>",
                "<p style='margin-bottom: 0; margin-top: 15px; font-size: 0.9em; color: #856404;'>",
                "<strong>Recommendation:</strong> Address these warnings before making clinical decisions based on these results.</p>",
                "</div>"
            )
            
            # Append warnings to existing interpretation
            current_interpretation <- self$results$interpretation$content
            enhanced_interpretation <- paste0(current_interpretation, warning_html)
            self$results$interpretation$setContent(enhanced_interpretation)
        },
        
        .boxplot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
                
            data <- image$state
            if (is.null(data) || nrow(data) == 0)
                return(FALSE)
            
            texture_cols <- self$options$texture_features
            if (length(texture_cols) == 0)
                return(FALSE)
            
            group_col <- self$options$group_var
            if (is.null(group_col) || !group_col %in% names(data))
                return(FALSE)
            
            # Prepare data for plotting
            texture_data <- data[, c(texture_cols, group_col), drop = FALSE]
            texture_long <- tidyr::pivot_longer(
                texture_data, 
                cols = all_of(texture_cols),
                names_to = "Feature",
                values_to = "Value"
            )
            
            p <- ggplot2::ggplot(texture_long, ggplot2::aes(x = .data[[group_col]], y = Value)) +
                ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
                ggplot2::facet_wrap(~ Feature, scales = "free_y") +
                ggplot2::labs(
                    title = "Texture Features by Group",
                    x = group_col,
                    y = "Feature Value"
                ) +
                ggtheme
                
            print(p)
            TRUE
        }
    )
)