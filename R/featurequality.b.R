# Feature Quality Assessment for Clinical Data
# Comprehensive quality control analysis for tabular clinical datasets

featurequalityClass <- R6::R6Class(
    "featurequalityClass",
    inherit = featurequalityBase,
    private = list(
        .init = function() {
            # Initialize analysis instructions
            self$results$instructions$setContent(
                "<h3>Feature Quality Assessment</h3>
                <p>This module provides comprehensive quality control analysis for clinical and pathology research data:</p>
                <ul>
                    <li><b>Missing Data Analysis:</b> Pattern detection, impact assessment, and missingness mechanisms</li>
                    <li><b>Distribution Analysis:</b> Descriptive statistics, normality testing, and distribution characterization</li>
                    <li><b>Outlier Detection:</b> Multiple methods (IQR, Z-score, MAD, Isolation Forest) with clinical interpretation</li>
                    <li><b>Correlation Analysis:</b> Feature redundancy detection and multicollinearity assessment</li>
                    <li><b>Variance Analysis:</b> Information content evaluation and low-variance feature identification</li>
                    <li><b>Quality Scoring:</b> Overall feature quality assessment with actionable recommendations</li>
                </ul>
                <p>Select your features and analysis options to begin the quality assessment.</p>"
            )
            
            # Initialize progress
            self$results$progress$setContent(
                "<p><i>Ready to perform feature quality assessment. Please select features to analyze.</i></p>"
            )
        },
        
        .run = function() {
            # Check for required inputs
            if (is.null(self$options$features) || length(self$options$features) == 0) {
                self$results$progress$setContent(
                    "<p style='color: red;'>Please select at least one feature to analyze.</p>"
                )
                return()
            }
            
            # Update progress
            self$results$progress$setContent(
                "<p><i>Performing comprehensive feature quality assessment...</i></p>"
            )
            
            # Get data and features
            data <- self$data
            features <- self$options$features
            
            # Validate features exist and are numeric
            valid_features <- private$.validateFeatures(data, features)
            if (length(valid_features) == 0) {
                self$results$progress$setContent(
                    "<p style='color: red;'>No valid numeric features found for analysis.</p>"
                )
                return()
            }
            
            # Set random seed for reproducibility
            set.seed(self$options$random_seed)
            
            # Perform comprehensive quality analysis
            tryCatch({
                private$.performQualityAnalysis(data, valid_features)
                
                # Update final progress
                self$results$progress$setContent(
                    "<p style='color: green;'><b>Feature quality assessment completed successfully!</b></p>
                    <p>Review the quality summary table and detailed analyses below.</p>"
                )
                
            }, error = function(e) {
                self$results$progress$setContent(
                    paste0("<p style='color: red;'>Error during analysis: ", e$message, "</p>")
                )
            })
        },
        
        .validateFeatures = function(data, features) {
            # Validate that features exist and are numeric
            valid_features <- c()
            
            for (feature in features) {
                if (feature %in% names(data)) {
                    feature_data <- data[[feature]]
                    if (is.numeric(feature_data)) {
                        valid_features <- c(valid_features, feature)
                    }
                }
            }
            
            return(valid_features)
        },
        
        .performQualityAnalysis = function(data, features) {
            # Main quality analysis orchestrator
            
            # 1. Quality Summary Analysis
            private$.performQualitySummary(data, features)
            
            # 2. Missing Data Analysis
            if (self$options$missing_data_analysis) {
                private$.performMissingDataAnalysis(data, features)
            }
            
            # 3. Distribution Analysis
            if (self$options$distribution_analysis) {
                private$.performDistributionAnalysis(data, features)
            }
            
            # 4. Outlier Detection
            if (self$options$outlier_detection) {
                private$.performOutlierAnalysis(data, features)
            }
            
            # 5. Correlation Analysis
            if (self$options$correlation_analysis && length(features) > 1) {
                private$.performCorrelationAnalysis(data, features)
            }
            
            # 6. Variance Analysis
            if (self$options$variance_analysis) {
                private$.performVarianceAnalysis(data, features)
            }
            
            # 7. Normality Testing
            if (self$options$normality_testing) {
                private$.performNormalityAnalysis(data, features)
            }
            
            # 8. Feature Importance (if grouping variable provided)
            if (self$options$feature_importance && !is.null(self$options$group_var)) {
                private$.performFeatureImportanceAnalysis(data, features)
            }
            
            # 9. Transformation Recommendations
            if (self$options$data_transformation) {
                private$.performTransformationRecommendations(data, features)
            }
            
            # 10. Overall Recommendations
            private$.generateOverallRecommendations(data, features)
            
            # 11. Clinical Interpretation
            if (self$options$clinical_context) {
                private$.generateClinicalInterpretation(data, features)
            }
        },
        
        .performQualitySummary = function(data, features) {
            # Generate overall quality summary table
            
            quality_table <- self$results$quality_summary
            
            for (feature in features) {
                feature_data <- data[[feature]]
                
                # Calculate quality metrics
                missing_pct <- sum(is.na(feature_data)) / length(feature_data)
                
                # Outlier detection (using IQR method for summary)
                Q1 <- quantile(feature_data, 0.25, na.rm = TRUE)
                Q3 <- quantile(feature_data, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                outliers <- feature_data < (Q1 - 1.5 * IQR) | feature_data > (Q3 + 1.5 * IQR)
                outliers_pct <- sum(outliers, na.rm = TRUE) / length(feature_data)
                
                # Normality test (Shapiro-Wilk for summary)
                if (length(feature_data[!is.na(feature_data)]) >= 3 && 
                    length(feature_data[!is.na(feature_data)]) <= 5000) {
                    normality_p <- tryCatch({
                        shapiro.test(feature_data[!is.na(feature_data)])$p.value
                    }, error = function(e) NA)
                } else {
                    normality_p <- NA
                }
                
                # Skewness and kurtosis
                if (length(feature_data[!is.na(feature_data)]) >= 3) {
                    skewness_val <- private$.calculateSkewness(feature_data)
                    kurtosis_val <- private$.calculateKurtosis(feature_data)
                } else {
                    skewness_val <- NA
                    kurtosis_val <- NA
                }
                
                # Variance
                variance_val <- var(feature_data, na.rm = TRUE)
                
                # Calculate overall quality score (0-100)
                quality_score <- private$.calculateQualityScore(
                    missing_pct, outliers_pct, normality_p, skewness_val, kurtosis_val, variance_val
                )
                
                # Generate recommendation
                recommendation <- private$.generateFeatureRecommendation(
                    missing_pct, outliers_pct, normality_p, skewness_val, quality_score
                )
                
                # Add row to table
                quality_table$addRow(list(
                    feature = feature,
                    quality_score = quality_score,
                    missing_pct = missing_pct,
                    outliers_pct = outliers_pct,
                    normality_p = if (is.na(normality_p)) NULL else normality_p,
                    skewness = if (is.na(skewness_val)) NULL else skewness_val,
                    kurtosis = if (is.na(kurtosis_val)) NULL else kurtosis_val,
                    variance = if (is.na(variance_val)) NULL else variance_val,
                    recommendation = recommendation
                ))
            }
        },
        
        .performMissingDataAnalysis = function(data, features) {
            # Detailed missing data analysis
            
            missing_table <- self$results$missing_data_analysis
            
            for (feature in features) {
                feature_data <- data[[feature]]
                
                # Missing data statistics
                missing_count <- sum(is.na(feature_data))
                missing_percent <- missing_count / length(feature_data)
                
                # Missing pattern analysis
                missing_pattern <- private$.analyzeMissingPattern(feature_data)
                
                # Impact assessment
                impact_assessment <- private$.assessMissingDataImpact(missing_percent, missing_pattern)
                
                missing_table$addRow(list(
                    feature = feature,
                    missing_count = missing_count,
                    missing_percent = missing_percent,
                    missing_pattern = missing_pattern,
                    impact_assessment = impact_assessment
                ))
            }
        },
        
        .performDistributionAnalysis = function(data, features) {
            # Comprehensive distribution analysis
            
            dist_table <- self$results$distribution_analysis
            
            for (feature in features) {
                feature_data <- data[[feature]]
                clean_data <- feature_data[!is.na(feature_data)]
                
                if (length(clean_data) < 3) {
                    dist_table$addRow(list(
                        feature = feature,
                        mean = NULL, median = NULL, sd = NULL,
                        min = NULL, max = NULL, q25 = NULL, q75 = NULL,
                        distribution_type = "Insufficient data"
                    ))
                    next
                }
                
                # Calculate descriptive statistics
                mean_val <- mean(clean_data)
                median_val <- median(clean_data)
                sd_val <- sd(clean_data)
                min_val <- min(clean_data)
                max_val <- max(clean_data)
                q25_val <- quantile(clean_data, 0.25)
                q75_val <- quantile(clean_data, 0.75)
                
                # Determine distribution type
                distribution_type <- private$.determineDistributionType(clean_data)
                
                dist_table$addRow(list(
                    feature = feature,
                    mean = mean_val,
                    median = median_val,
                    sd = sd_val,
                    min = min_val,
                    max = max_val,
                    q25 = q25_val,
                    q75 = q75_val,
                    distribution_type = distribution_type
                ))
            }
        },
        
        .performOutlierAnalysis = function(data, features) {
            # Comprehensive outlier detection
            
            outlier_table <- self$results$outlier_analysis
            
            for (feature in features) {
                feature_data <- data[[feature]]
                clean_data <- feature_data[!is.na(feature_data)]
                
                if (length(clean_data) < 3) {
                    outlier_table$addRow(list(
                        feature = feature,
                        method = "Insufficient data",
                        outlier_count = 0,
                        outlier_percent = 0,
                        lower_bound = NULL,
                        upper_bound = NULL,
                        severity = "None"
                    ))
                    next
                }
                
                # Apply selected outlier detection methods
                methods <- if (self$options$outlier_method == "multiple") {
                    c("iqr", "zscore", "modified_zscore")
                } else {
                    self$options$outlier_method
                }
                
                for (method in methods) {
                    outlier_results <- private$.detectOutliers(clean_data, method, self$options$outlier_threshold)
                    
                    outlier_table$addRow(list(
                        feature = feature,
                        method = toupper(method),
                        outlier_count = outlier_results$count,
                        outlier_percent = outlier_results$percent,
                        lower_bound = outlier_results$lower_bound,
                        upper_bound = outlier_results$upper_bound,
                        severity = outlier_results$severity
                    ))
                }
            }
        },
        
        .performCorrelationAnalysis = function(data, features) {
            # Feature correlation and redundancy analysis
            
            corr_table <- self$results$correlation_analysis
            
            # Calculate correlation matrix
            feature_data <- data[, features, drop = FALSE]
            feature_data <- feature_data[complete.cases(feature_data), ]
            
            if (nrow(feature_data) < 3) {
                corr_table$addRow(list(
                    feature1 = "All features",
                    feature2 = "All features",
                    correlation = NULL,
                    p_value = NULL,
                    significance = "Insufficient data",
                    redundancy_risk = "Cannot assess"
                ))
                return()
            }
            
            for (i in 1:(length(features) - 1)) {
                for (j in (i + 1):length(features)) {
                    feature1 <- features[i]
                    feature2 <- features[j]
                    
                    # Calculate correlation
                    corr_test <- tryCatch({
                        cor.test(feature_data[[feature1]], feature_data[[feature2]])
                    }, error = function(e) NULL)
                    
                    if (!is.null(corr_test)) {
                        correlation <- corr_test$estimate
                        p_value <- corr_test$p.value
                        
                        # Assess significance
                        significance <- if (p_value < 0.001) {
                            "***"
                        } else if (p_value < 0.01) {
                            "**"
                        } else if (p_value < 0.05) {
                            "*"
                        } else {
                            "ns"
                        }
                        
                        # Assess redundancy risk
                        redundancy_risk <- if (abs(correlation) >= self$options$correlation_threshold) {
                            "High - Consider removing one feature"
                        } else if (abs(correlation) >= 0.7) {
                            "Moderate - Monitor for multicollinearity"
                        } else if (abs(correlation) >= 0.5) {
                            "Low - Acceptable correlation"
                        } else {
                            "Minimal - Independent features"
                        }
                        
                        corr_table$addRow(list(
                            feature1 = feature1,
                            feature2 = feature2,
                            correlation = correlation,
                            p_value = p_value,
                            significance = significance,
                            redundancy_risk = redundancy_risk
                        ))
                    }
                }
            }
        },
        
        .performVarianceAnalysis = function(data, features) {
            # Variance and information content analysis
            
            var_table <- self$results$variance_analysis
            
            for (feature in features) {
                feature_data <- data[[feature]]
                clean_data <- feature_data[!is.na(feature_data)]
                
                if (length(clean_data) < 2) {
                    var_table$addRow(list(
                        feature = feature,
                        variance = NULL,
                        coefficient_variation = NULL,
                        variance_category = "Insufficient data",
                        information_content = "Cannot assess"
                    ))
                    next
                }
                
                # Calculate variance metrics
                variance_val <- var(clean_data)
                mean_val <- mean(clean_data)
                cv <- if (mean_val != 0) abs(sd(clean_data) / mean_val) else Inf
                
                # Categorize variance
                variance_category <- if (variance_val < self$options$low_variance_threshold) {
                    "Low Variance - Limited Information"
                } else if (variance_val < 1) {
                    "Moderate Variance"
                } else {
                    "High Variance"
                }
                
                # Assess information content
                information_content <- if (variance_val < self$options$low_variance_threshold) {
                    "Low - Consider removal"
                } else if (cv > 2) {
                    "High - Highly variable feature"
                } else if (cv < 0.1) {
                    "Low - Nearly constant values"
                } else {
                    "Good - Adequate variability"
                }
                
                var_table$addRow(list(
                    feature = feature,
                    variance = variance_val,
                    coefficient_variation = cv,
                    variance_category = variance_category,
                    information_content = information_content
                ))
            }
        },
        
        .performNormalityAnalysis = function(data, features) {
            # Comprehensive normality testing
            
            norm_table <- self$results$normality_analysis
            
            for (feature in features) {
                feature_data <- data[[feature]]
                clean_data <- feature_data[!is.na(feature_data)]
                
                if (length(clean_data) < 3) {
                    norm_table$addRow(list(
                        feature = feature,
                        test_method = "Insufficient data",
                        statistic = NULL,
                        p_value = NULL,
                        normality_conclusion = "Cannot test",
                        transformation_suggestion = "N/A"
                    ))
                    next
                }
                
                # Apply selected normality tests
                methods <- if (self$options$normality_method == "multiple") {
                    c("shapiro", "anderson")
                } else {
                    self$options$normality_method
                }
                
                for (method in methods) {
                    norm_results <- private$.testNormality(clean_data, method)
                    
                    # Generate transformation suggestion
                    transformation_suggestion <- private$.suggestTransformation(clean_data, norm_results$p_value)
                    
                    norm_table$addRow(list(
                        feature = feature,
                        test_method = norm_results$method,
                        statistic = norm_results$statistic,
                        p_value = norm_results$p_value,
                        normality_conclusion = norm_results$conclusion,
                        transformation_suggestion = transformation_suggestion
                    ))
                }
            }
        },
        
        .performFeatureImportanceAnalysis = function(data, features) {
            # Feature importance analysis (requires grouping variable)
            
            importance_table <- self$results$feature_importance
            group_var <- self$options$group_var
            
            if (is.null(group_var) || !group_var %in% names(data)) {
                return()
            }
            
            # Prepare data
            feature_data <- data[, c(features, group_var), drop = FALSE]
            feature_data <- feature_data[complete.cases(feature_data), ]
            
            if (nrow(feature_data) < 10) {
                importance_table$addRow(list(
                    feature = "All features",
                    importance_score = NULL,
                    importance_rank = NULL,
                    relative_importance = NULL,
                    clinical_relevance = "Insufficient data for importance analysis"
                ))
                return()
            }
            
            # Calculate importance based on selected method
            importance_scores <- private$.calculateFeatureImportance(
                feature_data, features, group_var, self$options$importance_method
            )
            
            # Rank features
            importance_ranks <- rank(-importance_scores, ties.method = "min")
            total_importance <- sum(importance_scores)
            
            for (i in 1:length(features)) {
                feature <- features[i]
                score <- importance_scores[i]
                rank_val <- importance_ranks[i]
                relative_importance <- if (total_importance > 0) score / total_importance else 0
                
                # Assess clinical relevance
                clinical_relevance <- if (relative_importance > 0.2) {
                    "High - Key discriminative feature"
                } else if (relative_importance > 0.1) {
                    "Moderate - Contributing feature"
                } else if (relative_importance > 0.05) {
                    "Low - Minor contribution"
                } else {
                    "Minimal - Consider removal"
                }
                
                importance_table$addRow(list(
                    feature = feature,
                    importance_score = score,
                    importance_rank = rank_val,
                    relative_importance = relative_importance,
                    clinical_relevance = clinical_relevance
                ))
            }
        },
        
        .performTransformationRecommendations = function(data, features) {
            # Generate transformation recommendations
            
            transform_table <- self$results$transformation_recommendations
            
            for (feature in features) {
                feature_data <- data[[feature]]
                clean_data <- feature_data[!is.na(feature_data)]
                
                if (length(clean_data) < 3) {
                    transform_table$addRow(list(
                        feature = feature,
                        current_issue = "Insufficient data",
                        recommended_transformation = "None",
                        expected_improvement = "N/A",
                        implementation_notes = "Need more data points"
                    ))
                    next
                }
                
                # Analyze current issues and recommend transformations
                analysis_results <- private$.analyzeFeatureIssues(clean_data)
                
                transform_table$addRow(list(
                    feature = feature,
                    current_issue = analysis_results$issue,
                    recommended_transformation = analysis_results$transformation,
                    expected_improvement = analysis_results$improvement,
                    implementation_notes = analysis_results$notes
                ))
            }
        },
        
        .generateOverallRecommendations = function(data, features) {
            # Generate comprehensive recommendations
            
            recommendations_html <- "<h4>Overall Quality Assessment Recommendations</h4>"
            
            # Analyze overall data quality
            total_features <- length(features)
            high_quality_features <- 0
            medium_quality_features <- 0
            low_quality_features <- 0
            
            for (feature in features) {
                feature_data <- data[[feature]]
                missing_pct <- sum(is.na(feature_data)) / length(feature_data)
                
                if (missing_pct < 0.05) {
                    high_quality_features <- high_quality_features + 1
                } else if (missing_pct < 0.20) {
                    medium_quality_features <- medium_quality_features + 1
                } else {
                    low_quality_features <- low_quality_features + 1
                }
            }
            
            # Generate summary
            recommendations_html <- paste0(recommendations_html,
                "<div class='quality-summary'>",
                "<h5>Dataset Quality Overview</h5>",
                "<ul>",
                "<li><b>Total Features Analyzed:</b> ", total_features, "</li>",
                "<li><b>High Quality Features (&lt;5% missing):</b> ", high_quality_features, " (", 
                round(100 * high_quality_features / total_features, 1), "%)</li>",
                "<li><b>Medium Quality Features (5-20% missing):</b> ", medium_quality_features, " (", 
                round(100 * medium_quality_features / total_features, 1), "%)</li>",
                "<li><b>Low Quality Features (&gt;20% missing):</b> ", low_quality_features, " (", 
                round(100 * low_quality_features / total_features, 1), "%)</li>",
                "</ul>",
                "</div>"
            )
            
            # Provide actionable recommendations
            recommendations_html <- paste0(recommendations_html,
                "<h5>Actionable Recommendations</h5>",
                "<ol>"
            )
            
            if (low_quality_features > 0) {
                recommendations_html <- paste0(recommendations_html,
                    "<li><b>Address High Missing Data:</b> ", low_quality_features, 
                    " features have >20% missing data. Consider imputation strategies or feature removal.</li>"
                )
            }
            
            if (medium_quality_features > 0) {
                recommendations_html <- paste0(recommendations_html,
                    "<li><b>Monitor Medium Quality Features:</b> ", medium_quality_features, 
                    " features have 5-20% missing data. Implement quality control measures.</li>"
                )
            }
            
            recommendations_html <- paste0(recommendations_html,
                "<li><b>Review Outlier Detection:</b> Check the outlier analysis results and determine if outliers represent true biological variation or measurement errors.</li>",
                "<li><b>Consider Data Transformations:</b> Review transformation recommendations for non-normal features if parametric tests are planned.</li>",
                "<li><b>Assess Feature Redundancy:</b> High correlations (&gt;", self$options$correlation_threshold, 
                ") may indicate redundant features that could be removed.</li>",
                "<li><b>Clinical Validation:</b> Validate all quality issues with clinical context and domain expertise.</li>",
                "</ol>"
            )
            
            self$results$overall_recommendations$setContent(recommendations_html)
        },
        
        .generateClinicalInterpretation = function(data, features) {
            # Generate clinical context interpretation
            
            clinical_html <- "<h4>Clinical Context Interpretation</h4>"
            clinical_html <- paste0(clinical_html,
                "<div class='clinical-interpretation'>",
                "<h5>Clinical Data Quality Considerations</h5>",
                "<p>In clinical and pathology research, data quality directly impacts the validity and reproducibility of study findings. This analysis provides essential quality control for:</p>",
                "<ul>",
                "<li><b>Biomarker Studies:</b> Ensuring measurement reliability and biological validity</li>",
                "<li><b>Clinical Trials:</b> Meeting regulatory standards for data quality and integrity</li>",
                "<li><b>Diagnostic Test Development:</b> Validating assay performance and analytical validity</li>",
                "<li><b>Registry Studies:</b> Assessing data completeness and representativeness</li>",
                "</ul>",
                
                "<h5>Clinical Implications of Quality Issues</h5>",
                "<ul>",
                "<li><b>Missing Data (>20%):</b> May indicate systematic collection problems or limit statistical power</li>",
                "<li><b>Extreme Outliers:</b> Could represent rare phenotypes, measurement errors, or protocol deviations</li>",
                "<li><b>Non-normal Distributions:</b> May require transformation or non-parametric methods for valid inference</li>",
                "<li><b>High Feature Correlation:</b> May indicate redundant measurements or underlying biological relationships</li>",
                "<li><b>Low Variance Features:</b> Provide limited discriminatory power and may be measurement artifacts</li>",
                "</ul>",
                
                "<h5>Best Practice Recommendations</h5>",
                "<ol>",
                "<li><b>Document Quality Issues:</b> Maintain detailed records of all quality control decisions</li>",
                "<li><b>Clinical Review:</b> Have domain experts review all flagged outliers and anomalies</li>",
                "<li><b>Pre-specified Plans:</b> Establish quality control procedures before analysis begins</li>",
                "<li><b>Sensitivity Analysis:</b> Test robustness of findings to quality control decisions</li>",
                "<li><b>Regulatory Compliance:</b> Ensure quality standards meet regulatory requirements (FDA, EMA)</li>",
                "</ol>",
                "</div>"
            )
            
            self$results$clinical_interpretation$setContent(clinical_html)
        },
        
        # Helper functions for calculations
        .calculateSkewness = function(x) {
            # Calculate skewness
            x <- x[!is.na(x)]
            n <- length(x)
            if (n < 3) return(NA)
            
            mean_x <- mean(x)
            sd_x <- sd(x)
            if (sd_x == 0) return(0)
            
            skewness <- (n / ((n - 1) * (n - 2))) * sum(((x - mean_x) / sd_x)^3)
            return(skewness)
        },
        
        .calculateKurtosis = function(x) {
            # Calculate excess kurtosis
            x <- x[!is.na(x)]
            n <- length(x)
            if (n < 4) return(NA)
            
            mean_x <- mean(x)
            sd_x <- sd(x)
            if (sd_x == 0) return(0)
            
            kurtosis <- (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * 
                       sum(((x - mean_x) / sd_x)^4) - 
                       (3 * (n - 1)^2 / ((n - 2) * (n - 3)))
            return(kurtosis)
        },
        
        .calculateQualityScore = function(missing_pct, outliers_pct, normality_p, skewness, kurtosis, variance) {
            # Calculate overall quality score (0-100)
            score <- 100
            
            # Penalize for missing data
            score <- score - (missing_pct * 50)  # Max 50 points deduction
            
            # Penalize for high outlier percentage
            score <- score - (outliers_pct * 30)  # Max 30 points deduction
            
            # Penalize for extreme skewness
            if (!is.na(skewness)) {
                if (abs(skewness) > 2) {
                    score <- score - 10
                } else if (abs(skewness) > 1) {
                    score <- score - 5
                }
            }
            
            # Penalize for very low variance
            if (!is.na(variance) && variance < 0.01) {
                score <- score - 10
            }
            
            return(max(0, min(100, score)))
        },
        
        .generateFeatureRecommendation = function(missing_pct, outliers_pct, normality_p, skewness, quality_score) {
            # Generate specific recommendation for each feature
            
            if (quality_score >= 80) {
                return("Good quality - No major issues")
            } else if (quality_score >= 60) {
                return("Moderate quality - Minor issues to address")
            } else if (quality_score >= 40) {
                return("Poor quality - Significant issues require attention")
            } else {
                return("Very poor quality - Consider exclusion or major preprocessing")
            }
        },
        
        .analyzeMissingPattern = function(x) {
            # Analyze missing data patterns
            missing_indices <- which(is.na(x))
            total_length <- length(x)
            
            if (length(missing_indices) == 0) {
                return("Complete data")
            } else if (length(missing_indices) == total_length) {
                return("Completely missing")
            } else if (length(missing_indices) / total_length > 0.5) {
                return("Mostly missing (>50%)")
            } else if (length(missing_indices) / total_length > 0.2) {
                return("Substantially missing (20-50%)")
            } else {
                return("Partially missing (<20%)")
            }
        },
        
        .assessMissingDataImpact = function(missing_percent, missing_pattern) {
            # Assess impact of missing data
            
            if (missing_percent == 0) {
                return("No impact - Complete data")
            } else if (missing_percent < 0.05) {
                return("Minimal impact - Acceptable for most analyses")
            } else if (missing_percent < 0.15) {
                return("Moderate impact - Consider imputation strategies")
            } else if (missing_percent < 0.30) {
                return("Substantial impact - Imputation or exclusion needed")
            } else {
                return("Severe impact - Strong consideration for exclusion")
            }
        },
        
        .determineDistributionType = function(x) {
            # Determine likely distribution type
            
            skewness <- private$.calculateSkewness(x)
            kurtosis <- private$.calculateKurtosis(x)
            
            if (is.na(skewness) || is.na(kurtosis)) {
                return("Cannot determine")
            }
            
            # Basic distribution classification
            if (abs(skewness) < 0.5 && abs(kurtosis) < 0.5) {
                return("Approximately normal")
            } else if (skewness > 1) {
                return("Right-skewed (positive skew)")
            } else if (skewness < -1) {
                return("Left-skewed (negative skew)")
            } else if (abs(kurtosis) > 2) {
                return("Heavy-tailed or platykurtic")
            } else {
                return("Moderately non-normal")
            }
        },
        
        .detectOutliers = function(x, method, threshold) {
            # Detect outliers using specified method
            
            outliers <- logical(length(x))
            lower_bound <- NA
            upper_bound <- NA
            
            if (method == "iqr") {
                Q1 <- quantile(x, 0.25, na.rm = TRUE)
                Q3 <- quantile(x, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                lower_bound <- Q1 - 1.5 * IQR
                upper_bound <- Q3 + 1.5 * IQR
                outliers <- x < lower_bound | x > upper_bound
                
            } else if (method == "zscore") {
                z_scores <- abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
                outliers <- z_scores > threshold
                lower_bound <- mean(x, na.rm = TRUE) - threshold * sd(x, na.rm = TRUE)
                upper_bound <- mean(x, na.rm = TRUE) + threshold * sd(x, na.rm = TRUE)
                
            } else if (method == "modified_zscore") {
                median_x <- median(x, na.rm = TRUE)
                mad_x <- mad(x, na.rm = TRUE)
                if (mad_x == 0) mad_x <- 1.4826 * median(abs(x - median_x), na.rm = TRUE)
                modified_z <- 0.6745 * (x - median_x) / mad_x
                outliers <- abs(modified_z) > threshold
                lower_bound <- median_x - threshold * mad_x / 0.6745
                upper_bound <- median_x + threshold * mad_x / 0.6745
            }
            
            outlier_count <- sum(outliers, na.rm = TRUE)
            outlier_percent <- outlier_count / length(x)
            
            # Assess severity
            severity <- if (outlier_percent > 0.1) {
                "High"
            } else if (outlier_percent > 0.05) {
                "Moderate"
            } else if (outlier_percent > 0.01) {
                "Low"
            } else {
                "Minimal"
            }
            
            return(list(
                count = outlier_count,
                percent = outlier_percent,
                lower_bound = lower_bound,
                upper_bound = upper_bound,
                severity = severity
            ))
        },
        
        .testNormality = function(x, method) {
            # Test normality using specified method
            
            if (method == "shapiro") {
                if (length(x) > 5000) {
                    # Sample for large datasets
                    x <- sample(x, 5000)
                }
                
                test_result <- tryCatch({
                    shapiro.test(x)
                }, error = function(e) list(statistic = NA, p.value = NA))
                
                return(list(
                    method = "Shapiro-Wilk",
                    statistic = test_result$statistic,
                    p_value = test_result$p.value,
                    conclusion = if (is.na(test_result$p.value)) "Test failed" else 
                                if (test_result$p.value < 0.05) "Non-normal" else "Normal"
                ))
                
            } else if (method == "anderson") {
                # Anderson-Darling test (simplified implementation)
                n <- length(x)
                x_sorted <- sort(x)
                z <- pnorm((x_sorted - mean(x)) / sd(x))
                
                # Anderson-Darling statistic
                i <- 1:n
                ad_stat <- -n - sum((2*i - 1) * (log(z) + log(1 - z[n + 1 - i]))) / n
                
                # Approximate p-value (simplified)
                p_value <- if (ad_stat > 1.092) 0.001 else if (ad_stat > 0.787) 0.05 else 0.25
                
                return(list(
                    method = "Anderson-Darling",
                    statistic = ad_stat,
                    p_value = p_value,
                    conclusion = if (p_value < 0.05) "Non-normal" else "Normal"
                ))
            }
            
            return(list(method = "Unknown", statistic = NA, p_value = NA, conclusion = "Test not available"))
        },
        
        .suggestTransformation = function(x, normality_p) {
            # Suggest appropriate transformation
            
            if (is.na(normality_p) || normality_p >= 0.05) {
                return("None - Data appears normal")
            }
            
            skewness <- private$.calculateSkewness(x)
            
            if (is.na(skewness)) {
                return("Cannot determine - Insufficient data")
            }
            
            if (skewness > 1) {
                if (all(x > 0, na.rm = TRUE)) {
                    return("Log transformation - Right skewed, positive values")
                } else {
                    return("Square root or Box-Cox transformation")
                }
            } else if (skewness < -1) {
                return("Reflect and log, or power transformation")
            } else {
                return("Consider Box-Cox or Yeo-Johnson transformation")
            }
        },
        
        .calculateFeatureImportance = function(feature_data, features, group_var, method) {
            # Calculate feature importance scores
            
            importance_scores <- numeric(length(features))
            
            if (method == "correlation") {
                # Correlation-based importance
                group_numeric <- as.numeric(as.factor(feature_data[[group_var]]))
                
                for (i in 1:length(features)) {
                    corr_test <- tryCatch({
                        cor.test(feature_data[[features[i]]], group_numeric)
                    }, error = function(e) list(estimate = 0))
                    
                    importance_scores[i] <- abs(corr_test$estimate)
                }
                
            } else if (method == "chi_square") {
                # Chi-square based importance (for discretized features)
                for (i in 1:length(features)) {
                    # Discretize continuous feature
                    feature_discrete <- cut(feature_data[[features[i]]], breaks = 4, labels = FALSE)
                    
                    chi_test <- tryCatch({
                        chisq.test(table(feature_discrete, feature_data[[group_var]]))
                    }, error = function(e) list(statistic = 0))
                    
                    importance_scores[i] <- as.numeric(chi_test$statistic)
                }
                
            } else {
                # Default to simple variance-based importance
                for (i in 1:length(features)) {
                    group_means <- aggregate(feature_data[[features[i]]], 
                                           by = list(feature_data[[group_var]]), 
                                           FUN = mean, na.rm = TRUE)
                    importance_scores[i] <- var(group_means$x, na.rm = TRUE)
                }
            }
            
            # Normalize scores to 0-1 range
            if (max(importance_scores) > 0) {
                importance_scores <- importance_scores / max(importance_scores)
            }
            
            return(importance_scores)
        },
        
        .analyzeFeatureIssues = function(x) {
            # Analyze feature issues and recommend transformations
            
            missing_pct <- sum(is.na(x)) / length(x)
            skewness <- private$.calculateSkewness(x)
            variance_val <- var(x, na.rm = TRUE)
            
            # Identify primary issue
            issue <- "No major issues detected"
            transformation <- "None required"
            improvement <- "Feature appears acceptable"
            notes <- "Continue with current data"
            
            if (missing_pct > 0.2) {
                issue <- "High missing data rate"
                transformation <- "Multiple imputation or exclusion"
                improvement <- "Improved statistical power and reduced bias"
                notes <- "Consider imputation methods: MICE, median/mode, or predictive imputation"
                
            } else if (!is.na(variance_val) && variance_val < 0.01) {
                issue <- "Very low variance (near-constant values)"
                transformation <- "Consider feature removal"
                improvement <- "Reduced noise and improved model efficiency"
                notes <- "Low variance features provide minimal discriminatory information"
                
            } else if (!is.na(skewness) && abs(skewness) > 2) {
                issue <- "Highly skewed distribution"
                if (skewness > 0 && all(x > 0, na.rm = TRUE)) {
                    transformation <- "Log transformation"
                    improvement <- "Improved normality and reduced outlier impact"
                    notes <- "log(x) or log(x + 1) if zero values present"
                } else {
                    transformation <- "Box-Cox or Yeo-Johnson transformation"
                    improvement <- "Improved normality for parametric tests"
                    notes <- "Use power transformations to address skewness"
                }
                
            } else if (!is.na(skewness) && abs(skewness) > 1) {
                issue <- "Moderately skewed distribution"
                transformation <- "Square root or cube root transformation"
                improvement <- "Mild improvement in normality"
                notes <- "Consider if parametric tests are planned"
            }
            
            return(list(
                issue = issue,
                transformation = transformation,
                improvement = improvement,
                notes = notes
            ))
        },
        
        # Plot functions
        .distributionPlot = function(image, ...) {
            # Generate comprehensive distribution plots
            
            data <- self$data
            features <- self$options$features
            
            if (length(features) == 0) return(FALSE)
            
            # Create multi-panel distribution plot
            n_features <- min(length(features), 12)  # Limit to 12 features
            selected_features <- features[1:n_features]
            
            # Set up plot layout
            n_cols <- min(3, n_features)
            n_rows <- ceiling(n_features / n_cols)
            
            par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
            
            for (feature in selected_features) {
                feature_data <- data[[feature]]
                clean_data <- feature_data[!is.na(feature_data)]
                
                if (length(clean_data) > 0) {
                    # Histogram with density overlay
                    hist(clean_data, main = paste("Distribution:", feature),
                         xlab = feature, ylab = "Frequency", 
                         col = "lightblue", border = "white",
                         probability = TRUE)
                    
                    # Add density curve
                    tryCatch({
                        lines(density(clean_data), col = "red", lwd = 2)
                    }, error = function(e) {})
                    
                    # Add normal curve for comparison
                    x_seq <- seq(min(clean_data), max(clean_data), length.out = 100)
                    normal_curve <- dnorm(x_seq, mean(clean_data), sd(clean_data))
                    lines(x_seq, normal_curve, col = "blue", lwd = 2, lty = 2)
                }
            }
            
            return(TRUE)
        },
        
        .correlationPlot = function(image, ...) {
            # Generate correlation heatmap
            
            data <- self$data
            features <- self$options$features
            
            if (length(features) < 2) return(FALSE)
            
            # Prepare correlation matrix
            feature_data <- data[, features, drop = FALSE]
            feature_data <- feature_data[complete.cases(feature_data), ]
            
            if (nrow(feature_data) < 3) return(FALSE)
            
            corr_matrix <- cor(feature_data, use = "complete.obs")
            
            # Create heatmap
            image(1:ncol(corr_matrix), 1:nrow(corr_matrix), 
                  as.matrix(corr_matrix),
                  col = colorRampPalette(c("blue", "white", "red"))(100),
                  xlab = "", ylab = "", axes = FALSE,
                  main = "Feature Correlation Heatmap")
            
            # Add labels
            axis(1, at = 1:ncol(corr_matrix), labels = colnames(corr_matrix), las = 2)
            axis(2, at = 1:nrow(corr_matrix), labels = rownames(corr_matrix), las = 2)
            
            # Add correlation values
            for (i in 1:nrow(corr_matrix)) {
                for (j in 1:ncol(corr_matrix)) {
                    text(j, i, round(corr_matrix[i, j], 2), 
                         col = if (abs(corr_matrix[i, j]) > 0.5) "white" else "black")
                }
            }
            
            return(TRUE)
        },
        
        .outlierPlot = function(image, ...) {
            # Generate outlier detection visualization
            
            data <- self$data
            features <- self$options$features
            
            if (length(features) == 0) return(FALSE)
            
            # Create boxplots for outlier visualization
            n_features <- min(length(features), 8)
            selected_features <- features[1:n_features]
            
            plot_data <- data[, selected_features, drop = FALSE]
            
            # Create boxplot
            boxplot(plot_data, main = "Outlier Detection (Boxplots)",
                    ylab = "Values", las = 2, col = "lightgreen")
            
            # Add outlier counts as text
            for (i in 1:length(selected_features)) {
                feature_data <- plot_data[[i]]
                Q1 <- quantile(feature_data, 0.25, na.rm = TRUE)
                Q3 <- quantile(feature_data, 0.75, na.rm = TRUE)
                IQR <- Q3 - Q1
                outliers <- sum(feature_data < (Q1 - 1.5 * IQR) | 
                               feature_data > (Q3 + 1.5 * IQR), na.rm = TRUE)
                
                text(i, max(feature_data, na.rm = TRUE), 
                     paste("n =", outliers), pos = 3, cex = 0.8)
            }
            
            return(TRUE)
        },
        
        .missingPlot = function(image, ...) {
            # Generate missing data pattern visualization
            
            data <- self$data
            features <- self$options$features
            
            if (length(features) == 0) return(FALSE)
            
            # Create missing data pattern plot
            missing_data <- is.na(data[, features, drop = FALSE])
            missing_pattern <- as.matrix(missing_data)
            
            # Create heatmap of missing data
            image(1:ncol(missing_pattern), 1:nrow(missing_pattern), 
                  t(missing_pattern),
                  col = c("lightblue", "red"),
                  xlab = "Features", ylab = "Observations",
                  main = "Missing Data Patterns",
                  axes = FALSE)
            
            # Add feature labels
            axis(1, at = 1:ncol(missing_pattern), labels = colnames(missing_pattern), las = 2)
            
            # Add legend
            legend("topright", legend = c("Observed", "Missing"), 
                   fill = c("lightblue", "red"), cex = 0.8)
            
            return(TRUE)
        },
        
        .qualityDashboard = function(image, ...) {
            # Generate comprehensive quality dashboard
            
            data <- self$data
            features <- self$options$features
            
            if (length(features) == 0) return(FALSE)
            
            # Set up dashboard layout
            par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
            
            # 1. Missing data summary
            missing_counts <- sapply(features, function(f) sum(is.na(data[[f]])))
            barplot(missing_counts, main = "Missing Data by Feature",
                    xlab = "Features", ylab = "Missing Count",
                    col = "orange", las = 2)
            
            # 2. Quality scores
            quality_scores <- numeric(length(features))
            for (i in 1:length(features)) {
                feature_data <- data[[features[i]]]
                missing_pct <- sum(is.na(feature_data)) / length(feature_data)
                quality_scores[i] <- private$.calculateQualityScore(missing_pct, 0, 0.5, 0, 0, 1)
            }
            
            barplot(quality_scores, main = "Feature Quality Scores",
                    xlab = "Features", ylab = "Quality Score (0-100)",
                    col = "lightgreen", las = 2, ylim = c(0, 100))
            abline(h = 80, col = "red", lty = 2)  # Good quality threshold
            
            # 3. Distribution comparison (first 4 features)
            first_features <- features[1:min(4, length(features))]
            if (length(first_features) > 0) {
                boxplot(data[, first_features, drop = FALSE], 
                        main = "Distribution Comparison",
                        ylab = "Values", col = "lightblue", las = 2)
            }
            
            # 4. Summary statistics
            plot.new()
            title("Quality Assessment Summary")
            
            # Calculate summary statistics
            total_obs <- nrow(data)
            avg_missing <- mean(sapply(features, function(f) sum(is.na(data[[f]])) / total_obs))
            high_quality_features <- sum(quality_scores >= 80)
            
            text(0.1, 0.8, paste("Total Observations:", total_obs), adj = 0, cex = 1.2)
            text(0.1, 0.6, paste("Features Analyzed:", length(features)), adj = 0, cex = 1.2)
            text(0.1, 0.4, paste("Average Missing Rate:", round(avg_missing * 100, 1), "%"), adj = 0, cex = 1.2)
            text(0.1, 0.2, paste("High Quality Features:", high_quality_features), adj = 0, cex = 1.2)
            
            return(TRUE)
        }
    )
)